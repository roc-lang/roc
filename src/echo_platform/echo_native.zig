//! Native CLI for the echo platform — reproduces the WASM `compileAndRun`
//! flow with real stack traces. Use this to debug pipeline failures that
//! surface as silent exit-255s in the browser.
//!
//! Usage:
//!   echo_native <path/to/app.roc> [--with-file Name=path/to/Module.roc] ...
//!
//! The file at `<path/to/app.roc>` is the headerless body; the runner wraps
//! it in an `app [main!]` header that imports the embedded echo platform.

const std = @import("std");
const compile = @import("compile");
const reporting = @import("reporting");
const roc_target = @import("roc_target");
const base = @import("base");
const runner = @import("runner.zig");

const Allocator = std.mem.Allocator;
const Diagnostics = runner.Diagnostics;
const ExtraFile = runner.ExtraFile;

const CoreCtx = compile.CoreCtx;
const EchoNativeError = std.process.Args.ToSliceError || std.Io.Dir.ReadFileAllocError || runner.RunEchoError;

// --- Diagnostics: write to real stderr ---
//
// The diagnostics vtable doesn't carry a `std.Io`, so we capture the process
// I/O instance into a file-scope variable in `main` before installing the
// vtable. All diagnostic writes happen after `main` has run, so the variable
// is guaranteed to be initialized at the time it is read.
var process_io: std.Io = undefined;

fn stderrWrite(_: ?*anyopaque, msg: []const u8) void {
    const stderr_file: std.Io.File = .stderr();
    stderr_file.writeStreamingAll(process_io, msg) catch {};
}

fn stderrEmitReport(_: ?*anyopaque, gpa: Allocator, report: *reporting.Report) void {
    var diag_writer: std.Io.Writer.Allocating = .init(gpa);
    defer diag_writer.deinit();
    reporting.renderReport(report, &diag_writer.writer, .color_terminal) catch return;
    const output = diag_writer.written();
    if (output.len > 0) {
        const stderr_file: std.Io.File = .stderr();
        stderr_file.writeStreamingAll(process_io, output) catch {};
    }
}

const stderr_diagnostics_vtable = Diagnostics.VTable{
    .write = &stderrWrite,
    .emitReport = &stderrEmitReport,
};

fn stderrDiagnostics() Diagnostics {
    return .{ .ctx = null, .vtable = &stderr_diagnostics_vtable };
}

// --- CLI ---

fn printUsageAndExit() noreturn {
    const msg =
        \\Usage: echo_native <path/to/app.roc> [--with-file Name=path/to/Module.roc]...
        \\
        \\Compiles & runs a headerless Roc app through the echo platform pipeline.
        \\Each --with-file registers an extra module accessible as `import Name`.
        \\
    ;
    const stderr_file: std.Io.File = .stderr();
    stderr_file.writeStreamingAll(process_io, msg) catch {};
    std.process.exit(2);
}

/// CLI entry point. Parses argv, reads the app source (and any
/// `--with-file` modules), then drives `runner.runEcho` against a
/// 128 MiB FixedBufferAllocator. Exits with the Roc program's exit code.
pub fn main(init: std.process.Init) EchoNativeError!void {
    process_io = init.io;
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try init.minimal.args.toSlice(arena);
    if (args.len < 2) printUsageAndExit();

    const app_path = args[1];

    var extras = std.ArrayList(ExtraFile).empty;
    defer extras.deinit(arena);

    const stderr_file: std.Io.File = .stderr();

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (!std.mem.eql(u8, arg, "--with-file")) {
            stderr_file.writeStreamingAll(io, "unknown argument: ") catch {};
            stderr_file.writeStreamingAll(io, arg) catch {};
            stderr_file.writeStreamingAll(io, "\n") catch {};
            printUsageAndExit();
        }
        i += 1;
        if (i >= args.len) printUsageAndExit();
        const spec = args[i];
        const eq = std.mem.findScalar(u8, spec, '=') orelse {
            stderr_file.writeStreamingAll(io, "--with-file value must be Name=path\n") catch {};
            std.process.exit(2);
        };
        const name = spec[0..eq];
        const path = spec[eq + 1 ..];
        const content = try std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .unlimited);
        try extras.append(arena, .{ .name = name, .content = content });
    }

    const source = try std.Io.Dir.cwd().readFileAlloc(io, app_path, gpa, .unlimited);

    // Use a 128 MiB FBA so the pipeline behaves like the wasm case (single
    // contiguous heap) — this is also what the LIR image migration in
    // Phase 3 will require.
    const RUNTIME_BUFFER_SIZE: usize = 128 * 1024 * 1024;
    const runtime_buffer = try gpa.alignedAlloc(u8, .@"16", RUNTIME_BUFFER_SIZE);
    defer gpa.free(runtime_buffer);
    var runtime_fba = std.heap.FixedBufferAllocator.init(runtime_buffer);

    // ECHO_NATIVE_TARGET env var: "native" (default), "wasm32" — cross-compile
    // through the same target_usize=.u32 path the wasm uses, while still
    // running the interpreter natively. Useful to isolate wasm-runtime bugs
    // from wasm32-target compilation bugs.
    //
    // In Zig 0.16 `std.process.getEnvVarOwned` was removed; on POSIX we read
    // from `init.minimal.environ` via `Environ.getPosix` (no alloc). The
    // echo native CLI is only built for POSIX targets, so the Windows branch
    // is just a defensive default.
    const target_choice: []const u8 = blk: {
        if (@import("builtin").os.tag == .windows) break :blk "native";
        const v = std.process.Environ.getPosix(init.minimal.environ, "ECHO_NATIVE_TARGET");
        break :blk if (v) |s| s else "native";
    };
    const is_wasm_target = std.mem.eql(u8, target_choice, "wasm32");

    const code = runner.runEcho(.{
        .runtime_fba = &runtime_fba,
        .fallback_io = CoreCtx.default(gpa, arena, io),
        .std_io = io,
        .source = source,
        .extras = extras.items,
        .diagnostics = stderrDiagnostics(),
        .roc_target = if (is_wasm_target) .wasm32 else roc_target.RocTarget.detectNative(),
        .target_usize = if (is_wasm_target) .u32 else .native,
    }) catch |err| {
        stderr_file.writeStreamingAll(io, "echo_native: pipeline returned error\n") catch {};
        return err;
    };

    std.process.exit(code);
}
