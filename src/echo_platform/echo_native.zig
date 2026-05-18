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
const runner = @import("runner.zig");

const Allocator = std.mem.Allocator;
const Diagnostics = runner.Diagnostics;
const ExtraFile = runner.ExtraFile;

const Io = compile.Io;

// --- Diagnostics: write to real stderr ---

fn stderrWrite(_: ?*anyopaque, msg: []const u8) void {
    std.fs.File.stderr().writeAll(msg) catch {};
}

fn stderrEmitReport(_: ?*anyopaque, gpa: Allocator, report: *reporting.Report) void {
    var diag_writer: std.Io.Writer.Allocating = .init(gpa);
    defer diag_writer.deinit();
    reporting.renderReport(report, &diag_writer.writer, .color_terminal) catch return;
    const output = diag_writer.written();
    if (output.len > 0) {
        std.fs.File.stderr().writeAll(output) catch {};
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
    std.fs.File.stderr().writeAll(msg) catch {};
    std.process.exit(2);
}

pub fn main() !void {
    var gpa_impl: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try std.process.argsAlloc(arena);
    if (args.len < 2) printUsageAndExit();

    const app_path = args[1];

    var extras = std.ArrayList(ExtraFile).empty;
    defer extras.deinit(arena);

    var i: usize = 2;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (!std.mem.eql(u8, arg, "--with-file")) {
            std.fs.File.stderr().writeAll("unknown argument: ") catch {};
            std.fs.File.stderr().writeAll(arg) catch {};
            std.fs.File.stderr().writeAll("\n") catch {};
            printUsageAndExit();
        }
        i += 1;
        if (i >= args.len) printUsageAndExit();
        const spec = args[i];
        const eq = std.mem.indexOfScalar(u8, spec, '=') orelse {
            std.fs.File.stderr().writeAll("--with-file value must be Name=path\n") catch {};
            std.process.exit(2);
        };
        const name = spec[0..eq];
        const path = spec[eq + 1 ..];
        const content = try std.fs.cwd().readFileAlloc(arena, path, std.math.maxInt(usize));
        try extras.append(arena, .{ .name = name, .content = content });
    }

    const source = try std.fs.cwd().readFileAlloc(arena, app_path, std.math.maxInt(usize));

    // Use a 128 MiB FBA so the pipeline behaves like the wasm case (single
    // contiguous heap) — this is also what the runtime-image migration in
    // Phase 3 will require.
    const RUNTIME_BUFFER_SIZE: usize = 128 * 1024 * 1024;
    const runtime_buffer = try gpa.alignedAlloc(u8, .@"16", RUNTIME_BUFFER_SIZE);
    defer gpa.free(runtime_buffer);
    var runtime_fba = std.heap.FixedBufferAllocator.init(runtime_buffer);

    // ECHO_NATIVE_TARGET env var: "native" (default), "wasm32" — cross-compile
    // through the same target_usize=.u32 path the wasm uses, while still
    // running the interpreter natively. Useful to isolate wasm-runtime bugs
    // from wasm32-target compilation bugs.
    const target_choice = std.posix.getenv("ECHO_NATIVE_TARGET") orelse "native";
    const is_wasm_target = std.mem.eql(u8, target_choice, "wasm32");

    const code = runner.runEcho(.{
        .runtime_fba = &runtime_fba,
        .fallback_io = Io.default(),
        .source = source,
        .extras = extras.items,
        .diagnostics = stderrDiagnostics(),
        .roc_target = if (is_wasm_target) .wasm32 else roc_target.RocTarget.detectNative(),
        .target_usize = if (is_wasm_target) .u32 else .native,
    }) catch |err| {
        std.fs.File.stderr().writeAll("echo_native: pipeline returned error\n") catch {};
        return err;
    };

    std.process.exit(code);
}
