//! WASM entry point for the echo platform.
//!
//! Provides exported functions for JavaScript to:
//! 1. Initialize the WASM module
//! 2. Register module files (e.g. Greeting.roc)
//! 3. Compile and execute a headerless Roc app through the full compiler pipeline
//!
//! Delegates the compile-and-run pipeline to `runner.zig`, which is shared
//! with the native CLI in `echo_native.zig`.

const std = @import("std");
const builtin = @import("builtin");
const reporting = @import("reporting");
const runner = @import("runner.zig");

const WasmFilesystem = @import("WasmFilesystem.zig");

const Allocator = std.mem.Allocator;
const Diagnostics = runner.Diagnostics;
const ExtraFile = runner.ExtraFile;
const ReportingConfig = reporting.ReportingConfig;

/// Public API.
pub const std_options: std.Options = .{
    .log_level = .warn,
    .logFn = logFn,
};

fn logFn(comptime level: std.log.Level, comptime scope: @TypeOf(.enum_literal), comptime format: []const u8, args: anytype) void {
    if (comptime builtin.target.os.tag == .freestanding) {
        return;
    }
    std.log.defaultLog(level, scope, format, args);
}

// Fixed-size heap in WASM linear memory (128 MiB — matches the runtime arena
// size recommended in src/compile/README.md "Runtime arena"; smaller sizes
// OOM during BuildEnv.init / lowering for non-trivial programs). align(16)
// so the LIR image's base_ptr satisfies the alignment constraints
// documented in the same section.
var wasm_heap_memory: [128 * 1024 * 1024]u8 align(16) = undefined;
var fba: std.heap.FixedBufferAllocator = undefined;
var allocator: Allocator = undefined;

// --- JS imports ---

const js = struct {
    extern "env" fn js_stderr(ptr: [*]const u8, len: usize) void;
};

// --- Diagnostics implementation: emits HTML reports + step labels via js_stderr ---

fn jsWrite(_: ?*anyopaque, msg: []const u8) void {
    js.js_stderr(msg.ptr, msg.len);
}

fn jsEmitReport(_: ?*anyopaque, gpa: Allocator, report: *reporting.Report) void {
    const config = ReportingConfig.initHtml();
    var diag_writer: std.Io.Writer.Allocating = .init(gpa);
    reporting.renderReportToHtml(report, &diag_writer.writer, config) catch return;
    const output = diag_writer.written();
    if (output.len > 0) {
        js.js_stderr(output.ptr, output.len);
    }
}

const js_diagnostics_vtable = Diagnostics.VTable{
    .write = &jsWrite,
    .emitReport = &jsEmitReport,
};

fn jsDiagnostics() Diagnostics {
    return .{ .ctx = null, .vtable = &js_diagnostics_vtable };
}

// --- Extra module file storage (static, survives FBA reset) ---

const MAX_FILES = 16;
const MAX_NAME_LEN = 256;
const MAX_CONTENT_LEN = 64 * 1024;

const ExtraFileSlot = struct {
    name_buf: [MAX_NAME_LEN]u8 = undefined,
    name_len: usize = 0,
    content_buf: [MAX_CONTENT_LEN]u8 = undefined,
    content_len: usize = 0,

    fn name(self: *const ExtraFileSlot) []const u8 {
        return self.name_buf[0..self.name_len];
    }

    fn content(self: *const ExtraFileSlot) []const u8 {
        return self.content_buf[0..self.content_len];
    }
};

var extra_files: [MAX_FILES]ExtraFileSlot = [_]ExtraFileSlot{.{}} ** MAX_FILES;
var extra_file_count: usize = 0;

// Static buffer for copying source before FBA reset (avoids 64KB stack allocation in WASM).
var source_copy_buf: [MAX_CONTENT_LEN]u8 = undefined;

// WASM filesystem context — static so it survives FBA reset.
var wasm_ctx: WasmFilesystem.WasmContext = .{};

// --- Exported WASM API ---

/// Initialize the WASM module. Must be called once before compileAndRun.
export fn init() void {
    fba = std.heap.FixedBufferAllocator.init(&wasm_heap_memory);
    allocator = fba.allocator();
    extra_file_count = 0;
}

/// Allocate a buffer for the host to write data into.
/// Returns a pointer to the buffer, or null on OOM.
export fn allocateBuffer(size: usize) ?[*]u8 {
    const buf = allocator.alloc(u8, size) catch return null;
    return buf.ptr;
}

/// Register an extra module file (e.g. name="Greeting", content="module [msg]\n...").
/// Files are resolved as `/app/<name>.roc` during compilation.
/// Must be called after init() and before compileAndRun().
/// Returns 0 on success, 1 if too many files, 2 if name too long, 3 if content too long.
export fn addFile(
    name_ptr: [*]const u8,
    name_len: usize,
    content_ptr: [*]const u8,
    content_len: usize,
) u8 {
    if (extra_file_count >= MAX_FILES) {
        jsWrite(null, "addFile: too many files (max 16)\n");
        return 1;
    }
    if (name_len > MAX_NAME_LEN) {
        jsWrite(null, "addFile: module name too long (max 256 bytes)\n");
        return 2;
    }
    if (content_len > MAX_CONTENT_LEN) {
        jsWrite(null, "addFile: module content too long (max 64KB)\n");
        return 3;
    }

    var ef = &extra_files[extra_file_count];
    @memcpy(ef.name_buf[0..name_len], name_ptr[0..name_len]);
    ef.name_len = name_len;
    @memcpy(ef.content_buf[0..content_len], content_ptr[0..content_len]);
    ef.content_len = content_len;
    extra_file_count += 1;
    return 0;
}

/// Compile and execute Roc source code through the echo platform pipeline.
///
/// The source should be a headerless Roc module containing a `main!` declaration.
/// Any modules registered via addFile() are available as imports.
/// Returns the exit code from the Roc program (0 on success), or 255 on error.
export fn compileAndRun(source_ptr: [*]const u8, source_len: usize) u8 {
    // Copy source to a static buffer before resetting the FBA,
    // since allocateBuffer returned a pointer into the same heap.
    const len = @min(source_len, source_copy_buf.len);
    @memcpy(source_copy_buf[0..len], source_ptr[0..len]);

    const result = compileAndRunInner(source_copy_buf[0..len]) catch 255;

    // Clear extra files for next run
    extra_file_count = 0;

    return result;
}

fn compileAndRunInner(source: []const u8) runner.RunEchoError!u8 {
    fba.reset();
    allocator = fba.allocator();

    // Build the extras slice in the FBA-backed allocator.
    var extras = try allocator.alloc(ExtraFile, extra_file_count);
    defer allocator.free(extras);
    for (extra_files[0..extra_file_count], 0..) |*slot, i| {
        extras[i] = .{ .name = slot.name(), .content = slot.content() };
    }

    // Synthetic app source must be set on the WasmFilesystem so it shares
    // the filename mapping; runner.runEcho generates it and passes it back
    // through the Io vtable for any internal reads it triggers.
    wasm_ctx.setFilename(allocator, "/app/main.roc");
    // The runner writes the synthetic source itself via EchoCtx — the wasm
    // fallback's setSource is only consulted for non-synthetic paths.

    // On wasm32-freestanding there is no real `std.Io` — the host JS provides
    // I/O through the WasmFilesystem vtable and direct `extern "env"` calls.
    // We pass `std.Io.failing` as a tripwire: if any code path on WASM ever
    // tries to actually use the std.Io (e.g. a `writeStreamingAll` outside
    // the explicit `is_wasm` branches), it traps loudly instead of silently
    // dereferencing `undefined`.
    const wasm_std_io: std.Io = std.Io.failing;

    return runner.runEcho(.{
        .runtime_fba = &fba,
        .fallback_io = WasmFilesystem.wasm(&wasm_ctx, allocator, wasm_std_io),
        .std_io = wasm_std_io,
        .source = source,
        .extras = extras,
        .diagnostics = jsDiagnostics(),
    });
}
