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
// so the runtime image's base_ptr satisfies the alignment constraints
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

}
