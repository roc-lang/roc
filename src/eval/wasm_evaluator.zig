//! LIR-backed WebAssembly evaluator.

const std = @import("std");
const builtin = @import("builtin");
const backend = @import("backend");
const pipeline = @import("pipeline.zig");
const wasm_runner = if (builtin.target.os.tag == .freestanding) struct {
    pub const EvalError = error{WasmExecFailed};

    pub fn runWasmStr(_: std.mem.Allocator, _: []const u8, _: bool) EvalError![]u8 {
        return error.WasmExecFailed;
    }
} else @import("wasm_runner.zig");

const WasmCodeGen = backend.wasm.WasmCodeGen;

pub const WasmEvalError = if (builtin.target.os.tag == .freestanding)
    wasm_runner.EvalError
else
    wasm_runner.WasmEvalError;

pub fn evalLoweredToStr(
    allocator: std.mem.Allocator,
    lowered: *const pipeline.LoweredProgram,
) WasmEvalError![]u8 {
    if (comptime builtin.target.os.tag == .freestanding) {
        return error.WasmExecFailed;
    }
    var codegen = WasmCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
    );
    defer codegen.deinit();

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const wasm_result = codegen.generateModule(lowered.main_proc, proc.ret_layout) catch return error.OutOfMemory;
    defer allocator.free(wasm_result.wasm_bytes);

    if (comptime builtin.target.os.tag != .freestanding and std.debug.runtime_safety) {
        if (std.process.getEnvVarOwned(allocator, "ROC_WASM_DUMP")) |dump_path| {
            defer allocator.free(dump_path);
            std.fs.cwd().writeFile(.{ .sub_path = dump_path, .data = wasm_result.wasm_bytes }) catch {};
        } else |_| {}
    }

    return wasm_runner.runWasmStr(allocator, wasm_result.wasm_bytes, wasm_result.has_imports);
}
