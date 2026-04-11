//! LIR-backed WebAssembly evaluator.

const std = @import("std");
const backend = @import("backend");
const pipeline = @import("pipeline.zig");
const wasm_runner = @import("wasm_runner.zig");

const WasmCodeGen = backend.wasm.WasmCodeGen;

pub const WasmEvalError = wasm_runner.WasmEvalError;

pub fn evalLoweredToStr(
    allocator: std.mem.Allocator,
    lowered: *const pipeline.LoweredProgram,
) WasmEvalError![]u8 {
    var codegen = WasmCodeGen.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
    );
    defer codegen.deinit();

    const proc = lowered.lir_result.store.getProcSpec(lowered.main_proc);
    const wasm_result = codegen.generateModule(lowered.main_proc, proc.ret_layout) catch return error.OutOfMemory;
    defer allocator.free(wasm_result.wasm_bytes);

    return wasm_runner.runWasmStr(allocator, wasm_result.wasm_bytes, wasm_result.has_imports);
}
