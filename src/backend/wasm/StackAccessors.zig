//! The stack-pointer accessor functions of a WebAssembly 1.0 module.
//!
//! A host tells a stack overflow from any other trap by reading the stack
//! pointer after the trap, and resets it before calling in again. Later wasm
//! exports the `__stack_pointer` global itself, but WebAssembly 1.0 cannot
//! export a mutable global, so a wasm32v1 module exports these two functions
//! instead.

const std = @import("std");
const shim_symbols = @import("builtins").shim_symbols;

const WasmModule = @import("WasmModule.zig");
const WasmLinking = @import("WasmLinking.zig");
const CodeBuilder = @import("CodeBuilder.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;

const Allocator = std.mem.Allocator;
const Op = WasmModule.Op;

/// A relocatable object defining `roc_wasm_stack_pointer`, which returns the
/// stack pointer, and `roc_wasm_set_stack_pointer`, which replaces it.
pub fn encodeObject(allocator: Allocator) WasmModule.RelocatableEncodeError![]u8 {
    var module = WasmModule.init(allocator);
    defer module.deinit();
    const stack_pointer = try module.addStackPointerImportWithSymbol();

    const getter_type = try module.addFuncType(&.{}, &.{.i32});
    const getter = try module.addDefinedFunction(getter_type);
    _ = try module.addDefinedFunctionSymbol(getter.local, shim_symbols.roc_wasm_stack_pointer, 0);
    try appendBody(allocator, &module, stack_pointer, &.{}, Op.global_get, &.{});

    const setter_type = try module.addFuncType(&.{.i32}, &.{});
    const setter = try module.addDefinedFunction(setter_type);
    _ = try module.addDefinedFunctionSymbol(setter.local, shim_symbols.roc_wasm_set_stack_pointer, 0);
    try appendBody(allocator, &module, stack_pointer, &.{ Op.local_get, 0 }, Op.global_set, &.{});

    return module.encodeRelocatable(allocator);
}

/// Append a body that runs `before`, then `global_op` on the stack pointer,
/// then `after`.
fn appendBody(
    allocator: Allocator,
    module: *WasmModule,
    stack_pointer: SymbolIndex,
    before: []const u8,
    global_op: u8,
    after: []const u8,
) Allocator.Error!void {
    var body = CodeBuilder.init();
    defer body.deinit(allocator);
    try body.preamble.append(allocator, 0); // no locals
    try body.code.appendSlice(allocator, before);
    try body.code.append(allocator, global_op);
    try body.addIndexRelocation(allocator, .global_index_leb, @intCast(body.code.items.len), stack_pointer);
    try WasmModule.appendPaddedU32(allocator, &body.code, 0);
    try body.code.appendSlice(allocator, after);
    try body.code.append(allocator, Op.end);
    _ = try body.insertIntoModule(allocator, module);
}

test "the stack accessor object defines both accessors over the stack pointer" {
    const allocator = std.testing.allocator;
    const bytes = try encodeObject(allocator);
    defer allocator.free(bytes);

    var module = try WasmModule.preload(allocator, bytes, .relocatable_for_merge);
    defer module.deinit();

    _ = try module.findDefinedFunctionSymbolExact(shim_symbols.roc_wasm_stack_pointer);
    _ = try module.findDefinedFunctionSymbolExact(shim_symbols.roc_wasm_set_stack_pointer);
    try std.testing.expectEqual(@as(usize, 2), module.reloc_code.entries.items.len);
    for (module.reloc_code.entries.items) |entry| {
        try std.testing.expectEqual(WasmLinking.IndexRelocType.global_index_leb, entry.index.type_id);
    }
}
