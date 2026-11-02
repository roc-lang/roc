//! Compiler-owned wasm functions that work on the stack pointer.
//!
//! A host tells a stack overflow from any other trap by reading the stack
//! pointer after the trap, and resets it before calling in again. Later wasm
//! exports the `__stack_pointer` global itself, but WebAssembly 1.0 cannot
//! export a mutable global, so a wasm32v1 module exports accessor functions
//! instead.
//!
//! When a platform reserves low memory, a failed stack-bound check calls
//! `roc_wasm_stack_overflow`, which leaves the stack pointer below
//! `__stack_low` so the host sees the same sign of an overflow a stack-first
//! module leaves, and traps.

const std = @import("std");
const shim_symbols = @import("builtins").shim_symbols;

const WasmModule = @import("WasmModule.zig");
const WasmLinking = @import("WasmLinking.zig");
const CodeBuilder = @import("CodeBuilder.zig");
const SymbolIndex = @import("index_types.zig").SymbolIndex;

const Allocator = std.mem.Allocator;
const Op = WasmModule.Op;

/// Which functions an object from `encodeObject` defines.
pub const Contents = struct {
    /// `roc_wasm_stack_pointer`, which returns the stack pointer, and
    /// `roc_wasm_set_stack_pointer`, which replaces it.
    accessors: bool,
    /// `roc_wasm_stack_overflow`.
    overflow: bool,
};

/// A relocatable object defining the functions `contents` selects.
pub fn encodeObject(allocator: Allocator, contents: Contents) WasmModule.RelocatableEncodeError![]u8 {
    var module = WasmModule.init(allocator);
    defer module.deinit();
    const stack_pointer = try module.addStackPointerImportWithSymbol();

    if (contents.accessors) {
        const getter_type = try module.addFuncType(&.{}, &.{.i32});
        const getter = try module.addDefinedFunction(getter_type);
        _ = try module.addDefinedFunctionSymbol(getter.local, shim_symbols.roc_wasm_stack_pointer, 0);
        try appendBody(allocator, &module, stack_pointer, &.{}, Op.global_get, &.{});

        const setter_type = try module.addFuncType(&.{.i32}, &.{});
        const setter = try module.addDefinedFunction(setter_type);
        _ = try module.addDefinedFunctionSymbol(setter.local, shim_symbols.roc_wasm_set_stack_pointer, 0);
        try appendBody(allocator, &module, stack_pointer, &.{ Op.local_get, 0 }, Op.global_set, &.{});
    }

    if (contents.overflow) {
        // Zero is below `__stack_low` whenever a platform reserves low memory,
        // the only time a check calls this.
        const overflow_type = try module.addFuncType(&.{}, &.{});
        const overflow = try module.addDefinedFunction(overflow_type);
        _ = try module.addDefinedFunctionSymbol(overflow.local, shim_symbols.roc_wasm_stack_overflow, 0);
        try appendBody(allocator, &module, stack_pointer, &.{ Op.i32_const, 0 }, Op.global_set, &.{Op.@"unreachable"});
    }

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

test "the stack support object defines the accessors and the overflow function over the stack pointer" {
    const allocator = std.testing.allocator;
    const bytes = try encodeObject(allocator, .{ .accessors = true, .overflow = true });
    defer allocator.free(bytes);

    var module = try WasmModule.preload(allocator, bytes, .relocatable_for_merge);
    defer module.deinit();

    _ = try module.findDefinedFunctionSymbolExact(shim_symbols.roc_wasm_stack_pointer);
    _ = try module.findDefinedFunctionSymbolExact(shim_symbols.roc_wasm_set_stack_pointer);
    _ = try module.findDefinedFunctionSymbolExact(shim_symbols.roc_wasm_stack_overflow);
    try std.testing.expectEqual(@as(usize, 3), module.reloc_code.entries.items.len);
    for (module.reloc_code.entries.items) |entry| {
        try std.testing.expectEqual(WasmLinking.IndexRelocType.global_index_leb, entry.index.type_id);
    }
}
