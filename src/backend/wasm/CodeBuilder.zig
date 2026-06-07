//! Accumulates one WASM function body with deferred relocation resolution.
//!
//! During code generation, instructions are emitted into `code` and relocatable
//! call sites are recorded as relative offsets. Only when `insertIntoModule()` is
//! called are relocations resolved to absolute code-section offsets. This matches
//! the Rust compiler's `CodeBuilder` / `insert_into_module()` pattern.

const std = @import("std");
const Allocator = std.mem.Allocator;
const WasmModule = @import("WasmModule.zig");
const WasmLinking = @import("WasmLinking.zig");
const index_types = @import("index_types.zig");
const SymbolIndex = index_types.SymbolIndex;

const Self = @This();

/// Main instruction bytes for the current function.
code: std.ArrayList(u8),
/// Locals declaration preamble (local count groups + types).
preamble: std.ArrayList(u8),
/// Relocations within this function: (code_pos, symbol_index).
/// code_pos is relative to the start of self.code, NOT the module's code section.
import_relocations: std.ArrayList(Relocation),

/// A relocation entry recording a call site that needs patching during linking.
pub const Relocation = union(enum) {
    index: struct {
        /// Relocation kind for this padded LEB operand.
        type_id: WasmLinking.IndexRelocType = .function_index_leb,
        /// Byte position of the 5-byte padded LEB128 placeholder, relative to start of `code`.
        code_pos: u32,
        /// Index into the module's linking symbol table.
        symbol_index: SymbolIndex,
    },
    offset: struct {
        /// Relocation kind for this padded memory-address operand.
        type_id: WasmLinking.OffsetRelocType,
        /// Byte position of the 5-byte padded LEB128 placeholder, relative to start of `code`.
        code_pos: u32,
        /// Index into the module's linking symbol table.
        symbol_index: SymbolIndex,
        /// Link-time addend applied to the symbol address.
        addend: i32,
    },

    fn codePos(self: Relocation) u32 {
        return switch (self) {
            .index => |idx| idx.code_pos,
            .offset => |off| off.code_pos,
        };
    }

    fn shiftCodePos(self: *Relocation, amount: u32) void {
        switch (self.*) {
            .index => |*idx| idx.code_pos += amount,
            .offset => |*off| off.code_pos += amount,
        }
    }
};

pub fn init() Self {
    return .{
        .code = .empty,
        .preamble = .empty,
        .import_relocations = .empty,
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.code.deinit(allocator);
    self.preamble.deinit(allocator);
    self.import_relocations.deinit(allocator);
}

/// Emit a relocatable call instruction.
///
/// Writes a `call` opcode followed by a 5-byte padded LEB128 operand.
/// Records the relocation position relative to this function's code buffer so that
/// `insertIntoModule` can compute the absolute offset later.
pub fn emitRelocatableCall(self: *Self, allocator: Allocator, symbol_idx: SymbolIndex, provisional_func_idx: u32) Allocator.Error!void {
    try self.code.append(allocator, WasmModule.Op.call);
    const code_pos: u32 = @intCast(self.code.items.len);
    try self.import_relocations.append(allocator, .{ .index = .{
        .type_id = .function_index_leb,
        .code_pos = code_pos,
        .symbol_index = symbol_idx,
    } });
    try WasmModule.appendPaddedU32(allocator, &self.code, provisional_func_idx);
}

/// Record a relocatable padded LEB operand already emitted into this function.
pub fn addIndexRelocation(
    self: *Self,
    allocator: Allocator,
    type_id: WasmLinking.IndexRelocType,
    code_pos: u32,
    symbol_idx: SymbolIndex,
) Allocator.Error!void {
    try self.import_relocations.append(allocator, .{ .index = .{
        .type_id = type_id,
        .code_pos = code_pos,
        .symbol_index = symbol_idx,
    } });
}

/// Record a relocatable padded memory-address operand already emitted into this function.
pub fn addOffsetRelocation(
    self: *Self,
    allocator: Allocator,
    type_id: WasmLinking.OffsetRelocType,
    code_pos: u32,
    symbol_idx: SymbolIndex,
    addend: i32,
) Allocator.Error!void {
    try self.import_relocations.append(allocator, .{ .offset = .{
        .type_id = type_id,
        .code_pos = code_pos,
        .symbol_index = symbol_idx,
        .addend = addend,
    } });
}

/// Finalize this function and insert it into the module's code section.
///
/// Computes final relocation offsets based on the function's actual position in
/// `code_bytes` (accounting for the LEB128 body-length prefix and preamble).
/// Returns the byte offset where this function's body starts in `code_bytes`
/// (i.e. the position of the body-length prefix).
pub fn insertIntoModule(self: *Self, allocator: Allocator, module: *WasmModule) Allocator.Error!u32 {
    const fn_offset: u32 = @intCast(module.code_bytes.items.len);
    try module.function_offsets.append(allocator, fn_offset);

    // Encode body length as LEB128 (preamble + instructions)
    const body_len: u32 = @intCast(self.preamble.items.len + self.code.items.len);
    try WasmModule.leb128WriteU32(allocator, &module.code_bytes, body_len);

    // Append preamble (locals declaration)
    try module.code_bytes.appendSlice(allocator, self.preamble.items);

    // Record the code start offset (after length prefix + preamble)
    const code_start: u32 = @intCast(module.code_bytes.items.len);

    // Append instruction bytes
    try module.code_bytes.appendSlice(allocator, self.code.items);

    std.sort.heap(Relocation, self.import_relocations.items, {}, struct {
        fn lessThan(_: void, a: Relocation, b: Relocation) bool {
            return a.codePos() < b.codePos();
        }
    }.lessThan);

    // Create relocation entries with absolute offsets.
    for (self.import_relocations.items) |reloc| {
        switch (reloc) {
            .index => |idx| {
                try module.reloc_code.entries.append(allocator, .{ .index = .{
                    .type_id = idx.type_id,
                    .offset = code_start + idx.code_pos,
                    .symbol_index = idx.symbol_index.raw(),
                } });
            },
            .offset => |off| {
                try module.reloc_code.entries.append(allocator, .{ .offset = .{
                    .type_id = off.type_id,
                    .offset = code_start + off.code_pos,
                    .symbol_index = off.symbol_index.raw(),
                    .addend = off.addend,
                } });
            },
        }
    }

    return fn_offset;
}

/// Prepend bytes to the code buffer and shift all relocation offsets accordingly.
///
/// Used for stack prologues and other code that must appear before the main
/// instruction bytes but whose content isn't known until after code generation
/// (e.g. because the stack frame size depends on what instructions were emitted).
pub fn prependToCode(self: *Self, allocator: Allocator, prefix: []const u8) Allocator.Error!void {
    if (prefix.len == 0) return;
    const prefix_len: u32 = @intCast(prefix.len);
    // Adjust relocation offsets to account for the prefix
    for (self.import_relocations.items) |*reloc| {
        reloc.shiftCodePos(prefix_len);
    }
    // Grow capacity if needed
    try self.code.ensureTotalCapacity(allocator, self.code.items.len + prefix.len);
    // Shift existing bytes right
    const old_len = self.code.items.len;
    self.code.items.len += prefix.len;
    std.mem.copyBackwards(u8, self.code.items[prefix.len..], self.code.items[0..old_len]);
    // Copy prefix into the beginning
    @memcpy(self.code.items[0..prefix.len], prefix);
}

/// Reset the builder for the next function without deallocating memory.
pub fn clear(self: *Self) void {
    self.code.clearRetainingCapacity();
    self.preamble.clearRetainingCapacity();
    self.import_relocations.clearRetainingCapacity();
}

// Tests

const testing = std.testing;

/// Create a minimal WasmModule for testing (only code_bytes, function_offsets,
/// and reloc_code are needed).
fn testModule() WasmModule {
    return WasmModule.init(testing.allocator);
}

test "CodeBuilder.emitRelocatableCall — records code_pos relative to function start" {
    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // Emit some dummy instructions first
    try cb.code.append(testing.allocator, WasmModule.Op.nop); // byte 0
    try cb.code.append(testing.allocator, WasmModule.Op.nop); // byte 1

    // Now emit a relocatable call — the call opcode lands at byte 2,
    // and the 5-byte placeholder starts at byte 3.
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(42), 12);

    try testing.expectEqual(@as(usize, 1), cb.import_relocations.items.len);
    // code_pos should be 3: after 2 nops + 1 call opcode byte
    try testing.expectEqual(@as(u32, 3), cb.import_relocations.items[0].index.code_pos);
    try testing.expectEqual(SymbolIndex.fromRaw(42), cb.import_relocations.items[0].index.symbol_index);

    // Total code: 2 nops + 1 call opcode + 5 padded bytes = 8
    try testing.expectEqual(@as(usize, 8), cb.code.items.len);
    // The call opcode should be at position 2
    try testing.expectEqual(WasmModule.Op.call, cb.code.items[2]);
    try testing.expectEqual(@as(u8, 0x8c), cb.code.items[3]);
    try testing.expectEqual(@as(u8, 0x80), cb.code.items[4]);
    try testing.expectEqual(@as(u8, 0x80), cb.code.items[5]);
    try testing.expectEqual(@as(u8, 0x80), cb.code.items[6]);
    try testing.expectEqual(@as(u8, 0x00), cb.code.items[7]);
}

test "CodeBuilder.insertIntoModule — function appended at correct code_bytes position" {
    var module = testModule();
    defer module.deinit();

    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // Simulate a simple preamble (1 byte: 0 local groups)
    try cb.preamble.append(testing.allocator, 0x00);
    // Simulate simple code: just an `end` opcode
    try cb.code.append(testing.allocator, WasmModule.Op.end);

    const fn_offset = try cb.insertIntoModule(testing.allocator, &module);

    // First function — should start at offset 0
    try testing.expectEqual(@as(u32, 0), fn_offset);
    // function_offsets should have one entry
    try testing.expectEqual(@as(usize, 1), module.function_offsets.items.len);
    try testing.expectEqual(@as(u32, 0), module.function_offsets.items[0]);

    // code_bytes: [body_len_leb128] [preamble: 0x00] [code: 0x0B]
    // body_len = 1 (preamble) + 1 (code) = 2, which encodes as single byte 0x02
    try testing.expectEqual(@as(usize, 3), module.code_bytes.items.len);
    try testing.expectEqual(@as(u8, 0x02), module.code_bytes.items[0]); // body length = 2
    try testing.expectEqual(@as(u8, 0x00), module.code_bytes.items[1]); // preamble
    try testing.expectEqual(WasmModule.Op.end, module.code_bytes.items[2]); // code
}

test "CodeBuilder.insertIntoModule — relocation offset accounts for body length prefix" {
    var module = testModule();
    defer module.deinit();

    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // Empty preamble (0 local groups)
    try cb.preamble.append(testing.allocator, 0x00);
    // Emit a relocatable call as the first instruction
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(7), 7);
    // End opcode
    try cb.code.append(testing.allocator, WasmModule.Op.end);

    _ = try cb.insertIntoModule(testing.allocator, &module);

    // Check the relocation was created with correct absolute offset.
    // code_bytes layout:
    //   [0]: body_len LEB128 (1 byte for small values)
    //   [1]: preamble (0x00)
    //   [2]: call opcode (0x10)
    //   [3..7]: 5-byte padded placeholder  <-- relocation points here
    //   [8]: end opcode
    try testing.expectEqual(@as(usize, 1), module.reloc_code.entries.items.len);
    const reloc = module.reloc_code.entries.items[0];
    const expected_offset: u32 = 1 + 1 + 1; // body_len_prefix(1) + preamble(1) + call_opcode(1)
    try testing.expectEqual(expected_offset, reloc.index.offset);
    try testing.expectEqual(@as(u32, 7), reloc.index.symbol_index);
}

test "CodeBuilder.insertIntoModule — relocation offset accounts for preamble size" {
    var module = testModule();
    defer module.deinit();

    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // Larger preamble: 5 bytes (simulating multiple local groups)
    try cb.preamble.appendSlice(testing.allocator, &.{ 0x02, 0x03, 0x7F, 0x01, 0x7E });
    // Emit a relocatable call
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(99), 99);
    try cb.code.append(testing.allocator, WasmModule.Op.end);

    _ = try cb.insertIntoModule(testing.allocator, &module);

    const reloc = module.reloc_code.entries.items[0];
    // body_len = 5 (preamble) + 6 (call) + 1 (end) = 12
    // 12 encodes as 1 byte LEB128
    // code_start = 1 (body_len) + 5 (preamble) = 6
    // reloc.code_pos = 1 (after call opcode byte)
    // absolute offset = 6 + 1 = 7
    const body_len_prefix_size: u32 = 1; // 12 fits in 1 LEB128 byte
    const preamble_size: u32 = 5;
    const code_pos: u32 = 1; // after the call opcode
    try testing.expectEqual(body_len_prefix_size + preamble_size + code_pos, reloc.index.offset);
}

test "CodeBuilder.insertIntoModule — multiple relocations in one function" {
    var module = testModule();
    defer module.deinit();

    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    try cb.preamble.append(testing.allocator, 0x00); // empty locals

    // First relocatable call
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(10), 10);
    // Some instructions between calls
    try cb.code.append(testing.allocator, WasmModule.Op.drop);
    // Second relocatable call
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(20), 20);
    // End
    try cb.code.append(testing.allocator, WasmModule.Op.end);

    _ = try cb.insertIntoModule(testing.allocator, &module);

    try testing.expectEqual(@as(usize, 2), module.reloc_code.entries.items.len);

    // code_start = 1 (body_len) + 1 (preamble) = 2
    const code_start: u32 = 2;

    // First relocation: code_pos = 1 (after call opcode at byte 0)
    const reloc0 = module.reloc_code.entries.items[0];
    try testing.expectEqual(code_start + 1, reloc0.index.offset);
    try testing.expectEqual(@as(u32, 10), reloc0.index.symbol_index);

    // Second relocation: call opcode at byte 7 (1 call + 5 padded + 1 drop), placeholder at 8
    const reloc1 = module.reloc_code.entries.items[1];
    try testing.expectEqual(code_start + 8, reloc1.index.offset);
    try testing.expectEqual(@as(u32, 20), reloc1.index.symbol_index);
}

test "CodeBuilder — two functions inserted sequentially have non-overlapping offsets" {
    var module = testModule();
    defer module.deinit();

    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // --- Function 1 ---
    try cb.preamble.append(testing.allocator, 0x00);
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(1), 1);
    try cb.code.append(testing.allocator, WasmModule.Op.end);
    const offset1 = try cb.insertIntoModule(testing.allocator, &module);
    cb.clear();

    // --- Function 2 ---
    try cb.preamble.append(testing.allocator, 0x00);
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(2), 2);
    try cb.code.append(testing.allocator, WasmModule.Op.end);
    const offset2 = try cb.insertIntoModule(testing.allocator, &module);

    // Function 2 must start after function 1 ends
    try testing.expect(offset2 > offset1);

    // Two function_offsets entries
    try testing.expectEqual(@as(usize, 2), module.function_offsets.items.len);

    // Relocation offsets must not overlap
    try testing.expectEqual(@as(usize, 2), module.reloc_code.entries.items.len);
    const reloc1_offset = module.reloc_code.entries.items[0].index.offset;
    const reloc2_offset = module.reloc_code.entries.items[1].index.offset;
    // Second reloc must be at a higher offset than the first
    try testing.expect(reloc2_offset > reloc1_offset);
    // And the gap should be at least 5 bytes (size of the padded LEB128 placeholder)
    try testing.expect(reloc2_offset >= reloc1_offset + 5);
}

test "CodeBuilder — clear resets state for next function without leaking relocations" {
    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // Populate with some data
    try cb.code.appendSlice(testing.allocator, &.{ 0x01, 0x02, 0x03 });
    try cb.preamble.appendSlice(testing.allocator, &.{0x00});
    try cb.import_relocations.append(testing.allocator, .{ .index = .{ .code_pos = 0, .symbol_index = SymbolIndex.fromRaw(5) } });

    cb.clear();

    try testing.expectEqual(@as(usize, 0), cb.code.items.len);
    try testing.expectEqual(@as(usize, 0), cb.preamble.items.len);
    try testing.expectEqual(@as(usize, 0), cb.import_relocations.items.len);

    // Capacity should be retained (not deallocated)
    try testing.expect(cb.code.capacity > 0);
    try testing.expect(cb.import_relocations.capacity > 0);
}

test "CodeBuilder.prependToCode — shifts relocations and inserts prefix" {
    var cb = Self.init();
    defer cb.deinit(testing.allocator);

    // Emit a relocatable call (call opcode at byte 0, placeholder at byte 1)
    try cb.emitRelocatableCall(testing.allocator, SymbolIndex.fromRaw(5), 5);
    try cb.code.append(testing.allocator, WasmModule.Op.end);

    // Before prepend: reloc code_pos = 1 (after call opcode)
    try testing.expectEqual(@as(u32, 1), cb.import_relocations.items[0].index.code_pos);

    // Prepend a 3-byte prologue
    try cb.prependToCode(testing.allocator, &.{ 0xAA, 0xBB, 0xCC });

    // After prepend: code is [AA, BB, CC, call, 5-byte-pad(5 bytes), end] = 10
    try testing.expectEqual(@as(usize, 10), cb.code.items.len);
    try testing.expectEqual(@as(u8, 0xAA), cb.code.items[0]);
    try testing.expectEqual(@as(u8, 0xBB), cb.code.items[1]);
    try testing.expectEqual(@as(u8, 0xCC), cb.code.items[2]);
    try testing.expectEqual(WasmModule.Op.call, cb.code.items[3]);
    try testing.expectEqual(WasmModule.Op.end, cb.code.items[9]);

    // Relocation code_pos should be shifted by 3
    try testing.expectEqual(@as(u32, 4), cb.import_relocations.items[0].index.code_pos);
}
