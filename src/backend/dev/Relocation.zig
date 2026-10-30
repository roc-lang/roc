//! Relocation types for the dev backend.
//!
//! Relocations represent places in the generated code that need to be
//! patched during linking to refer to the correct addresses.

const std = @import("std");

/// Producer-selected encoding of a linked function call.
pub const FunctionRelocationKind = enum(u8) {
    /// Offset names the four-byte operand following the E8 opcode.
    /// The displacement is relative to the end of that operand.
    x86_call_rel32,
    /// Offset names the BL instruction. Its signed 26-bit immediate counts
    /// four-byte words relative to the instruction address.
    aarch64_call26,
};

/// Machine encoding used for a data-address relocation in generated code.
pub const DataRelocationKind = enum {
    abs64,
    rel32,
    page21,
    pageoff12,
};

/// A number the Boxy runtime reads its sidecar tables by, loaded into a
/// register by a fixed-width instruction sequence at the relocation's offset.
/// An arranged image renumbers what its code uses (design.md "Deterministic Dev
/// Images") and patches each load, so these are never linker relocations.
pub const RuntimeNumber = union(enum) {
    /// A layout index.
    layout: u32,
    /// The start of a run of the program's erased argument layouts.
    erased_arg_layouts: struct { start: u32, len: u32 },
};

/// A named relocation at a linker or process boundary. Machine-code producers
/// use IndexedRelocation and carry its symbol-name column alongside the code.
pub const Relocation = union(enum) {
    /// Inline data that should be placed in the data section.
    /// The offset is where in the code the reference to this data appears.
    local_data: struct {
        /// Offset in the code where the data reference appears
        offset: u64,
        /// The actual data bytes to be placed in the data section
        data: []const u8,
    },

    /// Reference to a function that will be linked.
    /// Used for calls to external functions or other Roc procedures.
    linked_function: struct {
        /// Offset in the code where the function address should be patched
        offset: u64,
        /// Name of the function to link to
        name: []const u8,
        kind: FunctionRelocationKind,
    },

    /// Reference to data that will be linked.
    /// Used for references to global data or string literals.
    linked_data: struct {
        /// Offset in the code where the data address should be patched
        offset: u64,
        /// Name of the data symbol to link to
        name: []const u8,
        kind: DataRelocationKind = .abs64,
    },

    /// Get the offset in the code where this relocation applies
    pub fn getOffset(self: Relocation) u64 {
        return switch (self) {
            inline .local_data, .linked_function, .linked_data => |r| r.offset,
        };
    }

    /// Adjust the offset of this relocation by the given amount
    pub fn adjustOffset(self: *Relocation, delta: u64) void {
        switch (self.*) {
            inline .local_data, .linked_function, .linked_data => |*r| r.offset += delta,
        }
    }
};

/// A relocation whose producer has already assigned its target identity.
/// Symbol IDs are scoped to the accompanying SymbolTable name column.
pub const IndexedRelocation = union(enum) {
    linked_function: struct { offset: u64, symbol: @import("SymbolTable.zig").Id, kind: FunctionRelocationKind },
    linked_data: struct {
        offset: u64,
        symbol: @import("SymbolTable.zig").Id,
        kind: DataRelocationKind = .abs64,
    },
    local_data: @FieldType(Relocation, "local_data"),
    /// A runtime table number's patchable load; `offset` is the encoding's
    /// patch site (`CodeGen.patchRuntimeNumber`).
    runtime_number: struct { offset: u64, ref: RuntimeNumber },
    /// A relocation superseded by a stub that carries its own relocations.
    /// Consumers skip it; it keeps its slot so recorded relocation indices
    /// stay valid.
    retired,

    pub fn getOffset(self: IndexedRelocation) u64 {
        return switch (self) {
            inline .linked_function, .linked_data, .local_data, .runtime_number => |r| r.offset,
            .retired => 0,
        };
    }

    pub fn adjustOffset(self: *IndexedRelocation, delta: u64) void {
        switch (self.*) {
            inline .linked_function, .linked_data, .local_data, .runtime_number => |*r| r.offset += delta,
            .retired => {},
        }
    }
};

/// Function that resolves a symbol name to its address.
pub const SymbolResolver = *const fn (name: []const u8) ?usize;

/// Function that resolves a symbol name using caller-provided context.
pub const SymbolResolverContext = *const fn (ctx: *const anyopaque, name: []const u8) ?usize;

/// Errors that can occur when applying relocations to generated machine code.
pub const ApplyRelocationsError = error{
    UnresolvedSymbol,
    InvalidOffset,
    UnsupportedRelocationEncoding,
    MisalignedBranchTarget,
    BranchOutOfRange,
    OutOfMemory,
};

/// Apply relocations to a mutable code buffer.
/// The buffer should be writable. After this returns, the buffer can be
/// made executable via ExecutableMemory.
///
/// For x86_64 call instructions, this patches the 4-byte relative offset
/// after the E8 opcode.
pub fn applyRelocations(
    code: []u8,
    code_base_addr: usize,
    relocations: []const Relocation,
    resolver: SymbolResolver,
) ApplyRelocationsError!void {
    const ResolverBox = struct {
        resolver: SymbolResolver,

        fn resolve(ctx: *const anyopaque, name: []const u8) ?usize {
            const self: *const @This() = @ptrCast(@alignCast(ctx));
            return self.resolver(name);
        }
    };

    const box = ResolverBox{ .resolver = resolver };
    return applyRelocationsWithContext(code, code_base_addr, relocations, &box, ResolverBox.resolve);
}

/// Apply relocations using a resolver that receives explicit caller context.
pub fn applyRelocationsWithContext(
    code: []u8,
    code_base_addr: usize,
    relocations: []const Relocation,
    resolver_ctx: *const anyopaque,
    resolver: SymbolResolverContext,
) ApplyRelocationsError!void {
    for (relocations) |reloc| {
        switch (reloc) {
            .linked_function => |func_reloc| {
                const target_addr = resolver(resolver_ctx, func_reloc.name) orelse {
                    return error.UnresolvedSymbol;
                };
                try patchLinkedFunctionRelocation(code, code_base_addr, func_reloc.offset, target_addr, func_reloc.kind);
            },
            .linked_data => |data_reloc| {
                const target_addr = resolver(resolver_ctx, data_reloc.name) orelse {
                    return error.UnresolvedSymbol;
                };
                try patchLinkedDataRelocation(code, code_base_addr, data_reloc.offset, target_addr, data_reloc.kind);
            },
            .local_data => |local_reloc| {
                // The data's address is baked into the generated code, so the
                // referenced bytes must outlive that code—the caller owns
                // that lifetime.
                const target_addr = @intFromPtr(local_reloc.data.ptr);
                try patchLinkedDataRelocation(code, code_base_addr, local_reloc.offset, target_addr, .abs64);
            },
        }
    }
}

fn patchLinkedFunctionRelocation(code: []u8, code_base_addr: usize, reloc_offset_u64: u64, target_addr: usize, kind: FunctionRelocationKind) ApplyRelocationsError!void {
    const reloc_offset = try asCodeOffset(reloc_offset_u64, code.len);
    switch (kind) {
        .x86_call_rel32 => {
            std.debug.assert(reloc_offset > 0 and code[reloc_offset - 1] == 0xE8);
            return patchX86Rel32Operand(code, reloc_offset, code_base_addr + reloc_offset + 4, target_addr);
        },
        .aarch64_call26 => return patchAarch64Call26(code, reloc_offset, code_base_addr + reloc_offset, target_addr),
    }
}

fn patchLinkedDataRelocation(
    code: []u8,
    code_base_addr: usize,
    reloc_offset_u64: u64,
    target_addr: usize,
    kind: DataRelocationKind,
) ApplyRelocationsError!void {
    const reloc_offset = try asCodeOffset(reloc_offset_u64, code.len);

    switch (kind) {
        .abs64 => return patchAbsolutePointerOperand(code, reloc_offset, target_addr),
        .rel32 => {
            const next_instr = code_base_addr + reloc_offset + 4;
            return patchX86Rel32Operand(code, reloc_offset, next_instr, target_addr);
        },
        .page21 => return patchAarch64AdrpRelocation(code, reloc_offset, code_base_addr + reloc_offset, target_addr),
        .pageoff12 => return patchAarch64PageOffset12Relocation(code, reloc_offset, target_addr),
    }
}

fn patchX86Rel32Operand(code: []u8, operand_offset: usize, next_instr_addr: usize, target_addr: usize) ApplyRelocationsError!void {
    if (operand_offset + 4 > code.len) return error.InvalidOffset;

    const rel_i128 = @as(i128, @intCast(target_addr)) - @as(i128, @intCast(next_instr_addr));
    if (rel_i128 < std.math.minInt(i32) or rel_i128 > std.math.maxInt(i32)) return error.BranchOutOfRange;

    const rel: i32 = @intCast(rel_i128);
    const rel_bytes: [4]u8 = @bitCast(rel);
    @memcpy(code[operand_offset..][0..4], &rel_bytes);
}

fn patchAarch64Call26(code: []u8, inst_offset: usize, inst_addr: usize, target_addr: usize) ApplyRelocationsError!void {
    if (inst_offset + 4 > code.len) return error.InvalidOffset;

    var inst = std.mem.readInt(u32, code[inst_offset..][0..4], .little);
    const rel_bytes_i128 = @as(i128, @intCast(target_addr)) - @as(i128, @intCast(inst_addr));
    if ((rel_bytes_i128 & 0b11) != 0) return error.MisalignedBranchTarget;
    // Alignment is established above. Arithmetic shift expresses the signed
    // word displacement directly, without a debug-build i128 division libcall.
    const rel_words_i128 = rel_bytes_i128 >> 2;

    std.debug.assert((inst >> 26) == 0b100101);
    if (!fitsSignedBits(rel_words_i128, 26)) return error.BranchOutOfRange;
    const rel_words_i26: i26 = @intCast(rel_words_i128);
    const imm26: u26 = @bitCast(rel_words_i26);
    inst = (inst & 0xFC000000) | @as(u32, imm26);

    std.mem.writeInt(u32, code[inst_offset..][0..4], inst, .little);
}

fn patchAbsolutePointerOperand(code: []u8, operand_offset: usize, target_addr: usize) ApplyRelocationsError!void {
    if (@sizeOf(usize) == 8) {
        if (operand_offset + 8 > code.len) return error.InvalidOffset;
        std.mem.writeInt(u64, code[operand_offset..][0..8], @intCast(target_addr), .little);
        return;
    }

    if (@sizeOf(usize) == 4) {
        if (operand_offset + 4 > code.len) return error.InvalidOffset;
        if (target_addr > std.math.maxInt(u32)) return error.BranchOutOfRange;
        std.mem.writeInt(u32, code[operand_offset..][0..4], @intCast(target_addr), .little);
        return;
    }

    @compileError("Unsupported pointer size");
}

fn patchAarch64AdrpRelocation(code: []u8, inst_offset: usize, inst_addr: usize, target_addr: usize) ApplyRelocationsError!void {
    if (inst_offset + 4 > code.len) return error.InvalidOffset;

    var inst = std.mem.readInt(u32, code[inst_offset..][0..4], .little);
    if ((inst & 0x9F00_0000) != 0x9000_0000) return error.UnsupportedRelocationEncoding;

    const page_mask: usize = ~@as(usize, 0xFFF);
    const inst_page = inst_addr & page_mask;
    const target_page = target_addr & page_mask;
    // Both addresses have their low twelve bits cleared. This signed shift
    // is exact for backward as well as forward page displacements.
    const delta_pages_i128 = (@as(i128, @intCast(target_page)) - @as(i128, @intCast(inst_page))) >> 12;
    if (!fitsSignedBits(delta_pages_i128, 21)) return error.BranchOutOfRange;

    const delta_pages_i21: i21 = @intCast(delta_pages_i128);
    const imm21: u21 = @bitCast(delta_pages_i21);
    const immlo: u32 = @as(u32, imm21 & 0b11);
    const immhi: u32 = @as(u32, imm21 >> 2);

    inst &= ~(@as(u32, 0b11) << 29);
    inst &= ~(@as(u32, 0x7FFFF) << 5);
    inst |= immlo << 29;
    inst |= immhi << 5;

    std.mem.writeInt(u32, code[inst_offset..][0..4], inst, .little);
}

fn patchAarch64PageOffset12Relocation(code: []u8, inst_offset: usize, target_addr: usize) ApplyRelocationsError!void {
    if (inst_offset + 4 > code.len) return error.InvalidOffset;

    var inst = std.mem.readInt(u32, code[inst_offset..][0..4], .little);
    if ((inst & 0x7F00_0000) != 0x1100_0000) return error.UnsupportedRelocationEncoding;

    const lo12: u32 = @intCast(target_addr & 0xFFF);
    inst &= ~(@as(u32, 0xFFF) << 10);
    inst |= lo12 << 10;

    std.mem.writeInt(u32, code[inst_offset..][0..4], inst, .little);
}

fn asCodeOffset(offset: u64, code_len: usize) ApplyRelocationsError!usize {
    if (offset > std.math.maxInt(usize)) return error.InvalidOffset;
    const idx: usize = @intCast(offset);
    if (idx > code_len) return error.InvalidOffset;
    return idx;
}

fn fitsSignedBits(value: i128, comptime bits: comptime_int) bool {
    const min = -(@as(i128, 1) << (bits - 1));
    const max = (@as(i128, 1) << (bits - 1)) - 1;
    return value >= min and value <= max;
}

test "relocation creation" {
    const data = [_]u8{ 1, 2, 3, 4 };
    const local = Relocation{ .local_data = .{
        .offset = 100,
        .data = &data,
    } };
    try std.testing.expectEqual(@as(u64, 100), local.getOffset());

    const func = Relocation{ .linked_function = .{
        .offset = 200,
        .name = @import("builtins").shim_symbols.roc_alloc,
        .kind = .x86_call_rel32,
    } };
    try std.testing.expectEqual(@as(u64, 200), func.getOffset());
}

fn testNullResolver(_: []const u8) ?usize {
    return null;
}

fn readPointerFromCode(bytes: []const u8) usize {
    if (@sizeOf(usize) == 8) {
        return @intCast(std.mem.readInt(u64, bytes[0..8], .little));
    }

    if (@sizeOf(usize) == 4) {
        return @intCast(std.mem.readInt(u32, bytes[0..4], .little));
    }

    @compileError("Unsupported pointer size");
}

test "applyRelocations patches x86_64 linked_function call" {
    var code = [_]u8{ 0xE8, 0x00, 0x00, 0x00, 0x00 };
    const code_base: usize = 0x1000;
    const target_addr: usize = 0x1020;

    const resolver: SymbolResolver = struct {
        fn resolve(name: []const u8) ?usize {
            if (std.mem.eql(u8, name, "callee")) return target_addr;
            return null;
        }
    }.resolve;

    const relocs = [_]Relocation{
        .{ .linked_function = .{ .offset = 1, .name = "callee", .kind = .x86_call_rel32 } },
    };

    try applyRelocations(&code, code_base, &relocs, resolver);

    const patched = std.mem.readInt(i32, code[1..5], .little);
    try std.testing.expectEqual(@as(i32, 27), patched); // 0x1020 - (0x1000 + 5)
}

test "applyRelocations patches aarch64 linked_function bl" {
    var code = [_]u8{ 0x00, 0x00, 0x00, 0x94 }; // BL #0
    const code_base: usize = 0x2000;
    const target_addr: usize = code_base + 16;

    const resolver: SymbolResolver = struct {
        fn resolve(name: []const u8) ?usize {
            if (std.mem.eql(u8, name, "callee")) return target_addr;
            return null;
        }
    }.resolve;

    const relocs = [_]Relocation{
        .{ .linked_function = .{ .offset = 0, .name = "callee", .kind = .aarch64_call26 } },
    };

    try applyRelocations(&code, code_base, &relocs, resolver);

    const inst = std.mem.readInt(u32, &code, .little);
    try std.testing.expectEqual(@as(u32, 0b100101), inst >> 26);
    try std.testing.expectEqual(@as(u32, 4), inst & 0x03FF_FFFF); // 16-byte delta / 4
}

test "applyRelocations patches linked_data absolute pointer operand" {
    var code = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    const target_addr: usize = if (@sizeOf(usize) == 8) 0x1122334455667788 else 0x11223344;

    const resolver: SymbolResolver = struct {
        fn resolve(name: []const u8) ?usize {
            if (std.mem.eql(u8, name, "global_data")) return target_addr;
            return null;
        }
    }.resolve;

    const relocs = [_]Relocation{
        .{ .linked_data = .{ .offset = 4, .name = "global_data" } },
    };

    try applyRelocations(&code, 0, &relocs, resolver);
    try std.testing.expectEqual(target_addr, readPointerFromCode(code[4..]));
}

test "applyRelocations patches local_data pointer and stores bytes" {
    var code = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    const bytes = [_]u8{ 'a', 'b', 'c' };

    const relocs = [_]Relocation{
        .{ .local_data = .{ .offset = 0, .data = &bytes } },
    };

    try applyRelocations(&code, 0, &relocs, testNullResolver);

    const ptr_value = readPointerFromCode(code[0..]);
    try std.testing.expect(ptr_value != 0);
    const data_ptr: [*]const u8 = @ptrFromInt(ptr_value);
    try std.testing.expectEqualSlices(u8, &bytes, data_ptr[0..bytes.len]);
}

test "aarch64 signed branch and page displacements preserve exact units" {
    var code: [4]u8 = undefined;
    for ([_]i64{ -0x8000000, -4, 0, 4, 0x7fffffc }) |displacement| {
        std.mem.writeInt(u32, &code, 0x94000000, .little); // BL
        const base: usize = 0x8000000;
        const target: usize = @intCast(@as(i64, base) + displacement);
        try patchAarch64Call26(&code, 0, base, target);
        const words: i26 = @bitCast(@as(u26, @truncate(std.mem.readInt(u32, &code, .little))));
        try std.testing.expectEqual(displacement, @as(i64, words) * 4);
    }
    std.mem.writeInt(u32, &code, 0x94000000, .little);
    try std.testing.expectError(error.MisalignedBranchTarget, patchAarch64Call26(&code, 0, 0, 2));
    try std.testing.expectError(error.BranchOutOfRange, patchAarch64Call26(&code, 0, 0, 0x8000000));
    if (@sizeOf(usize) < 8) return;
    for ([_]i64{ -0x100000, -1, 0, 1, 0xfffff }) |pages| {
        std.mem.writeInt(u32, &code, 0x90000000, .little); // ADRP
        const base: usize = 0x100000000;
        const target: usize = @intCast(@as(i64, base) + pages * 4096 + 7);
        try patchAarch64AdrpRelocation(&code, 0, base + 3, target);
        const inst = std.mem.readInt(u32, &code, .little);
        const imm: u21 = @intCast(((inst >> 29) & 3) | (((inst >> 5) & 0x7ffff) << 2));
        try std.testing.expectEqual(pages, @as(i64, @as(i21, @bitCast(imm))));
    }
    std.mem.writeInt(u32, &code, 0x90000000, .little);
    try std.testing.expectError(error.BranchOutOfRange, patchAarch64AdrpRelocation(&code, 0, 0, 0x100000000));
}
