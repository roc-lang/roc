//! Relocation types for the dev backend.
//!
//! Relocations represent places in the generated code that need to be
//! patched during linking to refer to the correct addresses.

const std = @import("std");

/// A relocation that needs to be applied during linking.
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
    },

    /// Reference to data that will be linked.
    /// Used for references to global data or string literals.
    linked_data: struct {
        /// Offset in the code where the data address should be patched
        offset: u64,
        /// Name of the data symbol to link to
        name: []const u8,
    },

    /// A jump to the function's return sequence.
    /// Used for early returns in the middle of a function.
    jmp_to_return: struct {
        /// Location of the jump instruction
        inst_loc: u64,
        /// Size of the jump instruction (for calculating relative offset)
        inst_size: u64,
        /// Offset to the return sequence from the start of the function
        offset: u64,
    },

    /// Get the offset in the code where this relocation applies
    pub fn getOffset(self: Relocation) u64 {
        return switch (self) {
            .local_data => |r| r.offset,
            .linked_function => |r| r.offset,
            .linked_data => |r| r.offset,
            .jmp_to_return => |r| r.inst_loc,
        };
    }

    /// Adjust all offset fields by a given delta.
    /// Used when prepending prologue code to shift relocations forward.
    pub fn adjustOffset(self: *Relocation, delta: usize) void {
        switch (self.*) {
            .local_data => |*ld| ld.offset += delta,
            .linked_function => |*lf| lf.offset += delta,
            .linked_data => |*ld| ld.offset += delta,
            .jmp_to_return => |*jr| {
                jr.inst_loc += delta;
                jr.offset += delta;
            },
        }
    }
};

/// Function that resolves a symbol name to its address.
pub const SymbolResolver = *const fn (name: []const u8) ?usize;

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
) error{UnresolvedSymbol}!void {
    for (relocations) |reloc| {
        switch (reloc) {
            .linked_function => |func_reloc| {
                const target_addr = resolver(func_reloc.name) orelse {
                    return error.UnresolvedSymbol;
                };
                // Calculate relative offset for x86_64 call instruction
                // The call is relative to the instruction after the call (offset + 4)
                const offset: usize = @intCast(func_reloc.offset);
                const call_site = code_base_addr + offset;
                const rel_offset: i32 = @intCast(@as(i64, @intCast(target_addr)) - @as(i64, @intCast(call_site + 4)));
                const offset_bytes: [4]u8 = @bitCast(rel_offset);
                @memcpy(code[offset..][0..4], &offset_bytes);
            },
            .local_data => {
                // Local data relocations not yet supported
            },
            .linked_data => {
                // Linked data relocations not yet supported
            },
            .jmp_to_return => {
                // Jump relocations not yet supported
            },
        }
    }
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
        .name = "roc_alloc",
    } };
    try std.testing.expectEqual(@as(u64, 200), func.getOffset());
}
