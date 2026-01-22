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
};

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
