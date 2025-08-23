//! NodeSlices provides a compact storage for slices of node indices.
//! This can be parameterized for different node index types.

const std = @import("std");
const collections = @import("mod.zig");
const Allocator = std.mem.Allocator;

/// Creates a parameterized NodeSlices type for a given index type
pub fn NodeSlices(comptime NodeIdx: type) type {
    return struct {
        entries: collections.SafeList(Entry),

        const Self = @This();

        pub const Idx = enum(i32) {
            _,

            // NIL sentinel for empty slices - sign bit set (i32 min value)
            pub const NIL: Idx = @enumFromInt(std.math.minInt(i32));

            fn asUsize(self: Idx) usize {
                return @intCast(@intFromEnum(self));
            }

            pub fn isNil(self: Idx) bool {
                return self == NIL;
            }
        };

        pub const Entry = union {
            node_idx: NodeIdx, // An individual NodeIdx in a slice. The last one will be negative to mark the end.
            binop_lhs: NodeIdx, // This is a BinOp's lhs node, and its rhs will be stored immediately after this entry.
            binop_rhs: NodeIdx, // This is a BinOp's rhs node, and its lhs will be stored immediately before this entry.
        };

        pub fn append(self: *Self, allocator: Allocator, node_slice: []const NodeIdx) Allocator.Error!Idx {
            // Handle empty slices by returning NIL
            if (node_slice.len == 0) {
                return Idx.NIL;
            }

            // OPTIMIZATION: For single elements, encode the NodeIdx directly with sign bit set
            // This saves 4 bytes by avoiding an indirection through NodeSlices
            if (node_slice.len == 1) {
                const node_val = @intFromEnum(node_slice[0]);
                // To avoid conflict with NIL (which is minInt(i32)), we need to ensure
                // we never produce that exact value. We'll add 1 before encoding.
                // This means valid single elements will be in range [minInt(i32)+1, -1]
                const shifted_val = node_val + 1;
                // Set the sign bit to indicate this is a direct NodeIdx, not a NodeSlices.Idx
                const encoded_val = shifted_val | std.math.minInt(i32);
                return @as(Idx, @enumFromInt(encoded_val));
            }

            const idx = @as(Idx, @enumFromInt(self.entries.items.items.len));

            // Reserve capacity for all nodes (no length stored anymore)
            try self.entries.items.ensureUnusedCapacity(allocator, node_slice.len);

            // Append all nodes except the last one
            for (node_slice[0 .. node_slice.len - 1]) |node| {
                self.entries.items.appendAssumeCapacity(.{ .node_idx = node });
            }

            // Append the last node with sign bit set to mark the end
            const last_node = node_slice[node_slice.len - 1];
            const last_val = @intFromEnum(last_node);
            // Set the sign bit to mark this as the last element
            const sign_bit = std.math.minInt(i32);
            const marked_val = last_val | sign_bit;
            const marked_last = @as(NodeIdx, @enumFromInt(marked_val));
            self.entries.items.appendAssumeCapacity(.{ .node_idx = marked_last });

            return idx;
        }

        pub fn appendBinOp(self: *Self, allocator: Allocator, lhs: NodeIdx, rhs: NodeIdx) Allocator.Error!Idx {
            const idx = @as(Idx, @enumFromInt(self.entries.items.items.len));

            // Reserve capacity for both nodes
            try self.entries.items.ensureUnusedCapacity(allocator, 2);

            self.entries.items.appendAssumeCapacity(.{ .binop_lhs = lhs });
            self.entries.items.appendAssumeCapacity(.{ .binop_rhs = rhs });

            return idx;
        }

        pub const Iterator = struct {
            entries: []const Entry,
            index: usize,
            done: bool,
            single_element: ?NodeIdx, // For single-element optimization

            pub fn next(it: *Iterator) ?NodeIdx {
                if (it.done) return null;

                // Handle single-element case
                if (it.single_element) |node| {
                    it.done = true;
                    // Clear the sign bit and subtract 1 to get the original node
                    const node_val = @intFromEnum(node);
                    const sign_bit = std.math.minInt(i32);
                    const cleared_val = node_val & ~@as(i32, sign_bit);
                    const original_val = cleared_val - 1; // Undo the shift we applied during encoding
                    return @as(NodeIdx, @enumFromInt(original_val));
                }

                // Bounds check
                if (it.index >= it.entries.len) {
                    it.done = true;
                    return null;
                }

                const node_idx = it.entries[it.index].node_idx;
                const node_val = @intFromEnum(node_idx);

                it.index += 1;

                // Check if sign bit is set (marking the end)
                if (node_val < 0) {
                    it.done = true;
                    // Clear the sign bit to get the original value
                    const sign_bit = std.math.minInt(i32);
                    const original_val = node_val & ~@as(i32, sign_bit);
                    return @as(NodeIdx, @enumFromInt(original_val));
                }

                return node_idx;
            }
        };

        pub fn nodes(self: *const Self, idx_ptr: *const Idx) Iterator {
            const idx = idx_ptr.*;

            // Check for NIL sentinel (empty slice)
            if (idx == Idx.NIL) {
                return .{
                    .entries = self.entries.items.items,
                    .index = 0,
                    .done = true, // Mark as done immediately for empty list
                    .single_element = null,
                };
            }

            // Check if this is a single element encoded directly (sign bit set)
            const idx_val = @intFromEnum(idx);
            if (idx_val < 0) {
                // This is a single NodeIdx encoded directly (negative value due to sign bit)
                // Store it for the iterator to return
                return .{
                    .entries = self.entries.items.items,
                    .index = 0,
                    .done = false,
                    .single_element = @as(NodeIdx, @enumFromInt(idx_val)), // Store with sign bit still set
                };
            }

            // Multiple elements - start reading from entries
            const slice_start = @as(usize, @intCast(idx_val));

            return .{
                .entries = self.entries.items.items,
                .index = slice_start,
                .done = false,
                .single_element = null,
            };
        }

        pub const BinOp = struct {
            lhs: NodeIdx,
            rhs: NodeIdx,
        };

        pub fn binOp(self: *const Self, idx: Idx) BinOp {
            // BinOps should never be single elements or NIL
            std.debug.assert(@intFromEnum(idx) >= 0); // Not encoded directly

            const entry_idx = @as(usize, @intCast(@intFromEnum(idx)));
            // The binop entries are stored as .binop_lhs and .binop_rhs
            // We need to extract the actual node indices from these entries
            const lhs = self.entries.items.items[entry_idx].binop_lhs;
            const rhs = self.entries.items.items[entry_idx + 1].binop_rhs;
            return .{
                .lhs = lhs,
                .rhs = rhs,
            };
        }
    };
}