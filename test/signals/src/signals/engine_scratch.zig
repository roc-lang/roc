const std = @import("std");
const descriptor_stream = @import("descriptor_stream.zig");
const retained_values = @import("retained_values.zig");
const structural_splice = @import("structural_splice.zig");

pub const Scratch = struct {
    debug_seen_render_nodes: std.ArrayListUnmanaged(bool) = .empty,
    debug_expected_children: std.ArrayListUnmanaged(u64) = .empty,
    binder_stack: std.ArrayListUnmanaged(descriptor_stream.BinderBinding) = .empty,
    each_keys: std.ArrayListUnmanaged(retained_values.HostValue) = .empty,
    each_key_hashes: std.ArrayListUnmanaged(u64) = .empty,
    each_next_hash_heads: std.AutoHashMapUnmanaged(u64, usize) = .empty,
    each_next_hash_links: std.ArrayListUnmanaged(usize) = .empty,
    each_matched_existing: std.ArrayListUnmanaged(bool) = .empty,
    replacement_target_scopes: std.ArrayListUnmanaged(bool) = .empty,
    elem_owned_removal: structural_splice.ElemOwnedRemovalScratch = .{},

    pub fn deinit(self: *Scratch, allocator: std.mem.Allocator) void {
        self.debug_seen_render_nodes.deinit(allocator);
        self.debug_expected_children.deinit(allocator);
        self.binder_stack.deinit(allocator);
        self.each_keys.deinit(allocator);
        self.each_key_hashes.deinit(allocator);
        self.each_next_hash_heads.deinit(allocator);
        self.each_next_hash_links.deinit(allocator);
        self.each_matched_existing.deinit(allocator);
        self.replacement_target_scopes.deinit(allocator);
        self.elem_owned_removal.deinit(allocator);
        self.* = .{};
    }
};

test "engine scratch deinit resets retained scratch storage" {
    const allocator = std.testing.allocator;
    var scratch = Scratch{};

    try scratch.debug_seen_render_nodes.append(allocator, true);
    try scratch.debug_expected_children.append(allocator, 42);
    try scratch.each_keys.append(allocator, 7);
    try scratch.each_key_hashes.append(allocator, 99);
    try scratch.each_next_hash_heads.put(allocator, 5, 0);
    try scratch.each_next_hash_links.append(allocator, 1);
    try scratch.each_matched_existing.append(allocator, false);
    try scratch.replacement_target_scopes.append(allocator, true);

    scratch.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 0), scratch.debug_seen_render_nodes.items.len);
    try std.testing.expectEqual(@as(usize, 0), scratch.debug_expected_children.items.len);
    try std.testing.expectEqual(@as(usize, 0), scratch.each_keys.items.len);
    try std.testing.expectEqual(@as(usize, 0), scratch.each_key_hashes.items.len);
    try std.testing.expectEqual(@as(usize, 0), scratch.each_next_hash_heads.count());
    try std.testing.expectEqual(@as(usize, 0), scratch.each_next_hash_links.items.len);
    try std.testing.expectEqual(@as(usize, 0), scratch.each_matched_existing.items.len);
    try std.testing.expectEqual(@as(usize, 0), scratch.replacement_target_scopes.items.len);
}
