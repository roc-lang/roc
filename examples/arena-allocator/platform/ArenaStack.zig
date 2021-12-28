const std = @import("std");
const allocator = std.heap.page_allocator;
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArenaStack = @This();

items: ArrayList(ArenaAllocator),

pub fn init() ArenaStack {
    return ArenaStack {
        .items = ArrayList(ArenaAllocator).init(allocator),
    };
}

pub fn push(self: *ArenaStack) void {
    self.items.append(ArenaAllocator.init(allocator)) catch unreachable;
}

pub fn pop(self: *ArenaStack) void {
    self.items.shrinkRetainingCapacity(self.items.items.len - 1);
}
