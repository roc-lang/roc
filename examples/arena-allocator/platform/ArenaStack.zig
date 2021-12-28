const std = @import("std");
const ArenaStack = @This();

items: std.ArrayList(std.heap.ArenaAllocator),

pub fn init() ArenaStack {
    return ArenaStack {
        items: std.ArrayList.init(std.heap.page_allocator),
    }
}
