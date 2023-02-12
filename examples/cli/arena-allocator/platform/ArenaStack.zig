const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const allocator = std.heap.page_allocator;
const ArenaStack = @This();

items: ArrayList(ArenaAllocator),

const DEBUG = false;

fn debug_info(comptime format: []const u8, args: anytype) void {
    if (DEBUG) {
        std.io.getStdOut()
            .writer()
            .print(format, args) catch unreachable;
    }
}

pub fn init() ArenaStack {
    return ArenaStack{
        .items = ArrayList(ArenaAllocator).init(allocator),
    };
}

pub fn deinit(self: ArenaStack) void {
    self.items.deinit();
}

pub fn push(self: *ArenaStack) void {
    std.io.getStdOut().writer().print("[push]\n", .{}) catch unreachable;

    self.items.append(ArenaAllocator.init(std.heap.page_allocator)) catch unreachable;
}

pub fn pop(self: *ArenaStack) void {
    std.io.getStdOut().writer().print("[pop]\n", .{}) catch unreachable;

    self.items.items[self.items.items.len - 1].deinit();
    self.items.shrinkRetainingCapacity(self.items.items.len - 1);
}

fn current_allocator(self: ArenaStack) *const std.mem.Allocator {
    const items = self.items.items;
    if (items.len == 0) {
        return &allocator;
    } else {
        return &items[items.len - 1].allocator();
    }
}

pub fn alloc(
    self: *ArenaStack,
    size: usize,
    alignment: u32,
) ?*anyopaque {
    const ptr = @ptrCast(
        ?*anyopaque,
        self.current_allocator().alloc(u8, size) catch unreachable
    );
    debug_info("alloc:   {d} (alignment {d}, size {d})\n", .{ ptr, alignment, size });

    return ptr;
}

pub fn realloc(
    self: *ArenaStack,
    old_ptr: *anyopaque,
    new_size: usize,
    old_size: usize,
    alignment: u32,
) ?*anyopaque {
    debug_info("realloc: {d} (alignment {d}, old_size {d})\n", .{ old_ptr, alignment, old_size });

    var old_slice: []u8 = @ptrCast([*]u8, old_ptr)[0..old_size];

    return @ptrCast(
        ?*anyopaque,
        self.current_allocator().realloc(old_slice, new_size) catch unreachable
    );
}

pub fn dealloc(
    self: *ArenaStack,
    ptr: *anyopaque,
    alignment: u32,
) void {
    debug_info("dealloc: {d} (alignment {d})\n", .{ ptr, alignment });

    if (self.items.items.len == 0) {
        // This allocator doesn't need to be given the correct size
        std.heap.page_allocator.free(@ptrCast([*]u8, ptr)[0..1]);
    } else {
        // Memory allocated with an arena allocator will be freed later
    }
}
