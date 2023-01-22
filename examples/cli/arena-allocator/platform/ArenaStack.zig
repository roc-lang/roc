const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArenaStack = @This();

allocator: *Allocator,
items: ArrayList(ArenaAllocator),

const DEBUG = false;

fn debug_info(comptime format: []const u8, args: anytype) void {
    if (DEBUG) {
        std.io.getStdOut()
            .writer()
            .print(format, args)
            catch unreachable;
    }
}

pub fn init(allocator: *Allocator) ArenaStack {
    return ArenaStack {
        .allocator = allocator,
        .items = ArrayList(ArenaAllocator).init(allocator),
    };
}

pub fn deinit(self: ArenaStack) void {
    self.items.deinit();
}

pub fn push(self: *ArenaStack) void {
    std.io.getStdOut().writer().print("[push]\n", .{}) catch unreachable;

    self.items.append(ArenaAllocator.init(self.allocator)) catch unreachable;
}

pub fn pop(self: *ArenaStack) void {
    std.io.getStdOut().writer().print("[pop]\n", .{}) catch unreachable;

    self.items.items[self.items.items.len - 1].deinit();
    self.items.shrinkRetainingCapacity(self.items.items.len - 1);
}

fn current_allocator(self: ArenaStack) ?*std.mem.Allocator {
    const items = self.items.items;
    if (items.len == 0) {
        return null;
    } else {
        return &items[items.len - 1].allocator;
    }
}

pub fn alloc(
    self: *ArenaStack,
    size: usize,
    alignment: u32,
) ?*c_void {
    const ptr = if (self.current_allocator()) |allocator|
        @ptrCast(
            ?*c_void,
            allocator.alloc(u8, size) catch unreachable,
        )
    else
        std.c.malloc(size);

    debug_info("alloc:   {d} (alignment {d}, size {d})\n", .{ ptr, alignment, size });

    return ptr;
}

pub fn realloc(
    self: *ArenaStack,
    old_ptr: *c_void,
    new_size: usize,
    old_size: usize,
    alignment: u32,
) ?*c_void {
    debug_info("realloc: {d} (alignment {d}, old_size {d})\n", .{ old_ptr, alignment, old_size });

    if (self.current_allocator()) |allocator| {
        var old_slice: []u8 = @ptrCast([*]u8, old_ptr)[0..old_size];

        return @ptrCast(
            ?*c_void,
            allocator.realloc(old_slice, new_size) catch unreachable,
        );
    } else {
        return std.c.realloc(old_ptr, new_size);
    }
}

pub fn dealloc(
    self: *ArenaStack,
    ptr: *c_void,
    alignment: u32,
) void {
    debug_info("dealloc: {d} (alignment {d})\n", .{ ptr, alignment });

    if (self.current_allocator()) |allocator| {
        // memory will be freed later with `pop` function
        _ = allocator;
    } else {
        std.c.free(ptr);
    }
}
