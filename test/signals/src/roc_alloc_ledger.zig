const std = @import("std");

pub const Allocation = struct {
    user_ptr: [*]u8,
    requested_size: usize,
    allocated_size: usize,
    alignment: std.mem.Alignment,
};

pub const FreedAllocation = struct {
    user_ptr_addr: usize,
    requested_size: usize,
    allocated_size: usize,
    alignment: std.mem.Alignment,
};

pub const recent_freed_capacity = 4096;

pub const Ledger = struct {
    allocations: std.ArrayListUnmanaged(Allocation) = .empty,
    exact_indexes: std.AutoHashMapUnmanaged(usize, usize) = .empty,
    recent_freed: [recent_freed_capacity]FreedAllocation = undefined,
    recent_freed_len: usize = 0,
    recent_freed_next: usize = 0,

    pub fn deinit(self: *Ledger, allocator: std.mem.Allocator) void {
        self.exact_indexes.deinit(allocator);
        self.allocations.deinit(allocator);
    }

    pub fn findContainingIndex(self: *const Ledger, ptr: *anyopaque) ?usize {
        const ptr_addr = @intFromPtr(ptr);
        for (self.allocations.items, 0..) |alloc, index| {
            const user_addr = @intFromPtr(alloc.user_ptr);
            const end_addr = user_addr + alloc.allocated_size;
            if (ptr_addr >= user_addr and ptr_addr < end_addr) return index;
        }
        return null;
    }

    pub fn findExactIndex(self: *const Ledger, ptr: *anyopaque) ?usize {
        return self.exact_indexes.get(@intFromPtr(ptr));
    }

    pub fn removeAt(self: *Ledger, allocator: std.mem.Allocator, index: usize) ?Allocation {
        if (index >= self.allocations.items.len) return null;
        const removed = self.allocations.swapRemove(index);
        if (!self.exact_indexes.remove(@intFromPtr(removed.user_ptr))) {
            @panic("Roc allocation ledger exact index missing removed allocation");
        }
        if (index < self.allocations.items.len) {
            const moved = self.allocations.items[index];
            self.exact_indexes.put(allocator, @intFromPtr(moved.user_ptr), index) catch std.process.exit(1);
        }
        return removed;
    }

    pub fn recordFreed(self: *Ledger, alloc: Allocation) void {
        self.recent_freed[self.recent_freed_next] = .{
            .user_ptr_addr = @intFromPtr(alloc.user_ptr),
            .requested_size = alloc.requested_size,
            .allocated_size = alloc.allocated_size,
            .alignment = alloc.alignment,
        };
        self.recent_freed_next = (self.recent_freed_next + 1) % recent_freed_capacity;
        self.recent_freed_len = @min(self.recent_freed_len + 1, recent_freed_capacity);
    }

    pub fn findRecentlyFreed(self: *const Ledger, ptr: *anyopaque) ?FreedAllocation {
        const ptr_addr = @intFromPtr(ptr);
        for (self.recent_freed[0..self.recent_freed_len]) |alloc| {
            if (alloc.user_ptr_addr == ptr_addr) return alloc;
        }
        return null;
    }

    pub fn record(self: *Ledger, allocator: std.mem.Allocator, user_ptr: [*]u8, requested_size: usize, allocated_size: usize, alignment: std.mem.Alignment) void {
        const index = self.allocations.items.len;
        self.allocations.append(allocator, .{
            .user_ptr = user_ptr,
            .requested_size = requested_size,
            .allocated_size = allocated_size,
            .alignment = alignment,
        }) catch std.process.exit(1);
        const entry = self.exact_indexes.getOrPut(allocator, @intFromPtr(user_ptr)) catch std.process.exit(1);
        if (entry.found_existing) @panic("Roc allocation ledger recorded duplicate live pointer");
        entry.value_ptr.* = index;
    }
};

test "roc allocation ledger records exact and recently freed pointers" {
    const allocator = std.testing.allocator;
    var ledger: Ledger = .{};
    defer ledger.deinit(allocator);

    const bytes = try allocator.alloc(u8, 16);
    defer allocator.free(bytes);

    ledger.record(allocator, bytes.ptr, 16, 16, .@"8");
    try std.testing.expectEqual(@as(?usize, 0), ledger.findExactIndex(bytes.ptr));
    try std.testing.expectEqual(@as(?usize, 0), ledger.findContainingIndex(bytes.ptr + 4));

    const alloc = ledger.removeAt(allocator, 0).?;
    ledger.recordFreed(alloc);

    try std.testing.expectEqual(@as(?usize, null), ledger.findExactIndex(bytes.ptr));
    try std.testing.expect(ledger.findRecentlyFreed(bytes.ptr) != null);
}

test "roc allocation ledger keeps exact indexes after middle removal" {
    const allocator = std.testing.allocator;
    var ledger: Ledger = .{};
    defer ledger.deinit(allocator);

    const first = try allocator.alloc(u8, 8);
    defer allocator.free(first);
    const middle = try allocator.alloc(u8, 16);
    defer allocator.free(middle);
    const last = try allocator.alloc(u8, 24);
    defer allocator.free(last);

    ledger.record(allocator, first.ptr, first.len, first.len, .@"8");
    ledger.record(allocator, middle.ptr, middle.len, middle.len, .@"8");
    ledger.record(allocator, last.ptr, last.len, last.len, .@"8");

    const removed = ledger.removeAt(allocator, 1).?;
    try std.testing.expectEqual(@intFromPtr(middle.ptr), @intFromPtr(removed.user_ptr));

    try std.testing.expectEqual(@as(?usize, 0), ledger.findExactIndex(first.ptr));
    try std.testing.expectEqual(@as(?usize, null), ledger.findExactIndex(middle.ptr));
    try std.testing.expectEqual(@as(?usize, 1), ledger.findExactIndex(last.ptr));
}
