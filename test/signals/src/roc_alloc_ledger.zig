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
    recent_freed: [recent_freed_capacity]FreedAllocation = undefined,
    recent_freed_len: usize = 0,
    recent_freed_next: usize = 0,

    pub fn deinit(self: *Ledger, allocator: std.mem.Allocator) void {
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
        const ptr_addr = @intFromPtr(ptr);
        for (self.allocations.items, 0..) |alloc, index| {
            if (ptr_addr == @intFromPtr(alloc.user_ptr)) return index;
        }
        return null;
    }

    pub fn removeAt(self: *Ledger, index: usize) ?Allocation {
        if (index >= self.allocations.items.len) return null;
        return self.allocations.swapRemove(index);
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
        self.allocations.append(allocator, .{
            .user_ptr = user_ptr,
            .requested_size = requested_size,
            .allocated_size = allocated_size,
            .alignment = alignment,
        }) catch std.process.exit(1);
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

    const alloc = ledger.removeAt(0).?;
    ledger.recordFreed(alloc);

    try std.testing.expectEqual(@as(?usize, null), ledger.findExactIndex(bytes.ptr));
    try std.testing.expect(ledger.findRecentlyFreed(bytes.ptr) != null);
}
