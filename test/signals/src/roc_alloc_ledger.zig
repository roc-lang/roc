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

pub const AllocResult = struct {
    ptr: *anyopaque,
    allocated_size: usize,
};

pub const ReallocResult = struct {
    ptr: *anyopaque,
    allocated_size: usize,
    freed: Allocation,
};

pub const DeallocError = error{
    LedgerIndexOutOfBounds,
    InteriorPointer,
    AlreadyFreedWithDifferentAlignment,
    AlreadyFreed,
    UnknownPointer,
    AlignmentMismatch,
};

pub const ReallocError = error{
    LedgerIndexOutOfBounds,
    InteriorPointer,
    AlreadyFreedWithDifferentAlignment,
    AlreadyFreed,
    UnknownPointer,
    AlignmentMismatch,
};

pub const recent_freed_capacity = 4096;

pub fn allocatedSizeForRequest(length: usize) usize {
    return if (length == 0) 1 else length;
}

pub fn alignmentFromAbi(alignment: usize) std.mem.Alignment {
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    return std.mem.Alignment.fromByteUnits(min_alignment);
}

pub fn deallocErrorMessage(err: DeallocError) []const u8 {
    return switch (err) {
        error.LedgerIndexOutOfBounds => "Roc allocation ledger index is out of bounds",
        error.InteriorPointer => "roc_dealloc received an interior pointer instead of the pointer returned by roc_alloc",
        error.AlreadyFreedWithDifferentAlignment => "roc_dealloc received an already freed pointer with a different alignment",
        error.AlreadyFreed => "roc_dealloc received a pointer that was already freed",
        error.UnknownPointer => "roc_dealloc received an unknown pointer",
        error.AlignmentMismatch => "roc_dealloc alignment did not match the tracked allocation",
    };
}

pub fn reallocErrorMessage(err: ReallocError) []const u8 {
    return switch (err) {
        error.LedgerIndexOutOfBounds => "Roc allocation ledger index is out of bounds",
        error.InteriorPointer => "roc_realloc received an interior pointer instead of the pointer returned by roc_alloc",
        error.AlreadyFreedWithDifferentAlignment => "roc_realloc received an already freed pointer with a different alignment",
        error.AlreadyFreed => "roc_realloc received a pointer that was already freed",
        error.UnknownPointer => "roc_realloc received an unknown pointer",
        error.AlignmentMismatch => "roc_realloc alignment did not match the tracked allocation",
    };
}

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

    pub fn allocate(self: *Ledger, ledger_allocator: std.mem.Allocator, backing_allocator: std.mem.Allocator, length: usize, alignment_arg: usize, ret_addr: usize) ?AllocResult {
        const alignment = alignmentFromAbi(alignment_arg);
        const allocated_size = allocatedSizeForRequest(length);
        const user_ptr = backing_allocator.rawAlloc(allocated_size, alignment, ret_addr) orelse std.process.exit(1);
        self.record(ledger_allocator, user_ptr, length, allocated_size, alignment);
        return .{
            .ptr = @ptrCast(user_ptr),
            .allocated_size = allocated_size,
        };
    }

    pub fn deallocate(self: *Ledger, ledger_allocator: std.mem.Allocator, backing_allocator: std.mem.Allocator, ptr: *anyopaque, alignment_arg: usize, ret_addr: usize) DeallocError!Allocation {
        const alloc = try self.removeForDealloc(ledger_allocator, ptr, alignment_arg);
        backing_allocator.rawFree(alloc.user_ptr[0..alloc.allocated_size], alloc.alignment, ret_addr);
        return alloc;
    }

    pub fn reallocate(self: *Ledger, ledger_allocator: std.mem.Allocator, backing_allocator: std.mem.Allocator, ptr: *anyopaque, new_length: usize, alignment_arg: usize, ret_addr: usize) ReallocError!ReallocResult {
        const pending = try self.beginRealloc(ptr, alignment_arg);

        const alignment = alignmentFromAbi(alignment_arg);
        const allocated_size = allocatedSizeForRequest(new_length);
        const new_user_ptr = backing_allocator.rawAlloc(allocated_size, alignment, ret_addr) orelse std.process.exit(1);
        self.record(ledger_allocator, new_user_ptr, new_length, allocated_size, alignment);

        const old_user_ptr: [*]const u8 = @ptrCast(ptr);
        const copy_size = @min(pending.allocation.requested_size, new_length);
        @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);

        const freed = self.finishRealloc(ledger_allocator, pending) catch |err| switch (err) {
            error.LedgerIndexOutOfBounds => return error.LedgerIndexOutOfBounds,
        };
        backing_allocator.rawFree(freed.user_ptr[0..freed.allocated_size], freed.alignment, ret_addr);

        return .{
            .ptr = @ptrCast(new_user_ptr),
            .allocated_size = allocated_size,
            .freed = freed,
        };
    }

    const PendingRealloc = struct {
        index: usize,
        allocation: Allocation,
    };

    fn removeForDealloc(self: *Ledger, ledger_allocator: std.mem.Allocator, ptr: *anyopaque, alignment_arg: usize) DeallocError!Allocation {
        const alignment = alignmentFromAbi(alignment_arg);
        const index = self.findExactIndex(ptr) orelse {
            if (self.findContainingIndex(ptr) != null) return error.InteriorPointer;
            if (self.findRecentlyFreed(ptr)) |freed| {
                if (freed.alignment != alignment) return error.AlreadyFreedWithDifferentAlignment;
                return error.AlreadyFreed;
            }
            return error.UnknownPointer;
        };
        if (self.allocations.items[index].alignment != alignment) return error.AlignmentMismatch;
        const alloc = self.removeAt(ledger_allocator, index) orelse return error.LedgerIndexOutOfBounds;
        self.recordFreed(alloc);
        return alloc;
    }

    fn beginRealloc(self: *Ledger, ptr: *anyopaque, alignment_arg: usize) ReallocError!PendingRealloc {
        const alignment = alignmentFromAbi(alignment_arg);
        const index = self.findExactIndex(ptr) orelse {
            if (self.findContainingIndex(ptr) != null) return error.InteriorPointer;
            if (self.findRecentlyFreed(ptr)) |freed| {
                if (freed.alignment != alignment) return error.AlreadyFreedWithDifferentAlignment;
                return error.AlreadyFreed;
            }
            return error.UnknownPointer;
        };
        const allocation = self.allocations.items[index];
        if (allocation.alignment != alignment) return error.AlignmentMismatch;
        return .{
            .index = index,
            .allocation = allocation,
        };
    }

    fn finishRealloc(self: *Ledger, ledger_allocator: std.mem.Allocator, pending: PendingRealloc) error{LedgerIndexOutOfBounds}!Allocation {
        const alloc = self.removeAt(ledger_allocator, pending.index) orelse return error.LedgerIndexOutOfBounds;
        self.recordFreed(alloc);
        return alloc;
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

test "roc allocation ledger allocates reallocates and frees backing memory" {
    const allocator = std.testing.allocator;
    var ledger: Ledger = .{};
    defer ledger.deinit(allocator);

    const first = ledger.allocate(allocator, allocator, 8, 8, @returnAddress()).?;
    const first_ptr: [*]u8 = @ptrCast(first.ptr);
    first_ptr[0] = 42;

    const grown = try ledger.reallocate(allocator, allocator, first.ptr, 32, 8, @returnAddress());
    const grown_ptr: [*]u8 = @ptrCast(grown.ptr);
    try std.testing.expectEqual(@as(u8, 42), grown_ptr[0]);
    try std.testing.expectEqual(@as(?usize, null), ledger.findExactIndex(first.ptr));
    try std.testing.expect(ledger.findExactIndex(grown.ptr) != null);
    try std.testing.expect(ledger.findRecentlyFreed(first.ptr) != null);

    _ = try ledger.deallocate(allocator, allocator, grown.ptr, 8, @returnAddress());
    try std.testing.expectEqual(@as(usize, 0), ledger.allocations.items.len);
    try std.testing.expect(ledger.findRecentlyFreed(grown.ptr) != null);
}

test "roc allocation ledger classifies dealloc diagnostics" {
    const allocator = std.testing.allocator;
    var ledger: Ledger = .{};
    defer ledger.deinit(allocator);

    const alloc = ledger.allocate(allocator, allocator, 16, 8, @returnAddress()).?;
    const alloc_ptr: [*]u8 = @ptrCast(alloc.ptr);

    try std.testing.expectError(error.InteriorPointer, ledger.deallocate(allocator, allocator, alloc_ptr + 1, 8, @returnAddress()));
    try std.testing.expectError(error.AlignmentMismatch, ledger.deallocate(allocator, allocator, alloc.ptr, 16, @returnAddress()));

    _ = try ledger.deallocate(allocator, allocator, alloc.ptr, 8, @returnAddress());
    try std.testing.expectError(error.AlreadyFreed, ledger.deallocate(allocator, allocator, alloc.ptr, 8, @returnAddress()));
    try std.testing.expectError(error.AlreadyFreedWithDifferentAlignment, ledger.deallocate(allocator, allocator, alloc.ptr, 16, @returnAddress()));
}
