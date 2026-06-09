//! RocOps environment used by compiler-owned evaluation.

const std = @import("std");
const builtins = @import("builtins");

const RocOps = builtins.host_abi.RocOps;

const CompilerHost = @This();

const Allocation = struct {
    size: usize,
    alignment: usize,
};

allocator: std.mem.Allocator,
allocations: std.AutoHashMap(usize, Allocation),
roc_ops: ?RocOps = null,
crash_message: ?[]u8 = null,
expect_message: ?[]u8 = null,

pub fn init(allocator: std.mem.Allocator) CompilerHost {
    return .{
        .allocator = allocator,
        .allocations = std.AutoHashMap(usize, Allocation).init(allocator),
    };
}

pub fn deinit(self: *CompilerHost) void {
    self.freeRemainingAllocations();
    self.allocations.deinit();
    if (self.crash_message) |msg| self.allocator.free(msg);
    if (self.expect_message) |msg| self.allocator.free(msg);
    self.* = CompilerHost.init(self.allocator);
}

/// Return the RocOps table used by compiler-owned evaluation.
pub fn ops(self: *CompilerHost) *RocOps {
    if (self.roc_ops == null) {
        self.roc_ops = .{
            .env = @ptrCast(self),
            .roc_alloc = rocAlloc,
            .roc_dealloc = rocDealloc,
            .roc_realloc = rocRealloc,
            .roc_dbg = rocDbg,
            .roc_expect_failed = rocExpectFailed,
            .roc_crashed = rocCrashed,
            .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
        };
    }
    return &self.roc_ops.?;
}

fn rocAlloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    const allocation: Allocation = .{ .size = length, .alignment = alignment };
    const ptr = allocateBytes(self.allocator, length, alignment) orelse {
        // OOM: signal failure to the caller (the interpreter turns this into a
        // Roc crash) instead of aborting the whole compiler.
        return null;
    };
    self.allocations.put(@intFromPtr(ptr), allocation) catch {
        freeBytes(self.allocator, ptr, allocation);
        return null;
    };
    return @ptrCast(ptr);
}

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    const removed = self.allocations.fetchRemove(@intFromPtr(ptr)) orelse
        @panic("compiler RocOps deallocated unknown pointer");
    freeBytes(self.allocator, ptr, removed.value);
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    const old_ptr = ptr;
    const allocation: Allocation = .{ .size = new_length, .alignment = alignment };

    // Allocate the new block before touching the tracking map, so a failure
    // leaves the old allocation intact and tracked.
    const new_ptr = allocateBytes(self.allocator, new_length, alignment) orelse {
        return null;
    };
    const removed = self.allocations.fetchRemove(@intFromPtr(old_ptr)) orelse
        @panic("compiler RocOps reallocated unknown pointer");
    const old_bytes: [*]u8 = @ptrCast(@alignCast(old_ptr));
    @memcpy(new_ptr[0..@min(removed.value.size, new_length)], old_bytes[0..@min(removed.value.size, new_length)]);
    freeBytes(self.allocator, old_ptr, removed.value);

    self.allocations.put(@intFromPtr(new_ptr), allocation) catch {
        freeBytes(self.allocator, new_ptr, allocation);
        return null;
    };
    return @ptrCast(new_ptr);
}

fn rocDbg(_: *RocOps, _: [*]const u8, _: usize) callconv(.c) void {}

fn rocExpectFailed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    if (self.expect_message) |msg| self.allocator.free(msg);
    self.expect_message = self.allocator.dupe(u8, bytes[0..len]) catch null;
}

fn rocCrashed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    if (self.crash_message) |msg| self.allocator.free(msg);
    self.crash_message = self.allocator.dupe(u8, bytes[0..len]) catch null;
}

/// Returns null on allocation failure (OOM); the caller signals that to the
/// evaluator rather than aborting the compiler.
fn allocateBytes(allocator: std.mem.Allocator, len: usize, alignment: usize) ?[*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch return null).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch return null).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch return null).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch return null).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch return null).ptr,
        else => @panic("unsupported compiler RocOps allocation alignment"),
    };
}

fn freeBytes(allocator: std.mem.Allocator, ptr: *anyopaque, allocation: Allocation) void {
    const bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    switch (allocation.alignment) {
        1 => allocator.free(bytes[0..allocation.size]),
        2 => allocator.free((@as([*]align(2) u8, @alignCast(bytes)))[0..allocation.size]),
        4 => allocator.free((@as([*]align(4) u8, @alignCast(bytes)))[0..allocation.size]),
        8 => allocator.free((@as([*]align(8) u8, @alignCast(bytes)))[0..allocation.size]),
        16 => allocator.free((@as([*]align(16) u8, @alignCast(bytes)))[0..allocation.size]),
        else => @panic("unsupported compiler RocOps free alignment"),
    }
}

fn freeRemainingAllocations(self: *CompilerHost) void {
    var iterator = self.allocations.iterator();
    while (iterator.next()) |entry| {
        const ptr: *anyopaque = @ptrFromInt(entry.key_ptr.*);
        freeBytes(self.allocator, ptr, entry.value_ptr.*);
    }
    self.allocations.clearRetainingCapacity();
}

test "compiler host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
