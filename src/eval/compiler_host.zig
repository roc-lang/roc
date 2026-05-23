//! RocOps environment used by compiler-owned evaluation.

const std = @import("std");
const builtins = @import("builtins");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

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

fn rocAlloc(args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(env));
    const ptr = allocateBytes(self.allocator, args.length, args.alignment);
    self.allocations.put(@intFromPtr(ptr), .{
        .size = args.length,
        .alignment = args.alignment,
    }) catch @panic("OOM");
    args.answer = @ptrCast(ptr);
}

fn rocDealloc(args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(env));
    const removed = self.allocations.fetchRemove(@intFromPtr(args.ptr)) orelse
        @panic("compiler RocOps deallocated unknown pointer");
    freeBytes(self.allocator, args.ptr, removed.value);
}

fn rocRealloc(args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(env));
    const old_ptr = args.answer;
    const removed = self.allocations.fetchRemove(@intFromPtr(old_ptr)) orelse
        @panic("compiler RocOps reallocated unknown pointer");

    const new_ptr = allocateBytes(self.allocator, args.new_length, args.alignment);
    const old_bytes: [*]u8 = @ptrCast(@alignCast(old_ptr));
    @memcpy(new_ptr[0..@min(removed.value.size, args.new_length)], old_bytes[0..@min(removed.value.size, args.new_length)]);
    freeBytes(self.allocator, old_ptr, removed.value);

    self.allocations.put(@intFromPtr(new_ptr), .{
        .size = args.new_length,
        .alignment = args.alignment,
    }) catch @panic("OOM");
    args.answer = @ptrCast(new_ptr);
}

fn rocDbg(_: *const RocDbg, _: *anyopaque) callconv(.c) void {}

fn rocExpectFailed(args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(env));
    if (self.expect_message) |msg| self.allocator.free(msg);
    self.expect_message = self.allocator.dupe(u8, args.utf8_bytes[0..args.len]) catch null;
}

fn rocCrashed(args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(env));
    if (self.crash_message) |msg| self.allocator.free(msg);
    self.crash_message = self.allocator.dupe(u8, args.utf8_bytes[0..args.len]) catch null;
}

fn allocateBytes(allocator: std.mem.Allocator, len: usize, alignment: usize) [*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch @panic("OOM")).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch @panic("OOM")).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch @panic("OOM")).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch @panic("OOM")).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch @panic("OOM")).ptr,
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
