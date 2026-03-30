//! An implementation of RocOps for testing purposes.

const std = @import("std");
const builtins = @import("builtins");
const eval_mod = @import("eval");

const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const CrashContext = eval_mod.CrashContext;
const CrashState = eval_mod.CrashState;

/// An implementation of RocOps for testing purposes.
pub const TestEnv = struct {
    allocator: std.mem.Allocator,
    crash: CrashContext,
    allocations: std.AutoHashMap(usize, AllocationInfo),
    roc_ops: RocOps,

    const AllocationInfo = struct {
        size: usize,
        alignment: usize,
    };

    pub fn init(allocator: std.mem.Allocator) TestEnv {
        return TestEnv{
            .allocator = allocator,
            .crash = CrashContext.init(allocator),
            .allocations = std.AutoHashMap(usize, AllocationInfo).init(allocator),
            .roc_ops = RocOps{
                .env = undefined, // set below
                .roc_alloc = testRocAlloc,
                .roc_dealloc = testRocDealloc,
                .roc_realloc = testRocRealloc,
                .roc_dbg = testRocDbg,
                .roc_expect_failed = testRocExpectFailed,
                .roc_crashed = testRocCrashed,
                .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
            },
        };
    }

    pub fn deinit(self: *TestEnv) void {
        self.allocations.deinit();
        self.crash.deinit();
    }

    pub fn get_ops(self: *TestEnv) *RocOps {
        self.roc_ops.env = @ptrCast(self);
        self.crash.reset();
        return &self.roc_ops;
    }

    pub fn crashState(self: *TestEnv) CrashState {
        return self.crash.state;
    }

    pub fn crashContextPtr(self: *TestEnv) *CrashContext {
        return &self.crash;
    }
};

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const alloc_ptr = allocateTrackedBytes(test_env.allocator, alloc_args.length, alloc_args.alignment);
    alloc_args.answer = @ptrCast(alloc_ptr);
    test_env.allocations.put(@intFromPtr(alloc_ptr), .{
        .size = alloc_args.length,
        .alignment = alloc_args.alignment,
    }) catch {
        std.debug.panic("Failed to track allocation in repl test env", .{});
    };
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const alloc_ptr = @intFromPtr(dealloc_args.ptr);
    const alloc_info = test_env.allocations.fetchRemove(alloc_ptr) orelse {
        std.debug.panic("Attempted to free untracked allocation in repl test env: 0x{x}", .{alloc_ptr});
    };
    freeTrackedBytes(test_env.allocator, dealloc_args.ptr, alloc_info.value);
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const old_alloc_ptr = @intFromPtr(realloc_args.answer);
    const old_info = test_env.allocations.fetchRemove(old_alloc_ptr) orelse {
        std.debug.panic("Attempted to realloc untracked allocation in repl test env: 0x{x}", .{old_alloc_ptr});
    };

    const new_alloc_ptr = allocateTrackedBytes(test_env.allocator, realloc_args.new_length, realloc_args.alignment);
    const old_bytes: [*]u8 = @ptrCast(@alignCast(realloc_args.answer));
    const copy_size = @min(old_info.value.size, realloc_args.new_length);
    @memcpy(new_alloc_ptr[0..copy_size], old_bytes[0..copy_size]);
    freeTrackedBytes(test_env.allocator, realloc_args.answer, old_info.value);
    realloc_args.answer = @ptrCast(new_alloc_ptr);
    test_env.allocations.put(@intFromPtr(new_alloc_ptr), .{
        .size = realloc_args.new_length,
        .alignment = realloc_args.alignment,
    }) catch {
        std.debug.panic("Failed to track reallocation in repl test env", .{});
    };
}

fn testRocDbg(_: *const RocDbg, _: *anyopaque) callconv(.c) void {
    @panic("testRocDbg not implemented yet");
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    const source_bytes = expect_args.utf8_bytes[0..expect_args.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    // Format and record the message
    const formatted = std.fmt.allocPrint(test_env.allocator, "Expect failed: {s}", .{trimmed}) catch {
        std.debug.panic("failed to allocate REPL expect failure message", .{});
    };
    test_env.crash.recordCrash(formatted) catch |err| {
        test_env.allocator.free(formatted);
        std.debug.panic("failed to store REPL expect failure: {}", .{err});
    };
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.c) void {
    const test_env: *TestEnv = @ptrCast(@alignCast(env));
    test_env.crash.recordCrash(crashed_args.utf8_bytes[0..crashed_args.len]) catch |err| {
        std.debug.panic("failed to store REPL crash message: {}", .{err});
    };
}

fn allocateTrackedBytes(allocator: std.mem.Allocator, len: usize, alignment: usize) [*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch oom("testRocAlloc")).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch oom("testRocAlloc")).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch oom("testRocAlloc")).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch oom("testRocAlloc")).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch oom("testRocAlloc")).ptr,
        else => std.debug.panic("Unsupported alignment in repl test env: {d}", .{alignment}),
    };
}

fn freeTrackedBytes(allocator: std.mem.Allocator, ptr: *anyopaque, alloc_info: TestEnv.AllocationInfo) void {
    const bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    switch (alloc_info.alignment) {
        1 => allocator.free(bytes[0..alloc_info.size]),
        2 => allocator.free((@as([*]align(2) u8, @alignCast(bytes)))[0..alloc_info.size]),
        4 => allocator.free((@as([*]align(4) u8, @alignCast(bytes)))[0..alloc_info.size]),
        8 => allocator.free((@as([*]align(8) u8, @alignCast(bytes)))[0..alloc_info.size]),
        16 => allocator.free((@as([*]align(16) u8, @alignCast(bytes)))[0..alloc_info.size]),
        else => std.debug.panic("Unsupported free alignment in repl test env: {d}", .{alloc_info.alignment}),
    }
}

fn oom(comptime context: []const u8) noreturn {
    std.debug.panic("Out of memory during {s}", .{context});
}
