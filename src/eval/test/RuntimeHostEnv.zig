//! Production-faithful RocOps recorder for eval test harnesses.
//!
//! This env records exactly what a real host can observe through `host_abi`:
//! callback kind, raw UTF-8 payload bytes, event order, and crash termination.
//! It also tracks allocations made through RocOps so tests can detect leaks or
//! clean up any surviving runtime allocations at the end of a run.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtins = @import("builtins");
const sljmp = @import("sljmp");

const RocOps = builtins.host_abi.RocOps;
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

const RuntimeHostEnv = @This();

const AllocationInfo = struct {
    size: usize,
    alignment: usize,
};

/// Poison value written to the refcount slot on free in debug builds. Mirrors
/// `src/builtins/utils.zig` so double-free / use-after-free bugs fail loudly.
const POISON_VALUE: isize = @bitCast(if (@sizeOf(usize) == 8)
    @as(usize, 0xDEADBEEFDEADBEEF)
else
    @as(usize, 0xDEADBEEF));

/// Public enum `Termination`.
pub const Termination = enum {
    returned,
    crashed,
};

/// Public union `HostEvent`.
pub const HostEvent = union(enum) {
    dbg: []u8,
    expect_failed: []u8,
    crashed: []u8,

    pub fn bytes(self: HostEvent) []const u8 {
        return switch (self) {
            .dbg => |msg| msg,
            .expect_failed => |msg| msg,
            .crashed => |msg| msg,
        };
    }

    pub fn deinit(self: *HostEvent, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .dbg => |msg| allocator.free(msg),
            .expect_failed => |msg| allocator.free(msg),
            .crashed => |msg| allocator.free(msg),
        }
    }
};

/// Public struct `RecordedRun`.
pub const RecordedRun = struct {
    events: []HostEvent,
    termination: Termination,
    allocation_count: u32,

    pub fn dupe(self: RecordedRun, allocator: std.mem.Allocator) Allocator.Error!RecordedRun {
        var out = try allocator.alloc(HostEvent, self.events.len);
        errdefer allocator.free(out);

        for (self.events, 0..) |event, i| {
            out[i] = switch (event) {
                .dbg => |msg| .{ .dbg = try allocator.dupe(u8, msg) },
                .expect_failed => |msg| .{ .expect_failed = try allocator.dupe(u8, msg) },
                .crashed => |msg| .{ .crashed = try allocator.dupe(u8, msg) },
            };
        }

        return .{
            .events = out,
            .termination = self.termination,
            .allocation_count = self.allocation_count,
        };
    }

    pub fn deinit(self: *RecordedRun, allocator: std.mem.Allocator) void {
        for (self.events) |*event| event.deinit(allocator);
        allocator.free(self.events);
    }
};

/// Public union `CrashState`.
pub const CrashState = union(enum) {
    did_not_crash,
    crashed: []const u8,
};

/// Public value `LeakError`.
pub const LeakError = error{MemoryLeak};

allocator: std.mem.Allocator,
roc_ops: ?RocOps = null,
jmp_buf: JmpBuf = undefined,
active_jmp_buf: ?*JmpBuf = null,
termination: Termination = .returned,
events: std.ArrayListUnmanaged(HostEvent) = .empty,
allocation_tracker: std.AutoHashMap(usize, AllocationInfo),
allocation_call_count: u32 = 0,
longjmp_on_crash: bool = true,

pub fn init(allocator: std.mem.Allocator) RuntimeHostEnv {
    // The allocation_tracker grows from inside rocAllocFn, which on Windows
    // is invoked from JIT'd dev-backend code. Stack-capturing allocators
    // (testing.allocator) crash inside walkStackWindows when unwinding
    // through JIT memory that lacks Windows unwind data, so the tracker
    // uses a non-tracing allocator regardless of what was passed in.
    return .{
        .allocator = allocator,
        .allocation_tracker = std.AutoHashMap(usize, AllocationInfo).init(runtime_bytes_allocator),
    };
}

pub fn deinit(self: *RuntimeHostEnv) void {
    self.resetObservation();
    self.freeRemainingAllocations();
    self.allocation_tracker.deinit();
}

/// Public function `resetObservation`.
pub fn resetObservation(self: *RuntimeHostEnv) void {
    for (self.events.items) |*event| event.deinit(self.allocator);
    self.events.clearAndFree(self.allocator);
    self.termination = .returned;
    self.active_jmp_buf = null;
}

/// Public function `resetAllocationTracker`.
pub fn resetAllocationTracker(self: *RuntimeHostEnv) void {
    self.freeRemainingAllocations();
    self.allocation_tracker.clearRetainingCapacity();
    self.allocation_call_count = 0;
}

/// Public function `checkForLeaks`.
pub fn checkForLeaks(self: *RuntimeHostEnv) LeakError!void {
    if (self.allocation_tracker.count() > 0) return error.MemoryLeak;
}

/// Public function `allocationCallCount`.
pub fn allocationCallCount(self: *const RuntimeHostEnv) u32 {
    return self.allocation_call_count;
}

/// Controls whether the crash callback exits through the active crash boundary.
pub fn setLongjmpOnCrash(self: *RuntimeHostEnv, enabled: bool) void {
    self.longjmp_on_crash = enabled;
}

/// Public function `get_ops`.
pub fn get_ops(self: *RuntimeHostEnv) *RocOps {
    if (self.roc_ops == null) {
        self.roc_ops = .{
            .env = @ptrCast(self),
            .roc_alloc = rocAllocFn,
            .roc_dealloc = rocDeallocFn,
            .roc_realloc = rocReallocFn,
            .roc_dbg = rocDbgFn,
            .roc_expect_failed = rocExpectFailedFn,
            .roc_crashed = rocCrashedFn,
            .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
        };
    }
    return &self.roc_ops.?;
}

/// Public function `terminationState`.
pub fn terminationState(self: *const RuntimeHostEnv) Termination {
    return self.termination;
}

/// Public function `crashState`.
pub fn crashState(self: *const RuntimeHostEnv) CrashState {
    for (0..self.events.items.len) |i| {
        const idx = self.events.items.len - 1 - i;
        switch (self.events.items[idx]) {
            .crashed => |msg| return .{ .crashed = msg },
            else => {},
        }
    }
    return .did_not_crash;
}

/// Public function `snapshot`.
pub fn snapshot(self: *const RuntimeHostEnv, allocator: std.mem.Allocator) Allocator.Error!RecordedRun {
    return RecordedRun.dupe(.{
        .events = self.events.items,
        .termination = self.termination,
        .allocation_count = @intCast(self.allocation_tracker.count()),
    }, allocator);
}

/// Public struct `CrashBoundary`.
pub const CrashBoundary = struct {
    env: *RuntimeHostEnv,
    prev_jmp_buf: ?*JmpBuf,

    pub fn init(env: *RuntimeHostEnv) CrashBoundary {
        return .{
            .env = env,
            .prev_jmp_buf = env.installJumpBuf(&env.jmp_buf),
        };
    }

    pub fn deinit(self: *CrashBoundary) void {
        self.env.restoreJumpBuf(self.prev_jmp_buf);
    }

    pub inline fn set(self: *CrashBoundary) c_int {
        return setjmp(&self.env.jmp_buf);
    }
};

/// Public function `enterCrashBoundary`.
pub fn enterCrashBoundary(self: *RuntimeHostEnv) CrashBoundary {
    return CrashBoundary.init(self);
}

fn installJumpBuf(self: *RuntimeHostEnv, jmp_buf: *JmpBuf) ?*JmpBuf {
    const prev = self.active_jmp_buf;
    self.active_jmp_buf = jmp_buf;
    return prev;
}

fn restoreJumpBuf(self: *RuntimeHostEnv, prev: ?*JmpBuf) void {
    self.active_jmp_buf = prev;
}

fn appendEvent(
    self: *RuntimeHostEnv,
    comptime tag: std.meta.Tag(HostEvent),
    bytes: []const u8,
) void {
    const owned = self.allocator.dupe(u8, bytes) catch {
        std.debug.panic("RuntimeHostEnv: failed to allocate host event payload", .{});
    };
    self.events.append(self.allocator, @unionInit(HostEvent, @tagName(tag), owned)) catch {
        self.allocator.free(owned);
        std.debug.panic("RuntimeHostEnv: failed to append host event", .{});
    };
}

fn rocDbgFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *RuntimeHostEnv = @ptrCast(@alignCast(ops.env));
    self.appendEvent(.dbg, bytes[0..len]);
}

fn rocExpectFailedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *RuntimeHostEnv = @ptrCast(@alignCast(ops.env));
    self.appendEvent(.expect_failed, bytes[0..len]);
}

fn rocCrashedFn(ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *RuntimeHostEnv = @ptrCast(@alignCast(ops.env));
    self.appendEvent(.crashed, bytes[0..len]);
    self.termination = .crashed;

    if (self.longjmp_on_crash) {
        if (self.active_jmp_buf) |active_jmp_buf| {
            self.active_jmp_buf = null;
            longjmp(active_jmp_buf, 1);
        }
    }
}

fn rocAllocFn(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const self: *RuntimeHostEnv = @ptrCast(@alignCast(ops.env));
    self.allocation_call_count += 1;
    const alloc_ptr = allocateTrackedBytes(self.allocator, length, alignment);
    self.allocation_tracker.put(@intFromPtr(alloc_ptr), .{
        .size = length,
        .alignment = alignment,
    }) catch {
        std.debug.panic("RuntimeHostEnv: failed to track allocation", .{});
    };
    return @ptrCast(alloc_ptr);
}

fn rocDeallocFn(ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *RuntimeHostEnv = @ptrCast(@alignCast(ops.env));
    const alloc_ptr = @intFromPtr(ptr);
    const alloc_info = self.allocation_tracker.fetchRemove(alloc_ptr) orelse {
        std.debug.panic("RuntimeHostEnv: double-free or untracked free at ptr=0x{x}", .{alloc_ptr});
    };

    if (alloc_info.value.size >= @sizeOf(isize)) {
        const refcount_ptr: *isize = @ptrCast(@alignCast(ptr));
        refcount_ptr.* = POISON_VALUE;
    }

    freeTrackedBytes(self.allocator, ptr, alloc_info.value);
}

fn rocReallocFn(ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const self: *RuntimeHostEnv = @ptrCast(@alignCast(ops.env));
    self.allocation_call_count += 1;
    const old_alloc_ptr = @intFromPtr(ptr);
    const old_info = self.allocation_tracker.fetchRemove(old_alloc_ptr) orelse {
        std.debug.panic("RuntimeHostEnv: realloc of untracked memory at ptr=0x{x}", .{old_alloc_ptr});
    };

    const new_base_ptr = allocateTrackedBytes(undefined, new_length, alignment);
    const old_bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    const copy_size = @min(old_info.value.size, new_length);
    @memcpy(new_base_ptr[0..copy_size], old_bytes[0..copy_size]);

    freeTrackedBytes(undefined, ptr, old_info.value);

    self.allocation_tracker.put(@intFromPtr(new_base_ptr), .{
        .size = new_length,
        .alignment = alignment,
    }) catch {
        std.debug.panic("RuntimeHostEnv: failed to track reallocation", .{});
    };

    return @ptrCast(new_base_ptr);
}

// Use a non-tracing allocator for Roc runtime bytes. On Windows the
// dev-backend JIT emits code without unwind data, so a stack-capturing
// allocator (e.g. DebugAllocator/testing.allocator) crashes inside
// walkStackWindows when an alloc happens from JIT'd code. RuntimeHostEnv
// owns its own allocation_tracker for leak detection, so we don't need the
// underlying allocator to track.
const runtime_bytes_allocator: std.mem.Allocator = if (@import("builtin").target.os.tag == .freestanding)
    std.heap.wasm_allocator
else
    std.heap.smp_allocator;

fn allocateTrackedBytes(_: std.mem.Allocator, len: usize, alignment: usize) [*]u8 {
    const allocator = runtime_bytes_allocator;
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch oom("roc_alloc")).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch oom("roc_alloc")).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch oom("roc_alloc")).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch oom("roc_alloc")).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch oom("roc_alloc")).ptr,
        else => std.debug.panic("RuntimeHostEnv: unsupported alignment {d}", .{alignment}),
    };
}

fn freeTrackedBytes(_: std.mem.Allocator, ptr: *anyopaque, alloc_info: AllocationInfo) void {
    const allocator = runtime_bytes_allocator;
    const bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    switch (alloc_info.alignment) {
        1 => allocator.free(bytes[0..alloc_info.size]),
        2 => allocator.free((@as([*]align(2) u8, @alignCast(bytes)))[0..alloc_info.size]),
        4 => allocator.free((@as([*]align(4) u8, @alignCast(bytes)))[0..alloc_info.size]),
        8 => allocator.free((@as([*]align(8) u8, @alignCast(bytes)))[0..alloc_info.size]),
        16 => allocator.free((@as([*]align(16) u8, @alignCast(bytes)))[0..alloc_info.size]),
        else => std.debug.panic("RuntimeHostEnv: unsupported free alignment {d}", .{alloc_info.alignment}),
    }
}

fn freeRemainingAllocations(self: *RuntimeHostEnv) void {
    var iterator = self.allocation_tracker.iterator();
    while (iterator.next()) |entry| {
        const ptr: *anyopaque = @ptrFromInt(entry.key_ptr.*);
        freeTrackedBytes(self.allocator, ptr, entry.value_ptr.*);
    }
}

fn oom(comptime context: []const u8) noreturn {
    std.debug.panic("RuntimeHostEnv: out of memory during {s}", .{context});
}

test "RuntimeHostEnv records raw dbg and expect payloads exactly" {
    var env = RuntimeHostEnv.init(std.testing.allocator);
    defer env.deinit();

    const ops = env.get_ops();

    const dbg_msg = "\"hello\"";
    const expect_msg = "expect failed";
    ops.roc_dbg(ops, dbg_msg.ptr, dbg_msg.len);
    ops.roc_expect_failed(ops, expect_msg.ptr, expect_msg.len);

    try std.testing.expectEqual(@as(usize, 2), env.events.items.len);
    try std.testing.expectEqualStrings(dbg_msg, env.events.items[0].bytes());
    try std.testing.expectEqualStrings(expect_msg, env.events.items[1].bytes());
    try std.testing.expectEqual(Termination.returned, env.terminationState());
}

test "RuntimeHostEnv records crash payload and termination without a jump buffer" {
    var env = RuntimeHostEnv.init(std.testing.allocator);
    defer env.deinit();

    const ops = env.get_ops();
    const crash_msg = "boom";
    ops.roc_crashed(ops, crash_msg.ptr, crash_msg.len);

    try std.testing.expectEqual(@as(usize, 1), env.events.items.len);
    try std.testing.expectEqualStrings(crash_msg, env.events.items[0].bytes());
    try std.testing.expectEqual(Termination.crashed, env.terminationState());
    switch (env.crashState()) {
        .did_not_crash => return error.TestUnexpectedResult,
        .crashed => |msg| try std.testing.expectEqualStrings(crash_msg, msg),
    }
}
