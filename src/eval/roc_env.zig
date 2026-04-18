//! Shared RocEnv for managing RocOps callbacks.
//!
//! Provides an allocator-backed RocOps environment that works for any backend.
//! Tracks allocations so realloc/dealloc and evaluator teardown can release
//! memory correctly under leak-checking test allocators.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const is_freestanding = builtin.target.os.tag == .freestanding;

const Allocator = std.mem.Allocator;

// Host ABI types for RocOps
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

const debugPrint = if (is_freestanding)
    struct {
        fn print(comptime fmt: []const u8, args: anytype) void {
            _ = fmt;
            _ = args;
        }
    }.print
else
    struct {
        fn print(comptime fmt: []const u8, args: anytype) void {
            std.debug.print(fmt, args);
        }
    }.print;

/// Environment for RocOps in evaluators.
/// Tracks allocator-owned buffers so generated code can use roc_alloc /
/// roc_realloc / roc_dealloc without leaking under test allocators.
pub const RocEnv = struct {
    allocator: Allocator,
    /// Track allocation metadata so realloc/dealloc can free correctly.
    allocations: std.AutoHashMapUnmanaged(usize, AllocInfo) = .{},

    const AllocInfo = struct {
        len: usize,
        alignment: usize,
    };

    pub fn init(allocator: Allocator) RocEnv {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *RocEnv) void {
        var iterator = self.allocations.iterator();
        while (iterator.next()) |entry| {
            freeTrackedAllocation(self.allocator, @as(*anyopaque, @ptrFromInt(entry.key_ptr.*)), entry.value_ptr.*);
        }
        self.allocations.deinit(self.allocator);
    }

    pub fn leakCount(self: *const RocEnv) usize {
        return self.allocations.count();
    }

    pub fn reportLeaks(self: *const RocEnv) void {
        var iterator = self.allocations.iterator();
        while (iterator.next()) |entry| {
            debugPrint(
                "RocEnv leak: ptr=0x{x} len={d} align={d}\n",
                .{ entry.key_ptr.*, entry.value_ptr.len, entry.value_ptr.alignment },
            );
        }
    }

    /// Allocation function for RocOps.
    pub fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const self: *RocEnv = @ptrCast(@alignCast(env));

        // Allocate memory with the requested alignment
        const ptr = switch (roc_alloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, .@"1", roc_alloc.length),
            2 => self.allocator.alignedAlloc(u8, .@"2", roc_alloc.length),
            4 => self.allocator.alignedAlloc(u8, .@"4", roc_alloc.length),
            8 => self.allocator.alignedAlloc(u8, .@"8", roc_alloc.length),
            16 => self.allocator.alignedAlloc(u8, .@"16", roc_alloc.length),
            else => @panic("RocEnv: Unsupported alignment"),
        } catch {
            @panic("RocEnv: Allocation failed");
        };

        roc_alloc.answer = @ptrCast(ptr.ptr);
        self.allocations.put(self.allocator, @intFromPtr(ptr.ptr), .{
            .len = roc_alloc.length,
            .alignment = roc_alloc.alignment,
        }) catch {};
    }

    /// Deallocation function for RocOps.
    pub fn rocDeallocFn(roc_dealloc: *RocDealloc, env: *anyopaque) callconv(.c) void {
        const self: *RocEnv = @ptrCast(@alignCast(env));
        const ptr = @intFromPtr(roc_dealloc.ptr);
        const alloc_info = self.allocations.fetchRemove(ptr) orelse return;
        freeTrackedAllocation(self.allocator, roc_dealloc.ptr, alloc_info.value);
    }

    /// Reallocation function for RocOps.
    pub fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const self: *RocEnv = @ptrCast(@alignCast(env));

        // Allocate new memory with the requested alignment
        const new_ptr = switch (roc_realloc.alignment) {
            1 => self.allocator.alignedAlloc(u8, .@"1", roc_realloc.new_length),
            2 => self.allocator.alignedAlloc(u8, .@"2", roc_realloc.new_length),
            4 => self.allocator.alignedAlloc(u8, .@"4", roc_realloc.new_length),
            8 => self.allocator.alignedAlloc(u8, .@"8", roc_realloc.new_length),
            16 => self.allocator.alignedAlloc(u8, .@"16", roc_realloc.new_length),
            else => @panic("RocEnv: Unsupported alignment"),
        } catch {
            @panic("RocEnv: Reallocation failed");
        };

        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        const old_info = self.allocations.fetchRemove(@intFromPtr(old_ptr));
        if (old_info) |info| {
            const copy_len = @min(info.value.len, roc_realloc.new_length);
            @memcpy(new_ptr[0..copy_len], old_ptr[0..copy_len]);
            freeTrackedAllocation(self.allocator, old_ptr, info.value);
        }

        // Return the new pointer and track its size
        roc_realloc.answer = @ptrCast(new_ptr.ptr);
        self.allocations.put(self.allocator, @intFromPtr(new_ptr.ptr), .{
            .len = roc_realloc.new_length,
            .alignment = roc_realloc.alignment,
        }) catch {};
    }

    /// Debug output function.
    pub fn rocDbgFn(roc_dbg: *const RocDbg, _: *anyopaque) callconv(.c) void {
        if (comptime !is_freestanding) {
            const msg = roc_dbg.utf8_bytes[0..roc_dbg.len];
            debugPrint("[dbg] {s}\n", .{msg});
        }
    }

    /// Expect failed function.
    pub fn rocExpectFailedFn(_: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
        if (comptime !is_freestanding) {
            debugPrint("[expect failed]\n", .{});
        }
    }

    /// Crash function.
    pub fn rocCrashedFn(roc_crashed: *const RocCrashed, _: *anyopaque) callconv(.c) noreturn {
        if (comptime is_freestanding) {
            @panic("Roc crashed");
        } else {
            const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
            debugPrint("Roc crashed: {s}\n", .{msg});
            unreachable;
        }
    }
};

fn freeTrackedAllocation(allocator: Allocator, ptr: anytype, alloc_info: RocEnv.AllocInfo) void {
    const bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    switch (alloc_info.alignment) {
        1 => allocator.free(bytes[0..alloc_info.len]),
        2 => allocator.free((@as([*]align(2) u8, @alignCast(bytes)))[0..alloc_info.len]),
        4 => allocator.free((@as([*]align(4) u8, @alignCast(bytes)))[0..alloc_info.len]),
        8 => allocator.free((@as([*]align(8) u8, @alignCast(bytes)))[0..alloc_info.len]),
        16 => allocator.free((@as([*]align(16) u8, @alignCast(bytes)))[0..alloc_info.len]),
        else => @panic("RocEnv: Unsupported alignment"),
    }
}

/// Create a RocOps struct from a RocEnv pointer.
/// Uses a static dummy array for hosted_fns since count=0 means no hosted functions.
pub fn createRocOps(roc_env: *RocEnv) RocOps {
    const empty_hosted_fns = struct {
        fn dummyHostedFn(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
        var empty: [1]builtins.host_abi.HostedFn = .{&dummyHostedFn};
    };
    return RocOps{
        .env = @ptrCast(roc_env),
        .roc_alloc = &RocEnv.rocAllocFn,
        .roc_dealloc = &RocEnv.rocDeallocFn,
        .roc_realloc = &RocEnv.rocReallocFn,
        .roc_dbg = &RocEnv.rocDbgFn,
        .roc_expect_failed = &RocEnv.rocExpectFailedFn,
        .roc_crashed = &RocEnv.rocCrashedFn,
        .hosted_fns = .{ .count = 0, .fns = &empty_hosted_fns.empty },
    };
}
