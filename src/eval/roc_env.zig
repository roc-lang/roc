//! Shared RocEnv for managing RocOps callbacks.
//!
//! Provides an arena-based allocator environment that works for any backend.
//! Manages RC tracking for in-place mutation optimization while arenas
//! handle actual memory deallocation.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");

const Allocator = std.mem.Allocator;

// Host ABI types for RocOps
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

/// Environment for RocOps in evaluators.
/// Manages arena-backed allocation where free() is a no-op.
/// This enables proper RC tracking for in-place mutation optimization
/// while arenas handle actual memory deallocation.
pub const RocEnv = struct {
    allocator: Allocator,
    /// Track allocation sizes so realloc can copy the correct number of bytes.
    alloc_sizes: std.AutoHashMapUnmanaged(usize, usize) = .{},

    pub fn init(allocator: Allocator) RocEnv {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *RocEnv) void {
        self.alloc_sizes.deinit(self.allocator);
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
        self.alloc_sizes.put(self.allocator, @intFromPtr(ptr.ptr), roc_alloc.length) catch {};
    }

    /// Deallocation function for RocOps.
    /// This is a NO-OP because arenas manage actual memory deallocation.
    /// RC operations still work to track uniqueness for in-place mutation.
    pub fn rocDeallocFn(_: *RocDealloc, _: *anyopaque) callconv(.c) void {
        // Intentional no-op: arena manages actual deallocation
        // This allows RC to track uniqueness while arena handles cleanup
    }

    /// Reallocation function for RocOps.
    /// For arena-based allocation, we just allocate new memory.
    /// Since free is a no-op, we don't need to worry about freeing the old allocation.
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

        // Copy old data from the existing allocation (only copy the old size, not new size)
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        const old_size = self.alloc_sizes.get(@intFromPtr(old_ptr)) orelse roc_realloc.new_length;
        const copy_len = @min(old_size, roc_realloc.new_length);
        @memcpy(new_ptr[0..copy_len], old_ptr[0..copy_len]);

        // Return the new pointer and track its size
        roc_realloc.answer = @ptrCast(new_ptr.ptr);
        self.alloc_sizes.put(self.allocator, @intFromPtr(new_ptr.ptr), roc_realloc.new_length) catch {};
    }

    /// Debug output function.
    pub fn rocDbgFn(roc_dbg: *const RocDbg, _: *anyopaque) callconv(.c) void {
        // On freestanding (WASM), skip debug output to avoid thread locking
        if (builtin.os.tag != .freestanding) {
            const msg = roc_dbg.utf8_bytes[0..roc_dbg.len];
            std.debug.print("[dbg] {s}\n", .{msg});
        }
    }

    /// Expect failed function.
    pub fn rocExpectFailedFn(_: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {
        // On freestanding (WASM), skip debug output to avoid thread locking
        if (builtin.os.tag != .freestanding) {
            std.debug.print("[expect failed]\n", .{});
        }
    }

    /// Crash function.
    pub fn rocCrashedFn(roc_crashed: *const RocCrashed, _: *anyopaque) callconv(.c) noreturn {
        // On freestanding (WASM), just panic without debug output to avoid thread locking
        if (builtin.os.tag == .freestanding) {
            @panic("Roc crashed");
        } else {
            const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
            std.debug.print("Roc crashed: {s}\n", .{msg});
            unreachable;
        }
    }
};

/// Create a RocOps struct from a RocEnv pointer.
/// Uses a static dummy array for hosted_fns since count=0 means no hosted functions.
pub fn createRocOps(roc_env: *RocEnv) RocOps {
    const empty_hosted_fns = struct {
        fn dummyHostedFn(_: *RocOps, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
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
