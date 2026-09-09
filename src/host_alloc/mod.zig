//! Size-tracking host allocator shared by the platform hosts (the test
//! platforms and the glue platform).
//!
//! `roc_dealloc` does not receive the allocation's length (seamless slices make
//! it unknown at runtime), so a host must store each allocation's total size
//! itself. This module implements the one scheme every host uses: over-allocate
//! by a prefix of `@max(alignment, @alignOf(usize))` bytes, store the total
//! size in the `usize` directly before the user data, and read it back in
//! dealloc/realloc.
//!
//! Hosts with plain behavior build their `RocOps` from `Callbacks`; hosts with
//! extra bookkeeping (allocation counting, live-block tracking) wrap the core
//! `alloc`/`dealloc`/`realloc` functions instead of restating the prefix math.
//! `exportRuntimeSymbols` exports the fixed runtime symbol set over a `RocOps`
//! so the symbol names are only ever spelled by `shim_symbols`.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const build_options = @import("build_options");

const RocOps = builtins.host_abi.RocOps;
const shim_symbols = builtins.shim_symbols;

// Freestanding hosts (the wasm test host) have no stderr to trace to.
const trace_refcount = build_options.trace_refcount and builtin.os.tag != .freestanding;

const tracking = @import("tracking.zig");
pub const sizeStorageBytes = tracking.sizeStorageBytes;
pub const backingAlignment = tracking.backingAlignment;
pub const storedTotalSize = tracking.storedTotalSize;
pub const basePtr = tracking.basePtr;

/// Allocate through the shared core and emit the configured host allocation trace.
pub fn alloc(backing: std.mem.Allocator, length: usize, alignment: usize) ?*anyopaque {
    const ptr = tracking.alloc(backing, length, alignment) orelse return null;
    if (trace_refcount) std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ @intFromPtr(ptr), length, alignment });
    return ptr;
}

/// Release a tracked allocation, tracing its size before the core frees it.
pub fn dealloc(backing: std.mem.Allocator, ptr: *anyopaque, alignment: usize) void {
    if (trace_refcount) std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{ @intFromPtr(ptr), alignment, storedTotalSize(ptr), sizeStorageBytes(alignment) });
    tracking.dealloc(backing, ptr, alignment);
}

/// Resize through the shared core, preserving the old allocation on failure.
pub fn realloc(backing: std.mem.Allocator, ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque {
    const answer = tracking.realloc(backing, ptr, new_length, alignment) orelse return null;
    if (trace_refcount) std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(ptr), @intFromPtr(answer), new_length });
    return answer;
}

/// Report an out-of-memory failure from a Roc host allocation callback and
/// exit. These callbacks use the C ABI and cannot return a Zig error, and a
/// platform host must not return a real pointer it could not allocate.
pub fn allocFailed() noreturn {
    std.debug.print("\x1b[31mHost error:\x1b[0m out of memory\n", .{});
    std.process.exit(1);
}

/// `RocOps` callbacks over `Env`, the struct the host's `RocOps.env` points
/// at. `Env` must define `rocAllocator(self: *Env) std.mem.Allocator`
/// returning the backing allocator.
pub fn Callbacks(comptime Env: type) type {
    return struct {
        pub fn rocAllocFn(ops: *RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
            const host: *Env = @ptrCast(@alignCast(ops.env));
            return alloc(host.rocAllocator(), length, alignment) orelse allocFailed();
        }

        pub fn rocDeallocFn(ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
            const host: *Env = @ptrCast(@alignCast(ops.env));
            dealloc(host.rocAllocator(), ptr, alignment);
        }

        pub fn rocReallocFn(ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
            const host: *Env = @ptrCast(@alignCast(ops.env));
            return realloc(host.rocAllocator(), ptr, new_length, alignment) orelse allocFailed();
        }

        pub fn rocDbgFn(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
            std.debug.print("ROC DBG: {s}\n", .{bytes[0..len]});
        }

        pub fn rocExpectFailedFn(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
            const trimmed = std.mem.trim(u8, bytes[0..len], " \t\n\r");
            std.debug.print("Expect failed: {s}\n", .{trimmed});
        }

        pub fn rocCrashedFn(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
            std.debug.print("\n\x1b[31mRoc crashed:\x1b[0m {s}\n", .{bytes[0..len]});
            std.process.exit(1);
        }
    };
}

/// Hooks for `exportRuntimeSymbols`.
pub const ExportOptions = struct {
    /// Runs before each exported `roc_alloc`/`roc_realloc` call (allocation
    /// counting). Host-internal builtins-helper calls go through the vtable
    /// directly and deliberately bypass this hook.
    on_alloc: ?fn () void = null,
};

/// The six runtime-symbol implementations, typed directly from
/// `host_abi.ExternHostFns` so a signature that drifts from the canonical ABI is
/// a compile error.
pub const RuntimeFns = struct {
    alloc: builtins.host_abi.ExternHostFns.roc_alloc,
    dealloc: builtins.host_abi.ExternHostFns.roc_dealloc,
    realloc: builtins.host_abi.ExternHostFns.roc_realloc,
    dbg: builtins.host_abi.ExternHostFns.roc_dbg,
    expect_failed: builtins.host_abi.ExternHostFns.roc_expect_failed,
    crashed: builtins.host_abi.ExternHostFns.roc_crashed,
};

/// Export `fns` under the fixed runtime symbol names. Call from a `comptime`
/// block. Hosts that build a `RocOps` use `exportRuntimeSymbols` instead.
pub fn exportRuntimeFns(comptime fns: RuntimeFns) void {
    shim_symbols.exportRuntimeFns(fns, .hidden);
}

/// Export the fixed runtime symbols (`roc_alloc` and friends) the symbol ABI
/// requires, delegating to the `RocOps` that `getOps` returns. Call from a
/// `comptime` block. (`getOps` is a function rather than a pointer so hosts
/// that only build their `RocOps` at runtime can participate.)
pub fn exportRuntimeSymbols(comptime getOps: fn () *RocOps, comptime options: ExportOptions) void {
    const wrappers = struct {
        fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
            if (options.on_alloc) |on_alloc| on_alloc();
            const ops = getOps();
            return ops.roc_alloc(ops, length, alignment);
        }

        fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
            const ops = getOps();
            ops.roc_dealloc(ops, ptr, alignment);
        }

        fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
            if (options.on_alloc) |on_alloc| on_alloc();
            const ops = getOps();
            return ops.roc_realloc(ops, ptr, new_length, alignment);
        }

        fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
            const ops = getOps();
            ops.roc_dbg(ops, bytes, len);
        }

        fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
            const ops = getOps();
            ops.roc_expect_failed(ops, bytes, len);
        }

        fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
            const ops = getOps();
            ops.roc_crashed(ops, bytes, len);
        }
    };

    exportRuntimeFns(.{
        .alloc = &wrappers.hostAlloc,
        .dealloc = &wrappers.hostDealloc,
        .realloc = &wrappers.hostRealloc,
        .dbg = &wrappers.hostDbg,
        .expect_failed = &wrappers.hostExpectFailed,
        .crashed = &wrappers.hostCrashed,
    });
}

test "alloc stores a size dealloc and realloc can read back" {
    const backing = std.testing.allocator;

    const first = alloc(backing, 24, 16).?;
    try std.testing.expectEqual(@as(usize, 24 + 16), storedTotalSize(first));
    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(first) % 16);

    const bytes: [*]u8 = @ptrCast(first);
    for (0..24) |i| bytes[i] = @intCast(i);

    const grown = realloc(backing, first, 100, 16).?;
    try std.testing.expectEqual(@as(usize, 100 + 16), storedTotalSize(grown));
    const grown_bytes: [*]u8 = @ptrCast(grown);
    for (0..24) |i| try std.testing.expectEqual(@as(u8, @intCast(i)), grown_bytes[i]);

    dealloc(backing, grown, 16);
}

test "small alignments still keep the size prefix aligned" {
    const backing = std.testing.allocator;

    const ptr = alloc(backing, 3, 1).?;
    try std.testing.expectEqual(@as(usize, 3 + @alignOf(usize)), storedTotalSize(ptr));
    dealloc(backing, ptr, 1);
}

const AccountingTestHost = struct {
    count: usize = 0,
    var env: @This() = .{};
    var ops: RocOps = undefined;

    pub fn rocAllocator(_: *@This()) std.mem.Allocator {
        return std.testing.allocator;
    }
    fn getOps() *RocOps {
        return &ops;
    }
    fn countAlloc() void {
        env.count += 1;
    }
};

comptime {
    if (builtin.is_test) exportRuntimeSymbols(AccountingTestHost.getOps, .{ .on_alloc = AccountingTestHost.countAlloc });
}

test "alloc-count counts exported alloc and realloc but not private callbacks or free" {
    const Host = AccountingTestHost;
    const callbacks = Callbacks(Host);
    Host.env = .{};
    Host.ops = .{
        .env = @ptrCast(&Host.env),
        .roc_alloc = callbacks.rocAllocFn,
        .roc_dealloc = callbacks.rocDeallocFn,
        .roc_realloc = callbacks.rocReallocFn,
        .roc_dbg = callbacks.rocDbgFn,
        .roc_expect_failed = callbacks.rocExpectFailedFn,
        .roc_crashed = callbacks.rocCrashedFn,
        .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
    };
    const exported = builtins.host_abi.extern_host;
    const private = Host.ops.roc_alloc(&Host.ops, 20, 8).?;
    Host.ops.roc_dealloc(&Host.ops, private, 8);
    try std.testing.expectEqual(@as(usize, 0), Host.env.count);
    const first = exported.roc_alloc(20, 8).?;
    try std.testing.expectEqual(@as(usize, 1), Host.env.count);
    const grown = exported.roc_realloc(first, 40, 8).?;
    try std.testing.expectEqual(@as(usize, 2), Host.env.count);
    const shrunk = exported.roc_realloc(grown, 5, 8).?;
    try std.testing.expectEqual(@as(usize, 3), Host.env.count);
    exported.roc_dealloc(shrunk, 8);
    try std.testing.expectEqual(@as(usize, 3), Host.env.count);
}

test "realloc failure preserves the live allocation and its bytes" {
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 1 });
    const backing = failing.allocator();
    const ptr = alloc(backing, 12, 8).?;
    defer dealloc(backing, ptr, 8);
    const bytes: [*]u8 = @ptrCast(ptr);
    @memset(bytes[0..12], 0x7b);
    try std.testing.expectEqual(@as(?*anyopaque, null), realloc(backing, ptr, 100, 8));
    try std.testing.expectEqual(@as(usize, 12 + sizeStorageBytes(8)), storedTotalSize(ptr));
    for (bytes[0..12]) |byte| try std.testing.expectEqual(@as(u8, 0x7b), byte);
}
