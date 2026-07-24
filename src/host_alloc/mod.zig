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

/// Bytes reserved before the user data for the stored total size. The prefix
/// is at least `alignment` bytes so the user data keeps its alignment, and at
/// least `@alignOf(usize)` bytes so the size store/load stays aligned.
pub fn sizeStorageBytes(alignment: usize) usize {
    return @max(alignment, @alignOf(usize));
}

/// The alignment the backing allocation is made with: the requested alignment,
/// raised to `@alignOf(usize)` so the size prefix is aligned.
pub fn backingAlignment(alignment: usize) std.mem.Alignment {
    return std.mem.Alignment.fromByteUnits(@max(alignment, @alignOf(usize)));
}

/// Total size (prefix included) stored for the live allocation at `ptr`.
pub fn storedTotalSize(ptr: *const anyopaque) usize {
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    return size_ptr.*;
}

/// The backing allocation's base pointer for the user allocation at `ptr`.
pub fn basePtr(ptr: *anyopaque, alignment: usize) [*]u8 {
    return @ptrFromInt(@intFromPtr(ptr) - sizeStorageBytes(alignment));
}

/// Allocate `length` bytes aligned to `alignment` with the size prefix filled
/// in, returning null on OOM.
pub fn alloc(backing: std.mem.Allocator, length: usize, alignment: usize) ?*anyopaque {
    const size_storage_bytes = sizeStorageBytes(alignment);
    const total_size = length + size_storage_bytes;

    const base_ptr = backing.rawAlloc(total_size, backingAlignment(alignment), @returnAddress()) orelse
        return null;

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    const answer: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
    std.debug.assert(@intFromPtr(answer) % @max(alignment, 1) == 0);
    if (trace_refcount) {
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ @intFromPtr(answer), length, alignment });
    }
    return answer;
}

/// Free the allocation at `ptr` using its stored total size. (On an arena
/// backing this is effectively a no-op, which is exactly what arena-based
/// hosts want.)
pub fn dealloc(backing: std.mem.Allocator, ptr: *anyopaque, alignment: usize) void {
    const total_size = storedTotalSize(ptr);
    if (trace_refcount) {
        std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{
            @intFromPtr(ptr),
            alignment,
            total_size,
            sizeStorageBytes(alignment),
        });
    }
    const base_ptr = basePtr(ptr, alignment);
    backing.rawFree(base_ptr[0..total_size], backingAlignment(alignment), @returnAddress());
}

/// Reallocate the allocation at `ptr` to `new_length` bytes, returning null on
/// OOM (in which case the old allocation stays live).
pub fn realloc(backing: std.mem.Allocator, ptr: *anyopaque, new_length: usize, alignment: usize) ?*anyopaque {
    const size_storage_bytes = sizeStorageBytes(alignment);
    const old_total_size = storedTotalSize(ptr);
    const old_base_ptr = basePtr(ptr, alignment);
    const new_total_size = new_length + size_storage_bytes;

    const new_base_ptr = backing.rawAlloc(new_total_size, backingAlignment(alignment), @returnAddress()) orelse
        return null;

    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_base_ptr[0..copy_size], old_base_ptr[0..copy_size]);

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    backing.rawFree(old_base_ptr[0..old_total_size], backingAlignment(alignment), @returnAddress());

    const answer: *anyopaque = @ptrFromInt(@intFromPtr(new_base_ptr) + size_storage_bytes);
    if (trace_refcount) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(ptr), @intFromPtr(answer), new_length });
    }
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
/// `host_abi.extern_host` so a signature that drifts from the canonical ABI is
/// a compile error.
pub const RuntimeFns = struct {
    alloc: *const @TypeOf(builtins.host_abi.extern_host.roc_alloc),
    dealloc: *const @TypeOf(builtins.host_abi.extern_host.roc_dealloc),
    realloc: *const @TypeOf(builtins.host_abi.extern_host.roc_realloc),
    dbg: *const @TypeOf(builtins.host_abi.extern_host.roc_dbg),
    expect_failed: *const @TypeOf(builtins.host_abi.extern_host.roc_expect_failed),
    crashed: *const @TypeOf(builtins.host_abi.extern_host.roc_crashed),
};

/// Export `fns` under the fixed runtime symbol names. Call from a `comptime`
/// block. Hosts that build a `RocOps` use `exportRuntimeSymbols` instead.
pub fn exportRuntimeFns(comptime fns: RuntimeFns) void {
    @export(fns.alloc, .{ .name = shim_symbols.roc_alloc, .visibility = .hidden });
    @export(fns.dealloc, .{ .name = shim_symbols.roc_dealloc, .visibility = .hidden });
    @export(fns.realloc, .{ .name = shim_symbols.roc_realloc, .visibility = .hidden });
    @export(fns.dbg, .{ .name = shim_symbols.roc_dbg, .visibility = .hidden });
    @export(fns.expect_failed, .{ .name = shim_symbols.roc_expect_failed, .visibility = .hidden });
    @export(fns.crashed, .{ .name = shim_symbols.roc_crashed, .visibility = .hidden });
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
