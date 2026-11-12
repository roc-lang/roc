//! Deterministic counters for the compiler's table storage: bytes filled by
//! initialization, bytes copied on growth, and the page primitive's mapping
//! calls. The collection types and the page primitive are the only code that
//! increments them.
//!
//! The counters exist only in compilers built with developer diagnostics
//! (every Debug compiler, and any other build made with `-Ddev-diagnostics`).
//! In every other compiler `enabled` is false, every `add` compiles to nothing,
//! and the counters occupy no storage. Whether a run prints them is decided by
//! the `storage_work` developer-diagnostic variable, which only
//! `base.dev_diagnostics` reads.

const std = @import("std");
const build_options = @import("build_options");

/// Whether this compiler carries the counters at all.
pub const enabled = build_options.dev_diagnostics;

/// One storage counter.
pub const Counter = enum {
    /// Bytes written by initialization fills inside the collection types.
    bytes_filled,
    /// Bytes copied by growth inside the collection types.
    bytes_copied,
    /// Growth steps that asked an allocator for a larger block.
    growth_allocations,
    /// Fresh zero-page mappings created by the page primitive.
    mappings,
    /// Bytes of fresh zero-page mappings created by the page primitive.
    mapped_bytes,
    /// Guaranteed-zero resets of a mapping.
    zero_resets,
    /// Virtual reservations made for growing columns.
    reservations,
    /// Cumulative successfully reserved virtual bytes, including alignment
    /// tails. This is work, not current or peak virtual memory usage.
    reserved_bytes,
    /// Commit calls made ahead of a growing column's length.
    commit_calls,
    /// Pages returned to the kernel by trimming a reservation's tail.
    pages_trimmed,
    /// Columns moved because they grew past their reservation.
    relocations,
    /// Columns moved from ordinary allocation onto a reservation.
    reservation_crossings,
    /// Epoch counters that reached their limit and cleared their stamps.
    epoch_wraps,
    /// Epoch resets (one increment each).
    epoch_resets,
    /// Bytes allocated from scratch arenas.
    scratch_bytes,
    /// Scratch arena resets.
    scratch_resets,
};

const counter_count = @typeInfo(Counter).@"enum".field_names.len;

// Baseline 32-bit targets need a lock for a coherent 64-bit counter.
// Diagnostics must not require CPU features the compiler host does not promise.
const Value = if (@sizeOf(usize) >= 8) struct {
    value: std.atomic.Value(u64) = .init(0),

    fn add(self: *@This(), amount: usize) void {
        _ = self.value.fetchAdd(amount, .monotonic);
    }

    fn get(self: *@This()) u64 {
        return self.value.load(.monotonic);
    }
} else struct {
    mutex: std.atomic.Mutex = .unlocked,
    value: u64 = 0,

    fn lock(self: *@This()) void {
        if (@import("builtin").single_threaded) return;
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
    }

    fn unlock(self: *@This()) void {
        if (@import("builtin").single_threaded) return;
        self.mutex.unlock();
    }

    fn add(self: *@This(), amount: usize) void {
        self.lock();
        defer self.unlock();
        self.value +%= amount;
    }

    fn get(self: *@This()) u64 {
        self.lock();
        defer self.unlock();
        return self.value;
    }
};

var counts: if (enabled) [counter_count]Value else void =
    if (enabled) @splat(.{}) else {};

/// Add `amount` to a counter. Compiles to nothing without developer
/// diagnostics.
pub inline fn add(comptime counter: Counter, amount: usize) void {
    if (comptime !enabled) return;
    counts[@backingInt(counter)].add(amount);
}

/// A counter's current value; zero without developer diagnostics.
pub fn get(counter: Counter) u64 {
    if (comptime !enabled) return 0;
    return counts[@backingInt(counter)].get();
}

/// Print every counter as `storage-work <name>=<value>` lines.
pub fn print(writer: *std.Io.Writer) std.Io.Writer.Error!void {
    inline for (@typeInfo(Counter).@"enum".field_names) |name| {
        try writer.print("storage-work {s}={d}\n", .{ name, get(@field(Counter, name)) });
    }
}

test "counters count only in builds with developer diagnostics" {
    const before = get(.bytes_filled);
    add(.bytes_filled, 3);
    const expected: u64 = if (enabled) before + 3 else 0;
    try std.testing.expectEqual(expected, get(.bytes_filled));
}

test "storage counters preserve totals wider than a host word" {
    if (!enabled) return error.SkipZigTest;
    const before = get(.scratch_bytes);
    add(.scratch_bytes, std.math.maxInt(u32));
    add(.scratch_bytes, 7);
    try std.testing.expectEqual(before + @as(u64, std.math.maxInt(u32)) + 7, get(.scratch_bytes));
}
