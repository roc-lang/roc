//! Experimental allocators for one short-lived Roc root. Copy its result into
//! checked storage before reset/deinit; no allocation escapes that boundary.
//! Instances are single-threaded, but may share a thread-safe backing allocator.
//!
//! Choose one policy and stats mode for the entire interval between resets.
//! Hosts can specialize their RocOps callbacks once without a runtime policy
//! switch or a union containing several copies of the allocator state.

const std = @import("std");
const base = @import("base");

pub const Policy = enum { arena, recycle, hybrid, segregated };

/// Successful public calls only. Realloc counts once, contributes its requested
/// new length to alloc_bytes, and replaces the old length in live_bytes. Zero
/// length requests own a distinct usable byte but contribute zero logical bytes.
/// Cumulative counters saturate. Reset clears all counters, including peaks.
pub const Stats = struct {
    alloc_count: usize = 0,
    alloc_bytes: usize = 0,
    realloc_count: usize = 0,
    free_count: usize = 0,
    live_bytes: usize = 0,
    peak_live_bytes: usize = 0,
    /// Arena usable chunk capacity plus complete individually backed blocks.
    /// Includes free lists, slab slack, retained large blocks and live storage;
    /// excludes arena chunk headers and backing allocator metadata/rounding.
    reserved_bytes: usize = 0,
    peak_reserved_bytes: usize = 0,

    /// Record a successful allocation (also usable by baseline host callbacks).
    pub fn onAlloc(self: *Stats, length: usize) void {
        self.alloc_count +|= 1;
        self.alloc_bytes +|= length;
        self.replaceLive(0, length);
    }

    /// Record a logical free, whether or not physical storage is retained.
    pub fn onFree(self: *Stats, length: usize) void {
        self.free_count +|= 1;
        self.replaceLive(length, 0);
    }

    /// Record one successful realloc, without counting its internal alloc/free.
    pub fn onRealloc(self: *Stats, old_length: usize, new_length: usize) void {
        self.realloc_count +|= 1;
        self.alloc_bytes +|= new_length;
        self.replaceLive(old_length, new_length);
    }

    fn replaceLive(self: *Stats, old: usize, new: usize) void {
        if (old > self.live_bytes) base.invariant("root allocation metrics must free only live bytes", .{});
        self.live_bytes = self.live_bytes - old + new;
        self.peak_live_bytes = @max(self.peak_live_bytes, self.live_bytes);
    }
};

const header_bytes = 32;
const max_alignment = 16;
const max_skew = if (std.debug.runtime_safety) max_alignment else 0;
const overhead = header_bytes + max_skew;
const live_stamp: u64 = 0x726f_6f74_616c_6c6f;
const small_limit = 32 * 1024;
const power_class_count = 10; // 64 .. 32768, including the header and skew.
const segregated_class_count = 35; // 64, 96, 128, then four bins per doubling.
const slab_bytes = 64 * 1024;

const Header = extern struct {
    length: usize,
    /// Physical payload capacity, independent of the current logical length.
    /// Shrinking a large allocation must not change its ownership or free size.
    capacity: usize,
    stamp: u64,
    alignment: usize,
};

const FreeBlock = extern struct {
    next: ?*FreeBlock,
};

/// Large blocks are explicitly owned, including after logical free under
/// hybrid/segregated. Their links never overlap the stamp or logical length.
const Large = extern struct {
    prev: ?*Large,
    next: ?*Large,
    total: usize,
};

const Slab = struct {
    next: ?[*]align(header_bytes) u8 = null,
    remaining: usize = 0,
};

comptime {
    std.debug.assert(@sizeOf(Header) <= header_bytes);
    std.debug.assert(@sizeOf(Large) <= header_bytes);
    std.debug.assert(@sizeOf(FreeBlock) <= @offsetOf(Header, "stamp"));
}

pub const RootAllocator = struct {
    arena: base.SingleThreadArena,
    free_lists: [segregated_class_count]?*FreeBlock = @splat(null),
    slabs: [segregated_class_count]Slab = @splat(.{}),
    large_blocks: ?*Large = null,
    /// Maintained only in instrumented specializations.
    large_reserved: usize = 0,
    counters: Stats = .{},

    pub fn init(backing: std.mem.Allocator) RootAllocator {
        return .{ .arena = base.SingleThreadArena.init(backing) };
    }

    pub fn deinit(self: *RootAllocator) void {
        self.reset();
        self.* = undefined;
    }

    /// Releases every backing allocation, including still-live Roc allocations.
    /// The same backing allocator remains available for the next root.
    pub fn reset(self: *RootAllocator) void {
        while (self.large_blocks) |block| self.releaseLarge(block);
        _ = self.arena.reset(.free_all);
        self.free_lists = @splat(null);
        self.slabs = @splat(.{});
        self.large_reserved = 0;
        self.counters = .{};
    }

    /// Returns zeros when record_stats is false. Instrumented allocation may
    /// walk arena chunks to sample capacity; timing runs should disable stats.
    pub fn stats(self: *const RootAllocator) Stats {
        return self.counters;
    }

    pub fn alloc(self: *RootAllocator, comptime policy: Policy, comptime record_stats: bool, length: usize, alignment: usize) error{OutOfMemory}!*anyopaque {
        const ptr = try self.allocate(policy, record_stats, length, alignment);
        if (record_stats) self.counters.onAlloc(length);
        return ptr;
    }

    /// Failure preserves the original allocation and all logical counters.
    /// Any request fitting the original physical capacity stays in place.
    pub fn realloc(self: *RootAllocator, comptime policy: Policy, comptime record_stats: bool, ptr: *anyopaque, new_length: usize, alignment: usize) error{OutOfMemory}!*anyopaque {
        checkAlignment(alignment);
        const old = getHeader(ptr);
        checkLive(old, alignment);
        const old_length = old.length;
        const result = if (@max(new_length, 1) <= old.capacity) in_place: {
            old.length = new_length;
            break :in_place ptr;
        } else moved: {
            const fresh = try self.allocate(policy, record_stats, new_length, alignment);
            const copied = @min(old_length, new_length);
            @memcpy(bytes(fresh)[0..copied], bytes(ptr)[0..copied]);
            self.release(policy, record_stats, ptr, alignment);
            break :moved fresh;
        };
        if (record_stats) self.counters.onRealloc(old_length, new_length);
        return result;
    }

    /// In an uninstrumented, unsafe arena specialization this has an empty
    /// body: no header read, stamp write, freelist work, or backing free.
    pub fn free(self: *RootAllocator, comptime policy: Policy, comptime record_stats: bool, ptr: *anyopaque, alignment: usize) void {
        if (policy == .arena and !record_stats and !std.debug.runtime_safety) return;
        checkAlignment(alignment);
        const header = getHeader(ptr);
        checkLive(header, alignment);
        if (record_stats) self.counters.onFree(header.length);
        self.release(policy, record_stats, ptr, alignment);
    }

    fn allocate(self: *RootAllocator, comptime policy: Policy, comptime record_stats: bool, length: usize, alignment: usize) error{OutOfMemory}!*anyopaque {
        checkAlignment(alignment);
        const needed = std.math.add(usize, @max(length, 1), overhead) catch return error.OutOfMemory;
        const header: *Header, const capacity: usize = if (policy == .arena) block: {
            const memory = try self.arenaBytes(needed);
            break :block .{ @ptrCast(memory.ptr), needed - overhead };
        } else if (needed <= small_limit) block: {
            const class = classFor(policy, needed);
            const size = classSize(policy, class);
            const memory: *Header = if (self.free_lists[class]) |available| reused: {
                self.free_lists[class] = available.next;
                break :reused @ptrCast(available);
            } else if (policy == .segregated) fresh: {
                const slab = &self.slabs[class];
                if (slab.remaining == 0) {
                    const count = slab_bytes / size;
                    const memory = try self.arenaBytes(count * size);
                    slab.* = .{ .next = memory.ptr, .remaining = count };
                }
                const next = slab.next.?;
                slab.remaining -= 1;
                slab.next = @alignCast(next + size);
                break :fresh @ptrCast(next);
            } else @ptrCast((try self.arenaBytes(size)).ptr);
            break :block .{ memory, size - overhead };
        } else block: {
            const total = std.math.add(usize, needed, header_bytes) catch return error.OutOfMemory;
            const memory = try self.arena.child_allocator.alignedAlloc(u8, .fromByteUnits(header_bytes), total);
            const links: *Large = @ptrCast(memory.ptr);
            links.* = .{ .prev = null, .next = self.large_blocks, .total = total };
            if (self.large_blocks) |next| next.prev = links;
            self.large_blocks = links;
            if (record_stats) self.large_reserved += total;
            break :block .{ @ptrCast(@alignCast(memory.ptr + header_bytes)), needed - overhead };
        };
        header.length = length;
        header.capacity = capacity;
        if (std.debug.runtime_safety) {
            header.alignment = alignment;
            header.stamp = live_stamp;
        }
        if (record_stats) self.sampleReserved();
        const raw: [*]u8 = @ptrCast(header);
        return @ptrCast(raw + header_bytes + if (std.debug.runtime_safety) alignment else @as(usize, 0));
    }

    /// SingleThreadArena's growth arithmetic is unchecked. Validate the exact
    /// prospective chunk sizing before entering it, so impossible user lengths
    /// propagate OOM instead of overflowing inside the arena implementation.
    fn arenaBytes(self: *RootAllocator, size: usize) error{OutOfMemory}![]align(header_bytes) u8 {
        const node = self.arena.state.first;
        const node_bytes = @sizeOf(@TypeOf(self.arena.state.first.?.*));
        const previous = if (node) |n| n.capacity - node_bytes else 0;
        const minimum = std.math.add(usize, size, header_bytes + node_bytes + 16) catch return error.OutOfMemory;
        const sum = std.math.add(usize, previous, minimum) catch return error.OutOfMemory;
        _ = std.math.add(usize, sum, sum / 2) catch return error.OutOfMemory;
        return self.arena.allocator().alignedAlloc(u8, .fromByteUnits(header_bytes), size);
    }

    fn release(self: *RootAllocator, comptime policy: Policy, comptime record_stats: bool, ptr: *anyopaque, alignment: usize) void {
        if (policy == .arena and !std.debug.runtime_safety) return;
        const header = getHeader(ptr);
        checkLive(header, alignment);
        if (std.debug.runtime_safety) header.stamp = 0;
        if (policy == .arena) return;
        const size = header.capacity + overhead;
        if (size <= small_limit) {
            const class = classFor(policy, size);
            const block: *FreeBlock = @ptrCast(header);
            block.next = self.free_lists[class];
            self.free_lists[class] = block;
        } else if (policy == .recycle) {
            const raw: [*]u8 = @ptrCast(header);
            const links: *Large = @ptrCast(@alignCast(raw - header_bytes));
            if (record_stats) self.large_reserved -= links.total;
            self.releaseLarge(links);
            if (record_stats) self.sampleReserved();
        }
    }

    fn releaseLarge(self: *RootAllocator, block: *Large) void {
        if (block.prev) |prev| prev.next = block.next else self.large_blocks = block.next;
        if (block.next) |next| next.prev = block.prev;
        const raw: [*]align(header_bytes) u8 = @ptrCast(@alignCast(block));
        self.arena.child_allocator.free(raw[0..block.total]);
    }

    fn sampleReserved(self: *RootAllocator) void {
        self.counters.reserved_bytes = self.arena.queryCapacity() + self.large_reserved;
        self.counters.peak_reserved_bytes = @max(self.counters.peak_reserved_bytes, self.counters.reserved_bytes);
    }
};

fn bytes(ptr: *anyopaque) [*]u8 {
    return @ptrCast(ptr);
}

fn getHeader(ptr: *anyopaque) *Header {
    // The header is 32-byte aligned, and the exact-alignment skew is <=16.
    return @ptrFromInt((@intFromPtr(ptr) - header_bytes) & ~@as(usize, header_bytes - 1));
}

fn checkAlignment(alignment: usize) void {
    if (alignment > max_alignment or !std.math.isPowerOfTwo(alignment)) {
        base.invariant("unsupported root allocation alignment: {d}", .{alignment});
    }
}

fn checkLive(header: *const Header, alignment: usize) void {
    if (!std.debug.runtime_safety) return;
    if (header.stamp != live_stamp) base.invariant("root allocation must be live (invalid pointer or double free)", .{});
    if (header.alignment != alignment) base.invariant("root allocation alignment must match: allocated {d}, received {d}", .{ header.alignment, alignment });
}

fn classFor(comptime policy: Policy, needed: usize) usize {
    if (policy != .segregated) return @max(std.math.log2_int_ceil(usize, needed), 6) - 6;
    if (needed <= 64) return 0;
    if (needed <= 96) return 1;
    if (needed <= 128) return 2;
    const log = std.math.log2_int(usize, needed - 1);
    const quantum = @as(usize, 1) << @intCast(log - 2);
    return 2 + (@as(usize, log) - 7) * 4 + (needed + quantum - 1) / quantum - 4;
}

fn classSize(comptime policy: Policy, class: usize) usize {
    if (policy != .segregated) return @as(usize, 1) << @intCast(class + 6);
    if (class <= 2) return 64 + class * 32;
    return (5 + (class - 3) % 4) << @intCast(5 + (class - 3) / 4);
}

test "root allocator alignment, zero lengths and logical metrics for every policy" {
    inline for (comptime std.meta.tags(Policy)) |policy| {
        inline for (.{ false, true }) |record_stats| {
            var root = RootAllocator.init(std.testing.allocator);
            defer root.deinit();
            for ([_]usize{ 0, 1, 17, 80, 200, 1000, small_limit, 2 * small_limit }) |length| {
                for ([_]usize{ 1, 2, 4, 8, 16 }) |alignment| {
                    const ptr = try root.alloc(policy, record_stats, length, alignment);
                    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(ptr) % alignment);
                    if (std.debug.runtime_safety) try std.testing.expectEqual(alignment, @intFromPtr(ptr) % (alignment * 2));
                    @memset(bytes(ptr)[0..@max(length, 1)], 0xa5);
                    root.free(policy, record_stats, ptr, alignment);
                }
            }
            try std.testing.expectEqual(@as(usize, 0), root.stats().live_bytes);
            try std.testing.expectEqual(@as(usize, if (record_stats) 40 else 0), root.stats().alloc_count);
            try std.testing.expectEqual(root.stats().alloc_count, root.stats().free_count);
            if (!record_stats) try std.testing.expectEqualDeep(Stats{}, root.stats());
        }
    }
}

test "root allocator realloc preserves bytes and allocation identity across shrink" {
    inline for (comptime std.meta.tags(Policy)) |policy| {
        inline for (.{ false, true }) |record_stats| {
            var root = RootAllocator.init(std.testing.allocator);
            defer root.deinit();
            for ([_]usize{ 1, 2, 4, 8, 16 }) |alignment| {
                const first = try root.alloc(policy, record_stats, 100, alignment);
                for (bytes(first)[0..100], 0..) |*byte, i| byte.* = @intCast(i);
                const grown = try root.realloc(policy, record_stats, first, 2 * small_limit, alignment);
                for (bytes(grown)[0..100], 0..) |byte, i| try std.testing.expectEqual(@as(u8, @intCast(i)), byte);
                const shrunk = try root.realloc(policy, record_stats, grown, 20, alignment);
                try std.testing.expectEqual(grown, shrunk);
                const regrown = try root.realloc(policy, record_stats, shrunk, small_limit, alignment);
                try std.testing.expectEqual(grown, regrown);
                for (bytes(regrown)[0..20], 0..) |byte, i| try std.testing.expectEqual(@as(u8, @intCast(i)), byte);
                root.free(policy, record_stats, regrown, alignment);
            }
            try std.testing.expectEqual(@as(usize, 0), root.stats().live_bytes);
            if (record_stats) {
                try std.testing.expectEqual(@as(usize, 5), root.stats().alloc_count);
                try std.testing.expectEqual(@as(usize, 15), root.stats().realloc_count);
                try std.testing.expectEqual(@as(usize, 5), root.stats().free_count);
                try std.testing.expectEqual(@as(usize, 2 * small_limit), root.stats().peak_live_bytes);
                try std.testing.expectEqual(@as(usize, 5 * (100 + 2 * small_limit + 20 + small_limit)), root.stats().alloc_bytes);
            }
        }
    }
}

test "root allocator size classes cover every small size and recycle across alignments" {
    inline for (.{ Policy.recycle, Policy.hybrid, Policy.segregated }) |policy| {
        var root = RootAllocator.init(std.testing.allocator);
        defer root.deinit();
        var previous: usize = 0;
        for (overhead + 1..small_limit + 1) |needed| {
            const class = classFor(policy, needed);
            const size = classSize(policy, class);
            try std.testing.expect(class < if (policy == .segregated) segregated_class_count else power_class_count);
            try std.testing.expect(size >= needed and size >= previous);
            try std.testing.expectEqual(@as(usize, 0), size % header_bytes);
            if (class != 0) try std.testing.expect(classSize(policy, class - 1) < needed);
            previous = size;
        }
        const count = if (policy == .segregated) segregated_class_count else power_class_count;
        for (0..count) |class| {
            const length = classSize(policy, class) - overhead;
            const first = try root.alloc(policy, true, length, 1);
            const header = getHeader(first);
            root.free(policy, true, first, 1);
            if (std.debug.runtime_safety) try std.testing.expectEqual(@as(u64, 0), header.stamp);
            const reserved = root.stats().reserved_bytes;
            const next = try root.alloc(policy, true, length, 16);
            try std.testing.expectEqual(header, getHeader(next));
            try std.testing.expectEqual(reserved, root.stats().reserved_bytes);
            @memset(bytes(next)[0..length], 0xcc);
            root.free(policy, true, next, 16);
        }
    }
}

test "root allocator large release, retention and reset" {
    inline for (comptime std.meta.tags(Policy)) |policy| {
        var backing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        var root = RootAllocator.init(backing.allocator());
        defer root.deinit();
        const first = try root.alloc(policy, true, 2 * small_limit, 16);
        const middle = try root.alloc(policy, true, 3 * small_limit, 16);
        const last = try root.alloc(policy, true, 4 * small_limit, 16);
        const reserved = root.stats().reserved_bytes;
        root.free(policy, true, middle, 16);
        try std.testing.expectEqual(@as(usize, if (policy == .recycle) 1 else 0), backing.deallocations);
        if (policy == .recycle) {
            try std.testing.expect(root.stats().reserved_bytes < reserved);
        } else {
            try std.testing.expectEqual(reserved, root.stats().reserved_bytes);
        }
        root.free(policy, true, first, 16);
        root.free(policy, true, last, 16);
        try std.testing.expectEqual(@as(usize, if (policy == .recycle) 3 else 0), backing.deallocations);
        try std.testing.expectEqual(@as(usize, 0), root.stats().live_bytes);
        try std.testing.expectEqual(reserved, root.stats().peak_reserved_bytes);
        if (policy == .recycle) {
            try std.testing.expectEqual(@as(usize, 0), root.stats().reserved_bytes);
            try std.testing.expect(root.large_blocks == null);
        }
        // Reset also owns outstanding allocations, as on a root crash.
        _ = try root.alloc(policy, true, 1, 1);
        _ = try root.alloc(policy, true, 2 * small_limit, 8);
        root.reset();
        try std.testing.expectEqual(backing.allocated_bytes, backing.freed_bytes);
        try std.testing.expectEqualDeep(Stats{}, root.stats());
        try std.testing.expect(root.large_blocks == null);
        try std.testing.expectEqual(@as(usize, 0), root.arena.queryCapacity());
        const again = try root.alloc(policy, true, 17, 4);
        root.free(policy, true, again, 4);
        root.reset();
        root.reset();
    }
}

test "root allocator segregated slab exhaustion preserves outstanding allocations" {
    var root = RootAllocator.init(std.testing.allocator);
    defer root.deinit();
    // More than one slab of one class, interleaved with another class. Free
    // alternating slots, reuse them, and verify the remaining slots survived.
    const count = slab_bytes / 64 + 3;
    var pointers: [count]*anyopaque = undefined;
    for (&pointers, 0..) |*ptr, i| {
        ptr.* = try root.alloc(.segregated, true, 1, 16);
        bytes(ptr.*)[0] = @truncate(i);
        const other = try root.alloc(.segregated, true, 500, 8);
        @memset(bytes(other)[0..500], 0xa5);
        root.free(.segregated, true, other, 8);
    }
    for (pointers, 0..) |ptr, i| {
        try std.testing.expectEqual(@as(u8, @truncate(i)), bytes(ptr)[0]);
        if (i % 2 == 0) root.free(.segregated, true, ptr, 16);
    }
    const reserved = root.stats().reserved_bytes;
    for (&pointers, 0..) |*ptr, i| {
        if (i % 2 == 0) {
            ptr.* = try root.alloc(.segregated, true, 1, 16);
            bytes(ptr.*)[0] = @truncate(i);
        }
    }
    try std.testing.expectEqual(reserved, root.stats().reserved_bytes);
    for (pointers, 0..) |ptr, i| {
        try std.testing.expectEqual(@as(u8, @truncate(i)), bytes(ptr)[0]);
        root.free(.segregated, true, ptr, 16);
    }
    try std.testing.expectEqual(@as(usize, 0), root.stats().live_bytes);
}

test "root allocator zero realloc and baseline stats helpers" {
    inline for (comptime std.meta.tags(Policy)) |policy| {
        var root = RootAllocator.init(std.testing.allocator);
        defer root.deinit();
        var baseline: Stats = .{};
        const first = try root.alloc(policy, true, 0, 8);
        baseline.onAlloc(0);
        const other = try root.alloc(policy, true, 0, 8);
        baseline.onAlloc(0);
        try std.testing.expect(first != other);
        const grown = try root.realloc(policy, true, first, 100, 8);
        baseline.onRealloc(0, 100);
        const shrunk = try root.realloc(policy, true, grown, 0, 8);
        baseline.onRealloc(100, 0);
        root.free(policy, true, shrunk, 8);
        baseline.onFree(0);
        root.free(policy, true, other, 8);
        baseline.onFree(0);
        var logical = root.stats();
        logical.reserved_bytes = 0;
        logical.peak_reserved_bytes = 0;
        try std.testing.expectEqualDeep(baseline, logical);
        try std.testing.expectEqual(@as(usize, 100), logical.peak_live_bytes);
        try std.testing.expectEqual(@as(usize, 0), logical.live_bytes);
    }
}

test "root allocator overflow and backing OOM preserve realloc input and counters" {
    inline for (comptime std.meta.tags(Policy)) |policy| {
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        var root = RootAllocator.init(failing.allocator());
        defer root.deinit();
        const ptr = try root.alloc(policy, true, 100, 8);
        @memset(bytes(ptr)[0..100], 42);
        const before = root.stats();
        try std.testing.expectError(error.OutOfMemory, root.alloc(policy, true, std.math.maxInt(usize), 8));
        try std.testing.expectError(error.OutOfMemory, root.realloc(policy, true, ptr, std.math.maxInt(usize), 8));
        failing.fail_index = failing.alloc_index;
        failing.resize_fail_index = failing.resize_index;
        try std.testing.expectError(error.OutOfMemory, root.realloc(policy, true, ptr, 8 * slab_bytes, 8));
        try std.testing.expectEqualDeep(before, root.stats());
        try std.testing.expectEqual(@as(usize, 100), getHeader(ptr).length);
        for (bytes(ptr)[0..100]) |byte| try std.testing.expectEqual(@as(u8, 42), byte);
        root.free(policy, true, ptr, 8);
    }
}
