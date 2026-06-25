//! A single-threaded arena allocator.
//!
//! Wraps a child allocator and hands out memory by bumping a pointer through a
//! linked list of chunks. Everything is freed together by `deinit` or `reset`;
//! freeing an individual allocation only reclaims it when it was the most recent
//! one (the same LIFO behavior as the standard library arena).
//!
//! This is the single-threaded counterpart to `std.heap.ArenaAllocator`. The
//! standard library arena is lock-free and pays an atomic read-modify-write
//! (plus a reclaim `cmpxchg`) on every allocation so that one arena can be
//! shared across threads. Every arena in this codebase is instead owned by a
//! single thread for its whole lifetime — worker threads each construct their
//! own — so that synchronization is pure overhead here. Dropping it leaves a
//! plain bump in the fast path. The child allocator must still be thread-safe
//! if several arenas share one concurrently, but this wrapper holds no shared
//! state of its own.

const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const SingleThreadArena = @This();

child_allocator: Allocator,
state: State,

/// The inner state of the arena. Can be stored on its own and `promote`d back
/// into a full arena later, saving the size of an `Allocator` when the child
/// allocator is recoverable from elsewhere.
pub const State = struct {
    /// Head of the chunk list; the head is always the chunk currently bumping.
    first: ?*BufNode = null,
    /// Bump offset into the head chunk's usable buffer.
    end_index: usize = 0,

    pub fn promote(state: State, child_allocator: Allocator) SingleThreadArena {
        return .{ .child_allocator = child_allocator, .state = state };
    }
};

/// Header stored at the start of every chunk; the usable buffer follows it.
const BufNode = struct {
    next: ?*BufNode,
    /// Total byte length of the chunk allocation, including this header.
    capacity: usize,
};

const buf_node_align: Alignment = .of(BufNode);

fn fullSlice(node: *BufNode) []u8 {
    return @as([*]u8, @ptrCast(node))[0..node.capacity];
}

fn usableBuf(node: *BufNode) []u8 {
    return @as([*]u8, @ptrCast(node))[@sizeOf(BufNode)..node.capacity];
}

pub fn init(child_allocator: Allocator) SingleThreadArena {
    return (State{}).promote(child_allocator);
}

pub fn deinit(arena: SingleThreadArena) void {
    var it = arena.state.first;
    while (it) |node| {
        // Read `next` before freeing, since the free invalidates the node.
        it = node.next;
        arena.child_allocator.rawFree(fullSlice(node), buf_node_align, @returnAddress());
    }
}

/// Returns the `std.mem.Allocator` interface backed by this arena.
pub fn allocator(arena: *SingleThreadArena) Allocator {
    return .{
        .ptr = arena,
        .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .remap = remap,
            .free = free,
        },
    };
}

/// Queries the current memory use of this arena, excluding chunk headers.
pub fn queryCapacity(arena: SingleThreadArena) usize {
    var size: usize = 0;
    var it = arena.state.first;
    while (it) |node| : (it = node.next) {
        size += node.capacity - @sizeOf(BufNode);
    }
    return size;
}

/// How `reset` should treat the arena's existing chunks.
pub const ResetMode = union(enum) {
    /// Releases all allocated memory in the arena.
    free_all,
    /// Retains a single chunk large enough to hold everything allocated since
    /// the last reset, so subsequent allocations avoid hitting the child
    /// allocator until that high-water mark is exceeded again.
    retain_capacity,
    /// Like `retain_capacity`, but caps the retained chunk at this many bytes.
    retain_with_limit: usize,
};

/// Resets the arena, freeing or retaining memory according to `mode`.
///
/// Returns `false` only when retaining capacity required a reallocation that
/// the child allocator could not satisfy; the arena stays fully usable in that
/// case, just without the requested preheating. `free_all` always returns
/// `true`.
pub fn reset(arena: *SingleThreadArena, mode: ResetMode) bool {
    const requested_capacity = switch (mode) {
        .retain_capacity => arena.queryCapacity(),
        .retain_with_limit => |limit| @min(limit, arena.queryCapacity()),
        .free_all => 0,
    };

    if (requested_capacity == 0) {
        arena.deinit();
        arena.state = .{};
        return true;
    }

    const total_size = requested_capacity + @sizeOf(BufNode);

    // Free every chunk except the oldest (the tail), which we keep and resize
    // to hold the requested capacity in one contiguous buffer.
    var it = arena.state.first;
    const node: *BufNode = while (it) |n| {
        const next = n.next;
        if (next == null) break n;
        arena.child_allocator.rawFree(fullSlice(n), buf_node_align, @returnAddress());
        it = next;
    } else {
        arena.state = .{};
        return true;
    };

    arena.state.first = node;
    arena.state.end_index = 0;
    node.next = null;

    if (node.capacity == total_size) return true;

    if (arena.child_allocator.rawResize(fullSlice(node), buf_node_align, total_size, @returnAddress())) {
        node.capacity = total_size;
        return true;
    }

    // Couldn't reach the requested capacity; the chunk is still valid as-is.
    return false;
}

fn createNode(arena: *SingleThreadArena, prev_capacity: usize, minimum_size: usize) ?*BufNode {
    const actual_min_size = minimum_size + (@sizeOf(BufNode) + 16);
    const big_enough_len = prev_capacity + actual_min_size;
    const len = big_enough_len + big_enough_len / 2;
    const ptr = arena.child_allocator.rawAlloc(len, buf_node_align, @returnAddress()) orelse
        return null;
    const node: *BufNode = @ptrCast(@alignCast(ptr));
    node.* = .{ .next = arena.state.first, .capacity = len };
    arena.state.first = node;
    arena.state.end_index = 0;
    return node;
}

fn alloc(ctx: *anyopaque, n: usize, alignment: Alignment, _: usize) ?[*]u8 {
    const arena: *SingleThreadArena = @ptrCast(@alignCast(ctx));
    assert(n > 0);

    const ptr_align = alignment.toByteUnits();
    var cur_node = arena.state.first orelse
        (arena.createNode(0, n + ptr_align) orelse return null);
    while (true) {
        const cur_buf = usableBuf(cur_node);
        const addr = @intFromPtr(cur_buf.ptr) + arena.state.end_index;
        const adjusted_addr = mem.alignForward(usize, addr, ptr_align);
        const adjusted_index = arena.state.end_index + (adjusted_addr - addr);
        const new_end_index = adjusted_index + n;

        if (new_end_index <= cur_buf.len) {
            arena.state.end_index = new_end_index;
            return cur_buf[adjusted_index..new_end_index].ptr;
        }

        // The head chunk is full. Try to grow it in place; otherwise chain a
        // new, larger chunk and bump from there.
        const bigger_buf_size = @sizeOf(BufNode) + new_end_index;
        if (arena.child_allocator.rawResize(fullSlice(cur_node), buf_node_align, bigger_buf_size, @returnAddress())) {
            cur_node.capacity = bigger_buf_size;
        } else {
            cur_node = arena.createNode(cur_buf.len, n + ptr_align) orelse return null;
        }
    }
}

fn resize(ctx: *anyopaque, buf: []u8, _: Alignment, new_len: usize, _: usize) bool {
    const arena: *SingleThreadArena = @ptrCast(@alignCast(ctx));

    const cur_node = arena.state.first orelse return false;
    const cur_buf = usableBuf(cur_node);
    if (@intFromPtr(cur_buf.ptr) + arena.state.end_index != @intFromPtr(buf.ptr) + buf.len) {
        // Not the most recent allocation: can't grow it, but shrinking is fine.
        return new_len <= buf.len;
    }

    if (buf.len >= new_len) {
        arena.state.end_index -= buf.len - new_len;
        return true;
    } else if (cur_buf.len - arena.state.end_index >= new_len - buf.len) {
        arena.state.end_index += new_len - buf.len;
        return true;
    } else {
        return false;
    }
}

fn remap(ctx: *anyopaque, buf: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    return if (resize(ctx, buf, alignment, new_len, ret_addr)) buf.ptr else null;
}

fn free(ctx: *anyopaque, buf: []u8, _: Alignment, _: usize) void {
    const arena: *SingleThreadArena = @ptrCast(@alignCast(ctx));

    const cur_node = arena.state.first orelse return;
    const cur_buf = usableBuf(cur_node);
    if (@intFromPtr(cur_buf.ptr) + arena.state.end_index == @intFromPtr(buf.ptr) + buf.len) {
        arena.state.end_index -= buf.len;
    }
}

test "basic allocation and free-all reset" {
    var arena = SingleThreadArena.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const first = try a.alloc(u8, 10);
    try std.testing.expectEqual(@as(usize, 10), first.len);

    // Force chunk growth with a large allocation.
    const big = try a.alloc(u64, 4096);
    try std.testing.expectEqual(@as(usize, 4096), big.len);

    try std.testing.expect(arena.reset(.free_all));
    try std.testing.expectEqual(@as(usize, 0), arena.queryCapacity());
}

test "alignment is honored across types" {
    var arena = SingleThreadArena.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    for (0..64) |_| {
        const p64 = try a.create(u64);
        try std.testing.expect(@intFromPtr(p64) % @alignOf(u64) == 0);
        _ = try a.alloc(u8, 1); // misalign the bump pointer
        const big_align = try a.alignedAlloc(u8, .@"64", 8);
        try std.testing.expect(@intFromPtr(big_align.ptr) % 64 == 0);
    }
}

test "retain_capacity preheats a single chunk" {
    var arena = SingleThreadArena.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var rounds: usize = 8;
    while (rounds > 0) : (rounds -= 1) {
        _ = arena.reset(.retain_capacity);
        var total: usize = 0;
        while (total < 8192) : (total += 64) {
            _ = try a.alloc(u8, 64);
        }
    }
    // After preheating, everything fits in one retained chunk.
    try std.testing.expect(arena.queryCapacity() >= 8192);
}

test "free reclaims only the most recent allocation" {
    var arena = SingleThreadArena.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const keep = try a.alloc(u8, 16);
    const last = try a.alloc(u8, 16);
    const end_with_both = arena.state.end_index;

    // `keep` is not the most recent allocation (`last` is), so freeing it must
    // not move the bump offset.
    a.free(keep);
    try std.testing.expectEqual(end_with_both, arena.state.end_index);

    // `last` is the most recent allocation, so freeing it rolls the bump back.
    a.free(last);
    try std.testing.expect(arena.state.end_index < end_with_both);
}

test "resize grows and shrinks the latest allocation" {
    var arena = SingleThreadArena.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var buf = try a.alloc(u8, 8);
    try std.testing.expect(a.resize(buf, 16));
    buf.len = 16;
    try std.testing.expect(a.resize(buf, 4));
}
