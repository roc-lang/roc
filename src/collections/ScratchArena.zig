//! A mark-and-reset bump allocator over a retained block, for the tables that
//! live and die inside one pass.
//!
//! Allocation bumps a pointer. A pass's tables are released together by one
//! `reset` (or `resetTo` a mark), which touches none of their bytes. The first
//! block is kept across resets, and a reset after a pass that overflowed it
//! replaces it with one large enough for that whole pass, so after the first
//! pass every table is a pointer bump into memory that is already faulted in.
//! Blocks come from the owner's allocator, which recycles large blocks.
//!
//! Nothing allocated from a scratch arena outlives the pass that owns it. It
//! is single-threaded: each owner has its own.

const std = @import("std");
const storage_work = @import("storage_work.zig");

const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const ScratchArena = @This();

const block_alignment: Alignment = .@"16";
const Block = []align(block_alignment.toByteUnits()) u8;
const min_block_len: usize = 64 * 1024;

gpa: Allocator,
/// `blocks.items[0]` is retained across resets; later blocks hold a pass's
/// overflow and are freed by the reset that ends it.
blocks: std.ArrayListUnmanaged(Block) = .empty,
/// Bytes used in the last block.
used: usize = 0,
/// Peak live extent since the last full reset, including alignment and block slack.
requested: usize = 0,
/// Capacity of every block before the current block.
preceding_bytes: usize = 0,
/// The size of the next first block.
first_block_len: usize = min_block_len,

/// A position to roll back to.
pub const Mark = struct { block: usize, used: usize };

/// An arena that allocates its first block on first use.
pub fn init(gpa: Allocator) ScratchArena {
    return .{ .gpa = gpa };
}

/// Free every block.
pub fn deinit(self: *ScratchArena) void {
    for (self.blocks.items) |block| self.gpa.free(block);
    self.blocks.deinit(self.gpa);
    self.* = .{ .gpa = self.gpa };
}

/// The current position.
pub fn mark(self: *const ScratchArena) Mark {
    return .{ .block = self.blocks.items.len, .used = self.used };
}

/// Release everything allocated since `position`.
pub fn resetTo(self: *ScratchArena, position: Mark) void {
    std.debug.assert(position.block <= self.blocks.items.len);
    while (self.blocks.items.len > @max(position.block, 1)) {
        self.gpa.free(self.blocks.pop().?);
    }
    self.used = if (position.block == 0) 0 else position.used;
    self.preceding_bytes = 0;
    if (self.blocks.items.len != 0) {
        for (self.blocks.items[0 .. self.blocks.items.len - 1]) |block| self.preceding_bytes +|= block.len;
    }
}

/// Release everything, keeping one block large enough for the pass that just
/// ended. An undersized block is freed here and its replacement is
/// allocated by the next pass's first allocation, sized for this pass.
pub fn reset(self: *ScratchArena) void {
    storage_work.add(.scratch_resets, 1);
    self.resetTo(.{ .block = 0, .used = 0 });
    if (self.blocks.items.len == 1 and self.blocks.items[0].len < self.requested) {
        self.first_block_len = std.math.ceilPowerOfTwo(usize, self.requested) catch self.requested;
        self.gpa.free(self.blocks.pop().?);
    }
    self.requested = 0;
}

/// The arena as an `Allocator`. Freeing the most recent allocation rolls the
/// arena back; every other free is a no-op until the next reset.
pub fn allocator(self: *ScratchArena) Allocator {
    return .{ .ptr = self, .vtable = &vtable };
}

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn bump(self: *ScratchArena, len: usize, alignment: Alignment) ?[*]u8 {
    if (self.blocks.items.len != 0) {
        const block = self.blocks.items[self.blocks.items.len - 1];
        const start = alignment.forward(@intFromPtr(block.ptr) + self.used) - @intFromPtr(block.ptr);
        if (start <= block.len and len <= block.len - start) {
            self.used = start + len;
            self.notePeak();
            return block.ptr + start;
        }
    }
    const grown_len = if (self.blocks.items.len == 0) self.first_block_len else self.blocks.items[self.blocks.items.len - 1].len *| 2;
    const needed = std.math.add(usize, len, alignment.toByteUnits() - 1) catch return null;
    const block_len = @max(grown_len, needed);
    self.blocks.ensureUnusedCapacity(self.gpa, 1) catch return null;
    const fresh = self.gpa.alignedAlloc(u8, block_alignment, block_len) catch return null;
    if (self.blocks.items.len != 0) self.preceding_bytes +|= self.blocks.items[self.blocks.items.len - 1].len;
    self.blocks.appendAssumeCapacity(fresh);
    const start = alignment.forward(@intFromPtr(fresh.ptr)) - @intFromPtr(fresh.ptr);
    self.used = start + len;
    self.notePeak();
    return fresh.ptr + start;
}

fn notePeak(self: *ScratchArena) void {
    self.requested = @max(self.requested, self.preceding_bytes +| self.used);
}

fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
    _ = ret_addr;
    const self: *ScratchArena = @ptrCast(@alignCast(ctx));
    const ptr = self.bump(len, alignment) orelse return null;
    storage_work.add(.scratch_bytes, len);
    return ptr;
}

fn isLast(self: *const ScratchArena, memory: []u8) bool {
    if (self.blocks.items.len == 0) return false;
    const block = self.blocks.items[self.blocks.items.len - 1];
    return @intFromPtr(memory.ptr) + memory.len == @intFromPtr(block.ptr) + self.used;
}

fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
    _ = alignment;
    _ = ret_addr;
    const self: *ScratchArena = @ptrCast(@alignCast(ctx));
    if (new_len <= memory.len) {
        if (self.isLast(memory)) self.used -= memory.len - new_len;
        return true;
    }
    if (!self.isLast(memory)) return false;
    const block = self.blocks.items[self.blocks.items.len - 1];
    const start = @intFromPtr(memory.ptr) - @intFromPtr(block.ptr);
    if (new_len > block.len - start) return false;
    self.used = start + new_len;
    self.notePeak();
    storage_work.add(.scratch_bytes, new_len - memory.len);
    return true;
}

fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    return if (resize(ctx, memory, alignment, new_len, ret_addr)) memory.ptr else null;
}

fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void {
    _ = alignment;
    _ = ret_addr;
    const self: *ScratchArena = @ptrCast(@alignCast(ctx));
    if (self.isLast(memory)) self.used -= memory.len;
}

const testing = std.testing;

test "tables from one pass are released together and the block is retained" {
    var arena = ScratchArena.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    const first = try a.alloc(u32, 100);
    @memset(first, 7);
    const second = try a.alloc(u64, 10);
    try testing.expect(@intFromPtr(second.ptr) % @alignOf(u64) == 0);
    arena.reset();
    const again = try a.alloc(u32, 100);
    try testing.expectEqual(first.ptr, again.ptr);
}

test "a pass that overflows the block gets one large enough on reset" {
    var arena = ScratchArena.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    _ = try a.alloc(u8, min_block_len - 8);
    _ = try a.alloc(u8, min_block_len * 3);
    try testing.expectEqual(@as(usize, 2), arena.blocks.items.len);
    arena.reset();
    try testing.expectEqual(@as(usize, 0), arena.blocks.items.len);
    _ = try a.alloc(u8, min_block_len - 8);
    try testing.expect(arena.blocks.items[0].len >= min_block_len * 4 - 8);
    _ = try a.alloc(u8, min_block_len * 3);
    try testing.expectEqual(@as(usize, 1), arena.blocks.items.len);
}

test "resetting to a mark keeps what came before it" {
    var arena = ScratchArena.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    const kept = try a.alloc(u16, 4);
    @memcpy(kept, &[_]u16{ 1, 2, 3, 4 });
    const position = arena.mark();
    _ = try a.alloc(u8, min_block_len * 2);
    arena.resetTo(position);
    try testing.expectEqualSlices(u16, &.{ 1, 2, 3, 4 }, kept);
    const next = try a.alloc(u16, 1);
    try testing.expectEqual(@intFromPtr(kept.ptr) + 8, @intFromPtr(next.ptr));
}

test "growing the newest table stays in place" {
    var arena = ScratchArena.init(testing.allocator);
    defer arena.deinit();
    var list: std.ArrayListUnmanaged(u32) = .empty;
    const a = arena.allocator();
    for (0..1000) |i| try list.append(a, @intCast(i));
    for (list.items, 0..) |item, i| try testing.expectEqual(@as(u32, @intCast(i)), item);
}

test "repeated marked scopes retain peak demand rather than cumulative allocation" {
    var arena = ScratchArena.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    const kept = try a.alloc(u8, 16);
    const position = arena.mark();
    for (0..1000) |_| {
        _ = try a.alloc(u8, 1000);
        arena.resetTo(position);
    }
    try testing.expectEqual(@as(usize, 1016), arena.requested);
    arena.reset();
    try testing.expectEqual(@as(usize, 1), arena.blocks.items.len);
    const again = try a.alloc(u8, 16);
    try testing.expectEqual(kept.ptr, again.ptr);
}

test "scratch allocation rejects an overflowing aligned extent" {
    var arena = ScratchArena.init(testing.allocator);
    defer arena.deinit();
    try testing.expect(arena.bump(std.math.maxInt(usize), .@"16") == null);
}
