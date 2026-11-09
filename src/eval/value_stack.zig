//! A last-in, first-out byte stack for interpreter value storage.
//!
//! Memory is handed out by bumping through a list of chunks. A `Mark` names
//! the current top, and `restore` releases everything allocated after it, so
//! storage whose lifetime is a procedure activation or a statement is
//! reclaimed exactly when that activation or statement ends. Chunks are never
//! moved or freed before `deinit`, so an allocation's address is stable until
//! a `restore` releases it, and a restored stack reuses its chunks.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const ValueStack = @This();

gpa: Allocator,
/// Chunk buffers, oldest first. Chunks after `chunk_index` are empty and
/// kept for reuse.
chunks: std.ArrayListUnmanaged([]u8) = .empty,
/// The chunk allocations currently bump through.
chunk_index: usize = 0,
/// Bytes used in `chunks[chunk_index]`.
offset: usize = 0,

const min_chunk_size = 64 * 1024;
const chunk_alignment: Alignment = .@"16";

/// The top of the stack at the time `mark` was called.
pub const Mark = struct {
    chunk_index: usize,
    offset: usize,
};

/// An empty stack whose chunks come from `gpa`.
pub fn init(gpa: Allocator) ValueStack {
    return .{ .gpa = gpa };
}

/// Free every chunk.
pub fn deinit(self: *ValueStack) void {
    for (self.chunks.items) |chunk| self.gpa.rawFree(chunk, chunk_alignment, @returnAddress());
    self.chunks.deinit(self.gpa);
    self.* = .{ .gpa = self.gpa };
}

/// The current top, for a later `restore`.
pub fn mark(self: *const ValueStack) Mark {
    return .{ .chunk_index = self.chunk_index, .offset = self.offset };
}

/// Release every allocation made after `m`. `m` must not be newer than the
/// current top.
pub fn restore(self: *ValueStack, m: Mark) void {
    if (!(m.chunk_index < self.chunk_index or
        (m.chunk_index == self.chunk_index and m.offset <= self.offset)))
    {
        base.invariant("{s}", .{"interpreter invariant violated: value stack restored to a mark above its top"});
    }
    self.chunk_index = m.chunk_index;
    self.offset = m.offset;
}

/// Release every allocation, keeping the chunks for reuse.
pub fn reset(self: *ValueStack) void {
    self.chunk_index = 0;
    self.offset = 0;
}

/// `size` bytes at `alignment`, released by the next `restore` to a mark
/// taken before this call.
pub fn alloc(self: *ValueStack, size: usize, alignment: Alignment) Allocator.Error![*]u8 {
    return self.bump(size, alignment) orelse error.OutOfMemory;
}

fn bump(self: *ValueStack, size: usize, alignment: Alignment) ?[*]u8 {
    while (self.chunk_index < self.chunks.items.len) {
        const chunk = self.chunks.items[self.chunk_index];
        const chunk_addr = @intFromPtr(chunk.ptr);
        const start = alignment.forward(chunk_addr + self.offset) - chunk_addr;
        if (start + size <= chunk.len) {
            self.offset = start + size;
            return chunk.ptr + start;
        }
        if (self.chunk_index + 1 == self.chunks.items.len) break;
        self.chunk_index += 1;
        self.offset = 0;
    }

    const previous_len = if (self.chunks.items.len == 0) 0 else self.chunks.items[self.chunks.items.len - 1].len;
    const needed = size + alignment.toByteUnits();
    const len = @max(min_chunk_size, previous_len * 2, needed);
    self.chunks.ensureUnusedCapacity(self.gpa, 1) catch return null;
    const ptr = self.gpa.rawAlloc(len, chunk_alignment, @returnAddress()) orelse return null;
    self.chunks.appendAssumeCapacity(ptr[0..len]);
    self.chunk_index = self.chunks.items.len - 1;
    const chunk_addr = @intFromPtr(ptr);
    const start = alignment.forward(chunk_addr) - chunk_addr;
    self.offset = start + size;
    return ptr + start;
}

/// The `std.mem.Allocator` interface over this stack. `free` and `resize`
/// adjust the top only for the most recent allocation; anything else is
/// released by `restore`.
pub fn allocator(self: *ValueStack) Allocator {
    return .{
        .ptr = self,
        .vtable = &.{
            .alloc = allocFn,
            .resize = resizeFn,
            .remap = remapFn,
            .free = freeFn,
        },
    };
}

fn allocFn(ctx: *anyopaque, n: usize, alignment: Alignment, _: usize) ?[*]u8 {
    const self: *ValueStack = @ptrCast(@alignCast(ctx));
    return self.bump(n, alignment);
}

fn isTop(self: *const ValueStack, memory: []u8) bool {
    if (self.chunk_index >= self.chunks.items.len) return false;
    const chunk = self.chunks.items[self.chunk_index];
    return @intFromPtr(memory.ptr) + memory.len == @intFromPtr(chunk.ptr) + self.offset;
}

fn resizeFn(ctx: *anyopaque, memory: []u8, _: Alignment, new_len: usize, _: usize) bool {
    const self: *ValueStack = @ptrCast(@alignCast(ctx));
    if (!self.isTop(memory)) return new_len <= memory.len;
    const chunk = self.chunks.items[self.chunk_index];
    const start = @intFromPtr(memory.ptr) - @intFromPtr(chunk.ptr);
    if (start + new_len > chunk.len) return false;
    self.offset = start + new_len;
    return true;
}

fn remapFn(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    return if (resizeFn(ctx, memory, alignment, new_len, ret_addr)) memory.ptr else null;
}

fn freeFn(ctx: *anyopaque, memory: []u8, _: Alignment, _: usize) void {
    const self: *ValueStack = @ptrCast(@alignCast(ctx));
    if (!self.isTop(memory)) return;
    const chunk = self.chunks.items[self.chunk_index];
    self.offset = @intFromPtr(memory.ptr) - @intFromPtr(chunk.ptr);
}

test "restore releases allocations made after the mark and reuses their bytes" {
    var stack = ValueStack.init(std.testing.allocator);
    defer stack.deinit();

    const kept = try stack.alloc(24, .@"8");
    const m = stack.mark();
    const released = try stack.alloc(40, .@"16");
    stack.restore(m);
    const reused = try stack.alloc(40, .@"16");

    try std.testing.expect(released == reused);
    try std.testing.expect(@intFromPtr(kept) + 24 <= @intFromPtr(reused));
    try std.testing.expectEqual(@as(usize, 0), @intFromPtr(reused) % 16);
}

test "allocations stay put when a later allocation needs a new chunk" {
    var stack = ValueStack.init(std.testing.allocator);
    defer stack.deinit();

    const first = try stack.alloc(8, .@"8");
    first[0] = 42;
    const m = stack.mark();
    _ = try stack.alloc(min_chunk_size * 3, .@"8");
    try std.testing.expectEqual(@as(u8, 42), first[0]);
    const chunks_after_growth = stack.chunks.items.len;

    stack.restore(m);
    _ = try stack.alloc(min_chunk_size * 3, .@"8");
    try std.testing.expectEqual(chunks_after_growth, stack.chunks.items.len);
}

test "the allocator interface frees and resizes only the most recent allocation" {
    var stack = ValueStack.init(std.testing.allocator);
    defer stack.deinit();
    const a = stack.allocator();

    var list: std.ArrayListUnmanaged(u32) = .empty;
    for (0..1000) |i| try list.append(a, @intCast(i));
    for (list.items, 0..) |item, i| try std.testing.expectEqual(@as(u32, @intCast(i)), item);

    const before = stack.mark();
    const top = try a.alloc(u8, 16);
    a.free(top);
    try std.testing.expectEqual(before, stack.mark());
}
