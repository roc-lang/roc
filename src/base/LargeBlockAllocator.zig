//! Allocator wrapper that owns large blocks itself and recycles them.
//!
//! The general-purpose allocators used by the compiler hand every request of
//! `large_threshold` bytes or more straight to the page allocator and return
//! it to the kernel on free. Compiler data structures grow by doubling and are
//! freed in bulk, so that policy turns each growth step into a fresh mapping,
//! a fault per page while copying into it, and an unmap: the kernel work
//! dominates the copy. This wrapper keeps freed large blocks in power-of-two
//! size classes and hands them back on the next request of the same class, so
//! a block that has already been faulted in is reused instead of remapped.
//!
//! Blocks of two megabytes or more ask the kernel for transparent huge pages,
//! which cuts the fault count of a fresh block by a factor of 512 where the
//! kernel honors the advice.
//!
//! Requests below the threshold, and requests with an alignment above the page
//! size, go to the backing allocator unchanged.
const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;
const PageAllocator = std.heap.PageAllocator;

const Self = @This();

/// Requests of at least this many bytes are owned by this allocator. It
/// matches the slab size above which `SmpAllocator` maps pages directly.
pub const large_threshold: usize = 64 * 1024;
/// A size class: the base-2 logarithm of a length, rounded up. Its type holds
/// every such logarithm of a `usize` on the target, including the word width.
const Class = std.math.Log2IntCeil(usize);
const min_class_log2: Class = std.math.log2_int(usize, large_threshold);
/// Largest cached class; bigger blocks are mapped and unmapped directly.
const max_class_log2: Class = 30; // 1 GiB
const class_count = max_class_log2 - min_class_log2 + 1;
/// Bytes retained per class beyond which a freed block is unmapped, so the
/// cache bounds peak memory at a few times the live set rather than growing
/// with every block the compiler ever freed.
const max_cached_bytes_per_class: usize = 128 * 1024 * 1024;
const huge_page_threshold: usize = 2 * 1024 * 1024;

const FreeBlock = struct {
    next: ?*FreeBlock,
};

const ClassCache = struct {
    mutex: std.atomic.Mutex = .unlocked,
    head: ?*FreeBlock = null,
    count: usize = 0,

    /// Critical sections here are a few instructions, so spinning is cheaper
    /// than a blocking lock, and it keeps the allocator free of `std.Io`.
    fn lock(self: *ClassCache) void {
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
    }

    fn unlock(self: *ClassCache) void {
        self.mutex.unlock();
    }
};

backing: Allocator,
classes: [class_count]ClassCache = @splat(.{}),

/// A wrapper that sends small and over-aligned requests to `backing`.
pub fn init(backing: Allocator) Self {
    return .{ .backing = backing };
}

/// The `Allocator` interface backed by this wrapper.
pub fn allocator(self: *Self) Allocator {
    return .{ .ptr = self, .vtable = &vtable };
}

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn classOf(len: usize) Class {
    std.debug.assert(len >= large_threshold);
    return std.math.log2_int_ceil(usize, len);
}

/// Only cached classes have a capacity; larger requests keep their exact length.
fn classCapacity(class: Class) usize {
    std.debug.assert(class <= max_class_log2);
    const shift: std.math.Log2Int(usize) = @intCast(class);
    return @as(usize, 1) << shift;
}

fn isLarge(len: usize, alignment: Alignment) bool {
    return len >= large_threshold and alignment.toByteUnits() <= std.heap.pageSize();
}

fn mapBlock(capacity: usize) ?[*]u8 {
    const ptr = PageAllocator.map(capacity, .fromByteUnits(std.heap.pageSize())) orelse return null;
    if (builtin.os.tag == .linux and capacity >= huge_page_threshold) {
        const aligned: [*]align(std.heap.page_size_min) u8 = @alignCast(ptr);
        std.posix.madvise(aligned, capacity, std.os.linux.MADV.HUGEPAGE) catch {};
    }
    return ptr;
}

fn unmapBlock(ptr: [*]u8, capacity: usize) void {
    const aligned: [*]align(std.heap.page_size_min) u8 = @alignCast(ptr);
    PageAllocator.unmap(aligned[0..capacity]);
}

fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));
    if (!isLarge(len, alignment)) return self.backing.rawAlloc(len, alignment, ret_addr);
    const class = classOf(len);
    if (class > max_class_log2) return mapBlock(len);
    const cache = &self.classes[class - min_class_log2];
    cache.lock();
    if (cache.head) |block| {
        cache.head = block.next;
        cache.count -= 1;
        cache.unlock();
        return @ptrCast(block);
    }
    cache.unlock();
    return mapBlock(classCapacity(class));
}

/// Growth and shrinkage stay in place while the class, and therefore the
/// mapped capacity that `free` will derive from the length, is unchanged.
fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
    const self: *Self = @ptrCast(@alignCast(ctx));
    const old_large = isLarge(memory.len, alignment);
    const new_large = isLarge(new_len, alignment);
    if (!old_large and !new_large) return self.backing.rawResize(memory, alignment, new_len, ret_addr);
    if (!old_large or !new_large) return false;
    const old_class = classOf(memory.len);
    if (old_class > max_class_log2) {
        return new_len <= memory.len and classOf(new_len) > max_class_log2;
    }
    return classOf(new_len) == old_class;
}

fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));
    const old_large = isLarge(memory.len, alignment);
    const new_large = isLarge(new_len, alignment);
    if (!old_large and !new_large) return self.backing.rawRemap(memory, alignment, new_len, ret_addr);
    if (!old_large or !new_large) return null;
    if (resize(ctx, memory, alignment, new_len, ret_addr)) return memory.ptr;
    const old_class = classOf(memory.len);
    const new_class = classOf(new_len);
    if (new_class > max_class_log2 or old_class > max_class_log2) return null;
    // Growing into a class with a cached block copies into already-faulted
    // pages; otherwise the kernel moves the mapping without copying.
    const cache = &self.classes[new_class - min_class_log2];
    cache.lock();
    const cached = cache.head;
    if (cached) |block| {
        cache.head = block.next;
        cache.count -= 1;
    }
    cache.unlock();
    if (cached) |block| {
        const dst: [*]u8 = @ptrCast(block);
        @memcpy(dst[0..@min(memory.len, new_len)], memory.ptr[0..@min(memory.len, new_len)]);
        free(ctx, memory, alignment, ret_addr);
        return dst;
    }
    const old_aligned: []align(std.heap.page_size_min) u8 = @alignCast(memory.ptr[0..classCapacity(old_class)]);
    const moved = PageAllocator.realloc(old_aligned, alignment, classCapacity(new_class), true) orelse return null;
    if (builtin.os.tag == .linux and classCapacity(new_class) >= huge_page_threshold) {
        std.posix.madvise(@alignCast(moved), classCapacity(new_class), std.os.linux.MADV.HUGEPAGE) catch {};
    }
    return moved;
}

fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    if (!isLarge(memory.len, alignment)) return self.backing.rawFree(memory, alignment, ret_addr);
    const class = classOf(memory.len);
    if (class > max_class_log2) return unmapBlock(memory.ptr, memory.len);
    const cache = &self.classes[class - min_class_log2];
    cache.lock();
    if ((cache.count + 1) * classCapacity(class) > max_cached_bytes_per_class) {
        cache.unlock();
        return unmapBlock(memory.ptr, classCapacity(class));
    }
    const block: *FreeBlock = @ptrCast(@alignCast(memory.ptr));
    block.* = .{ .next = cache.head };
    cache.head = block;
    cache.count += 1;
    cache.unlock();
}

test "large blocks are recycled through their size class" {
    var wrapper = Self.init(std.testing.allocator);
    const a = wrapper.allocator();
    const first = try a.alloc(u8, large_threshold + 1);
    first[0] = 7;
    const first_ptr = first.ptr;
    a.free(first);
    const second = try a.alloc(u8, large_threshold + 4096);
    defer a.free(second);
    try std.testing.expectEqual(first_ptr, second.ptr);
    const third = try a.alloc(u8, large_threshold * 3);
    defer a.free(third);
    try std.testing.expect(third.ptr != second.ptr);
}

test "resize stays in place within a class and refuses class changes" {
    var wrapper = Self.init(std.testing.allocator);
    const a = wrapper.allocator();
    var block = try a.alloc(u8, large_threshold + 1);
    defer a.free(block);
    try std.testing.expect(a.resize(block, large_threshold * 2));
    block.len = large_threshold * 2;
    try std.testing.expect(!a.resize(block, large_threshold * 2 + 1));
    try std.testing.expect(!a.resize(block, large_threshold - 1));
}

test "remap grows across classes and preserves contents" {
    var wrapper = Self.init(std.testing.allocator);
    const a = wrapper.allocator();
    var list = std.ArrayList(u64).empty;
    defer list.deinit(a);
    var i: u64 = 0;
    while (i < 1_000_000) : (i += 1) try list.append(a, i);
    i = 0;
    while (i < 1_000_000) : (i += 100_003) try std.testing.expectEqual(i, list.items[@intCast(i)]);
    list.clearAndFree(a);
    try std.testing.expectEqual(@as(usize, 0), list.capacity);
}

test "small requests pass through to the backing allocator" {
    var wrapper = Self.init(std.testing.allocator);
    const a = wrapper.allocator();
    const small = try a.alloc(u8, 100);
    const grown = try a.realloc(small, 200);
    a.free(grown);
    const back = try a.alloc(u8, 100);
    a.free(back);
}
