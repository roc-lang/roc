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
//! A helper thread, started on the first request the cache cannot serve,
//! maps and faults in replacement blocks ahead of demand: each request that
//! had to map a fresh block asks the helper for one more block of that size
//! class, and the helper keeps a small reserve per class. The compute threads
//! then find already-faulted memory in the cache instead of taking the fault
//! per page themselves. The helper is pinned to the slow cores of a hybrid
//! CPU, so the faulting work runs on cores the compiler would otherwise leave
//! idle.
//!
//! Requests below the threshold, and requests with an alignment above the page
//! size, go to the backing allocator unchanged.
const std = @import("std");
const builtin = @import("builtin");
const cpu_count = @import("cpu_count.zig");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;
const PageAllocator = std.heap.PageAllocator;

const Self = @This();

/// Requests of at least this many bytes are owned by this allocator. It
/// matches the slab size above which `SmpAllocator` maps pages directly.
pub const large_threshold: usize = 64 * 1024;
const min_class_log2: u6 = std.math.log2_int(usize, large_threshold);
/// Largest cached class; bigger blocks are mapped and unmapped directly.
const max_class_log2: u6 = 30; // 1 GiB
const class_count = max_class_log2 - min_class_log2 + 1;
/// Bytes retained per class beyond which a freed block is unmapped, so the
/// cache bounds peak memory at a few times the live set rather than growing
/// with every block the compiler ever freed.
const max_cached_bytes_per_class: usize = 128 * 1024 * 1024;
const huge_page_threshold: usize = 2 * 1024 * 1024;
/// Largest class the helper thread faults in ahead of demand; a reserve of
/// bigger blocks would cost more memory than the faults it saves.
const max_prefault_class_log2: u6 = 26; // 64 MiB
const prefault_class_count = max_prefault_class_log2 - min_class_log2 + 1;
/// Bytes of prefaulted reserve the helper keeps per class, so small classes
/// hold a few blocks and large classes hold one.
const prefault_reserve_bytes: usize = 32 * 1024 * 1024;
const max_prefault_reserve_blocks: usize = 4;
/// `madvise` advice that faults a range in for writing (Linux 5.14+).
const madv_populate_write: u32 = 23;

const prefault_supported = !builtin.single_threaded and
    builtin.os.tag != .freestanding and !builtin.cpu.arch.isWasm();

const FreeBlock = struct {
    next: ?*FreeBlock,
};

const ClassCache = struct {
    mutex: std.atomic.Mutex = .unlocked,
    head: ?*FreeBlock = null,
    count: usize = 0,
    /// Fresh mappings this class had to make that the helper thread has not
    /// yet answered with a prefaulted block.
    wanted: std.atomic.Value(u32) = .init(0),

    /// Critical sections here are a few instructions, so spinning is cheaper
    /// than a blocking lock, and it keeps the allocator free of `std.Io`.
    fn lock(self: *ClassCache) void {
        while (!self.mutex.tryLock()) std.atomic.spinLoopHint();
    }

    fn unlock(self: *ClassCache) void {
        self.mutex.unlock();
    }
};

/// Lifecycle of the prefaulting helper thread.
const HelperState = enum(u8) { disabled, not_started, running };

/// Futex word the helper sleeps on: `working` while it drains requests,
/// `sleeping` once it has parked, `signaled` when a request arrived.
const HelperSignal = enum(u32) { working, sleeping, signaled };

backing: Allocator,
classes: [class_count]ClassCache = @splat(.{}),
helper_state: std.atomic.Value(HelperState) = .init(.disabled),
helper_signal: std.atomic.Value(HelperSignal) = .init(.working),

pub fn init(backing: Allocator) Self {
    return .{ .backing = backing };
}

/// Like `init`, but a helper thread faults in replacement blocks ahead of
/// demand. Only for an allocator that lives as long as the process: the
/// helper is never joined.
pub fn initPrefaulting(backing: Allocator) Self {
    return .{
        .backing = backing,
        .helper_state = .init(if (prefault_supported) .not_started else .disabled),
    };
}

pub fn allocator(self: *Self) Allocator {
    return .{ .ptr = self, .vtable = &vtable };
}

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn classOf(len: usize) u6 {
    std.debug.assert(len >= large_threshold);
    return @intCast(std.math.log2_int_ceil(usize, len));
}

fn classCapacity(class: u6) usize {
    return @as(usize, 1) << class;
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
        const remaining = cache.count;
        cache.unlock();
        if (remaining < reserveTarget(class) and cache.wanted.load(.monotonic) != 0) self.signalHelper();
        return @ptrCast(block);
    }
    cache.unlock();
    self.noteMiss(class);
    return mapBlock(classCapacity(class));
}

/// Number of prefaulted blocks the helper keeps ready in a class.
fn reserveTarget(class: u6) usize {
    return @max(1, @min(max_prefault_reserve_blocks, prefault_reserve_bytes / classCapacity(class)));
}

/// A request in `class` had to map a fresh block: ask the helper for a
/// prefaulted replacement, starting it on the first such request.
fn noteMiss(self: *Self, class: u6) void {
    if (class > max_prefault_class_log2) return;
    switch (self.helper_state.load(.acquire)) {
        .disabled => return,
        .not_started => self.startHelper(),
        .running => {},
    }
    _ = self.classes[class - min_class_log2].wanted.fetchAdd(1, .monotonic);
    self.signalHelper();
}

fn startHelper(self: *Self) void {
    if (!prefault_supported) return;
    if (self.helper_state.cmpxchgStrong(.not_started, .running, .acq_rel, .acquire) != null) return;
    const thread = std.Thread.spawn(.{ .stack_size = 256 * 1024 }, helperMain, .{self}) catch {
        self.helper_state.store(.disabled, .release);
        return;
    };
    thread.setName(helperIo(), "roc-prefault") catch {};
    thread.detach();
}

fn helperIo() std.Io {
    return std.Io.Threaded.global_single_threaded.io();
}

fn signalHelper(self: *Self) void {
    if (!prefault_supported) return;
    if (self.helper_signal.swap(.signaled, .release) == .sleeping) {
        helperIo().futexWake(HelperSignal, &self.helper_signal.raw, 1);
    }
}

fn helperMain(self: *Self) void {
    cpu_count.pinCurrentThreadToEfficiencyCores();
    while (true) {
        self.helper_signal.store(.working, .monotonic);
        var produced = true;
        while (produced) {
            produced = false;
            var index: usize = 0;
            while (index < prefault_class_count) : (index += 1) {
                if (self.prefaultOne(index)) produced = true;
            }
        }
        if (self.helper_signal.cmpxchgStrong(.working, .sleeping, .acq_rel, .acquire) == null) {
            helperIo().futexWaitUncancelable(HelperSignal, &self.helper_signal.raw, .sleeping);
        }
    }
}

/// Fault in one block for the class at `index` if it has an unanswered
/// request and its reserve is below target. Returns whether a block was added.
fn prefaultOne(self: *Self, index: usize) bool {
    const cache = &self.classes[index];
    if (cache.wanted.load(.monotonic) == 0) return false;
    const class: u6 = @intCast(index + min_class_log2);
    cache.lock();
    const count = cache.count;
    cache.unlock();
    if (count >= reserveTarget(class)) return false;
    const capacity = classCapacity(class);
    const ptr = mapBlock(capacity) orelse return false;
    populate(ptr, capacity);
    const block: *FreeBlock = @ptrCast(@alignCast(ptr));
    cache.lock();
    block.* = .{ .next = cache.head };
    cache.head = block;
    cache.count += 1;
    cache.unlock();
    _ = cache.wanted.fetchSub(1, .monotonic);
    return true;
}

/// Fault every page of a fresh mapping in for writing.
fn populate(ptr: [*]u8, capacity: usize) void {
    if (builtin.os.tag == .linux) {
        const aligned: [*]align(std.heap.page_size_min) u8 = @alignCast(ptr);
        if (std.os.linux.errno(std.os.linux.madvise(aligned, capacity, madv_populate_write)) == .SUCCESS) return;
    }
    const page_size = std.heap.pageSize();
    var offset: usize = 0;
    while (offset < capacity) : (offset += page_size) {
        const page: *volatile u8 = &ptr[offset];
        page.* = 0;
    }
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
    self.noteMiss(new_class);
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

test "a prefaulted block answers the next request in its class" {
    var wrapper = Self.init(std.testing.allocator);
    const a = wrapper.allocator();
    const class = classOf(large_threshold);
    const cache = &wrapper.classes[class - min_class_log2];
    try std.testing.expect(!wrapper.prefaultOne(class - min_class_log2));
    cache.wanted.store(1, .monotonic);
    try std.testing.expect(wrapper.prefaultOne(class - min_class_log2));
    try std.testing.expectEqual(@as(usize, 1), cache.count);
    try std.testing.expectEqual(@as(u32, 0), cache.wanted.load(.monotonic));
    const ready = cache.head.?;
    const block = try a.alloc(u8, large_threshold);
    defer a.free(block);
    try std.testing.expectEqual(@as(*FreeBlock, ready), @as(*FreeBlock, @ptrCast(@alignCast(block.ptr))));
    try std.testing.expectEqual(@as(usize, 0), cache.count);
}

test "the prefault reserve stops at its target" {
    var wrapper = Self.init(std.testing.allocator);
    const a = wrapper.allocator();
    const class: u6 = max_prefault_class_log2 - 1;
    const index = class - min_class_log2;
    const cache = &wrapper.classes[index];
    try std.testing.expectEqual(@as(usize, 1), reserveTarget(class));
    try std.testing.expectEqual(@as(usize, 4), reserveTarget(min_class_log2));
    cache.wanted.store(3, .monotonic);
    try std.testing.expect(wrapper.prefaultOne(index));
    try std.testing.expect(!wrapper.prefaultOne(index));
    try std.testing.expectEqual(@as(usize, 1), cache.count);
    try std.testing.expectEqual(@as(u32, 2), cache.wanted.load(.monotonic));
    const block = try a.alloc(u8, classCapacity(class));
    a.free(block);
    try std.testing.expectEqual(@as(usize, 1), cache.count);
    while (cache.head) |head| {
        cache.head = head.next;
        cache.count -= 1;
        unmapBlock(@ptrCast(head), classCapacity(class));
    }
}

test "populate leaves every page mapped and zeroed" {
    const capacity = large_threshold;
    const ptr = mapBlock(capacity).?;
    defer unmapBlock(ptr, capacity);
    populate(ptr, capacity);
    var offset: usize = 0;
    while (offset < capacity) : (offset += std.heap.pageSize()) {
        try std.testing.expectEqual(@as(u8, 0), ptr[offset]);
    }
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
