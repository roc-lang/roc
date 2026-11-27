//! The compiler's multi-threaded allocator for requests of up to 32 KiB.
//!
//! The compiler builds data on worker threads and frees much of it on other
//! threads, and it grows lists step by step through many sizes. An allocator
//! that keeps each freed slot on the freeing thread's list for that exact size
//! class strands the slot whenever no later request on that thread asks for
//! that class, and power-of-two classes waste a quarter of every list that
//! grows by half. This allocator is shaped so that neither happens:
//!
//! - Size classes are 8 bytes, then every 16 bytes up to 128, then four
//!   classes per doubling up to 32 KiB, so a request is rounded up by at most
//!   a quarter of its size and by about a tenth on average.
//! - Memory comes in segments of `segment_size` bytes, aligned to their size
//!   and split into slices of `slice_size` bytes. The first slice of a
//!   segment holds the descriptors of every slice, so the descriptor of any
//!   block is found from its address alone. A span of one, two, four or eight
//!   slices holds the slots of one size class.
//! - Each size class has one central store shared by every thread under that
//!   class's own lock: a bounded stack of batches of free slots, and the list
//!   of spans with free slots. A freed slot reaches the central store within
//!   two batches, and any thread can allocate it again from there.
//! - Each thread slot keeps a small cache per size class in front of the
//!   central store, so allocation and free usually touch only the calling
//!   thread's slot. The cache holds at most two batches of a class, and it
//!   moves whole batches to and from the central store, so a trip there
//!   usually pushes or pops one batch in constant time. Only when the batch
//!   stack is full or empty does the trip return slots to their spans or take
//!   them from spans, and slots never handed out before are linked into a
//!   batch after the class lock is released, so their first-touch page faults
//!   happen outside it.
//! - A span whose slots are all free goes back to a page heap shared by every
//!   class: a buddy allocator over the segments' slices, which merges free
//!   neighbors so a block freed by one class can serve another of any size.
//! - The page heap keeps freed slices resident up to `default_max_free_resident_bytes`,
//!   so a block reused soon costs no page faults. A block freed while the
//!   resident free total is at that bound is released to the operating system
//!   with `MADV_DONTNEED`; its address range stays reserved for reuse. On
//!   Windows, giving pages back means decommitting them, and a decommitted
//!   page must be committed again before it is touched, so there the freed
//!   slices stay resident.
//!
//! Requests above 32 KiB, and requests aligned beyond a page, are mapped
//! directly from the operating system. The compiler's default allocator puts
//! `LargeBlockAllocator` in front of this one, so only over-aligned requests
//! reach that path in practice.
//!
//! Every thread slot, size class and the page heap is guarded by its own
//! spinning `std.atomic.Mutex`; the lock order is thread slot, then size
//! class, then page heap. Locks are held for a few dozen instructions, except
//! that the page heap maps segments and releases pages under its lock.
const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;
const PageAllocator = std.heap.PageAllocator;

const Self = @This();

/// Largest request served from size classes.
pub const max_small_len: usize = 32 * 1024;
/// Bytes in one slice, the page heap's unit.
pub const slice_size: usize = 64 * 1024;
/// Slices in one segment; the first one holds the segment's descriptors.
const slices_per_segment = 64;
/// Bytes in one segment, which is also its alignment.
pub const segment_size: usize = slice_size * slices_per_segment;
/// A free block holds 2^order slices; the largest is half a segment.
const max_order = 5;
/// Largest alignment a size class can promise: every slice is aligned to
/// `slice_size`, so a slot is aligned to the largest power of two that
/// divides its size, and a page-aligned request uses a class whose size is a
/// multiple of the page.
const max_class_alignment: usize = 4096;

/// Default bound on free page-heap bytes that may still be resident. Freed
/// blocks up to this total are kept as they are, so a span the compiler frees
/// and allocates again soon (one class emptying while another grows) costs no
/// page faults; past it, a freed block's pages are released. The bound is
/// a small fraction of the compiler's small-object heap, so it caps the memory
/// held without being used while still absorbing the churn between phases.
pub const default_max_free_resident_bytes: usize = 8 * 1024 * 1024;

/// Bytes of one batch, the unit moved between a thread slot's cache and a
/// class's central store; a thread slot holds at most two batches per class.
/// A batch amortizes one trip to the class lock over many requests, and its
/// size bounds the memory that idle thread slots hold.
const batch_bytes: usize = 16 * 1024;
/// Every batch holds at least this many slots, so that a thread allocating
/// and freeing a few large slots in turn stays in its own cache.
const min_batch_slots: u32 = 2;
/// No batch holds more than this many slots, which bounds the work of
/// returning one batch to its spans under a class lock.
const max_batch_slots: u32 = 128;
/// Bytes of free batches each class's central store keeps before freed
/// batches go back to their spans. The stack lets one thread's frees feed
/// another thread's allocations in constant time per batch; its bound keeps
/// spans from staying pinned by free slots in it.
const central_batch_bytes: usize = 1024 * 1024;
/// Most batches one class's central store keeps.
const max_central_batches = 64;

/// Size classes: 8 bytes, every 16 bytes to 128, then 1.25, 1.5, 1.75 and 2
/// times each power of two from 128 to 16 KiB.
pub const class_count = 9 + 4 * 8;
/// Size class of a request: an index into `class_sizes`.
const Class = u8;

/// Slot size of each class.
pub const class_sizes: [class_count]u32 = blk: {
    var sizes: [class_count]u32 = undefined;
    sizes[0] = 8;
    for (1..9) |i| sizes[i] = 16 * i;
    var i: usize = 9;
    for (7..15) |e| {
        for (4..8) |q| {
            sizes[i] = (q + 1) << (e - 2);
            i += 1;
        }
    }
    std.debug.assert(sizes[class_count - 1] == max_small_len);
    break :blk sizes;
};

/// Fewest slots a span holds. Spans of the large classes come and go through
/// the shared page heap; holding several slots each keeps that traffic, and
/// the contention for the page heap's lock, to a fraction of the requests.
const min_span_slots = 8;

/// Order of the span (2^order slices) that holds each class: the smallest
/// that holds `min_span_slots` slots and whose unusable tail, after the last
/// whole slot, is at most a sixteenth.
const class_span_orders: [class_count]u8 = blk: {
    var orders: [class_count]u8 = undefined;
    for (class_sizes, 0..) |size, c| {
        var order: u8 = 0;
        while ((slice_size << order) / size < min_span_slots or
            (slice_size << order) % size > (slice_size << order) / 16) order += 1;
        std.debug.assert(order <= 3);
        orders[c] = order;
    }
    break :blk orders;
};

/// Slot count of a span of each class.
const class_capacities: [class_count]u32 = blk: {
    var capacities: [class_count]u32 = undefined;
    for (class_sizes, class_span_orders, 0..) |size, order, c| capacities[c] = @intCast((slice_size << order) / size);
    break :blk capacities;
};

/// Slots in one batch of each class.
const class_batch_slots: [class_count]u32 = blk: {
    var slots: [class_count]u32 = undefined;
    for (class_sizes, 0..) |size, c| {
        slots[c] = @max(min_batch_slots, @min(max_batch_slots, batch_bytes / size));
    }
    break :blk slots;
};

/// Batches each class's central store keeps.
const class_central_batches: [class_count]u32 = blk: {
    var batches: [class_count]u32 = undefined;
    for (class_sizes, class_batch_slots, 0..) |size, slots, c| {
        batches[c] = @max(1, @min(max_central_batches, central_batch_bytes / (size * slots)));
    }
    break :blk batches;
};

/// Class of a length of at most 128 bytes, indexed by the length in 8-byte
/// units, rounded up.
const tiny_classes: [17]Class = blk: {
    var classes: [17]Class = undefined;
    for (0..17) |units| {
        const len = @max(units * 8, 1);
        var c: Class = 0;
        while (class_sizes[c] < len) c += 1;
        classes[units] = c;
    }
    break :blk classes;
};

/// Smallest class whose slots hold `len` bytes. `len` is at most `max_small_len`.
fn classForLen(len: usize) Class {
    std.debug.assert(len <= max_small_len);
    if (len <= 128) return tiny_classes[(len + 7) >> 3];
    const e: usize = std.math.log2_int(usize, len - 1);
    const q = (len - 1) >> @intCast(e - 2);
    return @intCast(9 + (e - 7) * 4 + (q - 4));
}

/// The class serving a request, or null when the request is mapped directly.
/// It depends only on the length and alignment, so `free` and `resize` find
/// the class a block was allocated from without reading any metadata.
fn classOf(len: usize, alignment: Alignment) ?Class {
    if (len > max_small_len) return null;
    const align_bytes = alignment.toByteUnits();
    if (align_bytes > max_class_alignment) return null;
    var c = classForLen(len);
    while (lowBit(class_sizes[c]) < align_bytes) {
        c += 1;
        if (c == class_count) return null;
    }
    return c;
}

fn lowBit(size: u32) usize {
    return size & (~size +% 1);
}

const Node = struct { next: ?*Node };

/// One slice's descriptor. The first slice of a block describes it; the
/// other slices of a span point back to it.
const SliceDesc = struct {
    /// Previous and next block in the list holding this one: a page-heap
    /// free list for a free block, the class's central list for a span.
    prev: ?*SliceDesc = null,
    next: ?*SliceDesc = null,
    /// Free slots of a span that have been handed out before.
    free_list: ?*Node = null,
    /// Slots of a span held by the program or by thread caches.
    used: u32 = 0,
    /// Slots of a span handed out at least once; slots past this have never
    /// been touched.
    carved: u32 = 0,
    kind: Kind = .interior,
    /// Size order of the block: 2^order slices.
    order: u8 = 0,
    /// Size class of a span.
    class: Class = 0,
    /// Index of the slice that describes the span holding this slice.
    head: u8 = 0,
    /// Whether a span is in its class's central list (it has a free slot).
    in_central: bool = false,

    const Kind = enum(u8) { header, free, span, interior };
};

/// The first slice of every segment.
const Segment = struct {
    next: ?*Segment,
    /// Bit i is set while slice i may be resident: it has been handed out
    /// since the segment was mapped or since it was last released.
    resident: u64,
    slices: [slices_per_segment]SliceDesc,

    comptime {
        std.debug.assert(@sizeOf(Segment) <= slice_size);
    }

    fn of(addr: usize) *Segment {
        return @ptrFromInt(addr & ~(segment_size - 1));
    }

    fn index(seg: *Segment, desc: *SliceDesc) u8 {
        return @intCast(desc - &seg.slices[0]);
    }

    fn base(seg: *Segment, slice: u8) [*]u8 {
        return @ptrFromInt(@intFromPtr(seg) + @as(usize, slice) * slice_size);
    }

    fn residentMask(slice: u8, order: u8) u64 {
        const count: u7 = @as(u7, 1) << @intCast(order);
        const ones: u64 = if (count == 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(count)) - 1;
        return ones << @intCast(slice);
    }

    fn residentBytes(seg: *Segment, slice: u8, order: u8) usize {
        return @popCount(seg.resident & residentMask(slice, order)) * slice_size;
    }
};

fn spanOf(ptr: [*]u8) *SliceDesc {
    const seg = Segment.of(@intFromPtr(ptr));
    const slice: usize = (@intFromPtr(ptr) - @intFromPtr(seg)) / slice_size;
    std.debug.assert(slice != 0);
    const desc = &seg.slices[seg.slices[slice].head];
    std.debug.assert(desc.kind == .span);
    return desc;
}

/// Spins until the mutex is taken, reading it between attempts so waiting
/// threads do not keep pulling its cache line away from the holder.
fn lock(mutex: *std.atomic.Mutex) void {
    while (!mutex.tryLock()) {
        while (@atomicLoad(std.atomic.Mutex, mutex, .monotonic) == .locked) std.atomic.spinLoopHint();
    }
}

/// Doubly linked list of block descriptors.
const DescList = struct {
    first: ?*SliceDesc = null,
    last: ?*SliceDesc = null,

    fn pushFront(list: *DescList, desc: *SliceDesc) void {
        desc.prev = null;
        desc.next = list.first;
        if (list.first) |f| f.prev = desc else list.last = desc;
        list.first = desc;
    }

    fn pushBack(list: *DescList, desc: *SliceDesc) void {
        desc.next = null;
        desc.prev = list.last;
        if (list.last) |l| l.next = desc else list.first = desc;
        list.last = desc;
    }

    fn remove(list: *DescList, desc: *SliceDesc) void {
        if (desc.prev) |p| p.next = desc.next else list.first = desc.next;
        if (desc.next) |n| n.prev = desc.prev else list.last = desc.prev;
        desc.prev = null;
        desc.next = null;
    }
};

/// Buddy allocator over the slices of every segment.
const PageHeap = struct {
    mutex: std.atomic.Mutex = .unlocked,
    /// Free blocks of each order. Blocks with resident slices are at the
    /// front, so they are reused before released ones.
    free: [max_order + 1]DescList = @splat(.{}),
    segments: ?*Segment = null,
    /// Bytes of free blocks whose slices may be resident.
    free_resident_bytes: usize = 0,
    max_free_resident_bytes: usize = default_max_free_resident_bytes,

    /// Takes a free block of 2^order slices, splitting a larger one when no
    /// block of that order is free, and mapping a segment, without the lock
    /// held, when no block is large enough. The block's descriptor is marked
    /// as a span before the lock is released, so a concurrent `freeBlock` of
    /// its buddy never mistakes it for a free block to merge with.
    fn allocBlock(heap: *PageHeap, order: u8) ?*SliceDesc {
        lock(&heap.mutex);
        defer heap.mutex.unlock();
        var found = order;
        while (true) {
            while (found <= max_order and heap.free[found].first == null) found += 1;
            if (found <= max_order) break;
            heap.mutex.unlock();
            const mem = PageAllocator.map(segment_size, .fromByteUnits(segment_size));
            lock(&heap.mutex);
            heap.addSegment(@ptrCast(@alignCast(mem orelse return null)));
            found = order;
        }
        const desc = heap.free[found].first.?;
        heap.free[found].remove(desc);
        const seg = Segment.of(@intFromPtr(desc));
        const slice = seg.index(desc);
        heap.free_resident_bytes -= seg.residentBytes(slice, found);
        while (found > order) {
            found -= 1;
            heap.insertFree(seg, slice + (@as(u8, 1) << @intCast(found)), found);
        }
        seg.resident |= Segment.residentMask(slice, order);
        desc.kind = .span;
        desc.order = order;
        return desc;
    }

    /// Returns a span's block to the free lists, merging it with free
    /// neighbors. When the resident free total is at its bound, the block's
    /// pages are released first, without the lock held: the block is in no
    /// free list and still marked as a span, so no other thread touches it.
    fn freeBlock(heap: *PageHeap, desc: *SliceDesc) void {
        const seg = Segment.of(@intFromPtr(desc));
        var slice = seg.index(desc);
        var order = desc.order;
        lock(&heap.mutex);
        defer heap.mutex.unlock();
        const resident = seg.residentBytes(slice, order);
        if (can_release_pages and resident > 0 and heap.free_resident_bytes + resident > heap.max_free_resident_bytes) {
            heap.mutex.unlock();
            const released = releasePages(seg.base(slice), slice_size << @intCast(order));
            lock(&heap.mutex);
            if (released) seg.resident &= ~Segment.residentMask(slice, order);
        }
        while (order < max_order) {
            const buddy_slice = slice ^ (@as(u8, 1) << @intCast(order));
            const buddy = &seg.slices[buddy_slice];
            if (buddy.kind != .free or buddy.order != order) break;
            heap.free[order].remove(buddy);
            heap.free_resident_bytes -= seg.residentBytes(buddy_slice, order);
            buddy.kind = .interior;
            slice = @min(slice, buddy_slice);
            order += 1;
        }
        heap.insertFree(seg, slice, order);
    }

    fn insertFree(heap: *PageHeap, seg: *Segment, slice: u8, order: u8) void {
        const desc = &seg.slices[slice];
        desc.* = .{ .kind = .free, .order = order };
        const resident = seg.residentBytes(slice, order);
        heap.free_resident_bytes += resident;
        if (resident > 0) heap.free[order].pushFront(desc) else heap.free[order].pushBack(desc);
    }

    /// Adds the slices of a newly mapped segment, after its header, to the
    /// free lists.
    fn addSegment(heap: *PageHeap, seg: *Segment) void {
        seg.* = .{ .next = heap.segments, .resident = 1, .slices = @splat(.{}) };
        seg.slices[0].kind = .header;
        heap.segments = seg;
        var order: u8 = 0;
        while (order <= max_order) : (order += 1) {
            heap.insertFree(seg, @as(u8, 1) << @intCast(order), order);
        }
    }

    fn unmapAll(heap: *PageHeap) void {
        var next = heap.segments;
        while (next) |seg| {
            next = seg.next;
            const bytes: [*]align(std.heap.page_size_min) u8 = @ptrCast(@alignCast(seg));
            PageAllocator.unmap(bytes[0..segment_size]);
        }
        heap.* = .{ .max_free_resident_bytes = heap.max_free_resident_bytes };
    }
};

/// Whether this target can give pages back while keeping their addresses
/// mapped and usable. Windows can only decommit pages, which then must be
/// committed again before use, so there freed slices stay resident.
const can_release_pages = builtin.os.tag != .windows;

/// Gives the pages of `len` bytes at `ptr` back to the operating system,
/// keeping the addresses mapped. False when the kernel refused, in which case
/// the pages stay resident and are counted as such.
fn releasePages(ptr: [*]u8, len: usize) bool {
    const aligned: [*]align(std.heap.page_size_min) u8 = @alignCast(ptr);
    std.posix.madvise(aligned, len, std.posix.MADV.DONTNEED) catch return false;
    return true;
}

/// A size class's central store.
const Central = struct {
    _: void align(std.atomic.cache_line) = {},
    mutex: std.atomic.Mutex = .unlocked,
    /// Spans with free slots that are not in `batches`.
    spans: DescList = .{},
    /// A span in `spans` with no used slot, kept so that a class whose last
    /// slots are freed and allocated again in turn does not move a span
    /// through the page heap each time. Any further empty span is freed.
    empty: ?*SliceDesc = null,
    batch_count: u32 = 0,
    /// Chains of exactly one batch of free slots each.
    batches: [max_central_batches]*Node = undefined,
};

/// One size class's cache in a thread slot.
const Bin = struct {
    /// Chain of `count` free slots, at most one batch.
    head: ?*Node = null,
    count: u32 = 0,
    /// Chain of exactly one batch of free slots, or null.
    spare: ?*Node = null,
};

/// One thread slot's caches.
const ThreadSlot = struct {
    _: void align(std.atomic.cache_line) = {},
    mutex: std.atomic.Mutex = .unlocked,
    bins: [class_count]Bin = @splat(.{}),
};

/// Thread slots; a thread uses the slot its `thread_index` names while it
/// can lock it, and moves to the next one when another thread holds it.
const max_thread_slots = 128;
threadlocal var thread_index: u32 = 0;

slots: [max_thread_slots]ThreadSlot = @splat(.{}),
slot_count: std.atomic.Value(u32) = .init(0),
centrals: [class_count]Central = @splat(.{}),
page_heap: PageHeap = .{},

/// An allocator with no memory mapped yet.
pub const init: Self = .{};

/// The `Allocator` interface of this allocator.
pub fn allocator(self: *Self) Allocator {
    return .{ .ptr = self, .vtable = &vtable };
}

/// Unmaps every segment. Every block allocated from `self` must already be
/// freed or abandoned; the thread caches are emptied without being returned.
pub fn deinit(self: *Self) void {
    self.page_heap.unmapAll();
    self.* = .{ .page_heap = .{ .max_free_resident_bytes = self.page_heap.max_free_resident_bytes } };
}

const vtable: Allocator.VTable = .{
    .alloc = alloc,
    .resize = resize,
    .remap = remap,
    .free = free,
};

fn slotCount(self: *Self) u32 {
    const count = self.slot_count.load(.monotonic);
    if (count != 0) return count;
    const n: u32 = @intCast(@max(1, @min(std.Thread.getCpuCount() catch max_thread_slots, max_thread_slots)));
    return if (self.slot_count.cmpxchgStrong(0, n, .monotonic, .monotonic)) |other| other else n;
}

fn lockSlot(self: *Self) *ThreadSlot {
    var index = thread_index;
    {
        const slot = &self.slots[index];
        if (slot.mutex.tryLock()) {
            @branchHint(.likely);
            return slot;
        }
    }
    const count = self.slotCount();
    while (true) {
        index = (index + 1) % count;
        const slot = &self.slots[index];
        if (slot.mutex.tryLock()) {
            thread_index = index;
            return slot;
        }
        std.atomic.spinLoopHint();
    }
}

fn alloc(ctx: *anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8 {
    _ = ret_addr;
    const self: *Self = @ptrCast(@alignCast(ctx));
    const class = classOf(len, alignment) orelse {
        @branchHint(.unlikely);
        return PageAllocator.map(len, alignment);
    };
    const slot = self.lockSlot();
    defer slot.mutex.unlock();
    const bin = &slot.bins[class];
    if (bin.head) |node| {
        @branchHint(.likely);
        bin.head = node.next;
        bin.count -= 1;
        return @ptrCast(node);
    }
    if (bin.spare) |node| {
        bin.head = node.next;
        bin.count = class_batch_slots[class] - 1;
        bin.spare = null;
        return @ptrCast(node);
    }
    const batch = self.fetch(class) orelse return null;
    bin.head = batch.first.next;
    bin.count = batch.count - 1;
    return @ptrCast(batch.first);
}

/// Growth and shrinkage stay in place while the class is unchanged.
fn resize(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool {
    _ = ctx;
    _ = ret_addr;
    const class = classOf(memory.len, alignment);
    const new_class = classOf(new_len, alignment);
    if (class == null and new_class == null) return PageAllocator.realloc(memory, alignment, new_len, false) != null;
    return class == new_class;
}

fn remap(ctx: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
    _ = ctx;
    _ = ret_addr;
    const class = classOf(memory.len, alignment);
    const new_class = classOf(new_len, alignment);
    if (class == null and new_class == null) return PageAllocator.realloc(memory, alignment, new_len, true);
    return if (class == new_class) memory.ptr else null;
}

fn free(ctx: *anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void {
    _ = ret_addr;
    const self: *Self = @ptrCast(@alignCast(ctx));
    const class = classOf(memory.len, alignment) orelse {
        @branchHint(.unlikely);
        return PageAllocator.unmap(@alignCast(memory));
    };
    if (std.debug.runtime_safety) {
        const span = spanOf(memory.ptr);
        const seg = Segment.of(@intFromPtr(span));
        std.debug.assert(span.class == class);
        std.debug.assert((@intFromPtr(memory.ptr) - @intFromPtr(seg.base(seg.index(span)))) % class_sizes[class] == 0);
    }
    const node: *Node = @ptrCast(@alignCast(memory.ptr));
    const slot = self.lockSlot();
    defer slot.mutex.unlock();
    const bin = &slot.bins[class];
    if (bin.count == class_batch_slots[class]) {
        @branchHint(.unlikely);
        if (bin.spare) |spare| self.release(class, spare, bin.count);
        bin.spare = bin.head;
        bin.head = null;
        bin.count = 0;
    }
    node.next = bin.head;
    bin.head = node;
    bin.count += 1;
}

const Batch = struct { first: *Node, count: u32 };

/// Takes a batch of free slots of `class` from its central store: a whole
/// batch from its stack when there is one, and otherwise up to a batch from
/// one span, mapping a new span when none has a free slot. Null only when no
/// slot is free and no memory could be mapped.
///
/// The class lock is not held while the page heap provides a new span, nor
/// while slots never handed out before are linked into the batch, so their
/// first-touch page faults happen outside it. A span's whole free list is
/// taken in constant time when it fits in the batch.
fn fetch(self: *Self, class: Class) ?Batch {
    const central = &self.centrals[class];
    const want = class_batch_slots[class];
    const size = class_sizes[class];
    const capacity = class_capacities[class];
    var first: ?*Node = null;
    var count: u32 = 0;
    var fresh: [*]u8 = undefined;
    var fresh_count: u32 = 0;
    {
        lock(&central.mutex);
        defer central.mutex.unlock();
        if (central.batch_count > 0) {
            central.batch_count -= 1;
            return .{ .first = central.batches[central.batch_count], .count = want };
        }
        const span = central.spans.first orelse blk: {
            central.mutex.unlock();
            const span = self.newSpan(class);
            lock(&central.mutex);
            central.spans.pushFront(span orelse return null);
            span.?.in_central = true;
            break :blk span.?;
        };
        if (central.empty == span) central.empty = null;
        const free_len = span.carved - span.used;
        if (free_len <= want) {
            first = span.free_list;
            span.free_list = null;
            count = free_len;
        } else {
            first = span.free_list;
            var last = span.free_list.?;
            for (1..want) |_| last = last.next.?;
            span.free_list = last.next;
            last.next = null;
            count = want;
        }
        if (count < want and span.carved < capacity) {
            fresh_count = @min(want - count, capacity - span.carved);
            const seg = Segment.of(@intFromPtr(span));
            fresh = seg.base(seg.index(span)) + @as(usize, span.carved) * size;
            span.carved += fresh_count;
            count += fresh_count;
        }
        span.used += count;
        if (span.used == capacity) {
            central.spans.remove(span);
            span.in_central = false;
        }
    }
    var i = fresh_count;
    while (i > 0) {
        i -= 1;
        const node: *Node = @ptrCast(@alignCast(fresh + @as(usize, i) * size));
        node.next = first;
        first = node;
    }
    return .{ .first = first.?, .count = count };
}

/// Gives a chain of `count` free slots of `class` to its central store: a
/// whole batch goes onto its stack while there is room, and otherwise every
/// slot goes back to its span, and spans left with no used slot go back to
/// the page heap.
fn release(self: *Self, class: Class, chain: *Node, count: u32) void {
    const central = &self.centrals[class];
    const emptied = emptied: {
        lock(&central.mutex);
        defer central.mutex.unlock();
        if (count == class_batch_slots[class] and central.batch_count < class_central_batches[class]) {
            central.batches[central.batch_count] = chain;
            central.batch_count += 1;
            return;
        }
        break :emptied releaseToSpans(central, class, chain);
    };
    self.freeSpans(emptied);
}

/// Returns a chain of free slots of `class` to their spans. The class lock is
/// held. Spans left with no used slot, other than the one the class keeps,
/// leave the class and are returned linked through `next`, for `freeSpans`
/// once the lock is released.
fn releaseToSpans(central: *Central, class: Class, chain: *Node) ?*SliceDesc {
    var emptied: ?*SliceDesc = null;
    var next: ?*Node = chain;
    while (next) |node| {
        next = node.next;
        const span = spanOf(@ptrCast(node));
        std.debug.assert(span.class == class and span.used > 0);
        node.next = span.free_list;
        span.free_list = node;
        span.used -= 1;
        if (!span.in_central) {
            central.spans.pushBack(span);
            span.in_central = true;
        }
        if (span.used == 0) {
            if (central.empty == null) {
                central.empty = span;
            } else {
                central.spans.remove(span);
                span.in_central = false;
                span.next = emptied;
                emptied = span;
            }
        }
    }
    return emptied;
}

fn freeSpans(self: *Self, emptied: ?*SliceDesc) void {
    var next = emptied;
    while (next) |span| {
        next = span.next;
        self.page_heap.freeBlock(span);
    }
}

/// Takes a block for a new span of `class` from the page heap. The page heap
/// reads a span's `kind` and `order` under its own lock while merging a
/// neighbor, so those two fields are written only by the page heap, and this
/// sets every other field of the descriptor.
fn newSpan(self: *Self, class: Class) ?*SliceDesc {
    const order = class_span_orders[class];
    const desc = self.page_heap.allocBlock(order) orelse return null;
    const seg = Segment.of(@intFromPtr(desc));
    const slice = seg.index(desc);
    std.debug.assert(desc.kind == .span and desc.order == order);
    desc.prev = null;
    desc.next = null;
    desc.free_list = null;
    desc.used = 0;
    desc.carved = 0;
    desc.class = class;
    desc.head = slice;
    desc.in_central = false;
    for (1..@as(usize, 1) << @intCast(order)) |i| {
        seg.slices[slice + i] = .{ .kind = .interior, .head = slice };
    }
    return desc;
}

/// Returns every free slot held in thread caches and central batch stacks to
/// its span, so spans with no used slot go back to the page heap.
pub fn flushCaches(self: *Self) void {
    for (&self.slots) |*slot| {
        lock(&slot.mutex);
        defer slot.mutex.unlock();
        for (&slot.bins, 0..) |*bin, class| {
            if (bin.head) |chain| self.release(@intCast(class), chain, bin.count);
            if (bin.spare) |chain| self.release(@intCast(class), chain, class_batch_slots[class]);
            bin.* = .{};
        }
    }
    for (&self.centrals, 0..) |*central, class| {
        var emptied: ?*SliceDesc = null;
        {
            lock(&central.mutex);
            defer central.mutex.unlock();
            while (central.batch_count > 0) {
                central.batch_count -= 1;
                const more = releaseToSpans(central, @intCast(class), central.batches[central.batch_count]);
                if (more) |first| {
                    var last = first;
                    while (last.next) |n| last = n;
                    last.next = emptied;
                    emptied = first;
                }
            }
            if (central.empty) |span| {
                central.spans.remove(span);
                span.in_central = false;
                span.next = emptied;
                emptied = span;
                central.empty = null;
            }
        }
        self.freeSpans(emptied);
    }
}

/// Counts that describe the page heap, for tests.
pub const Stats = struct {
    segments: usize,
    free_bytes: usize,
    free_resident_bytes: usize,
};

/// The page heap's counts.
pub fn stats(self: *Self) Stats {
    lock(&self.page_heap.mutex);
    defer self.page_heap.mutex.unlock();
    var segments: usize = 0;
    var next = self.page_heap.segments;
    while (next) |seg| : (next = seg.next) segments += 1;
    var free_bytes: usize = 0;
    for (self.page_heap.free, 0..) |list, order| {
        var desc = list.first;
        while (desc) |d| : (desc = d.next) free_bytes += slice_size << @intCast(order);
    }
    return .{ .segments = segments, .free_bytes = free_bytes, .free_resident_bytes = self.page_heap.free_resident_bytes };
}

// Tests

const testing = std.testing;

fn testInstance() Allocator.Error!*Self {
    const self = try testing.allocator.create(Self);
    self.* = .init;
    return self;
}

fn destroyTestInstance(self: *Self) void {
    self.deinit();
    testing.allocator.destroy(self);
}

test "class table covers every length with bounded rounding" {
    var len: usize = 1;
    while (len <= max_small_len) : (len += 1) {
        const c = classForLen(len);
        try testing.expect(class_sizes[c] >= len);
        if (c > 0) try testing.expect(class_sizes[c - 1] < len);
        if (len > 128) try testing.expect(class_sizes[c] - len < len / 4 + 1);
    }
    for (class_sizes, 0..) |size, c| {
        try testing.expectEqual(@as(Class, @intCast(c)), classForLen(size));
        const span_bytes = slice_size << @intCast(class_span_orders[c]);
        try testing.expect(span_bytes - class_capacities[c] * size <= span_bytes / 16);
        try testing.expect(class_capacities[c] >= min_span_slots);
    }
}

test "every class boundary allocates, writes and frees" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    for (class_sizes) |size| {
        for ([_]usize{ size - 1, size, size + 1 }) |len| {
            if (len == 0 or len > max_small_len + 1) continue;
            const block = try a.alloc(u8, len);
            @memset(block, 0xab);
            try testing.expect(std.mem.allEqual(u8, block, 0xab));
            a.free(block);
        }
    }
}

test "alignment is honored for every class and supported alignment" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    inline for (.{ 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 65536 }) |align_bytes| {
        const alignment = comptime Alignment.fromByteUnits(align_bytes);
        for ([_]usize{ 1, 7, 24, 100, 129, 1000, 3000, 4096, 5000, 20000, max_small_len, max_small_len + 1 }) |len| {
            const ptr = a.rawAlloc(len, alignment, @returnAddress()) orelse return error.OutOfMemory;
            try testing.expect(std.mem.isAligned(@intFromPtr(ptr), align_bytes));
            @memset(ptr[0..len], 1);
            a.rawFree(ptr[0..len], alignment, @returnAddress());
        }
    }
}

test "resize and remap stay in place within a class and refuse class changes" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    var block = try a.alloc(u8, 130);
    try testing.expect(a.resize(block, 160));
    block.len = 160;
    try testing.expect(!a.resize(block, 161));
    try testing.expect(!a.resize(block, 128));
    try testing.expect(a.remap(block, 140) != null);
    try testing.expectEqual(@as(?[*]u8, null), a.rawRemap(block, .@"1", 1000, @returnAddress()));
    block = try a.realloc(block, 5000);
    @memset(block, 3);
    block = try a.realloc(block, 100_000);
    try testing.expectEqual(@as(u8, 3), block[4999]);
    block = try a.realloc(block, 200_000);
    try testing.expectEqual(@as(u8, 3), block[4999]);
    block = try a.realloc(block, 10);
    try testing.expectEqual(@as(u8, 3), block[9]);
    a.free(block);
}

test "freed slots are reused by the same class" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    const first = try a.alloc(u8, 200);
    const first_ptr = first.ptr;
    a.free(first);
    const second = try a.alloc(u8, 220);
    defer a.free(second);
    try testing.expectEqual(first_ptr, second.ptr);
}

test "empty spans return to the page heap and serve other classes" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    var blocks: [4096][]u8 = undefined;
    for (&blocks) |*b| b.* = try a.alloc(u8, 48);
    const after_small = self.stats();
    for (blocks) |b| a.free(b);
    self.flushCaches();
    const after_free = self.stats();
    try testing.expectEqual(after_small.segments, after_free.segments);
    try testing.expectEqual(segment_size - slice_size, after_free.free_bytes);
    // The same slices now hold a class of another size, so no segment is mapped.
    for (blocks[0..64]) |*b| b.* = try a.alloc(u8, 3000);
    try testing.expectEqual(after_small.segments, self.stats().segments);
    for (blocks[0..64]) |b| a.free(b);
}

test "freed blocks past the resident bound are released" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    self.page_heap.max_free_resident_bytes = slice_size;
    const a = self.allocator();
    var blocks: [256][]u8 = undefined;
    for (&blocks) |*b| b.* = try a.alloc(u8, 32 * 1024);
    for (blocks) |b| a.free(b);
    self.flushCaches();
    const s = self.stats();
    try testing.expect(s.free_resident_bytes <= slice_size);
    try testing.expectEqual(s.segments * (segment_size - slice_size), s.free_bytes);
    // Released slices are usable again.
    for (&blocks) |*b| {
        b.* = try a.alloc(u8, 32 * 1024);
        @memset(b.*, 9);
    }
    for (blocks) |b| a.free(b);
}

test "slots freed on other threads are reused by any thread" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    const count = 20_000;
    const blocks = try testing.allocator.alloc([]u8, count);
    defer testing.allocator.free(blocks);
    for (blocks) |*b| b.* = try a.alloc(u8, 256);
    const segments = self.stats().segments;
    const Freer = struct {
        fn run(al: Allocator, list: [][]u8) void {
            for (list) |b| al.free(b);
        }
    };
    var threads: [4]std.Thread = undefined;
    const per = count / threads.len;
    for (&threads, 0..) |*t, i| t.* = try std.Thread.spawn(.{}, Freer.run, .{ a, blocks[i * per .. (i + 1) * per] });
    for (threads) |t| t.join();
    // This thread allocates them all again without mapping more memory.
    for (blocks) |*b| b.* = try a.alloc(u8, 256);
    try testing.expectEqual(segments, self.stats().segments);
    for (blocks) |b| a.free(b);
}

test "randomized alloc, free and realloc across threads match a reference" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    const a = self.allocator();
    const Shared = struct {
        mutex: std.atomic.Mutex = .unlocked,
        handoff: ?[]u8 = null,
        failed: std.atomic.Value(bool) = .init(false),
    };
    const Worker = struct {
        fn run(al: Allocator, seed: u64, shared: *Shared) void {
            var prng = std.Random.DefaultPrng.init(seed);
            const random = prng.random();
            var live: [64]?[]u8 = @splat(null);
            for (0..20_000) |_| {
                const i = random.uintLessThan(usize, live.len);
                if (live[i]) |block| {
                    if (!std.mem.allEqual(u8, block, fill(block))) shared.failed.store(true, .monotonic);
                    switch (random.uintLessThan(u8, 3)) {
                        0 => {
                            al.free(block);
                            live[i] = null;
                        },
                        1 => {
                            const new_len = randomLen(random);
                            const grown = al.realloc(block, new_len) catch {
                                shared.failed.store(true, .monotonic);
                                return;
                            };
                            const kept = @min(block.len, new_len);
                            if (!std.mem.allEqual(u8, grown[0..kept], fill(block))) shared.failed.store(true, .monotonic);
                            @memset(grown, fill(grown));
                            live[i] = grown;
                        },
                        else => {
                            // Hand the block to another thread to free.
                            lock(&shared.mutex);
                            defer shared.mutex.unlock();
                            if (shared.handoff) |other| al.free(other);
                            shared.handoff = block;
                            live[i] = null;
                        },
                    }
                } else {
                    const block = al.alloc(u8, randomLen(random)) catch {
                        shared.failed.store(true, .monotonic);
                        return;
                    };
                    @memset(block, fill(block));
                    live[i] = block;
                }
            }
            for (live) |maybe| if (maybe) |block| al.free(block);
        }

        fn fill(block: []const u8) u8 {
            return @truncate(block.len *% 31 +% 7);
        }

        fn randomLen(random: std.Random) usize {
            return switch (random.uintLessThan(u8, 8)) {
                0 => random.intRangeAtMost(usize, 1, 16),
                1, 2, 3 => random.intRangeAtMost(usize, 1, 512),
                4, 5 => random.intRangeAtMost(usize, 1, 8192),
                6 => random.intRangeAtMost(usize, 1, max_small_len),
                else => random.intRangeAtMost(usize, 1, 3 * max_small_len),
            };
        }
    };
    var shared: Shared = .{};
    var threads: [8]std.Thread = undefined;
    for (&threads, 0..) |*t, i| t.* = try std.Thread.spawn(.{}, Worker.run, .{ a, i + 1, &shared });
    for (threads) |t| t.join();
    if (shared.handoff) |block| a.free(block);
    try testing.expect(!shared.failed.load(.monotonic));
    // Everything was freed, so after the caches drain every span is back in the page heap.
    self.flushCaches();
    const s = self.stats();
    try testing.expectEqual(s.segments * (segment_size - slice_size), s.free_bytes);
}

test "spans of every order churn through the page heap from many threads at once" {
    const self = try testInstance();
    defer destroyTestInstance(self);
    // Freed spans past one slice's worth are released, so the page heap also
    // releases pages while other threads split and merge blocks.
    self.page_heap.max_free_resident_bytes = slice_size;
    const a = self.allocator();
    const Worker = struct {
        /// Classes whose spans have different orders, so neighboring blocks of
        /// several orders are taken and freed concurrently.
        const lens = [_]usize{ 3000, 10_000, 20_000, max_small_len };

        fn run(al: Allocator, seed: u64, failed: *std.atomic.Value(bool)) void {
            var prng = std.Random.DefaultPrng.init(seed);
            const random = prng.random();
            var blocks: [96][]u8 = undefined;
            for (0..60) |_| {
                for (&blocks) |*b| {
                    const len = lens[random.uintLessThan(usize, lens.len)];
                    b.* = al.alloc(u8, len) catch {
                        failed.store(true, .monotonic);
                        return;
                    };
                    stamp(b.*);
                }
                for (blocks) |b| {
                    if (!stamped(b)) failed.store(true, .monotonic);
                    al.free(b);
                }
            }
        }

        /// Marks both ends of a block with its address, so a block handed to
        /// two owners at once fails the check of whichever stamped it first.
        fn stamp(block: []u8) void {
            const addr = @intFromPtr(block.ptr);
            std.mem.writeInt(usize, block[0..@sizeOf(usize)], addr, .little);
            std.mem.writeInt(usize, block[block.len - @sizeOf(usize) ..][0..@sizeOf(usize)], ~addr, .little);
        }

        fn stamped(block: []u8) bool {
            const addr = @intFromPtr(block.ptr);
            return std.mem.readInt(usize, block[0..@sizeOf(usize)], .little) == addr and
                std.mem.readInt(usize, block[block.len - @sizeOf(usize) ..][0..@sizeOf(usize)], .little) == ~addr;
        }
    };
    var failed: std.atomic.Value(bool) = .init(false);
    var threads: [8]std.Thread = undefined;
    for (&threads, 0..) |*t, i| t.* = try std.Thread.spawn(.{}, Worker.run, .{ a, i + 1, &failed });
    for (threads) |t| t.join();
    try testing.expect(!failed.load(.monotonic));
    // Once the caches drain, every block has merged back with its buddies.
    self.flushCaches();
    const s = self.stats();
    try testing.expectEqual(s.segments * (segment_size - slice_size), s.free_bytes);
    try testing.expect(s.free_resident_bytes <= slice_size);
}
