//! Shared-memory control block for machine-code hot loading.

const std = @import("std");
const SharedMemoryAllocator = @import("SharedMemoryAllocator.zig");

/// Magic number identifying a hot-load control block ("ROCH" little-endian).
pub const MAGIC: u32 = 0x4843_4F52;

/// Version of the control block layout stored in shared-memory header padding.
pub const FORMAT_VERSION: u32 = 1;

/// Number of shared-memory image slots tracked by the hot-reload control block.
pub const max_image_slots = 8;
/// Sentinel used before any image slot has been published.
pub const invalid_image_slot: u32 = std.math.maxInt(u32);

/// Host-shim acknowledgement status for a published hot-load generation.
pub const Status = enum(u32) {
    none = 0,
    accepted = 1,
    rejected = 2,
};

/// Lifecycle state for one shared-memory image slot.
pub const SlotState = enum(u32) {
    free = 0,
    writing = 1,
    published = 2,
    retired = 3,
};

/// Metadata for one shared-memory image region and its active shim readers.
pub const ImageSlot = extern struct {
    state: u32 = @intFromEnum(SlotState.free),
    readers: u32 = 0,
    generation: u64 = 0,
    image_offset: u64 = 0,
    image_size: u64 = 0,
    slot_end: u64 = 0,
};

/// Shared-memory control block used to publish replacement executable images.
pub const Control = extern struct {
    magic: u32 = MAGIC,
    format_version: u32 = FORMAT_VERSION,
    publish_sequence: u64 = 0,
    published_generation: u64 = 0,
    image_offset: u64 = 0,
    image_size: u64 = 0,
    published_slot_index: u32 = invalid_image_slot,
    _publish_padding: u32 = 0,
    ack_sequence: u64 = 0,
    acknowledged_generation: u64 = 0,
    status: u32 = @intFromEnum(Status.none),
    _ack_padding: u32 = 0,
    image_slots: [max_image_slots]ImageSlot = [_]ImageSlot{.{}} ** max_image_slots,
};

comptime {
    if (@sizeOf(Control) > @sizeOf(@FieldType(SharedMemoryAllocator.Header, "reserved"))) {
        @compileError("hot reload control block must fit in SharedMemoryAllocator.Header.reserved");
    }
}

/// Reinterpret a mutable shared-memory header as its embedded control block.
pub fn controlFromHeader(header: *SharedMemoryAllocator.Header) *Control {
    return @ptrCast(@alignCast(&header.reserved));
}

/// Reinterpret a const shared-memory header as its embedded control block.
pub fn controlFromConstHeader(header: *const SharedMemoryAllocator.Header) *const Control {
    return @ptrCast(@alignCast(&header.reserved));
}

/// Locate the mutable control block from the base pointer of a shared mapping.
pub fn controlFromBase(base_ptr: [*]align(1) u8) *Control {
    const header: *SharedMemoryAllocator.Header = @ptrCast(@alignCast(base_ptr));
    return controlFromHeader(header);
}

/// Locate the const control block from the base pointer of a shared mapping.
pub fn controlFromConstBase(base_ptr: [*]align(1) const u8) *const Control {
    const header: *const SharedMemoryAllocator.Header = @ptrCast(@alignCast(base_ptr));
    return controlFromConstHeader(header);
}

/// Return whether the control block has the expected magic and format version.
pub fn initialized(control: *const Control) bool {
    return control.magic == MAGIC and control.format_version == FORMAT_VERSION;
}

/// Initialize the control block with generation 1 pointing at the initial image.
pub fn init(control: *Control, image_offset: usize, image_size: usize) void {
    control.* = .{};
    setSlotWriting(control, 0, 1, image_offset, image_size, image_size);
    publishSlot(control, 1, 0, image_offset, image_size, image_size);
}

/// 64-bit atomic load that also compiles on targets without lock-free 64-bit atomics.
/// Hot reload only runs on 64-bit hosts: the dev backend is unavailable on 32-bit, so
/// `HostLirCodeGen` is `void` there and this control block is never driven concurrently.
/// The 32-bit branch is therefore never executed; a plain load keeps the code compiling.
inline fn loadU64(ptr: *const u64, comptime order: std.builtin.AtomicOrder) u64 {
    if (comptime @sizeOf(usize) >= 8) return @atomicLoad(u64, ptr, order);
    return ptr.*;
}

/// 64-bit atomic store companion to `loadU64`; see its doc comment for the 32-bit rationale.
inline fn storeU64(ptr: *u64, value: u64, comptime order: std.builtin.AtomicOrder) void {
    if (comptime @sizeOf(usize) >= 8) {
        @atomicStore(u64, ptr, value, order);
    } else {
        ptr.* = value;
    }
}

inline fn loadU32(ptr: *const u32, comptime order: std.builtin.AtomicOrder) u32 {
    return @atomicLoad(u32, ptr, order);
}

inline fn storeU32(ptr: *u32, value: u32, comptime order: std.builtin.AtomicOrder) void {
    @atomicStore(u32, ptr, value, order);
}

/// Publish a replacement image for the host shim to load at the next safe point.
pub fn publish(control: *Control, generation: u64, image_offset: usize, image_size: usize) void {
    setSlotWriting(control, 0, generation, image_offset, image_size, image_size);
    publishSlot(control, generation, 0, image_offset, image_size, image_size);
}

/// Return whether an index can address the fixed slot table.
pub fn validSlotIndex(slot_index: u32) bool {
    return slot_index < max_image_slots;
}

/// Mark a slot as being written by the compiler before it is published.
pub fn setSlotWriting(
    control: *Control,
    slot_index: u32,
    generation: u64,
    image_offset: usize,
    image_size: usize,
    slot_end: usize,
) void {
    if (builtinModeDebug() and !validSlotIndex(slot_index)) {
        std.debug.panic("hot reload invariant violated: invalid image slot {d}", .{slot_index});
    }
    if (!validSlotIndex(slot_index)) return;

    const slot = &control.image_slots[slot_index];
    storeU32(&slot.state, @intFromEnum(SlotState.writing), .release);
    storeU64(&slot.generation, generation, .release);
    storeU64(&slot.image_offset, @intCast(image_offset), .release);
    storeU64(&slot.image_size, @intCast(image_size), .release);
    storeU64(&slot.slot_end, @intCast(slot_end), .release);
}

/// Publish a replacement image from a specific shared-memory slot.
pub fn publishSlot(
    control: *Control,
    generation: u64,
    slot_index: u32,
    image_offset: usize,
    image_size: usize,
    slot_end: usize,
) void {
    if (builtinModeDebug() and !validSlotIndex(slot_index)) {
        std.debug.panic("hot reload invariant violated: invalid published image slot {d}", .{slot_index});
    }
    if (!validSlotIndex(slot_index)) return;

    const slot = &control.image_slots[slot_index];
    storeU64(&slot.generation, generation, .release);
    storeU64(&slot.image_offset, @intCast(image_offset), .release);
    storeU64(&slot.image_size, @intCast(image_size), .release);
    storeU64(&slot.slot_end, @intCast(slot_end), .release);
    storeU32(&slot.state, @intFromEnum(SlotState.published), .release);

    const start = nextOddSequence(loadU64(&control.publish_sequence, .acquire));
    storeU64(&control.publish_sequence, start, .release);
    storeU64(&control.image_offset, @intCast(image_offset), .release);
    storeU64(&control.image_size, @intCast(image_size), .release);
    storeU64(&control.published_generation, generation, .release);
    storeU32(&control.published_slot_index, slot_index, .release);
    storeU64(&control.publish_sequence, start +% 1, .release);
}

fn builtinModeDebug() bool {
    return @import("builtin").mode == .Debug;
}

fn nextOddSequence(sequence: u64) u64 {
    return (sequence +% 1) | 1;
}

fn stableSequence(sequence: u64) bool {
    return sequence & 1 == 0;
}

/// Coherent snapshot of a published executable image.
pub const PublishedImage = struct {
    generation: u64,
    image_offset: usize,
    image_size: usize,
    slot_index: u32,
};

/// Return a coherent snapshot of the latest published image, if one is stable.
pub fn publishedImage(control: *const Control) ?PublishedImage {
    if (!initialized(control)) return null;

    for (0..16) |_| {
        const start = loadU64(&control.publish_sequence, .acquire);
        if (!stableSequence(start)) continue;

        const generation = loadU64(&control.published_generation, .acquire);
        const image_offset = loadU64(&control.image_offset, .acquire);
        const image_size = loadU64(&control.image_size, .acquire);
        const slot_index = loadU32(&control.published_slot_index, .acquire);

        const end = loadU64(&control.publish_sequence, .acquire);
        if (start == end and stableSequence(end)) {
            return .{
                .generation = generation,
                .image_offset = @intCast(image_offset),
                .image_size = @intCast(image_size),
                .slot_index = slot_index,
            };
        }
    }

    return null;
}

/// Acquire-load the latest generation published by the compiler parent.
pub fn publishedGeneration(control: *const Control) u64 {
    return if (publishedImage(control)) |image| image.generation else 0;
}

/// Return the shared-memory byte offset of the currently published image.
pub fn imageOffset(control: *const Control) usize {
    return @intCast(loadU64(&control.image_offset, .acquire));
}

/// Return the byte length of the currently published image.
pub fn imageSize(control: *const Control) usize {
    return @intCast(loadU64(&control.image_size, .acquire));
}

/// Coherent snapshot of one image slot's reclamation metadata.
pub const ImageSlotSnapshot = struct {
    state: SlotState,
    readers: u32,
    generation: u64,
    image_offset: usize,
    image_size: usize,
    slot_end: usize,
};

/// Return the current state of a shared-memory image slot.
pub fn slotSnapshot(control: *const Control, slot_index: u32) ?ImageSlotSnapshot {
    if (!validSlotIndex(slot_index)) return null;
    const slot = &control.image_slots[slot_index];
    return .{
        .state = slotStateFromRaw(loadU32(&slot.state, .acquire)),
        .readers = loadU32(&slot.readers, .acquire),
        .generation = loadU64(&slot.generation, .acquire),
        .image_offset = @intCast(loadU64(&slot.image_offset, .acquire)),
        .image_size = @intCast(loadU64(&slot.image_size, .acquire)),
        .slot_end = @intCast(loadU64(&slot.slot_end, .acquire)),
    };
}

/// Mark a slot as no longer published but not yet known reusable.
pub fn markSlotRetired(control: *Control, slot_index: u32) void {
    if (!validSlotIndex(slot_index)) return;
    storeU32(&control.image_slots[slot_index].state, @intFromEnum(SlotState.retired), .release);
}

/// Mark a slot reusable by a future compiler rebuild.
pub fn markSlotFree(control: *Control, slot_index: u32) void {
    if (!validSlotIndex(slot_index)) return;
    storeU32(&control.image_slots[slot_index].state, @intFromEnum(SlotState.free), .release);
}

/// Lease the latest published image while the shim copies it into executable memory.
pub fn acquirePublishedImage(control: *Control) ?PublishedImage {
    for (0..16) |_| {
        const image = publishedImage(control) orelse return null;
        if (!validSlotIndex(image.slot_index)) return null;

        const slot = &control.image_slots[image.slot_index];
        const previous_readers = @atomicRmw(u32, &slot.readers, .Add, 1, .acquire);
        if (builtinModeDebug() and previous_readers == std.math.maxInt(u32)) {
            std.debug.panic("hot reload invariant violated: image slot reader count overflowed", .{});
        }

        const slot_state = slotStateFromRaw(loadU32(&slot.state, .acquire));
        const slot_generation = loadU64(&slot.generation, .acquire);
        const slot_offset = loadU64(&slot.image_offset, .acquire);
        const slot_size = loadU64(&slot.image_size, .acquire);
        const current = publishedImage(control);

        if (slot_state == .published and
            slot_generation == image.generation and
            slot_offset == image.image_offset and
            slot_size == image.image_size and
            current != null and
            current.?.generation == image.generation and
            current.?.slot_index == image.slot_index and
            current.?.image_offset == image.image_offset and
            current.?.image_size == image.image_size)
        {
            return image;
        }

        releaseImage(control, image.slot_index);
    }

    return null;
}

/// Release a lease returned by `acquirePublishedImage`.
pub fn releaseImage(control: *Control, slot_index: u32) void {
    if (!validSlotIndex(slot_index)) return;
    const previous = @atomicRmw(u32, &control.image_slots[slot_index].readers, .Sub, 1, .release);
    if (builtinModeDebug() and previous == 0) {
        std.debug.panic("hot reload invariant violated: released an unreferenced image slot", .{});
    }
}

/// Publish the host shim's acknowledgement for a generation.
pub fn acknowledge(control: *Control, generation: u64, status: Status) void {
    const start = nextOddSequence(loadU64(&control.ack_sequence, .acquire));
    storeU64(&control.ack_sequence, start, .release);
    @atomicStore(u32, &control.status, @intFromEnum(status), .release);
    storeU64(&control.acknowledged_generation, generation, .release);
    storeU64(&control.ack_sequence, start +% 1, .release);
}

/// Coherent snapshot of the host shim acknowledgement.
pub const Acknowledgement = struct {
    generation: u64,
    status: Status,
};

/// Return a coherent acknowledgement snapshot, if one is stable.
pub fn acknowledgement(control: *const Control) ?Acknowledgement {
    if (!initialized(control)) return null;

    for (0..16) |_| {
        const start = loadU64(&control.ack_sequence, .acquire);
        if (!stableSequence(start)) continue;

        const generation = loadU64(&control.acknowledged_generation, .acquire);
        const status_raw = @atomicLoad(u32, &control.status, .acquire);

        const end = loadU64(&control.ack_sequence, .acquire);
        if (start == end and stableSequence(end)) {
            return .{
                .generation = generation,
                .status = statusFromRaw(status_raw),
            };
        }
    }

    return null;
}

fn statusFromRaw(raw: u32) Status {
    return switch (raw) {
        @intFromEnum(Status.accepted) => .accepted,
        @intFromEnum(Status.rejected) => .rejected,
        else => .none,
    };
}

fn slotStateFromRaw(raw: u32) SlotState {
    return switch (raw) {
        @intFromEnum(SlotState.writing) => .writing,
        @intFromEnum(SlotState.published) => .published,
        @intFromEnum(SlotState.retired) => .retired,
        else => .free,
    };
}

/// Acquire-load the latest generation acknowledged by the host shim.
pub fn acknowledgedGeneration(control: *const Control) u64 {
    return if (acknowledgement(control)) |ack| ack.generation else 0;
}

/// Return the acknowledgement status associated with the latest acknowledgement.
pub fn acknowledgedStatus(control: *const Control) Status {
    return if (acknowledgement(control)) |ack| ack.status else .none;
}

test "hot reload control publishes and acknowledges generations" {
    var control = std.mem.zeroes(Control);

    try std.testing.expect(!initialized(&control));

    init(&control, 512, 4096);
    try std.testing.expect(initialized(&control));
    try std.testing.expectEqual(@as(u64, 1), publishedGeneration(&control));
    try std.testing.expectEqual(@as(usize, 512), imageOffset(&control));
    try std.testing.expectEqual(@as(usize, 4096), imageSize(&control));
    try std.testing.expectEqual(@as(u32, 0), publishedImage(&control).?.slot_index);
    try std.testing.expectEqual(Status.none, acknowledgedStatus(&control));

    publish(&control, 2, 8192, 2048);
    try std.testing.expectEqual(@as(u64, 2), publishedGeneration(&control));
    try std.testing.expectEqual(@as(usize, 8192), imageOffset(&control));
    try std.testing.expectEqual(@as(usize, 2048), imageSize(&control));

    acknowledge(&control, 2, .accepted);
    try std.testing.expectEqual(@as(u64, 2), acknowledgedGeneration(&control));
    try std.testing.expectEqual(Status.accepted, acknowledgedStatus(&control));
}

test "hot reload publication snapshots reject in-progress writes" {
    var control = std.mem.zeroes(Control);
    init(&control, 512, 4096);

    storeU64(&control.publish_sequence, 3, .release);
    storeU64(&control.image_offset, 8192, .release);
    storeU64(&control.image_size, 16384, .release);
    storeU64(&control.published_generation, 2, .release);
    try std.testing.expect(publishedImage(&control) == null);

    storeU64(&control.publish_sequence, 4, .release);
    const image = publishedImage(&control).?;
    try std.testing.expectEqual(@as(u64, 2), image.generation);
    try std.testing.expectEqual(@as(usize, 8192), image.image_offset);
    try std.testing.expectEqual(@as(usize, 16384), image.image_size);
    try std.testing.expectEqual(@as(u32, 0), image.slot_index);
}

test "hot reload acknowledgement snapshots reject in-progress writes" {
    var control = std.mem.zeroes(Control);
    init(&control, 512, 4096);

    storeU64(&control.ack_sequence, 5, .release);
    @atomicStore(u32, &control.status, @intFromEnum(Status.accepted), .release);
    storeU64(&control.acknowledged_generation, 2, .release);
    try std.testing.expect(acknowledgement(&control) == null);

    storeU64(&control.ack_sequence, 6, .release);
    const ack = acknowledgement(&control).?;
    try std.testing.expectEqual(@as(u64, 2), ack.generation);
    try std.testing.expectEqual(Status.accepted, ack.status);
}

test "hot reload control block fits in shared memory reserved header bytes" {
    try std.testing.expect(@sizeOf(Control) <= @sizeOf(@FieldType(SharedMemoryAllocator.Header, "reserved")));
}

test "hot reload image slots can be leased and released" {
    var control = std.mem.zeroes(Control);
    init(&control, 512, 4096);

    const leased = acquirePublishedImage(&control).?;
    try std.testing.expectEqual(@as(u64, 1), leased.generation);
    try std.testing.expectEqual(@as(u32, 0), leased.slot_index);
    try std.testing.expectEqual(@as(u32, 1), slotSnapshot(&control, 0).?.readers);

    releaseImage(&control, leased.slot_index);
    try std.testing.expectEqual(@as(u32, 0), slotSnapshot(&control, 0).?.readers);
}

test "hot reload image lease rejects slots that stopped being current" {
    var control = std.mem.zeroes(Control);
    init(&control, 512, 4096);

    setSlotWriting(&control, 1, 2, 4096, 8192, 8192);
    publishSlot(&control, 2, 1, 4096, 8192, 8192);
    markSlotRetired(&control, 0);

    const leased = acquirePublishedImage(&control).?;
    try std.testing.expectEqual(@as(u64, 2), leased.generation);
    try std.testing.expectEqual(@as(u32, 1), leased.slot_index);
    try std.testing.expectEqual(@as(u32, 0), slotSnapshot(&control, 0).?.readers);
    try std.testing.expectEqual(@as(u32, 1), slotSnapshot(&control, 1).?.readers);
    releaseImage(&control, leased.slot_index);
}
