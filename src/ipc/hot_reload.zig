//! Shared-memory control block for machine-code hot loading.

const std = @import("std");
const SharedMemoryAllocator = @import("SharedMemoryAllocator.zig");

/// Magic number identifying a hot-load control block ("ROCH" little-endian).
pub const MAGIC: u32 = 0x4843_4F52;
/// Magic number identifying a hot-load image descriptor ("ROCD" little-endian).
pub const DESCRIPTOR_MAGIC: u32 = 0x4443_4F52;

/// Version of the control block layout stored in shared-memory header padding.
pub const FORMAT_VERSION: u32 = 2;

/// Sentinel used before any image descriptor has been published. Offset zero is
/// the shared-memory header, so a valid image descriptor is never stored there.
pub const invalid_descriptor_offset: usize = 0;

/// Host-shim acknowledgement status for a published hot-load generation.
pub const Status = enum(u32) {
    none = 0,
    accepted = 1,
    rejected = 2,
};

/// Lifecycle state for one shared-memory image descriptor.
pub const DescriptorState = enum(u32) {
    writing = 1,
    published = 2,
    retired = 3,
    reclaimed = 4,
};

/// Metadata for one shared-memory image allocation and its active shim refs.
///
/// This lives in the shared-memory allocation region, not in the fixed control
/// block. The compiler parent owns byte reclamation; the shim only updates refs.
pub const ImageDescriptor = extern struct {
    magic: u32 = DESCRIPTOR_MAGIC,
    state: u32 = @intFromEnum(DescriptorState.reclaimed),
    refs: u32 = 0,
    _padding: u32 = 0,
    generation: u64 = 0,
    image_offset: u64 = 0,
    image_size: u64 = 0,
    allocation_start: u64 = 0,
    allocation_end: u64 = 0,
};

/// Shared-memory control block used to publish replacement executable images.
pub const Control = extern struct {
    magic: u32 = MAGIC,
    format_version: u32 = FORMAT_VERSION,
    publish_sequence: u64 = 0,
    published_generation: u64 = 0,
    published_descriptor_offset: u64 = invalid_descriptor_offset,
    image_offset: u64 = 0,
    image_size: u64 = 0,
    ack_sequence: u64 = 0,
    acknowledged_generation: u64 = 0,
    status: u32 = @intFromEnum(Status.none),
    _ack_padding: u32 = 0,
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
pub fn init(control: *Control, descriptor_offset: usize, descriptor: *ImageDescriptor) void {
    control.* = .{};
    publishDescriptor(control, 1, descriptor_offset, descriptor);
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

fn builtinModeDebug() bool {
    return @import("builtin").mode == .Debug;
}

fn nextOddSequence(sequence: u64) u64 {
    return (sequence +% 1) | 1;
}

fn stableSequence(sequence: u64) bool {
    return sequence & 1 == 0;
}

/// Prepare a descriptor before its image is published.
///
/// `reset_refs` must be false when reusing memory that previously contained a
/// published descriptor. A stale host can transiently retain that descriptor
/// after the compiler has selected it for reuse; preserving the count keeps that
/// stale retain/release balanced while validation rejects the old generation.
pub fn prepareDescriptor(
    descriptor: *ImageDescriptor,
    generation: u64,
    image_offset: usize,
    image_size: usize,
    allocation_start: usize,
    allocation_end: usize,
    reset_refs: bool,
) void {
    descriptor.magic = DESCRIPTOR_MAGIC;
    storeU32(&descriptor.state, @intFromEnum(DescriptorState.writing), .release);
    if (reset_refs) storeU32(&descriptor.refs, 0, .release);
    descriptor._padding = 0;
    storeU64(&descriptor.generation, generation, .release);
    storeU64(&descriptor.image_offset, @intCast(image_offset), .release);
    storeU64(&descriptor.image_size, @intCast(image_size), .release);
    storeU64(&descriptor.allocation_start, @intCast(allocation_start), .release);
    storeU64(&descriptor.allocation_end, @intCast(allocation_end), .release);
}

/// Publish a prepared descriptor for the host shim to load at the next safe point.
pub fn publishDescriptor(control: *Control, generation: u64, descriptor_offset: usize, descriptor: *ImageDescriptor) void {
    if (builtinModeDebug()) {
        if (descriptor_offset == invalid_descriptor_offset) {
            std.debug.panic("hot reload invariant violated: invalid descriptor offset", .{});
        }
        if (descriptor.magic != DESCRIPTOR_MAGIC) {
            std.debug.panic("hot reload invariant violated: published descriptor missing magic", .{});
        }
    }

    const image_offset = loadU64(&descriptor.image_offset, .acquire);
    const image_size = loadU64(&descriptor.image_size, .acquire);
    storeU64(&descriptor.generation, generation, .release);
    storeU32(&descriptor.state, @intFromEnum(DescriptorState.published), .release);

    const start = nextOddSequence(loadU64(&control.publish_sequence, .acquire));
    storeU64(&control.publish_sequence, start, .release);
    storeU64(&control.published_descriptor_offset, @intCast(descriptor_offset), .release);
    storeU64(&control.image_offset, image_offset, .release);
    storeU64(&control.image_size, image_size, .release);
    storeU64(&control.published_generation, generation, .release);
    storeU64(&control.publish_sequence, start +% 1, .release);
}

/// Coherent snapshot of a published executable image.
pub const PublishedImage = struct {
    generation: u64,
    descriptor_offset: usize,
    image_offset: usize,
    image_size: usize,
};

/// Coherent snapshot of a published image with a retained descriptor pointer.
pub const RetainedImage = struct {
    generation: u64,
    descriptor_offset: usize,
    descriptor: *ImageDescriptor,
    image_offset: usize,
    image_size: usize,
};

/// Return a coherent snapshot of the latest published image, if one is stable.
pub fn publishedImage(control: *const Control) ?PublishedImage {
    if (!initialized(control)) return null;

    for (0..16) |_| {
        const start = loadU64(&control.publish_sequence, .acquire);
        if (!stableSequence(start)) continue;

        const generation = loadU64(&control.published_generation, .acquire);
        const descriptor_offset = loadU64(&control.published_descriptor_offset, .acquire);
        const image_offset = loadU64(&control.image_offset, .acquire);
        const image_size = loadU64(&control.image_size, .acquire);

        const end = loadU64(&control.publish_sequence, .acquire);
        if (start == end and stableSequence(end)) {
            if (descriptor_offset == invalid_descriptor_offset) return null;
            return .{
                .generation = generation,
                .descriptor_offset = @intCast(descriptor_offset),
                .image_offset = @intCast(image_offset),
                .image_size = @intCast(image_size),
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

fn validDescriptorRange(total_size: usize, descriptor_offset: usize) bool {
    if (descriptor_offset == invalid_descriptor_offset) return false;
    if (descriptor_offset > total_size) return false;
    if (total_size - descriptor_offset < @sizeOf(ImageDescriptor)) return false;
    return true;
}

/// Locate an image descriptor by shared-memory byte offset.
pub fn descriptorFromOffset(base_ptr: [*]align(1) u8, total_size: usize, descriptor_offset: usize) ?*ImageDescriptor {
    if (!validDescriptorRange(total_size, descriptor_offset)) return null;
    const ptr = base_ptr + descriptor_offset;
    if (@intFromPtr(ptr) % @alignOf(ImageDescriptor) != 0) return null;
    const descriptor: *ImageDescriptor = @ptrCast(@alignCast(ptr));
    if (descriptor.magic != DESCRIPTOR_MAGIC) return null;
    return descriptor;
}

/// Locate a const image descriptor by shared-memory byte offset.
pub fn descriptorFromConstOffset(base_ptr: [*]align(1) const u8, total_size: usize, descriptor_offset: usize) ?*const ImageDescriptor {
    if (!validDescriptorRange(total_size, descriptor_offset)) return null;
    const ptr = base_ptr + descriptor_offset;
    if (@intFromPtr(ptr) % @alignOf(ImageDescriptor) != 0) return null;
    const descriptor: *const ImageDescriptor = @ptrCast(@alignCast(ptr));
    if (descriptor.magic != DESCRIPTOR_MAGIC) return null;
    return descriptor;
}

/// Coherent snapshot of one image descriptor's reclamation metadata.
pub const ImageDescriptorSnapshot = struct {
    state: DescriptorState,
    refs: u32,
    generation: u64,
    image_offset: usize,
    image_size: usize,
    allocation_start: usize,
    allocation_end: usize,
};

/// Return the current state of a shared-memory image descriptor.
pub fn descriptorSnapshot(descriptor: *const ImageDescriptor) ImageDescriptorSnapshot {
    return .{
        .state = descriptorStateFromRaw(loadU32(&descriptor.state, .acquire)),
        .refs = loadU32(&descriptor.refs, .acquire),
        .generation = loadU64(&descriptor.generation, .acquire),
        .image_offset = @intCast(loadU64(&descriptor.image_offset, .acquire)),
        .image_size = @intCast(loadU64(&descriptor.image_size, .acquire)),
        .allocation_start = @intCast(loadU64(&descriptor.allocation_start, .acquire)),
        .allocation_end = @intCast(loadU64(&descriptor.allocation_end, .acquire)),
    };
}

/// Return the current state of a descriptor addressed by offset.
pub fn descriptorSnapshotFromOffset(base_ptr: [*]align(1) const u8, total_size: usize, descriptor_offset: usize) ?ImageDescriptorSnapshot {
    const descriptor = descriptorFromConstOffset(base_ptr, total_size, descriptor_offset) orelse return null;
    return descriptorSnapshot(descriptor);
}

/// Mark a descriptor as no longer published but not yet known reusable.
pub fn markDescriptorRetired(descriptor: *ImageDescriptor) void {
    storeU32(&descriptor.state, @intFromEnum(DescriptorState.retired), .release);
}

/// Mark a descriptor reusable by a future compiler rebuild.
pub fn markDescriptorReclaimed(descriptor: *ImageDescriptor) void {
    storeU32(&descriptor.state, @intFromEnum(DescriptorState.reclaimed), .release);
}

/// Retain the latest published image while the shim installs it for direct execution.
pub fn acquirePublishedImage(control: *Control, base_ptr: [*]align(1) u8, total_size: usize) ?RetainedImage {
    for (0..16) |_| {
        const image = publishedImage(control) orelse return null;
        const descriptor = descriptorFromOffset(base_ptr, total_size, image.descriptor_offset) orelse return null;
        retainDescriptor(descriptor);

        const snapshot = descriptorSnapshot(descriptor);
        const current = publishedImage(control);

        if (snapshot.state == .published and
            snapshot.generation == image.generation and
            snapshot.image_offset == image.image_offset and
            snapshot.image_size == image.image_size and
            current != null and
            current.?.generation == image.generation and
            current.?.descriptor_offset == image.descriptor_offset and
            current.?.image_offset == image.image_offset and
            current.?.image_size == image.image_size)
        {
            return .{
                .generation = image.generation,
                .descriptor_offset = image.descriptor_offset,
                .descriptor = descriptor,
                .image_offset = image.image_offset,
                .image_size = image.image_size,
            };
        }

        releaseDescriptor(descriptor);
    }

    return null;
}

/// Retain a descriptor already known to contain a live installed image.
pub fn retainDescriptor(descriptor: *ImageDescriptor) void {
    const previous_refs = @atomicRmw(u32, &descriptor.refs, .Add, 1, .acquire);
    if (builtinModeDebug() and previous_refs == std.math.maxInt(u32)) {
        std.debug.panic("hot reload invariant violated: image descriptor refcount overflowed", .{});
    }
}

/// Release a retained direct-execution image descriptor.
pub fn releaseDescriptor(descriptor: *ImageDescriptor) void {
    const previous = @atomicRmw(u32, &descriptor.refs, .Sub, 1, .acq_rel);
    if (builtinModeDebug() and previous == 0) {
        std.debug.panic("hot reload invariant violated: released an unreferenced image descriptor", .{});
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

fn descriptorStateFromRaw(raw: u32) DescriptorState {
    return switch (raw) {
        @intFromEnum(DescriptorState.writing) => .writing,
        @intFromEnum(DescriptorState.published) => .published,
        @intFromEnum(DescriptorState.retired) => .retired,
        else => .reclaimed,
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

fn testDescriptor(base_ptr: [*]align(1) u8, offset: usize) *ImageDescriptor {
    return @ptrCast(@alignCast(base_ptr + offset));
}

test "hot reload control publishes and acknowledges generations" {
    var bytes: [4096]u8 align(@alignOf(ImageDescriptor)) = [_]u8{0} ** 4096;
    const base: [*]align(1) u8 = &bytes;
    var control = std.mem.zeroes(Control);

    try std.testing.expect(!initialized(&control));

    const desc0_offset: usize = 512;
    const desc0 = testDescriptor(base, desc0_offset);
    prepareDescriptor(desc0, 1, 1024, 4096, 512, 8192, true);
    init(&control, desc0_offset, desc0);
    try std.testing.expect(initialized(&control));
    try std.testing.expectEqual(@as(u64, 1), publishedGeneration(&control));
    try std.testing.expectEqual(@as(usize, 1024), imageOffset(&control));
    try std.testing.expectEqual(@as(usize, 4096), imageSize(&control));
    try std.testing.expectEqual(desc0_offset, publishedImage(&control).?.descriptor_offset);
    try std.testing.expectEqual(Status.none, acknowledgedStatus(&control));

    const desc1_offset: usize = 2048;
    const desc1 = testDescriptor(base, desc1_offset);
    prepareDescriptor(desc1, 2, 8192, 2048, 2048, 12288, true);
    publishDescriptor(&control, 2, desc1_offset, desc1);
    try std.testing.expectEqual(@as(u64, 2), publishedGeneration(&control));
    try std.testing.expectEqual(@as(usize, 8192), imageOffset(&control));
    try std.testing.expectEqual(@as(usize, 2048), imageSize(&control));

    acknowledge(&control, 2, .accepted);
    try std.testing.expectEqual(@as(u64, 2), acknowledgedGeneration(&control));
    try std.testing.expectEqual(Status.accepted, acknowledgedStatus(&control));
}

test "hot reload publication snapshots reject in-progress writes" {
    var control = std.mem.zeroes(Control);
    control.magic = MAGIC;
    control.format_version = FORMAT_VERSION;

    storeU64(&control.publish_sequence, 3, .release);
    storeU64(&control.published_descriptor_offset, 512, .release);
    storeU64(&control.image_offset, 8192, .release);
    storeU64(&control.image_size, 16384, .release);
    storeU64(&control.published_generation, 2, .release);
    try std.testing.expect(publishedImage(&control) == null);

    storeU64(&control.publish_sequence, 4, .release);
    const image = publishedImage(&control).?;
    try std.testing.expectEqual(@as(u64, 2), image.generation);
    try std.testing.expectEqual(@as(usize, 512), image.descriptor_offset);
    try std.testing.expectEqual(@as(usize, 8192), image.image_offset);
    try std.testing.expectEqual(@as(usize, 16384), image.image_size);
}

test "hot reload acknowledgement snapshots reject in-progress writes" {
    var control = std.mem.zeroes(Control);
    control.magic = MAGIC;
    control.format_version = FORMAT_VERSION;

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

test "hot reload image descriptors can be retained and released" {
    var bytes: [4096]u8 align(@alignOf(ImageDescriptor)) = [_]u8{0} ** 4096;
    const base: [*]align(1) u8 = &bytes;
    var control = std.mem.zeroes(Control);

    const desc0_offset: usize = 512;
    const desc0 = testDescriptor(base, desc0_offset);
    prepareDescriptor(desc0, 1, 1024, 4096, 512, 8192, true);
    init(&control, desc0_offset, desc0);

    const retained = acquirePublishedImage(&control, base, bytes.len).?;
    try std.testing.expectEqual(@as(u64, 1), retained.generation);
    try std.testing.expectEqual(desc0_offset, retained.descriptor_offset);
    try std.testing.expectEqual(@as(u32, 1), descriptorSnapshot(desc0).refs);

    releaseDescriptor(retained.descriptor);
    try std.testing.expectEqual(@as(u32, 0), descriptorSnapshot(desc0).refs);
}

test "hot reload image retain rejects descriptors that stopped being current" {
    var bytes: [8192]u8 align(@alignOf(ImageDescriptor)) = [_]u8{0} ** 8192;
    const base: [*]align(1) u8 = &bytes;
    var control = std.mem.zeroes(Control);

    const desc0_offset: usize = 512;
    const desc0 = testDescriptor(base, desc0_offset);
    prepareDescriptor(desc0, 1, 1024, 4096, 512, 4096, true);
    init(&control, desc0_offset, desc0);

    const desc1_offset: usize = 4096;
    const desc1 = testDescriptor(base, desc1_offset);
    prepareDescriptor(desc1, 2, 4608, 8192, 4096, 8192, true);
    publishDescriptor(&control, 2, desc1_offset, desc1);
    markDescriptorRetired(desc0);

    const retained = acquirePublishedImage(&control, base, bytes.len).?;
    try std.testing.expectEqual(@as(u64, 2), retained.generation);
    try std.testing.expectEqual(desc1_offset, retained.descriptor_offset);
    try std.testing.expectEqual(@as(u32, 0), descriptorSnapshot(desc0).refs);
    try std.testing.expectEqual(@as(u32, 1), descriptorSnapshot(desc1).refs);
    releaseDescriptor(retained.descriptor);
}
