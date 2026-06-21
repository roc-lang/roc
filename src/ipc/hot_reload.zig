//! Shared-memory control block for machine-code hot loading.

const std = @import("std");
const SharedMemoryAllocator = @import("SharedMemoryAllocator.zig");

/// Magic number identifying a hot-load control block ("ROCH" little-endian).
pub const MAGIC: u32 = 0x4843_4F52;

/// Version of the control block layout stored in shared-memory header padding.
pub const FORMAT_VERSION: u32 = 1;

/// Host-shim acknowledgement status for a published hot-load generation.
pub const Status = enum(u32) {
    none = 0,
    accepted = 1,
    rejected = 2,
};

/// Shared-memory control block used to publish replacement executable images.
pub const Control = extern struct {
    magic: u32 = MAGIC,
    format_version: u32 = FORMAT_VERSION,
    published_generation: u64 = 0,
    image_offset: u64 = 0,
    image_size: u64 = 0,
    acknowledged_generation: u64 = 0,
    status: u32 = @intFromEnum(Status.none),
    reserved: [28]u8 = [_]u8{0} ** 28,
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
    control.* = .{
        .image_offset = @intCast(image_offset),
        .image_size = @intCast(image_size),
    };
    publish(control, 1, image_offset, image_size);
}

/// Publish a replacement image for the host shim to load at the next safe point.
pub fn publish(control: *Control, generation: u64, image_offset: usize, image_size: usize) void {
    control.image_offset = @intCast(image_offset);
    control.image_size = @intCast(image_size);
    control.status = @intFromEnum(Status.none);
    @atomicStore(u64, &control.published_generation, generation, .release);
}

/// Acquire-load the latest generation published by the compiler parent.
pub fn publishedGeneration(control: *const Control) u64 {
    if (!initialized(control)) return 0;
    return @atomicLoad(u64, &control.published_generation, .acquire);
}

/// Return the shared-memory byte offset of the currently published image.
pub fn imageOffset(control: *const Control) usize {
    return @intCast(control.image_offset);
}

/// Return the byte length of the currently published image.
pub fn imageSize(control: *const Control) usize {
    return @intCast(control.image_size);
}

/// Publish the host shim's acknowledgement for a generation.
pub fn acknowledge(control: *Control, generation: u64, status: Status) void {
    control.status = @intFromEnum(status);
    @atomicStore(u64, &control.acknowledged_generation, generation, .release);
}

/// Acquire-load the latest generation acknowledged by the host shim.
pub fn acknowledgedGeneration(control: *const Control) u64 {
    if (!initialized(control)) return 0;
    return @atomicLoad(u64, &control.acknowledged_generation, .acquire);
}

/// Return the acknowledgement status associated with the latest acknowledgement.
pub fn acknowledgedStatus(control: *const Control) Status {
    if (!initialized(control)) return .none;
    return switch (@atomicLoad(u32, &control.status, .acquire)) {
        @intFromEnum(Status.accepted) => .accepted,
        @intFromEnum(Status.rejected) => .rejected,
        else => .none,
    };
}

test "hot reload control publishes and acknowledges generations" {
    var control = std.mem.zeroes(Control);

    try std.testing.expect(!initialized(&control));

    init(&control, 512, 4096);
    try std.testing.expect(initialized(&control));
    try std.testing.expectEqual(@as(u64, 1), publishedGeneration(&control));
    try std.testing.expectEqual(@as(usize, 512), imageOffset(&control));
    try std.testing.expectEqual(@as(usize, 4096), imageSize(&control));
    try std.testing.expectEqual(Status.none, acknowledgedStatus(&control));

    publish(&control, 2, 8192, 2048);
    try std.testing.expectEqual(@as(u64, 2), publishedGeneration(&control));
    try std.testing.expectEqual(@as(usize, 8192), imageOffset(&control));
    try std.testing.expectEqual(@as(usize, 2048), imageSize(&control));

    acknowledge(&control, 2, .accepted);
    try std.testing.expectEqual(@as(u64, 2), acknowledgedGeneration(&control));
    try std.testing.expectEqual(Status.accepted, acknowledgedStatus(&control));
}

test "hot reload control block fits in shared memory reserved header bytes" {
    try std.testing.expect(@sizeOf(Control) <= @sizeOf(@FieldType(SharedMemoryAllocator.Header, "reserved")));
}
