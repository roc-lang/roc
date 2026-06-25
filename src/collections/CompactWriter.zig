//! CompactWriter provides efficient serialization using scatter-gather I/O operations.
//! It collects multiple memory regions into iovecs and writes them in a single system call
//! using pwritev, minimizing system call overhead for serialization tasks.
//! The writer handles alignment requirements and padding automatically to ensure
//! proper deserialization of the written data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const native_endian = @import("builtin").cpu.arch.endian();

const CompactWriter = @This();

/// The alignment requirement for buffers used in deserialization.
/// All serialized data must be aligned to this boundary to ensure proper
/// memory access patterns and avoid alignment faults on architectures that
/// require aligned memory access.
pub const SERIALIZATION_ALIGNMENT = std.mem.Alignment.@"16";

const ZEROS: [16]u8 = [_]u8{0} ** 16;

iovecs: std.ArrayList(Iovec),
total_bytes: usize,
// Track all allocated memory so we can free it in deinit
allocated_memory: std.ArrayList(AllocatedMemory),

pub fn init() CompactWriter {
    return CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
}

/// Write all gathered buffers to a file sequentially using positional writes.
/// Accepts any file/io pair where `file.writePositionalAll(io, bytes, offset)` is valid
/// (e.g. the std_io File and Io types). Generic to avoid depending on the io module.
pub fn writeGather(
    self: *@This(),
    file: anytype,
    io: anytype,
) std.Io.File.WritePositionalError!void {
    var offset: u64 = 0;
    for (self.iovecs.items) |iovec| {
        const bytes = @as([*]const u8, @ptrCast(iovec.iov_base))[0..iovec.iov_len];
        try file.writePositionalAll(io, bytes, offset);
        offset += iovec.iov_len;
    }
}

/// Allocates some undefined memory with the same size and alignment as the given value,
/// appends (a pointer to) that memory to the writer, and returns the pointer.
///
/// Since this is returning a pointer to uninitialized memory, it's up to the caller to
/// mutate it in-place to turn its nested pointers' memory addresses into offsets for serialization.
///
/// Note: Padding is added BEFORE the data to ensure proper alignment for the type.
pub fn appendAlloc(
    self: *@This(),
    allocator: std.mem.Allocator,
    comptime T: type,
) std.mem.Allocator.Error!*T {
    const size = @sizeOf(T);
    const alignment = @alignOf(T);

    // When we deserialize, we align the bytes we're deserializing into to ALIGNMENT,
    // which means that we can't serialize anything with alignment higher than that.
    std.debug.assert(alignment <= SERIALIZATION_ALIGNMENT.toByteUnits());

    // Pad up front to the alignment of T
    try self.padToAlignment(allocator, alignment);

    // Allocate a single item of type T, zeroed for deterministic serialization.
    const items = try allocator.alignedAlloc(T, std.mem.Alignment.fromByteUnits(alignment), 1);
    const answer = &items[0];
    @memset(std.mem.asBytes(answer), 0);

    // Track the allocated memory for cleanup
    try self.allocated_memory.append(allocator, .{
        .ptr = @as([*]u8, @ptrCast(answer)),
        .size = size,
        .alignment = alignment,
    });

    // Add the pointer to uninitialized memory to the iovecs for later.
    try self.iovecs.append(allocator, .{
        .iov_base = @ptrCast(@as([*]u8, @ptrCast(answer))),
        .iov_len = size,
    });
    self.total_bytes += size;

    return answer;
}

/// Never call this as the first append in the writer (e.g. always call appendAlloc first),
/// because that will result in this attempting to return a slice with an offset of 0,
/// which will be interpreted by Zig as an attempt to have a slice with a null pointer.
/// This is not allowed, and so will cause a panic in debug builds.
/// (In practice, this should never happen because we always write a struct as the very
/// first write in the writer, never an array.)
pub fn appendSlice(
    self: *@This(),
    allocator: std.mem.Allocator,
    slice: anytype,
) std.mem.Allocator.Error!@TypeOf(slice) {
    const SliceType = @TypeOf(slice);
    const info = @typeInfo(SliceType);
    const T = if (info == .pointer and info.pointer.size == .one)
        std.meta.Child(std.meta.Child(SliceType))
    else
        std.meta.Child(SliceType);
    const size = @sizeOf(T);
    const alignment = @alignOf(T);
    const len = slice.len;

    // Pad up front to the alignment of T
    try self.padToAlignment(allocator, alignment);

    const offset = self.total_bytes;

    try self.iovecs.append(allocator, .{
        .iov_base = @ptrCast(@as([*]const u8, @ptrCast(slice.ptr))),
        .iov_len = size * len,
    });
    self.total_bytes += size * len;

    // Return the same slice type as the input
    const result = if (info.pointer.is_const)
        @as([*]const T, @ptrFromInt(offset))[0..len]
    else
        @as([*]T, @ptrFromInt(offset))[0..len];

    return result;
}

/// Zero all padding bytes in a value of type `V` for deterministic serialization.
/// Auto-layout structs/unions/optionals have undefined padding (inter-field gaps,
/// union tail/overshoot, null optionals) that varies between runs (stack/heap
/// contents); assignment copies ALL bytes including padding, so it must be zeroed
/// before the bytes are written. Recurses into nested aggregates. Primitives, enums,
/// and extern/packed structs have no padding to zero. Shared by `SafeList.Serialized`
/// and `appendSlicePodZeroed` so there is a single deterministic-bytes implementation.
pub fn zeroValuePadding(comptime V: type, ptr: [*]u8) void {
    // The padding mask + per-byte `inline for` are O(@sizeOf(V)) comptime work; large
    // POD element types (e.g. the artifact's stored expr/payload unions) exceed the
    // default 1000-branch quota.
    @setEvalBranchQuota(1_000_000);
    const vinfo = @typeInfo(V);
    const vsize = @sizeOf(V);
    if (vsize == 0) return;

    if (vinfo == .@"union") {
        const uinfo = vinfo.@"union";
        if (uinfo.tag_type) |TagType| {
            const tag_size = @sizeOf(TagType);
            if (tag_size == 0) {
                // A zero-size tag (e.g. a single-variant union) carries no discriminant; the
                // sole payload sits at offset 0.
                if (uinfo.fields.len >= 1 and @sizeOf(uinfo.fields[0].type) > 0) {
                    zeroValuePadding(uinfo.fields[0].type, ptr);
                    @memset(ptr[@sizeOf(uinfo.fields[0].type)..vsize], 0);
                } else {
                    @memset(ptr[0..vsize], 0);
                }
            } else {
                const max_payload = comptime blk: {
                    var m: usize = 0;
                    for (uinfo.fields) |f| m = @max(m, @sizeOf(f.type));
                    break :blk m;
                };
                const max_payload_align = comptime blk: {
                    var a: usize = 1;
                    for (uinfo.fields) |f| a = @max(a, @alignOf(f.type));
                    break :blk a;
                };
                // Zig lays out a tagged union like a 2-field struct {tag, payload}: the tag
                // comes first iff its alignment is >= every payload's, otherwise the payload
                // is first (at offset 0) and the tag follows the largest variant.
                const tag_first = @alignOf(TagType) >= max_payload_align;
                const payload_offset = comptime if (tag_first)
                    std.mem.alignForward(usize, tag_size, max_payload_align)
                else
                    0;
                const tag_offset = comptime if (tag_first)
                    0
                else
                    std.mem.alignForward(usize, max_payload, @alignOf(TagType));

                // Read the discriminant as a raw, bit-width-masked integer — NEVER through
                // the enum/`switch`, which panics in safe builds on a poisoned tag (the
                // round-trip tests fill every byte, tags included). Masking to the tag's bit
                // width mirrors what the compiler's own tag read sees, so an in-range value
                // still selects the right variant.
                const TagInt = @typeInfo(TagType).@"enum".tag_type;
                const StorageInt = std.meta.Int(.unsigned, tag_size * 8);
                const stored = std.mem.readInt(StorageInt, ptr[tag_offset..][0..tag_size], native_endian);
                const tag_val: TagInt = @truncate(stored);

                // Save the tag + active payload, zero the WHOLE value (every inactive / unused
                // / alignment byte becomes deterministic 0), then restore them.
                const saved_tag: [tag_size]u8 = ptr[tag_offset..][0..tag_size].*;
                var handled = false;
                inline for (uinfo.fields) |f| {
                    if (!handled and @intFromEnum(@field(TagType, f.name)) == tag_val) {
                        handled = true;
                        const active_size = @sizeOf(f.type);
                        const saved_payload: [active_size]u8 = if (active_size > 0)
                            ptr[payload_offset..][0..active_size].*
                        else
                            undefined;
                        @memset(ptr[0..vsize], 0);
                        ptr[tag_offset..][0..tag_size].* = saved_tag;
                        if (active_size > 0) {
                            ptr[payload_offset..][0..active_size].* = saved_payload;
                            zeroValuePadding(f.type, ptr + payload_offset);
                        }
                    }
                }
                // An out-of-range discriminant has no active payload to preserve (only
                // reachable from poisoned test bytes, never a real serialized value): zero
                // everything for a deterministic result.
                if (!handled) @memset(ptr[0..vsize], 0);
            }
        }
    } else if (vinfo == .optional) {
        // For optionals: when null, the payload area contains garbage — zero it all.
        // When non-null, recurse into the payload to zero its internal padding,
        // then zero any trailing padding after payload + tag.
        const ChildType = vinfo.optional.child;
        const item = @as(*const V, @ptrCast(@alignCast(ptr)));
        if (item.* == null) {
            @memset(ptr[0..vsize], 0);
        } else {
            // Payload is at offset 0 (auto layout puts highest-alignment first)
            const child_size = @sizeOf(ChildType);
            if (child_size > 0) {
                zeroValuePadding(ChildType, ptr);
            }
            // Zero padding after payload + 1-byte tag
            const meaningful = child_size + 1;
            if (meaningful < vsize) {
                @memset(ptr[meaningful..vsize], 0);
            }
        }
    } else if (vinfo == .@"struct" and vinfo.@"struct".layout == .auto) {
        // Zero inter-field gaps
        const covered = comptime blk: {
            var mask = [_]bool{false} ** vsize;
            for (vinfo.@"struct".fields) |field| {
                const start = @offsetOf(V, field.name);
                const end = start + @sizeOf(field.type);
                for (start..end) |j| mask[j] = true;
            }
            break :blk mask;
        };
        const has_padding = comptime blk: {
            for (covered) |c| {
                if (!c) break :blk true;
            }
            break :blk false;
        };
        if (has_padding) {
            inline for (0..vsize) |j| {
                if (!covered[j]) ptr[j] = 0;
            }
        }
        // Recurse into struct fields that may have internal padding
        inline for (vinfo.@"struct".fields) |field| {
            const FType = field.type;
            const ftype_info = @typeInfo(FType);
            if (@sizeOf(FType) > 0) {
                const needs_recursion = (ftype_info == .@"union" and ftype_info.@"union".tag_type != null) or
                    (ftype_info == .@"struct" and ftype_info.@"struct".layout == .auto) or
                    (ftype_info == .optional);
                if (needs_recursion) {
                    zeroValuePadding(FType, ptr + @offsetOf(V, field.name));
                }
            }
        }
    }
    // Primitives, enums, extern structs: no padding to zero.
}

/// Whether `zeroValuePadding(V, …)` would write any bytes — i.e. whether `V` has
/// undefined padding (auto-struct inter-field gaps, tagged-union tail/overshoot, or an
/// optional). When this is false, a verbatim byte copy of `V` is already deterministic,
/// so serialization can iovec the source directly instead of allocating a scratch copy
/// just to run a no-op padding pass. Kept next to `zeroValuePadding` so the two stay in
/// lockstep: every shape `zeroValuePadding` acts on must return true here.
pub fn needsPaddingZeroing(comptime V: type) bool {
    return switch (@typeInfo(V)) {
        .@"union" => |u| u.tag_type != null,
        .optional => true,
        .@"struct" => |s| s.layout == .auto,
        else => false,
    };
}

/// Append a slice of POD elements with DETERMINISTIC bytes: copy into fresh
/// writer-owned memory, zero each element's padding (`zeroValuePadding`), and gather
/// it. Unlike `appendSlice` (which iovecs the caller's slice verbatim, including
/// undefined padding), this guarantees byte-identical output for byte-identical
/// logical data — required for reproducible builds and content-stable cache bodies.
/// Returns a slice whose `.ptr` is the data's offset within the serialized buffer.
pub fn appendSlicePodZeroed(
    self: *@This(),
    allocator: std.mem.Allocator,
    slice: anytype,
) std.mem.Allocator.Error!@TypeOf(slice) {
    const SliceType = @TypeOf(slice);
    const T = std.meta.Child(SliceType);
    const len = slice.len;

    try self.padToAlignment(allocator, @alignOf(T));
    const offset = self.total_bytes;

    if (len > 0) {
        if (comptime needsPaddingZeroing(T)) {
            // `T` has undefined padding; copy into writer-owned memory and zero it so the
            // bytes are deterministic.
            const buf = try allocator.alloc(T, len);
            for (slice, 0..) |item, i| buf[i] = item;
            for (buf) |*item| zeroValuePadding(T, @as([*]u8, @ptrCast(item)));

            try self.allocated_memory.append(allocator, .{
                .ptr = @ptrCast(buf.ptr),
                .size = len * @sizeOf(T),
                .alignment = @alignOf(T),
            });
            try self.iovecs.append(allocator, .{
                .iov_base = @ptrCast(buf.ptr),
                .iov_len = len * @sizeOf(T),
            });
        } else {
            // `T` has no padding to zero, so the source bytes are already deterministic:
            // iovec them verbatim (no scratch alloc, no copy). The source must outlive the
            // writer's flush — true on the serialize path, where the store owns the data.
            try self.iovecs.append(allocator, .{
                .iov_base = @ptrCast(@as([*]const u8, @ptrCast(slice.ptr))),
                .iov_len = len * @sizeOf(T),
            });
        }
        self.total_bytes += len * @sizeOf(T);
    }

    return @as([*]const T, @ptrFromInt(offset))[0..len];
}

/// Adds padding bytes to ensure the next write will be aligned to the specified boundary.
/// This is critical for ensuring that serialized data structures maintain their required
/// alignment when written to the output buffer.
pub fn padToAlignment(self: *@This(), allocator: std.mem.Allocator, alignment: usize) std.mem.Allocator.Error!void {
    const padding_bytes_needed = std.mem.alignForward(usize, self.total_bytes, alignment) - self.total_bytes;

    if (padding_bytes_needed > 0) {
        try self.iovecs.append(allocator, .{
            .iov_base = @ptrCast(@as([*]const u8, &ZEROS)),
            .iov_len = padding_bytes_needed,
        });
        self.total_bytes += padding_bytes_needed;
    }
}

/// Write all iovecs to a single contiguous buffer for testing purposes.
/// Returns the slice of buffer that was written to.
pub fn writeToBuffer(
    self: *@This(),
    buffer: []u8,
) error{BufferTooSmall}![]u8 {
    if (buffer.len < self.total_bytes) {
        return error.BufferTooSmall;
    }

    var offset: usize = 0;
    for (self.iovecs.items) |iovec| {
        @memcpy(buffer[offset..][0..iovec.iov_len], iovec.iov_base[0..iovec.iov_len]);
        offset += iovec.iov_len;
    }

    return buffer[0..self.total_bytes];
}

/// Deinitialize the CompactWriter, freeing all allocated memory
pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
    // Free all allocated memory slices
    for (self.allocated_memory.items) |memory_slice| {
        const slice = memory_slice.ptr[0..memory_slice.size];
        const alignment_log2 = std.math.log2_int(usize, memory_slice.alignment);
        const alignment: std.mem.Alignment = @enumFromInt(alignment_log2);
        allocator.rawFree(slice, alignment, @returnAddress());
    }
    self.allocated_memory.deinit(allocator);
    self.iovecs.deinit(allocator);
}

const Iovec = extern struct {
    iov_base: [*]const u8,
    iov_len: usize,
};

const AllocatedMemory = struct {
    ptr: [*]u8,
    size: usize,
    alignment: usize,
};

test "zeroValuePadding: tagged-union determinism regardless of prior garbage / layout" {
    // Tagged unions whose variants differ in size/alignment place the discriminant at a
    // target-dependent offset that no comptime formula can reliably predict. The padding
    // zeroer must locate the real tag (so it survives) AND deterministically zero every
    // inactive/padding byte. The property: the same logical value canonicalizes to the
    // SAME bytes no matter what garbage preceded it. (A mislocated tag breaks this and
    // corrupted the discriminant on x86_64.)
    const Case = struct {
        fn check(comptime U: type, value: U) error{TestExpectedEqual}!void {
            var a: [@sizeOf(U)]u8 align(@alignOf(U)) = undefined;
            var b: [@sizeOf(U)]u8 align(@alignOf(U)) = undefined;
            @memset(&a, 0xAA); // two different poisons in the inactive/padding bytes
            @memset(&b, 0x55);
            @as(*U, @ptrCast(&a)).* = value;
            @as(*U, @ptrCast(&b)).* = value;
            zeroValuePadding(U, @ptrCast(&a));
            zeroValuePadding(U, @ptrCast(&b));
            try std.testing.expectEqualSlices(u8, &a, &b); // deterministic, garbage-independent
            try std.testing.expectEqual(std.meta.activeTag(value), std.meta.activeTag(@as(*U, @ptrCast(&a)).*));
        }
    };
    // Payload-first union, largest variant (12) not a multiple of the max payload align (8).
    try Case.check(union(enum) { a: u64, b: [12]u8 }, .{ .b = [_]u8{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 } });
    try Case.check(union(enum) { a: u64, b: [12]u8 }, .{ .a = 0x0102030405060708 });
    // A few more shapes (different sizes/alignments → different tag placements).
    try Case.check(union(enum) { a: u32, b: [3]u8 }, .{ .b = [_]u8{ 9, 9, 9 } });
    try Case.check(union(enum) { a: u128, b: [4]u8 }, .{ .b = [_]u8{ 1, 2, 3, 4 } });
}
