//! CompactWriter provides efficient serialization using scatter-gather I/O operations.
//! It collects multiple memory regions into iovecs and writes them in a single system call
//! using pwritev, minimizing system call overhead for serialization tasks.
//! The writer handles alignment requirements and padding automatically to ensure
//! proper deserialization of the written data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const native_endian = @import("builtin").cpu.arch.endian();
const serde_validation = @import("serde_validation.zig");

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

    // Reserve the bookkeeping slots before allocating, so the handoff below cannot fail.
    try self.reserveOwnedBuffer(allocator);

    // Allocate a single item of type T, zeroed for deterministic serialization.
    const items = try allocator.alignedAlloc(T, std.mem.Alignment.fromByteUnits(alignment), 1);
    const answer = &items[0];
    @memset(std.mem.asBytes(answer), 0);

    self.takeOwnedBufferAssumeCapacity(@as([*]u8, @ptrCast(answer)), size, alignment);

    return answer;
}

/// Reserve room for one more writer-owned buffer in both bookkeeping lists.
///
/// Callers reserve *before* allocating the buffer, so that the handoff which makes
/// the writer responsible for freeing it cannot itself run out of memory. Doing it
/// the other way around strands the buffer: it is neither reachable from the caller
/// (which is unwinding) nor from `deinit`.
fn reserveOwnedBuffer(self: *@This(), allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    try self.allocated_memory.ensureUnusedCapacity(allocator, 1);
    try self.iovecs.ensureUnusedCapacity(allocator, 1);
}

/// Take ownership of `bytes` (allocated from the writer's allocator with `alignment`)
/// and gather it. Infallible by construction: `reserveOwnedBuffer` already made room,
/// so the buffer is registered for release and referenced by an iovec together, never
/// one without the other.
fn takeOwnedBufferAssumeCapacity(self: *@This(), ptr: [*]u8, size: usize, alignment: usize) void {
    self.allocated_memory.appendAssumeCapacity(.{
        .ptr = ptr,
        .size = size,
        .alignment = alignment,
    });
    self.iovecs.appendAssumeCapacity(.{
        .iov_base = ptr,
        .iov_len = size,
    });
    self.total_bytes += size;
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

/// Canonicalize every byte and bit of a value of type `V` that its declaration does not
/// define, so the result depends on the logical value alone.
///
/// What that covers, and why each case is undefined to begin with: an auto-layout
/// struct's inter-field gaps and a tagged union's inactive-variant and tail bytes are
/// never written, so they hold whatever the storage held before; a null optional's
/// payload area likewise; and a scalar, enum, or packed struct whose declared bits stop
/// short of its storage leaves the remaining bits unspecified, so those are rewritten
/// masked from the value's own width rather than assumed to be zero-extended. Nested
/// aggregates and arrays recurse. A type whose every byte is already declared is left
/// alone—`needsPaddingZeroing` says which, and rejects at compile time anything whose
/// undefined bytes nothing could identify.
///
/// Assignment copies ALL bytes, padding included, so this must run on the copy that is
/// about to be written. `appendSlicePodZeroedOffset` is the one caller that does so on
/// the serialization path, which keeps a single deterministic-bytes implementation.
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

                // Read the discriminant as a raw, bit-width-masked integer—NEVER through
                // the enum/`switch`, which panics in safe builds on a poisoned tag (the
                // round-trip tests fill every byte, tags included). Masking to the tag's bit
                // width mirrors what the compiler's own tag read sees, so an in-range value
                // still selects the right variant.
                const TagInt = @typeInfo(TagType).@"enum".tag_type;
                const StorageInt = std.meta.Int(.unsigned, tag_size * 8);
                const tag_mask: StorageInt = if (@bitSizeOf(TagInt) >= tag_size * 8)
                    ~@as(StorageInt, 0)
                else
                    (@as(StorageInt, 1) << @bitSizeOf(TagInt)) - 1;
                const stored = std.mem.readInt(StorageInt, ptr[tag_offset..][0..tag_size], native_endian);
                const masked_tag = stored & tag_mask;
                const tag_val: TagInt = @truncate(stored);

                // Save the active payload, zero the WHOLE value (every inactive / unused /
                // alignment byte becomes deterministic 0), then restore the payload and write
                // the discriminant back zero-extended from its masked value. Writing the
                // masked value rather than the original bytes makes a sub-byte-width tag's
                // storage byte deterministic without assuming how the compiler extended it.
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
                        std.mem.writeInt(StorageInt, ptr[tag_offset..][0..tag_size], masked_tag, native_endian);
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
        // For optionals: when null, the payload area contains garbage—zero it all.
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
        // Recurse into struct fields that are not already fully defined. A field whose
        // bytes are fully defined has nothing to zero, and a field that is neither
        // defined nor scrubbable is a compile error at the serialization boundary, so it
        // cannot reach here.
        inline for (vinfo.@"struct".fields) |field| {
            const FType = field.type;
            if (@sizeOf(FType) > 0 and comptime !serde_validation.isFullyDefined(FType)) {
                zeroValuePadding(FType, ptr + @offsetOf(V, field.name));
            }
        }
    } else if (vinfo == .@"struct" and vinfo.@"struct".layout == .@"extern") {
        // An extern struct has no implicit gaps (proven at the serialization boundary),
        // but a field may still need scrubbing.
        inline for (vinfo.@"struct".fields) |field| {
            const FType = field.type;
            if (@sizeOf(FType) > 0 and comptime !serde_validation.isFullyDefined(FType)) {
                zeroValuePadding(FType, ptr + @offsetOf(V, field.name));
            }
        }
    } else if (vinfo == .int or vinfo == .@"enum" or
        (vinfo == .@"struct" and vinfo.@"struct".layout == .@"packed"))
    {
        // A scalar (or packed struct) whose declared bits do not fill its storage. The
        // spare high bits are not part of the value, and Zig does not specify what a
        // store leaves in them, so write the value back zero-extended from its own
        // declared width. `bits` comes from the declaration, not from inspection.
        const bits = if (vinfo == .@"enum") @bitSizeOf(vinfo.@"enum".tag_type) else @bitSizeOf(V);
        if (bits < vsize * 8) {
            const StorageInt = std.meta.Int(.unsigned, vsize * 8);
            const mask: StorageInt = (@as(StorageInt, 1) << bits) - 1;
            const stored = std.mem.readInt(StorageInt, ptr[0..vsize], native_endian);
            std.mem.writeInt(StorageInt, ptr[0..vsize], stored & mask, native_endian);
        }
    } else if (vinfo == .array) {
        // An array of scrubbable elements: each element carries its own padding.
        const Child = vinfo.array.child;
        const child_size = @sizeOf(Child);
        if (child_size > 0 and comptime !serde_validation.isFullyDefined(Child)) {
            for (0..vsize / child_size) |i| zeroValuePadding(Child, ptr + i * child_size);
        }
    }
    // Everything left is already fully defined: a scalar whose value bits fill its
    // storage, and an extern struct/union whose declarations cover every byte.
}

/// Whether `zeroValuePadding(V, …)` would write any bytes—i.e. whether `V` has
/// undefined padding (auto-struct inter-field gaps, tagged-union tail/overshoot, or an
/// optional). When this is false, a verbatim byte copy of `V` is already deterministic,
/// so serialization can iovec the source directly instead of allocating a scratch copy
/// just to run a no-op padding pass.
///
/// This is `serde_validation.byteDetermination`, so asking the question is also the
/// compile-time proof that every byte of `V` is accounted for: a shape with undefined
/// bytes that no discriminant identifies (an `extern struct` with implicit padding, an
/// `extern union` with short variants) is a compile error here rather than a silent
/// source of garbage bytes in the output.
pub fn needsPaddingZeroing(comptime V: type) bool {
    return serde_validation.byteDetermination(V) == .scrubbable;
}

/// Append a slice of POD items with DETERMINISTIC bytes and return the byte offset the
/// data begins at within the serialized buffer.
///
/// This is the single append-and-scrub implementation. It pads to the item type's
/// alignment, then either gathers the caller's bytes verbatim—when every byte of the
/// item type is already defined, so no copy is needed—or copies them into writer-owned
/// memory and canonicalizes the undefined bytes there. Either way the caller's data is
/// never modified, so a frozen or shared store can be serialized. Unlike `appendSlice`
/// (which iovecs the caller's slice verbatim, undefined padding included), the bytes
/// this writes are byte-identical for byte-identical logical data, which is what
/// reproducible builds and content-stable cache bodies need.
///
/// The offset is returned as an integer, so a store whose data legitimately begins at
/// byte zero records zero. Anything recording an offset should use this rather than
/// `appendSlicePodZeroed`, whose slice-pointer encoding cannot represent zero.
pub fn appendSlicePodZeroedOffset(
    self: *@This(),
    allocator: std.mem.Allocator,
    slice: anytype,
) std.mem.Allocator.Error!usize {
    const T = std.meta.Child(@TypeOf(slice));
    const len = slice.len;

    try self.padToAlignment(allocator, @alignOf(T));
    const offset = self.total_bytes;

    if (len > 0) {
        if (comptime needsPaddingZeroing(T)) {
            // `T` has undefined padding; copy into writer-owned memory and zero it so the
            // bytes are deterministic. Reserve the bookkeeping slots first so the buffer
            // is never allocated without a path to releasing it.
            try self.reserveOwnedBuffer(allocator);

            const buf = try allocator.alloc(T, len);
            for (slice, 0..) |item, i| buf[i] = item;
            for (buf) |*item| zeroValuePadding(T, @as([*]u8, @ptrCast(item)));

            self.takeOwnedBufferAssumeCapacity(@ptrCast(buf.ptr), len * @sizeOf(T), @alignOf(T));
        } else {
            // `T` has no padding to zero, so the source bytes are already deterministic:
            // iovec them verbatim (no scratch alloc, no copy). The source must outlive the
            // writer's flush—true on the serialize path, where the store owns the data.
            try self.iovecs.append(allocator, .{
                .iov_base = @ptrCast(@as([*]const u8, @ptrCast(slice.ptr))),
                .iov_len = len * @sizeOf(T),
            });
            // The owned-buffer branch accounts for its own bytes when it takes ownership.
            self.total_bytes += len * @sizeOf(T);
        }
    }

    return offset;
}

/// `appendSlicePodZeroedOffset` for callers that carry the offset inside the returned
/// slice's pointer rather than as an integer. Same bytes, same ownership, same scrub;
/// the only difference is how the offset comes back.
///
/// Never call this as the first append on a writer: a pointer cannot hold an offset of
/// zero, so the data must begin past something else. In practice a store writes its
/// header struct with `appendAlloc` first, which is exactly what puts it past zero. A
/// caller that cannot promise that should record an integer offset instead.
pub fn appendSlicePodZeroed(
    self: *@This(),
    allocator: std.mem.Allocator,
    slice: anytype,
) std.mem.Allocator.Error!@TypeOf(slice) {
    const T = std.meta.Child(@TypeOf(slice));
    const offset = try self.appendSlicePodZeroedOffset(allocator, slice);
    std.debug.assert(offset != 0);
    return @as([*]const T, @ptrFromInt(offset))[0..slice.len];
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

test "zeroValuePadding: preserves the discriminant across union layouts" {
    // Tagged unions whose variants differ in size/alignment place the discriminant at a
    // target-dependent offset that no comptime formula can reliably predict, and the
    // scrubber has to locate it: a mislocated tag zeroes the discriminant instead of
    // preserving it (which is what corrupted values on x86_64). These shapes cover
    // payload-first and tag-first layouts and sizes that are not multiples of the
    // payload alignment.
    //
    // This case assigns whole values, which Zig may lower as a full-width copy from a
    // temporary; when it does, no stale bytes survive for the scrub to erase, so the
    // byte-equality check here is weak on its own. Byte determinism in the presence of
    // genuinely stale bytes is covered by the tests below, which place those bytes
    // deliberately.
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

test "needsPaddingZeroing: a fully defined element is gathered without a scrub copy" {
    // These are the shapes serialization may point an iovec straight at. Getting this
    // wrong in the permissive direction writes undefined bytes; getting it wrong in
    // the strict direction costs a copy of every element.
    try std.testing.expect(!needsPaddingZeroing(u64));
    try std.testing.expect(!needsPaddingZeroing(enum(u8) { a, b }));
    try std.testing.expect(!needsPaddingZeroing(extern struct { a: u32, b: u32 }));
    try std.testing.expect(!needsPaddingZeroing([4]extern struct { a: u32, b: u32 }));
    try std.testing.expect(!needsPaddingZeroing(packed struct(u8) { a: u4, b: u4 }));
    // An auto struct with no gaps needs no scrub either, even though its layout is
    // compiler-chosen.
    try std.testing.expect(!needsPaddingZeroing(struct { a: u64, b: u64 }));

    // ...and these need one.
    try std.testing.expect(needsPaddingZeroing(struct { a: u8, b: u64 }));
    try std.testing.expect(needsPaddingZeroing(union(enum) { a: u32, b: u64 }));
    try std.testing.expect(needsPaddingZeroing(?u32));
}

/// Poison patterns kept in mutable globals so the optimizer cannot fold a test's
/// "prior contents" away and make two differently-poisoned buffers identical before
/// the value is even written.
var poison_one: u8 = 0xAA;
var poison_two: u8 = 0x55;

/// Write `value` into `ptr` one leaf at a time, so that every byte NOT covered by a
/// declared field keeps whatever `ptr` held before. A whole-aggregate assignment
/// would copy the source temporary's own padding into the destination, which makes
/// two differently-poisoned buffers agree before any scrubbing happens—and a
/// determinism test built on that proves nothing.
fn writeLeafwiseForTest(comptime V: type, ptr: [*]u8, value: V) void {
    switch (@typeInfo(V)) {
        .@"struct" => |st| {
            if (st.layout == .@"packed") {
                @as(*align(1) V, @ptrCast(ptr)).* = value;
                return;
            }
            inline for (st.fields) |f| {
                if (@sizeOf(f.type) > 0) {
                    writeLeafwiseForTest(f.type, ptr + @offsetOf(V, f.name), @field(value, f.name));
                }
            }
        },
        .array => |a| {
            if (@sizeOf(a.child) > 0) {
                for (value, 0..) |elem, i| writeLeafwiseForTest(a.child, ptr + i * @sizeOf(a.child), elem);
            }
        },
        .bool,
        .int,
        .float,
        .pointer,
        .vector,
        .@"enum",
        .@"union",
        .optional,
        .void,
        .type,
        .noreturn,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .enum_literal,
        => @as(*align(1) V, @ptrCast(ptr)).* = value,
    }
}

/// Assert the property every scrubbable shape owes serialization: two representations
/// of the same logical value, built over different prior memory, canonicalize to the
/// same bytes without losing the value. `must_differ` says the caller arranged for the
/// two representations to actually disagree beforehand, so the scrub is doing the work.
fn expectScrubCanonicalizes(
    comptime V: type,
    one: *[@sizeOf(V)]u8,
    two: *[@sizeOf(V)]u8,
    expected: V,
    must_differ: bool,
) error{ TestExpectedEqual, TestUnexpectedResult }!void {
    if (must_differ) {
        try std.testing.expect(!std.mem.eql(u8, one, two));
    }
    zeroValuePadding(V, @ptrCast(one));
    zeroValuePadding(V, @ptrCast(two));
    try std.testing.expectEqualSlices(u8, one, two);
    try std.testing.expectEqualDeep(expected, @as(*const V, @ptrCast(@alignCast(one))).*);
}

/// A pair of `V`-sized, `V`-aligned buffers filled with the two poison patterns, so a
/// test can build the same logical value over two different prior memory states.
fn PoisonedPair(comptime V: type) type {
    return struct {
        one: [@sizeOf(V)]u8 align(@alignOf(V)) = undefined,
        two: [@sizeOf(V)]u8 align(@alignOf(V)) = undefined,

        /// Poison in place. Returning a poisoned pair by value instead lets the
        /// compiler treat the whole aggregate as undefined and drop the fills, which
        /// silently turns every "the two differ beforehand" assertion into a no-op.
        fn poison(self: *@This()) void {
            @memset(&self.one, poison_one);
            @memset(&self.two, poison_two);
        }

        fn onePtr(self: *@This()) *V {
            return @ptrCast(@alignCast(&self.one));
        }

        fn twoPtr(self: *@This()) *V {
            return @ptrCast(@alignCast(&self.two));
        }
    };
}

test "zeroValuePadding: canonicalizes inter-field and inter-element gaps" {
    // Leafwise writes leave every gap holding the prior poison, which is what a store
    // built column by column (or field by field) really looks like in memory.
    {
        const V = struct { a: u8, b: u64 };
        comptime std.debug.assert(needsPaddingZeroing(V));
        var pair: PoisonedPair(V) = .{};
        pair.poison();
        const value = V{ .a = 3, .b = 7 };
        writeLeafwiseForTest(V, @ptrCast(&pair.one), value);
        writeLeafwiseForTest(V, @ptrCast(&pair.two), value);
        try expectScrubCanonicalizes(V, &pair.one, &pair.two, value, true);
    }
    {
        // A nested auto struct: the inner gap and the outer gap are both scrubbed.
        const Inner = struct { a: u8, b: u32 };
        const V = struct { inner: Inner, c: u8 };
        comptime std.debug.assert(needsPaddingZeroing(V));
        var pair: PoisonedPair(V) = .{};
        pair.poison();
        const value = V{ .inner = .{ .a = 1, .b = 2 }, .c = 3 };
        writeLeafwiseForTest(V, @ptrCast(&pair.one), value);
        writeLeafwiseForTest(V, @ptrCast(&pair.two), value);
        try expectScrubCanonicalizes(V, &pair.one, &pair.two, value, true);
    }
    {
        // An array of padded values: every element carries its own gap.
        const Elem = struct { a: u8, b: u32 };
        const V = [3]Elem;
        comptime std.debug.assert(needsPaddingZeroing(V));
        var pair: PoisonedPair(V) = .{};
        pair.poison();
        const value = V{ .{ .a = 1, .b = 2 }, .{ .a = 3, .b = 4 }, .{ .a = 5, .b = 6 } };
        writeLeafwiseForTest(V, @ptrCast(&pair.one), value);
        writeLeafwiseForTest(V, @ptrCast(&pair.two), value);
        try expectScrubCanonicalizes(V, &pair.one, &pair.two, value, true);
    }
}

test "zeroValuePadding: canonicalizes a tagged union's inactive bytes" {
    // Assigning a union may or may not write the bytes outside the active variant:
    // Zig is free to copy the whole union from a temporary, or to write only the tag
    // and the live payload and leave the rest holding whatever was there. That freedom
    // is the hazard, so the test puts the stale bytes there deliberately rather than
    // hoping a particular codegen leaves them.
    const V = union(enum) { small: u32, large: u64 };
    comptime std.debug.assert(needsPaddingZeroing(V));

    var pair: PoisonedPair(V) = .{};
    pair.poison();
    const value = V{ .small = 9 };
    pair.onePtr().* = value;
    pair.twoPtr().* = value;

    // The largest payload has the greatest alignment, so the payload sits at offset 0
    // and the discriminant follows it. Check that before poisoning around it, so a
    // layout change fails here with a clear cause instead of corrupting the value.
    const tag_at = @sizeOf(u64);
    try std.testing.expectEqual(@intFromEnum(std.meta.Tag(V).small), pair.one[tag_at]);
    try std.testing.expectEqual(@intFromEnum(std.meta.Tag(V).small), pair.two[tag_at]);

    // Everything except the live payload and the discriminant is stale: the tail of the
    // payload area the smaller variant does not reach, and the bytes after the tag.
    @memset(pair.one[@sizeOf(u32)..tag_at], poison_one);
    @memset(pair.two[@sizeOf(u32)..tag_at], poison_two);
    @memset(pair.one[tag_at + 1 ..], poison_one);
    @memset(pair.two[tag_at + 1 ..], poison_two);

    try expectScrubCanonicalizes(V, &pair.one, &pair.two, value, true);
}

test "zeroValuePadding: canonicalizes a null optional's stale payload" {
    // A null optional's payload area is not written at all, so it keeps whatever the
    // slot held. Poison it explicitly after setting null: that is exactly the stale
    // state the scrubber exists to erase, and the tag must survive it.
    const V = ?u32;
    comptime std.debug.assert(needsPaddingZeroing(V));

    var pair: PoisonedPair(V) = .{};
    pair.poison();
    pair.onePtr().* = null;
    pair.twoPtr().* = null;
    // The payload occupies the leading `@sizeOf(u32)` bytes; the tag follows it.
    @memset(pair.one[0..@sizeOf(u32)], poison_one);
    @memset(pair.two[0..@sizeOf(u32)], poison_two);
    try expectScrubCanonicalizes(V, &pair.one, &pair.two, null, true);

    // And a present optional keeps its payload while its trailing bytes canonicalize.
    var present: PoisonedPair(V) = .{};
    present.poison();
    present.onePtr().* = 5;
    present.twoPtr().* = 5;
    @memset(present.one[@sizeOf(u32) + 1 ..], poison_one);
    @memset(present.two[@sizeOf(u32) + 1 ..], poison_two);
    try expectScrubCanonicalizes(V, &present.one, &present.two, 5, true);
}

test "zeroValuePadding: canonicalizes spare storage bits of narrow scalars and tags" {
    // Zig zero-extends a sub-byte store today, but it does not specify that it must, so
    // the scrubber masks those bits from the value's own declared width. Setting the
    // spare bits by hand is the only way to test that masking: it is the state an
    // unspecified store would be free to leave behind.
    const Cases = struct {
        fn check(comptime V: type, value: V, spare_mask: u8) error{ TestExpectedEqual, TestUnexpectedResult }!void {
            comptime std.debug.assert(needsPaddingZeroing(V));
            var pair: PoisonedPair(V) = .{};
            pair.poison();
            writeLeafwiseForTest(V, @ptrCast(&pair.one), value);
            writeLeafwiseForTest(V, @ptrCast(&pair.two), value);
            // Two different patterns in the bits that are not part of the value.
            pair.one[0] |= spare_mask;
            pair.two[0] |= spare_mask & 0x88;
            try expectScrubCanonicalizes(V, &pair.one, &pair.two, value, true);
        }
    };
    // A three-bit integer in a one-byte slot: the top five bits are spare.
    try Cases.check(struct { n: u3 }, .{ .n = 5 }, 0xF8);
    // A two-bit enum: same shape, reached through the enum's tag type.
    try Cases.check(struct { e: enum(u2) { a, b, c } }, .{ .e = .c }, 0xFC);
    // A packed struct whose declared bits stop short of its storage byte.
    try Cases.check(packed struct(u3) { flag: bool, rest: u2 }, .{ .flag = true, .rest = 2 }, 0xF8);
    // A tagged union whose discriminant is one bit wide inside a one-byte slot: the
    // scrubber rewrites the discriminant masked rather than copying its storage byte.
    {
        const V = union(enum) { a: u32, b: u64 };
        var pair: PoisonedPair(V) = .{};
        pair.poison();
        const value = V{ .a = 7 };
        pair.onePtr().* = value;
        pair.twoPtr().* = value;
        // The discriminant sits after the largest variant in this layout; poison its
        // spare high bits without disturbing the one bit that names the variant.
        const tag_at = @sizeOf(u64);
        pair.one[tag_at] |= 0xFE;
        pair.two[tag_at] |= 0x0E;
        try expectScrubCanonicalizes(V, &pair.one, &pair.two, value, true);
    }
}

test "byteDetermination: implicit padding in a fixed layout is not silently accepted" {
    // The shapes below are chosen so that their padding does not depend on the host's
    // alignment rules (`@alignOf(u64)` is 4 on 32-bit x86 and 8 elsewhere, so a
    // `{u32, u64}` struct has padding on one and none on the other). A trailing byte
    // after a wider field pads on every supported host, and the assertions check the
    // layout they assume rather than taking it on faith.
    {
        // Trailing padding: the fields cover 5 of the struct's bytes.
        const Padded = extern struct { a: u32, b: u8 };
        comptime std.debug.assert(@sizeOf(Padded) > @offsetOf(Padded, "b") + @sizeOf(u8));
        try std.testing.expect(!serde_validation.isFullyDefined(Padded));

        // Declaring the gap fixes it, without changing the size.
        const Declared = extern struct { a: u32, b: u8, _reserved: [3]u8 = .{ 0, 0, 0 } };
        comptime std.debug.assert(@sizeOf(Declared) == @sizeOf(Padded));
        try std.testing.expect(serde_validation.isFullyDefined(Declared));
    }
    {
        // An extern union whose variant is shorter than the union has the same problem,
        // and no discriminant that could tell a scrub which bytes are dead. This is the
        // shape `Node.Payload` had.
        const Short = extern union { small: u8, large: u64 };
        comptime std.debug.assert(@sizeOf(Short) > @sizeOf(u8));
        try std.testing.expect(!serde_validation.isFullyDefined(Short));

        const Filled = extern union {
            small: extern struct { v: u8, _reserved: [@sizeOf(u64) - 1]u8 = .{0} ** (@sizeOf(u64) - 1) },
            large: u64,
        };
        comptime std.debug.assert(@sizeOf(Filled) == @sizeOf(Short));
        try std.testing.expect(serde_validation.isFullyDefined(Filled));
    }
}

test "allocation failure at any point leaves no writer-owned buffer stranded" {
    // The writer allocates scratch buffers and then registers them for release. If the
    // registration is what fails, the buffer is reachable from neither the unwinding
    // caller nor `deinit`, so it leaks. Each scenario below is replayed with every
    // allocation failing in turn, on a FRESH writer, so the registration append really
    // is an allocating (and therefore failing) call rather than one absorbed by
    // capacity a previous append happened to leave behind. The testing allocator fails
    // the test if any buffer outlives its writer.
    const Padded = struct { small: u8, big: u64 }; // has an inter-field gap: scratch-copy path
    const Flat = extern struct { a: u32, b: u32 }; // fully defined: no-copy path
    comptime std.debug.assert(needsPaddingZeroing(Padded));
    comptime std.debug.assert(!needsPaddingZeroing(Flat));

    const padded = [_]Padded{ .{ .small = 1, .big = 2 }, .{ .small = 3, .big = 4 } };
    const flat = [_]Flat{ .{ .a = 5, .b = 6 }, .{ .a = 7, .b = 8 } };

    const Scenario = enum {
        /// The header path as the writer's very first call, so its registration append
        /// is the one that has to grow the bookkeeping list.
        header_first,
        /// A whole store's worth of writes: header, scrubbed column, verbatim column.
        full_sequence,
        /// Enough scrubbed columns to grow both bookkeeping lists several times, so a
        /// registration append is an allocating—and therefore failing—call at more than
        /// one failure index.
        many_columns,
    };

    for (std.enums.values(Scenario)) |scenario| {
        var fail_index: usize = 0;
        var reached_success = false;
        var failures_seen: usize = 0;
        while (fail_index < 128) : (fail_index += 1) {
            var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = fail_index });
            const allocator = failing.allocator();

            var writer = CompactWriter.init();
            defer writer.deinit(allocator);

            const attempt = switch (scenario) {
                .header_first => blk: {
                    _ = writer.appendAlloc(allocator, Flat) catch |err| break :blk err;
                    break :blk {};
                },
                .full_sequence => blk: {
                    const header = writer.appendAlloc(allocator, Flat) catch |err| break :blk err;
                    header.* = .{ .a = 1, .b = 2 };
                    _ = writer.appendSlicePodZeroed(allocator, @as([]const Padded, &padded)) catch |err| break :blk err;
                    _ = writer.appendSlicePodZeroed(allocator, @as([]const Flat, &flat)) catch |err| break :blk err;
                    break :blk {};
                },
                .many_columns => blk: {
                    _ = writer.appendAlloc(allocator, Flat) catch |err| break :blk err;
                    for (0..24) |_| {
                        _ = writer.appendSlicePodZeroed(allocator, @as([]const Padded, &padded)) catch |err| break :blk err;
                    }
                    break :blk {};
                },
            };
            if (attempt) |_| {
                // Past the last allocation: nothing left to fail, so stop walking.
                reached_success = true;
                break;
            } else |err| {
                try std.testing.expectEqual(error.OutOfMemory, err);
                failures_seen += 1;
            }
        }
        // The walk must have actually forced failures and then run clean, otherwise it
        // proved nothing about the later failure points.
        try std.testing.expect(reached_success);
        try std.testing.expect(failures_seen > 0);
    }
}
