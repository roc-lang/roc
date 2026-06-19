//! Shared serialization helpers for `CheckedModuleArtifact` sub-stores.
//!
//! These let a store keep its in-memory `[]T` representation unchanged while
//! still serializing via the CompactWriter offset-relocation scheme: a slice of
//! POD (relocation-invariant) elements is written contiguously and reconstructed
//! by recomputing one base-relative pointer — a single fixup per slice,
//! independent of the slice length. This is the transform-A building block (no
//! representation change, no consumer churn).
//!
//! "POD / relocation-invariant" means `T` contains only scalars, enums, packed
//! structs, and `Idx`/offset integers — NO pointers, slices, or maps nested
//! inside an element. Slices whose elements embed pointers/slices need the
//! side-list (transform B) treatment instead and must NOT use this helper.

const std = @import("std");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;

/// `@compileError` unless `T` is relocation-invariant ("POD"): it transitively
/// contains no pointers or slices, so the only fixup needed when a `[]T` moves
/// is the outer slice's base pointer. This is what makes a single-fixup
/// `SerializedSlice(T)` *correct* — an element with an embedded pointer/slice
/// would silently dangle after relocation, so we reject it at compile time
/// (such a type needs the transform-B side-list treatment instead).
pub fn assertRelocatablePod(comptime T: type) void {
    switch (@typeInfo(T)) {
        .int, .float, .bool, .void, .@"enum", .error_set, .vector => {},
        .optional => |o| assertRelocatablePod(o.child),
        .array => |a| assertRelocatablePod(a.child),
        .@"struct" => |s| {
            for (s.fields) |f| assertRelocatablePod(f.type);
        },
        .@"union" => |u| {
            for (u.fields) |f| assertRelocatablePod(f.type);
        },
        .pointer => @compileError("SerializedSlice element type '" ++ @typeName(T) ++
            "' contains a pointer/slice; it is not relocation-invariant. Use a side-list (transform B) instead."),
        else => @compileError("SerializedSlice element type '" ++ @typeName(T) ++
            "' has an unsupported (possibly non-POD) representation: " ++ @tagName(@typeInfo(T))),
    }
}

/// Relocatable serialized form of a `[]T` of POD elements. Exactly one
/// relocatable base pointer (`offset`), regardless of `len`.
pub fn SerializedSlice(comptime T: type) type {
    comptime assertRelocatablePod(T);
    comptime std.debug.assert(@alignOf(T) <= CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits());
    return extern struct {
        /// Byte offset of the first element within the serialized buffer.
        offset: i64 = 0,
        /// Number of elements.
        len: u64 = 0,

        const Self = @This();

        /// One relocatable base pointer (`offset`). Read by `relocatablePointerCount`.
        pub const serialized_relocatable_pointers: usize = 1;

        /// Append `slice`'s bytes to `writer` and record their offset/len.
        pub fn serialize(self: *Self, slice: []const T, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
            if (slice.len == 0) {
                self.* = .{};
                return;
            }
            const out = try writer.appendSlice(gpa, slice);
            self.offset = @intCast(@intFromPtr(out.ptr));
            self.len = out.len;
        }

        /// Reconstruct the slice pointing into the relocated buffer at `base`.
        pub fn deserialize(self: *const Self, base: usize) []T {
            if (self.len == 0) return &.{};
            const ptr: [*]T = @ptrFromInt(base +% @as(usize, @intCast(self.offset)));
            return ptr[0..@intCast(self.len)];
        }
    };
}

/// Relocatable serialized form of an `?T` of POD `T`. Encodes presence as a
/// 0- or 1-element `SerializedSlice` (the payload lives in the separate buffer
/// region, so `T` need not be extern-compatible, unlike an inline optional
/// field). One relocatable pointer.
pub fn SerializedOptional(comptime T: type) type {
    comptime assertRelocatablePod(T);
    return extern struct {
        slot: SerializedSlice(T) = .{},

        const Self = @This();

        /// `opt` must remain alive until the writer is flushed to a buffer: when
        /// present, the payload's memory (inside `opt`) is referenced, not copied.
        pub fn serialize(self: *Self, opt: *const ?T, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
            if (opt.* != null) {
                const payload: [*]const T = @ptrCast(&opt.*.?);
                try self.slot.serialize(payload[0..1], gpa, writer);
            } else {
                self.slot = .{};
            }
        }

        pub fn deserialize(self: *const Self, base: usize) ?T {
            const s = self.slot.deserialize(base);
            return if (s.len == 0) null else s[0];
        }
    };
}

/// Count the relocatable base pointers a `Serialized` type fixes up when it
/// moves — i.e. its deserialization fixup count. A `SerializedSlice` /
/// `SerializedOptional` field contributes exactly 1 (via its
/// `serialized_relocatable_pointers` decl); a nested aggregate is summed
/// recursively. This is a pure compile-time function of the *type*, never of the
/// data, so a store can `comptime`-assert its fixup count is a fixed constant —
/// the core invariant that makes deserialization O(1) in the data size.
pub fn relocatablePointerCount(comptime T: type) usize {
    return switch (@typeInfo(T)) {
        .@"struct" => |s| blk: {
            if (@hasDecl(T, "serialized_relocatable_pointers")) break :blk T.serialized_relocatable_pointers;
            var n: usize = 0;
            inline for (s.fields) |f| n += relocatablePointerCount(f.type);
            break :blk n;
        },
        .array => |a| a.len * relocatablePointerCount(a.child),
        else => 0,
    };
}

/// Test helper: serialize `store` through a `CompactWriter` into a fresh
/// 16-byte-aligned buffer, then deserialize it back. Works for any store whose
/// `Serialized` form exposes `serialize(self, *const Store, gpa, *CompactWriter)`
/// and `deserialize(self, base) Store`. Caller frees `.buffer`.
pub fn roundTripForTest(gpa: Allocator, comptime Store: type, store: *const Store) !struct {
    buffer: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
    loaded: Store,
} {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();

    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, Store.Serialized);
    try hdr.serialize(store, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);

    const ser: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    return .{ .buffer = buffer, .loaded = ser.deserialize(@intFromPtr(buffer.ptr)) };
}

const testing = std.testing;

test "relocatablePointerCount: counts SerializedSlice/SerializedOptional fields, recurses, ignores PODs" {
    const Elem = enum(u32) { _ };
    const Key = extern struct { bytes: [8]u8 };

    // A bare slice = 1; a bare optional (its `slot` is a SerializedSlice) = 1.
    try testing.expectEqual(@as(usize, 1), relocatablePointerCount(SerializedSlice(Elem)));
    try testing.expectEqual(@as(usize, 1), relocatablePointerCount(SerializedOptional(Key)));

    // Plain POD scalars and integer fields contribute nothing.
    const Pod = extern struct { a: u32, b: u64, c: [4]u8 };
    try testing.expectEqual(@as(usize, 0), relocatablePointerCount(Pod));

    // A mixed header sums only the relocatable fields, including a nested one.
    const Nested = extern struct { inner: SerializedSlice(Elem), tag: u32 };
    const Header = extern struct {
        a: SerializedSlice(Elem),
        b: SerializedSlice(Key),
        n: u64,
        opt: SerializedOptional(Key),
        nested: Nested,
    };
    try testing.expectEqual(@as(usize, 4), relocatablePointerCount(Header));
}

test "SerializedSlice: round-trips a POD slice via CompactWriter" {
    const gpa = testing.allocator;
    const Elem = enum(u32) { _ };

    const Holder = extern struct {
        items: SerializedSlice(Elem),
    };

    const src = [_]Elem{ @enumFromInt(7), @enumFromInt(0), @enumFromInt(4242), @enumFromInt(1) };

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();

    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, Holder);
    try hdr.items.serialize(&src, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const loaded: *const Holder = @ptrCast(@alignCast(buffer.ptr));
    const got = loaded.items.deserialize(@intFromPtr(buffer.ptr));
    try testing.expectEqualSlices(Elem, &src, got);
}

test "SerializedOptional: present and absent round-trip" {
    const gpa = testing.allocator;
    const Key = extern struct { bytes: [8]u8 };
    const Holder = extern struct {
        some: SerializedOptional(Key) = .{},
        none: SerializedOptional(Key) = .{},
    };

    const present: ?Key = Key{ .bytes = .{ 1, 2, 3, 4, 5, 6, 7, 8 } };
    const absent: ?Key = null;

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();
    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, Holder);
    try hdr.some.serialize(&present, aa, &writer);
    try hdr.none.serialize(&absent, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const loaded: *const Holder = @ptrCast(@alignCast(buffer.ptr));
    const got_some = loaded.some.deserialize(@intFromPtr(buffer.ptr));
    try testing.expect(got_some != null);
    try testing.expectEqualSlices(u8, &present.?.bytes, &got_some.?.bytes);
    try testing.expectEqual(@as(?Key, null), loaded.none.deserialize(@intFromPtr(buffer.ptr)));
}

test "SerializedSlice: empty slice round-trips to empty (no spurious offset)" {
    const gpa = testing.allocator;
    const Holder = extern struct {
        items: SerializedSlice(u64),
    };
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();

    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, Holder);
    try hdr.items.serialize(&.{}, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const loaded: *const Holder = @ptrCast(@alignCast(buffer.ptr));
    const got = loaded.items.deserialize(@intFromPtr(buffer.ptr));
    try testing.expectEqual(@as(usize, 0), got.len);
}
