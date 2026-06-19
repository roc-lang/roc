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

        /// The element type whose bytes this slice serializes. Read by the layout
        /// fingerprint (`serializedLayoutFingerprint`) so a change to the element's
        /// field order/size — which changes the serialized bytes — is reflected in
        /// the version hash even though this container's own `{offset,len}` layout
        /// is unchanged.
        pub const SerializedElement = T;

        /// One relocatable base pointer (`offset`). Read by `relocatablePointerCount`.
        pub const serialized_relocatable_pointers: usize = 1;

        /// Append `slice`'s bytes to `writer` and record their offset/len. Uses
        /// `appendSlicePodZeroed` so element padding is zeroed — the serialized blob is
        /// byte-deterministic (reproducible builds; content-stable cache bodies).
        pub fn serialize(self: *Self, slice: []const T, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
            if (slice.len == 0) {
                self.* = .{};
                return;
            }
            const out = try writer.appendSlicePodZeroed(gpa, slice);
            self.offset = @intCast(@intFromPtr(out.ptr));
            self.len = out.len;
        }

        /// Materialize the slice pointing into the relocated buffer at `base`.
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

/// Recursively fold a `Serialized` type's complete relocation-relevant layout into
/// `hasher`. Unlike `@typeName` + `@sizeOf` (which miss a same-size field reorder
/// inside a nested `Serialized`, and never see a `SerializedSlice`'s element type),
/// this descends into nested aggregates AND into the element type of any container
/// that exposes `SerializedElement` (`SerializedSlice`, `SafeList.Serialized`, …).
/// A change anywhere that could make a previously-written blob deserialize into a
/// differently-shaped struct therefore changes the digest. Pure compile-time.
pub fn serializedLayoutFingerprint(comptime T: type, hasher: anytype) void {
    switch (@typeInfo(T)) {
        .@"struct" => |s| {
            // A container (SerializedSlice / SafeList.Serialized / …) fixes a
            // {offset,len[,capacity]} header but its *element* layout determines the
            // serialized bytes — fold the element type in so an element reorder is seen.
            if (@hasDecl(T, "SerializedElement")) {
                hasher.update("E<");
                serializedLayoutFingerprint(T.SerializedElement, hasher);
                hasher.update(">");
            }
            hasher.update("struct{");
            inline for (s.fields) |f| {
                hasher.update(f.name);
                hasher.update(":");
                serializedLayoutFingerprint(f.type, hasher);
                hasher.update(",");
            }
            hasher.update("}");
        },
        .@"union" => |u| {
            hasher.update("union{");
            inline for (u.fields) |f| {
                hasher.update(f.name);
                hasher.update(":");
                serializedLayoutFingerprint(f.type, hasher);
                hasher.update(",");
            }
            hasher.update("}");
        },
        .@"enum" => |e| {
            hasher.update("enum(");
            serializedLayoutFingerprint(e.tag_type, hasher);
            hasher.update(")");
        },
        .optional => |o| {
            hasher.update("?");
            serializedLayoutFingerprint(o.child, hasher);
        },
        .array => |a| {
            hasher.update(std.fmt.comptimePrint("[{d}]", .{a.len}));
            serializedLayoutFingerprint(a.child, hasher);
        },
        // Scalars and anything else: the name fully captures its size/representation.
        else => hasher.update(@typeName(T)),
    }
}

/// Comptime FNV-1a accumulator. Cheap enough to fold a large recursive layout
/// fingerprint at compile time without exhausting the eval branch quota (a full
/// SHA-256 over the whole artifact layout does — and cryptographic strength is
/// unnecessary for detecting *accidental* layout drift).
const LayoutHasher = struct {
    state: u64 = 0xcbf29ce484222325,
    fn update(self: *LayoutHasher, bytes: []const u8) void {
        for (bytes) |b| self.state = (self.state ^ b) *% 0x100000001b3;
    }
};

fn splitmix64(x: u64) u64 {
    var z = x +% 0x9E3779B97F4A7C15;
    z = (z ^ (z >> 30)) *% 0xBF58476D1CE4E5B9;
    z = (z ^ (z >> 27)) *% 0x94D049BB133111EB;
    return z ^ (z >> 31);
}

/// 256-bit layout-version digest of a `Serialized` type: a manual `version`
/// discriminant + the total relocatable-pointer count + the full recursive layout
/// fingerprint, folded with FNV-1a and expanded to 32 bytes via splitmix64. Same
/// role as `cache_module.MODULE_ENV_VERSION_HASH`; validated on load so a blob whose
/// layout differs from the running compiler's is rejected, not relocated into a
/// mismatched struct. Bump `version` for a semantic change the fingerprint can't see.
pub fn layoutVersionHash(comptime T: type, comptime version: u32) [32]u8 {
    @setEvalBranchQuota(20_000_000);
    var hasher = LayoutHasher{};
    hasher.update(std.fmt.comptimePrint("roc-serialized-v{d};fixups={d};", .{
        version,
        relocatablePointerCount(T),
    }));
    serializedLayoutFingerprint(T, &hasher);

    // Expand the 64-bit fold into 4 little-endian 64-bit lanes via splitmix64 so a
    // change anywhere in the fingerprint perturbs the whole 32-byte digest. Bytes are
    // emitted by hand (`std.mem.writeInt` is a banned `std.mem.*` op in src/check).
    var result: [32]u8 = undefined;
    var s = hasher.state;
    inline for (0..4) |lane| {
        s = splitmix64(s);
        inline for (0..8) |byte_i| {
            result[lane * 8 + byte_i] = @truncate(s >> (byte_i * 8));
        }
    }
    return result;
}

/// Wrap a relocated slice as an `ArrayList` view (`items = slice`, `capacity =
/// slice.len`). A deserialized store keeps its in-memory `ArrayList` field type, but
/// its frozen backing is buffer-owned and never grown; this is the one shared adapter
/// every sub-store's `deserialize` uses to re-form those fields.
pub fn arrayListFromSlice(comptime T: type, slice: []T) std.ArrayList(T) {
    return .{ .items = slice, .capacity = slice.len };
}

/// A `(start, len)` range into a flat pool. `appendSpan` callers that don't have their
/// own named range type return this.
pub const Span = struct { start: u32 = 0, len: u32 = 0 };

/// Append `items` to a flat side pool (`ArrayList`), returning their `(start, len)`
/// range as `RangeT` (a `start: u32, len: u32` struct — a store's named range or the
/// shared `Span`). This is the single implementation behind every transform-B "flatten
/// a slice into a shared pool" helper. An empty input appends nothing and yields a
/// zero-length range at the current end.
pub fn appendSpan(
    comptime RangeT: type,
    comptime T: type,
    pool: *std.ArrayList(T),
    allocator: std.mem.Allocator,
    items: []const T,
) std.mem.Allocator.Error!RangeT {
    if (items.len == 0) return .{ .start = 0, .len = 0 };
    const start: u32 = @intCast(pool.items.len);
    try pool.appendSlice(allocator, items);
    return .{ .start = start, .len = @intCast(items.len) };
}

/// Comptime serialization framework for "transform-A" stores: a store whose *every*
/// field is backed by a same-named `SerializedSlice`/`SerializedOptional`, with no
/// extra runtime state. It generates `serialize`/`deserialize` by iterating the
/// `Serialized` fields, so the two can never drift apart when a field is added or
/// removed — the hand-written triplet's classic footgun (a missed line silently
/// drops or misaligns a field). A store opts in inside its `Serialized` extern struct:
///
///     pub const Serialized = extern struct {
///         foo: SerializedSlice(Foo) = .{},
///         bar: SerializedOptional(Bar) = .{},
///         const Serde = artifact_serialize.SliceStoreSerde(Store, @This());
///         pub const serialize = Serde.serialize;
///         pub const deserialize = Serde.deserialize;
///     };
///
/// Stores that carry extra state (a `serialized` flag, an interner, build-only side
/// tables, or `ArrayList` fields needing `arrayListFromSlice`) keep a hand-written
/// triplet; the comptime field-count check below rejects them so the mixin is only
/// ever used where the whole store *is* its serialized fields.
pub fn SliceStoreSerde(comptime Store: type, comptime Serialized: type) type {
    comptime {
        const store_fields = @typeInfo(Store).@"struct".fields.len;
        const ser_fields = @typeInfo(Serialized).@"struct".fields.len;
        if (store_fields != ser_fields) @compileError(
            "SliceStoreSerde: " ++ @typeName(Store) ++ " has fields beyond its serialized" ++
                " ones — it carries extra runtime state and needs a hand-written triplet.",
        );
    }
    return struct {
        pub fn serialize(self: *Serialized, store: *const Store, gpa: std.mem.Allocator, writer: *CompactWriter) std.mem.Allocator.Error!void {
            inline for (@typeInfo(Serialized).@"struct".fields) |field| {
                // `SerializedSlice.serialize` takes the slice by value; `SerializedOptional`
                // takes a pointer to the optional. Pick the form from the field's own
                // `serialize` signature so both shapes are handled uniformly, and pull the
                // slice out of an `ArrayList`-backed store field via `.items`.
                const SourceParam = @typeInfo(@TypeOf(field.type.serialize)).@"fn".params[1].type.?;
                if (@typeInfo(SourceParam).pointer.size == .one) {
                    try @field(self, field.name).serialize(&@field(store, field.name), gpa, writer);
                } else if (comptime isArrayListType(@TypeOf(@field(store, field.name)))) {
                    try @field(self, field.name).serialize(@field(store, field.name).items, gpa, writer);
                } else {
                    try @field(self, field.name).serialize(@field(store, field.name), gpa, writer);
                }
            }
        }
        pub fn deserialize(self: *const Serialized, base_addr: usize) Store {
            var store: Store = undefined;
            inline for (@typeInfo(Serialized).@"struct".fields) |field| {
                const value = @field(self, field.name).deserialize(base_addr);
                if (comptime isArrayListType(@TypeOf(@field(store, field.name)))) {
                    @field(store, field.name) = arrayListFromSlice(@typeInfo(@TypeOf(value)).pointer.child, value);
                } else {
                    @field(store, field.name) = value;
                }
            }
            return store;
        }
    };
}

/// True for `std.ArrayList(T)`: a struct with both `items` (a slice) and `capacity`.
/// Used by `SliceStoreSerde` to decide whether a store field needs `.items`/
/// `arrayListFromSlice` adapting versus being a plain slice.
fn isArrayListType(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "items") and @hasField(T, "capacity");
}

/// Test helper: serialize `store` through a `CompactWriter` into a fresh
/// 16-byte-aligned buffer, then deserialize it back. Works for any store whose
/// `Serialized` form exposes `serialize(self, *const Store, gpa, *CompactWriter)`
/// and `deserialize(self, base) Store`. Caller frees `.buffer`.
pub fn roundTripForTest(gpa: Allocator, comptime Store: type, store: *const Store) anyerror!struct {
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

/// Test helper: byte-fill a POD slice with a poison pattern (every byte, including
/// struct padding). Done per-element via `asBytes` because the slice-wide
/// `std.mem.sliceAsBytes` is a banned raw-byte op in `src/check`.
pub fn poisonSlice(comptime T: type, buf: []T, byte: u8) void {
    for (buf) |*elem| @memset(std.mem.asBytes(elem), byte);
}

/// Test helper: zero each element's padding, matching what the deterministic
/// serializer (`appendSlicePodZeroed`) writes. A byte-fidelity round-trip test
/// poisons a source then canonicalizes it with this, so the comparison reflects
/// logical fidelity rather than padding bytes (which serialization intentionally
/// zeroes, not preserves).
pub fn zeroSlicePadding(comptime T: type, buf: []T) void {
    for (buf) |*elem| CompactWriter.zeroValuePadding(T, @as([*]u8, @ptrCast(elem)));
}

/// Test helper: assert two POD slices are byte-identical (including padding),
/// element by element via `asBytes` (avoiding the banned `sliceAsBytes`).
pub fn expectSlicesByteEqual(comptime T: type, expected: []const T, actual: []const T) anyerror!void {
    try std.testing.expectEqual(expected.len, actual.len);
    for (expected, actual) |*e, *a| {
        try std.testing.expectEqualSlices(u8, std.mem.asBytes(e), std.mem.asBytes(a));
    }
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

test "relocatablePointerCount: counts SafeList.Serialized base pointers and sums recursively" {
    const SafeList = collections.SafeList;
    const Elem = enum(u32) { _ };

    // A SafeList.Serialized is one relocatable base pointer (its marker decl).
    try testing.expectEqual(@as(usize, 1), relocatablePointerCount(SafeList(u8).Serialized));

    // An interner-shaped header (3 SafeLists + POD counters) = 3.
    const InternerLike = extern struct {
        bytes: SafeList(u8).Serialized,
        ranges: SafeList(Elem).Serialized,
        index: SafeList(u32).Serialized,
        entry_count: u32,
        _padding: u32,
    };
    try testing.expectEqual(@as(usize, 3), relocatablePointerCount(InternerLike));

    // A store composed of slices + a nested interner sums to the true total — the
    // invariant that lets the artifact assert its full constant fixup count.
    const Composed = extern struct {
        a: SerializedSlice(Elem),
        interner: InternerLike,
        b: SerializedSlice(Elem),
    };
    try testing.expectEqual(@as(usize, 5), relocatablePointerCount(Composed));
}

fn versionHashesEqual(a: [32]u8, b: [32]u8) bool {
    // Local byte compare: `std.mem.eql` is banned in src/check.
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

test "layoutVersionHash: flips on nested reorder, element reorder, and version bump" {
    const SafeList = collections.SafeList;
    const Elem = enum(u32) { _ };

    // `layoutVersionHash` reflects over types + uses comptimePrint, so it is
    // comptime-only (exactly as it runs for the real SERIALIZED_VERSION_HASH const).
    const A = extern struct { x: SerializedSlice(Elem), y: SerializedSlice(Elem) };
    const B = extern struct { y: SerializedSlice(Elem), x: SerializedSlice(Elem) };
    const E1 = extern struct { p: u32, q: u64 };
    const E2 = extern struct { q: u64, p: u32 };
    const C1 = extern struct { s: SerializedSlice(E1) };
    const C2 = extern struct { s: SerializedSlice(E2) };
    const L1 = extern struct { s: SafeList(E1).Serialized };
    const L2 = extern struct { s: SafeList(E2).Serialized };

    const hA1 = comptime layoutVersionHash(A, 1);
    const hB1 = comptime layoutVersionHash(B, 1);
    const hA2 = comptime layoutVersionHash(A, 2);
    const hC1 = comptime layoutVersionHash(C1, 1);
    const hC2 = comptime layoutVersionHash(C2, 1);
    const hL1 = comptime layoutVersionHash(L1, 1);
    const hL2 = comptime layoutVersionHash(L2, 1);

    // Reordering two same-size fields of a Serialized changes the hash (the bug the
    // old @typeName+@sizeOf hash missed).
    try testing.expect(!versionHashesEqual(hA1, hB1));
    // A version-discriminant bump changes the hash.
    try testing.expect(!versionHashesEqual(hA1, hA2));
    // Reordering the ELEMENT type's fields changes the hash even though the
    // container's own {offset,len} layout is identical — captured via SerializedElement.
    try testing.expect(!versionHashesEqual(hC1, hC2));
    // Same for SafeList-backed element layout.
    try testing.expect(!versionHashesEqual(hL1, hL2));
    // Identical layout → identical hash (determinism).
    try testing.expect(versionHashesEqual(hA1, comptime layoutVersionHash(A, 1)));
}

test "SerializedSlice: round-trips a POD slice via CompactWriter" {
    const gpa = testing.allocator;
    const Elem = enum(u32) { _ };

    const Holder = extern struct {
        items: SerializedSlice(Elem),
    };

    const src = [_]Elem{ @enumFromInt(7), @enumFromInt(5), @enumFromInt(4242), @enumFromInt(1) };

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

test "SerializedSlice.serialize zeroes element padding (deterministic bytes)" {
    const gpa = testing.allocator;
    // Auto-layout struct with a 7-byte inter-field gap after `a`.
    const Padded = struct { a: u8, b: u64 };
    const Holder = extern struct { items: SerializedSlice(Padded) };

    // Two sources with IDENTICAL logical values but DIFFERENT padding garbage: poison
    // each, then set only the fields (a full struct assignment would re-poison the
    // padding from the literal's undefined gap). A non-deterministic serializer would
    // emit the differing padding; the deterministic one zeroes it, so both blobs match.
    var src_a = [_]Padded{ undefined, undefined };
    var src_b = [_]Padded{ undefined, undefined };
    poisonSlice(Padded, &src_a, 0xAA);
    poisonSlice(Padded, &src_b, 0x55);
    for (&src_a) |*e| {
        e.a = 7;
        e.b = 99;
    }
    for (&src_b) |*e| {
        e.a = 7;
        e.b = 99;
    }

    const buf_a = try serializeHolderToBuffer(gpa, Holder, &src_a);
    defer gpa.free(buf_a);
    const buf_b = try serializeHolderToBuffer(gpa, Holder, &src_b);
    defer gpa.free(buf_b);

    // Byte-identical despite the differing source padding.
    try testing.expectEqual(buf_a.len, buf_b.len);
    for (buf_a, buf_b) |x, y| try testing.expectEqual(x, y);
}

fn serializeHolderToBuffer(
    gpa: Allocator,
    comptime Holder: type,
    items: anytype,
) anyerror![]align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();
    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, Holder);
    try hdr.items.serialize(items, aa, &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);
    return buffer;
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
