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

/// Reject an `= undefined` field default anywhere inside a serialized POD type. A
/// fixed-layout serialized element always writes every field's bytes, so an `undefined`
/// default leaks uninitialized memory into the blob — a non-deterministic, cache-poisoning
/// result. Reading the bytes of an `undefined` comptime default is illegal, so building
/// this for such a type fails with "use of undefined value" pointing at the offending
/// type. The fix is to give the field a zero/explicit default (e.g. `= .{}` or
/// `std.mem.zeroes(T)`), never `= undefined`.
pub fn assertSerializedDefaultsDefined(comptime T: type) void {
    comptime {
        @setEvalBranchQuota(1_000_000);
        if (@typeInfo(T) != .@"struct") return;
        for (@typeInfo(T).@"struct".fields) |field| {
            if (field.defaultValue()) |default| touchAllDefined(field.type, default);
        }
    }
}

/// Comptime-read every scalar reachable from `value`. A read of an `undefined` scalar is
/// comptime-illegal, which is how `assertSerializedDefaultsDefined` detects an undefined
/// default; defined values read harmlessly.
fn touchAllDefined(comptime T: type, comptime value: T) void {
    comptime {
        switch (@typeInfo(T)) {
            .bool, .int, .float, .@"enum" => _ = (value == value),
            .optional => |o| if (value) |inner| touchAllDefined(o.child, inner),
            .array => |a| for (value) |elem| touchAllDefined(a.child, elem),
            .@"struct" => |s| for (s.fields) |f| touchAllDefined(f.type, @field(value, f.name)),
            .@"union" => |u| if (u.tag_type != null) switch (value) {
                inline else => |payload| touchAllDefined(@TypeOf(payload), payload),
            },
            else => {},
        }
    }
}

/// Comptime guard that every field of a `Serialized` type is either a recognized
/// relocatable marker (a container that declares `serialized_relocatable_pointers`
/// and so knows how to fix its own base pointer on load) or a relocation-invariant
/// POD leaf/aggregate. The hazard this closes: a raw pointer or slice embedded
/// *directly* in a `Serialized` struct (outside a marker) is silently skipped by
/// `relocatablePointerCount` — a `.pointer` type falls through to its `else => 0`
/// arm — so it contributes no fixup and dangles after relocation. A marker is
/// exempt because it self-describes relocation and its element/payload type was
/// already validated by `assertRelocatablePod` where the marker was built.
///
/// Recurses through nested non-marker aggregates, so one call on the top-level
/// artifact `Serialized` validates the entire sub-store tree at compile time.
pub fn assertSerializedRelocatable(comptime T: type) void {
    comptime {
        switch (@typeInfo(T)) {
            .@"struct" => |s| {
                if (@hasDecl(T, "serialized_relocatable_pointers")) return;
                for (s.fields) |f| assertSerializedRelocatable(f.type);
            },
            .@"union" => |u| {
                for (u.fields) |f| assertSerializedRelocatable(f.type);
            },
            .array => |a| assertSerializedRelocatable(a.child),
            .optional => |o| assertSerializedRelocatable(o.child),
            .int, .float, .bool, .void, .@"enum", .error_set, .vector => {},
            .pointer => @compileError("Serialized type '" ++ @typeName(T) ++
                "' embeds a pointer/slice outside a relocatable marker; it would dangle after relocation. Wrap it in a SerializedSlice/SerializedOptional."),
            else => @compileError("Serialized type '" ++ @typeName(T) ++
                "' has a field with an unsupported (possibly non-relocatable) representation: " ++ @tagName(@typeInfo(T))),
        }
    }
}

/// Relocatable serialized form of a `[]T` of POD elements. Exactly one
/// relocatable base pointer (`offset`), regardless of `len`.
pub fn SerializedSlice(comptime T: type) type {
    comptime assertRelocatablePod(T);
    comptime assertSerializedDefaultsDefined(T);
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

        /// L-10: reject an `(offset, len)` that would `deserialize` into a slice
        /// reaching outside the `backing_len`-byte buffer (truncated/corrupt blob).
        pub fn validateRelocations(self: *const Self, backing_len: u64) error{CorruptArtifact}!void {
            try validateOffsetLen(@sizeOf(T), @alignOf(T), self.offset, self.len, backing_len);
        }
    };
}

/// Bounds/alignment check for a `len`-element relocatable slice (`SerializedSlice`):
/// computes the byte extent overflow-safe (so a corrupt huge `len` can't wrap) and
/// delegates to the shared `collections.validateRelocatedSpan` primitive, which the
/// `SafeList`-backed markers also use directly.
pub fn validateOffsetLen(elem_size: u64, elem_align: u64, offset: i64, len: u64, backing_len: u64) error{CorruptArtifact}!void {
    const bytes = std.math.mul(u64, len, elem_size) catch return error.CorruptArtifact;
    return collections.validateRelocatedSpan(elem_align, offset, bytes, backing_len);
}

/// True if `T` transitively embeds a relocatable marker (a type declaring
/// `serialized_relocatable_pointers`). Used by `validateSerialized` to reject a
/// marker hidden inside a union variant, where validation cannot pick the active
/// variant without a tag.
fn comptimeHasMarker(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .@"struct" => |s| blk: {
            if (@hasDecl(T, "serialized_relocatable_pointers")) break :blk true;
            inline for (s.fields) |f| {
                if (comptimeHasMarker(f.type)) break :blk true;
            }
            break :blk false;
        },
        .@"union" => |u| blk: {
            inline for (u.fields) |f| {
                if (comptimeHasMarker(f.type)) break :blk true;
            }
            break :blk false;
        },
        .array => |a| comptimeHasMarker(a.child),
        .optional => |o| comptimeHasMarker(o.child),
        else => false,
    };
}

/// L-10 whole-artifact validation pass: walk a relocated `Serialized` value and
/// bounds-check every relocatable marker's `(offset, len)` against `backing_len`
/// BEFORE any consumer dereferences it, so a truncated or corrupt blob produces a
/// clean `error.CorruptArtifact` rather than an out-of-bounds read. A marker (a type
/// declaring `serialized_relocatable_pointers`) self-validates via
/// `validateRelocations`; everything else is recursed into. Pure structural walk
/// driven by the type, mirroring `relocatablePointerCount`/`assertSerializedRelocatable`.
pub fn validateSerialized(comptime T: type, self: *const T, backing_len: u64) error{CorruptArtifact}!void {
    switch (@typeInfo(T)) {
        .@"struct" => |s| {
            if (@hasDecl(T, "serialized_relocatable_pointers")) {
                try self.validateRelocations(backing_len);
                return;
            }
            inline for (s.fields) |f| {
                try validateSerialized(f.type, &@field(self, f.name), backing_len);
            }
        },
        .array => |a| {
            for (self) |*elem| try validateSerialized(a.child, elem, backing_len);
        },
        .@"union" => {
            if (comptime comptimeHasMarker(T)) @compileError("Serialized union '" ++ @typeName(T) ++
                "' has a variant containing a relocatable marker; L-10 validation cannot pick the active variant without a tag. Restructure so the marker is a plain struct field.");
        },
        else => {},
    }
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

/// Binary-search `sorted` (ascending by key) using a 3-way `order` comparator that
/// ranks an element against the search key. Returns a pointer to the matching element,
/// or null. This is the single implementation behind the transform-D "sorted-KV pool
/// replaces an `AutoHashMap`" lookups (`MethodRegistry`, the dispatch-plan `(key,val)`
/// pools, the procedure-template lookup).
pub fn binarySearchByKey(
    comptime Elem: type,
    comptime Key: type,
    sorted: []const Elem,
    key: Key,
    comptime order: fn (Elem, Key) std.math.Order,
) ?*const Elem {
    var lo: usize = 0;
    var hi: usize = sorted.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        switch (order(sorted[mid], key)) {
            .eq => return &sorted[mid],
            .lt => lo = mid + 1,
            .gt => hi = mid,
        }
    }
    return null;
}

/// Append `items` to a flat side pool (`ArrayList`), returning their `(start, len)`
/// range as `RangeT` (a `start: u32, len: u32` struct — a store's named range or the
/// shared `Span`). This is the single implementation behind every transform-B "flatten
/// a slice into a shared pool" helper. An empty input appends nothing and returns the
/// canonical empty range `{ .start = 0, .len = 0 }` (a zero-length range never indexes
/// the pool, so its start is irrelevant — every consumer slices `pool[start..start+0]`).
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

/// Comptime byte-equality of two comptime strings. `std.mem.eql` is banned in
/// `src/check`, so field-name matching uses this manual loop.
fn comptimeStrEq(comptime a: []const u8, comptime b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

/// The store's `serialized: bool` frozen flag, if present. `deserialize` sets it to
/// `true`; it never appears in `Serialized` (it is build-only state).
fn frozenFlagField(comptime Store: type) ?[]const u8 {
    for (@typeInfo(Store).@"struct".fields) |f| {
        if (comptimeStrEq(f.name, "serialized") and f.type == bool) return "serialized";
    }
    return null;
}

/// The store's retained build-only `allocator: Allocator` field, if present. A store
/// with one keeps the load allocator for its build-only fields; its `deserialize`
/// takes the allocator as a parameter (`deserializeWithAllocator`).
fn allocatorField(comptime Store: type) ?[]const u8 {
    for (@typeInfo(Store).@"struct".fields) |f| {
        if (comptimeStrEq(f.name, "allocator") and f.type == std.mem.Allocator) return "allocator";
    }
    return null;
}

/// Build-only store fields that are NOT serialized, beyond the auto-detected frozen
/// flag and allocator (e.g. dedup hashmaps, scratch buffers, side-index tables). A
/// store lists them in a `pub const serde_transient_fields = [_][]const u8{...}` decl;
/// `deserialize` resets each to its struct default (so they must declare one). They
/// are excluded from the serialized set, and a store field that is neither serialized,
/// the flag, the allocator, nor listed here is a compile error — closing the
/// "forgot to serialize a data field" footgun the strict field set otherwise guards.
fn transientFields(comptime Store: type) []const []const u8 {
    if (@hasDecl(Store, "serde_transient_fields")) return &Store.serde_transient_fields;
    return &.{};
}

fn isTransientField(comptime Store: type, comptime name: []const u8) bool {
    for (transientFields(Store)) |t| {
        if (comptimeStrEq(t, name)) return true;
    }
    return false;
}

/// Comptime serialization framework for "transform-A" stores: a store backed by a
/// set of same-named `SerializedSlice`/`SerializedOptional`/nested-`Serialized`
/// fields. It generates `serialize`/`deserialize` by iterating the `Serialized`
/// fields, so the two can never drift apart when a field is added or removed — the
/// hand-written triplet's classic footgun (a missed line silently drops or misaligns
/// a field). A store opts in inside its `Serialized` extern struct:
///
///     pub const Serialized = extern struct {
///         foo: SerializedSlice(Foo) = .{},
///         bar: SerializedOptional(Bar) = .{},
///         const Serde = artifact_serialize.SliceStoreSerde(Store, @This());
///         pub const serialize = Serde.serialize;
///         pub const deserialize = Serde.deserialize;
///     };
///
/// Stores carrying extra runtime state are handled by reflection, not excluded:
///   * a `serialized: bool` frozen flag — auto-detected and set `true` on load;
///   * a retained `allocator: Allocator` — auto-detected; the store aliases
///     `deserialize = Serde.deserializeWithAllocator` and the allocator is injected;
///   * build-only side tables — declared in `serde_transient_fields` and reset to
///     their struct default on load;
///   * `ArrayList`-backed fields — adapted via `.items`/`arrayListFromSlice`.
/// Every store field must be one of: serialized (a `Serialized` field), the frozen
/// flag, the allocator, or a declared transient — otherwise a compile error fires, so
/// a data field accidentally left out of `Serialized` cannot silently vanish. `deinit`
/// stays hand-written per store: it encodes genuine per-store ownership (slice-free vs
/// list/interner deinit, frozen gating, allocator retention) that does not belong in
/// shared infra.
pub fn SliceStoreSerde(comptime Store: type, comptime Serialized: type) type {
    comptime {
        // Every Serialized field must map to a store field of the same name.
        for (@typeInfo(Serialized).@"struct".fields) |sf| {
            if (!@hasField(Store, sf.name)) @compileError(
                "SliceStoreSerde: Serialized field '" ++ sf.name ++ "' has no matching field in " ++ @typeName(Store),
            );
        }
        // Every store field must be accounted for: serialized, the frozen flag, the
        // allocator, or a declared transient. A forgotten data field is a compile error.
        const flag = frozenFlagField(Store);
        const alloc = allocatorField(Store);
        for (@typeInfo(Store).@"struct".fields) |f| {
            const persistent = @hasField(Serialized, f.name);
            const is_flag = flag != null and comptimeStrEq(f.name, flag.?);
            const is_alloc = alloc != null and comptimeStrEq(f.name, alloc.?);
            if (!persistent and !is_flag and !is_alloc and !isTransientField(Store, f.name)) @compileError(
                "SliceStoreSerde: store field '" ++ f.name ++ "' of " ++ @typeName(Store) ++
                    " is neither serialized, the frozen flag, the allocator, nor a declared transient" ++
                    " (add it to `Serialized` or `serde_transient_fields`).",
            );
        }
    }
    return struct {
        pub fn serialize(self: *Serialized, store: *const Store, gpa: std.mem.Allocator, writer: *CompactWriter) std.mem.Allocator.Error!void {
            inline for (@typeInfo(Serialized).@"struct".fields) |field| {
                // `SerializedSlice.serialize` takes the slice by value; `SerializedOptional`
                // and a nested `Serialized` take a pointer. Pick the form from the field's
                // own `serialize` signature so all shapes are handled uniformly, and pull the
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

        fn fill(self: *const Serialized, base_addr: usize, allocator: std.mem.Allocator) Store {
            const flag = comptime frozenFlagField(Store);
            const alloc = comptime allocatorField(Store);
            var store: Store = undefined;
            // Serialized (persistent) fields come straight from the relocated blob.
            // `SafeList.Serialized` names its buffer-aliasing loader `deserializeInto`;
            // `SerializedSlice`/`SerializedOptional`/nested interners use `deserialize`.
            // Pick whichever the marker provides.
            inline for (@typeInfo(Serialized).@"struct".fields) |field| {
                const value = if (comptime @hasDecl(field.type, "deserializeInto"))
                    @field(self, field.name).deserializeInto(base_addr)
                else if (comptime @hasDecl(field.type, "deserializeWithAllocator"))
                    @field(self, field.name).deserializeWithAllocator(base_addr, allocator)
                else
                    @field(self, field.name).deserialize(base_addr);
                if (comptime isArrayListType(@TypeOf(@field(store, field.name)))) {
                    @field(store, field.name) = arrayListFromSlice(@typeInfo(@TypeOf(value)).pointer.child, value);
                } else {
                    @field(store, field.name) = value;
                }
            }
            // The remaining (build-only) fields: the frozen flag is set, the retained
            // allocator is injected, and every declared transient resets to its default.
            inline for (@typeInfo(Store).@"struct".fields) |f| {
                if (comptime @hasField(Serialized, f.name)) continue;
                if (comptime flag != null and comptimeStrEq(f.name, flag.?)) {
                    @field(store, f.name) = true;
                } else if (comptime alloc != null and comptimeStrEq(f.name, alloc.?)) {
                    @field(store, f.name) = allocator;
                } else {
                    @field(store, f.name) = initTransient(@FieldType(Store, f.name), Store, f.name, allocator);
                }
            }
            return store;
        }

        /// Materialize a store whose build-only fields need no allocator.
        pub fn deserialize(self: *const Serialized, base_addr: usize) Store {
            return fill(self, base_addr, undefined);
        }

        /// Materialize a store that retains `allocator` for its build-only fields.
        pub fn deserializeWithAllocator(self: *const Serialized, base_addr: usize, allocator: std.mem.Allocator) Store {
            return fill(self, base_addr, allocator);
        }
    };
}

fn fieldDefaultValue(comptime Store: type, comptime name: []const u8) ?@FieldType(Store, name) {
    for (@typeInfo(Store).@"struct".fields) |f| {
        if (comptimeStrEq(f.name, name)) return f.defaultValue();
    }
    unreachable;
}

fn initTakesOnlyAllocator(comptime FT: type) bool {
    if (!@hasDecl(FT, "init")) return false;
    const info = @typeInfo(@TypeOf(FT.init));
    if (info != .@"fn") return false;
    const params = info.@"fn".params;
    return params.len == 1 and params[0].type == std.mem.Allocator;
}

/// Reset value for a transient store field on deserialize: its struct default if it
/// has one, else `FT.init(allocator)` for a build-only container that needs the load
/// allocator (e.g. a managed hashmap). A field with neither is a compile error.
fn initTransient(comptime FT: type, comptime Store: type, comptime name: []const u8, allocator: std.mem.Allocator) FT {
    const default = comptime fieldDefaultValue(Store, name);
    if (comptime default != null) {
        return comptime default.?;
    } else if (comptime initTakesOnlyAllocator(FT)) {
        return FT.init(allocator);
    } else {
        @compileError("SliceStoreSerde: transient field '" ++ name ++ "' of " ++ @typeName(Store) ++
            " needs a struct default (e.g. `= .{}`) or an `init(Allocator)` so deserialize can reset it.");
    }
}

/// True for `std.ArrayList(T)`: a struct with both `items` (a slice) and `capacity`.
/// Used by `SliceStoreSerde` to decide whether a store field needs `.items`/
/// `arrayListFromSlice` adapting versus being a plain slice.
fn isArrayListType(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "items") and @hasField(T, "capacity");
}

/// Errors that can occur in serialization round-trip tests.
pub const TestError = Allocator.Error || error{ BufferTooSmall, TestExpectedEqual };

/// Test helper: serialize `store` through a `CompactWriter` into a fresh
/// 16-byte-aligned buffer, then deserialize it back. Works for any store whose
/// `Serialized` form exposes `serialize(self, *const Store, gpa, *CompactWriter)`
/// and `deserialize(self, base) Store`. Caller frees `.buffer`.
pub fn roundTripForTest(gpa: Allocator, comptime Store: type, store: *const Store) TestError!struct {
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
pub fn expectSlicesByteEqual(comptime T: type, expected: []const T, actual: []const T) TestError!void {
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

test "validateOffsetLen: bounds, alignment, and overflow (L-10)" {
    // Empty span is always valid, even with a wild offset.
    try validateOffsetLen(4, 4, 9999, 0, 0);
    // In-bounds and exactly at the edge.
    try validateOffsetLen(4, 4, 0, 4, 16);
    try validateOffsetLen(4, 4, 8, 2, 16);
    // One element past the edge.
    try testing.expectError(error.CorruptArtifact, validateOffsetLen(4, 4, 8, 3, 16));
    // Negative offset, misaligned offset.
    try testing.expectError(error.CorruptArtifact, validateOffsetLen(4, 4, -1, 1, 16));
    try testing.expectError(error.CorruptArtifact, validateOffsetLen(4, 4, 2, 1, 16));
    // len * elem_size overflows u64.
    try testing.expectError(error.CorruptArtifact, validateOffsetLen(8, 1, 0, std.math.maxInt(u64), 64));
}

test "validateSerialized: walks markers and rejects out-of-bounds ranges (L-10)" {
    const Elem = enum(u32) { _ };
    const Composed = extern struct {
        a: SerializedSlice(Elem),
        b: SerializedSlice(Elem),
    };
    // Two 2-element u32 slices packed into a 16-byte buffer: a at [0,8), b at [8,16).
    var c = Composed{
        .a = .{ .offset = 0, .len = 2 },
        .b = .{ .offset = 8, .len = 2 },
    };
    try validateSerialized(Composed, &c, 16);
    // Truncating the buffer pushes b's extent past the bound.
    try testing.expectError(error.CorruptArtifact, validateSerialized(Composed, &c, 12));
    // A corrupt, impossibly-large len is rejected.
    c.b.len = std.math.maxInt(u64);
    try testing.expectError(error.CorruptArtifact, validateSerialized(Composed, &c, 16));
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

    // Reordering two same-size fields of a Serialized changes the hash (a naive
    // @typeName+@sizeOf fingerprint would not detect this).
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

test "SerializedSlice.serialize: padding-free elements are iovec'd verbatim (no copy buffer)" {
    const gpa = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();

    // A padding-free element type (u32). The serializer must NOT allocate a
    // writer-owned copy buffer for it — it iovecs the source directly. Measure the
    // `allocated_memory` delta across the slice serialize (the header `appendAlloc`
    // registers one entry up front, which is not what we're measuring).
    const PodHolder = extern struct { items: SerializedSlice(u32) = .{} };
    var pod_writer = CompactWriter.init();
    const pod_hdr = try pod_writer.appendAlloc(aa, PodHolder);
    const pod_data = [_]u32{ 10, 20, 30, 40 };
    const pod_before = pod_writer.allocated_memory.items.len;
    try pod_hdr.items.serialize(pod_data[0..], aa, &pod_writer);
    try testing.expectEqual(pod_before, pod_writer.allocated_memory.items.len);

    // A padded element type (1-byte field + 8-byte field → inter-field gap) DOES need a
    // zeroed copy, so the copy path registers exactly one buffer.
    const Padded = struct { a: u8, b: u64 };
    const PaddedHolder = extern struct { items: SerializedSlice(Padded) = .{} };
    var padded_writer = CompactWriter.init();
    const padded_hdr = try padded_writer.appendAlloc(aa, PaddedHolder);
    const padded_data = [_]Padded{ .{ .a = 1, .b = 2 }, .{ .a = 3, .b = 4 } };
    const padded_before = padded_writer.allocated_memory.items.len;
    try padded_hdr.items.serialize(padded_data[0..], aa, &padded_writer);
    try testing.expectEqual(padded_before + 1, padded_writer.allocated_memory.items.len);
}

fn serializeHolderToBuffer(
    gpa: Allocator,
    comptime Holder: type,
    items: anytype,
) TestError![]align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
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
