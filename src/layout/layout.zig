//! Memory layout representations for values in running Roc programs.
//!
//! See the Layout Store for how these representations actually get created
//! (using type and target information from previous steps in compilation).

const std = @import("std");
const base = @import("base");
const types = @import("types");
const collections = @import("collections");

const CIR = @import("can").CIR;

pub const store = @import("store.zig");

const target = base.target;

/// Tag for Layout variants
pub const LayoutTag = enum(u4) {
    scalar,
    box,
    box_of_zst, // Box of a zero-sized type, e.g. Box({}) - needs a special-cased runtime implementation
    list,
    list_of_zst, // List of zero-sized types, e.g. List({}) - needs a special-cased runtime implementation
    struct_, // Unified struct layout for both records and tuples (fields stable-sorted by alignment)
    closure,
    erased_callable, // Refcounted boxed erased function payload: header + inline capture bytes
    zst, // Zero-sized type (empty records, empty tuples, phantom types, etc.)
    tag_union, // Tag union with variant-specific layouts for proper refcounting
    ptr, // Compiler-internal pointer to a value of the element layout; never refcounted.
    // Introduced by the TRMC pass (see src/lir/trmc.zig); never a struct field,
    // tag payload, or list element. The pass author must uphold those invariants.
};

/// The Layout untagged union should take up this many bits in memory.
/// We verify this with a test, and make use of it to calculate Idx sizes.
const layout_bit_size = 32;

/// Tag for scalar variants
///
/// The exact numbers here are important, because we use them to convert between
/// Scalar and Idx using branchless arithmetic instructions. Don't change them
/// lightly, and make sure to re-run tests if you do!
pub const ScalarTag = enum(u3) {
    str = 0, // Maps to Idx 1
    int = 1, // Maps to Idx 2-11 (depending on precision)
    frac = 2, // Maps to Idx 12-14 (depending on precision)
    opaque_ptr = 3, // Maps to Idx 15
    vector = 4, // Maps to Idx 17-24 (depending on lane kind)
};

/// Lane interpretation for a fixed-width 128-bit integer SIMD value.
pub const Vector = enum(u3) {
    u8x16,
    i8x16,
    u16x8,
    i16x8,
    u32x4,
    i32x4,
    u64x2,
    i64x2,

    pub fn laneBits(self: Vector) u8 {
        return switch (self) {
            .u8x16, .i8x16 => 8,
            .u16x8, .i16x8 => 16,
            .u32x4, .i32x4 => 32,
            .u64x2, .i64x2 => 64,
        };
    }

    pub fn laneCount(self: Vector) u8 {
        return 128 / self.laneBits();
    }

    pub fn isSigned(self: Vector) bool {
        return switch (self) {
            .i8x16, .i16x8, .i32x4, .i64x2 => true,
            .u8x16, .u16x8, .u32x4, .u64x2 => false,
        };
    }

    pub fn lanePrecision(self: Vector) types.Int.Precision {
        return switch (self) {
            .u8x16 => .u8,
            .i8x16 => .i8,
            .u16x8 => .u16,
            .i16x8 => .i16,
            .u32x4 => .u32,
            .i32x4 => .i32,
            .u64x2 => .u64,
            .i64x2 => .i64,
        };
    }
};

/// Raw backing for scalar data (largest payload is Int.Precision = u4).
/// In Zig 0.16, packed unions require uniform field widths, so we use
/// a raw integer with typed accessors instead.
pub const ScalarData = u4;

/// A scalar value such as a str, int, or frac.
/// Uses the Zig 0.16 pattern of packed struct with raw data + typed accessors.
pub const Scalar = packed struct {
    data: ScalarData,
    tag: ScalarTag,
    _pad: u21 = 0,

    pub fn getInt(self: Scalar) types.Int.Precision {
        return @enumFromInt(self.data);
    }

    pub fn getFrac(self: Scalar) types.Frac.Precision {
        return @enumFromInt(@as(u3, @truncate(self.data)));
    }

    pub fn getVector(self: Scalar) Vector {
        return @enumFromInt(@as(u3, @truncate(self.data)));
    }

    pub fn initStr() Scalar {
        return .{ .data = 0, .tag = .str };
    }

    pub fn initInt(precision: types.Int.Precision) Scalar {
        return .{ .data = @intFromEnum(precision), .tag = .int };
    }

    pub fn initFrac(precision: types.Frac.Precision) Scalar {
        return .{ .data = @intFromEnum(precision), .tag = .frac };
    }

    pub fn initVector(vector: Vector) Scalar {
        return .{ .data = @intFromEnum(vector), .tag = .vector };
    }
};

/// Index into a Layout Store
pub const Idx = enum(std.meta.Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag))) {
    // Sentinel values for scalar builtin layouts. When we init the layout store, it automatically
    // adds entries for each of these at an index equal to the enum's value. That way, if you
    // look up one of these in the store, it's always returns the correct layout, and we can have
    // any type that resolves to one of these layouts use one of these hardcoded ones instead
    // of adding redundant layouts to the store.
    //
    // The layout store's idxFromScalar method relies on these exact numbers being what they are now,
    // so be careful when changing them! (Changing them will, at a minimum, cause tests to fail.)
    bool = 0,
    str = 1,

    // ints
    u8 = 2,
    i8 = 3,
    u16 = 4,
    i16 = 5,
    u32 = 6,
    i32 = 7,
    u64 = 8,
    i64 = 9,
    u128 = 10,
    i128 = 11,

    // fracs
    f32 = 12,
    f64 = 13,
    dec = 14,

    // opaque pointer
    opaque_ptr = 15,

    // zero-sized type
    zst = 16,

    // 128-bit integer SIMD vectors
    u8x16 = 17,
    i8x16 = 18,
    u16x8 = 19,
    i16x8 = 20,
    u32x4 = 21,
    i32x4 = 22,
    u64x2 = 23,
    i64x2 = 24,

    // Regular indices start from here.
    // num_primitives in store.zig must refer to how many variants we had up to this point.
    _,

    /// Sentinel value representing "not present" / "no layout".
    /// Used by ArrayListMap as the empty slot marker.
    pub const none: Idx = @enumFromInt(std.math.maxInt(@typeInfo(Idx).@"enum".tag_type));

    /// Returns true if this layout represents a signed integer type.
    /// Used for determining signed vs unsigned operations (sdiv vs udiv, etc.)
    pub fn isSigned(self: Idx) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128, .dec, .i8x16, .i16x8, .i32x4, .i64x2 => true,
            .u8, .u16, .u32, .u64, .u128, .u8x16, .u16x8, .u32x4, .u64x2 => false,
            // Default to signed for other types (floats don't use this, bools are unsigned)
            else => true,
        };
    }

    /// Default numeric type for unbound/polymorphic numbers.
    /// Dec is the default in the new Roc compiler.
    pub const default_num: Idx = .dec;
};

/// Represents a closure with its captured environment
pub const Closure = struct {
    body_idx: CIR.Expr.Idx,
    params: CIR.Pattern.Span,
    captures_pattern_idx: CIR.Pattern.Idx,
    // Layout index for the captured environment record
    captures_layout_idx: Idx,
    // Original lambda expression index for accessing captures
    lambda_expr_idx: CIR.Expr.Idx,
    // Module environment where this closure was created (for correct expression evaluation)
    source_env: *const @import("can").ModuleEnv,
};

/// Raw backing type for the Layout data (28 bits).
/// In Zig 0.16, packed unions require uniform field widths, so we use
/// a raw integer with typed accessors on the Layout struct instead.
pub const LayoutData = std.meta.Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag));

/// Unified struct field layout — used for both records and tuples at the layout level.
/// At the shared LIR/layout commit, records and tuples become contiguous fields that are
/// stable-sorted by descending alignment.
/// The `index` field stores the canonical semantic field index:
///   - For records: alphabetical closed-record field order
///   - For tuples: the original tuple element index (e.g. .0, .1, .2)
/// Equal-alignment fields preserve that earlier semantic order.
pub const StructField = struct {
    /// The canonical semantic index of this field before layout sorting.
    index: u16,
    /// The layout of the field's value
    layout: Idx,
    /// True for unnamed nominal-record padding spacers. Such a field reserves
    /// `sizeof(layout)` bytes at alignment 1 (its layout's own alignment is
    /// ignored) and is excluded from every semantic field operation — name
    /// resolution, equality, refcounting, inspect, glue, and construction — while
    /// still occupying its bytes for offsets and total struct size. It never
    /// contributes its alignment to the struct.
    is_padding: bool = false,

    /// A SafeMultiList for storing struct fields
    pub const SafeMultiList = collections.SafeMultiList(StructField);
};

/// Backwards-compat aliases so existing code that references the old names still compiles.
/// Callers will be migrated incrementally.
pub const RecordField = StructField;
/// Backwards-compat alias for `StructField`.
pub const TupleField = StructField;
/// Backwards-compat alias for `StructField`.
pub const TupleFieldLayout = StructField;

/// Struct layout - stores the target-independent alignment class (`SortKey`) and
/// an index to full data in Store. Unified representation for records and tuples.
/// The actual alignment in bytes is derived per target via `SortKey.alignment`.
pub const StructLayout = packed struct {
    /// Target-independent alignment class of the struct.
    sort_key: SortKey,
    /// Index into the Store's struct data
    idx: StructIdx,
};

/// Backwards-compat alias for `StructLayout`.
pub const RecordLayout = StructLayout;
/// Backwards-compat alias for `StructLayout`.
pub const TupleLayout = StructLayout;

/// Index into the Store's struct data
pub const StructIdx = packed struct {
    int_idx: std.meta.Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag) - @bitSizeOf(SortKey)),
};

/// Backwards-compat alias for `StructIdx`.
pub const RecordIdx = StructIdx;
/// Backwards-compat alias for `StructIdx`.
pub const TupleIdx = StructIdx;

/// A byte value precomputed for both pointer widths, keeping the layout store
/// target-independent: both targets' values are stored and the right one is a
/// direct array read once the target is known (no per-read computation). Indexed
/// by `@intFromEnum(TargetUsize)` — `[0]` is the 32-bit target, `[1]` the 64-bit.
/// Computing both directly (rather than a pointer count) accounts for the
/// pointer-width-dependent alignment padding exactly.
pub fn WidthValues(comptime T: type) type {
    return struct {
        per_target: [2]T,

        pub fn get(self: @This(), target_usize: target.TargetUsize) T {
            return self.per_target[@intFromEnum(target_usize)];
        }

        pub fn both(value_for_u32: T, value_for_u64: T) @This() {
            return .{ .per_target = .{ value_for_u32, value_for_u64 } };
        }
    };
}

/// Struct data stored in the layout Store — unified for records and tuples.
pub const StructData = struct {
    /// Size of the struct in bytes, precomputed for both pointer widths.
    size: WidthValues(u32),
    /// Range of fields in the struct_fields list
    fields: collections.NonEmptyRange,
    /// Whether this struct transitively contains refcounted data. Precomputed
    /// when the struct is committed (its field layouts are already committed by
    /// then) so `Store.layoutContainsRefcounted` is an O(1), infallible lookup.
    contains_refcounted: bool = false,

    pub fn getFields(self: StructData) StructField.SafeMultiList.Range {
        // Handle empty structs specially - NonEmptyRange.toRange() asserts count > 0
        if (self.fields.count == 0) {
            return StructField.SafeMultiList.Range.empty();
        }
        return self.fields.toRange(StructField.SafeMultiList.Idx);
    }
};

/// Backwards-compat alias for `StructData`.
pub const RecordData = StructData;
/// Backwards-compat alias for `StructData`.
pub const TupleData = StructData;

/// Closure layout - stores captures layout index
pub const ClosureLayout = packed struct {
    /// Layout index of the captured environment
    captures_layout_idx: Idx,
};

/// Tag union layout - stores the target-independent alignment class (`SortKey`)
/// and an index to full data in Store. This preserves variant information needed
/// for correct reference counting. Actual alignment is derived per target.
pub const TagUnionLayout = packed struct {
    /// Target-independent alignment class of the tag union
    sort_key: SortKey,
    /// Index into the Store's tag union data
    idx: TagUnionIdx,
};

/// Index into the Store's tag union data
pub const TagUnionIdx = packed struct {
    int_idx: std.meta.Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag) - @bitSizeOf(SortKey)),
};

/// Tag union data stored in the layout Store
pub const TagUnionData = struct {
    /// Size of the tag union in bytes (max payload + discriminant, aligned),
    /// precomputed for both pointer widths.
    size: WidthValues(u32),
    /// Offset of the discriminant within the union (after the payload),
    /// precomputed for both pointer widths.
    discriminant_offset: WidthValues(u16),
    /// Size of the discriminant in bytes (0, 1, 2, 4, or 8).
    /// A size of 0 means the tag union has exactly one variant, so the
    /// discriminant is implicit and always 0.
    discriminant_size: u8,
    /// Range of variants in the tag_union_variants list
    variants: collections.NonEmptyRange,
    /// Whether this tag union transitively contains refcounted data. Precomputed
    /// at commit time so `Store.layoutContainsRefcounted` is an O(1) lookup.
    contains_refcounted: bool = false,

    pub fn getVariants(self: TagUnionData) TagUnionVariant.SafeMultiList.Range {
        return self.variants.toRange(TagUnionVariant.SafeMultiList.Idx);
    }

    /// Read the discriminant value from memory at the given base pointer.
    /// Adds the discriminant offset (for the given target) internally.
    pub fn readDiscriminant(self: TagUnionData, base_ptr: [*]const u8, target_usize: target.TargetUsize) u32 {
        if (self.discriminant_size == 0) return 0;
        return self.readDiscriminantFromPtr(base_ptr + self.discriminant_offset.get(target_usize));
    }

    /// Read the discriminant value from a pointer already at the discriminant location.
    /// Use this when you have a pre-computed discriminant pointer (e.g., from getTagUnionDiscriminantOffset).
    pub fn readDiscriminantFromPtr(self: TagUnionData, disc_ptr: [*]const u8) u32 {
        return switch (self.discriminant_size) {
            0 => 0,
            1 => disc_ptr[0],
            2 => @as(u32, disc_ptr[0]) | (@as(u32, disc_ptr[1]) << 8),
            4 => @as(u32, disc_ptr[0]) | (@as(u32, disc_ptr[1]) << 8) | (@as(u32, disc_ptr[2]) << 16) | (@as(u32, disc_ptr[3]) << 24),
            8 => @as(u32, disc_ptr[0]) | (@as(u32, disc_ptr[1]) << 8) | (@as(u32, disc_ptr[2]) << 16) | (@as(u32, disc_ptr[3]) << 24), // truncate to u32
            else => unreachable, // discriminant_size is 0, 1, 2, 4, or 8
        };
    }

    /// Write a discriminant value to memory at the given base pointer.
    /// Adds the discriminant offset (for the given target) internally.
    pub fn writeDiscriminant(self: TagUnionData, base_ptr: [*]u8, value: u32, target_usize: target.TargetUsize) void {
        if (self.discriminant_size == 0) return;
        self.writeDiscriminantToPtr(base_ptr + self.discriminant_offset.get(target_usize), value);
    }

    /// Write a discriminant value to a pointer already at the discriminant location.
    /// Use this when you have a pre-computed discriminant pointer (e.g., from getTagUnionDiscriminantOffset).
    pub fn writeDiscriminantToPtr(self: TagUnionData, disc_ptr: [*]u8, value: u32) void {
        switch (self.discriminant_size) {
            0 => {},
            1 => disc_ptr[0] = @intCast(value),
            2 => {
                disc_ptr[0] = @intCast(value & 0xFF);
                disc_ptr[1] = @intCast((value >> 8) & 0xFF);
            },
            4 => {
                disc_ptr[0] = @intCast(value & 0xFF);
                disc_ptr[1] = @intCast((value >> 8) & 0xFF);
                disc_ptr[2] = @intCast((value >> 16) & 0xFF);
                disc_ptr[3] = @intCast((value >> 24) & 0xFF);
            },
            8 => {
                disc_ptr[0] = @intCast(value & 0xFF);
                disc_ptr[1] = @intCast((value >> 8) & 0xFF);
                disc_ptr[2] = @intCast((value >> 16) & 0xFF);
                disc_ptr[3] = @intCast((value >> 24) & 0xFF);
                disc_ptr[4] = 0;
                disc_ptr[5] = 0;
                disc_ptr[6] = 0;
                disc_ptr[7] = 0;
            },
            else => unreachable, // discriminant_size is 0, 1, 2, 4, or 8
        }
    }

    /// Get the alignment requirement for this discriminant.
    pub fn discriminantAlignment(self: TagUnionData) std.mem.Alignment {
        return alignmentForDiscriminantSize(self.discriminant_size);
    }

    /// Get the alignment requirement for a given discriminant size.
    /// Can be called before a TagUnionData is created.
    pub fn alignmentForDiscriminantSize(size: u8) std.mem.Alignment {
        return switch (size) {
            0 => .@"1",
            1 => .@"1",
            2 => .@"2",
            4 => .@"4",
            8 => .@"8",
            else => unreachable, // discriminant_size is 0, 1, 2, 4, or 8
        };
    }

    /// Compute the discriminant size in bytes from a variant count.
    /// Can be called before a TagUnionData is created.
    ///
    /// A single-variant tag union has an implicit discriminant (the tag is
    /// statically known), so it reserves zero discriminant bytes in memory.
    /// This is the width committed into `TagUnionData.discriminant_size`, so
    /// it is the width every backend and glue read back for the layout.
    pub fn discriminantSize(variant_count: usize) u8 {
        return if (variant_count <= 1) 0 else if (variant_count <= 256) 1 else if (variant_count <= 65536) 2 else if (variant_count <= (1 << 32)) 4 else 8;
    }

    /// Get the integer precision for this discriminant (always unsigned).
    pub fn discriminantPrecision(self: TagUnionData) types.Int.Precision {
        return precisionForDiscriminantSize(self.discriminant_size);
    }

    /// Get the integer precision for a given discriminant size (always unsigned).
    /// Can be called before a TagUnionData is created.
    pub fn precisionForDiscriminantSize(size: u8) types.Int.Precision {
        return switch (size) {
            0 => .u8,
            1 => .u8,
            2 => .u16,
            4 => .u32,
            8 => .u64,
            else => unreachable, // discriminant_size is 0, 1, 2, 4, or 8
        };
    }
};

/// Per-variant information for tag unions
pub const TagUnionVariant = struct {
    /// The layout of this variant's payload
    payload_layout: Idx,

    /// A SafeMultiList for storing tag union variants
    pub const SafeMultiList = collections.SafeMultiList(TagUnionVariant);
};

/// Roc's version of alignment that is limited to a max alignment of 16B to save bits.
pub const RocAlignment = enum(u3) {
    @"1" = 0,
    @"2" = 1,
    @"4" = 2,
    @"8" = 3,
    @"16" = 4,
    _,

    pub fn toByteUnits(a: RocAlignment) usize {
        return @as(usize, 1) << @intFromEnum(a);
    }

    pub fn fromByteUnits(n: u16) RocAlignment {
        std.debug.assert(std.math.isPowerOfTwo(n));
        return @enumFromInt(@ctz(n));
    }
};

/// Target-independent key for ordering aggregate fields by alignment.
///
/// This is the target-independent alignment class of a value: it both orders
/// fields and (given a target) yields the actual alignment in bytes. A pointer
/// is its own class, sorting strictly between 4-byte and 8-byte alignment —
/// because a pointer is the only type whose real alignment varies by target
/// (4 bytes on a 32-bit target, 8 on a 64-bit one), its fixed slot between
/// `align_4` and `align_8` makes a record/tuple's field order identical on both
/// targets and lets the alignment be stored target-independently and resolved to
/// bytes only when a target is known (see `alignment`). This is the foundation
/// for caching layout across pointer widths.
pub const SortKey = enum(u3) {
    align_1 = 0,
    align_2 = 1,
    align_4 = 2,
    pointer = 3,
    align_8 = 4,
    align_16 = 5,

    /// Sort key for a fixed (non-pointer) power-of-two alignment in bytes.
    pub fn fromAlignBytes(bytes: u64) SortKey {
        return switch (bytes) {
            1 => .align_1,
            2 => .align_2,
            4 => .align_4,
            8 => .align_8,
            16 => .align_16,
            else => unreachable, // alignments are powers of two up to 16
        };
    }

    /// The actual alignment in bytes for a given target. A `.pointer` resolves to
    /// the target's pointer alignment; every other class is target-independent.
    pub fn alignment(self: SortKey, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self) {
            .align_1 => .@"1",
            .align_2 => .@"2",
            .align_4 => .@"4",
            .pointer => target_usize.alignment(),
            .align_8 => .@"8",
            .align_16 => .@"16",
        };
    }

    /// The greater of two sort keys (used to fold an aggregate's children).
    pub fn max(a: SortKey, b: SortKey) SortKey {
        return if (@intFromEnum(a) >= @intFromEnum(b)) a else b;
    }

    /// Whether `a` sorts before `b` in the field order (higher key first).
    pub fn sortsBefore(a: SortKey, b: SortKey) bool {
        return @intFromEnum(a) > @intFromEnum(b);
    }
};

/// Size and alignment information
pub const SizeAlign = packed struct(u32) {
    size: u29, // u29 can represent sizes up to ~1GiB (is 1 byte shy of it).
    alignment: RocAlignment, // u3 bits

    /// Box size and alignment (pointer-sized)
    pub const box = SizeAlign{
        .size = @sizeOf(usize),
        .alignment = RocAlignment.fromByteUnits(@alignOf(usize)),
    };

    /// List size and alignment (3 pointer-sized fields)
    pub const list = SizeAlign{
        .size = 3 * @sizeOf(usize),
        .alignment = RocAlignment.fromByteUnits(@alignOf(usize)),
    };
};

test "Size of SizeAlign type" {
    try std.testing.expectEqual(32, @bitSizeOf(SizeAlign));
}

/// Bundled information about a list's element layout
pub const ListInfo = struct {
    elem_layout_idx: Idx,
    elem_layout: Layout,
    elem_size: u32,
    elem_alignment: u32,
    contains_refcounted: bool,

    /// Iterator for traversing list elements with proper pointer arithmetic.
    /// Use iterateElements() to create one.
    pub const ElementIterator = struct {
        base: [*]u8,
        elem_size: usize,
        elem_layout: Layout,
        count: usize,
        idx: usize = 0,

        /// Get the next element pointer and advance the iterator.
        /// Returns null when all elements have been visited.
        pub fn next(self: *ElementIterator) ?[*]u8 {
            if (self.idx >= self.count) return null;
            const ptr = self.base + self.idx * self.elem_size;
            self.idx += 1;
            return ptr;
        }

        /// Reset the iterator to the beginning.
        pub fn reset(self: *ElementIterator) void {
            self.idx = 0;
        }

        /// Get remaining element count.
        pub fn remaining(self: ElementIterator) usize {
            return self.count - self.idx;
        }
    };

    /// Create an iterator for traversing list elements.
    /// The caller should obtain base_ptr and count from RocList methods:
    ///   - base_ptr from list.getAllocationDataPtr(ops)
    ///   - count from list.getAllocationElementCount(self.contains_refcounted, ops)
    pub fn iterateElements(self: ListInfo, base_ptr: [*]u8, count: usize) ElementIterator {
        return ElementIterator{
            .base = base_ptr,
            .elem_size = self.elem_size,
            .elem_layout = self.elem_layout,
            .count = count,
        };
    }
};

/// Bundled information about a box's element layout
pub const BoxInfo = struct {
    elem_layout_idx: Idx,
    elem_layout: Layout,
    elem_size: u32,
    elem_alignment: u32,
    contains_refcounted: bool,
};

/// Bundled information about a struct layout (unified for records and tuples)
pub const StructInfo = struct {
    data: *const StructData,
    alignment: std.mem.Alignment,
    /// Size in bytes, resolved for the store's target.
    byte_size: u32,
    fields: StructField.SafeMultiList.Slice,
    contains_refcounted: bool,

    pub fn size(self: StructInfo) u32 {
        return self.byte_size;
    }
};

/// Backwards-compat alias for `StructInfo`.
pub const RecordInfo = StructInfo;
/// Backwards-compat alias for `StructInfo`.
pub const TupleInfo = StructInfo;

/// Bundled information about a tag union layout
pub const TagUnionInfo = struct {
    idx: TagUnionIdx,
    data: *const TagUnionData,
    alignment: std.mem.Alignment,
    /// Size in bytes, resolved for the store's target.
    byte_size: u32,
    /// Discriminant offset, resolved for the store's target.
    discriminant_offset: u16,
    variants: TagUnionVariant.SafeMultiList.Slice,
    contains_refcounted: bool,

    pub fn size(self: TagUnionInfo) u32 {
        return self.byte_size;
    }

    pub fn readDiscriminant(self: TagUnionInfo, ptr: [*]const u8) u32 {
        return self.data.readDiscriminantFromPtr(ptr + self.discriminant_offset);
    }
};

/// Bundled information about a scalar layout
pub const ScalarInfo = struct {
    tag: ScalarTag,
    size: u32,
    alignment: u32,
    int_precision: ?types.Int.Precision,
    frac_precision: ?types.Frac.Precision,
    vector: ?Vector,
};

/// The memory layout of a value in a running Roc program.
///
/// A Layout can be created from a Roc type, given the additional information
/// of the build target's `usize`. Layouts cannot be created without knowing
/// that aspect of the build target, because pointers in layouts are different
/// sizes on 32-bit and 64-bit targets. No other target information is needed.
///
/// When a Roc type gets converted to a Layout, zero-sized types (ZSTs)
/// like empty records and empty tag unions are represented with a first-class
/// ZST layout (`.zst` tag). Abstract type parameters must already have been
/// eliminated or collapsed to ZST before reaching this layer. ZST fields in
/// records and tuples are kept (not dropped) since they're a normal part
/// of the type structure, they just happen to have size 0.
/// (Exception: List({}) and Box({}) get special layouts `.list_of_zst` and
/// `.box_of_zst` because the stack-allocated container can be used at runtime
/// even if individual elements cannot be accessed.)
///
/// Once a type has been converted to a Layout, there is no longer any
/// distinction between nominal and structural types, there's just memory.
/// Records and tuples have both been flattened (so, no more extension vars)
/// and converted into a single unified struct type whose fields are sorted
/// by alignment and then by field name (records) or tuple index (tuples).
/// We store the original source index for each field (for tuple element access).
pub const Layout = packed struct {
    // Zig 0.16: packed unions require uniform field widths, so we use a raw
    // integer backing with typed accessors (wrap/unwrap pattern from Zir.zig).
    data: LayoutData,
    tag: LayoutTag,

    // -- Typed accessors for unpacking the raw data field --

    pub fn getScalar(self: Layout) Scalar {
        return @bitCast(@as(std.meta.Int(.unsigned, @bitSizeOf(Scalar)), @truncate(self.data)));
    }

    pub fn getIdx(self: Layout) Idx {
        return @enumFromInt(self.data);
    }

    pub fn getStruct(self: Layout) StructLayout {
        return @bitCast(@as(std.meta.Int(.unsigned, @bitSizeOf(StructLayout)), @truncate(self.data)));
    }

    pub fn getClosure(self: Layout) ClosureLayout {
        return @bitCast(@as(std.meta.Int(.unsigned, @bitSizeOf(ClosureLayout)), @truncate(self.data)));
    }

    pub fn getTagUnion(self: Layout) TagUnionLayout {
        return @bitCast(@as(std.meta.Int(.unsigned, @bitSizeOf(TagUnionLayout)), @truncate(self.data)));
    }

    fn packData(val: anytype) LayoutData {
        const T = @TypeOf(val);
        const bits = @bitSizeOf(T);
        return @intCast(@as(std.meta.Int(.unsigned, bits), @bitCast(val)));
    }

    /// This layout's alignment, given a particular target usize.
    pub fn alignment(self: Layout, target_usize: target.TargetUsize) std.mem.Alignment {
        return switch (self.tag) {
            .scalar => switch (self.getScalar().tag) {
                .int => self.getScalar().getInt().alignment(),
                .frac => self.getScalar().getFrac().alignment(),
                .str => target_usize.alignment(),
                .opaque_ptr => target_usize.alignment(),
                .vector => .@"16",
            },
            .box, .box_of_zst => target_usize.alignment(),
            .list, .list_of_zst => target_usize.alignment(),
            .erased_callable => target_usize.alignment(),
            .struct_ => self.getStruct().sort_key.alignment(target_usize),
            .tag_union => self.getTagUnion().sort_key.alignment(target_usize),
            .closure => target_usize.alignment(),
            .zst => std.mem.Alignment.@"1",
            .ptr => target_usize.alignment(),
        };
    }

    /// This layout's target-independent alignment class (see `SortKey`). Pointers
    /// are `.pointer`; fixed-width scalars map to their alignment band; aggregates
    /// return the `sort_key` they stored at commit time. Pure (no store, no
    /// target) — identical on 32-bit and 64-bit targets.
    pub fn sortKey(self: Layout) SortKey {
        return switch (self.tag) {
            .scalar => switch (self.getScalar().tag) {
                .int => SortKey.fromAlignBytes(self.getScalar().getInt().alignment().toByteUnits()),
                .frac => SortKey.fromAlignBytes(self.getScalar().getFrac().alignment().toByteUnits()),
                .str, .opaque_ptr => .pointer,
                .vector => .align_16,
            },
            .box, .box_of_zst, .list, .list_of_zst, .erased_callable, .ptr, .closure => .pointer,
            .zst => .align_1,
            .struct_ => self.getStruct().sort_key,
            .tag_union => self.getTagUnion().sort_key,
        };
    }

    /// int layout with the given precision
    pub fn int(precision: types.Int.Precision) Layout {
        return .{ .data = packData(Scalar.initInt(precision)), .tag = .scalar };
    }

    /// frac layout with the given precision
    pub fn frac(precision: types.Frac.Precision) Layout {
        return .{ .data = packData(Scalar.initFrac(precision)), .tag = .scalar };
    }

    /// Fixed-width 128-bit integer SIMD vector layout.
    pub fn vector(kind: Vector) Layout {
        return .{ .data = packData(Scalar.initVector(kind)), .tag = .scalar };
    }

    /// Default number layout (Dec) for unresolved polymorphic number types
    pub fn default_num() Layout {
        return Layout.frac(.dec);
    }

    /// Canonical layout for any two-nullary tag union.
    /// The shared layout store reserves tag-union metadata index 0 for this shape.
    pub fn boolType() Layout {
        return Layout.tagUnion(.align_1, .{ .int_idx = 0 });
    }

    /// bool layout (alias for consistency)
    pub fn boolean() Layout {
        return boolType();
    }

    /// str layout
    pub fn str() Layout {
        return .{ .data = packData(Scalar.initStr()), .tag = .scalar };
    }

    pub fn opaquePtr() Layout {
        return .{ .data = packData(Scalar{ .data = 0, .tag = .opaque_ptr }), .tag = .scalar };
    }

    /// box layout with the given element layout
    pub fn box(elem_idx: Idx) Layout {
        return .{ .data = @intFromEnum(elem_idx), .tag = .box };
    }

    /// box of zero-sized type layout (e.g. Box({}))
    pub fn boxOfZst() Layout {
        return .{ .data = 0, .tag = .box_of_zst };
    }

    /// compiler-internal pointer layout with the given element layout
    pub fn ptr(elem_idx: Idx) Layout {
        return .{ .data = @intFromEnum(elem_idx), .tag = .ptr };
    }

    /// list layout with the given element layout
    pub fn list(elem_idx: Idx) Layout {
        return .{ .data = @intFromEnum(elem_idx), .tag = .list };
    }

    /// list of zero-sized type layout (e.g. List({}))
    pub fn listOfZst() Layout {
        return .{ .data = 0, .tag = .list_of_zst };
    }

    /// struct layout with the given alignment class and struct metadata.
    /// Used for both records and tuples — at the layout level they are identical.
    pub fn struct_(struct_sort_key: SortKey, struct_idx: StructIdx) Layout {
        return .{ .data = packData(StructLayout{ .sort_key = struct_sort_key, .idx = struct_idx }), .tag = .struct_ };
    }

    /// Backwards-compat aliases
    pub const record = struct_;
    pub const tuple = struct_;

    pub fn closure(captures_layout_idx: Idx) Layout {
        return .{ .data = packData(ClosureLayout{ .captures_layout_idx = captures_layout_idx }), .tag = .closure };
    }

    /// Runtime layout for an erased callable stored behind a `Box(T)` boundary.
    /// The value itself is one ordinary Roc refcounted payload pointer.
    /// The heap payload starts with `builtins.erased_callable.Payload` and then
    /// stores the erased callable's hidden capture bytes inline.
    pub fn erasedCallable() Layout {
        return .{ .data = 0, .tag = .erased_callable };
    }

    /// Zero-sized type layout (empty records, empty tuples, phantom types, etc.)
    pub fn zst() Layout {
        return .{ .data = 0, .tag = .zst };
    }

    /// tag union layout with the given alignment class and tag union metadata
    pub fn tagUnion(tu_sort_key: SortKey, tu_idx: TagUnionIdx) Layout {
        return .{ .data = packData(TagUnionLayout{ .sort_key = tu_sort_key, .idx = tu_idx }), .tag = .tag_union };
    }

    /// Check if a layout represents a heap-allocated type that needs refcounting
    pub fn isRefcounted(self: Layout) bool {
        return switch (self.tag) {
            .scalar => switch (self.getScalar().tag) {
                .str => true, // RocStr needs refcounting
                .int, .frac, .opaque_ptr, .vector => false,
            },
            .list, .list_of_zst => true, // Lists need refcounting
            .box, .box_of_zst => true, // Boxes need refcounting
            .erased_callable => true, // Boxed erased functions need refcounting
            else => false,
        };
    }

    /// Compare two layouts for equality.
    /// This compares only the active variant based on the tag, avoiding
    /// comparison of uninitialized union bytes that would trigger Valgrind warnings.
    pub fn eql(self: Layout, other: Layout) bool {
        if (self.tag != other.tag) return false;
        return switch (self.tag) {
            .scalar => self.getScalar().tag == other.getScalar().tag and switch (self.getScalar().tag) {
                .str => true, // No additional data to compare
                .int => self.getScalar().getInt() == other.getScalar().getInt(),
                .frac => self.getScalar().getFrac() == other.getScalar().getFrac(),
                .opaque_ptr => true,
                .vector => self.getScalar().getVector() == other.getScalar().getVector(),
            },
            .box => self.getIdx() == other.getIdx(),
            .box_of_zst => true, // No additional data
            .list => self.getIdx() == other.getIdx(),
            .list_of_zst => true, // No additional data
            .struct_ => self.getStruct().sort_key == other.getStruct().sort_key and
                self.getStruct().idx.int_idx == other.getStruct().idx.int_idx,
            .closure => self.getClosure().captures_layout_idx == other.getClosure().captures_layout_idx,
            .erased_callable => true,
            .zst => true, // No additional data
            .tag_union => self.getTagUnion().sort_key == other.getTagUnion().sort_key and
                self.getTagUnion().idx.int_idx == other.getTagUnion().idx.int_idx,
            .ptr => self.getIdx() == other.getIdx(),
        };
    }
};

test "Size of Layout type" {
    // The Layout should have small size since it's used a ton, so avoid letting this number increase!
    try std.testing.expectEqual(layout_bit_size, @bitSizeOf(Layout));
}

test "Layout.alignment() - scalar types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.int(.u8).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.int(.i8).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"2", Layout.int(.u16).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"2", Layout.int(.i16).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", Layout.int(.u32).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", Layout.int(.i32).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", Layout.int(.u64).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", Layout.int(.i64).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", Layout.int(.u128).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", Layout.int(.i128).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"4", Layout.frac(.f32).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"8", Layout.frac(.f64).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"16", Layout.frac(.dec).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.@"1", Layout.boolType().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.str().alignment(target_usize));
        inline for (std.enums.values(Vector)) |vector_kind| {
            try testing.expectEqual(std.mem.Alignment.@"16", Layout.vector(vector_kind).alignment(target_usize));
        }
    }
}

test "integer SIMD vector facts" {
    const testing = std.testing;

    try testing.expectEqual(@as(u8, 8), Vector.u8x16.laneBits());
    try testing.expectEqual(@as(u8, 16), Vector.u8x16.laneCount());
    try testing.expectEqual(@as(u8, 64), Vector.i64x2.laneBits());
    try testing.expectEqual(@as(u8, 2), Vector.i64x2.laneCount());
    try testing.expect(!Vector.u32x4.isSigned());
    try testing.expect(Vector.i32x4.isSigned());
    try testing.expectEqual(types.Int.Precision.i16, Vector.i16x8.lanePrecision());
}
test "Layout.alignment() - types containing pointers" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(target_usize.alignment(), Layout.box(.bool).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.boxOfZst().alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.list(.bool).alignment(target_usize));
        try testing.expectEqual(target_usize.alignment(), Layout.listOfZst().alignment(target_usize));
    }
}

test "Layout.alignment() - struct types" {
    const testing = std.testing;

    for (target.TargetUsize.all()) |target_usize| {
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), Layout.struct_(.align_4, StructIdx{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), Layout.struct_(.align_16, StructIdx{ .int_idx = 1 }).alignment(target_usize));
    }
}

test "StructData.getFields()" {
    const testing = std.testing;

    const struct_data = StructData{
        .size = WidthValues(u32).both(40, 40),
        .fields = .{ .start = 10, .count = 5 },
    };

    const fields_range = struct_data.getFields();
    try testing.expectEqual(@as(u32, 10), @intFromEnum(fields_range.start));
    try testing.expectEqual(@as(u32, 15), @intFromEnum(fields_range.start) + fields_range.count);
}

test "Layout scalar data access" {
    const testing = std.testing;

    // Test int
    const int_layout = Layout.int(.i32);
    try testing.expectEqual(LayoutTag.scalar, int_layout.tag);
    try testing.expectEqual(ScalarTag.int, int_layout.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i32, int_layout.getScalar().getInt());

    // Test frac
    const frac_layout = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_layout.tag);
    try testing.expectEqual(ScalarTag.frac, frac_layout.getScalar().tag);
    try testing.expectEqual(types.Frac.Precision.f64, frac_layout.getScalar().getFrac());

    // Test canonical two-nullary enum layout
    const bool_layout = Layout.boolType();
    try testing.expectEqual(LayoutTag.tag_union, bool_layout.tag);
    try testing.expectEqual(@as(u16, 0), bool_layout.getTagUnion().idx.int_idx);

    // Test str
    const str_layout = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_layout.tag);
    try testing.expectEqual(ScalarTag.str, str_layout.getScalar().tag);
    try testing.expectEqual(ScalarTag.str, str_layout.getScalar().tag);
}

test "Non-scalar layout variants - fallback to indexed approach" {
    const testing = std.testing;

    // Test non-scalar box (should use .box tag with index)
    const box_non_scalar = Layout.box(@as(Idx, @enumFromInt(42)));
    try testing.expectEqual(LayoutTag.box, box_non_scalar.tag);
    try testing.expectEqual(@as(u28, 42), @intFromEnum(box_non_scalar.getIdx()));

    // Test non-scalar list (should use .list tag with index)
    const list_non_scalar = Layout.list(@as(Idx, @enumFromInt(123)));
    try testing.expectEqual(LayoutTag.list, list_non_scalar.tag);
    try testing.expectEqual(@as(u28, 123), @intFromEnum(list_non_scalar.getIdx()));

    // Test struct layout (definitely non-scalar)
    const struct_layout = Layout.struct_(.align_8, StructIdx{ .int_idx = 456 });
    try testing.expectEqual(LayoutTag.struct_, struct_layout.tag);
    try testing.expectEqual(SortKey.align_8, struct_layout.getStruct().sort_key);
    try testing.expectEqual(@as(@TypeOf(struct_layout.getStruct().idx.int_idx), 456), struct_layout.getStruct().idx.int_idx);
}

test "Layout scalar precision coverage" {
    const testing = std.testing;

    // Test all int precisions
    for ([_]types.Int.Precision{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 }) |precision| {
        const int_layout = Layout.int(precision);
        try testing.expectEqual(LayoutTag.scalar, int_layout.tag);
        try testing.expectEqual(ScalarTag.int, int_layout.getScalar().tag);
        try testing.expectEqual(precision, int_layout.getScalar().getInt());
    }

    // Test all frac precisions
    for ([_]types.Frac.Precision{ .f32, .f64, .dec }) |precision| {
        const frac_layout = Layout.frac(precision);
        try testing.expectEqual(LayoutTag.scalar, frac_layout.tag);
        try testing.expectEqual(ScalarTag.frac, frac_layout.getScalar().tag);
        try testing.expectEqual(precision, frac_layout.getScalar().getFrac());
    }

    // Test complex layout types have correct tags
    const complex_layouts = [_]Layout{
        Layout.box(.bool),
        Layout.boxOfZst(),
        Layout.list(.bool),
        Layout.listOfZst(),
        Layout.struct_(.align_4, StructIdx{ .int_idx = 0 }),
        Layout.struct_(.align_8, StructIdx{ .int_idx = 0 }),
    };

    const expected_tags = [_]LayoutTag{
        .box,
        .box_of_zst,
        .list,
        .list_of_zst,
        .struct_,
        .struct_,
    };

    for (complex_layouts, expected_tags) |layout, expected_tag| {
        try testing.expectEqual(expected_tag, layout.tag);
    }
}
