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
    struct_, // Unified struct layout for both records and tuples (fields sorted by alignment)
    closure,
    zst, // Zero-sized type (empty records, empty tuples, phantom types, etc.)
    tag_union, // Tag union with variant-specific layouts for proper refcounting
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

    pub fn initStr() Scalar {
        return .{ .data = 0, .tag = .str };
    }

    pub fn initInt(precision: types.Int.Precision) Scalar {
        return .{ .data = @intFromEnum(precision), .tag = .int };
    }

    pub fn initFrac(precision: types.Frac.Precision) Scalar {
        return .{ .data = @intFromEnum(precision), .tag = .frac };
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

    // zero-sized type
    zst = 15,

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
            .i8, .i16, .i32, .i64, .i128, .dec => true,
            .u8, .u16, .u32, .u64, .u128 => false,
            // Default to signed for other types (floats don't use this, bools are unsigned)
            else => true,
        };
    }

    /// Sentinel for call expressions where the function is resolved by name
    /// (e.g., external method calls like `List.map`), not by closure dispatch.
    /// The dev backend resolves these via symbol lookup, so no closure layout is needed.
    pub const named_fn: Idx = @enumFromInt(std.math.maxInt(@typeInfo(Idx).@"enum".tag_type) - 1);

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
/// At the LIR level, records and tuples are both just contiguous fields sorted by alignment.
/// The `index` field stores the canonical semantic field index:
///   - For records: alphabetical closed-record field order
///   - For tuples: the original tuple element index (e.g. .0, .1, .2)
pub const StructField = struct {
    /// The canonical semantic index of this field before layout sorting.
    index: u16,
    /// The layout of the field's value
    layout: Idx,

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

/// Struct layout - stores alignment and index to full data in Store.
/// Unified representation for both records and tuples.
pub const StructLayout = packed struct {
    /// Alignment of the struct
    alignment: std.mem.Alignment,
    /// Index into the Store's struct data
    idx: StructIdx,
};

/// Backwards-compat alias for `StructLayout`.
pub const RecordLayout = StructLayout;
/// Backwards-compat alias for `StructLayout`.
pub const TupleLayout = StructLayout;

/// Index into the Store's struct data
pub const StructIdx = packed struct {
    int_idx: std.meta.Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag) - @bitSizeOf(std.mem.Alignment)),
};

/// Backwards-compat alias for `StructIdx`.
pub const RecordIdx = StructIdx;
/// Backwards-compat alias for `StructIdx`.
pub const TupleIdx = StructIdx;

/// Struct data stored in the layout Store — unified for records and tuples.
pub const StructData = struct {
    /// Size of the struct, in bytes
    size: u32,
    /// Range of fields in the struct_fields list
    fields: collections.NonEmptyRange,

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

/// Tag union layout - stores alignment and index to full data in Store
/// This preserves variant information needed for correct reference counting.
pub const TagUnionLayout = packed struct {
    /// Alignment of the tag union
    alignment: std.mem.Alignment,
    /// Index into the Store's tag union data
    idx: TagUnionIdx,
};

/// Index into the Store's tag union data
pub const TagUnionIdx = packed struct {
    int_idx: std.meta.Int(.unsigned, layout_bit_size - @bitSizeOf(LayoutTag) - @bitSizeOf(std.mem.Alignment)),
};

/// Tag union data stored in the layout Store
pub const TagUnionData = struct {
    /// Size of the tag union, in bytes (max payload + discriminant, aligned)
    size: u32,
    /// Offset of the discriminant within the union (usually after payload)
    discriminant_offset: u16,
    /// Size of the discriminant in bytes (0, 1, 2, 4, or 8).
    /// A size of 0 means the tag union has exactly one variant, so the
    /// discriminant is implicit and always 0.
    discriminant_size: u8,
    /// Range of variants in the tag_union_variants list
    variants: collections.NonEmptyRange,

    pub fn getVariants(self: TagUnionData) TagUnionVariant.SafeMultiList.Range {
        return self.variants.toRange(TagUnionVariant.SafeMultiList.Idx);
    }

    /// Read the discriminant value from memory at the given base pointer.
    /// Adds discriminant_offset internally to find the discriminant location.
    pub fn readDiscriminant(self: TagUnionData, base_ptr: [*]const u8) u32 {
        if (self.discriminant_size == 0) return 0;
        return self.readDiscriminantFromPtr(base_ptr + self.discriminant_offset);
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
    /// Adds discriminant_offset internally to find the discriminant location.
    pub fn writeDiscriminant(self: TagUnionData, base_ptr: [*]u8, value: u32) void {
        if (self.discriminant_size == 0) return;
        self.writeDiscriminantToPtr(base_ptr + self.discriminant_offset, value);
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
    pub fn discriminantSize(variant_count: usize) u8 {
        return if (variant_count <= 256) 1 else if (variant_count <= 65536) 2 else if (variant_count <= (1 << 32)) 4 else 8;
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
    fields: StructField.SafeMultiList.Slice,
    contains_refcounted: bool,

    pub fn size(self: StructInfo) u32 {
        return self.data.size;
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
    variants: TagUnionVariant.SafeMultiList.Slice,
    contains_refcounted: bool,

    pub fn size(self: TagUnionInfo) u32 {
        return self.data.size;
    }

    pub fn readDiscriminant(self: TagUnionInfo, ptr: [*]const u8) u32 {
        return self.data.readDiscriminantFromPtr(ptr + self.data.discriminant_offset);
    }
};

/// Bundled information about a scalar layout
pub const ScalarInfo = struct {
    tag: ScalarTag,
    size: u32,
    alignment: u32,
    int_precision: ?types.Int.Precision,
    frac_precision: ?types.Frac.Precision,
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
            },
            .box, .box_of_zst => target_usize.alignment(),
            .list, .list_of_zst => target_usize.alignment(),
            .struct_ => self.getStruct().alignment,
            .tag_union => self.getTagUnion().alignment,
            .closure => target_usize.alignment(),
            .zst => std.mem.Alignment.@"1",
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

    /// Default number layout (Dec) for unresolved polymorphic number types
    pub fn default_num() Layout {
        return Layout.frac(.dec);
    }

    /// Canonical layout for any two-nullary tag union.
    /// The shared layout store reserves tag-union metadata index 0 for this shape.
    pub fn boolType() Layout {
        return Layout.tagUnion(.@"1", .{ .int_idx = 0 });
    }

    /// bool layout (alias for consistency)
    pub fn boolean() Layout {
        return boolType();
    }

    /// str layout
    pub fn str() Layout {
        return .{ .data = packData(Scalar.initStr()), .tag = .scalar };
    }

    /// box layout with the given element layout
    pub fn box(elem_idx: Idx) Layout {
        return .{ .data = @intFromEnum(elem_idx), .tag = .box };
    }

    /// box of zero-sized type layout (e.g. Box({}))
    pub fn boxOfZst() Layout {
        return .{ .data = 0, .tag = .box_of_zst };
    }

    /// list layout with the given element layout
    pub fn list(elem_idx: Idx) Layout {
        return .{ .data = @intFromEnum(elem_idx), .tag = .list };
    }

    /// list of zero-sized type layout (e.g. List({}))
    pub fn listOfZst() Layout {
        return .{ .data = 0, .tag = .list_of_zst };
    }

    /// struct layout with the given alignment and struct metadata (e.g. size and field layouts)
    /// Used for both records and tuples — at the layout level they are identical.
    pub fn struct_(struct_alignment: std.mem.Alignment, struct_idx: StructIdx) Layout {
        return .{ .data = packData(StructLayout{ .alignment = struct_alignment, .idx = struct_idx }), .tag = .struct_ };
    }

    /// Backwards-compat aliases
    pub const record = struct_;
    pub const tuple = struct_;

    pub fn closure(captures_layout_idx: Idx) Layout {
        return .{ .data = packData(ClosureLayout{ .captures_layout_idx = captures_layout_idx }), .tag = .closure };
    }

    /// Zero-sized type layout (empty records, empty tuples, phantom types, etc.)
    pub fn zst() Layout {
        return .{ .data = 0, .tag = .zst };
    }

    /// tag union layout with the given alignment and tag union metadata
    pub fn tagUnion(tu_alignment: std.mem.Alignment, tu_idx: TagUnionIdx) Layout {
        return .{ .data = packData(TagUnionLayout{ .alignment = tu_alignment, .idx = tu_idx }), .tag = .tag_union };
    }

    /// Check if a layout represents a heap-allocated type that needs refcounting
    pub fn isRefcounted(self: Layout) bool {
        return switch (self.tag) {
            .scalar => switch (self.getScalar().tag) {
                .str => true, // RocStr needs refcounting
                else => false,
            },
            .list, .list_of_zst => true, // Lists need refcounting
            .box, .box_of_zst => true, // Boxes need refcounting
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
            },
            .box => self.getIdx() == other.getIdx(),
            .box_of_zst => true, // No additional data
            .list => self.getIdx() == other.getIdx(),
            .list_of_zst => true, // No additional data
            .struct_ => self.getStruct().alignment == other.getStruct().alignment and
                self.getStruct().idx.int_idx == other.getStruct().idx.int_idx,
            .closure => self.getClosure().captures_layout_idx == other.getClosure().captures_layout_idx,
            .zst => true, // No additional data
            .tag_union => self.getTagUnion().alignment == other.getTagUnion().alignment and
                self.getTagUnion().idx.int_idx == other.getTagUnion().idx.int_idx,
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
    }
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
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(4), Layout.struct_(std.mem.Alignment.@"4", StructIdx{ .int_idx = 0 }).alignment(target_usize));
        try testing.expectEqual(std.mem.Alignment.fromByteUnits(16), Layout.struct_(std.mem.Alignment.@"16", StructIdx{ .int_idx = 1 }).alignment(target_usize));
    }
}

test "StructData.getFields()" {
    const testing = std.testing;

    const struct_data = StructData{
        .size = 40,
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
    try testing.expectEqual({}, str_layout.getScalar().data.str);
}

test "Layout non-scalar types" {
    const testing = std.testing;

    // Test that non-scalar types have correct tags
    const box_layout = Layout.box(.bool);
    try testing.expectEqual(LayoutTag.box, box_layout.tag);

    const list_layout = Layout.list(.bool);
    try testing.expectEqual(LayoutTag.list, list_layout.tag);

    const struct_layout = Layout.struct_(std.mem.Alignment.@"4", StructIdx{ .int_idx = 0 });
    try testing.expectEqual(LayoutTag.struct_, struct_layout.tag);
}

test "Layout scalar variants" {
    const testing = std.testing;

    // Test scalar type creation
    const int_scalar = Layout.int(.i32);
    try testing.expectEqual(LayoutTag.scalar, int_scalar.tag);
    try testing.expectEqual(ScalarTag.int, int_scalar.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i32, int_scalar.getScalar().getInt());

    const str_scalar = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_scalar.tag);
    try testing.expectEqual(ScalarTag.str, str_scalar.getScalar().tag);

    const frac_scalar = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_scalar.tag);
    try testing.expectEqual(ScalarTag.frac, frac_scalar.getScalar().tag);
    try testing.expectEqual(types.Frac.Precision.f64, frac_scalar.getScalar().getFrac());

    // Test zst variants separately
    const box_zst = Layout.boxOfZst();
    try testing.expectEqual(LayoutTag.box_of_zst, box_zst.tag);

    const list_zst = Layout.listOfZst();
    try testing.expectEqual(LayoutTag.list_of_zst, list_zst.tag);
}

test "Scalar memory optimization - comprehensive coverage" {
    const testing = std.testing;

    const bool_layout = Layout.boolType();
    try testing.expectEqual(LayoutTag.tag_union, bool_layout.tag);
    try testing.expectEqual(@as(u16, 0), bool_layout.getTagUnion().idx.int_idx);

    const str_layout = Layout.str();
    try testing.expectEqual(LayoutTag.scalar, str_layout.tag);
    try testing.expectEqual(ScalarTag.str, str_layout.getScalar().tag);

    // Test ALL integer precisions
    const int_u8 = Layout.int(.u8);
    try testing.expectEqual(LayoutTag.scalar, int_u8.tag);
    try testing.expectEqual(ScalarTag.int, int_u8.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.u8, int_u8.getScalar().getInt());

    const int_i8 = Layout.int(.i8);
    try testing.expectEqual(LayoutTag.scalar, int_i8.tag);
    try testing.expectEqual(ScalarTag.int, int_i8.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i8, int_i8.getScalar().getInt());

    const int_u16 = Layout.int(.u16);
    try testing.expectEqual(LayoutTag.scalar, int_u16.tag);
    try testing.expectEqual(ScalarTag.int, int_u16.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.u16, int_u16.getScalar().getInt());

    const int_i16 = Layout.int(.i16);
    try testing.expectEqual(LayoutTag.scalar, int_i16.tag);
    try testing.expectEqual(ScalarTag.int, int_i16.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i16, int_i16.getScalar().getInt());

    const int_u32 = Layout.int(.u32);
    try testing.expectEqual(LayoutTag.scalar, int_u32.tag);
    try testing.expectEqual(ScalarTag.int, int_u32.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.u32, int_u32.getScalar().getInt());

    const int_i32 = Layout.int(.i32);
    try testing.expectEqual(LayoutTag.scalar, int_i32.tag);
    try testing.expectEqual(ScalarTag.int, int_i32.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i32, int_i32.getScalar().getInt());

    const int_u64 = Layout.int(.u64);
    try testing.expectEqual(LayoutTag.scalar, int_u64.tag);
    try testing.expectEqual(ScalarTag.int, int_u64.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.u64, int_u64.getScalar().getInt());

    const int_i64 = Layout.int(.i64);
    try testing.expectEqual(LayoutTag.scalar, int_i64.tag);
    try testing.expectEqual(ScalarTag.int, int_i64.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i64, int_i64.getScalar().getInt());

    const int_u128 = Layout.int(.u128);
    try testing.expectEqual(LayoutTag.scalar, int_u128.tag);
    try testing.expectEqual(ScalarTag.int, int_u128.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.u128, int_u128.getScalar().getInt());

    const int_i128 = Layout.int(.i128);
    try testing.expectEqual(LayoutTag.scalar, int_i128.tag);
    try testing.expectEqual(ScalarTag.int, int_i128.getScalar().tag);
    try testing.expectEqual(types.Int.Precision.i128, int_i128.getScalar().getInt());

    // Test ALL fraction precisions
    const frac_f32 = Layout.frac(.f32);
    try testing.expectEqual(LayoutTag.scalar, frac_f32.tag);
    try testing.expectEqual(ScalarTag.frac, frac_f32.getScalar().tag);
    try testing.expectEqual(types.Frac.Precision.f32, frac_f32.getScalar().getFrac());

    const frac_f64 = Layout.frac(.f64);
    try testing.expectEqual(LayoutTag.scalar, frac_f64.tag);
    try testing.expectEqual(ScalarTag.frac, frac_f64.getScalar().tag);
    try testing.expectEqual(types.Frac.Precision.f64, frac_f64.getScalar().getFrac());

    const frac_dec = Layout.frac(.dec);
    try testing.expectEqual(LayoutTag.scalar, frac_dec.tag);
    try testing.expectEqual(ScalarTag.frac, frac_dec.getScalar().tag);
    try testing.expectEqual(types.Frac.Precision.dec, frac_dec.getScalar().getFrac());
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
    const struct_layout = Layout.struct_(std.mem.Alignment.@"8", StructIdx{ .int_idx = 456 });
    try testing.expectEqual(LayoutTag.struct_, struct_layout.tag);
    try testing.expectEqual(std.mem.Alignment.@"8", struct_layout.getStruct().alignment);
    try testing.expectEqual(@as(u19, 456), struct_layout.getStruct().idx.int_idx);
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
        Layout.struct_(std.mem.Alignment.@"4", StructIdx{ .int_idx = 0 }),
        Layout.struct_(std.mem.Alignment.@"8", StructIdx{ .int_idx = 0 }),
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
