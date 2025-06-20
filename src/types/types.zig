//! This module defines the core data structures for representing types in the compiler's
//! Hindley-Milner type inference system. It includes:
//!
//! - `Var`: unique type variable identifiers
//! - `Descriptor`: the rank, mark, and structure of a type
//! - `Content`: the semantic meaning of a type (flex var, alias, function, record, etc.)
//! - `FlatType`: the 'flat' shape of a type (tuples, numbers, tag unions, etc.)
//! - `Alias`: nominal or structural type aliases
//! - `Func`, `Record`, `TagUnion`: structured type forms
//!
//! Special care is taken to keep memory layouts small and efficient. When modifying
//! these types, please consider their size impact and unification performance.

const std = @import("std");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");

const MkSafeList = collections.SafeList;
const MkSafeMultiList = collections.SafeMultiList;
const exitOnOom = collections.utils.exitOnOom;

test {
    // If your changes caused this number to go down, great! Please update it to the lower number.
    // If it went up, please make sure your changes are absolutely required!
    try std.testing.expectEqual(32, @sizeOf(Descriptor));
    try std.testing.expectEqual(24, @sizeOf(Content));
    try std.testing.expectEqual(16, @sizeOf(Alias));
    try std.testing.expectEqual(20, @sizeOf(FlatType));
    try std.testing.expectEqual(12, @sizeOf(Record));
}

/// A type variable
pub const Var = enum(u32) {
    _,

    /// A safe list of type variables
    pub const SafeList = MkSafeList(Var);

    /// Debug representation of a type variable, panics on allocation failure
    pub fn allocPrint(self: Var, gpa: std.mem.Allocator) []u8 {
        return std.fmt.allocPrint(gpa, "#{d}", .{@intFromEnum(self)}) catch |err| exitOnOom(err);
    }
};

/// A type descriptor
pub const Descriptor = struct { content: Content, rank: Rank, mark: Mark };

/// A type variable rank
pub const Rank = enum(u4) {
    generalized = 0,
    top_level = 1,
    _,

    /// Get the lowest rank
    pub fn min(a: Rank, b: Rank) Rank {
        return @enumFromInt(@min(@intFromEnum(a), @intFromEnum(b)));
    }
};

/// A type variable mark
///
/// Marks are temporary annotations used during various phases of type inference
/// and type checking to track state.
///
/// Some places `Mark` is used:
/// * Marking variables as visited in occurs checks to avoid redundant work
/// * Marking variables for generalizing during solving
pub const Mark = enum(u32) {
    const Self = @This();

    visited = 0,
    none = 1,
    _,

    /// Get the next mark
    pub fn next(self: Self) Self {
        return self + 1;
    }
};

// content //

/// Represents what the a type *is*
pub const Content = union(enum) {
    const Self = @This();

    flex_var: ?Ident.Idx,
    rigid_var: Ident.Idx,
    alias: Alias,
    effectful,
    pure,
    structure: FlatType,
    err,

    // helpers //

    /// Unwrap a record or return null
    pub fn unwrapRecord(content: Self) ?Record {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .record => |record| {
                        return record;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a tag union or return null
    pub fn unwrapTagUnion(content: Self) ?TagUnion {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |tag_union| {
                        return tag_union;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    /// Unwrap a custom type or return null
    pub fn unwrapCustomType(content: Self) ?CustomType {
        switch (content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .custom_type => |custom_type| {
                        return custom_type;
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }
};

// alias //

/// A named alias to a different type
pub const Alias = struct {
    ident: TypeIdent,
    args: Var.SafeList.Range,
    backing_var: Var,
};

/// Represents an ident of a type
/// TODO: Should this be something like CanIdent???
pub const TypeIdent = struct {
    const Self = @This();

    ident_idx: Ident.Idx,
    // TODO: Add module ident?

    pub fn eql(store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.order(store, a, b) == .eq;
    }

    /// Get the ordering of how a compares to b
    pub fn order(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.ident_idx);
        const b_text = store.getText(b.ident_idx);
        return std.mem.order(u8, a_text, b_text);
    }
};

// flat types //

/// A "flat" data type
pub const FlatType = union(enum) {
    str,
    box: Var,
    list: Var,
    tuple: Tuple,
    num: Num,
    custom_type: CustomType,
    func: Func,
    record: Record,
    empty_record,
    tag_union: TagUnion,
    empty_tag_union,
};

// tuples //

/// Represents a tuple
pub const Tuple = struct {
    elems: Var.SafeList.Range,
};

// numbers //

/// Represents numeric types in the type system.
///
/// Numbers are extremely common, so we special-case their representation
/// for both performance and memory efficiency. While Roc exposes numbers
/// as opaque types with phantom type variables (eg, `Num(a)`), we avoid
/// representing them using fully generic applications unless absolutely
/// necessary.
///
/// In most cases—when a specific number type is known (like `U8`) — we store
/// a compact, canonical form directly. This avoids the need for multiple
/// indirections, such as separate type variables and layered aliases.
///
/// When a polymorphic number is required (eg in the type signature of
/// a generic function over `Num(a)`), we allow full representation via
/// `num_poly`, `int_poly`, or `frac_poly`. However, during unification,
/// if a polymorphic number is unified with a compact one, the compact
/// form always wins: we discard the polymorphic wrapper and store the
/// concrete, memory-efficient version instead.
pub const Num = union(enum) {
    num_poly: Var,
    int_poly: Var,
    frac_poly: Var,
    int_precision: Int.Precision,
    frac_precision: Frac.Precision,
    num_compact: Compact,

    /// Represents a compact number
    pub const Compact = union(enum) {
        const Self = @This();

        int: Int.Precision,
        frac: Frac.Precision,

        pub fn placeholder() Compact {
            return Compact{ .int = .u8 };
        }
    };

    /// The Frac data type
    pub const Frac = struct {
        /// The precision of a Frac
        pub const Precision = enum(u3) {
            f32 = 2,
            f64 = 3,
            dec = 4,

            /// Default precision for Frac(a), e.g. if you put `1.1` into `roc repl`.
            pub const default = Num.Frac.Precision.dec;

            /// Size in bytes
            pub fn size(self: @This()) u32 {
                // frac values always have the same size as their alignment
                return @as(u32, @intCast(self.alignment().toByteUnits()));
            }

            /// Alignment
            pub fn alignment(self: @This()) std.mem.Alignment {
                // Both self and std.mem.Alignment are stored as log2(alignment) integers.
                return @enumFromInt(@intFromEnum(self));
            }

            /// Get the lowest precision needed to hold the provided float
            /// This only supports f32s and f64s. Decimals must be assigned based on different criteria.
            /// When in doubt, this prefers f32s.
            pub fn fromValue(value: f64) Frac.Precision {
                if (std.math.isNan(value) or std.math.isInf(value)) {
                    return .f32;
                }

                // Check if the value fits in f32 range and precision
                const abs_value = @abs(value);
                if (abs_value == 0.0) {
                    return .f32;
                } else if (abs_value <= std.math.floatMax(f32) and abs_value >= std.math.floatMin(f32)) {
                    // Not every f64 can be downcast to f32 with the same precision
                    //
                    // For example if you had an f64 with the value
                    // `1.0000000000000002` and you downcast to  f32, it becomes
                    // 0.0. So this round trip equality tests ensure that if we
                    // downcast the provided we don't loose precision
                    const as_f32 = @as(f32, @floatCast(value));
                    const back_to_f64 = @as(f64, @floatCast(as_f32));
                    if (value == back_to_f64) {
                        return .f32;
                    }
                }

                return .f64;
            }
        };
    };

    /// The Int data type
    pub const Int = struct {
        /// The precision of an Int
        pub const Precision = enum(u4) {
            u8 = 0,
            i8 = 1,
            u16 = 2,
            i16 = 3,
            u32 = 4,
            i32 = 5,
            u64 = 6,
            i64 = 7,
            u128 = 8,
            i128 = 9,

            /// Default precision for Int(a), e.g. if you put `0x1` into `roc repl`.
            ///
            /// Note that numbers default to integers, so this is also what you'll
            /// get if you put `1` into `roc repl`.
            pub const default = Num.Int.Precision.i128;

            /// Size in bytes
            pub fn size(self: @This()) u32 {
                // int values always have the same size as their alignment
                return @as(u32, @intCast(self.alignment().toByteUnits()));
            }

            /// Alignment
            pub fn alignment(self: @This()) std.mem.Alignment {
                // Both self and std.mem.Alignment are stored as log2(alignment) integers,
                // although we have to divide self by 2 to get to that exact representation.
                return @enumFromInt(@intFromEnum(self) / 2);
            }

            /// Get the lowest precision needed to hold the provided value.
            ///
            /// For positive values, this function prefers signed types when the value
            /// fits in both signed and unsigned variants of the same bit width.
            /// This provides better compatibility since signed types can represent
            /// a wider range of operations (e.g., subtraction that might go negative).
            ///
            /// Examples:
            /// - 0 to 127: returns i8 (not u8)
            /// - 128 to 255: returns u8 (doesn't fit in i8)
            /// - 256 to 32767: returns i16 (not u16)
            /// - 32768 to 65535: returns u16 (doesn't fit in i16)
            ///
            /// For negative values, only signed types are considered.
            pub fn fromValue(value: i128) Int.Precision {
                if (value >= 0) {
                    const unsigned_value = @as(u128, @intCast(value));
                    // For positive values, prefer signed types when they fit
                    if (unsigned_value <= std.math.maxInt(i8)) return .i8;
                    if (unsigned_value <= std.math.maxInt(u8)) return .u8;
                    if (unsigned_value <= std.math.maxInt(i16)) return .i16;
                    if (unsigned_value <= std.math.maxInt(u16)) return .u16;
                    if (unsigned_value <= std.math.maxInt(i32)) return .i32;
                    if (unsigned_value <= std.math.maxInt(u32)) return .u32;
                    if (unsigned_value <= std.math.maxInt(i64)) return .i64;
                    if (unsigned_value <= std.math.maxInt(u64)) return .u64;
                    return .i128;
                } else {
                    // Negative values can only fit in signed types
                    if (value >= std.math.minInt(i8)) return .i8;
                    if (value >= std.math.minInt(i16)) return .i16;
                    if (value >= std.math.minInt(i32)) return .i32;
                    if (value >= std.math.minInt(i64)) return .i64;
                    return .i128;
                }
            }
        };
    };

    /// a frac f32
    pub const frac_f32: Num = Num{ .num_compact = Compact{ .frac = .f32 } };

    /// a frac f64
    pub const frac_f64: Num = Num{ .num_compact = Compact{ .frac = .f64 } };

    /// a frac dec
    pub const frac_dec: Num = Num{ .num_compact = Compact{ .frac = .dec } };

    /// an int u8
    pub const int_u8: Num = Num{ .num_compact = Compact{ .int = .u8 } };

    /// an int i8
    pub const int_i8: Num = Num{ .num_compact = Compact{ .int = .i8 } };

    /// an int u16
    pub const int_u16: Num = Num{ .num_compact = Compact{ .int = .u16 } };

    /// an int i16
    pub const int_i16: Num = Num{ .num_compact = Compact{ .int = .i16 } };

    /// an int u32
    pub const int_u32: Num = Num{ .num_compact = Compact{ .int = .u32 } };

    /// an int i32
    pub const int_i32: Num = Num{ .num_compact = Compact{ .int = .i32 } };

    /// an int u64
    pub const int_u64: Num = Num{ .num_compact = Compact{ .int = .u64 } };

    /// an int i64
    pub const int_i64: Num = Num{ .num_compact = Compact{ .int = .i64 } };

    /// an int u128
    pub const int_u128: Num = Num{ .num_compact = Compact{ .int = .u128 } };

    /// an int i128
    pub const int_i128: Num = Num{ .num_compact = Compact{ .int = .i128 } };
};

// custom types //

/// A nominal user-defined type
pub const CustomType = struct {
    ident: TypeIdent,
    args: Var.SafeList.Range,
    backing_var: Var,
};

// functions //

/// Represents a function
pub const Func = struct {
    args: Var.SafeList.Range,
    ret: Var,
    eff: Var,
};

// records //

/// Represents a record
pub const Record = struct {
    fields: RecordField.SafeMultiList.Range,
    ext: Var,

    const Self = @This();
};

/// A field on a record
pub const RecordField = struct {
    const Self = @This();

    /// The name of the field
    name: Ident.Idx,
    /// The type of the field's value
    var_: Var,

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }

    /// A safe multi list of record fields
    pub const SafeMultiList = MkSafeMultiList(Self);

    /// A safe list of record fields
    pub const SafeList = MkSafeList(Self);
};

/// Two record fields
pub const TwoRecordFields = struct {
    a: RecordField,
    b: RecordField,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

// tag unions //

/// Represents a tag union
pub const TagUnion = struct {
    tags: Tag.SafeMultiList.Range,
    ext: Var,
};

/// A tag entry in a tag union row
pub const Tag = struct {
    /// The name of the tag (e.g. "Ok", "Err")
    name: Ident.Idx,

    /// A list of argument types for the tag (0 = no payload)
    args: Var.SafeList.Range,

    const Self = @This();

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Self, b: Self) bool {
        return Self.orderByName(ident_store, a, b) == .lt;
    }

    /// Get the ordering of how a compares to b
    pub fn orderByName(store: *const Ident.Store, a: Self, b: Self) std.math.Order {
        const a_text = store.getText(a.name);
        const b_text = store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }

    /// A safe list of tags
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tags
    pub const SafeMultiList = MkSafeMultiList(@This());
};

/// Two tag union fields
pub const TwoTags = struct {
    a: Tag,
    b: Tag,

    /// A safe list of tag union fields
    pub const SafeList = MkSafeList(@This());

    /// A safe multi list of tag union fields
    pub const SafeMultiList = MkSafeMultiList(@This());
};

test "Precision.size() and alignment()" {
    const testing = @import("std").testing;

    // u8 and i8 should have size and alignment of 1
    try testing.expectEqual(1, Num.Int.Precision.u8.size());
    try testing.expectEqual(1, Num.Int.Precision.i8.size());
    try testing.expectEqual(1, Num.Int.Precision.u8.alignment().toByteUnits());
    try testing.expectEqual(1, Num.Int.Precision.i8.alignment().toByteUnits());

    // u16 and i16 should have size and alignment of 2
    try testing.expectEqual(2, Num.Int.Precision.u16.size());
    try testing.expectEqual(2, Num.Int.Precision.i16.size());
    try testing.expectEqual(2, Num.Int.Precision.u16.alignment().toByteUnits());
    try testing.expectEqual(2, Num.Int.Precision.i16.alignment().toByteUnits());

    // u32 and i32 should have size and alignment of 4
    try testing.expectEqual(4, Num.Int.Precision.u32.size());
    try testing.expectEqual(4, Num.Int.Precision.i32.size());
    try testing.expectEqual(4, Num.Int.Precision.u32.alignment().toByteUnits());
    try testing.expectEqual(4, Num.Int.Precision.i32.alignment().toByteUnits());

    // u64 and i64 should have size and alignment of 8
    try testing.expectEqual(8, Num.Int.Precision.u64.size());
    try testing.expectEqual(8, Num.Int.Precision.i64.size());
    try testing.expectEqual(8, Num.Int.Precision.u64.alignment().toByteUnits());
    try testing.expectEqual(8, Num.Int.Precision.i64.alignment().toByteUnits());

    // u128 and i128 should have size and alignment of 16
    try testing.expectEqual(16, Num.Int.Precision.u128.size());
    try testing.expectEqual(16, Num.Int.Precision.i128.size());
    try testing.expectEqual(16, Num.Int.Precision.u128.alignment().toByteUnits());
    try testing.expectEqual(16, Num.Int.Precision.i128.alignment().toByteUnits());

    // f32 should have size and alignment of 4
    try testing.expectEqual(4, Num.Frac.Precision.f32.size());
    try testing.expectEqual(4, Num.Frac.Precision.f32.alignment().toByteUnits());

    // f64 should have size and alignment of 8
    try testing.expectEqual(8, Num.Frac.Precision.f64.size());
    try testing.expectEqual(8, Num.Frac.Precision.f64.alignment().toByteUnits());

    // dec should have size and alignment of 16
    try testing.expectEqual(16, Num.Frac.Precision.dec.size());
    try testing.expectEqual(16, Num.Frac.Precision.dec.alignment().toByteUnits());
}
