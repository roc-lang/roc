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
    int_precision: Compact.Int.Precision,
    frac_precision: Compact.Frac.Precision,
    num_compact: Compact,

    /// Represents a compact number
    pub const Compact = union(enum) {
        const Self = @This();

        int: Int.Precision,
        frac: Frac.Precision,

        pub fn placeholder() Compact {
            return Compact{ .int = .u8 };
        }

        /// the Num data type
        pub const Num = struct {};

        /// the Frac data type
        pub const Frac = struct {
            /// the precision of a frac
            pub const Precision = enum {
                f32,
                f64,
                dec,

                /// Get the lowest precision needed to hold the provided float
                /// This only supports f32s and f64s. Decimals must be assigned based on different criteria.
                /// When in doubt, this prefers f32s.
                pub fn fromValue(value: f64) Frac.Precision {
                    if (std.math.isNan(value) or std.math.isInf(value)) {
                        return .f32;
                    }

                    // Check if the value fits in f32 range and precision
                    if (@abs(value) == 0.0) {
                        return .f32;
                    } else if (value <= std.math.floatMax(f32) and value >= std.math.floatMin(f32)) {
                        // Not every f64 can be downcast to f32 with the same precision
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

        /// the Int data type
        pub const Int = struct {
            /// the precision of an int
            pub const Precision = enum {
                u8,
                i8,
                u16,
                i16,
                u32,
                i32,
                u64,
                i64,
                u128,
                i128,

                /// Get the lowest precision needed to hold the provided
                pub fn fromValue(value: i128) Int.Precision {
                    if (value >= 0) {
                        const unsigned_value = @as(u128, @intCast(value));
                        if (unsigned_value <= std.math.maxInt(u8)) return .u8;
                        if (unsigned_value <= std.math.maxInt(i8)) return .i8;
                        if (unsigned_value <= std.math.maxInt(u16)) return .u16;
                        if (unsigned_value <= std.math.maxInt(i16)) return .i16;
                        if (unsigned_value <= std.math.maxInt(u32)) return .u32;
                        if (unsigned_value <= std.math.maxInt(i32)) return .i32;
                        if (unsigned_value <= std.math.maxInt(u64)) return .u64;
                        if (unsigned_value <= std.math.maxInt(i64)) return .i64;
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
    };
    ///
    /// the precision of a num
    pub const Precision = struct {
        sign: Sign,
        min_precision: Compact.Int.Precision,

        pub const Sign = enum { positive, negative };

        /// Get the lowest precision needed to hold the provided num
        pub fn fromValue(value: i128) Precision {
            var sign: Sign = .negative;
            if (value >= 0) sign = .positive;
            return .{ .sign = sign, .min_precision = Compact.Int.Precision.fromValue(value) };
        }
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
