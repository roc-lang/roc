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
pub const Mark = enum(u32) {
    const Self = @This();

    getVarNames = 0,
    occurs = 1,
    none = 2,
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

/// Represents number
///
/// Numbers are special-cased here. We represent them to the Roc programmer
/// as opaque types with phantom type variables, but since they come up so
/// often, we unify that representation into a special (much more compact)
/// representation which saves a lot of memory.
pub const Num = union(enum) {
    const Self = @This();

    flex_var,
    int: Int,
    frac: Frac,

    /// The Frac data type
    pub const Frac = union(enum) {
        flex_var,
        exact: Precision,

        /// The precision of a Frac
        pub const Precision = enum(u3) {
            f32 = 2,
            f64 = 3,
            dec = 4,

            /// Default precision for Frac(a), e.g. if you put `1.1` into `roc repl`.
            pub const default = Num.Frac.Precision.dec;

            /// Size in bytes
            pub fn size(self: @This()) usize {
                // frac values always have the same size as their alignment
                return self.alignment().toByteUnits();
            }

            /// Alignment
            pub fn alignment(self: @This()) std.mem.Alignment {
                // Both self and std.mem.Alignment are stored as log2(alignment) integers.
                return @enumFromInt(@intFromEnum(self));
            }
        };
    };

    /// The Int data type
    pub const Int = union(enum) {
        flex_var,
        exact: Precision,

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
            pub fn size(self: @This()) usize {
                // int values always have the same size as their alignment
                return self.alignment().toByteUnits();
            }

            /// Alignment
            pub fn alignment(self: @This()) std.mem.Alignment {
                // Both self and std.mem.Alignment are stored as log2(alignment) integers,
                // although we have to divide self by 2 to get to that exact representation.
                return @enumFromInt(@intFromEnum(self) / 2);
            }
        };
    };
};

/// a num flex_var
pub const num_flex_var: FlatType = .{ .num = Num.flex_var };

/// a frac flex_var
pub const frac_flex_var: FlatType = .{ .num = Num{ .frac = .flex_var } };

/// a frac f32
pub const frac_f32: FlatType = .{ .num = Num{ .frac = .{ .exact = .f32 } } };

/// a frac f64
pub const frac_f64: FlatType = .{ .num = Num{ .frac = .{ .exact = .f64 } } };

/// a frac dec
pub const frac_dec: FlatType = .{ .num = Num{ .frac = .{ .exact = .dec } } };

/// a int flex_var
pub const int_flex_var: FlatType = .{ .num = Num{ .int = .flex_var } };

/// an int u8
pub const int_u8: FlatType = .{ .num = Num{ .int = .{ .exact = .u8 } } };

/// an int i8
pub const int_i8: FlatType = .{ .num = Num{ .int = .{ .exact = .i8 } } };

/// an int u16
pub const int_u16: FlatType = .{ .num = Num{ .int = .{ .exact = .u16 } } };

/// an int i16
pub const int_i16: FlatType = .{ .num = Num{ .int = .{ .exact = .i16 } } };

/// an int u32
pub const int_u32: FlatType = .{ .num = Num{ .int = .{ .exact = .u32 } } };

/// an int i32
pub const int_i32: FlatType = .{ .num = Num{ .int = .{ .exact = .i32 } } };

/// an int u64
pub const int_u64: FlatType = .{ .num = Num{ .int = .{ .exact = .u64 } } };

/// an int i64
pub const int_i64: FlatType = .{ .num = Num{ .int = .{ .exact = .i64 } } };

/// an int u128
pub const int_u128: FlatType = .{ .num = Num{ .int = .{ .exact = .u128 } } };

/// an int i128
pub const int_i128: FlatType = .{ .num = Num{ .int = .{ .exact = .i128 } } };

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
