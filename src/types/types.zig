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

    /// Get the lowest rank
    pub fn next(self: Self) Self {
        return self + 1;
    }
};

// content //

/// Represents what the a type *is*
///
/// Numbers are special cased here. This means that when constraints, types
/// like `Num(Int(Unsigned64))` should be reperesntsed as it's specific
/// `flat_type.num` *not* as `flat_type.apply`. See 'Num' struct for additional
/// details
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
/// Numbers could be representsed by `type_apply` and phantom types, but
/// that ends up being inefficient because every number type requires
/// multiple different type entries. By special casing them here we can
/// ensure that they have more compact representations
pub const Num = union(enum) {
    const Self = @This();

    flex_var,
    int: Int,
    frac: Frac,

    /// the Frac data type
    pub const Frac = enum { flex_var, f32, f64, dec };

    /// the Int data type
    pub const Int = enum { flex_var, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128 };
};

/// constant
pub const num_flex_var: FlatType = .{ .num = Num.flex_var };
/// constant
pub const frac_flex_var: FlatType = .{ .num = Num{ .frac = .flex_var } };
/// constant
pub const frac_f32: FlatType = .{ .num = Num{ .frac = .f32 } };
/// constant
pub const frac_f64: FlatType = .{ .num = Num{ .frac = .f64 } };
/// constant
pub const frac_dec: FlatType = .{ .num = Num{ .frac = .dec } };
/// constant
pub const int_flex_var: FlatType = .{ .num = Num{ .int = .flex_var } };
/// constant
pub const int_u8: FlatType = .{ .num = Num{ .int = .u8 } };
/// constant
pub const int_i8: FlatType = .{ .num = Num{ .int = .i8 } };
/// constant
pub const int_u16: FlatType = .{ .num = Num{ .int = .u16 } };
/// constant
pub const int_i16: FlatType = .{ .num = Num{ .int = .i16 } };
/// constant
pub const int_u32: FlatType = .{ .num = Num{ .int = .u32 } };
/// constant
pub const int_i32: FlatType = .{ .num = Num{ .int = .i32 } };
/// constant
pub const int_u64: FlatType = .{ .num = Num{ .int = .u64 } };
/// constant
pub const int_i64: FlatType = .{ .num = Num{ .int = .i64 } };
/// constant
pub const int_u128: FlatType = .{ .num = Num{ .int = .u128 } };
/// constant
pub const int_i128: FlatType = .{ .num = Num{ .int = .i128 } };

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
    var_: Var,

    /// The context need by sortByFieldNameAsc
    pub const SortCtx = struct { store: *const Ident.Store };

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ctx: SortCtx, a: Self, b: Self) bool {
        return Self.orderByName(ctx.store, a, b) == .lt;
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

    /// The context need by sortByFieldNameAsc
    pub const SortCtx = struct { store: *const Ident.Store };

    /// A function to be passed into std.mem.sort to sort fields by name
    pub fn sortByNameAsc(ctx: SortCtx, a: Self, b: Self) bool {
        return Self.orderByName(ctx.store, a, b) == .lt;
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
