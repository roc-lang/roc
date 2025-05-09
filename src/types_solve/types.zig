const std = @import("std");
const collections = @import("../collections.zig");
const Ident = @import("../base/Ident.zig");

const SmallStringInterner = collections.SmallStringInterner;
const SafeList = collections.SafeList;

test {
    // If your changes caused this number to go down, great! Please update it to the lower number.
    // If it went up, please make sure your changes are absolutely required!
    try std.testing.expectEqual(28, @sizeOf(Descriptor));
    try std.testing.expectEqual(24, @sizeOf(Content));
    try std.testing.expectEqual(20, @sizeOf(Alias));
    try std.testing.expectEqual(20, @sizeOf(FlatType));
    try std.testing.expectEqual(12, @sizeOf(Record));
}

/// A type variable
pub const Var = enum(u32) { _ };

/// A safelist of type variables
pub const VarSafeList = SafeList(Var);

// A type descriptor
pub const Descriptor = struct { content: Content, rank: Rank };

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

// content //

/// Represents what the a type *is*
///
/// Numbers are special cased here. This means that when constraints, types
/// like `Num(Int(Unsigned64))` should be reperesntsed as it's specific
/// `flat_type.num` *not* as `flat_type.apply`. See 'Num' struct for additional
/// details
pub const Content = union(enum) {
    flex_var: ?SmallStringInterner.Idx,
    rigid_var: SmallStringInterner.Idx,
    alias: Alias,
    effectful,
    pure,
    structure: FlatType,
    err,
};

// alias //

// a nominal or structural alias
// can hold up to 16 arguments
pub const Alias = struct {
    type: Type,
    ident: TypeIdent,
    args: VarSafeList.Range,
    backing_var: Var,
    // TODO: lambda sets var

    /// the type of an alias
    pub const Type = enum { opaque_, structural };
};

/// Represents an ident of a type
/// TODO: Should this be something like CanIdent???
pub const TypeIdent = struct {
    const Self = @This();

    ident_idx: Ident.Idx,
    // TODO: Add module ident

    pub fn eql(a: Self, b: Self) bool {
        return a.ident_idx == b.ident_idx;
    }
};

// flat types //

// A "flat" data type
// todo: rename?
pub const FlatType = union(enum) {
    type_apply: TypeApply,
    tuple: Tuple,
    num: Num,
    func: Func,
    record: Record,
    empty_record,
    tag_union: TagUnion,
    empty_tag_union,
};

// type application //

/// Represents a type application, like `List String` or `Result Error Value`.
/// Applications may have up to 16 type arguments.
pub const TypeApply = struct {
    ident: TypeIdent,
    args: VarSafeList.Range,
};

// tuples //

/// Represents a tuple
pub const Tuple = struct {
    elems: VarSafeList.Range,
};

// numbers //

/// Represents number
///
/// Numbers could be representsed by `type_apply` and phantom types, but
/// that ends up being inefficient because every number type requires
/// multiple different type entrie.s By special casing them here we can
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

// functions //

/// Represents a function
/// Functions may have up to 16 arguments.
/// TODO: Should function support more args?
pub const Func = struct {
    args: VarSafeList.Range,
    ret: Var,
    eff: Var,

    // TODO: These are needed once we have lambda sets
    // lambda_set: Var,
};

// records //

/// Represents a record
pub const Record = struct {
    // TODO: Should we use a multilist here?
    fields: RecordFieldSafeList.Range,
    ext: Var,

    const Self = @This();

    /// Returns true if all fields in a record are optional
    /// If there are no fields, also returns true
    pub fn areAllFieldsOptional(self: *const Self, backing_fields: *const RecordFieldSafeList) bool {
        for (backing_fields.rangeToSlice(self.fields)) |field| {
            if (field.typ != .optional) {
                return false;
            }
        }
        return true;
    }
};

/// TODO: Rust compiler has `demanded` here too
const RecordFieldType = enum { required, optional };

/// A field on a record
pub const RecordField = struct {
    const Self = @This();

    name: collections.SmallStringInterner.Idx,
    typ: RecordFieldType,
    var_: Var,

    /// A function to be pased into std.mem.sort to sort fields by name
    pub fn sortByFieldNameAsc(_: @TypeOf(.{}), a: Self, b: Self) bool {
        return @intFromEnum(a.name) < @intFromEnum(b.name);
    }
};

/// A safelist of record fields
pub const RecordFieldSafeList = SafeList(RecordField);

/// A safelist of record fields
pub const TwoRecordFieldsSafeList = SafeList(TwoRecordFields);

/// Two record fields
pub const TwoRecordFields = struct { a: RecordField, b: RecordField };

// tag unions //

/// Represents a tag union
pub const TagUnion = struct {
    tags: TagSafeList.Range,
    ext: Var,
};

/// A tag entry in a tag union row
pub const Tag = struct {
    /// The name of the tag (e.g. "Ok", "Err")
    name: collections.SmallStringInterner.Idx,

    /// A list of argument types for the tag (0 = no payload)
    args: VarSafeList.Range,

    const Self = @This();

    /// A function to be pased into std.mem.sort to sort tags by idx
    pub fn sortByTagIdxAsc(_: @TypeOf(.{}), a: Self, b: Self) bool {
        return @intFromEnum(a.name) < @intFromEnum(b.name);
    }
};

/// A safelist of tag union fields
pub const TagSafeList = SafeList(Tag);

/// A safelist of tag union fields
pub const TwoTagsSafeList = SafeList(TwoTags);

/// Two tag union fields
pub const TwoTags = struct { a: Tag, b: Tag };
