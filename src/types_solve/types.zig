const std = @import("std");
const Ident = @import("../base/Ident.zig");

test {
    // TODO: ADD size assertions here
    std.debug.print("Size of Descriptor: {} bytes\n", .{@sizeOf(Descriptor)});
    std.debug.print("Size of FlatType: {} bytes\n", .{@sizeOf(FlatType)});
    std.debug.print("Size of Record: {} bytes\n", .{@sizeOf(Record)});
}

/// A type variable id
pub const Var = enum(u32) { _ };

// A type descritpro
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

/// Represents what the a type *is*
///
/// Numbers are special cased here. This means that when constraints, types
/// like `Num(Int(Unsigned64))` should be reperesntsed as it's specific
/// `flat_type.num` *not* as `flat_type.apply`. See 'Num' struct for additional
/// details
pub const Content = union(enum) {
    flex_var: ?Ident.Idx,
    structural_alias: Alias,
    opaque_alias: Alias,
    effectful,
    pure,
    concrete: FlatType,
    err,
};

/// Represents an ident of a type
pub const TypeIdent = struct {
    const Self = @This();

    ident_idx: Ident.Idx,
    // TODO: Add module ident

    pub fn eql(a: Self, b: Self) bool {
        return a.ident_idx == b.ident_idx;
    }
};

// a nominal or structural alias
// can hold up to 16 arguments
pub const Alias = struct {
    ident: TypeIdent,
    args: ArgsArray,
    backing_var: Var,

    // TODO: In the rust compiler it seems like Args has lambda set variables too?

    /// Represents the max capacity of the args array
    pub const args_capacity = 16;
    /// Bounded array to hold args
    pub const ArgsArray = std.BoundedArray(Var, args_capacity);
};

// flat types

// A "flat" data type
// todo: rename?
pub const FlatType = union(enum) {
    type_apply: TypeApply,
    tuple: Tuple,
    num: Num,
    func: Func,
    record: Record,
    empty_record,
};

/// Represents a type application, like `List String` or `Result Error Value`.
/// Applications may have up to 16 type arguments.
pub const TypeApply = struct {
    ident: TypeIdent,
    args: ArgsArray,

    /// Represents the max capacity of the args array
    pub const arg_capacity = 16;
    /// Bounded array to hold args
    pub const ArgsArray = std.BoundedArray(Var, arg_capacity);
};

/// Represents a tuple
/// Tuples may have up to 16 type elements.
pub const Tuple = struct {
    elems: ElemsArray,

    /// Represents the max capacity of the elems array
    pub const elems_capacity = 16;
    /// Bounded array to hold elems
    pub const ElemsArray = std.BoundedArray(Var, elems_capacity);
};

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

    pub const Frac = enum { flex_var, f32, f64, dec };
    pub const Int = enum { flex_var, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128 };
};

pub const num_flex_var: FlatType = .{ .num = Num.flex_var };
pub const frac_flex_var: FlatType = .{ .num = Num{ .frac = .flex_var } };
pub const frac_f32: FlatType = .{ .num = Num{ .frac = .f32 } };
pub const frac_f64: FlatType = .{ .num = Num{ .frac = .f64 } };
pub const frac_dec: FlatType = .{ .num = Num{ .frac = .dec } };
pub const int_flex_var: FlatType = .{ .num = Num{ .int = .flex_var } };
pub const int_u8: FlatType = .{ .num = Num{ .int = .u8 } };
pub const int_i8: FlatType = .{ .num = Num{ .int = .i8 } };
pub const int_u16: FlatType = .{ .num = Num{ .int = .u16 } };
pub const int_i16: FlatType = .{ .num = Num{ .int = .i16 } };
pub const int_u32: FlatType = .{ .num = Num{ .int = .u32 } };
pub const int_i32: FlatType = .{ .num = Num{ .int = .i32 } };
pub const int_u64: FlatType = .{ .num = Num{ .int = .u64 } };
pub const int_i64: FlatType = .{ .num = Num{ .int = .i64 } };
pub const int_u128: FlatType = .{ .num = Num{ .int = .u128 } };
pub const int_i128: FlatType = .{ .num = Num{ .int = .i128 } };

/// Represents a function
/// Functions may have up to 16 arguments.
/// TODO: Should function support more args?
pub const Func = struct {
    args: ArgsArray,
    ret: Var,
    eff: Var,

    // TODO: These are needed once we have lambda sets
    // lambda_set: Var,

    /// Represents the max capacity of the args array
    pub const arg_capacity = 16;
    /// Bounded array to hold args
    pub const ArgsArray = std.BoundedArray(Var, arg_capacity);
};

/// A record field name
/// Once this module is used, this should be an index into a FieldName store or something
const RecordFieldName = enum(u32) { _ };

/// TODO: Rust compiler has `demanded` here too
const RecordFieldType = enum { required, optional };

/// A field on a record
pub const RecordField = struct {
    const Self = @This();

    name: RecordFieldName,
    typ: RecordFieldType,
    type_var: Var,

    /// Type to represent the field diff of 2 records
    pub const Partitioned = struct {
        const SelfP = @This();

        only_in_a: RecordFieldArray,
        only_in_b: RecordFieldArray,
        in_both: RecordFieldArray,

        /// Create a new `Partitioned` value
        /// TODO: Should this  be inline? These arrays could be a lot to copy if
        /// passing by value
        pub inline fn init() SelfP {
            // These are unreachable bc RecordFieldArray can hold more than 0 elems
            return .{
                .only_in_a = RecordFieldArray.init(0) catch unreachable,
                .only_in_b = RecordFieldArray.init(0) catch unreachable,
                .in_both = RecordFieldArray.init(0) catch unreachable,
            };
        }
    };

    /// A function to be pased into std.mem.sort to sort fields by name
    fn fieldNameAsc(_: @TypeOf(.{}), a: Self, b: Self) bool {
        return @intFromEnum(a.name) < @intFromEnum(b.name);
    }

    /// Given 2 records, divide the fields by name into groups of
    /// * in only a
    /// * in only b
    /// * in both
    ///
    /// This sorts the recored fields in-place if not already sorted
    ///
    /// O(n + m)
    pub fn parition(
        a_fields_arr: *RecordFieldArray,
        b_fields_arr: *RecordFieldArray,
        partitioned: *Partitioned,
    ) error{Overflow}!void {
        // First sort the fields
        const a_fields = a_fields_arr.slice();
        std.mem.sort(RecordField, a_fields, .{}, comptime fieldNameAsc);
        const b_fields = b_fields_arr.slice();
        std.mem.sort(RecordField, b_fields, .{}, comptime fieldNameAsc);

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_fields.len and b_i < b_fields.len) {
            const a_next = a_fields[a_i];
            const b_next = b_fields[b_i];

            if (@intFromEnum(a_next.name) == @intFromEnum(b_next.name)) {
                try partitioned.in_both.append(a_next);
                a_i = a_i + 1;
                b_i = b_i + 1;
            } else if (@intFromEnum(a_next.name) < @intFromEnum(b_next.name)) {
                try partitioned.only_in_a.append(a_next);
                a_i = a_i + 1;
            } else {
                try partitioned.only_in_b.append(b_next);
                b_i = b_i + 1;
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_fields.len) {
            const a_next = a_fields[a_i];
            try partitioned.only_in_a.append(a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_fields.len) {
            const b_next = b_fields[b_i];
            try partitioned.only_in_b.append(b_next);
            b_i = b_i + 1;
        }
    }
};

/// Bounded array of record fields
pub const RecordFieldArray = std.BoundedArray(RecordField, 16);

/// Represents a record
pub const Record = struct {
    fields: RecordFieldArray, // TODO: Should records support more fields?
    ext: Var,

    const SelfR = @This();

    /// Returns true if all fields in a record are optional
    pub fn areAllFieldsOptional(self: *const SelfR) bool {
        for (0..self.fields.len) |i| {
            if (self.fields.buffer[i].typ != .optional) {
                return false;
            }
        }
        return true;
    }
};

// tests

test "paritionFields - same record" {
    const field_x = RecordField{ .name = @enumFromInt(1), .typ = .required, .type_var = @enumFromInt(0) };
    const field_y = RecordField{ .name = @enumFromInt(2), .typ = .optional, .type_var = @enumFromInt(1) };

    var a_record_fields =
        RecordFieldArray.fromSlice(&[_]RecordField{ field_x, field_y }) catch unreachable;
    var b_record_fields = a_record_fields;

    var partitioned = RecordField.Partitioned.init();
    try RecordField.parition(&a_record_fields, &b_record_fields, &partitioned);

    try std.testing.expectEqual(0, partitioned.only_in_a.len);
    try std.testing.expectEqual(0, partitioned.only_in_b.len);
    try std.testing.expectEqual(2, partitioned.in_both.len);
    try std.testing.expectEqual(field_x.name, partitioned.in_both.buffer[0].name);
    try std.testing.expectEqual(field_y.name, partitioned.in_both.buffer[1].name);
}

test "paritionFields - disjoint fields" {
    const a1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .type_var = @enumFromInt(0) };
    const a2 = RecordField{ .name = @enumFromInt(2), .typ = .required, .type_var = @enumFromInt(1) };
    const b1 = RecordField{ .name = @enumFromInt(3), .typ = .required, .type_var = @enumFromInt(2) };

    var a_record_fields = RecordFieldArray.fromSlice(&[_]RecordField{ a1, a2 }) catch unreachable;
    var b_record_fields = RecordFieldArray.fromSlice(&[_]RecordField{b1}) catch unreachable;

    var partitioned = RecordField.Partitioned.init();
    try RecordField.parition(&a_record_fields, &b_record_fields, &partitioned);

    try std.testing.expectEqual(2, partitioned.only_in_a.len);
    try std.testing.expectEqual(1, partitioned.only_in_b.len);
    try std.testing.expectEqual(0, partitioned.in_both.len);
}

test "paritionFields - overlapping fields" {
    const a1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .type_var = @enumFromInt(0) };
    const both = RecordField{ .name = @enumFromInt(2), .typ = .optional, .type_var = @enumFromInt(1) };
    const b1 = RecordField{ .name = @enumFromInt(3), .typ = .required, .type_var = @enumFromInt(2) };

    var a_record_fields = RecordFieldArray.fromSlice(&[_]RecordField{ a1, both }) catch unreachable;
    var b_record_fields = RecordFieldArray.fromSlice(&[_]RecordField{ b1, both }) catch unreachable;

    var partitioned = RecordField.Partitioned.init();
    try RecordField.parition(&a_record_fields, &b_record_fields, &partitioned);

    try std.testing.expectEqual(1, partitioned.only_in_a.len);
    try std.testing.expectEqual(1, partitioned.only_in_b.len);
    try std.testing.expectEqual(1, partitioned.in_both.len);

    try std.testing.expectEqual(a1.name, partitioned.only_in_a.buffer[0].name);
    try std.testing.expectEqual(b1.name, partitioned.only_in_b.buffer[0].name);
    try std.testing.expectEqual(both.name, partitioned.in_both.buffer[0].name);
}

test "paritionFields - reordering is normalized" {
    const f1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .type_var = @enumFromInt(0) };
    const f2 = RecordField{ .name = @enumFromInt(2), .typ = .optional, .type_var = @enumFromInt(1) };
    const f3 = RecordField{ .name = @enumFromInt(3), .typ = .optional, .type_var = @enumFromInt(2) };

    var a_record_fields = RecordFieldArray.fromSlice(&[_]RecordField{ f3, f1, f2 }) catch unreachable;
    var b_record_fields = RecordFieldArray.fromSlice(&[_]RecordField{ f1, f2, f3 }) catch unreachable;

    var partitioned = RecordField.Partitioned.init();
    try RecordField.parition(&a_record_fields, &b_record_fields, &partitioned);

    try std.testing.expectEqual(0, partitioned.only_in_a.len);
    try std.testing.expectEqual(0, partitioned.only_in_b.len);
    try std.testing.expectEqual(3, partitioned.in_both.len);
    try std.testing.expectEqual(@as(RecordFieldName, @enumFromInt(1)), partitioned.in_both.buffer[0].name);
    try std.testing.expectEqual(@as(RecordFieldName, @enumFromInt(2)), partitioned.in_both.buffer[1].name);
    try std.testing.expectEqual(@as(RecordFieldName, @enumFromInt(3)), partitioned.in_both.buffer[2].name);
}
