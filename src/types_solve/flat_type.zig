const std = @import("std");

const shared = @import("./shared.zig");

const Var = shared.Var;
const MAX_ELEMS = shared.MAX_ELEMS;
const VarBoundedArray16 = shared.VarBoundedArray16;

// flat types

// A "flat" data type
// todo: rename?
pub const FlatType = union(enum) {
    type_apply: TypeApply,
    tuple: Tuple,
    num: Num,
    func: Func,
    empty_record,
    record: Record,
};

/// Represents a type name, like Str, Bool, List
///
/// This includes both builtin types as well as user-defined types, and aliases
///
/// TODO: Does this need to be u32? Can we get away with u16?
/// TODO: A mapping from user-defined aliases to Names probably needs to exist somwhere
pub const TypeName = enum(u32) {
    bool_ = 0,
    str = 1,
    list = 2,
    maybe = 3,
    result = 4,
    _,
};

/// Represents a type application, like `List String` or `Result Error Value`.
/// Applications may have up to 16 type arguments.
pub const TypeApply = struct {
    name: TypeName,
    args: ArgsArray,

    /// Represents the max capacity of the args array
    pub const arg_array_capacity = 16;
    /// Bounded array to hold args
    pub const ArgsArray = std.BoundedArray(Var, arg_array_capacity);
};

/// Represents a tuple
/// Tuples may have up to 16 type elements.
pub const Tuple = struct {
    elems: ElemsArray,

    /// Represents the max capacity of the elems array
    pub const elems_array_capacity = 16;
    /// Bounded array to hold elems
    pub const ElemsArray = std.BoundedArray(Var, elems_array_capacity);
};

/// Represents number
///
/// Numbers could be representsed by `type_apply` and phantom types, but
/// that ends up being inefficient because every number type requires
/// multiple different type entrie.s By special casing them here we can
/// ensure that they have more compact representations
pub const Num = union(enum) {
    flex_var,
    int: Int,
    frac: Frac,

    pub const Frac = enum { flex_var, f32, f64, dec };
    pub const Int = enum { flex_var, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128 };
};

/// Represents a function
/// Functions may have up to 16 arguments.
/// TODO: Should function support more args?
pub const Func = struct {
    args: ArgsArray,
    ret: Var,

    // TODO: These are needed once we have effects & lambda sets
    // lambda_set: Var,
    // effect: Var,

    /// Represents the max capacity of the args array
    pub const arg_array_capacity = 16;
    /// Bounded array to hold args
    pub const ArgsArray = std.BoundedArray(Var, arg_array_capacity);
};

/// A record field name
///
/// Once this module is used, this should be an index into a FieldName store or something
const RecordFieldName = enum(u32) { _ };

/// TODO: Rust compiler has `demanded` here too
const RecordFieldType = enum { required, optional };

/// A field on a record
const RecordField = struct {
    name: RecordFieldName,
    typ: RecordFieldType,
    type_var: Var,
};

/// FieldBoundedArray
/// It has a max size of 16 elements
pub const RecordFieldBoundedArray = std.BoundedArray(RecordField, 16);

/// Represents a record
///
/// Records may haev up to 16 fields
/// Only the first `field_count` elements of `fields` are considered valid.
/// TODO: Should records support more fields?
pub const Record = struct {
    fields: RecordFieldBoundedArray,
    are_fields_sorted: bool, // are the fields sorted **desc**
    type_var: Var,
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

    /// A function to be pased into std.mem.sort to sort fields by name
    pub fn fieldNameAsc(_: @TypeOf(.{}), a: RecordField, b: RecordField) bool {
        return @intFromEnum(a.name) < @intFromEnum(b.name);
    }

    /// Type to represent the field diff of 2 records
    pub const Separated = struct {
        in_a: RecordFieldBoundedArray,
        in_b: RecordFieldBoundedArray,
        in_both: RecordFieldBoundedArray,
    };

    /// Given 2 records, divide the fields by name into groups of
    /// * in only a
    /// * in only b
    /// * in both
    ///
    /// This sorts the recored fields if not already sorted, then walks them grouping
    /// them according to above. O(n + m)
    pub fn separateFieldsByName(a_record: *SelfR, b_record: *SelfR) error{Overflow}!Separated {
        // First make sure the fields are sorted
        const a_fields = a_record.fields.slice();
        if (!a_record.are_fields_sorted) {
            std.mem.sort(RecordField, a_fields, .{}, comptime fieldNameAsc);
            a_record.are_fields_sorted = true;
        }
        const b_fields = b_record.fields.slice();
        if (!b_record.are_fields_sorted) {
            std.mem.sort(RecordField, b_fields, .{}, comptime fieldNameAsc);
            b_record.are_fields_sorted = true;
        }

        // Create return type
        var separated: Separated = .{
            .in_a = try RecordFieldBoundedArray.init(0),
            .in_b = try RecordFieldBoundedArray.init(0),
            .in_both = try RecordFieldBoundedArray.init(0),
        };

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_fields.len and b_i < b_fields.len) {
            const a_next = a_fields[a_i];
            const b_next = b_fields[b_i];

            if (@intFromEnum(a_next.name) == @intFromEnum(b_next.name)) {
                try separated.in_both.append(a_next);
                a_i = a_i + 1;
                b_i = b_i + 1;
            } else if (@intFromEnum(a_next.name) < @intFromEnum(b_next.name)) {
                try separated.in_a.append(a_next);
                a_i = a_i + 1;
            } else {
                try separated.in_b.append(b_next);
                b_i = b_i + 1;
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_fields.len) {
            const a_next = a_fields[a_i];
            try separated.in_a.append(a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_fields.len) {
            const b_next = b_fields[b_i];
            try separated.in_b.append(b_next);
            b_i = b_i + 1;
        }

        return separated;
    }
};

// tests

test "separateFieldsByName - same record" {
    const field_x = RecordField{ .name = @enumFromInt(1), .typ = .required, .type_var = @enumFromInt(0) };
    const field_y = RecordField{ .name = @enumFromInt(2), .typ = .optional, .type_var = @enumFromInt(1) };

    var record_a = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{ field_x, field_y }) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(0),
        .ext = @enumFromInt(0),
    };

    var record_b = record_a;

    const separated = try record_a.separateFieldsByName(&record_b);

    try std.testing.expectEqual(0, separated.in_a.len);
    try std.testing.expectEqual(0, separated.in_b.len);
    try std.testing.expectEqual(2, separated.in_both.len);
    try std.testing.expectEqual(field_x.name, separated.in_both.buffer[0].name);
    try std.testing.expectEqual(field_y.name, separated.in_both.buffer[1].name);
}

test "separateFieldsByName - disjoint fields" {
    const a1 = RecordField{ .name = @enumFromInt(10), .typ = .required, .type_var = @enumFromInt(0) };
    const a2 = RecordField{ .name = @enumFromInt(20), .typ = .required, .type_var = @enumFromInt(1) };
    const b1 = RecordField{ .name = @enumFromInt(30), .typ = .required, .type_var = @enumFromInt(2) };

    var record_a = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{ a1, a2 }) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(0),
        .ext = @enumFromInt(0),
    };

    var record_b = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{b1}) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(1),
        .ext = @enumFromInt(0),
    };

    const separated = try record_a.separateFieldsByName(&record_b);

    try std.testing.expectEqual(2, separated.in_a.len);
    try std.testing.expectEqual(1, separated.in_b.len);
    try std.testing.expectEqual(0, separated.in_both.len);
}

test "separateFieldsByName - overlapping fields" {
    const a1 = RecordField{ .name = @enumFromInt(10), .typ = .required, .type_var = @enumFromInt(0) };
    const both = RecordField{ .name = @enumFromInt(20), .typ = .optional, .type_var = @enumFromInt(1) };
    const b1 = RecordField{ .name = @enumFromInt(30), .typ = .required, .type_var = @enumFromInt(2) };

    var record_a = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{ a1, both }) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(0),
        .ext = @enumFromInt(0),
    };

    var record_b = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{ b1, both }) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(1),
        .ext = @enumFromInt(0),
    };

    const separated = try record_a.separateFieldsByName(&record_b);

    try std.testing.expectEqual(1, separated.in_a.len);
    try std.testing.expectEqual(1, separated.in_b.len);
    try std.testing.expectEqual(1, separated.in_both.len);

    try std.testing.expectEqual(a1.name, separated.in_a.buffer[0].name);
    try std.testing.expectEqual(b1.name, separated.in_b.buffer[0].name);
    try std.testing.expectEqual(both.name, separated.in_both.buffer[0].name);
}

test "separateFieldsByName - reordering is normalized" {
    const f1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .type_var = @enumFromInt(0) };
    const f2 = RecordField{ .name = @enumFromInt(2), .typ = .optional, .type_var = @enumFromInt(1) };
    const f3 = RecordField{ .name = @enumFromInt(3), .typ = .optional, .type_var = @enumFromInt(2) };

    var record_a = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{ f3, f1, f2 }) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(0),
        .ext = @enumFromInt(0),
    };

    var record_b = Record{
        .fields = RecordFieldBoundedArray.fromSlice(&[_]RecordField{ f1, f2, f3 }) catch unreachable,
        .are_fields_sorted = false,
        .type_var = @enumFromInt(1),
        .ext = @enumFromInt(0),
    };

    const separated = try record_a.separateFieldsByName(&record_b);

    try std.testing.expectEqual(0, separated.in_a.len);
    try std.testing.expectEqual(0, separated.in_b.len);
    try std.testing.expectEqual(3, separated.in_both.len);
    try std.testing.expectEqual(@as(RecordFieldName, @enumFromInt(1)), separated.in_both.buffer[0].name);
    try std.testing.expectEqual(@as(RecordFieldName, @enumFromInt(2)), separated.in_both.buffer[1].name);
    try std.testing.expectEqual(@as(RecordFieldName, @enumFromInt(3)), separated.in_both.buffer[2].name);
}
