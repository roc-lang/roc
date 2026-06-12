//! Small typed Roc source generator for the typecheck fuzzer.
//!
//! This intentionally starts with a narrow, easy-to-audit subset. It generates a
//! type module named `Main` with associated functions whose annotations make the
//! intended types explicit.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

allocator: std.mem.Allocator,
reader: *FuzzReader,
output: std.ArrayList(u8),
name_counter: u32,

const Type = enum {
    bool,
    str,
    list_bool,
    list_str,
    list_u64,
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
    f32,
    f64,

    fn isList(self: Type) bool {
        return switch (self) {
            .list_bool, .list_str, .list_u64 => true,
            else => false,
        };
    }
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .output = .empty,
        .name_counter = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
}

pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

pub fn generateModule(self: *Self) std.mem.Allocator.Error!void {
    try self.write("Main := [Main].{\n");

    try self.generateFunction(.bool);
    try self.generateFunction(.str);
    try self.generateFunction(.u64);
    try self.generateFunction(.list_u64);

    const extra_count = self.reader.intRangeAtMost(u8, 0, 8);
    for (0..extra_count) |_| {
        try self.generateFunction(self.chooseType());
    }

    try self.write("}\n");
}

fn generateFunction(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    const style = self.reader.intRangeAtMost(u8, 0, 3);
    const name_id = self.name_counter;
    self.name_counter += 1;

    switch (style) {
        0 => try self.generateLiteralFunction(name_id, typ),
        1 => try self.generateIfFunction(name_id, typ),
        2 => try self.generateBlockFunction(name_id, typ),
        3 => try self.generateIdentityFunction(name_id, typ),
        else => unreachable,
    }
}

fn generateLiteralFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| ");
    try self.writeLiteral(typ);
    try self.write("\n");
}

fn generateIfFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| if ");
    try self.writeBoolLiteral();
    try self.write(" ");
    try self.writeLiteral(typ);
    try self.write(" else ");
    try self.writeLiteral(typ);
    try self.write("\n");
}

fn generateBlockFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.writeFunctionSignature(name_id, "Main", typ);
    try self.writeFunctionHeader(name_id);
    try self.write("|_| {\n");
    try self.write("        value = ");
    try self.writeLiteral(typ);
    try self.write("\n        value\n    }\n");
}

fn generateIdentityFunction(self: *Self, name_id: u32, typ: Type) std.mem.Allocator.Error!void {
    try self.write("    value");
    try self.output.print(self.allocator, "{d}", .{name_id});
    try self.write(" : Main, ");
    try self.writeType(typ);
    try self.write(" -> ");
    try self.writeType(typ);
    try self.write("\n");

    try self.writeFunctionHeader(name_id);
    try self.write("|_, value| value\n");
}

fn writeFunctionSignature(self: *Self, name_id: u32, arg_type: []const u8, return_type: Type) std.mem.Allocator.Error!void {
    try self.write("    value");
    try self.output.print(self.allocator, "{d}", .{name_id});
    try self.write(" : ");
    try self.write(arg_type);
    try self.write(" -> ");
    try self.writeType(return_type);
    try self.write("\n");
}

fn writeFunctionHeader(self: *Self, name_id: u32) std.mem.Allocator.Error!void {
    try self.write("    value");
    try self.output.print(self.allocator, "{d}", .{name_id});
    try self.write(" = ");
}

fn chooseType(self: *Self) Type {
    return switch (self.reader.intRangeAtMost(u8, 0, 16)) {
        0 => .bool,
        1 => .str,
        2 => .list_bool,
        3 => .list_str,
        4 => .list_u64,
        5 => .u8,
        6 => .i8,
        7 => .u16,
        8 => .i16,
        9 => .u32,
        10 => .i32,
        11 => .u64,
        12 => .i64,
        13 => .u128,
        14 => .i128,
        15 => .f32,
        16 => .f64,
        else => unreachable,
    };
}

fn writeType(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    try self.write(switch (typ) {
        .bool => "Bool",
        .str => "Str",
        .list_bool => "List(Bool)",
        .list_str => "List(Str)",
        .list_u64 => "List(U64)",
        .u8 => "U8",
        .i8 => "I8",
        .u16 => "U16",
        .i16 => "I16",
        .u32 => "U32",
        .i32 => "I32",
        .u64 => "U64",
        .i64 => "I64",
        .u128 => "U128",
        .i128 => "I128",
        .f32 => "F32",
        .f64 => "F64",
    });
}

fn writeLiteral(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .bool => try self.writeBoolLiteral(),
        .str => try self.writeStringLiteral(),
        .list_bool => try self.writeListLiteral(.bool),
        .list_str => try self.writeListLiteral(.str),
        .list_u64 => try self.writeListLiteral(.u64),
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => {
            const value = self.reader.intRangeAtMost(u8, 0, 100);
            try self.output.print(self.allocator, "{d}", .{value});
        },
        .f32, .f64 => {
            const whole = self.reader.intRangeAtMost(u8, 0, 100);
            const frac = self.reader.intRangeAtMost(u8, 0, 99);
            try self.output.print(self.allocator, "{d}.{d}", .{ whole, frac });
        },
    }
}

fn writeListLiteral(self: *Self, elem_type: Type) std.mem.Allocator.Error!void {
    std.debug.assert(!elem_type.isList());

    try self.write("[");
    const len = self.reader.intRangeAtMost(u8, 0, 4);
    for (0..len) |i| {
        if (i > 0) try self.write(", ");
        try self.writeLiteral(elem_type);
    }
    try self.write("]");
}

fn writeBoolLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write(if (self.reader.boolean()) "True" else "False");
}

fn writeStringLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write("\"");
    const len = self.reader.intRangeAtMost(u8, 0, 8);
    for (0..len) |_| {
        const char = self.reader.intRangeAtMost(u8, 'a', 'z');
        try self.output.append(self.allocator, char);
    }
    try self.write("\"");
}

fn write(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.output.appendSlice(self.allocator, text);
}
