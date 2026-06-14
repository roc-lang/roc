//! Typed Roc source generator for the typecheck fuzzer.
//!
//! Userspace Roc identifiers emitted by this generator must come from `fresh`.
//! Builtin type, tag, and function names are the only hardcoded Roc names here.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

allocator: std.mem.Allocator,
reader: *FuzzReader,
output: std.ArrayList(u8),
root_file_name: ?[]u8,
next_id: u32,
symbols: RootSymbols,

const SymbolKind = enum {
    type,
    tag,
    field,
    value,
    function,
};

const Symbol = struct {
    kind: SymbolKind,
    id: u32,
};

const RootSymbols = struct {
    typ: Symbol,
    tag0: Symbol,
    tag1: Symbol,
    tag2: Symbol,
    tag3: Symbol,
    field0: Symbol,
    field1: Symbol,
    field2: Symbol,
};

const Type = enum {
    root,
    bool,
    str,
    u64,
    record,
    list_u64,
    try_u64_str,
    tuple_bool_u64,
};

const Template = enum {
    root_identity,
    root_literal,
    root_to_u64,
    root_to_str,
    root_to_record,
    record_literal,
    record_access,
    record_update,
    list_map,
    list_fold,
    list_pattern,
    try_literal,
    try_match,
    tuple_match,
    local_function,
    closure_capture,
    local_type,
    row_record,
    str_builtin,
    list_get,
};

const all_templates = [_]Template{
    .root_identity,
    .root_literal,
    .root_to_u64,
    .root_to_str,
    .root_to_record,
    .record_literal,
    .record_access,
    .record_update,
    .list_map,
    .list_fold,
    .list_pattern,
    .try_literal,
    .try_match,
    .tuple_match,
    .local_function,
    .closure_capture,
    .local_type,
    .row_record,
    .str_builtin,
    .list_get,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .output = .empty,
        .root_file_name = null,
        .next_id = 0,
        .symbols = undefined,
    };
}

pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
    if (self.root_file_name) |name| self.allocator.free(name);
}

pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

pub fn getRootFileName(self: *const Self) []const u8 {
    return self.root_file_name orelse @panic("typecheck fuzzer root file name requested before generation");
}

pub fn generateModule(self: *Self) std.mem.Allocator.Error!void {
    self.output.clearRetainingCapacity();
    self.next_id = 0;

    self.symbols = .{
        .typ = self.fresh(.type),
        .tag0 = self.fresh(.tag),
        .tag1 = self.fresh(.tag),
        .tag2 = self.fresh(.tag),
        .tag3 = self.fresh(.tag),
        .field0 = self.fresh(.field),
        .field1 = self.fresh(.field),
        .field2 = self.fresh(.field),
    };

    try self.setRootFileName(self.symbols.typ);
    try self.writeRootTypeHeader();

    for (all_templates) |template| {
        try self.generateTemplate(template);
    }

    const extra_count = self.reader.intRangeAtMost(u8, 12, 48);
    for (0..extra_count) |_| {
        const index = self.reader.intRangeLessThan(usize, 0, all_templates.len);
        try self.generateTemplate(all_templates[index]);
    }

    try self.write("}\n");
}

fn setRootFileName(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    if (self.root_file_name) |name| self.allocator.free(name);
    self.root_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(symbol.kind), symbol.id });
}

fn writeRootTypeHeader(self: *Self) std.mem.Allocator.Error!void {
    try self.writeSymbol(self.symbols.typ);
    try self.write(" := [");
    try self.writeSymbol(self.symbols.tag0);
    try self.write(", ");
    try self.writeSymbol(self.symbols.tag1);
    try self.write("(U64), ");
    try self.writeSymbol(self.symbols.tag2);
    try self.write("(Str), ");
    try self.writeSymbol(self.symbols.tag3);
    try self.write("(");
    try self.writeType(.record);
    try self.write(")].{\n");
}

fn generateTemplate(self: *Self, template: Template) std.mem.Allocator.Error!void {
    switch (template) {
        .root_identity => try self.generateRootIdentity(),
        .root_literal => try self.generateRootLiteralFunction(),
        .root_to_u64 => try self.generateRootToU64(),
        .root_to_str => try self.generateRootToStr(),
        .root_to_record => try self.generateRootToRecord(),
        .record_literal => try self.generateRecordLiteralFunction(),
        .record_access => try self.generateRecordAccess(),
        .record_update => try self.generateRecordUpdate(),
        .list_map => try self.generateListMap(),
        .list_fold => try self.generateListFold(),
        .list_pattern => try self.generateListPattern(),
        .try_literal => try self.generateTryLiteralFunction(),
        .try_match => try self.generateTryMatch(),
        .tuple_match => try self.generateTupleMatch(),
        .local_function => try self.generateLocalFunction(),
        .closure_capture => try self.generateClosureCapture(),
        .local_type => try self.generateLocalType(),
        .row_record => try self.generateRowRecord(),
        .str_builtin => try self.generateStrBuiltin(),
        .list_get => try self.generateListGet(),
    }
}

fn generateRootIdentity(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const arg = self.fresh(.value);
    try self.writeUnaryFunctionHeader(function, .root, .root, arg);
    try self.writeSymbol(arg);
    try self.write("\n");
}

fn generateRootLiteralFunction(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    try self.writeIgnoredUnaryFunctionHeader(function, .root, .root);
    try self.writeRootLiteral();
    try self.write("\n");
}

fn generateRootToU64(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const arg = self.fresh(.value);
    const payload0 = self.fresh(.value);
    const payload1 = self.fresh(.value);
    const payload2 = self.fresh(.value);

    try self.writeUnaryFunctionHeader(function, .root, .u64, arg);
    try self.write("match ");
    try self.writeSymbol(arg);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag0);
    try self.write(" => ");
    try self.writeU64Literal();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag1);
    try self.write("(");
    try self.writeSymbol(payload0);
    try self.write(") => ");
    try self.writeSymbol(payload0);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag2);
    try self.write("(");
    try self.writeSymbol(payload1);
    try self.write(") => Str.count_utf8_bytes(");
    try self.writeSymbol(payload1);
    try self.write(")\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag3);
    try self.write("(");
    try self.writeSymbol(payload2);
    try self.write(") => ");
    try self.writeSymbol(payload2);
    try self.write(".");
    try self.writeSymbol(self.symbols.field1);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateRootToStr(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const arg = self.fresh(.value);
    const payload0 = self.fresh(.value);
    const payload1 = self.fresh(.value);
    const payload2 = self.fresh(.value);

    try self.writeUnaryFunctionHeader(function, .root, .str, arg);
    try self.write("match ");
    try self.writeSymbol(arg);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag0);
    try self.write(" => ");
    try self.writeStringLiteral();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag1);
    try self.write("(");
    try self.writeSymbol(payload0);
    try self.write(") => if ");
    try self.writeSymbol(payload0);
    try self.write(" == 0 ");
    try self.writeStringLiteral();
    try self.write(" else ");
    try self.writeStringLiteral();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag2);
    try self.write("(");
    try self.writeSymbol(payload1);
    try self.write(") => ");
    try self.writeSymbol(payload1);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag3);
    try self.write("(");
    try self.writeSymbol(payload2);
    try self.write(") => ");
    try self.writeSymbol(payload2);
    try self.write(".");
    try self.writeSymbol(self.symbols.field2);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateRootToRecord(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const arg = self.fresh(.value);
    const payload0 = self.fresh(.value);
    const payload1 = self.fresh(.value);
    const payload2 = self.fresh(.value);

    try self.writeUnaryFunctionHeader(function, .root, .record, arg);
    try self.write("match ");
    try self.writeSymbol(arg);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag0);
    try self.write(" => ");
    try self.writeRecordLiteral();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag1);
    try self.write("(");
    try self.writeSymbol(payload0);
    try self.write(") => { ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeSymbol(payload0);
    try self.write(" > 0, ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeSymbol(payload0);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field2);
    try self.write(": ");
    try self.writeStringLiteral();
    try self.write(" }\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag2);
    try self.write("(");
    try self.writeSymbol(payload1);
    try self.write(") => { ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": Str.is_empty(");
    try self.writeSymbol(payload1);
    try self.write("), ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": Str.count_utf8_bytes(");
    try self.writeSymbol(payload1);
    try self.write("), ");
    try self.writeSymbol(self.symbols.field2);
    try self.write(": ");
    try self.writeSymbol(payload1);
    try self.write(" }\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag3);
    try self.write("(");
    try self.writeSymbol(payload2);
    try self.write(") => ");
    try self.writeSymbol(payload2);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateRecordLiteralFunction(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    try self.writeIgnoredUnaryFunctionHeader(function, .root, .record);
    try self.writeRecordLiteral();
    try self.write("\n");
}

fn generateRecordAccess(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const local = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" = ");
    try self.writeRecordLiteral();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(".");
    try self.writeSymbol(self.symbols.field1);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateRecordUpdate(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const local = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .record);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" = ");
    try self.writeRecordLiteral();
    try self.write("\n");
    try self.writeIndent(2);
    try self.write("{ ..");
    try self.writeSymbol(local);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeSymbol(local);
    try self.write(".");
    try self.writeSymbol(self.symbols.field1);
    try self.write(" + ");
    try self.writeU64Literal();
    try self.write(" }\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateListMap(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const item = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .list_u64);
    try self.writeListU64Literal(.nonempty);
    try self.write(".map(|");
    try self.writeSymbol(item);
    try self.write("| ");
    try self.writeSymbol(item);
    try self.write(" + ");
    try self.writeU64Literal();
    try self.write(")\n");
}

fn generateListFold(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const acc = self.fresh(.value);
    const item = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.writeListU64Literal(.nonempty);
    try self.write(".fold(0, |");
    try self.writeSymbol(acc);
    try self.write(", ");
    try self.writeSymbol(item);
    try self.write("| ");
    try self.writeSymbol(acc);
    try self.write(" + ");
    try self.writeSymbol(item);
    try self.write(")\n");
}

fn generateListPattern(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const head = self.fresh(.value);
    const tail = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .list_u64);
    try self.write("match ");
    try self.writeListU64Literal(.nonempty);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.write("[");
    try self.writeSymbol(head);
    try self.write(", .. as ");
    try self.writeSymbol(tail);
    try self.write("] => ");
    try self.writeSymbol(tail);
    try self.write(".append(");
    try self.writeSymbol(head);
    try self.write(")\n");
    try self.writeIndent(2);
    try self.write("[] => []\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateTryLiteralFunction(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .try_u64_str);
    try self.write("if Bool.not(False) Ok(");
    try self.writeU64Literal();
    try self.write(") else Err(");
    try self.writeStringLiteral();
    try self.write(")\n");
}

fn generateTryMatch(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const local = self.fresh(.value);
    const ok_payload = self.fresh(.value);
    const err_payload = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .try_u64_str);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" = ");
    try self.writeTryLiteral();
    try self.write("\n");
    try self.writeIndent(2);
    try self.write("match ");
    try self.writeSymbol(local);
    try self.write(" {\n");
    try self.writeIndent(3);
    try self.write("Ok(");
    try self.writeSymbol(ok_payload);
    try self.write(") => Ok(");
    try self.writeSymbol(ok_payload);
    try self.write(" + ");
    try self.writeU64Literal();
    try self.write(")\n");
    try self.writeIndent(3);
    try self.write("Err(");
    try self.writeSymbol(err_payload);
    try self.write(") => Err(");
    try self.writeSymbol(err_payload);
    try self.write(")\n");
    try self.writeIndent(2);
    try self.write("}\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateTupleMatch(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const value = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.write("match ");
    try self.writeTupleLiteral();
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.write("(_, ");
    try self.writeSymbol(value);
    try self.write(") => ");
    try self.writeSymbol(value);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateLocalFunction(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const local = self.fresh(.value);
    const arg = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" : U64 -> U64\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" = |");
    try self.writeSymbol(arg);
    try self.write("| ");
    try self.writeSymbol(arg);
    try self.write(" + ");
    try self.writeU64Literal();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write("(");
    try self.writeU64Literal();
    try self.write(")\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateClosureCapture(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const captured = self.fresh(.value);
    const item = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(captured);
    try self.write(" = ");
    try self.writeU64Literal();
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeListU64Literal(.nonempty);
    try self.write(".map(|");
    try self.writeSymbol(item);
    try self.write("| ");
    try self.writeSymbol(item);
    try self.write(" + ");
    try self.writeSymbol(captured);
    try self.write(").len()\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateLocalType(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const typ = self.fresh(.type);
    const tag = self.fresh(.tag);
    const local = self.fresh(.value);
    const payload = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(typ);
    try self.write(" := [");
    try self.writeSymbol(tag);
    try self.write("(U64)]\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" : ");
    try self.writeSymbol(typ);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" = ");
    try self.writeSymbol(tag);
    try self.write("(");
    try self.writeU64Literal();
    try self.write(")\n");
    try self.writeIndent(2);
    try self.write("match ");
    try self.writeSymbol(local);
    try self.write(" {\n");
    try self.writeIndent(3);
    try self.writeSymbol(tag);
    try self.write("(");
    try self.writeSymbol(payload);
    try self.write(") => ");
    try self.writeSymbol(payload);
    try self.write("\n");
    try self.writeIndent(2);
    try self.write("}\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateRowRecord(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const arg = self.fresh(.value);

    try self.writeIndent(1);
    try self.writeSymbol(function);
    try self.write(" : { ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(" : U64, .. } -> U64\n");
    try self.writeIndent(1);
    try self.writeSymbol(function);
    try self.write(" = |");
    try self.writeSymbol(arg);
    try self.write("| ");
    try self.writeSymbol(arg);
    try self.write(".");
    try self.writeSymbol(self.symbols.field1);
    try self.write("\n");
}

fn generateStrBuiltin(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const local = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .str);
    try self.write("{\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(" = Str.concat(");
    try self.writeStringLiteral();
    try self.write(", ");
    try self.writeStringLiteral();
    try self.write(")\n");
    try self.writeIndent(2);
    try self.writeSymbol(local);
    try self.write(".trim()\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn generateListGet(self: *Self) std.mem.Allocator.Error!void {
    const function = self.fresh(.function);
    const ok_payload = self.fresh(.value);

    try self.writeIgnoredUnaryFunctionHeader(function, .root, .u64);
    try self.write("match List.get(");
    try self.writeListU64Literal(.nonempty);
    try self.write(", ");
    try self.writeU64Literal();
    try self.write(") {\n");
    try self.writeIndent(2);
    try self.write("Ok(");
    try self.writeSymbol(ok_payload);
    try self.write(") => ");
    try self.writeSymbol(ok_payload);
    try self.write("\n");
    try self.writeIndent(2);
    try self.write("Err(_) => ");
    try self.writeU64Literal();
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}\n");
}

fn writeUnaryFunctionHeader(
    self: *Self,
    function: Symbol,
    param_type: Type,
    result_type: Type,
    arg: Symbol,
) std.mem.Allocator.Error!void {
    try self.writeIndent(1);
    try self.writeSymbol(function);
    try self.write(" : ");
    try self.writeType(param_type);
    try self.write(" -> ");
    try self.writeType(result_type);
    try self.write("\n");
    try self.writeIndent(1);
    try self.writeSymbol(function);
    try self.write(" = |");
    try self.writeSymbol(arg);
    try self.write("| ");
}

fn writeIgnoredUnaryFunctionHeader(
    self: *Self,
    function: Symbol,
    param_type: Type,
    result_type: Type,
) std.mem.Allocator.Error!void {
    try self.writeIndent(1);
    try self.writeSymbol(function);
    try self.write(" : ");
    try self.writeType(param_type);
    try self.write(" -> ");
    try self.writeType(result_type);
    try self.write("\n");
    try self.writeIndent(1);
    try self.writeSymbol(function);
    try self.write(" = |_| ");
}

fn writeType(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .root => try self.writeSymbol(self.symbols.typ),
        .bool => try self.write("Bool"),
        .str => try self.write("Str"),
        .u64 => try self.write("U64"),
        .record => {
            try self.write("{ ");
            try self.writeSymbol(self.symbols.field0);
            try self.write(" : Bool, ");
            try self.writeSymbol(self.symbols.field1);
            try self.write(" : U64, ");
            try self.writeSymbol(self.symbols.field2);
            try self.write(" : Str }");
        },
        .list_u64 => try self.write("List(U64)"),
        .try_u64_str => try self.write("Try(U64, Str)"),
        .tuple_bool_u64 => try self.write("(Bool, U64)"),
    }
}

fn writeRootLiteral(self: *Self) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeSymbol(self.symbols.tag0),
        1 => {
            try self.writeSymbol(self.symbols.tag1);
            try self.write("(");
            try self.writeU64Literal();
            try self.write(")");
        },
        2 => {
            try self.writeSymbol(self.symbols.tag2);
            try self.write("(");
            try self.writeStringLiteral();
            try self.write(")");
        },
        3 => {
            try self.writeSymbol(self.symbols.tag3);
            try self.write("(");
            try self.writeRecordLiteral();
            try self.write(")");
        },
        else => unreachable,
    }
}

fn writeRecordLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write("{ ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeBoolLiteral();
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeU64Literal();
    try self.write(", ");
    try self.writeSymbol(self.symbols.field2);
    try self.write(": ");
    try self.writeStringLiteral();
    try self.write(" }");
}

const ListShape = enum { maybe_empty, nonempty };

fn writeListU64Literal(self: *Self, shape: ListShape) std.mem.Allocator.Error!void {
    const count = switch (shape) {
        .maybe_empty => self.reader.intRangeAtMost(u8, 0, 4),
        .nonempty => self.reader.intRangeAtMost(u8, 1, 4),
    };

    try self.write("[");
    for (0..count) |index| {
        if (index > 0) try self.write(", ");
        try self.writeU64Literal();
    }
    try self.write("]");
}

fn writeTryLiteral(self: *Self) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("Ok(");
        try self.writeU64Literal();
        try self.write(")");
    } else {
        try self.write("Err(");
        try self.writeStringLiteral();
        try self.write(")");
    }
}

fn writeTupleLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write("(");
    try self.writeBoolLiteral();
    try self.write(", ");
    try self.writeU64Literal();
    try self.write(")");
}

fn writeBoolLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write(if (self.reader.boolean()) "True" else "False");
}

fn writeU64Literal(self: *Self) std.mem.Allocator.Error!void {
    const value = self.reader.intRangeAtMost(u8, 0, 100);
    try self.output.print(self.allocator, "{d}", .{value});
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

fn fresh(self: *Self, kind: SymbolKind) Symbol {
    const symbol: Symbol = .{ .kind = kind, .id = self.next_id };
    self.next_id += 1;
    return symbol;
}

fn writeSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try self.output.print(self.allocator, "{c}{d}", .{ symbolPrefix(symbol.kind), symbol.id });
}

fn symbolPrefix(kind: SymbolKind) u8 {
    return switch (kind) {
        .type => 'T',
        .tag => 'A',
        .field => 'r',
        .value => 'v',
        .function => 'f',
    };
}

fn writeIndent(self: *Self, count: usize) std.mem.Allocator.Error!void {
    for (0..count) |_| try self.write("    ");
}

fn write(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.output.appendSlice(self.allocator, text);
}
