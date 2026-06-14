//! Typed Roc source generator for the typecheck fuzzer.
//!
//! Userspace Roc identifiers emitted by this generator must come from `fresh`.
//! Builtin type, tag, and function names are the only hardcoded Roc names here.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

const max_methods = 256;

allocator: std.mem.Allocator,
reader: *FuzzReader,
output: std.ArrayList(u8),
root_file_name: ?[]u8,
next_id: u32,
symbols: RootSymbols,
methods: [max_methods]Method,
method_count: usize,

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

const Method = struct {
    symbol: Symbol,
    param_type: Type,
    result_type: Type,
};

const ExprContext = struct {
    arg: Symbol,
    arg_type: Type,
};

const all_types = [_]Type{
    .root,
    .bool,
    .str,
    .u64,
    .record,
    .list_u64,
    .try_u64_str,
    .tuple_bool_u64,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .output = .empty,
        .root_file_name = null,
        .next_id = 0,
        .symbols = undefined,
        .methods = undefined,
        .method_count = 0,
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
    self.method_count = 0;

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
    try self.generateSeedMethods();

    const method_count = self.reader.intRangeAtMost(u8, 48, 128);
    for (0..method_count) |_| {
        const param_type = self.chooseType();
        const result_type = self.chooseType();
        const depth = self.reader.intRangeAtMost(u8, 2, 5);
        try self.generateMethod(param_type, result_type, depth);
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

fn generateSeedMethods(self: *Self) std.mem.Allocator.Error!void {
    for (all_types) |result_type| {
        try self.generateMethod(.root, result_type, 0);
    }
    for (all_types) |param_type| {
        try self.generateMethod(param_type, param_type, 1);
    }
}

fn generateMethod(self: *Self, param_type: Type, result_type: Type, depth: u8) std.mem.Allocator.Error!void {
    if (self.method_count >= self.methods.len) return;

    const method: Method = .{
        .symbol = self.fresh(.function),
        .param_type = param_type,
        .result_type = result_type,
    };
    const arg = self.fresh(.value);
    const visible_methods = self.method_count;

    try self.writeIndent(1);
    try self.writeSymbol(method.symbol);
    try self.write(" : ");
    try self.writeType(param_type);
    try self.write(" -> ");
    try self.writeType(result_type);
    try self.write("\n");
    try self.writeIndent(1);
    try self.writeSymbol(method.symbol);
    try self.write(" = |");
    try self.writeSymbol(arg);
    try self.write("| ");
    try self.writeExpr(result_type, .{ .arg = arg, .arg_type = param_type }, depth, visible_methods);
    try self.write("\n");

    self.methods[self.method_count] = method;
    self.method_count += 1;
}

fn writeExpr(self: *Self, typ: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (depth == 0) {
        try self.writeLeafExpr(typ, context, visible_methods);
        return;
    }

    switch (typ) {
        .root => try self.writeRootExpr(context, depth, visible_methods),
        .bool => try self.writeBoolExpr(context, depth, visible_methods),
        .str => try self.writeStrExpr(context, depth, visible_methods),
        .u64 => try self.writeU64Expr(context, depth, visible_methods),
        .record => try self.writeRecordExpr(context, depth, visible_methods),
        .list_u64 => try self.writeListU64Expr(context, depth, visible_methods),
        .try_u64_str => try self.writeTryExpr(context, depth, visible_methods),
        .tuple_bool_u64 => try self.writeTupleExpr(context, depth, visible_methods),
    }
}

fn writeLeafExpr(self: *Self, typ: Type, context: ExprContext, visible_methods: usize) std.mem.Allocator.Error!void {
    if (context.arg_type == typ and self.reader.boolean()) {
        try self.writeSymbol(context.arg);
        return;
    }
    if (self.reader.boolean() and try self.writeMethodCall(typ, context, 0, visible_methods)) return;
    try self.writeLiteral(typ);
}

fn writeRootExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 4)) {
        0 => if (context.arg_type == .root) try self.writeSymbol(context.arg) else try self.writeRootLiteral(context, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.root, context, depth - 1, visible_methods)) try self.writeRootLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.root, context, depth - 1, visible_methods),
        3 => try self.writeRootLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.root, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeBoolExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => if (context.arg_type == .bool) try self.writeSymbol(context.arg) else try self.writeBoolLiteral(),
        1 => if (!try self.writeMethodCall(.bool, context, depth - 1, visible_methods)) try self.writeBoolLiteral(),
        2 => try self.writeIfExpr(.bool, context, depth - 1, visible_methods),
        3 => {
            try self.write("Bool.not(");
            try self.writeExpr(.bool, context, depth - 1, visible_methods);
            try self.write(")");
        },
        4 => {
            try self.write("Str.is_empty(");
            try self.writeExpr(.str, context, depth - 1, visible_methods);
            try self.write(")");
        },
        5 => {
            try self.write("List.is_empty(");
            try self.writeExpr(.list_u64, context, depth - 1, visible_methods);
            try self.write(")");
        },
        6 => {
            try self.writeParenthesizedExpr(.u64, context, depth - 1, visible_methods);
            try self.write(" == ");
            try self.writeParenthesizedExpr(.u64, context, depth - 1, visible_methods);
        },
        7 => try self.writeRootMatchExpr(.bool, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeStrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 6)) {
        0 => if (context.arg_type == .str) try self.writeSymbol(context.arg) else try self.writeStringLiteral(),
        1 => if (!try self.writeMethodCall(.str, context, depth - 1, visible_methods)) try self.writeStringLiteral(),
        2 => try self.writeIfExpr(.str, context, depth - 1, visible_methods),
        3 => {
            try self.write("Str.concat(");
            try self.writeExpr(.str, context, depth - 1, visible_methods);
            try self.write(", ");
            try self.writeExpr(.str, context, depth - 1, visible_methods);
            try self.write(")");
        },
        4 => {
            try self.writeParenthesizedExpr(.str, context, depth - 1, visible_methods);
            try self.write(".trim()");
        },
        5 => try self.writeStringLiteral(),
        6 => try self.writeRootMatchExpr(.str, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeU64Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 8)) {
        0 => if (context.arg_type == .u64) try self.writeSymbol(context.arg) else try self.writeU64Literal(),
        1 => if (!try self.writeMethodCall(.u64, context, depth - 1, visible_methods)) try self.writeU64Literal(),
        2 => try self.writeIfExpr(.u64, context, depth - 1, visible_methods),
        3 => {
            try self.write("Str.count_utf8_bytes(");
            try self.writeExpr(.str, context, depth - 1, visible_methods);
            try self.write(")");
        },
        4 => {
            try self.write("List.len(");
            try self.writeExpr(.list_u64, context, depth - 1, visible_methods);
            try self.write(")");
        },
        5 => {
            try self.writeParenthesizedExpr(.u64, context, depth - 1, visible_methods);
            try self.write(" + ");
            try self.writeParenthesizedExpr(.u64, context, depth - 1, visible_methods);
        },
        6 => {
            try self.writeParenthesizedExpr(.record, context, depth - 1, visible_methods);
            try self.write(".");
            try self.writeSymbol(self.symbols.field1);
        },
        7 => try self.writeRootMatchExpr(.u64, context, depth - 1, visible_methods),
        8 => try self.writeListGetExpr(context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeRecordExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 5)) {
        0 => if (context.arg_type == .record) try self.writeSymbol(context.arg) else try self.writeRecordLiteral(context, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.record, context, depth - 1, visible_methods)) try self.writeRecordLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.record, context, depth - 1, visible_methods),
        3 => try self.writeRecordLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRecordUpdateExpr(context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.record, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeListU64Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 5)) {
        0 => if (context.arg_type == .list_u64) try self.writeSymbol(context.arg) else try self.writeListLiteral(context, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.list_u64, context, depth - 1, visible_methods)) try self.writeListLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.list_u64, context, depth - 1, visible_methods),
        3 => try self.writeListLiteral(context, depth - 1, visible_methods),
        4 => try self.writeListMapExpr(context, depth - 1, visible_methods),
        5 => try self.writeListMatchExpr(context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeTryExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 5)) {
        0 => if (context.arg_type == .try_u64_str) try self.writeSymbol(context.arg) else try self.writeTryLiteral(context, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.try_u64_str, context, depth - 1, visible_methods)) try self.writeTryLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.try_u64_str, context, depth - 1, visible_methods),
        3 => try self.writeTryLiteral(context, depth - 1, visible_methods),
        4 => try self.writeTryMatchExpr(context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.try_u64_str, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeTupleExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 4)) {
        0 => if (context.arg_type == .tuple_bool_u64) try self.writeSymbol(context.arg) else try self.writeTupleLiteral(context, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.tuple_bool_u64, context, depth - 1, visible_methods)) try self.writeTupleLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        3 => try self.writeTupleLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    const method = self.chooseMethod(result_type, visible_methods) orelse return false;

    try self.writeSymbol(self.symbols.typ);
    try self.write(".");
    try self.writeSymbol(method.symbol);
    try self.write("(");
    try self.writeExpr(method.param_type, context, depth, visible_methods);
    try self.write(")");
    return true;
}

fn chooseMethod(self: *Self, result_type: Type, visible_methods: usize) ?Method {
    var count: usize = 0;
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type == result_type) count += 1;
    }
    if (count == 0) return null;

    var index = self.reader.intRangeLessThan(usize, 0, count);
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type != result_type) continue;
        if (index == 0) return method;
        index -= 1;
    }
    unreachable;
}

fn writeIfExpr(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("if ");
    try self.writeParenthesizedExpr(.bool, context, depth, visible_methods);
    try self.write(" ");
    try self.writeParenthesizedExpr(result_type, context, depth, visible_methods);
    try self.write(" else ");
    try self.writeParenthesizedExpr(result_type, context, depth, visible_methods);
}

fn writeParenthesizedExpr(self: *Self, typ: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("(");
    try self.writeExpr(typ, context, depth, visible_methods);
    try self.write(")");
}

fn writeRootMatchExpr(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const u64_payload = self.fresh(.value);
    const str_payload = self.fresh(.value);
    const record_payload = self.fresh(.value);

    try self.write("match ");
    try self.writeTypedRootScrutinee(context, depth, visible_methods);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag0);
    try self.write(" => ");
    try self.writeExpr(result_type, context, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag1);
    try self.write("(");
    try self.writeSymbol(u64_payload);
    try self.write(") => ");
    try self.writeExpr(result_type, .{ .arg = u64_payload, .arg_type = .u64 }, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag2);
    try self.write("(");
    try self.writeSymbol(str_payload);
    try self.write(") => ");
    try self.writeExpr(result_type, .{ .arg = str_payload, .arg_type = .str }, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag3);
    try self.write("(");
    try self.writeSymbol(record_payload);
    try self.write(") => ");
    try self.writeExpr(result_type, .{ .arg = record_payload, .arg_type = .record }, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeTypedRootScrutinee(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (context.arg_type == .root and self.reader.boolean()) {
        try self.writeSymbol(context.arg);
        return;
    }
    if (try self.writeMethodCall(.root, context, depth, visible_methods)) return;
    try self.writeRootLiteral(context, depth, visible_methods);
}

fn writeListMapExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const item = self.fresh(.value);

    try self.writeParenthesizedExpr(.list_u64, context, depth, visible_methods);
    try self.write(".map(|");
    try self.writeSymbol(item);
    try self.write("| ");
    try self.writeExpr(.u64, .{ .arg = item, .arg_type = .u64 }, depth, visible_methods);
    try self.write(")");
}

fn writeListMatchExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const head = self.fresh(.value);
    const tail = self.fresh(.value);

    try self.write("match ");
    try self.writeExpr(.list_u64, context, depth, visible_methods);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.write("[");
    try self.writeSymbol(head);
    try self.write(", .. as ");
    try self.writeSymbol(tail);
    try self.write("] => ");
    try self.writeSymbol(tail);
    try self.write(".append(");
    try self.writeExpr(.u64, .{ .arg = head, .arg_type = .u64 }, depth, visible_methods);
    try self.write(")\n");
    try self.writeIndent(2);
    try self.write("[] => []\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeListGetExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const ok_payload = self.fresh(.value);

    try self.write("match List.get(");
    try self.writeExpr(.list_u64, context, depth, visible_methods);
    try self.write(", ");
    try self.writeU64Literal();
    try self.write(") {\n");
    try self.writeIndent(2);
    try self.write("Ok(");
    try self.writeSymbol(ok_payload);
    try self.write(") => ");
    try self.writeExpr(.u64, .{ .arg = ok_payload, .arg_type = .u64 }, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(2);
    try self.write("Err(_) => ");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeRecordUpdateExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("{ ..(");
    try self.writeExpr(.record, context, depth, visible_methods);
    try self.write(")");
    try self.write(", ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeExpr(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write(" }");
}

fn writeTryMatchExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const ok_payload = self.fresh(.value);
    const err_payload = self.fresh(.value);

    try self.write("match ");
    try self.writeTypedTryScrutinee(context, depth, visible_methods);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.write("Ok(");
    try self.writeSymbol(ok_payload);
    try self.write(") => Ok(");
    try self.writeExpr(.u64, .{ .arg = ok_payload, .arg_type = .u64 }, depth, visible_methods);
    try self.write(")\n");
    try self.writeIndent(2);
    try self.write("Err(");
    try self.writeSymbol(err_payload);
    try self.write(") => Err(");
    try self.writeExpr(.str, .{ .arg = err_payload, .arg_type = .str }, depth, visible_methods);
    try self.write(")\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeTypedTryScrutinee(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (context.arg_type == .try_u64_str and self.reader.boolean()) {
        try self.writeSymbol(context.arg);
        return;
    }
    if (try self.writeMethodCall(.try_u64_str, context, depth, visible_methods)) return;
    try self.writeTryLiteral(context, depth, visible_methods);
}

fn writeLiteral(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .root => try self.writeRootLiteral(.{ .arg = self.symbols.tag0, .arg_type = .root }, 0, self.method_count),
        .bool => try self.writeBoolLiteral(),
        .str => try self.writeStringLiteral(),
        .u64 => try self.writeU64Literal(),
        .record => try self.writeRecordLiteral(.{ .arg = self.symbols.tag0, .arg_type = .root }, 0, self.method_count),
        .list_u64 => try self.writeListLiteral(.{ .arg = self.symbols.tag0, .arg_type = .root }, 0, self.method_count),
        .try_u64_str => try self.writeTryLiteral(.{ .arg = self.symbols.tag0, .arg_type = .root }, 0, self.method_count),
        .tuple_bool_u64 => try self.writeTupleLiteral(.{ .arg = self.symbols.tag0, .arg_type = .root }, 0, self.method_count),
    }
}

fn writeRootLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeSymbol(self.symbols.tag0),
        1 => {
            try self.writeSymbol(self.symbols.tag1);
            try self.write("(");
            try self.writeExpr(.u64, context, depth, visible_methods);
            try self.write(")");
        },
        2 => {
            try self.writeSymbol(self.symbols.tag2);
            try self.write("(");
            try self.writeExpr(.str, context, depth, visible_methods);
            try self.write(")");
        },
        3 => {
            try self.writeSymbol(self.symbols.tag3);
            try self.write("(");
            try self.writeExpr(.record, context, depth, visible_methods);
            try self.write(")");
        },
        else => unreachable,
    }
}

fn writeRecordLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("{ ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeExpr(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field2);
    try self.write(": ");
    try self.writeExpr(.str, context, depth, visible_methods);
    try self.write(" }");
}

fn writeListLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const count = self.reader.intRangeAtMost(u8, 0, 4);
    try self.write("[");
    for (0..count) |index| {
        if (index > 0) try self.write(", ");
        try self.writeExpr(.u64, context, depth, visible_methods);
    }
    try self.write("]");
}

fn writeTryLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("Ok(");
        try self.writeExpr(.u64, context, depth, visible_methods);
        try self.write(")");
    } else {
        try self.write("Err(");
        try self.writeExpr(.str, context, depth, visible_methods);
        try self.write(")");
    }
}

fn writeTupleLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("(");
    try self.writeExpr(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write(")");
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

fn chooseType(self: *Self) Type {
    return all_types[self.reader.intRangeLessThan(usize, 0, all_types.len)];
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
