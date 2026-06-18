//! Typed Roc type-module generator for fuzzers.
//!
//! Userspace Roc identifiers emitted by this generator must come from `fresh`.
//! Builtin type, tag, and function names are the only hardcoded Roc names here.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

const max_methods = 256;
pub const max_method_arity = 2;
const max_local_bindings = 4;
const max_context_values = 32;

allocator: std.mem.Allocator,
reader: *FuzzReader,
output: std.ArrayList(u8),
root_file_name: ?[]u8,
next_id: u32,
symbols: RootSymbols,
methods: [max_methods]Method,
method_count: usize,

pub const SymbolKind = enum {
    app_file,
    platform_file,
    package,
    type,
    tag,
    field,
    value,
    function,
};

pub const Symbol = struct {
    kind: SymbolKind,
    id: u32,
};

pub const RootSymbols = struct {
    typ: Symbol,
    tag0: Symbol,
    tag1: Symbol,
    tag2: Symbol,
    tag3: Symbol,
    field0: Symbol,
    field1: Symbol,
    field2: Symbol,
};

pub const Type = enum {
    root,
    bool,
    str,
    u64,
    record,
    list_u64,
    try_u64_str,
    tuple_bool_u64,
};

pub const Method = struct {
    symbol: Symbol,
    param_types: [max_method_arity]Type,
    arity: u8,
    result_type: Type,
};

const ExprContext = struct {
    args: [max_context_values]Symbol,
    arg_types: [max_context_values]Type,
    arity: u8,
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

pub fn getRootSymbols(self: *const Self) RootSymbols {
    return self.symbols;
}

pub fn getMethods(self: *const Self) []const Method {
    return self.methods[0..self.method_count];
}

pub fn freshSymbol(self: *Self, kind: SymbolKind) Symbol {
    return self.fresh(kind);
}

pub fn writeSymbolTo(output: *std.ArrayList(u8), allocator: std.mem.Allocator, symbol: Symbol) std.mem.Allocator.Error!void {
    try output.print(allocator, "{c}{d}", .{ symbolPrefix(symbol.kind), symbol.id });
}

pub fn writeTypeTo(self: *const Self, output: *std.ArrayList(u8), allocator: std.mem.Allocator, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .root => try writeSymbolTo(output, allocator, self.symbols.typ),
        .bool => try output.appendSlice(allocator, "Bool"),
        .str => try output.appendSlice(allocator, "Str"),
        .u64 => try output.appendSlice(allocator, "U64"),
        .record => {
            try output.appendSlice(allocator, "{ ");
            try writeSymbolTo(output, allocator, self.symbols.field0);
            try output.appendSlice(allocator, " : Bool, ");
            try writeSymbolTo(output, allocator, self.symbols.field1);
            try output.appendSlice(allocator, " : U64, ");
            try writeSymbolTo(output, allocator, self.symbols.field2);
            try output.appendSlice(allocator, " : Str }");
        },
        .list_u64 => try output.appendSlice(allocator, "List(U64)"),
        .try_u64_str => try output.appendSlice(allocator, "Try(U64, Str)"),
        .tuple_bool_u64 => try output.appendSlice(allocator, "(Bool, U64)"),
    }
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
        const arity = self.chooseMethodArity();
        var param_types: [max_method_arity]Type = undefined;
        for (0..arity) |index| {
            param_types[index] = self.chooseType();
        }
        const result_type = self.chooseType();
        const depth = self.reader.intRangeAtMost(u8, 2, 5);
        try self.generateMethod(param_types, arity, result_type, depth);
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
        try self.generateUnaryMethod(.root, result_type, 0);
    }
    for (all_types) |param_type| {
        try self.generateUnaryMethod(param_type, param_type, 1);
    }
}

fn generateUnaryMethod(self: *Self, param_type: Type, result_type: Type, depth: u8) std.mem.Allocator.Error!void {
    var param_types: [max_method_arity]Type = undefined;
    param_types[0] = param_type;
    try self.generateMethod(param_types, 1, result_type, depth);
}

fn generateMethod(self: *Self, param_types: [max_method_arity]Type, arity: u8, result_type: Type, depth: u8) std.mem.Allocator.Error!void {
    if (self.method_count >= self.methods.len) return;

    const method: Method = .{
        .symbol = self.fresh(.function),
        .param_types = param_types,
        .arity = arity,
        .result_type = result_type,
    };
    var args: [max_method_arity]Symbol = undefined;
    for (0..arity) |index| {
        args[index] = self.fresh(.value);
    }
    const visible_methods = self.method_count;

    try self.writeIndent(1);
    try self.writeSymbol(method.symbol);
    try self.write(" : ");
    for (0..arity) |index| {
        if (index > 0) try self.write(", ");
        try self.writeType(param_types[index]);
    }
    try self.write(" -> ");
    try self.writeType(result_type);
    try self.write("\n");
    try self.writeIndent(1);
    try self.writeSymbol(method.symbol);
    try self.write(" = |");
    for (0..arity) |index| {
        if (index > 0) try self.write(", ");
        try self.writeSymbol(args[index]);
    }
    try self.write("| ");
    try self.writeMethodBody(result_type, args, param_types, arity, depth, visible_methods);
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
    if (self.contextSymbol(typ, context)) |arg| {
        if (self.reader.boolean()) {
            try self.writeSymbol(arg);
            return;
        }
    }
    if (self.reader.boolean() and try self.writeMethodCall(typ, context, 0, visible_methods)) return;
    try self.writeLiteral(typ);
}

fn writeMethodBody(self: *Self, result_type: Type, args: [max_method_arity]Symbol, arg_types: [max_method_arity]Type, arity: u8, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    var context = contextFromParams(args, arg_types, arity);
    const local_count = if (depth == 0) 0 else self.reader.intRangeAtMost(u8, 0, max_local_bindings);

    if (local_count == 0) {
        try self.writeExpr(result_type, context, depth, visible_methods);
        return;
    }

    try self.write("{\n");
    for (0..local_count) |_| {
        const local = self.fresh(.value);
        const local_type = self.chooseType();
        const local_depth = self.reader.intRangeAtMost(u8, 0, depth - 1);

        try self.writeIndent(2);
        try self.writeSymbol(local);
        try self.write(" : ");
        try self.writeType(local_type);
        try self.write("\n");
        try self.writeIndent(2);
        try self.writeSymbol(local);
        try self.write(" = ");
        try self.writeExpr(local_type, context, local_depth, visible_methods);
        try self.write("\n");

        context = extendContext(context, local, local_type);
    }

    try self.writeIndent(2);
    try self.writeExpr(result_type, context, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn contextFromParams(args: [max_method_arity]Symbol, arg_types: [max_method_arity]Type, arity: u8) ExprContext {
    var context = emptyContext();
    for (0..arity) |index| {
        context = extendContext(context, args[index], arg_types[index]);
    }
    return context;
}

fn contextSymbol(self: *Self, typ: Type, context: ExprContext) ?Symbol {
    var count: usize = 0;
    for (0..context.arity) |index| {
        if (context.arg_types[index] == typ) count += 1;
    }
    if (count == 0) return null;

    var selected = self.reader.intRangeLessThan(usize, 0, count);
    for (0..context.arity) |index| {
        if (context.arg_types[index] != typ) continue;
        if (selected == 0) return context.args[index];
        selected -= 1;
    }
    unreachable;
}

fn contextHasSymbol(typ: Type, context: ExprContext) bool {
    for (0..context.arity) |index| {
        if (context.arg_types[index] == typ) return true;
    }
    return false;
}

fn extendContext(context: ExprContext, arg: Symbol, typ: Type) ExprContext {
    std.debug.assert(context.arity < max_context_values);
    var extended = context;
    extended.args[extended.arity] = arg;
    extended.arg_types[extended.arity] = typ;
    extended.arity += 1;
    return extended;
}

fn emptyContext() ExprContext {
    return .{ .args = undefined, .arg_types = undefined, .arity = 0 };
}

fn chooseMethodArity(self: *Self) u8 {
    return self.reader.intRangeAtMost(u8, 1, max_method_arity);
}

fn writeContextSymbolOrElse(self: *Self, typ: Type, context: ExprContext, fallback: *const fn (*Self, ExprContext, u8, usize) std.mem.Allocator.Error!void, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.contextSymbol(typ, context)) |arg| {
        try self.writeSymbol(arg);
        return;
    }
    try fallback(self, context, depth, visible_methods);
}

fn writeRootExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 5)) {
        0 => try self.writeContextSymbolOrElse(.root, context, writeRootLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.root, context, depth - 1, visible_methods)) try self.writeRootLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.root, context, depth - 1, visible_methods),
        3 => try self.writeRootLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.root, context, depth - 1, visible_methods),
        5 => try self.writeRecordMatchExpr(.root, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeBoolExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 9)) {
        0 => if (self.contextSymbol(.bool, context)) |arg| try self.writeSymbol(arg) else try self.writeBoolLiteral(),
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
        8 => try self.writeTupleFirstExpr(context, depth - 1, visible_methods),
        9 => try self.writeRecordMatchExpr(.bool, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeStrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 9)) {
        0 => if (self.contextSymbol(.str, context)) |arg| try self.writeSymbol(arg) else try self.writeStringLiteral(),
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
        7 => try self.writeTryErrOrExpr(context, depth - 1, visible_methods),
        8 => try self.writeRecordMatchExpr(.str, context, depth - 1, visible_methods),
        9 => if (self.canWriteStringInterpolationPart(context, visible_methods)) try self.writeStringInterpolationExpr(context, visible_methods) else try self.writeStringLiteral(),
        else => unreachable,
    }
}

fn writeU64Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 11)) {
        0 => if (self.contextSymbol(.u64, context)) |arg| try self.writeSymbol(arg) else try self.writeU64Literal(),
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
        9 => try self.writeTryOkOrExpr(context, depth - 1, visible_methods),
        10 => try self.writeTupleSecondExpr(context, depth - 1, visible_methods),
        11 => try self.writeRecordMatchExpr(.u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeRecordExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 6)) {
        0 => try self.writeContextSymbolOrElse(.record, context, writeRecordLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.record, context, depth - 1, visible_methods)) try self.writeRecordLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.record, context, depth - 1, visible_methods),
        3 => try self.writeRecordLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRecordUpdateExpr(context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.record, context, depth - 1, visible_methods),
        6 => try self.writeRecordMatchExpr(.record, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeListU64Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 9)) {
        0 => try self.writeContextSymbolOrElse(.list_u64, context, writeListLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.list_u64, context, depth - 1, visible_methods)) try self.writeListLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.list_u64, context, depth - 1, visible_methods),
        3 => try self.writeListLiteral(context, depth - 1, visible_methods),
        4 => try self.writeListMapExpr(context, depth - 1, visible_methods),
        5 => try self.writeListMatchExpr(context, depth - 1, visible_methods),
        6 => try self.writeListConcatExpr(context, depth - 1, visible_methods),
        7 => try self.writeListAppendExpr(context, depth - 1, visible_methods),
        8 => try self.writeListFilterExpr(context, depth - 1, visible_methods),
        9 => try self.writeRecordMatchExpr(.list_u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeTryExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 8)) {
        0 => try self.writeContextSymbolOrElse(.try_u64_str, context, writeTryLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.try_u64_str, context, depth - 1, visible_methods)) try self.writeTryLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.try_u64_str, context, depth - 1, visible_methods),
        3 => try self.writeTryLiteral(context, depth - 1, visible_methods),
        4 => try self.writeTryMatchExpr(context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.try_u64_str, context, depth - 1, visible_methods),
        6 => try self.writeTryMapOkExpr(context, depth - 1, visible_methods),
        7 => try self.writeTryMapErrExpr(context, depth - 1, visible_methods),
        8 => try self.writeRecordMatchExpr(.try_u64_str, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeTupleExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => try self.writeContextSymbolOrElse(.tuple_bool_u64, context, writeTupleLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.tuple_bool_u64, context, depth - 1, visible_methods)) try self.writeTupleLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        3 => try self.writeTupleLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        5 => try self.writeTupleProjectionExpr(context, depth - 1, visible_methods),
        6 => try self.writeTupleMatchExpr(context, depth - 1, visible_methods),
        7 => try self.writeRecordMatchExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    if (self.reader.boolean() and try self.writeReceiverMethodCall(result_type, context, depth, visible_methods)) return true;
    return self.writeAssociatedMethodCall(result_type, context, depth, visible_methods);
}

fn writeAssociatedMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    const method = self.chooseMethod(result_type, visible_methods) orelse return false;

    try self.writeSymbol(self.symbols.typ);
    try self.write(".");
    try self.writeSymbol(method.symbol);
    try self.write("(");
    for (0..method.arity) |index| {
        if (index > 0) try self.write(", ");
        try self.writeExpr(method.param_types[index], context, depth, visible_methods);
    }
    try self.write(")");
    return true;
}

fn writeReceiverMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    const method = self.chooseReceiverMethod(result_type, visible_methods) orelse return false;

    if (!try self.writeRootReceiver(context, depth, visible_methods)) return false;
    try self.write(".");
    try self.writeSymbol(method.symbol);
    try self.write("(");
    for (1..method.arity) |index| {
        if (index > 1) try self.write(", ");
        try self.writeExpr(method.param_types[index], context, depth, visible_methods);
    }
    try self.write(")");
    return true;
}

fn writeRootReceiver(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    if (self.contextSymbol(.root, context)) |arg| {
        if (self.reader.boolean()) {
            try self.writeSymbol(arg);
            return true;
        }
    }
    if (try self.writeAssociatedMethodCall(.root, context, depth, visible_methods)) return true;
    if (self.contextSymbol(.root, context)) |arg| {
        try self.writeSymbol(arg);
        return true;
    }
    return false;
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

fn chooseReceiverMethod(self: *Self, result_type: Type, visible_methods: usize) ?Method {
    var count: usize = 0;
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type == result_type and method.param_types[0] == .root) count += 1;
    }
    if (count == 0) return null;

    var index = self.reader.intRangeLessThan(usize, 0, count);
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type != result_type or method.param_types[0] != .root) continue;
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
    try self.writeExpr(result_type, extendContext(context, u64_payload, .u64), depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag2);
    try self.write("(");
    try self.writeSymbol(str_payload);
    try self.write(") => ");
    try self.writeExpr(result_type, extendContext(context, str_payload, .str), depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(2);
    try self.writeSymbol(self.symbols.tag3);
    try self.write("(");
    try self.writeSymbol(record_payload);
    try self.write(") => ");
    try self.writeExpr(result_type, extendContext(context, record_payload, .record), depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeTypedRootScrutinee(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.contextSymbol(.root, context)) |arg| {
        if (self.reader.boolean()) {
            try self.writeSymbol(arg);
            return;
        }
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
    try self.writeExpr(.u64, extendContext(context, item, .u64), depth, visible_methods);
    try self.write(")");
}

fn writeListConcatExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("List.concat(");
    try self.writeExpr(.list_u64, context, depth, visible_methods);
    try self.write(", ");
    try self.writeExpr(.list_u64, context, depth, visible_methods);
    try self.write(")");
}

fn writeListAppendExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("List.append(");
    try self.writeExpr(.list_u64, context, depth, visible_methods);
    try self.write(", ");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write(")");
}

fn writeListFilterExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const item = self.fresh(.value);

    try self.write(if (self.reader.boolean()) "List.keep_if(" else "List.drop_if(");
    try self.writeExpr(.list_u64, context, depth, visible_methods);
    try self.write(", |");
    try self.writeSymbol(item);
    try self.write("| ");
    try self.writeExpr(.bool, extendContext(context, item, .u64), depth, visible_methods);
    try self.write(")");
}

fn writeListMatchExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const head = self.fresh(.value);
    const tail = self.fresh(.value);
    const non_empty_context = extendContext(extendContext(context, head, .u64), tail, .list_u64);

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
    try self.writeExpr(.u64, non_empty_context, depth, visible_methods);
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
    try self.writeExpr(.u64, extendContext(context, ok_payload, .u64), depth, visible_methods);
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

fn writeRecordMatchExpr(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const bool_payload = self.fresh(.value);
    const u64_payload = self.fresh(.value);
    const str_payload = self.fresh(.value);
    const branch_context = extendContext(extendContext(extendContext(context, bool_payload, .bool), u64_payload, .u64), str_payload, .str);

    try self.write("match ");
    try self.writeExpr(.record, context, depth, visible_methods);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.write("{ ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeSymbol(bool_payload);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeSymbol(u64_payload);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field2);
    try self.write(": ");
    try self.writeSymbol(str_payload);
    try self.write(" } => ");
    try self.writeExpr(result_type, branch_context, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
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
    try self.writeExpr(.u64, extendContext(context, ok_payload, .u64), depth, visible_methods);
    try self.write(")\n");
    try self.writeIndent(2);
    try self.write("Err(");
    try self.writeSymbol(err_payload);
    try self.write(") => Err(");
    try self.writeExpr(.str, extendContext(context, err_payload, .str), depth, visible_methods);
    try self.write(")\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeTryMapOkExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const ok_payload = self.fresh(.value);

    try self.write("Try.map_ok(");
    try self.writeExpr(.try_u64_str, context, depth, visible_methods);
    try self.write(", |");
    try self.writeSymbol(ok_payload);
    try self.write("| ");
    try self.writeExpr(.u64, extendContext(context, ok_payload, .u64), depth, visible_methods);
    try self.write(")");
}

fn writeTryMapErrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const err_payload = self.fresh(.value);

    try self.write("Try.map_err(");
    try self.writeExpr(.try_u64_str, context, depth, visible_methods);
    try self.write(", |");
    try self.writeSymbol(err_payload);
    try self.write("| ");
    try self.writeExpr(.str, extendContext(context, err_payload, .str), depth, visible_methods);
    try self.write(")");
}

fn writeTryOkOrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("Try.ok_or(");
    try self.writeExpr(.try_u64_str, context, depth, visible_methods);
    try self.write(", ");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write(")");
}

fn writeTryErrOrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("Try.err_or(");
    try self.writeExpr(.try_u64_str, context, depth, visible_methods);
    try self.write(", ");
    try self.writeExpr(.str, context, depth, visible_methods);
    try self.write(")");
}

fn writeStringInterpolationExpr(self: *Self, context: ExprContext, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("\"");
    try self.writeStringLiteralChars(self.reader.intRangeAtMost(u8, 0, 4));
    const part_count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..part_count) |_| {
        try self.write("${(");
        try self.writeStringInterpolationPartExpr(context, visible_methods);
        try self.write(")}");
        try self.writeStringLiteralChars(self.reader.intRangeAtMost(u8, 0, 4));
    }
    try self.write("\"");
}

fn writeStringInterpolationPartExpr(self: *Self, context: ExprContext, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.contextSymbol(.str, context)) |arg| {
        if (self.reader.boolean()) {
            try self.writeSymbol(arg);
            return;
        }
    }

    if (self.chooseRootUnaryMethod(.str, visible_methods)) |method| {
        try self.writeSymbol(self.symbols.typ);
        try self.write(".");
        try self.writeSymbol(method.symbol);
        try self.write("(");
        try self.writeSymbol(self.symbols.tag0);
        try self.write(")");
        return;
    }

    if (self.contextSymbol(.str, context)) |arg| {
        try self.writeSymbol(arg);
        return;
    }

    unreachable;
}

fn canWriteStringInterpolationPart(self: *Self, context: ExprContext, visible_methods: usize) bool {
    return contextHasSymbol(.str, context) or self.hasRootUnaryMethod(.str, visible_methods);
}

fn hasRootUnaryMethod(self: *Self, result_type: Type, visible_methods: usize) bool {
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type == result_type and method.arity == 1 and method.param_types[0] == .root) return true;
    }
    return false;
}

fn chooseRootUnaryMethod(self: *Self, result_type: Type, visible_methods: usize) ?Method {
    var count: usize = 0;
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type == result_type and method.arity == 1 and method.param_types[0] == .root) count += 1;
    }
    if (count == 0) return null;

    var index = self.reader.intRangeLessThan(usize, 0, count);
    for (self.methods[0..visible_methods]) |method| {
        if (method.result_type != result_type or method.arity != 1 or method.param_types[0] != .root) continue;
        if (index == 0) return method;
        index -= 1;
    }
    unreachable;
}

fn writeTupleFirstExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.writeParenthesizedExpr(.tuple_bool_u64, context, depth, visible_methods);
    try self.write(".0");
}

fn writeTupleSecondExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.writeParenthesizedExpr(.tuple_bool_u64, context, depth, visible_methods);
    try self.write(".1");
}

fn writeTupleProjectionExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("(");
    try self.writeTupleFirstExpr(context, depth, visible_methods);
    try self.write(", ");
    try self.writeTupleSecondExpr(context, depth, visible_methods);
    try self.write(")");
}

fn writeTupleMatchExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const bool_payload = self.fresh(.value);
    const u64_payload = self.fresh(.value);
    const branch_context = extendContext(extendContext(context, bool_payload, .bool), u64_payload, .u64);

    try self.write("match ");
    try self.writeExpr(.tuple_bool_u64, context, depth, visible_methods);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.write("(");
    try self.writeSymbol(bool_payload);
    try self.write(", ");
    try self.writeSymbol(u64_payload);
    try self.write(") => ");
    try self.writeTupleLiteral(branch_context, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
}

fn writeTypedTryScrutinee(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.contextSymbol(.try_u64_str, context)) |arg| {
        if (self.reader.boolean()) {
            try self.writeSymbol(arg);
            return;
        }
    }
    if (try self.writeMethodCall(.try_u64_str, context, depth, visible_methods)) return;
    try self.writeTryLiteral(context, depth, visible_methods);
}

fn writeLiteral(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .root => try self.writeRootLiteral(emptyContext(), 0, self.method_count),
        .bool => try self.writeBoolLiteral(),
        .str => try self.writeStringLiteral(),
        .u64 => try self.writeU64Literal(),
        .record => try self.writeRecordLiteral(emptyContext(), 0, self.method_count),
        .list_u64 => try self.writeListLiteral(emptyContext(), 0, self.method_count),
        .try_u64_str => try self.writeTryLiteral(emptyContext(), 0, self.method_count),
        .tuple_bool_u64 => try self.writeTupleLiteral(emptyContext(), 0, self.method_count),
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
    try self.writeTypeTo(&self.output, self.allocator, typ);
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
    try self.writeStringLiteralChars(len);
    try self.write("\"");
}

fn writeStringLiteralChars(self: *Self, len: usize) std.mem.Allocator.Error!void {
    for (0..len) |_| {
        const char = self.reader.intRangeAtMost(u8, 'a', 'z');
        try self.output.append(self.allocator, char);
    }
}

fn fresh(self: *Self, kind: SymbolKind) Symbol {
    const symbol: Symbol = .{ .kind = kind, .id = self.next_id };
    self.next_id += 1;
    return symbol;
}

fn writeSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try writeSymbolTo(&self.output, self.allocator, symbol);
}

pub fn symbolPrefix(kind: SymbolKind) u8 {
    return switch (kind) {
        .app_file => 'B',
        .platform_file => 'P',
        .package => 'p',
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
