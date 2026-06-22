//! Build-fuzzer app/platform wrapper around the shared typed module generator.

const std = @import("std");
const roc_target = @import("roc_target");
const FuzzReader = @import("FuzzReader.zig");
const TypedCodeGenerator = @import("TypedCodeGenerator.zig");

const Self = @This();

const Symbol = TypedCodeGenerator.Symbol;
const Type = TypedCodeGenerator.Type;
const Method = TypedCodeGenerator.Method;

pub const build_targets = blk: {
    const fields = @typeInfo(roc_target.RocTarget).@"enum".fields;
    var targets: [fields.len]roc_target.RocTarget = undefined;
    for (fields, 0..) |field, index| {
        targets[index] = @enumFromInt(field.value);
    }
    break :blk targets;
};

allocator: std.mem.Allocator,
reader: *FuzzReader,
module: *TypedCodeGenerator,
app_output: std.ArrayList(u8),
platform_output: std.ArrayList(u8),
app_file_name: ?[]u8,
platform_file_name: ?[]u8,
target: roc_target.RocTarget,
symbols: Symbols,

const Symbols = struct {
    app_file: Symbol,
    platform_file: Symbol,
    platform_alias: Symbol,
    app_entry: Symbol,
    platform_entry: Symbol,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader, module: *TypedCodeGenerator) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .module = module,
        .app_output = .empty,
        .platform_output = .empty,
        .app_file_name = null,
        .platform_file_name = null,
        .target = .wasm32,
        .symbols = undefined,
    };
}

pub fn deinit(self: *Self) void {
    self.app_output.deinit(self.allocator);
    self.platform_output.deinit(self.allocator);
    if (self.app_file_name) |name| self.allocator.free(name);
    if (self.platform_file_name) |name| self.allocator.free(name);
}

pub fn getAppOutput(self: *const Self) []const u8 {
    return self.app_output.items;
}

pub fn getPlatformOutput(self: *const Self) []const u8 {
    return self.platform_output.items;
}

pub fn getAppFileName(self: *const Self) []const u8 {
    return self.app_file_name orelse @panic("build fuzzer app file name requested before generation");
}

pub fn getPlatformFileName(self: *const Self) []const u8 {
    return self.platform_file_name orelse @panic("build fuzzer platform file name requested before generation");
}

pub fn getTarget(self: *const Self) roc_target.RocTarget {
    return self.target;
}

pub fn chooseTarget(reader: *FuzzReader) roc_target.RocTarget {
    return build_targets[reader.intRangeLessThan(usize, 0, build_targets.len)];
}

pub fn generate(self: *Self, target: roc_target.RocTarget) std.mem.Allocator.Error!void {
    self.app_output.clearRetainingCapacity();
    self.platform_output.clearRetainingCapacity();

    self.target = target;
    self.symbols = .{
        .app_file = self.module.freshSymbol(.app_file),
        .platform_file = self.module.freshSymbol(.platform_file),
        .platform_alias = self.module.freshSymbol(.package),
        .app_entry = self.module.freshSymbol(.function),
        .platform_entry = self.module.freshSymbol(.function),
    };

    try self.setFileNames();
    try self.writeApp();
    try self.writePlatform();
}

fn setFileNames(self: *Self) std.mem.Allocator.Error!void {
    if (self.app_file_name) |name| self.allocator.free(name);
    if (self.platform_file_name) |name| self.allocator.free(name);
    self.app_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ TypedCodeGenerator.symbolPrefix(self.symbols.app_file.kind), self.symbols.app_file.id });
    self.platform_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ TypedCodeGenerator.symbolPrefix(self.symbols.platform_file.kind), self.symbols.platform_file.id });
}

fn writeApp(self: *Self) std.mem.Allocator.Error!void {
    const score = self.module.freshSymbol(.value);

    try self.writeAppText("app [");
    try self.writeAppSymbol(self.symbols.app_entry);
    try self.writeAppText("!] { ");
    try self.writeAppSymbol(self.symbols.platform_alias);
    try self.writeAppText(": platform \"./");
    try self.writeAppText(self.getPlatformFileName());
    try self.writeAppText("\" }\n\nimport ");
    try self.writeAppSymbol(self.module.getRootSymbols().typ);
    try self.writeAppText("\n\n");

    try self.writeAppSymbol(self.symbols.app_entry);
    try self.writeAppText("! : {} => I32\n");
    try self.writeAppSymbol(self.symbols.app_entry);
    try self.writeAppText("! = |{}| {\n");
    try self.writeAppText("    ");
    try self.writeAppSymbol(score);
    try self.writeAppText(" : U64\n    ");
    try self.writeAppSymbol(score);
    try self.writeAppText(" = ");
    try self.writeExpr(.u64, self.reader.intRangeAtMost(u8, 2, 5));
    try self.writeAppText("\n    U64.to_i32_wrap(");
    try self.writeAppSymbol(score);
    try self.writeAppText(")\n}\n");
}

fn writePlatform(self: *Self) std.mem.Allocator.Error!void {
    try self.writePlatformText("platform \"\"\n    requires {} { ");
    try self.writePlatformSymbol(self.symbols.app_entry);
    try self.writePlatformText("! : {} => I32 }\n    exposes []\n    packages {}\n    provides { \"main\": ");
    try self.writePlatformSymbol(self.symbols.platform_entry);
    try self.writePlatformText("! }\n    targets: {\n        inputs_dir: \"targets/\",\n");
    for (build_targets) |target| {
        try self.writePlatformText("        ");
        try self.writePlatformText(@tagName(target));
        try self.writePlatformText(": { inputs: [app], output: Archive },\n");
    }
    try self.writePlatformText("    }\n\n");
    try self.writePlatformSymbol(self.symbols.platform_entry);
    try self.writePlatformText("! : {} => I32\n");
    try self.writePlatformSymbol(self.symbols.platform_entry);
    try self.writePlatformText("! = ");
    try self.writePlatformSymbol(self.symbols.app_entry);
    try self.writePlatformText("!\n");
}

fn writeExpr(self: *Self, typ: Type, depth: u8) std.mem.Allocator.Error!void {
    if (depth == 0) {
        try self.writeLiteral(typ, 0);
        return;
    }

    switch (typ) {
        .root => switch (self.reader.intRangeAtMost(u8, 0, 2)) {
            0 => if (!try self.writeMethodCall(.root, depth - 1)) try self.writeRootLiteral(depth - 1),
            1 => try self.writeRootLiteral(depth - 1),
            2 => try self.writeIfExpr(.root, depth - 1),
            else => unreachable,
        },
        .bool => switch (self.reader.intRangeAtMost(u8, 0, 4)) {
            0 => if (!try self.writeMethodCall(.bool, depth - 1)) try self.writeBoolLiteral(),
            1 => try self.writeBoolLiteral(),
            2 => try self.writeIfExpr(.bool, depth - 1),
            3 => {
                try self.writeParenthesizedExpr(.u64, depth - 1);
                try self.writeAppText(" == ");
                try self.writeParenthesizedExpr(.u64, depth - 1);
            },
            4 => {
                try self.writeAppText("Bool.not(");
                try self.writeExpr(.bool, depth - 1);
                try self.writeAppText(")");
            },
            else => unreachable,
        },
        .str => switch (self.reader.intRangeAtMost(u8, 0, 3)) {
            0 => if (!try self.writeMethodCall(.str, depth - 1)) try self.writeStringLiteral(),
            1 => try self.writeStringLiteral(),
            2 => try self.writeIfExpr(.str, depth - 1),
            3 => {
                try self.writeAppText("Str.concat(");
                try self.writeExpr(.str, depth - 1);
                try self.writeAppText(", ");
                try self.writeExpr(.str, depth - 1);
                try self.writeAppText(")");
            },
            else => unreachable,
        },
        .u64 => switch (self.reader.intRangeAtMost(u8, 0, 5)) {
            0 => if (!try self.writeMethodCall(.u64, depth - 1)) try self.writeU64Literal(),
            1 => try self.writeU64Literal(),
            2 => try self.writeIfExpr(.u64, depth - 1),
            3 => {
                try self.writeParenthesizedExpr(.u64, depth - 1);
                try self.writeAppText(" + ");
                try self.writeParenthesizedExpr(.u64, depth - 1);
            },
            4 => {
                try self.writeAppText("Str.count_utf8_bytes(");
                try self.writeExpr(.str, depth - 1);
                try self.writeAppText(")");
            },
            5 => {
                try self.writeAppText("List.len(");
                try self.writeExpr(.list_u64, depth - 1);
                try self.writeAppText(")");
            },
            else => unreachable,
        },
        .record => switch (self.reader.intRangeAtMost(u8, 0, 2)) {
            0 => if (!try self.writeMethodCall(.record, depth - 1)) try self.writeRecordLiteral(depth - 1),
            1 => try self.writeRecordLiteral(depth - 1),
            2 => try self.writeIfExpr(.record, depth - 1),
            else => unreachable,
        },
        .list_u64 => switch (self.reader.intRangeAtMost(u8, 0, 4)) {
            0 => if (!try self.writeMethodCall(.list_u64, depth - 1)) try self.writeListLiteral(depth - 1),
            1 => try self.writeListLiteral(depth - 1),
            2 => try self.writeIfExpr(.list_u64, depth - 1),
            3 => {
                try self.writeAppText("List.concat(");
                try self.writeExpr(.list_u64, depth - 1);
                try self.writeAppText(", ");
                try self.writeExpr(.list_u64, depth - 1);
                try self.writeAppText(")");
            },
            4 => {
                try self.writeAppText("List.append(");
                try self.writeExpr(.list_u64, depth - 1);
                try self.writeAppText(", ");
                try self.writeExpr(.u64, depth - 1);
                try self.writeAppText(")");
            },
            else => unreachable,
        },
        .try_u64_str => switch (self.reader.intRangeAtMost(u8, 0, 2)) {
            0 => if (!try self.writeMethodCall(.try_u64_str, depth - 1)) try self.writeTryLiteral(depth - 1),
            1 => try self.writeTryLiteral(depth - 1),
            2 => try self.writeIfExpr(.try_u64_str, depth - 1),
            else => unreachable,
        },
        .tuple_bool_u64 => switch (self.reader.intRangeAtMost(u8, 0, 2)) {
            0 => if (!try self.writeMethodCall(.tuple_bool_u64, depth - 1)) try self.writeTupleLiteral(depth - 1),
            1 => try self.writeTupleLiteral(depth - 1),
            2 => try self.writeIfExpr(.tuple_bool_u64, depth - 1),
            else => unreachable,
        },
    }
}

fn writeMethodCall(self: *Self, result_type: Type, depth: u8) std.mem.Allocator.Error!bool {
    const method = self.chooseMethod(result_type) orelse return false;
    try self.writeAppSymbol(self.module.getRootSymbols().typ);
    try self.writeAppText(".");
    try self.writeAppSymbol(method.symbol);
    try self.writeAppText("(");
    for (0..method.arity) |index| {
        if (index > 0) try self.writeAppText(", ");
        try self.writeExpr(method.param_types[index], depth);
    }
    try self.writeAppText(")");
    return true;
}

fn chooseMethod(self: *Self, result_type: Type) ?Method {
    const methods = self.module.getMethods();
    var count: usize = 0;
    for (methods) |method| {
        if (method.result_type == result_type) count += 1;
    }
    if (count == 0) return null;

    var selected = self.reader.intRangeLessThan(usize, 0, count);
    for (methods) |method| {
        if (method.result_type != result_type) continue;
        if (selected == 0) return method;
        selected -= 1;
    }
    unreachable;
}

fn writeIfExpr(self: *Self, typ: Type, depth: u8) std.mem.Allocator.Error!void {
    try self.writeAppText("if ");
    try self.writeParenthesizedExpr(.bool, depth);
    try self.writeAppText(" ");
    try self.writeParenthesizedExpr(typ, depth);
    try self.writeAppText(" else ");
    try self.writeParenthesizedExpr(typ, depth);
}

fn writeParenthesizedExpr(self: *Self, typ: Type, depth: u8) std.mem.Allocator.Error!void {
    try self.writeAppText("(");
    try self.writeExpr(typ, depth);
    try self.writeAppText(")");
}

fn writeLiteral(self: *Self, typ: Type, depth: u8) std.mem.Allocator.Error!void {
    switch (typ) {
        .root => try self.writeRootLiteral(depth),
        .bool => try self.writeBoolLiteral(),
        .str => try self.writeStringLiteral(),
        .u64 => try self.writeU64Literal(),
        .record => try self.writeRecordLiteral(depth),
        .list_u64 => try self.writeListLiteral(depth),
        .try_u64_str => try self.writeTryLiteral(depth),
        .tuple_bool_u64 => try self.writeTupleLiteral(depth),
    }
}

fn writeRootLiteral(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    const root = self.module.getRootSymbols();
    try self.writeAppSymbol(root.typ);
    try self.writeAppText(".");
    switch (self.reader.intRangeAtMost(u8, 0, 3)) {
        0 => try self.writeAppSymbol(root.tag0),
        1 => {
            try self.writeAppSymbol(root.tag1);
            try self.writeAppText("(");
            try self.writeExpr(.u64, depth);
            try self.writeAppText(")");
        },
        2 => {
            try self.writeAppSymbol(root.tag2);
            try self.writeAppText("(");
            try self.writeExpr(.str, depth);
            try self.writeAppText(")");
        },
        3 => {
            try self.writeAppSymbol(root.tag3);
            try self.writeAppText("(");
            try self.writeRecordLiteral(depth);
            try self.writeAppText(")");
        },
        else => unreachable,
    }
}

fn writeRecordLiteral(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    const root = self.module.getRootSymbols();
    try self.writeAppText("{ ");
    try self.writeAppSymbol(root.field0);
    try self.writeAppText(": ");
    try self.writeExpr(.bool, depth);
    try self.writeAppText(", ");
    try self.writeAppSymbol(root.field1);
    try self.writeAppText(": ");
    try self.writeExpr(.u64, depth);
    try self.writeAppText(", ");
    try self.writeAppSymbol(root.field2);
    try self.writeAppText(": ");
    try self.writeExpr(.str, depth);
    try self.writeAppText(" }");
}

fn writeListLiteral(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    const count = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeAppText("[");
    for (0..count) |index| {
        if (index > 0) try self.writeAppText(", ");
        try self.writeExpr(.u64, depth);
    }
    try self.writeAppText("]");
}

fn writeTryLiteral(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.writeAppText("Ok(");
        try self.writeExpr(.u64, depth);
        try self.writeAppText(")");
    } else {
        try self.writeAppText("Err(");
        try self.writeExpr(.str, depth);
        try self.writeAppText(")");
    }
}

fn writeTupleLiteral(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.writeAppText("(");
    try self.writeExpr(.bool, depth);
    try self.writeAppText(", ");
    try self.writeExpr(.u64, depth);
    try self.writeAppText(")");
}

fn writeBoolLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.writeAppText(if (self.reader.boolean()) "True" else "False");
}

fn writeU64Literal(self: *Self) std.mem.Allocator.Error!void {
    const value = self.reader.intRangeAtMost(u8, 0, 100);
    try self.app_output.print(self.allocator, "{d}", .{value});
}

fn writeStringLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.writeAppText("\"");
    const len = self.reader.intRangeAtMost(u8, 0, 8);
    for (0..len) |_| {
        const char = self.reader.intRangeAtMost(u8, 'a', 'z');
        try self.app_output.append(self.allocator, char);
    }
    try self.writeAppText("\"");
}

fn writeAppSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try TypedCodeGenerator.writeSymbolTo(&self.app_output, self.allocator, symbol);
}

fn writePlatformSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try TypedCodeGenerator.writeSymbolTo(&self.platform_output, self.allocator, symbol);
}

fn writeAppText(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.app_output.appendSlice(self.allocator, text);
}

fn writePlatformText(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.platform_output.appendSlice(self.allocator, text);
}
