//! Typed Roc app generator for the post-check build fuzzer.
//!
//! Userspace Roc identifiers emitted by this generator must come from `fresh`.
//! Builtin names, target names, and external ABI symbol strings are the only
//! hardcoded names here.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

allocator: std.mem.Allocator,
reader: *FuzzReader,
app_output: std.ArrayList(u8),
platform_output: std.ArrayList(u8),
app_file_name: ?[]u8,
platform_file_name: ?[]u8,
next_id: u32,
symbols: Symbols,

const SymbolKind = enum {
    app_file,
    platform_file,
    package,
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

const Symbols = struct {
    app_entry: Symbol,
    platform_alias: Symbol,
    platform_wrapper: Symbol,
    item_type: Symbol,
    state_type: Symbol,
    err_type: Symbol,
    builder_type: Symbol,
    err_missing: Symbol,
    err_bad: Symbol,
    item_id: Symbol,
    item_text: Symbol,
    item_flag: Symbol,
    state_items: Symbol,
    state_count: Symbol,
    state_label: Symbol,
    builder_items: Symbol,
    builder_label: Symbol,
    builder_count: Symbol,
    builder_make: Symbol,
    builder_add: Symbol,
    builder_add_many: Symbol,
    builder_build: Symbol,
    builder_text: Symbol,
    make_item: Symbol,
    sum_items: Symbol,
    is_even: Symbol,
    is_odd: Symbol,
    get_item: Symbol,
    collect_items: Symbol,
    walk_rows: Symbol,
    walk_cols: Symbol,
    describe_try: Symbol,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .app_output = .empty,
        .platform_output = .empty,
        .app_file_name = null,
        .platform_file_name = null,
        .next_id = 0,
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

pub fn generate(self: *Self) std.mem.Allocator.Error!void {
    self.app_output.clearRetainingCapacity();
    self.platform_output.clearRetainingCapacity();
    self.next_id = 0;

    const app_file = self.fresh(.app_file);
    const platform_file = self.fresh(.platform_file);
    self.symbols = .{
        .app_entry = self.fresh(.function),
        .platform_alias = self.fresh(.package),
        .platform_wrapper = self.fresh(.function),
        .item_type = self.fresh(.type),
        .state_type = self.fresh(.type),
        .err_type = self.fresh(.type),
        .builder_type = self.fresh(.type),
        .err_missing = self.fresh(.tag),
        .err_bad = self.fresh(.tag),
        .item_id = self.fresh(.field),
        .item_text = self.fresh(.field),
        .item_flag = self.fresh(.field),
        .state_items = self.fresh(.field),
        .state_count = self.fresh(.field),
        .state_label = self.fresh(.field),
        .builder_items = self.fresh(.field),
        .builder_label = self.fresh(.field),
        .builder_count = self.fresh(.field),
        .builder_make = self.fresh(.function),
        .builder_add = self.fresh(.function),
        .builder_add_many = self.fresh(.function),
        .builder_build = self.fresh(.function),
        .builder_text = self.fresh(.function),
        .make_item = self.fresh(.function),
        .sum_items = self.fresh(.function),
        .is_even = self.fresh(.function),
        .is_odd = self.fresh(.function),
        .get_item = self.fresh(.function),
        .collect_items = self.fresh(.function),
        .walk_rows = self.fresh(.function),
        .walk_cols = self.fresh(.function),
        .describe_try = self.fresh(.function),
    };

    try self.setFileNames(app_file, platform_file);
    try self.writePlatform();
    try self.writeApp();
}

fn setFileNames(self: *Self, app_file: Symbol, platform_file: Symbol) std.mem.Allocator.Error!void {
    if (self.app_file_name) |name| self.allocator.free(name);
    if (self.platform_file_name) |name| self.allocator.free(name);
    self.app_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(app_file.kind), app_file.id });
    self.platform_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(platform_file.kind), platform_file.id });
}

fn writePlatform(self: *Self) std.mem.Allocator.Error!void {
    try self.writePlatformText("platform \"\"\n");
    try self.writePlatformText("    requires {} { ");
    try self.writePlatformSymbol(self.symbols.app_entry);
    try self.writePlatformText(" : Str -> Str }\n");
    try self.writePlatformText("    exposes []\n");
    try self.writePlatformText("    packages {}\n");
    try self.writePlatformText("    provides { \"roc_process_string\": ");
    try self.writePlatformSymbol(self.symbols.platform_wrapper);
    try self.writePlatformText(" }\n");
    try self.writePlatformText("    targets: {\n");
    try self.writePlatformText("        inputs: \"targets/\",\n");
    try self.writePlatformText("        x64mac: { inputs: [app] },\n");
    try self.writePlatformText("        arm64mac: { inputs: [app] },\n");
    try self.writePlatformText("        x64musl: { inputs: [app] },\n");
    try self.writePlatformText("        arm64musl: { inputs: [app] },\n");
    try self.writePlatformText("        x64win: { inputs: [app] },\n");
    try self.writePlatformText("        arm64win: { inputs: [app] },\n");
    try self.writePlatformText("    }\n\n");
    try self.writePlatformSymbol(self.symbols.platform_wrapper);
    try self.writePlatformText(" : Str -> Str\n");
    try self.writePlatformSymbol(self.symbols.platform_wrapper);
    try self.writePlatformText(" = |");
    const arg = self.fresh(.value);
    try self.writePlatformSymbol(arg);
    try self.writePlatformText("| ");
    try self.writePlatformSymbol(self.symbols.app_entry);
    try self.writePlatformText("(");
    try self.writePlatformSymbol(arg);
    try self.writePlatformText(")\n");
}

fn writeApp(self: *Self) std.mem.Allocator.Error!void {
    try self.writeAppText("app [");
    try self.writeAppSymbol(self.symbols.app_entry);
    try self.writeAppText("] { ");
    try self.writeAppSymbol(self.symbols.platform_alias);
    try self.writeAppText(": platform \"./");
    try self.writeAppText(self.getPlatformFileName());
    try self.writeAppText("\" }\n\n");

    try self.writeTypeDeclarations();
    try self.writeBuilderType();
    try self.writeTopLevelFunctions();
    try self.writeEntryPoint();
}

fn writeTypeDeclarations(self: *Self) std.mem.Allocator.Error!void {
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(" : {\n");
    try self.writeFieldTypeLine(self.symbols.item_id, "U64");
    try self.writeFieldTypeLine(self.symbols.item_text, "Str");
    try self.writeFieldTypeLine(self.symbols.item_flag, "Bool");
    try self.writeAppText("}\n\n");

    try self.writeAppSymbol(self.symbols.state_type);
    try self.writeAppText(" : {\n");
    try self.writeAppText("    ");
    try self.writeAppSymbol(self.symbols.state_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("),\n");
    try self.writeFieldTypeLine(self.symbols.state_count, "U64");
    try self.writeFieldTypeLine(self.symbols.state_label, "Str");
    try self.writeAppText("}\n\n");

    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(" := [");
    try self.writeAppSymbol(self.symbols.err_missing);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.err_bad);
    try self.writeAppText("(Str)]\n\n");
}

fn writeBuilderType(self: *Self) std.mem.Allocator.Error!void {
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(" :: {\n");
    try self.writeAppText("    ");
    try self.writeAppSymbol(self.symbols.builder_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("),\n");
    try self.writeFieldTypeLine(self.symbols.builder_label, "Str");
    try self.writeFieldTypeLine(self.symbols.builder_count, "U64");
    try self.writeAppText("}.{\n");

    try self.writeBuilderMake();
    try self.writeBuilderAdd();
    try self.writeBuilderAddMany();
    try self.writeBuilderBuild();
    try self.writeBuilderText();

    try self.writeAppText("}\n\n");
}

fn writeBuilderMake(self: *Self) std.mem.Allocator.Error!void {
    const label = self.fresh(.value);
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_make);
    try self.writeAppText(" : Str -> ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_make);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(label);
    try self.writeAppText("| { ");
    try self.writeAppSymbol(self.symbols.builder_items);
    try self.writeAppText(": [], ");
    try self.writeAppSymbol(self.symbols.builder_label);
    try self.writeAppText(": ");
    try self.writeAppSymbol(label);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.builder_count);
    try self.writeAppText(": 0 }\n");
}

fn writeBuilderAdd(self: *Self) std.mem.Allocator.Error!void {
    const builder = self.fresh(.value);
    const item = self.fresh(.value);
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_add);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(" -> ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_add);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(builder);
    try self.writeAppText(", ");
    try self.writeAppSymbol(item);
    try self.writeAppText("| { ..");
    try self.writeAppSymbol(builder);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.builder_items);
    try self.writeAppText(": List.append(");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(item);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.builder_count);
    try self.writeAppText(": ");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_count);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" }\n");
}

fn writeBuilderAddMany(self: *Self) std.mem.Allocator.Error!void {
    const builder = self.fresh(.value);
    const items = self.fresh(.value);
    const acc = self.fresh(.value);
    const item = self.fresh(.value);
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_add_many);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(", List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(") -> ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_add_many);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(builder);
    try self.writeAppText(", ");
    try self.writeAppSymbol(items);
    try self.writeAppText("| {\n");
    try self.writeIndent(2);
    try self.writeAppText("var $");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(builder);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("for ");
    try self.writeAppSymbol(item);
    try self.writeAppText(" in ");
    try self.writeAppSymbol(items);
    try self.writeAppText(" {\n");
    try self.writeIndent(3);
    try self.writeAppText("$");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_add);
    try self.writeAppText("($");
    try self.writeAppSymbol(acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(item);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppText("}\n");
    try self.writeIndent(2);
    try self.writeAppText("$");
    try self.writeAppSymbol(acc);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");
}

fn writeBuilderBuild(self: *Self) std.mem.Allocator.Error!void {
    const builder = self.fresh(.value);
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_build);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(" -> ");
    try self.writeAppSymbol(self.symbols.state_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_build);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(builder);
    try self.writeAppText("| { ");
    try self.writeAppSymbol(self.symbols.state_items);
    try self.writeAppText(": ");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.state_count);
    try self.writeAppText(": ");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_count);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.state_label);
    try self.writeAppText(": ");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_label);
    try self.writeAppText(" }\n");
}

fn writeBuilderText(self: *Self) std.mem.Allocator.Error!void {
    const builder = self.fresh(.value);
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_text);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(" -> Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.builder_text);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(builder);
    try self.writeAppText("| Str.concat(");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_label);
    try self.writeAppText(", U64.to_str(");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_count);
    try self.writeAppText("))\n");
}

fn writeTopLevelFunctions(self: *Self) std.mem.Allocator.Error!void {
    try self.writeMakeItem();
    try self.writeSumItems();
    try self.writeMutualRecursion();
    try self.writeGetItem();
    try self.writeCollectItems();
    try self.writeGridRecursion();
    try self.writeDescribeTry();
}

fn writeMakeItem(self: *Self) std.mem.Allocator.Error!void {
    const text = self.fresh(.value);
    const id = self.fresh(.value);
    try self.writeAppSymbol(self.symbols.make_item);
    try self.writeAppText(" : Str, U64 -> ");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("\n");
    try self.writeAppSymbol(self.symbols.make_item);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(id);
    try self.writeAppText("| { ");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(id);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(": ");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(": ");
    try self.writeAppSymbol(id);
    try self.writeAppText(" == 0 }\n\n");
}

fn writeSumItems(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const acc = self.fresh(.value);
    const item = self.fresh(.value);
    const rest = self.fresh(.value);
    try self.writeAppSymbol(self.symbols.sum_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.sum_items);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(acc);
    try self.writeAppText("| match ");
    try self.writeAppSymbol(items);
    try self.writeAppText(" {\n");
    try self.writeIndent(1);
    try self.writeAppText("[");
    try self.writeAppSymbol(item);
    try self.writeAppText(", .. as ");
    try self.writeAppSymbol(rest);
    try self.writeAppText("] => ");
    try self.writeAppSymbol(self.symbols.sum_items);
    try self.writeAppText("(");
    try self.writeAppSymbol(rest);
    try self.writeAppText(", ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppText("[] => ");
    try self.writeAppSymbol(acc);
    try self.writeAppText("\n}\n\n");
}

fn writeMutualRecursion(self: *Self) std.mem.Allocator.Error!void {
    const even_arg = self.fresh(.value);
    const odd_arg = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText(" : U64 -> Bool\n");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(even_arg);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(even_arg);
    try self.writeAppText(" == 0 True else ");
    try self.writeAppSymbol(self.symbols.is_odd);
    try self.writeAppText("(");
    try self.writeAppSymbol(even_arg);
    try self.writeAppText(" - 1)\n\n");

    try self.writeAppSymbol(self.symbols.is_odd);
    try self.writeAppText(" : U64 -> Bool\n");
    try self.writeAppSymbol(self.symbols.is_odd);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(odd_arg);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(odd_arg);
    try self.writeAppText(" == 0 False else ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(odd_arg);
    try self.writeAppText(" - 1)\n\n");
}

fn writeGetItem(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const index = self.fresh(.value);
    const item = self.fresh(.value);
    try self.writeAppSymbol(self.symbols.get_item);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> Try(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(")\n");
    try self.writeAppSymbol(self.symbols.get_item);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText("| match List.get(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText(") {\n");
    try self.writeIndent(1);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(item);
    try self.writeAppText(") => Ok(");
    try self.writeAppSymbol(item);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppText("Err(_) => Err(");
    try self.writeAppSymbol(self.symbols.err_missing);
    try self.writeAppText(")\n}\n\n");
}

fn writeCollectItems(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const prefix = self.fresh(.value);
    const next = self.fresh(.value);
    const item = self.fresh(.value);
    try self.writeAppSymbol(self.symbols.collect_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), Str -> List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeAppSymbol(self.symbols.collect_items);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(prefix);
    try self.writeAppText("| {\n");
    try self.writeIndent(1);
    try self.writeAppText("var $");
    try self.writeAppSymbol(next);
    try self.writeAppText(" = []\n");
    try self.writeIndent(1);
    try self.writeAppText("for ");
    try self.writeAppSymbol(item);
    try self.writeAppText(" in ");
    try self.writeAppSymbol(items);
    try self.writeAppText(" {\n");
    try self.writeIndent(2);
    try self.writeAppText("$");
    try self.writeAppSymbol(next);
    try self.writeAppText(" = List.append($");
    try self.writeAppSymbol(next);
    try self.writeAppText(", { ..");
    try self.writeAppSymbol(item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(": Str.concat(");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(": ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(") })\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");
    try self.writeIndent(1);
    try self.writeAppText("$");
    try self.writeAppSymbol(next);
    try self.writeAppText("\n}\n\n");
}

fn writeGridRecursion(self: *Self) std.mem.Allocator.Error!void {
    const row_state = self.fresh(.value);
    const row = self.fresh(.value);
    const row_acc = self.fresh(.value);
    const col_state = self.fresh(.value);
    const col_row = self.fresh(.value);
    const col = self.fresh(.value);
    const col_acc = self.fresh(.value);
    const max_col = self.reader.intRangeAtMost(u8, 1, 5);

    try self.writeAppSymbol(self.symbols.walk_rows);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.state_type);
    try self.writeAppText(", U64, U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.walk_rows);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(row_state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(row);
    try self.writeAppText(", ");
    try self.writeAppSymbol(row_acc);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(row);
    try self.writeAppText(" == 0 ");
    try self.writeAppSymbol(row_acc);
    try self.writeAppText(" else ");
    try self.writeAppSymbol(self.symbols.walk_cols);
    try self.writeAppText("(");
    try self.writeAppSymbol(row_state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(row);
    try self.writeAppText(", ");
    try self.writeU64(max_col);
    try self.writeAppText(", ");
    try self.writeAppSymbol(row_acc);
    try self.writeAppText(")\n\n");

    try self.writeAppSymbol(self.symbols.walk_cols);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.state_type);
    try self.writeAppText(", U64, U64, U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.walk_cols);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(col_state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(col_row);
    try self.writeAppText(", ");
    try self.writeAppSymbol(col);
    try self.writeAppText(", ");
    try self.writeAppSymbol(col_acc);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(col);
    try self.writeAppText(" == 0 ");
    try self.writeAppSymbol(self.symbols.walk_rows);
    try self.writeAppText("(");
    try self.writeAppSymbol(col_state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(col_row);
    try self.writeAppText(" - 1, ");
    try self.writeAppSymbol(col_acc);
    try self.writeAppText(") else ");
    try self.writeAppSymbol(self.symbols.walk_cols);
    try self.writeAppText("(");
    try self.writeAppSymbol(col_state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(col_row);
    try self.writeAppText(", ");
    try self.writeAppSymbol(col);
    try self.writeAppText(" - 1, ");
    try self.writeAppSymbol(col_acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(self.symbols.sum_items);
    try self.writeAppText("(");
    try self.writeAppSymbol(col_state);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.state_items);
    try self.writeAppText(", 0))\n\n");
}

fn writeDescribeTry(self: *Self) std.mem.Allocator.Error!void {
    const result = self.fresh(.value);
    const item = self.fresh(.value);
    const err_payload = self.fresh(.value);
    try self.writeAppSymbol(self.symbols.describe_try);
    try self.writeAppText(" : Try(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(") -> Str\n");
    try self.writeAppSymbol(self.symbols.describe_try);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(result);
    try self.writeAppText("| match ");
    try self.writeAppSymbol(result);
    try self.writeAppText(" {\n");
    try self.writeIndent(1);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(item);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("Err(");
    try self.writeAppSymbol(self.symbols.err_missing);
    try self.writeAppText(") => \"\"\n");
    try self.writeIndent(1);
    try self.writeAppText("Err(");
    try self.writeAppSymbol(self.symbols.err_bad);
    try self.writeAppText("(");
    try self.writeAppSymbol(err_payload);
    try self.writeAppText(")) => ");
    try self.writeAppSymbol(err_payload);
    try self.writeAppText("\n}\n\n");
}

fn writeEntryPoint(self: *Self) std.mem.Allocator.Error!void {
    const input = self.fresh(.value);
    const first = self.fresh(.value);
    const second = self.fresh(.value);
    const third = self.fresh(.value);
    const items = self.fresh(.value);
    const collected = self.fresh(.value);
    const builder = self.fresh(.value);
    const state = self.fresh(.value);
    const walked = self.fresh(.value);
    const picked = self.fresh(.value);
    const rows = self.reader.intRangeAtMost(u8, 1, 6);
    const first_id = self.reader.intRangeAtMost(u8, 0, 9);
    const second_id = self.reader.intRangeAtMost(u8, 10, 29);
    const third_id = self.reader.intRangeAtMost(u8, 30, 59);
    const pick_index = self.reader.intRangeAtMost(u8, 0, 5);

    try self.writeAppSymbol(self.symbols.app_entry);
    try self.writeAppText(" : Str -> Str\n");
    try self.writeAppSymbol(self.symbols.app_entry);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(input);
    try self.writeAppText("| {\n");

    try self.writeLocalHeader(first, self.symbols.item_type);
    try self.writeAppSymbol(self.symbols.make_item);
    try self.writeAppText("(");
    try self.writeAppSymbol(input);
    try self.writeAppText(", ");
    try self.writeU64(first_id);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(second, self.symbols.item_type);
    try self.writeAppSymbol(self.symbols.make_item);
    try self.writeAppText("(Str.concat(");
    try self.writeAppSymbol(input);
    try self.writeAppText(", \"x\"), ");
    try self.writeU64(second_id);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(third, self.symbols.item_type);
    try self.writeAppSymbol(self.symbols.make_item);
    try self.writeAppText("(Str.concat(");
    try self.writeAppSymbol(input);
    try self.writeAppText(", \"y\"), ");
    try self.writeU64(third_id);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(items);
    try self.writeAppText(" = [");
    try self.writeAppSymbol(first);
    try self.writeAppText(", ");
    try self.writeAppSymbol(second);
    try self.writeAppText(", ");
    try self.writeAppSymbol(third);
    try self.writeAppText("]\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(collected);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(collected);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.collect_items);
    try self.writeAppText("(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(input);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(builder);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(builder);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.builder_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_make);
    try self.writeAppText("(");
    try self.writeAppSymbol(input);
    try self.writeAppText(").");
    try self.writeAppSymbol(self.symbols.builder_add);
    try self.writeAppText("(");
    try self.writeAppSymbol(first);
    try self.writeAppText(").");
    try self.writeAppSymbol(self.symbols.builder_add_many);
    try self.writeAppText("(");
    try self.writeAppSymbol(collected);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(state, self.symbols.state_type);
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_build);
    try self.writeAppText("()\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(walked);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(walked);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.walk_rows);
    try self.writeAppText("(");
    try self.writeAppSymbol(state);
    try self.writeAppText(", ");
    try self.writeU64(rows);
    try self.writeAppText(", 0)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(picked);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(picked);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.describe_try);
    try self.writeAppText("(");
    try self.writeAppSymbol(self.symbols.get_item);
    try self.writeAppText("(");
    try self.writeAppSymbol(state);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.state_items);
    try self.writeAppText(", ");
    try self.writeU64(pick_index);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppText("if ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(walked);
    try self.writeAppText(") Str.concat(");
    try self.writeAppSymbol(picked);
    try self.writeAppText(", U64.to_str(");
    try self.writeAppSymbol(walked);
    try self.writeAppText(")) else ");
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_text);
    try self.writeAppText("()\n");
    try self.writeAppText("}\n");
}

fn writeFieldTypeLine(self: *Self, field: Symbol, type_text: []const u8) std.mem.Allocator.Error!void {
    try self.writeAppText("    ");
    try self.writeAppSymbol(field);
    try self.writeAppText(" : ");
    try self.writeAppText(type_text);
    try self.writeAppText(",\n");
}

fn writeLocalHeader(self: *Self, local: Symbol, typ: Symbol) std.mem.Allocator.Error!void {
    try self.writeIndent(1);
    try self.writeAppSymbol(local);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(typ);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(local);
    try self.writeAppText(" = ");
}

fn writeIndent(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    for (0..depth) |_| try self.writeAppText("    ");
}

fn writeU64(self: *Self, value: u64) std.mem.Allocator.Error!void {
    const text = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
    defer self.allocator.free(text);
    try self.writeAppText(text);
}

fn fresh(self: *Self, kind: SymbolKind) Symbol {
    const symbol = Symbol{ .kind = kind, .id = self.next_id };
    self.next_id += 1;
    return symbol;
}

fn writeAppSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try writeSymbolTo(&self.app_output, self.allocator, symbol);
}

fn writePlatformSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try writeSymbolTo(&self.platform_output, self.allocator, symbol);
}

fn writeSymbolTo(output: *std.ArrayList(u8), allocator: std.mem.Allocator, symbol: Symbol) std.mem.Allocator.Error!void {
    const text = try std.fmt.allocPrint(allocator, "{c}{d}", .{ symbolPrefix(symbol.kind), symbol.id });
    defer allocator.free(text);
    try output.appendSlice(allocator, text);
}

fn symbolPrefix(kind: SymbolKind) u8 {
    return switch (kind) {
        .app_file => 'A',
        .platform_file => 'P',
        .package => 'p',
        .type => 'T',
        .tag => 'C',
        .field => 'r',
        .value => 'v',
        .function => 'f',
    };
}

fn writeAppText(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.app_output.appendSlice(self.allocator, text);
}

fn writePlatformText(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.platform_output.appendSlice(self.allocator, text);
}
