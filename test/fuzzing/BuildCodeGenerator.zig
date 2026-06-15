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
module_output: std.ArrayList(u8),
app_file_name: ?[]u8,
platform_file_name: ?[]u8,
module_file_name: ?[]u8,
next_id: u32,
symbols: Symbols,

const SymbolKind = enum {
    app_file,
    platform_file,
    module_file,
    package,
    type,
    type_var,
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
    import_module: Symbol,
    item_type: Symbol,
    state_type: Symbol,
    err_type: Symbol,
    builder_type: Symbol,
    tree_type: Symbol,
    key_type: Symbol,
    imported_type: Symbol,
    err_missing: Symbol,
    err_bad: Symbol,
    tree_leaf: Symbol,
    tree_branch: Symbol,
    tree_named: Symbol,
    item_id: Symbol,
    item_text: Symbol,
    item_flag: Symbol,
    imported_num: Symbol,
    imported_text: Symbol,
    state_items: Symbol,
    state_count: Symbol,
    state_label: Symbol,
    builder_items: Symbol,
    builder_label: Symbol,
    builder_count: Symbol,
    key_id: Symbol,
    key_text: Symbol,
    builder_make: Symbol,
    builder_add: Symbol,
    builder_add_many: Symbol,
    builder_build: Symbol,
    builder_text: Symbol,
    key_from_item: Symbol,
    key_fallback: Symbol,
    key_score: Symbol,
    imported_make: Symbol,
    imported_score: Symbol,
    make_item: Symbol,
    sum_items: Symbol,
    is_even: Symbol,
    is_odd: Symbol,
    get_item: Symbol,
    try_item_score: Symbol,
    collect_items: Symbol,
    transform_items: Symbol,
    fold_items: Symbol,
    score_with: Symbol,
    score_pairs: Symbol,
    walk_rows: Symbol,
    walk_cols: Symbol,
    describe_try: Symbol,
    score_tree: Symbol,
    score_tree_list: Symbol,
    generic_id: Symbol,
    generic_choose: Symbol,
    score_iter: Symbol,
    score_boxed: Symbol,
    score_control: Symbol,
    score_dict: Symbol,
    score_list_ops: Symbol,
    score_str_ops: Symbol,
    score_try_ops: Symbol,
    score_set_ops: Symbol,
    score_num_ops: Symbol,
    score_patterns: Symbol,
    score_structural_keys: Symbol,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .app_output = .empty,
        .platform_output = .empty,
        .module_output = .empty,
        .app_file_name = null,
        .platform_file_name = null,
        .module_file_name = null,
        .next_id = 0,
        .symbols = undefined,
    };
}

pub fn deinit(self: *Self) void {
    self.app_output.deinit(self.allocator);
    self.platform_output.deinit(self.allocator);
    self.module_output.deinit(self.allocator);
    if (self.app_file_name) |name| self.allocator.free(name);
    if (self.platform_file_name) |name| self.allocator.free(name);
    if (self.module_file_name) |name| self.allocator.free(name);
}

pub fn getAppOutput(self: *const Self) []const u8 {
    return self.app_output.items;
}

pub fn getPlatformOutput(self: *const Self) []const u8 {
    return self.platform_output.items;
}

pub fn getModuleOutput(self: *const Self) []const u8 {
    return self.module_output.items;
}

pub fn getAppFileName(self: *const Self) []const u8 {
    return self.app_file_name orelse @panic("build fuzzer app file name requested before generation");
}

pub fn getPlatformFileName(self: *const Self) []const u8 {
    return self.platform_file_name orelse @panic("build fuzzer platform file name requested before generation");
}

pub fn getModuleFileName(self: *const Self) []const u8 {
    return self.module_file_name orelse @panic("build fuzzer module file name requested before generation");
}

pub fn generate(self: *Self) std.mem.Allocator.Error!void {
    self.app_output.clearRetainingCapacity();
    self.platform_output.clearRetainingCapacity();
    self.module_output.clearRetainingCapacity();
    self.next_id = 0;

    const app_file = self.fresh(.app_file);
    const platform_file = self.fresh(.platform_file);
    const module_file = self.fresh(.module_file);
    self.symbols = .{
        .app_entry = self.fresh(.function),
        .platform_alias = self.fresh(.package),
        .platform_wrapper = self.fresh(.function),
        .import_module = module_file,
        .item_type = self.fresh(.type),
        .state_type = self.fresh(.type),
        .err_type = self.fresh(.type),
        .builder_type = self.fresh(.type),
        .tree_type = self.fresh(.type),
        .key_type = self.fresh(.type),
        .imported_type = module_file,
        .err_missing = self.fresh(.tag),
        .err_bad = self.fresh(.tag),
        .tree_leaf = self.fresh(.tag),
        .tree_branch = self.fresh(.tag),
        .tree_named = self.fresh(.tag),
        .item_id = self.fresh(.field),
        .item_text = self.fresh(.field),
        .item_flag = self.fresh(.field),
        .imported_num = self.fresh(.field),
        .imported_text = self.fresh(.field),
        .state_items = self.fresh(.field),
        .state_count = self.fresh(.field),
        .state_label = self.fresh(.field),
        .builder_items = self.fresh(.field),
        .builder_label = self.fresh(.field),
        .builder_count = self.fresh(.field),
        .key_id = self.fresh(.field),
        .key_text = self.fresh(.field),
        .builder_make = self.fresh(.function),
        .builder_add = self.fresh(.function),
        .builder_add_many = self.fresh(.function),
        .builder_build = self.fresh(.function),
        .builder_text = self.fresh(.function),
        .key_from_item = self.fresh(.function),
        .key_fallback = self.fresh(.function),
        .key_score = self.fresh(.function),
        .imported_make = self.fresh(.function),
        .imported_score = self.fresh(.function),
        .make_item = self.fresh(.function),
        .sum_items = self.fresh(.function),
        .is_even = self.fresh(.function),
        .is_odd = self.fresh(.function),
        .get_item = self.fresh(.function),
        .try_item_score = self.fresh(.function),
        .collect_items = self.fresh(.function),
        .transform_items = self.fresh(.function),
        .fold_items = self.fresh(.function),
        .score_with = self.fresh(.function),
        .score_pairs = self.fresh(.function),
        .walk_rows = self.fresh(.function),
        .walk_cols = self.fresh(.function),
        .describe_try = self.fresh(.function),
        .score_tree = self.fresh(.function),
        .score_tree_list = self.fresh(.function),
        .generic_id = self.fresh(.function),
        .generic_choose = self.fresh(.function),
        .score_iter = self.fresh(.function),
        .score_boxed = self.fresh(.function),
        .score_control = self.fresh(.function),
        .score_dict = self.fresh(.function),
        .score_list_ops = self.fresh(.function),
        .score_str_ops = self.fresh(.function),
        .score_try_ops = self.fresh(.function),
        .score_set_ops = self.fresh(.function),
        .score_num_ops = self.fresh(.function),
        .score_patterns = self.fresh(.function),
        .score_structural_keys = self.fresh(.function),
    };

    try self.setFileNames(app_file, platform_file, module_file);
    try self.writeModule();
    try self.writePlatform();
    try self.writeApp();
}

fn setFileNames(self: *Self, app_file: Symbol, platform_file: Symbol, module_file: Symbol) std.mem.Allocator.Error!void {
    if (self.app_file_name) |name| self.allocator.free(name);
    if (self.platform_file_name) |name| self.allocator.free(name);
    if (self.module_file_name) |name| self.allocator.free(name);
    self.app_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(app_file.kind), app_file.id });
    self.platform_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(platform_file.kind), platform_file.id });
    self.module_file_name = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(module_file.kind), module_file.id });
}

fn writeModule(self: *Self) std.mem.Allocator.Error!void {
    const num = self.fresh(.value);
    const text = self.fresh(.value);
    const record = self.fresh(.value);
    const extra = self.fresh(.value);

    try self.writeModuleSymbol(self.symbols.imported_type);
    try self.writeModuleText(" := {\n");
    try self.writeModuleText("    ");
    try self.writeModuleSymbol(self.symbols.imported_num);
    try self.writeModuleText(" : U64,\n");
    try self.writeModuleText("    ");
    try self.writeModuleSymbol(self.symbols.imported_text);
    try self.writeModuleText(" : Str,\n");
    try self.writeModuleText("}.{\n");

    try self.writeModuleText("    ");
    try self.writeModuleSymbol(self.symbols.imported_make);
    try self.writeModuleText(" : U64, Str -> ");
    try self.writeModuleSymbol(self.symbols.imported_type);
    try self.writeModuleText("\n");
    try self.writeModuleText("    ");
    try self.writeModuleSymbol(self.symbols.imported_make);
    try self.writeModuleText(" = |");
    try self.writeModuleSymbol(num);
    try self.writeModuleText(", ");
    try self.writeModuleSymbol(text);
    try self.writeModuleText("| { ");
    try self.writeModuleSymbol(self.symbols.imported_num);
    try self.writeModuleText(": ");
    try self.writeModuleSymbol(num);
    try self.writeModuleText(", ");
    try self.writeModuleSymbol(self.symbols.imported_text);
    try self.writeModuleText(": ");
    try self.writeModuleSymbol(text);
    try self.writeModuleText(" }\n");

    try self.writeModuleText("    ");
    try self.writeModuleSymbol(self.symbols.imported_score);
    try self.writeModuleText(" : ");
    try self.writeModuleSymbol(self.symbols.imported_type);
    try self.writeModuleText(", U64 -> U64\n");
    try self.writeModuleText("    ");
    try self.writeModuleSymbol(self.symbols.imported_score);
    try self.writeModuleText(" = |");
    try self.writeModuleSymbol(record);
    try self.writeModuleText(", ");
    try self.writeModuleSymbol(extra);
    try self.writeModuleText("| ");
    try self.writeModuleSymbol(record);
    try self.writeModuleText(".");
    try self.writeModuleSymbol(self.symbols.imported_num);
    try self.writeModuleText(" + ");
    try self.writeModuleSymbol(extra);
    try self.writeModuleText(" + List.len(Str.to_utf8(");
    try self.writeModuleSymbol(record);
    try self.writeModuleText(".");
    try self.writeModuleSymbol(self.symbols.imported_text);
    try self.writeModuleText("))\n");
    try self.writeModuleText("}\n");
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
    try self.writeAppText("import ");
    try self.writeAppSymbol(self.symbols.import_module);
    try self.writeAppText("\n\n");

    try self.writeTypeDeclarations();
    try self.writeBuilderType();
    try self.writeKeyType();
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

    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText(" := [");
    try self.writeAppSymbol(self.symbols.tree_leaf);
    try self.writeAppText("(U64), ");
    try self.writeAppSymbol(self.symbols.tree_branch);
    try self.writeAppText("(List(");
    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText("), U64), ");
    try self.writeAppSymbol(self.symbols.tree_named);
    try self.writeAppText("(Str, List(");
    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText("))]\n\n");
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

fn writeKeyType(self: *Self) std.mem.Allocator.Error!void {
    const item = self.fresh(.value);
    const prefix = self.fresh(.value);
    const seed = self.fresh(.value);
    const text = self.fresh(.value);
    const left = self.fresh(.value);
    const right = self.fresh(.value);
    const key = self.fresh(.value);
    const hasher = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(" :: {\n");
    try self.writeFieldTypeLine(self.symbols.key_id, "U64");
    try self.writeFieldTypeLine(self.symbols.key_text, "Str");
    try self.writeAppText("}.{\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.key_from_item);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(", Str, U64 -> ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.key_from_item);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| { ");
    try self.writeAppSymbol(self.symbols.key_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.key_text);
    try self.writeAppText(": Str.concat(");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(") }\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.key_fallback);
    try self.writeAppText(" : Str, U64 -> ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.key_fallback);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| { ");
    try self.writeAppSymbol(self.symbols.key_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.key_text);
    try self.writeAppText(": ");
    try self.writeAppSymbol(text);
    try self.writeAppText(" }\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(" -> U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(key);
    try self.writeAppText("| ");
    try self.writeAppSymbol(key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_id);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_text);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppText("is_eq : ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(" -> Bool\n");
    try self.writeIndent(1);
    try self.writeAppText("is_eq = |");
    try self.writeAppSymbol(left);
    try self.writeAppText(", ");
    try self.writeAppSymbol(right);
    try self.writeAppText("| ");
    try self.writeAppSymbol(left);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_id);
    try self.writeAppText(" == ");
    try self.writeAppSymbol(right);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_id);
    try self.writeAppText(" and ");
    try self.writeAppSymbol(left);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_text);
    try self.writeAppText(" == ");
    try self.writeAppSymbol(right);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_text);
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppText("to_hash : ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(", Hasher -> Hasher\n");
    try self.writeIndent(1);
    try self.writeAppText("to_hash = |");
    try self.writeAppSymbol(key);
    try self.writeAppText(", ");
    try self.writeAppSymbol(hasher);
    try self.writeAppText("| Str.to_hash(");
    try self.writeAppSymbol(key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_text);
    try self.writeAppText(", U64.to_hash(");
    try self.writeAppSymbol(key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_id);
    try self.writeAppText(", ");
    try self.writeAppSymbol(hasher);
    try self.writeAppText("))\n");

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
    try self.writeTryItemScore();
    try self.writeCollectItems();
    try self.writeItemTransforms();
    try self.writeHigherOrderScoring();
    try self.writeTupleScoring();
    try self.writeGridRecursion();
    try self.writeDescribeTry();
    try self.writeTreeScoring();
    try self.writeGenericHelpers();
    try self.writeIteratorScoring();
    try self.writeBoxScoring();
    try self.writeControlFlowScoring();
    try self.writeDictScoring();
    try self.writeListOpsScoring();
    try self.writeStrOpsScoring();
    try self.writeTryOpsScoring();
    try self.writeSetOpsScoring();
    try self.writeNumOpsScoring();
    try self.writePatternScoring();
    try self.writeStructuralKeyScoring();
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

fn writeTryItemScore(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const index = self.fresh(.value);
    const item = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.try_item_score);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> Try(U64, ");
    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(")\n");
    try self.writeAppSymbol(self.symbols.try_item_score);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText("| {\n");
    try self.writeLocalHeader(item, self.symbols.item_type);
    try self.writeAppSymbol(self.symbols.get_item);
    try self.writeAppText("(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText(")?\n");
    try self.writeIndent(1);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(")))\n}\n\n");
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

fn writeItemTransforms(self: *Self) std.mem.Allocator.Error!void {
    const input_items = self.fresh(.value);
    const prefix = self.fresh(.value);
    const offset = self.fresh(.value);
    const mapped = self.fresh(.value);
    const map_item = self.fresh(.value);
    const map_index = self.fresh(.value);
    const sorted = self.fresh(.value);
    const sort_left = self.fresh(.value);
    const sort_right = self.fresh(.value);
    const filtered = self.fresh(.value);
    const keep_item = self.fresh(.value);
    const drop_item = self.fresh(.value);
    const second_map_item = self.fresh(.value);
    const fold_items = self.fresh(.value);
    const fold_seed = self.fresh(.value);
    const fold_acc = self.fresh(.value);
    const fold_item = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.transform_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), Str, U64 -> List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeAppSymbol(self.symbols.transform_items);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(input_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(offset);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" = List.map_with_index(");
    try self.writeAppSymbol(input_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(map_index);
    try self.writeAppText("| { ..");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(offset);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(map_index);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(": Str.concat(");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(": ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(map_index);
    try self.writeAppText(") })\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(sorted);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(sorted);
    try self.writeAppText(" = if ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(offset);
    try self.writeAppText(") List.sort_with(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText(", |");
    try self.writeAppSymbol(sort_left);
    try self.writeAppText(", ");
    try self.writeAppSymbol(sort_right);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(sort_left);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" < ");
    try self.writeAppSymbol(sort_right);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" LT else if ");
    try self.writeAppSymbol(sort_left);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" > ");
    try self.writeAppSymbol(sort_right);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" GT else EQ) else ");
    try self.writeAppSymbol(mapped);
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(filtered);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(filtered);
    try self.writeAppText(" = if ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(offset);
    try self.writeAppText(") List.keep_if(");
    try self.writeAppSymbol(sorted);
    try self.writeAppText(", |");
    try self.writeAppSymbol(keep_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(keep_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(") else List.drop_if(");
    try self.writeAppSymbol(sorted);
    try self.writeAppText(", |");
    try self.writeAppSymbol(drop_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(drop_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" == 0)\n");

    try self.writeIndent(1);
    try self.writeAppText("List.concat(");
    try self.writeAppSymbol(filtered);
    try self.writeAppText(", List.map(");
    try self.writeAppSymbol(input_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(second_map_item);
    try self.writeAppText("| { ..");
    try self.writeAppSymbol(second_map_item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(second_map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(offset);
    try self.writeAppText(" }))\n}\n\n");

    try self.writeAppSymbol(self.symbols.fold_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.fold_items);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(fold_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(fold_seed);
    try self.writeAppText("| List.fold(");
    try self.writeAppSymbol(fold_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(fold_seed);
    try self.writeAppText(", |");
    try self.writeAppSymbol(fold_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(fold_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(fold_acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(fold_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + (if ");
    try self.writeAppSymbol(fold_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(" 1 else 0))\n\n");
}

fn writeHigherOrderScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const scorer = self.fresh(.value);
    const seed = self.fresh(.value);
    const acc = self.fresh(.value);
    const item = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_with);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), (");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(" -> U64), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_with);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(scorer);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| List.fold(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", |");
    try self.writeAppSymbol(acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(scorer);
    try self.writeAppText("(");
    try self.writeAppSymbol(item);
    try self.writeAppText("))\n\n");
}

fn writeTupleScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const seed = self.fresh(.value);
    const pairs = self.fresh(.value);
    const map_item = self.fresh(.value);
    const acc = self.fresh(.value);
    const pair_id = self.fresh(.value);
    const pair_text = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_pairs);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_pairs);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(pairs);
    try self.writeAppText(" : List((U64, Str))\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(pairs);
    try self.writeAppText(" = List.map(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_item);
    try self.writeAppText("| (");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(", ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppText("List.fold(");
    try self.writeAppSymbol(pairs);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", |");
    try self.writeAppSymbol(acc);
    try self.writeAppText(", (");
    try self.writeAppSymbol(pair_id);
    try self.writeAppText(", ");
    try self.writeAppSymbol(pair_text);
    try self.writeAppText(")| ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pair_id);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(pair_text);
    try self.writeAppText(")))\n}\n\n");
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

fn writeTreeScoring(self: *Self) std.mem.Allocator.Error!void {
    const node = self.fresh(.value);
    const acc = self.fresh(.value);
    const leaf_value = self.fresh(.value);
    const branch_children = self.fresh(.value);
    const branch_weight = self.fresh(.value);
    const named_label = self.fresh(.value);
    const named_children = self.fresh(.value);
    const list = self.fresh(.value);
    const list_acc = self.fresh(.value);
    const first = self.fresh(.value);
    const rest = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_tree);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText(", U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_tree);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(node);
    try self.writeAppText(", ");
    try self.writeAppSymbol(acc);
    try self.writeAppText("| match ");
    try self.writeAppSymbol(node);
    try self.writeAppText(" {\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.tree_leaf);
    try self.writeAppText("(");
    try self.writeAppSymbol(leaf_value);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(leaf_value);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.tree_branch);
    try self.writeAppText("(");
    try self.writeAppSymbol(branch_children);
    try self.writeAppText(", ");
    try self.writeAppSymbol(branch_weight);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(self.symbols.score_tree_list);
    try self.writeAppText("(");
    try self.writeAppSymbol(branch_children);
    try self.writeAppText(", ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(branch_weight);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.tree_named);
    try self.writeAppText("(");
    try self.writeAppSymbol(named_label);
    try self.writeAppText(", ");
    try self.writeAppSymbol(named_children);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(self.symbols.score_tree_list);
    try self.writeAppText("(");
    try self.writeAppSymbol(named_children);
    try self.writeAppText(", ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(named_label);
    try self.writeAppText(")))\n}\n\n");

    try self.writeAppSymbol(self.symbols.score_tree_list);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_tree_list);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(list);
    try self.writeAppText(", ");
    try self.writeAppSymbol(list_acc);
    try self.writeAppText("| match ");
    try self.writeAppSymbol(list);
    try self.writeAppText(" {\n");
    try self.writeIndent(1);
    try self.writeAppText("[");
    try self.writeAppSymbol(first);
    try self.writeAppText(", .. as ");
    try self.writeAppSymbol(rest);
    try self.writeAppText("] => ");
    try self.writeAppSymbol(self.symbols.score_tree_list);
    try self.writeAppText("(");
    try self.writeAppSymbol(rest);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.score_tree);
    try self.writeAppText("(");
    try self.writeAppSymbol(first);
    try self.writeAppText(", ");
    try self.writeAppSymbol(list_acc);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppText("[] => ");
    try self.writeAppSymbol(list_acc);
    try self.writeAppText("\n}\n\n");
}

fn writeGenericHelpers(self: *Self) std.mem.Allocator.Error!void {
    const id_type = self.fresh(.type_var);
    const id_value = self.fresh(.value);
    const choose_type = self.fresh(.type_var);
    const condition = self.fresh(.value);
    const left = self.fresh(.value);
    const right = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(id_type);
    try self.writeAppText(" -> ");
    try self.writeAppSymbol(id_type);
    try self.writeAppText("\n");
    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(id_value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(id_value);
    try self.writeAppText("\n\n");

    try self.writeAppSymbol(self.symbols.generic_choose);
    try self.writeAppText(" : Bool, ");
    try self.writeAppSymbol(choose_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(choose_type);
    try self.writeAppText(" -> ");
    try self.writeAppSymbol(choose_type);
    try self.writeAppText("\n");
    try self.writeAppSymbol(self.symbols.generic_choose);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(condition);
    try self.writeAppText(", ");
    try self.writeAppSymbol(left);
    try self.writeAppText(", ");
    try self.writeAppSymbol(right);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(condition);
    try self.writeAppText(" ");
    try self.writeAppSymbol(left);
    try self.writeAppText(" else ");
    try self.writeAppSymbol(right);
    try self.writeAppText("\n\n");
}

fn writeIteratorScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const seed = self.fresh(.value);
    const ids = self.fresh(.value);
    const map_item = self.fresh(.value);
    const base = self.fresh(.value);
    const any_id = self.fresh(.value);
    const keep_id = self.fresh(.value);
    const map_id = self.fresh(.value);
    const acc = self.fresh(.value);
    const folded_id = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_iter);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_iter);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(ids);
    try self.writeAppText(" : List(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(ids);
    try self.writeAppText(" = List.map(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(base);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(base);
    try self.writeAppText(" = if List.any(");
    try self.writeAppSymbol(ids);
    try self.writeAppText(", |");
    try self.writeAppSymbol(any_id);
    try self.writeAppText("| ");
    try self.writeAppSymbol(any_id);
    try self.writeAppText(" == ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(") ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" else 0\n");

    try self.writeIndent(1);
    try self.writeAppText("Iter.fold(Iter.map(Iter.keep_if(");
    try self.writeAppSymbol(ids);
    try self.writeAppText(".iter(), |");
    try self.writeAppSymbol(keep_id);
    try self.writeAppText("| ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(keep_id);
    try self.writeAppText(")), |");
    try self.writeAppSymbol(map_id);
    try self.writeAppText("| ");
    try self.writeAppSymbol(map_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("), ");
    try self.writeAppSymbol(base);
    try self.writeAppText(", |");
    try self.writeAppSymbol(acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(folded_id);
    try self.writeAppText("| ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(folded_id);
    try self.writeAppText(")\n}\n\n");
}

fn writeBoxScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const tree = self.fresh(.value);
    const seed = self.fresh(.value);
    const boxed_tree = self.fresh(.value);
    const unboxed_tree = self.fresh(.value);
    const boxed_scorer = self.fresh(.value);
    const scorer = self.fresh(.value);
    const scorer_item = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_boxed);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText(", U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_boxed);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(tree);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(boxed_tree);
    try self.writeAppText(" = Box.box(");
    try self.writeAppSymbol(tree);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(unboxed_tree, self.symbols.tree_type);
    try self.writeAppText("Box.unbox(");
    try self.writeAppSymbol(boxed_tree);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(boxed_scorer);
    try self.writeAppText(" = Box.box(|");
    try self.writeAppSymbol(scorer_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(scorer_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(scorer);
    try self.writeAppText(" = Box.unbox(");
    try self.writeAppSymbol(boxed_scorer);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(self.symbols.score_with);
    try self.writeAppText("(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(scorer);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.score_tree);
    try self.writeAppText("(");
    try self.writeAppSymbol(unboxed_tree);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("))\n}\n\n");
}

fn writeControlFlowScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const seed = self.fresh(.value);
    const total = self.fresh(.value);
    const item = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_control);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_control);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppText("var $");
    try self.writeAppSymbol(total);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppText("for ");
    try self.writeAppSymbol(item);
    try self.writeAppText(" in ");
    try self.writeAppSymbol(items);
    try self.writeAppText(" {\n");

    try self.writeIndent(2);
    try self.writeAppText("if ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(" {\n");
    try self.writeIndent(3);
    try self.writeAppText("return $");
    try self.writeAppSymbol(total);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("}\n");

    try self.writeIndent(2);
    try self.writeAppText("if ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" == ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" {\n");
    try self.writeIndent(3);
    try self.writeAppText("break\n");
    try self.writeIndent(2);
    try self.writeAppText("}\n");

    try self.writeIndent(2);
    try self.writeAppText("$");
    try self.writeAppSymbol(total);
    try self.writeAppText(" = $");
    try self.writeAppSymbol(total);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppText("$");
    try self.writeAppSymbol(total);
    try self.writeAppText("\n}\n\n");
}

fn writeDictScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const prefix = self.fresh(.value);
    const seed = self.fresh(.value);
    const pairs = self.fresh(.value);
    const pair_item = self.fresh(.value);
    const dict = self.fresh(.value);
    const inserted = self.fresh(.value);
    const filtered = self.fresh(.value);
    const keep_key = self.fresh(.value);
    const keep_value = self.fresh(.value);
    const mapped = self.fresh(.value);
    const map_key = self.fresh(.value);
    const map_value = self.fresh(.value);
    const lookup = self.fresh(.value);
    const lookup_value = self.fresh(.value);
    const key_score = self.fresh(.value);
    const key_acc = self.fresh(.value);
    const key = self.fresh(.value);
    const value_score = self.fresh(.value);
    const value_acc = self.fresh(.value);
    const value = self.fresh(.value);
    const fold_acc = self.fresh(.value);
    const fold_key = self.fresh(.value);
    const fold_value = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_dict);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), Str, U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_dict);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(pairs);
    try self.writeAppText(" : List((Str, U64))\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(pairs);
    try self.writeAppText(" = List.map(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(pair_item);
    try self.writeAppText("| (Str.concat(");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(pair_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText("), ");
    try self.writeAppSymbol(pair_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(dict);
    try self.writeAppText(" : Dict(Str, U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(dict);
    try self.writeAppText(" = Dict.from_list(");
    try self.writeAppSymbol(pairs);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(inserted);
    try self.writeAppText(" : Dict(Str, U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(inserted);
    try self.writeAppText(" = Dict.insert(");
    try self.writeAppSymbol(dict);
    try self.writeAppText(", ");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(filtered);
    try self.writeAppText(" : Dict(Str, U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(filtered);
    try self.writeAppText(" = Dict.keep_if(");
    try self.writeAppSymbol(inserted);
    try self.writeAppText(", |(");
    try self.writeAppSymbol(keep_key);
    try self.writeAppText(", ");
    try self.writeAppSymbol(keep_value);
    try self.writeAppText(")| if Str.is_empty(");
    try self.writeAppSymbol(keep_key);
    try self.writeAppText(") False else ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(keep_value);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" : Dict(Str, U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" = Dict.map(");
    try self.writeAppSymbol(filtered);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_key);
    try self.writeAppText(", ");
    try self.writeAppSymbol(map_value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(map_value);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(map_key);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(lookup);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(lookup);
    try self.writeAppText(" = match Dict.get(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText(", ");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(lookup_value);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(lookup_value);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(key_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(key_score);
    try self.writeAppText(" = List.fold(Dict.keys(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText("), 0, |");
    try self.writeAppSymbol(key_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(key);
    try self.writeAppText("| ");
    try self.writeAppSymbol(key_acc);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(key);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(value_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(value_score);
    try self.writeAppText(" = List.fold(Dict.values(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText("), 0, |");
    try self.writeAppSymbol(value_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(value_acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(value);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppText("Dict.fold(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText(", ");
    try self.writeAppSymbol(lookup);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(key_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(value_score);
    try self.writeAppText(", |");
    try self.writeAppSymbol(fold_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(fold_key);
    try self.writeAppText(", ");
    try self.writeAppSymbol(fold_value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(fold_acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(fold_value);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(fold_key);
    try self.writeAppText(")))\n}\n\n");
}

fn writeListOpsScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const seed = self.fresh(.value);
    const replacement = self.fresh(.value);
    const grown = self.fresh(.value);
    const prefix = self.fresh(.value);
    const suffix = self.fresh(.value);
    const window = self.fresh(.value);
    const set_items = self.fresh(.value);
    const set_ok = self.fresh(.value);
    const update_items = self.fresh(.value);
    const update_ok = self.fresh(.value);
    const update_item = self.fresh(.value);
    const swap_items = self.fresh(.value);
    const swap_ok = self.fresh(.value);
    const first_score = self.fresh(.value);
    const first_item = self.fresh(.value);
    const last_score = self.fresh(.value);
    const last_item = self.fresh(.value);
    const found_score = self.fresh(.value);
    const found_item = self.fresh(.value);
    const index_score = self.fresh(.value);
    const found_index = self.fresh(.value);
    const count_score = self.fresh(.value);
    const count_item = self.fresh(.value);
    const split_parts = self.fresh(.value);
    const split_score = self.fresh(.value);
    const split_acc = self.fresh(.value);
    const split_chunk = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_list_ops);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_list_ops);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeLocalHeader(replacement, self.symbols.item_type);
    try self.writeAppSymbol(self.symbols.make_item);
    try self.writeAppText("(U64.to_str(");
    try self.writeAppSymbol(seed);
    try self.writeAppText("), ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(grown);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(grown);
    try self.writeAppText(" = List.prepend(List.append(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(replacement);
    try self.writeAppText("), ");
    try self.writeAppSymbol(replacement);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(prefix);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(prefix);
    try self.writeAppText(" = List.take_first(");
    try self.writeAppSymbol(grown);
    try self.writeAppText(", List.len(");
    try self.writeAppSymbol(items);
    try self.writeAppText(") + 1)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(suffix);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(suffix);
    try self.writeAppText(" = List.drop_first(");
    try self.writeAppSymbol(grown);
    try self.writeAppText(", 1)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(window);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(window);
    try self.writeAppText(" = List.sublist(List.concat(");
    try self.writeAppSymbol(prefix);
    try self.writeAppText(", ");
    try self.writeAppSymbol(suffix);
    try self.writeAppText("), { start: 0, len: List.len(");
    try self.writeAppSymbol(items);
    try self.writeAppText(") })\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(set_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(set_items);
    try self.writeAppText(" = match List.set(");
    try self.writeAppSymbol(window);
    try self.writeAppText(", 0, ");
    try self.writeAppSymbol(replacement);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(set_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(set_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(window);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(update_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(update_items);
    try self.writeAppText(" = match List.update(");
    try self.writeAppSymbol(set_items);
    try self.writeAppText(", 0, |");
    try self.writeAppSymbol(update_item);
    try self.writeAppText("| { ..");
    try self.writeAppSymbol(update_item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(update_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" }) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(update_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(update_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(set_items);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(" = match List.swap(");
    try self.writeAppSymbol(update_items);
    try self.writeAppText(", 0, List.len(");
    try self.writeAppSymbol(update_items);
    try self.writeAppText(") - 1) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(swap_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(swap_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(update_items);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(first_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(first_score);
    try self.writeAppText(" = match List.first(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(first_item);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(first_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(last_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(last_score);
    try self.writeAppText(" = match List.last(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(last_item);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(last_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(found_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(found_score);
    try self.writeAppText(" = match List.find_last(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(found_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(found_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(found_item);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(found_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(index_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(index_score);
    try self.writeAppText(" = match List.find_first_index(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(found_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(found_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" == ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(found_index);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(found_index);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(count_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(count_score);
    try self.writeAppText(" = List.count_if(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(count_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(count_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(split_parts);
    try self.writeAppText(" : List(List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(split_parts);
    try self.writeAppText(" = List.split_if(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(count_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(count_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(split_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(split_score);
    try self.writeAppText(" = List.fold(");
    try self.writeAppSymbol(split_parts);
    try self.writeAppText(", 0, |");
    try self.writeAppSymbol(split_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(split_chunk);
    try self.writeAppText("| ");
    try self.writeAppSymbol(split_acc);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(split_chunk);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(first_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(last_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(found_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(index_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(count_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(split_score);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(swap_items);
    try self.writeAppText(")\n}\n\n");
}

fn writeStrOpsScoring(self: *Self) std.mem.Allocator.Error!void {
    const text = self.fresh(.value);
    const seed = self.fresh(.value);
    const delimiter = self.fresh(.value);
    const suffix = self.fresh(.value);
    const joined_seed = self.fresh(.value);
    const normalized = self.fresh(.value);
    const repeated = self.fresh(.value);
    const prefixed = self.fresh(.value);
    const dropped = self.fresh(.value);
    const pieces = self.fresh(.value);
    const joined = self.fresh(.value);
    const bytes = self.fresh(.value);
    const roundtrip = self.fresh(.value);
    const roundtrip_ok = self.fresh(.value);
    const quoted = self.fresh(.value);
    const quoted_ok = self.fresh(.value);
    const reserved = self.fresh(.value);
    const contains_score = self.fresh(.value);
    const starts_score = self.fresh(.value);
    const ends_score = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_str_ops);
    try self.writeAppText(" : Str, U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_str_ops);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(delimiter);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(delimiter);
    try self.writeAppText(" = ");
    try self.writeNonEmptyStringLiteral();
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(suffix);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(suffix);
    try self.writeAppText(" = ");
    try self.writeNonEmptyStringLiteral();
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(joined_seed);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(joined_seed);
    try self.writeAppText(" = Str.concat(Str.trim(");
    try self.writeAppSymbol(text);
    try self.writeAppText("), U64.to_str(");
    try self.writeAppSymbol(seed);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(normalized);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(normalized);
    try self.writeAppText(" = Str.with_ascii_lowercased(Str.with_ascii_uppercased(");
    try self.writeAppSymbol(joined_seed);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(repeated);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(repeated);
    try self.writeAppText(" = Str.repeat(");
    try self.writeAppSymbol(delimiter);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 4 + 1)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(prefixed);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(prefixed);
    try self.writeAppText(" = Str.with_prefix(Str.concat(");
    try self.writeAppSymbol(normalized);
    try self.writeAppText(", ");
    try self.writeAppSymbol(suffix);
    try self.writeAppText("), ");
    try self.writeAppSymbol(repeated);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(dropped);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(dropped);
    try self.writeAppText(" = Str.drop_suffix(Str.drop_prefix(");
    try self.writeAppSymbol(prefixed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(repeated);
    try self.writeAppText("), ");
    try self.writeAppSymbol(suffix);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(pieces);
    try self.writeAppText(" : List(Str)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(pieces);
    try self.writeAppText(" = Str.split_on(Str.concat(");
    try self.writeAppSymbol(dropped);
    try self.writeAppText(", ");
    try self.writeAppSymbol(delimiter);
    try self.writeAppText("), ");
    try self.writeAppSymbol(delimiter);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(joined);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(joined);
    try self.writeAppText(" = Str.join_with(");
    try self.writeAppSymbol(pieces);
    try self.writeAppText(", ");
    try self.writeAppSymbol(delimiter);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(bytes);
    try self.writeAppText(" : List(U8)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(bytes);
    try self.writeAppText(" = Str.to_utf8(");
    try self.writeAppSymbol(joined);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(roundtrip);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(roundtrip);
    try self.writeAppText(" = match Str.from_utf8(");
    try self.writeAppSymbol(bytes);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(roundtrip_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(roundtrip_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => Str.from_utf8_lossy(");
    try self.writeAppSymbol(bytes);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(quoted);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(quoted);
    try self.writeAppText(" = match Str.from_quote(");
    try self.writeAppSymbol(joined);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(quoted_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(quoted_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(joined);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(reserved);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(reserved);
    try self.writeAppText(" = Str.release_excess_capacity(Str.reserve(Str.with_capacity(List.len(");
    try self.writeAppSymbol(bytes);
    try self.writeAppText(")), ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 8))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" = if Str.contains(");
    try self.writeAppSymbol(joined);
    try self.writeAppText(", ");
    try self.writeAppSymbol(delimiter);
    try self.writeAppText(") 1 else 0\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(starts_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(starts_score);
    try self.writeAppText(" = if Str.starts_with(");
    try self.writeAppSymbol(prefixed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(repeated);
    try self.writeAppText(") 2 else 0\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(ends_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(ends_score);
    try self.writeAppText(" = if Str.ends_with(Str.concat(");
    try self.writeAppSymbol(joined);
    try self.writeAppText(", ");
    try self.writeAppSymbol(suffix);
    try self.writeAppText("), ");
    try self.writeAppSymbol(suffix);
    try self.writeAppText(") 3 else 0\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(starts_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(ends_score);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(bytes);
    try self.writeAppText(") + List.len(Str.to_utf8(");
    try self.writeAppSymbol(roundtrip);
    try self.writeAppText(")) + List.len(Str.to_utf8(");
    try self.writeAppSymbol(quoted);
    try self.writeAppText(")) + List.len(Str.to_utf8(");
    try self.writeAppSymbol(reserved);
    try self.writeAppText("))\n}\n\n");
}

fn writeTryOpsScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const index = self.fresh(.value);
    const text = self.fresh(.value);
    const base = self.fresh(.value);
    const mapped = self.fresh(.value);
    const map_item = self.fresh(.value);
    const remapped = self.fresh(.value);
    const map_err = self.fresh(.value);
    const err_payload = self.fresh(.value);
    const ok_score = self.fresh(.value);
    const err_score = self.fresh(.value);
    const remapped_payload = self.fresh(.value);
    const ok_bonus = self.fresh(.value);
    const err_bonus = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_try_ops);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64, Str -> U64\n");
    try self.writeAppSymbol(self.symbols.score_try_ops);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText(", ");
    try self.writeAppSymbol(text);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(base);
    try self.writeAppText(" : Try(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(base);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.get_item);
    try self.writeAppText("(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" : Try(U64, ");
    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" = Try.map_ok(");
    try self.writeAppSymbol(base);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(remapped);
    try self.writeAppText(" : Try(U64, ");
    try self.writeAppSymbol(self.symbols.err_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(remapped);
    try self.writeAppText(" = Try.map_err(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_err);
    try self.writeAppText("| match ");
    try self.writeAppSymbol(map_err);
    try self.writeAppText(" {\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.err_missing);
    try self.writeAppText(" => ");
    try self.writeAppSymbol(self.symbols.err_bad);
    try self.writeAppText("(");
    try self.writeAppSymbol(text);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.err_bad);
    try self.writeAppText("(");
    try self.writeAppSymbol(err_payload);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(self.symbols.err_bad);
    try self.writeAppText("(Str.concat(");
    try self.writeAppSymbol(err_payload);
    try self.writeAppText(", ");
    try self.writeAppSymbol(text);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppText("})\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(ok_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(ok_score);
    try self.writeAppText(" = Try.ok_or(");
    try self.writeAppSymbol(remapped);
    try self.writeAppText(", ");
    try self.writeAppSymbol(index);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(err_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(err_score);
    try self.writeAppText(" = match Try.err_or(");
    try self.writeAppSymbol(remapped);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.err_missing);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.err_missing);
    try self.writeAppText(" => 1\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.err_bad);
    try self.writeAppText("(");
    try self.writeAppSymbol(remapped_payload);
    try self.writeAppText(") => List.len(Str.to_utf8(");
    try self.writeAppSymbol(remapped_payload);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(ok_bonus);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(ok_bonus);
    try self.writeAppText(" = if Try.is_ok(");
    try self.writeAppSymbol(remapped);
    try self.writeAppText(") 2 else 0\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(err_bonus);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(err_bonus);
    try self.writeAppText(" = if Try.is_err(");
    try self.writeAppSymbol(remapped);
    try self.writeAppText(") 3 else 0\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(ok_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(err_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(ok_bonus);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(err_bonus);
    try self.writeAppText("\n}\n\n");
}

fn writeSetOpsScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const seed = self.fresh(.value);
    const ids = self.fresh(.value);
    const map_item = self.fresh(.value);
    const base = self.fresh(.value);
    const extended = self.fresh(.value);
    const trimmed = self.fresh(.value);
    const kept = self.fresh(.value);
    const keep_value = self.fresh(.value);
    const dropped = self.fresh(.value);
    const drop_value = self.fresh(.value);
    const merged = self.fresh(.value);
    const overlap = self.fresh(.value);
    const diffed = self.fresh(.value);
    const mapped = self.fresh(.value);
    const map_value = self.fresh(.value);
    const contains_score = self.fresh(.value);
    const acc = self.fresh(.value);
    const value = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_set_ops);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_set_ops);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(ids);
    try self.writeAppText(" : List(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(ids);
    try self.writeAppText(" = List.map(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(base);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(base);
    try self.writeAppText(" = Set.from_list(");
    try self.writeAppSymbol(ids);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(extended);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(extended);
    try self.writeAppText(" = Set.insert(Set.insert(");
    try self.writeAppSymbol(base);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("), List.len(");
    try self.writeAppSymbol(ids);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(trimmed);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(trimmed);
    try self.writeAppText(" = Set.remove(");
    try self.writeAppSymbol(extended);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" + 1)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(kept);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(kept);
    try self.writeAppText(" = Set.keep_if(");
    try self.writeAppSymbol(trimmed);
    try self.writeAppText(", |");
    try self.writeAppSymbol(keep_value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(keep_value);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(dropped);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(dropped);
    try self.writeAppText(" = Set.drop_if(");
    try self.writeAppSymbol(trimmed);
    try self.writeAppText(", |");
    try self.writeAppSymbol(drop_value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(drop_value);
    try self.writeAppText(" == ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(merged);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(merged);
    try self.writeAppText(" = Set.union(");
    try self.writeAppSymbol(kept);
    try self.writeAppText(", ");
    try self.writeAppSymbol(dropped);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(overlap);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(overlap);
    try self.writeAppText(" = Set.intersection(");
    try self.writeAppSymbol(merged);
    try self.writeAppText(", Set.single(");
    try self.writeAppSymbol(seed);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(diffed);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(diffed);
    try self.writeAppText(" = Set.difference(");
    try self.writeAppSymbol(merged);
    try self.writeAppText(", ");
    try self.writeAppSymbol(overlap);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" : Set(U64)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(mapped);
    try self.writeAppText(" = Set.map(");
    try self.writeAppSymbol(diffed);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(map_value);
    try self.writeAppText(" + Set.len(");
    try self.writeAppSymbol(overlap);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" = if Set.contains(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(") 1 else 0\n");

    try self.writeIndent(1);
    try self.writeAppText("List.fold(Set.to_list(");
    try self.writeAppSymbol(mapped);
    try self.writeAppText("), ");
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" + Set.len(");
    try self.writeAppSymbol(merged);
    try self.writeAppText(") + Set.len(");
    try self.writeAppSymbol(overlap);
    try self.writeAppText("), |");
    try self.writeAppSymbol(acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(value);
    try self.writeAppText("| ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(value);
    try self.writeAppText(")\n}\n\n");
}

fn writeNumOpsScoring(self: *Self) std.mem.Allocator.Error!void {
    const seed = self.fresh(.value);
    const text = self.fresh(.value);
    const shift = self.fresh(.value);
    const small = self.fresh(.value);
    const digits = self.fresh(.value);
    const digits_value = self.fresh(.value);
    const digits_u64 = self.fresh(.value);
    const parsed_u64 = self.fresh(.value);
    const parsed_u64_ok = self.fresh(.value);
    const parsed_u8 = self.fresh(.value);
    const parsed_u8_ok = self.fresh(.value);
    const parsed_i64 = self.fresh(.value);
    const parsed_i64_ok = self.fresh(.value);
    const tried_u8 = self.fresh(.value);
    const tried_u8_ok = self.fresh(.value);
    const checked_add = self.fresh(.value);
    const checked_add_ok = self.fresh(.value);
    const checked_sub = self.fresh(.value);
    const checked_sub_ok = self.fresh(.value);
    const checked_mul = self.fresh(.value);
    const checked_mul_ok = self.fresh(.value);
    const checked_div = self.fresh(.value);
    const checked_div_ok = self.fresh(.value);
    const overflow_add = self.fresh(.value);
    const overflow_add_ok = self.fresh(.value);
    const signed_add = self.fresh(.value);
    const signed_add_ok = self.fresh(.value);
    const bits = self.fresh(.value);
    const signed_bits = self.fresh(.value);
    const u8_bits = self.fresh(.value);
    const range_score = self.fresh(.value);
    const range_acc = self.fresh(.value);
    const range_item = self.fresh(.value);
    const u8_range_score = self.fresh(.value);
    const u8_range_acc = self.fresh(.value);
    const u8_range_item = self.fresh(.value);
    const text_score = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_num_ops);
    try self.writeAppText(" : U64, Str -> U64\n");
    try self.writeAppSymbol(self.symbols.score_num_ops);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(text);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(shift);
    try self.writeAppText(" : U8\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(shift);
    try self.writeAppText(" = U64.to_u8_wrap(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 8)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(small);
    try self.writeAppText(" : U8\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(small);
    try self.writeAppText(" = U64.to_u8_wrap(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(digits);
    try self.writeAppText(" : List(U8)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(digits);
    try self.writeAppText(" = [U64.to_u8_wrap(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 10), U64.to_u8_wrap(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 7), ");
    try self.writeAppSymbol(small);
    try self.writeAppText("]\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(digits_value);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(digits_value);
    try self.writeAppText(" = match U64.from_int_digits(");
    try self.writeAppSymbol(digits);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(digits_u64);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(digits_u64);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(" = match U64.from_str(Str.concat(U64.to_str(");
    try self.writeAppSymbol(seed);
    try self.writeAppText("), ");
    try self.writeAppSymbol(text);
    try self.writeAppText(")) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(parsed_u64_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(parsed_u64_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(digits_value);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(parsed_u8);
    try self.writeAppText(" : U8\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(parsed_u8);
    try self.writeAppText(" = match U8.from_str(U8.to_str(");
    try self.writeAppSymbol(small);
    try self.writeAppText(")) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(parsed_u8_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(parsed_u8_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(small);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText(" : I64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText(" = match I64.from_str(Str.concat(\"-\", U64.to_str(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 1000))) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(parsed_i64_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(parsed_i64_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(tried_u8);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(tried_u8);
    try self.writeAppText(" = match U64.to_u8_try(");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(tried_u8_ok);
    try self.writeAppText(") => U8.to_u64(");
    try self.writeAppSymbol(tried_u8_ok);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => U8.to_u64(");
    try self.writeAppSymbol(small);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(checked_add);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(checked_add);
    try self.writeAppText(" = match U64.add_checked(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(checked_add_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(checked_add_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => U64.highest\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(checked_sub);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(checked_sub);
    try self.writeAppText(" = match U64.sub_checked(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(checked_sub_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(checked_sub_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => U64.lowest\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(checked_mul);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(checked_mul);
    try self.writeAppText(" = match U64.mul_checked(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 16, ");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(" % 16) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(checked_mul_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(checked_mul_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(checked_div);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(checked_div);
    try self.writeAppText(" = match U64.div_checked(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", (");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(" % 7) + 1) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(checked_div_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(checked_div_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(overflow_add);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(overflow_add);
    try self.writeAppText(" = match U64.add_checked(U64.highest, ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(overflow_add_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(overflow_add_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(checked_add);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(signed_add);
    try self.writeAppText(" : I64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(signed_add);
    try self.writeAppText(" = match I64.add_checked(");
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText(", U64.to_i64_wrap(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 1000)) {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(signed_add_ok);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(signed_add_ok);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(bits);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(bits);
    try self.writeAppText(" = U64.bitwise_xor(U64.bitwise_and(U64.shift_left_by(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(shift);
    try self.writeAppText("), U64.bitwise_not(");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText(")), U64.shift_right_by(U64.bitwise_or(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(checked_div);
    try self.writeAppText("), ");
    try self.writeAppSymbol(shift);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(signed_bits);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(signed_bits);
    try self.writeAppText(" = I64.to_u64_wrap(I64.bitwise_xor(I64.shift_right_by(");
    try self.writeAppSymbol(signed_add);
    try self.writeAppText(", ");
    try self.writeAppSymbol(shift);
    try self.writeAppText("), I64.bitwise_not(");
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(u8_bits);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(u8_bits);
    try self.writeAppText(" = U8.to_u64(U8.bitwise_xor(U8.shift_left_by(");
    try self.writeAppSymbol(parsed_u8);
    try self.writeAppText(", ");
    try self.writeAppSymbol(shift);
    try self.writeAppText("), U8.bitwise_not(");
    try self.writeAppSymbol(small);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(range_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(range_score);
    try self.writeAppText(" = Iter.fold(U64.until(0, ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 4), 0, |");
    try self.writeAppSymbol(range_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(range_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(range_acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(range_item);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(u8_range_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(u8_range_score);
    try self.writeAppText(" = Iter.fold(U8.to(0, U64.to_u8_wrap(");
    try self.writeAppSymbol(seed);
    try self.writeAppText(" % 3)), 0, |");
    try self.writeAppSymbol(u8_range_acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(u8_range_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(u8_range_acc);
    try self.writeAppText(" + U8.to_u64(");
    try self.writeAppSymbol(u8_range_item);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(text_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(text_score);
    try self.writeAppText(" = List.len(Str.to_utf8(U64.to_str(");
    try self.writeAppSymbol(parsed_u64);
    try self.writeAppText("))) + List.len(Str.to_utf8(U8.to_str(");
    try self.writeAppSymbol(parsed_u8);
    try self.writeAppText("))) + List.len(Str.to_utf8(I64.to_str(");
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(tried_u8);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(checked_add);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(checked_sub);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(checked_mul);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(checked_div);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(overflow_add);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(bits);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(signed_bits);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(u8_bits);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(range_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(u8_range_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(text_score);
    try self.writeAppText(" + I64.abs_diff(");
    try self.writeAppSymbol(signed_add);
    try self.writeAppText(", ");
    try self.writeAppSymbol(parsed_i64);
    try self.writeAppText(")\n}\n\n");
}

fn writePatternScoring(self: *Self) std.mem.Allocator.Error!void {
    const state = self.fresh(.value);
    const tree = self.fresh(.value);
    const imported = self.fresh(.value);
    const state_items = self.fresh(.value);
    const state_count = self.fresh(.value);
    const state_label = self.fresh(.value);
    const parts = self.fresh(.value);
    const empty_count = self.fresh(.value);
    const empty_label = self.fresh(.value);
    const head_id = self.fresh(.value);
    const head_text = self.fresh(.value);
    const head_flag = self.fresh(.value);
    const rest_items = self.fresh(.value);
    const nonempty_count = self.fresh(.value);
    const nonempty_label = self.fresh(.value);
    const state_score = self.fresh(.value);
    const tree_score = self.fresh(.value);
    const leaf_value = self.fresh(.value);
    const first_leaf_value = self.fresh(.value);
    const branch_rest = self.fresh(.value);
    const first_branch_extra = self.fresh(.value);
    const branch_children = self.fresh(.value);
    const second_branch_extra = self.fresh(.value);
    const named_label = self.fresh(.value);
    const first_child = self.fresh(.value);
    const child_rest = self.fresh(.value);
    const empty_named_label = self.fresh(.value);
    const imported_score = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_patterns);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.state_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.tree_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.imported_type);
    try self.writeAppText(" -> U64\n");
    try self.writeAppSymbol(self.symbols.score_patterns);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(tree);
    try self.writeAppText(", ");
    try self.writeAppSymbol(imported);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(parts);
    try self.writeAppText(" : (List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), U64, Str)\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(parts);
    try self.writeAppText(" = match ");
    try self.writeAppSymbol(state);
    try self.writeAppText(" {\n");
    try self.writeIndent(2);
    try self.writeAppText("{ ");
    try self.writeAppSymbol(self.symbols.state_items);
    try self.writeAppText(": ");
    try self.writeAppSymbol(state_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.state_count);
    try self.writeAppText(": ");
    try self.writeAppSymbol(state_count);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.state_label);
    try self.writeAppText(": ");
    try self.writeAppSymbol(state_label);
    try self.writeAppText(" } => (");
    try self.writeAppSymbol(state_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(state_count);
    try self.writeAppText(", ");
    try self.writeAppSymbol(state_label);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(state_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(state_score);
    try self.writeAppText(" = match ");
    try self.writeAppSymbol(parts);
    try self.writeAppText(" {\n");
    try self.writeIndent(2);
    try self.writeAppText("([], ");
    try self.writeAppSymbol(empty_count);
    try self.writeAppText(", ");
    try self.writeAppSymbol(empty_label);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(empty_count);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(empty_label);
    try self.writeAppText("))\n");
    try self.writeIndent(2);
    try self.writeAppText("([{ ");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(": ");
    try self.writeAppSymbol(head_id);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText(": ");
    try self.writeAppSymbol(head_text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_flag);
    try self.writeAppText(": ");
    try self.writeAppSymbol(head_flag);
    try self.writeAppText(" }, .. as ");
    try self.writeAppSymbol(rest_items);
    try self.writeAppText("], ");
    try self.writeAppSymbol(nonempty_count);
    try self.writeAppText(", ");
    try self.writeAppSymbol(nonempty_label);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(head_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(nonempty_count);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(rest_items);
    try self.writeAppText(") + List.len(Str.to_utf8(Str.concat(");
    try self.writeAppSymbol(head_text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(nonempty_label);
    try self.writeAppText("))) + if ");
    try self.writeAppSymbol(head_flag);
    try self.writeAppText(" 1 else 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" = match ");
    try self.writeAppSymbol(tree);
    try self.writeAppText(" {\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.tree_leaf);
    try self.writeAppText("(");
    try self.writeAppSymbol(leaf_value);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(leaf_value);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.tree_branch);
    try self.writeAppText("([");
    try self.writeAppSymbol(self.symbols.tree_leaf);
    try self.writeAppText("(");
    try self.writeAppSymbol(first_leaf_value);
    try self.writeAppText("), .. as ");
    try self.writeAppSymbol(branch_rest);
    try self.writeAppText("], ");
    try self.writeAppSymbol(first_branch_extra);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(first_leaf_value);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(first_branch_extra);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(branch_rest);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.tree_branch);
    try self.writeAppText("(");
    try self.writeAppSymbol(branch_children);
    try self.writeAppText(", ");
    try self.writeAppSymbol(second_branch_extra);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(second_branch_extra);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(branch_children);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.tree_named);
    try self.writeAppText("(");
    try self.writeAppSymbol(named_label);
    try self.writeAppText(", [");
    try self.writeAppSymbol(first_child);
    try self.writeAppText(", .. as ");
    try self.writeAppSymbol(child_rest);
    try self.writeAppText("]) => ");
    try self.writeAppSymbol(self.symbols.score_patterns);
    try self.writeAppText("({ ..");
    try self.writeAppSymbol(state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.state_count);
    try self.writeAppText(": ");
    try self.writeAppSymbol(state);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.state_count);
    try self.writeAppText(" + List.len(");
    try self.writeAppSymbol(child_rest);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.state_label);
    try self.writeAppText(": ");
    try self.writeAppSymbol(named_label);
    try self.writeAppText("}, ");
    try self.writeAppSymbol(first_child);
    try self.writeAppText(", ");
    try self.writeAppSymbol(imported);
    try self.writeAppText(") + List.len(");
    try self.writeAppSymbol(child_rest);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppSymbol(self.symbols.tree_named);
    try self.writeAppText("(");
    try self.writeAppSymbol(empty_named_label);
    try self.writeAppText(", []) => List.len(Str.to_utf8(");
    try self.writeAppSymbol(empty_named_label);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(imported_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(imported_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(imported);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.imported_score);
    try self.writeAppText("(");
    try self.writeAppSymbol(state_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(state_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(imported_score);
    try self.writeAppText("\n}\n\n");
}

fn writeStructuralKeyScoring(self: *Self) std.mem.Allocator.Error!void {
    const items = self.fresh(.value);
    const text = self.fresh(.value);
    const seed = self.fresh(.value);
    const keyed = self.fresh(.value);
    const map_item = self.fresh(.value);
    const dict = self.fresh(.value);
    const first_key = self.fresh(.value);
    const first_item = self.fresh(.value);
    const lookup = self.fresh(.value);
    const lookup_item = self.fresh(.value);
    const key_set = self.fresh(.value);
    const mapped_set = self.fresh(.value);
    const map_key = self.fresh(.value);
    const contains_score = self.fresh(.value);
    const sorted_keys = self.fresh(.value);
    const left_key = self.fresh(.value);
    const right_key = self.fresh(.value);
    const acc = self.fresh(.value);
    const fold_key = self.fresh(.value);

    try self.writeAppSymbol(self.symbols.score_structural_keys);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("), Str, U64 -> U64\n");
    try self.writeAppSymbol(self.symbols.score_structural_keys);
    try self.writeAppText(" = |");
    try self.writeAppSymbol(items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("| {\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(keyed);
    try self.writeAppText(" : List((");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText("))\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(keyed);
    try self.writeAppText(" = List.map(");
    try self.writeAppSymbol(items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_item);
    try self.writeAppText("| (");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_from_item);
    try self.writeAppText("(");
    try self.writeAppSymbol(map_item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("), ");
    try self.writeAppSymbol(map_item);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(dict);
    try self.writeAppText(" : Dict(");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(", ");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(dict);
    try self.writeAppText(" = Dict.from_list(");
    try self.writeAppSymbol(keyed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(first_key);
    try self.writeAppText(" : ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(first_key);
    try self.writeAppText(" = match List.first(");
    try self.writeAppSymbol(items);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(first_item);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_from_item);
    try self.writeAppText("(");
    try self.writeAppSymbol(first_item);
    try self.writeAppText(", ");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_fallback);
    try self.writeAppText("(");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(seed);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(lookup);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(lookup);
    try self.writeAppText(" = match Dict.get(");
    try self.writeAppSymbol(dict);
    try self.writeAppText(", ");
    try self.writeAppSymbol(first_key);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(lookup_item);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(lookup_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + List.len(Str.to_utf8(");
    try self.writeAppSymbol(lookup_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_text);
    try self.writeAppText("))\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => ");
    try self.writeAppSymbol(seed);
    try self.writeAppText("\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(key_set);
    try self.writeAppText(" : Set(");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(key_set);
    try self.writeAppText(" = Set.from_list(Dict.keys(");
    try self.writeAppSymbol(dict);
    try self.writeAppText("))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(mapped_set);
    try self.writeAppText(" : Set(");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(mapped_set);
    try self.writeAppText(" = Set.map(");
    try self.writeAppSymbol(key_set);
    try self.writeAppText(", |");
    try self.writeAppSymbol(map_key);
    try self.writeAppText("| ");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_fallback);
    try self.writeAppText("(");
    try self.writeAppSymbol(text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(map_key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText("() + Set.len(");
    try self.writeAppSymbol(key_set);
    try self.writeAppText(")))\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" = if Set.contains(");
    try self.writeAppSymbol(key_set);
    try self.writeAppText(", ");
    try self.writeAppSymbol(first_key);
    try self.writeAppText(") 1 else 0\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(sorted_keys);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.key_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(sorted_keys);
    try self.writeAppText(" = List.sort_with(Set.to_list(");
    try self.writeAppSymbol(mapped_set);
    try self.writeAppText("), |");
    try self.writeAppSymbol(left_key);
    try self.writeAppText(", ");
    try self.writeAppSymbol(right_key);
    try self.writeAppText("| if ");
    try self.writeAppSymbol(left_key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText("() < ");
    try self.writeAppSymbol(right_key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText("() LT else if ");
    try self.writeAppSymbol(left_key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText("() > ");
    try self.writeAppSymbol(right_key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText("() GT else EQ)\n");

    try self.writeIndent(1);
    try self.writeAppText("List.fold(");
    try self.writeAppSymbol(sorted_keys);
    try self.writeAppText(", ");
    try self.writeAppSymbol(lookup);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(contains_score);
    try self.writeAppText(" + Set.len(");
    try self.writeAppSymbol(key_set);
    try self.writeAppText("), |");
    try self.writeAppSymbol(acc);
    try self.writeAppText(", ");
    try self.writeAppSymbol(fold_key);
    try self.writeAppText("| ");
    try self.writeAppSymbol(acc);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(fold_key);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.key_score);
    try self.writeAppText("())\n}\n\n");
}

fn writeEntryPoint(self: *Self) std.mem.Allocator.Error!void {
    const input = self.fresh(.value);
    const first = self.fresh(.value);
    const second = self.fresh(.value);
    const third = self.fresh(.value);
    const items = self.fresh(.value);
    const collected = self.fresh(.value);
    const transformed = self.fresh(.value);
    const folded = self.fresh(.value);
    const pair_score = self.fresh(.value);
    const try_score = self.fresh(.value);
    const try_value = self.fresh(.value);
    const generic_num = self.fresh(.value);
    const generic_text = self.fresh(.value);
    const generic_item = self.fresh(.value);
    const generic_items = self.fresh(.value);
    const iter_score = self.fresh(.value);
    const control_score = self.fresh(.value);
    const dict_score = self.fresh(.value);
    const list_ops_score = self.fresh(.value);
    const str_score = self.fresh(.value);
    const try_ops_score = self.fresh(.value);
    const set_ops_score = self.fresh(.value);
    const num_ops_score = self.fresh(.value);
    const imported_record = self.fresh(.value);
    const alternate_imported = self.fresh(.value);
    const generic_imported = self.fresh(.value);
    const imported_score = self.fresh(.value);
    const builder = self.fresh(.value);
    const state = self.fresh(.value);
    const tree = self.fresh(.value);
    const tree_score = self.fresh(.value);
    const pattern_score = self.fresh(.value);
    const structural_score = self.fresh(.value);
    const boxed_score = self.fresh(.value);
    const captured_score = self.fresh(.value);
    const captured_item = self.fresh(.value);
    const walked = self.fresh(.value);
    const picked = self.fresh(.value);
    const rows = self.reader.intRangeAtMost(u8, 1, 6);
    const tree_depth = self.reader.intRangeAtMost(u8, 1, 4);
    const first_id = self.reader.intRangeAtMost(u8, 0, 9);
    const second_id = self.reader.intRangeAtMost(u8, 10, 29);
    const third_id = self.reader.intRangeAtMost(u8, 30, 59);
    const transform_offset = self.reader.intRangeAtMost(u8, 0, 7);
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
    try self.writeAppSymbol(transformed);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(transformed);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.transform_items);
    try self.writeAppText("(");
    try self.writeAppSymbol(collected);
    try self.writeAppText(", ");
    try self.writeAppSymbol(input);
    try self.writeAppText(", ");
    try self.writeU64(transform_offset);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(folded);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(folded);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.fold_items);
    try self.writeAppText("(");
    try self.writeAppSymbol(transformed);
    try self.writeAppText(", 0)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(pair_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(pair_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_pairs);
    try self.writeAppText("(");
    try self.writeAppSymbol(transformed);
    try self.writeAppText(", ");
    try self.writeAppSymbol(folded);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(try_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(try_score);
    try self.writeAppText(" = match ");
    try self.writeAppSymbol(self.symbols.try_item_score);
    try self.writeAppText("(");
    try self.writeAppSymbol(transformed);
    try self.writeAppText(", ");
    try self.writeU64(pick_index);
    try self.writeAppText(") {\n");
    try self.writeIndent(2);
    try self.writeAppText("Ok(");
    try self.writeAppSymbol(try_value);
    try self.writeAppText(") => ");
    try self.writeAppSymbol(try_value);
    try self.writeAppText("\n");
    try self.writeIndent(2);
    try self.writeAppText("Err(_) => 0\n");
    try self.writeIndent(1);
    try self.writeAppText("}\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(generic_num);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(generic_num);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.generic_choose);
    try self.writeAppText("(");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(try_score);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText("(");
    try self.writeAppSymbol(folded);
    try self.writeAppText("), ");
    try self.writeAppSymbol(pair_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(" : Str\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText("(");
    try self.writeAppSymbol(input);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(generic_item, self.symbols.item_type);
    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText("(");
    try self.writeAppSymbol(first);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(" : List(");
    try self.writeAppSymbol(self.symbols.item_type);
    try self.writeAppText(")\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText("(");
    try self.writeAppSymbol(transformed);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_iter);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_num);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(control_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(control_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_control);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(dict_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(dict_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_dict);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(control_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(list_ops_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(list_ops_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_list_ops);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(dict_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(str_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(str_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_str_ops);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(list_ops_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(try_ops_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(try_ops_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_try_ops);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(str_score);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(set_ops_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(set_ops_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_set_ops);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(try_ops_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(num_ops_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(num_ops_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_num_ops);
    try self.writeAppText("(");
    try self.writeAppSymbol(set_ops_score);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(imported_record, self.symbols.imported_type);
    try self.writeAppSymbol(self.symbols.imported_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.imported_make);
    try self.writeAppText("(");
    try self.writeAppSymbol(try_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pair_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(generic_num);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(control_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(dict_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(list_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(str_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(try_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(set_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(num_ops_score);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(alternate_imported, self.symbols.imported_type);
    try self.writeAppSymbol(self.symbols.imported_type);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.imported_make);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_num);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(generic_imported, self.symbols.imported_type);
    try self.writeAppSymbol(self.symbols.generic_choose);
    try self.writeAppText("(");
    try self.writeAppSymbol(self.symbols.is_even);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_num);
    try self.writeAppText("), ");
    try self.writeAppSymbol(self.symbols.generic_id);
    try self.writeAppText("(");
    try self.writeAppSymbol(imported_record);
    try self.writeAppText("), ");
    try self.writeAppSymbol(alternate_imported);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(imported_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(imported_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(generic_imported);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.imported_score);
    try self.writeAppText("(");
    try self.writeAppSymbol(folded);
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
    try self.writeAppSymbol(generic_item);
    try self.writeAppText(").");
    try self.writeAppSymbol(self.symbols.builder_add_many);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(")\n");

    try self.writeLocalHeader(state, self.symbols.state_type);
    try self.writeAppSymbol(builder);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.builder_build);
    try self.writeAppText("()\n");

    try self.writeLocalHeader(tree, self.symbols.tree_type);
    try self.writeTreeExpression(input, tree_depth);
    try self.writeAppText("\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_tree);
    try self.writeAppText("(");
    try self.writeAppSymbol(tree);
    try self.writeAppText(", 0)\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(pattern_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(pattern_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_patterns);
    try self.writeAppText("(");
    try self.writeAppSymbol(state);
    try self.writeAppText(", ");
    try self.writeAppSymbol(tree);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_imported);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(structural_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(structural_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_structural_keys);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(generic_text);
    try self.writeAppText(", ");
    try self.writeAppSymbol(pattern_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(boxed_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(boxed_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_boxed);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", ");
    try self.writeAppSymbol(tree);
    try self.writeAppText(", ");
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pattern_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(structural_score);
    try self.writeAppText(")\n");

    try self.writeIndent(1);
    try self.writeAppSymbol(captured_score);
    try self.writeAppText(" : U64\n");
    try self.writeIndent(1);
    try self.writeAppSymbol(captured_score);
    try self.writeAppText(" = ");
    try self.writeAppSymbol(self.symbols.score_with);
    try self.writeAppText("(");
    try self.writeAppSymbol(generic_items);
    try self.writeAppText(", |");
    try self.writeAppSymbol(captured_item);
    try self.writeAppText("| ");
    try self.writeAppSymbol(captured_item);
    try self.writeAppText(".");
    try self.writeAppSymbol(self.symbols.item_id);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(folded);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pair_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(try_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(control_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(dict_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(list_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(str_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(try_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(set_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(num_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(imported_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pattern_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(structural_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(boxed_score);
    try self.writeAppText(", 0)\n");

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
    try self.writeAppText(", 0) + ");
    try self.writeAppSymbol(folded);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pair_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(try_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(iter_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(control_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(dict_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(list_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(str_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(try_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(set_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(num_ops_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(imported_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(tree_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(pattern_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(structural_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(boxed_score);
    try self.writeAppText(" + ");
    try self.writeAppSymbol(captured_score);
    try self.writeAppText("\n");

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

fn writeTreeExpression(self: *Self, text_seed: Symbol, depth: u8) std.mem.Allocator.Error!void {
    if (depth == 0) {
        return self.writeTreeLeaf();
    }

    switch (self.reader.intRangeLessThan(u8, 0, 3)) {
        0 => try self.writeTreeLeaf(),
        1 => try self.writeTreeBranch(text_seed, depth - 1),
        else => try self.writeTreeNamed(text_seed, depth - 1),
    }
}

fn writeTreeLeaf(self: *Self) std.mem.Allocator.Error!void {
    try self.writeAppSymbol(self.symbols.tree_leaf);
    try self.writeAppText("(");
    try self.writeU64(self.reader.intRangeAtMost(u8, 0, 31));
    try self.writeAppText(")");
}

fn writeTreeBranch(self: *Self, text_seed: Symbol, depth: u8) std.mem.Allocator.Error!void {
    const child_count = self.reader.intRangeAtMost(u8, 1, 3);

    try self.writeAppSymbol(self.symbols.tree_branch);
    try self.writeAppText("([");
    for (0..child_count) |index| {
        if (index != 0) try self.writeAppText(", ");
        try self.writeTreeExpression(text_seed, depth);
    }
    try self.writeAppText("], ");
    try self.writeU64(self.reader.intRangeAtMost(u8, 0, 31));
    try self.writeAppText(")");
}

fn writeTreeNamed(self: *Self, text_seed: Symbol, depth: u8) std.mem.Allocator.Error!void {
    const child_count = self.reader.intRangeAtMost(u8, 1, 2);

    try self.writeAppSymbol(self.symbols.tree_named);
    try self.writeAppText("(Str.concat(");
    try self.writeAppSymbol(text_seed);
    try self.writeAppText(", ");
    try self.writeStringLiteral();
    try self.writeAppText("), [");
    for (0..child_count) |index| {
        if (index != 0) try self.writeAppText(", ");
        try self.writeTreeExpression(text_seed, depth);
    }
    try self.writeAppText("])");
}

fn writeStringLiteral(self: *Self) std.mem.Allocator.Error!void {
    const len = self.reader.intRangeAtMost(u8, 0, 4);
    try self.writeAsciiStringLiteral(len);
}

fn writeNonEmptyStringLiteral(self: *Self) std.mem.Allocator.Error!void {
    const len = self.reader.intRangeAtMost(u8, 1, 4);
    try self.writeAsciiStringLiteral(len);
}

fn writeAsciiStringLiteral(self: *Self, len: u8) std.mem.Allocator.Error!void {
    try self.writeAppText("\"");
    for (0..len) |_| {
        const byte = @as(u8, 'a') + self.reader.intRangeLessThan(u8, 0, 26);
        var buf = [_]u8{byte};
        try self.writeAppText(buf[0..]);
    }
    try self.writeAppText("\"");
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

fn writeModuleSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try writeSymbolTo(&self.module_output, self.allocator, symbol);
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
        .module_file => 'M',
        .package => 'p',
        .type => 'T',
        .type_var => 'a',
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

fn writeModuleText(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.module_output.appendSlice(self.allocator, text);
}
