//! Typed Roc type-module generator for fuzzers.
//!
//! Userspace Roc identifiers emitted by this generator must come from `fresh`.
//! Builtin type, tag, and function names are the only hardcoded Roc names here.
//!
//! A generated program is up to `max_modules` type modules. Each module may
//! import earlier modules and call their methods whose signatures mention no
//! module's own nominal types. The last module is the main module.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");

const Self = @This();

pub const max_modules = 3;
const max_methods = 512;
pub const max_method_arity = 2;
const max_local_bindings = 4;
const max_context_values = 64;
const max_type_errors = 3;
const max_top_level_values = 3;
/// How many leaf expressions may be nested inside one another through method
/// call arguments. A literal's components are leaves too, but literals nest
/// only as deep as their types do.
const max_leaf_nesting = 4;

/// A construct deliberately written so that checking rejects it.
pub const TypeError = union(enum) {
    /// An expression or pattern written at `written` where `expected` is required.
    wrong_type: struct { expected: Type, written: Type },
    /// A `.?` access of a field whose kind is required.
    optional_access_of_required_field,
    /// A plain `.` access of a field whose kind is optional.
    required_access_of_optional_field,
    /// An `x: _` unset of a field whose kind is required.
    unset_of_required_field,
    /// A top-level value whose right-hand side calls an effectful method.
    effectful_top_level_value,
    /// A module that imports itself.
    self_import,
    /// A module that imports a later module, which imports it back.
    import_cycle,

    /// Whether the error stops the modules it reaches from being checked at
    /// all, so a program that imports them has no checked app root.
    pub fn preventsChecking(self: TypeError) bool {
        return switch (self) {
            .self_import, .import_cycle => true,
            .wrong_type,
            .optional_access_of_required_field,
            .required_access_of_optional_field,
            .unset_of_required_field,
            .effectful_top_level_value,
            => false,
        };
    }
};

/// One construct deliberately written so that checking rejects it.
pub const TypeErrorSite = struct {
    module: u8,
    output_offset: usize,
    err: TypeError,
};

allocator: std.mem.Allocator,
reader: *FuzzReader,
outputs: [max_modules]std.ArrayList(u8),
file_names: [max_modules]?[]u8,
module_symbols: [max_modules]RootSymbols,
module_count: u8,
/// The module being generated.
current_module: u8,
/// The modules the current module imports, as a bit set.
current_imports: u8,
/// Imports each module must write because an earlier module imports it in
/// a deliberate cycle, as a bit set per module.
cycle_imports: [max_modules]u8,
/// The current module's root symbols.
symbols: RootSymbols,
/// The current module's top-level values visible to its methods and to its
/// later top-level values.
module_values: ExprContext,
next_id: u32,
/// Every generated method, grouped by module in generation order.
methods: [max_methods]Method,
method_count: usize,
/// Where the main module's methods start in `methods`.
main_methods_start: usize,
/// How many leaf expressions enclose the one being written.
leaf_nesting: u8,
/// How many constructs may still be written so that checking rejects them.
/// Zero (the default) generates only well-typed programs, and the budget
/// check consumes no input.
type_error_budget: u8,
/// How many constructs were written so that checking rejects them.
type_errors_written: u8,
/// Where each deliberately rejected construct starts in the output.
type_error_sites: [max_type_errors]TypeErrorSite,

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
    /// The optional field of `nominal_record` and `optional_record`.
    field3: Symbol,
    /// The nominal record type nested in the module's type.
    nominal_record: Symbol,
    /// The single-tag nominal tag union type nested in the module's type.
    wrapper: Symbol,
    wrapper_tag: Symbol,
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
    /// `{ field0 : Bool, field1 : U64, field3 ?: U64 }`, nested in the module's type.
    nominal_record,
    /// `[wrapper_tag(U64, Str)]`, nested in the module's type.
    wrapper,
    /// The structural record `{ field0 : Bool, field3 ?: U64 }`.
    optional_record,

    /// Whether the type is one of a module's own nominal types, which other
    /// modules cannot name.
    fn isModuleLocal(self: Type) bool {
        return switch (self) {
            .root, .nominal_record, .wrapper => true,
            .bool, .str, .u64, .record, .list_u64, .try_u64_str, .tuple_bool_u64, .optional_record => false,
        };
    }

    /// Whether a pattern can destructure every value of the type.
    fn isDestructurable(self: Type) bool {
        return switch (self) {
            .record, .tuple_bool_u64, .nominal_record, .wrapper => true,
            .root, .bool, .str, .u64, .list_u64, .try_u64_str, .optional_record => false,
        };
    }
};

pub const Method = struct {
    symbol: Symbol,
    module: u8,
    effectful: bool,
    param_types: [max_method_arity]Type,
    arity: u8,
    result_type: Type,
};

const ExprContext = struct {
    args: [max_context_values]Symbol,
    arg_types: [max_context_values]Type,
    arity: u8,
    /// Whether the expression may call effectful methods.
    effectful: bool,
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
    .nominal_record,
    .wrapper,
    .optional_record,
};

const destructurable_types = [_]Type{
    .record,
    .tuple_bool_u64,
    .nominal_record,
    .wrapper,
};

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .outputs = @splat(.empty),
        .file_names = @splat(null),
        .module_symbols = undefined,
        .module_count = 0,
        .current_module = 0,
        .current_imports = 0,
        .cycle_imports = @splat(0),
        .symbols = undefined,
        .module_values = emptyContext(),
        .next_id = 0,
        .methods = undefined,
        .method_count = 0,
        .main_methods_start = 0,
        .leaf_nesting = 0,
        .type_error_budget = 0,
        .type_errors_written = 0,
        .type_error_sites = undefined,
    };
}

pub fn deinit(self: *Self) void {
    for (&self.outputs) |*output| output.deinit(self.allocator);
    for (self.file_names) |file_name| {
        if (file_name) |name| self.allocator.free(name);
    }
}

pub fn moduleCount(self: *const Self) usize {
    return self.module_count;
}

/// The module the program's app imports; it imports the others, if at all.
pub fn mainModule(self: *const Self) usize {
    std.debug.assert(self.module_count > 0);
    return self.module_count - 1;
}

pub fn moduleOutput(self: *const Self, index: usize) []const u8 {
    return self.outputs[index].items;
}

pub fn moduleFileName(self: *const Self, index: usize) []const u8 {
    return self.file_names[index] orelse @panic("typed fuzz module file name requested before generation");
}

/// The main module's root symbols.
pub fn getRootSymbols(self: *const Self) RootSymbols {
    return self.module_symbols[self.mainModule()];
}

/// The main module's methods.
pub fn getMethods(self: *const Self) []const Method {
    return self.methods[self.main_methods_start..self.method_count];
}

pub fn freshSymbol(self: *Self, kind: SymbolKind) Symbol {
    return self.fresh(kind);
}

pub fn writeSymbolTo(output: *std.ArrayList(u8), allocator: std.mem.Allocator, symbol: Symbol) std.mem.Allocator.Error!void {
    try output.print(allocator, "{c}{d}", .{ symbolPrefix(symbol.kind), symbol.id });
}

/// Write `typ` as the module whose root symbols are `symbols` names it.
pub fn writeTypeTo(symbols: RootSymbols, output: *std.ArrayList(u8), allocator: std.mem.Allocator, typ: Type) std.mem.Allocator.Error!void {
    switch (typ) {
        .root => try writeSymbolTo(output, allocator, symbols.typ),
        .bool => try output.appendSlice(allocator, "Bool"),
        .str => try output.appendSlice(allocator, "Str"),
        .u64 => try output.appendSlice(allocator, "U64"),
        .record => {
            try output.appendSlice(allocator, "{ ");
            try writeSymbolTo(output, allocator, symbols.field0);
            try output.appendSlice(allocator, " : Bool, ");
            try writeSymbolTo(output, allocator, symbols.field1);
            try output.appendSlice(allocator, " : U64, ");
            try writeSymbolTo(output, allocator, symbols.field2);
            try output.appendSlice(allocator, " : Str }");
        },
        .list_u64 => try output.appendSlice(allocator, "List(U64)"),
        .try_u64_str => try output.appendSlice(allocator, "Try(U64, Str)"),
        .tuple_bool_u64 => try output.appendSlice(allocator, "(Bool, U64)"),
        .nominal_record => {
            try writeSymbolTo(output, allocator, symbols.typ);
            try output.appendSlice(allocator, ".");
            try writeSymbolTo(output, allocator, symbols.nominal_record);
        },
        .wrapper => {
            try writeSymbolTo(output, allocator, symbols.typ);
            try output.appendSlice(allocator, ".");
            try writeSymbolTo(output, allocator, symbols.wrapper);
        },
        .optional_record => {
            try output.appendSlice(allocator, "{ ");
            try writeSymbolTo(output, allocator, symbols.field0);
            try output.appendSlice(allocator, " : Bool, ");
            try writeSymbolTo(output, allocator, symbols.field3);
            try output.appendSlice(allocator, " ?: U64 }");
        },
    }
}

/// Allow up to `budget` constructs of the next generated program to be
/// written so that checking rejects them: at a type other than the one their
/// position requires, or as the rejected variant of a construct (a `.?` of a
/// required field, an unset of a required field, an effectful top-level
/// value, a self-import, an import cycle). Every construct the generator
/// composes then also reaches checking and lowering rejected.
pub fn allowTypeErrors(self: *Self, budget: u8) void {
    self.type_error_budget = @min(budget, max_type_errors);
}

/// The deliberately rejected constructs of the last generated program.
pub fn typeErrorSites(self: *const Self) []const TypeErrorSite {
    return self.type_error_sites[0..self.type_errors_written];
}

pub fn generateModules(self: *Self) std.mem.Allocator.Error!void {
    self.next_id = 0;
    self.method_count = 0;
    self.type_errors_written = 0;

    const field0 = self.fresh(.field);
    const field1 = self.fresh(.field);
    const field2 = self.fresh(.field);
    const field3 = self.fresh(.field);

    self.module_count = self.reader.intRangeAtMost(u8, 1, max_modules);
    for (0..self.module_count) |index| {
        const typ = self.fresh(.type);
        self.module_symbols[index] = .{
            .typ = typ,
            .tag0 = self.fresh(.tag),
            .tag1 = self.fresh(.tag),
            .tag2 = self.fresh(.tag),
            .tag3 = self.fresh(.tag),
            .field0 = field0,
            .field1 = field1,
            .field2 = field2,
            .field3 = field3,
            .nominal_record = self.fresh(.type),
            .wrapper = self.fresh(.type),
            .wrapper_tag = self.fresh(.tag),
        };
        self.outputs[index].clearRetainingCapacity();
        self.cycle_imports[index] = 0;
        try self.setFileName(index, typ);
    }

    for (0..self.module_count) |index| {
        try self.generateModule(@intCast(index));
    }
}

fn generateModule(self: *Self, index: u8) std.mem.Allocator.Error!void {
    const is_main = index + 1 == self.module_count;
    self.current_module = index;
    self.symbols = self.module_symbols[index];
    self.module_values = emptyContext();
    if (is_main) self.main_methods_start = self.method_count;

    try self.writeImports();
    try self.generateTopLevelValues();
    try self.writeRootTypeHeader();
    try self.writeNestedTypes();
    try self.generateSeedMethods();

    const method_count = if (is_main) self.reader.intRangeAtMost(u8, 48, 128) else self.reader.intRangeAtMost(u8, 8, 32);
    for (0..method_count) |_| {
        const arity = self.chooseMethodArity();
        var param_types: [max_method_arity]Type = undefined;
        for (0..arity) |param_index| {
            param_types[param_index] = self.chooseType();
        }
        const result_type = self.chooseType();
        const depth = self.reader.intRangeAtMost(u8, 2, 5);
        const effectful = self.reader.intRangeAtMost(u8, 0, 7) == 7;
        try self.generateMethod(param_types, arity, result_type, depth, effectful);
    }

    try self.write("}\n");
    try self.generateTopLevelValues();
}

fn setFileName(self: *Self, index: usize, symbol: Symbol) std.mem.Allocator.Error!void {
    if (self.file_names[index]) |name| self.allocator.free(name);
    self.file_names[index] = try std.fmt.allocPrint(self.allocator, "{c}{d}.roc", .{ symbolPrefix(symbol.kind), symbol.id });
}

/// Import earlier modules, whose methods the current module may then call:
/// some of them, or all of them for the main module, so the program reaches
/// every module. Also import every later module that imports this one in a
/// deliberate cycle. While the type-error budget lasts, a module occasionally
/// imports itself, or imports a later module that will import it back.
fn writeImports(self: *Self) std.mem.Allocator.Error!void {
    const index = self.current_module;
    const is_main = index + 1 == self.module_count;
    var imports = self.cycle_imports[index];
    for (0..index) |earlier| {
        if (is_main or self.reader.boolean()) imports |= moduleBit(earlier);
    }
    self.current_imports = imports;

    var wrote_import = false;
    for (0..index) |earlier| {
        if (imports & moduleBit(earlier) == 0) continue;
        try self.writeImport(earlier);
        wrote_import = true;
    }
    if (self.type_error_budget != 0 and self.reader.intRangeAtMost(u8, 0, 7) == 0) {
        self.recordTypeError(.self_import);
        try self.writeImport(index);
        wrote_import = true;
    }
    if (self.type_error_budget != 0 and index + 1 < self.module_count and self.reader.intRangeAtMost(u8, 0, 7) == 0) {
        const later = self.reader.intRangeLessThan(u8, index + 1, self.module_count);
        self.recordTypeError(.import_cycle);
        try self.writeImport(later);
        self.cycle_imports[later] |= moduleBit(index);
        wrote_import = true;
    }
    if (wrote_import) try self.write("\n");
}

fn writeImport(self: *Self, module: usize) std.mem.Allocator.Error!void {
    try self.write("import ");
    try self.writeSymbol(self.module_symbols[module].typ);
    try self.write("\n");
}

fn moduleBit(module: usize) u8 {
    return @as(u8, 1) << @intCast(module);
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

fn writeNestedTypes(self: *Self) std.mem.Allocator.Error!void {
    try self.writeIndent(1);
    try self.writeSymbol(self.symbols.nominal_record);
    try self.write(" := { ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(" : Bool, ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(" : U64, ");
    try self.writeSymbol(self.symbols.field3);
    try self.write(" ?: U64 }\n");
    try self.writeIndent(1);
    try self.writeSymbol(self.symbols.wrapper);
    try self.write(" := [");
    try self.writeSymbol(self.symbols.wrapper_tag);
    try self.write("(U64, Str)]\n");
}

/// Top-level values outside the module's type. Those before the type are
/// visible to its methods; those after it can call every method.
fn generateTopLevelValues(self: *Self) std.mem.Allocator.Error!void {
    const count = self.reader.intRangeAtMost(u8, 0, max_top_level_values);
    for (0..count) |_| try self.generateTopLevelValue();
}

/// One top-level value: an annotated name, or a destructure of a typed
/// source. While the type-error budget lasts, its right-hand side
/// occasionally calls an effectful method.
fn generateTopLevelValue(self: *Self) std.mem.Allocator.Error!void {
    const visible_methods = self.method_count;
    if (self.type_error_budget != 0 and self.reader.intRangeAtMost(u8, 0, 7) == 0) {
        if (self.chooseEffectfulMethod(visible_methods)) |method| {
            const context = self.module_values;
            self.recordTypeError(.effectful_top_level_value);
            try self.writeTopLevelPattern(method.result_type);
            try self.write(" = ");
            try self.writeAssociatedCall(method, context, 1, visible_methods);
            try self.write("\n");
            return;
        }
    }

    const typ = self.chooseType();
    if (!typ.isDestructurable() or self.reader.boolean()) {
        try self.writeAnnotatedTopLevelValue(typ, visible_methods);
        return;
    }

    const source_type = self.chooseWrittenType(typ);
    if (!contextHasSymbol(source_type, self.module_values) and !self.hasMethod(source_type, false, visible_methods)) {
        try self.writeAnnotatedTopLevelValue(source_type, visible_methods);
    }
    const context = self.module_values;
    self.module_values = try self.writeDestructurePattern(typ, context);
    try self.write(" = ");
    try self.writeTypedSource(source_type, context, visible_methods);
    try self.write("\n");
}

fn writeAnnotatedTopLevelValue(self: *Self, typ: Type, visible_methods: usize) std.mem.Allocator.Error!void {
    const value = self.fresh(.value);
    try self.writeSymbol(value);
    try self.write(" : ");
    try self.writeType(typ);
    try self.write("\n");
    try self.writeSymbol(value);
    try self.write(" = ");
    try self.writeExpr(typ, self.module_values, self.reader.intRangeAtMost(u8, 0, 3), visible_methods);
    try self.write("\n");
    self.module_values = extendContext(self.module_values, value, typ);
}

/// The pattern of a top-level value of type `typ` that has no annotation: a
/// plain name, or a destructure.
fn writeTopLevelPattern(self: *Self, typ: Type) std.mem.Allocator.Error!void {
    if (typ.isDestructurable() and self.reader.boolean()) {
        self.module_values = try self.writeDestructurePattern(typ, self.module_values);
        return;
    }
    const value = self.fresh(.value);
    try self.writeSymbol(value);
    self.module_values = extendContext(self.module_values, value, typ);
}

/// A value of type `typ` whose type its own annotation fixes: a name in
/// `context`, or a call of a pure method. One of the two must exist.
fn writeTypedSource(self: *Self, typ: Type, context: ExprContext, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.contextSymbol(typ, context)) |value| {
        if (self.reader.boolean() or !self.hasMethod(typ, false, visible_methods)) {
            try self.writeSymbol(value);
            return;
        }
    }
    const method = self.chooseMethod(typ, false, visible_methods).?;
    try self.writeAssociatedCall(method, pureContext(context), 1, visible_methods);
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
    try self.generateMethod(param_types, 1, result_type, depth, false);
}

fn generateMethod(self: *Self, param_types: [max_method_arity]Type, arity: u8, result_type: Type, depth: u8, effectful: bool) std.mem.Allocator.Error!void {
    if (self.method_count >= self.methods.len) return;

    const method: Method = .{
        .symbol = self.fresh(.function),
        .module = self.current_module,
        .effectful = effectful,
        .param_types = param_types,
        .arity = arity,
        .result_type = result_type,
    };
    const visible_methods = self.method_count;

    try self.writeIndent(1);
    try self.writeMethodSymbol(method);
    try self.write(" : ");
    for (0..arity) |index| {
        if (index > 0) try self.write(", ");
        try self.writeType(param_types[index]);
    }
    try self.write(if (effectful) " => " else " -> ");
    try self.writeType(result_type);
    try self.write("\n");
    try self.writeIndent(1);
    try self.writeMethodSymbol(method);
    try self.write(" = |");
    var context = self.module_values;
    context.effectful = effectful;
    for (0..arity) |index| {
        if (index > 0) try self.write(", ");
        context = try self.writeParamPattern(param_types[index], context);
    }
    try self.write("| ");
    try self.writeMethodBody(result_type, context, depth, visible_methods);
    try self.write("\n");

    self.methods[self.method_count] = method;
    self.method_count += 1;
}

fn writeExpr(self: *Self, expected: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const typ = self.chooseWrittenType(expected);
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
        .nominal_record => try self.writeNominalRecordExpr(context, depth, visible_methods),
        .wrapper => try self.writeWrapperExpr(context, depth, visible_methods),
        .optional_record => try self.writeOptionalRecordExpr(context, depth, visible_methods),
    }
}

/// A name, a method call whose arguments are leaves, or a literal. Method
/// calls stop past `max_leaf_nesting` nested leaves, so a leaf is finite.
fn writeLeafExpr(self: *Self, typ: Type, context: ExprContext, visible_methods: usize) std.mem.Allocator.Error!void {
    self.leaf_nesting += 1;
    defer self.leaf_nesting -= 1;
    if (self.contextSymbol(typ, context)) |arg| {
        if (self.reader.boolean()) {
            try self.writeSymbol(arg);
            return;
        }
    }
    if (self.leaf_nesting <= max_leaf_nesting and self.reader.boolean() and try self.writeMethodCall(typ, context, 0, visible_methods)) return;
    try self.writeLiteral(typ);
}

/// Write the pattern for one parameter of type `param_type` and return
/// `context` extended with the names it binds. A parameter of a record,
/// tuple, nominal record, or single-tag nominal union may be destructured
/// into its components; while the type-error budget lasts, a parameter is
/// occasionally destructured as a different one of those types.
fn writeParamPattern(self: *Self, param_type: Type, context: ExprContext) std.mem.Allocator.Error!ExprContext {
    if (self.type_error_budget != 0 and self.reader.intRangeAtMost(u8, 0, 15) == 0) {
        const written = self.chooseOtherDestructurableType(param_type);
        self.recordTypeError(.{ .wrong_type = .{ .expected = param_type, .written = written } });
        return try self.writeDestructurePattern(written, context);
    }
    if (param_type.isDestructurable() and self.reader.boolean()) {
        return try self.writeDestructurePattern(param_type, context);
    }
    const arg = self.fresh(.value);
    try self.writeSymbol(arg);
    return extendContext(context, arg, param_type);
}

fn chooseOtherDestructurableType(self: *Self, typ: Type) Type {
    const index = std.mem.findScalar(Type, &destructurable_types, typ) orelse
        return destructurable_types[self.reader.intRangeLessThan(usize, 0, destructurable_types.len)];
    const offset = self.reader.intRangeAtMost(usize, 1, destructurable_types.len - 1);
    return destructurable_types[(index + offset) % destructurable_types.len];
}

fn writeDestructurePattern(self: *Self, typ: Type, context: ExprContext) std.mem.Allocator.Error!ExprContext {
    switch (typ) {
        .record => {
            const bool_part = self.fresh(.value);
            const u64_part = self.fresh(.value);
            const str_part = self.fresh(.value);
            try self.write("{ ");
            try self.writeSymbol(self.symbols.field0);
            try self.write(": ");
            try self.writeSymbol(bool_part);
            try self.write(", ");
            try self.writeSymbol(self.symbols.field1);
            try self.write(": ");
            try self.writeSymbol(u64_part);
            try self.write(", ");
            try self.writeSymbol(self.symbols.field2);
            try self.write(": ");
            try self.writeSymbol(str_part);
            try self.write(" }");
            return extendContext(extendContext(extendContext(context, bool_part, .bool), u64_part, .u64), str_part, .str);
        },
        .tuple_bool_u64 => {
            const bool_part = self.fresh(.value);
            const u64_part = self.fresh(.value);
            try self.write("(");
            try self.writeSymbol(bool_part);
            try self.write(", ");
            try self.writeSymbol(u64_part);
            try self.write(")");
            return extendContext(extendContext(context, bool_part, .bool), u64_part, .u64);
        },
        .nominal_record => {
            const bool_part = self.fresh(.value);
            const u64_part = self.fresh(.value);
            try self.writeType(.nominal_record);
            try self.write(".{ ");
            try self.writeSymbol(self.symbols.field0);
            try self.write(": ");
            try self.writeSymbol(bool_part);
            try self.write(", ");
            try self.writeSymbol(self.symbols.field1);
            try self.write(": ");
            try self.writeSymbol(u64_part);
            try self.write(", ");
            try self.writeSymbol(self.symbols.field3);
            try self.write(": _ }");
            return extendContext(extendContext(context, bool_part, .bool), u64_part, .u64);
        },
        .wrapper => {
            const u64_part = self.fresh(.value);
            const str_part = self.fresh(.value);
            try self.writeWrapperTag();
            try self.write("(");
            try self.writeSymbol(u64_part);
            try self.write(", ");
            try self.writeSymbol(str_part);
            try self.write(")");
            return extendContext(extendContext(context, u64_part, .u64), str_part, .str);
        },
        .root, .bool, .str, .u64, .list_u64, .try_u64_str, .optional_record => unreachable,
    }
}

fn writeMethodBody(self: *Self, result_type: Type, params: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    var context = params;
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
    return .{ .args = undefined, .arg_types = undefined, .arity = 0, .effectful = false };
}

/// `context` inside a lambda the generator writes, which is pure even in an
/// effectful method.
fn pureContext(context: ExprContext) ExprContext {
    var pure = context;
    pure.effectful = false;
    return pure;
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
    switch (self.reader.intRangeAtMost(u8, 0, 6)) {
        0 => try self.writeContextSymbolOrElse(.root, context, writeRootLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.root, context, depth - 1, visible_methods)) try self.writeRootLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.root, context, depth - 1, visible_methods),
        3 => try self.writeRootLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.root, context, depth - 1, visible_methods),
        5 => try self.writeRecordMatchExpr(.root, context, depth - 1, visible_methods),
        6 => try self.writeWrapperMatchExpr(.root, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeBoolExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 11)) {
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
        10 => try self.writeField0Expr(context, depth - 1, visible_methods),
        11 => try self.writeWrapperMatchExpr(.bool, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeStrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 10)) {
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
        10 => try self.writeWrapperMatchExpr(.str, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeU64Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 14)) {
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
        12 => {
            try self.writeParenthesizedExpr(.nominal_record, context, depth - 1, visible_methods);
            try self.write(".");
            try self.writeSymbol(self.symbols.field1);
        },
        13 => try self.writeOptionalFieldExpr(context, depth - 1, visible_methods),
        14 => try self.writeWrapperMatchExpr(.u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeRecordExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => try self.writeContextSymbolOrElse(.record, context, writeRecordLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.record, context, depth - 1, visible_methods)) try self.writeRecordLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.record, context, depth - 1, visible_methods),
        3 => try self.writeRecordLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRecordUpdateExpr(context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.record, context, depth - 1, visible_methods),
        6 => try self.writeRecordMatchExpr(.record, context, depth - 1, visible_methods),
        7 => try self.writeWrapperMatchExpr(.record, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeListU64Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 10)) {
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
        10 => try self.writeWrapperMatchExpr(.list_u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeTryExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 9)) {
        0 => try self.writeContextSymbolOrElse(.try_u64_str, context, writeTryLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.try_u64_str, context, depth - 1, visible_methods)) try self.writeTryLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.try_u64_str, context, depth - 1, visible_methods),
        3 => try self.writeTryLiteral(context, depth - 1, visible_methods),
        4 => try self.writeTryMatchExpr(context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.try_u64_str, context, depth - 1, visible_methods),
        6 => try self.writeTryMapOkExpr(context, depth - 1, visible_methods),
        7 => try self.writeTryMapErrExpr(context, depth - 1, visible_methods),
        8 => try self.writeRecordMatchExpr(.try_u64_str, context, depth - 1, visible_methods),
        9 => try self.writeWrapperMatchExpr(.try_u64_str, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeTupleExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 8)) {
        0 => try self.writeContextSymbolOrElse(.tuple_bool_u64, context, writeTupleLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.tuple_bool_u64, context, depth - 1, visible_methods)) try self.writeTupleLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        3 => try self.writeTupleLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        5 => try self.writeTupleProjectionExpr(context, depth - 1, visible_methods),
        6 => try self.writeTupleMatchExpr(context, depth - 1, visible_methods),
        7 => try self.writeRecordMatchExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        8 => try self.writeWrapperMatchExpr(.tuple_bool_u64, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeNominalRecordExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => try self.writeContextSymbolOrElse(.nominal_record, context, writeNominalRecordLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.nominal_record, context, depth - 1, visible_methods)) try self.writeNominalRecordLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.nominal_record, context, depth - 1, visible_methods),
        3 => try self.writeNominalRecordLiteral(context, depth - 1, visible_methods),
        4 => try self.writeOptionalFieldUpdateExpr(.nominal_record, context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.nominal_record, context, depth - 1, visible_methods),
        6 => try self.writeRecordMatchExpr(.nominal_record, context, depth - 1, visible_methods),
        7 => try self.writeWrapperMatchExpr(.nominal_record, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeWrapperExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 6)) {
        0 => try self.writeContextSymbolOrElse(.wrapper, context, writeWrapperLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.wrapper, context, depth - 1, visible_methods)) try self.writeWrapperLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.wrapper, context, depth - 1, visible_methods),
        3 => try self.writeWrapperLiteral(context, depth - 1, visible_methods),
        4 => try self.writeRootMatchExpr(.wrapper, context, depth - 1, visible_methods),
        5 => try self.writeRecordMatchExpr(.wrapper, context, depth - 1, visible_methods),
        6 => try self.writeWrapperMatchExpr(.wrapper, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeOptionalRecordExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    switch (self.reader.intRangeAtMost(u8, 0, 7)) {
        0 => try self.writeContextSymbolOrElse(.optional_record, context, writeOptionalRecordLiteral, depth - 1, visible_methods),
        1 => if (!try self.writeMethodCall(.optional_record, context, depth - 1, visible_methods)) try self.writeOptionalRecordLiteral(context, depth - 1, visible_methods),
        2 => try self.writeIfExpr(.optional_record, context, depth - 1, visible_methods),
        3 => try self.writeOptionalRecordLiteral(context, depth - 1, visible_methods),
        4 => try self.writeOptionalFieldUpdateExpr(.optional_record, context, depth - 1, visible_methods),
        5 => try self.writeRootMatchExpr(.optional_record, context, depth - 1, visible_methods),
        6 => try self.writeRecordMatchExpr(.optional_record, context, depth - 1, visible_methods),
        7 => try self.writeWrapperMatchExpr(.optional_record, context, depth - 1, visible_methods),
        else => unreachable,
    }
}

fn writeMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    if (self.reader.boolean() and try self.writeReceiverMethodCall(result_type, context, depth, visible_methods)) return true;
    return self.writeAssociatedMethodCall(result_type, context, depth, visible_methods);
}

fn writeAssociatedMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    const method = self.chooseMethod(result_type, context.effectful, visible_methods) orelse return false;
    try self.writeAssociatedCall(method, context, depth, visible_methods);
    return true;
}

fn writeAssociatedCall(self: *Self, method: Method, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.writeSymbol(self.module_symbols[method.module].typ);
    try self.write(".");
    try self.writeMethodSymbol(method);
    try self.write("(");
    for (0..method.arity) |index| {
        if (index > 0) try self.write(", ");
        try self.writeExpr(method.param_types[index], context, depth, visible_methods);
    }
    try self.write(")");
}

fn writeReceiverMethodCall(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!bool {
    const method = self.chooseMethodMatching(.{ .result_type = result_type, .effectful = context.effectful, .root_receiver = true }, visible_methods) orelse return false;

    if (!try self.writeRootReceiver(context, depth, visible_methods)) return false;
    try self.write(".");
    try self.writeMethodSymbol(method);
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

/// Which of the methods generated so far an expression may call.
const MethodQuery = struct {
    /// Only methods returning this type, when set.
    result_type: ?Type = null,
    /// Whether effectful methods may be called.
    effectful: bool,
    /// Only effectful methods.
    only_effectful: bool = false,
    /// Only methods whose first parameter is the current module's type.
    root_receiver: bool = false,
    /// Only methods whose one parameter is the current module's type.
    root_unary: bool = false,
};

fn methodMatches(self: *const Self, method: Method, query: MethodQuery) bool {
    if (method.effectful and !query.effectful) return false;
    if (query.only_effectful and !method.effectful) return false;
    if (query.result_type) |result_type| {
        if (method.result_type != result_type) return false;
    }
    if (query.root_receiver and method.param_types[0] != .root) return false;
    if (query.root_unary and (method.arity != 1 or method.param_types[0] != .root)) return false;
    if (method.module == self.current_module) return true;
    // Another module's method is callable through an import, as long as its
    // signature mentions none of that module's own types.
    if (self.current_imports & moduleBit(method.module) == 0) return false;
    if (method.result_type.isModuleLocal()) return false;
    for (method.param_types[0..method.arity]) |param_type| {
        if (param_type.isModuleLocal()) return false;
    }
    return true;
}

fn countMethodsMatching(self: *const Self, query: MethodQuery, visible_methods: usize) usize {
    var count: usize = 0;
    for (self.methods[0..visible_methods]) |method| {
        if (self.methodMatches(method, query)) count += 1;
    }
    return count;
}

fn chooseMethodMatching(self: *Self, query: MethodQuery, visible_methods: usize) ?Method {
    const count = self.countMethodsMatching(query, visible_methods);
    if (count == 0) return null;

    var index = self.reader.intRangeLessThan(usize, 0, count);
    for (self.methods[0..visible_methods]) |method| {
        if (!self.methodMatches(method, query)) continue;
        if (index == 0) return method;
        index -= 1;
    }
    unreachable;
}

fn chooseMethod(self: *Self, result_type: Type, effectful: bool, visible_methods: usize) ?Method {
    return self.chooseMethodMatching(.{ .result_type = result_type, .effectful = effectful }, visible_methods);
}

fn hasMethod(self: *const Self, result_type: Type, effectful: bool, visible_methods: usize) bool {
    return self.countMethodsMatching(.{ .result_type = result_type, .effectful = effectful }, visible_methods) != 0;
}

fn chooseEffectfulMethod(self: *Self, visible_methods: usize) ?Method {
    return self.chooseMethodMatching(.{ .effectful = true, .only_effectful = true }, visible_methods);
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
    // A bare tag here would be a structural tag union, since nothing around
    // a scrutinee says which type it has; the module's type qualifies it.
    try self.writeSymbol(self.symbols.typ);
    try self.write(".");
    try self.writeRootLiteral(context, depth, visible_methods);
}

fn writeListMapExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const item = self.fresh(.value);

    try self.writeParenthesizedExpr(.list_u64, context, depth, visible_methods);
    try self.write(".map(|");
    try self.writeSymbol(item);
    try self.write("| ");
    try self.writeExpr(.u64, extendContext(pureContext(context), item, .u64), depth, visible_methods);
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
    try self.writeExpr(.bool, extendContext(pureContext(context), item, .u64), depth, visible_methods);
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
    try self.writeRequiredFieldValue(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeRequiredFieldValue(.u64, context, depth, visible_methods);
    try self.write(" }");
}

/// An update of a nominal record or an optional record that sets or unsets
/// the optional field, and sometimes also sets the `Bool` field.
fn writeOptionalFieldUpdateExpr(self: *Self, typ: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("{ ..(");
    try self.writeExpr(typ, context, depth, visible_methods);
    try self.write(")");
    if (self.reader.boolean()) {
        try self.write(", ");
        try self.writeSymbol(self.symbols.field0);
        try self.write(": ");
        try self.writeRequiredFieldValue(.bool, context, depth, visible_methods);
    }
    try self.write(", ");
    try self.writeSymbol(self.symbols.field3);
    try self.write(": ");
    try self.writeOptionalFieldValue(context, depth, visible_methods);
    try self.write(" }");
}

/// The value of a required field in a record literal or update. While the
/// type-error budget lasts, the field is occasionally unset instead.
fn writeRequiredFieldValue(self: *Self, typ: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.type_error_budget != 0 and self.reader.intRangeAtMost(u8, 0, 15) == 0) {
        self.recordTypeError(.unset_of_required_field);
        try self.write("_");
        return;
    }
    try self.writeExpr(typ, context, depth, visible_methods);
}

/// The value of the optional `U64` field in a record literal or update:
/// a value, or `_` to unset it.
fn writeOptionalFieldValue(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("_");
        return;
    }
    try self.writeExpr(.u64, context, depth, visible_methods);
}

/// A read of the `Bool` field that the record, the nominal record, and the
/// optional record all have.
fn writeField0Expr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const sources = [_]Type{ .record, .nominal_record, .optional_record };
    try self.writeParenthesizedExpr(sources[self.reader.intRangeLessThan(usize, 0, sources.len)], context, depth, visible_methods);
    try self.write(".");
    try self.writeSymbol(self.symbols.field0);
}

/// A `.?` access of the optional field of a nominal record or an optional
/// record, with `??` supplying a value when the field is missing. While the
/// type-error budget lasts, the access occasionally uses `.?` on a required
/// field, or a plain `.` on the optional field.
fn writeOptionalFieldExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const optional_sources = [_]Type{ .nominal_record, .optional_record };
    if (self.type_error_budget != 0 and self.reader.intRangeAtMost(u8, 0, 7) == 0) {
        if (self.reader.boolean()) {
            self.recordTypeError(.optional_access_of_required_field);
            const required_sources = [_]Type{ .record, .nominal_record };
            try self.write("(");
            try self.writeParenthesizedExpr(required_sources[self.reader.intRangeLessThan(usize, 0, required_sources.len)], context, depth, visible_methods);
            try self.write(".?");
            try self.writeSymbol(self.symbols.field1);
            try self.write(" ?? ");
            try self.writeParenthesizedExpr(.u64, context, depth, visible_methods);
            try self.write(")");
        } else {
            self.recordTypeError(.required_access_of_optional_field);
            try self.writeParenthesizedExpr(optional_sources[self.reader.intRangeLessThan(usize, 0, optional_sources.len)], context, depth, visible_methods);
            try self.write(".");
            try self.writeSymbol(self.symbols.field3);
        }
        return;
    }
    try self.write("(");
    try self.writeParenthesizedExpr(optional_sources[self.reader.intRangeLessThan(usize, 0, optional_sources.len)], context, depth, visible_methods);
    try self.write(".?");
    try self.writeSymbol(self.symbols.field3);
    try self.write(" ?? ");
    try self.writeParenthesizedExpr(.u64, context, depth, visible_methods);
    try self.write(")");
}

fn writeWrapperMatchExpr(self: *Self, result_type: Type, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const u64_payload = self.fresh(.value);
    const str_payload = self.fresh(.value);
    const branch_context = extendContext(extendContext(context, u64_payload, .u64), str_payload, .str);

    try self.write("match ");
    try self.writeExpr(.wrapper, context, depth, visible_methods);
    try self.write(" {\n");
    try self.writeIndent(2);
    try self.writeWrapperTag();
    try self.write("(");
    try self.writeSymbol(u64_payload);
    try self.write(", ");
    try self.writeSymbol(str_payload);
    try self.write(") => ");
    try self.writeExpr(result_type, branch_context, depth, visible_methods);
    try self.write("\n");
    try self.writeIndent(1);
    try self.write("}");
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
    try self.writeExpr(.u64, extendContext(pureContext(context), ok_payload, .u64), depth, visible_methods);
    try self.write(")");
}

fn writeTryMapErrExpr(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    const err_payload = self.fresh(.value);

    try self.write("Try.map_err(");
    try self.writeExpr(.try_u64_str, context, depth, visible_methods);
    try self.write(", |");
    try self.writeSymbol(err_payload);
    try self.write("| ");
    try self.writeExpr(.str, extendContext(pureContext(context), err_payload, .str), depth, visible_methods);
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

    if (self.chooseMethodMatching(rootUnaryQuery(context), visible_methods)) |method| {
        try self.writeSymbol(self.symbols.typ);
        try self.write(".");
        try self.writeMethodSymbol(method);
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
    return contextHasSymbol(.str, context) or self.countMethodsMatching(rootUnaryQuery(context), visible_methods) != 0;
}

fn rootUnaryQuery(context: ExprContext) MethodQuery {
    return .{ .result_type = .str, .effectful = context.effectful, .root_unary = true };
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
        .nominal_record => try self.writeNominalRecordLiteral(emptyContext(), 0, self.method_count),
        .wrapper => try self.writeWrapperLiteral(emptyContext(), 0, self.method_count),
        .optional_record => try self.writeOptionalRecordLiteral(emptyContext(), 0, self.method_count),
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
    try self.writeRequiredFieldValue(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeRequiredFieldValue(.u64, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field2);
    try self.write(": ");
    try self.writeRequiredFieldValue(.str, context, depth, visible_methods);
    try self.write(" }");
}

/// A nominal record literal, which supplies, omits, or unsets its optional
/// field.
fn writeNominalRecordLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.writeType(.nominal_record);
    try self.write(".{ ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeRequiredFieldValue(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field1);
    try self.write(": ");
    try self.writeRequiredFieldValue(.u64, context, depth, visible_methods);
    if (self.reader.boolean()) {
        try self.write(", ");
        try self.writeSymbol(self.symbols.field3);
        try self.write(": ");
        try self.writeOptionalFieldValue(context, depth, visible_methods);
    }
    try self.write(" }");
}

/// An optional record literal, which supplies or unsets its optional field.
/// It never omits it: a structural literal only has the fields it names, and
/// its position does not always fix its type.
fn writeOptionalRecordLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.write("{ ");
    try self.writeSymbol(self.symbols.field0);
    try self.write(": ");
    try self.writeRequiredFieldValue(.bool, context, depth, visible_methods);
    try self.write(", ");
    try self.writeSymbol(self.symbols.field3);
    try self.write(": ");
    try self.writeOptionalFieldValue(context, depth, visible_methods);
    try self.write(" }");
}

fn writeWrapperLiteral(self: *Self, context: ExprContext, depth: u8, visible_methods: usize) std.mem.Allocator.Error!void {
    try self.writeWrapperTag();
    try self.write("(");
    try self.writeExpr(.u64, context, depth, visible_methods);
    try self.write(", ");
    try self.writeExpr(.str, context, depth, visible_methods);
    try self.write(")");
}

fn writeWrapperTag(self: *Self) std.mem.Allocator.Error!void {
    try self.writeType(.wrapper);
    try self.write(".");
    try self.writeSymbol(self.symbols.wrapper_tag);
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
    try writeTypeTo(self.symbols, self.currentOutput(), self.allocator, typ);
}

/// The type an expression required at `expected` is actually written at:
/// `expected` itself, or, while the type-error budget lasts, occasionally a
/// different type.
fn chooseWrittenType(self: *Self, expected: Type) Type {
    if (self.type_error_budget == 0) return expected;
    if (self.reader.intRangeAtMost(u8, 0, 15) != 0) return expected;
    const offset = self.reader.intRangeAtMost(usize, 1, all_types.len - 1);
    const expected_index = std.mem.findScalar(Type, &all_types, expected).?;
    const written = all_types[(expected_index + offset) % all_types.len];
    self.recordTypeError(.{ .wrong_type = .{ .expected = expected, .written = written } });
    return written;
}

fn recordTypeError(self: *Self, err: TypeError) void {
    std.debug.assert(self.type_error_budget != 0);
    self.type_error_sites[self.type_errors_written] = .{
        .module = self.current_module,
        .output_offset = self.currentOutput().items.len,
        .err = err,
    };
    self.type_error_budget -= 1;
    self.type_errors_written += 1;
}

fn chooseType(self: *Self) Type {
    return all_types[self.reader.intRangeLessThan(usize, 0, all_types.len)];
}

fn writeBoolLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.write(if (self.reader.boolean()) "True" else "False");
}

fn writeU64Literal(self: *Self) std.mem.Allocator.Error!void {
    const value = self.reader.intRangeAtMost(u8, 0, 100);
    try self.currentOutput().print(self.allocator, "{d}", .{value});
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
        try self.currentOutput().append(self.allocator, char);
    }
}

fn fresh(self: *Self, kind: SymbolKind) Symbol {
    const symbol: Symbol = .{ .kind = kind, .id = self.next_id };
    self.next_id += 1;
    return symbol;
}

fn writeSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try writeSymbolTo(self.currentOutput(), self.allocator, symbol);
}

fn writeMethodSymbol(self: *Self, method: Method) std.mem.Allocator.Error!void {
    try self.writeSymbol(method.symbol);
    if (method.effectful) try self.write("!");
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
    try self.currentOutput().appendSlice(self.allocator, text);
}

/// The current module's output.
fn currentOutput(self: *Self) *std.ArrayList(u8) {
    return &self.outputs[self.current_module];
}
