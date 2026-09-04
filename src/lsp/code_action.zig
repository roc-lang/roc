//! Values used to fill in a generated `expect` test.
//!
//! A generated test calls the function it was asked for, so it needs a value
//! for every parameter and one to compare the result against. Those values are
//! read off the checked types rather than guessed from the names.
//!
//! Only a type with an obvious literal gets one. Anything else - a nominal type
//! the author declared, a tag union, a function, a type variable - makes the
//! whole test unavailable instead of generating source that does not compile.

const std = @import("std");
const can = @import("can");
const types = @import("types");
const type_utils = @import("type_utils.zig");

const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Allocator = std.mem.Allocator;

/// How many alias layers to follow before giving up on a type.
const max_alias_depth: usize = 16;

/// How deeply a placeholder may nest.
///
/// A record holds types that hold records, and a recursive type has no bottom,
/// so the nesting is bounded rather than trusted.
const max_nesting: usize = 4;

/// Why a placeholder could not be written.
const PlaceholderError = Allocator.Error || error{
    /// The type has no literal value this knows how to write.
    UnsupportedType,
};

/// The prefix every builtin type's qualified name carries.
const builtin_module_prefix = "Builtin.";

/// Whether `name` is one of the spellings the compiler carries `spec` under.
///
/// A nominal type's ident holds whichever spelling reached it, and the three
/// are all in use at once: `Builtin.Str` came off an annotation in the same
/// file that gave `List` its bare name.
fn specHasName(spec: CIR.BuiltinTypeSpec, name: []const u8) bool {
    std.debug.assert(std.mem.startsWith(u8, spec.qualified_name, builtin_module_prefix));
    if (std.mem.eql(u8, name, spec.qualified_name)) return true;
    if (std.mem.eql(u8, name, spec.qualified_name[builtin_module_prefix.len..])) return true;
    return std.mem.eql(u8, name, spec.display_name);
}

/// The literal to write for a builtin type, or null when it has none.
///
/// The types are looked up in the compiler's own builtin registry rather than
/// in a list kept here, so a builtin that is renamed or removed cannot leave a
/// stale entry behind.
fn builtinPlaceholder(name: []const u8) ?[]const u8 {
    for (CIR.builtin_type_specs) |spec| {
        if (!specHasName(spec, name)) continue;

        // Every fixed-width number takes a `0`. The SIMD vector types sit in
        // the same registry without a `num_kind`, and `0` is not one of those.
        if (spec.num_kind != null) return "0";
        if (std.mem.eql(u8, spec.qualified_name, "Builtin.Str")) return "\"\"";
        if (std.mem.eql(u8, spec.qualified_name, "Builtin.Bool")) return "Bool.True";
        if (std.mem.eql(u8, spec.qualified_name, "Builtin.List")) return "[]";
        return null;
    }
    return null;
}

/// Write a literal value of `type_var` into `out`.
fn writePlaceholder(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    type_var: types.Var,
    out: *std.ArrayList(u8),
) PlaceholderError!void {
    return writeNestedPlaceholder(allocator, module_env, type_var, out, 0);
}

/// Write a literal value, refusing once the nesting runs too deep.
fn writeNestedPlaceholder(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    type_var: types.Var,
    out: *std.ArrayList(u8),
    depth: usize,
) PlaceholderError!void {
    if (depth > max_nesting) return error.UnsupportedType;

    const content = type_utils.unwrapAliases(&module_env.types, type_var, max_alias_depth).content;
    switch (content) {
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| {
                // A nominal type the author declared is built by its own
                // constructor, which this cannot know.
                if (!nominal.originIsBuiltin()) return error.UnsupportedType;

                const name = module_env.common.idents.getText(nominal.ident.ident_idx);
                const literal = builtinPlaceholder(name) orelse return error.UnsupportedType;
                try out.appendSlice(allocator, literal);
            },
            .empty_record => try out.appendSlice(allocator, "{}"),
            .record => |record| try writeRecordPlaceholder(
                allocator,
                module_env,
                module_env.types.getRecordFieldsSlice(record.fields),
                out,
                depth,
            ),
            .record_unbound => |fields| try writeRecordPlaceholder(
                allocator,
                module_env,
                module_env.types.getRecordFieldsSlice(fields),
                out,
                depth,
            ),
            .tuple => |tuple| {
                try out.appendSlice(allocator, "(");
                for (module_env.types.sliceVars(tuple.elems), 0..) |elem_var, index| {
                    if (index > 0) try out.appendSlice(allocator, ", ");
                    try writeNestedPlaceholder(allocator, module_env, elem_var, out, depth + 1);
                }
                try out.appendSlice(allocator, ")");
            },
            // A tag union needs one of its tags, and which one is a decision
            // about the test rather than about the type. A function argument
            // would have to be written out in full.
            .tag_union,
            .empty_tag_union,
            .fn_pure,
            .fn_effectful,
            .fn_unbound,
            => return error.UnsupportedType,
        },
        // A type variable stands for whichever type the caller picks, so there
        // is no one literal that fits it.
        .flex, .rigid, .alias, .field_presence, .err => return error.UnsupportedType,
    }
}

/// Write a record literal holding a value for each of its fields.
fn writeRecordPlaceholder(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    fields: types.RecordField.SafeMultiList.Slice,
    out: *std.ArrayList(u8),
    depth: usize,
) PlaceholderError!void {
    if (fields.len == 0) {
        try out.appendSlice(allocator, "{}");
        return;
    }

    var iterator = type_utils.RecordFieldsIterator{
        .names = fields.items(.name),
        .presences = fields.items(.presence),
    };

    try out.appendSlice(allocator, "{ ");
    var written: usize = 0;
    while (iterator.next()) |field| : (written += 1) {
        if (written > 0) try out.appendSlice(allocator, ", ");
        try out.appendSlice(allocator, module_env.common.idents.getText(field.name));
        try out.appendSlice(allocator, ": ");
        try writeNestedPlaceholder(allocator, module_env, field.type_var, out, depth + 1);
    }
    try out.appendSlice(allocator, " }");
}

/// The function type behind a binding, or null when the binding is not one a
/// generated test could call.
///
/// An effectful function is left out: `expect` checks a value, and running
/// effects to produce one is not something a generated test should decide to
/// do. A function taking no arguments is left out as well, because its call
/// syntax is not the one written here.
fn callableFunction(module_env: *const ModuleEnv, type_var: types.Var) ?types.Func {
    const content = type_utils.unwrapAliases(&module_env.types, type_var, max_alias_depth).content;
    const func = switch (content) {
        .structure => |flat_type| switch (flat_type) {
            .fn_pure, .fn_unbound => |pure| pure,
            .fn_effectful,
            .record,
            .record_unbound,
            .tuple,
            .nominal_type,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => return null,
        },
        .flex, .rigid, .alias, .field_presence, .err => return null,
    };
    if (func.args.len() == 0) return null;
    return func;
}

/// Build the source of an `expect` that calls `name`, or null when no test can
/// be written for it: the binding is not a function this can call, or one of
/// its types has no placeholder value.
///
/// The text starts with the blank line that separates it from the definition it
/// is meant to follow, and carries no trailing newline: it is inserted at the
/// end of that definition, so the source already continues on the next line.
///
/// The caller owns the returned slice.
pub fn renderExpectTest(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    name: []const u8,
    type_var: types.Var,
) Allocator.Error!?[]u8 {
    var text: std.ArrayList(u8) = .empty;
    errdefer text.deinit(allocator);

    writeExpectTest(allocator, module_env, name, type_var, &text) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.UnsupportedType => {
            text.deinit(allocator);
            return null;
        },
    };

    return try text.toOwnedSlice(allocator);
}

/// Write the `expect`, refusing as soon as one of the types cannot be filled in.
fn writeExpectTest(
    allocator: Allocator,
    module_env: *const ModuleEnv,
    name: []const u8,
    type_var: types.Var,
    out: *std.ArrayList(u8),
) PlaceholderError!void {
    const func = callableFunction(module_env, type_var) orelse return error.UnsupportedType;

    // The values are placeholders of the right type, not a case anybody chose,
    // so the comment says so rather than claiming the test checks something.
    try out.appendSlice(allocator, "\n\n## TODO Replace these placeholder values with a case worth checking.\nexpect ");
    try out.appendSlice(allocator, name);
    try out.appendSlice(allocator, "(");
    for (module_env.types.sliceVars(func.args), 0..) |arg_var, index| {
        if (index > 0) try out.appendSlice(allocator, ", ");
        try writePlaceholder(allocator, module_env, arg_var, out);
    }
    try out.appendSlice(allocator, ") == ");
    try writePlaceholder(allocator, module_env, func.ret, out);
}
