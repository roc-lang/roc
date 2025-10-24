//! Cross-module type copying for imports.
//!
//! This module provides functionality to copy types from one module's type store
//! to another module's type store when importing. This ensures each module maintains
//! its own consistent type variable namespace while still being able to use types
//! from other modules.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");

const TypesStore = types_mod.Store;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Alias = types_mod.Alias;
const Func = types_mod.Func;
const Record = types_mod.Record;
const TagUnion = types_mod.TagUnion;
const RecordField = types_mod.RecordField;
const Tag = types_mod.Tag;
const Num = types_mod.Num;
const NominalType = types_mod.NominalType;

/// A mapping from source type variables to destination type variables
/// This is only used during the copy operation to ensure consistent mapping
/// of type variables that appear multiple times in the same type structure.
const VarMapping = std.AutoHashMap(Var, Var);

/// Copy a type from one module's type store to another module's type store.
/// This creates a completely fresh copy with new variable indices in the destination store.
pub fn copyVar(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    const resolved = source_store.resolveVar(source_var);

    // Check if we've already copied this variable
    if (var_mapping.get(resolved.var_)) |dest_var| {
        return dest_var;
    }

    // Create a placeholder variable first to break cycles
    const placeholder_var = try dest_store.fresh();

    // Record the mapping immediately to handle recursive types
    try var_mapping.put(resolved.var_, placeholder_var);

    // Now copy the content (which may recursively reference this variable)
    const dest_content = try copyContent(source_store, dest_store, resolved.desc.content, var_mapping, source_idents, dest_idents, allocator);

    // Update the placeholder with the actual content
    try dest_store.setVarDesc(placeholder_var, .{
        .content = dest_content,
        .rank = types_mod.Rank.generalized,
        .mark = types_mod.Mark.none,
    });

    return placeholder_var;
}

fn copyContent(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    content: Content,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex => |flex| Content{ .flex = try copyFlex(source_store, dest_store, flex, var_mapping, source_idents, dest_idents, allocator) },
        .rigid => |rigid| Content{ .rigid = try copyRigid(source_store, dest_store, rigid, var_mapping, source_idents, dest_idents, allocator) },
        .alias => |alias| Content{ .alias = try copyAlias(source_store, dest_store, alias, var_mapping, source_idents, dest_idents, allocator) },
        .structure => |flat_type| Content{ .structure = try copyFlatType(source_store, dest_store, flat_type, var_mapping, source_idents, dest_idents, allocator) },
        .err => Content.err,
    };
}

fn copyFlex(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_flex: Flex,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Flex {
    // Translate the type name ident
    const mb_translated_name = blk: {
        if (source_flex.name) |name_ident| {
            const name_bytes = source_idents.getText(name_ident);
            const translated_name = try dest_idents.insert(allocator, base.Ident.for_text(name_bytes));
            break :blk translated_name;
        } else {
            break :blk null;
        }
    };

    // Copy the constraints
    const dest_constraints_range = try copyStaticDispatchConstraints(
        source_store,
        dest_store,
        source_flex.constraints,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    );

    return Flex{
        .name = mb_translated_name,
        .constraints = dest_constraints_range,
    };
}

fn copyRigid(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_rigid: Rigid,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Rigid {
    // Translate the type name ident
    const name_bytes = source_idents.getText(source_rigid.name);
    const translated_name = try dest_idents.insert(allocator, base.Ident.for_text(name_bytes));

    // Copy the constraints
    const dest_constraints_range = try copyStaticDispatchConstraints(
        source_store,
        dest_store,
        source_rigid.constraints,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    );

    return Rigid{
        .name = translated_name,
        .constraints = dest_constraints_range,
    };
}

fn copyAlias(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_alias: Alias,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Alias {
    // Translate the type name ident
    const type_name_str = source_idents.getText(source_alias.ident.ident_idx);
    const translated_ident = try dest_idents.insert(allocator, base.Ident.for_text(type_name_str));

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    const origin_backing = source_store.getAliasBackingVar(source_alias);
    const dest_backing = try copyVar(source_store, dest_store, origin_backing, var_mapping, source_idents, dest_idents, allocator);
    try dest_args.append(dest_store.gpa, dest_backing);

    const origin_args = source_store.sliceAliasArgs(source_alias);
    for (origin_args) |arg_var| {
        const dest_arg = try copyVar(source_store, dest_store, arg_var, var_mapping, source_idents, dest_idents, allocator);
        try dest_args.append(dest_store.gpa, dest_arg);
    }

    const dest_vars_span = try dest_store.appendVars(dest_args.items);

    return Alias{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .vars = .{ .nonempty = dest_vars_span },
    };
}

fn copyFlatType(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    flat_type: FlatType,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .str => FlatType.str,
        .box => |box_var| FlatType{ .box = try copyVar(source_store, dest_store, box_var, var_mapping, source_idents, dest_idents, allocator) },
        .list => |list_var| FlatType{ .list = try copyVar(source_store, dest_store, list_var, var_mapping, source_idents, dest_idents, allocator) },
        .list_unbound => FlatType.list_unbound,
        .tuple => |tuple| FlatType{ .tuple = try copyTuple(source_store, dest_store, tuple, var_mapping, source_idents, dest_idents, allocator) },
        .num => |num| FlatType{ .num = try copyNum(source_store, dest_store, num, var_mapping, source_idents, dest_idents, allocator) },
        .nominal_type => |nominal| FlatType{ .nominal_type = try copyNominalType(source_store, dest_store, nominal, var_mapping, source_idents, dest_idents, allocator) },
        .fn_pure => |func| FlatType{ .fn_pure = try copyFunc(source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try copyFunc(source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try copyFunc(source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .record => |record| FlatType{ .record = try copyRecord(source_store, dest_store, record, var_mapping, source_idents, dest_idents, allocator) },
        .tag_union => |tag_union| FlatType{ .tag_union = try copyTagUnion(source_store, dest_store, tag_union, var_mapping, source_idents, dest_idents, allocator) },
        .record_unbound => |fields| FlatType{ .record_unbound = try copyRecordFields(source_store, dest_store, fields, var_mapping, source_idents, dest_idents, allocator) },
        .empty_record => FlatType.empty_record,
        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn copyTuple(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    tuple: types_mod.Tuple,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = source_store.sliceVars(tuple.elems);

    var dest_elems = std.ArrayList(Var).empty;
    defer dest_elems.deinit(dest_store.gpa);

    for (elems_slice) |elem_var| {
        const dest_elem = try copyVar(source_store, dest_store, elem_var, var_mapping, source_idents, dest_idents, allocator);
        try dest_elems.append(dest_store.gpa, dest_elem);
    }

    const dest_range = try dest_store.appendVars(dest_elems.items);
    return types_mod.Tuple{ .elems = dest_range };
}

fn copyNum(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    num: Num,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Num {
    return switch (num) {
        .num_poly => |poly_var| Num{ .num_poly = try copyVar(source_store, dest_store, poly_var, var_mapping, source_idents, dest_idents, allocator) },
        .int_poly => |poly_var| Num{ .int_poly = try copyVar(source_store, dest_store, poly_var, var_mapping, source_idents, dest_idents, allocator) },
        .frac_poly => |poly_var| Num{ .frac_poly = try copyVar(source_store, dest_store, poly_var, var_mapping, source_idents, dest_idents, allocator) },
        .num_unbound => |unbound| Num{ .num_unbound = unbound },
        .int_unbound => |unbound| Num{ .int_unbound = unbound },
        .frac_unbound => |unbound| Num{ .frac_unbound = unbound },
        .int_precision => |precision| Num{ .int_precision = precision },
        .frac_precision => |precision| Num{ .frac_precision = precision },
        .num_compact => |compact| Num{ .num_compact = compact },
    };
}

fn copyFunc(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    func: Func,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Func {
    const args_slice = source_store.sliceVars(func.args);

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    for (args_slice) |arg_var| {
        const dest_arg = try copyVar(source_store, dest_store, arg_var, var_mapping, source_idents, dest_idents, allocator);
        try dest_args.append(dest_store.gpa, dest_arg);
    }

    const dest_ret = try copyVar(source_store, dest_store, func.ret, var_mapping, source_idents, dest_idents, allocator);

    const dest_args_range = try dest_store.appendVars(dest_args.items);
    return Func{
        .args = dest_args_range,
        .ret = dest_ret,
        .needs_instantiation = func.needs_instantiation,
    };
}

fn copyRecordFields(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!types_mod.RecordField.SafeMultiList.Range {
    const source_fields = source_store.getRecordFieldsSlice(fields_range);

    var fresh_fields = std.ArrayList(RecordField).empty;
    defer fresh_fields.deinit(allocator);

    for (source_fields.items(.name), source_fields.items(.var_)) |name, var_| {
        const name_str = source_idents.getText(name);
        const translated_name = try dest_idents.insert(allocator, base.Ident.for_text(name_str));
        _ = try fresh_fields.append(allocator, .{
            .name = translated_name, // Field names are local to the record type
            .var_ = try copyVar(source_store, dest_store, var_, var_mapping, source_idents, dest_idents, allocator),
        });
    }

    return try dest_store.appendRecordFields(fresh_fields.items);
}

fn copyRecord(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    record: Record,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Record {
    const fields_range = try copyRecordFields(
        source_store,
        dest_store,
        record.fields,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    );

    return Record{
        .fields = fields_range,
        .ext = try copyVar(source_store, dest_store, record.ext, var_mapping, source_idents, dest_idents, allocator),
    };
}

fn copyTagUnion(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    tag_union: TagUnion,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!TagUnion {
    const tags_slice = source_store.getTagsSlice(tag_union.tags);

    var fresh_tags = std.ArrayList(Tag).empty;
    defer fresh_tags.deinit(allocator);

    for (tags_slice.items(.name), tags_slice.items(.args)) |name, args_range| {
        const args_slice = source_store.sliceVars(args_range);

        var dest_args = std.ArrayList(Var).empty;
        defer dest_args.deinit(dest_store.gpa);

        for (args_slice) |arg_var| {
            const dest_arg = try copyVar(source_store, dest_store, arg_var, var_mapping, source_idents, dest_idents, allocator);
            try dest_args.append(dest_store.gpa, dest_arg);
        }

        const dest_args_range = try dest_store.appendVars(dest_args.items);

        const name_str = source_idents.getText(name);
        const translated_name = try dest_idents.insert(allocator, base.Ident.for_text(name_str));

        _ = try fresh_tags.append(allocator, .{
            .name = translated_name, // Tag names are local to the union type
            .args = dest_args_range,
        });
    }

    const tags_range = try dest_store.appendTags(fresh_tags.items);
    return TagUnion{
        .tags = tags_range,
        .ext = try copyVar(source_store, dest_store, tag_union.ext, var_mapping, source_idents, dest_idents, allocator),
    };
}

fn copyNominalType(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_nominal: NominalType,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!NominalType {

    // Translate the type name ident
    const type_name_str = source_idents.getText(source_nominal.ident.ident_idx);
    const translated_ident = try dest_idents.insert(allocator, base.Ident.for_text(type_name_str));

    // Translate the origin module ident
    const origin_str = source_idents.getText(source_nominal.origin_module);
    const translated_origin = try dest_idents.insert(allocator, base.Ident.for_text(origin_str));

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    const origin_backing = source_store.getNominalBackingVar(source_nominal);
    const dest_backing = try copyVar(source_store, dest_store, origin_backing, var_mapping, source_idents, dest_idents, allocator);
    try dest_args.append(dest_store.gpa, dest_backing);

    const origin_args = source_store.sliceNominalArgs(source_nominal);
    for (origin_args) |arg_var| {
        const dest_arg = try copyVar(source_store, dest_store, arg_var, var_mapping, source_idents, dest_idents, allocator);
        try dest_args.append(dest_store.gpa, dest_arg);
    }

    const dest_vars_span = try dest_store.appendVars(dest_args.items);

    return NominalType{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .vars = .{ .nonempty = dest_vars_span },
        .origin_module = translated_origin,
    };
}

fn copyStaticDispatchConstraints(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_constraints: StaticDispatchConstraint.SafeList.Range,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
    const source_constraints_len = source_constraints.len();
    if (source_constraints_len == 0) {
        return StaticDispatchConstraint.SafeList.Range.empty();
    } else {
        // Setup tmp state
        var dest_constraints = try std.array_list.Managed(StaticDispatchConstraint).initCapacity(dest_store.gpa, source_constraints_len);
        defer dest_constraints.deinit();

        // Iterate over the constraints
        for (source_store.sliceStaticDispatchConstraints(source_constraints)) |source_constraint| {
            // Translate the fn name
            const fn_name_bytes = source_idents.getText(source_constraint.fn_name);
            const translated_fn_name = try dest_idents.insert(allocator, base.Ident.for_text(fn_name_bytes));

            try dest_constraints.append(StaticDispatchConstraint{
                .fn_name = translated_fn_name,
                .fn_var = try copyVar(source_store, dest_store, source_constraint.fn_var, var_mapping, source_idents, dest_idents, allocator),
            });
        }

        const dest_constraints_range = try dest_store.appendStaticDispatchConstraints(dest_constraints.items);
        return dest_constraints_range;
    }
}
