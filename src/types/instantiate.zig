//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const types = @import("types.zig");
const store_mod = @import("store.zig");

const TypesStore = store_mod.Store;
const Var = types.Var;
const Content = types.Content;
const FlatType = types.FlatType;
const Alias = types.Alias;
const Func = types.Func;
const Record = types.Record;
const TagUnion = types.TagUnion;
const RecordField = types.RecordField;
const Tag = types.Tag;
const Num = types.Num;
const NominalType = types.NominalType;

/// A mapping from old type variables to their fresh instantiations
pub const VarSubstitution = std.AutoHashMap(Var, Var);

/// A mapping from rigid variable names to their target variables
pub const RigidVarSubstitution = std.StringHashMap(Var);

/// Instantiation mode determines how rigid variables are handled
pub const InstantiationMode = union(enum) {
    /// Copy mode: Create fresh copies, preserving rigid variables (case 1)
    copy,
    /// Apply mode: Replace rigid variables according to substitution map (case 2)
    apply_rigid_substitution: *RigidVarSubstitution,
    /// Generalize mode: Replace rigid variables with fresh flex variables (case 3)
    generalize_rigid_to_flex: *RigidVarSubstitution,
};

/// Instantiate a polymorphic type with fresh type variables.
/// This creates a copy of the type structure based on the instantiation mode.
pub fn instantiateVar(
    store: *TypesStore,
    var_: Var,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!Var {
    // Check if we've already instantiated this variable
    if (substitution.get(var_)) |fresh_var| {
        return fresh_var;
    }

    const resolved = store.resolveVar(var_);

    switch (resolved.desc.content) {
        .rigid_var => |ident| {
            switch (mode) {
                .copy => {
                    // Case 1: Preserve rigid variables as-is
                    const fresh_var = try store.freshFromContent(resolved.desc.content);
                    try substitution.put(var_, fresh_var);
                    return fresh_var;
                },
                .apply_rigid_substitution => |rigid_subs_map| {
                    // Case 2: Apply rigid variable substitution (type alias instantiation)
                    const ident_bytes = idents.getText(ident);
                    if (rigid_subs_map.get(ident_bytes)) |var_to_sub| {
                        try substitution.put(var_, var_to_sub);
                        return var_to_sub;
                    } else {
                        // Rigid variable not in substitution map - preserve as rigid
                        const fresh_var = try store.freshFromContent(resolved.desc.content);
                        try substitution.put(var_, fresh_var);
                        return fresh_var;
                    }
                },
                .generalize_rigid_to_flex => |flex_map| {
                    // Case 3: Convert rigid variables to fresh flex variables (function call instantiation)
                    const ident_bytes = idents.getText(ident);

                    if (flex_map.get(ident_bytes)) |existing_flex_var| {
                        try substitution.put(var_, existing_flex_var);
                        return existing_flex_var;
                    } else {
                        // Create a new flex variable for this rigid variable name
                        const fresh_var = try store.freshFromContent(Content{ .flex_var = ident });
                        try flex_map.put(ident_bytes, fresh_var);
                        try substitution.put(var_, fresh_var);
                        return fresh_var;
                    }
                },
            }
        },
        else => {
            const fresh_content = try instantiateContent(store, resolved.desc.content, idents, substitution, mode);

            // Create a fresh variable with the instantiated content
            const fresh_var = try store.freshFromContent(fresh_content);

            // Remember this substitution for recursive references
            try substitution.put(var_, fresh_var);

            return fresh_var;
        },
    }
}

/// Instantiate a polymorphic type with fresh type variables.
/// Prefer `instantiateVar` over this function, as this function inits/deinits
/// the substitution map each call.
pub fn instantiateVarAlloc(
    store: *TypesStore,
    var_to_instantiate: Var,
    idents: *const base.Ident.Store,
    mode: InstantiationMode,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    var substitution = VarSubstitution.init(allocator);
    defer substitution.deinit();

    return instantiateVar(store, var_to_instantiate, idents, &substitution, mode);
}

fn instantiateContent(
    store: *TypesStore,
    content: Content,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex_var => |maybe_ident| Content{ .flex_var = maybe_ident },
        // .rigid_var => |maybe_ident| Content{ .rigid_var = maybe_ident },
        .rigid_var => unreachable,
        .alias => |alias| {
            // Instantiate the structure recursively
            const fresh_alias = try instantiateAlias(store, alias, idents, substitution, mode);
            return Content{ .alias = fresh_alias };
        },
        .structure => |flat_type| blk: {
            // Instantiate the structure recursively
            const fresh_flat_type = try instantiateFlatType(store, flat_type, idents, substitution, mode);
            break :blk Content{ .structure = fresh_flat_type };
        },
        .err => Content.err,
    };
}

fn instantiateAlias(
    store: *TypesStore,
    alias: types.Alias,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!types.Alias {
    var fresh_vars = std.ArrayList(Var).init(store.gpa);
    defer fresh_vars.deinit();

    const backing_var = store.getAliasBackingVar(alias);
    const fresh_backing_var = try instantiateVar(store, backing_var, idents, substitution, mode);
    try fresh_vars.append(fresh_backing_var);

    var iter = store.iterAliasArgs(alias);
    while (iter.next()) |arg_var| {
        const fresh_elem = try instantiateVar(store, arg_var, idents, substitution, mode);
        try fresh_vars.append(fresh_elem);
    }

    const fresh_vars_range = try store.appendVars(fresh_vars.items);
    return types.Alias{
        .ident = alias.ident,
        .vars = .{ .nonempty = fresh_vars_range },
    };
}

fn instantiateFlatType(
    store: *TypesStore,
    flat_type: FlatType,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .str => FlatType.str,
        .box => |box_var| FlatType{ .box = try instantiateVar(store, box_var, idents, substitution, mode) },
        .list => |list_var| FlatType{ .list = try instantiateVar(store, list_var, idents, substitution, mode) },
        .list_unbound => FlatType.list_unbound,
        .tuple => |tuple| FlatType{ .tuple = try instantiateTuple(store, tuple, idents, substitution, mode) },
        .num => |num| FlatType{ .num = try instantiateNum(store, num, idents, substitution, mode) },
        .nominal_type => |nominal| FlatType{ .nominal_type = try instantiateNominalType(store, nominal, idents, substitution, mode) },
        .fn_pure => |func| FlatType{ .fn_pure = try instantiateFunc(store, func, idents, substitution, mode) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try instantiateFunc(store, func, idents, substitution, mode) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try instantiateFunc(store, func, idents, substitution, mode) },
        .record => |record| FlatType{ .record = try instantiateRecord(store, record, idents, substitution, mode) },
        .record_unbound => |fields| FlatType{ .record_unbound = try instantiateRecordFields(store, fields, idents, substitution, mode) },
        .record_poly => |poly| blk: {
            const fresh_record = try instantiateRecord(store, poly.record, idents, substitution, mode);
            const fresh_var = try instantiateVar(store, poly.var_, idents, substitution, mode);
            break :blk FlatType{ .record_poly = .{ .record = fresh_record, .var_ = fresh_var } };
        },
        .empty_record => FlatType.empty_record,
        .tag_union => |tag_union| FlatType{ .tag_union = try instantiateTagUnion(store, tag_union, idents, substitution, mode) },
        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn instantiateNominalType(
    store: *TypesStore,
    nominal: types.NominalType,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!types.NominalType {
    var fresh_vars = std.ArrayList(Var).init(store.gpa);
    defer fresh_vars.deinit();

    try fresh_vars.append(store.getNominalBackingVar(nominal));

    var iter = store.iterNominalArgs(nominal);
    while (iter.next()) |arg_var| {
        const fresh_elem = try instantiateVar(store, arg_var, idents, substitution, mode);
        try fresh_vars.append(fresh_elem);
    }

    const fresh_vars_range = try store.appendVars(fresh_vars.items);
    return types.NominalType{
        .ident = nominal.ident,
        .vars = .{ .nonempty = fresh_vars_range },
        .origin_module = nominal.origin_module,
    };
}

fn instantiateTuple(
    store: *TypesStore,
    tuple: types.Tuple,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!types.Tuple {
    const elems_slice = store.sliceVars(tuple.elems);
    var fresh_elems = std.ArrayList(Var).init(store.gpa);
    defer fresh_elems.deinit();

    for (elems_slice) |elem_var| {
        const fresh_elem = try instantiateVar(store, elem_var, idents, substitution, mode);
        try fresh_elems.append(fresh_elem);
    }

    const fresh_elems_range = try store.appendVars(fresh_elems.items);
    return types.Tuple{ .elems = fresh_elems_range };
}

fn instantiateNum(
    store: *TypesStore,
    num: Num,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!Num {
    return switch (num) {
        .num_poly => |poly| Num{ .num_poly = .{ .var_ = try instantiateVar(store, poly.var_, idents, substitution, mode), .requirements = poly.requirements } },
        .int_poly => |poly| Num{ .int_poly = .{ .var_ = try instantiateVar(store, poly.var_, idents, substitution, mode), .requirements = poly.requirements } },
        .frac_poly => |poly| Num{ .frac_poly = .{ .var_ = try instantiateVar(store, poly.var_, idents, substitution, mode), .requirements = poly.requirements } },
        // Concrete types remain unchanged
        .int_precision => |precision| Num{ .int_precision = precision },
        .frac_precision => |precision| Num{ .frac_precision = precision },
        .num_unbound => |unbound| Num{ .num_unbound = unbound },
        .int_unbound => |unbound| Num{ .int_unbound = unbound },
        .frac_unbound => |unbound| Num{ .frac_unbound = unbound },
        .num_compact => |compact| Num{ .num_compact = compact },
    };
}

fn instantiateFunc(
    store: *TypesStore,
    func: Func,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!Func {
    const args_slice = store.sliceVars(func.args);
    var fresh_args = std.ArrayList(Var).init(store.gpa);
    defer fresh_args.deinit();

    for (args_slice) |arg_var| {
        const fresh_arg = try instantiateVar(store, arg_var, idents, substitution, mode);
        try fresh_args.append(fresh_arg);
    }

    const fresh_ret = try instantiateVar(store, func.ret, idents, substitution, mode);
    const fresh_args_range = try store.appendVars(fresh_args.items);
    return Func{
        .args = fresh_args_range,
        .ret = fresh_ret,
        .needs_instantiation = true,
    };
}

fn instantiateRecordFields(
    store: *TypesStore,
    fields: RecordField.SafeMultiList.Range,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const fields_slice = store.getRecordFieldsSlice(fields);

    var fresh_fields = std.ArrayList(RecordField).init(store.gpa);
    defer fresh_fields.deinit();

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVar(store, type_var, idents, substitution, mode);
        _ = try fresh_fields.append(RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    return try store.appendRecordFields(fresh_fields.items);
}

fn instantiateRecord(
    store: *TypesStore,
    record: Record,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!Record {
    const fields_slice = store.getRecordFieldsSlice(record.fields);

    var fresh_fields = std.ArrayList(RecordField).init(store.gpa);
    defer fresh_fields.deinit();

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVar(store, type_var, idents, substitution, mode);
        _ = try fresh_fields.append(RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    const fields_range = try store.appendRecordFields(fresh_fields.items);
    return Record{
        .fields = fields_range,
        .ext = try instantiateVar(store, record.ext, idents, substitution, mode),
    };
}

fn instantiateTagUnion(
    store: *TypesStore,
    tag_union: TagUnion,
    idents: *const base.Ident.Store,
    substitution: *VarSubstitution,
    mode: InstantiationMode,
) std.mem.Allocator.Error!TagUnion {
    const tags_slice = store.getTagsSlice(tag_union.tags);

    var fresh_tags = std.ArrayList(Tag).init(store.gpa);
    defer fresh_tags.deinit();

    for (tags_slice.items(.name), tags_slice.items(.args)) |tag_name, tag_args| {
        var fresh_args = std.ArrayList(Var).init(store.gpa);
        defer fresh_args.deinit();

        const args_slice = store.sliceVars(tag_args);
        for (args_slice) |arg_var| {
            const fresh_arg = try instantiateVar(store, arg_var, idents, substitution, mode);
            try fresh_args.append(fresh_arg);
        }

        const fresh_args_range = try store.appendVars(fresh_args.items);

        _ = try fresh_tags.append(Tag{
            .name = tag_name,
            .args = fresh_args_range,
        });
    }

    const tags_range = try store.appendTags(fresh_tags.items);
    return TagUnion{
        .tags = tags_range,
        .ext = try instantiateVar(store, tag_union.ext, idents, substitution, mode),
    };
}
