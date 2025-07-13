//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("../../base.zig");
const types_mod = @import("../../types.zig");
const collections = @import("../../collections.zig");
const TypesStore = types_mod.Store;
const Var = types_mod.Var;
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

/// A mapping from old type variables to their fresh instantiations
pub const VarSubstitution = std.AutoHashMap(Var, Var);

/// Instantiate a polymorphic type with fresh type variables.
/// This creates a copy of the type structure with all flexible variables
/// replaced by fresh ones, while preserving type aliases and rigid variables.
pub fn instantiateVar(
    store: *TypesStore,
    var_: Var,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Var {
    // Check if we've already instantiated this variable
    if (substitution.get(var_)) |fresh_var| {
        return fresh_var;
    }

    const resolved = store.resolveVar(var_);
    const fresh_content = try instantiateContent(store, resolved.desc.content, substitution);

    // Create a fresh variable with the instantiated content
    const fresh_var = store.freshFromContent(fresh_content);

    // Remember this substitution for recursive references
    try substitution.put(var_, fresh_var);

    return fresh_var;
}

/// Instantiate a polymorphic type with fresh type variables.
/// Prefer `instantiateVar` over this function, as this function inits/deinits
/// the substitution map each call.
pub fn instantiateVarAlloc(
    store: *TypesStore,
    var_to_instantiate: Var,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    var substitution = VarSubstitution.init(allocator);
    defer substitution.deinit();

    return instantiateVar(store, var_to_instantiate, &substitution);
}

fn instantiateContent(
    store: *TypesStore,
    content: Content,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex_var => |maybe_ident| Content{ .flex_var = maybe_ident },
        .rigid_var => |ident| Content{ .rigid_var = ident },
        .alias => |alias| Content{ .alias = alias },
        .structure => |flat_type| blk: {
            // Instantiate the structure recursively
            const fresh_flat_type = try instantiateFlatType(store, flat_type, substitution);
            break :blk Content{ .structure = fresh_flat_type };
        },
        .err => Content.err,
    };
}

fn instantiateFlatType(
    store: *TypesStore,
    flat_type: FlatType,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .str => FlatType.str,
        .box => |box_var| FlatType{ .box = try instantiateVar(store, box_var, substitution) },
        .list => |list_var| FlatType{ .list = try instantiateVar(store, list_var, substitution) },
        .list_unbound => FlatType.list_unbound,
        .tuple => |tuple| FlatType{ .tuple = try instantiateTuple(store, tuple, substitution) },
        .num => |num| FlatType{ .num = try instantiateNum(store, num, substitution) },
        .nominal_type => |nominal| FlatType{ .nominal_type = try instantiateNominalType(store, nominal, substitution) },
        .fn_pure => |func| FlatType{ .fn_pure = try instantiateFunc(store, func, substitution) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try instantiateFunc(store, func, substitution) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try instantiateFunc(store, func, substitution) },
        .record => |record| FlatType{ .record = try instantiateRecord(store, record, substitution) },
        .record_unbound => |fields| FlatType{ .record_unbound = try instantiateRecordFields(store, fields, substitution) },
        .record_poly => |poly| blk: {
            const fresh_record = try instantiateRecord(store, poly.record, substitution);
            const fresh_var = try instantiateVar(store, poly.var_, substitution);
            break :blk FlatType{ .record_poly = .{ .record = fresh_record, .var_ = fresh_var } };
        },
        .empty_record => FlatType.empty_record,
        .tag_union => |tag_union| FlatType{ .tag_union = try instantiateTagUnion(store, tag_union, substitution) },

        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn instantiateNominalType(
    store: *TypesStore,
    nominal: types_mod.NominalType,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!types_mod.NominalType {
    var fresh_vars = std.ArrayList(Var).init(store.gpa);
    defer fresh_vars.deinit();

    try fresh_vars.append(store.getNominalBackingVar(nominal));

    var iter = store.iterNominalArgs(nominal);
    while (iter.next()) |arg_var| {
        const fresh_elem = try instantiateVar(store, arg_var, substitution);
        try fresh_vars.append(fresh_elem);
    }

    const fresh_vars_range = store.appendVars(fresh_vars.items);
    return types_mod.NominalType{
        .ident = nominal.ident,
        .vars = .{ .nonempty = fresh_vars_range },
        .origin_module = nominal.origin_module,
    };
}

fn instantiateTuple(
    store: *TypesStore,
    tuple: types_mod.Tuple,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = store.getTupleElemsSlice(tuple.elems);
    var fresh_elems = std.ArrayList(Var).init(store.gpa);
    defer fresh_elems.deinit();

    for (elems_slice) |elem_var| {
        const fresh_elem = try instantiateVar(store, elem_var, substitution);
        try fresh_elems.append(fresh_elem);
    }

    const fresh_elems_range = store.appendTupleElems(fresh_elems.items);
    return types_mod.Tuple{ .elems = fresh_elems_range };
}

fn instantiateNum(
    store: *TypesStore,
    num: Num,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Num {
    return switch (num) {
        .num_poly => |poly| Num{ .num_poly = .{ .var_ = try instantiateVar(store, poly.var_, substitution), .requirements = poly.requirements } },
        .int_poly => |poly| Num{ .int_poly = .{ .var_ = try instantiateVar(store, poly.var_, substitution), .requirements = poly.requirements } },
        .frac_poly => |poly| Num{ .frac_poly = .{ .var_ = try instantiateVar(store, poly.var_, substitution), .requirements = poly.requirements } },
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
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Func {
    const args_slice = store.getFuncArgsSlice(func.args);
    var fresh_args = std.ArrayList(Var).init(store.gpa);
    defer fresh_args.deinit();

    for (args_slice) |arg_var| {
        const fresh_arg = try instantiateVar(store, arg_var, substitution);
        try fresh_args.append(fresh_arg);
    }

    const fresh_ret = try instantiateVar(store, func.ret, substitution);
    const fresh_args_range = store.appendFuncArgs(fresh_args.items);
    return Func{
        .args = fresh_args_range,
        .ret = fresh_ret,
        .needs_instantiation = true,
    };
}

fn instantiateRecordFields(
    store: *TypesStore,
    fields: RecordField.SafeMultiList.Range,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const fields_start = store.record_fields.len();
    const fields_slice = store.getRecordFieldsSlice(fields);

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVar(store, type_var, substitution);
        _ = store.record_fields.append(store.gpa, RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    const fields_range = RecordField.SafeMultiList.Range{
        .start = @enumFromInt(fields_start),
        .end = @enumFromInt(store.record_fields.len()),
    };

    return fields_range;
}

fn instantiateRecord(
    store: *TypesStore,
    record: Record,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Record {
    const fields_start = store.record_fields.len();
    const fields_slice = store.getRecordFieldsSlice(record.fields);

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVar(store, type_var, substitution);
        _ = store.record_fields.append(store.gpa, RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    return Record{
        .fields = RecordField.SafeMultiList.Range{
            .start = @enumFromInt(fields_start),
            .end = @enumFromInt(store.record_fields.len()),
        },
        .ext = try instantiateVar(store, record.ext, substitution),
    };
}

fn instantiateTagUnion(
    store: *TypesStore,
    tag_union: TagUnion,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!TagUnion {
    const tags_start = store.tags.len();
    const tags_slice = store.getTagsSlice(tag_union.tags);

    for (tags_slice.items(.name), tags_slice.items(.args)) |tag_name, tag_args| {
        var fresh_args = std.ArrayList(Var).init(store.gpa);
        defer fresh_args.deinit();

        const args_slice = store.getTagArgsSlice(tag_args);
        for (args_slice) |arg_var| {
            const fresh_arg = try instantiateVar(store, arg_var, substitution);
            try fresh_args.append(fresh_arg);
        }

        const fresh_args_range = store.appendTagArgs(fresh_args.items);

        _ = store.tags.append(store.gpa, Tag{
            .name = tag_name,
            .args = fresh_args_range,
        });
    }

    const tags_range = Tag.SafeMultiList.Range{
        .start = @enumFromInt(tags_start),
        .end = @enumFromInt(store.tags.len()),
    };

    return TagUnion{
        .tags = tags_range,
        .ext = try instantiateVar(store, tag_union.ext, substitution),
    };
}
