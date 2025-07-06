//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const types_mod = @import("../../types.zig");
const collections = @import("../../collections.zig");
const base = @import("../../base.zig");
const CIR = @import("../canonicalize/CIR.zig");
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
const Region = base.Region;

/// A mapping from old type variables to their fresh instantiations
const VarSubstitution = std.AutoHashMap(Var, Var);

/// Maps from old variables to their regions for copying during instantiation
const RegionMap = std.AutoHashMap(Var, Region);

/// Context for instantiation that includes the CIR for region tracking
pub const InstantiateContext = struct {
    store: *TypesStore,
    can_ir: *CIR,
    allocator: std.mem.Allocator,
};

/// Instantiate a polymorphic type with fresh type variables.
/// This creates a copy of the type structure with all flexible variables
/// replaced by fresh ones, while preserving type aliases and rigid variables.
pub fn instantiateVar(
    ctx: InstantiateContext,
    var_to_instantiate: Var,
) std.mem.Allocator.Error!Var {
    var substitution = VarSubstitution.init(ctx.allocator);
    defer substitution.deinit();

    var region_map = RegionMap.init(ctx.allocator);
    defer region_map.deinit();

    return instantiateVarWithSubst(ctx, var_to_instantiate, &substitution, &region_map);
}

/// Test-only version that doesn't require CIR/regions
pub fn instantiateVarForTest(
    store: *TypesStore,
    var_to_instantiate: Var,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    var substitution = VarSubstitution.init(allocator);
    defer substitution.deinit();

    return instantiateVarWithSubstForTest(store, var_to_instantiate, &substitution);
}

/// Internal test implementation without regions
fn instantiateVarWithSubstForTest(
    store: *TypesStore,
    var_: Var,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Var {
    // Check if we've already instantiated this variable
    if (substitution.get(var_)) |fresh_var| {
        return fresh_var;
    }

    const resolved = store.resolveVar(var_);
    const fresh_content = try instantiateContentForTest(store, resolved.desc.content, substitution);

    // Create a fresh variable with the instantiated content
    const fresh_var = store.freshFromContent(fresh_content);

    // Remember this substitution for recursive references
    try substitution.put(var_, fresh_var);

    return fresh_var;
}

/// Internal implementation that tracks variable substitutions
fn instantiateVarWithSubst(
    ctx: InstantiateContext,
    var_: Var,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!Var {
    // Check if we've already instantiated this variable
    if (substitution.get(var_)) |fresh_var| {
        return fresh_var;
    }

    const resolved = ctx.store.resolveVar(var_);
    const fresh_content = try instantiateContent(ctx, resolved.desc.content, substitution, region_map);

    // Create a fresh variable with the instantiated content
    const fresh_var = ctx.store.freshFromContent(fresh_content);

    // Copy the region from the original variable to the new one
    const original_idx: u32 = @intFromEnum(var_);
    const fresh_idx: u32 = @intFromEnum(fresh_var);

    // Get the original region if it exists
    var original_region = Region.zero();
    if (original_idx < ctx.can_ir.store.regions.len()) {
        original_region = ctx.can_ir.store.regions.get(@enumFromInt(original_idx)).*;
    }

    // Ensure regions array is large enough for the fresh variable
    const needed_len = fresh_idx + 1;
    if (ctx.can_ir.store.regions.len() < needed_len) {
        // Fill gaps with zero regions
        for (ctx.can_ir.store.regions.len()..needed_len) |_| {
            _ = ctx.can_ir.store.regions.append(ctx.store.gpa, Region.zero());
        }
    }

    // Set the region for the fresh variable
    ctx.can_ir.store.regions.set(@enumFromInt(fresh_idx), original_region);

    // Remember this substitution for recursive references
    try substitution.put(var_, fresh_var);

    return fresh_var;
}

fn instantiateContent(
    ctx: InstantiateContext,
    content: Content,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex_var => |maybe_ident| Content{ .flex_var = maybe_ident },
        .rigid_var => |ident| Content{ .rigid_var = ident },
        .alias => |alias| blk: {
            // Instantiate the alias while preserving its identity
            const fresh_alias = try instantiateAlias(ctx.store, alias, substitution);
            break :blk Content{ .alias = fresh_alias };
        },
        .structure => |flat_type| blk: {
            // Instantiate the structure recursively
            const fresh_flat_type = try instantiateFlatType(ctx, flat_type, substitution, region_map);
            break :blk Content{ .structure = fresh_flat_type };
        },
        .err => Content.err,
    };
}

fn instantiateContentForTest(
    store: *TypesStore,
    content: Content,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex_var => |maybe_ident| Content{ .flex_var = maybe_ident },
        .rigid_var => |ident| Content{ .rigid_var = ident },
        .alias => |alias| blk: {
            // Instantiate the alias while preserving its identity
            const fresh_alias = try instantiateAlias(store, alias, substitution);
            break :blk Content{ .alias = fresh_alias };
        },
        .structure => |flat_type| blk: {
            // Instantiate the structure recursively
            const fresh_flat_type = try instantiateFlatTypeForTest(store, flat_type, substitution);
            break :blk Content{ .structure = fresh_flat_type };
        },
        .err => Content.err,
    };
}

fn instantiateFlatTypeForTest(
    store: *TypesStore,
    flat_type: FlatType,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .str => FlatType.str,
        .box => |box_var| FlatType{ .box = try instantiateVarWithSubstForTest(store, box_var, substitution) },
        .list => |list_var| FlatType{ .list = try instantiateVarWithSubstForTest(store, list_var, substitution) },
        .list_unbound => FlatType.list_unbound,
        .tuple => |tuple| FlatType{ .tuple = try instantiateTupleForTest(store, tuple, substitution) },
        .num => |num| FlatType{ .num = try instantiateNumForTest(store, num, substitution) },
        .nominal_type => |nominal| FlatType{ .nominal_type = try instantiateNominalType(store, nominal, substitution) },
        .fn_pure => |func| FlatType{ .fn_pure = try instantiateFuncForTest(store, func, substitution) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try instantiateFuncForTest(store, func, substitution) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try instantiateFuncForTest(store, func, substitution) },
        .record => |record| FlatType{ .record = try instantiateRecordForTest(store, record, substitution) },
        .record_unbound => |fields| FlatType{ .record_unbound = try instantiateRecordFieldsForTest(store, fields, substitution) },
        .record_poly => |poly| blk: {
            const fresh_record = try instantiateRecordForTest(store, poly.record, substitution);
            const fresh_var = try instantiateVarWithSubstForTest(store, poly.var_, substitution);
            break :blk FlatType{ .record_poly = .{ .record = fresh_record, .var_ = fresh_var } };
        },
        .empty_record => FlatType.empty_record,
        .tag_union => |tag_union| FlatType{ .tag_union = try instantiateTagUnionForTest(store, tag_union, substitution) },
        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn instantiateAlias(
    _: *TypesStore,
    alias: Alias,
    _: *VarSubstitution,
) std.mem.Allocator.Error!Alias {
    // Type aliases in Roc use a special variable layout where the alias variable
    // and its components (backing type and arguments) are stored as adjacent
    // variables in the type store. The layout is:
    //   - alias_var: The alias itself
    //   - alias_var + 1: The backing type variable
    //   - alias_var + 2, +3, ...: Type argument variables
    //
    // During instantiation, we preserve the alias structure itself. The backing
    // type and arguments will be instantiated if/when they are accessed through
    // the alias variable.

    return alias;
}

fn instantiateFlatType(
    ctx: InstantiateContext,
    flat_type: FlatType,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .str => FlatType.str,
        .box => |box_var| FlatType{ .box = try instantiateVarWithSubst(ctx, box_var, substitution, region_map) },
        .list => |list_var| FlatType{ .list = try instantiateVarWithSubst(ctx, list_var, substitution, region_map) },
        .list_unbound => FlatType.list_unbound,
        .tuple => |tuple| FlatType{ .tuple = try instantiateTuple(ctx, tuple, substitution, region_map) },
        .num => |num| FlatType{ .num = try instantiateNum(ctx, num, substitution, region_map) },
        .nominal_type => |nominal| FlatType{ .nominal_type = try instantiateNominalType(ctx.store, nominal, substitution) },
        .fn_pure => |func| FlatType{ .fn_pure = try instantiateFunc(ctx, func, substitution, region_map) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try instantiateFunc(ctx, func, substitution, region_map) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try instantiateFunc(ctx, func, substitution, region_map) },
        .record => |record| FlatType{ .record = try instantiateRecord(ctx, record, substitution, region_map) },
        .record_unbound => |fields| FlatType{ .record_unbound = try instantiateRecordFields(ctx, fields, substitution, region_map) },
        .record_poly => |poly| blk: {
            const fresh_record = try instantiateRecord(ctx, poly.record, substitution, region_map);
            const fresh_var = try instantiateVarWithSubst(ctx, poly.var_, substitution, region_map);
            break :blk FlatType{ .record_poly = .{ .record = fresh_record, .var_ = fresh_var } };
        },
        .empty_record => FlatType.empty_record,
        .tag_union => |tag_union| FlatType{ .tag_union = try instantiateTagUnion(ctx, tag_union, substitution, region_map) },
        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn instantiateTuple(
    ctx: InstantiateContext,
    tuple: types_mod.Tuple,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = ctx.store.getTupleElemsSlice(tuple.elems);

    var fresh_elems = std.ArrayList(Var).init(ctx.store.gpa);
    defer fresh_elems.deinit();

    for (elems_slice) |elem_var| {
        const fresh_elem = try instantiateVarWithSubst(ctx, elem_var, substitution, region_map);
        try fresh_elems.append(fresh_elem);
    }

    const fresh_range = ctx.store.appendTupleElems(fresh_elems.items);
    return types_mod.Tuple{ .elems = fresh_range };
}

fn instantiateTupleForTest(
    store: *TypesStore,
    tuple: types_mod.Tuple,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = store.getTupleElemsSlice(tuple.elems);

    var fresh_elems = std.ArrayList(Var).init(store.gpa);
    defer fresh_elems.deinit();

    for (elems_slice) |elem_var| {
        const fresh_elem = try instantiateVarWithSubstForTest(store, elem_var, substitution);
        try fresh_elems.append(fresh_elem);
    }

    const fresh_range = store.appendTupleElems(fresh_elems.items);
    return types_mod.Tuple{ .elems = fresh_range };
}

fn instantiateNum(
    ctx: InstantiateContext,
    num: Num,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!Num {
    return switch (num) {
        .num_poly => |poly| Num{ .num_poly = .{ .var_ = try instantiateVarWithSubst(ctx, poly.var_, substitution, region_map), .requirements = poly.requirements } },
        .int_poly => |poly| Num{ .int_poly = .{ .var_ = try instantiateVarWithSubst(ctx, poly.var_, substitution, region_map), .requirements = poly.requirements } },
        .frac_poly => |poly| Num{ .frac_poly = .{ .var_ = try instantiateVarWithSubst(ctx, poly.var_, substitution, region_map), .requirements = poly.requirements } },
        // Concrete types remain unchanged
        .int_precision => |precision| Num{ .int_precision = precision },
        .frac_precision => |precision| Num{ .frac_precision = precision },
        .num_compact => |compact| Num{ .num_compact = compact },
        .num_unbound => |requirements| Num{ .num_unbound = requirements },
        .int_unbound => |requirements| Num{ .int_unbound = requirements },
        .frac_unbound => |requirements| Num{ .frac_unbound = requirements },
    };
}

fn instantiateNumForTest(
    store: *TypesStore,
    num: Num,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Num {
    return switch (num) {
        .num_poly => |poly| Num{ .num_poly = .{ .var_ = try instantiateVarWithSubstForTest(store, poly.var_, substitution), .requirements = poly.requirements } },
        .int_poly => |poly| Num{ .int_poly = .{ .var_ = try instantiateVarWithSubstForTest(store, poly.var_, substitution), .requirements = poly.requirements } },
        .frac_poly => |poly| Num{ .frac_poly = .{ .var_ = try instantiateVarWithSubstForTest(store, poly.var_, substitution), .requirements = poly.requirements } },
        // Concrete types remain unchanged
        .int_precision => |precision| Num{ .int_precision = precision },
        .frac_precision => |precision| Num{ .frac_precision = precision },
        .num_compact => |compact| Num{ .num_compact = compact },
        .num_unbound => |requirements| Num{ .num_unbound = requirements },
        .int_unbound => |requirements| Num{ .int_unbound = requirements },
        .frac_unbound => |requirements| Num{ .frac_unbound = requirements },
    };
}

fn instantiateNominalType(
    _: *TypesStore,
    nominal: NominalType,
    _: *VarSubstitution,
) std.mem.Allocator.Error!NominalType {
    // Nominal types (opaque types) use the same variable layout pattern as aliases:
    //   - nominal_var: The nominal type itself
    //   - nominal_var + 1: The backing type variable
    //   - nominal_var + 2, +3, ...: Type argument variables
    //
    // Like aliases, we preserve the nominal type structure during instantiation.
    // The backing type and arguments are instantiated when accessed.

    return nominal;
}

fn instantiateTagUnionForTest(
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
            const fresh_arg = try instantiateVarWithSubstForTest(store, arg_var, substitution);
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

    // Handle the extension variable
    const fresh_ext = try instantiateVarWithSubstForTest(store, tag_union.ext, substitution);

    return TagUnion{
        .tags = tags_range,
        .ext = fresh_ext,
    };
}

fn instantiateFunc(
    ctx: InstantiateContext,
    func: Func,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!Func {
    const args_slice = ctx.store.getFuncArgsSlice(func.args);

    var fresh_args = std.ArrayList(Var).init(ctx.store.gpa);
    defer fresh_args.deinit();

    for (args_slice) |arg_var| {
        const fresh_arg = try instantiateVarWithSubst(ctx, arg_var, substitution, region_map);
        try fresh_args.append(fresh_arg);
    }

    const fresh_ret = try instantiateVarWithSubst(ctx, func.ret, substitution, region_map);
    const fresh_args_range = ctx.store.appendFuncArgs(fresh_args.items);

    return Func{
        .args = fresh_args_range,
        .ret = fresh_ret,
        // Assume it still needs instantiation even after replacing with
        // fresh type variables, because recalculating it just in case it
        // might not does not seem likely to be worth it - especially since
        // any given instantiation will likely never get instantiated again.
        .needs_instantiation = true,
    };
}

fn instantiateFuncForTest(
    store: *TypesStore,
    func: Func,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Func {
    const args_slice = store.getFuncArgsSlice(func.args);

    var fresh_args = std.ArrayList(Var).init(store.gpa);
    defer fresh_args.deinit();

    for (args_slice) |arg_var| {
        const fresh_arg = try instantiateVarWithSubstForTest(store, arg_var, substitution);
        try fresh_args.append(fresh_arg);
    }

    const fresh_ret = try instantiateVarWithSubstForTest(store, func.ret, substitution);
    const fresh_args_range = store.appendFuncArgs(fresh_args.items);

    return Func{
        .args = fresh_args_range,
        .ret = fresh_ret,
        .needs_instantiation = true,
    };
}

fn instantiateRecord(
    ctx: InstantiateContext,
    record: Record,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!Record {
    const fields_start = ctx.store.record_fields.len();
    const fields_slice = ctx.store.getRecordFieldsSlice(record.fields);

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVarWithSubst(ctx, type_var, substitution, region_map);
        _ = ctx.store.record_fields.append(ctx.store.gpa, RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    const fields_range = RecordField.SafeMultiList.Range{
        .start = @enumFromInt(fields_start),
        .end = @enumFromInt(ctx.store.record_fields.len()),
    };

    // Handle the extension variable
    const fresh_ext = try instantiateVarWithSubst(ctx, record.ext, substitution, region_map);

    return Record{
        .fields = fields_range,
        .ext = fresh_ext,
    };
}

fn instantiateRecordForTest(
    store: *TypesStore,
    record: Record,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!Record {
    const fields_start = store.record_fields.len();
    const fields_slice = store.getRecordFieldsSlice(record.fields);

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVarWithSubstForTest(store, type_var, substitution);
        _ = store.record_fields.append(store.gpa, RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    const fields_range = RecordField.SafeMultiList.Range{
        .start = @enumFromInt(fields_start),
        .end = @enumFromInt(store.record_fields.len()),
    };

    // Handle the extension variable
    const fresh_ext = try instantiateVarWithSubstForTest(store, record.ext, substitution);

    return Record{
        .fields = fields_range,
        .ext = fresh_ext,
    };
}

fn instantiateRecordFields(
    ctx: InstantiateContext,
    fields: RecordField.SafeMultiList.Range,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const fields_start = ctx.store.record_fields.len();
    const fields_slice = ctx.store.getRecordFieldsSlice(fields);

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVarWithSubst(ctx, type_var, substitution, region_map);
        _ = ctx.store.record_fields.append(ctx.store.gpa, RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    return RecordField.SafeMultiList.Range{
        .start = @enumFromInt(fields_start),
        .end = @enumFromInt(ctx.store.record_fields.len()),
    };
}

fn instantiateRecordFieldsForTest(
    store: *TypesStore,
    fields: RecordField.SafeMultiList.Range,
    substitution: *VarSubstitution,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const fields_start = store.record_fields.len();
    const fields_slice = store.getRecordFieldsSlice(fields);

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const fresh_type = try instantiateVarWithSubstForTest(store, type_var, substitution);
        _ = store.record_fields.append(store.gpa, RecordField{
            .name = name,
            .var_ = fresh_type,
        });
    }

    return RecordField.SafeMultiList.Range{
        .start = @enumFromInt(fields_start),
        .end = @enumFromInt(store.record_fields.len()),
    };
}

fn instantiateTagUnion(
    ctx: InstantiateContext,
    tag_union: TagUnion,
    substitution: *VarSubstitution,
    region_map: *RegionMap,
) std.mem.Allocator.Error!TagUnion {
    const tags_start = ctx.store.tags.len();
    const tags_slice = ctx.store.getTagsSlice(tag_union.tags);

    for (tags_slice.items(.name), tags_slice.items(.args)) |tag_name, tag_args| {
        var fresh_args = std.ArrayList(Var).init(ctx.store.gpa);
        defer fresh_args.deinit();

        const args_slice = ctx.store.getTagArgsSlice(tag_args);
        for (args_slice) |arg_var| {
            const fresh_arg = try instantiateVarWithSubst(ctx, arg_var, substitution, region_map);
            try fresh_args.append(fresh_arg);
        }

        const fresh_args_range = ctx.store.appendTagArgs(fresh_args.items);

        _ = ctx.store.tags.append(ctx.store.gpa, Tag{
            .name = tag_name,
            .args = fresh_args_range,
        });
    }

    const tags_range = Tag.SafeMultiList.Range{
        .start = @enumFromInt(tags_start),
        .end = @enumFromInt(ctx.store.tags.len()),
    };

    // Handle the extension variable
    const fresh_ext = try instantiateVarWithSubst(ctx, tag_union.ext, substitution, region_map);

    return TagUnion{
        .tags = tags_range,
        .ext = fresh_ext,
    };
}
