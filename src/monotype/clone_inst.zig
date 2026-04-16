//! Specialization-local clone-inst of solved source vars into monotype scope state.
//!
//! This is the monotype-owned analogue of cor's clone-inst step:
//! copy only the reachable solved source type graph needed by the current
//! specialization into local mutable scope state, while preserving shared
//! structure through an explicit var map.

const std = @import("std");
const base = @import("base");
const types_mod = @import("types");
pub const TypeKey = struct {
    module_idx: u32,
    var_: Var,
};

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
const NominalType = types_mod.NominalType;

pub const VarMapping = std.AutoHashMap(Var, Var);
pub const ScopedCloneMap = std.AutoHashMap(TypeKey, Var);

fn copyIdent(
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    source_ident: base.Ident.Idx,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!base.Ident.Idx {
    const text = source_idents.getText(source_ident);
    const ident_value = base.Ident.for_text(text);
    if (dest_idents.lookup(ident_value)) |existing| return existing;
    return try dest_idents.insert(allocator, ident_value);
}

pub fn cloneVar(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    const resolved = source_store.resolveVar(source_var);
    if (var_mapping.get(resolved.var_)) |dest_var| return dest_var;

    const placeholder_var = try dest_store.fresh();
    try var_mapping.put(resolved.var_, placeholder_var);

    const dest_content = try copyContent(
        source_store,
        dest_store,
        resolved.desc.content,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    );

    try dest_store.dangerousSetVarDesc(placeholder_var, .{
        .content = dest_content,
        .rank = types_mod.Rank.generalized,
    });

    return placeholder_var;
}

pub fn cloneVarFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    const resolved = source_store.resolveVar(source_var);
    const key: TypeKey = .{
        .module_idx = source_module_idx,
        .var_ = resolved.var_,
    };
    if (var_mapping.get(key)) |dest_var| return dest_var;

    const placeholder_var = try dest_store.fresh();
    try var_mapping.put(key, placeholder_var);

    const dest_content = try copyContentFromModule(
        source_module_idx,
        source_store,
        dest_store,
        resolved.desc.content,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    );

    try dest_store.dangerousSetVarDesc(placeholder_var, .{
        .content = dest_content,
        .rank = types_mod.Rank.generalized,
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
        .flex => |flex| .{ .flex = try copyFlex(source_store, dest_store, flex, var_mapping, source_idents, dest_idents, allocator) },
        .rigid => |rigid| .{ .rigid = try copyRigid(source_store, dest_store, rigid, var_mapping, source_idents, dest_idents, allocator) },
        .alias => |alias| .{ .alias = try copyAlias(source_store, dest_store, alias, var_mapping, source_idents, dest_idents, allocator) },
        .structure => |flat_type| .{ .structure = try copyFlatType(source_store, dest_store, flat_type, var_mapping, source_idents, dest_idents, allocator) },
        .err => .err,
    };
}

fn copyContentFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    content: Content,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex => |flex| .{ .flex = try copyFlexFromModule(source_module_idx, source_store, dest_store, flex, var_mapping, source_idents, dest_idents, allocator) },
        .rigid => |rigid| .{ .rigid = try copyRigidFromModule(source_module_idx, source_store, dest_store, rigid, var_mapping, source_idents, dest_idents, allocator) },
        .alias => |alias| .{ .alias = try copyAliasFromModule(source_module_idx, source_store, dest_store, alias, var_mapping, source_idents, dest_idents, allocator) },
        .structure => |flat_type| .{ .structure = try copyFlatTypeFromModule(source_module_idx, source_store, dest_store, flat_type, var_mapping, source_idents, dest_idents, allocator) },
        .err => .err,
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
    const translated_name = if (source_flex.name) |name_ident|
        try copyIdent(source_idents, dest_idents, name_ident, allocator)
    else
        null;

    return .{
        .name = translated_name,
        .constraints = try copyStaticDispatchConstraints(
            source_store,
            dest_store,
            source_flex.constraints,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ),
    };
}

fn copyFlexFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_flex: Flex,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Flex {
    const translated_name = if (source_flex.name) |name_ident|
        try copyIdent(source_idents, dest_idents, name_ident, allocator)
    else
        null;

    return .{
        .name = translated_name,
        .constraints = try copyStaticDispatchConstraintsFromModule(
            source_module_idx,
            source_store,
            dest_store,
            source_flex.constraints,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ),
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
    return .{
        .name = try copyIdent(source_idents, dest_idents, source_rigid.name, allocator),
        .constraints = try copyStaticDispatchConstraints(
            source_store,
            dest_store,
            source_rigid.constraints,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ),
    };
}

fn copyRigidFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_rigid: Rigid,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Rigid {
    return .{
        .name = try copyIdent(source_idents, dest_idents, source_rigid.name, allocator),
        .constraints = try copyStaticDispatchConstraintsFromModule(
            source_module_idx,
            source_store,
            dest_store,
            source_rigid.constraints,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ),
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
    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    const origin_backing = source_store.getAliasBackingVar(source_alias);
    try dest_args.append(dest_store.gpa, try cloneVar(
        source_store,
        dest_store,
        origin_backing,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    ));

    const origin_args = source_store.sliceAliasArgs(source_alias);
    for (origin_args) |arg_var| {
        try dest_args.append(dest_store.gpa, try cloneVar(
            source_store,
            dest_store,
            arg_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{
        .ident = .{ .ident_idx = try copyIdent(source_idents, dest_idents, source_alias.ident.ident_idx, allocator) },
        .vars = .{ .nonempty = try dest_store.appendVars(dest_args.items) },
        .origin_module = try copyIdent(source_idents, dest_idents, source_alias.origin_module, allocator),
    };
}

fn copyAliasFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_alias: Alias,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Alias {
    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    const origin_backing = source_store.getAliasBackingVar(source_alias);
    try dest_args.append(dest_store.gpa, try cloneVarFromModule(
        source_module_idx,
        source_store,
        dest_store,
        origin_backing,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    ));

    const origin_args = source_store.sliceAliasArgs(source_alias);
    for (origin_args) |arg_var| {
        try dest_args.append(dest_store.gpa, try cloneVarFromModule(
            source_module_idx,
            source_store,
            dest_store,
            arg_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{
        .ident = .{ .ident_idx = try copyIdent(source_idents, dest_idents, source_alias.ident.ident_idx, allocator) },
        .vars = .{ .nonempty = try dest_store.appendVars(dest_args.items) },
        .origin_module = try copyIdent(source_idents, dest_idents, source_alias.origin_module, allocator),
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
        .tuple => |tuple| .{ .tuple = try copyTuple(source_store, dest_store, tuple, var_mapping, source_idents, dest_idents, allocator) },
        .nominal_type => |nominal| .{ .nominal_type = try copyNominalType(source_store, dest_store, nominal, var_mapping, source_idents, dest_idents, allocator) },
        .fn_pure => |func| .{ .fn_pure = try copyFunc(source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .fn_effectful => |func| .{ .fn_effectful = try copyFunc(source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .fn_unbound => |func| .{ .fn_unbound = try copyFunc(source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .record => |record| .{ .record = try copyRecord(source_store, dest_store, record, var_mapping, source_idents, dest_idents, allocator) },
        .tag_union => |tag_union| .{ .tag_union = try copyTagUnion(source_store, dest_store, tag_union, var_mapping, source_idents, dest_idents, allocator) },
        .record_unbound => |fields| .{ .record_unbound = try copyRecordFields(source_store, dest_store, fields, var_mapping, source_idents, dest_idents, allocator) },
        .empty_record => .empty_record,
        .empty_tag_union => .empty_tag_union,
    };
}

fn copyFlatTypeFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    flat_type: FlatType,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .tuple => |tuple| .{ .tuple = try copyTupleFromModule(source_module_idx, source_store, dest_store, tuple, var_mapping, source_idents, dest_idents, allocator) },
        .nominal_type => |nominal| .{ .nominal_type = try copyNominalTypeFromModule(source_module_idx, source_store, dest_store, nominal, var_mapping, source_idents, dest_idents, allocator) },
        .fn_pure => |func| .{ .fn_pure = try copyFuncFromModule(source_module_idx, source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .fn_effectful => |func| .{ .fn_effectful = try copyFuncFromModule(source_module_idx, source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .fn_unbound => |func| .{ .fn_unbound = try copyFuncFromModule(source_module_idx, source_store, dest_store, func, var_mapping, source_idents, dest_idents, allocator) },
        .record => |record| .{ .record = try copyRecordFromModule(source_module_idx, source_store, dest_store, record, var_mapping, source_idents, dest_idents, allocator) },
        .tag_union => |tag_union| .{ .tag_union = try copyTagUnionFromModule(source_module_idx, source_store, dest_store, tag_union, var_mapping, source_idents, dest_idents, allocator) },
        .record_unbound => |fields| .{ .record_unbound = try copyRecordFieldsFromModule(source_module_idx, source_store, dest_store, fields, var_mapping, source_idents, dest_idents, allocator) },
        .empty_record => .empty_record,
        .empty_tag_union => .empty_tag_union,
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
        try dest_elems.append(dest_store.gpa, try cloneVar(
            source_store,
            dest_store,
            elem_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{ .elems = try dest_store.appendVars(dest_elems.items) };
}

fn copyTupleFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    tuple: types_mod.Tuple,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = source_store.sliceVars(tuple.elems);
    var dest_elems = std.ArrayList(Var).empty;
    defer dest_elems.deinit(dest_store.gpa);

    for (elems_slice) |elem_var| {
        try dest_elems.append(dest_store.gpa, try cloneVarFromModule(
            source_module_idx,
            source_store,
            dest_store,
            elem_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{ .elems = try dest_store.appendVars(dest_elems.items) };
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
        try dest_args.append(dest_store.gpa, try cloneVar(
            source_store,
            dest_store,
            arg_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{
        .args = try dest_store.appendVars(dest_args.items),
        .ret = try cloneVar(source_store, dest_store, func.ret, var_mapping, source_idents, dest_idents, allocator),
        .needs_instantiation = func.needs_instantiation,
    };
}

fn copyFuncFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    func: Func,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Func {
    const args_slice = source_store.sliceVars(func.args);
    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    for (args_slice) |arg_var| {
        try dest_args.append(dest_store.gpa, try cloneVarFromModule(
            source_module_idx,
            source_store,
            dest_store,
            arg_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{
        .args = try dest_store.appendVars(dest_args.items),
        .ret = try cloneVarFromModule(source_module_idx, source_store, dest_store, func.ret, var_mapping, source_idents, dest_idents, allocator),
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
        try fresh_fields.append(allocator, .{
            .name = try copyIdent(source_idents, dest_idents, name, allocator),
            .var_ = try cloneVar(source_store, dest_store, var_, var_mapping, source_idents, dest_idents, allocator),
        });
    }

    return try dest_store.appendRecordFields(fresh_fields.items);
}

fn copyRecordFieldsFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!types_mod.RecordField.SafeMultiList.Range {
    const source_fields = source_store.getRecordFieldsSlice(fields_range);
    var fresh_fields = std.ArrayList(RecordField).empty;
    defer fresh_fields.deinit(allocator);

    for (source_fields.items(.name), source_fields.items(.var_)) |name, var_| {
        try fresh_fields.append(allocator, .{
            .name = try copyIdent(source_idents, dest_idents, name, allocator),
            .var_ = try cloneVarFromModule(source_module_idx, source_store, dest_store, var_, var_mapping, source_idents, dest_idents, allocator),
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
    return .{
        .fields = try copyRecordFields(source_store, dest_store, record.fields, var_mapping, source_idents, dest_idents, allocator),
        .ext = try cloneVar(source_store, dest_store, record.ext, var_mapping, source_idents, dest_idents, allocator),
    };
}

fn copyRecordFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    record: Record,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Record {
    return .{
        .fields = try copyRecordFieldsFromModule(source_module_idx, source_store, dest_store, record.fields, var_mapping, source_idents, dest_idents, allocator),
        .ext = try cloneVarFromModule(source_module_idx, source_store, dest_store, record.ext, var_mapping, source_idents, dest_idents, allocator),
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
            try dest_args.append(dest_store.gpa, try cloneVar(
                source_store,
                dest_store,
                arg_var,
                var_mapping,
                source_idents,
                dest_idents,
                allocator,
            ));
        }

        try fresh_tags.append(allocator, .{
            .name = try copyIdent(source_idents, dest_idents, name, allocator),
            .args = try dest_store.appendVars(dest_args.items),
        });
    }

    return .{
        .tags = try dest_store.appendTags(fresh_tags.items),
        .ext = try cloneVar(source_store, dest_store, tag_union.ext, var_mapping, source_idents, dest_idents, allocator),
    };
}

fn copyTagUnionFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    tag_union: TagUnion,
    var_mapping: *ScopedCloneMap,
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
            try dest_args.append(dest_store.gpa, try cloneVarFromModule(
                source_module_idx,
                source_store,
                dest_store,
                arg_var,
                var_mapping,
                source_idents,
                dest_idents,
                allocator,
            ));
        }

        try fresh_tags.append(allocator, .{
            .name = try copyIdent(source_idents, dest_idents, name, allocator),
            .args = try dest_store.appendVars(dest_args.items),
        });
    }

    return .{
        .tags = try dest_store.appendTags(fresh_tags.items),
        .ext = try cloneVarFromModule(source_module_idx, source_store, dest_store, tag_union.ext, var_mapping, source_idents, dest_idents, allocator),
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
    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    const origin_backing = source_store.getNominalBackingVar(source_nominal);
    try dest_args.append(dest_store.gpa, try cloneVar(
        source_store,
        dest_store,
        origin_backing,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    ));

    const origin_args = source_store.sliceNominalArgs(source_nominal);
    for (origin_args) |arg_var| {
        try dest_args.append(dest_store.gpa, try cloneVar(
            source_store,
            dest_store,
            arg_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{
        .ident = .{ .ident_idx = try copyIdent(source_idents, dest_idents, source_nominal.ident.ident_idx, allocator) },
        .vars = .{ .nonempty = try dest_store.appendVars(dest_args.items) },
        .origin_module = try copyIdent(source_idents, dest_idents, source_nominal.origin_module, allocator),
        .is_opaque = source_nominal.is_opaque,
    };
}

fn copyNominalTypeFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_nominal: NominalType,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!NominalType {
    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(dest_store.gpa);

    const origin_backing = source_store.getNominalBackingVar(source_nominal);
    try dest_args.append(dest_store.gpa, try cloneVarFromModule(
        source_module_idx,
        source_store,
        dest_store,
        origin_backing,
        var_mapping,
        source_idents,
        dest_idents,
        allocator,
    ));

    const origin_args = source_store.sliceNominalArgs(source_nominal);
    for (origin_args) |arg_var| {
        try dest_args.append(dest_store.gpa, try cloneVarFromModule(
            source_module_idx,
            source_store,
            dest_store,
            arg_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        ));
    }

    return .{
        .ident = .{ .ident_idx = try copyIdent(source_idents, dest_idents, source_nominal.ident.ident_idx, allocator) },
        .vars = .{ .nonempty = try dest_store.appendVars(dest_args.items) },
        .origin_module = try copyIdent(source_idents, dest_idents, source_nominal.origin_module, allocator),
        .is_opaque = source_nominal.is_opaque,
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
    if (source_constraints.len() == 0) return StaticDispatchConstraint.SafeList.Range.empty();

    var dest_constraints = try std.array_list.Managed(StaticDispatchConstraint).initCapacity(dest_store.gpa, source_constraints.len());
    defer dest_constraints.deinit();

    for (source_store.sliceStaticDispatchConstraints(source_constraints)) |source_constraint| {
        var dest_constraint = source_constraint;
        dest_constraint.fn_name = try copyIdent(source_idents, dest_idents, source_constraint.fn_name, allocator);
        dest_constraint.fn_var = try cloneVar(
            source_store,
            dest_store,
            source_constraint.fn_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        );
        try dest_constraints.append(dest_constraint);
    }

    return try dest_store.appendStaticDispatchConstraints(dest_constraints.items);
}

fn copyStaticDispatchConstraintsFromModule(
    source_module_idx: u32,
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_constraints: StaticDispatchConstraint.SafeList.Range,
    var_mapping: *ScopedCloneMap,
    source_idents: *const base.Ident.Store,
    dest_idents: *base.Ident.Store,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
    if (source_constraints.len() == 0) return StaticDispatchConstraint.SafeList.Range.empty();

    var dest_constraints = try std.array_list.Managed(StaticDispatchConstraint).initCapacity(dest_store.gpa, source_constraints.len());
    defer dest_constraints.deinit();

    for (source_store.sliceStaticDispatchConstraints(source_constraints)) |source_constraint| {
        var dest_constraint = source_constraint;
        dest_constraint.fn_name = try copyIdent(source_idents, dest_idents, source_constraint.fn_name, allocator);
        dest_constraint.fn_var = try cloneVarFromModule(
            source_module_idx,
            source_store,
            dest_store,
            source_constraint.fn_var,
            var_mapping,
            source_idents,
            dest_idents,
            allocator,
        );
        try dest_constraints.append(dest_constraint);
    }

    return try dest_store.appendStaticDispatchConstraints(dest_constraints.items);
}
