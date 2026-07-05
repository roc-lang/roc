//! Cross-module type copying for imports.
//!
//! This module provides functionality to copy types from one module's type store
//! to another module's type store when importing. This ensures each module maintains
//! its own consistent type variable namespace while still being able to use types
//! from other modules.
//!
//! Copying a type across module envs is an identity REBASE boundary: nominal and
//! alias `origin_module` values are env-local indices into the source env's
//! module identity table, so the copy reads the 32-byte content identity hash
//! from the source table and getOrInserts it into the destination table. This is
//! the single cross-env identity resolution mechanism — no name matching.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");

const ModuleEnv = can.ModuleEnv;
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

/// A mapping from source type variables to destination type variables
/// This is only used during the copy operation to ensure consistent mapping
/// of type variables that appear multiple times in the same type structure.
const VarMapping = std.AutoHashMap(Var, Var);

/// All state threaded through a single cross-module copy operation.
const CopyContext = struct {
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    var_mapping: *VarMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,

    fn sourceIdents(self: *const CopyContext) *const base.Ident.Store {
        return self.source_env.getIdentStoreConst();
    }

    fn copyIdent(self: *const CopyContext, source_ident: base.Ident.Idx) std.mem.Allocator.Error!base.Ident.Idx {
        const text = self.sourceIdents().getText(source_ident);
        const source_ident_value = base.Ident.for_text(text);
        const dest_idents = self.dest_env.getIdentStore();
        if (dest_idents.lookup(source_ident_value)) |existing| return existing;
        return try dest_idents.insert(self.allocator, source_ident_value);
    }

    /// Rebase an env-local module identity index from the source env's
    /// identity table into the destination env's table via the 32-byte
    /// content identity hash.
    fn copyOriginModule(self: *const CopyContext, source_origin: base.ModuleIdentity.Idx) std.mem.Allocator.Error!base.ModuleIdentity.Idx {
        const hash = self.source_env.moduleIdentityHash(source_origin);
        if (self.dest_env.lookupModuleIdentity(hash)) |existing| return existing;
        const source_display = self.source_env.moduleIdentityDisplayIdent(source_origin);
        const display = if (source_display.isNone())
            base.Ident.Idx.NONE
        else
            try self.copyIdent(source_display);
        return try self.dest_env.internModuleIdentity(hash, display);
    }
};

/// Copy a type from one module's type store to another module's type store.
/// This creates a completely fresh copy with new variable indices in the destination store.
///
/// Imported identifiers are interned directly into the destination module's
/// authoritative identifier store so all copied types in that module reference
/// one consistent `Ident.Store`; imported module identities are rebased into
/// the destination module's identity table the same way.
pub fn copyVar(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    const ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
    };
    return copyVarCtx(&ctx, source_var);
}

fn copyVarCtx(ctx: *const CopyContext, source_var: Var) std.mem.Allocator.Error!Var {
    const resolved = ctx.source_store.resolveVar(source_var);

    if (ctx.var_mapping.get(resolved.var_)) |dest_var| {
        return dest_var;
    }

    const placeholder_var = try ctx.dest_store.fresh();
    try ctx.var_mapping.put(resolved.var_, placeholder_var);

    const dest_content = try copyContent(ctx, resolved.desc.content);

    try ctx.dest_store.dangerousSetVarDesc(placeholder_var, .{
        .content = dest_content,
        .rank = types_mod.Rank.generalized,
    });

    // NOTE: a copied var whose content is a flex carrying a literal-conversion
    // constraint is an open literal in the destination module. Registering it on
    // the checker's open-literal worklist is the CALLER's job (see `Check.copyVar`,
    // which walks `var_mapping` after the copy) — this module only copies type
    // data between stores.
    return placeholder_var;
}

fn copyContent(ctx: *const CopyContext, content: Content) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex => |flex| Content{ .flex = try copyFlex(ctx, flex) },
        .rigid => |rigid| Content{ .rigid = try copyRigid(ctx, rigid) },
        .alias => |alias| Content{ .alias = try copyAlias(ctx, alias) },
        .structure => |flat_type| Content{ .structure = try copyFlatType(ctx, flat_type) },
        .err => Content.err,
    };
}

fn copyFlex(ctx: *const CopyContext, source_flex: Flex) std.mem.Allocator.Error!Flex {
    const mb_translated_name = if (source_flex.name) |name_ident|
        try ctx.copyIdent(name_ident)
    else
        null;

    const dest_constraints_range = try copyStaticDispatchConstraints(ctx, source_flex.constraints);

    return Flex{
        .name = mb_translated_name,
        .constraints = dest_constraints_range,
    };
}

fn copyRigid(ctx: *const CopyContext, source_rigid: Rigid) std.mem.Allocator.Error!Rigid {
    const translated_name = try ctx.copyIdent(source_rigid.name);

    const dest_constraints_range = try copyStaticDispatchConstraints(ctx, source_rigid.constraints);

    return Rigid{
        .name = translated_name,
        .constraints = dest_constraints_range,
    };
}

fn copyAlias(ctx: *const CopyContext, source_alias: Alias) std.mem.Allocator.Error!Alias {
    const translated_ident = try ctx.copyIdent(source_alias.ident.ident_idx);

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(ctx.dest_store.gpa);

    const origin_backing = ctx.source_store.getAliasBackingVar(source_alias);
    const dest_backing = try copyVarCtx(ctx, origin_backing);
    try dest_args.append(ctx.dest_store.gpa, dest_backing);

    const origin_args = ctx.source_store.sliceAliasArgs(source_alias);
    for (origin_args) |arg_var| {
        const dest_arg = try copyVarCtx(ctx, arg_var);
        try dest_args.append(ctx.dest_store.gpa, dest_arg);
    }

    const dest_vars_span = try ctx.dest_store.appendVars(dest_args.items);
    const translated_origin = try ctx.copyOriginModule(source_alias.origin_module);

    return Alias{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .vars = .{ .nonempty = dest_vars_span },
        .origin_module = translated_origin,
        .source_decl = source_alias.source_decl,
    };
}

fn copyFlatType(ctx: *const CopyContext, flat_type: FlatType) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .tuple => |tuple| FlatType{ .tuple = try copyTuple(ctx, tuple) },
        .nominal_type => |nominal| FlatType{ .nominal_type = try copyNominalType(ctx, nominal) },
        .fn_pure => |func| FlatType{ .fn_pure = try copyFunc(ctx, func) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try copyFunc(ctx, func) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try copyFunc(ctx, func) },
        .record => |record| FlatType{ .record = try copyRecord(ctx, record) },
        .tag_union => |tag_union| FlatType{ .tag_union = try copyTagUnion(ctx, tag_union) },
        .record_unbound => |fields| FlatType{ .record_unbound = try copyRecordFields(ctx, fields) },
        .empty_record => FlatType.empty_record,
        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn copyTuple(ctx: *const CopyContext, tuple: types_mod.Tuple) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = ctx.source_store.sliceVars(tuple.elems);

    var dest_elems = std.ArrayList(Var).empty;
    defer dest_elems.deinit(ctx.dest_store.gpa);

    for (elems_slice) |elem_var| {
        const dest_elem = try copyVarCtx(ctx, elem_var);
        try dest_elems.append(ctx.dest_store.gpa, dest_elem);
    }

    const dest_range = try ctx.dest_store.appendVars(dest_elems.items);
    return types_mod.Tuple{ .elems = dest_range };
}

fn copyFunc(ctx: *const CopyContext, func: Func) std.mem.Allocator.Error!Func {
    const args_slice = ctx.source_store.sliceVars(func.args);

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(ctx.dest_store.gpa);

    for (args_slice) |arg_var| {
        const dest_arg = try copyVarCtx(ctx, arg_var);
        try dest_args.append(ctx.dest_store.gpa, dest_arg);
    }

    const dest_ret = try copyVarCtx(ctx, func.ret);

    const dest_args_range = try ctx.dest_store.appendVars(dest_args.items);
    return Func{
        .args = dest_args_range,
        .ret = dest_ret,
        .needs_instantiation = func.needs_instantiation,
    };
}

fn copyRecordFields(
    ctx: *const CopyContext,
    fields_range: types_mod.RecordField.SafeMultiList.Range,
) std.mem.Allocator.Error!types_mod.RecordField.SafeMultiList.Range {
    const source_fields = ctx.source_store.getRecordFieldsSlice(fields_range);

    var fresh_fields = std.ArrayList(RecordField).empty;
    defer fresh_fields.deinit(ctx.allocator);

    for (source_fields.items(.name), source_fields.items(.var_)) |name, var_| {
        const translated_name = try ctx.copyIdent(name);
        try fresh_fields.append(ctx.allocator, .{
            .name = translated_name,
            .var_ = try copyVarCtx(ctx, var_),
        });
    }

    return try ctx.dest_store.appendRecordFields(fresh_fields.items);
}

fn copyRecord(ctx: *const CopyContext, record: Record) std.mem.Allocator.Error!Record {
    const fields_range = try copyRecordFields(ctx, record.fields);

    return Record{
        .fields = fields_range,
        .ext = try copyVarCtx(ctx, record.ext),
    };
}

fn copyTagUnion(ctx: *const CopyContext, tag_union: TagUnion) std.mem.Allocator.Error!TagUnion {
    const tags_slice = ctx.source_store.getTagsSlice(tag_union.tags);

    var fresh_tags = std.ArrayList(Tag).empty;
    defer fresh_tags.deinit(ctx.allocator);

    for (tags_slice.items(.name), tags_slice.items(.args)) |name, args_range| {
        const args_slice = ctx.source_store.sliceVars(args_range);

        var dest_args = std.ArrayList(Var).empty;
        defer dest_args.deinit(ctx.dest_store.gpa);

        for (args_slice) |arg_var| {
            const dest_arg = try copyVarCtx(ctx, arg_var);
            try dest_args.append(ctx.dest_store.gpa, dest_arg);
        }

        const dest_args_range = try ctx.dest_store.appendVars(dest_args.items);
        const translated_name = try ctx.copyIdent(name);

        try fresh_tags.append(ctx.allocator, .{
            .name = translated_name,
            .args = dest_args_range,
        });
    }

    const tags_range = try ctx.dest_store.appendTags(fresh_tags.items);
    return TagUnion{
        .tags = tags_range,
        .ext = try copyVarCtx(ctx, tag_union.ext),
    };
}

fn copyNominalType(ctx: *const CopyContext, source_nominal: NominalType) std.mem.Allocator.Error!NominalType {
    const translated_ident = try ctx.copyIdent(source_nominal.ident.ident_idx);
    const translated_origin = try ctx.copyOriginModule(source_nominal.origin_module);

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(ctx.dest_store.gpa);

    const origin_backing = ctx.source_store.getNominalBackingVar(source_nominal);
    const dest_backing = try copyVarCtx(ctx, origin_backing);
    try dest_args.append(ctx.dest_store.gpa, dest_backing);

    const origin_args = ctx.source_store.sliceNominalArgs(source_nominal);
    for (origin_args) |arg_var| {
        const dest_arg = try copyVarCtx(ctx, arg_var);
        try dest_args.append(ctx.dest_store.gpa, dest_arg);
    }

    const dest_vars_span = try ctx.dest_store.appendVars(dest_args.items);

    return NominalType{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .vars = .{ .nonempty = dest_vars_span },
        .origin_module = translated_origin,
        .source = source_nominal.source,
    };
}

fn copyStaticDispatchConstraints(
    ctx: *const CopyContext,
    source_constraints: StaticDispatchConstraint.SafeList.Range,
) std.mem.Allocator.Error!StaticDispatchConstraint.SafeList.Range {
    const source_constraints_len = source_constraints.len();
    if (source_constraints_len == 0) {
        return StaticDispatchConstraint.SafeList.Range.empty();
    }

    var dest_constraints = try std.array_list.Managed(StaticDispatchConstraint).initCapacity(ctx.dest_store.gpa, source_constraints_len);
    defer dest_constraints.deinit();

    for (ctx.source_store.sliceStaticDispatchConstraints(source_constraints)) |source_constraint| {
        const translated_fn_name = try ctx.copyIdent(source_constraint.fn_name);

        var dest_constraint = source_constraint;
        dest_constraint.fn_name = translated_fn_name;
        dest_constraint.fn_var = try copyVarCtx(ctx, source_constraint.fn_var);

        try dest_constraints.append(dest_constraint);
    }

    return try ctx.dest_store.appendStaticDispatchConstraints(dest_constraints.items);
}
