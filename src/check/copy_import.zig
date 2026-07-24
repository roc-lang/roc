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
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");

const ModuleEnv = can.ModuleEnv;
const TypesStore = types_mod.Store;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const InterpolationPartMetadata = types_mod.InterpolationPartMetadata;
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

    var dest_effect_deps = std.ArrayList(Var).empty;
    defer dest_effect_deps.deinit(ctx.dest_store.gpa);
    for (ctx.source_store.sliceVars(func.effect_deps)) |effect_dep| {
        try dest_effect_deps.append(ctx.dest_store.gpa, try copyVarCtx(ctx, effect_dep));
    }

    const dest_args_range = try ctx.dest_store.appendVars(dest_args.items);
    const dest_effect_deps_range = try ctx.dest_store.appendVars(dest_effect_deps.items);
    return Func{
        .args = dest_args_range,
        .ret = dest_ret,
        .effect_deps = dest_effect_deps_range,
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

    try ensureNominalDeclCopied(ctx, source_nominal, translated_origin);

    var dest_args = std.ArrayList(Var).empty;
    defer dest_args.deinit(ctx.dest_store.gpa);

    const origin_args = ctx.source_store.sliceNominalArgs(source_nominal);
    for (origin_args) |arg_var| {
        const dest_arg = try copyVarCtx(ctx, arg_var);
        try dest_args.append(ctx.dest_store.gpa, dest_arg);
    }

    const dest_args_range = try ctx.dest_store.appendVars(dest_args.items);

    return NominalType{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .args = dest_args_range,
        .origin_module = translated_origin,
        .source = source_nominal.source,
    };
}

/// Ensure the destination store's nominal declaration table has an entry for
/// the declaration behind `source_nominal`, copying it from the source store's
/// table on first encounter. This runs once per (destination module,
/// declaration): every later application of the same declaration finds the
/// key already present and returns immediately, so declaration data crosses a
/// module boundary at most once regardless of how many applications do.
///
/// The entry is reserved (key registered) before its formals and backing are
/// copied so that self-referential backing templates terminate: copying the
/// template's own recursive application re-enters this function and finds the
/// key already present.
fn ensureNominalDeclCopied(
    ctx: *const CopyContext,
    source_nominal: NominalType,
    translated_origin: base.ModuleIdentity.Idx,
) std.mem.Allocator.Error!void {
    const source_decl = source_nominal.sourceDecl();
    // A nominal without a source declaration has no key and no declaration
    // table entry (only possible for hand-constructed types in tests).
    if (!source_decl.present) return;

    if (ctx.dest_store.lookupNominalDeclByKey(translated_origin, source_decl.statement) != null) return;

    const source_decl_idx = ctx.source_store.lookupNominalDecl(source_nominal) orelse {
        // Invariant: every nominal application in a store can resolve its
        // declaration in that store, so a keyed application without a source
        // table entry is a compiler bug.
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "copy_import invariant violated: nominal '{s}' has a source declaration but no declaration table entry in its source store",
                .{ctx.sourceIdents().getText(source_nominal.ident.ident_idx)},
            );
        }
        unreachable;
    };

    try copyNominalDeclEntry(ctx, ctx.source_store.getNominalDecl(source_decl_idx), translated_origin);
}

/// Ensure the destination store has a declaration-table entry for the nominal
/// declaration at `statement` in the source module env, keyed under the source
/// module's own identity rebased into the destination env. No-op when the
/// source store has no entry for that statement (e.g. an alias declaration) or
/// when the destination already has one.
///
/// Newly created destination vars are recorded in `var_mapping`; the caller
/// owns follow-up bookkeeping for them (regions, worklists), exactly as with
/// `copyVar`.
pub fn ensureNominalDeclForStatement(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    statement: u32,
    var_mapping: *VarMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    const ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
    };

    const source_origin = source_env.selfModuleIdentity();
    const source_decl_idx = source_store.lookupNominalDeclByKey(source_origin, statement) orelse return;
    const translated_origin = try ctx.copyOriginModule(source_origin);
    if (dest_store.lookupNominalDeclByKey(translated_origin, statement) != null) return;

    try copyNominalDeclEntry(&ctx, source_store.getNominalDecl(source_decl_idx), translated_origin);
}

/// Copy one declaration-table entry (formals + backing template) into the
/// destination store. The key is reserved before the graph copy so that
/// self-referential backing templates terminate: copying the template's own
/// recursive application re-enters `ensureNominalDeclCopied` and finds the key
/// already present. Nothing reads the reserved entry's formals/backing while
/// the copy is in flight — lookups only test key presence.
fn copyNominalDeclEntry(
    ctx: *const CopyContext,
    source_entry: types_mod.NominalDecl,
    translated_origin: base.ModuleIdentity.Idx,
) std.mem.Allocator.Error!void {
    const translated_ident = try ctx.copyIdent(source_entry.ident.ident_idx);
    const reserved_idx = try ctx.dest_store.registerNominalDecl(.{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .origin_module = translated_origin,
        .source = source_entry.source,
        .formals = Var.SafeList.Range.empty(),
        // Never read while the copy is in flight (see above); both fields are
        // filled in below once the graph copy completes.
        .backing = undefined,
        .flags = source_entry.flags,
    });

    var dest_formals = std.ArrayList(Var).empty;
    defer dest_formals.deinit(ctx.dest_store.gpa);
    const source_formals = ctx.source_store.sliceVars(source_entry.formals);
    for (source_formals) |source_formal| {
        const dest_formal = try copyVarCtx(ctx, source_formal);
        try dest_formals.append(ctx.dest_store.gpa, dest_formal);
    }
    const dest_formals_range = try ctx.dest_store.appendVars(dest_formals.items);

    const dest_backing = try copyVarCtx(ctx, source_entry.backing);

    var dest_entry = ctx.dest_store.getNominalDecl(reserved_idx);
    dest_entry.formals = dest_formals_range;
    dest_entry.backing = dest_backing;
    ctx.dest_store.setNominalDecl(reserved_idx, dest_entry);
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
        dest_constraint.interpolation = try copyInterpolationMetadata(ctx, source_constraint.interpolation);
        if (source_constraint.derived_map_plan) |plan| {
            dest_constraint.derived_map_plan = .{
                .tag_name = try ctx.copyIdent(plan.tag_name),
                .payload_index = plan.payload_index,
            };
        }
        // Provenance (introducing expression + expect region) is module-scoped:
        // its indices refer to the SOURCE module's CIR and are meaningless here.
        // Clear it on the boundary crossing so a consumer never dereferences a
        // foreign expr index against the destination module (the old module-local
        // side tables likewise had no entry for an imported constraint).
        dest_constraint.provenance = .{};

        try dest_constraints.append(dest_constraint);
    }

    return try ctx.dest_store.appendStaticDispatchConstraints(dest_constraints.items);
}

fn copyInterpolationMetadata(
    ctx: *const CopyContext,
    source_metadata: StaticDispatchConstraint.InterpolationMetadata,
) std.mem.Allocator.Error!StaticDispatchConstraint.InterpolationMetadata {
    if (!source_metadata.isPresent()) return source_metadata;

    const source_parts = ctx.source_store.sliceInterpolationParts(source_metadata.interpolated_parts);
    var dest_parts = try std.ArrayList(InterpolationPartMetadata).initCapacity(ctx.dest_store.gpa, source_parts.len);
    defer dest_parts.deinit(ctx.dest_store.gpa);

    for (source_parts) |source_part| {
        dest_parts.appendAssumeCapacity(.{
            .var_ = try copyVarCtx(ctx, source_part.var_),
            .region = source_part.region,
        });
    }

    return .{
        .expr_region = source_metadata.expr_region,
        .item_var = try copyVarCtx(ctx, source_metadata.item_var),
        .interpolated_parts = try ctx.dest_store.appendInterpolationParts(dest_parts.items),
    };
}
