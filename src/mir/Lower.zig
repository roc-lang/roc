//! CIR → strongest-form MIR lowering.
//!
//! Converts polymorphic, sugar-rich CIR into monomorphic, statement-only MIR.
//! Callable instantiation must already be decided by explicit monomorphization
//! before this pass runs.
//!
//! Key transformations:
//! - `e_if` lowers directly to explicit Bool `switch_stmt`
//! - nested value expressions lower into explicit local-defining statements
//! - `e_nominal` strips to backing value plus explicit nominal assignment
//! - `e_closure` lowers to explicit lambda + capture locals
//! - global names appear only through `assign_symbol`

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const types = @import("types");

const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");
const Monomorphize = @import("Monomorphize.zig");

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Self = @This();

const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
    module_idx: ?u32 = null,
};

const SymbolMetadata = union(enum) {
    local_ident: struct {
        module_idx: u32,
        ident_idx: Ident.Idx,
    },
    external_def: struct {
        module_idx: u32,
        def_node_idx: u16,
        display_ident_idx: Ident.Idx,
    },
};

const PatternBinding = struct {
    ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
};

const RecursiveGroupMember = struct {
    binding_pattern: CIR.Pattern.Idx,
    callable_inst_id: Monomorphize.CallableInstId,
};

const ClosureLowerPlan = struct {
    recursive_members: std.ArrayList(RecursiveGroupMember),

    fn deinit(self: *ClosureLowerPlan, allocator: Allocator) void {
        self.recursive_members.deinit(allocator);
    }
};

const LoopContext = struct {
    exit_id: MIR.JoinPointId,
    carried_locals: MIR.LocalSpan,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

// --- Fields ---

allocator: Allocator,

/// Target MIR store
store: *MIR.Store,

/// Explicit callable-instantiation decisions computed before lowering.
monomorphization: *const Monomorphize.Result,

/// All module environments (indexed by module_idx)
all_module_envs: []const *ModuleEnv,

/// Types store for resolving type variables
types_store: *const types.Store,

/// Current module being lowered
current_module_idx: u32,

/// Scope key used to make local pattern symbols specialization-specific.
/// 0 means unscoped (module/pattern only).
current_pattern_scope: u64,

/// App module index (for resolving `e_lookup_required` from platform modules)
app_module_idx: ?u32,

/// Optional for-clause type substitutions for lowering platform modules
/// against concrete app types.
type_scope: ?*const types.TypeScope,
type_scope_module_idx: ?u32,
type_scope_caller_module_idx: ?u32,

/// Map from ((scope_key << 64) | (module_idx << 32 | CIR.Pattern.Idx)) → MIR.LocalId
/// Used to resolve CIR local lookups to executable MIR locals.
pattern_symbols: std.AutoHashMap(u128, MIR.LocalId),

/// Specialization bindings: maps polymorphic type vars to concrete monotypes.
/// Written by `bindTypeVarMonotypes`, read by `fromTypeVar`.
type_var_seen: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cycle breakers for recursive nominal types (e.g. Tree := [Leaf, Node(Tree)]).
/// Used only by `fromNominalType` during monotype construction; separate from
/// specialization bindings so monotype construction never pollutes them.
nominal_cycle_breakers: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cache for callable bodies already lowered for callable instances chosen by monomorphization.
/// The callable value itself is rebuilt per use-site scope because captures are
/// context-sensitive, but the lowered lambda body is unique per callable inst.
lowered_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Cache for direct statement-lowered lambda expressions keyed by lowering context.
lowered_callable_lambdas: std.AutoHashMap(u128, MIR.LambdaId),

/// Recursion guard for named top-level constants lowered on demand.
in_progress_const_defs: std.AutoHashMap(u64, void),

/// Metadata for opaque symbol IDs; populated at symbol construction time.
symbol_metadata: std.AutoHashMap(u64, SymbolMetadata),

/// Counter for generating synthetic ident indices for polymorphic specializations.
/// Counts down from NONE - 1 to avoid collision with real idents.
next_synthetic_ident: u29,

/// Counter for assigning unique local-pattern scopes to statement-lowered lambdas.
next_statement_pattern_scope: u64,

/// Counter for assigning unique explicit join-point ids in strongest-form MIR.
next_join_point_id: u32,

/// First local index that belongs to the currently lowered root/lambda body.
current_body_local_floor: usize,

/// Tracks callable instances currently being lowered (recursion guard).
in_progress_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Reserved callable skeletons for recursive callable-inst groups whose bodies are not
/// fully lowered yet but already need stable lambda ids.
reserved_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Binding patterns whose callable identity is satisfied by explicit callable bindings
/// rather than ordinary value binding locals.
skipped_callable_backed_binding_patterns: std.AutoHashMap(u64, void),

/// Callable-inst context for context-sensitive monomorphized lookup/call resolution.
current_callable_inst_context: Monomorphize.CallableInstId,

/// Current root CIR source expression when no callable-inst context is active.
current_root_source_expr_context: ?CIR.Expr.Idx,

/// Pre-resolved static dispatch targets keyed by (module_idx, expr_idx).
/// Filled from type-checker constraints so MIR lowering uses authoritative
/// dispatch resolution data directly.
resolved_dispatch_targets: std.AutoHashMap(u64, ResolvedDispatchTarget),

scratch_local_ids: base.Scratch(MIR.LocalId),
scratch_ident_idxs: base.Scratch(Ident.Idx),
scratch_cf_stmt_ids: base.Scratch(MIR.CFStmtId),
scratch_capture_locals: base.Scratch(MIR.LocalId),
active_loops: std.ArrayList(LoopContext),
mono_scratches: Monotype.Store.Scratches,

// --- Init/Deinit ---

pub fn init(
    allocator: Allocator,
    store: *MIR.Store,
    monomorphization: *const Monomorphize.Result,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
) Allocator.Error!Self {
    // Pre-build resolved static dispatch targets for all modules.
    var resolved_dispatch_targets = std.AutoHashMap(u64, ResolvedDispatchTarget).init(allocator);
    for (all_module_envs, 0..) |env, mod_idx| {
        const constraints = env.types.sliceAllStaticDispatchConstraints();
        for (constraints) |constraint| {
            if (constraint.source_expr_idx == types.StaticDispatchConstraint.no_source_expr) continue;
            if (constraint.resolved_target.isNone()) continue;

            const key = (@as(u64, @intCast(mod_idx)) << 32) | @as(u64, constraint.source_expr_idx);
            try resolved_dispatch_targets.put(key, .{
                .origin = constraint.resolved_target.origin_module,
                .method_ident = constraint.resolved_target.method_ident,
                .fn_var = constraint.fn_var,
            });
        }
    }

    return .{
        .allocator = allocator,
        .store = store,
        .monomorphization = monomorphization,
        .all_module_envs = all_module_envs,
        .types_store = types_store,
        .current_module_idx = current_module_idx,
        .current_pattern_scope = 0,
        .app_module_idx = app_module_idx,
        .type_scope = null,
        .type_scope_module_idx = null,
        .type_scope_caller_module_idx = null,
        .pattern_symbols = std.AutoHashMap(u128, MIR.LocalId).init(allocator),
        .type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .lowered_callable_lambdas = std.AutoHashMap(u128, MIR.LambdaId).init(allocator),
        .in_progress_const_defs = std.AutoHashMap(u64, void).init(allocator),
        .symbol_metadata = std.AutoHashMap(u64, SymbolMetadata).init(allocator),
        .next_synthetic_ident = Ident.Idx.NONE.idx - 1,
        .next_statement_pattern_scope = 1,
        .next_join_point_id = 0,
        .current_body_local_floor = 0,
        .in_progress_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .reserved_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .skipped_callable_backed_binding_patterns = std.AutoHashMap(u64, void).init(allocator),
        .current_callable_inst_context = .none,
        .current_root_source_expr_context = null,
        .resolved_dispatch_targets = resolved_dispatch_targets,
        .scratch_local_ids = try base.Scratch(MIR.LocalId).init(allocator),
        .scratch_ident_idxs = try base.Scratch(Ident.Idx).init(allocator),
        .scratch_cf_stmt_ids = try base.Scratch(MIR.CFStmtId).init(allocator),
        .scratch_capture_locals = try base.Scratch(MIR.LocalId).init(allocator),
        .active_loops = .empty,
        .mono_scratches = blk: {
            var ms = try Monotype.Store.Scratches.init(allocator);
            ms.ident_store = all_module_envs[current_module_idx].getIdentStoreConst();
            ms.module_env = all_module_envs[current_module_idx];
            ms.module_idx = current_module_idx;
            ms.all_module_envs = all_module_envs;
            break :blk ms;
        },
    };
}

pub fn deinit(self: *Self) void {
    self.pattern_symbols.deinit();
    self.type_var_seen.deinit();
    self.nominal_cycle_breakers.deinit();
    self.lowered_callable_insts.deinit();
    self.lowered_callable_lambdas.deinit();
    self.in_progress_const_defs.deinit();
    self.symbol_metadata.deinit();
    self.in_progress_callable_insts.deinit();
    self.reserved_callable_insts.deinit();
    self.skipped_callable_backed_binding_patterns.deinit();
    self.resolved_dispatch_targets.deinit();
    self.scratch_local_ids.deinit();
    self.scratch_ident_idxs.deinit();
    self.scratch_cf_stmt_ids.deinit();
    self.scratch_capture_locals.deinit();
    self.active_loops.deinit(self.allocator);
    self.mono_scratches.deinit();
}

/// Provide platform for-clause type substitutions for the given module so
/// MIR monotype resolution can use the concrete caller types during lowering.
pub fn setTypeScope(
    self: *Self,
    module_idx: u32,
    type_scope: *const types.TypeScope,
    caller_module_idx: u32,
) Allocator.Error!void {
    self.type_scope = type_scope;
    self.type_scope_module_idx = module_idx;
    self.type_scope_caller_module_idx = caller_module_idx;
    try self.seedTypeScopeBindingsInStore(
        self.current_module_idx,
        self.types_store,
        &self.type_var_seen,
    );
}

const symbol_namespace_local: u64 = 0;
const symbol_namespace_external_def: u64 = 1;

fn packLocalSymbolId(module_idx: u32, ident_idx: Ident.Idx) u64 {
    if (builtin.mode == .Debug) std.debug.assert(module_idx <= std.math.maxInt(u31));
    const ident_bits: u32 = @bitCast(ident_idx);
    return (symbol_namespace_local << 63) | (@as(u64, module_idx) << 32) | @as(u64, ident_bits);
}

fn packExternalDefSymbolId(module_idx: u32, def_node_idx: u16) u64 {
    if (builtin.mode == .Debug) std.debug.assert(module_idx <= std.math.maxInt(u31));
    return (symbol_namespace_external_def << 63) | (@as(u64, module_idx) << 32) | @as(u64, def_node_idx);
}

fn symbolMetadataModuleIdx(meta: SymbolMetadata) u32 {
    return switch (meta) {
        .local_ident => |m| m.module_idx,
        .external_def => |m| m.module_idx,
    };
}

fn moduleOwnsIdent(env: *const ModuleEnv, ident: Ident.Idx) bool {
    const ident_store = env.getIdentStoreConst();
    const bytes = ident_store.interner.bytes.items.items;
    const start: usize = @intCast(ident.idx);
    if (start >= bytes.len) return false;

    const tail = bytes[start..];
    const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return false;
    const text = tail[0..end_rel];

    const roundtrip = ident_store.findByString(text) orelse return false;
    return roundtrip.eql(ident);
}

fn getOwnedIdentText(env: *const ModuleEnv, ident: Ident.Idx) []const u8 {
    if (builtin.mode == .Debug) std.debug.assert(moduleOwnsIdent(env, ident));
    return env.getIdent(ident);
}

fn monomorphizationRootSourceExprContext(self: *const Self, context_callable_inst: Monomorphize.CallableInstId) ?CIR.Expr.Idx {
    return if (context_callable_inst.isNone()) self.current_root_source_expr_context else null;
}

fn flatRecordRepresentsEmpty(store_types: *const types.Store, record: types.Record) bool {
    var current_row = record;

    rows: while (true) {
        if (store_types.getRecordFieldsSlice(current_row.fields).len != 0) return false;

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = store_types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .alias => |alias| {
                    ext_var = store_types.getAliasBackingVar(alias);
                    continue;
                },
                .structure => |ext_flat| switch (ext_flat) {
                    .record => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    else => return false,
                },
                .flex, .rigid => return false,
                .err => return false,
            }
        }
    }
}

fn isTopLevelPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) bool {
    for (module_env.store.sliceDefs(module_env.all_defs)) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (def.pattern == pattern_idx) return true;
    }
    return false;
}

fn typeBindingInvariant(comptime fmt: []const u8, args: anytype) noreturn {
    std.debug.panic("statement-only MIR type-binding invariant violated: " ++ fmt, args);
}

fn toStrLowLevelForPrim(prim: Monotype.Prim) ?CIR.Expr.LowLevel {
    return switch (prim) {
        .str => null,
        .u8 => .u8_to_str,
        .i8 => .i8_to_str,
        .u16 => .u16_to_str,
        .i16 => .i16_to_str,
        .u32 => .u32_to_str,
        .i32 => .i32_to_str,
        .u64 => .u64_to_str,
        .i64 => .i64_to_str,
        .u128 => .u128_to_str,
        .i128 => .i128_to_str,
        .f32 => .f32_to_str,
        .f64 => .f64_to_str,
        .dec => .dec_to_str,
    };
}

fn builtinPrimForNominal(ident: Ident.Idx, common: ModuleEnv.CommonIdents) ?Monotype.Prim {
    if (ident.eql(common.str)) return .str;
    if (ident.eql(common.u8_type)) return .u8;
    if (ident.eql(common.i8_type)) return .i8;
    if (ident.eql(common.u16_type)) return .u16;
    if (ident.eql(common.i16_type)) return .i16;
    if (ident.eql(common.u32_type)) return .u32;
    if (ident.eql(common.i32_type)) return .i32;
    if (ident.eql(common.u64_type)) return .u64;
    if (ident.eql(common.i64_type)) return .i64;
    if (ident.eql(common.u128_type)) return .u128;
    if (ident.eql(common.i128_type)) return .i128;
    if (ident.eql(common.f32_type)) return .f32;
    if (ident.eql(common.f64_type)) return .f64;
    if (ident.eql(common.dec_type)) return .dec;
    return null;
}

fn seedTypeScopeBindingsInStore(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!void {
    const type_scope = self.type_scope orelse return;
    const type_scope_module_idx = self.type_scope_module_idx orelse return;
    const caller_module_idx = self.type_scope_caller_module_idx orelse return;

    if (module_idx != type_scope_module_idx) return;

    for (type_scope.scopes.items) |*scope| {
        var it = scope.iterator();
        while (it.next()) |entry| {
            const platform_var = entry.key_ptr.*;
            const caller_var = entry.value_ptr.*;
            const caller_mono = try self.monotypeFromTypeVarWithBindings(
                caller_module_idx,
                &self.all_module_envs[caller_module_idx].types,
                caller_var,
                bindings,
            );
            if (caller_mono.isNone()) continue;

            const normalized_mono = if (caller_module_idx == module_idx)
                caller_mono
            else
                try self.remapMonotypeBetweenModules(caller_mono, caller_module_idx, module_idx);

            const saved_bindings = self.type_var_seen;
            self.type_var_seen = bindings.*;
            defer self.type_var_seen = saved_bindings;
            try self.bindTypeVarMonotypes(platform_var, normalized_mono);
            bindings.* = self.type_var_seen;
        }
    }

    _ = store_types;
}

fn monotypeFromTypeVarWithBindings(
    self: *Self,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    defer nominal_cycle_breakers.deinit();

    const saved_bindings = self.type_var_seen;
    const saved_breakers = self.nominal_cycle_breakers;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_module_idx = self.mono_scratches.module_idx;
    const saved_all_module_envs = self.mono_scratches.all_module_envs;
    self.type_var_seen = bindings.*;
    self.nominal_cycle_breakers = nominal_cycle_breakers;
    self.mono_scratches.ident_store = self.all_module_envs[module_idx].getIdentStoreConst();
    self.mono_scratches.module_env = self.all_module_envs[module_idx];
    self.mono_scratches.module_idx = module_idx;
    self.mono_scratches.all_module_envs = self.all_module_envs;
    defer {
        bindings.* = self.type_var_seen;
        self.type_var_seen = saved_bindings;
        self.nominal_cycle_breakers = saved_breakers;
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
        self.mono_scratches.module_idx = saved_module_idx;
        self.mono_scratches.all_module_envs = saved_all_module_envs;
    }

    return self.store.monotype_store.fromTypeVar(
        self.allocator,
        store_types,
        var_,
        self.all_module_envs[module_idx].idents,
        &self.type_var_seen,
        &self.nominal_cycle_breakers,
        &self.mono_scratches,
    );
}

fn resolveImportedModuleIdx(self: *Self, caller_env: *const ModuleEnv, import_idx: CIR.Import.Idx) ?u32 {
    if (caller_env.imports.getResolvedModule(import_idx)) |module_idx| {
        if (module_idx < self.all_module_envs.len) return module_idx;
    }

    const import_pos = @intFromEnum(import_idx);
    if (import_pos >= caller_env.imports.imports.len()) return null;

    const import_name = caller_env.common.getString(caller_env.imports.imports.items.items[import_pos]);
    const base_name = identLastSegment(import_name);

    for (self.all_module_envs, 0..) |candidate_env, module_idx| {
        if (std.mem.eql(u8, candidate_env.module_name, import_name) or
            std.mem.eql(u8, candidate_env.module_name, base_name))
        {
            @constCast(&caller_env.imports).setResolvedModule(import_idx, @intCast(module_idx));
            return @intCast(module_idx);
        }
    }

    return null;
}

fn getCallLowLevelOp(self: *Self, caller_env: *const ModuleEnv, func_expr_idx: CIR.Expr.Idx) ?CIR.Expr.LowLevel {
    return switch (caller_env.store.getExpr(func_expr_idx)) {
        .e_lookup_external => |lookup| self.getExternalLowLevelOp(caller_env, lookup),
        .e_lookup_local => |lookup| getLocalLowLevelOp(caller_env, lookup.pattern_idx),
        else => null,
    };
}

fn getLocalLowLevelOp(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Expr.LowLevel {
    const def_idx = findDefByPattern(module_env, pattern_idx) orelse return null;
    const def = module_env.store.getDef(def_idx);
    const def_expr = module_env.store.getExpr(def.expr);
    if (def_expr == .e_lambda) {
        const body_expr = module_env.store.getExpr(def_expr.e_lambda.body);
        if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
    }
    return null;
}

fn findDefByPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    for (module_env.store.sliceDefs(module_env.all_defs)) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        if (def.pattern == pattern_idx) return def_idx;
    }
    return null;
}

fn getExternalLowLevelOp(
    self: *Self,
    caller_env: *const ModuleEnv,
    lookup: @TypeOf(@as(CIR.Expr, undefined).e_lookup_external),
) ?CIR.Expr.LowLevel {
    const ext_module_idx = self.resolveImportedModuleIdx(caller_env, lookup.module_idx) orelse return null;
    const ext_env = self.all_module_envs[ext_module_idx];
    if (!ext_env.store.isDefNode(lookup.target_node_idx)) return null;
    const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
    const def = ext_env.store.getDef(def_idx);
    const def_expr = ext_env.store.getExpr(def.expr);
    if (def_expr == .e_lambda) {
        const body_expr = ext_env.store.getExpr(def_expr.e_lambda.body);
        if (body_expr == .e_run_low_level) return body_expr.e_run_low_level.op;
    }
    return null;
}

fn resolveRequiredLookupTarget(
    self: *Self,
    module_env: *const ModuleEnv,
    lookup: @TypeOf(@as(CIR.Expr, undefined).e_lookup_required),
) ?RequiredLookupTarget {
    const app_idx = self.app_module_idx orelse return null;
    const required_type = module_env.requires_types.get(lookup.requires_idx);
    const required_name = module_env.getIdent(required_type.ident);

    const app_env = self.all_module_envs[app_idx];
    const app_ident = app_env.common.findIdent(required_name) orelse return null;
    const app_exports = app_env.store.sliceDefs(app_env.exports);
    for (app_exports) |def_idx| {
        const def = app_env.store.getDef(def_idx);
        const pat = app_env.store.getPattern(def.pattern);
        if (pat == .assign and pat.assign.ident.eql(app_ident)) {
            return .{
                .module_idx = app_idx,
                .def_idx = def_idx,
            };
        }
    }

    return null;
}

fn recordFieldIndexByName(
    self: *Self,
    field_name: Ident.Idx,
    mono_fields: []const Monotype.Field,
) u32 {
    for (mono_fields, 0..) |mono_field, field_idx| {
        if (self.identsStructurallyEqual(field_name, mono_field.name)) {
            return @intCast(field_idx);
        }
    }

    std.debug.panic(
        "statement-only MIR invariant violated: record field '{s}' missing from monotype",
        .{self.all_module_envs[self.current_module_idx].getIdent(field_name)},
    );
}

fn tagIndexByName(
    self: *Self,
    tag_name: Ident.Idx,
    mono_tags: []const Monotype.Tag,
) u32 {
    for (mono_tags, 0..) |mono_tag, tag_idx| {
        if (self.identsStructurallyEqual(tag_name, mono_tag.name)) {
            return @intCast(tag_idx);
        }
    }

    std.debug.panic(
        "statement-only MIR invariant violated: tag '{s}' missing from monotype",
        .{self.all_module_envs[self.current_module_idx].getIdent(tag_name)},
    );
}

fn collectPatternBindings(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    out: *std.ArrayList(PatternBinding),
) Allocator.Error!void {
    const pattern = module_env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => |assign| try out.append(self.allocator, .{ .ident = assign.ident, .pattern_idx = pattern_idx }),
        .as => |as_pat| {
            try out.append(self.allocator, .{ .ident = as_pat.ident, .pattern_idx = pattern_idx });
            try self.collectPatternBindings(module_env, as_pat.pattern, out);
        },
        .tuple => |tuple| {
            for (module_env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectPatternBindings(module_env, elem_pattern_idx, out);
            }
        },
        .applied_tag => |tag| {
            for (module_env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectPatternBindings(module_env, arg_pattern_idx, out);
            }
        },
        .record_destructure => |destructure| {
            for (module_env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectPatternBindings(module_env, sub_pattern_idx, out),
                    .SubPattern => |sub_pattern_idx| try self.collectPatternBindings(module_env, sub_pattern_idx, out),
                    .Rest => |sub_pattern_idx| try self.collectPatternBindings(module_env, sub_pattern_idx, out),
                }
            }
        },
        .list => |list| {
            for (module_env.store.slicePatterns(list.patterns)) |elem_pattern_idx| {
                try self.collectPatternBindings(module_env, elem_pattern_idx, out);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try self.collectPatternBindings(module_env, rest_pattern_idx, out);
                }
            }
        },
        .nominal => |nom| try self.collectPatternBindings(module_env, nom.backing_pattern, out),
        .nominal_external => |nom| try self.collectPatternBindings(module_env, nom.backing_pattern, out),
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => {},
    }
}

fn seenIndex(seen_indices: []const u32, idx: u32) bool {
    for (seen_indices) |seen_idx| {
        if (seen_idx == idx) return true;
    }
    return false;
}

fn appendSeenIndex(
    allocator: Allocator,
    seen_indices: *std.ArrayListUnmanaged(u32),
    idx: u32,
) Allocator.Error!void {
    if (seenIndex(seen_indices.items, idx)) return;
    try seen_indices.append(allocator, idx);
}

fn remainingRecordTailMonotype(
    self: *Self,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
) Allocator.Error!Monotype.Idx {
    var remaining_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
    defer remaining_fields.deinit(self.allocator);

    for (mono_fields, 0..) |field, field_idx| {
        if (seenIndex(seen_indices, @intCast(field_idx))) continue;
        try remaining_fields.append(self.allocator, field);
    }

    if (remaining_fields.items.len == 0) {
        return self.store.monotype_store.unit_idx;
    }

    const field_span = try self.store.monotype_store.addFields(self.allocator, remaining_fields.items);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .record = .{ .fields = field_span } });
}

fn remainingTagUnionTailMonotype(
    self: *Self,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
) Allocator.Error!Monotype.Idx {
    var remaining_tags: std.ArrayListUnmanaged(Monotype.Tag) = .empty;
    defer remaining_tags.deinit(self.allocator);

    for (mono_tags, 0..) |tag, tag_idx| {
        if (seenIndex(seen_indices, @intCast(tag_idx))) continue;
        try remaining_tags.append(self.allocator, tag);
    }

    const tag_span = try self.store.monotype_store.addTags(self.allocator, remaining_tags.items);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .tag_union = .{ .tags = tag_span } });
}

fn bindRecordRowTail(
    self: *Self,
    ext_var: types.Var,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
) Allocator.Error!void {
    const tail_mono = try self.remainingRecordTailMonotype(mono_fields, seen_indices);
    try self.bindTypeVarMonotypes(ext_var, tail_mono);
}

fn bindTagUnionRowTail(
    self: *Self,
    ext_var: types.Var,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
) Allocator.Error!void {
    const tail_mono = try self.remainingTagUnionTailMonotype(mono_tags, seen_indices);
    try self.bindTypeVarMonotypes(ext_var, tail_mono);
}

fn packCallableCacheKey(
    self: *const Self,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) u128 {
    const context_bits: u32 = @intFromEnum(self.current_callable_inst_context);
    const module_bits: u32 = self.current_module_idx;
    const expr_bits: u32 = @intFromEnum(expr_idx);
    const mono_bits: u32 = @intFromEnum(fn_monotype);
    return (@as(u128, context_bits) << 96) |
        (@as(u128, module_bits) << 64) |
        (@as(u128, expr_bits) << 32) |
        @as(u128, mono_bits);
}

fn lookupMonomorphizedExprMonotype(self: *const Self, expr_idx: CIR.Expr.Idx) ?Monomorphize.ResolvedMonotype {
    const rooted = self.monomorphization.getExprMonotype(
        self.current_callable_inst_context,
        self.monomorphizationRootSourceExprContext(self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    if (rooted != null) return rooted;

    if (self.current_callable_inst_context.isNone()) {
        return self.monomorphization.getExprMonotype(.none, null, self.current_module_idx, expr_idx);
    }

    return null;
}

fn lookupMonomorphizedExprCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Monomorphize.CallableInstId {
    const rooted = self.monomorphization.getExprCallableInst(
        self.current_callable_inst_context,
        self.monomorphizationRootSourceExprContext(self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    if (rooted != null) return rooted;

    if (self.current_callable_inst_context.isNone()) {
        return self.monomorphization.getExprCallableInst(.none, null, self.current_module_idx, expr_idx);
    }

    return null;
}

fn lookupMonomorphizedExprCallableInsts(self: *const Self, expr_idx: CIR.Expr.Idx) ?[]const Monomorphize.CallableInstId {
    const rooted = self.monomorphization.getExprCallableInsts(
        self.current_callable_inst_context,
        self.monomorphizationRootSourceExprContext(self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    if (rooted != null) return rooted;

    if (self.current_callable_inst_context.isNone()) {
        return self.monomorphization.getExprCallableInsts(.none, null, self.current_module_idx, expr_idx);
    }

    return null;
}

fn lookupMonomorphizedLookupCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Monomorphize.CallableInstId {
    const rooted = self.monomorphization.getLookupExprCallableInst(
        self.current_callable_inst_context,
        self.monomorphizationRootSourceExprContext(self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    if (rooted != null) return rooted;

    if (self.current_callable_inst_context.isNone()) {
        return self.monomorphization.getLookupExprCallableInst(.none, null, self.current_module_idx, expr_idx);
    }

    return null;
}

fn lookupMonomorphizedLookupCallableInsts(self: *const Self, expr_idx: CIR.Expr.Idx) ?[]const Monomorphize.CallableInstId {
    const rooted = self.monomorphization.getLookupExprCallableInsts(
        self.current_callable_inst_context,
        self.monomorphizationRootSourceExprContext(self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    if (rooted != null) return rooted;

    if (self.current_callable_inst_context.isNone()) {
        return self.monomorphization.getLookupExprCallableInsts(.none, null, self.current_module_idx, expr_idx);
    }

    return null;
}

fn lookupMonomorphizedValueExprCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Monomorphize.CallableInstId {
    if (self.lookupMonomorphizedExprCallableInst(expr_idx)) |callable_inst_id| return callable_inst_id;
    return self.lookupMonomorphizedLookupCallableInst(expr_idx);
}

fn callableInstMatchesFnMonotype(
    self: *Self,
    callable_inst_id: Monomorphize.CallableInstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!bool {
    const callable_inst = self.monomorphization.getCallableInst(callable_inst_id);
    const imported_callable_mono = try self.importMonotypeFromStore(
        &self.monomorphization.monotype_store,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
        fn_monotype_module_idx,
    );
    return self.monotypesStructurallyEqual(imported_callable_mono, fn_monotype);
}

fn selectCallableInstForFnMonotype(
    self: *Self,
    callable_inst_ids: []const Monomorphize.CallableInstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Monomorphize.CallableInstId {
    var resolved: ?Monomorphize.CallableInstId = null;
    for (callable_inst_ids) |callable_inst_id| {
        if (!try self.callableInstMatchesFnMonotype(callable_inst_id, fn_monotype, fn_monotype_module_idx)) continue;
        if (resolved) |existing| {
            if (existing != callable_inst_id) return null;
        } else {
            resolved = callable_inst_id;
        }
    }
    return resolved;
}

fn lookupMonomorphizedValueExprCallableInstForMonotype(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Monomorphize.CallableInstId {
    if (self.lookupMonomorphizedValueExprCallableInsts(expr_idx)) |callable_inst_ids| {
        if (try self.selectCallableInstForFnMonotype(callable_inst_ids, fn_monotype, fn_monotype_module_idx)) |callable_inst_id| {
            return callable_inst_id;
        }
    }

    if (self.lookupMonomorphizedValueExprCallableInst(expr_idx)) |callable_inst_id| {
        if (try self.callableInstMatchesFnMonotype(callable_inst_id, fn_monotype, fn_monotype_module_idx)) {
            return callable_inst_id;
        }
    }

    return null;
}

fn lookupMonomorphizedValueExprCallableInsts(self: *const Self, expr_idx: CIR.Expr.Idx) ?[]const Monomorphize.CallableInstId {
    const module_env = self.all_module_envs[self.current_module_idx];
    switch (module_env.store.getExpr(expr_idx)) {
        .e_lookup_local => |lookup| {
            const rooted = self.monomorphization.getContextPatternCallableInsts(
                self.current_callable_inst_context,
                self.current_module_idx,
                lookup.pattern_idx,
            );
            if (rooted != null) return rooted;

            if (self.current_callable_inst_context.isNone()) {
                return self.monomorphization.getContextPatternCallableInsts(
                    .none,
                    self.current_module_idx,
                    lookup.pattern_idx,
                );
            }
        },
        else => {},
    }

    if (self.lookupMonomorphizedExprCallableInsts(expr_idx)) |callable_inst_ids| return callable_inst_ids;
    return self.lookupMonomorphizedLookupCallableInsts(expr_idx);
}

fn lookupMonomorphizedDispatchCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Monomorphize.CallableInstId {
    const rooted = self.monomorphization.getDispatchExprCallableInst(
        self.current_callable_inst_context,
        self.monomorphizationRootSourceExprContext(self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
    if (rooted != null) return rooted;

    if (self.current_callable_inst_context.isNone()) {
        return self.monomorphization.getDispatchExprCallableInst(.none, null, self.current_module_idx, expr_idx);
    }

    return null;
}

fn patternScopeForCallableInst(callable_inst_id: Monomorphize.CallableInstId) u64 {
    if (callable_inst_id.isNone()) return 0;
    return (@as(u64, 1) << 63) | (@as(u64, @intFromEnum(callable_inst_id)) + 1);
}

fn identTextForCompare(self: *const Self, ident: Ident.Idx) ?[]const u8 {
    if (moduleOwnsIdent(self.all_module_envs[self.current_module_idx], ident)) {
        return getOwnedIdentText(self.all_module_envs[self.current_module_idx], ident);
    }

    for (self.all_module_envs, 0..) |module_env, module_idx| {
        if (module_idx == self.current_module_idx) continue;
        if (moduleOwnsIdent(module_env, ident)) {
            return getOwnedIdentText(module_env, ident);
        }
    }

    return null;
}

fn labelTextForCompare(self: *const Self, label: anytype) ?[]const u8 {
    return switch (@TypeOf(label)) {
        Ident.Idx => self.identTextForCompare(label),
        Monotype.Name => label.text(self.all_module_envs),
        else => @compileError("unsupported label type"),
    };
}

fn identsStructurallyEqual(self: *const Self, lhs: anytype, rhs: anytype) bool {
    if (@TypeOf(lhs) == Ident.Idx and @TypeOf(rhs) == Ident.Idx and lhs.eql(rhs)) return true;
    if (@TypeOf(lhs) == Monotype.Name and @TypeOf(rhs) == Monotype.Name and lhs.eql(rhs)) return true;

    const lhs_text = self.labelTextForCompare(lhs) orelse return false;
    const rhs_text = self.labelTextForCompare(rhs) orelse return false;
    return std.mem.eql(u8, lhs_text, rhs_text);
}

fn identLastSegment(text: []const u8) []const u8 {
    const dot = std.mem.lastIndexOfScalar(u8, text, '.') orelse return text;
    return text[dot + 1 ..];
}

fn identsTagNameEquivalent(self: *const Self, lhs: anytype, rhs: anytype) bool {
    if (self.identsStructurallyEqual(lhs, rhs)) return true;

    const lhs_text = self.labelTextForCompare(lhs) orelse return false;
    const rhs_text = self.labelTextForCompare(rhs) orelse return false;
    return std.mem.eql(u8, identLastSegment(lhs_text), identLastSegment(rhs_text));
}

fn remapMonotypeBetweenModules(
    self: *Self,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;

    var remapped = std.AutoHashMap(Monotype.Idx, Monotype.Idx).init(self.allocator);
    defer remapped.deinit();

    return self.remapMonotypeBetweenModulesRec(
        monotype,
        from_module_idx,
        to_module_idx,
        &remapped,
    );
}

fn remapMonotypeBetweenModulesRec(
    self: *Self,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
    remapped: *std.AutoHashMap(Monotype.Idx, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;
    if (remapped.get(monotype)) |existing| return existing;

    const mono = self.store.monotype_store.getMonotype(monotype);
    switch (mono) {
        .unit => return self.store.monotype_store.unit_idx,
        .prim => |prim| return self.store.monotype_store.primIdx(prim),
        .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic("remapMonotypeBetweenModules: unexpected recursive_placeholder", .{});
            }
            unreachable;
        },
        .list, .box, .tuple, .func, .record, .tag_union => {},
    }

    const placeholder = try self.store.monotype_store.addMonotype(self.allocator, .recursive_placeholder);
    try remapped.put(monotype, placeholder);

    const mapped_mono: Monotype.Monotype = switch (mono) {
        .list => |list_mono| .{ .list = .{
            .elem = try self.remapMonotypeBetweenModulesRec(
                list_mono.elem,
                from_module_idx,
                to_module_idx,
                remapped,
            ),
        } },
        .box => |box_mono| .{ .box = .{
            .inner = try self.remapMonotypeBetweenModulesRec(
                box_mono.inner,
                from_module_idx,
                to_module_idx,
                remapped,
            ),
        } },
        .tuple => |tuple_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            const elem_span = tuple_mono.elems;
            var elem_i: u32 = 0;
            while (elem_i < @as(u32, elem_span.len)) : (elem_i += 1) {
                const elem_pos_u64 = @as(u64, elem_span.start) + elem_i;
                if (builtin.mode == .Debug and elem_pos_u64 >= self.store.monotype_store.extra_idx.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: tuple elem span out of bounds (start={d}, len={d}, i={d}, extra_len={d})",
                        .{ elem_span.start, elem_span.len, elem_i, self.store.monotype_store.extra_idx.items.len },
                    );
                }
                const elem_pos: usize = @intCast(elem_pos_u64);
                const elem_mono: Monotype.Idx = @enumFromInt(self.store.monotype_store.extra_idx.items[elem_pos]);
                try self.mono_scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                    elem_mono,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                ));
            }

            const mapped_elems = try self.store.monotype_store.addIdxSpan(
                self.allocator,
                self.mono_scratches.idxs.sliceFromStart(idx_top),
            );
            break :blk .{ .tuple = .{ .elems = mapped_elems } };
        },
        .func => |func_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            const arg_span = func_mono.args;
            var arg_i: u32 = 0;
            while (arg_i < @as(u32, arg_span.len)) : (arg_i += 1) {
                const arg_pos_u64 = @as(u64, arg_span.start) + arg_i;
                if (builtin.mode == .Debug and arg_pos_u64 >= self.store.monotype_store.extra_idx.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: func arg span out of bounds (start={d}, len={d}, i={d}, extra_len={d})",
                        .{ arg_span.start, arg_span.len, arg_i, self.store.monotype_store.extra_idx.items.len },
                    );
                }
                const arg_pos: usize = @intCast(arg_pos_u64);
                const arg_mono: Monotype.Idx = @enumFromInt(self.store.monotype_store.extra_idx.items[arg_pos]);
                try self.mono_scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                    arg_mono,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                ));
            }
            const mapped_args = try self.store.monotype_store.addIdxSpan(
                self.allocator,
                self.mono_scratches.idxs.sliceFromStart(idx_top),
            );

            const mapped_ret = try self.remapMonotypeBetweenModulesRec(
                func_mono.ret,
                from_module_idx,
                to_module_idx,
                remapped,
            );

            break :blk .{ .func = .{
                .args = mapped_args,
                .ret = mapped_ret,
                .effectful = func_mono.effectful,
            } };
        },
        .record => |record_mono| blk: {
            const fields_top = self.mono_scratches.fields.top();
            defer self.mono_scratches.fields.clearFrom(fields_top);

            const field_span = record_mono.fields;
            var field_i: u32 = 0;
            while (field_i < @as(u32, field_span.len)) : (field_i += 1) {
                const field_pos_u64 = @as(u64, field_span.start) + field_i;
                if (builtin.mode == .Debug and field_pos_u64 >= self.store.monotype_store.fields.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: record field span out of bounds (start={d}, len={d}, i={d}, fields_len={d})",
                        .{ field_span.start, field_span.len, field_i, self.store.monotype_store.fields.items.len },
                    );
                }
                const field_pos: usize = @intCast(field_pos_u64);
                const field = self.store.monotype_store.fields.items[field_pos];
                try self.mono_scratches.fields.append(.{
                    .name = field.name,
                    .type_idx = try self.remapMonotypeBetweenModulesRec(
                        field.type_idx,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                    ),
                });
            }

            const mapped_fields = try self.store.monotype_store.addFields(
                self.allocator,
                self.mono_scratches.fields.sliceFromStart(fields_top),
            );
            break :blk .{ .record = .{ .fields = mapped_fields } };
        },
        .tag_union => |tag_union_mono| blk: {
            const tags_top = self.mono_scratches.tags.top();
            defer self.mono_scratches.tags.clearFrom(tags_top);

            const tag_span = tag_union_mono.tags;
            var tag_i: u32 = 0;
            while (tag_i < @as(u32, tag_span.len)) : (tag_i += 1) {
                const tag_pos_u64 = @as(u64, tag_span.start) + tag_i;
                if (builtin.mode == .Debug and tag_pos_u64 >= self.store.monotype_store.tags.items.len) {
                    std.debug.panic(
                        "remapMonotypeBetweenModulesRec: tag span out of bounds (start={d}, len={d}, i={d}, tags_len={d})",
                        .{ tag_span.start, tag_span.len, tag_i, self.store.monotype_store.tags.items.len },
                    );
                }
                const tag_pos: usize = @intCast(tag_pos_u64);
                const tag = self.store.monotype_store.tags.items[tag_pos];

                const payload_top = self.mono_scratches.idxs.top();
                defer self.mono_scratches.idxs.clearFrom(payload_top);

                const payload_span = tag.payloads;
                var payload_i: u32 = 0;
                while (payload_i < @as(u32, payload_span.len)) : (payload_i += 1) {
                    const payload_pos_u64 = @as(u64, payload_span.start) + payload_i;
                    if (builtin.mode == .Debug and payload_pos_u64 >= self.store.monotype_store.extra_idx.items.len) {
                        std.debug.panic(
                            "remapMonotypeBetweenModulesRec: tag payload span out of bounds (start={d}, len={d}, i={d}, extra_len={d}, tag={d})",
                            .{
                                payload_span.start,
                                payload_span.len,
                                payload_i,
                                self.store.monotype_store.extra_idx.items.len,
                                tag.name.ident.idx,
                            },
                        );
                    }
                    const payload_pos: usize = @intCast(payload_pos_u64);
                    const payload_mono: Monotype.Idx = @enumFromInt(self.store.monotype_store.extra_idx.items[payload_pos]);
                    try self.mono_scratches.idxs.append(try self.remapMonotypeBetweenModulesRec(
                        payload_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                    ));
                }

                const mapped_payloads = try self.store.monotype_store.addIdxSpan(
                    self.allocator,
                    self.mono_scratches.idxs.sliceFromStart(payload_top),
                );
                try self.mono_scratches.tags.append(.{
                    .name = tag.name,
                    .payloads = mapped_payloads,
                });
            }

            const mapped_tags = try self.store.monotype_store.addTags(
                self.allocator,
                self.mono_scratches.tags.sliceFromStart(tags_top),
            );
            break :blk .{ .tag_union = .{ .tags = mapped_tags } };
        },
        .unit, .prim, .recursive_placeholder => unreachable,
    };

    self.store.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;
    return placeholder;
}

fn importMonotypeFromStore(
    self: *Self,
    source_store: *const Monotype.Store,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone()) return monotype;

    if (source_store == &self.store.monotype_store and from_module_idx == to_module_idx) {
        return monotype;
    }

    var imported = std.AutoHashMap(Monotype.Idx, Monotype.Idx).init(self.allocator);
    defer imported.deinit();

    return self.importMonotypeFromStoreRec(
        source_store,
        monotype,
        from_module_idx,
        to_module_idx,
        &imported,
    );
}

fn importMonotypeFromStoreRec(
    self: *Self,
    source_store: *const Monotype.Store,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
    imported: *std.AutoHashMap(Monotype.Idx, Monotype.Idx),
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone()) return monotype;
    if (source_store == &self.store.monotype_store and from_module_idx == to_module_idx) {
        return monotype;
    }
    if (imported.get(monotype)) |existing| return existing;

    const mono = source_store.getMonotype(monotype);
    switch (mono) {
        .unit => return self.store.monotype_store.unit_idx,
        .prim => |prim| return self.store.monotype_store.primIdx(prim),
        .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic("importMonotypeFromStore: unexpected recursive_placeholder", .{});
            }
            unreachable;
        },
        .list, .box, .tuple, .func, .record, .tag_union => {},
    }

    const placeholder = try self.store.monotype_store.addMonotype(self.allocator, .recursive_placeholder);
    try imported.put(monotype, placeholder);

    const mapped_mono: Monotype.Monotype = switch (mono) {
        .list => |list_mono| .{ .list = .{
            .elem = try self.importMonotypeFromStoreRec(
                source_store,
                list_mono.elem,
                from_module_idx,
                to_module_idx,
                imported,
            ),
        } },
        .box => |box_mono| .{ .box = .{
            .inner = try self.importMonotypeFromStoreRec(
                source_store,
                box_mono.inner,
                from_module_idx,
                to_module_idx,
                imported,
            ),
        } },
        .tuple => |tuple_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            for (source_store.getIdxSpan(tuple_mono.elems)) |elem_mono| {
                try self.mono_scratches.idxs.append(try self.importMonotypeFromStoreRec(
                    source_store,
                    elem_mono,
                    from_module_idx,
                    to_module_idx,
                    imported,
                ));
            }

            break :blk .{ .tuple = .{
                .elems = try self.store.monotype_store.addIdxSpan(
                    self.allocator,
                    self.mono_scratches.idxs.sliceFromStart(idx_top),
                ),
            } };
        },
        .func => |func_mono| blk: {
            const idx_top = self.mono_scratches.idxs.top();
            defer self.mono_scratches.idxs.clearFrom(idx_top);

            for (source_store.getIdxSpan(func_mono.args)) |arg_mono| {
                try self.mono_scratches.idxs.append(try self.importMonotypeFromStoreRec(
                    source_store,
                    arg_mono,
                    from_module_idx,
                    to_module_idx,
                    imported,
                ));
            }

            break :blk .{ .func = .{
                .args = try self.store.monotype_store.addIdxSpan(
                    self.allocator,
                    self.mono_scratches.idxs.sliceFromStart(idx_top),
                ),
                .ret = try self.importMonotypeFromStoreRec(
                    source_store,
                    func_mono.ret,
                    from_module_idx,
                    to_module_idx,
                    imported,
                ),
                .effectful = func_mono.effectful,
            } };
        },
        .record => |record_mono| blk: {
            const fields_top = self.mono_scratches.fields.top();
            defer self.mono_scratches.fields.clearFrom(fields_top);

            for (source_store.getFields(record_mono.fields)) |field| {
                try self.mono_scratches.fields.append(.{
                    .name = field.name,
                    .type_idx = try self.importMonotypeFromStoreRec(
                        source_store,
                        field.type_idx,
                        from_module_idx,
                        to_module_idx,
                        imported,
                    ),
                });
            }

            break :blk .{ .record = .{
                .fields = try self.store.monotype_store.addFields(
                    self.allocator,
                    self.mono_scratches.fields.sliceFromStart(fields_top),
                ),
            } };
        },
        .tag_union => |tag_union_mono| blk: {
            const tags_top = self.mono_scratches.tags.top();
            defer self.mono_scratches.tags.clearFrom(tags_top);

            for (source_store.getTags(tag_union_mono.tags)) |tag| {
                const payload_top = self.mono_scratches.idxs.top();
                defer self.mono_scratches.idxs.clearFrom(payload_top);

                for (source_store.getIdxSpan(tag.payloads)) |payload_mono| {
                    try self.mono_scratches.idxs.append(try self.importMonotypeFromStoreRec(
                        source_store,
                        payload_mono,
                        from_module_idx,
                        to_module_idx,
                        imported,
                    ));
                }

                try self.mono_scratches.tags.append(.{
                    .name = tag.name,
                    .payloads = try self.store.monotype_store.addIdxSpan(
                        self.allocator,
                        self.mono_scratches.idxs.sliceFromStart(payload_top),
                    ),
                });
            }

            break :blk .{ .tag_union = .{
                .tags = try self.store.monotype_store.addTags(
                    self.allocator,
                    self.mono_scratches.tags.sliceFromStart(tags_top),
                ),
            } };
        },
        .unit, .prim, .recursive_placeholder => unreachable,
    };

    self.store.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;

    // Propagate opaque type markers across stores so that Str.inspect
    // correctly renders opaque types as "<opaque>" even through polymorphic
    // wrappers where the type falls back to the structural monotype.
    if (source_store.isOpaque(monotype)) {
        try self.store.monotype_store.markOpaque(self.allocator, placeholder);
    }

    return placeholder;
}

fn monotypeIdxSpanIsValid(self: *const Self, span: Monotype.Span) bool {
    const start: usize = @intCast(span.start);
    return start <= self.store.monotype_store.extra_idx.items.len and
        start + span.len <= self.store.monotype_store.extra_idx.items.len;
}

fn monotypeTagSpanIsValid(self: *const Self, span: Monotype.TagSpan) bool {
    const start: usize = @intCast(span.start);
    return start <= self.store.monotype_store.tags.items.len and
        start + span.len <= self.store.monotype_store.tags.items.len;
}

fn monotypeFieldSpanIsValid(self: *const Self, span: Monotype.FieldSpan) bool {
    const start: usize = @intCast(span.start);
    return start <= self.store.monotype_store.fields.items.len and
        start + span.len <= self.store.monotype_store.fields.items.len;
}

fn monotypeIdxIsValid(self: *const Self, monotype: Monotype.Idx) bool {
    return !monotype.isNone() and @intFromEnum(monotype) < self.store.monotype_store.monotypes.items.len;
}

fn monotypeIsUnit(self: *const Self, monotype: Monotype.Idx) bool {
    return self.monotypeIdxIsValid(monotype) and self.store.monotype_store.getMonotype(monotype) == .unit;
}

fn monotypeIsWellFormed(self: *const Self, monotype: Monotype.Idx) bool {
    var seen = std.AutoHashMap(Monotype.Idx, void).init(self.allocator);
    defer seen.deinit();
    return self.monotypeIsWellFormedRec(monotype, &seen);
}

fn monotypeIsWellFormedRec(
    self: *const Self,
    monotype: Monotype.Idx,
    seen: *std.AutoHashMap(Monotype.Idx, void),
) bool {
    if (!self.monotypeIdxIsValid(monotype)) return false;
    if (seen.contains(monotype)) return true;
    seen.put(monotype, {}) catch return false;

    return switch (self.store.monotype_store.getMonotype(monotype)) {
        .unit, .prim => true,
        .recursive_placeholder => false,
        .list => |list_mono| self.monotypeIsWellFormedRec(list_mono.elem, seen),
        .box => |box_mono| self.monotypeIsWellFormedRec(box_mono.inner, seen),
        .tuple => |tuple_mono| blk: {
            if (!self.monotypeIdxSpanIsValid(tuple_mono.elems)) break :blk false;
            for (self.store.monotype_store.getIdxSpan(tuple_mono.elems)) |elem| {
                if (!self.monotypeIsWellFormedRec(elem, seen)) break :blk false;
            }
            break :blk true;
        },
        .func => |func_mono| blk: {
            if (!self.monotypeIdxSpanIsValid(func_mono.args)) break :blk false;
            for (self.store.monotype_store.getIdxSpan(func_mono.args)) |arg| {
                if (!self.monotypeIsWellFormedRec(arg, seen)) break :blk false;
            }
            break :blk self.monotypeIsWellFormedRec(func_mono.ret, seen);
        },
        .record => |record_mono| blk: {
            if (!self.monotypeFieldSpanIsValid(record_mono.fields)) break :blk false;
            for (self.store.monotype_store.getFields(record_mono.fields)) |field| {
                if (!self.monotypeIsWellFormedRec(field.type_idx, seen)) break :blk false;
            }
            break :blk true;
        },
        .tag_union => |tag_union_mono| blk: {
            if (!self.monotypeTagSpanIsValid(tag_union_mono.tags)) break :blk false;
            for (self.store.monotype_store.getTags(tag_union_mono.tags)) |tag| {
                if (!self.monotypeIdxSpanIsValid(tag.payloads)) break :blk false;
                for (self.store.monotype_store.getIdxSpan(tag.payloads)) |payload| {
                    if (!self.monotypeIsWellFormedRec(payload, seen)) break :blk false;
                }
            }
            break :blk true;
        },
    };
}

fn internSymbol(self: *Self, namespace_idx: u32, ident_idx: Ident.Idx) Allocator.Error!MIR.Symbol {
    const raw = packLocalSymbolId(namespace_idx, ident_idx);
    const gop = try self.symbol_metadata.getOrPut(raw);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{ .local_ident = .{ .module_idx = namespace_idx, .ident_idx = ident_idx } };
    } else if (builtin.mode == .Debug) {
        switch (gop.value_ptr.*) {
            .local_ident => |existing| {
                if (existing.module_idx != namespace_idx or !existing.ident_idx.eql(ident_idx)) {
                    std.debug.panic(
                        "Local symbol metadata mismatch for raw id {d}: existing module={d} ident={d}, new module={d} ident={d}",
                        .{ raw, existing.module_idx, existing.ident_idx.idx, namespace_idx, ident_idx.idx },
                    );
                }
            },
            .external_def => |existing| std.debug.panic(
                "Symbol namespace mismatch for raw id {d}: existing external def (module={d}, node={d}), new local ident (module={d}, ident={d})",
                .{ raw, existing.module_idx, existing.def_node_idx, namespace_idx, ident_idx.idx },
            ),
        }
    }
    return MIR.Symbol.fromRaw(raw);
}

fn internExternalDefSymbol(self: *Self, module_idx: u32, def_node_idx: u16) Allocator.Error!MIR.Symbol {
    const module_env = self.all_module_envs[module_idx];
    if (!module_env.store.isDefNode(def_node_idx)) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "internExternalDefSymbol: non-def node index {d} for module_idx={d}",
                .{ def_node_idx, module_idx },
            );
        }
        unreachable;
    }

    const def_idx: CIR.Def.Idx = @enumFromInt(def_node_idx);
    const def = module_env.store.getDef(def_idx);
    const pattern = module_env.store.getPattern(def.pattern);
    const display_ident: Ident.Idx = switch (pattern) {
        .assign => |assign| assign.ident,
        .as => |as_pattern| as_pattern.ident,
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "internExternalDefSymbol: expected top-level assign/as pattern for module_idx={d} node={d}, found '{s}'",
                    .{ module_idx, def_node_idx, @tagName(pattern) },
                );
            }
            unreachable;
        },
    };

    const raw = packExternalDefSymbolId(module_idx, def_node_idx);
    const gop = try self.symbol_metadata.getOrPut(raw);
    if (!gop.found_existing) {
        gop.value_ptr.* = .{ .external_def = .{
            .module_idx = module_idx,
            .def_node_idx = def_node_idx,
            .display_ident_idx = display_ident,
        } };
    } else if (builtin.mode == .Debug) {
        switch (gop.value_ptr.*) {
            .external_def => |existing| {
                if (existing.module_idx != module_idx or existing.def_node_idx != def_node_idx or !existing.display_ident_idx.eql(display_ident)) {
                    std.debug.panic(
                        "External def symbol metadata mismatch for raw id {d}: existing module={d} node={d} ident={d}, new module={d} node={d} ident={d}",
                        .{
                            raw,
                            existing.module_idx,
                            existing.def_node_idx,
                            existing.display_ident_idx.idx,
                            module_idx,
                            def_node_idx,
                            display_ident.idx,
                        },
                    );
                }
            },
            .local_ident => |existing| std.debug.panic(
                "Symbol namespace mismatch for raw id {d}: existing local ident (module={d}, ident={d}), new external def (module={d}, node={d})",
                .{ raw, existing.module_idx, existing.ident_idx.idx, module_idx, def_node_idx },
            ),
        }
    }

    return MIR.Symbol.fromRaw(raw);
}

fn getSymbolMetadata(self: *const Self, symbol: MIR.Symbol) SymbolMetadata {
    const key = symbol.raw();
    return self.symbol_metadata.get(key) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("Missing symbol metadata for symbol key {d}", .{key});
        }
        unreachable;
    };
}

/// Copy a CIR string literal into MIR's own string store.
/// This ensures MIR is self-contained and downstream passes (LIR, codegen)
/// never need to reach back into CIR module envs for string data.
fn copyStringToMir(self: *Self, module_env: *const ModuleEnv, cir_str_idx: StringLiteral.Idx) Allocator.Error!StringLiteral.Idx {
    if (cir_str_idx == .none) return .none;
    const str_bytes = module_env.getString(cir_str_idx);
    return self.store.strings.insert(self.allocator, str_bytes);
}


/// Create a symbol using MIR lowering's current opaque ID encoding.
pub fn makeSymbol(self: *Self, module_idx: u32, ident_idx: Ident.Idx) Allocator.Error!MIR.Symbol {
    return self.internSymbol(module_idx, ident_idx);
}

/// Lower a CIR root source expression to a strongest-form MIR constant definition.
pub fn lowerRootConst(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ConstDefId {
    const saved_root_source_expr_context = self.current_root_source_expr_context;
    if (self.current_callable_inst_context.isNone() and self.current_root_source_expr_context == null) {
        self.current_root_source_expr_context = expr_idx;
    }
    defer self.current_root_source_expr_context = saved_root_source_expr_context;
    return self.lowerConstDefFromSourceExpr(expr_idx);
}

fn lowerNamedConstDefFromSourceExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    symbol: MIR.Symbol,
) Allocator.Error!MIR.ConstDefId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const region = module_env.store.getExprRegion(expr_idx);
    const monotype = try self.sourceExprResultMonotype(expr_idx);
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;
    const result_local = try self.freshSyntheticLocal(monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    const body = try self.lowerCirExprInto(expr_idx, result_local, ret_stmt);

    return self.store.registerConstDef(self.allocator, .{
        .symbol = symbol,
        .body = body,
        .monotype = monotype,
        .source_region = region,
    });
}

fn freshSyntheticLocal(self: *Self, monotype: Monotype.Idx, reassignable: bool) Allocator.Error!MIR.LocalId {
    return self.store.addLocal(self.allocator, .{
        .monotype = monotype,
        .reassignable = reassignable,
    });
}

fn freshJoinPointId(self: *Self) MIR.JoinPointId {
    if (builtin.mode == .Debug and self.next_join_point_id == std.math.maxInt(u32)) {
        std.debug.panic(
            "statement-only MIR ran out of join point ids",
            .{},
        );
    }

    const id = self.next_join_point_id;
    self.next_join_point_id += 1;
    return @enumFromInt(id);
}

fn currentBodyReassignableLocals(self: *Self) Allocator.Error![]MIR.LocalId {
    var locals = std.ArrayList(MIR.LocalId).empty;
    errdefer locals.deinit(self.allocator);

    for (self.store.locals.items[self.current_body_local_floor..], self.current_body_local_floor..) |local, i| {
        if (!local.reassignable) continue;
        try locals.append(self.allocator, @enumFromInt(@as(u32, @intCast(i))));
    }

    return locals.toOwnedSlice(self.allocator);
}

fn sourceExprResultMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    const module_env = self.all_module_envs[self.current_module_idx];
    const expr = module_env.store.getExpr(expr_idx);

    if (expr == .e_run_low_level and expr.e_run_low_level.op == .str_inspect) {
        // Test/repl helpers synthesize Str.inspect directly in CIR without threading a
        // resolved type var through canonicalization. Strongest-form MIR must still
        // force the root result to Str so later layout/codegen stages see the real ABI.
        return self.store.monotype_store.primIdx(.str);
    }

    return self.resolveMonotype(expr_idx);
}

fn lowerConstDefFromSourceExpr(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ConstDefId {
    const symbol = try self.internSymbol(
        self.current_module_idx,
        self.makeSyntheticIdent(Ident.Idx.NONE),
    );
    return self.lowerNamedConstDefFromSourceExpr(expr_idx, symbol);
}

fn lowerStringConcatInto(
    self: *Self,
    segments: []const CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (segments.len == 0) {
        const empty = try self.store.insertString(self.allocator, "");
        return self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .str = empty },
            .next = next,
        } });
    }
    if (segments.len == 1) return self.lowerCirExprInto(segments[0], target, next);

    const monotype = self.store.getLocal(target).monotype;
    const lhs_local = try self.freshSyntheticLocal(monotype, false);
    const rhs_local = try self.freshSyntheticLocal(monotype, false);
    const concat_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = target,
        .op = .str_concat,
        .args = try self.store.addLocalSpan(self.allocator, &.{ lhs_local, rhs_local }),
        .next = next,
    } });
    const rhs_entry = try self.lowerCirExprInto(segments[segments.len - 1], rhs_local, concat_stmt);
    return self.lowerStringConcatInto(segments[0 .. segments.len - 1], lhs_local, rhs_entry);
}

fn lowerStrLiteralInto(
    self: *Self,
    target: MIR.LocalId,
    text: []const u8,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const mir_str = try self.store.insertString(self.allocator, text);
    return self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = target,
        .literal = .{ .str = mir_str },
        .next = next,
    } });
}

fn lowerBoolLiteralInto(
    self: *Self,
    target: MIR.LocalId,
    bool_mono: Monotype.Idx,
    value: bool,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const tag_names = boolTagNamesForMonotype(self, bool_mono) orelse std.debug.panic(
        "statement-only MIR expected Bool monotype for local {d}",
        .{@intFromEnum(target)},
    );

    return self.store.addCFStmt(self.allocator, .{ .assign_tag = .{
        .target = target,
        .name = if (value) tag_names.true_name else tag_names.false_name,
        .args = MIR.LocalSpan.empty(),
        .next = next,
    } });
}

fn lowerUnaryLowLevelInto(
    self: *Self,
    target: MIR.LocalId,
    op: CIR.Expr.LowLevel,
    source: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = target,
        .op = op,
        .args = try self.store.addLocalSpan(self.allocator, &.{source}),
        .next = next,
    } });
}

fn lowerRefInto(
    self: *Self,
    target: MIR.LocalId,
    op: MIR.RefOp,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.store.addCFStmt(self.allocator, .{ .assign_ref = .{
        .target = target,
        .op = op,
        .next = next,
    } });
}

fn lowerLocalAliasInto(
    self: *Self,
    target: MIR.LocalId,
    source: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.lowerRefInto(target, .{ .local = source }, next);
}

fn lowerConcatLocalsInto(
    self: *Self,
    segments: []const MIR.LocalId,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (segments.len == 0) {
        return self.lowerStrLiteralInto(target, "", next);
    }
    if (segments.len == 1) {
        return self.lowerLocalAliasInto(target, segments[0], next);
    }

    const str_mono = self.store.monotype_store.primIdx(.str);
    const lhs_local = try self.freshSyntheticLocal(str_mono, false);
    const rhs_local = try self.freshSyntheticLocal(str_mono, false);
    const concat_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = target,
        .op = .str_concat,
        .args = try self.store.addLocalSpan(self.allocator, &.{ lhs_local, rhs_local }),
        .next = next,
    } });
    const rhs_entry = try self.lowerLocalAliasInto(rhs_local, segments[segments.len - 1], concat_stmt);
    return self.lowerConcatLocalsInto(segments[0 .. segments.len - 1], lhs_local, rhs_entry);
}

fn lowerTupleInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    tuple_data: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const elems = self.store.monotype_store.getIdxSpan(tuple_data.elems);
    if (elems.len == 0) return self.lowerStrLiteralInto(target, "()", next);

    const str_mono = self.store.monotype_store.primIdx(.str);
    const scratch_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(scratch_top);

    const open_local = try self.freshSyntheticLocal(str_mono, false);
    const close_local = try self.freshSyntheticLocal(str_mono, false);
    const comma_local = if (elems.len > 1) try self.freshSyntheticLocal(str_mono, false) else open_local;

    const elem_pair_top = self.scratch_local_ids.top();
    for (elems) |elem_mono| {
        try self.scratch_local_ids.append(try self.freshSyntheticLocal(elem_mono, false));
        try self.scratch_local_ids.append(try self.freshSyntheticLocal(str_mono, false));
    }

    const segment_top = self.scratch_local_ids.top();
    try self.scratch_local_ids.append(open_local);
    for (elems, 0..) |_, i| {
        const elem_str_local = self.scratch_local_ids.items.items[elem_pair_top + (i * 2) + 1];
        try self.scratch_local_ids.append(elem_str_local);
        if (i + 1 < elems.len) {
            try self.scratch_local_ids.append(comma_local);
        }
    }
    try self.scratch_local_ids.append(close_local);

    var current = try self.lowerConcatLocalsInto(
        self.scratch_local_ids.sliceFromStart(segment_top),
        target,
        next,
    );

    var i = elems.len;
    while (i > 0) {
        i -= 1;
        const elem_local = self.scratch_local_ids.items.items[elem_pair_top + (i * 2)];
        const elem_str_local = self.scratch_local_ids.items.items[elem_pair_top + (i * 2) + 1];
        current = try self.lowerStrInspectLocalInto(elem_local, elems[i], elem_str_local, current);
        current = try self.lowerRefInto(elem_local, .{ .field = .{
            .source = source,
            .field_idx = @intCast(i),
        } }, current);
    }

    current = try self.lowerStrLiteralInto(close_local, ")", current);
    if (elems.len > 1) {
        current = try self.lowerStrLiteralInto(comma_local, ", ", current);
    }
    current = try self.lowerStrLiteralInto(open_local, "(", current);
    return current;
}

fn lowerListInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    list_data: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const str_mono = self.store.monotype_store.primIdx(.str);
    const u64_mono = self.store.monotype_store.primIdx(.u64);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );

    const len_local = try self.freshSyntheticLocal(u64_mono, false);
    const zero_local = try self.freshSyntheticLocal(u64_mono, false);
    const one_local = try self.freshSyntheticLocal(u64_mono, false);
    const open_local = try self.freshSyntheticLocal(str_mono, false);
    const close_local = try self.freshSyntheticLocal(str_mono, false);
    const empty_local = try self.freshSyntheticLocal(str_mono, false);
    const comma_local = try self.freshSyntheticLocal(str_mono, false);
    const accum_local = try self.freshSyntheticLocal(str_mono, false);
    const index_local = try self.freshSyntheticLocal(u64_mono, false);
    const next_accum_local = try self.freshSyntheticLocal(str_mono, false);
    const next_index_local = try self.freshSyntheticLocal(u64_mono, false);
    const elem_local = try self.freshSyntheticLocal(list_data.elem, false);
    const elem_str_local = try self.freshSyntheticLocal(str_mono, false);
    const prefix_local = try self.freshSyntheticLocal(str_mono, false);
    const is_first_local = try self.freshSyntheticLocal(bool_mono, false);
    const cond_local = try self.freshSyntheticLocal(bool_mono, false);

    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();

    const exit_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_exit_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{target}),
    } });
    const false_body = try self.lowerConcatLocalsInto(&.{ open_local, accum_local, close_local }, target, exit_jump);

    const loop_back = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{ next_accum_local, next_index_local }),
    } });
    const next_index_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = next_index_local,
        .op = .num_plus,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, one_local }),
        .next = loop_back,
    } });
    const next_accum_stmt = try self.lowerConcatLocalsInto(&.{ accum_local, prefix_local, elem_str_local }, next_accum_local, next_index_stmt);
    const non_first_prefix = try self.lowerLocalAliasInto(prefix_local, comma_local, next_accum_stmt);
    const first_prefix = try self.lowerLocalAliasInto(prefix_local, empty_local, next_accum_stmt);
    const bool_tag_names = boolTagNamesForMonotype(self, bool_mono) orelse std.debug.panic(
        "statement-only MIR list inspect expected Bool monotype",
        .{},
    );
    const prefix_switch = try self.lowerSwitchOnDiscriminant(
        is_first_local,
        bool_mono,
        bool_tag_names.true_name,
        first_prefix,
        non_first_prefix,
    );
    const is_first_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = is_first_local,
        .op = .num_is_eq,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, zero_local }),
        .next = prefix_switch,
    } });
    const inspect_elem = try self.lowerStrInspectLocalInto(elem_local, list_data.elem, elem_str_local, is_first_stmt);
    const get_elem = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = elem_local,
        .op = .list_get_unsafe,
        .args = try self.store.addLocalSpan(self.allocator, &.{ source, index_local }),
        .next = inspect_elem,
    } });
    const true_body = get_elem;

    const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, bool_mono, bool_tag_names.true_name, true_body, false_body);
    const cond_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = cond_local,
        .op = .num_is_lt,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, len_local }),
        .next = switch_stmt,
    } });

    const initial_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{ empty_local, zero_local }),
    } });
    const init_comma = try self.lowerStrLiteralInto(comma_local, ", ", initial_jump);
    const init_empty = try self.lowerStrLiteralInto(empty_local, "", init_comma);
    const init_close = try self.lowerStrLiteralInto(close_local, "]", init_empty);
    const init_open = try self.lowerStrLiteralInto(open_local, "[", init_close);
    const init_one = try self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = one_local,
        .literal = .{ .int = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 } },
        .next = init_open,
    } });
    const init_zero = try self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = zero_local,
        .literal = .{ .int = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 } },
        .next = init_one,
    } });
    const init_len = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = len_local,
        .op = .list_len,
        .args = try self.store.addLocalSpan(self.allocator, &.{source}),
        .next = init_zero,
    } });

    const head_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_head_id,
        .params = try self.store.addLocalSpan(self.allocator, &.{ accum_local, index_local }),
        .body = cond_stmt,
        .remainder = init_len,
    } });

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_exit_id,
        .params = try self.store.addLocalSpan(self.allocator, &.{target}),
        .body = next,
        .remainder = head_join,
    } });
}

fn lowerRecordInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    record: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const fields = self.store.monotype_store.getFields(record.fields);
    if (fields.len == 0) return self.lowerStrLiteralInto(target, "{}", next);

    const str_mono = self.store.monotype_store.primIdx(.str);
    const scratch_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(scratch_top);

    const open_local = try self.freshSyntheticLocal(str_mono, false);
    const close_local = try self.freshSyntheticLocal(str_mono, false);
    const comma_local = if (fields.len > 1) try self.freshSyntheticLocal(str_mono, false) else open_local;

    const field_triplet_top = self.scratch_local_ids.top();
    for (fields) |field| {
        try self.scratch_local_ids.append(try self.freshSyntheticLocal(str_mono, false)); // label
        try self.scratch_local_ids.append(try self.freshSyntheticLocal(field.type_idx, false)); // value
        try self.scratch_local_ids.append(try self.freshSyntheticLocal(str_mono, false)); // inspected value
    }

    const segment_top = self.scratch_local_ids.top();
    try self.scratch_local_ids.append(open_local);
    for (fields, 0..) |_, i| {
        const label_local = self.scratch_local_ids.items.items[field_triplet_top + (i * 3)];
        const value_str_local = self.scratch_local_ids.items.items[field_triplet_top + (i * 3) + 2];
        try self.scratch_local_ids.append(label_local);
        try self.scratch_local_ids.append(value_str_local);
        if (i + 1 < fields.len) {
            try self.scratch_local_ids.append(comma_local);
        }
    }
    try self.scratch_local_ids.append(close_local);

    var current = try self.lowerConcatLocalsInto(
        self.scratch_local_ids.sliceFromStart(segment_top),
        target,
        next,
    );

    var i = fields.len;
    while (i > 0) {
        i -= 1;
        const field = fields[i];
        const label_local = self.scratch_local_ids.items.items[field_triplet_top + (i * 3)];
        const value_local = self.scratch_local_ids.items.items[field_triplet_top + (i * 3) + 1];
        const value_str_local = self.scratch_local_ids.items.items[field_triplet_top + (i * 3) + 2];
        const label_text = try std.fmt.allocPrint(self.allocator, "{s}: ", .{field.name.text(self.all_module_envs)});
        defer self.allocator.free(label_text);

        current = try self.lowerStrInspectLocalInto(value_local, field.type_idx, value_str_local, current);
        current = try self.lowerRefInto(value_local, .{ .field = .{
            .source = source,
            .field_idx = @intCast(i),
        } }, current);
        current = try self.lowerStrLiteralInto(label_local, label_text, current);
    }

    current = try self.lowerStrLiteralInto(close_local, " }", current);
    if (fields.len > 1) {
        current = try self.lowerStrLiteralInto(comma_local, ", ", current);
    }
    current = try self.lowerStrLiteralInto(open_local, "{ ", current);
    return current;
}

fn lowerBoxInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    inner_mono: Monotype.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const str_mono = self.store.monotype_store.primIdx(.str);
    const open_local = try self.freshSyntheticLocal(str_mono, false);
    const inner_local = try self.freshSyntheticLocal(inner_mono, false);
    const inner_str_local = try self.freshSyntheticLocal(str_mono, false);
    const close_local = try self.freshSyntheticLocal(str_mono, false);

    var current = try self.lowerConcatLocalsInto(&.{ open_local, inner_str_local, close_local }, target, next);
    current = try self.lowerStrInspectLocalInto(inner_local, inner_mono, inner_str_local, current);
    current = try self.lowerUnaryLowLevelInto(inner_local, .box_unbox, source, current);
    current = try self.lowerStrLiteralInto(close_local, ")", current);
    current = try self.lowerStrLiteralInto(open_local, "Box(", current);
    return current;
}

fn lowerTagUnionInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    source_mono: Monotype.Idx,
    tag_union: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    _ = source_mono;

    const tags = self.store.monotype_store.getTags(tag_union.tags);
    if (tags.len == 0) {
        std.debug.panic(
            "statement-only MIR str_inspect encountered an empty tag union",
            .{},
        );
    }

    const str_mono = self.store.monotype_store.primIdx(.str);
    const discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
    const lowered_branches = try self.allocator.alloc(MIR.SwitchBranch, tags.len);
    defer self.allocator.free(lowered_branches);

    for (tags, 0..) |tag, i| {
        const tag_text = tag.name.text(self.all_module_envs);
        const payload_monos = self.store.monotype_store.getIdxSpan(tag.payloads);
        const branch_body = if (payload_monos.len == 0) blk: {
            break :blk try self.lowerStrLiteralInto(target, tag_text, next);
        } else blk: {
            const scratch_top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(scratch_top);

            const name_local = try self.freshSyntheticLocal(str_mono, false);
            const open_local = try self.freshSyntheticLocal(str_mono, false);
            const close_local = try self.freshSyntheticLocal(str_mono, false);
            const comma_local = if (payload_monos.len > 1)
                try self.freshSyntheticLocal(str_mono, false)
            else
                open_local;

            const payload_pair_top = self.scratch_local_ids.top();
            for (payload_monos) |payload_mono| {
                try self.scratch_local_ids.append(try self.freshSyntheticLocal(payload_mono, false));
                try self.scratch_local_ids.append(try self.freshSyntheticLocal(str_mono, false));
            }

            const segment_top = self.scratch_local_ids.top();
            try self.scratch_local_ids.append(name_local);
            try self.scratch_local_ids.append(open_local);
            for (payload_monos, 0..) |_, payload_idx| {
                const payload_str_local = self.scratch_local_ids.items.items[payload_pair_top + (payload_idx * 2) + 1];
                try self.scratch_local_ids.append(payload_str_local);
                if (payload_idx + 1 < payload_monos.len) {
                    try self.scratch_local_ids.append(comma_local);
                }
            }
            try self.scratch_local_ids.append(close_local);

            var current = try self.lowerConcatLocalsInto(
                self.scratch_local_ids.sliceFromStart(segment_top),
                target,
                next,
            );

            var payload_idx = payload_monos.len;
            while (payload_idx > 0) {
                payload_idx -= 1;
                const payload_local = self.scratch_local_ids.items.items[payload_pair_top + (payload_idx * 2)];
                const payload_str_local = self.scratch_local_ids.items.items[payload_pair_top + (payload_idx * 2) + 1];
                current = try self.lowerStrInspectLocalInto(
                    payload_local,
                    payload_monos[payload_idx],
                    payload_str_local,
                    current,
                );
                current = try self.lowerRefInto(payload_local, .{ .tag_payload = .{
                    .source = source,
                    .payload_idx = @intCast(payload_idx),
                } }, current);
            }

            current = try self.lowerStrLiteralInto(close_local, ")", current);
            if (payload_monos.len > 1) {
                current = try self.lowerStrLiteralInto(comma_local, ", ", current);
            }
            current = try self.lowerStrLiteralInto(open_local, "(", current);
            current = try self.lowerStrLiteralInto(name_local, tag_text, current);
            break :blk current;
        };

        lowered_branches[i] = .{
            .value = @intCast(i),
            .body = branch_body,
        };
    }

    const impossible_default = try self.store.addCFStmt(self.allocator, .{ .crash = try self.store.insertString(
        self.allocator,
        "statement-only MIR invariant violated: unreachable tag discriminant during str_inspect",
    ) });
    const switch_stmt = try self.store.addCFStmt(self.allocator, .{ .switch_stmt = .{
        .scrutinee = discrim_local,
        .branches = try self.store.addSwitchBranches(self.allocator, lowered_branches),
        .default_branch = impossible_default,
    } });
    return self.lowerRefInto(discrim_local, .{ .discriminant = .{ .source = source } }, switch_stmt);
}

fn boolTagNamesForMonotype(
    self: *Self,
    mono_idx: Monotype.Idx,
) ?struct { true_name: Monotype.Name, false_name: Monotype.Name } {
    const mono = self.store.monotype_store.getMonotype(mono_idx);
    const module_env = self.all_module_envs[self.current_module_idx];
    const tags = switch (mono) {
        .tag_union => |tu| self.store.monotype_store.getTags(tu.tags),
        else => return null,
    };

    if (tags.len != 2) return null;

    var true_name: ?Monotype.Name = null;
    var false_name: ?Monotype.Name = null;
    for (tags) |tag| {
        if (self.store.monotype_store.getIdxSpan(tag.payloads).len != 0) return null;
        if (self.identsTagNameEquivalent(tag.name, module_env.idents.true_tag)) {
            true_name = tag.name;
        } else if (self.identsTagNameEquivalent(tag.name, module_env.idents.false_tag)) {
            false_name = tag.name;
        } else {
            return null;
        }
    }

    return .{
        .true_name = true_name orelse return null,
        .false_name = false_name orelse return null,
    };
}

fn tagDiscriminantForMonotypeName(self: *Self, mono_idx: Monotype.Idx, tag_name: Monotype.Name) u64 {
    const mono = self.store.monotype_store.getMonotype(mono_idx);
    const tags = switch (mono) {
        .tag_union => |tu| self.store.monotype_store.getTags(tu.tags),
        else => std.debug.panic(
            "statement-only MIR tag discriminant lookup expected tag union, found {s}",
            .{@tagName(mono)},
        ),
    };

    for (tags, 0..) |tag, i| {
        if (self.identsTagNameEquivalent(tag.name, tag_name)) {
            return @intCast(i);
        }
    }

    std.debug.panic(
        "statement-only MIR tag discriminant lookup could not find requested tag in monotype",
        .{},
    );
}

fn lowerSwitchOnDiscriminant(
    self: *Self,
    scrutinee_local: MIR.LocalId,
    scrutinee_mono: Monotype.Idx,
    true_tag: Monotype.Name,
    on_true: MIR.CFStmtId,
    on_false: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
    const switch_stmt = try self.store.addCFStmt(self.allocator, .{ .switch_stmt = .{
        .scrutinee = discrim_local,
        .branches = try self.store.addSwitchBranches(self.allocator, &.{.{
            .value = self.tagDiscriminantForMonotypeName(scrutinee_mono, true_tag),
            .body = on_true,
        }}),
        .default_branch = on_false,
    } });
    return self.lowerRefInto(discrim_local, .{ .discriminant = .{ .source = scrutinee_local } }, switch_stmt);
}

fn lowerBoolInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    bool_mono: Monotype.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const tag_names = boolTagNamesForMonotype(self, bool_mono) orelse std.debug.panic(
        "statement-only MIR str_inspect expected Bool monotype for local {d}",
        .{@intFromEnum(source)},
    );

    const true_body = try self.lowerStrLiteralInto(target, "True", next);
    const false_body = try self.lowerStrLiteralInto(target, "False", next);
    return self.lowerSwitchOnDiscriminant(source, bool_mono, tag_names.true_name, true_body, false_body);
}

fn lowerStrInspectLocalInto(
    self: *Self,
    source: MIR.LocalId,
    source_mono: Monotype.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (self.store.monotype_store.isOpaque(source_mono)) {
        return self.lowerStrLiteralInto(target, "<opaque>", next);
    }

    return switch (self.store.monotype_store.getMonotype(source_mono)) {
        .prim => |prim| switch (prim) {
            .str => self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
                .target = target,
                .op = .str_inspect,
                .args = try self.store.addLocalSpan(self.allocator, &.{source}),
                .next = next,
            } }),
            else => |p| self.lowerUnaryLowLevelInto(
                target,
                toStrLowLevelForPrim(p) orelse std.debug.panic(
                    "statement-only MIR str_inspect has no primitive renderer for {s}",
                    .{@tagName(p)},
                ),
                source,
                next,
            ),
        },
        .unit => self.lowerStrLiteralInto(target, "{}", next),
        .record => |record| self.lowerRecordInspectLocalInto(source, record, target, next),
        .tuple => |tuple_data| self.lowerTupleInspectLocalInto(source, tuple_data, target, next),
        .tag_union => |tag_union| blk: {
            if (boolTagNamesForMonotype(self, source_mono) != null) {
                break :blk try self.lowerBoolInspectLocalInto(source, source_mono, target, next);
            }
            break :blk try self.lowerTagUnionInspectLocalInto(source, source_mono, tag_union, target, next);
        },
        .box => |box_data| self.lowerBoxInspectLocalInto(source, box_data.inner, target, next),
        .func => self.lowerStrLiteralInto(target, "<function>", next),
        .list => |list_data| self.lowerListInspectLocalInto(source, list_data, target, next),
        .recursive_placeholder => {
            std.debug.panic("recursive_placeholder survived monotype construction", .{});
        },
    };
}

fn lookupSymbolForPattern(
    self: *Self,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!MIR.Symbol {
    const module_env = self.all_module_envs[module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);
    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |assign| assign.ident,
        .as => |as_pattern| as_pattern.ident,
        else => std.debug.panic(
            "statement-only MIR lookup symbol expected assign/as pattern for pattern {d}",
            .{@intFromEnum(pattern_idx)},
        ),
    };
    return self.internSymbol(module_idx, ident_idx);
}

fn lowerExactCallableLookupInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!?MIR.CFStmtId {
    const target_monotype = self.store.getLocal(target).monotype;
    if (self.store.monotype_store.getMonotype(target_monotype) != .func) return null;

    const callable_inst_id = (try self.lookupMonomorphizedValueExprCallableInstForMonotype(
        expr_idx,
        target_monotype,
        self.current_module_idx,
    )) orelse blk: {
        if (self.lookupMonomorphizedValueExprCallableInst(expr_idx)) |resolved| {
            if (!try self.callableInstMatchesFnMonotype(resolved, target_monotype, self.current_module_idx)) {
                std.debug.panic(
                    "statement-only MIR invariant violated: exact callable inst for expr {d} did not match target function monotype",
                    .{@intFromEnum(expr_idx)},
                );
            }
            break :blk resolved;
        }

        if (self.lookupMonomorphizedValueExprCallableInsts(expr_idx)) |resolved_set| {
            if (resolved_set.len == 1) {
                if (!try self.callableInstMatchesFnMonotype(resolved_set[0], target_monotype, self.current_module_idx)) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: sole callable-inst candidate for expr {d} did not match target function monotype",
                        .{@intFromEnum(expr_idx)},
                    );
                }
                break :blk resolved_set[0];
            }
        }

        std.debug.panic(
            "statement-only MIR TODO: function-valued global lookup expr {d} must lower from an exact callable inst",
            .{@intFromEnum(expr_idx)},
        );
    };

    return @as(?MIR.CFStmtId, try self.lowerResolvedCallableInstValueInto(callable_inst_id, target, next));
}

fn lowerLookupLocalInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (self.lookupExistingPatternLocal(lookup.pattern_idx)) |source| {
        return self.lowerLocalAliasInto(target, source, next);
    }

    if (try self.lowerExactCallableLookupInto(expr_idx, target, next)) |lowered| {
        return lowered;
    }

    if (self.monomorphization.getPatternSourceExpr(self.current_module_idx, lookup.pattern_idx)) |source| {
        if (source.module_idx == self.current_module_idx and source.expr_idx == expr_idx) {
            std.debug.panic(
                "statement-only MIR invariant violated: local lookup expr {d} resolved to itself as a pattern source",
                .{@intFromEnum(expr_idx)},
            );
        }
        return self.lowerCapturedSourceExprInto(source, target, next);
    }

    const def_idx = findDefByPattern(self.all_module_envs[self.current_module_idx], lookup.pattern_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: local lookup pattern {d} had no in-scope local or top-level def",
        .{@intFromEnum(lookup.pattern_idx)},
    );
    const symbol = try self.lookupSymbolForPattern(self.current_module_idx, lookup.pattern_idx);
    return self.materializeTopLevelDefInto(self.current_module_idx, def_idx, symbol, target, next);
}

fn lowerLookupExternalInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    module_env: *const ModuleEnv,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (try self.lowerExactCallableLookupInto(expr_idx, target, next)) |lowered| {
        return lowered;
    }

    const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse unreachable;
    if (!self.all_module_envs[target_module_idx].store.isDefNode(lookup.target_node_idx)) {
        std.debug.panic(
            "statement-only MIR TODO: external lookup node {d} in module {d} does not lower to a def-backed value yet",
            .{ lookup.target_node_idx, target_module_idx },
        );
    }
    const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
    const symbol = try self.internExternalDefSymbol(target_module_idx, lookup.target_node_idx);
    return self.materializeTopLevelDefInto(target_module_idx, def_idx, symbol, target, next);
}

fn lowerLookupRequiredInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    module_env: *const ModuleEnv,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (try self.lowerExactCallableLookupInto(expr_idx, target, next)) |lowered| {
        return lowered;
    }

    const target_lookup = self.resolveRequiredLookupTarget(module_env, lookup) orelse std.debug.panic(
        "statement-only MIR TODO: required lookup could not be resolved to an app export",
        .{},
    );
    const def = self.all_module_envs[target_lookup.module_idx].store.getDef(target_lookup.def_idx);
    const symbol = try self.lookupSymbolForPattern(target_lookup.module_idx, def.pattern);
    return self.materializeTopLevelDefInto(target_lookup.module_idx, target_lookup.def_idx, symbol, target, next);
}

fn ensureNamedConstDefRegistered(
    self: *Self,
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    symbol: MIR.Symbol,
) Allocator.Error!void {
    if (self.store.getConstDefForSymbol(symbol)) |_| return;

    const key = symbol.raw();
    const gop = try self.in_progress_const_defs.getOrPut(key);
    if (gop.found_existing) {
        std.debug.panic(
            "statement-only MIR TODO: recursive named constant lowering for symbol {d} is not implemented yet",
            .{key},
        );
    }
    defer _ = self.in_progress_const_defs.remove(key);

    const saved_module_idx = self.current_module_idx;
    const saved_pattern_scope = self.current_pattern_scope;
    const saved_callable_context = self.current_callable_inst_context;
    const saved_root_context = self.current_root_source_expr_context;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_scratches_module_idx = self.mono_scratches.module_idx;

    const target_env = self.all_module_envs[module_idx];
    const def = target_env.store.getDef(def_idx);

    self.current_module_idx = module_idx;
    self.current_pattern_scope = 0;
    self.current_callable_inst_context = .none;
    self.current_root_source_expr_context = def.expr;
    self.mono_scratches.ident_store = target_env.getIdentStoreConst();
    self.mono_scratches.module_env = target_env;
    self.mono_scratches.module_idx = module_idx;

    defer self.current_module_idx = saved_module_idx;
    defer self.current_pattern_scope = saved_pattern_scope;
    defer self.current_callable_inst_context = saved_callable_context;
    defer self.current_root_source_expr_context = saved_root_context;
    defer self.mono_scratches.ident_store = saved_ident_store;
    defer self.mono_scratches.module_env = saved_module_env;
    defer self.mono_scratches.module_idx = saved_scratches_module_idx;

    _ = try self.lowerNamedConstDefFromSourceExpr(def.expr, symbol);
}

fn materializeTopLevelDefInto(
    self: *Self,
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    symbol: MIR.Symbol,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const target_monotype = self.store.getLocal(target).monotype;
    if (self.store.monotype_store.getMonotype(target_monotype) == .func) {
        const saved_module_idx = self.current_module_idx;
        const saved_pattern_scope = self.current_pattern_scope;
        const saved_callable_context = self.current_callable_inst_context;
        const saved_root_context = self.current_root_source_expr_context;
        const saved_ident_store = self.mono_scratches.ident_store;
        const saved_module_env = self.mono_scratches.module_env;
        const saved_scratches_module_idx = self.mono_scratches.module_idx;

        const target_env = self.all_module_envs[module_idx];
        const def = target_env.store.getDef(def_idx);

        self.current_module_idx = module_idx;
        self.current_pattern_scope = 0;
        self.current_callable_inst_context = .none;
        self.current_root_source_expr_context = def.expr;
        self.mono_scratches.ident_store = target_env.getIdentStoreConst();
        self.mono_scratches.module_env = target_env;
        self.mono_scratches.module_idx = module_idx;

        defer self.current_module_idx = saved_module_idx;
        defer self.current_pattern_scope = saved_pattern_scope;
        defer self.current_callable_inst_context = saved_callable_context;
        defer self.current_root_source_expr_context = saved_root_context;
        defer self.mono_scratches.ident_store = saved_ident_store;
        defer self.mono_scratches.module_env = saved_module_env;
        defer self.mono_scratches.module_idx = saved_scratches_module_idx;

        return (try self.lowerExactCallableLookupInto(def.expr, target, next)) orelse
            try self.lowerCirExprInto(def.expr, target, next);
    }

    try self.ensureNamedConstDefRegistered(module_idx, def_idx, symbol);
    return self.store.addCFStmt(self.allocator, .{ .assign_symbol = .{
        .target = target,
        .symbol = symbol,
        .next = next,
    } });
}

fn lowerLambda(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    lambda: CIR.Expr.Lambda,
    fn_monotype: Monotype.Idx,
) Allocator.Error!MIR.LambdaId {
    const cache_key = self.packCallableCacheKey(expr_idx, fn_monotype);
    if (self.lowered_callable_lambdas.get(cache_key)) |cached| return cached;

    const ret_monotype = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR lambda expected function monotype for expr {d}",
            .{@intFromEnum(expr_idx)},
        ),
    };

    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = self.freshStatementPatternScope();
    defer self.current_pattern_scope = saved_scope;
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;

    const param_patterns = module_env.store.slicePatterns(lambda.args);
    const params = try self.lowerLambdaParamLocals(module_env, lambda.args);
    const param_locals = self.store.getLocalSpan(params);
    for (param_patterns) |pattern_idx| {
        try self.predeclarePatternLocals(module_env, pattern_idx);
    }
    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    var body = try self.lowerCirExprInto(lambda.body, result_local, ret_stmt);

    var i = param_patterns.len;
    while (i > 0) {
        i -= 1;
        body = try self.lowerPatternBindingLocalInto(module_env, param_patterns[i], param_locals[i], body);
    }

    const lowered = try self.store.addLambda(self.allocator, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = null,
        .callable_bindings = try self.lowerCurrentCallableBindings(param_locals),
        .recursion = .not_recursive,
        .hosted = null,
    });
    try self.lowered_callable_lambdas.put(cache_key, lowered);
    return lowered;
}

fn lowerLambdaParamLocals(
    self: *Self,
    module_env: *const ModuleEnv,
    span: CIR.Pattern.Span,
) Allocator.Error!MIR.LocalSpan {
    const cir_ids = module_env.store.slicePatterns(span);
    if (cir_ids.len == 0) return MIR.LocalSpan.empty();

    const scratch_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(scratch_top);

    for (cir_ids) |pattern_idx| {
        const monotype = try self.monotypeFromTypeVarWithBindings(
            self.current_module_idx,
            self.types_store,
            ModuleEnv.varFrom(pattern_idx),
            &self.type_var_seen,
        );
        try self.bindPatternMonotypes(module_env, pattern_idx, monotype);
        try self.scratch_local_ids.append(try self.freshSyntheticLocal(monotype, false));
    }

    return self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(scratch_top));
}

fn appendCallableBindingUnique(
    self: *Self,
    out: *std.ArrayList(MIR.CallableBinding),
    binding: MIR.CallableBinding,
) Allocator.Error!void {
    for (out.items) |existing| {
        if (existing.source_param != binding.source_param) continue;
        if (!callableProjectionSpansEqual(self.store, existing.projections, binding.projections)) continue;
        if (existing.lambda != binding.lambda or existing.requires_hidden_capture != binding.requires_hidden_capture) {
            std.debug.panic(
                "statement-only MIR callable binding collision for parameter local {d}",
                .{@intFromEnum(binding.source_param)},
            );
        }
        return;
    }

    try out.append(self.allocator, binding);
}

fn callableProjectionSpansEqual(
    store: *const MIR.Store,
    lhs: MIR.CallableProjectionSpan,
    rhs: MIR.CallableProjectionSpan,
) bool {
    const lhs_items = store.getCallableProjectionSpan(lhs);
    const rhs_items = store.getCallableProjectionSpan(rhs);
    if (lhs_items.len != rhs_items.len) return false;

    for (lhs_items, rhs_items) |lhs_item, rhs_item| {
        switch (lhs_item) {
            .field => |lhs_idx| switch (rhs_item) {
                .field => |rhs_idx| if (lhs_idx != rhs_idx) return false,
                else => return false,
            },
            .tag_payload => |lhs_idx| switch (rhs_item) {
                .tag_payload => |rhs_idx| if (lhs_idx != rhs_idx) return false,
                else => return false,
            },
            .nominal => switch (rhs_item) {
                .nominal => {},
                else => return false,
            },
        }
    }

    return true;
}

fn appendCurrentCallableBindings(
    self: *Self,
    bindings: *std.ArrayList(MIR.CallableBinding),
    param_locals: []const MIR.LocalId,
) Allocator.Error!void {
    if (self.current_callable_inst_context.isNone()) return;

    const callable_inst = self.monomorphization.getCallableInst(self.current_callable_inst_context);
    const specs = self.monomorphization.getCallableParamSpecEntries(callable_inst.callable_param_specs);
    if (specs.len == 0) return;

    for (specs) |spec| {
        if (spec.param_index >= param_locals.len) {
            std.debug.panic(
                "statement-only MIR callable binding param index {d} exceeds lambda arity {d}",
                .{ spec.param_index, param_locals.len },
            );
        }

        const source_param = param_locals[spec.param_index];
        var current_mono = self.store.getLocal(source_param).monotype;
        var projections = std.ArrayList(MIR.CallableProjection).empty;
        defer projections.deinit(self.allocator);

        for (self.monomorphization.getCallableParamProjectionEntries(spec.projections)) |projection| {
            switch (projection) {
                .field => |field_name| {
                    const record_fields = switch (self.store.monotype_store.getMonotype(current_mono)) {
                        .record => |record_mono| self.store.monotype_store.getFields(record_mono.fields),
                        else => std.debug.panic(
                            "statement-only MIR callable binding field projection expected record monotype",
                            .{},
                        ),
                    };
                    const field_idx = self.recordFieldIndexByName(field_name.ident, record_fields);
                    current_mono = record_fields[field_idx].type_idx;
                    try projections.append(self.allocator, .{ .field = @intCast(field_idx) });
                },
                .tuple_elem => |elem_idx| {
                    const elem_monos = switch (self.store.monotype_store.getMonotype(current_mono)) {
                        .tuple => |tuple_mono| self.store.monotype_store.getIdxSpan(tuple_mono.elems),
                        else => std.debug.panic(
                            "statement-only MIR callable binding tuple projection expected tuple monotype",
                            .{},
                        ),
                    };
                    if (elem_idx >= elem_monos.len) {
                        std.debug.panic(
                            "statement-only MIR callable binding tuple elem {d} exceeds tuple arity {d}",
                            .{ elem_idx, elem_monos.len },
                        );
                    }
                    current_mono = elem_monos[elem_idx];
                    try projections.append(self.allocator, .{ .field = @intCast(elem_idx) });
                },
            }
        }

        const callable_inst_set = self.monomorphization.getCallableInstSet(spec.callable_inst_set_id);
        const callable_members = self.monomorphization.getCallableInstSetMembers(callable_inst_set.members);
        if (callable_members.len != 1) {
            std.debug.panic(
                "statement-only MIR TODO: callable bindings with {d} exact callable members are not implemented yet",
                .{callable_members.len},
            );
        }

        const binding_callable_inst = callable_members[0];
        const binding_lambda = try self.lowerResolvedCallableInstLambda(binding_callable_inst);
        try self.appendCallableBindingUnique(&bindings, .{
            .source_param = source_param,
            .projections = try self.store.addCallableProjectionSpan(self.allocator, projections.items),
            .lambda = binding_lambda,
            .requires_hidden_capture = try self.callableInstProducesClosureValue(binding_callable_inst),
        });
    }
}

fn lowerClosureLambda(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    fn_monotype: Monotype.Idx,
) Allocator.Error!MIR.LambdaId {
    const cache_key = self.packCallableCacheKey(expr_idx, fn_monotype);
    if (self.lowered_callable_lambdas.get(cache_key)) |cached| return cached;

    const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
    if (lambda_expr != .e_lambda) {
        std.debug.panic(
            "statement-only MIR closure expected lambda body expr for {d}, found {s}",
            .{ @intFromEnum(expr_idx), @tagName(lambda_expr) },
        );
    }

    const ret_monotype = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR closure expected function monotype for expr {d}",
            .{@intFromEnum(expr_idx)},
        ),
    };

    const captures = module_env.store.sliceCaptures(closure.captures);
    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = self.freshStatementPatternScope();
    defer self.current_pattern_scope = saved_scope;
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;

    const param_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args);
    const params = try self.lowerLambdaParamLocals(module_env, lambda_expr.e_lambda.args);
    const param_locals = self.store.getLocalSpan(params);
    for (param_patterns) |pattern_idx| {
        try self.predeclarePatternLocals(module_env, pattern_idx);
    }

    const capture_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(capture_top);

    for (captures) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_local = try self.patternToLocal(capture.pattern_idx);
        try self.scratch_local_ids.append(capture_local);
    }

    const captures_tuple_monotype = if (captures.len == 0)
        Monotype.Idx.none
    else blk: {
        var capture_monotypes = try std.ArrayList(Monotype.Idx).initCapacity(self.allocator, captures.len);
        defer capture_monotypes.deinit(self.allocator);
        for (self.scratch_local_ids.sliceFromStart(capture_top)) |capture_local| {
            capture_monotypes.appendAssumeCapacity(self.store.getLocal(capture_local).monotype);
        }
        break :blk try self.tupleMonotypeForFields(capture_monotypes.items);
    };

    const captures_param_local = if (captures.len == 0)
        null
    else
        try self.freshSyntheticLocal(captures_tuple_monotype, false);
    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    var body = try self.lowerCirExprInto(lambda_expr.e_lambda.body, result_local, ret_stmt);

    if (captures_param_local) |local| {
        var i = captures.len;
        while (i > 0) {
            i -= 1;
            const capture_local = self.scratch_local_ids.items.items[capture_top + i];
            body = try self.lowerRefInto(capture_local, .{ .field = .{
                .source = local,
                .field_idx = @intCast(i),
            } }, body);
        }
    }

    var param_i = param_patterns.len;
    while (param_i > 0) {
        param_i -= 1;
        body = try self.lowerPatternBindingLocalInto(module_env, param_patterns[param_i], param_locals[param_i], body);
    }

    var callable_bindings = std.ArrayList(MIR.CallableBinding).empty;
    defer callable_bindings.deinit(self.allocator);
    try self.appendCurrentCallableBindings(&callable_bindings, param_locals);

    const lowered = try self.store.addLambda(self.allocator, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = captures_param_local,
        .callable_bindings = try self.store.addCallableBindingSpan(self.allocator, callable_bindings.items),
        .recursion = .not_recursive,
        .hosted = null,
    });
    try self.lowered_callable_lambdas.put(cache_key, lowered);
    return lowered;
}

fn recursiveCallableInstForPattern(
    recursive_members: []const RecursiveGroupMember,
    pattern_idx: CIR.Pattern.Idx,
) ?Monomorphize.CallableInstId {
    for (recursive_members) |member| {
        if (member.binding_pattern == pattern_idx) return member.callable_inst_id;
    }
    return null;
}

fn planClosureLowering(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    callable_inst_id: Monomorphize.CallableInstId,
) Allocator.Error!ClosureLowerPlan {
    var recursive_members = std.ArrayList(RecursiveGroupMember).empty;
    errdefer recursive_members.deinit(self.allocator);

    const current_callable_inst = self.monomorphization.getCallableInst(callable_inst_id);
    for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_callable_inst_id = self.monomorphization.getClosureCaptureCallableInst(
            callable_inst_id,
            self.current_module_idx,
            expr_idx,
            capture.pattern_idx,
        ) orelse continue;

        const capture_template = self.monomorphization.getCallableTemplate(
            self.monomorphization.getCallableInst(capture_callable_inst_id).template,
        );
        const binding_pattern = capture_template.binding_pattern orelse continue;
        if (binding_pattern != capture.pattern_idx) continue;

        const shares_recursive_environment =
            capture_callable_inst_id == callable_inst_id or
            capture_callable_inst_id == current_callable_inst.defining_context_callable_inst or
            (!current_callable_inst.defining_context_callable_inst.isNone() and
                self.monomorphization.getCallableInst(capture_callable_inst_id).defining_context_callable_inst ==
                    current_callable_inst.defining_context_callable_inst);
        if (!shares_recursive_environment) continue;

        var exists = false;
        for (recursive_members.items) |existing| {
            if (existing.binding_pattern == binding_pattern and existing.callable_inst_id == capture_callable_inst_id) {
                exists = true;
                break;
            }
        }
        if (exists) continue;

        try recursive_members.append(self.allocator, .{
            .binding_pattern = binding_pattern,
            .callable_inst_id = capture_callable_inst_id,
        });
    }

    return .{ .recursive_members = recursive_members };
}

fn closureRuntimeCaptureCount(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    callable_inst_id: ?Monomorphize.CallableInstId,
) Allocator.Error!usize {
    if (callable_inst_id) |id| {
        var lower_plan = try self.planClosureLowering(module_env, expr_idx, closure, id);
        defer lower_plan.deinit(self.allocator);

        var count: usize = 0;
        for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
            const capture = module_env.store.getCapture(capture_idx);
            if (recursiveCallableInstForPattern(lower_plan.recursive_members.items, capture.pattern_idx) != null) continue;
            count += 1;
        }
        return count;
    }

    return module_env.store.sliceCaptures(closure.captures).len;
}

fn reserveResolvedCallableInstClosureLambda(
    self: *Self,
    callable_inst_id: Monomorphize.CallableInstId,
    fn_monotype: Monotype.Idx,
    source_region: Region,
) Allocator.Error!MIR.LambdaId {
    const callable_inst_key = @intFromEnum(callable_inst_id);
    if (self.lowered_callable_insts.get(callable_inst_key)) |existing| return existing;
    if (self.reserved_callable_insts.get(callable_inst_key)) |existing| return existing;

    const ret_monotype = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR reserved closure lambda expected function monotype for callable inst {d}",
            .{@intFromEnum(callable_inst_id)},
        ),
    };

    const unresolved_body = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });
    const reserved = try self.store.addLambda(self.allocator, .{
        .fn_monotype = fn_monotype,
        .params = MIR.LocalSpan.empty(),
        .body = unresolved_body,
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = source_region,
        .captures_param = null,
        .callable_bindings = MIR.CallableBindingSpan.empty(),
        .recursion = .not_recursive,
        .hosted = null,
    });
    try self.reserved_callable_insts.put(callable_inst_key, reserved);
    return reserved;
}

fn lowerReservedTrivialClosureLambda(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    fn_monotype: Monotype.Idx,
    callable_inst_id: Monomorphize.CallableInstId,
    reserved_lambda: MIR.LambdaId,
) Allocator.Error!MIR.LambdaId {
    const cache_key = self.packCallableCacheKey(expr_idx, fn_monotype);
    if (self.lowered_callable_lambdas.get(cache_key)) |cached| {
        if (cached == reserved_lambda) {
            if (self.in_progress_callable_insts.get(@intFromEnum(callable_inst_id)) != null) return cached;
            if (self.reserved_callable_insts.get(@intFromEnum(callable_inst_id)) == null) return cached;
        } else {
            return cached;
        }
    }
    try self.lowered_callable_lambdas.put(cache_key, reserved_lambda);

    const lambda_expr = module_env.store.getExpr(closure.lambda_idx);
    if (lambda_expr != .e_lambda) {
        std.debug.panic(
            "statement-only MIR reserved closure expected lambda body expr for {d}, found {s}",
            .{ @intFromEnum(expr_idx), @tagName(lambda_expr) },
        );
    }

    const ret_monotype = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR reserved closure expected function monotype for expr {d}",
            .{@intFromEnum(expr_idx)},
        ),
    };

    var lower_plan = try self.planClosureLowering(module_env, expr_idx, closure, callable_inst_id);
    defer lower_plan.deinit(self.allocator);

    const callable_inst_key = @intFromEnum(callable_inst_id);
    const already_in_progress = self.in_progress_callable_insts.get(callable_inst_key) != null;
    if (!already_in_progress) {
        try self.in_progress_callable_insts.put(callable_inst_key, reserved_lambda);
    }
    defer {
        if (!already_in_progress) {
            _ = self.in_progress_callable_insts.remove(callable_inst_key);
        }
    }

    const captures = module_env.store.sliceCaptures(closure.captures);
    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = self.freshStatementPatternScope();
    defer self.current_pattern_scope = saved_scope;
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;

    const param_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args);
    const params = try self.lowerLambdaParamLocals(module_env, lambda_expr.e_lambda.args);
    const param_locals = self.store.getLocalSpan(params);
    for (param_patterns) |pattern_idx| {
        try self.predeclarePatternLocals(module_env, pattern_idx);
    }

    const capture_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(capture_top);
    var recursive_captures = std.ArrayList(struct {
        local: MIR.LocalId,
        callable_inst_id: Monomorphize.CallableInstId,
    }).empty;
    defer recursive_captures.deinit(self.allocator);

    for (captures) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_local = try self.patternToLocal(capture.pattern_idx);
        if (recursiveCallableInstForPattern(lower_plan.recursive_members.items, capture.pattern_idx)) |member_callable_inst_id| {
            try recursive_captures.append(self.allocator, .{
                .local = capture_local,
                .callable_inst_id = member_callable_inst_id,
            });
            continue;
        }
        try self.scratch_local_ids.append(capture_local);
    }

    const nonrecursive_capture_locals = self.scratch_local_ids.sliceFromStart(capture_top);
    const captures_tuple_monotype = if (nonrecursive_capture_locals.len == 0)
        Monotype.Idx.none
    else blk: {
        var capture_monotypes = try std.ArrayList(Monotype.Idx).initCapacity(self.allocator, nonrecursive_capture_locals.len);
        defer capture_monotypes.deinit(self.allocator);
        for (nonrecursive_capture_locals) |capture_local| {
            capture_monotypes.appendAssumeCapacity(self.store.getLocal(capture_local).monotype);
        }
        break :blk try self.tupleMonotypeForFields(capture_monotypes.items);
    };

    const captures_param_local = if (nonrecursive_capture_locals.len == 0)
        null
    else
        try self.freshSyntheticLocal(captures_tuple_monotype, false);
    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    var body = try self.lowerCirExprInto(lambda_expr.e_lambda.body, result_local, ret_stmt);

    const nonrecursive_capture_span = try self.store.addLocalSpan(self.allocator, nonrecursive_capture_locals);

    var recursive_i = recursive_captures.items.len;
    while (recursive_i > 0) {
        recursive_i -= 1;
        const recursive_capture = recursive_captures.items[recursive_i];
        const member_lambda = try self.lowerResolvedCallableInstLambda(recursive_capture.callable_inst_id);
        body = if (!(try self.callableInstProducesClosureValue(recursive_capture.callable_inst_id)))
            try self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
                .target = recursive_capture.local,
                .lambda = member_lambda,
                .next = body,
            } })
        else
            try self.store.addCFStmt(self.allocator, .{ .assign_closure = .{
                .target = recursive_capture.local,
                .lambda = member_lambda,
                .captures = nonrecursive_capture_span,
                .next = body,
            } });
    }

    if (captures_param_local) |local| {
        var i = nonrecursive_capture_locals.len;
        while (i > 0) {
            i -= 1;
            body = try self.lowerRefInto(nonrecursive_capture_locals[i], .{ .field = .{
                .source = local,
                .field_idx = @intCast(i),
            } }, body);
        }
    }

    var param_i = param_patterns.len;
    while (param_i > 0) {
        param_i -= 1;
        body = try self.lowerPatternBindingLocalInto(module_env, param_patterns[param_i], param_locals[param_i], body);
    }

    var callable_bindings = std.ArrayList(MIR.CallableBinding).empty;
    defer callable_bindings.deinit(self.allocator);
    try self.appendCurrentCallableBindings(&callable_bindings, param_locals);

    if (captures_param_local) |captures_param| {
        var capture_field_index: u32 = 0;
        for (captures) |capture_idx| {
            const capture = module_env.store.getCapture(capture_idx);
            if (recursiveCallableInstForPattern(lower_plan.recursive_members.items, capture.pattern_idx) != null) continue;

            if (self.monomorphization.getClosureCaptureCallableInst(
                callable_inst_id,
                self.current_module_idx,
                expr_idx,
                capture.pattern_idx,
            )) |capture_callable_inst_id| {
                const capture_lambda = try self.lowerResolvedCallableInstLambda(capture_callable_inst_id);
                try self.appendCallableBindingUnique(&callable_bindings, .{
                    .source_param = captures_param,
                    .projections = try self.store.addCallableProjectionSpan(self.allocator, &.{.{ .field = capture_field_index }}),
                    .lambda = capture_lambda,
                    .requires_hidden_capture = try self.callableInstProducesClosureValue(capture_callable_inst_id),
                });
            }

            capture_field_index += 1;
        }
    }

    self.store.getLambdaPtr(reserved_lambda).* = .{
        .fn_monotype = fn_monotype,
        .params = params,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = captures_param_local,
        .callable_bindings = try self.store.addCallableBindingSpan(self.allocator, callable_bindings.items),
        .recursion = if (recursive_captures.items.len != 0) .recursive else .not_recursive,
        .hosted = null,
    };

    return reserved_lambda;
}

fn callableInstProducesClosureValue(
    self: *Self,
    callable_inst_id: Monomorphize.CallableInstId,
) Allocator.Error!bool {
    const callable_inst = self.monomorphization.getCallableInst(callable_inst_id);
    const template = self.monomorphization.getCallableTemplate(callable_inst.template);
    return switch (template.kind) {
        .closure => blk: {
            const module_env = self.all_module_envs[template.module_idx];
            const template_expr = module_env.store.getExpr(template.cir_expr);
            const closure = switch (template_expr) {
                .e_closure => |closure| closure,
                else => std.debug.panic(
                    "statement-only MIR callable-inst closure lowering expected closure expr for callable inst {d}, found {s}",
                    .{ @intFromEnum(callable_inst_id), @tagName(template_expr) },
                ),
            };
            break :blk (try self.closureRuntimeCaptureCount(
                module_env,
                template.cir_expr,
                closure,
                callable_inst_id,
            )) != 0;
        },
        .lambda, .top_level_def, .hosted_lambda => false,
    };
}

fn lowerResolvedCallableInstValueInto(
    self: *Self,
    callable_inst_id: Monomorphize.CallableInstId,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const lambda_id = try self.lowerResolvedCallableInstLambda(callable_inst_id);
    if (!(try self.callableInstProducesClosureValue(callable_inst_id))) {
        return self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
            .target = target,
            .lambda = lambda_id,
            .next = next,
        } });
    }

    const callable_inst = self.monomorphization.getCallableInst(callable_inst_id);
    const template = self.monomorphization.getCallableTemplate(callable_inst.template);
    if (template.kind != .closure) {
        std.debug.panic(
            "statement-only MIR invariant violated: callable inst {d} required runtime captures but template kind was {s}",
            .{ @intFromEnum(callable_inst_id), @tagName(template.kind) },
        );
    }

    const module_idx = template.module_idx;
    const module_env = self.all_module_envs[module_idx];
    const template_expr = module_env.store.getExpr(template.cir_expr);
    const closure = switch (template_expr) {
        .e_closure => |closure| closure,
        else => std.debug.panic(
            "statement-only MIR invariant violated: callable inst {d} required runtime captures but template expr was {s}",
            .{ @intFromEnum(callable_inst_id), @tagName(template_expr) },
        ),
    };

    var lower_plan = try self.planClosureLowering(module_env, template.cir_expr, closure, callable_inst_id);
    defer lower_plan.deinit(self.allocator);

    const switching_module = module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    if (switching_module) {
        self.current_module_idx = module_idx;
        self.types_store = &module_env.types;
        self.mono_scratches.ident_store = module_env.getIdentStoreConst();
        self.mono_scratches.module_env = module_env;
        self.mono_scratches.module_idx = module_idx;
    }
    defer if (switching_module) {
        self.current_module_idx = saved_module_idx;
        self.types_store = saved_types_store;
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
        self.mono_scratches.module_idx = saved_mono_module_idx;
    };

    const capture_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(capture_top);

    var capture_materializations = std.ArrayList(union(enum) {
        top_level_def: struct {
            local: MIR.LocalId,
            def_idx: CIR.Def.Idx,
            symbol: MIR.Symbol,
            module_idx: u32,
        },
        source_expr: struct {
            local: MIR.LocalId,
            source: Monomorphize.ExprSource,
        },
    }).empty;
    defer capture_materializations.deinit(self.allocator);

    for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        if (recursiveCallableInstForPattern(lower_plan.recursive_members.items, capture.pattern_idx) != null) continue;

        const capture_local = if (self.lookupExistingPatternLocal(capture.pattern_idx)) |existing|
            existing
        else blk: {
            const local = try self.patternToLocal(capture.pattern_idx);
            if (self.monomorphization.getPatternSourceExpr(module_idx, capture.pattern_idx)) |source| {
                try capture_materializations.append(self.allocator, .{ .source_expr = .{
                    .local = local,
                    .source = source,
                } });
                break :blk local;
            }

            const def_idx = findDefByPattern(module_env, capture.pattern_idx) orelse std.debug.panic(
                "statement-only MIR callable inst {d} capture pattern {d} was not in scope, had no recorded source expr, and was not a top-level def",
                .{ @intFromEnum(callable_inst_id), @intFromEnum(capture.pattern_idx) },
            );
            try capture_materializations.append(self.allocator, .{ .top_level_def = .{
                .local = local,
                .def_idx = def_idx,
                .symbol = try self.lookupSymbolForPattern(module_idx, capture.pattern_idx),
                .module_idx = module_idx,
            } });
            break :blk local;
        };

        try self.scratch_local_ids.append(capture_local);
    }

    const runtime_captures = self.scratch_local_ids.sliceFromStart(capture_top);
    var current = if (runtime_captures.len == 0)
        try self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
            .target = target,
            .lambda = lambda_id,
            .next = next,
        } })
    else
        try self.store.addCFStmt(self.allocator, .{ .assign_closure = .{
            .target = target,
            .lambda = lambda_id,
            .captures = try self.store.addLocalSpan(self.allocator, runtime_captures),
            .next = next,
        } });

    var i = capture_materializations.items.len;
    while (i > 0) {
        i -= 1;
        switch (capture_materializations.items[i]) {
            .top_level_def => |materialization| {
                current = try self.materializeTopLevelDefInto(
                    materialization.module_idx,
                    materialization.def_idx,
                    materialization.symbol,
                    materialization.local,
                    current,
                );
            },
            .source_expr => |materialization| {
                current = try self.lowerCapturedSourceExprInto(
                    materialization.source,
                    materialization.local,
                    current,
                );
            },
        }
    }

    return current;
}

fn lowerCapturedSourceExprInto(
    self: *Self,
    source: Monomorphize.ExprSource,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (source.module_idx == self.current_module_idx) {
        return self.lowerCirExprInto(source.expr_idx, target, next);
    }

    const module_env = self.all_module_envs[source.module_idx];
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;

    self.current_module_idx = source.module_idx;
    self.types_store = &module_env.types;
    self.mono_scratches.ident_store = module_env.getIdentStoreConst();
    self.mono_scratches.module_env = module_env;
    self.mono_scratches.module_idx = source.module_idx;
    defer {
        self.current_module_idx = saved_module_idx;
        self.types_store = saved_types_store;
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
        self.mono_scratches.module_idx = saved_mono_module_idx;
    }

    return self.lowerCirExprInto(source.expr_idx, target, next);
}

fn lowerResolvedCallableInstLambda(
    self: *Self,
    callable_inst_id: Monomorphize.CallableInstId,
) Allocator.Error!MIR.LambdaId {
    const callable_inst_key = @intFromEnum(callable_inst_id);
    if (self.lowered_callable_insts.get(callable_inst_key)) |cached| return cached;
    if (self.reserved_callable_insts.get(callable_inst_key)) |reserved| return reserved;

    if (self.in_progress_callable_insts.get(callable_inst_key)) |_| {
        std.debug.panic(
            "statement-only MIR callable-inst lambda lowering does not support recursive seed resolution yet for callable inst {d}",
            .{@intFromEnum(callable_inst_id)},
        );
    }

    const callable_inst = self.monomorphization.getCallableInst(callable_inst_id);
    const template = self.monomorphization.getCallableTemplate(callable_inst.template);
    const module_idx = template.module_idx;
    const module_env = self.all_module_envs[module_idx];
    const template_expr = module_env.store.getExpr(template.cir_expr);
    const fn_monotype = try self.importMonotypeFromStore(
        &self.monomorphization.monotype_store,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
        module_idx,
    );
    const reserved_lambda = if (template.kind == .closure)
        try self.reserveResolvedCallableInstClosureLambda(callable_inst_id, fn_monotype, template.source_region)
    else
        null;

    const switching_module = module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    const saved_callable_inst_context = self.current_callable_inst_context;
    const saved_root_source_expr_context = self.current_root_source_expr_context;
    const saved_type_var_seen = self.type_var_seen;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;

    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.current_callable_inst_context = callable_inst_id;
    self.current_root_source_expr_context = null;

    if (switching_module) {
        self.current_module_idx = module_idx;
        self.types_store = &module_env.types;
        self.mono_scratches.ident_store = module_env.getIdentStoreConst();
        self.mono_scratches.module_env = module_env;
        self.mono_scratches.module_idx = module_idx;
    }

    defer {
        self.type_var_seen.deinit();
        self.type_var_seen = saved_type_var_seen;
        self.nominal_cycle_breakers.deinit();
        self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
        self.current_callable_inst_context = saved_callable_inst_context;
        self.current_root_source_expr_context = saved_root_source_expr_context;
        if (switching_module) {
            self.current_module_idx = saved_module_idx;
            self.types_store = saved_types_store;
            self.mono_scratches.ident_store = saved_ident_store;
            self.mono_scratches.module_env = saved_module_env;
            self.mono_scratches.module_idx = saved_mono_module_idx;
        }
    }

    try self.seedTypeScopeBindingsInStore(
        self.current_module_idx,
        self.types_store,
        &self.type_var_seen,
    );

    if (!callable_inst.subst.isNone()) {
        const subst = self.monomorphization.getTypeSubst(callable_inst.subst);
        for (self.monomorphization.getTypeSubstEntries(subst.entries)) |entry| {
            if (builtin.mode == .Debug and entry.key.module_idx != module_idx) {
                std.debug.panic(
                    "Lower: callable inst subst entry from module {d} imported into module {d}",
                    .{ entry.key.module_idx, module_idx },
                );
            }
            const imported_mono = try self.importMonotypeFromStore(
                &self.monomorphization.monotype_store,
                entry.monotype.idx,
                entry.monotype.module_idx,
                module_idx,
            );
            try self.type_var_seen.put(entry.key.type_var, imported_mono);
        }
    }

    const lambda_id = switch (template_expr) {
        .e_lambda => |lambda| try self.lowerLambda(module_env, template.cir_expr, lambda, fn_monotype),
        .e_closure => |closure| try self.lowerReservedTrivialClosureLambda(
            module_env,
            template.cir_expr,
            closure,
            fn_monotype,
            callable_inst_id,
            reserved_lambda orelse unreachable,
        ),
        .e_hosted_lambda => |hosted| try self.lowerLambda(module_env, template.cir_expr, .{
            .args = hosted.args,
            .body = hosted.body,
        }, fn_monotype),
        else => std.debug.panic(
            "statement-only MIR callable-inst lambda lowering expected callable template, found {s}",
            .{@tagName(template_expr)},
        ),
    };

    _ = self.reserved_callable_insts.remove(callable_inst_key);
    try self.lowered_callable_insts.put(callable_inst_key, lambda_id);
    return lambda_id;
}

fn lowerCallInto(
    self: *Self,
    module_env: *const ModuleEnv,
    call: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (self.getCallLowLevelOp(module_env, call.func)) |ll_op| {
        const cir_args = module_env.store.sliceExpr(call.args);
        if (ll_op == .str_inspect) {
            if (cir_args.len != 1) {
                std.debug.panic(
                    "statement-only MIR str_inspect call expected 1 arg, got {d}",
                    .{cir_args.len},
                );
            }

            const source_mono = try self.resolveMonotype(cir_args[0]);
            const source_local = try self.freshSyntheticLocal(source_mono, false);
            const inspect_stmt = try self.lowerStrInspectLocalInto(source_local, source_mono, target, next);
            return self.lowerCirExprInto(cir_args[0], source_local, inspect_stmt);
        }

        const top = self.scratch_local_ids.top();
        defer self.scratch_local_ids.clearFrom(top);

        const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
            .target = target,
            .op = ll_op,
            .args = MIR.LocalSpan.empty(),
            .next = next,
        } });

        var lowered_next = assign_stmt;
        var i: usize = cir_args.len;
        while (i > 0) {
            i -= 1;
            const arg_idx = cir_args[i];
            const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(arg_idx), false);
            try self.scratch_local_ids.append(arg_local);
            lowered_next = try self.lowerCirExprInto(arg_idx, arg_local, lowered_next);
        }

        std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
        self.store.getCFStmtPtr(assign_stmt).assign_low_level.args =
            try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
        return lowered_next;
    }

    const cir_args = module_env.store.sliceExpr(call.args);
    const top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(top);

    const callee_local = try self.freshSyntheticLocal(try self.resolveMonotype(call.func), false);
    const call_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_call = .{
        .target = target,
        .callee = callee_local,
        .args = MIR.LocalSpan.empty(),
        .next = next,
    } });

    var lowered_next = call_stmt;
    var i: usize = cir_args.len;
    while (i > 0) {
        i -= 1;
        const arg_idx = cir_args[i];
        const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(arg_idx), false);
        try self.scratch_local_ids.append(arg_local);
        lowered_next = try self.lowerCirExprInto(arg_idx, arg_local, lowered_next);
    }

    lowered_next = try self.lowerCirExprInto(call.func, callee_local, lowered_next);

    std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
    self.store.getCFStmtPtr(call_stmt).assign_call.args =
        try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.items.items[top..]);
    return lowered_next;
}

fn binopLowLevel(self: *Self, op: CIR.Expr.Binop.Op, lhs_mono: Monotype.Idx) ?CIR.Expr.LowLevel {
    const mono = self.store.monotype_store.getMonotype(lhs_mono);
    return switch (op) {
        .add => .num_plus,
        .sub => .num_minus,
        .mul => .num_times,
        .div => .num_div_by,
        .div_trunc => .num_div_trunc_by,
        .rem => .num_rem_by,
        .lt => .num_is_lt,
        .le => .num_is_lte,
        .gt => .num_is_gt,
        .ge => .num_is_gte,
        .eq, .ne => switch (mono) {
            .prim => |prim| switch (prim) {
                .str => .str_is_eq,
                else => .num_is_eq,
            },
            .record,
            .tuple,
            .tag_union,
            .list,
            => .num_is_eq,
            else => null,
        },
        .@"and", .@"or" => null,
    };
}

fn lowerPrimitiveBinopInto(
    self: *Self,
    lhs_expr: CIR.Expr.Idx,
    rhs_expr: CIR.Expr.Idx,
    target: MIR.LocalId,
    op: CIR.Expr.LowLevel,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(top);

    const lhs_local = try self.freshSyntheticLocal(try self.resolveMonotype(lhs_expr), false);
    try self.scratch_local_ids.append(lhs_local);
    const rhs_local = try self.freshSyntheticLocal(try self.resolveMonotype(rhs_expr), false);
    try self.scratch_local_ids.append(rhs_local);

    const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = target,
        .op = op,
        .args = MIR.LocalSpan.empty(),
        .next = next,
    } });

    var lowered_next = assign_stmt;
    lowered_next = try self.lowerCirExprInto(rhs_expr, rhs_local, lowered_next);
    lowered_next = try self.lowerCirExprInto(lhs_expr, lhs_local, lowered_next);

    self.store.getCFStmtPtr(assign_stmt).assign_low_level.args =
        try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
    return lowered_next;
}

fn lowerUnaryMinusInto(
    self: *Self,
    um: CIR.Expr.UnaryMinus,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const operand_mono = try self.resolveMonotype(um.expr);
    switch (self.store.monotype_store.getMonotype(operand_mono)) {
        .prim => |prim| switch (prim) {
            .u8, .u16, .u32, .u64, .u128 => std.debug.panic(
                "statement-only MIR unary minus is not defined for unsigned primitive {s}",
                .{@tagName(prim)},
            ),
            .i8, .i16, .i32, .i64, .i128, .f32, .f64, .dec => {},
            else => std.debug.panic(
                "statement-only MIR unary minus is not implemented yet for primitive {s}",
                .{@tagName(prim)},
            ),
        },
        else => std.debug.panic(
            "statement-only MIR unary minus is not implemented yet for monotype kind {s}",
            .{@tagName(self.store.monotype_store.getMonotype(operand_mono))},
        ),
    }

    const source_local = try self.freshSyntheticLocal(operand_mono, false);
    const negate_stmt = try self.lowerUnaryLowLevelInto(target, .num_negate, source_local, next);
    return self.lowerCirExprInto(um.expr, source_local, negate_stmt);
}

fn lowerBinopInto(
    self: *Self,
    binop: CIR.Expr.Binop,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const result_mono = self.store.getLocal(target).monotype;

    switch (binop.op) {
        .@"and" => {
            const cond_mono = try self.resolveMonotype(binop.lhs);
            const cond_local = try self.freshSyntheticLocal(cond_mono, false);
            const else_body = try self.lowerBoolLiteralInto(target, result_mono, false, next);
            const then_body = try self.lowerCirExprInto(binop.rhs, target, next);
            const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
                "statement-only MIR and expected Bool condition monotype for expr {d}",
                .{@intFromEnum(binop.lhs)},
            );
            const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, then_body, else_body);
            return self.lowerCirExprInto(binop.lhs, cond_local, switch_stmt);
        },
        .@"or" => {
            const cond_mono = try self.resolveMonotype(binop.lhs);
            const cond_local = try self.freshSyntheticLocal(cond_mono, false);
            const then_body = try self.lowerBoolLiteralInto(target, result_mono, true, next);
            const else_body = try self.lowerCirExprInto(binop.rhs, target, next);
            const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
                "statement-only MIR or expected Bool condition monotype for expr {d}",
                .{@intFromEnum(binop.lhs)},
            );
            const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, then_body, else_body);
            return self.lowerCirExprInto(binop.lhs, cond_local, switch_stmt);
        },
        .eq, .ne => {
            const lhs_mono = try self.resolveMonotype(binop.lhs);
            if (self.store.monotype_store.getMonotype(lhs_mono) == .unit) {
                return self.lowerBoolLiteralInto(target, result_mono, binop.op == .eq, next);
            }

            const eq_op = binopLowLevel(self, .eq, lhs_mono) orelse std.debug.panic(
                "statement-only MIR binop {s} is not implemented yet for monotype kind {s}",
                .{ @tagName(binop.op), @tagName(self.store.monotype_store.getMonotype(lhs_mono)) },
            );

            if (binop.op == .eq) {
                return self.lowerPrimitiveBinopInto(binop.lhs, binop.rhs, target, eq_op, next);
            }

            const eq_local = try self.freshSyntheticLocal(result_mono, false);
            const not_stmt = try self.lowerUnaryLowLevelInto(target, .bool_not, eq_local, next);
            return self.lowerPrimitiveBinopInto(binop.lhs, binop.rhs, eq_local, eq_op, not_stmt);
        },
        else => {
            const lhs_mono = try self.resolveMonotype(binop.lhs);
            const ll = binopLowLevel(self, binop.op, lhs_mono) orelse std.debug.panic(
                "statement-only MIR binop {s} is not implemented yet for monotype kind {s}",
                .{ @tagName(binop.op), @tagName(self.store.monotype_store.getMonotype(lhs_mono)) },
            );
            return self.lowerPrimitiveBinopInto(binop.lhs, binop.rhs, target, ll, next);
        },
    }
}

fn lowerDotAccessInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    da: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const module_env = self.all_module_envs[self.current_module_idx];

    if (da.args != null) {
        const receiver_mono = try self.resolveMonotype(da.receiver);
        const receiver_local = try self.freshSyntheticLocal(receiver_mono, false);
        const dot_args = module_env.store.sliceExpr(da.args.?);

        if (self.lookupMonomorphizedDispatchCallableInst(expr_idx)) |callable_inst_id| {
            if (try self.callableInstProducesClosureValue(callable_inst_id)) {
                std.debug.panic(
                    "statement-only MIR TODO: dot-call field '{s}' requires runtime closure captures",
                    .{module_env.getIdent(da.field_name)},
                );
            }

            const callable_inst = self.monomorphization.getCallableInst(callable_inst_id);
            const callee_monotype = try self.importMonotypeFromStore(
                &self.monomorphization.monotype_store,
                callable_inst.fn_monotype,
                callable_inst.fn_monotype_module_idx,
                self.current_module_idx,
            );
            const callee_local = try self.freshSyntheticLocal(callee_monotype, false);
            const lambda_id = try self.lowerResolvedCallableInstLambda(callable_inst_id);

            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            try self.scratch_local_ids.append(receiver_local);
            for (dot_args) |arg_idx| {
                try self.scratch_local_ids.append(try self.freshSyntheticLocal(try self.resolveMonotype(arg_idx), false));
            }

            const call_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_call = .{
                .target = target,
                .callee = callee_local,
                .args = MIR.LocalSpan.empty(),
                .next = next,
            } });

            var lowered_next = call_stmt;
            var i: usize = dot_args.len;
            while (i > 0) {
                i -= 1;
                const arg_local = self.scratch_local_ids.items.items[top + 1 + i];
                lowered_next = try self.lowerCirExprInto(dot_args[i], arg_local, lowered_next);
            }

            lowered_next = try self.lowerCirExprInto(da.receiver, receiver_local, lowered_next);
            lowered_next = try self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
                .target = callee_local,
                .lambda = lambda_id,
                .next = lowered_next,
            } });

            self.store.getCFStmtPtr(call_stmt).assign_call.args =
                try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            return lowered_next;
        }

        if (dot_args.len == 0 and std.mem.eql(u8, module_env.getIdent(da.field_name), "to_str")) {
            return switch (self.store.monotype_store.getMonotype(receiver_mono)) {
                .prim => |prim| {
                    const rendered = toStrLowLevelForPrim(prim) orelse {
                        const alias_stmt = try self.lowerLocalAliasInto(target, receiver_local, next);
                        return self.lowerCirExprInto(da.receiver, receiver_local, alias_stmt);
                    };
                    const render_stmt = try self.lowerUnaryLowLevelInto(target, rendered, receiver_local, next);
                    return self.lowerCirExprInto(da.receiver, receiver_local, render_stmt);
                },
                else => std.debug.panic(
                    "statement-only MIR dot-call field '{s}' has no resolved dispatch target for monotype kind {s}",
                    .{ module_env.getIdent(da.field_name), @tagName(self.store.monotype_store.getMonotype(receiver_mono)) },
                ),
            };
        }

        std.debug.panic(
            "statement-only MIR TODO: dot-call field '{s}' has no resolved dispatch lowering",
            .{module_env.getIdent(da.field_name)},
        );
    }

    const receiver_mono = try self.resolveMonotype(da.receiver);
    const receiver_record = switch (self.store.monotype_store.getMonotype(receiver_mono)) {
        .record => |record| record,
        else => typeBindingInvariant(
            "lowerDotAccessInto: field access receiver is not a record monotype (field='{s}', monotype='{s}')",
            .{ module_env.getIdent(da.field_name), @tagName(self.store.monotype_store.getMonotype(receiver_mono)) },
        ),
    };

    const field_idx = self.recordFieldIndexByName(
        da.field_name,
        self.store.monotype_store.getFields(receiver_record.fields),
    );
    const receiver_local = try self.freshSyntheticLocal(receiver_mono, false);
    const field_stmt = try self.lowerRefInto(target, .{ .field = .{
        .source = receiver_local,
        .field_idx = field_idx,
    } }, next);

    return self.lowerCirExprInto(da.receiver, receiver_local, field_stmt);
}

// CIR `if` is only front-end sugar here. Strongest-form MIR lowers it
// immediately into explicit Bool `switch_stmt` control flow plus `join`/`jump`
// value merges.
fn lowerBoolBranchChainInto(
    self: *Self,
    branches: []const CIR.Expr.IfBranch.Idx,
    final_else: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const module_env = self.all_module_envs[self.current_module_idx];

    if (branches.len == 0) {
        return self.lowerCirExprInto(final_else, target, next);
    }

    const branch = module_env.store.getIfBranch(branches[0]);
    const cond_mono = try self.resolveMonotype(branch.cond);
    const result_mono = self.store.getLocal(target).monotype;
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const join_id = self.freshJoinPointId();
    const join_params = try self.store.addLocalSpan(self.allocator, &.{target});
    const outer_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = join_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{target}),
    } });

    const else_body = try self.lowerBoolBranchChainInto(branches[1..], final_else, target, outer_jump);

    const then_value = try self.freshSyntheticLocal(result_mono, false);
    const then_body = try self.lowerCirExprInto(
        branch.body,
        then_value,
        try self.store.addCFStmt(self.allocator, .{ .jump = .{
            .id = join_id,
            .args = try self.store.addLocalSpan(self.allocator, &.{then_value}),
        } }),
    );
    const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
        "statement-only MIR if expected Bool condition monotype for expr {d}",
        .{@intFromEnum(branch.cond)},
    );

    const switch_stmt = try self.lowerSwitchOnDiscriminant(
        cond_local,
        cond_mono,
        tag_names.true_name,
        then_body,
        else_body,
    );

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = join_id,
        .params = join_params,
        .body = next,
        .remainder = try self.lowerCirExprInto(branch.cond, cond_local, switch_stmt),
    } });
}

fn setPatternLocalsReassignable(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    reassignable: bool,
) void {
    switch (module_env.store.getPattern(pattern_idx)) {
        .assign, .as => {
            const local = self.patternToLocal(pattern_idx) catch unreachable;
            self.store.getLocalPtr(local).reassignable = reassignable;
        },
        .underscore,
        .num_literal,
        .str_literal,
        .dec_literal,
        .small_dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
        .nominal => |nom| self.setPatternLocalsReassignable(module_env, nom.backing_pattern, reassignable),
        .nominal_external => |nom| self.setPatternLocalsReassignable(module_env, nom.backing_pattern, reassignable),
        .applied_tag => |tag| {
            for (module_env.store.slicePatterns(tag.args)) |arg_pattern| {
                self.setPatternLocalsReassignable(module_env, arg_pattern, reassignable);
            }
        },
        .record_destructure => |record_pat| {
            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                self.setPatternLocalsReassignable(module_env, destruct.kind.toPatternIdx(), reassignable);
            }
        },
        .tuple => |tuple_pat| {
            for (module_env.store.slicePatterns(tuple_pat.patterns)) |elem_pattern| {
                self.setPatternLocalsReassignable(module_env, elem_pattern, reassignable);
            }
        },
        .list => |list_pat| {
            for (module_env.store.slicePatterns(list_pat.patterns)) |elem_pattern| {
                self.setPatternLocalsReassignable(module_env, elem_pattern, reassignable);
            }
            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern| {
                    self.setPatternLocalsReassignable(module_env, rest_pattern, reassignable);
                }
            }
        },
    }
}

fn lowerLiteralPatternMatchLocalInto(
    self: *Self,
    source_local: MIR.LocalId,
    literal_mono: Monotype.Idx,
    literal: MIR.LiteralValue,
    eq_op: CIR.Expr.LowLevel,
    on_match: MIR.CFStmtId,
    on_fail: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
    const tag_names = boolTagNamesForMonotype(self, bool_mono) orelse std.debug.panic(
        "statement-only MIR literal-pattern lowering expected Bool monotype",
        .{},
    );

    const literal_local = try self.freshSyntheticLocal(literal_mono, false);
    const eq_local = try self.freshSyntheticLocal(bool_mono, false);
    var body = try self.lowerSwitchOnDiscriminant(eq_local, bool_mono, tag_names.true_name, on_match, on_fail);
    body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = eq_local,
        .op = eq_op,
        .args = try self.store.addLocalSpan(self.allocator, &.{ source_local, literal_local }),
        .next = body,
    } });
    return self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = literal_local,
        .literal = literal,
        .next = body,
    } });
}

fn lowerPatternBindingInto(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    mark_reassignable: bool,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const source_mono = try self.resolveMonotype(expr_idx);
    const source_local = try self.freshSyntheticLocal(source_mono, false);
    try self.bindPatternMonotypes(module_env, pattern_idx, source_mono);
    if (mark_reassignable) {
        self.setPatternLocalsReassignable(module_env, pattern_idx, true);
    }
    const bound = try self.lowerPatternBindingLocalInto(module_env, pattern_idx, source_local, next);
    return self.lowerCirExprInto(expr_idx, source_local, bound);
}

fn lowerPatternMatchLocalInto(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_local: MIR.LocalId,
    on_match: MIR.CFStmtId,
    on_fail: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const source_mono = self.store.getLocal(source_local).monotype;
    try self.bindPatternMonotypes(module_env, pattern_idx, source_mono);
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign => {
            const local = try self.patternToLocal(pattern_idx);
            if (local == source_local) return on_match;
            return self.lowerLocalAliasInto(local, source_local, on_match);
        },
        .underscore => on_match,
        .as => |as_pattern| blk: {
            const local = try self.patternToLocal(pattern_idx);
            const alias_stmt = if (local == source_local)
                on_match
            else
                try self.lowerLocalAliasInto(local, source_local, on_match);
            break :blk try self.lowerPatternMatchLocalInto(
                module_env,
                as_pattern.pattern,
                source_local,
                alias_stmt,
                on_fail,
            );
        },
        .nominal => |nom| self.lowerPatternMatchLocalInto(module_env, nom.backing_pattern, source_local, on_match, on_fail),
        .nominal_external => |nom| self.lowerPatternMatchLocalInto(module_env, nom.backing_pattern, source_local, on_match, on_fail),
        .num_literal => |nl| self.lowerLiteralPatternMatchLocalInto(
            source_local,
            source_mono,
            .{ .int = nl.value },
            .num_is_eq,
            on_match,
            on_fail,
        ),
        .str_literal => |sl| self.lowerLiteralPatternMatchLocalInto(
            source_local,
            source_mono,
            .{ .str = try self.copyStringToMir(module_env, sl.literal) },
            .str_is_eq,
            on_match,
            on_fail,
        ),
        .dec_literal => |dl| self.lowerLiteralPatternMatchLocalInto(
            source_local,
            source_mono,
            .{ .dec = dl.value },
            .num_is_eq,
            on_match,
            on_fail,
        ),
        .small_dec_literal => |sdl| self.lowerLiteralPatternMatchLocalInto(
            source_local,
            source_mono,
            .{ .dec = sdl.value.toRocDec() },
            .num_is_eq,
            on_match,
            on_fail,
        ),
        .frac_f32_literal => |fl| self.lowerLiteralPatternMatchLocalInto(
            source_local,
            source_mono,
            .{ .frac_f32 = fl.value },
            .num_is_eq,
            on_match,
            on_fail,
        ),
        .frac_f64_literal => |fl| self.lowerLiteralPatternMatchLocalInto(
            source_local,
            source_mono,
            .{ .frac_f64 = fl.value },
            .num_is_eq,
            on_match,
            on_fail,
        ),
        .applied_tag => |tag| blk: {
            const payload_monos = self.tagPayloadMonotypesByName(source_mono, tag.name);
            const payload_patterns = module_env.store.slicePatterns(tag.args);
            if (payload_patterns.len != payload_monos.len) {
                std.debug.panic(
                    "statement-only MIR tag pattern payload arity mismatch during direct lowering",
                    .{},
                );
            }

            var body = on_match;
            var i = payload_patterns.len;
            while (i > 0) {
                i -= 1;
                const payload_local = try self.freshSyntheticLocal(payload_monos[i], false);
                body = try self.lowerPatternMatchLocalInto(
                    module_env,
                    payload_patterns[i],
                    payload_local,
                    body,
                    on_fail,
                );
                body = try self.lowerRefInto(payload_local, .{ .tag_payload = .{
                    .source = source_local,
                    .payload_idx = @intCast(i),
                } }, body);
            }

            break :blk try self.lowerSwitchOnDiscriminant(
                source_local,
                source_mono,
                self.resolveTagNameForMonotype(source_mono, tag.name),
                body,
                on_fail,
            );
        },
        .record_destructure => |record_pat| blk: {
            const mono_fields = switch (self.store.monotype_store.getMonotype(source_mono)) {
                .record => |record_mono| self.store.monotype_store.getFields(record_mono.fields),
                .unit => &.{},
                else => std.debug.panic(
                    "statement-only MIR record pattern expected record/unit source monotype",
                    .{},
                ),
            };

            var body = on_match;
            const cir_destructs = module_env.store.sliceRecordDestructs(record_pat.destructs);
            var i = cir_destructs.len;
            while (i > 0) {
                i -= 1;
                const destruct = module_env.store.getRecordDestruct(cir_destructs[i]);
                const field_idx = self.recordFieldIndexByName(destruct.label, mono_fields);
                const field_local = try self.freshSyntheticLocal(mono_fields[field_idx].type_idx, false);
                body = try self.lowerPatternMatchLocalInto(
                    module_env,
                    destruct.kind.toPatternIdx(),
                    field_local,
                    body,
                    on_fail,
                );
                body = try self.lowerRefInto(field_local, .{ .field = .{
                    .source = source_local,
                    .field_idx = @intCast(field_idx),
                } }, body);
            }
            break :blk body;
        },
        .tuple => |tuple_pat| blk: {
            const elem_monos = switch (self.store.monotype_store.getMonotype(source_mono)) {
                .tuple => |tuple_mono| self.store.monotype_store.getIdxSpan(tuple_mono.elems),
                else => std.debug.panic(
                    "statement-only MIR tuple pattern expected tuple source monotype",
                    .{},
                ),
            };
            const elem_patterns = module_env.store.slicePatterns(tuple_pat.patterns);
            if (elem_patterns.len != elem_monos.len) {
                std.debug.panic(
                    "statement-only MIR tuple pattern arity mismatch during direct lowering",
                    .{},
                );
            }

            var body = on_match;
            var i = elem_patterns.len;
            while (i > 0) {
                i -= 1;
                const elem_local = try self.freshSyntheticLocal(elem_monos[i], false);
                body = try self.lowerPatternMatchLocalInto(
                    module_env,
                    elem_patterns[i],
                    elem_local,
                    body,
                    on_fail,
                );
                body = try self.lowerRefInto(elem_local, .{ .field = .{
                    .source = source_local,
                    .field_idx = @intCast(i),
                } }, body);
            }
            break :blk body;
        },
        .list => std.debug.panic(
            "statement-only MIR TODO: list pattern lowering into explicit MIR statements is not implemented yet",
            .{},
        ),
        .runtime_error => std.debug.panic(
            "statement-only MIR TODO: runtime-error patterns are not implemented in pattern-free MIR lowering",
            .{},
        ),
    };
}

fn lowerPatternBindingLocalInto(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_local: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });
    return self.lowerPatternMatchLocalInto(
        module_env,
        pattern_idx,
        source_local,
        next,
        failure,
    );
}

fn lowerLoopBreakJump(self: *Self) Allocator.Error!MIR.CFStmtId {
    const loop_ctx = self.active_loops.getLastOrNull() orelse std.debug.panic(
        "statement-only MIR break appeared outside any active loop",
        .{},
    );
    return self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_ctx.exit_id,
        .args = loop_ctx.carried_locals,
    } });
}

fn lowerWhileStmtInto(
    self: *Self,
    while_stmt: std.meta.TagPayload(CIR.Statement, .s_while),
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const carried_locals = try self.currentBodyReassignableLocals();
    defer self.allocator.free(carried_locals);

    const carried_span = try self.store.addLocalSpan(self.allocator, carried_locals);
    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();

    try self.active_loops.append(self.allocator, .{
        .exit_id = loop_exit_id,
        .carried_locals = carried_span,
    });
    defer _ = self.active_loops.pop();

    const cond_mono = try self.resolveMonotype(while_stmt.cond);
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);

    const continue_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = carried_span,
    } });
    const body_value = try self.freshSyntheticLocal(try self.resolveMonotype(while_stmt.body), false);
    const true_body = try self.lowerCirExprInto(while_stmt.body, body_value, continue_jump);
    const false_body = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_exit_id,
        .args = carried_span,
    } });

    const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
        "statement-only MIR while expected Bool condition monotype for expr {d}",
        .{@intFromEnum(while_stmt.cond)},
    );
    const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, true_body, false_body);
    const head_body = try self.lowerCirExprInto(while_stmt.cond, cond_local, switch_stmt);

    const head_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_head_id,
        .params = carried_span,
        .body = head_body,
        .remainder = try self.store.addCFStmt(self.allocator, .{ .jump = .{
            .id = loop_head_id,
            .args = carried_span,
        } }),
    } });

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_exit_id,
        .params = carried_span,
        .body = next,
        .remainder = head_join,
    } });
}

fn lowerBoolExprInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    on_true: MIR.CFStmtId,
    on_false: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const cond_mono = try self.resolveMonotype(expr_idx);
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
        "statement-only MIR Bool control-flow expected Bool condition monotype for expr {d}",
        .{@intFromEnum(expr_idx)},
    );
    const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, on_true, on_false);
    return self.lowerCirExprInto(expr_idx, cond_local, switch_stmt);
}

fn lowerForStmtInto(
    self: *Self,
    module_env: *const ModuleEnv,
    for_stmt: std.meta.TagPayload(CIR.Statement, .s_for),
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const carried_locals = try self.currentBodyReassignableLocals();
    defer self.allocator.free(carried_locals);

    const list_mono = try self.resolveMonotype(for_stmt.expr);
    const item_mono = switch (self.store.monotype_store.getMonotype(list_mono)) {
        .list => |list| list.elem,
        else => std.debug.panic(
            "statement-only MIR for-loop expected list source monotype",
            .{},
        ),
    };
    try self.bindPatternMonotypes(module_env, for_stmt.patt, item_mono);
    const u64_mono = self.store.monotype_store.primIdx(.u64);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );

    const list_local = try self.freshSyntheticLocal(list_mono, false);
    const len_local = try self.freshSyntheticLocal(u64_mono, false);
    const index_local = try self.freshSyntheticLocal(u64_mono, false);

    const head_args = try self.allocator.alloc(MIR.LocalId, carried_locals.len + 3);
    defer self.allocator.free(head_args);
    @memcpy(head_args[0..carried_locals.len], carried_locals);
    head_args[carried_locals.len] = list_local;
    head_args[carried_locals.len + 1] = len_local;
    head_args[carried_locals.len + 2] = index_local;

    const carried_span = try self.store.addLocalSpan(self.allocator, carried_locals);
    const head_params = try self.store.addLocalSpan(self.allocator, head_args);

    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();

    try self.active_loops.append(self.allocator, .{
        .exit_id = loop_exit_id,
        .carried_locals = carried_span,
    });
    defer _ = self.active_loops.pop();

    const false_body = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_exit_id,
        .args = carried_span,
    } });

    const one_local = try self.freshSyntheticLocal(u64_mono, false);
    const next_index_local = try self.freshSyntheticLocal(u64_mono, false);
    const item_local = try self.freshSyntheticLocal(item_mono, false);
    const body_value = try self.freshSyntheticLocal(try self.resolveMonotype(for_stmt.body), false);
    const loop_back_args = try self.allocator.alloc(MIR.LocalId, carried_locals.len + 3);
    defer self.allocator.free(loop_back_args);
    @memcpy(loop_back_args[0..carried_locals.len], carried_locals);
    loop_back_args[carried_locals.len] = list_local;
    loop_back_args[carried_locals.len + 1] = len_local;
    loop_back_args[carried_locals.len + 2] = next_index_local;

    const loop_back = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.store.addLocalSpan(self.allocator, loop_back_args),
    } });
    const increment_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = next_index_local,
        .op = .num_plus,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, one_local }),
        .next = loop_back,
    } });
    try self.predeclarePatternLocals(module_env, for_stmt.patt);
    const body_stmt = try self.lowerCirExprInto(for_stmt.body, body_value, increment_stmt);
    const bind_item = try self.lowerPatternBindingLocalInto(module_env, for_stmt.patt, item_local, body_stmt);
    const get_item = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = item_local,
        .op = .list_get_unsafe,
        .args = try self.store.addLocalSpan(self.allocator, &.{ list_local, index_local }),
        .next = bind_item,
    } });
    const true_body = try self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = one_local,
        .literal = .{ .int = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 } },
        .next = get_item,
    } });

    const cond_local = try self.freshSyntheticLocal(bool_mono, false);
    const tag_names = boolTagNamesForMonotype(self, bool_mono) orelse std.debug.panic(
        "statement-only MIR for-loop expected Bool condition monotype",
        .{},
    );
    const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, bool_mono, tag_names.true_name, true_body, false_body);
    const cond_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = cond_local,
        .op = .num_is_lt,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, len_local }),
        .next = switch_stmt,
    } });

    const initial_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = head_params,
    } });
    const init_index = try self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = index_local,
        .literal = .{ .int = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 } },
        .next = initial_jump,
    } });
    const init_len = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = len_local,
        .op = .list_len,
        .args = try self.store.addLocalSpan(self.allocator, &.{list_local}),
        .next = init_index,
    } });
    const init_list = try self.lowerCirExprInto(for_stmt.expr, list_local, init_len);

    const head_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_head_id,
        .params = head_params,
        .body = cond_stmt,
        .remainder = init_list,
    } });

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_exit_id,
        .params = carried_span,
        .body = next,
        .remainder = head_join,
    } });
}

fn lowerMatchInto(
    self: *Self,
    module_env: *const ModuleEnv,
    match_expr: CIR.Expr.Match,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const cond_mono = try self.resolveMonotype(match_expr.cond);
    const result_mono = self.store.getLocal(target).monotype;
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const join_id = self.freshJoinPointId();
    const join_params = try self.store.addLocalSpan(self.allocator, &.{target});

    const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
    for (branch_indices) |branch_idx| {
        const cir_branch = module_env.store.getMatchBranch(branch_idx);
        const branch_pattern_indices = module_env.store.sliceMatchBranchPatterns(cir_branch.patterns);
        const representative_pattern_idx = if (branch_pattern_indices.len > 0)
            module_env.store.getMatchBranchPattern(branch_pattern_indices[0]).pattern
        else
            null;

        for (branch_pattern_indices, 0..) |branch_pattern_idx, pattern_i| {
            const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
            if (pattern_i != 0) {
                if (representative_pattern_idx) |representative| {
                    try self.alignAlternativePatternSymbols(module_env, representative, branch_pattern.pattern);
                }
            }
            try self.bindPatternMonotypes(module_env, branch_pattern.pattern, cond_mono);
        }
    }

    const match_failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });
    const branch_degenerate_failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });

    var entry = match_failure;
    var branch_i = branch_indices.len;
    while (branch_i > 0) {
        branch_i -= 1;
        const cir_branch = module_env.store.getMatchBranch(branch_indices[branch_i]);
        const branch_fallthrough = entry;
        const branch_value_local = try self.freshSyntheticLocal(result_mono, false);
        const branch_value = try self.lowerCirExprInto(
            cir_branch.value,
            branch_value_local,
            try self.store.addCFStmt(self.allocator, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{branch_value_local}),
            } }),
        );
        const guarded_body = if (cir_branch.guard) |guard_idx|
            try self.lowerBoolExprInto(guard_idx, branch_value, branch_fallthrough)
        else
            branch_value;

        const branch_pattern_indices = module_env.store.sliceMatchBranchPatterns(cir_branch.patterns);
        var branch_entry = branch_fallthrough;
        var pattern_i = branch_pattern_indices.len;
        while (pattern_i > 0) {
            pattern_i -= 1;
            const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_indices[pattern_i]);
            branch_entry = try self.lowerPatternMatchLocalInto(
                module_env,
                branch_pattern.pattern,
                cond_local,
                if (branch_pattern.degenerate) branch_degenerate_failure else guarded_body,
                branch_entry,
            );
        }

        if (branch_pattern_indices.len == 0) {
            branch_entry = guarded_body;
        }
        entry = branch_entry;
    }

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = join_id,
        .params = join_params,
        .body = next,
        .remainder = try self.lowerCirExprInto(match_expr.cond, cond_local, entry),
    } });
}

fn lowerBlockStmtInto(
    self: *Self,
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const stmt = module_env.store.getStatement(stmt_idx);

    return switch (stmt) {
        .s_decl => |decl| self.lowerPatternBindingInto(
            module_env,
            decl.pattern,
            decl.expr,
            false,
            next,
        ),
        .s_var => |var_decl| self.lowerPatternBindingInto(
            module_env,
            var_decl.pattern_idx,
            var_decl.expr,
            true,
            next,
        ),
        .s_reassign => |reassign| self.lowerPatternBindingInto(
            module_env,
            reassign.pattern_idx,
            reassign.expr,
            false,
            next,
        ),
        .s_expr => |expr_stmt| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(expr_stmt.expr), false);
            break :blk try self.lowerCirExprInto(expr_stmt.expr, value_local, next);
        },
        .s_dbg => |dbg_stmt| blk: {
            const value_mono = try self.resolveMonotype(dbg_stmt.expr);
            const value_local = try self.freshSyntheticLocal(value_mono, false);
            const message_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.str), false);
            const debug_stmt = try self.store.addCFStmt(self.allocator, .{ .debug = .{
                .value = message_local,
                .next = next,
            } });
            const inspect_stmt = try self.lowerStrInspectLocalInto(value_local, value_mono, message_local, debug_stmt);
            break :blk try self.lowerCirExprInto(dbg_stmt.expr, value_local, inspect_stmt);
        },
        .s_expect => |expect_stmt| blk: {
            const cond_local = try self.freshSyntheticLocal(try self.resolveMonotype(expect_stmt.body), false);
            const mir_expect = try self.store.addCFStmt(self.allocator, .{ .expect = .{
                .condition = cond_local,
                .next = next,
            } });
            break :blk try self.lowerCirExprInto(expect_stmt.body, cond_local, mir_expect);
        },
        .s_crash => |crash_stmt| blk: {
            const mir_str = try self.copyStringToMir(module_env, crash_stmt.msg);
            break :blk self.store.addCFStmt(self.allocator, .{ .crash = mir_str });
        },
        .s_return => |return_stmt| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(return_stmt.expr), false);
            const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = value_local } });
            break :blk try self.lowerCirExprInto(return_stmt.expr, value_local, ret_stmt);
        },
        .s_runtime_error => |runtime_error_stmt| self.store.addCFStmt(self.allocator, .{ .runtime_error = .{
            .can_diagnostic = runtime_error_stmt.diagnostic,
        } }),
        .s_import,
        .s_alias_decl,
        .s_nominal_decl,
        .s_type_anno,
        .s_type_var_alias,
        => next,
        .s_for => |s_for| self.lowerForStmtInto(module_env, s_for, next),
        .s_while => |s_while| self.lowerWhileStmtInto(s_while, next),
        .s_break => self.lowerLoopBreakJump(),
    };
}

fn predeclareTrivialBlockStmtPatterns(
    self: *Self,
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
) Allocator.Error!void {
    switch (module_env.store.getStatement(stmt_idx)) {
        .s_decl => |decl| {
            try self.bindPatternMonotypes(module_env, decl.pattern, try self.resolveMonotype(decl.expr));
            try self.predeclarePatternLocals(module_env, decl.pattern);
        },
        .s_var => |var_decl| {
            try self.bindPatternMonotypes(module_env, var_decl.pattern_idx, try self.resolveMonotype(var_decl.expr));
            try self.predeclarePatternLocals(module_env, var_decl.pattern_idx);
        },
        else => {},
    }
}

fn predeclarePatternLocals(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    var bindings = std.ArrayList(PatternBinding).empty;
    defer bindings.deinit(self.allocator);
    try self.collectPatternBindings(module_env, pattern_idx, &bindings);
    for (bindings.items) |binding| {
        _ = try self.patternToLocal(binding.pattern_idx);
    }
}

fn lowerBlockInto(
    self: *Self,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const stmts = module_env.store.sliceStatements(block.stmts);

    for (stmts) |stmt_idx| {
        try self.predeclareTrivialBlockStmtPatterns(module_env, stmt_idx);
    }

    var entry = try self.lowerCirExprInto(block.final_expr, target, next);
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        entry = try self.lowerBlockStmtInto(module_env, stmts[i], entry);
    }
    return entry;
}

fn lowerCirExprInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const module_env = self.all_module_envs[self.current_module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const monotype = self.store.getLocal(target).monotype;

    return switch (expr) {
        .e_num => |num| self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .int = num.value },
            .next = next,
        } }),
        .e_frac_f32 => |frac| self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .frac_f32 = frac.value },
            .next = next,
        } }),
        .e_frac_f64 => |frac| self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .frac_f64 = frac.value },
            .next = next,
        } }),
        .e_dec => |dec| self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .dec = dec.value },
            .next = next,
        } }),
        .e_dec_small => |dec| self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .dec = dec.value.toRocDec() },
            .next = next,
        } }),
        .e_typed_int => |ti| self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
            .target = target,
            .literal = .{ .int = ti.value },
            .next = next,
        } }),
        .e_typed_frac => |tf| blk: {
            const mono = self.store.monotype_store.getMonotype(monotype);
            const roc_dec = builtins.dec.RocDec{ .num = tf.value.toI128() };
            const literal: MIR.LiteralValue = switch (mono) {
                .prim => |p| switch (p) {
                    .f64 => .{ .frac_f64 = roc_dec.toF64() },
                    .f32 => .{ .frac_f32 = @floatCast(roc_dec.toF64()) },
                    .dec => .{ .dec = roc_dec },
                    else => std.debug.panic(
                        "statement-only MIR lowerExpr: unsupported typed fractional literal monotype for expr {d}",
                        .{@intFromEnum(expr_idx)},
                    ),
                },
                else => std.debug.panic(
                    "statement-only MIR lowerExpr: non-primitive typed fractional literal for expr {d}",
                    .{@intFromEnum(expr_idx)},
                ),
            };
            break :blk self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
                .target = target,
                .literal = literal,
                .next = next,
            } });
        },
        .e_str_segment => |seg| blk: {
            const mir_str = try self.copyStringToMir(module_env, seg.literal);
            break :blk self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
                .target = target,
                .literal = .{ .str = mir_str },
                .next = next,
            } });
        },
        .e_str => |str_expr| blk: {
            const span = module_env.store.sliceExpr(str_expr.span);
            break :blk try self.lowerStringConcatInto(span, target, next);
        },
        .e_empty_list => self.store.addCFStmt(self.allocator, .{ .assign_list = .{
            .target = target,
            .elems = MIR.LocalSpan.empty(),
            .next = next,
        } }),
        .e_list => |list| blk: {
            const cir_elems = module_env.store.sliceExpr(list.elems);
            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_list = .{
                .target = target,
                .elems = MIR.LocalSpan.empty(),
                .next = next,
            } });

            var lowered_next = assign_stmt;
            var i: usize = cir_elems.len;
            while (i > 0) {
                i -= 1;
                const elem_idx = cir_elems[i];
                const elem_local = try self.freshSyntheticLocal(try self.resolveMonotype(elem_idx), false);
                try self.scratch_local_ids.append(elem_local);
                lowered_next = try self.lowerCirExprInto(elem_idx, elem_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const elems = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            self.store.getCFStmtPtr(assign_stmt).assign_list.elems = elems;
            break :blk lowered_next;
        },
        .e_empty_record => self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
            .target = target,
            .fields = MIR.LocalSpan.empty(),
            .next = next,
        } }),
        .e_tuple => |tuple| blk: {
            const elems = module_env.store.sliceExpr(tuple.elems);
            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                .target = target,
                .fields = MIR.LocalSpan.empty(),
                .next = next,
            } });

            var lowered_next = assign_stmt;
            var i: usize = elems.len;
            while (i > 0) {
                i -= 1;
                const elem_idx = elems[i];
                const elem_local = try self.freshSyntheticLocal(try self.resolveMonotype(elem_idx), false);
                try self.scratch_local_ids.append(elem_local);
                lowered_next = try self.lowerCirExprInto(elem_idx, elem_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const fields = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            self.store.getCFStmtPtr(assign_stmt).assign_struct.fields = fields;
            break :blk lowered_next;
        },
        .e_record => |record| blk: {
            if (record.ext != null) {
                std.debug.panic(
                    "statement-only MIR lowerExpr: record updates are not implemented yet for expr {d}",
                    .{@intFromEnum(expr_idx)},
                );
            }

            const mono_record = switch (self.store.monotype_store.getMonotype(monotype)) {
                .record => |mono_record| mono_record,
                .unit => {
                    break :blk self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                        .target = target,
                        .fields = MIR.LocalSpan.empty(),
                        .next = next,
                    } });
                },
                else => std.debug.panic(
                    "statement-only MIR lowerExpr: record literal expected record monotype for expr {d}",
                    .{@intFromEnum(expr_idx)},
                ),
            };

            const mono_fields = self.store.monotype_store.getFields(mono_record.fields);
            const ordered = try self.allocator.alloc(?CIR.Expr.Idx, mono_fields.len);
            defer self.allocator.free(ordered);
            @memset(ordered, null);

            for (module_env.store.sliceRecordFields(record.fields)) |field_idx| {
                const field = module_env.store.getRecordField(field_idx);
                const canonical_idx = self.recordFieldIndexByName(field.name, mono_fields);
                ordered[canonical_idx] = field.value;
            }

            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                .target = target,
                .fields = MIR.LocalSpan.empty(),
                .next = next,
            } });

            var lowered_next = assign_stmt;
            var i: usize = ordered.len;
            while (i > 0) {
                i -= 1;
                const field_expr_idx = ordered[i] orelse std.debug.panic(
                    "statement-only MIR lowerExpr: record literal missing canonical field {d} for expr {d}",
                    .{ i, @intFromEnum(expr_idx) },
                );
                const field_local = try self.freshSyntheticLocal(mono_fields[i].type_idx, false);
                try self.scratch_local_ids.append(field_local);
                lowered_next = try self.lowerCirExprInto(field_expr_idx, field_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const fields = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            self.store.getCFStmtPtr(assign_stmt).assign_struct.fields = fields;
            break :blk lowered_next;
        },
        .e_zero_argument_tag => |tag| self.store.addCFStmt(self.allocator, .{ .assign_tag = .{
            .target = target,
            .name = self.resolveTagNameForMonotype(monotype, tag.name),
            .args = MIR.LocalSpan.empty(),
            .next = next,
        } }),
        .e_tag => |tag| blk: {
            const args = module_env.store.sliceExpr(tag.args);
            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_tag = .{
                .target = target,
                .name = self.resolveTagNameForMonotype(monotype, tag.name),
                .args = MIR.LocalSpan.empty(),
                .next = next,
            } });

            var lowered_next = assign_stmt;
            var i: usize = args.len;
            while (i > 0) {
                i -= 1;
                const arg_idx = args[i];
                const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(arg_idx), false);
                try self.scratch_local_ids.append(arg_local);
                lowered_next = try self.lowerCirExprInto(arg_idx, arg_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const arg_span = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            self.store.getCFStmtPtr(assign_stmt).assign_tag.args = arg_span;
            break :blk lowered_next;
        },
        .e_nominal => |nominal| blk: {
            const backing_local = try self.freshSyntheticLocal(try self.resolveMonotype(nominal.backing_expr), false);
            const nominal_stmt = try self.lowerRefInto(target, .{ .nominal = .{ .backing = backing_local } }, next);
            break :blk try self.lowerCirExprInto(nominal.backing_expr, backing_local, nominal_stmt);
        },
        .e_nominal_external => |nominal| blk: {
            const backing_local = try self.freshSyntheticLocal(try self.resolveMonotype(nominal.backing_expr), false);
            const nominal_stmt = try self.lowerRefInto(target, .{ .nominal = .{ .backing = backing_local } }, next);
            break :blk try self.lowerCirExprInto(nominal.backing_expr, backing_local, nominal_stmt);
        },
        .e_lookup_local => |lookup| self.lowerLookupLocalInto(expr_idx, lookup, target, next),
        .e_lookup_external => |lookup| self.lowerLookupExternalInto(expr_idx, module_env, lookup, target, next),
        .e_lookup_required => |lookup| self.lowerLookupRequiredInto(expr_idx, module_env, lookup, target, next),
        .e_lambda => |lambda| blk: {
            const callable_inst_id = (try self.lookupMonomorphizedValueExprCallableInstForMonotype(
                expr_idx,
                monotype,
                self.current_module_idx,
            )) orelse self.lookupMonomorphizedValueExprCallableInst(expr_idx);
            const lambda_id = if (callable_inst_id) |resolved_callable_inst_id|
                try self.lowerResolvedCallableInstLambda(resolved_callable_inst_id)
            else
                try self.lowerLambda(module_env, expr_idx, lambda, monotype);
            break :blk self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
                .target = target,
                .lambda = lambda_id,
                .next = next,
            } });
        },
        .e_closure => |closure| blk: {
            const closure_callable_inst_id = (try self.lookupMonomorphizedValueExprCallableInstForMonotype(
                expr_idx,
                monotype,
                self.current_module_idx,
            )) orelse self.lookupMonomorphizedValueExprCallableInst(expr_idx);
            const lambda_id = if (closure_callable_inst_id) |callable_inst_id|
                try self.lowerResolvedCallableInstLambda(callable_inst_id)
            else
                try self.lowerClosureLambda(module_env, expr_idx, closure, monotype);
            var lower_plan_storage: ?ClosureLowerPlan = null;
            defer {
                if (lower_plan_storage != null) {
                    lower_plan_storage.?.deinit(self.allocator);
                }
            }
            const recursive_members = if (closure_callable_inst_id) |callable_inst_id| blk2: {
                lower_plan_storage = try self.planClosureLowering(module_env, expr_idx, closure, callable_inst_id);
                break :blk2 lower_plan_storage.?.recursive_members.items;
            } else &.{};

            const capture_top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(capture_top);
            var top_level_capture_materializations = std.ArrayList(struct {
                local: MIR.LocalId,
                def_idx: CIR.Def.Idx,
                symbol: MIR.Symbol,
            }).empty;
            defer top_level_capture_materializations.deinit(self.allocator);
            for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
                const capture = module_env.store.getCapture(capture_idx);
                if (recursiveCallableInstForPattern(recursive_members, capture.pattern_idx) != null) continue;
                const capture_local = if (self.lookupExistingPatternLocal(capture.pattern_idx)) |existing|
                    existing
                else blk2: {
                    const def_idx = findDefByPattern(module_env, capture.pattern_idx) orelse std.debug.panic(
                        "statement-only MIR closure capture pattern {d} was not in scope and was not a top-level def",
                        .{@intFromEnum(capture.pattern_idx)},
                    );
                    const local = try self.patternToLocal(capture.pattern_idx);
                    try top_level_capture_materializations.append(self.allocator, .{
                        .local = local,
                        .def_idx = def_idx,
                        .symbol = try self.lookupSymbolForPattern(self.current_module_idx, capture.pattern_idx),
                    });
                    break :blk2 local;
                };
                try self.scratch_local_ids.append(capture_local);
            }

            const runtime_captures = self.scratch_local_ids.sliceFromStart(capture_top);
            if (runtime_captures.len == 0) {
                var current = try self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
                    .target = target,
                    .lambda = lambda_id,
                    .next = next,
                } });
                var i = top_level_capture_materializations.items.len;
                while (i > 0) {
                    i -= 1;
                    const materialization = top_level_capture_materializations.items[i];
                    current = try self.materializeTopLevelDefInto(
                        self.current_module_idx,
                        materialization.def_idx,
                        materialization.symbol,
                        materialization.local,
                        current,
                    );
                }
                break :blk current;
            }

            var current = try self.store.addCFStmt(self.allocator, .{ .assign_closure = .{
                .target = target,
                .lambda = lambda_id,
                .captures = try self.store.addLocalSpan(self.allocator, runtime_captures),
                .next = next,
            } });
            var i = top_level_capture_materializations.items.len;
            while (i > 0) {
                i -= 1;
                const materialization = top_level_capture_materializations.items[i];
                current = try self.materializeTopLevelDefInto(
                    self.current_module_idx,
                    materialization.def_idx,
                    materialization.symbol,
                    materialization.local,
                    current,
                );
            }
            break :blk current;
        },
        .e_block => |block| self.lowerBlockInto(block, target, next),
        .e_dot_access => |da| self.lowerDotAccessInto(expr_idx, da, target, next),
        .e_binop => |binop| self.lowerBinopInto(binop, target, next),
        .e_unary_minus => |um| self.lowerUnaryMinusInto(um, target, next),
        .e_call => |call| self.lowerCallInto(module_env, call, target, next),
        .e_match => |match_expr| self.lowerMatchInto(module_env, match_expr, target, next),
        .e_if => |if_expr| self.lowerBoolBranchChainInto(
            module_env.store.sliceIfBranches(if_expr.branches),
            if_expr.final_else,
            target,
            next,
        ),
        .e_dbg => |dbg_expr| blk: {
            const value_mono = try self.resolveMonotype(dbg_expr.expr);
            const value_local = try self.freshSyntheticLocal(value_mono, false);
            const message_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.str), false);
            const result_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                .target = target,
                .fields = MIR.LocalSpan.empty(),
                .next = next,
            } });
            const debug_stmt = try self.store.addCFStmt(self.allocator, .{ .debug = .{
                .value = message_local,
                .next = result_stmt,
            } });
            const inspect_stmt = try self.lowerStrInspectLocalInto(value_local, value_mono, message_local, debug_stmt);
            break :blk try self.lowerCirExprInto(dbg_expr.expr, value_local, inspect_stmt);
        },
        .e_expect => |expect_expr| blk: {
            const cond_local = try self.freshSyntheticLocal(try self.resolveMonotype(expect_expr.body), false);
            const result_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                .target = target,
                .fields = MIR.LocalSpan.empty(),
                .next = next,
            } });
            const mir_expect = try self.store.addCFStmt(self.allocator, .{ .expect = .{
                .condition = cond_local,
                .next = result_stmt,
            } });
            break :blk try self.lowerCirExprInto(expect_expr.body, cond_local, mir_expect);
        },
        .e_run_low_level => |run_ll| blk: {
            const cir_args = module_env.store.sliceExpr(run_ll.args);
            if (run_ll.op == .str_inspect) {
                if (cir_args.len != 1) {
                    std.debug.panic(
                        "statement-only MIR str_inspect expected 1 arg, got {d}",
                        .{cir_args.len},
                    );
                }

                const source_mono = try self.resolveMonotype(cir_args[0]);
                const source_local = try self.freshSyntheticLocal(source_mono, false);
                const inspect_stmt = try self.lowerStrInspectLocalInto(source_local, source_mono, target, next);
                break :blk try self.lowerCirExprInto(cir_args[0], source_local, inspect_stmt);
            }

            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
                .target = target,
                .op = run_ll.op,
                .args = MIR.LocalSpan.empty(),
                .next = next,
            } });

            var lowered_next = assign_stmt;
            var i: usize = cir_args.len;
            while (i > 0) {
                i -= 1;
                const arg_idx = cir_args[i];
                const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(arg_idx), false);
                try self.scratch_local_ids.append(arg_local);
                lowered_next = try self.lowerCirExprInto(arg_idx, arg_local, lowered_next);
            }

            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            self.store.getCFStmtPtr(assign_stmt).assign_low_level.args =
                try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            break :blk lowered_next;
        },
        .e_runtime_error => |err| self.store.addCFStmt(self.allocator, .{ .runtime_error = .{
            .can_diagnostic = err.diagnostic,
        } }),
        .e_return => |return_expr| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(return_expr.expr), false);
            const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = value_local } });
            break :blk try self.lowerCirExprInto(return_expr.expr, value_local, ret_stmt);
        },
        .e_crash => |crash| blk: {
            const mir_str = try self.copyStringToMir(module_env, crash.msg);
            break :blk self.store.addCFStmt(self.allocator, .{ .crash = mir_str });
        },
        else => std.debug.panic(
            "statement-only MIR lowerExpr is not implemented yet for CIR expr {d} kind={s}",
            .{ @intFromEnum(expr_idx), @tagName(expr) },
        ),
    };
}

fn alignAlternativePatternSymbols(
    self: *Self,
    module_env: *const ModuleEnv,
    representative_pattern_idx: CIR.Pattern.Idx,
    alternative_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    var representative_bindings = std.ArrayList(PatternBinding).empty;
    defer representative_bindings.deinit(self.allocator);

    var alternative_bindings = std.ArrayList(PatternBinding).empty;
    defer alternative_bindings.deinit(self.allocator);

    try self.collectPatternBindings(module_env, representative_pattern_idx, &representative_bindings);
    try self.collectPatternBindings(module_env, alternative_pattern_idx, &alternative_bindings);

    for (alternative_bindings.items) |alt_binding| {
        for (representative_bindings.items) |rep_binding| {
            if (!rep_binding.ident.eql(alt_binding.ident)) continue;

            const rep_symbol = try self.patternToLocal(rep_binding.pattern_idx);
            const base_key: u64 = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(alt_binding.pattern_idx);
            const key: u128 = (@as(u128, self.current_pattern_scope) << 64) | @as(u128, base_key);
            try self.pattern_symbols.put(key, rep_symbol);
            break;
        }
    }
}

/// Resolve a CIR binding pattern to one executable MIR local.
fn patternToLocal(self: *Self, pattern_idx: CIR.Pattern.Idx) Allocator.Error!MIR.LocalId {
    const base_key: u64 = (@as(u64, self.current_module_idx) << 32) | @intFromEnum(pattern_idx);
    const key: u128 = (@as(u128, self.current_pattern_scope) << 64) | @as(u128, base_key);

    if (self.pattern_symbols.get(key)) |existing| {
        return existing;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);
    const is_top_level_pattern = isTopLevelPattern(module_env, pattern_idx);
    const use_scoped_local_ident = self.current_pattern_scope != 0 and !is_top_level_pattern;

    const ident_idx: Ident.Idx = switch (pattern) {
        .assign => |a| if (use_scoped_local_ident) self.makeSyntheticIdent(a.ident) else a.ident,
        .as => |a| if (use_scoped_local_ident) self.makeSyntheticIdent(a.ident) else a.ident,
        .applied_tag,
        .nominal,
        .nominal_external,
        .record_destructure,
        .list,
        .tuple,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => Ident.Idx.NONE,
    };

    const monotype = try self.monotypeFromTypeVarWithBindings(
        self.current_module_idx,
        self.types_store,
        ModuleEnv.varFrom(pattern_idx),
        &self.type_var_seen,
    );
    const local = try self.store.addLocal(self.allocator, .{
        .monotype = monotype,
        .reassignable = ident_idx.attributes.reassignable,
    });
    try self.pattern_symbols.put(key, local);
    return local;
}

fn lookupExistingPatternLocal(self: *const Self, pattern_idx: CIR.Pattern.Idx) ?MIR.LocalId {
    return self.lookupExistingPatternLocalInScope(
        self.current_module_idx,
        self.current_pattern_scope,
        pattern_idx,
    );
}

fn lookupExistingPatternLocalInScope(
    self: *const Self,
    module_idx: u32,
    pattern_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const base_key: u64 = (@as(u64, module_idx) << 32) | @intFromEnum(pattern_idx);
    const key: u128 = (@as(u128, pattern_scope) << 64) | @as(u128, base_key);
    return self.pattern_symbols.get(key);
}

fn makeSyntheticIdent(self: *Self, original_ident: Ident.Idx) Ident.Idx {
    const idx = self.next_synthetic_ident;
    self.next_synthetic_ident -= 1;
    return .{
        .attributes = original_ident.attributes,
        .idx = idx,
    };
}

fn freshStatementPatternScope(self: *Self) u64 {
    const scope = self.next_statement_pattern_scope;
    self.next_statement_pattern_scope += 1;
    return scope;
}

fn callableBindingHasDemandedValueUse(self: *const Self, pattern_idx: CIR.Pattern.Idx) bool {
    const callable_inst_ids = self.monomorphization.getContextPatternCallableInsts(
        self.current_callable_inst_context,
        self.current_module_idx,
        pattern_idx,
    ) orelse return false;
    return callable_inst_ids.len != 0;
}

fn bindingPatternKey(module_idx: u32, pattern_idx: CIR.Pattern.Idx) u64 {
    return (@as(u64, module_idx) << 32) | @intFromEnum(pattern_idx);
}

fn markSkippedCallableBackedBindingPatterns(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    var bindings = std.ArrayList(PatternBinding).empty;
    defer bindings.deinit(self.allocator);
    try self.collectPatternBindings(module_env, pattern_idx, &bindings);
    for (bindings.items) |binding| {
        try self.skipped_callable_backed_binding_patterns.put(
            bindingPatternKey(self.current_module_idx, binding.pattern_idx),
            {},
        );
    }
}

fn isSkippedCallableBackedBindingPattern(
    self: *const Self,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) bool {
    return self.skipped_callable_backed_binding_patterns.contains(bindingPatternKey(module_idx, pattern_idx));
}

fn monotypesStructurallyEqual(self: *Self, lhs: Monotype.Idx, rhs: Monotype.Idx) Allocator.Error!bool {
    if (lhs == rhs) return true;

    var seen = std.AutoHashMap(u64, void).init(self.allocator);
    defer seen.deinit();

    return try self.monotypesStructurallyEqualRec(lhs, rhs, &seen);
}

fn monotypesStructurallyEqualRec(
    self: *Self,
    lhs: Monotype.Idx,
    rhs: Monotype.Idx,
    seen: *std.AutoHashMap(u64, void),
) Allocator.Error!bool {
    if (lhs == rhs) return true;

    const lhs_u32: u32 = @intFromEnum(lhs);
    const rhs_u32: u32 = @intFromEnum(rhs);
    const key: u64 = (@as(u64, lhs_u32) << 32) | @as(u64, rhs_u32);

    if (seen.contains(key)) return true;
    try seen.put(key, {});

    const lhs_mono = self.store.monotype_store.getMonotype(lhs);
    const rhs_mono = self.store.monotype_store.getMonotype(rhs);
    if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

    return switch (lhs_mono) {
        .recursive_placeholder => {
            if (std.debug.runtime_safety) {
                std.debug.panic("recursive_placeholder survived monotype construction", .{});
            }
            unreachable;
        },
        .unit => true,
        .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
        .list => |lhs_list| try self.monotypesStructurallyEqualRec(lhs_list.elem, rhs_mono.list.elem, seen),
        .box => |lhs_box| try self.monotypesStructurallyEqualRec(lhs_box.inner, rhs_mono.box.inner, seen),
        .tuple => |lhs_tuple| blk: {
            const lhs_elem_span = lhs_tuple.elems;
            const rhs_elem_span = rhs_mono.tuple.elems;
            if (lhs_elem_span.len != rhs_elem_span.len) break :blk false;
            for (0..lhs_elem_span.len) |i| {
                const lhs_elems = self.store.monotype_store.getIdxSpan(lhs_elem_span);
                const rhs_elems = self.store.monotype_store.getIdxSpan(rhs_elem_span);
                const lhs_elem = lhs_elems[i];
                const rhs_elem = rhs_elems[i];
                if (!try self.monotypesStructurallyEqualRec(lhs_elem, rhs_elem, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .func => |lhs_func| blk: {
            const rhs_func = rhs_mono.func;
            if (lhs_func.effectful != rhs_func.effectful) break :blk false;
            if (lhs_func.args.len != rhs_func.args.len) break :blk false;
            for (0..lhs_func.args.len) |i| {
                const lhs_args = self.store.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = self.store.monotype_store.getIdxSpan(rhs_func.args);
                const lhs_arg = lhs_args[i];
                const rhs_arg = rhs_args[i];
                if (!try self.monotypesStructurallyEqualRec(lhs_arg, rhs_arg, seen)) {
                    break :blk false;
                }
            }
            break :blk try self.monotypesStructurallyEqualRec(lhs_func.ret, rhs_func.ret, seen);
        },
        .record => |lhs_record| blk: {
            const lhs_field_span = lhs_record.fields;
            const rhs_field_span = rhs_mono.record.fields;
            if (lhs_field_span.len != rhs_field_span.len) break :blk false;
            for (0..lhs_field_span.len) |i| {
                const lhs_fields = self.store.monotype_store.getFields(lhs_field_span);
                const rhs_fields = self.store.monotype_store.getFields(rhs_field_span);
                const lhs_field = lhs_fields[i];
                const rhs_field = rhs_fields[i];
                if (!self.identsStructurallyEqual(lhs_field.name, rhs_field.name)) break :blk false;
                if (!try self.monotypesStructurallyEqualRec(lhs_field.type_idx, rhs_field.type_idx, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .tag_union => |lhs_union| blk: {
            const lhs_tag_span = lhs_union.tags;
            const rhs_tag_span = rhs_mono.tag_union.tags;
            if (lhs_tag_span.len != rhs_tag_span.len) break :blk false;
            for (0..lhs_tag_span.len) |tag_i| {
                const lhs_tags = self.store.monotype_store.getTags(lhs_tag_span);
                const rhs_tags = self.store.monotype_store.getTags(rhs_tag_span);
                const lhs_tag = lhs_tags[tag_i];
                const rhs_tag = rhs_tags[tag_i];

                if (!self.identsTagNameEquivalent(lhs_tag.name, rhs_tag.name)) break :blk false;

                if (lhs_tag.payloads.len != rhs_tag.payloads.len) break :blk false;
                for (0..lhs_tag.payloads.len) |payload_i| {
                    const lhs_payloads = self.store.monotype_store.getIdxSpan(lhs_tag.payloads);
                    const rhs_payloads = self.store.monotype_store.getIdxSpan(rhs_tag.payloads);
                    const lhs_payload = lhs_payloads[payload_i];
                    const rhs_payload = rhs_payloads[payload_i];
                    if (!try self.monotypesStructurallyEqualRec(lhs_payload, rhs_payload, seen)) {
                        break :blk false;
                    }
                }
            }
            break :blk true;
        },
    };
}

/// Get the monotype for a CIR expression (via its type var).
fn resolveMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    if (self.lookupMonomorphizedExprMonotype(expr_idx)) |mono| {
        return self.importMonotypeFromStore(
            &self.monomorphization.monotype_store,
            mono.idx,
            mono.module_idx,
            self.current_module_idx,
        );
    }

    const type_var = ModuleEnv.varFrom(expr_idx);
    return try self.monotypeFromTypeVarWithBindings(
        self.current_module_idx,
        self.types_store,
        type_var,
        &self.type_var_seen,
    );
}

fn currentCommonIdents(self: *const Self) ModuleEnv.CommonIdents {
    return self.all_module_envs[self.current_module_idx].idents;
}

fn bindPatternMonotypes(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    monotype: Monotype.Idx,
) Allocator.Error!void {
    if (monotype.isNone()) return;

    try self.bindTypeVarMonotypes(ModuleEnv.varFrom(pattern_idx), monotype);

    const pattern = module_env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign,
        .underscore,
        .num_literal,
        .str_literal,
        .dec_literal,
        .small_dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
        .as => |a| {
            try self.bindPatternMonotypes(module_env, a.pattern, monotype);
        },
        .nominal => |nom| {
            try self.bindPatternMonotypes(module_env, nom.backing_pattern, monotype);
        },
        .nominal_external => |nom| {
            try self.bindPatternMonotypes(module_env, nom.backing_pattern, monotype);
        },
        .applied_tag => |tag| {
            const mono_tags = switch (self.store.monotype_store.getMonotype(monotype)) {
                .tag_union => |tag_union| self.store.monotype_store.getTags(tag_union.tags),
                else => typeBindingInvariant(
                    "bindPatternMonotypes(applied_tag): expected tag_union monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };
            const tag_idx = self.tagIndexByName(tag.name, mono_tags);
            const mono_payloads = self.store.monotype_store.getIdxSpan(mono_tags[tag_idx].payloads);
            const payload_patterns = module_env.store.slicePatterns(tag.args);

            if (builtin.mode == .Debug and payload_patterns.len != mono_payloads.len) {
                std.debug.panic(
                    "bindPatternMonotypes(applied_tag): payload arity mismatch for tag '{s}' (patterns={d}, monos={d})",
                    .{ module_env.getIdent(tag.name), payload_patterns.len, mono_payloads.len },
                );
            }

            for (payload_patterns, mono_payloads) |payload_pattern_idx, payload_mono| {
                try self.bindPatternMonotypes(module_env, payload_pattern_idx, payload_mono);
            }
        },
        .record_destructure => |record_pat| {
            const mono_fields = switch (self.store.monotype_store.getMonotype(monotype)) {
                .record => |record_mono| self.store.monotype_store.getFields(record_mono.fields),
                .unit => &.{},
                else => typeBindingInvariant(
                    "bindPatternMonotypes(record_destructure): expected record monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };

            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                const pat_idx = destruct.kind.toPatternIdx();
                const field_idx = self.recordFieldIndexByName(destruct.label, mono_fields);
                try self.bindPatternMonotypes(module_env, pat_idx, mono_fields[field_idx].type_idx);
            }
        },
        .list => |list_pat| {
            const elem_mono = switch (self.store.monotype_store.getMonotype(monotype)) {
                .list => |list_mono| list_mono.elem,
                else => typeBindingInvariant(
                    "bindPatternMonotypes(list): expected list monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };

            for (module_env.store.slicePatterns(list_pat.patterns)) |elem_pattern_idx| {
                try self.bindPatternMonotypes(module_env, elem_pattern_idx, elem_mono);
            }

            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try self.bindPatternMonotypes(module_env, rest_pattern_idx, monotype);
                }
            }
        },
        .tuple => |tuple_pat| {
            const mono_elems = switch (self.store.monotype_store.getMonotype(monotype)) {
                .tuple => |tuple_mono| self.store.monotype_store.getIdxSpan(tuple_mono.elems),
                else => typeBindingInvariant(
                    "bindPatternMonotypes(tuple): expected tuple monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };
            const elem_patterns = module_env.store.slicePatterns(tuple_pat.patterns);

            if (builtin.mode == .Debug and elem_patterns.len != mono_elems.len) {
                std.debug.panic(
                    "bindPatternMonotypes(tuple): arity mismatch (patterns={d}, monos={d})",
                    .{ elem_patterns.len, mono_elems.len },
                );
            }

            for (elem_patterns, mono_elems) |elem_pattern_idx, elem_mono| {
                try self.bindPatternMonotypes(module_env, elem_pattern_idx, elem_mono);
            }
        },
    }
}

fn tupleMonotypeForFields(self: *Self, field_monotypes: []const Monotype.Idx) Allocator.Error!Monotype.Idx {
    const elems = try self.store.monotype_store.addIdxSpan(self.allocator, field_monotypes);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .tuple = .{ .elems = elems } });
}

fn tagPayloadMonotypesByName(
    self: *Self,
    union_monotype: Monotype.Idx,
    tag_name: Ident.Idx,
) []const Monotype.Idx {
    const mono = self.store.monotype_store.getMonotype(union_monotype);
    const tags = switch (mono) {
        .tag_union => |tu| self.store.monotype_store.getTags(tu.tags),
        else => typeBindingInvariant(
            "tag payload lookup expected tag_union monotype, found '{s}'",
            .{@tagName(mono)},
        ),
    };

    for (tags) |tag| {
        if (self.identsTagNameEquivalent(tag.name, tag_name)) {
            return self.store.monotype_store.getIdxSpan(tag.payloads);
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "tag '{s}' missing from monotype",
        .{module_env.getIdent(tag_name)},
    );
}

fn resolveTagNameForMonotype(
    self: *Self,
    union_monotype: Monotype.Idx,
    tag_name: Ident.Idx,
) Monotype.Name {
    const mono = self.store.monotype_store.getMonotype(union_monotype);
    const tags = switch (mono) {
        .tag_union => |tu| self.store.monotype_store.getTags(tu.tags),
        else => typeBindingInvariant(
            "tag name lookup expected tag_union monotype, found '{s}'",
            .{@tagName(mono)},
        ),
    };

    for (tags) |tag| {
        if (self.identsTagNameEquivalent(tag.name, tag_name)) return tag.name;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "tag '{s}' missing from monotype",
        .{module_env.getIdent(tag_name)},
    );
}

fn bindTagPayloadsByName(
    self: *Self,
    tag_name: Ident.Idx,
    payload_vars: []const types.Var,
    mono_tags: []const Monotype.Tag,
) Allocator.Error!void {
    for (mono_tags) |mono_tag| {
        if (!self.identsTagNameEquivalent(mono_tag.name, tag_name)) continue;

        const mono_payload_span = mono_tag.payloads;
        if (payload_vars.len != mono_payload_span.len) {
            const module_env = self.all_module_envs[self.current_module_idx];
            typeBindingInvariant(
                "bindFlatTypeMonotypes(tag_union): payload arity mismatch for tag '{s}'",
                .{module_env.getIdent(tag_name)},
            );
        }
        for (payload_vars, 0..) |payload_var, i| {
            const mono_payload = self.store.monotype_store.getIdxSpan(mono_payload_span)[i];
            try self.bindTypeVarMonotypes(payload_var, mono_payload);
        }
        return;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "bindFlatTypeMonotypes(tag_union): tag '{s}' missing from monotype",
        .{module_env.getIdent(tag_name)},
    );
}

fn bindRecordFieldByName(
    self: *Self,
    field_name: Ident.Idx,
    field_var: types.Var,
    mono_fields: []const Monotype.Field,
) Allocator.Error!void {
    for (mono_fields) |mono_field| {
        if (!self.identsStructurallyEqual(field_name, mono_field.name)) continue;
        try self.bindTypeVarMonotypes(field_var, mono_field.type_idx);
        return;
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    typeBindingInvariant(
        "bindFlatTypeMonotypes(record_unbound): field '{s}' missing from monotype",
        .{module_env.getIdent(field_name)},
    );
}

/// Bind concrete monotypes to polymorphic vars for the current lowering scope.
fn bindTypeVarMonotypes(self: *Self, type_var: types.Var, monotype: Monotype.Idx) Allocator.Error!void {
    if (monotype.isNone()) return;

    const resolved = self.types_store.resolveVar(type_var);
    if (self.type_var_seen.get(resolved.var_)) |existing| {
        if (!(try self.monotypesStructurallyEqual(existing, monotype))) {
            typeBindingInvariant(
                "bindTypeVarMonotypes: conflicting monotype binding for type var root {d} (existing={d}, new={d})",
                .{ @intFromEnum(resolved.var_), @intFromEnum(existing), @intFromEnum(monotype) },
            );
        }
        return;
    }

    switch (resolved.desc.content) {
        .flex, .rigid => {
            try self.type_var_seen.put(resolved.var_, monotype);
        },
        .alias => |alias| {
            const backing_var = self.types_store.getAliasBackingVar(alias);
            try self.bindTypeVarMonotypes(backing_var, monotype);
        },
        .structure => |flat_type| {
            // Register before recursing so recursive structures short-circuit.
            try self.type_var_seen.put(resolved.var_, monotype);
            try self.bindFlatTypeMonotypes(flat_type, monotype);
        },
        .err => {},
    }
}

fn bindFlatTypeMonotypes(self: *Self, flat_type: types.FlatType, monotype: Monotype.Idx) Allocator.Error!void {
    if (monotype.isNone()) return;

    const mono = self.store.monotype_store.getMonotype(monotype);
    switch (flat_type) {
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            const mfunc = switch (mono) {
                .func => |mfunc| mfunc,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(fn): expected function monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const type_args = self.types_store.sliceVars(func.args);
            const mono_arg_span = mfunc.args;
            if (type_args.len != mono_arg_span.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(fn): arity mismatch (type={d}, monotype={d})",
                    .{ type_args.len, mono_arg_span.len },
                );
            }
            for (type_args, 0..) |ta, i| {
                const ma = self.store.monotype_store.getIdxSpan(mono_arg_span)[i];
                try self.bindTypeVarMonotypes(ta, ma);
            }
            try self.bindTypeVarMonotypes(func.ret, mfunc.ret);
        },
        .nominal_type => |nominal| {
            const common = self.currentCommonIdents();
            const ident = nominal.ident.ident_idx;
            const origin = nominal.origin_module;

            if (origin.eql(common.builtin_module) and ident.eql(common.list)) {
                const mlist = switch (mono) {
                    .list => |mlist| mlist,
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal List): expected list monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const type_args = self.types_store.sliceNominalArgs(nominal);
                if (type_args.len != 1) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal List): expected exactly 1 type arg, found {d}",
                        .{type_args.len},
                    );
                }
                try self.bindTypeVarMonotypes(type_args[0], mlist.elem);
                return;
            }
            if (origin.eql(common.builtin_module) and ident.eql(common.box)) {
                const mbox = switch (mono) {
                    .box => |mbox| mbox,
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal Box): expected box monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                };
                const type_args = self.types_store.sliceNominalArgs(nominal);
                if (type_args.len != 1) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal Box): expected exactly 1 type arg, found {d}",
                        .{type_args.len},
                    );
                }
                try self.bindTypeVarMonotypes(type_args[0], mbox.inner);
                return;
            }

            if (origin.eql(common.builtin_module) and builtinPrimForNominal(ident, common) != null) {
                switch (mono) {
                    .prim => {},
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal prim): expected prim monotype, found '{s}'",
                        .{@tagName(mono)},
                    ),
                }
                return;
            }

            // Non-builtin nominals (and non-primitive builtin nominals) resolve by backing var.
            const backing_var = self.types_store.getNominalBackingVar(nominal);
            try self.bindTypeVarMonotypes(backing_var, monotype);
        },
        .record => |record| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                .unit => {
                    if (flatRecordRepresentsEmpty(self.types_store, record)) return;
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(record): non-empty record matched unit monotype",
                        .{},
                    );
                },
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(record): expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const mono_field_span = mrec.fields;
            // Copy mono_fields into a local owned buffer. Recursive bindTypeVarMonotypes
            // calls below may reallocate the monotype store, invalidating any direct
            // slice into it. Monotype.Field contains only indices (no pointers).
            var owned_mono_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
            defer owned_mono_fields.deinit(self.allocator);
            try owned_mono_fields.appendSlice(self.allocator, self.store.monotype_store.getFields(mono_field_span));
            const mono_fields = owned_mono_fields.items;
            var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_field_indices.deinit(self.allocator);

            var current_row = record;
            rows: while (true) {
                const fields_slice = self.types_store.getRecordFieldsSlice(current_row.fields);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);

                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                    try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                    try self.bindTypeVarMonotypes(field_var, mono_fields[field_idx].type_idx);
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = self.types_store.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = self.types_store.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = self.types_store.getRecordFieldsSlice(fields_range);
                                const ext_field_names = ext_fields.items(.name);
                                const ext_field_vars = ext_fields.items(.var_);
                                for (ext_field_names, ext_field_vars) |field_name, field_var| {
                                    const field_idx = self.recordFieldIndexByName(field_name, mono_fields);
                                    try appendSeenIndex(self.allocator, &seen_field_indices, field_idx);
                                    try self.bindTypeVarMonotypes(field_var, mono_fields[field_idx].type_idx);
                                }
                                break :rows;
                            },
                            .empty_record => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypes(record): unexpected ext flat type '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex, .rigid => {
                            try self.bindRecordRowTail(ext_var, mono_fields, seen_field_indices.items);
                            for (mono_fields, 0..) |_, field_idx| {
                                try appendSeenIndex(self.allocator, &seen_field_indices, @intCast(field_idx));
                            }
                            break :rows;
                        },
                        .err => typeBindingInvariant(
                            "bindFlatTypeMonotypes(record): error extension",
                            .{},
                        ),
                    }
                }
            }

            for (mono_fields, 0..) |mono_field, field_idx| {
                if (!seenIndex(seen_field_indices.items, @intCast(field_idx))) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(record): monotype field '{s}' missing from type row",
                        .{mono_field.name.text(self.all_module_envs)},
                    );
                }
            }
        },
        .record_unbound => |fields_range| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                .unit => {
                    if (self.types_store.getRecordFieldsSlice(fields_range).len == 0) return;
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(record_unbound): non-empty record matched unit monotype",
                        .{},
                    );
                },
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(record_unbound): expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const fields_slice = self.types_store.getRecordFieldsSlice(fields_range);
            const field_names = fields_slice.items(.name);
            const field_vars = fields_slice.items(.var_);
            const mono_field_span = mrec.fields;

            if (field_names.len != mono_field_span.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(record_unbound): field count mismatch (type={d}, monotype={d})",
                    .{ field_names.len, mono_field_span.len },
                );
            }

            for (field_names, field_vars) |field_name, field_var| {
                try self.bindRecordFieldByName(field_name, field_var, self.store.monotype_store.getFields(mono_field_span));
            }
        },
        .tuple => |tuple| {
            const mtuple = switch (mono) {
                .tuple => |mtuple| mtuple,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(tuple): expected tuple monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const elem_vars = self.types_store.sliceVars(tuple.elems);
            const mono_elem_span = mtuple.elems;
            if (elem_vars.len != mono_elem_span.len) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(tuple): arity mismatch (type={d}, monotype={d})",
                    .{ elem_vars.len, mono_elem_span.len },
                );
            }
            for (elem_vars, 0..) |ev, i| {
                const me = self.store.monotype_store.getIdxSpan(mono_elem_span)[i];
                try self.bindTypeVarMonotypes(ev, me);
            }
        },
        .tag_union => |tag_union_row| {
            const mono_tag_span = switch (mono) {
                .tag_union => |mtu| mtu.tags,
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(tag_union): expected tag_union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            // Copy mono_tags into a local owned buffer. Recursive bindTypeVarMonotypes
            // calls below may reallocate the monotype store (e.g. via addTags/addMonotype
            // in remainingTagUnionTailMonotype), which would invalidate any direct slice
            // into the store. Monotype.Tag contains only indices (no pointers), so a
            // value copy is safe and cheap.
            var owned_mono_tags: std.ArrayListUnmanaged(Monotype.Tag) = .empty;
            defer owned_mono_tags.deinit(self.allocator);
            try owned_mono_tags.appendSlice(self.allocator, self.store.monotype_store.getTags(mono_tag_span));
            const mono_tags = owned_mono_tags.items;
            var seen_tag_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_tag_indices.deinit(self.allocator);

            var current_row = tag_union_row;
            rows: while (true) {
                const type_tags = self.types_store.getTagsSlice(current_row.tags);
                const type_tag_names = type_tags.items(.name);
                const type_tag_args = type_tags.items(.args);

                for (type_tag_names, type_tag_args) |tag_name, tag_args| {
                    const tag_idx = self.tagIndexByName(tag_name, mono_tags);
                    try appendSeenIndex(self.allocator, &seen_tag_indices, tag_idx);
                    const payload_vars = self.types_store.sliceVars(tag_args);
                    try self.bindTagPayloadsByName(tag_name, payload_vars, mono_tags);
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = self.types_store.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = self.types_store.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .tag_union => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .empty_tag_union => break :rows,
                            else => typeBindingInvariant(
                                "bindFlatTypeMonotypes(tag_union): unexpected ext flat type '{s}'",
                                .{@tagName(ext_flat)},
                            ),
                        },
                        .flex, .rigid => {
                            try self.bindTagUnionRowTail(ext_var, mono_tags, seen_tag_indices.items);
                            for (mono_tags, 0..) |_, tag_idx| {
                                try appendSeenIndex(self.allocator, &seen_tag_indices, @intCast(tag_idx));
                            }
                            break :rows;
                        },
                        .err => typeBindingInvariant(
                            "bindFlatTypeMonotypes(tag_union): error extension",
                            .{},
                        ),
                    }
                }
            }

            for (mono_tags, 0..) |mono_tag, tag_idx| {
                if (!seenIndex(seen_tag_indices.items, @intCast(tag_idx))) {
                    typeBindingInvariant(
                        "bindFlatTypeMonotypes(tag_union): monotype tag '{s}' missing from type row",
                        .{mono_tag.name.text(self.all_module_envs)},
                    );
                }
            }
        },
        .empty_record => {
            switch (mono) {
                .unit => {},
                .record => |mrec| {
                    const fields = self.store.monotype_store.getFields(mrec.fields);
                    if (fields.len != 0) {
                        typeBindingInvariant(
                            "bindFlatTypeMonotypes(empty_record): expected zero record fields, found {d}",
                            .{fields.len},
                        );
                    }
                },
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(empty_record): expected unit/empty-record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            }
        },
        .empty_tag_union => {
            const mono_tags = switch (mono) {
                .tag_union => |mtu| self.store.monotype_store.getTags(mtu.tags),
                else => typeBindingInvariant(
                    "bindFlatTypeMonotypes(empty_tag_union): expected empty tag union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            if (mono_tags.len != 0) {
                typeBindingInvariant(
                    "bindFlatTypeMonotypes(empty_tag_union): expected zero tags, found {d}",
                    .{mono_tags.len},
                );
            }
        },
    }
}
