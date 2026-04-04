//! CIR → strongest-form MIR lowering.
//!
//! Converts polymorphic, sugar-rich CIR into monomorphic, statement-only MIR.
//! Callable instantiation must already be decided by the staged callable
//! pipeline
//! before this pass runs.
//!
//! Key transformations:
//! - `e_if` lowers directly to explicit Bool `switch_stmt`
//! - nested value expressions lower into explicit local-defining statements
//! - `e_nominal` strips to backing value plus explicit nominal assignment
//! - callable introduction sites (`e_lambda`, `e_closure`, `e_hosted_lambda`)
//!   lower to explicit `assign_lambda` / `assign_closure`
//! - existing callable values flow through ordinary lookup/projection/ref paths;
//!   they are never reconstructed from lexical captures during lookup/call lowering
//! - global names appear only through `assign_symbol`

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const build_options = @import("build_options");
const builtins = @import("builtins");
const can = @import("can");
const corecir = @import("corecir");
const types = @import("types");

const MIR = @import("MIR.zig");
const Monotype = corecir.Monotype;
const Pipeline = corecir.Pipeline;
const DebugVerifyMir = @import("DebugVerifyMir.zig");

const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const Allocator = std.mem.Allocator;

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

const Self = @This();

const trace = struct {
    const enabled = build_options.trace_eval;

    fn log(comptime fmt: []const u8, args: anytype) void {
        if (!enabled) return;
        std.debug.print("[mir-lower] " ++ fmt ++ "\n", args);
    }
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

const BindingNode = struct {
    ident: Ident.Idx,
    pattern_idx: CIR.Pattern.Idx,
};

const PlannedRuntimeCaptureLocal = struct {
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
    local_monotype: Monotype.Idx,
    callable_value: ?Pipeline.CallableValue = null,
    storage: Pipeline.CaptureStorage,
};

const PlannedCallableCaptureLocal = struct {
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
    callable_inst_id: Pipeline.CallableInstId,
};

const TopLevelDefCallableSemantics = union(enum) {
    non_callable,
    callable: Pipeline.CallableValue,
};

const PlannedCaptureLocals = struct {
    runtime: std.ArrayList(PlannedRuntimeCaptureLocal),
    callable_only: std.ArrayList(PlannedCallableCaptureLocal),
    recursive: std.ArrayList(PlannedCallableCaptureLocal),

    fn init() PlannedCaptureLocals {
        return .{
            .runtime = .empty,
            .callable_only = .empty,
            .recursive = .empty,
        };
    }

    fn deinit(self: *PlannedCaptureLocals, allocator: Allocator) void {
        self.runtime.deinit(allocator);
        self.callable_only.deinit(allocator);
        self.recursive.deinit(allocator);
    }
};

const CallableResolution = MIR.CallableResolution;

const LambdaEntryBindingState = struct {
    by_local: std.AutoHashMap(u32, void),

    fn init(allocator: Allocator) LambdaEntryBindingState {
        return .{ .by_local = std.AutoHashMap(u32, void).init(allocator) };
    }

    fn deinit(self: *LambdaEntryBindingState) void {
        self.by_local.deinit();
    }

    fn add(self: *LambdaEntryBindingState, local: MIR.LocalId) Allocator.Error!void {
        try self.by_local.put(@intFromEnum(local), {});
    }

    fn contains(self: *const LambdaEntryBindingState, local: MIR.LocalId) bool {
        return self.by_local.contains(@intFromEnum(local));
    }

    fn cloneFrom(
        allocator: Allocator,
        source: ?*const LambdaEntryBindingState,
    ) Allocator.Error!LambdaEntryBindingState {
        var cloned = LambdaEntryBindingState.init(allocator);
        if (source) |existing| {
            var it = existing.by_local.iterator();
            while (it.next()) |entry| {
                try cloned.by_local.put(entry.key_ptr.*, {});
            }
        }
        return cloned;
    }
};

const LowerExprSourceRef = struct {
    source_context: Pipeline.SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const Pipeline.ValueProjection = &.{},
};

const LoopContext = struct {
    exit_id: MIR.JoinPointId,
    exit_scope: u64,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const VisibleBindingLocal = struct {
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
};

fn recordSeenCapturePattern(
    seen: *std.AutoHashMap(CIR.Pattern.Idx, void),
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!bool {
    if (seen.contains(pattern_idx)) return false;
    try seen.put(pattern_idx, {});
    return true;
}

const BlockStmtScopeInfo = struct {
    before: u64,
    after: u64,
};

const LoweredExprWithScope = struct {
    entry: MIR.CFStmtId,
    exit_scope: u64,
    falls_through: bool,
};

const MatchBranchChainResult = struct {
    entry: MIR.CFStmtId,
    reaches_result_join: bool,
};

const ExprSequenceScopeInfo = struct {
    before: u64,
    after: u64,
};

const BindingRecord = struct {
    local: ?MIR.LocalId = null,
    explicit_monotype: ?Monotype.Idx = null,
    source_expr_ref: ?LowerExprSourceRef = null,
    callable_value: ?Pipeline.CallableValue = null,
    shadowed: bool = false,
};

// --- Fields ---

allocator: Allocator,

/// Target MIR store
store: *MIR.Store,

/// Explicit callable-instantiation decisions computed before lowering.
callable_pipeline: *const Pipeline.Result,

/// All module environments (indexed by module_idx)
all_module_envs: []const *ModuleEnv,

/// Types store for resolving type variables
types_store: *const types.Store,

/// Current module being lowered
current_module_idx: u32,

/// Lowering-time scope key used to distinguish lexically-shadowed CIR binders.
/// This is only a source-binder lookup environment during CIR -> MIR lowering.
/// It must never be treated as executable MIR semantics.
/// 0 means unscoped (module/pattern only).
current_binding_scope: u64,

/// App module index (for resolving `e_lookup_required` from platform modules)
app_module_idx: ?u32,

/// Map from ((scope_key << 64) | (module_idx << 32 | CIR.Pattern.Idx)) → lowering binding.
/// Used only to resolve CIR local lookups to the current MIR local during lowering.
binding_records: std.AutoHashMap(u128, BindingRecord),

/// Cycle breakers for recursive nominal types (e.g. Tree := [Leaf, Node(Tree)]).
/// Used only by `fromNominalType` during monotype construction; separate from
/// specialization bindings so monotype construction never pollutes them.
nominal_cycle_breakers: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cache for callable bodies already lowered for callable instances chosen by callable_pipeline.
/// Closure values are materialized from explicit specialization-owned capture
/// sources in the current lowering scope, but the lowered lambda body is unique
/// per callable inst.
lowered_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Cache for direct statement-lowered lambda expressions keyed by the full
/// lowering context that can affect the emitted callable body.
lowered_callable_lambdas: std.AutoHashMap(CallableCacheKey, MIR.LambdaId),

/// Metadata for opaque symbol IDs; populated at symbol construction time.
symbol_metadata: std.AutoHashMap(u64, SymbolMetadata),

/// Counter for generating synthetic ident indices for polymorphic specializations.
/// Counts down from NONE - 1 to avoid collision with real idents.
next_synthetic_ident: u29,

/// Counter for assigning unique local-pattern scopes to statement-lowered lambdas.
next_statement_binding_scope: u64,

/// Parent scope links for lexically nested statement-lowered lambdas.
binding_scope_parents: std.AutoHashMap(u64, u64),

/// Counter for assigning unique explicit join-point ids in strongest-form MIR.
next_join_point_id: u32,

/// First local index that belongs to the currently lowered root/lambda body.
current_body_local_floor: usize,

/// Tracks callable instances currently being lowered (recursion guard).
in_progress_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Reserved callable skeletons for recursive callable-inst groups whose bodies are not
/// fully lowered yet but already need stable lambda ids.
reserved_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Pending exact callable resolutions for join params, accumulated from all
/// incoming jumps before the join statement is recorded.
pending_join_param_callables: std.AutoHashMap(u32, ?CallableResolution),

/// Locals whose entry bindings are prepended after the body is lowered.
current_lambda_entry_bound_locals: ?*LambdaEntryBindingState,

/// Locals whose non-lambda body preludes are prepended after the body fragment
/// is lowered (for example loop item bindings and match-branch destructures).
current_prelude_bound_locals: ?*LambdaEntryBindingState,

/// Root pattern scope for the currently lowered lambda body. Bindings found
/// within this scope tree are ordinary lexical lookups, not implicit captures.
current_lambda_scope_boundary: ?u64,

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
    callable_pipeline: *const Pipeline.Result,
    all_module_envs: []const *ModuleEnv,
    types_store: *const types.Store,
    current_module_idx: u32,
    app_module_idx: ?u32,
) Allocator.Error!Self {
    return .{
        .allocator = allocator,
        .store = store,
        .callable_pipeline = callable_pipeline,
        .all_module_envs = all_module_envs,
        .types_store = types_store,
        .current_module_idx = current_module_idx,
        .current_binding_scope = 0,
        .app_module_idx = app_module_idx,
        .binding_records = std.AutoHashMap(u128, BindingRecord).init(allocator),
        .nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .lowered_callable_lambdas = std.AutoHashMap(CallableCacheKey, MIR.LambdaId).init(allocator),
        .symbol_metadata = std.AutoHashMap(u64, SymbolMetadata).init(allocator),
        .next_synthetic_ident = Ident.Idx.NONE.idx - 1,
        .next_statement_binding_scope = 1,
        .binding_scope_parents = std.AutoHashMap(u64, u64).init(allocator),
        .next_join_point_id = 0,
        .current_body_local_floor = 0,
        .in_progress_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .reserved_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .pending_join_param_callables = std.AutoHashMap(u32, ?CallableResolution).init(allocator),
        .current_lambda_entry_bound_locals = null,
        .current_prelude_bound_locals = null,
        .current_lambda_scope_boundary = null,
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
    self.binding_records.deinit();
    self.nominal_cycle_breakers.deinit();
    self.lowered_callable_insts.deinit();
    self.lowered_callable_lambdas.deinit();
    self.symbol_metadata.deinit();
    self.binding_scope_parents.deinit();
    self.in_progress_callable_insts.deinit();
    self.reserved_callable_insts.deinit();
    self.pending_join_param_callables.deinit();
    self.scratch_local_ids.deinit();
    self.scratch_ident_idxs.deinit();
    self.scratch_cf_stmt_ids.deinit();
    self.scratch_capture_locals.deinit();
    self.active_loops.deinit(self.allocator);
    self.mono_scratches.deinit();
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

fn callableInstSourceContext(context_callable_inst: Pipeline.CallableInstId) Pipeline.SourceContext {
    return .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) };
}

const LowerSession = struct {
    lower: *Self,
    source_context: Pipeline.SourceContext,

    fn withSourceContext(self: LowerSession, source_context: Pipeline.SourceContext) LowerSession {
        return .{
            .lower = self.lower,
            .source_context = source_context,
        };
    }

    fn requireCallableInstContext(self: LowerSession) Pipeline.CallableInstId {
        return switch (self.source_context) {
            .callable_inst => |context_id| @enumFromInt(@intFromEnum(context_id)),
            .root_expr, .provenance_expr, .template_expr => std.debug.panic(
                "Lower invariant violated: operation required callable-inst context but active source context was {s}",
                .{@tagName(self.source_context)},
            ),
        };
    }

    fn callableInstRawForDebug(self: LowerSession) u32 {
        return switch (self.source_context) {
            .callable_inst => |context_id| @intFromEnum(@as(Pipeline.CallableInstId, @enumFromInt(@intFromEnum(context_id)))),
            .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
    }

    fn rootExprRawForDebug(self: LowerSession) u32 {
        return switch (self.source_context) {
            .root_expr => |root| @intFromEnum(root.expr_idx),
            .callable_inst, .provenance_expr, .template_expr => std.math.maxInt(u32),
        };
    }

    fn activeCallableDef(self: LowerSession) ?*const Pipeline.CallableDef {
        const callable_inst_id = switch (self.source_context) {
            .callable_inst => |context_id| @as(Pipeline.CallableInstId, @enumFromInt(@intFromEnum(context_id))),
            .root_expr, .provenance_expr, .template_expr => return null,
        };
        return self.lower.callable_pipeline.getCallableDefForInst(callable_inst_id);
    }
};

fn lowerSession(self: *Self, source_context: Pipeline.SourceContext) LowerSession {
    return .{
        .lower = self,
        .source_context = source_context,
    };
}

fn lowerExprSourceRefFromPipeline(self: *const Self, expr_ref: Pipeline.ExprRef) LowerExprSourceRef {
    return .{
        .source_context = expr_ref.source_context,
        .module_idx = expr_ref.module_idx,
        .expr_idx = expr_ref.expr_idx,
        .projections = self.callable_pipeline.getValueProjectionEntries(expr_ref.projections),
    };
}

fn exprSourceRefAliasOrSelf(
    self: *const Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) LowerExprSourceRef {
    if (self.callable_pipeline.getExprOriginExpr(
        session.source_context,
        self.current_module_idx,
        expr_idx,
    )) |origin| {
        return self.lowerExprSourceRefFromPipeline(origin);
    }

    return .{
        .source_context = session.source_context,
        .module_idx = self.current_module_idx,
        .expr_idx = expr_idx,
        .projections = &.{},
    };
}

fn extendLowerExprSourceRef(
    self: *Self,
    source_expr_ref: LowerExprSourceRef,
    projection: Pipeline.ValueProjection,
) Allocator.Error!LowerExprSourceRef {
    const next_projections = try self.allocator.alloc(
        Pipeline.ValueProjection,
        source_expr_ref.projections.len + 1,
    );
    @memcpy(next_projections[0..source_expr_ref.projections.len], source_expr_ref.projections);
    next_projections[source_expr_ref.projections.len] = projection;
    return .{
        .source_context = source_expr_ref.source_context,
        .module_idx = source_expr_ref.module_idx,
        .expr_idx = source_expr_ref.expr_idx,
        .projections = next_projections,
    };
}

fn callableDefBodyExpr(self: *const Self, callable_def: *const Pipeline.CallableDef) *const corecir.Lambdamono.Expr {
    return self.callable_pipeline.getProgramExprForRef(callable_def.body_expr);
}

fn callableDefBodyRetMonotype(
    self: *Self,
    callable_def: *const Pipeline.CallableDef,
) Allocator.Error!Monotype.Idx {
    const body_expr = self.callableDefBodyExpr(callable_def);
    const resolved = if (body_expr.getCallable()) |callable_semantics| switch (callable_semantics) {
        .callable => |callable_value| self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value),
        .intro => |intro| self.callable_pipeline.getCallableValueRuntimeMonotype(intro.callable_value),
    } else body_expr.common().monotype;
    return self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        resolved.idx,
        resolved.module_idx,
        self.current_module_idx,
    );
}

fn callableRuntimeValueKindText(runtime_value: Pipeline.RuntimeValue) []const u8 {
    return switch (runtime_value) {
        .direct_lambda => "direct",
        .closure => "closure",
    };
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

fn resolveImportedModuleIdx(self: *Self, caller_env: *const ModuleEnv, import_idx: CIR.Import.Idx) ?u32 {
    if (caller_env.imports.getResolvedModule(import_idx)) |module_idx| {
        if (module_idx < self.all_module_envs.len) return module_idx;
    }

    const import_pos = @intFromEnum(import_idx);
    if (import_pos >= caller_env.imports.imports.len()) return null;

    const import_name = caller_env.common.getString(caller_env.imports.imports.items.items[import_pos]);
    const base_name = identLastSegment(import_name);

    for (self.all_module_envs, 0..) |module_env_entry, module_idx| {
        if (std.mem.eql(u8, module_env_entry.module_name, import_name) or
            std.mem.eql(u8, module_env_entry.module_name, base_name))
        {
            @constCast(&caller_env.imports).setResolvedModule(import_idx, @intCast(module_idx));
            return @intCast(module_idx);
        }
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
    field_name: anytype,
    mono_fields: []const Monotype.Field,
) u32 {
    for (mono_fields, 0..) |mono_field, field_idx| {
        if (self.identsStructurallyEqual(field_name, mono_field.name)) {
            return @intCast(field_idx);
        }
    }

    std.debug.panic(
        "statement-only MIR invariant violated: record field '{s}' missing from monotype",
        .{self.labelTextForCompare(field_name) orelse "<unknown-field>"},
    );
}

fn tagIndexByName(
    self: *Self,
    tag_name: anytype,
    mono_tags: []const Monotype.Tag,
) u32 {
    for (mono_tags, 0..) |mono_tag, tag_idx| {
        if (self.identsStructurallyEqual(tag_name, mono_tag.name)) {
            return @intCast(tag_idx);
        }
    }

    std.debug.panic(
        "statement-only MIR invariant violated: tag '{s}' missing from monotype",
        .{self.labelTextForCompare(tag_name) orelse "<unknown-tag>"},
    );
}

fn collectBindingNodes(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    out: *std.ArrayList(BindingNode),
) Allocator.Error!void {
    const pattern = module_env.store.getPattern(pattern_idx);
    switch (pattern) {
        .assign => |assign| try out.append(self.allocator, .{ .ident = assign.ident, .pattern_idx = pattern_idx }),
        .as => |as_pat| {
            try out.append(self.allocator, .{ .ident = as_pat.ident, .pattern_idx = pattern_idx });
            try self.collectBindingNodes(module_env, as_pat.pattern, out);
        },
        .tuple => |tuple| {
            for (module_env.store.slicePatterns(tuple.patterns)) |elem_pattern_idx| {
                try self.collectBindingNodes(module_env, elem_pattern_idx, out);
            }
        },
        .applied_tag => |tag| {
            for (module_env.store.slicePatterns(tag.args)) |arg_pattern_idx| {
                try self.collectBindingNodes(module_env, arg_pattern_idx, out);
            }
        },
        .record_destructure => |destructure| {
            for (module_env.store.sliceRecordDestructs(destructure.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required => |sub_pattern_idx| try self.collectBindingNodes(module_env, sub_pattern_idx, out),
                    .SubPattern => |sub_pattern_idx| try self.collectBindingNodes(module_env, sub_pattern_idx, out),
                    .Rest => |sub_pattern_idx| try self.collectBindingNodes(module_env, sub_pattern_idx, out),
                }
            }
        },
        .list => |list| {
            for (module_env.store.slicePatterns(list.patterns)) |elem_pattern_idx| {
                try self.collectBindingNodes(module_env, elem_pattern_idx, out);
            }
            if (list.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try self.collectBindingNodes(module_env, rest_pattern_idx, out);
                }
            }
        },
        .nominal => |nom| try self.collectBindingNodes(module_env, nom.backing_pattern, out),
        .nominal_external => |nom| try self.collectBindingNodes(module_env, nom.backing_pattern, out),
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

const CallableCacheKey = struct {
    source_context_kind: enum(u2) { callable_inst, root_expr, provenance_expr, template_expr },
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    parent_binding_scope: u64,
};

fn makeCallableCacheKey(
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) CallableCacheKey {
    const self = session.lower;
    const source_context = session.source_context;
    return .{
        .source_context_kind = switch (source_context) {
            .callable_inst => .callable_inst,
            .root_expr => .root_expr,
            .provenance_expr => .provenance_expr,
            .template_expr => .template_expr,
        },
        .source_context_module_idx = switch (source_context) {
            .callable_inst => std.math.maxInt(u32),
            .root_expr => |root| root.module_idx,
            .provenance_expr => |source| source.module_idx,
            .template_expr => |template| template.module_idx,
        },
        .source_context_raw = switch (source_context) {
            .callable_inst => |callable_inst| @intFromEnum(callable_inst),
            .root_expr => |root| @intFromEnum(root.expr_idx),
            .provenance_expr => |source| @intFromEnum(source.expr_idx),
            .template_expr => |template| @intFromEnum(template.expr_idx),
        },
        .module_idx = self.current_module_idx,
        .expr_idx = expr_idx,
        .fn_monotype = fn_monotype,
        .parent_binding_scope = self.current_binding_scope,
    };
}

fn lookupLoweredCallableLambdaCache(
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) Allocator.Error!?MIR.LambdaId {
    return session.lower.lowered_callable_lambdas.get(makeCallableCacheKey(session, expr_idx, fn_monotype));
}

fn lookupProgramExprMonotype(session: LowerSession, expr_idx: CIR.Expr.Idx) ?Pipeline.ResolvedMonotype {
    return session.lower.callable_pipeline.getExprMonotype(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn resolveProgramExprMonotype(
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!?Pipeline.ResolvedMonotype {
    if (lookupProgramExprCallableValue(session, expr_idx)) |callable_value| {
        return session.lower.callable_pipeline.getCallableValueRuntimeMonotype(callable_value);
    }
    const SourceContextThread = struct {
        source_context: Pipeline.SourceContext,

        pub fn requireSourceContext(self: @This()) Pipeline.SourceContext {
            return self.source_context;
        }
    };
    return corecir.ContextMono.resolveExprMonotypeResolved(
        session.lower,
        session.lower.callable_pipeline,
        SourceContextThread{ .source_context = session.source_context },
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn lookupProgramPatternMonotype(session: LowerSession, pattern_idx: CIR.Pattern.Idx) ?Pipeline.ResolvedMonotype {
    if (lookupCurrentCallableParamCallableValue(session, pattern_idx)) |callable_value| {
        return session.lower.callable_pipeline.getCallableValueRuntimeMonotype(callable_value);
    }
    if (session.lower.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref| {
        return session.lower.resolveLowerExprSourceMonotype(source_expr_ref) catch unreachable;
    }
    const SourceContextThread = struct {
        source_context: Pipeline.SourceContext,

        pub fn requireSourceContext(self: @This()) Pipeline.SourceContext {
            return self.source_context;
        }
    };
    return corecir.ContextMono.resolveTypeVarMonotypeResolved(
        session.lower,
        session.lower.callable_pipeline,
        SourceContextThread{ .source_context = session.source_context },
        session.lower.current_module_idx,
        ModuleEnv.varFrom(pattern_idx),
    ) catch unreachable;
}

fn lookupCurrentCallableParamCallableValue(
    session: LowerSession,
    pattern_idx: CIR.Pattern.Idx,
) ?Pipeline.CallableValue {
    const callable_inst_id = switch (session.source_context) {
        .callable_inst => |context_id| @as(Pipeline.CallableInstId, @enumFromInt(@intFromEnum(context_id))),
        .root_expr, .provenance_expr, .template_expr => return null,
    };

    const callable_inst = session.lower.callable_pipeline.getCallableInst(callable_inst_id);
    const template = session.lower.callable_pipeline.getCallableTemplate(callable_inst.template);
    const param_patterns = session.lower.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
    const param_index = for (param_patterns, 0..) |param_pattern_idx, idx| {
        if (param_pattern_idx == pattern_idx) break idx;
    } else return null;

    for (session.lower.callable_pipeline.getCallableParamSpecEntries(callable_inst.callable_param_specs)) |spec| {
        if (spec.param_index != param_index) continue;
        if (session.lower.callable_pipeline.getCallableParamProjectionEntries(spec.projections).len != 0) continue;
        return spec.callable_value;
    }

    return null;
}

fn resolveProgramPatternCallableValue(
    session: LowerSession,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!?Pipeline.CallableValue {
    if (session.lower.lookupBindingCallableValue(pattern_idx)) |callable_value| {
        return callable_value;
    }
    if (lookupCurrentCallableParamCallableValue(session, pattern_idx)) |callable_value| {
        return callable_value;
    }
    if (session.lower.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref| {
        return session.lower.resolveLowerExprSourceCallableValue(source_expr_ref);
    }
    return null;
}

fn lookupProgramExprCallableValue(session: LowerSession, expr_idx: CIR.Expr.Idx) ?Pipeline.CallableValue {
    return session.lower.callable_pipeline.getExprCallableValue(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn resolveExprRefCallableValue(
    self: *Self,
    expr_ref: Pipeline.ExprRef,
) Allocator.Error!?Pipeline.CallableValue {
    return self.resolveProjectedExprCallableValue(
        expr_ref.source_context,
        expr_ref.module_idx,
        expr_ref.expr_idx,
        self.callable_pipeline.getValueProjectionEntries(expr_ref.projections),
    );
}

fn resolveLowerExprSourceCallableValue(
    self: *Self,
    expr_ref: LowerExprSourceRef,
) Allocator.Error!?Pipeline.CallableValue {
    return self.resolveProjectedExprCallableValue(
        expr_ref.source_context,
        expr_ref.module_idx,
        expr_ref.expr_idx,
        expr_ref.projections,
    );
}

fn resolveLowerExprSourceMonotype(
    self: *Self,
    expr_ref: LowerExprSourceRef,
) Allocator.Error!?Pipeline.ResolvedMonotype {
    var source_resolved = if (try self.resolveLowerExprSourceCallableValue(expr_ref)) |callable_value|
        self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value)
    else blk: {
        const SourceContextThread = struct {
            source_context: Pipeline.SourceContext,

            pub fn requireSourceContext(thread: @This()) Pipeline.SourceContext {
                return thread.source_context;
            }
        };
        break :blk (try corecir.ContextMono.resolveExprMonotypeResolved(
            self,
            self.callable_pipeline,
            SourceContextThread{ .source_context = expr_ref.source_context },
            expr_ref.module_idx,
            expr_ref.expr_idx,
        )) orelse return null;
    };

    for (expr_ref.projections) |projection| {
        source_resolved = try corecir.ContextMono.projectResolvedMonotypeByValueProjection(
            self,
            self.callable_pipeline,
            source_resolved,
            projection,
        );
        if (source_resolved.isNone()) return null;
    }

    return source_resolved;
}

fn resolveProjectedExprCallableValue(
    self: *Self,
    source_context: Pipeline.SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: []const Pipeline.ValueProjection,
) Allocator.Error!?Pipeline.CallableValue {
    if (projections.len == 0) {
        return self.callable_pipeline.getExprCallableValue(source_context, module_idx, expr_idx);
    }

    if (self.callable_pipeline.getExprOriginExpr(source_context, module_idx, expr_idx)) |origin| {
        const origin_projections = self.callable_pipeline.getValueProjectionEntries(origin.projections);
        const combined = try self.allocator.alloc(Pipeline.ValueProjection, origin_projections.len + projections.len);
        defer self.allocator.free(combined);
        @memcpy(combined[0..origin_projections.len], origin_projections);
        @memcpy(combined[origin_projections.len..], projections);
        return self.resolveProjectedExprCallableValue(
            origin.source_context,
            origin.module_idx,
            origin.expr_idx,
            combined,
        );
    }

    const module_env = self.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const projection = projections[0];
    const rest = projections[1..];
    return switch (projection) {
        .field => |field_name| switch (expr) {
            .e_record => |record_expr| blk: {
                for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                    const field = module_env.store.getRecordField(field_idx);
                    if (!self.identsStructurallyEqual(field.name, field_name.ident)) continue;
                    break :blk try self.resolveProjectedExprCallableValue(
                        source_context,
                        module_idx,
                        field.value,
                        rest,
                    );
                }
                break :blk null;
            },
            else => null,
        },
        .tuple_elem => |elem_index| switch (expr) {
            .e_tuple => |tuple_expr| blk: {
                const elems = module_env.store.sliceExpr(tuple_expr.elems);
                if (builtin.mode == .Debug and elem_index >= elems.len) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: tuple projection elem_index {d} out of bounds for expr {d} in module {d}",
                        .{ elem_index, @intFromEnum(expr_idx), module_idx },
                    );
                }
                break :blk try self.resolveProjectedExprCallableValue(
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                );
            },
            else => null,
        },
        .tag_payload => |payload| switch (expr) {
            .e_tag => |tag_expr| blk: {
                if (!self.identsTagNameEquivalent(tag_expr.name, payload.tag_name.ident)) break :blk null;
                const args = module_env.store.sliceExpr(tag_expr.args);
                if (builtin.mode == .Debug and payload.payload_index >= args.len) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: tag payload projection index {d} out of bounds for expr {d} in module {d}",
                        .{ payload.payload_index, @intFromEnum(expr_idx), module_idx },
                    );
                }
                break :blk try self.resolveProjectedExprCallableValue(
                    source_context,
                    module_idx,
                    args[payload.payload_index],
                    rest,
                );
            },
            else => null,
        },
        .list_elem => |elem_index| switch (expr) {
            .e_list => |list_expr| blk: {
                const elems = module_env.store.sliceExpr(list_expr.elems);
                if (builtin.mode == .Debug and elem_index >= elems.len) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: list projection elem_index {d} out of bounds for expr {d} in module {d}",
                        .{ elem_index, @intFromEnum(expr_idx), module_idx },
                    );
                }
                break :blk try self.resolveProjectedExprCallableValue(
                    source_context,
                    module_idx,
                    elems[elem_index],
                    rest,
                );
            },
            else => null,
        },
    };
}

fn lookupProgramExprIntroCallableInst(session: LowerSession, expr_idx: CIR.Expr.Idx) ?Pipeline.CallableInstId {
    return session.lower.callable_pipeline.getExprIntroCallableInst(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn lookupProgramExprLookupResolution(
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.LookupResolution {
    return session.lower.callable_pipeline.getLookupResolution(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn lookupProgramExprCallSite(session: LowerSession, expr_idx: CIR.Expr.Idx) ?Pipeline.CallSite {
    return session.lower.callable_pipeline.getExprCallSite(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn lookupProgramExprDispatchTarget(
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.DispatchExprTarget {
    return session.lower.callable_pipeline.getDispatchExprTarget(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn lookupProgramExprDispatchIntrinsic(
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.DispatchIntrinsic {
    return session.lower.callable_pipeline.getDispatchExprIntrinsic(
        session.source_context,
        session.lower.current_module_idx,
        expr_idx,
    );
}

fn callableExprKind(expr: CIR.Expr) ?Pipeline.CallableTemplateKind {
    return switch (expr) {
        .e_lambda => .lambda,
        .e_closure => .closure,
        .e_hosted_lambda => .hosted_lambda,
        else => null,
    };
}

fn shouldOmitCallableBindingExpr(
    session: LowerSession,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) bool {
    if (lookupProgramExprMonotype(session, expr_idx) != null) return false;
    if (lookupProgramExprCallableValue(session, expr_idx) != null) return false;
    if (lookupProgramExprIntroCallableInst(session, expr_idx) != null) return false;

    return callableExprKind(module_env.store.getExpr(expr_idx)) != null;
}

fn lowerProgramCallableIntroInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
    comptime expr_label: []const u8,
) Allocator.Error!MIR.CFStmtId {
    if (lookupProgramExprCallableValue(session, expr_idx)) |callable_value| {
        return switch (callable_value) {
            .direct => |callable_inst_id| self.lowerResolvedCallableInstValueInto(
                callable_inst_id,
                target,
                next,
            ),
            .packed_fn => |packed_fn| self.lowerPackedCallableIntroInto(
                packed_fn,
                lookupProgramExprIntroCallableInst(session, expr_idx) orelse std.debug.panic(
                    "statement-only MIR invariant violated: {s} expr {d} specialized to packed callable semantics without explicit intro callable inst",
                    .{ expr_label, @intFromEnum(expr_idx) },
                ),
                target,
                next,
            ),
        };
    }

    if (lookupProgramExprIntroCallableInst(session, expr_idx)) |callable_inst_id| {
        return self.lowerResolvedCallableInstValueInto(
            callable_inst_id,
            target,
            next,
        );
    }

    std.debug.panic(
        "statement-only MIR invariant violated: {s} expr {d} lacked callable introduction specialization in context callable_inst={d} root_source_expr={d} expr_id={d} template_present={any}",
        .{
            expr_label,
            @intFromEnum(expr_idx),
            session.callableInstRawForDebug(),
            session.rootExprRawForDebug(),
            if (self.callable_pipeline.lambdamono.getExprId(
                session.source_context,
                self.current_module_idx,
                expr_idx,
            )) |expr_id|
                @intFromEnum(expr_id)
            else
                std.math.maxInt(u32),
            self.callable_pipeline.getExprTemplateId(
                session.source_context,
                self.current_module_idx,
                expr_idx,
            ),
        },
    );
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

fn exposedNameForNode(self: *const Self, module_idx: u32, node_idx: u16) ?[]const u8 {
    const module_env = self.all_module_envs[module_idx];
    var exposed_items = module_env.common.exposed_items;
    exposed_items.ensureSorted(module_env.gpa);
    for (exposed_items.items.entries.items) |entry| {
        if (entry.value != node_idx) continue;
        return module_env.getIdent(@bitCast(entry.key));
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

fn remapMonotypeNameBetweenModules(
    self: *Self,
    name: Monotype.Name,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Monotype.Name {
    if (from_module_idx == to_module_idx or name.module_idx != from_module_idx) return name;

    const text = name.text(self.all_module_envs);
    const target_env = self.all_module_envs[to_module_idx];
    const target_ident = target_env.common.findIdent(text) orelse
        try target_env.common.insertIdent(self.allocator, Ident.for_text(text));

    return .{
        .module_idx = to_module_idx,
        .ident = target_ident,
    };
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
                    .name = try self.remapMonotypeNameBetweenModules(
                        field.name,
                        from_module_idx,
                        to_module_idx,
                    ),
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
                    .name = try self.remapMonotypeNameBetweenModules(
                        tag.name,
                        from_module_idx,
                        to_module_idx,
                    ),
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
                    .name = try self.remapMonotypeNameBetweenModules(
                        field.name,
                        from_module_idx,
                        to_module_idx,
                    ),
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
                    .name = try self.remapMonotypeNameBetweenModules(
                        tag.name,
                        from_module_idx,
                        to_module_idx,
                    ),
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
    const session = self.lowerSession(.{ .root_expr = .{
        .module_idx = self.current_module_idx,
        .expr_idx = expr_idx,
    } });
    return self.lowerConstDefFromSourceExpr(session, expr_idx);
}

fn lowerNamedConstDefFromSourceExpr(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    symbol: MIR.Symbol,
) Allocator.Error!MIR.ConstDefId {
    const lowered = try self.lowerConstDefBodyFromSourceExpr(session, expr_idx);
    const id = try self.store.registerConstDef(self.allocator, .{
        .symbol = symbol,
        .body = lowered.body,
        .monotype = lowered.monotype,
        .source_region = lowered.source_region,
    });
    if (builtin.mode == .Debug) {
        try DebugVerifyMir.verifyConst(self.allocator, self.store, id);
    }
    return id;
}

fn lowerConstDefBodyFromSourceExpr(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!struct {
    body: MIR.CFStmtId,
    monotype: Monotype.Idx,
    source_region: Region,
} {
    const module_env = self.all_module_envs[self.current_module_idx];
    const region = module_env.store.getExprRegion(expr_idx);
    const monotype = try self.sourceExprResultMonotype(session, expr_idx);
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;
    const result_local = try self.freshSyntheticLocal(monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    const body = try self.lowerCirExprInto(session, expr_idx, result_local, ret_stmt);
    return .{
        .body = body,
        .monotype = monotype,
        .source_region = region,
    };
}

fn freshSyntheticLocal(self: *Self, monotype: Monotype.Idx, reassignable: bool) Allocator.Error!MIR.LocalId {
    if (builtin.mode == .Debug and self.store.monotype_store.getMonotype(monotype) == .func) {
        std.debug.panic(
            "statement-only MIR invariant violated: executable local cannot be created with source function monotype {d}",
            .{@intFromEnum(monotype)},
        );
    }
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

fn sourceExprResultMonotype(self: *Self, session: LowerSession, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    return self.resolveMonotype(session, expr_idx);
}

fn lowerConstDefFromSourceExpr(self: *Self, session: LowerSession, expr_idx: CIR.Expr.Idx) Allocator.Error!MIR.ConstDefId {
    const symbol = try self.internSymbol(
        self.current_module_idx,
        self.makeSyntheticIdent(Ident.Idx.NONE),
    );
    return self.lowerNamedConstDefFromSourceExpr(session, expr_idx, symbol);
}

fn lowerStringConcatInto(
    self: *Self,
    session: LowerSession,
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
    if (segments.len == 1) return self.lowerCirExprInto(session, segments[0], target, next);

    const monotype = self.store.getLocal(target).monotype;
    const lhs_local = try self.freshSyntheticLocal(monotype, false);
    const rhs_local = try self.freshSyntheticLocal(monotype, false);
    const concat_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = target,
        .op = .str_concat,
        .args = try self.store.addLocalSpan(self.allocator, &.{ lhs_local, rhs_local }),
        .next = next,
    } });
    const rhs_entry = try self.lowerCirExprInto(session, segments[segments.len - 1], rhs_local, concat_stmt);
    return self.lowerStringConcatInto(session, segments[0 .. segments.len - 1], lhs_local, rhs_entry);
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

fn lowerU64LiteralInto(
    self: *Self,
    target: MIR.LocalId,
    value: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = target,
        .literal = .{ .int = .{
            .bytes = @bitCast(@as(u128, value)),
            .kind = .u128,
        } },
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

fn lowerUnitLocalInto(
    self: *Self,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
        .target = target,
        .fields = MIR.LocalSpan.empty(),
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
    const result_callable = switch (op) {
        .local => |source| self.resolveLocalCallableResolution(source),
        .nominal => |nominal| self.resolveLocalCallableResolution(nominal.backing),
        .field, .tag_payload, .discriminant => null,
    };
    return self.lowerRefIntoWithCallable(target, op, result_callable, next);
}

fn lowerRefIntoWithCallable(
    self: *Self,
    target: MIR.LocalId,
    op: MIR.RefOp,
    result_callable: ?CallableResolution,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.store.addCFStmt(self.allocator, .{ .assign_ref = .{
        .target = target,
        .op = op,
        .result_callable = result_callable,
        .next = next,
    } });
}

fn lowerLocalAliasInto(
    self: *Self,
    target: MIR.LocalId,
    source: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (builtin.mode == .Debug) {
        if (self.current_lambda_entry_bound_locals) |entry_bound_locals| {
            if (entry_bound_locals.contains(target)) {
                std.debug.panic(
                    "statement-only MIR invariant violated: body lowering tried to alias into entry-bound local {d} from source {d} (source_def={s}, scope={d})",
                    .{
                        @intFromEnum(target),
                        @intFromEnum(source),
                        if (self.store.getLocalDefOpt(source)) |source_def| @tagName(source_def) else "none",
                        self.current_binding_scope,
                    },
                );
            }
        }
    }
    try self.requireLocalMonotype(target, self.store.getLocal(source).monotype, "lowerLocalAliasInto");
    return self.lowerRefInto(target, .{ .local = source }, next);
}

fn callableResolutionsEqual(
    left: CallableResolution,
    right: CallableResolution,
) bool {
    return left.lambda == right.lambda and
        left.captures_local == right.captures_local;
}

fn resolveLocalCallableResolution(
    self: *Self,
    local_id: MIR.LocalId,
) ?CallableResolution {
    return self.store.resolveLocalCallable(local_id);
}

fn mergeJoinParamCallableResolution(
    self: *Self,
    dest_local: MIR.LocalId,
    source_local: MIR.LocalId,
) Allocator.Error!void {
    return self.mergeJoinParamCallableResolutionValue(
        dest_local,
        self.resolveLocalCallableResolution(source_local),
    );
}

fn mergeJoinParamCallableResolutionValue(
    self: *Self,
    dest_local: MIR.LocalId,
    source_resolution: ?CallableResolution,
) Allocator.Error!void {
    const key = @intFromEnum(dest_local);

    if (self.pending_join_param_callables.getPtr(key)) |existing| {
        if (existing.*) |existing_resolution| {
            if (source_resolution) |resolved| {
                if (!callableResolutionsEqual(existing_resolution, resolved)) {
                    existing.* = null;
                }
            } else {
                existing.* = null;
            }
        } else {
            return;
        }
        return;
    }

    try self.pending_join_param_callables.put(key, source_resolution);
}

fn bindCallableFactsForBindingLocal(
    self: *Self,
    session: LowerSession,
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
    source_expr_ref: ?LowerExprSourceRef,
) Allocator.Error!void {
    const binding_key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
    if (source_expr_ref) |source| {
        try self.bindBindingSourceExprRefInCurrentScope(pattern_idx, source);
    }
    const callable_value = (try resolveProgramPatternCallableValue(session, pattern_idx)) orelse {
        if (self.binding_records.getPtr(binding_key)) |binding| {
            if (!binding.shadowed) binding.callable_value = null;
        }
        if (comptime trace.enabled) {
            trace.log(
                "bindCallableFactsForBindingLocal binding={d} local={d} callable_value=none monotype={s}",
                .{
                    @intFromEnum(pattern_idx),
                    @intFromEnum(local),
                    @tagName(self.store.monotype_store.getMonotype(self.store.getLocal(local).monotype)),
                },
            );
        }
        return;
    };
    if (self.binding_records.getPtr(binding_key)) |binding| {
        if (binding.shadowed) unreachable;
        binding.explicit_monotype = self.store.getLocal(local).monotype;
        binding.callable_value = callable_value;
    } else {
        try self.binding_records.put(binding_key, .{
            .local = local,
            .explicit_monotype = self.store.getLocal(local).monotype,
            .callable_value = callable_value,
        });
    }
    if (comptime trace.enabled) {
        trace.log(
            "bindCallableFactsForBindingLocal binding={d} local={d} callable_value={s} monotype={s}",
            .{
                @intFromEnum(pattern_idx),
                @intFromEnum(local),
                @tagName(callable_value),
                @tagName(self.store.monotype_store.getMonotype(self.store.getLocal(local).monotype)),
            },
        );
    }
}

fn buildCallableParamResolutions(
    self: *Self,
    session: LowerSession,
    param_patterns: []const CIR.Pattern.Idx,
    param_locals: []const MIR.LocalId,
) Allocator.Error!MIR.CallableResolutionSpan {
    if (param_patterns.len != param_locals.len) {
        std.debug.panic(
            "statement-only MIR invariant violated: lambda param pattern arity {d} != param local arity {d}",
            .{ param_patterns.len, param_locals.len },
        );
    }

    const callable_resolutions = try self.allocator.alloc(?CallableResolution, param_patterns.len);
    defer self.allocator.free(callable_resolutions);

    for (param_patterns, param_locals) |pattern_idx, param_local| {
        try self.bindCallableFactsForBindingLocal(session, pattern_idx, param_local, null);
    }

    var any_callable = false;
    for (param_patterns, param_locals, 0..) |pattern_idx, param_local, i| {
        _ = param_local;
        callable_resolutions[i] = if (try resolveProgramPatternCallableValue(session, pattern_idx)) |callable_value| blk: {
            const resolved = try self.callableResolutionFromValue(callable_value);
            if (resolved != null) any_callable = true;
            break :blk resolved;
        } else null;
    }

    if (!any_callable) return .empty();
    return self.store.addCallableResolutionSpan(self.allocator, callable_resolutions);
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
    const first_prefix_value_local = try self.freshSyntheticLocal(str_mono, false);
    const non_first_prefix_value_local = try self.freshSyntheticLocal(str_mono, false);
    const is_first_local = try self.freshSyntheticLocal(bool_mono, false);
    const cond_local = try self.freshSyntheticLocal(bool_mono, false);

    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();
    const prefix_join_id = self.freshJoinPointId();
    const loop_head_params = try self.store.addLocalSpan(
        self.allocator,
        &.{
            accum_local,
            index_local,
        },
    );
    const initial_head_args = try self.store.addLocalSpan(
        self.allocator,
        &.{
            empty_local,
            zero_local,
        },
    );

    const exit_value_local = try self.freshSyntheticLocal(str_mono, false);
    const exit_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_exit_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{exit_value_local}),
    } });
    const false_body = try self.lowerConcatLocalsInto(
        &.{ open_local, accum_local, close_local },
        exit_value_local,
        exit_jump,
    );

    const loop_back = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.store.addLocalSpan(
            self.allocator,
            &.{
                next_accum_local,
                next_index_local,
            },
        ),
    } });
    const next_index_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = next_index_local,
        .op = .num_plus,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, one_local }),
        .next = loop_back,
    } });
    const next_accum_stmt = try self.lowerConcatLocalsInto(&.{ accum_local, prefix_local, elem_str_local }, next_accum_local, next_index_stmt);
    const non_first_prefix_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = prefix_join_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{non_first_prefix_value_local}),
    } });
    const first_prefix_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = prefix_join_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{first_prefix_value_local}),
    } });
    const non_first_prefix = try self.lowerLocalAliasInto(non_first_prefix_value_local, comma_local, non_first_prefix_jump);
    const first_prefix = try self.lowerLocalAliasInto(first_prefix_value_local, empty_local, first_prefix_jump);
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
    const prefix_join = try self.addJoinStmt(
        prefix_join_id,
        try self.store.addLocalSpan(self.allocator, &.{prefix_local}),
        next_accum_stmt,
        prefix_switch,
    );
    const is_first_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = is_first_local,
        .op = .num_is_eq,
        .args = try self.store.addLocalSpan(self.allocator, &.{ index_local, zero_local }),
        .next = prefix_join,
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
        .args = initial_head_args,
    } });
    const head_join = try self.addJoinStmt(
        loop_head_id,
        loop_head_params,
        cond_stmt,
        initial_jump,
    );

    const init_comma = try self.lowerStrLiteralInto(comma_local, ", ", head_join);
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

    return self.addJoinStmt(
        loop_exit_id,
        try self.store.addLocalSpan(self.allocator, &.{target}),
        next,
        init_len,
    );
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
    const join_id = self.freshJoinPointId();
    const lowered_branches = try self.allocator.alloc(MIR.SwitchBranch, tags.len);
    defer self.allocator.free(lowered_branches);

    for (tags, 0..) |tag, i| {
        const tag_text = tag.name.text(self.all_module_envs);
        const payload_monos = self.store.monotype_store.getIdxSpan(tag.payloads);
        const branch_value = try self.freshSyntheticLocal(str_mono, false);
        const branch_jump = try self.store.reserveCFStmt(self.allocator);
        const branch_body = if (payload_monos.len == 0) blk: {
            break :blk try self.lowerStrLiteralInto(branch_value, tag_text, branch_jump);
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
                branch_value,
                branch_jump,
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
                    .tag_discriminant = @intCast(i),
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

        try self.store.finalizeCFStmt(branch_jump, .{ .jump = .{
            .id = join_id,
            .args = try self.store.addLocalSpan(self.allocator, &.{branch_value}),
        } });

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
    const switch_entry = try self.lowerRefInto(discrim_local, .{ .discriminant = .{ .source = source } }, switch_stmt);
    return self.addJoinStmt(
        join_id,
        try self.store.addLocalSpan(self.allocator, &.{target}),
        next,
        switch_entry,
    );
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

    const target_mono = self.store.getLocal(target).monotype;
    const join_id = self.freshJoinPointId();

    const true_value = try self.freshSyntheticLocal(target_mono, false);
    const true_jump = try self.store.reserveCFStmt(self.allocator);
    const true_body = try self.lowerStrLiteralInto(true_value, "True", true_jump);
    try self.store.finalizeCFStmt(true_jump, .{ .jump = .{
        .id = join_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{true_value}),
    } });

    const false_value = try self.freshSyntheticLocal(target_mono, false);
    const false_jump = try self.store.reserveCFStmt(self.allocator);
    const false_body = try self.lowerStrLiteralInto(false_value, "False", false_jump);
    try self.store.finalizeCFStmt(false_jump, .{ .jump = .{
        .id = join_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{false_value}),
    } });

    const switch_entry = try self.lowerSwitchOnDiscriminant(
        source,
        bool_mono,
        tag_names.true_name,
        true_body,
        false_body,
    );
    return self.addJoinStmt(
        join_id,
        try self.store.addLocalSpan(self.allocator, &.{target}),
        next,
        switch_entry,
    );
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
        .func => std.debug.panic(
            "statement-only MIR invariant violated: str_inspect received executable local {d} with source function monotype {d}",
            .{ @intFromEnum(source), @intFromEnum(source_mono) },
        ),
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

fn lowerLookupLocalInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const lookup_resolution = lookupProgramExprLookupResolution(session, expr_idx);
    const usable_source_local = blk: {
        if (self.lookupUsableBindingLocalAtScope(self.current_binding_scope, lookup.pattern_idx)) |local| {
            break :blk local;
        }

        if (self.current_lambda_scope_boundary != null) {
            break :blk self.lookupExistingBindingLocalWithinCurrentLambda(lookup.pattern_idx);
        }

        break :blk self.lookupUsableBindingLocalInAncestorScopes(
            self.current_module_idx,
            self.current_binding_scope,
            lookup.pattern_idx,
        );
    };

    if (usable_source_local) |source| {
        return self.lowerLocalAliasInto(target, source, next);
    }

    if (lookup_resolution) |resolution| switch (resolution) {
        .def => |target_def| {
            const def = self.all_module_envs[target_def.module_idx].store.getDef(target_def.def_idx);
            const symbol = try self.lookupSymbolForPattern(target_def.module_idx, def.pattern);
            return self.materializeTopLevelDefInto(
                session,
                target_def.module_idx,
                target_def.def_idx,
                symbol,
                target,
                next,
                if (lookupProgramExprCallableValue(session, expr_idx)) |callable_value|
                    .{ .callable = callable_value }
                else
                    .non_callable,
            );
        },
        .expr => |source_expr_ref| {
            if (source_expr_ref.module_idx == self.current_module_idx and source_expr_ref.expr_idx == expr_idx) {
                std.debug.panic(
                    "statement-only MIR invariant violated: local lookup expr {d} resolved to itself as a binding source",
                    .{@intFromEnum(expr_idx)},
                );
            }
            return self.lowerCapturedSourceExprInto(session, source_expr_ref, target, next);
        },
    };

    {
        const pattern = self.all_module_envs[self.current_module_idx].store.getPattern(lookup.pattern_idx);
        const ident = switch (pattern) {
            .assign => |assign| self.all_module_envs[self.current_module_idx].getIdent(assign.ident),
            .as => |as_pattern| self.all_module_envs[self.current_module_idx].getIdent(as_pattern.ident),
            else => "<non-binding>",
        };
        const has_root_local = self.lookupExistingBindingLocalInScope(
            self.current_module_idx,
            0,
            lookup.pattern_idx,
        ) != null;
        const active_callable_def = session.activeCallableDef();
        const active_callable_kind = switch (session.source_context) {
            .callable_inst => |context_id| callableRuntimeValueKindText(
                self.callable_pipeline.getCallableInst(@enumFromInt(@intFromEnum(context_id))).runtime_value,
            ),
            .root_expr, .provenance_expr, .template_expr => "none",
        };
        std.debug.panic(
            "statement-only MIR invariant violated: local lookup pattern {d} ({s}:{s}) in scope={d} had no in-scope local or specialized lookup resolution, root_local={any}, callable_inst={d}, callable_kind={s}, callable_expr={d}",
            .{
                @intFromEnum(lookup.pattern_idx),
                @tagName(pattern),
                ident,
                self.current_binding_scope,
                has_root_local,
                session.callableInstRawForDebug(),
                active_callable_kind,
                if (active_callable_def) |callable_def| @intFromEnum(callable_def.runtime_expr.expr_idx) else std.math.maxInt(u32),
            },
        );
    }
    unreachable;
}

fn planClosureEntryCaptureLocals(
    self: *Self,
    closure_expr_idx: CIR.Expr.Idx,
    capture_fields: []const Pipeline.CaptureField,
    entry_scope: u64,
) Allocator.Error!PlannedCaptureLocals {
    var planned = PlannedCaptureLocals.init();
    errdefer planned.deinit(self.allocator);

    var seen_capture_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
    defer seen_capture_patterns.deinit();

    const saved_scope = self.current_binding_scope;
    self.current_binding_scope = entry_scope;
    defer self.current_binding_scope = saved_scope;

    for (capture_fields) |capture_field| {
        if (!(try recordSeenCapturePattern(&seen_capture_patterns, capture_field.pattern_idx))) {
            std.debug.panic(
                "statement-only MIR invariant violated: closure expr {d} recorded duplicate capture pattern {d}",
                .{ @intFromEnum(closure_expr_idx), @intFromEnum(capture_field.pattern_idx) },
            );
        }

        const capture_monotype = switch (capture_field.storage) {
            .runtime_field => |runtime_field| try self.executableMonotypeForCallableValue(
                try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    runtime_field.field_monotype.idx,
                    runtime_field.field_monotype.module_idx,
                    self.current_module_idx,
                ),
                capture_field.callable_value,
            ),
            .callable_only, .recursive_member => blk: {
                const callable_value = capture_field.callable_value orelse std.debug.panic(
                    "statement-only MIR invariant violated: callable capture pattern {d} had no executable callable value when planning entry locals",
                    .{@intFromEnum(capture_field.pattern_idx)},
                );
                const runtime_monotype = self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value);
                break :blk try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    runtime_monotype.idx,
                    runtime_monotype.module_idx,
                    self.current_module_idx,
                );
            },
        };
        const capture_local = try self.patternToLocalWithMonotype(capture_field.pattern_idx, capture_monotype);
        if (self.current_lambda_entry_bound_locals) |entry_bound_locals| {
            try entry_bound_locals.add(capture_local);
        }

        if (capture_field.callable_value) |callable_value| {
            try self.bindBindingCallableValueInCurrentScope(capture_field.pattern_idx, callable_value);
        }

        switch (capture_field.storage) {
            .runtime_field => try planned.runtime.append(self.allocator, .{
                .pattern_idx = capture_field.pattern_idx,
                .local = capture_local,
                .local_monotype = capture_monotype,
                .callable_value = capture_field.callable_value,
                .storage = capture_field.storage,
            }),
            .callable_only => {
                const callable_inst_id = switch (capture_field.callable_value orelse std.debug.panic(
                    "statement-only MIR invariant violated: callable-only capture pattern {d} had no direct callable value",
                    .{@intFromEnum(capture_field.pattern_idx)},
                )) {
                    .direct => |direct_callable_inst_id| direct_callable_inst_id,
                    .packed_fn => unreachable,
                };
                try planned.callable_only.append(self.allocator, .{
                    .pattern_idx = capture_field.pattern_idx,
                    .local = capture_local,
                    .callable_inst_id = callable_inst_id,
                });
            },
            .recursive_member => {
                const callable_inst_id = switch (capture_field.callable_value orelse std.debug.panic(
                    "statement-only MIR invariant violated: recursive capture pattern {d} had no direct callable value",
                    .{@intFromEnum(capture_field.pattern_idx)},
                )) {
                    .direct => |direct_callable_inst_id| direct_callable_inst_id,
                    .packed_fn => unreachable,
                };
                try planned.recursive.append(self.allocator, .{
                    .pattern_idx = capture_field.pattern_idx,
                    .local = capture_local,
                    .callable_inst_id = callable_inst_id,
                });
            },
        }
    }

    return planned;
}

fn lowerLookupExternalInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    module_env: *const ModuleEnv,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    _ = module_env;
    _ = lookup;
    const target_def = switch (lookupProgramExprLookupResolution(session, expr_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: external lookup expr {d} had no specialized lookup resolution",
        .{@intFromEnum(expr_idx)},
    )) {
        .def => |target_def| target_def,
        .expr => |source_expr_ref| std.debug.panic(
            "statement-only MIR invariant violated: external lookup expr {d} unexpectedly specialized to source expr {d} in module {d}",
            .{
                @intFromEnum(expr_idx),
                @intFromEnum(source_expr_ref.expr_idx),
                source_expr_ref.module_idx,
            },
        ),
    };
    const symbol = try self.internExternalDefSymbol(target_def.module_idx, @intCast(@intFromEnum(target_def.def_idx)));
    return self.materializeTopLevelDefInto(
        session,
        target_def.module_idx,
        target_def.def_idx,
        symbol,
        target,
        next,
        if (lookupProgramExprCallableValue(session, expr_idx)) |callable_value|
            .{ .callable = callable_value }
        else
            .non_callable,
    );
}

fn lowerLookupRequiredInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    module_env: *const ModuleEnv,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    _ = module_env;
    _ = lookup;
    const target_def = switch (lookupProgramExprLookupResolution(session, expr_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: required lookup expr {d} had no specialized lookup resolution",
        .{@intFromEnum(expr_idx)},
    )) {
        .def => |target_def| target_def,
        .expr => |source_expr_ref| std.debug.panic(
            "statement-only MIR invariant violated: required lookup expr {d} unexpectedly specialized to source expr {d} in module {d}",
            .{
                @intFromEnum(expr_idx),
                @intFromEnum(source_expr_ref.expr_idx),
                source_expr_ref.module_idx,
            },
        ),
    };
    const def = self.all_module_envs[target_def.module_idx].store.getDef(target_def.def_idx);
    const symbol = try self.lookupSymbolForPattern(target_def.module_idx, def.pattern);
    return self.materializeTopLevelDefInto(
        session,
        target_def.module_idx,
        target_def.def_idx,
        symbol,
        target,
        next,
        if (lookupProgramExprCallableValue(session, expr_idx)) |callable_value|
            .{ .callable = callable_value }
        else
            .non_callable,
    );
}

fn materializeTopLevelDefInto(
    self: *Self,
    _: LowerSession,
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    symbol: MIR.Symbol,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
    callable_semantics: TopLevelDefCallableSemantics,
) Allocator.Error!MIR.CFStmtId {
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_scratches_module_idx = self.mono_scratches.module_idx;

    const target_env = self.all_module_envs[module_idx];
    const def = target_env.store.getDef(def_idx);
    const def_session = self.lowerSession(.{ .root_expr = .{
        .module_idx = module_idx,
        .expr_idx = def.expr,
    } });
    const normalized_callable_semantics: TopLevelDefCallableSemantics = switch (callable_semantics) {
        .callable => callable_semantics,
        .non_callable => if (lookupProgramExprCallableValue(def_session, def.expr)) |resolved_callable_value|
            .{ .callable = resolved_callable_value }
        else if (lookupProgramExprIntroCallableInst(def_session, def.expr)) |intro_callable_inst|
            .{ .callable = .{ .direct = intro_callable_inst } }
        else
            .non_callable,
    };

    if (module_idx != self.current_module_idx) {
        self.current_module_idx = module_idx;
        self.types_store = &target_env.types;
        self.mono_scratches.ident_store = target_env.getIdentStoreConst();
        self.mono_scratches.module_env = target_env;
        self.mono_scratches.module_idx = module_idx;
    }
    defer {
        if (module_idx != saved_module_idx) {
            self.current_module_idx = saved_module_idx;
            self.types_store = saved_types_store;
            self.mono_scratches.ident_store = saved_ident_store;
            self.mono_scratches.module_env = saved_module_env;
            self.mono_scratches.module_idx = saved_scratches_module_idx;
        }
    }

    switch (normalized_callable_semantics) {
        .callable => |resolved_callable_value| switch (resolved_callable_value) {
            .direct => |callable_inst_id| {
                const runtime_monotype = self.callable_pipeline.getCallableInstRuntimeMonotype(callable_inst_id);
                try self.requireLocalMonotype(target, try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    runtime_monotype.idx,
                    runtime_monotype.module_idx,
                    saved_module_idx,
                ), "materializeTopLevelDefInto.direct");
            },
            .packed_fn => |packed_fn| {
                const runtime_monotype = self.callable_pipeline.getCallableValueRuntimeMonotype(.{ .packed_fn = packed_fn });
                try self.requireLocalMonotype(target, try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    runtime_monotype.idx,
                    runtime_monotype.module_idx,
                    saved_module_idx,
                ), "materializeTopLevelDefInto.packed_fn");
            },
        },
        .non_callable => try self.requireTargetLocalMonotypeFromExprIntoModule(
            def_session,
            def.expr,
            module_idx,
            saved_module_idx,
            target,
        ),
    }

    switch (normalized_callable_semantics) {
        .callable => |resolved_callable_value| switch (resolved_callable_value) {
            .direct => |resolved_callable_inst| return self.lowerResolvedCallableInstValueInto(resolved_callable_inst, target, next),
            .packed_fn => |packed_fn| return self.lowerExprAsPackedCallableInto(def_session, def.expr, packed_fn, target, next),
        },
        .non_callable => {},
    }

    if (self.store.getConstDefForSymbol(symbol) == null) {
        _ = try self.lowerNamedConstDefFromSourceExpr(def_session, def.expr, symbol);
    }
    return self.store.addCFStmt(self.allocator, .{ .assign_symbol = .{
        .target = target,
        .symbol = symbol,
        .next = next,
    } });
}

fn lowerLambdaInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    arg_patterns: []const CIR.Pattern.Idx,
    body_expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    ret_monotype: Monotype.Idx,
    reserved_lambda: ?MIR.LambdaId,
) Allocator.Error!MIR.LambdaId {
    const cache_key = makeCallableCacheKey(session, expr_idx, fn_monotype);
    if (try lookupLoweredCallableLambdaCache(session, expr_idx, fn_monotype)) |cached| {
        if (reserved_lambda) |reserved| {
            if (cached == reserved) return cached;
        } else {
            return cached;
        }
    }
    if (reserved_lambda) |reserved| {
        try self.lowered_callable_lambdas.put(cache_key, reserved);
    }

    const saved_scope = self.current_binding_scope;
    const lambda_entry_scope = self.freshStatementBindingScope();
    self.current_binding_scope = lambda_entry_scope;
    defer self.current_binding_scope = saved_scope;
    const saved_lambda_scope_boundary = self.current_lambda_scope_boundary;
    self.current_lambda_scope_boundary = lambda_entry_scope;
    defer self.current_lambda_scope_boundary = saved_lambda_scope_boundary;
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;
    const saved_lambda_entry_bound_locals = self.current_lambda_entry_bound_locals;
    var lambda_entry_bound_locals = LambdaEntryBindingState.init(self.allocator);
    defer lambda_entry_bound_locals.deinit();
    self.current_lambda_entry_bound_locals = &lambda_entry_bound_locals;
    defer self.current_lambda_entry_bound_locals = saved_lambda_entry_bound_locals;

    const param_patterns = arg_patterns;
    const params = try self.lowerLambdaParamLocals(session, module_env, param_patterns, fn_monotype);
    var stable_param_locals = try std.ArrayList(MIR.LocalId).initCapacity(self.allocator, self.store.getLocalSpan(params).len);
    defer stable_param_locals.deinit(self.allocator);
    stable_param_locals.appendSliceAssumeCapacity(self.store.getLocalSpan(params));
    const param_locals = stable_param_locals.items;
    for (param_patterns) |pattern_idx| {
        try self.predeclareBindingLocals(session, module_env, pattern_idx);
    }

    const param_callables = try self.buildCallableParamResolutions(session, param_patterns, param_locals);
    const active_lambda = if (reserved_lambda) |reserved|
        reserved
    else blk: {
        break :blk try self.store.reserveLambda(self.allocator);
    };
    try self.store.installReservedLambdaPrototype(active_lambda, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .param_callables = param_callables,
        .body = @enumFromInt(0),
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = null,
        .recursion = .not_recursive,
        .hosted = null,
    });
    try self.lowered_callable_lambdas.put(cache_key, active_lambda);

    if (comptime trace.enabled) {
        for (param_locals, 0..) |param_local, i| {
            trace.log(
                "lowerLambdaInto expr={d} lambda_reserved={any} param_index={d} pattern={d} local={d} monotype={s} callable_resolution={any}",
                .{
                    @intFromEnum(expr_idx),
                    reserved_lambda,
                    i,
                    @intFromEnum(param_patterns[i]),
                    @intFromEnum(param_local),
                    @tagName(self.store.monotype_store.getMonotype(self.store.getLocal(param_local).monotype)),
                    self.store.resolveLocalCallable(param_local),
                },
            );
        }
    }

    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    const lambda_body_scope = self.freshChildBindingScope(lambda_entry_scope);
    self.current_binding_scope = lambda_body_scope;
    var body = try self.lowerCirExprInto(session, body_expr_idx, result_local, ret_stmt);
    self.current_binding_scope = lambda_entry_scope;

    const captures_param_local: ?MIR.LocalId = null;

    var i = param_patterns.len;
    while (i > 0) {
        i -= 1;
        if (!self.paramPatternNeedsBindingStmt(module_env, param_patterns[i], param_locals[i])) continue;
        body = try self.lowerBindingLocalInto(session, module_env, param_patterns[i], param_locals[i], self.lookupBindingSourceExprRef(param_patterns[i]), body);
    }

    const lowered_lambda = MIR.Lambda{
        .fn_monotype = fn_monotype,
        .params = params,
        .param_callables = param_callables,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = captures_param_local,
        .recursion = .not_recursive,
        .hosted = null,
    };

    try self.store.finalizeLambda(active_lambda, lowered_lambda);
    if (builtin.mode == .Debug) {
        try DebugVerifyMir.verifyLambda(self.allocator, self.store, active_lambda);
    }
    return active_lambda;
}

fn lowerLambdaParamLocals(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    cir_ids: []const CIR.Pattern.Idx,
    fn_monotype: Monotype.Idx,
) Allocator.Error!MIR.LocalSpan {
    if (cir_ids.len == 0) return MIR.LocalSpan.empty();

    const fn_mono = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => std.debug.panic(
            "statement-only MIR invariant violated: lambda params expected function monotype {d}, found {s}",
            .{ @intFromEnum(fn_monotype), @tagName(self.store.monotype_store.getMonotype(fn_monotype)) },
        ),
    };
    const param_monotypes = self.store.monotype_store.getIdxSpan(fn_mono.args);
    if (builtin.mode == .Debug and cir_ids.len != param_monotypes.len) {
        std.debug.panic(
            "statement-only MIR invariant violated: lambda param arity mismatch (patterns={d}, monotypes={d})",
            .{ cir_ids.len, param_monotypes.len },
        );
    }

    const scratch_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(scratch_top);

    for (cir_ids, param_monotypes) |pattern_idx, param_monotype| {
        const monotype = try self.resolvePatternExecutableMonotype(session, pattern_idx, param_monotype);
        const param_reassignable = switch (module_env.store.getPattern(pattern_idx)) {
            .assign => |assign_pattern| assign_pattern.ident.attributes.reassignable,
            .as => |as_pattern| as_pattern.ident.attributes.reassignable,
            else => false,
        };
        const param_local = try self.freshSyntheticLocal(monotype, param_reassignable);
        try self.predeclareBindingLocalsFromMonotype(module_env, pattern_idx, monotype, param_local);
        try self.scratch_local_ids.append(param_local);
    }

    return self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(scratch_top));
}

fn appendCaptureMaterialization(
    self: *Self,
    capture_field: Pipeline.CaptureField,
) Allocator.Error!MIR.LocalId {
    const capture_local = switch (capture_field.source) {
        .specialized_param => |source| blk: {
            const callable_inst = self.callable_pipeline.getCallableInst(source.callable_inst);
            const template = self.callable_pipeline.getCallableTemplate(callable_inst.template);
            const param_patterns = self.all_module_envs[template.module_idx].store.slicePatterns(template.arg_patterns);
            if (source.param_index >= param_patterns.len) {
                std.debug.panic(
                    "statement-only MIR invariant violated: specialized capture param index {d} out of bounds for callable inst {d}",
                    .{ source.param_index, @intFromEnum(source.callable_inst) },
                );
            }
            const pattern_idx = param_patterns[source.param_index];
            if (template.module_idx != self.current_module_idx) {
                std.debug.panic(
                    "statement-only MIR invariant violated: specialized capture param pattern {d} pointed at module {d} while lowering module {d}",
                    .{
                        @intFromEnum(pattern_idx),
                        template.module_idx,
                        self.current_module_idx,
                    },
                );
            }
            if (self.lookupExistingBindingLocalWithinCurrentLambda(pattern_idx)) |existing| break :blk existing;
            if (self.lookupUsableBindingLocalInAncestorScopes(
                self.current_module_idx,
                self.current_binding_scope,
                pattern_idx,
            )) |existing| break :blk existing;
            std.debug.panic(
                "statement-only MIR invariant violated: specialized capture param {d} had no reusable local in scope",
                .{source.param_index},
            );
        },
        .lexical_binding => |source| blk: {
            if (source.module_idx != self.current_module_idx) {
                std.debug.panic(
                    "statement-only MIR invariant violated: lexical capture pattern {d} pointed at module {d} while lowering module {d}",
                    .{
                        @intFromEnum(source.pattern_idx),
                        source.module_idx,
                        self.current_module_idx,
                    },
                );
            }
            if (self.lookupExistingBindingLocalWithinCurrentLambda(source.pattern_idx)) |existing| break :blk existing;
            if (self.lookupUsableBindingLocalInAncestorScopes(
                self.current_module_idx,
                self.current_binding_scope,
                source.pattern_idx,
            )) |existing| break :blk existing;
            std.debug.panic(
                "statement-only MIR invariant violated: lexical capture pattern {d} had no reusable local in scope",
                .{
                    @intFromEnum(source.pattern_idx),
                },
            );
        },
        .bound_expr => |bound_expr| blk: {
            if (bound_expr.expr_ref.module_idx != self.current_module_idx) {
                std.debug.panic(
                    "statement-only MIR invariant violated: bound capture pattern {d} pointed at module {d} while lowering module {d}",
                    .{
                        @intFromEnum(capture_field.pattern_idx),
                        bound_expr.expr_ref.module_idx,
                        self.current_module_idx,
                    },
                );
            }
            if (self.lookupExistingBindingLocalWithinCurrentLambda(capture_field.pattern_idx)) |existing| {
                break :blk existing;
            }
            if (self.lookupUsableBindingLocalInAncestorScopes(
                self.current_module_idx,
                self.current_binding_scope,
                capture_field.pattern_idx,
            )) |existing| {
                break :blk existing;
            }
            std.debug.panic(
                "statement-only MIR invariant violated: bound capture pattern {d} had no reusable local in scope",
                .{@intFromEnum(capture_field.pattern_idx)},
            );
        },
    };
    return capture_local;
}

fn reserveResolvedCallableInstLambdaSkeleton(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
    fn_monotype: Monotype.Idx,
    ret_monotype: Monotype.Idx,
    source_region: Region,
) Allocator.Error!MIR.LambdaId {
    const callable_inst_key = @intFromEnum(callable_inst_id);
    if (self.lowered_callable_insts.get(callable_inst_key)) |existing| return existing;
    if (self.reserved_callable_insts.get(callable_inst_key)) |existing| return existing;

    const reserved = try self.store.reserveLambda(self.allocator);
    try self.store.installReservedLambdaPrototype(reserved, .{
        .fn_monotype = fn_monotype,
        .params = MIR.LocalSpan.empty(),
        .body = @enumFromInt(0),
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = source_region,
        .captures_param = null,
        .recursion = .not_recursive,
        .hosted = null,
    });
    try self.reserved_callable_insts.put(callable_inst_key, reserved);
    return reserved;
}

fn lowerReservedTrivialClosureLambda(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    callable_def: *const Pipeline.CallableDef,
    fn_monotype: Monotype.Idx,
    callable_inst_id: Pipeline.CallableInstId,
    reserved_lambda: MIR.LambdaId,
) Allocator.Error!MIR.LambdaId {
    const cache_key = makeCallableCacheKey(session, expr_idx, fn_monotype);
    if (try lookupLoweredCallableLambdaCache(session, expr_idx, fn_monotype)) |cached| {
        if (cached == reserved_lambda) {
            if (self.in_progress_callable_insts.get(@intFromEnum(callable_inst_id)) != null) return cached;
            if (self.reserved_callable_insts.get(@intFromEnum(callable_inst_id)) == null) return cached;
        } else {
            return cached;
        }
    }
    try self.lowered_callable_lambdas.put(cache_key, reserved_lambda);

    const body_program_expr = self.callableDefBodyExpr(callable_def);
    const body_expr_idx = body_program_expr.common().source_expr;
    const param_patterns = self.callable_pipeline.getPatternIds(callable_def.arg_patterns);
    const ret_monotype = try self.callableDefBodyRetMonotype(callable_def);

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

    const saved_scope = self.current_binding_scope;
    const lambda_entry_scope = self.freshStatementBindingScope();
    self.current_binding_scope = lambda_entry_scope;
    defer self.current_binding_scope = saved_scope;
    const saved_lambda_scope_boundary = self.current_lambda_scope_boundary;
    self.current_lambda_scope_boundary = lambda_entry_scope;
    defer self.current_lambda_scope_boundary = saved_lambda_scope_boundary;
    const saved_body_local_floor = self.current_body_local_floor;
    self.current_body_local_floor = self.store.locals.items.len;
    defer self.current_body_local_floor = saved_body_local_floor;
    const saved_lambda_entry_bound_locals = self.current_lambda_entry_bound_locals;
    var lambda_entry_bound_locals = LambdaEntryBindingState.init(self.allocator);
    defer lambda_entry_bound_locals.deinit();
    self.current_lambda_entry_bound_locals = &lambda_entry_bound_locals;
    defer self.current_lambda_entry_bound_locals = saved_lambda_entry_bound_locals;

    const params = try self.lowerLambdaParamLocals(session, module_env, param_patterns, fn_monotype);
    var stable_param_locals = try std.ArrayList(MIR.LocalId).initCapacity(self.allocator, self.store.getLocalSpan(params).len);
    defer stable_param_locals.deinit(self.allocator);
    stable_param_locals.appendSliceAssumeCapacity(self.store.getLocalSpan(params));
    const param_locals = stable_param_locals.items;
    for (param_patterns) |pattern_idx| {
        try self.predeclareBindingLocals(session, module_env, pattern_idx);
    }

    const capture_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(capture_top);
    var capture_monotypes = std.ArrayList(Monotype.Idx).empty;
    defer capture_monotypes.deinit(self.allocator);
    var capture_pattern_by_local = std.AutoHashMap(u32, CIR.Pattern.Idx).init(self.allocator);
    defer capture_pattern_by_local.deinit();
    var recursive_captures = std.ArrayList(struct {
        local: MIR.LocalId,
        callable_inst_id: Pipeline.CallableInstId,
    }).empty;
    defer recursive_captures.deinit(self.allocator);
    var callable_only_captures = std.ArrayList(struct {
        local: MIR.LocalId,
        callable_inst_id: ?Pipeline.CallableInstId,
        lambda_id: ?MIR.LambdaId,
    }).empty;
    defer callable_only_captures.deinit(self.allocator);
    var planned_captures = try self.planClosureEntryCaptureLocals(
        expr_idx,
        self.callable_pipeline.getCaptureFields(callable_def.captures),
        lambda_entry_scope,
    );
    defer planned_captures.deinit(self.allocator);

    for (planned_captures.runtime.items) |capture| {
        try capture_pattern_by_local.put(@intFromEnum(capture.local), capture.pattern_idx);
        try self.scratch_local_ids.append(capture.local);
        try capture_monotypes.append(
            self.allocator,
            switch (capture.storage) {
                .runtime_field => |runtime_field| try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    runtime_field.field_monotype.idx,
                    runtime_field.field_monotype.module_idx,
                    self.current_module_idx,
                ),
                .callable_only, .recursive_member => unreachable,
            },
        );
    }

    for (planned_captures.recursive.items) |capture| {
        try capture_pattern_by_local.put(@intFromEnum(capture.local), capture.pattern_idx);
        try recursive_captures.append(self.allocator, .{
            .local = capture.local,
            .callable_inst_id = capture.callable_inst_id,
        });
    }

    for (planned_captures.callable_only.items) |capture| {
        try capture_pattern_by_local.put(@intFromEnum(capture.local), capture.pattern_idx);
        try callable_only_captures.append(self.allocator, .{
            .local = capture.local,
            .callable_inst_id = capture.callable_inst_id,
            .lambda_id = null,
        });
    }

    const param_callables = try self.buildCallableParamResolutions(session, param_patterns, param_locals);
    try self.store.installReservedLambdaPrototype(reserved_lambda, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .param_callables = param_callables,
        .body = @enumFromInt(0),
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = null,
        .recursion = .not_recursive,
        .hosted = null,
    });

    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    const lambda_body_scope = self.freshChildBindingScope(lambda_entry_scope);
    self.current_binding_scope = lambda_body_scope;
    var body = try self.lowerCirExprInto(session, body_expr_idx, result_local, ret_stmt);
    self.current_binding_scope = lambda_entry_scope;

    const nonrecursive_capture_locals = self.scratch_local_ids.sliceFromStart(capture_top);
    const captures_tuple_monotype = if (nonrecursive_capture_locals.len == 0)
        Monotype.Idx.none
    else
        try self.tupleMonotypeForFields(capture_monotypes.items);

    const captures_param_local = if (nonrecursive_capture_locals.len == 0)
        null
    else
        try self.freshSyntheticLocal(captures_tuple_monotype, false);

    const nonrecursive_capture_span = try self.store.addLocalSpan(self.allocator, nonrecursive_capture_locals);

    if (builtin.mode == .Debug and captures_param_local != null) {
        var seen_nonrecursive_capture_locals = std.AutoHashMap(u32, void).init(self.allocator);
        defer seen_nonrecursive_capture_locals.deinit();
        for (nonrecursive_capture_locals) |capture_local| {
            const key = @intFromEnum(capture_local);
            if (seen_nonrecursive_capture_locals.contains(key)) {
                std.debug.panic(
                    "statement-only MIR invariant violated: trivial closure lambda {d} collected duplicate nonrecursive capture local {d} (callable_inst={d})",
                    .{ @intFromEnum(expr_idx), key, @intFromEnum(callable_inst_id) },
                );
            }
            try seen_nonrecursive_capture_locals.put(key, {});
            if (self.store.getLocalDefOpt(capture_local)) |existing_def| {
                switch (existing_def) {
                    .ref => |existing_ref| std.debug.panic(
                        "statement-only MIR invariant violated: trivial closure lambda {d} capture local {d} (pattern={d}) already had ref def before captures-param unpack (callable_inst={d}, ref_tag={s}, source_local={d}, source_def={s}, field_idx={d}, payload_idx={d}, captures_param={d})",
                        .{
                            @intFromEnum(expr_idx),
                            key,
                            if (capture_pattern_by_local.get(key)) |pattern_idx| @intFromEnum(pattern_idx) else std.math.maxInt(u32),
                            @intFromEnum(callable_inst_id),
                            @tagName(existing_ref.op),
                            switch (existing_ref.op) {
                                .local => |source_local| @intFromEnum(source_local),
                                .field => |field| @intFromEnum(field.source),
                                .tag_payload => |payload| @intFromEnum(payload.source),
                                .nominal => |nominal| @intFromEnum(nominal.backing),
                                .discriminant => |discriminant| @intFromEnum(discriminant.source),
                            },
                            if (self.store.getLocalDefOpt(switch (existing_ref.op) {
                                .local => |source_local| source_local,
                                .field => |field| field.source,
                                .tag_payload => |payload| payload.source,
                                .nominal => |nominal| nominal.backing,
                                .discriminant => |discriminant| discriminant.source,
                            })) |source_def| @tagName(source_def) else "none",
                            switch (existing_ref.op) {
                                .field => |field| field.field_idx,
                                else => std.math.maxInt(u32),
                            },
                            switch (existing_ref.op) {
                                .tag_payload => |payload| payload.payload_idx,
                                else => std.math.maxInt(u16),
                            },
                            @intFromEnum(captures_param_local.?),
                        },
                    ),
                    else => std.debug.panic(
                        "statement-only MIR invariant violated: trivial closure lambda {d} capture local {d} already had def {s} before captures-param unpack (callable_inst={d})",
                        .{ @intFromEnum(expr_idx), key, @tagName(existing_def), @intFromEnum(callable_inst_id) },
                    ),
                }
            }
        }
    }

    var recursive_i = recursive_captures.items.len;
    while (recursive_i > 0) {
        recursive_i -= 1;
        const recursive_capture = recursive_captures.items[recursive_i];
        const member_lambda = try self.lowerResolvedCallableInstLambda(recursive_capture.callable_inst_id);
        try self.requireLocalMonotypeMatchesLambdaRuntime(recursive_capture.local, member_lambda);
        body = if (!self.callableInstRequiresHiddenCapture(recursive_capture.callable_inst_id))
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
            const capture_pattern_idx = capture_pattern_by_local.get(@intFromEnum(nonrecursive_capture_locals[i])) orelse unreachable;
            body = try self.lowerRefIntoWithCallable(
                nonrecursive_capture_locals[i],
                .{ .field = .{
                    .source = local,
                    .field_idx = @intCast(i),
                    .ownership = .move,
                } },
                try self.callableResolutionForPattern(session, capture_pattern_idx),
                body,
            );
        }
    }

    var param_i = param_patterns.len;
    while (param_i > 0) {
        param_i -= 1;
        if (!self.paramPatternNeedsBindingStmt(module_env, param_patterns[param_i], param_locals[param_i])) continue;
        body = try self.lowerBindingLocalInto(session, module_env, param_patterns[param_i], param_locals[param_i], self.lookupBindingSourceExprRef(param_patterns[param_i]), body);
    }

    var callable_only_i = callable_only_captures.items.len;
    while (callable_only_i > 0) {
        callable_only_i -= 1;
        const callable_only_capture = callable_only_captures.items[callable_only_i];
        if (callable_only_capture.callable_inst_id) |capture_callable_inst_id| {
            body = try self.lowerResolvedCallableInstValueInto(
                capture_callable_inst_id,
                callable_only_capture.local,
                body,
            );
        } else {
            const lambda_id = callable_only_capture.lambda_id orelse unreachable;
            try self.requireLocalMonotypeMatchesLambdaRuntime(callable_only_capture.local, lambda_id);
            body = try self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
                .target = callable_only_capture.local,
                .lambda = lambda_id,
                .next = body,
            } });
        }
    }

    try self.store.finalizeLambda(reserved_lambda, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .param_callables = param_callables,
        .body = body,
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = captures_param_local,
        .recursion = if (recursive_captures.items.len != 0) .recursive else .not_recursive,
        .hosted = null,
    });
    if (builtin.mode == .Debug) {
        try DebugVerifyMir.verifyLambda(self.allocator, self.store, reserved_lambda);
    }

    return reserved_lambda;
}

fn lowerResolvedCallableInstValueInto(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const lambda_id = try self.lowerResolvedCallableInstLambda(callable_inst_id);
    try self.requireLocalMonotypeMatchesCallableInst(target, callable_inst_id, "lowerResolvedCallableInstValueInto");
    if (!self.callableInstRequiresHiddenCapture(callable_inst_id)) {
        return self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
            .target = target,
            .lambda = lambda_id,
            .next = next,
        } });
    }

    const callable_inst = self.callable_pipeline.getCallableInst(callable_inst_id);
    const callable_def = self.callable_pipeline.getCallableDef(callable_inst.callable_def);
    const module_idx = callable_def.module_idx;
    const module_env = self.all_module_envs[module_idx];

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

    for (self.callable_pipeline.getCaptureFields(callable_def.captures)) |capture_field| {
        switch (capture_field.storage) {
            .runtime_field => {
                const capture_local = try self.appendCaptureMaterialization(capture_field);
                try self.scratch_local_ids.append(capture_local);
            },
            .callable_only, .recursive_member => {},
        }
    }

    const runtime_captures = self.scratch_local_ids.sliceFromStart(capture_top);
    const current = if (runtime_captures.len == 0)
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

    return current;
}

fn lowerCapturedSourceExprInto(
    self: *Self,
    session: LowerSession,
    source_expr_ref: Pipeline.ExprRef,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const projections = self.callable_pipeline.getValueProjectionEntries(source_expr_ref.projections);
    if (projections.len != 0) {
        const base_expr_ref = Pipeline.ExprRef{
            .source_context = source_expr_ref.source_context,
            .module_idx = source_expr_ref.module_idx,
            .expr_idx = source_expr_ref.expr_idx,
            .projections = .empty(),
        };
        const base_session = session.withSourceContext(base_expr_ref.source_context);
        const base_monotype = try self.resolveExprMonotypeInModule(
            base_session,
            base_expr_ref.module_idx,
            base_expr_ref.expr_idx,
        );
        const base_local = try self.freshSyntheticLocal(base_monotype, false);

        const locals_top = self.scratch_local_ids.top();
        defer self.scratch_local_ids.clearFrom(locals_top);

        var current_monotype = base_monotype;
        for (projections, 0..) |projection, i| {
            const projected_monotype = try self.executableMonotypeForCallableValue(
                try self.projectMonotypeByValueProjection(current_monotype, projection),
                try self.resolveProjectedExprCallableValue(
                    base_expr_ref.source_context,
                    base_expr_ref.module_idx,
                    base_expr_ref.expr_idx,
                    projections[0 .. i + 1],
                ),
            );
            if (i + 1 < projections.len) {
                try self.scratch_local_ids.append(try self.freshSyntheticLocal(projected_monotype, false));
            }
            current_monotype = projected_monotype;
        }

        var entry = next;
        var i: usize = projections.len;
        while (i > 0) {
            i -= 1;
            const source_local = if (i == 0)
                base_local
            else
                self.scratch_local_ids.items.items[locals_top + (i - 1)];
            const projected_local = if (i + 1 == projections.len)
                target
            else
                self.scratch_local_ids.items.items[locals_top + i];
            entry = try self.lowerValueProjectionInto(
                projections[i],
                source_local,
                projected_local,
                if (try self.resolveProjectedExprCallableValue(
                    base_expr_ref.source_context,
                    base_expr_ref.module_idx,
                    base_expr_ref.expr_idx,
                    projections[0 .. i + 1],
                )) |callable_value|
                    try self.callableResolutionFromValue(callable_value)
                else
                    null,
                entry,
            );
        }

        return self.lowerCapturedSourceExprInto(base_session, base_expr_ref, base_local, entry);
    }

    const source_session = session.withSourceContext(source_expr_ref.source_context);

    if (source_expr_ref.module_idx == self.current_module_idx) {
        return self.lowerCirExprInto(source_session, source_expr_ref.expr_idx, target, next);
    }

    const module_env = self.all_module_envs[source_expr_ref.module_idx];
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;

    self.current_module_idx = source_expr_ref.module_idx;
    self.types_store = &module_env.types;
    self.mono_scratches.ident_store = module_env.getIdentStoreConst();
    self.mono_scratches.module_env = module_env;
    self.mono_scratches.module_idx = source_expr_ref.module_idx;
    defer {
        self.current_module_idx = saved_module_idx;
        self.types_store = saved_types_store;
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
        self.mono_scratches.module_idx = saved_mono_module_idx;
    }

    try self.requireTargetLocalMonotypeFromExprIntoModule(
        source_session,
        source_expr_ref.expr_idx,
        source_expr_ref.module_idx,
        saved_module_idx,
        target,
    );
    return self.lowerCirExprInto(source_session, source_expr_ref.expr_idx, target, next);
}

fn projectMonotypeByValueProjection(
    self: *Self,
    source_monotype: Monotype.Idx,
    projection: Pipeline.ValueProjection,
) Allocator.Error!Monotype.Idx {
    return switch (projection) {
        .field => |field_name| blk: {
            const record = switch (self.store.monotype_store.getMonotype(source_monotype)) {
                .record => |record| record,
                else => typeBindingInvariant(
                    "value-path field projection expected record monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(source_monotype))},
                ),
            };
            const field_idx = self.recordFieldIndexByName(
                field_name,
                self.store.monotype_store.getFields(record.fields),
            );
            break :blk self.store.monotype_store.getFieldItem(record.fields, field_idx).type_idx;
        },
        .tuple_elem => |elem_index| blk: {
            const tuple = switch (self.store.monotype_store.getMonotype(source_monotype)) {
                .tuple => |tuple| tuple,
                else => typeBindingInvariant(
                    "value-path tuple projection expected tuple monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(source_monotype))},
                ),
            };
            const elems = self.store.monotype_store.getIdxSpan(tuple.elems);
            if (builtin.mode == .Debug and elem_index >= elems.len) {
                std.debug.panic(
                    "statement-only MIR invariant violated: tuple projection elem_index {d} out of bounds for tuple arity {d}",
                    .{ elem_index, elems.len },
                );
            }
            break :blk elems[elem_index];
        },
        .tag_payload => |payload| blk: {
            const payload_monos = self.tagPayloadMonotypesByName(source_monotype, payload.tag_name.ident);
            if (builtin.mode == .Debug and payload.payload_index >= payload_monos.len) {
                std.debug.panic(
                    "statement-only MIR invariant violated: tag payload projection index {d} out of bounds for tag payload arity {d}",
                    .{ payload.payload_index, payload_monos.len },
                );
            }
            break :blk payload_monos[payload.payload_index];
        },
        .list_elem => |elem_index| blk: {
            _ = elem_index;
            const list_mono = switch (self.store.monotype_store.getMonotype(source_monotype)) {
                .list => |list| list,
                else => typeBindingInvariant(
                    "value-path list projection expected list monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(source_monotype))},
                ),
            };
            break :blk list_mono.elem;
        },
    };
}

fn lowerValueProjectionInto(
    self: *Self,
    projection: Pipeline.ValueProjection,
    source_local: MIR.LocalId,
    target_local: MIR.LocalId,
    result_callable: ?CallableResolution,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const source_monotype = self.store.getLocal(source_local).monotype;
    return switch (projection) {
        .field => |field_name| blk: {
            const record = switch (self.store.monotype_store.getMonotype(source_monotype)) {
                .record => |record| record,
                else => typeBindingInvariant(
                    "value-path field projection expected record monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(source_monotype))},
                ),
            };
            const field_idx = self.recordFieldIndexByName(
                field_name,
                self.store.monotype_store.getFields(record.fields),
            );
            break :blk try self.lowerRefIntoWithCallable(
                target_local,
                .{ .field = .{
                    .source = source_local,
                    .field_idx = field_idx,
                    .ownership = .move,
                } },
                result_callable,
                next,
            );
        },
        .tuple_elem => |elem_index| try self.lowerRefIntoWithCallable(
            target_local,
            .{ .field = .{
                .source = source_local,
                .field_idx = elem_index,
                .ownership = .move,
            } },
            result_callable,
            next,
        ),
        .tag_payload => |payload| try self.lowerRefIntoWithCallable(
            target_local,
            .{ .tag_payload = .{
                .source = source_local,
                .payload_idx = payload.payload_index,
                .tag_discriminant = @intCast(self.tagDiscriminantForMonotypeName(source_monotype, payload.tag_name)),
                .ownership = .move,
            } },
            result_callable,
            next,
        ),
        .list_elem => |elem_index| blk: {
            const index_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
            const args = try self.store.addLocalSpan(self.allocator, &.{ source_local, index_local });
            const stmt = try self.store.reserveCFStmt(self.allocator);
            try self.store.finalizeCFStmt(stmt, .{ .assign_low_level = .{
                .target = target_local,
                .op = .list_get_unsafe,
                .result_callable = result_callable,
                .args = args,
                .next = next,
            } });
            break :blk try self.lowerU64LiteralInto(index_local, elem_index, stmt);
        },
    };
}

fn resolveExprMonotypeInModule(
    self: *Self,
    session: LowerSession,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!Monotype.Idx {
    if (module_idx == self.current_module_idx) {
        return self.resolveMonotype(session, expr_idx);
    }

    const module_env = self.all_module_envs[module_idx];
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;

    self.current_module_idx = module_idx;
    self.types_store = &module_env.types;
    self.mono_scratches.ident_store = module_env.getIdentStoreConst();
    self.mono_scratches.module_env = module_env;
    self.mono_scratches.module_idx = module_idx;
    defer {
        self.current_module_idx = saved_module_idx;
        self.types_store = saved_types_store;
        self.mono_scratches.ident_store = saved_ident_store;
        self.mono_scratches.module_env = saved_module_env;
        self.mono_scratches.module_idx = saved_mono_module_idx;
    }

    return self.resolveMonotype(session, expr_idx);
}

fn requireTargetLocalMonotypeFromExprIntoModule(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    expr_module_idx: u32,
    target_module_idx: u32,
    target: MIR.LocalId,
) Allocator.Error!void {
    const monotype = try self.resolveMonotype(session, expr_idx);
    if (monotype.isNone()) return;
    try self.requireLocalMonotype(
        target,
        if (expr_module_idx == target_module_idx)
            monotype
        else
            try self.remapMonotypeBetweenModules(monotype, expr_module_idx, target_module_idx),
        "requireTargetLocalMonotypeFromExprIntoModule",
    );
}

fn lowerResolvedCallableInstLambda(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
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

    const callable_inst = self.callable_pipeline.getCallableInst(callable_inst_id);
    const callable_def = self.callable_pipeline.getCallableDef(callable_inst.callable_def);
    const runtime_expr_idx = callable_def.runtime_expr.expr_idx;
    const module_idx = callable_def.module_idx;
    const module_env = self.all_module_envs[module_idx];
    const fn_monotype = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
        module_idx,
    );
    const body_ret_monotype = try self.callableDefBodyRetMonotype(callable_def);
    const reserved_lambda = try self.reserveResolvedCallableInstLambdaSkeleton(
        callable_inst_id,
        fn_monotype,
        body_ret_monotype,
        callable_def.source_region,
    );
    const switching_module = module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;
    const session = self.lowerSession(callableInstSourceContext(callable_inst_id));

    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);

    if (switching_module) {
        self.current_module_idx = module_idx;
        self.types_store = &module_env.types;
        self.mono_scratches.ident_store = module_env.getIdentStoreConst();
        self.mono_scratches.module_env = module_env;
        self.mono_scratches.module_idx = module_idx;
    }

    defer {
        self.nominal_cycle_breakers.deinit();
        self.nominal_cycle_breakers = saved_nominal_cycle_breakers;
        if (switching_module) {
            self.current_module_idx = saved_module_idx;
            self.types_store = saved_types_store;
            self.mono_scratches.ident_store = saved_ident_store;
            self.mono_scratches.module_env = saved_module_env;
            self.mono_scratches.module_idx = saved_mono_module_idx;
        }
    }

    const lambda_id = switch (callable_def.runtime_kind) {
        .closure => try self.lowerReservedTrivialClosureLambda(
            session,
            module_env,
            runtime_expr_idx,
            callable_def,
            fn_monotype,
            callable_inst_id,
            reserved_lambda,
        ),
        .lambda => try self.lowerLambdaInto(
            session,
            module_env,
            runtime_expr_idx,
            self.callable_pipeline.getPatternIds(callable_def.arg_patterns),
            self.callableDefBodyExpr(callable_def).common().source_expr,
            fn_monotype,
            body_ret_monotype,
            reserved_lambda,
        ),
        .hosted_lambda => try self.lowerLambdaInto(
            session,
            module_env,
            runtime_expr_idx,
            self.callable_pipeline.getPatternIds(callable_def.arg_patterns),
            self.callableDefBodyExpr(callable_def).common().source_expr,
            fn_monotype,
            body_ret_monotype,
            reserved_lambda,
        ),
    };

    _ = self.reserved_callable_insts.remove(callable_inst_key);
    try self.lowered_callable_insts.put(callable_inst_key, lambda_id);
    return lambda_id;
}

fn dispatchTargetEffectful(
    self: *Self,
    dispatch_target: Pipeline.DispatchExprTarget,
) Allocator.Error!bool {
    const target_env = self.all_module_envs[dispatch_target.module_idx];
    const def = target_env.store.getDef(dispatch_target.def_idx);
    var resolved = target_env.types.resolveVar(ModuleEnv.varFrom(def.expr));
    while (resolved.desc.content == .alias) {
        resolved = target_env.types.resolveVar(target_env.types.getAliasBackingVar(resolved.desc.content.alias));
    }
    const func_info = types.Content.unwrapFuncFull(resolved.desc.content) orelse std.debug.panic(
        "statement-only MIR invariant violated: dispatch target def {d} in module {d} did not have checked func type",
        .{ @intFromEnum(dispatch_target.def_idx), dispatch_target.module_idx },
    );
    return switch (func_info.ext) {
        .pure, .unbound => false,
        .effectful => true,
    };
}

fn lowerResolvedDispatchTargetCallInto(
    self: *Self,
    session: LowerSession,
    dispatch_target: Pipeline.DispatchExprTarget,
    result_expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    var actual_arg_exprs = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_arg_exprs.deinit(self.allocator);
    try self.appendDispatchActualArgExprs(session, result_expr_idx, &actual_arg_exprs);

    _ = try self.dispatchTargetEffectful(dispatch_target);
    const call_site = lookupProgramExprCallSite(session, result_expr_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: resolved dispatch expr {d} target module={d} def={d} had no call-site semantics in context={d} root_expr={d}",
        .{
            @intFromEnum(result_expr_idx),
            dispatch_target.module_idx,
            @intFromEnum(dispatch_target.def_idx),
            session.callableInstRawForDebug(),
            session.rootExprRawForDebug(),
        },
    );
    const target_env = self.all_module_envs[dispatch_target.module_idx];
    const target_def = target_env.store.getDef(dispatch_target.def_idx);
    const target_def_session = self.lowerSession(.{ .root_expr = .{
        .module_idx = dispatch_target.module_idx,
        .expr_idx = target_def.expr,
    } });
    const target_callable_value = lookupProgramExprCallableValue(target_def_session, target_def.expr);
    const callee_runtime_mono = switch (call_site) {
        .low_level => unreachable,
        .direct => |callable_inst_id| if (target_callable_value) |callable_value|
            self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value)
        else
            self.callable_pipeline.getCallableInstRuntimeMonotype(callable_inst_id),
        .indirect_call => |indirect_call| indirect_call.packed_fn.runtime_monotype,
    };
    const callee_local = try self.freshSyntheticLocal(try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        callee_runtime_mono.idx,
        callee_runtime_mono.module_idx,
        self.current_module_idx,
    ), false);
    const target_symbol = try self.internExternalDefSymbol(
        dispatch_target.module_idx,
        @intCast(@intFromEnum(dispatch_target.def_idx)),
    );

    const arg_locals = try self.allocator.alloc(MIR.LocalId, actual_arg_exprs.items.len);
    defer self.allocator.free(arg_locals);
    for (actual_arg_exprs.items, 0..) |arg_expr_idx, i| {
        arg_locals[i] = try self.freshSyntheticLocal(try self.resolveMonotype(session, arg_expr_idx), false);
    }
    const args = try self.store.addLocalSpan(self.allocator, arg_locals);

    const callee_entry = switch (call_site) {
        .low_level => unreachable,
        .direct => |direct_dispatch_callable_inst| blk: {
            const call_stmt = try self.store.reserveCFStmt(self.allocator);
            const NormalizedDispatchCallee = struct {
                entry: MIR.CFStmtId,
                callee_local: MIR.LocalId,
            };
            const normalized: NormalizedDispatchCallee = switch (target_callable_value orelse Pipeline.CallableValue{ .direct = direct_dispatch_callable_inst }) {
                .direct => .{
                    .entry = call_stmt,
                    .callee_local = callee_local,
                },
                .packed_fn => |packed_fn| blk2: {
                    const payload_resolved = self.callable_pipeline.getCallableInstRuntimeMonotype(direct_dispatch_callable_inst);
                    const payload_mono = try self.importMonotypeFromStore(
                        &self.callable_pipeline.context_mono.monotype_store,
                        payload_resolved.idx,
                        payload_resolved.module_idx,
                        self.current_module_idx,
                    );
                    const payload_local = try self.freshSyntheticLocal(payload_mono, false);
                    break :blk2 NormalizedDispatchCallee{
                        .entry = try self.lowerPackedCallablePayloadInto(
                            packed_fn,
                            direct_dispatch_callable_inst,
                            callee_local,
                            payload_local,
                            call_stmt,
                        ),
                        .callee_local = payload_local,
                    };
                },
            };
            const callee_callable = try self.callableResolutionFromInst(direct_dispatch_callable_inst);
            const result_callable = try self.resolveCallResultCallableFromExpr(session, result_expr_idx, target);
            try self.store.finalizeCFStmt(call_stmt, .{ .assign_call = .{
                .target = target,
                .callee = normalized.callee_local,
                .callee_callable = callee_callable,
                .result_callable = result_callable,
                .args = args,
                .next = next,
            } });
            break :blk try self.materializeTopLevelDefInto(
                target_def_session,
                dispatch_target.module_idx,
                dispatch_target.def_idx,
                target_symbol,
                callee_local,
                normalized.entry,
                .{ .callable = target_callable_value orelse Pipeline.CallableValue{ .direct = direct_dispatch_callable_inst } },
            );
        },
        .indirect_call => |indirect_call| blk: {
            const packed_fn = indirect_call.packed_fn;
            const call_entry = try self.lowerIndirectCallInto(
                session,
                result_expr_idx,
                indirect_call,
                callee_local,
                args,
                target,
                next,
            );
            break :blk try self.materializeTopLevelDefInto(
                target_def_session,
                dispatch_target.module_idx,
                dispatch_target.def_idx,
                target_symbol,
                callee_local,
                call_entry,
                .{ .callable = .{ .packed_fn = packed_fn } },
            );
        },
    };

    const lowered_args = try self.lowerExprSequenceIntoContinuationWithValues(
        session,
        actual_arg_exprs.items,
        arg_locals,
        callee_entry,
    );
    return lowered_args.entry;
}

fn appendDispatchActualArgExprs(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    actual_arg_exprs: *std.ArrayList(CIR.Expr.Idx),
) Allocator.Error!void {
    const expr_id = self.callable_pipeline.lambdamono.getExprId(
        session.source_context,
        self.current_module_idx,
        expr_idx,
    ) orelse std.debug.panic(
        "statement-only MIR invariant violated: dispatch expr {d} in module {d} had no specialized program node",
        .{ @intFromEnum(expr_idx), self.current_module_idx },
    );
    const child_expr_ids = self.callable_pipeline.lambdamono.getExprChildren(
        self.callable_pipeline.lambdamono.getExpr(expr_id).common().child_exprs,
    );
    for (child_expr_ids) |child_expr_id| {
        try actual_arg_exprs.append(
            self.allocator,
            self.callable_pipeline.lambdamono.getExpr(child_expr_id).common().source_expr,
        );
    }
}

fn lowerIndirectCallInto(
    self: *Self,
    session: LowerSession,
    call_expr_idx: CIR.Expr.Idx,
    indirect_call: Pipeline.IndirectCall,
    callee_local: MIR.LocalId,
    args: MIR.LocalSpan,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const packed_fn = indirect_call.packed_fn;
    const variants = self.callable_pipeline.getIndirectCallVariants(indirect_call);
    if (variants.len == 0) {
        std.debug.panic(
            "statement-only MIR invariant violated: indirect call {d} had no variants",
            .{@intFromEnum(indirect_call.packed_fn.variant_group)},
        );
    }

    const target_mono = self.store.getLocal(target).monotype;
    const joined_result_local = try self.freshSyntheticLocal(target_mono, false);
    const join_id = self.freshJoinPointId();
    const join_body = try self.lowerLocalAliasInto(target, joined_result_local, next);

    const switch_branches = try self.allocator.alloc(MIR.SwitchBranch, variants.len - 1);
    defer self.allocator.free(switch_branches);

    const direct_result_callable_inst_id = if (lookupProgramExprCallableValue(session, call_expr_idx)) |callable_value|
        switch (callable_value) {
            .direct => |callable_inst_id| callable_inst_id,
            .packed_fn => null,
        }
    else
        null;

    const default_branch = try self.lowerIndirectCallBranchInto(
        packed_fn,
        variants[0],
        callee_local,
        args,
        target_mono,
        joined_result_local,
        join_id,
        direct_result_callable_inst_id,
    );

    for (variants[1..], 0..) |variant_callable_inst_id, i| {
        switch_branches[i] = .{
            .value = self.tagDiscriminantForMonotypeName(
                self.store.getLocal(callee_local).monotype,
                try self.packedCallableTagName(
                    packed_fn,
                    variant_callable_inst_id,
                ),
            ),
            .body = try self.lowerIndirectCallBranchInto(
                packed_fn,
                variant_callable_inst_id,
                callee_local,
                args,
                target_mono,
                joined_result_local,
                join_id,
                direct_result_callable_inst_id,
            ),
        };
    }

    const discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
    const switch_stmt = try self.store.addCFStmt(self.allocator, .{ .switch_stmt = .{
        .scrutinee = discrim_local,
        .branches = try self.store.addSwitchBranches(self.allocator, switch_branches),
        .default_branch = default_branch,
    } });
    const discrim_entry = try self.lowerRefInto(discrim_local, .{ .discriminant = .{ .source = callee_local } }, switch_stmt);

    return self.addJoinStmt(
        join_id,
        try self.store.addLocalSpan(self.allocator, &.{joined_result_local}),
        join_body,
        discrim_entry,
    );
}

fn lowerIndirectCallBranchInto(
    self: *Self,
    packed_fn: Pipeline.PackedFn,
    variant_callable_inst_id: Pipeline.CallableInstId,
    packed_callee_local: MIR.LocalId,
    args: MIR.LocalSpan,
    target_mono: Monotype.Idx,
    joined_result_local: MIR.LocalId,
    join_id: MIR.JoinPointId,
    direct_result_callable_inst_id: ?Pipeline.CallableInstId,
) Allocator.Error!MIR.CFStmtId {
    const payload_resolved = self.callable_pipeline.getCallableInstRuntimeMonotype(variant_callable_inst_id);
    const payload_mono = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        payload_resolved.idx,
        payload_resolved.module_idx,
        self.current_module_idx,
    );
    const callee_payload_local = try self.freshSyntheticLocal(payload_mono, false);
    const branch_result_local = try self.freshSyntheticLocal(target_mono, false);
    const result_callable = if (direct_result_callable_inst_id) |callable_inst_id| blk: {
        try self.requireLocalMonotypeMatchesCallableInst(branch_result_local, callable_inst_id, "lowerIndirectCallBranchInto");
        break :blk try self.callableResolutionFromInst(callable_inst_id);
    } else null;

    const jump_stmt = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = join_id,
        .args = try self.store.addLocalSpan(self.allocator, &.{branch_result_local}),
    } });
    try self.mergeJoinParamCallableResolutionValue(joined_result_local, result_callable);

    const call_entry = try self.store.addCFStmt(self.allocator, .{ .assign_call = .{
        .target = branch_result_local,
        .callee = callee_payload_local,
        .callee_callable = try self.callableResolutionFromInst(variant_callable_inst_id),
        .result_callable = result_callable,
        .args = args,
        .next = jump_stmt,
    } });

    return self.lowerPackedCallablePayloadInto(
        packed_fn,
        variant_callable_inst_id,
        packed_callee_local,
        callee_payload_local,
        call_entry,
    );
}

fn lowerCallInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    call_expr_idx: CIR.Expr.Idx,
    call: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const call_site = lookupProgramExprCallSite(session, call_expr_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: call expr {d} had no specialized call-site semantics",
        .{@intFromEnum(call_expr_idx)},
    );
    const cir_args = module_env.store.sliceExpr(call.args);
    switch (call_site) {
        .low_level => |ll_op| {
            if (ll_op == .str_inspect) {
                if (cir_args.len != 1) {
                    std.debug.panic(
                        "statement-only MIR str_inspect call expected 1 arg, got {d}",
                        .{cir_args.len},
                    );
                }

                const source_mono = try self.resolveMonotype(session, cir_args[0]);
                const source_local = try self.freshSyntheticLocal(source_mono, false);
                const inspect_stmt = try self.lowerStrInspectLocalInto(source_local, source_mono, target, next);
                return self.lowerCirExprInto(session, cir_args[0], source_local, inspect_stmt);
            }

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);
            const arg_locals = try self.allocator.alloc(MIR.LocalId, cir_args.len);
            defer self.allocator.free(arg_locals);
            for (cir_args, 0..) |arg_idx, i| {
                arg_locals[i] = try self.freshSyntheticLocal(
                    try self.resolveMonotype(session, arg_idx),
                    false,
                );
            }
            const args = try self.store.addLocalSpan(self.allocator, arg_locals);
            try self.store.finalizeCFStmt(assign_stmt, .{ .assign_low_level = .{
                .target = target,
                .op = ll_op,
                .args = args,
                .next = next,
            } });
            const lowered_args = try self.lowerExprSequenceIntoContinuationWithValues(
                session,
                cir_args,
                arg_locals,
                assign_stmt,
            );
            return lowered_args.entry;
        },
        .direct => |direct_callable_inst_id| {
            const callee_value = lookupProgramExprCallableValue(session, call.func) orelse std.debug.panic(
                "statement-only MIR invariant violated: direct call callee expr {d} had no callable value semantics",
                .{@intFromEnum(call.func)},
            );
            const callee_runtime = self.callable_pipeline.getCallableValueRuntimeMonotype(callee_value);
            const callee_monotype = try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                callee_runtime.idx,
                callee_runtime.module_idx,
                self.current_module_idx,
            );
            const callee_local = try self.freshSyntheticLocal(callee_monotype, false);
            const arg_locals = try self.allocator.alloc(MIR.LocalId, cir_args.len);
            defer self.allocator.free(arg_locals);
            for (cir_args, 0..) |arg_idx, i| {
                arg_locals[i] = try self.freshSyntheticLocal(try self.resolveMonotype(session, arg_idx), false);
            }
            const args = try self.store.addLocalSpan(self.allocator, arg_locals);
            const call_stmt = try self.store.reserveCFStmt(self.allocator);
            const normalized = try self.lowerDirectCallTargetInto(
                session,
                call.func,
                direct_callable_inst_id,
                callee_local,
                call_stmt,
            );
            const callee_callable = try self.callableResolutionFromInst(direct_callable_inst_id);
            const result_callable = try self.resolveCallResultCallableFromExpr(session, call_expr_idx, target);
            try self.store.finalizeCFStmt(call_stmt, .{ .assign_call = .{
                .target = target,
                .callee = normalized.callee_local,
                .callee_callable = callee_callable,
                .result_callable = result_callable,
                .args = args,
                .next = next,
            } });

            const lowered_args = try self.lowerExprSequenceIntoContinuationWithValues(
                session,
                cir_args,
                arg_locals,
                normalized.entry,
            );
            const needs_structural_callee_lowering = switch (callee_value) {
                .direct => self.callableInstRequiresHiddenCapture(direct_callable_inst_id),
                .packed_fn => false,
            };
            if (!needs_structural_callee_lowering) return lowered_args.entry;
            return switch (module_env.store.getExpr(call.func)) {
                .e_lookup_local => |lookup| try self.lowerLookupLocalInto(session, call.func, lookup, callee_local, lowered_args.entry),
                .e_lookup_external => |lookup| try self.lowerLookupExternalInto(session, call.func, module_env, lookup, callee_local, lowered_args.entry),
                .e_lookup_required => |lookup| try self.lowerLookupRequiredInto(session, call.func, module_env, lookup, callee_local, lowered_args.entry),
                else => try self.lowerCirExprInto(session, call.func, callee_local, lowered_args.entry),
            };
        },
        .indirect_call => |indirect_call| {
            const callee_monotype = try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                indirect_call.packed_fn.runtime_monotype.idx,
                indirect_call.packed_fn.runtime_monotype.module_idx,
                self.current_module_idx,
            );
            const callee_local = try self.freshSyntheticLocal(callee_monotype, false);
            const arg_locals = try self.allocator.alloc(MIR.LocalId, cir_args.len);
            defer self.allocator.free(arg_locals);
            for (cir_args, 0..) |arg_idx, i| {
                arg_locals[i] = try self.freshSyntheticLocal(try self.resolveMonotype(session, arg_idx), false);
            }
            const args = try self.store.addLocalSpan(self.allocator, arg_locals);
            const call_entry = try self.lowerIndirectCallInto(
                session,
                call_expr_idx,
                indirect_call,
                callee_local,
                args,
                target,
                next,
            );

            const lowered_args = try self.lowerExprSequenceIntoContinuationWithValues(
                session,
                cir_args,
                arg_locals,
                call_entry,
            );
            return self.lowerExprAsPackedCallableInto(
                session,
                call.func,
                indirect_call.packed_fn,
                callee_local,
                lowered_args.entry,
            );
        },
    }
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
    session: LowerSession,
    lhs_expr: CIR.Expr.Idx,
    rhs_expr: CIR.Expr.Idx,
    target: MIR.LocalId,
    op: CIR.Expr.LowLevel,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(top);

    const lhs_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, lhs_expr), false);
    try self.scratch_local_ids.append(lhs_local);
    const rhs_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, rhs_expr), false);
    try self.scratch_local_ids.append(rhs_local);

    const assign_stmt = try self.store.reserveCFStmt(self.allocator);
    const args = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
    try self.store.finalizeCFStmt(assign_stmt, .{ .assign_low_level = .{
        .target = target,
        .op = op,
        .args = args,
        .next = next,
    } });
    const lowered_args = try self.lowerExprSequenceIntoContinuationWithValues(
        session,
        &.{ lhs_expr, rhs_expr },
        &.{ lhs_local, rhs_local },
        assign_stmt,
    );
    return lowered_args.entry;
}

fn lowerUnaryMinusInto(
    self: *Self,
    session: LowerSession,
    um: CIR.Expr.UnaryMinus,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const operand_mono = try self.resolveMonotype(session, um.expr);
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
    return self.lowerCirExprInto(session, um.expr, source_local, negate_stmt);
}

fn lowerUnaryNotInto(
    self: *Self,
    session: LowerSession,
    unary: CIR.Expr.UnaryNot,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const operand_mono = try self.resolveMonotype(session, unary.expr);
    _ = boolTagNamesForMonotype(self, operand_mono) orelse std.debug.panic(
        "statement-only MIR unary not expected Bool monotype for expr {d}",
        .{@intFromEnum(unary.expr)},
    );

    const source_local = try self.freshSyntheticLocal(operand_mono, false);
    const not_stmt = try self.lowerUnaryLowLevelInto(target, .bool_not, source_local, next);
    return self.lowerCirExprInto(session, unary.expr, source_local, not_stmt);
}

fn lowerIntrinsicDispatchInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    dispatch_intrinsic: Pipeline.DispatchIntrinsic,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    var arg_exprs = std.ArrayList(CIR.Expr.Idx).empty;
    defer arg_exprs.deinit(self.allocator);
    try self.appendDispatchActualArgExprs(session, expr_idx, &arg_exprs);

    return switch (dispatch_intrinsic) {
        .negate => blk: {
            if (arg_exprs.items.len != 1) {
                std.debug.panic(
                    "statement-only MIR negate intrinsic expected 1 arg, got {d}",
                    .{arg_exprs.items.len},
                );
            }

            const operand_mono = try self.resolveMonotype(session, arg_exprs.items[0]);
            switch (self.store.monotype_store.getMonotype(operand_mono)) {
                .prim => |prim| switch (prim) {
                    .i8, .i16, .i32, .i64, .i128, .f32, .f64, .dec => {},
                    else => std.debug.panic(
                        "statement-only MIR negate intrinsic is not defined for primitive {s}",
                        .{@tagName(prim)},
                    ),
                },
                else => std.debug.panic(
                    "statement-only MIR negate intrinsic is not implemented for monotype kind {s}",
                    .{@tagName(self.store.monotype_store.getMonotype(operand_mono))},
                ),
            }

            const source_local = try self.freshSyntheticLocal(operand_mono, false);
            const negate_stmt = try self.lowerUnaryLowLevelInto(target, .num_negate, source_local, next);
            break :blk try self.lowerCirExprInto(session, arg_exprs.items[0], source_local, negate_stmt);
        },
        .to_str => blk: {
            if (arg_exprs.items.len != 1) {
                std.debug.panic(
                    "statement-only MIR to_str intrinsic expected 1 arg, got {d}",
                    .{arg_exprs.items.len},
                );
            }

            const operand_mono = try self.resolveMonotype(session, arg_exprs.items[0]);
            const source_local = try self.freshSyntheticLocal(operand_mono, false);
            switch (self.store.monotype_store.getMonotype(operand_mono)) {
                .prim => |prim| {
                    const rendered = toStrLowLevelForPrim(prim) orelse {
                        const alias_stmt = try self.lowerLocalAliasInto(target, source_local, next);
                        break :blk try self.lowerCirExprInto(session, arg_exprs.items[0], source_local, alias_stmt);
                    };
                    const render_stmt = try self.lowerUnaryLowLevelInto(target, rendered, source_local, next);
                    break :blk try self.lowerCirExprInto(session, arg_exprs.items[0], source_local, render_stmt);
                },
                else => std.debug.panic(
                    "statement-only MIR to_str intrinsic is not implemented for monotype kind {s}",
                    .{@tagName(self.store.monotype_store.getMonotype(operand_mono))},
                ),
            }
        },
    };
}

fn lowerBinopInto(
    self: *Self,
    session: LowerSession,
    binop: CIR.Expr.Binop,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const result_mono = self.store.getLocal(target).monotype;

    switch (binop.op) {
        .@"and" => {
            const cond_mono = try self.resolveMonotype(session, binop.lhs);
            const cond_local = try self.freshSyntheticLocal(cond_mono, false);
            const join_id = self.freshJoinPointId();

            const else_value = try self.freshSyntheticLocal(result_mono, false);
            const else_jump = try self.store.reserveCFStmt(self.allocator);
            const else_body = try self.lowerBoolLiteralInto(else_value, result_mono, false, else_jump);
            try self.store.finalizeCFStmt(else_jump, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{else_value}),
            } });

            const then_value = try self.freshSyntheticLocal(result_mono, false);
            const then_jump = try self.store.reserveCFStmt(self.allocator);
            const then_body = try self.lowerCirExprInto(session, binop.rhs, then_value, then_jump);
            try self.store.finalizeCFStmt(then_jump, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{then_value}),
            } });

            const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
                "statement-only MIR and expected Bool condition monotype for expr {d}",
                .{@intFromEnum(binop.lhs)},
            );
            const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, then_body, else_body);
            const switch_entry = try self.lowerCirExprInto(session, binop.lhs, cond_local, switch_stmt);
            return self.addJoinStmt(
                join_id,
                try self.store.addLocalSpan(self.allocator, &.{target}),
                next,
                switch_entry,
            );
        },
        .@"or" => {
            const cond_mono = try self.resolveMonotype(session, binop.lhs);
            const cond_local = try self.freshSyntheticLocal(cond_mono, false);
            const join_id = self.freshJoinPointId();

            const then_value = try self.freshSyntheticLocal(result_mono, false);
            const then_jump = try self.store.reserveCFStmt(self.allocator);
            const then_body = try self.lowerBoolLiteralInto(then_value, result_mono, true, then_jump);
            try self.store.finalizeCFStmt(then_jump, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{then_value}),
            } });

            const else_value = try self.freshSyntheticLocal(result_mono, false);
            const else_jump = try self.store.reserveCFStmt(self.allocator);
            const else_body = try self.lowerCirExprInto(session, binop.rhs, else_value, else_jump);
            try self.store.finalizeCFStmt(else_jump, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{else_value}),
            } });

            const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
                "statement-only MIR or expected Bool condition monotype for expr {d}",
                .{@intFromEnum(binop.lhs)},
            );
            const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, then_body, else_body);
            const switch_entry = try self.lowerCirExprInto(session, binop.lhs, cond_local, switch_stmt);
            return self.addJoinStmt(
                join_id,
                try self.store.addLocalSpan(self.allocator, &.{target}),
                next,
                switch_entry,
            );
        },
        .eq, .ne => {
            const lhs_mono = try self.resolveMonotype(session, binop.lhs);
            if (self.store.monotype_store.getMonotype(lhs_mono) == .unit) {
                return self.lowerBoolLiteralInto(target, result_mono, binop.op == .eq, next);
            }

            const eq_op = binopLowLevel(self, .eq, lhs_mono) orelse std.debug.panic(
                "statement-only MIR binop {s} is not implemented yet for monotype kind {s}",
                .{ @tagName(binop.op), @tagName(self.store.monotype_store.getMonotype(lhs_mono)) },
            );

            if (binop.op == .eq) {
                return self.lowerPrimitiveBinopInto(session, binop.lhs, binop.rhs, target, eq_op, next);
            }

            const eq_local = try self.freshSyntheticLocal(result_mono, false);
            const not_stmt = try self.lowerUnaryLowLevelInto(target, .bool_not, eq_local, next);
            return self.lowerPrimitiveBinopInto(session, binop.lhs, binop.rhs, eq_local, eq_op, not_stmt);
        },
        else => {
            const lhs_mono = try self.resolveMonotype(session, binop.lhs);
            const ll = binopLowLevel(self, binop.op, lhs_mono) orelse std.debug.panic(
                "statement-only MIR binop {s} is not implemented yet for monotype kind {s}",
                .{ @tagName(binop.op), @tagName(self.store.monotype_store.getMonotype(lhs_mono)) },
            );
            return self.lowerPrimitiveBinopInto(session, binop.lhs, binop.rhs, target, ll, next);
        },
    }
}

fn lowerDotAccessInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    da: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const module_env = self.all_module_envs[self.current_module_idx];

    if (da.args != null) {
        if (lookupProgramExprDispatchTarget(session, expr_idx)) |dispatch_target| {
            return self.lowerResolvedDispatchTargetCallInto(
                session,
                dispatch_target,
                expr_idx,
                target,
                next,
            );
        }
        if (lookupProgramExprDispatchIntrinsic(session, expr_idx)) |dispatch_intrinsic| {
            return self.lowerIntrinsicDispatchInto(
                session,
                expr_idx,
                dispatch_intrinsic,
                target,
                next,
            );
        }

        std.debug.panic(
            "statement-only MIR invariant violated: dot-call field '{s}' had no resolved dispatch lowering",
            .{module_env.getIdent(da.field_name)},
        );
    }

    const receiver_mono = try self.resolveMonotype(session, da.receiver);
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
    const field_stmt = try self.lowerRefIntoWithCallable(
        target,
        .{ .field = .{
            .source = receiver_local,
            .field_idx = field_idx,
            .ownership = .move,
        } },
        try self.callableResolutionForExpr(session, expr_idx),
        next,
    );

    return self.lowerCirExprInto(session, da.receiver, receiver_local, field_stmt);
}

fn lowerTupleAccessInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    tuple_access: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const tuple_mono = try self.resolveMonotype(session, tuple_access.tuple);
    const tuple_data = switch (self.store.monotype_store.getMonotype(tuple_mono)) {
        .tuple => |tuple_data| tuple_data,
        else => typeBindingInvariant(
            "lowerTupleAccessInto: tuple access receiver is not a tuple monotype (elem_index={d}, monotype='{s}')",
            .{ tuple_access.elem_index, @tagName(self.store.monotype_store.getMonotype(tuple_mono)) },
        ),
    };
    const elems = self.store.monotype_store.getIdxSpan(tuple_data.elems);
    if (builtin.mode == .Debug and tuple_access.elem_index >= elems.len) {
        std.debug.panic(
            "statement-only MIR invariant violated: tuple access elem_index {d} out of bounds for tuple arity {d}",
            .{ tuple_access.elem_index, elems.len },
        );
    }

    const tuple_local = try self.freshSyntheticLocal(tuple_mono, false);
    const field_stmt = try self.lowerRefIntoWithCallable(
        target,
        .{ .field = .{
            .source = tuple_local,
            .field_idx = tuple_access.elem_index,
            .ownership = .move,
        } },
        try self.callableResolutionForExpr(session, expr_idx),
        next,
    );

    return self.lowerCirExprInto(session, tuple_access.tuple, tuple_local, field_stmt);
}

fn executableMonotypeForCallableValue(
    self: *Self,
    source_monotype: Monotype.Idx,
    callable_value: ?Pipeline.CallableValue,
) Allocator.Error!Monotype.Idx {
    const value = callable_value orelse return source_monotype;
    const runtime_monotype = self.callable_pipeline.getCallableValueRuntimeMonotype(value);
    return self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        runtime_monotype.idx,
        runtime_monotype.module_idx,
        self.current_module_idx,
    );
}

fn resolvePatternExecutableMonotype(
    self: *Self,
    session: LowerSession,
    pattern_idx: CIR.Pattern.Idx,
    source_monotype: Monotype.Idx,
) Allocator.Error!Monotype.Idx {
    return self.executableMonotypeForCallableValue(
        source_monotype,
        try resolveProgramPatternCallableValue(session, pattern_idx),
    );
}

fn requireLocalMonotypeMatchesLambdaRuntime(
    self: *Self,
    local_id: MIR.LocalId,
    lambda_id: MIR.LambdaId,
) Allocator.Error!void {
    const lambda = self.store.getLambdaAnyState(lambda_id);
    const expected_monotype = if (lambda.captures_param) |captures_param|
        self.store.getLocal(captures_param).monotype
    else
        self.store.monotype_store.unit_idx;
    const actual_monotype = self.store.getLocal(local_id).monotype;
    if (builtin.mode == .Debug and !try self.monotypesStructurallyEqual(actual_monotype, expected_monotype)) {
        std.debug.panic(
            "statement-only MIR invariant violated: local {d} monotype {d} did not match lambda {d} executable runtime monotype {d}",
            .{
                @intFromEnum(local_id),
                @intFromEnum(actual_monotype),
                @intFromEnum(lambda_id),
                @intFromEnum(expected_monotype),
            },
        );
    }
}

fn requireLocalMonotype(
    self: *Self,
    local_id: MIR.LocalId,
    expected_monotype: Monotype.Idx,
    context: []const u8,
) Allocator.Error!void {
    const actual_monotype = self.store.getLocal(local_id).monotype;
    if (builtin.mode == .Debug and !try self.monotypesStructurallyEqual(actual_monotype, expected_monotype)) {
        std.debug.panic(
            "statement-only MIR invariant violated: local {d} monotype {d} did not match required executable monotype {d} in {s}",
            .{
                @intFromEnum(local_id),
                @intFromEnum(actual_monotype),
                @intFromEnum(expected_monotype),
                context,
            },
        );
    }
}

fn requireLocalMonotypeMatchesCallableInst(
    self: *Self,
    local_id: MIR.LocalId,
    callable_inst_id: Pipeline.CallableInstId,
    context: []const u8,
) Allocator.Error!void {
    const runtime_monotype = self.callable_pipeline.getCallableInstRuntimeMonotype(callable_inst_id);
    const expected_monotype = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        runtime_monotype.idx,
        runtime_monotype.module_idx,
        self.current_module_idx,
    );
    const actual_monotype = self.store.getLocal(local_id).monotype;
    if (builtin.mode == .Debug and !try self.monotypesStructurallyEqual(actual_monotype, expected_monotype)) {
        std.debug.panic(
            "statement-only MIR invariant violated: local {d} monotype {d} did not match callable inst {d} executable runtime monotype {d} in {s}",
            .{
                @intFromEnum(local_id),
                @intFromEnum(actual_monotype),
                @intFromEnum(callable_inst_id),
                @intFromEnum(expected_monotype),
                context,
            },
        );
    }
}

fn packedCallableTagName(
    self: *Self,
    packed_fn: Pipeline.PackedFn,
    callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!Monotype.Name {
    const source_name = self.callable_pipeline.getPackedFnTagName(self.all_module_envs, packed_fn, callable_inst_id);
    return self.remapMonotypeNameBetweenModules(
        source_name,
        source_name.module_idx,
        self.current_module_idx,
    );
}

fn lowerPackedCallableIntroInto(
    self: *Self,
    packed_fn: Pipeline.PackedFn,
    callable_inst_id: Pipeline.CallableInstId,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const tag_name = try self.packedCallableTagName(packed_fn, callable_inst_id);
    const payload_resolved = self.callable_pipeline.getCallableInstRuntimeMonotype(callable_inst_id);
    const payload_mono = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        payload_resolved.idx,
        payload_resolved.module_idx,
        self.current_module_idx,
    );

    if (payload_mono == self.store.monotype_store.unit_idx) {
        return self.store.addCFStmt(self.allocator, .{ .assign_tag = .{
            .target = target,
            .name = tag_name,
            .args = MIR.LocalSpan.empty(),
            .next = next,
        } });
    }

    const payload_local = try self.freshSyntheticLocal(payload_mono, false);
    const tag_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_tag = .{
        .target = target,
        .name = tag_name,
        .args = try self.store.addLocalSpan(self.allocator, &.{payload_local}),
        .next = next,
    } });
    return self.lowerResolvedCallableInstValueInto(callable_inst_id, payload_local, tag_stmt);
}

fn lowerExprAsPackedCallableInto(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    packed_fn: Pipeline.PackedFn,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const callable_value = lookupProgramExprCallableValue(session, expr_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: expr {d} was required to lower as packed callable but had no callable-value semantics",
        .{@intFromEnum(expr_idx)},
    );

    return switch (callable_value) {
        .direct => |callable_inst_id| self.lowerPackedCallableIntroInto(
            packed_fn,
            callable_inst_id,
            target,
            next,
        ),
        .packed_fn => |existing_packed_fn| blk: {
            if (builtin.mode == .Debug and !std.meta.eql(existing_packed_fn, packed_fn)) {
                std.debug.panic(
                    "statement-only MIR invariant violated: expr {d} packed callable semantics mismatched required indirect-call shape",
                    .{@intFromEnum(expr_idx)},
                );
            }
            break :blk self.lowerCirExprInto(session, expr_idx, target, next);
        },
    };
}

fn lowerPackedCallablePayloadInto(
    self: *Self,
    packed_fn: Pipeline.PackedFn,
    callable_inst_id: Pipeline.CallableInstId,
    packed_value_local: MIR.LocalId,
    payload_local: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const payload_mono = self.store.getLocal(payload_local).monotype;
    if (payload_mono == self.store.monotype_store.unit_idx) {
        return self.lowerUnitLocalInto(payload_local, next);
    }

    const tag_name = try self.packedCallableTagName(packed_fn, callable_inst_id);
    return self.lowerRefInto(payload_local, .{ .tag_payload = .{
        .source = packed_value_local,
        .payload_idx = 0,
        .tag_discriminant = @intCast(self.tagDiscriminantForMonotypeName(
            self.store.getLocal(packed_value_local).monotype,
            tag_name,
        )),
        .ownership = .move,
    } }, next);
}

fn resolveCallResultCallableFromExpr(
    self: *Self,
    session: LowerSession,
    result_expr_idx: CIR.Expr.Idx,
    local_id: MIR.LocalId,
) Allocator.Error!?CallableResolution {
    _ = local_id;
    if (lookupProgramExprCallableValue(session, result_expr_idx)) |callable_value| {
        return self.callableResolutionFromValue(callable_value);
    }
    return null;
}

fn callableResolutionFromInst(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!CallableResolution {
    return .{
        .lambda = try self.lowerResolvedCallableInstLambda(callable_inst_id),
        .captures_local = self.callableInstRequiresHiddenCapture(callable_inst_id),
    };
}

fn callableResolutionFromValue(
    self: *Self,
    callable_value: Pipeline.CallableValue,
) Allocator.Error!?CallableResolution {
    return switch (callable_value) {
        .direct => |callable_inst_id| try self.callableResolutionFromInst(callable_inst_id),
        .packed_fn => null,
    };
}

fn callableResolutionForPattern(
    self: *Self,
    session: LowerSession,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!?CallableResolution {
    const callable_value = try resolveProgramPatternCallableValue(session, pattern_idx) orelse return null;
    return self.callableResolutionFromValue(callable_value);
}

fn callableResolutionForExpr(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!?CallableResolution {
    const callable_value = lookupProgramExprCallableValue(session, expr_idx) orelse return null;
    return self.callableResolutionFromValue(callable_value);
}

fn lowerDirectCallTargetInto(
    self: *Self,
    session: LowerSession,
    callee_expr_idx: CIR.Expr.Idx,
    direct_callable_inst_id: Pipeline.CallableInstId,
    lowered_callee_local: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!struct { entry: MIR.CFStmtId, callee_local: MIR.LocalId } {
    const callable_value = lookupProgramExprCallableValue(session, callee_expr_idx) orelse std.debug.panic(
        "statement-only MIR invariant violated: direct call callee expr {d} had no callable value semantics",
        .{@intFromEnum(callee_expr_idx)},
    );

    return switch (callable_value) {
        .direct => .{
            .entry = if (self.callableInstRequiresHiddenCapture(direct_callable_inst_id))
                next
            else
                try self.lowerResolvedCallableInstValueInto(
                    direct_callable_inst_id,
                    lowered_callee_local,
                    next,
                ),
            .callee_local = lowered_callee_local,
        },
        .packed_fn => |packed_fn| blk: {
            const payload_resolved = self.callable_pipeline.getCallableInstRuntimeMonotype(direct_callable_inst_id);
            const payload_mono = try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                payload_resolved.idx,
                payload_resolved.module_idx,
                self.current_module_idx,
            );
            const payload_local = try self.freshSyntheticLocal(payload_mono, false);
            break :blk .{
                .entry = try self.lowerPackedCallablePayloadInto(
                    packed_fn,
                    direct_callable_inst_id,
                    lowered_callee_local,
                    payload_local,
                    next,
                ),
                .callee_local = payload_local,
            };
        },
    };
}

fn callableInstRequiresHiddenCapture(
    self: *const Self,
    callable_inst_id: Pipeline.CallableInstId,
) bool {
    return switch (self.callable_pipeline.getCallableInst(callable_inst_id).runtime_value) {
        .direct_lambda => false,
        .closure => true,
    };
}

fn cirExprKnownBoolValue(self: *const Self, module_idx: u32, expr_idx: CIR.Expr.Idx) ?bool {
    const module_env = self.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_tag => |tag| blk: {
            if (module_env.store.sliceExpr(tag.args).len != 0) break :blk null;
            if (self.identsTagNameEquivalent(tag.name, module_env.idents.true_tag)) break :blk true;
            if (self.identsTagNameEquivalent(tag.name, module_env.idents.false_tag)) break :blk false;
            break :blk null;
        },
        else => null,
    };
}

fn setBindingLocalsReassignable(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    reassignable: bool,
) void {
    switch (module_env.store.getPattern(pattern_idx)) {
        .assign, .as => {
            const local = self.patternToLocal(session, pattern_idx) catch unreachable;
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
        .nominal => |nom| self.setBindingLocalsReassignable(session, module_env, nom.backing_pattern, reassignable),
        .nominal_external => |nom| self.setBindingLocalsReassignable(session, module_env, nom.backing_pattern, reassignable),
        .applied_tag => |tag| {
            for (module_env.store.slicePatterns(tag.args)) |arg_pattern| {
                self.setBindingLocalsReassignable(session, module_env, arg_pattern, reassignable);
            }
        },
        .record_destructure => |record_pat| {
            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                self.setBindingLocalsReassignable(session, module_env, destruct.kind.toPatternIdx(), reassignable);
            }
        },
        .tuple => |tuple_pat| {
            for (module_env.store.slicePatterns(tuple_pat.patterns)) |elem_pattern| {
                self.setBindingLocalsReassignable(session, module_env, elem_pattern, reassignable);
            }
        },
        .list => |list_pat| {
            for (module_env.store.slicePatterns(list_pat.patterns)) |elem_pattern| {
                self.setBindingLocalsReassignable(session, module_env, elem_pattern, reassignable);
            }
            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern| {
                    self.setBindingLocalsReassignable(session, module_env, rest_pattern, reassignable);
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

fn commonFieldName(self: *Self, text: []const u8) Allocator.Error!Monotype.Name {
    const module_env = self.all_module_envs[self.current_module_idx];
    const ident = module_env.common.findIdent(text) orelse
        try module_env.common.insertIdent(self.allocator, Ident.for_text(text));
    return .{
        .module_idx = self.current_module_idx,
        .ident = ident,
    };
}

fn listSublistBoundsMonotype(self: *Self) Allocator.Error!Monotype.Idx {
    const u64_mono = self.store.monotype_store.primIdx(.u64);
    const fields = [_]Monotype.Field{
        .{ .name = try self.commonFieldName("len"), .type_idx = u64_mono },
        .{ .name = try self.commonFieldName("start"), .type_idx = u64_mono },
    };
    const field_span = try self.store.monotype_store.addFields(self.allocator, &fields);
    return try self.store.monotype_store.addMonotype(self.allocator, .{ .record = .{ .fields = field_span } });
}

fn lowerListSublistInto(
    self: *Self,
    target: MIR.LocalId,
    source_local: MIR.LocalId,
    start_local: MIR.LocalId,
    len_local: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    switch (self.store.monotype_store.getMonotype(self.store.getLocal(source_local).monotype)) {
        .list => {},
        else => std.debug.panic(
            "statement-only MIR list_sublist lowering expected list source monotype",
            .{},
        ),
    }

    const bounds_local = try self.freshSyntheticLocal(try self.listSublistBoundsMonotype(), false);
    const call_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = target,
        .op = .list_sublist,
        .args = try self.store.addLocalSpan(self.allocator, &.{ source_local, bounds_local }),
        .next = next,
    } });
    return self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
        .target = bounds_local,
        .fields = try self.store.addLocalSpan(self.allocator, &.{ len_local, start_local }),
        .next = call_stmt,
    } });
}

fn lowerListPatternMatchLocalInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    list_pat: anytype,
    source_local: MIR.LocalId,
    source_expr_ref: ?LowerExprSourceRef,
    on_match: MIR.CFStmtId,
    on_fail: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const source_mono = self.store.getLocal(source_local).monotype;
    const list_data = switch (self.store.monotype_store.getMonotype(source_mono)) {
        .list => |list| list,
        else => std.debug.panic(
            "statement-only MIR list pattern expected list source monotype",
            .{},
        ),
    };

    const elem_patterns = module_env.store.slicePatterns(list_pat.patterns);
    const prefix_len: usize = if (list_pat.rest_info) |rest| @intCast(rest.index) else elem_patterns.len;
    if (builtin.mode == .Debug and prefix_len > elem_patterns.len) {
        std.debug.panic(
            "statement-only MIR list pattern rest index {d} exceeds pattern len {d}",
            .{ prefix_len, elem_patterns.len },
        );
    }
    const suffix_len = elem_patterns.len - prefix_len;

    const u64_mono = self.store.monotype_store.primIdx(.u64);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );

    const len_local = try self.freshSyntheticLocal(u64_mono, false);
    const required_len_local = try self.freshSyntheticLocal(u64_mono, false);
    const cond_local = try self.freshSyntheticLocal(bool_mono, false);

    var body = on_match;

    if (list_pat.rest_info) |rest| {
        if (rest.pattern) |rest_pattern_idx| {
            if (prefix_len == 0 and suffix_len == 0) {
                body = try self.lowerPatternMatchLocalInto(
                    session,
                    module_env,
                    rest_pattern_idx,
                    source_local,
                    source_expr_ref,
                    body,
                    on_fail,
                );
            } else {
                const rest_local = try self.freshSyntheticLocal(source_mono, false);
                const rest_start_local = try self.freshSyntheticLocal(u64_mono, false);
                const rest_len_local = try self.freshSyntheticLocal(u64_mono, false);

                body = try self.lowerPatternMatchLocalInto(
                    session,
                    module_env,
                    rest_pattern_idx,
                    rest_local,
                    null,
                    body,
                    on_fail,
                );
                body = try self.lowerListSublistInto(rest_local, source_local, rest_start_local, rest_len_local, body);
                body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
                    .target = rest_len_local,
                    .op = .num_minus,
                    .args = try self.store.addLocalSpan(self.allocator, &.{ len_local, required_len_local }),
                    .next = body,
                } });
                body = try self.lowerU64LiteralInto(rest_start_local, @intCast(prefix_len), body);
            }
        }
    }

    var suffix_i = suffix_len;
    while (suffix_i > 0) {
        suffix_i -= 1;
        const pattern_idx = elem_patterns[prefix_len + suffix_i];
        const elem_local = try self.freshSyntheticLocal(
            try self.resolvePatternExecutableMonotype(session, pattern_idx, list_data.elem),
            false,
        );
        const index_local = try self.freshSyntheticLocal(u64_mono, false);
        const offset_local = try self.freshSyntheticLocal(u64_mono, false);
        const offset_from_end: u64 = @intCast(suffix_len - suffix_i);
        const elem_source_expr_ref = if (source_expr_ref) |source|
            try self.extendLowerExprSourceRef(
                source,
                .{ .list_elem = @intCast(prefix_len + suffix_i) },
            )
        else
            null;

        body = try self.lowerPatternMatchLocalInto(
            session,
            module_env,
            pattern_idx,
            elem_local,
            elem_source_expr_ref,
            body,
            on_fail,
        );
                body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
                    .target = elem_local,
                    .op = .list_get_unsafe,
                    .result_callable = try self.callableResolutionForPattern(session, pattern_idx),
                    .args = try self.store.addLocalSpan(self.allocator, &.{ source_local, index_local }),
                    .next = body,
                } });
        body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
            .target = index_local,
            .op = .num_minus,
            .args = try self.store.addLocalSpan(self.allocator, &.{ len_local, offset_local }),
            .next = body,
        } });
        body = try self.lowerU64LiteralInto(offset_local, offset_from_end, body);
    }

    var prefix_i = prefix_len;
    while (prefix_i > 0) {
        prefix_i -= 1;
        const pattern_idx = elem_patterns[prefix_i];
        const elem_local = try self.freshSyntheticLocal(
            try self.resolvePatternExecutableMonotype(session, pattern_idx, list_data.elem),
            false,
        );
        const index_local = try self.freshSyntheticLocal(u64_mono, false);
        const elem_source_expr_ref = if (source_expr_ref) |source|
            try self.extendLowerExprSourceRef(source, .{ .list_elem = @intCast(prefix_i) })
        else
            null;

        body = try self.lowerPatternMatchLocalInto(
            session,
            module_env,
            pattern_idx,
            elem_local,
            elem_source_expr_ref,
            body,
            on_fail,
        );
        body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
            .target = elem_local,
            .op = .list_get_unsafe,
            .result_callable = try self.callableResolutionForPattern(session, pattern_idx),
            .args = try self.store.addLocalSpan(self.allocator, &.{ source_local, index_local }),
            .next = body,
        } });
        body = try self.lowerU64LiteralInto(index_local, @intCast(prefix_i), body);
    }

    const tag_names = boolTagNamesForMonotype(self, bool_mono) orelse std.debug.panic(
        "statement-only MIR list pattern lowering expected Bool monotype",
        .{},
    );
    const switch_stmt = try self.lowerSwitchOnDiscriminant(
        cond_local,
        bool_mono,
        tag_names.true_name,
        body,
        on_fail,
    );
    const cond_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = cond_local,
        .op = if (list_pat.rest_info == null) .num_is_eq else .num_is_gte,
        .args = try self.store.addLocalSpan(self.allocator, &.{ len_local, required_len_local }),
        .next = switch_stmt,
    } });
    const required_stmt = try self.lowerU64LiteralInto(required_len_local, @intCast(elem_patterns.len), cond_stmt);
    return self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = len_local,
        .op = .list_len,
        .args = try self.store.addLocalSpan(self.allocator, &.{source_local}),
        .next = required_stmt,
    } });
}

fn lowerBindingInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    mark_reassignable: bool,
    rhs_binding_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const continuation_scope = self.current_binding_scope;
    var bindings = std.ArrayList(BindingNode).empty;
    defer bindings.deinit(self.allocator);
    try self.collectBindingNodes(module_env, pattern_idx, &bindings);
    const excluded_patterns = try self.allocator.alloc(CIR.Pattern.Idx, bindings.items.len);
    defer self.allocator.free(excluded_patterns);
    for (bindings.items, 0..) |binding, i| {
        excluded_patterns[i] = binding.pattern_idx;
    }

    if (builtin.mode == .Debug) {
        if (self.current_lambda_entry_bound_locals) |entry_bound_locals| {
            if (self.lookupExistingBindingLocalInScope(
                self.current_module_idx,
                self.current_binding_scope,
                pattern_idx,
            )) |existing_local| {
                if (entry_bound_locals.contains(existing_local)) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: block binding pattern {d} in scope {d} collided with entry-bound local {d} (expr={d}, expr_tag={s}, callable_inst={d})",
                        .{
                            @intFromEnum(pattern_idx),
                            self.current_binding_scope,
                            @intFromEnum(existing_local),
                            @intFromEnum(expr_idx),
                            @tagName(module_env.store.getExpr(expr_idx)),
                            session.callableInstRawForDebug(),
                        },
                    );
                }
            }
        }
    }
    const source_mono = try self.resolveMonotype(session, expr_idx);
    const source_expr_ref = self.exprSourceRefAliasOrSelf(session, expr_idx);

    const source_local = if (continuation_scope == rhs_binding_scope)
        if (self.directBindingTargetLocal(module_env, pattern_idx)) |existing_local|
            existing_local
        else
            try self.freshSyntheticLocal(source_mono, false)
    else
        try self.freshSyntheticLocal(source_mono, false);
    if (mark_reassignable) {
        self.setBindingLocalsReassignable(session, module_env, pattern_idx, true);
    }
    if (continuation_scope == rhs_binding_scope) {
        const bound = try self.lowerBindingLocalInto(
            session,
            module_env,
            pattern_idx,
            source_local,
            source_expr_ref,
            next,
        );
        self.current_binding_scope = rhs_binding_scope;
        return self.lowerCirExprInto(session, expr_idx, source_local, bound);
    }

    const joined_source_local = try self.freshSyntheticLocal(source_mono, false);
    const after_expr_join_id = self.freshJoinPointId();
    const after_expr_jump = try self.store.reserveCFStmt(self.allocator);
    self.current_binding_scope = rhs_binding_scope;
    const lowered = try self.lowerExprWithExitScope(session, expr_idx, source_local, after_expr_jump);
    if (!lowered.falls_through) {
        return lowered.entry;
    }
    try self.store.finalizeCFStmt(after_expr_jump, .{ .jump = .{
        .id = after_expr_join_id,
        .args = try self.buildCarriedJumpArgsWithValueExcluding(
            lowered.exit_scope,
            continuation_scope,
            joined_source_local,
            source_local,
            excluded_patterns,
        ),
    } });

    const saved_scope = self.current_binding_scope;
    self.current_binding_scope = continuation_scope;
    const bound = try self.lowerBindingLocalInto(
        session,
        module_env,
        pattern_idx,
        joined_source_local,
        source_expr_ref,
        next,
    );
    self.current_binding_scope = saved_scope;
    const join_stmt = try self.addJoinStmt(
        after_expr_join_id,
        try self.buildJoinParamSpanWithValueExcluding(
            joined_source_local,
            continuation_scope,
            excluded_patterns,
        ),
        bound,
        lowered.entry,
    );
    return join_stmt;
}

fn lowerExprIntoContinuationScope(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    expr_scope: u64,
    continuation_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const saved_scope = self.current_binding_scope;
    defer self.current_binding_scope = saved_scope;

    self.current_binding_scope = expr_scope;
    const after_expr_join_id = self.freshJoinPointId();
    const after_expr_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered = try self.lowerExprWithExitScope(session, expr_idx, target, after_expr_jump);
    if (!lowered.falls_through) {
        return lowered.entry;
    }
    try self.store.finalizeCFStmt(after_expr_jump, .{ .jump = .{
        .id = after_expr_join_id,
        .args = try self.buildCarriedJumpArgsExcluding(lowered.exit_scope, continuation_scope, excluded_patterns),
    } });

    return self.addJoinStmt(
        after_expr_join_id,
        try self.buildJoinParamSpanExcluding(continuation_scope, excluded_patterns),
        next,
        lowered.entry,
    );
}

fn lowerExprIntoContinuationScopeWithValue(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    joined_target: MIR.LocalId,
    expr_scope: u64,
    continuation_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const saved_scope = self.current_binding_scope;
    defer self.current_binding_scope = saved_scope;

    self.current_binding_scope = expr_scope;
    const after_expr_join_id = self.freshJoinPointId();
    const after_expr_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered = try self.lowerExprWithExitScope(session, expr_idx, target, after_expr_jump);
    if (!lowered.falls_through) {
        return lowered.entry;
    }
    try self.store.finalizeCFStmt(after_expr_jump, .{ .jump = .{
        .id = after_expr_join_id,
        .args = try self.buildCarriedJumpArgsWithValueExcluding(
            lowered.exit_scope,
            continuation_scope,
            joined_target,
            target,
            excluded_patterns,
        ),
    } });

    const join_stmt = try self.addJoinStmt(
        after_expr_join_id,
        try self.buildJoinParamSpanWithValueExcluding(
            joined_target,
            continuation_scope,
            excluded_patterns,
        ),
        next,
        lowered.entry,
    );
    return join_stmt;
}

fn directBindingTargetLocal(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign, .as => self.lookupExistingBindingLocal(pattern_idx),
        .nominal => |nom| self.directBindingTargetLocal(module_env, nom.backing_pattern),
        .nominal_external => |nom| self.directBindingTargetLocal(module_env, nom.backing_pattern),
        else => null,
    };
}

fn lowerPatternMatchLocalInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_local: MIR.LocalId,
    source_expr_ref: ?LowerExprSourceRef,
    on_match: MIR.CFStmtId,
    on_fail: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const source_mono = self.store.getLocal(source_local).monotype;
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign => {
            if (source_expr_ref) |source| {
                try self.bindBindingSourceExprRefInCurrentScope(pattern_idx, source);
            }
            const local = try self.patternToLocal(session, pattern_idx);
            if (local == source_local) return on_match;
            return self.lowerLocalAliasInto(local, source_local, on_match);
        },
        .underscore => on_match,
        .as => |as_pattern| blk: {
            if (source_expr_ref) |source| {
                try self.bindBindingSourceExprRefInCurrentScope(pattern_idx, source);
            }
            const local = try self.patternToLocal(session, pattern_idx);
            const alias_stmt = if (local == source_local)
                on_match
            else
                try self.lowerLocalAliasInto(local, source_local, on_match);
            break :blk try self.lowerPatternMatchLocalInto(
                session,
                module_env,
                as_pattern.pattern,
                source_local,
                source_expr_ref,
                alias_stmt,
                on_fail,
            );
        },
        .nominal => |nom| self.lowerPatternMatchLocalInto(session, module_env, nom.backing_pattern, source_local, source_expr_ref, on_match, on_fail),
        .nominal_external => |nom| self.lowerPatternMatchLocalInto(session, module_env, nom.backing_pattern, source_local, source_expr_ref, on_match, on_fail),
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
            const tag_discriminant = self.tagDiscriminantForMonotypeName(
                source_mono,
                self.resolveTagNameForMonotype(source_mono, tag.name),
            );
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
                const payload_local = try self.freshSyntheticLocal(
                    try self.resolvePatternExecutableMonotype(session, payload_patterns[i], payload_monos[i]),
                    false,
                );
                const payload_source_expr_ref = if (source_expr_ref) |source|
                    try self.extendLowerExprSourceRef(source, .{ .tag_payload = .{
                        .tag_name = .{
                            .module_idx = self.current_module_idx,
                            .ident = tag.name,
                        },
                        .payload_index = @intCast(i),
                    } })
                else
                    null;
                try self.bindCallableFactsForBindingLocal(session, payload_patterns[i], payload_local, payload_source_expr_ref);
                body = try self.lowerPatternMatchLocalInto(
                    session,
                    module_env,
                    payload_patterns[i],
                    payload_local,
                    payload_source_expr_ref,
                    body,
                    on_fail,
                );
                body = try self.lowerRefIntoWithCallable(
                    payload_local,
                    .{ .tag_payload = .{
                        .source = source_local,
                        .payload_idx = @intCast(i),
                        .tag_discriminant = @intCast(tag_discriminant),
                        .ownership = .move,
                    } },
                    try self.callableResolutionForPattern(session, payload_patterns[i]),
                    body,
                );
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
            const cir_destructs = module_env.store.sliceRecordDestructs(record_pat.destructs);
            switch (self.store.monotype_store.getMonotype(source_mono)) {
                .record => |record_mono| {
                    const mono_fields = self.store.monotype_store.getFields(record_mono.fields);

                    var body = on_match;
                    var i = cir_destructs.len;
                    while (i > 0) {
                        i -= 1;
                        const destruct = module_env.store.getRecordDestruct(cir_destructs[i]);
                        const field_idx = self.recordFieldIndexByName(destruct.label, mono_fields);
                        const field_local = try self.freshSyntheticLocal(
                            try self.resolvePatternExecutableMonotype(
                                session,
                                destruct.kind.toPatternIdx(),
                                mono_fields[field_idx].type_idx,
                            ),
                            false,
                        );
                        const field_source_expr_ref = if (source_expr_ref) |source|
                            try self.extendLowerExprSourceRef(source, .{ .field = .{
                                .module_idx = self.current_module_idx,
                                .ident = destruct.label,
                            } })
                        else
                            null;
                        try self.bindCallableFactsForBindingLocal(
                            session,
                            destruct.kind.toPatternIdx(),
                            field_local,
                            field_source_expr_ref,
                        );
                        body = try self.lowerPatternMatchLocalInto(
                            session,
                            module_env,
                            destruct.kind.toPatternIdx(),
                            field_local,
                            field_source_expr_ref,
                            body,
                            on_fail,
                        );
                        body = try self.lowerRefIntoWithCallable(
                            field_local,
                            .{ .field = .{
                                .source = source_local,
                                .field_idx = @intCast(field_idx),
                                .ownership = .move,
                            } },
                            try self.callableResolutionForPattern(session, destruct.kind.toPatternIdx()),
                            body,
                        );
                    }
                    break :blk body;
                },
                .unit => {
                    var body = on_match;
                    var i = cir_destructs.len;
                    while (i > 0) {
                        i -= 1;
                        const destruct = module_env.store.getRecordDestruct(cir_destructs[i]);
                        const destruct_pattern_idx = destruct.kind.toPatternIdx();
                        const field_local = try self.freshSyntheticLocal(self.store.monotype_store.unit_idx, false);
                        body = try self.lowerPatternMatchLocalInto(
                            session,
                            module_env,
                            destruct_pattern_idx,
                            field_local,
                            null,
                            body,
                            on_fail,
                        );
                        body = try self.lowerUnitLocalInto(field_local, body);
                    }
                    break :blk body;
                },
                else => std.debug.panic(
                    "statement-only MIR record pattern expected record/unit source monotype",
                    .{},
                ),
            }
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
                const elem_local = try self.freshSyntheticLocal(
                    try self.resolvePatternExecutableMonotype(session, elem_patterns[i], elem_monos[i]),
                    false,
                );
                const elem_source_expr_ref = if (source_expr_ref) |source|
                    try self.extendLowerExprSourceRef(source, .{ .tuple_elem = @intCast(i) })
                else
                    null;
                try self.bindCallableFactsForBindingLocal(session, elem_patterns[i], elem_local, elem_source_expr_ref);
                body = try self.lowerPatternMatchLocalInto(
                    session,
                    module_env,
                    elem_patterns[i],
                    elem_local,
                    elem_source_expr_ref,
                    body,
                    on_fail,
                );
                body = try self.lowerRefIntoWithCallable(
                    elem_local,
                    .{ .field = .{
                        .source = source_local,
                        .field_idx = @intCast(i),
                        .ownership = .move,
                    } },
                    try self.callableResolutionForPattern(session, elem_patterns[i]),
                    body,
                );
            }
            break :blk body;
        },
        .list => |list_pat| self.lowerListPatternMatchLocalInto(
            session,
            module_env,
            list_pat,
            source_local,
            source_expr_ref,
            on_match,
            on_fail,
        ),
        .runtime_error => std.debug.panic(
            "statement-only MIR invariant violated: runtime-error pattern reached pattern-free MIR lowering",
            .{},
        ),
    };
}

fn patternIsIrrefutableForMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_mono: Monotype.Idx,
) bool {
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign, .underscore => true,
        .as => |as_pattern| self.patternIsIrrefutableForMonotype(module_env, as_pattern.pattern, source_mono),
        .nominal => |nom| self.patternIsIrrefutableForMonotype(module_env, nom.backing_pattern, source_mono),
        .nominal_external => |nom| self.patternIsIrrefutableForMonotype(module_env, nom.backing_pattern, source_mono),
        .num_literal,
        .str_literal,
        .dec_literal,
        .small_dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => false,
        .record_destructure => |record_pat| switch (self.store.monotype_store.getMonotype(source_mono)) {
            .record => |record_mono| blk: {
                const mono_fields = self.store.monotype_store.getFields(record_mono.fields);
                for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                    const destruct = module_env.store.getRecordDestruct(destruct_idx);
                    const field_idx = self.recordFieldIndexByName(destruct.label, mono_fields);
                    if (!self.patternIsIrrefutableForMonotype(
                        module_env,
                        destruct.kind.toPatternIdx(),
                        mono_fields[field_idx].type_idx,
                    )) break :blk false;
                }
                break :blk true;
            },
            .unit => blk: {
                for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                    const destruct = module_env.store.getRecordDestruct(destruct_idx);
                    if (!self.patternIsIrrefutableForMonotype(
                        module_env,
                        destruct.kind.toPatternIdx(),
                        self.store.monotype_store.unit_idx,
                    )) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        .tuple => |tuple_pat| switch (self.store.monotype_store.getMonotype(source_mono)) {
            .tuple => |tuple_mono| blk: {
                const elem_patterns = module_env.store.slicePatterns(tuple_pat.patterns);
                const elem_monos = self.store.monotype_store.getIdxSpan(tuple_mono.elems);
                if (elem_patterns.len != elem_monos.len) break :blk false;
                for (elem_patterns, elem_monos) |elem_pattern_idx, elem_mono| {
                    if (!self.patternIsIrrefutableForMonotype(module_env, elem_pattern_idx, elem_mono)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        .applied_tag => |tag_pat| switch (self.store.monotype_store.getMonotype(source_mono)) {
            .tag_union => |tag_union_mono| blk: {
                const tags = self.store.monotype_store.getTags(tag_union_mono.tags);
                if (tags.len != 1) break :blk false;
                if (!self.identsTagNameEquivalent(tags[0].name.ident, tag_pat.name)) break :blk false;
                const payload_patterns = module_env.store.slicePatterns(tag_pat.args);
                const payload_monos = self.store.monotype_store.getIdxSpan(tags[0].payloads);
                if (payload_patterns.len != payload_monos.len) break :blk false;
                for (payload_patterns, payload_monos) |payload_pattern_idx, payload_mono| {
                    if (!self.patternIsIrrefutableForMonotype(module_env, payload_pattern_idx, payload_mono)) break :blk false;
                }
                break :blk true;
            },
            else => false,
        },
        .list => |list_pat| blk: {
            const elem_patterns = module_env.store.slicePatterns(list_pat.patterns);
            const prefix_len: usize = if (list_pat.rest_info) |rest| @intCast(rest.index) else elem_patterns.len;
            if (prefix_len != 0 or elem_patterns.len != prefix_len) break :blk false;
            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    break :blk self.patternIsIrrefutableForMonotype(module_env, rest_pattern_idx, source_mono);
                }
            }
            break :blk true;
        },
    };
}

fn matchBranchIsIrrefutable(
    self: *Self,
    module_env: *const ModuleEnv,
    cir_branch: CIR.Expr.Match.Branch,
    cond_mono: Monotype.Idx,
) bool {
    if (cir_branch.guard != null) return false;
    const branch_pattern_indices = module_env.store.sliceMatchBranchPatterns(cir_branch.patterns);
    if (branch_pattern_indices.len == 0) return true;
    for (branch_pattern_indices) |branch_pattern_idx| {
        const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_idx);
        if (self.patternIsIrrefutableForMonotype(module_env, branch_pattern.pattern, cond_mono)) {
            return true;
        }
    }
    return false;
}

fn lowerBindingLocalInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_local: MIR.LocalId,
    source_expr_ref: ?LowerExprSourceRef,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });
    return self.lowerPatternMatchLocalInto(
        session,
        module_env,
        pattern_idx,
        source_local,
        source_expr_ref,
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
        .args = try self.buildCarriedJumpArgs(
            self.current_binding_scope,
            loop_ctx.exit_scope,
        ),
    } });
}

fn lowerWhileStmtInto(
    self: *Self,
    session: LowerSession,
    while_stmt: std.meta.TagPayload(CIR.Statement, .s_while),
    before_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const after_scope = self.current_binding_scope;
    const head_scope = self.freshChildBindingScope(before_scope);
    try self.predeclareVisibleReassignableBindingsInScope(before_scope, head_scope);

    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();

    try self.active_loops.append(self.allocator, .{
        .exit_id = loop_exit_id,
        .exit_scope = after_scope,
    });
    defer _ = self.active_loops.pop();

    const cond_mono = try self.resolveMonotype(session, while_stmt.cond);
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);

    self.current_binding_scope = head_scope;
    const saved_entry_bound_locals = self.current_lambda_entry_bound_locals;
    var loop_entry_bound_locals = try LambdaEntryBindingState.cloneFrom(self.allocator, saved_entry_bound_locals);
    defer loop_entry_bound_locals.deinit();
    self.current_lambda_entry_bound_locals = &loop_entry_bound_locals;
    defer self.current_lambda_entry_bound_locals = saved_entry_bound_locals;
    try self.markVisibleReassignableBindingsEntryBoundInScope(head_scope);
    const discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
    const switch_placeholder = try self.store.reserveCFStmt(self.allocator);
    const discrim_stmt = try self.lowerRefInto(
        discrim_local,
        .{ .discriminant = .{ .source = cond_local } },
        switch_placeholder,
    );
    const lowered_cond = try self.lowerExprWithExitScope(session, while_stmt.cond, cond_local, discrim_stmt);

    self.current_binding_scope = lowered_cond.exit_scope;
    const continue_jump = try self.store.reserveCFStmt(self.allocator);
    const body_value = try self.freshSyntheticLocal(try self.resolveMonotype(session, while_stmt.body), false);
    const lowered_body = try self.lowerExprWithExitScope(session, while_stmt.body, body_value, continue_jump);
    try self.store.finalizeCFStmt(continue_jump, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.buildCarriedJumpArgs(
            lowered_body.exit_scope,
            head_scope,
        ),
    } });

    const false_body = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_exit_id,
        .args = try self.buildCarriedJumpArgs(lowered_cond.exit_scope, after_scope),
    } });

    const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
        "statement-only MIR while expected Bool condition monotype for expr {d}",
        .{@intFromEnum(while_stmt.cond)},
    );
    try self.store.finalizeCFStmt(switch_placeholder, .{ .switch_stmt = .{
        .scrutinee = discrim_local,
        .branches = try self.store.addSwitchBranches(self.allocator, &.{.{
            .value = self.tagDiscriminantForMonotypeName(cond_mono, tag_names.true_name),
            .body = lowered_body.entry,
        }}),
        .default_branch = false_body,
    } });

    const initial_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.buildCarriedJumpArgs(before_scope, head_scope),
    } });

    const head_join = try self.addJoinStmt(
        loop_head_id,
        try self.buildJoinParamSpan(head_scope),
        lowered_cond.entry,
        initial_jump,
    );

    return self.addJoinStmt(
        loop_exit_id,
        try self.buildJoinParamSpan(after_scope),
        next,
        head_join,
    );
}

fn lowerForStmtInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    for_stmt: std.meta.TagPayload(CIR.Statement, .s_for),
    before_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const after_scope = self.current_binding_scope;
    const head_scope = self.freshChildBindingScope(before_scope);
    try self.predeclareVisibleReassignableBindingsInScope(before_scope, head_scope);

    const list_mono = try self.resolveMonotype(session, for_stmt.expr);
    const item_mono = switch (self.store.monotype_store.getMonotype(list_mono)) {
        .list => |list| list.elem,
        else => std.debug.panic(
            "statement-only MIR for-loop expected list source monotype",
            .{},
        ),
    };
    const u64_mono = self.store.monotype_store.primIdx(.u64);
    const bool_mono = try self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );

    const init_list_local = try self.freshSyntheticLocal(list_mono, false);
    const init_len_local = try self.freshSyntheticLocal(u64_mono, false);
    const init_index_local = try self.freshSyntheticLocal(u64_mono, false);
    const head_list_local = try self.freshSyntheticLocal(list_mono, false);
    const head_len_local = try self.freshSyntheticLocal(u64_mono, false);
    const head_index_local = try self.freshSyntheticLocal(u64_mono, false);

    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();

    try self.active_loops.append(self.allocator, .{
        .exit_id = loop_exit_id,
        .exit_scope = after_scope,
    });
    defer _ = self.active_loops.pop();

    const false_body = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_exit_id,
        .args = try self.buildCarriedJumpArgs(head_scope, after_scope),
    } });

    const one_local = try self.freshSyntheticLocal(u64_mono, false);
    const next_index_local = try self.freshSyntheticLocal(u64_mono, false);
    const item_local = try self.freshSyntheticLocal(
        try self.resolvePatternExecutableMonotype(session, for_stmt.patt, item_mono),
        false,
    );
    const body_value = try self.freshSyntheticLocal(try self.resolveMonotype(session, for_stmt.body), false);
    const loop_back = try self.store.reserveCFStmt(self.allocator);
    const increment_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = next_index_local,
        .op = .num_plus,
        .args = try self.store.addLocalSpan(self.allocator, &.{ head_index_local, one_local }),
        .next = loop_back,
    } });
    self.current_binding_scope = head_scope;
    const saved_entry_bound_locals = self.current_lambda_entry_bound_locals;
    var loop_entry_bound_locals = try LambdaEntryBindingState.cloneFrom(self.allocator, saved_entry_bound_locals);
    defer loop_entry_bound_locals.deinit();
    self.current_lambda_entry_bound_locals = &loop_entry_bound_locals;
    defer self.current_lambda_entry_bound_locals = saved_entry_bound_locals;
    try self.markVisibleReassignableBindingsEntryBoundInScope(head_scope);
    const saved_prelude_bound_locals = self.current_prelude_bound_locals;
    var loop_prelude_bound_locals = try LambdaEntryBindingState.cloneFrom(self.allocator, saved_prelude_bound_locals);
    defer loop_prelude_bound_locals.deinit();
    self.current_prelude_bound_locals = &loop_prelude_bound_locals;
    defer self.current_prelude_bound_locals = saved_prelude_bound_locals;
    try self.predeclareBindingLocals(session, module_env, for_stmt.patt);
    try self.markBindingLocalsPreludeBound(module_env, for_stmt.patt);
    const lowered_body = try self.lowerExprWithExitScope(session, for_stmt.body, body_value, increment_stmt);
    try self.store.finalizeCFStmt(loop_back, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.buildCarriedJumpArgsWithExtraLocals(
            lowered_body.exit_scope,
            head_scope,
            &.{ head_list_local, head_len_local, next_index_local },
        ),
    } });
    const body_stmt = lowered_body.entry;
    const bind_item = try self.lowerBindingLocalInto(session, module_env, for_stmt.patt, item_local, null, body_stmt);
    const get_item = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = item_local,
        .op = .list_get_unsafe,
        .result_callable = try self.callableResolutionForPattern(session, for_stmt.patt),
        .args = try self.store.addLocalSpan(self.allocator, &.{ head_list_local, head_index_local }),
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
        .args = try self.store.addLocalSpan(self.allocator, &.{ head_index_local, head_len_local }),
        .next = switch_stmt,
    } });

    const initial_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.buildCarriedJumpArgsWithExtraLocals(
            before_scope,
            head_scope,
            &.{ init_list_local, init_len_local, init_index_local },
        ),
    } });
    const init_index = try self.store.addCFStmt(self.allocator, .{ .assign_literal = .{
        .target = init_index_local,
        .literal = .{ .int = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 } },
        .next = initial_jump,
    } });
    const init_len = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = init_len_local,
        .op = .list_len,
        .args = try self.store.addLocalSpan(self.allocator, &.{init_list_local}),
        .next = init_index,
    } });
    const saved_scope = self.current_binding_scope;
    self.current_binding_scope = before_scope;
    defer self.current_binding_scope = saved_scope;
    const init_list = try self.lowerCirExprInto(session, for_stmt.expr, init_list_local, init_len);

    const head_join = try self.addJoinStmt(
        loop_head_id,
        try self.buildJoinParamSpanWithExtraLocals(
            head_scope,
            &.{ head_list_local, head_len_local, head_index_local },
        ),
        cond_stmt,
        init_list,
    );

    return self.addJoinStmt(
        loop_exit_id,
        try self.buildJoinParamSpan(after_scope),
        next,
        head_join,
    );
}

fn lowerBlockStmtInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
    before_binding_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const stmt = module_env.store.getStatement(stmt_idx);

    return switch (stmt) {
        .s_decl => |decl| if (shouldOmitCallableBindingExpr(session, module_env, decl.expr))
            next
        else
            self.lowerBindingInto(
            session,
            module_env,
            decl.pattern,
            decl.expr,
            false,
            before_binding_scope,
            next,
        ),
        .s_var => |var_decl| if (shouldOmitCallableBindingExpr(session, module_env, var_decl.expr))
            next
        else
            self.lowerBindingInto(
            session,
            module_env,
            var_decl.pattern_idx,
            var_decl.expr,
            true,
            before_binding_scope,
            next,
        ),
        .s_reassign => |reassign| if (shouldOmitCallableBindingExpr(session, module_env, reassign.expr))
            next
        else
            self.lowerBindingInto(
            session,
            module_env,
            reassign.pattern_idx,
            reassign.expr,
            false,
            before_binding_scope,
            next,
        ),
        .s_expr => |expr_stmt| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, expr_stmt.expr), false);
            break :blk try self.lowerExprIntoContinuationScope(
                session,
                expr_stmt.expr,
                value_local,
                before_binding_scope,
                self.current_binding_scope,
                &.{},
                next,
            );
        },
        .s_dbg => |dbg_stmt| blk: {
            const value_mono = try self.resolveMonotype(session, dbg_stmt.expr);
            const value_local = try self.freshSyntheticLocal(value_mono, false);
            const joined_value_local = try self.freshSyntheticLocal(value_mono, false);
            const message_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.str), false);
            const debug_stmt = try self.store.addCFStmt(self.allocator, .{ .debug = .{
                .value = message_local,
                .next = next,
            } });
            const inspect_stmt = try self.lowerStrInspectLocalInto(joined_value_local, value_mono, message_local, debug_stmt);
            break :blk try self.lowerExprIntoContinuationScopeWithValue(
                session,
                dbg_stmt.expr,
                value_local,
                joined_value_local,
                before_binding_scope,
                self.current_binding_scope,
                &.{},
                inspect_stmt,
            );
        },
        .s_expect => |expect_stmt| blk: {
            const cond_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, expect_stmt.body), false);
            const joined_cond_local = try self.freshSyntheticLocal(self.store.getLocal(cond_local).monotype, false);
            const mir_expect = try self.store.addCFStmt(self.allocator, .{ .expect = .{
                .condition = joined_cond_local,
                .next = next,
            } });
            break :blk try self.lowerExprIntoContinuationScopeWithValue(
                session,
                expect_stmt.body,
                cond_local,
                joined_cond_local,
                before_binding_scope,
                self.current_binding_scope,
                &.{},
                mir_expect,
            );
        },
        .s_crash => |crash_stmt| blk: {
            const mir_str = try self.copyStringToMir(module_env, crash_stmt.msg);
            break :blk self.store.addCFStmt(self.allocator, .{ .crash = mir_str });
        },
        .s_return => |return_stmt| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, return_stmt.expr), false);
            const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = value_local } });
            const saved_scope = self.current_binding_scope;
            self.current_binding_scope = before_binding_scope;
            defer self.current_binding_scope = saved_scope;
            break :blk try self.lowerCirExprInto(session, return_stmt.expr, value_local, ret_stmt);
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
        .s_for => |s_for| self.lowerForStmtInto(session, module_env, s_for, before_binding_scope, next),
        .s_while => |s_while| self.lowerWhileStmtInto(session, s_while, before_binding_scope, next),
        .s_break => self.lowerLoopBreakJump(),
    };
}

fn predeclareTrivialBlockStmtPatterns(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
) Allocator.Error!void {
    switch (module_env.store.getStatement(stmt_idx)) {
        .s_decl => |decl| {
            if (shouldOmitCallableBindingExpr(session, module_env, decl.expr)) return;
            if (builtin.mode == .Debug) {
                const node_len = module_env.store.nodes.len();
                if (@intFromEnum(decl.pattern) >= node_len or @intFromEnum(decl.expr) >= node_len) {
                    std.debug.panic(
                        "predeclareTrivialBlockStmtPatterns(s_decl): out-of-range stmt payload module={d} stmt={d} pattern={d} expr={d} node_len={d} callable_inst={d} root_source_expr={d}",
                        .{
                            self.current_module_idx,
                            @intFromEnum(stmt_idx),
                            @intFromEnum(decl.pattern),
                            @intFromEnum(decl.expr),
                            node_len,
                            session.callableInstRawForDebug(),
                            session.rootExprRawForDebug(),
                        },
                    );
                }
            }
            const source_mono = try self.resolveMonotype(session, decl.expr);
            if (builtin.mode == .Debug) {
                try self.debugAssertNoUnitPrimBinding(session, module_env, decl.pattern, decl.expr, source_mono, stmt_idx, "s_decl");
            }
            try self.predeclareBindingLocalsFromMonotype(module_env, decl.pattern, source_mono, null);
        },
        .s_var => |var_decl| {
            if (shouldOmitCallableBindingExpr(session, module_env, var_decl.expr)) return;
            if (builtin.mode == .Debug) {
                const node_len = module_env.store.nodes.len();
                if (@intFromEnum(var_decl.pattern_idx) >= node_len or @intFromEnum(var_decl.expr) >= node_len) {
                    std.debug.panic(
                        "predeclareTrivialBlockStmtPatterns(s_var): out-of-range stmt payload module={d} stmt={d} pattern={d} expr={d} node_len={d} callable_inst={d} root_source_expr={d}",
                        .{
                            self.current_module_idx,
                            @intFromEnum(stmt_idx),
                            @intFromEnum(var_decl.pattern_idx),
                            @intFromEnum(var_decl.expr),
                            node_len,
                            session.callableInstRawForDebug(),
                            session.rootExprRawForDebug(),
                        },
                    );
                }
            }
            const source_mono = try self.resolveMonotype(session, var_decl.expr);
            if (builtin.mode == .Debug) {
                try self.debugAssertNoUnitPrimBinding(session, module_env, var_decl.pattern_idx, var_decl.expr, source_mono, stmt_idx, "s_var");
            }
            try self.predeclareBindingLocalsFromMonotype(module_env, var_decl.pattern_idx, source_mono, null);
            self.setBindingLocalsReassignable(session, module_env, var_decl.pattern_idx, true);
        },
        .s_reassign => |reassign| {
            if (shouldOmitCallableBindingExpr(session, module_env, reassign.expr)) return;
            if (builtin.mode == .Debug) {
                const node_len = module_env.store.nodes.len();
                if (@intFromEnum(reassign.pattern_idx) >= node_len or @intFromEnum(reassign.expr) >= node_len) {
                    std.debug.panic(
                        "predeclareTrivialBlockStmtPatterns(s_reassign): out-of-range stmt payload module={d} stmt={d} pattern={d} expr={d} node_len={d} callable_inst={d} root_source_expr={d}",
                        .{
                            self.current_module_idx,
                            @intFromEnum(stmt_idx),
                            @intFromEnum(reassign.pattern_idx),
                            @intFromEnum(reassign.expr),
                            node_len,
                            session.callableInstRawForDebug(),
                            session.rootExprRawForDebug(),
                        },
                    );
                }
            }
            const source_mono = try self.resolveMonotype(session, reassign.expr);
            if (builtin.mode == .Debug) {
                try self.debugAssertNoUnitPrimBinding(session, module_env, reassign.pattern_idx, reassign.expr, source_mono, stmt_idx, "s_reassign");
            }
            try self.predeclareBindingLocalsFromMonotype(module_env, reassign.pattern_idx, source_mono, null);
            self.setBindingLocalsReassignable(session, module_env, reassign.pattern_idx, true);
        },
        else => {},
    }
}

fn predeclareReassignableBindingsForStmtScope(
    self: *Self,
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
    before_scope: u64,
    after_scope: u64,
) Allocator.Error!void {
    switch (module_env.store.getStatement(stmt_idx)) {
        .s_decl => |decl| {
            var bindings = std.ArrayList(BindingNode).empty;
            defer bindings.deinit(self.allocator);
            try self.collectBindingNodes(module_env, decl.pattern, &bindings);

            const excluded_patterns = try self.allocator.alloc(CIR.Pattern.Idx, bindings.items.len);
            defer self.allocator.free(excluded_patterns);
            for (bindings.items, 0..) |binding, i| {
                excluded_patterns[i] = binding.pattern_idx;
            }

            try self.predeclareVisibleReassignableBindingsInScopeExcluding(
                before_scope,
                after_scope,
                excluded_patterns,
            );
        },
        .s_var => |var_decl| {
            var bindings = std.ArrayList(BindingNode).empty;
            defer bindings.deinit(self.allocator);
            try self.collectBindingNodes(module_env, var_decl.pattern_idx, &bindings);

            const excluded_patterns = try self.allocator.alloc(CIR.Pattern.Idx, bindings.items.len);
            defer self.allocator.free(excluded_patterns);
            for (bindings.items, 0..) |binding, i| {
                excluded_patterns[i] = binding.pattern_idx;
            }

            try self.predeclareVisibleReassignableBindingsInScopeExcluding(
                before_scope,
                after_scope,
                excluded_patterns,
            );
        },
        .s_reassign => |reassign| {
            var bindings = std.ArrayList(BindingNode).empty;
            defer bindings.deinit(self.allocator);
            try self.collectBindingNodes(module_env, reassign.pattern_idx, &bindings);

            const excluded_patterns = try self.allocator.alloc(CIR.Pattern.Idx, bindings.items.len);
            defer self.allocator.free(excluded_patterns);
            for (bindings.items, 0..) |binding, i| {
                excluded_patterns[i] = binding.pattern_idx;
            }

            try self.predeclareVisibleReassignableBindingsInScopeExcluding(
                before_scope,
                after_scope,
                excluded_patterns,
            );
        },
        .s_expr, .s_dbg, .s_expect, .s_for, .s_while => try self.predeclareVisibleReassignableBindingsInScope(
            before_scope,
            after_scope,
        ),
        else => {},
    }
}

fn debugAssertNoUnitPrimBinding(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    source_mono: Monotype.Idx,
    stmt_idx: CIR.Statement.Idx,
    stmt_kind: []const u8,
) Allocator.Error!void {
    if (self.store.monotype_store.getMonotype(source_mono) != .unit) return;

    const resolved = self.types_store.resolveVar(ModuleEnv.varFrom(pattern_idx));
    const requires_prim = switch (resolved.desc.content) {
        .structure => |flat_type| switch (flat_type) {
            .nominal_type => |nominal| blk: {
                const common = self.currentCommonIdents();
                break :blk nominal.origin_module.eql(common.builtin_module) and
                    builtinPrimForNominal(nominal.ident.ident_idx, common) != null;
            },
            else => false,
        },
        else => false,
    };
    if (!requires_prim) return;

    const expr = module_env.store.getExpr(expr_idx);
    const maybe_low_level = switch (expr) {
        .e_call => if (lookupProgramExprCallSite(session, expr_idx)) |call_site|
            switch (call_site) {
                .low_level => |op| op,
                .direct, .indirect_call => null,
            }
        else
            null,
        else => null,
    };
    const call_func_expr: u32 = switch (expr) {
        .e_call => |call_expr| @intFromEnum(call_expr.func),
        else => std.math.maxInt(u32),
    };
    const call_func_tag: []const u8 = switch (expr) {
        .e_call => |call_expr| @tagName(module_env.store.getExpr(call_expr.func)),
        else => "none",
    };
    const call_func_pattern: u32 = switch (expr) {
        .e_call => |call_expr| switch (module_env.store.getExpr(call_expr.func)) {
            .e_lookup_local => |lookup| @intFromEnum(lookup.pattern_idx),
            else => std.math.maxInt(u32),
        },
        else => std.math.maxInt(u32),
    };
    const call_func_source_expr: u32 = switch (expr) {
        .e_call => |call_expr| blk: {
            const resolution = lookupProgramExprLookupResolution(session, call_expr.func) orelse break :blk std.math.maxInt(u32);
            break :blk switch (resolution) {
                .expr => |source_expr_ref| @intFromEnum(source_expr_ref.expr_idx),
                .def => std.math.maxInt(u32),
            };
        },
        else => std.math.maxInt(u32),
    };
    const has_program_expr_mono = lookupProgramExprMonotype(session, expr_idx) != null;
    const maybe_callsite_inst = switch (expr) {
        .e_call => if (lookupProgramExprCallSite(session, expr_idx)) |call_site|
            switch (call_site) {
                .direct => |callable_inst_id| callable_inst_id,
                .indirect_call => null,
                .low_level => null,
            }
        else
            null,
        else => null,
    };
    const callsite_template_expr: u32 = if (maybe_callsite_inst) |callsite_inst|
        @intFromEnum(self.callable_pipeline.getCallableDefForInst(callsite_inst).runtime_expr.expr_idx)
    else
        std.math.maxInt(u32);
    const callsite_template_body_expr: u32 = if (maybe_callsite_inst) |callsite_inst|
        @intFromEnum(self.callableDefBodyExpr(self.callable_pipeline.getCallableDefForInst(callsite_inst)).common().source_expr)
    else
        std.math.maxInt(u32);
    const callsite_template_body_tag: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_def = self.callable_pipeline.getCallableDefForInst(callsite_inst);
        const template_env = self.all_module_envs[callable_def.module_idx];
        break :blk @tagName(template_env.store.getExpr(self.callableDefBodyExpr(callable_def).common().source_expr));
    } else "none";
    const callsite_callable_kind: []const u8 = if (maybe_callsite_inst) |callsite_inst|
        callableRuntimeValueKindText(self.callable_pipeline.getCallableInst(callsite_inst).runtime_value)
    else
        "none";
    const callsite_fn_monotype: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.getCallableInst(callsite_inst);
        break :blk @tagName(self.callable_pipeline.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype));
    } else "none";
    const callsite_ret_monotype: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.getCallableInst(callsite_inst);
        const fn_mono = self.callable_pipeline.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype);
        break :blk switch (fn_mono) {
            .func => |func| @tagName(self.callable_pipeline.context_mono.monotype_store.getMonotype(func.ret)),
            else => "non-func",
        };
    } else "none";

    std.debug.panic(
        "predeclareTrivialBlockStmtPatterns({s}): unit source monotype would bind builtin prim pattern module={d} stmt={d} pattern={d} expr={d} resolved_root={d} expr_tag={s} call_func_expr={d} call_func_tag={s} call_func_pattern={d} call_func_source_expr={d} low_level={s} has_program_expr_mono={} callsite_inst={d} callsite_template_expr={d} callsite_template_body_expr={d} callsite_template_body_tag={s} callsite_callable_kind={s} callsite_fn_monotype={s} callsite_ret_monotype={s} callable_inst={d} root_source_expr={d}",
        .{
            stmt_kind,
            self.current_module_idx,
            @intFromEnum(stmt_idx),
            @intFromEnum(pattern_idx),
            @intFromEnum(expr_idx),
            @intFromEnum(resolved.var_),
            @tagName(expr),
            call_func_expr,
            call_func_tag,
            call_func_pattern,
            call_func_source_expr,
            if (maybe_low_level) |op| @tagName(op) else "none",
            has_program_expr_mono,
            if (maybe_callsite_inst) |callsite_inst| @intFromEnum(callsite_inst) else std.math.maxInt(u32),
            callsite_template_expr,
            callsite_template_body_expr,
            callsite_template_body_tag,
            callsite_callable_kind,
            callsite_fn_monotype,
            callsite_ret_monotype,
            session.callableInstRawForDebug(),
            session.rootExprRawForDebug(),
        },
    );
}

fn predeclareBindingLocals(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    var bindings = std.ArrayList(BindingNode).empty;
    defer bindings.deinit(self.allocator);
    try self.collectBindingNodes(module_env, pattern_idx, &bindings);
    for (bindings.items) |binding| {
        _ = try self.patternToLocal(session, binding.pattern_idx);
    }
}

fn predeclareBindingLocalsFromMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    monotype: Monotype.Idx,
    existing_local: ?MIR.LocalId,
) Allocator.Error!void {
    switch (module_env.store.getPattern(pattern_idx)) {
        .assign => {
            if (existing_local) |local| {
                try self.bindBindingLocalInCurrentScope(pattern_idx, local);
                const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
                const binding = self.binding_records.getPtr(key) orelse unreachable;
                if (binding.shadowed) unreachable;
                binding.explicit_monotype = monotype;
                try self.requireLocalMonotype(local, monotype, "predeclareBindingLocalsFromMonotype.assign");
            } else {
                _ = try self.patternToLocalWithMonotype(pattern_idx, monotype);
            }
        },
        .as => |as_pat| {
            if (existing_local) |local| {
                try self.bindBindingLocalInCurrentScope(pattern_idx, local);
                const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
                const binding = self.binding_records.getPtr(key) orelse unreachable;
                if (binding.shadowed) unreachable;
                binding.explicit_monotype = monotype;
                try self.requireLocalMonotype(local, monotype, "predeclareBindingLocalsFromMonotype.as");
            } else {
                _ = try self.patternToLocalWithMonotype(pattern_idx, monotype);
            }
            try self.predeclareBindingLocalsFromMonotype(module_env, as_pat.pattern, monotype, null);
        },
        .nominal => |nominal_pat| try self.predeclareBindingLocalsFromMonotype(module_env, nominal_pat.backing_pattern, monotype, existing_local),
        .nominal_external => |nominal_pat| try self.predeclareBindingLocalsFromMonotype(module_env, nominal_pat.backing_pattern, monotype, existing_local),
        .applied_tag => |tag_pat| {
            const payload_monos = self.tagPayloadMonotypesByName(monotype, tag_pat.name);
            const payload_patterns = module_env.store.slicePatterns(tag_pat.args);
            if (builtin.mode == .Debug and payload_patterns.len != payload_monos.len) {
                std.debug.panic(
                    "statement-only MIR invariant violated: tag pattern payload arity mismatch for pattern {d} (patterns={d}, monos={d})",
                    .{ @intFromEnum(pattern_idx), payload_patterns.len, payload_monos.len },
                );
            }
            for (payload_patterns, payload_monos) |payload_pattern_idx, payload_mono| {
                try self.predeclareBindingLocalsFromMonotype(module_env, payload_pattern_idx, payload_mono, null);
            }
        },
        .record_destructure => |record_pat| {
            const record_fields = switch (self.store.monotype_store.getMonotype(monotype)) {
                .record => |record| self.store.monotype_store.getFields(record.fields),
                .unit => &.{},
                else => typeBindingInvariant(
                    "record pattern expected record/unit monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };
            for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                const destruct = module_env.store.getRecordDestruct(destruct_idx);
                switch (destruct.kind) {
                    .Required, .SubPattern => |sub_pattern_idx| {
                        const field_idx = self.recordFieldIndexByName(
                            Monotype.Name{
                                .module_idx = self.current_module_idx,
                                .ident = destruct.label,
                            },
                            record_fields,
                        );
                        try self.predeclareBindingLocalsFromMonotype(
                            module_env,
                            sub_pattern_idx,
                            record_fields[field_idx].type_idx,
                            null,
                        );
                    },
                    .Rest => |sub_pattern_idx| try self.predeclareBindingLocalsFromMonotype(module_env, sub_pattern_idx, monotype, null),
                }
            }
        },
        .list => |list_pat| {
            const elem_mono = switch (self.store.monotype_store.getMonotype(monotype)) {
                .list => |list_mono| list_mono.elem,
                else => typeBindingInvariant(
                    "list pattern expected list monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };
            for (module_env.store.slicePatterns(list_pat.patterns)) |elem_pattern_idx| {
                try self.predeclareBindingLocalsFromMonotype(module_env, elem_pattern_idx, elem_mono, null);
            }
            if (list_pat.rest_info) |rest| {
                if (rest.pattern) |rest_pattern_idx| {
                    try self.predeclareBindingLocalsFromMonotype(module_env, rest_pattern_idx, monotype, null);
                }
            }
        },
        .tuple => |tuple_pat| {
            const elem_monos = switch (self.store.monotype_store.getMonotype(monotype)) {
                .tuple => |tuple_mono| self.store.monotype_store.getIdxSpan(tuple_mono.elems),
                else => typeBindingInvariant(
                    "tuple pattern expected tuple monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
            };
            const elem_patterns = module_env.store.slicePatterns(tuple_pat.patterns);
            if (builtin.mode == .Debug and elem_patterns.len != elem_monos.len) {
                std.debug.panic(
                    "statement-only MIR invariant violated: tuple pattern arity mismatch for pattern {d} (patterns={d}, monos={d})",
                    .{ @intFromEnum(pattern_idx), elem_patterns.len, elem_monos.len },
                );
            }
            for (elem_patterns, elem_monos) |elem_pattern_idx, elem_mono| {
                try self.predeclareBindingLocalsFromMonotype(module_env, elem_pattern_idx, elem_mono, null);
            }
        },
        .underscore,
        .num_literal,
        .small_dec_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .str_literal,
        .runtime_error,
        => {},
    }
}

fn markBindingLocalsPreludeBound(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    const prelude_bound_locals = self.current_prelude_bound_locals orelse return;

    var bindings = std.ArrayList(BindingNode).empty;
    defer bindings.deinit(self.allocator);
    try self.collectBindingNodes(module_env, pattern_idx, &bindings);
    for (bindings.items) |binding| {
        const local = self.lookupExistingBindingLocalInScope(
            self.current_module_idx,
            self.current_binding_scope,
            binding.pattern_idx,
        ) orelse std.debug.panic(
            "statement-only MIR invariant violated: prelude-bound marking missing predeclared binding local {d} in scope {d}",
            .{ @intFromEnum(binding.pattern_idx), self.current_binding_scope },
        );
        try prelude_bound_locals.add(local);
    }
}

fn bindRepresentativeBindingLocalsToBranchPattern(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    representative_pattern_idx: CIR.Pattern.Idx,
    alternative_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    if (representative_pattern_idx == alternative_pattern_idx) return;

    var representative_bindings = std.ArrayList(BindingNode).empty;
    defer representative_bindings.deinit(self.allocator);

    var alternative_bindings = std.ArrayList(BindingNode).empty;
    defer alternative_bindings.deinit(self.allocator);

    try self.collectBindingNodes(module_env, representative_pattern_idx, &representative_bindings);
    try self.collectBindingNodes(module_env, alternative_pattern_idx, &alternative_bindings);

    for (representative_bindings.items) |rep_binding| {
        for (alternative_bindings.items) |alt_binding| {
            if (!rep_binding.ident.eql(alt_binding.ident)) continue;

            const alt_local = self.lookupExistingBindingLocalInScope(
                self.current_module_idx,
                self.current_binding_scope,
                alt_binding.pattern_idx,
            ) orelse try self.patternToLocal(session, alt_binding.pattern_idx);
            try self.bindBindingLocalInCurrentScope(rep_binding.pattern_idx, alt_local);

            const alt_key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, alt_binding.pattern_idx);
            if (self.binding_records.get(alt_key)) |binding| if (!binding.shadowed) if (binding.explicit_monotype) |monotype| {
                const rep_key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, rep_binding.pattern_idx);
                const rep_binding_state = self.binding_records.getPtr(rep_key) orelse unreachable;
                if (rep_binding_state.shadowed) unreachable;
                rep_binding_state.explicit_monotype = monotype;
                rep_binding_state.source_expr_ref = binding.source_expr_ref;
                rep_binding_state.callable_value = binding.callable_value;
            };

            break;
        }
    }
}

fn findVisibleBindingLocal(
    bindings: []const VisibleBindingLocal,
    source_index: *usize,
    pattern_idx: CIR.Pattern.Idx,
    comptime missing_message: []const u8,
) MIR.LocalId {
    while (source_index.* < bindings.len and
        @intFromEnum(bindings[source_index.*].pattern_idx) < @intFromEnum(pattern_idx))
    {
        source_index.* += 1;
    }

    if (source_index.* >= bindings.len or bindings[source_index.*].pattern_idx != pattern_idx) {
        std.debug.panic(missing_message, .{});
    }

    return bindings[source_index.*].local;
}

fn buildCarriedJumpArgsWithValue(
    self: *Self,
    source_scope: u64,
    dest_scope: u64,
    dest_value_local: MIR.LocalId,
    value_local: MIR.LocalId,
) Allocator.Error!MIR.LocalSpan {
    const source_bindings = try self.visibleReassignableBindingsInScope(source_scope);
    defer self.allocator.free(source_bindings);
    const dest_bindings = try self.visibleReassignableBindingsInScope(dest_scope);
    defer self.allocator.free(dest_bindings);

    const args = try self.allocator.alloc(MIR.LocalId, dest_bindings.len + 1);
    defer self.allocator.free(args);
    args[0] = value_local;
    try self.mergeJoinParamCallableResolution(dest_value_local, value_local);
    var source_i: usize = 0;
    for (dest_bindings, 0..) |dest_binding, i| {
        args[i + 1] = findVisibleBindingLocal(
            source_bindings,
            &source_i,
            dest_binding.pattern_idx,
            "statement-only MIR join invariant violated: source scope is missing a carried binding required by destination scope",
        );
        try self.mergeJoinParamCallableResolution(dest_binding.local, args[i + 1]);
    }
    return self.store.addLocalSpan(self.allocator, args);
}

fn buildCarriedJumpArgsWithValueExcluding(
    self: *Self,
    source_scope: u64,
    dest_scope: u64,
    dest_value_local: MIR.LocalId,
    value_local: MIR.LocalId,
    excluded_patterns: []const CIR.Pattern.Idx,
) Allocator.Error!MIR.LocalSpan {
    const source_bindings = try self.visibleReassignableBindingsInScope(source_scope);
    defer self.allocator.free(source_bindings);
    const dest_bindings = try self.visibleReassignableBindingsInScope(dest_scope);
    defer self.allocator.free(dest_bindings);

    var count: usize = 1;
    for_bindings: for (dest_bindings) |dest_binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (dest_binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        count += 1;
    }

    const args = try self.allocator.alloc(MIR.LocalId, count);
    defer self.allocator.free(args);
    args[0] = value_local;
    try self.mergeJoinParamCallableResolution(dest_value_local, value_local);
    var source_i: usize = 0;
    var arg_i: usize = 1;
    for_bindings: for (dest_bindings) |dest_binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (dest_binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        args[arg_i] = findVisibleBindingLocal(
            source_bindings,
            &source_i,
            dest_binding.pattern_idx,
            "statement-only MIR join invariant violated: source scope is missing a carried binding required by destination scope",
        );
        try self.mergeJoinParamCallableResolution(dest_binding.local, args[arg_i]);
        arg_i += 1;
    }
    return self.store.addLocalSpan(self.allocator, args);
}

fn buildCarriedJumpArgs(
    self: *Self,
    source_scope: u64,
    dest_scope: u64,
) Allocator.Error!MIR.LocalSpan {
    const source_bindings = try self.visibleReassignableBindingsInScope(source_scope);
    defer self.allocator.free(source_bindings);
    const dest_bindings = try self.visibleReassignableBindingsInScope(dest_scope);
    defer self.allocator.free(dest_bindings);

    const args = try self.allocator.alloc(MIR.LocalId, dest_bindings.len);
    defer self.allocator.free(args);
    var source_i: usize = 0;
    for (dest_bindings, 0..) |dest_binding, i| {
        args[i] = findVisibleBindingLocal(
            source_bindings,
            &source_i,
            dest_binding.pattern_idx,
            "statement-only MIR join invariant violated: source scope is missing a carried binding required by destination scope",
        );
        try self.mergeJoinParamCallableResolution(dest_binding.local, args[i]);
    }
    return self.store.addLocalSpan(self.allocator, args);
}

fn buildCarriedJumpArgsExcluding(
    self: *Self,
    source_scope: u64,
    dest_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
) Allocator.Error!MIR.LocalSpan {
    const source_bindings = try self.visibleReassignableBindingsInScope(source_scope);
    defer self.allocator.free(source_bindings);
    const dest_bindings = try self.visibleReassignableBindingsInScope(dest_scope);
    defer self.allocator.free(dest_bindings);

    var count: usize = 0;
    for_bindings: for (dest_bindings) |dest_binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (dest_binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        count += 1;
    }

    const args = try self.allocator.alloc(MIR.LocalId, count);
    defer self.allocator.free(args);
    var source_i: usize = 0;
    var arg_i: usize = 0;
    for_bindings: for (dest_bindings) |dest_binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (dest_binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        args[arg_i] = findVisibleBindingLocal(
            source_bindings,
            &source_i,
            dest_binding.pattern_idx,
            "statement-only MIR join invariant violated: source scope is missing a carried binding required by destination scope",
        );
        try self.mergeJoinParamCallableResolution(dest_binding.local, args[arg_i]);
        arg_i += 1;
    }
    return self.store.addLocalSpan(self.allocator, args);
}

fn buildJoinParamSpanWithValue(
    self: *Self,
    result_local: MIR.LocalId,
    scope: u64,
) Allocator.Error!MIR.LocalSpan {
    const bindings = try self.visibleReassignableBindingsInScope(scope);
    defer self.allocator.free(bindings);

    const params = try self.allocator.alloc(MIR.LocalId, bindings.len + 1);
    defer self.allocator.free(params);
    params[0] = result_local;
    for (bindings, 0..) |binding, i| {
        params[i + 1] = binding.local;
    }
    return self.store.addLocalSpan(self.allocator, params);
}

fn buildJoinParamSpanWithValueExcluding(
    self: *Self,
    result_local: MIR.LocalId,
    scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
) Allocator.Error!MIR.LocalSpan {
    const bindings = try self.visibleReassignableBindingsInScope(scope);
    defer self.allocator.free(bindings);

    var count: usize = 1;
    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        count += 1;
    }

    const params = try self.allocator.alloc(MIR.LocalId, count);
    defer self.allocator.free(params);
    params[0] = result_local;
    var i: usize = 1;
    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        params[i] = binding.local;
        i += 1;
    }
    return self.store.addLocalSpan(self.allocator, params);
}

fn buildJoinParamSpan(
    self: *Self,
    scope: u64,
) Allocator.Error!MIR.LocalSpan {
    const bindings = try self.visibleReassignableBindingsInScope(scope);
    defer self.allocator.free(bindings);

    const params = try self.allocator.alloc(MIR.LocalId, bindings.len);
    defer self.allocator.free(params);
    for (bindings, 0..) |binding, i| {
        params[i] = binding.local;
    }
    return self.store.addLocalSpan(self.allocator, params);
}

fn buildJoinParamSpanExcluding(
    self: *Self,
    scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
) Allocator.Error!MIR.LocalSpan {
    const bindings = try self.visibleReassignableBindingsInScope(scope);
    defer self.allocator.free(bindings);

    var count: usize = 0;
    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        count += 1;
    }

    const params = try self.allocator.alloc(MIR.LocalId, count);
    defer self.allocator.free(params);
    var i: usize = 0;
    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        params[i] = binding.local;
        i += 1;
    }
    return self.store.addLocalSpan(self.allocator, params);
}

fn buildCarriedJumpArgsWithExtraLocals(
    self: *Self,
    source_scope: u64,
    dest_scope: u64,
    extra_locals: []const MIR.LocalId,
) Allocator.Error!MIR.LocalSpan {
    const source_bindings = try self.visibleReassignableBindingsInScope(source_scope);
    defer self.allocator.free(source_bindings);
    const dest_bindings = try self.visibleReassignableBindingsInScope(dest_scope);
    defer self.allocator.free(dest_bindings);

    const args = try self.allocator.alloc(MIR.LocalId, dest_bindings.len + extra_locals.len);
    defer self.allocator.free(args);
    var source_i: usize = 0;
    for (dest_bindings, 0..) |dest_binding, i| {
        args[i] = findVisibleBindingLocal(
            source_bindings,
            &source_i,
            dest_binding.pattern_idx,
            "statement-only MIR loop invariant violated: source scope is missing a carried binding required by destination scope",
        );
        try self.mergeJoinParamCallableResolution(dest_binding.local, args[i]);
    }
    @memcpy(args[dest_bindings.len..], extra_locals);
    return self.store.addLocalSpan(self.allocator, args);
}

fn buildJoinParamSpanWithExtraLocals(
    self: *Self,
    scope: u64,
    extra_locals: []const MIR.LocalId,
) Allocator.Error!MIR.LocalSpan {
    const bindings = try self.visibleReassignableBindingsInScope(scope);
    defer self.allocator.free(bindings);

    const params = try self.allocator.alloc(MIR.LocalId, bindings.len + extra_locals.len);
    defer self.allocator.free(params);
    for (bindings, 0..) |binding, i| {
        params[i] = binding.local;
    }
    @memcpy(params[bindings.len..], extra_locals);
    return self.store.addLocalSpan(self.allocator, params);
}

fn buildJoinParamCallableSpan(
    self: *Self,
    params: MIR.LocalSpan,
) Allocator.Error!MIR.CallableResolutionSpan {
    const param_locals = self.store.getLocalSpan(params);
    if (param_locals.len == 0) return .empty();

    const resolved = try self.allocator.alloc(?CallableResolution, param_locals.len);
    defer self.allocator.free(resolved);

    var any_callable = false;
    for (param_locals, 0..) |param_local, i| {
        const key = @intFromEnum(param_local);
        resolved[i] = if (self.pending_join_param_callables.fetchRemove(key)) |entry|
            entry.value
        else
            self.resolveLocalCallableResolution(param_local);
        if (resolved[i] != null) any_callable = true;
    }

    if (!any_callable) return .empty();
    return self.store.addCallableResolutionSpan(self.allocator, resolved);
}

fn addJoinStmt(
    self: *Self,
    id: MIR.JoinPointId,
    params: MIR.LocalSpan,
    body: MIR.CFStmtId,
    remainder: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = id,
        .params = params,
        .param_callables = try self.buildJoinParamCallableSpan(params),
        .body = body,
        .remainder = remainder,
    } });
}

fn lowerExprSequenceIntoContinuationWithValues(
    self: *Self,
    session: LowerSession,
    exprs: []const CIR.Expr.Idx,
    joined_locals: []const MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!struct { entry: MIR.CFStmtId, exit_scope: u64 } {
    if (builtin.mode == .Debug and exprs.len != joined_locals.len) {
        std.debug.panic(
            "statement-only MIR invariant violated: expr sequence arity mismatch exprs={d} locals={d}",
            .{ exprs.len, joined_locals.len },
        );
    }

    const start_scope = self.current_binding_scope;
    const scope_infos = try self.allocator.alloc(ExprSequenceScopeInfo, exprs.len);
    defer self.allocator.free(scope_infos);

    var scope = start_scope;
    for (exprs, 0..) |_, i| {
        scope_infos[i].before = scope;
        scope = self.freshChildBindingScope(scope);
        scope_infos[i].after = scope;

        const saved_scope = self.current_binding_scope;
        self.current_binding_scope = scope_infos[i].after;
        try self.predeclareVisibleReassignableBindingsInScope(scope_infos[i].before, scope_infos[i].after);
        self.current_binding_scope = saved_scope;
    }

    var lowered_next = next;
    var i = exprs.len;
    while (i > 0) {
        i -= 1;
        const expr_idx = exprs[i];
        const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, expr_idx), false);
        lowered_next = try self.lowerExprIntoContinuationScopeWithValue(
            session,
            expr_idx,
            value_local,
            joined_locals[i],
            scope_infos[i].before,
            scope_infos[i].after,
            &.{},
            lowered_next,
        );
    }

    const exit_scope = if (scope_infos.len == 0) start_scope else scope_infos[scope_infos.len - 1].after;
    self.current_binding_scope = exit_scope;
    return .{
        .entry = lowered_next,
        .exit_scope = exit_scope,
    };
}

fn lowerBoolBranchChainIntoWithExitScope(
    self: *Self,
    session: LowerSession,
    branches: []const CIR.Expr.IfBranch.Idx,
    final_else: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];

    if (branches.len == 0) {
        return self.lowerExprWithExitScope(session, final_else, target, next);
    }

    const branch = module_env.store.getIfBranch(branches[0]);
    if (self.cirExprKnownBoolValue(self.current_module_idx, branch.cond)) |known_bool| {
        return if (known_bool)
            self.lowerExprWithExitScope(session, branch.body, target, next)
        else
            self.lowerBoolBranchChainIntoWithExitScope(session, branches[1..], final_else, target, next);
    }

    const cond_mono = try self.resolveMonotype(session, branch.cond);
    const result_mono = self.store.getLocal(target).monotype;
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
    const switch_placeholder = try self.store.reserveCFStmt(self.allocator);
    const discrim_stmt = try self.lowerRefInto(
        discrim_local,
        .{ .discriminant = .{ .source = cond_local } },
        switch_placeholder,
    );
    const lowered_cond = try self.lowerExprWithExitScope(session, branch.cond, cond_local, discrim_stmt);
    if (!lowered_cond.falls_through) {
        return lowered_cond;
    }
    const join_scope = self.freshChildBindingScope(lowered_cond.exit_scope);
    try self.predeclareVisibleReassignableBindingsInScope(lowered_cond.exit_scope, join_scope);
    const join_id = self.freshJoinPointId();
    const joined_value_local = try self.freshSyntheticLocal(result_mono, false);
    var join_has_incoming = false;

    self.current_binding_scope = lowered_cond.exit_scope;
    const else_value = try self.freshSyntheticLocal(result_mono, false);
    const else_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered_else = try self.lowerBoolBranchChainIntoWithExitScope(session, branches[1..], final_else, else_value, else_jump);
    if (lowered_else.falls_through) {
        join_has_incoming = true;
        try self.store.finalizeCFStmt(else_jump, .{ .jump = .{
            .id = join_id,
            .args = try self.buildCarriedJumpArgsWithValue(
                lowered_else.exit_scope,
                join_scope,
                joined_value_local,
                else_value,
            ),
        } });
    }

    self.current_binding_scope = lowered_cond.exit_scope;
    const then_value = try self.freshSyntheticLocal(result_mono, false);
    const then_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered_then = try self.lowerExprWithExitScope(session, branch.body, then_value, then_jump);
    if (lowered_then.falls_through) {
        join_has_incoming = true;
        try self.store.finalizeCFStmt(then_jump, .{ .jump = .{
            .id = join_id,
            .args = try self.buildCarriedJumpArgsWithValue(
                lowered_then.exit_scope,
                join_scope,
                joined_value_local,
                then_value,
            ),
        } });
    }

    const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
        "statement-only MIR if expected Bool condition monotype for expr {d}",
        .{@intFromEnum(branch.cond)},
    );
    try self.store.finalizeCFStmt(switch_placeholder, .{ .switch_stmt = .{
        .scrutinee = discrim_local,
        .branches = try self.store.addSwitchBranches(self.allocator, &.{
            .{
                .value = self.tagDiscriminantForMonotypeName(cond_mono, tag_names.true_name),
                .body = lowered_then.entry,
            },
        }),
        .default_branch = lowered_else.entry,
    } });

    if (!join_has_incoming) {
        return .{
            .entry = lowered_cond.entry,
            .exit_scope = lowered_cond.exit_scope,
            .falls_through = false,
        };
    }

    const join_body = try self.lowerLocalAliasInto(target, joined_value_local, next);
    const entry = try self.addJoinStmt(
        join_id,
        try self.buildJoinParamSpanWithValue(joined_value_local, join_scope),
        join_body,
        lowered_cond.entry,
    );

    return .{
        .entry = entry,
        .exit_scope = join_scope,
        .falls_through = true,
    };
}

fn lowerExprWithExitScope(
    self: *Self,
    session: LowerSession,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];
    const start_scope = self.current_binding_scope;
    const lowered = switch (module_env.store.getExpr(expr_idx)) {
        .e_block => |block| try self.lowerBlockExprIntoWithExitScope(session, block, target, next),
        .e_if => |if_expr| try self.lowerBoolBranchChainIntoWithExitScope(
            session,
            module_env.store.sliceIfBranches(if_expr.branches),
            if_expr.final_else,
            target,
            next,
        ),
        .e_match => |match_expr| try self.lowerMatchIntoWithExitScope(session, module_env, match_expr, target, next),
        else => LoweredExprWithScope{
            .entry = try self.lowerCirExprInto(session, expr_idx, target, next),
            .exit_scope = self.current_binding_scope,
            .falls_through = switch (module_env.store.getExpr(expr_idx)) {
                .e_return, .e_runtime_error, .e_crash => false,
                else => true,
            },
        },
    };
    self.current_binding_scope = start_scope;
    return lowered;
}

fn lowerBlockExprIntoWithExitScope(
    self: *Self,
    session: LowerSession,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];
    const stmts = module_env.store.sliceStatements(block.stmts);
    const saved_scope = self.current_binding_scope;
    const scope_infos = try self.allocator.alloc(BlockStmtScopeInfo, stmts.len);
    defer self.allocator.free(scope_infos);

    var scope = self.current_binding_scope;
    for (stmts, 0..) |stmt_idx, i| {
        scope_infos[i].before = scope;
        switch (module_env.store.getStatement(stmt_idx)) {
            .s_decl, .s_var, .s_reassign, .s_expr, .s_dbg, .s_expect, .s_for, .s_while => {
                scope = self.freshChildBindingScope(scope);
                scope_infos[i].after = scope;
            },
            else => {
                scope_infos[i].after = scope;
            },
        }

        self.current_binding_scope = scope_infos[i].after;
        try self.predeclareReassignableBindingsForStmtScope(
            module_env,
            stmt_idx,
            scope_infos[i].before,
            scope_infos[i].after,
        );
        try self.predeclareTrivialBlockStmtPatterns(session, module_env, stmt_idx);
    }

    self.current_binding_scope = scope;
    const lowered_final = try self.lowerExprWithExitScope(session, block.final_expr, target, next);
    var entry = lowered_final.entry;
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        self.current_binding_scope = scope_infos[i].after;
        entry = try self.lowerBlockStmtInto(session, module_env, stmts[i], scope_infos[i].before, entry);
        self.current_binding_scope = scope_infos[i].before;
    }
    self.current_binding_scope = saved_scope;
    return .{
        .entry = entry,
        .exit_scope = lowered_final.exit_scope,
        .falls_through = lowered_final.falls_through,
    };
}

fn lowerBlockIntoWithExitScope(
    self: *Self,
    session: LowerSession,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];
    const stmts = module_env.store.sliceStatements(block.stmts);
    const saved_scope = self.current_binding_scope;
    const scope_infos = try self.allocator.alloc(BlockStmtScopeInfo, stmts.len);
    defer self.allocator.free(scope_infos);

    var scope = self.current_binding_scope;
    for (stmts, 0..) |stmt_idx, i| {
        scope_infos[i].before = scope;
        switch (module_env.store.getStatement(stmt_idx)) {
            .s_decl, .s_var, .s_reassign, .s_expr, .s_dbg, .s_expect, .s_for, .s_while => {
                scope = self.freshChildBindingScope(scope);
                scope_infos[i].after = scope;
            },
            else => {
                scope_infos[i].after = scope;
            },
        }

        self.current_binding_scope = scope_infos[i].after;
        try self.predeclareReassignableBindingsForStmtScope(
            module_env,
            stmt_idx,
            scope_infos[i].before,
            scope_infos[i].after,
        );
        try self.predeclareTrivialBlockStmtPatterns(session, module_env, stmt_idx);
    }

    const exit_scope = self.freshChildBindingScope(saved_scope);
    try self.predeclareVisibleReassignableBindingsInScope(saved_scope, exit_scope);
    const after_final_join_id = self.freshJoinPointId();
    const final_value_local = try self.freshSyntheticLocal(self.store.getLocal(target).monotype, false);
    const joined_final_local = try self.freshSyntheticLocal(self.store.getLocal(target).monotype, false);
    const after_final_jump = try self.store.reserveCFStmt(self.allocator);

    self.current_binding_scope = scope;
    const lowered_final = try self.lowerExprWithExitScope(session, block.final_expr, final_value_local, after_final_jump);
    var entry = lowered_final.entry;
    var final_exit_scope = lowered_final.exit_scope;
    if (lowered_final.falls_through) {
        try self.store.finalizeCFStmt(after_final_jump, .{ .jump = .{
            .id = after_final_join_id,
            .args = try self.buildCarriedJumpArgsWithValue(
                lowered_final.exit_scope,
                exit_scope,
                joined_final_local,
                final_value_local,
            ),
        } });
        const after_final_join_body = try self.lowerLocalAliasInto(target, joined_final_local, next);
        entry = try self.addJoinStmt(
            after_final_join_id,
            try self.buildJoinParamSpanWithValue(joined_final_local, exit_scope),
            after_final_join_body,
            lowered_final.entry,
        );
        final_exit_scope = exit_scope;
    }
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        self.current_binding_scope = scope_infos[i].after;
        entry = try self.lowerBlockStmtInto(session, module_env, stmts[i], scope_infos[i].before, entry);
        self.current_binding_scope = scope_infos[i].before;
    }
    self.current_binding_scope = saved_scope;
    return .{
        .entry = entry,
        .exit_scope = final_exit_scope,
        .falls_through = lowered_final.falls_through,
    };
}

fn lowerMatchIntoWithExitScope(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    match_expr: CIR.Expr.Match,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const cond_mono = try self.resolveMonotype(session, match_expr.cond);
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const dispatch_scope = self.freshChildBindingScope(self.current_binding_scope);
    const dispatch_join_cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const dispatch_join_id = self.freshJoinPointId();
    const dispatch_jump = try self.store.reserveCFStmt(self.allocator);
    const cond_source_expr_ref = self.exprSourceRefAliasOrSelf(session, match_expr.cond);
    const lowered_cond = try self.lowerExprWithExitScope(session, match_expr.cond, cond_local, dispatch_jump);
    if (!lowered_cond.falls_through) {
        return lowered_cond;
    }
    try self.predeclareVisibleReassignableBindingsInScope(lowered_cond.exit_scope, dispatch_scope);
    try self.store.finalizeCFStmt(dispatch_jump, .{ .jump = .{
        .id = dispatch_join_id,
        .args = try self.buildCarriedJumpArgsWithValue(
            lowered_cond.exit_scope,
            dispatch_scope,
            dispatch_join_cond_local,
            cond_local,
        ),
    } });

    const join_scope = self.freshChildBindingScope(dispatch_scope);
    try self.predeclareVisibleReassignableBindingsInScope(dispatch_scope, join_scope);
    const join_id = self.freshJoinPointId();
    const joined_result_local = try self.freshSyntheticLocal(self.store.getLocal(target).monotype, false);

    const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
    const match_failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });
    const branch_degenerate_failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });

    const chain = try self.lowerMatchBranchChainInto(
        session,
        module_env,
        branch_indices,
        dispatch_scope,
        dispatch_join_cond_local,
        cond_source_expr_ref,
        cond_mono,
        joined_result_local,
        join_id,
        join_scope,
        match_failure,
        branch_degenerate_failure,
    );

    const dispatch_join = try self.addJoinStmt(
        dispatch_join_id,
        try self.buildJoinParamSpanWithValue(dispatch_join_cond_local, dispatch_scope),
        chain.entry,
        lowered_cond.entry,
    );

    if (!chain.reaches_result_join) {
        return .{
            .entry = dispatch_join,
            .exit_scope = dispatch_scope,
            .falls_through = false,
        };
    }

    const result_join_body = try self.lowerLocalAliasInto(target, joined_result_local, next);
    const result_join_stmt = try self.addJoinStmt(
        join_id,
        try self.buildJoinParamSpanWithValue(joined_result_local, join_scope),
        result_join_body,
        dispatch_join,
    );
    return .{
        .entry = result_join_stmt,
        .exit_scope = join_scope,
        .falls_through = true,
    };
}

fn lowerMatchBranchChainInto(
    self: *Self,
    session: LowerSession,
    module_env: *const ModuleEnv,
    branch_indices: []const CIR.Expr.Match.Branch.Idx,
    input_scope: u64,
    cond_local: MIR.LocalId,
    cond_source_expr_ref: LowerExprSourceRef,
    cond_mono: Monotype.Idx,
    result_local: MIR.LocalId,
    result_join_id: MIR.JoinPointId,
    result_join_scope: u64,
    on_exhausted: MIR.CFStmtId,
    branch_degenerate_failure: MIR.CFStmtId,
) Allocator.Error!MatchBranchChainResult {
    if (branch_indices.len == 0) {
        return .{
            .entry = on_exhausted,
            .reaches_result_join = false,
        };
    }

    const cir_branch = module_env.store.getMatchBranch(branch_indices[0]);
    const rest_scope = self.freshChildBindingScope(input_scope);
    try self.predeclareVisibleReassignableBindingsInScope(input_scope, rest_scope);
    const rest = try self.lowerMatchBranchChainInto(
        session,
        module_env,
        branch_indices[1..],
        rest_scope,
        cond_local,
        cond_source_expr_ref,
        cond_mono,
        result_local,
        result_join_id,
        result_join_scope,
        on_exhausted,
        branch_degenerate_failure,
    );
    const rest_join_id = self.freshJoinPointId();
    const rest_join_params = try self.buildJoinParamSpan(rest_scope);
    const pattern_fail_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
        .id = rest_join_id,
        .args = try self.buildCarriedJumpArgs(input_scope, rest_scope),
    } });

    const branch_pattern_indices = module_env.store.sliceMatchBranchPatterns(cir_branch.patterns);
    const result_mono = self.store.getLocal(result_local).monotype;

    var branch_entry = pattern_fail_jump;
    var branch_reaches_result_join = false;
    if (branch_pattern_indices.len == 0) {
        const branch_scope = self.freshChildBindingScope(input_scope);
        try self.inheritVisibleReassignableBindingsInScope(input_scope, branch_scope);
        const saved_scope = self.current_binding_scope;
        self.current_binding_scope = branch_scope;
        defer self.current_binding_scope = saved_scope;

        const branch_value_local = try self.freshSyntheticLocal(result_mono, false);
        const branch_jump = try self.store.reserveCFStmt(self.allocator);
        const lowered_branch_value = try self.lowerExprWithExitScope(session, cir_branch.value, branch_value_local, branch_jump);
        if (lowered_branch_value.falls_through) {
            branch_reaches_result_join = true;
            try self.store.finalizeCFStmt(branch_jump, .{ .jump = .{
                .id = result_join_id,
                .args = try self.buildCarriedJumpArgsWithValue(
                    lowered_branch_value.exit_scope,
                    result_join_scope,
                    result_local,
                    branch_value_local,
                ),
            } });
        }

        branch_entry = if (cir_branch.guard) |guard_idx| blk: {
            const guard_mono = try self.resolveMonotype(session, guard_idx);
            const tag_names = boolTagNamesForMonotype(self, guard_mono) orelse std.debug.panic(
                "statement-only MIR match guard expected Bool condition monotype for expr {d}",
                .{@intFromEnum(guard_idx)},
            );
            const guard_local = try self.freshSyntheticLocal(guard_mono, false);
            const guard_discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
            const guard_switch_placeholder = try self.store.reserveCFStmt(self.allocator);
            const guard_discrim_stmt = try self.lowerRefInto(
                guard_discrim_local,
                .{ .discriminant = .{ .source = guard_local } },
                guard_switch_placeholder,
            );
            const lowered_guard = try self.lowerExprWithExitScope(session, guard_idx, guard_local, guard_discrim_stmt);
            if (!lowered_guard.falls_through) {
                break :blk lowered_guard.entry;
            }
            const guard_false_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
                .id = rest_join_id,
                .args = try self.buildCarriedJumpArgs(lowered_guard.exit_scope, rest_scope),
            } });
            try self.store.finalizeCFStmt(guard_switch_placeholder, .{ .switch_stmt = .{
                .scrutinee = guard_discrim_local,
                .branches = try self.store.addSwitchBranches(self.allocator, &.{.{
                    .value = self.tagDiscriminantForMonotypeName(guard_mono, tag_names.true_name),
                    .body = lowered_branch_value.entry,
                }}),
                .default_branch = guard_false_jump,
            } });
            branch_reaches_result_join = branch_reaches_result_join or rest.reaches_result_join;
            break :blk lowered_guard.entry;
        } else lowered_branch_value.entry;
    } else {
        const representative_pattern_idx = if (branch_pattern_indices.len > 0)
            module_env.store.getMatchBranchPattern(branch_pattern_indices[0]).pattern
        else
            null;
        var pattern_i = branch_pattern_indices.len;
        while (pattern_i > 0) {
            pattern_i -= 1;
            const branch_pattern = module_env.store.getMatchBranchPattern(branch_pattern_indices[pattern_i]);
            const alternative_scope = self.freshChildBindingScope(input_scope);
            try self.inheritVisibleReassignableBindingsInScope(input_scope, alternative_scope);
            branch_entry = blk: {
                const saved_scope = self.current_binding_scope;
                self.current_binding_scope = alternative_scope;
                defer self.current_binding_scope = saved_scope;
                const saved_prelude_bound_locals = self.current_prelude_bound_locals;
                var branch_prelude_bound_locals = try LambdaEntryBindingState.cloneFrom(self.allocator, saved_prelude_bound_locals);
                defer branch_prelude_bound_locals.deinit();
                self.current_prelude_bound_locals = &branch_prelude_bound_locals;
                defer self.current_prelude_bound_locals = saved_prelude_bound_locals;

                try self.predeclareBindingLocals(session, module_env, branch_pattern.pattern);
                try self.markBindingLocalsPreludeBound(module_env, branch_pattern.pattern);
                if (representative_pattern_idx) |representative_pattern| {
                    try self.bindRepresentativeBindingLocalsToBranchPattern(
                        session,
                        module_env,
                        representative_pattern,
                        branch_pattern.pattern,
                    );
                    try self.markBindingLocalsPreludeBound(module_env, representative_pattern);
                }

                const branch_value_local = try self.freshSyntheticLocal(result_mono, false);
                const branch_jump = try self.store.reserveCFStmt(self.allocator);
                const lowered_branch_value = try self.lowerExprWithExitScope(session, cir_branch.value, branch_value_local, branch_jump);
                if (lowered_branch_value.falls_through) {
                    branch_reaches_result_join = true;
                    try self.store.finalizeCFStmt(branch_jump, .{ .jump = .{
                        .id = result_join_id,
                        .args = try self.buildCarriedJumpArgsWithValue(
                            lowered_branch_value.exit_scope,
                            result_join_scope,
                            result_local,
                            branch_value_local,
                        ),
                    } });
                }

                const guarded_body = if (cir_branch.guard) |guard_idx| guard_blk: {
                    const guard_mono = try self.resolveMonotype(session, guard_idx);
                    const tag_names = boolTagNamesForMonotype(self, guard_mono) orelse std.debug.panic(
                        "statement-only MIR match guard expected Bool condition monotype for expr {d}",
                        .{@intFromEnum(guard_idx)},
                    );
                    const guard_local = try self.freshSyntheticLocal(guard_mono, false);
                    const guard_discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
                    const guard_switch_placeholder = try self.store.reserveCFStmt(self.allocator);
                    const guard_discrim_stmt = try self.lowerRefInto(
                        guard_discrim_local,
                        .{ .discriminant = .{ .source = guard_local } },
                        guard_switch_placeholder,
                    );
                    const lowered_guard = try self.lowerExprWithExitScope(session, guard_idx, guard_local, guard_discrim_stmt);
                    if (!lowered_guard.falls_through) {
                        break :guard_blk lowered_guard.entry;
                    }
                    const guard_false_jump = try self.store.addCFStmt(self.allocator, .{ .jump = .{
                        .id = rest_join_id,
                        .args = try self.buildCarriedJumpArgs(lowered_guard.exit_scope, rest_scope),
                    } });
                    try self.store.finalizeCFStmt(guard_switch_placeholder, .{ .switch_stmt = .{
                        .scrutinee = guard_discrim_local,
                        .branches = try self.store.addSwitchBranches(self.allocator, &.{.{
                            .value = self.tagDiscriminantForMonotypeName(guard_mono, tag_names.true_name),
                            .body = lowered_branch_value.entry,
                        }}),
                        .default_branch = guard_false_jump,
                    } });
                    branch_reaches_result_join = branch_reaches_result_join or rest.reaches_result_join;
                    break :guard_blk lowered_guard.entry;
                } else lowered_branch_value.entry;

                break :blk try self.lowerPatternMatchLocalInto(
                    session,
                    module_env,
                    branch_pattern.pattern,
                    cond_local,
                    cond_source_expr_ref,
                    if (branch_pattern.degenerate) branch_degenerate_failure else guarded_body,
                    branch_entry,
                );
            };
        }
    }

    if (self.matchBranchIsIrrefutable(module_env, cir_branch, cond_mono)) {
        return .{
            .entry = branch_entry,
            .reaches_result_join = branch_reaches_result_join,
        };
    }

    return .{
        .entry = try self.addJoinStmt(
            rest_join_id,
            rest_join_params,
            rest.entry,
            branch_entry,
        ),
        .reaches_result_join = branch_reaches_result_join or rest.reaches_result_join,
    };
}

fn lowerBlockInto(
    self: *Self,
    session: LowerSession,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return (try self.lowerBlockIntoWithExitScope(session, block, target, next)).entry;
}

fn lowerCirExprInto(
    self: *Self,
    session: LowerSession,
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
            break :blk try self.lowerStringConcatInto(session, span, target, next);
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

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);

            var lowered_next = assign_stmt;
            var i: usize = cir_elems.len;
            while (i > 0) {
                i -= 1;
                const elem_idx = cir_elems[i];
                const elem_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, elem_idx), false);
                try self.scratch_local_ids.append(elem_local);
                lowered_next = try self.lowerCirExprInto(session, elem_idx, elem_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const elems = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            try self.store.finalizeCFStmt(assign_stmt, .{ .assign_list = .{
                .target = target,
                .elems = elems,
                .next = next,
            } });
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

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);

            var lowered_next = assign_stmt;
            var i: usize = elems.len;
            while (i > 0) {
                i -= 1;
                const elem_idx = elems[i];
                const elem_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, elem_idx), false);
                try self.scratch_local_ids.append(elem_local);
                lowered_next = try self.lowerCirExprInto(session, elem_idx, elem_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const fields = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            try self.store.finalizeCFStmt(assign_stmt, .{ .assign_struct = .{
                .target = target,
                .fields = fields,
                .next = next,
            } });
            break :blk lowered_next;
        },
        .e_record => |record| blk: {
            switch (self.store.monotype_store.getMonotype(monotype)) {
                .unit => {
                    const assign_stmt = try self.lowerUnitLocalInto(target, next);
                    var lowered_next = assign_stmt;

                    if (record.ext) |ext_expr| {
                        const ext_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, ext_expr), false);
                        lowered_next = try self.lowerCirExprInto(session, ext_expr, ext_local, lowered_next);
                    }

                    const source_fields = module_env.store.sliceRecordFields(record.fields);
                    var i: usize = source_fields.len;
                    while (i > 0) {
                        i -= 1;
                        const field = module_env.store.getRecordField(source_fields[i]);
                        const field_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, field.value), false);
                        lowered_next = try self.lowerCirExprInto(session, field.value, field_local, lowered_next);
                    }

                    break :blk lowered_next;
                },
                else => {},
            }

            const mono_record = switch (self.store.monotype_store.getMonotype(monotype)) {
                .record => |mono_record| mono_record,
                else => std.debug.panic(
                    "statement-only MIR lowerExpr: record literal expected record monotype for expr {d}",
                    .{@intFromEnum(expr_idx)},
                ),
            };

            var owned_mono_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
            defer owned_mono_fields.deinit(self.allocator);
            try owned_mono_fields.appendSlice(self.allocator, self.store.monotype_store.getFields(mono_record.fields));
            const mono_fields = owned_mono_fields.items;
            const ordered_locals = try self.allocator.alloc(?MIR.LocalId, mono_fields.len);
            defer self.allocator.free(ordered_locals);
            @memset(ordered_locals, null);
            const provided = try self.allocator.alloc(bool, mono_fields.len);
            defer self.allocator.free(provided);
            @memset(provided, false);

            var ext_expr_idx: ?CIR.Expr.Idx = null;
            var ext_local: ?MIR.LocalId = null;
            var ext_fields: []const Monotype.Field = &.{};
            var owned_ext_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
            defer owned_ext_fields.deinit(self.allocator);
            if (record.ext) |ext_expr| {
                ext_expr_idx = ext_expr;
                const ext_monotype = try self.resolveMonotype(session, ext_expr);
                ext_local = try self.freshSyntheticLocal(ext_monotype, false);
                ext_fields = switch (self.store.monotype_store.getMonotype(ext_monotype)) {
                    .record => |ext_record| blk_fields: {
                        try owned_ext_fields.appendSlice(self.allocator, self.store.monotype_store.getFields(ext_record.fields));
                        break :blk_fields owned_ext_fields.items;
                    },
                    .unit => &.{},
                    else => std.debug.panic(
                        "statement-only MIR lowerExpr: record update extension expected record monotype for expr {d}",
                        .{@intFromEnum(expr_idx)},
                    ),
                };
            }

            for (module_env.store.sliceRecordFields(record.fields)) |field_idx| {
                const field = module_env.store.getRecordField(field_idx);
                const canonical_idx = self.recordFieldIndexByName(field.name, mono_fields);
                if (provided[canonical_idx]) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: duplicate record field '{s}' in expr {d}",
                        .{
                            self.all_module_envs[self.current_module_idx].getIdent(field.name),
                            @intFromEnum(expr_idx),
                        },
                    );
                }
                provided[canonical_idx] = true;
                ordered_locals[canonical_idx] = try self.freshSyntheticLocal(
                    try self.resolveMonotype(session, field.value),
                    false,
                );
            }

            for (mono_fields, 0..) |mono_field, i| {
                if (ordered_locals[i] != null) continue;
                if (ext_local == null) {
                    std.debug.panic(
                        "statement-only MIR lowerExpr: record literal missing canonical field {d} for expr {d}",
                        .{ i, @intFromEnum(expr_idx) },
                    );
                }

                _ = self.recordFieldIndexByName(mono_field.name, ext_fields);
                const field_projection: Pipeline.ValueProjection = .{ .field = .{
                    .module_idx = mono_field.name.module_idx,
                    .ident = mono_field.name.ident,
                } };
                ordered_locals[i] = try self.freshSyntheticLocal(
                    try self.executableMonotypeForCallableValue(
                        mono_field.type_idx,
                        try self.resolveProjectedExprCallableValue(
                            session.source_context,
                            self.current_module_idx,
                            ext_expr_idx.?,
                            &.{field_projection},
                        ),
                    ),
                    false,
                );
            }

            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            for (ordered_locals) |maybe_field_local| {
                try self.scratch_local_ids.append(maybe_field_local orelse std.debug.panic(
                    "statement-only MIR invariant violated: record field local missing for expr {d}",
                    .{@intFromEnum(expr_idx)},
                ));
            }
            const fields = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));

            const assign_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                .target = target,
                .fields = fields,
                .next = next,
            } });

            var lowered_next = assign_stmt;

            if (ext_local) |extension_local| {
                var i: usize = mono_fields.len;
                while (i > 0) {
                    i -= 1;
                    if (provided[i]) continue;
                    const ext_field_idx = self.recordFieldIndexByName(mono_fields[i].name, ext_fields);
                    const field_projection: Pipeline.ValueProjection = .{ .field = .{
                        .module_idx = mono_fields[i].name.module_idx,
                        .ident = mono_fields[i].name.ident,
                    } };
                    lowered_next = try self.lowerRefIntoWithCallable(
                        ordered_locals[i].?,
                        .{ .field = .{
                            .source = extension_local,
                            .field_idx = ext_field_idx,
                            .ownership = .move,
                        } },
                        if (try self.resolveProjectedExprCallableValue(
                            session.source_context,
                            self.current_module_idx,
                            ext_expr_idx.?,
                            &.{field_projection},
                        )) |callable_value|
                            try self.callableResolutionFromValue(callable_value)
                        else
                            null,
                        lowered_next,
                    );
                }

                lowered_next = try self.lowerCirExprInto(session, ext_expr_idx.?, extension_local, lowered_next);
            }

            const source_fields = module_env.store.sliceRecordFields(record.fields);
            var i: usize = source_fields.len;
            while (i > 0) {
                i -= 1;
                const field = module_env.store.getRecordField(source_fields[i]);
                const canonical_idx = self.recordFieldIndexByName(field.name, mono_fields);
                lowered_next = try self.lowerCirExprInto(session, field.value, ordered_locals[canonical_idx].?, lowered_next);
            }
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

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);

            var lowered_next = assign_stmt;
            var i: usize = args.len;
            while (i > 0) {
                i -= 1;
                const arg_idx = args[i];
                const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, arg_idx), false);
                try self.scratch_local_ids.append(arg_local);
                lowered_next = try self.lowerCirExprInto(session, arg_idx, arg_local, lowered_next);
            }
            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const arg_span = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            try self.store.finalizeCFStmt(assign_stmt, .{ .assign_tag = .{
                .target = target,
                .name = self.resolveTagNameForMonotype(monotype, tag.name),
                .args = arg_span,
                .next = next,
            } });
            break :blk lowered_next;
        },
        .e_nominal => |nominal| blk: {
            const backing_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, nominal.backing_expr), false);
            const nominal_stmt = try self.lowerRefInto(target, .{ .nominal = .{ .backing = backing_local } }, next);
            break :blk try self.lowerCirExprInto(session, nominal.backing_expr, backing_local, nominal_stmt);
        },
        .e_nominal_external => |nominal| blk: {
            const backing_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, nominal.backing_expr), false);
            const nominal_stmt = try self.lowerRefInto(target, .{ .nominal = .{ .backing = backing_local } }, next);
            break :blk try self.lowerCirExprInto(session, nominal.backing_expr, backing_local, nominal_stmt);
        },
        .e_lookup_local => |lookup| self.lowerLookupLocalInto(session, expr_idx, lookup, target, next),
        .e_lookup_external => |lookup| self.lowerLookupExternalInto(session, expr_idx, module_env, lookup, target, next),
        .e_lookup_required => |lookup| self.lowerLookupRequiredInto(session, expr_idx, module_env, lookup, target, next),
        .e_lambda => |lambda| blk: {
            _ = lambda;
            break :blk try self.lowerProgramCallableIntroInto(session, expr_idx, target, next, "lambda");
        },
        .e_closure => |closure| blk: {
            _ = closure;
            break :blk try self.lowerProgramCallableIntroInto(session, expr_idx, target, next, "closure");
        },
        .e_block => |block| blk: {
            const lowered = try self.lowerBlockExprIntoWithExitScope(session, block, target, next);
            self.current_binding_scope = lowered.exit_scope;
            break :blk lowered.entry;
        },
        .e_dot_access => |da| self.lowerDotAccessInto(session, expr_idx, da, target, next),
        .e_tuple_access => |tuple_access| self.lowerTupleAccessInto(session, expr_idx, tuple_access, target, next),
        .e_binop => |binop| self.lowerBinopInto(session, binop, target, next),
        .e_unary_minus => |um| self.lowerUnaryMinusInto(session, um, target, next),
        .e_unary_not => |unary| self.lowerUnaryNotInto(session, unary, target, next),
        .e_call => |call| self.lowerCallInto(session, module_env, expr_idx, call, target, next),
        .e_match => |match_expr| blk: {
            const lowered = try self.lowerMatchIntoWithExitScope(session, module_env, match_expr, target, next);
            self.current_binding_scope = lowered.exit_scope;
            break :blk lowered.entry;
        },
        .e_if => |if_expr| blk: {
            const lowered = try self.lowerBoolBranchChainIntoWithExitScope(
                session,
                module_env.store.sliceIfBranches(if_expr.branches),
                if_expr.final_else,
                target,
                next,
            );
            self.current_binding_scope = lowered.exit_scope;
            break :blk lowered.entry;
        },
        .e_dbg => |dbg_expr| blk: {
            const value_mono = try self.resolveMonotype(session, dbg_expr.expr);
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
            break :blk try self.lowerCirExprInto(session, dbg_expr.expr, value_local, inspect_stmt);
        },
        .e_expect => |expect_expr| blk: {
            const cond_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, expect_expr.body), false);
            const result_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_struct = .{
                .target = target,
                .fields = MIR.LocalSpan.empty(),
                .next = next,
            } });
            const mir_expect = try self.store.addCFStmt(self.allocator, .{ .expect = .{
                .condition = cond_local,
                .next = result_stmt,
            } });
            break :blk try self.lowerCirExprInto(session, expect_expr.body, cond_local, mir_expect);
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

                const source_mono = try self.resolveMonotype(session, cir_args[0]);
                const source_local = try self.freshSyntheticLocal(source_mono, false);
                const inspect_stmt = try self.lowerStrInspectLocalInto(source_local, source_mono, target, next);
                break :blk try self.lowerCirExprInto(session, cir_args[0], source_local, inspect_stmt);
            }

            const top = self.scratch_local_ids.top();
            defer self.scratch_local_ids.clearFrom(top);

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);

            var lowered_next = assign_stmt;
            var i: usize = cir_args.len;
            while (i > 0) {
                i -= 1;
                const arg_idx = cir_args[i];
                const arg_local = try self.freshSyntheticLocal(
                    try self.resolveMonotype(session, arg_idx),
                    false,
                );
                try self.scratch_local_ids.append(arg_local);
                lowered_next = try self.lowerCirExprInto(session, arg_idx, arg_local, lowered_next);
            }

            std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
            const args = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
            try self.store.finalizeCFStmt(assign_stmt, .{ .assign_low_level = .{
                .target = target,
                .op = run_ll.op,
                .args = args,
                .next = next,
            } });
            break :blk lowered_next;
        },
        .e_runtime_error => |err| self.store.addCFStmt(self.allocator, .{ .runtime_error = .{
            .can_diagnostic = err.diagnostic,
        } }),
        .e_return => |return_expr| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(session, return_expr.expr), false);
            const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = value_local } });
            break :blk try self.lowerCirExprInto(session, return_expr.expr, value_local, ret_stmt);
        },
        .e_type_var_dispatch => |dispatch_expr| blk: {
            if (lookupProgramExprDispatchTarget(session, expr_idx)) |dispatch_target| {
                break :blk try self.lowerResolvedDispatchTargetCallInto(
                    session,
                    dispatch_target,
                    expr_idx,
                    target,
                    next,
                );
            }
            if (lookupProgramExprDispatchIntrinsic(session, expr_idx)) |dispatch_intrinsic| {
                break :blk try self.lowerIntrinsicDispatchInto(
                    session,
                    expr_idx,
                    dispatch_intrinsic,
                    target,
                    next,
                );
            }
            std.debug.panic(
                "statement-only MIR invariant violated: missing resolved dispatch semantics for CIR expr {d} kind=e_type_var_dispatch module={d} method={s} callable_ctx={d} root_ctx={d}",
                .{
                    @intFromEnum(expr_idx),
                    self.current_module_idx,
                    module_env.getIdent(dispatch_expr.method_name),
                    session.callableInstRawForDebug(),
                    session.rootExprRawForDebug(),
                },
            );
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

/// Resolve a CIR binding pattern to one executable MIR local.
fn patternToLocal(self: *Self, session: LowerSession, pattern_idx: CIR.Pattern.Idx) Allocator.Error!MIR.LocalId {
    const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);

    if (self.binding_records.get(key)) |binding| {
        if (!binding.shadowed) {
            if (binding.explicit_monotype) |monotype| {
                self.requireLocalMonotype(binding.local.?, monotype, "patternToLocal") catch unreachable;
            }
            return binding.local.?;
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);
    const is_top_level_pattern = isTopLevelPattern(module_env, pattern_idx);
    const use_scoped_local_ident = self.current_binding_scope != 0 and !is_top_level_pattern;

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

    const monotype = try self.requirePatternMonotype(session, pattern_idx);
    const local = try self.store.addLocal(self.allocator, .{
        .monotype = monotype,
        .reassignable = ident_idx.attributes.reassignable,
    });
    if (self.binding_records.getPtr(key)) |binding| {
        binding.local = local;
        binding.shadowed = false;
    } else {
        try self.binding_records.put(key, .{ .local = local });
    }
    return local;
}

fn patternToLocalWithMonotype(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    monotype: Monotype.Idx,
) Allocator.Error!MIR.LocalId {
    const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);

    if (self.binding_records.getPtr(key)) |binding| {
        if (!binding.shadowed) {
            try self.requireLocalMonotype(binding.local.?, monotype, "patternToLocalWithMonotype");
            binding.explicit_monotype = monotype;
            return binding.local.?;
        }
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const pattern = module_env.store.getPattern(pattern_idx);
    const is_top_level_pattern = isTopLevelPattern(module_env, pattern_idx);
    const use_scoped_local_ident = self.current_binding_scope != 0 and !is_top_level_pattern;

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

    const local = try self.store.addLocal(self.allocator, .{
        .monotype = monotype,
        .reassignable = ident_idx.attributes.reassignable,
    });
    if (self.binding_records.getPtr(key)) |binding| {
        binding.local = local;
        binding.explicit_monotype = monotype;
        binding.shadowed = false;
    } else {
        try self.binding_records.put(key, .{ .local = local, .explicit_monotype = monotype });
    }
    return local;
}

fn lookupExistingBindingLocal(self: *Self, pattern_idx: CIR.Pattern.Idx) ?MIR.LocalId {
    if (self.lookupExistingBindingLocalAtScope(self.current_binding_scope, pattern_idx)) |local| return local;
    if (self.lookupBindingRecordInScope(self.current_module_idx, self.current_binding_scope, pattern_idx)) |binding| {
        if (binding.shadowed) return null;
    }

    var scope = self.binding_scope_parents.get(self.current_binding_scope) orelse return null;
    while (true) {
        if (self.lookupExistingBindingLocalAtScope(scope, pattern_idx)) |local| return local;
        if (self.lookupBindingRecordInScope(self.current_module_idx, scope, pattern_idx)) |binding| {
            if (binding.shadowed) return null;
        }
        scope = self.binding_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn lookupUsableBindingLocalAtScope(
    self: *Self,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const local = self.lookupExistingBindingLocalAtScope(binding_scope, pattern_idx) orelse return null;
    return local;
}

fn lookupExistingBindingLocalWithinCurrentLambda(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    if (self.lookupExistingBindingLocalAtScope(self.current_binding_scope, pattern_idx)) |local| return local;
    if (self.lookupBindingRecordInScope(self.current_module_idx, self.current_binding_scope, pattern_idx)) |binding| {
        if (binding.shadowed) return null;
    }

    const boundary = self.current_lambda_scope_boundary orelse return null;
    var scope = self.binding_scope_parents.get(self.current_binding_scope) orelse return null;
    while (true) {
        if (self.lookupExistingBindingLocalAtScope(scope, pattern_idx)) |local| return local;
        if (self.lookupBindingRecordInScope(self.current_module_idx, scope, pattern_idx)) |binding| {
            if (binding.shadowed) return null;
        }
        if (scope == boundary) break;
        scope = self.binding_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn lookupExistingBindingLocalAtScope(
    self: *Self,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const local = self.lookupExistingBindingLocalInScope(
        self.current_module_idx,
        binding_scope,
        pattern_idx,
    ) orelse return null;
    const key = bindingScopeKey(self.current_module_idx, binding_scope, pattern_idx);
    if (self.binding_records.get(key)) |binding| if (!binding.shadowed) if (binding.explicit_monotype) |monotype| {
        self.requireLocalMonotype(local, monotype, "lookupExistingBindingLocalAtScope") catch unreachable;
    };
    return local;
}

fn bindBindingLocalInCurrentScope(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
) Allocator.Error!void {
    const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
    if (self.binding_records.getPtr(key)) |binding| {
        binding.local = local;
        binding.shadowed = false;
        return;
    }
    try self.binding_records.put(key, .{ .local = local });
}

fn bindBindingSourceExprRefInCurrentScope(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    source_expr_ref: LowerExprSourceRef,
) Allocator.Error!void {
    const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
    if (self.binding_records.getPtr(key)) |binding| {
        if (!binding.shadowed) binding.source_expr_ref = source_expr_ref;
        return;
    }
    try self.binding_records.put(key, .{ .source_expr_ref = source_expr_ref });
}

fn bindBindingCallableValueInCurrentScope(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    callable_value: Pipeline.CallableValue,
) Allocator.Error!void {
    const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
    if (self.binding_records.getPtr(key)) |binding| {
        if (!binding.shadowed) binding.callable_value = callable_value;
        return;
    }
    try self.binding_records.put(key, .{ .callable_value = callable_value });
}

fn shadowBindingInCurrentScope(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    const key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
    try self.binding_records.put(key, .{ .shadowed = true });
}

fn bindingScopeKey(module_idx: u32, binding_scope: u64, pattern_idx: CIR.Pattern.Idx) u128 {
    const base_key: u64 = (@as(u64, module_idx) << 32) | @intFromEnum(pattern_idx);
    return (@as(u128, binding_scope) << 64) | @as(u128, base_key);
}

fn paramPatternNeedsBindingStmt(
    self: *const Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_local: MIR.LocalId,
) bool {
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign => {
            const existing = self.lookupExistingBindingLocalInScope(
                self.current_module_idx,
                self.current_binding_scope,
                pattern_idx,
            ) orelse return true;
            return existing != source_local;
        },
        else => true,
    };
}

fn lookupExistingBindingLocalInScope(
    self: *const Self,
    module_idx: u32,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const binding = self.lookupBindingRecordInScope(module_idx, binding_scope, pattern_idx) orelse return null;
    if (binding.shadowed) return null;
    return binding.local;
}

fn lookupBindingRecordInScope(
    self: *const Self,
    module_idx: u32,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?BindingRecord {
    const base_key: u64 = (@as(u64, module_idx) << 32) | @intFromEnum(pattern_idx);
    const key: u128 = (@as(u128, binding_scope) << 64) | @as(u128, base_key);
    return self.binding_records.get(key);
}

fn lookupBindingSourceExprRefAtScope(
    self: *const Self,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?LowerExprSourceRef {
    const binding = self.lookupBindingRecordInScope(
        self.current_module_idx,
        binding_scope,
        pattern_idx,
    ) orelse return null;
    if (binding.shadowed) return null;
    return binding.source_expr_ref;
}

fn lookupBindingSourceExprRef(
    self: *const Self,
    pattern_idx: CIR.Pattern.Idx,
) ?LowerExprSourceRef {
    if (self.lookupBindingSourceExprRefAtScope(self.current_binding_scope, pattern_idx)) |source| return source;
    if (self.lookupBindingRecordInScope(self.current_module_idx, self.current_binding_scope, pattern_idx)) |binding| {
        if (binding.shadowed) return null;
    }

    var scope = self.binding_scope_parents.get(self.current_binding_scope) orelse return null;
    while (true) {
        if (self.lookupBindingSourceExprRefAtScope(scope, pattern_idx)) |source| return source;
        if (self.lookupBindingRecordInScope(self.current_module_idx, scope, pattern_idx)) |binding| {
            if (binding.shadowed) return null;
        }
        scope = self.binding_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn lookupBindingCallableValueAtScope(
    self: *const Self,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?Pipeline.CallableValue {
    const binding = self.lookupBindingRecordInScope(
        self.current_module_idx,
        binding_scope,
        pattern_idx,
    ) orelse return null;
    if (binding.shadowed) return null;
    return binding.callable_value;
}

fn lookupBindingCallableValue(
    self: *const Self,
    pattern_idx: CIR.Pattern.Idx,
) ?Pipeline.CallableValue {
    if (self.lookupBindingCallableValueAtScope(self.current_binding_scope, pattern_idx)) |callable_value| return callable_value;
    if (self.lookupBindingRecordInScope(self.current_module_idx, self.current_binding_scope, pattern_idx)) |binding| {
        if (binding.shadowed) return null;
    }

    var scope = self.binding_scope_parents.get(self.current_binding_scope) orelse return null;
    while (true) {
        if (self.lookupBindingCallableValueAtScope(scope, pattern_idx)) |callable_value| return callable_value;
        if (self.lookupBindingRecordInScope(self.current_module_idx, scope, pattern_idx)) |binding| {
            if (binding.shadowed) return null;
        }
        scope = self.binding_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn lookupExistingBindingLocalInAncestorScopes(
    self: *Self,
    module_idx: u32,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const scoped = self.lookupExistingBindingLocalInAncestorScopesWithScope(
        module_idx,
        binding_scope,
        pattern_idx,
    ) orelse return null;
    return scoped.local;
}

fn lookupUsableBindingLocalInAncestorScopes(
    self: *Self,
    module_idx: u32,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    var scope = self.binding_scope_parents.get(binding_scope) orelse return null;
    while (true) {
        if (self.lookupBindingRecordInScope(module_idx, scope, pattern_idx)) |binding| {
            if (binding.shadowed) return null;
            const local = if (module_idx == self.current_module_idx)
                self.lookupUsableBindingLocalAtScope(scope, pattern_idx)
            else
                binding.local;
            if (local) |usable| return usable;
        }

        scope = self.binding_scope_parents.get(scope) orelse break;
    }

    return null;
}

const ScopedBindingLocal = struct {
    scope: u64,
    local: MIR.LocalId,
};

fn lookupExistingBindingLocalInAncestorScopesWithScope(
    self: *Self,
    module_idx: u32,
    binding_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?ScopedBindingLocal {
    var scope = self.binding_scope_parents.get(binding_scope) orelse return null;
    while (true) {
        if (self.lookupBindingRecordInScope(module_idx, scope, pattern_idx)) |binding| {
            if (binding.shadowed) return null;
            const local = binding.local orelse return null;
            if (module_idx == self.current_module_idx) {
                const key = bindingScopeKey(module_idx, scope, pattern_idx);
                if (self.binding_records.get(key)) |current_binding| if (!current_binding.shadowed) if (current_binding.explicit_monotype) |monotype| {
                    self.requireLocalMonotype(local, monotype, "lookupExistingBindingLocalInAncestorScopesWithScope") catch unreachable;
                };
            }
            return .{
                .scope = scope,
                .local = local,
            };
        }

        scope = self.binding_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn scopeIsWithinCurrentLambda(self: *const Self, scope: u64) bool {
    const boundary = self.current_lambda_scope_boundary orelse return false;
    var cursor = scope;
    while (true) {
        if (cursor == boundary) return true;
        cursor = self.binding_scope_parents.get(cursor) orelse return false;
    }
}

fn makeSyntheticIdent(self: *Self, original_ident: Ident.Idx) Ident.Idx {
    const idx = self.next_synthetic_ident;
    self.next_synthetic_ident -= 1;
    return .{
        .attributes = original_ident.attributes,
        .idx = idx,
    };
}

fn freshStatementBindingScope(self: *Self) u64 {
    return self.freshChildBindingScope(self.current_binding_scope);
}

fn freshChildBindingScope(self: *Self, parent_scope: u64) u64 {
    const scope = self.next_statement_binding_scope;
    self.next_statement_binding_scope += 1;
    if (parent_scope != 0) {
        self.binding_scope_parents.put(scope, parent_scope) catch @panic("OutOfMemory");
    }
    return scope;
}

fn currentVisibleReassignableBindings(self: *Self) Allocator.Error![]VisibleBindingLocal {
    var bindings = std.ArrayList(VisibleBindingLocal).empty;
    errdefer bindings.deinit(self.allocator);

    var seen_patterns = std.AutoHashMap(u64, void).init(self.allocator);
    defer seen_patterns.deinit();

    var scope = self.current_binding_scope;
    while (true) {
        var it = self.binding_records.iterator();
        while (it.next()) |entry| {
            const packed_key = entry.key_ptr.*;
            const entry_scope: u64 = @truncate(packed_key >> 64);
            if (entry_scope != scope) continue;

            const base_key: u64 = @truncate(packed_key);
            const entry_module_idx: u32 = @truncate(base_key >> 32);
            if (entry_module_idx != self.current_module_idx) continue;
            if (seen_patterns.contains(base_key)) continue;

            try seen_patterns.put(base_key, {});
            const binding = entry.value_ptr.*;
            if (binding.shadowed) continue;

            const local = binding.local orelse continue;
            if (@intFromEnum(local) < self.current_body_local_floor) continue;
            if (!self.store.getLocal(local).reassignable) continue;
            try bindings.append(self.allocator, .{
                .pattern_idx = @enumFromInt(@as(u32, @truncate(base_key))),
                .local = local,
            });
        }

        if (scope == 0) break;
        scope = self.binding_scope_parents.get(scope) orelse 0;
    }

    std.mem.sort(
        VisibleBindingLocal,
        bindings.items,
        {},
        struct {
            fn lessThan(_: void, lhs: VisibleBindingLocal, rhs: VisibleBindingLocal) bool {
                return @intFromEnum(lhs.pattern_idx) < @intFromEnum(rhs.pattern_idx);
            }
        }.lessThan,
    );

    return bindings.toOwnedSlice(self.allocator);
}

fn visibleReassignableBindingsInScope(
    self: *Self,
    scope: u64,
) Allocator.Error![]VisibleBindingLocal {
    const saved_scope = self.current_binding_scope;
    self.current_binding_scope = scope;
    defer self.current_binding_scope = saved_scope;
    return self.currentVisibleReassignableBindings();
}

fn predeclareVisibleReassignableBindingsInScope(
    self: *Self,
    before_scope: u64,
    after_scope: u64,
) Allocator.Error!void {
    return self.predeclareVisibleReassignableBindingsInScopeExcluding(
        before_scope,
        after_scope,
        &.{},
    );
}

fn markVisibleReassignableBindingsEntryBoundInScope(
    self: *Self,
    scope: u64,
) Allocator.Error!void {
    const entry_bound_locals = self.current_lambda_entry_bound_locals orelse return;
    const bindings = try self.visibleReassignableBindingsInScope(scope);
    defer self.allocator.free(bindings);
    for (bindings) |binding| {
        try entry_bound_locals.add(binding.local);
    }
}

fn inheritVisibleReassignableBindingsInScope(
    self: *Self,
    before_scope: u64,
    after_scope: u64,
) Allocator.Error!void {
    return self.inheritVisibleReassignableBindingsInScopeExcluding(
        before_scope,
        after_scope,
        &.{},
    );
}

fn inheritVisibleReassignableBindingsInScopeExcluding(
    self: *Self,
    before_scope: u64,
    after_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
) Allocator.Error!void {
    const bindings = try self.visibleReassignableBindingsInScope(before_scope);
    defer self.allocator.free(bindings);

    const saved_scope = self.current_binding_scope;
    self.current_binding_scope = after_scope;
    defer self.current_binding_scope = saved_scope;

    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        if (self.lookupExistingBindingLocalInScope(
            self.current_module_idx,
            after_scope,
            binding.pattern_idx,
        ) != null) continue;

        try self.bindBindingLocalInCurrentScope(binding.pattern_idx, binding.local);
    }

    for (excluded_patterns) |excluded_pattern| {
        if (self.lookupBindingRecordInScope(self.current_module_idx, after_scope, excluded_pattern) != null) continue;
        try self.shadowBindingInCurrentScope(excluded_pattern);
    }
}

fn predeclareVisibleReassignableBindingsInScopeExcluding(
    self: *Self,
    before_scope: u64,
    after_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
) Allocator.Error!void {
    const bindings = try self.visibleReassignableBindingsInScope(before_scope);
    defer self.allocator.free(bindings);

    const saved_scope = self.current_binding_scope;
    self.current_binding_scope = after_scope;
    defer self.current_binding_scope = saved_scope;

    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        if (self.lookupExistingBindingLocalInScope(
            self.current_module_idx,
            after_scope,
            binding.pattern_idx,
        ) != null) continue;

        const source_local = binding.local;
        const new_local = try self.freshSyntheticLocal(
            self.store.getLocal(source_local).monotype,
            true,
        );
        try self.bindBindingLocalInCurrentScope(binding.pattern_idx, new_local);
    }

    for (excluded_patterns) |excluded_pattern| {
        if (self.lookupBindingRecordInScope(self.current_module_idx, after_scope, excluded_pattern) != null) continue;
        try self.shadowBindingInCurrentScope(excluded_pattern);
    }
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

/// Get the exact executable monotype for a CIR expression in the active source context.
fn resolveMonotype(self: *Self, session: LowerSession, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    if (lookupProgramExprCallableValue(session, expr_idx)) |callable_value| {
        const mono = self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value);
        return self.requireExecutableMonotype(try self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            mono.idx,
            mono.module_idx,
            self.current_module_idx,
        ));
    }
    if (lookupProgramExprIntroCallableInst(session, expr_idx)) |intro_callable_inst| {
        const mono = self.callable_pipeline.getCallableInstRuntimeMonotype(intro_callable_inst);
        return self.requireExecutableMonotype(try self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            mono.idx,
            mono.module_idx,
            self.current_module_idx,
        ));
    }
    if (try resolveProgramExprMonotype(session, expr_idx)) |mono| {
        return self.requireExecutableMonotype(try self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            mono.idx,
            mono.module_idx,
            self.current_module_idx,
        ));
    }

    const module_env = self.all_module_envs[self.current_module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const expr_region = module_env.store.getExprRegion(expr_idx);
    const source = module_env.getSourceAll();
    const snippet_start = @min(expr_region.start.offset, source.len);
    const snippet_end = @min(expr_region.end.offset, source.len);
    const program_expr_id = self.callable_pipeline.lambdamono.getExprId(
        session.source_context,
        self.current_module_idx,
        expr_idx,
    );
    const context_expr_mono = self.callable_pipeline.context_mono.getExprMonotype(
        session.source_context,
        self.current_module_idx,
        expr_idx,
    );
    const origin_expr = self.callable_pipeline.getExprOriginExpr(
        session.source_context,
        self.current_module_idx,
        expr_idx,
    );
    switch (expr) {
        .e_lookup_local => |lookup| {
            if (try resolveProgramPatternCallableValue(session, lookup.pattern_idx)) |callable_value| {
                const mono = self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value);
                return self.requireExecutableMonotype(try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    mono.idx,
                    mono.module_idx,
                    self.current_module_idx,
                ));
            }
            if (lookupProgramPatternMonotype(session, lookup.pattern_idx)) |mono| {
                return self.requireExecutableMonotype(try self.importMonotypeFromStore(
                    &self.callable_pipeline.context_mono.monotype_store,
                    mono.idx,
                    mono.module_idx,
                    self.current_module_idx,
                ));
            }
        },
        else => {},
    }
    std.debug.panic(
        "statement-only MIR invariant violated: missing executable monotype for expr {d} kind={s} region={any} snippet=\"{s}\" in module {d} source_context={s} callable_inst={d} root_source_expr={d} program_expr={s} context_expr_mono={s} origin_ctx={s} origin_module={d} origin_expr={d}",
        .{
            @intFromEnum(expr_idx),
            @tagName(expr),
            expr_region,
            source[snippet_start..snippet_end],
            self.current_module_idx,
            @tagName(session.source_context),
            session.callableInstRawForDebug(),
            session.rootExprRawForDebug(),
            if (program_expr_id != null) "yes" else "no",
            if (context_expr_mono != null) "yes" else "no",
            if (origin_expr) |origin| @tagName(origin.source_context) else "none",
            if (origin_expr) |origin| origin.module_idx else std.math.maxInt(u32),
            if (origin_expr) |origin| @intFromEnum(origin.expr_idx) else std.math.maxInt(u32),
        },
    );
}

fn requireExecutableMonotype(self: *Self, monotype: Monotype.Idx) Monotype.Idx {
    if (builtin.mode == .Debug and self.store.monotype_store.getMonotype(monotype) == .func) {
        std.debug.panic(
            "statement-only MIR invariant violated: executable monotype resolver produced source function monotype {d}",
            .{@intFromEnum(monotype)},
        );
    }
    return monotype;
}

fn currentCommonIdents(self: *const Self) ModuleEnv.CommonIdents {
    return self.all_module_envs[self.current_module_idx].idents;
}

fn requirePatternMonotype(
    self: *Self,
    session: LowerSession,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!Monotype.Idx {
    const explicit_key = bindingScopeKey(self.current_module_idx, self.current_binding_scope, pattern_idx);
    if (self.binding_records.get(explicit_key)) |binding| if (!binding.shadowed) if (binding.explicit_monotype) |monotype| {
        return monotype;
    };

    if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref| {
        var source_resolved = if (try self.resolveLowerExprSourceCallableValue(source_expr_ref)) |callable_value|
            self.callable_pipeline.getCallableValueRuntimeMonotype(callable_value)
        else
            self.callable_pipeline.getExprMonotype(
                source_expr_ref.source_context,
                source_expr_ref.module_idx,
                source_expr_ref.expr_idx,
            ) orelse Pipeline.ResolvedMonotype{
                .idx = .none,
                .module_idx = source_expr_ref.module_idx,
            };

        if (!source_resolved.isNone()) {
            var monotype = self.requireExecutableMonotype(try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                source_resolved.idx,
                source_resolved.module_idx,
                self.current_module_idx,
            ));
            for (source_expr_ref.projections) |projection| {
                monotype = try self.projectMonotypeByValueProjection(monotype, projection);
            }
            return self.requireExecutableMonotype(monotype);
        }
    }

    if (lookupProgramPatternMonotype(session, pattern_idx)) |resolved| {
        return self.requireExecutableMonotype(try self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            resolved.idx,
            resolved.module_idx,
            self.current_module_idx,
        ));
    }

    std.debug.panic(
        "statement-only MIR invariant violated: missing exact monotype for pattern {d} kind={s} in module {d} scope={d} callable_inst={d} root_source_expr={d} source_ctx={s} source_module={d} source_expr={d} source_proj_len={d} pattern_callable={s} source_callable={s} source_expr_mono={s}",
        .{
            @intFromEnum(pattern_idx),
            @tagName(self.all_module_envs[self.current_module_idx].store.getPattern(pattern_idx)),
            self.current_module_idx,
            self.current_binding_scope,
            session.callableInstRawForDebug(),
            session.rootExprRawForDebug(),
            if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref|
                @tagName(source_expr_ref.source_context)
            else
                "none",
            if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref|
                source_expr_ref.module_idx
            else
                std.math.maxInt(u32),
            if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref|
                @intFromEnum(source_expr_ref.expr_idx)
            else
                std.math.maxInt(u32),
            if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref|
                source_expr_ref.projections.len
            else
                0,
            if ((try resolveProgramPatternCallableValue(session, pattern_idx)) != null) "yes" else "no",
            if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref|
                if (self.callable_pipeline.getExprCallableValue(
                    source_expr_ref.source_context,
                    source_expr_ref.module_idx,
                    source_expr_ref.expr_idx,
                ) != null)
                    "yes"
                else
                    "no"
            else
                "no",
            if (self.lookupBindingSourceExprRef(pattern_idx)) |source_expr_ref|
                if (self.callable_pipeline.getExprMonotype(
                    source_expr_ref.source_context,
                    source_expr_ref.module_idx,
                    source_expr_ref.expr_idx,
                ) != null)
                    "yes"
                else
                    "no"
            else
                "no",
        },
    );
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
