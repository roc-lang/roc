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
    callable_inst_id: Pipeline.CallableInstId,
};

const ClosureLowerPlan = struct {
    recursive_members: std.ArrayList(RecursiveGroupMember),

    fn deinit(self: *ClosureLowerPlan, allocator: Allocator) void {
        self.recursive_members.deinit(allocator);
    }
};

const ImplicitLambdaCapture = struct {
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
    monotype: Monotype.Idx,
    exact_callable_inst_id: ?Pipeline.CallableInstId,
    exact_lambda_id: ?MIR.LambdaId,
    requires_hidden_capture: bool,
};

const ExactCallableResolution = struct {
    lambda: MIR.LambdaId,
    requires_hidden_capture: bool,
};

fn implicitCaptureExactResolution(
    self: *Self,
    capture: ImplicitLambdaCapture,
) Allocator.Error!?ExactCallableResolution {
    if (capture.exact_lambda_id) |lambda_id| {
        return .{
            .lambda = lambda_id,
            .requires_hidden_capture = capture.requires_hidden_capture,
        };
    }

    if (capture.exact_callable_inst_id) |callable_inst_id| {
        return .{
            .lambda = try self.lowerResolvedCallableInstLambda(callable_inst_id),
            .requires_hidden_capture = try self.callableInstProducesClosureValue(callable_inst_id),
        };
    }

    return null;
}

const ImplicitLambdaCaptureState = struct {
    items: std.ArrayList(ImplicitLambdaCapture),
    by_pattern: std.AutoHashMap(CIR.Pattern.Idx, usize),

    fn init(allocator: Allocator) ImplicitLambdaCaptureState {
        return .{
            .items = std.ArrayList(ImplicitLambdaCapture).empty,
            .by_pattern = std.AutoHashMap(CIR.Pattern.Idx, usize).init(allocator),
        };
    }

    fn deinit(self: *ImplicitLambdaCaptureState, allocator: Allocator) void {
        self.items.deinit(allocator);
        self.by_pattern.deinit();
    }
};

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

const RuntimeCapturePatternSpan = extern struct {
    start: u32,
    len: u16,

    fn empty() RuntimeCapturePatternSpan {
        return .{ .start = 0, .len = 0 };
    }
};

const CaptureMaterialization = union(enum) {
    exact_callable: struct {
        local: MIR.LocalId,
        callable_inst_id: Pipeline.CallableInstId,
    },
    top_level_def: struct {
        local: MIR.LocalId,
        def_idx: CIR.Def.Idx,
        symbol: MIR.Symbol,
        module_idx: u32,
    },
    source_expr: struct {
        local: MIR.LocalId,
        source: Pipeline.ExprSource,
    },
};

const LoopContext = struct {
    exit_id: MIR.JoinPointId,
    exit_scope: u64,
};

const RequiredLookupTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const VisiblePatternBinding = struct {
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
current_pattern_scope: u64,

/// App module index (for resolving `e_lookup_required` from platform modules)
app_module_idx: ?u32,

/// Optional for-clause type substitutions for lowering platform modules
/// against concrete app types.
type_scope: ?*const types.TypeScope,
type_scope_module_idx: ?u32,
type_scope_caller_module_idx: ?u32,

/// Map from ((scope_key << 64) | (module_idx << 32 | CIR.Pattern.Idx)) → MIR.LocalId.
/// Used only to resolve CIR local lookups to the current MIR local during lowering.
pattern_symbols: std.AutoHashMap(u128, MIR.LocalId),

/// Scope-keyed explicit monotypes for pattern locals whose runtime type is introduced
/// by lowering itself rather than re-derived from the original pattern node.
explicit_pattern_local_monotypes: std.AutoHashMap(u128, Monotype.Idx),

/// Specialization bindings: maps polymorphic type vars to concrete monotypes.
/// Written by `bindTypeVarMonotypes`, read by `fromTypeVar`.
type_var_seen: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cycle breakers for recursive nominal types (e.g. Tree := [Leaf, Node(Tree)]).
/// Used only by `fromNominalType` during monotype construction; separate from
/// specialization bindings so monotype construction never pollutes them.
nominal_cycle_breakers: std.AutoHashMap(types.Var, Monotype.Idx),

/// Cache for callable bodies already lowered for callable instances chosen by callable_pipeline.
/// The callable value itself is rebuilt per use-site scope because captures are
/// context-sensitive, but the lowered lambda body is unique per callable inst.
lowered_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Cache for direct statement-lowered lambda expressions keyed by the full
/// lowering context that can affect the emitted callable body.
lowered_callable_lambdas: std.AutoHashMap(CallableCacheKey, MIR.LambdaId),

/// Recursion guard for named top-level constants lowered on demand.
in_progress_const_defs: std.AutoHashMap(u64, void),

/// Metadata for opaque symbol IDs; populated at symbol construction time.
symbol_metadata: std.AutoHashMap(u64, SymbolMetadata),

/// Counter for generating synthetic ident indices for polymorphic specializations.
/// Counts down from NONE - 1 to avoid collision with real idents.
next_synthetic_ident: u29,

/// Counter for assigning unique local-pattern scopes to statement-lowered lambdas.
next_statement_pattern_scope: u64,

/// Parent scope links for lexically nested statement-lowered lambdas.
pattern_scope_parents: std.AutoHashMap(u64, u64),

/// Counter for assigning unique explicit join-point ids in strongest-form MIR.
next_join_point_id: u32,

/// First local index that belongs to the currently lowered root/lambda body.
current_body_local_floor: usize,

/// Tracks callable instances currently being lowered (recursion guard).
in_progress_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Reserved callable skeletons for recursive callable-inst groups whose bodies are not
/// fully lowered yet but already need stable lambda ids.
reserved_callable_insts: std.AutoHashMap(u32, MIR.LambdaId),

/// Runtime capture patterns for lambda-template callable insts whose closure payload is
/// synthesized from implicit captures rather than an explicit CIR `e_closure`.
lowered_callable_runtime_captures: std.AutoHashMap(u32, RuntimeCapturePatternSpan),
runtime_capture_patterns: std.ArrayListUnmanaged(CIR.Pattern.Idx),

/// Memoized answer for whether a callable inst lowers to a runtime closure value.
callable_inst_produces_closure_value: std.AutoHashMap(u32, bool),

/// Recursion guard for closure-value analysis across mutually recursive callable insts.
in_progress_closure_value_callables: std.AutoHashMap(u32, void),

/// Binding patterns whose callable identity is satisfied by explicit exact-callable
/// lowering rather than ordinary value binding locals.
skipped_callable_backed_binding_patterns: std.AutoHashMap(u64, void),

/// Callable-inst context for context-sensitive staged lookup/call resolution.
current_callable_inst_context: Pipeline.CallableInstId,

/// Current explicit source context for source-level specialization lookups.
current_source_context: Pipeline.SourceContextState,

/// Demand-driven implicit captures for callable-inst lambda bodies.
current_implicit_lambda_captures: ?*ImplicitLambdaCaptureState,

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
        .current_pattern_scope = 0,
        .app_module_idx = app_module_idx,
        .type_scope = null,
        .type_scope_module_idx = null,
        .type_scope_caller_module_idx = null,
        .pattern_symbols = std.AutoHashMap(u128, MIR.LocalId).init(allocator),
        .explicit_pattern_local_monotypes = std.AutoHashMap(u128, Monotype.Idx).init(allocator),
        .type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(allocator),
        .lowered_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .lowered_callable_lambdas = std.AutoHashMap(CallableCacheKey, MIR.LambdaId).init(allocator),
        .in_progress_const_defs = std.AutoHashMap(u64, void).init(allocator),
        .symbol_metadata = std.AutoHashMap(u64, SymbolMetadata).init(allocator),
        .next_synthetic_ident = Ident.Idx.NONE.idx - 1,
        .next_statement_pattern_scope = 1,
        .pattern_scope_parents = std.AutoHashMap(u64, u64).init(allocator),
        .next_join_point_id = 0,
        .current_body_local_floor = 0,
        .in_progress_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .reserved_callable_insts = std.AutoHashMap(u32, MIR.LambdaId).init(allocator),
        .lowered_callable_runtime_captures = std.AutoHashMap(u32, RuntimeCapturePatternSpan).init(allocator),
        .runtime_capture_patterns = .empty,
        .callable_inst_produces_closure_value = std.AutoHashMap(u32, bool).init(allocator),
        .in_progress_closure_value_callables = std.AutoHashMap(u32, void).init(allocator),
        .skipped_callable_backed_binding_patterns = std.AutoHashMap(u64, void).init(allocator),
        .current_callable_inst_context = .none,
        .current_source_context = .inactive,
        .current_implicit_lambda_captures = null,
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
    self.pattern_symbols.deinit();
    self.explicit_pattern_local_monotypes.deinit();
    self.type_var_seen.deinit();
    self.nominal_cycle_breakers.deinit();
    self.lowered_callable_insts.deinit();
    self.lowered_callable_lambdas.deinit();
    self.in_progress_const_defs.deinit();
    self.symbol_metadata.deinit();
    self.pattern_scope_parents.deinit();
    self.in_progress_callable_insts.deinit();
    self.reserved_callable_insts.deinit();
    self.lowered_callable_runtime_captures.deinit();
    self.runtime_capture_patterns.deinit(self.allocator);
    self.callable_inst_produces_closure_value.deinit();
    self.in_progress_closure_value_callables.deinit();
    self.skipped_callable_backed_binding_patterns.deinit();
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

fn sourceContextForCallableInst(self: *const Self, context_callable_inst: Pipeline.CallableInstId) Pipeline.SourceContext {
    if (!context_callable_inst.isNone()) {
        return .{ .callable_inst = @enumFromInt(@intFromEnum(context_callable_inst)) };
    }
    return switch (self.current_source_context) {
        .active => |source_context| source_context,
        .inactive => std.debug.panic(
            "Lower: missing current source context for root-context lookup",
            .{},
        ),
    };
}

fn currentRootExprRawForDebug(self: *const Self) u32 {
    return switch (self.current_source_context) {
        .inactive => std.math.maxInt(u32),
        .active => |source_context| switch (source_context) {
            .root_expr => |root| @intFromEnum(root.expr_idx),
            .callable_inst, .provenance_expr, .template_expr => std.math.maxInt(u32),
        },
    };
}

fn bindRecordUnboundFlatTypeToUnit(
    self: *Self,
    fields_range: types.RecordField.SafeMultiList.Range,
) Allocator.Error!void {
    const fields_slice = self.types_store.getRecordFieldsSlice(fields_range);
    const field_vars = fields_slice.items(.var_);
    for (field_vars) |field_var| {
        try self.bindTypeVarMonotypes(field_var, self.store.monotype_store.unit_idx);
    }
}

fn bindRecordFlatTypeToUnit(self: *Self, record: types.Record) Allocator.Error!void {
    var current_row = record;

    rows: while (true) {
        const fields_slice = self.types_store.getRecordFieldsSlice(current_row.fields);
        const field_vars = fields_slice.items(.var_);
        for (field_vars) |field_var| {
            try self.bindTypeVarMonotypes(field_var, self.store.monotype_store.unit_idx);
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
                        try self.bindRecordUnboundFlatTypeToUnit(fields_range);
                        return;
                    },
                    .empty_record => return,
                    else => typeBindingInvariant(
                        "bindRecordFlatTypeToUnit(record): unexpected ext flat type '{s}'",
                        .{@tagName(ext_flat)},
                    ),
                },
                .flex, .rigid => {
                    try self.bindTypeVarMonotypes(ext_var, self.store.monotype_store.unit_idx);
                    return;
                },
                .err => typeBindingInvariant(
                    "bindRecordFlatTypeToUnit(record): error extension",
                    .{},
                ),
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
    const saved_bindings = self.type_var_seen;
    const saved_breakers = self.nominal_cycle_breakers;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_module_idx = self.mono_scratches.module_idx;
    const saved_all_module_envs = self.mono_scratches.all_module_envs;
    self.type_var_seen = bindings.*;
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.mono_scratches.ident_store = self.all_module_envs[module_idx].getIdentStoreConst();
    self.mono_scratches.module_env = self.all_module_envs[module_idx];
    self.mono_scratches.module_idx = module_idx;
    self.mono_scratches.all_module_envs = self.all_module_envs;
    defer {
        bindings.* = self.type_var_seen;
        self.type_var_seen = saved_bindings;
        self.nominal_cycle_breakers.deinit();
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

const CallableCacheKey = struct {
    context_callable_inst: Pipeline.CallableInstId,
    source_context_kind: enum(u2) { callable_inst, root_expr, provenance_expr, template_expr },
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    parent_pattern_scope: u64,
};

fn makeCallableCacheKey(
    self: *const Self,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) CallableCacheKey {
    const source_context = sourceContextForCallableInst(self, self.current_callable_inst_context);
    return .{
        .context_callable_inst = self.current_callable_inst_context,
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
        .parent_pattern_scope = self.current_pattern_scope,
    };
}

fn lookupLoweredCallableLambdaCache(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) Allocator.Error!?MIR.LambdaId {
    return self.lowered_callable_lambdas.get(makeCallableCacheKey(self, expr_idx, fn_monotype));
}

fn lookupPipelinedExprMonotype(self: *const Self, expr_idx: CIR.Expr.Idx) ?Pipeline.ResolvedMonotype {
    return self.callable_pipeline.getExprMonotype(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedPatternMonotype(self: *const Self, pattern_idx: CIR.Pattern.Idx) ?Pipeline.ResolvedMonotype {
    return self.callable_pipeline.getContextPatternMonotype(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        pattern_idx,
    );
}

fn lookupPipelinedPatternCallableInstsInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getContextPatternCallableInsts(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        pattern_idx,
    );
}

fn lookupPipelinedPatternCallableInstInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getContextPatternCallableInst(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        pattern_idx,
    );
}

fn lookupPipelinedExprCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getExprCallableInst(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedExprCallableInsts(self: *const Self, expr_idx: CIR.Expr.Idx) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getExprCallableInsts(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedLookupCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getLookupExprCallableInst(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedLookupCallableInsts(self: *const Self, expr_idx: CIR.Expr.Idx) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getLookupExprCallableInsts(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedValueExprCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Pipeline.CallableInstId {
    if (self.lookupPipelinedExprCallableInst(expr_idx)) |callable_inst_id| return callable_inst_id;
    return self.lookupPipelinedLookupCallableInst(expr_idx);
}

fn preferredCallableTemplateForValueExprInModule(
    self: *Self,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.CallableTemplateId {
    const module_env = self.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lambda, .e_closure, .e_hosted_lambda => self.callable_pipeline.lambda_solved.getExprCallableTemplate(
            module_idx,
            expr_idx,
        ),
        .e_lookup_local => |lookup| self.callable_pipeline.lambda_solved.getLocalCallableTemplate(
            module_idx,
            lookup.pattern_idx,
        ),
        .e_lookup_external => |lookup| blk: {
            const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
            break :blk self.callable_pipeline.lambda_solved.getExternalCallableTemplate(target_module_idx, lookup.target_node_idx);
        },
        .e_lookup_required => |lookup| blk: {
            const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
            break :blk self.callable_pipeline.lambda_solved.getExternalCallableTemplate(
                target.module_idx,
                @intCast(@intFromEnum(target.def_idx)),
            );
        },
        else => null,
    };
}

fn callableInstMatchesFnMonotype(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!bool {
    const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
    const imported_callable_mono = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
        fn_monotype_module_idx,
    );
    return self.monotypesStructurallyEqual(imported_callable_mono, fn_monotype);
}

fn selectCallableInstForFnMonotype(
    self: *Self,
    callable_inst_ids: []const Pipeline.CallableInstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    var resolved: ?Pipeline.CallableInstId = null;
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

fn selectCallableInstForFnMonotypeAndTemplate(
    self: *Self,
    callable_inst_ids: []const Pipeline.CallableInstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    preferred_template: Pipeline.CallableTemplateId,
) Allocator.Error!?Pipeline.CallableInstId {
    if (preferred_template.isNone()) return null;

    var resolved: ?Pipeline.CallableInstId = null;
    for (callable_inst_ids) |callable_inst_id| {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
        if (callable_inst.template != preferred_template) continue;
        if (!try self.callableInstMatchesFnMonotype(callable_inst_id, fn_monotype, fn_monotype_module_idx)) continue;
        if (resolved) |existing| {
            if (existing != callable_inst_id) return null;
        } else {
            resolved = callable_inst_id;
        }
    }

    return resolved;
}

fn lookupSpecializedCallableInstForTemplateAndMonotype(
    self: *Self,
    template_id: Pipeline.CallableTemplateId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    var resolved: ?Pipeline.CallableInstId = null;
    for (self.callable_pipeline.lambda_specialize.callable_insts.items, 0..) |callable_inst, i| {
        const callable_inst_id: Pipeline.CallableInstId = @enumFromInt(i);
        if (callable_inst.template != template_id) continue;
        if (!try self.callableInstMatchesFnMonotype(
            callable_inst_id,
            fn_monotype,
            fn_monotype_module_idx,
        )) continue;

        if (resolved) |existing| {
            if (existing != callable_inst_id) return null;
        } else {
            resolved = callable_inst_id;
        }
    }

    return resolved;
}

fn preferredCallableTemplateForValueExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.CallableTemplateId {
    return self.preferredCallableTemplateForValueExprInModule(self.current_module_idx, expr_idx);
}

fn lookupPipelinedValueExprCallableInstForMonotype(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    return self.lookupPipelinedValueExprCallableInstForMonotypeInContext(
        self.current_callable_inst_context,
        self.current_module_idx,
        expr_idx,
        fn_monotype,
        fn_monotype_module_idx,
    );
}

fn lookupPipelinedValueExprCallableInstForMonotypeInModule(
    self: *Self,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    if (module_idx == self.current_module_idx) {
        return self.lookupPipelinedValueExprCallableInstForMonotype(
            expr_idx,
            fn_monotype,
            fn_monotype_module_idx,
        );
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

    return self.lookupPipelinedValueExprCallableInstForMonotype(
        expr_idx,
        fn_monotype,
        fn_monotype_module_idx,
    );
}

fn lookupPipelinedValueExprCallableInstForMonotypeInContext(
    self: *Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    if (lookupPipelinedValueExprCallableInstsInContext(
        self,
        context_callable_inst,
        module_idx,
        expr_idx,
    )) |callable_inst_ids| {
        if (self.preferredCallableTemplateForValueExprInModule(module_idx, expr_idx)) |preferred_template| {
            if (try self.selectCallableInstForFnMonotypeAndTemplate(
                callable_inst_ids,
                fn_monotype,
                fn_monotype_module_idx,
                preferred_template,
            )) |callable_inst_id| {
                return callable_inst_id;
            }
        }
        if (try self.selectCallableInstForFnMonotype(
            callable_inst_ids,
            fn_monotype,
            fn_monotype_module_idx,
        )) |callable_inst_id| {
            return callable_inst_id;
        }
    }

    if (lookupPipelinedValueExprCallableInstInContext(
        self,
        context_callable_inst,
        module_idx,
        expr_idx,
    )) |callable_inst_id| {
        if (self.preferredCallableTemplateForValueExprInModule(module_idx, expr_idx)) |preferred_template| {
            if (self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id).template != preferred_template) {
                return null;
            }
        }
        if (try self.callableInstMatchesFnMonotype(callable_inst_id, fn_monotype, fn_monotype_module_idx)) {
            return callable_inst_id;
        }
    }

    return null;
}

fn callableInstSlicesEqualAsSet(
    lhs: []const Pipeline.CallableInstId,
    rhs: []const Pipeline.CallableInstId,
) bool {
    if (lhs.len != rhs.len) return false;

    for (lhs) |lhs_item| {
        var found = false;
        for (rhs) |rhs_item| {
            if (lhs_item == rhs_item) {
                found = true;
                break;
            }
        }
        if (!found) return false;
    }

    return true;
}

fn callableParamProjectionSeqEqual(
    self: *const Self,
    lhs: []const Pipeline.CallableParamProjection,
    rhs: []const Pipeline.CallableParamProjection,
) bool {
    if (lhs.len != rhs.len) return false;

    for (lhs, rhs) |lhs_proj, rhs_proj| {
        switch (lhs_proj) {
            .field => |lhs_name| switch (rhs_proj) {
                .field => |rhs_name| {
                    if (!self.identsStructurallyEqual(lhs_name, rhs_name)) return false;
                },
                else => return false,
            },
            .tuple_elem => |lhs_idx| switch (rhs_proj) {
                .tuple_elem => |rhs_idx| if (lhs_idx != rhs_idx) return false,
                else => return false,
            },
        }
    }

    return true;
}

const CallableExprVisitKey = struct {
    context_callable_inst_raw: u32,
    module_idx: u32,
    expr_raw: u32,
};

const CallableExprSource = struct {
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

fn lookupPipelinedExprCallableInstInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getExprCallableInst(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn lookupPipelinedExprCallableInstsInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getExprCallableInsts(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn lookupPipelinedLookupCallableInstInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getLookupExprCallableInst(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn lookupPipelinedLookupCallableInstsInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getLookupExprCallableInsts(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn lookupPipelinedValueExprCallableInstInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.CallableInstId {
    if (lookupPipelinedExprCallableInstInContext(self, context_callable_inst, module_idx, expr_idx)) |callable_inst_id| {
        return callable_inst_id;
    }
    return lookupPipelinedLookupCallableInstInContext(self, context_callable_inst, module_idx, expr_idx);
}

fn lookupPipelinedValueExprCallableInstsInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?[]const Pipeline.CallableInstId {
    const module_env = self.all_module_envs[module_idx];
    switch (module_env.store.getExpr(expr_idx)) {
        .e_lookup_local => |lookup| {
            if (lookupPipelinedPatternCallableInstsInContext(self, context_callable_inst, module_idx, lookup.pattern_idx)) |rooted| {
                return rooted;
            }
        },
        else => {},
    }

    if (lookupPipelinedExprCallableInstsInContext(self, context_callable_inst, module_idx, expr_idx)) |callable_inst_ids| {
        return callable_inst_ids;
    }

    return lookupPipelinedLookupCallableInstsInContext(self, context_callable_inst, module_idx, expr_idx);
}

fn lookupPipelinedCallSiteCallableInstInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getCallSiteCallableInst(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn lookupPipelinedCallSiteCallableInstsInContext(
    self: *const Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getCallSiteCallableInsts(
        sourceContextForCallableInst(self, context_callable_inst),
        module_idx,
        expr_idx,
    );
}

fn callableInstSetMatchesExpr(
    self: *Self,
    expected_set_id: Pipeline.CallableInstSetId,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) bool {
    const expected_members = self.callable_pipeline.lambda_specialize.getCallableInstSetMembers(
        self.callable_pipeline.lambda_specialize.getCallableInstSet(expected_set_id).members,
    );

    if (lookupPipelinedValueExprCallableInstsInContext(
        self,
        context_callable_inst,
        module_idx,
        expr_idx,
    )) |actual_members| {
        return callableInstSlicesEqualAsSet(expected_members, actual_members);
    }

    if (lookupPipelinedValueExprCallableInstInContext(
        self,
        context_callable_inst,
        module_idx,
        expr_idx,
    )) |actual_member| {
        return expected_members.len == 1 and expected_members[0] == actual_member;
    }

    return false;
}

fn callableBoundaryBodyExpr(
    self: *const Self,
    module_idx: u32,
    callable_expr_idx: CIR.Expr.Idx,
) ?CIR.Expr.Idx {
    const module_env = self.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(callable_expr_idx)) {
        .e_lambda => |lambda_expr| lambda_expr.body,
        .e_closure => |closure_expr| blk: {
            const lambda_expr = module_env.store.getExpr(closure_expr.lambda_idx);
            if (lambda_expr != .e_lambda) break :blk null;
            break :blk lambda_expr.e_lambda.body;
        },
        .e_hosted_lambda => |hosted_expr| hosted_expr.body,
        else => null,
    };
}

fn resolveRecordFieldSourceExprInContext(
    self: *Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    field_name: Monotype.Name,
    visiting: *std.AutoHashMapUnmanaged(CallableExprVisitKey, void),
) ?CallableExprSource {
    const visit_key = CallableExprVisitKey{
        .context_callable_inst_raw = @intFromEnum(context_callable_inst),
        .module_idx = module_idx,
        .expr_raw = @intFromEnum(expr_idx),
    };
    if (visiting.contains(visit_key)) return null;
    visiting.put(self.allocator, visit_key, {}) catch return null;
    defer _ = visiting.remove(visit_key);

    const module_env = self.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_record => |record| blk: {
            for (module_env.store.sliceRecordFields(record.fields)) |field_idx| {
                const field = module_env.store.getRecordField(field_idx);
                if (!self.identsStructurallyEqual(field.name, field_name)) continue;
                break :blk .{
                    .context_callable_inst = context_callable_inst,
                    .module_idx = module_idx,
                    .expr_idx = field.value,
                };
            }

            if (record.ext) |ext_expr| {
                break :blk self.resolveRecordFieldSourceExprInContext(
                    context_callable_inst,
                    module_idx,
                    ext_expr,
                    field_name,
                    visiting,
                );
            }

            break :blk null;
        },
        .e_lookup_local => |lookup| blk: {
            if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                break :blk self.resolveRecordFieldSourceExprInContext(
                    context_callable_inst,
                    source.module_idx,
                    source.expr_idx,
                    field_name,
                    visiting,
                );
            }
            if (findDefByPattern(module_env, lookup.pattern_idx)) |def_idx| {
                break :blk self.resolveRecordFieldSourceExprInContext(
                    context_callable_inst,
                    module_idx,
                    module_env.store.getDef(def_idx).expr,
                    field_name,
                    visiting,
                );
            }
            break :blk null;
        },
        .e_lookup_external => |lookup| blk: {
            const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
            if (!self.all_module_envs[target_module_idx].store.isDefNode(lookup.target_node_idx)) break :blk null;
            const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
            break :blk self.resolveRecordFieldSourceExprInContext(
                .none,
                target_module_idx,
                self.all_module_envs[target_module_idx].store.getDef(def_idx).expr,
                field_name,
                visiting,
            );
        },
        .e_lookup_required => |lookup| blk: {
            const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
            break :blk self.resolveRecordFieldSourceExprInContext(
                .none,
                target.module_idx,
                self.all_module_envs[target.module_idx].store.getDef(target.def_idx).expr,
                field_name,
                visiting,
            );
        },
        .e_block => |block| self.resolveRecordFieldSourceExprInContext(context_callable_inst, module_idx, block.final_expr, field_name, visiting),
        .e_dbg => |dbg_expr| self.resolveRecordFieldSourceExprInContext(context_callable_inst, module_idx, dbg_expr.expr, field_name, visiting),
        .e_expect => |expect_expr| self.resolveRecordFieldSourceExprInContext(context_callable_inst, module_idx, expect_expr.body, field_name, visiting),
        .e_return => |return_expr| self.resolveRecordFieldSourceExprInContext(context_callable_inst, module_idx, return_expr.expr, field_name, visiting),
        .e_nominal => |nominal_expr| self.resolveRecordFieldSourceExprInContext(context_callable_inst, module_idx, nominal_expr.backing_expr, field_name, visiting),
        .e_nominal_external => |nominal_expr| self.resolveRecordFieldSourceExprInContext(context_callable_inst, module_idx, nominal_expr.backing_expr, field_name, visiting),
        .e_call => |call_expr| blk: {
            if (self.getCallLowLevelOp(module_env, call_expr.func) != null) break :blk null;

            if (lookupPipelinedCallSiteCallableInstsInContext(
                self,
                context_callable_inst,
                module_idx,
                expr_idx,
            )) |callable_inst_ids| {
                if (callable_inst_ids.len != 1) {
                    std.debug.panic(
                        "statement-only MIR TODO: exact callable matching through projected call-result record fields with ambiguous callee instantiations is not implemented yet",
                        .{},
                    );
                }
            }

            const callee_callable_inst_id = lookupPipelinedCallSiteCallableInstInContext(
                self,
                context_callable_inst,
                module_idx,
                expr_idx,
            ) orelse break :blk null;
            const callee_callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callee_callable_inst_id);
            const callee_template = self.callable_pipeline.lambda_solved.getCallableTemplate(callee_callable_inst.template);
            const body_expr = self.callableBoundaryBodyExpr(callee_template.module_idx, callee_template.cir_expr) orelse break :blk null;
            break :blk self.resolveRecordFieldSourceExprInContext(
                callee_callable_inst_id,
                callee_template.module_idx,
                body_expr,
                field_name,
                visiting,
            );
        },
        else => null,
    };
}

fn resolveTupleElemSourceExprInContext(
    self: *Self,
    context_callable_inst: Pipeline.CallableInstId,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    elem_idx: u32,
    visiting: *std.AutoHashMapUnmanaged(CallableExprVisitKey, void),
) ?CallableExprSource {
    const visit_key = CallableExprVisitKey{
        .context_callable_inst_raw = @intFromEnum(context_callable_inst),
        .module_idx = module_idx,
        .expr_raw = @intFromEnum(expr_idx),
    };
    if (visiting.contains(visit_key)) return null;
    visiting.put(self.allocator, visit_key, {}) catch return null;
    defer _ = visiting.remove(visit_key);

    const module_env = self.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_tuple => |tuple_expr| blk: {
            const elems = module_env.store.sliceExpr(tuple_expr.elems);
            if (elem_idx >= elems.len) break :blk null;
            break :blk .{
                .context_callable_inst = context_callable_inst,
                .module_idx = module_idx,
                .expr_idx = elems[elem_idx],
            };
        },
        .e_lookup_local => |lookup| blk: {
            if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(module_idx, lookup.pattern_idx)) |source| {
                break :blk self.resolveTupleElemSourceExprInContext(
                    context_callable_inst,
                    source.module_idx,
                    source.expr_idx,
                    elem_idx,
                    visiting,
                );
            }
            if (findDefByPattern(module_env, lookup.pattern_idx)) |def_idx| {
                break :blk self.resolveTupleElemSourceExprInContext(
                    context_callable_inst,
                    module_idx,
                    module_env.store.getDef(def_idx).expr,
                    elem_idx,
                    visiting,
                );
            }
            break :blk null;
        },
        .e_lookup_external => |lookup| blk: {
            const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
            if (!self.all_module_envs[target_module_idx].store.isDefNode(lookup.target_node_idx)) break :blk null;
            const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
            break :blk self.resolveTupleElemSourceExprInContext(
                .none,
                target_module_idx,
                self.all_module_envs[target_module_idx].store.getDef(def_idx).expr,
                elem_idx,
                visiting,
            );
        },
        .e_lookup_required => |lookup| blk: {
            const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
            break :blk self.resolveTupleElemSourceExprInContext(
                .none,
                target.module_idx,
                self.all_module_envs[target.module_idx].store.getDef(target.def_idx).expr,
                elem_idx,
                visiting,
            );
        },
        .e_block => |block| self.resolveTupleElemSourceExprInContext(context_callable_inst, module_idx, block.final_expr, elem_idx, visiting),
        .e_dbg => |dbg_expr| self.resolveTupleElemSourceExprInContext(context_callable_inst, module_idx, dbg_expr.expr, elem_idx, visiting),
        .e_expect => |expect_expr| self.resolveTupleElemSourceExprInContext(context_callable_inst, module_idx, expect_expr.body, elem_idx, visiting),
        .e_return => |return_expr| self.resolveTupleElemSourceExprInContext(context_callable_inst, module_idx, return_expr.expr, elem_idx, visiting),
        .e_nominal => |nominal_expr| self.resolveTupleElemSourceExprInContext(context_callable_inst, module_idx, nominal_expr.backing_expr, elem_idx, visiting),
        .e_nominal_external => |nominal_expr| self.resolveTupleElemSourceExprInContext(context_callable_inst, module_idx, nominal_expr.backing_expr, elem_idx, visiting),
        .e_call => |call_expr| blk: {
            if (self.getCallLowLevelOp(module_env, call_expr.func) != null) break :blk null;

            if (lookupPipelinedCallSiteCallableInstsInContext(
                self,
                context_callable_inst,
                module_idx,
                expr_idx,
            )) |callable_inst_ids| {
                if (callable_inst_ids.len != 1) {
                    std.debug.panic(
                        "statement-only MIR TODO: exact callable matching through projected call-result tuple elements with ambiguous callee instantiations is not implemented yet",
                        .{},
                    );
                }
            }

            const callee_callable_inst_id = lookupPipelinedCallSiteCallableInstInContext(
                self,
                context_callable_inst,
                module_idx,
                expr_idx,
            ) orelse break :blk null;
            const callee_callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callee_callable_inst_id);
            const callee_template = self.callable_pipeline.lambda_solved.getCallableTemplate(callee_callable_inst.template);
            const body_expr = self.callableBoundaryBodyExpr(callee_template.module_idx, callee_template.cir_expr) orelse break :blk null;
            break :blk self.resolveTupleElemSourceExprInContext(
                callee_callable_inst_id,
                callee_template.module_idx,
                body_expr,
                elem_idx,
                visiting,
            );
        },
        else => null,
    };
}

fn resolveProjectedSourceExactCallableInst(
    self: *Self,
    source: CallableExprSource,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    return self.lookupPipelinedValueExprCallableInstForMonotypeInContext(
        source.context_callable_inst,
        source.module_idx,
        source.expr_idx,
        fn_monotype,
        fn_monotype_module_idx,
    );
}

fn resolveRecordFieldExactCallableInst(
    self: *Self,
    receiver_expr_idx: CIR.Expr.Idx,
    field_name: Monotype.Name,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    var visiting: std.AutoHashMapUnmanaged(CallableExprVisitKey, void) = .empty;
    defer visiting.deinit(self.allocator);

    const source = self.resolveRecordFieldSourceExprInContext(
        self.current_callable_inst_context,
        self.current_module_idx,
        receiver_expr_idx,
        field_name,
        &visiting,
    ) orelse return null;
    return try self.resolveProjectedSourceExactCallableInst(source, fn_monotype, fn_monotype_module_idx);
}

fn resolveTupleElemExactCallableInst(
    self: *Self,
    tuple_expr_idx: CIR.Expr.Idx,
    elem_idx: u32,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    var visiting: std.AutoHashMapUnmanaged(CallableExprVisitKey, void) = .empty;
    defer visiting.deinit(self.allocator);

    const source = self.resolveTupleElemSourceExprInContext(
        self.current_callable_inst_context,
        self.current_module_idx,
        tuple_expr_idx,
        elem_idx,
        &visiting,
    ) orelse return null;

    return self.resolveProjectedSourceExactCallableInst(source, fn_monotype, fn_monotype_module_idx);
}

fn callableInstMatchesArgumentSpecs(
    self: *Self,
    candidate_inst_id: Pipeline.CallableInstId,
    caller_module_idx: u32,
    arg_exprs: []const CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
) Allocator.Error!bool {
    const candidate_inst = self.callable_pipeline.lambda_specialize.getCallableInst(candidate_inst_id);
    const specs = self.callable_pipeline.lambda_specialize.getCallableParamSpecEntries(candidate_inst.callable_param_specs);
    const matched = try self.allocator.alloc(bool, specs.len);
    defer self.allocator.free(matched);
    @memset(matched, false);

    const func_mono = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func,
        else => return specs.len == 0,
    };
    const param_monos = self.store.monotype_store.getIdxSpan(func_mono.args);
    if (param_monos.len != arg_exprs.len) unreachable;

    var projections = std.ArrayListUnmanaged(Pipeline.CallableParamProjection).empty;
    defer projections.deinit(self.allocator);

    const Visitor = struct {
        fn visit(
            lower: *Self,
            context_callable_inst: Pipeline.CallableInstId,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
            monotype: Monotype.Idx,
            param_index: u16,
            proj_stack: *std.ArrayListUnmanaged(Pipeline.CallableParamProjection),
            candidate_specs: []const Pipeline.CallableParamSpecEntry,
            candidate_matched: []bool,
        ) Allocator.Error!bool {
            switch (lower.store.monotype_store.getMonotype(monotype)) {
                .func => {
                    var spec_index: ?usize = null;
                    for (candidate_specs, 0..) |spec, i| {
                        if (spec.param_index != param_index) continue;
                        if (!lower.callableParamProjectionSeqEqual(
                            lower.callable_pipeline.getCallableParamProjectionEntries(spec.projections),
                            proj_stack.items,
                        )) continue;
                        spec_index = i;
                        break;
                    }

                    const matched_index = spec_index orelse return false;
                    if (candidate_matched[matched_index]) return false;
                    if (!lower.callableInstSetMatchesExpr(
                        candidate_specs[matched_index].callable_inst_set_id,
                        context_callable_inst,
                        module_idx,
                        expr_idx,
                    )) return false;
                    candidate_matched[matched_index] = true;
                    return true;
                },
                .record => |record| {
                    for (lower.store.monotype_store.getFields(record.fields)) |field| {
                        var visiting: std.AutoHashMapUnmanaged(CallableExprVisitKey, void) = .empty;
                        defer visiting.deinit(lower.allocator);
                        const source = lower.resolveRecordFieldSourceExprInContext(
                            context_callable_inst,
                            module_idx,
                            expr_idx,
                            field.name,
                            &visiting,
                        ) orelse return false;
                        try proj_stack.append(lower.allocator, .{ .field = field.name });
                        defer _ = proj_stack.pop();
                        if (!(try visit(
                            lower,
                            source.context_callable_inst,
                            source.module_idx,
                            source.expr_idx,
                            field.type_idx,
                            param_index,
                            proj_stack,
                            candidate_specs,
                            candidate_matched,
                        ))) return false;
                    }
                    return true;
                },
                .tuple => |tuple_mono| {
                    const elems = lower.store.monotype_store.getIdxSpan(tuple_mono.elems);
                    for (elems, 0..) |elem_mono, elem_idx| {
                        var visiting: std.AutoHashMapUnmanaged(CallableExprVisitKey, void) = .empty;
                        defer visiting.deinit(lower.allocator);
                        const source = lower.resolveTupleElemSourceExprInContext(
                            context_callable_inst,
                            module_idx,
                            expr_idx,
                            @intCast(elem_idx),
                            &visiting,
                        ) orelse return false;
                        try proj_stack.append(lower.allocator, .{ .tuple_elem = @intCast(elem_idx) });
                        defer _ = proj_stack.pop();
                        if (!(try visit(
                            lower,
                            source.context_callable_inst,
                            source.module_idx,
                            source.expr_idx,
                            elem_mono,
                            param_index,
                            proj_stack,
                            candidate_specs,
                            candidate_matched,
                        ))) return false;
                    }
                    return true;
                },
                else => return true,
            }
        }
    };

    for (arg_exprs, 0..) |arg_expr_idx, param_index| {
        if (!(try Visitor.visit(
            self,
            self.current_callable_inst_context,
            caller_module_idx,
            arg_expr_idx,
            param_monos[param_index],
            @intCast(param_index),
            &projections,
            specs,
            matched,
        ))) return false;
    }

    for (matched) |was_matched| {
        if (!was_matched) return false;
    }

    return true;
}

fn selectExactCallCallableInstFromCandidates(
    self: *Self,
    callable_inst_ids: []const Pipeline.CallableInstId,
    arg_exprs: []const CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    var resolved: ?Pipeline.CallableInstId = null;

    for (callable_inst_ids) |callable_inst_id| {
        const mono_matches = try self.callableInstMatchesFnMonotype(callable_inst_id, fn_monotype, fn_monotype_module_idx);
        const args_match = mono_matches and (try self.callableInstMatchesArgumentSpecs(
            callable_inst_id,
            self.current_module_idx,
            arg_exprs,
            fn_monotype,
        ));
        if (!args_match) continue;

        if (resolved) |existing| {
            if (existing != callable_inst_id) return null;
        } else {
            resolved = callable_inst_id;
        }
    }

    return resolved;
}

fn resolveExactCallCallableInstFromArgs(
    self: *Self,
    callee_expr: CIR.Expr.Idx,
    arg_exprs: []const CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    if (self.lookupPipelinedValueExprCallableInsts(callee_expr)) |callable_inst_ids| {
        if (try self.selectExactCallCallableInstFromCandidates(
            callable_inst_ids,
            arg_exprs,
            fn_monotype,
            fn_monotype_module_idx,
        )) |resolved| {
            return resolved;
        }
    }

    if (self.lookupPipelinedValueExprCallableInst(callee_expr)) |callable_inst_id| {
        if (try self.selectExactCallCallableInstFromCandidates(
            &.{callable_inst_id},
            arg_exprs,
            fn_monotype,
            fn_monotype_module_idx,
        )) |resolved| {
            return resolved;
        }
    }

    return null;
}

fn lookupPipelinedValueExprCallableInsts(self: *const Self, expr_idx: CIR.Expr.Idx) ?[]const Pipeline.CallableInstId {
    const module_env = self.all_module_envs[self.current_module_idx];
    switch (module_env.store.getExpr(expr_idx)) {
        .e_lookup_local => |lookup| {
            if (lookupPipelinedPatternCallableInstsInContext(
                self,
                self.current_callable_inst_context,
                self.current_module_idx,
                lookup.pattern_idx,
            )) |rooted| {
                return rooted;
            }
        },
        else => {},
    }

    if (self.lookupPipelinedExprCallableInsts(expr_idx)) |callable_inst_ids| return callable_inst_ids;
    return self.lookupPipelinedLookupCallableInsts(expr_idx);
}

fn lookupPipelinedPatternCallableInsts(
    self: *const Self,
    pattern_idx: CIR.Pattern.Idx,
) ?[]const Pipeline.CallableInstId {
    return lookupPipelinedPatternCallableInstsInContext(
        self,
        self.current_callable_inst_context,
        self.current_module_idx,
        pattern_idx,
    );
}

fn lookupPipelinedPatternCallableInstForMonotype(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    if (self.lookupPipelinedPatternCallableInsts(pattern_idx)) |callable_inst_ids| {
        return self.selectCallableInstForFnMonotype(callable_inst_ids, fn_monotype, fn_monotype_module_idx);
    }

    return null;
}

fn lookupPipelinedCallSiteCallableInst(self: *const Self, expr_idx: CIR.Expr.Idx) ?Pipeline.CallableInstId {
    return self.callable_pipeline.getCallSiteCallableInst(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedCallSiteCallableInsts(
    self: *const Self,
    expr_idx: CIR.Expr.Idx,
) ?[]const Pipeline.CallableInstId {
    return self.callable_pipeline.getCallSiteCallableInsts(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
    );
}

fn lookupPipelinedDispatchTarget(
    self: *const Self,
    expr_idx: CIR.Expr.Idx,
) ?Pipeline.DispatchExprTarget {
    return self.callable_pipeline.getDispatchExprTarget(
        sourceContextForCallableInst(self, self.current_callable_inst_context),
        self.current_module_idx,
        expr_idx,
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
    const saved_source_context = self.current_source_context;
    if (self.current_callable_inst_context.isNone()) {
        switch (self.current_source_context) {
            .inactive => {
                self.current_source_context = .{ .active = .{ .root_expr = .{
                    .module_idx = self.current_module_idx,
                    .expr_idx = expr_idx,
                } } };
            },
            .active => {},
        }
    }
    defer self.current_source_context = saved_source_context;
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

    const id = try self.store.registerConstDef(self.allocator, .{
        .symbol = symbol,
        .body = body,
        .monotype = monotype,
        .source_region = region,
    });
    if (builtin.mode == .Debug) {
        try DebugVerifyMir.verifyConst(self.allocator, self.store, id);
    }
    return id;
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

fn sourceExprResultMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!Monotype.Idx {
    if (try self.semanticExprResultMonotype(expr_idx)) |monotype| {
        return monotype;
    }

    return self.resolveMonotype(expr_idx);
}

fn boolMonotype(self: *Self) Allocator.Error!Monotype.Idx {
    return self.store.monotype_store.addBoolTagUnion(
        self.allocator,
        self.current_module_idx,
        self.currentCommonIdents(),
    );
}

fn explicitExprMonotypeInModule(
    self: *Self,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!?Monotype.Idx {
    if (module_idx == self.current_module_idx) {
        return self.explicitExprMonotype(expr_idx);
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

    return self.explicitExprMonotype(expr_idx);
}

fn lowLevelResultMonotypeFromArgs(
    self: *Self,
    op: CIR.Expr.LowLevel,
    arg_exprs: []const CIR.Expr.Idx,
) Allocator.Error!?Monotype.Idx {
    return switch (op) {
        .str_is_eq,
        .str_contains,
        .str_caseless_ascii_equals,
        .str_starts_with,
        .str_ends_with,
        .bool_not,
        .num_is_eq,
        .num_is_gt,
        .num_is_gte,
        .num_is_lt,
        .num_is_lte,
        => try self.boolMonotype(),

        .str_concat,
        .str_trim,
        .str_trim_start,
        .str_trim_end,
        .str_with_ascii_lowercased,
        .str_with_ascii_uppercased,
        .str_repeat,
        .str_with_capacity,
        .str_reserve,
        .str_release_excess_capacity,
        .str_from_utf8_lossy,
        .str_join_with,
        .str_inspect,
        .u8_to_str,
        .i8_to_str,
        .u16_to_str,
        .i16_to_str,
        .u32_to_str,
        .i32_to_str,
        .u64_to_str,
        .i64_to_str,
        .u128_to_str,
        .i128_to_str,
        .dec_to_str,
        .f32_to_str,
        .f64_to_str,
        .num_to_str,
        => self.store.monotype_store.primIdx(.str),

        .str_count_utf8_bytes,
        .list_len,
        => self.store.monotype_store.primIdx(.u64),

        .list_concat,
        .list_append_unsafe,
        .list_drop_at,
        .list_sublist,
        .list_set,
        .list_prepend,
        .list_drop_first,
        .list_drop_last,
        .list_take_first,
        .list_take_last,
        .list_reverse,
        .list_reserve,
        .list_release_excess_capacity,
        => if (arg_exprs.len == 0) null else try self.explicitExprMonotype(arg_exprs[0]),

        .list_with_capacity => null,

        .list_get_unsafe => if (arg_exprs.len == 0) null else blk: {
            const list_mono = (try self.explicitExprMonotype(arg_exprs[0])) orelse break :blk null;
            break :blk switch (self.store.monotype_store.getMonotype(list_mono)) {
                .list => |list| list.elem,
                else => null,
            };
        },

        .num_negate,
        .num_abs,
        .num_abs_diff,
        .num_plus,
        .num_minus,
        .num_times,
        .num_div_by,
        .num_div_trunc_by,
        .num_rem_by,
        .num_mod_by,
        .num_pow,
        .num_sqrt,
        .num_log,
        .num_round,
        .num_floor,
        .num_ceiling,
        .num_shift_left_by,
        .num_shift_right_by,
        .num_shift_right_zf_by,
        => if (arg_exprs.len == 0) null else try self.explicitExprMonotype(arg_exprs[0]),

        .u8_to_i8_wrap,
        .u8_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .u8_to_i16 => self.store.monotype_store.primIdx(.i16),
        .u8_to_i32 => self.store.monotype_store.primIdx(.i32),
        .u8_to_i64 => self.store.monotype_store.primIdx(.i64),
        .u8_to_i128 => self.store.monotype_store.primIdx(.i128),
        .u8_to_u16 => self.store.monotype_store.primIdx(.u16),
        .u8_to_u32 => self.store.monotype_store.primIdx(.u32),
        .u8_to_u64 => self.store.monotype_store.primIdx(.u64),
        .u8_to_u128 => self.store.monotype_store.primIdx(.u128),
        .u8_to_f32 => self.store.monotype_store.primIdx(.f32),
        .u8_to_f64 => self.store.monotype_store.primIdx(.f64),
        .u8_to_dec => self.store.monotype_store.primIdx(.dec),

        .i8_to_i16 => self.store.monotype_store.primIdx(.i16),
        .i8_to_i32 => self.store.monotype_store.primIdx(.i32),
        .i8_to_i64 => self.store.monotype_store.primIdx(.i64),
        .i8_to_i128 => self.store.monotype_store.primIdx(.i128),
        .i8_to_u8_wrap,
        .i8_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .i8_to_u16_wrap,
        .i8_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .i8_to_u32_wrap,
        .i8_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .i8_to_u64_wrap,
        .i8_to_u64_try,
        => self.store.monotype_store.primIdx(.u64),
        .i8_to_u128_wrap,
        .i8_to_u128_try,
        => self.store.monotype_store.primIdx(.u128),
        .i8_to_f32 => self.store.monotype_store.primIdx(.f32),
        .i8_to_f64 => self.store.monotype_store.primIdx(.f64),
        .i8_to_dec => self.store.monotype_store.primIdx(.dec),

        .u16_to_i8_wrap,
        .u16_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .u16_to_i16_wrap,
        .u16_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .u16_to_i32 => self.store.monotype_store.primIdx(.i32),
        .u16_to_i64 => self.store.monotype_store.primIdx(.i64),
        .u16_to_i128 => self.store.monotype_store.primIdx(.i128),
        .u16_to_u8_wrap,
        .u16_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .u16_to_u32 => self.store.monotype_store.primIdx(.u32),
        .u16_to_u64 => self.store.monotype_store.primIdx(.u64),
        .u16_to_u128 => self.store.monotype_store.primIdx(.u128),
        .u16_to_f32 => self.store.monotype_store.primIdx(.f32),
        .u16_to_f64 => self.store.monotype_store.primIdx(.f64),
        .u16_to_dec => self.store.monotype_store.primIdx(.dec),

        .i16_to_i8_wrap,
        .i16_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .i16_to_i32 => self.store.monotype_store.primIdx(.i32),
        .i16_to_i64 => self.store.monotype_store.primIdx(.i64),
        .i16_to_i128 => self.store.monotype_store.primIdx(.i128),
        .i16_to_u8_wrap,
        .i16_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .i16_to_u16_wrap,
        .i16_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .i16_to_u32_wrap,
        .i16_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .i16_to_u64_wrap,
        .i16_to_u64_try,
        => self.store.monotype_store.primIdx(.u64),
        .i16_to_u128_wrap,
        .i16_to_u128_try,
        => self.store.monotype_store.primIdx(.u128),
        .i16_to_f32 => self.store.monotype_store.primIdx(.f32),
        .i16_to_f64 => self.store.monotype_store.primIdx(.f64),
        .i16_to_dec => self.store.monotype_store.primIdx(.dec),

        .u32_to_i8_wrap,
        .u32_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .u32_to_i16_wrap,
        .u32_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .u32_to_i32_wrap,
        .u32_to_i32_try,
        => self.store.monotype_store.primIdx(.i32),
        .u32_to_i64 => self.store.monotype_store.primIdx(.i64),
        .u32_to_i128 => self.store.monotype_store.primIdx(.i128),
        .u32_to_u8_wrap,
        .u32_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .u32_to_u16_wrap,
        .u32_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .u32_to_u64 => self.store.monotype_store.primIdx(.u64),
        .u32_to_u128 => self.store.monotype_store.primIdx(.u128),
        .u32_to_f32 => self.store.monotype_store.primIdx(.f32),
        .u32_to_f64 => self.store.monotype_store.primIdx(.f64),
        .u32_to_dec => self.store.monotype_store.primIdx(.dec),

        .i32_to_i8_wrap,
        .i32_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .i32_to_i16_wrap,
        .i32_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .i32_to_i64 => self.store.monotype_store.primIdx(.i64),
        .i32_to_i128 => self.store.monotype_store.primIdx(.i128),
        .i32_to_u8_wrap,
        .i32_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .i32_to_u16_wrap,
        .i32_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .i32_to_u32_wrap,
        .i32_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .i32_to_u64_wrap,
        .i32_to_u64_try,
        => self.store.monotype_store.primIdx(.u64),
        .i32_to_u128_wrap,
        .i32_to_u128_try,
        => self.store.monotype_store.primIdx(.u128),
        .i32_to_f32 => self.store.monotype_store.primIdx(.f32),
        .i32_to_f64 => self.store.monotype_store.primIdx(.f64),
        .i32_to_dec => self.store.monotype_store.primIdx(.dec),

        .u64_to_i8_wrap,
        .u64_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .u64_to_i16_wrap,
        .u64_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .u64_to_i32_wrap,
        .u64_to_i32_try,
        => self.store.monotype_store.primIdx(.i32),
        .u64_to_i64_wrap,
        .u64_to_i64_try,
        => self.store.monotype_store.primIdx(.i64),
        .u64_to_i128 => self.store.monotype_store.primIdx(.i128),
        .u64_to_u8_wrap,
        .u64_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .u64_to_u16_wrap,
        .u64_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .u64_to_u32_wrap,
        .u64_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .u64_to_u128 => self.store.monotype_store.primIdx(.u128),
        .u64_to_f32 => self.store.monotype_store.primIdx(.f32),
        .u64_to_f64 => self.store.monotype_store.primIdx(.f64),
        .u64_to_dec => self.store.monotype_store.primIdx(.dec),

        .i64_to_i8_wrap,
        .i64_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .i64_to_i16_wrap,
        .i64_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .i64_to_i32_wrap,
        .i64_to_i32_try,
        => self.store.monotype_store.primIdx(.i32),
        .i64_to_i128 => self.store.monotype_store.primIdx(.i128),
        .i64_to_u8_wrap,
        .i64_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .i64_to_u16_wrap,
        .i64_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .i64_to_u32_wrap,
        .i64_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .i64_to_u64_wrap,
        .i64_to_u64_try,
        => self.store.monotype_store.primIdx(.u64),
        .i64_to_u128_wrap,
        .i64_to_u128_try,
        => self.store.monotype_store.primIdx(.u128),
        .i64_to_f32 => self.store.monotype_store.primIdx(.f32),
        .i64_to_f64 => self.store.monotype_store.primIdx(.f64),
        .i64_to_dec => self.store.monotype_store.primIdx(.dec),

        .u128_to_i8_wrap,
        .u128_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .u128_to_i16_wrap,
        .u128_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .u128_to_i32_wrap,
        .u128_to_i32_try,
        => self.store.monotype_store.primIdx(.i32),
        .u128_to_i64_wrap,
        .u128_to_i64_try,
        => self.store.monotype_store.primIdx(.i64),
        .u128_to_i128_wrap,
        .u128_to_i128_try,
        => self.store.monotype_store.primIdx(.i128),
        .u128_to_u8_wrap,
        .u128_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .u128_to_u16_wrap,
        .u128_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .u128_to_u32_wrap,
        .u128_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .u128_to_u64_wrap,
        .u128_to_u64_try,
        => self.store.monotype_store.primIdx(.u64),
        .u128_to_f32 => self.store.monotype_store.primIdx(.f32),
        .u128_to_f64 => self.store.monotype_store.primIdx(.f64),
        .u128_to_dec_try_unsafe => self.store.monotype_store.primIdx(.dec),

        .i128_to_i8_wrap,
        .i128_to_i8_try,
        => self.store.monotype_store.primIdx(.i8),
        .i128_to_i16_wrap,
        .i128_to_i16_try,
        => self.store.monotype_store.primIdx(.i16),
        .i128_to_i32_wrap,
        .i128_to_i32_try,
        => self.store.monotype_store.primIdx(.i32),
        .i128_to_i64_wrap,
        .i128_to_i64_try,
        => self.store.monotype_store.primIdx(.i64),
        .i128_to_u8_wrap,
        .i128_to_u8_try,
        => self.store.monotype_store.primIdx(.u8),
        .i128_to_u16_wrap,
        .i128_to_u16_try,
        => self.store.monotype_store.primIdx(.u16),
        .i128_to_u32_wrap,
        .i128_to_u32_try,
        => self.store.monotype_store.primIdx(.u32),
        .i128_to_u64_wrap,
        .i128_to_u64_try,
        => self.store.monotype_store.primIdx(.u64),
        .i128_to_u128_wrap,
        .i128_to_u128_try,
        => self.store.monotype_store.primIdx(.u128),
        .i128_to_f32 => self.store.monotype_store.primIdx(.f32),
        .i128_to_f64 => self.store.monotype_store.primIdx(.f64),
        .i128_to_dec_try_unsafe => self.store.monotype_store.primIdx(.dec),

        .f32_to_i8_trunc,
        .f32_to_i8_try_unsafe,
        => self.store.monotype_store.primIdx(.i8),
        .f32_to_i16_trunc,
        .f32_to_i16_try_unsafe,
        => self.store.monotype_store.primIdx(.i16),
        .f32_to_i32_trunc,
        .f32_to_i32_try_unsafe,
        => self.store.monotype_store.primIdx(.i32),
        .f32_to_i64_trunc,
        .f32_to_i64_try_unsafe,
        => self.store.monotype_store.primIdx(.i64),
        .f32_to_i128_trunc,
        .f32_to_i128_try_unsafe,
        => self.store.monotype_store.primIdx(.i128),
        .f32_to_u8_trunc,
        .f32_to_u8_try_unsafe,
        => self.store.monotype_store.primIdx(.u8),
        .f32_to_u16_trunc,
        .f32_to_u16_try_unsafe,
        => self.store.monotype_store.primIdx(.u16),
        .f32_to_u32_trunc,
        .f32_to_u32_try_unsafe,
        => self.store.monotype_store.primIdx(.u32),
        .f32_to_u64_trunc,
        .f32_to_u64_try_unsafe,
        => self.store.monotype_store.primIdx(.u64),
        .f32_to_u128_trunc,
        .f32_to_u128_try_unsafe,
        => self.store.monotype_store.primIdx(.u128),
        .f32_to_f64 => self.store.monotype_store.primIdx(.f64),

        .f64_to_i8_trunc,
        .f64_to_i8_try_unsafe,
        => self.store.monotype_store.primIdx(.i8),
        .f64_to_i16_trunc,
        .f64_to_i16_try_unsafe,
        => self.store.monotype_store.primIdx(.i16),
        .f64_to_i32_trunc,
        .f64_to_i32_try_unsafe,
        => self.store.monotype_store.primIdx(.i32),
        .f64_to_i64_trunc,
        .f64_to_i64_try_unsafe,
        => self.store.monotype_store.primIdx(.i64),
        .f64_to_i128_trunc,
        .f64_to_i128_try_unsafe,
        => self.store.monotype_store.primIdx(.i128),
        .f64_to_u8_trunc,
        .f64_to_u8_try_unsafe,
        => self.store.monotype_store.primIdx(.u8),
        .f64_to_u16_trunc,
        .f64_to_u16_try_unsafe,
        => self.store.monotype_store.primIdx(.u16),
        .f64_to_u32_trunc,
        .f64_to_u32_try_unsafe,
        => self.store.monotype_store.primIdx(.u32),
        .f64_to_u64_trunc,
        .f64_to_u64_try_unsafe,
        => self.store.monotype_store.primIdx(.u64),
        .f64_to_u128_trunc,
        .f64_to_u128_try_unsafe,
        => self.store.monotype_store.primIdx(.u128),
        .f64_to_f32_wrap,
        .f64_to_f32_try_unsafe,
        => self.store.monotype_store.primIdx(.f32),

        .dec_to_i8_trunc,
        .dec_to_i8_try_unsafe,
        => self.store.monotype_store.primIdx(.i8),
        .dec_to_i16_trunc,
        .dec_to_i16_try_unsafe,
        => self.store.monotype_store.primIdx(.i16),
        .dec_to_i32_trunc,
        .dec_to_i32_try_unsafe,
        => self.store.monotype_store.primIdx(.i32),
        .dec_to_i64_trunc,
        .dec_to_i64_try_unsafe,
        => self.store.monotype_store.primIdx(.i64),
        .dec_to_i128_trunc,
        .dec_to_i128_try_unsafe,
        => self.store.monotype_store.primIdx(.i128),
        .dec_to_u8_trunc,
        .dec_to_u8_try_unsafe,
        => self.store.monotype_store.primIdx(.u8),
        .dec_to_u16_trunc,
        .dec_to_u16_try_unsafe,
        => self.store.monotype_store.primIdx(.u16),
        .dec_to_u32_trunc,
        .dec_to_u32_try_unsafe,
        => self.store.monotype_store.primIdx(.u32),
        .dec_to_u64_trunc,
        .dec_to_u64_try_unsafe,
        => self.store.monotype_store.primIdx(.u64),
        .dec_to_u128_trunc,
        .dec_to_u128_try_unsafe,
        => self.store.monotype_store.primIdx(.u128),
        .dec_to_f32_wrap,
        .dec_to_f32_try_unsafe,
        => self.store.monotype_store.primIdx(.f32),
        .dec_to_f64 => self.store.monotype_store.primIdx(.f64),

        .box_box => if (arg_exprs.len == 0) null else blk: {
            const inner = (try self.explicitExprMonotype(arg_exprs[0])) orelse break :blk null;
            break :blk try self.store.monotype_store.addMonotype(self.allocator, .{ .box = .{ .inner = inner } });
        },
        .box_unbox => if (arg_exprs.len == 0) null else blk: {
            const boxed = (try self.explicitExprMonotype(arg_exprs[0])) orelse break :blk null;
            break :blk switch (self.store.monotype_store.getMonotype(boxed)) {
                .box => |box| box.inner,
                else => null,
            };
        },

        .str_drop_prefix,
        .str_drop_suffix,
        .str_to_utf8,
        .str_from_utf8,
        .str_split_on,
        .list_first,
        .list_last,
        .list_split_first,
        .list_split_last,
        .num_from_numeral,
        .num_from_str,
        .compare,
        .crash,
        => null,
    };
}

fn semanticExprResultMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!?Monotype.Idx {
    const module_env = self.all_module_envs[self.current_module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lookup_local => |lookup| blk: {
            if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, lookup.pattern_idx)) |source| {
                if (source.module_idx == self.current_module_idx and source.expr_idx == expr_idx) break :blk null;
                break :blk try self.explicitExprMonotypeInModule(source.module_idx, source.expr_idx);
            }
            if (findDefByPattern(module_env, lookup.pattern_idx)) |def_idx| {
                const def_expr_idx = module_env.store.getDef(def_idx).expr;
                if (def_expr_idx == expr_idx) break :blk null;
                break :blk try self.explicitExprMonotype(def_expr_idx);
            }
            break :blk null;
        },
        .e_lookup_external => |lookup| blk: {
            const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse break :blk null;
            if (!self.all_module_envs[target_module_idx].store.isDefNode(lookup.target_node_idx)) break :blk null;
            const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
            break :blk try self.explicitExprMonotypeInModule(
                target_module_idx,
                self.all_module_envs[target_module_idx].store.getDef(def_idx).expr,
            );
        },
        .e_lookup_required => |lookup| blk: {
            const target = self.resolveRequiredLookupTarget(module_env, lookup) orelse break :blk null;
            break :blk try self.explicitExprMonotypeInModule(
                target.module_idx,
                self.all_module_envs[target.module_idx].store.getDef(target.def_idx).expr,
            );
        },
        .e_block => |block| try self.explicitExprMonotype(block.final_expr),
        .e_dbg => |dbg_expr| try self.explicitExprMonotype(dbg_expr.expr),
        .e_expect => |expect_expr| try self.explicitExprMonotype(expect_expr.body),
        .e_return => |return_expr| try self.explicitExprMonotype(return_expr.expr),
        .e_nominal => |nominal| try self.explicitExprMonotype(nominal.backing_expr),
        .e_nominal_external => |nominal| try self.explicitExprMonotype(nominal.backing_expr),
        .e_unary_minus => |um| try self.explicitExprMonotype(um.expr),
        .e_unary_not => try self.boolMonotype(),
        .e_binop => |binop| switch (binop.op) {
            .eq, .ne, .lt, .le, .gt, .ge, .@"and", .@"or" => try self.boolMonotype(),
            else => try self.explicitExprMonotype(binop.lhs),
        },
        .e_call => |call| blk: {
            const op = self.getCallLowLevelOp(module_env, call.func) orelse break :blk null;
            break :blk try self.lowLevelResultMonotypeFromArgs(op, module_env.store.sliceExpr(call.args));
        },
        .e_run_low_level => |run_ll| try self.lowLevelResultMonotypeFromArgs(run_ll.op, module_env.store.sliceExpr(run_ll.args)),
        else => null,
    };
}

fn explicitExprMonotype(self: *Self, expr_idx: CIR.Expr.Idx) Allocator.Error!?Monotype.Idx {
    if (self.lookupPipelinedExprMonotype(expr_idx)) |mono| {
        return @as(?Monotype.Idx, try self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            mono.idx,
            mono.module_idx,
            self.current_module_idx,
        ));
    }

    if (try self.semanticExprResultMonotype(expr_idx)) |monotype| {
        return monotype;
    }

    return null;
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
    if (builtin.mode == .Debug) {
        if (self.current_lambda_entry_bound_locals) |entry_bound_locals| {
            if (entry_bound_locals.contains(target)) {
                std.debug.panic(
                    "statement-only MIR invariant violated: body lowering tried to alias into entry-bound local {d} from source {d} (source_def={s}, callable_inst={d}, scope={d})",
                    .{
                        @intFromEnum(target),
                        @intFromEnum(source),
                        if (self.store.getLocalDefOpt(source)) |source_def| @tagName(source_def) else "none",
                        @intFromEnum(self.current_callable_inst_context),
                        self.current_pattern_scope,
                    },
                );
            }
        }
    }
    self.store.getLocalPtr(target).monotype = self.store.getLocal(source).monotype;
    if (self.store.monotype_store.getMonotype(self.store.getLocal(target).monotype) == .func) {
        const resolved = try self.resolveExactCallableResolutionForLocal(source) orelse std.debug.panic(
            "statement-only MIR invariant violated: function-valued alias source local {d} lacked explicit exact callable while lowering alias target {d}",
            .{ @intFromEnum(source), @intFromEnum(target) },
        );
        try self.bindExactLambdaLocal(target, resolved.lambda);
    } else {
        self.clearExactCallableLocal(target);
    }
    return self.lowerRefInto(target, .{ .local = source }, next);
}

fn clearExactCallableLocal(self: *Self, local_id: MIR.LocalId) void {
    self.store.getLocalPtr(local_id).exact_callable = null;
}

fn exactCallableResolutionsEqual(
    left: ExactCallableResolution,
    right: ExactCallableResolution,
) bool {
    return left.lambda == right.lambda and
        left.requires_hidden_capture == right.requires_hidden_capture;
}

fn resolveExactCallableResolutionForLocal(
    self: *Self,
    local_id: MIR.LocalId,
) Allocator.Error!?ExactCallableResolution {
    if (self.store.getLocal(local_id).exact_callable) |exact_callable| {
        return .{
            .lambda = exact_callable.lambda,
            .requires_hidden_capture = exact_callable.requires_hidden_capture,
        };
    }

    return null;
}

fn mergeExactCallableJoinIncoming(
    self: *Self,
    dest_local: MIR.LocalId,
    source_local: MIR.LocalId,
) Allocator.Error!void {
    if (self.store.monotype_store.getMonotype(self.store.getLocal(dest_local).monotype) != .func) {
        return;
    }

    const source_resolution = try self.resolveExactCallableResolutionForLocal(source_local) orelse std.debug.panic(
        "statement-only MIR invariant violated: function-valued join source local {d} lacked explicit exact callable while merging into join local {d}",
        .{ @intFromEnum(source_local), @intFromEnum(dest_local) },
    );

    if (self.store.getLocal(dest_local).exact_callable) |existing| {
        const existing_resolution = ExactCallableResolution{
            .lambda = existing.lambda,
            .requires_hidden_capture = existing.requires_hidden_capture,
        };
        if (!exactCallableResolutionsEqual(existing_resolution, source_resolution)) {
            std.debug.panic(
                "statement-only MIR invariant violated: join local {d} merged incompatible exact callables {d}/{any} and {d}/{any}",
                .{
                    @intFromEnum(dest_local),
                    @intFromEnum(existing.lambda),
                    existing.requires_hidden_capture,
                    @intFromEnum(source_resolution.lambda),
                    source_resolution.requires_hidden_capture,
                },
            );
        }
        return;
    }

    try self.bindExactLambdaLocal(dest_local, source_resolution.lambda);
}

fn resolveExactCallableResolutionForCapturePattern(
    self: *Self,
    capture_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!?ExactCallableResolution {
    const capture_local = self.lookupExistingPatternLocalInAncestorScopes(
        self.current_module_idx,
        self.current_pattern_scope,
        capture_pattern_idx,
    ) orelse return null;
    return self.resolveExactCallableResolutionForLocal(capture_local);
}

fn seedExactCallablePatternLocal(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
) Allocator.Error!void {
    if (self.store.monotype_store.getMonotype(self.store.getLocal(local).monotype) != .func) {
        self.clearExactCallableLocal(local);
        return;
    }

    const callable_inst_ids =
        if (lookupPipelinedPatternCallableInsts(self, pattern_idx)) |ids|
            ids
        else if (lookupPipelinedPatternCallableInstInContext(
            self,
            self.current_callable_inst_context,
            self.current_module_idx,
            pattern_idx,
        )) |id|
            &.{id}
        else
            &.{};

    if (callable_inst_ids.len == 0) {
        std.debug.panic(
            "statement-only MIR invariant violated: function-valued pattern local {d} for pattern {d} lacked an exact callable specialization in context callable_inst={d}",
            .{
                @intFromEnum(local),
                @intFromEnum(pattern_idx),
                @intFromEnum(self.current_callable_inst_context),
            },
        );
    }

    const resolution = try self.resolveUniqueExactCallableResolution(
        callable_inst_ids,
        self.store.getLocal(local).monotype,
    );
    try self.bindExactLambdaLocal(local, resolution.lambda);
    self.store.getLocalPtr(local).exact_callable = .{
        .lambda = resolution.lambda,
        .requires_hidden_capture = resolution.requires_hidden_capture,
    };
}

fn seedExactCallableParamLocals(
    self: *Self,
    param_patterns: []const CIR.Pattern.Idx,
    param_locals: []const MIR.LocalId,
) Allocator.Error!void {
    if (param_patterns.len != param_locals.len) {
        std.debug.panic(
            "statement-only MIR invariant violated: lambda param pattern arity {d} != param local arity {d}",
            .{ param_patterns.len, param_locals.len },
        );
    }

    for (param_patterns, param_locals) |pattern_idx, param_local| {
        try self.seedExactCallablePatternLocal(pattern_idx, param_local);
    }
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
    const prefix_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = prefix_join_id,
        .params = try self.store.addLocalSpan(self.allocator, &.{prefix_local}),
        .body = next_accum_stmt,
        .remainder = prefix_switch,
    } });
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
    const head_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_head_id,
        .params = loop_head_params,
        .body = cond_stmt,
        .remainder = initial_jump,
    } });

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

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_exit_id,
        .params = try self.store.addLocalSpan(self.allocator, &.{target}),
        .body = next,
        .remainder = init_len,
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
    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = join_id,
        .params = try self.store.addLocalSpan(self.allocator, &.{target}),
        .body = next,
        .remainder = switch_entry,
    } });
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
    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = join_id,
        .params = try self.store.addLocalSpan(self.allocator, &.{target}),
        .body = next,
        .remainder = switch_entry,
    } });
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

fn resolveBestExactCallableInstForExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!?Pipeline.CallableInstId {
    if (try self.lookupPipelinedValueExprCallableInstForMonotype(
        expr_idx,
        fn_monotype,
        fn_monotype_module_idx,
    )) |callable_inst_id| {
        return callable_inst_id;
    }

    if (self.lookupPipelinedValueExprCallableInst(expr_idx)) |callable_inst_id| {
        return callable_inst_id;
    }

    if (self.lookupPipelinedValueExprCallableInsts(expr_idx)) |resolved_set| {
        if (resolved_set.len == 1) return resolved_set[0];
    }

    return null;
}
fn lowerLookupLocalInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const pattern_source_expr = self.callable_pipeline.lambda_solved.getPatternSourceExpr(
        self.current_module_idx,
        lookup.pattern_idx,
    );
    const skipped_callable_backed = self.isSkippedCallableBackedBindingPattern(
        self.current_module_idx,
        lookup.pattern_idx,
    );

    const usable_source_local = if (!skipped_callable_backed) blk: {
        if (self.lookupUsablePatternLocalAtScope(self.current_pattern_scope, lookup.pattern_idx)) |local| {
            break :blk local;
        }

        if (self.current_lambda_scope_boundary != null) {
            break :blk self.lookupExistingPatternLocalWithinCurrentLambda(lookup.pattern_idx);
        }

        break :blk self.lookupUsablePatternLocalInAncestorScopes(
            self.current_module_idx,
            self.current_pattern_scope,
            lookup.pattern_idx,
        );
    } else null;

    if (usable_source_local) |source| {
        return self.lowerLocalAliasInto(target, source, next);
    }

    if (findDefByPattern(self.all_module_envs[self.current_module_idx], lookup.pattern_idx)) |def_idx| {
        const symbol = try self.lookupSymbolForPattern(self.current_module_idx, lookup.pattern_idx);
        const target_monotype = self.store.getLocal(target).monotype;
        const exact_callable_inst = if (self.store.monotype_store.getMonotype(target_monotype) == .func)
            (try self.resolveBestExactCallableInstForExpr(expr_idx, target_monotype, self.current_module_idx)) orelse
                std.debug.panic(
                    "statement-only MIR invariant violated: local top-level def lookup expr {d} lacked an exact callable specialization",
                    .{@intFromEnum(expr_idx)},
                )
        else
            null;
        return self.materializeTopLevelDefInto(
            self.current_module_idx,
            def_idx,
            symbol,
            target,
            next,
            exact_callable_inst,
        );
    }

    if (try self.ensureImplicitLambdaCaptureLocal(lookup.pattern_idx)) |capture_local| {
        return self.lowerLocalAliasInto(target, capture_local, next);
    }

    if (pattern_source_expr) |source| {
        if (source.module_idx == self.current_module_idx and source.expr_idx == expr_idx) {
            std.debug.panic(
                "statement-only MIR invariant violated: local lookup expr {d} resolved to itself as a pattern source",
                .{@intFromEnum(expr_idx)},
            );
        }
        return self.lowerCapturedSourceExprInto(source, target, next);
    }

    _ = findDefByPattern(self.all_module_envs[self.current_module_idx], lookup.pattern_idx) orelse blk: {
        const pattern = self.all_module_envs[self.current_module_idx].store.getPattern(lookup.pattern_idx);
        const ident = switch (pattern) {
            .assign => |assign| self.all_module_envs[self.current_module_idx].getIdent(assign.ident),
            .as => |as_pattern| self.all_module_envs[self.current_module_idx].getIdent(as_pattern.ident),
            else => "<non-binding>",
        };
        const has_root_local = self.lookupExistingPatternLocalInScope(
            self.current_module_idx,
            0,
            lookup.pattern_idx,
        ) != null;
        const active_template = if (!self.current_callable_inst_context.isNone())
            self.callable_pipeline.lambda_solved.getCallableTemplate(
                self.callable_pipeline.lambda_specialize.getCallableInst(self.current_callable_inst_context).template,
            )
        else
            null;
        break :blk std.debug.panic(
            "statement-only MIR invariant violated: local lookup pattern {d} ({s}:{s}) in scope={d} had no in-scope local, no pattern source expr, no top-level def, root_local={any}, callable_inst={d}, callable_kind={s}, callable_expr={d}",
            .{
                @intFromEnum(lookup.pattern_idx),
                @tagName(pattern),
                ident,
                self.current_pattern_scope,
                has_root_local,
                @intFromEnum(self.current_callable_inst_context),
                if (active_template) |template| @tagName(template.kind) else "none",
                if (active_template) |template| @intFromEnum(template.cir_expr) else std.math.maxInt(u32),
            },
        );
    };
    unreachable;
}

fn ensureImplicitLambdaCaptureLocal(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!?MIR.LocalId {
    const capture_state = self.current_implicit_lambda_captures orelse return null;

    if (capture_state.by_pattern.get(pattern_idx)) |existing_index| {
        return capture_state.items.items[existing_index].local;
    }

    const ancestor = self.lookupExistingPatternLocalInAncestorScopesWithScope(
        self.current_module_idx,
        self.current_pattern_scope,
        pattern_idx,
    );
    if (ancestor) |scoped| {
        if (self.scopeIsWithinCurrentLambda(scoped.scope)) return null;
    }
    const ancestor_local = if (ancestor) |scoped| scoped.local else null;
    if (ancestor_local == null and findDefByPattern(self.all_module_envs[self.current_module_idx], pattern_idx) != null) {
        return null;
    }
    if (ancestor_local == null and self.current_callable_inst_context.isNone()) return null;

    const monotype = if (ancestor_local) |outer_local|
        self.store.getLocal(outer_local).monotype
    else blk: {
        if (!self.current_callable_inst_context.isNone()) {
            if (self.runtimeClosureExprForCallableInst(self.current_callable_inst_context)) |runtime_closure| {
                break :blk try self.resolveRuntimePatternMonotype(
                    runtime_closure.module_env,
                    runtime_closure.expr_idx,
                    self.current_callable_inst_context,
                    pattern_idx,
                );
            }
        }

        if (self.lookupPipelinedPatternMonotype(pattern_idx)) |resolved| {
            break :blk try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                resolved.idx,
                resolved.module_idx,
                self.current_module_idx,
            );
        }

        if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, pattern_idx)) |source| {
            break :blk try self.resolveExprMonotypeInModule(source.module_idx, source.expr_idx);
        }

        if (findDefByPattern(self.all_module_envs[self.current_module_idx], pattern_idx)) |def_idx| {
            break :blk try self.resolveExprMonotypeInModule(
                self.current_module_idx,
                self.all_module_envs[self.current_module_idx].store.getDef(def_idx).expr,
            );
        }

        break :blk try self.monotypeFromTypeVarWithBindings(
            self.current_module_idx,
            self.types_store,
            ModuleEnv.varFrom(pattern_idx),
            &self.type_var_seen,
        );
    };
    const entry_scope = self.current_lambda_scope_boundary orelse self.current_pattern_scope;
    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = entry_scope;
    defer self.current_pattern_scope = saved_scope;

    const local = try self.patternToLocalWithMonotype(pattern_idx, monotype);
    if (self.current_lambda_entry_bound_locals) |entry_bound_locals| {
        try entry_bound_locals.add(local);
    }

    var exact_lambda_id: ?MIR.LambdaId = null;
    var requires_hidden_capture = false;
    if (ancestor_local) |outer_local| {
        if (try self.resolveExactCallableResolutionForLocal(outer_local)) |resolved| {
            exact_lambda_id = resolved.lambda;
            requires_hidden_capture = resolved.requires_hidden_capture;
        }
    }

    const exact_callable_inst_id = if (exact_lambda_id == null and self.store.monotype_store.getMonotype(monotype) == .func) blk: {
        if (!self.current_callable_inst_context.isNone()) {
            const capture_source_expr = if (self.runtimeClosureExprForCallableInst(self.current_callable_inst_context)) |runtime_closure|
                runtime_closure.expr_idx
            else blk2: {
                const current_callable = self.callable_pipeline.lambda_specialize.getCallableInst(self.current_callable_inst_context);
                const current_template = self.callable_pipeline.lambda_solved.getCallableTemplate(current_callable.template);
                break :blk2 current_template.cir_expr;
            };
            if (try self.resolveExactCallableInstForCapturePattern(
                capture_source_expr,
                pattern_idx,
                monotype,
                self.current_callable_inst_context,
            )) |callable_inst_id| {
                break :blk callable_inst_id;
            }
        }

        break :blk try self.lookupPipelinedPatternCallableInstForMonotype(
            pattern_idx,
            monotype,
            self.current_module_idx,
        );
    } else null;

    if (exact_callable_inst_id) |callable_inst_id| {
        try self.bindExactCallableLocal(local, callable_inst_id);
        const exact_callable = self.store.getLocal(local).exact_callable orelse unreachable;
        exact_lambda_id = exact_callable.lambda;
        requires_hidden_capture = exact_callable.requires_hidden_capture;
    } else if (exact_lambda_id) |lambda_id| {
        try self.bindExactLambdaLocal(local, lambda_id);
    }

    const new_index = capture_state.items.items.len;
    try capture_state.items.append(self.allocator, .{
        .pattern_idx = pattern_idx,
        .local = local,
        .monotype = monotype,
        .exact_callable_inst_id = exact_callable_inst_id,
        .exact_lambda_id = exact_lambda_id,
        .requires_hidden_capture = requires_hidden_capture,
    });
    try capture_state.by_pattern.put(pattern_idx, new_index);
    return local;
}

fn lookupReusableCaptureLocal(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!?MIR.LocalId {
    if (self.lookupExistingPatternLocalWithinCurrentLambda(pattern_idx)) |existing| {
        return existing;
    }

    if (try self.ensureImplicitLambdaCaptureLocal(pattern_idx)) |implicit_capture| {
        return implicit_capture;
    }

    // Outside lambda-body lowering there is no implicit-capture environment to
    // synthesize entry locals. In that case, an already-live ancestor binding is
    // the exact runtime value a closure should capture.
    if (self.current_implicit_lambda_captures == null) {
        if (self.lookupUsablePatternLocalInAncestorScopes(
            self.current_module_idx,
            self.current_pattern_scope,
            pattern_idx,
        )) |ancestor| {
            return ancestor;
        }
    }

    return null;
}

fn lowerLookupExternalInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    module_env: *const ModuleEnv,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const target_module_idx = self.resolveImportedModuleIdx(module_env, lookup.module_idx) orelse unreachable;
    if (!self.all_module_envs[target_module_idx].store.isDefNode(lookup.target_node_idx)) {
        std.debug.panic(
            "statement-only MIR TODO: external lookup node {d} in module {d} does not lower to a def-backed value yet",
            .{ lookup.target_node_idx, target_module_idx },
        );
    }
    const def_idx: CIR.Def.Idx = @enumFromInt(lookup.target_node_idx);
    const symbol = try self.internExternalDefSymbol(target_module_idx, lookup.target_node_idx);
    const target_monotype = self.store.getLocal(target).monotype;
    const exact_callable_inst = if (self.store.monotype_store.getMonotype(target_monotype) == .func)
        (try self.resolveBestExactCallableInstForExpr(expr_idx, target_monotype, self.current_module_idx)) orelse
            std.debug.panic(
                "statement-only MIR invariant violated: external lookup expr {d} lacked an exact callable specialization",
                .{@intFromEnum(expr_idx)},
            )
    else
        null;
    return self.materializeTopLevelDefInto(target_module_idx, def_idx, symbol, target, next, exact_callable_inst);
}

fn lowerLookupRequiredInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    module_env: *const ModuleEnv,
    lookup: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const target_lookup = self.resolveRequiredLookupTarget(module_env, lookup) orelse std.debug.panic(
        "statement-only MIR TODO: required lookup could not be resolved to an app export",
        .{},
    );
    const def = self.all_module_envs[target_lookup.module_idx].store.getDef(target_lookup.def_idx);
    const symbol = try self.lookupSymbolForPattern(target_lookup.module_idx, def.pattern);
    const target_monotype = self.store.getLocal(target).monotype;
    const exact_callable_inst = if (self.store.monotype_store.getMonotype(target_monotype) == .func)
        (try self.resolveBestExactCallableInstForExpr(expr_idx, target_monotype, self.current_module_idx)) orelse
            std.debug.panic(
                "statement-only MIR invariant violated: required lookup expr {d} lacked an exact callable specialization",
                .{@intFromEnum(expr_idx)},
            )
    else
        null;
    return self.materializeTopLevelDefInto(
        target_lookup.module_idx,
        target_lookup.def_idx,
        symbol,
        target,
        next,
        exact_callable_inst,
    );
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
    const saved_source_context = self.current_source_context;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_scratches_module_idx = self.mono_scratches.module_idx;

    const target_env = self.all_module_envs[module_idx];
    const def = target_env.store.getDef(def_idx);

    self.current_module_idx = module_idx;
    self.current_pattern_scope = 0;
    self.current_callable_inst_context = .none;
    self.current_source_context = .{ .active = .{ .root_expr = .{ .module_idx = module_idx, .expr_idx = def.expr } } };
    self.mono_scratches.ident_store = target_env.getIdentStoreConst();
    self.mono_scratches.module_env = target_env;
    self.mono_scratches.module_idx = module_idx;

    defer self.current_module_idx = saved_module_idx;
    defer self.current_pattern_scope = saved_pattern_scope;
    defer self.current_callable_inst_context = saved_callable_context;
    defer self.current_source_context = saved_source_context;
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
    exact_callable_inst: ?Pipeline.CallableInstId,
) Allocator.Error!MIR.CFStmtId {
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_scratches_module_idx = self.mono_scratches.module_idx;

    const target_env = self.all_module_envs[module_idx];
    const def = target_env.store.getDef(def_idx);

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

    try self.refreshTargetLocalMonotypeFromExprIntoModule(
        def.expr,
        module_idx,
        saved_module_idx,
        target,
    );

    const target_monotype = self.store.getLocal(target).monotype;
    if (self.store.monotype_store.getMonotype(target_monotype) == .func) {
        const resolved_callable_inst = exact_callable_inst orelse std.debug.panic(
            "statement-only MIR invariant violated: top-level callable def {d} in module {d} was materialized without an exact callable inst",
            .{ @intFromEnum(def_idx), module_idx },
        );
        return self.lowerResolvedCallableInstValueInto(resolved_callable_inst, target, next);
    }

    try self.ensureNamedConstDefRegistered(module_idx, def_idx, symbol);
    return self.store.addCFStmt(self.allocator, .{ .assign_symbol = .{
        .target = target,
        .symbol = symbol,
        .next = next,
    } });
}

fn lowerLambdaInto(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    lambda: CIR.Expr.Lambda,
    fn_monotype: Monotype.Idx,
    reserved_lambda: ?MIR.LambdaId,
) Allocator.Error!MIR.LambdaId {
    const cache_key = makeCallableCacheKey(self, expr_idx, fn_monotype);
    if (try self.lookupLoweredCallableLambdaCache(expr_idx, fn_monotype)) |cached| {
        if (reserved_lambda) |reserved| {
            if (cached == reserved) return cached;
        } else {
            return cached;
        }
    }
    if (reserved_lambda) |reserved| {
        try self.lowered_callable_lambdas.put(cache_key, reserved);
    }

    const ret_monotype = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR lambda expected function monotype for expr {d}",
            .{@intFromEnum(expr_idx)},
        ),
    };

    const saved_scope = self.current_pattern_scope;
    const lambda_entry_scope = self.freshStatementPatternScope();
    self.current_pattern_scope = lambda_entry_scope;
    defer self.current_pattern_scope = saved_scope;
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

    const param_patterns = module_env.store.slicePatterns(lambda.args);
    const params = try self.lowerLambdaParamLocals(module_env, lambda.args);
    var stable_param_locals = try std.ArrayList(MIR.LocalId).initCapacity(self.allocator, self.store.getLocalSpan(params).len);
    defer stable_param_locals.deinit(self.allocator);
    stable_param_locals.appendSliceAssumeCapacity(self.store.getLocalSpan(params));
    const param_locals = stable_param_locals.items;
    for (param_patterns) |pattern_idx| {
        try self.predeclarePatternLocals(module_env, pattern_idx);
    }

    const active_lambda = if (reserved_lambda) |reserved|
        reserved
    else blk: {
        break :blk try self.store.reserveLambda(self.allocator);
    };
    try self.store.installReservedLambdaPrototype(active_lambda, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .body = @enumFromInt(0),
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = null,
        .recursion = .not_recursive,
        .hosted = null,
    });
    try self.lowered_callable_lambdas.put(cache_key, active_lambda);

    const saved_implicit_lambda_captures = self.current_implicit_lambda_captures;
    var implicit_lambda_captures = ImplicitLambdaCaptureState.init(self.allocator);
    defer implicit_lambda_captures.deinit(self.allocator);
    if (!self.current_callable_inst_context.isNone()) {
        const active_template = self.callable_pipeline.lambda_solved.getCallableTemplate(
            self.callable_pipeline.lambda_specialize.getCallableInst(self.current_callable_inst_context).template,
        );
        if (active_template.kind == .lambda or active_template.kind == .closure) {
            self.current_implicit_lambda_captures = &implicit_lambda_captures;
        }
    }
    defer self.current_implicit_lambda_captures = saved_implicit_lambda_captures;

    try self.seedExactCallableParamLocals(param_patterns, param_locals);

    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    const lambda_body_scope = self.freshChildPatternScope(lambda_entry_scope);
    self.current_pattern_scope = lambda_body_scope;
    var body = try self.lowerCirExprInto(lambda.body, result_local, ret_stmt);
    self.current_pattern_scope = lambda_entry_scope;

    const capture_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(capture_top);
    var capture_monotypes = std.ArrayList(Monotype.Idx).empty;
    defer capture_monotypes.deinit(self.allocator);
    var runtime_implicit_captures = std.ArrayList(ImplicitLambdaCapture).empty;
    defer runtime_implicit_captures.deinit(self.allocator);
    var callable_only_captures = std.ArrayList(struct {
        local: MIR.LocalId,
        lambda_id: MIR.LambdaId,
    }).empty;
    defer callable_only_captures.deinit(self.allocator);

    for (implicit_lambda_captures.items.items) |capture| {
        const capture_resolution = try self.implicitCaptureExactResolution(capture);
        if (capture_resolution) |resolved| {
            if (!resolved.requires_hidden_capture) {
                try callable_only_captures.append(self.allocator, .{
                    .local = capture.local,
                    .lambda_id = resolved.lambda,
                });
                continue;
            }
        }

        try self.scratch_local_ids.append(capture.local);
        try capture_monotypes.append(
            self.allocator,
            try self.runtimeCaptureFieldMonotype(
                capture.monotype,
                capture_resolution,
            ),
        );
        try runtime_implicit_captures.append(self.allocator, capture);
    }

    const runtime_capture_locals = self.scratch_local_ids.sliceFromStart(capture_top);
    if (!self.current_callable_inst_context.isNone() and runtime_implicit_captures.items.len != 0) {
        var runtime_capture_patterns = std.ArrayList(CIR.Pattern.Idx).empty;
        defer runtime_capture_patterns.deinit(self.allocator);
        try runtime_capture_patterns.ensureTotalCapacity(self.allocator, runtime_implicit_captures.items.len);
        for (runtime_implicit_captures.items) |capture| {
            runtime_capture_patterns.appendAssumeCapacity(capture.pattern_idx);
        }
        try self.lowered_callable_runtime_captures.put(
            @intFromEnum(self.current_callable_inst_context),
            try self.addRuntimeCapturePatternSpan(runtime_capture_patterns.items),
        );
    }
    const captures_tuple_monotype = if (runtime_capture_locals.len == 0)
        Monotype.Idx.none
    else
        try self.tupleMonotypeForFields(capture_monotypes.items);
    const captures_param_local = if (runtime_capture_locals.len == 0)
        null
    else
        try self.freshSyntheticLocal(captures_tuple_monotype, false);

    var i = param_patterns.len;
    while (i > 0) {
        i -= 1;
        if (!self.paramPatternNeedsBindingStmt(module_env, param_patterns[i], param_locals[i])) continue;
        body = try self.lowerPatternBindingLocalInto(module_env, param_patterns[i], param_locals[i], body);
    }

    if (captures_param_local) |captures_param| {
        var capture_i = runtime_capture_locals.len;
        while (capture_i > 0) {
            capture_i -= 1;
            body = try self.lowerRefInto(runtime_capture_locals[capture_i], .{ .field = .{
                .source = captures_param,
                .field_idx = @intCast(capture_i),
                .ownership = .move,
            } }, body);
        }
    }

    var callable_only_i = callable_only_captures.items.len;
    while (callable_only_i > 0) {
        callable_only_i -= 1;
        const callable_only_capture = callable_only_captures.items[callable_only_i];
        try self.bindExactLambdaLocal(callable_only_capture.local, callable_only_capture.lambda_id);
        body = try self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
            .target = callable_only_capture.local,
            .lambda = callable_only_capture.lambda_id,
            .next = body,
        } });
    }

    const lowered_lambda = MIR.Lambda{
        .fn_monotype = fn_monotype,
        .params = params,
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
    module_env: *const ModuleEnv,
    span: CIR.Pattern.Span,
) Allocator.Error!MIR.LocalSpan {
    const cir_ids = module_env.store.slicePatterns(span);
    if (cir_ids.len == 0) return MIR.LocalSpan.empty();

    const scratch_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(scratch_top);

    for (cir_ids) |pattern_idx| {
        const monotype = try self.requirePatternMonotype(pattern_idx);
        try self.bindPatternMonotypes(module_env, pattern_idx, monotype);
        const param_reassignable = switch (module_env.store.getPattern(pattern_idx)) {
            .assign => |assign_pattern| assign_pattern.ident.attributes.reassignable,
            .as => |as_pattern| as_pattern.ident.attributes.reassignable,
            else => false,
        };
        const param_local = try self.freshSyntheticLocal(monotype, param_reassignable);
        self.store.getLocalPtr(param_local).monotype = monotype;
        switch (module_env.store.getPattern(pattern_idx)) {
            .assign, .as => {
                try self.bindPatternLocalInCurrentScope(pattern_idx, param_local);
                try self.explicit_pattern_local_monotypes.put(
                    patternScopeKey(self.current_module_idx, self.current_pattern_scope, pattern_idx),
                    monotype,
                );
            },
            else => {},
        }
        try self.scratch_local_ids.append(param_local);
    }

    return self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(scratch_top));
}

fn addRuntimeCapturePatternSpan(
    self: *Self,
    patterns: []const CIR.Pattern.Idx,
) Allocator.Error!RuntimeCapturePatternSpan {
    if (patterns.len == 0) return RuntimeCapturePatternSpan.empty();
    const start: u32 = @intCast(self.runtime_capture_patterns.items.len);
    try self.runtime_capture_patterns.appendSlice(self.allocator, patterns);
    return .{
        .start = start,
        .len = @intCast(patterns.len),
    };
}

fn getRuntimeCapturePatternSpan(
    self: *const Self,
    span: RuntimeCapturePatternSpan,
) []const CIR.Pattern.Idx {
    return self.runtime_capture_patterns.items[span.start..][0..span.len];
}
const ExactCallableSetResolution = struct {
    lambda: MIR.LambdaId,
    requires_hidden_capture: bool,
};

fn resolveUniqueExactCallableResolution(
    self: *Self,
    callable_members: []const Pipeline.CallableInstId,
    expected_fn_monotype: Monotype.Idx,
) Allocator.Error!ExactCallableSetResolution {
    if (callable_members.len == 0) {
        std.debug.panic(
            "statement-only MIR invariant violated: exact callable set had no members",
            .{},
        );
    }

    var resolved: ?ExactCallableSetResolution = null;
    var matching_members: usize = 0;
    for (callable_members) |callable_inst_id| {
        if (!(try self.callableInstMatchesFnMonotype(
            callable_inst_id,
            expected_fn_monotype,
            self.current_module_idx,
        ))) continue;

        matching_members += 1;
        const candidate = ExactCallableSetResolution{
            .lambda = try self.lowerResolvedCallableInstLambda(callable_inst_id),
            .requires_hidden_capture = try self.callableInstProducesClosureValue(callable_inst_id),
        };
        if (resolved) |existing| {
            if (existing.lambda != candidate.lambda or
                existing.requires_hidden_capture != candidate.requires_hidden_capture)
            {
                std.debug.panic(
                    "statement-only MIR TODO: exact callable set resolved to multiple distinct callable values ({d} members)",
                    .{callable_members.len},
                );
            }
        } else {
            resolved = candidate;
        }
    }

    if (matching_members == 0) {
        std.debug.panic(
            "statement-only MIR invariant violated: exact callable set had no member matching expected fn monotype {d}",
            .{@intFromEnum(expected_fn_monotype)},
        );
    }

    return resolved.?;
}

fn resolveExactCallableInstForCapturePattern(
    self: *Self,
    closure_expr_idx: CIR.Expr.Idx,
    capture_pattern_idx: CIR.Pattern.Idx,
    capture_monotype: Monotype.Idx,
    closure_callable_inst_id: ?Pipeline.CallableInstId,
) Allocator.Error!?Pipeline.CallableInstId {
    if (closure_callable_inst_id) |callable_inst_id| {
        if (self.callable_pipeline.lambda_specialize.getClosureCaptureCallableInst(
            callable_inst_id,
            self.current_module_idx,
            closure_expr_idx,
            capture_pattern_idx,
        )) |resolved| {
            return resolved;
        }
    }

    if (try self.lookupPipelinedPatternCallableInstForMonotype(
        capture_pattern_idx,
        capture_monotype,
        self.current_module_idx,
    )) |resolved| {
        return resolved;
    }

    if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, capture_pattern_idx)) |source| {
        if (try self.lookupPipelinedValueExprCallableInstForMonotypeInModule(
            source.module_idx,
            source.expr_idx,
            capture_monotype,
            self.current_module_idx,
        )) |callable_inst_id| {
            return callable_inst_id;
        }
    }

    if (findDefByPattern(self.all_module_envs[self.current_module_idx], capture_pattern_idx)) |def_idx| {
        const def_expr = self.all_module_envs[self.current_module_idx].store.getDef(def_idx).expr;
        if (try self.lookupPipelinedValueExprCallableInstForMonotypeInModule(
            self.current_module_idx,
            def_expr,
            capture_monotype,
            self.current_module_idx,
        )) |callable_inst_id| {
            return callable_inst_id;
        }

        if (self.callable_pipeline.lambda_solved.getExprCallableTemplate(
            self.current_module_idx,
            def_expr,
        )) |template_id| {
            if (try self.lookupSpecializedCallableInstForTemplateAndMonotype(
                template_id,
                capture_monotype,
                self.current_module_idx,
            )) |callable_inst_id| {
                return callable_inst_id;
            }
        }
    }

    if (self.callable_pipeline.lambda_solved.getLocalCallableTemplate(
        self.current_module_idx,
        capture_pattern_idx,
    )) |template_id| {
        if (try self.lookupSpecializedCallableInstForTemplateAndMonotype(
            template_id,
            capture_monotype,
            self.current_module_idx,
        )) |callable_inst_id| {
            return callable_inst_id;
        }
    }

    if (isTopLevelPattern(self.all_module_envs[self.current_module_idx], capture_pattern_idx)) {
        if (lookupPipelinedPatternCallableInstsInContext(self, .none, self.current_module_idx, capture_pattern_idx)) |root_callable_inst_ids| {
            if (try self.selectCallableInstForFnMonotype(
                root_callable_inst_ids,
                capture_monotype,
                self.current_module_idx,
            )) |callable_inst_id| {
                return callable_inst_id;
            }
            if (root_callable_inst_ids.len == 1) {
                return root_callable_inst_ids[0];
            }
            const pattern = self.all_module_envs[self.current_module_idx].store.getPattern(capture_pattern_idx);
            const ident = switch (pattern) {
                .assign => |assign| self.all_module_envs[self.current_module_idx].getIdent(assign.ident),
                .as => |as_pattern| self.all_module_envs[self.current_module_idx].getIdent(as_pattern.ident),
                else => "<non-binding>",
            };
            std.debug.panic(
                "statement-only MIR invariant violated: top-level capture pattern {d} ({s}:{s}) in module {d} had ambiguous root callable inst set of size {d}",
                .{
                    @intFromEnum(capture_pattern_idx),
                    @tagName(pattern),
                    ident,
                    self.current_module_idx,
                    root_callable_inst_ids.len,
                },
            );
        }
    }

    return null;
}

fn appendCaptureMaterialization(
    self: *Self,
    out: *std.ArrayList(CaptureMaterialization),
    module_env: *const ModuleEnv,
    closure_expr_idx: CIR.Expr.Idx,
    closure_callable_inst_id: ?Pipeline.CallableInstId,
    capture_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!MIR.LocalId {
    const capture_monotype = try self.resolveRuntimePatternMonotype(
        module_env,
        closure_expr_idx,
        closure_callable_inst_id,
        capture_pattern_idx,
    );
    const capture_local = try self.freshSyntheticLocal(capture_monotype, false);

    if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, capture_pattern_idx)) |source| {
        try out.append(self.allocator, .{ .source_expr = .{
            .local = capture_local,
            .source = source,
        } });
        return capture_local;
    }

    if (self.store.monotype_store.getMonotype(capture_monotype) == .func) {
        if (try self.resolveExactCallableInstForCapturePattern(
            closure_expr_idx,
            capture_pattern_idx,
            capture_monotype,
            closure_callable_inst_id,
        )) |callable_inst_id| {
            try self.bindExactCallableLocal(capture_local, callable_inst_id);
            try out.append(self.allocator, .{ .exact_callable = .{
                .local = capture_local,
                .callable_inst_id = callable_inst_id,
            } });
            return capture_local;
        }
    }

    if (findDefByPattern(module_env, capture_pattern_idx)) |def_idx| {
        try out.append(self.allocator, .{ .top_level_def = .{
            .local = capture_local,
            .def_idx = def_idx,
            .symbol = try self.lookupSymbolForPattern(self.current_module_idx, capture_pattern_idx),
            .module_idx = self.current_module_idx,
        } });
        return capture_local;
    }

    const closure_template = if (closure_callable_inst_id) |callable_inst_id|
        self.callable_pipeline.lambda_solved.getCallableTemplate(
            self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id).template,
        )
    else
        null;
    std.debug.panic(
        "statement-only MIR capture pattern {d} was not in scope and had no exact source; closure_expr={d} closure_callable_inst={d} current_callable_inst={d} current_scope={d} module={d} closure_template_kind={s} closure_template_expr={d} closure_template_owner={d} pattern_mono={d}@{d} current_pattern_callable_inst={d} closure_pattern_callable_inst={d}",
        .{
            @intFromEnum(capture_pattern_idx),
            @intFromEnum(closure_expr_idx),
            if (closure_callable_inst_id) |id| @intFromEnum(id) else std.math.maxInt(u32),
            @intFromEnum(self.current_callable_inst_context),
            self.current_pattern_scope,
            self.current_module_idx,
            if (closure_template) |template| @tagName(template.kind) else "none",
            if (closure_template) |template| @intFromEnum(template.cir_expr) else std.math.maxInt(u32),
            if (closure_template) |template| @intFromEnum(template.lexical_owner_template) else std.math.maxInt(u32),
            @intFromEnum(capture_monotype),
            self.current_module_idx,
            if (self.callable_pipeline.getContextPatternCallableInst(
                sourceContextForCallableInst(self, self.current_callable_inst_context),
                self.current_module_idx,
                capture_pattern_idx,
            )) |id| @intFromEnum(id) else std.math.maxInt(u32),
            if (closure_callable_inst_id) |id|
                if (self.callable_pipeline.getContextPatternCallableInst(
                    sourceContextForCallableInst(self, id),
                    self.current_module_idx,
                    capture_pattern_idx,
                )) |capture_id| @intFromEnum(capture_id) else std.math.maxInt(u32)
            else
                std.math.maxInt(u32),
        },
    );
}

fn recursiveCallableInstForPattern(
    recursive_members: []const RecursiveGroupMember,
    pattern_idx: CIR.Pattern.Idx,
) ?Pipeline.CallableInstId {
    for (recursive_members) |member| {
        if (member.binding_pattern == pattern_idx) return member.callable_inst_id;
    }
    return null;
}

fn callableInstCaptureGraphReaches(
    self: *Self,
    from_callable_inst_id: Pipeline.CallableInstId,
    target_callable_inst_id: Pipeline.CallableInstId,
    visited: *std.AutoHashMapUnmanaged(u32, void),
) Allocator.Error!bool {
    if (from_callable_inst_id == target_callable_inst_id) return true;

    const from_key = @intFromEnum(from_callable_inst_id);
    const entry = try visited.getOrPut(self.allocator, from_key);
    if (entry.found_existing) return false;

    const template = self.callable_pipeline.lambda_solved.getCallableTemplate(
        self.callable_pipeline.lambda_specialize.getCallableInst(from_callable_inst_id).template,
    );
    const runtime_closure = self.runtimeClosureExprForCallableInst(from_callable_inst_id) orelse return false;
    for (runtime_closure.module_env.store.sliceCaptures(runtime_closure.closure.captures)) |capture_idx| {
        const capture = runtime_closure.module_env.store.getCapture(capture_idx);
        const capture_callable_inst_id = self.callable_pipeline.getClosureCaptureCallableInst(
            from_callable_inst_id,
            template.module_idx,
            runtime_closure.expr_idx,
            capture.pattern_idx,
        ) orelse continue;
        if (try self.callableInstCaptureGraphReaches(
            capture_callable_inst_id,
            target_callable_inst_id,
            visited,
        )) {
            return true;
        }
    }

    return false;
}

fn callableInstSharesRecursiveEnvironment(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
    capture_callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!bool {
    var visited: std.AutoHashMapUnmanaged(u32, void) = .empty;
    defer visited.deinit(self.allocator);
    return self.callableInstCaptureGraphReaches(
        capture_callable_inst_id,
        callable_inst_id,
        &visited,
    );
}

fn planClosureLowering(
    self: *Self,
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!ClosureLowerPlan {
    var recursive_members = std.ArrayList(RecursiveGroupMember).empty;
    errdefer recursive_members.deinit(self.allocator);

    for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        const capture_callable_inst_id = self.callable_pipeline.lambda_specialize.getClosureCaptureCallableInst(
            callable_inst_id,
            self.current_module_idx,
            expr_idx,
            capture.pattern_idx,
        ) orelse continue;

        const capture_template = self.callable_pipeline.lambda_solved.getCallableTemplate(
            self.callable_pipeline.lambda_specialize.getCallableInst(capture_callable_inst_id).template,
        );
        const binding_pattern = capture_template.binding_pattern orelse continue;
        if (binding_pattern != capture.pattern_idx) continue;

        const shares_recursive_environment = try self.callableInstSharesRecursiveEnvironment(
            callable_inst_id,
            capture_callable_inst_id,
        );
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
    callable_inst_id: ?Pipeline.CallableInstId,
) Allocator.Error!usize {
    var lower_plan_storage: ?ClosureLowerPlan = null;
    defer {
        if (lower_plan_storage != null) {
            lower_plan_storage.?.deinit(self.allocator);
        }
    }

    const recursive_members = if (callable_inst_id) |id| blk: {
        lower_plan_storage = try self.planClosureLowering(module_env, expr_idx, closure, id);
        break :blk lower_plan_storage.?.recursive_members.items;
    } else &.{};

    var count: usize = 0;
    for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        if (recursiveCallableInstForPattern(recursive_members, capture.pattern_idx) != null) continue;
        if (!(try self.capturePatternRequiresRuntimeMaterialization(
            module_env,
            expr_idx,
            callable_inst_id,
            capture.pattern_idx,
        ))) continue;
        count += 1;
    }
    return count;
}

fn capturePatternRequiresRuntimeMaterialization(
    self: *Self,
    module_env: *const ModuleEnv,
    closure_expr_idx: CIR.Expr.Idx,
    closure_callable_inst_id: ?Pipeline.CallableInstId,
    capture_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!bool {
    const capture_monotype = try self.resolveRuntimePatternMonotype(
        module_env,
        closure_expr_idx,
        closure_callable_inst_id,
        capture_pattern_idx,
    );
    if (self.store.monotype_store.getMonotype(capture_monotype) != .func) {
        return true;
    }

    if (try self.resolveExactCallableInstForCapturePattern(
        closure_expr_idx,
        capture_pattern_idx,
        capture_monotype,
        closure_callable_inst_id,
    )) |capture_callable_inst_id| {
        return self.callableInstProducesClosureValue(capture_callable_inst_id);
    }

    if (try self.resolveExactCallableResolutionForCapturePattern(capture_pattern_idx)) |capture_resolution| {
        return capture_resolution.requires_hidden_capture;
    }

    return true;
}

fn reserveResolvedCallableInstLambdaSkeleton(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
    fn_monotype: Monotype.Idx,
    source_region: Region,
) Allocator.Error!MIR.LambdaId {
    const callable_inst_key = @intFromEnum(callable_inst_id);
    if (self.lowered_callable_insts.get(callable_inst_key)) |existing| return existing;
    if (self.reserved_callable_insts.get(callable_inst_key)) |existing| return existing;

    const ret_monotype = switch (self.store.monotype_store.getMonotype(fn_monotype)) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR reserved callable lambda expected function monotype for callable inst {d}",
            .{@intFromEnum(callable_inst_id)},
        ),
    };
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
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
    fn_monotype: Monotype.Idx,
    callable_inst_id: Pipeline.CallableInstId,
    reserved_lambda: MIR.LambdaId,
) Allocator.Error!MIR.LambdaId {
    const cache_key = makeCallableCacheKey(self, expr_idx, fn_monotype);
    if (try self.lookupLoweredCallableLambdaCache(expr_idx, fn_monotype)) |cached| {
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
    const lambda_entry_scope = self.freshStatementPatternScope();
    self.current_pattern_scope = lambda_entry_scope;
    defer self.current_pattern_scope = saved_scope;
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
    const saved_implicit_lambda_captures = self.current_implicit_lambda_captures;
    var implicit_lambda_captures = ImplicitLambdaCaptureState.init(self.allocator);
    defer implicit_lambda_captures.deinit(self.allocator);
    if (!self.current_callable_inst_context.isNone()) {
        const active_template = self.callable_pipeline.lambda_solved.getCallableTemplate(
            self.callable_pipeline.lambda_specialize.getCallableInst(self.current_callable_inst_context).template,
        );
        if (active_template.kind == .lambda or active_template.kind == .closure) {
            self.current_implicit_lambda_captures = &implicit_lambda_captures;
        }
    }
    defer self.current_implicit_lambda_captures = saved_implicit_lambda_captures;

    const param_patterns = module_env.store.slicePatterns(lambda_expr.e_lambda.args);
    const params = try self.lowerLambdaParamLocals(module_env, lambda_expr.e_lambda.args);
    var stable_param_locals = try std.ArrayList(MIR.LocalId).initCapacity(self.allocator, self.store.getLocalSpan(params).len);
    defer stable_param_locals.deinit(self.allocator);
    stable_param_locals.appendSliceAssumeCapacity(self.store.getLocalSpan(params));
    const param_locals = stable_param_locals.items;
    for (param_patterns) |pattern_idx| {
        try self.predeclarePatternLocals(module_env, pattern_idx);
    }

    const capture_top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(capture_top);
    var capture_monotypes = std.ArrayList(Monotype.Idx).empty;
    defer capture_monotypes.deinit(self.allocator);
    var runtime_implicit_captures = std.ArrayList(ImplicitLambdaCapture).empty;
    defer runtime_implicit_captures.deinit(self.allocator);
    var seen_capture_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
    defer seen_capture_patterns.deinit();
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

    for (captures) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        if (!(try recordSeenCapturePattern(&seen_capture_patterns, capture.pattern_idx))) {
            std.debug.panic(
                "statement-only MIR invariant violated: trivial closure lambda {d} recorded duplicate capture pattern {d}",
                .{ @intFromEnum(expr_idx), @intFromEnum(capture.pattern_idx) },
            );
        }
        const capture_monotype = try self.resolveRuntimePatternMonotype(
            module_env,
            expr_idx,
            callable_inst_id,
            capture.pattern_idx,
        );
        const capture_local = try self.patternToLocalWithMonotype(capture.pattern_idx, capture_monotype);
        try capture_pattern_by_local.put(@intFromEnum(capture_local), capture.pattern_idx);
        try lambda_entry_bound_locals.add(capture_local);
        if (recursiveCallableInstForPattern(lower_plan.recursive_members.items, capture.pattern_idx)) |member_callable_inst_id| {
            try self.bindExactCallableLocal(capture_local, member_callable_inst_id);
            try recursive_captures.append(self.allocator, .{
                .local = capture_local,
                .callable_inst_id = member_callable_inst_id,
            });
            continue;
        }
        if (self.store.monotype_store.getMonotype(capture_monotype) == .func) {
            var exact_capture_resolution: ?ExactCallableResolution = null;
            if (try self.resolveExactCallableInstForCapturePattern(
                expr_idx,
                capture.pattern_idx,
                capture_monotype,
                callable_inst_id,
            )) |capture_callable_inst_id| {
                try self.bindExactCallableLocal(capture_local, capture_callable_inst_id);
                const exact_callable = self.store.getLocal(capture_local).exact_callable orelse unreachable;
                exact_capture_resolution = .{
                    .lambda = exact_callable.lambda,
                    .requires_hidden_capture = exact_callable.requires_hidden_capture,
                };
                if (!exact_capture_resolution.?.requires_hidden_capture) {
                    try callable_only_captures.append(self.allocator, .{
                        .local = capture_local,
                        .callable_inst_id = capture_callable_inst_id,
                        .lambda_id = null,
                    });
                    continue;
                }
            } else if (try self.resolveExactCallableResolutionForCapturePattern(capture.pattern_idx)) |capture_resolution| {
                try self.bindExactLambdaLocal(capture_local, capture_resolution.lambda);
                exact_capture_resolution = capture_resolution;
                if (!capture_resolution.requires_hidden_capture) {
                    try callable_only_captures.append(self.allocator, .{
                        .local = capture_local,
                        .callable_inst_id = null,
                        .lambda_id = capture_resolution.lambda,
                    });
                    continue;
                }
            }
            try self.scratch_local_ids.append(capture_local);
            try capture_monotypes.append(
                self.allocator,
                try self.runtimeCaptureFieldMonotype(capture_monotype, exact_capture_resolution),
            );
            continue;
        }
        try self.scratch_local_ids.append(capture_local);
        try capture_monotypes.append(self.allocator, capture_monotype);
    }

    try self.store.installReservedLambdaPrototype(reserved_lambda, .{
        .fn_monotype = fn_monotype,
        .params = params,
        .body = @enumFromInt(0),
        .ret_monotype = ret_monotype,
        .debug_name = .none,
        .source_region = module_env.store.getExprRegion(expr_idx),
        .captures_param = null,
        .recursion = .not_recursive,
        .hosted = null,
    });

    try self.seedExactCallableParamLocals(param_patterns, param_locals);

    const result_local = try self.freshSyntheticLocal(ret_monotype, false);
    const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = result_local } });
    const lambda_body_scope = self.freshChildPatternScope(lambda_entry_scope);
    self.current_pattern_scope = lambda_body_scope;
    var body = try self.lowerCirExprInto(lambda_expr.e_lambda.body, result_local, ret_stmt);
    self.current_pattern_scope = lambda_entry_scope;

    for (implicit_lambda_captures.items.items) |capture| {
        if (!(try recordSeenCapturePattern(&seen_capture_patterns, capture.pattern_idx))) continue;
        try capture_pattern_by_local.put(@intFromEnum(capture.local), capture.pattern_idx);
        const capture_resolution = try self.implicitCaptureExactResolution(capture);
        if (builtin.mode == .Debug and
            self.store.monotype_store.getMonotype(capture.monotype) == .func and
            capture_resolution == null)
        {
            std.debug.panic(
                "statement-only MIR TODO: implicit recursive-closure capture pattern {d} local {d} has function monotype {d} but no exact callable resolution (capture_callable_inst={d}, capture_lambda={d}, current_callable_inst={d}, current_scope={d}, module={d})",
                .{
                    @intFromEnum(capture.pattern_idx),
                    @intFromEnum(capture.local),
                    @intFromEnum(capture.monotype),
                    if (capture.exact_callable_inst_id) |id| @intFromEnum(id) else std.math.maxInt(u32),
                    if (capture.exact_lambda_id) |id| @intFromEnum(id) else std.math.maxInt(u32),
                    @intFromEnum(self.current_callable_inst_context),
                    self.current_pattern_scope,
                    self.current_module_idx,
                },
            );
        }
        if (capture_resolution) |resolved| {
            if (!resolved.requires_hidden_capture) {
                try callable_only_captures.append(self.allocator, .{
                    .local = capture.local,
                    .callable_inst_id = null,
                    .lambda_id = resolved.lambda,
                });
                continue;
            }
        }

        try self.scratch_local_ids.append(capture.local);
        try capture_monotypes.append(
            self.allocator,
            try self.runtimeCaptureFieldMonotype(
                capture.monotype,
                capture_resolution,
            ),
        );
        try runtime_implicit_captures.append(self.allocator, capture);
    }

    const nonrecursive_capture_locals = self.scratch_local_ids.sliceFromStart(capture_top);
    if (runtime_implicit_captures.items.len != 0) {
        var runtime_capture_patterns = std.ArrayList(CIR.Pattern.Idx).empty;
        defer runtime_capture_patterns.deinit(self.allocator);
        try runtime_capture_patterns.ensureTotalCapacity(self.allocator, runtime_implicit_captures.items.len);
        for (runtime_implicit_captures.items) |capture| {
            runtime_capture_patterns.appendAssumeCapacity(capture.pattern_idx);
        }
        try self.lowered_callable_runtime_captures.put(
            @intFromEnum(callable_inst_id),
            try self.addRuntimeCapturePatternSpan(runtime_capture_patterns.items),
        );
    }
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
                            @tagName(existing_ref),
                            switch (existing_ref) {
                                .local => |source_local| @intFromEnum(source_local),
                                .field => |field| @intFromEnum(field.source),
                                .tag_payload => |payload| @intFromEnum(payload.source),
                                .nominal => |nominal| @intFromEnum(nominal.backing),
                                .discriminant => |discriminant| @intFromEnum(discriminant.source),
                            },
                            if (self.store.getLocalDefOpt(switch (existing_ref) {
                                .local => |source_local| source_local,
                                .field => |field| field.source,
                                .tag_payload => |payload| payload.source,
                                .nominal => |nominal| nominal.backing,
                                .discriminant => |discriminant| discriminant.source,
                            })) |source_def| @tagName(source_def) else "none",
                            switch (existing_ref) {
                                .field => |field| field.field_idx,
                                else => std.math.maxInt(u32),
                            },
                            switch (existing_ref) {
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
        try self.bindExactLambdaLocal(recursive_capture.local, member_lambda);
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
                .ownership = .move,
            } }, body);
        }
    }

    var param_i = param_patterns.len;
    while (param_i > 0) {
        param_i -= 1;
        if (!self.paramPatternNeedsBindingStmt(module_env, param_patterns[param_i], param_locals[param_i])) continue;
        body = try self.lowerPatternBindingLocalInto(module_env, param_patterns[param_i], param_locals[param_i], body);
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
            try self.bindExactLambdaLocal(callable_only_capture.local, lambda_id);
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

fn callableInstProducesClosureValue(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!bool {
    if (self.lowered_callable_insts.get(@intFromEnum(callable_inst_id))) |lambda_id| {
        return self.store.getLambdaAnyState(lambda_id).captures_param != null;
    }

    const callable_key = @intFromEnum(callable_inst_id);
    if (self.callable_inst_produces_closure_value.get(callable_key)) |cached| {
        return cached;
    }

    const in_progress = try self.in_progress_closure_value_callables.getOrPut(callable_key);
    if (in_progress.found_existing) {
        std.debug.panic(
            "statement-only MIR invariant violated: recursive closure-value resolution for callable inst {d}",
            .{@intFromEnum(callable_inst_id)},
        );
    }
    defer std.debug.assert(self.in_progress_closure_value_callables.remove(callable_key));

    const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
    const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
    const module_idx = template.module_idx;
    const module_env = self.all_module_envs[module_idx];
    const switching_module = module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    const saved_callable_inst_context = self.current_callable_inst_context;
    const saved_source_context = self.current_source_context;
    const saved_type_var_seen = self.type_var_seen;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;

    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.current_callable_inst_context = callable_inst_id;
    self.current_source_context = .{ .active = .{ .callable_inst = @enumFromInt(@intFromEnum(callable_inst_id)) } };

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
        self.current_source_context = saved_source_context;
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
        const subst = self.callable_pipeline.context_mono.getTypeSubst(callable_inst.subst);
        for (self.callable_pipeline.context_mono.getTypeSubstEntries(subst.entries)) |entry| {
            if (builtin.mode == .Debug and entry.key.module_idx != module_idx) {
                std.debug.panic(
                    "Lower: callable inst subst entry from module {d} imported into module {d}",
                    .{ entry.key.module_idx, module_idx },
                );
            }
            const imported_mono = try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                entry.monotype.idx,
                entry.monotype.module_idx,
                module_idx,
            );
            try self.type_var_seen.put(entry.key.type_var, imported_mono);
        }
    }

    const produces_closure = switch (template.kind) {
        .closure, .lambda => blk: {
            if (self.lowered_callable_runtime_captures.get(callable_key)) |runtime_capture_patterns| {
                break :blk self.getRuntimeCapturePatternSpan(runtime_capture_patterns).len != 0;
            }
            const runtime_closure = self.runtimeClosureExprForCallableInst(callable_inst_id) orelse break :blk false;
            break :blk (try self.closureRuntimeCaptureCount(
                runtime_closure.module_env,
                runtime_closure.expr_idx,
                runtime_closure.closure,
                callable_inst_id,
            )) != 0;
        },
        .top_level_def, .hosted_lambda => false,
    };
    try self.callable_inst_produces_closure_value.put(callable_key, produces_closure);
    return produces_closure;
}

const RuntimeClosureExpr = struct {
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
    closure: CIR.Expr.Closure,
};

fn runtimeClosureExprForCallableInst(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
) ?RuntimeClosureExpr {
    const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
    const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
    const module_env = self.all_module_envs[template.module_idx];

    return switch (template.kind) {
        .closure => blk: {
            const expr = module_env.store.getExpr(template.cir_expr);
            const closure = switch (expr) {
                .e_closure => |closure| closure,
                else => std.debug.panic(
                    "statement-only MIR callable-inst closure lowering expected closure expr for callable inst {d}, found {s}",
                    .{ @intFromEnum(callable_inst_id), @tagName(expr) },
                ),
            };
            break :blk .{
                .module_env = module_env,
                .expr_idx = template.cir_expr,
                .closure = closure,
            };
        },
        .lambda => blk: {
            const closure_expr_idx = self.findWrappingClosureExpr(module_env, template.cir_expr) orelse break :blk null;
            const closure_expr = module_env.store.getExpr(closure_expr_idx);
            const closure = switch (closure_expr) {
                .e_closure => |closure| closure,
                else => unreachable,
            };
            break :blk .{
                .module_env = module_env,
                .expr_idx = closure_expr_idx,
                .closure = closure,
            };
        },
        .top_level_def, .hosted_lambda => null,
    };
}

fn lowerResolvedCallableInstValueInto(
    self: *Self,
    callable_inst_id: Pipeline.CallableInstId,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const lambda_id = try self.lowerResolvedCallableInstLambda(callable_inst_id);
    try self.bindExactCallableLocal(target, callable_inst_id);
    if (!(try self.callableInstProducesClosureValue(callable_inst_id))) {
        return self.store.addCFStmt(self.allocator, .{ .assign_lambda = .{
            .target = target,
            .lambda = lambda_id,
            .next = next,
        } });
    }

    const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
    const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
    if (self.runtimeClosureExprForCallableInst(callable_inst_id) == null) {
        if (self.lowered_callable_runtime_captures.get(@intFromEnum(callable_inst_id))) |runtime_capture_patterns| {
            const module_idx = template.module_idx;
            const module_env = self.all_module_envs[template.module_idx];
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
            var capture_materializations = std.ArrayList(CaptureMaterialization).empty;
            defer capture_materializations.deinit(self.allocator);
            var seen_capture_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
            defer seen_capture_patterns.deinit();

            for (self.getRuntimeCapturePatternSpan(runtime_capture_patterns)) |capture_pattern_idx| {
                if (!(try recordSeenCapturePattern(&seen_capture_patterns, capture_pattern_idx))) continue;
                const skipped_callable_backed = self.isSkippedCallableBackedBindingPattern(
                    self.current_module_idx,
                    capture_pattern_idx,
                );
                const capture_local = if (!skipped_callable_backed) capture_blk: {
                    if (try self.lookupReusableCaptureLocal(capture_pattern_idx)) |existing| break :capture_blk existing;
                    break :capture_blk try self.appendCaptureMaterialization(
                        &capture_materializations,
                        module_env,
                        template.cir_expr,
                        callable_inst_id,
                        capture_pattern_idx,
                    );
                } else try self.appendCaptureMaterialization(
                    &capture_materializations,
                    module_env,
                    template.cir_expr,
                    callable_inst_id,
                    capture_pattern_idx,
                );
                try self.scratch_local_ids.append(capture_local);
            }

            var current = try self.store.addCFStmt(self.allocator, .{ .assign_closure = .{
                .target = target,
                .lambda = lambda_id,
                .captures = try self.store.addLocalSpan(
                    self.allocator,
                    self.scratch_local_ids.sliceFromStart(capture_top),
                ),
                .next = next,
            } });

            var i = capture_materializations.items.len;
            while (i > 0) {
                i -= 1;
                switch (capture_materializations.items[i]) {
                    .exact_callable => |materialization| {
                        current = try self.lowerResolvedCallableInstValueInto(
                            materialization.callable_inst_id,
                            materialization.local,
                            current,
                        );
                    },
                    .top_level_def => |materialization| {
                        current = try self.materializeTopLevelDefInto(
                            materialization.module_idx,
                            materialization.def_idx,
                            materialization.symbol,
                            materialization.local,
                            current,
                            null,
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
    }

    const runtime_closure = self.runtimeClosureExprForCallableInst(callable_inst_id) orelse std.debug.panic(
        "statement-only MIR invariant violated: callable inst {d} required runtime captures but had neither runtime closure expr nor implicit runtime capture metadata (template kind {s})",
        .{ @intFromEnum(callable_inst_id), @tagName(template.kind) },
    );
    const module_idx = template.module_idx;
    const module_env = runtime_closure.module_env;
    const closure = runtime_closure.closure;

    var lower_plan = try self.planClosureLowering(module_env, runtime_closure.expr_idx, closure, callable_inst_id);
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

    var capture_materializations = std.ArrayList(CaptureMaterialization).empty;
    defer capture_materializations.deinit(self.allocator);
    var seen_capture_patterns = std.AutoHashMap(CIR.Pattern.Idx, void).init(self.allocator);
    defer seen_capture_patterns.deinit();

    for (module_env.store.sliceCaptures(closure.captures)) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        if (!(try recordSeenCapturePattern(&seen_capture_patterns, capture.pattern_idx))) {
            std.debug.panic(
                "statement-only MIR invariant violated: runtime closure expr {d} recorded duplicate capture pattern {d}",
                .{ @intFromEnum(runtime_closure.expr_idx), @intFromEnum(capture.pattern_idx) },
            );
        }
        if (recursiveCallableInstForPattern(lower_plan.recursive_members.items, capture.pattern_idx) != null) continue;
        if (!(try self.capturePatternRequiresRuntimeMaterialization(
            module_env,
            template.cir_expr,
            callable_inst_id,
            capture.pattern_idx,
        ))) continue;

        const skipped_callable_backed = self.isSkippedCallableBackedBindingPattern(
            self.current_module_idx,
            capture.pattern_idx,
        );
        const capture_local = if (!skipped_callable_backed) capture_blk: {
            if (try self.lookupReusableCaptureLocal(capture.pattern_idx)) |existing| break :capture_blk existing;
            break :capture_blk try self.appendCaptureMaterialization(
                &capture_materializations,
                module_env,
                runtime_closure.expr_idx,
                callable_inst_id,
                capture.pattern_idx,
            );
        } else try self.appendCaptureMaterialization(
            &capture_materializations,
            module_env,
            runtime_closure.expr_idx,
            callable_inst_id,
            capture.pattern_idx,
        );

        try self.scratch_local_ids.append(capture_local);
    }

    if (self.lowered_callable_runtime_captures.get(@intFromEnum(callable_inst_id))) |runtime_capture_patterns| {
        for (self.getRuntimeCapturePatternSpan(runtime_capture_patterns)) |capture_pattern_idx| {
            if (!(try recordSeenCapturePattern(&seen_capture_patterns, capture_pattern_idx))) continue;
            const skipped_callable_backed = self.isSkippedCallableBackedBindingPattern(
                self.current_module_idx,
                capture_pattern_idx,
            );
            const capture_local = if (!skipped_callable_backed) capture_blk: {
                if (try self.lookupReusableCaptureLocal(capture_pattern_idx)) |existing| break :capture_blk existing;
                break :capture_blk try self.appendCaptureMaterialization(
                    &capture_materializations,
                    module_env,
                    runtime_closure.expr_idx,
                    callable_inst_id,
                    capture_pattern_idx,
                );
            } else try self.appendCaptureMaterialization(
                &capture_materializations,
                module_env,
                runtime_closure.expr_idx,
                callable_inst_id,
                capture_pattern_idx,
            );

            try self.scratch_local_ids.append(capture_local);
        }
    }

    const runtime_captures = self.scratch_local_ids.sliceFromStart(capture_top);
    try self.bindExactLambdaLocal(target, lambda_id);
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
            .exact_callable => |materialization| {
                current = try self.lowerResolvedCallableInstValueInto(
                    materialization.callable_inst_id,
                    materialization.local,
                    current,
                );
            },
            .top_level_def => |materialization| {
                current = try self.materializeTopLevelDefInto(
                    materialization.module_idx,
                    materialization.def_idx,
                    materialization.symbol,
                    materialization.local,
                    current,
                    null,
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
    source: Pipeline.ExprSource,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const saved_source_context = self.current_source_context;
    defer self.current_source_context = saved_source_context;
    if (self.current_callable_inst_context.isNone()) {
        self.current_source_context = .{ .active = .{ .provenance_expr = .{
            .module_idx = source.module_idx,
            .expr_idx = source.expr_idx,
        } } };
    }

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

    try self.refreshTargetLocalMonotypeFromExprIntoModule(
        source.expr_idx,
        source.module_idx,
        saved_module_idx,
        target,
    );
    return self.lowerCirExprInto(source.expr_idx, target, next);
}

fn resolveRuntimePatternMonotype(
    self: *Self,
    module_env: *const ModuleEnv,
    closure_expr_idx: CIR.Expr.Idx,
    closure_callable_inst_id: ?Pipeline.CallableInstId,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!Monotype.Idx {
    if (self.lookupPipelinedPatternMonotype(pattern_idx)) |resolved| {
        return self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            resolved.idx,
            resolved.module_idx,
            self.current_module_idx,
        );
    }

    if (closure_callable_inst_id) |callable_inst_id| {
        if (self.callable_pipeline.lambda_specialize.getClosureCaptureMonotype(
            callable_inst_id,
            self.current_module_idx,
            closure_expr_idx,
            pattern_idx,
        )) |resolved| {
            return self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                resolved.idx,
                resolved.module_idx,
                self.current_module_idx,
            );
        }
    }

    if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, pattern_idx)) |source| {
        return self.resolveExprMonotypeInModule(source.module_idx, source.expr_idx);
    }

    if (findDefByPattern(module_env, pattern_idx)) |def_idx| {
        return self.resolveExprMonotypeInModule(self.current_module_idx, module_env.store.getDef(def_idx).expr);
    }

    return self.monotypeFromTypeVarWithBindings(
        self.current_module_idx,
        self.types_store,
        ModuleEnv.varFrom(pattern_idx),
        &self.type_var_seen,
    );
}

fn runtimeCaptureFieldMonotype(
    self: *Self,
    semantic_monotype: Monotype.Idx,
    exact_resolution: ?ExactCallableResolution,
) Allocator.Error!Monotype.Idx {
    if (self.store.monotype_store.getMonotype(semantic_monotype) != .func) {
        return semantic_monotype;
    }

    const resolved = exact_resolution orelse std.debug.panic(
        "statement-only MIR TODO: missing exact callable resolution for function-valued runtime capture field",
        .{},
    );
    if (!resolved.requires_hidden_capture) {
        std.debug.panic(
            "statement-only MIR invariant violated: callable-only lambda must not be materialized into a runtime capture field",
            .{},
        );
    }

    const captures_param = self.store.getLambdaAnyState(resolved.lambda).captures_param orelse std.debug.panic(
        "statement-only MIR invariant violated: callable resolution for lambda {d} required hidden captures but the lambda has no captures_param",
        .{@intFromEnum(resolved.lambda)},
    );
    return self.store.getLocal(captures_param).monotype;
}

fn resolveExprMonotypeInModule(
    self: *Self,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!Monotype.Idx {
    if (module_idx == self.current_module_idx) {
        return self.resolveMonotype(expr_idx);
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

    return self.resolveMonotype(expr_idx);
}

fn refreshTargetLocalMonotypeFromExprIntoModule(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    expr_module_idx: u32,
    target_module_idx: u32,
    target: MIR.LocalId,
) Allocator.Error!void {
    const monotype = try self.resolveMonotype(expr_idx);
    if (monotype.isNone()) return;
    self.store.getLocalPtr(target).monotype = if (expr_module_idx == target_module_idx)
        monotype
    else
        try self.remapMonotypeBetweenModules(monotype, expr_module_idx, target_module_idx);
}

fn refreshTargetLocalMonotypeFromExpr(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
) Allocator.Error!void {
    const monotype = try self.resolveMonotype(expr_idx);
    if (monotype.isNone()) return;
    self.store.getLocalPtr(target).monotype = monotype;
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

    const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
    const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
    const module_idx = template.module_idx;
    const module_env = self.all_module_envs[module_idx];
    const template_expr = module_env.store.getExpr(template.cir_expr);
    const fn_monotype = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
        module_idx,
    );
    const reserved_lambda = try self.reserveResolvedCallableInstLambdaSkeleton(
        callable_inst_id,
        fn_monotype,
        template.source_region,
    );
    const switching_module = module_idx != self.current_module_idx;
    const saved_module_idx = self.current_module_idx;
    const saved_types_store = self.types_store;
    const saved_ident_store = self.mono_scratches.ident_store;
    const saved_module_env = self.mono_scratches.module_env;
    const saved_mono_module_idx = self.mono_scratches.module_idx;
    const saved_callable_inst_context = self.current_callable_inst_context;
    const saved_source_context = self.current_source_context;
    const saved_type_var_seen = self.type_var_seen;
    const saved_nominal_cycle_breakers = self.nominal_cycle_breakers;

    self.type_var_seen = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(self.allocator);
    self.current_callable_inst_context = callable_inst_id;
    self.current_source_context = .{ .active = .{ .callable_inst = @enumFromInt(@intFromEnum(callable_inst_id)) } };

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
        self.current_source_context = saved_source_context;
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
        const subst = self.callable_pipeline.context_mono.getTypeSubst(callable_inst.subst);
        for (self.callable_pipeline.context_mono.getTypeSubstEntries(subst.entries)) |entry| {
            if (builtin.mode == .Debug and entry.key.module_idx != module_idx) {
                std.debug.panic(
                    "Lower: callable inst subst entry from module {d} imported into module {d}",
                    .{ entry.key.module_idx, module_idx },
                );
            }
            const imported_mono = try self.importMonotypeFromStore(
                &self.callable_pipeline.context_mono.monotype_store,
                entry.monotype.idx,
                entry.monotype.module_idx,
                module_idx,
            );
            try self.type_var_seen.put(entry.key.type_var, imported_mono);
        }
    }

    const lambda_id = if (self.runtimeClosureExprForCallableInst(callable_inst_id)) |runtime_closure|
        try self.lowerReservedTrivialClosureLambda(
            runtime_closure.module_env,
            runtime_closure.expr_idx,
            runtime_closure.closure,
            fn_monotype,
            callable_inst_id,
            reserved_lambda,
        )
    else switch (template_expr) {
        .e_lambda => |lambda| try self.lowerLambdaInto(
            module_env,
            template.cir_expr,
            lambda,
            fn_monotype,
            reserved_lambda,
        ),
        .e_hosted_lambda => |hosted| try self.lowerLambdaInto(
            module_env,
            template.cir_expr,
            .{
                .args = hosted.args,
                .body = hosted.body,
            },
            fn_monotype,
            reserved_lambda,
        ),
        else => std.debug.panic(
            "statement-only MIR callable-inst lambda lowering expected callable template, found {s}",
            .{@tagName(template_expr)},
        ),
    };

    _ = self.reserved_callable_insts.remove(callable_inst_key);
    try self.lowered_callable_insts.put(callable_inst_key, lambda_id);
    return lambda_id;
}

fn resolveAppliedCallFnMonotype(
    self: *Self,
    result_monotype: Monotype.Idx,
    arg_exprs: []const CIR.Expr.Idx,
    effectful: bool,
) Allocator.Error!Monotype.Idx {
    var arg_monotypes = std.ArrayList(Monotype.Idx).empty;
    defer arg_monotypes.deinit(self.allocator);

    for (arg_exprs) |arg_expr_idx| {
        try arg_monotypes.append(self.allocator, try self.resolveMonotype(arg_expr_idx));
    }

    const arg_span = try self.store.monotype_store.addIdxSpan(self.allocator, arg_monotypes.items);
    return self.store.monotype_store.addMonotype(self.allocator, .{ .func = .{
        .args = arg_span,
        .ret = result_monotype,
        .effectful = effectful,
    } });
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
    dispatch_target: Pipeline.DispatchExprTarget,
    result_expr_idx: CIR.Expr.Idx,
    receiver_expr: ?CIR.Expr.Idx,
    arg_exprs: []const CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    var actual_arg_exprs = std.ArrayList(CIR.Expr.Idx).empty;
    defer actual_arg_exprs.deinit(self.allocator);
    if (receiver_expr) |receiver| {
        try actual_arg_exprs.append(self.allocator, receiver);
    }
    try actual_arg_exprs.appendSlice(self.allocator, arg_exprs);

    const callee_effectful = try self.dispatchTargetEffectful(dispatch_target);
    const callee_monotype = try self.resolveAppliedCallFnMonotype(
        self.store.getLocal(target).monotype,
        actual_arg_exprs.items,
        callee_effectful,
    );
    const target_env = self.all_module_envs[dispatch_target.module_idx];
    const target_def = target_env.store.getDef(dispatch_target.def_idx);
    const exact_dispatch_callable_inst =
        (try self.lookupPipelinedValueExprCallableInstForMonotypeInModule(
            dispatch_target.module_idx,
            target_def.expr,
            callee_monotype,
            self.current_module_idx,
        )) orelse
        self.lookupPipelinedExprCallableInstInContext(
            self.current_callable_inst_context,
            dispatch_target.module_idx,
            target_def.expr,
        ) orelse blk: {
            if (self.lookupPipelinedExprCallableInstsInContext(
                self.current_callable_inst_context,
                dispatch_target.module_idx,
                target_def.expr,
            )) |callable_inst_ids| {
                if (callable_inst_ids.len == 1) break :blk callable_inst_ids[0];
            }

            std.debug.panic(
                "statement-only MIR invariant violated: resolved dispatch target module={d} def={d} expr={d} has no exact callable inst in context={d} root_expr={d}",
                .{
                    dispatch_target.module_idx,
                    @intFromEnum(dispatch_target.def_idx),
                    @intFromEnum(target_def.expr),
                    @intFromEnum(self.current_callable_inst_context),
                    self.currentRootExprRawForDebug(),
                },
            );
        };
    const callee_local = try self.freshSyntheticLocal(callee_monotype, false);

    const top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(top);

    const call_stmt = try self.store.reserveCFStmt(self.allocator);

    var lowered_next = call_stmt;
    var i: usize = actual_arg_exprs.items.len;
    while (i > 0) {
        i -= 1;
        const arg_expr_idx = actual_arg_exprs.items[i];
        const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(arg_expr_idx), false);
        try self.scratch_local_ids.append(arg_local);
        lowered_next = try self.lowerCirExprInto(arg_expr_idx, arg_local, lowered_next);
    }

    lowered_next = try self.lowerResolvedCallableInstValueInto(
        exact_dispatch_callable_inst,
        callee_local,
        lowered_next,
    );

    const exact_call = ExactCallableResolution{
        .lambda = try self.lowerResolvedCallableInstLambda(exact_dispatch_callable_inst),
        .requires_hidden_capture = try self.callableInstProducesClosureValue(exact_dispatch_callable_inst),
    };
    const target_monotype = self.store.getLocal(target).monotype;
    const exact_result_callable_inst_id = if (self.store.monotype_store.getMonotype(target_monotype) == .func)
        try self.resolveBestExactCallableInstForExpr(
            result_expr_idx,
            target_monotype,
            self.current_module_idx,
        )
    else
        null;
    if (exact_result_callable_inst_id) |callable_inst_id| {
        try self.bindExactCallableLocal(target, callable_inst_id);
    } else if (self.store.monotype_store.getMonotype(target_monotype) == .func) {
        std.debug.panic(
            "statement-only MIR invariant violated: function-valued dispatch call result expr {d} lacked an exact callable specialization",
            .{@intFromEnum(result_expr_idx)},
        );
    } else {
        try self.refreshCallTargetMonotypeFromCallee(target, self.store.getLocal(callee_local).monotype);
    }

    std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
    const args = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.items.items[top..]);
    try self.store.finalizeCFStmt(call_stmt, .{ .assign_call = .{
        .target = target,
        .callee = callee_local,
        .exact_lambda = exact_call.lambda,
        .exact_requires_hidden_capture = exact_call.requires_hidden_capture,
        .args = args,
        .next = next,
    } });
    return lowered_next;
}

fn lowerCallInto(
    self: *Self,
    module_env: *const ModuleEnv,
    call_expr_idx: CIR.Expr.Idx,
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

        const assign_stmt = try self.store.reserveCFStmt(self.allocator);

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
        const args = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
        try self.store.finalizeCFStmt(assign_stmt, .{ .assign_low_level = .{
            .target = target,
            .op = ll_op,
            .args = args,
            .next = next,
        } });
        return lowered_next;
    }

    const cir_args = module_env.store.sliceExpr(call.args);
    const top = self.scratch_local_ids.top();
    defer self.scratch_local_ids.clearFrom(top);

    const call_fn_monotype = try self.resolveMonotype(call.func);
    const exact_lookup_call = switch (module_env.store.getExpr(call.func)) {
        .e_lookup_local => |lookup| blk: {
            const source_local = self.lookupExistingPatternLocalInAncestorScopes(
                self.current_module_idx,
                self.current_pattern_scope,
                lookup.pattern_idx,
            ) orelse break :blk null;
            break :blk try self.resolveExactCallableResolutionForLocal(source_local);
        },
        else => null,
    };
    const exact_callable_inst_id = blk: {
        if (exact_lookup_call != null) break :blk null;

        if (self.lookupPipelinedCallSiteCallableInsts(call_expr_idx)) |callable_inst_ids| {
            if (try self.selectExactCallCallableInstFromCandidates(
                callable_inst_ids,
                cir_args,
                call_fn_monotype,
                self.current_module_idx,
            )) |resolved| {
                break :blk resolved;
            }
        }

        if (self.lookupPipelinedCallSiteCallableInst(call_expr_idx)) |callable_inst_id| {
            if (try self.selectExactCallCallableInstFromCandidates(
                &.{callable_inst_id},
                cir_args,
                call_fn_monotype,
                self.current_module_idx,
            )) |resolved| {
                break :blk resolved;
            }
        }

        break :blk try self.resolveExactCallCallableInstFromArgs(
            call.func,
            cir_args,
            call_fn_monotype,
            self.current_module_idx,
        );
    };
    const callee_monotype = if (exact_callable_inst_id) |callable_inst_id| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
        break :blk try self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            callable_inst.fn_monotype,
            callable_inst.fn_monotype_module_idx,
            self.current_module_idx,
        );
    } else try self.resolveMonotype(call.func);
    const exact_call_lambda = if (exact_lookup_call) |resolved|
        resolved.lambda
    else if (exact_callable_inst_id) |callable_inst_id|
        try self.lowerResolvedCallableInstLambda(callable_inst_id)
    else
        null;
    const exact_call_requires_hidden_capture = if (exact_lookup_call) |resolved|
        resolved.requires_hidden_capture
    else if (exact_callable_inst_id) |callable_inst_id|
        try self.callableInstProducesClosureValue(callable_inst_id)
    else
        false;
    const callee_local = try self.freshSyntheticLocal(callee_monotype, false);
    const call_stmt = try self.store.reserveCFStmt(self.allocator);

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

    const target_monotype = self.store.getLocal(target).monotype;
    const exact_result_callable_inst_id = if (self.store.monotype_store.getMonotype(target_monotype) == .func)
        try self.resolveBestExactCallableInstForExpr(
            call_expr_idx,
            target_monotype,
            self.current_module_idx,
        )
    else
        null;
    if (exact_result_callable_inst_id) |callable_inst_id| {
        try self.bindExactCallableLocal(target, callable_inst_id);
    } else if (self.store.monotype_store.getMonotype(target_monotype) == .func) {
        std.debug.panic(
            "statement-only MIR invariant violated: function-valued call expr {d} lacked an exact callable specialization",
            .{@intFromEnum(call_expr_idx)},
        );
    } else {
        try self.refreshCallTargetMonotypeFromCallee(target, self.store.getLocal(callee_local).monotype);
    }

    std.mem.reverse(MIR.LocalId, self.scratch_local_ids.items.items[top..]);
    const args = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.items.items[top..]);
    try self.store.finalizeCFStmt(call_stmt, .{ .assign_call = .{
        .target = target,
        .callee = callee_local,
        .exact_lambda = exact_call_lambda,
        .exact_requires_hidden_capture = exact_call_requires_hidden_capture,
        .args = args,
        .next = next,
    } });
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

    const assign_stmt = try self.store.reserveCFStmt(self.allocator);

    var lowered_next = assign_stmt;
    lowered_next = try self.lowerCirExprInto(rhs_expr, rhs_local, lowered_next);
    lowered_next = try self.lowerCirExprInto(lhs_expr, lhs_local, lowered_next);

    const args = try self.store.addLocalSpan(self.allocator, self.scratch_local_ids.sliceFromStart(top));
    try self.store.finalizeCFStmt(assign_stmt, .{ .assign_low_level = .{
        .target = target,
        .op = op,
        .args = args,
        .next = next,
    } });
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

fn lowerUnaryNotInto(
    self: *Self,
    unary: CIR.Expr.UnaryNot,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const operand_mono = try self.resolveMonotype(unary.expr);
    _ = boolTagNamesForMonotype(self, operand_mono) orelse std.debug.panic(
        "statement-only MIR unary not expected Bool monotype for expr {d}",
        .{@intFromEnum(unary.expr)},
    );

    const source_local = try self.freshSyntheticLocal(operand_mono, false);
    const not_stmt = try self.lowerUnaryLowLevelInto(target, .bool_not, source_local, next);
    return self.lowerCirExprInto(unary.expr, source_local, not_stmt);
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
            const then_body = try self.lowerCirExprInto(binop.rhs, then_value, then_jump);
            try self.store.finalizeCFStmt(then_jump, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{then_value}),
            } });

            const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
                "statement-only MIR and expected Bool condition monotype for expr {d}",
                .{@intFromEnum(binop.lhs)},
            );
            const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, then_body, else_body);
            const switch_entry = try self.lowerCirExprInto(binop.lhs, cond_local, switch_stmt);
            return self.store.addCFStmt(self.allocator, .{ .join = .{
                .id = join_id,
                .params = try self.store.addLocalSpan(self.allocator, &.{target}),
                .body = next,
                .remainder = switch_entry,
            } });
        },
        .@"or" => {
            const cond_mono = try self.resolveMonotype(binop.lhs);
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
            const else_body = try self.lowerCirExprInto(binop.rhs, else_value, else_jump);
            try self.store.finalizeCFStmt(else_jump, .{ .jump = .{
                .id = join_id,
                .args = try self.store.addLocalSpan(self.allocator, &.{else_value}),
            } });

            const tag_names = boolTagNamesForMonotype(self, cond_mono) orelse std.debug.panic(
                "statement-only MIR or expected Bool condition monotype for expr {d}",
                .{@intFromEnum(binop.lhs)},
            );
            const switch_stmt = try self.lowerSwitchOnDiscriminant(cond_local, cond_mono, tag_names.true_name, then_body, else_body);
            const switch_entry = try self.lowerCirExprInto(binop.lhs, cond_local, switch_stmt);
            return self.store.addCFStmt(self.allocator, .{ .join = .{
                .id = join_id,
                .params = try self.store.addLocalSpan(self.allocator, &.{target}),
                .body = next,
                .remainder = switch_entry,
            } });
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
        const dot_args = module_env.store.sliceExpr(da.args.?);

        if (dot_args.len == 0 and std.mem.eql(u8, module_env.getIdent(da.field_name), "to_str")) {
            const receiver_local = try self.freshSyntheticLocal(receiver_mono, false);
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

        if (self.lookupPipelinedDispatchTarget(expr_idx)) |dispatch_target| {
            return self.lowerResolvedDispatchTargetCallInto(
                dispatch_target,
                expr_idx,
                da.receiver,
                dot_args,
                target,
                next,
            );
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
    const target_monotype = self.store.getLocal(target).monotype;
    if (self.store.monotype_store.getMonotype(target_monotype) == .func) {
        if (try self.resolveRecordFieldExactCallableInst(
            da.receiver,
            .{ .module_idx = self.current_module_idx, .ident = da.field_name },
            target_monotype,
            self.current_module_idx,
        )) |callable_inst_id| {
            try self.bindExactCallableLocal(target, callable_inst_id);
        } else {
            std.debug.panic(
                "statement-only MIR invariant violated: function-valued dot access expr {d} lacked an exact callable specialization",
                .{@intFromEnum(expr_idx)},
            );
        }
    }
    const field_stmt = try self.lowerRefInto(target, .{ .field = .{
        .source = receiver_local,
        .field_idx = field_idx,
        .ownership = .move,
    } }, next);

    return self.lowerCirExprInto(da.receiver, receiver_local, field_stmt);
}

fn lowerTupleAccessInto(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    tuple_access: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const tuple_mono = try self.resolveMonotype(tuple_access.tuple);
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
    const target_monotype = self.store.getLocal(target).monotype;
    if (self.store.monotype_store.getMonotype(target_monotype) == .func) {
        if (try self.resolveTupleElemExactCallableInst(
            tuple_access.tuple,
            tuple_access.elem_index,
            target_monotype,
            self.current_module_idx,
        )) |callable_inst_id| {
            try self.bindExactCallableLocal(target, callable_inst_id);
        } else {
            std.debug.panic(
                "statement-only MIR invariant violated: function-valued tuple access expr {d} lacked an exact callable specialization",
                .{@intFromEnum(expr_idx)},
            );
        }
    }
    const field_stmt = try self.lowerRefInto(target, .{ .field = .{
        .source = tuple_local,
        .field_idx = tuple_access.elem_index,
        .ownership = .move,
    } }, next);

    return self.lowerCirExprInto(tuple_access.tuple, tuple_local, field_stmt);
}

fn refreshCallTargetMonotypeFromCallee(
    self: *Self,
    target: MIR.LocalId,
    callee_monotype: Monotype.Idx,
) Allocator.Error!void {
    const callee = self.store.monotype_store.getMonotype(callee_monotype);
    const ret_monotype = switch (callee) {
        .func => |func| func.ret,
        else => std.debug.panic(
            "statement-only MIR invariant violated: call callee local monotype was {s}, not func",
            .{@tagName(callee)},
        ),
    };

    try self.refreshCallTargetMonotype(target, ret_monotype);
}

fn refreshCallTargetMonotype(
    self: *Self,
    target: MIR.LocalId,
    ret_monotype: Monotype.Idx,
) Allocator.Error!void {
    self.store.getLocalPtr(target).monotype = ret_monotype;
}

fn refreshLocalMonotypeFromCallableInst(
    self: *Self,
    local_id: MIR.LocalId,
    callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!void {
    const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callable_inst_id);
    self.store.getLocalPtr(local_id).monotype = try self.importMonotypeFromStore(
        &self.callable_pipeline.context_mono.monotype_store,
        callable_inst.fn_monotype,
        callable_inst.fn_monotype_module_idx,
        self.current_module_idx,
    );
}

fn bindExactLambdaLocal(
    self: *Self,
    local_id: MIR.LocalId,
    lambda_id: MIR.LambdaId,
) Allocator.Error!void {
    const lambda = self.store.getLambdaAnyState(lambda_id);
    self.store.getLocalPtr(local_id).monotype = lambda.fn_monotype;
    self.store.getLocalPtr(local_id).exact_callable = .{
        .lambda = lambda_id,
        .requires_hidden_capture = lambda.captures_param != null,
    };
}

fn bindExactCallableLocal(
    self: *Self,
    local_id: MIR.LocalId,
    callable_inst_id: Pipeline.CallableInstId,
) Allocator.Error!void {
    const lambda_id = try self.lowerResolvedCallableInstLambda(callable_inst_id);
    try self.refreshLocalMonotypeFromCallableInst(local_id, callable_inst_id);
    self.store.getLocalPtr(local_id).exact_callable = .{
        .lambda = lambda_id,
        .requires_hidden_capture = self.store.getLambdaAnyState(lambda_id).captures_param != null,
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
    module_env: *const ModuleEnv,
    list_pat: anytype,
    source_local: MIR.LocalId,
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
                    module_env,
                    rest_pattern_idx,
                    source_local,
                    body,
                    on_fail,
                );
            } else {
                const rest_local = try self.freshSyntheticLocal(source_mono, false);
                const rest_start_local = try self.freshSyntheticLocal(u64_mono, false);
                const rest_len_local = try self.freshSyntheticLocal(u64_mono, false);

                body = try self.lowerPatternMatchLocalInto(
                    module_env,
                    rest_pattern_idx,
                    rest_local,
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
        const elem_local = try self.freshSyntheticLocal(list_data.elem, false);
        const index_local = try self.freshSyntheticLocal(u64_mono, false);
        const offset_local = try self.freshSyntheticLocal(u64_mono, false);
        const pattern_idx = elem_patterns[prefix_len + suffix_i];
        const offset_from_end: u64 = @intCast(suffix_len - suffix_i);

        body = try self.lowerPatternMatchLocalInto(
            module_env,
            pattern_idx,
            elem_local,
            body,
            on_fail,
        );
        body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
            .target = elem_local,
            .op = .list_get_unsafe,
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
        const elem_local = try self.freshSyntheticLocal(list_data.elem, false);
        const index_local = try self.freshSyntheticLocal(u64_mono, false);
        const pattern_idx = elem_patterns[prefix_i];

        body = try self.lowerPatternMatchLocalInto(
            module_env,
            pattern_idx,
            elem_local,
            body,
            on_fail,
        );
        body = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
            .target = elem_local,
            .op = .list_get_unsafe,
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

fn lowerPatternBindingInto(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    mark_reassignable: bool,
    rhs_pattern_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const continuation_scope = self.current_pattern_scope;
    var bindings = std.ArrayList(PatternBinding).empty;
    defer bindings.deinit(self.allocator);
    try self.collectPatternBindings(module_env, pattern_idx, &bindings);
    const excluded_patterns = try self.allocator.alloc(CIR.Pattern.Idx, bindings.items.len);
    defer self.allocator.free(excluded_patterns);
    for (bindings.items, 0..) |binding, i| {
        excluded_patterns[i] = binding.pattern_idx;
    }

    if (builtin.mode == .Debug) {
        if (self.current_lambda_entry_bound_locals) |entry_bound_locals| {
            if (self.lookupExistingPatternLocalInScope(
                self.current_module_idx,
                self.current_pattern_scope,
                pattern_idx,
            )) |existing_local| {
                if (entry_bound_locals.contains(existing_local)) {
                    std.debug.panic(
                        "statement-only MIR invariant violated: block binding pattern {d} in scope {d} collided with entry-bound local {d} (expr={d}, expr_tag={s}, callable_inst={d})",
                        .{
                            @intFromEnum(pattern_idx),
                            self.current_pattern_scope,
                            @intFromEnum(existing_local),
                            @intFromEnum(expr_idx),
                            @tagName(module_env.store.getExpr(expr_idx)),
                            @intFromEnum(self.current_callable_inst_context),
                        },
                    );
                }
            }
        }
    }
    const source_mono = try self.resolveMonotype(expr_idx);
    if (self.store.monotype_store.getMonotype(source_mono) == .func and
        exprIsDirectCallableDefinition(module_env, expr_idx))
    {
        try self.bindPatternMonotypes(module_env, pattern_idx, source_mono);
        if (mark_reassignable) {
            self.setPatternLocalsReassignable(module_env, pattern_idx, true);
        }
        try self.markSkippedCallableBackedBindingPatterns(module_env, pattern_idx);
        self.current_pattern_scope = rhs_pattern_scope;
        return next;
    }

    const source_local = if (continuation_scope == rhs_pattern_scope)
        if (self.directPatternBindingTargetLocal(module_env, pattern_idx)) |existing_local|
            existing_local
        else
            try self.freshSyntheticLocal(source_mono, false)
    else
        try self.freshSyntheticLocal(source_mono, false);
    try self.bindPatternMonotypes(module_env, pattern_idx, source_mono);
    if (mark_reassignable) {
        self.setPatternLocalsReassignable(module_env, pattern_idx, true);
    }
    if (continuation_scope == rhs_pattern_scope) {
        const bound = try self.lowerPatternBindingLocalInto(module_env, pattern_idx, source_local, next);
        self.current_pattern_scope = rhs_pattern_scope;
        return self.lowerCirExprInto(expr_idx, source_local, bound);
    }

    const joined_source_local = try self.freshSyntheticLocal(source_mono, false);
    const after_expr_join_id = self.freshJoinPointId();
    const after_expr_jump = try self.store.reserveCFStmt(self.allocator);
    self.current_pattern_scope = rhs_pattern_scope;
    const lowered = try self.lowerExprWithExitScope(expr_idx, source_local, after_expr_jump);
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

    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = continuation_scope;
    const bound = try self.lowerPatternBindingLocalInto(module_env, pattern_idx, joined_source_local, next);
    self.current_pattern_scope = saved_scope;
    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = after_expr_join_id,
        .params = try self.buildJoinParamSpanWithValueExcluding(
            joined_source_local,
            continuation_scope,
            excluded_patterns,
        ),
        .body = bound,
        .remainder = lowered.entry,
    } });
}

fn lowerExprIntoContinuationScope(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    expr_scope: u64,
    continuation_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const saved_scope = self.current_pattern_scope;
    defer self.current_pattern_scope = saved_scope;

    self.current_pattern_scope = expr_scope;
    const after_expr_join_id = self.freshJoinPointId();
    const after_expr_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered = try self.lowerExprWithExitScope(expr_idx, target, after_expr_jump);
    try self.store.finalizeCFStmt(after_expr_jump, .{ .jump = .{
        .id = after_expr_join_id,
        .args = try self.buildCarriedJumpArgsExcluding(lowered.exit_scope, continuation_scope, excluded_patterns),
    } });

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = after_expr_join_id,
        .params = try self.buildJoinParamSpanExcluding(continuation_scope, excluded_patterns),
        .body = next,
        .remainder = lowered.entry,
    } });
}

fn lowerExprIntoContinuationScopeWithValue(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    joined_target: MIR.LocalId,
    expr_scope: u64,
    continuation_scope: u64,
    excluded_patterns: []const CIR.Pattern.Idx,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const saved_scope = self.current_pattern_scope;
    defer self.current_pattern_scope = saved_scope;

    self.current_pattern_scope = expr_scope;
    const after_expr_join_id = self.freshJoinPointId();
    const after_expr_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered = try self.lowerExprWithExitScope(expr_idx, target, after_expr_jump);
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

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = after_expr_join_id,
        .params = try self.buildJoinParamSpanWithValueExcluding(
            joined_target,
            continuation_scope,
            excluded_patterns,
        ),
        .body = next,
        .remainder = lowered.entry,
    } });
}

fn markDirectCallableBackedBindingIfNeeded(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    source_mono_opt: ?Monotype.Idx,
) Allocator.Error!void {
    if (!exprIsDirectCallableDefinition(module_env, expr_idx)) return;

    if (builtin.mode == .Debug) {
        if (source_mono_opt) |source_mono| {
            if (self.store.monotype_store.getMonotype(source_mono) != .func) {
                std.debug.panic(
                    "statement-only MIR invariant violated: direct callable binding expr {d} for pattern {d} resolved to non-func monotype {d} ({s})",
                    .{
                        @intFromEnum(expr_idx),
                        @intFromEnum(pattern_idx),
                        @intFromEnum(source_mono),
                        @tagName(self.store.monotype_store.getMonotype(source_mono)),
                    },
                );
            }
        }
    }

    try self.markSkippedCallableBackedBindingPatterns(module_env, pattern_idx);
}

fn exprIsDirectCallableDefinition(
    module_env: *const ModuleEnv,
    expr_idx: CIR.Expr.Idx,
) bool {
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_lambda, .e_closure, .e_hosted_lambda => true,
        else => false,
    };
}

fn directPatternBindingTargetLocal(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign, .as => self.lookupExistingPatternLocal(pattern_idx),
        .nominal => |nom| self.directPatternBindingTargetLocal(module_env, nom.backing_pattern),
        .nominal_external => |nom| self.directPatternBindingTargetLocal(module_env, nom.backing_pattern),
        else => null,
    };
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
                const payload_local = try self.freshSyntheticLocal(payload_monos[i], false);
                try self.seedExactCallablePatternLocal(payload_patterns[i], payload_local);
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
                    .tag_discriminant = @intCast(tag_discriminant),
                    .ownership = .move,
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
                        const field_local = try self.freshSyntheticLocal(mono_fields[field_idx].type_idx, false);
                        try self.seedExactCallablePatternLocal(destruct.kind.toPatternIdx(), field_local);
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
                            .ownership = .move,
                        } }, body);
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
                            module_env,
                            destruct_pattern_idx,
                            field_local,
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
                const elem_local = try self.freshSyntheticLocal(elem_monos[i], false);
                try self.seedExactCallablePatternLocal(elem_patterns[i], elem_local);
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
                    .ownership = .move,
                } }, body);
            }
            break :blk body;
        },
        .list => |list_pat| self.lowerListPatternMatchLocalInto(
            module_env,
            list_pat,
            source_local,
            on_match,
            on_fail,
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
        .args = try self.buildCarriedJumpArgs(
            self.current_pattern_scope,
            loop_ctx.exit_scope,
        ),
    } });
}

fn lowerWhileStmtInto(
    self: *Self,
    while_stmt: std.meta.TagPayload(CIR.Statement, .s_while),
    before_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const after_scope = self.current_pattern_scope;
    const head_scope = self.freshChildPatternScope(before_scope);
    try self.predeclareVisibleReassignableBindingsInScope(before_scope, head_scope);

    const loop_exit_id = self.freshJoinPointId();
    const loop_head_id = self.freshJoinPointId();

    try self.active_loops.append(self.allocator, .{
        .exit_id = loop_exit_id,
        .exit_scope = after_scope,
    });
    defer _ = self.active_loops.pop();

    const cond_mono = try self.resolveMonotype(while_stmt.cond);
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);

    self.current_pattern_scope = head_scope;
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
    const lowered_cond = try self.lowerExprWithExitScope(while_stmt.cond, cond_local, discrim_stmt);

    self.current_pattern_scope = lowered_cond.exit_scope;
    const continue_jump = try self.store.reserveCFStmt(self.allocator);
    const body_value = try self.freshSyntheticLocal(try self.resolveMonotype(while_stmt.body), false);
    const lowered_body = try self.lowerExprWithExitScope(while_stmt.body, body_value, continue_jump);
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

    const head_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_head_id,
        .params = try self.buildJoinParamSpan(head_scope),
        .body = lowered_cond.entry,
        .remainder = initial_jump,
    } });

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_exit_id,
        .params = try self.buildJoinParamSpan(after_scope),
        .body = next,
        .remainder = head_join,
    } });
}

fn lowerForStmtInto(
    self: *Self,
    module_env: *const ModuleEnv,
    for_stmt: std.meta.TagPayload(CIR.Statement, .s_for),
    before_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const after_scope = self.current_pattern_scope;
    const head_scope = self.freshChildPatternScope(before_scope);
    try self.predeclareVisibleReassignableBindingsInScope(before_scope, head_scope);

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
    const item_local = try self.freshSyntheticLocal(item_mono, false);
    const body_value = try self.freshSyntheticLocal(try self.resolveMonotype(for_stmt.body), false);
    const loop_back = try self.store.reserveCFStmt(self.allocator);
    const increment_stmt = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = next_index_local,
        .op = .num_plus,
        .args = try self.store.addLocalSpan(self.allocator, &.{ head_index_local, one_local }),
        .next = loop_back,
    } });
    self.current_pattern_scope = head_scope;
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
    try self.predeclarePatternLocals(module_env, for_stmt.patt);
    try self.markPatternLocalsPreludeBound(module_env, for_stmt.patt);
    const lowered_body = try self.lowerExprWithExitScope(for_stmt.body, body_value, increment_stmt);
    try self.store.finalizeCFStmt(loop_back, .{ .jump = .{
        .id = loop_head_id,
        .args = try self.buildCarriedJumpArgsWithExtraLocals(
            lowered_body.exit_scope,
            head_scope,
            &.{ head_list_local, head_len_local, next_index_local },
        ),
    } });
    const body_stmt = lowered_body.entry;
    const bind_item = try self.lowerPatternBindingLocalInto(module_env, for_stmt.patt, item_local, body_stmt);
    const get_item = try self.store.addCFStmt(self.allocator, .{ .assign_low_level = .{
        .target = item_local,
        .op = .list_get_unsafe,
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
    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = before_scope;
    defer self.current_pattern_scope = saved_scope;
    const init_list = try self.lowerCirExprInto(for_stmt.expr, init_list_local, init_len);

    const head_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_head_id,
        .params = try self.buildJoinParamSpanWithExtraLocals(
            head_scope,
            &.{ head_list_local, head_len_local, head_index_local },
        ),
        .body = cond_stmt,
        .remainder = init_list,
    } });

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = loop_exit_id,
        .params = try self.buildJoinParamSpan(after_scope),
        .body = next,
        .remainder = head_join,
    } });
}

fn lowerBlockStmtInto(
    self: *Self,
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
    before_pattern_scope: u64,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    const stmt = module_env.store.getStatement(stmt_idx);

    return switch (stmt) {
        .s_decl => |decl| self.lowerPatternBindingInto(
            module_env,
            decl.pattern,
            decl.expr,
            false,
            before_pattern_scope,
            next,
        ),
        .s_var => |var_decl| self.lowerPatternBindingInto(
            module_env,
            var_decl.pattern_idx,
            var_decl.expr,
            true,
            before_pattern_scope,
            next,
        ),
        .s_reassign => |reassign| self.lowerPatternBindingInto(
            module_env,
            reassign.pattern_idx,
            reassign.expr,
            false,
            before_pattern_scope,
            next,
        ),
        .s_expr => |expr_stmt| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(expr_stmt.expr), false);
            break :blk try self.lowerExprIntoContinuationScope(
                expr_stmt.expr,
                value_local,
                before_pattern_scope,
                self.current_pattern_scope,
                &.{},
                next,
            );
        },
        .s_dbg => |dbg_stmt| blk: {
            const value_mono = try self.resolveMonotype(dbg_stmt.expr);
            const value_local = try self.freshSyntheticLocal(value_mono, false);
            const joined_value_local = try self.freshSyntheticLocal(value_mono, false);
            const message_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.str), false);
            const debug_stmt = try self.store.addCFStmt(self.allocator, .{ .debug = .{
                .value = message_local,
                .next = next,
            } });
            const inspect_stmt = try self.lowerStrInspectLocalInto(joined_value_local, value_mono, message_local, debug_stmt);
            break :blk try self.lowerExprIntoContinuationScopeWithValue(
                dbg_stmt.expr,
                value_local,
                joined_value_local,
                before_pattern_scope,
                self.current_pattern_scope,
                &.{},
                inspect_stmt,
            );
        },
        .s_expect => |expect_stmt| blk: {
            const cond_local = try self.freshSyntheticLocal(try self.resolveMonotype(expect_stmt.body), false);
            const joined_cond_local = try self.freshSyntheticLocal(self.store.getLocal(cond_local).monotype, false);
            const mir_expect = try self.store.addCFStmt(self.allocator, .{ .expect = .{
                .condition = joined_cond_local,
                .next = next,
            } });
            break :blk try self.lowerExprIntoContinuationScopeWithValue(
                expect_stmt.body,
                cond_local,
                joined_cond_local,
                before_pattern_scope,
                self.current_pattern_scope,
                &.{},
                mir_expect,
            );
        },
        .s_crash => |crash_stmt| blk: {
            const mir_str = try self.copyStringToMir(module_env, crash_stmt.msg);
            break :blk self.store.addCFStmt(self.allocator, .{ .crash = mir_str });
        },
        .s_return => |return_stmt| blk: {
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(return_stmt.expr), false);
            const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = value_local } });
            const saved_scope = self.current_pattern_scope;
            self.current_pattern_scope = before_pattern_scope;
            defer self.current_pattern_scope = saved_scope;
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
        .s_for => |s_for| self.lowerForStmtInto(module_env, s_for, before_pattern_scope, next),
        .s_while => |s_while| self.lowerWhileStmtInto(s_while, before_pattern_scope, next),
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
                            @intFromEnum(self.current_callable_inst_context),
                            self.currentRootExprRawForDebug(),
                        },
                    );
                }
            }
            try self.predeclarePatternLocals(module_env, decl.pattern);
            const source_mono_opt = try self.explicitExprMonotype(decl.expr);
            if (source_mono_opt) |source_mono| {
                if (builtin.mode == .Debug) {
                    try self.debugAssertNoUnitPrimPatternBinding(module_env, decl.pattern, decl.expr, source_mono, stmt_idx, "s_decl");
                }
                try self.bindPatternMonotypes(module_env, decl.pattern, source_mono);
            }
            try self.markDirectCallableBackedBindingIfNeeded(
                module_env,
                decl.pattern,
                decl.expr,
                source_mono_opt,
            );
        },
        .s_var => |var_decl| {
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
                            @intFromEnum(self.current_callable_inst_context),
                            self.currentRootExprRawForDebug(),
                        },
                    );
                }
            }
            try self.predeclarePatternLocals(module_env, var_decl.pattern_idx);
            self.setPatternLocalsReassignable(module_env, var_decl.pattern_idx, true);
            const source_mono_opt = try self.explicitExprMonotype(var_decl.expr);
            if (source_mono_opt) |source_mono| {
                if (builtin.mode == .Debug) {
                    try self.debugAssertNoUnitPrimPatternBinding(module_env, var_decl.pattern_idx, var_decl.expr, source_mono, stmt_idx, "s_var");
                }
                try self.bindPatternMonotypes(module_env, var_decl.pattern_idx, source_mono);
            }
            try self.markDirectCallableBackedBindingIfNeeded(
                module_env,
                var_decl.pattern_idx,
                var_decl.expr,
                source_mono_opt,
            );
        },
        .s_reassign => |reassign| {
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
                            @intFromEnum(self.current_callable_inst_context),
                            self.currentRootExprRawForDebug(),
                        },
                    );
                }
            }
            try self.predeclarePatternLocals(module_env, reassign.pattern_idx);
            self.setPatternLocalsReassignable(module_env, reassign.pattern_idx, true);
            const source_mono_opt = try self.explicitExprMonotype(reassign.expr);
            if (source_mono_opt) |source_mono| {
                if (builtin.mode == .Debug) {
                    try self.debugAssertNoUnitPrimPatternBinding(module_env, reassign.pattern_idx, reassign.expr, source_mono, stmt_idx, "s_reassign");
                }
                try self.bindPatternMonotypes(module_env, reassign.pattern_idx, source_mono);
            }
            try self.markDirectCallableBackedBindingIfNeeded(
                module_env,
                reassign.pattern_idx,
                reassign.expr,
                source_mono_opt,
            );
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
            var bindings = std.ArrayList(PatternBinding).empty;
            defer bindings.deinit(self.allocator);
            try self.collectPatternBindings(module_env, decl.pattern, &bindings);

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
            var bindings = std.ArrayList(PatternBinding).empty;
            defer bindings.deinit(self.allocator);
            try self.collectPatternBindings(module_env, var_decl.pattern_idx, &bindings);

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
            var bindings = std.ArrayList(PatternBinding).empty;
            defer bindings.deinit(self.allocator);
            try self.collectPatternBindings(module_env, reassign.pattern_idx, &bindings);

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

fn debugAssertNoUnitPrimPatternBinding(
    self: *Self,
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
        .e_call => |call_expr| self.getCallLowLevelOp(module_env, call_expr.func),
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
    const call_func_source_expr: u32 = if (call_func_pattern != std.math.maxInt(u32))
        if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, @enumFromInt(call_func_pattern))) |source|
            @intFromEnum(source.expr_idx)
        else
            std.math.maxInt(u32)
    else
        std.math.maxInt(u32);
    const has_pipelined_expr_mono = self.lookupPipelinedExprMonotype(expr_idx) != null;
    const maybe_callsite_inst = switch (expr) {
        .e_call => self.lookupPipelinedCallSiteCallableInst(expr_idx),
        else => null,
    };
    const callsite_template_expr: u32 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callsite_inst);
        const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
        break :blk @intFromEnum(template.cir_expr);
    } else std.math.maxInt(u32);
    const callsite_template_body_expr: u32 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callsite_inst);
        const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
        const template_env = self.all_module_envs[template.module_idx];
        break :blk switch (template_env.store.getExpr(template.cir_expr)) {
            .e_lambda => |lambda_expr| @intFromEnum(lambda_expr.body),
            .e_hosted_lambda => |hosted_expr| @intFromEnum(hosted_expr.body),
            .e_closure => |closure_expr| blk2: {
                const lambda_expr = template_env.store.getExpr(closure_expr.lambda_idx);
                break :blk2 switch (lambda_expr) {
                    .e_lambda => |lambda| @intFromEnum(lambda.body),
                    else => std.math.maxInt(u32),
                };
            },
            else => std.math.maxInt(u32),
        };
    } else std.math.maxInt(u32);
    const callsite_template_body_tag: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callsite_inst);
        const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
        const template_env = self.all_module_envs[template.module_idx];
        break :blk switch (template_env.store.getExpr(template.cir_expr)) {
            .e_lambda => |lambda_expr| @tagName(template_env.store.getExpr(lambda_expr.body)),
            .e_hosted_lambda => |hosted_expr| @tagName(template_env.store.getExpr(hosted_expr.body)),
            .e_closure => |closure_expr| blk2: {
                const lambda_expr = template_env.store.getExpr(closure_expr.lambda_idx);
                break :blk2 switch (lambda_expr) {
                    .e_lambda => |lambda| @tagName(template_env.store.getExpr(lambda.body)),
                    else => "unknown",
                };
            },
            else => "none",
        };
    } else "none";
    const callsite_template_kind: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callsite_inst);
        const template = self.callable_pipeline.lambda_solved.getCallableTemplate(callable_inst.template);
        break :blk @tagName(template.kind);
    } else "none";
    const callsite_fn_monotype: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callsite_inst);
        break :blk @tagName(self.callable_pipeline.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype));
    } else "none";
    const callsite_ret_monotype: []const u8 = if (maybe_callsite_inst) |callsite_inst| blk: {
        const callable_inst = self.callable_pipeline.lambda_specialize.getCallableInst(callsite_inst);
        const fn_mono = self.callable_pipeline.context_mono.monotype_store.getMonotype(callable_inst.fn_monotype);
        break :blk switch (fn_mono) {
            .func => |func| @tagName(self.callable_pipeline.context_mono.monotype_store.getMonotype(func.ret)),
            else => "non-func",
        };
    } else "none";

    std.debug.panic(
        "predeclareTrivialBlockStmtPatterns({s}): unit source monotype would bind builtin prim pattern module={d} stmt={d} pattern={d} expr={d} resolved_root={d} expr_tag={s} call_func_expr={d} call_func_tag={s} call_func_pattern={d} call_func_source_expr={d} low_level={s} has_pipelined_expr_mono={} callsite_inst={d} callsite_template_expr={d} callsite_template_body_expr={d} callsite_template_body_tag={s} callsite_template_kind={s} callsite_fn_monotype={s} callsite_ret_monotype={s} callable_inst={d} root_source_expr={d}",
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
            has_pipelined_expr_mono,
            if (maybe_callsite_inst) |callsite_inst| @intFromEnum(callsite_inst) else std.math.maxInt(u32),
            callsite_template_expr,
            callsite_template_body_expr,
            callsite_template_body_tag,
            callsite_template_kind,
            callsite_fn_monotype,
            callsite_ret_monotype,
            @intFromEnum(self.current_callable_inst_context),
            self.currentRootExprRawForDebug(),
        },
    );
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

fn markPatternLocalsPreludeBound(
    self: *Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    const prelude_bound_locals = self.current_prelude_bound_locals orelse return;

    var bindings = std.ArrayList(PatternBinding).empty;
    defer bindings.deinit(self.allocator);
    try self.collectPatternBindings(module_env, pattern_idx, &bindings);
    for (bindings.items) |binding| {
        const local = self.lookupExistingPatternLocalInScope(
            self.current_module_idx,
            self.current_pattern_scope,
            binding.pattern_idx,
        ) orelse std.debug.panic(
            "statement-only MIR invariant violated: prelude-bound marking missing predeclared pattern local {d} in scope {d}",
            .{ @intFromEnum(binding.pattern_idx), self.current_pattern_scope },
        );
        try prelude_bound_locals.add(local);
    }
}

fn bindRepresentativePatternLocalsToAlternative(
    self: *Self,
    module_env: *const ModuleEnv,
    representative_pattern_idx: CIR.Pattern.Idx,
    alternative_pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!void {
    if (representative_pattern_idx == alternative_pattern_idx) return;

    var representative_bindings = std.ArrayList(PatternBinding).empty;
    defer representative_bindings.deinit(self.allocator);

    var alternative_bindings = std.ArrayList(PatternBinding).empty;
    defer alternative_bindings.deinit(self.allocator);

    try self.collectPatternBindings(module_env, representative_pattern_idx, &representative_bindings);
    try self.collectPatternBindings(module_env, alternative_pattern_idx, &alternative_bindings);

    for (representative_bindings.items) |rep_binding| {
        for (alternative_bindings.items) |alt_binding| {
            if (!rep_binding.ident.eql(alt_binding.ident)) continue;

            const alt_local = self.lookupExistingPatternLocalInScope(
                self.current_module_idx,
                self.current_pattern_scope,
                alt_binding.pattern_idx,
            ) orelse try self.patternToLocal(alt_binding.pattern_idx);
            try self.bindPatternLocalInCurrentScope(rep_binding.pattern_idx, alt_local);

            const alt_key = patternScopeKey(self.current_module_idx, self.current_pattern_scope, alt_binding.pattern_idx);
            if (self.explicit_pattern_local_monotypes.get(alt_key)) |monotype| {
                const rep_key = patternScopeKey(self.current_module_idx, self.current_pattern_scope, rep_binding.pattern_idx);
                try self.explicit_pattern_local_monotypes.put(rep_key, monotype);
            }

            break;
        }
    }
}

fn findVisibleBindingLocal(
    bindings: []const VisiblePatternBinding,
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
    try self.mergeExactCallableJoinIncoming(dest_value_local, value_local);
    var source_i: usize = 0;
    for (dest_bindings, 0..) |dest_binding, i| {
        args[i + 1] = findVisibleBindingLocal(
            source_bindings,
            &source_i,
            dest_binding.pattern_idx,
            "statement-only MIR join invariant violated: source scope is missing a carried binding required by destination scope",
        );
        try self.mergeExactCallableJoinIncoming(dest_binding.local, args[i + 1]);
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
    try self.mergeExactCallableJoinIncoming(dest_value_local, value_local);
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
        try self.mergeExactCallableJoinIncoming(dest_binding.local, args[arg_i]);
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
        try self.mergeExactCallableJoinIncoming(dest_binding.local, args[i]);
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
        try self.mergeExactCallableJoinIncoming(dest_binding.local, args[arg_i]);
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
        try self.mergeExactCallableJoinIncoming(dest_binding.local, args[i]);
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

fn lowerBoolBranchChainIntoWithExitScope(
    self: *Self,
    branches: []const CIR.Expr.IfBranch.Idx,
    final_else: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];

    if (branches.len == 0) {
        return self.lowerExprWithExitScope(final_else, target, next);
    }

    const branch = module_env.store.getIfBranch(branches[0]);
    if (self.cirExprKnownBoolValue(self.current_module_idx, branch.cond)) |known_bool| {
        return if (known_bool)
            self.lowerExprWithExitScope(branch.body, target, next)
        else
            self.lowerBoolBranchChainIntoWithExitScope(branches[1..], final_else, target, next);
    }

    const cond_mono = try self.resolveMonotype(branch.cond);
    const result_mono = self.store.getLocal(target).monotype;
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const discrim_local = try self.freshSyntheticLocal(self.store.monotype_store.primIdx(.u64), false);
    const switch_placeholder = try self.store.reserveCFStmt(self.allocator);
    const discrim_stmt = try self.lowerRefInto(
        discrim_local,
        .{ .discriminant = .{ .source = cond_local } },
        switch_placeholder,
    );
    const lowered_cond = try self.lowerExprWithExitScope(branch.cond, cond_local, discrim_stmt);
    const join_scope = self.freshChildPatternScope(lowered_cond.exit_scope);
    try self.predeclareVisibleReassignableBindingsInScope(lowered_cond.exit_scope, join_scope);
    const join_id = self.freshJoinPointId();
    const joined_value_local = try self.freshSyntheticLocal(result_mono, false);

    self.current_pattern_scope = lowered_cond.exit_scope;
    const else_value = try self.freshSyntheticLocal(result_mono, false);
    const else_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered_else = try self.lowerBoolBranchChainIntoWithExitScope(branches[1..], final_else, else_value, else_jump);
    try self.store.finalizeCFStmt(else_jump, .{ .jump = .{
        .id = join_id,
        .args = try self.buildCarriedJumpArgsWithValue(
            lowered_else.exit_scope,
            join_scope,
            joined_value_local,
            else_value,
        ),
    } });

    self.current_pattern_scope = lowered_cond.exit_scope;
    const then_value = try self.freshSyntheticLocal(result_mono, false);
    const then_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered_then = try self.lowerExprWithExitScope(branch.body, then_value, then_jump);
    try self.store.finalizeCFStmt(then_jump, .{ .jump = .{
        .id = join_id,
        .args = try self.buildCarriedJumpArgsWithValue(
            lowered_then.exit_scope,
            join_scope,
            joined_value_local,
            then_value,
        ),
    } });

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

    const join_body = try self.lowerLocalAliasInto(target, joined_value_local, next);
    const entry = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = join_id,
        .params = try self.buildJoinParamSpanWithValue(joined_value_local, join_scope),
        .body = join_body,
        .remainder = lowered_cond.entry,
    } });

    return .{
        .entry = entry,
        .exit_scope = join_scope,
    };
}

fn lowerExprWithExitScope(
    self: *Self,
    expr_idx: CIR.Expr.Idx,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];
    const start_scope = self.current_pattern_scope;
    const lowered = switch (module_env.store.getExpr(expr_idx)) {
        .e_block => |block| try self.lowerBlockExprIntoWithExitScope(block, target, next),
        .e_if => |if_expr| try self.lowerBoolBranchChainIntoWithExitScope(
            module_env.store.sliceIfBranches(if_expr.branches),
            if_expr.final_else,
            target,
            next,
        ),
        .e_match => |match_expr| try self.lowerMatchIntoWithExitScope(module_env, match_expr, target, next),
        else => LoweredExprWithScope{
            .entry = try self.lowerCirExprInto(expr_idx, target, next),
            .exit_scope = self.current_pattern_scope,
        },
    };
    self.current_pattern_scope = start_scope;
    return lowered;
}

fn lowerBlockExprIntoWithExitScope(
    self: *Self,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];
    const stmts = module_env.store.sliceStatements(block.stmts);
    const saved_scope = self.current_pattern_scope;
    const scope_infos = try self.allocator.alloc(BlockStmtScopeInfo, stmts.len);
    defer self.allocator.free(scope_infos);

    var scope = self.current_pattern_scope;
    for (stmts, 0..) |stmt_idx, i| {
        scope_infos[i].before = scope;
        switch (module_env.store.getStatement(stmt_idx)) {
            .s_decl, .s_var, .s_reassign, .s_expr, .s_dbg, .s_expect, .s_for, .s_while => {
                scope = self.freshChildPatternScope(scope);
                scope_infos[i].after = scope;
            },
            else => {
                scope_infos[i].after = scope;
            },
        }

        self.current_pattern_scope = scope_infos[i].after;
        try self.predeclareReassignableBindingsForStmtScope(
            module_env,
            stmt_idx,
            scope_infos[i].before,
            scope_infos[i].after,
        );
        try self.predeclareTrivialBlockStmtPatterns(module_env, stmt_idx);
    }

    self.current_pattern_scope = scope;
    const lowered_final = try self.lowerExprWithExitScope(block.final_expr, target, next);
    var entry = lowered_final.entry;
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        self.current_pattern_scope = scope_infos[i].after;
        entry = try self.lowerBlockStmtInto(module_env, stmts[i], scope_infos[i].before, entry);
        self.current_pattern_scope = scope_infos[i].before;
    }
    self.current_pattern_scope = saved_scope;
    return .{
        .entry = entry,
        .exit_scope = lowered_final.exit_scope,
    };
}

fn lowerBlockIntoWithExitScope(
    self: *Self,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const module_env = self.all_module_envs[self.current_module_idx];
    const stmts = module_env.store.sliceStatements(block.stmts);
    const saved_scope = self.current_pattern_scope;
    const scope_infos = try self.allocator.alloc(BlockStmtScopeInfo, stmts.len);
    defer self.allocator.free(scope_infos);

    var scope = self.current_pattern_scope;
    for (stmts, 0..) |stmt_idx, i| {
        scope_infos[i].before = scope;
        switch (module_env.store.getStatement(stmt_idx)) {
            .s_decl, .s_var, .s_reassign, .s_expr, .s_dbg, .s_expect, .s_for, .s_while => {
                scope = self.freshChildPatternScope(scope);
                scope_infos[i].after = scope;
            },
            else => {
                scope_infos[i].after = scope;
            },
        }

        self.current_pattern_scope = scope_infos[i].after;
        try self.predeclareReassignableBindingsForStmtScope(
            module_env,
            stmt_idx,
            scope_infos[i].before,
            scope_infos[i].after,
        );
        try self.predeclareTrivialBlockStmtPatterns(module_env, stmt_idx);
    }

    const exit_scope = self.freshChildPatternScope(saved_scope);
    try self.predeclareVisibleReassignableBindingsInScope(saved_scope, exit_scope);
    const after_final_join_id = self.freshJoinPointId();
    const final_value_local = try self.freshSyntheticLocal(self.store.getLocal(target).monotype, false);
    const joined_final_local = try self.freshSyntheticLocal(self.store.getLocal(target).monotype, false);
    const after_final_jump = try self.store.reserveCFStmt(self.allocator);

    self.current_pattern_scope = scope;
    const lowered_final = try self.lowerExprWithExitScope(block.final_expr, final_value_local, after_final_jump);
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
    var entry = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = after_final_join_id,
        .params = try self.buildJoinParamSpanWithValue(joined_final_local, exit_scope),
        .body = after_final_join_body,
        .remainder = lowered_final.entry,
    } });
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        self.current_pattern_scope = scope_infos[i].after;
        entry = try self.lowerBlockStmtInto(module_env, stmts[i], scope_infos[i].before, entry);
        self.current_pattern_scope = scope_infos[i].before;
    }
    self.current_pattern_scope = saved_scope;
    return .{
        .entry = entry,
        .exit_scope = exit_scope,
    };
}

fn lowerMatchIntoWithExitScope(
    self: *Self,
    module_env: *const ModuleEnv,
    match_expr: CIR.Expr.Match,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!LoweredExprWithScope {
    const cond_mono = try self.resolveMonotype(match_expr.cond);
    const cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const dispatch_scope = self.freshChildPatternScope(self.current_pattern_scope);
    const dispatch_join_cond_local = try self.freshSyntheticLocal(cond_mono, false);
    const dispatch_join_id = self.freshJoinPointId();
    const dispatch_jump = try self.store.reserveCFStmt(self.allocator);
    const lowered_cond = try self.lowerExprWithExitScope(match_expr.cond, cond_local, dispatch_jump);
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

    const join_scope = self.freshChildPatternScope(dispatch_scope);
    try self.predeclareVisibleReassignableBindingsInScope(dispatch_scope, join_scope);
    const join_id = self.freshJoinPointId();
    const joined_result_local = try self.freshSyntheticLocal(self.store.getLocal(target).monotype, false);

    const branch_indices = module_env.store.sliceMatchBranches(match_expr.branches);
    const match_failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });
    const branch_degenerate_failure = try self.store.addCFStmt(self.allocator, .{ .runtime_error = .type_error });

    const entry = try self.lowerMatchBranchChainInto(
        module_env,
        branch_indices,
        dispatch_scope,
        dispatch_join_cond_local,
        cond_mono,
        joined_result_local,
        join_id,
        join_scope,
        match_failure,
        branch_degenerate_failure,
    );

    const dispatch_join = try self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = dispatch_join_id,
        .params = try self.buildJoinParamSpanWithValue(dispatch_join_cond_local, dispatch_scope),
        .body = entry,
        .remainder = lowered_cond.entry,
    } });

    const result_join_body = try self.lowerLocalAliasInto(target, joined_result_local, next);
    return .{
        .entry = try self.store.addCFStmt(self.allocator, .{ .join = .{
            .id = join_id,
            .params = try self.buildJoinParamSpanWithValue(joined_result_local, join_scope),
            .body = result_join_body,
            .remainder = dispatch_join,
        } }),
        .exit_scope = join_scope,
    };
}

fn lowerMatchBranchChainInto(
    self: *Self,
    module_env: *const ModuleEnv,
    branch_indices: []const CIR.Expr.Match.Branch.Idx,
    input_scope: u64,
    cond_local: MIR.LocalId,
    cond_mono: Monotype.Idx,
    result_local: MIR.LocalId,
    result_join_id: MIR.JoinPointId,
    result_join_scope: u64,
    on_exhausted: MIR.CFStmtId,
    branch_degenerate_failure: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    if (branch_indices.len == 0) return on_exhausted;

    const cir_branch = module_env.store.getMatchBranch(branch_indices[0]);
    const rest_scope = self.freshChildPatternScope(input_scope);
    try self.predeclareVisibleReassignableBindingsInScope(input_scope, rest_scope);
    const rest_entry = try self.lowerMatchBranchChainInto(
        module_env,
        branch_indices[1..],
        rest_scope,
        cond_local,
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
    if (branch_pattern_indices.len == 0) {
        const branch_scope = self.freshChildPatternScope(input_scope);
        try self.inheritVisibleReassignableBindingsInScope(input_scope, branch_scope);
        const saved_scope = self.current_pattern_scope;
        self.current_pattern_scope = branch_scope;
        defer self.current_pattern_scope = saved_scope;

        const branch_value_local = try self.freshSyntheticLocal(result_mono, false);
        const branch_jump = try self.store.reserveCFStmt(self.allocator);
        const lowered_branch_value = try self.lowerExprWithExitScope(cir_branch.value, branch_value_local, branch_jump);
        try self.store.finalizeCFStmt(branch_jump, .{ .jump = .{
            .id = result_join_id,
            .args = try self.buildCarriedJumpArgsWithValue(
                lowered_branch_value.exit_scope,
                result_join_scope,
                result_local,
                branch_value_local,
            ),
        } });

        branch_entry = if (cir_branch.guard) |guard_idx| blk: {
            const guard_mono = try self.resolveMonotype(guard_idx);
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
            const lowered_guard = try self.lowerExprWithExitScope(guard_idx, guard_local, guard_discrim_stmt);
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
            const alternative_scope = self.freshChildPatternScope(input_scope);
            try self.inheritVisibleReassignableBindingsInScope(input_scope, alternative_scope);
            branch_entry = blk: {
                const saved_scope = self.current_pattern_scope;
                self.current_pattern_scope = alternative_scope;
                defer self.current_pattern_scope = saved_scope;
                const saved_prelude_bound_locals = self.current_prelude_bound_locals;
                var branch_prelude_bound_locals = try LambdaEntryBindingState.cloneFrom(self.allocator, saved_prelude_bound_locals);
                defer branch_prelude_bound_locals.deinit();
                self.current_prelude_bound_locals = &branch_prelude_bound_locals;
                defer self.current_prelude_bound_locals = saved_prelude_bound_locals;

                try self.bindPatternMonotypes(module_env, branch_pattern.pattern, cond_mono);
                try self.predeclarePatternLocals(module_env, branch_pattern.pattern);
                try self.markPatternLocalsPreludeBound(module_env, branch_pattern.pattern);
                if (representative_pattern_idx) |representative_pattern| {
                    try self.bindRepresentativePatternLocalsToAlternative(
                        module_env,
                        representative_pattern,
                        branch_pattern.pattern,
                    );
                    try self.markPatternLocalsPreludeBound(module_env, representative_pattern);
                }

                const branch_value_local = try self.freshSyntheticLocal(result_mono, false);
                const branch_jump = try self.store.reserveCFStmt(self.allocator);
                const lowered_branch_value = try self.lowerExprWithExitScope(cir_branch.value, branch_value_local, branch_jump);
                try self.store.finalizeCFStmt(branch_jump, .{ .jump = .{
                    .id = result_join_id,
                    .args = try self.buildCarriedJumpArgsWithValue(
                        lowered_branch_value.exit_scope,
                        result_join_scope,
                        result_local,
                        branch_value_local,
                    ),
                } });

                const guarded_body = if (cir_branch.guard) |guard_idx| guard_blk: {
                    const guard_mono = try self.resolveMonotype(guard_idx);
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
                    const lowered_guard = try self.lowerExprWithExitScope(guard_idx, guard_local, guard_discrim_stmt);
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
                    break :guard_blk lowered_guard.entry;
                } else lowered_branch_value.entry;

                break :blk try self.lowerPatternMatchLocalInto(
                    module_env,
                    branch_pattern.pattern,
                    cond_local,
                    if (branch_pattern.degenerate) branch_degenerate_failure else guarded_body,
                    branch_entry,
                );
            };
        }
    }

    return self.store.addCFStmt(self.allocator, .{ .join = .{
        .id = rest_join_id,
        .params = rest_join_params,
        .body = rest_entry,
        .remainder = branch_entry,
    } });
}

fn lowerBlockInto(
    self: *Self,
    block: anytype,
    target: MIR.LocalId,
    next: MIR.CFStmtId,
) Allocator.Error!MIR.CFStmtId {
    return (try self.lowerBlockIntoWithExitScope(block, target, next)).entry;
}

fn findWrappingClosureExpr(
    _: *Self,
    module_env: *const ModuleEnv,
    lambda_expr_idx: CIR.Expr.Idx,
) ?CIR.Expr.Idx {
    var found: ?CIR.Expr.Idx = null;

    const node_count = module_env.store.nodes.len();
    var raw_idx: usize = 0;
    while (raw_idx < node_count) : (raw_idx += 1) {
        const node_idx: CIR.Expr.Idx = @enumFromInt(raw_idx);
        const node = module_env.store.nodes.get(@enumFromInt(raw_idx));
        if (node.tag != .expr_closure) continue;

        const expr = module_env.store.getExpr(node_idx);
        const closure = switch (expr) {
            .e_closure => |closure| closure,
            else => unreachable,
        };
        if (closure.lambda_idx != lambda_expr_idx) continue;

        if (found) |existing| {
            std.debug.panic(
                "statement-only MIR invariant violated: lambda expr {d} was wrapped by multiple closure exprs ({d}, {d})",
                .{
                    @intFromEnum(lambda_expr_idx),
                    @intFromEnum(existing),
                    @intFromEnum(node_idx),
                },
            );
        }

        found = node_idx;
    }

    return found;
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

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);

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
                const elem_local = try self.freshSyntheticLocal(try self.resolveMonotype(elem_idx), false);
                try self.scratch_local_ids.append(elem_local);
                lowered_next = try self.lowerCirExprInto(elem_idx, elem_local, lowered_next);
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
                        const ext_local = try self.freshSyntheticLocal(try self.resolveMonotype(ext_expr), false);
                        lowered_next = try self.lowerCirExprInto(ext_expr, ext_local, lowered_next);
                    }

                    const source_fields = module_env.store.sliceRecordFields(record.fields);
                    var i: usize = source_fields.len;
                    while (i > 0) {
                        i -= 1;
                        const field = module_env.store.getRecordField(source_fields[i]);
                        const field_local = try self.freshSyntheticLocal(try self.resolveMonotype(field.value), false);
                        lowered_next = try self.lowerCirExprInto(field.value, field_local, lowered_next);
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
                const ext_monotype = try self.resolveMonotype(ext_expr);
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
                ordered_locals[canonical_idx] = try self.freshSyntheticLocal(mono_fields[canonical_idx].type_idx, false);
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
                ordered_locals[i] = try self.freshSyntheticLocal(mono_field.type_idx, false);
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
                    lowered_next = try self.lowerRefInto(
                        ordered_locals[i].?,
                        .{ .field = .{
                            .source = extension_local,
                            .field_idx = ext_field_idx,
                            .ownership = .move,
                        } },
                        lowered_next,
                    );
                }

                lowered_next = try self.lowerCirExprInto(ext_expr_idx.?, extension_local, lowered_next);
            }

            const source_fields = module_env.store.sliceRecordFields(record.fields);
            var i: usize = source_fields.len;
            while (i > 0) {
                i -= 1;
                const field = module_env.store.getRecordField(source_fields[i]);
                const canonical_idx = self.recordFieldIndexByName(field.name, mono_fields);
                lowered_next = try self.lowerCirExprInto(field.value, ordered_locals[canonical_idx].?, lowered_next);
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
                const arg_local = try self.freshSyntheticLocal(try self.resolveMonotype(arg_idx), false);
                try self.scratch_local_ids.append(arg_local);
                lowered_next = try self.lowerCirExprInto(arg_idx, arg_local, lowered_next);
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
            if (self.findWrappingClosureExpr(module_env, expr_idx)) |closure_expr_idx| {
                break :blk try self.lowerCirExprInto(closure_expr_idx, target, next);
            }

            const callable_inst_id = (try self.resolveBestExactCallableInstForExpr(
                expr_idx,
                monotype,
                self.current_module_idx,
            ));
            _ = lambda;
            const resolved_callable_inst_id = callable_inst_id orelse std.debug.panic(
                "statement-only MIR invariant violated: lambda expr {d} lacked an exact callable specialization in context callable_inst={d} root_source_expr={d}",
                .{
                    @intFromEnum(expr_idx),
                    @intFromEnum(self.current_callable_inst_context),
                    self.currentRootExprRawForDebug(),
                },
            );
            break :blk try self.lowerResolvedCallableInstValueInto(
                resolved_callable_inst_id,
                target,
                next,
            );
        },
        .e_closure => |closure| blk: {
            const closure_callable_inst_id = (try self.resolveBestExactCallableInstForExpr(
                expr_idx,
                monotype,
                self.current_module_idx,
            ));
            const callable_inst_id = closure_callable_inst_id orelse std.debug.panic(
                "statement-only MIR invariant violated: closure expr {d} lacked an exact callable specialization in context callable_inst={d} root_source_expr={d}",
                .{
                    @intFromEnum(expr_idx),
                    @intFromEnum(self.current_callable_inst_context),
                    self.currentRootExprRawForDebug(),
                },
            );
            _ = closure;
            break :blk try self.lowerResolvedCallableInstValueInto(
                callable_inst_id,
                target,
                next,
            );
        },
        .e_block => |block| blk: {
            const lowered = try self.lowerBlockExprIntoWithExitScope(block, target, next);
            self.current_pattern_scope = lowered.exit_scope;
            break :blk lowered.entry;
        },
        .e_dot_access => |da| self.lowerDotAccessInto(expr_idx, da, target, next),
        .e_tuple_access => |tuple_access| self.lowerTupleAccessInto(expr_idx, tuple_access, target, next),
        .e_binop => |binop| self.lowerBinopInto(binop, target, next),
        .e_unary_minus => |um| self.lowerUnaryMinusInto(um, target, next),
        .e_unary_not => |unary| self.lowerUnaryNotInto(unary, target, next),
        .e_call => |call| self.lowerCallInto(module_env, expr_idx, call, target, next),
        .e_match => |match_expr| blk: {
            const lowered = try self.lowerMatchIntoWithExitScope(module_env, match_expr, target, next);
            self.current_pattern_scope = lowered.exit_scope;
            break :blk lowered.entry;
        },
        .e_if => |if_expr| blk: {
            const lowered = try self.lowerBoolBranchChainIntoWithExitScope(
                module_env.store.sliceIfBranches(if_expr.branches),
                if_expr.final_else,
                target,
                next,
            );
            self.current_pattern_scope = lowered.exit_scope;
            break :blk lowered.entry;
        },
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

            const assign_stmt = try self.store.reserveCFStmt(self.allocator);

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
            const value_local = try self.freshSyntheticLocal(try self.resolveMonotype(return_expr.expr), false);
            const ret_stmt = try self.store.addCFStmt(self.allocator, .{ .ret = .{ .value = value_local } });
            break :blk try self.lowerCirExprInto(return_expr.expr, value_local, ret_stmt);
        },
        .e_type_var_dispatch => |dispatch_expr| blk: {
            const dispatch_target = self.lookupPipelinedDispatchTarget(expr_idx) orelse std.debug.panic(
                "statement-only MIR invariant violated: missing resolved dispatch target for CIR expr {d} kind=e_type_var_dispatch module={d} method={s} callable_ctx={d} root_ctx={d}",
                .{
                    @intFromEnum(expr_idx),
                    self.current_module_idx,
                    module_env.getIdent(dispatch_expr.method_name),
                    @intFromEnum(self.current_callable_inst_context),
                    self.currentRootExprRawForDebug(),
                },
            );
            break :blk try self.lowerResolvedDispatchTargetCallInto(
                dispatch_target,
                expr_idx,
                null,
                module_env.store.sliceExpr(dispatch_expr.args),
                target,
                next,
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
fn patternToLocal(self: *Self, pattern_idx: CIR.Pattern.Idx) Allocator.Error!MIR.LocalId {
    const key = patternScopeKey(self.current_module_idx, self.current_pattern_scope, pattern_idx);

    if (self.pattern_symbols.get(key)) |existing| {
        if (self.explicit_pattern_local_monotypes.get(key)) |monotype| {
            self.store.getLocalPtr(existing).monotype = monotype;
            return existing;
        }
        try self.refreshPatternLocalMonotype(pattern_idx, existing);
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

    const monotype = try self.requirePatternMonotype(pattern_idx);
    const local = try self.store.addLocal(self.allocator, .{
        .monotype = monotype,
        .reassignable = ident_idx.attributes.reassignable,
    });
    try self.pattern_symbols.put(key, local);
    return local;
}

fn patternToLocalWithMonotype(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    monotype: Monotype.Idx,
) Allocator.Error!MIR.LocalId {
    const key = patternScopeKey(self.current_module_idx, self.current_pattern_scope, pattern_idx);

    if (self.pattern_symbols.get(key)) |existing| {
        self.store.getLocalPtr(existing).monotype = monotype;
        try self.explicit_pattern_local_monotypes.put(key, monotype);
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

    const local = try self.store.addLocal(self.allocator, .{
        .monotype = monotype,
        .reassignable = ident_idx.attributes.reassignable,
    });
    try self.pattern_symbols.put(key, local);
    try self.explicit_pattern_local_monotypes.put(key, monotype);
    return local;
}

fn lookupExistingPatternLocal(self: *Self, pattern_idx: CIR.Pattern.Idx) ?MIR.LocalId {
    if (self.lookupExistingPatternLocalAtScope(self.current_pattern_scope, pattern_idx)) |local| return local;

    var scope = self.pattern_scope_parents.get(self.current_pattern_scope) orelse return null;
    while (true) {
        if (self.lookupExistingPatternLocalAtScope(scope, pattern_idx)) |local| return local;
        scope = self.pattern_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn lookupUsablePatternLocalAtScope(
    self: *Self,
    pattern_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const local = self.lookupExistingPatternLocalAtScope(pattern_scope, pattern_idx) orelse return null;
    return local;
}

fn lookupExistingPatternLocalWithinCurrentLambda(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    if (self.lookupExistingPatternLocalAtScope(self.current_pattern_scope, pattern_idx)) |local| return local;

    const boundary = self.current_lambda_scope_boundary orelse return null;
    var scope = self.pattern_scope_parents.get(self.current_pattern_scope) orelse return null;
    while (true) {
        if (self.lookupExistingPatternLocalAtScope(scope, pattern_idx)) |local| return local;
        if (scope == boundary) break;
        scope = self.pattern_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn lookupExistingPatternLocalAtScope(
    self: *Self,
    pattern_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const local = self.lookupExistingPatternLocalInScope(
        self.current_module_idx,
        pattern_scope,
        pattern_idx,
    ) orelse return null;
    const key = patternScopeKey(self.current_module_idx, pattern_scope, pattern_idx);
    if (self.explicit_pattern_local_monotypes.get(key)) |monotype| {
        self.store.getLocalPtr(local).monotype = monotype;
    } else {
        self.refreshPatternLocalMonotype(pattern_idx, local) catch unreachable;
    }
    return local;
}

fn bindPatternLocalInCurrentScope(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
) Allocator.Error!void {
    const key = patternScopeKey(self.current_module_idx, self.current_pattern_scope, pattern_idx);
    try self.pattern_symbols.put(key, local);
}

fn patternScopeKey(module_idx: u32, pattern_scope: u64, pattern_idx: CIR.Pattern.Idx) u128 {
    const base_key: u64 = (@as(u64, module_idx) << 32) | @intFromEnum(pattern_idx);
    return (@as(u128, pattern_scope) << 64) | @as(u128, base_key);
}

fn paramPatternNeedsBindingStmt(
    self: *const Self,
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    source_local: MIR.LocalId,
) bool {
    return switch (module_env.store.getPattern(pattern_idx)) {
        .assign => {
            const existing = self.lookupExistingPatternLocalInScope(
                self.current_module_idx,
                self.current_pattern_scope,
                pattern_idx,
            ) orelse return true;
            return existing != source_local;
        },
        else => true,
    };
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

fn lookupExistingPatternLocalInAncestorScopes(
    self: *Self,
    module_idx: u32,
    pattern_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    const scoped = self.lookupExistingPatternLocalInAncestorScopesWithScope(
        module_idx,
        pattern_scope,
        pattern_idx,
    ) orelse return null;
    return scoped.local;
}

fn lookupUsablePatternLocalInAncestorScopes(
    self: *Self,
    module_idx: u32,
    pattern_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?MIR.LocalId {
    var scope = self.pattern_scope_parents.get(pattern_scope) orelse return null;
    while (true) {
        const local = if (module_idx == self.current_module_idx)
            self.lookupUsablePatternLocalAtScope(scope, pattern_idx)
        else
            self.lookupExistingPatternLocalInScope(module_idx, scope, pattern_idx);
        if (local) |usable| return usable;

        scope = self.pattern_scope_parents.get(scope) orelse break;
    }

    return null;
}

const ScopedPatternLocal = struct {
    scope: u64,
    local: MIR.LocalId,
};

fn lookupExistingPatternLocalInAncestorScopesWithScope(
    self: *Self,
    module_idx: u32,
    pattern_scope: u64,
    pattern_idx: CIR.Pattern.Idx,
) ?ScopedPatternLocal {
    var scope = self.pattern_scope_parents.get(pattern_scope) orelse return null;
    while (true) {
        if (self.lookupExistingPatternLocalInScope(module_idx, scope, pattern_idx)) |local| {
            if (module_idx == self.current_module_idx) {
                const key = patternScopeKey(module_idx, scope, pattern_idx);
                if (self.explicit_pattern_local_monotypes.get(key)) |monotype| {
                    self.store.getLocalPtr(local).monotype = monotype;
                } else {
                    self.refreshPatternLocalMonotype(pattern_idx, local) catch unreachable;
                }
            }
            return .{
                .scope = scope,
                .local = local,
            };
        }

        scope = self.pattern_scope_parents.get(scope) orelse break;
    }

    return null;
}

fn scopeIsWithinCurrentLambda(self: *const Self, scope: u64) bool {
    const boundary = self.current_lambda_scope_boundary orelse return false;
    var cursor = scope;
    while (true) {
        if (cursor == boundary) return true;
        cursor = self.pattern_scope_parents.get(cursor) orelse return false;
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

fn freshStatementPatternScope(self: *Self) u64 {
    return self.freshChildPatternScope(self.current_pattern_scope);
}

fn freshChildPatternScope(self: *Self, parent_scope: u64) u64 {
    const scope = self.next_statement_pattern_scope;
    self.next_statement_pattern_scope += 1;
    if (parent_scope != 0) {
        self.pattern_scope_parents.put(scope, parent_scope) catch @panic("OutOfMemory");
    }
    return scope;
}

fn currentVisibleReassignableBindings(self: *Self) Allocator.Error![]VisiblePatternBinding {
    var bindings = std.ArrayList(VisiblePatternBinding).empty;
    errdefer bindings.deinit(self.allocator);

    var seen_patterns = std.AutoHashMap(u64, void).init(self.allocator);
    defer seen_patterns.deinit();

    var scope = self.current_pattern_scope;
    while (true) {
        var it = self.pattern_symbols.iterator();
        while (it.next()) |entry| {
            const packed_key = entry.key_ptr.*;
            const entry_scope: u64 = @truncate(packed_key >> 64);
            if (entry_scope != scope) continue;

            const base_key: u64 = @truncate(packed_key);
            const entry_module_idx: u32 = @truncate(base_key >> 32);
            if (entry_module_idx != self.current_module_idx) continue;
            if (seen_patterns.contains(base_key)) continue;

            const local = entry.value_ptr.*;
            if (@intFromEnum(local) < self.current_body_local_floor) continue;
            if (!self.store.getLocal(local).reassignable) continue;

            try seen_patterns.put(base_key, {});
            try bindings.append(self.allocator, .{
                .pattern_idx = @enumFromInt(@as(u32, @truncate(base_key))),
                .local = local,
            });
        }

        if (scope == 0) break;
        scope = self.pattern_scope_parents.get(scope) orelse 0;
    }

    std.mem.sort(
        VisiblePatternBinding,
        bindings.items,
        {},
        struct {
            fn lessThan(_: void, lhs: VisiblePatternBinding, rhs: VisiblePatternBinding) bool {
                return @intFromEnum(lhs.pattern_idx) < @intFromEnum(rhs.pattern_idx);
            }
        }.lessThan,
    );

    return bindings.toOwnedSlice(self.allocator);
}

fn visibleReassignableBindingsInScope(
    self: *Self,
    scope: u64,
) Allocator.Error![]VisiblePatternBinding {
    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = scope;
    defer self.current_pattern_scope = saved_scope;
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

    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = after_scope;
    defer self.current_pattern_scope = saved_scope;

    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        if (self.lookupExistingPatternLocalInScope(
            self.current_module_idx,
            after_scope,
            binding.pattern_idx,
        ) != null) continue;

        try self.bindPatternLocalInCurrentScope(binding.pattern_idx, binding.local);
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

    const saved_scope = self.current_pattern_scope;
    self.current_pattern_scope = after_scope;
    defer self.current_pattern_scope = saved_scope;

    for_bindings: for (bindings) |binding| {
        for (excluded_patterns) |excluded_pattern| {
            if (binding.pattern_idx == excluded_pattern) continue :for_bindings;
        }
        if (self.lookupExistingPatternLocalInScope(
            self.current_module_idx,
            after_scope,
            binding.pattern_idx,
        ) != null) continue;

        const source_local = binding.local;
        const new_local = try self.freshSyntheticLocal(
            self.store.getLocal(source_local).monotype,
            true,
        );
        try self.bindPatternLocalInCurrentScope(binding.pattern_idx, new_local);
    }
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
    if (self.lookupPipelinedExprMonotype(expr_idx)) |mono| {
        return self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            mono.idx,
            mono.module_idx,
            self.current_module_idx,
        );
    }

    if (try self.semanticExprResultMonotype(expr_idx)) |monotype| {
        return monotype;
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
    if (self.lookupExistingPatternLocalInScope(
        self.current_module_idx,
        self.current_pattern_scope,
        pattern_idx,
    )) |local| {
        try self.refreshPatternLocalMonotype(pattern_idx, local);
    }

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
            switch (self.store.monotype_store.getMonotype(monotype)) {
                .record => |record_mono| {
                    const mono_fields = self.store.monotype_store.getFields(record_mono.fields);
                    for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                        const destruct = module_env.store.getRecordDestruct(destruct_idx);
                        const pat_idx = destruct.kind.toPatternIdx();
                        const field_idx = self.recordFieldIndexByName(destruct.label, mono_fields);
                        try self.bindPatternMonotypes(module_env, pat_idx, mono_fields[field_idx].type_idx);
                    }
                },
                .unit => {
                    for (module_env.store.sliceRecordDestructs(record_pat.destructs)) |destruct_idx| {
                        const destruct = module_env.store.getRecordDestruct(destruct_idx);
                        try self.bindPatternMonotypes(module_env, destruct.kind.toPatternIdx(), self.store.monotype_store.unit_idx);
                    }
                },
                else => typeBindingInvariant(
                    "bindPatternMonotypes(record_destructure): expected record monotype, found '{s}'",
                    .{@tagName(self.store.monotype_store.getMonotype(monotype))},
                ),
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

fn refreshPatternLocalMonotype(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
    local: MIR.LocalId,
) Allocator.Error!void {
    const monotype = try self.requirePatternMonotype(pattern_idx);
    self.store.getLocalPtr(local).monotype = monotype;
}

fn requirePatternMonotype(
    self: *Self,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!Monotype.Idx {
    const module_env = self.all_module_envs[self.current_module_idx];

    if (self.lookupPipelinedPatternMonotype(pattern_idx)) |resolved| {
        return self.importMonotypeFromStore(
            &self.callable_pipeline.context_mono.monotype_store,
            resolved.idx,
            resolved.module_idx,
            self.current_module_idx,
        );
    }

    if (self.callable_pipeline.lambda_solved.getPatternSourceExpr(self.current_module_idx, pattern_idx)) |source| {
        const monotype = try self.resolveExprMonotypeInModule(source.module_idx, source.expr_idx);
        return monotype;
    }

    const resolved_pattern_var = self.types_store.resolveVar(ModuleEnv.varFrom(pattern_idx)).var_;
    if (self.type_var_seen.get(resolved_pattern_var)) |bound| {
        return bound;
    }

    if (findDefByPattern(module_env, pattern_idx)) |def_idx| {
        return self.resolveExprMonotypeInModule(self.current_module_idx, module_env.store.getDef(def_idx).expr);
    }

    std.debug.panic(
        "statement-only MIR invariant violated: missing exact monotype for pattern {d} in module {d} scope={d} callable_inst={d} root_source_expr={d}",
        .{
            @intFromEnum(pattern_idx),
            self.current_module_idx,
            self.current_pattern_scope,
            @intFromEnum(self.current_callable_inst_context),
            self.currentRootExprRawForDebug(),
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
            const context_template = if (!self.current_callable_inst_context.isNone())
                self.callable_pipeline.lambda_solved.getCallableTemplate(
                    self.callable_pipeline.lambda_specialize.getCallableInst(self.current_callable_inst_context).template,
                )
            else
                null;
            const context_region = if (context_template) |template|
                self.all_module_envs[template.module_idx].store.getExprRegion(template.cir_expr)
            else
                null;
            const root_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(resolved.var_));
            const root_node = self.all_module_envs[self.current_module_idx].store.nodes.get(root_node_idx);
            typeBindingInvariant(
                "bindTypeVarMonotypes: conflicting monotype binding for type var root {d} node_tag={s} in module {d} existing={d} existing_mono={any} new={d} new_mono={any} callable_inst={d} root_source_expr={d} template_expr={d} template_kind={s} template_region={any}",
                .{
                    @intFromEnum(resolved.var_),
                    @tagName(root_node.tag),
                    self.current_module_idx,
                    @intFromEnum(existing),
                    self.store.monotype_store.getMonotype(existing),
                    @intFromEnum(monotype),
                    self.store.monotype_store.getMonotype(monotype),
                    @intFromEnum(self.current_callable_inst_context),
                    self.currentRootExprRawForDebug(),
                    if (context_template) |template| @intFromEnum(template.cir_expr) else std.math.maxInt(u32),
                    if (context_template) |template| @tagName(template.kind) else "none",
                    context_region,
                },
            );
        }
        return;
    }

    switch (resolved.desc.content) {
        .flex, .rigid => {
            try self.type_var_seen.put(resolved.var_, monotype);
        },
        .alias => |alias| {
            try self.type_var_seen.put(resolved.var_, monotype);
            const backing_var = self.types_store.getAliasBackingVar(alias);
            try self.bindTypeVarMonotypes(backing_var, monotype);
        },
        .structure => |flat_type| {
            // Register before recursing so recursive structures short-circuit.
            try self.type_var_seen.put(resolved.var_, monotype);
            try self.bindFlatTypeMonotypes(resolved.var_, flat_type, monotype);
        },
        .err => {},
    }
}

fn bindFlatTypeMonotypes(
    self: *Self,
    root_var: types.Var,
    flat_type: types.FlatType,
    monotype: Monotype.Idx,
) Allocator.Error!void {
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
                const store_nodes_len = self.all_module_envs[self.current_module_idx].store.nodes.len();
                switch (mono) {
                    .prim => {},
                    else => typeBindingInvariant(
                        "bindFlatTypeMonotypes(nominal prim): expected prim monotype, found '{s}' for root_var={d} module={d} store_nodes_len={d} root_var_in_range={any} callable_inst={d} root_source_expr={d}",
                        .{
                            @tagName(mono),
                            @intFromEnum(root_var),
                            self.current_module_idx,
                            store_nodes_len,
                            @intFromEnum(root_var) < store_nodes_len,
                            @intFromEnum(self.current_callable_inst_context),
                            self.currentRootExprRawForDebug(),
                        },
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
                    try self.bindRecordFlatTypeToUnit(record);
                    return;
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
                    try self.bindRecordUnboundFlatTypeToUnit(fields_range);
                    return;
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
