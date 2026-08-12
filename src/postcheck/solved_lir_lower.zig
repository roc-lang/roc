//! Direct Lambda Solved IR to LIR lowering.
//!
//! This lowerer keeps the Lambda Mono type and callable/procedure decisions, but
//! does not materialize a Lambda Mono expression, pattern, statement, or local
//! tree in release builds.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");
const layout = @import("layout");
const Common = @import("common.zig");
const match_tree = @import("match_tree.zig");
const Mono = @import("monotype/ast.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const SolvedInline = @import("solved_inline.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");
const LambdaMono = @import("lambda_mono/ast.zig");
const LambdaMonoLower = @import("lambda_mono/lower.zig");
const MonoType = @import("monotype/type.zig");
const Type = @import("lambda_mono/type.zig");
const lir_core = @import("lir_core");
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const RootMetadata = lir_core.RootMetadata.RootMetadata;
const CheckedArithmetic = lir_core.CheckedArithmetic;
const const_store = check.ConstStore;
const GuardedList = collections.GuardedList;
const PatternRefutability = can.PatternRefutability;

/// Runtime field order for a named record field.
pub const RuntimeRecordFieldSchema = struct {
    name: []const u8,
    logical_index: u32,
};

/// Runtime field order for a named record.
pub const RuntimeRecordSchema = struct {
    type_name: []const u8,
    fields: []const RuntimeRecordFieldSchema,

    pub fn fieldLogicalIndex(self: RuntimeRecordSchema, field_name: []const u8) ?u32 {
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) return field.logical_index;
        }
        return null;
    }
};

/// Runtime discriminant for a named tag.
pub const RuntimeTagSchema = struct {
    name: []const u8,
    discriminant: u16,
};

/// Runtime tag order for a named tag union.
pub const RuntimeTagUnionSchema = struct {
    type_name: []const u8,
    tags: []const RuntimeTagSchema,

    pub fn tagDiscriminant(self: RuntimeTagUnionSchema, tag_name: []const u8) ?u16 {
        for (self.tags) |tag| {
            if (std.mem.eql(u8, tag.name, tag_name)) return tag.discriminant;
        }
        return null;
    }
};

/// Runtime schemas emitted while lowering Lambda Solved to LIR.
pub const RuntimeSchemaStore = struct {
    allocator: std.mem.Allocator,
    records: std.ArrayList(RuntimeRecordSchema),
    tag_unions: std.ArrayList(RuntimeTagUnionSchema),

    pub fn init(allocator: std.mem.Allocator) RuntimeSchemaStore {
        return .{
            .allocator = allocator,
            .records = .empty,
            .tag_unions = .empty,
        };
    }

    pub fn deinit(self: *RuntimeSchemaStore) void {
        for (self.records.items) |schema| {
            for (schema.fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema.fields);
            self.allocator.free(schema.type_name);
        }
        for (self.tag_unions.items) |schema| {
            for (schema.tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema.tags);
            self.allocator.free(schema.type_name);
        }
        self.tag_unions.deinit(self.allocator);
        self.records.deinit(self.allocator);
    }
};

/// LIR result plus runtime schemas emitted by direct LIR lowering.
pub const Output = struct {
    lir_result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,

    pub fn deinit(self: *Output) void {
        self.runtime_schemas.deinit();
        self.lir_result.deinit();
    }
};

/// Whether inline expects should materialize during lowering.
pub const InlineExpectMode = enum {
    run,
    omit,
};

/// Seed source used by the internal Dict implementation.
pub const DictSeedMode = enum {
    runtime,
    comptime_zero,
};

/// Configuration for direct solved-to-LIR lowering.
pub const Options = struct {
    inline_plan: SolvedInline.Plan = .{},
    /// Whether inline `expect` is lowered into runtime statements. Compile-time
    /// finalization and tests leave this enabled; optimized runtime builds omit
    /// inline expects before LIR reaches any backend.
    inline_expects: InlineExpectMode = .run,
    /// Allow `List.map` to reuse a unique input list's allocation for its
    /// output when the input and output element layouts are interchangeable.
    /// When disabled, `list_map_can_reuse` lowers to a constant 0 and the
    /// in-place branch of `List.map` is dropped before it reaches LIR.
    list_in_place_map: bool = false,
    /// Compile-time evaluation must use deterministic seed zero. Runtime
    /// lowering retains the process-randomized seed operation.
    dict_seed_mode: DictSeedMode = .runtime,
    /// Preserve source-level procedure names in LIR for runtime diagnostics.
    proc_debug_names: bool = false,
    /// Build ConstStore materialization plans for requested layouts.
    /// Static-data exports require this; layout-only consumers such as glue do not.
    layout_request_const_plans: bool = true,
    /// Optional command-level test-plan metadata keyed by checked root order.
    test_plan_metadata: []const Common.RootTestPlanMetadata = &.{},
    /// Debug-only destination for the materialized Lambda Mono verifier input.
    debug_materialized_out: ?*?LambdaMono.Program = null,
};

/// Lower Lambda Solved directly into LIR.
pub fn run(
    allocator: std.mem.Allocator,
    target_usize: base.target.TargetUsize,
    solved: Solved.Program,
    options: Options,
) Common.LowerError!Output {
    var owned = solved;
    errdefer owned.deinit();

    var lowerer = try Lowerer.init(allocator, target_usize, &owned, options);
    errdefer lowerer.deinit();

    try lowerer.result.store.setSourceFiles(owned.lifted.sourceFileNames());
    try lowerer.lowerInlineScopes();
    try lowerer.lower();
    try lowerer.bindRoots();
    try lowerer.lowerReachableFns();
    try lowerer.writeRuntimeSchemas();
    if (builtin.mode == .Debug) {
        try lowerer.verifyMaterializedDecisions();
    }

    owned.deinit();
    return lowerer.finish();
}

const CaptureAbi = enum {
    finite,
    erased,
};

const CaptureSpanSource = enum {
    solved,
    own,
};

const CaptureSpanId = struct {
    source: CaptureSpanSource,
    start: u32,
    len: u32,

    fn fromSolved(span: SolvedType.Span) CaptureSpanId {
        return .{ .source = .solved, .start = span.start, .len = span.len };
    }

    fn fromOwn(start: u32, len: u32) CaptureSpanId {
        return .{ .source = .own, .start = start, .len = len };
    }
};

fn specializationIdentityCaptureStart(span: CaptureSpanId) u32 {
    return switch (span.source) {
        .solved => span.start,
        .own => 0,
    };
}

const CaptureTypeKey = struct {
    source: CaptureSpanSource,
    start: u32,
    len: u32,
    solved_fn_ty: SolvedType.TypeVarId,

    fn from(span: CaptureSpanId, solved_fn_ty: SolvedType.TypeVarId) CaptureTypeKey {
        return .{
            .source = span.source,
            .start = span.start,
            .len = span.len,
            .solved_fn_ty = solved_fn_ty,
        };
    }
};

const ErasedReturnReuse = union(enum) {
    none,
    erased_callable: ?Type.TypeId,

    fn enabled(self: ErasedReturnReuse) bool {
        return switch (self) {
            .none => false,
            .erased_callable => true,
        };
    }
};

/// Type-derived demand for reusing one erased-callable allocation somewhere
/// in a procedure result. Record and tuple children are simultaneous, while
/// tag variants are alternatives; `.single_slot` therefore means every
/// runtime result contains at most one demanded slot and at least one variant
/// contains exactly one.
const ErasedResultDemand = enum {
    none,
    single_slot,
    ambiguous,
};

/// Ownership provenance recorded at the point where lowering emits a local
/// definition. `.alias` is reserved for representation-transparent edges
/// whose source and target denote the same refcounted allocation.
const ErasedOwnerState = union(enum) {
    pending,
    root,
    alias: LIR.LocalId,
    ambiguous,
};

const ErasedCallOwnerUse = struct {
    stmt: LIR.CFStmtId,
    closure: LIR.LocalId,
};

fn mergeSimultaneousErasedResultDemand(
    lhs: ErasedResultDemand,
    rhs: ErasedResultDemand,
) ErasedResultDemand {
    if (lhs == .ambiguous or rhs == .ambiguous) return .ambiguous;
    if (lhs == .single_slot and rhs == .single_slot) return .ambiguous;
    if (lhs == .single_slot or rhs == .single_slot) return .single_slot;
    return .none;
}

const FnSpec = struct {
    source: Lifted.FnId,
    solved_fn_ty: SolvedType.TypeVarId,
    abi: CaptureAbi,
    captures: CaptureSpanId,
    capture_ty: ?Type.TypeId,
    /// Optional erased-callable destination threaded through an internal
    /// finite proc. The payload records the capture type already resident in
    /// that destination; `null` means an enabled destination with no captures.
    return_reuse: ErasedReturnReuse,
};

const FnSpecContext = struct {
    pub fn hash(_: FnSpecContext, spec: FnSpec) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, @intFromEnum(spec.source));
        std.hash.autoHash(&hasher, @intFromEnum(spec.solved_fn_ty));
        std.hash.autoHash(&hasher, spec.abi);
        std.hash.autoHash(&hasher, spec.captures.source);
        std.hash.autoHash(&hasher, spec.captures.start);
        std.hash.autoHash(&hasher, spec.captures.len);
        if (spec.capture_ty) |capture_ty| {
            std.hash.autoHash(&hasher, @intFromEnum(capture_ty));
        } else {
            std.hash.autoHash(&hasher, @as(u32, std.math.maxInt(u32)));
        }
        switch (spec.return_reuse) {
            .none => std.hash.autoHash(&hasher, @as(u8, 0)),
            .erased_callable => |capture_ty| {
                std.hash.autoHash(&hasher, @as(u8, 1));
                std.hash.autoHash(&hasher, if (capture_ty) |ty| @intFromEnum(ty) else std.math.maxInt(u32));
            },
        }
        return hasher.final();
    }

    pub fn eql(_: FnSpecContext, lhs: FnSpec, rhs: FnSpec) bool {
        return lhs.source == rhs.source and
            lhs.solved_fn_ty == rhs.solved_fn_ty and
            lhs.abi == rhs.abi and
            lhs.captures.source == rhs.captures.source and
            lhs.captures.start == rhs.captures.start and
            lhs.captures.len == rhs.captures.len and
            lhs.capture_ty == rhs.capture_ty and
            std.meta.eql(lhs.return_reuse, rhs.return_reuse);
    }
};

const CaptureTypeMap = std.HashMap(CaptureTypeKey, Type.TypeId, CaptureSpanContext, std.hash_map.default_max_load_percentage);

const CaptureSpanContext = struct {
    pub fn hash(_: CaptureSpanContext, span: CaptureTypeKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, span.source);
        std.hash.autoHash(&hasher, span.start);
        std.hash.autoHash(&hasher, span.len);
        std.hash.autoHash(&hasher, @intFromEnum(span.solved_fn_ty));
        return hasher.final();
    }

    pub fn eql(_: CaptureSpanContext, lhs: CaptureTypeKey, rhs: CaptureTypeKey) bool {
        return lhs.source == rhs.source and
            lhs.start == rhs.start and
            lhs.len == rhs.len and
            lhs.solved_fn_ty == rhs.solved_fn_ty;
    }
};

const FnEntry = struct {
    spec: FnSpec,
    symbol: Common.Symbol,
    source: ?Mono.FnTemplate,
    args: Type.Span,
    ret: Type.TypeId,
    capture_arg_ty: ?Type.TypeId,
    proc: ?LIR.LirProcSpecId,
};

const RootEntry = struct {
    fn_id: Type.FnId,
    request: check.CheckedModule.RootRequest,
};

const LayoutRequest = struct {
    checked_type: check.CheckedModule.CheckedTypeId,
    ty: Type.TypeId,
    initializer: ?Type.FnId,
    const_locator: ?check.CheckedModule.ConstLocator,
};

const StaticInitializerRequest = struct {
    static_data: Common.StaticDataId,
    layout_idx: layout.Idx,
};

const StaticInitializerEntry = struct {
    expr: Lifted.ExprId,
    ty: Type.TypeId,
    proc: LIR.LirProcSpecId,
};

const RuntimeSchemaRequest = struct {
    def: MonoType.TypeDef,
    ty: Type.TypeId,
};

const CaptureSource = union(enum) {
    record: LIR.LocalId,
    erased_ptr: LIR.LocalId,
};

const CaptureBinding = struct {
    source: CaptureSource,
    record_ty: Type.TypeId,
    symbol: Common.Symbol,
    ty: Type.TypeId,
    storage_ty: Type.TypeId,
};

const TypedLiftedLocal = struct {
    local: Lifted.LocalId,
    ty: Type.TypeId,
};

const Lowerer = struct {
    allocator: std.mem.Allocator,
    solved: *const Solved.Program,
    solved_types: SolvedType.Store.View,
    types: Type.Store,
    result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,
    type_map: collections.DenseMap(SolvedType.TypeVarId, Type.TypeId),
    fn_specs: std.ArrayList(FnSpec),
    fn_entries: std.ArrayList(FnEntry),
    fn_spec_map: std.HashMap(FnSpec, Type.FnId, FnSpecContext, std.hash_map.default_max_load_percentage),
    fn_written: std.ArrayList(bool),
    fn_reachable: std.ArrayList(bool),
    fn_reach_queue: std.ArrayList(Type.FnId),
    inline_plan: SolvedInline.Plan,
    inline_expects: InlineExpectMode,
    list_in_place_map: bool,
    dict_seed_mode: DictSeedMode,
    proc_debug_names: bool,
    layout_request_const_plans: bool,
    /// Match sites statically resolved by `foldListMapCanReuseMatch`,
    /// recorded (Debug only) so the Lambda Mono verifier replays them.
    folded_map_matches: std.ArrayList(Lifted.Program.FoldedMatch),
    source_symbols: std.AutoHashMap(Common.Symbol, Lifted.FnId),
    capture_types: CaptureTypeMap,
    captures: collections.DenseMap(Lifted.LocalId, CaptureBinding),
    recursive_value_locals: collections.DenseMap(Lifted.LocalId, void),
    recursive_value_capture_ids: collections.DenseMap(check.CheckedModule.CaptureId, void),
    recursive_slot_types: collections.DenseMap(Type.TypeId, Type.TypeId),
    own_captures: std.ArrayList(SolvedType.Capture),
    own_capture_spans: []?CaptureSpanId,
    roots: std.ArrayList(RootEntry),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),
    type_layouts: collections.DenseMap(Type.TypeId, layout.Idx),
    layout_owner_types: collections.DenseMap(Type.TypeId, Type.TypeId),
    const_plan_map: collections.DenseMap(Type.TypeId, LirProgram.ConstPlanId),
    const_type_map: collections.DenseMap(Type.TypeId, const_store.ConstTypeId),
    mono_const_type_map: collections.DenseMap(MonoType.TypeId, const_store.ConstTypeId),
    callable_source_fn_map: collections.DenseMap(Type.TypeId, SolvedType.TypeVarId),
    static_initializer_map: std.AutoHashMap(StaticInitializerRequest, LIR.StaticDataId),
    static_initializer_queue: std.ArrayList(StaticInitializerEntry),
    root_requests: Common.RootRequests,
    symbols: Common.SymbolGen,
    local_map: []?LIR.LocalId,
    typed_local_map: std.AutoHashMap(TypedLiftedLocal, LIR.LocalId),
    local_types: collections.DenseMap(LIR.LocalId, Type.TypeId),
    comptime_site_map: []?LIR.ComptimeSiteId,
    next_join_point: u32 = 0,
    loop_stack: std.ArrayList(LoopContext),
    join_stack: std.ArrayList(JoinContext),
    current_ret_ty: ?Type.TypeId = null,
    current_proc_locals: ?*ProcLocalSet = null,
    current_fn: ?Type.FnId = null,
    current_proc: ?LIR.LirProcSpecId = null,
    current_erased_reuse: ?LIR.LocalId = null,
    current_return_target: ?LIR.LocalId = null,
    /// Locals proven during backwards lowering to feed the procedure's one
    /// selected erased-callable result slot. A later lexical producer uses
    /// this explicit provenance to inherit the return destination.
    return_forwarding_locals: collections.DenseMap(LIR.LocalId, void),
    /// Multiple distinct eager producers can feed one later runtime choice,
    /// but the hidden reuse owner is affine and cannot be offered to all of
    /// them. Keep that candidate group disqualified until every producer has
    /// been crossed during backwards lowering.
    return_forwarding_ambiguous: bool = false,
    /// Loop and recursive-join bodies may execute more than once. The hidden
    /// erased reuse owner is affine, so lexical return forwarding is disabled
    /// anywhere inside a repeatable control region.
    return_forwarding_repeatable_depth: usize = 0,
    erased_owner_states: std.ArrayList(ErasedOwnerState),
    erased_call_owner_uses: std.ArrayList(ErasedCallOwnerUse),
    erased_capture_ptr_ty: ?Type.TypeId = null,
    debug_materialized_out: ?*?LambdaMono.Program = null,

    const ProcLocalSet = std.AutoArrayHashMapUnmanaged(u32, void);

    const LoopContext = struct {
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        param_tys: []const Type.TypeId,
        result_target: LIR.LocalId,
        result_ty: Type.TypeId,
        after_loop: LIR.CFStmtId,
    };

    const JoinContext = struct {
        source_id: Lifted.JoinPointId,
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        param_tys: []const Type.TypeId,
    };

    fn init(
        allocator: std.mem.Allocator,
        target_usize: base.target.TargetUsize,
        solved: *const Solved.Program,
        options: Options,
    ) Common.LowerError!Lowerer {
        const local_map = try allocator.alloc(?LIR.LocalId, solved.lifted.localCount());
        errdefer allocator.free(local_map);
        @memset(local_map, null);

        const comptime_site_map = try allocator.alloc(?LIR.ComptimeSiteId, solved.lifted.comptimeSiteCount());
        errdefer allocator.free(comptime_site_map);
        @memset(comptime_site_map, null);

        const own_capture_spans = try allocator.alloc(?CaptureSpanId, solved.lifted.fnCount());
        errdefer allocator.free(own_capture_spans);
        @memset(own_capture_spans, null);

        var recursive_value_locals = collections.DenseMap(Lifted.LocalId, void).init(allocator);
        errdefer recursive_value_locals.deinit();
        var recursive_value_capture_ids = collections.DenseMap(check.CheckedModule.CaptureId, void).init(allocator);
        errdefer recursive_value_capture_ids.deinit();
        try Lowerer.collectRecursiveValueLocals(&solved.lifted, &recursive_value_locals, &recursive_value_capture_ids);

        return .{
            .allocator = allocator,
            .solved = solved,
            .solved_types = solved.types.view(),
            .types = Type.Store.init(allocator),
            .result = try LirProgram.Result.init(allocator, target_usize),
            .runtime_schemas = RuntimeSchemaStore.init(allocator),
            .type_map = collections.DenseMap(SolvedType.TypeVarId, Type.TypeId).init(allocator),
            .fn_specs = .empty,
            .fn_entries = .empty,
            .fn_spec_map = std.HashMap(FnSpec, Type.FnId, FnSpecContext, std.hash_map.default_max_load_percentage).initContext(allocator, .{}),
            .fn_written = .empty,
            .fn_reachable = .empty,
            .fn_reach_queue = .empty,
            .inline_plan = options.inline_plan,
            .inline_expects = options.inline_expects,
            .list_in_place_map = options.list_in_place_map,
            .dict_seed_mode = options.dict_seed_mode,
            .proc_debug_names = options.proc_debug_names,
            .layout_request_const_plans = options.layout_request_const_plans,
            .debug_materialized_out = options.debug_materialized_out,
            .root_requests = .{ .test_plan_metadata = options.test_plan_metadata },
            .folded_map_matches = .empty,
            .source_symbols = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator),
            .capture_types = CaptureTypeMap.initContext(allocator, .{}),
            .captures = collections.DenseMap(Lifted.LocalId, CaptureBinding).init(allocator),
            .recursive_value_locals = recursive_value_locals,
            .recursive_value_capture_ids = recursive_value_capture_ids,
            .recursive_slot_types = collections.DenseMap(Type.TypeId, Type.TypeId).init(allocator),
            .own_captures = .empty,
            .own_capture_spans = own_capture_spans,
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .type_layouts = collections.DenseMap(Type.TypeId, layout.Idx).init(allocator),
            .layout_owner_types = collections.DenseMap(Type.TypeId, Type.TypeId).init(allocator),
            .const_plan_map = collections.DenseMap(Type.TypeId, LirProgram.ConstPlanId).init(allocator),
            .const_type_map = collections.DenseMap(Type.TypeId, const_store.ConstTypeId).init(allocator),
            .mono_const_type_map = collections.DenseMap(MonoType.TypeId, const_store.ConstTypeId).init(allocator),
            .callable_source_fn_map = collections.DenseMap(Type.TypeId, SolvedType.TypeVarId).init(allocator),
            .static_initializer_map = std.AutoHashMap(StaticInitializerRequest, LIR.StaticDataId).init(allocator),
            .static_initializer_queue = .empty,
            .symbols = .{ .next = solved.lifted.next_symbol },
            .local_map = local_map,
            .typed_local_map = std.AutoHashMap(TypedLiftedLocal, LIR.LocalId).init(allocator),
            .local_types = collections.DenseMap(LIR.LocalId, Type.TypeId).init(allocator),
            .comptime_site_map = comptime_site_map,
            .loop_stack = .empty,
            .join_stack = .empty,
            .return_forwarding_locals = collections.DenseMap(LIR.LocalId, void).init(allocator),
            .erased_owner_states = .empty,
            .erased_call_owner_uses = .empty,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.folded_map_matches.deinit(self.allocator);
        self.return_forwarding_locals.deinit();
        self.erased_call_owner_uses.deinit(self.allocator);
        self.erased_owner_states.deinit(self.allocator);
        self.join_stack.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.comptime_site_map);
        self.typed_local_map.deinit();
        self.local_types.deinit();
        self.allocator.free(self.local_map);
        self.const_plan_map.deinit();
        self.const_type_map.deinit();
        self.mono_const_type_map.deinit();
        self.callable_source_fn_map.deinit();
        self.static_initializer_queue.deinit(self.allocator);
        self.static_initializer_map.deinit();
        self.layout_owner_types.deinit();
        self.type_layouts.deinit();
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.allocator.free(self.own_capture_spans);
        self.own_captures.deinit(self.allocator);
        self.recursive_slot_types.deinit();
        self.recursive_value_capture_ids.deinit();
        self.recursive_value_locals.deinit();
        self.captures.deinit();
        self.capture_types.deinit();
        self.source_symbols.deinit();
        self.fn_reach_queue.deinit(self.allocator);
        self.fn_reachable.deinit(self.allocator);
        self.fn_written.deinit(self.allocator);
        self.fn_spec_map.deinit();
        self.fn_entries.deinit(self.allocator);
        self.fn_specs.deinit(self.allocator);
        self.type_map.deinit();
        self.types.deinit();
        self.runtime_schemas.deinit();
        self.result.deinit();
    }

    fn finish(self: *Lowerer) Output {
        const output = Output{
            .lir_result = self.result,
            .runtime_schemas = self.runtime_schemas,
        };
        self.folded_map_matches.deinit(self.allocator);
        self.return_forwarding_locals.deinit();
        self.erased_call_owner_uses.deinit(self.allocator);
        self.erased_owner_states.deinit(self.allocator);
        self.join_stack.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.comptime_site_map);
        self.typed_local_map.deinit();
        self.local_types.deinit();
        self.allocator.free(self.local_map);
        self.const_plan_map.deinit();
        self.const_type_map.deinit();
        self.mono_const_type_map.deinit();
        self.callable_source_fn_map.deinit();
        self.static_initializer_queue.deinit(self.allocator);
        self.static_initializer_map.deinit();
        self.layout_owner_types.deinit();
        self.type_layouts.deinit();
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.allocator.free(self.own_capture_spans);
        self.own_captures.deinit(self.allocator);
        self.recursive_slot_types.deinit();
        self.recursive_value_capture_ids.deinit();
        self.recursive_value_locals.deinit();
        self.captures.deinit();
        self.capture_types.deinit();
        self.source_symbols.deinit();
        self.fn_reach_queue.deinit(self.allocator);
        self.fn_reachable.deinit(self.allocator);
        self.fn_written.deinit(self.allocator);
        self.fn_spec_map.deinit();
        self.fn_entries.deinit(self.allocator);
        self.fn_specs.deinit(self.allocator);
        self.type_map.deinit();
        self.types.deinit();
        self.result = undefined;
        self.runtime_schemas = RuntimeSchemaStore.init(self.allocator);
        self.local_map = &.{};
        self.own_capture_spans = &.{};
        self.own_captures = .empty;
        self.recursive_slot_types = collections.DenseMap(Type.TypeId, Type.TypeId).init(self.allocator);
        self.recursive_value_capture_ids = collections.DenseMap(check.CheckedModule.CaptureId, void).init(self.allocator);
        self.recursive_value_locals = collections.DenseMap(Lifted.LocalId, void).init(self.allocator);
        self.typed_local_map = std.AutoHashMap(TypedLiftedLocal, LIR.LocalId).init(self.allocator);
        self.local_types = collections.DenseMap(LIR.LocalId, Type.TypeId).init(self.allocator);
        self.static_initializer_queue = .empty;
        self.static_initializer_map = std.AutoHashMap(StaticInitializerRequest, LIR.StaticDataId).init(self.allocator);
        self.comptime_site_map = &.{};
        self.loop_stack = .empty;
        self.join_stack = .empty;
        self.return_forwarding_locals = collections.DenseMap(LIR.LocalId, void).init(self.allocator);
        self.return_forwarding_ambiguous = false;
        self.return_forwarding_repeatable_depth = 0;
        self.erased_owner_states = .empty;
        self.erased_call_owner_uses = .empty;
        self.folded_map_matches = .empty;
        return output;
    }

    fn lower(self: *Lowerer) Common.LowerError!void {
        try self.indexSourceFns();

        try self.roots.ensureTotalCapacity(self.allocator, self.solved.lifted.rootCount());
        for (self.solved.lifted.rootsView()) |root| {
            const fn_id = try self.ensureOwnFnSpec(root.fn_id, .finite);
            try self.roots.append(self.allocator, .{
                .fn_id = fn_id,
                .request = root.request,
            });
            _ = try self.markReachableFn(fn_id);
        }

        try self.layout_requests.ensureTotalCapacity(self.allocator, self.solved.layout_requests.items.len);
        for (self.solved.layout_requests.items) |request| {
            try self.layout_requests.append(self.allocator, .{
                .checked_type = request.checked_type,
                .ty = try self.lowerType(request.ty),
                .initializer = if (request.fn_id) |fn_id| try self.ensureOwnFnSpec(fn_id, .finite) else null,
                .const_locator = request.const_locator,
            });
        }

        try self.runtime_schema_requests.ensureTotalCapacity(self.allocator, self.solved.runtime_schema_requests.items.len);
        for (self.solved.runtime_schema_requests.items) |request| {
            try self.runtime_schema_requests.append(self.allocator, .{
                .def = request.def,
                .ty = try self.lowerType(request.ty),
            });
        }

        try self.lowerReachableFns();
    }

    fn lowerInlineScopes(self: *Lowerer) Common.LowerError!void {
        const lifted = self.solved.lifted.view();
        try self.result.store.inline_scopes.ensureTotalCapacity(self.allocator, lifted.inline_scopes.len);
        for (lifted.inline_scopes, 0..) |scope, index| {
            const expected: LIR.InlineScopeId = @enumFromInt(@as(u32, @intCast(index)));
            const source_name = if (self.solved.lifted.procDebugName(scope.source_symbol)) |name|
                try self.result.store.insertString(self.solved.lifted.names.exportNameText(name))
            else
                base.StringLiteral.Idx.none;
            const actual = try self.result.store.addInlineScope(.{
                .source_symbol = lirSymbol(scope.source_symbol),
                .source_name = source_name,
                .source_loc = scope.source_loc,
                .call_site = scope.call_site,
                .parent = lirInlineScopeId(scope.parent),
            });
            if (actual != expected) Common.invariant("LIR inline-scope identities diverged from lifted inline-scope identities");
        }
    }

    fn indexSourceFns(self: *Lowerer) Common.LowerError!void {
        for (0..self.solved.lifted.fnCount()) |index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            const fn_ = self.solved.lifted.getFn(fn_id);
            const result = try self.source_symbols.getOrPut(fn_.symbol);
            if (result.found_existing) Common.invariant("two lifted functions had the same symbol");
            result.value_ptr.* = fn_id;
        }
    }

    fn collectRecursiveValueLocals(
        lifted: *const Lifted.Program,
        locals: *collections.DenseMap(Lifted.LocalId, void),
        capture_ids: *collections.DenseMap(check.CheckedModule.CaptureId, void),
    ) std.mem.Allocator.Error!void {
        for (lifted.stmtsView()) |stmt| {
            if (stmt == .let_ and stmt.let_.recursive) {
                try Lowerer.collectRecursiveValueLocal(lifted, stmt.let_.pat, locals, capture_ids);
            }
        }
    }

    fn collectRecursiveValueLocal(
        lifted: *const Lifted.Program,
        pat_id: Lifted.PatId,
        locals: *collections.DenseMap(Lifted.LocalId, void),
        capture_ids: *collections.DenseMap(check.CheckedModule.CaptureId, void),
    ) std.mem.Allocator.Error!void {
        const recursive_pat = lifted.getPat(pat_id);
        if (recursive_pat.data != .bind) Common.invariant("recursive Monotype let statement must bind one local directly");
        const local = recursive_pat.data.bind;
        try locals.put(local, {});
        if (lifted.getLocal(local).capture_id) |capture_id| try capture_ids.put(capture_id, {});
    }

    fn lowerReachableFns(self: *Lowerer) Common.LowerError!void {
        var fn_queue_index: usize = 0;
        var initializer_queue_index: usize = 0;
        while (fn_queue_index < self.fn_reach_queue.items.len or initializer_queue_index < self.static_initializer_queue.items.len) {
            while (fn_queue_index < self.fn_reach_queue.items.len) : (fn_queue_index += 1) {
                const fn_id = self.fn_reach_queue.items[fn_queue_index];
                const fn_index = @intFromEnum(fn_id);
                if (self.fn_written.items[fn_index]) continue;
                try self.lowerFnSpec(fn_id, self.fn_specs.items[fn_index]);
            }
            if (initializer_queue_index < self.static_initializer_queue.items.len) {
                const initializer = self.static_initializer_queue.items[initializer_queue_index];
                initializer_queue_index += 1;
                try self.lowerStaticInitializer(initializer);
            }
        }
    }

    fn lowerStaticInitializer(self: *Lowerer, initializer: StaticInitializerEntry) Common.LowerError!void {
        self.captures.clearRetainingCapacity();
        @memset(self.local_map, null);
        self.typed_local_map.clearRetainingCapacity();
        self.local_types.clearRetainingCapacity();
        self.return_forwarding_locals.clearRetainingCapacity();
        self.return_forwarding_ambiguous = false;
        self.return_forwarding_repeatable_depth = 0;

        const saved_ret_ty = self.current_ret_ty;
        const saved_proc_locals = self.current_proc_locals;
        const saved_current_fn = self.current_fn;
        const saved_current_proc = self.current_proc;
        const saved_loc = self.result.store.current_loc;
        const saved_region = self.result.store.current_region;
        var proc_locals: ProcLocalSet = .{};
        defer proc_locals.deinit(self.allocator);
        defer {
            self.current_ret_ty = saved_ret_ty;
            self.current_proc_locals = saved_proc_locals;
            self.current_fn = saved_current_fn;
            self.current_proc = saved_current_proc;
            self.result.store.current_loc = saved_loc;
            self.result.store.current_region = saved_region;
        }

        self.current_ret_ty = initializer.ty;
        self.current_proc_locals = &proc_locals;
        self.current_fn = null;
        self.current_proc = initializer.proc;
        self.result.store.current_loc = self.solved.lifted.exprLoc(initializer.expr);
        self.result.store.current_region = self.solved.lifted.exprRegion(initializer.expr);

        // A recursive type can have a different representation at a nested
        // occurrence than at its graph root (for example, the occurrence is a
        // box while the root is a tag union). The request already carries that
        // exact layout in the initializer proc; lower directly into it instead
        // of reconstructing the root layout from the logical type.
        const ret_layout = self.result.store.getProcSpec(initializer.proc).ret_layout;
        const ret_local = try self.addLocalForLayout(ret_layout);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        const erased_owner_use_start = self.erased_call_owner_uses.items.len;
        const body = try self.lowerExprIntoAtType(ret_local, initializer.expr, initializer.ty, ret_stmt);
        if (self.return_forwarding_repeatable_depth != 0) {
            Common.invariant("static initializer lowering left a repeatable return-forwarding region active");
        }
        self.resolveErasedCallOwnerUses(erased_owner_use_start);
        const frame_locals = try self.writeFrameLocals(&proc_locals);
        const proc = self.result.store.getProcSpecPtr(initializer.proc);
        proc.body = body;
        proc.frame_locals = frame_locals;
        proc.stack_probe = self.stackProbeForProc(proc.args, proc.frame_locals, proc.ret_layout);
    }

    fn lowerFnSpec(self: *Lowerer, fn_id: Type.FnId, spec: FnSpec) Common.LowerError!void {
        const proc_id = try self.procPlaceholder(fn_id);
        const entry = self.fn_entries.items[@intFromEnum(fn_id)];
        const source_fn = self.solved.lifted.getFn(spec.source);

        self.captures.clearRetainingCapacity();
        @memset(self.local_map, null);
        self.typed_local_map.clearRetainingCapacity();
        self.local_types.clearRetainingCapacity();
        self.return_forwarding_locals.clearRetainingCapacity();
        self.return_forwarding_ambiguous = false;
        self.return_forwarding_repeatable_depth = 0;

        // Body lowering appends local spans, so keep proc arguments independent
        // of the guarded span store before any recursive lowering begins.
        const proc_args = try GuardedList.dupe(
            self.allocator,
            LIR.LocalId,
            self.result.store.getLocalSpan(self.result.store.getProcSpec(proc_id).args),
        );
        defer self.allocator.free(proc_args);
        const solved_fn_ty = spec.solved_fn_ty;
        const fn_content = self.solved.types.rootContent(solved_fn_ty);
        if (fn_content != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        const func = fn_content.func;
        const solved_args = self.solved_types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (solved_args.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");
        if (proc_args.len < lifted_args.len) Common.invariant("direct Lambda Mono proc placeholder had too few arguments");

        for (0..lifted_args.len) |i| {
            const arg = GuardedList.at(lifted_args, i);
            const proc_arg = GuardedList.at(proc_args, i);
            const arg_ty = try self.lowerType(GuardedList.at(solved_args, i));
            self.local_map[@intFromEnum(arg.local)] = proc_arg;
            try self.typed_local_map.put(.{
                .local = arg.local,
                .ty = arg_ty,
            }, proc_arg);
            try self.local_types.put(proc_arg, arg_ty);
        }

        switch (spec.abi) {
            .finite => {
                const expected_args = lifted_args.len +
                    @as(usize, @intFromBool(spec.capture_ty != null)) +
                    @as(usize, @intFromBool(spec.return_reuse.enabled()));
                if (proc_args.len != expected_args) Common.invariant("finite proc placeholder had wrong internal arity");
                if (spec.capture_ty) |capture_ty| {
                    try self.bindCaptureRecord(spec.captures, capture_ty, .{ .record = GuardedList.at(proc_args, lifted_args.len) });
                }
            },
            .erased => {
                if (proc_args.len != lifted_args.len + 2) Common.invariant("erased proc placeholder had wrong arity");
            },
        }

        switch (source_fn.body) {
            .roc => |body_expr| {
                const saved_ret_ty = self.current_ret_ty;
                const saved_proc_locals = self.current_proc_locals;
                const saved_current_fn = self.current_fn;
                const saved_current_proc = self.current_proc;
                const saved_erased_reuse = self.current_erased_reuse;
                var proc_locals: ProcLocalSet = .{};
                defer proc_locals.deinit(self.allocator);

                self.current_proc_locals = &proc_locals;
                self.current_ret_ty = entry.ret;
                self.current_fn = fn_id;
                self.current_proc = proc_id;
                self.current_erased_reuse = switch (spec.abi) {
                    .erased => if (self.erasedResultDemand(entry.ret) == .single_slot)
                        GuardedList.at(proc_args, lifted_args.len + 1)
                    else
                        null,
                    .finite => if (spec.return_reuse.enabled())
                        GuardedList.at(proc_args, lifted_args.len + @as(usize, @intFromBool(spec.capture_ty != null)))
                    else
                        null,
                };
                defer {
                    self.current_ret_ty = saved_ret_ty;
                    self.current_proc_locals = saved_proc_locals;
                    self.current_fn = saved_current_fn;
                    self.current_proc = saved_current_proc;
                    self.current_erased_reuse = saved_erased_reuse;
                }

                try self.noteLocalSpan(self.result.store.getProcSpec(proc_id).args);

                var capture_snapshot: ?LIR.LocalId = null;
                if (spec.capture_ty) |capture_ty| switch (spec.abi) {
                    .finite => {},
                    .erased => {
                        const capture_ptr = GuardedList.at(proc_args, lifted_args.len);
                        if (self.erasedResultDemand(entry.ret) == .single_slot) {
                            const snapshot = try self.addTemp(capture_ty);
                            try self.bindCaptureRecord(spec.captures, capture_ty, .{ .record = snapshot });
                            capture_snapshot = snapshot;
                        } else {
                            try self.bindCaptureRecord(spec.captures, capture_ty, .{ .erased_ptr = capture_ptr });
                        }
                    },
                };

                const erased_owner_use_start = self.erased_call_owner_uses.items.len;
                var body = try self.lowerExprReturn(body_expr, entry.ret);
                if (self.return_forwarding_repeatable_depth != 0) {
                    Common.invariant("function lowering left a repeatable return-forwarding region active");
                }
                if (capture_snapshot) |snapshot| {
                    body = try self.lowerAnchoredErasedCaptureLoadInto(
                        snapshot,
                        GuardedList.at(proc_args, lifted_args.len),
                        GuardedList.at(proc_args, lifted_args.len + 1),
                        body,
                    );
                }
                self.resolveErasedCallOwnerUses(erased_owner_use_start);

                const frame_locals = try self.writeFrameLocals(&proc_locals);
                const proc = self.result.store.getProcSpecPtr(proc_id);
                proc.body = body;
                proc.frame_locals = frame_locals;
                proc.stack_probe = self.stackProbeForProc(proc.args, proc.frame_locals, proc.ret_layout);
            },
            .hosted => {
                if (self.result.store.getProcSpec(proc_id).hosted == null) {
                    Common.invariant("hosted function reached direct LIR without hosted metadata");
                }
            },
        }

        self.fn_written.items[@intFromEnum(fn_id)] = true;
    }

    fn hostedProcForSource(self: *Lowerer, source: ?Mono.FnTemplate) Common.LowerError!?LIR.HostedProc {
        const template = source orelse return null;
        return try self.hostedProcForTemplate(template);
    }

    fn hostedProcForTemplate(self: *Lowerer, source: Mono.FnTemplate) Common.LowerError!?LIR.HostedProc {
        const hosted = if (source.fn_def == .local_hosted)
            source.fn_def.local_hosted
        else if (source.fn_def == .imported_hosted)
            source.fn_def.imported_hosted
        else
            return null;
        return .{
            .symbol = try self.result.store.insertString(
                self.solved.lifted.names.externalSymbolNameText(hosted.external_symbol_name),
            ),
            .dispatch_index = hosted.dispatch_index,
        };
    }

    fn ensureOwnFnSpec(self: *Lowerer, fn_id: Lifted.FnId, abi: CaptureAbi) Common.LowerError!Type.FnId {
        const solved_fn_ty = self.solved.types.root(self.solved.fn_tys.items[@intFromEnum(fn_id)]);
        if (self.solved.types.rootContent(solved_fn_ty) != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        return try self.ensureFnSpec(fn_id, solved_fn_ty, abi, try self.ownCaptureSpanForFn(fn_id), .none);
    }

    fn ensureOwnFnSpecWithErasedReturnReuse(
        self: *Lowerer,
        fn_id: Lifted.FnId,
        capture_ty: ?Type.TypeId,
    ) Common.LowerError!Type.FnId {
        const solved_fn_ty = self.solved.types.root(self.solved.fn_tys.items[@intFromEnum(fn_id)]);
        if (self.solved.types.rootContent(solved_fn_ty) != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        return try self.ensureFnSpec(
            fn_id,
            solved_fn_ty,
            .finite,
            try self.ownCaptureSpanForFn(fn_id),
            .{ .erased_callable = capture_ty },
        );
    }

    fn ensureFnSpec(
        self: *Lowerer,
        source: Lifted.FnId,
        solved_fn_ty: SolvedType.TypeVarId,
        abi: CaptureAbi,
        captures: CaptureSpanId,
        return_reuse: ErasedReturnReuse,
    ) Common.LowerError!Type.FnId {
        const capture_items = self.captureSpan(captures);
        const root_fn_ty = self.solved.types.root(solved_fn_ty);
        const spec = FnSpec{
            .source = source,
            .solved_fn_ty = root_fn_ty,
            .abi = abi,
            .captures = captures,
            .capture_ty = if (capture_items.len == 0) null else try self.captureRecordType(captures, root_fn_ty),
            .return_reuse = return_reuse,
        };

        const result = try self.fn_spec_map.getOrPut(spec);
        if (result.found_existing) return result.value_ptr.*;

        const fn_id: Type.FnId = @enumFromInt(@as(u32, @intCast(self.fn_specs.items.len)));
        result.value_ptr.* = fn_id;
        try self.fn_specs.append(self.allocator, spec);
        try self.fn_written.append(self.allocator, false);
        try self.fn_reachable.append(self.allocator, false);
        try self.fn_entries.append(self.allocator, undefined);
        const source_fn = self.solved.lifted.getFn(source);
        const symbol = self.symbols.fresh();
        const fn_content = self.solved.types.rootContent(spec.solved_fn_ty);
        if (fn_content != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        const func = fn_content.func;
        const solved_args = self.solved_types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (solved_args.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");

        const arg_tys = try self.allocator.alloc(Type.TypeId, solved_args.len);
        defer self.allocator.free(arg_tys);
        for (solved_args, 0..) |arg_ty, i| {
            arg_tys[i] = try self.lowerType(arg_ty);
        }

        const ret_ty = try self.lowerType(func.ret);
        const capture_arg_ty = switch (spec.abi) {
            .finite => spec.capture_ty,
            .erased => try self.erasedCapturePtrType(),
        };

        self.fn_entries.items[@intFromEnum(fn_id)] = .{
            .spec = spec,
            .symbol = symbol,
            .source = source_fn.source,
            .args = try self.types.addSpan(arg_tys),
            .ret = ret_ty,
            .capture_arg_ty = capture_arg_ty,
            .proc = null,
        };
        return fn_id;
    }

    fn markReachableFn(self: *Lowerer, fn_id: Type.FnId) Common.LowerError!LIR.LirProcSpecId {
        const index = @intFromEnum(fn_id);
        if (index >= self.fn_entries.items.len) Common.invariant("direct LIR reachability referenced a missing function spec");
        const proc = try self.procPlaceholder(fn_id);
        if (!self.fn_reachable.items[index]) {
            self.fn_reachable.items[index] = true;
            try self.fn_reach_queue.append(self.allocator, fn_id);
        }
        return proc;
    }

    fn procPlaceholder(self: *Lowerer, fn_id: Type.FnId) Common.LowerError!LIR.LirProcSpecId {
        const index = @intFromEnum(fn_id);
        if (self.fn_entries.items[index].proc) |existing| return existing;
        return try self.finalizeFnProc(fn_id);
    }

    fn finalizeFnProc(self: *Lowerer, fn_id: Type.FnId) Common.LowerError!LIR.LirProcSpecId {
        const index = @intFromEnum(fn_id);
        var entry = self.fn_entries.items[index];
        const spec = entry.spec;
        const source_fn = self.solved.lifted.getFn(spec.source);
        if (spec.return_reuse.enabled()) switch (source_fn.body) {
            .roc => {},
            .hosted => Common.invariant("hosted function acquired an internal erased-return reuse ABI"),
        };
        const arg_tys = self.types.span(entry.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (arg_tys.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");

        const arg_count = lifted_args.len + switch (spec.abi) {
            .finite => (if (entry.capture_arg_ty == null) @as(usize, 0) else 1) +
                @as(usize, @intFromBool(spec.return_reuse.enabled())),
            .erased => 2,
        };
        const arg_locals = try self.allocator.alloc(LIR.LocalId, arg_count);
        defer self.allocator.free(arg_locals);

        for (0..arg_tys.len) |i| {
            const arg_ty = GuardedList.at(arg_tys, i);
            arg_locals[i] = try self.addLocalForLayout(try self.layoutOfType(arg_ty));
        }
        if (entry.capture_arg_ty) |capture_arg_ty| {
            arg_locals[lifted_args.len] = try self.addLocalForLayout(try self.layoutOfType(capture_arg_ty));
        }
        if (spec.abi == .finite and spec.return_reuse.enabled()) {
            const reuse_index = lifted_args.len + @as(usize, @intFromBool(entry.capture_arg_ty != null));
            arg_locals[reuse_index] = try self.addLocalForLayout(try self.result.layouts.insertErasedCallable());
        }
        if (spec.abi == .erased) {
            if (spec.return_reuse.enabled()) Common.invariant("erased proc carried a second return-reuse specialization");
            arg_locals[lifted_args.len + 1] = try self.addLocalForLayout(try self.result.layouts.insertErasedCallable());
        }

        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
        self.result.store.current_loc = switch (source_fn.body) {
            .roc => |body_id| self.solved.lifted.exprLoc(body_id),
            .hosted => base.SourceLoc.none,
        };
        self.result.store.current_region = switch (source_fn.body) {
            .roc => |body_id| self.solved.lifted.exprRegion(body_id),
            .hosted => base.Region.zero(),
        };
        const args_span = try self.result.store.addLocalSpan(arg_locals);
        const ret_layout = try self.layoutOfType(entry.ret);
        const erased_arg_layouts = if (spec.abi == .erased)
            try self.appendErasedArgumentLayouts(arg_locals[0..lifted_args.len])
        else
            LIR.BoxySpan.empty();
        const erased_reuse_arg = switch (spec.abi) {
            .erased => arg_locals[lifted_args.len + 1],
            .finite => if (spec.return_reuse.enabled())
                arg_locals[lifted_args.len + @as(usize, @intFromBool(entry.capture_arg_ty != null))]
            else
                null,
        };
        const proc = try self.result.store.addProcSpec(.{
            .name = lirSymbol(entry.symbol),
            .args = args_span,
            .erased_reuse_arg = erased_reuse_arg,
            .erased_call_args = if (spec.abi == .erased)
                try self.erasedCallArgsPlan(arg_locals[0..lifted_args.len])
            else
                null,
            .body = null,
            .ret_layout = ret_layout,
            .erased_arg_layouts = erased_arg_layouts,
            .erased_capture_arg = if (spec.abi == .erased) arg_locals[lifted_args.len] else null,
            .abi = if (spec.abi == .erased) .erased_callable else .roc,
            .hosted = try self.hostedProcForSource(source_fn.source),
            .stack_probe = self.stackProbeForProc(args_span, LIR.LocalSpan.empty(), ret_layout),
        });
        if (self.proc_debug_names) {
            if (self.solved.lifted.procDebugName(source_fn.symbol)) |name| {
                try self.result.store.setProcDebugName(proc, self.solved.lifted.names.exportNameText(name));
            }
        }
        entry.proc = proc;
        self.fn_entries.items[index] = entry;
        return proc;
    }

    fn appendErasedArgumentLayouts(self: *Lowerer, args: []const LIR.LocalId) Common.LowerError!LIR.BoxySpan {
        if (args.len == 0) return .{};
        const start = self.result.boxy_erased_arg_layouts.items.len;
        for (args) |arg| {
            const local_layout = self.result.store.getLocal(arg).layout_idx;
            try self.result.boxy_erased_arg_layouts.append(
                self.allocator,
                self.result.layouts.runtimeRepresentationLayoutIdx(local_layout),
            );
        }
        return .{ .start = @intCast(start), .len = @intCast(args.len) };
    }

    fn sourceFnForSymbol(self: *Lowerer, symbol: Common.Symbol) Lifted.FnId {
        return self.source_symbols.get(symbol) orelse
            Common.invariant("direct Lambda Mono callable member referenced a missing lifted function symbol");
    }

    fn captureSpan(self: *const Lowerer, span: CaptureSpanId) []const SolvedType.Capture {
        return switch (span.source) {
            .solved => self.solved_types.captureSpan(.{ .start = span.start, .len = span.len }),
            .own => self.own_captures.items[span.start..][0..span.len],
        };
    }

    fn ownCaptureSpanForFn(self: *Lowerer, fn_id: Lifted.FnId) std.mem.Allocator.Error!CaptureSpanId {
        const raw_fn = @intFromEnum(fn_id);
        if (raw_fn >= self.own_capture_spans.len) Common.invariant("own capture span requested for a missing lifted function");
        if (self.own_capture_spans[raw_fn]) |existing| return existing;

        const fn_ = self.solved.lifted.getFn(fn_id);
        const capture_locals = self.solved.lifted.typedLocalSpan(fn_.captures);
        if (capture_locals.len == 0) {
            const empty = CaptureSpanId.fromOwn(0, 0);
            self.own_capture_spans[raw_fn] = empty;
            return empty;
        }

        const start: u32 = @intCast(self.own_captures.items.len);
        errdefer self.own_captures.shrinkRetainingCapacity(@intCast(start));

        try self.own_captures.ensureUnusedCapacity(self.allocator, capture_locals.len);
        for (0..capture_locals.len) |index| {
            const capture = GuardedList.at(capture_locals, index);
            const local = self.solved.lifted.getLocal(capture.local);
            self.own_captures.appendAssumeCapacity(.{
                .local = capture.local,
                .symbol = local.symbol,
                .binder = local.binder,
                .capture_id = local.capture_id,
                .checked_capture_id = local.checked_capture_id,
                .ty = self.solved.local_tys.items[@intFromEnum(capture.local)],
            });
        }

        const span = CaptureSpanId.fromOwn(start, @intCast(capture_locals.len));
        self.own_capture_spans[raw_fn] = span;
        return span;
    }

    fn bindCaptureRecord(self: *Lowerer, captures_id: CaptureSpanId, capture_ty: Type.TypeId, source: CaptureSource) Common.LowerError!void {
        const captures = self.captureSpan(captures_id);
        const capture_content = self.types.get(capture_ty);
        if (capture_content != .capture_record) Common.invariant("function capture argument was not a capture record");
        const fields = self.types.captureFieldSpan(capture_content.capture_record);
        if (captures.len != fields.len) Common.invariant("function capture argument arity differed from capture slots");

        for (0..captures.len) |index| {
            const capture = GuardedList.at(captures, index);
            const field = GuardedList.at(fields, index);
            if (capture.capture_id != field.capture_id) {
                Common.invariant("function capture argument fields differed from capture slots");
            }
            try self.captures.put(capture.local, .{
                .source = source,
                .record_ty = capture_ty,
                .symbol = capture.symbol,
                .ty = field.ty,
                .storage_ty = field.storage_ty,
            });
        }
    }

    fn lowerLocalInto(self: *Lowerer, target: LIR.LocalId, local: Lifted.LocalId, ty: Type.TypeId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.typed_local_map.get(.{ .local = local, .ty = ty })) |source| {
            try self.noteLocal(source);
            try self.noteReturnForwardingLocal(target, source);
            return try self.assignTypedBoundary(target, ty, source, ty, next);
        }
        if (self.local_map[@intFromEnum(local)]) |source| {
            try self.noteLocal(source);
            const source_ty = try self.lowerLocalTy(local);
            try self.noteReturnForwardingLocal(target, source);
            return try self.assignTypedBoundary(target, ty, source, source_ty, next);
        }
        // A current lexical binding is authoritative over an enclosing capture.
        // Wrapper inlining installs the callee arguments in `local_map`; nested
        // functions can otherwise resolve those exact locals through their own
        // capture table and incorrectly read the pre-call values.
        if (try self.captureBindingForLocal(local)) |capture| {
            return try self.lowerCaptureBindingInto(target, capture, next);
        }
        const source = try self.bindUnboundLocalForTarget(local, ty, target);
        const source_ty = try self.lowerLocalTy(local);
        try self.noteReturnForwardingLocal(target, source);
        return try self.assignTypedBoundary(target, ty, source, source_ty, next);
    }

    fn noteReturnForwardingLocal(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
    ) Common.LowerError!void {
        if (self.return_forwarding_repeatable_depth != 0) return;
        if (self.current_return_target != target) return;
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const source_layout = self.result.store.getLocal(source).layout_idx;
        // Equal committed layouts make assignTypedBoundary emit one explicit
        // local alias. Layout similarity alone is not allocation provenance.
        if (target_layout != source_layout) return;
        const existing_candidates = self.return_forwarding_locals.count();
        const candidate = try self.return_forwarding_locals.getOrPut(source);
        if (!candidate.found_existing) {
            candidate.value_ptr.* = {};
            if (existing_candidates != 0) self.return_forwarding_ambiguous = true;
        }
    }

    fn captureBindingForLocal(self: *Lowerer, local: Lifted.LocalId) Common.LowerError!?CaptureBinding {
        if (self.captures.get(local)) |capture| return capture;

        var found: ?CaptureBinding = null;
        var iterator = self.captures.iterator();
        while (iterator.next()) |entry| {
            if (!self.captureLocalMatchesLocal(local, entry.key_ptr.*)) continue;
            if (!try self.liftedLocalTypesMatch(local, entry.key_ptr.*)) continue;
            if (found != null) Common.invariant("lifted local matched multiple capture bindings");
            found = entry.value_ptr.*;
        }
        return found;
    }

    fn captureLocalMatchesLocal(self: *Lowerer, local_id: Lifted.LocalId, capture_local: Lifted.LocalId) bool {
        if (local_id == capture_local) return true;

        // A body reference and a capture slot name the same captured binding
        // iff they carry the same CaptureId (the exact join key that
        // survives specialization/cloning). This is only used to search a
        // function's own capture set, where CaptureIds are unique.
        const local = self.solved.lifted.getLocal(local_id);
        const capture = self.solved.lifted.getLocal(capture_local);
        return local.capture_id != null and capture.capture_id != null and local.capture_id.? == capture.capture_id.?;
    }

    /// Whether two lifted locals are aliases of the same lowered binding, used
    /// to collapse locals that spec_constr cloning produced for one value. This
    /// is a general local-identity remap, NOT a capture join: it must match by
    /// the per-binding symbol, because CaptureId is deliberately coarser than a
    /// symbol (a mutable variable's loop-carried instances all share one
    /// captured-binder CaptureId yet are distinct lowered bindings).
    fn aliasedLocalMatches(self: *Lowerer, local_id: Lifted.LocalId, other: Lifted.LocalId) bool {
        if (local_id == other) return true;
        const local = self.solved.lifted.getLocal(local_id);
        const alias = self.solved.lifted.getLocal(other);
        return local.symbol == alias.symbol;
    }

    fn lowerCaptureBindingInto(self: *Lowerer, target: LIR.LocalId, capture: CaptureBinding, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const field_index = self.captureFieldIndex(capture.record_ty, capture.symbol);
        if (self.isZstLocal(target)) return try self.assignZst(target, next);

        const record_local = switch (capture.source) {
            .record => |local| local,
            .erased_ptr => try self.addTemp(capture.record_ty),
        };
        const assign = try self.assignTypedRefRead(
            target,
            capture.ty,
            capture.storage_ty,
            self.localFieldLayout(record_local, field_index),
            .{ .field = .{ .source = record_local, .field_idx = field_index } },
            next,
        );
        return switch (capture.source) {
            .record => assign,
            .erased_ptr => |ptr| try self.lowerErasedCaptureLoadInto(record_local, ptr, assign),
        };
    }

    fn lowerErasedCaptureLoadInto(self: *Lowerer, target: LIR.LocalId, ptr: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .erased_capture_load,
            .rc_effect = LIR.LowLevel.erased_capture_load.rcEffect(),
            .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{ptr}),
            .next = next,
        } });
    }

    /// Snapshot an erased frame's capture record before its callable
    /// allocation can be consumed as a result destination. `owner` is an
    /// ARC-only sequencing anchor: backends read capture bytes through `ptr`,
    /// while the explicit owner operand keeps the allocation live through the
    /// copy. `retain_result` then gives the snapshot independent ownership
    /// before later statements may consume the owner.
    fn lowerAnchoredErasedCaptureLoadInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ptr: LIR.LocalId,
        owner: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .erased_capture_load,
            .rc_effect = LIR.LowLevel.RcEffect.retainsResult(),
            .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{ ptr, owner }),
            .next = next,
        } });
    }

    fn memberCapturesForExpr(self: *Lowerer, expr_id: Lifted.ExprId, fn_id: Lifted.FnId) CaptureSpanId {
        const fn_symbol = self.solved.lifted.getFn(fn_id).symbol;
        const expr_ty = self.solved.expr_tys.items[@intFromEnum(expr_id)];
        const expr_content = self.solved.types.rootContent(expr_ty);
        const callable = if (expr_content == .func)
            expr_content.func.callable
        else if (expr_content == .lambda_set or expr_content == .erased)
            expr_ty
        else
            Common.invariant("function reference expression had no callable Lambda Solved type");
        const callable_content = self.solved.types.rootContent(callable);
        const members = if (callable_content == .lambda_set)
            callable_content.lambda_set
        else if (callable_content == .erased)
            callable_content.erased.members
        else
            Common.invariant("function reference callable slot was unresolved before direct Lambda Mono");
        for (self.solved_types.memberSpan(members)) |member| {
            if (member.lambda == fn_symbol) return CaptureSpanId.fromSolved(member.captures);
        }
        Common.invariant("function reference callable slot did not contain referenced function");
    }

    fn capturesForFn(self: *Lowerer, fn_id: Lifted.FnId) std.mem.Allocator.Error!CaptureSpanId {
        return try self.ownCaptureSpanForFn(fn_id);
    }

    fn erasedCapturePtrType(self: *Lowerer) Common.LowerError!Type.TypeId {
        if (self.erased_capture_ptr_ty) |ty| return ty;
        const ty = try self.types.add(.erased_capture_ptr);
        self.erased_capture_ptr_ty = ty;
        return ty;
    }

    fn captureFieldStorageType(self: *Lowerer, capture: SolvedType.Capture, ty: Type.TypeId) Common.LowerError!Type.TypeId {
        if (!self.captureUsesRecursiveSlotStorage(capture)) return ty;
        if (try self.solvedTypeAlreadyHasRecursiveSlotStorage(capture.ty)) return ty;
        return try self.boxedRecursiveSlotTypeOfType(ty);
    }

    fn captureUsesRecursiveSlotStorage(self: *Lowerer, capture: SolvedType.Capture) bool {
        if (self.recursive_value_locals.contains(capture.local)) return true;
        if (capture.capture_id) |capture_id| {
            if (self.recursive_value_capture_ids.contains(capture_id)) return true;
        }
        if (capture.checked_capture_id) |capture_id| {
            if (self.recursive_value_capture_ids.contains(capture_id)) return true;
        }
        return false;
    }

    fn captureRecordType(
        self: *Lowerer,
        captures: CaptureSpanId,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Common.LowerError!Type.TypeId {
        const id = CaptureTypeKey.from(captures, self.solved.types.root(solved_fn_ty));
        if (self.capture_types.get(id)) |existing| return existing;

        const capture_items = self.captureSpan(captures);
        const fields = try self.allocator.alloc(Type.CaptureField, capture_items.len);
        defer self.allocator.free(fields);
        for (capture_items, 0..) |capture, i| {
            const ty = try self.lowerType(capture.ty);
            fields[i] = .{
                .symbol = capture.symbol,
                .binder = capture.binder,
                .capture_id = capture.capture_id,
                .checked_capture_id = capture.checked_capture_id,
                .ty = ty,
                .storage_ty = try self.captureFieldStorageType(capture, ty),
            };
        }
        const ty = try self.types.add(.{ .capture_record = try self.types.addCaptureFields(fields) });
        try self.capture_types.put(id, ty);
        return ty;
    }

    fn lowerExprTy(self: *Lowerer, expr_id: Lifted.ExprId) Common.LowerError!Type.TypeId {
        return try self.lowerType(self.solved.expr_tys.items[@intFromEnum(expr_id)]);
    }

    fn lowerExprContextTy(self: *Lowerer, expr_id: Lifted.ExprId) Common.LowerError!Type.TypeId {
        const expr = self.solved.lifted.getExpr(expr_id);
        if (expr.data == .field_access) {
            const field = expr.data.field_access;
            var prefix_ty = try self.lowerExprContextTy(field.receiver);
            const segments = self.solved.lifted.fieldAccessSegmentSpan(field.segments);
            if (segments.len == 0) Common.invariant("field access path had no segments");
            for (0..segments.len) |index| {
                const segment = GuardedList.at(segments, index);
                prefix_ty = self.recordFieldType(prefix_ty, segment.field);
            }
            return prefix_ty;
        }
        if (expr.data == .tuple_access) {
            const access = expr.data.tuple_access;
            const items = self.tupleItemTypes(try self.lowerExprContextTy(access.tuple));
            if (access.elem_index >= items.len) Common.invariant("tuple access index exceeded tuple type");
            return GuardedList.at(items, @intCast(access.elem_index));
        }
        return try self.lowerExprTy(expr_id);
    }

    fn lowerPatTy(self: *Lowerer, pat_id: Lifted.PatId) Common.LowerError!Type.TypeId {
        return try self.lowerType(self.solved.pat_tys.items[@intFromEnum(pat_id)]);
    }

    fn lowerLocalTy(self: *Lowerer, local: Lifted.LocalId) Common.LowerError!Type.TypeId {
        return try self.lowerType(self.solved.local_tys.items[@intFromEnum(local)]);
    }

    fn lowerType(self: *Lowerer, solved_ty: SolvedType.TypeVarId) Common.LowerError!Type.TypeId {
        const root = self.solved.types.root(solved_ty);
        if (self.type_map.get(root)) |cached| return cached;

        const content = self.solved.types.get(root);
        if (content == .func) {
            const reserved = try self.types.add(.zst);
            try self.type_map.put(root, reserved);
            try self.callable_source_fn_map.put(reserved, root);
            errdefer {
                if (self.type_map.get(root) == reserved) _ = self.type_map.remove(root);
                if (self.callable_source_fn_map.get(reserved) == root) _ = self.callable_source_fn_map.remove(reserved);
            }
            self.types.set(reserved, try self.lowerCallableForFn(content.func.callable, root));
            return reserved;
        }

        const reserved = try self.types.add(.zst);
        try self.type_map.put(root, reserved);
        self.types.set(reserved, try self.lowerTypeContent(content));
        return reserved;
    }

    fn lowerTypeContent(self: *Lowerer, content: SolvedType.Content) Common.LowerError!Type.Content {
        return switch (content) {
            .link => Common.invariant("direct Lambda Mono type lowering saw an unresolved Lambda Solved link"),
            .unbound, .forall => Common.invariant("direct Lambda Mono type lowering saw an unresolved Lambda Solved type"),
            .mono => Common.invariant("direct Lambda Mono type lowering saw an unfinalized lazy Monotype leaf"),
            .primitive => |primitive| .{ .primitive = primitive },
            .zst => .zst,
            .erased => |erased| .{ .erased_fn = .{
                .source_fn_ty = erased.source_fn_ty,
                .members = try self.lowerFnMembersFromOwnTypes(erased.members, .erased),
            } },
            .func => Common.invariant("function type reached content lowering without its call signature"),
            .list => |elem| .{ .list = try self.lowerType(elem) },
            .box => |elem| .{ .box = try self.lowerType(elem) },
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSpan(self.solved_types.span(items));
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.types.addSpan(lowered) };
            },
            .record => |fields| blk: {
                const lowered = try self.allocator.alloc(Type.Field, fields.len);
                defer self.allocator.free(lowered);
                for (self.solved_types.fieldSpan(fields), 0..) |field, i| {
                    lowered[i] = .{
                        .name = field.name,
                        .ty = try self.lowerType(field.ty),
                        .value_ty = if (field.value_ty) |value_ty| try self.lowerType(value_ty) else null,
                        .default = field.default,
                    };
                }
                break :blk .{ .record = try self.types.addFields(lowered) };
            },
            .tag_union => |tags| blk: {
                const lowered = try self.allocator.alloc(Type.Tag, tags.len);
                defer self.allocator.free(lowered);
                for (self.solved_types.tagSpan(tags), 0..) |tag, i| {
                    const payloads = try self.lowerTypeSpan(self.solved_types.span(tag.payloads));
                    defer self.allocator.free(payloads);
                    lowered[i] = .{
                        .name = tag.name,
                        .checked_name = tag.checked_name,
                        .payloads = try self.types.addSpan(payloads),
                    };
                }
                break :blk .{ .tag_union = try self.types.addTags(lowered) };
            },
            .named => |named| blk: {
                const args = try self.lowerTypeSpan(self.solved_types.span(named.args));
                defer self.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = named.named_type,
                    .def = named.def,
                    .kind = named.kind,
                    .builtin_owner = named.builtin_owner,
                    .args = try self.types.addSpan(args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.lowerType(backing.ty),
                        .use = backing.use,
                        .authority = backing.authority,
                    } else null,
                    .declared_order = try self.lowerDeclaredOrder(named.declared_order),
                } };
            },
            .lambda_set => |members| .{ .callable = try self.lowerFnMembersFromOwnTypes(members, .finite) },
        };
    }

    fn lowerCallableForFn(
        self: *Lowerer,
        callable: SolvedType.TypeVarId,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Common.LowerError!Type.Content {
        const content = self.solved.types.rootContent(callable);
        if (content == .lambda_set) {
            return .{ .callable = try self.lowerFnMembers(content.lambda_set, .finite, solved_fn_ty) };
        } else if (content == .erased) {
            return .{ .erased_fn = .{
                .source_fn_ty = content.erased.source_fn_ty,
                .members = try self.lowerFnMembers(content.erased.members, .erased, solved_fn_ty),
            } };
        } else {
            return Common.invariant("function callable slot was unresolved before direct Lambda Mono");
        }
    }

    /// Re-materializes a nominal record's declared field order from the Lambda
    /// Solved store into this lowerer's Lambda Mono store. Named entries copy the
    /// shared field-name id; padding entries re-lower their reserved type.
    fn lowerDeclaredOrder(self: *Lowerer, span: SolvedType.Span) Common.LowerError!Type.Span {
        const source = self.solved_types.declaredFieldSpan(span);
        if (source.len == 0) return Type.Span.empty();
        const lowered = try self.allocator.alloc(Type.DeclaredField, source.len);
        defer self.allocator.free(lowered);
        for (source, 0..) |entry, i| {
            lowered[i] = switch (entry) {
                .named => |name| .{ .named = name },
                .padding => |ty| .{ .padding = try self.lowerType(ty) },
            };
        }
        return try self.types.addDeclaredFields(lowered);
    }

    fn lowerFnMembers(
        self: *Lowerer,
        members: SolvedType.Span,
        abi: CaptureAbi,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Common.LowerError!Type.Span {
        const solved_members = self.solved_types.memberSpan(members);
        const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
        defer self.allocator.free(variants);
        const root_fn_ty = self.solved.types.root(solved_fn_ty);
        for (solved_members, 0..) |member, i| {
            const source = self.sourceFnForSymbol(member.lambda);
            const target = try self.ensureFnSpec(
                source,
                root_fn_ty,
                abi,
                CaptureSpanId.fromSolved(member.captures),
                .none,
            );
            variants[i] = .{
                .id = undefined,
                .source = member.lambda,
                .target = target,
                .capture_ty = self.fn_entries.items[@intFromEnum(target)].spec.capture_ty,
            };
        }
        return try self.types.addFnVariants(variants);
    }

    fn lowerFnMembersFromOwnTypes(self: *Lowerer, members: SolvedType.Span, abi: CaptureAbi) Common.LowerError!Type.Span {
        const solved_members = self.solved_types.memberSpan(members);
        const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
        defer self.allocator.free(variants);
        for (solved_members, 0..) |member, i| {
            const source = self.sourceFnForSymbol(member.lambda);
            const target = try self.ensureFnSpec(
                source,
                self.solved.types.root(self.solved.fn_tys.items[@intFromEnum(source)]),
                abi,
                CaptureSpanId.fromSolved(member.captures),
                .none,
            );
            variants[i] = .{
                .id = undefined,
                .source = member.lambda,
                .target = target,
                .capture_ty = self.fn_entries.items[@intFromEnum(target)].spec.capture_ty,
            };
        }
        return try self.types.addFnVariants(variants);
    }

    fn lowerTypeSpan(self: *Lowerer, items: []const SolvedType.TypeVarId) Common.LowerError![]Type.TypeId {
        const lowered = try self.allocator.alloc(Type.TypeId, items.len);
        errdefer self.allocator.free(lowered);
        for (items, 0..) |item, i| lowered[i] = try self.lowerType(item);
        return lowered;
    }

    fn stringLiteralText(self: *const Lowerer, id: Lifted.StringLiteralId) []const u8 {
        return self.stringLiteral(id).text();
    }

    fn stringLiteral(self: *const Lowerer, id: Lifted.StringLiteralId) Mono.StringLiteral {
        return self.solved.lifted.getStringLiteral(id);
    }

    fn lirStrLiteral(self: *Lowerer, id: Lifted.StringLiteralId) Common.LowerError!LIR.StrLiteral {
        const str_lit = self.stringLiteral(id);
        return try self.result.store.insertStringView(str_lit.backing, str_lit.offset, str_lit.len);
    }

    fn noteLocal(self: *Lowerer, local: LIR.LocalId) Common.LowerError!void {
        if (self.current_proc_locals) |locals| {
            try locals.put(self.allocator, @intFromEnum(local), {});
        }
    }

    fn noteLocalSpan(self: *Lowerer, span: LIR.LocalSpan) Common.LowerError!void {
        const locals = self.result.store.getLocalSpan(span);
        for (0..locals.len) |index| {
            const local = GuardedList.at(locals, index);
            try self.noteLocal(local);
        }
    }

    fn lowerComptimeSite(self: *Lowerer, site: Lifted.ComptimeSiteId) Common.LowerError!LIR.ComptimeSiteId {
        const index = @intFromEnum(site);
        if (self.comptime_site_map[index]) |existing| return existing;
        const proc = self.current_proc orelse Common.invariant("compile-time site reached direct LIR lowering outside a proc");
        const source = self.solved.lifted.comptimeSite(site);
        const lowered = try self.result.addComptimeSite(switch (source.kind) {
            .match => .match,
            .destructure => .destructure,
            .if_ => .if_,
        }, source.region, source.checked_site, proc, source.branch_regions);
        self.comptime_site_map[index] = lowered;
        return lowered;
    }

    fn writeFrameLocals(self: *Lowerer, locals: *ProcLocalSet) Common.LowerError!LIR.LocalSpan {
        const raw_ids = locals.keys();
        const sorted = try self.allocator.alloc(LIR.LocalId, raw_ids.len);
        defer self.allocator.free(sorted);
        for (raw_ids, 0..) |raw_id, i| {
            sorted[i] = @enumFromInt(raw_id);
        }
        std.mem.sort(LIR.LocalId, sorted, {}, localIdLessThan);
        return try self.result.store.addLocalSpan(sorted);
    }

    fn stackProbeForProc(self: *Lowerer, args: LIR.LocalSpan, frame_locals: LIR.LocalSpan, ret_layout: layout.Idx) LIR.StackProbe {
        if (self.result.store.localSpanNeedsStackProbe(&self.result.layouts, args)) return .required;
        if (self.result.store.localSpanNeedsStackProbe(&self.result.layouts, frame_locals)) return .required;
        if (LIR.layoutNeedsStackProbe(&self.result.layouts, ret_layout)) return .required;
        return .default;
    }

    fn localIdLessThan(_: void, a: LIR.LocalId, b: LIR.LocalId) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }

    fn bindRoots(self: *Lowerer) Common.LowerError!void {
        for (self.roots.items) |root| {
            const entry = self.fn_entries.items[@intFromEnum(root.fn_id)];
            const proc = try self.markReachableFn(root.fn_id);
            try self.result.root_procs.append(self.allocator, proc);
            var metadata = RootMetadata.fromCheckedRoot(root.request);
            metadata.test_plan = Common.testPlanMetadataForRoot(self.root_requests, root.request);
            try self.result.root_metadata.append(self.allocator, metadata);
            if (root.request.abi == .compile_time) {
                try self.result.const_roots.append(self.allocator, .{
                    .root_order = root.request.order,
                    .request = root.request,
                    .proc = proc,
                    .ret_layout = try self.layoutOfType(entry.ret),
                    .ret_type = try self.constTypeOfType(entry.ret),
                    .plan = try self.constPlanOfType(entry.ret),
                });
            }
        }

        for (self.layout_requests.items) |request| {
            const initializer = if (request.initializer) |initializer_fn| blk: {
                const proc = try self.markReachableFn(initializer_fn);
                self.result.store.getProcSpecPtr(proc).is_static_initializer = true;
                break :blk proc;
            } else null;
            try self.result.requested_layouts.append(self.allocator, .{
                .ty = self.types.typeDigest(&self.solved.lifted.names, request.ty),
                .checked_type = request.checked_type,
                .const_locator = request.const_locator,
                .layout_idx = try self.layoutOfType(request.ty),
                .plan = if (initializer != null)
                    try self.layoutOnlyConstPlan()
                else if (self.layout_request_const_plans)
                    try self.constPlanOfType(request.ty)
                else
                    try self.layoutOnlyConstPlan(),
                .initializer = initializer,
            });
        }
    }

    fn layoutOnlyConstPlan(self: *Lowerer) Common.LowerError!LirProgram.ConstPlanId {
        const id: LirProgram.ConstPlanId = @enumFromInt(@as(u32, @intCast(self.result.const_plans.items.len)));
        try self.result.const_plans.append(self.allocator, .layout_only);
        return id;
    }

    fn constPlanOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LirProgram.ConstPlanId {
        if (self.const_plan_map.get(ty)) |existing| return existing;

        const content = self.types.get(ty);
        if (content == .named and content.named.kind == .alias) {
            // Aliases are transparent values. Reuse the backing plan so
            // ConstStore never records an alias as a nominal wrapper.
            const backing = content.named.backing orelse Common.invariant("alias without backing reached ConstStore plan output");
            const backing_plan = try self.constPlanOfType(backing.ty);
            try self.const_plan_map.put(ty, backing_plan);
            return backing_plan;
        }

        const id: LirProgram.ConstPlanId = @enumFromInt(@as(u32, @intCast(self.result.const_plans.items.len)));
        try self.result.const_plans.append(self.allocator, .pending);
        try self.const_plan_map.put(ty, id);
        errdefer {
            if (self.const_plan_map.get(ty) == id) _ = self.const_plan_map.remove(ty);
        }

        const plan = try self.buildConstPlan(ty);
        self.result.const_plans.items[@intFromEnum(id)] = plan;
        return id;
    }

    fn lirStaticDataFor(
        self: *Lowerer,
        candidate: Mono.StaticDataCandidate,
        ty: Type.TypeId,
        layout_idx: layout.Idx,
    ) Common.LowerError!LIR.StaticDataId {
        const request = StaticInitializerRequest{
            .static_data = candidate.static_data,
            .layout_idx = layout_idx,
        };
        const gop = try self.static_initializer_map.getOrPut(request);
        if (gop.found_existing) return gop.value_ptr.*;

        const proc = try self.result.store.addProcSpec(.{
            .name = lirSymbol(self.symbols.fresh()),
            .args = LIR.LocalSpan.empty(),
            .body = null,
            .ret_layout = layout_idx,
            .abi = .roc,
            .is_static_initializer = true,
        });
        const result_id: LIR.StaticDataId = @enumFromInt(@as(u32, @intCast(self.result.static_data_values.items.len)));
        try self.result.static_data_values.append(self.allocator, .{
            .initializer = proc,
        });
        try self.static_initializer_queue.append(self.allocator, .{
            .expr = candidate.runtime_expr,
            .ty = ty,
            .proc = proc,
        });
        gop.value_ptr.* = result_id;
        return result_id;
    }

    fn layoutNeedsStaticData(self: *Lowerer, layout_idx: layout.Idx) bool {
        const layout_data = self.result.layouts.getLayout(layout_idx);
        if (self.result.layouts.layoutSize(layout_data) == 0) return false;
        return self.result.layouts.layoutContainsRefcounted(layout_data);
    }

    fn buildConstPlan(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LirProgram.ConstPlan {
        return switch (self.types.get(ty)) {
            .primitive => |primitive| switch (primitive) {
                .bool => Common.invariant("primitive Bool reached ConstStore plan output; Bool must be a checked named tag union"),
                .str => .str,
                .u8,
                .i8,
                .u16,
                .i16,
                .u32,
                .i32,
                .u64,
                .i64,
                .u128,
                .i128,
                .f32,
                .f64,
                .dec,
                .u8x16,
                .i8x16,
                .u16x8,
                .i16x8,
                .u32x4,
                .i32x4,
                .u64x2,
                .i64x2,
                => .scalar,
            },
            .zst => .zst,
            .list => |elem| .{ .list = try self.constPlanOfType(elem) },
            .box => |elem| .{ .box = try self.constPlanOfType(elem) },
            .tuple => |items| blk: {
                const plans = try self.allocator.alloc(LirProgram.ConstPlanId, items.len);
                errdefer self.allocator.free(plans);
                for (0..items.len) |i| plans[i] = try self.constPlanOfType(self.types.typeAt(items, i));
                break :blk .{ .tuple = plans };
            },
            .record => |fields| blk: {
                const plans = try self.allocator.alloc(LirProgram.ConstPlanId, fields.len);
                errdefer self.allocator.free(plans);
                for (0..fields.len) |i| {
                    const field = self.types.fieldAt(fields, i);
                    plans[i] = try self.constPlanOfType(field.ty);
                }
                break :blk .{ .record = plans };
            },
            .tag_union => |tags| blk: {
                const variants = try self.allocator.alloc(LirProgram.ConstTagVariant, tags.len);
                var initialized: usize = 0;
                errdefer {
                    for (variants[0..initialized]) |variant| {
                        self.allocator.free(variant.name);
                        self.allocator.free(variant.payloads);
                    }
                    self.allocator.free(variants);
                }
                for (0..tags.len) |i| {
                    const tag = self.types.tagAt(tags, i);
                    const name = try self.allocator.dupe(u8, self.solved.lifted.names.tagLabelText(tag.name));
                    errdefer self.allocator.free(name);
                    const payloads = try self.allocator.alloc(LirProgram.ConstPlanId, tag.payloads.len);
                    var payloads_owned = true;
                    errdefer if (payloads_owned) self.allocator.free(payloads);
                    for (0..tag.payloads.len) |j| payloads[j] = try self.constPlanOfType(self.types.typeAt(tag.payloads, j));
                    variants[i] = .{
                        .name = name,
                        .checked_name = tag.checked_name,
                        .discriminant = @intCast(i),
                        .payloads = payloads,
                    };
                    payloads_owned = false;
                    initialized += 1;
                }
                break :blk .{ .tag_union = variants };
            },
            .named => |named| blk: {
                const backing = named.backing orelse Common.invariant("named type without backing reached ConstStore plan output");
                break :blk .{ .named = .{
                    .named_type = .{
                        .module = named.named_type.module,
                        .ty = named.named_type.ty,
                    },
                    .backing = try self.constPlanOfType(backing.ty),
                } };
            },
            .callable => |variants| .{ .fn_value = try self.fnSetForType(ty, variants) },
            .erased_fn => |erased| .{ .erased_fn = try self.erasedFnsForType(ty, erased) },
            .capture_record => Common.invariant("capture record reached root ConstStore plan output"),
            .erased_capture_ptr => Common.invariant("erased capture pointer reached root ConstStore plan output"),
        };
    }

    fn constTypeOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!const_store.ConstTypeId {
        if (self.const_type_map.get(ty)) |existing| return existing;

        const id = try self.result.const_types.reserve();
        try self.const_type_map.put(ty, id);
        errdefer {
            if (self.const_type_map.get(ty) == id) _ = self.const_type_map.remove(ty);
        }

        const stored = try self.buildConstType(ty);
        self.result.const_types.fill(id, stored);
        return id;
    }

    fn constRecordFieldName(self: *Lowerer, name: check.CheckedNames.RecordFieldNameId) std.mem.Allocator.Error!check.CheckedNames.RecordFieldNameId {
        return self.result.const_type_names.internRecordFieldLabel(self.solved.lifted.names.recordFieldLabelText(name));
    }

    fn constTagName(self: *Lowerer, name: check.CheckedNames.TagNameId) std.mem.Allocator.Error!check.CheckedNames.TagNameId {
        return self.result.const_type_names.internTagLabel(self.solved.lifted.names.tagLabelText(name));
    }

    fn constFieldDefault(self: *Lowerer, default: ?MonoType.FieldDefault) std.mem.Allocator.Error!?const_store.TypeFieldDefault {
        const field_default = default orelse return null;
        return .{
            .module = try self.result.const_type_names.internModuleIdentity(self.solved.lifted.names.moduleIdentityBytes(field_default.module)),
            .expr_node = field_default.expr_node,
        };
    }

    fn constTypeDef(self: *Lowerer, def: MonoType.TypeDef) std.mem.Allocator.Error!const_store.TypeDef {
        return .{
            .module = try self.result.const_type_names.internModuleIdentity(self.solved.lifted.names.moduleIdentityBytes(def.module)),
            .type_name = try self.result.const_type_names.internTypeName(self.solved.lifted.names.typeNameText(def.type_name)),
            .source_decl = def.source_decl,
            .generated = def.generated,
            .iterator_representation = @enumFromInt(@intFromEnum(def.iterator_representation)),
            .iterator_kind = @enumFromInt(@intFromEnum(def.iterator_kind)),
            .iterator_depth = def.iterator_depth,
            .iterator_topology = if (def.iterator_topology) |topology| .{
                .len_field = try self.constRecordFieldName(topology.len_field),
                .step_field = try self.constRecordFieldName(topology.step_field),
                .known_tag = try self.constTagName(topology.known_tag),
                .unknown_tag = try self.constTagName(topology.unknown_tag),
                .done_tag = try self.constTagName(topology.done_tag),
                .one_tag = try self.constTagName(topology.one_tag),
                .skip_tag = try self.constTagName(topology.skip_tag),
                .item_field = try self.constRecordFieldName(topology.item_field),
                .rest_field = try self.constRecordFieldName(topology.rest_field),
            } else null,
        };
    }

    fn buildConstType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!const_store.ConstType {
        return switch (self.types.get(ty)) {
            .primitive => |primitive| .{ .primitive = constPrimitive(primitive) },
            .zst => .zst,
            .list => |elem| .{ .list = try self.constTypeOfType(elem) },
            .box => |elem| .{ .box = try self.constTypeOfType(elem) },
            .tuple => |items| blk: {
                const out = try self.allocator.alloc(const_store.ConstTypeId, items.len);
                defer self.allocator.free(out);
                for (0..items.len) |i| out[i] = try self.constTypeOfType(self.types.typeAt(items, i));
                break :blk .{ .tuple = try self.result.const_types.appendTypeSpan(out) };
            },
            .record => |fields| blk: {
                const out = try self.allocator.alloc(const_store.TypeField, fields.len);
                defer self.allocator.free(out);
                for (0..fields.len) |i| {
                    const field = self.types.fieldAt(fields, i);
                    out[i] = .{
                        .name = try self.constRecordFieldName(field.name),
                        .ty = try self.constTypeOfType(field.ty),
                        .value_ty = if (field.value_ty) |value_ty|
                            try self.constTypeOfType(value_ty)
                        else
                            null,
                        .default = try self.constFieldDefault(field.default),
                    };
                }
                break :blk .{ .record = try self.result.const_types.appendFieldSpan(out) };
            },
            .tag_union => |tags| blk: {
                const out = try self.allocator.alloc(const_store.TypeTag, tags.len);
                defer self.allocator.free(out);
                for (0..tags.len) |i| {
                    const tag = self.types.tagAt(tags, i);
                    const stored_payloads = try self.allocator.alloc(const_store.ConstTypeId, tag.payloads.len);
                    defer self.allocator.free(stored_payloads);
                    for (0..tag.payloads.len) |j| stored_payloads[j] = try self.constTypeOfType(self.types.typeAt(tag.payloads, j));
                    out[i] = .{
                        .name = try self.constTagName(tag.name),
                        .checked_name = try self.constTagName(tag.checked_name),
                        .payloads = try self.result.const_types.appendTypeSpan(stored_payloads),
                    };
                }
                break :blk .{ .tag_union = try self.result.const_types.appendTagSpan(out) };
            },
            .named => |named| blk: {
                const stored_args = try self.allocator.alloc(const_store.ConstTypeId, named.args.len);
                defer self.allocator.free(stored_args);
                for (0..named.args.len) |i| stored_args[i] = try self.constTypeOfType(self.types.typeAt(named.args, i));

                const stored_declared = try self.allocator.alloc(const_store.TypeDeclaredField, named.declared_order.len);
                defer self.allocator.free(stored_declared);
                for (0..named.declared_order.len) |i| {
                    const entry = self.types.declaredFieldAt(named.declared_order, i);
                    stored_declared[i] = switch (entry) {
                        .named => |name| .{ .named = try self.constRecordFieldName(name) },
                        .padding => |padding| .{ .padding = try self.constTypeOfType(padding) },
                    };
                }

                break :blk .{ .named = .{
                    .named_type = .{
                        .module = named.named_type.module,
                        .ty = named.named_type.ty,
                    },
                    .def = try self.constTypeDef(named.def),
                    .kind = constNamedKind(named.kind),
                    .builtin_owner = named.builtin_owner,
                    .args = try self.result.const_types.appendTypeSpan(stored_args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.constTypeOfType(backing.ty),
                        .use = constBackingUse(backing.use),
                        .authority = constBackingAuthority(backing.authority),
                    } else null,
                    .declared_order = try self.result.const_types.appendDeclaredFieldSpan(stored_declared),
                } };
            },
            // The solved source signature below is the durable authority for
            // both callable shapes; runtime variant sets are irrelevant here
            // and may legitimately be empty when every member is unreachable.
            .callable, .erased_fn => return try self.constFuncTypeForCallableSource(ty),
            .capture_record => Common.invariant("capture record reached ConstStore type output as a captured value"),
            .erased_capture_ptr => Common.invariant("erased capture pointer reached ConstStore type output as a captured value"),
        };
    }

    /// A callable type's solved function node is the explicit common source
    /// signature for every runtime variant. Individual variants may have
    /// distinct specialization-private Monotype signatures, so neither one
    /// variant nor an equality check across those signatures can define the
    /// durable ConstStore function type.
    fn constFuncTypeForCallableSource(self: *Lowerer, ty: Type.TypeId) Common.LowerError!const_store.ConstType {
        const solved_fn_ty = self.callable_source_fn_map.get(ty) orelse
            Common.invariant("callable const type lacked its solved source function type");
        const fn_content = self.solved.types.rootContent(solved_fn_ty);
        if (fn_content != .func) Common.invariant("callable source type was not a function");
        const func = fn_content.func;

        const source_args = self.solved_types.span(func.args);
        const stored_args = try self.allocator.alloc(const_store.ConstTypeId, source_args.len);
        defer self.allocator.free(stored_args);
        for (source_args, 0..) |arg, i| {
            stored_args[i] = try self.constTypeOfType(try self.lowerType(arg));
        }

        return .{ .func = .{
            .args = try self.result.const_types.appendTypeSpan(stored_args),
            .ret = try self.constTypeOfType(try self.lowerType(func.ret)),
        } };
    }

    fn constTypeOfMonoType(self: *Lowerer, ty: MonoType.TypeId) Common.LowerError!const_store.ConstTypeId {
        if (self.mono_const_type_map.get(ty)) |existing| return existing;

        const id = try self.result.const_types.reserve();
        try self.mono_const_type_map.put(ty, id);
        errdefer {
            if (self.mono_const_type_map.get(ty) == id) _ = self.mono_const_type_map.remove(ty);
        }

        const stored = try self.buildConstTypeFromMono(ty);
        self.result.const_types.fill(id, stored);
        return id;
    }

    fn buildConstTypeFromMono(self: *Lowerer, ty: MonoType.TypeId) Common.LowerError!const_store.ConstType {
        const mono_types = &self.solved.lifted.types;
        return switch (mono_types.get(ty)) {
            .primitive => |primitive| .{ .primitive = constPrimitive(primitive) },
            .zst => .zst,
            .erased => |erased| .{ .erased = erased },
            .list => |elem| .{ .list = try self.constTypeOfMonoType(elem) },
            .box => |elem| .{ .box = try self.constTypeOfMonoType(elem) },
            .tuple => |items| blk: {
                const source = mono_types.span(items);
                const out = try self.allocator.alloc(const_store.ConstTypeId, source.len);
                defer self.allocator.free(out);
                for (0..source.len) |i| out[i] = try self.constTypeOfMonoType(GuardedList.at(source, i));
                break :blk .{ .tuple = try self.result.const_types.appendTypeSpan(out) };
            },
            .func => |function| blk: {
                const args = mono_types.span(function.args);
                const stored_args = try self.allocator.alloc(const_store.ConstTypeId, args.len);
                defer self.allocator.free(stored_args);
                for (0..args.len) |i| stored_args[i] = try self.constTypeOfMonoType(GuardedList.at(args, i));
                break :blk .{ .func = .{
                    .args = try self.result.const_types.appendTypeSpan(stored_args),
                    .ret = try self.constTypeOfMonoType(function.ret),
                } };
            },
            .record => |fields| blk: {
                const source = mono_types.fieldSpan(fields);
                const out = try self.allocator.alloc(const_store.TypeField, source.len);
                defer self.allocator.free(out);
                for (0..source.len) |i| {
                    const field = GuardedList.at(source, i);
                    out[i] = .{
                        .name = try self.constRecordFieldName(field.name),
                        .ty = try self.constTypeOfMonoType(field.ty),
                        .value_ty = if (field.value_ty) |value_ty|
                            try self.constTypeOfMonoType(value_ty)
                        else
                            null,
                        .default = try self.constFieldDefault(field.default),
                    };
                }
                break :blk .{ .record = try self.result.const_types.appendFieldSpan(out) };
            },
            .tag_union => |tags| blk: {
                const source = mono_types.tagSpan(tags);
                const out = try self.allocator.alloc(const_store.TypeTag, source.len);
                defer self.allocator.free(out);
                for (0..source.len) |i| {
                    const tag = GuardedList.at(source, i);
                    const payloads = mono_types.span(tag.payloads);
                    const stored_payloads = try self.allocator.alloc(const_store.ConstTypeId, payloads.len);
                    defer self.allocator.free(stored_payloads);
                    for (0..payloads.len) |j| stored_payloads[j] = try self.constTypeOfMonoType(GuardedList.at(payloads, j));
                    out[i] = .{
                        .name = try self.constTagName(tag.name),
                        .checked_name = try self.constTagName(tag.checked_name),
                        .payloads = try self.result.const_types.appendTypeSpan(stored_payloads),
                    };
                }
                break :blk .{ .tag_union = try self.result.const_types.appendTagSpan(out) };
            },
            .named => |named| blk: {
                const args = mono_types.span(named.args);
                const stored_args = try self.allocator.alloc(const_store.ConstTypeId, args.len);
                defer self.allocator.free(stored_args);
                for (0..args.len) |i| stored_args[i] = try self.constTypeOfMonoType(GuardedList.at(args, i));

                const declared = mono_types.declaredFieldSpan(named.declared_order);
                const stored_declared = try self.allocator.alloc(const_store.TypeDeclaredField, declared.len);
                defer self.allocator.free(stored_declared);
                for (0..declared.len) |i| {
                    const entry = GuardedList.at(declared, i);
                    stored_declared[i] = switch (entry) {
                        .named => |name| .{ .named = try self.constRecordFieldName(name) },
                        .padding => |padding| .{ .padding = try self.constTypeOfMonoType(padding) },
                    };
                }

                break :blk .{ .named = .{
                    .named_type = .{
                        .module = named.named_type.module,
                        .ty = named.named_type.ty,
                    },
                    .def = try self.constTypeDef(named.def),
                    .kind = constNamedKind(named.kind),
                    .builtin_owner = named.builtin_owner,
                    .args = try self.result.const_types.appendTypeSpan(stored_args),
                    .backing = if (named.backing) |backing| .{
                        .ty = try self.constTypeOfMonoType(backing.ty),
                        .use = constBackingUse(backing.use),
                        .authority = constBackingAuthority(backing.authority),
                    } else null,
                    .declared_order = try self.result.const_types.appendDeclaredFieldSpan(stored_declared),
                } };
            },
        };
    }

    fn fnSetForType(self: *Lowerer, ty: Type.TypeId, variants_span: Type.Span) Common.LowerError!LirProgram.FnSetId {
        const type_variants = self.types.fnVariantSpan(variants_span);
        const value_layout = try self.layoutOfType(ty);
        const variants = try self.allocator.alloc(LirProgram.FnVariant, type_variants.len);
        var initialized: usize = 0;
        errdefer {
            for (variants[0..initialized]) |variant| {
                if (variant.captures.len > 0) self.allocator.free(variant.captures);
                if (variant.template.evidence.len > 0) self.allocator.free(variant.template.evidence);
                if (variant.template.evidence_frames.len > 0) self.allocator.free(variant.template.evidence_frames);
            }
            self.allocator.free(variants);
        }

        for (0..type_variants.len) |index| {
            const variant = GuardedList.at(type_variants, index);
            const captures = if (variant.capture_ty) |capture_ty|
                try self.captureSlotsForType(capture_ty)
            else
                &.{};
            var captures_owned = captures.len > 0;
            errdefer if (captures_owned) self.allocator.free(captures);

            variants[index] = .{
                .id = @enumFromInt(@as(u32, @intCast(index))),
                .discriminant = @intCast(index),
                .variant_index = @intCast(index),
                .payload_layout = if (variant.capture_ty) |capture_ty|
                    try self.callablePayloadLayout(value_layout, type_variants.len, @intCast(index), capture_ty)
                else
                    .zst,
                .template = try constFnTemplateFromMono(self, self.fnTemplateForFn(variant.target)),
                .captures = captures,
            };
            captures_owned = false;
            initialized += 1;
        }

        const id: LirProgram.FnSetId = @enumFromInt(@as(u32, @intCast(self.result.fn_sets.items.len)));
        try self.result.fn_sets.append(self.allocator, .{
            .layout = value_layout,
            .variants = variants,
        });
        return id;
    }

    fn erasedFnsForType(self: *Lowerer, ty: Type.TypeId, erased: anytype) Common.LowerError!LirProgram.ErasedFnsId {
        // A member set proven empty means no value of this callable can exist
        // at runtime; the schema keeps the explicit empty table rather than
        // inventing an entry for code that can never run.
        const members = self.types.fnVariantSpan(erased.members);
        const entries = try self.allocator.alloc(LirProgram.ErasedFn, members.len);
        var initialized: usize = 0;
        errdefer {
            for (entries[0..initialized]) |entry| {
                if (entry.captures.len > 0) self.allocator.free(entry.captures);
                if (entry.template.evidence.len > 0) self.allocator.free(entry.template.evidence);
                if (entry.template.evidence_frames.len > 0) self.allocator.free(entry.template.evidence_frames);
            }
            self.allocator.free(entries);
        }

        for (0..members.len) |index| {
            const member = GuardedList.at(members, index);
            const captures = if (member.capture_ty) |capture_ty|
                try self.captureSlotsForType(capture_ty)
            else
                &.{};
            var captures_owned = captures.len > 0;
            errdefer if (captures_owned) self.allocator.free(captures);

            const entry_proc = try self.markReachableFn(member.target);
            entries[index] = .{
                .entry = entry_proc,
                .capture_layout = if (member.capture_ty) |capture_ty| try self.layoutOfType(capture_ty) else .zst,
                .template = try constFnTemplateFromMono(self, self.fnTemplateForFn(member.target)),
                .captures = captures,
            };
            captures_owned = false;
            initialized += 1;
        }

        const id: LirProgram.ErasedFnsId = @enumFromInt(@as(u32, @intCast(self.result.erased_fns.items.len)));
        try self.result.erased_fns.append(self.allocator, .{
            .layout = try self.layoutOfType(ty),
            .entries = entries,
        });
        return id;
    }

    fn callablePayloadLayout(
        self: *Lowerer,
        value_layout: layout.Idx,
        variant_count: usize,
        variant_index: u16,
        capture_ty: Type.TypeId,
    ) Common.LowerError!layout.Idx {
        const capture_layout = try self.layoutOfType(capture_ty);
        const value_layout_data = self.result.layouts.getLayout(value_layout);
        if (self.result.layouts.isZeroSized(value_layout_data)) {
            if (!self.result.layouts.isZeroSized(self.result.layouts.getLayout(capture_layout))) {
                Common.invariant("callable value layout is zero-sized but capture payload is not");
            }
            return .zst;
        }
        if (value_layout_data.tag == .tag_union) {
            return self.tagUnionPayloadLayout(value_layout, variant_index);
        }
        if (variant_count != 1) {
            Common.invariant("multi-variant callable with captures did not commit a tag-union layout");
        }
        return capture_layout;
    }

    fn captureSlotsForType(self: *Lowerer, ty: Type.TypeId) Common.LowerError![]const LirProgram.CaptureSlot {
        const content = self.types.get(ty);
        if (content != .capture_record) Common.invariant("function result capture slot output expected capture record type");
        const fields = self.types.captureFieldSpan(content.capture_record);
        const slots = try self.allocator.alloc(LirProgram.CaptureSlot, fields.len);
        errdefer self.allocator.free(slots);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            const checked_capture_id = field.checked_capture_id orelse
                Common.invariant("ConstStore capture field had no checked capture identity");
            slots[index] = .{
                .id = checked_capture_id,
                .slot = @intCast(index),
                .ty = try self.constTypeOfType(field.ty),
                .plan = try self.constPlanOfType(field.ty),
                .storage = if (field.storage_ty == field.ty) .value else .recursive_box,
            };
        }
        return slots;
    }

    fn fnTemplateForFn(self: *Lowerer, fn_id: Type.FnId) Mono.FnTemplate {
        const raw = @intFromEnum(fn_id);
        if (raw >= self.fn_entries.items.len) Common.invariant("function result referenced a missing function");
        return self.fn_entries.items[raw].source orelse
            Common.invariant("function result referenced a generated function without checked source identity");
    }

    fn writeRuntimeSchemas(self: *Lowerer) Common.LowerError!void {
        for (self.runtime_schema_requests.items) |request| {
            try self.writeRuntimeSchema(request);
        }
    }

    fn writeRuntimeSchema(self: *Lowerer, request: RuntimeSchemaRequest) Common.LowerError!void {
        const content = self.types.get(request.ty);
        if (content != .named) Common.invariant("runtime schema request did not reference a named Lambda Mono type");
        const named = content.named;
        if (named.def.module != request.def.module or named.def.type_name != request.def.type_name) {
            Common.invariant("runtime schema request named type identity changed before LIR lowering");
        }

        const backing = named.backing orelse Common.invariant("runtime schema request referenced a named type without runtime backing");
        if (backing.use != .inspectable) {
            Common.invariant("runtime schema request referenced a named type whose backing is not inspectable");
        }
        _ = try self.layoutOfType(request.ty);

        const type_name = self.solved.lifted.names.typeNameText(request.def.type_name);
        switch (self.runtimeSchemaShape(backing.ty)) {
            .record => try self.writeRecordSchema(type_name, request.ty),
            .tag_union => try self.writeTagUnionSchema(type_name, request.ty),
        }
    }

    const RuntimeSchemaShape = enum {
        record,
        tag_union,
    };

    fn runtimeSchemaShape(self: *Lowerer, ty: Type.TypeId) RuntimeSchemaShape {
        const content = self.types.get(ty);
        if (content == .record) return .record;
        if (content == .tag_union) return .tag_union;
        if (content != .named) Common.invariant("runtime schema request backing was not a record or tag union");
        const backing = content.named.backing orelse Common.invariant("runtime schema request crossed a named type without backing");
        if (backing.use != .inspectable) {
            Common.invariant("runtime schema request crossed a non-inspectable named backing");
        }
        return self.runtimeSchemaShape(backing.ty);
    }

    fn writeRecordSchema(self: *Lowerer, type_name: []const u8, ty: Type.TypeId) Common.LowerError!void {
        const source = self.recordFields(ty);
        const schema_fields = try self.allocator.alloc(RuntimeRecordFieldSchema, source.len);
        errdefer {
            for (schema_fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema_fields);
        }
        for (0..source.len) |index| {
            const field = GuardedList.at(source, index);
            schema_fields[index] = .{
                .name = try self.allocator.dupe(u8, self.solved.lifted.names.recordFieldLabelText(field.name)),
                .logical_index = @intCast(index),
            };
        }
        try self.runtime_schemas.records.append(self.allocator, .{
            .type_name = try self.allocator.dupe(u8, type_name),
            .fields = schema_fields,
        });
    }

    fn writeTagUnionSchema(self: *Lowerer, type_name: []const u8, ty: Type.TypeId) Common.LowerError!void {
        const source = self.tagUnionTags(ty);
        const schema_tags = try self.allocator.alloc(RuntimeTagSchema, source.len);
        errdefer {
            for (schema_tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema_tags);
        }
        for (0..source.len) |index| {
            const tag = GuardedList.at(source, index);
            schema_tags[index] = .{
                .name = try self.allocator.dupe(u8, self.solved.lifted.names.tagLabelText(tag.name)),
                .discriminant = self.tagIndex(ty, tag.name),
            };
        }
        try self.runtime_schemas.tag_unions.append(self.allocator, .{
            .type_name = try self.allocator.dupe(u8, type_name),
            .tags = schema_tags,
        });
    }

    fn verifyMaterializedDecisions(self: *Lowerer) Common.LowerError!void {
        if (builtin.mode != .Debug) return;
        var solved_clone = try cloneSolvedProgram(self.allocator, self.solved);
        var clone_owned = true;
        errdefer if (clone_owned) solved_clone.deinit();

        var materialized_identities = std.ArrayList(LambdaMonoLower.SpecializationIdentity).empty;
        defer materialized_identities.deinit(self.allocator);
        var materialized = try LambdaMonoLower.run(self.allocator, solved_clone, self.folded_map_matches.items, .{
            .inline_expects = switch (self.inline_expects) {
                .run => .run,
                .omit => .omit,
            },
            .debug_specialization_identities = &materialized_identities,
        });
        clone_owned = false;
        var materialized_owned = true;
        defer if (materialized_owned) materialized.deinit();

        var type_equivalence = TypeEquivalence.init(self.allocator, self, &materialized.types);
        defer type_equivalence.deinit();

        try self.verifyFnEntriesMatch(&materialized, materialized_identities.items);
        try self.verifyRootsMatch(&materialized, materialized_identities.items);
        try self.verifyLayoutRequestsMatch(&materialized, &type_equivalence);
        try self.verifyRuntimeSchemaRequestsMatch(&materialized, &type_equivalence);

        if (self.debug_materialized_out) |out| {
            out.* = materialized;
            materialized_owned = false;
        }
    }

    fn specializationIdentity(spec: FnSpec) LambdaMonoLower.SpecializationIdentity {
        return .{
            .source = spec.source,
            .solved_fn_ty = spec.solved_fn_ty,
            .abi = switch (spec.abi) {
                .finite => .finite,
                .erased => .erased,
            },
            .captures_source = switch (spec.captures.source) {
                .solved => .solved,
                .own => .own,
            },
            .captures_start = specializationIdentityCaptureStart(spec.captures),
            .captures_len = spec.captures.len,
        };
    }

    fn verifyFnEntriesMatch(
        self: *Lowerer,
        materialized: *const LambdaMono.Program,
        identities: []const LambdaMonoLower.SpecializationIdentity,
    ) Common.LowerError!void {
        const materialized_fns = materialized.fnsView();
        if (identities.len != materialized_fns.len) {
            Common.invariant("debug Lambda Mono verifier saw a specialization identity count mismatch");
        }
        var by_identity = std.AutoHashMap(LambdaMonoLower.SpecializationIdentity, LambdaMono.FnId).init(self.allocator);
        defer by_identity.deinit();
        for (identities, 0..) |identity, index| {
            const result = try by_identity.getOrPut(identity);
            if (result.found_existing) {
                Common.invariant("debug Lambda Mono verifier saw duplicate specialization identities");
            }
            result.value_ptr.* = @enumFromInt(@as(u32, @intCast(index)));
        }

        for (self.fn_entries.items, 0..) |entry, entry_index| {
            // Direct LIR keeps type-level entries so callable layouts and
            // ConstStore metadata can refer to source templates without forcing
            // every queued Lambda Mono function into a LIR proc.
            if (!self.fn_reachable.items[entry_index]) continue;
            const materialized_fn_id = by_identity.get(specializationIdentity(entry.spec)) orelse {
                Common.invariant("debug Lambda Mono verifier could not match a direct function spec");
            };
            if (!try self.fnEntryMatchesMaterialized(entry, materialized.getFn(materialized_fn_id), materialized)) {
                Common.invariant("debug Lambda Mono verifier saw different types for an exact function specialization");
            }
        }
    }

    fn fnEntryMatchesMaterialized(
        self: *Lowerer,
        entry: FnEntry,
        fn_: LambdaMono.Fn,
        materialized: *const LambdaMono.Program,
    ) Common.LowerError!bool {
        var type_equivalence = TypeEquivalence.init(self.allocator, self, &materialized.types);
        defer type_equivalence.deinit();

        if (!std.meta.eql(entry.source, fn_.source)) return false;
        if (!try type_equivalence.equivalent(entry.ret, fn_.ret)) return false;

        const args = materialized.typedLocalSpan(fn_.args);
        const arg_tys = self.types.span(entry.args);
        const expected_arg_count = arg_tys.len + if (entry.capture_arg_ty == null) @as(usize, 0) else 1;
        if (args.len != expected_arg_count) return false;
        for (0..arg_tys.len) |arg_index| {
            const arg_ty = GuardedList.at(arg_tys, arg_index);
            const arg = GuardedList.at(args, arg_index);
            if (!try type_equivalence.equivalent(arg_ty, arg.ty)) return false;
        }
        if (entry.capture_arg_ty) |capture_arg_ty| {
            if (!try type_equivalence.equivalent(capture_arg_ty, GuardedList.at(args, arg_tys.len).ty)) return false;
        }
        return true;
    }

    fn verifyRootsMatch(
        self: *Lowerer,
        materialized: *const LambdaMono.Program,
        identities: []const LambdaMonoLower.SpecializationIdentity,
    ) Common.LowerError!void {
        const roots = materialized.rootsView();
        if (self.roots.items.len != roots.len) {
            Common.invariant("debug Lambda Mono verifier saw a root count mismatch");
        }
        for (self.roots.items, roots) |direct, expected| {
            if (!std.meta.eql(direct.request, expected.request)) {
                Common.invariant("debug Lambda Mono verifier saw a root mismatch");
            }
            if (!std.meta.eql(
                specializationIdentity(self.fn_entries.items[@intFromEnum(direct.fn_id)].spec),
                identities[@intFromEnum(expected.fn_id)],
            )) {
                Common.invariant("debug Lambda Mono verifier saw a root specialization identity mismatch");
            }
            const expected_fn = materialized.getFn(expected.fn_id);
            if (!try self.fnEntryMatchesMaterialized(self.fn_entries.items[@intFromEnum(direct.fn_id)], expected_fn, materialized)) {
                Common.invariant("debug Lambda Mono verifier saw a root mismatch");
            }
        }
    }

    fn verifyLayoutRequestsMatch(self: *Lowerer, materialized: *const LambdaMono.Program, type_equivalence: *TypeEquivalence) Common.LowerError!void {
        const requests = materialized.layoutRequestsView();
        if (self.layout_requests.items.len != requests.len) {
            Common.invariant("debug Lambda Mono verifier saw a layout request count mismatch");
        }
        for (self.layout_requests.items, requests) |direct, expected| {
            if (direct.checked_type != expected.checked_type or
                !std.meta.eql(direct.const_locator, expected.const_locator) or
                !try type_equivalence.equivalent(direct.ty, expected.ty))
            {
                Common.invariant("debug Lambda Mono verifier saw a layout request mismatch");
            }
        }
    }

    fn verifyRuntimeSchemaRequestsMatch(self: *Lowerer, materialized: *const LambdaMono.Program, type_equivalence: *TypeEquivalence) Common.LowerError!void {
        const requests = materialized.runtimeSchemaRequestsView();
        if (self.runtime_schema_requests.items.len != requests.len) {
            Common.invariant("debug Lambda Mono verifier saw a runtime schema request count mismatch");
        }
        for (self.runtime_schema_requests.items, requests) |direct, expected| {
            if (!std.meta.eql(direct.def, expected.def) or !try type_equivalence.equivalent(direct.ty, expected.ty)) {
                Common.invariant("debug Lambda Mono verifier saw a runtime schema request mismatch");
            }
        }
    }

    fn lowerExprReturn(self: *Lowerer, expr_id: Lifted.ExprId, ret_ty: Type.TypeId) Common.LowerError!LIR.CFStmtId {
        // The proc signature carries the exact context-sensitive layout
        // committed for this specialization. Recursive types can have an
        // unboxed ordinary layout and a boxed recursive-slot layout for the
        // same monomorphic type, so recomputing from `ret_ty` here would lose
        // the explicit ABI choice.
        const proc_id = self.current_proc orelse Common.invariant("return lowering ran without a current procedure");
        const ret_layout = self.result.store.getProcSpec(proc_id).ret_layout;
        const ret_local = try self.addLocalForLayout(ret_layout);
        try self.local_types.put(ret_local, ret_ty);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        const saved_return_target = self.current_return_target;
        self.current_return_target = ret_local;
        defer self.current_return_target = saved_return_target;
        return try self.lowerExprIntoAtType(ret_local, expr_id, ret_ty, ret_stmt);
    }

    fn lowerStaticDataCandidateInto(
        self: *Lowerer,
        target: LIR.LocalId,
        candidate: Mono.StaticDataCandidate,
        ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const layout_idx = self.result.store.getLocal(target).layout_idx;
        if (self.layoutNeedsStaticData(layout_idx)) {
            return try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .static_data = try self.lirStaticDataFor(candidate, ty, layout_idx) },
                .next = next,
            } });
        }
        return try self.lowerExprIntoAtType(target, candidate.runtime_expr, ty, next);
    }

    fn lowerExprInto(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const expr_data = self.solved.lifted.getExpr(expr_id);
        const expr_ty = try self.lowerExprTy(expr_id);
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
        const saved_inline_scope = self.result.store.current_inline_scope;
        defer self.result.store.current_inline_scope = saved_inline_scope;
        self.result.store.current_inline_scope = lirInlineScopeId(self.solved.lifted.exprInlineScope(expr_id));
        const expr_loc = self.solved.lifted.exprLoc(expr_id);
        if (expr_loc.hasLocation()) {
            self.result.store.current_loc = expr_loc;
        }
        const expr_region = self.solved.lifted.exprRegion(expr_id);
        if (!expr_region.isEmpty()) {
            self.result.store.current_region = expr_region;
        }
        return switch (expr_data.data) {
            .local => |local| try self.lowerLocalInto(target, local, expr_ty, next),
            .unit => try self.assignZst(target, next),
            .int_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i128_literal = .{
                    .value = value.toI128(),
                    .layout_idx = self.result.store.getLocal(target).layout_idx,
                } },
                .next = next,
            } }),
            .frac_f32_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = value },
                .next = next,
            } }),
            .frac_f64_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = value },
                .next = next,
            } }),
            .dec_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .dec_literal = value.num },
                .next = next,
            } }),
            .str_lit => |literal| blk: {
                const str_lit = self.stringLiteral(literal);
                break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .str_literal = try self.result.store.insertStringView(str_lit.backing, str_lit.offset, str_lit.len) },
                    .next = next,
                } });
            },
            .bytes_lit => |literal| blk: {
                const bytes_lit = self.stringLiteral(literal.literal);
                const elem_layout = self.localListElemLayout(target);
                const elem_size_align = self.result.layouts.layoutSizeAlign(self.result.layouts.getLayout(elem_layout));
                const elem_size: u32 = elem_size_align.size;
                const elem_alignment: u32 = @intCast(@max(elem_size_align.alignment.toByteUnits(), 1));
                const expected_bytes = std.math.mul(u32, literal.len, elem_size) catch
                    Common.invariant("packed list literal byte length overflowed");
                if (expected_bytes != bytes_lit.len) {
                    Common.invariant("packed list literal byte length did not match its element layout");
                }
                if (bytes_lit.offset % elem_alignment != 0) {
                    Common.invariant("packed list literal view was not aligned for its element layout");
                }
                break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .bytes_literal = .{
                        .bytes = try self.result.store.insertStringViewAligned(
                            bytes_lit.backing,
                            bytes_lit.offset,
                            bytes_lit.len,
                            elem_alignment,
                        ),
                        .len = literal.len,
                    } },
                    .next = next,
                } });
            },
            .@"unreachable" => Common.invariant("unreachable marker escaped its terminated block-final position during direct LIR lowering"),
            .uninitialized, .uninitialized_payload => next,
            .static_data_candidate => |candidate| try self.lowerStaticDataCandidateInto(target, candidate, expr_ty, next),
            .list => |items| try self.lowerListIntoAtType(target, expr_ty, items, next),
            .tuple => |items| try self.lowerTupleIntoAtType(target, expr_ty, items, next),
            .record => |fields| try self.lowerRecordInto(target, expr_ty, fields, next),
            .record_update => |update| try self.lowerRecordUpdateInto(target, expr_ty, update, next),
            .tag => |tag| try self.lowerTagInto(target, expr_ty, tag.name, tag.payloads, next),
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached direct LIR lowering"),
            .fn_ref => |fn_ref| try self.lowerFnRefInto(target, expr_id, fn_ref.fn_id, self.solved.lifted.captureOperandSpan(fn_ref.captures), next),
            .nominal => |backing| try self.lowerNominalInto(target, expr_ty, backing, next),
            .let_ => |let_| try self.lowerLetIntoAtType(target, expr_ty, let_, next),
            .call_proc => |call| switch (Lifted.directCallee(call)) {
                .local => |callee| try self.lowerDirectProcCallInto(
                    target,
                    expr_ty,
                    callee,
                    self.solved.lifted.exprSpan(call.args),
                    self.solved.lifted.captureOperandSpan(call.captures),
                    call.is_cold,
                    next,
                ),
                .imported => Common.invariant("direct LIR lowering requires imported Monotype calls to be linked before this stage"),
            },
            .call_value => |call| try self.lowerValueCallInto(target, expr_ty, call.callee, self.solved.lifted.exprSpan(call.args), next),
            .low_level => |call| try self.lowerLowLevelInto(target, call.op, call.args, next),
            .field_access => |field| try self.lowerFieldAccessInto(target, field.receiver, field.segments, next),
            .tuple_access => |access| try self.lowerTupleAccessInto(target, access.tuple, access.elem_index, next),
            .structural_eq => |eq| try self.lowerStructuralEqInto(target, eq.lhs, eq.rhs, eq.negated, next),
            .structural_hash => |h| try self.lowerStructuralHashInto(target, h.value, h.hasher, next),
            .match_ => |match_| try self.lowerMatchInto(target, expr_ty, match_.scrutinee, match_.branches, match_.comptime_site, next),
            .if_ => |if_| try self.lowerIfInto(target, expr_ty, if_.branches, if_.final_else, next),
            .if_initialized_payload => |payload_switch| try self.lowerInitializedPayloadSwitchInto(target, payload_switch, next),
            .try_sequence => |sequence| try self.lowerTrySequenceInto(target, expr_ty, sequence, next),
            .try_record_sequence => |sequence| try self.lowerTryRecordSequenceInto(target, expr_ty, sequence, next),
            .block => |block| try self.lowerBlockIntoAtType(target, expr_ty, block.statements, block.final_expr, next),
            .loop_ => |loop| try self.lowerLoopIntoAtType(target, expr_ty, loop, next),
            .break_ => |value| try self.lowerBreak(value),
            .continue_ => |continue_| try self.lowerContinue(continue_.values),
            .join_point => |join_point| try self.lowerJoinPointIntoAtType(target, expr_ty, join_point, next),
            .jump => |jump| try self.lowerJump(jump),
            .return_ => |ret| try self.lowerReturn(ret),
            .crash => |msg| try self.result.store.addCFStmt(.{ .crash = .{
                .msg = .{ .literal = try self.result.store.insertString(self.stringLiteralText(msg)) },
            } }),
            .comptime_branch_taken => |taken| try self.result.store.addCFStmt(.{ .comptime_branch_taken = .{
                .site = try self.lowerComptimeSite(taken.site),
                .branch_index = taken.branch_index,
                .next = try self.lowerExprInto(target, taken.body, next),
            } }),
            .comptime_exhaustiveness_failed => |site| try self.result.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{
                .site = try self.lowerComptimeSite(site),
            } }),
            .dbg => |child| blk: {
                const after_dbg = try self.assignZst(target, next);
                const message = try self.addTemp(try self.lowerExprTy(child));
                const debug_stmt = try self.result.store.addCFStmt(.{ .debug = .{ .message = message, .next = after_dbg } });
                break :blk try self.lowerExprInto(message, child, debug_stmt);
            },
            .expect_err => |expect_err| blk: {
                const message = try self.addTemp(try self.lowerExprTy(expect_err.msg));
                const expect_err_stmt = try self.result.store.addCFStmt(.{ .expect_err = .{
                    .message = message,
                    .region = expect_err.region,
                } });
                break :blk try self.lowerExprInto(message, expect_err.msg, expect_err_stmt);
            },
            .expect => |child| if (self.inline_expects == .omit)
                try self.assignZst(target, next)
            else
                try self.lowerExpectExprInto(target, child, next),
        };
    }

    fn lowerExprIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const expr_data = self.solved.lifted.getExpr(expr_id);
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
        const saved_inline_scope = self.result.store.current_inline_scope;
        defer self.result.store.current_inline_scope = saved_inline_scope;
        self.result.store.current_loc = self.solved.lifted.exprLoc(expr_id);
        self.result.store.current_region = self.solved.lifted.exprRegion(expr_id);
        self.result.store.current_inline_scope = lirInlineScopeId(self.solved.lifted.exprInlineScope(expr_id));

        return switch (expr_data.data) {
            .local => |local| try self.lowerLocalInto(target, local, ty, next),
            .list => |items| try self.lowerListIntoAtType(target, ty, items, next),
            .tuple => |items| try self.lowerTupleIntoAtType(target, ty, items, next),
            .record => |fields| try self.lowerRecordInto(target, ty, fields, next),
            .record_update => |update| try self.lowerRecordUpdateInto(target, ty, update, next),
            .tag => |tag| try self.lowerTagInto(target, ty, tag.name, tag.payloads, next),
            .call_proc => |call| switch (Lifted.directCallee(call)) {
                .local => |callee| try self.lowerDirectProcCallInto(
                    target,
                    ty,
                    callee,
                    self.solved.lifted.exprSpan(call.args),
                    self.solved.lifted.captureOperandSpan(call.captures),
                    call.is_cold,
                    next,
                ),
                .imported => Common.invariant("direct LIR lowering requires imported Monotype calls to be linked before this stage"),
            },
            .fn_ref => |fn_ref| try self.lowerFnRefIntoAtType(
                target,
                expr_id,
                fn_ref.fn_id,
                self.solved.lifted.captureOperandSpan(fn_ref.captures),
                ty,
                next,
            ),
            .nominal => |backing| try self.lowerNominalInto(target, ty, backing, next),
            .let_ => |let_| try self.lowerLetIntoAtType(target, ty, let_, next),
            .static_data_candidate => |candidate| try self.lowerStaticDataCandidateInto(target, candidate, ty, next),
            .field_access => |field| try self.lowerFieldAccessInto(target, field.receiver, field.segments, next),
            .call_value => |call| try self.lowerValueCallInto(target, ty, call.callee, self.solved.lifted.exprSpan(call.args), next),
            .match_ => |match_| try self.lowerMatchInto(target, ty, match_.scrutinee, match_.branches, match_.comptime_site, next),
            .if_ => |if_| try self.lowerIfInto(target, ty, if_.branches, if_.final_else, next),
            .try_sequence => |sequence| try self.lowerTrySequenceInto(target, ty, sequence, next),
            .try_record_sequence => |sequence| try self.lowerTryRecordSequenceInto(target, ty, sequence, next),
            .block => |block| try self.lowerBlockIntoAtType(target, ty, block.statements, block.final_expr, next),
            .loop_ => |loop| try self.lowerLoopIntoAtType(target, ty, loop, next),
            .join_point => |join_point| try self.lowerJoinPointIntoAtType(target, ty, join_point, next),
            .comptime_branch_taken => |taken| try self.result.store.addCFStmt(.{ .comptime_branch_taken = .{
                .site = try self.lowerComptimeSite(taken.site),
                .branch_index = taken.branch_index,
                .next = try self.lowerExprIntoAtType(target, taken.body, ty, next),
            } }),
            .unit,
            .@"unreachable",
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bytes_lit,
            .lambda,
            .def_ref,
            .fn_def,
            .low_level,
            .tuple_access,
            .structural_eq,
            .structural_hash,
            .uninitialized,
            .uninitialized_payload,
            .if_initialized_payload,
            .break_,
            .continue_,
            .jump,
            .return_,
            .crash,
            .comptime_exhaustiveness_failed,
            .dbg,
            .expect_err,
            .expect,
            => try self.lowerExprInto(target, expr_id, next),
        };
    }

    fn lowerListIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        span: Lifted.Span(Lifted.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const items = self.solved.lifted.exprSpan(span);
        const elem_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(elem_locals);

        const elem_ty = self.listElemType(ty);
        const elem_layout = self.localListElemLayout(target);
        for (0..items.len) |i| {
            elem_locals[i] = try self.addLocalForLayout(elem_layout);
            try self.local_types.put(elem_locals[i], elem_ty);
        }

        var current = try self.result.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.result.store.addLocalSpan(elem_locals),
            .next = next,
        } });
        var i = items.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerExprIntoAtType(elem_locals[i], GuardedList.at(items, i), elem_ty, current);
        }
        return current;
    }

    fn lowerTupleIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        span: Lifted.Span(Lifted.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const items = self.solved.lifted.exprSpan(span);
        return try self.lowerStructExprsIntoAtTypes(target, items, self.tupleItemTypes(ty), next);
    }

    fn lowerStructExprsIntoAtTypes(
        self: *Lowerer,
        target: LIR.LocalId,
        items: anytype,
        item_tys: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (items.len != item_tys.len) Common.invariant("struct expression arity differed from target type arity");
        const owned_items = try GuardedList.dupe(self.allocator, Lifted.ExprId, items);
        defer self.allocator.free(owned_items);
        const owned_tys = try GuardedList.dupe(self.allocator, Type.TypeId, item_tys);
        defer self.allocator.free(owned_tys);
        if (self.boxBackingLayoutForDirectConstruction(target)) |backing_layout| {
            const backing_local = try self.addLocalForLayout(backing_layout);
            const boundary = try self.assignBoxBoundary(target, backing_local, backing_layout, next);
            const saved_return_target = self.current_return_target;
            if (saved_return_target == target) {
                self.current_return_target = backing_local;
            }
            defer self.current_return_target = saved_return_target;
            return try self.lowerStructExprsIntoAtTypes(backing_local, owned_items, owned_tys, boundary);
        }

        const field_locals = try self.allocator.alloc(LIR.LocalId, owned_items.len);
        defer self.allocator.free(field_locals);

        const target_is_zst = self.isZstLocal(target);
        for (0..owned_items.len) |i| {
            const field_layout = if (target_is_zst)
                layout.Idx.zst
            else
                self.localFieldLayout(target, @intCast(i));
            field_locals[i] = try self.addLocalForLayout(field_layout);
            try self.local_types.put(field_locals[i], owned_tys[i]);
        }

        var current = if (target_is_zst)
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.result.store.addLocalSpan(field_locals),
                .next = next,
            } });
        const saved_return_target = self.current_return_target;
        const demanded_child = if (saved_return_target == target)
            self.singleErasedResultChildIndex(owned_tys)
        else
            null;
        var i = owned_items.len;
        while (i > 0) {
            i -= 1;
            self.current_return_target = if (demanded_child != null and demanded_child.? == i)
                field_locals[i]
            else if (saved_return_target == target)
                null
            else
                saved_return_target;
            current = self.lowerExprIntoAtType(field_locals[i], owned_items[i], owned_tys[i], current) catch |err| {
                self.current_return_target = saved_return_target;
                return err;
            };
        }
        self.current_return_target = saved_return_target;
        return current;
    }

    fn lowerCaptureRecordFromCaptureExprsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        capture_span: CaptureSpanId,
        capture_operands: anytype,
        capture_ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const captures = self.captureSpan(capture_span);
        const capture_content = self.types.get(capture_ty);
        if (capture_content != .capture_record) Common.invariant("callable capture payload was not a capture record");
        const fields = self.types.captureFieldSpan(capture_content.capture_record);
        if (captures.len != fields.len) Common.invariant("callable capture payload arity differed from captured locals");
        if (captures.len != capture_operands.len) {
            Common.invariant("function reference capture operand count differed from lifted function captures");
        }

        const field_locals = try self.allocator.alloc(LIR.LocalId, captures.len);
        defer self.allocator.free(field_locals);
        const field_storage_tys = try self.allocator.alloc(Type.TypeId, captures.len);
        defer self.allocator.free(field_storage_tys);

        const target_is_zst = self.isZstLocal(target);
        // Member captures, capture-record fields, and keyed operands are all in
        // ascending CaptureId order, so the join is an exact indexed walk.
        for (0..captures.len) |i| {
            const capture = GuardedList.at(captures, i);
            const field = GuardedList.at(fields, i);
            const operand = GuardedList.at(capture_operands, i);
            if (capture.capture_id != field.capture_id) {
                Common.invariant("callable capture payload fields differed from captured locals");
            }
            const capture_id = capture.capture_id orelse Common.invariant("member capture had no CaptureId");
            if (operand.id != capture_id) Common.invariant("capture operand CaptureId did not match its member capture slot");
            field_storage_tys[i] = field.storage_ty;
            if (target_is_zst) {
                field_locals[i] = try self.addTemp(field.storage_ty);
                continue;
            }
            const field_layout = self.localFieldLayout(target, @intCast(i));
            field_locals[i] = try self.addLocalForLayout(field_layout);
            try self.local_types.put(field_locals[i], field.storage_ty);
        }

        var current = if (target_is_zst)
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.result.store.addLocalSpan(field_locals),
                .next = next,
            } });
        var i = captures.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerExprIntoAtType(field_locals[i], GuardedList.at(capture_operands, i).value, field_storage_tys[i], current);
        }
        return current;
    }

    fn lowerRecordInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        span: Lifted.Span(Lifted.FieldExpr),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const type_fields = self.recordFields(ty);
        const expr_fields = self.solved.lifted.fieldExprSpan(span);
        const ordered = try self.allocator.alloc(Lifted.ExprId, type_fields.len);
        defer self.allocator.free(ordered);
        const field_tys = try self.allocator.alloc(Type.TypeId, type_fields.len);
        defer self.allocator.free(field_tys);

        for (0..type_fields.len) |i| {
            const field = GuardedList.at(type_fields, i);
            ordered[i] = for (0..expr_fields.len) |expr_index| {
                const expr_field = GuardedList.at(expr_fields, expr_index);
                if (expr_field.name == field.name) break expr_field.value;
            } else Common.invariant("record expression missing field output by Lambda Mono type");
            field_tys[i] = field.ty;
        }

        return try self.lowerStructExprsIntoAtTypes(target, ordered, field_tys, next);
    }

    fn lowerRecordUpdateInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        update: Lifted.RecordUpdate,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.boxBackingLayoutForDirectConstruction(target)) |backing_layout| {
            const backing_local = try self.addLocalForLayout(backing_layout);
            const boundary = try self.assignBoxBoundary(target, backing_local, backing_layout, next);
            return try self.lowerRecordUpdateInto(backing_local, ty, update, boundary);
        }

        const type_fields = try GuardedList.dupe(self.allocator, Type.Field, self.recordFields(ty));
        defer self.allocator.free(type_fields);
        const update_fields = try GuardedList.dupe(self.allocator, Lifted.FieldExpr, self.solved.lifted.fieldExprSpan(update.fields));
        defer self.allocator.free(update_fields);
        const updates_by_field = try self.allocator.alloc(?Lifted.ExprId, type_fields.len);
        defer self.allocator.free(updates_by_field);
        @memset(updates_by_field, null);
        for (update_fields) |field| {
            const field_index = Lowerer.recordFieldIndexInFields(type_fields, field.name);
            if (updates_by_field[field_index] != null) Common.invariant("record update contained the same field more than once");
            updates_by_field[field_index] = field.value;
        }

        const base_ty = try self.lowerExprContextTy(update.base);
        const base_local = try self.addTemp(base_ty);
        const field_locals = try self.allocator.alloc(LIR.LocalId, type_fields.len);
        defer self.allocator.free(field_locals);
        const target_is_zst = self.isZstLocal(target);
        for (type_fields, 0..) |field, index| {
            const field_layout = if (target_is_zst)
                layout.Idx.zst
            else
                self.localFieldLayout(target, @intCast(index));
            field_locals[index] = try self.addLocalForLayout(field_layout);
            try self.local_types.put(field_locals[index], field.ty);
        }

        var current = if (target_is_zst)
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.result.store.addLocalSpan(field_locals),
                .next = next,
            } });

        var index = type_fields.len;
        while (index > 0) {
            index -= 1;
            if (updates_by_field[index]) |value| {
                current = try self.lowerExprIntoAtType(field_locals[index], value, type_fields[index].ty, current);
            }
        }

        const source_ty = self.storageTypeOfLocalOr(base_local, base_ty);
        index = type_fields.len;
        while (index > 0) {
            index -= 1;
            if (updates_by_field[index] != null) continue;
            if (target_is_zst) {
                current = try self.assignZst(field_locals[index], current);
                continue;
            }
            const source_index = self.recordFieldIndex(source_ty, type_fields[index].name);
            const source_field_ty = self.recordFieldType(source_ty, type_fields[index].name);
            current = try self.assignTypedRefRead(
                field_locals[index],
                type_fields[index].ty,
                source_field_ty,
                self.localFieldLayout(base_local, source_index),
                .{ .field = .{ .source = base_local, .field_idx = source_index } },
                current,
            );
        }

        return try self.lowerExprIntoAtType(base_local, update.base, base_ty, current);
    }

    fn lowerTagInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        name: Type.names.TagNameId,
        payload_span: Lifted.Span(Lifted.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const variant_index = self.tagIndex(ty, name);
        return try self.lowerTagPayloadInto(target, ty, variant_index, payload_span, next);
    }

    fn lowerTagPayloadInto(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        variant_index: u16,
        payload_span: Lifted.Span(Lifted.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.boxBackingLayoutForDirectConstruction(target)) |backing_layout| {
            const backing_local = try self.addLocalForLayout(backing_layout);
            const boundary = try self.assignBoxBoundary(target, backing_local, backing_layout, next);
            const saved_return_target = self.current_return_target;
            if (saved_return_target == target) {
                self.current_return_target = backing_local;
            }
            defer self.current_return_target = saved_return_target;
            return try self.lowerTagPayloadInto(backing_local, ty, variant_index, payload_span, boundary);
        }

        const payloads = self.solved.lifted.exprSpan(payload_span);
        const payload_tys = self.tagPayloadTypesByIndex(ty, variant_index);
        if (payloads.len != payload_tys.len) Common.invariant("tag expression payload arity differed from target tag type");
        if (payloads.len == 0) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = variant_index,
                .discriminant = variant_index,
                .payload = null,
                .next = next,
            } });
        }

        const target_layout = self.result.store.getLocal(target).layout_idx;
        const payload_local = try self.addLocalForLayout(if (self.isZstLocal(target))
            .zst
        else
            self.tagUnionPayloadLayout(target_layout, variant_index));
        if (self.isZstLocal(target) and !self.isZstLocal(payload_local)) {
            Common.invariant("zero-sized tag layout had a non-zero-sized payload");
        }
        const assign_tag = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = variant_index,
                .discriminant = variant_index,
                .payload = payload_local,
                .next = next,
            } });

        const saved_return_target = self.current_return_target;
        const variant_has_demand = self.erasedResultDemandForSimultaneousTypes(payload_tys) == .single_slot;
        if (saved_return_target == target) {
            self.current_return_target = if (variant_has_demand) payload_local else null;
        }
        defer self.current_return_target = saved_return_target;

        if (payloads.len == 1) {
            return try self.lowerExprIntoAtType(
                payload_local,
                GuardedList.at(payloads, 0),
                GuardedList.at(payload_tys, 0),
                assign_tag,
            );
        }
        return try self.lowerStructExprsIntoAtTypes(payload_local, payloads, payload_tys, assign_tag);
    }

    fn boxBackingLayoutForDirectConstruction(self: *Lowerer, target: LIR.LocalId) ?layout.Idx {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const target_content = self.result.layouts.getLayout(target_layout);
        if (target_content.tag == .box) return target_content.getIdx();
        if (target_content.tag == .box_of_zst) return .zst;
        return null;
    }

    fn lowerNominalInto(self: *Lowerer, target: LIR.LocalId, nominal_ty: Type.TypeId, backing: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const backing_ty = try self.nominalBackingType(nominal_ty, backing);
        const backing_data = self.solved.lifted.getExpr(backing).data;
        // Constructors do not have a pre-existing runtime representation.
        // Build them directly in the nominal target layout so recursive
        // Box edges come from that producer-owned layout graph instead of
        // constructing an independently rooted backing layout and then
        // recursively converting the completed value.
        if (backing_data == .list or backing_data == .tuple or backing_data == .record or
            backing_data == .record_update or backing_data == .tag)
        {
            return try self.lowerExprIntoAtType(target, backing, backing_ty, next);
        }

        const backing_layout = try self.layoutOfType(backing_ty);
        const backing_local = try self.addLocalForLayout(backing_layout);
        const assign = try self.assignNominalBoundaryAtTypes(target, nominal_ty, backing_local, backing_ty, backing_layout, next);
        const saved_return_target = self.current_return_target;
        if (saved_return_target == target and self.result.store.getLocal(target).layout_idx == backing_layout) {
            self.current_return_target = backing_local;
        }
        defer self.current_return_target = saved_return_target;
        return try self.lowerExprIntoAtType(backing_local, backing, backing_ty, assign);
    }

    fn nominalBackingType(self: *Lowerer, nominal_ty: Type.TypeId, backing: Lifted.ExprId) Common.LowerError!Type.TypeId {
        const content = self.types.get(nominal_ty);
        if (content == .named) return (content.named.backing orelse Common.invariant("nominal expression target had no runtime backing")).ty;
        if (content == .box) return content.box;
        return try self.lowerExprTy(backing);
    }

    fn assignNominalBoundaryAtTypes(
        self: *Lowerer,
        target: LIR.LocalId,
        nominal_ty: Type.TypeId,
        backing_local: LIR.LocalId,
        backing_ty: Type.TypeId,
        backing_layout: layout.Idx,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (target_layout == backing_layout) return try self.assignLocal(target, backing_local, next);

        const target_content = self.result.layouts.getLayout(target_layout);
        const backing_content = self.result.layouts.getLayout(backing_layout);
        const target_is_box = target_content.tag == .box or target_content.tag == .box_of_zst;
        const backing_is_box = backing_content.tag == .box or backing_content.tag == .box_of_zst;
        if (!target_is_box and !backing_is_box) {
            const nominal_runtime_ty = self.runtimeBackingType(nominal_ty);
            const backing_runtime_ty = self.runtimeBackingType(backing_ty);
            const nominal_runtime = self.types.get(nominal_runtime_ty);
            const backing_runtime = self.types.get(backing_runtime_ty);
            if (nominal_runtime == .record and backing_runtime == .record) {
                return try self.assignRecordBoundary(target, nominal_runtime.record, backing_local, backing_runtime.record, next);
            }
            if (nominal_runtime == .tuple and backing_runtime == .tuple) {
                return try self.assignTupleBoundary(target, nominal_runtime.tuple, backing_local, backing_runtime.tuple, next);
            }
            if (nominal_runtime == .tag_union and backing_runtime == .tag_union) {
                return try self.assignTagUnionBoundary(target, nominal_runtime.tag_union, backing_local, backing_runtime.tag_union, next);
            }
        }

        return try self.assignNominalBoundary(target, backing_local, backing_layout, next);
    }

    fn assignNominalBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        backing_local: LIR.LocalId,
        backing_layout: layout.Idx,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (target_layout == backing_layout) return try self.assignLocal(target, backing_local, next);

        const target_content = self.result.layouts.getLayout(target_layout);
        const backing_content = self.result.layouts.getLayout(backing_layout);

        if (target_content.tag == .box and try self.layoutsEquivalent(target_content.getIdx(), backing_layout)) {
            return try self.assignUnaryLowLevel(target, .box_box, backing_local, next);
        }
        if (target_content.tag == .box_of_zst and self.result.layouts.isZeroSized(backing_content)) {
            return try self.assignUnaryLowLevel(target, .box_box, backing_local, next);
        }
        if (backing_content.tag == .box and try self.layoutsEquivalent(backing_content.getIdx(), target_layout)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, backing_local, next);
        }
        if (backing_content.tag == .box_of_zst and self.result.layouts.isZeroSized(target_content)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, backing_local, next);
        }

        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(backing_local)) {
                Common.invariant("nominal boundary tried to store non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }

        const target_is_box = target_content.tag == .box or target_content.tag == .box_of_zst;
        const backing_is_box = backing_content.tag == .box or backing_content.tag == .box_of_zst;
        const target_is_erased_ptr = target_content.tag == .scalar and target_content.getScalar().tag == .opaque_ptr;
        const backing_is_erased_ptr = backing_content.tag == .scalar and backing_content.getScalar().tag == .opaque_ptr;
        const target_is_list = target_content.tag == .list or target_content.tag == .list_of_zst;
        const backing_is_list = backing_content.tag == .list or backing_content.tag == .list_of_zst;
        const boxing_compatible =
            (target_is_box == backing_is_box) or
            (target_is_box and backing_is_erased_ptr) or
            (backing_is_box and target_is_erased_ptr);
        if ((target_is_box or backing_is_box or target_is_erased_ptr or backing_is_erased_ptr) and boxing_compatible and !target_is_list and !backing_is_list) {
            return try self.addAssignRef(target, .{ .nominal = .{ .backing_ref = backing_local } }, next);
        }

        if (target_content.tag == .struct_ and backing_content.tag == .struct_) {
            if (try self.assignStructBoundary(target, target_content, backing_local, backing_content, next)) |converted| {
                return converted;
            }
        }
        if (target_content.tag == .tag_union and backing_content.tag == .tag_union) {
            if (try self.assignTagUnionLayoutBoundary(target, target_content, backing_local, backing_content, next)) |converted| {
                return converted;
            }
        }
        if (try self.layoutsEquivalent(target_layout, backing_layout)) {
            return try self.assignLocal(target, backing_local, next);
        }

        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "postcheck invariant violated: LIR lowering expected nominal layouts to stay on one side of layout boxing, target={d} ({s}) source={d} ({s})",
                .{
                    @intFromEnum(target_layout),
                    @tagName(target_content.tag),
                    @intFromEnum(backing_layout),
                    @tagName(backing_content.tag),
                },
            );
        }
        unreachable;
    }

    fn assignBoxBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        source_layout: layout.Idx,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (target_layout == source_layout) return try self.assignLocal(target, source, next);
        const target_content = self.result.layouts.getLayout(target_layout);
        const source_content = self.result.layouts.getLayout(source_layout);
        if (target_content.eql(source_content)) return try self.assignLocal(target, source, next);
        if (target_content.tag == .box and try self.layoutsEquivalent(target_content.getIdx(), source_layout)) {
            return try self.assignUnaryLowLevel(target, .box_box, source, next);
        }
        if (target_content.tag == .box_of_zst and self.result.layouts.isZeroSized(source_content)) {
            return try self.assignUnaryLowLevel(target, .box_box, source, next);
        }
        if (source_content.tag == .box and try self.layoutsEquivalent(source_content.getIdx(), target_layout)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
        }
        if (source_content.tag == .box_of_zst and self.result.layouts.isZeroSized(target_content)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
        }

        if (target_content.tag == .struct_ and source_content.tag == .struct_) {
            if (try self.assignStructBoundary(target, target_content, source, source_content, next)) |converted| {
                return converted;
            }
        }
        if (target_content.tag == .tag_union and source_content.tag == .tag_union) {
            if (try self.assignTagUnionLayoutBoundary(target, target_content, source, source_content, next)) |converted| {
                return converted;
            }
        }

        if (try self.layoutsEquivalent(target_layout, source_layout)) return try self.assignLocal(target, source, next);

        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(source)) {
                Common.invariant("box boundary tried to store non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }

        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "postcheck invariant violated: LIR lowering expected layouts to match or differ by an explicit Box edge, target={d} ({s}) source={d} ({s})",
                .{
                    @intFromEnum(target_layout),
                    @tagName(target_content.tag),
                    @intFromEnum(source_layout),
                    @tagName(source_content.tag),
                },
            );
        }
        unreachable;
    }

    fn assignStructBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_content: layout.Layout,
        source: LIR.LocalId,
        source_content: layout.Layout,
        next: LIR.CFStmtId,
    ) Common.LowerError!?LIR.CFStmtId {
        const target_info = self.result.layouts.getStructInfo(target_content);
        const source_info = self.result.layouts.getStructInfo(source_content);
        if (target_info.fields.len != source_info.fields.len) return null;

        const field_count = target_info.fields.len;
        const filled = try self.allocator.alloc(bool, field_count);
        defer self.allocator.free(filled);
        @memset(filled, false);

        for (0..field_count) |i| {
            const target_field = target_info.fields.get(i);
            if (target_field.is_padding) return null;
            if (target_field.index >= field_count) return null;
            if (filled[target_field.index]) return null;
            filled[target_field.index] = true;
        }
        for (filled) |was_filled| {
            if (!was_filled) return null;
        }

        const fields = try self.allocator.alloc(LIR.LocalId, field_count);
        defer self.allocator.free(fields);

        for (0..field_count) |i| {
            const target_field = target_info.fields.get(i);
            fields[target_field.index] = try self.addLocalForLayout(target_field.layout);
        }

        var current = try self.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.result.store.addLocalSpan(fields),
            .next = next,
        } });

        var i = field_count;
        while (i > 0) {
            i -= 1;
            const target_field = target_info.fields.get(i);
            const source_field = structFieldByOriginalIndex(source_info.fields, target_field.index) orelse return null;
            if (source_field.is_padding) return null;
            current = try self.assignRefRead(
                fields[target_field.index],
                source_field.layout,
                .{ .field = .{ .source = source, .field_idx = target_field.index } },
                current,
            );
        }

        return current;
    }

    fn assignTagUnionLayoutBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_content: layout.Layout,
        source: LIR.LocalId,
        source_content: layout.Layout,
        next: LIR.CFStmtId,
    ) Common.LowerError!?LIR.CFStmtId {
        if (!try self.layoutsEquivalent(
            self.result.store.getLocal(target).layout_idx,
            self.result.store.getLocal(source).layout_idx,
        )) {
            return null;
        }

        const target_info = self.result.layouts.getTagUnionInfo(target_content);
        const source_info = self.result.layouts.getTagUnionInfo(source_content);
        if (target_info.variants.len != source_info.variants.len) return null;

        if (self.isZstLocal(source)) {
            return try self.assignTagUnionLayoutVariantBoundary(
                target,
                target_info,
                @intCast(0),
                source,
                source_info,
                @intCast(0),
                next,
            );
        }

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, source_info.variants.len);
        defer self.allocator.free(branches);
        for (0..source_info.variants.len) |variant_index| {
            branches[variant_index] = .{
                .value = @intCast(variant_index),
                .body = try self.assignTagUnionLayoutVariantBoundary(
                    target,
                    target_info,
                    @intCast(variant_index),
                    source,
                    source_info,
                    @intCast(variant_index),
                    next,
                ),
            };
        }

        const disc = try self.addLocalForLayout(.u16);
        const impossible = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = impossible,
            .default_is_cold = true,
            .continuation = null,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = disc,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn assignTagUnionLayoutVariantBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_info: layout.TagUnionInfo,
        target_index: u16,
        source: LIR.LocalId,
        source_info: layout.TagUnionInfo,
        source_index: u16,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_payload_layout = target_info.variants.get(target_index).payload_layout;
        const source_payload_layout = source_info.variants.get(source_index).payload_layout;
        const target_payload_content = self.result.layouts.getLayout(target_payload_layout);
        const source_payload_content = self.result.layouts.getLayout(source_payload_layout);
        const target_payload_is_zst = self.result.layouts.isZeroSized(target_payload_content);
        const source_payload_is_zst = self.result.layouts.isZeroSized(source_payload_content);
        if (target_payload_is_zst != source_payload_is_zst) {
            Common.invariant("equivalent tag-union layout boundary saw mismatched zero-sized payloads");
        }
        if (target_payload_is_zst) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = target_index,
                .discriminant = target_index,
                .payload = null,
                .next = next,
            } });
        }

        const target_payload = try self.addLocalForLayout(target_payload_layout);
        const source_payload = try self.addLocalForLayout(source_payload_layout);
        const assign_tag = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = target_index,
                .discriminant = target_index,
                .payload = target_payload,
                .next = next,
            } });

        var current = try self.assignBoxBoundary(target_payload, source_payload, source_payload_layout, assign_tag);
        current = try self.assignRefRead(
            source_payload,
            source_payload_layout,
            .{ .tag_payload_struct = .{
                .source = source,
                .variant_index = source_index,
                .tag_discriminant = source_index,
            } },
            current,
        );
        return current;
    }

    fn structFieldByOriginalIndex(fields: layout.StructField.SafeMultiList.Slice, index: u16) ?layout.StructField {
        for (0..fields.len) |i| {
            const field = fields.get(i);
            if (field.index == index) return field;
        }
        return null;
    }

    fn layoutsEquivalent(self: *Lowerer, lhs: layout.Idx, rhs: layout.Idx) Common.LowerError!bool {
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        return try self.layoutsEquivalentInner(lhs, rhs, &visited);
    }

    fn layoutsEquivalentInner(
        self: *Lowerer,
        lhs_idx: layout.Idx,
        rhs_idx: layout.Idx,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        if (lhs_idx == rhs_idx) return true;
        const key = (@as(u64, @intFromEnum(lhs_idx)) << 32) | @as(u64, @intFromEnum(rhs_idx));
        if (visited.contains(key)) return true;
        try visited.put(key, {});

        const lhs = self.result.layouts.getLayout(lhs_idx);
        const rhs = self.result.layouts.getLayout(rhs_idx);
        if (lhs.eql(rhs)) return true;
        if (lhs.tag != rhs.tag) return false;

        return switch (lhs.tag) {
            .scalar,
            .erased_callable,
            => lhs.eql(rhs),
            .zst,
            .box_of_zst,
            .list_of_zst,
            => true,
            .box,
            .list,
            .ptr,
            => try self.layoutsEquivalentInner(lhs.getIdx(), rhs.getIdx(), visited),
            .closure => try self.layoutsEquivalentInner(lhs.getClosure().captures_layout_idx, rhs.getClosure().captures_layout_idx, visited),
            .struct_ => blk: {
                const lhs_info = self.result.layouts.getStructInfo(lhs);
                const rhs_info = self.result.layouts.getStructInfo(rhs);
                if (lhs_info.alignment != rhs_info.alignment) break :blk false;
                if (lhs_info.size() != rhs_info.size()) break :blk false;
                if (lhs_info.fields.len != rhs_info.fields.len) break :blk false;
                for (0..lhs_info.fields.len) |index| {
                    const lhs_field = lhs_info.fields.get(index);
                    const rhs_field = rhs_info.fields.get(index);
                    if (lhs_field.index != rhs_field.index) break :blk false;
                    if (lhs_field.is_padding != rhs_field.is_padding) break :blk false;
                    if (!try self.layoutsEquivalentInner(lhs_field.layout, rhs_field.layout, visited)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => blk: {
                const lhs_info = self.result.layouts.getTagUnionInfo(lhs);
                const rhs_info = self.result.layouts.getTagUnionInfo(rhs);
                if (lhs_info.alignment != rhs_info.alignment) break :blk false;
                if (lhs_info.size() != rhs_info.size()) break :blk false;
                if (!std.meta.eql(lhs_info.data.discriminant_offset, rhs_info.data.discriminant_offset)) break :blk false;
                if (lhs_info.data.discriminant_size != rhs_info.data.discriminant_size) break :blk false;
                if (lhs_info.variants.len != rhs_info.variants.len) break :blk false;
                for (0..lhs_info.variants.len) |index| {
                    const lhs_variant = lhs_info.variants.get(index);
                    const rhs_variant = rhs_info.variants.get(index);
                    if (!try self.layoutsEquivalentInner(lhs_variant.payload_layout, rhs_variant.payload_layout, visited)) break :blk false;
                }
                break :blk true;
            },
        };
    }

    fn assignRefRead(
        self: *Lowerer,
        target: LIR.LocalId,
        storage_layout: layout.Idx,
        op: LIR.RefOp,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (target_layout == storage_layout) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.addAssignRef(target, op, next);
        }

        const storage_local = try self.addLocalForLayout(storage_layout);
        const after_read = try self.assignBoxBoundary(target, storage_local, storage_layout, next);
        if (self.isZstLocal(storage_local)) return try self.assignZst(storage_local, after_read);
        return try self.addAssignRef(storage_local, op, after_read);
    }

    fn assignTypedRefRead(
        self: *Lowerer,
        target: LIR.LocalId,
        target_ty: Type.TypeId,
        source_ty: Type.TypeId,
        storage_layout: layout.Idx,
        op: LIR.RefOp,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        if (self.layoutsShareRepresentation(target_layout, storage_layout)) {
            return try self.assignRefRead(target, storage_layout, op, next);
        }

        const storage_local = try self.addLocalForLayout(storage_layout);
        try self.local_types.put(storage_local, source_ty);
        var current = try self.assignTypedBoundary(target, target_ty, storage_local, source_ty, next);
        current = try self.assignRefRead(storage_local, storage_layout, op, current);
        return current;
    }

    fn assignUnaryLowLevel(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        arg: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const args = [_]LIR.LocalId{arg};
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&args),
            .next = next,
        } });
    }

    fn lowerLetIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        let_: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const rest = try self.lowerExprIntoAtType(target, let_.rest, result_ty, next);
        const binding_pat = self.pat(let_.bind);
        if (binding_pat.data == .bind) {
            return try self.lowerDirectBindValue(let_.bind, binding_pat.data.bind, let_.value, rest);
        }
        const value_local = try self.addTemp(try self.lowerExprTy(let_.value));
        const bind = try self.bindPatternOrCrash(let_.bind, value_local, rest, let_.comptime_site);
        return try self.lowerExprInto(value_local, let_.value, bind);
    }

    fn lowerDirectBindValue(
        self: *Lowerer,
        pat_id: Lifted.PatId,
        local: Lifted.LocalId,
        value: Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const bind_ty = try self.lowerPatTy(pat_id);
        const value_local = self.existingLocalForTyped(local, bind_ty) orelse try self.addTemp(bind_ty);
        const is_return_candidate = self.return_forwarding_locals.remove(value_local);
        const forwards_return = is_return_candidate and
            !self.return_forwarding_ambiguous and
            self.return_forwarding_repeatable_depth == 0;
        const finishes_ambiguous_group = is_return_candidate and
            self.return_forwarding_ambiguous and
            self.return_forwarding_locals.count() == 0;
        const saved_return_target = self.current_return_target;
        if (forwards_return) self.current_return_target = value_local;
        defer {
            self.current_return_target = saved_return_target;
            if (finishes_ambiguous_group) self.return_forwarding_ambiguous = false;
        }
        return try self.lowerExprIntoAtType(value_local, value, self.typeOfLocalOr(value_local, bind_ty), next);
    }

    fn lowerRecursiveLetStmt(self: *Lowerer, let_: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const pattern = self.solved.lifted.getPat(let_.pat);
        if (pattern.data != .bind) Common.invariant("recursive Monotype let statement must bind one local directly");
        const local = pattern.data.bind;
        const bind_ty = try self.lowerPatTy(let_.pat);
        const binding = try self.bindRecursiveLocalForTyped(local, bind_ty);
        const target = binding.slot;
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const target_content = self.result.layouts.getLayout(target_layout);
        if (target_content.tag != .box) Common.invariant("recursive Monotype let statement must bind a boxed runtime layout");
        const payload_layout = target_content.getIdx();

        const value = try self.addTemp(bind_ty);
        const value_layout = self.result.store.getLocal(value).layout_idx;
        const payload = if (value_layout == payload_layout)
            value
        else
            try self.addLocalForLayout(payload_layout);
        const ptr = try self.addLocalForLayout(.opaque_ptr);
        const unit = try self.addLocalForLayout(.zst);

        var after_store = next;
        if (binding.forward_local) |forward_local| {
            after_store = try self.assignTypedBoundary(
                forward_local,
                self.typeOfLocalOr(forward_local, bind_ty),
                target,
                bind_ty,
                after_store,
            );
        }

        const ptr_store_args = [_]LIR.LocalId{ ptr, payload };
        const ptr_store_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = unit,
            .op = .ptr_store,
            .rc_effect = LIR.LowLevel.ptr_store.rcEffect(),
            .args = try self.result.store.addLocalSpan(&ptr_store_args),
            .next = after_store,
        } });
        var current = ptr_store_stmt;
        if (payload != value) {
            current = try self.assignBoxBoundary(payload, value, value_layout, current);
        }
        current = try self.lowerExprIntoAtType(value, let_.value, bind_ty, current);
        const ptr_cast_stmt = try self.assignUnaryLowLevel(ptr, .ptr_cast, target, current);
        const alloc_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .box_alloc_zeroed,
            .rc_effect = LIR.LowLevel.box_alloc_zeroed.rcEffect(),
            .args = LIR.LocalSpan.empty(),
            .next = ptr_cast_stmt,
        } });
        return alloc_stmt;
    }

    fn lowerFnRefInto(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        fn_id: Lifted.FnId,
        capture_operands: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        return try self.lowerFnRefIntoAtType(target, expr_id, fn_id, capture_operands, try self.lowerExprTy(expr_id), next);
    }

    fn lowerFnRefIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        fn_id: Lifted.FnId,
        capture_operands: anytype,
        ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const captures = self.memberCapturesForExpr(expr_id, fn_id);
        if (self.solved.lifted.typedLocalSpan(self.solved.lifted.getFn(fn_id).captures).len != capture_operands.len) {
            Common.invariant("function reference capture operand count differed from lifted function captures");
        }
        const fn_symbol = self.solved.lifted.getFn(fn_id).symbol;
        const content = self.types.get(ty);
        if (content == .callable) {
            const variants = self.types.fnVariantSpan(content.callable);
            for (0..variants.len) |variant_index| {
                const variant = GuardedList.at(variants, variant_index);
                if (variant.source != fn_symbol) continue;
                return try self.lowerFiniteCallableValueInto(target, @intCast(variant_index), captures, capture_operands, variant.capture_ty, next);
            }
            return Common.invariant("finite callable type did not contain referenced function");
        } else if (content == .erased_fn) {
            const variants = self.types.fnVariantSpan(content.erased_fn.members);
            for (0..variants.len) |variant_index| {
                const variant = GuardedList.at(variants, variant_index);
                if (variant.source != fn_symbol) continue;
                return try self.lowerPackedErasedFnInto(target, variant.target, captures, capture_operands, variant.capture_ty, next);
            }
            return Common.invariant("erased callable type did not contain referenced function");
        } else {
            return Common.invariant("function value lowered to non-callable direct Lambda Mono type");
        }
    }

    fn lowerFiniteCallableValueInto(
        self: *Lowerer,
        target: LIR.LocalId,
        variant_index: u16,
        captures: CaptureSpanId,
        capture_operands: anytype,
        capture_ty: ?Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (capture_ty) |payload_ty| {
            if (self.captureSpan(captures).len != capture_operands.len) {
                Common.invariant("finite callable capture operand count differed from lifted function captures");
            }
            const payload = try self.addTemp(payload_ty);
            if (self.isZstLocal(target) and !self.isZstLocal(payload)) {
                Common.invariant("zero-sized callable layout had a non-zero-sized payload");
            }
            const assign = if (self.isZstLocal(target))
                try self.assignZst(target, next)
            else
                try self.result.store.addCFStmt(.{ .assign_tag = .{
                    .target = target,
                    .variant_index = variant_index,
                    .discriminant = variant_index,
                    .payload = payload,
                    .next = next,
                } });
            return try self.lowerCaptureRecordFromCaptureExprsInto(payload, captures, capture_operands, payload_ty, assign);
        }
        if (capture_operands.len != 0) Common.invariant("finite callable without capture payload carried capture operands");
        if (self.isZstLocal(target)) return try self.assignZst(target, next);
        return try self.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = variant_index,
            .discriminant = variant_index,
            .payload = null,
            .next = next,
        } });
    }

    fn lowerPackedErasedFnInto(
        self: *Lowerer,
        target: LIR.LocalId,
        fn_id: Type.FnId,
        captures: CaptureSpanId,
        capture_operands: anytype,
        capture_ty: ?Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.captureSpan(captures).len != capture_operands.len) {
            Common.invariant("erased callable capture operand count differed from lifted function captures");
        }
        const capture = if (capture_ty) |ty| try self.addTemp(ty) else null;
        const capture_layout = if (capture) |local| self.result.store.getLocal(local).layout_idx else null;
        const reuse = try self.erasedCallableReuseForPack(target, capture_layout);
        const assign = try self.result.store.addCFStmt(.{ .assign_packed_erased_fn = .{
            .target = target,
            .proc = try self.markReachableFn(fn_id),
            .capture = capture,
            .capture_layout = capture_layout,
            .on_drop = self.erasedCallableOnDrop(capture_layout),
            .reuse = reuse,
            .next = next,
        } });
        if (capture_ty) |ty| return try self.lowerCaptureRecordFromCaptureExprsInto(capture.?, captures, capture_operands, ty, assign);
        if (capture_operands.len != 0) Common.invariant("erased callable without capture payload carried capture operands");
        return assign;
    }

    fn erasedCallableReuseForPack(
        self: *Lowerer,
        target: LIR.LocalId,
        capture_layout: ?layout.Idx,
    ) Common.LowerError!?LIR.LocalId {
        if (!try self.sameErasedCaptureShapeForCurrentProc(capture_layout)) return null;
        if (self.current_return_target != target) return null;
        return self.current_erased_reuse;
    }

    fn erasedCallableOnDrop(self: *Lowerer, capture_layout: ?layout.Idx) LIR.ErasedCallableOnDrop {
        const layout_idx = capture_layout orelse return .none;
        const helper_key = layout.RcHelper{ .op = .decref, .layout_idx = layout_idx };
        return if (self.result.layouts.rcHelperPlan(helper_key) == .noop)
            .none
        else
            .{ .rc_helper = helper_key };
    }

    fn sameErasedCaptureShapeForCurrentProc(self: *Lowerer, new_layout: ?layout.Idx) Common.LowerError!bool {
        const fn_id = self.current_fn orelse return false;
        const spec = self.fn_entries.items[@intFromEnum(fn_id)].spec;
        const old_capture_ty = switch (spec.abi) {
            .erased => spec.capture_ty,
            .finite => switch (spec.return_reuse) {
                .none => return false,
                .erased_callable => |capture_ty| capture_ty,
            },
        };
        const old_layout = if (old_capture_ty) |capture_ty| try self.layoutOfType(capture_ty) else null;
        if (old_layout == null or new_layout == null) return old_layout == null and new_layout == null;
        const old_size_align = self.result.layouts.layoutSizeAlign(self.result.layouts.getLayout(old_layout.?));
        const new_size_align = self.result.layouts.layoutSizeAlign(self.result.layouts.getLayout(new_layout.?));
        return old_size_align.size == new_size_align.size and
            old_size_align.alignment.toByteUnits() == new_size_align.alignment.toByteUnits();
    }

    /// Classify whether a result has one statically selected erased-callable
    /// ownership slot. Aggregate siblings are simultaneous and therefore sum;
    /// tag variants are alternatives and may each select their own single
    /// slot. Lists and recursive paths decline reuse because their runtime
    /// multiplicity is not one statically named slot.
    fn erasedResultDemand(self: *Lowerer, ty: Type.TypeId) ErasedResultDemand {
        return self.erasedResultDemandInner(ty, self.types.typeCount());
    }

    fn erasedResultDemandForSimultaneousTypes(self: *Lowerer, tys: anytype) ErasedResultDemand {
        var result: ErasedResultDemand = .none;
        for (0..GuardedList.borrowLen(tys)) |index| {
            result = mergeSimultaneousErasedResultDemand(
                result,
                self.erasedResultDemand(GuardedList.at(tys, index)),
            );
            if (result == .ambiguous) break;
        }
        return result;
    }

    fn singleErasedResultChildIndex(self: *Lowerer, tys: anytype) ?usize {
        var selected: ?usize = null;
        for (0..GuardedList.borrowLen(tys)) |index| {
            switch (self.erasedResultDemand(GuardedList.at(tys, index))) {
                .none => {},
                .single_slot => {
                    if (selected != null) return null;
                    selected = index;
                },
                .ambiguous => return null,
            }
        }
        return selected;
    }

    fn erasedResultDemandInner(
        self: *Lowerer,
        ty: Type.TypeId,
        remaining: usize,
    ) ErasedResultDemand {
        if (remaining == 0) return .ambiguous;
        const next_remaining = remaining - 1;
        return switch (self.types.get(ty)) {
            .erased_fn => .single_slot,
            .named => |named| if (named.backing) |backing|
                self.erasedResultDemandInner(backing.ty, next_remaining)
            else
                .none,
            .box => |elem| self.erasedResultDemandInner(elem, next_remaining),
            .record => |fields_span| blk: {
                var result: ErasedResultDemand = .none;
                const fields = self.types.fieldSpan(fields_span);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    result = mergeSimultaneousErasedResultDemand(
                        result,
                        self.erasedResultDemandInner(GuardedList.at(fields, index).ty, next_remaining),
                    );
                    if (result == .ambiguous) break;
                }
                break :blk result;
            },
            .capture_record => |fields_span| blk: {
                var result: ErasedResultDemand = .none;
                const fields = self.types.captureFieldSpan(fields_span);
                for (0..GuardedList.borrowLen(fields)) |index| {
                    result = mergeSimultaneousErasedResultDemand(
                        result,
                        self.erasedResultDemandInner(GuardedList.at(fields, index).storage_ty, next_remaining),
                    );
                    if (result == .ambiguous) break;
                }
                break :blk result;
            },
            .tuple => |items_span| blk: {
                var result: ErasedResultDemand = .none;
                const items = self.types.span(items_span);
                for (0..GuardedList.borrowLen(items)) |index| {
                    result = mergeSimultaneousErasedResultDemand(
                        result,
                        self.erasedResultDemandInner(GuardedList.at(items, index), next_remaining),
                    );
                    if (result == .ambiguous) break;
                }
                break :blk result;
            },
            .tag_union => |tags_span| blk: {
                var any_single = false;
                const tags = self.types.tagSpan(tags_span);
                for (0..GuardedList.borrowLen(tags)) |tag_index| {
                    var variant: ErasedResultDemand = .none;
                    const payloads = self.types.span(GuardedList.at(tags, tag_index).payloads);
                    for (0..GuardedList.borrowLen(payloads)) |payload_index| {
                        variant = mergeSimultaneousErasedResultDemand(
                            variant,
                            self.erasedResultDemandInner(GuardedList.at(payloads, payload_index), next_remaining),
                        );
                        if (variant == .ambiguous) break;
                    }
                    if (variant == .ambiguous) break :blk .ambiguous;
                    any_single = any_single or variant == .single_slot;
                }
                break :blk if (any_single) .single_slot else .none;
            },
            .list => |elem| switch (self.erasedResultDemandInner(elem, next_remaining)) {
                .none => .none,
                .single_slot, .ambiguous => .ambiguous,
            },
            .primitive,
            .callable,
            .erased_capture_ptr,
            .zst,
            => .none,
        };
    }

    fn lowerDirectProcCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee: Lifted.FnId,
        args: anytype,
        capture_operands: anytype,
        is_cold: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const callee_is_hosted = switch (self.solved.lifted.getFn(callee).body) {
            .roc => false,
            .hosted => true,
        };
        const return_reuse = if (!callee_is_hosted and
            self.erasedResultDemand(result_ty) == .single_slot and
            self.current_return_target == target)
            self.currentErasedReturnReuse()
        else
            ErasedReturnReuse.none;
        const target_fn = switch (return_reuse) {
            .none => try self.ensureOwnFnSpec(callee, .finite),
            .erased_callable => |capture_ty| try self.ensureOwnFnSpecWithErasedReturnReuse(callee, capture_ty),
        };
        const captures = try self.capturesForFn(callee);
        const capture_items = self.captureSpan(captures);
        if (capture_items.len == 0) {
            if (capture_operands.len != 0) Common.invariant("direct call carried capture operands for a capture-free callee");
            return try self.lowerKnownCallInto(target, result_ty, target_fn, args, null, is_cold, next);
        }
        if (capture_items.len != capture_operands.len) Common.invariant("direct call capture operand count differed from callee capture count");
        const capture_ty = self.fn_entries.items[@intFromEnum(target_fn)].spec.capture_ty orelse
            Common.invariant("capturing direct call target had no capture record type");
        const capture_local = try self.addTemp(capture_ty);
        const call = try self.lowerKnownCallInto(target, result_ty, target_fn, args, capture_local, is_cold, next);
        return try self.lowerCaptureRecordFromCaptureExprsInto(capture_local, captures, capture_operands, capture_ty, call);
    }

    fn lowerKnownCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee: Type.FnId,
        args: anytype,
        capture_arg: ?LIR.LocalId,
        is_cold: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const arg_tys = try self.lowerFnSpecArgTypes(callee);
        defer self.allocator.free(arg_tys);

        const lowered = try self.lowerExprsToTempsAtTypes(args, arg_tys);
        defer lowered.deinit(self.allocator);

        const callee_ret_ty = self.fn_entries.items[@intFromEnum(callee)].ret;
        const callee_ret_layout = try self.layoutOfType(callee_ret_ty);
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const call_target = if (target_layout == callee_ret_layout)
            target
        else
            try self.addTemp(callee_ret_ty);
        const after_call = if (call_target == target)
            next
        else
            try self.assignTypedBoundary(target, result_ty, call_target, callee_ret_ty, next);

        const callee_spec = self.fn_entries.items[@intFromEnum(callee)].spec;
        const has_return_reuse = callee_spec.return_reuse.enabled();
        // Destination-specialized helpers carry an explicit hidden ownership
        // input. The ordinary inline path has no representation for that
        // input, so preserve the specialized call boundary until inlining can
        // model destination demand directly.
        if (capture_arg == null and !is_cold and !has_return_reuse) {
            if (try self.inlineBodyForKnownCall(callee)) |body_expr| {
                var current = try self.lowerInlineKnownCallInto(call_target, callee, lowered.ids, body_expr, after_call);
                current = try self.prependExprsAtTypes(lowered, arg_tys, current);
                return current;
            }
        }

        const extra_arg_count = @as(usize, @intFromBool(capture_arg != null)) +
            @as(usize, @intFromBool(has_return_reuse));
        const call_args = if (extra_arg_count > 0) blk: {
            const values = try self.allocator.alloc(LIR.LocalId, lowered.ids.len + extra_arg_count);
            errdefer self.allocator.free(values);
            @memcpy(values[0..lowered.ids.len], lowered.ids);
            var index = lowered.ids.len;
            if (capture_arg) |capture_local| {
                values[index] = capture_local;
                index += 1;
            }
            if (has_return_reuse) {
                values[index] = self.current_erased_reuse orelse
                    Common.invariant("destination-specialized direct call lacked a current erased return destination");
            }
            break :blk values;
        } else lowered.ids;
        defer if (extra_arg_count > 0) self.allocator.free(call_args);
        var current = try self.result.store.addCFStmt(.{ .assign_call = .{
            .target = call_target,
            .proc = try self.markReachableFn(callee),
            .args = try self.result.store.addLocalSpan(call_args),
            .is_cold = is_cold,
            .next = after_call,
        } });
        current = try self.prependExprsAtTypes(lowered, arg_tys, current);
        return current;
    }

    fn currentErasedReturnReuse(self: *Lowerer) ErasedReturnReuse {
        if (self.current_erased_reuse == null) return .none;
        const fn_id = self.current_fn orelse return .none;
        const spec = self.fn_entries.items[@intFromEnum(fn_id)].spec;
        return switch (spec.abi) {
            .erased => .{ .erased_callable = spec.capture_ty },
            .finite => spec.return_reuse,
        };
    }

    fn lowerFnSpecArgTypes(self: *Lowerer, callee: Type.FnId) Common.LowerError![]Type.TypeId {
        const spec = self.fn_specs.items[@intFromEnum(callee)];
        return try self.lowerSolvedFnArgTypes(spec.solved_fn_ty);
    }

    fn lowerCallableArgTypes(self: *Lowerer, ty: Type.TypeId) Common.LowerError![]Type.TypeId {
        const solved_fn_ty = self.callable_source_fn_map.get(ty) orelse
            Common.invariant("callable value call lacked source function type");
        return try self.lowerSolvedFnArgTypes(solved_fn_ty);
    }

    fn lowerSolvedFnArgTypes(self: *Lowerer, solved_fn_ty: SolvedType.TypeVarId) Common.LowerError![]Type.TypeId {
        const content = self.solved.types.rootContent(solved_fn_ty);
        if (content != .func) Common.invariant("call argument lowering saw a non-function source type");
        const func = content.func;
        return try self.lowerTypeSpan(self.solved_types.span(func.args));
    }

    fn inlineBodyForKnownCall(self: *Lowerer, callee: Type.FnId) Common.LowerError!?Lifted.ExprId {
        const spec = self.fn_specs.items[@intFromEnum(callee)];
        const body_expr = self.inline_plan.bodyForFn(spec.source) orelse return null;

        if (spec.abi != .finite) Common.invariant("inline plan selected a non-finite function spec");
        if (spec.capture_ty != null) Common.invariant("inline plan selected a capturing function spec");

        return body_expr;
    }

    fn lowerInlineKnownCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        callee: Type.FnId,
        arg_locals: []const LIR.LocalId,
        body_expr: Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const spec = self.fn_specs.items[@intFromEnum(callee)];
        if (spec.abi != .finite) Common.invariant("attempted to inline a non-finite function spec");
        if (spec.capture_ty != null) Common.invariant("attempted to inline a capturing function spec");

        const source_fn = self.solved.lifted.getFn(spec.source);
        const fn_content = self.solved.types.rootContent(spec.solved_fn_ty);
        if (fn_content != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        const func = fn_content.func;
        const solved_args = self.solved_types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (solved_args.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");
        if (arg_locals.len != lifted_args.len) Common.invariant("inline call argument count differed from function arity");

        const saved = try self.allocator.alloc(?LIR.LocalId, lifted_args.len);
        defer self.allocator.free(saved);
        for (0..lifted_args.len) |i| {
            const arg = GuardedList.at(lifted_args, i);
            saved[i] = self.local_map[@intFromEnum(arg.local)];
        }
        for (0..lifted_args.len) |i| {
            const arg = GuardedList.at(lifted_args, i);
            self.local_map[@intFromEnum(arg.local)] = arg_locals[i];
        }
        defer {
            for (0..lifted_args.len) |i| {
                const arg = GuardedList.at(lifted_args, i);
                self.local_map[@intFromEnum(arg.local)] = saved[i];
            }
        }

        return try self.lowerExprInto(target, body_expr, next);
    }

    fn lowerValueCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee_expr: Lifted.ExprId,
        arg_exprs: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const callee_ty = try self.lowerExprContextTy(callee_expr);
        const content = self.types.get(callee_ty);
        if (content == .callable) {
            return try self.lowerCallableValueCallInto(target, result_ty, callee_expr, callee_ty, content.callable, arg_exprs, next);
        } else if (content == .erased_fn) {
            return try self.lowerErasedValueCallInto(target, result_ty, callee_expr, callee_ty, arg_exprs, next);
        } else {
            return Common.invariant("value call callee had no callable direct Lambda Mono representation");
        }
    }

    fn lowerErasedValueCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee_expr: Lifted.ExprId,
        callee_ty: Type.TypeId,
        arg_exprs: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const arg_tys = try self.lowerCallableArgTypes(callee_ty);
        defer self.allocator.free(arg_tys);

        const args = try self.lowerExprsToTempsAtTypes(arg_exprs, arg_tys);
        defer args.deinit(self.allocator);
        const callee = try self.addTemp(callee_ty);
        const result_layout = try self.layoutOfType(result_ty);
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const call_target = if (target_layout == result_layout)
            target
        else
            try self.addTemp(result_ty);
        const after_call = if (call_target == target)
            next
        else
            try self.assignTypedBoundary(target, result_ty, call_target, result_ty, next);
        const reuse_closure = self.erasedResultDemand(result_ty) == .single_slot;
        const call_stmt = try self.result.store.addCFStmt(.{ .assign_call_erased = .{
            .target = call_target,
            .closure = callee,
            .args = try self.result.store.addLocalSpan(args.ids),
            .arg_layouts = try self.appendErasedArgumentLayouts(args.ids),
            .arg_plan = try self.erasedCallArgsPlan(args.ids),
            .reuse_closure = reuse_closure,
            .reuse_source = if (reuse_closure) callee else null,
            .next = after_call,
        } });
        if (reuse_closure) {
            try self.erased_call_owner_uses.append(self.allocator, .{
                .stmt = call_stmt,
                .closure = callee,
            });
        }
        var current = call_stmt;
        current = try self.prependExprsAtTypes(args, arg_tys, current);
        return try self.lowerExprIntoAtType(callee, callee_expr, callee_ty, current);
    }

    fn erasedCallArgsPlan(self: *Lowerer, args: []const LIR.LocalId) Common.LowerError!LIR.ErasedCallArgsPlanId {
        const arg_layouts = try self.allocator.alloc(layout.Idx, args.len);
        defer self.allocator.free(arg_layouts);
        for (args, arg_layouts) |arg, *arg_layout| {
            arg_layout.* = self.result.store.getLocal(arg).layout_idx;
        }
        return try self.result.store.internErasedCallArgsPlan(&self.result.layouts, arg_layouts);
    }

    /// Resolve the ownership unit consumed by erased calls from provenance
    /// recorded while their definitions were emitted. This deliberately does
    /// not inspect completed control flow: every followed edge is an explicit
    /// representation-transparent producer edge, and ambiguity declines the
    /// outer-owner optimization in favor of consuming `closure` itself.
    fn resolveErasedCallOwnerUses(self: *Lowerer, start: usize) void {
        for (self.erased_call_owner_uses.items[start..]) |use| {
            const call = &self.result.store.getCFStmtPtr(use.stmt).assign_call_erased;
            call.reuse_source = self.resolveErasedOwner(use.closure) orelse use.closure;
        }
        self.erased_call_owner_uses.shrinkRetainingCapacity(start);
    }

    fn resolveErasedOwner(self: *Lowerer, initial: LIR.LocalId) ?LIR.LocalId {
        var current = initial;
        // Local ids form a finite graph. Following at most its node count
        // either reaches a root or proves that the path contains a cycle.
        for (0..self.erased_owner_states.items.len) |_| {
            switch (self.erased_owner_states.items[@intFromEnum(current)]) {
                .pending, .root => {
                    const source_layout = self.result.layouts.getLayout(self.result.store.getLocal(current).layout_idx);
                    return if (self.result.layouts.layoutContainsRefcounted(source_layout)) current else null;
                },
                .alias => |source| current = source,
                .ambiguous => return null,
            }
        }
        return null;
    }

    fn noteErasedOwnerDefinition(self: *Lowerer, target: LIR.LocalId, source: ?LIR.LocalId) void {
        const state = &self.erased_owner_states.items[@intFromEnum(target)];
        state.* = switch (state.*) {
            .pending => if (source) |owner| .{ .alias = owner } else .root,
            .root, .alias, .ambiguous => .ambiguous,
        };
    }

    fn noteErasedOwnerAmbiguous(self: *Lowerer, target: LIR.LocalId) void {
        self.erased_owner_states.items[@intFromEnum(target)] = .ambiguous;
    }

    fn addAssignRef(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.RefOp,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerDefinition(target, self.transparentErasedOwnershipSource(op, target));
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = op,
            .next = next,
        } });
    }

    fn transparentErasedOwnershipSource(self: *Lowerer, op: LIR.RefOp, target: LIR.LocalId) ?LIR.LocalId {
        const source = switch (op) {
            .local => |local| local,
            .nominal => |nominal| nominal.backing_ref,
            inline .tag_payload, .tag_payload_struct => |payload| blk: {
                if (payload.variant_index != 0) break :blk null;
                const source_layout = self.result.layouts.getLayout(self.result.store.getLocal(payload.source).layout_idx);
                if (source_layout.tag != .tag_union) break :blk null;
                const data = self.result.layouts.getTagUnionData(source_layout.getTagUnion().idx);
                if (data.discriminant_size != 0) break :blk null;
                break :blk payload.source;
            },
            .discriminant, .field, .list_reinterpret => null,
        } orelse return null;
        return if (self.sameTransparentPointerRepresentation(source, target)) source else null;
    }

    fn sameTransparentPointerRepresentation(self: *Lowerer, source: LIR.LocalId, target: LIR.LocalId) bool {
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const source_size_align = self.result.layouts.layoutSizeAlign(self.result.layouts.getLayout(source_layout));
        const target_size_align = self.result.layouts.layoutSizeAlign(self.result.layouts.getLayout(target_layout));
        return source_size_align.size == self.result.layouts.targetUsize().size() and
            source_size_align.size == target_size_align.size;
    }

    fn lowerCallableValueCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee_expr: Lifted.ExprId,
        callee_ty: Type.TypeId,
        variants_span: Type.Span,
        arg_exprs: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const callee = try self.addTemp(callee_ty);
        const done = self.freshJoinPointId();
        // Branch lowering can lower argument types that append more variants to
        // self.types, so keep this iteration independent of the store backing.
        const variants = try GuardedList.dupe(self.allocator, Type.FnVariant, self.types.fnVariantSpan(variants_span));
        defer self.allocator.free(variants);
        var current = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        var i = variants.len;
        while (i > 0) {
            i -= 1;
            const variant: Type.FnVariant = variants[i];
            const variant_index: u16 = @intCast(i);
            const branch_done = try self.joinJump(done);
            const branch_body = try self.lowerCallableVariantCallInto(target, result_ty, callee, variant, variant_index, arg_exprs, branch_done);
            current = try self.discriminantSwitch(callee, variant_index, branch_body, current, false);
        }
        const remainder = try self.lowerExprIntoAtType(callee, callee_expr, callee_ty, current);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = remainder,
        } });
    }

    fn lowerCallableVariantCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee: LIR.LocalId,
        variant: Type.FnVariant,
        variant_index: u16,
        arg_exprs: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (variant.capture_ty) |_| {
            const payload = try self.addLocalForLayout(self.tagUnionPayloadLayout(self.result.store.getLocal(callee).layout_idx, variant_index));
            const call = try self.lowerKnownCallInto(target, result_ty, variant.target, arg_exprs, payload, false, next);
            if (self.isZstLocal(payload)) return call;
            return try self.addAssignRef(payload, .{ .tag_payload_struct = .{
                .source = callee,
                .variant_index = variant_index,
                .tag_discriminant = variant_index,
            } }, call);
        }
        return try self.lowerKnownCallInto(target, result_ty, variant.target, arg_exprs, null, false, next);
    }

    fn lowerLowLevelInto(self: *Lowerer, target: LIR.LocalId, op: can.CIR.Expr.LowLevel, span: Lifted.Span(Lifted.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const args = self.solved.lifted.exprSpan(span);
        if (op == .crash) {
            if (args.len != 1) Common.invariant("crash low-level operation received the wrong number of arguments");
            const lowered = try self.lowerExprsToTemps(args);
            defer lowered.deinit(self.allocator);
            const crash_stmt = try self.result.store.addCFStmt(.{ .crash = .{
                .msg = .{ .local = lowered.ids[0] },
            } });
            return try self.prependExprs(lowered, crash_stmt);
        }
        if (op == .dict_pseudo_seed and self.dict_seed_mode == .comptime_zero) {
            if (args.len != 0) Common.invariant("Dict seed low-level operation received arguments");
            return try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i64_literal = .{
                    .value = 0,
                    .layout_idx = self.result.store.getLocal(target).layout_idx,
                } },
                .next = next,
            } });
        }
        if (op == .box_box or op == .box_unbox) {
            return try self.lowerBoxBoundaryLowLevelInto(target, op, args, next);
        }
        if (op == .list_map_can_reuse) {
            const interchangeable = try self.listMapLayoutsInterchangeable(args);
            if (!interchangeable.get(.u32) and !interchangeable.get(.u64)) {
                // Reuse is statically impossible (or disabled) on every
                // width, so the runtime uniqueness check never needs to
                // run. The op's arguments are pure local reads, so dropping
                // them is safe.
                return try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .i64_literal = .{
                        .value = 0,
                        .layout_idx = self.result.store.getLocal(target).layout_idx,
                    } },
                    .next = next,
                } });
            }
            // Reuse is possible on at least one width; carry the per-width
            // bits so codegen forces a constant 0 on any width where the
            // layouts are not interchangeable.
            const lowered = try self.lowerExprsToTemps(args);
            defer lowered.deinit(self.allocator);
            var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.result.store.addLocalSpan(lowered.ids),
                .interchangeable = interchangeable,
                .next = next,
            } });
            current = try self.prependExprs(lowered, current);
            return current;
        }
        const lowered = try self.lowerExprsToTemps(args);
        defer lowered.deinit(self.allocator);
        const lowered_op = if (lowered.ids.len > 0)
            CheckedArithmetic.lowerOp(op, self.result.store.getLocal(lowered.ids[0]).layout_idx)
        else
            op;
        var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = lowered_op,
            .rc_effect = lowered_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(lowered.ids),
            .next = next,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    /// Compile-time half of the `list_map_can_reuse` decision, computed for
    /// both pointer widths. A width's bit is true when the input and output
    /// element layouts of a `List.map` call are interchangeable in one
    /// allocation on that width—same element stride, same allocation
    /// alignment class, and same refcounted-elements header shape—in which
    /// case the runtime uniqueness check decides at that width. A false bit
    /// means reuse is statically impossible (or the optimization is disabled)
    /// on that width, so the op resolves to a constant 0 there. Both bits are
    /// stored so the lowered op is target-independent: codegen selects the bit
    /// for the width it is building.
    fn listMapLayoutsInterchangeable(self: *Lowerer, args: anytype) Common.LowerError!layout.WidthValues(bool) {
        const none = layout.WidthValues(bool).both(false, false);
        if (args.len != 2) Common.invariant("list_map_can_reuse reached LIR lowering with the wrong arity");
        if (!self.list_in_place_map) return none;

        const list_layout_idx = try self.layoutOfType(try self.lowerExprTy(GuardedList.at(args, 0)));
        const list_layout = self.result.layouts.getLayout(list_layout_idx);
        if (list_layout.tag != .list) return none;
        const in_elem_idx = self.result.layouts.runtimeRepresentationLayoutIdx(list_layout.getIdx());

        const transform_ty = self.solved.expr_tys.items[@intFromEnum(GuardedList.at(args, 1))];
        const transform_root = self.solved.types.root(transform_ty);
        const transform_content = self.solved.types.get(transform_root);
        if (transform_content != .func) Common.invariant("list_map_can_reuse transform argument is not a function");
        const out_ret = transform_content.func.ret;
        const out_elem_idx = self.result.layouts.runtimeRepresentationLayoutIdx(
            try self.layoutOfType(try self.lowerType(out_ret)),
        );

        return layout.WidthValues(bool).both(
            self.listMapInterchangeableAtWidth(in_elem_idx, out_elem_idx, .u32),
            self.listMapInterchangeableAtWidth(in_elem_idx, out_elem_idx, .u64),
        );
    }

    /// Whether the input and output element layouts share one allocation at the
    /// given pointer width. See `listMapLayoutsInterchangeable`.
    fn listMapInterchangeableAtWidth(
        self: *Lowerer,
        in_elem_idx: layout.Idx,
        out_elem_idx: layout.Idx,
        target_usize: base.target.TargetUsize,
    ) bool {
        const layouts = &self.result.layouts;
        const in_elem = layouts.getLayout(in_elem_idx);
        // A list of zero-sized elements has no allocation to reuse.
        const in_size = layouts.sizeAt(in_elem, target_usize);
        if (in_size == 0) return false;

        const out_elem = layouts.getLayout(out_elem_idx);
        if (layouts.sizeAt(out_elem, target_usize) != in_size) return false;

        // The allocation's hidden header and the alignment passed to the
        // allocator both derive from max(ptr_width, element alignment) and
        // from whether elements are refcounted (which reserves an extra
        // element-count slot for seamless slices). Both must agree or a
        // later free would compute the wrong allocation pointer.
        const ptr_width = target_usize.size();
        const in_alignment: u32 = @intCast(in_elem.alignment(target_usize).toByteUnits());
        const out_alignment: u32 = @intCast(out_elem.alignment(target_usize).toByteUnits());
        if (@max(ptr_width, in_alignment) != @max(ptr_width, out_alignment)) return false;

        if (layouts.layoutContainsRefcounted(in_elem) != layouts.layoutContainsRefcounted(out_elem)) return false;

        return true;
    }

    fn lowerBoxBoundaryLowLevelInto(self: *Lowerer, target: LIR.LocalId, op: can.CIR.Expr.LowLevel, args: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (args.len != 1) Common.invariant("Box low-level operation reached LIR lowering with the wrong arity");
        const arg = GuardedList.at(args, 0);
        const source = try self.addTemp(try self.lowerExprTy(arg));
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const saved_return_target = self.current_return_target;
        if (saved_return_target == target and op == .box_box) {
            self.current_return_target = source;
        }
        defer self.current_return_target = saved_return_target;
        if (op != .box_box and op != .box_unbox) unreachable;
        const assign = try self.assignBoxBoundary(target, source, source_layout, next);
        return try self.lowerExprInto(source, arg, assign);
    }

    fn lowerFieldAccessInto(
        self: *Lowerer,
        target: LIR.LocalId,
        receiver: Lifted.ExprId,
        segments_span: Lifted.Span(Lifted.FieldAccessSegment),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const segments = self.solved.lifted.fieldAccessSegmentSpan(segments_span);
        if (segments.len == 0) Common.invariant("field access path had no segments");

        const receiver_ty = try self.lowerExprContextTy(receiver);
        const receiver_local = try self.addTemp(receiver_ty);

        const FieldRead = struct {
            target: LIR.LocalId,
            target_ty: Type.TypeId,
            source: LIR.LocalId,
            source_ty: Type.TypeId,
            source_field_index: u16,
            storage_layout: ?layout.Idx,
        };
        const reads = try self.allocator.alloc(FieldRead, segments.len);
        defer self.allocator.free(reads);

        var logical_source_ty = receiver_ty;
        var source_local = receiver_local;
        for (0..segments.len) |index| {
            const segment = GuardedList.at(segments, index);
            const target_field_index = self.recordFieldIndex(logical_source_ty, segment.field);
            const target_fields = self.recordFields(logical_source_ty);
            const target_field_ty = GuardedList.at(target_fields, @intCast(target_field_index)).ty;
            const destination = if (index + 1 == segments.len)
                target
            else
                try self.addTemp(target_field_ty);

            const storage_source_ty = self.storageTypeOfLocalOr(source_local, logical_source_ty);
            const source_field_index = self.recordFieldIndex(storage_source_ty, segment.field);
            const source_fields = self.recordFields(storage_source_ty);
            const source_field_ty = GuardedList.at(source_fields, @intCast(source_field_index)).ty;
            reads[index] = .{
                .target = destination,
                .target_ty = target_field_ty,
                .source = source_local,
                .source_ty = source_field_ty,
                .source_field_index = source_field_index,
                .storage_layout = if (self.isZstLocal(destination))
                    null
                else
                    self.localFieldLayout(source_local, source_field_index),
            };

            source_local = destination;
            logical_source_ty = target_field_ty;
        }

        var current = next;
        var index = reads.len;
        while (index > 0) {
            index -= 1;
            const read = reads[index];
            current = if (read.storage_layout) |storage_layout|
                try self.assignTypedRefRead(
                    read.target,
                    read.target_ty,
                    read.source_ty,
                    storage_layout,
                    .{ .field = .{ .source = read.source, .field_idx = read.source_field_index } },
                    current,
                )
            else
                try self.assignZst(read.target, current);
        }
        return try self.lowerExprIntoAtType(receiver_local, receiver, receiver_ty, current);
    }

    fn lowerTupleAccessInto(self: *Lowerer, target: LIR.LocalId, tuple: Lifted.ExprId, elem_index: u32, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const tuple_ty = try self.lowerExprContextTy(tuple);
        const tuple_local = try self.addTemp(tuple_ty);
        const field_index: u16 = @intCast(elem_index);
        const target_item_tys = self.tupleItemTypes(tuple_ty);
        const source_item_tys = self.tupleItemTypes(self.storageTypeOfLocalOr(tuple_local, tuple_ty));
        if (self.isZstLocal(target)) {
            return try self.lowerExprIntoAtType(tuple_local, tuple, tuple_ty, try self.assignZst(target, next));
        }
        const assign = try self.assignTypedRefRead(
            target,
            GuardedList.at(target_item_tys, elem_index),
            GuardedList.at(source_item_tys, elem_index),
            self.localFieldLayout(tuple_local, field_index),
            .{ .field = .{ .source = tuple_local, .field_idx = field_index } },
            next,
        );
        return try self.lowerExprIntoAtType(tuple_local, tuple, tuple_ty, assign);
    }

    fn lowerStructuralEqInto(self: *Lowerer, target: LIR.LocalId, lhs: Lifted.ExprId, rhs: Lifted.ExprId, negated: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const lhs_ty = try self.lowerExprTy(lhs);
        const lhs_local = try self.addTemp(lhs_ty);
        const rhs_local = try self.addTemp(try self.lowerExprTy(rhs));
        var current = try self.lowerEqLocalsInto(target, lhs_local, rhs_local, lhs_ty, negated, next);
        current = try self.lowerExprInto(rhs_local, rhs, current);
        return try self.lowerExprInto(lhs_local, lhs, current);
    }

    fn lowerStructuralHashInto(self: *Lowerer, target: LIR.LocalId, value: Lifted.ExprId, hasher: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const value_ty = try self.lowerExprTy(value);
        const value_local = try self.addTemp(value_ty);
        const hasher_local = try self.addTemp(try self.lowerExprTy(hasher));
        var current = try self.lowerHashLocalsInto(target, value_local, hasher_local, value_ty, next);
        current = try self.lowerExprInto(hasher_local, hasher, current);
        return try self.lowerExprInto(value_local, value, current);
    }

    /// Feed `value` (of `value_ty`) into `hasher`, writing the resulting Hasher
    /// into `target`. Aggregate types are decomposed in Monotype lowering, so
    /// this only ever sees a scalar/str/zst leaf or a transparent nominal wrapper.
    fn lowerHashLocalsInto(self: *Lowerer, target: LIR.LocalId, value: LIR.LocalId, hasher: LIR.LocalId, value_ty: Type.TypeId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return switch (self.types.get(value_ty)) {
            .primitive => |primitive| try self.lowerPrimitiveHashLocalsInto(target, value, hasher, primitive, next),
            // Hashing a zero-sized value contributes nothing; thread the hasher through.
            .zst => try self.assignLocal(target, hasher, next),
            .named => |named| blk: {
                const backing = named.backing orelse Common.invariant("named hash reached direct LIR without runtime backing");
                break :blk try self.lowerHashLocalsInto(target, value, hasher, backing.ty, next);
            },
            .record,
            .tuple,
            .tag_union,
            .capture_record,
            .list,
            .box,
            .callable,
            .erased_fn,
            .erased_capture_ptr,
            => Common.invariant("aggregate/owned hash reached direct LIR structural hash lowering"),
        };
    }

    fn lowerPrimitiveHashLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        value: LIR.LocalId,
        hasher: LIR.LocalId,
        primitive: MonoType.Primitive,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const op = Common.hasherWriteOp(primitive);
        const args = [_]LIR.LocalId{ hasher, value };
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&args),
            .next = next,
        } });
    }

    fn lowerMatchInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        scrutinee: Lifted.ExprId,
        branches_span: Lifted.Span(Lifted.Branch),
        comptime_site: ?Lifted.ComptimeSiteId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (try self.foldListMapCanReuseMatch(target, result_ty, scrutinee, branches_span, next)) |folded| return folded;
        self.noteErasedOwnerAmbiguous(target);
        const scrutinee_ty = try self.lowerExprContextTy(scrutinee);
        const scrutinee_local = try self.addTemp(scrutinee_ty);
        const branches = try GuardedList.dupe(self.allocator, Lifted.Branch, self.solved.lifted.branchSpan(branches_span));
        defer self.allocator.free(branches);
        const done = self.freshJoinPointId();
        const branch_chain = try self.lowerBranchTree(scrutinee_local, scrutinee_ty, branches, target, result_ty, done, comptime_site);
        const remainder = try self.lowerExprIntoAtType(scrutinee_local, scrutinee, scrutinee_ty, branch_chain);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = remainder,
        } });
    }

    /// When the in-place `List.map` branch is statically impossible—the
    /// optimization is disabled or the element layouts are not
    /// interchangeable—lower only the branch a constant-0
    /// `list_map_can_reuse` selects, so the in-place machinery never reaches
    /// LIR. Each fold is recorded as explicit data; the debug Lambda Mono
    /// materializer replays the recorded resolutions instead of recomputing
    /// them (it runs before layout selection and has no layout store), so
    /// the two derivations demand the same set of functions.
    fn foldListMapCanReuseMatch(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        scrutinee: Lifted.ExprId,
        branches_span: Lifted.Span(Lifted.Branch),
        next: LIR.CFStmtId,
    ) Common.LowerError!?LIR.CFStmtId {
        const match = self.solved.lifted.listMapCanReuseMatch(scrutinee, branches_span) orelse return null;
        const args = self.solved.lifted.exprSpan(match.call_args);
        const interchangeable = try self.listMapLayoutsInterchangeable(args);
        if (interchangeable.get(.u32) or interchangeable.get(.u64)) {
            // Reuse is possible on at least one width; keep the match so each
            // backend can decide per the width it is building.
            return null;
        }
        if (builtin.mode == .Debug) {
            try self.folded_map_matches.append(self.allocator, .{
                .scrutinee = scrutinee,
                .body = match.zero_branch_body,
            });
        }
        return try self.lowerExprIntoAtType(target, match.zero_branch_body, result_ty, next);
    }

    fn lowerIfInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        branches_span: Lifted.Span(Lifted.IfBranch),
        final_else: Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const branches = self.solved.lifted.ifBranchSpan(branches_span);
        const done = self.freshJoinPointId();
        var current = try self.lowerExprIntoAtType(target, final_else, result_ty, try self.joinJump(done));
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            const branch = GuardedList.at(branches, i);
            const body = try self.lowerExprIntoAtType(target, branch.body, result_ty, try self.joinJump(done));
            const cond_local = try self.addTemp(try self.lowerExprTy(branch.cond));
            const switch_stmt = try self.boolSwitchNoContinuation(cond_local, body, current);
            current = try self.lowerExprInto(cond_local, branch.cond, switch_stmt);
        }
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = current,
        } });
    }

    fn lowerInitializedPayloadSwitchInto(
        self: *Lowerer,
        target: LIR.LocalId,
        payload_switch: Lifted.InitializedPayloadSwitch,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const done = self.freshJoinPointId();
        const branch_done = try self.joinJump(done);
        var initialized_body: LIR.CFStmtId = undefined;
        var uninitialized_body: LIR.CFStmtId = undefined;
        if (payload_switch.initialized == payload_switch.uninitialized) {
            const shared = try self.lowerExprInto(target, payload_switch.initialized, branch_done);
            initialized_body = shared;
            uninitialized_body = shared;
        } else {
            initialized_body = try self.lowerExprInto(target, payload_switch.initialized, branch_done);
            uninitialized_body = try self.lowerExprInto(target, payload_switch.uninitialized, branch_done);
        }
        const cond_data = self.solved.lifted.getExpr(payload_switch.cond);
        const cond_local = if (cond_data.data == .local)
            try self.localFor(cond_data.data.local)
        else
            try self.addTemp(try self.lowerExprTy(payload_switch.cond));
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_initialized_payload = .{
            .cond = cond_local,
            .cond_mask = payload_switch.cond_mask,
            .payload = try self.localFor(payload_switch.payload),
            .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
            .initialized_branch = initialized_body,
            .uninitialized_branch = uninitialized_body,
        } });
        const remainder = if (cond_data.data == .local)
            switch_stmt
        else
            try self.lowerExprInto(cond_local, payload_switch.cond, switch_stmt);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = remainder,
        } });
    }

    fn lowerTrySequenceInto(
        self: *Lowerer,
        target: LIR.LocalId,
        out_try_ty: Type.TypeId,
        sequence: Mono.TrySequence,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const input_try_ty = try self.lowerExprTy(sequence.try_expr);
        const input_try_local = try self.addTemp(input_try_ty);

        const ok_variant = self.tagIndexByText(input_try_ty, "Ok");
        const err_variant = self.tagIndexByText(input_try_ty, "Err");
        const out_err_variant = self.tagIndexByText(out_try_ty, "Err");

        const ok_body = try self.lowerExprIntoAtType(target, sequence.ok_body, out_try_ty, next);
        const ok_payload_local = try self.localFor(sequence.ok_local);
        const ok_start = if (self.isZstLocal(ok_payload_local))
            try self.assignZst(ok_payload_local, ok_body)
        else
            try self.addAssignRef(ok_payload_local, .{ .tag_payload_struct = .{
                .source = input_try_local,
                .variant_index = ok_variant,
                .tag_discriminant = ok_variant,
            } }, ok_body);

        const out_backing_ty = self.runtimeBackingType(out_try_ty);
        const out_backing_layout = try self.layoutOfType(out_backing_ty);
        const out_backing_local = try self.addLocalForLayout(out_backing_layout);
        const boundary = try self.assignNominalBoundary(target, out_backing_local, out_backing_layout, next);

        const err_payload_layout = self.tagUnionPayloadLayout(self.result.store.getLocal(input_try_local).layout_idx, err_variant);
        const err_payload_local = try self.addLocalForLayout(err_payload_layout);
        const err_tag = if (self.isZstLocal(out_backing_local))
            try self.assignZst(out_backing_local, boundary)
        else
            try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = out_backing_local,
                .variant_index = out_err_variant,
                .discriminant = out_err_variant,
                .payload = err_payload_local,
                .next = boundary,
            } });
        const err_start = if (self.isZstLocal(err_payload_local))
            err_tag
        else
            try self.addAssignRef(err_payload_local, .{ .tag_payload_struct = .{
                .source = input_try_local,
                .variant_index = err_variant,
                .tag_discriminant = err_variant,
            } }, err_tag);

        const switch_stmt = try self.discriminantSwitch(input_try_local, ok_variant, ok_start, err_start, sequence.err_is_cold);
        return try self.lowerExprInto(input_try_local, sequence.try_expr, switch_stmt);
    }

    fn lowerTryRecordSequenceInto(
        self: *Lowerer,
        target: LIR.LocalId,
        out_try_ty: Type.TypeId,
        sequence: Mono.TryRecordSequence,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const input_try_ty = try self.lowerExprTy(sequence.try_expr);
        const input_try_local = try self.addTemp(input_try_ty);

        const ok_variant = self.tagIndexByText(input_try_ty, "Ok");
        const err_variant = self.tagIndexByText(input_try_ty, "Err");
        const out_err_variant = self.tagIndexByText(out_try_ty, "Err");
        const ok_payload_ty = self.singleTagPayloadTypeByIndex(input_try_ty, ok_variant);

        var ok_start = try self.lowerExprIntoAtType(target, sequence.ok_body, out_try_ty, next);
        ok_start = try self.bindTryRecordPayloadField(
            sequence.rest_local,
            input_try_local,
            ok_variant,
            ok_payload_ty,
            sequence.rest_field,
            ok_start,
        );
        ok_start = try self.bindTryRecordPayloadField(
            sequence.value_local,
            input_try_local,
            ok_variant,
            ok_payload_ty,
            sequence.value_field,
            ok_start,
        );

        const out_backing_ty = self.runtimeBackingType(out_try_ty);
        const out_backing_layout = try self.layoutOfType(out_backing_ty);
        const out_backing_local = try self.addLocalForLayout(out_backing_layout);
        const boundary = try self.assignNominalBoundary(target, out_backing_local, out_backing_layout, next);

        const err_payload_layout = self.tagUnionPayloadLayout(self.result.store.getLocal(input_try_local).layout_idx, err_variant);
        const err_payload_local = try self.addLocalForLayout(err_payload_layout);
        const err_tag = if (self.isZstLocal(out_backing_local))
            try self.assignZst(out_backing_local, boundary)
        else
            try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = out_backing_local,
                .variant_index = out_err_variant,
                .discriminant = out_err_variant,
                .payload = err_payload_local,
                .next = boundary,
            } });
        const err_start = if (self.isZstLocal(err_payload_local))
            err_tag
        else
            try self.addAssignRef(err_payload_local, .{ .tag_payload_struct = .{
                .source = input_try_local,
                .variant_index = err_variant,
                .tag_discriminant = err_variant,
            } }, err_tag);

        const switch_stmt = try self.discriminantSwitch(input_try_local, ok_variant, ok_start, err_start, sequence.err_is_cold);
        return try self.lowerExprInto(input_try_local, sequence.try_expr, switch_stmt);
    }

    fn bindTryRecordPayloadField(
        self: *Lowerer,
        local: Mono.LocalId,
        input_try_local: LIR.LocalId,
        ok_variant: u16,
        ok_payload_ty: Type.TypeId,
        field_name: Type.names.RecordFieldNameId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target = try self.localFor(local);
        if (self.isZstLocal(target)) return try self.assignZst(target, next);
        const field_index = self.recordFieldIndex(ok_payload_ty, field_name);
        return try self.addAssignRef(target, .{ .tag_payload = .{
            .source = input_try_local,
            .payload_idx = field_index,
            .variant_index = ok_variant,
            .tag_discriminant = ok_variant,
        } }, next);
    }

    fn lowerBlockIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        stmts_span: Lifted.Span(Lifted.StmtId),
        final_expr: Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const stmts = self.solved.lifted.stmtSpan(stmts_span);
        var current = if (self.solved.lifted.getExpr(final_expr).data == .@"unreachable") blk: {
            if (stmts.len == 0) {
                Common.invariant("unreachable block-final marker had no preceding terminating statement");
            }
            // Monotype emits this marker only after its checked, explicit
            // statement-termination relation has stopped the block. Its
            // block-final position is the capability to erase it; the
            // generic expression path rejects the marker.
            break :blk next;
        } else try self.lowerExprIntoAtType(target, final_expr, result_ty, next);
        var i = stmts.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerStmt(GuardedList.at(stmts, i), current);
        }
        return current;
    }

    fn lowerStmt(self: *Lowerer, stmt_id: Lifted.StmtId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
        const saved_inline_scope = self.result.store.current_inline_scope;
        defer self.result.store.current_inline_scope = saved_inline_scope;
        self.result.store.current_inline_scope = lirInlineScopeId(self.solved.lifted.stmtInlineScope(stmt_id));
        const stmt_loc = self.solved.lifted.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) {
            self.result.store.current_loc = stmt_loc;
        }
        const stmt_region = self.solved.lifted.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) {
            self.result.store.current_region = stmt_region;
        }
        return switch (self.solved.lifted.getStmt(stmt_id)) {
            .uninitialized => |pat_id| try self.initUninitializedPattern(pat_id, next),
            .let_ => |let_| blk: {
                if (let_.recursive) break :blk try self.lowerRecursiveLetStmt(let_, next);
                const binding_pat = self.pat(let_.pat);
                if (binding_pat.data == .bind) {
                    break :blk try self.lowerDirectBindValue(let_.pat, binding_pat.data.bind, let_.value, next);
                }
                const value = try self.addTemp(try self.lowerExprTy(let_.value));
                const bind = try self.bindPatternOrCrash(let_.pat, value, next, let_.comptime_site);
                break :blk try self.lowerExprInto(value, let_.value, bind);
            },
            .expr => |expr_id| blk: {
                const temp = try self.addTemp(try self.lowerExprTy(expr_id));
                break :blk try self.lowerExprInto(temp, expr_id, next);
            },
            .expect => |expr_id| if (self.inline_expects == .omit) next else try self.lowerExpectStmt(expr_id, next),
            .dbg => |expr_id| blk: {
                const temp = try self.addTemp(try self.lowerExprTy(expr_id));
                const debug_stmt = try self.result.store.addCFStmt(.{ .debug = .{ .message = temp, .next = next } });
                break :blk try self.lowerExprInto(temp, expr_id, debug_stmt);
            },
            .return_ => |ret| try self.lowerReturn(ret),
            .crash => |msg| try self.result.store.addCFStmt(.{ .crash = .{
                .msg = .{ .literal = try self.result.store.insertString(self.stringLiteralText(msg)) },
            } }),
        };
    }

    fn initUninitializedPattern(
        self: *Lowerer,
        pat_id: Lifted.PatId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const pat_data = self.pat(pat_id);
        const pat_ty = try self.lowerPatTy(pat_id);
        return switch (pat_data.data) {
            .bind => |local| try self.initUninitializedLocal(try self.bindLocalForTyped(local, pat_ty), next),
            .wildcard,
            .int_lit,
            .dec_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .str_lit,
            => next,
            .str_pattern => |str| blk: {
                var current = next;
                const steps = self.solved.lifted.strPatternStepSpan(str.steps);
                var i = steps.len;
                while (i > 0) {
                    i -= 1;
                    if (GuardedList.at(steps, i).capture) |capture| {
                        current = try self.initUninitializedPattern(capture, current);
                    }
                }
                break :blk current;
            },
            .as => |as| blk: {
                const inner = try self.initUninitializedPattern(as.pattern, next);
                break :blk try self.initUninitializedLocal(try self.bindLocalForTyped(as.local, pat_ty), inner);
            },
            .record => |fields| blk: {
                var current = next;
                const destructs = self.solved.lifted.recordDestructSpan(fields);
                var i = destructs.len;
                while (i > 0) {
                    i -= 1;
                    current = try self.initUninitializedPattern(GuardedList.at(destructs, i).pattern, current);
                }
                break :blk current;
            },
            .tuple => |items| blk: {
                var current = next;
                const pats = self.solved.lifted.patSpan(items);
                var i = pats.len;
                while (i > 0) {
                    i -= 1;
                    current = try self.initUninitializedPattern(GuardedList.at(pats, i), current);
                }
                break :blk current;
            },
            .list => |list| blk: {
                var current = next;
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| current = try self.initUninitializedPattern(rest_pattern, current);
                }
                const pats = self.solved.lifted.patSpan(list.patterns);
                var i = pats.len;
                while (i > 0) {
                    i -= 1;
                    current = try self.initUninitializedPattern(GuardedList.at(pats, i), current);
                }
                break :blk current;
            },
            .tag => |tag| blk: {
                var current = next;
                const payloads = self.solved.lifted.patSpan(tag.payloads);
                var i = payloads.len;
                while (i > 0) {
                    i -= 1;
                    current = try self.initUninitializedPattern(GuardedList.at(payloads, i), current);
                }
                break :blk current;
            },
            .nominal => |inner| try self.initUninitializedPattern(inner, next),
        };
    }

    fn initUninitializedLocal(
        self: *Lowerer,
        target: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(target)) return next;
        return try self.result.store.addCFStmt(.{ .init_uninitialized = .{
            .target = target,
            .next = next,
        } });
    }

    fn lowerLoopIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        loop: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const saved_return_target = self.current_return_target;
        self.current_return_target = null;
        self.return_forwarding_repeatable_depth += 1;
        defer {
            self.return_forwarding_repeatable_depth -= 1;
            self.current_return_target = saved_return_target;
        }
        const params = self.solved.lifted.typedLocalSpan(loop.params);
        const initial_values = self.solved.lifted.exprSpan(loop.initial_values);
        if (params.len != initial_values.len) Common.invariant("Lambda Mono loop parameter count differs from initial value count");

        const param_locals = try self.allocator.alloc(LIR.LocalId, params.len);
        defer self.allocator.free(param_locals);
        const param_tys = try self.allocator.alloc(Type.TypeId, params.len);
        defer self.allocator.free(param_tys);
        for (0..params.len) |i| {
            const param = GuardedList.at(params, i);
            param_tys[i] = try self.lowerLocalTy(param.local);
            param_locals[i] = try self.bindLocalForTyped(param.local, param_tys[i]);
        }
        const param_span = try self.result.store.addLocalSpan(param_locals);
        var maybe_uninitialized_params: std.ArrayList(LIR.LocalId) = .empty;
        defer maybe_uninitialized_params.deinit(self.allocator);
        var maybe_uninitialized_conditions: std.ArrayList(LIR.LocalId) = .empty;
        defer maybe_uninitialized_conditions.deinit(self.allocator);
        var maybe_uninitialized_condition_masks: std.ArrayList(u64) = .empty;
        defer maybe_uninitialized_condition_masks.deinit(self.allocator);
        for (0..initial_values.len) |i| {
            const initial = GuardedList.at(initial_values, i);
            const param_local = param_locals[i];
            const initial_data = self.solved.lifted.getExpr(initial).data;
            if (initial_data == .uninitialized_payload) {
                const payload = initial_data.uninitialized_payload;
                try maybe_uninitialized_params.append(self.allocator, param_local);
                try maybe_uninitialized_conditions.append(self.allocator, try self.localFor(payload.condition));
                try maybe_uninitialized_condition_masks.append(self.allocator, payload.mask);
            }
        }
        const maybe_uninitialized_span = try self.result.store.addLocalSpan(maybe_uninitialized_params.items);
        const maybe_uninitialized_conditions_span = try self.result.store.addLocalSpan(maybe_uninitialized_conditions.items);
        const maybe_uninitialized_condition_masks_span = try self.result.store.addU64Span(maybe_uninitialized_condition_masks.items);
        const join_id = self.freshJoinPointId();

        try self.loop_stack.append(self.allocator, .{
            .join_id = join_id,
            .params = param_span,
            .param_tys = param_tys,
            .result_target = target,
            .result_ty = result_ty,
            .after_loop = next,
        });
        defer _ = self.loop_stack.pop();

        const body = try self.lowerExprIntoAtType(target, loop.body, result_ty, next);
        const jump_args = try self.lowerExprsToJoinTempsAtTypes(param_span, param_tys, initial_values);
        defer jump_args.deinit(self.allocator);
        var initial_jump = try self.result.store.addCFStmt(.{ .jump = .{
            .target = join_id,
        } });
        initial_jump = try self.prependJoinParamInitializers(param_span, jump_args.ids, initial_jump);
        initial_jump = try self.prependJoinExprsAtTypes(jump_args, param_tys, initial_jump);

        return try self.result.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = param_span,
            .maybe_uninitialized_params = maybe_uninitialized_span,
            .maybe_uninitialized_conditions = maybe_uninitialized_conditions_span,
            .maybe_uninitialized_condition_masks = maybe_uninitialized_condition_masks_span,
            .body = body,
            .remainder = initial_jump,
        } });
    }

    fn lowerBreak(self: *Lowerer, value: ?Lifted.ExprId) Common.LowerError!LIR.CFStmtId {
        const loop = self.currentLoop();
        if (value) |expr_id| {
            return try self.lowerExprIntoAtType(loop.result_target, expr_id, loop.result_ty, loop.after_loop);
        }
        return try self.assignZst(loop.result_target, loop.after_loop);
    }

    fn lowerContinue(self: *Lowerer, values_span: Lifted.Span(Lifted.ExprId)) Common.LowerError!LIR.CFStmtId {
        const loop = self.currentLoop();
        const values = self.solved.lifted.exprSpan(values_span);
        if (self.result.store.getLocalSpan(loop.params).len != values.len) {
            Common.invariant("continue value count differed from loop parameter count during direct LIR lowering");
        }
        const locals = try self.lowerExprsToJoinTempsAtTypes(loop.params, loop.param_tys, values);
        defer locals.deinit(self.allocator);
        var jump = try self.result.store.addCFStmt(.{ .jump = .{
            .target = loop.join_id,
        } });
        jump = try self.prependJoinParamInitializers(loop.params, locals.ids, jump);
        jump = try self.prependJoinExprsAtTypes(locals, loop.param_tys, jump);
        return jump;
    }

    fn lowerJoinPointIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        join_point: Lifted.JoinPointExpr,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        self.noteErasedOwnerAmbiguous(target);
        const saved_return_target = self.current_return_target;
        self.current_return_target = null;
        self.return_forwarding_repeatable_depth += 1;
        defer {
            self.return_forwarding_repeatable_depth -= 1;
            self.current_return_target = saved_return_target;
        }
        const params = self.solved.lifted.typedLocalSpan(join_point.params);
        const param_locals = try self.allocator.alloc(LIR.LocalId, params.len);
        defer self.allocator.free(param_locals);
        const param_tys = try self.allocator.alloc(Type.TypeId, params.len);
        defer self.allocator.free(param_tys);
        for (0..params.len) |index| {
            const param = GuardedList.at(params, index);
            param_tys[index] = try self.lowerLocalTy(param.local);
            param_locals[index] = try self.bindLocalForTyped(param.local, param_tys[index]);
        }

        const param_span = try self.result.store.addLocalSpan(param_locals);
        const lir_join_id = self.freshJoinPointId();
        try self.join_stack.append(self.allocator, .{
            .source_id = join_point.id,
            .join_id = lir_join_id,
            .params = param_span,
            .param_tys = param_tys,
        });
        defer _ = self.join_stack.pop();

        const body = try self.lowerExprIntoAtType(target, join_point.body, result_ty, next);
        const remainder = try self.lowerExprIntoAtType(target, join_point.remainder, result_ty, next);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = lir_join_id,
            .params = param_span,
            .body = body,
            .remainder = remainder,
        } });
    }

    fn lowerJump(self: *Lowerer, jump: Lifted.JumpExpr) Common.LowerError!LIR.CFStmtId {
        const join_point = self.activeJoinPoint(jump.target);
        const args = self.solved.lifted.exprSpan(jump.args);
        if (self.result.store.getLocalSpan(join_point.params).len != args.len) {
            Common.invariant("jump argument count differed from join-point parameter count during direct LIR lowering");
        }

        const locals = try self.lowerExprsToJoinTempsAtTypes(join_point.params, join_point.param_tys, args);
        defer locals.deinit(self.allocator);
        var lir_jump = try self.result.store.addCFStmt(.{ .jump = .{ .target = join_point.join_id } });
        lir_jump = try self.prependJoinParamInitializers(join_point.params, locals.ids, lir_jump);
        lir_jump = try self.prependJoinExprsAtTypes(locals, join_point.param_tys, lir_jump);
        return lir_jump;
    }

    fn lowerReturn(self: *Lowerer, ret: Mono.Return) Common.LowerError!LIR.CFStmtId {
        const ret_ty = self.current_ret_ty orelse Common.invariant("return expression reached LIR lowering outside a function");
        const ret_local = try self.addTemp(ret_ty);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        const saved_return_target = self.current_return_target;
        self.current_return_target = if (self.return_forwarding_repeatable_depth == 0) ret_local else null;
        defer self.current_return_target = saved_return_target;
        return try self.lowerExprIntoAtType(ret_local, ret.value, ret_ty, ret_stmt);
    }

    fn lowerExpectExprInto(self: *Lowerer, target: LIR.LocalId, child: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const after = try self.assignZst(target, next);
        return try self.lowerExpectStmt(child, after);
    }

    fn lowerExpectStmt(self: *Lowerer, child: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const cond = try self.addTemp(try self.lowerExprTy(child));
        const expect_stmt = try self.result.store.addCFStmt(.{ .expect = .{ .condition = cond, .next = next } });
        return try self.lowerExprInto(cond, child, expect_stmt);
    }

    const PatternMiss = struct {
        join_id: LIR.JoinPointId,
    };

    /// Lower a match through the shared decision-tree compiler
    /// (src/postcheck/match_tree.zig): one multiway test per tested
    /// occurrence, one discriminant/length/field read per position, and
    /// branch bodies lowered exactly once.
    fn lowerBranchTree(
        self: *Lowerer,
        scrutinee: LIR.LocalId,
        scrutinee_ty: Type.TypeId,
        branches: []const Lifted.Branch,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        done: LIR.JoinPointId,
        comptime_site: ?Lifted.ComptimeSiteId,
    ) Common.LowerError!LIR.CFStmtId {
        var arena_state = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_state.deinit();
        const arena = arena_state.allocator();

        var shapes = MatchTreeCtx.ShapeInterner{ .map = .init(arena) };
        const ctx = MatchTreeCtx{
            .l = self,
            .arena = arena,
            .target = target,
            .result_ty = result_ty,
            .comptime_site = comptime_site,
            .shapes = &shapes,
        };

        const MT = match_tree.Compiler(MatchTreeCtx);
        const mt_branches = try arena.alloc(MT.Branch, branches.len);
        for (branches, mt_branches, 0..) |branch, *out, i| {
            out.* = .{
                .pat = branch.pat,
                .bindings = branch.bindings,
                .guard = branch.guard,
                .body = branch.body,
                .branch_index = @intCast(i),
            };
        }
        const built = try MT.build(arena, ctx, scrutinee_ty, mt_branches);
        return try MT.Emitter.emitMatch(arena, ctx, built, scrutinee, done);
    }

    /// Accessor/emission context adapting this lowerer to the shared
    /// decision-tree match compiler. Construction methods answer pattern and
    /// type queries; emission methods delegate to the same primitives the
    /// chain used, so the tree and chain produce identical leaf-level LIR.
    const MatchTreeCtx = struct {
        l: *Lowerer,
        arena: std.mem.Allocator,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        comptime_site: ?Lifted.ComptimeSiteId,
        shapes: *ShapeInterner,

        pub const PatId = Lifted.PatId;
        pub const TypeId = Type.TypeId;
        pub const ExprId = Lifted.ExprId;
        pub const LocalId = Lifted.LocalId;
        pub const BindingSpan = Lifted.Span(Lifted.StmtId);
        pub const LirLocal = LIR.LocalId;
        pub const CFStmtId = LIR.CFStmtId;
        pub const JoinPointId = LIR.JoinPointId;
        pub const StrArm = LIR.StrMatchArm;
        pub const LowerError = Common.LowerError;

        /// One sub-pattern of a destructuring pattern, at its committed index.
        pub const SubPat = struct { index: u16, ty: Type.TypeId, pat: Lifted.PatId };

        /// Assigns dense u32 ids to structurally-distinct string shapes
        /// (prefix, delimiters, end) so equal string arms merge and captures
        /// intern consistently. A `str_lit` is the shape with no steps and an
        /// exact end, which is exactly string equality.
        pub const ShapeInterner = struct {
            map: std.StringHashMap(u32),

            fn internShape(interner: *ShapeInterner, arena: std.mem.Allocator, encoded: []const u8) error{OutOfMemory}!u32 {
                const gop = try interner.map.getOrPut(encoded);
                if (!gop.found_existing) {
                    gop.key_ptr.* = try arena.dupe(u8, encoded);
                    gop.value_ptr.* = @intCast(interner.map.count() - 1);
                }
                return gop.value_ptr.*;
            }
        };

        pub fn patKind(self: MatchTreeCtx, pat_id: Lifted.PatId) match_tree.PatKind {
            return switch (self.l.pat(pat_id).data) {
                .bind => .bind,
                .wildcard => .wildcard,
                .as => .as_pattern,
                .record => .record,
                .tuple => .tuple,
                .list => .list,
                .nominal => .nominal,
                .tag => .tag,
                .int_lit => .int_lit,
                .dec_lit => .dec_lit,
                .frac_f32_lit => .frac_f32_lit,
                .frac_f64_lit => .frac_f64_lit,
                .str_lit => .str_lit,
                .str_pattern => .str_pattern,
            };
        }

        pub fn bindLocal(self: MatchTreeCtx, pat_id: Lifted.PatId) Lifted.LocalId {
            return self.l.pat(pat_id).data.bind;
        }

        pub fn asInfo(self: MatchTreeCtx, pat_id: Lifted.PatId) struct { pattern: Lifted.PatId, local: Lifted.LocalId } {
            const info = self.l.pat(pat_id).data.as;
            return .{ .pattern = info.pattern, .local = info.local };
        }

        pub fn recordDestructCount(self: MatchTreeCtx, pat_id: Lifted.PatId) u16 {
            return @intCast(self.l.solved.lifted.recordDestructSpan(self.l.pat(pat_id).data.record).len);
        }

        pub fn recordDestruct(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId, i: u16) Common.LowerError!SubPat {
            const destruct = GuardedList.at(self.l.solved.lifted.recordDestructSpan(self.l.pat(pat_id).data.record), i);
            const index = self.l.recordFieldIndex(ty, destruct.name);
            return .{
                .index = index,
                .ty = GuardedList.at(self.l.recordFields(ty), @as(usize, @intCast(index))).ty,
                .pat = destruct.pattern,
            };
        }

        pub fn tupleItemCount(self: MatchTreeCtx, pat_id: Lifted.PatId) u16 {
            return @intCast(self.l.solved.lifted.patSpan(self.l.pat(pat_id).data.tuple).len);
        }

        pub fn tupleItem(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId, i: u16) Common.LowerError!SubPat {
            const items = self.l.solved.lifted.patSpan(self.l.pat(pat_id).data.tuple);
            const item_tys = self.l.tupleItemTypes(ty);
            if (items.len != item_tys.len) Common.invariant("tuple pattern arity differed from target tuple type");
            return .{ .index = i, .ty = GuardedList.at(item_tys, i), .pat = GuardedList.at(items, i) };
        }

        pub fn nominalInner(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId) Common.LowerError!SubPat {
            const inner = self.l.pat(pat_id).data.nominal;
            return .{ .index = 0, .ty = try self.l.nominalPatternBackingType(ty, inner), .pat = inner };
        }

        pub fn tagVariant(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId) u16 {
            return self.l.tagIndex(ty, self.l.pat(pat_id).data.tag.name);
        }

        pub fn tagPayloadCount(self: MatchTreeCtx, pat_id: Lifted.PatId) u16 {
            return @intCast(self.l.solved.lifted.patSpan(self.l.pat(pat_id).data.tag.payloads).len);
        }

        pub fn tagPayload(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId, i: u16) Common.LowerError!SubPat {
            const tag = self.l.pat(pat_id).data.tag;
            const payloads = self.l.solved.lifted.patSpan(tag.payloads);
            const payload_tys = self.l.tagPayloadTypesByIndex(ty, self.l.tagIndex(ty, tag.name));
            if (payloads.len != payload_tys.len) Common.invariant("tag pattern payload arity differed from target tag type");
            return .{ .index = i, .ty = GuardedList.at(payload_tys, i), .pat = GuardedList.at(payloads, i) };
        }

        pub fn tagVariantCount(self: MatchTreeCtx, ty: Type.TypeId) ?u32 {
            const content = self.l.types.get(ty);
            if (content == .tag_union) return @intCast(self.l.types.tagSpan(content.tag_union).len);
            if (content == .named and content.named.backing != null) return self.tagVariantCount(content.named.backing.?.ty);
            return null;
        }

        pub fn callableVariant(_: MatchTreeCtx, _: Lifted.PatId, _: Type.TypeId) u16 {
            Common.invariant("callable pattern reached solved match lowering");
        }

        pub fn callablePayload(_: MatchTreeCtx, _: Lifted.PatId, _: Type.TypeId) Common.LowerError!?SubPat {
            Common.invariant("callable pattern reached solved match lowering");
        }

        pub fn callableVariantCount(_: MatchTreeCtx, _: Type.TypeId) ?u32 {
            return null;
        }

        /// Neutral view of a list pattern's shape.
        pub const ListPatView = struct {
            fixed_count: u32,
            rest: ?struct { index: u32, pattern: ?Lifted.PatId },
        };

        pub fn listView(self: MatchTreeCtx, pat_id: Lifted.PatId) ListPatView {
            const list = self.l.pat(pat_id).data.list;
            return .{
                .fixed_count = @intCast(list.patterns.len),
                .rest = if (list.rest) |rest| .{ .index = rest.index, .pattern = rest.pattern } else null,
            };
        }

        pub fn listElemPat(self: MatchTreeCtx, pat_id: Lifted.PatId, i: u32) Lifted.PatId {
            return GuardedList.at(self.l.solved.lifted.patSpan(self.l.pat(pat_id).data.list.patterns), i);
        }

        pub fn listElemTy(self: MatchTreeCtx, ty: Type.TypeId) Type.TypeId {
            return self.l.listElemType(ty);
        }

        pub fn ctorKey(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId) Common.LowerError!u128 {
            return switch (self.l.pat(pat_id).data) {
                .tag => |tag| self.l.tagIndex(ty, tag.name),
                .int_lit => |value| @bitCast(value.toI128()),
                .dec_lit => |value| @bitCast(value.num),
                // IEEE `==` behavior: +0.0 and -0.0 are the same
                // constructor; NaN arms never match but stay distinct.
                .frac_f32_lit => |value| @as(u32, @bitCast(if (value == 0.0) @as(f32, 0.0) else value)),
                .frac_f64_lit => |value| @as(u64, @bitCast(if (value == 0.0) @as(f64, 0.0) else value)),
                .str_lit => |lit| try self.strShapeKey(self.l.stringLiteralText(lit), &.{}, .exact),
                .str_pattern => |str| blk: {
                    const steps = try GuardedList.dupe(self.arena, Lifted.StrPatternStep, self.l.solved.lifted.strPatternStepSpan(str.steps));
                    break :blk try self.strShapeKey(self.l.stringLiteralText(str.prefix), steps, str.end);
                },
                .list => |list| list.patterns.len,
                .bind, .wildcard, .as, .record, .tuple, .nominal => Common.invariant("constructor key requested for non-constructor pattern"),
            };
        }

        fn strShapeKey(
            self: MatchTreeCtx,
            prefix: []const u8,
            steps: []const Lifted.StrPatternStep,
            end: Lifted.StrPatternEnd,
        ) Common.LowerError!u128 {
            var encoded: std.ArrayList(u8) = .empty;
            const writer = &encoded;
            try appendLenPrefixed(writer, self.arena, prefix);
            for (steps) |step| {
                try appendLenPrefixed(writer, self.arena, self.l.stringLiteralText(step.delimiter));
            }
            try writer.append(self.arena, switch (end) {
                .exact => 0,
                .tail => 1,
            });
            return try self.shapes.internShape(self.arena, encoded.items);
        }

        fn appendLenPrefixed(list: *std.ArrayList(u8), arena: std.mem.Allocator, bytes: []const u8) error{OutOfMemory}!void {
            var len_bytes: [4]u8 = undefined;
            std.mem.writeInt(u32, &len_bytes, @intCast(bytes.len), .little);
            try list.appendSlice(arena, &len_bytes);
            try list.appendSlice(arena, bytes);
        }

        pub fn intSwitchValue(self: MatchTreeCtx, pat_id: Lifted.PatId, ty: Type.TypeId) ?u64 {
            const pat_data = self.l.pat(pat_id).data;
            if (pat_data != .int_lit) return null;
            const value: i128 = pat_data.int_lit.toI128();
            // Executors disagree on how a narrow signed condition widens to
            // the u64 the switch compares (the interpreter zero-extends the
            // stored bytes; the dev backend loads sign-extended), so narrow
            // signed layouts may only switch on sign-bit-clear values—for
            // those the two widenings coincide. Everything else keeps the
            // equality-chain lowering.
            const width: u16 = switch (self.intPrimitive(ty) orelse return null) {
                .u8 => 8,
                .u16 => 16,
                .u32 => 32,
                .u64, .i64 => 64,
                .i8 => if (value < 0 or value > std.math.maxInt(i8)) return null else 8,
                .i16 => if (value < 0 or value > std.math.maxInt(i16)) return null else 16,
                .i32 => if (value < 0 or value > std.math.maxInt(i32)) return null else 32,
                // 128-bit integers exceed switch_stmt's condition width; Dec,
                // floats, bool, and str never take this path.
                .u128, .i128, .bool, .str, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => return null,
            };
            // Encode as the value's bits zero-extended at layout width—the
            // read every executor performs on an unsigned or sign-bit-clear
            // condition.
            const bits: u128 = @bitCast(value);
            const mask: u128 = if (width == 64) std.math.maxInt(u64) else (@as(u128, 1) << @intCast(width)) - 1;
            return @intCast(bits & mask);
        }

        fn intPrimitive(self: MatchTreeCtx, ty: Type.TypeId) ?MonoType.Primitive {
            const content = self.l.types.get(ty);
            if (content == .primitive) return content.primitive;
            if (content == .named and content.named.backing != null) return self.intPrimitive(content.named.backing.?.ty);
            return null;
        }

        pub fn strLitIsSetArm(_: MatchTreeCtx) bool {
            // A str_match arm with no steps and an exact end is byte equality,
            // which is exactly Str equality (see execStrMatchArm).
            return true;
        }

        pub fn strCaptureCount(self: MatchTreeCtx, pat_id: Lifted.PatId) u16 {
            const data = self.l.pat(pat_id).data;
            return if (data == .str_pattern)
                @intCast(self.l.solved.lifted.strPatternStepSpan(data.str_pattern.steps).len)
            else
                0;
        }

        pub fn strCapturePat(self: MatchTreeCtx, pat_id: Lifted.PatId, i: u16) ?Lifted.PatId {
            const str = self.l.pat(pat_id).data.str_pattern;
            return GuardedList.at(self.l.solved.lifted.strPatternStepSpan(str.steps), i).capture;
        }

        pub fn stmtCount(self: MatchTreeCtx) usize {
            return self.l.result.store.cf_stmts.len();
        }

        pub fn freshJoinPointId(self: MatchTreeCtx) LIR.JoinPointId {
            return self.l.freshJoinPointId();
        }

        pub fn joinJump(self: MatchTreeCtx, id: LIR.JoinPointId) Common.LowerError!LIR.CFStmtId {
            return try self.l.joinJump(id);
        }

        pub fn addExitJoin(self: MatchTreeCtx, id: LIR.JoinPointId, body: LIR.CFStmtId, remainder: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.result.store.addCFStmt(.{ .join = .{
                .id = id,
                .params = LIR.LocalSpan.empty(),
                .body = body,
                .remainder = remainder,
            } });
        }

        pub fn failTerminal(self: MatchTreeCtx) Common.LowerError!LIR.CFStmtId {
            if (self.comptime_site) |site| {
                return try self.l.result.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{ .site = try self.l.lowerComptimeSite(site) } });
            }
            return try self.l.result.store.addCFStmt(.{ .runtime_error = {} });
        }

        pub fn lirLocalForOcc(self: MatchTreeCtx, _: match_tree.Step, ty: Type.TypeId, _: ?LIR.LocalId) Common.LowerError!LIR.LocalId {
            return try self.l.addTemp(ty);
        }

        pub fn lirLocalU16(self: MatchTreeCtx) Common.LowerError!LIR.LocalId {
            return try self.l.addLocalForLayout(.u16);
        }

        pub fn lirLocalU64(self: MatchTreeCtx) Common.LowerError!LIR.LocalId {
            return try self.l.addLocalForLayout(.u64);
        }

        pub fn isZstLirLocal(self: MatchTreeCtx, local: LIR.LocalId) bool {
            return self.l.isZstLocal(local);
        }

        pub fn readDiscriminant(self: MatchTreeCtx, disc: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.result.store.addCFStmt(.{ .assign_ref = .{
                .target = disc,
                .op = .{ .discriminant = .{ .source = source } },
                .next = next,
            } });
        }

        pub fn readField(self: MatchTreeCtx, dest: LIR.LocalId, ty: Type.TypeId, source: LIR.LocalId, index: u16, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.assignTypedRefRead(
                dest,
                ty,
                ty,
                self.l.localFieldLayout(source, index),
                .{ .field = .{ .source = source, .field_idx = index } },
                next,
            );
        }

        pub fn readTagPayload(self: MatchTreeCtx, dest: LIR.LocalId, ty: Type.TypeId, source: LIR.LocalId, variant: u16, index: u16, single: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            if (single) {
                return try self.l.assignTypedRefRead(
                    dest,
                    ty,
                    ty,
                    self.l.localTagPayloadLayout(source, variant, null),
                    .{ .tag_payload_struct = .{
                        .source = source,
                        .variant_index = variant,
                        .tag_discriminant = variant,
                    } },
                    next,
                );
            }
            return try self.l.assignTypedRefRead(
                dest,
                ty,
                ty,
                self.l.localTagPayloadLayout(source, variant, index),
                .{ .tag_payload = .{
                    .source = source,
                    .payload_idx = index,
                    .variant_index = variant,
                    .tag_discriminant = variant,
                } },
                next,
            );
        }

        pub fn readCallablePayload(_: MatchTreeCtx, _: LIR.LocalId, _: Type.TypeId, _: LIR.LocalId, _: u16, _: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            Common.invariant("callable pattern reached solved match lowering");
        }

        pub fn readListLen(self: MatchTreeCtx, dest: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.assignUnaryLowLevel(dest, .list_len, source, next);
        }

        pub fn readListElemFront(self: MatchTreeCtx, dest: LIR.LocalId, source: LIR.LocalId, index: u64, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const index_local = try self.l.addLocalForLayout(.u64);
            const get = try self.l.assignBinaryLowLevel(dest, .list_get_unsafe, source, index_local, next);
            return try self.l.assignU64Literal(index_local, @intCast(index), get);
        }

        pub fn readListElemBack(self: MatchTreeCtx, dest: LIR.LocalId, source: LIR.LocalId, len_local: LIR.LocalId, back: u64, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const index_local = try self.l.addLocalForLayout(.u64);
            const get = try self.l.assignBinaryLowLevel(dest, .list_get_unsafe, source, index_local, next);
            return try self.l.lenMinusConst(index_local, len_local, @intCast(back), get);
        }

        pub fn readListRest(self: MatchTreeCtx, dest: LIR.LocalId, source: LIR.LocalId, len_local: LIR.LocalId, front: u64, back: u64, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            // rest = take_first(take_last(source, len - front), len - (front + back))
            const front_dropped = try self.l.addLocalForLayout(self.l.result.store.getLocal(source).layout_idx);
            const keep_len = try self.l.addLocalForLayout(.u64);
            var current = try self.l.assignBinaryLowLevel(dest, .list_take_first, front_dropped, keep_len, next);
            current = try self.l.lenMinusConst(keep_len, len_local, @intCast(front + back), current);
            const keep_after_front = try self.l.addLocalForLayout(.u64);
            current = try self.l.assignBinaryLowLevel(front_dropped, .list_take_last, source, keep_after_front, current);
            return try self.l.lenMinusConst(keep_after_front, len_local, @intCast(front), current);
        }

        pub fn readNominalBacking(self: MatchTreeCtx, dest: LIR.LocalId, backing_ty: Type.TypeId, source: LIR.LocalId, nominal_ty: Type.TypeId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const source_layout = self.l.result.store.getLocal(source).layout_idx;
            return try self.l.assignNominalPatternBoundaryAtTypes(dest, backing_ty, source, nominal_ty, source_layout, next);
        }

        pub fn switchStmt(self: MatchTreeCtx, cond: LIR.LocalId, values: []const u64, bodies: []const LIR.CFStmtId, default: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const branches = try self.arena.alloc(LIR.CFSwitchBranch, values.len);
            for (branches, values, bodies) |*branch, value, body| {
                branch.* = .{ .value = value, .body = body };
            }
            return try self.l.result.store.addCFStmt(.{ .switch_stmt = .{
                .cond = cond,
                .branches = try self.l.result.store.addCFSwitchBranches(branches),
                .default_branch = default,
                .continuation = null,
            } });
        }

        pub fn lenGteTest(self: MatchTreeCtx, len_local: LIR.LocalId, min: u64, then: LIR.CFStmtId, els: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const required = try self.l.addLocalForLayout(.u64);
            const cond = try self.l.addLocalForLayout(.bool);
            const length_check = try self.l.boolSwitchNoContinuation(cond, then, els);
            const compare = try self.l.assignBinaryLowLevel(cond, .num_is_gte, len_local, required, length_check);
            return try self.l.assignU64Literal(required, @intCast(min), compare);
        }

        pub fn literalEqTest(self: MatchTreeCtx, source: LIR.LocalId, ty: Type.TypeId, pat_id: Lifted.PatId, on_match: LIR.CFStmtId, on_miss: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const literal: LiteralPattern = switch (self.l.pat(pat_id).data) {
                .int_lit => |value| .{ .int_lit = value },
                .dec_lit => |value| .{ .dec_lit = value },
                .frac_f32_lit => |value| .{ .frac_f32_lit = value },
                .frac_f64_lit => |value| .{ .frac_f64_lit = value },
                .str_lit => |value| .{ .str_lit = value },
                .bind, .wildcard, .as, .record, .tuple, .list, .tag, .nominal, .str_pattern => Common.invariant("equality arm requested for non-literal pattern"),
            };
            const lit_local = try self.l.addTemp(ty);
            const eq_local = try self.l.addLocalForLayout(.bool);
            const branch = try self.l.boolSwitchNoContinuation(eq_local, on_match, on_miss);
            const compare = try self.l.lowerEqLocalsInto(eq_local, source, lit_local, ty, false, branch);
            return try self.l.lowerLiteralInto(lit_local, literal, compare);
        }

        pub fn strArmCaptureLocals(self: MatchTreeCtx, pat_id: Lifted.PatId) Common.LowerError![]const ?LIR.LocalId {
            const data = self.l.pat(pat_id).data;
            if (data == .str_lit) return &.{};
            if (data != .str_pattern) Common.invariant("string arm requested for non-string pattern");
            const str = data.str_pattern;
            const steps = self.l.solved.lifted.strPatternStepSpan(str.steps);
            const locals = try self.arena.alloc(?LIR.LocalId, steps.len);
            for (locals, 0..) |*out, i| {
                out.* = if (GuardedList.at(steps, i).capture) |capture| try self.l.addTemp(try self.l.lowerPatTy(capture)) else null;
            }
            return locals;
        }

        pub fn buildStrArm(self: MatchTreeCtx, pat_id: Lifted.PatId, capture_locals: []const ?LIR.LocalId, on_match: LIR.CFStmtId) Common.LowerError!LIR.StrMatchArm {
            switch (self.l.pat(pat_id).data) {
                .str_lit => |lit| return .{
                    .prefix = try self.l.lirStrLiteral(lit),
                    .steps = try self.l.result.store.addStrMatchSteps(&.{}),
                    .end = .exact,
                    .on_match = on_match,
                },
                .str_pattern => |str| {
                    const input_steps = self.l.solved.lifted.strPatternStepSpan(str.steps);
                    const lir_steps = try self.arena.alloc(LIR.StrMatchStep, input_steps.len);
                    for (lir_steps, capture_locals, 0..) |*lir_step, capture, i| {
                        lir_step.* = .{
                            .capture = if (capture) |local| .{ .view = local } else .discard,
                            .delimiter = try self.l.lirStrLiteral(GuardedList.at(input_steps, i).delimiter),
                        };
                    }
                    return .{
                        .prefix = try self.l.lirStrLiteral(str.prefix),
                        .steps = try self.l.result.store.addStrMatchSteps(lir_steps),
                        .end = switch (str.end) {
                            .exact => .exact,
                            .tail => .tail,
                        },
                        .on_match = on_match,
                    };
                },
                .bind, .wildcard, .as, .record, .tuple, .list, .tag, .nominal, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit => Common.invariant("string arm requested for non-string pattern"),
            }
        }

        pub fn strMatchSet(self: MatchTreeCtx, source: LIR.LocalId, arms: []const LIR.StrMatchArm, on_miss: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.result.store.addCFStmt(.{ .str_match_set = .{
                .source = source,
                .arms = try self.l.result.store.addStrMatchArms(arms),
                .on_miss = on_miss,
            } });
        }

        pub fn bindPatternLocal(self: MatchTreeCtx, local: Lifted.LocalId, ty: Type.TypeId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.bindLocalFromTyped(local, ty, source, next);
        }

        pub fn lowerBindings(self: MatchTreeCtx, bindings: BindingSpan, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            const stmts = self.l.solved.lifted.stmtSpan(bindings);
            var current = next;
            var i = stmts.len;
            while (i > 0) {
                i -= 1;
                current = try self.l.lowerStmt(GuardedList.at(stmts, i), current);
            }
            return current;
        }

        pub fn lowerBody(self: MatchTreeCtx, body: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.lowerExprIntoAtType(self.target, body, self.result_ty, next);
        }

        pub fn guardTemp(self: MatchTreeCtx, guard: Lifted.ExprId) Common.LowerError!LIR.LocalId {
            return try self.l.addTemp(try self.l.lowerExprTy(guard));
        }

        pub fn lowerGuard(self: MatchTreeCtx, cond: LIR.LocalId, guard: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.lowerExprInto(cond, guard, next);
        }

        pub fn boolSwitch(self: MatchTreeCtx, cond: LIR.LocalId, then: LIR.CFStmtId, els: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
            return try self.l.boolSwitchNoContinuation(cond, then, els);
        }
    };

    fn joinJump(self: *Lowerer, join_id: LIR.JoinPointId) Common.LowerError!LIR.CFStmtId {
        return try self.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    }

    fn patternMissJump(self: *Lowerer, miss: ?PatternMiss) Common.LowerError!LIR.CFStmtId {
        const miss_info = miss orelse Common.invariant("refutable pattern lowering had no miss target");
        return try self.result.store.addCFStmt(.{ .jump = .{ .target = miss_info.join_id } });
    }

    const LiftedPatternRefutabilityAdapter = struct {
        pub const PatternId = Lifted.PatId;

        lowerer: *Lowerer,

        pub fn patternClass(self: @This(), pat_id: PatternId) PatternRefutability.PatternClass {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .bind,
                .wildcard,
                => .cannot_miss,
                .as,
                .nominal,
                => .child,
                .record => .record,
                .tuple => .sequence,
                .list => .list,
                .tag,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => .can_miss,
            };
        }

        pub fn child(self: @This(), pat_id: PatternId) PatternId {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .as => |as| as.pattern,
                .nominal => |inner| inner,
                .bind,
                .wildcard,
                .record,
                .tuple,
                .list,
                .tag,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn sequenceLen(self: @This(), pat_id: PatternId) usize {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .tuple => |items| self.lowerer.solved.lifted.patSpan(items).len,
                .bind,
                .wildcard,
                .as,
                .record,
                .list,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn sequenceChild(self: @This(), pat_id: PatternId, index: usize) PatternId {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .tuple => |items| GuardedList.at(self.lowerer.solved.lifted.patSpan(items), index),
                .bind,
                .wildcard,
                .as,
                .record,
                .list,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn recordLen(self: @This(), pat_id: PatternId) usize {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .record => |fields| self.lowerer.solved.lifted.recordDestructSpan(fields).len,
                .bind,
                .wildcard,
                .as,
                .tuple,
                .list,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn recordChild(self: @This(), pat_id: PatternId, index: usize) PatternId {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .record => |fields| GuardedList.at(self.lowerer.solved.lifted.recordDestructSpan(fields), index).pattern,
                .bind,
                .wildcard,
                .as,
                .tuple,
                .list,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn listFixedLen(self: @This(), pat_id: PatternId) usize {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .list => |list| list.patterns.len,
                .bind,
                .wildcard,
                .as,
                .record,
                .tuple,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn listHasRest(self: @This(), pat_id: PatternId) bool {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .list => |list| list.rest != null,
                .bind,
                .wildcard,
                .as,
                .record,
                .tuple,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }

        pub fn listRestPattern(self: @This(), pat_id: PatternId) ?PatternId {
            const pat_data = self.lowerer.pat(pat_id);
            return switch (pat_data.data) {
                .list => |list| list.rest.?.pattern,
                .bind,
                .wildcard,
                .as,
                .record,
                .tuple,
                .tag,
                .nominal,
                .int_lit,
                .dec_lit,
                .frac_f32_lit,
                .frac_f64_lit,
                .str_lit,
                .str_pattern,
                => unreachable,
            };
        }
    };

    fn patternCanMiss(self: *Lowerer, pat_id: Lifted.PatId) bool {
        return PatternRefutability.canMiss(LiftedPatternRefutabilityAdapter, .{ .lowerer = self }, pat_id);
    }

    fn lowerPatternThen(self: *Lowerer, pat_id: Lifted.PatId, source: LIR.LocalId, on_match: LIR.CFStmtId, miss: ?PatternMiss, continuation: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return try self.lowerPatternThenAtType(pat_id, try self.lowerPatTy(pat_id), source, on_match, miss, continuation);
    }

    fn lowerPatternThenAtType(self: *Lowerer, pat_id: Lifted.PatId, pat_ty: Type.TypeId, source: LIR.LocalId, on_match: LIR.CFStmtId, miss: ?PatternMiss, continuation: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind, .wildcard => try self.bindPatternAtType(pat_id, pat_ty, source, on_match),
            .as => |as| blk: {
                const tested = try self.lowerPatternThenAtType(as.pattern, pat_ty, source, on_match, miss, continuation);
                break :blk try self.bindLocalFromTyped(as.local, pat_ty, source, tested);
            },
            .record => |fields| try self.lowerRecordPatternThen(pat_ty, fields, source, on_match, miss, continuation),
            .tuple => |items| try self.lowerTuplePatternThen(pat_ty, items, source, on_match, miss, continuation),
            .list => |list| try self.lowerListPatternThen(pat_ty, list, source, on_match, miss, continuation),
            .nominal => |inner| try self.lowerNominalPatternThen(pat_ty, inner, source, on_match, miss, continuation),
            .tag => |tag| try self.lowerTagPatternThen(pat_ty, tag.name, tag.payloads, source, on_match, miss, continuation),
            .int_lit => |value| try self.lowerLiteralPatternThen(source, pat_ty, .{ .int_lit = value }, on_match, miss),
            .dec_lit => |value| try self.lowerLiteralPatternThen(source, pat_ty, .{ .dec_lit = value }, on_match, miss),
            .frac_f32_lit => |value| try self.lowerLiteralPatternThen(source, pat_ty, .{ .frac_f32_lit = value }, on_match, miss),
            .frac_f64_lit => |value| try self.lowerLiteralPatternThen(source, pat_ty, .{ .frac_f64_lit = value }, on_match, miss),
            .str_lit => |value| try self.lowerLiteralPatternThen(source, pat_ty, .{ .str_lit = value }, on_match, miss),
            .str_pattern => |str| try self.lowerStrPatternThen(str, source, on_match, miss),
        };
    }

    const LiteralPattern = union(enum) {
        int_lit: can.CIR.IntValue,
        dec_lit: builtins.dec.RocDec,
        frac_f32_lit: f32,
        frac_f64_lit: f64,
        str_lit: Lifted.StringLiteralId,
    };

    fn lowerLiteralPatternThen(
        self: *Lowerer,
        source: LIR.LocalId,
        ty: Type.TypeId,
        literal: LiteralPattern,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
    ) Common.LowerError!LIR.CFStmtId {
        const lit_local = try self.addTemp(ty);
        const eq_local = try self.addLocalForLayout(.bool);
        const switch_stmt = try self.boolSwitchNoContinuation(eq_local, on_match, try self.patternMissJump(miss));
        const compare = try self.lowerEqLocalsInto(eq_local, source, lit_local, ty, false, switch_stmt);
        return try self.lowerLiteralInto(lit_local, literal, compare);
    }

    fn lowerLiteralInto(self: *Lowerer, target: LIR.LocalId, literal: LiteralPattern, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return switch (literal) {
            .int_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .i128_literal = .{ .value = value.toI128(), .layout_idx = self.result.store.getLocal(target).layout_idx } },
                .next = next,
            } }),
            .dec_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .dec_literal = value.num },
                .next = next,
            } }),
            .frac_f32_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f32_literal = value },
                .next = next,
            } }),
            .frac_f64_lit => |value| try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .f64_literal = value },
                .next = next,
            } }),
            .str_lit => |value| blk: {
                const str_lit = self.stringLiteral(value);
                break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .str_literal = try self.result.store.insertStringView(str_lit.backing, str_lit.offset, str_lit.len) },
                    .next = next,
                } });
            },
        };
    }

    fn lowerTagPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        name: Type.names.TagNameId,
        payloads: Lifted.Span(Lifted.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_ty = self.storageTypeOfLocalOr(source, ty);
        const variant_index = self.tagIndex(source_ty, name);
        const bind_payloads = try self.matchTagPayloadPatterns(source_ty, variant_index, payloads, source, on_match, miss, continuation);
        return try self.discriminantSwitch(source, variant_index, bind_payloads, try self.patternMissJump(miss), false);
    }

    fn lowerRecordPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        span: Lifted.Span(Lifted.RecordDestruct),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const source_ty = self.storageTypeOfLocalOr(source, ty);
        const destructs = self.solved.lifted.recordDestructSpan(span);
        var i = destructs.len;
        while (i > 0) {
            i -= 1;
            const destruct = GuardedList.at(destructs, i);
            const field_index = self.recordFieldIndex(source_ty, destruct.name);
            const source_fields = self.recordFields(source_ty);
            const field_ty = GuardedList.at(source_fields, @intCast(field_index)).ty;
            const field_local = try self.addTemp(field_ty);
            current = try self.lowerPatternThenAtType(destruct.pattern, field_ty, field_local, current, miss, continuation);
            if (!self.isZstLocal(field_local)) {
                current = try self.assignTypedRefRead(
                    field_local,
                    field_ty,
                    field_ty,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
            }
        }
        return current;
    }

    fn lowerTuplePatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        span: Lifted.Span(Lifted.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const items = self.solved.lifted.patSpan(span);
        const item_tys = self.tupleItemTypes(self.storageTypeOfLocalOr(source, ty));
        if (items.len != item_tys.len) Common.invariant("tuple pattern arity differed from target tuple type");
        var i = items.len;
        while (i > 0) {
            i -= 1;
            const item_ty = GuardedList.at(item_tys, i);
            const item = GuardedList.at(items, i);
            const item_local = try self.addTemp(item_ty);
            current = try self.lowerPatternThenAtType(item, item_ty, item_local, current, miss, continuation);
            if (!self.isZstLocal(item_local)) {
                const field_index: u16 = @intCast(i);
                current = try self.assignTypedRefRead(
                    item_local,
                    item_ty,
                    item_ty,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
            }
        }
        return current;
    }

    /// Lower a list pattern as a sequence of tests that all share a single
    /// `miss` target: a length test, then one extracted-and-matched element per
    /// fixed pattern, then the optional captured rest slice. Because every miss
    /// jumps to the one shared join, the lowered control flow is linear in the
    /// pattern's size rather than duplicating the remainder of the match per
    /// element.
    fn lowerListPatternThen(
        self: *Lowerer,
        ty: Type.TypeId,
        list: Lifted.ListPattern,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const elems = self.solved.lifted.patSpan(list.patterns);
        const elem_ty = self.listElemType(ty);
        const fixed_count: i64 = @intCast(elems.len);

        if (elems.len == 0) {
            if (list.rest) |rest| {
                if (rest.pattern) |rest_pattern| {
                    return try self.bindPattern(rest_pattern, source, on_match);
                }
                return on_match;
            }
        }

        // The list length is read by the length test, by the indices of fixed
        // elements that match from the back, and by the rest slice. It is
        // assigned once at the head of the chain so every reader sees it.
        const len_local = try self.addLocalForLayout(.u64);

        var current = on_match;

        // The captured rest binds the slice between the matched front and back
        // elements. `[.. as rest]` with no fixed elements is the whole list and
        // simply aliases the source; a bare `..` binds nothing.
        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                if (elems.len == 0) {
                    current = try self.bindPatternAtType(rest_pattern, ty, source, current);
                } else {
                    const source_layout = self.result.store.getLocal(source).layout_idx;
                    const rest_local = try self.addLocalForLayout(source_layout);
                    current = try self.bindPatternAtType(rest_pattern, ty, rest_local, current);
                    // rest = take_first(take_last(source, len - rest.index), len - fixed_count)
                    const front_dropped = try self.addLocalForLayout(source_layout);
                    const keep_len = try self.addLocalForLayout(.u64);
                    current = try self.assignBinaryLowLevel(rest_local, .list_take_first, front_dropped, keep_len, current);
                    current = try self.lenMinusConst(keep_len, len_local, fixed_count, current);
                    const keep_after_front = try self.addLocalForLayout(.u64);
                    current = try self.assignBinaryLowLevel(front_dropped, .list_take_last, source, keep_after_front, current);
                    current = try self.lenMinusConst(keep_after_front, len_local, @intCast(rest.index), current);
                }
            }
        }

        const rest_index: ?u32 = if (list.rest) |rest| rest.index else null;
        var i = elems.len;
        while (i > 0) {
            i -= 1;
            const elem_local = try self.addTemp(elem_ty);
            current = try self.lowerPatternThenAtType(GuardedList.at(elems, i), elem_ty, elem_local, current, miss, continuation);
            const index_local = try self.addLocalForLayout(.u64);
            current = try self.assignBinaryLowLevel(elem_local, .list_get_unsafe, source, index_local, current);
            const matches_from_back = if (rest_index) |ri| i >= ri else false;
            if (matches_from_back) {
                // Elements after the rest are indexed relative to the end:
                // len - (fixed_count - i).
                current = try self.lenMinusConst(index_local, len_local, fixed_count - @as(i64, @intCast(i)), current);
            } else {
                current = try self.assignU64Literal(index_local, @intCast(i), current);
            }
        }

        // Length test at the head: an exact match needs `len == fixed_count`; a
        // pattern with a rest needs `len >= fixed_count`.
        const required = try self.addLocalForLayout(.u64);
        const cond = try self.addLocalForLayout(.bool);
        const cmp_op: LIR.LowLevel = if (list.rest == null) .num_is_eq else .num_is_gte;
        const length_check = try self.boolSwitchNoContinuation(cond, current, try self.patternMissJump(miss));
        var head = try self.assignBinaryLowLevel(cond, cmp_op, len_local, required, length_check);
        head = try self.assignU64Literal(required, fixed_count, head);
        head = try self.assignUnaryLowLevel(len_local, .list_len, source, head);
        return head;
    }

    fn assignU64Literal(self: *Lowerer, target: LIR.LocalId, value: i64, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return try self.result.store.addCFStmt(.{ .assign_literal = .{
            .target = target,
            .value = .{ .i64_literal = .{ .value = value, .layout_idx = .u64 } },
            .next = next,
        } });
    }

    fn assignBinaryLowLevel(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const args = [_]LIR.LocalId{ lhs, rhs };
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&args),
            .next = next,
        } });
    }

    /// Emit `target = len_local - value` using a fresh u64 literal operand.
    fn lenMinusConst(self: *Lowerer, target: LIR.LocalId, len_local: LIR.LocalId, value: i64, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const operand = try self.addLocalForLayout(.u64);
        const subtract = try self.assignBinaryLowLevel(target, .num_minus, len_local, operand, next);
        return try self.assignU64Literal(operand, value, subtract);
    }

    fn bindPattern(self: *Lowerer, pat_id: Lifted.PatId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return try self.bindPatternAtType(pat_id, try self.lowerPatTy(pat_id), source, next);
    }

    fn bindPatternAtType(self: *Lowerer, pat_id: Lifted.PatId, pat_ty: Type.TypeId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind => |local| try self.bindLocalFromTyped(local, pat_ty, source, next),
            .wildcard => next,
            .as => |as| blk: {
                const bound = try self.bindPatternAtType(as.pattern, pat_ty, source, next);
                break :blk try self.bindLocalFromTyped(as.local, pat_ty, source, bound);
            },
            .record => |fields| try self.bindRecordPattern(pat_ty, fields, source, next),
            .tuple => |items| try self.bindTuplePattern(pat_ty, items, source, next),
            // Only an irrefutable list pattern reaches binding: `[.. as rest]`
            // binds the whole list (aliasing the source); `[..]` binds nothing.
            .list => |list| if (list.rest) |rest| (if (rest.pattern) |rest_pattern|
                try self.bindPatternAtType(rest_pattern, pat_ty, source, next)
            else
                next) else next,
            .tag => |tag| blk: {
                const source_ty = self.storageTypeOfLocalOr(source, pat_ty);
                break :blk try self.bindTagPayloadPatterns(source_ty, self.tagIndex(source_ty, tag.name), tag.payloads, source, next);
            },
            .nominal => |inner| try self.bindNominalPattern(pat_ty, inner, source, next),
            .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => next,
        };
    }

    fn nominalPatternBackingType(
        self: *Lowerer,
        nominal_ty: Type.TypeId,
        inner: Lifted.PatId,
    ) Common.LowerError!Type.TypeId {
        const content = self.types.get(nominal_ty);
        if (content == .named) return (content.named.backing orelse Common.invariant("nominal pattern target had no runtime backing")).ty;
        if (content == .box) return content.box;
        return try self.lowerPatTy(inner);
    }

    fn bindNominalPattern(
        self: *Lowerer,
        nominal_ty: Type.TypeId,
        inner: Lifted.PatId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const backing_ty = try self.nominalPatternBackingType(nominal_ty, inner);
        const backing_local = try self.addLocalForLayout(try self.layoutOfType(backing_ty));
        const bound = try self.bindPatternAtType(inner, backing_ty, backing_local, next);
        return try self.assignNominalPatternBoundaryAtTypes(backing_local, backing_ty, source, nominal_ty, source_layout, bound);
    }

    fn lowerNominalPatternThen(
        self: *Lowerer,
        nominal_ty: Type.TypeId,
        inner: Lifted.PatId,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const backing_ty = try self.nominalPatternBackingType(nominal_ty, inner);
        const backing_local = try self.addLocalForLayout(try self.layoutOfType(backing_ty));
        const matched = try self.lowerPatternThenAtType(inner, backing_ty, backing_local, on_match, miss, continuation);
        return try self.assignNominalPatternBoundaryAtTypes(backing_local, backing_ty, source, nominal_ty, source_layout, matched);
    }

    fn assignNominalPatternBoundaryAtTypes(
        self: *Lowerer,
        backing_target: LIR.LocalId,
        backing_ty: Type.TypeId,
        source: LIR.LocalId,
        nominal_ty: Type.TypeId,
        source_layout: layout.Idx,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const backing_layout = self.result.store.getLocal(backing_target).layout_idx;
        if (backing_layout == source_layout) return try self.assignLocal(backing_target, source, next);

        const backing_content = self.result.layouts.getLayout(backing_layout);
        const source_content = self.result.layouts.getLayout(source_layout);
        const backing_is_box = backing_content.tag == .box or backing_content.tag == .box_of_zst;
        const source_is_box = source_content.tag == .box or source_content.tag == .box_of_zst;
        if (!backing_is_box and !source_is_box) {
            const backing_runtime_ty = self.runtimeBackingType(backing_ty);
            const nominal_runtime_ty = self.runtimeBackingType(nominal_ty);
            const backing_runtime = self.types.get(backing_runtime_ty);
            const nominal_runtime = self.types.get(nominal_runtime_ty);
            if (backing_runtime == .record and nominal_runtime == .record) {
                return try self.assignRecordBoundary(backing_target, backing_runtime.record, source, nominal_runtime.record, next);
            }
            if (backing_runtime == .tuple and nominal_runtime == .tuple) {
                return try self.assignTupleBoundary(backing_target, backing_runtime.tuple, source, nominal_runtime.tuple, next);
            }
            if (backing_runtime == .tag_union and nominal_runtime == .tag_union) {
                return try self.assignTagUnionBoundary(backing_target, backing_runtime.tag_union, source, nominal_runtime.tag_union, next);
            }
        }

        return try self.assignNominalBoundary(backing_target, source, source_layout, next);
    }
    fn lowerStrPatternThen(
        self: *Lowerer,
        str: Lifted.StrPattern,
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
    ) Common.LowerError!LIR.CFStmtId {
        const arm = try self.lowerStrPatternArm(str, on_match);
        return try self.result.store.addCFStmt(.{ .str_match = .{
            .source = source,
            .prefix = arm.prefix,
            .steps = arm.steps,
            .end = arm.end,
            .on_match = arm.on_match,
            .on_miss = try self.patternMissJump(miss),
        } });
    }

    fn lowerStrPatternArm(
        self: *Lowerer,
        str: Lifted.StrPattern,
        on_match: LIR.CFStmtId,
    ) Common.LowerError!LIR.StrMatchArm {
        const input_steps = self.solved.lifted.strPatternStepSpan(str.steps);
        const lir_steps = try self.allocator.alloc(LIR.StrMatchStep, input_steps.len);
        defer self.allocator.free(lir_steps);

        for (0..input_steps.len) |index| {
            const input_step = GuardedList.at(input_steps, index);
            const lir_step = &lir_steps[index];
            lir_step.* = .{
                .capture = if (input_step.capture) |capture|
                    .{ .view = try self.addTemp(try self.lowerPatTy(capture)) }
                else
                    .discard,
                .delimiter = try self.lirStrLiteral(input_step.delimiter),
            };
        }

        var match_body = on_match;
        var index = input_steps.len;
        while (index > 0) {
            index -= 1;
            if (GuardedList.at(input_steps, index).capture) |capture| {
                const capture_local = switch (lir_steps[index].capture) {
                    .view => |local| local,
                    .discard => Common.invariant("string-pattern capture step lowered without a capture local"),
                };
                match_body = try self.bindPattern(capture, capture_local, match_body);
            }
        }

        return .{
            .prefix = try self.lirStrLiteral(str.prefix),
            .steps = try self.result.store.addStrMatchSteps(lir_steps),
            .end = switch (str.end) {
                .exact => .exact,
                .tail => .tail,
            },
            .on_match = match_body,
        };
    }

    fn bindPatternOrCrash(
        self: *Lowerer,
        pat_id: Lifted.PatId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
        comptime_site: ?Lifted.ComptimeSiteId,
    ) Common.LowerError!LIR.CFStmtId {
        if (!self.patternCanMiss(pat_id)) return try self.bindPattern(pat_id, source, next);

        const miss = PatternMiss{ .join_id = self.freshJoinPointId() };
        const crash = if (comptime_site) |site|
            try self.result.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{ .site = try self.lowerComptimeSite(site) } })
        else
            try self.result.store.addCFStmt(.{ .runtime_error = {} });
        const on_match = if (comptime_site) |site|
            try self.result.store.addCFStmt(.{ .comptime_branch_taken = .{
                .site = try self.lowerComptimeSite(site),
                .branch_index = 0,
                .next = next,
            } })
        else
            next;
        const matched = try self.lowerPatternThen(pat_id, source, on_match, miss, on_match);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = miss.join_id,
            .params = LIR.LocalSpan.empty(),
            .body = crash,
            .remainder = matched,
        } });
    }

    fn bindRecordPattern(self: *Lowerer, ty: Type.TypeId, span: Lifted.Span(Lifted.RecordDestruct), source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = next;
        const source_ty = self.storageTypeOfLocalOr(source, ty);
        const destructs = self.solved.lifted.recordDestructSpan(span);
        var i = destructs.len;
        while (i > 0) {
            i -= 1;
            const destruct = GuardedList.at(destructs, i);
            const field_index = self.recordFieldIndex(source_ty, destruct.name);
            const source_fields = self.recordFields(source_ty);
            const field_ty = GuardedList.at(source_fields, @intCast(field_index)).ty;
            const field_local = try self.addTemp(field_ty);
            current = try self.bindPatternAtType(destruct.pattern, field_ty, field_local, current);
            if (!self.isZstLocal(field_local)) {
                current = try self.assignTypedRefRead(
                    field_local,
                    field_ty,
                    field_ty,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
            }
        }
        return current;
    }

    fn matchTagPayloadPatterns(
        self: *Lowerer,
        ty: Type.TypeId,
        variant_index: u16,
        payload_span: Lifted.Span(Lifted.PatId),
        source: LIR.LocalId,
        on_match: LIR.CFStmtId,
        miss: ?PatternMiss,
        continuation: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_match;
        const source_ty = self.storageTypeOfLocalOr(source, ty);
        const payloads = self.solved.lifted.patSpan(payload_span);
        const payload_tys = self.tagPayloadTypesByIndex(source_ty, variant_index);
        if (payloads.len != payload_tys.len) Common.invariant("tag pattern payload arity differed from target tag type");
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload = GuardedList.at(payloads, i);
            const payload_ty = GuardedList.at(payload_tys, i);
            const payload_local = try self.addTemp(payload_ty);
            current = try self.lowerPatternThenAtType(payload, payload_ty, payload_local, current, miss, continuation);
            if (!self.isZstLocal(payload_local)) {
                if (payloads.len == 1) {
                    current = try self.assignTypedRefRead(
                        payload_local,
                        payload_ty,
                        payload_ty,
                        self.localTagPayloadLayout(source, variant_index, null),
                        .{ .tag_payload_struct = .{
                            .source = source,
                            .variant_index = variant_index,
                            .tag_discriminant = variant_index,
                        } },
                        current,
                    );
                    continue;
                }
                const payload_index: u16 = @intCast(i);
                current = try self.assignTypedRefRead(
                    payload_local,
                    payload_ty,
                    payload_ty,
                    self.localTagPayloadLayout(source, variant_index, payload_index),
                    .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = payload_index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
            }
        }
        return current;
    }

    fn bindTuplePattern(self: *Lowerer, ty: Type.TypeId, span: Lifted.Span(Lifted.PatId), source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = next;
        const items = self.solved.lifted.patSpan(span);
        const item_tys = self.tupleItemTypes(self.storageTypeOfLocalOr(source, ty));
        if (items.len != item_tys.len) Common.invariant("tuple pattern arity differed from target tuple type");
        var i = items.len;
        while (i > 0) {
            i -= 1;
            const item = GuardedList.at(items, i);
            const item_ty = GuardedList.at(item_tys, i);
            const item_local = try self.addTemp(item_ty);
            current = try self.bindPatternAtType(item, item_ty, item_local, current);
            if (!self.isZstLocal(item_local)) {
                const field_index: u16 = @intCast(i);
                current = try self.assignTypedRefRead(
                    item_local,
                    item_ty,
                    item_ty,
                    self.localFieldLayout(source, field_index),
                    .{ .field = .{ .source = source, .field_idx = field_index } },
                    current,
                );
            }
        }
        return current;
    }

    fn bindTagPayloadPatterns(
        self: *Lowerer,
        ty: Type.TypeId,
        variant_index: u16,
        payload_span: Lifted.Span(Lifted.PatId),
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = next;
        const source_ty = self.storageTypeOfLocalOr(source, ty);
        const payloads = self.solved.lifted.patSpan(payload_span);
        const payload_tys = self.tagPayloadTypesByIndex(source_ty, variant_index);
        if (payloads.len != payload_tys.len) Common.invariant("tag pattern payload arity differed from target tag type");
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload = GuardedList.at(payloads, i);
            const payload_ty = GuardedList.at(payload_tys, i);
            const payload_local = try self.addTemp(payload_ty);
            current = try self.bindPatternAtType(payload, payload_ty, payload_local, current);
            if (!self.isZstLocal(payload_local)) {
                if (payloads.len == 1) {
                    current = try self.assignTypedRefRead(
                        payload_local,
                        payload_ty,
                        payload_ty,
                        self.localTagPayloadLayout(source, variant_index, null),
                        .{ .tag_payload_struct = .{
                            .source = source,
                            .variant_index = variant_index,
                            .tag_discriminant = variant_index,
                        } },
                        current,
                    );
                    continue;
                }
                const payload_index: u16 = @intCast(i);
                current = try self.assignTypedRefRead(
                    payload_local,
                    payload_ty,
                    payload_ty,
                    self.localTagPayloadLayout(source, variant_index, payload_index),
                    .{ .tag_payload = .{
                        .source = source,
                        .payload_idx = payload_index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
            }
        }
        return current;
    }

    fn lowerEqLocalsInto(self: *Lowerer, target: LIR.LocalId, lhs: LIR.LocalId, rhs: LIR.LocalId, ty: Type.TypeId, negated: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        return switch (self.types.get(ty)) {
            .primitive => |primitive| try self.lowerPrimitiveEqLocalsInto(target, lhs, rhs, primitive, negated, next),
            .zst => try self.assignBool(target, !negated, next),
            .named => |named| blk: {
                const backing = named.backing orelse Common.invariant("named equality reached direct LIR without runtime backing");
                break :blk try self.lowerEqLocalsInto(target, lhs, rhs, backing.ty, negated, next);
            },
            .record => |fields| try self.lowerRecordEqLocalsInto(target, lhs, rhs, self.types.fieldSpan(fields), negated, next),
            .capture_record => |fields| try self.lowerCaptureRecordEqLocalsInto(target, lhs, rhs, self.types.captureFieldSpan(fields), negated, next),
            .tuple => |items| try self.lowerTupleEqLocalsInto(target, lhs, rhs, self.types.span(items), negated, next),
            .tag_union => |tags| try self.lowerTagUnionEqLocalsInto(target, lhs, rhs, self.types.tagSpan(tags), negated, next),
            .list, .box, .callable, .erased_fn, .erased_capture_ptr => Common.invariant("non-structural equality reached direct LIR structural equality lowering"),
        };
    }

    fn lowerPrimitiveEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        primitive: MonoType.Primitive,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (primitive == .bool) {
            return try self.lowerBoolEqLocalsInto(target, lhs, rhs, negated, next);
        }

        // SIMD leaves participate in structural equality, but scalar numeric
        // equality operations do not accept vector operands. Compare the full
        // 128-bit representation explicitly so every LIR consumer observes all
        // lanes through the existing U128 equality operation.
        if (primitive == .u8x16 or primitive == .i8x16 or primitive == .u16x8 or primitive == .i16x8 or
            primitive == .u32x4 or primitive == .i32x4 or primitive == .u64x2 or primitive == .i64x2)
        {
            const lhs_bits = try self.addLocalForLayout(.u128);
            const rhs_bits = try self.addLocalForLayout(.u128);
            const compare = try self.lowerPrimitiveEqLocalsInto(target, lhs_bits, rhs_bits, .u128, negated, next);
            const lower_rhs = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = rhs_bits,
                .op = .simd_to_u128_bits,
                .rc_effect = LIR.LowLevel.simd_to_u128_bits.rcEffect(),
                .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{rhs}),
                .next = compare,
            } });
            return try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = lhs_bits,
                .op = .simd_to_u128_bits,
                .rc_effect = LIR.LowLevel.simd_to_u128_bits.rcEffect(),
                .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{lhs}),
                .next = lower_rhs,
            } });
        }
        const eq_op: LIR.LowLevel = switch (primitive) {
            .str => .str_is_eq,
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => .num_is_eq,
            .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => unreachable,
            .bool => unreachable,
        };
        const args = [_]LIR.LocalId{ lhs, rhs };
        const not_op: LIR.LowLevel = .bool_not;
        if (!negated) {
            return try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = eq_op,
                .rc_effect = eq_op.rcEffect(),
                .args = try self.result.store.addLocalSpan(&args),
                .next = next,
            } });
        }
        const raw = try self.addLocalForLayout(.bool);
        const not_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = not_op,
            .rc_effect = not_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{raw}),
            .next = next,
        } });
        return try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = raw,
            .op = eq_op,
            .rc_effect = eq_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&args),
            .next = not_stmt,
        } });
    }

    fn lowerBoolEqLocalsInto(self: *Lowerer, target: LIR.LocalId, lhs: LIR.LocalId, rhs: LIR.LocalId, negated: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const lhs_disc = try self.addLocalForLayout(.u16);
        const rhs_disc = try self.addLocalForLayout(.u16);
        const compare = try self.lowerPrimitiveEqLocalsInto(target, lhs_disc, rhs_disc, .u16, negated, next);
        const read_rhs = try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = rhs_disc,
            .op = .{ .discriminant = .{ .source = rhs } },
            .next = compare,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = lhs_disc,
            .op = .{ .discriminant = .{ .source = lhs } },
            .next = read_rhs,
        } });
    }

    fn lowerRecordEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        fields: anytype,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = fields.len;
        while (i > 0) {
            i -= 1;
            const field = GuardedList.at(fields, i);
            current = try self.lowerFieldEqStep(lhs, rhs, field.ty, @intCast(i), current, failed);
        }
        return current;
    }

    fn lowerCaptureRecordEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        fields: anytype,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = fields.len;
        while (i > 0) {
            i -= 1;
            const field = GuardedList.at(fields, i);
            current = try self.lowerFieldEqStep(lhs, rhs, field.ty, @intCast(i), current, failed);
        }
        return current;
    }

    fn lowerTupleEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        items: anytype,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, GuardedList.at(items, i), @intCast(i), current, failed);
        }
        return current;
    }

    fn lowerFieldEqStep(
        self: *Lowerer,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        field_ty: Type.TypeId,
        field_index: u16,
        on_equal: LIR.CFStmtId,
        on_not_equal: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const lhs_field = try self.addTemp(field_ty);
        const rhs_field = try self.addTemp(field_ty);
        const eq = try self.addLocalForLayout(.bool);
        var current = try self.boolSwitchNoContinuation(eq, on_equal, on_not_equal);
        current = try self.lowerEqLocalsInto(eq, lhs_field, rhs_field, field_ty, false, current);
        if (!self.isZstLocal(rhs_field)) {
            current = try self.assignRefRead(
                rhs_field,
                self.localFieldLayout(rhs, field_index),
                .{ .field = .{ .source = rhs, .field_idx = field_index } },
                current,
            );
        }
        if (!self.isZstLocal(lhs_field)) {
            current = try self.assignRefRead(
                lhs_field,
                self.localFieldLayout(lhs, field_index),
                .{ .field = .{ .source = lhs, .field_idx = field_index } },
                current,
            );
        }
        return current;
    }

    fn lowerTagUnionEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        tags: anytype,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(lhs) and self.isZstLocal(rhs)) {
            return try self.assignBool(target, !negated, next);
        }

        const success = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);

        const lhs_disc = try self.addLocalForLayout(.u16);
        const rhs_disc = try self.addLocalForLayout(.u16);
        const same_disc = try self.addLocalForLayout(.bool);

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, tags.len);
        defer self.allocator.free(branches);
        for (0..tags.len) |index| {
            const tag = GuardedList.at(tags, index);
            branches[index] = .{
                .value = @intCast(index),
                .body = try self.lowerTagPayloadEqVariant(lhs, rhs, tag, @intCast(index), success, failed),
            };
        }

        const payload_switch = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = lhs_disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = failed,
            .continuation = null,
        } });
        const disc_switch = try self.boolSwitchNoContinuation(same_disc, payload_switch, failed);
        const eq_op: LIR.LowLevel = .num_is_eq;
        const compare_disc = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = same_disc,
            .op = eq_op,
            .rc_effect = eq_op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&[_]LIR.LocalId{ lhs_disc, rhs_disc }),
            .next = disc_switch,
        } });
        const read_rhs = try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = rhs_disc,
            .op = .{ .discriminant = .{ .source = rhs } },
            .next = compare_disc,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = lhs_disc,
            .op = .{ .discriminant = .{ .source = lhs } },
            .next = read_rhs,
        } });
    }

    fn lowerTagPayloadEqVariant(
        self: *Lowerer,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        tag: Type.Tag,
        variant_index: u16,
        on_equal: LIR.CFStmtId,
        on_not_equal: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = on_equal;
        const payloads = self.types.span(tag.payloads);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_ty = GuardedList.at(payloads, i);
            const lhs_payload = try self.addTemp(payload_ty);
            const rhs_payload = try self.addTemp(payload_ty);
            const eq = try self.addLocalForLayout(.bool);
            current = try self.boolSwitchNoContinuation(eq, current, on_not_equal);
            current = try self.lowerEqLocalsInto(eq, lhs_payload, rhs_payload, payload_ty, false, current);
            const payload_idx: ?u16 = if (payloads.len == 1) null else @as(u16, @intCast(i));
            if (!self.isZstLocal(rhs_payload)) {
                current = try self.assignRefRead(
                    rhs_payload,
                    self.localTagPayloadLayout(rhs, variant_index, payload_idx),
                    if (payload_idx) |index| .{ .tag_payload = .{
                        .source = rhs,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = rhs,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
            }
            if (!self.isZstLocal(lhs_payload)) {
                current = try self.assignRefRead(
                    lhs_payload,
                    self.localTagPayloadLayout(lhs, variant_index, payload_idx),
                    if (payload_idx) |index| .{ .tag_payload = .{
                        .source = lhs,
                        .payload_idx = index,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } } else .{ .tag_payload_struct = .{
                        .source = lhs,
                        .variant_index = variant_index,
                        .tag_discriminant = variant_index,
                    } },
                    current,
                );
            }
        }
        return current;
    }

    fn assignBool(self: *Lowerer, target: LIR.LocalId, value: bool, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.result.store.getLocal(target).layout_idx != .bool) {
            Common.invariant("boolean assignment target was not Bool layout");
        }
        const discriminant: u16 = if (value) 1 else 0;
        return try self.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = discriminant,
            .discriminant = discriminant,
            .payload = null,
            .next = next,
        } });
    }

    fn boolSwitchNoContinuation(self: *Lowerer, cond: LIR.LocalId, true_body: LIR.CFStmtId, false_body: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const branches = [_]LIR.CFSwitchBranch{.{ .value = 1, .body = true_body }};
        return try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try self.result.store.addCFSwitchBranches(&branches),
            .default_branch = false_body,
            .continuation = null,
        } });
    }

    fn discriminantSwitch(
        self: *Lowerer,
        source: LIR.LocalId,
        discriminant: u16,
        body: LIR.CFStmtId,
        default: LIR.CFStmtId,
        default_is_cold: bool,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(source)) return body;
        const disc_local = try self.addLocalForLayout(.u16);
        const branches = [_]LIR.CFSwitchBranch{.{ .value = discriminant, .body = body }};
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc_local,
            .branches = try self.result.store.addCFSwitchBranches(&branches),
            .default_branch = default,
            .default_is_cold = default_is_cold,
            .continuation = null,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = disc_local,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    const LoweredExprLocals = struct {
        exprs: []Lifted.ExprId,
        ids: []LIR.LocalId,

        fn deinit(self: LoweredExprLocals, allocator: std.mem.Allocator) void {
            allocator.free(self.exprs);
            allocator.free(self.ids);
        }
    };

    const LoweredJoinExprLocals = struct {
        exprs: []Lifted.ExprId,
        ids: []?LIR.LocalId,

        fn deinit(self: LoweredJoinExprLocals, allocator: std.mem.Allocator) void {
            allocator.free(self.exprs);
            allocator.free(self.ids);
        }
    };

    fn lowerExprsToTemps(self: *Lowerer, exprs: anytype) Common.LowerError!LoweredExprLocals {
        const owned_exprs = try GuardedList.dupe(self.allocator, Lifted.ExprId, exprs);
        errdefer self.allocator.free(owned_exprs);
        const ids = try self.allocator.alloc(LIR.LocalId, owned_exprs.len);
        errdefer self.allocator.free(ids);
        for (owned_exprs, 0..) |expr_id, i| ids[i] = try self.addTemp(try self.lowerExprTy(expr_id));
        return .{ .exprs = owned_exprs, .ids = ids };
    }

    fn lowerExprsToTempsAtTypes(
        self: *Lowerer,
        exprs: anytype,
        tys: []const Type.TypeId,
    ) Common.LowerError!LoweredExprLocals {
        if (exprs.len != tys.len) Common.invariant("typed expression temp arity differed from expression arity");
        const owned_exprs = try GuardedList.dupe(self.allocator, Lifted.ExprId, exprs);
        errdefer self.allocator.free(owned_exprs);
        const ids = try self.allocator.alloc(LIR.LocalId, owned_exprs.len);
        errdefer self.allocator.free(ids);
        for (tys, 0..) |ty, i| ids[i] = try self.addTemp(ty);
        return .{ .exprs = owned_exprs, .ids = ids };
    }

    fn lowerExprsToJoinTempsAtTypes(
        self: *Lowerer,
        params: LIR.LocalSpan,
        param_tys: []const Type.TypeId,
        exprs: anytype,
    ) Common.LowerError!LoweredJoinExprLocals {
        const param_locals = self.result.store.getLocalSpan(params);
        if (param_locals.len != exprs.len or param_tys.len != exprs.len) {
            Common.invariant("typed LIR join arg arity differed from parameter arity");
        }

        const owned_exprs = try GuardedList.dupe(self.allocator, Lifted.ExprId, exprs);
        errdefer self.allocator.free(owned_exprs);
        const ids = try self.allocator.alloc(?LIR.LocalId, owned_exprs.len);
        errdefer self.allocator.free(ids);
        for (owned_exprs, param_tys, 0..) |expr_id, param_ty, i| {
            ids[i] = if (try self.joinArgNeedsWriteAtType(GuardedList.at(param_locals, i), param_ty, expr_id))
                try self.addTemp(param_ty)
            else
                null;
        }
        return .{ .exprs = owned_exprs, .ids = ids };
    }

    fn joinArgNeedsWriteAtType(self: *Lowerer, param: LIR.LocalId, ty: Type.TypeId, expr_id: Lifted.ExprId) Common.LowerError!bool {
        const data = self.solved.lifted.getExpr(expr_id).data;
        if (data == .@"unreachable") Common.invariant("unreachable marker escaped its terminated block-final position into a direct join argument");
        if (data == .uninitialized or data == .uninitialized_payload) return false;
        if (data == .local) return (self.existingLocalForTyped(data.local, ty) orelse return true) != param;
        return true;
    }

    fn prependExprs(self: *Lowerer, lowered: LoweredExprLocals, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        var current = next;
        var i = lowered.ids.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerExprInto(lowered.ids[i], lowered.exprs[i], current);
        }
        return current;
    }

    fn prependExprsAtTypes(
        self: *Lowerer,
        lowered: LoweredExprLocals,
        tys: []const Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (lowered.ids.len != tys.len) Common.invariant("typed expression prepend arity differed from local arity");
        var current = next;
        var i = lowered.ids.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerExprIntoAtType(lowered.ids[i], lowered.exprs[i], tys[i], current);
        }
        return current;
    }

    fn prependJoinExprsAtTypes(
        self: *Lowerer,
        lowered: LoweredJoinExprLocals,
        tys: []const Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (lowered.ids.len != tys.len) Common.invariant("typed join expression prepend arity differed from local arity");
        var current = next;
        var i = lowered.ids.len;
        while (i > 0) {
            i -= 1;
            const target = lowered.ids[i] orelse continue;
            current = try self.lowerExprIntoAtType(target, lowered.exprs[i], tys[i], current);
        }
        return current;
    }

    fn prependJoinParamInitializers(
        self: *Lowerer,
        params: LIR.LocalSpan,
        args: []const ?LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const param_locals = self.result.store.getLocalSpan(params);
        if (param_locals.len != args.len) {
            Common.invariant("LIR join parameter initializer arity differed from jump argument arity");
        }

        var current = next;
        var i = args.len;
        while (i > 0) {
            i -= 1;
            const arg = args[i] orelse continue;
            current = try self.result.store.addCFStmt(.{ .set_local = .{
                .target = GuardedList.at(param_locals, i),
                .value = arg,
                .mode = .initialize_join_param,
                .next = current,
            } });
        }
        return current;
    }

    fn addTemp(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LIR.LocalId {
        const local = try self.addLocalForLayout(try self.layoutOfType(ty));
        try self.local_types.put(local, ty);
        return local;
    }

    fn addLocalForLayout(self: *Lowerer, layout_idx: layout.Idx) Common.LowerError!LIR.LocalId {
        const local = try self.result.store.addLocal(.{ .layout_idx = layout_idx });
        if (@intFromEnum(local) != self.erased_owner_states.items.len) {
            Common.invariant("erased-owner provenance table diverged from the LIR local table");
        }
        try self.erased_owner_states.append(self.allocator, .pending);
        try self.noteLocal(local);
        return local;
    }

    fn typeOfLocalOr(self: *Lowerer, local: LIR.LocalId, fallback: Type.TypeId) Type.TypeId {
        return self.local_types.get(local) orelse fallback;
    }

    fn storageTypeOfLocalOr(self: *Lowerer, local: LIR.LocalId, fallback: Type.TypeId) Type.TypeId {
        const logical_ty = self.typeOfLocalOr(local, fallback);
        return self.layout_owner_types.get(logical_ty) orelse logical_ty;
    }

    fn localFor(self: *Lowerer, local: Lifted.LocalId) Common.LowerError!LIR.LocalId {
        return try self.localForTyped(local, try self.lowerLocalTy(local));
    }

    fn localForTyped(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Common.LowerError!LIR.LocalId {
        if (self.typed_local_map.get(.{ .local = local, .ty = ty })) |existing| {
            try self.noteLocal(existing);
            return existing;
        }
        const index = @intFromEnum(local);
        if (self.local_map[index]) |existing| {
            try self.noteLocal(existing);
            return existing;
        }
        if (try self.mappedLocalForMatchingLocal(local)) |existing| {
            self.local_map[index] = existing;
            try self.noteLocal(existing);
            return existing;
        }
        const lir_local = try self.addTemp(try self.lowerLocalTy(local));
        try self.result.store.setLocalName(lir_local, self.solved.lifted.localName(local));
        self.local_map[index] = lir_local;
        return lir_local;
    }

    fn bindUnboundLocalForTarget(
        self: *Lowerer,
        local: Lifted.LocalId,
        ty: Type.TypeId,
        target: LIR.LocalId,
    ) Common.LowerError!LIR.LocalId {
        const index = @intFromEnum(local);
        if (self.local_map[index] != null) Common.invariant("unbound local destination was already bound");

        // Recursive values have an explicit slot representation selected before
        // ordinary value lowering. A backwards-built lookup must reserve that
        // slot now so the recursive let later initializes the same local.
        if (self.recursive_value_locals.contains(local)) {
            const binding = try self.bindRecursiveLocalForTyped(local, ty);
            if (binding.forward_local != null) {
                Common.invariant("new recursive local slot unexpectedly required a forward local");
            }
            return binding.slot;
        }

        // LIR chains are built backwards, so the first use can reach an
        // unbound local before its producer. Preserve that use's committed
        // destination layout in a distinct local; direct let lowering later
        // writes the producer into this exact slot.
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const source = try self.addLocalForLayout(target_layout);
        self.local_map[index] = source;
        try self.typed_local_map.put(.{ .local = local, .ty = ty }, source);
        try self.local_types.put(source, ty);
        try self.result.store.setLocalName(source, self.solved.lifted.localName(local));
        return source;
    }

    fn mappedLocalForMatchingLocal(self: *Lowerer, local: Lifted.LocalId) Common.LowerError!?LIR.LocalId {
        var found: ?LIR.LocalId = null;
        for (self.local_map, 0..) |maybe_existing, raw_other| {
            const existing = maybe_existing orelse continue;
            const other: Lifted.LocalId = @enumFromInt(@as(u32, @intCast(raw_other)));
            if (!self.aliasedLocalMatches(local, other)) continue;
            if (!try self.liftedLocalTypesMatch(local, other)) continue;
            if (found) |previous| {
                if (previous != existing) {
                    Common.invariant("lifted local matched multiple lowered local bindings");
                }
            } else {
                found = existing;
            }
        }
        return found;
    }

    fn liftedLocalTypesMatch(self: *Lowerer, left_id: Lifted.LocalId, right_id: Lifted.LocalId) Common.LowerError!bool {
        const left = self.solved.lifted.getLocal(left_id);
        const right = self.solved.lifted.getLocal(right_id);
        return try self.solved.lifted.types.typeEql(&self.solved.lifted.names, left.ty, right.ty);
    }

    fn existingLocalForTyped(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) ?LIR.LocalId {
        if (self.typed_local_map.get(.{ .local = local, .ty = ty })) |existing| return existing;
        return self.local_map[@intFromEnum(local)];
    }

    fn bindLocalForTyped(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Common.LowerError!LIR.LocalId {
        const index = @intFromEnum(local);
        const requested_layout = try self.layoutOfType(ty);
        const declared_layout = try self.layoutOfType(try self.lowerLocalTy(local));
        if (self.local_map[index]) |existing| {
            const existing_layout = self.result.store.getLocal(existing).layout_idx;
            if (self.layoutsMatch(existing_layout, requested_layout)) {
                try self.noteLocal(existing);
                return existing;
            }
        } else if (self.layoutsMatch(declared_layout, requested_layout)) {
            const lir_local = try self.addLocalForLayout(requested_layout);
            try self.result.store.setLocalName(lir_local, self.solved.lifted.localName(local));
            self.local_map[index] = lir_local;
            return lir_local;
        }

        const typed = TypedLiftedLocal{ .local = local, .ty = ty };
        if (self.typed_local_map.get(typed)) |existing| {
            try self.noteLocal(existing);
            return existing;
        }

        const lir_local = try self.addTemp(ty);
        try self.result.store.setLocalName(lir_local, self.solved.lifted.localName(local));
        try self.typed_local_map.put(typed, lir_local);
        return lir_local;
    }

    const RecursiveLocalBinding = struct {
        slot: LIR.LocalId,
        forward_local: ?LIR.LocalId = null,
    };

    fn rememberRecursiveSlotLocalForType(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId, slot: LIR.LocalId) Common.LowerError!void {
        try self.typed_local_map.put(.{ .local = local, .ty = ty }, slot);

        const slot_ty = try self.recursiveSlotTypeOfType(ty);
        try self.typed_local_map.put(.{ .local = local, .ty = slot_ty }, slot);

        const runtime_ty = self.runtimeBackingType(ty);
        if (runtime_ty != ty) {
            const runtime_slot_ty = try self.recursiveSlotTypeOfType(runtime_ty);
            try self.typed_local_map.put(.{ .local = local, .ty = runtime_slot_ty }, slot);
        }
    }

    fn bindRecursiveLocalForTyped(self: *Lowerer, local: Lifted.LocalId, ty: Type.TypeId) Common.LowerError!RecursiveLocalBinding {
        const slot_layout = try self.recursiveSlotLayoutOfType(ty);
        const typed = TypedLiftedLocal{ .local = local, .ty = ty };
        if (self.typed_local_map.get(typed)) |existing| {
            const existing_layout = self.result.store.getLocal(existing).layout_idx;
            if (!self.layoutsMatch(existing_layout, slot_layout)) {
                Common.invariant("recursive Monotype local was already bound with a different layout");
            }
            try self.rememberRecursiveSlotLocalForType(local, ty, existing);
            try self.noteLocal(existing);
            return .{ .slot = existing };
        }

        const index = @intFromEnum(local);
        if (self.local_map[index]) |existing| {
            const existing_layout = self.result.store.getLocal(existing).layout_idx;
            if (self.layoutsMatch(existing_layout, slot_layout)) {
                try self.rememberRecursiveSlotLocalForType(local, ty, existing);
                try self.noteLocal(existing);
                return .{ .slot = existing };
            }

            const lir_local = try self.addLocalForLayout(slot_layout);
            try self.local_types.put(lir_local, ty);
            try self.result.store.setLocalName(lir_local, self.solved.lifted.localName(local));
            try self.rememberRecursiveSlotLocalForType(local, ty, lir_local);
            return .{ .slot = lir_local, .forward_local = existing };
        }

        const lir_local = try self.addLocalForLayout(slot_layout);
        try self.local_types.put(lir_local, ty);
        try self.result.store.setLocalName(lir_local, self.solved.lifted.localName(local));
        self.local_map[index] = lir_local;
        try self.rememberRecursiveSlotLocalForType(local, ty, lir_local);
        return .{ .slot = lir_local };
    }

    fn bindLocalFromTyped(
        self: *Lowerer,
        local: Lifted.LocalId,
        source_ty: Type.TypeId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const index = @intFromEnum(local);
        if (self.local_map[index]) |target| {
            return try self.assignTypedBoundary(target, try self.lowerLocalTy(local), source, source_ty, next);
        }

        const target = try self.bindLocalForTyped(local, source_ty);
        return try self.assignTypedBoundary(target, source_ty, source, source_ty, next);
    }

    fn layoutsMatch(_: *Lowerer, lhs: layout.Idx, rhs: layout.Idx) bool {
        return lhs == rhs;
    }

    fn layoutsShareRepresentation(self: *Lowerer, lhs: layout.Idx, rhs: layout.Idx) bool {
        if (lhs == rhs) return true;
        return self.result.layouts.getLayout(lhs).eql(self.result.layouts.getLayout(rhs));
    }

    fn assignTypedBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_ty: Type.TypeId,
        source: LIR.LocalId,
        source_ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (target == source) return next;
        if (try self.maybeAssignDirectLayoutBoundary(target, source, next)) |stmt| return stmt;

        const target_runtime_ty = self.runtimeBackingType(target_ty);
        const source_runtime_ty = self.runtimeBackingType(source_ty);
        if (target_runtime_ty != target_ty or source_runtime_ty != source_ty) {
            return try self.assignTypedBoundary(target, target_runtime_ty, source, source_runtime_ty, next);
        }

        const target_content = self.types.get(target_ty);
        const source_content = self.types.get(source_ty);
        if (target_content == .erased_fn and source_content == .callable) {
            return try self.assignCallableToErasedBoundary(target, target_content.erased_fn, source, source_content.callable, next);
        }
        if (target_content == .callable and source_content == .callable) {
            return try self.assignCallableBoundary(target, target_content.callable, source, source_content.callable, next);
        }
        if (try self.assignEquivalentNamedBoundary(target, target_ty, source, source_ty, next)) |stmt| return stmt;
        if (target_content == .capture_record and source_content == .capture_record) {
            return try self.assignCaptureRecordBoundary(target, target_content.capture_record, source, source_content.capture_record, next);
        }
        if (target_content == .record and source_content == .record) {
            return try self.assignRecordBoundary(target, target_content.record, source, source_content.record, next);
        }
        if (target_content == .tuple and source_content == .tuple) {
            return try self.assignTupleBoundary(target, target_content.tuple, source, source_content.tuple, next);
        }
        if (target_content == .tag_union and source_content == .tag_union) {
            return try self.assignTagUnionBoundary(target, target_content.tag_union, source, source_content.tag_union, next);
        }

        return try self.assignBoxBoundary(target, source, self.result.store.getLocal(source).layout_idx, next);
    }

    fn assignEquivalentNamedBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_ty: Type.TypeId,
        source: LIR.LocalId,
        source_ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!?LIR.CFStmtId {
        const target_content = self.types.get(target_ty);
        if (target_content != .named) return null;
        const target_named = target_content.named;
        const source_content = self.types.get(source_ty);
        if (source_content != .named) return null;
        const source_named = source_content.named;

        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        if (!try self.publicTypesEquivalent(target_ty, source_ty, &visited)) return null;

        const target_backing = target_named.backing orelse
            Common.invariant("equivalent named boundary target had no runtime backing");
        const source_backing = source_named.backing orelse
            Common.invariant("equivalent named boundary source had no runtime backing");

        const target_backing_layout = try self.layoutOfType(target_backing.ty);
        const target_backing_local = try self.addTemp(target_backing.ty);
        const source_backing_local = try self.addTemp(source_backing.ty);

        var current = try self.assignNominalBoundary(target, target_backing_local, target_backing_layout, next);
        current = try self.assignTypedBoundary(target_backing_local, target_backing.ty, source_backing_local, source_backing.ty, current);
        return try self.assignNominalBoundary(source_backing_local, source, self.result.store.getLocal(source).layout_idx, current);
    }

    fn maybeAssignDirectLayoutBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) Common.LowerError!?LIR.CFStmtId {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const source_layout = self.result.store.getLocal(source).layout_idx;
        if (self.layoutsMatch(target_layout, source_layout)) return try self.assignLocal(target, source, next);

        const target_content = self.result.layouts.getLayout(target_layout);
        const source_content = self.result.layouts.getLayout(source_layout);
        if (target_content.tag == .box and try self.layoutsEquivalent(target_content.getIdx(), source_layout)) {
            return try self.assignUnaryLowLevel(target, .box_box, source, next);
        }
        if (target_content.tag == .box_of_zst and self.result.layouts.isZeroSized(source_content)) {
            return try self.assignUnaryLowLevel(target, .box_box, source, next);
        }
        if (source_content.tag == .box and try self.layoutsEquivalent(source_content.getIdx(), target_layout)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
        }
        if (source_content.tag == .box_of_zst and self.result.layouts.isZeroSized(target_content)) {
            return try self.assignUnaryLowLevel(target, .box_unbox, source, next);
        }
        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(source)) {
                Common.invariant("typed boundary tried to store non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }
        return null;
    }

    fn assignCallableBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_span: Type.Span,
        source: LIR.LocalId,
        source_span: Type.Span,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_variants = self.types.fnVariantSpan(source_span);
        const target_variants = self.types.fnVariantSpan(target_span);
        if (source_variants.len != target_variants.len) {
            Common.invariant("callable boundary saw different source and target variant counts");
        }
        if (self.isZstLocal(source)) return try self.assignZst(target, next);

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, source_variants.len);
        defer self.allocator.free(branches);
        for (0..source_variants.len) |source_index| {
            const source_variant = GuardedList.at(source_variants, source_index);
            const target_index = Lowerer.callableVariantIndexBySource(target_variants, source_variant.source);
            const target_variant = GuardedList.at(target_variants, target_index);
            branches[source_index] = .{
                .value = @intCast(source_index),
                .body = try self.assignCallableVariantBoundary(
                    target,
                    target_variant,
                    @intCast(target_index),
                    source,
                    source_variant,
                    @intCast(source_index),
                    next,
                ),
            };
        }

        const disc = try self.addLocalForLayout(.u16);
        const impossible = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = impossible,
            .default_is_cold = true,
            .continuation = null,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = disc,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn assignCallableToErasedBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_erased: anytype,
        source: LIR.LocalId,
        source_span: Type.Span,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_variants = self.types.fnVariantSpan(source_span);
        const target_variants = self.types.fnVariantSpan(target_erased.members);
        if (source_variants.len == 0) {
            Common.invariant("callable-to-erased boundary saw an empty source callable set");
        }

        if (self.isZstLocal(source)) {
            if (source_variants.len != 1) {
                Common.invariant("zero-sized callable-to-erased boundary saw multiple source variants");
            }
            const source_variant = GuardedList.at(source_variants, 0);
            const target_index = Lowerer.callableVariantIndexBySource(target_variants, source_variant.source);
            const target_variant = GuardedList.at(target_variants, target_index);
            return try self.assignCallableVariantToErasedBoundary(
                target,
                target_variant,
                source,
                source_variant,
                0,
                next,
            );
        }

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, source_variants.len);
        defer self.allocator.free(branches);
        for (0..source_variants.len) |source_index| {
            const source_variant = GuardedList.at(source_variants, source_index);
            const target_index = Lowerer.callableVariantIndexBySource(target_variants, source_variant.source);
            const target_variant = GuardedList.at(target_variants, target_index);
            branches[source_index] = .{
                .value = @intCast(source_index),
                .body = try self.assignCallableVariantToErasedBoundary(
                    target,
                    target_variant,
                    source,
                    source_variant,
                    @intCast(source_index),
                    next,
                ),
            };
        }

        const disc = try self.addLocalForLayout(.u16);
        const impossible = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = impossible,
            .default_is_cold = true,
            .continuation = null,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = disc,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn assignCallableVariantToErasedBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_variant: Type.FnVariant,
        source: LIR.LocalId,
        source_variant: Type.FnVariant,
        source_index: u16,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (target_variant.capture_ty == null and source_variant.capture_ty == null) {
            return try self.result.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                .target = target,
                .proc = try self.markReachableFn(target_variant.target),
                .capture = null,
                .capture_layout = null,
                .on_drop = .none,
                .reuse = try self.erasedCallableReuseForPack(target, null),
                .next = next,
            } });
        }
        const target_capture_ty = target_variant.capture_ty orelse
            Common.invariant("callable-to-erased boundary target variant dropped a capture payload");
        const source_capture_ty = source_variant.capture_ty orelse
            Common.invariant("callable-to-erased boundary source variant lacked a capture payload");

        const capture = try self.addTemp(target_capture_ty);
        const capture_layout = self.result.store.getLocal(capture).layout_idx;
        const pack = try self.result.store.addCFStmt(.{ .assign_packed_erased_fn = .{
            .target = target,
            .proc = try self.markReachableFn(target_variant.target),
            .capture = capture,
            .capture_layout = capture_layout,
            .on_drop = self.erasedCallableOnDrop(capture_layout),
            .reuse = try self.erasedCallableReuseForPack(target, capture_layout),
            .next = next,
        } });
        if (self.isZstLocal(source)) return try self.assignZst(capture, pack);

        const source_payload_layout = self.localTagPayloadLayout(source, source_index, null);
        const source_payload = try self.addLocalForLayout(source_payload_layout);
        var current = try self.assignTypedValueIntoStorage(capture, target_capture_ty, source_payload, source_capture_ty, pack);
        current = try self.assignRefRead(
            source_payload,
            source_payload_layout,
            .{ .tag_payload_struct = .{
                .source = source,
                .variant_index = source_index,
                .tag_discriminant = source_index,
            } },
            current,
        );
        return current;
    }

    fn assignCallableVariantBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_variant: Type.FnVariant,
        target_index: u16,
        source: LIR.LocalId,
        source_variant: Type.FnVariant,
        source_index: u16,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.boxBackingLayoutForDirectConstruction(target)) |backing_layout| {
            const backing_local = try self.addLocalForLayout(backing_layout);
            const boundary = try self.assignBoxBoundary(target, backing_local, backing_layout, next);
            return try self.assignCallableVariantBoundary(
                backing_local,
                target_variant,
                target_index,
                source,
                source_variant,
                source_index,
                boundary,
            );
        }

        if (target_variant.capture_ty == null and source_variant.capture_ty == null) {
            return try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = target_index,
                .discriminant = target_index,
                .payload = null,
                .next = next,
            } });
        }
        const target_capture_ty = target_variant.capture_ty orelse
            Common.invariant("callable boundary target variant dropped a capture payload");
        const source_capture_ty = source_variant.capture_ty orelse
            Common.invariant("callable boundary source variant lacked a capture payload");

        const target_payload_layout = self.localTagPayloadLayout(target, target_index, null);
        const source_payload_layout = self.localTagPayloadLayout(source, source_index, null);
        const target_payload = try self.addLocalForLayout(target_payload_layout);
        const source_payload = try self.addLocalForLayout(source_payload_layout);
        const assign_tag = try self.result.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = target_index,
            .discriminant = target_index,
            .payload = target_payload,
            .next = next,
        } });
        var current = try self.assignTypedValueIntoStorage(target_payload, target_capture_ty, source_payload, source_capture_ty, assign_tag);
        current = try self.assignRefRead(
            source_payload,
            source_payload_layout,
            .{ .tag_payload_struct = .{
                .source = source,
                .variant_index = source_index,
                .tag_discriminant = source_index,
            } },
            current,
        );
        return current;
    }

    fn assignCaptureRecordBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_span: Type.Span,
        source: LIR.LocalId,
        source_span: Type.Span,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_fields = self.types.captureFieldSpan(target_span);
        const source_fields = self.types.captureFieldSpan(source_span);
        const target_locals = try self.allocator.alloc(LIR.LocalId, target_fields.len);
        defer self.allocator.free(target_locals);
        for (0..target_fields.len) |i| {
            target_locals[i] = try self.addLocalForLayout(self.localFieldLayout(target, @intCast(i)));
        }

        var current = try self.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.result.store.addLocalSpan(target_locals),
            .next = next,
        } });
        var i = target_fields.len;
        while (i > 0) {
            i -= 1;
            const target_field = GuardedList.at(target_fields, i);
            const source_index = Lowerer.captureFieldIndexInFields(source_fields, target_field);
            const source_field = GuardedList.at(source_fields, source_index);
            current = try self.assignStructFieldBoundary(
                target_locals,
                @intCast(i),
                target_field.storage_ty,
                source,
                @intCast(source_index),
                source_field.storage_ty,
                current,
            );
        }
        return current;
    }

    fn assignRecordBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_span: Type.Span,
        source: LIR.LocalId,
        source_span: Type.Span,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_fields = self.types.fieldSpan(target_span);
        const source_fields = self.types.fieldSpan(source_span);
        const target_locals = try self.allocator.alloc(LIR.LocalId, target_fields.len);
        defer self.allocator.free(target_locals);
        for (0..target_fields.len) |i| {
            target_locals[i] = try self.addLocalForLayout(self.localFieldLayout(target, @intCast(i)));
        }

        var current = try self.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.result.store.addLocalSpan(target_locals),
            .next = next,
        } });
        var i = target_fields.len;
        while (i > 0) {
            i -= 1;
            const target_field = GuardedList.at(target_fields, i);
            const source_index = Lowerer.recordFieldIndexInFields(source_fields, target_field.name);
            const source_field = GuardedList.at(source_fields, source_index);
            current = try self.assignStructFieldBoundary(
                target_locals,
                @intCast(i),
                target_field.ty,
                source,
                @intCast(source_index),
                source_field.ty,
                current,
            );
        }
        return current;
    }

    fn assignTupleBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_span: Type.Span,
        source: LIR.LocalId,
        source_span: Type.Span,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_items = self.types.span(target_span);
        const source_items = self.types.span(source_span);
        if (target_items.len != source_items.len) Common.invariant("tuple boundary saw different arities");
        const target_locals = try self.allocator.alloc(LIR.LocalId, target_items.len);
        defer self.allocator.free(target_locals);
        for (0..target_items.len) |i| {
            target_locals[i] = try self.addLocalForLayout(self.localFieldLayout(target, @intCast(i)));
        }

        var current = try self.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.result.store.addLocalSpan(target_locals),
            .next = next,
        } });
        var i = target_items.len;
        while (i > 0) {
            i -= 1;
            current = try self.assignStructFieldBoundary(
                target_locals,
                @intCast(i),
                GuardedList.at(target_items, i),
                source,
                @intCast(i),
                GuardedList.at(source_items, i),
                current,
            );
        }
        return current;
    }

    fn assignTagUnionBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_span: Type.Span,
        source: LIR.LocalId,
        source_span: Type.Span,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_tags = self.types.tagSpan(target_span);
        const source_tags = self.types.tagSpan(source_span);
        if (source_tags.len == 0) Common.invariant("tag union boundary saw an empty source tag union");
        if (self.isZstLocal(source)) {
            if (source_tags.len != 1) Common.invariant("zero-sized source tag union had multiple variants");
            const source_tag = GuardedList.at(source_tags, 0);
            const target_index = Lowerer.tagIndexInTags(target_tags, source_tag);
            const target_tag = GuardedList.at(target_tags, target_index);
            return try self.assignTagUnionVariantBoundary(
                target,
                target_tag,
                @intCast(target_index),
                source,
                source_tag,
                0,
                next,
            );
        }

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, source_tags.len);
        defer self.allocator.free(branches);
        for (0..source_tags.len) |source_index| {
            const source_tag = GuardedList.at(source_tags, source_index);
            const target_index = Lowerer.tagIndexInTags(target_tags, source_tag);
            const target_tag = GuardedList.at(target_tags, target_index);
            branches[source_index] = .{
                .value = @intCast(source_index),
                .body = try self.assignTagUnionVariantBoundary(
                    target,
                    target_tag,
                    @intCast(target_index),
                    source,
                    source_tag,
                    @intCast(source_index),
                    next,
                ),
            };
        }

        const disc = try self.addLocalForLayout(.u16);
        const impossible = try self.result.store.addCFStmt(.{ .runtime_error = {} });
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_stmt = .{
            .cond = disc,
            .branches = try self.result.store.addCFSwitchBranches(branches),
            .default_branch = impossible,
            .default_is_cold = true,
            .continuation = null,
        } });
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = disc,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn assignTagUnionVariantBoundary(
        self: *Lowerer,
        target: LIR.LocalId,
        target_tag: Type.Tag,
        target_index: u16,
        source: LIR.LocalId,
        source_tag: Type.Tag,
        source_index: u16,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_payload_tys = self.types.span(target_tag.payloads);
        const source_payload_tys = self.types.span(source_tag.payloads);
        if (target_payload_tys.len != source_payload_tys.len) {
            Common.invariant("tag union boundary saw different payload arities");
        }
        if (self.boxBackingLayoutForDirectConstruction(target)) |backing_layout| {
            const backing_local = try self.addLocalForLayout(backing_layout);
            const boundary = try self.assignBoxBoundary(target, backing_local, backing_layout, next);
            return try self.assignTagUnionVariantBoundary(
                backing_local,
                target_tag,
                target_index,
                source,
                source_tag,
                source_index,
                boundary,
            );
        }

        if (target_payload_tys.len == 0) {
            if (self.isZstLocal(target)) return try self.assignZst(target, next);
            return try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = target_index,
                .discriminant = target_index,
                .payload = null,
                .next = next,
            } });
        }

        const target_payload_layout = self.localTagPayloadLayout(target, target_index, null);
        const source_payload_layout = self.localTagPayloadLayout(source, source_index, null);
        const target_payload = try self.addLocalForLayout(target_payload_layout);
        const source_payload = try self.addLocalForLayout(source_payload_layout);
        const assign_tag = if (self.isZstLocal(target))
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = target_index,
                .discriminant = target_index,
                .payload = target_payload,
                .next = next,
            } });

        var current = try self.assignTagPayloadBoundary(
            target_payload,
            target_payload_tys,
            source_payload,
            source_payload_tys,
            assign_tag,
        );
        current = try self.assignRefRead(
            source_payload,
            source_payload_layout,
            .{ .tag_payload_struct = .{
                .source = source,
                .variant_index = source_index,
                .tag_discriminant = source_index,
            } },
            current,
        );
        return current;
    }

    fn assignTagPayloadBoundary(
        self: *Lowerer,
        target_payload: LIR.LocalId,
        target_tys: anytype,
        source_payload: LIR.LocalId,
        source_tys: anytype,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (target_tys.len != source_tys.len) Common.invariant("tag payload boundary saw different arities");
        const owned_target_tys = try GuardedList.dupe(self.allocator, Type.TypeId, target_tys);
        defer self.allocator.free(owned_target_tys);
        const owned_source_tys = try GuardedList.dupe(self.allocator, Type.TypeId, source_tys);
        defer self.allocator.free(owned_source_tys);
        if (owned_target_tys.len == 1) {
            return try self.assignTypedValueIntoStorage(target_payload, owned_target_tys[0], source_payload, owned_source_tys[0], next);
        }

        const target_fields = try self.allocator.alloc(LIR.LocalId, owned_target_tys.len);
        defer self.allocator.free(target_fields);
        for (owned_target_tys, 0..) |_, i| {
            target_fields[i] = if (self.isZstLocal(target_payload))
                try self.addLocalForLayout(.zst)
            else
                try self.addLocalForLayout(self.localFieldLayout(target_payload, @intCast(i)));
        }

        var current = if (self.isZstLocal(target_payload))
            try self.assignZst(target_payload, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target_payload,
                .fields = try self.result.store.addLocalSpan(target_fields),
                .next = next,
            } });

        var i = owned_target_tys.len;
        while (i > 0) {
            i -= 1;
            const source_layout = if (self.isZstLocal(source_payload))
                layout.Idx.zst
            else
                self.localFieldLayout(source_payload, @intCast(i));
            const source_field = try self.addLocalForLayout(source_layout);
            current = try self.assignTypedValueIntoStorage(target_fields[i], owned_target_tys[i], source_field, owned_source_tys[i], current);
            current = try self.assignRefRead(
                source_field,
                source_layout,
                .{ .field = .{ .source = source_payload, .field_idx = @intCast(i) } },
                current,
            );
        }
        return current;
    }

    fn assignStructFieldBoundary(
        self: *Lowerer,
        target_locals: []LIR.LocalId,
        target_index: u16,
        target_ty: Type.TypeId,
        source: LIR.LocalId,
        source_index: u16,
        source_ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_layout = self.localFieldLayout(source, source_index);
        const target_storage = target_locals[target_index];
        const source_storage = try self.addLocalForLayout(source_layout);

        var current = try self.assignTypedValueIntoStorage(target_storage, target_ty, source_storage, source_ty, next);
        current = try self.assignRefRead(
            source_storage,
            source_layout,
            .{ .field = .{ .source = source, .field_idx = source_index } },
            current,
        );
        return current;
    }

    fn assignTypedValueIntoStorage(
        self: *Lowerer,
        target_storage: LIR.LocalId,
        target_ty: Type.TypeId,
        source_storage: LIR.LocalId,
        source_ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const source_value_layout = try self.layoutOfType(source_ty);
        const source_storage_layout = self.result.store.getLocal(source_storage).layout_idx;
        const source_value = if (self.layoutsShareRepresentation(source_storage_layout, source_value_layout))
            source_storage
        else
            try self.addLocalForLayout(source_value_layout);

        var current = next;
        const target_value_layout = try self.layoutOfType(target_ty);
        const target_storage_layout = self.result.store.getLocal(target_storage).layout_idx;
        if (self.layoutsShareRepresentation(target_storage_layout, target_value_layout)) {
            current = try self.assignTypedBoundary(target_storage, target_ty, source_value, source_ty, current);
        } else {
            const target_value = try self.addLocalForLayout(target_value_layout);
            current = try self.assignBoxBoundary(target_storage, target_value, target_value_layout, current);
            current = try self.assignTypedBoundary(target_value, target_ty, source_value, source_ty, current);
        }

        if (source_value != source_storage) {
            current = try self.assignBoxBoundary(source_value, source_storage, source_storage_layout, current);
        }
        return current;
    }

    fn callableVariantIndexBySource(variants: anytype, source: Common.Symbol) usize {
        for (0..variants.len) |index| {
            const variant = GuardedList.at(variants, index);
            if (variant.source == source) return index;
        }
        Common.invariant("callable boundary target type did not contain source variant");
    }

    fn tagIndexInTags(tags: anytype, needle: Type.Tag) usize {
        for (0..tags.len) |index| {
            const tag = GuardedList.at(tags, index);
            if (tag.name == needle.name and tag.checked_name == needle.checked_name) return index;
        }
        Common.invariant("tag union boundary target type did not contain source tag");
    }

    fn captureFieldIndexInFields(fields: anytype, needle: Type.CaptureField) usize {
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            // CaptureId is the exact identity of a capture-record field.
            if (field.capture_id == needle.capture_id) return index;
        }
        Common.invariant("capture record boundary target field was absent from source");
    }

    fn recordFieldIndexInFields(fields: anytype, name: Type.names.RecordFieldNameId) usize {
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            if (field.name == name) return index;
        }
        Common.invariant("record boundary target field was absent from source");
    }

    fn assignLocal(self: *Lowerer, target: LIR.LocalId, source: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (self.isZstLocal(target)) {
            if (!self.isZstLocal(source)) {
                Common.invariant("local assignment from non-zero-sized source into zero-sized target");
            }
            return try self.assignZst(target, next);
        }
        if (@import("builtin").mode == .Debug) {
            const target_layout = self.result.store.getLocal(target).layout_idx;
            const source_layout = self.result.store.getLocal(source).layout_idx;
            if (target_layout != source_layout) {
                Common.invariant("local assignment layouts differed without an explicit boundary");
            }
        }
        return try self.addAssignRef(target, .{ .local = source }, next);
    }

    fn assignZst(self: *Lowerer, target: LIR.LocalId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (!self.isZstLocal(target)) {
            Common.invariant("zero-sized assignment target was not a zero-sized layout");
        }
        return try self.result.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = LIR.LocalSpan.empty(),
            .next = next,
        } });
    }

    fn isZstLocal(self: *Lowerer, local: LIR.LocalId) bool {
        const layout_idx = self.result.store.getLocal(local).layout_idx;
        return self.result.layouts.isZeroSized(self.result.layouts.getLayout(layout_idx));
    }

    fn knownLayoutForType(self: *Lowerer, ty: Type.TypeId) ?layout.Idx {
        return self.type_layouts.get(ty);
    }

    fn rememberLayoutForType(self: *Lowerer, ty: Type.TypeId, layout_idx: layout.Idx) Common.LowerError!void {
        try self.type_layouts.put(ty, layout_idx);
    }

    fn solvedTypeAlreadyHasRecursiveSlotStorage(self: *Lowerer, ty: SolvedType.TypeVarId) Common.LowerError!bool {
        var visited = collections.DenseMap(SolvedType.TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        return try self.solvedTypeAlreadyHasRecursiveSlotStorageInner(ty, &visited);
    }

    fn solvedTypeAlreadyHasRecursiveSlotStorageInner(
        self: *Lowerer,
        ty: SolvedType.TypeVarId,
        visited: *collections.DenseMap(SolvedType.TypeVarId, void),
    ) Common.LowerError!bool {
        const root = self.solved.types.root(ty);
        if (visited.contains(root)) return false;
        try visited.put(root, {});

        const content = self.solved.types.get(root);
        if (content == .box) return true;
        if (content == .named) {
            return if (content.named.backing) |backing| blk: {
                if (content.named.kind == .@"opaque" and
                    backing.authority != .generated_private and
                    self.solved.types.rootContent(backing.ty) == .record and
                    try self.solvedTypeContainsCallable(backing.ty))
                {
                    break :blk true;
                }
                break :blk try self.solvedTypeAlreadyHasRecursiveSlotStorageInner(backing.ty, visited);
            } else false;
        }
        return false;
    }

    fn typeAlreadyHasRecursiveSlotStorage(self: *Lowerer, ty: Type.TypeId) Common.LowerError!bool {
        var visited = collections.DenseMap(Type.TypeId, void).init(self.allocator);
        defer visited.deinit();
        return try self.typeAlreadyHasRecursiveSlotStorageInner(ty, &visited);
    }

    fn typeAlreadyHasRecursiveSlotStorageInner(
        self: *Lowerer,
        ty: Type.TypeId,
        visited: *collections.DenseMap(Type.TypeId, void),
    ) Common.LowerError!bool {
        if (visited.contains(ty)) return false;
        try visited.put(ty, {});

        const content = self.types.get(ty);
        if (content == .box) return true;
        if (content == .named) {
            return if (content.named.backing) |backing| blk: {
                if (content.named.kind == .@"opaque" and
                    backing.authority != .generated_private and
                    self.types.get(backing.ty) == .record and
                    try self.typeContainsCallable(backing.ty))
                {
                    break :blk true;
                }
                break :blk try self.typeAlreadyHasRecursiveSlotStorageInner(backing.ty, visited);
            } else false;
        }
        return false;
    }

    fn recursiveSlotLayoutOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!layout.Idx {
        return try self.layoutOfType(try self.recursiveSlotTypeOfType(ty));
    }

    fn recursiveSlotTypeOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!Type.TypeId {
        if (try self.typeAlreadyHasRecursiveSlotStorage(ty)) return ty;
        return try self.boxedRecursiveSlotTypeOfType(ty);
    }

    fn boxedRecursiveSlotTypeOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!Type.TypeId {
        if (self.recursive_slot_types.get(ty)) |existing| return existing;
        const slot_ty = try self.types.add(.{ .box = ty });
        try self.recursive_slot_types.put(ty, slot_ty);
        return slot_ty;
    }

    const EquivalentNamedLayout = struct {
        ty: Type.TypeId,
        layout_idx: layout.Idx,
    };

    fn knownLayoutForEquivalentNamedType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!?EquivalentNamedLayout {
        if (self.types.get(ty) != .named) return null;

        var iterator = self.type_layouts.iterator();
        while (iterator.next()) |entry| {
            const other_ty = entry.key_ptr.*;
            if (other_ty == ty) continue;
            if (self.types.get(other_ty) != .named) continue;
            var visited = std.AutoHashMap(u64, void).init(self.allocator);
            defer visited.deinit();
            if (try self.representationTypesEquivalent(ty, other_ty, &visited)) {
                return .{ .ty = other_ty, .layout_idx = entry.value_ptr.* };
            }
        }
        return null;
    }

    /// Public equivalence compares the checked interface of two Lambda Mono
    /// types; nominal backings are private and stay uncompared. Representation
    /// equivalence additionally requires private backings (and so callable
    /// member sets) to match, which is what shared-layout reuse must key on:
    /// a layout's field slots, discriminant space, and dispatch targets are
    /// functions of the representation, not of the public interface.
    const EquivalenceMode = enum { public, representation };

    fn publicTypesEquivalent(
        self: *Lowerer,
        lhs_ty: Type.TypeId,
        rhs_ty: Type.TypeId,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        return try self.typesEquivalentInMode(.public, lhs_ty, rhs_ty, visited);
    }

    fn representationTypesEquivalent(
        self: *Lowerer,
        lhs_ty: Type.TypeId,
        rhs_ty: Type.TypeId,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        return try self.typesEquivalentInMode(.representation, lhs_ty, rhs_ty, visited);
    }

    fn typesEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs_ty: Type.TypeId,
        rhs_ty: Type.TypeId,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        if (lhs_ty == rhs_ty) return true;
        const key = (@as(u64, @intFromEnum(lhs_ty)) << 32) | @as(u64, @intFromEnum(rhs_ty));
        if (visited.contains(key)) return true;
        try visited.put(key, {});

        const lhs = self.types.get(lhs_ty);
        const rhs = self.types.get(rhs_ty);
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        return switch (lhs) {
            .primitive => |primitive| primitive == rhs.primitive,
            .zst => true,
            .erased_capture_ptr => true,
            .list => |elem| try self.typesEquivalentInMode(mode, elem, rhs.list, visited),
            .box => |elem| try self.typesEquivalentInMode(mode, elem, rhs.box, visited),
            .tuple => |items| try self.typeSpansEquivalentInMode(mode, items, rhs.tuple, visited),
            .record => |fields| try self.fieldsEquivalentInMode(mode, fields, rhs.record, visited),
            .capture_record => |fields| try self.captureFieldsEquivalentInMode(mode, fields, rhs.capture_record, visited),
            .tag_union => |tags| try self.tagsEquivalentInMode(mode, tags, rhs.tag_union, visited),
            .callable => |variants| try self.fnVariantsEquivalentInMode(mode, variants, rhs.callable, visited),
            .erased_fn => |erased| std.mem.eql(u8, erased.source_fn_ty.bytes[0..], rhs.erased_fn.source_fn_ty.bytes[0..]) and
                try self.fnVariantsEquivalentInMode(mode, erased.members, rhs.erased_fn.members, visited),
            .named => |named| try self.namedTypesEquivalentInMode(mode, named, rhs.named, visited),
        };
    }

    fn namedTypesEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs: std.meta.fieldInfo(Type.Content, .named).type,
        rhs: std.meta.fieldInfo(Type.Content, .named).type,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        if (lhs.kind != rhs.kind) return false;
        if (!std.mem.eql(u8, lhs.named_type.module.bytes[0..], rhs.named_type.module.bytes[0..])) return false;
        if (!std.mem.eql(u8, self.solved.lifted.names.moduleIdentityBytes(lhs.def.module), self.solved.lifted.names.moduleIdentityBytes(rhs.def.module))) return false;
        if (lhs.def.source_decl != rhs.def.source_decl) return false;
        if (lhs.def.source_decl == null and
            !std.mem.eql(u8, self.solved.lifted.names.typeNameText(lhs.def.type_name), self.solved.lifted.names.typeNameText(rhs.def.type_name)))
        {
            return false;
        }
        if (!std.meta.eql(lhs.builtin_owner, rhs.builtin_owner)) return false;
        if (!try self.typeSpansEquivalentInMode(mode, lhs.args, rhs.args, visited)) return false;

        if (lhs.kind == .alias) {
            const lhs_backing = lhs.backing orelse return rhs.backing == null;
            const rhs_backing = rhs.backing orelse return false;
            return try self.typesEquivalentInMode(mode, lhs_backing.ty, rhs_backing.ty, visited);
        }

        if (lhs.builtin_owner) |owner| {
            if (generatedEvidenceOwnerUsesBacking(owner)) {
                const lhs_backing = lhs.backing orelse return rhs.backing == null;
                const rhs_backing = rhs.backing orelse return false;
                return try self.typesEquivalentInMode(mode, lhs_backing.ty, rhs_backing.ty, visited);
            }
        }

        if (mode == .representation) {
            const lhs_backing = lhs.backing orelse return rhs.backing == null;
            const rhs_backing = rhs.backing orelse return false;
            if (lhs_backing.use != rhs_backing.use) return false;
            if (lhs_backing.authority != rhs_backing.authority) return false;
            return try self.typesEquivalentInMode(mode, lhs_backing.ty, rhs_backing.ty, visited);
        }

        return true;
    }

    fn generatedEvidenceOwnerUsesBacking(owner: check.StaticDispatchRegistry.BuiltinOwner) bool {
        return owner == .fields or owner == .parse_tag_union_spec;
    }

    fn typeSpansEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.span(lhs_span);
        const rhs = self.types.span(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const lhs_ty = GuardedList.at(lhs, index);
            const rhs_ty = GuardedList.at(rhs, index);
            if (!try self.typesEquivalentInMode(mode, lhs_ty, rhs_ty, visited)) return false;
        }
        return true;
    }

    fn fieldsEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.fieldSpan(lhs_span);
        const rhs = self.types.fieldSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const lhs_field = GuardedList.at(lhs, index);
            const rhs_field = GuardedList.at(rhs, index);
            if (lhs_field.name != rhs_field.name) return false;
            if (!try self.typesEquivalentInMode(mode, lhs_field.ty, rhs_field.ty, visited)) return false;
        }
        return true;
    }

    fn captureFieldsEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.captureFieldSpan(lhs_span);
        const rhs = self.types.captureFieldSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const lhs_field = GuardedList.at(lhs, index);
            const rhs_field = GuardedList.at(rhs, index);
            if (!std.meta.eql(lhs_field.capture_id, rhs_field.capture_id)) return false;
            if (!try self.typesEquivalentInMode(mode, lhs_field.ty, rhs_field.ty, visited)) return false;
        }
        return true;
    }

    fn tagsEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.tagSpan(lhs_span);
        const rhs = self.types.tagSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const lhs_tag = GuardedList.at(lhs, index);
            const rhs_tag = GuardedList.at(rhs, index);
            if (lhs_tag.name != rhs_tag.name) return false;
            if (lhs_tag.checked_name != rhs_tag.checked_name) return false;
            if (!try self.typeSpansEquivalentInMode(mode, lhs_tag.payloads, rhs_tag.payloads, visited)) return false;
        }
        return true;
    }

    fn fnVariantsEquivalentInMode(
        self: *Lowerer,
        comptime mode: EquivalenceMode,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.fnVariantSpan(lhs_span);
        const rhs = self.types.fnVariantSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const lhs_variant = GuardedList.at(lhs, index);
            const rhs_variant = GuardedList.at(rhs, index);
            if (lhs_variant.source != rhs_variant.source) return false;
            if (lhs_variant.target != rhs_variant.target) return false;
            if (!std.meta.eql(lhs_variant.capture_ty, rhs_variant.capture_ty)) {
                if (lhs_variant.capture_ty == null or rhs_variant.capture_ty == null) return false;
                if (!try self.typesEquivalentInMode(mode, lhs_variant.capture_ty.?, rhs_variant.capture_ty.?, visited)) return false;
            }
        }
        return true;
    }

    fn typeContainsCallable(self: *Lowerer, ty: Type.TypeId) Common.LowerError!bool {
        var visited = collections.DenseMap(Type.TypeId, void).init(self.allocator);
        defer visited.deinit();
        return try self.typeContainsCallableInner(ty, &visited);
    }

    fn solvedTypeContainsCallable(self: *Lowerer, ty: SolvedType.TypeVarId) Common.LowerError!bool {
        var visited = collections.DenseMap(SolvedType.TypeVarId, void).init(self.allocator);
        defer visited.deinit();
        return try self.solvedTypeContainsCallableInner(ty, &visited);
    }

    fn solvedTypeContainsCallableInner(
        self: *Lowerer,
        ty: SolvedType.TypeVarId,
        visited: *collections.DenseMap(SolvedType.TypeVarId, void),
    ) Common.LowerError!bool {
        const root = self.solved.types.root(ty);
        if (visited.contains(root)) return false;
        try visited.put(root, {});

        return switch (self.solved.types.get(root)) {
            .func, .lambda_set, .erased => true,
            .mono => Common.invariant("callable scan saw an unfinalized lazy Monotype leaf"),
            .link, .unbound, .forall => Common.invariant("callable scan saw unresolved Lambda Solved type"),
            .primitive, .zst => false,
            .list => |elem| try self.solvedTypeContainsCallableInner(elem, visited),
            .box => |elem| try self.solvedTypeContainsCallableInner(elem, visited),
            .tuple => |items| try self.solvedTypeSpanContainsCallable(items, visited),
            .record => |fields| blk: {
                const field_span = self.solved_types.fieldSpan(fields);
                for (0..field_span.len) |index| {
                    const field = GuardedList.at(field_span, index);
                    if (try self.solvedTypeContainsCallableInner(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tags| blk: {
                const tag_span = self.solved_types.tagSpan(tags);
                for (0..tag_span.len) |index| {
                    const tag = GuardedList.at(tag_span, index);
                    if (try self.solvedTypeSpanContainsCallable(tag.payloads, visited)) break :blk true;
                }
                break :blk false;
            },
            .named => |named| if (named.backing) |backing|
                try self.solvedTypeContainsCallableInner(backing.ty, visited)
            else
                false,
        };
    }

    fn solvedTypeSpanContainsCallable(
        self: *Lowerer,
        span: SolvedType.Span,
        visited: *collections.DenseMap(SolvedType.TypeVarId, void),
    ) Common.LowerError!bool {
        const values = self.solved_types.span(span);
        for (0..values.len) |index| {
            const ty = GuardedList.at(values, index);
            if (try self.solvedTypeContainsCallableInner(ty, visited)) return true;
        }
        return false;
    }

    fn typeContainsCallableInner(
        self: *Lowerer,
        ty: Type.TypeId,
        visited: *collections.DenseMap(Type.TypeId, void),
    ) Common.LowerError!bool {
        if (visited.contains(ty)) return false;
        try visited.put(ty, {});

        return switch (self.types.get(ty)) {
            .callable, .erased_fn => true,
            .primitive, .zst, .erased_capture_ptr => false,
            .list => |elem| try self.typeContainsCallableInner(elem, visited),
            .box => |elem| try self.typeContainsCallableInner(elem, visited),
            .tuple => |items| try self.typeSpanContainsCallable(items, visited),
            .record => |fields| blk: {
                const field_span = self.types.fieldSpan(fields);
                for (0..field_span.len) |index| {
                    const field = GuardedList.at(field_span, index);
                    if (try self.typeContainsCallableInner(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .capture_record => |fields| blk: {
                const field_span = self.types.captureFieldSpan(fields);
                for (0..field_span.len) |index| {
                    const field = GuardedList.at(field_span, index);
                    if (try self.typeContainsCallableInner(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tags| blk: {
                const tag_span = self.types.tagSpan(tags);
                for (0..tag_span.len) |index| {
                    const tag = GuardedList.at(tag_span, index);
                    if (try self.typeSpanContainsCallable(tag.payloads, visited)) break :blk true;
                }
                break :blk false;
            },
            .named => |named| if (named.backing) |backing|
                try self.typeContainsCallableInner(backing.ty, visited)
            else
                false,
        };
    }

    fn typeSpanContainsCallable(
        self: *Lowerer,
        span: Type.Span,
        visited: *collections.DenseMap(Type.TypeId, void),
    ) Common.LowerError!bool {
        const values = self.types.span(span);
        for (0..values.len) |index| {
            const ty = GuardedList.at(values, index);
            if (try self.typeContainsCallableInner(ty, visited)) return true;
        }
        return false;
    }

    fn layoutOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!layout.Idx {
        if (self.knownLayoutForType(ty)) |existing| return existing;
        if (try self.knownLayoutForEquivalentNamedType(ty)) |existing| {
            try self.rememberLayoutForType(ty, existing.layout_idx);
            try self.layout_owner_types.put(ty, existing.ty);
            return existing.layout_idx;
        }

        var graph = layout.Graph{};
        defer graph.deinit(self.allocator);

        const local_nodes = try self.allocator.alloc(?layout.GraphNodeId, self.types.typeCount());
        defer self.allocator.free(local_nodes);
        @memset(local_nodes, null);
        var builder = LayoutGraphBuilder{
            .lowerer = self,
            .graph = &graph,
            .local_nodes = local_nodes,
        };
        const root = try builder.inputForType(ty);
        var commit = try self.result.layouts.commitGraph(&graph, root);
        defer commit.deinit(self.allocator);

        if (layout.graphInputCommitted(root)) |layout_idx| {
            try self.rememberLayoutForType(ty, layout_idx);
            return layout_idx;
        }

        const node = layout.graphInputLocal(root) orelse Common.invariant("layout graph root was neither committed nor local");
        for (local_nodes, 0..) |maybe_node, type_index| {
            if (maybe_node) |mapped_node| {
                try self.rememberLayoutForType(@enumFromInt(type_index), commit.value_layouts[@intFromEnum(mapped_node)]);
            }
        }
        return self.knownLayoutForType(ty) orelse commit.value_layouts[@intFromEnum(node)];
    }

    const LayoutGraphBuilder = struct {
        lowerer: *Lowerer,
        graph: *layout.Graph,
        local_nodes: []?layout.GraphNodeId,

        fn inputForType(self: *LayoutGraphBuilder, ty: Type.TypeId) Common.LowerError!layout.GraphInput {
            const index = @intFromEnum(ty);
            if (self.lowerer.knownLayoutForType(ty)) |layout_idx| return layout.committedGraphInput(layout_idx);
            if (try self.lowerer.knownLayoutForEquivalentNamedType(ty)) |layout_idx| {
                try self.lowerer.rememberLayoutForType(ty, layout_idx.layout_idx);
                try self.lowerer.layout_owner_types.put(ty, layout_idx.ty);
                return layout.committedGraphInput(layout_idx.layout_idx);
            }
            if (self.local_nodes[index]) |node| return layout.localGraphInput(node);

            switch (self.lowerer.types.get(ty)) {
                .primitive => |primitive| return layout.committedGraphInput(primitiveLayout(primitive)),
                .zst => return layout.committedGraphInput(.zst),
                .erased_capture_ptr => return layout.committedGraphInput(.opaque_ptr),
                .named => |named| if (named.builtin_owner) |owner| {
                    if (builtinOwnerLayout(owner)) |layout_idx| return layout.committedGraphInput(layout_idx);
                },
                .record, .capture_record, .tuple, .tag_union, .callable, .list, .box, .erased_fn => {},
            }

            switch (self.lowerer.types.get(ty)) {
                .named => |named| if (named.backing) |backing| {
                    if (named.kind == .@"opaque" and
                        // Generated-private backing is the checked producer's
                        // explicit runtime representation, not source opacity.
                        backing.authority != .generated_private and
                        self.lowerer.types.get(backing.ty) == .record and
                        try self.lowerer.typeContainsCallable(backing.ty))
                    {
                        const node = try self.graph.reserveNode(self.lowerer.allocator);
                        self.local_nodes[index] = node;
                        self.graph.setNode(node, .{ .box = try self.inputForType(backing.ty) });
                        return layout.localGraphInput(node);
                    }

                    // A nominal or opaque record lays out its fields in declared
                    // order. The declared-order channel carries that order (the
                    // backing row stays lexicographic for name resolution); build
                    // the struct node from it and mark it nominal so the shared
                    // commit keeps declared order, repaired only for padding.
                    // Reserve the node first (mapping both the named type and its
                    // backing) so a recursive backing field resolves to it.
                    if (named.kind != .alias and named.declared_order.len != 0 and
                        self.lowerer.types.get(backing.ty) == .record)
                    {
                        const node = try self.graph.reserveNode(self.lowerer.allocator);
                        self.local_nodes[index] = node;
                        self.local_nodes[@intFromEnum(backing.ty)] = node;
                        if (try self.declaredOrderStructFields(named.declared_order, backing.ty)) |field_span| {
                            self.graph.setNode(node, .{ .struct_ = field_span });
                            try self.graph.markNominalStruct(self.lowerer.allocator, node);
                        } else {
                            self.graph.setNode(node, try self.nodeForType(backing.ty));
                        }
                        return layout.localGraphInput(node);
                    }

                    const backing_input = try self.inputForType(backing.ty);
                    if (layout.graphInputCommitted(backing_input)) |layout_idx| return layout.committedGraphInput(layout_idx);
                    if (layout.graphInputLocal(backing_input)) |node| {
                        self.local_nodes[index] = node;
                        return layout.localGraphInput(node);
                    }
                    Common.invariant("named backing layout input was neither committed nor local");
                },
                .primitive, .record, .capture_record, .tuple, .tag_union, .callable, .list, .box, .erased_fn, .erased_capture_ptr, .zst => {},
            }

            const node = try self.graph.reserveNode(self.lowerer.allocator);
            self.local_nodes[index] = node;
            self.graph.setNode(node, try self.nodeForType(ty));
            return layout.localGraphInput(node);
        }

        fn nodeForType(self: *LayoutGraphBuilder, ty: Type.TypeId) Common.LowerError!layout.GraphNode {
            return switch (self.lowerer.types.get(ty)) {
                .primitive, .zst => unreachable,
                .named => |named| blk: {
                    const backing = named.backing orelse Common.invariant("named type without runtime backing reached layout selection");
                    break :blk .{ .nominal = try self.inputForType(backing.ty) };
                },
                .record => |fields| .{ .struct_ = try self.appendStructFields(self.lowerer.types.fieldSpan(fields)) },
                .capture_record => |fields| .{ .struct_ = try self.appendCaptureFields(self.lowerer.types.captureFieldSpan(fields)) },
                .tuple => |items| .{ .struct_ = try self.appendTupleFields(self.lowerer.types.span(items)) },
                .tag_union => |tags| .{ .tag_union = try self.appendTagPayloadInputs(self.lowerer.types.tagSpan(tags)) },
                .callable => |variants| .{ .tag_union = try self.appendCallablePayloadInputs(self.lowerer.types.fnVariantSpan(variants)) },
                .list => |elem| .{ .list = try self.inputForType(elem) },
                .box => |elem| if (self.isErasedCallableValueType(elem))
                    .erased_callable
                else
                    .{ .box = try self.inputForType(elem) },
                .erased_fn => .erased_callable,
                .erased_capture_ptr => unreachable,
            };
        }

        fn isErasedCallableValueType(self: *LayoutGraphBuilder, ty: Type.TypeId) bool {
            var current = ty;
            var depth: u8 = 0;
            while (true) {
                if (depth == 32) Common.invariant("transparent alias chain exceeded layout lowering limit");
                depth += 1;
                switch (self.lowerer.types.get(current)) {
                    .erased_fn => return true,
                    .named => |named| {
                        if (named.kind != .alias) return false;
                        const backing = named.backing orelse Common.invariant("transparent alias reached layout lowering without a backing type");
                        current = backing.ty;
                    },
                    .primitive, .record, .capture_record, .tuple, .tag_union, .callable, .list, .box, .erased_capture_ptr, .zst => return false,
                }
            }
        }

        fn appendTupleFields(self: *LayoutGraphBuilder, items: anytype) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (0..items.len) |i| {
                const item = GuardedList.at(items, i);
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendStructFields(self: *LayoutGraphBuilder, items: anytype) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (0..items.len) |i| {
                const item = GuardedList.at(items, i);
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item.ty) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        /// Builds graph fields for a nominal record in declared order from its
        /// declared-order channel. Each named entry maps to the matching backing
        /// field, keeping `.index` = the field's lexicographic position so
        /// name-resolution (which indexes the lexicographic backing row) and the
        /// layout offset map stay consistent. Returns null when the backing is
        /// not a record or the declared order does not cover the backing fields,
        /// so the caller falls back to the structural path.
        fn declaredOrderStructFields(
            self: *LayoutGraphBuilder,
            declared_order: Type.Span,
            backing_ty: Type.TypeId,
        ) Common.LowerError!?layout.GraphFieldSpan {
            const backing_content = self.lowerer.types.get(backing_ty);
            if (backing_content != .record) return null;
            const backing_fields = self.lowerer.types.fieldSpan(backing_content.record);
            const entries = self.lowerer.types.declaredFieldSpan(declared_order);

            var named_count: usize = 0;
            for (0..entries.len) |entry_index| {
                const entry = GuardedList.at(entries, entry_index);
                if (entry == .named) named_count += 1;
            }
            if (named_count != backing_fields.len) return null;

            const fields = try self.lowerer.allocator.alloc(layout.GraphField, entries.len);
            defer self.lowerer.allocator.free(fields);
            // Padding spacers carry an index past every named field so they never
            // collide with a named field's original (lexicographic) index, which
            // is what `getStructFieldOffsetByOriginalIndex` looks up.
            var padding_ordinal: u16 = 0;
            for (0..entries.len) |i| {
                const entry = GuardedList.at(entries, i);
                switch (entry) {
                    .named => |name| {
                        var lexicographic_index: ?u16 = null;
                        var field_ty: Type.TypeId = undefined;
                        for (0..backing_fields.len) |idx| {
                            const field = GuardedList.at(backing_fields, idx);
                            if (field.name == name) {
                                lexicographic_index = @intCast(idx);
                                field_ty = field.ty;
                                break;
                            }
                        }
                        const idx = lexicographic_index orelse return null;
                        fields[i] = .{ .index = idx, .child = try self.inputForType(field_ty) };
                    },
                    .padding => |ty| {
                        const pad_index: u16 = @intCast(backing_fields.len + padding_ordinal);
                        padding_ordinal += 1;
                        fields[i] = .{ .index = pad_index, .child = try self.inputForType(ty), .is_padding = true };
                    },
                }
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendCaptureFields(self: *LayoutGraphBuilder, items: anytype) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (0..items.len) |i| {
                const item = GuardedList.at(items, i);
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item.storage_ty) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendTagPayloadInputs(self: *LayoutGraphBuilder, tags: anytype) Common.LowerError!layout.GraphInputSpan {
            const refs = try self.lowerer.allocator.alloc(layout.GraphInput, tags.len);
            defer self.lowerer.allocator.free(refs);
            for (0..tags.len) |i| {
                const tag = GuardedList.at(tags, i);
                refs[i] = try self.payloadInput(self.lowerer.types.span(tag.payloads));
            }
            return try self.graph.appendRefs(self.lowerer.allocator, refs);
        }

        fn appendCallablePayloadInputs(self: *LayoutGraphBuilder, variants: anytype) Common.LowerError!layout.GraphInputSpan {
            const refs = try self.lowerer.allocator.alloc(layout.GraphInput, variants.len);
            defer self.lowerer.allocator.free(refs);
            for (0..variants.len) |i| {
                const variant = GuardedList.at(variants, i);
                refs[i] = if (variant.capture_ty) |capture_ty| try self.inputForType(capture_ty) else layout.committedGraphInput(.zst);
            }
            return try self.graph.appendRefs(self.lowerer.allocator, refs);
        }

        fn payloadInput(self: *LayoutGraphBuilder, payloads: anytype) Common.LowerError!layout.GraphInput {
            return switch (payloads.len) {
                0 => layout.committedGraphInput(.zst),
                1 => try self.inputForType(GuardedList.at(payloads, 0)),
                else => blk: {
                    const node = try self.graph.reserveNode(self.lowerer.allocator);
                    self.graph.setNode(node, .{ .struct_ = try self.appendTupleFields(payloads) });
                    break :blk layout.localGraphInput(node);
                },
            };
        }
    };

    fn primitiveLayout(primitive: MonoType.Primitive) layout.Idx {
        return switch (primitive) {
            .bool => .bool,
            .str => .str,
            .u8 => .u8,
            .i8 => .i8,
            .u16 => .u16,
            .i16 => .i16,
            .u32 => .u32,
            .i32 => .i32,
            .u64 => .u64,
            .i64 => .i64,
            .u128 => .u128,
            .i128 => .i128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
            .u8x16 => .u8x16,
            .i8x16 => .i8x16,
            .u16x8 => .u16x8,
            .i16x8 => .i16x8,
            .u32x4 => .u32x4,
            .i32x4 => .i32x4,
            .u64x2 => .u64x2,
            .i64x2 => .i64x2,
        };
    }

    fn builtinOwnerLayout(owner: check.StaticDispatchRegistry.BuiltinOwner) ?layout.Idx {
        return switch (owner) {
            .bool => .bool,
            .str => .str,
            .u8 => .u8,
            .i8 => .i8,
            .u16 => .u16,
            .i16 => .i16,
            .u32 => .u32,
            .i32 => .i32,
            .u64 => .u64,
            .i64 => .i64,
            .u128 => .u128,
            .i128 => .i128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
            .u8x16 => .u8x16,
            .i8x16 => .i8x16,
            .u16x8 => .u16x8,
            .i16x8 => .i16x8,
            .u32x4 => .u32x4,
            .i32x4 => .i32x4,
            .u64x2 => .u64x2,
            .i64x2 => .i64x2,
            .list,
            .box,
            .dict,
            .set,
            .parse_tag_union_spec,
            .fields,
            .field,
            .crypto_sha256_digest,
            .crypto_sha256_hasher,
            .crypto_blake3_digest,
            .crypto_blake3_hasher,
            .iter,
            .stream,
            => null,
        };
    }

    fn tagIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.TagNameId) u16 {
        const tags = self.tagUnionTags(ty);
        for (0..tags.len) |index| {
            const tag = GuardedList.at(tags, index);
            if (tag.name == name) return @intCast(index);
        }
        Common.invariant("tag operation referenced tag absent from Lambda Mono type");
    }

    fn tagIndexByText(self: *Lowerer, ty: Type.TypeId, text: []const u8) u16 {
        const tags = self.tagUnionTags(ty);
        for (0..tags.len) |index| {
            const tag = GuardedList.at(tags, index);
            if (std.mem.eql(u8, self.solved.lifted.names.tagLabelText(tag.name), text)) return @intCast(index);
        }
        Common.invariant("tag operation referenced tag text absent from Lambda Mono type");
    }

    fn tagUnionTags(self: *Lowerer, ty: Type.TypeId) Type.StoreSpanBorrow(Type.Tag, "tags") {
        const content = self.types.get(ty);
        if (content == .tag_union) return self.types.tagSpan(content.tag_union);
        if (content != .named) Common.invariant("tag operation expected tag-union type");
        const backing = content.named.backing orelse Common.invariant("named tag has no backing");
        return self.tagUnionTags(backing.ty);
    }

    fn tupleItemTypes(self: *Lowerer, ty: Type.TypeId) Type.StoreSpanBorrow(Type.TypeId, "spans") {
        const content = self.types.get(ty);
        if (content == .tuple) return self.types.span(content.tuple);
        if (content != .named) Common.invariant("tuple operation expected tuple type");
        const backing = content.named.backing orelse Common.invariant("named tuple has no backing");
        return self.tupleItemTypes(backing.ty);
    }

    fn listElemType(self: *Lowerer, ty: Type.TypeId) Type.TypeId {
        const content = self.types.get(ty);
        if (content == .list) return content.list;
        if (content != .named) Common.invariant("list operation expected list type");
        const backing = content.named.backing orelse Common.invariant("named list has no backing");
        return self.listElemType(backing.ty);
    }

    fn tagPayloadTypesByIndex(self: *Lowerer, ty: Type.TypeId, variant_index: u16) Type.StoreSpanBorrow(Type.TypeId, "spans") {
        const tags = self.tagUnionTags(ty);
        if (variant_index >= tags.len) Common.invariant("tag operation referenced variant outside Lambda Mono type");
        return self.types.span(GuardedList.at(tags, variant_index).payloads);
    }

    fn singleTagPayloadTypeByIndex(self: *Lowerer, ty: Type.TypeId, variant_index: u16) Type.TypeId {
        const tags = self.tagUnionTags(ty);
        const payloads = self.types.span(GuardedList.at(tags, variant_index).payloads);
        if (payloads.len != 1) Common.invariant("operation expected tag with exactly one payload");
        return GuardedList.at(payloads, 0);
    }

    fn runtimeBackingType(self: *Lowerer, ty: Type.TypeId) Type.TypeId {
        const content = self.types.get(ty);
        return if (content == .named and content.named.backing != null) content.named.backing.?.ty else ty;
    }

    fn recordFields(self: *Lowerer, ty: Type.TypeId) Type.StoreSpanBorrow(Type.Field, "fields") {
        const content = self.types.get(ty);
        if (content == .record) return self.types.fieldSpan(content.record);
        if (content != .named) Common.invariant("record operation expected record type");
        const backing = content.named.backing orelse Common.invariant("named record has no backing");
        return self.recordFields(backing.ty);
    }

    fn recordFieldType(self: *Lowerer, ty: Type.TypeId, name: Type.names.RecordFieldNameId) Type.TypeId {
        const fields = self.recordFields(ty);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            if (field.name == name) return field.ty;
        }
        Common.invariant("record operation referenced field absent from Lambda Mono type");
    }

    fn recordFieldIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.RecordFieldNameId) u16 {
        const fields = self.recordFields(ty);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            if (field.name == name) return @intCast(index);
        }
        Common.invariant("record operation referenced field absent from Lambda Mono type");
    }

    fn captureFieldIndex(self: *Lowerer, ty: Type.TypeId, symbol: Common.Symbol) u16 {
        const content = self.types.get(ty);
        if (content != .capture_record) Common.invariant("capture access expected capture record type");
        const fields = self.types.captureFieldSpan(content.capture_record);
        for (0..fields.len) |index| {
            const field = GuardedList.at(fields, index);
            if (field.symbol == symbol) return @intCast(index);
        }
        Common.invariant("capture access referenced symbol absent from capture record");
    }

    fn tagUnionPayloadLayout(self: *Lowerer, tag_union_layout_idx: layout.Idx, variant_index: u16) layout.Idx {
        const tag_union_layout = self.result.layouts.getLayout(tag_union_layout_idx);
        return switch (tag_union_layout.tag) {
            .tag_union => blk: {
                const data = self.result.layouts.getTagUnionData(tag_union_layout.getTagUnion().idx);
                const variants = self.result.layouts.getTagUnionVariants(data);
                if (variant_index >= variants.len) Common.invariant("tag payload variant exceeded committed tag-union layout");
                break :blk variants.get(@intCast(variant_index)).payload_layout;
            },
            .box => self.tagUnionPayloadLayout(tag_union_layout.getIdx(), variant_index),
            .box_of_zst => .zst,
            .zst, .scalar => .zst,
            .list, .list_of_zst, .struct_, .closure, .erased_callable, .ptr => Common.invariant("tag payload operation expected tag-union layout"),
        };
    }

    fn localFieldLayout(self: *Lowerer, source: LIR.LocalId, field_index: u16) layout.Idx {
        const source_layout_idx = self.result.store.getLocal(source).layout_idx;
        const source_layout = self.result.layouts.getLayout(source_layout_idx);
        const struct_layout_idx = if (source_layout.tag == .box) source_layout.getIdx() else source_layout_idx;
        const struct_layout = self.result.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) {
            Common.invariant("field read expected a struct layout");
        }
        return self.result.layouts.getStructFieldLayoutByOriginalIndex(struct_layout.getStruct().idx, field_index);
    }

    fn localListElemLayout(self: *Lowerer, source: LIR.LocalId) layout.Idx {
        const list_layout_idx = self.result.store.getLocal(source).layout_idx;
        const list_layout = self.result.layouts.getLayout(list_layout_idx);
        return switch (list_layout.tag) {
            .list => list_layout.getIdx(),
            .list_of_zst => .zst,
            .scalar, .box, .box_of_zst, .struct_, .closure, .erased_callable, .zst, .tag_union, .ptr => Common.invariant("list expression target was not a list layout"),
        };
    }

    fn localTagPayloadLayout(self: *Lowerer, source: LIR.LocalId, variant_index: u16, payload_idx: ?u16) layout.Idx {
        const payload_layout_idx = self.tagUnionPayloadLayout(self.result.store.getLocal(source).layout_idx, variant_index);
        const index = payload_idx orelse return payload_layout_idx;
        const payload_layout = self.result.layouts.getLayout(payload_layout_idx);
        if (payload_layout.tag == .struct_) {
            return self.result.layouts.getStructFieldLayoutByOriginalIndex(payload_layout.getStruct().idx, index);
        }
        if (index != 0) Common.invariant("tag payload field read indexed a non-struct payload");
        return payload_layout_idx;
    }

    fn currentLoop(self: *Lowerer) LoopContext {
        if (self.loop_stack.items.len == 0) Common.invariant("loop control expression reached LIR outside a loop");
        return self.loop_stack.items[self.loop_stack.items.len - 1];
    }

    fn activeJoinPoint(self: *Lowerer, id: Lifted.JoinPointId) JoinContext {
        var index = self.join_stack.items.len;
        while (index > 0) {
            index -= 1;
            const join_point = self.join_stack.items[index];
            if (join_point.source_id == id) return join_point;
        }
        Common.invariant("jump expression reached LIR lowering outside its join-point scope");
    }

    fn freshJoinPointId(self: *Lowerer) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn pat(self: *const Lowerer, id: Lifted.PatId) Lifted.Pat {
        return self.solved.lifted.getPat(id);
    }
};

const TypeEquivalence = struct {
    allocator: std.mem.Allocator,
    lowerer: *Lowerer,
    materialized: *const Type.Store,
    map: collections.DenseMap(Type.TypeId, Type.TypeId),

    fn init(allocator: std.mem.Allocator, lowerer: *Lowerer, materialized: *const Type.Store) TypeEquivalence {
        return .{
            .allocator = allocator,
            .lowerer = lowerer,
            .materialized = materialized,
            .map = collections.DenseMap(Type.TypeId, Type.TypeId).init(allocator),
        };
    }

    fn deinit(self: *TypeEquivalence) void {
        self.map.deinit();
    }

    fn equivalent(self: *TypeEquivalence, direct: Type.TypeId, materialized: Type.TypeId) Common.LowerError!bool {
        if (self.map.get(direct)) |existing| return existing == materialized;
        try self.map.put(direct, materialized);

        const lhs = self.lowerer.types.get(direct);
        const rhs = self.materialized.get(materialized);
        if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

        return switch (lhs) {
            .primitive => |primitive| primitive == rhs.primitive,
            .zst => true,
            .erased_capture_ptr => true,
            .list => |elem| try self.equivalent(elem, rhs.list),
            .box => |elem| try self.equivalent(elem, rhs.box),
            .tuple => |items| try self.typeSpansEquivalent(items, rhs.tuple),
            .record => |fields| try self.fieldsEquivalent(fields, rhs.record),
            .capture_record => |fields| try self.captureFieldsEquivalent(fields, rhs.capture_record),
            .tag_union => |tags| try self.tagsEquivalent(tags, rhs.tag_union),
            .callable => |variants| try self.fnVariantsEquivalent(variants, rhs.callable),
            .erased_fn => |erased| std.mem.eql(u8, erased.source_fn_ty.bytes[0..], rhs.erased_fn.source_fn_ty.bytes[0..]) and
                try self.fnVariantsEquivalent(erased.members, rhs.erased_fn.members),
            .named => |named| blk: {
                const other = rhs.named;
                if (!std.meta.eql(named.named_type, other.named_type)) break :blk false;
                if (!std.meta.eql(named.def, other.def)) break :blk false;
                if (named.kind != other.kind) break :blk false;
                if (!std.meta.eql(named.builtin_owner, other.builtin_owner)) break :blk false;
                if (!try self.typeSpansEquivalent(named.args, other.args)) break :blk false;
                if (named.backing == null or other.backing == null) break :blk named.backing == null and other.backing == null;
                if (named.backing.?.use != other.backing.?.use) break :blk false;
                if (named.backing.?.authority != other.backing.?.authority) break :blk false;
                break :blk try self.equivalent(named.backing.?.ty, other.backing.?.ty);
            },
        };
    }

    fn typeSpansEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.span(direct);
        const rhs = self.materialized.span(materialized);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const a = GuardedList.at(lhs, index);
            const b = GuardedList.at(rhs, index);
            if (!try self.equivalent(a, b)) return false;
        }
        return true;
    }

    fn fieldsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.fieldSpan(direct);
        const rhs = self.materialized.fieldSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const a = GuardedList.at(lhs, index);
            const b = GuardedList.at(rhs, index);
            if (a.name != b.name or !try self.equivalent(a.ty, b.ty)) return false;
        }
        return true;
    }

    fn captureFieldsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.captureFieldSpan(direct);
        const rhs = self.materialized.captureFieldSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const a = GuardedList.at(lhs, index);
            const b = GuardedList.at(rhs, index);
            if (!std.meta.eql(a.capture_id, b.capture_id)) return false;
            if (!try self.equivalent(a.ty, b.ty)) return false;
        }
        return true;
    }

    fn tagsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.tagSpan(direct);
        const rhs = self.materialized.tagSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const a = GuardedList.at(lhs, index);
            const b = GuardedList.at(rhs, index);
            if (a.name != b.name or a.checked_name != b.checked_name) return false;
            if (!try self.typeSpansEquivalent(a.payloads, b.payloads)) return false;
        }
        return true;
    }

    fn fnVariantsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.fnVariantSpan(direct);
        const rhs = self.materialized.fnVariantSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (0..lhs.len) |index| {
            const a = GuardedList.at(lhs, index);
            const b = GuardedList.at(rhs, index);
            if (a.source != b.source) return false;
            if (a.capture_ty == null or b.capture_ty == null) {
                if (!(a.capture_ty == null and b.capture_ty == null)) return false;
                continue;
            }
            if (!try self.equivalent(a.capture_ty.?, b.capture_ty.?)) return false;
        }
        return true;
    }
};

fn cloneSolvedProgram(allocator: std.mem.Allocator, solved: *const Solved.Program) std.mem.Allocator.Error!Solved.Program {
    var lifted = try cloneLiftedProgram(allocator, &solved.lifted);
    errdefer lifted.deinit();

    var types = try cloneSolvedTypeStore(allocator, &solved.types);
    errdefer types.deinit();

    return .{
        .allocator = allocator,
        .lifted = lifted,
        .types = types,
        .defs = try cloneArrayList(Solved.Def, allocator, &solved.defs),
        .local_tys = try cloneArrayList(SolvedType.TypeVarId, allocator, &solved.local_tys),
        .expr_tys = try cloneArrayList(SolvedType.TypeVarId, allocator, &solved.expr_tys),
        .pat_tys = try cloneArrayList(SolvedType.TypeVarId, allocator, &solved.pat_tys),
        .fn_tys = try cloneArrayList(SolvedType.TypeVarId, allocator, &solved.fn_tys),
        .layout_requests = try cloneArrayList(Solved.LayoutRequest, allocator, &solved.layout_requests),
        .runtime_schema_requests = try cloneArrayList(Solved.RuntimeSchemaRequest, allocator, &solved.runtime_schema_requests),
    };
}

fn cloneLiftedProgram(allocator: std.mem.Allocator, program: *const Lifted.Program) std.mem.Allocator.Error!Lifted.Program {
    const view = program.view();

    var name_store = try cloneNameStore(allocator, &program.names);
    errdefer name_store.deinit();

    var types = try cloneMonoTypeStore(allocator, &program.types);
    errdefer types.deinit();

    var string_literals = try cloneStringLiterals(allocator, program.stringLiteralsView());
    errdefer deinitStringLiterals(allocator, &string_literals);

    var source_files: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (source_files.items) |file| allocator.free(file);
        source_files.deinit(allocator);
    }
    try source_files.ensureTotalCapacityPrecise(allocator, program.sourceFileNames().len);
    for (program.sourceFileNames()) |file| {
        source_files.appendAssumeCapacity(try allocator.dupe(u8, file));
    }

    var const_fn_evidence = try clonedLiftedProgramList(check.ConstStore.ConstFnEvidence, "const_fn_evidence", allocator, view.const_fn_evidence);
    errdefer const_fn_evidence.deinit(allocator);

    var const_fn_evidence_frames = try clonedLiftedProgramList(check.ConstStore.ConstFnEvidenceFrame, "const_fn_evidence_frames", allocator, view.const_fn_evidence_frames);
    errdefer const_fn_evidence_frames.deinit(allocator);

    return .{
        .allocator = allocator,
        .names = name_store,
        .next_symbol = program.next_symbol,
        .types = types,
        .imported_fns = try clonedLiftedProgramList(Lifted.ImportedFn, "imported_fns", allocator, view.imported_fns),
        .const_fn_evidence = const_fn_evidence,
        .const_fn_evidence_frames = const_fn_evidence_frames,
        .fns = try clonedLiftedProgramList(Lifted.Fn, "fns", allocator, view.fns),
        .exprs = try clonedLiftedProgramList(Lifted.Expr, "exprs", allocator, view.exprs),
        .pats = try clonedLiftedProgramList(Lifted.Pat, "pats", allocator, view.pats),
        .stmts = try clonedLiftedProgramList(Lifted.Stmt, "stmts", allocator, view.stmts),
        .locals = try clonedLiftedProgramList(Lifted.Local, "locals", allocator, view.locals),
        .expr_ids = try clonedLiftedProgramList(Lifted.ExprId, "expr_ids", allocator, view.expr_ids),
        .pat_ids = try clonedLiftedProgramList(Lifted.PatId, "pat_ids", allocator, view.pat_ids),
        .typed_locals = try clonedLiftedProgramList(Lifted.TypedLocal, "typed_locals", allocator, view.typed_locals),
        .stmt_ids = try clonedLiftedProgramList(Lifted.StmtId, "stmt_ids", allocator, view.stmt_ids),
        .field_exprs = try clonedLiftedProgramList(Lifted.FieldExpr, "field_exprs", allocator, view.field_exprs),
        .field_access_segments = try clonedLiftedProgramList(Lifted.FieldAccessSegment, "field_access_segments", allocator, view.field_access_segments),
        .fn_def_captures = try clonedLiftedProgramList(Lifted.FnDefCapture, "fn_def_captures", allocator, view.fn_def_captures),
        .capture_operands = try clonedLiftedProgramList(Lifted.CaptureOperand, "capture_operands", allocator, view.capture_operands),
        .record_destructs = try clonedLiftedProgramList(Lifted.RecordDestruct, "record_destructs", allocator, view.record_destructs),
        .str_pattern_steps = try clonedLiftedProgramList(Lifted.StrPatternStep, "str_pattern_steps", allocator, view.str_pattern_steps),
        .branches = try clonedLiftedProgramList(Lifted.Branch, "branches", allocator, view.branches),
        .if_branches = try clonedLiftedProgramList(Lifted.IfBranch, "if_branches", allocator, view.if_branches),
        .string_literals = Lifted.ProgramList(Mono.StringLiteral, "string_literals").fromArrayList(string_literals),
        .next_lift_capture_id = program.next_lift_capture_id,
        .proc_debug_names = try cloneProcDebugNameMap(allocator, &program.proc_debug_names),
        .roots = try clonedLiftedProgramList(Lifted.Root, "roots", allocator, view.roots),
        .layout_requests = try clonedLiftedProgramList(Lifted.LayoutRequest, "layout_requests", allocator, view.layout_requests),
        .runtime_schema_requests = try clonedLiftedProgramList(Lifted.RuntimeSchemaRequest, "runtime_schema_requests", allocator, view.runtime_schema_requests),
        .static_data_values = try clonedLiftedProgramList(Lifted.StaticDataValue, "static_data_values", allocator, view.static_data_values),
        .comptime_sites = Lifted.ProgramList(Lifted.ComptimeSite, "comptime_sites").fromArrayList(try cloneComptimeSites(allocator, view.comptime_sites)),
        .source_files = Lifted.ProgramList([]const u8, "source_files").fromArrayList(source_files),
        .expr_locs = try clonedLiftedProgramList(base.SourceLoc, "expr_locs", allocator, view.expr_locs),
        .expr_regions = try clonedLiftedProgramList(base.Region, "expr_regions", allocator, view.expr_regions),
        .stmt_locs = try clonedLiftedProgramList(base.SourceLoc, "stmt_locs", allocator, view.stmt_locs),
        .stmt_regions = try clonedLiftedProgramList(base.Region, "stmt_regions", allocator, view.stmt_regions),
        .inline_scopes = try clonedLiftedProgramList(Lifted.InlineScope, "inline_scopes", allocator, view.inline_scopes),
        .expr_inline_scopes = try clonedLiftedProgramList(Lifted.InlineScopeId, "expr_inline_scopes", allocator, view.expr_inline_scopes),
        .stmt_inline_scopes = try clonedLiftedProgramList(Lifted.InlineScopeId, "stmt_inline_scopes", allocator, view.stmt_inline_scopes),
        .local_names = blk: {
            var names: std.ArrayList([]const u8) = .empty;
            errdefer {
                for (names.items) |name| {
                    if (name.len > 0) allocator.free(name);
                }
                names.deinit(allocator);
            }
            try names.ensureTotalCapacityPrecise(allocator, view.local_names.len);
            for (view.local_names) |name| {
                names.appendAssumeCapacity(if (name.len == 0) "" else try allocator.dupe(u8, name));
            }
            break :blk Lifted.ProgramList([]const u8, "local_names").fromArrayList(names);
        },
        .current_loc = program.current_loc,
        .current_region = program.current_region,
        .current_inline_scope = program.current_inline_scope,
    };
}

fn clonedLiftedProgramList(
    comptime T: type,
    comptime field_name: []const u8,
    allocator: std.mem.Allocator,
    source: []const T,
) std.mem.Allocator.Error!Lifted.ProgramList(T, field_name) {
    return Lifted.ProgramList(T, field_name).fromArrayList(try cloneSlice(T, allocator, source));
}

fn cloneComptimeSites(allocator: std.mem.Allocator, source: []const Lifted.ComptimeSite) std.mem.Allocator.Error!std.ArrayList(Lifted.ComptimeSite) {
    var cloned: std.ArrayList(Lifted.ComptimeSite) = .empty;
    errdefer {
        for (cloned.items) |site| allocator.free(site.branch_regions);
        cloned.deinit(allocator);
    }
    try cloned.ensureTotalCapacityPrecise(allocator, source.len);
    for (source) |site| {
        cloned.appendAssumeCapacity(.{
            .kind = site.kind,
            .region = site.region,
            .checked_site = site.checked_site,
            .branch_regions = try allocator.dupe(base.Region, site.branch_regions),
        });
    }
    return cloned;
}

fn cloneProcDebugNameMap(allocator: std.mem.Allocator, source: *const Lifted.ProcDebugNameMap) std.mem.Allocator.Error!Lifted.ProcDebugNameMap {
    var cloned = Lifted.ProcDebugNameMap.init(allocator);
    errdefer cloned.deinit();

    try cloned.items.ensureTotalCapacityPrecise(allocator, source.items.items.len);
    for (source.items.items) |entry| {
        cloned.items.appendAssumeCapacity(entry);
    }

    return cloned;
}

fn cloneNameStore(allocator: std.mem.Allocator, source: *const check.CheckedNames.NameStore) std.mem.Allocator.Error!check.CheckedNames.NameStore {
    var cloned = check.CheckedNames.NameStore.init(allocator);
    errdefer cloned.deinit();

    // Re-intern every name in serial-id order; the ids must come back identical.
    try reinternNames("module-name", &source.module_names, &cloned, NameStore.internModuleName);
    try reinternNames("type-name", &source.type_names, &cloned, NameStore.internTypeName);
    try reinternNames("method-name", &source.method_names, &cloned, NameStore.internMethodName);
    try reinternNames("record-field", &source.record_field_labels, &cloned, NameStore.internRecordFieldLabel);
    try reinternNames("tag", &source.tag_labels, &cloned, NameStore.internTagLabel);
    try reinternNames("export-name", &source.export_names, &cloned, NameStore.internExportName);
    try reinternNames("external-symbol", &source.external_symbol_names, &cloned, NameStore.internExternalSymbolName);
    for (source.proc_bases.items.items, 0..) |key, index| {
        const id = try cloned.internProcBase(key);
        if (@intFromEnum(id) != index) Common.invariant("debug name-store clone changed proc-base ids");
    }

    return cloned;
}

const NameStore = check.CheckedNames.NameStore;

/// Re-intern every text of one source interner into `dest` in serial-id order,
/// asserting each id round-trips to its original position. A name interner
/// deduplicates, so this reproduces ids `0..count-1` exactly UNLESS the source
/// held the same text under two ids—dedup would collapse those and shift every
/// later id, which the per-id check turns into a loud invariant break rather than
/// a silently divergent clone.
fn reinternNames(
    comptime label: []const u8,
    source: anytype,
    dest: *NameStore,
    comptime intern: anytype,
) std.mem.Allocator.Error!void {
    var i: u32 = 0;
    while (i < source.count()) : (i += 1) {
        const id = try intern(dest, source.getText(i));
        if (@intFromEnum(id) != i) Common.invariant("debug name-store clone changed " ++ label ++ " ids (duplicate text in source?)");
    }
}

fn cloneMonoTypeStore(allocator: std.mem.Allocator, source: *const MonoType.Store) std.mem.Allocator.Error!MonoType.Store {
    const view = source.view();
    return .{
        .allocator = allocator,
        .types = @TypeOf(source.types).fromArrayList(try cloneSlice(MonoType.Content, allocator, view.types)),
        .type_digests = @TypeOf(source.type_digests).fromArrayList(try cloneSlice(?check.CheckedNames.TypeDigest, allocator, view.type_digests)),
        .specialization_digests = @TypeOf(source.specialization_digests).fromArrayList(try cloneSlice(?check.CheckedNames.TypeDigest, allocator, source.specializationDigestsView())),
        .constructing = @TypeOf(source.constructing).fromArrayList(try cloneSlice(bool, allocator, source.constructing.unsafeRawItemsForView())),
        .spans = @TypeOf(source.spans).fromArrayList(try cloneSlice(MonoType.TypeId, allocator, view.spans)),
        .fields = @TypeOf(source.fields).fromArrayList(try cloneSlice(MonoType.Field, allocator, view.fields)),
        .tags = @TypeOf(source.tags).fromArrayList(try cloneSlice(MonoType.Tag, allocator, view.tags)),
        .declared_fields = @TypeOf(source.declared_fields).fromArrayList(try cloneSlice(MonoType.DeclaredField, allocator, view.declared_fields)),
        .frozen = source.isFrozen(),
    };
}

fn cloneSolvedTypeStore(allocator: std.mem.Allocator, source: *const SolvedType.Store) std.mem.Allocator.Error!SolvedType.Store {
    return .{
        .allocator = allocator,
        .vars = try cloneArrayList(SolvedType.Content, allocator, &source.vars),
        .spans = try cloneArrayList(SolvedType.TypeVarId, allocator, &source.spans),
        .fields = try cloneArrayList(SolvedType.Field, allocator, &source.fields),
        .tags = try cloneArrayList(SolvedType.Tag, allocator, &source.tags),
        .captures = try cloneArrayList(SolvedType.Capture, allocator, &source.captures),
        .fn_members = try cloneArrayList(SolvedType.FnMember, allocator, &source.fn_members),
        .declared_fields = try cloneArrayList(SolvedType.DeclaredField, allocator, &source.declared_fields),
    };
}

fn cloneStringLiterals(allocator: std.mem.Allocator, source: []const Mono.StringLiteral) std.mem.Allocator.Error!std.ArrayList(Mono.StringLiteral) {
    var cloned: std.ArrayList(Mono.StringLiteral) = .empty;
    errdefer deinitStringLiterals(allocator, &cloned);
    try cloned.ensureTotalCapacity(allocator, source.len);
    for (source) |literal| {
        const backing = try allocator.dupe(u8, literal.backing);
        cloned.appendAssumeCapacity(.{
            .backing = backing,
            .offset = literal.offset,
            .len = literal.len,
        });
    }
    return cloned;
}

fn deinitStringLiterals(allocator: std.mem.Allocator, literals: *std.ArrayList(Mono.StringLiteral)) void {
    for (literals.items) |literal| allocator.free(literal.backing);
    literals.deinit(allocator);
}

fn cloneArrayList(comptime T: type, allocator: std.mem.Allocator, source: *const std.ArrayList(T)) std.mem.Allocator.Error!std.ArrayList(T) {
    return cloneSlice(T, allocator, source.items);
}

fn cloneSlice(comptime T: type, allocator: std.mem.Allocator, source: []const T) std.mem.Allocator.Error!std.ArrayList(T) {
    var cloned: std.ArrayList(T) = .empty;
    errdefer cloned.deinit(allocator);
    try cloned.appendSlice(allocator, source);
    return cloned;
}

fn constPrimitive(primitive: MonoType.Primitive) const_store.Primitive {
    return std.meta.stringToEnum(const_store.Primitive, @tagName(primitive)) orelse
        Common.invariant("monotype primitive had no ConstStore primitive equivalent");
}

fn constNamedKind(kind: MonoType.NamedKind) const_store.TypeNamedKind {
    return switch (kind) {
        .nominal => .nominal,
        .@"opaque" => .@"opaque",
        .alias => .alias,
    };
}

fn constBackingUse(use: MonoType.BackingUse) const_store.TypeBackingUse {
    return switch (use) {
        .inspectable => .inspectable,
        .runtime_layout_only => .runtime_layout_only,
    };
}

fn constBackingAuthority(authority: MonoType.BackingAuthority) const_store.TypeBackingAuthority {
    return switch (authority) {
        .checked_public => .checked_public,
        .generated_private => .generated_private,
    };
}

fn lirSymbol(symbol: Common.Symbol) LIR.Symbol {
    return LIR.Symbol.fromRaw(@intCast(@intFromEnum(symbol)));
}

fn lirInlineScopeId(scope: Lifted.InlineScopeId) LIR.InlineScopeId {
    return @enumFromInt(@intFromEnum(scope));
}

fn constFnTemplateFromMono(self: *Lowerer, template: Mono.FnTemplate) std.mem.Allocator.Error!LirProgram.FnTemplate {
    requireConstFnEvidenceTopology(template);
    const lifted = self.solved.lifted.view();
    const evidence = try self.allocator.dupe(check.ConstStore.ConstFnEvidence, lifted.const_fn_evidence[template.const_evidence.start..][0..template.const_evidence.len]);
    errdefer self.allocator.free(evidence);
    const evidence_frames = try self.allocator.dupe(check.ConstStore.ConstFnEvidenceFrame, lifted.const_fn_evidence_frames[template.const_evidence_frames.start..][0..template.const_evidence_frames.len]);
    return .{
        .fn_def = constFnDefFromMono(template.fn_def),
        .source_fn_ty = template.source_fn_ty,
        .source_fn_key = template.source_fn_key,
        .evidence = evidence,
        .evidence_frames = evidence_frames,
        .evidence_frame_head = template.const_evidence_frame_head,
    };
}

fn requireConstFnEvidenceTopology(template: Mono.FnTemplate) void {
    if (template.const_evidence_frames.len != 0 and template.const_evidence_frame_head != null) return;
    switch (template.fn_def) {
        .parser_runtime, .encoder_for_runtime => {},
        .nested => Common.invariant("nested callable reached solved LIR without explicit evidence topology"),
        .local_template, .imported_template => Common.invariant("template callable reached solved LIR without explicit evidence topology"),
        .local_hosted, .imported_hosted => Common.invariant("hosted callable reached solved LIR without explicit evidence topology"),
        .checked_generated => Common.invariant("checked-generated callable reached solved LIR without explicit evidence topology"),
    }
}

fn constFnDefFromMono(fn_def: Mono.FnDef) check.ConstStore.FnDef {
    return switch (fn_def) {
        .local_template => |template| .{ .local_template = template },
        .imported_template => |template| .{ .imported_template = template },
        .nested => |nested| .{ .nested = .{
            .owner = nested.owner,
            .site = nested.site,
            .context_fn_key = nested.context_fn_key,
            .local_proc_context_digest = nested.local_proc_context_digest,
        } },
        .local_hosted => |hosted| .{ .local_hosted = hosted.template },
        .imported_hosted => |hosted| .{ .imported_hosted = hosted.template },
        .checked_generated => |template| .{ .checked_generated = template },
        .parser_runtime => |runtime| .{ .parser_runtime = .{
            .owner = runtime.owner,
            .expr = runtime.expr,
        } },
        .encoder_for_runtime => |runtime| .{ .encoder_for_runtime = .{
            .owner = runtime.owner,
            .expr = runtime.expr,
        } },
    };
}

fn emptySolvedProgramForTest(allocator: std.mem.Allocator) Solved.Program {
    const lifted = Lifted.Program.init(
        allocator,
        NameStore.init(allocator),
        MonoType.Store.init(allocator),
        .empty, // imported_fns
        .empty, // const_fn_evidence
        .empty, // const_fn_evidence_frames
        .empty, // exprs
        .empty, // pats
        .empty, // stmts
        .empty, // locals
        .empty, // expr_ids
        .empty, // pat_ids
        .empty, // typed_locals
        .empty, // stmt_ids
        .empty, // field_exprs
        .empty, // field_access_segments
        .empty, // fn_def_captures
        .empty, // capture_operands
        .empty, // record_destructs
        .empty, // str_pattern_steps
        .empty, // branches
        .empty, // if_branches
        .empty, // string_literals
        Mono.ProcDebugNameMap.init(allocator),
        .empty, // source_files
        .empty, // expr_locs
        .empty, // expr_regions
        .empty, // stmt_locs
        .empty, // stmt_regions
        .empty, // inline_scopes
        .empty, // expr_inline_scopes
        .empty, // stmt_inline_scopes
        .empty, // local_names
        .empty, // static_data_values
        .empty, // comptime_sites
        0, // next_symbol
    );
    return Solved.Program.init(allocator, lifted);
}

test "layout lowering accepts tag payload alias backed by primitive" {
    const allocator = std.testing.allocator;

    var solved = emptySolvedProgramForTest(allocator);
    defer solved.deinit();

    const module_identity = try solved.lifted.names.internModuleIdentity(&([_]u8{0xAB} ** 32));
    const repro_name = try solved.lifted.names.internTypeName("Repro");
    const alias_name = try solved.lifted.names.internTypeName("Alias");
    const wrap_name = try solved.lifted.names.internTagLabel("Wrap");

    var lowerer = try Lowerer.init(allocator, .u64, &solved, .{});
    defer lowerer.deinit();

    const u32_ty = try lowerer.types.add(.{ .primitive = .u32 });
    const alias_ty = try lowerer.types.add(.{
        .named = .{
            // Layout lowering does not read checked type ids for this synthetic type.
            .named_type = .{ .module = .{}, .ty = undefined },
            .def = .{ .module = module_identity, .type_name = alias_name },
            .kind = .alias,
            .args = .empty(),
            .backing = .{ .ty = u32_ty, .use = .inspectable },
        },
    });

    const payloads = try lowerer.types.addSpan(&.{alias_ty});
    const tags = try lowerer.types.addTags(&.{.{
        .name = wrap_name,
        .checked_name = wrap_name,
        .payloads = payloads,
    }});
    const tag_union_ty = try lowerer.types.add(.{ .tag_union = tags });

    const repro_ty = try lowerer.types.add(.{
        .named = .{
            // Layout lowering does not read checked type ids for this synthetic type.
            .named_type = .{ .module = .{}, .ty = undefined },
            .def = .{ .module = module_identity, .type_name = repro_name },
            .kind = .nominal,
            .args = .empty(),
            .backing = .{ .ty = tag_union_ty, .use = .inspectable },
        },
    });

    // Regression for #9878: the primitive-backed alias payload lowers to the
    // primitive payload layout instead of requiring a graph node for U32.
    const repro_layout_idx = try lowerer.layoutOfType(repro_ty);
    const repro_layout = lowerer.result.layouts.getLayout(repro_layout_idx);
    try std.testing.expectEqual(layout.LayoutTag.tag_union, repro_layout.tag);

    const tag_union_info = lowerer.result.layouts.getTagUnionInfo(repro_layout);
    try std.testing.expectEqual(@as(usize, 1), tag_union_info.variants.len);
    try std.testing.expectEqual(layout.Idx.u32, tag_union_info.variants.get(0).payload_layout);
}

test "direct LIR lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}
