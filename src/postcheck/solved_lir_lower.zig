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
const layout = @import("layout");
const Common = @import("common.zig");
const Mono = @import("monotype/ast.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const SolvedInline = @import("solved_inline.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");
const LambdaMono = @import("lambda_mono/ast.zig");
const MonoType = @import("monotype/type.zig");
const Type = @import("lambda_mono/type.zig");
const lir_core = @import("lir_core");
const LIR = lir_core.LIR;
const LirProgram = lir_core.Program;
const RootMetadata = lir_core.RootMetadata.RootMetadata;
const const_store = check.ConstStore;

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
    /// Preserve source-level procedure names in LIR for runtime diagnostics.
    proc_debug_names: bool = false,
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

    try lowerer.result.store.setSourceFiles(owned.lifted.source_files.items);
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

const CaptureSpanId = struct {
    start: u32,
    len: u32,

    fn from(span: SolvedType.Span) CaptureSpanId {
        return .{ .start = span.start, .len = span.len };
    }
};

const CaptureTypeId = struct {
    start: u32,
    len: u32,
    solved_fn_ty: SolvedType.TypeVarId,

    fn from(span: SolvedType.Span, solved_fn_ty: SolvedType.TypeVarId) CaptureTypeId {
        return .{
            .start = span.start,
            .len = span.len,
            .solved_fn_ty = solved_fn_ty,
        };
    }
};

const FnSpec = struct {
    source: Lifted.FnId,
    solved_fn_ty: SolvedType.TypeVarId,
    abi: CaptureAbi,
    captures: CaptureSpanId,
    capture_ty: ?Type.TypeId,
};

const FnSpecContext = struct {
    pub fn hash(_: FnSpecContext, spec: FnSpec) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, @intFromEnum(spec.source));
        std.hash.autoHash(&hasher, @intFromEnum(spec.solved_fn_ty));
        std.hash.autoHash(&hasher, spec.abi);
        std.hash.autoHash(&hasher, spec.captures.start);
        std.hash.autoHash(&hasher, spec.captures.len);
        if (spec.capture_ty) |capture_ty| {
            std.hash.autoHash(&hasher, @intFromEnum(capture_ty));
        } else {
            std.hash.autoHash(&hasher, @as(u32, std.math.maxInt(u32)));
        }
        return hasher.final();
    }

    pub fn eql(_: FnSpecContext, lhs: FnSpec, rhs: FnSpec) bool {
        return lhs.source == rhs.source and
            lhs.solved_fn_ty == rhs.solved_fn_ty and
            lhs.abi == rhs.abi and
            lhs.captures.start == rhs.captures.start and
            lhs.captures.len == rhs.captures.len and
            lhs.capture_ty == rhs.capture_ty;
    }
};

const CaptureTypeMap = std.HashMap(CaptureTypeId, Type.TypeId, CaptureSpanContext, std.hash_map.default_max_load_percentage);

const CaptureSpanContext = struct {
    pub fn hash(_: CaptureSpanContext, span: CaptureTypeId) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, span.start);
        std.hash.autoHash(&hasher, span.len);
        std.hash.autoHash(&hasher, @intFromEnum(span.solved_fn_ty));
        return hasher.final();
    }

    pub fn eql(_: CaptureSpanContext, lhs: CaptureTypeId, rhs: CaptureTypeId) bool {
        return lhs.start == rhs.start and
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

const ConstFnTypeInfo = struct {
    args: const_store.ConstRange,
    ret: const_store.ConstTypeId,
};

const RootEntry = struct {
    fn_id: Type.FnId,
    request: check.CheckedModule.RootRequest,
};

const LayoutRequest = struct {
    checked_type: check.CheckedModule.CheckedTypeId,
    ty: Type.TypeId,
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
};

const TypedLiftedLocal = struct {
    local: Lifted.LocalId,
    ty: Type.TypeId,
};

const Lowerer = struct {
    allocator: std.mem.Allocator,
    solved: *const Solved.Program,
    types: Type.Store,
    result: LirProgram.Result,
    runtime_schemas: RuntimeSchemaStore,
    type_map: std.AutoHashMap(SolvedType.TypeVarId, Type.TypeId),
    fn_specs: std.ArrayList(FnSpec),
    fn_entries: std.ArrayList(FnEntry),
    fn_spec_map: std.HashMap(FnSpec, Type.FnId, FnSpecContext, std.hash_map.default_max_load_percentage),
    fn_written: std.ArrayList(bool),
    fn_reachable: std.ArrayList(bool),
    fn_reach_queue: std.ArrayList(Type.FnId),
    inline_plan: SolvedInline.Plan,
    inline_expects: InlineExpectMode,
    list_in_place_map: bool,
    proc_debug_names: bool,
    /// Match sites statically resolved by `foldListMapCanReuseMatch`,
    /// recorded (Debug only) so the Lambda Mono verifier replays them.
    folded_map_matches: std.ArrayList(Lifted.Program.FoldedMatch),
    source_symbols: std.AutoHashMap(Common.Symbol, Lifted.FnId),
    capture_types: CaptureTypeMap,
    captures: std.AutoHashMap(Lifted.LocalId, CaptureBinding),
    roots: std.ArrayList(RootEntry),
    layout_requests: std.ArrayList(LayoutRequest),
    runtime_schema_requests: std.ArrayList(RuntimeSchemaRequest),
    type_layouts: std.AutoHashMap(Type.TypeId, layout.Idx),
    layout_owner_types: std.AutoHashMap(Type.TypeId, Type.TypeId),
    const_plan_map: std.AutoHashMap(Type.TypeId, LirProgram.ConstPlanId),
    const_type_map: std.AutoHashMap(Type.TypeId, const_store.ConstTypeId),
    mono_const_type_map: std.AutoHashMap(MonoType.TypeId, const_store.ConstTypeId),
    callable_source_fn_map: std.AutoHashMap(Type.TypeId, SolvedType.TypeVarId),
    symbols: Common.SymbolGen,
    local_map: []?LIR.LocalId,
    typed_local_map: std.AutoHashMap(TypedLiftedLocal, LIR.LocalId),
    local_types: std.AutoHashMap(LIR.LocalId, Type.TypeId),
    comptime_site_map: []?LIR.ComptimeSiteId,
    next_join_point: u32 = 0,
    loop_stack: std.ArrayList(LoopContext),
    current_ret_ty: ?Type.TypeId = null,
    current_proc_locals: ?*ProcLocalSet = null,
    current_fn: ?Type.FnId = null,
    current_proc: ?LIR.LirProcSpecId = null,
    erased_capture_ptr_ty: ?Type.TypeId = null,

    const ProcLocalSet = std.AutoArrayHashMapUnmanaged(u32, void);

    const LoopContext = struct {
        join_id: LIR.JoinPointId,
        params: LIR.LocalSpan,
        param_tys: []const Type.TypeId,
        result_target: LIR.LocalId,
        after_loop: LIR.CFStmtId,
    };

    fn init(
        allocator: std.mem.Allocator,
        target_usize: base.target.TargetUsize,
        solved: *const Solved.Program,
        options: Options,
    ) Common.LowerError!Lowerer {
        const local_map = try allocator.alloc(?LIR.LocalId, solved.lifted.locals.items.len);
        errdefer allocator.free(local_map);
        @memset(local_map, null);

        const comptime_site_map = try allocator.alloc(?LIR.ComptimeSiteId, solved.lifted.comptime_sites.items.len);
        errdefer allocator.free(comptime_site_map);
        @memset(comptime_site_map, null);

        return .{
            .allocator = allocator,
            .solved = solved,
            .types = Type.Store.init(allocator),
            .result = try LirProgram.Result.init(allocator, target_usize),
            .runtime_schemas = RuntimeSchemaStore.init(allocator),
            .type_map = std.AutoHashMap(SolvedType.TypeVarId, Type.TypeId).init(allocator),
            .fn_specs = .empty,
            .fn_entries = .empty,
            .fn_spec_map = std.HashMap(FnSpec, Type.FnId, FnSpecContext, std.hash_map.default_max_load_percentage).initContext(allocator, .{}),
            .fn_written = .empty,
            .fn_reachable = .empty,
            .fn_reach_queue = .empty,
            .inline_plan = options.inline_plan,
            .inline_expects = options.inline_expects,
            .list_in_place_map = options.list_in_place_map,
            .proc_debug_names = options.proc_debug_names,
            .folded_map_matches = .empty,
            .source_symbols = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator),
            .capture_types = CaptureTypeMap.initContext(allocator, .{}),
            .captures = std.AutoHashMap(Lifted.LocalId, CaptureBinding).init(allocator),
            .roots = .empty,
            .layout_requests = .empty,
            .runtime_schema_requests = .empty,
            .type_layouts = std.AutoHashMap(Type.TypeId, layout.Idx).init(allocator),
            .layout_owner_types = std.AutoHashMap(Type.TypeId, Type.TypeId).init(allocator),
            .const_plan_map = std.AutoHashMap(Type.TypeId, LirProgram.ConstPlanId).init(allocator),
            .const_type_map = std.AutoHashMap(Type.TypeId, const_store.ConstTypeId).init(allocator),
            .mono_const_type_map = std.AutoHashMap(MonoType.TypeId, const_store.ConstTypeId).init(allocator),
            .callable_source_fn_map = std.AutoHashMap(Type.TypeId, SolvedType.TypeVarId).init(allocator),
            .symbols = .{ .next = solved.lifted.next_symbol },
            .local_map = local_map,
            .typed_local_map = std.AutoHashMap(TypedLiftedLocal, LIR.LocalId).init(allocator),
            .local_types = std.AutoHashMap(LIR.LocalId, Type.TypeId).init(allocator),
            .comptime_site_map = comptime_site_map,
            .loop_stack = .empty,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.folded_map_matches.deinit(self.allocator);
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.comptime_site_map);
        self.typed_local_map.deinit();
        self.local_types.deinit();
        self.allocator.free(self.local_map);
        self.const_plan_map.deinit();
        self.const_type_map.deinit();
        self.mono_const_type_map.deinit();
        self.callable_source_fn_map.deinit();
        self.layout_owner_types.deinit();
        self.type_layouts.deinit();
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
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
        self.loop_stack.deinit(self.allocator);
        self.allocator.free(self.comptime_site_map);
        self.typed_local_map.deinit();
        self.local_types.deinit();
        self.allocator.free(self.local_map);
        self.const_plan_map.deinit();
        self.const_type_map.deinit();
        self.mono_const_type_map.deinit();
        self.callable_source_fn_map.deinit();
        self.layout_owner_types.deinit();
        self.type_layouts.deinit();
        self.runtime_schema_requests.deinit(self.allocator);
        self.layout_requests.deinit(self.allocator);
        self.roots.deinit(self.allocator);
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
        self.typed_local_map = std.AutoHashMap(TypedLiftedLocal, LIR.LocalId).init(self.allocator);
        self.local_types = std.AutoHashMap(LIR.LocalId, Type.TypeId).init(self.allocator);
        self.comptime_site_map = &.{};
        self.loop_stack = .empty;
        self.folded_map_matches = .empty;
        return output;
    }

    fn lower(self: *Lowerer) Common.LowerError!void {
        try self.indexSourceFns();

        try self.roots.ensureTotalCapacity(self.allocator, self.solved.lifted.roots.items.len);
        for (self.solved.lifted.roots.items) |root| {
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

    fn indexSourceFns(self: *Lowerer) Common.LowerError!void {
        for (self.solved.lifted.fns.items, 0..) |fn_, index| {
            const fn_id: Lifted.FnId = @enumFromInt(@as(u32, @intCast(index)));
            const result = try self.source_symbols.getOrPut(fn_.symbol);
            if (result.found_existing) Common.invariant("two lifted functions had the same symbol");
            result.value_ptr.* = fn_id;
        }
    }

    fn lowerReachableFns(self: *Lowerer) Common.LowerError!void {
        var index: usize = 0;
        while (index < self.fn_reach_queue.items.len) : (index += 1) {
            const fn_id = self.fn_reach_queue.items[index];
            const fn_index = @intFromEnum(fn_id);
            if (self.fn_written.items[fn_index]) continue;
            try self.lowerFnSpec(fn_id, self.fn_specs.items[fn_index]);
        }
    }

    fn lowerFnSpec(self: *Lowerer, fn_id: Type.FnId, spec: FnSpec) Common.LowerError!void {
        const proc_id = try self.procPlaceholder(fn_id);
        const entry = self.fn_entries.items[@intFromEnum(fn_id)];
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(spec.source)];

        self.captures.clearRetainingCapacity();
        @memset(self.local_map, null);
        self.typed_local_map.clearRetainingCapacity();
        self.local_types.clearRetainingCapacity();

        const proc_args = self.result.store.getLocalSpan(self.result.store.getProcSpec(proc_id).args);
        const solved_fn_ty = spec.solved_fn_ty;
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("direct Lambda Mono function table contains a non-function type"),
        };
        const solved_args = self.solved.types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (solved_args.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");
        if (proc_args.len < lifted_args.len) Common.invariant("direct Lambda Mono proc placeholder had too few arguments");

        for (lifted_args, 0..) |arg, i| {
            const arg_ty = try self.lowerType(solved_args[i]);
            self.local_map[@intFromEnum(arg.local)] = proc_args[i];
            try self.typed_local_map.put(.{
                .local = arg.local,
                .ty = arg_ty,
            }, proc_args[i]);
            try self.local_types.put(proc_args[i], arg_ty);
        }

        switch (spec.abi) {
            .finite => {
                if (spec.capture_ty) |capture_ty| {
                    if (proc_args.len != lifted_args.len + 1) Common.invariant("finite capture proc placeholder had wrong arity");
                    try self.bindCaptureRecord(spec.captures, capture_ty, .{ .record = proc_args[lifted_args.len] });
                } else if (proc_args.len != lifted_args.len) {
                    Common.invariant("finite non-capturing proc placeholder had wrong arity");
                }
            },
            .erased => {
                if (proc_args.len != lifted_args.len + 1) Common.invariant("erased proc placeholder had wrong arity");
                if (spec.capture_ty) |capture_ty| {
                    try self.bindCaptureRecord(spec.captures, capture_ty, .{ .erased_ptr = proc_args[lifted_args.len] });
                }
            },
        }

        switch (source_fn.body) {
            .roc => |body_expr| {
                const saved_ret_ty = self.current_ret_ty;
                const saved_proc_locals = self.current_proc_locals;
                const saved_current_fn = self.current_fn;
                const saved_current_proc = self.current_proc;
                var proc_locals: ProcLocalSet = .{};
                defer proc_locals.deinit(self.allocator);

                self.current_proc_locals = &proc_locals;
                self.current_ret_ty = entry.ret;
                self.current_fn = fn_id;
                self.current_proc = proc_id;
                defer {
                    self.current_ret_ty = saved_ret_ty;
                    self.current_proc_locals = saved_proc_locals;
                    self.current_fn = saved_current_fn;
                    self.current_proc = saved_current_proc;
                }

                try self.noteLocalSpan(self.result.store.getProcSpec(proc_id).args);
                const body = try self.lowerExprReturn(body_expr, entry.ret);

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
        return switch (source.fn_def) {
            .local_hosted,
            .imported_hosted,
            => |hosted| .{
                .symbol = try self.result.store.insertString(
                    self.solved.lifted.names.externalSymbolNameText(hosted.external_symbol_name),
                ),
                .dispatch_index = hosted.dispatch_index,
            },
            else => null,
        };
    }

    fn ensureOwnFnSpec(self: *Lowerer, fn_id: Lifted.FnId, abi: CaptureAbi) Common.LowerError!Type.FnId {
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        const solved_fn_ty = self.solved.types.root(self.solved.fn_tys.items[@intFromEnum(fn_id)]);
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("direct Lambda Mono function table contains a non-function type"),
        };

        const callable = self.solved.types.rootContent(func.callable);
        const members = switch (callable) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("function callable slot was unresolved before direct Lambda Mono"),
        };

        for (self.solved.types.memberSpan(members)) |member| {
            if (member.lambda != fn_symbol) continue;
            return try self.ensureFnSpec(fn_id, solved_fn_ty, abi, member.captures);
        }

        if (std.meta.activeTag(callable) == .erased) {
            return try self.ensureFnSpec(fn_id, solved_fn_ty, abi, .empty());
        }
        Common.invariant("function callable slot did not contain its own lambda member");
    }

    fn ensureFnSpec(
        self: *Lowerer,
        source: Lifted.FnId,
        solved_fn_ty: SolvedType.TypeVarId,
        abi: CaptureAbi,
        captures: SolvedType.Span,
    ) Common.LowerError!Type.FnId {
        const capture_items = self.solved.types.captureSpan(captures);
        const root_fn_ty = self.solved.types.root(solved_fn_ty);
        const spec = FnSpec{
            .source = source,
            .solved_fn_ty = root_fn_ty,
            .abi = abi,
            .captures = CaptureSpanId.from(captures),
            .capture_ty = if (capture_items.len == 0) null else try self.captureRecordType(captures, root_fn_ty),
        };

        const result = try self.fn_spec_map.getOrPut(spec);
        if (result.found_existing) return result.value_ptr.*;

        const fn_id: Type.FnId = @enumFromInt(@as(u32, @intCast(self.fn_specs.items.len)));
        result.value_ptr.* = fn_id;
        try self.fn_specs.append(self.allocator, spec);
        try self.fn_written.append(self.allocator, false);
        try self.fn_reachable.append(self.allocator, false);
        try self.fn_entries.append(self.allocator, undefined);
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(source)];
        const symbol = self.symbols.fresh();
        const func = switch (self.solved.types.rootContent(spec.solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("direct Lambda Mono function table contains a non-function type"),
        };
        const solved_args = self.solved.types.span(func.args);
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
        const source_fn = self.solved.lifted.fns.items[@intFromEnum(spec.source)];
        const arg_tys = self.types.span(entry.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (arg_tys.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");

        const arg_count = lifted_args.len + if (entry.capture_arg_ty == null) @as(usize, 0) else 1;
        const arg_locals = try self.allocator.alloc(LIR.LocalId, arg_count);
        defer self.allocator.free(arg_locals);

        for (arg_tys, 0..) |arg_ty, i| {
            arg_locals[i] = try self.addLocalForLayout(try self.layoutOfType(arg_ty));
        }
        if (entry.capture_arg_ty) |capture_arg_ty| {
            arg_locals[lifted_args.len] = try self.addLocalForLayout(try self.layoutOfType(capture_arg_ty));
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
        const proc = try self.result.store.addProcSpec(.{
            .name = lirSymbol(entry.symbol),
            .args = args_span,
            .body = null,
            .ret_layout = ret_layout,
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

    fn sourceFnForSymbol(self: *Lowerer, symbol: Common.Symbol) Lifted.FnId {
        return self.source_symbols.get(symbol) orelse
            Common.invariant("direct Lambda Mono callable member referenced a missing lifted function symbol");
    }

    fn bindCaptureRecord(self: *Lowerer, captures_id: CaptureSpanId, capture_ty: Type.TypeId, source: CaptureSource) Common.LowerError!void {
        const captures = self.solved.types.captureSpan(.{ .start = captures_id.start, .len = captures_id.len });
        const fields = switch (self.types.get(capture_ty)) {
            .capture_record => |fields| self.types.captureFieldSpan(fields),
            else => Common.invariant("function capture argument was not a capture record"),
        };
        if (captures.len != fields.len) Common.invariant("function capture argument arity differed from capture slots");

        for (captures, fields) |capture, field| {
            if (capture.capture_id != field.capture_id) {
                Common.invariant("function capture argument fields differed from capture slots");
            }
            try self.captures.put(capture.local, .{
                .source = source,
                .record_ty = capture_ty,
                .symbol = capture.symbol,
                .ty = field.ty,
            });
        }
    }

    fn lowerLocalInto(self: *Lowerer, target: LIR.LocalId, local: Lifted.LocalId, ty: Type.TypeId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (try self.captureBindingForLocal(local)) |capture| {
            return try self.lowerCaptureBindingInto(target, capture, next);
        }
        if (self.typed_local_map.get(.{ .local = local, .ty = ty })) |source| {
            try self.noteLocal(source);
            return try self.assignTypedBoundary(target, ty, source, ty, next);
        }
        return try self.assignTypedBoundary(target, ty, try self.localForTyped(local, ty), try self.lowerLocalTy(local), next);
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
        const local = self.solved.lifted.locals.items[@intFromEnum(local_id)];
        const capture = self.solved.lifted.locals.items[@intFromEnum(capture_local)];
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
        const local = self.solved.lifted.locals.items[@intFromEnum(local_id)];
        const alias = self.solved.lifted.locals.items[@intFromEnum(other)];
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
            capture.ty,
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

    fn memberCapturesForExpr(self: *Lowerer, expr_id: Lifted.ExprId, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        const expr_ty = self.solved.expr_tys.items[@intFromEnum(expr_id)];
        const callable = switch (self.solved.types.rootContent(expr_ty)) {
            .func => |func| func.callable,
            .lambda_set, .erased => expr_ty,
            else => Common.invariant("function reference expression had no callable Lambda Solved type"),
        };
        const members = switch (self.solved.types.rootContent(callable)) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("function reference callable slot was unresolved before direct Lambda Mono"),
        };
        for (self.solved.types.memberSpan(members)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        Common.invariant("function reference callable slot did not contain referenced function");
    }

    fn capturesForFn(self: *Lowerer, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        const func = switch (self.solved.types.rootContent(self.solved.fn_tys.items[@intFromEnum(fn_id)])) {
            .func => |func| func,
            else => Common.invariant("direct Lambda Mono function table contains a non-function type"),
        };
        const callable = switch (self.solved.types.rootContent(func.callable)) {
            .lambda_set => |members| members,
            .erased => |erased| erased.members,
            else => Common.invariant("callable value did not have a resolved callable slot"),
        };
        for (self.solved.types.memberSpan(callable)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        return .empty();
    }

    fn erasedCapturePtrType(self: *Lowerer) Common.LowerError!Type.TypeId {
        if (self.erased_capture_ptr_ty) |ty| return ty;
        const ty = try self.types.add(.erased_capture_ptr);
        self.erased_capture_ptr_ty = ty;
        return ty;
    }

    fn captureRecordType(
        self: *Lowerer,
        captures: SolvedType.Span,
        solved_fn_ty: SolvedType.TypeVarId,
    ) Common.LowerError!Type.TypeId {
        const id = CaptureTypeId.from(captures, self.solved.types.root(solved_fn_ty));
        if (self.capture_types.get(id)) |existing| return existing;

        const capture_items = self.solved.types.captureSpan(captures);
        const fields = try self.allocator.alloc(Type.CaptureField, capture_items.len);
        defer self.allocator.free(fields);
        for (capture_items, 0..) |capture, i| {
            fields[i] = .{
                .symbol = capture.symbol,
                .binder = capture.binder,
                .capture_id = capture.capture_id,
                .ty = try self.lowerType(capture.ty),
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
        const expr = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .field_access => |field| self.recordFieldType(
                try self.lowerExprContextTy(field.receiver),
                field.field,
            ),
            .tuple_access => |access| blk: {
                const items = self.tupleItemTypes(try self.lowerExprContextTy(access.tuple));
                if (access.elem_index >= items.len) Common.invariant("tuple access index exceeded tuple type");
                break :blk items[@intCast(access.elem_index)];
            },
            else => try self.lowerExprTy(expr_id),
        };
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
                const lowered = try self.lowerTypeSpan(self.solved.types.span(items));
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.types.addSpan(lowered) };
            },
            .record => |fields| blk: {
                const lowered = try self.allocator.alloc(Type.Field, fields.len);
                defer self.allocator.free(lowered);
                for (self.solved.types.fieldSpan(fields), 0..) |field, i| {
                    lowered[i] = .{ .name = field.name, .ty = try self.lowerType(field.ty) };
                }
                break :blk .{ .record = try self.types.addFields(lowered) };
            },
            .tag_union => |tags| blk: {
                const lowered = try self.allocator.alloc(Type.Tag, tags.len);
                defer self.allocator.free(lowered);
                for (self.solved.types.tagSpan(tags), 0..) |tag, i| {
                    const payloads = try self.lowerTypeSpan(self.solved.types.span(tag.payloads));
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
                const args = try self.lowerTypeSpan(self.solved.types.span(named.args));
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
        return switch (self.solved.types.rootContent(callable)) {
            .lambda_set => |members| .{ .callable = try self.lowerFnMembers(members, .finite, solved_fn_ty) },
            .erased => |erased| .{ .erased_fn = .{
                .source_fn_ty = erased.source_fn_ty,
                .members = try self.lowerFnMembers(erased.members, .erased, solved_fn_ty),
            } },
            else => Common.invariant("function callable slot was unresolved before direct Lambda Mono"),
        };
    }

    /// Re-materializes a nominal record's declared field order from the Lambda
    /// Solved store into this lowerer's Lambda Mono store. Named entries copy the
    /// shared field-name id; padding entries re-lower their reserved type.
    fn lowerDeclaredOrder(self: *Lowerer, span: SolvedType.Span) Common.LowerError!Type.Span {
        const source = self.solved.types.declaredFieldSpan(span);
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
        const solved_members = self.solved.types.memberSpan(members);
        const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
        defer self.allocator.free(variants);
        const root_fn_ty = self.solved.types.root(solved_fn_ty);
        for (solved_members, 0..) |member, i| {
            const source = self.sourceFnForSymbol(member.lambda);
            const target = try self.ensureFnSpec(
                source,
                root_fn_ty,
                abi,
                member.captures,
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
        const solved_members = self.solved.types.memberSpan(members);
        const variants = try self.allocator.alloc(Type.FnVariant, solved_members.len);
        defer self.allocator.free(variants);
        for (solved_members, 0..) |member, i| {
            const source = self.sourceFnForSymbol(member.lambda);
            const target = try self.ensureFnSpec(
                source,
                self.solved.types.root(self.solved.fn_tys.items[@intFromEnum(source)]),
                abi,
                member.captures,
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
        return self.solved.lifted.string_literals.items[@intFromEnum(id)];
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
        for (self.result.store.getLocalSpan(span)) |local| {
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
            try self.result.root_metadata.append(self.allocator, RootMetadata.fromCheckedRoot(root.request));
            if (root.request.abi == .compile_time) {
                try self.result.const_roots.append(self.allocator, .{
                    .root_order = root.request.order,
                    .request = root.request,
                    .proc = proc,
                    .ret_layout = try self.layoutOfType(entry.ret),
                    .plan = try self.constPlanOfType(entry.ret),
                });
            }
        }

        for (self.layout_requests.items) |request| {
            try self.result.requested_layouts.append(self.allocator, .{
                .ty = self.types.typeDigest(&self.solved.lifted.names, request.ty),
                .checked_type = request.checked_type,
                .layout_idx = try self.layoutOfType(request.ty),
                .plan = try self.constPlanOfType(request.ty),
            });
        }
    }

    fn constPlanOfType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!LirProgram.ConstPlanId {
        if (self.const_plan_map.get(ty)) |existing| return existing;

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
                => .scalar,
            },
            .zst => .zst,
            .list => |elem| .{ .list = try self.constPlanOfType(elem) },
            .box => |elem| .{ .box = try self.constPlanOfType(elem) },
            .tuple => |items| blk: {
                const source = self.types.span(items);
                const plans = try self.allocator.alloc(LirProgram.ConstPlanId, source.len);
                errdefer self.allocator.free(plans);
                for (source, 0..) |item, i| plans[i] = try self.constPlanOfType(item);
                break :blk .{ .tuple = plans };
            },
            .record => |fields| blk: {
                const source = self.types.fieldSpan(fields);
                const plans = try self.allocator.alloc(LirProgram.ConstPlanId, source.len);
                errdefer self.allocator.free(plans);
                for (source, 0..) |field, i| plans[i] = try self.constPlanOfType(field.ty);
                break :blk .{ .record = plans };
            },
            .tag_union => |tags| blk: {
                const source = self.types.tagSpan(tags);
                const variants = try self.allocator.alloc(LirProgram.ConstTagVariant, source.len);
                var initialized: usize = 0;
                errdefer {
                    for (variants[0..initialized]) |variant| {
                        self.allocator.free(variant.name);
                        self.allocator.free(variant.payloads);
                    }
                    self.allocator.free(variants);
                }
                for (source, 0..) |tag, i| {
                    const name = try self.allocator.dupe(u8, self.solved.lifted.names.tagLabelText(tag.name));
                    errdefer self.allocator.free(name);
                    const payload_tys = self.types.span(tag.payloads);
                    const payloads = try self.allocator.alloc(LirProgram.ConstPlanId, payload_tys.len);
                    var payloads_owned = true;
                    errdefer if (payloads_owned) self.allocator.free(payloads);
                    for (payload_tys, 0..) |payload_ty, j| payloads[j] = try self.constPlanOfType(payload_ty);
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

    fn constTypeDef(self: *Lowerer, def: MonoType.TypeDef) std.mem.Allocator.Error!const_store.TypeDef {
        return .{
            .module_name = try self.result.const_type_names.internModuleName(self.solved.lifted.names.moduleNameText(def.module_name)),
            .type_name = try self.result.const_type_names.internTypeName(self.solved.lifted.names.typeNameText(def.type_name)),
            .source_decl = def.source_decl,
        };
    }

    fn buildConstType(self: *Lowerer, ty: Type.TypeId) Common.LowerError!const_store.ConstType {
        return switch (self.types.get(ty)) {
            .primitive => |primitive| .{ .primitive = constPrimitive(primitive) },
            .zst => .zst,
            .list => |elem| .{ .list = try self.constTypeOfType(elem) },
            .box => |elem| .{ .box = try self.constTypeOfType(elem) },
            .tuple => |items| blk: {
                const source = self.types.span(items);
                const out = try self.allocator.alloc(const_store.ConstTypeId, source.len);
                defer self.allocator.free(out);
                for (source, 0..) |item, i| out[i] = try self.constTypeOfType(item);
                break :blk .{ .tuple = try self.result.const_types.appendTypeSpan(out) };
            },
            .record => |fields| blk: {
                const source = self.types.fieldSpan(fields);
                const out = try self.allocator.alloc(const_store.TypeField, source.len);
                defer self.allocator.free(out);
                for (source, 0..) |field, i| {
                    out[i] = .{
                        .name = try self.constRecordFieldName(field.name),
                        .ty = try self.constTypeOfType(field.ty),
                    };
                }
                break :blk .{ .record = try self.result.const_types.appendFieldSpan(out) };
            },
            .tag_union => |tags| blk: {
                const source = self.types.tagSpan(tags);
                const out = try self.allocator.alloc(const_store.TypeTag, source.len);
                defer self.allocator.free(out);
                for (source, 0..) |tag, i| {
                    const payloads = self.types.span(tag.payloads);
                    const stored_payloads = try self.allocator.alloc(const_store.ConstTypeId, payloads.len);
                    defer self.allocator.free(stored_payloads);
                    for (payloads, 0..) |payload, j| stored_payloads[j] = try self.constTypeOfType(payload);
                    out[i] = .{
                        .name = try self.constTagName(tag.name),
                        .checked_name = try self.constTagName(tag.checked_name),
                        .payloads = try self.result.const_types.appendTypeSpan(stored_payloads),
                    };
                }
                break :blk .{ .tag_union = try self.result.const_types.appendTagSpan(out) };
            },
            .named => |named| blk: {
                const args = self.types.span(named.args);
                const stored_args = try self.allocator.alloc(const_store.ConstTypeId, args.len);
                defer self.allocator.free(stored_args);
                for (args, 0..) |arg, i| stored_args[i] = try self.constTypeOfType(arg);

                const declared = self.types.declaredFieldSpan(named.declared_order);
                const stored_declared = try self.allocator.alloc(const_store.TypeDeclaredField, declared.len);
                defer self.allocator.free(stored_declared);
                for (declared, 0..) |entry, i| {
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
                    } else null,
                    .declared_order = try self.result.const_types.appendDeclaredFieldSpan(stored_declared),
                } };
            },
            .callable => |variants| return try self.constFuncTypeForCallable(ty, variants),
            .erased_fn => |erased| return try self.constFuncTypeForErased(erased.members),
            .capture_record => Common.invariant("capture record reached ConstStore type output as a captured value"),
            .erased_capture_ptr => Common.invariant("erased capture pointer reached ConstStore type output as a captured value"),
        };
    }

    fn constFuncTypeForCallable(self: *Lowerer, ty: Type.TypeId, variants_span: Type.Span) Common.LowerError!const_store.ConstType {
        const variants = self.types.fnVariantSpan(variants_span);
        if (variants.len == 0) return try self.constFuncTypeForEmptyCallable(ty);
        const function = try self.constFuncTypeForVariants(variants);
        return .{ .func = .{ .args = function.args, .ret = function.ret } };
    }

    fn constFuncTypeForEmptyCallable(self: *Lowerer, ty: Type.TypeId) Common.LowerError!const_store.ConstType {
        const solved_fn_ty = self.callable_source_fn_map.get(ty) orelse
            Common.invariant("empty callable const type lacked source function type");
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("empty callable source type was not a function"),
        };

        const source_args = self.solved.types.span(func.args);
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

    fn constFuncTypeForErased(self: *Lowerer, variants_span: Type.Span) Common.LowerError!const_store.ConstType {
        const variants = self.types.fnVariantSpan(variants_span);
        if (variants.len == 0) Common.invariant("erased function capture type had no function variants");
        const function = try self.constFuncTypeForVariants(variants);
        return .{ .func = .{ .args = function.args, .ret = function.ret } };
    }

    fn constFuncTypeForVariants(
        self: *Lowerer,
        variants: []const Type.FnVariant,
    ) Common.LowerError!ConstFnTypeInfo {
        const first_template = self.fnTemplateForFn(variants[0].target);
        const first_digest = self.solved.lifted.types.typeDigest(&self.solved.lifted.names, first_template.mono_fn_ty);
        for (variants[1..]) |variant| {
            const template = self.fnTemplateForFn(variant.target);
            const digest = self.solved.lifted.types.typeDigest(&self.solved.lifted.names, template.mono_fn_ty);
            if (!std.mem.eql(u8, first_digest.bytes[0..], digest.bytes[0..])) {
                Common.invariant("callable capture variants had different source-level function types");
            }
        }
        const fn_ty = try self.constTypeOfMonoType(first_template.mono_fn_ty);
        return switch (self.result.const_types.get(fn_ty)) {
            .func => |function| .{ .args = function.args, .ret = function.ret },
            else => Common.invariant("callable capture source type was not a function"),
        };
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
                for (source, 0..) |item, i| out[i] = try self.constTypeOfMonoType(item);
                break :blk .{ .tuple = try self.result.const_types.appendTypeSpan(out) };
            },
            .func => |function| blk: {
                const args = mono_types.span(function.args);
                const stored_args = try self.allocator.alloc(const_store.ConstTypeId, args.len);
                defer self.allocator.free(stored_args);
                for (args, 0..) |arg, i| stored_args[i] = try self.constTypeOfMonoType(arg);
                break :blk .{ .func = .{
                    .args = try self.result.const_types.appendTypeSpan(stored_args),
                    .ret = try self.constTypeOfMonoType(function.ret),
                } };
            },
            .record => |fields| blk: {
                const source = mono_types.fieldSpan(fields);
                const out = try self.allocator.alloc(const_store.TypeField, source.len);
                defer self.allocator.free(out);
                for (source, 0..) |field, i| {
                    out[i] = .{
                        .name = try self.constRecordFieldName(field.name),
                        .ty = try self.constTypeOfMonoType(field.ty),
                    };
                }
                break :blk .{ .record = try self.result.const_types.appendFieldSpan(out) };
            },
            .tag_union => |tags| blk: {
                const source = mono_types.tagSpan(tags);
                const out = try self.allocator.alloc(const_store.TypeTag, source.len);
                defer self.allocator.free(out);
                for (source, 0..) |tag, i| {
                    const payloads = mono_types.span(tag.payloads);
                    const stored_payloads = try self.allocator.alloc(const_store.ConstTypeId, payloads.len);
                    defer self.allocator.free(stored_payloads);
                    for (payloads, 0..) |payload, j| stored_payloads[j] = try self.constTypeOfMonoType(payload);
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
                for (args, 0..) |arg, i| stored_args[i] = try self.constTypeOfMonoType(arg);

                const declared = mono_types.declaredFieldSpan(named.declared_order);
                const stored_declared = try self.allocator.alloc(const_store.TypeDeclaredField, declared.len);
                defer self.allocator.free(stored_declared);
                for (declared, 0..) |entry, i| {
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
            }
            self.allocator.free(variants);
        }

        for (type_variants, 0..) |variant, index| {
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
                .template = constFnTemplateFromMono(self.fnTemplateForFn(variant.target)),
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
        const members = self.types.fnVariantSpan(erased.members);
        if (members.len == 0) {
            Common.invariant("erased function ConstStore output requires explicit erased function entries");
        }

        const entries = try self.allocator.alloc(LirProgram.ErasedFn, members.len);
        var initialized: usize = 0;
        errdefer {
            for (entries[0..initialized]) |entry| {
                if (entry.captures.len > 0) self.allocator.free(entry.captures);
            }
            self.allocator.free(entries);
        }

        for (members, 0..) |member, index| {
            const captures = if (member.capture_ty) |capture_ty|
                try self.captureSlotsForType(capture_ty)
            else
                &.{};
            var captures_owned = captures.len > 0;
            errdefer if (captures_owned) self.allocator.free(captures);

            entries[index] = .{
                .entry = try self.markReachableFn(member.target),
                .capture_layout = if (member.capture_ty) |capture_ty| try self.layoutOfType(capture_ty) else .zst,
                .template = constFnTemplateFromMono(self.fnTemplateForFn(member.target)),
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
        const fields = switch (self.types.get(ty)) {
            .capture_record => |fields| self.types.captureFieldSpan(fields),
            else => Common.invariant("function result capture slot output expected capture record type"),
        };
        const slots = try self.allocator.alloc(LirProgram.CaptureSlot, fields.len);
        errdefer self.allocator.free(slots);
        for (fields, 0..) |field, index| {
            slots[index] = .{
                .id = field.capture_id orelse Common.invariant("capture record field had no CaptureId"),
                .slot = @intCast(index),
                .ty = try self.constTypeOfType(field.ty),
                .plan = try self.constPlanOfType(field.ty),
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
        const named = switch (content) {
            .named => |named| named,
            else => Common.invariant("runtime schema request did not reference a named Lambda Mono type"),
        };
        if (named.def.module_name != request.def.module_name or named.def.type_name != request.def.type_name) {
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
        return switch (self.types.get(ty)) {
            .record => .record,
            .tag_union => .tag_union,
            .named => |named| if (named.backing) |backing| blk: {
                if (backing.use != .inspectable) {
                    Common.invariant("runtime schema request crossed a non-inspectable named backing");
                }
                break :blk self.runtimeSchemaShape(backing.ty);
            } else Common.invariant("runtime schema request crossed a named type without backing"),
            else => Common.invariant("runtime schema request backing was not a record or tag union"),
        };
    }

    fn writeRecordSchema(self: *Lowerer, type_name: []const u8, ty: Type.TypeId) Common.LowerError!void {
        const source = self.recordFields(ty);
        const schema_fields = try self.allocator.alloc(RuntimeRecordFieldSchema, source.len);
        errdefer {
            for (schema_fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema_fields);
        }
        for (source, 0..) |field, index| {
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
        for (source, 0..) |tag, index| {
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
        const LambdaMonoLower = @import("lambda_mono/lower.zig");

        var solved_clone = try cloneSolvedProgram(self.allocator, self.solved);
        var clone_owned = true;
        errdefer if (clone_owned) solved_clone.deinit();

        var materialized = try LambdaMonoLower.run(self.allocator, solved_clone, self.folded_map_matches.items, .{
            .inline_expects = switch (self.inline_expects) {
                .run => .run,
                .omit => .omit,
            },
        });
        clone_owned = false;
        defer materialized.deinit();

        var type_equivalence = TypeEquivalence.init(self.allocator, self, &materialized.types);
        defer type_equivalence.deinit();

        try self.verifyFnEntriesMatch(&materialized);
        try self.verifyRootsMatch(&materialized);
        try self.verifyLayoutRequestsMatch(&materialized, &type_equivalence);
        try self.verifyRuntimeSchemaRequestsMatch(&materialized, &type_equivalence);
    }

    fn verifyFnEntriesMatch(self: *Lowerer, materialized: *const LambdaMono.Program) Common.LowerError!void {
        var reachable_count: usize = 0;
        for (self.fn_reachable.items) |reachable| {
            if (reachable) reachable_count += 1;
        }
        if (reachable_count > materialized.fns.items.len) {
            Common.invariant("debug Lambda Mono verifier saw too many direct function specs");
        }
        const used = try self.allocator.alloc(bool, materialized.fns.items.len);
        defer self.allocator.free(used);
        @memset(used, false);

        for (self.fn_entries.items, 0..) |entry, entry_index| {
            // Direct LIR keeps type-level entries so callable layouts and
            // ConstStore metadata can refer to source templates without forcing
            // every queued Lambda Mono function into a LIR proc.
            if (!self.fn_reachable.items[entry_index]) continue;
            for (materialized.fns.items, 0..) |fn_, index| {
                if (used[index]) continue;
                if (try self.fnEntryMatchesMaterialized(entry, fn_, materialized)) {
                    used[index] = true;
                    break;
                }
            } else {
                Common.invariant("debug Lambda Mono verifier could not match a direct function spec");
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
        for (arg_tys, 0..) |arg_ty, arg_index| {
            if (!try type_equivalence.equivalent(arg_ty, args[arg_index].ty)) return false;
        }
        if (entry.capture_arg_ty) |capture_arg_ty| {
            if (!try type_equivalence.equivalent(capture_arg_ty, args[arg_tys.len].ty)) return false;
        }
        return true;
    }

    fn verifyRootsMatch(self: *Lowerer, materialized: *const LambdaMono.Program) Common.LowerError!void {
        if (self.roots.items.len != materialized.roots.items.len) {
            Common.invariant("debug Lambda Mono verifier saw a root count mismatch");
        }
        for (self.roots.items, materialized.roots.items) |direct, expected| {
            if (!std.meta.eql(direct.request, expected.request)) {
                Common.invariant("debug Lambda Mono verifier saw a root mismatch");
            }
            const expected_fn = materialized.fns.items[@intFromEnum(expected.fn_id)];
            if (!try self.fnEntryMatchesMaterialized(self.fn_entries.items[@intFromEnum(direct.fn_id)], expected_fn, materialized)) {
                Common.invariant("debug Lambda Mono verifier saw a root mismatch");
            }
        }
    }

    fn verifyLayoutRequestsMatch(self: *Lowerer, materialized: *const LambdaMono.Program, type_equivalence: *TypeEquivalence) Common.LowerError!void {
        if (self.layout_requests.items.len != materialized.layout_requests.items.len) {
            Common.invariant("debug Lambda Mono verifier saw a layout request count mismatch");
        }
        for (self.layout_requests.items, materialized.layout_requests.items) |direct, expected| {
            if (direct.checked_type != expected.checked_type or !try type_equivalence.equivalent(direct.ty, expected.ty)) {
                Common.invariant("debug Lambda Mono verifier saw a layout request mismatch");
            }
        }
    }

    fn verifyRuntimeSchemaRequestsMatch(self: *Lowerer, materialized: *const LambdaMono.Program, type_equivalence: *TypeEquivalence) Common.LowerError!void {
        if (self.runtime_schema_requests.items.len != materialized.runtime_schema_requests.items.len) {
            Common.invariant("debug Lambda Mono verifier saw a runtime schema request count mismatch");
        }
        for (self.runtime_schema_requests.items, materialized.runtime_schema_requests.items) |direct, expected| {
            if (!std.meta.eql(direct.def, expected.def) or !try type_equivalence.equivalent(direct.ty, expected.ty)) {
                Common.invariant("debug Lambda Mono verifier saw a runtime schema request mismatch");
            }
        }
    }

    fn lowerExprReturn(self: *Lowerer, expr_id: Lifted.ExprId, ret_ty: Type.TypeId) Common.LowerError!LIR.CFStmtId {
        const ret_local = try self.addTemp(ret_ty);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
        return try self.lowerExprIntoAtType(ret_local, expr_id, ret_ty, ret_stmt);
    }

    fn lowerExprInto(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const expr_data = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        const expr_ty = try self.lowerExprTy(expr_id);
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
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
                const bytes_lit = self.stringLiteral(literal);
                break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .bytes_literal = try self.result.store.insertStringView(bytes_lit.backing, bytes_lit.offset, bytes_lit.len) },
                    .next = next,
                } });
            },
            .uninitialized, .uninitialized_payload => next,
            .list => |items| try self.lowerListInto(target, items, next),
            .tuple => |items| try self.lowerTupleInto(target, items, next),
            .record => |fields| try self.lowerRecordInto(target, expr_ty, fields, next),
            .tag => |tag| try self.lowerTagInto(target, expr_ty, tag.name, tag.payloads, next),
            .lambda,
            .def_ref,
            .fn_def,
            => Common.invariant("pre-lift function expression reached direct LIR lowering"),
            .fn_ref => |fn_ref| try self.lowerFnRefInto(target, expr_id, fn_ref.fn_id, self.solved.lifted.captureOperandSpan(fn_ref.captures), next),
            .nominal => |backing| try self.lowerNominalInto(target, expr_ty, backing, next),
            .let_ => |let_| try self.lowerLetInto(target, let_, next),
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
            .field_access => |field| try self.lowerFieldAccessInto(target, field.receiver, field.field, next),
            .tuple_access => |access| try self.lowerTupleAccessInto(target, access.tuple, access.elem_index, next),
            .structural_eq => |eq| try self.lowerStructuralEqInto(target, eq.lhs, eq.rhs, eq.negated, next),
            .structural_hash => |h| try self.lowerStructuralHashInto(target, h.value, h.hasher, next),
            .match_ => |match_| try self.lowerMatchInto(target, expr_ty, match_.scrutinee, match_.branches, match_.comptime_site, next),
            .if_ => |if_| try self.lowerIfInto(target, expr_ty, if_.branches, if_.final_else, next),
            .if_initialized_payload => |payload_switch| try self.lowerInitializedPayloadSwitchInto(target, payload_switch, next),
            .try_sequence => |sequence| try self.lowerTrySequenceInto(target, expr_ty, sequence, next),
            .try_record_sequence => |sequence| try self.lowerTryRecordSequenceInto(target, expr_ty, sequence, next),
            .block => |block| try self.lowerBlockIntoAtType(target, expr_ty, block.statements, block.final_expr, next),
            .loop_ => |loop| try self.lowerLoopInto(target, loop, next),
            .break_ => |value| try self.lowerBreak(value),
            .continue_ => |continue_| try self.lowerContinue(continue_.values),
            .return_ => |ret| try self.lowerReturn(ret),
            .crash => |msg| try self.result.store.addCFStmt(.{ .crash = .{
                .msg = try self.result.store.insertString(self.stringLiteralText(msg)),
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
        const expr_data = self.solved.lifted.exprs.items[@intFromEnum(expr_id)];
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
        self.result.store.current_loc = self.solved.lifted.exprLoc(expr_id);
        self.result.store.current_region = self.solved.lifted.exprRegion(expr_id);

        return switch (expr_data.data) {
            .local => |local| try self.lowerLocalInto(target, local, ty, next),
            .list => |items| try self.lowerListIntoAtType(target, ty, items, next),
            .tuple => |items| try self.lowerTupleIntoAtType(target, ty, items, next),
            .record => |fields| try self.lowerRecordInto(target, ty, fields, next),
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
            .field_access => |field| try self.lowerFieldAccessInto(target, field.receiver, field.field, next),
            .call_value => |call| try self.lowerValueCallInto(target, ty, call.callee, self.solved.lifted.exprSpan(call.args), next),
            .match_ => |match_| try self.lowerMatchInto(target, ty, match_.scrutinee, match_.branches, match_.comptime_site, next),
            .if_ => |if_| try self.lowerIfInto(target, ty, if_.branches, if_.final_else, next),
            .try_sequence => |sequence| try self.lowerTrySequenceInto(target, ty, sequence, next),
            .try_record_sequence => |sequence| try self.lowerTryRecordSequenceInto(target, ty, sequence, next),
            .block => |block| try self.lowerBlockIntoAtType(target, ty, block.statements, block.final_expr, next),
            else => try self.lowerExprInto(target, expr_id, next),
        };
    }

    fn lowerListInto(self: *Lowerer, target: LIR.LocalId, span: Lifted.Span(Lifted.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const items = self.solved.lifted.exprSpan(span);
        const expr_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(expr_locals);
        const elem_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(elem_locals);

        const elem_layout = self.localListElemLayout(target);
        for (items, 0..) |expr_id, i| {
            expr_locals[i] = try self.addTemp(try self.lowerExprTy(expr_id));
            const expr_layout = self.result.store.getLocal(expr_locals[i]).layout_idx;
            elem_locals[i] = if (expr_layout == elem_layout)
                expr_locals[i]
            else
                try self.addLocalForLayout(elem_layout);
        }

        var current = try self.result.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.result.store.addLocalSpan(elem_locals),
            .next = next,
        } });
        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (elem_locals[i] != expr_locals[i]) {
                current = try self.assignBoxBoundary(
                    elem_locals[i],
                    expr_locals[i],
                    self.result.store.getLocal(expr_locals[i]).layout_idx,
                    current,
                );
            }
            current = try self.lowerExprInto(expr_locals[i], items[i], current);
        }
        return current;
    }

    fn lowerListIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        ty: Type.TypeId,
        span: Lifted.Span(Lifted.ExprId),
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const items = self.solved.lifted.exprSpan(span);
        const expr_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(expr_locals);
        const elem_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(elem_locals);

        const elem_ty = self.listElemType(ty);
        const elem_layout = self.localListElemLayout(target);
        for (items, 0..) |_, i| {
            expr_locals[i] = try self.addTemp(elem_ty);
            const expr_layout = self.result.store.getLocal(expr_locals[i]).layout_idx;
            elem_locals[i] = if (expr_layout == elem_layout)
                expr_locals[i]
            else
                try self.addLocalForLayout(elem_layout);
        }

        var current = try self.result.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.result.store.addLocalSpan(elem_locals),
            .next = next,
        } });
        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (elem_locals[i] != expr_locals[i]) {
                current = try self.assignBoxBoundary(
                    elem_locals[i],
                    expr_locals[i],
                    self.result.store.getLocal(expr_locals[i]).layout_idx,
                    current,
                );
            }
            current = try self.lowerExprIntoAtType(expr_locals[i], items[i], elem_ty, current);
        }
        return current;
    }

    fn lowerTupleInto(self: *Lowerer, target: LIR.LocalId, span: Lifted.Span(Lifted.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const items = self.solved.lifted.exprSpan(span);
        return try self.lowerStructExprsInto(target, items, next);
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

    fn lowerStructExprsInto(self: *Lowerer, target: LIR.LocalId, items: []const Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const item_tys = try self.allocator.alloc(Type.TypeId, items.len);
        defer self.allocator.free(item_tys);
        for (items, 0..) |expr_id, i| {
            item_tys[i] = try self.lowerExprTy(expr_id);
        }
        return try self.lowerStructExprsIntoAtTypes(target, items, item_tys, next);
    }

    fn lowerStructExprsIntoAtTypes(
        self: *Lowerer,
        target: LIR.LocalId,
        items: []const Lifted.ExprId,
        item_tys: []const Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (items.len != item_tys.len) Common.invariant("struct expression arity differed from target type arity");
        if (self.boxBackingLayoutForDirectConstruction(target)) |backing_layout| {
            const backing_local = try self.addLocalForLayout(backing_layout);
            const boundary = try self.assignBoxBoundary(target, backing_local, backing_layout, next);
            return try self.lowerStructExprsIntoAtTypes(backing_local, items, item_tys, boundary);
        }

        const expr_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(expr_locals);
        const field_locals = try self.allocator.alloc(LIR.LocalId, items.len);
        defer self.allocator.free(field_locals);

        const target_is_zst = self.isZstLocal(target);
        for (items, 0..) |_, i| {
            expr_locals[i] = try self.addTemp(item_tys[i]);
            if (target_is_zst) {
                field_locals[i] = expr_locals[i];
                continue;
            }
            const field_layout = self.localFieldLayout(target, @intCast(i));
            const expr_layout = self.result.store.getLocal(expr_locals[i]).layout_idx;
            field_locals[i] = if (expr_layout == field_layout)
                expr_locals[i]
            else
                try self.addLocalForLayout(field_layout);
        }

        var current = if (target_is_zst)
            try self.assignZst(target, next)
        else
            try self.result.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.result.store.addLocalSpan(field_locals),
                .next = next,
            } });
        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (field_locals[i] != expr_locals[i]) {
                current = try self.assignBoxBoundary(
                    field_locals[i],
                    expr_locals[i],
                    self.result.store.getLocal(expr_locals[i]).layout_idx,
                    current,
                );
            }
            current = try self.lowerExprIntoAtType(expr_locals[i], items[i], item_tys[i], current);
        }
        return current;
    }

    fn lowerCaptureRecordFromCaptureExprsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        capture_span: SolvedType.Span,
        capture_operands: []const Lifted.CaptureOperand,
        capture_ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const captures = self.solved.types.captureSpan(capture_span);
        const fields = switch (self.types.get(capture_ty)) {
            .capture_record => |fields| self.types.captureFieldSpan(fields),
            else => Common.invariant("callable capture payload was not a capture record"),
        };
        if (captures.len != fields.len) Common.invariant("callable capture payload arity differed from captured locals");
        if (captures.len != capture_operands.len) {
            Common.invariant("function reference capture operand count differed from lifted function captures");
        }

        const expr_locals = try self.allocator.alloc(LIR.LocalId, captures.len);
        defer self.allocator.free(expr_locals);
        const field_locals = try self.allocator.alloc(LIR.LocalId, captures.len);
        defer self.allocator.free(field_locals);

        const target_is_zst = self.isZstLocal(target);
        // Member captures, capture-record fields, and keyed operands are all in
        // ascending CaptureId order, so the join is an exact indexed walk.
        for (captures, fields, capture_operands, 0..) |capture, field, operand, i| {
            if (capture.capture_id != field.capture_id) {
                Common.invariant("callable capture payload fields differed from captured locals");
            }
            const capture_id = capture.capture_id orelse Common.invariant("member capture had no CaptureId");
            if (operand.id != capture_id) Common.invariant("capture operand CaptureId did not match its member capture slot");
            expr_locals[i] = try self.addTemp(field.ty);
            if (target_is_zst) {
                field_locals[i] = expr_locals[i];
                continue;
            }
            const field_layout = self.localFieldLayout(target, @intCast(i));
            const expr_layout = self.result.store.getLocal(expr_locals[i]).layout_idx;
            field_locals[i] = if (expr_layout == field_layout)
                expr_locals[i]
            else
                try self.addLocalForLayout(field_layout);
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
            if (field_locals[i] != expr_locals[i]) {
                current = try self.assignBoxBoundary(
                    field_locals[i],
                    expr_locals[i],
                    self.result.store.getLocal(expr_locals[i]).layout_idx,
                    current,
                );
            }
            current = try self.lowerExprInto(expr_locals[i], capture_operands[i].value, current);
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

        for (type_fields, 0..) |field, i| {
            ordered[i] = for (expr_fields) |expr_field| {
                if (expr_field.name == field.name) break expr_field.value;
            } else Common.invariant("record expression missing field output by Lambda Mono type");
            field_tys[i] = field.ty;
        }

        return try self.lowerStructExprsIntoAtTypes(target, ordered, field_tys, next);
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

        if (payloads.len == 1) {
            return try self.lowerExprIntoAtType(payload_local, payloads[0], payload_tys[0], assign_tag);
        }
        return try self.lowerStructExprsIntoAtTypes(payload_local, payloads, payload_tys, assign_tag);
    }

    fn boxBackingLayoutForDirectConstruction(self: *Lowerer, target: LIR.LocalId) ?layout.Idx {
        const target_layout = self.result.store.getLocal(target).layout_idx;
        const target_content = self.result.layouts.getLayout(target_layout);
        return switch (target_content.tag) {
            .box => target_content.getIdx(),
            .box_of_zst => .zst,
            else => null,
        };
    }

    fn lowerNominalInto(self: *Lowerer, target: LIR.LocalId, nominal_ty: Type.TypeId, backing: Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const backing_ty = try self.nominalBackingType(nominal_ty, backing);
        const backing_layout = try self.layoutOfType(backing_ty);
        const backing_local = try self.addLocalForLayout(backing_layout);
        const assign = try self.assignNominalBoundaryAtTypes(target, nominal_ty, backing_local, backing_ty, backing_layout, next);
        return try self.lowerExprInto(backing_local, backing, assign);
    }

    fn nominalBackingType(self: *Lowerer, nominal_ty: Type.TypeId, backing: Lifted.ExprId) Common.LowerError!Type.TypeId {
        return switch (self.types.get(nominal_ty)) {
            .named => |named| (named.backing orelse Common.invariant("nominal expression target had no runtime backing")).ty,
            .box => |elem| elem,
            else => try self.lowerExprTy(backing),
        };
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
            return try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{ .backing_ref = backing_local } },
                .next = next,
            } });
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

        Common.invariant("LIR lowering expected layouts to match or differ by an explicit Box edge");
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
            return try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = op,
                .next = next,
            } });
        }

        const storage_local = try self.addLocalForLayout(storage_layout);
        const after_read = try self.assignBoxBoundary(target, storage_local, storage_layout, next);
        if (self.isZstLocal(storage_local)) return try self.assignZst(storage_local, after_read);
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = storage_local,
            .op = op,
            .next = after_read,
        } });
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

    fn lowerLetInto(self: *Lowerer, target: LIR.LocalId, let_: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const rest = try self.lowerExprInto(target, let_.rest, next);
        const value_local = try self.addTemp(try self.lowerExprTy(let_.value));
        const bind = try self.bindPatternOrCrash(let_.bind, value_local, rest, let_.comptime_site);
        return try self.lowerExprInto(value_local, let_.value, bind);
    }

    fn lowerFnRefInto(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        fn_id: Lifted.FnId,
        capture_operands: []const Lifted.CaptureOperand,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        return try self.lowerFnRefIntoAtType(target, expr_id, fn_id, capture_operands, try self.lowerExprTy(expr_id), next);
    }

    fn lowerFnRefIntoAtType(
        self: *Lowerer,
        target: LIR.LocalId,
        expr_id: Lifted.ExprId,
        fn_id: Lifted.FnId,
        capture_operands: []const Lifted.CaptureOperand,
        ty: Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const captures = self.memberCapturesForExpr(expr_id, fn_id);
        if (self.solved.lifted.typedLocalSpan(self.solved.lifted.fns.items[@intFromEnum(fn_id)].captures).len != capture_operands.len) {
            Common.invariant("function reference capture operand count differed from lifted function captures");
        }
        const fn_symbol = self.solved.lifted.fns.items[@intFromEnum(fn_id)].symbol;
        return switch (self.types.get(ty)) {
            .callable => |variants_span| blk: {
                const variants = self.types.fnVariantSpan(variants_span);
                for (variants, 0..) |variant, variant_index| {
                    if (variant.source != fn_symbol) continue;
                    break :blk try self.lowerFiniteCallableValueInto(target, @intCast(variant_index), fn_id, captures, capture_operands, variant.capture_ty, next);
                }
                Common.invariant("finite callable type did not contain referenced function");
            },
            .erased_fn => |erased| blk: {
                for (self.types.fnVariantSpan(erased.members)) |variant| {
                    if (variant.source != fn_symbol) continue;
                    break :blk try self.lowerPackedErasedFnInto(target, variant.target, fn_id, captures, capture_operands, variant.capture_ty, next);
                }
                Common.invariant("erased callable type did not contain referenced function");
            },
            else => Common.invariant("function value lowered to non-callable direct Lambda Mono type"),
        };
    }

    fn lowerFiniteCallableValueInto(
        self: *Lowerer,
        target: LIR.LocalId,
        variant_index: u16,
        lifted_fn_id: Lifted.FnId,
        captures: SolvedType.Span,
        capture_operands: []const Lifted.CaptureOperand,
        capture_ty: ?Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (capture_ty) |payload_ty| {
            if (self.solved.lifted.typedLocalSpan(self.solved.lifted.fns.items[@intFromEnum(lifted_fn_id)].captures).len != capture_operands.len) {
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
        lifted_fn_id: Lifted.FnId,
        captures: SolvedType.Span,
        capture_operands: []const Lifted.CaptureOperand,
        capture_ty: ?Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (self.solved.lifted.typedLocalSpan(self.solved.lifted.fns.items[@intFromEnum(lifted_fn_id)].captures).len != capture_operands.len) {
            Common.invariant("erased callable capture operand count differed from lifted function captures");
        }
        const capture = if (capture_ty) |ty| try self.addTemp(ty) else null;
        const capture_layout = if (capture) |local| self.result.store.getLocal(local).layout_idx else null;
        const on_drop: LIR.ErasedCallableOnDrop = if (capture_layout) |layout_idx| blk: {
            const helper_key = layout.RcHelper{ .op = .decref, .layout_idx = layout_idx };
            break :blk if (self.result.layouts.rcHelperPlan(helper_key) == .noop)
                .none
            else
                .{ .rc_helper = helper_key };
        } else .none;

        const assign = try self.result.store.addCFStmt(.{ .assign_packed_erased_fn = .{
            .target = target,
            .proc = try self.markReachableFn(fn_id),
            .capture = capture,
            .capture_layout = capture_layout,
            .on_drop = on_drop,
            .next = next,
        } });
        if (capture_ty) |ty| return try self.lowerCaptureRecordFromCaptureExprsInto(capture.?, captures, capture_operands, ty, assign);
        if (capture_operands.len != 0) Common.invariant("erased callable without capture payload carried capture operands");
        return assign;
    }

    fn lowerDirectProcCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee: Lifted.FnId,
        args: []const Lifted.ExprId,
        capture_operands: []const Lifted.CaptureOperand,
        is_cold: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const target_fn = try self.ensureOwnFnSpec(callee, .finite);
        const captures = self.capturesForFn(callee);
        if (captures.len == 0) {
            if (capture_operands.len != 0) Common.invariant("direct call carried capture operands for a capture-free callee");
            return try self.lowerKnownCallInto(target, result_ty, target_fn, args, null, is_cold, next);
        }
        if (captures.len != capture_operands.len) Common.invariant("direct call capture operand count differed from callee capture count");
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
        args: []const Lifted.ExprId,
        capture_arg: ?LIR.LocalId,
        is_cold: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const arg_tys = try self.lowerFnSpecArgTypes(callee);
        defer self.allocator.free(arg_tys);

        const lowered = try self.lowerExprsToTempsAtTypes(args, arg_tys);
        defer self.allocator.free(lowered.ids);

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

        if (capture_arg == null and !is_cold) {
            if (try self.inlineBodyForKnownCall(callee)) |body_expr| {
                var current = try self.lowerInlineKnownCallInto(call_target, callee, lowered.ids, body_expr, after_call);
                current = try self.prependExprsAtTypes(lowered, arg_tys, current);
                return current;
            }
        }

        const call_args = if (capture_arg) |capture_local| blk: {
            const values = try self.allocator.alloc(LIR.LocalId, lowered.ids.len + 1);
            errdefer self.allocator.free(values);
            @memcpy(values[0..lowered.ids.len], lowered.ids);
            values[lowered.ids.len] = capture_local;
            break :blk values;
        } else lowered.ids;
        defer if (capture_arg != null) self.allocator.free(call_args);
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
        const func = switch (self.solved.types.rootContent(solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("call argument lowering saw a non-function source type"),
        };
        return try self.lowerTypeSpan(self.solved.types.span(func.args));
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

        const source_fn = self.solved.lifted.fns.items[@intFromEnum(spec.source)];
        const func = switch (self.solved.types.rootContent(spec.solved_fn_ty)) {
            .func => |func| func,
            else => Common.invariant("direct Lambda Mono function table contains a non-function type"),
        };
        const solved_args = self.solved.types.span(func.args);
        const lifted_args = self.solved.lifted.typedLocalSpan(source_fn.args);
        if (solved_args.len != lifted_args.len) Common.invariant("direct Lambda Mono function arity changed after Lambda Solved");
        if (arg_locals.len != lifted_args.len) Common.invariant("inline call argument count differed from function arity");

        const saved = try self.allocator.alloc(?LIR.LocalId, lifted_args.len);
        defer self.allocator.free(saved);
        for (lifted_args, 0..) |arg, i| {
            saved[i] = self.local_map[@intFromEnum(arg.local)];
        }
        for (lifted_args, 0..) |arg, i| {
            self.local_map[@intFromEnum(arg.local)] = arg_locals[i];
        }
        defer {
            for (lifted_args, 0..) |arg, i| {
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
        arg_exprs: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const callee_ty = try self.lowerExprContextTy(callee_expr);
        return switch (self.types.get(callee_ty)) {
            .callable => |variants| try self.lowerCallableValueCallInto(target, result_ty, callee_expr, callee_ty, variants, arg_exprs, next),
            .erased_fn => try self.lowerErasedValueCallInto(target, result_ty, callee_expr, callee_ty, arg_exprs, next),
            else => Common.invariant("value call callee had no callable direct Lambda Mono representation"),
        };
    }

    fn lowerErasedValueCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee_expr: Lifted.ExprId,
        callee_ty: Type.TypeId,
        arg_exprs: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const arg_tys = try self.lowerCallableArgTypes(callee_ty);
        defer self.allocator.free(arg_tys);

        const args = try self.lowerExprsToTempsAtTypes(arg_exprs, arg_tys);
        defer self.allocator.free(args.ids);
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
        var current = try self.result.store.addCFStmt(.{ .assign_call_erased = .{
            .target = call_target,
            .closure = callee,
            .args = try self.result.store.addLocalSpan(args.ids),
            .next = after_call,
        } });
        current = try self.prependExprsAtTypes(args, arg_tys, current);
        return try self.lowerExprIntoAtType(callee, callee_expr, callee_ty, current);
    }

    fn lowerCallableValueCallInto(
        self: *Lowerer,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        callee_expr: Lifted.ExprId,
        callee_ty: Type.TypeId,
        variants_span: Type.Span,
        arg_exprs: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        const callee = try self.addTemp(callee_ty);
        const done = self.freshJoinPointId();
        // Branch lowering can lower argument types that append more variants to
        // self.types, so keep this iteration independent of the store backing.
        const variants = try self.allocator.dupe(Type.FnVariant, self.types.fnVariantSpan(variants_span));
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
        arg_exprs: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (variant.capture_ty) |_| {
            const payload = try self.addLocalForLayout(self.tagUnionPayloadLayout(self.result.store.getLocal(callee).layout_idx, variant_index));
            const call = try self.lowerKnownCallInto(target, result_ty, variant.target, arg_exprs, payload, false, next);
            if (self.isZstLocal(payload)) return call;
            return try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = payload,
                .op = .{ .tag_payload_struct = .{
                    .source = callee,
                    .variant_index = variant_index,
                    .tag_discriminant = variant_index,
                } },
                .next = call,
            } });
        }
        return try self.lowerKnownCallInto(target, result_ty, variant.target, arg_exprs, null, false, next);
    }

    fn lowerLowLevelInto(self: *Lowerer, target: LIR.LocalId, op: can.CIR.Expr.LowLevel, span: Lifted.Span(Lifted.ExprId), next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const args = self.solved.lifted.exprSpan(span);
        switch (op) {
            .box_box,
            .box_unbox,
            => return try self.lowerBoxBoundaryLowLevelInto(target, op, args, next),
            .list_map_can_reuse => {
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
                defer self.allocator.free(lowered.ids);
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
            },
            else => {},
        }
        if (lowLevelNeedsSignedIntegerLowestCheck(op)) {
            return try self.lowerSignedIntegerLowestCheckedUnaryLowLevelInto(target, op, args, next);
        }
        if (lowLevelNeedsIntegerMultiplicationOverflowCheck(op)) {
            return try self.lowerIntegerOverflowCheckedMultiplicationLowLevelInto(target, op, args, next);
        }
        if (lowLevelNeedsIntegerZeroDenominatorCheck(op)) {
            return try self.lowerIntegerZeroDenominatorCheckedLowLevelInto(target, op, args, next);
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);
        var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(lowered.ids),
            .next = next,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    /// Compile-time half of the `list_map_can_reuse` decision, computed for
    /// both pointer widths. A width's bit is true when the input and output
    /// element layouts of a `List.map` call are interchangeable in one
    /// allocation on that width — same element stride, same allocation
    /// alignment class, and same refcounted-elements header shape — in which
    /// case the runtime uniqueness check decides at that width. A false bit
    /// means reuse is statically impossible (or the optimization is disabled)
    /// on that width, so the op resolves to a constant 0 there. Both bits are
    /// stored so the lowered op is target-independent: codegen selects the bit
    /// for the width it is building.
    fn listMapLayoutsInterchangeable(self: *Lowerer, args: []const Lifted.ExprId) Common.LowerError!layout.WidthValues(bool) {
        const none = layout.WidthValues(bool).both(false, false);
        if (args.len != 2) Common.invariant("list_map_can_reuse reached LIR lowering with the wrong arity");
        if (!self.list_in_place_map) return none;

        const list_layout_idx = try self.layoutOfType(try self.lowerExprTy(args[0]));
        const list_layout = self.result.layouts.getLayout(list_layout_idx);
        if (list_layout.tag != .list) return none;
        const in_elem_idx = self.result.layouts.runtimeRepresentationLayoutIdx(list_layout.getIdx());

        const transform_ty = self.solved.expr_tys.items[@intFromEnum(args[1])];
        const transform_root = self.solved.types.root(transform_ty);
        const out_ret = switch (self.solved.types.get(transform_root)) {
            .func => |func| func.ret,
            else => Common.invariant("list_map_can_reuse transform argument is not a function"),
        };
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

    fn lowLevelNeedsIntegerZeroDenominatorCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            => true,
            else => false,
        };
    }

    fn lowLevelNeedsIntegerMultiplicationOverflowCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_times => true,
            else => false,
        };
    }

    fn lowLevelNeedsSignedIntegerLowestCheck(op: LIR.LowLevel) bool {
        return switch (op) {
            .num_negate,
            .num_abs,
            => true,
            else => false,
        };
    }

    fn integerDivisionByZeroMessage(layout_idx: layout.Idx) ?[]const u8 {
        return switch (layout_idx) {
            .u8 => "U8 division by zero",
            .i8 => "I8 division by zero",
            .u16 => "U16 division by zero",
            .i16 => "I16 division by zero",
            .u32 => "U32 division by zero",
            .i32 => "I32 division by zero",
            .u64 => "U64 division by zero",
            .i64 => "I64 division by zero",
            .u128 => "U128 division by zero",
            .i128 => "I128 division by zero",
            else => null,
        };
    }

    fn signedIntegerLowestValue(layout_idx: layout.Idx) ?i128 {
        return switch (layout_idx) {
            .i8 => std.math.minInt(i8),
            .i16 => std.math.minInt(i16),
            .i32 => std.math.minInt(i32),
            .i64 => std.math.minInt(i64),
            .i128 => std.math.minInt(i128),
            else => null,
        };
    }

    fn signedIntegerLowestOverflowMessage(op: LIR.LowLevel, layout_idx: layout.Idx) ?[]const u8 {
        if (signedIntegerLowestValue(layout_idx) == null) return null;

        return switch (op) {
            .num_negate => "signed integer negation overflowed",
            .num_abs => "signed integer absolute value overflowed",
            .num_div_by,
            .num_div_trunc_by,
            => "signed integer division overflowed",
            else => null,
        };
    }

    fn lowerIntegerOverflowCheckedMultiplicationLowLevelInto(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (args.len != 2) {
            Common.invariant("integer multiplication reached LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);

        const lhs = lowered.ids[0];
        const rhs = lowered.ids[1];
        const lhs_layout = self.result.store.getLocal(lhs).layout_idx;
        const rhs_layout = self.result.store.getLocal(rhs).layout_idx;
        if (integerDivisionByZeroMessage(lhs_layout) == null) {
            var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.result.store.addLocalSpan(lowered.ids),
                .next = next,
            } });
            current = try self.prependExprs(lowered, current);
            return current;
        }

        const zero = try self.addLocalForLayout(rhs_layout);
        const rhs_is_zero = try self.addLocalForLayout(.bool);
        const quotient = try self.addLocalForLayout(lhs_layout);
        const quotient_matches_lhs = try self.addLocalForLayout(.bool);

        const crash_stmt = try self.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.result.store.insertString("integer multiplication overflowed"),
        } });

        const quotient_match_switch = try self.boolSwitchNoContinuation(quotient_matches_lhs, next, crash_stmt);
        const quotient_eq_args = [_]LIR.LocalId{ quotient, lhs };
        const quotient_eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = quotient_matches_lhs,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.result.store.addLocalSpan(&quotient_eq_args),
            .next = quotient_match_switch,
        } });
        const div_args = [_]LIR.LocalId{ target, rhs };
        const division_check = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = quotient,
            .op = .num_div_trunc_by,
            .rc_effect = LIR.LowLevel.num_div_trunc_by.rcEffect(),
            .args = try self.result.store.addLocalSpan(&div_args),
            .next = quotient_eq_stmt,
        } });

        const nonzero_check = if (signedIntegerLowestValue(lhs_layout)) |lowest_value| blk: {
            const lowest = try self.addLocalForLayout(lhs_layout);
            const neg_one = try self.addLocalForLayout(rhs_layout);
            const lhs_is_lowest = try self.addLocalForLayout(.bool);
            const rhs_is_neg_one = try self.addLocalForLayout(.bool);

            const rhs_neg_one_switch = try self.boolSwitchNoContinuation(rhs_is_neg_one, crash_stmt, division_check);
            const rhs_eq_args = [_]LIR.LocalId{ rhs, neg_one };
            const rhs_eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = rhs_is_neg_one,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.result.store.addLocalSpan(&rhs_eq_args),
                .next = rhs_neg_one_switch,
            } });
            const assign_neg_one = try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = neg_one,
                .value = .{ .i128_literal = .{
                    .value = -1,
                    .layout_idx = rhs_layout,
                } },
                .next = rhs_eq_stmt,
            } });

            const lhs_lowest_switch = try self.boolSwitchNoContinuation(lhs_is_lowest, assign_neg_one, division_check);
            const lhs_eq_args = [_]LIR.LocalId{ lhs, lowest };
            const lhs_eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = lhs_is_lowest,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.result.store.addLocalSpan(&lhs_eq_args),
                .next = lhs_lowest_switch,
            } });
            break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = lowest,
                .value = .{ .i128_literal = .{
                    .value = lowest_value,
                    .layout_idx = lhs_layout,
                } },
                .next = lhs_eq_stmt,
            } });
        } else division_check;

        const zero_switch = try self.boolSwitchNoContinuation(rhs_is_zero, next, nonzero_check);
        const zero_eq_args = [_]LIR.LocalId{ rhs, zero };
        const zero_eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = rhs_is_zero,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.result.store.addLocalSpan(&zero_eq_args),
            .next = zero_switch,
        } });
        const assign_zero = try self.result.store.addCFStmt(.{ .assign_literal = .{
            .target = zero,
            .value = .{ .i128_literal = .{
                .value = 0,
                .layout_idx = rhs_layout,
            } },
            .next = zero_eq_stmt,
        } });

        const raw_args = [_]LIR.LocalId{ lhs, rhs };
        var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&raw_args),
            .next = assign_zero,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    fn lowerSignedIntegerLowestCheckedUnaryLowLevelInto(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (args.len != 1) {
            Common.invariant("signed integer unary operation reached LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);

        const source = lowered.ids[0];
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const lowest_value = signedIntegerLowestValue(source_layout) orelse {
            var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.result.store.addLocalSpan(lowered.ids),
                .next = next,
            } });
            current = try self.prependExprs(lowered, current);
            return current;
        };
        const crash_message = signedIntegerLowestOverflowMessage(op, source_layout) orelse {
            Common.invariant("signed integer unary operation did not have an overflow message");
        };

        const lowest = try self.addLocalForLayout(source_layout);
        const is_lowest = try self.addLocalForLayout(.bool);

        const raw_args = [_]LIR.LocalId{source};
        const raw_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&raw_args),
            .next = next,
        } });

        const crash_stmt = try self.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.result.store.insertString(crash_message),
        } });

        const switch_stmt = try self.boolSwitchNoContinuation(is_lowest, crash_stmt, raw_stmt);

        const eq_args = [_]LIR.LocalId{ source, lowest };
        const eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = is_lowest,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.result.store.addLocalSpan(&eq_args),
            .next = switch_stmt,
        } });

        var current = try self.result.store.addCFStmt(.{ .assign_literal = .{
            .target = lowest,
            .value = .{ .i128_literal = .{
                .value = lowest_value,
                .layout_idx = source_layout,
            } },
            .next = eq_stmt,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    fn lowerIntegerZeroDenominatorCheckedLowLevelInto(
        self: *Lowerer,
        target: LIR.LocalId,
        op: LIR.LowLevel,
        args: []const Lifted.ExprId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (args.len != 2) {
            Common.invariant("integer division/remainder reached LIR lowering with the wrong arity");
        }

        const lowered = try self.lowerExprsToTemps(args);
        defer self.allocator.free(lowered.ids);

        const lhs = lowered.ids[0];
        const rhs = lowered.ids[1];
        const rhs_layout = self.result.store.getLocal(rhs).layout_idx;
        const lhs_layout = self.result.store.getLocal(lhs).layout_idx;
        const crash_message = integerDivisionByZeroMessage(rhs_layout) orelse {
            var current = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.result.store.addLocalSpan(lowered.ids),
                .next = next,
            } });
            current = try self.prependExprs(lowered, current);
            return current;
        };

        const zero = try self.addLocalForLayout(rhs_layout);
        const is_zero = try self.addLocalForLayout(.bool);

        const div_args = [_]LIR.LocalId{ lhs, rhs };
        const div_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.result.store.addLocalSpan(&div_args),
            .next = next,
        } });

        const normal_stmt = if (signedIntegerLowestValue(lhs_layout)) |lowest_value| blk: {
            const overflow_body = switch (op) {
                .num_div_by,
                .num_div_trunc_by,
                => try self.result.store.addCFStmt(.{ .crash = .{
                    .msg = try self.result.store.insertString(signedIntegerLowestOverflowMessage(op, lhs_layout) orelse {
                        Common.invariant("signed integer division operation did not have an overflow message");
                    }),
                } }),
                .num_rem_by,
                .num_mod_by,
                => try self.result.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .i128_literal = .{
                        .value = 0,
                        .layout_idx = lhs_layout,
                    } },
                    .next = next,
                } }),
                else => div_stmt,
            };

            const lowest = try self.addLocalForLayout(lhs_layout);
            const neg_one = try self.addLocalForLayout(rhs_layout);
            const lhs_is_lowest = try self.addLocalForLayout(.bool);
            const rhs_is_neg_one = try self.addLocalForLayout(.bool);

            const rhs_neg_one_switch = try self.boolSwitchNoContinuation(rhs_is_neg_one, overflow_body, div_stmt);
            const rhs_eq_args = [_]LIR.LocalId{ rhs, neg_one };
            const rhs_eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = rhs_is_neg_one,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.result.store.addLocalSpan(&rhs_eq_args),
                .next = rhs_neg_one_switch,
            } });
            const assign_neg_one = try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = neg_one,
                .value = .{ .i128_literal = .{
                    .value = -1,
                    .layout_idx = rhs_layout,
                } },
                .next = rhs_eq_stmt,
            } });

            const lhs_lowest_switch = try self.boolSwitchNoContinuation(lhs_is_lowest, assign_neg_one, div_stmt);
            const lhs_eq_args = [_]LIR.LocalId{ lhs, lowest };
            const lhs_eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
                .target = lhs_is_lowest,
                .op = .num_is_eq,
                .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                .args = try self.result.store.addLocalSpan(&lhs_eq_args),
                .next = lhs_lowest_switch,
            } });
            break :blk try self.result.store.addCFStmt(.{ .assign_literal = .{
                .target = lowest,
                .value = .{ .i128_literal = .{
                    .value = lowest_value,
                    .layout_idx = lhs_layout,
                } },
                .next = lhs_eq_stmt,
            } });
        } else div_stmt;

        const crash_stmt = try self.result.store.addCFStmt(.{ .crash = .{
            .msg = try self.result.store.insertString(crash_message),
        } });

        const switch_stmt = try self.boolSwitchNoContinuation(is_zero, crash_stmt, normal_stmt);

        const eq_args = [_]LIR.LocalId{ rhs, zero };
        const eq_stmt = try self.result.store.addCFStmt(.{ .assign_low_level = .{
            .target = is_zero,
            .op = .num_is_eq,
            .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
            .args = try self.result.store.addLocalSpan(&eq_args),
            .next = switch_stmt,
        } });

        var current = try self.result.store.addCFStmt(.{ .assign_literal = .{
            .target = zero,
            .value = .{ .i128_literal = .{
                .value = 0,
                .layout_idx = rhs_layout,
            } },
            .next = eq_stmt,
        } });
        current = try self.prependExprs(lowered, current);
        return current;
    }

    fn lowerBoxBoundaryLowLevelInto(self: *Lowerer, target: LIR.LocalId, op: can.CIR.Expr.LowLevel, args: []const Lifted.ExprId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        if (args.len != 1) Common.invariant("Box low-level operation reached LIR lowering with the wrong arity");
        const source = try self.addTemp(try self.lowerExprTy(args[0]));
        const source_layout = self.result.store.getLocal(source).layout_idx;
        const assign = switch (op) {
            .box_box,
            .box_unbox,
            => try self.assignBoxBoundary(target, source, source_layout, next),
            else => unreachable,
        };
        return try self.lowerExprInto(source, args[0], assign);
    }

    fn lowerFieldAccessInto(self: *Lowerer, target: LIR.LocalId, receiver: Lifted.ExprId, field_name: Type.names.RecordFieldNameId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const receiver_ty = try self.lowerExprContextTy(receiver);
        const receiver_local = try self.addTemp(receiver_ty);
        const target_field_index = self.recordFieldIndex(receiver_ty, field_name);
        const target_field_ty = self.recordFields(receiver_ty)[@as(usize, @intCast(target_field_index))].ty;
        const source_ty = self.storageTypeOfLocalOr(receiver_local, receiver_ty);
        const source_field_index = self.recordFieldIndex(source_ty, field_name);
        const source_field_ty = self.recordFields(source_ty)[@as(usize, @intCast(source_field_index))].ty;
        if (self.isZstLocal(target)) {
            return try self.lowerExprIntoAtType(receiver_local, receiver, receiver_ty, try self.assignZst(target, next));
        }
        const assign = try self.assignTypedRefRead(
            target,
            target_field_ty,
            source_field_ty,
            self.localFieldLayout(receiver_local, source_field_index),
            .{ .field = .{ .source = receiver_local, .field_idx = source_field_index } },
            next,
        );
        return try self.lowerExprIntoAtType(receiver_local, receiver, receiver_ty, assign);
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
            target_item_tys[elem_index],
            source_item_tys[elem_index],
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
        const scrutinee_ty = try self.lowerExprContextTy(scrutinee);
        const scrutinee_local = try self.addTemp(scrutinee_ty);
        const branches = self.solved.lifted.branchSpan(branches_span);
        const done = self.freshJoinPointId();
        const branch_chain = try self.lowerBranchChain(scrutinee_local, scrutinee_ty, branches, target, result_ty, done, comptime_site);
        const remainder = try self.lowerExprIntoAtType(scrutinee_local, scrutinee, scrutinee_ty, branch_chain);
        return try self.result.store.addCFStmt(.{ .join = .{
            .id = done,
            .params = try self.result.store.addLocalSpan(&[_]LIR.LocalId{target}),
            .body = next,
            .remainder = remainder,
        } });
    }

    /// When the in-place `List.map` branch is statically impossible — the
    /// optimization is disabled or the element layouts are not
    /// interchangeable — lower only the branch a constant-0
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
        const branches = self.solved.lifted.ifBranchSpan(branches_span);
        const done = self.freshJoinPointId();
        var current = try self.lowerExprIntoAtType(target, final_else, result_ty, try self.joinJump(done));
        var i = branches.len;
        while (i > 0) {
            i -= 1;
            const body = try self.lowerExprIntoAtType(target, branches[i].body, result_ty, try self.joinJump(done));
            const cond_local = try self.addTemp(try self.lowerExprTy(branches[i].cond));
            const switch_stmt = try self.boolSwitchNoContinuation(cond_local, body, current);
            current = try self.lowerExprInto(cond_local, branches[i].cond, switch_stmt);
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
        const cond_data = self.solved.lifted.exprs.items[@intFromEnum(payload_switch.cond)];
        const cond_local = switch (cond_data.data) {
            .local => |local| try self.localFor(local),
            else => try self.addTemp(try self.lowerExprTy(payload_switch.cond)),
        };
        const switch_stmt = try self.result.store.addCFStmt(.{ .switch_initialized_payload = .{
            .cond = cond_local,
            .cond_mask = payload_switch.cond_mask,
            .payload = try self.localFor(payload_switch.payload),
            .uninitialized_is_cold = payload_switch.uninitialized_is_cold,
            .initialized_branch = initialized_body,
            .uninitialized_branch = uninitialized_body,
        } });
        const remainder = switch (cond_data.data) {
            .local => switch_stmt,
            else => try self.lowerExprInto(cond_local, payload_switch.cond, switch_stmt),
        };
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
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = ok_payload_local,
                .op = .{ .tag_payload_struct = .{
                    .source = input_try_local,
                    .variant_index = ok_variant,
                    .tag_discriminant = ok_variant,
                } },
                .next = ok_body,
            } });

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
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = err_payload_local,
                .op = .{ .tag_payload_struct = .{
                    .source = input_try_local,
                    .variant_index = err_variant,
                    .tag_discriminant = err_variant,
                } },
                .next = err_tag,
            } });

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
            try self.result.store.addCFStmt(.{ .assign_ref = .{
                .target = err_payload_local,
                .op = .{ .tag_payload_struct = .{
                    .source = input_try_local,
                    .variant_index = err_variant,
                    .tag_discriminant = err_variant,
                } },
                .next = err_tag,
            } });

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
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .tag_payload = .{
                .source = input_try_local,
                .payload_idx = field_index,
                .variant_index = ok_variant,
                .tag_discriminant = ok_variant,
            } },
            .next = next,
        } });
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
        var current = try self.lowerExprIntoAtType(target, final_expr, result_ty, next);
        var i = stmts.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerStmt(stmts[i], current);
        }
        return current;
    }

    fn lowerStmt(self: *Lowerer, stmt_id: Lifted.StmtId, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const saved_loc = self.result.store.current_loc;
        defer self.result.store.current_loc = saved_loc;
        const saved_region = self.result.store.current_region;
        defer self.result.store.current_region = saved_region;
        const stmt_loc = self.solved.lifted.stmtLoc(stmt_id);
        if (stmt_loc.hasLocation()) {
            self.result.store.current_loc = stmt_loc;
        }
        const stmt_region = self.solved.lifted.stmtRegion(stmt_id);
        if (!stmt_region.isEmpty()) {
            self.result.store.current_region = stmt_region;
        }
        return switch (self.solved.lifted.stmts.items[@intFromEnum(stmt_id)]) {
            .uninitialized => |pat_id| try self.initUninitializedPattern(pat_id, next),
            .let_ => |let_| blk: {
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
                .msg = try self.result.store.insertString(self.stringLiteralText(msg)),
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
                    if (steps[i].capture) |capture| {
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
                    current = try self.initUninitializedPattern(destructs[i].pattern, current);
                }
                break :blk current;
            },
            .tuple => |items| blk: {
                var current = next;
                const pats = self.solved.lifted.patSpan(items);
                var i = pats.len;
                while (i > 0) {
                    i -= 1;
                    current = try self.initUninitializedPattern(pats[i], current);
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
                    current = try self.initUninitializedPattern(pats[i], current);
                }
                break :blk current;
            },
            .tag => |tag| blk: {
                var current = next;
                const payloads = self.solved.lifted.patSpan(tag.payloads);
                var i = payloads.len;
                while (i > 0) {
                    i -= 1;
                    current = try self.initUninitializedPattern(payloads[i], current);
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

    fn lowerLoopInto(self: *Lowerer, target: LIR.LocalId, loop: anytype, next: LIR.CFStmtId) Common.LowerError!LIR.CFStmtId {
        const params = self.solved.lifted.typedLocalSpan(loop.params);
        const initial_values = self.solved.lifted.exprSpan(loop.initial_values);
        if (params.len != initial_values.len) Common.invariant("Lambda Mono loop parameter count differs from initial value count");

        const param_locals = try self.allocator.alloc(LIR.LocalId, params.len);
        defer self.allocator.free(param_locals);
        const param_tys = try self.allocator.alloc(Type.TypeId, params.len);
        defer self.allocator.free(param_tys);
        for (params, 0..) |param, i| {
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
        for (initial_values, param_locals) |initial, param_local| {
            switch (self.solved.lifted.exprs.items[@intFromEnum(initial)].data) {
                .uninitialized_payload => |payload| {
                    try maybe_uninitialized_params.append(self.allocator, param_local);
                    try maybe_uninitialized_conditions.append(self.allocator, try self.localFor(payload.condition));
                    try maybe_uninitialized_condition_masks.append(self.allocator, payload.mask);
                },
                else => {},
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
            .after_loop = next,
        });
        defer _ = self.loop_stack.pop();

        const body = try self.lowerExprInto(target, loop.body, next);
        const jump_args = try self.lowerExprsToJoinTempsAtTypes(param_span, param_tys, initial_values);
        defer self.allocator.free(jump_args.ids);
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
            return try self.lowerExprInto(loop.result_target, expr_id, loop.after_loop);
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
        defer self.allocator.free(locals.ids);
        var jump = try self.result.store.addCFStmt(.{ .jump = .{
            .target = loop.join_id,
        } });
        jump = try self.prependJoinParamInitializers(loop.params, locals.ids, jump);
        jump = try self.prependJoinExprsAtTypes(locals, loop.param_tys, jump);
        return jump;
    }

    fn lowerReturn(self: *Lowerer, ret: Mono.Return) Common.LowerError!LIR.CFStmtId {
        const ret_ty = self.current_ret_ty orelse Common.invariant("return expression reached LIR lowering outside a function");
        const ret_local = try self.addTemp(ret_ty);
        const ret_stmt = try self.result.store.addCFStmt(.{ .ret = .{ .value = ret_local } });
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

    fn lowerBranchChain(
        self: *Lowerer,
        scrutinee: LIR.LocalId,
        scrutinee_ty: Type.TypeId,
        branches: []const Lifted.Branch,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        done: LIR.JoinPointId,
        comptime_site: ?Lifted.ComptimeSiteId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = if (comptime_site) |site|
            try self.result.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{ .site = try self.lowerComptimeSite(site) } })
        else
            try self.result.store.addCFStmt(.{ .runtime_error = {} });
        var i = branches.len;
        while (i > 0) {
            const group_end = i;
            const group_start = self.strPatternBranchGroupStart(branches, group_end);
            if (group_end - group_start >= 2) {
                const next_branch = current;
                const miss = PatternMiss{ .join_id = self.freshJoinPointId() };
                const branch_start = try self.lowerStrPatternBranchGroup(branches[group_start..group_end], scrutinee, target, result_ty, done, miss);
                current = try self.result.store.addCFStmt(.{ .join = .{
                    .id = miss.join_id,
                    .params = LIR.LocalSpan.empty(),
                    .body = next_branch,
                    .remainder = branch_start,
                } });
                i = group_start;
                continue;
            }

            i -= 1;
            const next_branch = current;
            const needs_miss_join = branches[i].guard != null or self.patternCanMiss(branches[i].pat);
            const miss = if (needs_miss_join)
                PatternMiss{ .join_id = self.freshJoinPointId() }
            else
                null;
            const branch_done = try self.joinJump(done);
            const branch_body = try self.lowerExprIntoAtType(target, branches[i].body, result_ty, branch_done);
            const guarded = if (branches[i].guard) |guard| blk: {
                const guard_local = try self.addTemp(try self.lowerExprTy(guard));
                const guard_switch = try self.boolSwitchNoContinuation(guard_local, branch_body, try self.patternMissJump(miss));
                break :blk try self.lowerExprInto(guard_local, guard, guard_switch);
            } else branch_body;
            const branch_start = try self.lowerPatternThenAtType(branches[i].pat, scrutinee_ty, scrutinee, guarded, miss, branch_done);
            current = if (miss) |miss_info|
                try self.result.store.addCFStmt(.{ .join = .{
                    .id = miss_info.join_id,
                    .params = LIR.LocalSpan.empty(),
                    .body = next_branch,
                    .remainder = branch_start,
                } })
            else
                branch_start;
        }
        return current;
    }

    fn strPatternBranchGroupStart(self: *Lowerer, branches: []const Lifted.Branch, end: usize) usize {
        var start = end;
        while (start > 0 and self.directStrPattern(branches[start - 1].pat) != null) {
            start -= 1;
        }
        return start;
    }

    fn directStrPattern(self: *Lowerer, pat_id: Lifted.PatId) ?Lifted.StrPattern {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .str_pattern => |str| str,
            else => null,
        };
    }

    fn lowerStrPatternBranchGroup(
        self: *Lowerer,
        branches: []const Lifted.Branch,
        source: LIR.LocalId,
        target: LIR.LocalId,
        result_ty: Type.TypeId,
        done: LIR.JoinPointId,
        miss: PatternMiss,
    ) Common.LowerError!LIR.CFStmtId {
        const arms = try self.allocator.alloc(LIR.StrMatchArm, branches.len);
        defer self.allocator.free(arms);

        for (branches, arms) |branch, *arm| {
            const str = self.directStrPattern(branch.pat) orelse Common.invariant("string-pattern branch group contained a non-string-pattern branch");
            const branch_done = try self.joinJump(done);
            const branch_body = try self.lowerExprIntoAtType(target, branch.body, result_ty, branch_done);
            const guarded = if (branch.guard) |guard| blk: {
                const guard_local = try self.addTemp(try self.lowerExprTy(guard));
                const guard_switch = try self.boolSwitchNoContinuation(guard_local, branch_body, try self.patternMissJump(miss));
                break :blk try self.lowerExprInto(guard_local, guard, guard_switch);
            } else branch_body;
            arm.* = try self.lowerStrPatternArm(str, guarded);
        }

        return try self.result.store.addCFStmt(.{ .str_match_set = .{
            .source = source,
            .arms = try self.result.store.addStrMatchArms(arms),
            .on_miss = try self.patternMissJump(miss),
        } });
    }

    fn joinJump(self: *Lowerer, join_id: LIR.JoinPointId) Common.LowerError!LIR.CFStmtId {
        return try self.result.store.addCFStmt(.{ .jump = .{ .target = join_id } });
    }

    fn patternMissJump(self: *Lowerer, miss: ?PatternMiss) Common.LowerError!LIR.CFStmtId {
        const miss_info = miss orelse Common.invariant("refutable pattern lowering had no miss target");
        return try self.result.store.addCFStmt(.{ .jump = .{ .target = miss_info.join_id } });
    }

    fn patternCanMiss(self: *Lowerer, pat_id: Lifted.PatId) bool {
        const pat_data = self.pat(pat_id);
        return switch (pat_data.data) {
            .bind, .wildcard => false,
            .as => |as| self.patternCanMiss(as.pattern),
            .record => |fields| blk: {
                for (self.solved.lifted.recordDestructSpan(fields)) |field| {
                    if (self.patternCanMiss(field.pattern)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |items| blk: {
                for (self.solved.lifted.patSpan(items)) |item| {
                    if (self.patternCanMiss(item)) break :blk true;
                }
                break :blk false;
            },
            // A list pattern can fail unless it imposes no length constraint:
            // only `[.. as rest]` / `[..]` (a rest with no fixed elements, which
            // matches every list) is irrefutable.
            .list => |list| list.patterns.len > 0 or list.rest == null,
            .nominal => |inner| self.patternCanMiss(inner),
            .tag, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => true,
        };
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
            const field_index = self.recordFieldIndex(source_ty, destructs[i].name);
            const field_ty = self.recordFields(source_ty)[@as(usize, @intCast(field_index))].ty;
            const field_local = try self.addTemp(field_ty);
            current = try self.lowerPatternThenAtType(destructs[i].pattern, field_ty, field_local, current, miss, continuation);
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
            const item_local = try self.addTemp(item_tys[i]);
            current = try self.lowerPatternThenAtType(items[i], item_tys[i], item_local, current, miss, continuation);
            if (!self.isZstLocal(item_local)) {
                const field_index: u16 = @intCast(i);
                current = try self.assignTypedRefRead(
                    item_local,
                    item_tys[i],
                    item_tys[i],
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
            current = try self.lowerPatternThenAtType(elems[i], elem_ty, elem_local, current, miss, continuation);
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
        return switch (self.types.get(nominal_ty)) {
            .named => |named| (named.backing orelse Common.invariant("nominal pattern target had no runtime backing")).ty,
            .box => |elem| elem,
            else => try self.lowerPatTy(inner),
        };
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

        for (input_steps, lir_steps) |input_step, *lir_step| {
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
            if (input_steps[index].capture) |capture| {
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
            const field_index = self.recordFieldIndex(source_ty, destructs[i].name);
            const field_ty = self.recordFields(source_ty)[@as(usize, @intCast(field_index))].ty;
            const field_local = try self.addTemp(field_ty);
            current = try self.bindPatternAtType(destructs[i].pattern, field_ty, field_local, current);
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
            const payload_local = try self.addTemp(payload_tys[i]);
            current = try self.lowerPatternThenAtType(payloads[i], payload_tys[i], payload_local, current, miss, continuation);
            if (!self.isZstLocal(payload_local)) {
                if (payloads.len == 1) {
                    current = try self.assignTypedRefRead(
                        payload_local,
                        payload_tys[i],
                        payload_tys[i],
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
                    payload_tys[i],
                    payload_tys[i],
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
            const item_local = try self.addTemp(item_tys[i]);
            current = try self.bindPatternAtType(items[i], item_tys[i], item_local, current);
            if (!self.isZstLocal(item_local)) {
                const field_index: u16 = @intCast(i);
                current = try self.assignTypedRefRead(
                    item_local,
                    item_tys[i],
                    item_tys[i],
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
            const payload_local = try self.addTemp(payload_tys[i]);
            current = try self.bindPatternAtType(payloads[i], payload_tys[i], payload_local, current);
            if (!self.isZstLocal(payload_local)) {
                if (payloads.len == 1) {
                    current = try self.assignTypedRefRead(
                        payload_local,
                        payload_tys[i],
                        payload_tys[i],
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
                    payload_tys[i],
                    payload_tys[i],
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
        const eq_op: LIR.LowLevel = switch (primitive) {
            .str => .str_is_eq,
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => .num_is_eq,
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
        fields: []const Type.Field,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = fields.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, fields[i].ty, @intCast(i), current, failed);
        }
        return current;
    }

    fn lowerCaptureRecordEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        fields: []const Type.CaptureField,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = fields.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, fields[i].ty, @intCast(i), current, failed);
        }
        return current;
    }

    fn lowerTupleEqLocalsInto(
        self: *Lowerer,
        target: LIR.LocalId,
        lhs: LIR.LocalId,
        rhs: LIR.LocalId,
        items: []const Type.TypeId,
        negated: bool,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        var current = try self.assignBool(target, !negated, next);
        const failed = try self.assignBool(target, negated, next);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            current = try self.lowerFieldEqStep(lhs, rhs, items[i], @intCast(i), current, failed);
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
        tags: []const Type.Tag,
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
        for (tags, 0..) |tag, index| {
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
            const payload_ty = payloads[i];
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
        exprs: []const Lifted.ExprId,
        ids: []LIR.LocalId,
    };

    const LoweredJoinExprLocals = struct {
        exprs: []const Lifted.ExprId,
        ids: []?LIR.LocalId,
    };

    fn lowerExprsToTemps(self: *Lowerer, exprs: []const Lifted.ExprId) Common.LowerError!LoweredExprLocals {
        const ids = try self.allocator.alloc(LIR.LocalId, exprs.len);
        errdefer self.allocator.free(ids);
        for (exprs, 0..) |expr_id, i| ids[i] = try self.addTemp(try self.lowerExprTy(expr_id));
        return .{ .exprs = exprs, .ids = ids };
    }

    fn lowerExprsToTempsAtTypes(
        self: *Lowerer,
        exprs: []const Lifted.ExprId,
        tys: []const Type.TypeId,
    ) Common.LowerError!LoweredExprLocals {
        if (exprs.len != tys.len) Common.invariant("typed expression temp arity differed from expression arity");
        const ids = try self.allocator.alloc(LIR.LocalId, exprs.len);
        errdefer self.allocator.free(ids);
        for (tys, 0..) |ty, i| ids[i] = try self.addTemp(ty);
        return .{ .exprs = exprs, .ids = ids };
    }

    fn lowerExprsToJoinTempsAtTypes(
        self: *Lowerer,
        params: LIR.LocalSpan,
        param_tys: []const Type.TypeId,
        exprs: []const Lifted.ExprId,
    ) Common.LowerError!LoweredJoinExprLocals {
        const param_locals = self.result.store.getLocalSpan(params);
        if (param_locals.len != exprs.len or param_tys.len != exprs.len) {
            Common.invariant("typed LIR join arg arity differed from parameter arity");
        }

        const ids = try self.allocator.alloc(?LIR.LocalId, exprs.len);
        errdefer self.allocator.free(ids);
        for (exprs, param_tys, 0..) |expr_id, param_ty, i| {
            ids[i] = if (try self.joinArgNeedsWriteAtType(param_locals[i], param_ty, expr_id))
                try self.addTemp(param_ty)
            else
                null;
        }
        return .{ .exprs = exprs, .ids = ids };
    }

    fn joinArgNeedsWriteAtType(self: *Lowerer, param: LIR.LocalId, ty: Type.TypeId, expr_id: Lifted.ExprId) Common.LowerError!bool {
        return switch (self.solved.lifted.exprs.items[@intFromEnum(expr_id)].data) {
            .uninitialized, .uninitialized_payload => false,
            .local => |local| (self.existingLocalForTyped(local, ty) orelse return true) != param,
            else => true,
        };
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
                .target = param_locals[i],
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
        const left = self.solved.lifted.locals.items[@intFromEnum(left_id)];
        const right = self.solved.lifted.locals.items[@intFromEnum(right_id)];
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
        if (try self.maybeAssignDirectLayoutBoundary(target, source, next)) |stmt| return stmt;

        const target_runtime_ty = self.runtimeBackingType(target_ty);
        const source_runtime_ty = self.runtimeBackingType(source_ty);
        if (target_runtime_ty != target_ty or source_runtime_ty != source_ty) {
            return try self.assignTypedBoundary(target, target_runtime_ty, source, source_runtime_ty, next);
        }

        const target_content = self.types.get(target_ty);
        const source_content = self.types.get(source_ty);
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
        const target_named = switch (self.types.get(target_ty)) {
            .named => |named| named,
            else => return null,
        };
        const source_named = switch (self.types.get(source_ty)) {
            .named => |named| named,
            else => return null,
        };

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
        for (source_variants, 0..) |source_variant, source_index| {
            const target_index = Lowerer.callableVariantIndexBySource(target_variants, source_variant.source);
            branches[source_index] = .{
                .value = @intCast(source_index),
                .body = try self.assignCallableVariantBoundary(
                    target,
                    target_variants[target_index],
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
        for (target_fields, 0..) |_, i| {
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
            const source_index = Lowerer.captureFieldIndexInFields(source_fields, target_fields[i]);
            current = try self.assignStructFieldBoundary(
                target_locals,
                @intCast(i),
                target_fields[i].ty,
                source,
                @intCast(source_index),
                source_fields[source_index].ty,
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
        for (target_fields, 0..) |_, i| {
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
            const source_index = Lowerer.recordFieldIndexInFields(source_fields, target_fields[i].name);
            current = try self.assignStructFieldBoundary(
                target_locals,
                @intCast(i),
                target_fields[i].ty,
                source,
                @intCast(source_index),
                source_fields[source_index].ty,
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
        for (target_items, 0..) |_, i| {
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
                target_items[i],
                source,
                @intCast(i),
                source_items[i],
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
            const target_index = Lowerer.tagIndexInTags(target_tags, source_tags[0]);
            return try self.assignTagUnionVariantBoundary(
                target,
                target_tags[target_index],
                @intCast(target_index),
                source,
                source_tags[0],
                0,
                next,
            );
        }

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, source_tags.len);
        defer self.allocator.free(branches);
        for (source_tags, 0..) |source_tag, source_index| {
            const target_index = Lowerer.tagIndexInTags(target_tags, source_tag);
            branches[source_index] = .{
                .value = @intCast(source_index),
                .body = try self.assignTagUnionVariantBoundary(
                    target,
                    target_tags[target_index],
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
        target_tys: []const Type.TypeId,
        source_payload: LIR.LocalId,
        source_tys: []const Type.TypeId,
        next: LIR.CFStmtId,
    ) Common.LowerError!LIR.CFStmtId {
        if (target_tys.len != source_tys.len) Common.invariant("tag payload boundary saw different arities");
        if (target_tys.len == 1) {
            return try self.assignTypedValueIntoStorage(target_payload, target_tys[0], source_payload, source_tys[0], next);
        }

        const target_fields = try self.allocator.alloc(LIR.LocalId, target_tys.len);
        defer self.allocator.free(target_fields);
        for (target_tys, 0..) |_, i| {
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

        var i = target_tys.len;
        while (i > 0) {
            i -= 1;
            const source_layout = if (self.isZstLocal(source_payload))
                layout.Idx.zst
            else
                self.localFieldLayout(source_payload, @intCast(i));
            const source_field = try self.addLocalForLayout(source_layout);
            current = try self.assignTypedValueIntoStorage(target_fields[i], target_tys[i], source_field, source_tys[i], current);
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

    fn callableVariantIndexBySource(variants: []const Type.FnVariant, source: Common.Symbol) usize {
        for (variants, 0..) |variant, index| {
            if (variant.source == source) return index;
        }
        Common.invariant("callable boundary target type did not contain source variant");
    }

    fn tagIndexInTags(tags: []const Type.Tag, needle: Type.Tag) usize {
        for (tags, 0..) |tag, index| {
            if (tag.name == needle.name and tag.checked_name == needle.checked_name) return index;
        }
        Common.invariant("tag union boundary target type did not contain source tag");
    }

    fn captureFieldIndexInFields(fields: []const Type.CaptureField, needle: Type.CaptureField) usize {
        for (fields, 0..) |field, index| {
            // CaptureId is the exact identity of a capture-record field.
            if (field.capture_id == needle.capture_id) return index;
        }
        Common.invariant("capture record boundary target field was absent from source");
    }

    fn recordFieldIndexInFields(fields: []const Type.Field, name: Type.names.RecordFieldNameId) usize {
        for (fields, 0..) |field, index| {
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
        return try self.result.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .local = source },
            .next = next,
        } });
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
            if (try self.publicTypesEquivalent(ty, other_ty, &visited) and
                try self.namedBackingsEquivalentForLayoutReuse(ty, other_ty))
            {
                return .{ .ty = other_ty, .layout_idx = entry.value_ptr.* };
            }
        }
        return null;
    }

    fn namedBackingsEquivalentForLayoutReuse(self: *Lowerer, lhs_ty: Type.TypeId, rhs_ty: Type.TypeId) Common.LowerError!bool {
        const lhs = switch (self.types.get(lhs_ty)) {
            .named => |named| named,
            else => return false,
        };
        const rhs = switch (self.types.get(rhs_ty)) {
            .named => |named| named,
            else => return false,
        };
        if (lhs.backing == null or rhs.backing == null) return lhs.backing == null and rhs.backing == null;
        if (lhs.backing.?.use != rhs.backing.?.use) return false;
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        return try self.publicTypesEquivalent(lhs.backing.?.ty, rhs.backing.?.ty, &visited);
    }

    fn publicTypesEquivalent(
        self: *Lowerer,
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
            .list => |elem| try self.publicTypesEquivalent(elem, rhs.list, visited),
            .box => |elem| try self.publicTypesEquivalent(elem, rhs.box, visited),
            .tuple => |items| try self.publicTypeSpansEquivalent(items, rhs.tuple, visited),
            .record => |fields| try self.publicFieldsEquivalent(fields, rhs.record, visited),
            .capture_record => |fields| try self.publicCaptureFieldsEquivalent(fields, rhs.capture_record, visited),
            .tag_union => |tags| try self.publicTagsEquivalent(tags, rhs.tag_union, visited),
            .callable => |variants| try self.publicFnVariantsEquivalent(variants, rhs.callable, visited),
            .erased_fn => |erased| std.mem.eql(u8, erased.source_fn_ty.bytes[0..], rhs.erased_fn.source_fn_ty.bytes[0..]) and
                try self.publicFnVariantsEquivalent(erased.members, rhs.erased_fn.members, visited),
            .named => |named| try self.publicNamedTypesEquivalent(named, rhs.named, visited),
        };
    }

    fn publicNamedTypesEquivalent(
        self: *Lowerer,
        lhs: std.meta.fieldInfo(Type.Content, .named).type,
        rhs: std.meta.fieldInfo(Type.Content, .named).type,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        if (lhs.kind != rhs.kind) return false;
        if (!std.mem.eql(u8, lhs.named_type.module.bytes[0..], rhs.named_type.module.bytes[0..])) return false;
        if (!std.mem.eql(u8, self.solved.lifted.names.moduleNameText(lhs.def.module_name), self.solved.lifted.names.moduleNameText(rhs.def.module_name))) return false;
        if (lhs.def.source_decl != rhs.def.source_decl) return false;
        if (lhs.def.source_decl == null and
            !std.mem.eql(u8, self.solved.lifted.names.typeNameText(lhs.def.type_name), self.solved.lifted.names.typeNameText(rhs.def.type_name)))
        {
            return false;
        }
        if (!std.meta.eql(lhs.builtin_owner, rhs.builtin_owner)) return false;
        if (!try self.publicTypeSpansEquivalent(lhs.args, rhs.args, visited)) return false;

        if (lhs.kind == .alias) {
            const lhs_backing = lhs.backing orelse return rhs.backing == null;
            const rhs_backing = rhs.backing orelse return false;
            return try self.publicTypesEquivalent(lhs_backing.ty, rhs_backing.ty, visited);
        }

        if (lhs.builtin_owner) |owner| {
            if (generatedEvidenceOwnerUsesBacking(owner)) {
                const lhs_backing = lhs.backing orelse return rhs.backing == null;
                const rhs_backing = rhs.backing orelse return false;
                return try self.publicTypesEquivalent(lhs_backing.ty, rhs_backing.ty, visited);
            }
        }

        return true;
    }

    fn generatedEvidenceOwnerUsesBacking(owner: check.StaticDispatchRegistry.BuiltinOwner) bool {
        return switch (owner) {
            .fields,
            .parse_tag_union_spec,
            => true,
            else => false,
        };
    }

    fn publicTypeSpansEquivalent(
        self: *Lowerer,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.span(lhs_span);
        const rhs = self.types.span(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_ty, rhs_ty| {
            if (!try self.publicTypesEquivalent(lhs_ty, rhs_ty, visited)) return false;
        }
        return true;
    }

    fn publicFieldsEquivalent(
        self: *Lowerer,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.fieldSpan(lhs_span);
        const rhs = self.types.fieldSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_field, rhs_field| {
            if (lhs_field.name != rhs_field.name) return false;
            if (!try self.publicTypesEquivalent(lhs_field.ty, rhs_field.ty, visited)) return false;
        }
        return true;
    }

    fn publicCaptureFieldsEquivalent(
        self: *Lowerer,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.captureFieldSpan(lhs_span);
        const rhs = self.types.captureFieldSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_field, rhs_field| {
            if (!std.meta.eql(lhs_field.capture_id, rhs_field.capture_id)) return false;
            if (!try self.publicTypesEquivalent(lhs_field.ty, rhs_field.ty, visited)) return false;
        }
        return true;
    }

    fn publicTagsEquivalent(
        self: *Lowerer,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.tagSpan(lhs_span);
        const rhs = self.types.tagSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_tag, rhs_tag| {
            if (lhs_tag.name != rhs_tag.name) return false;
            if (lhs_tag.checked_name != rhs_tag.checked_name) return false;
            if (!try self.publicTypeSpansEquivalent(lhs_tag.payloads, rhs_tag.payloads, visited)) return false;
        }
        return true;
    }

    fn publicFnVariantsEquivalent(
        self: *Lowerer,
        lhs_span: Type.Span,
        rhs_span: Type.Span,
        visited: *std.AutoHashMap(u64, void),
    ) Common.LowerError!bool {
        const lhs = self.types.fnVariantSpan(lhs_span);
        const rhs = self.types.fnVariantSpan(rhs_span);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |lhs_variant, rhs_variant| {
            if (lhs_variant.source != rhs_variant.source) return false;
            if (lhs_variant.target != rhs_variant.target) return false;
            if (!std.meta.eql(lhs_variant.capture_ty, rhs_variant.capture_ty)) {
                if (lhs_variant.capture_ty == null or rhs_variant.capture_ty == null) return false;
                if (!try self.publicTypesEquivalent(lhs_variant.capture_ty.?, rhs_variant.capture_ty.?, visited)) return false;
            }
        }
        return true;
    }

    fn typeContainsCallable(self: *Lowerer, ty: Type.TypeId) Common.LowerError!bool {
        var visited = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer visited.deinit();
        return try self.typeContainsCallableInner(ty, &visited);
    }

    fn typeContainsCallableInner(
        self: *Lowerer,
        ty: Type.TypeId,
        visited: *std.AutoHashMap(Type.TypeId, void),
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
                for (self.types.fieldSpan(fields)) |field| {
                    if (try self.typeContainsCallableInner(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .capture_record => |fields| blk: {
                for (self.types.captureFieldSpan(fields)) |field| {
                    if (try self.typeContainsCallableInner(field.ty, visited)) break :blk true;
                }
                break :blk false;
            },
            .tag_union => |tags| blk: {
                for (self.types.tagSpan(tags)) |tag| {
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
        visited: *std.AutoHashMap(Type.TypeId, void),
    ) Common.LowerError!bool {
        for (self.types.span(span)) |ty| {
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

        const local_nodes = try self.allocator.alloc(?layout.GraphNodeId, self.types.types.items.len);
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
                else => {},
            }

            switch (self.lowerer.types.get(ty)) {
                .named => |named| if (named.backing) |backing| {
                    if (named.kind == .@"opaque" and
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
                else => {},
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
                    else => return false,
                }
            }
        }

        fn appendTupleFields(self: *LayoutGraphBuilder, items: []const Type.TypeId) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (items, 0..) |item, i| {
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendStructFields(self: *LayoutGraphBuilder, items: []const Type.Field) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (items, 0..) |item, i| {
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
            const backing_fields = switch (self.lowerer.types.get(backing_ty)) {
                .record => |span| self.lowerer.types.fieldSpan(span),
                else => return null,
            };
            const entries = self.lowerer.types.declaredFieldSpan(declared_order);

            var named_count: usize = 0;
            for (entries) |entry| {
                if (entry == .named) named_count += 1;
            }
            if (named_count != backing_fields.len) return null;

            const fields = try self.lowerer.allocator.alloc(layout.GraphField, entries.len);
            defer self.lowerer.allocator.free(fields);
            // Padding spacers carry an index past every named field so they never
            // collide with a named field's original (lexicographic) index, which
            // is what `getStructFieldOffsetByOriginalIndex` looks up.
            var padding_ordinal: u16 = 0;
            for (entries, 0..) |entry, i| {
                switch (entry) {
                    .named => |name| {
                        var lexicographic_index: ?u16 = null;
                        var field_ty: Type.TypeId = undefined;
                        for (backing_fields, 0..) |field, idx| {
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

        fn appendCaptureFields(self: *LayoutGraphBuilder, items: []const Type.CaptureField) Common.LowerError!layout.GraphFieldSpan {
            const fields = try self.lowerer.allocator.alloc(layout.GraphField, items.len);
            defer self.lowerer.allocator.free(fields);
            for (items, 0..) |item, i| {
                fields[i] = .{ .index = @intCast(i), .child = try self.inputForType(item.ty) };
            }
            return try self.graph.appendFields(self.lowerer.allocator, fields);
        }

        fn appendTagPayloadInputs(self: *LayoutGraphBuilder, tags: []const Type.Tag) Common.LowerError!layout.GraphInputSpan {
            const refs = try self.lowerer.allocator.alloc(layout.GraphInput, tags.len);
            defer self.lowerer.allocator.free(refs);
            for (tags, 0..) |tag, i| {
                refs[i] = try self.payloadInput(self.lowerer.types.span(tag.payloads));
            }
            return try self.graph.appendRefs(self.lowerer.allocator, refs);
        }

        fn appendCallablePayloadInputs(self: *LayoutGraphBuilder, variants: []const Type.FnVariant) Common.LowerError!layout.GraphInputSpan {
            const refs = try self.lowerer.allocator.alloc(layout.GraphInput, variants.len);
            defer self.lowerer.allocator.free(refs);
            for (variants, 0..) |variant, i| {
                refs[i] = if (variant.capture_ty) |capture_ty| try self.inputForType(capture_ty) else layout.committedGraphInput(.zst);
            }
            return try self.graph.appendRefs(self.lowerer.allocator, refs);
        }

        fn payloadInput(self: *LayoutGraphBuilder, payloads: []const Type.TypeId) Common.LowerError!layout.GraphInput {
            return switch (payloads.len) {
                0 => layout.committedGraphInput(.zst),
                1 => try self.inputForType(payloads[0]),
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
            => null,
        };
    }

    fn tagIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.TagNameId) u16 {
        for (self.tagUnionTags(ty), 0..) |tag, index| {
            if (tag.name == name) return @intCast(index);
        }
        Common.invariant("tag operation referenced tag absent from Lambda Mono type");
    }

    fn tagIndexByText(self: *Lowerer, ty: Type.TypeId, text: []const u8) u16 {
        for (self.tagUnionTags(ty), 0..) |tag, index| {
            if (std.mem.eql(u8, self.solved.lifted.names.tagLabelText(tag.name), text)) return @intCast(index);
        }
        Common.invariant("tag operation referenced tag text absent from Lambda Mono type");
    }

    fn tagUnionTags(self: *Lowerer, ty: Type.TypeId) []const Type.Tag {
        return switch (self.types.get(ty)) {
            .tag_union => |tags| self.types.tagSpan(tags),
            .named => |named| if (named.backing) |backing| return self.tagUnionTags(backing.ty) else Common.invariant("named tag has no backing"),
            else => Common.invariant("tag operation expected tag-union type"),
        };
    }

    fn tupleItemTypes(self: *Lowerer, ty: Type.TypeId) []const Type.TypeId {
        return switch (self.types.get(ty)) {
            .tuple => |items| self.types.span(items),
            .named => |named| if (named.backing) |backing| return self.tupleItemTypes(backing.ty) else Common.invariant("named tuple has no backing"),
            else => Common.invariant("tuple operation expected tuple type"),
        };
    }

    fn listElemType(self: *Lowerer, ty: Type.TypeId) Type.TypeId {
        return switch (self.types.get(ty)) {
            .list => |elem| elem,
            .named => |named| if (named.backing) |backing| return self.listElemType(backing.ty) else Common.invariant("named list has no backing"),
            else => Common.invariant("list operation expected list type"),
        };
    }

    fn tagPayloadTypesByIndex(self: *Lowerer, ty: Type.TypeId, variant_index: u16) []const Type.TypeId {
        const tags = self.tagUnionTags(ty);
        if (variant_index >= tags.len) Common.invariant("tag operation referenced variant outside Lambda Mono type");
        return self.types.span(tags[variant_index].payloads);
    }

    fn singleTagPayloadTypeByIndex(self: *Lowerer, ty: Type.TypeId, variant_index: u16) Type.TypeId {
        const tags = self.tagUnionTags(ty);
        const payloads = self.types.span(tags[variant_index].payloads);
        if (payloads.len != 1) Common.invariant("operation expected tag with exactly one payload");
        return payloads[0];
    }

    fn runtimeBackingType(self: *Lowerer, ty: Type.TypeId) Type.TypeId {
        return switch (self.types.get(ty)) {
            .named => |named| if (named.backing) |backing| backing.ty else ty,
            else => ty,
        };
    }

    fn recordFields(self: *Lowerer, ty: Type.TypeId) []const Type.Field {
        return switch (self.types.get(ty)) {
            .record => |fields| self.types.fieldSpan(fields),
            .named => |named| if (named.backing) |backing| return self.recordFields(backing.ty) else Common.invariant("named record has no backing"),
            else => Common.invariant("record operation expected record type"),
        };
    }

    fn recordFieldType(self: *Lowerer, ty: Type.TypeId, name: Type.names.RecordFieldNameId) Type.TypeId {
        for (self.recordFields(ty)) |field| {
            if (field.name == name) return field.ty;
        }
        Common.invariant("record operation referenced field absent from Lambda Mono type");
    }

    fn recordFieldIndex(self: *Lowerer, ty: Type.TypeId, name: Type.names.RecordFieldNameId) u16 {
        for (self.recordFields(ty), 0..) |field, index| {
            if (field.name == name) return @intCast(index);
        }
        Common.invariant("record operation referenced field absent from Lambda Mono type");
    }

    fn captureFieldIndex(self: *Lowerer, ty: Type.TypeId, symbol: Common.Symbol) u16 {
        const fields = switch (self.types.get(ty)) {
            .capture_record => |fields| self.types.captureFieldSpan(fields),
            else => Common.invariant("capture access expected capture record type"),
        };
        for (fields, 0..) |field, index| {
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
            else => Common.invariant("tag payload operation expected tag-union layout"),
        };
    }

    fn localFieldLayout(self: *Lowerer, source: LIR.LocalId, field_index: u16) layout.Idx {
        const source_layout_idx = self.result.store.getLocal(source).layout_idx;
        const source_layout = self.result.layouts.getLayout(source_layout_idx);
        const struct_layout_idx = switch (source_layout.tag) {
            .box => source_layout.getIdx(),
            else => source_layout_idx,
        };
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
            else => Common.invariant("list expression target was not a list layout"),
        };
    }

    fn localTagPayloadLayout(self: *Lowerer, source: LIR.LocalId, variant_index: u16, payload_idx: ?u16) layout.Idx {
        const payload_layout_idx = self.tagUnionPayloadLayout(self.result.store.getLocal(source).layout_idx, variant_index);
        const index = payload_idx orelse return payload_layout_idx;
        const payload_layout = self.result.layouts.getLayout(payload_layout_idx);
        return switch (payload_layout.tag) {
            .struct_ => self.result.layouts.getStructFieldLayoutByOriginalIndex(payload_layout.getStruct().idx, index),
            else => blk: {
                if (index != 0) Common.invariant("tag payload field read indexed a non-struct payload");
                break :blk payload_layout_idx;
            },
        };
    }

    fn currentLoop(self: *Lowerer) LoopContext {
        if (self.loop_stack.items.len == 0) Common.invariant("loop control expression reached LIR outside a loop");
        return self.loop_stack.items[self.loop_stack.items.len - 1];
    }

    fn freshJoinPointId(self: *Lowerer) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn pat(self: *const Lowerer, id: Lifted.PatId) Lifted.Pat {
        return self.solved.lifted.pats.items[@intFromEnum(id)];
    }
};

const TypeEquivalence = struct {
    allocator: std.mem.Allocator,
    lowerer: *Lowerer,
    materialized: *const Type.Store,
    map: std.AutoHashMap(Type.TypeId, Type.TypeId),

    fn init(allocator: std.mem.Allocator, lowerer: *Lowerer, materialized: *const Type.Store) TypeEquivalence {
        return .{
            .allocator = allocator,
            .lowerer = lowerer,
            .materialized = materialized,
            .map = std.AutoHashMap(Type.TypeId, Type.TypeId).init(allocator),
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
                break :blk try self.equivalent(named.backing.?.ty, other.backing.?.ty);
            },
        };
    }

    fn typeSpansEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.span(direct);
        const rhs = self.materialized.span(materialized);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |a, b| {
            if (!try self.equivalent(a, b)) return false;
        }
        return true;
    }

    fn fieldsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.fieldSpan(direct);
        const rhs = self.materialized.fieldSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |a, b| {
            if (a.name != b.name or !try self.equivalent(a.ty, b.ty)) return false;
        }
        return true;
    }

    fn captureFieldsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.captureFieldSpan(direct);
        const rhs = self.materialized.captureFieldSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |a, b| {
            if (!std.meta.eql(a.capture_id, b.capture_id)) return false;
            if (!try self.equivalent(a.ty, b.ty)) return false;
        }
        return true;
    }

    fn tagsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.tagSpan(direct);
        const rhs = self.materialized.tagSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |a, b| {
            if (a.name != b.name or a.checked_name != b.checked_name) return false;
            if (!try self.typeSpansEquivalent(a.payloads, b.payloads)) return false;
        }
        return true;
    }

    fn fnVariantsEquivalent(self: *TypeEquivalence, direct: Type.Span, materialized: Type.Span) Common.LowerError!bool {
        const lhs = self.lowerer.types.fnVariantSpan(direct);
        const rhs = self.materialized.fnVariantSpan(materialized);
        if (lhs.len != rhs.len) return false;
        for (lhs, rhs) |a, b| {
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
    var name_store = try cloneNameStore(allocator, &program.names);
    errdefer name_store.deinit();

    var types = try cloneMonoTypeStore(allocator, &program.types);
    errdefer types.deinit();

    var string_literals = try cloneStringLiterals(allocator, &program.string_literals);
    errdefer deinitStringLiterals(allocator, &string_literals);

    var source_files: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (source_files.items) |file| allocator.free(file);
        source_files.deinit(allocator);
    }
    try source_files.ensureTotalCapacityPrecise(allocator, program.source_files.items.len);
    for (program.source_files.items) |file| {
        source_files.appendAssumeCapacity(try allocator.dupe(u8, file));
    }

    return .{
        .allocator = allocator,
        .names = name_store,
        .next_symbol = program.next_symbol,
        .types = types,
        .imported_fns = try cloneArrayList(Lifted.ImportedFn, allocator, &program.imported_fns),
        .fns = try cloneArrayList(Lifted.Fn, allocator, &program.fns),
        .exprs = try cloneArrayList(Lifted.Expr, allocator, &program.exprs),
        .pats = try cloneArrayList(Lifted.Pat, allocator, &program.pats),
        .stmts = try cloneArrayList(Lifted.Stmt, allocator, &program.stmts),
        .locals = try cloneArrayList(Lifted.Local, allocator, &program.locals),
        .expr_ids = try cloneArrayList(Lifted.ExprId, allocator, &program.expr_ids),
        .pat_ids = try cloneArrayList(Lifted.PatId, allocator, &program.pat_ids),
        .typed_locals = try cloneArrayList(Lifted.TypedLocal, allocator, &program.typed_locals),
        .stmt_ids = try cloneArrayList(Lifted.StmtId, allocator, &program.stmt_ids),
        .field_exprs = try cloneArrayList(Lifted.FieldExpr, allocator, &program.field_exprs),
        .fn_def_captures = try cloneArrayList(Lifted.FnDefCapture, allocator, &program.fn_def_captures),
        .capture_operands = try cloneArrayList(Lifted.CaptureOperand, allocator, &program.capture_operands),
        .record_destructs = try cloneArrayList(Lifted.RecordDestruct, allocator, &program.record_destructs),
        .str_pattern_steps = try cloneArrayList(Lifted.StrPatternStep, allocator, &program.str_pattern_steps),
        .branches = try cloneArrayList(Lifted.Branch, allocator, &program.branches),
        .if_branches = try cloneArrayList(Lifted.IfBranch, allocator, &program.if_branches),
        .string_literals = string_literals,
        .next_lift_capture_id = program.next_lift_capture_id,
        .proc_debug_names = try cloneProcDebugNameMap(allocator, &program.proc_debug_names),
        .roots = try cloneArrayList(Lifted.Root, allocator, &program.roots),
        .layout_requests = try cloneArrayList(Lifted.LayoutRequest, allocator, &program.layout_requests),
        .runtime_schema_requests = try cloneArrayList(Lifted.RuntimeSchemaRequest, allocator, &program.runtime_schema_requests),
        .comptime_sites = try cloneComptimeSites(allocator, &program.comptime_sites),
        .source_files = source_files,
        .expr_locs = try cloneArrayList(base.SourceLoc, allocator, &program.expr_locs),
        .expr_regions = try cloneArrayList(base.Region, allocator, &program.expr_regions),
        .stmt_locs = try cloneArrayList(base.SourceLoc, allocator, &program.stmt_locs),
        .stmt_regions = try cloneArrayList(base.Region, allocator, &program.stmt_regions),
        .local_names = blk: {
            var names: std.ArrayList([]const u8) = .empty;
            errdefer {
                for (names.items) |name| {
                    if (name.len > 0) allocator.free(name);
                }
                names.deinit(allocator);
            }
            try names.ensureTotalCapacityPrecise(allocator, program.local_names.items.len);
            for (program.local_names.items) |name| {
                names.appendAssumeCapacity(if (name.len == 0) "" else try allocator.dupe(u8, name));
            }
            break :blk names;
        },
        .current_loc = program.current_loc,
        .current_region = program.current_region,
    };
}

fn cloneComptimeSites(allocator: std.mem.Allocator, source: *const std.ArrayList(Lifted.ComptimeSite)) std.mem.Allocator.Error!std.ArrayList(Lifted.ComptimeSite) {
    var cloned: std.ArrayList(Lifted.ComptimeSite) = .empty;
    errdefer {
        for (cloned.items) |site| allocator.free(site.branch_regions);
        cloned.deinit(allocator);
    }
    try cloned.ensureTotalCapacityPrecise(allocator, source.items.len);
    for (source.items) |site| {
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
/// held the same text under two ids — dedup would collapse those and shift every
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
    return .{
        .allocator = allocator,
        .types = try cloneArrayList(MonoType.Content, allocator, &source.types),
        .type_digests = try cloneArrayList(?check.CheckedNames.TypeDigest, allocator, &source.type_digests),
        .specialization_digests = try cloneArrayList(?check.CheckedNames.TypeDigest, allocator, &source.specialization_digests),
        .spans = try cloneArrayList(MonoType.TypeId, allocator, &source.spans),
        .fields = try cloneArrayList(MonoType.Field, allocator, &source.fields),
        .tags = try cloneArrayList(MonoType.Tag, allocator, &source.tags),
        .declared_fields = try cloneArrayList(MonoType.DeclaredField, allocator, &source.declared_fields),
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

fn cloneStringLiterals(allocator: std.mem.Allocator, source: *const std.ArrayList(Mono.StringLiteral)) std.mem.Allocator.Error!std.ArrayList(Mono.StringLiteral) {
    var cloned: std.ArrayList(Mono.StringLiteral) = .empty;
    errdefer deinitStringLiterals(allocator, &cloned);
    try cloned.ensureTotalCapacity(allocator, source.items.len);
    for (source.items) |literal| {
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
    var cloned: std.ArrayList(T) = .empty;
    errdefer cloned.deinit(allocator);
    try cloned.appendSlice(allocator, source.items);
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

fn lirSymbol(symbol: Common.Symbol) LIR.Symbol {
    return LIR.Symbol.fromRaw(@intCast(@intFromEnum(symbol)));
}

fn constFnTemplateFromMono(template: Mono.FnTemplate) LirProgram.FnTemplate {
    return .{
        .fn_def = constFnDefFromMono(template.fn_def),
        .source_fn_ty = template.source_fn_ty,
        .source_fn_key = template.source_fn_key,
    };
}

fn constFnDefFromMono(fn_def: Mono.FnDef) check.ConstStore.FnDef {
    return switch (fn_def) {
        .local_template => |template| .{ .local_template = template },
        .imported_template => |template| .{ .imported_template = template },
        .nested => |nested| .{ .nested = .{
            .owner = nested.owner,
            .site = nested.site,
            .context_fn_key = nested.context_fn_key,
        } },
        .local_hosted => |hosted| .{ .local_hosted = hosted.template },
        .imported_hosted => |hosted| .{ .imported_hosted = hosted.template },
        .checked_generated => |template| .{ .checked_generated = template },
        .parser_runtime => |runtime| .{ .parser_runtime = .{
            .owner = runtime.owner,
            .expr = runtime.expr,
        } },
        .encode_to_runtime => |runtime| .{ .encode_to_runtime = .{
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
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        Mono.ProcDebugNameMap.init(allocator),
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        .empty,
        0,
    );
    return Solved.Program.init(allocator, lifted);
}

test "layout lowering accepts tag payload alias backed by primitive" {
    const allocator = std.testing.allocator;

    var solved = emptySolvedProgramForTest(allocator);
    defer solved.deinit();

    const module_name = try solved.lifted.names.internModuleName("Repro");
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
            .def = .{ .module_name = module_name, .type_name = alias_name },
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
            .def = .{ .module_name = module_name, .type_name = repro_name },
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
