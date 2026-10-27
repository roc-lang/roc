//! `.boxy` lowering: checked modules directly to LIR, one worker per
//! reachable procedure template, lowered once at its checked definition type
//! (design.md "`.boxy` Runtime Lowering").
//!
//! The lowerer runs one worklist over reachable workers, seeded from the
//! requested roots and grown by the call targets, dictionary targets, and
//! function values the bodies it lowers name. Every memo is keyed by checked
//! identity. Body lowering keeps its pending work on explicit stacks (see
//! `body.zig`).

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const Common = @import("../common.zig");
const types = @import("types.zig");
const RowJoins = @import("row_joins.zig");
const Layouts = @import("layouts.zig").Layouts;
const StaticDescs = @import("static_descs.zig").StaticDescs;
const Body = @import("body.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const canonical = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const LIR = lir_core.LIR;
const LirStore = lir_core.LirStore;
const LirProgram = lir_core.Program;
const RootMetadata = lir_core.RootMetadata.RootMetadata;

const TypeId = types.TypeId;
const ModuleIndex = types.ModuleIndex;
const invariant = types.invariant;
const invariantFmt = types.invariantFmt;

/// Everything one `.boxy` lowering reads.
pub const Input = struct {
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    target_usize: base.target.TargetUsize,
    inline_expects: enum { run, omit } = .run,
};

/// The lowered program and the static data it carries.
pub const Output = struct {
    lir_result: LirProgram.Result,
    static_data: []LirProgram.StaticDataExport,
};

/// A worker's identity: what it lowers.
pub const WorkerKey = union(enum) {
    /// A procedure template.
    template: struct { module: ModuleIndex, template: canonical.CheckedProcedureTemplateId },
    /// A lambda or closure expression of the worker `outer`, whose scope
    /// (its type variables and their descriptors) it shares through its
    /// captures.
    lambda: struct { module: ModuleIndex, expr: checked.CheckedExprId, outer: WorkerId },
    /// The erased-callable entry of a template worker that a dictionary
    /// names: it takes the method's arguments as its argument record, the
    /// target's hidden descriptors and dictionaries as its captures, and
    /// calls the target.
    method_entry: WorkerId,
    /// A compiler-derived structural method over the descriptor its
    /// dictionary captures.
    structural: StructuralMethod,
    /// The host-visible wrapper of root request `index`: it takes the
    /// root's checked arguments, supplies the static descriptors and
    /// dictionaries the root's instantiation names, and calls its target
    /// (design.md "Host ABI").
    root: u32,
};

/// The structural methods a dictionary can denote.
pub const StructuralMethod = enum { is_eq, to_hash };

pub const WorkerId = enum(u32) { _ };

/// One worker: its procedure and the signature callers use.
pub const Worker = struct {
    key: WorkerKey,
    proc: LIR.LirProcSpecId,
    module: ModuleIndex,
    /// The checked scheme variables of the worker's scope, in hidden
    /// descriptor parameter order.
    scheme_vars: []const checked.CheckedTypeId,
    /// Evidence parameters that are runtime dictionaries, in hidden
    /// dictionary parameter order (indices into the template's params).
    dictionary_params: []const u32,
    params: []const TypeId,
    ret: TypeId,
    lowered: bool = false,
    /// Whether the worker's body is queued for lowering.
    queued: bool = false,
    /// For a root wrapper: the root request it wraps.
    root_request: ?checked.RootRequest = null,
    /// For a lambda worker: the checked captures of its closure expression,
    /// whose values follow it in the capture record before the outer
    /// worker's descriptors.
    captures: []const checked.CheckedCapture = &.{},
    /// For a lambda worker: the stored type of each captured value.
    capture_types: []const TypeId = &.{},
    /// The dictionaries in scope, by generalized scope, innermost first:
    /// each scope's runtime evidence-parameter indices. A template has its
    /// own scope; a lambda shares its outer worker's scopes; a generalized
    /// local procedure has its own scope inside its outer worker's.
    dict_scopes: []const []const u32 = &.{},
    /// For a generalized local procedure: how many descriptors and
    /// dictionaries of its own scope its argument record carries before its
    /// explicit arguments (they vary per call, unlike its captures).
    own_descs: u32 = 0,
    own_dicts: u32 = 0,

    /// The dictionaries a lambda worker captures: every scope's but its own.
    pub fn capturedDictionaryCount(self: Worker) u32 {
        var count: u32 = 0;
        for (self.dict_scopes) |scope| count += @intCast(scope.len);
        return count - self.own_dicts;
    }
};

/// A generalized local procedure's own scheme: its quantified variables and
/// its runtime evidence-parameter indices.
pub const LocalScheme = struct {
    scheme_vars: []const checked.CheckedTypeId,
    dictionaries: []const u32,
};

/// A checked expression by its module in this lowering.
pub const ExprKey = struct { module: ModuleIndex, expr: checked.CheckedExprId };

/// A procedure template by its module in this lowering.
pub const TemplateKey = struct { module: ModuleIndex, template: canonical.CheckedProcedureTemplateId };

/// A hosted procedure's external symbol and its slot in the host's dispatch
/// table, or null when the platform's hosted section omits it.
pub const HostedEntry = struct { symbol: []const u8, dispatch_index: ?u32 };

pub const Program = struct {
    allocator: Allocator,
    input: Input,
    modules: types.Modules,
    store: types.Store,
    joins: RowJoins.Joins,
    layouts: Layouts,
    result: LirProgram.Result,
    static_descs: StaticDescs,
    workers: std.ArrayList(Worker) = .empty,
    worker_ids: std.AutoHashMapUnmanaged(WorkerKey, WorkerId) = .empty,
    worklist: std.ArrayList(WorkerId) = .empty,
    /// The program-local source file of each module, by module index.
    source_files: []u32 = &.{},
    lowering_module_ids: []LIR.LoweringModuleId = &.{},
    /// Modules by the checker's `module_idx`.
    by_module_idx: std.AutoHashMapUnmanaged(u32, ModuleIndex) = .empty,
    observe_expects: bool,
    arena: std.heap.ArenaAllocator,
    /// The host binding of each hosted procedure template.
    hosted: std.AutoHashMapUnmanaged(TemplateKey, HostedEntry) = .empty,
    /// The generalized-local scope a lambda or closure expression owns.
    local_scopes: std.AutoHashMapUnmanaged(ExprKey, checked.DispatchScopeId) = .empty,

    pub fn converter(self: *Program) types.Converter {
        return .{ .store = &self.store, .modules = &self.modules };
    }

    pub fn module(self: *const Program, index: ModuleIndex) *const types.ModuleView {
        return self.modules.get(index);
    }

    pub fn moduleByKey(self: *const Program, key: checked.ModuleId) *const types.ModuleView {
        return self.modules.byKey(key);
    }

    pub fn moduleByIdx(self: *const Program, idx: u32) *const types.ModuleView {
        const index = self.by_module_idx.get(idx) orelse invariant("a checked target named a module index outside this lowering");
        return self.modules.get(index);
    }

    pub fn sourceLoc(self: *const Program, index: ModuleIndex, region: base.Region) base.SourceLoc {
        if (region.isEmpty()) return base.SourceLoc.none;
        const view = self.modules.get(index);
        const line_starts = view.view.module_env.common.getLineStartsAll();
        if (line_starts.len == 0) return base.SourceLoc.none;
        const offset = region.start.offset;
        const line = base.RegionInfo.lineIdx(line_starts, offset);
        const column = base.RegionInfo.columnIdx(line_starts, line, offset) catch
            invariant("checked node region resolved to an invalid line/column position");
        return .{ .file = self.source_files[@backingInt(index)], .line = line + 1, .column = column + 1 };
    }

    /// The worker for a procedure template, requested if new.
    /// The worker of a procedure template, queued for lowering.
    pub fn templateWorker(self: *Program, module_index: ModuleIndex, template_id: canonical.CheckedProcedureTemplateId) Allocator.Error!WorkerId {
        const id = try self.templateScope(module_index, template_id);
        if (!self.worker(id).queued) {
            self.worker(id).queued = true;
            try self.worklist.append(self.allocator, id);
        }
        return id;
    }

    /// The worker of a procedure template as the scope of the lambdas it
    /// contains, lowered only when something calls the template itself. The
    /// owner of a closure that compile-time evaluation stored is such a
    /// scope: its body ran at compile time.
    pub fn templateScope(self: *Program, module_index: ModuleIndex, template_id: canonical.CheckedProcedureTemplateId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .template = .{ .module = module_index, .template = template_id } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const view = self.module(module_index);
        const template = view.view.checked_procedure_templates.templates.items[@backingInt(template_id)];
        const scheme_vars = poolSlice(checked.CheckedTypeId, view.view.checked_procedure_templates.scheme_vars_pool, template.scheme_vars);
        var dictionary_params: std.ArrayList(u32) = .empty;
        const evidence_params = poolSlice(static_dispatch.EvidenceParamRecord, view.view.checked_procedure_templates.evidence_params_pool, template.evidence_params);
        for (evidence_params, 0..) |param, index| {
            if (param.runtime_dictionary) try dictionary_params.append(self.arena.allocator(), @intCast(index));
        }
        const bound = try self.arena.allocator().alloc(TypeId, scheme_vars.len);
        for (bound, 0..) |*slot, index| slot.* = try self.store.variable(@intCast(index));
        const env: types.Env = .{ .module = module_index, .vars = scheme_vars, .bound = bound };
        const fn_ty = try self.converter().convert(module_index, template.checked_fn_root, env);
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            else => invariant("a procedure template's checked root is not a function type"),
        };
        const params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args));

        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.worker", @backingInt(id)),
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = scheme_vars,
            .dictionary_params = dictionary_params.items,
            .params = params,
            .ret = signature.ret,
            .dict_scopes = try self.arena.allocator().dupe([]const u32, &.{dictionary_params.items}),
        });
        try self.worker_ids.put(self.allocator, key, id);
        return id;
    }

    pub fn templateRefScope(self: *Program, ref: canonical.ProcedureTemplateRef) Allocator.Error!WorkerId {
        const view = self.moduleByKey(.{ .bytes = ref.artifact.bytes });
        return try self.templateScope(view.index, ref.template);
    }

    /// The erased-callable worker of a lambda or closure expression of the
    /// worker `outer`.
    pub fn lambdaWorker(
        self: *Program,
        outer: WorkerId,
        lambda_expr: checked.CheckedExprId,
        captures: []const checked.CheckedCapture,
        capture_types: []const TypeId,
        fn_ty: TypeId,
        own: ?LocalScheme,
    ) Allocator.Error!WorkerId {
        const outer_worker = self.worker(outer).*;
        const key: WorkerKey = .{ .lambda = .{ .module = outer_worker.module, .expr = lambda_expr, .outer = outer } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            else => invariant("a lambda's type is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.lambda", @backingInt(id)),
            .args = .empty(),
            .ret_layout = .zst,
            .abi = .erased_record,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = outer_worker.module,
            .scheme_vars = if (own) |scheme| try std.mem.concat(self.arena.allocator(), checked.CheckedTypeId, &.{ outer_worker.scheme_vars, scheme.scheme_vars }) else outer_worker.scheme_vars,
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .captures = try self.arena.allocator().dupe(checked.CheckedCapture, captures),
            .capture_types = try self.arena.allocator().dupe(TypeId, capture_types),
            .dict_scopes = if (own) |scheme| try std.mem.concat(self.arena.allocator(), []const u32, &.{ &.{scheme.dictionaries}, outer_worker.dict_scopes }) else outer_worker.dict_scopes,
            .own_descs = if (own) |scheme| @intCast(scheme.scheme_vars.len) else 0,
            .own_dicts = if (own) |scheme| @intCast(scheme.dictionaries.len) else 0,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// The wrapper of root request `index` (see `WorkerKey.root`).
    pub fn rootWorker(self: *Program, index: u32, request: checked.RootRequest, module_index: ModuleIndex) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .root = index };
        if (self.worker_ids.get(key)) |existing| return existing;
        const fn_ty = try self.converter().convert(module_index, request.checked_type, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        const signature = switch (self.store.get(fn_ty)) {
            .function => |function| function,
            else => invariant("a procedure root's checked type is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.root", @backingInt(id)),
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = &.{},
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .root_request = request,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// The dictionary entry of template worker `target`.
    pub fn methodEntryWorker(self: *Program, target: WorkerId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .method_entry = target };
        if (self.worker_ids.get(key)) |existing| return existing;
        const target_worker = self.worker(target).*;
        return try self.addErasedWorker(key, target_worker.module, target_worker.scheme_vars, target_worker.params, target_worker.ret, "boxy.method_entry");
    }

    /// The structural method worker of `method`, generic over one type `a`
    /// whose descriptor it captures.
    pub fn structuralWorker(self: *Program, module_index: ModuleIndex, method: StructuralMethod) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .structural = method };
        if (self.worker_ids.get(key)) |existing| return existing;
        const a = try self.store.variable(0);
        const params: []const TypeId, const ret: TypeId = switch (method) {
            .is_eq => .{ try self.arena.allocator().dupe(TypeId, &.{ a, a }), try self.store.primitive(.bool) },
            .to_hash => blk: {
                const hasher = try self.store.primitive(.u64);
                break :blk .{ try self.arena.allocator().dupe(TypeId, &.{ a, hasher }), hasher };
            },
        };
        const scheme_vars = try self.arena.allocator().alloc(checked.CheckedTypeId, 1);
        scheme_vars[0] = @fromBackingInt(std.math.maxInt(u32));
        return try self.addErasedWorker(key, module_index, scheme_vars, params, ret, "boxy.structural");
    }

    fn addErasedWorker(self: *Program, key: WorkerKey, module_index: ModuleIndex, scheme_vars: []const checked.CheckedTypeId, params: []const TypeId, ret: TypeId, role: []const u8) Allocator.Error!WorkerId {
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal(role, @backingInt(id)),
            .args = .empty(),
            .ret_layout = .zst,
            .abi = .erased_record,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = scheme_vars,
            .dictionary_params = &.{},
            .params = params,
            .ret = ret,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    pub fn worker(self: *Program, id: WorkerId) *Worker {
        return &self.workers.items[@backingInt(id)];
    }

    /// The worker of a template reference, which names its module by digest.
    pub fn templateRefWorker(self: *Program, ref: canonical.ProcedureTemplateRef) Allocator.Error!WorkerId {
        const view = self.moduleByKey(.{ .bytes = ref.artifact.bytes });
        return try self.templateWorker(view.index, ref.template);
    }

    fn drain(self: *Program) Allocator.Error!void {
        while (self.worklist.pop()) |id| {
            if (self.worker(id).lowered) continue;
            self.worker(id).lowered = true;
            try Body.lowerWorker(self, id);
        }
    }
};

pub fn poolSlice(comptime T: type, pool: []const T, span: anytype) []const T {
    return pool[span.start..][0..span.len];
}

/// Lower the requested roots and everything they reach.
pub fn run(allocator: Allocator, input: Input) Allocator.Error!Output {
    base.dev_diagnostics.traceBoxyWork("boxy lowering");
    var program: Program = .{
        .allocator = allocator,
        .input = input,
        .modules = .{ .allocator = allocator },
        .store = types.Store.init(allocator),
        .joins = RowJoins.Joins.init(allocator),
        .layouts = undefined,
        .result = try LirProgram.Result.init(allocator, input.target_usize),
        .static_descs = undefined,
        .observe_expects = input.roots.test_plan_metadata.len != 0,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    defer {
        program.static_descs.deinit();
        program.layouts.deinit();
        program.joins.deinit();
        program.store.deinit();
        program.modules.deinit();
        program.workers.deinit(allocator);
        program.worker_ids.deinit(allocator);
        program.worklist.deinit(allocator);
        program.by_module_idx.deinit(allocator);
        program.hosted.deinit(allocator);
        program.local_scopes.deinit(allocator);
        program.arena.deinit();
    }
    errdefer program.result.deinit();
    program.result.store.runtime_contract = .boxy;
    program.layouts = .{
        .allocator = allocator,
        .store = &program.store,
        .converter = program.converter(),
        .joins = &program.joins,
        .layouts = &program.result.layouts,
    };
    program.static_descs = StaticDescs.init(allocator, &program.store, &program.layouts, &program.result, input.target_usize);

    _ = try program.modules.add(checked.importedView(input.modules.root.module));
    for (input.modules.imports) |view| _ = try program.modules.add(view);
    for (input.modules.root.relation_modules) |view| _ = try program.modules.add(view);
    for (program.modules.views.items) |view| {
        try program.by_module_idx.put(allocator, view.view.module_identity.module_idx, view.index);
    }
    try seedSourceFiles(&program);
    try initHosted(&program);
    for (program.modules.views.items) |*view| {
        const bodies = view.bodies();
        for (view.view.checked_procedure_templates.dispatch_scopes, 0..) |scope, index| {
            const id: checked.DispatchScopeId = @fromBackingInt(@intCast(index));
            try program.local_scopes.put(allocator, .{ .module = view.index, .expr = scope.checked_expr }, id);
            switch (bodies.expr(scope.checked_expr).data) {
                .closure => |closure| try program.local_scopes.put(allocator, .{ .module = view.index, .expr = closure.lambda }, id),
                else => {},
            }
        }
    }

    // Row joins are seeded from every checked body and sealed before any
    // layout is committed.
    try seedRowJoins(&program);
    try program.joins.seal(&program.store);

    const requests = input.roots.requests;
    for (requests, 0..) |request, index| {
        const source_module = if (input.roots.source_modules.len == 0)
            program.modules.views.items[0].index
        else
            program.moduleByKey(input.roots.source_modules[index]).index;
        const proc = try lowerRoot(&program, request, @intCast(index), source_module);
        try program.result.root_procs.append(allocator, proc);
        var metadata = RootMetadata.fromCheckedRoot(request);
        metadata.test_plan = Common.testPlanMetadataForRoot(input.roots, request, index);
        try program.result.root_metadata.append(allocator, metadata);
    }
    try program.drain();
    for (input.roots.layout_requests) |checked_ty| {
        const ty = try program.converter().convert(program.modules.views.items[0].index, checked_ty, .{ .module = program.modules.views.items[0].index, .vars = &.{}, .bound = &.{} });
        _ = try program.layouts.layoutOf(ty);
    }
    if (input.roots.static_data_requests.len != 0) invariant("static data requests are not yet lowered by Boxy");

    program.result.finishExpectSites();
    return .{
        .lir_result = program.result,
        .static_data = try program.static_descs.takeExports(),
    };
}

fn seedSourceFiles(program: *Program) Allocator.Error!void {
    const allocator = program.allocator;
    const count = program.modules.views.items.len;
    const order = try allocator.alloc(u32, count);
    defer allocator.free(order);
    for (order, 0..) |*slot, index| slot.* = @intCast(index);
    std.mem.sort(u32, order, program, struct {
        fn lessThan(p: *Program, a: u32, b: u32) bool {
            return std.mem.order(u8, &p.modules.views.items[a].key.bytes, &p.modules.views.items[b].key.bytes) == .lt;
        }
    }.lessThan);
    program.source_files = try program.arena.allocator().alloc(u32, count);
    program.lowering_module_ids = try program.arena.allocator().alloc(LIR.LoweringModuleId, count);
    const files = try allocator.alloc(base.SourceFileEntry, count);
    defer allocator.free(files);
    for (order, 0..) |module_position, file_index| {
        const view = &program.modules.views.items[module_position];
        files[file_index] = .{
            .name = view.view.module_env.module_name,
            .qualified_name = view.view.module_env.qualifiedModuleName(),
            .module_identity = view.view.module_identity.stable_hash,
        };
        program.source_files[module_position] = @intCast(file_index);
        program.lowering_module_ids[module_position] = try program.result.appendLoweringModule(view.key);
    }
    try program.result.store.setSourceFiles(files);
}

/// A root's procedure: its template's worker, which a host-shaped wrapper
/// would call with static hidden arguments when the worker has any.
/// Seed the row joins (design.md "Row Joins"): each `row_coerce` with its
/// value, each lambda's body with its function's result, and each `return`
/// with its function's result, in every module's checked bodies.
fn seedRowJoins(program: *Program) Allocator.Error!void {
    const allocator = program.allocator;
    for (program.modules.views.items) |*view| {
        var scheme_vars: std.AutoHashMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer scheme_vars.deinit(allocator);
        for (view.view.checked_procedure_templates.scheme_vars_pool) |variable| try scheme_vars.put(allocator, variable, {});
        var converter = program.converter();
        converter.seed_scheme_vars = &scheme_vars;
        const bodies = view.bodies();
        for (0..bodies.exprCount()) |index| {
            const id: checked.CheckedExprId = @fromBackingInt(@intCast(index));
            const expr = bodies.expr(id);
            const pair: [2]checked.CheckedTypeId = switch (expr.data) {
                .row_coerce => |coerce| .{ expr.ty, bodies.expr(coerce.value).ty },
                .lambda => |lambda| .{ checkedResult(view, expr.ty) orelse continue, bodies.expr(lambda.body).ty },
                .return_ => |ret| .{ checkedResult(view, bodies.expr(ret.lambda).ty) orelse continue, bodies.expr(ret.expr).ty },
                else => continue,
            };
            try seedJoin(program, converter, view.index, pair);
        }
        for (0..bodies.statementCount()) |index| {
            const id: checked.CheckedStatementId = @fromBackingInt(@intCast(index));
            switch (bodies.statement(id).data) {
                .return_ => |ret| try seedJoin(program, converter, view.index, .{ checkedResult(view, bodies.expr(ret.lambda).ty) orelse continue, bodies.expr(ret.expr).ty }),
                else => {},
            }
        }
    }
}

/// Join the types of a flow edge; one type flowing to itself joins nothing.
fn seedJoin(program: *Program, converter: types.Converter, module_index: ModuleIndex, pair: [2]checked.CheckedTypeId) Allocator.Error!void {
    if (pair[0] == pair[1]) return;
    const env: types.Env = .{ .module = module_index, .vars = &.{}, .bound = &.{} };
    try program.joins.join(&program.store, try converter.convert(module_index, pair[0], env), try converter.convert(module_index, pair[1], env));
}

/// The checked result type of a lambda's function type, through aliases; a
/// rejected lambda's type has none.
fn checkedResult(view: *const types.ModuleView, fn_ty: checked.CheckedTypeId) ?checked.CheckedTypeId {
    var current = fn_ty;
    while (true) switch (view.types().payload(current)) {
        .alias => |alias| current = alias.backing,
        .function => |function| return function.ret,
        else => return null,
    };
}

/// Bind every hosted procedure in scope to its external symbol and dispatch
/// slot: the platform header's hosted section, in order, when a platform is
/// in scope; otherwise every declaration in a stable order.
fn initHosted(program: *Program) Allocator.Error!void {
    const allocator = program.allocator;
    const DefKey = struct { module: ModuleIndex, def: u32 };
    var by_def: std.AutoHashMapUnmanaged(DefKey, TemplateKey) = .empty;
    defer by_def.deinit(allocator);
    const Ordered = struct { key: TemplateKey, order: []const u8, def: u32, digest: [32]u8 };
    var ordered: std.ArrayList(Ordered) = .empty;
    defer ordered.deinit(allocator);
    var platform: ?*const types.ModuleView = null;
    for (program.modules.views.items) |*view| {
        if (view.view.module_identity.kind == .platform) platform = view;
        for (view.view.hosted_procs.procs) |proc| {
            const owner = program.moduleByKey(.{ .bytes = proc.template.artifact.bytes });
            const key: TemplateKey = .{ .module = owner.index, .template = proc.template.template };
            try program.hosted.put(allocator, key, .{
                .symbol = view.view.canonical_names.externalSymbolNameText(proc.external_symbol_name),
                .dispatch_index = null,
            });
            try by_def.put(allocator, .{ .module = view.index, .def = @backingInt(proc.def_idx) }, key);
            try ordered.append(allocator, .{ .key = key, .order = proc.orderKey(view.view.hosted_procs), .def = @backingInt(proc.def_idx), .digest = view.key.bytes });
        }
    }
    if (platform) |view| {
        for (view.view.hosted_bindings.bindings, 0..) |binding, index| {
            const target = program.moduleByKey(binding.target_checked_module);
            const key = by_def.get(.{ .module = target.index, .def = @backingInt(binding.target_def) }) orelse
                invariant("a hosted section names a function with no hosted declaration in scope");
            program.hosted.getPtr(key).?.* = .{
                .symbol = view.view.canonical_names.externalSymbolNameText(binding.external_symbol_name),
                .dispatch_index = @intCast(index),
            };
        }
        return;
    }
    std.mem.sort(Ordered, ordered.items, {}, struct {
        fn lessThan(_: void, a: Ordered, b: Ordered) bool {
            return switch (std.mem.order(u8, a.order, b.order)) {
                .lt => true,
                .gt => false,
                .eq => if (a.def != b.def) a.def < b.def else std.mem.order(u8, &a.digest, &b.digest) == .lt,
            };
        }
    }.lessThan);
    for (ordered.items, 0..) |item, index| program.hosted.getPtr(item.key).?.dispatch_index = @intCast(index);
}

fn lowerRoot(program: *Program, request: checked.RootRequest, index: u32, source_module: ModuleIndex) Allocator.Error!LIR.LirProcSpecId {
    // A root whose target takes no hidden arguments is the target itself.
    const direct: ?canonical.ProcedureTemplateRef = if (request.procedure_template) |template|
        template
    else if (request.procedure_binding) |binding| switch (program.module(source_module).view.top_level_procedure_bindings.bindings.items[@backingInt(binding)].body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |ref| ref,
            .lifted, .synthetic => invariant("lifted and synthetic procedure templates are not yet lowered by Boxy"),
        },
        .callable_eval_template => null,
        .checked_error => invariant("a root names a rejected procedure binding"),
    } else null;
    if (direct) |template| {
        const id = try program.templateRefWorker(template);
        const worker = program.worker(id);
        if (worker.scheme_vars.len == 0 and worker.dictionary_params.len == 0) return worker.proc;
    }
    return program.worker(try program.rootWorker(index, request, source_module)).proc;
}
