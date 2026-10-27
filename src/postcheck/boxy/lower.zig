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
};

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
    /// For a lambda worker: the checked captures of its closure expression,
    /// whose values follow it in the capture record before the outer
    /// worker's descriptors.
    captures: []const checked.CheckedCapture = &.{},
    /// For a lambda worker: the stored type of each captured value.
    capture_types: []const TypeId = &.{},
};

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
    pub fn templateWorker(self: *Program, module_index: ModuleIndex, template_id: canonical.CheckedProcedureTemplateId) Allocator.Error!WorkerId {
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
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
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
    ) Allocator.Error!WorkerId {
        const outer_worker = self.worker(outer);
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
            .scheme_vars = outer_worker.scheme_vars,
            .dictionary_params = &.{},
            .params = try self.arena.allocator().dupe(TypeId, self.store.span(signature.args)),
            .ret = signature.ret,
            .captures = try self.arena.allocator().dupe(checked.CheckedCapture, captures),
            .capture_types = try self.arena.allocator().dupe(TypeId, capture_types),
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

    // Row joins are sealed before any layout is committed.
    try program.joins.seal(&program.store);

    const requests = input.roots.requests;
    for (requests, 0..) |request, index| {
        const source_module = if (input.roots.source_modules.len == 0)
            program.modules.views.items[0].index
        else
            program.moduleByKey(input.roots.source_modules[index]).index;
        const proc = try lowerRoot(&program, request, source_module);
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
fn lowerRoot(program: *Program, request: checked.RootRequest, source_module: ModuleIndex) Allocator.Error!LIR.LirProcSpecId {
    _ = source_module;
    const template = request.procedure_template orelse invariantFmt("a {s} root has no procedure template", .{@tagName(request.kind)});
    const id = try program.templateRefWorker(template);
    const worker = program.worker(id);
    if (worker.scheme_vars.len != 0 or worker.dictionary_params.len != 0) {
        invariant("a root whose template has hidden parameters needs a host wrapper, which Boxy does not yet emit");
    }
    return worker.proc;
}
