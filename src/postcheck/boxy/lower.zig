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
const Codec = @import("codec.zig");

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
    /// Name each procedure in the LIR store by the kind of worker it is,
    /// for dumps and tests that read the program.
    proc_debug_names: bool = false,
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
    /// A function value of type `to` calling one of type `from`, which differ
    /// in the storage of record fields (design.md "Field Kinds"); both are
    /// in the variable space of the worker `outer`, whose descriptors the
    /// value captures after the called function.
    adapter: struct { outer: WorkerId, from: TypeId, to: TypeId },
    /// The `??` default expression `expr` of `module`, evaluated where a
    /// construction omits its field (design.md "Defaulted Fields").
    default: struct { module: ModuleIndex, expr: checked.CheckedExprId },
    /// A lambda or closure expression of the worker `outer`, whose scope
    /// (its type variables and their descriptors) it shares through its
    /// captures.
    /// `captured_scheme` is set for a local procedure's function value whose
    /// own scheme's instantiation it captures (a value compile-time
    /// evaluation stored) rather than taking per call.
    lambda: struct { module: ModuleIndex, expr: checked.CheckedExprId, outer: WorkerId, captured_scheme: bool = false },
    /// The erased-callable entry of a template worker that a dictionary
    /// names: it takes the method's arguments as its argument record, the
    /// target's hidden descriptors and dictionaries as its captures, and
    /// calls the target.
    method_entry: WorkerId,
    /// A compiler-derived structural method over the descriptor its
    /// dictionary captures.
    structural: StructuralMethod,
    /// A derived `map`/`map!` over the tag union its dictionary describes:
    /// the variant `name` of `arity` payloads has payload `payload_index`
    /// transformed, every other value is re-tagged unchanged.
    structural_map: StructuralMap,
    /// The host-visible wrapper of root request `index`: it takes the
    /// root's checked arguments, supplies the static descriptors and
    /// dictionaries the root's instantiation names, and calls its target
    /// (design.md "Host ABI").
    root: u32,
    /// A worker of a generated codec family (`Codec.Piece`, by index).
    codec: u32,
    /// The method of a dictionary whose checked evidence resolves its slot
    /// to a crash: a dispatch checking rejected, or one no value can reach
    /// (design.md "Static Dispatch In Monotype"). Calling it crashes.
    crash_dictionary: CrashDictionary,
};

/// Why a dictionary slot's method crashes.
pub const CrashDictionary = enum { checked_error, unreachable_value };

/// An exported procedure binding's identity within its module.
pub const ExportedBindingKey = struct { module: ModuleIndex, def: @FieldType(checked.ImportedProcedureBindingRef, "def"), pattern: checked.CheckedPatternId };

/// The structural methods a dictionary can denote.
pub const StructuralMethod = enum { is_eq, to_hash, to_inspect };

/// A derived map's checked payload selection. Its worker's descriptors are
/// the input union's, the output union's, the selected variant's `arity`
/// input payloads', and the transformed payload's output type's.
pub const StructuralMap = struct { name: types.NameId, payload_index: u32, arity: u32, effectful: bool };

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
    /// Each of `dict_scopes`' scopes' evidence-parameter records, by
    /// parameter index.
    dict_scope_params: []const []const static_dispatch.EvidenceParamRecord = &.{},
    /// For a generalized local procedure: how many descriptors and
    /// dictionaries of its own scope its argument record carries before its
    /// explicit arguments (they vary per call, unlike its captures).
    own_descs: u32 = 0,
    /// For a structural worker: how many descriptors its dictionary captures,
    /// each standing for a structural type parameter (`types.Store.variable`
    /// by position) rather than a checked scheme variable.
    structural_descs: u32 = 0,
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
    /// The scope's evidence-parameter records, by parameter index.
    params: []const static_dispatch.EvidenceParamRecord = &.{},
    /// Whether each call passes the scheme's descriptors and dictionaries (a
    /// local procedure's), or the function value captures them at its
    /// construction (any other generalized lambda's).
    per_call: bool,
};

/// A nominal method override: its dictionary entry, the types the method's
/// scheme variables take, and its dictionaries: each the structural method
/// over the type its constraint dispatches on, which honors that type's own
/// overrides through its descriptor.
pub const NominalOverride = struct { entry: WorkerId, bound: []const TypeId, dicts: []const StructuralDict };

pub const StructuralDict = struct { method: StructuralMethod, ty: TypeId };

/// The overrides of a nominal type, in `Methods` order.
pub const NominalOverrides = [3]?NominalOverride;

/// A field a record construction omits.
pub const OmittedField = struct { module: ModuleIndex, expr: checked.CheckedExprId, name: types.NameId };

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
    /// Each module's exported procedure bindings, by their definition.
    exported_bindings: std.AutoHashMapUnmanaged(ExportedBindingKey, *const checked.ImportedProcedureBindingView) = .empty,
    observe_expects: bool,
    arena: std.heap.ArenaAllocator,
    /// The host binding of each hosted procedure template.
    hosted: std.AutoHashMapUnmanaged(TemplateKey, HostedEntry) = .empty,
    /// The generalized-local scope a lambda or closure expression owns.
    local_scopes: std.AutoHashMapUnmanaged(ExprKey, checked.DispatchScopeId) = .empty,
    /// The lambda and closure expressions local procedures bind.
    local_procs: std.AutoHashMapUnmanaged(ExprKey, void) = .empty,
    /// Whether each worker asked about stores a record field of its
    /// signature in a presence slot (`signatureHasSlots`).
    signature_slots: std.AutoHashMapUnmanaged(WorkerId, bool) = .empty,
    /// `nominalOverrides` of each nominal type asked about.
    nominal_overrides: std.AutoHashMapUnmanaged(TypeId, NominalOverrides) = .empty,
    /// The nested procedure site of each lambda and closure expression.
    sites_by_expr: std.AutoHashMapUnmanaged(ExprKey, u32) = .empty,
    /// Modules by their stable identity hash.
    by_identity: std.AutoHashMapUnmanaged([32]u8, ModuleIndex) = .empty,
    /// The default a record construction that omits a field materializes.
    omitted_defaults: std.AutoHashMapUnmanaged(OmittedField, checked.CheckedFieldDefault) = .empty,
    /// The generated codec families and their workers.
    codec: Codec.State = .{},

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
            .dict_scope_params = try self.arena.allocator().dupe([]const static_dispatch.EvidenceParamRecord, &.{evidence_params}),
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
        const key: WorkerKey = .{ .lambda = .{ .module = outer_worker.module, .expr = lambda_expr, .outer = outer, .captured_scheme = if (own) |scheme| scheme.per_call == false and self.local_procs.contains(.{ .module = outer_worker.module, .expr = lambda_expr }) else false } };
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
            .dict_scope_params = if (own) |scheme| try std.mem.concat(self.arena.allocator(), []const static_dispatch.EvidenceParamRecord, &.{ &.{scheme.params}, outer_worker.dict_scope_params }) else outer_worker.dict_scope_params,
            .own_descs = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.scheme_vars.len) else 0) else 0,
            .own_dicts = if (own) |scheme| (if (scheme.per_call) @intCast(scheme.dictionaries.len) else 0) else 0,
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// The relation of `relateVariablesAll`, leaving variables the types do
    /// not relate null.
    pub fn relateVariablesPartial(self: *Program, allocator: Allocator, roots: []const [2]TypeId, count: usize) Allocator.Error![]?TypeId {
        return try self.relateVariablesRows(allocator, roots, count, null);
    }

    /// `relateVariablesPartial`, also marking in `row_vars` each variable
    /// related as an open tag row's remainder. A closed actual row relates
    /// through its join representative, whose layout it has: an open row's
    /// remainder is the representative's tags the row does not name
    /// (design.md "Row Joins").
    pub fn relateVariablesRows(self: *Program, allocator: Allocator, roots: []const [2]TypeId, count: usize, row_vars: ?[]bool) Allocator.Error![]?TypeId {
        const store = &self.store;
        const bound = try allocator.alloc(?TypeId, count);
        defer allocator.free(bound);
        @memset(bound, null);
        var pending: std.ArrayList([2]TypeId) = .empty;
        defer pending.deinit(allocator);
        try pending.appendSlice(allocator, roots);
        while (pending.pop()) |pair| {
            const formal = store.get(pair[0]);
            if (formal == .variable) {
                bound[formal.variable] = pair[1];
                continue;
            }
            const other = store.get(pair[1]);
            if (std.meta.activeTag(formal) != std.meta.activeTag(other)) {
                // A nominal shares its backing's representation: relate a
                // structural position through the other side's backing.
                if (other == .nominal and other.nominal.has_backing and formal != .nominal) {
                    try pending.append(allocator, .{ pair[0], try self.converter().backing(pair[1]) });
                } else if (formal == .nominal and formal.nominal.has_backing and other != .nominal) {
                    try pending.append(allocator, .{ try self.converter().backing(pair[0]), pair[1] });
                }
                continue;
            }
            switch (formal) {
                .list => |elem| try pending.append(allocator, .{ elem, other.list }),
                .box => |payload| try pending.append(allocator, .{ payload, other.box }),
                .tuple => |items| for (store.span(items), store.span(other.tuple)) |a, b| try pending.append(allocator, .{ a, b }),
                .function => |function| {
                    for (store.span(function.args), store.span(other.function.args)) |a, b| try pending.append(allocator, .{ a, b });
                    try pending.append(allocator, .{ function.ret, other.function.ret });
                },
                .nominal => |nominal| for (store.nominalArgs(nominal), store.nominalArgs(other.nominal)) |a, b| try pending.append(allocator, .{ a, b }),
                .record => |record| {
                    var residual: std.ArrayList(types.Field) = .empty;
                    defer residual.deinit(allocator);
                    for (store.fieldSpan(other.record.fields)) |candidate| {
                        var named = false;
                        for (store.fieldSpan(record.fields)) |field| {
                            if (candidate.name == field.name) {
                                named = true;
                                try pending.append(allocator, .{ field.ty, candidate.ty });
                            }
                        }
                        if (!named) try residual.append(allocator, candidate);
                    }
                    if (record.ext) |ext| {
                        const remainder = if (residual.items.len == 0 and other.record.ext == null)
                            try store.emptyRecord()
                        else
                            try store.intern(.{ .record = .{ .fields = residual.items, .ext = other.record.ext } });
                        try pending.append(allocator, .{ ext, remainder });
                    }
                },
                .tag_union => |tag_union| {
                    var residual: std.ArrayList(types.TagInput) = .empty;
                    defer residual.deinit(allocator);
                    const actual = if (tag_union.ext != null and other.tag_union.ext == null) switch (store.get(self.joins.representative(pair[1]))) {
                        .tag_union => |rep| rep,
                        else => other.tag_union,
                    } else other.tag_union;
                    for (store.tagSpan(actual.tags)) |candidate| {
                        var named = false;
                        for (store.tagSpan(tag_union.tags)) |tag| {
                            if (candidate.name == tag.name and candidate.payloads.len == tag.payloads.len) {
                                named = true;
                                for (store.span(tag.payloads), store.span(candidate.payloads)) |a, b| try pending.append(allocator, .{ a, b });
                            }
                        }
                        if (!named) try residual.append(allocator, .{ .name = candidate.name, .payloads = store.span(candidate.payloads) });
                    }
                    // An open formal row's remainder is the actual row's
                    // tags it does not name.
                    if (tag_union.ext) |ext| {
                        if (row_vars) |marks| switch (store.get(ext)) {
                            .variable => |index| if (index < marks.len) {
                                marks[index] = true;
                            },
                            else => {},
                        };
                        try pending.append(allocator, .{ ext, try store.intern(.{ .tag_union = .{ .tags = residual.items, .ext = actual.ext } }) });
                    }
                },
                .primitive, .empty_record, .variable => {},
            }
        }
        return try allocator.dupe(?TypeId, bound);
    }

    /// Whether worker `id`'s parameters or result store a record field in a
    /// presence slot, so a caller may store it differently (design.md
    /// "Field Kinds").
    pub fn signatureHasSlots(self: *Program, id: WorkerId) Allocator.Error!bool {
        if (self.signature_slots.get(id)) |memo| return memo;
        const target = self.worker(id).*;
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(self.allocator);
        var seen: std.AutoHashMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(self.allocator);
        try pending.appendSlice(self.allocator, target.params);
        try pending.append(self.allocator, target.ret);
        const found = walk: while (pending.pop()) |ty| {
            if ((try seen.getOrPut(self.allocator, ty)).found_existing) continue;
            switch (self.store.get(ty)) {
                .record => |record| {
                    for (self.store.fieldSpan(record.fields)) |field| {
                        if (field.kind == .presence_slot) break :walk true;
                        try pending.append(self.allocator, field.ty);
                    }
                    if (record.ext) |ext| try pending.append(self.allocator, ext);
                },
                .tuple => |items| try pending.appendSlice(self.allocator, self.store.span(items)),
                .tag_union => |tag_union| {
                    for (self.store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(self.allocator, self.store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(self.allocator, ext);
                },
                .list => |elem| try pending.append(self.allocator, elem),
                .box => |payload| try pending.append(self.allocator, payload),
                .nominal => |nominal| try pending.appendSlice(self.allocator, self.store.nominalArgs(nominal)),
                .function => |function| {
                    try pending.appendSlice(self.allocator, self.store.span(function.args));
                    try pending.append(self.allocator, function.ret);
                },
                .primitive, .empty_record, .variable => {},
            }
        } else false;
        try self.signature_slots.put(self.allocator, id, found);
        return found;
    }

    /// `ty` with each variable `i` replaced by `bound[i]`. A row whose
    /// remainder becomes a row joins it.
    pub fn substitute(self: *Program, allocator: Allocator, root: TypeId, bound: []const TypeId) Allocator.Error!TypeId {
        var done: std.AutoHashMapUnmanaged(TypeId, TypeId) = .empty;
        defer done.deinit(allocator);
        const Frame = struct { ty: TypeId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, .{ .ty = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (done.contains(frame.ty)) continue;
            const node = self.store.get(frame.ty);
            if (!frame.expanded) {
                try stack.append(allocator, .{ .ty = frame.ty, .expanded = true });
                switch (node) {
                    .record => |record| {
                        for (self.store.fieldSpan(record.fields)) |field| try stack.append(allocator, .{ .ty = field.ty, .expanded = false });
                        if (record.ext) |ext| try stack.append(allocator, .{ .ty = ext, .expanded = false });
                    },
                    .tuple => |items| for (self.store.span(items)) |item| try stack.append(allocator, .{ .ty = item, .expanded = false }),
                    .tag_union => |tag_union| {
                        for (self.store.tagSpan(tag_union.tags)) |tag| for (self.store.span(tag.payloads)) |payload| try stack.append(allocator, .{ .ty = payload, .expanded = false });
                        if (tag_union.ext) |ext| try stack.append(allocator, .{ .ty = ext, .expanded = false });
                    },
                    .list => |elem| try stack.append(allocator, .{ .ty = elem, .expanded = false }),
                    .box => |payload| try stack.append(allocator, .{ .ty = payload, .expanded = false }),
                    .nominal => |nominal| for (self.store.nominalArgs(nominal)) |arg| try stack.append(allocator, .{ .ty = arg, .expanded = false }),
                    .function => |function| {
                        for (self.store.span(function.args)) |arg| try stack.append(allocator, .{ .ty = arg, .expanded = false });
                        try stack.append(allocator, .{ .ty = function.ret, .expanded = false });
                    },
                    .primitive, .empty_record, .variable => {},
                }
                continue;
            }
            const result: TypeId = switch (node) {
                .primitive, .empty_record => frame.ty,
                .variable => |index| if (index < bound.len) bound[index] else invariant("a substituted type names a variable past its bindings"),
                .list => |elem| try self.store.intern(.{ .list = done.get(elem).? }),
                .box => |payload| try self.store.intern(.{ .box = done.get(payload).? }),
                .tuple => |items| blk: {
                    const out = try allocator.alloc(TypeId, items.len);
                    defer allocator.free(out);
                    for (self.store.span(items), out) |item, *slot| slot.* = done.get(item).?;
                    break :blk try self.store.intern(.{ .tuple = out });
                },
                .function => |function| blk: {
                    const args = try allocator.alloc(TypeId, function.args.len);
                    defer allocator.free(args);
                    for (self.store.span(function.args), args) |arg, *slot| slot.* = done.get(arg).?;
                    break :blk try self.store.intern(.{ .function = .{ .args = args, .ret = done.get(function.ret).?, .effectful = function.effectful } });
                },
                .nominal => |nominal| blk: {
                    const args = try allocator.alloc(TypeId, nominal.args.len);
                    defer allocator.free(args);
                    for (self.store.nominalArgs(nominal), args) |arg, *slot| slot.* = done.get(arg).?;
                    var copy = nominal;
                    _ = &copy;
                    break :blk try self.store.intern(.{ .nominal = .{ .decl = nominal.decl, .args = args, .name = nominal.name, .is_opaque = nominal.is_opaque, .builtin = nominal.builtin, .has_backing = nominal.has_backing } });
                },
                .record => |record| blk: {
                    var fields: std.ArrayList(types.Field) = .empty;
                    defer fields.deinit(allocator);
                    for (self.store.fieldSpan(record.fields)) |field| try fields.append(allocator, .{ .name = field.name, .ty = done.get(field.ty).?, .kind = field.kind });
                    var ext: ?TypeId = if (record.ext) |ext| done.get(ext).? else null;
                    if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                        .empty_record => ext = null,
                        .record => |ext_record| {
                            try fields.appendSlice(allocator, self.store.fieldSpan(ext_record.fields));
                            ext = ext_record.ext;
                        },
                        .variable => {},
                        else => invariant("a record row's remainder substituted to a non-record type"),
                    };
                    if (fields.items.len == 0 and ext == null) break :blk try self.store.emptyRecord();
                    const store = &self.store;
                    std.mem.sort(types.Field, fields.items, store, struct {
                        fn lessThan(s: *types.Store, a: types.Field, b: types.Field) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk try self.store.intern(.{ .record = .{ .fields = fields.items, .ext = ext } });
                },
                .tag_union => |tag_union| blk: {
                    var tags: std.ArrayList(types.TagInput) = .empty;
                    defer tags.deinit(allocator);
                    var payloads: std.ArrayList(TypeId) = .empty;
                    defer payloads.deinit(allocator);
                    const own = self.store.tagSpan(tag_union.tags);
                    for (own) |tag| for (self.store.span(tag.payloads)) |payload| try payloads.append(allocator, done.get(payload).?);
                    var at: usize = 0;
                    for (own) |tag| {
                        try tags.append(allocator, .{ .name = tag.name, .payloads = payloads.items[at..][0..tag.payloads.len] });
                        at += tag.payloads.len;
                    }
                    var ext: ?TypeId = if (tag_union.ext) |ext| done.get(ext).? else null;
                    if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                        .tag_union => |ext_union| {
                            for (self.store.tagSpan(ext_union.tags)) |tag| {
                                const named = for (own) |known| {
                                    if (known.name == tag.name and known.payloads.len == tag.payloads.len) break true;
                                } else false;
                                if (!named) try tags.append(allocator, .{ .name = tag.name, .payloads = self.store.span(tag.payloads) });
                            }
                            ext = ext_union.ext;
                        },
                        .variable => {},
                        else => invariant("a tag row's remainder substituted to a non-tag type"),
                    };
                    const store = &self.store;
                    std.mem.sort(types.TagInput, tags.items, store, struct {
                        fn lessThan(s: *types.Store, a: types.TagInput, b: types.TagInput) bool {
                            return s.nameLessThan(a.name, b.name);
                        }
                    }.lessThan);
                    break :blk try self.store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = ext } });
                },
            };
            try done.put(allocator, frame.ty, result);
        }
        return done.get(root).?;
    }

    /// The override method entries a nominal type's descriptor carries, in
    /// `Methods` order (`is_eq`, `to_hash`, `to_inspect`): each method's
    /// dictionary entry and the types its scheme takes at `nominal_ty`.
    pub fn nominalOverrides(self: *Program, nominal_ty: TypeId) Allocator.Error!NominalOverrides {
        if (self.nominal_overrides.get(nominal_ty)) |memo| return memo;
        const nominal = switch (self.store.get(nominal_ty)) {
            .nominal => |value| value,
            else => invariant("asked for the overrides of a non-nominal type"),
        };
        var result: NominalOverrides = .{ null, null, null };
        if (nominal.has_backing) {
            const view = self.module(nominal.decl.module);
            const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
            const owner: static_dispatch.MethodOwner = if (nominal.builtin) |builtin| (if (builtin == .try_)
                .{ .nominal = declaration.nominal }
            else
                .{ .builtin = static_dispatch.builtinOwnerForCheckedBuiltin(builtin) }) else .{ .nominal = declaration.nominal };
            const registry = view.view.method_registry;
            const names = view.nameStore();
            const method_names = [_][]const u8{ "is_eq", "to_hash", "to_inspect" };
            for (method_names, &result) |method_name, *slot| {
                const method = names.lookupMethodName(method_name) orelse continue;
                const key: static_dispatch.MethodKey = .{ .owner = owner, .method = method };
                // Inspection calls only an eligible `to_inspect`.
                const target = if (std.mem.eql(u8, method_name, "to_inspect"))
                    (registry.inspectOverride(key) orelse continue).target
                else switch (registry.lookup(key) orelse continue) {
                    // A type whose method checking rejected compares and
                    // hashes as checked-error code.
                    .rejected => {
                        slot.* = .{ .entry = try self.crashDictionaryWorker(nominal.decl.module, .checked_error), .bound = &.{}, .dicts = &.{} };
                        continue;
                    },
                    .target => |found| found,
                };
                const template = switch (target.kind) {
                    .procedure => |procedure| procedure.template,
                    .local_proc, .structural => continue,
                };
                const worker_id = try self.templateRefWorker(template);
                const target_worker = self.worker(worker_id).*;
                if (target_worker.params.len == 0) invariant("a nominal method override takes no arguments");
                const related = try self.relateVariablesPartial(self.allocator, &.{.{ target_worker.params[0], nominal_ty }}, target_worker.scheme_vars.len);
                defer self.allocator.free(related);
                const bound = try self.arena.allocator().alloc(TypeId, related.len);
                for (related, bound, target_worker.scheme_vars) |ty, *out, variable| out.* = ty orelse try self.converter().sealedScheme(target_worker.module, variable);
                const dicts = try self.arena.allocator().alloc(StructuralDict, target_worker.dictionary_params.len);
                if (dicts.len != 0) {
                    const target_view = self.module(target_worker.module);
                    const target_key = target_worker.key.template;
                    const templates = target_view.view.checked_procedure_templates;
                    const params = poolSlice(static_dispatch.EvidenceParamRecord, templates.evidence_params_pool, templates.templates.items[@backingInt(target_key.template)].evidence_params);
                    const env: types.Env = .{ .module = target_worker.module, .vars = target_worker.scheme_vars, .bound = bound };
                    for (target_worker.dictionary_params, dicts) |param_index, *dict| {
                        const param = params[param_index];
                        const method_text = target_view.nameStore().methodNameText(param.method);
                        dict.* = .{
                            .method = std.meta.stringToEnum(StructuralMethod, method_text) orelse
                                invariantFmt("a nominal method override constrained by {s} is not yet lowered by Boxy", .{method_text}),
                            .ty = try self.converter().convert(target_worker.module, param.dispatcher_ty, env),
                        };
                    }
                }
                slot.* = .{ .entry = try self.methodEntryWorker(worker_id), .bound = bound, .dicts = dicts };
            }
        }
        try self.nominal_overrides.put(self.allocator, nominal_ty, result);
        return result;
    }

    /// The worker of an adapter function value (see `WorkerKey.adapter`).
    pub fn adapterWorker(self: *Program, outer: WorkerId, from: TypeId, to: TypeId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .adapter = .{ .outer = outer, .from = from, .to = to } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const outer_worker = self.worker(outer).*;
        const signature = switch (self.store.get(to)) {
            .function => |function| function,
            else => invariant("an adapted function value's type is not a function type"),
        };
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.adapter", @backingInt(id)),
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
            .capture_types = try self.arena.allocator().dupe(TypeId, &.{from}),
        });
        try self.worker_ids.put(self.allocator, key, id);
        try self.worklist.append(self.allocator, id);
        return id;
    }

    /// What the function value of lambda or closure expression `expr`
    /// captures: the binders of enclosing bodies whose values it needs,
    /// which are its own captures and those of the local procedures its
    /// body uses (`NestedProcSite.runtime_captures`).
    pub fn siteCaptures(self: *const Program, module_index: ModuleIndex, expr: checked.CheckedExprId) []const checked.CheckedCapture {
        const site_index = self.sites_by_expr.get(.{ .module = module_index, .expr = expr }) orelse
            invariant("a lambda or closure has no nested procedure site");
        const table = self.module(module_index).view.nested_proc_sites;
        const span = table.sites[site_index].runtime_captures;
        return table.runtime_captures[span.start..][0..span.len];
    }

    /// The worker evaluating default expression `expr` of `module_index`.
    pub fn defaultWorker(self: *Program, module_index: ModuleIndex, expr: checked.CheckedExprId) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .default = .{ .module = module_index, .expr = expr } };
        if (self.worker_ids.get(key)) |existing| return existing;
        const view = self.module(module_index);
        const ret = try self.converter().convert(module_index, view.bodies().expr(expr).ty, .{ .module = module_index, .vars = &.{}, .bound = &.{} });
        const id: WorkerId = @fromBackingInt(@intCast(self.workers.items.len));
        const proc = try self.result.store.addProcSpec(.{
            .name = self.result.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.programLocal("boxy.default", @backingInt(id)),
            .args = .empty(),
            .ret_layout = .zst,
        }, base.SourceLoc.none);
        try self.workers.append(self.allocator, .{
            .key = key,
            .proc = proc,
            .module = module_index,
            .scheme_vars = &.{},
            .dictionary_params = &.{},
            .params = &.{},
            .ret = ret,
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
            .to_inspect => .{ try self.arena.allocator().dupe(TypeId, &.{a}), try self.store.primitive(.str) },
        };
        const id = try self.addErasedWorker(key, module_index, &.{}, params, ret, "boxy.structural");
        self.worker(id).structural_descs = 1;
        return id;
    }

    /// The worker of a derived map with the checked selection `map`.
    pub fn structuralMapWorker(self: *Program, module_index: ModuleIndex, map: StructuralMap) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .structural_map = map };
        if (self.worker_ids.get(key)) |existing| return existing;
        const input = try self.store.variable(0);
        const output = try self.store.variable(1);
        const selected = try self.store.variable(2 + map.payload_index);
        const mapped = try self.store.variable(2 + map.arity);
        const transform = try self.store.intern(.{ .function = .{ .args = &.{selected}, .ret = mapped, .effectful = map.effectful } });
        const params = try self.arena.allocator().dupe(TypeId, &.{ input, transform });
        const id = try self.addErasedWorker(key, module_index, &.{}, params, output, "boxy.structural_map");
        self.worker(id).structural_descs = 3 + map.arity;
        return id;
    }

    /// The method of a crashing dictionary slot (see
    /// `WorkerKey.crash_dictionary`). It reads no arguments and never
    /// returns.
    pub fn crashDictionaryWorker(self: *Program, module_index: ModuleIndex, kind: CrashDictionary) Allocator.Error!WorkerId {
        const key: WorkerKey = .{ .crash_dictionary = kind };
        if (self.worker_ids.get(key)) |existing| return existing;
        return try self.addErasedWorker(key, module_index, &.{}, &.{}, try self.store.emptyRecord(), "boxy.crash_dictionary");
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
    /// The body of the procedure binding a use names, and the module that
    /// owns it. A hosted binding is its hosted template's direct body.
    pub fn procedureBinding(self: *const Program, binding: checked.ProcedureBindingRef) struct { module: ModuleIndex, body: checked.ProcedureBindingBody } {
        return switch (binding) {
            .top_level => |top| blk: {
                const owner = self.moduleByKey(top.artifact);
                break :blk .{ .module = owner.index, .body = owner.view.top_level_procedure_bindings.bindings.items[@backingInt(top.binding)].body };
            },
            .imported => |imported| blk: {
                const owner = self.moduleByKey(imported.artifact);
                const exported = self.exported_bindings.get(.{ .module = owner.index, .def = imported.def, .pattern = imported.pattern }) orelse
                    invariant("an imported procedure binding is not exported by its module");
                break :blk .{ .module = owner.index, .body = exported.body };
            },
            .platform_required => |required| blk: {
                const owner = self.moduleByKey(required.artifact);
                break :blk .{ .module = owner.index, .body = owner.view.top_level_procedure_bindings.bindings.items[@backingInt(required.procedure_binding)].body };
            },
            .hosted => |hosted| .{ .module = self.moduleByIdx(hosted.module_idx).index, .body = .{ .direct_template = .{ .proc_value = hosted.proc, .template = .{ .checked = hosted.template } } } },
        };
    }

    /// The checked template a binding's direct body names. Checking binds
    /// every procedure to a checked template or a synthetic one wrapping it;
    /// lifted templates exist only after post-check lifting.
    pub fn directTemplate(template: canonical.CallableProcedureTemplateRef) canonical.ProcedureTemplateRef {
        return switch (template) {
            .checked => |ref| ref,
            .synthetic => |synthetic| synthetic.template,
            .lifted => invariant("a checked procedure binding names a lifted template, which only post-check lifting creates"),
        };
    }

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
        program.exported_bindings.deinit(allocator);
        program.hosted.deinit(allocator);
        program.local_scopes.deinit(allocator);
        program.local_procs.deinit(allocator);
        program.by_identity.deinit(allocator);
        program.sites_by_expr.deinit(allocator);
        program.nominal_overrides.deinit(allocator);
        program.signature_slots.deinit(allocator);
        program.omitted_defaults.deinit(allocator);
        program.codec.deinit(allocator);
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
        for (view.view.exported_procedure_bindings.bindings) |*binding| {
            try program.exported_bindings.put(allocator, .{ .module = view.index, .def = binding.binding.def, .pattern = binding.binding.pattern }, binding);
        }
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
        try program.by_identity.put(allocator, view.view.module_identity.stable_hash, view.index);
        for (view.view.nested_proc_sites.sites, 0..) |site, site_index| {
            const site_expr = site.checked_expr orelse continue;
            try program.sites_by_expr.put(allocator, .{ .module = view.index, .expr = site_expr }, @intCast(site_index));
        }
        for (bodies.record_omitted_defaults) |omitted| {
            const name = try program.store.internName(view.nameStore().recordFieldLabelText(omitted.field_name));
            try program.omitted_defaults.put(allocator, .{ .module = view.index, .expr = omitted.expr, .name = name }, omitted.default);
        }
        // A local procedure's uses all reach it through its binding, so its
        // own scheme's arguments are passed per call.
        for (view.view.resolved_value_refs.records) |record| {
            const local = switch (record.ref) {
                .local_proc => |local| local,
                else => continue,
            };
            try program.local_procs.put(allocator, .{ .module = view.index, .expr = local.expr }, {});
            switch (bodies.expr(local.expr).data) {
                .closure => |closure| try program.local_procs.put(allocator, .{ .module = view.index, .expr = closure.lambda }, {}),
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

    if (input.proc_debug_names) {
        for (program.workers.items) |worker| {
            const name: []const u8 = switch (worker.key) {
                .codec => |piece| switch (program.codec.pieces.items[piece].kind) {
                    inline else => |kind| "boxy.codec." ++ @tagName(kind),
                },
                inline else => |_, kind| "boxy." ++ @tagName(kind),
            };
            try program.result.store.setProcDebugName(worker.proc, name);
        }
    }
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
        .direct_template => |direct| Program.directTemplate(direct.template),
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
