//! Executable MIR construction state.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const types = @import("types");
const symbol_mod = @import("symbol");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const LambdaSolved = @import("../lambda_solved/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const Layouts = @import("layouts.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;
const repr = LambdaSolved.Representation;

/// Public `ArtifactViews` declaration.
pub const ArtifactViews = struct {
    root: ?checked_artifact.LoweringModuleView = null,
    imports: []const checked_artifact.ImportedModuleView = &.{},
};

/// Public `PublishedExecutableTypeRequest` declaration.
pub const PublishedExecutableTypeRequest = struct {
    ty: checked_artifact.ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

const MaterializationStores = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    canonical_names: *const canonical.CanonicalNameStore,
    plans: *const checked_artifact.CompileTimePlanStore,
    values: *const checked_artifact.CompileTimeValueStore,
};

fn artifactRefFromKey(key: checked_artifact.CheckedModuleArtifactKey) canonical.ArtifactRef {
    return .{ .bytes = key.bytes };
}

const PublishedTransformContext = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    materialization: MaterializationStores,
    executable_type_payloads: *const checked_artifact.ExecutableTypePayloadStore,
    executable_value_transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
};

fn constructionSlotBridgeForProgram(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    source_ty: Type.TypeId,
    target_ty: Type.TypeId,
) Allocator.Error!Ast.BridgeId {
    if (source_ty == target_ty) {
        return switch (program.types.getType(source_ty)) {
            .placeholder => executableInvariant("executable construction bridge saw placeholder type"),
            .link => executableInvariant("executable construction bridge saw unresolved link type"),
            .primitive => try ast.addBridgePlan(.direct),
            .nominal, .box, .callable_set, .erased_fn => try ast.addBridgePlan(.nominal_reinterpret),
            .list => try ast.addBridgePlan(.list_reinterpret),
            .tuple => |items| try ast.addBridgePlan(.{ .struct_ = try constructionSlotStructBridgeForProgram(allocator, program, ast, items, items) }),
            .record => |record| try ast.addBridgePlan(.{ .struct_ = try constructionSlotRecordBridgeForProgram(allocator, program, ast, record, record) }),
            .tag_union => |tag_union| try ast.addBridgePlan(.{ .tag_union = try constructionSlotTagUnionBridgeForProgram(allocator, program, ast, tag_union, tag_union) }),
            .vacant_callable_slot => try ast.addBridgePlan(.zst),
        };
    }

    const source = program.types.getType(source_ty);
    const target = program.types.getType(target_ty);
    return try ast.addBridgePlan(switch (source) {
        .placeholder => executableInvariant("executable construction bridge saw placeholder source type"),
        .link => executableInvariant("executable construction bridge saw unresolved source link"),
        .primitive => |source_prim| switch (target) {
            .primitive => |target_prim| blk: {
                if (source_prim != target_prim) executableInvariant("executable construction bridge crossed primitive types");
                break :blk .direct;
            },
            else => executableInvariant("executable construction bridge crossed primitive/non-primitive types"),
        },
        .nominal => |source_nominal| switch (target) {
            .nominal => |target_nominal| blk: {
                if (source_nominal.nominal.module_name == target_nominal.nominal.module_name and
                    source_nominal.nominal.type_name == target_nominal.nominal.type_name)
                {
                    break :blk .nominal_reinterpret;
                }
                executableInvariant("executable construction bridge crossed distinct nominal types");
            },
            else => .nominal_reinterpret,
        },
        .list => switch (target) {
            .list => .list_reinterpret,
            .nominal => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed list/non-list types"),
        },
        .box => switch (target) {
            .box, .nominal => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed box/non-box types"),
        },
        .tuple => |source_items| switch (target) {
            .tuple => |target_items| .{ .struct_ = try constructionSlotStructBridgeForProgram(allocator, program, ast, source_items, target_items) },
            .nominal => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed tuple/non-tuple types"),
        },
        .record => |source_record| switch (target) {
            .record => |target_record| .{ .struct_ = try constructionSlotRecordBridgeForProgram(allocator, program, ast, source_record, target_record) },
            .nominal => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed record/non-record types"),
        },
        .tag_union => |source_union| switch (target) {
            .tag_union => |target_union| .{ .tag_union = try constructionSlotTagUnionBridgeForProgram(allocator, program, ast, source_union, target_union) },
            .nominal => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed tag-union/non-tag-union types"),
        },
        .callable_set => |source_callable| switch (target) {
            .callable_set => |target_callable| blk: {
                if (!repr.callableSetKeyEql(source_callable.key, target_callable.key)) {
                    executableInvariant("executable construction bridge crossed callable-set keys");
                }
                break :blk .nominal_reinterpret;
            },
            .nominal => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed callable-set/non-callable-set types"),
        },
        .erased_fn => switch (target) {
            .erased_fn => .nominal_reinterpret,
            else => executableInvariant("executable construction bridge crossed erased-fn/non-erased-fn types"),
        },
        .vacant_callable_slot => switch (target) {
            .vacant_callable_slot => .zst,
            else => executableInvariant("executable construction bridge crossed vacant/non-vacant callable-slot types"),
        },
    });
}

fn constructionSlotStructBridgeForProgram(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    source_items: []const Type.TypeId,
    target_items: []const Type.TypeId,
) Allocator.Error!Ast.Span(Ast.BridgeId) {
    if (source_items.len != target_items.len) executableInvariant("executable construction struct bridge arity mismatch");
    if (source_items.len == 0) return Ast.Span(Ast.BridgeId).empty();
    const children = try allocator.alloc(Ast.BridgeId, source_items.len);
    defer allocator.free(children);
    for (source_items, target_items, 0..) |source, target, i| {
        children[i] = try constructionSlotBridgeForProgram(allocator, program, ast, source, target);
    }
    return try ast.addBridgePlanSpan(children);
}

fn constructionSlotRecordBridgeForProgram(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    source: Type.RecordType,
    target: Type.RecordType,
) Allocator.Error!Ast.Span(Ast.BridgeId) {
    if (source.fields.len != target.fields.len) executableInvariant("executable construction record bridge arity mismatch");
    if (source.fields.len == 0) return Ast.Span(Ast.BridgeId).empty();
    const children = try allocator.alloc(Ast.BridgeId, source.fields.len);
    defer allocator.free(children);
    for (target.fields, 0..) |target_field, i| {
        const target_label = program.row_shapes.recordField(target_field.field).label;
        const source_field = recordFieldForLabel(program, source, target_label);
        children[i] = try constructionSlotBridgeForProgram(allocator, program, ast, source_field.ty, target_field.ty);
    }
    return try ast.addBridgePlanSpan(children);
}

fn constructionSlotTagUnionBridgeForProgram(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    source: Type.TagUnionType,
    target: Type.TagUnionType,
) Allocator.Error!Ast.Span(Ast.BridgeId) {
    if (source.tags.len != target.tags.len) executableInvariant("executable construction tag-union bridge arity mismatch");
    if (source.tags.len == 0) return Ast.Span(Ast.BridgeId).empty();
    const children = try allocator.alloc(Ast.BridgeId, target.tags.len);
    defer allocator.free(children);
    for (target.tags, 0..) |target_tag, i| {
        const target_label = program.row_shapes.tag(target_tag.tag).label;
        const source_tag = tagTypeForLabel(program, source, target_label);
        children[i] = try constructionSlotTagPayloadBridgeForProgram(allocator, program, ast, source_tag, target_tag);
    }
    return try ast.addBridgePlanSpan(children);
}

fn constructionSlotTagPayloadBridgeForProgram(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    source: Type.TagType,
    target: Type.TagType,
) Allocator.Error!Ast.BridgeId {
    if (source.payloads.len != target.payloads.len) executableInvariant("executable construction tag payload bridge arity mismatch");
    if (source.payloads.len == 0) return try ast.addBridgePlan(.zst);
    if (source.payloads.len == 1) {
        return try constructionSlotBridgeForProgram(allocator, program, ast, source.payloads[0].ty, target.payloads[0].ty);
    }
    const source_payloads = try allocator.alloc(Type.TypeId, source.payloads.len);
    defer allocator.free(source_payloads);
    const target_payloads = try allocator.alloc(Type.TypeId, target.payloads.len);
    defer allocator.free(target_payloads);
    for (source.payloads) |payload| {
        source_payloads[@intCast(program.row_shapes.tagPayload(payload.payload).logical_index)] = payload.ty;
    }
    for (target.payloads) |payload| {
        target_payloads[@intCast(program.row_shapes.tagPayload(payload.payload).logical_index)] = payload.ty;
    }
    return try ast.addBridgePlan(.{ .struct_ = try constructionSlotStructBridgeForProgram(allocator, program, ast, source_payloads, target_payloads) });
}

fn addTupleItemExprSpanForConstruction(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    exprs: []const Ast.ExprId,
    target_tys: []const Type.TypeId,
) Allocator.Error!Ast.Span(Ast.TupleItemExpr) {
    if (exprs.len == 0) return Ast.Span(Ast.TupleItemExpr).empty();
    if (exprs.len != target_tys.len) executableInvariant("executable tuple construction helper arity mismatch");
    const items = try allocator.alloc(Ast.TupleItemExpr, exprs.len);
    defer allocator.free(items);
    for (exprs, 0..) |expr_id, i| {
        const expr = ast.getExpr(expr_id);
        items[i] = .{
            .expr = expr_id,
            .ty = target_tys[i],
            .value = expr.value,
            .bridge = try constructionSlotBridgeForProgram(allocator, program, ast, expr.ty, target_tys[i]),
        };
    }
    return try ast.addTupleItemExprSpan(items);
}

fn addListItemExprSpanForConstruction(
    allocator: Allocator,
    program: *const Program,
    ast: *Ast.Store,
    exprs: []const Ast.ExprId,
    target_ty: Type.TypeId,
) Allocator.Error!Ast.Span(Ast.ListItemExpr) {
    if (exprs.len == 0) return Ast.Span(Ast.ListItemExpr).empty();
    const items = try allocator.alloc(Ast.ListItemExpr, exprs.len);
    defer allocator.free(items);
    for (exprs, 0..) |expr_id, i| {
        const expr = ast.getExpr(expr_id);
        items[i] = .{
            .expr = expr_id,
            .ty = target_ty,
            .value = expr.value,
            .bridge = try constructionSlotBridgeForProgram(allocator, program, ast, expr.ty, target_ty),
        };
    }
    return try ast.addListItemExprSpan(items);
}

/// Public `Proc` declaration.
pub const Proc = struct {
    executable_proc: Ast.ExecutableProcId,
    origin: Ast.ProcOrigin,
    body: Ast.DefId,
};

/// Public `ErasedAdapterProcReservation` declaration.
pub const ErasedAdapterProcReservation = struct {
    key: repr.ErasedAdapterKey,
    payload_solve_session: ?repr.RepresentationSolveSessionId = null,
    payload_artifact_owner: ?checked_artifact.CheckedModuleArtifactKey = null,
    artifact_descriptor_owner: ?checked_artifact.CheckedModuleArtifactKey = null,
    member_targets: []const repr.ExecutableSpecializationKey = &.{},
    branches: []const repr.FiniteSetEraseAdapterBranchPlan = &.{},
    published_branches: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan = &.{},
    executable_proc: Ast.ExecutableProcId,
};

/// Public `ErasedDirectProcAdapterReservation` declaration.
pub const ErasedDirectProcAdapterReservation = struct {
    code: canonical.ErasedDirectProcCodeRef,
    sig_key: repr.ErasedFnSigKey,
    target_specialization: repr.ExecutableSpecializationKey,
    target_instance: repr.ProcRepresentationInstanceId,
    solve_session: repr.RepresentationSolveSessionId,
    arg_transforms: []const repr.ValueTransformBoundaryId = &.{},
    executable_proc: Ast.ExecutableProcId,
};

const ErasedAdapterRequirement = struct {
    key: repr.ErasedAdapterKey,
    payload_solve_session: ?repr.RepresentationSolveSessionId = null,
    payload_artifact_owner: ?checked_artifact.CheckedModuleArtifactKey = null,
    artifact_descriptor_owner: ?checked_artifact.CheckedModuleArtifactKey = null,
    member_targets: []const repr.ExecutableSpecializationKey = &.{},
    branches: []const repr.FiniteSetEraseAdapterBranchPlan = &.{},
    published_branches: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan = &.{},
};

const ErasedDirectProcAdapterRequirement = struct {
    code: canonical.ErasedDirectProcCodeRef,
    sig_key: repr.ErasedFnSigKey,
    target_specialization: repr.ExecutableSpecializationKey,
    target_instance: repr.ProcRepresentationInstanceId,
    solve_session: repr.RepresentationSolveSessionId,
    arg_transforms: []const repr.ValueTransformBoundaryId = &.{},
};

const ConstInstanceAdapterVisitKey = struct {
    owner: [32]u8,
    instance: checked_artifact.ConstInstanceId,
};

/// Public `Program` declaration.
pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    concrete_source_types: ConcreteSourceType.Store,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    erased_direct_proc_adapters: std.ArrayList(ErasedDirectProcAdapterReservation),
    erased_adapter_procs: std.ArrayList(ErasedAdapterProcReservation),
    root_procs: std.ArrayList(Ast.ExecutableProcId),
    root_metadata: std.ArrayList(ids.RootMetadata),
    lowered_session_types_by_key: std.AutoHashMap(repr.CanonicalExecValueTypeKey, Type.TypeId),
    callable_set_descriptors: []const repr.CanonicalCallableSetDescriptor = &.{},
    artifact_views: ArtifactViews = .{},
    layouts: ?Layouts.Layouts = null,

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .erased_direct_proc_adapters = .empty,
            .erased_adapter_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
            .lowered_session_types_by_key = std.AutoHashMap(repr.CanonicalExecValueTypeKey, Type.TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        if (self.layouts) |*layouts| layouts.deinit();
        self.lowered_session_types_by_key.deinit();
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        for (self.erased_adapter_procs.items) |*proc| deinitErasedAdapterProcReservation(self.allocator, proc);
        self.erased_adapter_procs.deinit(self.allocator);
        for (self.erased_direct_proc_adapters.items) |*proc| deinitErasedDirectProcAdapterReservation(self.allocator, proc);
        self.erased_direct_proc_adapters.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.concrete_source_types.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

/// Public `run` function.
pub fn run(
    allocator: Allocator,
    solved: LambdaSolved.Solve.Program,
    artifact_views: ArtifactViews,
    callable_set_descriptors: []const repr.CanonicalCallableSetDescriptor,
) Allocator.Error!Program {
    var input = solved;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.callable_set_descriptors = callable_set_descriptors;
    program.artifact_views = artifact_views;
    program.canonical_names = input.canonical_names;
    input.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.concrete_source_types = input.concrete_source_types;
    input.concrete_source_types = ConcreteSourceType.Store.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    var proc_exec_map = std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId).init(allocator);
    defer proc_exec_map.deinit();
    const normal_proc_count = input.procs.items.len;
    const executable_synthetic_proc_count = input.executable_synthetic_procs.items.len;
    var erased_direct_adapter_requirements = try collectErasedDirectProcAdapterRequirements(allocator, &input);
    defer {
        for (erased_direct_adapter_requirements.items) |*requirement| deinitErasedDirectProcAdapterRequirement(allocator, requirement);
        erased_direct_adapter_requirements.deinit(allocator);
    }

    var erased_adapter_requirements = try collectErasedAdapterRequirements(allocator, &input, program.artifact_views);
    defer {
        for (erased_adapter_requirements.items) |*requirement| deinitErasedAdapterRequirement(allocator, requirement);
        erased_adapter_requirements.deinit(allocator);
    }
    const erased_direct_adapter_proc_count = erased_direct_adapter_requirements.items.len;
    const erased_adapter_proc_count = erased_adapter_requirements.items.len;
    const total_proc_count = normal_proc_count + executable_synthetic_proc_count + erased_direct_adapter_proc_count + erased_adapter_proc_count;

    try proc_exec_map.ensureTotalCapacity(@intCast(input.procs.items.len + input.executable_synthetic_proc_instances.items.len));
    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        proc_exec_map.putAssumeCapacity(proc.representation_instance, executable_proc);
    }
    for (input.executable_synthetic_proc_instances.items) |synthetic_instance| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + synthetic_instance.synthetic_index)));
        proc_exec_map.putAssumeCapacity(synthetic_instance.representation_instance, executable_proc);
    }
    try program.erased_direct_proc_adapters.ensureTotalCapacity(allocator, erased_direct_adapter_proc_count);
    for (erased_direct_adapter_requirements.items, 0..) |requirement, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + executable_synthetic_proc_count + i)));
        var target_specialization = try repr.cloneExecutableSpecializationKey(allocator, requirement.target_specialization);
        errdefer repr.deinitExecutableSpecializationKey(allocator, &target_specialization);
        program.erased_direct_proc_adapters.appendAssumeCapacity(.{
            .code = requirement.code,
            .sig_key = requirement.sig_key,
            .target_specialization = target_specialization,
            .target_instance = requirement.target_instance,
            .solve_session = requirement.solve_session,
            .arg_transforms = requirement.arg_transforms,
            .executable_proc = executable_proc,
        });
    }

    try program.erased_adapter_procs.ensureTotalCapacity(allocator, erased_adapter_proc_count);
    for (erased_adapter_requirements.items, 0..) |requirement, i| {
        const proc_offset = normal_proc_count + executable_synthetic_proc_count + erased_direct_adapter_proc_count + i;
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(proc_offset)));
        const member_targets = try cloneExecutableSpecializationKeySlice(allocator, requirement.member_targets);
        errdefer deinitExecutableSpecializationKeySlice(allocator, member_targets);
        const branches = try repr.cloneFiniteSetEraseAdapterBranches(allocator, requirement.branches);
        errdefer repr.deinitFiniteSetEraseAdapterBranches(allocator, branches);
        const published_branches = try clonePublishedFiniteSetEraseAdapterBranches(allocator, requirement.published_branches);
        errdefer deinitPublishedFiniteSetEraseAdapterBranches(allocator, published_branches);
        program.erased_adapter_procs.appendAssumeCapacity(.{
            .key = requirement.key,
            .payload_solve_session = requirement.payload_solve_session,
            .payload_artifact_owner = requirement.payload_artifact_owner,
            .artifact_descriptor_owner = requirement.artifact_descriptor_owner,
            .member_targets = member_targets,
            .branches = branches,
            .published_branches = published_branches,
            .executable_proc = executable_proc,
        });
    }

    try program.procs.ensureTotalCapacity(allocator, total_proc_count);
    var type_lowerer = TypeLowerer.init(allocator, &input.types, &program.types, &program.row_shapes);
    defer type_lowerer.deinit();

    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        const value_store = &input.value_stores.items[@intFromEnum(proc.representation_instance)];
        const proc_instance = &input.proc_instances.items[@intFromEnum(proc.representation_instance)];
        const representation_store = &input.solve_sessions.items[@intFromEnum(proc_instance.solve_session)].representation_store;
        var builder = BodyBuilder{
            .allocator = allocator,
            .program = &program,
            .input = &input.ast,
            .output = &program.ast,
            .canonical_names = &program.canonical_names,
            .type_lowerer = &type_lowerer,
            .session_type_lowerer = SessionTypeLowerer.init(allocator, &representation_store.session_executable_type_payloads, &program.types, &program.lowered_session_types_by_key),
            .value_store = value_store,
            .representation_store = representation_store,
            .callable_set_descriptors = program.callable_set_descriptors,
            .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
            .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
            .executable_proc = executable_proc,
            .source_proc = proc.proc,
            .representation_instance = proc.representation_instance,
            .proc_instance = proc_instance,
            .proc_instances = input.proc_instances.items,
            .solve_sessions = input.solve_sessions.items,
            .value_stores = input.value_stores.items,
            .proc_exec_map = &proc_exec_map,
            .erased_adapter_procs = program.erased_adapter_procs.items,
        };
        defer builder.deinit();

        program.procs.appendAssumeCapacity(.{
            .executable_proc = executable_proc,
            .origin = .{ .source = proc.proc },
            .body = try builder.lowerDef(proc.body),
        });
    }
    for (input.executable_synthetic_procs.items, 0..) |synthetic, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + i)));
        program.procs.appendAssumeCapacity(.{
            .executable_proc = executable_proc,
            .origin = .{ .source = synthetic.source_proc },
            .body = try lowerExecutableSyntheticProc(allocator, &program, synthetic, executable_proc),
        });
    }
    for (program.erased_direct_proc_adapters.items) |adapter| {
        program.procs.appendAssumeCapacity(.{
            .executable_proc = adapter.executable_proc,
            .origin = .{ .erased_direct_proc_adapter = adapter.code },
            .body = try lowerErasedDirectProcAdapterProc(allocator, &program, &input, &type_lowerer, &proc_exec_map, adapter, adapter.executable_proc),
        });
    }
    for (program.erased_adapter_procs.items) |adapter| {
        program.procs.appendAssumeCapacity(.{
            .executable_proc = adapter.executable_proc,
            .origin = .{ .erased_adapter = adapter.key },
            .body = try lowerErasedFiniteSetAdapterProc(allocator, &program, &input, &type_lowerer, &proc_exec_map, adapter, adapter.executable_proc),
        });
    }

    if (input.root_instances.items.len != input.root_metadata.items.len) {
        executableInvariant("executable build root instance count differs from root metadata");
    }
    for (input.root_instances.items, input.root_metadata.items) |root_instance, metadata| {
        const executable_root = proc_exec_map.get(root_instance) orelse {
            debug.invariant(false, "executable build invariant violated: root representation instance has no executable proc");
            unreachable;
        };
        try program.root_procs.append(allocator, executable_root);
        try program.root_metadata.append(allocator, metadata);
    }

    if (debug.enabled()) verifyExecutableProgram(&program);

    input.deinit();
    return program;
}

/// Public `ensurePublishedExecutableTypeRequests` function.
pub fn ensurePublishedExecutableTypeRequests(
    allocator: Allocator,
    program: *Program,
    requests: []const PublishedExecutableTypeRequest,
) Allocator.Error!void {
    if (requests.len == 0) return;

    for (requests) |request| {
        if (program.lowered_session_types_by_key.get(request.key) != null) continue;

        const artifact_key = checked_artifact.CheckedModuleArtifactKey{ .bytes = request.ty.artifact.bytes };
        const context = resolvePublishedTransformContextInArtifactViews(program.artifact_views, artifact_key);
        const ty = blk: {
            var published_types = PublishedTypeLowerer.init(
                allocator,
                context.executable_type_payloads,
                context.materialization.canonical_names,
                &program.canonical_names,
                &program.types,
                &program.row_shapes,
                &program.lowered_session_types_by_key,
            );
            defer published_types.deinit();
            break :blk try published_types.lower(request.ty, request.key);
        };
        if (program.lowered_session_types_by_key.get(request.key) != ty) {
            executableInvariant("executable published type request did not intern by canonical key");
        }
    }
}

fn verifyExecutableProgram(program: *const Program) void {
    verifyExecutableTypes(&program.types);
}

fn verifyExecutableTypes(types_store: *const Type.Store) void {
    const len = types_store.types.items.len;
    for (types_store.types.items) |content| {
        switch (content) {
            .placeholder => debug.invariant(false, "executable MIR type store contains an unresolved placeholder"),
            .link => |next| verifyExecutableTypeRef(next, len),
            .primitive,
            .vacant_callable_slot,
            => {},
            .nominal => |nominal| verifyExecutableTypeRef(nominal.backing, len),
            .list,
            .box,
            => |child| verifyExecutableTypeRef(child, len),
            .tuple => |items| {
                for (items) |item| verifyExecutableTypeRef(item, len);
            },
            .record => |record| {
                for (record.fields) |field| verifyExecutableTypeRef(field.ty, len);
            },
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    for (tag.payloads) |payload| verifyExecutableTypeRef(payload.ty, len);
                }
            },
            .callable_set => |callable_set| {
                for (callable_set.members) |member| {
                    if (member.payload_ty) |payload_ty| verifyExecutableTypeRef(payload_ty, len);
                }
            },
            .erased_fn => |erased_fn| {
                if (erased_fn.capture_ty) |capture_ty| verifyExecutableTypeRef(capture_ty, len);
            },
        }
    }
}

fn verifyExecutableTypeRef(ref: Type.TypeId, len: usize) void {
    if (@intFromEnum(ref) >= len) {
        debug.invariant(false, "executable MIR type store contains an out-of-range type reference");
    }
}

fn programCallableSetDescriptor(
    program: *const Program,
    key: repr.CanonicalCallableSetKey,
) ?*const repr.CanonicalCallableSetDescriptor {
    return callableSetDescriptorFromSlice(program.callable_set_descriptors, key);
}

fn callableSetDescriptorFromSlice(
    descriptors: []const repr.CanonicalCallableSetDescriptor,
    key: repr.CanonicalCallableSetKey,
) ?*const repr.CanonicalCallableSetDescriptor {
    for (descriptors) |*descriptor| {
        if (repr.callableSetKeyEql(descriptor.key, key)) return descriptor;
    }
    return null;
}

fn callableSetDescriptorMember(
    descriptor: *const repr.CanonicalCallableSetDescriptor,
    member_id: repr.CallableSetMemberId,
) ?*const repr.CanonicalCallableSetMember {
    for (descriptor.members) |*member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn programCallableSetMember(
    program: *const Program,
    key: repr.CanonicalCallableSetKey,
    member_id: repr.CallableSetMemberId,
) ?*const repr.CanonicalCallableSetMember {
    const descriptor = programCallableSetDescriptor(program, key) orelse return null;
    for (descriptor.members) |*member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn collectErasedDirectProcAdapterRequirements(
    allocator: Allocator,
    input: *const LambdaSolved.Solve.Program,
) Allocator.Error!std.ArrayList(ErasedDirectProcAdapterRequirement) {
    var adapters = std.ArrayList(ErasedDirectProcAdapterRequirement).empty;
    errdefer {
        for (adapters.items) |*adapter| deinitErasedDirectProcAdapterRequirement(allocator, adapter);
        adapters.deinit(allocator);
    }

    for (input.solve_sessions.items, 0..) |*session, session_index| {
        for (session.representation_store.callable_emission_plans) |plan| {
            switch (plan) {
                .erase_proc_value => |erase| try appendErasedDirectProcAdapterRequirement(
                    allocator,
                    &adapters,
                    .{ .solve_session = @enumFromInt(@as(u32, @intCast(session_index))) },
                    erase,
                ),
                .already_erased,
                .finite,
                .erase_finite_set,
                .pending_proc_value,
                => {},
            }
        }
    }

    return adapters;
}

fn collectErasedAdapterRequirements(
    allocator: Allocator,
    input: *const LambdaSolved.Solve.Program,
    artifact_views: ArtifactViews,
) Allocator.Error!std.ArrayList(ErasedAdapterRequirement) {
    var adapters = std.ArrayList(ErasedAdapterRequirement).empty;
    errdefer {
        for (adapters.items) |*adapter| deinitErasedAdapterRequirement(allocator, adapter);
        adapters.deinit(allocator);
    }
    var visited_const_instances = std.AutoHashMap(ConstInstanceAdapterVisitKey, void).init(allocator);
    defer visited_const_instances.deinit();

    for (input.solve_sessions.items, 0..) |*session, raw_session| {
        const payload_solve_session: repr.RepresentationSolveSessionId = @enumFromInt(@as(u32, @intCast(raw_session)));
        for (session.representation_store.callable_emission_plans) |plan| {
            switch (plan) {
                .erase_finite_set => |erase| try appendErasedAdapterRequirement(allocator, &adapters, .{
                    .key = erase.adapter,
                    .payload_solve_session = payload_solve_session,
                    .member_targets = try cloneExecutableSpecializationKeySlice(allocator, erase.member_targets),
                    .branches = try repr.cloneFiniteSetEraseAdapterBranches(allocator, erase.branches),
                }),
                .already_erased,
                .finite,
                .erase_proc_value,
                .pending_proc_value,
                => {},
            }
        }
        for (session.representation_store.session_value_transforms.plans.items) |plan| {
            try collectSessionValueTransformAdapters(
                allocator,
                &adapters,
                artifact_views,
                input,
                session,
                payload_solve_session,
                plan,
            );
        }
    }

    for (input.ast.exprs.items) |expr| {
        switch (expr.data) {
            .const_instance => |const_instance| try collectConstInstanceAdapters(
                allocator,
                &adapters,
                artifact_views,
                &visited_const_instances,
                const_instance,
            ),
            .const_ref => executableInvariant("executable adapter collection reached non-runnable compile-time dependency const_ref"),
            .pending_local_root => executableInvariant("executable adapter collection reached summary-only pending local root"),
            else => {},
        }
    }

    for (input.executable_synthetic_procs.items) |synthetic| {
        switch (synthetic.body) {
            .erased_promoted_wrapper => |erased| {
                try collectErasedCodeRefAdapter(
                    allocator,
                    &adapters,
                    erased.code,
                    synthetic.artifact,
                    try cloneExecutableSpecializationKeySlice(allocator, erased.finite_adapter_member_targets),
                    try clonePublishedFiniteSetEraseAdapterBranches(allocator, erased.finite_adapter_branches),
                );
                try collectErasedCaptureMaterializationAdapters(
                    allocator,
                    &adapters,
                    artifact_views,
                    &visited_const_instances,
                    synthetic.comptime_plans,
                    erased.capture,
                );
                switch (erased.hidden_capture_arg) {
                    .none => {},
                    .materialized_capture => |capture| try collectErasedCaptureMaterializationAdapters(
                        allocator,
                        &adapters,
                        artifact_views,
                        &visited_const_instances,
                        synthetic.comptime_plans,
                        capture,
                    ),
                }
            },
        }
    }

    return adapters;
}

fn collectErasedCodeRefAdapter(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    code: canonical.ErasedCallableCodeRef,
    artifact_descriptor_owner: ?checked_artifact.CheckedModuleArtifactKey,
    member_targets: []const repr.ExecutableSpecializationKey,
    published_branches: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan,
) Allocator.Error!void {
    switch (code) {
        .direct_proc_value => {
            if (member_targets.len != 0) {
                executableInvariant("direct erased callable code carried finite adapter member targets");
            }
            if (published_branches.len != 0) {
                executableInvariant("direct erased callable code carried finite adapter branches");
            }
            deinitExecutableSpecializationKeySlice(allocator, member_targets);
            deinitPublishedFiniteSetEraseAdapterBranches(allocator, published_branches);
        },
        .finite_set_adapter => |adapter| try appendErasedAdapterRequirement(allocator, adapters, .{
            .key = adapter,
            .payload_artifact_owner = artifact_descriptor_owner,
            .artifact_descriptor_owner = artifact_descriptor_owner,
            .member_targets = member_targets,
            .published_branches = published_branches,
        }),
    }
}

fn collectSessionValueTransformAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    input: *const LambdaSolved.Solve.Program,
    session: *const repr.RepresentationSolveSession,
    payload_solve_session: repr.RepresentationSolveSessionId,
    plan: repr.SessionExecutableValueTransformPlan,
) Allocator.Error!void {
    switch (plan.op) {
        .identity,
        .already_erased_callable,
        => {},
        .callable_to_erased => |callable| switch (callable) {
            .finite_value => |finite| try appendErasedAdapterRequirement(allocator, adapters, .{
                .key = finite.adapter,
                .payload_solve_session = payload_solve_session,
                .member_targets = try cloneExecutableSpecializationKeySlice(allocator, finite.member_targets),
                .branches = try repr.cloneFiniteSetEraseAdapterBranches(allocator, finite.branches),
            }),
            .proc_value => {},
        },
        .record => |fields| for (fields) |field| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, field.transform);
        },
        .tuple => |items| for (items) |item| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, item.transform);
        },
        .tag_union => |cases| for (cases) |case| {
            for (case.payloads) |payload| {
                try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, payload.transform);
            }
        },
        .nominal => |nominal| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, nominal.backing),
        .list => |list| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, list.elem),
        .box_payload => |box| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, box.payload),
        .structural_bridge => |bridge| try collectSessionStructuralBridgeAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, bridge),
    }
}

fn collectExecutableValueTransformRefAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    input: *const LambdaSolved.Solve.Program,
    session: *const repr.RepresentationSolveSession,
    payload_solve_session: repr.RepresentationSolveSessionId,
    transform: checked_artifact.ExecutableValueTransformRef,
) Allocator.Error!void {
    const store = &session.representation_store;
    switch (transform) {
        .session => |id| try collectSessionValueTransformAdapters(
            allocator,
            adapters,
            artifact_views,
            input,
            session,
            payload_solve_session,
            store.sessionExecutableValueTransform(id),
        ),
        .published => |published| {
            const context = resolvePublishedTransformContextInArtifactViews(artifact_views, published.artifact);
            try collectPublishedValueTransformAdapters(
                allocator,
                adapters,
                artifact_views,
                context.artifact,
                context.executable_value_transforms,
                published.transform,
            );
        },
    }
}

fn collectSessionStructuralBridgeAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    input: *const LambdaSolved.Solve.Program,
    session: *const repr.RepresentationSolveSession,
    payload_solve_session: repr.RepresentationSolveSessionId,
    bridge: repr.SessionExecutableStructuralBridgePlan,
) Allocator.Error!void {
    switch (bridge) {
        .direct,
        .zst,
        .list_reinterpret,
        .nominal_reinterpret,
        => {},
        .box_unbox => |child| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, child),
        .box_box => |child| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, child),
        .singleton_to_tag_union => |singleton| if (singleton.value_transform) |child| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, child);
        },
        .tag_union_to_singleton => |singleton| if (singleton.value_transform) |child| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, input, session, payload_solve_session, child);
        },
    }
}

fn collectPublishedValueTransformAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    transform_id: checked_artifact.ExecutableValueTransformPlanId,
) Allocator.Error!void {
    const plan = transforms.get(transform_id);
    switch (plan.op) {
        .identity,
        .already_erased_callable,
        => {},
        .callable_to_erased => |callable| switch (callable) {
            .finite_value => |finite| try appendErasedAdapterRequirement(allocator, adapters, .{
                .key = finite.adapter_key,
                .payload_artifact_owner = owner_artifact,
                .artifact_descriptor_owner = owner_artifact,
                .published_branches = try clonePublishedFiniteSetEraseAdapterBranches(allocator, finite.adapter_branches),
            }),
            .proc_value => {},
        },
        .record => |fields| for (fields) |field| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, field.transform);
        },
        .tuple => |items| for (items) |item| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, item.transform);
        },
        .tag_union => |cases| for (cases) |case| {
            for (case.payloads) |payload| {
                try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, payload.transform);
            }
        },
        .nominal => |nominal| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, nominal.backing),
        .list => |list| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, list.elem),
        .box_payload => |box| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, box.payload),
        .structural_bridge => |bridge| try collectPublishedStructuralBridgeAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, bridge),
    }
}

fn collectPublishedStructuralBridgeAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    bridge: checked_artifact.ExecutableStructuralBridgePlan,
) Allocator.Error!void {
    switch (bridge) {
        .direct,
        .zst,
        .list_reinterpret,
        .nominal_reinterpret,
        => {},
        .box_unbox => |child| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, child),
        .box_box => |child| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, child),
        .singleton_to_tag_union => |singleton| if (singleton.value_transform) |child| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, child);
        },
        .tag_union_to_singleton => |singleton| if (singleton.value_transform) |child| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, owner_artifact, transforms, child);
        },
    }
}

fn collectErasedCaptureMaterializationAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    visited_const_instances: *std.AutoHashMap(ConstInstanceAdapterVisitKey, void),
    plans: *const checked_artifact.CompileTimePlanStore,
    capture: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) Allocator.Error!void {
    switch (capture) {
        .none,
        .zero_sized_typed,
        => {},
        .node => |node| try collectErasedCaptureMaterializationNodeAdapters(
            allocator,
            adapters,
            artifact_views,
            visited_const_instances,
            plans,
            node,
        ),
    }
}

fn collectErasedCaptureMaterializationNodeAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    visited_const_instances: *std.AutoHashMap(ConstInstanceAdapterVisitKey, void),
    plans: *const checked_artifact.CompileTimePlanStore,
    node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
) Allocator.Error!void {
    const node = plans.erasedCaptureExecutableMaterializationNode(node_id);
    switch (node) {
        .pending => executableInvariant("executable adapter collection reached pending erased capture materialization node"),
        .pure_const,
        .pure_value,
        => {},
        .const_instance => |const_instance| try collectConstInstanceAdapters(
            allocator,
            adapters,
            artifact_views,
            visited_const_instances,
            const_instance,
        ),
        .finite_callable_set => |finite| for (finite.captures) |capture| {
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                capture,
            );
        },
        .erased_callable => |erased| {
            try collectErasedCodeRefAdapter(
                allocator,
                adapters,
                erased.code,
                // Materialized erased callable values that need an artifact
                // descriptor must publish member targets before reaching this
                // path. The owner is unused for direct code and empty-target
                // session descriptors.
                null,
                &.{},
                &.{},
            );
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                erased.capture,
            );
        },
        .record => |fields| for (fields) |field| {
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                field.value,
            );
        },
        .tuple => |items| for (items) |item| {
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                item,
            );
        },
        .tag_union => |tag| for (tag.payloads) |payload| {
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                payload.value,
            );
        },
        .list => |items| for (items) |item| {
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                item,
            );
        },
        .box => |payload| try collectErasedCaptureMaterializationAdapters(
            allocator,
            adapters,
            artifact_views,
            visited_const_instances,
            plans,
            payload,
        ),
        .nominal => |nominal| try collectErasedCaptureMaterializationAdapters(
            allocator,
            adapters,
            artifact_views,
            visited_const_instances,
            plans,
            nominal.backing,
        ),
        .recursive_ref => |ref| try collectErasedCaptureMaterializationNodeAdapters(
            allocator,
            adapters,
            artifact_views,
            visited_const_instances,
            plans,
            ref,
        ),
    }
}

fn collectConstInstanceAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    visited_const_instances: *std.AutoHashMap(ConstInstanceAdapterVisitKey, void),
    ref: checked_artifact.ConstInstanceRef,
) Allocator.Error!void {
    const visit_key = ConstInstanceAdapterVisitKey{
        .owner = ref.owner.bytes,
        .instance = ref.instance,
    };
    const visit = try visited_const_instances.getOrPut(visit_key);
    if (visit.found_existing) return;

    const resolved = resolveConstInstanceInArtifactViews(artifact_views, ref);
    try collectComptimeValueAdapters(
        allocator,
        adapters,
        artifact_views,
        visited_const_instances,
        resolved.materialization,
        resolved.instance.schema,
        resolved.instance.value,
    );
}

fn collectComptimeValueAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    visited_const_instances: *std.AutoHashMap(ConstInstanceAdapterVisitKey, void),
    materialization: MaterializationStores,
    schema_id: checked_artifact.ComptimeSchemaId,
    value_id: checked_artifact.ComptimeValueId,
) Allocator.Error!void {
    const schema = comptimeSchema(materialization.values, schema_id);
    const value = comptimeValue(materialization.values, value_id);
    switch (schema) {
        .pending => executableInvariant("executable adapter collection reached pending compile-time schema"),
        .zst,
        .int,
        .frac,
        .str,
        => {},
        .list => |elem_schema| {
            const items = switch (value) {
                .list => |items| items,
                else => executableInvariant("executable adapter collection reached list schema/value mismatch"),
            };
            for (items) |item| {
                try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, elem_schema, item);
            }
        },
        .box => |payload_schema| {
            const payload = switch (value) {
                .box => |payload| payload,
                else => executableInvariant("executable adapter collection reached Box(T) schema/value mismatch"),
            };
            try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, payload_schema, payload);
        },
        .tuple => |schemas| {
            const items = switch (value) {
                .tuple => |items| items,
                else => executableInvariant("executable adapter collection reached tuple schema/value mismatch"),
            };
            if (schemas.len != items.len) executableInvariant("executable adapter collection reached tuple arity mismatch");
            for (schemas, items) |item_schema, item| {
                try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, item_schema, item);
            }
        },
        .record => |fields| {
            const values = switch (value) {
                .record => |values| values,
                else => executableInvariant("executable adapter collection reached record schema/value mismatch"),
            };
            if (fields.len != values.len) executableInvariant("executable adapter collection reached record field count mismatch");
            for (fields, values) |field, field_value| {
                try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, field.schema, field_value);
            }
        },
        .tag_union => |variants| {
            const tag = switch (value) {
                .tag_union => |tag| tag,
                else => executableInvariant("executable adapter collection reached tag-union schema/value mismatch"),
            };
            const index: usize = @intCast(tag.variant_index);
            if (index >= variants.len) executableInvariant("executable adapter collection reached tag index outside schema");
            const variant = variants[index];
            if (variant.payloads.len != tag.payloads.len) executableInvariant("executable adapter collection reached tag payload count mismatch");
            for (variant.payloads, tag.payloads) |payload_schema, payload| {
                try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, payload_schema, payload);
            }
        },
        .alias => |alias| {
            const backing = switch (value) {
                .alias => |backing| backing,
                else => executableInvariant("executable adapter collection reached alias schema/value mismatch"),
            };
            try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, alias.backing, backing);
        },
        .nominal => |nominal| {
            const backing = switch (value) {
                .nominal => |backing| backing,
                else => executableInvariant("executable adapter collection reached nominal schema/value mismatch"),
            };
            try collectComptimeValueAdapters(allocator, adapters, artifact_views, visited_const_instances, materialization, nominal.backing, backing);
        },
        .callable => {
            const leaf = switch (value) {
                .callable => |leaf| leaf,
                else => executableInvariant("executable adapter collection reached callable schema/value mismatch"),
            };
            try collectComptimeCallableLeafAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                materialization.plans,
                leaf,
            );
        },
    }
}

fn collectComptimeCallableLeafAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    artifact_views: ArtifactViews,
    visited_const_instances: *std.AutoHashMap(ConstInstanceAdapterVisitKey, void),
    plans: *const checked_artifact.CompileTimePlanStore,
    leaf: checked_artifact.CallableLeafInstance,
) Allocator.Error!void {
    switch (leaf) {
        .finite => {},
        .erased_boxed => |erased| {
            try collectErasedCodeRefAdapter(allocator, adapters, erased.code, null, &.{}, &.{});
            try collectErasedCaptureMaterializationAdapters(
                allocator,
                adapters,
                artifact_views,
                visited_const_instances,
                plans,
                erased.capture,
            );
        },
    }
}

fn appendErasedDirectProcAdapterRequirement(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedDirectProcAdapterRequirement),
    context: struct {
        solve_session: repr.RepresentationSolveSessionId,
    },
    erase: repr.ProcValueErasePlan,
) Allocator.Error!void {
    const code = canonical.ErasedDirectProcCodeRef{
        .proc_value = erase.proc_value,
        .capture_shape_key = erase.capture_shape_key,
    };
    for (adapters.items) |*existing| {
        if (!erasedDirectProcCodeRefEql(existing.code, code)) continue;
        if (!repr.erasedFnSigKeyEql(existing.sig_key, erase.erased_fn_sig_key)) {
            executableInvariant("direct erased proc adapter requirement had conflicting erased signatures");
        }
        if (!repr.executableSpecializationKeyEql(existing.target_specialization, erase.executable_specialization_key)) {
            executableInvariant("direct erased proc adapter requirement had conflicting target specializations");
        }
        if (existing.target_instance != erase.target_instance) {
            executableInvariant("direct erased proc adapter requirement had conflicting target instances");
        }
        return;
    }

    var target_specialization = try repr.cloneExecutableSpecializationKey(allocator, erase.executable_specialization_key);
    errdefer repr.deinitExecutableSpecializationKey(allocator, &target_specialization);
    try adapters.append(allocator, .{
        .code = code,
        .sig_key = erase.erased_fn_sig_key,
        .target_specialization = target_specialization,
        .target_instance = erase.target_instance,
        .solve_session = context.solve_session,
        .arg_transforms = erase.adapter_arg_transforms,
    });
}

fn deinitErasedDirectProcAdapterRequirement(
    allocator: Allocator,
    requirement: *ErasedDirectProcAdapterRequirement,
) void {
    repr.deinitExecutableSpecializationKey(allocator, &requirement.target_specialization);
}

fn deinitErasedDirectProcAdapterReservation(
    allocator: Allocator,
    reservation: *ErasedDirectProcAdapterReservation,
) void {
    repr.deinitExecutableSpecializationKey(allocator, &reservation.target_specialization);
}

fn verifyErasedDirectProcAdapterArgBoundary(
    boundary: repr.ValueTransformBoundary,
    reservation: ErasedDirectProcAdapterReservation,
    target_instance: *const repr.ProcRepresentationInstance,
    index: u32,
) void {
    const kind = switch (boundary.kind) {
        .erased_proc_value_adapter_arg => |arg| arg,
        else => executableInvariant("direct erased proc adapter argument transform has wrong boundary kind"),
    };
    if (!canonical.procedureCallableRefEql(kind.proc_value, reservation.code.proc_value) or
        !repr.erasedFnSigKeyEql(kind.erased_fn_sig_key, reservation.sig_key) or
        kind.index != index)
    {
        executableInvariant("direct erased proc adapter argument transform points at a different erased proc-value occurrence");
    }

    const from = switch (boundary.from_endpoint.owner) {
        .erased_proc_value_adapter_arg => |arg| arg,
        else => executableInvariant("direct erased proc adapter argument transform source endpoint has wrong owner"),
    };
    if (from.emission_plan != kind.emission_plan or
        from.source_value != kind.source_value or
        !canonical.procedureCallableRefEql(from.proc_value, kind.proc_value) or
        !repr.erasedFnSigKeyEql(from.erased_fn_sig_key, kind.erased_fn_sig_key) or
        from.index != kind.index)
    {
        executableInvariant("direct erased proc adapter argument transform source endpoint differs from boundary owner");
    }

    const to = switch (boundary.to_endpoint.owner) {
        .procedure_param => |param| param,
        else => executableInvariant("direct erased proc adapter argument transform target endpoint is not a procedure parameter"),
    };
    if (to.instance != reservation.target_instance or to.index != index) {
        executableInvariant("direct erased proc adapter argument transform target endpoint differs from target procedure");
    }
    const arg_index: usize = @intCast(index);
    if (arg_index >= target_instance.executable_specialization_key.exec_arg_tys.len) {
        executableInvariant("direct erased proc adapter argument transform index exceeds target arity");
    }
    if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
        executableInvariant("direct erased proc adapter argument transform target key differs from specialization");
    }
}

fn appendErasedAdapterRequirement(
    allocator: Allocator,
    adapters: *std.ArrayList(ErasedAdapterRequirement),
    requirement: ErasedAdapterRequirement,
) Allocator.Error!void {
    var owned_requirement = requirement;
    errdefer deinitErasedAdapterRequirement(allocator, &owned_requirement);
    for (adapters.items) |*existing| {
        if (!erasedAdapterKeyEql(existing.key, owned_requirement.key)) continue;
        if (existing.payload_solve_session == null) {
            existing.payload_solve_session = owned_requirement.payload_solve_session;
        } else if (owned_requirement.payload_solve_session) |owner| {
            if (existing.payload_solve_session.? != owner) {
                executableInvariant("executable erased adapter requirement had conflicting payload solve sessions");
            }
        }
        if (existing.payload_artifact_owner == null) {
            existing.payload_artifact_owner = owned_requirement.payload_artifact_owner;
        } else if (owned_requirement.payload_artifact_owner) |owner| {
            if (!artifactKeyEql(existing.payload_artifact_owner.?, owner)) {
                executableInvariant("executable erased adapter requirement had conflicting payload artifact owners");
            }
        }
        if (existing.artifact_descriptor_owner == null) {
            existing.artifact_descriptor_owner = owned_requirement.artifact_descriptor_owner;
        } else if (owned_requirement.artifact_descriptor_owner) |owner| {
            if (!artifactKeyEql(existing.artifact_descriptor_owner.?, owner)) {
                executableInvariant("executable erased adapter requirement had conflicting artifact descriptor owners");
            }
        }
        if (existing.member_targets.len == 0 and owned_requirement.member_targets.len != 0) {
            existing.member_targets = owned_requirement.member_targets;
            existing.branches = owned_requirement.branches;
            existing.published_branches = owned_requirement.published_branches;
            owned_requirement.member_targets = &.{};
            owned_requirement.branches = &.{};
            owned_requirement.published_branches = &.{};
            return;
        }
        if (owned_requirement.member_targets.len != 0 and !executableSpecializationKeySlicesEql(existing.member_targets, owned_requirement.member_targets)) {
            executableInvariant("executable erased adapter requirement had conflicting member target keys");
        }
        if (existing.branches.len == 0 and owned_requirement.branches.len != 0) {
            existing.branches = owned_requirement.branches;
            owned_requirement.branches = &.{};
        } else if (owned_requirement.branches.len != 0 and existing.branches.len != owned_requirement.branches.len) {
            executableInvariant("executable erased adapter requirement had conflicting branch plans");
        }
        if (existing.published_branches.len == 0 and owned_requirement.published_branches.len != 0) {
            existing.published_branches = owned_requirement.published_branches;
            owned_requirement.published_branches = &.{};
        } else if (owned_requirement.published_branches.len != 0 and existing.published_branches.len != owned_requirement.published_branches.len) {
            executableInvariant("executable erased adapter requirement had conflicting published branch plans");
        }
        return;
    }
    try adapters.append(allocator, owned_requirement);
    owned_requirement.member_targets = &.{};
    owned_requirement.branches = &.{};
    owned_requirement.published_branches = &.{};
}

fn deinitErasedAdapterRequirement(allocator: Allocator, requirement: *ErasedAdapterRequirement) void {
    deinitExecutableSpecializationKeySlice(allocator, requirement.member_targets);
    repr.deinitFiniteSetEraseAdapterBranches(allocator, requirement.branches);
    deinitPublishedFiniteSetEraseAdapterBranches(allocator, requirement.published_branches);
    requirement.member_targets = &.{};
    requirement.branches = &.{};
    requirement.published_branches = &.{};
}

fn deinitErasedAdapterProcReservation(allocator: Allocator, reservation: *ErasedAdapterProcReservation) void {
    deinitExecutableSpecializationKeySlice(allocator, reservation.member_targets);
    repr.deinitFiniteSetEraseAdapterBranches(allocator, reservation.branches);
    deinitPublishedFiniteSetEraseAdapterBranches(allocator, reservation.published_branches);
    reservation.member_targets = &.{};
    reservation.branches = &.{};
    reservation.published_branches = &.{};
}

fn cloneExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const repr.ExecutableSpecializationKey,
) Allocator.Error![]const repr.ExecutableSpecializationKey {
    if (keys.len == 0) return &.{};
    const out = try allocator.alloc(repr.ExecutableSpecializationKey, keys.len);
    var initialized: usize = 0;
    errdefer {
        for (out[0..initialized]) |*key| repr.deinitExecutableSpecializationKey(allocator, key);
        allocator.free(out);
    }
    for (keys, 0..) |key, i| {
        out[i] = try repr.cloneExecutableSpecializationKey(allocator, key);
        initialized += 1;
    }
    return out;
}

fn clonePublishedFiniteSetEraseAdapterBranches(
    allocator: Allocator,
    branches: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan,
) Allocator.Error![]const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan {
    if (branches.len == 0) return &.{};
    const out = try allocator.alloc(checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan, branches.len);
    @memset(out, .{
        .member = .{
            .callable_set_key = .{ .bytes = [_]u8{0} ** 32 },
            .member_index = @enumFromInt(0),
        },
        .target_key = .{
            .base = @enumFromInt(0),
            .requested_fn_ty = .{ .bytes = [_]u8{0} ** 32 },
            .exec_arg_tys = &.{},
            .exec_ret_ty = .{ .bytes = [_]u8{0} ** 32 },
            .callable_repr_mode = .direct,
            .capture_shape_key = .{ .bytes = [_]u8{0} ** 32 },
        },
        .arg_transforms = &.{},
        .capture_transforms = &.{},
        .result_transform = .{
            .artifact = .{ .bytes = [_]u8{0} ** 32 },
            .transform = @enumFromInt(0),
        },
    });
    errdefer deinitPublishedFiniteSetEraseAdapterBranches(allocator, out);
    for (branches, 0..) |branch, i| {
        out[i] = .{
            .member = branch.member,
            .target_key = try repr.cloneExecutableSpecializationKey(allocator, branch.target_key),
            .arg_transforms = if (branch.arg_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(checked_artifact.PublishedExecutableValueTransformRef, branch.arg_transforms),
            .capture_transforms = if (branch.capture_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(checked_artifact.PublishedExecutableValueTransformRef, branch.capture_transforms),
            .result_transform = branch.result_transform,
        };
    }
    return out;
}

fn deinitPublishedFiniteSetEraseAdapterBranches(
    allocator: Allocator,
    branches: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan,
) void {
    for (branches) |branch| {
        var target_key = branch.target_key;
        repr.deinitExecutableSpecializationKey(allocator, &target_key);
        if (branch.arg_transforms.len > 0) allocator.free(branch.arg_transforms);
        if (branch.capture_transforms.len > 0) allocator.free(branch.capture_transforms);
    }
    if (branches.len > 0) allocator.free(branches);
}

fn deinitExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const repr.ExecutableSpecializationKey,
) void {
    for (keys) |key| {
        var owned = key;
        repr.deinitExecutableSpecializationKey(allocator, &owned);
    }
    if (keys.len > 0) allocator.free(keys);
}

fn executableSpecializationKeySlicesEql(
    a: []const repr.ExecutableSpecializationKey,
    b: []const repr.ExecutableSpecializationKey,
) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (!repr.executableSpecializationKeyEql(left, right)) return false;
    }
    return true;
}

fn erasedAdapterKeyEql(a: repr.ErasedAdapterKey, b: repr.ErasedAdapterKey) bool {
    return repr.canonicalTypeKeyEql(a.source_fn_ty, b.source_fn_ty) and
        repr.callableSetKeyEql(a.callable_set_key, b.callable_set_key) and
        repr.erasedFnSigKeyEql(a.erased_fn_sig_key, b.erased_fn_sig_key) and
        repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
}

fn erasedDirectProcCodeRefEql(a: canonical.ErasedDirectProcCodeRef, b: canonical.ErasedDirectProcCodeRef) bool {
    return canonical.procedureCallableRefEql(a.proc_value, b.proc_value) and
        repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
}

const AdapterDescriptorView = struct {
    descriptor: ?repr.CanonicalCallableSetDescriptor = null,
    owned_members: []repr.CanonicalCallableSetMember = &.{},
};

fn callableSetDescriptorForErasedAdapterReservation(
    allocator: Allocator,
    program: *const Program,
    input: *const LambdaSolved.Solve.Program,
    reservation: ErasedAdapterProcReservation,
) Allocator.Error!AdapterDescriptorView {
    if (reservation.member_targets.len == 0) {
        executableInvariant("erased finite-set adapter reservation has no published member targets");
    }
    if (reservation.artifact_descriptor_owner) |owner| {
        const descriptors = callableSetDescriptorsForArtifactInViews(program.artifact_views, owner) orelse {
            executableInvariant("persisted erased finite-set adapter referenced unavailable artifact descriptors");
        };
        const source_descriptor = descriptors.descriptorFor(reservation.key.callable_set_key) orelse {
            executableInvariant("persisted erased finite-set adapter referenced missing artifact callable-set descriptor");
        };
        return try remapAdapterDescriptorMembers(allocator, input, source_descriptor.key, source_descriptor.members, reservation.member_targets);
    }

    const source_descriptor = programCallableSetDescriptor(program, reservation.key.callable_set_key) orelse {
        executableInvariant("session erased finite-set adapter referenced missing callable-set descriptor");
    };
    return try remapAdapterDescriptorMembers(allocator, input, source_descriptor.key, source_descriptor.members, reservation.member_targets);
}

fn remapAdapterDescriptorMembers(
    allocator: Allocator,
    input: *const LambdaSolved.Solve.Program,
    key: repr.CanonicalCallableSetKey,
    source_members: anytype,
    member_targets: []const repr.ExecutableSpecializationKey,
) Allocator.Error!AdapterDescriptorView {
    if (source_members.len != member_targets.len) {
        executableInvariant("erased finite-set adapter member target count differs from descriptor");
    }

    const members = try allocator.alloc(repr.CanonicalCallableSetMember, source_members.len);
    errdefer allocator.free(members);
    for (source_members, member_targets, 0..) |source_member, target_key, i| {
        const target_instance = procInstanceForExecutableSpecializationKey(input, target_key) orelse {
            executableInvariant("erased finite-set adapter member target was not reserved in current solve session");
        };
        members[i] = .{
            .member = source_member.member,
            .proc_value = source_member.proc_value,
            .source_proc = source_member.source_proc,
            .target_instance = target_instance,
            .capture_slots = source_member.capture_slots,
            .capture_shape_key = source_member.capture_shape_key,
        };
    }
    return .{
        .descriptor = .{
            .key = key,
            .members = members,
        },
        .owned_members = members,
    };
}

fn procInstanceForExecutableSpecializationKey(
    input: *const LambdaSolved.Solve.Program,
    key: repr.ExecutableSpecializationKey,
) ?repr.ProcRepresentationInstanceId {
    for (input.proc_instances.items, 0..) |instance, raw| {
        if (!instance.materialized) continue;
        if (repr.executableSpecializationKeyEql(instance.executable_specialization_key, key)) {
            return @enumFromInt(@as(u32, @intCast(raw)));
        }
    }
    return null;
}

fn lowerExecutableSyntheticProc(
    allocator: Allocator,
    program: *Program,
    synthetic: ids.ExecutableSyntheticProc,
    executable_proc: Ast.ExecutableProcId,
) Allocator.Error!Ast.DefId {
    switch (synthetic.body) {
        .erased_promoted_wrapper => |erased| return try lowerErasedPromotedWrapperProc(
            allocator,
            program,
            synthetic,
            executable_proc,
            erased,
        ),
    }
}

fn lowerErasedDirectProcAdapterProc(
    allocator: Allocator,
    program: *Program,
    input: *const LambdaSolved.Solve.Program,
    type_lowerer: *TypeLowerer,
    proc_exec_map: *const std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId),
    reservation: ErasedDirectProcAdapterReservation,
    executable_proc: Ast.ExecutableProcId,
) Allocator.Error!Ast.DefId {
    const target_instance_index = @intFromEnum(reservation.target_instance);
    if (target_instance_index >= input.proc_instances.items.len) {
        executableInvariant("direct erased proc adapter target instance is out of range");
    }
    const target_instance = &input.proc_instances.items[target_instance_index];
    if (!repr.executableSpecializationKeyEql(target_instance.executable_specialization_key, reservation.target_specialization)) {
        executableInvariant("direct erased proc adapter target instance key differs from reservation");
    }
    const solve_session_index = @intFromEnum(reservation.solve_session);
    if (solve_session_index >= input.solve_sessions.items.len) {
        executableInvariant("direct erased proc adapter solve session is out of range");
    }
    const representation_store = &input.solve_sessions.items[solve_session_index].representation_store;
    const value_store_index = @intFromEnum(target_instance.value_store);
    if (value_store_index >= input.value_stores.items.len) {
        executableInvariant("direct erased proc adapter target value store is out of range");
    }
    const value_store = &input.value_stores.items[value_store_index];

    const target_proc = proc_exec_map.get(reservation.target_instance) orelse {
        executableInvariant("direct erased proc adapter target instance was not reserved as an executable procedure");
    };
    const target_record = executableProcRecord(program, target_proc);
    const target_def = program.ast.defs.items[@intFromEnum(target_record.body)];
    const target_source = switch (target_record.origin) {
        .source => |source| source,
        .erased_direct_proc_adapter,
        .erased_adapter,
        => executableInvariant("direct erased proc adapter target was not an ordinary source procedure"),
    };
    const target_fn = switch (target_def.value) {
        .fn_ => |fn_| fn_,
        .hosted_fn => executableInvariant("direct erased proc adapter target cannot be a hosted function"),
    };
    const target_args = program.ast.typed_values.items[target_fn.args.start..][0..target_fn.args.len];
    const has_capture_payload = reservation.sig_key.capture_ty != null;
    if (has_capture_payload and target_args.len == 0) {
        executableInvariant("direct erased proc adapter target has capture payload but no target capture argument");
    }
    const explicit_arg_count = if (has_capture_payload) target_args.len - 1 else target_args.len;
    if (reservation.arg_transforms.len != explicit_arg_count) {
        executableInvariant("direct erased proc adapter argument transform count differs from source arity");
    }

    var builder = BodyBuilder{
        .allocator = allocator,
        .program = program,
        .input = &input.ast,
        .output = &program.ast,
        .canonical_names = &program.canonical_names,
        .type_lowerer = type_lowerer,
        .session_type_lowerer = SessionTypeLowerer.init(allocator, &representation_store.session_executable_type_payloads, &program.types, &program.lowered_session_types_by_key),
        .value_store = value_store,
        .representation_store = representation_store,
        .callable_set_descriptors = program.callable_set_descriptors,
        .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
        .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
        .executable_proc = executable_proc,
        .source_proc = target_source,
        .representation_instance = reservation.target_instance,
        .proc_instance = target_instance,
        .proc_instances = input.proc_instances.items,
        .solve_sessions = input.solve_sessions.items,
        .value_stores = input.value_stores.items,
        .proc_exec_map = proc_exec_map,
        .erased_adapter_procs = program.erased_adapter_procs.items,
    };
    defer builder.deinit();

    const hidden_capture_arg_ty = try program.types.addType(.{ .primitive = .erased });
    const wrapper_arg_count = explicit_arg_count + 1;
    const wrapper_args = try allocator.alloc(Ast.TypedValue, wrapper_arg_count);
    defer allocator.free(wrapper_args);
    for (reservation.arg_transforms, 0..) |boundary_id, i| {
        const boundary = representation_store.valueTransformBoundary(boundary_id);
        verifyErasedDirectProcAdapterArgBoundary(boundary, reservation, target_instance, @intCast(i));
        wrapper_args[i] = .{
            .ty = try builder.lowerSessionExecutableEndpointType(boundary.from_endpoint),
            .value = program.ast.freshValueRef(),
        };
    }
    const hidden_capture_value = program.ast.freshValueRef();
    wrapper_args[explicit_arg_count] = .{
        .ty = hidden_capture_arg_ty,
        .value = hidden_capture_value,
    };
    const args = try program.ast.addTypedValueSpan(wrapper_args);

    const result_ty = program.ast.exprs.items[@intFromEnum(target_fn.body)].ty;
    const body = try lowerErasedDirectProcAdapterBody(
        allocator,
        program,
        target_source,
        reservation.target_specialization,
        target_proc,
        wrapper_args[0..explicit_arg_count],
        reservation.arg_transforms,
        &builder,
        hidden_capture_value,
        if (has_capture_payload) target_args[explicit_arg_count].ty else null,
        result_ty,
    );

    var specialization_key = try repr.cloneExecutableSpecializationKey(allocator, reservation.target_specialization);
    specialization_key.callable_repr_mode = .erased_callable;
    specialization_key.capture_shape_key = reservation.code.capture_shape_key;
    return try program.ast.addDef(.{
        .proc = executable_proc,
        .origin = .{ .erased_direct_proc_adapter = reservation.code },
        .specialization_key = specialization_key,
        .value = .{ .fn_ = .{
            .args = args,
            .body = body,
        } },
    });
}

fn lowerErasedDirectProcAdapterBody(
    allocator: Allocator,
    program: *Program,
    source_proc: canonical.MirProcedureRef,
    target_specialization: repr.ExecutableSpecializationKey,
    target_proc: Ast.ExecutableProcId,
    explicit_args: []const Ast.TypedValue,
    arg_transforms: []const repr.ValueTransformBoundaryId,
    builder: *BodyBuilder,
    hidden_capture_handle: Ast.ExecutableValueRef,
    hidden_capture_payload_ty: ?Type.TypeId,
    result_ty: Type.TypeId,
) Allocator.Error!Ast.ExprId {
    var stmt_ids = std.ArrayList(Ast.StmtId).empty;
    defer stmt_ids.deinit(allocator);
    if (explicit_args.len != arg_transforms.len) {
        executableInvariant("direct erased proc adapter body argument transform count differs from arity");
    }

    const capture_value: ?Ast.ExecutableValueRef = if (hidden_capture_payload_ty) |payload_ty| blk: {
        const handle_ty = try program.types.addType(.{ .primitive = .erased });
        const handle_expr = try program.ast.addValueRefExpr(handle_ty, hidden_capture_handle);
        const value = program.ast.freshValueRef();
        const payload_expr = try program.ast.addExpr(payload_ty, value, .{ .low_level = .{
            .op = .erased_capture_load,
            .rc_effect = base.LowLevel.erased_capture_load.rcEffect(),
            .args = try program.ast.addExprSpan(&.{handle_expr}),
        } });
        try stmt_ids.append(allocator, try program.ast.addStmt(.{ .decl = .{
            .value = value,
            .body = payload_expr,
        } }));
        break :blk value;
    } else null;

    const direct_arg_count = explicit_args.len + if (capture_value == null) @as(usize, 0) else 1;
    const direct_args = try allocator.alloc(Ast.DirectCallArg, direct_arg_count);
    defer allocator.free(direct_args);
    for (explicit_args, arg_transforms, 0..) |arg, boundary_id, i| {
        const boundary = builder.representation_store.valueTransformBoundary(boundary_id);
        direct_args[i] = .{
            .value = try builder.applyValueTransformBoundary(&stmt_ids, boundary, arg.value),
        };
    }
    if (capture_value) |payload| {
        direct_args[explicit_args.len] = .{ .value = payload };
    }

    const raw_result_value = program.ast.freshValueRef();
    const direct_call = try program.ast.addExpr(result_ty, raw_result_value, .{ .call_direct = .{
        .source = source_proc.proc,
        .executable_specialization_key = try repr.cloneExecutableSpecializationKey(allocator, target_specialization),
        .executable_proc = target_proc,
        .direct_args = try program.ast.addDirectCallArgSpan(direct_args),
    } });
    try stmt_ids.append(allocator, try program.ast.addStmt(.{ .decl = .{
        .value = raw_result_value,
        .body = direct_call,
    } }));

    const final_expr = try program.ast.addValueRefExpr(result_ty, raw_result_value);
    return try program.ast.addExpr(result_ty, raw_result_value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(stmt_ids.items),
        .final_expr = final_expr,
    } });
}

fn lowerErasedFiniteSetAdapterProc(
    allocator: Allocator,
    program: *Program,
    input: *const LambdaSolved.Solve.Program,
    type_lowerer: *TypeLowerer,
    proc_exec_map: *const std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId),
    reservation: ErasedAdapterProcReservation,
    executable_proc: Ast.ExecutableProcId,
) Allocator.Error!Ast.DefId {
    const adapter = reservation.key;
    var descriptor_view = try callableSetDescriptorForErasedAdapterReservation(allocator, program, input, reservation);
    defer if (descriptor_view.owned_members.len > 0) allocator.free(descriptor_view.owned_members);
    const descriptor = if (descriptor_view.descriptor) |*view_descriptor|
        view_descriptor
    else
        executableInvariant("executable erased finite-set adapter has no descriptor");
    if (descriptor.members.len == 0) {
        executableInvariant("executable erased finite-set adapter descriptor has no members");
    }
    const has_session_branches = reservation.branches.len != 0;
    const has_published_branches = reservation.published_branches.len != 0;
    if (has_session_branches == has_published_branches) {
        executableInvariant("executable erased finite-set adapter must have exactly one branch transform plan source");
    }
    if (has_session_branches and reservation.branches.len != descriptor.members.len) {
        executableInvariant("executable erased finite-set adapter has no finalized branch transform plan");
    }
    if (has_published_branches and reservation.published_branches.len != descriptor.members.len) {
        executableInvariant("executable erased finite-set adapter has no finalized published branch transform plan");
    }

    const first_instance_id = if (has_session_branches)
        reservation.branches[0].target_instance
    else
        descriptor.members[0].target_instance;
    const first_instance = &input.proc_instances.items[@intFromEnum(first_instance_id)];
    if (!repr.canonicalTypeKeyEql(first_instance.executable_specialization_key.requested_fn_ty, adapter.source_fn_ty)) {
        executableInvariant("executable erased finite-set adapter source function type differs from first member specialization");
    }

    const value_store = &input.value_stores.items[@intFromEnum(first_instance.value_store)];
    const member_representation_store = &input.solve_sessions.items[@intFromEnum(first_instance.solve_session)].representation_store;
    const payload_representation_store = if (reservation.payload_solve_session) |session_id| blk: {
        const session_index = @intFromEnum(session_id);
        if (session_index >= input.solve_sessions.items.len) {
            executableInvariant("executable erased finite-set adapter payload solve session is out of range");
        }
        break :blk &input.solve_sessions.items[session_index].representation_store;
    } else member_representation_store;
    var published_adapter_payloads_storage: PublishedTypeLowerer = undefined;
    var published_adapter_payloads: ?*PublishedTypeLowerer = null;
    var published_adapter_context: ?PublishedTransformContext = null;
    var published_adapter_artifact = canonical.ArtifactRef{};
    if (reservation.payload_solve_session == null) {
        if (reservation.payload_artifact_owner) |owner| {
            const context = resolvePublishedTransformContextInArtifactViews(program.artifact_views, owner);
            published_adapter_context = context;
            published_adapter_payloads_storage = PublishedTypeLowerer.init(
                allocator,
                context.executable_type_payloads,
                context.materialization.canonical_names,
                &program.canonical_names,
                &program.types,
                &program.row_shapes,
                &program.lowered_session_types_by_key,
            );
            published_adapter_payloads = &published_adapter_payloads_storage;
            published_adapter_artifact = .{ .bytes = owner.bytes };
        }
    }
    defer if (published_adapter_payloads) |published| published.deinit();
    var builder = BodyBuilder{
        .allocator = allocator,
        .program = program,
        .input = &input.ast,
        .output = &program.ast,
        .canonical_names = &program.canonical_names,
        .type_lowerer = type_lowerer,
        .session_type_lowerer = SessionTypeLowerer.init(allocator, &payload_representation_store.session_executable_type_payloads, &program.types, &program.lowered_session_types_by_key),
        .value_store = value_store,
        .representation_store = payload_representation_store,
        .published_adapter_payloads = published_adapter_payloads,
        .published_adapter_artifact = published_adapter_artifact,
        .callable_set_descriptors = program.callable_set_descriptors,
        .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
        .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
        .executable_proc = executable_proc,
        .source_proc = first_instance.proc,
        .representation_instance = first_instance_id,
        .proc_instance = first_instance,
        .proc_instances = input.proc_instances.items,
        .solve_sessions = input.solve_sessions.items,
        .value_stores = input.value_stores.items,
        .proc_exec_map = proc_exec_map,
        .erased_adapter_procs = program.erased_adapter_procs.items,
    };
    defer builder.deinit();

    const hidden_capture_payload_ty = try builder.lowerFiniteSetAdapterCaptureType(adapter, descriptor);
    if (hidden_capture_payload_ty == null and descriptor.members.len != 1) {
        executableInvariant("executable erased finite-set adapter without hidden capture cannot dispatch a multi-member callable set");
    }
    const result_ty = if (has_session_branches) blk: {
        const first_branch = reservation.branches[0];
        if (first_branch.result_transform == null) {
            executableInvariant("executable erased finite-set adapter first branch has no result transform");
        }
        const first_result_boundary = payload_representation_store.valueTransformBoundary(first_branch.result_transform.?);
        break :blk try builder.lowerSessionExecutableEndpointType(first_result_boundary.to_endpoint);
    } else blk: {
        const context = published_adapter_context orelse executableInvariant("published erased finite-set adapter has no artifact transform context");
        const published = published_adapter_payloads orelse executableInvariant("published erased finite-set adapter has no published payload lowerer");
        const transform_id = publishedExecutableValueTransformId(context.artifact, reservation.published_branches[0].result_transform);
        const transform = context.executable_value_transforms.get(transform_id);
        break :blk try published.lower(transform.to.ty, transform.to.key);
    };

    const hidden_capture_arg_ty = try program.types.addType(.{ .primitive = .erased });
    const explicit_arg_len = if (has_session_branches)
        reservation.branches[0].arg_transforms.len
    else
        reservation.published_branches[0].arg_transforms.len;
    const typed_arg_count = explicit_arg_len + 1;
    const typed_args = try allocator.alloc(Ast.TypedValue, typed_arg_count);
    defer allocator.free(typed_args);
    if (has_session_branches) {
        for (reservation.branches[0].arg_transforms, 0..) |boundary_id, i| {
            const boundary = payload_representation_store.valueTransformBoundary(boundary_id);
            typed_args[i] = .{
                .ty = try builder.lowerSessionExecutableEndpointType(boundary.from_endpoint),
                .value = program.ast.freshValueRef(),
            };
        }
    } else {
        const context = published_adapter_context orelse executableInvariant("published erased finite-set adapter has no artifact transform context");
        const published = published_adapter_payloads orelse executableInvariant("published erased finite-set adapter has no published payload lowerer");
        for (reservation.published_branches[0].arg_transforms, 0..) |transform_ref, i| {
            const transform_id = publishedExecutableValueTransformId(context.artifact, transform_ref);
            const transform = context.executable_value_transforms.get(transform_id);
            typed_args[i] = .{
                .ty = try published.lower(transform.from.ty, transform.from.key),
                .value = program.ast.freshValueRef(),
            };
        }
    }

    const hidden_capture_value = program.ast.freshValueRef();
    typed_args[explicit_arg_len] = .{
        .ty = hidden_capture_arg_ty,
        .value = hidden_capture_value,
    };

    const args = try program.ast.addTypedValueSpan(typed_args);
    const body = try lowerErasedFiniteSetAdapterBody(
        allocator,
        program,
        &builder,
        adapter,
        descriptor,
        reservation.branches,
        reservation.published_branches,
        published_adapter_context,
        published_adapter_payloads,
        typed_args[0..explicit_arg_len],
        hidden_capture_value,
        hidden_capture_payload_ty,
        result_ty,
    );

    var specialization_key = try repr.cloneExecutableSpecializationKey(allocator, first_instance.executable_specialization_key);
    specialization_key.callable_repr_mode = .erased_adapter;
    specialization_key.capture_shape_key = adapter.capture_shape_key;
    return try program.ast.addDef(.{
        .proc = executable_proc,
        .origin = .{ .erased_adapter = adapter },
        .specialization_key = specialization_key,
        .value = .{ .fn_ = .{
            .args = args,
            .body = body,
        } },
    });
}

fn lowerErasedFiniteSetAdapterBody(
    allocator: Allocator,
    program: *Program,
    builder: *BodyBuilder,
    adapter: repr.ErasedAdapterKey,
    descriptor: *const repr.CanonicalCallableSetDescriptor,
    branch_plans: []const repr.FiniteSetEraseAdapterBranchPlan,
    published_branch_plans: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan,
    published_context: ?PublishedTransformContext,
    published_types: ?*PublishedTypeLowerer,
    explicit_args: []const Ast.TypedValue,
    hidden_capture_handle: Ast.ExecutableValueRef,
    hidden_capture_payload_ty: ?Type.TypeId,
    result_ty: Type.TypeId,
) Allocator.Error!Ast.ExprId {
    if (!repr.callableSetKeyEql(descriptor.key, adapter.callable_set_key)) {
        executableInvariant("executable erased finite-set adapter descriptor key differs from adapter key");
    }

    const CalleeValue = struct {
        value: Ast.ExecutableValueRef,
        stmt: Ast.StmtId,
    };
    const callee_value: CalleeValue = if (hidden_capture_payload_ty) |payload_ty| blk: {
        const handle_ty = program.ast.requireValueType(hidden_capture_handle);
        const handle_expr = try program.ast.addValueRefExpr(handle_ty, hidden_capture_handle);
        const value = program.ast.freshValueRef();
        const payload_expr = try program.ast.addExpr(payload_ty, value, .{ .low_level = .{
            .op = .erased_capture_load,
            .rc_effect = base.LowLevel.erased_capture_load.rcEffect(),
            .args = try program.ast.addExprSpan(&.{handle_expr}),
        } });
        const stmt = try program.ast.addStmt(.{ .decl = .{
            .value = value,
            .body = payload_expr,
        } });
        break :blk .{
            .value = value,
            .stmt = stmt,
        };
    } else blk: {
        const member = descriptor.members[0];
        if (member.capture_slots.len != 0) {
            executableInvariant("executable erased finite-set adapter without hidden capture cannot synthesize captured callable set");
        }
        const callable_set_ty = try builder.lowerCallableSetType(adapter.callable_set_key);
        const value = program.ast.freshValueRef();
        const singleton = try program.ast.addExpr(callable_set_ty, value, .{ .callable_set_value = .{
            .construction_plan = null,
            .callable_set_key = adapter.callable_set_key,
            .member = .{
                .callable_set_key = adapter.callable_set_key,
                .member_index = member.member,
            },
            .capture_record = null,
        } });
        const stmt = try program.ast.addStmt(.{ .decl = .{
            .value = value,
            .body = singleton,
        } });
        break :blk .{
            .value = value,
            .stmt = stmt,
        };
    };

    const final_call = try lowerErasedFiniteSetAdapterCallableMatch(
        allocator,
        program,
        builder,
        adapter,
        descriptor,
        branch_plans,
        published_branch_plans,
        published_context,
        published_types,
        explicit_args,
        callee_value.value,
        result_ty,
    );
    return try program.ast.addExpr(result_ty, program.ast.getExpr(final_call).value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(&.{callee_value.stmt}),
        .final_expr = final_call,
    } });
}

fn lowerErasedFiniteSetAdapterCallableMatch(
    allocator: Allocator,
    program: *Program,
    builder: *BodyBuilder,
    adapter: repr.ErasedAdapterKey,
    descriptor: *const repr.CanonicalCallableSetDescriptor,
    branch_plans: []const repr.FiniteSetEraseAdapterBranchPlan,
    published_branch_plans: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan,
    published_context: ?PublishedTransformContext,
    published_types: ?*PublishedTypeLowerer,
    explicit_args: []const Ast.TypedValue,
    callee_value: Ast.ExecutableValueRef,
    result_ty: Type.TypeId,
) Allocator.Error!Ast.ExprId {
    const arg_values: []Ast.ExecutableValueRef = if (explicit_args.len == 0)
        &.{}
    else
        try allocator.alloc(Ast.ExecutableValueRef, explicit_args.len);
    defer if (arg_values.len > 0) allocator.free(arg_values);
    for (explicit_args, 0..) |arg, i| {
        arg_values[i] = arg.value;
    }

    const branches = try allocator.alloc(Ast.CallableMatchBranch, descriptor.members.len);
    defer allocator.free(branches);
    if (branch_plans.len != 0) {
        if (branch_plans.len != descriptor.members.len) {
            executableInvariant("executable erased finite-set adapter branch plan count differs from descriptor");
        }
        for (descriptor.members, branch_plans, 0..) |member, branch_plan, i| {
            if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, adapter.source_fn_ty)) {
                executableInvariant("executable erased finite-set adapter member source type differs from adapter key");
            }
            const expected_member_ref: repr.CallableSetMemberRef = .{
                .callable_set_key = adapter.callable_set_key,
                .member_index = member.member,
            };
            if (!repr.callableSetKeyEql(branch_plan.member.callable_set_key, expected_member_ref.callable_set_key) or
                branch_plan.member.member_index != expected_member_ref.member_index)
            {
                executableInvariant("executable erased finite-set adapter branch plan member differs from descriptor");
            }
            const target_instance_id = branch_plan.target_instance;
            const executable_proc = builder.proc_exec_map.get(target_instance_id) orelse {
                executableInvariant("executable erased finite-set adapter member target was not reserved");
            };
            const target_instance = builder.proc_instances[@intFromEnum(target_instance_id)];
            if (!canonical.mirProcedureRefEql(target_instance.proc, member.source_proc)) {
                executableInvariant("executable erased finite-set adapter member target instance differs from descriptor source procedure");
            }
            if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, adapter.source_fn_ty)) {
                executableInvariant("executable erased finite-set adapter member target specialization source type differs from adapter key");
            }

            const capture_payload_ty = try builder.lowerCallableSetMemberPayloadType(adapter.callable_set_key, member);
            const capture_payload = if (capture_payload_ty) |payload_ty| try program.ast.freshTypedValueRef(payload_ty) else null;
            const result_transform_id = branch_plan.result_transform orelse {
                executableInvariant("executable erased finite-set adapter branch has no result transform");
            };
            const result_boundary = builder.representation_store.valueTransformBoundary(result_transform_id);
            builder.verifyErasedFiniteAdapterBranchResultBoundary(
                result_boundary,
                adapter,
                expected_member_ref,
                target_instance_id,
                target_instance,
            );
            const lowered_branch = try builder.lowerCallableMatchBranchBody(
                member.source_proc,
                target_instance,
                target_instance_id,
                executable_proc,
                arg_values,
                branch_plan.arg_transforms,
                null,
                null,
                expected_member_ref,
                capture_payload,
                capture_payload_ty,
                branch_plan.capture_transforms,
                result_ty,
                result_boundary,
            );

            branches[i] = .{
                .member = expected_member_ref,
                .source_fn_ty = member.proc_value.source_fn_ty,
                .capture_payload = capture_payload,
                .capture_payload_ty = capture_payload_ty,
                .executable_specialization_key = try repr.cloneExecutableSpecializationKey(allocator, target_instance.executable_specialization_key),
                .executable_proc = executable_proc,
                .arg_transforms = lowered_branch.arg_transforms,
                .direct_args = lowered_branch.direct_args,
                .body = lowered_branch.body,
            };
        }
    } else {
        const context = published_context orelse executableInvariant("published erased finite-set adapter has no transform context");
        const published = published_types orelse executableInvariant("published erased finite-set adapter has no type lowerer");
        if (published_branch_plans.len != descriptor.members.len) {
            executableInvariant("executable erased finite-set adapter published branch plan count differs from descriptor");
        }
        for (descriptor.members, published_branch_plans, 0..) |member, branch_plan, i| {
            const expected_member_ref: repr.CallableSetMemberRef = .{
                .callable_set_key = adapter.callable_set_key,
                .member_index = member.member,
            };
            if (!repr.callableSetKeyEql(branch_plan.member.callable_set_key, expected_member_ref.callable_set_key) or
                branch_plan.member.member_index != expected_member_ref.member_index)
            {
                executableInvariant("executable erased finite-set adapter published branch member differs from descriptor");
            }
            const target_instance_id = member.target_instance;
            const executable_proc = builder.proc_exec_map.get(target_instance_id) orelse {
                executableInvariant("executable erased finite-set adapter published member target was not reserved");
            };
            const target_instance = builder.proc_instances[@intFromEnum(target_instance_id)];
            if (!repr.executableSpecializationKeyEql(target_instance.executable_specialization_key, branch_plan.target_key)) {
                executableInvariant("executable erased finite-set adapter published branch target key differs from target instance");
            }
            const capture_payload_ty = try builder.lowerCallableSetMemberPayloadType(adapter.callable_set_key, member);
            const capture_payload = if (capture_payload_ty) |payload_ty| try program.ast.freshTypedValueRef(payload_ty) else null;
            const lowered_branch = try lowerPublishedErasedFiniteSetAdapterBranchBody(
                allocator,
                program,
                context,
                published,
                context.executable_value_transforms,
                member.source_proc,
                target_instance,
                executable_proc,
                arg_values,
                branch_plan.arg_transforms,
                capture_payload,
                capture_payload_ty,
                branch_plan.capture_transforms,
                result_ty,
                branch_plan.result_transform,
            );
            branches[i] = .{
                .member = expected_member_ref,
                .source_fn_ty = member.proc_value.source_fn_ty,
                .capture_payload = capture_payload,
                .capture_payload_ty = capture_payload_ty,
                .executable_specialization_key = try repr.cloneExecutableSpecializationKey(allocator, target_instance.executable_specialization_key),
                .executable_proc = executable_proc,
                .arg_transforms = lowered_branch.arg_transforms,
                .direct_args = lowered_branch.direct_args,
                .body = lowered_branch.body,
            };
        }
    }

    const result_value = program.ast.freshValueRef();
    return try program.ast.addExpr(result_ty, result_value, .{ .callable_match = .{
        .callable_set_key = adapter.callable_set_key,
        .requested_source_fn_ty = adapter.source_fn_ty,
        .callee = callee_value,
        .args = try program.ast.addValueRefSpan(arg_values),
        .branches = try program.ast.addCallableMatchBranchSpan(branches),
        .result_ty = result_ty,
        .result_value = result_value,
    } });
}

const PublishedLoweredCallableMatchBranch = struct {
    arg_transforms: Ast.Span(checked_artifact.ExecutableValueTransformRef),
    direct_args: Ast.Span(Ast.DirectCallArg),
    body: Ast.ExprId,
};

fn lowerPublishedErasedFiniteSetAdapterBranchBody(
    allocator: Allocator,
    program: *Program,
    materialization: PublishedTransformContext,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    source_proc: canonical.MirProcedureRef,
    target_instance: repr.ProcRepresentationInstance,
    executable_proc: Ast.ExecutableProcId,
    arg_values: []const Ast.ExecutableValueRef,
    arg_transforms: []const checked_artifact.PublishedExecutableValueTransformRef,
    capture_payload: ?Ast.ExecutableValueRef,
    capture_payload_ty: ?Type.TypeId,
    capture_transforms: []const checked_artifact.PublishedExecutableValueTransformRef,
    result_ty: Type.TypeId,
    result_transform: checked_artifact.PublishedExecutableValueTransformRef,
) Allocator.Error!PublishedLoweredCallableMatchBranch {
    var stmt_ids = std.ArrayList(Ast.StmtId).empty;
    defer stmt_ids.deinit(allocator);

    if (arg_values.len != arg_transforms.len) {
        executableInvariant("published erased finite-set adapter branch argument transform count differs from arity");
    }
    const capture_arg_len: usize = if (capture_payload == null) 0 else 1;
    const direct_args = try allocator.alloc(Ast.DirectCallArg, arg_values.len + capture_arg_len);
    defer allocator.free(direct_args);
    const transform_refs = try allocator.alloc(checked_artifact.ExecutableValueTransformRef, arg_transforms.len);
    defer allocator.free(transform_refs);

    for (arg_values, arg_transforms, 0..) |arg_value, transform_ref, i| {
        const transform_id = publishedExecutableValueTransformId(materialization.artifact, transform_ref);
        const transform = transforms.get(transform_id);
        if (!repr.canonicalExecValueTypeKeyEql(transform.to.key, target_instance.executable_specialization_key.exec_arg_tys[i])) {
            executableInvariant("published erased finite-set adapter branch argument target key differs from specialization");
        }
        transform_refs[i] = .{ .published = transform_ref };
        direct_args[i] = .{
            .value = try applyPublishedExecutableValueTransform(
                program,
                materialization.materialization,
                published_types,
                transforms,
                &stmt_ids,
                transform_id,
                arg_value,
            ),
        };
    }

    if (capture_payload) |payload| {
        if (capture_transforms.len > 0) {
            direct_args[arg_values.len] = .{
                .value = try lowerPublishedErasedFiniteAdapterBranchCaptureArg(
                    allocator,
                    program,
                    materialization,
                    published_types,
                    transforms,
                    &stmt_ids,
                    payload,
                    capture_payload_ty orelse executableInvariant("published erased finite-set adapter capture transform has no payload type"),
                    capture_transforms,
                    target_instance,
                ),
            };
        } else {
            direct_args[arg_values.len] = .{ .value = payload };
        }
    } else if (capture_transforms.len != 0) {
        executableInvariant("published erased finite-set adapter branch has capture transforms without capture payload");
    }
    const direct_args_span = try program.ast.addDirectCallArgSpan(direct_args);
    const arg_transforms_span = try program.ast.addExecutableValueTransformRefSpan(transform_refs);
    const result_transform_id = publishedExecutableValueTransformId(materialization.artifact, result_transform);
    const result_plan = transforms.get(result_transform_id);
    if (!repr.canonicalExecValueTypeKeyEql(result_plan.from.key, target_instance.executable_specialization_key.exec_ret_ty)) {
        executableInvariant("published erased finite-set adapter branch result source key differs from specialization");
    }
    const raw_result_ty = try published_types.lower(result_plan.from.ty, result_plan.from.key);
    const raw_result_value = program.ast.freshValueRef();
    const direct_call = try program.ast.addExpr(raw_result_ty, raw_result_value, .{ .call_direct = .{
        .source = source_proc.proc,
        .executable_specialization_key = try repr.cloneExecutableSpecializationKey(allocator, target_instance.executable_specialization_key),
        .executable_proc = executable_proc,
        .direct_args = direct_args_span,
    } });
    try stmt_ids.append(allocator, try program.ast.addStmt(.{ .decl = .{
        .value = raw_result_value,
        .body = direct_call,
    } }));

    const result_value = try applyPublishedExecutableValueTransform(
        program,
        materialization.materialization,
        published_types,
        transforms,
        &stmt_ids,
        result_transform_id,
        raw_result_value,
    );
    const final_expr = try program.ast.addValueRefExpr(result_ty, result_value);
    const body = try program.ast.addExpr(result_ty, result_value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(stmt_ids.items),
        .final_expr = final_expr,
    } });
    return .{
        .arg_transforms = arg_transforms_span,
        .direct_args = direct_args_span,
        .body = body,
    };
}

fn lowerPublishedErasedFiniteAdapterBranchCaptureArg(
    allocator: Allocator,
    program: *Program,
    materialization: PublishedTransformContext,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmt_ids: *std.ArrayList(Ast.StmtId),
    capture_payload: Ast.ExecutableValueRef,
    capture_payload_ty: Type.TypeId,
    capture_transforms: []const checked_artifact.PublishedExecutableValueTransformRef,
    target_instance: repr.ProcRepresentationInstance,
) Allocator.Error!Ast.ExecutableValueRef {
    _ = target_instance;
    const source_items = switch (program.types.getType(capture_payload_ty)) {
        .tuple => |items| items,
        else => executableInvariant("published erased finite-set adapter branch capture payload type is not a tuple"),
    };
    if (source_items.len != capture_transforms.len) {
        executableInvariant("published erased finite-set adapter branch capture transform count differs from payload arity");
    }
    const payload_expr = try program.ast.addValueRefExpr(capture_payload_ty, capture_payload);
    const output_items = try allocator.alloc(Ast.ExprId, capture_transforms.len);
    defer allocator.free(output_items);
    const target_item_tys = try allocator.alloc(Type.TypeId, capture_transforms.len);
    defer allocator.free(target_item_tys);

    for (capture_transforms, 0..) |transform_ref, slot_i| {
        const transform_id = publishedExecutableValueTransformId(materialization.artifact, transform_ref);
        const transform = transforms.get(transform_id);
        const source_ty = try published_types.lower(transform.from.ty, transform.from.key);
        if (source_ty != source_items[slot_i]) {
            executableInvariant("published erased finite-set adapter branch capture source type differs from member payload slot");
        }
        const access_value = program.ast.freshValueRef();
        const access_expr = try program.ast.addExpr(source_ty, access_value, .{ .tuple_access = .{
            .tuple = payload_expr,
            .elem_index = @intCast(slot_i),
        } });
        try stmt_ids.append(allocator, try program.ast.addStmt(.{ .decl = .{
            .value = access_value,
            .body = access_expr,
        } }));
        const transformed = try applyPublishedExecutableValueTransform(
            program,
            materialization.materialization,
            published_types,
            transforms,
            stmt_ids,
            transform_id,
            access_value,
        );
        const target_ty = try published_types.lower(transform.to.ty, transform.to.key);
        target_item_tys[slot_i] = target_ty;
        output_items[slot_i] = try program.ast.addValueRefExpr(target_ty, transformed);
    }

    const tuple_tys = if (target_item_tys.len == 0)
        &.{}
    else
        try allocator.dupe(Type.TypeId, target_item_tys);
    errdefer if (tuple_tys.len > 0) allocator.free(tuple_tys);
    const capture_arg_ty = try program.types.addType(.{ .tuple = tuple_tys });
    const capture_arg_value = program.ast.freshValueRef();
    const capture_arg_expr = try program.ast.addExpr(capture_arg_ty, capture_arg_value, .{
        .tuple = try addTupleItemExprSpanForConstruction(allocator, program, &program.ast, output_items, target_item_tys),
    });
    try stmt_ids.append(allocator, try program.ast.addStmt(.{ .decl = .{
        .value = capture_arg_value,
        .body = capture_arg_expr,
    } }));
    return capture_arg_value;
}

fn lowerErasedPromotedWrapperProc(
    allocator: Allocator,
    program: *Program,
    synthetic: ids.ExecutableSyntheticProc,
    executable_proc: Ast.ExecutableProcId,
    erased: checked_artifact.ErasedPromotedWrapperBodyPlan,
) Allocator.Error!Ast.DefId {
    var published_types = PublishedTypeLowerer.init(
        allocator,
        synthetic.executable_type_payloads,
        canonicalNamesForArtifactInViews(program.artifact_views, synthetic.artifact),
        &program.canonical_names,
        &program.types,
        &program.row_shapes,
        &program.lowered_session_types_by_key,
    );
    defer published_types.deinit();

    const signature = erased.executable_signature;
    if (signature.wrapper_params.len != erased.params.len) {
        executableInvariant("executable erased promoted wrapper signature param count differs from wrapper body plan");
    }

    const args = try lowerErasedPromotedWrapperParams(allocator, &program.ast, &published_types, signature.wrapper_params);
    const body = try lowerErasedPromotedWrapperBody(
        allocator,
        program,
        materializationStoresForArtifact(
            synthetic.artifact,
            canonicalNamesForArtifactInViews(program.artifact_views, synthetic.artifact),
            synthetic.comptime_plans,
            synthetic.comptime_values,
        ),
        synthetic.artifact,
        synthetic.executable_value_transforms,
        &published_types,
        args,
        signature,
        erased,
    );

    return try program.ast.addDef(.{
        .proc = executable_proc,
        .origin = .{ .source = synthetic.source_proc },
        .specialization_key = try repr.cloneExecutableSpecializationKey(allocator, signature.specialization_key),
        .value = .{ .fn_ = .{
            .args = args,
            .body = body,
        } },
    });
}

fn lowerErasedPromotedWrapperParams(
    allocator: Allocator,
    ast: *Ast.Store,
    published_types: *PublishedTypeLowerer,
    params: []const checked_artifact.ExecutableProcedureParamPayload,
) Allocator.Error!Ast.Span(Ast.TypedValue) {
    if (params.len == 0) return Ast.Span(Ast.TypedValue).empty();
    const out = try allocator.alloc(Ast.TypedValue, params.len);
    defer allocator.free(out);
    for (params, 0..) |param, i| {
        out[i] = .{
            .ty = try published_types.lower(param.exec_ty, param.exec_ty_key),
            .value = ast.freshValueRef(),
        };
    }
    return try ast.addTypedValueSpan(out);
}

fn lowerErasedPromotedWrapperBody(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    transform_owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    published_types: *PublishedTypeLowerer,
    args: Ast.Span(Ast.TypedValue),
    signature: checked_artifact.ErasedPromotedProcedureExecutableSignature,
    erased: checked_artifact.ErasedPromotedWrapperBodyPlan,
) Allocator.Error!Ast.ExprId {
    if (signature.erased_call_args.len != signature.wrapper_params.len) {
        executableInvariant("executable erased promoted wrapper erased-call arity differs from wrapper params");
    }
    if (erased.arg_transforms.len != signature.wrapper_params.len) {
        executableInvariant("executable erased promoted wrapper arg transform count differs from wrapper params");
    }

    const wrapper_args = program.ast.typed_values.items[args.start..][0..args.len];
    if (wrapper_args.len != signature.wrapper_params.len) {
        executableInvariant("executable erased promoted wrapper arg span differs from signature params");
    }

    const wrapper_ret_ty = try published_types.lower(signature.wrapper_ret, signature.wrapper_ret_key);
    const raw_call_ty = try published_types.lower(signature.erased_call_ret, signature.erased_call_ret_key);

    const capture_ty = if (signature.hidden_capture) |hidden|
        try published_types.lower(hidden.exec_ty, hidden.exec_ty_key)
    else
        null;
    const capture = try lowerErasedPromotedCapture(
        allocator,
        program,
        materialization,
        capture_ty,
        erased.capture,
        erased.hidden_capture_arg,
    );

    const code_proc = executableProcForErasedCode(program, erased.code);
    const packed_ty = try program.types.addType(.{ .erased_fn = .{
        .sig_key = erased.sig_key,
        .capture_shape = signature.specialization_key.capture_shape_key,
        .capture_ty = capture_ty,
    } });
    const packed_value = program.ast.freshValueRef();
    const packed_expr = try program.ast.addExpr(packed_ty, packed_value, .{ .packed_erased_fn = .{
        .sig_key = erased.sig_key,
        .code = code_proc,
        .capture = capture.value,
        .capture_ty = capture_ty,
        .capture_shape = signature.specialization_key.capture_shape_key,
    } });

    const call_args: []Ast.ExecutableValueRef = if (wrapper_args.len == 0)
        &.{}
    else
        try allocator.alloc(Ast.ExecutableValueRef, wrapper_args.len);
    defer if (call_args.len > 0) allocator.free(call_args);
    var stmts = std.ArrayList(Ast.StmtId).empty;
    defer stmts.deinit(allocator);
    if (capture.stmt) |stmt| try stmts.append(allocator, stmt);
    try stmts.append(allocator, try program.ast.addStmt(.{ .decl = .{
        .value = packed_value,
        .body = packed_expr,
    } }));

    for (wrapper_args, signature.erased_call_args, signature.erased_call_arg_keys, 0..) |arg, erased_arg, erased_arg_key, i| {
        _ = erased_arg;
        _ = erased_arg_key;
        call_args[i] = try applyPublishedExecutableValueTransformRef(
            program,
            materialization,
            transform_owner_artifact,
            published_types,
            transforms,
            &stmts,
            erased.arg_transforms[i],
            arg.value,
        );
    }

    const raw_call_value = program.ast.freshValueRef();
    const raw_call_expr = try program.ast.addExpr(raw_call_ty, raw_call_value, .{ .call_erased = .{
        .func = packed_value,
        .args = try program.ast.addValueRefSpan(call_args),
        .sig_key = erased.sig_key,
        .capture_ty = capture_ty,
    } });

    const result_transform = publishedExecutableValueTransformId(transform_owner_artifact, erased.result_transform);
    const result_plan = transforms.get(result_transform);
    const final_expr = switch (result_plan.op) {
        .identity => raw_call_expr,
        else => blk: {
            try stmts.append(allocator, try program.ast.addStmt(.{ .decl = .{
                .value = raw_call_value,
                .body = raw_call_expr,
            } }));
            const result_value = try applyPublishedExecutableValueTransform(
                program,
                materialization,
                published_types,
                transforms,
                &stmts,
                result_transform,
                raw_call_value,
            );
            break :blk try program.ast.addValueRefExpr(wrapper_ret_ty, result_value);
        },
    };

    return try program.ast.addExpr(wrapper_ret_ty, program.ast.getExpr(final_expr).value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(stmts.items),
        .final_expr = final_expr,
    } });
}

fn publishedExecutableValueTransformId(
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    transform_ref: checked_artifact.PublishedExecutableValueTransformRef,
) checked_artifact.ExecutableValueTransformPlanId {
    if (!std.mem.eql(u8, &owner_artifact.bytes, &transform_ref.artifact.bytes)) {
        executableInvariant("executable published value transform refers to a different checked artifact");
    }
    return transform_ref.transform;
}

fn applyPublishedExecutableValueTransformRef(
    program: *Program,
    materialization: MaterializationStores,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    transform_ref: checked_artifact.PublishedExecutableValueTransformRef,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    return try applyPublishedExecutableValueTransform(
        program,
        materialization,
        published_types,
        transforms,
        stmts,
        publishedExecutableValueTransformId(owner_artifact, transform_ref),
        value,
    );
}

fn applyPublishedExecutableValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    transform_id: checked_artifact.ExecutableValueTransformPlanId,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const plan = transforms.get(transform_id);
    switch (plan.op) {
        .identity => {
            if (!repr.canonicalExecValueTypeKeyEql(plan.from.key, plan.to.key)) {
                executableInvariant("executable identity value transform changes representation");
            }
            return value;
        },
        .structural_bridge => |structural| {
            const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
            const bridge = try lowerPublishedExecutableValueTransformAsBridge(
                program,
                materialization,
                published_types,
                transforms,
                transform_id,
                structural,
            );
            const bridged_value = program.ast.freshValueRef();
            const bridged_expr = try program.ast.addExpr(to_ty, bridged_value, .{ .bridge = .{
                .bridge = bridge,
                .value = value,
            } });
            try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
                .value = bridged_value,
                .body = bridged_expr,
            } }));
            return bridged_value;
        },
        .record => |fields| return try applyRecordValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            fields,
            value,
        ),
        .tuple => |items| return try applyTupleValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            items,
            value,
        ),
        .nominal => |nominal| return try applyNominalValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            nominal,
            value,
        ),
        .tag_union => |cases| return try applyTagUnionValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            cases,
            value,
        ),
        .list => |list| return try applyListValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            list.elem,
            value,
        ),
        .box_payload => |box| return try applyBoxValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            box,
            value,
        ),
        .callable_to_erased => |callable| return try applyCallableToErasedValueTransform(
            program,
            materialization,
            published_types,
            stmts,
            plan,
            callable,
            value,
        ),
        .already_erased_callable => |already_erased| {
            const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
            const erased_ty = erasedFnType(program, to_ty);
            if (!repr.erasedFnSigKeyEql(erased_ty.sig_key, already_erased.sig_key)) {
                executableInvariant("already-erased value transform signature differs from target endpoint");
            }
            return value;
        },
    }
}

fn applyRecordValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    fields: []const checked_artifact.ValueTransformRecordField,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    const source = switch (program.types.getType(from_ty)) {
        .record => |record| record,
        else => executableInvariant("record value transform source endpoint is not a record"),
    };
    const target = switch (program.types.getType(to_ty)) {
        .record => |record| record,
        else => executableInvariant("record value transform target endpoint is not a record"),
    };
    if (fields.len != target.fields.len) {
        executableInvariant("record value transform field count differs from target record");
    }

    const source_expr = try program.ast.addValueRefExpr(from_ty, value);
    const seen = try program.allocator.alloc(bool, fields.len);
    defer program.allocator.free(seen);
    @memset(seen, false);

    const output_fields = try program.allocator.alloc(Ast.RecordFieldExpr, target.fields.len);
    defer program.allocator.free(output_fields);
    for (target.fields, 0..) |target_field, target_i| {
        const label = program.row_shapes.recordField(target_field.field).label;
        const field_plan = (try findValueTransformRecordField(program, materialization, fields, label, seen)) orelse {
            executableInvariant("record value transform omitted a target field");
        };
        const source_field = recordFieldForLabel(program, source, label);
        const access_value = program.ast.freshValueRef();
        const access_expr = try program.ast.addExpr(source_field.ty, access_value, .{ .access = .{
            .record = source_expr,
            .field = source_field.field,
        } });
        try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
            .value = access_value,
            .body = access_expr,
        } }));
        const transformed = try applyPublishedExecutableValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            field_plan.transform,
            access_value,
        );
        output_fields[target_i] = .{
            .field = target_field.field,
            .expr = try program.ast.addValueRefExpr(target_field.ty, transformed),
            .ty = target_field.ty,
            .value = transformed,
            .bridge = try constructionSlotBridgeForProgram(program.allocator, program, &program.ast, target_field.ty, target_field.ty),
        };
    }
    verifyAllSeen(seen, "record value transform had an extra source field transform");

    const record_value = program.ast.freshValueRef();
    const record_expr = try program.ast.addExpr(to_ty, record_value, .{ .record = .{
        .shape = target.shape,
        .fields = try program.ast.addRecordFieldExprSpan(output_fields),
    } });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = record_value,
        .body = record_expr,
    } }));
    return record_value;
}

fn findValueTransformRecordField(
    program: *Program,
    materialization: MaterializationStores,
    fields: []const checked_artifact.ValueTransformRecordField,
    label: canonical.RecordFieldLabelId,
    seen: []bool,
) Allocator.Error!?checked_artifact.ValueTransformRecordField {
    for (fields, 0..) |field, i| {
        const field_label = try materializationRecordFieldLabel(program, materialization, field.field);
        if (field_label != label) continue;
        if (seen[i]) executableInvariant("record value transform duplicated a field");
        seen[i] = true;
        return field;
    }
    return null;
}

fn applyNominalValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    nominal: anytype,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    const source = switch (program.types.getType(from_ty)) {
        .nominal => |source| source,
        else => executableInvariant("nominal value transform source endpoint is not nominal"),
    };
    const target = switch (program.types.getType(to_ty)) {
        .nominal => |target| target,
        else => executableInvariant("nominal value transform target endpoint is not nominal"),
    };
    const remapped_nominal = try materializationNominalTypeKey(program, materialization, nominal.nominal);
    if (!nominalTypeKeyEql(target.nominal, remapped_nominal)) {
        executableInvariant("nominal value transform target nominal differs from plan");
    }
    if (!repr.canonicalTypeKeyEql(target.source_ty, nominal.source_ty)) {
        executableInvariant("nominal value transform target source type differs from plan");
    }

    const source_expr = try program.ast.addValueRefExpr(from_ty, value);
    const backing_value = program.ast.freshValueRef();
    const backing_expr = try program.ast.addExpr(source.backing, backing_value, .{ .nominal_reinterpret = source_expr });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = backing_value,
        .body = backing_expr,
    } }));

    const transformed_backing = try applyPublishedExecutableValueTransform(
        program,
        materialization,
        published_types,
        transforms,
        stmts,
        nominal.backing,
        backing_value,
    );
    const transformed_expr = try program.ast.addValueRefExpr(target.backing, transformed_backing);
    const nominal_value = program.ast.freshValueRef();
    const nominal_expr = try program.ast.addExpr(to_ty, nominal_value, .{ .nominal_reinterpret = transformed_expr });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = nominal_value,
        .body = nominal_expr,
    } }));
    return nominal_value;
}

fn applyTupleValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    items: []const checked_artifact.ValueTransformTupleElem,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    const source = switch (program.types.getType(from_ty)) {
        .tuple => |tuple| tuple,
        else => executableInvariant("tuple value transform source endpoint is not a tuple"),
    };
    const target = switch (program.types.getType(to_ty)) {
        .tuple => |tuple| tuple,
        else => executableInvariant("tuple value transform target endpoint is not a tuple"),
    };
    if (items.len != target.len or source.len != target.len) {
        executableInvariant("tuple value transform arity differs from endpoint tuple");
    }

    const tuple_expr = try program.ast.addValueRefExpr(from_ty, value);
    const seen = try program.allocator.alloc(bool, items.len);
    defer program.allocator.free(seen);
    @memset(seen, false);

    const output_items = try program.allocator.alloc(Ast.ExprId, target.len);
    defer program.allocator.free(output_items);
    for (target, 0..) |target_item_ty, i| {
        const item_plan = findValueTransformTupleElem(items, @intCast(i), seen) orelse {
            executableInvariant("tuple value transform omitted a target element");
        };
        const access_value = program.ast.freshValueRef();
        const access_expr = try program.ast.addExpr(source[i], access_value, .{ .tuple_access = .{
            .tuple = tuple_expr,
            .elem_index = @intCast(i),
        } });
        try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
            .value = access_value,
            .body = access_expr,
        } }));
        const transformed = try applyPublishedExecutableValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            item_plan.transform,
            access_value,
        );
        output_items[i] = try program.ast.addValueRefExpr(target_item_ty, transformed);
    }
    verifyAllSeen(seen, "tuple value transform had an extra element transform");

    const tuple_value = program.ast.freshValueRef();
    const result_expr = try program.ast.addExpr(to_ty, tuple_value, .{ .tuple = try addTupleItemExprSpanForConstruction(program.allocator, program, &program.ast, output_items, target) });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = tuple_value,
        .body = result_expr,
    } }));
    return tuple_value;
}

fn findValueTransformTupleElem(
    items: []const checked_artifact.ValueTransformTupleElem,
    index: u32,
    seen: []bool,
) ?checked_artifact.ValueTransformTupleElem {
    for (items, 0..) |item, i| {
        if (item.index != index) continue;
        if (seen[i]) executableInvariant("tuple value transform duplicated an element");
        seen[i] = true;
        return item;
    }
    return null;
}

fn applyListValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    elem_transform: checked_artifact.ExecutableValueTransformPlanId,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    const source_elem_ty = listElementTypeForTransform(program, from_ty, "source");
    const target_elem_ty = listElementTypeForTransform(program, to_ty, "target");

    const source_elem = program.ast.freshValueRef();
    var body_stmts = std.ArrayList(Ast.StmtId).empty;
    defer body_stmts.deinit(program.allocator);

    const transformed_elem = try applyPublishedExecutableValueTransform(
        program,
        materialization,
        published_types,
        transforms,
        &body_stmts,
        elem_transform,
        source_elem,
    );
    const transformed_expr = try program.ast.addValueRefExpr(target_elem_ty, transformed_elem);
    const body_expr = if (body_stmts.items.len == 0)
        transformed_expr
    else
        try program.ast.addExpr(target_elem_ty, transformed_elem, .{ .block = .{
            .stmts = try program.ast.addStmtSpan(body_stmts.items),
            .final_expr = transformed_expr,
        } });

    const list_value = program.ast.freshValueRef();
    const list_expr = try program.ast.addExpr(to_ty, list_value, .{ .value_transform_list = .{
        .source = value,
        .source_elem = source_elem,
        .source_elem_ty = source_elem_ty,
        .target_elem_ty = target_elem_ty,
        .body = body_expr,
    } });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = list_value,
        .body = list_expr,
    } }));

    return list_value;
}

fn listElementTypeForTransform(
    program: *Program,
    list_ty: Type.TypeId,
    comptime side: []const u8,
) Type.TypeId {
    return switch (program.types.getType(list_ty)) {
        .list => |elem| elem,
        else => executableInvariant("list value transform " ++ side ++ " endpoint is not List(T)"),
    };
}

fn applyTagUnionValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    cases: []const checked_artifact.ValueTransformTagCase,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    const source = switch (program.types.getType(from_ty)) {
        .tag_union => |tag_union| tag_union,
        else => executableInvariant("tag-union value transform source endpoint is not a tag union"),
    };
    const target = switch (program.types.getType(to_ty)) {
        .tag_union => |tag_union| tag_union,
        else => executableInvariant("tag-union value transform target endpoint is not a tag union"),
    };
    if (cases.len != source.tags.len) {
        executableInvariant("tag-union value transform case count differs from source tag-union arity");
    }

    const seen_cases = try program.allocator.alloc(bool, cases.len);
    defer program.allocator.free(seen_cases);
    @memset(seen_cases, false);

    const branches = try program.allocator.alloc(Ast.ValueTransformTagBranch, source.tags.len);
    defer program.allocator.free(branches);
    for (source.tags, 0..) |source_tag, source_i| {
        const source_label = program.row_shapes.tag(source_tag.tag).label;
        const case = (try findValueTransformTagCase(program, materialization, cases, source_label, seen_cases)) orelse {
            executableInvariant("tag-union value transform omitted a source tag case");
        };
        const target_label = try materializationTagLabel(program, materialization, case.target_tag);
        const target_tag = tagTypeForLabel(program, target, target_label);

        var branch_stmts = std.ArrayList(Ast.StmtId).empty;
        defer branch_stmts.deinit(program.allocator);
        const branch_body = try tagUnionValueTransformBranchBody(
            program,
            materialization,
            published_types,
            transforms,
            &branch_stmts,
            from_ty,
            to_ty,
            source_tag,
            target,
            target_tag,
            case,
            value,
        );

        branches[source_i] = .{
            .discriminant = @intCast(program.row_shapes.tag(source_tag.tag).logical_index),
            .body = branch_body,
        };
    }
    verifyAllSeen(seen_cases, "tag-union value transform had an extra source tag case");

    const transformed_value = program.ast.freshValueRef();
    const transformed_expr = try program.ast.addExpr(to_ty, transformed_value, .{ .value_transform_tag_union = .{
        .source = value,
        .source_union_shape = source.shape,
        .branches = try program.ast.addValueTransformTagBranchSpan(branches),
    } });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = transformed_value,
        .body = transformed_expr,
    } }));
    return transformed_value;
}

fn tagUnionValueTransformBranchBody(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    branch_stmts: *std.ArrayList(Ast.StmtId),
    source_union_ty: Type.TypeId,
    target_union_ty: Type.TypeId,
    source_tag: Type.TagType,
    target_union: Type.TagUnionType,
    target_tag: Type.TagType,
    case: checked_artifact.ValueTransformTagCase,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExprId {
    if (case.payloads.len != target_tag.payloads.len) {
        executableInvariant("tag-union value transform payload edge count differs from target tag arity");
    }

    const source_expr = try program.ast.addValueRefExpr(source_union_ty, value);
    const seen_payloads = try program.allocator.alloc(bool, case.payloads.len);
    defer program.allocator.free(seen_payloads);
    @memset(seen_payloads, false);

    const payload_exprs = try program.allocator.alloc(Ast.TagPayloadExpr, target_tag.payloads.len);
    defer program.allocator.free(payload_exprs);
    for (target_tag.payloads, 0..) |target_payload, target_i| {
        const edge = findValueTransformPayloadEdge(case.payloads, @intCast(target_i), seen_payloads) orelse {
            executableInvariant("tag-union value transform omitted a target payload edge");
        };
        const source_payload_index: usize = @intCast(edge.source_payload_index);
        if (source_payload_index >= source_tag.payloads.len) {
            executableInvariant("tag-union value transform source payload index exceeded source tag arity");
        }
        const source_payload = source_tag.payloads[source_payload_index];

        const access_value = program.ast.freshValueRef();
        const access_expr = try program.ast.addExpr(source_payload.ty, access_value, .{ .tag_payload = .{
            .tag_union = source_expr,
            .payload = source_payload.payload,
        } });
        try branch_stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
            .value = access_value,
            .body = access_expr,
        } }));

        const transformed = try applyPublishedExecutableValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            branch_stmts,
            edge.transform,
            access_value,
        );
        payload_exprs[target_i] = .{
            .payload = target_payload.payload,
            .expr = try program.ast.addValueRefExpr(target_payload.ty, transformed),
            .ty = target_payload.ty,
            .value = transformed,
            .bridge = try constructionSlotBridgeForProgram(program.allocator, program, &program.ast, target_payload.ty, target_payload.ty),
        };
    }
    verifyAllSeen(seen_payloads, "tag-union value transform had an extra payload edge");

    const tag_value = program.ast.freshValueRef();
    const tag_expr = try program.ast.addExpr(target_union_ty, tag_value, .{ .tag = .{
        .union_shape = target_union.shape,
        .tag = target_tag.tag,
        .payloads = try program.ast.addTagPayloadExprSpan(payload_exprs),
    } });
    if (branch_stmts.items.len == 0) return tag_expr;
    return try program.ast.addExpr(target_union_ty, tag_value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(branch_stmts.items),
        .final_expr = tag_expr,
    } });
}

fn findValueTransformTagCase(
    program: *Program,
    materialization: MaterializationStores,
    cases: []const checked_artifact.ValueTransformTagCase,
    source_label: canonical.TagLabelId,
    seen: []bool,
) Allocator.Error!?checked_artifact.ValueTransformTagCase {
    for (cases, 0..) |case, i| {
        const case_label = try materializationTagLabel(program, materialization, case.source_tag);
        if (case_label != source_label) continue;
        if (seen[i]) executableInvariant("tag-union value transform duplicated a source tag case");
        seen[i] = true;
        return case;
    }
    return null;
}

fn findValueTransformPayloadEdge(
    payloads: []const checked_artifact.ValueTransformTagPayloadEdge,
    target_payload_index: u32,
    seen: []bool,
) ?checked_artifact.ValueTransformTagPayloadEdge {
    for (payloads, 0..) |payload, i| {
        if (payload.target_payload_index != target_payload_index) continue;
        if (seen[i]) executableInvariant("tag-union value transform duplicated a target payload edge");
        seen[i] = true;
        return payload;
    }
    return null;
}

fn tagTypeForLabel(
    program: *const Program,
    tag_union: Type.TagUnionType,
    label: canonical.TagLabelId,
) Type.TagType {
    for (tag_union.tags) |tag| {
        if (program.row_shapes.tag(tag.tag).label == label) return tag;
    }
    executableInvariant("tag-union value transform target tag label is absent from target type");
}

fn recordFieldForId(
    program: *const Program,
    record: Type.RecordType,
    field_id: MonoRow.RecordFieldId,
) Type.RecordFieldType {
    _ = program;
    for (record.fields) |field| {
        if (field.field == field_id) return field;
    }
    executableInvariant("session record value transform field id is absent from source type");
}

fn findSessionValueTransformRecordField(
    fields: []const repr.SessionValueTransformRecordField,
    field_id: MonoRow.RecordFieldId,
    seen: []bool,
) ?repr.SessionValueTransformRecordField {
    for (fields, 0..) |field, i| {
        if (field.field != field_id) continue;
        if (seen[i]) executableInvariant("session record value transform duplicated a field");
        seen[i] = true;
        return field;
    }
    return null;
}

fn findSessionValueTransformTupleElem(
    items: []const repr.SessionValueTransformTupleElem,
    index: u32,
    seen: []bool,
) ?repr.SessionValueTransformTupleElem {
    for (items, 0..) |item, i| {
        if (item.index != index) continue;
        if (seen[i]) executableInvariant("session tuple value transform duplicated an element");
        seen[i] = true;
        return item;
    }
    return null;
}

fn tagTypeForId(
    program: *const Program,
    tag_union: Type.TagUnionType,
    tag_id: MonoRow.TagId,
) Type.TagType {
    _ = program;
    for (tag_union.tags) |tag| {
        if (tag.tag == tag_id) return tag;
    }
    executableInvariant("session tag-union value transform target tag id is absent from target type");
}

fn tagPayloadEndpointType(
    allocator: Allocator,
    program: *Program,
    tag: Type.TagType,
) Allocator.Error!?Type.TypeId {
    if (tag.payloads.len == 0) return null;
    if (tag.payloads.len == 1) return tag.payloads[0].ty;

    const payload_tys = try allocator.alloc(Type.TypeId, tag.payloads.len);
    var seen = try allocator.alloc(bool, tag.payloads.len);
    defer allocator.free(seen);
    @memset(seen, false);
    for (tag.payloads) |payload| {
        const index: usize = @intCast(program.row_shapes.tagPayload(payload.payload).logical_index);
        if (index >= payload_tys.len or seen[index]) {
            executableInvariant("executable tag payload endpoint type saw invalid payload index");
        }
        payload_tys[index] = payload.ty;
        seen[index] = true;
    }
    verifyAllSeen(seen, "executable tag payload endpoint type omitted payload");
    return try program.types.addType(.{ .tuple = payload_tys });
}

fn findSessionValueTransformTagCase(
    cases: []const repr.SessionValueTransformTagCase,
    source_tag: MonoRow.TagId,
    seen: []bool,
) ?repr.SessionValueTransformTagCase {
    for (cases, 0..) |case, i| {
        if (case.source_tag != source_tag) continue;
        if (seen[i]) executableInvariant("session tag-union value transform duplicated a source tag case");
        seen[i] = true;
        return case;
    }
    return null;
}

fn findSessionValueTransformPayloadEdge(
    payloads: []const repr.SessionValueTransformTagPayloadEdge,
    target_payload_index: u32,
    seen: []bool,
) ?repr.SessionValueTransformTagPayloadEdge {
    for (payloads, 0..) |payload, i| {
        if (payload.target_payload_index != target_payload_index) continue;
        if (seen[i]) executableInvariant("session tag-union value transform duplicated a target payload edge");
        seen[i] = true;
        return payload;
    }
    return null;
}

fn tagDiscriminantForId(
    program: *const Program,
    ty: Type.TypeId,
    tag_id: MonoRow.TagId,
) Allocator.Error!u16 {
    const tag_union = switch (program.types.getType(ty)) {
        .tag_union => |tag_union| tag_union,
        else => executableInvariant("executable session structural bridge expected a tag union endpoint"),
    };
    for (tag_union.tags, 0..) |tag, i| {
        if (tag.tag == tag_id) return @intCast(i);
    }
    executableInvariant("executable session structural bridge tag id is absent from endpoint type");
}

fn applyBoxValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    box: checked_artifact.BoxPayloadTransformPlan,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);

    switch (box.kind) {
        .payload_to_box => {
            _ = boxPayloadType(program, to_ty);
            const transformed = try applyPublishedExecutableValueTransform(
                program,
                materialization,
                published_types,
                transforms,
                stmts,
                box.payload,
                value,
            );
            return try boxTransformedPayload(program, stmts, to_ty, transformed);
        },
        .box_to_payload => {
            _ = boxPayloadType(program, from_ty);
            const unboxed = try unboxPayloadForTransform(program, stmts, from_ty, value);
            return try applyPublishedExecutableValueTransform(
                program,
                materialization,
                published_types,
                transforms,
                stmts,
                box.payload,
                unboxed,
            );
        },
        .box_to_box => {
            _ = boxPayloadType(program, from_ty);
            _ = boxPayloadType(program, to_ty);
            const unboxed = try unboxPayloadForTransform(program, stmts, from_ty, value);
            const transformed = try applyPublishedExecutableValueTransform(
                program,
                materialization,
                published_types,
                transforms,
                stmts,
                box.payload,
                unboxed,
            );
            return try boxTransformedPayload(program, stmts, to_ty, transformed);
        },
    }
}

fn unboxPayloadForTransform(
    program: *Program,
    stmts: *std.ArrayList(Ast.StmtId),
    source_box_ty: Type.TypeId,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const payload_ty = boxPayloadType(program, source_box_ty);

    const source_expr = try program.ast.addValueRefExpr(source_box_ty, value);
    const unboxed_value = program.ast.freshValueRef();
    const args = [_]Ast.ExprId{source_expr};
    const unboxed_expr = try program.ast.addExpr(payload_ty, unboxed_value, .{ .low_level = .{
        .op = .box_unbox,
        .rc_effect = base.LowLevel.box_unbox.rcEffect(),
        .args = try program.ast.addExprSpan(&args),
    } });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = unboxed_value,
        .body = unboxed_expr,
    } }));
    return unboxed_value;
}

fn boxTransformedPayload(
    program: *Program,
    stmts: *std.ArrayList(Ast.StmtId),
    target_box_ty: Type.TypeId,
    payload: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const payload_ty = boxPayloadType(program, target_box_ty);

    const payload_expr = try program.ast.addValueRefExpr(payload_ty, payload);
    const boxed_value = program.ast.freshValueRef();
    const args = [_]Ast.ExprId{payload_expr};
    const boxed_expr = try program.ast.addExpr(target_box_ty, boxed_value, .{ .low_level = .{
        .op = .box_box,
        .rc_effect = base.LowLevel.box_box.rcEffect(),
        .args = try program.ast.addExprSpan(&args),
    } });
    try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
        .value = boxed_value,
        .body = boxed_expr,
    } }));
    return boxed_value;
}

fn boxPayloadType(program: *const Program, ty: Type.TypeId) Type.TypeId {
    return switch (program.types.getType(ty)) {
        .box => |payload| payload,
        else => executableInvariant("box value transform endpoint is not Box(T)"),
    };
}

fn applyCallableToErasedValueTransform(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    stmts: *std.ArrayList(Ast.StmtId),
    plan: checked_artifact.ExecutableValueTransformPlan,
    callable: checked_artifact.CallableToErasedTransformPlan,
    value: Ast.ExecutableValueRef,
) Allocator.Error!Ast.ExecutableValueRef {
    const result_ty = try published_types.lower(plan.to.ty, plan.to.key);
    const erased_ty = erasedFnType(program, result_ty);
    switch (callable) {
        .finite_value => |finite| {
            if (!repr.erasedFnSigKeyEql(erased_ty.sig_key, finite.adapter_key.erased_fn_sig_key)) {
                executableInvariant("finite callable erasure transform target signature differs from adapter key");
            }
            if (!repr.callableSetKeyEql(finite.callable_set_key, finite.adapter_key.callable_set_key)) {
                executableInvariant("finite callable erasure transform callable-set key differs from adapter key");
            }
            if (!repr.canonicalTypeKeyEql(finite.source_fn_ty, finite.adapter_key.source_fn_ty)) {
                executableInvariant("finite callable erasure transform source function type differs from adapter key");
            }

            _ = erased_ty.capture_ty;
            const hidden_capture_ty = if (finite.adapter_key.erased_fn_sig_key.capture_ty) |capture_key| blk: {
                const capture_ref = published_types.payloads.refForKey(artifactRefFromKey(materialization.owner), capture_key) orelse {
                    executableInvariant("finite callable erasure transform hidden capture key has no published payload");
                };
                break :blk try published_types.lower(capture_ref, capture_key);
            } else null;
            const hidden_capture = if (hidden_capture_ty == null) null else value;
            const packed_value = program.ast.freshValueRef();
            const packed_expr = try program.ast.addExpr(result_ty, packed_value, .{ .packed_erased_fn = .{
                .sig_key = finite.adapter_key.erased_fn_sig_key,
                .code = executableProcForErasedAdapter(program, finite.adapter_key),
                .capture = hidden_capture,
                .capture_ty = hidden_capture_ty,
                .capture_shape = finite.adapter_key.capture_shape_key,
            } });
            try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
                .value = packed_value,
                .body = packed_expr,
            } }));
            return packed_value;
        },
        .proc_value => |proc| {
            if (!repr.erasedFnSigKeyEql(erased_ty.sig_key, proc.erased_fn_sig_key)) {
                executableInvariant("proc-value erasure transform target signature differs from plan");
            }
            _ = executableProcForSpecializationKey(program, proc.executable_specialization_key);
            const erased_expr = try lowerMaterializedErasedCallableValue(
                program.allocator,
                program,
                materialization,
                result_ty,
                .{
                    .source_fn_ty = proc.proc_value.source_fn_ty,
                    .sig_key = proc.erased_fn_sig_key,
                    .code = .{ .direct_proc_value = .{
                        .proc_value = proc.proc_value,
                        .capture_shape_key = proc.capture_shape_key,
                    } },
                    .capture = proc.capture,
                    .provenance = &.{},
                },
            );
            const erased_value = program.ast.getExpr(erased_expr).value;
            try stmts.append(program.allocator, try program.ast.addStmt(.{ .decl = .{
                .value = erased_value,
                .body = erased_expr,
            } }));
            return erased_value;
        },
    }
}

fn erasedFnType(program: *const Program, ty: Type.TypeId) Type.ErasedFnType {
    return switch (program.types.getType(ty)) {
        .erased_fn => |erased_fn| erased_fn,
        else => executableInvariant("executable value transform expected erased function target"),
    };
}

fn lowerPublishedExecutableValueTransformAsBridge(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    transform_id: checked_artifact.ExecutableValueTransformPlanId,
    structural: checked_artifact.ExecutableStructuralBridgePlan,
) Allocator.Error!Ast.BridgeId {
    const plan = transforms.get(transform_id);
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    return try lowerExecutableStructuralBridgePlan(program, materialization, published_types, transforms, from_ty, to_ty, structural);
}

fn lowerExecutableValueChildBridge(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    child: checked_artifact.ExecutableValueTransformPlanId,
) Allocator.Error!Ast.BridgeId {
    const plan = transforms.get(child);
    return switch (plan.op) {
        .identity => try constructionSlotBridgeForProgram(
            program.allocator,
            program,
            &program.ast,
            try published_types.lower(plan.from.ty, plan.from.key),
            try published_types.lower(plan.to.ty, plan.to.key),
        ),
        .structural_bridge => |structural| try lowerPublishedExecutableValueTransformAsBridge(
            program,
            materialization,
            published_types,
            transforms,
            child,
            structural,
        ),
        else => executableInvariant("structural bridge child must lower to a bridge plan"),
    };
}

fn lowerExecutableStructuralBridgePlan(
    program: *Program,
    materialization: MaterializationStores,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    from_ty: Type.TypeId,
    to_ty: Type.TypeId,
    op: checked_artifact.ExecutableStructuralBridgePlan,
) Allocator.Error!Ast.BridgeId {
    switch (op) {
        .direct => return try constructionSlotBridgeForProgram(program.allocator, program, &program.ast, from_ty, to_ty),
        else => {},
    }
    const plan: Ast.BridgePlan = switch (op) {
        .direct => unreachable,
        .zst => .zst,
        .list_reinterpret => .list_reinterpret,
        .nominal_reinterpret => .nominal_reinterpret,
        .box_unbox => |child| .{ .box_unbox = try lowerExecutableValueChildBridge(program, materialization, published_types, transforms, child) },
        .box_box => |child| .{ .box_box = try lowerExecutableValueChildBridge(program, materialization, published_types, transforms, child) },
        .singleton_to_tag_union => |singleton| .{ .singleton_to_tag_union = .{
            .source_payload = from_ty,
            .target_discriminant = try tagDiscriminantForLabel(program, to_ty, try materializationTagLabel(program, materialization, singleton.target_tag)),
            .payload_plan = if (singleton.value_transform) |payload|
                try lowerExecutableValueChildBridge(program, materialization, published_types, transforms, payload)
            else blk: {
                const target_union = switch (program.types.getType(to_ty)) {
                    .tag_union => |tag_union| tag_union,
                    else => executableInvariant("executable singleton_to_tag_union bridge target was not a tag union"),
                };
                const target_label = try materializationTagLabel(program, materialization, singleton.target_tag);
                const target_tag = tagTypeForLabel(program, target_union, target_label);
                const target_payload_ty = (try tagPayloadEndpointType(program.allocator, program, target_tag)) orelse break :blk null;
                break :blk try constructionSlotBridgeForProgram(program.allocator, program, &program.ast, from_ty, target_payload_ty);
            },
        } },
        .tag_union_to_singleton => |singleton| .{ .tag_union_to_singleton = .{
            .target_payload = to_ty,
            .source_discriminant = try tagDiscriminantForLabel(program, from_ty, try materializationTagLabel(program, materialization, singleton.source_tag)),
            .payload_plan = if (singleton.value_transform) |payload|
                try lowerExecutableValueChildBridge(program, materialization, published_types, transforms, payload)
            else blk: {
                const source_union = switch (program.types.getType(from_ty)) {
                    .tag_union => |tag_union| tag_union,
                    else => executableInvariant("executable tag_union_to_singleton bridge source was not a tag union"),
                };
                const source_label = try materializationTagLabel(program, materialization, singleton.source_tag);
                const source_tag = tagTypeForLabel(program, source_union, source_label);
                const source_payload_ty = (try tagPayloadEndpointType(program.allocator, program, source_tag)) orelse break :blk null;
                break :blk try constructionSlotBridgeForProgram(program.allocator, program, &program.ast, source_payload_ty, to_ty);
            },
        } },
    };
    return try program.ast.addBridgePlan(plan);
}

fn recordFieldForLabel(
    program: *const Program,
    record: Type.RecordType,
    label: canonical.RecordFieldLabelId,
) Type.RecordFieldType {
    for (record.fields) |field| {
        if (program.row_shapes.recordField(field.field).label == label) return field;
    }
    executableInvariant("record value transform source field label is absent from source type");
}

fn tagDiscriminantForLabel(
    program: *const Program,
    ty: Type.TypeId,
    label: canonical.TagLabelId,
) Allocator.Error!u16 {
    const tag_union = switch (program.types.getType(ty)) {
        .tag_union => |tag_union| tag_union,
        else => executableInvariant("executable structural bridge expected a tag union endpoint"),
    };
    return @intCast(try tagVariantIndexForLabel(program, tag_union, label));
}

fn tagVariantIndexForLabel(
    program: *const Program,
    tag_union: Type.TagUnionType,
    label: canonical.TagLabelId,
) Allocator.Error!usize {
    for (tag_union.tags, 0..) |tag, i| {
        if (program.row_shapes.tag(tag.tag).label == label) return i;
    }
    executableInvariant("executable structural bridge tag label is absent from target type");
}

const ErasedPromotedCaptureLowering = struct {
    value: ?Ast.ExecutableValueRef = null,
    stmt: ?Ast.StmtId = null,
};

fn lowerErasedPromotedCapture(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    capture_ty: ?Type.TypeId,
    capture: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
    hidden_arg: checked_artifact.ErasedHiddenCaptureArgPlan,
) Allocator.Error!ErasedPromotedCaptureLowering {
    if (capture_ty == null) {
        switch (capture) {
            .none => {},
            else => executableInvariant("executable erased promoted wrapper has capture materialization but no hidden capture type"),
        }
        switch (hidden_arg) {
            .none => {},
            else => executableInvariant("executable erased promoted wrapper has hidden arg materialization but no hidden capture type"),
        }
        return .{};
    }
    const ty = capture_ty.?;
    switch (hidden_arg) {
        .none => executableInvariant("executable erased promoted wrapper has hidden capture type but no hidden arg"),
        .materialized_capture => {},
    }
    switch (capture) {
        .none => executableInvariant("executable erased promoted wrapper has hidden capture type but no capture materialization"),
        .zero_sized_typed => |key| {
            _ = key;
        },
        .node => |node| return try lowerErasedCaptureExecutableMaterializationNode(allocator, program, materialization, ty, node),
    }
    const value = program.ast.freshValueRef();
    const expr = try program.ast.addExpr(ty, value, .unit);
    return .{
        .value = value,
        .stmt = try program.ast.addStmt(.{ .decl = .{
            .value = value,
            .body = expr,
        } }),
    };
}

fn lowerErasedCaptureExecutableMaterializationNode(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
) Allocator.Error!ErasedPromotedCaptureLowering {
    const expr = try lowerErasedCaptureExecutableMaterializationNodeExpr(allocator, program, materialization, expected_ty, node_id);
    const value = program.ast.getExpr(expr).value;
    return .{
        .value = value,
        .stmt = try program.ast.addStmt(.{ .decl = .{
            .value = value,
            .body = expr,
        } }),
    };
}

fn lowerErasedCaptureExecutableMaterializationPlanExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    plan: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) Allocator.Error!Ast.ExprId {
    return switch (plan) {
        .none => executableInvariant("executable erased capture materialization required a value but got none"),
        .zero_sized_typed => |key| blk: {
            _ = key;
            const value = program.ast.freshValueRef();
            break :blk try program.ast.addExpr(expected_ty, value, .unit);
        },
        .node => |node| try lowerErasedCaptureExecutableMaterializationNodeExpr(allocator, program, materialization, expected_ty, node),
    };
}

fn lowerErasedCaptureExecutableMaterializationNodeExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
) Allocator.Error!Ast.ExprId {
    const node = materialization.plans.erasedCaptureExecutableMaterializationNode(node_id);
    return switch (node) {
        .pending => executableInvariant("executable erased capture materialization reached pending node"),
        .const_instance => |const_instance| try lowerConstInstanceExpr(allocator, program, materialization, expected_ty, const_instance),
        .pure_const => |pure_const| try lowerPureConstInstanceExpr(allocator, program, expected_ty, pure_const.const_instance),
        .pure_value => |pure_value| try lowerPureComptimeValueExpr(
            allocator,
            program,
            materialization,
            expected_ty,
            pure_value.schema,
            pure_value.value,
        ),
        .finite_callable_set => |finite| try lowerMaterializedFiniteCallableSetValue(allocator, program, materialization, expected_ty, finite),
        .erased_callable => |erased| try lowerMaterializedErasedCallableValue(allocator, program, materialization, expected_ty, erased),
        .tuple => |items| try lowerErasedCaptureTupleMaterialization(allocator, program, materialization, expected_ty, items),
        .record => |fields| try lowerErasedCaptureRecordMaterialization(allocator, program, materialization, expected_ty, fields),
        .tag_union => |tag| try lowerErasedCaptureTagMaterialization(allocator, program, materialization, expected_ty, tag),
        .list => |items| try lowerErasedCaptureListMaterialization(allocator, program, materialization, expected_ty, items),
        .box => |payload| try lowerErasedCaptureBoxMaterialization(allocator, program, materialization, expected_ty, payload),
        .nominal => |nominal| try lowerErasedCaptureNominalMaterialization(allocator, program, materialization, expected_ty, nominal),
        .recursive_ref => |ref| try lowerErasedCaptureExecutableMaterializationNodeExpr(allocator, program, materialization, expected_ty, ref),
    };
}

const ResolvedConstInstance = struct {
    materialization: MaterializationStores,
    instance: checked_artifact.ConstInstance,
};

fn lowerPureConstInstanceExpr(
    allocator: Allocator,
    program: *Program,
    expected_ty: Type.TypeId,
    const_instance: checked_artifact.ConstInstanceRef,
) Allocator.Error!Ast.ExprId {
    const resolved = resolveConstInstanceForExecutable(program, const_instance);
    return try lowerPureComptimeValueExpr(
        allocator,
        program,
        resolved.materialization,
        expected_ty,
        resolved.instance.schema,
        resolved.instance.value,
    );
}

fn lowerConstInstanceExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    const_instance: checked_artifact.ConstInstanceRef,
) Allocator.Error!Ast.ExprId {
    _ = materialization;
    const resolved = resolveConstInstanceForExecutable(program, const_instance);
    return try lowerComptimeValueExpr(
        allocator,
        program,
        resolved.materialization,
        expected_ty,
        resolved.instance.schema,
        resolved.instance.value,
        true,
    );
}

fn resolveConstInstanceForExecutable(
    program: *const Program,
    ref: checked_artifact.ConstInstanceRef,
) ResolvedConstInstance {
    return resolveConstInstanceInArtifactViews(program.artifact_views, ref);
}

fn resolvePublishedTransformContext(
    program: *const Program,
    ref: checked_artifact.PublishedExecutableValueTransformRef,
) PublishedTransformContext {
    return resolvePublishedTransformContextInArtifactViews(program.artifact_views, ref.artifact);
}

fn resolvePublishedTransformContextInArtifactViews(
    artifact_views: ArtifactViews,
    artifact: checked_artifact.CheckedModuleArtifactKey,
) PublishedTransformContext {
    if (artifact_views.root) |root| {
        if (artifactKeyEql(root.artifact.key, artifact)) {
            return .{
                .artifact = root.artifact.key,
                .materialization = materializationStoresForArtifact(
                    root.artifact.key,
                    &root.artifact.canonical_names,
                    &root.artifact.comptime_plans,
                    &root.artifact.comptime_values,
                ),
                .executable_type_payloads = &root.artifact.executable_type_payloads,
                .executable_value_transforms = &root.artifact.executable_value_transforms,
            };
        }
        for (root.relation_artifacts) |related| {
            if (!artifactKeyEql(related.key, artifact)) continue;
            return publishedTransformContextFromImportedView(related);
        }
    }
    for (artifact_views.imports) |imported| {
        if (!artifactKeyEql(imported.key, artifact)) continue;
        return publishedTransformContextFromImportedView(imported);
    }
    executableInvariant("executable published value transform referenced an artifact view that was not published to executable MIR");
}

fn publishedTransformContextFromImportedView(view: checked_artifact.ImportedModuleView) PublishedTransformContext {
    return .{
        .artifact = view.key,
        .materialization = materializationStoresForArtifact(
            view.key,
            view.canonical_names,
            view.comptime_plans,
            view.comptime_values,
        ),
        .executable_type_payloads = view.executable_type_payloads,
        .executable_value_transforms = view.executable_value_transforms,
    };
}

fn resolveConstInstanceInArtifactViews(
    artifact_views: ArtifactViews,
    ref: checked_artifact.ConstInstanceRef,
) ResolvedConstInstance {
    if (artifact_views.root) |root| {
        if (artifactKeyEql(root.artifact.key, ref.owner)) {
            return resolveConstInstanceInView(
                materializationStoresForArtifact(
                    root.artifact.key,
                    &root.artifact.canonical_names,
                    &root.artifact.comptime_plans,
                    &root.artifact.comptime_values,
                ),
                root.artifact.const_instances.view(),
                ref,
            );
        }
        for (root.relation_artifacts) |related| {
            if (!artifactKeyEql(related.key, ref.owner)) continue;
            return resolveConstInstanceInView(
                materializationStoresForArtifact(
                    related.key,
                    related.canonical_names,
                    related.comptime_plans,
                    related.comptime_values,
                ),
                related.const_instances,
                ref,
            );
        }
    }
    for (artifact_views.imports) |imported| {
        if (!artifactKeyEql(imported.key, ref.owner)) continue;
        return resolveConstInstanceInView(
            materializationStoresForArtifact(
                imported.key,
                imported.canonical_names,
                imported.comptime_plans,
                imported.comptime_values,
            ),
            imported.const_instances,
            ref,
        );
    }
    executableInvariant("executable constant materialization referenced an artifact that was not published to executable MIR");
}

fn materializationStoresForArtifact(
    owner: checked_artifact.CheckedModuleArtifactKey,
    canonical_names: *const canonical.CanonicalNameStore,
    plans: *const checked_artifact.CompileTimePlanStore,
    values: *const checked_artifact.CompileTimeValueStore,
) MaterializationStores {
    return .{
        .owner = owner,
        .canonical_names = canonical_names,
        .plans = plans,
        .values = values,
    };
}

fn canonicalNamesForArtifactInViews(
    artifact_views: ArtifactViews,
    artifact: checked_artifact.CheckedModuleArtifactKey,
) *const canonical.CanonicalNameStore {
    if (artifact_views.root) |root| {
        if (artifactKeyEql(root.artifact.key, artifact)) return &root.artifact.canonical_names;
        for (root.relation_artifacts) |related| {
            if (artifactKeyEql(related.key, artifact)) return related.canonical_names;
        }
    }
    for (artifact_views.imports) |imported| {
        if (artifactKeyEql(imported.key, artifact)) return imported.canonical_names;
    }
    executableInvariant("executable materialization referenced an artifact name store that was not published");
}

fn callableSetDescriptorsForArtifactInViews(
    artifact_views: ArtifactViews,
    artifact: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.CallableSetDescriptorStore {
    if (artifact_views.root) |root| {
        if (artifactKeyEql(root.artifact.key, artifact)) return &root.artifact.callable_set_descriptors;
        for (root.relation_artifacts) |related| {
            if (artifactKeyEql(related.key, artifact)) return related.callable_set_descriptors;
        }
    }
    for (artifact_views.imports) |imported| {
        if (artifactKeyEql(imported.key, artifact)) return imported.callable_set_descriptors;
    }
    return null;
}

fn resolveConstInstanceInView(
    materialization: MaterializationStores,
    instances: checked_artifact.ConstInstantiationStoreView,
    ref: checked_artifact.ConstInstanceRef,
) ResolvedConstInstance {
    if (!artifactKeyEql(instances.owner, ref.owner)) {
        executableInvariant("executable constant materialization view has wrong owning artifact");
    }
    const index: usize = @intFromEnum(ref.instance);
    if (index >= instances.instances.len) {
        executableInvariant("executable constant materialization referenced an out-of-range constant instance");
    }
    const record = instances.instances[index];
    if (!constInstantiationKeyEql(record.key, ref.key)) {
        executableInvariant("executable constant materialization instance key does not match published row");
    }
    const instance = switch (record.state) {
        .evaluated => |evaluated| evaluated,
        .reserved,
        .evaluating,
        => executableInvariant("executable constant materialization consumed an unsealed constant instance"),
    };
    return .{
        .materialization = materialization,
        .instance = instance,
    };
}

fn lowerPureComptimeValueExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    schema_id: checked_artifact.ComptimeSchemaId,
    value_id: checked_artifact.ComptimeValueId,
) Allocator.Error!Ast.ExprId {
    return try lowerComptimeValueExpr(
        allocator,
        program,
        materialization,
        expected_ty,
        schema_id,
        value_id,
        false,
    );
}

fn lowerComptimeValueExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    schema_id: checked_artifact.ComptimeSchemaId,
    value_id: checked_artifact.ComptimeValueId,
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const schema = comptimeSchema(materialization.values, schema_id);
    const value = comptimeValue(materialization.values, value_id);
    if (schema != .nominal and schema != .alias) {
        switch (program.types.getType(expected_ty)) {
            .nominal => |nominal| {
                const backing = try lowerComptimeValueExpr(
                    allocator,
                    program,
                    materialization,
                    nominal.backing,
                    schema_id,
                    value_id,
                    allow_callable,
                );
                const out = program.ast.freshValueRef();
                return try program.ast.addExpr(expected_ty, out, .{ .nominal_reinterpret = backing });
            },
            else => {},
        }
    }
    return switch (schema) {
        .pending => executableInvariant("executable pure compile-time materialization reached pending schema"),
        .zst => blk: {
            switch (value) {
                .zst => {},
                else => executableInvariant("executable pure compile-time zst materialization value mismatch"),
            }
            const out = program.ast.freshValueRef();
            break :blk try program.ast.addExpr(expected_ty, out, .unit);
        },
        .int => |precision| blk: {
            const bytes = switch (value) {
                .int_bytes => |bytes| bytes,
                else => executableInvariant("executable pure compile-time int materialization value mismatch"),
            };
            verifyExpectedIntType(program, expected_ty, precision);
            const out = program.ast.freshValueRef();
            break :blk try program.ast.addExpr(expected_ty, out, .{ .int_lit = intLiteralFromBytes(bytes, precision) });
        },
        .frac => |precision| switch (precision) {
            .f32 => blk: {
                const literal = switch (value) {
                    .f32 => |literal| literal,
                    else => executableInvariant("executable pure compile-time f32 materialization value mismatch"),
                };
                verifyExpectedPrimitive(program, expected_ty, .f32);
                const out = program.ast.freshValueRef();
                break :blk try program.ast.addExpr(expected_ty, out, .{ .frac_f32_lit = literal });
            },
            .f64 => blk: {
                const literal = switch (value) {
                    .f64 => |literal| literal,
                    else => executableInvariant("executable pure compile-time f64 materialization value mismatch"),
                };
                verifyExpectedPrimitive(program, expected_ty, .f64);
                const out = program.ast.freshValueRef();
                break :blk try program.ast.addExpr(expected_ty, out, .{ .frac_f64_lit = literal });
            },
            .dec => blk: {
                const bytes = switch (value) {
                    .dec => |bytes| bytes,
                    else => executableInvariant("executable pure compile-time decimal materialization value mismatch"),
                };
                verifyExpectedPrimitive(program, expected_ty, .dec);
                const out = program.ast.freshValueRef();
                break :blk try program.ast.addExpr(expected_ty, out, .{ .dec_lit = @as(i128, @bitCast(std.mem.readInt(u128, &bytes, .little))) });
            },
        },
        .str => blk: {
            const bytes = switch (value) {
                .str => |bytes| bytes,
                else => executableInvariant("executable pure compile-time string materialization value mismatch"),
            };
            verifyExpectedPrimitive(program, expected_ty, .str);
            const literal = try program.literal_pool.intern(bytes);
            const out = program.ast.freshValueRef();
            break :blk try program.ast.addExpr(expected_ty, out, .{ .str_lit = literal });
        },
        .list => |elem_schema| try lowerPureComptimeListExpr(allocator, program, materialization, expected_ty, elem_schema, value, allow_callable),
        .box => |payload_schema| try lowerPureComptimeBoxExpr(allocator, program, materialization, expected_ty, payload_schema, value, allow_callable),
        .tuple => |items| try lowerPureComptimeTupleExpr(allocator, program, materialization, expected_ty, items, value, allow_callable),
        .record => |fields| try lowerPureComptimeRecordExpr(allocator, program, materialization, expected_ty, fields, value, allow_callable),
        .tag_union => |variants| try lowerPureComptimeTagExpr(allocator, program, materialization, expected_ty, variants, value, allow_callable),
        .alias => |alias| try lowerPureComptimeWrappedExpr(allocator, program, materialization, expected_ty, alias.type_name, alias.backing, value, .alias, allow_callable),
        .nominal => |nominal| try lowerPureComptimeWrappedExpr(allocator, program, materialization, expected_ty, nominal.type_name, nominal.backing, value, .nominal, allow_callable),
        .callable => blk: {
            if (!allow_callable) executableInvariant("executable pure compile-time materialization contained a callable slot");
            const leaf = switch (value) {
                .callable => |leaf| leaf,
                else => executableInvariant("executable compile-time callable materialization value mismatch"),
            };
            break :blk try lowerComptimeCallableLeafExpr(allocator, program, materialization, expected_ty, leaf);
        },
    };
}

fn lowerPureComptimeListExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    elem_schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValue,
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const elem_ty = switch (program.types.getType(expected_ty)) {
        .list => |elem| elem,
        else => executableInvariant("executable pure compile-time list materialization expected List(T) type"),
    };
    const items = switch (value) {
        .list => |items| items,
        else => executableInvariant("executable pure compile-time list materialization value mismatch"),
    };
    const exprs = try allocator.alloc(Ast.ExprId, items.len);
    defer allocator.free(exprs);
    for (items, 0..) |item, i| {
        exprs[i] = try lowerComptimeValueExpr(allocator, program, materialization, elem_ty, elem_schema, item, allow_callable);
    }
    const out = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, out, .{ .list = try addListItemExprSpanForConstruction(allocator, program, &program.ast, exprs, elem_ty) });
}

fn lowerPureComptimeBoxExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    payload_schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValue,
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const payload_ty = switch (program.types.getType(expected_ty)) {
        .box => |payload| payload,
        else => executableInvariant("executable pure compile-time box materialization expected Box(T) type"),
    };
    const payload_value = switch (value) {
        .box => |payload| payload,
        else => executableInvariant("executable pure compile-time box materialization value mismatch"),
    };
    const payload_expr = try lowerComptimeValueExpr(allocator, program, materialization, payload_ty, payload_schema, payload_value, allow_callable);
    const exprs = [_]Ast.ExprId{payload_expr};
    const out = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, out, .{ .low_level = .{
        .op = .box_box,
        .rc_effect = base.LowLevel.box_box.rcEffect(),
        .args = try program.ast.addExprSpan(&exprs),
    } });
}

fn lowerPureComptimeTupleExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    schemas: []const checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValue,
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const item_tys = switch (program.types.getType(expected_ty)) {
        .tuple => |tuple| tuple,
        else => executableInvariant("executable pure compile-time tuple materialization expected tuple type"),
    };
    const items = switch (value) {
        .tuple => |items| items,
        else => executableInvariant("executable pure compile-time tuple materialization value mismatch"),
    };
    if (item_tys.len != schemas.len or item_tys.len != items.len) {
        executableInvariant("executable pure compile-time tuple materialization arity mismatch");
    }
    const exprs = try allocator.alloc(Ast.ExprId, items.len);
    defer allocator.free(exprs);
    for (items, schemas, 0..) |item, schema, i| {
        exprs[i] = try lowerComptimeValueExpr(allocator, program, materialization, item_tys[i], schema, item, allow_callable);
    }
    const out = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, out, .{ .tuple = try addTupleItemExprSpanForConstruction(allocator, program, &program.ast, exprs, item_tys) });
}

fn lowerPureComptimeRecordExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    schema_fields: []const checked_artifact.ComptimeFieldSchema,
    value: checked_artifact.ComptimeValue,
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const record_ty = switch (program.types.getType(expected_ty)) {
        .record => |record| record,
        else => executableInvariant("executable pure compile-time record materialization expected record type"),
    };
    const value_fields = switch (value) {
        .record => |fields| fields,
        else => executableInvariant("executable pure compile-time record materialization value mismatch"),
    };
    if (schema_fields.len != value_fields.len) {
        executableInvariant("executable pure compile-time record materialization schema/value field count mismatch");
    }
    const seen = try allocator.alloc(bool, schema_fields.len);
    defer allocator.free(seen);
    @memset(seen, false);
    const output_fields = try allocator.alloc(Ast.RecordFieldExpr, record_ty.fields.len);
    defer allocator.free(output_fields);
    for (record_ty.fields, 0..) |expected_field, expected_i| {
        const expected_label = program.row_shapes.recordField(expected_field.field).label;
        const materialized = (try findPureComptimeRecordField(program, materialization, schema_fields, value_fields, expected_label, seen)) orelse missing: {
            break :missing executableInvariant("executable pure compile-time record materialization missing expected field");
        };
        const lowered = try lowerComptimeValueExpr(
            allocator,
            program,
            materialization,
            expected_field.ty,
            materialized.schema,
            materialized.value,
            allow_callable,
        );
        output_fields[expected_i] = .{
            .field = expected_field.field,
            .expr = lowered,
            .ty = expected_field.ty,
            .value = program.ast.getExpr(lowered).value,
            .bridge = try constructionSlotBridgeForProgram(allocator, program, &program.ast, program.ast.getExpr(lowered).ty, expected_field.ty),
        };
    }
    const out = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, out, .{ .record = .{
        .shape = record_ty.shape,
        .fields = try program.ast.addRecordFieldExprSpan(output_fields),
    } });
}

const PureComptimeField = struct {
    schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValueId,
};

fn findPureComptimeRecordField(
    program: *Program,
    materialization: MaterializationStores,
    schemas: []const checked_artifact.ComptimeFieldSchema,
    values: []const checked_artifact.ComptimeValueId,
    label: canonical.RecordFieldLabelId,
    seen: []bool,
) Allocator.Error!?PureComptimeField {
    for (schemas, 0..) |schema, i| {
        const schema_label = try materializationRecordFieldLabel(program, materialization, schema.name);
        if (schema_label != label) continue;
        if (seen[i]) executableInvariant("executable pure compile-time record materialization duplicated field");
        seen[i] = true;
        return .{
            .schema = schema.schema,
            .value = values[i],
        };
    }
    return null;
}

fn lowerPureComptimeTagExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    schema_variants: []const checked_artifact.ComptimeVariantSchema,
    value: checked_artifact.ComptimeValue,
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const tag_union_ty = switch (program.types.getType(expected_ty)) {
        .tag_union => |tag_union| tag_union,
        else => |content| executableInvariantFmt(
            "executable pure compile-time tag materialization expected tag-union type, found {s}",
            .{@tagName(content)},
        ),
    };
    const variant_value = switch (value) {
        .tag_union => |tag| tag,
        else => executableInvariant("executable pure compile-time tag materialization value mismatch"),
    };
    const variant_index: usize = @intCast(variant_value.variant_index);
    if (variant_index >= schema_variants.len) {
        executableInvariant("executable pure compile-time tag materialization variant index exceeded schema arity");
    }
    const schema_variant = schema_variants[variant_index];
    if (schema_variant.payloads.len != variant_value.payloads.len) {
        executableInvariant("executable pure compile-time tag materialization payload count mismatch");
    }
    const selected_label = try materializationTagLabel(program, materialization, schema_variant.name);
    const selected = findPureComptimeTagType(program, tag_union_ty, selected_label) orelse {
        executableInvariant("executable pure compile-time tag materialization selected tag missing from expected type");
    };
    if (selected.payloads.len != schema_variant.payloads.len) {
        executableInvariant("executable pure compile-time tag materialization expected payload arity mismatch");
    }
    const output_payloads = try allocator.alloc(Ast.TagPayloadExpr, selected.payloads.len);
    defer allocator.free(output_payloads);
    for (selected.payloads, 0..) |expected_payload, expected_i| {
        const payload_info = program.row_shapes.tagPayload(expected_payload.payload);
        const payload_index: usize = @intCast(payload_info.logical_index);
        if (payload_index >= variant_value.payloads.len) {
            executableInvariant("executable pure compile-time tag materialization payload index exceeded stored arity");
        }
        const lowered = try lowerComptimeValueExpr(
            allocator,
            program,
            materialization,
            expected_payload.ty,
            schema_variant.payloads[payload_index],
            variant_value.payloads[payload_index],
            allow_callable,
        );
        output_payloads[expected_i] = .{
            .payload = expected_payload.payload,
            .expr = lowered,
            .ty = expected_payload.ty,
            .value = program.ast.getExpr(lowered).value,
            .bridge = try constructionSlotBridgeForProgram(allocator, program, &program.ast, program.ast.getExpr(lowered).ty, expected_payload.ty),
        };
    }
    const out = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, out, .{ .tag = .{
        .union_shape = tag_union_ty.shape,
        .tag = selected.tag,
        .payloads = try program.ast.addTagPayloadExprSpan(output_payloads),
    } });
}

fn findPureComptimeTagType(
    program: *const Program,
    tag_union_ty: Type.TagUnionType,
    label: canonical.TagLabelId,
) ?Type.TagType {
    for (tag_union_ty.tags) |tag| {
        if (program.row_shapes.tag(tag.tag).label == label) return tag;
    }
    return null;
}

fn lowerPureComptimeWrappedExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    nominal: canonical.NominalTypeKey,
    backing_schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValue,
    comptime wrapper: enum { alias, nominal },
    allow_callable: bool,
) Allocator.Error!Ast.ExprId {
    const expected_nominal = switch (program.types.getType(expected_ty)) {
        .nominal => |expected| expected,
        else => executableInvariant("executable pure compile-time wrapped materialization expected nominal type"),
    };
    const remapped_nominal = try materializationNominalTypeKey(program, materialization, nominal);
    if (!nominalTypeKeyEql(expected_nominal.nominal, remapped_nominal)) {
        executableInvariant("executable pure compile-time wrapped materialization nominal key mismatch");
    }
    const backing_value = switch (wrapper) {
        .alias => switch (value) {
            .alias => |backing| backing,
            else => executableInvariant("executable pure compile-time alias materialization value mismatch"),
        },
        .nominal => switch (value) {
            .nominal => |backing| backing,
            else => executableInvariant("executable pure compile-time nominal materialization value mismatch"),
        },
    };
    const backing = try lowerComptimeValueExpr(
        allocator,
        program,
        materialization,
        expected_nominal.backing,
        backing_schema,
        backing_value,
        allow_callable,
    );
    const out = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, out, .{ .nominal_reinterpret = backing });
}

fn lowerComptimeCallableLeafExpr(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    leaf: checked_artifact.CallableLeafInstance,
) Allocator.Error!Ast.ExprId {
    return switch (leaf) {
        .finite => |finite| try lowerComptimeFiniteCallableLeafExpr(program, expected_ty, finite),
        .erased_boxed => |erased| try lowerMaterializedErasedCallableValue(
            allocator,
            program,
            materialization,
            expected_ty,
            .{
                .source_fn_ty = erased.source_fn_ty,
                .sig_key = erased.sig_key,
                .code = erased.code,
                .capture = erased.capture,
                .provenance = erased.provenance,
            },
        ),
    };
}

fn lowerComptimeFiniteCallableLeafExpr(
    program: *Program,
    expected_ty: Type.TypeId,
    finite: checked_artifact.FiniteCallableLeafInstance,
) Allocator.Error!Ast.ExprId {
    const callable_set = switch (program.types.getType(expected_ty)) {
        .callable_set => |callable_set| callable_set,
        else => executableInvariant("executable compile-time finite callable leaf expected callable-set type"),
    };
    const member = findCallableSetMemberForProc(program, callable_set.key, finite.proc_value) orelse {
        executableInvariant("executable compile-time finite callable leaf missing callable-set member");
    };
    if (member.capture_slots.len != 0) {
        executableInvariant("executable compile-time finite callable leaf must be a closed procedure value");
    }
    const member_ty = callableSetMemberType(callable_set, member.member) orelse {
        executableInvariant("executable compile-time finite callable leaf selected member missing from expected type");
    };
    if (member_ty.payload_ty != null) {
        executableInvariant("executable compile-time finite callable leaf expected type carries capture payload");
    }
    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .callable_set_value = .{
        .construction_plan = null,
        .callable_set_key = callable_set.key,
        .member = .{
            .callable_set_key = callable_set.key,
            .member_index = member.member,
        },
        .capture_record = null,
    } });
}

fn findCallableSetMemberForProc(
    program: *const Program,
    key: repr.CanonicalCallableSetKey,
    proc_value: canonical.ProcedureCallableRef,
) ?*const repr.CanonicalCallableSetMember {
    const descriptor = programCallableSetDescriptor(program, key) orelse return null;
    for (descriptor.members) |*member| {
        if (canonical.procedureCallableRefEql(member.proc_value, proc_value)) return member;
    }
    return null;
}

fn callableSetMemberType(callable_set: Type.CallableSetType, member_id: repr.CallableSetMemberId) ?Type.CallableSetMemberType {
    for (callable_set.members) |member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn comptimeSchema(
    values: *const checked_artifact.CompileTimeValueStore,
    id: checked_artifact.ComptimeSchemaId,
) checked_artifact.ComptimeSchema {
    const index: usize = @intFromEnum(id);
    if (index >= values.schemas.items.len) {
        executableInvariant("executable pure compile-time materialization schema id out of range");
    }
    return values.schemas.items[index];
}

fn comptimeValue(
    values: *const checked_artifact.CompileTimeValueStore,
    id: checked_artifact.ComptimeValueId,
) checked_artifact.ComptimeValue {
    const index: usize = @intFromEnum(id);
    if (index >= values.values.items.len) {
        executableInvariant("executable pure compile-time materialization value id out of range");
    }
    return values.values.items[index];
}

fn verifyExpectedIntType(
    program: *const Program,
    expected_ty: Type.TypeId,
    precision: types.Int.Precision,
) void {
    verifyExpectedPrimitive(program, expected_ty, switch (precision) {
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
    });
}

fn verifyExpectedPrimitive(program: *const Program, expected_ty: Type.TypeId, expected: Type.Prim) void {
    const actual = switch (program.types.getType(expected_ty)) {
        .primitive => |prim| prim,
        else => executableInvariant("executable pure compile-time scalar materialization expected primitive type"),
    };
    if (actual != expected) {
        executableInvariant("executable pure compile-time scalar materialization primitive mismatch");
    }
}

fn intLiteralFromBytes(bytes: [16]u8, precision: types.Int.Precision) i128 {
    return switch (precision) {
        .u8 => @as(i128, @intCast(bytes[0])),
        .i8 => @as(i128, @intCast(@as(i8, @bitCast(bytes[0])))),
        .u16 => @as(i128, @intCast(std.mem.readInt(u16, bytes[0..2], .little))),
        .i16 => @as(i128, @intCast(@as(i16, @bitCast(std.mem.readInt(u16, bytes[0..2], .little))))),
        .u32 => @as(i128, @intCast(std.mem.readInt(u32, bytes[0..4], .little))),
        .i32 => @as(i128, @intCast(@as(i32, @bitCast(std.mem.readInt(u32, bytes[0..4], .little))))),
        .u64 => @as(i128, @intCast(std.mem.readInt(u64, bytes[0..8], .little))),
        .i64 => @as(i128, @intCast(@as(i64, @bitCast(std.mem.readInt(u64, bytes[0..8], .little))))),
        .u128 => @as(i128, @bitCast(std.mem.readInt(u128, &bytes, .little))),
        .i128 => @as(i128, @bitCast(std.mem.readInt(u128, &bytes, .little))),
    };
}

fn artifactKeyEql(a: checked_artifact.CheckedModuleArtifactKey, b: checked_artifact.CheckedModuleArtifactKey) bool {
    return std.mem.eql(u8, &a.bytes, &b.bytes);
}

fn constInstantiationKeyEql(
    a: checked_artifact.ConstInstantiationKey,
    b: checked_artifact.ConstInstantiationKey,
) bool {
    return constRefEql(a.const_ref, b.const_ref) and
        std.mem.eql(u8, &a.requested_source_ty.bytes, &b.requested_source_ty.bytes);
}

fn constRefEql(a: checked_artifact.ConstRef, b: checked_artifact.ConstRef) bool {
    return artifactKeyEql(a.artifact, b.artifact) and
        constOwnerEql(a.owner, b.owner) and
        a.template == b.template and
        std.mem.eql(u8, &a.source_scheme.bytes, &b.source_scheme.bytes);
}

fn constOwnerEql(a: checked_artifact.ConstOwner, b: checked_artifact.ConstOwner) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .top_level_binding => |left| blk: {
            const right = b.top_level_binding;
            break :blk left.module_idx == right.module_idx and left.pattern == right.pattern;
        },
        .promoted_capture => |left| blk: {
            const right = b.promoted_capture;
            break :blk left.capture_index == right.capture_index and
                left.promoted_proc.module_idx == right.promoted_proc.module_idx and
                canonical.procedureValueRefEql(left.promoted_proc.proc, right.promoted_proc.proc);
        },
    };
}

fn lowerErasedCaptureRecordMaterialization(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    fields: []const checked_artifact.ErasedCaptureExecutableMaterializationRecordField,
) Allocator.Error!Ast.ExprId {
    const record_ty = switch (program.types.getType(expected_ty)) {
        .record => |record| record,
        else => executableInvariant("executable erased capture record materialization expected a record type"),
    };
    if (record_ty.fields.len != fields.len) {
        executableInvariant("executable erased capture record materialization field count differs from expected type");
    }

    const seen = try allocator.alloc(bool, fields.len);
    defer allocator.free(seen);
    @memset(seen, false);

    const output_fields = try allocator.alloc(Ast.RecordFieldExpr, record_ty.fields.len);
    defer allocator.free(output_fields);
    for (record_ty.fields, 0..) |expected_field, expected_i| {
        const expected_label = program.row_shapes.recordField(expected_field.field).label;
        const materialized = (try findErasedCaptureRecordField(program, materialization, fields, expected_label, seen)) orelse {
            executableInvariant("executable erased capture record materialization missing expected field");
        };
        const lowered = try lowerErasedCaptureExecutableMaterializationPlanExpr(
            allocator,
            program,
            materialization,
            expected_field.ty,
            materialized.value,
        );
        output_fields[expected_i] = .{
            .field = expected_field.field,
            .expr = lowered,
            .ty = expected_field.ty,
            .value = program.ast.getExpr(lowered).value,
            .bridge = try constructionSlotBridgeForProgram(allocator, program, &program.ast, program.ast.getExpr(lowered).ty, expected_field.ty),
        };
    }
    verifyAllSeen(seen, "executable erased capture record materialization had extra field");

    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .record = .{
        .shape = record_ty.shape,
        .fields = try program.ast.addRecordFieldExprSpan(output_fields),
    } });
}

fn findErasedCaptureRecordField(
    program: *Program,
    materialization: MaterializationStores,
    fields: []const checked_artifact.ErasedCaptureExecutableMaterializationRecordField,
    expected_label: canonical.RecordFieldLabelId,
    seen: []bool,
) Allocator.Error!?checked_artifact.ErasedCaptureExecutableMaterializationRecordField {
    for (fields, 0..) |field, i| {
        const field_label = try materializationRecordFieldLabel(program, materialization, field.field);
        if (field_label != expected_label) continue;
        if (seen[i]) executableInvariant("executable erased capture record materialization duplicated field");
        seen[i] = true;
        return field;
    }
    return null;
}

fn lowerErasedCaptureTupleMaterialization(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    items: []const checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) Allocator.Error!Ast.ExprId {
    const tuple_tys = switch (program.types.getType(expected_ty)) {
        .tuple => |tuple| tuple,
        else => executableInvariant("executable erased capture tuple materialization expected a tuple type"),
    };
    if (tuple_tys.len != items.len) {
        executableInvariant("executable erased capture tuple materialization arity disagrees with expected type");
    }
    const exprs = try allocator.alloc(Ast.ExprId, items.len);
    defer allocator.free(exprs);
    for (items, 0..) |item, i| {
        exprs[i] = try lowerErasedCaptureExecutableMaterializationPlanExpr(allocator, program, materialization, tuple_tys[i], item);
    }
    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .tuple = try addTupleItemExprSpanForConstruction(allocator, program, &program.ast, exprs, tuple_tys) });
}

fn lowerErasedCaptureTagMaterialization(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    tag: checked_artifact.ErasedCaptureExecutableMaterializationTagNode,
) Allocator.Error!Ast.ExprId {
    const tag_union_ty = switch (program.types.getType(expected_ty)) {
        .tag_union => |tag_union| tag_union,
        else => executableInvariant("executable erased capture tag materialization expected a tag-union type"),
    };
    const selected_label = try materializationTagLabel(program, materialization, tag.tag);
    const selected = findErasedCaptureTagType(program, tag_union_ty, selected_label) orelse {
        executableInvariant("executable erased capture tag materialization selected tag missing from expected type");
    };
    if (selected.payloads.len != tag.payloads.len) {
        executableInvariant("executable erased capture tag materialization payload count differs from expected type");
    }

    const seen = try allocator.alloc(bool, tag.payloads.len);
    defer allocator.free(seen);
    @memset(seen, false);

    const output_payloads = try allocator.alloc(Ast.TagPayloadExpr, selected.payloads.len);
    defer allocator.free(output_payloads);
    for (selected.payloads, 0..) |expected_payload, expected_i| {
        const payload_info = program.row_shapes.tagPayload(expected_payload.payload);
        const materialized = findErasedCaptureTagPayload(tag.payloads, payload_info.logical_index, seen) orelse {
            executableInvariant("executable erased capture tag materialization missing expected payload");
        };
        const lowered = try lowerErasedCaptureExecutableMaterializationPlanExpr(
            allocator,
            program,
            materialization,
            expected_payload.ty,
            materialized.value,
        );
        output_payloads[expected_i] = .{
            .payload = expected_payload.payload,
            .expr = lowered,
            .ty = expected_payload.ty,
            .value = program.ast.getExpr(lowered).value,
            .bridge = try constructionSlotBridgeForProgram(allocator, program, &program.ast, program.ast.getExpr(lowered).ty, expected_payload.ty),
        };
    }
    verifyAllSeen(seen, "executable erased capture tag materialization had extra payload");

    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .tag = .{
        .union_shape = tag_union_ty.shape,
        .tag = selected.tag,
        .payloads = try program.ast.addTagPayloadExprSpan(output_payloads),
    } });
}

fn findErasedCaptureTagType(
    program: *const Program,
    tag_union_ty: Type.TagUnionType,
    expected_label: canonical.TagLabelId,
) ?Type.TagType {
    for (tag_union_ty.tags) |tag| {
        if (program.row_shapes.tag(tag.tag).label == expected_label) return tag;
    }
    return null;
}

fn findErasedCaptureTagPayload(
    payloads: []const checked_artifact.ErasedCaptureExecutableMaterializationTagPayload,
    expected_index: u32,
    seen: []bool,
) ?checked_artifact.ErasedCaptureExecutableMaterializationTagPayload {
    for (payloads, 0..) |payload, i| {
        if (payload.index != expected_index) continue;
        if (seen[i]) executableInvariant("executable erased capture tag materialization duplicated payload");
        seen[i] = true;
        return payload;
    }
    return null;
}

fn lowerErasedCaptureListMaterialization(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    items: []const checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) Allocator.Error!Ast.ExprId {
    const elem_ty = switch (program.types.getType(expected_ty)) {
        .list => |elem| elem,
        else => executableInvariant("executable erased capture list materialization expected a list type"),
    };
    const exprs = try allocator.alloc(Ast.ExprId, items.len);
    defer allocator.free(exprs);
    for (items, 0..) |item, i| {
        exprs[i] = try lowerErasedCaptureExecutableMaterializationPlanExpr(allocator, program, materialization, elem_ty, item);
    }
    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .list = try addListItemExprSpanForConstruction(allocator, program, &program.ast, exprs, elem_ty) });
}

fn lowerErasedCaptureBoxMaterialization(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    payload: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) Allocator.Error!Ast.ExprId {
    const payload_ty = switch (program.types.getType(expected_ty)) {
        .box => |payload_ty| payload_ty,
        else => executableInvariant("executable erased capture box materialization expected Box(T) type"),
    };
    const payload_expr = try lowerErasedCaptureExecutableMaterializationPlanExpr(allocator, program, materialization, payload_ty, payload);
    const exprs = [_]Ast.ExprId{payload_expr};
    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .low_level = .{
        .op = .box_box,
        .rc_effect = base.LowLevel.box_box.rcEffect(),
        .args = try program.ast.addExprSpan(&exprs),
    } });
}

fn lowerErasedCaptureNominalMaterialization(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    nominal: anytype,
) Allocator.Error!Ast.ExprId {
    const expected_nominal = switch (program.types.getType(expected_ty)) {
        .nominal => |expected_nominal| expected_nominal,
        else => executableInvariant("executable erased capture nominal materialization expected a nominal type"),
    };
    const remapped_nominal = try materializationNominalTypeKey(program, materialization, nominal.nominal);
    if (!nominalTypeKeyEql(expected_nominal.nominal, remapped_nominal)) {
        executableInvariant("executable erased capture nominal materialization nominal type differs from expected type");
    }
    const backing = try lowerErasedCaptureExecutableMaterializationPlanExpr(
        allocator,
        program,
        materialization,
        expected_nominal.backing,
        nominal.backing,
    );
    const value = program.ast.freshValueRef();
    return try program.ast.addExpr(expected_ty, value, .{ .nominal_reinterpret = backing });
}

fn nominalTypeKeyEql(a: canonical.NominalTypeKey, b: canonical.NominalTypeKey) bool {
    return a.module_name == b.module_name and a.type_name == b.type_name;
}

fn materializationRecordFieldLabel(
    program: *Program,
    materialization: MaterializationStores,
    label: canonical.RecordFieldLabelId,
) Allocator.Error!canonical.RecordFieldLabelId {
    return try program.canonical_names.internRecordFieldLabel(sourceRecordFieldLabelText(materialization.canonical_names, label));
}

fn materializationTagLabel(
    program: *Program,
    materialization: MaterializationStores,
    label: canonical.TagLabelId,
) Allocator.Error!canonical.TagLabelId {
    return try program.canonical_names.internTagLabel(sourceTagLabelText(materialization.canonical_names, label));
}

fn materializationNominalTypeKey(
    program: *Program,
    materialization: MaterializationStores,
    nominal: canonical.NominalTypeKey,
) Allocator.Error!canonical.NominalTypeKey {
    return .{
        .module_name = try program.canonical_names.internModuleName(sourceModuleNameText(materialization.canonical_names, nominal.module_name)),
        .type_name = try program.canonical_names.internTypeName(sourceTypeNameText(materialization.canonical_names, nominal.type_name)),
    };
}

fn sourceModuleNameText(names: *const canonical.CanonicalNameStore, id: canonical.ModuleNameId) []const u8 {
    const index: usize = @intFromEnum(id);
    if (index >= names.module_names.items.len) executableInvariant("executable materialization module name id is outside owning artifact name table");
    return names.module_names.items[index];
}

fn sourceTypeNameText(names: *const canonical.CanonicalNameStore, id: canonical.TypeNameId) []const u8 {
    const index: usize = @intFromEnum(id);
    if (index >= names.type_names.items.len) executableInvariant("executable materialization type name id is outside owning artifact name table");
    return names.type_names.items[index];
}

fn sourceRecordFieldLabelText(names: *const canonical.CanonicalNameStore, id: canonical.RecordFieldLabelId) []const u8 {
    const index: usize = @intFromEnum(id);
    if (index >= names.record_field_labels.items.len) executableInvariant("executable materialization record field label id is outside owning artifact name table");
    return names.record_field_labels.items[index];
}

fn sourceTagLabelText(names: *const canonical.CanonicalNameStore, id: canonical.TagLabelId) []const u8 {
    const index: usize = @intFromEnum(id);
    if (index >= names.tag_labels.items.len) executableInvariant("executable materialization tag label id is outside owning artifact name table");
    return names.tag_labels.items[index];
}

fn verifyAllSeen(seen: []const bool, comptime message: []const u8) void {
    for (seen) |was_seen| {
        if (!was_seen) executableInvariant(message);
    }
}

fn lowerMaterializedFiniteCallableSetValue(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    finite: checked_artifact.MaterializedFiniteCallableSetValue,
) Allocator.Error!Ast.ExprId {
    const callable_set = switch (program.types.getType(expected_ty)) {
        .callable_set => |callable_set| callable_set,
        else => executableInvariant("executable erased finite capture materialization expected callable-set type"),
    };
    if (!repr.callableSetKeyEql(callable_set.key, finite.callable_set_key)) {
        executableInvariant("executable erased finite capture materialization callable-set key differs from expected type");
    }
    const descriptor_member = materializedCallableSetMember(program, materialization.owner, finite.callable_set_key, finite.selected_member) orelse {
        executableInvariant("executable erased finite capture materialization selected missing callable-set member");
    };
    if (descriptor_member.capture_slots.len != finite.captures.len) {
        executableInvariant("executable erased finite capture materialization capture count differs from descriptor");
    }
    var member_type: ?Type.CallableSetMemberType = null;
    for (callable_set.members) |candidate| {
        if (candidate.member == finite.selected_member) {
            member_type = candidate;
            break;
        }
    }
    const selected_member_type = member_type orelse {
        executableInvariant("executable erased finite capture materialization selected member missing from expected type");
    };

    const capture_refs = try allocator.alloc(Ast.CaptureValueRef, finite.captures.len);
    defer allocator.free(capture_refs);
    const stmt_ids = try allocator.alloc(Ast.StmtId, finite.captures.len);
    defer allocator.free(stmt_ids);

    if (finite.captures.len == 0) {
        if (selected_member_type.payload_ty != null) {
            executableInvariant("executable erased finite capture materialization has no captures but expected member has payload type");
        }
    } else {
        const payload_ty = selected_member_type.payload_ty orelse {
            executableInvariant("executable erased finite capture materialization has captures but expected member has no payload type");
        };
        const capture_tys = switch (program.types.getType(payload_ty)) {
            .tuple => |tuple| tuple,
            else => executableInvariant("executable erased finite capture materialization expected tuple payload type"),
        };
        if (capture_tys.len != finite.captures.len) {
            executableInvariant("executable erased finite capture materialization payload type arity differs from captures");
        }
        for (finite.captures, 0..) |capture, i| {
            const lowered = try lowerErasedCaptureExecutableMaterializationPlanExpr(
                allocator,
                program,
                materialization,
                capture_tys[i],
                capture,
            );
            const value = program.ast.getExpr(lowered).value;
            capture_refs[i] = .{
                .slot = @intCast(i),
                .value = value,
                .exec_ty = capture_tys[i],
            };
            stmt_ids[i] = try program.ast.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }
    }

    const value = program.ast.freshValueRef();
    const final_expr = try program.ast.addExpr(expected_ty, value, .{ .callable_set_value = .{
        .construction_plan = null,
        .callable_set_key = finite.callable_set_key,
        .member = .{
            .callable_set_key = finite.callable_set_key,
            .member_index = finite.selected_member,
        },
        .capture_record = if (capture_refs.len == 0) null else .{
            .capture_shape_key = descriptor_member.capture_shape_key,
            .values = try program.ast.addCaptureValueRefSpan(capture_refs),
            .record_tmp = program.ast.freshValueRef(),
        },
    } });
    if (stmt_ids.len == 0) return final_expr;
    return try program.ast.addExpr(expected_ty, value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(stmt_ids),
        .final_expr = final_expr,
    } });
}

const MaterializedCallableSetMember = struct {
    capture_slots: []const repr.CallableSetCaptureSlot,
    capture_shape_key: repr.CaptureShapeKey,
};

fn materializedCallableSetMember(
    program: *const Program,
    owner: checked_artifact.CheckedModuleArtifactKey,
    key: repr.CanonicalCallableSetKey,
    member_id: repr.CallableSetMemberId,
) ?MaterializedCallableSetMember {
    if (programCallableSetMember(program, key, member_id)) |member| {
        return .{
            .capture_slots = member.capture_slots,
            .capture_shape_key = member.capture_shape_key,
        };
    }
    const descriptors = callableSetDescriptorsForArtifactInViews(program.artifact_views, owner) orelse return null;
    const descriptor = descriptors.descriptorFor(key) orelse return null;
    for (descriptor.members) |member| {
        if (member.member != member_id) continue;
        return .{
            .capture_slots = member.capture_slots,
            .capture_shape_key = member.capture_shape_key,
        };
    }
    return null;
}

fn lowerMaterializedErasedCallableValue(
    allocator: Allocator,
    program: *Program,
    materialization: MaterializationStores,
    expected_ty: Type.TypeId,
    erased: checked_artifact.MaterializedErasedCallableValue,
) Allocator.Error!Ast.ExprId {
    const erased_ty = switch (program.types.getType(expected_ty)) {
        .erased_fn => |erased_fn| erased_fn,
        else => executableInvariant("executable erased callable materialization expected erased-fn type"),
    };
    if (!repr.erasedFnSigKeyEql(erased_ty.sig_key, erased.sig_key)) {
        executableInvariant("executable erased callable materialization signature differs from expected type");
    }
    const capture_expr: ?Ast.ExprId = if (erased.sig_key.capture_ty) |_| blk: {
        const capture_ty = erased_ty.capture_ty orelse {
            executableInvariant("executable erased callable materialization expected type has no capture type");
        };
        break :blk try lowerErasedCaptureExecutableMaterializationPlanExpr(allocator, program, materialization, capture_ty, erased.capture);
    } else null;

    const stmt_count: usize = if (capture_expr == null) 0 else 1;
    const stmt_ids = try allocator.alloc(Ast.StmtId, stmt_count);
    defer allocator.free(stmt_ids);
    const capture_ref: ?Ast.ExecutableValueRef = if (capture_expr) |expr| blk: {
        const value = program.ast.getExpr(expr).value;
        stmt_ids[0] = try program.ast.addStmt(.{ .decl = .{
            .value = value,
            .body = expr,
        } });
        break :blk value;
    } else null;

    const value = program.ast.freshValueRef();
    const packed_fn = try program.ast.addExpr(expected_ty, value, .{ .packed_erased_fn = .{
        .sig_key = erased.sig_key,
        .code = executableProcForErasedCode(program, erased.code),
        .capture = capture_ref,
        .capture_ty = erased_ty.capture_ty,
        .capture_shape = erased_ty.capture_shape,
    } });
    if (stmt_ids.len == 0) return packed_fn;
    return try program.ast.addExpr(expected_ty, value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(stmt_ids),
        .final_expr = packed_fn,
    } });
}

fn executableProcForErasedCode(
    program: *const Program,
    code: canonical.ErasedCallableCodeRef,
) Ast.ExecutableProcId {
    return switch (code) {
        .direct_proc_value => |direct| executableProcForErasedDirectProcAdapter(program, direct),
        .finite_set_adapter => |adapter| executableProcForErasedAdapter(program, adapter),
    };
}

fn executableProcRecord(
    program: *const Program,
    executable_proc: Ast.ExecutableProcId,
) Proc {
    for (program.procs.items) |proc| {
        if (proc.executable_proc == executable_proc) return proc;
    }
    executableInvariant("executable proc record lookup reached an unreserved proc");
}

fn executableProcForSpecializationKey(
    program: *const Program,
    key: repr.ExecutableSpecializationKey,
) Ast.ExecutableProcId {
    for (program.procs.items) |proc| {
        const def = program.ast.defs.items[@intFromEnum(proc.body)];
        if (repr.executableSpecializationKeyEql(def.specialization_key, key)) return proc.executable_proc;
    }
    executableInvariant("executable value transform referenced an unreserved executable specialization");
}

fn executableProcForErasedAdapter(
    program: *const Program,
    adapter: repr.ErasedAdapterKey,
) Ast.ExecutableProcId {
    for (program.erased_adapter_procs.items) |proc| {
        if (erasedAdapterKeyEql(proc.key, adapter)) return proc.executable_proc;
    }
    executableInvariant("executable erased callable referenced an unreserved finite-set adapter");
}

fn executableProcForErasedDirectProcAdapter(
    program: *const Program,
    code: canonical.ErasedDirectProcCodeRef,
) Ast.ExecutableProcId {
    for (program.erased_direct_proc_adapters.items) |proc| {
        if (erasedDirectProcCodeRefEql(proc.code, code)) return proc.executable_proc;
    }
    executableInvariant("executable erased callable referenced an unreserved direct erased entry adapter");
}

const PublishedTypeLowerer = struct {
    allocator: Allocator,
    payloads: *const checked_artifact.ExecutableTypePayloadStore,
    source_names: *const canonical.CanonicalNameStore,
    lowering_names: *canonical.CanonicalNameStore,
    output: *Type.Store,
    row_shapes: *MonoRow.Store,
    lowered_by_key: *std.AutoHashMap(repr.CanonicalExecValueTypeKey, Type.TypeId),
    active: std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, Type.TypeId),

    fn init(
        allocator: Allocator,
        payloads: *const checked_artifact.ExecutableTypePayloadStore,
        source_names: *const canonical.CanonicalNameStore,
        lowering_names: *canonical.CanonicalNameStore,
        output: *Type.Store,
        row_shapes: *MonoRow.Store,
        lowered_by_key: *std.AutoHashMap(repr.CanonicalExecValueTypeKey, Type.TypeId),
    ) PublishedTypeLowerer {
        return .{
            .allocator = allocator,
            .payloads = payloads,
            .source_names = source_names,
            .lowering_names = lowering_names,
            .output = output,
            .row_shapes = row_shapes,
            .lowered_by_key = lowered_by_key,
            .active = std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, Type.TypeId).init(allocator),
        };
    }

    fn deinit(self: *PublishedTypeLowerer) void {
        self.active.deinit();
    }

    fn lower(
        self: *PublishedTypeLowerer,
        ref: checked_artifact.ExecutableTypePayloadRef,
        expected_key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!Type.TypeId {
        if (@intFromEnum(ref.payload) >= self.payloads.entries.len) {
            executableInvariant("executable published type payload ref is out of range");
        }
        const actual_key = self.payloads.keyFor(ref.payload);
        if (!repr.canonicalExecValueTypeKeyEql(actual_key, expected_key)) {
            executableInvariant("executable published type payload key differs from endpoint key");
        }
        if (self.lowered_by_key.get(expected_key)) |existing| return existing;
        return try self.lowerPayloadWithKey(ref.payload, expected_key);
    }

    fn lowerPayload(
        self: *PublishedTypeLowerer,
        id: checked_artifact.ExecutableTypePayloadId,
    ) Allocator.Error!Type.TypeId {
        return try self.lowerPayloadWithKey(id, self.payloads.keyFor(id));
    }

    fn lowerPayloadWithKey(
        self: *PublishedTypeLowerer,
        id: checked_artifact.ExecutableTypePayloadId,
        key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!Type.TypeId {
        if (self.lowered_by_key.get(key)) |existing| return existing;
        if (self.active.get(id)) |existing| return existing;

        const ty = try self.output.addType(.placeholder);
        try self.active.put(id, ty);
        errdefer _ = self.active.remove(id);
        try self.lowered_by_key.put(key, ty);
        errdefer _ = self.lowered_by_key.remove(key);

        const lowered = try self.lowerPayloadContent(self.payloads.get(id));
        self.output.types.items[@intFromEnum(ty)] = lowered;
        _ = self.active.remove(id);
        return ty;
    }

    fn lowerPayloadContent(
        self: *PublishedTypeLowerer,
        payload: checked_artifact.ExecutableTypePayload,
    ) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => executableInvariant("executable published type payload was pending"),
            .primitive => |prim| .{ .primitive = publishedPrimitive(prim) },
            .record => |fields| try self.lowerRecordPayload(fields),
            .tuple => |items| .{ .tuple = try self.lowerTuplePayload(items) },
            .tag_union => |variants| try self.lowerTagUnionPayload(variants),
            .list => |child| .{ .list = try self.lower(child.ty, child.key) },
            .box => |child| .{ .box = try self.lower(child.ty, child.key) },
            .nominal => |nominal| .{ .nominal = .{
                .nominal = try self.remapNominalTypeKey(nominal.nominal),
                .source_ty = nominal.source_ty,
                .backing = try self.lower(nominal.backing, nominal.backing_key),
            } },
            .callable_set => |callable_set| try self.lowerCallableSetPayload(callable_set),
            .erased_fn => |erased| .{ .erased_fn = .{
                .sig_key = erased.sig_key,
                .capture_shape = erased.capture_shape_key,
                .capture_ty = if (erased.capture_ty) |capture| blk: {
                    const capture_key = erased.capture_ty_key orelse executableInvariant("executable erased payload capture ref has no key");
                    break :blk try self.lower(capture, capture_key);
                } else null,
            } },
            .vacant_callable_slot => .vacant_callable_slot,
            .recursive_ref => |ref| .{ .link = try self.lowerPayload(ref) },
        };
    }

    fn lowerRecordPayload(
        self: *PublishedTypeLowerer,
        fields: []const checked_artifact.ExecutableRecordFieldPayload,
    ) Allocator.Error!Type.Content {
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, fields.len);
        defer self.allocator.free(labels);
        for (fields, 0..) |field, i| labels[i] = try self.remapRecordFieldLabel(field.field);
        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        if (self.row_shapes.recordShapeFields(shape).len != fields.len) {
            executableInvariant("executable published record payload shape arity mismatch");
        }

        const out = try self.allocator.alloc(Type.RecordFieldType, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .field = self.recordFieldInShape(shape, labels[i]),
                .ty = try self.lower(field.ty, field.key),
            };
        }
        return .{ .record = .{
            .shape = shape,
            .fields = out,
        } };
    }

    fn lowerTuplePayload(
        self: *PublishedTypeLowerer,
        items: []const checked_artifact.ExecutableTupleElemPayload,
    ) Allocator.Error![]const Type.TypeId {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, items.len);
        errdefer self.allocator.free(out);
        const seen = try self.allocator.alloc(bool, items.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (items) |item| {
            const index: usize = @intCast(item.index);
            if (index >= items.len) executableInvariant("executable published tuple payload index exceeded arity");
            if (seen[index]) executableInvariant("executable published tuple payload index was duplicated");
            out[index] = try self.lower(item.ty, item.key);
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable published tuple payload was not dense");
        }
        return out;
    }

    fn lowerTagUnionPayload(
        self: *PublishedTypeLowerer,
        variants: []const checked_artifact.ExecutableTagVariantPayload,
    ) Allocator.Error!Type.Content {
        const descriptors = try self.allocator.alloc(MonoRow.Store.TagShapeDescriptor, variants.len);
        defer self.allocator.free(descriptors);
        for (variants, 0..) |variant, i| {
            descriptors[i] = .{
                .name = try self.remapTagLabel(variant.tag),
                .payload_arity = @intCast(variant.payloads.len),
            };
        }
        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        if (self.row_shapes.tagUnionTags(shape).len != variants.len) executableInvariant("executable published tag payload shape arity mismatch");

        const out = try self.allocator.alloc(Type.TagType, variants.len);
        for (out) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.payloads);
            self.allocator.free(out);
        }
        for (variants, 0..) |variant, i| {
            const shape_tag = self.tagInShape(shape, descriptors[i].name);
            const payloads = try self.lowerTagPayloads(shape_tag, variant.payloads);
            out[i] = .{
                .tag = shape_tag,
                .payloads = payloads,
            };
        }
        return .{ .tag_union = .{
            .shape = shape,
            .tags = out,
        } };
    }

    fn lowerTagPayloads(
        self: *PublishedTypeLowerer,
        tag: MonoRow.TagId,
        payloads: []const checked_artifact.ExecutableTagPayload,
    ) Allocator.Error![]const Type.TagPayloadType {
        if (payloads.len == 0) return &.{};
        const shape_payloads = self.row_shapes.tagPayloads(tag);
        if (shape_payloads.len != payloads.len) executableInvariant("executable published tag payload arity mismatch");
        const out = try self.allocator.alloc(Type.TagPayloadType, payloads.len);
        errdefer self.allocator.free(out);
        for (payloads, 0..) |payload, i| {
            if (payload.index != i) executableInvariant("executable published tag payload indexes are not canonical");
            out[i] = .{
                .payload = shape_payloads[i],
                .ty = try self.lower(payload.ty, payload.key),
            };
        }
        return out;
    }

    fn recordFieldInShape(
        self: *PublishedTypeLowerer,
        shape: MonoRow.RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) MonoRow.RecordFieldId {
        for (self.row_shapes.recordShapeFields(shape)) |field_id| {
            if (self.row_shapes.recordField(field_id).label == label) return field_id;
        }
        executableInvariant("executable published record payload label missing from interned shape");
    }

    fn tagInShape(
        self: *PublishedTypeLowerer,
        shape: MonoRow.TagUnionShapeId,
        label: canonical.TagLabelId,
    ) MonoRow.TagId {
        for (self.row_shapes.tagUnionTags(shape)) |tag_id| {
            if (self.row_shapes.tag(tag_id).label == label) return tag_id;
        }
        executableInvariant("executable published tag payload label missing from interned shape");
    }

    fn lowerCallableSetPayload(
        self: *PublishedTypeLowerer,
        callable_set: checked_artifact.ExecutableCallableSetPayload,
    ) Allocator.Error!Type.Content {
        const members = try self.allocator.alloc(Type.CallableSetMemberType, callable_set.members.len);
        errdefer self.allocator.free(members);
        for (callable_set.members, 0..) |member, i| {
            members[i] = .{
                .member = member.member,
                .payload_ty = if (member.payload_ty) |payload| blk: {
                    const payload_key = member.payload_ty_key orelse executableInvariant("executable callable-set member payload ref has no key");
                    break :blk try self.lower(payload, payload_key);
                } else null,
            };
        }
        return .{ .callable_set = .{
            .key = callable_set.key,
            .members = members,
        } };
    }

    fn remapRecordFieldLabel(
        self: *PublishedTypeLowerer,
        label: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.lowering_names.internRecordFieldLabel(sourceRecordFieldLabelText(self.source_names, label));
    }

    fn remapTagLabel(
        self: *PublishedTypeLowerer,
        tag: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.lowering_names.internTagLabel(sourceTagLabelText(self.source_names, tag));
    }

    fn remapNominalTypeKey(
        self: *PublishedTypeLowerer,
        nominal: canonical.NominalTypeKey,
    ) Allocator.Error!canonical.NominalTypeKey {
        return .{
            .module_name = try self.lowering_names.internModuleName(sourceModuleNameText(self.source_names, nominal.module_name)),
            .type_name = try self.lowering_names.internTypeName(sourceTypeNameText(self.source_names, nominal.type_name)),
        };
    }
};

const SessionTypeLowerer = struct {
    allocator: Allocator,
    payloads: *const repr.SessionExecutableTypePayloadStore,
    output: *Type.Store,
    lowered_by_key: *std.AutoHashMap(repr.CanonicalExecValueTypeKey, Type.TypeId),
    active: std.AutoHashMap(repr.SessionExecutableTypePayloadId, Type.TypeId),

    fn init(
        allocator: Allocator,
        payloads: *const repr.SessionExecutableTypePayloadStore,
        output: *Type.Store,
        lowered_by_key: *std.AutoHashMap(repr.CanonicalExecValueTypeKey, Type.TypeId),
    ) SessionTypeLowerer {
        return .{
            .allocator = allocator,
            .payloads = payloads,
            .output = output,
            .lowered_by_key = lowered_by_key,
            .active = std.AutoHashMap(repr.SessionExecutableTypePayloadId, Type.TypeId).init(allocator),
        };
    }

    fn deinit(self: *SessionTypeLowerer) void {
        self.active.deinit();
    }

    fn lower(
        self: *SessionTypeLowerer,
        ref: repr.SessionExecutableTypePayloadRef,
        expected_key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!Type.TypeId {
        if (@intFromEnum(ref.payload) >= self.payloads.entries.len) {
            executableInvariant("executable session type payload ref is out of range");
        }
        const actual_key = self.payloads.keyFor(ref.payload);
        if (!repr.canonicalExecValueTypeKeyEql(actual_key, expected_key)) {
            executableInvariant("executable session type payload key differs from endpoint key");
        }
        if (self.lowered_by_key.get(expected_key)) |existing| return existing;
        return try self.lowerPayloadWithKey(ref.payload, expected_key);
    }

    fn lowerPayload(
        self: *SessionTypeLowerer,
        id: repr.SessionExecutableTypePayloadId,
    ) Allocator.Error!Type.TypeId {
        return try self.lowerPayloadWithKey(id, self.payloads.keyFor(id));
    }

    fn lowerPayloadWithKey(
        self: *SessionTypeLowerer,
        id: repr.SessionExecutableTypePayloadId,
        key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!Type.TypeId {
        if (self.lowered_by_key.get(key)) |existing| return existing;
        if (self.active.get(id)) |existing| return existing;

        const ty = try self.output.addType(.placeholder);
        try self.active.put(id, ty);
        errdefer _ = self.active.remove(id);
        try self.lowered_by_key.put(key, ty);
        errdefer _ = self.lowered_by_key.remove(key);

        const lowered = try self.lowerPayloadContent(self.payloads.get(id));
        self.output.types.items[@intFromEnum(ty)] = lowered;
        _ = self.active.remove(id);
        return ty;
    }

    fn lowerPayloadContent(
        self: *SessionTypeLowerer,
        payload: repr.SessionExecutableTypePayload,
    ) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => executableInvariant("executable session type payload was pending"),
            .primitive => |prim| .{ .primitive = publishedPrimitive(prim) },
            .record => |record| try self.lowerRecordPayload(record),
            .tuple => |items| .{ .tuple = try self.lowerTuplePayload(items) },
            .tag_union => |tag_union| try self.lowerTagUnionPayload(tag_union),
            .list => |child| .{ .list = try self.lower(child.ty, child.key) },
            .box => |child| .{ .box = try self.lower(child.ty, child.key) },
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .source_ty = nominal.source_ty,
                .backing = try self.lower(nominal.backing, nominal.backing_key),
            } },
            .callable_set => |callable_set| try self.lowerCallableSetPayload(callable_set),
            .erased_fn => |erased| .{ .erased_fn = .{
                .sig_key = erased.sig_key,
                .capture_shape = erased.capture_shape_key,
                .capture_ty = if (erased.capture_ty) |capture| blk: {
                    const capture_key = erased.capture_ty_key orelse executableInvariant("executable session erased payload capture ref has no key");
                    break :blk try self.lower(capture, capture_key);
                } else null,
            } },
            .vacant_callable_slot => .vacant_callable_slot,
            .recursive_ref => |ref_id| .{ .link = try self.lowerPayload(ref_id) },
        };
    }

    fn lowerRecordPayload(
        self: *SessionTypeLowerer,
        record: repr.SessionExecutableRecordPayload,
    ) Allocator.Error!Type.Content {
        if (record.fields.len == 0) return .{ .record = .{
            .shape = record.shape,
            .fields = &.{},
        } };
        const out = try self.allocator.alloc(Type.RecordFieldType, record.fields.len);
        errdefer self.allocator.free(out);
        for (record.fields, 0..) |field, i| {
            out[i] = .{
                .field = field.field,
                .ty = try self.lower(field.ty, field.key),
            };
        }
        return .{ .record = .{
            .shape = record.shape,
            .fields = out,
        } };
    }

    fn lowerTuplePayload(
        self: *SessionTypeLowerer,
        items: []const repr.SessionExecutableTupleElemPayload,
    ) Allocator.Error![]const Type.TypeId {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, items.len);
        errdefer self.allocator.free(out);
        const seen = try self.allocator.alloc(bool, items.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (items) |item| {
            const index: usize = @intCast(item.index);
            if (index >= items.len) executableInvariant("executable session tuple payload index exceeded arity");
            if (seen[index]) executableInvariant("executable session tuple payload index was duplicated");
            out[index] = try self.lower(item.ty, item.key);
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable session tuple payload was not dense");
        }
        return out;
    }

    fn lowerTagUnionPayload(
        self: *SessionTypeLowerer,
        tag_union: repr.SessionExecutableTagUnionPayload,
    ) Allocator.Error!Type.Content {
        if (tag_union.variants.len == 0) return .{ .tag_union = .{
            .shape = tag_union.shape,
            .tags = &.{},
        } };
        const out = try self.allocator.alloc(Type.TagType, tag_union.variants.len);
        for (out) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.payloads);
            self.allocator.free(out);
        }
        for (tag_union.variants, 0..) |variant, i| {
            out[i] = .{
                .tag = variant.tag,
                .payloads = try self.lowerTagPayloads(variant.payloads),
            };
        }
        return .{ .tag_union = .{
            .shape = tag_union.shape,
            .tags = out,
        } };
    }

    fn lowerTagPayloads(
        self: *SessionTypeLowerer,
        payloads: []const repr.SessionExecutableTagPayload,
    ) Allocator.Error![]const Type.TagPayloadType {
        if (payloads.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TagPayloadType, payloads.len);
        errdefer self.allocator.free(out);
        for (payloads, 0..) |payload, i| {
            out[i] = .{
                .payload = payload.payload,
                .ty = try self.lower(payload.ty, payload.key),
            };
        }
        return out;
    }

    fn lowerCallableSetPayload(
        self: *SessionTypeLowerer,
        callable_set: repr.SessionExecutableCallableSetPayload,
    ) Allocator.Error!Type.Content {
        if (callable_set.members.len == 0) return .{ .callable_set = .{
            .key = callable_set.key,
            .members = &.{},
        } };
        const members = try self.allocator.alloc(Type.CallableSetMemberType, callable_set.members.len);
        errdefer self.allocator.free(members);
        for (callable_set.members, 0..) |member, i| {
            members[i] = .{
                .member = member.member,
                .payload_ty = if (member.payload_ty) |payload| blk: {
                    const payload_key = member.payload_ty_key orelse executableInvariant("executable session callable-set member payload ref has no key");
                    break :blk try self.lower(payload, payload_key);
                } else null,
            };
        }
        return .{ .callable_set = .{
            .key = callable_set.key,
            .members = members,
        } };
    }
};

fn publishedPrimitive(prim: checked_artifact.ExecutablePrimitive) Type.Prim {
    return switch (prim) {
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
        .erased => .erased,
    };
}

const TypeLowerer = struct {
    allocator: Allocator,
    input: *const LambdaSolved.Type.Store,
    output: *Type.Store,
    row_shapes: *MonoRow.Store,
    active: std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId),
    lowered: std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId),

    fn init(
        allocator: Allocator,
        input: *const LambdaSolved.Type.Store,
        output: *Type.Store,
        row_shapes: *MonoRow.Store,
    ) TypeLowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .row_shapes = row_shapes,
            .active = std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId).init(allocator),
            .lowered = std.AutoHashMap(LambdaSolved.Type.TypeVarId, Type.TypeId).init(allocator),
        };
    }

    fn deinit(self: *TypeLowerer) void {
        self.lowered.deinit();
        self.active.deinit();
    }

    fn lowerType(self: *TypeLowerer, source: LambdaSolved.Type.TypeVarId) Allocator.Error!Type.TypeId {
        const root = self.input.unlinkConst(source);
        if (self.lowered.get(root)) |existing| return existing;
        if (self.active.get(root)) |existing| return existing;

        const target = try self.output.addType(.placeholder);
        try self.active.put(root, target);
        errdefer _ = self.active.remove(root);

        const lowered: Type.Content = switch (self.input.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => executableInvariant("executable type lowering received unresolved lambda-solved type"),
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .source_ty = nominal.source_ty,
                .backing = try self.lowerType(nominal.backing),
            } },
            .content => |content| switch (content) {
                .primitive => |prim| .{ .primitive = prim },
                .list => |elem| .{ .list = try self.lowerType(elem) },
                .box => |elem| .{ .box = try self.lowerType(elem) },
                .tuple => |span| blk: {
                    const source_items = self.input.sliceTypeVarSpan(span);
                    const items = try self.allocator.alloc(Type.TypeId, source_items.len);
                    defer self.allocator.free(items);
                    for (source_items, 0..) |item, i| {
                        items[i] = try self.lowerType(item);
                    }
                    break :blk .{ .tuple = try self.allocator.dupe(Type.TypeId, items) };
                },
                .func => executableInvariant("executable type lowering requires solved callable representation for function type"),
                .record => |record| try self.lowerRecordType(record.fields),
                .tag_union => |tag_union| try self.lowerTagUnionType(tag_union.tags),
            },
        };

        self.output.types.items[@intFromEnum(target)] = lowered;
        _ = self.active.remove(root);
        try self.lowered.put(root, target);
        return target;
    }

    fn lowerRecordType(self: *TypeLowerer, span: LambdaSolved.Type.Span(LambdaSolved.Type.Field)) Allocator.Error!Type.Content {
        const source_fields = self.input.sliceFields(span);
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, source_fields.len);
        defer self.allocator.free(labels);
        for (source_fields, 0..) |field, i| {
            labels[i] = field.name;
        }

        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        if (self.row_shapes.recordShapeFields(shape).len != source_fields.len) executableInvariant("executable type lowering record shape arity mismatch");

        const fields = try self.allocator.alloc(Type.RecordFieldType, source_fields.len);
        errdefer self.allocator.free(fields);
        for (source_fields, 0..) |field, i| {
            fields[i] = .{
                .field = self.recordFieldInShape(shape, labels[i]),
                .ty = try self.lowerType(field.ty),
            };
        }

        return .{ .record = .{
            .shape = shape,
            .fields = fields,
        } };
    }

    fn lowerTagUnionType(self: *TypeLowerer, span: LambdaSolved.Type.Span(LambdaSolved.Type.Tag)) Allocator.Error!Type.Content {
        const source_tags = self.input.sliceTags(span);
        const descriptors = try self.allocator.alloc(MonoRow.Store.TagShapeDescriptor, source_tags.len);
        defer self.allocator.free(descriptors);
        for (source_tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = tag.args.len,
            };
        }

        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        if (self.row_shapes.tagUnionTags(shape).len != source_tags.len) executableInvariant("executable type lowering tag-union shape arity mismatch");

        const tags = try self.allocator.alloc(Type.TagType, source_tags.len);
        for (tags) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags[0..source_tags.len]) |tag| {
                if (tag.payloads.len > 0) self.allocator.free(tag.payloads);
            }
            self.allocator.free(tags);
        }
        for (source_tags, 0..) |source_tag, i| {
            const shape_tag = self.tagInShape(shape, source_tag.name);
            const source_payload_tys = self.input.sliceTypeVarSpan(source_tag.args);
            const shape_payloads = self.row_shapes.tagPayloads(shape_tag);
            if (shape_payloads.len != source_payload_tys.len) executableInvariant("executable type lowering tag payload arity mismatch");

            const payloads = try self.allocator.alloc(Type.TagPayloadType, source_payload_tys.len);
            errdefer self.allocator.free(payloads);
            for (source_payload_tys, 0..) |payload_ty, payload_i| {
                payloads[payload_i] = .{
                    .payload = shape_payloads[payload_i],
                    .ty = try self.lowerType(payload_ty),
                };
            }

            tags[i] = .{
                .tag = shape_tag,
                .payloads = payloads,
            };
        }

        return .{ .tag_union = .{
            .shape = shape,
            .tags = tags,
        } };
    }

    fn recordFieldInShape(
        self: *TypeLowerer,
        shape: MonoRow.RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) MonoRow.RecordFieldId {
        for (self.row_shapes.recordShapeFields(shape)) |field_id| {
            if (self.row_shapes.recordField(field_id).label == label) return field_id;
        }
        executableInvariant("executable type lowering record field label missing from interned shape");
    }

    fn tagInShape(
        self: *TypeLowerer,
        shape: MonoRow.TagUnionShapeId,
        label: canonical.TagLabelId,
    ) MonoRow.TagId {
        for (self.row_shapes.tagUnionTags(shape)) |tag_id| {
            if (self.row_shapes.tag(tag_id).label == label) return tag_id;
        }
        executableInvariant("executable type lowering tag label missing from interned shape");
    }
};

const BodyBuilder = struct {
    allocator: Allocator,
    program: *Program,
    input: *const LambdaSolved.Ast.Store,
    output: *Ast.Store,
    canonical_names: *const canonical.CanonicalNameStore,
    type_lowerer: *TypeLowerer,
    session_type_lowerer: SessionTypeLowerer,
    value_store: *const repr.ValueInfoStore,
    representation_store: *const repr.RepresentationStore,
    published_adapter_payloads: ?*PublishedTypeLowerer = null,
    published_adapter_artifact: canonical.ArtifactRef = .{},
    callable_set_descriptors: []const repr.CanonicalCallableSetDescriptor,
    env: std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef),
    expr_map: std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId),
    executable_proc: Ast.ExecutableProcId,
    source_proc: canonical.MirProcedureRef,
    representation_instance: repr.ProcRepresentationInstanceId,
    proc_instance: *const repr.ProcRepresentationInstance,
    proc_instances: []const repr.ProcRepresentationInstance,
    solve_sessions: []const repr.RepresentationSolveSession,
    value_stores: []const repr.ValueInfoStore,
    proc_exec_map: *const std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId),
    erased_adapter_procs: []const ErasedAdapterProcReservation,
    capture_record_arg: ?Ast.TypedValue = null,

    fn deinit(self: *BodyBuilder) void {
        self.session_type_lowerer.deinit();
        self.expr_map.deinit();
        self.env.deinit();
    }

    fn lowerDef(self: *BodyBuilder, def_id: LambdaSolved.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.defs.items[@intFromEnum(def_id)];
        return try self.output.addDef(switch (def.value) {
            .fn_ => |fn_| blk: {
                const args = try self.lowerFnArgSpan(fn_.args);
                const body = try self.lowerExpr(fn_.body);
                break :blk .{
                    .proc = self.executable_proc,
                    .origin = .{ .source = def.proc },
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .fn_ = .{
                        .args = args,
                        .body = body,
                    } },
                };
            },
            .hosted_fn => |hosted| blk: {
                self.capture_record_arg = null;
                const args = try self.lowerParamSpan(hosted.args);
                break :blk .{
                    .proc = self.executable_proc,
                    .origin = .{ .source = def.proc },
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .hosted_fn = .{
                        .args = args,
                        .ret_ty = try self.type_lowerer.lowerType(hosted.ret_ty),
                        .hosted = hosted.hosted,
                    } },
                };
            },
            .val => |expr| blk: {
                self.capture_record_arg = null;
                const body = try self.lowerExpr(expr);
                break :blk .{
                    .proc = self.executable_proc,
                    .origin = .{ .source = def.proc },
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .fn_ = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    } },
                };
            },
            .run => |run_def| blk: {
                self.capture_record_arg = null;
                const body = try self.lowerExpr(run_def.body);
                break :blk .{
                    .proc = self.executable_proc,
                    .origin = .{ .source = def.proc },
                    .specialization_key = try self.executableSpecializationKey(),
                    .value = .{ .fn_ = .{
                        .args = Ast.Span(Ast.TypedValue).empty(),
                        .body = body,
                    } },
                };
            },
        });
    }

    fn executableSpecializationKey(self: *const BodyBuilder) Allocator.Error!repr.ExecutableSpecializationKey {
        if (!canonical.mirProcedureRefEql(self.proc_instance.proc, self.source_proc)) {
            executableInvariant("executable build procedure instance does not match the source procedure being lowered");
        }
        return try repr.cloneExecutableSpecializationKey(self.allocator, self.proc_instance.executable_specialization_key);
    }

    fn executableProcForSpecializationKey(
        self: *const BodyBuilder,
        key: repr.ExecutableSpecializationKey,
    ) Ast.ExecutableProcId {
        for (self.proc_instances, 0..) |instance, i| {
            if (!instance.materialized) continue;
            if (repr.executableSpecializationKeyEql(instance.executable_specialization_key, key)) {
                const instance_id: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
                return self.proc_exec_map.get(instance_id) orelse executableInvariant("executable specialization key matched an unreserved proc instance");
            }
        }
        executableInvariant("executable specialization key was not reserved before body lowering");
    }

    fn executableProcForErasedAdapter(
        self: *const BodyBuilder,
        adapter: repr.ErasedAdapterKey,
    ) Ast.ExecutableProcId {
        for (self.erased_adapter_procs) |proc| {
            if (erasedAdapterKeyEql(proc.key, adapter)) return proc.executable_proc;
        }
        executableInvariant("executable finite-set erase plan referenced an unreserved erased adapter");
    }

    fn executableProcForErasedDirectProcAdapter(
        self: *const BodyBuilder,
        code: canonical.ErasedDirectProcCodeRef,
    ) Ast.ExecutableProcId {
        for (self.program.erased_direct_proc_adapters.items) |proc| {
            if (erasedDirectProcCodeRefEql(proc.code, code)) return proc.executable_proc;
        }
        executableInvariant("executable proc-value erase plan referenced an unreserved direct erased entry adapter");
    }

    fn lowerExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!Type.TypeId {
        return try self.lowerExecutableValueTypeInStore(
            logical_ty,
            value_info_id,
            self.value_store,
            self.representation_store,
        );
    }

    fn lowerSessionExecutableEndpointType(
        self: *BodyBuilder,
        endpoint: repr.SessionExecutableValueEndpoint,
    ) Allocator.Error!Type.TypeId {
        return try self.session_type_lowerer.lower(endpoint.exec_ty.ty, endpoint.exec_ty.key);
    }

    fn lowerSessionExecutableTypeKey(
        self: *BodyBuilder,
        key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!Type.TypeId {
        return try self.lowerSessionExecutableTypeKeyInStore(key, self.representation_store);
    }

    fn lowerSessionExecutableTypeKeyInStore(
        self: *BodyBuilder,
        key: repr.CanonicalExecValueTypeKey,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        const ref = representation_store.session_executable_type_payloads.refForKey(key) orelse {
            executableInvariant("executable session type key has no published payload");
        };
        return try self.lowerSessionExecutableTypeInStore(.{ .ty = ref, .key = key }, representation_store);
    }

    fn lowerSessionExecutableTypeInStore(
        self: *BodyBuilder,
        endpoint: repr.SessionExecutableTypeEndpoint,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        if (representation_store == self.representation_store) {
            return try self.session_type_lowerer.lower(endpoint.ty, endpoint.key);
        }

        var lowerer = SessionTypeLowerer.init(
            self.allocator,
            &representation_store.session_executable_type_payloads,
            &self.program.types,
            &self.program.lowered_session_types_by_key,
        );
        defer lowerer.deinit();
        return try lowerer.lower(endpoint.ty, endpoint.key);
    }

    fn lowerExecutableValueTypeInStore(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        _ = logical_ty;
        const value_info = value_store.values.items[@intFromEnum(value_info_id)];
        const endpoint = value_info.exec_ty orelse {
            executableInvariant("executable value type has no published endpoint");
        };
        return try self.lowerSessionExecutableTypeInStore(endpoint, representation_store);
    }

    fn lowerCallableSetMemberPayloadType(
        self: *BodyBuilder,
        callable_set_key: repr.CanonicalCallableSetKey,
        member: repr.CanonicalCallableSetMember,
    ) Allocator.Error!?Type.TypeId {
        if (self.published_adapter_payloads) |published| {
            return try self.lowerPublishedCallableSetMemberPayloadType(published, callable_set_key, member);
        }
        const callable_set_payload_key = repr.finiteCallableSetExecValueTypeKey(callable_set_key);
        const callable_set_ref = self.representation_store.session_executable_type_payloads.refForKey(callable_set_payload_key) orelse {
            executableInvariant("executable callable-set member payload type has no published callable-set payload");
        };
        const payload = self.representation_store.session_executable_type_payloads.get(callable_set_ref.payload);
        const callable_set = switch (payload) {
            .callable_set => |set| set,
            else => executableInvariant("executable callable-set member payload key did not point at a callable-set payload"),
        };
        if (!repr.callableSetKeyEql(callable_set.key, callable_set_key)) {
            executableInvariant("executable callable-set payload key disagrees with descriptor key");
        }
        for (callable_set.members) |payload_member| {
            if (payload_member.member != member.member) continue;
            if (member.capture_slots.len == 0) {
                if (payload_member.payload_ty != null or payload_member.payload_ty_key != null) {
                    executableInvariant("executable callable-set member has no captures but published a capture payload");
                }
                return null;
            }
            const payload_ty = payload_member.payload_ty orelse {
                executableInvariant("executable callable-set captured member has no published capture payload");
            };
            const payload_key = payload_member.payload_ty_key orelse {
                executableInvariant("executable callable-set captured member payload has no key");
            };
            if (!repr.canonicalExecValueTypeKeyEql(payload_key, repr.captureTupleExecKeyForSlots(member.capture_slots))) {
                executableInvariant("executable callable-set member payload key differs from member capture schema");
            }
            return try self.lowerSessionExecutableTypeInStore(
                .{ .ty = payload_ty, .key = payload_key },
                self.representation_store,
            );
        }
        executableInvariant("executable callable-set member had no published payload entry");
    }

    fn lowerPublishedCallableSetMemberPayloadType(
        self: *BodyBuilder,
        published: *PublishedTypeLowerer,
        callable_set_key: repr.CanonicalCallableSetKey,
        member: repr.CanonicalCallableSetMember,
    ) Allocator.Error!?Type.TypeId {
        const callable_set_payload_key = repr.finiteCallableSetExecValueTypeKey(callable_set_key);
        const callable_set_ref = published.payloads.refForKey(self.published_adapter_artifact, callable_set_payload_key) orelse {
            executableInvariant("executable published callable-set member payload type has no published callable-set payload");
        };
        const payload = published.payloads.get(callable_set_ref.payload);
        const callable_set = switch (payload) {
            .callable_set => |set| set,
            else => executableInvariant("executable published callable-set member payload key did not point at a callable-set payload"),
        };
        if (!repr.callableSetKeyEql(callable_set.key, callable_set_key)) {
            executableInvariant("executable published callable-set payload key disagrees with descriptor key");
        }
        for (callable_set.members) |payload_member| {
            if (payload_member.member != member.member) continue;
            if (member.capture_slots.len == 0) {
                if (payload_member.payload_ty != null or payload_member.payload_ty_key != null) {
                    executableInvariant("executable published callable-set member has no captures but published a capture payload");
                }
                return null;
            }
            const payload_ty = payload_member.payload_ty orelse {
                executableInvariant("executable published callable-set captured member has no published capture payload");
            };
            const payload_key = payload_member.payload_ty_key orelse {
                executableInvariant("executable published callable-set captured member payload has no key");
            };
            if (!repr.canonicalExecValueTypeKeyEql(payload_key, repr.captureTupleExecKeyForSlots(member.capture_slots))) {
                executableInvariant("executable published callable-set member payload key differs from member capture schema");
            }
            return try published.lower(payload_ty, payload_key);
        }
        executableInvariant("executable published callable-set member had no published payload entry");
    }

    fn lowerFiniteSetAdapterCaptureType(
        self: *BodyBuilder,
        adapter: repr.ErasedAdapterKey,
        descriptor: *const repr.CanonicalCallableSetDescriptor,
    ) Allocator.Error!?Type.TypeId {
        if (descriptor.members.len == 1 and descriptor.members[0].capture_slots.len == 0) {
            return null;
        }
        return try self.lowerCallableSetType(adapter.callable_set_key);
    }

    fn lowerCallableSetType(
        self: *BodyBuilder,
        key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!Type.TypeId {
        const payload_key = repr.finiteCallableSetExecValueTypeKey(key);
        if (self.published_adapter_payloads) |published| {
            const ref = published.payloads.refForKey(self.published_adapter_artifact, payload_key) orelse {
                executableInvariant("executable callable-set published payload key has no payload");
            };
            return try published.lower(ref, payload_key);
        }
        return try self.lowerSessionExecutableTypeKey(payload_key);
    }

    fn lowerFnArgSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedValue) {
        const capture_arg = try self.lowerCaptureRecordArg();
        self.capture_record_arg = capture_arg;

        const input_items = self.input.typed_symbols.items[span.start..][0..span.len];
        const capture_arg_len: usize = if (capture_arg != null) 1 else 0;
        const total_len = input_items.len + capture_arg_len;
        if (total_len == 0) return Ast.Span(Ast.TypedValue).empty();

        const output_items = try self.allocator.alloc(Ast.TypedValue, total_len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |param, i| {
            const value = self.output.freshValueRef();
            try self.env.put(param.binding_info, value);
            const binding = self.value_store.bindings.items[@intFromEnum(param.binding_info)];
            output_items[i] = .{
                .ty = try self.lowerExecutableValueType(param.ty, binding.value),
                .value = value,
            };
        }
        if (capture_arg) |arg| {
            output_items[input_items.len] = arg;
        }
        return try self.output.addTypedValueSpan(output_items);
    }

    fn lowerParamSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TypedSymbol)) Allocator.Error!Ast.Span(Ast.TypedValue) {
        if (span.len == 0) return Ast.Span(Ast.TypedValue).empty();
        const input_items = self.input.typed_symbols.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.TypedValue, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |param, i| {
            const value = self.output.freshValueRef();
            try self.env.put(param.binding_info, value);
            const binding = self.value_store.bindings.items[@intFromEnum(param.binding_info)];
            output_items[i] = .{
                .ty = try self.lowerExecutableValueType(param.ty, binding.value),
                .value = value,
            };
        }
        return try self.output.addTypedValueSpan(output_items);
    }

    fn lowerCaptureRecordArg(self: *BodyBuilder) Allocator.Error!?Ast.TypedValue {
        const captures = self.value_store.sliceValueSpan(self.proc_instance.public_roots.captures);
        if (captures.len == 0) return null;

        const capture_tys = try self.allocator.alloc(Type.TypeId, captures.len);
        errdefer self.allocator.free(capture_tys);
        for (captures, 0..) |capture, i| {
            const info = self.value_store.values.items[@intFromEnum(capture)];
            capture_tys[i] = try self.lowerExecutableValueType(info.logical_ty, capture);
        }

        const capture_record_ty = try self.type_lowerer.output.addType(.{ .tuple = capture_tys });
        return .{
            .ty = capture_record_ty,
            .value = self.output.freshValueRef(),
        };
    }

    fn expressionCanUseExprMap(expr: LambdaSolved.Ast.Expr) bool {
        return switch (expr.data) {
            .capture_ref,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .const_instance,
            .crash,
            .runtime_error,
            => true,
            else => false,
        };
    }

    fn lowerExpr(self: *BodyBuilder, expr_id: LambdaSolved.Ast.ExprId) Allocator.Error!Ast.ExprId {
        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        const can_cache = expressionCanUseExprMap(expr);
        if (can_cache) {
            if (self.expr_map.get(expr_id)) |existing| return existing;
        }

        const lowered = switch (expr.data) {
            .var_ => |var_| blk: {
                const value = self.env.get(var_.binding_info) orelse executableInvariant("executable variable occurrence has no lowered binding value");
                const binding = self.value_store.bindings.items[@intFromEnum(var_.binding_info)];
                const occurrence_info = self.value_store.values.items[@intFromEnum(expr.value_info)];
                if (!occurrence_info.value_alias_needs_executable_transform) {
                    executableInvariant("executable variable occurrence was not published as a materialized alias");
                }
                const alias_source = occurrence_info.value_alias_source orelse executableInvariant("executable variable occurrence has no published alias source");
                if (alias_source != binding.value) {
                    executableInvariant("executable variable occurrence alias source differs from binding value");
                }
                const alias_transform = occurrence_info.value_alias_transform orelse executableInvariant("executable variable occurrence has no published alias transform");
                const boundary = self.representation_store.valueTransformBoundary(alias_transform);
                const expected_ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                var stmt_ids = std.ArrayList(Ast.StmtId).empty;
                defer stmt_ids.deinit(self.allocator);
                const transformed = try self.applyValueTransformBoundary(&stmt_ids, boundary, value);
                const final_expr = try self.output.addValueRefExpr(expected_ty, transformed);
                if (stmt_ids.items.len == 0) break :blk final_expr;
                break :blk try self.output.addExpr(expected_ty, self.output.getExpr(final_expr).value, .{ .block = .{
                    .stmts = try self.output.addStmtSpan(stmt_ids.items),
                    .final_expr = final_expr,
                } });
            },
            .int_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .int_lit = literal }),
            .frac_f32_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .frac_f32_lit = literal }),
            .frac_f64_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .frac_f64_lit = literal }),
            .dec_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .dec_lit = literal }),
            .str_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .str_lit = literal }),
            .bool_lit => |literal| try self.lowerBoolLiteralExpr(expr.ty, expr.value_info, literal),
            .unit => try self.addValueExpr(expr.ty, expr.value_info, .unit),
            .const_instance => |const_instance| blk: {
                const resolved = resolveConstInstanceForExecutable(self.program, const_instance);
                if (@import("builtin").mode == .Debug and
                    !std.mem.eql(u8, &const_instance.key.requested_source_ty.bytes, &expr.source_ty.bytes))
                {
                    executableInvariant("executable const_instance expression source type disagrees with requested source type");
                }
                break :blk try lowerComptimeValueExpr(
                    self.allocator,
                    self.program,
                    resolved.materialization,
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    resolved.instance.schema,
                    resolved.instance.value,
                    true,
                );
            },
            .const_ref => executableInvariant("executable lowering reached non-runnable compile-time dependency const_ref"),
            .pending_local_root => executableInvariant("executable lowering reached summary-only pending local root"),
            .record => |record| blk: {
                const ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const fields = try self.lowerRecordFieldsForType(expr.value_info, record.eval_order, record.assembly_order, ty);
                break :blk try self.output.addExpr(
                    ty,
                    self.output.freshValueRef(),
                    .{ .record = .{
                        .shape = record.shape,
                        .fields = fields,
                    } },
                );
            },
            .nominal_reinterpret => |backing| blk: {
                const ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const lowered_backing = try self.lowerNominalBackingAtType(expr.value_info, backing, ty);
                break :blk try self.output.addExpr(
                    ty,
                    self.output.freshValueRef(),
                    .{ .nominal_reinterpret = lowered_backing },
                );
            },
            .tag => |tag| blk: {
                const ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const resolved_tag = self.tagTypeForType(ty, tag.tag);
                const tag_id = resolved_tag.tag_type.tag;
                const payloads = try self.lowerTagPayloadValuesForTagType(expr.value_info, resolved_tag.tag_type, tag.eval_order, tag.assembly_order);
                break :blk try self.output.addExpr(
                    ty,
                    self.output.freshValueRef(),
                    .{ .tag = .{
                        .union_shape = resolved_tag.union_shape,
                        .tag = tag_id,
                        .payloads = payloads,
                    } },
                );
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                const projected_ty = self.recordFieldTypeForProjection(self.output.getExpr(record).ty, access.field);
                break :blk try self.output.addExpr(
                    projected_ty,
                    self.output.freshValueRef(),
                    .{ .access = .{
                        .record = record,
                        .field = access.field,
                    } },
                );
            },
            .let_ => |let_| blk: {
                const body = try self.lowerExpr(let_.body);
                const bind_value = try self.output.freshTypedValueRef(self.output.getExpr(body).ty);
                const previous = try self.env.fetchPut(let_.bind.binding_info, bind_value);
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.binding_info, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.binding_info);
                    }
                }
                const rest = try self.lowerExpr(let_.rest);
                const stmt = try self.output.addStmt(.{ .decl = .{
                    .value = bind_value,
                    .body = body,
                } });
                const stmt_span = try self.output.addStmtSpan(&.{stmt});
                break :blk try self.output.addExpr(
                    self.output.getExpr(rest).ty,
                    self.exprValue(rest),
                    .{ .block = .{
                        .stmts = stmt_span,
                        .final_expr = rest,
                    } },
                );
            },
            .block => |block| blk: {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = try self.lowerExpr(block.final_expr);
                break :blk try self.output.addExpr(
                    self.output.getExpr(final_expr).ty,
                    self.exprValue(final_expr),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = final_expr,
                    } },
                );
            },
            .tuple => |items| blk: {
                const ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const items_span = try self.lowerTupleItemsForType(expr.value_info, items, ty);
                break :blk try self.output.addExpr(
                    ty,
                    self.output.freshValueRef(),
                    .{ .tuple = items_span },
                );
            },
            .list => |items| blk: {
                const ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const items_span = try self.lowerListItemsForType(expr.value_info, items, ty);
                break :blk try self.output.addExpr(
                    ty,
                    self.output.freshValueRef(),
                    .{ .list = items_span },
                );
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const tag_union_ty = self.output.getExpr(tag_union).ty;
                break :blk try self.output.addExpr(
                    self.tagPayloadTypeForPattern(tag_union_ty, payload.payload),
                    self.output.freshValueRef(),
                    .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                    } },
                );
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                const projected_ty = self.tupleElemTypeForProjection(self.output.getExpr(tuple).ty, access.elem_index);
                break :blk try self.output.addExpr(
                    projected_ty,
                    self.output.freshValueRef(),
                    .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                    } },
                );
            },
            .structural_eq => |eq| blk: {
                const lhs = try self.lowerExpr(eq.lhs);
                const rhs = try self.lowerExpr(eq.rhs);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .structural_eq = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    } },
                );
            },
            .low_level => |low_level| blk: {
                if (builtin.mode == .Debug) self.verifyLowLevelValueFlow(low_level.value_flow);
                const args = try self.lowerExprIds(low_level.args);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .low_level = .{
                        .op = low_level.op,
                        .rc_effect = low_level.rc_effect,
                        .args = args,
                    } },
                );
            },
            .return_ => |return_| blk: {
                const body = try self.lowerReturnValue(return_.expr, return_.return_info);
                break :blk try self.output.addExpr(
                    self.output.getExpr(body).ty,
                    self.exprValue(body),
                    .{ .return_ = body },
                );
            },
            .bool_not => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                const result_ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const false_expr = try self.addBoolTagExpr(result_ty, false);
                const true_expr = try self.addBoolTagExpr(result_ty, true);
                break :blk try self.output.addExpr(result_ty, self.output.freshValueRef(), .{ .if_ = .{
                    .cond = lowered_child,
                    .true_discriminant = self.boolTrueDiscriminant(self.output.getExpr(lowered_child).ty),
                    .then_body = false_expr,
                    .else_body = true_expr,
                } });
            },
            .crash => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .crash = literal }),
            .runtime_error => try self.addValueExpr(expr.ty, expr.value_info, .runtime_error),
            .match_ => |match_| blk: {
                const cond = try self.lowerExpr(match_.cond);
                const scrutinee_exprs = [_]Ast.ExprId{cond};
                const scrutinees = [_]Ast.ExecutableValueRef{self.exprValue(cond)};
                const result_ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const lowered_branches = try self.lowerBranchSpanProducer(match_.branches, self.output.getExpr(cond).ty, result_ty);
                const transformed_branches = try self.wrapSourceMatchBranches(match_.join_info, lowered_branches, result_ty);
                const scrutinee_expr_span = try self.output.addExprSpan(&scrutinee_exprs);
                const scrutinee_span = try self.output.addValueRefSpan(&scrutinees);
                const decision_plan = try self.buildSourceMatchDecisionPlan(scrutinee_span, self.output.getExpr(cond).ty, transformed_branches);
                break :blk try self.output.addExpr(
                    result_ty,
                    self.output.freshValueRef(),
                    .{ .source_match = .{
                        .scrutinee_exprs = scrutinee_expr_span,
                        .scrutinees = scrutinee_span,
                        .decision_plan = decision_plan,
                        .branches = transformed_branches,
                    } },
                );
            },
            .if_ => |if_| blk: {
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerExpr(if_.then_body);
                const else_body = try self.lowerExpr(if_.else_body);
                const result_ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
                const transformed = try self.wrapIfBranches(if_.join_info, then_body, else_body, result_ty);
                const true_discriminant = self.boolTrueDiscriminant(self.output.getExpr(cond).ty);
                break :blk try self.output.addExpr(
                    result_ty,
                    self.output.freshValueRef(),
                    .{ .if_ = .{
                        .cond = cond,
                        .true_discriminant = true_discriminant,
                        .then_body = transformed.then_body,
                        .else_body = transformed.else_body,
                    } },
                );
            },
            .for_ => |for_| try self.lowerForExpr(expr.ty, expr.value_info, for_),
            .capture_ref => |slot| try self.lowerCaptureRef(expr.ty, slot),
            .call_value => |call| try self.lowerCallValue(expr.ty, call),
            .call_proc => |call| try self.lowerCallProc(expr.ty, call),
            .proc_value => |proc_value| try self.lowerProcValue(expr.ty, expr.value_info, proc_value),
        };
        if (can_cache) {
            try self.expr_map.put(expr_id, lowered);
        }
        return lowered;
    }

    fn lowerExprAtType(
        self: *BodyBuilder,
        expr_id: LambdaSolved.Ast.ExprId,
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .record => |record| blk: {
                const fields = try self.lowerRecordFieldsForType(expr.value_info, record.eval_order, record.assembly_order, expected_ty);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .record = .{
                        .shape = record.shape,
                        .fields = fields,
                    } },
                );
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerNominalBackingAtType(expr.value_info, backing, expected_ty);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .nominal_reinterpret = lowered_backing },
                );
            },
            .tag => |tag| blk: {
                const resolved_tag = self.tagTypeForType(expected_ty, tag.tag);
                const payloads = try self.lowerTagPayloadValuesForTagType(expr.value_info, resolved_tag.tag_type, tag.eval_order, tag.assembly_order);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .tag = .{
                        .union_shape = resolved_tag.union_shape,
                        .tag = resolved_tag.tag_type.tag,
                        .payloads = payloads,
                    } },
                );
            },
            .tuple => |items| blk: {
                const items_span = try self.lowerTupleItemsForType(expr.value_info, items, expected_ty);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .tuple = items_span },
                );
            },
            .list => |items| blk: {
                const items_span = try self.lowerListItemsForType(expr.value_info, items, expected_ty);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .list = items_span },
                );
            },
            .let_ => |let_| blk: {
                const body = try self.lowerExpr(let_.body);
                const bind_value = try self.output.freshTypedValueRef(self.output.getExpr(body).ty);
                const previous = try self.env.fetchPut(let_.bind.binding_info, bind_value);
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.binding_info, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.binding_info);
                    }
                }
                const rest = if (self.lambdaExprCanCompleteNormally(let_.body))
                    try self.lowerExprAtType(let_.rest, expected_ty)
                else
                    try self.lowerExpr(let_.rest);
                const stmt = try self.output.addStmt(.{ .decl = .{
                    .value = bind_value,
                    .body = body,
                } });
                const stmt_span = try self.output.addStmtSpan(&.{stmt});
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.exprValue(rest),
                    .{ .block = .{
                        .stmts = stmt_span,
                        .final_expr = rest,
                    } },
                );
            },
            .block => |block| blk: {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = if (self.lambdaStmtSpanCanCompleteNormally(block.stmts))
                    try self.lowerExprAtType(block.final_expr, expected_ty)
                else
                    try self.lowerExpr(block.final_expr);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.exprValue(final_expr),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = final_expr,
                    } },
                );
            },
            .match_ => |match_| blk: {
                const cond = try self.lowerExpr(match_.cond);
                const scrutinee_exprs = [_]Ast.ExprId{cond};
                const scrutinees = [_]Ast.ExecutableValueRef{self.exprValue(cond)};
                const lowered_branches = try self.lowerBranchSpanAtType(match_.branches, match_.join_info, self.output.getExpr(cond).ty, expected_ty);
                const scrutinee_expr_span = try self.output.addExprSpan(&scrutinee_exprs);
                const scrutinee_span = try self.output.addValueRefSpan(&scrutinees);
                const decision_plan = try self.buildSourceMatchDecisionPlan(scrutinee_span, self.output.getExpr(cond).ty, lowered_branches);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .source_match = .{
                        .scrutinee_exprs = scrutinee_expr_span,
                        .scrutinees = scrutinee_span,
                        .decision_plan = decision_plan,
                        .branches = lowered_branches,
                    } },
                );
            },
            .if_ => |if_| blk: {
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerIfBranchAtType(if_.then_body, if_.join_info, .then_, expected_ty);
                const else_body = try self.lowerIfBranchAtType(if_.else_body, if_.join_info, .else_, expected_ty);
                const true_discriminant = self.boolTrueDiscriminant(self.output.getExpr(cond).ty);
                break :blk try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .{ .if_ = .{
                        .cond = cond,
                        .true_discriminant = true_discriminant,
                        .then_body = then_body,
                        .else_body = else_body,
                    } },
                );
            },
            else => self.lowerExpr(expr_id),
        };
    }

    fn lowerExprAtConsumerUse(
        self: *BodyBuilder,
        expr_id: LambdaSolved.Ast.ExprId,
        use_id: repr.ConsumerUsePlanId,
    ) Allocator.Error!Ast.ExprId {
        const plan = self.representation_store.consumerUsePlan(use_id);
        const source_expr = self.input.exprs.items[@intFromEnum(expr_id)];
        if (source_expr.value_info != plan.child_value) {
            executableInvariant("executable consumer-use plan child value differs from expression value");
        }
        const expected_ty = try self.lowerSessionExecutableEndpointType(plan.expected_endpoint);
        return switch (plan.lowering) {
            .construct_directly,
            .lower_control_flow_contextually,
            => try self.lowerExprAtType(expr_id, expected_ty),
            .existing_value => |boundary_id| blk: {
                const lowered = try self.lowerExpr(expr_id);
                const boundary = self.representation_store.valueTransformBoundary(boundary_id);
                if (boundary.from_value != plan.child_value) {
                    executableInvariant("executable consumer-use existing transform source value differs from plan child");
                }
                if (!sessionExecutableValueEndpointEql(boundary.to_endpoint, plan.expected_endpoint)) {
                    executableInvariant("executable consumer-use existing transform target endpoint differs from plan endpoint");
                }
                var stmt_ids = std.ArrayList(Ast.StmtId).empty;
                defer stmt_ids.deinit(self.allocator);
                const lowered_value = try self.materializeExprValue(&stmt_ids, lowered);
                const transformed = try self.applyValueTransformBoundary(&stmt_ids, boundary, lowered_value);
                const final_expr = try self.output.addValueRefExpr(expected_ty, transformed);
                break :blk try self.output.addExpr(expected_ty, transformed, .{ .block = .{
                    .stmts = try self.output.addStmtSpan(stmt_ids.items),
                    .final_expr = final_expr,
                } });
            },
        };
    }

    const PendingPatternTest = struct {
        path_value: Ast.PatternPathValuePlanId,
        pattern_test: Ast.PatternTest,
    };

    const SourceMatchDecisionRow = struct {
        branch: Ast.BranchId,
        degenerate: bool,
        guard: ?Ast.ExprId,
        body: Ast.ExprId,
        tests: []const PendingPatternTest,
        bindings: []const Ast.PatternBinding,
    };

    const SourceMatchDecisionRowRef = struct {
        row: *const SourceMatchDecisionRow,
        remaining: []const PendingPatternTest,
        owns_remaining: bool = false,
    };

    const PatternTestAssumptionRelation = enum {
        impossible,
        possible,
        proves,
    };

    const TransformedIfBranches = struct {
        then_body: Ast.ExprId,
        else_body: Ast.ExprId,
    };

    fn lowerReturnValue(
        self: *BodyBuilder,
        expr: LambdaSolved.Ast.ExprId,
        return_info: repr.ReturnInfoId,
    ) Allocator.Error!Ast.ExprId {
        const ret = self.returnInfo(return_info);
        const use_id = ret.consumer_use orelse executableInvariant("executable return reached unfinalized return consumer-use plan");
        const plan = self.representation_store.consumerUsePlan(use_id);
        const owner_return = switch (plan.owner) {
            .return_value => |owner| owner,
            else => executableInvariant("executable return consumer-use plan had non-return owner"),
        };
        if (owner_return != return_info) {
            executableInvariant("executable return consumer-use plan owner points at a different return");
        }
        if (plan.child_value != ret.value) {
            executableInvariant("executable return consumer-use child value differs from return info");
        }

        const body = try self.lowerExprAtConsumerUse(expr, use_id);
        const return_ty = try self.lowerSessionExecutableEndpointType(plan.expected_endpoint);
        if (self.output.getExpr(body).ty != return_ty) {
            executableInvariant("executable return expression type differs from return endpoint type");
        }
        return body;
    }

    fn wrapIfBranches(
        self: *BodyBuilder,
        join_id: repr.JoinInfoId,
        then_body: Ast.ExprId,
        else_body: Ast.ExprId,
        result_ty: Type.TypeId,
    ) Allocator.Error!TransformedIfBranches {
        const join = self.value_store.joins.items[@intFromEnum(join_id)];
        const inputs = self.value_store.sliceJoinInputSpan(join.inputs);
        const transforms = self.value_store.sliceValueTransformBoundarySpan(join.input_transforms);
        if (inputs.len != transforms.len or inputs.len > 2) {
            executableInvariant("executable if join transform count differs from published join inputs");
        }

        var result: TransformedIfBranches = .{
            .then_body = then_body,
            .else_body = else_body,
        };
        var saw_then = false;
        var saw_else = false;
        for (inputs, transforms) |input, transform_id| {
            const boundary = self.representation_store.valueTransformBoundary(transform_id);
            const source = switch (input.source) {
                .if_branch => |if_branch| if_branch,
                else => executableInvariant("executable if join input has non-if source"),
            };
            self.verifyJoinInputBoundary(boundary, input);
            switch (source.branch) {
                .then_ => {
                    if (saw_then) executableInvariant("executable if join saw duplicate then branch");
                    if (!self.executableExprReturnsValue(then_body)) {
                        executableInvariant("executable if join input referenced non-returning then branch");
                    }
                    result.then_body = try self.wrapJoinInputBody(then_body, boundary, result_ty);
                    saw_then = true;
                },
                .else_ => {
                    if (saw_else) executableInvariant("executable if join saw duplicate else branch");
                    if (!self.executableExprReturnsValue(else_body)) {
                        executableInvariant("executable if join input referenced non-returning else branch");
                    }
                    result.else_body = try self.wrapJoinInputBody(else_body, boundary, result_ty);
                    saw_else = true;
                },
            }
        }
        return result;
    }

    fn wrapSourceMatchBranches(
        self: *BodyBuilder,
        join_id: repr.JoinInfoId,
        branches: Ast.Span(Ast.BranchId),
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.BranchId) {
        const join = self.value_store.joins.items[@intFromEnum(join_id)];
        const inputs = self.value_store.sliceJoinInputSpan(join.inputs);
        const transforms = self.value_store.sliceValueTransformBoundarySpan(join.input_transforms);
        if (inputs.len != transforms.len) {
            executableInvariant("executable source_match join transform count differs from published join inputs");
        }

        const branch_ids = self.output.branch_ids.items[branches.start..][0..branches.len];
        var seen = try self.allocator.alloc(bool, branch_ids.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (inputs, transforms) |input, transform_id| {
            const boundary = self.representation_store.valueTransformBoundary(transform_id);
            const source = switch (input.source) {
                .source_match_branch => |source_match| source_match,
                else => executableInvariant("executable source_match join input has non-match source"),
            };
            self.verifyJoinInputBoundary(boundary, input);
            const branch_index: usize = @intCast(@intFromEnum(source.branch));
            if (branch_index >= branch_ids.len) {
                executableInvariant("executable source_match join input referenced branch outside branch span");
            }
            if (seen[branch_index]) {
                executableInvariant("executable source_match join saw duplicate branch input");
            }
            seen[branch_index] = true;

            const branch_id = branch_ids[branch_index];
            const branch = &self.output.branches.items[@intFromEnum(branch_id)];
            if (!self.executableExprReturnsValue(branch.body)) {
                executableInvariant("executable source_match join input referenced non-returning branch");
            }
            branch.body = try self.wrapJoinInputBody(branch.body, boundary, result_ty);
        }

        return branches;
    }

    fn wrapJoinInputBody(
        self: *BodyBuilder,
        body: Ast.ExprId,
        boundary: repr.ValueTransformBoundary,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (!self.executableExprReturnsValue(body)) {
            executableInvariant("executable join transform attempted to wrap a non-returning body");
        }

        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        const body_value = try self.materializeExprValue(&stmt_ids, body);
        const result_value = try self.applyValueTransformBoundary(&stmt_ids, boundary, body_value);
        const final_expr = try self.output.addValueRefExpr(result_ty, result_value);
        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
    }

    fn executableExprReturnsValue(self: *const BodyBuilder, expr_id: Ast.ExprId) bool {
        return switch (self.output.getExpr(expr_id).data) {
            .return_,
            .crash,
            .runtime_error,
            .@"unreachable",
            => false,
            .block => |block| self.executableBlockReturnsValue(block.stmts, block.final_expr),
            .if_ => |if_| self.executableExprReturnsValue(if_.then_body) or
                self.executableExprReturnsValue(if_.else_body),
            .source_match => |source_match| self.anyExecutableBranchReturnsValue(source_match.branches),
            else => true,
        };
    }

    fn boolTrueDiscriminant(self: *const BodyBuilder, ty: Type.TypeId) u16 {
        return switch (self.program.types.getType(ty)) {
            .link => |next| self.boolTrueDiscriminant(next),
            .nominal => |nominal| self.boolTrueDiscriminant(nominal.backing),
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    if (tag.payloads.len != 0) continue;
                    const label = self.program.row_shapes.tag(tag.tag).label;
                    if (std.mem.eql(u8, self.program.canonical_names.tagLabelText(label), "True")) {
                        break :blk @intCast(self.program.row_shapes.tag(tag.tag).logical_index);
                    }
                }
                executableInvariant("executable Bool condition had no zero-payload True tag");
            },
            else => executableInvariant("executable Bool condition expected ordinary Bool tag-union type"),
        };
    }

    fn lowerBoolLiteralExpr(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info: repr.ValueInfoId,
        literal: bool,
    ) Allocator.Error!Ast.ExprId {
        const ty = try self.lowerExecutableValueType(source_ty, value_info);
        return try self.addBoolTagExpr(ty, literal);
    }

    fn addBoolTagExpr(
        self: *BodyBuilder,
        ty: Type.TypeId,
        literal: bool,
    ) Allocator.Error!Ast.ExprId {
        const tag = self.boolTagForType(ty, literal);
        return try self.output.addExpr(ty, self.output.freshValueRef(), .{ .tag = .{
            .union_shape = tag.union_shape,
            .tag = tag.tag,
            .payloads = Ast.Span(Ast.TagPayloadExpr).empty(),
        } });
    }

    fn boolTagForType(
        self: *const BodyBuilder,
        ty: Type.TypeId,
        literal: bool,
    ) TypeTag {
        return switch (self.program.types.getType(ty)) {
            .link => |next| self.boolTagForType(next, literal),
            .nominal => |nominal| self.boolTagForType(nominal.backing, literal),
            .tag_union => |tag_union| blk: {
                const expected = if (literal) "True" else "False";
                for (tag_union.tags) |tag| {
                    if (tag.payloads.len != 0) continue;
                    const label = self.program.row_shapes.tag(tag.tag).label;
                    if (std.mem.eql(u8, self.program.canonical_names.tagLabelText(label), expected)) {
                        break :blk .{
                            .union_shape = tag_union.shape,
                            .tag = tag.tag,
                        };
                    }
                }
                executableInvariant("executable Bool literal had no matching zero-payload tag");
            },
            else => executableInvariant("executable Bool literal expected ordinary Bool tag-union type"),
        };
    }

    fn executableBlockReturnsValue(
        self: *const BodyBuilder,
        stmts: Ast.Span(Ast.StmtId),
        final_expr: Ast.ExprId,
    ) bool {
        const stmt_ids = self.output.stmt_ids.items[stmts.start..][0..stmts.len];
        for (stmt_ids) |stmt_id| {
            if (!self.executableStmtCanCompleteNormally(stmt_id)) return false;
        }
        return self.executableExprReturnsValue(final_expr);
    }

    fn executableStmtCanCompleteNormally(self: *const BodyBuilder, stmt_id: Ast.StmtId) bool {
        return switch (self.output.stmts.items[@intFromEnum(stmt_id)]) {
            .decl => |decl| self.executableExprReturnsValue(decl.body),
            .reassign => |reassign| self.executableExprReturnsValue(reassign.body),
            .expr => |expr| self.executableExprReturnsValue(expr),
            .debug => |expr| self.executableExprReturnsValue(expr),
            .expect => |expr| self.executableExprReturnsValue(expr),
            .crash,
            .return_,
            .break_,
            => false,
            .for_,
            .while_,
            => true,
        };
    }

    fn anyExecutableBranchReturnsValue(
        self: *const BodyBuilder,
        branches: Ast.Span(Ast.BranchId),
    ) bool {
        const branch_ids = self.output.branch_ids.items[branches.start..][0..branches.len];
        for (branch_ids) |branch_id| {
            if (self.executableExprReturnsValue(self.output.branches.items[@intFromEnum(branch_id)].body)) return true;
        }
        return false;
    }

    fn lambdaExprCanCompleteNormally(self: *const BodyBuilder, expr_id: LambdaSolved.Ast.ExprId) bool {
        return switch (self.input.exprs.items[@intFromEnum(expr_id)].data) {
            .return_,
            .crash,
            .runtime_error,
            => false,
            .block => |block| self.lambdaBlockCanCompleteNormally(block.stmts, block.final_expr),
            .if_ => |if_| self.lambdaExprCanCompleteNormally(if_.then_body) or
                self.lambdaExprCanCompleteNormally(if_.else_body),
            .match_ => |match_| self.lambdaAnyBranchCanCompleteNormally(match_.branches),
            else => true,
        };
    }

    fn lambdaBlockCanCompleteNormally(
        self: *const BodyBuilder,
        stmts: LambdaSolved.Ast.Span(LambdaSolved.Ast.StmtId),
        final_expr: LambdaSolved.Ast.ExprId,
    ) bool {
        if (!self.lambdaStmtSpanCanCompleteNormally(stmts)) return false;
        return self.lambdaExprCanCompleteNormally(final_expr);
    }

    fn lambdaStmtSpanCanCompleteNormally(
        self: *const BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.StmtId),
    ) bool {
        const stmt_ids = self.input.stmt_ids.items[span.start..][0..span.len];
        for (stmt_ids) |stmt_id| {
            if (!self.lambdaStmtCanCompleteNormally(stmt_id)) return false;
        }
        return true;
    }

    fn lambdaStmtCanCompleteNormally(self: *const BodyBuilder, stmt_id: LambdaSolved.Ast.StmtId) bool {
        return switch (self.input.stmts.items[@intFromEnum(stmt_id)]) {
            .decl => |decl| self.lambdaExprCanCompleteNormally(decl.body),
            .var_decl => |decl| self.lambdaExprCanCompleteNormally(decl.body),
            .reassign => |reassign| self.lambdaExprCanCompleteNormally(reassign.body),
            .expr => |expr| self.lambdaExprCanCompleteNormally(expr),
            .debug => |expr| self.lambdaExprCanCompleteNormally(expr),
            .expect => |expr| self.lambdaExprCanCompleteNormally(expr),
            .crash,
            .return_,
            .break_,
            => false,
            .for_,
            .while_,
            => true,
        };
    }

    fn lambdaAnyBranchCanCompleteNormally(
        self: *const BodyBuilder,
        branches: LambdaSolved.Ast.Span(LambdaSolved.Ast.BranchId),
    ) bool {
        const branch_ids = self.input.branch_ids.items[branches.start..][0..branches.len];
        for (branch_ids) |branch_id| {
            if (self.lambdaExprCanCompleteNormally(self.input.branches.items[@intFromEnum(branch_id)].body)) return true;
        }
        return false;
    }

    fn returnInfo(
        self: *BodyBuilder,
        return_info_id: repr.ReturnInfoId,
    ) repr.ReturnInfo {
        const index = @intFromEnum(return_info_id);
        if (index >= self.value_store.returns.items.len) {
            executableInvariant("executable return referenced missing return info");
        }
        return self.value_store.returns.items[index];
    }

    fn verifyJoinInputBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        input: repr.JoinInputInfo,
    ) void {
        if (boundary.from_value != input.value) {
            executableInvariant("executable join input boundary source value differs from join input");
        }
        const local = switch (boundary.from_endpoint.owner) {
            .local_value => |value| value,
            else => executableInvariant("executable join input boundary source endpoint is not local_value"),
        };
        if (local != input.value) {
            executableInvariant("executable join input boundary source endpoint differs from join input");
        }
        switch (input.source) {
            .if_branch => |expected| {
                const actual = switch (boundary.kind) {
                    .if_branch_result => |if_branch| if_branch,
                    else => executableInvariant("executable if join input boundary has non-if kind"),
                };
                if (actual.if_expr != expected.if_expr or actual.branch != expected.branch) {
                    executableInvariant("executable if join input boundary source identity differs from join input");
                }
            },
            .source_match_branch => |expected| {
                const actual = switch (boundary.kind) {
                    .source_match_branch_result => |match_branch| match_branch,
                    else => executableInvariant("executable source_match join input boundary has non-match kind"),
                };
                if (actual.match != expected.match or
                    actual.branch != expected.branch or
                    actual.alternative != expected.alternative)
                {
                    executableInvariant("executable source_match join input boundary source identity differs from join input");
                }
            },
            .loop_phi => |expected| {
                const actual = switch (boundary.kind) {
                    .loop_phi => |loop_phi| loop_phi,
                    else => executableInvariant("executable loop join input boundary has non-loop kind"),
                };
                if (actual != expected) {
                    executableInvariant("executable loop join input boundary source identity differs from join input");
                }
            },
        }
        _ = self;
    }

    fn buildSourceMatchDecisionPlan(
        self: *BodyBuilder,
        scrutinees: Ast.Span(Ast.ExecutableValueRef),
        scrutinee_ty: Type.TypeId,
        branches: Ast.Span(Ast.BranchId),
    ) Allocator.Error!Ast.PatternDecisionPlanId {
        const branch_ids = self.output.branch_ids.items[branches.start..][0..branches.len];
        if (branch_ids.len == 0) executableInvariant("executable source_match decision plan requires at least one branch");

        var path_plans = std.ArrayList(Ast.PatternPathValuePlanId).empty;
        defer path_plans.deinit(self.allocator);
        const root_path = try self.addPatternPathValuePlan(&path_plans, .{
            .path = .{
                .scrutinee = 0,
                .steps = Ast.Span(Ast.PatternPathStep).empty(),
            },
            .source = .{ .scrutinee = 0 },
            .ty = scrutinee_ty,
        });

        var rows = std.ArrayList(SourceMatchDecisionRow).empty;
        defer {
            for (rows.items) |row| {
                self.allocator.free(row.tests);
                self.allocator.free(row.bindings);
            }
            rows.deinit(self.allocator);
        }
        for (branch_ids) |branch_id| {
            const branch = self.output.branches.items[@intFromEnum(branch_id)];

            var tests = std.ArrayList(PendingPatternTest).empty;
            defer tests.deinit(self.allocator);
            var bindings = std.ArrayList(Ast.PatternBinding).empty;
            defer bindings.deinit(self.allocator);
            try self.collectPatternDecisionData(branch.pat, root_path, &tests, &bindings, &path_plans);

            var owned_tests = try self.allocator.dupe(PendingPatternTest, tests.items);
            errdefer if (owned_tests.len > 0) self.allocator.free(owned_tests);
            var owned_bindings = try self.allocator.dupe(Ast.PatternBinding, bindings.items);
            errdefer if (owned_bindings.len > 0) self.allocator.free(owned_bindings);

            try rows.append(self.allocator, .{
                .branch = branch_id,
                .degenerate = branch.degenerate,
                .guard = branch.guard,
                .body = branch.body,
                .tests = owned_tests,
                .bindings = owned_bindings,
            });
            owned_tests = &.{};
            owned_bindings = &.{};
        }

        const initial_rows = try self.allocator.alloc(SourceMatchDecisionRowRef, rows.items.len);
        defer self.allocator.free(initial_rows);
        for (rows.items, 0..) |*row, i| {
            initial_rows[i] = .{
                .row = row,
                .remaining = row.tests,
            };
        }

        var leaves = std.ArrayList(Ast.DecisionLeafId).empty;
        defer leaves.deinit(self.allocator);
        const root = (try self.buildSourceMatchDecisionRows(initial_rows, &leaves)) orelse
            executableInvariant("executable source_match decision plan had no reachable root");

        return try self.output.addPatternDecisionPlan(.{
            .scrutinees = scrutinees,
            .path_value_plans = try self.output.addPatternPathValuePlanSpan(path_plans.items),
            .root = root,
            .leaves = try self.output.addDecisionLeafSpan(leaves.items),
            .branches = branches,
        });
    }

    fn buildSourceMatchDecisionRows(
        self: *BodyBuilder,
        rows: []const SourceMatchDecisionRowRef,
        leaves: *std.ArrayList(Ast.DecisionLeafId),
    ) Allocator.Error!?Ast.DecisionNodeId {
        if (rows.len == 0) return null;

        const first = rows[0];
        if (first.remaining.len == 0) {
            const fallback = if (first.row.guard != null)
                try self.buildSourceMatchDecisionRows(rows[1..], leaves)
            else
                null;

            const leaf_id = try self.output.addDecisionLeaf(.{
                .branch = first.row.branch,
                .degenerate = first.row.degenerate,
                .guard = first.row.guard,
                .body = first.row.body,
                .fallback = fallback,
                .bindings = try self.output.addPatternBindingSpan(first.row.bindings),
            });
            try leaves.append(self.allocator, leaf_id);
            return try self.output.addDecisionNode(.{ .leaf = leaf_id });
        }

        const selected = first.remaining[0];
        var tests_at_path = std.ArrayList(Ast.PatternTest).empty;
        defer tests_at_path.deinit(self.allocator);
        try self.collectDecisionTestsAtPath(rows, selected.path_value, &tests_at_path);
        if (tests_at_path.items.len == 0) executableInvariant("executable source_match decision row selected a path with no tests");

        var edges = std.ArrayList(Ast.DecisionEdge).empty;
        defer edges.deinit(self.allocator);
        for (tests_at_path.items) |pattern_test| {
            const edge_rows = try self.decisionRowsForAssumedTest(rows, selected.path_value, pattern_test);
            defer self.deinitSourceMatchDecisionRowRefs(edge_rows);
            const next = (try self.buildSourceMatchDecisionRows(edge_rows, leaves)) orelse
                executableInvariant("executable source_match decision edge had no reachable rows");
            try edges.append(self.allocator, .{
                .pattern_test = pattern_test,
                .next = next,
            });
        }

        const default_rows = try self.decisionRowsWithoutPathTest(rows, selected.path_value);
        defer self.deinitSourceMatchDecisionRowRefs(default_rows);
        const default = try self.buildSourceMatchDecisionRows(default_rows, leaves);

        return try self.output.addDecisionNode(.{ .decision_test = .{
            .path_value = selected.path_value,
            .edges = try self.output.addDecisionEdgeSpan(edges.items),
            .default = default,
        } });
    }

    fn collectDecisionTestsAtPath(
        self: *BodyBuilder,
        rows: []const SourceMatchDecisionRowRef,
        path_value: Ast.PatternPathValuePlanId,
        tests: *std.ArrayList(Ast.PatternTest),
    ) Allocator.Error!void {
        for (rows) |row| {
            if (self.firstTestIndexAtPath(row.remaining, path_value)) |index| {
                const pattern_test = row.remaining[index].pattern_test;
                if (!self.hasDecisionTest(tests.items, pattern_test)) {
                    try tests.append(self.allocator, pattern_test);
                }
            }
        }
    }

    fn decisionRowsForAssumedTest(
        self: *BodyBuilder,
        rows: []const SourceMatchDecisionRowRef,
        path_value: Ast.PatternPathValuePlanId,
        assumed: Ast.PatternTest,
    ) Allocator.Error![]SourceMatchDecisionRowRef {
        var out = std.ArrayList(SourceMatchDecisionRowRef).empty;
        errdefer {
            for (out.items) |row| {
                if (row.owns_remaining and row.remaining.len > 0) {
                    self.allocator.free(row.remaining);
                }
            }
            out.deinit(self.allocator);
        }

        for (rows) |row| {
            if (self.firstTestIndexAtPath(row.remaining, path_value)) |index| {
                const relation = self.patternTestRelationWhenAssumed(assumed, row.remaining[index].pattern_test);
                switch (relation) {
                    .impossible => continue,
                    .proves => {
                        var remaining = try self.remainingWithoutTest(row.remaining, index);
                        errdefer if (remaining.len > 0) self.allocator.free(remaining);
                        try out.append(self.allocator, .{
                            .row = row.row,
                            .remaining = remaining,
                            .owns_remaining = true,
                        });
                        remaining = &.{};
                    },
                    .possible => try out.append(self.allocator, .{
                        .row = row.row,
                        .remaining = row.remaining,
                    }),
                }
            } else {
                try out.append(self.allocator, .{
                    .row = row.row,
                    .remaining = row.remaining,
                });
            }
        }

        return try out.toOwnedSlice(self.allocator);
    }

    fn decisionRowsWithoutPathTest(
        self: *BodyBuilder,
        rows: []const SourceMatchDecisionRowRef,
        path_value: Ast.PatternPathValuePlanId,
    ) Allocator.Error![]SourceMatchDecisionRowRef {
        var out = std.ArrayList(SourceMatchDecisionRowRef).empty;
        errdefer out.deinit(self.allocator);
        for (rows) |row| {
            if (self.firstTestIndexAtPath(row.remaining, path_value) == null) {
                try out.append(self.allocator, .{
                    .row = row.row,
                    .remaining = row.remaining,
                });
            }
        }
        return try out.toOwnedSlice(self.allocator);
    }

    fn remainingWithoutTest(
        self: *BodyBuilder,
        tests: []const PendingPatternTest,
        removed: usize,
    ) Allocator.Error![]const PendingPatternTest {
        if (removed >= tests.len) executableInvariant("executable source_match removed test index out of range");
        if (tests.len == 1) return &.{};
        const out = try self.allocator.alloc(PendingPatternTest, tests.len - 1);
        var write: usize = 0;
        for (tests, 0..) |pending_test, i| {
            if (i == removed) continue;
            out[write] = pending_test;
            write += 1;
        }
        return out;
    }

    fn deinitSourceMatchDecisionRowRefs(
        self: *BodyBuilder,
        rows: []const SourceMatchDecisionRowRef,
    ) void {
        for (rows) |row| {
            if (row.owns_remaining and row.remaining.len > 0) {
                self.allocator.free(row.remaining);
            }
        }
        if (rows.len > 0) self.allocator.free(rows);
    }

    fn firstTestIndexAtPath(
        self: *BodyBuilder,
        tests: []const PendingPatternTest,
        path_value: Ast.PatternPathValuePlanId,
    ) ?usize {
        _ = self;
        for (tests, 0..) |pending_test, i| {
            if (pending_test.path_value == path_value) return i;
        }
        return null;
    }

    fn hasDecisionTest(
        self: *BodyBuilder,
        tests: []const Ast.PatternTest,
        needle: Ast.PatternTest,
    ) bool {
        for (tests) |candidate| {
            if (self.patternTestEql(candidate, needle)) return true;
        }
        return false;
    }

    fn patternTestRelationWhenAssumed(
        self: *BodyBuilder,
        assumed: Ast.PatternTest,
        required: Ast.PatternTest,
    ) PatternTestAssumptionRelation {
        if (self.patternTestEql(assumed, required)) return .proves;

        return switch (assumed) {
            .list_len_exact => |assumed_len| switch (required) {
                .list_len_exact => |required_len| if (assumed_len == required_len) .proves else .impossible,
                .list_len_at_least => |required_len| if (assumed_len >= required_len) .proves else .impossible,
                else => .impossible,
            },
            .list_len_at_least => |assumed_len| switch (required) {
                .list_len_exact => |required_len| if (required_len >= assumed_len) .possible else .impossible,
                .list_len_at_least => |required_len| if (assumed_len >= required_len) .proves else .possible,
                else => .impossible,
            },
            else => .impossible,
        };
    }

    fn patternTestEql(
        self: *BodyBuilder,
        a: Ast.PatternTest,
        b: Ast.PatternTest,
    ) bool {
        _ = self;
        return switch (a) {
            .tag => |left| switch (b) {
                .tag => |right| left.union_shape == right.union_shape and left.tag == right.tag,
                else => false,
            },
            .int_literal => |left| switch (b) {
                .int_literal => |right| left == right,
                else => false,
            },
            .float_f32_literal => |left| switch (b) {
                .float_f32_literal => |right| @as(u32, @bitCast(left)) == @as(u32, @bitCast(right)),
                else => false,
            },
            .float_f64_literal => |left| switch (b) {
                .float_f64_literal => |right| @as(u64, @bitCast(left)) == @as(u64, @bitCast(right)),
                else => false,
            },
            .decimal_literal => |left| switch (b) {
                .decimal_literal => |right| left == right,
                else => false,
            },
            .str_literal => |left| switch (b) {
                .str_literal => |right| left == right,
                else => false,
            },
            .list_len_exact => |left| switch (b) {
                .list_len_exact => |right| left == right,
                else => false,
            },
            .list_len_at_least => |left| switch (b) {
                .list_len_at_least => |right| left == right,
                else => false,
            },
            .guard => |left| switch (b) {
                .guard => |right| left == right,
                else => false,
            },
        };
    }

    fn patternPathValuePlanEql(
        self: *BodyBuilder,
        a: Ast.PatternPathValuePlan,
        b: Ast.PatternPathValuePlan,
    ) bool {
        if (a.ty != b.ty) return false;
        if (a.path.scrutinee != b.path.scrutinee) return false;
        if (!self.patternPathValueSourceEql(a.source, b.source)) return false;

        const a_steps = self.output.slicePatternPathStepSpan(a.path.steps);
        const b_steps = self.output.slicePatternPathStepSpan(b.path.steps);
        if (a_steps.len != b_steps.len) return false;
        for (a_steps, b_steps) |left, right| {
            if (!self.patternPathStepEql(left, right)) return false;
        }
        return true;
    }

    fn patternPathStepEql(
        self: *BodyBuilder,
        a: Ast.PatternPathStep,
        b: Ast.PatternPathStep,
    ) bool {
        _ = self;
        return switch (a) {
            .tag_payload_record => |left| switch (b) {
                .tag_payload_record => |right| left == right,
                else => false,
            },
            .tag_payload => |left| switch (b) {
                .tag_payload => |right| left == right,
                else => false,
            },
            .record_field => |left| switch (b) {
                .record_field => |right| left == right,
                else => false,
            },
            .record_rest => |left| switch (b) {
                .record_rest => |right| left == right,
                else => false,
            },
            .tuple_field => |left| switch (b) {
                .tuple_field => |right| left == right,
                else => false,
            },
            .list_index => |left| switch (b) {
                .list_index => |right| left.index == right.index and left.from_end == right.from_end,
                else => false,
            },
            .list_rest => |left| switch (b) {
                .list_rest => |right| left.start == right.start and left.from_end_count == right.from_end_count,
                else => false,
            },
            .nominal_payload => switch (b) {
                .nominal_payload => true,
                else => false,
            },
        };
    }

    fn patternPathValueSourceEql(
        self: *BodyBuilder,
        a: Ast.PatternPathValueSource,
        b: Ast.PatternPathValueSource,
    ) bool {
        _ = self;
        return switch (a) {
            .scrutinee => |left| switch (b) {
                .scrutinee => |right| left == right,
                else => false,
            },
            .tag_payload_record => |left| switch (b) {
                .tag_payload_record => |right| left.parent == right.parent and left.tag == right.tag,
                else => false,
            },
            .tag_payload_field => |left| switch (b) {
                .tag_payload_field => |right| left.parent_payload_record == right.parent_payload_record and left.payload == right.payload,
                else => false,
            },
            .record_field => |left| switch (b) {
                .record_field => |right| left.parent == right.parent and left.field == right.field,
                else => false,
            },
            .record_rest => |left| switch (b) {
                .record_rest => |right| left == right,
                else => false,
            },
            .tuple_field => |left| switch (b) {
                .tuple_field => |right| left.parent == right.parent and left.field == right.field,
                else => false,
            },
            .list_element => |left| switch (b) {
                .list_element => |right| left.parent == right.parent and
                    left.probe.index == right.probe.index and
                    left.probe.from_end == right.probe.from_end,
                else => false,
            },
            .list_rest => |left| switch (b) {
                .list_rest => |right| left.parent == right.parent and
                    left.probe.start == right.probe.start and
                    left.probe.from_end_count == right.probe.from_end_count,
                else => false,
            },
            .nominal_payload => |left| switch (b) {
                .nominal_payload => |right| left == right,
                else => false,
            },
        };
    }

    fn addPatternPathValuePlan(
        self: *BodyBuilder,
        path_plans: *std.ArrayList(Ast.PatternPathValuePlanId),
        plan: Ast.PatternPathValuePlan,
    ) Allocator.Error!Ast.PatternPathValuePlanId {
        for (path_plans.items) |existing_id| {
            if (self.patternPathValuePlanEql(self.output.getPatternPathValuePlan(existing_id), plan)) {
                return existing_id;
            }
        }
        const id = try self.output.addPatternPathValuePlan(plan);
        try path_plans.append(self.allocator, id);
        return id;
    }

    fn addChildPatternPathValuePlan(
        self: *BodyBuilder,
        path_plans: *std.ArrayList(Ast.PatternPathValuePlanId),
        parent_id: Ast.PatternPathValuePlanId,
        step: Ast.PatternPathStep,
        source: Ast.PatternPathValueSource,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.PatternPathValuePlanId {
        const parent = self.output.getPatternPathValuePlan(parent_id);
        const parent_steps = self.output.slicePatternPathStepSpan(parent.path.steps);

        for (path_plans.items) |existing_id| {
            const existing = self.output.getPatternPathValuePlan(existing_id);
            if (existing.ty != ty) continue;
            if (existing.path.scrutinee != parent.path.scrutinee) continue;
            if (!self.patternPathValueSourceEql(existing.source, source)) continue;

            const existing_steps = self.output.slicePatternPathStepSpan(existing.path.steps);
            if (existing_steps.len != parent_steps.len + 1) continue;

            var matches_parent = true;
            for (parent_steps, 0..) |parent_step, i| {
                if (!self.patternPathStepEql(existing_steps[i], parent_step)) {
                    matches_parent = false;
                    break;
                }
            }
            if (matches_parent and self.patternPathStepEql(existing_steps[parent_steps.len], step)) {
                return existing_id;
            }
        }

        const steps = try self.allocator.alloc(Ast.PatternPathStep, parent_steps.len + 1);
        defer self.allocator.free(steps);
        if (parent_steps.len > 0) @memcpy(steps[0..parent_steps.len], parent_steps);
        steps[parent_steps.len] = step;
        return try self.addPatternPathValuePlan(path_plans, .{
            .path = .{
                .scrutinee = parent.path.scrutinee,
                .steps = try self.output.addPatternPathStepSpan(steps),
            },
            .source = source,
            .ty = ty,
        });
    }

    fn collectPatternDecisionData(
        self: *BodyBuilder,
        pat_id: Ast.PatId,
        path_value: Ast.PatternPathValuePlanId,
        tests: *std.ArrayList(PendingPatternTest),
        bindings: *std.ArrayList(Ast.PatternBinding),
        path_plans: *std.ArrayList(Ast.PatternPathValuePlanId),
    ) Allocator.Error!void {
        const pat = self.output.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .wildcard => {},
            .bind => |bind| {
                const source_plan = self.output.getPatternPathValuePlan(path_value);
                try bindings.append(self.allocator, .{
                    .binder = bind,
                    .source = path_value,
                    .bridge = try self.patternBindingBridge(source_plan.ty, pat.ty),
                    .ty = pat.ty,
                });
            },
            .as => |as| {
                const source_plan = self.output.getPatternPathValuePlan(path_value);
                try bindings.append(self.allocator, .{
                    .binder = as.bind,
                    .source = path_value,
                    .bridge = try self.patternBindingBridge(source_plan.ty, pat.ty),
                    .ty = pat.ty,
                });
                try self.collectPatternDecisionData(as.pattern, path_value, tests, bindings, path_plans);
            },
            .nominal => |child| try self.collectPatternDecisionData(child, path_value, tests, bindings, path_plans),
            .int_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .pattern_test = .{ .int_literal = literal } }),
            .frac_f32_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .pattern_test = .{ .float_f32_literal = literal } }),
            .frac_f64_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .pattern_test = .{ .float_f64_literal = literal } }),
            .dec_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .pattern_test = .{ .decimal_literal = literal } }),
            .str_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .pattern_test = .{ .str_literal = literal } }),
            .tuple => |items| {
                const child_ids = self.output.pat_ids.items[items.start..][0..items.len];
                for (child_ids, 0..) |child_id, i| {
                    const child = self.output.pats.items[@intFromEnum(child_id)];
                    const child_path = try self.addChildPatternPathValuePlan(
                        path_plans,
                        path_value,
                        .{ .tuple_field = @intCast(i) },
                        .{ .tuple_field = .{ .parent = path_value, .field = @intCast(i) } },
                        child.ty,
                    );
                    try self.collectPatternDecisionData(child_id, child_path, tests, bindings, path_plans);
                }
            },
            .record => |record| {
                const field_patterns = self.output.record_field_patterns.items[record.fields.start..][0..record.fields.len];
                for (field_patterns) |field_pattern| {
                    const child = self.output.pats.items[@intFromEnum(field_pattern.pattern)];
                    const child_path = try self.addChildPatternPathValuePlan(
                        path_plans,
                        path_value,
                        .{ .record_field = field_pattern.field },
                        .{ .record_field = .{ .parent = path_value, .field = field_pattern.field } },
                        child.ty,
                    );
                    try self.collectPatternDecisionData(field_pattern.pattern, child_path, tests, bindings, path_plans);
                }
                if (record.rest) |rest_pat| {
                    const rest = self.output.pats.items[@intFromEnum(rest_pat)];
                    const projection = try self.recordRestProjection(path_value, record.shape, rest.ty);
                    const rest_path = try self.addChildPatternPathValuePlan(
                        path_plans,
                        path_value,
                        .{ .record_rest = projection },
                        .{ .record_rest = projection },
                        rest.ty,
                    );
                    try self.collectPatternDecisionData(rest_pat, rest_path, tests, bindings, path_plans);
                }
            },
            .tag => |tag| {
                try tests.append(self.allocator, .{ .path_value = path_value, .pattern_test = .{ .tag = .{
                    .union_shape = tag.union_shape,
                    .tag = tag.tag,
                } } });
                const payload_patterns = self.output.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                if (payload_patterns.len == 0) return;

                const parent_path = self.output.getPatternPathValuePlan(path_value);
                const payload_record_ty = try self.payloadRecordTypeForPattern(parent_path.ty, tag.tag);
                const payload_record_path = try self.addChildPatternPathValuePlan(
                    path_plans,
                    path_value,
                    .{ .tag_payload_record = tag.tag },
                    .{ .tag_payload_record = .{ .parent = path_value, .tag = tag.tag } },
                    payload_record_ty,
                );
                for (payload_patterns) |payload_pattern| {
                    const child_ty = self.tagPayloadTypeForPattern(parent_path.ty, payload_pattern.payload);
                    const child_path = try self.addChildPatternPathValuePlan(
                        path_plans,
                        payload_record_path,
                        .{ .tag_payload = payload_pattern.payload },
                        .{ .tag_payload_field = .{ .parent_payload_record = payload_record_path, .payload = payload_pattern.payload } },
                        child_ty,
                    );
                    try self.collectPatternDecisionData(payload_pattern.pattern, child_path, tests, bindings, path_plans);
                }
            },
            .list => |list| {
                const item_ids = self.output.pat_ids.items[list.items.start..][0..list.items.len];
                try tests.append(self.allocator, .{
                    .path_value = path_value,
                    .pattern_test = if (list.rest == null)
                        .{ .list_len_exact = @intCast(item_ids.len) }
                    else
                        .{ .list_len_at_least = @intCast(item_ids.len) },
                });

                for (item_ids, 0..) |child_id, i| {
                    const child = self.output.pats.items[@intFromEnum(child_id)];
                    const probe = listElementProbe(i, item_ids.len, list.rest);
                    const child_path = try self.addChildPatternPathValuePlan(
                        path_plans,
                        path_value,
                        .{ .list_index = probe },
                        .{ .list_element = .{ .parent = path_value, .probe = probe } },
                        child.ty,
                    );
                    try self.collectPatternDecisionData(child_id, child_path, tests, bindings, path_plans);
                }

                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pat| {
                        const rest_child = self.output.pats.items[@intFromEnum(rest_pat)];
                        const probe = Ast.ListRestProbe{
                            .start = rest.index,
                            .from_end_count = @intCast(item_ids.len - rest.index),
                        };
                        const rest_path = try self.addChildPatternPathValuePlan(
                            path_plans,
                            path_value,
                            .{ .list_rest = probe },
                            .{ .list_rest = .{ .parent = path_value, .probe = probe } },
                            rest_child.ty,
                        );
                        try self.collectPatternDecisionData(rest_pat, rest_path, tests, bindings, path_plans);
                    }
                }
            },
        }
    }

    fn listElementProbe(index: usize, item_count: usize, rest: ?Ast.ListRestPattern) Ast.ListElementProbe {
        if (rest) |rest_info| {
            if (index >= rest_info.index) {
                return .{
                    .index = @intCast(item_count - index),
                    .from_end = true,
                };
            }
        }
        return .{ .index = @intCast(index), .from_end = false };
    }

    fn recordRestProjection(
        self: *BodyBuilder,
        parent: Ast.PatternPathValuePlanId,
        source_shape: MonoRow.RecordShapeId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.RecordRestProjectionId {
        const result_record = self.recordTypeForPattern(result_ty);
        const result_fields = self.program.row_shapes.recordShapeFields(result_record.shape);
        const projected = try self.allocator.alloc(Ast.RecordRestProjectedField, result_fields.len);
        defer self.allocator.free(projected);
        for (result_fields, 0..) |result_field, i| {
            const result_field_info = self.program.row_shapes.recordField(result_field);
            projected[i] = .{
                .source_field = self.matchingSourceRecordField(source_shape, result_field),
                .result_field = result_field,
                .ty = self.recordFieldType(result_record, result_field),
                .result_logical_index = result_field_info.logical_index,
            };
        }
        return try self.output.addRecordRestProjection(.{
            .parent = parent,
            .source_shape = source_shape,
            .result_shape = result_record.shape,
            .projected_fields = try self.output.addRecordRestProjectedFieldSpan(projected),
        });
    }

    fn matchingSourceRecordField(
        self: *BodyBuilder,
        source_shape: MonoRow.RecordShapeId,
        result_field: MonoRow.RecordFieldId,
    ) MonoRow.RecordFieldId {
        const result_label = self.program.row_shapes.recordField(result_field).label;
        for (self.program.row_shapes.recordShapeFields(source_shape)) |source_field| {
            if (self.program.row_shapes.recordField(source_field).label == result_label) return source_field;
        }
        executableInvariant("executable record-rest projection referenced a field absent from source record");
    }

    fn recordTypeForPattern(self: *BodyBuilder, ty: Type.TypeId) Type.RecordType {
        return switch (self.program.types.getType(ty)) {
            .record => |record| record,
            .nominal => |nominal| self.recordTypeForPattern(nominal.backing),
            else => executableInvariant("executable record-rest pattern binding did not have a record type"),
        };
    }

    fn recordFieldType(self: *BodyBuilder, record: Type.RecordType, field_id: MonoRow.RecordFieldId) Type.TypeId {
        _ = self;
        for (record.fields) |field| {
            if (field.field == field_id) return field.ty;
        }
        executableInvariant("executable record-rest projection field was absent from result record type");
    }

    fn recordFieldTypeForProjection(
        self: *BodyBuilder,
        record_ty: Type.TypeId,
        field_id: MonoRow.RecordFieldId,
    ) Type.TypeId {
        return switch (self.program.types.getType(record_ty)) {
            .record => |record| self.recordFieldType(record, field_id),
            .nominal => |nominal| self.recordFieldTypeForProjection(nominal.backing, field_id),
            else => executableInvariant("executable record projection source did not have a record type"),
        };
    }

    fn tupleElemTypeForProjection(
        self: *BodyBuilder,
        tuple_ty: Type.TypeId,
        elem_index: u32,
    ) Type.TypeId {
        return switch (self.program.types.getType(tuple_ty)) {
            .tuple => |items| blk: {
                const index: usize = @intCast(elem_index);
                if (index >= items.len) executableInvariant("executable tuple projection index exceeded tuple arity");
                break :blk items[index];
            },
            .nominal => |nominal| self.tupleElemTypeForProjection(nominal.backing, elem_index),
            else => executableInvariant("executable tuple projection source did not have a tuple type"),
        };
    }

    fn payloadRecordTypeForPattern(
        self: *BodyBuilder,
        tag_union_ty: Type.TypeId,
        tag_id: MonoRow.TagId,
    ) Allocator.Error!Type.TypeId {
        const payloads = self.program.row_shapes.tagPayloads(tag_id);
        if (payloads.len == 0) executableInvariant("executable tag payload record requested for zero-payload tag");
        if (payloads.len == 1) {
            return self.tagPayloadTypeForPattern(tag_union_ty, payloads[0]);
        }

        const fields = try self.allocator.alloc(Type.TypeId, payloads.len);
        errdefer self.allocator.free(fields);
        for (payloads, 0..) |payload, i| {
            fields[i] = self.tagPayloadTypeForPattern(tag_union_ty, payload);
        }
        return try self.type_lowerer.output.addType(.{ .tuple = fields });
    }

    fn tagPayloadTypeForPattern(
        self: *BodyBuilder,
        tag_union_ty: Type.TypeId,
        payload_id: MonoRow.TagPayloadId,
    ) Type.TypeId {
        const payload_info = self.program.row_shapes.tagPayload(payload_id);
        const tag_union = switch (self.program.types.getType(tag_union_ty)) {
            .tag_union => |tag_union| tag_union,
            .nominal => |nominal| return self.tagPayloadTypeForPattern(nominal.backing, payload_id),
            else => executableInvariant("executable tag pattern payload did not have a tag-union type"),
        };
        for (tag_union.tags) |tag| {
            if (tag.tag != payload_info.tag) continue;
            for (tag.payloads) |payload| {
                if (payload.payload == payload_id) return payload.ty;
            }
        }
        executableInvariant("executable tag pattern payload was absent from tag-union type");
    }

    fn patternBindingBridge(
        self: *BodyBuilder,
        source_ty: Type.TypeId,
        target_ty: Type.TypeId,
    ) Allocator.Error!Ast.BridgeId {
        return try self.valueBridge(source_ty, target_ty, .pattern_binding);
    }

    fn constructionSlotBridge(
        self: *BodyBuilder,
        source_ty: Type.TypeId,
        target_ty: Type.TypeId,
    ) Allocator.Error!Ast.BridgeId {
        return try self.valueBridge(source_ty, target_ty, .construction_slot);
    }

    const ValueBridgeMode = enum {
        pattern_binding,
        construction_slot,
    };

    fn valueBridge(
        self: *BodyBuilder,
        source_ty: Type.TypeId,
        target_ty: Type.TypeId,
        mode: ValueBridgeMode,
    ) Allocator.Error!Ast.BridgeId {
        const source = self.program.types.getType(source_ty);
        const target = self.program.types.getType(target_ty);
        const plan: Ast.BridgePlan = switch (source) {
            .placeholder => executableInvariant("executable value bridge saw placeholder source type"),
            .link => executableInvariant("executable value bridge saw unresolved source link"),
            .primitive => |source_prim| switch (target) {
                .primitive => |target_prim| blk: {
                    if (source_prim != target_prim) executableInvariant("executable value bridge crossed primitive types");
                    break :blk .direct;
                },
                else => executableInvariant("executable value bridge crossed primitive/non-primitive types"),
            },
            .nominal => |source_nominal| switch (target) {
                .nominal => |target_nominal| blk: {
                    if (source_nominal.nominal.module_name == target_nominal.nominal.module_name and
                        source_nominal.nominal.type_name == target_nominal.nominal.type_name)
                    {
                        break :blk .nominal_reinterpret;
                    }
                    executableInvariant("executable value bridge crossed distinct nominal types");
                },
                else => .nominal_reinterpret,
            },
            .list => switch (target) {
                .list => .list_reinterpret,
                .nominal => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed list/non-list types"),
            },
            .box => switch (target) {
                .box, .nominal => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed box/non-box types"),
            },
            .tuple => |source_items| switch (target) {
                .tuple => |target_items| .{ .struct_ = try self.valueStructBridge(source_items, target_items, mode) },
                .nominal => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed tuple/non-tuple types"),
            },
            .record => |source_record| switch (target) {
                .record => |target_record| .{ .struct_ = try self.valueRecordBridge(source_record, target_record, mode) },
                .nominal => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed record/non-record types"),
            },
            .tag_union => |source_union| switch (target) {
                .tag_union => |target_union| .{ .tag_union = try self.valueTagUnionBridge(source_union, target_union, mode) },
                .nominal => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed tag-union/non-tag-union types"),
            },
            .callable_set => |source_callable| switch (target) {
                .callable_set => |target_callable| blk: {
                    if (!repr.callableSetKeyEql(source_callable.key, target_callable.key)) {
                        executableInvariant("executable value bridge crossed callable-set keys");
                    }
                    break :blk .nominal_reinterpret;
                },
                .nominal => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed callable-set/non-callable-set types"),
            },
            .erased_fn => switch (target) {
                .erased_fn => .nominal_reinterpret,
                else => executableInvariant("executable value bridge crossed erased-fn/non-erased-fn types"),
            },
            .vacant_callable_slot => switch (target) {
                .vacant_callable_slot => .zst,
                else => executableInvariant("executable value bridge crossed vacant/non-vacant callable-slot types"),
            },
        };
        return try self.output.addBridgePlan(plan);
    }

    fn valueStructBridge(
        self: *BodyBuilder,
        source_items: []const Type.TypeId,
        target_items: []const Type.TypeId,
        mode: ValueBridgeMode,
    ) Allocator.Error!Ast.Span(Ast.BridgeId) {
        if (source_items.len != target_items.len) {
            executableInvariant("executable value struct bridge arity mismatch");
        }
        if (source_items.len == 0) return Ast.Span(Ast.BridgeId).empty();
        const children = try self.allocator.alloc(Ast.BridgeId, source_items.len);
        defer self.allocator.free(children);
        for (source_items, target_items, 0..) |source, target, i| {
            children[i] = try self.valueBridge(source, target, mode);
        }
        return try self.output.addBridgePlanSpan(children);
    }

    fn valueRecordBridge(
        self: *BodyBuilder,
        source: Type.RecordType,
        target: Type.RecordType,
        mode: ValueBridgeMode,
    ) Allocator.Error!Ast.Span(Ast.BridgeId) {
        if (source.fields.len != target.fields.len) {
            executableInvariant("executable value record bridge arity mismatch");
        }
        if (source.fields.len == 0) return Ast.Span(Ast.BridgeId).empty();
        const children = try self.allocator.alloc(Ast.BridgeId, source.fields.len);
        defer self.allocator.free(children);
        for (target.fields, 0..) |target_field, i| {
            const source_field = self.recordFieldTypeByLabel(source, target_field.field);
            children[i] = try self.valueBridge(source_field.ty, target_field.ty, mode);
        }
        return try self.output.addBridgePlanSpan(children);
    }

    fn recordFieldTypeByLabel(
        self: *BodyBuilder,
        record: Type.RecordType,
        target_field_id: MonoRow.RecordFieldId,
    ) Type.RecordFieldType {
        const target_label = self.program.row_shapes.recordField(target_field_id).label;
        for (record.fields) |field| {
            const source_label = self.program.row_shapes.recordField(field.field).label;
            if (source_label == target_label) return field;
        }
        executableInvariant("executable value record bridge could not find field by finalized label");
    }

    fn valueTagUnionBridge(
        self: *BodyBuilder,
        source: Type.TagUnionType,
        target: Type.TagUnionType,
        mode: ValueBridgeMode,
    ) Allocator.Error!Ast.Span(Ast.BridgeId) {
        if (source.tags.len != target.tags.len) {
            executableInvariant("executable value tag-union bridge arity mismatch");
        }
        if (source.tags.len == 0) return Ast.Span(Ast.BridgeId).empty();
        const children = try self.allocator.alloc(Ast.BridgeId, target.tags.len);
        defer self.allocator.free(children);
        for (target.tags, 0..) |target_tag, i| {
            const source_tag = self.tagTypeByLabel(source, target_tag.tag);
            children[i] = try self.valueTagPayloadBridge(source_tag, target_tag, mode);
        }
        return try self.output.addBridgePlanSpan(children);
    }

    fn tagTypeByLabel(
        self: *BodyBuilder,
        tag_union: Type.TagUnionType,
        target_tag_id: MonoRow.TagId,
    ) Type.TagType {
        const target_label = self.program.row_shapes.tag(target_tag_id).label;
        for (tag_union.tags) |tag| {
            const source_label = self.program.row_shapes.tag(tag.tag).label;
            if (source_label == target_label) return tag;
        }
        executableInvariant("executable value tag-union bridge could not find tag by finalized label");
    }

    fn valueTagPayloadBridge(
        self: *BodyBuilder,
        source: Type.TagType,
        target: Type.TagType,
        mode: ValueBridgeMode,
    ) Allocator.Error!Ast.BridgeId {
        if (source.payloads.len != target.payloads.len) {
            executableInvariant("executable value tag payload bridge arity mismatch");
        }
        if (source.payloads.len == 0) return try self.output.addBridgePlan(.zst);
        if (source.payloads.len == 1) {
            return try self.valueBridge(source.payloads[0].ty, target.payloads[0].ty, mode);
        }

        const source_payloads = try self.allocator.alloc(Type.TypeId, source.payloads.len);
        defer self.allocator.free(source_payloads);
        const target_payloads = try self.allocator.alloc(Type.TypeId, target.payloads.len);
        defer self.allocator.free(target_payloads);
        var seen_source = try self.allocator.alloc(bool, source.payloads.len);
        defer self.allocator.free(seen_source);
        var seen_target = try self.allocator.alloc(bool, target.payloads.len);
        defer self.allocator.free(seen_target);
        @memset(seen_source, false);
        @memset(seen_target, false);
        for (source.payloads) |payload| {
            const index: usize = @intCast(self.program.row_shapes.tagPayload(payload.payload).logical_index);
            if (index >= source_payloads.len or seen_source[index]) {
                executableInvariant("executable value source payload bridge index mismatch");
            }
            source_payloads[index] = payload.ty;
            seen_source[index] = true;
        }
        for (target.payloads) |payload| {
            const index: usize = @intCast(self.program.row_shapes.tagPayload(payload.payload).logical_index);
            if (index >= target_payloads.len or seen_target[index]) {
                executableInvariant("executable value target payload bridge index mismatch");
            }
            target_payloads[index] = payload.ty;
            seen_target[index] = true;
        }
        verifyAllSeen(seen_source, "executable value source payload bridge omitted payload");
        verifyAllSeen(seen_target, "executable value target payload bridge omitted payload");
        return try self.output.addBridgePlan(.{ .struct_ = try self.valueStructBridge(source_payloads, target_payloads, mode) });
    }

    const SavedBinding = struct {
        binding: repr.BindingInfoId,
        previous: ?Ast.ExecutableValueRef,
    };

    fn lowerPatScoped(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        const ty = try self.lowerExecutableValueType(pat.ty, pat.value_info);
        return try self.lowerPatScopedWithType(pat_id, ty, saved);
    }

    fn lowerUnreachableDecisionPat(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        return try self.output.addPat(.{ .ty = ty, .data = switch (pat.data) {
            .bool_lit => |literal| blk: {
                const resolved_tag = self.boolTagForType(ty, literal);
                break :blk .{ .tag = .{
                    .union_shape = resolved_tag.union_shape,
                    .tag = resolved_tag.tag,
                    .payloads = Ast.Span(Ast.TagPayloadPattern).empty(),
                } };
            },
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .tag => |tag| blk: {
                const resolved_tag = self.tagForType(ty, tag.tag);
                break :blk .{ .tag = .{
                    .union_shape = resolved_tag.union_shape,
                    .tag = resolved_tag.tag,
                    .payloads = try self.lowerUnreachableDecisionTagPayloadPatterns(ty, resolved_tag.tag, tag.payloads),
                } };
            },
            .nominal => |child| blk: {
                const backing_ty = switch (self.program.types.getType(ty)) {
                    .nominal => |nominal| nominal.backing,
                    else => ty,
                };
                break :blk .{ .nominal = try self.lowerUnreachableDecisionPat(child, backing_ty) };
            },
            .as => |as| return try self.lowerUnreachableDecisionPat(as.pattern, ty),
            .wildcard,
            .var_,
            .tuple,
            .record,
            .list,
            => .wildcard,
        } });
    }

    fn lowerUnreachableDecisionTagPayloadPatterns(
        self: *BodyBuilder,
        parent_ty: Type.TypeId,
        target_tag: MonoRow.TagId,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadPattern),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const input_items = self.input.tag_payload_patterns.items[span.start..][0..span.len];
        const payloads = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(payloads);
        for (input_items, 0..) |payload, i| {
            const target_payload = self.payloadForTag(target_tag, payload.payload);
            const child_ty = self.tagPayloadTypeForPattern(parent_ty, target_payload);
            payloads[i] = .{
                .payload = target_payload,
                .pattern = try self.lowerUnreachableDecisionPat(payload.pattern, child_ty),
            };
        }
        return try self.output.addTagPayloadPatternSpan(payloads);
    }

    fn lowerPatScopedWithType(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        ty: Type.TypeId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        return try self.output.addPat(.{ .ty = ty, .data = switch (pat.data) {
            .bool_lit => |literal| blk: {
                const resolved_tag = self.boolTagForType(ty, literal);
                break :blk .{ .tag = .{
                    .union_shape = resolved_tag.union_shape,
                    .tag = resolved_tag.tag,
                    .payloads = Ast.Span(Ast.TagPayloadPattern).empty(),
                } };
            },
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .wildcard => .wildcard,
            .nominal => |child| .{ .nominal = try self.lowerPatScoped(child, saved) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpanScoped(items, saved) },
            .record => |record| .{ .record = .{
                .shape = record.shape,
                .fields = try self.lowerRecordFieldPatternSpanScoped(record.fields, saved),
                .rest = if (record.rest) |rest| try self.lowerPatScoped(rest, saved) else null,
            } },
            .list => |list| .{ .list = .{
                .items = try self.lowerPatSpanScoped(list.items, saved),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |pattern| try self.lowerPatScoped(pattern, saved) else null,
                } else null,
            } },
            .as => |as| blk: {
                const value = try self.bindPatternValue(as.binding_info, saved);
                break :blk .{ .as = .{
                    .pattern = try self.lowerPatScopedWithType(as.pattern, ty, saved),
                    .bind = value,
                } };
            },
            .var_ => |var_| blk: {
                const value = try self.bindPatternValue(var_.binding_info, saved);
                break :blk .{ .bind = value };
            },
            .tag => |tag| blk: {
                const resolved_tag = self.tagForType(ty, tag.tag);
                break :blk .{ .tag = .{
                    .union_shape = resolved_tag.union_shape,
                    .tag = resolved_tag.tag,
                    .payloads = try self.lowerTagPayloadPatternSpanForTag(ty, resolved_tag.tag, tag.payloads, saved),
                } };
            },
        } });
    }

    fn bindPatternValue(
        self: *BodyBuilder,
        binding: repr.BindingInfoId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.ExecutableValueRef {
        const value = self.output.freshValueRef();
        const previous = try self.env.fetchPut(binding, value);
        try saved.append(self.allocator, .{
            .binding = binding,
            .previous = if (previous) |entry| entry.value else null,
        });
        return value;
    }

    fn lowerPatSpanScoped(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.PatId),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        if (span.len == 0) return Ast.Span(Ast.PatId).empty();
        const input_items = self.input.pat_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerPatScoped(item, saved);
        }
        return try self.output.addPatSpan(output_items);
    }

    fn lowerRecordFieldPatternSpanScoped(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.RecordFieldPattern) {
        if (span.len == 0) return Ast.Span(Ast.RecordFieldPattern).empty();
        const input_items = self.input.record_field_patterns.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.RecordFieldPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .pattern = try self.lowerPatScoped(field.pattern, saved),
            };
        }
        return try self.output.addRecordFieldPatternSpan(output_items);
    }

    fn restoreBindings(self: *BodyBuilder, saved: *std.ArrayList(SavedBinding), start: usize) void {
        while (saved.items.len > start) {
            const binding = saved.pop().?;
            if (binding.previous) |previous| {
                self.env.put(binding.binding, previous) catch unreachable;
            } else {
                _ = self.env.remove(binding.binding);
            }
        }
    }

    fn lowerBranchProducer(
        self: *BodyBuilder,
        branch_id: LambdaSolved.Ast.BranchId,
        scrutinee_ty: Type.TypeId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.BranchId {
        const branch = self.input.branches.items[@intFromEnum(branch_id)];
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        if (branch.source_match_branch) |branch_ref| {
            if (!self.value_store.sourceMatchBranchReachable(branch_ref)) {
                const pat = try self.lowerUnreachableDecisionPat(branch.pat, scrutinee_ty);
                const body = try self.output.addExpr(
                    result_ty,
                    self.output.freshValueRef(),
                    .@"unreachable",
                );
                return try self.output.addBranch(.{
                    .pat = pat,
                    .guard = null,
                    .body = body,
                    .degenerate = branch.degenerate,
                });
            }
        }
        const pat = try self.lowerPatScopedWithType(branch.pat, scrutinee_ty, &saved);
        defer self.restoreBindings(&saved, 0);
        const guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null;
        return try self.output.addBranch(.{
            .pat = pat,
            .guard = guard,
            .body = try self.lowerExpr(branch.body),
            .degenerate = branch.degenerate,
        });
    }

    fn lowerBranchSpanProducer(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.BranchId),
        scrutinee_ty: Type.TypeId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.BranchId) {
        if (span.len == 0) return Ast.Span(Ast.BranchId).empty();
        const input_items = self.input.branch_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerBranchProducer(item, scrutinee_ty, result_ty);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn lowerBranchAtType(
        self: *BodyBuilder,
        branch_id: LambdaSolved.Ast.BranchId,
        join_id: repr.JoinInfoId,
        branch_index: u32,
        scrutinee_ty: Type.TypeId,
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.BranchId {
        const branch = self.input.branches.items[@intFromEnum(branch_id)];
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        if (branch.source_match_branch) |branch_ref| {
            if (!self.value_store.sourceMatchBranchReachable(branch_ref)) {
                const pat = try self.lowerUnreachableDecisionPat(branch.pat, scrutinee_ty);
                const body = try self.output.addExpr(
                    expected_ty,
                    self.output.freshValueRef(),
                    .@"unreachable",
                );
                return try self.output.addBranch(.{
                    .pat = pat,
                    .guard = null,
                    .body = body,
                    .degenerate = branch.degenerate,
                });
            }
        }
        const pat = try self.lowerPatScopedWithType(branch.pat, scrutinee_ty, &saved);
        defer self.restoreBindings(&saved, 0);
        const guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null;
        const body = if (self.lambdaExprCanCompleteNormally(branch.body)) blk: {
            const use_id = self.contextualMatchBranchConsumerUse(join_id, branch_index) orelse {
                executableInvariant("executable contextual source_match branch has no published consumer-use plan");
            };
            break :blk try self.lowerExprAtConsumerUse(branch.body, use_id);
        } else try self.lowerExpr(branch.body);
        if (self.lambdaExprCanCompleteNormally(branch.body) and self.output.getExpr(body).ty != expected_ty) {
            executableInvariant("executable contextual source_match branch type differs from expected type");
        }
        return try self.output.addBranch(.{
            .pat = pat,
            .guard = guard,
            .body = body,
            .degenerate = branch.degenerate,
        });
    }

    fn lowerBranchSpanAtType(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.BranchId),
        join_id: repr.JoinInfoId,
        scrutinee_ty: Type.TypeId,
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.BranchId) {
        if (span.len == 0) return Ast.Span(Ast.BranchId).empty();
        const input_items = self.input.branch_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerBranchAtType(item, join_id, @intCast(i), scrutinee_ty, expected_ty);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn lowerIfBranchAtType(
        self: *BodyBuilder,
        branch_expr: LambdaSolved.Ast.ExprId,
        join_id: repr.JoinInfoId,
        branch: repr.IfBranch,
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (!self.lambdaExprCanCompleteNormally(branch_expr)) {
            return try self.lowerExpr(branch_expr);
        }
        const use_id = self.contextualIfBranchConsumerUse(join_id, branch) orelse {
            executableInvariant("executable contextual if branch has no published consumer-use plan");
        };
        const body = try self.lowerExprAtConsumerUse(branch_expr, use_id);
        if (self.output.getExpr(body).ty != expected_ty) {
            executableInvariant("executable contextual if branch type differs from expected type");
        }
        return body;
    }

    fn lowerCaptureRef(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        slot: u32,
    ) Allocator.Error!Ast.ExprId {
        const capture_arg = self.capture_record_arg orelse executableInvariant("executable capture_ref reached procedure without capture record argument");
        const captures = self.value_store.sliceValueSpan(self.proc_instance.public_roots.captures);
        const capture_index: usize = @intCast(slot);
        if (capture_index >= captures.len) executableInvariant("executable capture_ref slot does not exist in procedure capture roots");

        const capture_record = try self.output.addExpr(
            capture_arg.ty,
            capture_arg.value,
            .{ .value_ref = capture_arg.value },
        );
        const capture_value = captures[capture_index];
        return try self.output.addExpr(
            try self.lowerExecutableValueType(source_ty, capture_value),
            self.output.freshValueRef(),
            .{ .tuple_access = .{
                .tuple = capture_record,
                .elem_index = slot,
            } },
        );
    }

    fn lowerForExpr(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        for_: anytype,
    ) Allocator.Error!Ast.ExprId {
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        defer self.restoreBindings(&saved, 0);
        return try self.output.addExpr(
            try self.lowerExecutableValueType(source_ty, value_info_id),
            self.output.freshValueRef(),
            .{ .for_ = .{
                .patt = patt,
                .iterable = try self.lowerExpr(for_.iterable),
                .body = try self.lowerExpr(for_.body),
            } },
        );
    }

    fn verifyLowLevelValueFlow(self: *const BodyBuilder, value_flow: repr.LowLevelValueFlowSignatureId) void {
        const index = @intFromEnum(value_flow);
        if (index >= self.value_store.low_level_value_flows.items.len) {
            executableInvariant("executable low-level expression referenced a missing lambda-solved value-flow signature");
        }
    }

    fn lowerStmt(self: *BodyBuilder, stmt_id: LambdaSolved.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.stmts.items[@intFromEnum(stmt_id)];
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_value = self.output.freshValueRef();
                try self.env.put(decl.bind.binding_info, bind_value);
                break :blk .{ .decl = .{
                    .value = bind_value,
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_value = self.output.freshValueRef();
                try self.env.put(decl.bind.binding_info, bind_value);
                break :blk .{ .decl = .{
                    .value = bind_value,
                    .body = body,
                } };
            },
            .reassign => |reassign| blk: {
                const body = try self.lowerExpr(reassign.body);
                const target = self.env.get(reassign.version) orelse executableInvariant("executable reassignment target has no lowered binding value");
                break :blk .{ .reassign = .{
                    .target = target,
                    .body = body,
                } };
            },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |return_| blk: {
                break :blk .{ .return_ = try self.lowerReturnValue(return_.expr, return_.return_info) };
            },
            .break_ => .break_,
            .for_ => |for_| blk: {
                var saved = std.ArrayList(SavedBinding).empty;
                defer saved.deinit(self.allocator);
                const patt = try self.lowerPatScoped(for_.patt, &saved);
                defer self.restoreBindings(&saved, 0);
                break :blk .{ .for_ = .{
                    .patt = patt,
                    .iterable = try self.lowerExpr(for_.iterable),
                    .body = try self.lowerExpr(for_.body),
                } };
            },
            .while_ => |while_| .{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } },
        });
    }

    fn lowerStmtSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        if (span.len == 0) return Ast.Span(Ast.StmtId).empty();
        const input_items = self.input.stmt_ids.items[span.start..][0..span.len];
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerExprIds(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (span.len == 0) return Ast.Span(Ast.ExprId).empty();
        const input_items = self.input.expr_ids.items[span.start..][0..span.len];
        const exprs = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(exprs);
        for (input_items, 0..) |expr, i| {
            exprs[i] = try self.lowerExpr(expr);
        }
        return try self.output.addExprSpan(exprs);
    }

    const TypeTag = struct {
        union_shape: MonoRow.TagUnionShapeId,
        tag: MonoRow.TagId,
    };

    const TypeTagConstruction = struct {
        union_shape: MonoRow.TagUnionShapeId,
        tag_type: Type.TagType,
    };

    fn tagForType(
        self: *BodyBuilder,
        ty: Type.TypeId,
        source_tag: MonoRow.TagId,
    ) TypeTag {
        const resolved = self.tagTypeForType(ty, source_tag);
        return .{
            .union_shape = resolved.union_shape,
            .tag = resolved.tag_type.tag,
        };
    }

    fn tagTypeForType(
        self: *BodyBuilder,
        ty: Type.TypeId,
        source_tag: MonoRow.TagId,
    ) TypeTagConstruction {
        return switch (self.program.types.getType(ty)) {
            .tag_union => |tag_union| self.tagTypeForTagUnionType(tag_union, source_tag),
            .nominal => |nominal| self.tagTypeForType(nominal.backing, source_tag),
            else => executableInvariant("executable tag construction expected a tag-union endpoint"),
        };
    }

    fn tagTypeForTagUnionType(
        self: *BodyBuilder,
        tag_union: Type.TagUnionType,
        source_tag: MonoRow.TagId,
    ) TypeTagConstruction {
        const source_info = self.program.row_shapes.tag(source_tag);
        const source_payloads = self.program.row_shapes.tagPayloads(source_tag);
        for (tag_union.tags) |target_tag| {
            const target_info = self.program.row_shapes.tag(target_tag.tag);
            if (target_info.label != source_info.label) continue;
            if (target_tag.payloads.len != source_payloads.len) {
                executableInvariant("executable tag row re-keying found tag label with different payload arity");
            }
            return .{
                .union_shape = tag_union.shape,
                .tag_type = target_tag,
            };
        }
        executableInvariant("executable tag row re-keying could not find tag label in expression type");
    }

    fn recordTypeForConstruction(
        self: *BodyBuilder,
        ty: Type.TypeId,
    ) Type.RecordType {
        return switch (self.program.types.getType(ty)) {
            .record => |record| record,
            .nominal => |nominal| self.recordTypeForConstruction(nominal.backing),
            else => executableInvariant("executable record construction expected a record endpoint"),
        };
    }

    fn recordFieldForConstruction(
        self: *BodyBuilder,
        record: Type.RecordType,
        source_field: MonoRow.RecordFieldId,
    ) Type.RecordFieldType {
        const source_label = self.program.row_shapes.recordField(source_field).label;
        for (record.fields) |target_field| {
            const target_label = self.program.row_shapes.recordField(target_field.field).label;
            if (target_label == source_label) return target_field;
        }
        executableInvariant("executable record construction could not find field label in expected endpoint");
    }

    fn tupleTypesForConstruction(
        self: *BodyBuilder,
        ty: Type.TypeId,
    ) []const Type.TypeId {
        return switch (self.program.types.getType(ty)) {
            .tuple => |items| items,
            .nominal => |nominal| self.tupleTypesForConstruction(nominal.backing),
            else => executableInvariant("executable tuple construction expected a tuple endpoint"),
        };
    }

    fn listElemTypeForConstruction(
        self: *BodyBuilder,
        ty: Type.TypeId,
    ) Type.TypeId {
        return switch (self.program.types.getType(ty)) {
            .list => |elem| elem,
            .nominal => |nominal| self.listElemTypeForConstruction(nominal.backing),
            else => executableInvariant("executable list construction expected a list endpoint"),
        };
    }

    fn payloadForTag(
        self: *BodyBuilder,
        target_tag: MonoRow.TagId,
        source_payload: MonoRow.TagPayloadId,
    ) MonoRow.TagPayloadId {
        const source_payload_info = self.program.row_shapes.tagPayload(source_payload);
        const source_tag_info = self.program.row_shapes.tag(source_payload_info.tag);
        const target_tag_info = self.program.row_shapes.tag(target_tag);
        if (source_tag_info.label != target_tag_info.label) {
            executableInvariant("executable tag payload row re-keying crossed tag labels");
        }
        const target_payloads = self.program.row_shapes.tagPayloads(target_tag);
        const payload_index: usize = @intCast(source_payload_info.logical_index);
        if (payload_index >= target_payloads.len) {
            executableInvariant("executable tag payload row re-keying source index exceeded target arity");
        }
        return target_payloads[payload_index];
    }

    fn payloadTypeForTagType(
        self: *BodyBuilder,
        target_tag: Type.TagType,
        source_payload: MonoRow.TagPayloadId,
    ) Type.TagPayloadType {
        const target_payload = self.payloadForTag(target_tag.tag, source_payload);
        for (target_tag.payloads) |payload| {
            if (payload.payload == target_payload) return payload;
        }
        executableInvariant("executable tag construction could not find payload in expected endpoint");
    }

    fn lowerTagPayloadPatternSpanForTag(
        self: *BodyBuilder,
        parent_ty: Type.TypeId,
        target_tag: MonoRow.TagId,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        const target_payloads = self.program.row_shapes.tagPayloads(target_tag);
        if (span.len == 0) {
            if (target_payloads.len != 0) {
                executableInvariant("executable tag pattern row re-keying payload arity mismatch");
            }
            return Ast.Span(Ast.TagPayloadPattern).empty();
        }
        const input_items = self.input.tag_payload_patterns.items[span.start..][0..span.len];
        if (input_items.len != target_payloads.len) {
            executableInvariant("executable tag pattern row re-keying payload arity mismatch");
        }
        const payloads = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(payloads);
        var seen = try self.allocator.alloc(bool, target_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (input_items, 0..) |payload, i| {
            const target_payload = self.payloadForTag(target_tag, payload.payload);
            const payload_index: usize = @intCast(self.program.row_shapes.tagPayload(target_payload).logical_index);
            if (seen[payload_index]) {
                executableInvariant("executable tag pattern row re-keying duplicated payload");
            }
            seen[payload_index] = true;
            const child_ty = self.tagPayloadTypeForPattern(parent_ty, target_payload);
            payloads[i] = .{
                .payload = target_payload,
                .pattern = try self.lowerPatScopedWithType(payload.pattern, child_ty, saved),
            };
        }
        verifyAllSeen(seen, "executable tag pattern row re-keying omitted payload");
        return try self.output.addTagPayloadPatternSpan(payloads);
    }

    fn lowerNominalBackingAtType(
        self: *BodyBuilder,
        parent_value: repr.ValueInfoId,
        backing: LambdaSolved.Ast.ExprId,
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const use_id = self.nominalBackingConsumerUse(parent_value) orelse {
            return switch (self.program.types.getType(expected_ty)) {
                .nominal => |nominal| try self.lowerExprAtType(backing, nominal.backing),
                else => try self.lowerExprAtType(backing, expected_ty),
            };
        };
        return try self.lowerExprAtConsumerUse(backing, use_id);
    }

    fn nominalBackingConsumerUse(
        self: *const BodyBuilder,
        parent_value: repr.ValueInfoId,
    ) ?repr.ConsumerUsePlanId {
        return self.value_store.values.items[@intFromEnum(parent_value)].nominal_backing_consumer_use;
    }

    fn recordFieldConsumerUse(
        self: *const BodyBuilder,
        parent_value: repr.ValueInfoId,
        field: MonoRow.RecordFieldId,
    ) ?repr.ConsumerUsePlanId {
        const aggregate = self.value_store.values.items[@intFromEnum(parent_value)].aggregate orelse return null;
        const record = switch (aggregate) {
            .record => |record| record,
            else => return null,
        };
        for (record.fields) |candidate| {
            if (candidate.field == field) return candidate.consumer_use;
        }
        return null;
    }

    fn tupleElemConsumerUse(
        self: *const BodyBuilder,
        parent_value: repr.ValueInfoId,
        index: u32,
    ) ?repr.ConsumerUsePlanId {
        const aggregate = self.value_store.values.items[@intFromEnum(parent_value)].aggregate orelse return null;
        const elems = switch (aggregate) {
            .tuple => |elems| elems,
            else => return null,
        };
        for (elems) |candidate| {
            if (candidate.index == index) return candidate.consumer_use;
        }
        return null;
    }

    fn tagPayloadConsumerUse(
        self: *const BodyBuilder,
        parent_value: repr.ValueInfoId,
        payload: MonoRow.TagPayloadId,
    ) ?repr.ConsumerUsePlanId {
        const aggregate = self.value_store.values.items[@intFromEnum(parent_value)].aggregate orelse return null;
        const tag = switch (aggregate) {
            .tag => |tag| tag,
            else => return null,
        };
        for (tag.payloads) |candidate| {
            if (candidate.payload == payload) return candidate.consumer_use;
        }
        return null;
    }

    fn listElemConsumerUse(
        self: *const BodyBuilder,
        parent_value: repr.ValueInfoId,
        index: u32,
    ) ?repr.ConsumerUsePlanId {
        const aggregate = self.value_store.values.items[@intFromEnum(parent_value)].aggregate orelse return null;
        const list = switch (aggregate) {
            .list => |list| list,
            else => return null,
        };
        for (list.elems) |candidate| {
            if (candidate.index == index) return candidate.consumer_use;
        }
        return null;
    }

    fn contextualIfBranchConsumerUse(
        self: *const BodyBuilder,
        join_id: repr.JoinInfoId,
        branch: repr.IfBranch,
    ) ?repr.ConsumerUsePlanId {
        const join_index = @intFromEnum(join_id);
        if (join_index >= self.value_store.joins.items.len) {
            executableInvariant("executable contextual if consumer-use referenced missing join");
        }
        const join = self.value_store.joins.items[join_index];
        if (join.kind != .if_expr) {
            executableInvariant("executable contextual if consumer-use referenced non-if join");
        }
        const inputs = self.value_store.sliceJoinInputSpan(join.inputs);
        const uses = self.value_store.sliceConsumerUsePlanSpan(join.contextual_consumer_uses);
        if (uses.len != inputs.len) {
            executableInvariant("executable contextual if consumer-use count differs from join inputs");
        }
        for (inputs, uses) |input, use_id| {
            const source = switch (input.source) {
                .if_branch => |if_branch| if_branch,
                else => executableInvariant("executable contextual if consumer-use saw non-if join input"),
            };
            if (source.branch == branch) return use_id;
        }
        return null;
    }

    fn contextualMatchBranchConsumerUse(
        self: *const BodyBuilder,
        join_id: repr.JoinInfoId,
        branch_index: u32,
    ) ?repr.ConsumerUsePlanId {
        const join_index = @intFromEnum(join_id);
        if (join_index >= self.value_store.joins.items.len) {
            executableInvariant("executable contextual match consumer-use referenced missing join");
        }
        const join = self.value_store.joins.items[join_index];
        if (join.kind != .match_expr) {
            executableInvariant("executable contextual match consumer-use referenced non-match join");
        }
        const inputs = self.value_store.sliceJoinInputSpan(join.inputs);
        const uses = self.value_store.sliceConsumerUsePlanSpan(join.contextual_consumer_uses);
        if (uses.len != inputs.len) {
            executableInvariant("executable contextual match consumer-use count differs from join inputs");
        }
        for (inputs, uses) |input, use_id| {
            const source = switch (input.source) {
                .source_match_branch => |match_branch| match_branch,
                else => executableInvariant("executable contextual match consumer-use saw non-match join input"),
            };
            if (@intFromEnum(source.branch) == branch_index) return use_id;
        }
        return null;
    }

    fn recordAssemblyEval(
        evals: []const LambdaSolved.Ast.RecordFieldEval,
        assembly: LambdaSolved.Ast.RecordFieldAssembly,
    ) LambdaSolved.Ast.RecordFieldEval {
        if (assembly.eval_index >= evals.len) {
            executableInvariant("executable record assembly referenced eval index outside eval order");
        }
        const evaluated = evals[assembly.eval_index];
        if (evaluated.field != assembly.field) {
            executableInvariant("executable record assembly field disagreed with eval-order field");
        }
        return evaluated;
    }

    fn tagAssemblyEval(
        evals: []const LambdaSolved.Ast.TagPayloadEval,
        assembly: LambdaSolved.Ast.TagPayloadAssembly,
    ) LambdaSolved.Ast.TagPayloadEval {
        if (assembly.eval_index >= evals.len) {
            executableInvariant("executable tag assembly referenced eval index outside eval order");
        }
        const evaluated = evals[assembly.eval_index];
        if (evaluated.payload != assembly.payload) {
            executableInvariant("executable tag assembly payload disagreed with eval-order payload");
        }
        return evaluated;
    }

    fn lowerRecordFieldsForType(
        self: *BodyBuilder,
        parent_value: repr.ValueInfoId,
        eval_order: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldEval),
        assembly_order: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldAssembly),
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.RecordFieldExpr) {
        const record_ty = self.recordTypeForConstruction(expected_ty);
        if (assembly_order.len == 0) {
            if (record_ty.fields.len != 0) {
                executableInvariant("executable record construction expected fields but source assembly was empty");
            }
            return Ast.Span(Ast.RecordFieldExpr).empty();
        }
        const evals = self.input.record_field_evals.items[eval_order.start..][0..eval_order.len];
        const input_items = self.input.record_field_assemblies.items[assembly_order.start..][0..assembly_order.len];
        if (input_items.len != record_ty.fields.len) {
            executableInvariant("executable record construction field arity disagreed with expected endpoint");
        }
        const values = try self.allocator.alloc(Ast.RecordFieldExpr, record_ty.fields.len);
        defer self.allocator.free(values);
        const seen = try self.allocator.alloc(bool, record_ty.fields.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (input_items) |field| {
            const target_field = self.recordFieldForConstruction(record_ty, field.field);
            const field_index: usize = @intCast(self.program.row_shapes.recordField(target_field.field).logical_index);
            if (field_index >= values.len) {
                executableInvariant("executable record construction field index exceeded expected endpoint arity");
            }
            if (seen[field_index]) {
                executableInvariant("executable record construction saw duplicate field");
            }
            seen[field_index] = true;
            const use_id = self.recordFieldConsumerUse(parent_value, field.field) orelse
                executableInvariant("executable record construction field had no published consumer-use plan");
            const evaluated = recordAssemblyEval(evals, field);
            const lowered = try self.lowerExprAtConsumerUse(evaluated.value, use_id);
            const lowered_expr = self.output.getExpr(lowered);
            values[field_index] = .{
                .field = target_field.field,
                .expr = lowered,
                .ty = target_field.ty,
                .value = lowered_expr.value,
                .bridge = try self.constructionSlotBridge(lowered_expr.ty, target_field.ty),
            };
        }
        verifyAllSeen(seen, "executable record construction omitted field");
        return try self.output.addRecordFieldExprSpan(values);
    }

    fn lowerTagPayloadValuesForTagType(
        self: *BodyBuilder,
        parent_value: repr.ValueInfoId,
        target_tag: Type.TagType,
        eval_order: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadEval),
        assembly_order: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadAssembly),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadExpr) {
        if (assembly_order.len == 0) {
            if (target_tag.payloads.len != 0) {
                executableInvariant("executable tag construction expected payloads but source assembly was empty");
            }
            return Ast.Span(Ast.TagPayloadExpr).empty();
        }
        const evals = self.input.tag_payload_evals.items[eval_order.start..][0..eval_order.len];
        const input_items = self.input.tag_payload_assemblies.items[assembly_order.start..][0..assembly_order.len];
        if (input_items.len != target_tag.payloads.len) {
            executableInvariant("executable tag construction payload arity disagreed with expected endpoint");
        }
        const values = try self.allocator.alloc(Ast.TagPayloadExpr, target_tag.payloads.len);
        defer self.allocator.free(values);
        const seen = try self.allocator.alloc(bool, target_tag.payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (input_items) |payload| {
            const target_payload = self.payloadTypeForTagType(target_tag, payload.payload);
            const payload_index: usize = @intCast(self.program.row_shapes.tagPayload(target_payload.payload).logical_index);
            if (payload_index >= values.len) {
                executableInvariant("executable tag construction payload index exceeded expected endpoint arity");
            }
            if (seen[payload_index]) {
                executableInvariant("executable tag construction saw duplicate payload");
            }
            seen[payload_index] = true;
            const use_id = self.tagPayloadConsumerUse(parent_value, payload.payload) orelse
                executableInvariant("executable tag construction payload had no published consumer-use plan");
            const plan = self.representation_store.consumerUsePlan(use_id);
            const planned_ty = try self.lowerSessionExecutableEndpointType(plan.expected_endpoint);
            if (planned_ty != target_payload.ty) {
                executableInvariantFmt(
                    "executable tag payload consumer-use endpoint disagreed with target payload type: planned={} target={}",
                    .{ planned_ty, target_payload.ty },
                );
            }
            const evaluated = tagAssemblyEval(evals, payload);
            const lowered = try self.lowerExprAtConsumerUse(evaluated.value, use_id);
            const lowered_expr = self.output.getExpr(lowered);
            values[payload_index] = .{
                .payload = target_payload.payload,
                .expr = lowered,
                .ty = target_payload.ty,
                .value = lowered_expr.value,
                .bridge = try self.constructionSlotBridge(lowered_expr.ty, target_payload.ty),
            };
        }
        verifyAllSeen(seen, "executable tag construction omitted payload");
        return try self.output.addTagPayloadExprSpan(values);
    }

    fn lowerTupleItemsForType(
        self: *BodyBuilder,
        parent_value: repr.ValueInfoId,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.ExprId),
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.TupleItemExpr) {
        const tuple_tys = self.tupleTypesForConstruction(expected_ty);
        if (span.len == 0) {
            if (tuple_tys.len != 0) {
                executableInvariant("executable tuple construction expected elements but source assembly was empty");
            }
            return Ast.Span(Ast.TupleItemExpr).empty();
        }
        const input_items = self.input.expr_ids.items[span.start..][0..span.len];
        if (input_items.len != tuple_tys.len) {
            executableInvariant("executable tuple construction arity disagreed with expected endpoint");
        }
        const exprs = try self.allocator.alloc(Ast.TupleItemExpr, input_items.len);
        defer self.allocator.free(exprs);
        for (input_items, tuple_tys, 0..) |expr, elem_ty, i| {
            const use_id = self.tupleElemConsumerUse(parent_value, @intCast(i)) orelse
                executableInvariant("executable tuple construction element had no published consumer-use plan");
            const lowered = try self.lowerExprAtConsumerUse(expr, use_id);
            const lowered_expr = self.output.getExpr(lowered);
            exprs[i] = .{
                .expr = lowered,
                .ty = elem_ty,
                .value = lowered_expr.value,
                .bridge = try self.constructionSlotBridge(lowered_expr.ty, elem_ty),
            };
        }
        return try self.output.addTupleItemExprSpan(exprs);
    }

    fn lowerListItemsForType(
        self: *BodyBuilder,
        parent_value: repr.ValueInfoId,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.ExprId),
        expected_ty: Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.ListItemExpr) {
        if (span.len == 0) return Ast.Span(Ast.ListItemExpr).empty();
        const elem_ty = self.listElemTypeForConstruction(expected_ty);
        const input_items = self.input.expr_ids.items[span.start..][0..span.len];
        const exprs = try self.allocator.alloc(Ast.ListItemExpr, input_items.len);
        defer self.allocator.free(exprs);
        for (input_items, 0..) |expr, i| {
            const use_id = self.listElemConsumerUse(parent_value, @intCast(i)) orelse
                executableInvariant("executable list construction element had no published consumer-use plan");
            const lowered = try self.lowerExprAtConsumerUse(expr, use_id);
            const lowered_expr = self.output.getExpr(lowered);
            exprs[i] = .{
                .expr = lowered,
                .ty = elem_ty,
                .value = lowered_expr.value,
                .bridge = try self.constructionSlotBridge(lowered_expr.ty, elem_ty),
            };
        }
        return try self.output.addListItemExprSpan(exprs);
    }

    fn lowerCallProc(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const dispatch = self.value_store.call_sites.items[@intFromEnum(call.call_site)].dispatch orelse executableInvariant("executable call_proc reached unresolved call-site dispatch");
        const target_instance_id = switch (dispatch) {
            .call_proc => |target| target,
            .call_value_finite,
            .call_value_erased,
            .pending_local_root_call,
            => executableInvariant("executable call_proc reached non-procedure call-site dispatch"),
        };
        const target_proc = self.proc_exec_map.get(target_instance_id) orelse executableInvariant("executable call_proc target was not reserved before body lowering");
        const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
        if (!canonical.mirProcedureRefEql(target_instance.proc, call.proc)) {
            executableInvariant("executable call_proc dispatch target differs from expression target");
        }

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const call_site = self.value_store.call_sites.items[@intFromEnum(call.call_site)];
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable call_proc call-site requested source type differs from expression");
        }
        if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, call_site.requested_source_fn_ty)) {
            executableInvariant("executable call_proc target specialization source type differs from call site");
        }

        const arg_consumer_use_ids = self.value_store.sliceConsumerUsePlanSpan(call_site.arg_consumer_uses);
        if (arg_consumer_use_ids.len != arg_items.len) {
            executableInvariant("executable call_proc argument consumer-use count differs from call arity");
        }
        const result_transform_id = call_site.result_transform orelse {
            executableInvariant("executable call_proc has no result transform");
        };
        const result_boundary = self.representation_store.valueTransformBoundary(result_transform_id);
        self.verifyCallProcResultBoundary(result_boundary, call.call_site, target_instance_id, target_instance);

        const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_items.len);
        defer self.allocator.free(direct_args);
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        for (arg_items, 0..) |arg, i| {
            self.verifyCallProcArgConsumerUse(arg_consumer_use_ids[i], arg, call.call_site, target_instance_id, target_instance, @intCast(i));
            const lowered = try self.lowerExprAtConsumerUse(arg, arg_consumer_use_ids[i]);
            const value = try self.materializeExprValue(&stmt_ids, lowered);
            direct_args[i] = .{ .value = value };
        }

        const raw_result_ty = try self.lowerSessionExecutableEndpointType(result_boundary.from_endpoint);
        const raw_result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(raw_result_ty, raw_result_value, .{ .call_direct = .{
            .source = call.proc.proc,
            .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
            .executable_proc = target_proc,
            .direct_args = try self.output.addDirectCallArgSpan(direct_args),
        } });
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = raw_result_value,
            .body = final_call,
        } }));
        const result_value = try self.applyValueTransformBoundary(&stmt_ids, result_boundary, raw_result_value);
        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const final_expr = try self.output.addValueRefExpr(result_ty, result_value);

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
    }

    fn verifyCallProcArgConsumerUse(
        self: *BodyBuilder,
        use_id: repr.ConsumerUsePlanId,
        arg_expr: LambdaSolved.Ast.ExprId,
        call_site_id: repr.CallSiteInfoId,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
        index: u32,
    ) void {
        const plan = self.representation_store.consumerUsePlan(use_id);
        const owner = switch (plan.owner) {
            .call_arg => |call_arg| call_arg,
            else => executableInvariant("executable call argument consumer-use has non-call-arg owner"),
        };
        if (owner.call != call_site_id or owner.arg_index != index) {
            executableInvariant("executable call argument consumer-use owner differs from call site");
        }
        const source_expr = self.input.exprs.items[@intFromEnum(arg_expr)];
        if (source_expr.value_info != plan.child_value) {
            executableInvariant("executable call argument consumer-use child value differs from argument expression");
        }
        const to = switch (plan.expected_endpoint.owner) {
            .procedure_param => |param| param,
            else => executableInvariant("executable call_proc argument consumer-use endpoint is not a procedure parameter"),
        };
        if (to.instance != target_instance_id or to.index != index) {
            executableInvariant("executable call_proc argument consumer-use endpoint differs from target procedure");
        }
        const arg_index: usize = @intCast(index);
        if (arg_index >= target_instance.executable_specialization_key.exec_arg_tys.len) {
            executableInvariant("executable call_proc argument consumer-use index exceeds target arity");
        }
        if (!repr.canonicalExecValueTypeKeyEql(plan.expected_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
            executableInvariant("executable call_proc argument consumer-use endpoint key differs from target specialization");
        }
    }

    fn verifyCallProcResultBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        call_site_id: repr.CallSiteInfoId,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
    ) void {
        _ = self;
        const kind = switch (boundary.kind) {
            .call_result => |call_result| call_result,
            else => executableInvariant("executable call_proc result transform has non-call-result boundary kind"),
        };
        if (kind != call_site_id) {
            executableInvariant("executable call_proc result transform boundary kind differs from call site");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .procedure_return => |proc| proc,
            else => executableInvariant("executable call_proc result transform source is not a procedure return"),
        };
        if (from != target_instance_id) {
            executableInvariant("executable call_proc result transform source differs from target procedure");
        }
        switch (boundary.to_endpoint.owner) {
            .local_value => {},
            else => executableInvariant("executable call_proc result transform target is not a local value"),
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.from_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_ret_ty)) {
            executableInvariant("executable call_proc result endpoint key differs from target specialization");
        }
    }

    fn lowerProcValue(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        proc_value: anytype,
    ) Allocator.Error!Ast.ExprId {
        const value_info = self.value_store.values.items[@intFromEnum(value_info_id)];
        const callable = value_info.callable orelse executableInvariant("executable proc_value reached value without callable metadata");
        const emission = self.representation_store.callableEmissionPlan(callable.emission_plan);
        switch (emission) {
            .pending_proc_value => executableInvariant("executable proc_value reached pending callable emission"),
            .finite => {},
            .erase_proc_value => |erase| return try self.lowerProcValueErased(source_ty, value_info_id, callable, proc_value, erase),
            .erase_finite_set => |erase| return try self.lowerFiniteSetValueErased(source_ty, value_info_id, callable, proc_value, erase),
            .already_erased => executableInvariant("executable proc_value reached erased emission that is not a proc-value erase plan"),
        }

        const construction_id = callable.construction_plan orelse executableInvariant("executable proc_value reached finite callable value without construction metadata");
        const construction = self.representation_store.callableConstructionPlan(construction_id);
        if (construction.result != value_info_id) {
            executableInvariant("executable proc_value construction plan is attached to the wrong value");
        }
        const emission_key = switch (emission) {
            .finite => |key| key,
            else => executableInvariant("executable proc_value construction plan does not have finite callable emission"),
        };
        if (!repr.callableSetKeyEql(emission_key, construction.callable_set_key)) {
            executableInvariant("executable proc_value construction key differs from finite emission key");
        }
        const member = self.representation_store.callableSetMember(construction.callable_set_key, construction.selected_member) orelse {
            executableInvariant("executable proc_value construction selected a missing callable-set member");
        };
        if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, construction.source_fn_ty)) {
            executableInvariant("executable proc_value construction source function type differs from descriptor member");
        }
        if (member.capture_slots.len != construction.capture_values.len) {
            executableInvariant("executable proc_value construction capture count differs from descriptor member");
        }
        const capture_items = self.input.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
        if (capture_items.len != construction.capture_values.len) {
            executableInvariant("executable proc_value capture arity does not match construction plan");
        }
        const capture_refs = try self.allocator.alloc(Ast.CaptureValueRef, capture_items.len);
        defer self.allocator.free(capture_refs);
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        for (capture_items, 0..) |capture, i| {
            if (member.capture_slots[i].slot != capture.slot) {
                executableInvariant("executable proc_value capture slot differs from construction member schema");
            }
            if (capture.value_info != construction.capture_values[i]) {
                executableInvariant("executable proc_value capture value differs from construction plan");
            }
            if (i >= construction.capture_transforms.len) {
                executableInvariant("executable proc_value construction omitted a capture transform");
            }
            const boundary = self.representation_store.valueTransformBoundary(construction.capture_transforms[i]);
            self.verifyCallableConstructionCaptureBoundary(boundary, construction_id, construction, member, capture.slot, capture.value_info);
            const lowered = try self.lowerExpr(capture.expr);
            const raw_value = try self.materializeExprValue(&stmt_ids, lowered);
            const value = try self.applyValueTransformBoundary(&stmt_ids, boundary, raw_value);
            capture_refs[i] = .{
                .slot = capture.slot,
                .value = value,
                .exec_ty = try self.lowerSessionExecutableEndpointType(boundary.to_endpoint),
            };
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, value_info_id);
        const result_value = self.output.freshValueRef();
        const final_value = try self.output.addExpr(result_ty, result_value, .{ .callable_set_value = .{
            .construction_plan = construction_id,
            .callable_set_key = construction.callable_set_key,
            .member = .{
                .callable_set_key = construction.callable_set_key,
                .member_index = construction.selected_member,
            },
            .capture_record = if (capture_refs.len == 0) null else .{
                .capture_shape_key = member.capture_shape_key,
                .values = try self.output.addCaptureValueRefSpan(capture_refs),
                .record_tmp = self.output.freshValueRef(),
            },
        } });

        if (stmt_ids.items.len == 0) return final_value;

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_value,
        } });
    }

    fn lowerFiniteSetValueErased(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        callable: repr.CallableValueInfo,
        proc_value: anytype,
        erase: repr.FiniteSetErasePlan,
    ) Allocator.Error!Ast.ExprId {
        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => executableInvariant("executable finite-set erase plan is attached to a non-proc callable source"),
        };
        if (!canonical.mirProcedureRefEql(source.proc, proc_value.proc)) {
            executableInvariant("executable finite-set erase source procedure differs from proc_value expression");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, erase.adapter.source_fn_ty)) {
            executableInvariant("executable finite-set erase source type differs from adapter key");
        }

        const construction_id = callable.construction_plan orelse {
            executableInvariant("executable finite-set erase reached callable value without construction metadata");
        };
        const construction = self.representation_store.callableConstructionPlan(construction_id);
        if (construction.result != value_info_id) {
            executableInvariant("executable finite-set erase construction plan is attached to the wrong value");
        }
        if (!repr.callableSetKeyEql(construction.callable_set_key, erase.adapter.callable_set_key)) {
            executableInvariant("executable finite-set erase construction key differs from adapter key");
        }
        if (!repr.canonicalTypeKeyEql(construction.source_fn_ty, erase.adapter.source_fn_ty)) {
            executableInvariant("executable finite-set erase construction source type differs from adapter key");
        }

        const descriptor = callableSetDescriptorFromSlice(self.callable_set_descriptors, erase.adapter.callable_set_key) orelse {
            executableInvariant("executable finite-set erase adapter has no callable-set descriptor");
        };
        const member = self.representation_store.callableSetMember(construction.callable_set_key, construction.selected_member) orelse {
            executableInvariant("executable finite-set erase construction selected a missing callable-set member");
        };
        if (member.capture_slots.len != construction.capture_values.len) {
            executableInvariant("executable finite-set erase capture count differs from descriptor member");
        }

        const capture_items = self.input.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
        if (capture_items.len != construction.capture_values.len) {
            executableInvariant("executable finite-set erase proc_value capture arity does not match construction plan");
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, value_info_id);
        const hidden_capture_ty = try self.lowerFiniteSetAdapterCaptureType(erase.adapter, descriptor);
        if (hidden_capture_ty == null and (descriptor.members.len != 1 or capture_items.len != 0)) {
            executableInvariant("executable finite-set erase without hidden capture cannot preserve callable-set value");
        }

        const capture_refs: []Ast.CaptureValueRef = if (capture_items.len == 0)
            &.{}
        else
            try self.allocator.alloc(Ast.CaptureValueRef, capture_items.len);
        defer if (capture_refs.len > 0) self.allocator.free(capture_refs);
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        for (capture_items, 0..) |capture, i| {
            if (capture.slot != @as(u32, @intCast(i))) {
                executableInvariant("executable finite-set erase proc_value capture slots are not canonical");
            }
            if (capture.value_info != construction.capture_values[i]) {
                executableInvariant("executable finite-set erase proc_value capture value differs from construction plan");
            }
            if (member.capture_slots[i].slot != capture.slot) {
                executableInvariant("executable finite-set erase capture slot differs from descriptor member");
            }
            if (i >= construction.capture_transforms.len) {
                executableInvariant("executable finite-set erase construction omitted a capture transform");
            }
            const boundary = self.representation_store.valueTransformBoundary(construction.capture_transforms[i]);
            self.verifyCallableConstructionCaptureBoundary(boundary, construction_id, construction, member, capture.slot, capture.value_info);
            const lowered = try self.lowerExpr(capture.expr);
            const raw_value = try self.materializeExprValue(&stmt_ids, lowered);
            const value = try self.applyValueTransformBoundary(&stmt_ids, boundary, raw_value);
            capture_refs[i] = .{
                .slot = capture.slot,
                .value = value,
                .exec_ty = try self.lowerSessionExecutableEndpointType(boundary.to_endpoint),
            };
        }

        const hidden_capture_value: ?Ast.ExecutableValueRef = if (hidden_capture_ty) |capture_ty| blk: {
            const value = self.output.freshValueRef();
            const capture_expr = try self.output.addExpr(capture_ty, value, .{ .callable_set_value = .{
                .construction_plan = construction_id,
                .callable_set_key = construction.callable_set_key,
                .member = .{
                    .callable_set_key = construction.callable_set_key,
                    .member_index = construction.selected_member,
                },
                .capture_record = if (capture_refs.len == 0) null else .{
                    .capture_shape_key = member.capture_shape_key,
                    .values = try self.output.addCaptureValueRefSpan(capture_refs),
                    .record_tmp = self.output.freshValueRef(),
                },
            } });
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = capture_expr,
            } }));
            break :blk value;
        } else null;

        const result_value = self.output.freshValueRef();
        const final_value = try self.output.addExpr(result_ty, result_value, .{ .packed_erased_fn = .{
            .sig_key = erase.adapter.erased_fn_sig_key,
            .code = self.executableProcForErasedAdapter(erase.adapter),
            .capture = hidden_capture_value,
            .capture_ty = hidden_capture_ty,
            .capture_shape = erase.adapter.capture_shape_key,
        } });

        if (stmt_ids.items.len == 0) return final_value;
        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_value,
        } });
    }

    fn lowerProcValueErased(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        callable: repr.CallableValueInfo,
        proc_value: anytype,
        erase: repr.ProcValueErasePlan,
    ) Allocator.Error!Ast.ExprId {
        if (erase.source_value != value_info_id) {
            executableInvariant("executable proc-value erase plan is attached to the wrong value");
        }
        if (!canonical.procedureCallableRefEql(erase.proc_value, proc_value.proc.callable)) {
            executableInvariant("executable proc-value erase plan procedure differs from proc_value expression");
        }
        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => executableInvariant("executable proc-value erase plan is attached to a non-proc callable source"),
        };
        if (!canonical.mirProcedureRefEql(source.proc, proc_value.proc)) {
            executableInvariant("executable proc-value erase source procedure differs from proc_value expression");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, erase.proc_value.source_fn_ty)) {
            executableInvariant("executable proc-value erase source function type differs from erase plan");
        }
        if (source.captures.len != erase.capture_slots.len) {
            executableInvariant("executable proc-value erase source capture count differs from erase plan");
        }

        const capture_items = self.input.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
        if (capture_items.len != erase.capture_slots.len) {
            executableInvariant("executable proc-value erase capture arity does not match erase plan");
        }

        const result_ty = try self.lowerExecutableValueType(source_ty, value_info_id);
        const capture_ty = self.erasedFnCaptureType(result_ty, erase.erased_fn_sig_key);
        if ((capture_ty != null) != (erase.erased_fn_sig_key.capture_ty != null)) {
            executableInvariant("executable proc-value erase capture type disagrees with erased signature");
        }
        if (capture_ty == null and erase.capture_slots.len != 0) {
            executableInvariant("executable proc-value erase has captures but no hidden capture type");
        }

        const selected_executable_proc = self.executableProcForErasedDirectProcAdapter(.{
            .proc_value = erase.proc_value,
            .capture_shape_key = erase.capture_shape_key,
        });

        const lowered_captures: []Ast.ExprId = if (capture_items.len == 0)
            &.{}
        else
            try self.allocator.alloc(Ast.ExprId, capture_items.len);
        defer if (lowered_captures.len > 0) self.allocator.free(lowered_captures);
        const seen: []bool = if (capture_items.len == 0)
            &.{}
        else
            try self.allocator.alloc(bool, capture_items.len);
        defer if (seen.len > 0) self.allocator.free(seen);
        if (seen.len > 0) @memset(seen, false);

        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        for (capture_items) |capture| {
            const slot_index: usize = @intCast(capture.slot);
            if (slot_index >= capture_items.len) executableInvariant("executable proc-value erase capture slot exceeded capture arity");
            if (seen[slot_index]) executableInvariant("executable proc-value erase capture slot was duplicated");
            if (erase.capture_slots[slot_index].slot != capture.slot) {
                executableInvariant("executable proc-value erase capture slot differs from erase plan");
            }
            if (capture.value_info != source.captures[slot_index]) {
                executableInvariant("executable proc-value erase capture value differs from callable source");
            }
            if (slot_index >= erase.capture_transforms.len) {
                executableInvariant("executable proc-value erase omitted a capture transform");
            }
            const boundary = self.representation_store.valueTransformBoundary(erase.capture_transforms[slot_index]);
            self.verifyProcValueEraseCaptureBoundary(boundary, callable.emission_plan, erase, capture.slot, capture.value_info);
            const lowered = try self.lowerExpr(capture.expr);
            const raw_value = try self.materializeExprValue(&stmt_ids, lowered);
            const value = try self.applyValueTransformBoundary(&stmt_ids, boundary, raw_value);
            const transformed_ty = try self.lowerSessionExecutableEndpointType(boundary.to_endpoint);
            lowered_captures[slot_index] = try self.output.addValueRefExpr(transformed_ty, value);
            seen[slot_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable proc-value erase plan did not provide every capture slot");
        }

        const capture_value: ?Ast.ExecutableValueRef = if (capture_ty) |ty| blk: {
            const capture_tuple_items: []const Type.TypeId = switch (self.program.types.getType(ty)) {
                .tuple => |items| items,
                else => executableInvariant("executable erased proc-value capture type was not a tuple"),
            };
            const capture_expr = if (lowered_captures.len == 0)
                try self.output.addExpr(ty, self.output.freshValueRef(), .unit)
            else
                try self.output.addExpr(ty, self.output.freshValueRef(), .{ .tuple = try addTupleItemExprSpanForConstruction(self.allocator, self.program, self.output, lowered_captures, capture_tuple_items) });
            const value = try self.materializeExprValue(&stmt_ids, capture_expr);
            break :blk value;
        } else null;

        const result_value = self.output.freshValueRef();
        const final_value = try self.output.addExpr(result_ty, result_value, .{ .packed_erased_fn = .{
            .sig_key = erase.erased_fn_sig_key,
            .code = selected_executable_proc,
            .capture = capture_value,
            .capture_ty = capture_ty,
            .capture_shape = erase.capture_shape_key,
        } });

        if (stmt_ids.items.len == 0) return final_value;
        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_value,
        } });
    }

    fn lowerCallValue(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const func = try self.lowerExpr(call.func);
        const call_site = self.value_store.call_sites.items[@intFromEnum(call.call_site)];
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable call_value call-site requested source type differs from expression");
        }
        const dispatch = call_site.dispatch orelse executableInvariant("executable call_value reached unresolved call-site dispatch");
        const finite_dispatch = switch (dispatch) {
            .call_value_finite => |plan| self.value_store.callValueFiniteDispatchPlan(plan),
            .call_value_erased => |sig_key| return try self.lowerCallValueErased(source_ty, call, func, call.call_site, call_site, sig_key),
            .call_proc => executableInvariant("executable call_value reached procedure call-site dispatch"),
            .pending_local_root_call => executableInvariant("executable call_value reached summary-only pending local root dispatch"),
        };
        const callable_set_key = finite_dispatch.callable_set_key;
        const finite_branches = self.value_store.sliceCallValueFiniteDispatchBranches(finite_dispatch.branches);
        const func_value_info_id = self.input.exprs.items[@intFromEnum(call.func)].value_info;
        const func_value_info = self.value_store.values.items[@intFromEnum(func_value_info_id)];
        const callable = func_value_info.callable orelse executableInvariant("executable call_value callee has no callable metadata");
        const emission = self.representation_store.callableEmissionPlan(callable.emission_plan);
        switch (emission) {
            .pending_proc_value => executableInvariant("executable call_value finite dispatch reached pending callable emission"),
            .finite => |key| if (!repr.callableSetKeyEql(key, callable_set_key)) {
                executableInvariant("executable call_value call-site dispatch differs from callee finite emission");
            },
            .already_erased,
            .erase_proc_value,
            .erase_finite_set,
            => executableInvariant("executable call_value finite dispatch reached erased callee emission"),
        }
        const descriptor = callableSetDescriptorFromSlice(self.callable_set_descriptors, callable_set_key) orelse {
            executableInvariant("executable call_value finite callable set has no descriptor");
        };
        if (descriptor.members.len == 0) executableInvariant("executable call_value finite callable set has no members");

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const arg_values = try self.allocator.alloc(Ast.ExecutableValueRef, arg_items.len);
        defer self.allocator.free(arg_values);
        const stmt_ids = try self.allocator.alloc(Ast.StmtId, arg_items.len + 1);
        defer self.allocator.free(stmt_ids);
        const materialized_func_value = self.output.freshValueRef();
        stmt_ids[0] = try self.output.addStmt(.{ .decl = .{
            .value = materialized_func_value,
            .body = func,
        } });
        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.output.freshValueRef();
            arg_values[i] = value;
            stmt_ids[i + 1] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const requested_source_fn_ty = call_site.requested_source_fn_ty;
        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const call_arg_infos = self.value_store.sliceValueSpan(call_site.args);
        if (call_arg_infos.len != arg_values.len) {
            executableInvariant("executable call_value finite call-site argument metadata differs from call arity");
        }
        if (finite_branches.len != descriptor.members.len) {
            executableInvariant("executable call_value finite branch count differs from callable-set member count");
        }
        const branches = try self.allocator.alloc(Ast.CallableMatchBranch, finite_branches.len);
        defer self.allocator.free(branches);
        for (finite_branches, 0..) |finite_branch, i| {
            const member_ptr = callableSetDescriptorMember(descriptor, finite_branch.member.member_index) orelse {
                executableInvariant("executable call_value finite dispatch branch selected missing descriptor member");
            };
            const member = member_ptr.*;
            if (!repr.callableSetKeyEql(finite_branch.member.callable_set_key, callable_set_key)) {
                executableInvariant("executable call_value finite dispatch branch has wrong callable-set key");
            }
            if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value callable-set member source type differs from call site");
            }
            const target_instance_id = finite_branch.target_instance;
            const executable_proc = self.proc_exec_map.get(target_instance_id) orelse executableInvariant("executable call_value member target was not reserved");
            const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
            const target = target_instance.proc;
            if (!canonical.mirProcedureRefEql(target, member.source_proc)) {
                executableInvariant("executable call_value branch target instance differs from descriptor source procedure");
            }
            if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value member target specialization source type differs from call site");
            }
            const capture_payload_ty = try self.lowerCallableSetMemberPayloadType(callable_set_key, member);
            const capture_payload = if (capture_payload_ty) |payload_ty| try self.output.freshTypedValueRef(payload_ty) else null;
            const member_ref: repr.CallableSetMemberRef = .{
                .callable_set_key = callable_set_key,
                .member_index = member.member,
            };
            const branch_arg_transform_ids = self.value_store.sliceValueTransformBoundarySpan(finite_branch.arg_transforms);
            if (branch_arg_transform_ids.len != arg_values.len) {
                executableInvariant("executable call_value finite branch argument transform count differs from call arity");
            }
            const result_boundary = self.representation_store.valueTransformBoundary(finite_branch.result_transform);
            self.verifyCallableMatchBranchResultBoundary(
                result_boundary,
                call.call_site,
                member_ref,
                target_instance_id,
                target_instance,
                call_site.result,
            );
            const lowered_branch = try self.lowerCallableMatchBranchBody(
                target,
                target_instance,
                target_instance_id,
                executable_proc,
                arg_values,
                branch_arg_transform_ids,
                call_arg_infos,
                call.call_site,
                member_ref,
                capture_payload,
                capture_payload_ty,
                null,
                result_ty,
                result_boundary,
            );
            branches[i] = .{
                .member = member_ref,
                .source_fn_ty = member.proc_value.source_fn_ty,
                .capture_payload = capture_payload,
                .capture_payload_ty = capture_payload_ty,
                .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
                .executable_proc = executable_proc,
                .arg_transforms = lowered_branch.arg_transforms,
                .direct_args = lowered_branch.direct_args,
                .body = lowered_branch.body,
            };
        }

        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .callable_match = .{
            .callable_set_key = callable_set_key,
            .requested_source_fn_ty = requested_source_fn_ty,
            .callee = materialized_func_value,
            .args = try self.output.addValueRefSpan(arg_values),
            .branches = try self.output.addCallableMatchBranchSpan(branches),
            .result_ty = result_ty,
            .result_value = result_value,
        } });

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids),
            .final_expr = final_call,
        } });
    }

    fn lowerCallValueErased(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
        func: Ast.ExprId,
        call_site_id: repr.CallSiteInfoId,
        call_site: repr.CallSiteInfo,
        sig_key: repr.ErasedFnSigKey,
    ) Allocator.Error!Ast.ExprId {
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable erased call_value call-site requested source type differs from expression");
        }
        if (!repr.canonicalTypeKeyEql(sig_key.source_fn_ty, call_site.requested_source_fn_ty)) {
            executableInvariant("executable erased call_value signature source type differs from call site");
        }
        const func_value_info_id = self.input.exprs.items[@intFromEnum(call.func)].value_info;
        const func_value_info = self.value_store.values.items[@intFromEnum(func_value_info_id)];
        const callable = func_value_info.callable orelse executableInvariant("executable erased call_value callee has no callable metadata");
        switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .pending_proc_value => executableInvariant("executable erased call_value reached pending callable emission"),
            .already_erased => |erased| if (!repr.erasedFnSigKeyEql(erased.sig_key, sig_key)) {
                executableInvariant("executable erased call_value call-site dispatch differs from already-erased callee emission");
            },
            .erase_proc_value => |erase| if (!repr.erasedFnSigKeyEql(erase.erased_fn_sig_key, sig_key)) {
                executableInvariant("executable erased call_value call-site dispatch differs from proc erase emission");
            },
            .erase_finite_set => |erase| if (!repr.erasedFnSigKeyEql(erase.adapter.erased_fn_sig_key, sig_key)) {
                executableInvariant("executable erased call_value call-site dispatch differs from finite-set adapter emission");
            },
            .finite => executableInvariant("executable erased call_value reached finite callee emission"),
        }

        const capture_ty = self.erasedFnCaptureType(self.output.getExpr(func).ty, sig_key);

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const arg_consumer_use_ids = self.value_store.sliceConsumerUsePlanSpan(call_site.arg_consumer_uses);
        if (arg_consumer_use_ids.len != arg_items.len) {
            executableInvariant("executable erased call_value argument consumer-use count differs from call arity");
        }
        const result_transform_id = call_site.result_transform orelse {
            executableInvariant("executable erased call_value has no result transform");
        };
        const result_boundary = self.representation_store.valueTransformBoundary(result_transform_id);
        self.verifyErasedCallRawResultBoundary(result_boundary, call_site_id, sig_key);

        const arg_values = try self.allocator.alloc(Ast.ExecutableValueRef, arg_items.len);
        defer self.allocator.free(arg_values);
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);
        const materialized_func_value = self.output.freshValueRef();
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = materialized_func_value,
            .body = func,
        } }));
        for (arg_items, 0..) |arg, i| {
            self.verifyErasedCallRawArgConsumerUse(arg_consumer_use_ids[i], arg, call_site_id, @intCast(i), sig_key);
            const lowered = try self.lowerExprAtConsumerUse(arg, arg_consumer_use_ids[i]);
            const value = try self.materializeExprValue(&stmt_ids, lowered);
            arg_values[i] = value;
        }

        const raw_result_ty = try self.lowerSessionExecutableEndpointType(result_boundary.from_endpoint);
        const raw_result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(raw_result_ty, raw_result_value, .{ .call_erased = .{
            .func = materialized_func_value,
            .args = try self.output.addValueRefSpan(arg_values),
            .sig_key = sig_key,
            .capture_ty = capture_ty,
        } });
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = raw_result_value,
            .body = final_call,
        } }));
        const result_value = try self.applyValueTransformBoundary(&stmt_ids, result_boundary, raw_result_value);
        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const final_expr = try self.output.addValueRefExpr(result_ty, result_value);

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
    }

    const LoweredCallableMatchBranch = struct {
        arg_transforms: Ast.Span(checked_artifact.ExecutableValueTransformRef),
        direct_args: Ast.Span(Ast.DirectCallArg),
        body: Ast.ExprId,
    };

    fn lowerCallableMatchBranchBody(
        self: *BodyBuilder,
        source_proc: canonical.MirProcedureRef,
        target_instance: repr.ProcRepresentationInstance,
        target_instance_id: repr.ProcRepresentationInstanceId,
        executable_proc: Ast.ExecutableProcId,
        arg_values: []const Ast.ExecutableValueRef,
        branch_arg_transform_ids: ?[]const repr.ValueTransformBoundaryId,
        call_arg_infos: ?[]const repr.ValueInfoId,
        branch_call_site_id: ?repr.CallSiteInfoId,
        branch_member_ref: ?repr.CallableSetMemberRef,
        capture_payload: ?Ast.ExecutableValueRef,
        capture_payload_ty: ?Type.TypeId,
        branch_capture_transform_ids: ?[]const repr.ValueTransformBoundaryId,
        result_ty: Ast.TypeId,
        result_boundary: ?repr.ValueTransformBoundary,
    ) Allocator.Error!LoweredCallableMatchBranch {
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        const capture_arg_len: usize = if (capture_payload == null) 0 else 1;
        const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_values.len + capture_arg_len);
        defer self.allocator.free(direct_args);

        const arg_transform_count: usize = if (branch_arg_transform_ids) |boundary_ids| boundary_ids.len else 0;
        const arg_transform_refs = try self.allocator.alloc(checked_artifact.ExecutableValueTransformRef, arg_transform_count);
        defer self.allocator.free(arg_transform_refs);

        if (branch_arg_transform_ids) |boundary_ids| {
            if (boundary_ids.len != arg_values.len) {
                executableInvariant("executable callable_match branch argument transform count differs from call arity");
            }
            if (call_arg_infos) |infos| {
                if (infos.len != arg_values.len) {
                    executableInvariant("executable callable_match branch call argument metadata differs from call arity");
                }
                for (arg_values, boundary_ids, infos, 0..) |arg_value, boundary_id, source_arg_info, arg_i| {
                    const boundary = self.representation_store.valueTransformBoundary(boundary_id);
                    self.verifyCallableMatchBranchArgBoundary(
                        boundary,
                        branch_call_site_id orelse executableInvariant("executable callable_match branch argument transforms require call-site id"),
                        branch_member_ref orelse executableInvariant("executable callable_match branch argument transforms require member ref"),
                        source_arg_info,
                        target_instance_id,
                        target_instance,
                        @intCast(arg_i),
                    );
                    arg_transform_refs[arg_i] = boundary.transform;
                    direct_args[arg_i] = .{
                        .value = try self.applyValueTransformBoundary(&stmt_ids, boundary, arg_value),
                    };
                }
            } else {
                if (branch_call_site_id != null) {
                    executableInvariant("executable callable_match adapter branch argument transforms received unexpected call-site id");
                }
                const member_ref = branch_member_ref orelse executableInvariant("executable callable_match adapter branch argument transforms require member ref");
                for (arg_values, boundary_ids, 0..) |arg_value, boundary_id, arg_i| {
                    const boundary = self.representation_store.valueTransformBoundary(boundary_id);
                    self.verifyErasedFiniteAdapterBranchArgBoundary(
                        boundary,
                        member_ref,
                        target_instance_id,
                        target_instance,
                        @intCast(arg_i),
                    );
                    arg_transform_refs[arg_i] = boundary.transform;
                    direct_args[arg_i] = .{
                        .value = try self.applyValueTransformBoundary(&stmt_ids, boundary, arg_value),
                    };
                }
            }
        } else {
            if (call_arg_infos != null) {
                executableInvariant("executable callable_match raw branch arguments received unexpected call argument metadata");
            }
            for (arg_values, 0..) |arg_value, arg_i| {
                direct_args[arg_i] = .{ .value = arg_value };
            }
        }

        if (capture_payload) |payload| {
            if (branch_capture_transform_ids) |capture_boundary_ids| {
                direct_args[arg_values.len] = .{
                    .value = try self.lowerErasedFiniteAdapterBranchCaptureArg(
                        &stmt_ids,
                        payload,
                        capture_payload_ty orelse executableInvariant("executable erased finite adapter branch capture transform has no payload type"),
                        capture_boundary_ids,
                        branch_member_ref orelse executableInvariant("executable erased finite adapter branch capture transforms require member ref"),
                        target_instance_id,
                        target_instance,
                    ),
                };
            } else {
                direct_args[arg_values.len] = .{ .value = payload };
            }
        } else if (branch_capture_transform_ids) |capture_boundary_ids| {
            if (capture_boundary_ids.len != 0) {
                executableInvariant("executable erased finite adapter branch has capture transforms without capture payload");
            }
        }
        const direct_args_span = try self.output.addDirectCallArgSpan(direct_args);
        const arg_transforms_span = try self.output.addExecutableValueTransformRefSpan(arg_transform_refs);

        const raw_result_ty = if (result_boundary) |boundary|
            try self.lowerSessionExecutableEndpointType(boundary.from_endpoint)
        else
            result_ty;
        const raw_result_value = self.output.freshValueRef();
        const direct_call = try self.output.addExpr(raw_result_ty, raw_result_value, .{ .call_direct = .{
            .source = source_proc.proc,
            .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
            .executable_proc = executable_proc,
            .direct_args = direct_args_span,
        } });
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = raw_result_value,
            .body = direct_call,
        } }));

        const result_value = if (result_boundary) |boundary|
            try self.applyValueTransformBoundary(&stmt_ids, boundary, raw_result_value)
        else
            raw_result_value;
        const final_expr = try self.output.addValueRefExpr(result_ty, result_value);
        const body = try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
        return .{
            .arg_transforms = arg_transforms_span,
            .direct_args = direct_args_span,
            .body = body,
        };
    }

    fn lowerErasedFiniteAdapterBranchCaptureArg(
        self: *BodyBuilder,
        stmt_ids: *std.ArrayList(Ast.StmtId),
        capture_payload: Ast.ExecutableValueRef,
        capture_payload_ty: Type.TypeId,
        capture_boundary_ids: []const repr.ValueTransformBoundaryId,
        member_ref: repr.CallableSetMemberRef,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const source_items = switch (self.program.types.getType(capture_payload_ty)) {
            .tuple => |items| items,
            else => executableInvariant("executable erased finite adapter branch capture payload type is not a tuple"),
        };
        if (source_items.len != capture_boundary_ids.len) {
            executableInvariant("executable erased finite adapter branch capture transform count differs from payload arity");
        }
        const payload_expr = try self.output.addValueRefExpr(capture_payload_ty, capture_payload);
        const output_items = try self.allocator.alloc(Ast.ExprId, capture_boundary_ids.len);
        defer self.allocator.free(output_items);
        const target_item_tys = try self.allocator.alloc(Type.TypeId, capture_boundary_ids.len);
        defer self.allocator.free(target_item_tys);

        for (capture_boundary_ids, 0..) |boundary_id, slot_i| {
            const slot: u32 = @intCast(slot_i);
            const boundary = self.representation_store.valueTransformBoundary(boundary_id);
            self.verifyErasedFiniteAdapterBranchCaptureBoundary(
                boundary,
                member_ref,
                target_instance_id,
                target_instance,
                slot,
            );
            const source_ty = try self.lowerSessionExecutableEndpointType(boundary.from_endpoint);
            if (source_ty != source_items[slot_i]) {
                executableInvariant("executable erased finite adapter branch capture source type differs from member payload slot");
            }
            const access_value = self.output.freshValueRef();
            const access_expr = try self.output.addExpr(source_ty, access_value, .{ .tuple_access = .{
                .tuple = payload_expr,
                .elem_index = slot,
            } });
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = access_value,
                .body = access_expr,
            } }));
            const transformed = try self.applyValueTransformBoundary(stmt_ids, boundary, access_value);
            const target_ty = try self.lowerSessionExecutableEndpointType(boundary.to_endpoint);
            target_item_tys[slot_i] = target_ty;
            output_items[slot_i] = try self.output.addValueRefExpr(target_ty, transformed);
        }

        const tuple_tys = if (target_item_tys.len == 0)
            &.{}
        else
            try self.allocator.dupe(Type.TypeId, target_item_tys);
        errdefer if (tuple_tys.len > 0) self.allocator.free(tuple_tys);
        const capture_arg_ty = try self.program.types.addType(.{ .tuple = tuple_tys });
        const capture_arg_value = self.output.freshValueRef();
        const capture_arg_expr = try self.output.addExpr(capture_arg_ty, capture_arg_value, .{
            .tuple = try addTupleItemExprSpanForConstruction(self.allocator, self.program, self.output, output_items, target_item_tys),
        });
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = capture_arg_value,
            .body = capture_arg_expr,
        } }));
        return capture_arg_value;
    }

    fn verifyCallableMatchBranchArgBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        call_site_id: repr.CallSiteInfoId,
        member_ref: repr.CallableSetMemberRef,
        source_arg: repr.ValueInfoId,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
        index: u32,
    ) void {
        _ = self;
        const kind = switch (boundary.kind) {
            .callable_match_branch_arg => |branch_arg| branch_arg,
            else => executableInvariant("executable callable_match argument boundary has non-branch-arg kind"),
        };
        if (kind.arg_index != index) {
            executableInvariant("executable callable_match argument boundary index differs from branch argument");
        }
        if (kind.call != call_site_id or
            !repr.callableSetKeyEql(kind.member.callable_set_key, member_ref.callable_set_key) or
            kind.member.member_index != member_ref.member_index)
        {
            executableInvariant("executable callable_match argument boundary points at a different branch");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .local_value => |value| value,
            else => executableInvariant("executable callable_match argument boundary source is not local_value"),
        };
        if (from != source_arg) {
            executableInvariant("executable callable_match argument boundary source differs from call argument");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .procedure_param => |param| param,
            else => executableInvariant("executable callable_match argument boundary target is not procedure_param"),
        };
        if (to.instance != target_instance_id or to.index != index) {
            executableInvariant("executable callable_match argument boundary target differs from branch target");
        }
        const arg_index: usize = @intCast(index);
        if (arg_index >= target_instance.executable_specialization_key.exec_arg_tys.len) {
            executableInvariant("executable callable_match argument boundary index exceeds branch target arity");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
            executableInvariant("executable callable_match argument boundary target key differs from branch specialization");
        }
    }

    fn verifyCallableMatchBranchResultBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        call_site_id: repr.CallSiteInfoId,
        member_ref: repr.CallableSetMemberRef,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
        result: repr.ValueInfoId,
    ) void {
        const branch = switch (boundary.kind) {
            .callable_match_branch_result => |branch| branch,
            else => executableInvariant("executable callable_match result boundary has non-branch kind"),
        };
        if (branch.call != call_site_id or
            !repr.callableSetKeyEql(branch.member.callable_set_key, member_ref.callable_set_key) or
            branch.member.member_index != member_ref.member_index)
        {
            executableInvariant("executable callable_match result boundary points at a different branch");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .procedure_return => |proc| proc,
            else => executableInvariant("executable callable_match result boundary source is not procedure_return"),
        };
        if (from != target_instance_id) {
            executableInvariant("executable callable_match result boundary source differs from branch target");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .local_value => |value| value,
            else => executableInvariant("executable callable_match result boundary target is not local_value"),
        };
        if (to != result) {
            executableInvariant("executable callable_match result boundary target differs from call result");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.from_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_ret_ty)) {
            executableInvariant("executable callable_match result boundary source key differs from branch specialization");
        }
        _ = self;
    }

    fn verifyErasedFiniteAdapterBranchArgBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        member_ref: repr.CallableSetMemberRef,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
        index: u32,
    ) void {
        _ = self;
        const kind = switch (boundary.kind) {
            .erased_finite_adapter_arg => |branch_arg| branch_arg,
            else => executableInvariant("executable erased finite adapter argument boundary has non-adapter-arg kind"),
        };
        if (!repr.callableSetKeyEql(kind.member.callable_set_key, member_ref.callable_set_key) or
            kind.member.member_index != member_ref.member_index or
            kind.index != index)
        {
            executableInvariant("executable erased finite adapter argument boundary points at a different branch");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .erased_finite_adapter_arg => |arg| arg,
            else => executableInvariant("executable erased finite adapter argument boundary source has wrong owner"),
        };
        if (!repr.erasedAdapterKeyEql(from.adapter, kind.adapter) or
            !repr.callableSetKeyEql(from.member.callable_set_key, kind.member.callable_set_key) or
            from.member.member_index != kind.member.member_index or
            from.index != kind.index)
        {
            executableInvariant("executable erased finite adapter argument boundary source differs from boundary owner");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .procedure_param => |param| param,
            else => executableInvariant("executable erased finite adapter argument boundary target is not procedure_param"),
        };
        if (to.instance != target_instance_id or to.index != index) {
            executableInvariant("executable erased finite adapter argument boundary target differs from branch target");
        }
        const arg_index: usize = @intCast(index);
        if (arg_index >= target_instance.executable_specialization_key.exec_arg_tys.len) {
            executableInvariant("executable erased finite adapter argument boundary index exceeds branch target arity");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
            executableInvariant("executable erased finite adapter argument boundary target key differs from branch specialization");
        }
    }

    fn verifyErasedFiniteAdapterBranchCaptureBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        member_ref: repr.CallableSetMemberRef,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
        slot: u32,
    ) void {
        const kind = switch (boundary.kind) {
            .erased_finite_adapter_capture => |capture| capture,
            else => executableInvariant("executable erased finite adapter capture boundary has non-adapter-capture kind"),
        };
        if (!repr.callableSetKeyEql(kind.member.callable_set_key, member_ref.callable_set_key) or
            kind.member.member_index != member_ref.member_index or
            kind.slot != slot)
        {
            executableInvariant("executable erased finite adapter capture boundary points at a different branch");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .erased_finite_adapter_capture => |capture| capture,
            else => executableInvariant("executable erased finite adapter capture boundary source has wrong owner"),
        };
        if (!repr.erasedAdapterKeyEql(from.adapter, kind.adapter) or
            !repr.callableSetKeyEql(from.member.callable_set_key, kind.member.callable_set_key) or
            from.member.member_index != kind.member.member_index or
            from.slot != kind.slot)
        {
            executableInvariant("executable erased finite adapter capture boundary source differs from boundary owner");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .procedure_capture => |capture| capture,
            else => executableInvariant("executable erased finite adapter capture boundary target is not procedure_capture"),
        };
        if (to.instance != target_instance_id or to.slot != slot) {
            executableInvariant("executable erased finite adapter capture boundary target differs from branch target");
        }
        const target_value_store = self.value_stores[@intFromEnum(target_instance.value_store)];
        const target_captures = target_value_store.sliceValueSpan(target_instance.public_roots.captures);
        const slot_index: usize = @intCast(slot);
        if (slot_index >= target_captures.len) {
            executableInvariant("executable erased finite adapter capture boundary slot exceeds branch target capture count");
        }
    }

    fn verifyErasedFiniteAdapterBranchResultBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        adapter: repr.ErasedAdapterKey,
        member_ref: repr.CallableSetMemberRef,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
    ) void {
        _ = self;
        const kind = switch (boundary.kind) {
            .erased_finite_adapter_result => |branch_result| branch_result,
            else => executableInvariant("executable erased finite adapter result boundary has non-adapter-result kind"),
        };
        if (!repr.erasedAdapterKeyEql(kind.adapter, adapter) or
            !repr.callableSetKeyEql(kind.member.callable_set_key, member_ref.callable_set_key) or
            kind.member.member_index != member_ref.member_index)
        {
            executableInvariant("executable erased finite adapter result boundary points at a different branch");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .procedure_return => |proc| proc,
            else => executableInvariant("executable erased finite adapter result boundary source is not procedure_return"),
        };
        if (from != target_instance_id) {
            executableInvariant("executable erased finite adapter result boundary source differs from branch target");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .erased_finite_adapter_result => |result| result,
            else => executableInvariant("executable erased finite adapter result boundary target has wrong owner"),
        };
        if (!repr.erasedAdapterKeyEql(to.adapter, adapter) or
            !repr.callableSetKeyEql(to.member.callable_set_key, member_ref.callable_set_key) or
            to.member.member_index != member_ref.member_index)
        {
            executableInvariant("executable erased finite adapter result boundary target differs from branch");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.from_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_ret_ty)) {
            executableInvariant("executable erased finite adapter result boundary source key differs from branch specialization");
        }
    }

    fn verifyCallableConstructionCaptureBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        construction_id: repr.CallableSetConstructionPlanId,
        construction: repr.CallableSetConstructionPlan,
        member: *const repr.CanonicalCallableSetMember,
        slot: u32,
        source_capture: repr.ValueInfoId,
    ) void {
        const capture_id = switch (boundary.kind) {
            .capture_value => |id| id,
            else => executableInvariant("executable callable construction capture transform has non-capture kind"),
        };
        const capture_info = self.representation_store.captureBoundary(capture_id);
        switch (capture_info.owner) {
            .callable_set_construction => |owner| {
                if (owner.construction != construction_id) {
                    executableInvariant("executable callable construction capture boundary points at a different construction plan");
                }
                if (!repr.callableSetKeyEql(owner.selected_member.callable_set_key, construction.callable_set_key) or
                    owner.selected_member.member_index != construction.selected_member)
                {
                    executableInvariant("executable callable construction capture boundary points at a different member");
                }
            },
            else => executableInvariant("executable callable construction capture boundary has wrong owner"),
        }
        if (capture_info.slot != slot) {
            executableInvariant("executable callable construction capture boundary slot differs from capture arg");
        }
        if (capture_info.source_capture_value != source_capture) {
            executableInvariant("executable callable construction capture boundary source differs from capture arg");
        }
        if (capture_info.target_instance != member.target_instance) {
            executableInvariant("executable callable construction capture boundary target instance differs from descriptor member");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .local_value => |value| value,
            else => executableInvariant("executable callable construction capture boundary source is not local_value"),
        };
        if (from != source_capture) {
            executableInvariant("executable callable construction capture boundary source endpoint differs from capture arg");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .procedure_capture => |capture| capture,
            else => executableInvariant("executable callable construction capture boundary target is not procedure_capture"),
        };
        if (to.instance != capture_info.target_instance or to.slot != slot) {
            executableInvariant("executable callable construction capture boundary target endpoint differs from capture metadata");
        }
        const slot_index: usize = @intCast(slot);
        if (slot_index >= member.capture_slots.len) {
            executableInvariant("executable callable construction capture boundary slot exceeds member schema");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, member.capture_slots[slot_index].exec_value_ty)) {
            executableInvariant("executable callable construction capture boundary target key differs from member schema");
        }
    }

    fn verifyProcValueEraseCaptureBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        emission_plan: repr.CallableValueEmissionPlanId,
        erase: repr.ProcValueErasePlan,
        slot: u32,
        source_capture: repr.ValueInfoId,
    ) void {
        const capture_id = switch (boundary.kind) {
            .capture_value => |id| id,
            else => executableInvariant("executable proc-value erase capture transform has non-capture kind"),
        };
        const capture_info = self.representation_store.captureBoundary(capture_id);
        switch (capture_info.owner) {
            .proc_value_erase => |owner| {
                if (owner.emission_plan != emission_plan or owner.source_value != erase.source_value) {
                    executableInvariant("executable proc-value erase capture boundary points at a different emission plan");
                }
                if (!canonical.procedureCallableRefEql(owner.proc_value, erase.proc_value)) {
                    executableInvariant("executable proc-value erase capture boundary procedure differs from erase plan");
                }
                if (!repr.erasedFnSigKeyEql(owner.erased_fn_sig_key, erase.erased_fn_sig_key)) {
                    executableInvariant("executable proc-value erase capture boundary signature differs from erase plan");
                }
            },
            else => executableInvariant("executable proc-value erase capture boundary has wrong owner"),
        }
        if (capture_info.target_instance != erase.target_instance) {
            executableInvariant("executable proc-value erase capture boundary target instance differs from erase plan");
        }
        if (capture_info.slot != slot) {
            executableInvariant("executable proc-value erase capture boundary slot differs from capture arg");
        }
        if (capture_info.source_capture_value != source_capture) {
            executableInvariant("executable proc-value erase capture boundary source differs from capture arg");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .local_value => |value| value,
            else => executableInvariant("executable proc-value erase capture boundary source is not local_value"),
        };
        if (from != source_capture) {
            executableInvariant("executable proc-value erase capture boundary source endpoint differs from capture arg");
        }
        const to = switch (boundary.to_endpoint.owner) {
            .procedure_capture => |capture| capture,
            else => executableInvariant("executable proc-value erase capture boundary target is not procedure_capture"),
        };
        if (to.instance != erase.target_instance or to.slot != slot) {
            executableInvariant("executable proc-value erase capture boundary target endpoint differs from capture metadata");
        }
        const slot_index: usize = @intCast(slot);
        if (slot_index >= erase.capture_slots.len) {
            executableInvariant("executable proc-value erase capture boundary slot exceeds erase plan schema");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, erase.capture_slots[slot_index].exec_value_ty)) {
            executableInvariant("executable proc-value erase capture boundary target key differs from erase plan schema");
        }
    }

    fn verifyErasedCallRawArgConsumerUse(
        self: *BodyBuilder,
        use_id: repr.ConsumerUsePlanId,
        arg_expr: LambdaSolved.Ast.ExprId,
        call_site_id: repr.CallSiteInfoId,
        index: u32,
        sig_key: repr.ErasedFnSigKey,
    ) void {
        const plan = self.representation_store.consumerUsePlan(use_id);
        const owner = switch (plan.owner) {
            .call_arg => |call_arg| call_arg,
            else => executableInvariant("executable erased call argument consumer-use has non-call-arg owner"),
        };
        if (owner.call != call_site_id or owner.arg_index != index) {
            executableInvariant("executable erased call argument consumer-use owner differs from call site");
        }
        const source_expr = self.input.exprs.items[@intFromEnum(arg_expr)];
        if (source_expr.value_info != plan.child_value) {
            executableInvariant("executable erased call argument consumer-use child value differs from argument expression");
        }
        const abi = self.representation_store.erased_fn_abis.abiFor(sig_key.abi) orelse {
            executableInvariant("executable erased call argument transform references missing ABI payload");
        };
        if (index >= abi.arg_exec_keys.len) executableInvariant("executable erased call argument transform index exceeds ABI arity");
        const to = switch (plan.expected_endpoint.owner) {
            .call_raw_arg => |raw| raw,
            else => executableInvariant("executable erased call argument consumer-use endpoint is not call_raw_arg"),
        };
        if (to.call != call_site_id or to.index != index) {
            executableInvariant("executable erased call argument consumer-use endpoint differs from call site");
        }
        if (!repr.canonicalExecValueTypeKeyEql(plan.expected_endpoint.exec_ty.key, abi.arg_exec_keys[index])) {
            executableInvariant("executable erased call argument consumer-use endpoint key differs from ABI payload");
        }
    }

    fn verifyErasedCallRawResultBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        call_site_id: repr.CallSiteInfoId,
        sig_key: repr.ErasedFnSigKey,
    ) void {
        const abi = self.representation_store.erased_fn_abis.abiFor(sig_key.abi) orelse {
            executableInvariant("executable erased call result transform references missing ABI payload");
        };
        const from = switch (boundary.from_endpoint.owner) {
            .call_raw_result => |raw| raw,
            else => executableInvariant("executable erased call result transform source is not call_raw_result"),
        };
        if (from != call_site_id) {
            executableInvariant("executable erased call result transform source differs from call site");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.from_endpoint.exec_ty.key, abi.ret_exec_key)) {
            executableInvariant("executable erased call result endpoint key differs from ABI payload");
        }
    }

    fn applyValueTransformBoundary(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        boundary: repr.ValueTransformBoundary,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        return try self.applyExecutableValueTransformRef(stmts, boundary.transform, value);
    }

    fn applyExecutableValueTransformRef(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        transform: checked_artifact.ExecutableValueTransformRef,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        return switch (transform) {
            .session => |id| try self.applySessionExecutableValueTransform(stmts, self.representation_store.sessionExecutableValueTransform(id), value),
            .published => |published| try self.applyPublishedValueTransformRef(stmts, published, value),
        };
    }

    fn applyPublishedValueTransformRef(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        transform_ref: checked_artifact.PublishedExecutableValueTransformRef,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const context = resolvePublishedTransformContext(self.program, transform_ref);
        var published_types = PublishedTypeLowerer.init(
            self.allocator,
            context.executable_type_payloads,
            context.materialization.canonical_names,
            &self.program.canonical_names,
            &self.program.types,
            &self.program.row_shapes,
            &self.program.lowered_session_types_by_key,
        );
        defer published_types.deinit();
        return try applyPublishedExecutableValueTransformRef(
            self.program,
            context.materialization,
            context.artifact,
            &published_types,
            context.executable_value_transforms,
            stmts,
            transform_ref,
            value,
        );
    }

    fn applySessionExecutableValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        self.verifySessionExecutableValueTransformScope(plan);
        return switch (plan.op) {
            .identity => blk: {
                if (!repr.canonicalExecValueTypeKeyEql(plan.from.exec_ty.key, plan.to.exec_ty.key)) {
                    executableInvariant("executable session identity transform changes representation");
                }
                break :blk value;
            },
            .structural_bridge => |structural| blk: {
                const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
                const bridge = try self.lowerSessionExecutableValueTransformAsBridge(plan, structural);
                const bridged_value = self.output.freshValueRef();
                const bridged_expr = try self.output.addExpr(to_ty, bridged_value, .{ .bridge = .{
                    .bridge = bridge,
                    .value = value,
                } });
                try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                    .value = bridged_value,
                    .body = bridged_expr,
                } }));
                break :blk bridged_value;
            },
            .record => |fields| try self.applySessionRecordValueTransform(stmts, plan, fields, value),
            .tuple => |items| try self.applySessionTupleValueTransform(stmts, plan, items, value),
            .tag_union => |cases| try self.applySessionTagUnionValueTransform(stmts, plan, cases, value),
            .nominal => |nominal| try self.applySessionNominalValueTransform(stmts, plan, nominal, value),
            .list => |list| try self.applySessionListValueTransform(stmts, plan, list.elem, value),
            .box_payload => |box| try self.applySessionBoxValueTransform(stmts, plan, box, value),
            .callable_to_erased => |callable| try self.applySessionCallableToErasedTransform(stmts, plan, callable, value),
            .already_erased_callable => |erased| blk: {
                const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
                const from_erased_ty = erasedFnType(self.program, from_ty);
                if (!repr.erasedFnSigKeyEql(from_erased_ty.sig_key, erased.sig_key)) {
                    executableInvariant("executable already-erased session transform source signature differs from plan");
                }
                const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
                const erased_ty = erasedFnType(self.program, to_ty);
                if (!repr.erasedFnSigKeyEql(erased_ty.sig_key, erased.sig_key)) {
                    executableInvariant("executable already-erased session transform target signature differs from plan");
                }
                break :blk value;
            },
        };
    }

    fn verifySessionExecutableValueTransformScope(
        self: *BodyBuilder,
        plan: repr.SessionExecutableValueTransformPlan,
    ) void {
        if (plan.scope) |scope_id| {
            const scope = self.representation_store.transformEndpointScope(scope_id);
            if (!sessionExecutableValueEndpointEql(scope.root_from, plan.from) and
                !sessionEndpointIsTransformChildForScope(plan.from, scope_id, .from))
            {
                executableInvariant("executable session transform source endpoint does not belong to its transform scope");
            }
            if (!sessionExecutableValueEndpointEql(scope.root_to, plan.to) and
                !sessionEndpointIsTransformChildForScope(plan.to, scope_id, .to))
            {
                executableInvariant("executable session transform target endpoint does not belong to its transform scope");
            }
            return;
        }

        if (sessionEndpointOwnerIsTransformChild(plan.from.owner) or
            sessionEndpointOwnerIsTransformChild(plan.to.owner))
        {
            executableInvariant("executable session transform child endpoint has no transform scope");
        }
    }

    fn applySessionRecordValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        fields: []const repr.SessionValueTransformRecordField,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        const source = switch (self.program.types.getType(from_ty)) {
            .record => |record| record,
            else => executableInvariant("session record value transform source endpoint is not a record"),
        };
        const target = switch (self.program.types.getType(to_ty)) {
            .record => |record| record,
            else => executableInvariant("session record value transform target endpoint is not a record"),
        };
        if (fields.len != target.fields.len) {
            executableInvariant("session record value transform field count differs from target record");
        }

        const source_expr = try self.output.addValueRefExpr(from_ty, value);
        const seen = try self.allocator.alloc(bool, fields.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        const output_fields = try self.allocator.alloc(Ast.RecordFieldExpr, target.fields.len);
        defer self.allocator.free(output_fields);
        for (target.fields, 0..) |target_field, target_i| {
            const field_plan = findSessionValueTransformRecordField(fields, target_field.field, seen) orelse {
                executableInvariant("session record value transform omitted a target field");
            };
            const source_field = recordFieldForId(self.program, source, field_plan.field);
            const access_value = self.output.freshValueRef();
            const access_expr = try self.output.addExpr(source_field.ty, access_value, .{ .access = .{
                .record = source_expr,
                .field = source_field.field,
            } });
            try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = access_value,
                .body = access_expr,
            } }));
            const transformed = try self.applyExecutableValueTransformRef(stmts, field_plan.transform, access_value);
            output_fields[target_i] = .{
                .field = target_field.field,
                .expr = try self.output.addValueRefExpr(target_field.ty, transformed),
                .ty = target_field.ty,
                .value = transformed,
                .bridge = try self.constructionSlotBridge(target_field.ty, target_field.ty),
            };
        }
        verifyAllSeen(seen, "session record value transform had an extra field transform");

        const record_value = self.output.freshValueRef();
        const record_expr = try self.output.addExpr(to_ty, record_value, .{ .record = .{
            .shape = target.shape,
            .fields = try self.output.addRecordFieldExprSpan(output_fields),
        } });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = record_value,
            .body = record_expr,
        } }));
        return record_value;
    }

    fn applySessionTupleValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        items: []const repr.SessionValueTransformTupleElem,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        const source = switch (self.program.types.getType(from_ty)) {
            .tuple => |tuple| tuple,
            else => executableInvariant("session tuple value transform source endpoint is not a tuple"),
        };
        const target = switch (self.program.types.getType(to_ty)) {
            .tuple => |tuple| tuple,
            else => executableInvariant("session tuple value transform target endpoint is not a tuple"),
        };
        if (items.len != target.len or source.len != target.len) {
            executableInvariant("session tuple value transform arity differs from endpoint tuple");
        }

        const tuple_expr = try self.output.addValueRefExpr(from_ty, value);
        const seen = try self.allocator.alloc(bool, items.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        const output_items = try self.allocator.alloc(Ast.ExprId, target.len);
        defer self.allocator.free(output_items);
        for (target, 0..) |target_item_ty, i| {
            const item_plan = findSessionValueTransformTupleElem(items, @intCast(i), seen) orelse {
                executableInvariant("session tuple value transform omitted a target element");
            };
            const access_value = self.output.freshValueRef();
            const access_expr = try self.output.addExpr(source[i], access_value, .{ .tuple_access = .{
                .tuple = tuple_expr,
                .elem_index = @intCast(i),
            } });
            try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = access_value,
                .body = access_expr,
            } }));
            const transformed = try self.applyExecutableValueTransformRef(stmts, item_plan.transform, access_value);
            output_items[i] = try self.output.addValueRefExpr(target_item_ty, transformed);
        }
        verifyAllSeen(seen, "session tuple value transform had an extra element transform");

        const tuple_value = self.output.freshValueRef();
        const result_expr = try self.output.addExpr(to_ty, tuple_value, .{ .tuple = try addTupleItemExprSpanForConstruction(self.allocator, self.program, self.output, output_items, target) });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = tuple_value,
            .body = result_expr,
        } }));
        return tuple_value;
    }

    fn applySessionNominalValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        nominal: anytype,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        const source = switch (self.program.types.getType(from_ty)) {
            .nominal => |source| source,
            else => executableInvariant("session nominal value transform source endpoint is not nominal"),
        };
        const target = switch (self.program.types.getType(to_ty)) {
            .nominal => |target| target,
            else => executableInvariant("session nominal value transform target endpoint is not nominal"),
        };
        if (!nominalTypeKeyEql(target.nominal, nominal.nominal)) {
            executableInvariant("session nominal value transform target nominal differs from plan");
        }
        if (!repr.canonicalTypeKeyEql(target.source_ty, nominal.source_ty)) {
            executableInvariant("session nominal value transform target source type differs from plan");
        }

        const source_expr = try self.output.addValueRefExpr(from_ty, value);
        const backing_value = self.output.freshValueRef();
        const backing_expr = try self.output.addExpr(source.backing, backing_value, .{ .nominal_reinterpret = source_expr });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = backing_value,
            .body = backing_expr,
        } }));

        const transformed_backing = try self.applyExecutableValueTransformRef(stmts, nominal.backing, backing_value);
        const transformed_expr = try self.output.addValueRefExpr(target.backing, transformed_backing);
        const nominal_value = self.output.freshValueRef();
        const nominal_expr = try self.output.addExpr(to_ty, nominal_value, .{ .nominal_reinterpret = transformed_expr });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = nominal_value,
            .body = nominal_expr,
        } }));
        return nominal_value;
    }

    fn applySessionListValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        elem_transform: checked_artifact.ExecutableValueTransformRef,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        const source_elem_ty = listElementTypeForTransform(self.program, from_ty, "source");
        const target_elem_ty = listElementTypeForTransform(self.program, to_ty, "target");

        const source_elem = try self.output.freshTypedValueRef(source_elem_ty);
        var body_stmts = std.ArrayList(Ast.StmtId).empty;
        defer body_stmts.deinit(self.allocator);

        const transformed_elem = try self.applyExecutableValueTransformRef(&body_stmts, elem_transform, source_elem);
        const transformed_expr = try self.output.addValueRefExpr(target_elem_ty, transformed_elem);
        const body_expr = if (body_stmts.items.len == 0)
            transformed_expr
        else
            try self.output.addExpr(target_elem_ty, transformed_elem, .{ .block = .{
                .stmts = try self.output.addStmtSpan(body_stmts.items),
                .final_expr = transformed_expr,
            } });

        const list_value = self.output.freshValueRef();
        const list_expr = try self.output.addExpr(to_ty, list_value, .{ .value_transform_list = .{
            .source = value,
            .source_elem = source_elem,
            .source_elem_ty = source_elem_ty,
            .target_elem_ty = target_elem_ty,
            .body = body_expr,
        } });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = list_value,
            .body = list_expr,
        } }));
        return list_value;
    }

    fn applySessionBoxValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        box: repr.SessionBoxPayloadTransformPlan,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        switch (box.kind) {
            .payload_to_box => {
                _ = boxPayloadType(self.program, to_ty);
                const transformed = try self.applyExecutableValueTransformRef(stmts, box.payload, value);
                return try boxTransformedPayload(self.program, stmts, to_ty, transformed);
            },
            .box_to_payload => {
                _ = boxPayloadType(self.program, from_ty);
                const unboxed = try unboxPayloadForTransform(self.program, stmts, from_ty, value);
                return try self.applyExecutableValueTransformRef(stmts, box.payload, unboxed);
            },
            .box_to_box => {
                _ = boxPayloadType(self.program, from_ty);
                _ = boxPayloadType(self.program, to_ty);
                const unboxed = try unboxPayloadForTransform(self.program, stmts, from_ty, value);
                const transformed = try self.applyExecutableValueTransformRef(stmts, box.payload, unboxed);
                return try boxTransformedPayload(self.program, stmts, to_ty, transformed);
            },
        }
    }

    fn applySessionTagUnionValueTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        cases: []const repr.SessionValueTransformTagCase,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        const source = switch (self.program.types.getType(from_ty)) {
            .tag_union => |tag_union| tag_union,
            else => executableInvariant("session tag-union value transform source endpoint is not a tag union"),
        };
        const target = switch (self.program.types.getType(to_ty)) {
            .tag_union => |tag_union| tag_union,
            else => executableInvariant("session tag-union value transform target endpoint is not a tag union"),
        };
        if (cases.len != source.tags.len) {
            executableInvariant("session tag-union value transform case count differs from source tag-union arity");
        }

        const seen_cases = try self.allocator.alloc(bool, cases.len);
        defer self.allocator.free(seen_cases);
        @memset(seen_cases, false);

        const branches = try self.allocator.alloc(Ast.ValueTransformTagBranch, source.tags.len);
        defer self.allocator.free(branches);
        for (source.tags, 0..) |source_tag, source_i| {
            const case = findSessionValueTransformTagCase(cases, source_tag.tag, seen_cases) orelse {
                executableInvariant("session tag-union value transform omitted a source tag case");
            };
            const target_tag = tagTypeForId(self.program, target, case.target_tag);

            var branch_stmts = std.ArrayList(Ast.StmtId).empty;
            defer branch_stmts.deinit(self.allocator);
            const branch_body = try self.sessionTagUnionValueTransformBranchBody(
                &branch_stmts,
                from_ty,
                to_ty,
                source_tag,
                target,
                target_tag,
                case,
                value,
            );

            branches[source_i] = .{
                .discriminant = @intCast(self.program.row_shapes.tag(source_tag.tag).logical_index),
                .body = branch_body,
            };
        }
        verifyAllSeen(seen_cases, "session tag-union value transform had an extra source tag case");

        const transformed_value = self.output.freshValueRef();
        const transformed_expr = try self.output.addExpr(to_ty, transformed_value, .{ .value_transform_tag_union = .{
            .source = value,
            .source_union_shape = source.shape,
            .branches = try self.output.addValueTransformTagBranchSpan(branches),
        } });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = transformed_value,
            .body = transformed_expr,
        } }));
        return transformed_value;
    }

    fn sessionTagUnionValueTransformBranchBody(
        self: *BodyBuilder,
        branch_stmts: *std.ArrayList(Ast.StmtId),
        source_union_ty: Type.TypeId,
        target_union_ty: Type.TypeId,
        source_tag: Type.TagType,
        target_union: Type.TagUnionType,
        target_tag: Type.TagType,
        case: repr.SessionValueTransformTagCase,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExprId {
        if (case.payloads.len != target_tag.payloads.len) {
            executableInvariant("session tag-union value transform payload edge count differs from target tag arity");
        }

        const source_expr = try self.output.addValueRefExpr(source_union_ty, value);
        const seen_payloads = try self.allocator.alloc(bool, case.payloads.len);
        defer self.allocator.free(seen_payloads);
        @memset(seen_payloads, false);

        const payload_exprs = try self.allocator.alloc(Ast.TagPayloadExpr, target_tag.payloads.len);
        defer self.allocator.free(payload_exprs);
        for (target_tag.payloads, 0..) |target_payload, target_i| {
            const edge = findSessionValueTransformPayloadEdge(case.payloads, @intCast(target_i), seen_payloads) orelse {
                executableInvariant("session tag-union value transform omitted a target payload edge");
            };
            const source_payload_index: usize = @intCast(edge.source_payload_index);
            if (source_payload_index >= source_tag.payloads.len) {
                executableInvariant("session tag-union value transform source payload index exceeded source tag arity");
            }
            const source_payload = source_tag.payloads[source_payload_index];
            const access_value = self.output.freshValueRef();
            const access_expr = try self.output.addExpr(source_payload.ty, access_value, .{ .tag_payload = .{
                .tag_union = source_expr,
                .payload = source_payload.payload,
            } });
            try branch_stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = access_value,
                .body = access_expr,
            } }));

            const transformed = try self.applyExecutableValueTransformRef(branch_stmts, edge.transform, access_value);
            payload_exprs[target_i] = .{
                .payload = target_payload.payload,
                .expr = try self.output.addValueRefExpr(target_payload.ty, transformed),
                .ty = target_payload.ty,
                .value = transformed,
                .bridge = try self.constructionSlotBridge(target_payload.ty, target_payload.ty),
            };
        }
        verifyAllSeen(seen_payloads, "session tag-union value transform had an extra payload edge");

        const tag_value = self.output.freshValueRef();
        const tag_expr = try self.output.addExpr(target_union_ty, tag_value, .{ .tag = .{
            .union_shape = target_union.shape,
            .tag = target_tag.tag,
            .payloads = try self.output.addTagPayloadExprSpan(payload_exprs),
        } });
        if (branch_stmts.items.len == 0) return tag_expr;
        return try self.output.addExpr(target_union_ty, tag_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(branch_stmts.items),
            .final_expr = tag_expr,
        } });
    }

    fn lowerSessionExecutableValueTransformAsBridge(
        self: *BodyBuilder,
        plan: repr.SessionExecutableValueTransformPlan,
        structural: repr.SessionExecutableStructuralBridgePlan,
    ) Allocator.Error!Ast.BridgeId {
        const from_ty = try self.lowerSessionExecutableEndpointType(plan.from);
        const to_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        return try self.lowerSessionExecutableStructuralBridgePlan(from_ty, to_ty, structural);
    }

    fn lowerSessionExecutableValueChildBridge(
        self: *BodyBuilder,
        child: checked_artifact.ExecutableValueTransformRef,
    ) Allocator.Error!Ast.BridgeId {
        return switch (child) {
            .session => |id| blk: {
                const plan = self.representation_store.sessionExecutableValueTransform(id);
                break :blk switch (plan.op) {
                    .identity => try self.constructionSlotBridge(
                        try self.lowerSessionExecutableEndpointType(plan.from),
                        try self.lowerSessionExecutableEndpointType(plan.to),
                    ),
                    .structural_bridge => |structural| try self.lowerSessionExecutableValueTransformAsBridge(plan, structural),
                    else => executableInvariant("session structural bridge child was not a bridge transform"),
                };
            },
            .published => |published| blk: {
                const context = resolvePublishedTransformContext(self.program, published);
                var published_types = PublishedTypeLowerer.init(
                    self.allocator,
                    context.executable_type_payloads,
                    context.materialization.canonical_names,
                    &self.program.canonical_names,
                    &self.program.types,
                    &self.program.row_shapes,
                    &self.program.lowered_session_types_by_key,
                );
                defer published_types.deinit();
                break :blk try lowerExecutableValueChildBridge(
                    self.program,
                    context.materialization,
                    &published_types,
                    context.executable_value_transforms,
                    published.transform,
                );
            },
        };
    }

    fn lowerSessionExecutableStructuralBridgePlan(
        self: *BodyBuilder,
        from_ty: Type.TypeId,
        to_ty: Type.TypeId,
        op: repr.SessionExecutableStructuralBridgePlan,
    ) Allocator.Error!Ast.BridgeId {
        switch (op) {
            .direct => return try self.constructionSlotBridge(from_ty, to_ty),
            else => {},
        }
        const plan: Ast.BridgePlan = switch (op) {
            .direct => unreachable,
            .zst => .zst,
            .list_reinterpret => .list_reinterpret,
            .nominal_reinterpret => .nominal_reinterpret,
            .box_unbox => |child| .{ .box_unbox = try self.lowerSessionExecutableValueChildBridge(child) },
            .box_box => |child| .{ .box_box = try self.lowerSessionExecutableValueChildBridge(child) },
            .singleton_to_tag_union => |singleton| .{ .singleton_to_tag_union = .{
                .source_payload = from_ty,
                .target_discriminant = try tagDiscriminantForId(self.program, to_ty, singleton.target_tag),
                .payload_plan = if (singleton.value_transform) |payload|
                    try self.lowerSessionExecutableValueChildBridge(payload)
                else blk: {
                    const target_union = switch (self.program.types.getType(to_ty)) {
                        .tag_union => |tag_union| tag_union,
                        else => executableInvariant("executable session singleton_to_tag_union bridge target was not a tag union"),
                    };
                    const target_tag = tagTypeForId(self.program, target_union, singleton.target_tag);
                    const target_payload_ty = (try tagPayloadEndpointType(self.allocator, self.program, target_tag)) orelse break :blk null;
                    break :blk try self.constructionSlotBridge(from_ty, target_payload_ty);
                },
            } },
            .tag_union_to_singleton => |singleton| .{ .tag_union_to_singleton = .{
                .target_payload = to_ty,
                .source_discriminant = try tagDiscriminantForId(self.program, from_ty, singleton.source_tag),
                .payload_plan = if (singleton.value_transform) |payload|
                    try self.lowerSessionExecutableValueChildBridge(payload)
                else blk: {
                    const source_union = switch (self.program.types.getType(from_ty)) {
                        .tag_union => |tag_union| tag_union,
                        else => executableInvariant("executable session tag_union_to_singleton bridge source was not a tag union"),
                    };
                    const source_tag = tagTypeForId(self.program, source_union, singleton.source_tag);
                    const source_payload_ty = (try tagPayloadEndpointType(self.allocator, self.program, source_tag)) orelse break :blk null;
                    break :blk try self.constructionSlotBridge(source_payload_ty, to_ty);
                },
            } },
        };
        return try self.output.addBridgePlan(plan);
    }

    fn applySessionCallableToErasedTransform(
        self: *BodyBuilder,
        stmts: *std.ArrayList(Ast.StmtId),
        plan: repr.SessionExecutableValueTransformPlan,
        callable: repr.SessionCallableToErasedTransformPlan,
        value: Ast.ExecutableValueRef,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const result_ty = try self.lowerSessionExecutableEndpointType(plan.to);
        const erased_ty = erasedFnType(self.program, result_ty);
        return switch (callable) {
            .finite_value => |finite| blk: {
                const source_ty = try self.lowerSessionExecutableEndpointType(plan.from);
                const source_callable_set = switch (self.program.types.getType(source_ty)) {
                    .callable_set => |callable_set| callable_set,
                    else => executableInvariant("finite session callable erasure source endpoint is not a callable set"),
                };
                if (!repr.callableSetKeyEql(source_callable_set.key, finite.adapter.callable_set_key)) {
                    executableInvariant("finite session callable erasure source callable-set key differs from adapter key");
                }
                if (!repr.erasedFnSigKeyEql(erased_ty.sig_key, finite.adapter.erased_fn_sig_key)) {
                    executableInvariant("finite session callable erasure target signature differs from adapter key");
                }
                const descriptor = callableSetDescriptorFromSlice(self.callable_set_descriptors, finite.adapter.callable_set_key) orelse {
                    executableInvariant("executable finite session callable erasure adapter has no callable-set descriptor");
                };
                const hidden_capture_ty = try self.lowerFiniteSetAdapterCaptureType(finite.adapter, descriptor);
                const hidden_capture = if (hidden_capture_ty == null) null else value;
                const packed_value = self.output.freshValueRef();
                const packed_expr = try self.output.addExpr(result_ty, packed_value, .{ .packed_erased_fn = .{
                    .sig_key = finite.adapter.erased_fn_sig_key,
                    .code = self.executableProcForErasedAdapter(finite.adapter),
                    .capture = hidden_capture,
                    .capture_ty = hidden_capture_ty,
                    .capture_shape = finite.adapter.capture_shape_key,
                } });
                try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                    .value = packed_value,
                    .body = packed_expr,
                } }));
                break :blk packed_value;
            },
            .proc_value => executableInvariant("proc-value session callable erasure is valid only while lowering the owning proc_value occurrence"),
        };
    }

    fn erasedFnCaptureType(
        self: *BodyBuilder,
        func_ty: Type.TypeId,
        sig_key: repr.ErasedFnSigKey,
    ) ?Type.TypeId {
        return switch (self.type_lowerer.output.getType(func_ty)) {
            .link => |next| self.erasedFnCaptureType(next, sig_key),
            .erased_fn => |erased| blk: {
                if (!repr.erasedFnSigKeyEql(erased.sig_key, sig_key)) {
                    executableInvariant("executable erased call callee type signature differs from call site");
                }
                break :blk erased.capture_ty;
            },
            else => executableInvariant("executable erased call callee is not an erased function value"),
        };
    }

    fn addValueExpr(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        data: Ast.Expr.Data,
    ) Allocator.Error!Ast.ExprId {
        return try self.output.addExpr(
            try self.lowerExecutableValueType(source_ty, value_info_id),
            self.output.freshValueRef(),
            data,
        );
    }

    fn exprValue(self: *const BodyBuilder, expr: Ast.ExprId) Ast.ExecutableValueRef {
        return self.output.getExpr(expr).value;
    }

    fn materializeExprValue(
        self: *BodyBuilder,
        stmt_ids: *std.ArrayList(Ast.StmtId),
        expr: Ast.ExprId,
    ) Allocator.Error!Ast.ExecutableValueRef {
        const value = self.output.freshValueRef();
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = value,
            .body = expr,
        } }));
        return value;
    }
};

fn executableInvariant(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

fn executableInvariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(fmt, args);
    unreachable;
}

/// Public `verifyCallableMatchBranch` function.
pub fn verifyCallableMatchBranch(
    representation_store: *const repr.RepresentationStore,
    callable_set_key: repr.CanonicalCallableSetKey,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    branch: Ast.CallableMatchBranch,
) void {
    debug.invariant(
        repr.callableSetKeyEql(branch.member.callable_set_key, callable_set_key),
        "executable invariant violated: callable_match branch points at a different callable set",
    );
    const member = representation_store.callableSetMember(callable_set_key, branch.member.member_index) orelse {
        debug.invariant(false, "executable invariant violated: callable_match branch points at missing callable member");
        return;
    };
    debug.invariant(
        repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match member source function type differs from call site",
    );
    debug.invariant(
        repr.canonicalTypeKeyEql(branch.source_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match branch source function type differs from call site",
    );
    debug.invariant(
        repr.canonicalTypeKeyEql(branch.executable_specialization_key.requested_fn_ty, requested_source_fn_ty),
        "executable invariant violated: callable_match executable specialization requested type differs from call site",
    );
}

fn sessionExecutableValueEndpointEql(
    a: repr.SessionExecutableValueEndpoint,
    b: repr.SessionExecutableValueEndpoint,
) bool {
    return sessionExecutableValueEndpointOwnerEql(a.owner, b.owner) and
        a.logical_ty == b.logical_ty and
        a.exec_ty.ty.payload == b.exec_ty.ty.payload and
        repr.canonicalExecValueTypeKeyEql(a.exec_ty.key, b.exec_ty.key);
}

fn sessionExecutableValueEndpointOwnerEql(
    a: repr.SessionExecutableValueEndpointOwner,
    b: repr.SessionExecutableValueEndpointOwner,
) bool {
    return switch (a) {
        .local_value => |value| switch (b) {
            .local_value => |other| value == other,
            else => false,
        },
        .procedure_param => |param| switch (b) {
            .procedure_param => |other| param.instance == other.instance and param.index == other.index,
            else => false,
        },
        .procedure_return => |proc| switch (b) {
            .procedure_return => |other| proc == other,
            else => false,
        },
        .procedure_capture => |capture| switch (b) {
            .procedure_capture => |other| capture.instance == other.instance and capture.slot == other.slot,
            else => false,
        },
        .call_raw_arg => |arg| switch (b) {
            .call_raw_arg => |other| arg.call == other.call and arg.index == other.index,
            else => false,
        },
        .erased_proc_value_adapter_arg => |arg| switch (b) {
            .erased_proc_value_adapter_arg => |other| arg.emission_plan == other.emission_plan and
                arg.source_value == other.source_value and
                canonical.procedureCallableRefEql(arg.proc_value, other.proc_value) and
                repr.erasedFnSigKeyEql(arg.erased_fn_sig_key, other.erased_fn_sig_key) and
                arg.index == other.index,
            else => false,
        },
        .erased_finite_adapter_arg => |arg| switch (b) {
            .erased_finite_adapter_arg => |other| repr.erasedAdapterKeyEql(arg.adapter, other.adapter) and
                repr.callableSetKeyEql(arg.member.callable_set_key, other.member.callable_set_key) and
                arg.member.member_index == other.member.member_index and
                arg.index == other.index,
            else => false,
        },
        .erased_finite_adapter_capture => |capture| switch (b) {
            .erased_finite_adapter_capture => |other| repr.erasedAdapterKeyEql(capture.adapter, other.adapter) and
                repr.callableSetKeyEql(capture.member.callable_set_key, other.member.callable_set_key) and
                capture.member.member_index == other.member.member_index and
                capture.slot == other.slot,
            else => false,
        },
        .erased_finite_adapter_result => |result| switch (b) {
            .erased_finite_adapter_result => |other| repr.erasedAdapterKeyEql(result.adapter, other.adapter) and
                repr.callableSetKeyEql(result.member.callable_set_key, other.member.callable_set_key) and
                result.member.member_index == other.member.member_index,
            else => false,
        },
        .call_raw_result => |call| switch (b) {
            .call_raw_result => |other| call == other,
            else => false,
        },
        .consumer_use => |owner| switch (b) {
            .consumer_use => |other| consumerUseOwnerEql(owner, other),
            else => false,
        },
        .transform_child => |child| switch (b) {
            .transform_child => |other| child.scope == other.scope and
                child.side == other.side and
                child.path == other.path,
            else => false,
        },
    };
}

fn consumerUseOwnerEql(
    a: repr.ConsumerUseOwner,
    b: repr.ConsumerUseOwner,
) bool {
    return switch (a) {
        .return_value => |ret| switch (b) {
            .return_value => |other| ret == other,
            else => false,
        },
        .call_arg => |arg| switch (b) {
            .call_arg => |other| arg.call == other.call and arg.arg_index == other.arg_index,
            else => false,
        },
        .record_field => |field| switch (b) {
            .record_field => |other| field.parent == other.parent and field.field == other.field,
            else => false,
        },
        .tuple_elem => |elem| switch (b) {
            .tuple_elem => |other| elem.parent == other.parent and elem.index == other.index,
            else => false,
        },
        .tag_payload => |payload| switch (b) {
            .tag_payload => |other| payload.parent == other.parent and
                payload.tag == other.tag and
                payload.payload == other.payload,
            else => false,
        },
        .list_elem => |elem| switch (b) {
            .list_elem => |other| elem.parent == other.parent and elem.index == other.index,
            else => false,
        },
        .nominal_backing => |backing| switch (b) {
            .nominal_backing => |other| backing.parent == other.parent and
                backing.nominal.module_name == other.nominal.module_name and
                backing.nominal.type_name == other.nominal.type_name,
            else => false,
        },
        .if_branch_result => |branch| switch (b) {
            .if_branch_result => |other| branch.parent == other.parent and
                branch.join == other.join and
                branch.branch == other.branch,
            else => false,
        },
        .source_match_branch_result => |branch| switch (b) {
            .source_match_branch_result => |other| branch.parent == other.parent and
                branch.join == other.join and
                branch.branch_index == other.branch_index,
            else => false,
        },
    };
}

fn sessionEndpointIsTransformChildForScope(
    endpoint: repr.SessionExecutableValueEndpoint,
    scope: repr.TransformEndpointScopeId,
    side: repr.TransformEndpointSide,
) bool {
    return switch (endpoint.owner) {
        .transform_child => |child| child.scope == scope and child.side == side,
        else => false,
    };
}

fn sessionEndpointOwnerIsTransformChild(owner: repr.SessionExecutableValueEndpointOwner) bool {
    return switch (owner) {
        .transform_child => true,
        else => false,
    };
}

test "executable build owns final program state" {
    std.testing.refAllDecls(@This());
}
