//! Public checked-artifact-to-LIR lowering API.
//!
//! This is the only public semantic lowering entrance after type checking. It
//! accepts already-published checked artifacts, explicit root requests, and
//! target configuration. It returns lowered LIR or resource failure only.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const mir = @import("mir");
const ir = @import("ir");

const Arc = @import("arc.zig");
const LowerIr = @import("lower_ir.zig");
const LIR = @import("LIR.zig");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const repr = mir.LambdaSolved.Representation;

pub const LowerResourceError = Allocator.Error;

pub const ArtifactSet = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
};

pub const RootRequestSet = struct {
    requests: []const checked_artifact.RootRequest,
    purpose: RootPurpose = .runtime,
    compile_time_plan_sink: ?*checked_artifact.CompileTimePlanStore = null,
};

pub const RootPurpose = enum {
    runtime,
    compile_time,
};

pub const TargetConfig = struct {
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    artifact_state: ArtifactState = .published,
};

pub const ArtifactState = enum {
    published,
    checking_finalization,
};

pub const LoweredProgram = struct {
    lir_result: LowerIr.Result,
    main_proc: LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,
    callable_set_descriptors: []repr.CanonicalCallableSetDescriptor = &.{},
    compile_time_root_payloads: []checked_artifact.CompileTimeRootPayload = &.{},

    pub fn deinit(self: *LoweredProgram) void {
        if (self.compile_time_root_payloads.len > 0) {
            self.lir_result.store.allocator.free(self.compile_time_root_payloads);
        }
        mir.Executable.Build.deinitCallableSetDescriptors(
            self.lir_result.store.allocator,
            self.callable_set_descriptors,
        );
        self.lir_result.deinit();
    }
};

pub fn lowerArtifactsToLir(
    allocator: Allocator,
    artifacts: ArtifactSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram {
    switch (target.artifact_state) {
        .published => artifacts.root.artifact.verifyPublished(),
        .checking_finalization => artifacts.root.artifact.verifyReadyForCompileTimeLowering(),
    }

    const selected_roots = try filterRootsForPurpose(allocator, roots.requests, roots.purpose);
    defer allocator.free(selected_roots);

    var mono = try mir.Mono.Specialize.run(allocator, .{
        .root = artifacts.root,
        .imports = artifacts.imports,
    }, selected_roots);
    errdefer mono.deinit();

    var row_finalized = try mir.MonoRow.run(allocator, mono);
    errdefer row_finalized.deinit();

    var lifted = try mir.Lifted.Lift.run(allocator, row_finalized);
    errdefer lifted.deinit();

    var solved = try mir.LambdaSolved.Solve.run(allocator, lifted);
    errdefer solved.deinit();

    const compile_time_root_payloads = try publishCompileTimeRootPayloads(
        allocator,
        artifacts.root.artifact,
        &solved,
        selected_roots,
        roots,
    );
    errdefer if (compile_time_root_payloads.len > 0) allocator.free(compile_time_root_payloads);

    var executable = try mir.Executable.Build.run(allocator, solved);
    errdefer executable.deinit();

    const callable_set_descriptors = executable.callable_set_descriptors;
    executable.callable_set_descriptors = &.{};
    errdefer mir.Executable.Build.deinitCallableSetDescriptors(allocator, callable_set_descriptors);

    var executable_for_ir = executable;
    executable = mir.Executable.Build.Program.init(allocator);

    var lowered_ir = try ir.Lower.fromExecutable(allocator, executable_for_ir);
    errdefer lowered_ir.deinit();

    const executable_roots = lowered_ir.root_procs.items;
    const executable_root_metadata = lowered_ir.root_metadata.items;

    var lowered_lir = try LowerIr.run(
        allocator,
        target.target_usize,
        lowered_ir,
        executable_roots,
        executable_root_metadata,
    );
    errdefer lowered_lir.deinit();

    try Arc.insert(&lowered_lir.store);

    if (lowered_lir.root_procs.items.len == 0) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked pipeline invariant violated: explicit root set produced no LIR roots", .{});
        }
        unreachable;
    }

    return .{
        .lir_result = lowered_lir,
        .main_proc = lowered_lir.root_procs.items[0],
        .target_usize = target.target_usize,
        .callable_set_descriptors = callable_set_descriptors,
        .compile_time_root_payloads = compile_time_root_payloads,
    };
}

fn publishCompileTimeRootPayloads(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    solved: *const mir.LambdaSolved.Solve.Program,
    selected_roots: []const checked_artifact.RootRequest,
    roots: RootRequestSet,
) Allocator.Error![]checked_artifact.CompileTimeRootPayload {
    if (roots.purpose != .compile_time) return &.{};

    const plan_sink = roots.compile_time_plan_sink orelse checkedPipelineInvariant("compile-time lowering requires a compile-time plan sink");
    if (selected_roots.len != solved.root_procs.items.len) {
        checkedPipelineInvariant("compile-time lowering root count changed before plan publication");
    }

    const payloads = try allocator.alloc(checked_artifact.CompileTimeRootPayload, selected_roots.len);
    errdefer allocator.free(payloads);

    var const_builder = ConstGraphPlanBuilder{
        .allocator = allocator,
        .artifact = artifact,
        .plans = plan_sink,
        .active = std.AutoHashMap(ConstPlanKey, checked_artifact.ConstGraphReificationPlanId).init(allocator),
    };
    defer const_builder.deinit();

    for (selected_roots, solved.root_procs.items, 0..) |root_request, root_proc, i| {
        const root = compileTimeRootForRequest(artifact, root_request);
        const value_context = constValueContextForRoot(solved, root_proc);
        payloads[i] = switch (root.kind) {
            .constant => .{ .const_graph = try const_builder.planFor(root.checked_type, value_context, value_context.ret) },
            .callable_binding => .{ .callable_result = try callableResultPlanForRoot(
                allocator,
                plan_sink,
                solved,
                root_proc,
            ) },
            .expect => .expect,
        };
    }

    return payloads;
}

const ConstValueContext = struct {
    value_store_id: repr.ValueInfoStoreId,
    value_store: *const repr.ValueInfoStore,
    representation_store: *const repr.RepresentationStore,
    row_shapes: *const mir.MonoRow.Store,
    ret: repr.ValueInfoId,
};

fn constValueContextForRoot(
    solved: *const mir.LambdaSolved.Solve.Program,
    root_proc: canonical.MirProcedureRef,
) ConstValueContext {
    const instance = procRepresentationInstanceForRoot(solved, root_proc);
    return .{
        .value_store_id = instance.value_store,
        .value_store = &solved.value_stores.items[@intFromEnum(instance.value_store)],
        .representation_store = &solved.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store,
        .row_shapes = &solved.row_shapes,
        .ret = instance.public_roots.ret,
    };
}

fn callableResultPlanForRoot(
    allocator: Allocator,
    plans: *checked_artifact.CompileTimePlanStore,
    solved: *const mir.LambdaSolved.Solve.Program,
    root_proc: canonical.MirProcedureRef,
) Allocator.Error!checked_artifact.CallableResultPlanId {
    const instance = procRepresentationInstanceForRoot(solved, root_proc);
    const value_store = &solved.value_stores.items[@intFromEnum(instance.value_store)];
    const representation_store = &solved.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
    const ret_info = value_store.values.items[@intFromEnum(instance.public_roots.ret)];
    const callable = ret_info.callable orelse checkedPipelineInvariant("compile-time callable root returned a value without callable metadata");
    const value_context = ConstValueContext{
        .value_store_id = instance.value_store,
        .value_store = value_store,
        .representation_store = representation_store,
        .row_shapes = &solved.row_shapes,
        .ret = instance.public_roots.ret,
    };
    const emission = representation_store.callableEmissionPlan(callable.emission_plan);
    return switch (emission) {
        .finite => |key| try finiteCallableResultPlan(allocator, plans, value_context, callable, key),
        .already_erased,
        .erase_proc_value,
        .erase_finite_set,
        => checkedPipelineInvariant("compile-time erased callable result publication is not sealed"),
    };
}

fn finiteCallableResultPlan(
    allocator: Allocator,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    callable: repr.CallableValueInfo,
    key: repr.CanonicalCallableSetKey,
) Allocator.Error!checked_artifact.CallableResultPlanId {
    const representation_store = value_context.representation_store;
    const descriptor = representation_store.callableSetDescriptor(key) orelse {
        checkedPipelineInvariant("finite compile-time callable result has no descriptor");
    };
    if (descriptor.members.len == 0) {
        checkedPipelineInvariant("finite compile-time callable result descriptor has no members");
    }

    const source_fn_ty = descriptor.members[0].proc_value.source_fn_ty;
    const members = try allocator.alloc(checked_artifact.CallableResultMemberPlan, descriptor.members.len);
    errdefer allocator.free(members);

    for (descriptor.members, 0..) |member, i| {
        if (!std.mem.eql(u8, &member.proc_value.source_fn_ty.bytes, &source_fn_ty.bytes)) {
            checkedPipelineInvariant("finite compile-time callable result descriptor mixes source function types");
        }
        if (member.capture_slots.len != 0) {
            const construction_id = callable.construction_plan orelse {
                checkedPipelineInvariant("captured finite compile-time callable result has no construction plan");
            };
            const construction = representation_store.callableConstructionPlan(construction_id);
            if (construction.selected_member != member.member) {
                checkedPipelineInvariant("captured multi-member compile-time callable result needs per-member capture plans");
            }
            if (construction.capture_values.len != member.capture_slots.len) {
                checkedPipelineInvariant("captured finite compile-time callable result capture arity disagrees with descriptor");
            }
            checkedPipelineInvariant("captured finite compile-time callable result promotion is not sealed");
        }
        members[i] = .{
            .member = member.member,
            .capture_slots = &.{},
        };
    }

    return try plans.appendCallableResult(allocator, .{ .finite = .{
        .source_fn_ty = source_fn_ty,
        .callable_set_key = key,
        .members = members,
    } });
}

fn procRepresentationInstanceForRoot(
    solved: *const mir.LambdaSolved.Solve.Program,
    root_proc: canonical.MirProcedureRef,
) repr.ProcRepresentationInstance {
    for (solved.procs.items) |proc| {
        if (!canonical.mirProcedureRefEql(proc.proc, root_proc)) continue;
        return solved.proc_instances.items[@intFromEnum(proc.representation_instance)];
    }
    checkedPipelineInvariant("compile-time root procedure has no lambda-solved representation instance");
}

fn compileTimeRootForRequest(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    request: checked_artifact.RootRequest,
) checked_artifact.CompileTimeRoot {
    for (artifact.compile_time_roots.roots) |root| {
        if (!rootMatchesRequest(root, request)) continue;
        return root;
    }
    checkedPipelineInvariant("compile-time root request had no matching root record");
}

fn rootMatchesRequest(
    root: checked_artifact.CompileTimeRoot,
    request: checked_artifact.RootRequest,
) bool {
    const kind_matches = switch (root.kind) {
        .constant => request.kind == .compile_time_constant,
        .callable_binding => request.kind == .compile_time_callable,
        .expect => request.kind == .test_expect,
    };
    return kind_matches and rootSourceEql(root.source, request.source);
}

fn rootSourceEql(a: checked_artifact.RootSource, b: checked_artifact.RootSource) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .def => |def| def == b.def,
        .expr => |expr| expr == b.expr,
        .statement => |statement| statement == b.statement,
        .required_binding => |binding| binding == b.required_binding,
    };
}

const ConstGraphPlanBuilder = struct {
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    active: std.AutoHashMap(ConstPlanKey, checked_artifact.ConstGraphReificationPlanId),

    fn deinit(self: *ConstGraphPlanBuilder) void {
        self.active.deinit();
    }

    fn planFor(
        self: *ConstGraphPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.ConstGraphReificationPlanId {
        const key = ConstPlanKey.from(checked_ty, value_context, value_info);
        if (self.active.get(key)) |active| {
            const recursive = try self.plans.reserveConstGraph(self.allocator);
            self.plans.fillConstGraph(recursive, .{ .recursive_ref = active });
            return recursive;
        }

        const id = try self.plans.reserveConstGraph(self.allocator);
        try self.active.put(key, id);
        errdefer _ = self.active.remove(key);

        const plan = try self.buildPlan(checked_ty, value_context, value_info);
        self.plans.fillConstGraph(id, plan);
        _ = self.active.remove(key);
        return id;
    }

    fn buildPlan(
        self: *ConstGraphPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.ConstGraphReificationPlan {
        return switch (self.checkedPayload(checked_ty)) {
            .empty_record => .{ .record = &.{} },
            .record => |record| .{ .record = try self.recordFields(record.fields, value_context, value_info) },
            .record_unbound => |fields| .{ .record = try self.recordFields(fields, value_context, value_info) },
            .tuple => |items| .{ .tuple = try self.tupleItems(items, value_context, value_info) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagVariants(tag_union.tags, value_context, value_info) },
            .empty_tag_union => checkedPipelineInvariant("attempted to plan empty tag union constant"),
            .alias => |alias| .{ .transparent_alias = .{
                .alias = .{
                    .module_name = alias.origin_module,
                    .type_name = alias.name,
                },
                .backing = try self.planFor(alias.backing, value_context, value_info),
            } },
            .nominal => |nominal| try self.nominalPlan(checked_ty, nominal, value_context, value_info),
            .function => if (value_info == null)
                .{ .callable_schema = self.artifact.checked_types.roots[@intFromEnum(checked_ty)].key }
            else
                .{ .callable_leaf = try self.callableLeafPlan(value_context, value_info) },
            .flex, .rigid => checkedPipelineInvariant("compile-time constant planning reached unresolved type variable"),
            .pending => checkedPipelineInvariant("compile-time constant planning reached pending checked type"),
        };
    }

    fn nominalPlan(
        self: *ConstGraphPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        nominal: checked_artifact.CheckedNominalType,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.ConstGraphReificationPlan {
        if (nominal.builtin) |builtin_nominal| {
            return switch (builtin_nominal) {
                .str => .{ .string = checked_ty },
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => .{ .scalar = checked_ty },
                .list => .{ .list = .{ .elem = try self.planFor(
                    nominalArg(nominal, 0),
                    value_context,
                    self.listElemValue(value_context, value_info),
                ) } },
                .box => .{ .box = .{ .payload = try self.planFor(
                    nominalArg(nominal, 0),
                    value_context,
                    self.boxPayloadValue(value_context, value_info),
                ) } },
                .bool => self.buildPlan(nominal.backing, value_context, value_info),
            };
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
            },
            .backing = try self.planFor(nominal.backing, value_context, value_info),
        } };
    }

    fn recordFields(
        self: *ConstGraphPlanBuilder,
        fields: []const checked_artifact.CheckedRecordField,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error![]const checked_artifact.ConstRecordFieldPlan {
        if (fields.len == 0) return &.{};
        const plans = try self.allocator.alloc(checked_artifact.ConstRecordFieldPlan, fields.len);
        errdefer self.allocator.free(plans);
        for (fields, 0..) |field, i| {
            plans[i] = .{
                .field = field.name,
                .value = try self.planFor(
                    field.ty,
                    value_context,
                    self.recordFieldValue(value_context, value_info, field.name),
                ),
            };
        }
        return plans;
    }

    fn tupleItems(
        self: *ConstGraphPlanBuilder,
        items: []const checked_artifact.CheckedTypeId,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error![]const checked_artifact.ConstTupleElemPlan {
        if (items.len == 0) return &.{};
        const plans = try self.allocator.alloc(checked_artifact.ConstTupleElemPlan, items.len);
        errdefer self.allocator.free(plans);
        for (items, 0..) |item, i| {
            plans[i] = .{
                .index = @intCast(i),
                .value = try self.planFor(item, value_context, self.tupleElemValue(value_context, value_info, @intCast(i))),
            };
        }
        return plans;
    }

    fn tagVariants(
        self: *ConstGraphPlanBuilder,
        tags: []const checked_artifact.CheckedTag,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error![]const checked_artifact.ConstTagVariantPlan {
        const variants = try self.allocator.alloc(checked_artifact.ConstTagVariantPlan, tags.len);
        errdefer {
            for (variants) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(variants);
        }
        for (tags, 0..) |tag, i| {
            const payloads = try self.allocator.alloc(checked_artifact.ConstTagPayloadPlan, tag.args.len);
            errdefer self.allocator.free(payloads);
            for (tag.args, 0..) |arg_ty, arg_i| {
                payloads[arg_i] = .{
                    .index = @intCast(arg_i),
                    .value = try self.planFor(
                        arg_ty,
                        value_context,
                        self.tagPayloadValue(value_context, value_info, tag.name, @intCast(arg_i)),
                    ),
                };
            }
            variants[i] = .{
                .tag = tag.name,
                .payloads = payloads,
            };
        }
        return variants;
    }

    fn checkedPayload(
        self: *const ConstGraphPlanBuilder,
        ty: checked_artifact.CheckedTypeId,
    ) checked_artifact.CheckedTypePayload {
        return self.artifact.checked_types.payloads[@intFromEnum(ty)];
    }

    fn callableLeafPlan(
        self: *ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CallableLeafReificationPlan {
        const context = value_context orelse checkedPipelineInvariant("callable constant leaf requires value context");
        const info_id = value_info orelse checkedPipelineInvariant("callable constant leaf requires lambda-solved value metadata");
        const info = context.value_store.values.items[@intFromEnum(info_id)];
        const callable = info.callable orelse checkedPipelineInvariant("function-typed constant leaf has no callable metadata");
        const emission = context.representation_store.callableEmissionPlan(callable.emission_plan);
        return switch (emission) {
            .finite => |key| .{ .finite = try finiteCallableResultPlan(
                self.allocator,
                self.plans,
                context,
                callable,
                key,
            ) },
            .already_erased,
            .erase_proc_value,
            .erase_finite_set,
            => checkedPipelineInvariant("erased callable constant leaf publication is not sealed"),
        };
    }

    fn valueInfo(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) ?repr.ValueInfo {
        _ = self;
        const context = value_context orelse return null;
        const info_id = value_info orelse return null;
        return context.value_store.values.items[@intFromEnum(info_id)];
    }

    fn recordFieldValue(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
        label: canonical.RecordFieldLabelId,
    ) ?repr.ValueInfoId {
        const context = value_context orelse return null;
        const info = self.valueInfo(value_context, value_info) orelse return null;
        const aggregate = info.aggregate orelse return null;
        const record = switch (aggregate) {
            .record => |record| record,
            else => checkedPipelineInvariant("record constant value had non-record aggregate metadata"),
        };
        const field_id = recordFieldIdForLabel(context.row_shapes, record.shape, label) orelse {
            checkedPipelineInvariant("record constant plan referenced missing finalized field label");
        };
        for (record.fields) |field| {
            if (field.field == field_id) return field.value;
        }
        checkedPipelineInvariant("record constant aggregate metadata omitted a finalized field");
    }

    fn tupleElemValue(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
        index: u32,
    ) ?repr.ValueInfoId {
        const info = self.valueInfo(value_context, value_info) orelse return null;
        const aggregate = info.aggregate orelse return null;
        const tuple = switch (aggregate) {
            .tuple => |tuple| tuple,
            else => checkedPipelineInvariant("tuple constant value had non-tuple aggregate metadata"),
        };
        for (tuple) |elem| {
            if (elem.index == index) return elem.value;
        }
        checkedPipelineInvariant("tuple constant aggregate metadata omitted an element");
    }

    fn tagPayloadValue(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
        tag_label: canonical.TagLabelId,
        payload_index: u32,
    ) ?repr.ValueInfoId {
        const context = value_context orelse return null;
        const info = self.valueInfo(value_context, value_info) orelse return null;
        const aggregate = info.aggregate orelse return null;
        const tag = switch (aggregate) {
            .tag => |tag| tag,
            else => checkedPipelineInvariant("tag-union constant value had non-tag aggregate metadata"),
        };
        const active_tag = context.row_shapes.tag(tag.tag);
        if (active_tag.label != tag_label) return null;
        const payloads = context.row_shapes.tagPayloads(tag.tag);
        if (payload_index >= payloads.len) {
            checkedPipelineInvariant("tag-union constant plan referenced missing finalized payload index");
        }
        const payload_id = payloads[payload_index];
        for (tag.payloads) |payload| {
            if (payload.payload == payload_id) return payload.value;
        }
        checkedPipelineInvariant("tag-union constant aggregate metadata omitted a finalized payload");
    }

    fn listElemValue(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) ?repr.ValueInfoId {
        const info = self.valueInfo(value_context, value_info) orelse return null;
        const aggregate = info.aggregate orelse return null;
        const list = switch (aggregate) {
            .list => |list| list,
            else => checkedPipelineInvariant("List(T) constant value had non-list aggregate metadata"),
        };
        if (list.elems.len == 0) return null;
        return list.elems[0];
    }

    fn boxPayloadValue(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) ?repr.ValueInfoId {
        _ = self;
        _ = value_context;
        _ = value_info;
        return null;
    }
};

const ConstPlanKey = struct {
    checked_ty: checked_artifact.CheckedTypeId,
    value_store: u32,
    value_info: u32,

    const none = std.math.maxInt(u32);

    fn from(
        checked_ty: checked_artifact.CheckedTypeId,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) ConstPlanKey {
        return .{
            .checked_ty = checked_ty,
            .value_store = if (value_context) |context| @intFromEnum(context.value_store_id) else none,
            .value_info = if (value_info) |info| @intFromEnum(info) else none,
        };
    }
};

fn recordFieldIdForLabel(
    shapes: *const mir.MonoRow.Store,
    shape: mir.MonoRow.RecordShapeId,
    label: canonical.RecordFieldLabelId,
) ?mir.MonoRow.RecordFieldId {
    for (shapes.recordShapeFields(shape)) |field_id| {
        if (shapes.recordField(field_id).label == label) return field_id;
    }
    return null;
}

fn nominalArg(
    nominal: checked_artifact.CheckedNominalType,
    index: usize,
) checked_artifact.CheckedTypeId {
    if (index >= nominal.args.len) checkedPipelineInvariant("builtin nominal type was missing an argument");
    return nominal.args[index];
}

fn filterRootsForPurpose(
    allocator: Allocator,
    roots: []const checked_artifact.RootRequest,
    purpose: RootPurpose,
) Allocator.Error![]checked_artifact.RootRequest {
    var selected = std.ArrayList(checked_artifact.RootRequest).empty;
    errdefer selected.deinit(allocator);

    for (roots) |root| {
        if (!rootMatchesPurpose(root, purpose)) continue;
        try selected.append(allocator, root);
    }

    return try selected.toOwnedSlice(allocator);
}

fn rootMatchesPurpose(root: checked_artifact.RootRequest, purpose: RootPurpose) bool {
    return switch (purpose) {
        .runtime => root.abi != .compile_time,
        .compile_time => root.abi == .compile_time,
    };
}

fn checkedPipelineInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("checked pipeline invariant violated: " ++ message, .{});
    }
    unreachable;
}

test "checked pipeline public API returns resource errors only" {
    std.testing.refAllDecls(@This());
}
