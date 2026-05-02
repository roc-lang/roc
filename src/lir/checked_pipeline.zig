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
    compile_time_artifact_sink: ?*checked_artifact.CheckedModuleArtifact = null,
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
    const artifact_sink = roots.compile_time_artifact_sink orelse checkedPipelineInvariant("compile-time lowering requires a mutable checked artifact sink");
    if (@intFromPtr(artifact_sink) != @intFromPtr(artifact)) {
        checkedPipelineInvariant("compile-time lowering artifact sink does not match root artifact");
    }
    if (selected_roots.len != solved.root_procs.items.len) {
        checkedPipelineInvariant("compile-time lowering root count changed before plan publication");
    }

    const payloads = try allocator.alloc(checked_artifact.CompileTimeRootPayload, selected_roots.len);
    errdefer allocator.free(payloads);

    var const_builder = ConstGraphPlanBuilder{
        .allocator = allocator,
        .artifact = artifact,
        .artifact_sink = artifact_sink,
        .plans = plan_sink,
        .values = &artifact_sink.comptime_values,
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
                artifact_sink,
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
    canonical_names: *const canonical.CanonicalNameStore,
    types: *const mir.LambdaSolved.Type.Store,
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
        .canonical_names = &solved.canonical_names,
        .types = &solved.types,
        .value_store_id = instance.value_store,
        .value_store = &solved.value_stores.items[@intFromEnum(instance.value_store)],
        .representation_store = &solved.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store,
        .row_shapes = &solved.row_shapes,
        .ret = instance.public_roots.ret,
    };
}

fn callableResultPlanForRoot(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
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
        .canonical_names = &solved.canonical_names,
        .types = &solved.types,
        .value_store_id = instance.value_store,
        .value_store = value_store,
        .representation_store = representation_store,
        .row_shapes = &solved.row_shapes,
        .ret = instance.public_roots.ret,
    };
    const emission = representation_store.callableEmissionPlan(callable.emission_plan);
    return switch (emission) {
        .finite => |key| try finiteCallableResultPlan(allocator, artifact_sink, plans, value_context, callable, key),
        .erase_proc_value => |erase| try erasedProcValueResultPlan(allocator, artifact_sink, plans, value_context, callable, erase),
        .already_erased,
        .erase_finite_set,
        => checkedPipelineInvariant("compile-time erased callable result publication is not sealed"),
    };
}

fn finiteCallableResultPlan(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
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
            const slot_plans = try allocator.alloc(checked_artifact.CaptureSlotReificationPlanId, member.capture_slots.len);
            errdefer allocator.free(slot_plans);
            var capture_builder = CaptureSlotPlanBuilder{
                .allocator = allocator,
                .artifact = artifact_sink,
                .plans = plans,
                .value_context = value_context,
                .active = std.AutoHashMap(CapturePlanKey, checked_artifact.CaptureSlotReificationPlanId).init(allocator),
            };
            defer capture_builder.deinit();
            for (member.capture_slots, construction.capture_values, 0..) |slot, capture_value, slot_i| {
                if (slot.slot != @as(u32, @intCast(slot_i))) {
                    checkedPipelineInvariant("captured finite compile-time callable result capture slots are not canonical");
                }
                slot_plans[slot_i] = try capture_builder.planFor(slot.source_ty, capture_value);
            }
            members[i] = .{
                .member = member.member,
                .capture_slots = slot_plans,
            };
            continue;
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

fn erasedProcValueResultPlan(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    callable: repr.CallableValueInfo,
    erase: repr.ProcValueErasePlan,
) Allocator.Error!checked_artifact.CallableResultPlanId {
    const source = switch (callable.source) {
        .proc_value => |source| source,
        else => checkedPipelineInvariant("erased proc-value result plan was attached to a non-proc callable source"),
    };
    if (!canonical.procedureCallableRefEql(source.proc.callable, erase.proc_value)) {
        checkedPipelineInvariant("erased proc-value result plan procedure differs from callable source");
    }
    if (!repr.canonicalTypeKeyEql(source.fn_ty, erase.proc_value.source_fn_ty)) {
        checkedPipelineInvariant("erased proc-value result plan source function type differs from callable source");
    }
    if (source.captures.len != erase.capture_slots.len) {
        checkedPipelineInvariant("erased proc-value result plan capture arity differs from callable source");
    }

    return try plans.appendCallableResult(allocator, .{ .erased = .{
        .source_fn_ty = erase.erased_fn_sig_key.source_fn_ty,
        .sig_key = erase.erased_fn_sig_key,
        .provenance = try cloneBoxBoundarySpan(allocator, erase.provenance),
        .code = .{ .direct_proc_value = .{
            .proc_value = erase.proc_value,
            .capture_shape_key = erase.capture_shape_key,
        } },
        .capture = try erasedCapturePlanForProcValue(allocator, artifact_sink, plans, value_context, source, erase),
    } });
}

fn erasedCapturePlanForProcValue(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    source: anytype,
    erase: repr.ProcValueErasePlan,
) Allocator.Error!checked_artifact.ErasedCaptureReificationPlan {
    if (erase.erased_fn_sig_key.capture_ty == null) {
        if (erase.capture_slots.len != 0) {
            checkedPipelineInvariant("erased proc-value result had capture slots but no erased hidden capture type");
        }
        return .none;
    }
    if (erase.capture_slots.len == 0) {
        return .{ .zero_sized_typed = erase.erased_fn_sig_key.capture_ty.? };
    }

    const slot_plans = try allocator.alloc(checked_artifact.CaptureSlotReificationPlanId, erase.capture_slots.len);
    errdefer allocator.free(slot_plans);
    var seen = try allocator.alloc(bool, erase.capture_slots.len);
    defer allocator.free(seen);
    @memset(seen, false);

    var capture_builder = CaptureSlotPlanBuilder{
        .allocator = allocator,
        .artifact = artifact_sink,
        .plans = plans,
        .value_context = value_context,
        .active = std.AutoHashMap(CapturePlanKey, checked_artifact.CaptureSlotReificationPlanId).init(allocator),
    };
    defer capture_builder.deinit();

    for (erase.capture_slots) |slot| {
        const slot_index: usize = @intCast(slot.slot);
        if (slot_index >= source.captures.len) {
            checkedPipelineInvariant("erased proc-value result capture slot exceeded source capture arity");
        }
        if (seen[slot_index]) {
            checkedPipelineInvariant("erased proc-value result capture slot was duplicated");
        }
        slot_plans[slot_index] = try capture_builder.planFor(slot.source_ty, source.captures[slot_index]);
        seen[slot_index] = true;
    }
    for (seen) |was_seen| {
        if (!was_seen) checkedPipelineInvariant("erased proc-value result did not publish every capture slot");
    }
    return .{ .values = slot_plans };
}

fn cloneBoxBoundarySpan(
    allocator: Allocator,
    provenance: []const canonical.BoxBoundaryId,
) Allocator.Error![]const canonical.BoxBoundaryId {
    if (provenance.len == 0) {
        checkedPipelineInvariant("erased callable result had no Box(T) provenance");
    }
    return try allocator.dupe(canonical.BoxBoundaryId, provenance);
}

const CapturePlanKey = struct {
    source_ty: canonical.CanonicalTypeKey,
    value_store_id: repr.ValueInfoStoreId,
    value_info: repr.ValueInfoId,
};

const CaptureSlotPlanBuilder = struct {
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    active: std.AutoHashMap(CapturePlanKey, checked_artifact.CaptureSlotReificationPlanId),

    fn deinit(self: *CaptureSlotPlanBuilder) void {
        self.active.deinit();
    }

    fn planFor(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        value_info: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const key = CapturePlanKey{
            .source_ty = source_ty,
            .value_store_id = self.value_context.value_store_id,
            .value_info = value_info,
        };
        if (self.active.get(key)) |active| {
            const recursive = try self.plans.reserveCaptureSlot(self.allocator);
            self.plans.fillCaptureSlot(recursive, .{ .recursive_ref = active });
            return recursive;
        }

        const id = try self.plans.reserveCaptureSlot(self.allocator);
        try self.active.put(key, id);
        errdefer _ = self.active.remove(key);

        const plan = try self.buildPlan(source_ty, value_info);
        self.plans.fillCaptureSlot(id, plan);
        _ = self.active.remove(key);
        return id;
    }

    fn buildPlan(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        const checked_ty = self.artifact.checked_types.rootForKey(source_ty) orelse {
            checkedPipelineInvariant("capture slot source type was not published in checked type store");
        };
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        if (info.callable != null) {
            return .{ .callable_leaf = try self.callableLeafPlan(value_info_id) };
        }

        return switch (self.artifact.checked_types.payloads[@intFromEnum(checked_ty)]) {
            .pending => checkedPipelineInvariant("capture slot planning reached pending checked type"),
            .flex, .rigid => checkedPipelineInvariant("capture slot planning reached unresolved type variable"),
            .function => .{ .callable_leaf = try self.callableLeafPlan(value_info_id) },
            .empty_record => .{ .record = &.{} },
            .record => |record| .{ .record = try self.recordFields(record.fields, value_info_id) },
            .record_unbound => |fields| .{ .record = try self.recordFields(fields, value_info_id) },
            .tuple => |items| .{ .tuple = try self.tupleItems(items, value_info_id) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagVariants(tag_union.tags, value_info_id) },
            .empty_tag_union => checkedPipelineInvariant("capture slot planning reached empty tag union"),
            .alias => |alias| .{ .nominal = .{
                .nominal = .{
                    .module_name = alias.origin_module,
                    .type_name = alias.name,
                },
                .backing = try self.planFor(
                    self.artifact.checked_types.roots[@intFromEnum(alias.backing)].key,
                    value_info_id,
                ),
            } },
            .nominal => |nominal| try self.nominalPlan(checked_ty, source_ty, nominal, value_info_id),
        };
    }

    fn callableLeafPlan(
        self: *CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CallableResultPlanId {
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        const callable = info.callable orelse checkedPipelineInvariant("function-typed capture leaf has no callable metadata");
        const emission = self.value_context.representation_store.callableEmissionPlan(callable.emission_plan);
        return switch (emission) {
            .finite => |key| try finiteCallableResultPlan(
                self.allocator,
                self.artifact,
                self.plans,
                self.value_context,
                callable,
                key,
            ),
            .erase_proc_value => |erase| try erasedProcValueResultPlan(
                self.allocator,
                self.artifact,
                self.plans,
                self.value_context,
                callable,
                erase,
            ),
            .already_erased,
            .erase_finite_set,
            => checkedPipelineInvariant("erased capture callable leaf publication is not sealed"),
        };
    }

    fn serializableLeafPlan(
        self: *CaptureSlotPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        source_ty: canonical.CanonicalTypeKey,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        var const_builder = ConstGraphPlanBuilder{
            .allocator = self.allocator,
            .artifact = self.artifact,
            .artifact_sink = self.artifact,
            .plans = self.plans,
            .values = &self.artifact.comptime_values,
            .active = std.AutoHashMap(ConstPlanKey, checked_artifact.ConstGraphReificationPlanId).init(self.allocator),
        };
        defer const_builder.deinit();
        const reification_plan = try const_builder.planFor(checked_ty, self.value_context, value_info_id);
        return .{ .serializable_leaf = .{
            .requested_source_ty = source_ty,
            .source_scheme = try self.artifact.checked_types.ensureSchemeForRoot(self.allocator, checked_ty),
            .schema = try const_builder.schemaForPlan(reification_plan),
            .reification_plan = reification_plan,
        } };
    }

    fn nominalPlan(
        self: *CaptureSlotPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        source_ty: canonical.CanonicalTypeKey,
        nominal: checked_artifact.CheckedNominalType,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        if (nominal.builtin) |builtin_nominal| {
            return switch (builtin_nominal) {
                .str,
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
                => try self.serializableLeafPlan(
                    checked_ty,
                    source_ty,
                    value_info_id,
                ),
                .list => .{ .list = .{
                    .elem = try self.listElemPlan(nominalArg(nominal, 0), value_info_id),
                } },
                .box => .{ .box = try self.boxPayloadPlan(nominalArg(nominal, 0), value_info_id) },
                .bool => try self.planFor(
                    self.artifact.checked_types.roots[@intFromEnum(nominal.backing)].key,
                    value_info_id,
                ),
            };
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
            },
            .backing = try self.planFor(
                self.artifact.checked_types.roots[@intFromEnum(nominal.backing)].key,
                value_info_id,
            ),
        } };
    }

    fn recordFields(
        self: *CaptureSlotPlanBuilder,
        fields: []const checked_artifact.CheckedRecordField,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error![]const checked_artifact.CaptureRecordFieldPlan {
        if (fields.len == 0) return &.{};
        const plans_out = try self.allocator.alloc(checked_artifact.CaptureRecordFieldPlan, fields.len);
        errdefer self.allocator.free(plans_out);
        for (fields, 0..) |field, i| {
            const child = self.recordFieldValue(value_info_id, field.name);
            plans_out[i] = .{
                .field = field.name,
                .value = try self.planFor(self.artifact.checked_types.roots[@intFromEnum(field.ty)].key, child),
            };
        }
        return plans_out;
    }

    fn tupleItems(
        self: *CaptureSlotPlanBuilder,
        items: []const checked_artifact.CheckedTypeId,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error![]const checked_artifact.CaptureTupleElemPlan {
        if (items.len == 0) return &.{};
        const plans_out = try self.allocator.alloc(checked_artifact.CaptureTupleElemPlan, items.len);
        errdefer self.allocator.free(plans_out);
        for (items, 0..) |item, i| {
            plans_out[i] = .{
                .index = @intCast(i),
                .value = try self.planFor(
                    self.artifact.checked_types.roots[@intFromEnum(item)].key,
                    self.tupleElemValue(value_info_id, @intCast(i)),
                ),
            };
        }
        return plans_out;
    }

    fn tagVariants(
        self: *CaptureSlotPlanBuilder,
        tags: []const checked_artifact.CheckedTag,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error![]const checked_artifact.CaptureTagVariantPlan {
        const variants = try self.allocator.alloc(checked_artifact.CaptureTagVariantPlan, tags.len);
        errdefer {
            for (variants) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(variants);
        }
        for (tags, 0..) |tag, i| {
            const payloads = try self.allocator.alloc(checked_artifact.CaptureTagPayloadPlan, tag.args.len);
            errdefer self.allocator.free(payloads);
            for (tag.args, 0..) |arg_ty, arg_i| {
                payloads[arg_i] = .{
                    .index = @intCast(arg_i),
                    .value = try self.planFor(
                        self.artifact.checked_types.roots[@intFromEnum(arg_ty)].key,
                        self.tagPayloadValue(value_info_id, tag.name, @intCast(arg_i)),
                    ),
                };
            }
            variants[i] = .{ .tag = tag.name, .payloads = payloads };
        }
        return variants;
    }

    fn listElemPlan(
        self: *CaptureSlotPlanBuilder,
        elem_ty: checked_artifact.CheckedTypeId,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const elem_value = self.listRepresentativeValue(value_info_id);
        return try self.planFor(self.artifact.checked_types.roots[@intFromEnum(elem_ty)].key, elem_value);
    }

    fn boxPayloadPlan(
        self: *CaptureSlotPlanBuilder,
        payload_ty: checked_artifact.CheckedTypeId,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        const boxed = info.boxed orelse checkedPipelineInvariant("Box(T) capture had no boxed metadata");
        const payload_value = self.valueForRoot(boxed.payload_root) orelse {
            checkedPipelineInvariant("Box(T) capture payload root had no value-flow metadata");
        };
        return try self.planFor(
            self.artifact.checked_types.roots[@intFromEnum(payload_ty)].key,
            payload_value,
        );
    }

    fn valueForRoot(
        self: *const CaptureSlotPlanBuilder,
        root: repr.RepRootId,
    ) ?repr.ValueInfoId {
        for (self.value_context.value_store.values.items, 0..) |value, i| {
            if (value.root == root) return @enumFromInt(@as(u32, @intCast(i)));
        }
        return null;
    }

    fn recordFieldValue(
        self: *CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
        label: canonical.RecordFieldLabelId,
    ) repr.ValueInfoId {
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        const aggregate = info.aggregate orelse checkedPipelineInvariant("record capture had no aggregate metadata");
        const record = switch (aggregate) {
            .record => |record| record,
            else => checkedPipelineInvariant("record capture value had non-record aggregate metadata"),
        };
        const field_id = recordFieldIdForLabel(self.value_context.row_shapes, record.shape, label) orelse {
            checkedPipelineInvariant("record capture plan referenced missing finalized field label");
        };
        for (record.fields) |field| {
            if (field.field == field_id) return field.value;
        }
        checkedPipelineInvariant("record capture aggregate metadata omitted a finalized field");
    }

    fn tupleElemValue(
        self: *CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
        index: u32,
    ) repr.ValueInfoId {
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        const aggregate = info.aggregate orelse checkedPipelineInvariant("tuple capture had no aggregate metadata");
        const tuple = switch (aggregate) {
            .tuple => |tuple| tuple,
            else => checkedPipelineInvariant("tuple capture value had non-tuple aggregate metadata"),
        };
        for (tuple) |elem| {
            if (elem.index == index) return elem.value;
        }
        checkedPipelineInvariant("tuple capture aggregate metadata omitted an element");
    }

    fn tagPayloadValue(
        self: *CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
        tag_label: canonical.TagLabelId,
        payload_index: u32,
    ) repr.ValueInfoId {
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        const aggregate = info.aggregate orelse checkedPipelineInvariant("tag capture had no aggregate metadata");
        const tag = switch (aggregate) {
            .tag => |tag| tag,
            else => checkedPipelineInvariant("tag capture value had non-tag aggregate metadata"),
        };
        const active_tag = self.value_context.row_shapes.tag(tag.tag);
        if (active_tag.label != tag_label) {
            checkedPipelineInvariant("tag capture plan selected inactive tag variant");
        }
        const payloads = self.value_context.row_shapes.tagPayloads(tag.tag);
        if (payload_index >= payloads.len) {
            checkedPipelineInvariant("tag capture plan referenced missing finalized payload index");
        }
        const payload_id = payloads[payload_index];
        for (tag.payloads) |payload| {
            if (payload.payload == payload_id) return payload.value;
        }
        checkedPipelineInvariant("tag capture aggregate metadata omitted a finalized payload");
    }

    fn listRepresentativeValue(
        self: *CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
    ) repr.ValueInfoId {
        const info = self.value_context.value_store.values.items[@intFromEnum(value_info_id)];
        const aggregate = info.aggregate orelse checkedPipelineInvariant("List(T) capture had no aggregate metadata");
        const list = switch (aggregate) {
            .list => |list| list,
            else => checkedPipelineInvariant("List(T) capture value had non-list aggregate metadata"),
        };
        if (list.elems.len == 0) {
            checkedPipelineInvariant("List(T) capture slot planning needs a representative element plan");
        }
        return list.elems[0];
    }
};

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

fn callableLeafSourceFnTy(
    leaf: checked_artifact.CallableLeafInstance,
) canonical.CanonicalTypeKey {
    return switch (leaf) {
        .finite => |finite| finite.proc_value.source_fn_ty,
        .erased_boxed => |erased| erased.source_fn_ty,
    };
}

const ConstGraphPlanBuilder = struct {
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    artifact_sink: ?*checked_artifact.CheckedModuleArtifact = null,
    plans: *checked_artifact.CompileTimePlanStore,
    values: ?*checked_artifact.CompileTimeValueStore = null,
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

    fn schemaForPlan(
        self: *ConstGraphPlanBuilder,
        plan_id: checked_artifact.ConstGraphReificationPlanId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const values = self.values orelse checkedPipelineInvariant("const graph schema construction requires a value store sink");
        const plan = self.plans.constGraph(plan_id);
        return switch (plan) {
            .pending => checkedPipelineInvariant("const graph schema construction reached pending plan"),
            .scalar => |checked_ty| self.schemaForScalarCheckedType(values, checked_ty),
            .string => values.addSchema(.str),
            .list => |list| values.addSchema(.{ .list = try self.schemaForPlan(list.elem) }),
            .box => |box| values.addSchema(.{ .box = try self.schemaForPlan(box.payload) }),
            .tuple => |items| self.schemaForTuplePlan(values, items),
            .record => |fields| self.schemaForRecordPlan(values, fields),
            .tag_union => |variants| self.schemaForTagUnionPlan(values, variants),
            .transparent_alias => |alias| values.addSchema(.{ .alias = .{
                .type_name = alias.alias,
                .backing = try self.schemaForPlan(alias.backing),
            } }),
            .nominal => |nominal| values.addSchema(.{ .nominal = .{
                .type_name = nominal.nominal,
                .backing = try self.schemaForPlan(nominal.backing),
                .is_opaque = false,
            } }),
            .callable_leaf => |leaf| self.schemaForCallableLeaf(values, leaf),
            .callable_schema => |source_fn_ty| values.addSchema(.{ .callable = source_fn_ty }),
            .recursive_ref => |ref| self.schemaForPlan(ref),
        };
    }

    fn schemaForScalarCheckedType(
        self: *ConstGraphPlanBuilder,
        values: *checked_artifact.CompileTimeValueStore,
        checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const nominal = switch (self.checkedPayload(checked_ty)) {
            .nominal => |nominal| nominal,
            else => checkedPipelineInvariant("scalar const graph plan did not reference a nominal scalar type"),
        };
        const builtin_nominal = nominal.builtin orelse checkedPipelineInvariant("scalar const graph plan referenced non-builtin nominal type");
        return switch (builtin_nominal) {
            .u8 => values.addSchema(.{ .int = .u8 }),
            .i8 => values.addSchema(.{ .int = .i8 }),
            .u16 => values.addSchema(.{ .int = .u16 }),
            .i16 => values.addSchema(.{ .int = .i16 }),
            .u32 => values.addSchema(.{ .int = .u32 }),
            .i32 => values.addSchema(.{ .int = .i32 }),
            .u64 => values.addSchema(.{ .int = .u64 }),
            .i64 => values.addSchema(.{ .int = .i64 }),
            .u128 => values.addSchema(.{ .int = .u128 }),
            .i128 => values.addSchema(.{ .int = .i128 }),
            .f32 => values.addSchema(.{ .frac = .f32 }),
            .f64 => values.addSchema(.{ .frac = .f64 }),
            .dec => values.addSchema(.{ .frac = .dec }),
            .str,
            .list,
            .box,
            .bool,
            => checkedPipelineInvariant("scalar const graph plan referenced non-scalar builtin nominal type"),
        };
    }

    fn schemaForTuplePlan(
        self: *ConstGraphPlanBuilder,
        values: *checked_artifact.CompileTimeValueStore,
        items: []const checked_artifact.ConstTupleElemPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (items.len == 0) return values.addSchema(.zst);
        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        for (items, 0..) |item, i| {
            schemas[i] = try self.schemaForPlan(item.value);
        }
        return values.addSchema(.{ .tuple = schemas });
    }

    fn schemaForRecordPlan(
        self: *ConstGraphPlanBuilder,
        values: *checked_artifact.CompileTimeValueStore,
        fields: []const checked_artifact.ConstRecordFieldPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (fields.len == 0) return values.addSchema(.zst);
        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        for (fields, 0..) |field, i| {
            schema_fields[i] = .{
                .name = field.field,
                .schema = try self.schemaForPlan(field.value),
            };
        }
        return values.addSchema(.{ .record = schema_fields });
    }

    fn schemaForTagUnionPlan(
        self: *ConstGraphPlanBuilder,
        values: *checked_artifact.CompileTimeValueStore,
        variants_plan: []const checked_artifact.ConstTagVariantPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const variants = try self.allocator.alloc(checked_artifact.ComptimeVariantSchema, variants_plan.len);
        errdefer {
            for (variants) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(variants);
        }
        for (variants_plan, 0..) |variant_plan, i| {
            const payloads = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, variant_plan.payloads.len);
            errdefer self.allocator.free(payloads);
            for (variant_plan.payloads, 0..) |payload_plan, payload_i| {
                payloads[payload_i] = try self.schemaForPlan(payload_plan.value);
            }
            variants[i] = .{ .name = variant_plan.tag, .payloads = payloads };
        }
        return values.addSchema(.{ .tag_union = variants });
    }

    fn schemaForCallableLeaf(
        self: *ConstGraphPlanBuilder,
        values: *checked_artifact.CompileTimeValueStore,
        leaf: checked_artifact.CallableLeafReificationPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        return switch (leaf) {
            .already_resolved => |resolved| values.addSchema(.{ .callable = callableLeafSourceFnTy(resolved) }),
            .finite => |result_plan| switch (self.plans.callableResult(result_plan)) {
                .finite => |finite| values.addSchema(.{ .callable = finite.source_fn_ty }),
                .erased => |erased| values.addSchema(.{ .callable = erased.source_fn_ty }),
            },
            .erased_boxed => |result_plan| switch (self.plans.callableResult(result_plan)) {
                .finite => checkedPipelineInvariant("erased boxed callable leaf referenced a finite callable result plan"),
                .erased => |erased| values.addSchema(.{ .callable = erased.source_fn_ty }),
            },
        };
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
                    try self.listElemValue(value_context, value_info),
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
                self.artifactSink(),
                self.plans,
                context,
                callable,
                key,
            ) },
            .erase_proc_value => |erase| .{ .erased_boxed = try erasedProcValueResultPlan(
                self.allocator,
                self.artifactSink(),
                self.plans,
                context,
                callable,
                erase,
            ) },
            .already_erased,
            .erase_finite_set,
            => checkedPipelineInvariant("erased callable constant leaf publication is not sealed"),
        };
    }

    fn artifactSink(self: *ConstGraphPlanBuilder) *checked_artifact.CheckedModuleArtifact {
        return self.artifact_sink orelse checkedPipelineInvariant("compile-time callable leaf planning requires mutable artifact sink");
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
    ) Allocator.Error!?repr.ValueInfoId {
        const context = value_context orelse return null;
        const info = self.valueInfo(value_context, value_info) orelse return null;
        const aggregate = info.aggregate orelse return null;
        const list = switch (aggregate) {
            .list => |list| list,
            else => checkedPipelineInvariant("List(T) constant value had non-list aggregate metadata"),
        };
        if (list.elems.len == 0) return null;
        const first_key = try repr.execValueTypeKeyForValue(
            self.allocator,
            context.canonical_names,
            context.types,
            context.representation_store,
            context.value_store,
            list.elems[0],
        );
        for (list.elems[1..]) |elem| {
            const elem_key = try repr.execValueTypeKeyForValue(
                self.allocator,
                context.canonical_names,
                context.types,
                context.representation_store,
                context.value_store,
                elem,
            );
            if (!repr.canonicalExecValueTypeKeyEql(first_key, elem_key)) {
                checkedPipelineInvariant("List(T) constant elements have different executable representations");
            }
        }
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
