//! Public checked-artifact-to-LIR lowering API.
//!
//! This is the only public semantic lowering entrance after type checking. It
//! accepts already-published checked artifacts, explicit root requests, and
//! target configuration. It returns lowered LIR or resource failure only.

const std = @import("std");
const builtin = @import("builtin");
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
    requests: []const checked_artifact.RootRequest = &.{},
    compile_time_requests: []const checked_artifact.CompileTimeEvaluationRequest = &.{},
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
    compile_time_payloads: []checked_artifact.CompileTimeEvaluationPayload = &.{},
    erased_callable_code_map: []LoweredErasedCallableCodeEntry = &.{},

    pub fn deinit(self: *LoweredProgram) void {
        for (self.erased_callable_code_map) |entry| {
            if (entry.exec_arg_tys.len > 0) {
                self.lir_result.store.allocator.free(entry.exec_arg_tys);
            }
        }
        if (self.erased_callable_code_map.len > 0) {
            self.lir_result.store.allocator.free(self.erased_callable_code_map);
        }
        if (self.compile_time_payloads.len > 0) {
            self.lir_result.store.allocator.free(self.compile_time_payloads);
        }
        self.lir_result.deinit();
    }
};

pub const LoweredErasedCallableCodeEntry = struct {
    lir_proc: LIR.LirProcSpecId,
    code: canonical.ErasedCallableCodeRef,
    source_fn_ty: canonical.CanonicalTypeKey,
    exec_arg_tys: []const canonical.CanonicalExecValueTypeKey,
    exec_ret_ty: canonical.CanonicalExecValueTypeKey,
    capture_shape_key: canonical.CaptureShapeKey,
};

const ExecutableErasedCallableCodeOrigin = struct {
    executable_proc: mir.Executable.Ast.ExecutableProcId,
    code: canonical.ErasedCallableCodeRef,
    source_fn_ty: canonical.CanonicalTypeKey,
    exec_arg_tys: []const canonical.CanonicalExecValueTypeKey,
    exec_ret_ty: canonical.CanonicalExecValueTypeKey,
    capture_shape_key: canonical.CaptureShapeKey,
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
    const selected_entrypoints = try entrypointsForPurpose(allocator, selected_roots, roots);
    defer allocator.free(selected_entrypoints);

    var solved = try lowerArtifactsToLambdaSolved(allocator, artifacts, selected_entrypoints);
    errdefer solved.deinit();

    try publishCallableSetDescriptorsForLowering(
        allocator,
        artifacts.root.artifact,
        &solved,
        roots,
        target.artifact_state,
    );
    try publishErasedFnAbisForLowering(
        allocator,
        artifacts.root.artifact,
        &solved,
        roots,
        target.artifact_state,
    );

    const compile_time_payloads = try publishCompileTimePayloads(
        allocator,
        artifacts.root.artifact,
        &solved,
        selected_entrypoints,
        roots,
    );
    errdefer if (compile_time_payloads.len > 0) allocator.free(compile_time_payloads);

    var executable = try mir.Executable.Build.run(
        allocator,
        solved,
        .{
            .root = artifacts.root,
            .imports = artifacts.imports,
        },
        artifacts.root.artifact.callable_set_descriptors.descriptors,
    );
    errdefer executable.deinit();

    var erased_code_origins = try collectExecutableErasedCallableCodeOrigins(allocator, &executable);
    errdefer deinitExecutableErasedCallableCodeOrigins(allocator, erased_code_origins);

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

    const erased_callable_code_map = try buildLoweredErasedCallableCodeMap(allocator, erased_code_origins, &lowered_lir);
    errdefer deinitLoweredErasedCallableCodeMap(allocator, erased_callable_code_map);
    deinitExecutableErasedCallableCodeOrigins(allocator, erased_code_origins);
    erased_code_origins = &.{};

    try Arc.insert(&lowered_lir.store, &lowered_lir.layouts);

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
        .compile_time_payloads = compile_time_payloads,
        .erased_callable_code_map = erased_callable_code_map,
    };
}

fn lowerArtifactsToLambdaSolved(
    allocator: Allocator,
    artifacts: ArtifactSet,
    selected_entrypoints: []const checked_artifact.LoweringEntrypointRequest,
) Allocator.Error!mir.LambdaSolved.Solve.Program {
    var mono = try mir.Mono.Specialize.run(allocator, .{
        .root = artifacts.root,
        .imports = artifacts.imports,
    }, selected_entrypoints);
    errdefer mono.deinit();

    var row_finalized = try mir.MonoRow.run(allocator, mono);
    errdefer row_finalized.deinit();

    var lifted = try mir.Lifted.Lift.run(allocator, row_finalized);
    errdefer lifted.deinit();

    return try mir.LambdaSolved.Solve.run(allocator, lifted, .{
        .root = artifacts.root,
        .imports = artifacts.imports,
    });
}

fn collectExecutableErasedCallableCodeOrigins(
    allocator: Allocator,
    program: *const mir.Executable.Build.Program,
) Allocator.Error![]ExecutableErasedCallableCodeOrigin {
    const entries = try allocator.alloc(ExecutableErasedCallableCodeOrigin, program.procs.items.len);
    var initialized: usize = 0;
    errdefer {
        for (entries[0..initialized]) |entry| {
            if (entry.exec_arg_tys.len > 0) allocator.free(entry.exec_arg_tys);
        }
        if (entries.len > 0) allocator.free(entries);
    }

    for (program.procs.items, 0..) |proc, i| {
        const def = program.ast.defs.items[@intFromEnum(proc.body)];
        const exec_arg_tys = try allocator.dupe(canonical.CanonicalExecValueTypeKey, def.specialization_key.exec_arg_tys);
        entries[i] = switch (proc.origin) {
            .source => |source| .{
                .executable_proc = proc.executable_proc,
                .code = .{ .direct_proc_value = .{
                    .proc_value = source.callable,
                    .capture_shape_key = def.specialization_key.capture_shape_key,
                } },
                .source_fn_ty = source.callable.source_fn_ty,
                .exec_arg_tys = exec_arg_tys,
                .exec_ret_ty = def.specialization_key.exec_ret_ty,
                .capture_shape_key = def.specialization_key.capture_shape_key,
            },
            .erased_adapter => |adapter| blk: {
                if (!repr.canonicalTypeKeyEql(def.specialization_key.requested_fn_ty, adapter.source_fn_ty)) {
                    checkedPipelineInvariant("erased adapter executable code origin source function type differs from specialization key");
                }
                if (!repr.captureShapeKeyEql(def.specialization_key.capture_shape_key, adapter.capture_shape_key)) {
                    checkedPipelineInvariant("erased adapter executable code origin capture shape differs from adapter key");
                }
                break :blk .{
                    .executable_proc = proc.executable_proc,
                    .code = .{ .finite_set_adapter = adapter },
                    .source_fn_ty = adapter.source_fn_ty,
                    .exec_arg_tys = exec_arg_tys,
                    .exec_ret_ty = def.specialization_key.exec_ret_ty,
                    .capture_shape_key = adapter.capture_shape_key,
                };
            },
        };
        initialized += 1;
    }

    return entries;
}

fn deinitExecutableErasedCallableCodeOrigins(
    allocator: Allocator,
    entries: []ExecutableErasedCallableCodeOrigin,
) void {
    for (entries) |entry| {
        if (entry.exec_arg_tys.len > 0) allocator.free(entry.exec_arg_tys);
    }
    if (entries.len > 0) allocator.free(entries);
}

fn buildLoweredErasedCallableCodeMap(
    allocator: Allocator,
    origins: []const ExecutableErasedCallableCodeOrigin,
    lowered_lir: *const LowerIr.Result,
) Allocator.Error![]LoweredErasedCallableCodeEntry {
    const entries = try allocator.alloc(LoweredErasedCallableCodeEntry, origins.len);
    var initialized: usize = 0;
    errdefer {
        for (entries[0..initialized]) |entry| {
            if (entry.exec_arg_tys.len > 0) allocator.free(entry.exec_arg_tys);
        }
        if (entries.len > 0) allocator.free(entries);
    }

    for (origins, 0..) |origin, i| {
        const lir_proc = lowered_lir.lirProcForExecutable(origin.executable_proc) orelse {
            checkedPipelineInvariant("lowered erased callable code origin has no LIR procedure");
        };
        entries[i] = .{
            .lir_proc = lir_proc,
            .code = origin.code,
            .source_fn_ty = origin.source_fn_ty,
            .exec_arg_tys = try allocator.dupe(canonical.CanonicalExecValueTypeKey, origin.exec_arg_tys),
            .exec_ret_ty = origin.exec_ret_ty,
            .capture_shape_key = origin.capture_shape_key,
        };
        initialized += 1;
    }

    return entries;
}

fn deinitLoweredErasedCallableCodeMap(
    allocator: Allocator,
    entries: []LoweredErasedCallableCodeEntry,
) void {
    for (entries) |entry| {
        if (entry.exec_arg_tys.len > 0) allocator.free(entry.exec_arg_tys);
    }
    if (entries.len > 0) allocator.free(entries);
}

fn publishCallableSetDescriptorsForLowering(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    solved: *const mir.LambdaSolved.Solve.Program,
    roots: RootRequestSet,
    artifact_state: ArtifactState,
) Allocator.Error!void {
    switch (artifact_state) {
        .published => {
            if (builtin.mode != .Debug) return;
            for (solved.solve_sessions.items) |*session| {
                for (session.representation_store.callable_set_descriptors) |descriptor| {
                    const published = artifact.callable_set_descriptors.descriptorFor(descriptor.key) orelse {
                        checkedPipelineInvariant("published checked artifact is missing a callable-set descriptor required by lowering");
                    };
                    if (!callableSetDescriptorEql(published.*, descriptor)) {
                        checkedPipelineInvariant("published checked artifact callable-set descriptor differs from solved descriptor");
                    }
                }
            }
        },
        .checking_finalization => {
            const artifact_sink = roots.compile_time_artifact_sink orelse checkedPipelineInvariant("checking-finalization lowering requires mutable checked artifact sink");
            if (@intFromPtr(artifact_sink) != @intFromPtr(artifact)) {
                checkedPipelineInvariant("checking-finalization descriptor publication artifact sink does not match root artifact");
            }

            var descriptors = std.ArrayList(repr.CanonicalCallableSetDescriptor).empty;
            defer descriptors.deinit(allocator);
            for (solved.solve_sessions.items) |*session| {
                try descriptors.appendSlice(allocator, session.representation_store.callable_set_descriptors);
            }
            try artifact_sink.callable_set_descriptors.publishFromDescriptors(allocator, descriptors.items);
        },
    }
}

fn callableSetDescriptorEql(a: repr.CanonicalCallableSetDescriptor, b: repr.CanonicalCallableSetDescriptor) bool {
    if (!repr.callableSetKeyEql(a.key, b.key)) return false;
    if (a.members.len != b.members.len) return false;
    for (a.members, b.members) |left, right| {
        if (left.member != right.member) return false;
        if (!canonical.procedureCallableRefEql(left.proc_value, right.proc_value)) return false;
        if (!canonical.mirProcedureRefEql(left.source_proc, right.source_proc)) return false;
        if (!std.mem.eql(u8, &left.capture_shape_key.bytes, &right.capture_shape_key.bytes)) return false;
        if (left.capture_slots.len != right.capture_slots.len) return false;
        for (left.capture_slots, right.capture_slots) |left_slot, right_slot| {
            if (left_slot.slot != right_slot.slot) return false;
            if (!std.mem.eql(u8, &left_slot.source_ty.bytes, &right_slot.source_ty.bytes)) return false;
            if (!repr.canonicalExecValueTypeKeyEql(left_slot.exec_value_ty, right_slot.exec_value_ty)) return false;
        }
    }
    return true;
}

fn publishErasedFnAbisForLowering(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    solved: *const mir.LambdaSolved.Solve.Program,
    roots: RootRequestSet,
    artifact_state: ArtifactState,
) Allocator.Error!void {
    switch (artifact_state) {
        .published => {
            if (builtin.mode != .Debug) return;
            for (solved.solve_sessions.items) |*session| {
                session.representation_store.erased_fn_abis.verifyPublished();
                for (session.representation_store.erased_fn_abis.abis) |abi| {
                    if (artifact.erased_fn_abis.abiFor(abi.key) == null) {
                        checkedPipelineInvariant("published checked artifact is missing an erased ABI required by lowering");
                    }
                }
            }
        },
        .checking_finalization => {
            const artifact_sink = roots.compile_time_artifact_sink orelse checkedPipelineInvariant("checking-finalization erased ABI publication requires mutable checked artifact sink");
            if (@intFromPtr(artifact_sink) != @intFromPtr(artifact)) {
                checkedPipelineInvariant("checking-finalization erased ABI publication artifact sink does not match root artifact");
            }

            for (solved.solve_sessions.items) |*session| {
                session.representation_store.erased_fn_abis.verifyPublished();
                for (session.representation_store.erased_fn_abis.abis) |abi| {
                    _ = try artifact_sink.erased_fn_abis.append(allocator, abi);
                }
            }
        },
    }
}

fn publishCompileTimePayloads(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    solved: *const mir.LambdaSolved.Solve.Program,
    selected_entrypoints: []const checked_artifact.LoweringEntrypointRequest,
    roots: RootRequestSet,
) Allocator.Error![]checked_artifact.CompileTimeEvaluationPayload {
    if (roots.purpose != .compile_time) return &.{};

    const plan_sink = roots.compile_time_plan_sink orelse checkedPipelineInvariant("compile-time lowering requires a compile-time plan sink");
    const artifact_sink = roots.compile_time_artifact_sink orelse checkedPipelineInvariant("compile-time lowering requires a mutable checked artifact sink");
    if (@intFromPtr(artifact_sink) != @intFromPtr(artifact)) {
        checkedPipelineInvariant("compile-time lowering artifact sink does not match root artifact");
    }
    if (selected_entrypoints.len != solved.root_procs.items.len) {
        checkedPipelineInvariant("compile-time lowering root count changed before plan publication");
    }

    const payloads = try allocator.alloc(checked_artifact.CompileTimeEvaluationPayload, selected_entrypoints.len);
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

    for (selected_entrypoints, solved.root_procs.items, 0..) |entrypoint, root_proc, i| {
        const value_context = constValueContextForRoot(solved, root_proc);
        payloads[i] = switch (entrypoint) {
            .root => |root_request| root_payload: {
                const root = compileTimeRootForRequest(artifact, root_request);
                break :root_payload .{ .local_root = switch (root.kind) {
                    .constant => .{ .const_graph = try const_builder.planFor(root.checked_type, value_context, value_context.ret) },
                    .callable_binding => .{ .callable_result = try callableResultPlanForRoot(
                        allocator,
                        artifact_sink,
                        plan_sink,
                        solved,
                        root_proc,
                    ) },
                    .expect => .expect,
                } };
            },
            .const_instance => |request| .{ .const_instance = try const_builder.planFor(
                request.requested_source_ty_payload,
                value_context,
                value_context.ret,
            ) },
            .callable_binding_instance => .{ .callable_binding_instance = try callableResultPlanForRoot(
                allocator,
                artifact_sink,
                plan_sink,
                solved,
                root_proc,
            ) },
        };
    }

    return payloads;
}

const ConstValueContext = struct {
    solved: *const mir.LambdaSolved.Solve.Program,
    canonical_names: *const canonical.CanonicalNameStore,
    types: *const mir.LambdaSolved.Type.Store,
    value_store_id: repr.ValueInfoStoreId,
    value_store: *const repr.ValueInfoStore,
    representation_store: *const repr.RepresentationStore,
    row_shapes: *const mir.MonoRow.Store,
    ret: repr.ValueInfoId,
};

const ExecutablePayloadWithKey = struct {
    ref: checked_artifact.ExecutableTypePayloadRef,
    key: canonical.CanonicalExecValueTypeKey,
};

const ExecutablePayloadSessionKey = struct {
    solve_session: repr.RepresentationSolveSessionId,
    payload: repr.SessionExecutableTypePayloadId,
};

const ExecutableTypePayloadBuilder = struct {
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    context: ConstValueContext,
    active_session_payloads: std.AutoHashMap(ExecutablePayloadSessionKey, checked_artifact.ExecutableTypePayloadId),

    fn init(
        allocator: Allocator,
        artifact: *checked_artifact.CheckedModuleArtifact,
        context: ConstValueContext,
    ) ExecutableTypePayloadBuilder {
        return .{
            .allocator = allocator,
            .artifact = artifact,
            .context = context,
            .active_session_payloads = std.AutoHashMap(ExecutablePayloadSessionKey, checked_artifact.ExecutableTypePayloadId).init(allocator),
        };
    }

    fn deinit(self: *ExecutableTypePayloadBuilder) void {
        self.active_session_payloads.deinit();
    }

    fn artifactRef(self: *const ExecutableTypePayloadBuilder) canonical.ArtifactRef {
        return .{ .bytes = self.artifact.key.bytes };
    }

    fn refFor(self: *const ExecutableTypePayloadBuilder, id: checked_artifact.ExecutableTypePayloadId) checked_artifact.ExecutableTypePayloadRef {
        return .{
            .artifact = self.artifactRef(),
            .payload = id,
        };
    }

    fn appendPayload(
        self: *ExecutableTypePayloadBuilder,
        key: canonical.CanonicalExecValueTypeKey,
        payload: checked_artifact.ExecutableTypePayload,
    ) Allocator.Error!checked_artifact.ExecutableTypePayloadRef {
        const id = try self.artifact.executable_type_payloads.append(self.allocator, key, payload);
        return self.refFor(id);
    }

    fn payloadForCurrentValue(
        self: *ExecutableTypePayloadBuilder,
        value: repr.ValueInfoId,
    ) Allocator.Error!ExecutablePayloadWithKey {
        return try self.payloadForValueInStore(
            self.context.value_store_id,
            self.context.value_store,
            self.context.representation_store,
            value,
        );
    }

    fn payloadForValueInStore(
        self: *ExecutableTypePayloadBuilder,
        value_store_id: repr.ValueInfoStoreId,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
        value: repr.ValueInfoId,
    ) Allocator.Error!ExecutablePayloadWithKey {
        _ = value_store_id;
        const info = value_store.values.items[@intFromEnum(value)];
        const endpoint = info.exec_ty orelse {
            checkedPipelineInvariant("artifact executable payload value has no published session endpoint");
        };
        return try self.payloadForSessionEndpoint(
            self.solveSessionIdForRepresentationStore(representation_store),
            endpoint,
        );
    }

    fn payloadForSessionEndpoint(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        endpoint: repr.SessionExecutableTypeEndpoint,
    ) Allocator.Error!ExecutablePayloadWithKey {
        const store = self.sessionPayloadStore(solve_session);
        const actual_key = store.keyFor(endpoint.ty.payload);
        if (!repr.canonicalExecValueTypeKeyEql(actual_key, endpoint.key)) {
            checkedPipelineInvariant("artifact executable payload session endpoint key differs from payload store key");
        }
        return try self.payloadForSessionPayload(solve_session, endpoint.ty.payload, endpoint.key);
    }

    fn payloadForCurrentSessionKey(
        self: *ExecutableTypePayloadBuilder,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!ExecutablePayloadWithKey {
        const solve_session = self.solveSessionIdForRepresentationStore(self.context.representation_store);
        const ref = self.context.representation_store.session_executable_type_payloads.refForKey(key) orelse {
            checkedPipelineInvariant("artifact executable payload key has no current-session payload ref");
        };
        return try self.payloadForSessionPayload(solve_session, ref.payload, key);
    }

    fn payloadForSessionPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        payload_id: repr.SessionExecutableTypePayloadId,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!ExecutablePayloadWithKey {
        const store = self.sessionPayloadStore(solve_session);
        const actual_key = store.keyFor(payload_id);
        if (!repr.canonicalExecValueTypeKeyEql(actual_key, key)) {
            checkedPipelineInvariant("artifact executable payload session payload key differs from requested key");
        }
        const active_key = ExecutablePayloadSessionKey{
            .solve_session = solve_session,
            .payload = payload_id,
        };
        if (self.active_session_payloads.get(active_key)) |active| {
            return .{
                .ref = self.refFor(active),
                .key = key,
            };
        }
        if (self.artifact.executable_type_payloads.refForKey(self.artifactRef(), key)) |existing| {
            return .{
                .ref = existing,
                .key = key,
            };
        }

        const id = try self.artifact.executable_type_payloads.reserve(self.allocator, key);
        try self.active_session_payloads.put(active_key, id);
        errdefer _ = self.active_session_payloads.remove(active_key);

        const payload = try self.sessionPayload(solve_session, payload_id);
        self.artifact.executable_type_payloads.fill(id, payload);
        _ = self.active_session_payloads.remove(active_key);
        return .{
            .ref = self.refFor(id),
            .key = key,
        };
    }

    fn sessionPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        payload_id: repr.SessionExecutableTypePayloadId,
    ) Allocator.Error!checked_artifact.ExecutableTypePayload {
        const payload = self.sessionPayloadStore(solve_session).get(payload_id);
        return switch (payload) {
            .pending => checkedPipelineInvariant("artifact executable payload reached pending session payload"),
            .primitive => |prim| .{ .primitive = prim },
            .record => |record| .{ .record = try self.sessionRecordPayload(solve_session, record) },
            .tuple => |items| .{ .tuple = try self.sessionTuplePayload(solve_session, items) },
            .tag_union => |tag_union| .{ .tag_union = try self.sessionTagUnionPayload(solve_session, tag_union) },
            .list => |child| .{ .list = try self.sessionChildPayload(solve_session, child) },
            .box => |child| .{ .box = try self.sessionChildPayload(solve_session, child) },
            .nominal => |nominal| blk: {
                const backing = try self.payloadForSessionPayload(solve_session, nominal.backing.payload, nominal.backing_key);
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .source_ty = nominal.source_ty,
                    .backing = backing.ref,
                    .backing_key = backing.key,
                } };
            },
            .vacant_callable_slot => .vacant_callable_slot,
            .callable_set => |callable_set| .{ .callable_set = try self.sessionCallableSetPayload(solve_session, callable_set) },
            .erased_fn => |erased| .{ .erased_fn = try self.sessionErasedFnPayload(solve_session, erased) },
            .recursive_ref => |ref| .{ .recursive_ref = self.active_session_payloads.get(.{
                .solve_session = solve_session,
                .payload = ref,
            }) orelse checkedPipelineInvariant("artifact executable payload recursive ref has no active artifact payload") },
        };
    }

    fn sessionChildPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        child: repr.SessionExecutableTypePayloadChild,
    ) Allocator.Error!checked_artifact.ExecutableTypePayloadChild {
        const payload = try self.payloadForSessionPayload(solve_session, child.ty.payload, child.key);
        return .{ .ty = payload.ref, .key = payload.key };
    }

    fn sessionRecordPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        record: repr.SessionExecutableRecordPayload,
    ) Allocator.Error![]const checked_artifact.ExecutableRecordFieldPayload {
        if (record.fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ExecutableRecordFieldPayload, record.fields.len);
        errdefer self.allocator.free(out);
        for (record.fields, 0..) |field, i| {
            const child = try self.payloadForSessionPayload(solve_session, field.ty.payload, field.key);
            out[i] = .{
                .field = self.context.row_shapes.recordField(field.field).label,
                .ty = child.ref,
                .key = child.key,
            };
        }
        return out;
    }

    fn sessionTuplePayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        items: []const repr.SessionExecutableTupleElemPayload,
    ) Allocator.Error![]const checked_artifact.ExecutableTupleElemPayload {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ExecutableTupleElemPayload, items.len);
        errdefer self.allocator.free(out);
        for (items, 0..) |item, i| {
            const child = try self.payloadForSessionPayload(solve_session, item.ty.payload, item.key);
            out[i] = .{
                .index = item.index,
                .ty = child.ref,
                .key = child.key,
            };
        }
        return out;
    }

    fn sessionTagUnionPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        tag_union: repr.SessionExecutableTagUnionPayload,
    ) Allocator.Error![]const checked_artifact.ExecutableTagVariantPayload {
        if (tag_union.variants.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ExecutableTagVariantPayload, tag_union.variants.len);
        for (out) |*variant| variant.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (out) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(out);
        }
        for (tag_union.variants, 0..) |variant, i| {
            out[i] = .{
                .tag = self.context.row_shapes.tag(variant.tag).label,
                .payloads = try self.sessionTagPayloads(solve_session, variant.payloads),
            };
        }
        return out;
    }

    fn sessionTagPayloads(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        payloads: []const repr.SessionExecutableTagPayload,
    ) Allocator.Error![]const checked_artifact.ExecutableTagPayload {
        if (payloads.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ExecutableTagPayload, payloads.len);
        errdefer self.allocator.free(out);
        for (payloads, 0..) |payload, i| {
            const child = try self.payloadForSessionPayload(solve_session, payload.ty.payload, payload.key);
            out[i] = .{
                .index = self.context.row_shapes.tagPayload(payload.payload).logical_index,
                .ty = child.ref,
                .key = child.key,
            };
        }
        return out;
    }

    fn sessionCallableSetPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        callable_set: repr.SessionExecutableCallableSetPayload,
    ) Allocator.Error!checked_artifact.ExecutableCallableSetPayload {
        if (callable_set.members.len == 0) return .{
            .key = callable_set.key,
            .members = &.{},
        };
        const members = try self.allocator.alloc(checked_artifact.ExecutableCallableSetMemberPayload, callable_set.members.len);
        errdefer self.allocator.free(members);
        for (callable_set.members, 0..) |member, i| {
            members[i] = .{ .member = member.member };
            if (member.payload_ty) |payload_ty| {
                const payload_key = member.payload_ty_key orelse {
                    checkedPipelineInvariant("artifact executable callable-set member payload ref has no key");
                };
                const payload = try self.payloadForSessionPayload(solve_session, payload_ty.payload, payload_key);
                members[i].payload_ty = payload.ref;
                members[i].payload_ty_key = payload.key;
            }
        }
        return .{
            .key = callable_set.key,
            .members = members,
        };
    }

    fn sessionErasedFnPayload(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
        erased: repr.SessionExecutableErasedFnPayload,
    ) Allocator.Error!checked_artifact.ExecutableErasedFnPayload {
        const capture = if (erased.capture_ty) |capture_ty| blk: {
            const capture_key = erased.capture_ty_key orelse {
                checkedPipelineInvariant("artifact executable erased payload capture ref has no key");
            };
            break :blk try self.payloadForSessionPayload(solve_session, capture_ty.payload, capture_key);
        } else null;
        return .{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .capture_ty = if (capture) |item| item.ref else null,
            .capture_ty_key = if (capture) |item| item.key else null,
        };
    }

    fn sessionPayloadStore(
        self: *ExecutableTypePayloadBuilder,
        solve_session: repr.RepresentationSolveSessionId,
    ) *const repr.SessionExecutableTypePayloadStore {
        const index = @intFromEnum(solve_session);
        if (index >= self.context.solved.solve_sessions.items.len) {
            checkedPipelineInvariant("artifact executable payload referenced out-of-range solve session");
        }
        return &self.context.solved.solve_sessions.items[index].representation_store.session_executable_type_payloads;
    }

    fn solveSessionIdForRepresentationStore(
        self: *ExecutableTypePayloadBuilder,
        representation_store: *const repr.RepresentationStore,
    ) repr.RepresentationSolveSessionId {
        for (self.context.solved.solve_sessions.items, 0..) |*session, raw| {
            if (&session.representation_store == representation_store) {
                return @enumFromInt(@as(u32, @intCast(raw)));
            }
        }
        checkedPipelineInvariant("artifact executable payload could not identify solve session for representation store");
    }

    fn hiddenCapturePayloadForAlreadyErased(
        self: *ExecutableTypePayloadBuilder,
        erased: repr.AlreadyErasedCallablePlan,
    ) Allocator.Error!?ExecutablePayloadWithKey {
        return switch (erased.capture) {
            .none => blk: {
                if (erased.sig_key.capture_ty != null) checkedPipelineInvariant("already-erased executable payload has no capture but signature has capture type");
                break :blk null;
            },
            .zero_sized_ty => |ty| blk: {
                if (erased.sig_key.capture_ty) |expected| {
                    const capture = try self.payloadForCurrentSessionKey(expected);
                    if (!repr.canonicalExecValueTypeKeyEql(capture.key, expected)) {
                        checkedPipelineInvariant("already-erased executable payload zero-sized capture key differs from signature");
                    }
                    _ = ty;
                    break :blk capture;
                } else {
                    checkedPipelineInvariant("already-erased executable payload zero-sized capture has no signature capture type");
                }
            },
            .value => |value| blk: {
                const capture = try self.payloadForCurrentValue(value);
                if (erased.sig_key.capture_ty) |expected| {
                    if (!repr.canonicalExecValueTypeKeyEql(capture.key, expected)) {
                        checkedPipelineInvariant("already-erased executable payload capture key differs from signature");
                    }
                } else {
                    checkedPipelineInvariant("already-erased executable payload capture value has no signature capture type");
                }
                break :blk capture;
            },
        };
    }

    fn hiddenCapturePayloadForProcValue(
        self: *ExecutableTypePayloadBuilder,
        erase: repr.ProcValueErasePlan,
    ) Allocator.Error!?ExecutablePayloadWithKey {
        if (erase.erased_fn_sig_key.capture_ty == null) {
            if (erase.capture_slots.len != 0) checkedPipelineInvariant("erased proc-value executable payload has captures but no hidden capture type");
            return null;
        }
        const instance = &self.context.solved.proc_instances.items[@intFromEnum(erase.target_instance)];
        const value_store = &self.context.solved.value_stores.items[@intFromEnum(instance.value_store)];
        const representation_store = &self.context.solved.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
        const captures = value_store.sliceValueSpan(instance.public_roots.captures);
        return try self.tuplePayloadForTargetCaptureSlots(instance.value_store, value_store, representation_store, captures, erase.capture_slots, erase.erased_fn_sig_key.capture_ty.?);
    }

    fn payloadForCallableSetType(
        self: *ExecutableTypePayloadBuilder,
        key: repr.CanonicalCallableSetKey,
        expected_key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!ExecutablePayloadWithKey {
        const ref = self.context.representation_store.session_executable_type_payloads.refForKey(expected_key) orelse {
            checkedPipelineInvariant("callable-set executable payload key had no session payload");
        };
        const source_payload = self.context.representation_store.session_executable_type_payloads.get(ref.payload);
        const callable_set = switch (source_payload) {
            .callable_set => |callable_set| callable_set,
            else => checkedPipelineInvariant("callable-set executable payload key did not name a callable set"),
        };
        if (!repr.callableSetKeyEql(callable_set.key, key)) {
            checkedPipelineInvariant("callable-set executable payload key disagrees with requested callable-set key");
        }
        return try self.payloadForSessionPayload(
            self.solveSessionIdForRepresentationStore(self.context.representation_store),
            ref.payload,
            expected_key,
        );
    }

    fn tuplePayloadForTargetCaptureSlots(
        self: *ExecutableTypePayloadBuilder,
        value_store_id: repr.ValueInfoStoreId,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
        captures: []const repr.ValueInfoId,
        slots: []const repr.CallableSetCaptureSlot,
        expected_key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!ExecutablePayloadWithKey {
        if (captures.len != slots.len) {
            checkedPipelineInvariant("erased proc-value executable payload target capture count differs from erase plan");
        }
        if (slots.len == 0) {
            const ref = try self.appendPayload(expected_key, .{ .tuple = &.{} });
            return .{
                .ref = ref,
                .key = expected_key,
            };
        }

        const items = try self.allocator.alloc(checked_artifact.ExecutableTupleElemPayload, slots.len);
        errdefer self.allocator.free(items);
        for (slots, captures, 0..) |slot, capture, i| {
            if (slot.slot != @as(u32, @intCast(i))) {
                checkedPipelineInvariant("erased proc-value executable payload target capture slots are not canonical");
            }
            const child = try self.payloadForValueInStore(value_store_id, value_store, representation_store, capture);
            if (!repr.canonicalExecValueTypeKeyEql(child.key, slot.exec_value_ty)) {
                checkedPipelineInvariant("erased proc-value executable payload target capture key differs from erase plan");
            }
            items[i] = .{
                .index = @intCast(i),
                .ty = child.ref,
                .key = child.key,
            };
        }
        const ref = try self.appendPayload(expected_key, .{ .tuple = items });
        return .{
            .ref = ref,
            .key = expected_key,
        };
    }

    fn procInstanceForMir(
        self: *ExecutableTypePayloadBuilder,
        proc: canonical.MirProcedureRef,
    ) *const repr.ProcRepresentationInstance {
        for (self.context.solved.proc_instances.items) |*instance| {
            if (canonical.mirProcedureRefEql(instance.proc, proc)) return instance;
        }
        checkedPipelineInvariant("executable payload could not find procedure instance for MIR procedure");
    }
};

fn constValueContextForRoot(
    solved: *const mir.LambdaSolved.Solve.Program,
    root_proc: canonical.MirProcedureRef,
) ConstValueContext {
    const instance = procRepresentationInstanceForRoot(solved, root_proc);
    return constValueContextForInstance(solved, instance);
}

fn constValueContextForInstance(
    solved: *const mir.LambdaSolved.Solve.Program,
    instance: repr.ProcRepresentationInstance,
) ConstValueContext {
    return .{
        .solved = solved,
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
        .solved = solved,
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
        .finite => |key| try finiteCallableResultPlan(allocator, artifact_sink, plans, value_context, instance.public_roots.ret, callable, key),
        .already_erased => |erased| try alreadyErasedResultPlan(allocator, artifact_sink, plans, value_context, erased),
        .erase_proc_value => |erase| try erasedProcValueResultPlan(allocator, artifact_sink, plans, value_context, callable, erase),
        .erase_finite_set => |erase| try erasedFiniteSetResultPlan(allocator, artifact_sink, plans, value_context, instance.public_roots.ret, callable, erase),
    };
}

fn finiteCallableResultPlan(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    callable_value: repr.ValueInfoId,
    callable: repr.CallableValueInfo,
    key: repr.CanonicalCallableSetKey,
) Allocator.Error!checked_artifact.CallableResultPlanId {
    var capture_builder = CaptureSlotPlanBuilder{
        .allocator = allocator,
        .artifact = artifact_sink,
        .plans = plans,
        .value_context = value_context,
        .active = std.AutoHashMap(CapturePlanKey, checked_artifact.CaptureSlotReificationPlanId).init(allocator),
    };
    defer capture_builder.deinit();
    return try capture_builder.finiteCallableResultPlanForValue(callable_value, callable, key);
}

fn erasedPromotedSignaturePayloadsForProcValue(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    value_context: ConstValueContext,
    erase: repr.ProcValueErasePlan,
) Allocator.Error!checked_artifact.ErasedPromotedProcedureExecutableSignaturePayloads {
    var builder = ExecutableTypePayloadBuilder.init(allocator, artifact_sink, value_context);
    defer builder.deinit();

    const target_instance = &value_context.solved.proc_instances.items[@intFromEnum(erase.target_instance)];
    return try erasedPromotedSignaturePayloadsForProcInstance(
        allocator,
        &builder,
        target_instance,
        erase.erased_fn_sig_key,
        erase.erased_fn_sig_key.source_fn_ty,
        erase.executable_specialization_key.exec_ret_ty,
        erase.capture_shape_key,
        try builder.hiddenCapturePayloadForProcValue(erase),
    );
}

fn erasedPromotedSignaturePayloadsForFiniteSetAdapter(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    value_context: ConstValueContext,
    erase: repr.FiniteSetErasePlan,
) Allocator.Error!checked_artifact.ErasedPromotedProcedureExecutableSignaturePayloads {
    var builder = ExecutableTypePayloadBuilder.init(allocator, artifact_sink, value_context);
    defer builder.deinit();

    const descriptor = value_context.representation_store.callableSetDescriptor(erase.adapter.callable_set_key) orelse {
        checkedPipelineInvariant("erased finite-set promoted signature has no callable-set descriptor");
    };
    if (descriptor.members.len == 0) {
        checkedPipelineInvariant("erased finite-set promoted signature descriptor has no members");
    }

    const first_instance = builder.procInstanceForMir(descriptor.members[0].source_proc);
    const hidden_capture = if (erase.adapter.erased_fn_sig_key.capture_ty == null)
        null
    else
        try builder.payloadForCallableSetType(erase.adapter.callable_set_key, erase.adapter.erased_fn_sig_key.capture_ty.?);
    return try erasedPromotedSignaturePayloadsForProcInstance(
        allocator,
        &builder,
        first_instance,
        erase.adapter.erased_fn_sig_key,
        erase.adapter.source_fn_ty,
        erase.result_ty,
        erase.adapter.capture_shape_key,
        hidden_capture,
    );
}

fn erasedPromotedSignaturePayloadsForAlreadyErased(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    value_context: ConstValueContext,
    erased: repr.AlreadyErasedCallablePlan,
) Allocator.Error!checked_artifact.ErasedPromotedProcedureExecutableSignaturePayloads {
    var builder = ExecutableTypePayloadBuilder.init(allocator, artifact_sink, value_context);
    defer builder.deinit();

    const abi = value_context.representation_store.erased_fn_abis.abiFor(erased.sig_key.abi) orelse {
        checkedPipelineInvariant("already-erased promoted signature references missing erased ABI payload");
    };
    if (abi.fixed_arity != @as(u32, @intCast(abi.arg_exec_keys.len))) {
        checkedPipelineInvariant("already-erased promoted signature ABI arity differs from argument key count");
    }

    const param_exec_tys = if (abi.arg_exec_keys.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.ExecutableTypePayloadRef, abi.arg_exec_keys.len);
    errdefer if (param_exec_tys.len > 0) allocator.free(param_exec_tys);
    const erased_call_args = if (abi.arg_exec_keys.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.ExecutableTypePayloadRef, abi.arg_exec_keys.len);
    errdefer if (erased_call_args.len > 0) allocator.free(erased_call_args);
    const arg_keys = if (abi.arg_exec_keys.len == 0)
        &.{}
    else
        try allocator.dupe(canonical.CanonicalExecValueTypeKey, abi.arg_exec_keys);
    errdefer if (arg_keys.len > 0) allocator.free(arg_keys);
    const erased_arg_keys = if (abi.arg_exec_keys.len == 0)
        &.{}
    else
        try allocator.dupe(canonical.CanonicalExecValueTypeKey, abi.arg_exec_keys);
    errdefer if (erased_arg_keys.len > 0) allocator.free(erased_arg_keys);

    for (abi.arg_exec_keys, 0..) |arg_key, i| {
        const payload_ref = builder.artifact.executable_type_payloads.refForKey(builder.artifactRef(), arg_key) orelse {
            checkedPipelineInvariant("already-erased promoted signature ABI argument key has no published executable payload");
        };
        param_exec_tys[i] = payload_ref;
        erased_call_args[i] = payload_ref;
    }

    const ret_payload = builder.artifact.executable_type_payloads.refForKey(builder.artifactRef(), abi.ret_exec_key) orelse {
        checkedPipelineInvariant("already-erased promoted signature ABI result key has no published executable payload");
    };
    const hidden_capture = try builder.hiddenCapturePayloadForAlreadyErased(erased);
    if ((erased.sig_key.capture_ty == null) != (hidden_capture == null)) {
        checkedPipelineInvariant("already-erased promoted signature hidden capture presence differs from erased signature key");
    }

    return .{
        .source_fn_ty = erased.sig_key.source_fn_ty,
        .param_exec_tys = param_exec_tys,
        .param_exec_ty_keys = arg_keys,
        .wrapper_ret = ret_payload,
        .wrapper_ret_key = abi.ret_exec_key,
        .erased_call_args = erased_call_args,
        .erased_call_arg_keys = erased_arg_keys,
        .erased_call_ret = ret_payload,
        .erased_call_ret_key = abi.ret_exec_key,
        .hidden_capture = if (hidden_capture) |capture| .{
            .exec_ty = capture.ref,
            .exec_ty_key = capture.key,
        } else null,
        .capture_shape_key = erased.capture_shape_key,
    };
}

fn erasedPromotedSignaturePayloadsForProcInstance(
    allocator: Allocator,
    builder: *ExecutableTypePayloadBuilder,
    instance: *const repr.ProcRepresentationInstance,
    sig_key: canonical.ErasedFnSigKey,
    source_fn_ty: canonical.CanonicalTypeKey,
    expected_wrapper_ret_key: canonical.CanonicalExecValueTypeKey,
    capture_shape_key: canonical.CaptureShapeKey,
    hidden_capture: ?ExecutablePayloadWithKey,
) Allocator.Error!checked_artifact.ErasedPromotedProcedureExecutableSignaturePayloads {
    const value_store = &builder.context.solved.value_stores.items[@intFromEnum(instance.value_store)];
    const representation_store = &builder.context.solved.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
    const abi = representation_store.erased_fn_abis.abiFor(sig_key.abi) orelse {
        checkedPipelineInvariant("erased promoted executable signature references missing erased ABI payload");
    };
    const params = value_store.sliceValueSpan(instance.public_roots.params);
    if (abi.fixed_arity != @as(u32, @intCast(params.len)) or abi.arg_exec_keys.len != params.len) {
        checkedPipelineInvariant("erased promoted executable signature ABI arity differs from wrapper parameter arity");
    }
    const param_exec_tys = if (params.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.ExecutableTypePayloadRef, params.len);
    errdefer if (param_exec_tys.len > 0) allocator.free(param_exec_tys);
    const param_exec_ty_keys = if (params.len == 0)
        &.{}
    else
        try allocator.alloc(canonical.CanonicalExecValueTypeKey, params.len);
    errdefer if (param_exec_ty_keys.len > 0) allocator.free(param_exec_ty_keys);

    for (params, 0..) |param, i| {
        const payload = try builder.payloadForValueInStore(
            instance.value_store,
            value_store,
            representation_store,
            param,
        );
        param_exec_tys[i] = payload.ref;
        param_exec_ty_keys[i] = payload.key;
    }

    const erased_call_args = if (abi.arg_exec_keys.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.ExecutableTypePayloadRef, abi.arg_exec_keys.len);
    errdefer if (erased_call_args.len > 0) allocator.free(erased_call_args);
    const erased_call_arg_keys = if (abi.arg_exec_keys.len == 0)
        &.{}
    else
        try allocator.dupe(canonical.CanonicalExecValueTypeKey, abi.arg_exec_keys);
    errdefer if (erased_call_arg_keys.len > 0) allocator.free(erased_call_arg_keys);
    for (abi.arg_exec_keys, 0..) |arg_key, i| {
        erased_call_args[i] = builder.artifact.executable_type_payloads.refForKey(builder.artifactRef(), arg_key) orelse {
            checkedPipelineInvariant("erased promoted executable signature ABI argument key has no published executable payload");
        };
    }

    const wrapper_ret = try builder.payloadForValueInStore(
        instance.value_store,
        value_store,
        representation_store,
        instance.public_roots.ret,
    );
    if (!repr.canonicalExecValueTypeKeyEql(wrapper_ret.key, expected_wrapper_ret_key)) {
        checkedPipelineInvariant("erased promoted executable signature wrapper return key differs from target proc return key");
    }
    const erased_call_ret = builder.artifact.executable_type_payloads.refForKey(builder.artifactRef(), abi.ret_exec_key) orelse {
        checkedPipelineInvariant("erased promoted executable signature ABI result key has no published executable payload");
    };

    if ((sig_key.capture_ty == null) != (hidden_capture == null)) {
        checkedPipelineInvariant("erased promoted executable signature hidden capture presence differs from erased signature key");
    }
    if (hidden_capture) |capture| {
        const expected = sig_key.capture_ty orelse unreachable;
        if (!repr.canonicalExecValueTypeKeyEql(capture.key, expected)) {
            checkedPipelineInvariant("erased promoted executable signature hidden capture key differs from erased signature key");
        }
    }

    return .{
        .source_fn_ty = source_fn_ty,
        .param_exec_tys = param_exec_tys,
        .param_exec_ty_keys = param_exec_ty_keys,
        .wrapper_ret = wrapper_ret.ref,
        .wrapper_ret_key = wrapper_ret.key,
        .erased_call_args = erased_call_args,
        .erased_call_arg_keys = erased_call_arg_keys,
        .erased_call_ret = erased_call_ret,
        .erased_call_ret_key = abi.ret_exec_key,
        .hidden_capture = if (hidden_capture) |capture| .{
            .exec_ty = capture.ref,
            .exec_ty_key = capture.key,
        } else null,
        .capture_shape_key = capture_shape_key,
    };
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
        .code_plan = .{ .materialized_by_lowering = .{ .direct_proc_value = .{
            .proc_value = erase.proc_value,
            .capture_shape_key = erase.capture_shape_key,
        } } },
        .capture = try erasedCapturePlanForProcValue(allocator, artifact_sink, plans, value_context, erase),
        .result_ty = erase.executable_specialization_key.exec_ret_ty,
        .executable_signature_payloads = try erasedPromotedSignaturePayloadsForProcValue(
            allocator,
            artifact_sink,
            value_context,
            erase,
        ),
    } });
}

fn erasedFiniteSetResultPlan(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    callable_value: repr.ValueInfoId,
    callable: repr.CallableValueInfo,
    erase: repr.FiniteSetErasePlan,
) Allocator.Error!checked_artifact.CallableResultPlanId {
    return try plans.appendCallableResult(allocator, .{ .erased = .{
        .source_fn_ty = erase.adapter.source_fn_ty,
        .sig_key = erase.adapter.erased_fn_sig_key,
        .provenance = try cloneBoxBoundarySpan(allocator, erase.provenance),
        .code_plan = .{ .materialized_by_lowering = .{ .finite_set_adapter = erase.adapter } },
        .result_ty = erase.result_ty,
        .capture = if (erase.adapter.erased_fn_sig_key.capture_ty == null)
            .none
        else
            .{ .finite_callable_set_value = try finiteCallableResultPlan(
                allocator,
                artifact_sink,
                plans,
                value_context,
                callable_value,
                callable,
                erase.adapter.callable_set_key,
            ) },
        .executable_signature_payloads = try erasedPromotedSignaturePayloadsForFiniteSetAdapter(
            allocator,
            artifact_sink,
            value_context,
            erase,
        ),
    } });
}

fn alreadyErasedResultPlan(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    erased: repr.AlreadyErasedCallablePlan,
) Allocator.Error!checked_artifact.CallableResultPlanId {
    return try plans.appendCallableResult(allocator, .{ .erased = .{
        .source_fn_ty = erased.sig_key.source_fn_ty,
        .sig_key = erased.sig_key,
        .provenance = try cloneBoxBoundarySpan(allocator, erased.provenance),
        .code_plan = .read_from_interpreted_erased_value,
        .capture = try alreadyErasedCapturePlan(allocator, artifact_sink, plans, value_context, erased),
        .result_ty = erased.result_ty,
        .executable_signature_payloads = try erasedPromotedSignaturePayloadsForAlreadyErased(
            allocator,
            artifact_sink,
            value_context,
            erased,
        ),
    } });
}

fn alreadyErasedCapturePlan(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
    erased: repr.AlreadyErasedCallablePlan,
) Allocator.Error!checked_artifact.ErasedCaptureReificationPlan {
    return switch (erased.capture) {
        .none => blk: {
            if (erased.sig_key.capture_ty != null) {
                checkedPipelineInvariant("already-erased callable capture plan is none but signature has hidden capture type");
            }
            break :blk .none;
        },
        .zero_sized_ty => blk: {
            const capture_ty = erased.sig_key.capture_ty orelse {
                checkedPipelineInvariant("already-erased zero-sized capture has no hidden capture type");
            };
            break :blk .{ .zero_sized_typed = capture_ty };
        },
        .value => |capture_value| blk: {
            if (erased.sig_key.capture_ty == null) {
                checkedPipelineInvariant("already-erased capture value has no hidden capture type");
            }
            const capture_info = value_context.value_store.values.items[@intFromEnum(capture_value)];
            var capture_builder = CaptureSlotPlanBuilder{
                .allocator = allocator,
                .artifact = artifact_sink,
                .plans = plans,
                .value_context = value_context,
                .active = std.AutoHashMap(CapturePlanKey, checked_artifact.CaptureSlotReificationPlanId).init(allocator),
            };
            defer capture_builder.deinit();
            break :blk .{ .whole_hidden_capture_value = .{
                .source_ty = capture_info.source_ty,
                .plan = try capture_builder.planFor(capture_info.source_ty, capture_value),
            } };
        },
    };
}

fn erasedCapturePlanForProcValue(
    allocator: Allocator,
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    plans: *checked_artifact.CompileTimePlanStore,
    value_context: ConstValueContext,
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

    const slot_plans = try allocator.alloc(checked_artifact.ErasedCaptureSlotReificationRef, erase.capture_slots.len);
    errdefer allocator.free(slot_plans);
    var seen = try allocator.alloc(bool, erase.capture_slots.len);
    defer allocator.free(seen);
    @memset(seen, false);

    const target_instance = value_context.solved.proc_instances.items[@intFromEnum(erase.target_instance)];
    const target_context = constValueContextForInstance(value_context.solved, target_instance);
    const target_captures = target_context.value_store.sliceValueSpan(target_instance.public_roots.captures);
    if (target_captures.len != erase.capture_slots.len) {
        checkedPipelineInvariant("erased proc-value result capture slot count differs from target capture arity");
    }

    var capture_builder = CaptureSlotPlanBuilder{
        .allocator = allocator,
        .artifact = artifact_sink,
        .plans = plans,
        .value_context = target_context,
        .active = std.AutoHashMap(CapturePlanKey, checked_artifact.CaptureSlotReificationPlanId).init(allocator),
    };
    defer capture_builder.deinit();

    for (erase.capture_slots) |slot| {
        const slot_index: usize = @intCast(slot.slot);
        if (slot_index >= target_captures.len) {
            checkedPipelineInvariant("erased proc-value result capture slot exceeded target capture arity");
        }
        if (seen[slot_index]) {
            checkedPipelineInvariant("erased proc-value result capture slot was duplicated");
        }
        const target_capture = target_captures[slot_index];
        const target_capture_info = target_context.value_store.values.items[@intFromEnum(target_capture)];
        const target_endpoint = target_capture_info.exec_ty orelse {
            checkedPipelineInvariant("erased proc-value result target capture has no published executable endpoint");
        };
        if (!repr.canonicalExecValueTypeKeyEql(target_endpoint.key, slot.exec_value_ty)) {
            checkedPipelineInvariant("erased proc-value result target capture executable key differs from capture slot key");
        }
        slot_plans[slot_index] = .{
            .source_ty = target_capture_info.source_ty,
            .plan = try capture_builder.planFor(target_capture_info.source_ty, target_capture),
        };
        seen[slot_index] = true;
    }
    for (seen) |was_seen| {
        if (!was_seen) checkedPipelineInvariant("erased proc-value result did not publish every capture slot");
    }
    return .{ .proc_capture_tuple = slot_plans };
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
    exec_ty: canonical.CanonicalExecValueTypeKey,
    value_store: u32,
    value_info: u32,
    has_exec_ty: bool,

    const none = std.math.maxInt(u32);

    fn from(
        source_ty: canonical.CanonicalTypeKey,
        value_store_id: repr.ValueInfoStoreId,
        value_info: ?repr.ValueInfoId,
    ) CapturePlanKey {
        return .{
            .source_ty = source_ty,
            .exec_ty = .{},
            .value_store = @intFromEnum(value_store_id),
            .value_info = if (value_info) |info| @intFromEnum(info) else none,
            .has_exec_ty = false,
        };
    }

    fn fromExecutable(
        source_ty: canonical.CanonicalTypeKey,
        value_store_id: repr.ValueInfoStoreId,
        exec_ty: canonical.CanonicalExecValueTypeKey,
    ) CapturePlanKey {
        return .{
            .source_ty = source_ty,
            .exec_ty = exec_ty,
            .value_store = @intFromEnum(value_store_id),
            .value_info = none,
            .has_exec_ty = true,
        };
    }
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
        return try self.planForOptional(source_ty, value_info);
    }

    fn planForTypeOnly(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        return try self.planForOptional(source_ty, null);
    }

    fn planForInContext(
        self: *CaptureSlotPlanBuilder,
        value_context: ConstValueContext,
        source_ty: canonical.CanonicalTypeKey,
        value_info: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const previous = self.value_context;
        self.value_context = value_context;
        defer self.value_context = previous;
        return try self.planFor(source_ty, value_info);
    }

    fn planForOptional(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        value_info: ?repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        return try self.planForOptionalExecutable(source_ty, value_info, null);
    }

    fn planForOptionalExecutable(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        value_info: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const resolved_value = if (value_info) |info| self.resolveValueInfoId(info) else null;
        const key = if (resolved_value == null and exec_ty != null)
            CapturePlanKey.fromExecutable(source_ty, self.value_context.value_store_id, exec_ty.?)
        else
            CapturePlanKey.from(source_ty, self.value_context.value_store_id, resolved_value);
        if (self.active.get(key)) |active| {
            const recursive = try self.plans.reserveCaptureSlot(self.allocator);
            self.plans.fillCaptureSlot(recursive, .{ .recursive_ref = active });
            return recursive;
        }

        const id = try self.plans.reserveCaptureSlot(self.allocator);
        try self.active.put(key, id);
        errdefer _ = self.active.remove(key);

        const plan = try self.buildPlan(source_ty, resolved_value, if (resolved_value == null) exec_ty else null);
        self.plans.fillCaptureSlot(id, plan);
        _ = self.active.remove(key);
        return id;
    }

    fn buildPlan(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        const checked_ty = self.artifact.checked_types.rootForKey(source_ty) orelse {
            checkedPipelineInvariant("capture slot source type was not published in checked type store");
        };
        if (value_info_id) |info_id| {
            const info = self.valueInfo(info_id);
            if (info.callable != null) {
                return .{ .callable_leaf = try self.callableLeafPlan(info_id) };
            }
        }

        return switch (self.artifact.checked_types.payloads[@intFromEnum(checked_ty)]) {
            .pending => checkedPipelineInvariant("capture slot planning reached pending checked type"),
            .flex, .rigid => checkedPipelineInvariant("capture slot planning reached unresolved type variable"),
            .function => if (value_info_id) |info_id|
                .{ .callable_leaf = try self.callableLeafPlan(info_id) }
            else if (exec_ty) |key|
                try self.callablePlanForExecutableKey(source_ty, key)
            else
                .{ .callable_schema = self.artifact.checked_types.roots[@intFromEnum(checked_ty)].key },
            .empty_record => .{ .record = &.{} },
            .record => |record| .{ .record = try self.recordFields(record.fields, value_info_id, exec_ty) },
            .record_unbound => |fields| .{ .record = try self.recordFields(fields, value_info_id, exec_ty) },
            .tuple => |items| .{ .tuple = try self.tupleItems(items, value_info_id, exec_ty) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagVariants(tag_union.tags, value_info_id, exec_ty) },
            .empty_tag_union => checkedPipelineInvariant("capture slot planning reached empty tag union"),
            .alias => |alias| .{ .nominal = .{
                .nominal = .{
                    .module_name = alias.origin_module,
                    .type_name = alias.name,
                },
                .backing = try self.planForOptionalExecutable(
                    self.artifact.checked_types.roots[@intFromEnum(alias.backing)].key,
                    value_info_id,
                    self.nominalBackingExecutableKey(exec_ty),
                ),
            } },
            .nominal => |nominal| try self.nominalPlan(checked_ty, source_ty, nominal, value_info_id, exec_ty),
        };
    }

    fn finiteCallableResultPlanForValue(
        self: *CaptureSlotPlanBuilder,
        callable_value: repr.ValueInfoId,
        callable: repr.CallableValueInfo,
        key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!checked_artifact.CallableResultPlanId {
        const representation_store = self.value_context.representation_store;
        const descriptor = representation_store.callableSetDescriptor(key) orelse {
            checkedPipelineInvariant("finite compile-time callable result has no descriptor");
        };
        if (descriptor.members.len == 0) {
            checkedPipelineInvariant("finite compile-time callable result descriptor has no members");
        }

        const source_fn_ty = descriptor.members[0].proc_value.source_fn_ty;
        const members = try self.allocator.alloc(checked_artifact.CallableResultMemberPlan, descriptor.members.len);
        errdefer self.allocator.free(members);

        for (descriptor.members, 0..) |member, i| {
            if (!std.mem.eql(u8, &member.proc_value.source_fn_ty.bytes, &source_fn_ty.bytes)) {
                checkedPipelineInvariant("finite compile-time callable result descriptor mixes source function types");
            }
            if (member.capture_slots.len != 0) {
                const slot_plans = try self.allocator.alloc(checked_artifact.CaptureSlotReificationPlanId, member.capture_slots.len);
                errdefer self.allocator.free(slot_plans);
                const construction_id = callable.construction_plan;
                const construction = if (construction_id) |id|
                    representation_store.callableConstructionPlan(id)
                else
                    null;
                if (construction) |constructed| {
                    if (constructed.result != callable_value) {
                        checkedPipelineInvariant("captured finite compile-time callable result construction is attached to a different value");
                    }
                    if (constructed.selected_member == member.member) {
                        try self.fillMemberCapturePlansFromConstruction(member, construction_id.?, constructed, slot_plans);
                        members[i] = .{
                            .member = member.member,
                            .capture_slots = slot_plans,
                        };
                        continue;
                    }
                }
                try self.fillMemberCapturePlansFromTargetProc(member, slot_plans);
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

        return try self.plans.appendCallableResult(self.allocator, .{ .finite = .{
            .source_fn_ty = source_fn_ty,
            .callable_set_key = key,
            .members = members,
        } });
    }

    fn fillMemberCapturePlansFromConstruction(
        self: *CaptureSlotPlanBuilder,
        member: repr.CanonicalCallableSetMember,
        construction_id: repr.CallableSetConstructionPlanId,
        constructed: repr.CallableSetConstructionPlan,
        slot_plans: []checked_artifact.CaptureSlotReificationPlanId,
    ) Allocator.Error!void {
        if (constructed.capture_values.len != member.capture_slots.len or slot_plans.len != member.capture_slots.len) {
            checkedPipelineInvariant("captured finite compile-time callable result capture arity disagrees with descriptor");
        }
        if (constructed.capture_transforms.len != member.capture_slots.len) {
            checkedPipelineInvariant("captured finite compile-time callable result capture transform arity disagrees with descriptor");
        }
        for (member.capture_slots, constructed.capture_transforms, 0..) |slot, transform_id, slot_i| {
            if (slot.slot != @as(u32, @intCast(slot_i))) {
                checkedPipelineInvariant("captured finite compile-time callable result capture slots are not canonical");
            }
            const transform = self.value_context.representation_store.valueTransformBoundary(transform_id);
            const capture = switch (transform.kind) {
                .capture_value => |capture_id| self.value_context.representation_store.captureBoundary(capture_id),
                else => checkedPipelineInvariant("captured finite compile-time callable result transform is not a capture transform"),
            };
            switch (capture.owner) {
                .callable_set_construction => |owner| {
                    if (owner.construction != construction_id or
                        !repr.callableSetKeyEql(owner.selected_member.callable_set_key, constructed.callable_set_key) or
                        owner.selected_member.member_index != constructed.selected_member)
                    {
                        checkedPipelineInvariant("captured finite compile-time callable result capture transform belongs to another construction");
                    }
                },
                else => checkedPipelineInvariant("captured finite compile-time callable result capture transform has wrong owner"),
            }
            if (capture.slot != slot.slot) {
                checkedPipelineInvariant("captured finite compile-time callable result capture transform slot disagrees with descriptor");
            }
            if (capture.source_capture_value != constructed.capture_values[slot_i]) {
                checkedPipelineInvariant("captured finite compile-time callable result capture transform source value disagrees with construction");
            }
            const target_instance = self.value_context.solved.proc_instances.items[@intFromEnum(capture.target_instance)];
            if (!canonical.mirProcedureRefEql(target_instance.proc, member.source_proc)) {
                checkedPipelineInvariant("captured finite compile-time callable result target capture instance disagrees with descriptor member");
            }
            const target_context = constValueContextForInstance(self.value_context.solved, target_instance);
            const target_info = target_context.value_store.values.items[@intFromEnum(capture.target_capture_value)];
            if (!repr.canonicalTypeKeyEql(target_info.source_ty, slot.source_ty)) {
                checkedPipelineInvariant("captured finite compile-time callable result target capture source type disagrees with descriptor");
            }
            if (target_info.exec_ty) |target_exec| {
                if (!repr.canonicalExecValueTypeKeyEql(target_exec.key, slot.exec_value_ty)) {
                    checkedPipelineInvariant("captured finite compile-time callable result target capture executable key disagrees with descriptor");
                }
            } else {
                checkedPipelineInvariant("captured finite compile-time callable result target capture has no executable endpoint");
            }
            slot_plans[slot_i] = try self.planForInContext(target_context, slot.source_ty, capture.target_capture_value);
        }
    }

    fn fillMemberCapturePlansFromTargetProc(
        self: *CaptureSlotPlanBuilder,
        member: repr.CanonicalCallableSetMember,
        slot_plans: []checked_artifact.CaptureSlotReificationPlanId,
    ) Allocator.Error!void {
        if (slot_plans.len != member.capture_slots.len) {
            checkedPipelineInvariant("finite executable callable result capture plan arity differs from descriptor");
        }
        const target_instance = procRepresentationInstanceForRoot(self.value_context.solved, member.source_proc);
        const target_context = constValueContextForInstance(self.value_context.solved, target_instance);
        const target_captures = target_context.value_store.sliceValueSpan(target_instance.public_roots.captures);
        if (target_captures.len != member.capture_slots.len) {
            checkedPipelineInvariant("finite executable callable result target capture arity differs from descriptor");
        }
        for (member.capture_slots, target_captures, 0..) |slot, target_capture, slot_i| {
            if (slot.slot != @as(u32, @intCast(slot_i))) {
                checkedPipelineInvariant("finite executable callable result capture slots are not canonical");
            }
            const target_info = target_context.value_store.values.items[@intFromEnum(target_capture)];
            if (!repr.canonicalTypeKeyEql(target_info.source_ty, slot.source_ty)) {
                checkedPipelineInvariant("finite executable callable result target capture source type disagrees with descriptor");
            }
            const target_endpoint = target_info.exec_ty orelse {
                checkedPipelineInvariant("finite executable callable result target capture has no published executable endpoint");
            };
            if (!repr.canonicalExecValueTypeKeyEql(target_endpoint.key, slot.exec_value_ty)) {
                checkedPipelineInvariant("finite executable callable result target capture executable key differs from descriptor");
            }
            slot_plans[slot_i] = try self.planForInContext(target_context, slot.source_ty, target_capture);
        }
    }

    fn callablePlanForExecutableKey(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        exec_ty: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        const payload = self.executablePayloadForKey(exec_ty);
        return switch (payload) {
            .callable_set => |callable_set| .{ .callable_leaf = try self.finiteCallableResultPlanForExecutableKey(source_ty, callable_set.key) },
            .vacant_callable_slot => .{ .callable_schema = source_ty },
            .erased_fn => checkedPipelineInvariant("capture slot executable schema reached erased function without explicit value metadata"),
            .recursive_ref => |ref| try self.callablePlanForExecutablePayloadRef(source_ty, ref),
            else => checkedPipelineInvariant("function capture slot executable key did not reference callable payload"),
        };
    }

    fn callablePlanForExecutablePayloadRef(
        self: *CaptureSlotPlanBuilder,
        source_ty: canonical.CanonicalTypeKey,
        ref: repr.SessionExecutableTypePayloadId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        const key = self.value_context.representation_store.session_executable_type_payloads.keyFor(ref);
        return try self.callablePlanForExecutableKey(source_ty, key);
    }

    fn finiteCallableResultPlanForExecutableKey(
        self: *CaptureSlotPlanBuilder,
        source_fn_ty: canonical.CanonicalTypeKey,
        key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!checked_artifact.CallableResultPlanId {
        const descriptor = self.value_context.representation_store.callableSetDescriptor(key) orelse {
            checkedPipelineInvariant("finite executable callable result has no descriptor");
        };
        if (descriptor.members.len == 0) {
            checkedPipelineInvariant("finite executable callable result descriptor has no members");
        }

        const members = try self.allocator.alloc(checked_artifact.CallableResultMemberPlan, descriptor.members.len);
        errdefer self.allocator.free(members);

        for (descriptor.members, 0..) |member, i| {
            if (!std.mem.eql(u8, &member.proc_value.source_fn_ty.bytes, &source_fn_ty.bytes)) {
                checkedPipelineInvariant("finite executable callable result descriptor source function type disagrees with payload");
            }
            const slot_plans: []const checked_artifact.CaptureSlotReificationPlanId = if (member.capture_slots.len == 0)
                &.{}
            else blk: {
                const out = try self.allocator.alloc(checked_artifact.CaptureSlotReificationPlanId, member.capture_slots.len);
                errdefer self.allocator.free(out);
                try self.fillMemberCapturePlansFromTargetProc(member, out);
                break :blk out;
            };
            members[i] = .{
                .member = member.member,
                .capture_slots = slot_plans,
            };
        }

        return try self.plans.appendCallableResult(self.allocator, .{ .finite = .{
            .source_fn_ty = source_fn_ty,
            .callable_set_key = key,
            .members = members,
        } });
    }

    fn callableLeafPlan(
        self: *CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CallableResultPlanId {
        const info = self.valueInfo(value_info_id);
        const callable = info.callable orelse checkedPipelineInvariant("function-typed capture leaf has no callable metadata");
        const emission = self.value_context.representation_store.callableEmissionPlan(callable.emission_plan);
        return switch (emission) {
            .finite => |key| try self.finiteCallableResultPlanForValue(value_info_id, callable, key),
            .already_erased => |erased| try alreadyErasedResultPlan(
                self.allocator,
                self.artifact,
                self.plans,
                self.value_context,
                erased,
            ),
            .erase_proc_value => |erase| try erasedProcValueResultPlan(
                self.allocator,
                self.artifact,
                self.plans,
                self.value_context,
                callable,
                erase,
            ),
            .erase_finite_set => |erase| try erasedFiniteSetResultPlan(
                self.allocator,
                self.artifact,
                self.plans,
                self.value_context,
                value_info_id,
                callable,
                erase,
            ),
        };
    }

    fn serializableLeafPlan(
        self: *CaptureSlotPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        source_ty: canonical.CanonicalTypeKey,
        value_info_id: ?repr.ValueInfoId,
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
        const const_value_context: ?ConstValueContext = if (value_info_id == null) null else self.value_context;
        const reification_plan = try const_builder.planFor(
            checked_ty,
            const_value_context,
            value_info_id,
        );
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
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
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
                    .elem = try self.listElemPlan(nominalArg(nominal, 0), value_info_id, exec_ty),
                } },
                .box => try self.boxPlan(checked_ty, source_ty, nominalArg(nominal, 0), value_info_id, exec_ty),
                .bool => try self.planForOptionalExecutable(
                    self.artifact.checked_types.roots[@intFromEnum(nominal.backing)].key,
                    value_info_id,
                    self.nominalBackingExecutableKey(exec_ty),
                ),
            };
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = nominal.origin_module,
                .type_name = nominal.name,
            },
            .backing = try self.planForOptionalExecutable(
                self.artifact.checked_types.roots[@intFromEnum(nominal.backing)].key,
                value_info_id,
                self.nominalBackingExecutableKey(exec_ty),
            ),
        } };
    }

    fn boxPlan(
        self: *CaptureSlotPlanBuilder,
        checked_ty: checked_artifact.CheckedTypeId,
        source_ty: canonical.CanonicalTypeKey,
        payload_ty: checked_artifact.CheckedTypeId,
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlan {
        if (value_info_id) |info_id| {
            if (self.boxPayloadNeedsExecutableMaterialization(info_id)) {
                return try self.serializableLeafPlan(checked_ty, source_ty, info_id);
            }
            return .{ .box = try self.boxPayloadPlan(payload_ty, info_id) };
        }
        return .{ .box = try self.planForOptionalExecutable(
            self.artifact.checked_types.roots[@intFromEnum(payload_ty)].key,
            null,
            self.boxPayloadExecutableKey(exec_ty),
        ) };
    }

    fn recordFields(
        self: *CaptureSlotPlanBuilder,
        fields: []const checked_artifact.CheckedRecordField,
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error![]const checked_artifact.CaptureRecordFieldPlan {
        if (fields.len == 0) return &.{};
        const plans_out = try self.allocator.alloc(checked_artifact.CaptureRecordFieldPlan, fields.len);
        errdefer self.allocator.free(plans_out);
        for (fields, 0..) |field, i| {
            const child = self.recordFieldValue(value_info_id, field.name);
            const child_exec = if (child == null) self.recordFieldExecutableKey(exec_ty, field.name) else null;
            plans_out[i] = .{
                .field = field.name,
                .value = try self.planForOptionalExecutable(
                    self.artifact.checked_types.roots[@intFromEnum(field.ty)].key,
                    child,
                    child_exec,
                ),
            };
        }
        return plans_out;
    }

    fn tupleItems(
        self: *CaptureSlotPlanBuilder,
        items: []const checked_artifact.CheckedTypeId,
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error![]const checked_artifact.CaptureTupleElemPlan {
        if (items.len == 0) return &.{};
        const plans_out = try self.allocator.alloc(checked_artifact.CaptureTupleElemPlan, items.len);
        errdefer self.allocator.free(plans_out);
        for (items, 0..) |item, i| {
            const child = self.tupleElemValue(value_info_id, @intCast(i));
            const child_exec = if (child == null) self.tupleElemExecutableKey(exec_ty, @intCast(i)) else null;
            plans_out[i] = .{
                .index = @intCast(i),
                .value = try self.planForOptionalExecutable(
                    self.artifact.checked_types.roots[@intFromEnum(item)].key,
                    child,
                    child_exec,
                ),
            };
        }
        return plans_out;
    }

    fn tagVariants(
        self: *CaptureSlotPlanBuilder,
        tags: []const checked_artifact.CheckedTag,
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
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
                const child = self.tagPayloadValue(value_info_id, tag.name, @intCast(arg_i));
                const child_exec = if (child == null) self.tagPayloadExecutableKey(exec_ty, tag.name, @intCast(arg_i)) else null;
                payloads[arg_i] = .{
                    .index = @intCast(arg_i),
                    .value = try self.planForOptionalExecutable(
                        self.artifact.checked_types.roots[@intFromEnum(arg_ty)].key,
                        child,
                        child_exec,
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
        value_info_id: ?repr.ValueInfoId,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const child = self.listRepresentativeValue(value_info_id);
        const child_exec = if (child == null) self.listElemExecutableKey(exec_ty) else null;
        return try self.planForOptionalExecutable(
            self.artifact.checked_types.roots[@intFromEnum(elem_ty)].key,
            child,
            child_exec,
        );
    }

    fn boxPayloadPlan(
        self: *CaptureSlotPlanBuilder,
        payload_ty: checked_artifact.CheckedTypeId,
        value_info_id: repr.ValueInfoId,
    ) Allocator.Error!checked_artifact.CaptureSlotReificationPlanId {
        const info = self.valueInfo(value_info_id);
        const boxed = info.boxed orelse checkedPipelineInvariant("Box(T) capture had no boxed metadata");
        const payload_value = self.valueForRoot(boxed.payload_root) orelse {
            checkedPipelineInvariant("Box(T) capture payload root had no value-flow metadata");
        };
        return try self.planFor(
            self.artifact.checked_types.roots[@intFromEnum(payload_ty)].key,
            payload_value,
        );
    }

    fn boxPayloadNeedsExecutableMaterialization(
        self: *const CaptureSlotPlanBuilder,
        value_info_id: repr.ValueInfoId,
    ) bool {
        const info = self.valueInfo(value_info_id);
        const boxed = info.boxed orelse checkedPipelineInvariant("Box(T) capture had no boxed metadata");
        const boundary_id = boxed.boundary orelse return false;
        const boundary = self.value_context.representation_store.box_boundaries[@intFromEnum(boundary_id)];
        return switch (boundary.payload_plan) {
            .unchanged => false,
            .function_erased,
            .record,
            .tag_union,
            .tuple,
            .list,
            .nested_box,
            .nominal,
            => true,
        };
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
        value_info_id: ?repr.ValueInfoId,
        label: canonical.RecordFieldLabelId,
    ) ?repr.ValueInfoId {
        const info_id = value_info_id orelse return null;
        const info = self.valueInfo(info_id);
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
        value_info_id: ?repr.ValueInfoId,
        index: u32,
    ) ?repr.ValueInfoId {
        const info_id = value_info_id orelse return null;
        const info = self.valueInfo(info_id);
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
        value_info_id: ?repr.ValueInfoId,
        tag_label: canonical.TagLabelId,
        payload_index: u32,
    ) ?repr.ValueInfoId {
        const info_id = value_info_id orelse return null;
        const info = self.valueInfo(info_id);
        const aggregate = info.aggregate orelse checkedPipelineInvariant("tag capture had no aggregate metadata");
        const tag = switch (aggregate) {
            .tag => |tag| tag,
            else => checkedPipelineInvariant("tag capture value had non-tag aggregate metadata"),
        };
        const active_tag = self.value_context.row_shapes.tag(tag.tag);
        if (active_tag.label != tag_label) {
            return null;
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
        value_info_id: ?repr.ValueInfoId,
    ) ?repr.ValueInfoId {
        const info_id = value_info_id orelse return null;
        const info = self.valueInfo(info_id);
        const aggregate = info.aggregate orelse checkedPipelineInvariant("List(T) capture had no aggregate metadata");
        const list = switch (aggregate) {
            .list => |list| list,
            else => checkedPipelineInvariant("List(T) capture value had non-list aggregate metadata"),
        };
        if (list.elems.len == 0) {
            return null;
        }
        return list.elems[0];
    }

    fn executablePayloadForKey(
        self: *const CaptureSlotPlanBuilder,
        key: canonical.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableTypePayload {
        const ref = self.value_context.representation_store.session_executable_type_payloads.refForKey(key) orelse {
            checkedPipelineInvariant("capture slot executable key has no published payload");
        };
        return self.value_context.representation_store.session_executable_type_payloads.get(ref.payload);
    }

    fn executablePayloadForRef(
        self: *const CaptureSlotPlanBuilder,
        ref: repr.SessionExecutableTypePayloadId,
    ) repr.SessionExecutableTypePayload {
        return self.value_context.representation_store.session_executable_type_payloads.get(ref);
    }

    fn recordFieldExecutableKey(
        self: *const CaptureSlotPlanBuilder,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
        label: canonical.RecordFieldLabelId,
    ) ?canonical.CanonicalExecValueTypeKey {
        const key = exec_ty orelse return null;
        return self.recordFieldExecutableKeyFromPayload(self.executablePayloadForKey(key), label);
    }

    fn recordFieldExecutableKeyFromPayload(
        self: *const CaptureSlotPlanBuilder,
        payload: repr.SessionExecutableTypePayload,
        label: canonical.RecordFieldLabelId,
    ) ?canonical.CanonicalExecValueTypeKey {
        return switch (payload) {
            .record => |record| blk: {
                for (record.fields) |field| {
                    if (self.value_context.row_shapes.recordField(field.field).label == label) break :blk field.key;
                }
                checkedPipelineInvariant("record capture executable payload omitted a checked field label");
            },
            .recursive_ref => |ref| self.recordFieldExecutableKeyFromPayload(self.executablePayloadForRef(ref), label),
            else => checkedPipelineInvariant("record capture executable key did not reference a record payload"),
        };
    }

    fn tupleElemExecutableKey(
        self: *const CaptureSlotPlanBuilder,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
        index: u32,
    ) ?canonical.CanonicalExecValueTypeKey {
        const key = exec_ty orelse return null;
        return self.tupleElemExecutableKeyFromPayload(self.executablePayloadForKey(key), index);
    }

    fn tupleElemExecutableKeyFromPayload(
        self: *const CaptureSlotPlanBuilder,
        payload: repr.SessionExecutableTypePayload,
        index: u32,
    ) ?canonical.CanonicalExecValueTypeKey {
        return switch (payload) {
            .tuple => |items| blk: {
                for (items) |item| {
                    if (item.index == index) break :blk item.key;
                }
                checkedPipelineInvariant("tuple capture executable payload omitted a checked element");
            },
            .recursive_ref => |ref| self.tupleElemExecutableKeyFromPayload(self.executablePayloadForRef(ref), index),
            else => checkedPipelineInvariant("tuple capture executable key did not reference a tuple payload"),
        };
    }

    fn tagPayloadExecutableKey(
        self: *const CaptureSlotPlanBuilder,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
        tag_label: canonical.TagLabelId,
        payload_index: u32,
    ) ?canonical.CanonicalExecValueTypeKey {
        const key = exec_ty orelse return null;
        return self.tagPayloadExecutableKeyFromPayload(self.executablePayloadForKey(key), tag_label, payload_index);
    }

    fn tagPayloadExecutableKeyFromPayload(
        self: *const CaptureSlotPlanBuilder,
        payload: repr.SessionExecutableTypePayload,
        tag_label: canonical.TagLabelId,
        payload_index: u32,
    ) ?canonical.CanonicalExecValueTypeKey {
        return switch (payload) {
            .tag_union => |tag_union| blk: {
                for (tag_union.variants) |variant| {
                    const tag = self.value_context.row_shapes.tag(variant.tag);
                    if (tag.label != tag_label) continue;
                    const payload_ids = self.value_context.row_shapes.tagPayloads(variant.tag);
                    if (payload_index >= payload_ids.len) {
                        checkedPipelineInvariant("tag capture executable payload index exceeded finalized payload arity");
                    }
                    const payload_id = payload_ids[payload_index];
                    for (variant.payloads) |item| {
                        if (item.payload == payload_id) break :blk item.key;
                    }
                    checkedPipelineInvariant("tag capture executable payload omitted a checked payload");
                }
                break :blk null;
            },
            .recursive_ref => |ref| self.tagPayloadExecutableKeyFromPayload(self.executablePayloadForRef(ref), tag_label, payload_index),
            else => checkedPipelineInvariant("tag capture executable key did not reference a tag-union payload"),
        };
    }

    fn listElemExecutableKey(
        self: *const CaptureSlotPlanBuilder,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) ?canonical.CanonicalExecValueTypeKey {
        const key = exec_ty orelse return null;
        return self.listElemExecutableKeyFromPayload(self.executablePayloadForKey(key));
    }

    fn listElemExecutableKeyFromPayload(
        self: *const CaptureSlotPlanBuilder,
        payload: repr.SessionExecutableTypePayload,
    ) ?canonical.CanonicalExecValueTypeKey {
        return switch (payload) {
            .list => |list| list.key,
            .recursive_ref => |ref| self.listElemExecutableKeyFromPayload(self.executablePayloadForRef(ref)),
            else => checkedPipelineInvariant("List(T) capture executable key did not reference a list payload"),
        };
    }

    fn boxPayloadExecutableKey(
        self: *const CaptureSlotPlanBuilder,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) ?canonical.CanonicalExecValueTypeKey {
        const key = exec_ty orelse return null;
        return self.boxPayloadExecutableKeyFromPayload(self.executablePayloadForKey(key));
    }

    fn boxPayloadExecutableKeyFromPayload(
        self: *const CaptureSlotPlanBuilder,
        payload: repr.SessionExecutableTypePayload,
    ) ?canonical.CanonicalExecValueTypeKey {
        return switch (payload) {
            .box => |box| box.key,
            .recursive_ref => |ref| self.boxPayloadExecutableKeyFromPayload(self.executablePayloadForRef(ref)),
            else => checkedPipelineInvariant("Box(T) capture executable key did not reference a box payload"),
        };
    }

    fn nominalBackingExecutableKey(
        self: *const CaptureSlotPlanBuilder,
        exec_ty: ?canonical.CanonicalExecValueTypeKey,
    ) ?canonical.CanonicalExecValueTypeKey {
        const key = exec_ty orelse return null;
        return self.nominalBackingExecutableKeyFromPayload(key, self.executablePayloadForKey(key));
    }

    fn nominalBackingExecutableKeyFromPayload(
        self: *const CaptureSlotPlanBuilder,
        original_key: canonical.CanonicalExecValueTypeKey,
        payload: repr.SessionExecutableTypePayload,
    ) ?canonical.CanonicalExecValueTypeKey {
        return switch (payload) {
            .nominal => |nominal| nominal.backing_key,
            .recursive_ref => |ref| self.nominalBackingExecutableKeyFromPayload(
                self.value_context.representation_store.session_executable_type_payloads.keyFor(ref),
                self.executablePayloadForRef(ref),
            ),
            else => original_key,
        };
    }

    fn valueInfo(self: *const CaptureSlotPlanBuilder, value_info_id: repr.ValueInfoId) repr.ValueInfo {
        return self.value_context.value_store.values.items[@intFromEnum(self.resolveValueInfoId(value_info_id))];
    }

    fn resolveValueInfoId(self: *const CaptureSlotPlanBuilder, value_info_id: repr.ValueInfoId) repr.ValueInfoId {
        var current = value_info_id;
        var remaining = self.value_context.value_store.values.items.len;
        while (remaining != 0) : (remaining -= 1) {
            const info = self.value_context.value_store.values.items[@intFromEnum(current)];
            current = info.value_alias_source orelse return current;
        }
        checkedPipelineInvariant("capture slot value alias chain is cyclic");
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
        const raw_info_id = value_info orelse checkedPipelineInvariant("callable constant leaf requires lambda-solved value metadata");
        const info_id = self.resolveValueInfoId(context, raw_info_id);
        const info = context.value_store.values.items[@intFromEnum(info_id)];
        const callable = info.callable orelse checkedPipelineInvariant("function-typed constant leaf has no callable metadata");
        const emission = context.representation_store.callableEmissionPlan(callable.emission_plan);
        return switch (emission) {
            .finite => |key| .{ .finite = try finiteCallableResultPlan(
                self.allocator,
                self.artifactSink(),
                self.plans,
                context,
                info_id,
                callable,
                key,
            ) },
            .already_erased => |erased| .{ .erased_boxed = try alreadyErasedResultPlan(
                self.allocator,
                self.artifactSink(),
                self.plans,
                context,
                erased,
            ) },
            .erase_proc_value => |erase| .{ .erased_boxed = try erasedProcValueResultPlan(
                self.allocator,
                self.artifactSink(),
                self.plans,
                context,
                callable,
                erase,
            ) },
            .erase_finite_set => |erase| .{ .erased_boxed = try erasedFiniteSetResultPlan(
                self.allocator,
                self.artifactSink(),
                self.plans,
                context,
                info_id,
                callable,
                erase,
            ) },
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
        const context = value_context orelse return null;
        const info_id = value_info orelse return null;
        return context.value_store.values.items[@intFromEnum(self.resolveValueInfoId(context, info_id))];
    }

    fn resolveValueInfoId(
        self: *const ConstGraphPlanBuilder,
        context: ConstValueContext,
        value_info: repr.ValueInfoId,
    ) repr.ValueInfoId {
        _ = self;
        var current = value_info;
        var remaining = context.value_store.values.items.len;
        while (remaining != 0) : (remaining -= 1) {
            const info = context.value_store.values.items[@intFromEnum(current)];
            current = info.value_alias_source orelse return current;
        }
        checkedPipelineInvariant("const graph value alias chain is cyclic");
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
        const first_info = context.value_store.values.items[@intFromEnum(list.elems[0])];
        const first_endpoint = first_info.exec_ty orelse {
            checkedPipelineInvariant("List(T) constant element has no published executable endpoint");
        };
        for (list.elems[1..]) |elem| {
            const elem_info = context.value_store.values.items[@intFromEnum(elem)];
            const elem_endpoint = elem_info.exec_ty orelse {
                checkedPipelineInvariant("List(T) constant element has no published executable endpoint");
            };
            if (!repr.canonicalExecValueTypeKeyEql(first_endpoint.key, elem_endpoint.key)) {
                checkedPipelineInvariant("List(T) constant elements have different executable representations");
            }
        }
        return list.elems[0];
    }

    fn valueForRoot(
        self: *const ConstGraphPlanBuilder,
        context: ConstValueContext,
        root: repr.RepRootId,
    ) ?repr.ValueInfoId {
        _ = self;
        for (context.value_store.values.items, 0..) |value, i| {
            if (value.root == root) return @enumFromInt(@as(u32, @intCast(i)));
        }
        return null;
    }

    fn boxPayloadValue(
        self: *const ConstGraphPlanBuilder,
        value_context: ?ConstValueContext,
        value_info: ?repr.ValueInfoId,
    ) ?repr.ValueInfoId {
        const context = value_context orelse return null;
        const info = self.valueInfo(value_context, value_info) orelse return null;
        const boxed = info.boxed orelse checkedPipelineInvariant("Box(T) constant value had no boxed metadata");
        if (boxed.payload_value) |payload| return payload;
        return self.valueForRoot(context, boxed.payload_root) orelse {
            checkedPipelineInvariant("Box(T) constant payload root had no value-flow metadata");
        };
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

fn entrypointsForPurpose(
    allocator: Allocator,
    selected_roots: []const checked_artifact.RootRequest,
    requests: RootRequestSet,
) Allocator.Error![]checked_artifact.LoweringEntrypointRequest {
    var entrypoints = std.ArrayList(checked_artifact.LoweringEntrypointRequest).empty;
    errdefer entrypoints.deinit(allocator);

    for (selected_roots) |root| {
        try entrypoints.append(allocator, .{ .root = root });
    }

    switch (requests.purpose) {
        .runtime => {
            if (requests.compile_time_requests.len != 0) {
                checkedPipelineInvariant("runtime lowering received compile-time evaluation requests");
            }
        },
        .compile_time => {
            for (requests.compile_time_requests) |request| {
                try entrypoints.append(allocator, switch (request) {
                    .local_root => |root| .{ .root = root },
                    .const_instance => |const_request| .{ .const_instance = const_request },
                    .callable_binding_instance => |callable_request| .{ .callable_binding_instance = callable_request },
                });
            }
        },
    }

    return try entrypoints.toOwnedSlice(allocator);
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
