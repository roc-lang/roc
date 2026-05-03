//! Lambda-solved MIR construction state.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const Lifted = @import("../lifted/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const repr = @import("representation.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

pub const Proc = struct {
    proc: canonical.MirProcedureRef,
    body: Ast.DefId,
    representation_instance: repr.ProcRepresentationInstanceId,
};

pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    executable_synthetic_procs: std.ArrayList(ids.ExecutableSyntheticProc),
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    root_metadata: std.ArrayList(ids.RootMetadata),
    solve_sessions: std.ArrayList(repr.RepresentationSolveSession),
    proc_instances: std.ArrayList(repr.ProcRepresentationInstance),
    value_stores: std.ArrayList(repr.ValueInfoStore),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .executable_synthetic_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
            .solve_sessions = .empty,
            .proc_instances = .empty,
            .value_stores = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.value_stores.items) |*store| {
            store.deinit();
        }
        self.value_stores.deinit(self.allocator);
        for (self.proc_instances.items) |*instance| {
            repr.deinitProcRepresentationInstance(self.allocator, instance);
        }
        self.proc_instances.deinit(self.allocator);
        for (self.solve_sessions.items) |*session| {
            session.deinit();
        }
        self.solve_sessions.deinit(self.allocator);
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.executable_synthetic_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

pub fn run(allocator: Allocator, lifted: Lifted.Lift.Program) Allocator.Error!Program {
    var input = lifted;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.canonical_names;
    input.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.solve_sessions.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.proc_instances.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.value_stores.ensureTotalCapacity(allocator, input.procs.items.len);

    var type_importer = TypeImporter.init(allocator, &input.types, &program.types);
    defer type_importer.deinit();

    var proc_instance_map = std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId).init(allocator);
    defer proc_instance_map.deinit();
    try proc_instance_map.ensureTotalCapacity(@intCast(input.procs.items.len));

    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        program.solve_sessions.appendAssumeCapacity(.{
            .members = &.{},
            .representation_store = repr.RepresentationStore.init(allocator),
            .state = .building,
        });
        program.value_stores.appendAssumeCapacity(repr.ValueInfoStore.init(allocator));
        proc_instance_map.putAssumeCapacity(proc.proc, instance);
    }

    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        const session_id: repr.RepresentationSolveSessionId = @enumFromInt(@as(u32, @intCast(i)));
        const value_store_id: repr.ValueInfoStoreId = @enumFromInt(@as(u32, @intCast(i)));

        var solver = BodySolver{
            .allocator = allocator,
            .input = &input.ast,
            .output = &program.ast,
            .canonical_names = &program.canonical_names,
            .type_importer = &type_importer,
            .representation_store = &program.solve_sessions.items[i].representation_store,
            .value_store = &program.value_stores.items[i],
            .env = std.AutoHashMap(Ast.Symbol, repr.BindingInfoId).init(allocator),
            .expr_map = std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId).init(allocator),
            .instance = instance,
            .proc_instance_map = &proc_instance_map,
        };
        defer solver.deinit();

        const body = try solver.lowerDef(proc.body);
        const roots = solver.public_roots orelse lambdaInvariant("lambda-solved MIR built a procedure without public roots");
        const executable_key = try repr.executableSpecializationKeyForProc(
            allocator,
            &program.canonical_names,
            &program.types,
            &program.solve_sessions.items[i].representation_store,
            &program.value_stores.items[i],
            proc.proc,
            roots,
        );
        program.proc_instances.appendAssumeCapacity(.{
            .proc = proc.proc,
            .executable_specialization_key = executable_key,
            .solve_session = session_id,
            .value_store = value_store_id,
            .public_roots = roots,
        });
        program.procs.appendAssumeCapacity(.{
            .proc = proc.proc,
            .body = body,
            .representation_instance = instance,
        });
    }
    try finalizeValueTransformBoundaries(&program);
    for (program.solve_sessions.items) |*session| {
        session.representation_store.verifySealed();
        session.state = .sealed;
    }
    try program.executable_synthetic_procs.appendSlice(allocator, input.executable_synthetic_procs.items);
    try program.root_procs.appendSlice(allocator, input.root_procs.items);
    try program.root_metadata.appendSlice(allocator, input.root_metadata.items);

    input.deinit();
    return program;
}

fn finalizeValueTransformBoundaries(program: *Program) Allocator.Error!void {
    for (program.proc_instances.items, 0..) |*instance, raw_instance| {
        var finalizer = ValueTransformFinalizer{
            .allocator = program.allocator,
            .program = program,
            .instance_id = @enumFromInt(@as(u32, @intCast(raw_instance))),
            .instance = instance,
        };
        try finalizer.finalizeCallSites();
        try finalizer.finalizeJoins();
        try finalizer.finalizeReturns();
    }
}

const ValueTransformFinalizer = struct {
    allocator: Allocator,
    program: *Program,
    instance_id: repr.ProcRepresentationInstanceId,
    instance: *const repr.ProcRepresentationInstance,

    fn finalizeCallSites(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.call_sites.items, 0..) |*call_site, raw_call_site| {
            const call_site_id: repr.CallSiteInfoId = @enumFromInt(@as(u32, @intCast(raw_call_site)));
            switch (call_site.dispatch) {
                .call_proc => |target| try self.finalizeCallProc(call_site_id, call_site, target),
                .call_value_finite => |key| try self.finalizeCallValueFinite(call_site_id, call_site, key),
                .call_value_erased => |sig_key| try self.finalizeCallValueErased(call_site_id, call_site, sig_key),
            }
        }
    }

    fn finalizeJoins(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.joins.items, 0..) |*join, raw_join| {
            if (!join.input_transforms.isEmpty()) {
                lambdaInvariant("lambda-solved value transform finalization reached an already-finalized join");
            }
            const join_id: repr.JoinInfoId = @enumFromInt(@as(u32, @intCast(raw_join)));
            const inputs = value_store.sliceJoinInputSpan(join.inputs);
            const boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, inputs.len);
            defer self.allocator.free(boundaries);

            const result_to = try self.localEndpoint(join.result);
            for (inputs, 0..) |input, i| {
                const from = try self.localEndpoint(input.value);
                const transform = try self.appendIdentityTransform(from, result_to);
                boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = self.joinBoundaryKind(join_id, input.source),
                    .from_value = input.value,
                    .to_value = join.result,
                    .from_endpoint = from,
                    .to_endpoint = result_to,
                    .transform = transform,
                });
            }

            join.input_transforms = try self.valueStore().addValueTransformBoundarySpan(boundaries);
        }
    }

    fn finalizeReturns(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.returns.items, 0..) |*ret, raw_return| {
            if (ret.transform != null) {
                lambdaInvariant("lambda-solved value transform finalization reached an already-finalized return");
            }

            const from = try self.localEndpoint(ret.value);
            const to = try self.targetReturnEndpoint(self.instance_id, self.instance);
            const transform = try self.appendIdentityTransform(from, to);
            ret.transform = try self.representationStore().appendValueTransformBoundary(.{
                .kind = .{ .return_value = @enumFromInt(@as(u32, @intCast(raw_return))) },
                .from_value = ret.value,
                .to_value = self.instance.public_roots.ret,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }
    }

    fn joinBoundaryKind(
        self: *ValueTransformFinalizer,
        join_id: repr.JoinInfoId,
        source: repr.JoinInputSource,
    ) repr.ValueTransformBoundaryKind {
        _ = self;
        _ = join_id;
        return switch (source) {
            .if_branch => |if_branch| .{ .if_branch_result = .{
                .if_expr = if_branch.if_expr,
                .branch = if_branch.branch,
            } },
            .source_match_branch => |match_branch| .{ .source_match_branch_result = .{
                .match = match_branch.match,
                .branch = match_branch.branch,
                .alternative = match_branch.alternative,
            } },
            .loop_phi => |loop_phi| .{ .loop_phi = loop_phi },
        };
    }

    fn finalizeCallProc(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        target_id: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!void {
        self.verifyCallSiteUnfinalized(call_site);
        const args = self.valueStore().sliceValueSpan(call_site.args);
        const target_instance = self.procInstance(target_id);
        const target_params = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.params);
        if (args.len != target_params.len or args.len != target_instance.executable_specialization_key.exec_arg_tys.len) {
            lambdaInvariant("lambda-solved call_proc boundary finalization saw target arity mismatch");
        }

        const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, args.len);
        defer self.allocator.free(arg_boundaries);

        for (args, target_params, 0..) |arg, target_param, i| {
            const from = try self.localEndpoint(arg);
            const to = try self.targetParamEndpoint(target_id, target_instance, target_param, @intCast(i));
            const transform = try self.appendIdentityTransform(from, to);
            arg_boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = .{ .call_arg = .{
                    .call = call_site_id,
                    .arg_index = @intCast(i),
                } },
                .from_value = arg,
                .to_value = target_param,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }

        const result_from = try self.targetReturnEndpoint(target_id, target_instance);
        const result_to = try self.localEndpoint(call_site.result);
        const result_transform = try self.appendIdentityTransform(result_from, result_to);
        const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
            .kind = .{ .call_result = call_site_id },
            .from_value = target_instance.public_roots.ret,
            .to_value = call_site.result,
            .from_endpoint = result_from,
            .to_endpoint = result_to,
            .transform = result_transform,
        });

        call_site.arg_transforms = try self.valueStore().addValueTransformBoundarySpan(arg_boundaries);
        call_site.result_transform = result_boundary;
    }

    fn finalizeCallValueFinite(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        callable_set_key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!void {
        self.verifyCallSiteUnfinalized(call_site);
        const descriptor = self.representationStore().callableSetDescriptor(callable_set_key) orelse {
            lambdaInvariant("lambda-solved finite call boundary finalization referenced a missing callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite call boundary finalization saw empty callable-set descriptor");
        }

        const branch_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, descriptor.members.len);
        defer self.allocator.free(branch_boundaries);

        const result_to = try self.localEndpoint(call_site.result);
        for (descriptor.members, 0..) |member, i| {
            const target_id = self.procInstanceForSource(member.source_proc);
            const target_instance = self.procInstance(target_id);
            const result_from = try self.targetReturnEndpoint(target_id, target_instance);
            const result_transform = try self.appendIdentityTransform(result_from, result_to);
            branch_boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = .{ .callable_match_branch_result = .{
                    .call = call_site_id,
                    .member = .{
                        .callable_set_key = callable_set_key,
                        .member_index = member.member,
                    },
                } },
                .from_value = target_instance.public_roots.ret,
                .to_value = call_site.result,
                .from_endpoint = result_from,
                .to_endpoint = result_to,
                .transform = result_transform,
            });
        }

        call_site.branch_result_transforms = try self.valueStore().addValueTransformBoundarySpan(branch_boundaries);
    }

    fn finalizeCallValueErased(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        sig_key: repr.ErasedFnSigKey,
    ) Allocator.Error!void {
        self.verifyCallSiteUnfinalized(call_site);
        const abi = self.representationStore().erased_fn_abis.abiFor(sig_key.abi) orelse {
            lambdaInvariant("lambda-solved erased call boundary finalization referenced an unpublished ABI");
        };
        const args = self.valueStore().sliceValueSpan(call_site.args);
        if (args.len != abi.arg_exec_keys.len or args.len != abi.fixed_arity) {
            lambdaInvariant("lambda-solved erased call boundary finalization saw ABI arity mismatch");
        }

        const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, args.len);
        defer self.allocator.free(arg_boundaries);

        for (args, 0..) |arg, i| {
            const from = try self.localEndpoint(arg);
            const to = self.rawArgEndpoint(call_site_id, @intCast(i), from.logical_ty, abi.arg_exec_keys[i]);
            const transform = try self.appendIdentityTransform(from, to);
            arg_boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = .{ .call_arg = .{
                    .call = call_site_id,
                    .arg_index = @intCast(i),
                } },
                .from_value = arg,
                .to_value = arg,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }

        const result_to = try self.localEndpoint(call_site.result);
        const result_from = self.rawResultEndpoint(call_site_id, result_to.logical_ty, abi.ret_exec_key);
        const result_transform = try self.appendIdentityTransform(result_from, result_to);
        const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
            .kind = .{ .call_result = call_site_id },
            .from_value = call_site.result,
            .to_value = call_site.result,
            .from_endpoint = result_from,
            .to_endpoint = result_to,
            .transform = result_transform,
        });

        call_site.arg_transforms = try self.valueStore().addValueTransformBoundarySpan(arg_boundaries);
        call_site.result_transform = result_boundary;
    }

    fn verifyCallSiteUnfinalized(
        self: *ValueTransformFinalizer,
        call_site: *const repr.CallSiteInfo,
    ) void {
        _ = self;
        if (!call_site.arg_transforms.isEmpty() or
            !call_site.branch_result_transforms.isEmpty() or
            call_site.result_transform != null)
        {
            lambdaInvariant("lambda-solved value transform finalization reached an already-finalized call site");
        }
    }

    fn appendIdentityTransform(
        self: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (!repr.canonicalExecValueTypeKeyEql(from.exec_ty.key, to.exec_ty.key)) {
            lambdaInvariant("lambda-solved value transform finalization reached mismatched executable endpoint keys without an explicit transform plan");
        }
        const id = try self.representationStore().appendSessionExecutableValueTransform(.{
            .from = from,
            .to = to,
            .provenance = .none,
            .op = .identity,
        });
        return .{ .session = id };
    }

    fn localEndpoint(
        self: *ValueTransformFinalizer,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const info = self.valueStore().values.items[@intFromEnum(value)];
        return .{
            .owner = .{ .local_value = value },
            .logical_ty = info.logical_ty,
            .exec_ty = try repr.sessionExecutableTypeEndpointForValue(
                self.allocator,
                &self.program.canonical_names,
                &self.program.row_shapes,
                &self.program.types,
                self.representationStore(),
                self.valueStore(),
                value,
            ),
        };
    }

    fn targetParamEndpoint(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        index: u32,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        const exec_ty = try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_instance),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            target_value,
        );
        const arg_index: usize = @intCast(index);
        if (!repr.canonicalExecValueTypeKeyEql(exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
            lambdaInvariant("lambda-solved target procedure parameter endpoint key differs from target executable specialization");
        }
        return .{
            .owner = .{ .procedure_param = .{
                .instance = target_id,
                .index = index,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn targetReturnEndpoint(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_value = target_instance.public_roots.ret;
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        const exec_ty = try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_instance),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            target_value,
        );
        if (!repr.canonicalExecValueTypeKeyEql(exec_ty.key, target_instance.executable_specialization_key.exec_ret_ty)) {
            lambdaInvariant("lambda-solved target procedure return endpoint key differs from target executable specialization");
        }
        return .{
            .owner = .{ .procedure_return = target_id },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn rawArgEndpoint(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        index: u32,
        logical_ty: Type.TypeVarId,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        return .{
            .owner = .{ .call_raw_arg = .{
                .call = call_site_id,
                .index = index,
            } },
            .logical_ty = logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn rawResultEndpoint(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        logical_ty: Type.TypeVarId,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        return .{
            .owner = .{ .call_raw_result = call_site_id },
            .logical_ty = logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn sessionEndpointForPublishedKey(
        self: *ValueTransformFinalizer,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableTypeEndpoint {
        const payload = self.representationStore().session_executable_type_payloads.refForKey(key) orelse {
            lambdaInvariant("lambda-solved raw ABI endpoint key has no session executable type payload");
        };
        return .{
            .ty = payload,
            .key = key,
        };
    }

    fn procInstanceForSource(
        self: *ValueTransformFinalizer,
        proc: canonical.MirProcedureRef,
    ) repr.ProcRepresentationInstanceId {
        for (self.program.proc_instances.items, 0..) |instance, raw| {
            if (canonical.mirProcedureRefEql(instance.proc, proc)) {
                return @enumFromInt(@as(u32, @intCast(raw)));
            }
        }
        lambdaInvariant("lambda-solved finite call boundary finalization referenced missing member procedure");
    }

    fn procInstance(
        self: *ValueTransformFinalizer,
        id: repr.ProcRepresentationInstanceId,
    ) *const repr.ProcRepresentationInstance {
        const index = @intFromEnum(id);
        if (index >= self.program.proc_instances.items.len) {
            lambdaInvariant("lambda-solved value transform finalization referenced missing procedure instance");
        }
        return &self.program.proc_instances.items[index];
    }

    fn valueStore(self: *ValueTransformFinalizer) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(self.instance.value_store)];
    }

    fn valueStoreFor(self: *ValueTransformFinalizer, instance: *const repr.ProcRepresentationInstance) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(instance.value_store)];
    }

    fn representationStore(self: *ValueTransformFinalizer) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(self.instance.solve_session)].representation_store;
    }

    fn representationStoreFor(self: *ValueTransformFinalizer, instance: *const repr.ProcRepresentationInstance) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
    }
};

const TypeImporter = struct {
    allocator: Allocator,
    input: *const Lifted.Type.Store,
    output: *Type.Store,
    active: std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId),

    fn init(allocator: Allocator, input: *const Lifted.Type.Store, output: *Type.Store) TypeImporter {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .active = std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId).init(allocator),
        };
    }

    fn deinit(self: *TypeImporter) void {
        self.active.deinit();
    }

    fn importType(self: *TypeImporter, source: Lifted.Type.TypeId) Allocator.Error!Type.TypeVarId {
        switch (self.input.getTypePreservingNominal(source)) {
            .link => |next| return try self.importType(next),
            else => {},
        }

        if (self.active.get(source)) |existing| return existing;

        const target = try self.output.freshUnbd();
        try self.active.put(source, target);
        errdefer _ = self.active.remove(source);

        const node: Type.Node = switch (self.input.getTypePreservingNominal(source)) {
            .placeholder,
            .unbd,
            => lambdaInvariant("lambda-solved type import received unresolved lifted type"),
            .link => unreachable,
            .primitive => |prim| .{ .content = .{ .primitive = prim } },
            .func => |func| blk: {
                const args = try self.allocator.alloc(Type.TypeVarId, func.args.len);
                defer self.allocator.free(args);
                for (func.args, 0..) |arg, i| {
                    args[i] = try self.importType(arg);
                }
                const ret = try self.importType(func.ret);
                break :blk .{ .content = .{ .func = .{
                    .fixed_arity = @intCast(func.args.len),
                    .args = try self.output.addTypeVarSpan(args),
                    .ret = ret,
                    .callable = self.output.freshCallableVar(),
                } } };
            },
            .nominal => |nominal| blk: {
                const args = try self.allocator.alloc(Type.TypeVarId, nominal.args.len);
                defer self.allocator.free(args);
                for (nominal.args, 0..) |arg, i| {
                    args[i] = try self.importType(arg);
                }
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.output.addTypeVarSpan(args),
                    .backing = try self.importType(nominal.backing),
                } };
            },
            .list => |elem| .{ .content = .{ .list = try self.importType(elem) } },
            .box => |elem| .{ .content = .{ .box = try self.importType(elem) } },
            .tuple => |elems| blk: {
                const items = try self.allocator.alloc(Type.TypeVarId, elems.len);
                defer self.allocator.free(items);
                for (elems, 0..) |elem, i| {
                    items[i] = try self.importType(elem);
                }
                break :blk .{ .content = .{ .tuple = try self.output.addTypeVarSpan(items) } };
            },
            .tag_union => |tag_union| blk: {
                const tags = try self.allocator.alloc(Type.Tag, tag_union.tags.len);
                defer self.allocator.free(tags);
                for (tag_union.tags, 0..) |tag, i| {
                    const args = try self.allocator.alloc(Type.TypeVarId, tag.args.len);
                    defer self.allocator.free(args);
                    for (tag.args, 0..) |arg, j| {
                        args[j] = try self.importType(arg);
                    }
                    tags[i] = .{
                        .name = tag.name,
                        .args = try self.output.addTypeVarSpan(args),
                    };
                }
                break :blk .{ .content = .{ .tag_union = .{ .tags = try self.output.addTags(tags) } } };
            },
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.Field, record.fields.len);
                defer self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    fields[i] = .{
                        .name = field.name,
                        .ty = try self.importType(field.ty),
                    };
                }
                break :blk .{ .content = .{ .record = .{ .fields = try self.output.addFields(fields) } } };
            },
        };

        self.output.setNode(target, node);
        _ = self.active.remove(source);
        return target;
    }
};

const BodySolver = struct {
    allocator: Allocator,
    input: *const Lifted.Ast.Store,
    output: *Ast.Store,
    canonical_names: *const canonical.CanonicalNameStore,
    type_importer: *TypeImporter,
    representation_store: *repr.RepresentationStore,
    value_store: *repr.ValueInfoStore,
    env: std.AutoHashMap(Ast.Symbol, repr.BindingInfoId),
    expr_map: std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId),
    instance: repr.ProcRepresentationInstanceId,
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    public_roots: ?repr.ProcPublicValueRoots = null,
    active_captures: ?repr.Span(repr.ValueInfoId) = null,
    next_source_match_id: u32 = 0,
    next_if_expr_id: u32 = 0,

    fn deinit(self: *BodySolver) void {
        self.expr_map.deinit();
        self.env.deinit();
    }

    fn lowerDef(self: *BodySolver, def_id: Lifted.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.getDef(def_id);
        return try self.output.addDef(.{
            .proc = def.proc,
            .value = switch (def.value) {
                .fn_ => |fn_| blk: {
                    const lowered_args = try self.lowerParamSpan(fn_.args);
                    const capture_values = try self.lowerCaptureSlotRoots(fn_.captures);
                    const previous_captures = self.active_captures;
                    self.active_captures = capture_values;
                    defer self.active_captures = previous_captures;
                    const body = try self.lowerExpr(fn_.body);
                    const body_value = self.exprValue(body);
                    const function_root = self.representation_store.reserveRoot();
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = body_value,
                        .captures = capture_values,
                        .function_root = function_root,
                    };
                    break :blk .{ .fn_ = .{
                        .args = lowered_args.symbols,
                        .body = body,
                        .representation_instance = self.instance,
                    } };
                },
                .hosted_fn => |hosted| blk: {
                    const lowered_args = try self.lowerParamSpan(hosted.args);
                    const ret_ty = try self.type_importer.importType(hosted.ret_ty);
                    const ret = try self.newValue(ret_ty, .{});
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = ret,
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .hosted_fn = .{
                        .proc = hosted.proc,
                        .args = lowered_args.symbols,
                        .ret_ty = ret_ty,
                        .hosted = hosted.hosted,
                    } };
                },
                .val => |expr| blk: {
                    const body = try self.lowerExpr(expr);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = self.exprValue(body),
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .val = body };
                },
                .run => |run| blk: {
                    const body = try self.lowerExpr(run.body);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = self.exprValue(body),
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .run = .{ .body = body } };
                },
            },
        });
    }

    const LoweredParams = struct {
        symbols: Ast.Span(Ast.TypedSymbol),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerParamSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TypedSymbol)) Allocator.Error!LoweredParams {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        if (input_items.len == 0) return .{
            .symbols = Ast.Span(Ast.TypedSymbol).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const symbols = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(symbols);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |param, i| {
            const ty = try self.type_importer.importType(param.ty);
            const value = try self.newValue(ty, param.source_ty);
            const binding = try self.value_store.addBinding(.{
                .symbol = param.symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(param.symbol, binding);
            symbols[i] = .{
                .ty = ty,
                .source_ty = param.source_ty,
                .symbol = param.symbol,
                .binding_info = binding,
            };
            values[i] = value;
        }
        return .{
            .symbols = try self.output.addTypedSymbolSpan(symbols),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerCaptureSlotRoots(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.CaptureSlot)) Allocator.Error!repr.Span(repr.ValueInfoId) {
        const input_items = self.input.sliceCaptureSlotSpan(span);
        if (input_items.len == 0) return repr.Span(repr.ValueInfoId).empty();
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |slot, i| {
            const ty = try self.type_importer.importType(slot.ty);
            const value = try self.newValue(ty, slot.source_ty);
            const binding = try self.value_store.addBinding(.{
                .symbol = slot.source_symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(slot.source_symbol, binding);
            values[i] = value;
        }
        return try self.value_store.addValueSpan(values);
    }

    fn lowerExpr(self: *BodySolver, expr_id: Lifted.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.getExpr(expr_id);
        const ty = try self.type_importer.importType(expr.ty);
        switch (expr.data) {
            .var_ => |symbol| {
                const binding_info = self.env.get(symbol) orelse lambdaInvariant("lambda-solved variable occurrence has no published binding info");
                const binding = self.value_store.bindings.items[@intFromEnum(binding_info)];
                const lowered = try self.output.addExpr(ty, expr.source_ty, binding.value, .{ .var_ = .{
                    .symbol = symbol,
                    .binding_info = binding_info,
                } });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            .capture_ref => |slot| {
                const captures_span = self.active_captures orelse lambdaInvariant("lambda-solved capture_ref reached a procedure without capture roots");
                const captures = self.value_store.sliceValueSpan(captures_span);
                const capture_index: usize = @intCast(slot);
                if (capture_index >= captures.len) lambdaInvariant("lambda-solved capture_ref slot does not exist in procedure capture roots");
                const lowered = try self.output.addExpr(ty, expr.source_ty, captures[capture_index], .{ .capture_ref = slot });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            else => {},
        }
        switch (expr.data) {
            .let_ => |let_| {
                const body = try self.lowerExpr(let_.body);
                const bind_ty = try self.type_importer.importType(let_.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = let_.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                const previous = try self.env.fetchPut(let_.bind.symbol, binding);
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.symbol, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.symbol);
                    }
                }
                const rest = try self.lowerExpr(let_.rest);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(rest), .{ .let_ = .{
                    .bind = .{
                        .ty = bind_ty,
                        .source_ty = let_.bind.source_ty,
                        .symbol = let_.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                    .rest = rest,
                } });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            .block => |block| {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = try self.lowerExpr(block.final_expr);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(final_expr), .{ .block = .{
                    .stmts = stmts,
                    .final_expr = final_expr,
                } });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            .inspect => |child| {
                const lowered_child = try self.lowerExpr(child);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(lowered_child), .{ .inspect = lowered_child });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            else => {},
        }
        switch (expr.data) {
            .access => |access| {
                const record = try self.lowerExpr(access.record);
                const source = self.exprValue(record);
                if (self.value_store.values.items[@intFromEnum(source)].aggregate) |aggregate| {
                    const result = switch (aggregate) {
                        .record => |record_info| self.recordAggregateFieldValue(record_info, access.field),
                        else => lambdaInvariant("lambda-solved record access source had non-record aggregate metadata"),
                    };
                    const projection = try self.value_store.addProjection(.{
                        .source = source,
                        .result = result,
                        .root = self.valueRoot(result),
                        .kind = .{ .record_field = access.field },
                    });
                    const lowered = try self.output.addExpr(ty, expr.source_ty, result, .{ .access = .{
                        .record = record,
                        .field = access.field,
                        .projection_info = projection,
                    } });
                    try self.expr_map.put(expr_id, lowered);
                    return lowered;
                }
            },
            .tuple_access => |access| {
                const tuple = try self.lowerExpr(access.tuple);
                const source = self.exprValue(tuple);
                if (self.value_store.values.items[@intFromEnum(source)].aggregate) |aggregate| {
                    const result = switch (aggregate) {
                        .tuple => |tuple_info| self.tupleAggregateElemValue(tuple_info, access.elem_index),
                        else => lambdaInvariant("lambda-solved tuple access source had non-tuple aggregate metadata"),
                    };
                    const projection = try self.value_store.addProjection(.{
                        .source = source,
                        .result = result,
                        .root = self.valueRoot(result),
                        .kind = .{ .tuple_elem = access.elem_index },
                    });
                    const lowered = try self.output.addExpr(ty, expr.source_ty, result, .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                        .projection_info = projection,
                    } });
                    try self.expr_map.put(expr_id, lowered);
                    return lowered;
                }
            },
            .tag_payload => |payload| {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const source = self.exprValue(tag_union);
                if (self.value_store.values.items[@intFromEnum(source)].aggregate) |aggregate| {
                    const result = switch (aggregate) {
                        .tag => |tag_info| self.tagAggregatePayloadValue(tag_info, payload.payload),
                        else => lambdaInvariant("lambda-solved tag payload source had non-tag aggregate metadata"),
                    };
                    const projection = try self.value_store.addProjection(.{
                        .source = source,
                        .result = result,
                        .root = self.valueRoot(result),
                        .kind = .{ .tag_payload = payload.payload },
                    });
                    const lowered = try self.output.addExpr(ty, expr.source_ty, result, .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                        .projection_info = projection,
                    } });
                    try self.expr_map.put(expr_id, lowered);
                    return lowered;
                }
            },
            else => {},
        }

        const value = try self.newValue(ty, expr.source_ty);
        const lowered = try self.output.addExpr(ty, expr.source_ty, value, switch (expr.data) {
            .var_,
            .capture_ref,
            => unreachable,
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .bool_lit => |literal| .{ .bool_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .const_instance => |const_instance| .{ .const_instance = const_instance },
            .tag => |tag| blk: {
                const eval_order = try self.lowerTagPayloadEvalSpan(tag.eval_order);
                const assembly_order = try self.lowerTagPayloadAssemblySpan(tag.assembly_order);
                try self.publishTagAggregate(value, tag.union_shape, tag.tag, assembly_order);
                break :blk .{ .tag = .{
                    .union_shape = tag.union_shape,
                    .tag = tag.tag,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                    .constructor_ty = try self.type_importer.importType(tag.constructor_ty),
                } };
            },
            .record => |record| blk: {
                const eval_order = try self.lowerRecordFieldEvalSpan(record.eval_order);
                const assembly_order = try self.lowerRecordFieldAssemblySpan(record.assembly_order);
                try self.publishRecordAggregate(value, record.shape, assembly_order);
                break :blk .{ .record = .{
                    .shape = record.shape,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                } };
            },
            .nominal_reinterpret => |backing| .{ .nominal_reinterpret = try self.lowerExpr(backing) },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(record),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .record_field = access.field },
                });
                break :blk .{ .access = .{
                    .record = record,
                    .field = access.field,
                    .projection_info = projection,
                } };
            },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
            } },
            .bool_not => |child| .{ .bool_not = try self.lowerExpr(child) },
            .let_ => unreachable,
            .call_value => |call| blk: {
                const func = try self.lowerExpr(call.func);
                const callee_value = self.exprValue(func);
                const lowered_args = try self.lowerExprSpanWithValues(call.args);
                const requested_fn_ty = try self.type_importer.importType(call.requested_fn_ty);
                const call_site = try self.value_store.addCallSite(.{
                    .callee = callee_value,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = self.representation_store.reserveRoot(),
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .dispatch = self.callSiteDispatchForCallee(callee_value),
                });
                break :blk .{ .call_value = .{
                    .func = func,
                    .args = lowered_args.exprs,
                    .requested_fn_ty = requested_fn_ty,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .call_site = call_site,
                } };
            },
            .call_proc => |call| blk: {
                const lowered_args = try self.lowerExprSpanWithValues(call.args);
                const requested_fn_ty = try self.type_importer.importType(call.requested_fn_ty);
                const call_site = try self.value_store.addCallSite(.{
                    .callee = null,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = self.representation_store.reserveRoot(),
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .dispatch = .{ .call_proc = self.procRepresentationInstance(call.proc) },
                });
                break :blk .{ .call_proc = .{
                    .proc = call.proc,
                    .args = lowered_args.exprs,
                    .requested_fn_ty = requested_fn_ty,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .call_site = call_site,
                } };
            },
            .proc_value => |proc_value| blk: {
                const captures = try self.lowerCaptureArgSpanWithValues(proc_value.captures);
                const callable = try self.representation_store.addSingletonProcValueCallable(
                    self.canonical_names,
                    self.type_importer.output,
                    self.value_store,
                    value,
                    self.valueRoot(value),
                    proc_value.proc,
                    self.value_store.sliceValueSpan(captures.values),
                );
                self.value_store.values.items[@intFromEnum(value)].callable = callable;
                break :blk .{ .proc_value = .{
                    .proc = proc_value.proc,
                    .captures = captures.args,
                    .fn_ty = try self.type_importer.importType(proc_value.fn_ty),
                } };
            },
            .inspect => unreachable,
            .low_level => |low_level| try self.lowerLowLevel(value, expr.source_ty, low_level),
            .block => unreachable,
            .tuple => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishTupleAggregate(value, lowered_items.values);
                break :blk .{ .tuple = lowered_items.exprs };
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(tag_union),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .tag_payload = payload.payload },
                });
                break :blk .{ .tag_payload = .{
                    .tag_union = tag_union,
                    .payload = payload.payload,
                    .projection_info = projection,
                } };
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(tuple),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .tuple_elem = access.elem_index },
                });
                break :blk .{ .tuple_access = .{
                    .tuple = tuple,
                    .elem_index = access.elem_index,
                    .projection_info = projection,
                } };
            },
            .list => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishListAggregate(value, lowered_items.values);
                break :blk .{ .list = lowered_items.exprs };
            },
            .unit => .unit,
            .return_ => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                const return_info = try self.value_store.addReturn(.{
                    .value = self.exprValue(lowered_child),
                });
                break :blk .{ .return_ = .{
                    .expr = lowered_child,
                    .return_info = return_info,
                } };
            },
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .match_ => |match_| blk: {
                const match_id = self.freshSourceMatchId();
                const cond = try self.lowerExpr(match_.cond);
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const branch_inputs = try self.joinInputsForBranches(match_id, lowered_branches);
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = branch_inputs,
                    .root = self.valueRoot(value),
                    .kind = .match_expr,
                });
                break :blk .{ .match_ = .{
                    .cond = cond,
                    .branches = lowered_branches,
                    .is_try_suffix = match_.is_try_suffix,
                    .join_info = join_info,
                } };
            },
            .if_ => |if_| blk: {
                const if_expr_id = self.freshIfExprId();
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerExpr(if_.then_body);
                const else_body = try self.lowerExpr(if_.else_body);
                var inputs = std.ArrayList(repr.JoinInputInfo).empty;
                defer inputs.deinit(self.allocator);
                if (self.exprReturnsValue(then_body)) {
                    try inputs.append(self.allocator, .{
                        .source = .{ .if_branch = .{
                            .if_expr = if_expr_id,
                            .branch = .then_,
                        } },
                        .value = self.exprValue(then_body),
                    });
                }
                if (self.exprReturnsValue(else_body)) {
                    try inputs.append(self.allocator, .{
                        .source = .{ .if_branch = .{
                            .if_expr = if_expr_id,
                            .branch = .else_,
                        } },
                        .value = self.exprValue(else_body),
                    });
                }
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = try self.value_store.addJoinInputSpan(inputs.items),
                    .root = self.valueRoot(value),
                    .kind = .if_expr,
                });
                break :blk .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                    .join_info = join_info,
                } };
            },
            .for_ => |for_| try self.lowerForExpr(value, for_),
        });
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    const SavedBinding = struct {
        symbol: Ast.Symbol,
        previous: ?repr.BindingInfoId,
    };

    fn lowerPatScoped(
        self: *BodySolver,
        pat_id: Lifted.Ast.PatId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        const ty = try self.type_importer.importType(pat.ty);
        const value = try self.newValue(ty, pat.source_ty);
        return try self.lowerPatScopedWithValue(pat_id, ty, value, saved);
    }

    fn lowerPatScopedWithValue(
        self: *BodySolver,
        pat_id: Lifted.Ast.PatId,
        ty: Type.TypeVarId,
        value: repr.ValueInfoId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        return try self.output.addPat(.{ .ty = ty, .source_ty = pat.source_ty, .value_info = value, .data = switch (pat.data) {
            .bool_lit => |literal| .{ .bool_lit = literal },
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
                const binding = try self.bindPatternSymbol(as.symbol, value, saved);
                break :blk .{ .as = .{
                    .pattern = try self.lowerPatScopedWithValue(as.pattern, ty, value, saved),
                    .symbol = as.symbol,
                    .binding_info = binding,
                } };
            },
            .var_ => |symbol| blk: {
                const binding = try self.bindPatternSymbol(symbol, value, saved);
                break :blk .{ .var_ = .{
                    .symbol = symbol,
                    .binding_info = binding,
                } };
            },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads, saved),
            } },
        } });
    }

    fn bindPatternSymbol(
        self: *BodySolver,
        symbol: Ast.Symbol,
        value: repr.ValueInfoId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!repr.BindingInfoId {
        const binding = try self.value_store.addBinding(.{
            .symbol = symbol,
            .value = value,
            .root = self.valueRoot(value),
        });
        const previous = try self.env.fetchPut(symbol, binding);
        try saved.append(self.allocator, .{
            .symbol = symbol,
            .previous = if (previous) |entry| entry.value else null,
        });
        return binding;
    }

    fn lowerPatSpanScoped(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.PatId),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.input.slicePatSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.PatId).empty();
        const output_items = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerPatScoped(item, saved);
        }
        return try self.output.addPatSpan(output_items);
    }

    fn lowerRecordFieldPatternSpanScoped(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.RecordFieldPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.RecordFieldPattern) {
        const input_items = self.input.sliceRecordFieldPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldPattern).empty();
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

    fn restoreBindings(self: *BodySolver, saved: *std.ArrayList(SavedBinding), start: usize) void {
        while (saved.items.len > start) {
            const binding = saved.pop().?;
            if (binding.previous) |previous| {
                self.env.put(binding.symbol, previous) catch unreachable;
            } else {
                _ = self.env.remove(binding.symbol);
            }
        }
    }

    fn lowerBranch(self: *BodySolver, branch_id: Lifted.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const pat = try self.lowerPatScoped(branch.pat, &saved);
        defer self.restoreBindings(&saved, 0);
        const guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null;
        const body = try self.lowerExpr(branch.body);
        return try self.output.addBranch(.{
            .pat = pat,
            .guard = guard,
            .body = body,
            .degenerate = branch.degenerate,
        });
    }

    fn lowerForExpr(self: *BodySolver, value: repr.ValueInfoId, for_: anytype) Allocator.Error!Ast.Expr.Data {
        _ = value;
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        defer self.restoreBindings(&saved, 0);
        return .{ .for_ = .{
            .patt = patt,
            .iterable = try self.lowerExpr(for_.iterable),
            .body = try self.lowerExpr(for_.body),
        } };
    }

    fn lowerStmt(self: *BodySolver, stmt_id: Lifted.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.getStmt(stmt_id);
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_ty = try self.type_importer.importType(decl.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = decl.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                try self.env.put(decl.bind.symbol, binding);
                break :blk .{ .decl = .{
                    .bind = .{
                        .ty = bind_ty,
                        .source_ty = decl.bind.source_ty,
                        .symbol = decl.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_ty = try self.type_importer.importType(decl.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = decl.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                try self.env.put(decl.bind.symbol, binding);
                break :blk .{ .var_decl = .{
                    .bind = .{
                        .ty = bind_ty,
                        .source_ty = decl.bind.source_ty,
                        .symbol = decl.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                } };
            },
            .reassign => |reassign| blk: {
                const body = try self.lowerExpr(reassign.body);
                const binding = self.env.get(reassign.target) orelse lambdaInvariant("lambda-solved reassignment target has no binding info");
                break :blk .{ .reassign = .{
                    .target = reassign.target,
                    .version = binding,
                    .body = body,
                } };
            },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| blk: {
                const lowered_child = try self.lowerExpr(expr);
                const return_info = try self.value_store.addReturn(.{
                    .value = self.exprValue(lowered_child),
                });
                break :blk .{ .return_ = .{
                    .expr = lowered_child,
                    .return_info = return_info,
                } };
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

    const LoweredExprSpan = struct {
        exprs: Ast.Span(Ast.ExprId),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerExprSpanWithValues(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!LoweredExprSpan {
        const input_items = self.input.sliceExprSpan(span);
        if (input_items.len == 0) return .{
            .exprs = Ast.Span(Ast.ExprId).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const exprs = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(exprs);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |expr, i| {
            exprs[i] = try self.lowerExpr(expr);
            values[i] = self.exprValue(exprs[i]);
        }
        return .{
            .exprs = try self.output.addExprSpan(exprs),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn callSiteDispatchForCallee(
        self: *BodySolver,
        callee_value: repr.ValueInfoId,
    ) repr.CallSiteDispatch {
        const value_info = self.value_store.values.items[@intFromEnum(callee_value)];
        const callable = value_info.callable orelse lambdaInvariant("lambda-solved call_value callee has no callable representation");
        return switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .finite => |key| .{ .call_value_finite = key },
            .already_erased => |erased| .{ .call_value_erased = erased.sig_key },
            .erase_finite_set => |erase| .{ .call_value_erased = erase.adapter.erased_fn_sig_key },
            .erase_proc_value => |erase| .{ .call_value_erased = erase.erased_fn_sig_key },
        };
    }

    fn procRepresentationInstance(
        self: *const BodySolver,
        proc: canonical.MirProcedureRef,
    ) repr.ProcRepresentationInstanceId {
        return self.proc_instance_map.get(proc) orelse lambdaInvariant("lambda-solved call_proc target was not reserved before body lowering");
    }

    fn lowerExprSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        return (try self.lowerExprSpanWithValues(span)).exprs;
    }

    fn lowerLowLevel(
        self: *BodySolver,
        result_value: repr.ValueInfoId,
        result_source_ty: canonical.CanonicalTypeKey,
        low_level: anytype,
    ) Allocator.Error!Ast.Expr.Data {
        const lowered_args = try self.lowerExprSpanWithValues(low_level.args);
        const source_constraint_ty = try self.type_importer.importType(low_level.source_constraint_ty);
        switch (low_level.op) {
            .box_box => {
                const arg_values = self.value_store.sliceValueSpan(lowered_args.values);
                if (arg_values.len != 1) lambdaInvariant("lambda-solved Box.box reached non-unary low-level expression");
                const payload_value = arg_values[0];
                const payload_info = self.value_store.values.items[@intFromEnum(payload_value)];
                const result_root = self.valueRoot(result_value);
                const payload_root = self.valueRoot(payload_value);
                const boundary = try self.representation_store.appendBoxBoundary(self.allocator, .{
                    .box_ty = result_source_ty,
                    .payload_source_ty = payload_info.source_ty,
                    .payload_boundary_ty = payload_info.source_ty,
                    .direction = .box,
                    .source_root = payload_root,
                    .boundary_root = result_root,
                    .payload_plan = .unchanged,
                });
                self.value_store.values.items[@intFromEnum(result_value)].boxed = .{
                    .box_root = result_root,
                    .payload_root = payload_root,
                    .boundary = boundary,
                };
            },
            .box_unbox => {
                const arg_values = self.value_store.sliceValueSpan(lowered_args.values);
                if (arg_values.len != 1) lambdaInvariant("lambda-solved Box.unbox reached non-unary low-level expression");
                const boxed_value = arg_values[0];
                const boxed_info = self.value_store.values.items[@intFromEnum(boxed_value)];
                _ = try self.representation_store.appendBoxBoundary(self.allocator, .{
                    .box_ty = boxed_info.source_ty,
                    .payload_source_ty = result_source_ty,
                    .payload_boundary_ty = result_source_ty,
                    .direction = .unbox,
                    .source_root = self.valueRoot(boxed_value),
                    .boundary_root = self.valueRoot(result_value),
                    .payload_plan = .unchanged,
                });
            },
            else => {},
        }
        return .{ .low_level = .{
            .op = low_level.op,
            .args = lowered_args.exprs,
            .source_constraint_ty = source_constraint_ty,
        } };
    }

    fn lowerStmtSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.sliceStmtSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.StmtId).empty();
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerBranchSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            output_items[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn joinInputsForBranches(
        self: *BodySolver,
        match_id: repr.SourceMatchId,
        span: Ast.Span(Ast.BranchId),
    ) Allocator.Error!repr.Span(repr.JoinInputInfo) {
        if (span.len == 0) return repr.Span(repr.JoinInputInfo).empty();
        const branch_ids = self.output.branch_ids.items[span.start..][0..span.len];
        const inputs = try self.allocator.alloc(repr.JoinInputInfo, branch_ids.len);
        defer self.allocator.free(inputs);
        var input_len: usize = 0;
        for (branch_ids, 0..) |branch_id, i| {
            const body = self.output.branches.items[@intFromEnum(branch_id)].body;
            if (!self.exprReturnsValue(body)) continue;
            inputs[input_len] = .{
                .source = .{ .source_match_branch = .{
                    .match = match_id,
                    .branch = @enumFromInt(@as(u32, @intCast(i))),
                    .alternative = @enumFromInt(@as(u32, @intCast(i))),
                } },
                .value = self.exprValue(body),
            };
            input_len += 1;
        }
        return try self.value_store.addJoinInputSpan(inputs[0..input_len]);
    }

    fn exprReturnsValue(self: *const BodySolver, expr_id: Ast.ExprId) bool {
        return switch (self.output.exprs.items[@intFromEnum(expr_id)].data) {
            .return_,
            .crash,
            .runtime_error,
            => false,
            else => true,
        };
    }

    fn freshSourceMatchId(self: *BodySolver) repr.SourceMatchId {
        const id: repr.SourceMatchId = @enumFromInt(self.next_source_match_id);
        self.next_source_match_id += 1;
        return id;
    }

    fn freshIfExprId(self: *BodySolver) repr.IfExprId {
        const id: repr.IfExprId = @enumFromInt(self.next_if_expr_id);
        self.next_if_expr_id += 1;
        return id;
    }

    const LoweredCaptureArgs = struct {
        args: Ast.Span(Ast.CaptureArg),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerCaptureArgSpanWithValues(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.CaptureArg)) Allocator.Error!LoweredCaptureArgs {
        const input_items = self.input.sliceCaptureArgSpan(span);
        if (input_items.len == 0) return .{
            .args = Ast.Span(Ast.CaptureArg).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const output_items = try self.allocator.alloc(Ast.CaptureArg, input_items.len);
        defer self.allocator.free(output_items);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |capture, i| {
            const expr = try self.lowerExpr(capture.expr);
            const value = self.exprValue(expr);
            output_items[i] = .{
                .slot = capture.slot,
                .value_info = value,
                .expr = expr,
            };
            values[i] = value;
        }
        return .{
            .args = try self.output.addCaptureArgSpan(output_items),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerRecordFieldEvalSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldEval)) Allocator.Error!Ast.Span(Ast.RecordFieldEval) {
        const input_items = self.input.sliceRecordFieldEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldEval).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldEvalSpan(output_items);
    }

    fn lowerRecordFieldAssemblySpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldAssembly) {
        const input_items = self.input.sliceRecordFieldAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldAssemblySpan(output_items);
    }

    fn lowerTagPayloadEvalSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadEval)) Allocator.Error!Ast.Span(Ast.TagPayloadEval) {
        const input_items = self.input.sliceTagPayloadEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadEval).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadEvalSpan(output_items);
    }

    fn lowerTagPayloadAssemblySpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadAssembly) {
        const input_items = self.input.sliceTagPayloadAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadAssemblySpan(output_items);
    }

    fn recordAggregateFieldValue(self: *const BodySolver, record: anytype, field: MonoRow.RecordFieldId) repr.ValueInfoId {
        _ = self;
        for (record.fields) |field_info| {
            if (field_info.field == field) return field_info.value;
        }
        lambdaInvariant("lambda-solved record aggregate projection referenced a missing field");
    }

    fn tupleAggregateElemValue(self: *const BodySolver, tuple: []const repr.ElemValueInfo, elem_index: u32) repr.ValueInfoId {
        _ = self;
        for (tuple) |elem| {
            if (elem.index == elem_index) return elem.value;
        }
        lambdaInvariant("lambda-solved tuple aggregate projection referenced a missing element");
    }

    fn tagAggregatePayloadValue(self: *const BodySolver, tag: anytype, payload: MonoRow.TagPayloadId) repr.ValueInfoId {
        _ = self;
        for (tag.payloads) |payload_info| {
            if (payload_info.payload == payload) return payload_info.value;
        }
        lambdaInvariant("lambda-solved tag aggregate projection referenced a missing payload");
    }

    fn publishRecordAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        shape: MonoRow.RecordShapeId,
        assembly_order: Ast.Span(Ast.RecordFieldAssembly),
    ) Allocator.Error!void {
        const assemblies = self.output.record_field_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const fields = try self.allocator.alloc(repr.FieldValueInfo, assemblies.len);
        errdefer if (fields.len > 0) self.allocator.free(fields);

        for (assemblies, 0..) |field, i| {
            fields[i] = .{
                .field = field.field,
                .value = self.exprValue(field.value),
            };
        }
        self.publishAggregate(value, .{ .record = .{
            .shape = shape,
            .fields = fields,
        } });
    }

    fn publishTupleAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const infos = try self.allocator.alloc(repr.ElemValueInfo, elem_values.len);
        errdefer if (infos.len > 0) self.allocator.free(infos);

        for (elem_values, 0..) |elem, i| {
            infos[i] = .{
                .index = @intCast(i),
                .value = elem,
            };
        }
        self.publishAggregate(value, .{ .tuple = infos });
    }

    fn publishTagAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        union_shape: MonoRow.TagUnionShapeId,
        tag_id: MonoRow.TagId,
        assembly_order: Ast.Span(Ast.TagPayloadAssembly),
    ) Allocator.Error!void {
        const assemblies = self.output.tag_payload_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const payloads = try self.allocator.alloc(repr.TagPayloadValueInfo, assemblies.len);
        errdefer if (payloads.len > 0) self.allocator.free(payloads);

        for (assemblies, 0..) |payload, i| {
            payloads[i] = .{
                .payload = payload.payload,
                .value = self.exprValue(payload.value),
            };
        }
        self.publishAggregate(value, .{ .tag = .{
            .union_shape = union_shape,
            .tag = tag_id,
            .payloads = payloads,
        } });
    }

    fn publishListAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const owned_elems = try self.allocator.dupe(repr.ValueInfoId, elem_values);
        errdefer if (owned_elems.len > 0) self.allocator.free(owned_elems);

        self.publishAggregate(value, .{ .list = .{
            .elem_root = self.representation_store.reserveRoot(),
            .elems = owned_elems,
        } });
    }

    fn publishAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        aggregate: repr.AggregateValueInfo,
    ) void {
        const value_info = &self.value_store.values.items[@intFromEnum(value)];
        if (value_info.aggregate != null) lambdaInvariant("lambda-solved value published aggregate metadata twice");
        value_info.aggregate = aggregate;
    }

    fn lowerTagPayloadPatternSpan(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        const input_items = self.input.sliceTagPayloadPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPatScoped(payload.pattern, saved),
            };
        }
        return try self.output.addTagPayloadPatternSpan(output_items);
    }

    fn newValue(
        self: *BodySolver,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!repr.ValueInfoId {
        const root = self.representation_store.reserveRoot();
        const class = self.representation_store.reserveClass();
        return try self.value_store.addValue(.{
            .logical_ty = ty,
            .source_ty = source_ty,
            .root = root,
            .solved_class = class,
        });
    }

    fn exprValue(self: *const BodySolver, expr: Ast.ExprId) repr.ValueInfoId {
        return self.output.exprs.items[@intFromEnum(expr)].value_info;
    }

    fn valueRoot(self: *const BodySolver, value: repr.ValueInfoId) repr.RepRootId {
        return self.value_store.values.items[@intFromEnum(value)].root;
    }
};

fn lambdaInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "lambda-solved program owns representation tables" {
    std.testing.refAllDecls(@This());
}
