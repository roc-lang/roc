//! Compile-time evaluation finalization for checked artifacts.
//!
//! This module owns the post-check work that cannot live in `check` because it
//! must run the MIR-family pipeline, ARC insertion, and the LIR interpreter.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const check = @import("check");
const layout_mod = @import("layout");
const lir = @import("lir");
const mir = @import("mir");

const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("test/RuntimeHostEnv.zig");
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const checked_artifact = check.CheckedArtifact;

pub fn finalizer() checked_artifact.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

fn finalize(
    _: ?*anyopaque,
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    imports: []const checked_artifact.PublishImportArtifact,
) anyerror!void {
    const compile_time_roots = try compileTimeRootRequests(allocator, artifact.root_requests.requests);
    defer if (compile_time_roots.len > 0) allocator.free(compile_time_roots);

    if (compile_time_roots.len == 0) {
        try artifact.comptime_values.sealBindings();
        return;
    }

    const import_views = try allocator.alloc(checked_artifact.ImportedModuleView, imports.len);
    defer allocator.free(import_views);
    for (imports, 0..) |import, i| {
        import_views[i] = import.view;
    }

    var lowered = try lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{
            .root = checked_artifact.loweringView(artifact),
            .imports = import_views,
        },
        .{
            .requests = compile_time_roots,
            .purpose = .compile_time,
            .compile_time_plan_sink = &artifact.comptime_plans,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    defer lowered.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    var interpreter = try Interpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interpreter.deinit();

    if (compile_time_roots.len != lowered.lir_result.root_procs.items.len) {
        compileTimeFinalizationInvariant("compile-time lowering did not preserve root cardinality");
    }

    if (compile_time_roots.len != lowered.compile_time_root_payloads.len) {
        compileTimeFinalizationInvariant("compile-time lowering did not publish one root payload per root");
    }

    for (compile_time_roots, lowered.lir_result.root_procs.items, lowered.compile_time_root_payloads) |root_request, lir_root, payload| {
        const root = compileTimeRootForRequest(artifact, root_request);
        artifact.compile_time_roots.fillPayload(root.id, payload);
        switch (root.kind) {
            .constant => try evaluateConstantRoot(
                allocator,
                artifact,
                &interpreter,
                &lowered,
                root,
                lir_root,
                switch (payload) {
                    .const_graph => |plan| plan,
                    else => compileTimeFinalizationInvariant("constant root did not publish a const graph plan"),
                },
            ),
            .callable_binding => try evaluateCallableBindingRoot(
                allocator,
                artifact,
                &interpreter,
                &lowered,
                root,
                lir_root,
                switch (payload) {
                    .callable_result => |plan| plan,
                    else => compileTimeFinalizationInvariant("callable root did not publish a callable result plan"),
                },
            ),
            .expect => {},
        }
    }

    try artifact.comptime_values.sealBindings();
}

fn compileTimeRootRequests(
    allocator: Allocator,
    roots: []const checked_artifact.RootRequest,
) Allocator.Error![]checked_artifact.RootRequest {
    var out = std.ArrayList(checked_artifact.RootRequest).empty;
    errdefer out.deinit(allocator);

    for (roots) |root| {
        if (root.abi != .compile_time) continue;
        try out.append(allocator, root);
    }

    return try out.toOwnedSlice(allocator);
}

fn evaluateConstantRoot(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    interpreter: *Interpreter,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    root: checked_artifact.CompileTimeRoot,
    lir_root: lir.LIR.LirProcSpecId,
    reification_plan: checked_artifact.ConstGraphReificationPlanId,
) anyerror!void {
    const pattern = root.pattern orelse compileTimeFinalizationInvariant("constant root had no top-level pattern");
    const top_level = artifact.top_level_values.lookupByPattern(pattern) orelse {
        compileTimeFinalizationInvariant("constant root had no top-level value entry");
    };
    const const_ref = switch (top_level.value) {
        .const_ref => |ref| ref,
        .procedure_binding => compileTimeFinalizationInvariant("constant root top-level value was a procedure binding"),
    };

    const result = try interpreter.eval(.{
        .proc_id = lir_root,
        .arg_layouts = &.{},
    });
    const ret_layout = lowered.lir_result.store.getProcSpec(lir_root).ret_layout;
    defer interpreter.dropValue(result.value, ret_layout);

    var reifier = ComptimeReifier{
        .allocator = allocator,
        .values = &artifact.comptime_values,
        .plans = &artifact.comptime_plans,
        .checked_types = &artifact.checked_types,
        .layouts = &lowered.lir_result.layouts,
        .callable_set_descriptors = lowered.callable_set_descriptors,
    };
    const reified = try reifier.reifyPlan(reification_plan, ret_layout, result.value);

    artifact.const_templates.fillValueGraph(const_ref, .{
        .schema = reified.schema,
        .value = reified.value,
    });
    try artifact.comptime_values.bind(pattern, reified.schema, reified.value);

    const requested_source_ty = artifact.checked_types.roots[@intFromEnum(root.checked_type)].key;
    const instance_ref = try artifact.const_instances.reserve(allocator, .{
        .const_ref = const_ref,
        .requested_source_ty = requested_source_ty,
    });
    artifact.const_instances.fill(instance_ref, .{
        .schema = reified.schema,
        .value = reified.value,
    });
}

fn evaluateCallableBindingRoot(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    interpreter: *Interpreter,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    root: checked_artifact.CompileTimeRoot,
    lir_root: lir.LIR.LirProcSpecId,
    result_plan: checked_artifact.CallableResultPlanId,
) anyerror!void {
    const pattern = root.pattern orelse compileTimeFinalizationInvariant("callable root had no top-level pattern");
    const top_level = artifact.top_level_values.lookupByPattern(pattern) orelse {
        compileTimeFinalizationInvariant("callable root had no top-level value entry");
    };
    const binding_ref = switch (top_level.value) {
        .procedure_binding => |binding| binding,
        .const_ref => compileTimeFinalizationInvariant("callable root top-level value was a const"),
    };

    const result = try interpreter.eval(.{
        .proc_id = lir_root,
        .arg_layouts = &.{},
    });
    const ret_layout = lowered.lir_result.store.getProcSpec(lir_root).ret_layout;
    defer interpreter.dropValue(result.value, ret_layout);

    const requested_source_fn_ty = artifact.checked_types.roots[@intFromEnum(root.checked_type)].key;
    const callable = existingNoCaptureCallableResult(
        &artifact.comptime_plans,
        lowered.callable_set_descriptors,
        &lowered.lir_result.layouts,
        result_plan,
        ret_layout,
        result.value,
    );
    if (!std.mem.eql(u8, &callable.source_fn_ty.bytes, &requested_source_fn_ty.bytes)) {
        compileTimeFinalizationInvariant("callable root result source type differed from checked root type");
    }

    const key = checked_artifact.CallableBindingInstantiationKey{
        .binding = .{ .top_level = binding_ref },
        .requested_source_fn_ty = requested_source_fn_ty,
    };
    const instance_ref = try artifact.callable_binding_instances.reserve(allocator, key);
    artifact.callable_binding_instances.markEvaluating(instance_ref);
    artifact.callable_binding_instances.fill(instance_ref, .{
        .key = key,
        .dependency_summary = @enumFromInt(@intFromEnum(root.dependency_summary_request)),
        .executable_root = root.id,
        .result_plan = result_plan,
        .promotion_plan = null,
        .promotion_output = .{ .existing_procedure = callable },
        .proc_value = callable,
    });
}

fn existingNoCaptureCallableResult(
    plans: *const checked_artifact.CompileTimePlanStore,
    descriptors: []const mir.LambdaSolved.Representation.CanonicalCallableSetDescriptor,
    layouts: *const layout_mod.Store,
    result_plan_id: checked_artifact.CallableResultPlanId,
    layout_idx: layout_mod.Idx,
    value: Value,
) check.CanonicalNames.ProcedureCallableRef {
    const plan = plans.callableResult(result_plan_id);
    const finite = switch (plan) {
        .finite => |finite| finite,
        .erased => compileTimeFinalizationInvariant("erased compile-time callable promotion is not sealed yet"),
    };
    if (finite.members.len == 0) {
        compileTimeFinalizationInvariant("finite compile-time callable result plan had no members");
    }

    const selected_member_id = readCallableSetMemberDiscriminant(
        layouts,
        layout_idx,
        value,
        finite.members.len,
    );
    const planned_member = callableResultMember(finite.members, selected_member_id) orelse {
        compileTimeFinalizationInvariant("compile-time callable result selected a member outside the result plan");
    };
    if (planned_member.capture_slots.len != 0) {
        compileTimeFinalizationInvariant("captured compile-time callable promotion is not sealed yet");
    }

    const descriptor = callableSetDescriptor(descriptors, finite.callable_set_key) orelse {
        compileTimeFinalizationInvariant("compile-time callable result descriptor was not preserved");
    };
    const member = callableSetMember(descriptor, planned_member.member) orelse {
        compileTimeFinalizationInvariant("compile-time callable result selected missing descriptor member");
    };
    if (member.capture_slots.len != 0) {
        compileTimeFinalizationInvariant("descriptor member for no-capture callable result had captures");
    }
    if (!std.mem.eql(u8, &member.proc_value.source_fn_ty.bytes, &finite.source_fn_ty.bytes)) {
        compileTimeFinalizationInvariant("compile-time callable descriptor member source type differs from result plan");
    }
    return member.proc_value;
}

fn readCallableSetMemberDiscriminant(
    layouts: *const layout_mod.Store,
    layout_idx: layout_mod.Idx,
    value: Value,
    member_count: usize,
) check.CanonicalNames.CallableSetMemberId {
    const layout = layouts.getLayout(layout_idx);
    if (layout.tag != .tag_union) {
        compileTimeFinalizationInvariant("finite compile-time callable result did not lower to tag-union layout");
    }
    const info = layouts.getTagUnionInfo(layout);
    const discriminant = info.data.readDiscriminant(value.ptr);
    if (discriminant >= member_count) {
        compileTimeFinalizationInvariant("finite compile-time callable result discriminant exceeded member count");
    }
    return @enumFromInt(discriminant);
}

fn callableResultMember(
    members: []const checked_artifact.CallableResultMemberPlan,
    member_id: check.CanonicalNames.CallableSetMemberId,
) ?checked_artifact.CallableResultMemberPlan {
    for (members) |member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn callableSetDescriptor(
    descriptors: []const mir.LambdaSolved.Representation.CanonicalCallableSetDescriptor,
    key: check.CanonicalNames.CanonicalCallableSetKey,
) ?*const mir.LambdaSolved.Representation.CanonicalCallableSetDescriptor {
    for (descriptors) |*descriptor| {
        if (mir.LambdaSolved.Representation.callableSetKeyEql(descriptor.key, key)) return descriptor;
    }
    return null;
}

fn callableSetMember(
    descriptor: *const mir.LambdaSolved.Representation.CanonicalCallableSetDescriptor,
    member_id: check.CanonicalNames.CallableSetMemberId,
) ?*const check.CanonicalNames.CanonicalCallableSetMember {
    for (descriptor.members) |*member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn compileTimeRootForRequest(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    request: checked_artifact.RootRequest,
) checked_artifact.CompileTimeRoot {
    for (artifact.compile_time_roots.roots) |root| {
        if (!rootMatchesRequest(root, request)) continue;
        return root;
    }
    compileTimeFinalizationInvariant("compile-time root request had no matching root record");
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

const ReifiedValue = struct {
    schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValueId,
};

const PhysicalValue = struct {
    layout_idx: layout_mod.Idx,
    value: Value,
};

const ComptimeReifier = struct {
    allocator: Allocator,
    values: *checked_artifact.CompileTimeValueStore,
    plans: *const checked_artifact.CompileTimePlanStore,
    checked_types: *const checked_artifact.CheckedTypeStore,
    layouts: *const layout_mod.Store,
    callable_set_descriptors: []const mir.LambdaSolved.Representation.CanonicalCallableSetDescriptor,

    fn reifyPlan(
        self: *ComptimeReifier,
        plan_id: checked_artifact.ConstGraphReificationPlanId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const plan = self.plans.constGraph(plan_id);
        return switch (plan) {
            .pending => reifierInvariant("compile-time reification reached pending const graph plan"),
            .scalar => self.reifyScalar(layout_idx, value),
            .string => self.reifyStr(layout_idx, value),
            .list => |list| self.reifyListPlan(list.elem, layout_idx, value),
            .box => |box| self.reifyBoxPlan(box.payload, layout_idx, value),
            .tuple => |items| self.reifyTuplePlan(items, layout_idx, value),
            .record => |fields| self.reifyRecordPlan(fields, layout_idx, value),
            .tag_union => |variants| self.reifyTagUnionPlan(variants, layout_idx, value),
            .transparent_alias => |alias| self.reifyWrappedPlan(alias.alias, alias.backing, layout_idx, value, .alias),
            .nominal => |nominal| self.reifyWrappedPlan(nominal.nominal, nominal.backing, layout_idx, value, .nominal),
            .callable_leaf => |leaf| self.reifyCallableLeaf(leaf, layout_idx, value),
            .recursive_ref => |ref| self.reifyPlan(ref, layout_idx, value),
        };
    }

    fn schemaForPlan(
        self: *ComptimeReifier,
        plan_id: checked_artifact.ConstGraphReificationPlanId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const plan = self.plans.constGraph(plan_id);
        return switch (plan) {
            .pending => reifierInvariant("compile-time schema construction reached pending const graph plan"),
            .scalar => |checked_ty| self.schemaForScalarCheckedType(checked_ty),
            .string => self.values.addSchema(.str),
            .list => |list| self.values.addSchema(.{ .list = try self.schemaForPlan(list.elem) }),
            .box => |box| self.values.addSchema(.{ .box = try self.schemaForPlan(box.payload) }),
            .tuple => |items| self.schemaForTuplePlan(items),
            .record => |fields| self.schemaForRecordPlan(fields),
            .tag_union => |variants| self.schemaForTagUnionPlan(variants),
            .transparent_alias => |alias| self.values.addSchema(.{ .alias = .{
                .type_name = alias.alias,
                .backing = try self.schemaForPlan(alias.backing),
            } }),
            .nominal => |nominal| self.values.addSchema(.{ .nominal = .{
                .type_name = nominal.nominal,
                .backing = try self.schemaForPlan(nominal.backing),
                .is_opaque = false,
            } }),
            .callable_leaf => |leaf| self.schemaForCallableLeaf(leaf),
            .recursive_ref => |ref| self.schemaForPlan(ref),
        };
    }

    fn schemaForScalarCheckedType(
        self: *ComptimeReifier,
        checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const nominal = switch (self.checkedPayload(checked_ty)) {
            .nominal => |nominal| nominal,
            else => reifierInvariant("scalar const graph plan did not reference a nominal scalar type"),
        };
        const builtin_nominal = nominal.builtin orelse reifierInvariant("scalar const graph plan referenced non-builtin nominal type");
        return switch (builtin_nominal) {
            .u8 => self.values.addSchema(.{ .int = .u8 }),
            .i8 => self.values.addSchema(.{ .int = .i8 }),
            .u16 => self.values.addSchema(.{ .int = .u16 }),
            .i16 => self.values.addSchema(.{ .int = .i16 }),
            .u32 => self.values.addSchema(.{ .int = .u32 }),
            .i32 => self.values.addSchema(.{ .int = .i32 }),
            .u64 => self.values.addSchema(.{ .int = .u64 }),
            .i64 => self.values.addSchema(.{ .int = .i64 }),
            .u128 => self.values.addSchema(.{ .int = .u128 }),
            .i128 => self.values.addSchema(.{ .int = .i128 }),
            .f32 => self.values.addSchema(.{ .frac = .f32 }),
            .f64 => self.values.addSchema(.{ .frac = .f64 }),
            .dec => self.values.addSchema(.{ .frac = .dec }),
            .str,
            .list,
            .box,
            .bool,
            => reifierInvariant("scalar const graph plan referenced non-scalar builtin nominal type"),
        };
    }

    fn reifyZst(self: *ComptimeReifier) Allocator.Error!ReifiedValue {
        return .{
            .schema = try self.values.addSchema(.zst),
            .value = try self.values.addValue(.zst),
        };
    }

    fn reifyCallableLeaf(
        self: *ComptimeReifier,
        leaf: checked_artifact.CallableLeafReificationPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        return switch (leaf) {
            .already_resolved => |resolved| .{
                .schema = try self.values.addSchema(.{ .callable = callableLeafSourceFnTy(resolved) }),
                .value = try self.values.addValue(.{ .callable = resolved }),
            },
            .finite => |result_plan| blk: {
                const proc_value = existingNoCaptureCallableResult(
                    self.plans,
                    self.callable_set_descriptors,
                    self.layouts,
                    result_plan,
                    layout_idx,
                    value,
                );
                break :blk .{
                    .schema = try self.values.addSchema(.{ .callable = proc_value.source_fn_ty }),
                    .value = try self.values.addValue(.{ .callable = .{ .finite = .{ .proc_value = proc_value } } }),
                };
            },
            .erased_boxed,
            => reifierInvariant("callable constant leaf reification requires sealed callable promotion output"),
        };
    }

    fn schemaForCallableLeaf(
        self: *ComptimeReifier,
        leaf: checked_artifact.CallableLeafReificationPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        return switch (leaf) {
            .already_resolved => |resolved| self.values.addSchema(.{ .callable = callableLeafSourceFnTy(resolved) }),
            .finite => |result_plan| switch (self.plans.callableResult(result_plan)) {
                .finite => |finite| self.values.addSchema(.{ .callable = finite.source_fn_ty }),
                .erased => |erased| self.values.addSchema(.{ .callable = erased.source_fn_ty }),
            },
            .erased_boxed,
            => reifierInvariant("callable constant leaf schema requires sealed callable promotion output"),
        };
    }

    fn reifyWrappedPlan(
        self: *ComptimeReifier,
        type_name: check.CanonicalNames.NominalTypeKey,
        backing_plan: checked_artifact.ConstGraphReificationPlanId,
        layout_idx: layout_mod.Idx,
        value: Value,
        comptime wrapper: enum { alias, nominal },
    ) Allocator.Error!ReifiedValue {
        const backing = try self.reifyPlan(backing_plan, layout_idx, value);
        const schema = switch (wrapper) {
            .alias => try self.values.addSchema(.{ .alias = .{
                .type_name = type_name,
                .backing = backing.schema,
            } }),
            .nominal => try self.values.addSchema(.{ .nominal = .{
                .type_name = type_name,
                .backing = backing.schema,
                .is_opaque = false,
            } }),
        };
        return .{
            .schema = schema,
            .value = switch (wrapper) {
                .alias => try self.values.addValue(.{ .alias = backing.value }),
                .nominal => try self.values.addValue(.{ .nominal = backing.value }),
            },
        };
    }

    fn reifyListPlan(
        self: *ComptimeReifier,
        elem_plan: checked_artifact.ConstGraphReificationPlanId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const layout = self.layouts.getLayout(layout_idx);
        const elem_layout_idx = switch (layout.tag) {
            .list => layout.data.list,
            .list_of_zst => layout_mod.Idx.zst,
            else => reifierInvariant("List(T) const graph plan did not lower to list layout"),
        };
        const elem_layout = self.layouts.getLayout(elem_layout_idx);
        const elem_size: usize = @intCast(self.layouts.layoutSize(elem_layout));
        const elem_schema = try self.schemaForPlan(elem_plan);

        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        const items = try self.allocator.alloc(checked_artifact.ComptimeValueId, roc_list.len());
        errdefer self.allocator.free(items);

        var i: usize = 0;
        while (i < roc_list.len()) : (i += 1) {
            const elem_value = if (elem_size == 0)
                Value.zst
            else
                Value{ .ptr = (roc_list.bytes orelse reifierInvariant("non-empty list had null bytes pointer")) + i * elem_size };
            items[i] = (try self.reifyPlan(elem_plan, elem_layout_idx, elem_value)).value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .list = elem_schema }),
            .value = try self.values.addValue(.{ .list = items }),
        };
    }

    fn reifyBoxPlan(
        self: *ComptimeReifier,
        payload_plan: checked_artifact.ConstGraphReificationPlanId,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const layout = self.layouts.getLayout(layout_idx);
        const elem_layout_idx = switch (layout.tag) {
            .box => layout.data.box,
            .box_of_zst => layout_mod.Idx.zst,
            else => reifierInvariant("Box(T) const graph plan did not lower to box layout"),
        };
        const child = if (layout.tag == .box_of_zst)
            try self.reifyPlan(payload_plan, elem_layout_idx, Value.zst)
        else blk: {
            const payload = value.read(?[*]u8) orelse reifierInvariant("Box(T) value had null payload pointer");
            break :blk try self.reifyPlan(payload_plan, elem_layout_idx, .{ .ptr = payload });
        };
        return .{
            .schema = try self.values.addSchema(.{ .box = child.schema }),
            .value = try self.values.addValue(.{ .box = child.value }),
        };
    }

    fn reifyRecordPlan(
        self: *ComptimeReifier,
        fields: []const checked_artifact.ConstRecordFieldPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        if (fields.len == 0) return self.reifyZst();
        const physical = self.logicalAggregateValue(layout_idx, value, .struct_);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag != .struct_) reifierInvariant("record const graph plan did not lower to struct layout");

        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        const value_fields = try self.allocator.alloc(checked_artifact.ComptimeValueId, fields.len);
        errdefer self.allocator.free(value_fields);

        for (fields, 0..) |field, i| {
            const field_layout_idx = self.layouts.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const field_layout = self.layouts.getLayout(field_layout_idx);
            const offset = self.layouts.getStructFieldOffsetByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const field_value = if (self.layouts.layoutSize(field_layout) == 0) Value.zst else physical.value.offset(offset);
            const reified = try self.reifyPlan(field.value, field_layout_idx, field_value);
            schema_fields[i] = .{ .name = field.field, .schema = reified.schema };
            value_fields[i] = reified.value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .record = schema_fields }),
            .value = try self.values.addValue(.{ .record = value_fields }),
        };
    }

    fn schemaForRecordPlan(
        self: *ComptimeReifier,
        fields: []const checked_artifact.ConstRecordFieldPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (fields.len == 0) return self.values.addSchema(.zst);

        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        for (fields, 0..) |field, i| {
            schema_fields[i] = .{
                .name = field.field,
                .schema = try self.schemaForPlan(field.value),
            };
        }
        return self.values.addSchema(.{ .record = schema_fields });
    }

    fn reifyTuplePlan(
        self: *ComptimeReifier,
        items: []const checked_artifact.ConstTupleElemPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        if (items.len == 0) return self.reifyZst();
        const physical = self.logicalAggregateValue(layout_idx, value, .struct_);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag != .struct_) reifierInvariant("tuple const graph plan did not lower to struct layout");

        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        const values = try self.allocator.alloc(checked_artifact.ComptimeValueId, items.len);
        errdefer self.allocator.free(values);

        for (items, 0..) |item, i| {
            const item_layout_idx = self.layouts.getStructFieldLayoutByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const item_layout = self.layouts.getLayout(item_layout_idx);
            const offset = self.layouts.getStructFieldOffsetByOriginalIndex(layout.data.struct_.idx, @intCast(i));
            const item_value = if (self.layouts.layoutSize(item_layout) == 0) Value.zst else physical.value.offset(offset);
            const reified = try self.reifyPlan(item.value, item_layout_idx, item_value);
            schemas[i] = reified.schema;
            values[i] = reified.value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .tuple = schemas }),
            .value = try self.values.addValue(.{ .tuple = values }),
        };
    }

    fn schemaForTuplePlan(
        self: *ComptimeReifier,
        items: []const checked_artifact.ConstTupleElemPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (items.len == 0) return self.values.addSchema(.zst);

        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        for (items, 0..) |item, i| {
            schemas[i] = try self.schemaForPlan(item.value);
        }
        return self.values.addSchema(.{ .tuple = schemas });
    }

    fn reifyTagUnionPlan(
        self: *ComptimeReifier,
        variants_plan: []const checked_artifact.ConstTagVariantPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const physical = self.logicalAggregateValue(layout_idx, value, .tag_union);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag != .tag_union) reifierInvariant("tag union const graph plan did not lower to tag-union layout");
        const info = self.layouts.getTagUnionInfo(layout);
        const discriminant = info.data.readDiscriminant(physical.value.ptr);
        if (discriminant >= variants_plan.len) reifierInvariant("tag union discriminant was outside const graph plan");

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

        const active_variant = variants_plan[discriminant];
        const active_payload_layout = info.variants.get(@intCast(discriminant)).payload_layout;
        const payload_values = try self.allocator.alloc(checked_artifact.ComptimeValueId, active_variant.payloads.len);
        errdefer self.allocator.free(payload_values);
        for (active_variant.payloads, 0..) |payload_plan, payload_i| {
            const arg_layout_idx = payloadLayoutForTagArg(self.layouts, active_payload_layout, active_variant.payloads.len, @intCast(payload_i));
            const arg_layout = self.layouts.getLayout(arg_layout_idx);
            const offset = payloadOffsetForTagArg(self.layouts, active_payload_layout, active_variant.payloads.len, @intCast(payload_i));
            const arg_value = if (self.layouts.layoutSize(arg_layout) == 0) Value.zst else physical.value.offset(offset);
            payload_values[payload_i] = (try self.reifyPlan(payload_plan.value, arg_layout_idx, arg_value)).value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .tag_union = variants }),
            .value = try self.values.addValue(.{ .tag_union = .{
                .variant_index = discriminant,
                .payloads = payload_values,
            } }),
        };
    }

    fn schemaForTagUnionPlan(
        self: *ComptimeReifier,
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

        return self.values.addSchema(.{ .tag_union = variants });
    }

    fn reifyScalar(
        self: *ComptimeReifier,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const layout = self.layouts.getLayout(layout_idx);
        if (layout.tag != .scalar) reifierInvariant("scalar checked type did not lower to scalar layout");
        const scalar = layout.data.scalar;
        return switch (scalar.tag) {
            .str => self.reifyStr(layout_idx, value),
            .int => blk: {
                var bytes = [_]u8{0} ** 16;
                const size: usize = @intCast(self.layouts.layoutSize(layout));
                @memcpy(bytes[0..size], value.readBytes(size));
                break :blk .{
                    .schema = try self.values.addSchema(.{ .int = scalar.data.int }),
                    .value = try self.values.addValue(.{ .int_bytes = bytes }),
                };
            },
            .frac => switch (scalar.data.frac) {
                .f32 => .{
                    .schema = try self.values.addSchema(.{ .frac = .f32 }),
                    .value = try self.values.addValue(.{ .f32 = value.read(f32) }),
                },
                .f64 => .{
                    .schema = try self.values.addSchema(.{ .frac = .f64 }),
                    .value = try self.values.addValue(.{ .f64 = value.read(f64) }),
                },
                .dec => blk: {
                    var bytes = [_]u8{0} ** 16;
                    @memcpy(bytes[0..16], value.readBytes(16));
                    break :blk .{
                        .schema = try self.values.addSchema(.{ .frac = .dec }),
                        .value = try self.values.addValue(.{ .dec = bytes }),
                    };
                },
            },
            .opaque_ptr => reifierInvariant("compile-time constants cannot reify opaque pointers"),
        };
    }

    fn reifyStr(
        self: *ComptimeReifier,
        _: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
        const owned = try self.allocator.dupe(u8, roc_str.asSlice());
        errdefer self.allocator.free(owned);
        return .{
            .schema = try self.values.addSchema(.str),
            .value = try self.values.addValue(.{ .str = owned }),
        };
    }

    fn checkedPayload(self: *const ComptimeReifier, ty: checked_artifact.CheckedTypeId) checked_artifact.CheckedTypePayload {
        return self.checked_types.payloads[@intFromEnum(ty)];
    }

    fn logicalAggregateValue(
        self: *const ComptimeReifier,
        layout_idx: layout_mod.Idx,
        value: Value,
        expected_tag: layout_mod.LayoutTag,
    ) PhysicalValue {
        const layout = self.layouts.getLayout(layout_idx);
        switch (layout.tag) {
            .box => {
                const payload = value.read(?[*]u8) orelse reifierInvariant("logical aggregate used boxed physical layout with null payload");
                const inner_layout = self.layouts.getLayout(layout.data.box);
                if (inner_layout.tag != expected_tag) {
                    reifierInvariant("logical aggregate physical box did not contain expected layout tag");
                }
                return .{ .layout_idx = layout.data.box, .value = .{ .ptr = payload } };
            },
            .box_of_zst => {
                if (expected_tag != .zst) reifierInvariant("logical aggregate used box_of_zst for non-ZST layout");
                return .{ .layout_idx = .zst, .value = Value.zst };
            },
            else => return .{ .layout_idx = layout_idx, .value = value },
        }
    }
};

fn callableLeafSourceFnTy(
    leaf: checked_artifact.CallableLeafInstance,
) check.CanonicalNames.CanonicalTypeKey {
    return switch (leaf) {
        .finite => |finite| finite.proc_value.source_fn_ty,
        .erased_boxed => |erased| erased.source_fn_ty,
    };
}

fn payloadLayoutForTagArg(
    layouts: *const layout_mod.Store,
    variant_layout_idx: layout_mod.Idx,
    arg_count: usize,
    arg_index: u32,
) layout_mod.Idx {
    if (arg_count == 0) return layout_mod.Idx.zst;
    if (arg_count == 1) return variant_layout_idx;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (variant_layout.tag != .struct_) reifierInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldLayoutByOriginalIndex(variant_layout.data.struct_.idx, arg_index);
}

fn payloadOffsetForTagArg(
    layouts: *const layout_mod.Store,
    variant_layout_idx: layout_mod.Idx,
    arg_count: usize,
    arg_index: u32,
) u32 {
    if (arg_count <= 1) return 0;
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (variant_layout.tag != .struct_) reifierInvariant("multi-payload tag did not use struct payload layout");
    return layouts.getStructFieldOffsetByOriginalIndex(variant_layout.data.struct_.idx, arg_index);
}

fn reifierInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time reifier invariant violated: " ++ message, .{});
    }
    unreachable;
}

fn compileTimeFinalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time finalization invariant violated: " ++ message, .{});
    }
    unreachable;
}

test "compile-time finalization tests" {
    std.testing.refAllDecls(@This());
}
