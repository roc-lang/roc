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
const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;
const repr = mir.LambdaSolved.Representation;

/// Public `finalizer` function.
pub fn finalizer() checked_artifact.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

fn finalize(
    _: ?*anyopaque,
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    imports: []const checked_artifact.PublishImportArtifact,
    available_artifacts: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
) anyerror!void {
    const compile_time_roots = try compileTimeRootRequests(allocator, artifact.root_requests.requests);
    defer if (compile_time_roots.len > 0) allocator.free(compile_time_roots);

    const import_views = try allocator.alloc(checked_artifact.ImportedModuleView, imports.len);
    defer allocator.free(import_views);
    for (imports, 0..) |import, i| {
        import_views[i] = import.view;
    }

    const lowering_imports = try loweringArtifactViews(
        allocator,
        artifact,
        import_views,
        available_artifacts,
        relation_artifacts,
    );
    defer if (lowering_imports.len > 0) allocator.free(lowering_imports);

    const dependency_views = try dependencyArtifactViews(allocator, lowering_imports, relation_artifacts);
    defer if (dependency_views.len > 0) allocator.free(dependency_views);

    var dependency_summaries = try lir.CheckedPipeline.summarizeCompileTimeDependencies(
        allocator,
        .{
            .root = checked_artifact.loweringViewWithRelations(artifact, relation_artifacts),
            .imports = lowering_imports,
        },
        .{
            .requests = artifact.root_requests.requests,
            .purpose = .compile_time,
            .compile_time_plan_sink = &artifact.comptime_plans,
            .compile_time_artifact_sink = artifact,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    defer dependency_summaries.deinit();

    var runtime_env = RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    if (compile_time_roots.len == 0) {
        try finalizeRuntimeDependencySummaries(allocator, artifact, dependency_views, lowering_imports, relation_artifacts, &runtime_env);
        try artifact.comptime_values.sealBindings();
        return;
    }

    const ordered_roots = try orderCompileTimeRootRequests(allocator, artifact, compile_time_roots);
    defer allocator.free(ordered_roots);

    for (ordered_roots) |root_request| {
        try ensureRootConcreteDependencies(
            allocator,
            artifact,
            dependency_views,
            lowering_imports,
            relation_artifacts,
            &runtime_env,
            root_request,
        );

        const root = compileTimeRootForRequest(artifact, root_request);
        if (try publishAlreadyEvaluatedConstantRoot(
            allocator,
            artifact,
            root,
        )) continue;

        var lowered_request = try lowerSingleCompileTimeRequest(
            allocator,
            artifact,
            lowering_imports,
            relation_artifacts,
            .{ .local_root = root_request },
        );
        defer lowered_request.deinit();

        var interpreter = try Interpreter.init(
            allocator,
            &lowered_request.lowered.lir_result.store,
            &lowered_request.lowered.lir_result.layouts,
            runtime_env.get_ops(),
        );
        defer interpreter.deinit();

        const payload = switch (lowered_request.payload) {
            .local_root => |local| local,
            else => compileTimeFinalizationInvariant("local compile-time root lowering did not publish a local-root payload"),
        };
        artifact.compile_time_roots.fillPayload(root.id, payload);
        switch (root.kind) {
            .constant => try evaluateConstantRoot(
                allocator,
                artifact,
                &interpreter,
                &lowered_request.lowered,
                root,
                lowered_request.lir_root,
                switch (payload) {
                    .const_graph => |plan| plan,
                    else => compileTimeFinalizationInvariant("constant root did not publish a const graph plan"),
                },
            ),
            .callable_binding => try evaluateCallableBindingRoot(
                allocator,
                artifact,
                &interpreter,
                &lowered_request.lowered,
                root,
                lowered_request.lir_root,
                switch (payload) {
                    .callable_result => |plan| plan,
                    else => compileTimeFinalizationInvariant("callable root did not publish a callable result plan"),
                },
            ),
            .expect => {},
        }
    }

    try finalizeRuntimeDependencySummaries(allocator, artifact, dependency_views, lowering_imports, relation_artifacts, &runtime_env);

    try artifact.comptime_values.sealBindings();
}

fn finalizeRuntimeDependencySummaries(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
) anyerror!void {
    if (artifactHasUnboundPlatformRequirements(artifact)) return;

    try ensurePlatformRequiredConstInstances(
        allocator,
        artifact,
        dependency_views,
        lowering_imports,
        relation_artifacts,
        runtime_env,
    );

    var runtime_dependency_summaries = try lir.CheckedPipeline.summarizeCompileTimeDependencies(
        allocator,
        .{
            .root = checked_artifact.loweringViewWithRelations(artifact, relation_artifacts),
            .imports = lowering_imports,
        },
        .{
            .requests = artifact.root_requests.requests,
            .purpose = .runtime,
            .compile_time_artifact_sink = artifact,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    defer runtime_dependency_summaries.deinit();

    try ensureDependencySummaryIdsConcreteDependencies(
        allocator,
        artifact,
        dependency_views,
        lowering_imports,
        relation_artifacts,
        runtime_env,
        runtime_dependency_summaries.dependency_summaries,
    );
}

fn artifactHasUnboundPlatformRequirements(artifact: *const checked_artifact.CheckedModuleArtifact) bool {
    return artifact.platform_required_declarations.declarations.len != 0 and
        artifact.platform_required_bindings.bindings.len == 0;
}

fn ensurePlatformRequiredConstInstances(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
) anyerror!void {
    for (artifact.platform_required_bindings.bindings) |binding| {
        const const_use = switch (binding.value_use) {
            .const_value => |platform_const| platform_const.const_use,
            .procedure_value => continue,
        };
        const request = try constInstantiationRequestForUse(
            allocator,
            artifact,
            dependency_views,
            const_use,
        );
        _ = try ensureConstInstanceRequest(
            allocator,
            artifact,
            dependency_views,
            lowering_imports,
            relation_artifacts,
            runtime_env,
            request,
        );
    }
}

fn constInstantiationRequestForUse(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    const_use: checked_artifact.ConstUseTemplate,
) Allocator.Error!checked_artifact.ConstInstantiationRequest {
    const key = if (try concreteConstProducerKeyForRef(
        allocator,
        artifact,
        dependency_views,
        const_use.const_ref,
    )) |producer_key|
        producer_key
    else
        const_use.requested_source_ty_template;

    const instance_key = checked_artifact.ConstInstantiationKey{
        .const_ref = const_use.const_ref,
        .requested_source_ty = key,
    };
    return .{
        .key = instance_key,
        .requested_source_ty_payload = try constInstantiationPayloadForKey(
            allocator,
            artifact,
            dependency_views,
            const_use,
            instance_key,
        ),
    };
}

fn concreteConstProducerKeyForRef(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    ref: checked_artifact.ConstRef,
) Allocator.Error!?canonical.CanonicalTypeKey {
    const source = constTemplateSourceForRef(artifact, dependency_views, ref);
    const scheme = source.checked_types.schemeForKey(ref.source_scheme) orelse {
        compileTimeFinalizationInvariant("constant use referenced a missing producer source scheme");
    };
    if (!try source.checked_types.isConcreteConstProducerScheme(allocator, scheme.root)) return null;
    return source.checked_types.rootKey(scheme.root);
}

fn constInstantiationPayloadForKey(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    const_use: checked_artifact.ConstUseTemplate,
    key: checked_artifact.ConstInstantiationKey,
) Allocator.Error!checked_artifact.CheckedTypeId {
    if (std.meta.eql(key.requested_source_ty.bytes, const_use.requested_source_ty_template.bytes)) {
        return const_use.requested_source_ty_payload orelse {
            compileTimeFinalizationInvariant("constant use had no requested source type payload");
        };
    }
    return try checkedTypePayloadForConstInstanceDependency(
        allocator,
        artifact,
        dependency_views,
        key,
    );
}

fn publishAlreadyEvaluatedConstantRoot(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    root: checked_artifact.CompileTimeRoot,
) anyerror!bool {
    if (root.kind != .constant) return false;

    const request = constInstantiationRequestForConstantRoot(artifact, root);
    const instance_ref = try artifact.const_instances.reserveRequest(allocator, &artifact.checked_types, request);
    switch (artifact.const_instances.stateForRef(instance_ref)) {
        .reserved => return false,
        .evaluating => compileTimeFinalizationInvariant("compile-time root constant instance was still evaluating when root was reached"),
        .evaluated => |instance| {
            try publishConstantRootFromInstance(artifact, root, instance);
            return true;
        },
    }
}

fn publishConstantRootFromInstance(
    artifact: *checked_artifact.CheckedModuleArtifact,
    root: checked_artifact.CompileTimeRoot,
    instance: checked_artifact.ConstInstance,
) Allocator.Error!void {
    const pattern = root.pattern orelse compileTimeFinalizationInvariant("constant root had no top-level pattern");
    const reification_plan = instance.reification_plan orelse {
        compileTimeFinalizationInvariant("evaluated constant root instance had no reification plan");
    };
    switch (artifact.compile_time_roots.root(root.id).payload) {
        .pending => {},
        else => compileTimeFinalizationInvariant("compile-time root was published more than once"),
    }
    try artifact.comptime_values.bind(pattern, instance.schema, instance.value);
    artifact.compile_time_roots.fillPayload(root.id, .{ .const_graph = reification_plan });
}

fn constInstantiationRequestForConstantRoot(
    artifact: *checked_artifact.CheckedModuleArtifact,
    root: checked_artifact.CompileTimeRoot,
) checked_artifact.ConstInstantiationRequest {
    const pattern = root.pattern orelse compileTimeFinalizationInvariant("constant root had no top-level pattern");
    const top_level = artifact.top_level_values.lookupByPattern(pattern) orelse {
        compileTimeFinalizationInvariant("constant root had no top-level value entry");
    };
    const const_ref = switch (top_level.value) {
        .const_ref => |ref| ref,
        .procedure_binding => compileTimeFinalizationInvariant("constant root top-level value was a procedure binding"),
    };
    const requested_source_ty = artifact.checked_types.roots[@intFromEnum(root.checked_type)].key;
    return .{
        .key = .{
            .const_ref = const_ref,
            .requested_source_ty = requested_source_ty,
        },
        .requested_source_ty_payload = root.checked_type,
    };
}

const LoweredCompileTimeRequest = struct {
    lowered: lir.CheckedPipeline.LoweredProgram,
    lir_root: lir.LIR.LirProcSpecId,
    payload: checked_artifact.CompileTimeEvaluationPayload,

    fn deinit(self: *LoweredCompileTimeRequest) void {
        self.lowered.deinit();
    }
};

fn lowerSingleCompileTimeRequest(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    request: checked_artifact.CompileTimeEvaluationRequest,
) anyerror!LoweredCompileTimeRequest {
    const single_request = [_]checked_artifact.CompileTimeEvaluationRequest{request};
    var lowered = try lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{
            .root = checked_artifact.loweringViewWithRelations(artifact, relation_artifacts),
            .imports = import_views,
        },
        .{
            .compile_time_requests = &single_request,
            .purpose = .compile_time,
            .compile_time_plan_sink = &artifact.comptime_plans,
            .compile_time_artifact_sink = artifact,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    errdefer lowered.deinit();

    if (lowered.lir_result.root_procs.items.len != 1) {
        compileTimeFinalizationInvariant("single compile-time request lowering did not produce exactly one LIR root");
    }

    if (lowered.compile_time_payloads.len != 1) {
        compileTimeFinalizationInvariant("single compile-time request lowering did not publish exactly one payload");
    }

    return .{
        .lowered = lowered,
        .lir_root = lowered.lir_result.root_procs.items[0],
        .payload = lowered.compile_time_payloads[0],
    };
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

fn dependencyArtifactViews(
    allocator: Allocator,
    import_views: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
) Allocator.Error![]checked_artifact.ImportedModuleView {
    var views = std.ArrayList(checked_artifact.ImportedModuleView).empty;
    errdefer views.deinit(allocator);

    for (import_views) |view| {
        try appendDependencyArtifactView(&views, allocator, view);
    }
    for (relation_artifacts) |view| {
        try appendDependencyArtifactView(&views, allocator, view);
    }

    return try views.toOwnedSlice(allocator);
}

fn loweringArtifactViews(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    available_artifacts: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
) Allocator.Error![]checked_artifact.ImportedModuleView {
    var views = std.ArrayList(checked_artifact.ImportedModuleView).empty;
    errdefer views.deinit(allocator);

    for (import_views) |view| {
        try appendDependencyArtifactView(&views, allocator, view);
    }

    var keys = std.ArrayList(checked_artifact.CheckedModuleArtifactKey).empty;
    defer keys.deinit(allocator);

    for (artifact.platform_required_bindings.bindings) |binding| {
        const relation_artifact = artifactViewForKey(relation_artifacts, binding.app_value.artifact) orelse {
            compileTimeFinalizationInvariant("platform relation binding referenced an unavailable relation artifact");
        };
        try checked_artifact.appendPlatformRelationDependencyArtifactKeysFromView(
            allocator,
            &keys,
            relation_artifact,
            binding,
        );
    }

    for (keys.items) |key| {
        if (checkedArtifactKeyEql(key, artifact.key)) continue;
        if (artifactViewForKey(relation_artifacts, key) != null) continue;
        const view = artifactViewForKey(import_views, key) orelse artifactViewForKey(available_artifacts, key) orelse {
            compileTimeFinalizationInvariant("relation closure referenced a checked artifact that was not published to finalization availability");
        };
        try appendDependencyArtifactView(&views, allocator, view);
    }

    return try views.toOwnedSlice(allocator);
}

fn appendDependencyArtifactView(
    views: *std.ArrayList(checked_artifact.ImportedModuleView),
    allocator: Allocator,
    candidate: checked_artifact.ImportedModuleView,
) Allocator.Error!void {
    for (views.items) |existing| {
        if (std.meta.eql(existing.key.bytes, candidate.key.bytes)) return;
    }
    try views.append(allocator, candidate);
}

fn artifactViewForKey(
    views: []const checked_artifact.ImportedModuleView,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.ImportedModuleView {
    for (views) |view| {
        if (checkedArtifactKeyEql(view.key, key)) return view;
    }
    return null;
}

fn checkedArtifactKeyEql(
    a: checked_artifact.CheckedModuleArtifactKey,
    b: checked_artifact.CheckedModuleArtifactKey,
) bool {
    return std.meta.eql(a.bytes, b.bytes);
}

fn orderCompileTimeRootRequests(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    roots: []const checked_artifact.RootRequest,
) Allocator.Error![]checked_artifact.RootRequest {
    var root_to_request = std.AutoHashMap(checked_artifact.ComptimeRootId, usize).init(allocator);
    defer root_to_request.deinit();

    for (roots, 0..) |request, i| {
        const root = compileTimeRootForRequest(artifact, request);
        try root_to_request.put(root.id, i);
    }

    var graph = try buildCompileTimeRootDependencyGraph(allocator, artifact, roots, &root_to_request);
    defer graph.deinit(allocator);

    const emitted = try allocator.alloc(bool, roots.len);
    defer allocator.free(emitted);
    @memset(emitted, false);

    var ordered = std.ArrayList(checked_artifact.RootRequest).empty;
    errdefer ordered.deinit(allocator);

    var remaining = roots.len;
    while (remaining != 0) {
        var progressed = false;
        for (roots, 0..) |request, i| {
            if (emitted[i]) continue;
            const root = compileTimeRootForRequest(artifact, request);
            if (!localRootDependenciesSatisfied(root.id, graph.edges, &root_to_request, emitted)) continue;

            try ordered.append(allocator, request);
            emitted[i] = true;
            remaining -= 1;
            progressed = true;
        }
        if (!progressed) {
            compileTimeFinalizationInvariant("compile-time root dependency graph contains a local-root cycle or missing prerequisite");
        }
    }

    return try ordered.toOwnedSlice(allocator);
}

fn buildCompileTimeRootDependencyGraph(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    roots: []const checked_artifact.RootRequest,
    selected_roots: *const std.AutoHashMap(checked_artifact.ComptimeRootId, usize),
) Allocator.Error!CompileTimeRootDependencyGraph {
    const nodes = try allocator.alloc(CompileTimeRootNode, roots.len);
    errdefer allocator.free(nodes);

    var edges = std.ArrayList(CompileTimeRootEdge).empty;
    errdefer edges.deinit(allocator);

    for (roots, 0..) |request, i| {
        const root = compileTimeRootForRequest(artifact, request);
        nodes[i] = switch (root.kind) {
            .constant => .{ .compile_time_constant_root = root.id },
            .callable_binding => .{ .callable_binding_root = root.id },
            .expect => .{ .expect_root = root.id },
        };
        const summary = artifact.comptime_dependencies.summaryForRootRequest(root.dependency_summary_request);
        for (summary.availability_values) |availability| {
            const prerequisite: ?CompileTimeRootPrerequisite = switch (availability) {
                .local_root => |dependency| blk: {
                    if (selected_roots.contains(dependency)) {
                        break :blk .{ .local_root = dependency };
                    }
                    verifyUnselectedLocalRootDependencyCoveredByConcreteUse(artifact, summary, dependency);
                    break :blk null;
                },
                .imported_value => |imported| .{ .imported_value = imported },
                .const_template,
                .procedure_binding,
                => null,
            };
            if (prerequisite) |to| {
                try edges.append(allocator, .{
                    .from = root.id,
                    .to = to,
                    .reason = .{ .availability_value = availability },
                });
            }
        }
    }

    return .{
        .nodes = nodes,
        .edges = try edges.toOwnedSlice(allocator),
    };
}

fn localRootDependenciesSatisfied(
    root: checked_artifact.ComptimeRootId,
    edges: []const CompileTimeRootEdge,
    root_to_request: *const std.AutoHashMap(checked_artifact.ComptimeRootId, usize),
    emitted: []const bool,
) bool {
    for (edges) |edge| {
        if (edge.from != root) continue;
        switch (edge.to) {
            .local_root => |dependency| {
                const dependency_index = root_to_request.get(dependency) orelse {
                    compileTimeFinalizationInvariant("compile-time root dependency references a root outside the selected compile-time root set");
                };
                if (!emitted[dependency_index]) return false;
            },
            .imported_value => {},
        }
    }
    return true;
}

fn verifyUnselectedLocalRootDependencyCoveredByConcreteUse(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    summary: checked_artifact.ComptimeDependencySummary,
    dependency: checked_artifact.ComptimeRootId,
) void {
    const root = artifact.compile_time_roots.root(dependency);
    switch (root.kind) {
        .constant => {
            const pattern = root.pattern orelse {
                compileTimeFinalizationInvariant("unselected local constant dependency had no top-level pattern");
            };
            const top_level = artifact.top_level_values.lookupByPattern(pattern) orelse {
                compileTimeFinalizationInvariant("unselected local constant dependency had no top-level value");
            };
            const const_ref = switch (top_level.value) {
                .const_ref => |ref| ref,
                .procedure_binding => compileTimeFinalizationInvariant("unselected local constant dependency resolved to a procedure binding"),
            };
            for (summary.concrete_values) |value| {
                switch (value) {
                    .const_instance => |key| if (constRefEql(key.const_ref, const_ref)) return,
                    .callable_binding_instance,
                    .procedure_callable,
                    .procedure_callable_with_payloads,
                    => {},
                }
            }
            compileTimeFinalizationInvariant("unselected local constant dependency had no matching concrete const instance");
        },
        .callable_binding => {
            const pattern = root.pattern orelse {
                compileTimeFinalizationInvariant("unselected local callable dependency had no top-level pattern");
            };
            const top_level = artifact.top_level_values.lookupByPattern(pattern) orelse {
                compileTimeFinalizationInvariant("unselected local callable dependency had no top-level value");
            };
            const binding = switch (top_level.value) {
                .procedure_binding => |binding| checked_artifact.ProcedureBindingRef{ .top_level = binding },
                .const_ref => compileTimeFinalizationInvariant("unselected local callable dependency resolved to a const ref"),
            };
            for (summary.concrete_values) |value| {
                switch (value) {
                    .callable_binding_instance => |key| if (checked_artifact.procedureBindingRefEql(key.binding, binding)) return,
                    .const_instance,
                    .procedure_callable,
                    .procedure_callable_with_payloads,
                    => {},
                }
            }
            compileTimeFinalizationInvariant("unselected local callable dependency had no matching concrete callable instance");
        },
        .expect => compileTimeFinalizationInvariant("compile-time dependency summary referenced an unselected expect root"),
    }
}

fn constRefEql(a: checked_artifact.ConstRef, b: checked_artifact.ConstRef) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        constOwnerEql(a.owner, b.owner) and
        a.template == b.template and
        std.meta.eql(a.source_scheme.bytes, b.source_scheme.bytes);
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
    const requested_source_ty = artifact.checked_types.roots[@intFromEnum(root.checked_type)].key;
    const const_instance_key = checked_artifact.ConstInstantiationKey{
        .const_ref = const_ref,
        .requested_source_ty = requested_source_ty,
    };

    const result = try evalCompileTimeRoot(interpreter, lir_root);
    const ret_layout = lowered.lir_result.store.getProcSpec(lir_root).ret_layout;
    defer interpreter.dropValue(result.value, ret_layout);

    var dependencies = ConcreteDependencyCollector.init(allocator, artifact);
    defer dependencies.deinit();

    var reifier = ComptimeReifier{
        .allocator = allocator,
        .artifact = artifact,
        .values = &artifact.comptime_values,
        .plans = &artifact.comptime_plans,
        .checked_types = &artifact.checked_types,
        .layouts = &lowered.lir_result.layouts,
        .lowered = lowered,
        .callable_set_descriptors = lowered.callable_set_descriptors,
        .active_schemas = std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, checked_artifact.ComptimeSchemaId).init(allocator),
        .promotion_context = .{
            .source_binding = pattern,
            .base = .{ .local_const_root = .{
                .root = root.id,
                .instance = const_instance_key,
            } },
        },
        .dependencies = &dependencies,
    };
    defer reifier.deinit();
    const reified = try reifier.reifyPlan(reification_plan, ret_layout, result.value);
    const dependency_summary = try appendConcreteDependencySummary(allocator, artifact, root, dependencies.concrete.items);

    try artifact.comptime_values.bind(pattern, reified.schema, reified.value);

    const instance_ref = try artifact.const_instances.reserveRequest(allocator, &artifact.checked_types, .{
        .key = const_instance_key,
        .requested_source_ty_payload = root.checked_type,
    });
    const generated_procedures = try generatedProceduresForConstInstance(
        allocator,
        artifact,
        const_instance_key,
    );
    artifact.const_instances.fill(instance_ref, .{
        .schema = reified.schema,
        .value = reified.value,
        .dependency_summary = dependency_summary,
        .reification_plan = reification_plan,
        .generated_procedures = generated_procedures,
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

    const result = try evalCompileTimeRoot(interpreter, lir_root);
    const ret_layout = lowered.lir_result.store.getProcSpec(lir_root).ret_layout;
    defer interpreter.dropValue(result.value, ret_layout);

    const requested_source_fn_ty = artifact.checked_types.roots[@intFromEnum(root.checked_type)].key;
    const promotion_context = PromotedCallablePublicationContext{
        .source_binding = pattern,
        .base = .{ .local_callable_root = root.id },
    };
    const callable = switch (artifact.comptime_plans.callableResult(result_plan)) {
        .finite => blk: {
            const selected_callable = selectFiniteCallableResult(
                &artifact.comptime_plans,
                lowered.callable_set_descriptors,
                &lowered.lir_result.layouts,
                result_plan,
                ret_layout,
                result.value,
            );
            break :blk try publishCallableResult(
                allocator,
                artifact,
                lowered,
                promotion_context,
                root.checked_type,
                result_plan,
                selected_callable,
            );
        },
        .erased => |erased| try publishErasedCallableResult(
            allocator,
            artifact,
            lowered,
            promotion_context,
            root.checked_type,
            ret_layout,
            result.value,
            result_plan,
            erased,
        ),
    };
    if (!std.meta.eql(callable.proc_value.source_fn_ty.bytes, requested_source_fn_ty.bytes)) {
        compileTimeFinalizationInvariant("callable root result source type differed from checked root type");
    }
    const dependency_summary = try appendConcreteDependencySummaryForCallableRoot(
        allocator,
        artifact,
        root,
        result_plan,
        callable.proc_value,
    );

    const key = checked_artifact.CallableBindingInstantiationKey{
        .binding = .{ .top_level = binding_ref },
        .requested_source_fn_ty = requested_source_fn_ty,
    };
    const instance_ref = try artifact.callable_binding_instances.reserveRequest(allocator, &artifact.checked_types, .{
        .key = key,
        .requested_source_fn_ty_payload = root.checked_type,
    });
    artifact.callable_binding_instances.markEvaluating(instance_ref);
    const generated_procedures = try generatedProceduresForCallableBindingInstance(
        allocator,
        artifact,
        key,
    );
    artifact.callable_binding_instances.fill(instance_ref, .{
        .key = key,
        .dependency_summary = dependency_summary,
        .proc_value = callable.proc_value,
        .body = .{ .evaluated = .{
            .executable_root = .{ .local_root = root.id },
            .result_plan = result_plan,
            .promotion_plan = callable.promotion_plan,
            .promotion_output = callable.output,
        } },
        .generated_procedures = generated_procedures,
    });
}

fn evalCompileTimeRoot(
    interpreter: *Interpreter,
    lir_root: lir.LIR.LirProcSpecId,
) Allocator.Error!Interpreter.EvalResult {
    return interpreter.eval(.{
        .proc_id = lir_root,
        .arg_layouts = &.{},
    }) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.RuntimeError => compileTimeFinalizationInvariant("compile-time root produced a runtime error"),
        error.DivisionByZero => compileTimeFinalizationInvariant("compile-time root divided by zero"),
        error.Crash => compileTimeFinalizationInvariant("compile-time root crashed"),
    };
}

const SelectedFiniteCallableResult = struct {
    result_plan_id: checked_artifact.CallableResultPlanId,
    result_plan: checked_artifact.FiniteCallableResultPlan,
    planned_member: checked_artifact.CallableResultMemberPlan,
    descriptor_member: *const repr.CanonicalCallableSetMember,
    payload_layout: layout_mod.Idx,
    payload_value: Value,
};

const PublishedCallableResult = struct {
    proc_value: check.CanonicalNames.ProcedureCallableRef,
    source_fn_ty_payload: checked_artifact.CheckedTypeId,
    output: checked_artifact.CallablePromotionOutput,
    promotion_plan: ?checked_artifact.CallablePromotionPlanId,
    generated_procedure: ?checked_artifact.SemanticInstantiationProcedureId = null,
};

const PromotedCallablePublicationContext = struct {
    source_binding: ?checked_artifact.CheckedPatternId,
    base: PromotedCallableProvenanceBase,
};

const PromotedCallableProvenanceBase = union(enum) {
    local_callable_root: checked_artifact.ComptimeRootId,
    local_const_root: struct {
        root: checked_artifact.ComptimeRootId,
        instance: checked_artifact.ConstInstantiationKey,
    },
    callable_binding_instance: checked_artifact.CallableBindingInstantiationKey,
    const_instance: checked_artifact.ConstInstantiationKey,
    private_capture: PromotedPrivateCaptureOwner,
};

const PromotedPrivateCaptureOwner = struct {
    promoted_ref: checked_artifact.PromotedProcedureRef,
    template: canonical.ProcedureTemplateRef,
    source_fn_ty: canonical.CanonicalTypeKey,
};

fn promotedProcedureProvenance(
    context: PromotedCallablePublicationContext,
    result_plan: checked_artifact.CallableResultPlanId,
) checked_artifact.PromotedProcedureProvenance {
    return switch (context.base) {
        .local_callable_root => |root| .{ .local_callable_root_result = .{
            .root = root,
            .result_plan = result_plan,
        } },
        .local_const_root => |local| .{ .local_const_root_callable_leaf = .{
            .root = local.root,
            .instance = local.instance,
            .result_plan = result_plan,
            .value_path = comptimeValuePathKeyForCallableResult(result_plan),
        } },
        .callable_binding_instance => |instance| .{ .callable_binding_instance_result = .{
            .instance = instance,
            .result_plan = result_plan,
            .callable_path = promotedCallablePathKeyForCallableResult(result_plan),
        } },
        .const_instance => |instance| .{ .const_instance_callable_leaf = .{
            .instance = instance,
            .result_plan = result_plan,
            .value_path = comptimeValuePathKeyForCallableResult(result_plan),
        } },
        .private_capture => |owner| .{ .private_capture_callable_leaf = .{
            .promoted_proc = owner.promoted_ref,
            .result_plan = result_plan,
            .capture_path = privateCapturePathKeyForCallableResult(result_plan),
        } },
    };
}

fn privateCaptureOwner(
    reserved: checked_artifact.ReservedPromotedCallableWrapper,
) PromotedPrivateCaptureOwner {
    return .{
        .promoted_ref = reserved.promoted_ref,
        .template = reserved.template,
        .source_fn_ty = reserved.source_fn_ty,
    };
}

fn privateCaptureOwnerMirProcedureRef(
    owner: PromotedPrivateCaptureOwner,
) canonical.MirProcedureRef {
    return .{
        .proc = owner.promoted_ref.proc,
        .callable = .{
            .template = .{ .synthetic = .{ .template = owner.template } },
            .source_fn_ty = owner.source_fn_ty,
        },
    };
}

fn semanticInstantiationKeyForPromotedProcedure(
    provenance: checked_artifact.PromotedProcedureProvenance,
    source_fn_ty: canonical.CanonicalTypeKey,
) ?checked_artifact.SemanticInstantiationProcedureKey {
    return switch (provenance) {
        .local_callable_root_result => null,
        .local_const_root_callable_leaf => |local| .{ .const_instance_callable_leaf = .{
            .instance = local.instance,
            .value_path = local.value_path,
            .source_fn_ty = source_fn_ty,
        } },
        .callable_binding_instance_result => |callable| .{ .callable_binding_promoted_leaf = .{
            .instance = callable.instance,
            .callable_path = callable.callable_path,
            .source_fn_ty = source_fn_ty,
        } },
        .const_instance_callable_leaf => |instance| .{ .const_instance_callable_leaf = .{
            .instance = instance.instance,
            .value_path = instance.value_path,
            .source_fn_ty = source_fn_ty,
        } },
        .private_capture_callable_leaf => |private| .{ .private_capture_callable_leaf = .{
            .promoted_proc = private.promoted_proc,
            .capture_path = private.capture_path,
            .source_fn_ty = source_fn_ty,
        } },
    };
}

fn publishSemanticInstantiationProcedureForPromoted(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    reserved: checked_artifact.ReservedPromotedCallableWrapper,
    source_fn_ty: canonical.CanonicalTypeKey,
) Allocator.Error!?checked_artifact.SemanticInstantiationProcedureId {
    const key = semanticInstantiationKeyForPromotedProcedure(reserved.provenance, source_fn_ty) orelse return null;
    return try artifact.semantic_instantiation_procedures.publish(allocator, key, .{
        .template = .{ .synthetic = .{ .template = reserved.template } },
        .proc_value = reserved.proc_value,
        .promoted = reserved.promoted_ref,
    });
}

fn generatedProceduresForConstInstance(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    instance: checked_artifact.ConstInstantiationKey,
) Allocator.Error![]const checked_artifact.SemanticInstantiationProcedureId {
    var ids = std.ArrayList(checked_artifact.SemanticInstantiationProcedureId).empty;
    errdefer ids.deinit(allocator);

    for (artifact.semantic_instantiation_procedures.procedures.items) |record| {
        switch (record.key) {
            .const_instance_callable_leaf => |leaf| {
                if (!checked_artifact.constInstantiationKeyEql(leaf.instance, instance)) continue;
                try ids.append(allocator, record.id);
            },
            else => {},
        }
    }

    return try ids.toOwnedSlice(allocator);
}

fn generatedProceduresForCallableBindingInstance(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    instance: checked_artifact.CallableBindingInstantiationKey,
) Allocator.Error![]const checked_artifact.SemanticInstantiationProcedureId {
    var ids = std.ArrayList(checked_artifact.SemanticInstantiationProcedureId).empty;
    errdefer ids.deinit(allocator);

    for (artifact.semantic_instantiation_procedures.procedures.items) |record| {
        switch (record.key) {
            .callable_binding_promoted_leaf => |leaf| {
                if (!checked_artifact.callableBindingInstantiationKeyEql(leaf.instance, instance)) continue;
                try ids.append(allocator, record.id);
            },
            else => {},
        }
    }

    return try ids.toOwnedSlice(allocator);
}

fn ensureConstInstanceRequest(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
    request: checked_artifact.ConstInstantiationRequest,
) anyerror!checked_artifact.ConstInstanceRef {
    const instance_ref = try artifact.const_instances.reserveRequest(allocator, &artifact.checked_types, request);
    switch (artifact.const_instances.stateForRef(instance_ref)) {
        .evaluated => return instance_ref,
        .evaluating => compileTimeFinalizationInvariant("constant instance dependency cycle reached checking finalization"),
        .reserved => {},
    }

    const source = constTemplateSourceForRef(artifact, dependency_views, request.key.const_ref);
    switch (source.template.state) {
        .reserved => compileTimeFinalizationInvariant("constant instance request reached unsealed const template"),
        .value_graph_template => |graph| {
            var dependencies = ConcreteDependencyCollector.init(allocator, artifact);
            defer dependencies.deinit();
            var cloner = ComptimeGraphCloner.init(
                allocator,
                artifact,
                source.values,
                source.plans,
                &dependencies,
            );
            defer cloner.deinit();

            const cloned = try cloner.clone(graph.schema, graph.value);
            artifact.const_instances.fill(instance_ref, .{
                .schema = cloned.schema,
                .value = cloned.value,
                .dependency_summary = try dependencies.appendSummary(),
                .reification_plan = null,
                .generated_procedures = try generatedProceduresForConstInstance(
                    allocator,
                    artifact,
                    request.key,
                ),
            });
        },
        .eval_template => {
            artifact.const_instances.markEvaluating(instance_ref);
            const dependency_summary = try dependencySummaryForCompileTimeRequest(
                allocator,
                artifact,
                lowering_imports,
                relation_artifacts,
                .{ .const_instance = request },
            );
            try ensureDependencySummaryConcreteDependencies(
                allocator,
                artifact,
                dependency_views,
                lowering_imports,
                relation_artifacts,
                runtime_env,
                artifact.comptime_dependencies.getSummary(dependency_summary),
            );

            var lowered_request = try lowerSingleCompileTimeRequest(
                allocator,
                artifact,
                lowering_imports,
                relation_artifacts,
                .{ .const_instance = request },
            );
            defer lowered_request.deinit();

            var interpreter = try Interpreter.init(
                allocator,
                &lowered_request.lowered.lir_result.store,
                &lowered_request.lowered.lir_result.layouts,
                runtime_env.get_ops(),
            );
            defer interpreter.deinit();

            const reification_plan = switch (lowered_request.payload) {
                .const_instance => |plan| plan,
                else => compileTimeFinalizationInvariant("const instance lowering did not publish a const graph payload"),
            };

            const result = try evalCompileTimeRoot(&interpreter, lowered_request.lir_root);
            const ret_layout = lowered_request.lowered.lir_result.store.getProcSpec(lowered_request.lir_root).ret_layout;
            defer interpreter.dropValue(result.value, ret_layout);

            var concrete_dependencies = ConcreteDependencyCollector.init(allocator, artifact);
            defer concrete_dependencies.deinit();

            var reifier = ComptimeReifier{
                .allocator = allocator,
                .artifact = artifact,
                .values = &artifact.comptime_values,
                .plans = &artifact.comptime_plans,
                .checked_types = &artifact.checked_types,
                .layouts = &lowered_request.lowered.lir_result.layouts,
                .lowered = &lowered_request.lowered,
                .callable_set_descriptors = lowered_request.lowered.callable_set_descriptors,
                .active_schemas = std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, checked_artifact.ComptimeSchemaId).init(allocator),
                .promotion_context = .{
                    .source_binding = sourceBindingForConstInstanceRequest(artifact, request.key.const_ref),
                    .base = .{ .const_instance = request.key },
                },
                .dependencies = &concrete_dependencies,
            };
            defer reifier.deinit();
            const reified = try reifier.reifyPlan(reification_plan, ret_layout, result.value);
            const concrete_dependency_summary = try appendConcreteDependencySummaryFromBase(
                allocator,
                artifact,
                dependency_summary,
                concrete_dependencies.concrete.items,
            );
            artifact.const_instances.fill(instance_ref, .{
                .schema = reified.schema,
                .value = reified.value,
                .dependency_summary = concrete_dependency_summary,
                .reification_plan = reification_plan,
                .generated_procedures = try generatedProceduresForConstInstance(
                    allocator,
                    artifact,
                    request.key,
                ),
            });
        },
    }
    return instance_ref;
}

fn sourceBindingForConstInstanceRequest(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    ref: checked_artifact.ConstRef,
) ?checked_artifact.CheckedPatternId {
    if (!std.meta.eql(ref.artifact.bytes, artifact.key.bytes)) return null;
    return switch (ref.owner) {
        .top_level_binding => |top_level| top_level.pattern,
        .promoted_capture => null,
    };
}

fn ensureRootConcreteDependencies(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
    request: checked_artifact.RootRequest,
) anyerror!void {
    const root = compileTimeRootForRequest(artifact, request);
    const summary = artifact.comptime_dependencies.summaryForRootRequest(root.dependency_summary_request);
    try ensureDependencySummaryConcreteDependencies(
        allocator,
        artifact,
        dependency_views,
        lowering_imports,
        relation_artifacts,
        runtime_env,
        summary,
    );
}

fn ensureDependencySummaryIdsConcreteDependencies(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
    summaries: []const ?checked_artifact.ComptimeDependencySummaryId,
) anyerror!void {
    for (summaries) |summary_id| {
        const id = summary_id orelse {
            compileTimeFinalizationInvariant("runtime root dependency summary was not published");
        };
        try ensureDependencySummaryConcreteDependencies(
            allocator,
            artifact,
            dependency_views,
            lowering_imports,
            relation_artifacts,
            runtime_env,
            artifact.comptime_dependencies.getSummary(id),
        );
    }
}

fn ensureDependencySummaryConcreteDependencies(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
    summary: checked_artifact.ComptimeDependencySummary,
) anyerror!void {
    for (summary.concrete_values) |value| {
        try ensureConcreteDependencyValue(
            allocator,
            artifact,
            dependency_views,
            lowering_imports,
            relation_artifacts,
            runtime_env,
            value,
        );
    }
}

fn ensureConcreteDependencyValue(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
    value: checked_artifact.ComptimeConcreteValueUse,
) anyerror!void {
    switch (value) {
        .const_instance => |key| {
            const requested_source_ty_payload = try checkedTypePayloadForConstInstanceDependency(allocator, artifact, dependency_views, key);
            _ = try ensureConstInstanceRequest(
                allocator,
                artifact,
                dependency_views,
                lowering_imports,
                relation_artifacts,
                runtime_env,
                .{
                    .key = key,
                    .requested_source_ty_payload = requested_source_ty_payload,
                },
            );
        },
        .callable_binding_instance => |key| {
            const requested_source_fn_ty_payload = artifact.checked_types.rootForKey(key.requested_source_fn_ty) orelse {
                compileTimeFinalizationInvariant("concrete dependency callable instance requested function type has no checked payload");
            };
            _ = try ensureCallableBindingInstanceRequest(
                allocator,
                artifact,
                dependency_views,
                lowering_imports,
                relation_artifacts,
                runtime_env,
                .{
                    .key = key,
                    .requested_source_fn_ty_payload = requested_source_fn_ty_payload,
                },
            );
        },
        .procedure_callable,
        .procedure_callable_with_payloads,
        => {},
    }
}

fn checkedTypePayloadForConstInstanceDependency(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    key: checked_artifact.ConstInstantiationKey,
) Allocator.Error!checked_artifact.CheckedTypeId {
    if (artifact.checked_types.rootForKey(key.requested_source_ty)) |root| return root;

    var projector = checked_artifact.CheckedTypeProjector.init(allocator, artifact, import_views);
    defer projector.deinit();

    if (!std.meta.eql(key.const_ref.artifact.bytes, artifact.key.bytes)) {
        for (import_views) |imported| {
            if (!std.meta.eql(imported.key.bytes, key.const_ref.artifact.bytes)) continue;
            if (try projector.projectImportedCheckedTypeForKey(imported, key.requested_source_ty)) |projected| {
                return projected;
            }
            break;
        }
    }

    for (import_views) |imported| {
        if (try projector.projectImportedCheckedTypeForKey(imported, key.requested_source_ty)) |projected| {
            return projected;
        }
    }

    compileTimeFinalizationInvariant("concrete dependency const instance requested type has no checked payload");
}

fn dependencySummaryForCompileTimeRequest(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    request: checked_artifact.CompileTimeEvaluationRequest,
) anyerror!checked_artifact.ComptimeDependencySummaryId {
    const single_request = [_]checked_artifact.CompileTimeEvaluationRequest{request};
    var summaries = try lir.CheckedPipeline.summarizeCompileTimeDependencies(
        allocator,
        .{
            .root = checked_artifact.loweringViewWithRelations(artifact, relation_artifacts),
            .imports = lowering_imports,
        },
        .{
            .compile_time_requests = &single_request,
            .purpose = .compile_time,
            .compile_time_plan_sink = &artifact.comptime_plans,
            .compile_time_artifact_sink = artifact,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    defer summaries.deinit();

    if (summaries.dependency_summaries.len != 1) {
        compileTimeFinalizationInvariant("single compile-time dependency summary request did not publish exactly one summary");
    }
    return summaries.dependency_summaries[0] orelse {
        compileTimeFinalizationInvariant("single compile-time dependency summary request published no summary id");
    };
}

const DirectCallableBindingInfo = struct {
    direct: checked_artifact.DirectProcedureBinding,
};

fn fillDirectCallableBindingInstance(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    request: checked_artifact.CallableBindingInstantiationRequest,
) Allocator.Error!bool {
    const direct = directCallableBindingInfo(artifact, dependency_views, request.key.binding) orelse return false;
    const proc_value = canonical.ProcedureCallableRef{
        .template = direct.direct.template,
        .source_fn_ty = request.key.requested_source_fn_ty,
    };
    const instance_ref = try artifact.callable_binding_instances.reserveRequest(allocator, &artifact.checked_types, request);
    switch (artifact.callable_binding_instances.stateForRef(instance_ref)) {
        .evaluated => return true,
        .evaluating => compileTimeFinalizationInvariant("direct callable binding instance was requested recursively while evaluating"),
        .reserved => {},
    }
    const dependency_summary = try appendDirectCallableBindingDependencySummary(
        allocator,
        artifact,
        request.key.binding,
        proc_value,
    );
    artifact.callable_binding_instances.fill(instance_ref, .{
        .key = request.key,
        .dependency_summary = dependency_summary,
        .proc_value = proc_value,
        .body = .{ .direct = .{
            .binding = request.key.binding,
            .template = direct.direct.template,
        } },
    });
    return true;
}

fn ensureCallableBindingInstanceRequest(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    lowering_imports: []const checked_artifact.ImportedModuleView,
    relation_artifacts: []const checked_artifact.ImportedModuleView,
    runtime_env: *RuntimeHostEnv,
    request: checked_artifact.CallableBindingInstantiationRequest,
) anyerror!checked_artifact.CallableBindingInstanceRef {
    const instance_ref = try artifact.callable_binding_instances.reserveRequest(allocator, &artifact.checked_types, request);
    switch (artifact.callable_binding_instances.stateForRef(instance_ref)) {
        .evaluated => return instance_ref,
        .evaluating => compileTimeFinalizationInvariant("callable binding instance dependency cycle reached checking finalization"),
        .reserved => {},
    }

    if (try fillDirectCallableBindingInstance(allocator, artifact, dependency_views, request)) {
        return instance_ref;
    }

    artifact.callable_binding_instances.markEvaluating(instance_ref);
    const dependency_summary = try dependencySummaryForCompileTimeRequest(
        allocator,
        artifact,
        lowering_imports,
        relation_artifacts,
        .{ .callable_binding_instance = request },
    );
    try ensureDependencySummaryConcreteDependencies(
        allocator,
        artifact,
        dependency_views,
        lowering_imports,
        relation_artifacts,
        runtime_env,
        artifact.comptime_dependencies.getSummary(dependency_summary),
    );

    var lowered_request = try lowerSingleCompileTimeRequest(
        allocator,
        artifact,
        lowering_imports,
        relation_artifacts,
        .{ .callable_binding_instance = request },
    );
    defer lowered_request.deinit();

    var interpreter = try Interpreter.init(
        allocator,
        &lowered_request.lowered.lir_result.store,
        &lowered_request.lowered.lir_result.layouts,
        runtime_env.get_ops(),
    );
    defer interpreter.deinit();

    const result_plan = switch (lowered_request.payload) {
        .callable_binding_instance => |plan| plan,
        else => compileTimeFinalizationInvariant("callable binding instance lowering did not publish a callable-result payload"),
    };

    const result = try evalCompileTimeRoot(&interpreter, lowered_request.lir_root);
    const ret_layout = lowered_request.lowered.lir_result.store.getProcSpec(lowered_request.lir_root).ret_layout;
    defer interpreter.dropValue(result.value, ret_layout);

    const promotion_context = PromotedCallablePublicationContext{
        .source_binding = sourceBindingForCallableBindingRequest(artifact, request.key.binding),
        .base = .{ .callable_binding_instance = request.key },
    };
    const callable = switch (artifact.comptime_plans.callableResult(result_plan)) {
        .finite => blk: {
            const selected_callable = selectFiniteCallableResult(
                &artifact.comptime_plans,
                lowered_request.lowered.callable_set_descriptors,
                &lowered_request.lowered.lir_result.layouts,
                result_plan,
                ret_layout,
                result.value,
            );
            break :blk try publishCallableResult(
                allocator,
                artifact,
                &lowered_request.lowered,
                promotion_context,
                request.requested_source_fn_ty_payload,
                result_plan,
                selected_callable,
            );
        },
        .erased => |erased| try publishErasedCallableResult(
            allocator,
            artifact,
            &lowered_request.lowered,
            promotion_context,
            request.requested_source_fn_ty_payload,
            ret_layout,
            result.value,
            result_plan,
            erased,
        ),
    };
    if (!std.meta.eql(callable.proc_value.source_fn_ty.bytes, request.key.requested_source_fn_ty.bytes)) {
        compileTimeFinalizationInvariant("callable binding instance result source type differed from requested source function type");
    }

    const generated_procedures = try generatedProceduresForCallableBindingInstance(
        allocator,
        artifact,
        request.key,
    );
    artifact.callable_binding_instances.fill(instance_ref, .{
        .key = request.key,
        .dependency_summary = dependency_summary,
        .proc_value = callable.proc_value,
        .body = .{ .evaluated = .{
            .executable_root = .{ .concrete_request = request.key },
            .result_plan = result_plan,
            .promotion_plan = callable.promotion_plan,
            .promotion_output = callable.output,
        } },
        .generated_procedures = generated_procedures,
    });
    return instance_ref;
}

fn sourceBindingForCallableBindingRequest(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    binding: checked_artifact.ProcedureBindingRef,
) ?checked_artifact.CheckedPatternId {
    return switch (binding) {
        .top_level => |binding_ref| blk: {
            for (artifact.top_level_values.entries) |entry| {
                const candidate = switch (entry.value) {
                    .procedure_binding => |candidate| candidate,
                    .const_ref => continue,
                };
                if (candidate == binding_ref) break :blk entry.pattern;
            }
            break :blk null;
        },
        .platform_required => |required| if (std.meta.eql(required.artifact.bytes, artifact.key.bytes))
            required.app_value.pattern
        else
            null,
        .imported,
        .hosted,
        .promoted,
        => null,
    };
}

fn appendDirectCallableBindingDependencySummary(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    binding: checked_artifact.ProcedureBindingRef,
    proc_value: canonical.ProcedureCallableRef,
) Allocator.Error!checked_artifact.ComptimeDependencySummaryId {
    const availability = [_]checked_artifact.ComptimeAvailabilityUse{.{ .procedure_binding = binding }};
    const concrete = [_]checked_artifact.ComptimeConcreteValueUse{.{ .procedure_callable = proc_value }};
    return try artifact.comptime_dependencies.appendSummary(allocator, .{
        .availability_values = availability[0..],
        .concrete_values = concrete[0..],
    });
}

fn directCallableBindingInfo(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    binding: checked_artifact.ProcedureBindingRef,
) ?DirectCallableBindingInfo {
    return switch (binding) {
        .top_level => |binding_ref| switch (artifact.top_level_procedure_bindings.get(binding_ref).body) {
            .direct_template => |direct| .{ .direct = direct },
            .callable_eval_template => null,
        },
        .imported => |imported| blk: {
            const view = importedProcedureBindingView(import_views, imported) orelse {
                compileTimeFinalizationInvariant("direct callable binding request referenced missing imported binding");
            };
            break :blk switch (view.body) {
                .direct_template => |direct| .{ .direct = direct },
                .callable_eval_template => null,
            };
        },
        .hosted => |hosted| .{ .direct = .{
            .proc_value = hosted.proc,
            .template = .{ .checked = hosted.template },
        } },
        .platform_required => |required| blk: {
            const owner_bindings = topLevelProcedureBindingsForArtifact(
                artifact,
                import_views,
                required.artifact,
            ) orelse {
                compileTimeFinalizationInvariant("platform-required direct callable binding owner artifact was not available");
            };
            break :blk switch (owner_bindings.get(required.procedure_binding).body) {
                .direct_template => |direct| .{ .direct = direct },
                .callable_eval_template => null,
            };
        },
        .promoted => |promoted| blk: {
            const promoted_record = promotedProcedureForRef(artifact, import_views, promoted) orelse {
                compileTimeFinalizationInvariant("direct callable binding request referenced missing promoted procedure");
            };
            break :blk .{ .direct = .{
                .proc_value = promoted_record.proc,
                .template = .{ .synthetic = .{ .template = promoted_record.template } },
            } };
        },
    };
}

fn topLevelProcedureBindingsForArtifact(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    owner: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.TopLevelProcedureBindingTable {
    if (std.meta.eql(artifact.key.bytes, owner.bytes)) return &artifact.top_level_procedure_bindings;
    for (import_views) |view| {
        if (std.meta.eql(view.key.bytes, owner.bytes)) return view.top_level_procedure_bindings;
    }
    return null;
}

fn importedProcedureBindingView(
    import_views: []const checked_artifact.ImportedModuleView,
    binding: checked_artifact.ImportedProcedureBindingRef,
) ?checked_artifact.ImportedProcedureBindingView {
    for (import_views) |view| {
        if (!std.meta.eql(view.key.bytes, binding.artifact.bytes)) continue;
        for (view.exported_procedure_bindings.bindings) |candidate| {
            if (importedProcedureBindingRefEql(candidate.binding, binding)) return candidate;
        }
    }
    return null;
}

fn importedProcedureBindingRefEql(
    a: checked_artifact.ImportedProcedureBindingRef,
    b: checked_artifact.ImportedProcedureBindingRef,
) bool {
    return std.meta.eql(a.artifact.bytes, b.artifact.bytes) and
        a.def == b.def and
        a.pattern == b.pattern;
}

fn promotedProcedureForRef(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    promoted: checked_artifact.PromotedProcedureRef,
) ?checked_artifact.PromotedProcedure {
    if (artifact.module_identity.module_idx == promoted.module_idx) {
        if (artifact.promoted_procedures.get(promoted)) |procedure| return procedure;
    }
    for (import_views) |view| {
        if (view.module_identity.module_idx != promoted.module_idx) continue;
        if (view.promoted_procedures.get(promoted)) |procedure| return procedure;
    }
    return null;
}

const CompileTimeRootDependencyGraph = struct {
    nodes: []CompileTimeRootNode = &.{},
    edges: []CompileTimeRootEdge = &.{},

    fn deinit(self: *CompileTimeRootDependencyGraph, allocator: Allocator) void {
        allocator.free(self.edges);
        allocator.free(self.nodes);
        self.* = .{};
    }
};

const CompileTimeRootNode = union(enum) {
    compile_time_constant_root: checked_artifact.ComptimeRootId,
    callable_binding_root: checked_artifact.ComptimeRootId,
    expect_root: checked_artifact.ComptimeRootId,
};

const CompileTimeRootEdge = struct {
    from: checked_artifact.ComptimeRootId,
    to: CompileTimeRootPrerequisite,
    reason: CompileTimeRootDependencyReason,
};

const CompileTimeRootPrerequisite = union(enum) {
    local_root: checked_artifact.ComptimeRootId,
    imported_value: checked_artifact.TopLevelValueRef,
};

const CompileTimeRootDependencyReason = union(enum) {
    availability_value: checked_artifact.ComptimeAvailabilityUse,
};

fn constGraphContainsCallableSlots(
    allocator: Allocator,
    plans: *const checked_artifact.CompileTimePlanStore,
    root: checked_artifact.ConstGraphReificationPlanId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, void).init(allocator);
    defer active.deinit();
    return try constGraphContainsCallableSlotsInner(plans, &active, root);
}

fn constGraphContainsCallableSlotsInner(
    plans: *const checked_artifact.CompileTimePlanStore,
    active: *std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, void),
    root: checked_artifact.ConstGraphReificationPlanId,
) Allocator.Error!bool {
    if (active.contains(root)) return false;
    try active.put(root, {});
    defer _ = active.remove(root);

    return switch (plans.constGraph(root)) {
        .pending => compileTimeFinalizationInvariant("callable-slot scan reached pending const graph plan"),
        .callable_leaf => true,
        .callable_schema => false,
        .scalar,
        .string,
        => false,
        .list => |list| try constGraphContainsCallableSlotsInner(plans, active, list.elem),
        .box => |box| try constGraphContainsCallableSlotsInner(plans, active, box.payload),
        .tuple => |items| blk: {
            for (items) |item| {
                if (try constGraphContainsCallableSlotsInner(plans, active, item.value)) break :blk true;
            }
            break :blk false;
        },
        .record => |fields| blk: {
            for (fields) |field| {
                if (try constGraphContainsCallableSlotsInner(plans, active, field.value)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |variants| blk: {
            for (variants) |variant| {
                for (variant.payloads) |payload| {
                    if (try constGraphContainsCallableSlotsInner(plans, active, payload.value)) break :blk true;
                }
            }
            break :blk false;
        },
        .transparent_alias => |alias| try constGraphContainsCallableSlotsInner(plans, active, alias.backing),
        .nominal => |nominal| try constGraphContainsCallableSlotsInner(plans, active, nominal.backing),
        .recursive_ref => |ref| try constGraphContainsCallableSlotsInner(plans, active, ref),
    };
}

fn appendConcreteDependencySummaryForCallableRoot(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    root: checked_artifact.CompileTimeRoot,
    _: checked_artifact.CallableResultPlanId,
    published_proc: canonical.ProcedureCallableRef,
) Allocator.Error!checked_artifact.ComptimeDependencySummaryId {
    var collector = ConcreteDependencyCollector.init(allocator, artifact);
    defer collector.deinit();
    try collector.appendProcedureCallable(published_proc);
    return try appendConcreteDependencySummary(allocator, artifact, root, collector.concrete.items);
}

fn appendConcreteDependencySummary(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    root: checked_artifact.CompileTimeRoot,
    concrete_values: []const checked_artifact.ComptimeConcreteValueUse,
) Allocator.Error!checked_artifact.ComptimeDependencySummaryId {
    const root_summary = artifact.comptime_dependencies.summaryForRootRequest(root.dependency_summary_request);
    return try artifact.comptime_dependencies.appendSummary(allocator, .{
        .availability_values = root_summary.availability_values,
        .concrete_values = concrete_values,
    });
}

fn appendConcreteDependencySummaryFromBase(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    base_summary_id: checked_artifact.ComptimeDependencySummaryId,
    concrete_values: []const checked_artifact.ComptimeConcreteValueUse,
) Allocator.Error!checked_artifact.ComptimeDependencySummaryId {
    const base_summary = artifact.comptime_dependencies.getSummary(base_summary_id);
    return try artifact.comptime_dependencies.appendSummary(allocator, .{
        .availability_values = base_summary.availability_values,
        .concrete_values = concrete_values,
    });
}

const ConcreteDependencyCollector = struct {
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    concrete: std.ArrayList(checked_artifact.ComptimeConcreteValueUse),
    active_const_graphs: std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, void),
    active_capture_slots: std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, void),
    active_erased_nodes: std.AutoHashMap(checked_artifact.ErasedCaptureExecutableMaterializationNodeId, void),

    fn init(
        allocator: Allocator,
        artifact: *checked_artifact.CheckedModuleArtifact,
    ) ConcreteDependencyCollector {
        return .{
            .allocator = allocator,
            .artifact = artifact,
            .concrete = .empty,
            .active_const_graphs = std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, void).init(allocator),
            .active_capture_slots = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, void).init(allocator),
            .active_erased_nodes = std.AutoHashMap(checked_artifact.ErasedCaptureExecutableMaterializationNodeId, void).init(allocator),
        };
    }

    fn deinit(self: *ConcreteDependencyCollector) void {
        self.active_erased_nodes.deinit();
        self.active_capture_slots.deinit();
        self.active_const_graphs.deinit();
        self.concrete.deinit(self.allocator);
    }

    fn appendProcedureCallable(
        self: *ConcreteDependencyCollector,
        proc_value: canonical.ProcedureCallableRef,
    ) Allocator.Error!void {
        try self.concrete.append(self.allocator, .{ .procedure_callable = proc_value });
    }

    fn appendConstInstance(
        self: *ConcreteDependencyCollector,
        instance: checked_artifact.ConstInstanceRef,
    ) Allocator.Error!void {
        try self.concrete.append(self.allocator, .{ .const_instance = instance.key });
    }

    fn appendSummary(
        self: *ConcreteDependencyCollector,
    ) Allocator.Error!checked_artifact.ComptimeDependencySummaryId {
        return try self.artifact.comptime_dependencies.appendSummary(self.allocator, .{
            .availability_values = &.{},
            .concrete_values = self.concrete.items,
        });
    }

    fn collectConstGraph(
        self: *ConcreteDependencyCollector,
        root: checked_artifact.ConstGraphReificationPlanId,
    ) Allocator.Error!void {
        if (self.active_const_graphs.contains(root)) return;
        try self.active_const_graphs.put(root, {});
        defer _ = self.active_const_graphs.remove(root);

        switch (self.artifact.comptime_plans.constGraph(root)) {
            .pending => compileTimeFinalizationInvariant("concrete dependency collection reached pending const graph plan"),
            .scalar,
            .string,
            .callable_schema,
            => {},
            .list => |list| try self.collectConstGraph(list.elem),
            .box => |box| try self.collectConstGraph(box.payload),
            .tuple => |items| for (items) |item| try self.collectConstGraph(item.value),
            .record => |fields| for (fields) |field| try self.collectConstGraph(field.value),
            .tag_union => |variants| for (variants) |variant| {
                for (variant.payloads) |payload| try self.collectConstGraph(payload.value);
            },
            .transparent_alias => |alias| try self.collectConstGraph(alias.backing),
            .nominal => |nominal| try self.collectConstGraph(nominal.backing),
            .callable_leaf => |leaf| try self.collectCallableLeaf(leaf),
            .recursive_ref => |ref| try self.collectConstGraph(ref),
        }
    }

    fn collectCallableLeaf(
        self: *ConcreteDependencyCollector,
        leaf: checked_artifact.CallableLeafReificationPlan,
    ) Allocator.Error!void {
        switch (leaf) {
            .finite,
            .erased_boxed,
            => |result_plan| try self.collectCallableResult(result_plan),
            .already_resolved => |resolved| try self.appendCallableLeafInstance(resolved),
        }
    }

    fn appendCallableLeafInstance(
        self: *ConcreteDependencyCollector,
        leaf: checked_artifact.CallableLeafInstance,
    ) Allocator.Error!void {
        switch (leaf) {
            .finite => |finite| try self.appendProcedureCallable(finite.proc_value),
            .erased_boxed => |erased| {
                try self.collectErasedCodeRef(erased.code);
                try self.collectErasedCaptureExecutableMaterialization(erased.capture);
            },
        }
    }

    fn collectCallableResult(
        self: *ConcreteDependencyCollector,
        result_plan_id: checked_artifact.CallableResultPlanId,
    ) Allocator.Error!void {
        switch (self.artifact.comptime_plans.callableResult(result_plan_id)) {
            .finite => |finite| {
                const descriptor = persistedCallableSetDescriptor(self.artifact.callable_set_descriptors.descriptors, finite.callable_set_key) orelse {
                    compileTimeFinalizationInvariant("concrete dependency collection reached finite callable result without descriptor");
                };
                for (finite.members) |member_plan| {
                    const member = persistedCallableSetMember(descriptor, member_plan.member) orelse {
                        compileTimeFinalizationInvariant("concrete dependency collection reached missing callable-set member");
                    };
                    if (!canonical.procedureCallableRefEql(member.proc_value, member_plan.member_proc)) {
                        compileTimeFinalizationInvariant("concrete dependency collection finite member plan disagreed with descriptor member");
                    }
                    try self.concrete.append(self.allocator, .{ .procedure_callable_with_payloads = .{
                        .proc_value = member_plan.member_proc,
                        .source_fn_ty_payload = member_plan.member_proc_source_fn_ty_payload,
                        .lifted_owner_source_fn_ty_payload = member_plan.member_lifted_owner_source_fn_ty_payload,
                    } });
                    for (member_plan.capture_slots) |capture| try self.collectCaptureSlot(capture);
                }
            },
            .erased => |erased| {
                switch (erased.code_plan) {
                    .materialized_by_lowering => |code| try self.collectErasedCodeRef(code),
                    .read_from_interpreted_erased_value => {},
                }
                try self.collectErasedCaptureReification(erased.capture);
            },
        }
    }

    fn collectCaptureSlot(
        self: *ConcreteDependencyCollector,
        slot_id: checked_artifact.CaptureSlotReificationPlanId,
    ) Allocator.Error!void {
        if (self.active_capture_slots.contains(slot_id)) return;
        try self.active_capture_slots.put(slot_id, {});
        defer _ = self.active_capture_slots.remove(slot_id);

        switch (self.artifact.comptime_plans.captureSlot(slot_id)) {
            .pending => compileTimeFinalizationInvariant("concrete dependency collection reached pending capture slot plan"),
            .serializable_leaf => |leaf| try self.collectConstGraph(leaf.reification_plan),
            .callable_leaf => |result_plan| try self.collectCallableResult(result_plan),
            .callable_schema => {},
            .record => |fields| for (fields) |field| try self.collectCaptureSlot(field.value),
            .tuple => |items| for (items) |item| try self.collectCaptureSlot(item.value),
            .tag_union => |variants| for (variants) |variant| {
                for (variant.payloads) |payload| try self.collectCaptureSlot(payload.value);
            },
            .list => |list| try self.collectCaptureSlot(list.elem),
            .box => |payload| try self.collectCaptureSlot(payload),
            .nominal => |nominal| try self.collectCaptureSlot(nominal.backing),
            .recursive_ref => |ref| try self.collectCaptureSlot(ref),
        }
    }

    fn collectErasedCaptureReification(
        self: *ConcreteDependencyCollector,
        capture: checked_artifact.ErasedCaptureReificationPlan,
    ) Allocator.Error!void {
        switch (capture) {
            .none,
            .zero_sized_typed,
            => {},
            .whole_hidden_capture_value => |ref| try self.collectCaptureSlot(ref.plan),
            .proc_capture_tuple => |refs| for (refs) |ref| try self.collectCaptureSlot(ref.plan),
            .finite_callable_set_value => |result_plan| try self.collectCallableResult(result_plan),
        }
    }

    fn collectErasedCodeRef(
        self: *ConcreteDependencyCollector,
        code: canonical.ErasedCallableCodeRef,
    ) Allocator.Error!void {
        switch (code) {
            .direct_proc_value => |direct| try self.appendProcedureCallable(direct.proc_value),
            .finite_set_adapter => |adapter| {
                const descriptor = persistedCallableSetDescriptor(self.artifact.callable_set_descriptors.descriptors, adapter.callable_set_key) orelse {
                    compileTimeFinalizationInvariant("concrete dependency collection reached erased finite adapter without descriptor");
                };
                for (descriptor.members) |member| try self.appendProcedureCallable(member.proc_value);
            },
        }
    }

    fn collectErasedCaptureExecutableMaterialization(
        self: *ConcreteDependencyCollector,
        capture: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
    ) Allocator.Error!void {
        switch (capture) {
            .none,
            .zero_sized_typed,
            => {},
            .node => |node_id| try self.collectErasedCaptureExecutableMaterializationNode(node_id),
        }
    }

    fn collectErasedCaptureExecutableMaterializationNode(
        self: *ConcreteDependencyCollector,
        node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
    ) Allocator.Error!void {
        if (self.active_erased_nodes.contains(node_id)) return;
        try self.active_erased_nodes.put(node_id, {});
        defer _ = self.active_erased_nodes.remove(node_id);

        switch (self.artifact.comptime_plans.erasedCaptureExecutableMaterializationNode(node_id)) {
            .pending => compileTimeFinalizationInvariant("concrete dependency collection reached pending erased capture node"),
            .const_instance => |instance| try self.appendConstInstance(instance),
            .pure_const => |pure| try self.appendConstInstance(pure.const_instance),
            .pure_value => {},
            .finite_callable_set => |finite| {
                const descriptor = persistedCallableSetDescriptor(self.artifact.callable_set_descriptors.descriptors, finite.callable_set_key) orelse {
                    compileTimeFinalizationInvariant("concrete dependency collection reached materialized finite callable set without descriptor");
                };
                const member = persistedCallableSetMember(descriptor, finite.selected_member) orelse {
                    compileTimeFinalizationInvariant("concrete dependency collection reached materialized finite callable set with missing member");
                };
                try self.appendProcedureCallable(member.proc_value);
                for (finite.captures) |capture| try self.collectErasedCaptureExecutableMaterialization(capture);
            },
            .erased_callable => |erased| {
                try self.collectErasedCodeRef(erased.code);
                try self.collectErasedCaptureExecutableMaterialization(erased.capture);
            },
            .record => |fields| for (fields) |field| try self.collectErasedCaptureExecutableMaterialization(field.value),
            .tuple => |items| for (items) |item| try self.collectErasedCaptureExecutableMaterialization(item),
            .tag_union => |tag| for (tag.payloads) |payload| try self.collectErasedCaptureExecutableMaterialization(payload.value),
            .list => |items| for (items) |item| try self.collectErasedCaptureExecutableMaterialization(item),
            .box => |payload| try self.collectErasedCaptureExecutableMaterialization(payload),
            .nominal => |nominal| try self.collectErasedCaptureExecutableMaterialization(nominal.backing),
            .recursive_ref => |ref| try self.collectErasedCaptureExecutableMaterializationNode(ref),
        }
    }
};

const ConstTemplateSource = struct {
    template: checked_artifact.ConstTemplate,
    checked_types: checked_artifact.CheckedTypeStoreView,
    values: *const checked_artifact.CompileTimeValueStore,
    plans: *const checked_artifact.CompileTimePlanStore,
    dependencies: checked_artifact.ComptimeDependencySummaryStoreView,
};

fn constTemplateSourceForRef(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    import_views: []const checked_artifact.ImportedModuleView,
    ref: checked_artifact.ConstRef,
) ConstTemplateSource {
    if (std.meta.eql(ref.artifact.bytes, artifact.key.bytes)) {
        return .{
            .template = artifact.const_templates.get(ref),
            .checked_types = artifact.checked_types.view(),
            .values = &artifact.comptime_values,
            .plans = &artifact.comptime_plans,
            .dependencies = artifact.comptime_dependencies.view(),
        };
    }
    for (import_views) |view| {
        if (!std.meta.eql(ref.artifact.bytes, view.key.bytes)) continue;
        return .{
            .template = view.const_templates.get(ref),
            .checked_types = view.checked_types,
            .values = view.comptime_values,
            .plans = view.comptime_plans,
            .dependencies = view.comptime_dependencies,
        };
    }
    compileTimeFinalizationInvariant("const instance request referenced unavailable const template artifact");
}

const ComptimeGraphCloner = struct {
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    source_values: *const checked_artifact.CompileTimeValueStore,
    source_plans: *const checked_artifact.CompileTimePlanStore,
    dependencies: *ConcreteDependencyCollector,
    schema_map: std.AutoHashMap(checked_artifact.ComptimeSchemaId, checked_artifact.ComptimeSchemaId),
    value_map: std.AutoHashMap(checked_artifact.ComptimeValueId, checked_artifact.ComptimeValueId),
    erased_node_map: std.AutoHashMap(
        checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
        checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
    ),

    fn init(
        allocator: Allocator,
        artifact: *checked_artifact.CheckedModuleArtifact,
        source_values: *const checked_artifact.CompileTimeValueStore,
        source_plans: *const checked_artifact.CompileTimePlanStore,
        dependencies: *ConcreteDependencyCollector,
    ) ComptimeGraphCloner {
        return .{
            .allocator = allocator,
            .artifact = artifact,
            .source_values = source_values,
            .source_plans = source_plans,
            .dependencies = dependencies,
            .schema_map = std.AutoHashMap(checked_artifact.ComptimeSchemaId, checked_artifact.ComptimeSchemaId).init(allocator),
            .value_map = std.AutoHashMap(checked_artifact.ComptimeValueId, checked_artifact.ComptimeValueId).init(allocator),
            .erased_node_map = std.AutoHashMap(
                checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
                checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
            ).init(allocator),
        };
    }

    fn deinit(self: *ComptimeGraphCloner) void {
        self.erased_node_map.deinit();
        self.value_map.deinit();
        self.schema_map.deinit();
    }

    fn clone(
        self: *ComptimeGraphCloner,
        schema: checked_artifact.ComptimeSchemaId,
        value: checked_artifact.ComptimeValueId,
    ) Allocator.Error!ReifiedValue {
        return .{
            .schema = try self.cloneSchema(schema),
            .value = try self.cloneValue(value),
        };
    }

    fn cloneSchema(
        self: *ComptimeGraphCloner,
        schema_id: checked_artifact.ComptimeSchemaId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        if (self.schema_map.get(schema_id)) |existing| return existing;
        const raw = @intFromEnum(schema_id);
        if (raw >= self.source_values.schemas.items.len) {
            compileTimeFinalizationInvariant("constant value graph clone reached missing schema");
        }
        const cloned = try self.cloneSchemaPayload(self.source_values.schemas.items[raw]);
        const out = try self.artifact.comptime_values.addSchema(cloned);
        try self.schema_map.put(schema_id, out);
        return out;
    }

    fn cloneSchemaPayload(
        self: *ComptimeGraphCloner,
        schema: checked_artifact.ComptimeSchema,
    ) Allocator.Error!checked_artifact.ComptimeSchema {
        return switch (schema) {
            .pending => compileTimeFinalizationInvariant("constant value graph clone reached pending schema"),
            .zst => .zst,
            .int => |int| .{ .int = int },
            .frac => |frac| .{ .frac = frac },
            .str => .str,
            .callable => |source_fn_ty| .{ .callable = source_fn_ty },
            .list => |child| .{ .list = try self.cloneSchema(child) },
            .box => |child| .{ .box = try self.cloneSchema(child) },
            .tuple => |items| .{ .tuple = try self.cloneSchemaSpan(items) },
            .record => |fields| .{ .record = try self.cloneRecordSchema(fields) },
            .tag_union => |variants| .{ .tag_union = try self.cloneTagUnionSchema(variants) },
            .alias => |alias| .{ .alias = .{
                .type_name = alias.type_name,
                .backing = try self.cloneSchema(alias.backing),
                .is_opaque = alias.is_opaque,
            } },
            .nominal => |nominal| .{ .nominal = .{
                .type_name = nominal.type_name,
                .backing = try self.cloneSchema(nominal.backing),
                .is_opaque = nominal.is_opaque,
            } },
        };
    }

    fn cloneSchemaSpan(
        self: *ComptimeGraphCloner,
        items: []const checked_artifact.ComptimeSchemaId,
    ) Allocator.Error![]checked_artifact.ComptimeSchemaId {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        for (items, 0..) |item, i| out[i] = try self.cloneSchema(item);
        return out;
    }

    fn cloneRecordSchema(
        self: *ComptimeGraphCloner,
        fields: []const checked_artifact.ComptimeFieldSchema,
    ) Allocator.Error![]checked_artifact.ComptimeFieldSchema {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = field.name,
                .schema = try self.cloneSchema(field.schema),
            };
        }
        return out;
    }

    fn cloneTagUnionSchema(
        self: *ComptimeGraphCloner,
        variants: []const checked_artifact.ComptimeVariantSchema,
    ) Allocator.Error![]checked_artifact.ComptimeVariantSchema {
        if (variants.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ComptimeVariantSchema, variants.len);
        for (variants, 0..) |variant, i| {
            out[i] = .{
                .name = variant.name,
                .payloads = try self.cloneSchemaSpan(variant.payloads),
            };
        }
        return out;
    }

    fn cloneValue(
        self: *ComptimeGraphCloner,
        value_id: checked_artifact.ComptimeValueId,
    ) Allocator.Error!checked_artifact.ComptimeValueId {
        if (self.value_map.get(value_id)) |existing| return existing;
        const raw = @intFromEnum(value_id);
        if (raw >= self.source_values.values.items.len) {
            compileTimeFinalizationInvariant("constant value graph clone reached missing value");
        }
        const cloned = try self.cloneValuePayload(self.source_values.values.items[raw]);
        const out = try self.artifact.comptime_values.addValue(cloned);
        try self.value_map.put(value_id, out);
        return out;
    }

    fn cloneValuePayload(
        self: *ComptimeGraphCloner,
        value: checked_artifact.ComptimeValue,
    ) Allocator.Error!checked_artifact.ComptimeValue {
        return switch (value) {
            .pending => compileTimeFinalizationInvariant("constant value graph clone reached pending value"),
            .zst => .zst,
            .int_bytes => |bytes| .{ .int_bytes = bytes },
            .f32 => |float| .{ .f32 = float },
            .f64 => |float| .{ .f64 = float },
            .dec => |bytes| .{ .dec = bytes },
            .str => |bytes| .{ .str = try self.allocator.dupe(u8, bytes) },
            .list => |items| .{ .list = try self.cloneValueSpan(items) },
            .tuple => |items| .{ .tuple = try self.cloneValueSpan(items) },
            .record => |items| .{ .record = try self.cloneValueSpan(items) },
            .box => |child| .{ .box = try self.cloneValue(child) },
            .alias => |child| .{ .alias = try self.cloneValue(child) },
            .nominal => |child| .{ .nominal = try self.cloneValue(child) },
            .tag_union => |tag| .{ .tag_union = .{
                .variant_index = tag.variant_index,
                .payloads = try self.cloneValueSpan(tag.payloads),
            } },
            .callable => |leaf| .{ .callable = try self.cloneCallableLeaf(leaf) },
        };
    }

    fn cloneValueSpan(
        self: *ComptimeGraphCloner,
        items: []const checked_artifact.ComptimeValueId,
    ) Allocator.Error![]checked_artifact.ComptimeValueId {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ComptimeValueId, items.len);
        for (items, 0..) |item, i| out[i] = try self.cloneValue(item);
        return out;
    }

    fn cloneCallableLeaf(
        self: *ComptimeGraphCloner,
        leaf: checked_artifact.CallableLeafInstance,
    ) Allocator.Error!checked_artifact.CallableLeafInstance {
        return switch (leaf) {
            .finite => |finite| blk: {
                try self.dependencies.appendProcedureCallable(finite.proc_value);
                break :blk .{ .finite = finite };
            },
            .erased_boxed => |erased| blk: {
                try self.dependencies.collectErasedCodeRef(erased.code);
                break :blk .{ .erased_boxed = .{
                    .source_fn_ty = erased.source_fn_ty,
                    .sig_key = erased.sig_key,
                    .provenance = try cloneBoxBoundarySpan(self.allocator, erased.provenance),
                    .code = erased.code,
                    .capture = try self.cloneErasedCapturePlan(erased.capture),
                } };
            },
        };
    }

    fn cloneErasedCapturePlan(
        self: *ComptimeGraphCloner,
        plan: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        return switch (plan) {
            .none => .none,
            .zero_sized_typed => |key| .{ .zero_sized_typed = key },
            .node => |node| .{ .node = try self.cloneErasedCaptureNode(node) },
        };
    }

    fn cloneErasedCaptureNode(
        self: *ComptimeGraphCloner,
        node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationNodeId {
        if (self.erased_node_map.get(node_id)) |existing| return existing;

        const out = try self.artifact.comptime_plans.reserveErasedCaptureExecutableMaterializationNode(self.allocator);
        try self.erased_node_map.put(node_id, out);
        errdefer _ = self.erased_node_map.remove(node_id);

        const cloned = try self.cloneErasedCaptureNodePayload(
            self.source_plans.erasedCaptureExecutableMaterializationNode(node_id),
        );
        self.artifact.comptime_plans.fillErasedCaptureExecutableMaterializationNode(out, cloned);
        return out;
    }

    fn cloneErasedCaptureNodePayload(
        self: *ComptimeGraphCloner,
        node: checked_artifact.ErasedCaptureExecutableMaterializationNode,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationNode {
        return switch (node) {
            .pending => compileTimeFinalizationInvariant("constant value graph clone reached pending erased capture node"),
            .const_instance => |instance| blk: {
                try self.dependencies.appendConstInstance(instance);
                break :blk .{ .const_instance = instance };
            },
            .pure_const => |pure| blk: {
                try self.dependencies.appendConstInstance(pure.const_instance);
                break :blk .{ .pure_const = pure };
            },
            .pure_value => |pure| blk: {
                const cloned = try self.clone(pure.schema, pure.value);
                break :blk .{ .pure_value = .{
                    .schema = cloned.schema,
                    .value = cloned.value,
                    .no_reachable_callable_slots = pure.no_reachable_callable_slots,
                } };
            },
            .finite_callable_set => |finite| blk: {
                const descriptor = persistedCallableSetDescriptor(self.artifact.callable_set_descriptors.descriptors, finite.callable_set_key) orelse {
                    compileTimeFinalizationInvariant("constant value graph clone reached finite callable set without descriptor");
                };
                const member = persistedCallableSetMember(descriptor, finite.selected_member) orelse {
                    compileTimeFinalizationInvariant("constant value graph clone reached finite callable set with missing member");
                };
                try self.dependencies.appendProcedureCallable(member.proc_value);
                break :blk .{ .finite_callable_set = .{
                    .source_fn_ty = finite.source_fn_ty,
                    .callable_set_key = finite.callable_set_key,
                    .selected_member = finite.selected_member,
                    .captures = try self.cloneErasedCapturePlanSpan(finite.captures),
                } };
            },
            .erased_callable => |erased| blk: {
                try self.dependencies.collectErasedCodeRef(erased.code);
                break :blk .{ .erased_callable = .{
                    .source_fn_ty = erased.source_fn_ty,
                    .sig_key = erased.sig_key,
                    .code = erased.code,
                    .capture = try self.cloneErasedCapturePlan(erased.capture),
                    .provenance = try cloneBoxBoundarySpan(self.allocator, erased.provenance),
                } };
            },
            .record => |fields| .{ .record = try self.cloneErasedCaptureRecord(fields) },
            .tuple => |items| .{ .tuple = try self.cloneErasedCapturePlanSpan(items) },
            .tag_union => |tag| .{ .tag_union = .{
                .tag = tag.tag,
                .payloads = try self.cloneErasedCaptureTagPayloads(tag.payloads),
            } },
            .list => |items| .{ .list = try self.cloneErasedCapturePlanSpan(items) },
            .box => |payload| .{ .box = try self.cloneErasedCapturePlan(payload) },
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .backing = try self.cloneErasedCapturePlan(nominal.backing),
            } },
            .recursive_ref => |ref| .{ .recursive_ref = try self.cloneErasedCaptureNode(ref) },
        };
    }

    fn cloneErasedCapturePlanSpan(
        self: *ComptimeGraphCloner,
        items: []const checked_artifact.ErasedCaptureExecutableMaterializationPlan,
    ) Allocator.Error![]const checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationPlan, items.len);
        for (items, 0..) |item, i| out[i] = try self.cloneErasedCapturePlan(item);
        return out;
    }

    fn cloneErasedCaptureRecord(
        self: *ComptimeGraphCloner,
        fields: []const checked_artifact.ErasedCaptureExecutableMaterializationRecordField,
    ) Allocator.Error![]const checked_artifact.ErasedCaptureExecutableMaterializationRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationRecordField, fields.len);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .field = field.field,
                .value = try self.cloneErasedCapturePlan(field.value),
            };
        }
        return out;
    }

    fn cloneErasedCaptureTagPayloads(
        self: *ComptimeGraphCloner,
        payloads: []const checked_artifact.ErasedCaptureExecutableMaterializationTagPayload,
    ) Allocator.Error![]const checked_artifact.ErasedCaptureExecutableMaterializationTagPayload {
        if (payloads.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationTagPayload, payloads.len);
        for (payloads, 0..) |payload, i| {
            out[i] = .{
                .index = payload.index,
                .value = try self.cloneErasedCapturePlan(payload.value),
            };
        }
        return out;
    }
};

fn selectFiniteCallableResult(
    plans: *const checked_artifact.CompileTimePlanStore,
    descriptors: []const repr.CanonicalCallableSetDescriptor,
    layouts: *const layout_mod.Store,
    result_plan_id: checked_artifact.CallableResultPlanId,
    layout_idx: layout_mod.Idx,
    value: Value,
) SelectedFiniteCallableResult {
    const plan = plans.callableResult(result_plan_id);
    const finite = switch (plan) {
        .finite => |finite| finite,
        .erased => compileTimeFinalizationInvariant("finite callable selection received an erased callable result plan"),
    };
    if (finite.members.len == 0) {
        compileTimeFinalizationInvariant("finite compile-time callable result plan had no members");
    }

    const result_layout = layouts.getLayout(layout_idx);
    const selected = selectFiniteCallableSetMember(
        layouts,
        layout_idx,
        result_layout,
        value,
        finite.members,
    );
    const planned_member = callableResultMember(finite.members, selected.member) orelse {
        compileTimeFinalizationInvariant("compile-time callable result selected a member outside the result plan");
    };

    const descriptor = runtimeCallableSetDescriptor(descriptors, finite.callable_set_key) orelse {
        compileTimeFinalizationInvariant("compile-time callable result descriptor was not preserved");
    };
    const member = runtimeCallableSetMember(descriptor, planned_member.member) orelse {
        compileTimeFinalizationInvariant("compile-time callable result selected missing descriptor member");
    };
    if (member.capture_slots.len != planned_member.capture_slots.len) {
        compileTimeFinalizationInvariant("compile-time callable result member capture arity differs from descriptor");
    }
    return .{
        .result_plan_id = result_plan_id,
        .result_plan = finite,
        .planned_member = planned_member,
        .descriptor_member = member,
        .payload_layout = selected.payload_layout,
        .payload_value = value,
    };
}

fn publishCallableResult(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    context: PromotedCallablePublicationContext,
    checked_fn_root: checked_artifact.CheckedTypeId,
    result_plan_id: checked_artifact.CallableResultPlanId,
    selected: SelectedFiniteCallableResult,
) Allocator.Error!PublishedCallableResult {
    if (selectedFiniteCallableRequiresPromotion(selected)) {
        return try promoteFiniteCallableResult(allocator, artifact, lowered, context, checked_fn_root, result_plan_id, selected);
    }
    if (selected.descriptor_member.capture_slots.len != 0) {
        compileTimeFinalizationInvariant("descriptor member for no-capture callable result had captures");
    }
    const proc_value = closedFiniteCallableLeafFromSelectedCallableResult(selected);
    return .{
        .proc_value = proc_value,
        .source_fn_ty_payload = selected.planned_member.member_proc_source_fn_ty_payload,
        .output = .{ .existing_procedure = proc_value },
        .promotion_plan = null,
        .generated_procedure = null,
    };
}

fn selectedFiniteCallableRequiresPromotion(
    selected: SelectedFiniteCallableResult,
) bool {
    if (selected.planned_member.capture_slots.len != 0) return true;
    if (selected.descriptor_member.capture_slots.len != 0) return true;
    if (selected.descriptor_member.published_proc_value == null) return true;
    if (!repr.canonicalTypeKeyEql(selected.descriptor_member.published_proc_value.?.source_fn_ty, selected.result_plan.source_fn_ty)) return true;
    if (selected.descriptor_member.published_source_proc == null) {
        compileTimeFinalizationInvariant("finite callable descriptor published procedure identity was not paired with published source procedure");
    }
    return false;
}

fn closedFiniteCallableLeafFromSelectedCallableResult(
    selected: SelectedFiniteCallableResult,
) canonical.ProcedureCallableRef {
    if (selected.planned_member.capture_slots.len != 0) {
        compileTimeFinalizationInvariant("captured finite callable value cannot collapse to a closed callable leaf");
    }
    if (selected.descriptor_member.capture_slots.len != 0) {
        compileTimeFinalizationInvariant("finite callable descriptor member unexpectedly had captures");
    }
    return selected.descriptor_member.published_proc_value orelse
        compileTimeFinalizationInvariant("finite callable result tried to persist a runtime-only procedure identity");
}

fn promoteFiniteCallableResult(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    context: PromotedCallablePublicationContext,
    checked_fn_root: checked_artifact.CheckedTypeId,
    result_plan_id: checked_artifact.CallableResultPlanId,
    selected: SelectedFiniteCallableResult,
) Allocator.Error!PublishedCallableResult {
    const checked_fn_scheme = try artifact.checked_types.ensureSchemeForRoot(allocator, checked_fn_root);
    const reserved = try artifact.reservePromotedCallableWrapper(
        allocator,
        context.source_binding,
        checked_fn_root,
        checked_fn_scheme,
        promotedProcedureProvenance(context, result_plan_id),
    );

    const params = try promotedWrapperParamsForFnRoot(allocator, artifact, checked_fn_root);
    const call_args = try promotedWrapperCallArgs(allocator, params.len);
    const captures = try allocator.alloc(checked_artifact.PrivateCaptureRef, selected.planned_member.capture_slots.len);
    const member_target = try cloneCallableResultMemberTargetPlan(allocator, selected.planned_member.target);
    var member_target_owned = true;
    errdefer if (member_target_owned) deinitCallableResultMemberTargetPlan(allocator, member_target);

    var capture_builder = PrivateCaptureBuilder{
        .allocator = allocator,
        .artifact = artifact,
        .lowered = lowered,
        .layouts = &lowered.lir_result.layouts,
        .callable_set_descriptors = lowered.callable_set_descriptors,
        .owner = reserved.promoted_ref,
        .promotion_context = .{
            .source_binding = context.source_binding,
            .base = .{ .private_capture = privateCaptureOwner(reserved) },
        },
        .active = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.PrivateCaptureNodeId).init(allocator),
        .erased_active = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.ErasedCaptureExecutableMaterializationNodeId).init(allocator),
    };
    defer capture_builder.deinit();

    if (selected.descriptor_member.capture_slots.len != selected.planned_member.capture_slots.len) {
        compileTimeFinalizationInvariant("promoted callable selected member capture schema disagrees with result plan");
    }
    const member_proc = try artifactOwnedCallableForSelectedMember(artifact, selected);
    const member_capture_slots = if (selected.descriptor_member.capture_slots.len == 0)
        &.{}
    else
        try allocator.dupe(canonical.CallableSetCaptureSlot, selected.descriptor_member.capture_slots);
    var member_capture_slots_owned = true;
    errdefer if (member_capture_slots_owned and member_capture_slots.len != 0) allocator.free(member_capture_slots);
    for (selected.planned_member.capture_slots, selected.descriptor_member.capture_slots, 0..) |slot_plan, slot, i| {
        if (slot.slot != @as(u32, @intCast(i))) {
            compileTimeFinalizationInvariant("promoted callable selected member capture slots are not canonical");
        }
        captures[i] = try capture_builder.captureRef(
            slot.source_ty,
            slot_plan,
            captureSlotValue(&lowered.lir_result.layouts, selected, @intCast(i)),
            @intCast(i),
        );
    }

    artifact.fillPromotedCallableWrapperBody(reserved, .{ .finite = .{
        .source_fn_ty = selected.result_plan.source_fn_ty,
        .callable_set_key = selected.result_plan.callable_set_key,
        .member = selected.planned_member.member,
        .member_proc = member_proc,
        .member_proc_source_fn_ty_payload = selected.planned_member.member_proc_source_fn_ty_payload,
        .member_lifted_owner_source_fn_ty_payload = selected.planned_member.member_lifted_owner_source_fn_ty_payload,
        .member_target = member_target,
        .member_target_promoted_wrapper = finitePromotedWrapperMemberTargetProvenance(context),
        .member_capture_shape = selected.descriptor_member.capture_shape_key,
        .member_capture_slots = member_capture_slots,
        .captures = captures,
        .params = params,
        .call_args = call_args,
    } });
    member_target_owned = false;
    member_capture_slots_owned = false;
    try artifact.publishPromotedCallableWrapper(allocator, reserved);
    const generated_procedure = try publishSemanticInstantiationProcedureForPromoted(
        allocator,
        artifact,
        reserved,
        selected.result_plan.source_fn_ty,
    );

    const promotion_plan = try artifact.comptime_plans.appendCallablePromotion(allocator, .{ .finite = .{
        .result_plan = result_plan_id,
        .selected_member = selected.planned_member.member,
        .promoted_proc = reserved.promoted_ref,
    } });
    const proc_value = canonical.ProcedureCallableRef{
        .template = .{ .synthetic = .{ .template = reserved.template } },
        .source_fn_ty = selected.result_plan.source_fn_ty,
    };
    return .{
        .proc_value = proc_value,
        .source_fn_ty_payload = checked_fn_root,
        .output = .{ .promoted_procedure = reserved.promoted_ref },
        .promotion_plan = promotion_plan,
        .generated_procedure = generated_procedure,
    };
}

fn artifactOwnedCallableForSelectedMember(
    artifact: *checked_artifact.CheckedModuleArtifact,
    selected: SelectedFiniteCallableResult,
) Allocator.Error!canonical.ProcedureCallableRef {
    if (selected.descriptor_member.published_proc_value) |published| {
        if (std.meta.eql(published.source_fn_ty.bytes, selected.result_plan.source_fn_ty.bytes)) return published;
    }

    return switch (selected.planned_member.member_proc.template) {
        .lifted => |lifted| if (!std.meta.eql(lifted.owner_mono_specialization.template.artifact.bytes, artifact.key.bytes))
            selected.planned_member.member_proc
        else
            .{
                .template = .{ .lifted = .{
                    .owner_mono_specialization = .{
                        .template = artifactOwnedOwnerTemplateForLiftedMember(artifact, lifted.owner_mono_specialization.template),
                        .requested_mono_fn_ty = lifted.owner_mono_specialization.requested_mono_fn_ty,
                    },
                    .site = lifted.site,
                } },
                .source_fn_ty = selected.result_plan.source_fn_ty,
            },
        .checked,
        .synthetic,
        => compileTimeFinalizationInvariant("promoted finite callable selected non-lifted member had no artifact-owned procedure"),
    };
}

fn artifactOwnedOwnerTemplateForLiftedMember(
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered_template: canonical.ProcedureTemplateRef,
) canonical.ProcedureTemplateRef {
    if (!std.meta.eql(lowered_template.artifact.bytes, artifact.key.bytes)) {
        compileTimeFinalizationInvariant("promoted finite callable selected lifted member owner artifact was unavailable");
    }
    const raw_template: usize = @intFromEnum(lowered_template.template);
    if (raw_template >= artifact.checked_procedure_templates.templates.len) {
        compileTimeFinalizationInvariant("promoted finite callable selected lifted member owner template id was outside checked artifact");
    }
    const record = artifact.checked_procedure_templates.templates[raw_template];
    if (record.template_id != lowered_template.template) {
        compileTimeFinalizationInvariant("promoted finite callable selected lifted member owner template id disagreed with checked artifact table");
    }
    return .{
        .artifact = lowered_template.artifact,
        .proc_base = record.proc_base,
        .template = lowered_template.template,
    };
}

fn finitePromotedWrapperMemberTargetProvenance(
    context: PromotedCallablePublicationContext,
) ?canonical.MirProcedureRef {
    return switch (context.base) {
        .private_capture => |owner| privateCaptureOwnerMirProcedureRef(owner),
        .local_callable_root,
        .local_const_root,
        .callable_binding_instance,
        .const_instance,
        => null,
    };
}

fn publishErasedCallableResult(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    context: PromotedCallablePublicationContext,
    checked_fn_root: checked_artifact.CheckedTypeId,
    ret_layout: layout_mod.Idx,
    ret_value: Value,
    result_plan: checked_artifact.CallableResultPlanId,
    erased: checked_artifact.ErasedCallableResultPlan,
) Allocator.Error!PublishedCallableResult {
    const checked_fn_scheme = try artifact.checked_types.ensureSchemeForRoot(allocator, checked_fn_root);
    const reserved = try artifact.reservePromotedCallableWrapper(
        allocator,
        context.source_binding,
        checked_fn_root,
        checked_fn_scheme,
        promotedProcedureProvenance(context, result_plan),
    );

    const params = try promotedWrapperParamsForFnRoot(allocator, artifact, checked_fn_root);
    var capture_builder = PrivateCaptureBuilder{
        .allocator = allocator,
        .artifact = artifact,
        .lowered = lowered,
        .layouts = &lowered.lir_result.layouts,
        .callable_set_descriptors = lowered.callable_set_descriptors,
        .owner = reserved.promoted_ref,
        .promotion_context = .{
            .source_binding = context.source_binding,
            .base = .{ .private_capture = privateCaptureOwner(reserved) },
        },
        .active = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.PrivateCaptureNodeId).init(allocator),
        .erased_active = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.ErasedCaptureExecutableMaterializationNodeId).init(allocator),
    };
    defer capture_builder.deinit();
    const code = resolveErasedCallableResultCode(
        artifact,
        lowered,
        erased,
        ret_layout,
        ret_value,
    );
    const publication = try persistedErasedCallablePublication(
        allocator,
        artifact,
        lowered,
        &capture_builder,
        reserved,
        context,
        code,
        erased,
        ret_layout,
        ret_value,
    );
    var publication_owned = true;
    errdefer if (publication_owned) deinitPersistedErasedCallablePublication(allocator, publication);
    const executable_signature = try buildErasedPromotedProcedureExecutableSignature(
        allocator,
        reserved,
        publication.erased,
        params,
    );
    const transforms = try publishErasedPromotedWrapperValueTransforms(
        allocator,
        artifact,
        .{
            .proc = reserved.promoted_ref.proc,
            .callable = .{
                .template = .{ .synthetic = .{ .template = reserved.template } },
                .source_fn_ty = publication.erased.source_fn_ty,
            },
        },
        executable_signature,
    );
    artifact.fillPromotedCallableWrapperBody(reserved, .{ .erased = .{
        .source_fn_ty = publication.erased.source_fn_ty,
        .params = params,
        .executable_signature = executable_signature,
        .sig_key = publication.erased.sig_key,
        .code = publication.code,
        .finite_adapter_member_targets = publication.finite_adapter_member_targets,
        .finite_adapter_branches = publication.finite_adapter_branches,
        .capture = publication.capture,
        .arg_transforms = transforms.args,
        .hidden_capture_arg = if (publication.erased.sig_key.capture_ty == null)
            .none
        else
            .{ .materialized_capture = publication.capture },
        .result_transform = transforms.result,
        .provenance = try cloneBoxBoundarySpan(allocator, publication.erased.provenance),
    } });
    publication_owned = false;
    try artifact.publishPromotedCallableWrapper(allocator, reserved);
    const generated_procedure = try publishSemanticInstantiationProcedureForPromoted(
        allocator,
        artifact,
        reserved,
        publication.erased.source_fn_ty,
    );

    const promotion_plan = try artifact.comptime_plans.appendCallablePromotion(allocator, .{ .erased = .{
        .result_plan = result_plan,
        .promoted_proc = reserved.promoted_ref,
    } });
    const proc_value = canonical.ProcedureCallableRef{
        .template = .{ .synthetic = .{ .template = reserved.template } },
        .source_fn_ty = publication.erased.source_fn_ty,
    };
    return .{
        .proc_value = proc_value,
        .source_fn_ty_payload = checked_fn_root,
        .output = .{ .promoted_procedure = reserved.promoted_ref },
        .promotion_plan = promotion_plan,
        .generated_procedure = generated_procedure,
    };
}

const PersistedErasedCallablePublication = struct {
    erased: checked_artifact.ErasedCallableResultPlan,
    code: canonical.ErasedCallableCodeRef,
    finite_adapter_member_targets: []const canonical.ExecutableSpecializationKey = &.{},
    finite_adapter_branches: []const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan = &.{},
    capture: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
};

fn persistedErasedCallablePublication(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    capture_builder: *PrivateCaptureBuilder,
    owner: checked_artifact.ReservedPromotedCallableWrapper,
    context: PromotedCallablePublicationContext,
    code: canonical.ErasedCallableCodeRef,
    erased: checked_artifact.ErasedCallableResultPlan,
    ret_layout: layout_mod.Idx,
    ret_value: Value,
) Allocator.Error!PersistedErasedCallablePublication {
    switch (code) {
        .direct_proc_value => return .{
            .erased = erased,
            .code = code,
            .finite_adapter_member_targets = &.{},
            .capture = try materializeErasedPromotedCapture(
                allocator,
                artifact,
                lowered,
                capture_builder,
                erased,
                ret_layout,
                ret_value,
            ),
        },
        .finite_set_adapter => {},
    }

    return try persistConcreteFiniteAdapterAsSingleton(
        allocator,
        artifact,
        lowered,
        owner,
        context,
        code.finite_set_adapter,
        erased,
        ret_layout,
        ret_value,
    );
}

fn deinitPersistedErasedCallablePublication(
    allocator: Allocator,
    publication: PersistedErasedCallablePublication,
) void {
    deinitExecutableSpecializationKeySlice(allocator, publication.finite_adapter_member_targets);
    deinitPublishedFiniteSetEraseAdapterBranches(allocator, publication.finite_adapter_branches);
}

fn persistConcreteFiniteAdapterAsSingleton(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    owner: checked_artifact.ReservedPromotedCallableWrapper,
    context: PromotedCallablePublicationContext,
    adapter: canonical.ErasedAdapterKey,
    erased: checked_artifact.ErasedCallableResultPlan,
    ret_layout: layout_mod.Idx,
    ret_value: Value,
) Allocator.Error!PersistedErasedCallablePublication {
    const result_plan = switch (erased.capture) {
        .finite_callable_set_value => |plan| plan,
        else => compileTimeFinalizationInvariant("persisted finite-set erased adapter did not carry a finite callable-set capture"),
    };
    const original_targets = try finiteAdapterMemberTargetsForResolvedErasedCode(
        allocator,
        lowered,
        .{ .finite_set_adapter = adapter },
    );
    defer deinitExecutableSpecializationKeySlice(allocator, original_targets);

    const hidden_physical = erasedClosureHiddenCapturePhysical(
        &lowered.lir_result.layouts,
        erasedHiddenCaptureLayout(lowered, erased),
        ret_layout,
        ret_value,
    ) orelse {
        compileTimeFinalizationInvariant("persisted finite-set erased adapter had no hidden capture payload");
    };
    const selected = selectFiniteCallableResult(
        &artifact.comptime_plans,
        lowered.callable_set_descriptors,
        &lowered.lir_result.layouts,
        result_plan,
        hidden_physical.layout_idx,
        hidden_physical.value,
    );
    const member_index = @intFromEnum(selected.planned_member.member);
    if (member_index >= original_targets.len) {
        compileTimeFinalizationInvariant("persisted finite-set erased adapter selected member exceeded target count");
    }

    const checked_fn_root = artifact.checked_types.rootForKey(selected.result_plan.source_fn_ty) orelse {
        compileTimeFinalizationInvariant("persisted finite-set erased adapter selected member source function type was not published");
    };
    const private_context = PromotedCallablePublicationContext{
        .source_binding = context.source_binding,
        .base = .{ .private_capture = privateCaptureOwner(owner) },
    };
    const published_member = try publishCallableResult(
        allocator,
        artifact,
        lowered,
        private_context,
        checked_fn_root,
        result_plan,
        selected,
    );
    const source_proc = mirProcedureRefForPublishedCallable(published_member.proc_value);
    const target_key = try persistedSingletonAdapterMemberTarget(
        allocator,
        published_member.proc_value,
        source_proc,
        erased,
        original_targets[member_index],
    );
    errdefer {
        var owned = target_key;
        repr.deinitExecutableSpecializationKey(allocator, &owned);
    }
    const callable_set_key = persistedSingletonCallableSetKey(published_member.proc_value, source_proc, target_key);
    try publishPersistedSingletonCallableSetDescriptor(
        allocator,
        artifact,
        callable_set_key,
        published_member.proc_value,
        source_proc,
    );
    const hidden_capture = try publishPersistedSingletonCallableSetPayload(
        allocator,
        artifact,
        callable_set_key,
    );
    const hidden_capture_key = hidden_capture.exec_ty_key;
    const sig_key = canonical.ErasedFnSigKey{
        .source_fn_ty = erased.source_fn_ty,
        .abi = erased.sig_key.abi,
        .capture_ty = hidden_capture_key,
    };
    const hidden_capture_keys = [_]canonical.CanonicalExecValueTypeKey{hidden_capture_key};
    const capture_shape_key = repr.captureShapeKeyForExecKeys(&hidden_capture_keys);
    const singleton_adapter = canonical.ErasedAdapterKey{
        .source_fn_ty = erased.source_fn_ty,
        .callable_set_key = callable_set_key,
        .erased_fn_sig_key = sig_key,
        .capture_shape_key = capture_shape_key,
    };

    const branches = try persistedSingletonAdapterBranches(
        allocator,
        artifact,
        .{
            .proc = owner.promoted_ref.proc,
            .callable = .{
                .template = .{ .synthetic = .{ .template = owner.template } },
                .source_fn_ty = erased.source_fn_ty,
            },
        },
        singleton_adapter,
        erased,
        published_member.source_fn_ty_payload,
        null,
        target_key,
    );
    errdefer deinitPublishedFiniteSetEraseAdapterBranches(allocator, branches);

    const targets = try allocator.alloc(canonical.ExecutableSpecializationKey, 1);
    targets[0] = target_key;
    return .{
        .erased = .{
            .source_fn_ty = erased.source_fn_ty,
            .sig_key = sig_key,
            .provenance = erased.provenance,
            .code_plan = erased.code_plan,
            .capture = erased.capture,
            .result_ty = erased.result_ty,
            .executable_signature_payloads = erasedSignaturePayloadsWithHiddenCapture(
                erased.executable_signature_payloads,
                hidden_capture,
                capture_shape_key,
            ),
        },
        .code = .{ .finite_set_adapter = singleton_adapter },
        .finite_adapter_member_targets = targets,
        .finite_adapter_branches = branches,
        .capture = .{ .node = try artifact.comptime_plans.appendErasedCaptureExecutableMaterializationNode(
            allocator,
            .{ .finite_callable_set = .{
                .source_fn_ty = selected.result_plan.source_fn_ty,
                .callable_set_key = callable_set_key,
                .selected_member = canonical.onlyCallableSetMemberId(),
                .captures = &.{},
            } },
        ) },
    };
}

fn persistedSingletonAdapterBranches(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    owner_proc: canonical.MirProcedureRef,
    adapter: canonical.ErasedAdapterKey,
    erased: checked_artifact.ErasedCallableResultPlan,
    member_proc_source_fn_ty_payload: checked_artifact.CheckedTypeId,
    member_lifted_owner_source_fn_ty_payload: ?checked_artifact.CheckedTypeId,
    target_key: canonical.ExecutableSpecializationKey,
) Allocator.Error![]const checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan {
    if (target_key.exec_arg_tys.len != erased.executable_signature_payloads.erased_call_arg_keys.len) {
        compileTimeFinalizationInvariant("persisted finite-set adapter branch target arity differs from erased ABI");
    }
    if (!repr.canonicalExecValueTypeKeyEql(target_key.exec_ret_ty, erased.executable_signature_payloads.erased_call_ret_key)) {
        compileTimeFinalizationInvariant("persisted finite-set adapter branch target return differs from erased ABI");
    }

    const branches = try allocator.alloc(checked_artifact.PublishedFiniteSetEraseAdapterBranchPlan, 1);
    @memset(branches, .{
        .member = .{
            .callable_set_key = adapter.callable_set_key,
            .member_index = undefined,
        },
        .member_proc_source_fn_ty_payload = undefined,
        .member_lifted_owner_source_fn_ty_payload = null,
        .target_key = .{
            .base = undefined,
            .requested_fn_ty = .{ .bytes = [_]u8{0} ** 32 },
            .exec_arg_tys = &.{},
            .exec_ret_ty = .{ .bytes = [_]u8{0} ** 32 },
            .callable_repr_mode = .direct,
            .capture_shape_key = .{ .bytes = [_]u8{0} ** 32 },
        },
        .arg_transforms = &.{},
        .capture_transforms = &.{},
        .result_transform = .{
            .artifact = artifact.key,
            .transform = undefined,
        },
    });
    errdefer deinitPublishedFiniteSetEraseAdapterBranches(allocator, branches);

    const arg_transforms: []checked_artifact.PublishedExecutableValueTransformRef = if (target_key.exec_arg_tys.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.PublishedExecutableValueTransformRef, target_key.exec_arg_tys.len);
    errdefer if (arg_transforms.len > 0) allocator.free(arg_transforms);

    const provenance = [_]checked_artifact.BoxErasureProvenance{.{ .promoted_wrapper = owner_proc }};
    var planner = PublishedValueTransformPlanner{
        .allocator = allocator,
        .artifact = artifact,
        .provenance = provenance[0..],
    };

    for (target_key.exec_arg_tys, erased.executable_signature_payloads.erased_call_args, erased.executable_signature_payloads.erased_call_arg_keys, 0..) |target_arg_key, erased_arg_ty, erased_arg_key, i| {
        if (!repr.canonicalExecValueTypeKeyEql(target_arg_key, erased_arg_key)) {
            compileTimeFinalizationInvariant("persisted finite-set adapter branch target argument differs from erased ABI");
        }
        arg_transforms[i] = try planner.publish(
            .{ .ty = erased_arg_ty, .key = erased_arg_key },
            executableEndpointForKey(artifact, target_arg_key),
        );
    }

    branches[0] = .{
        .member = .{
            .callable_set_key = adapter.callable_set_key,
            .member_index = canonical.onlyCallableSetMemberId(),
        },
        .member_proc_source_fn_ty_payload = member_proc_source_fn_ty_payload,
        .member_lifted_owner_source_fn_ty_payload = member_lifted_owner_source_fn_ty_payload,
        .target_key = try repr.cloneExecutableSpecializationKey(allocator, target_key),
        .arg_transforms = arg_transforms,
        .capture_transforms = &.{},
        .result_transform = try planner.publish(
            executableEndpointForKey(artifact, target_key.exec_ret_ty),
            .{
                .ty = erased.executable_signature_payloads.erased_call_ret,
                .key = erased.executable_signature_payloads.erased_call_ret_key,
            },
        ),
    };
    return branches;
}

fn erasedSignaturePayloadsWithHiddenCapture(
    payloads: checked_artifact.ErasedPromotedProcedureExecutableSignaturePayloads,
    hidden_capture: checked_artifact.ExecutableHiddenCapturePayload,
    capture_shape_key: canonical.CaptureShapeKey,
) checked_artifact.ErasedPromotedProcedureExecutableSignaturePayloads {
    return .{
        .source_fn_ty = payloads.source_fn_ty,
        .param_exec_tys = payloads.param_exec_tys,
        .param_exec_ty_keys = payloads.param_exec_ty_keys,
        .wrapper_ret = payloads.wrapper_ret,
        .wrapper_ret_key = payloads.wrapper_ret_key,
        .erased_call_args = payloads.erased_call_args,
        .erased_call_arg_keys = payloads.erased_call_arg_keys,
        .erased_call_ret = payloads.erased_call_ret,
        .erased_call_ret_key = payloads.erased_call_ret_key,
        .hidden_capture = hidden_capture,
        .capture_shape_key = capture_shape_key,
    };
}

fn mirProcedureRefForPublishedCallable(
    proc_value: canonical.ProcedureCallableRef,
) canonical.MirProcedureRef {
    const template = switch (proc_value.template) {
        .checked => |checked| checked,
        .synthetic => |synthetic| synthetic.template,
        .lifted => compileTimeFinalizationInvariant("persisted callable adapter member promotion leaked a lifted procedure"),
    };
    return .{
        .proc = .{
            .artifact = template.artifact,
            .proc_base = template.proc_base,
        },
        .callable = proc_value,
    };
}

fn persistedSingletonAdapterMemberTarget(
    allocator: Allocator,
    proc_value: canonical.ProcedureCallableRef,
    source_proc: canonical.MirProcedureRef,
    erased: checked_artifact.ErasedCallableResultPlan,
    original_target: canonical.ExecutableSpecializationKey,
) Allocator.Error!canonical.ExecutableSpecializationKey {
    if (!std.meta.eql(proc_value.source_fn_ty.bytes, original_target.requested_fn_ty.bytes)) {
        compileTimeFinalizationInvariant("persisted singleton adapter member source function type differs from original target");
    }
    if (original_target.exec_arg_tys.len != erased.executable_signature_payloads.param_exec_ty_keys.len) {
        compileTimeFinalizationInvariant("persisted singleton adapter member arity differs from promoted executable signature");
    }
    return .{
        .base = source_proc.proc.proc_base,
        .requested_fn_ty = proc_value.source_fn_ty,
        .exec_arg_tys = try cloneExecValueTypeKeySlice(allocator, erased.executable_signature_payloads.param_exec_ty_keys),
        .exec_ret_ty = erased.executable_signature_payloads.wrapper_ret_key,
        .callable_repr_mode = .direct,
        .capture_shape_key = repr.captureShapeKeyForExecKeys(&.{}),
    };
}

fn persistedSingletonCallableSetKey(
    proc_value: canonical.ProcedureCallableRef,
    source_proc: canonical.MirProcedureRef,
    target_key: canonical.ExecutableSpecializationKey,
) canonical.CanonicalCallableSetKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashPathTag(&hasher, "persisted-singleton-callable-set");
    hasher.update(&source_proc.proc.artifact.bytes);
    hashPathU32(&hasher, @intFromEnum(source_proc.proc.proc_base));
    hasher.update(&proc_value.source_fn_ty.bytes);
    hasher.update(&target_key.requested_fn_ty.bytes);
    hashPathU32(&hasher, @intCast(target_key.exec_arg_tys.len));
    for (target_key.exec_arg_tys) |arg| hasher.update(&arg.bytes);
    hasher.update(&target_key.exec_ret_ty.bytes);
    hashPathU32(&hasher, @intFromEnum(target_key.callable_repr_mode));
    hasher.update(&target_key.capture_shape_key.bytes);
    return .{ .bytes = hasher.finalResult() };
}

fn publishPersistedSingletonCallableSetDescriptor(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    key: canonical.CanonicalCallableSetKey,
    proc_value: canonical.ProcedureCallableRef,
    source_proc: canonical.MirProcedureRef,
) Allocator.Error!void {
    const members = [_]canonical.CanonicalCallableSetMember{.{
        .member = canonical.onlyCallableSetMemberId(),
        .proc_value = proc_value,
        .source_proc = source_proc,
        .capture_slots = &.{},
        .capture_shape_key = repr.captureShapeKeyForExecKeys(&.{}),
    }};
    const descriptors = [_]canonical.CanonicalCallableSetDescriptor{.{
        .key = key,
        .members = members[0..],
    }};
    try artifact.callable_set_descriptors.publishFromDescriptors(allocator, descriptors[0..]);
}

fn publishPersistedSingletonCallableSetPayload(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    key: canonical.CanonicalCallableSetKey,
) Allocator.Error!checked_artifact.ExecutableHiddenCapturePayload {
    const payload_key = repr.finiteCallableSetExecValueTypeKey(key);
    const artifact_ref = canonical.ArtifactRef{ .bytes = artifact.key.bytes };
    if (artifact.executable_type_payloads.refForKey(artifact_ref, payload_key)) |existing| {
        return .{
            .exec_ty = existing,
            .exec_ty_key = payload_key,
        };
    }

    const members = try allocator.alloc(checked_artifact.ExecutableCallableSetMemberPayload, 1);
    var members_owned = true;
    errdefer if (members_owned) allocator.free(members);
    members[0] = .{
        .member = canonical.onlyCallableSetMemberId(),
        .payload_ty = null,
        .payload_ty_key = null,
    };
    _ = try artifact.executable_type_payloads.append(allocator, payload_key, .{ .callable_set = .{
        .key = key,
        .members = members,
    } });
    members_owned = false;
    const ref = artifact.executable_type_payloads.refForKey(artifact_ref, payload_key) orelse {
        compileTimeFinalizationInvariant("persisted singleton callable-set payload was not published");
    };
    return .{
        .exec_ty = ref,
        .exec_ty_key = payload_key,
    };
}

fn materializeErasedPromotedCapture(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    capture_builder: *PrivateCaptureBuilder,
    erased: checked_artifact.ErasedCallableResultPlan,
    ret_layout: layout_mod.Idx,
    ret_value: Value,
) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationPlan {
    return switch (erased.capture) {
        .none => blk: {
            if (erased.sig_key.capture_ty != null) {
                compileTimeFinalizationInvariant("erased callable result had no capture materialization but signature has hidden capture type");
            }
            break :blk .none;
        },
        .zero_sized_typed => |ty| blk: {
            const expected = erased.sig_key.capture_ty orelse {
                compileTimeFinalizationInvariant("erased callable zero-sized capture had no hidden capture type");
            };
            if (!std.meta.eql(expected.bytes, ty.bytes)) {
                compileTimeFinalizationInvariant("erased callable zero-sized capture type differs from signature hidden capture type");
            }
            break :blk .{ .zero_sized_typed = ty };
        },
        .whole_hidden_capture_value => |capture| blk: {
            const physical = erasedClosureHiddenCapturePhysical(
                &lowered.lir_result.layouts,
                erasedHiddenCaptureLayout(lowered, erased),
                ret_layout,
                ret_value,
            ) orelse {
                compileTimeFinalizationInvariant("erased callable whole hidden capture had no returned hidden capture payload");
            };
            break :blk try capture_builder.executablePlan(capture.plan, physical);
        },
        .proc_capture_tuple => |captures| blk: {
            const physical = erasedClosureHiddenCapturePhysical(
                &lowered.lir_result.layouts,
                erasedHiddenCaptureLayout(lowered, erased),
                ret_layout,
                ret_value,
            ) orelse {
                compileTimeFinalizationInvariant("erased proc-value capture tuple had no returned hidden capture payload");
            };
            if (captures.len == 0) {
                compileTimeFinalizationInvariant("erased proc-value capture tuple materialization had no captures");
            }
            const tuple_items = try allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationPlan, captures.len);
            errdefer allocator.free(tuple_items);
            const tuple_layout = lowered.lir_result.layouts.getLayout(physical.layout_idx);
            if (tuple_layout.tag != .struct_) {
                compileTimeFinalizationInvariant("erased proc-value capture tuple did not lower to a struct layout");
            }
            for (captures, 0..) |capture, i| {
                const field = structFieldValue(&lowered.lir_result.layouts, tuple_layout, physical.value, @intCast(i));
                tuple_items[i] = try capture_builder.executablePlan(capture.plan, field);
            }
            break :blk .{ .node = try artifact.comptime_plans.appendErasedCaptureExecutableMaterializationNode(
                allocator,
                .{ .tuple = tuple_items },
            ) };
        },
        .finite_callable_set_value => |result_plan| blk: {
            const physical = erasedClosureHiddenCapturePhysical(
                &lowered.lir_result.layouts,
                erasedHiddenCaptureLayout(lowered, erased),
                ret_layout,
                ret_value,
            ) orelse {
                compileTimeFinalizationInvariant("erased finite callable-set adapter capture had no returned hidden capture payload");
            };
            const finite = try materializedFiniteCallableSetValue(
                allocator,
                lowered,
                capture_builder,
                result_plan,
                physical.layout_idx,
                physical.value,
            );
            break :blk .{ .node = try artifact.comptime_plans.appendErasedCaptureExecutableMaterializationNode(
                allocator,
                .{ .finite_callable_set = finite },
            ) };
        },
    };
}

fn materializedFiniteCallableSetValue(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    capture_builder: *PrivateCaptureBuilder,
    result_plan: checked_artifact.CallableResultPlanId,
    layout_idx: layout_mod.Idx,
    value: Value,
) Allocator.Error!checked_artifact.MaterializedFiniteCallableSetValue {
    const selected = selectFiniteCallableResult(
        &capture_builder.artifact.comptime_plans,
        capture_builder.callable_set_descriptors,
        &lowered.lir_result.layouts,
        result_plan,
        layout_idx,
        value,
    );
    if (selected.descriptor_member.capture_slots.len != selected.planned_member.capture_slots.len) {
        compileTimeFinalizationInvariant("materialized finite erased capture selected member capture schema disagrees with result plan");
    }
    if (capture_builder.dependencies) |dependencies| {
        const published_member_proc = selected.descriptor_member.published_proc_value orelse
            compileTimeFinalizationInvariant("materialized finite callable set tried to persist a runtime-only procedure identity");
        try dependencies.appendProcedureCallable(published_member_proc);
    }
    const captures = try allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationPlan, selected.planned_member.capture_slots.len);
    errdefer allocator.free(captures);
    for (selected.planned_member.capture_slots, selected.descriptor_member.capture_slots, 0..) |slot_plan, slot, i| {
        if (slot.slot != @as(u32, @intCast(i))) {
            compileTimeFinalizationInvariant("materialized finite erased capture slots are not canonical");
        }
        captures[i] = try capture_builder.executablePlan(
            slot_plan,
            captureSlotValue(&lowered.lir_result.layouts, selected, @intCast(i)),
        );
    }

    return .{
        .source_fn_ty = selected.result_plan.source_fn_ty,
        .callable_set_key = selected.result_plan.callable_set_key,
        .selected_member = selected.planned_member.member,
        .captures = captures,
    };
}

fn materializedErasedCallableValue(
    capture_builder: *PrivateCaptureBuilder,
    erased: checked_artifact.ErasedCallableResultPlan,
    layout_idx: layout_mod.Idx,
    value: Value,
) Allocator.Error!checked_artifact.MaterializedErasedCallableValue {
    const code = resolveErasedCallableResultCode(
        capture_builder.artifact,
        capture_builder.lowered,
        erased,
        layout_idx,
        value,
    );
    if (capture_builder.dependencies) |dependencies| try dependencies.collectErasedCodeRef(code);
    return .{
        .source_fn_ty = erased.source_fn_ty,
        .sig_key = erased.sig_key,
        .code = code,
        .capture = try materializeErasedPromotedCapture(
            capture_builder.allocator,
            capture_builder.artifact,
            capture_builder.lowered,
            capture_builder,
            erased,
            layout_idx,
            value,
        ),
        .provenance = try cloneBoxBoundarySpan(capture_builder.allocator, erased.provenance),
    };
}

fn resolveErasedCallableResultCode(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    erased: checked_artifact.ErasedCallableResultPlan,
    layout_idx: layout_mod.Idx,
    value: Value,
) canonical.ErasedCallableCodeRef {
    return switch (erased.code_plan) {
        .materialized_by_lowering => |code| code,
        .read_from_interpreted_erased_value => blk: {
            const lir_proc = erasedClosureCodeProc(lowered, layout_idx, value);
            const entry = loweredErasedCallableCodeEntry(lowered, lir_proc) orelse {
                compileTimeFinalizationInvariant("interpreted erased callable code was not published in lowered code map");
            };
            validateLoweredErasedCallableCodeEntry(artifact, erased, entry);
            break :blk entry.code;
        },
    };
}

fn loweredErasedCallableCodeEntry(
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    lir_proc: lir.LIR.LirProcSpecId,
) ?lir.CheckedPipeline.LoweredErasedCallableCodeEntry {
    for (lowered.erased_callable_code_map) |entry| {
        if (entry.lir_proc == lir_proc) return entry;
    }
    return null;
}

fn finiteAdapterMemberTargetsForResolvedErasedCode(
    allocator: Allocator,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    code: canonical.ErasedCallableCodeRef,
) Allocator.Error![]const canonical.ExecutableSpecializationKey {
    switch (code) {
        .direct_proc_value => return &.{},
        .finite_set_adapter => {},
    }
    for (lowered.erased_callable_code_map) |entry| {
        if (!erasedCallableCodeRefEql(entry.code, code)) continue;
        if (entry.finite_adapter_member_targets.len == 0) {
            compileTimeFinalizationInvariant("finite erased callable code entry has no member targets");
        }
        return try cloneExecutableSpecializationKeySlice(allocator, entry.finite_adapter_member_targets);
    }
    compileTimeFinalizationInvariant("finite erased callable code was not published in lowered code map");
}

fn erasedCallableCodeRefEql(a: canonical.ErasedCallableCodeRef, b: canonical.ErasedCallableCodeRef) bool {
    return switch (a) {
        .direct_proc_value => |left| switch (b) {
            .direct_proc_value => |right| canonical.procedureCallableRefEql(left.proc_value, right.proc_value) and
                repr.captureShapeKeyEql(left.capture_shape_key, right.capture_shape_key),
            .finite_set_adapter => false,
        },
        .finite_set_adapter => |left| switch (b) {
            .direct_proc_value => false,
            .finite_set_adapter => |right| erasedAdapterKeyEql(left, right),
        },
    };
}

fn erasedAdapterKeyEql(a: canonical.ErasedAdapterKey, b: canonical.ErasedAdapterKey) bool {
    return repr.canonicalTypeKeyEql(a.source_fn_ty, b.source_fn_ty) and
        repr.callableSetKeyEql(a.callable_set_key, b.callable_set_key) and
        repr.erasedFnSigKeyEql(a.erased_fn_sig_key, b.erased_fn_sig_key) and
        repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
}

fn cloneExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const canonical.ExecutableSpecializationKey,
) Allocator.Error![]const canonical.ExecutableSpecializationKey {
    if (keys.len == 0) return &.{};
    const out = try allocator.alloc(canonical.ExecutableSpecializationKey, keys.len);
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

fn cloneCallableResultMemberTargetPlan(
    allocator: Allocator,
    target: checked_artifact.CallableResultMemberTargetPlan,
) Allocator.Error!checked_artifact.CallableResultMemberTargetPlan {
    return switch (target) {
        .artifact_owned => |key| .{ .artifact_owned = try repr.cloneExecutableSpecializationKey(allocator, key) },
        .member_proc_relative => |endpoint| .{ .member_proc_relative = .{
            .requested_fn_ty = endpoint.requested_fn_ty,
            .exec_arg_tys = if (endpoint.exec_arg_tys.len == 0)
                &.{}
            else
                try allocator.dupe(canonical.CanonicalExecValueTypeKey, endpoint.exec_arg_tys),
            .exec_ret_ty = endpoint.exec_ret_ty,
            .callable_repr_mode = endpoint.callable_repr_mode,
            .capture_shape_key = endpoint.capture_shape_key,
        } },
    };
}

fn deinitCallableResultMemberTargetPlan(
    allocator: Allocator,
    target: checked_artifact.CallableResultMemberTargetPlan,
) void {
    switch (target) {
        .artifact_owned => |key| {
            var owned = key;
            repr.deinitExecutableSpecializationKey(allocator, &owned);
        },
        .member_proc_relative => |endpoint| if (endpoint.exec_arg_tys.len > 0) allocator.free(endpoint.exec_arg_tys),
    }
}

fn cloneExecValueTypeKeySlice(
    allocator: Allocator,
    keys: []const canonical.CanonicalExecValueTypeKey,
) Allocator.Error![]const canonical.CanonicalExecValueTypeKey {
    if (keys.len == 0) return &.{};
    return try allocator.dupe(canonical.CanonicalExecValueTypeKey, keys);
}

fn deinitExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const canonical.ExecutableSpecializationKey,
) void {
    for (keys) |key| {
        var owned = key;
        repr.deinitExecutableSpecializationKey(allocator, &owned);
    }
    if (keys.len > 0) allocator.free(keys);
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

fn executableEndpointForKey(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    key: canonical.CanonicalExecValueTypeKey,
) checked_artifact.ExecutableValueEndpoint {
    const artifact_ref = canonical.ArtifactRef{ .bytes = artifact.key.bytes };
    const ty = artifact.executable_type_payloads.refForKey(artifact_ref, key) orelse {
        compileTimeFinalizationInvariant("published finite adapter branch endpoint key has no executable payload");
    };
    return .{
        .ty = ty,
        .key = key,
    };
}

fn validateLoweredErasedCallableCodeEntry(
    artifact: *const checked_artifact.CheckedModuleArtifact,
    erased: checked_artifact.ErasedCallableResultPlan,
    entry: lir.CheckedPipeline.LoweredErasedCallableCodeEntry,
) void {
    if (!repr.canonicalTypeKeyEql(entry.source_fn_ty, erased.source_fn_ty)) {
        compileTimeFinalizationInvariant("interpreted erased callable source function type differs from result plan");
    }
    if (!repr.captureShapeKeyEql(entry.capture_shape_key, erased.executable_signature_payloads.capture_shape_key)) {
        compileTimeFinalizationInvariant("interpreted erased callable capture shape differs from result plan");
    }

    const abi = artifact.erased_fn_abis.abiFor(erased.sig_key.abi) orelse {
        compileTimeFinalizationInvariant("interpreted erased callable result references missing erased ABI");
    };
    if (entry.exec_arg_tys.len != abi.arg_exec_keys.len) {
        compileTimeFinalizationInvariant("interpreted erased callable argument ABI arity differs from result plan");
    }
    for (entry.exec_arg_tys, abi.arg_exec_keys) |entry_arg, expected_arg| {
        if (!repr.canonicalExecValueTypeKeyEql(entry_arg, expected_arg)) {
            compileTimeFinalizationInvariant("interpreted erased callable argument ABI differs from result plan");
        }
    }
    if (!repr.canonicalExecValueTypeKeyEql(entry.exec_ret_ty, abi.ret_exec_key)) {
        compileTimeFinalizationInvariant("interpreted erased callable return ABI differs from result plan");
    }

    switch (entry.code) {
        .direct_proc_value => |direct| {
            if (!repr.canonicalTypeKeyEql(direct.proc_value.source_fn_ty, erased.source_fn_ty)) {
                compileTimeFinalizationInvariant("interpreted direct erased code source function type differs from result plan");
            }
            if (!repr.captureShapeKeyEql(direct.capture_shape_key, entry.capture_shape_key)) {
                compileTimeFinalizationInvariant("interpreted direct erased code capture shape differs from lowered entry");
            }
        },
        .finite_set_adapter => |adapter| {
            if (entry.finite_adapter_member_targets.len == 0) {
                compileTimeFinalizationInvariant("interpreted finite-set adapter code has no member targets");
            }
            if (!repr.canonicalTypeKeyEql(adapter.source_fn_ty, erased.source_fn_ty)) {
                compileTimeFinalizationInvariant("interpreted finite-set adapter source function type differs from result plan");
            }
            if (!repr.erasedFnSigKeyEql(adapter.erased_fn_sig_key, erased.sig_key)) {
                compileTimeFinalizationInvariant("interpreted finite-set adapter erased signature differs from result plan");
            }
            if (!repr.captureShapeKeyEql(adapter.capture_shape_key, entry.capture_shape_key)) {
                compileTimeFinalizationInvariant("interpreted finite-set adapter capture shape differs from lowered entry");
            }
        },
    }
}

fn erasedClosureCodeProc(
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    layout_idx: layout_mod.Idx,
    value: Value,
) lir.LIR.LirProcSpecId {
    const layouts = &lowered.lir_result.layouts;
    const layout = layouts.getLayout(layout_idx);
    if (layout.tag != .erased_callable) {
        compileTimeFinalizationInvariant("erased callable result did not lower to an erased_callable layout");
    }
    const payload_ptr = value.read(?[*]u8) orelse {
        compileTimeFinalizationInvariant("erased callable result payload pointer was null");
    };
    return Interpreter.erasedCallableInterpreterProcId(payload_ptr);
}

fn erasedHiddenCaptureLayout(
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    erased: checked_artifact.ErasedCallableResultPlan,
) layout_mod.Idx {
    const hidden = erased.executable_signature_payloads.hidden_capture orelse {
        compileTimeFinalizationInvariant("erased callable capture materialization has no published hidden capture payload");
    };
    if (erased.sig_key.capture_ty) |expected| {
        if (!repr.canonicalExecValueTypeKeyEql(hidden.exec_ty_key, expected)) {
            compileTimeFinalizationInvariant("erased callable hidden capture payload key differs from erased signature key");
        }
    } else {
        compileTimeFinalizationInvariant("erased callable hidden capture payload exists but erased signature has no capture type");
    }
    return lowered.lir_result.requestedLayoutForKey(hidden.exec_ty_key) orelse {
        compileTimeFinalizationInvariant("erased callable hidden capture payload layout was not requested during LIR lowering");
    };
}

fn erasedClosureHiddenCapturePhysical(
    layouts: *const layout_mod.Store,
    expected_capture_layout_idx: layout_mod.Idx,
    layout_idx: layout_mod.Idx,
    value: Value,
) ?PhysicalValue {
    const layout = layouts.getLayout(layout_idx);
    if (layout.tag != .erased_callable) {
        compileTimeFinalizationInvariant("erased callable result did not lower to an erased_callable layout");
    }
    const payload = value.read(?[*]u8) orelse {
        compileTimeFinalizationInvariant("erased callable result payload pointer was null");
    };
    if (layouts.isZeroSized(layouts.getLayout(expected_capture_layout_idx))) {
        return .{
            .layout_idx = .zst,
            .value = Value.zst,
        };
    }
    return .{
        .layout_idx = expected_capture_layout_idx,
        .value = .{ .ptr = Interpreter.erasedCallableInterpreterSemanticCapturePtr(payload) },
    };
}

const ErasedPromotedWrapperValueTransforms = struct {
    args: []const checked_artifact.PublishedExecutableValueTransformRef,
    result: checked_artifact.PublishedExecutableValueTransformRef,
};

fn publishErasedPromotedWrapperValueTransforms(
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    owner: canonical.MirProcedureRef,
    signature: checked_artifact.ErasedPromotedProcedureExecutableSignature,
) Allocator.Error!ErasedPromotedWrapperValueTransforms {
    if (signature.wrapper_params.len != signature.erased_call_args.len or
        signature.wrapper_params.len != signature.erased_call_arg_keys.len)
    {
        compileTimeFinalizationInvariant("erased promoted wrapper transform arity differs from signature");
    }

    const arg_transforms: []checked_artifact.PublishedExecutableValueTransformRef = if (signature.wrapper_params.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.PublishedExecutableValueTransformRef, signature.wrapper_params.len);
    errdefer if (arg_transforms.len > 0) allocator.free(arg_transforms);

    const provenance = [_]checked_artifact.BoxErasureProvenance{.{ .promoted_wrapper = owner }};
    var planner = PublishedValueTransformPlanner{
        .allocator = allocator,
        .artifact = artifact,
        .provenance = provenance[0..],
    };

    for (signature.wrapper_params, signature.erased_call_args, signature.erased_call_arg_keys, 0..) |param, erased_arg, erased_arg_key, i| {
        arg_transforms[i] = try planner.publish(
            .{ .ty = param.exec_ty, .key = param.exec_ty_key },
            .{ .ty = erased_arg, .key = erased_arg_key },
        );
    }

    const result_transform = try planner.publish(
        .{ .ty = signature.erased_call_ret, .key = signature.erased_call_ret_key },
        .{ .ty = signature.wrapper_ret, .key = signature.wrapper_ret_key },
    );

    return .{
        .args = arg_transforms,
        .result = result_transform,
    };
}

const PublishedValueTransformPlanner = struct {
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    provenance: []const checked_artifact.BoxErasureProvenance,

    fn publish(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
    ) Allocator.Error!checked_artifact.PublishedExecutableValueTransformRef {
        return .{
            .artifact = self.artifact.key,
            .transform = try self.plan(from, to),
        };
    }

    fn plan(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        self.verifyEndpoint(from);
        self.verifyEndpoint(to);

        if (repr.canonicalExecValueTypeKeyEql(from.key, to.key)) {
            return try self.append(from, to, .none, .identity);
        }

        const from_payload = self.payload(from);
        const to_payload = self.payload(to);
        return switch (from_payload) {
            .record => |source| switch (to_payload) {
                .record => |target| try self.planRecord(from, to, source, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .tuple => |source| switch (to_payload) {
                .tuple => |target| try self.planTuple(from, to, source, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .tag_union => |source| switch (to_payload) {
                .tag_union => |target| try self.planTagUnion(from, to, source, target),
                .nominal => |target| try self.planBackingToNominal(from, to, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .nominal => |source| switch (to_payload) {
                .nominal => |target| try self.planNominal(from, to, source, target),
                .tag_union => try self.planNominalToBacking(from, to, source),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .list => |source| switch (to_payload) {
                .list => |target| try self.planList(from, to, source, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .box => |source| switch (to_payload) {
                .box => |target| try self.planBox(from, to, source, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .callable_set => |source| switch (to_payload) {
                .erased_fn => |target| try self.planFiniteCallableToErased(from, to, source, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .erased_fn => |source| switch (to_payload) {
                .erased_fn => |target| try self.planAlreadyErasedCallable(from, to, source, target),
                else => self.transformPayloadInvariant(from_payload, to_payload),
            },
            .primitive,
            .vacant_callable_slot,
            .recursive_ref,
            .pending,
            => self.transformPayloadInvariant(from_payload, to_payload),
        };
    }

    fn append(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        provenance: checked_artifact.ValueTransformProvenance,
        op: checked_artifact.ExecutableValueTransformOp,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        return try self.artifact.executable_value_transforms.append(self.allocator, .{
            .from = from,
            .to = to,
            .provenance = provenance,
            .op = op,
        });
    }

    fn provenanceForTransform(self: *PublishedValueTransformPlanner) Allocator.Error!checked_artifact.ValueTransformProvenance {
        if (self.provenance.len == 0) {
            compileTimeFinalizationInvariant("published erased-wrapper value transform has no Box(T) provenance");
        }
        return .{ .box_erasure = try self.allocator.dupe(checked_artifact.BoxErasureProvenance, self.provenance) };
    }

    fn payload(
        self: *PublishedValueTransformPlanner,
        endpoint: checked_artifact.ExecutableValueEndpoint,
    ) checked_artifact.ExecutableTypePayload {
        return self.artifact.executable_type_payloads.get(endpoint.ty.payload);
    }

    fn verifyEndpoint(
        self: *PublishedValueTransformPlanner,
        endpoint: checked_artifact.ExecutableValueEndpoint,
    ) void {
        if (!std.meta.eql(endpoint.ty.artifact.bytes, self.artifact.key.bytes)) {
            compileTimeFinalizationInvariant("published erased-wrapper value transform endpoint points at a different artifact");
        }
        const actual = self.artifact.executable_type_payloads.keyFor(endpoint.ty.payload);
        if (!repr.canonicalExecValueTypeKeyEql(actual, endpoint.key)) {
            compileTimeFinalizationInvariant("published erased-wrapper value transform endpoint key differs from payload key");
        }
    }

    fn transformPayloadInvariant(
        _: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableTypePayload,
        to: checked_artifact.ExecutableTypePayload,
    ) noreturn {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "compile-time finalization invariant violated: erased promoted wrapper value transform has incompatible executable payloads: {s} -> {s}",
                .{ @tagName(from), @tagName(to) },
            );
        }
        unreachable;
    }

    fn planRecord(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: []const checked_artifact.ExecutableRecordFieldPayload,
        target: []const checked_artifact.ExecutableRecordFieldPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        const fields = try self.allocator.alloc(checked_artifact.ValueTransformRecordField, target.len);
        errdefer self.allocator.free(fields);

        for (target, 0..) |target_field, i| {
            const source_field = sourceRecordFieldPayload(source, target_field.field) orelse {
                compileTimeFinalizationInvariant("published erased-wrapper record transform target field has no source field");
            };
            fields[i] = .{
                .field = target_field.field,
                .transform = try self.plan(
                    .{ .ty = source_field.ty, .key = source_field.key },
                    .{ .ty = target_field.ty, .key = target_field.key },
                ),
            };
        }

        return try self.append(from, to, try self.provenanceForTransform(), .{ .record = fields });
    }

    fn planTuple(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: []const checked_artifact.ExecutableTupleElemPayload,
        target: []const checked_artifact.ExecutableTupleElemPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        if (source.len != target.len) {
            compileTimeFinalizationInvariant("published erased-wrapper tuple transform arity mismatch");
        }
        const elems = try self.allocator.alloc(checked_artifact.ValueTransformTupleElem, target.len);
        errdefer self.allocator.free(elems);

        for (target, 0..) |target_elem, i| {
            const source_elem = sourceTupleElemPayload(source, target_elem.index) orelse {
                compileTimeFinalizationInvariant("published erased-wrapper tuple transform target index has no source element");
            };
            elems[i] = .{
                .index = target_elem.index,
                .transform = try self.plan(
                    .{ .ty = source_elem.ty, .key = source_elem.key },
                    .{ .ty = target_elem.ty, .key = target_elem.key },
                ),
            };
        }

        return try self.append(from, to, try self.provenanceForTransform(), .{ .tuple = elems });
    }

    fn planTagUnion(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: []const checked_artifact.ExecutableTagVariantPayload,
        target: []const checked_artifact.ExecutableTagVariantPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        const cases = try self.allocator.alloc(checked_artifact.ValueTransformTagCase, source.len);
        @memset(cases, .{
            .source_tag = undefined,
            .target_tag = undefined,
            .payloads = &.{},
        });
        errdefer {
            for (cases) |case| if (case.payloads.len > 0) self.allocator.free(case.payloads);
            self.allocator.free(cases);
        }

        for (source, 0..) |source_variant, i| {
            const target_variant = targetTagVariantPayload(target, source_variant.tag) orelse {
                compileTimeFinalizationInvariant("published erased-wrapper tag transform source tag has no target tag");
            };
            if (source_variant.payloads.len != target_variant.payloads.len) {
                compileTimeFinalizationInvariant("published erased-wrapper tag transform payload arity mismatch");
            }
            const payloads = try self.allocator.alloc(checked_artifact.ValueTransformTagPayloadEdge, target_variant.payloads.len);
            errdefer self.allocator.free(payloads);

            for (target_variant.payloads, 0..) |target_payload, payload_i| {
                const source_payload = sourceTagPayload(source_variant.payloads, target_payload.index) orelse {
                    compileTimeFinalizationInvariant("published erased-wrapper tag transform target payload has no source payload");
                };
                payloads[payload_i] = .{
                    .source_payload_index = source_payload.index,
                    .target_payload_index = target_payload.index,
                    .transform = try self.plan(
                        .{ .ty = source_payload.ty, .key = source_payload.key },
                        .{ .ty = target_payload.ty, .key = target_payload.key },
                    ),
                };
            }

            cases[i] = .{
                .source_tag = source_variant.tag,
                .target_tag = target_variant.tag,
                .payloads = payloads,
            };
        }

        return try self.append(from, to, try self.provenanceForTransform(), .{ .tag_union = cases });
    }

    fn planNominal(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: checked_artifact.ExecutableNominalPayload,
        target: checked_artifact.ExecutableNominalPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        if (!nominalTypeKeyEql(source.nominal, target.nominal)) {
            compileTimeFinalizationInvariant("published erased-wrapper nominal transform changed nominal type");
        }
        return try self.append(from, to, try self.provenanceForTransform(), .{ .nominal = .{
            .nominal = target.nominal,
            .source_ty = target.source_ty,
            .backing = try self.plan(
                .{ .ty = source.backing, .key = source.backing_key },
                .{ .ty = target.backing, .key = target.backing_key },
            ),
        } });
    }

    fn planNominalToBacking(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: checked_artifact.ExecutableNominalPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        if (!repr.canonicalExecValueTypeKeyEql(source.backing_key, to.key)) {
            self.transformPayloadInvariant(self.payload(from), self.payload(to));
        }
        return try self.append(from, to, try self.provenanceForTransform(), .{ .structural_bridge = .nominal_reinterpret });
    }

    fn planBackingToNominal(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        target: checked_artifact.ExecutableNominalPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        if (!repr.canonicalExecValueTypeKeyEql(from.key, target.backing_key)) {
            self.transformPayloadInvariant(self.payload(from), self.payload(to));
        }
        return try self.append(from, to, try self.provenanceForTransform(), .{ .structural_bridge = .nominal_reinterpret });
    }

    fn planList(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: checked_artifact.ExecutableTypePayloadChild,
        target: checked_artifact.ExecutableTypePayloadChild,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        return try self.append(from, to, try self.provenanceForTransform(), .{ .list = .{
            .elem = try self.plan(
                .{ .ty = source.ty, .key = source.key },
                .{ .ty = target.ty, .key = target.key },
            ),
        } });
    }

    fn planBox(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: checked_artifact.ExecutableTypePayloadChild,
        target: checked_artifact.ExecutableTypePayloadChild,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        return try self.append(from, to, try self.provenanceForTransform(), .{ .box_payload = .{
            .boundary = self.localBoxBoundary(),
            .kind = .box_to_box,
            .payload = try self.plan(
                .{ .ty = source.ty, .key = source.key },
                .{ .ty = target.ty, .key = target.key },
            ),
        } });
    }

    fn planFiniteCallableToErased(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: checked_artifact.ExecutableCallableSetPayload,
        target: checked_artifact.ExecutableErasedFnPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        if (self.provenance.len == 0) {
            compileTimeFinalizationInvariant("published erased-wrapper finite callable erasure has no Box(T) provenance");
        }
        const adapter = canonical.ErasedAdapterKey{
            .source_fn_ty = target.sig_key.source_fn_ty,
            .callable_set_key = source.key,
            .erased_fn_sig_key = target.sig_key,
            .capture_shape_key = target.capture_shape_key,
        };
        return try self.append(from, to, try self.provenanceForTransform(), .{ .callable_to_erased = .{ .finite_value = .{
            .source_fn_ty = target.sig_key.source_fn_ty,
            .callable_set_key = source.key,
            .adapter_key = adapter,
        } } });
    }

    fn planAlreadyErasedCallable(
        self: *PublishedValueTransformPlanner,
        from: checked_artifact.ExecutableValueEndpoint,
        to: checked_artifact.ExecutableValueEndpoint,
        source: checked_artifact.ExecutableErasedFnPayload,
        target: checked_artifact.ExecutableErasedFnPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformPlanId {
        if (!repr.erasedFnSigKeyEql(source.sig_key, target.sig_key)) {
            compileTimeFinalizationInvariant("published erased-wrapper already-erased transform changed erased signature");
        }
        return try self.append(from, to, .none, .{ .already_erased_callable = .{ .sig_key = target.sig_key } });
    }

    fn localBoxBoundary(self: *PublishedValueTransformPlanner) ?canonical.BoxBoundaryId {
        for (self.provenance) |provenance| {
            switch (provenance) {
                .local_box_boundary => |boundary| return boundary,
                .promoted_wrapper => {},
            }
        }
        return null;
    }
};

fn sourceRecordFieldPayload(
    fields: []const checked_artifact.ExecutableRecordFieldPayload,
    label: canonical.RecordFieldLabelId,
) ?checked_artifact.ExecutableRecordFieldPayload {
    for (fields) |field| {
        if (field.field == label) return field;
    }
    return null;
}

fn sourceTupleElemPayload(
    elems: []const checked_artifact.ExecutableTupleElemPayload,
    index: u32,
) ?checked_artifact.ExecutableTupleElemPayload {
    for (elems) |elem| {
        if (elem.index == index) return elem;
    }
    return null;
}

fn targetTagVariantPayload(
    variants: []const checked_artifact.ExecutableTagVariantPayload,
    label: canonical.TagLabelId,
) ?checked_artifact.ExecutableTagVariantPayload {
    for (variants) |variant| {
        if (variant.tag == label) return variant;
    }
    return null;
}

fn sourceTagPayload(
    payloads: []const checked_artifact.ExecutableTagPayload,
    index: u32,
) ?checked_artifact.ExecutableTagPayload {
    for (payloads) |payload| {
        if (payload.index == index) return payload;
    }
    return null;
}

fn nominalTypeKeyEql(a: canonical.NominalTypeKey, b: canonical.NominalTypeKey) bool {
    return a.module_name == b.module_name and a.type_name == b.type_name;
}

fn buildErasedPromotedProcedureExecutableSignature(
    allocator: Allocator,
    reserved: checked_artifact.ReservedPromotedCallableWrapper,
    erased: checked_artifact.ErasedCallableResultPlan,
    params: []const checked_artifact.PromotedWrapperParam,
) Allocator.Error!checked_artifact.ErasedPromotedProcedureExecutableSignature {
    const payloads = erased.executable_signature_payloads;
    if (!std.meta.eql(payloads.source_fn_ty.bytes, erased.source_fn_ty.bytes)) {
        compileTimeFinalizationInvariant("erased callable signature payload source type differs from result plan");
    }
    if (payloads.param_exec_tys.len != params.len or payloads.param_exec_ty_keys.len != params.len) {
        compileTimeFinalizationInvariant("erased callable signature payload param arity differs from promoted wrapper params");
    }
    if (payloads.erased_call_args.len != payloads.erased_call_arg_keys.len) {
        compileTimeFinalizationInvariant("erased callable signature payload erased-call arg refs/keys differ in length");
    }

    const wrapper_params: []checked_artifact.ExecutableProcedureParamPayload = if (params.len == 0)
        &.{}
    else
        try allocator.alloc(checked_artifact.ExecutableProcedureParamPayload, params.len);
    errdefer if (wrapper_params.len > 0) allocator.free(wrapper_params);
    for (params, 0..) |param, i| {
        wrapper_params[i] = .{
            .param = param,
            .exec_ty = payloads.param_exec_tys[i],
            .exec_ty_key = payloads.param_exec_ty_keys[i],
        };
    }

    return .{
        .specialization_key = .{
            .base = reserved.proc_value.proc_base,
            .requested_fn_ty = erased.source_fn_ty,
            .exec_arg_tys = try allocator.dupe(canonical.CanonicalExecValueTypeKey, payloads.param_exec_ty_keys),
            .exec_ret_ty = payloads.wrapper_ret_key,
            .callable_repr_mode = .erased_callable,
            .capture_shape_key = payloads.capture_shape_key,
        },
        .source_fn_ty = erased.source_fn_ty,
        .wrapper_params = wrapper_params,
        .wrapper_ret = payloads.wrapper_ret,
        .wrapper_ret_key = payloads.wrapper_ret_key,
        .erased_call_args = try allocator.dupe(checked_artifact.ExecutableTypePayloadRef, payloads.erased_call_args),
        .erased_call_arg_keys = try allocator.dupe(canonical.CanonicalExecValueTypeKey, payloads.erased_call_arg_keys),
        .erased_call_ret = payloads.erased_call_ret,
        .erased_call_ret_key = payloads.erased_call_ret_key,
        .hidden_capture = payloads.hidden_capture,
    };
}

fn cloneBoxBoundarySpan(
    allocator: Allocator,
    provenance: []const checked_artifact.BoxErasureProvenance,
) Allocator.Error![]const checked_artifact.BoxErasureProvenance {
    if (provenance.len == 0) {
        compileTimeFinalizationInvariant("erased callable publication had no Box(T) provenance");
    }
    return try allocator.dupe(checked_artifact.BoxErasureProvenance, provenance);
}

fn promotedWrapperParamsForFnRoot(
    allocator: Allocator,
    artifact: *const checked_artifact.CheckedModuleArtifact,
    checked_fn_root: checked_artifact.CheckedTypeId,
) Allocator.Error![]const checked_artifact.PromotedWrapperParam {
    const payload = artifact.checked_types.payloads[@intFromEnum(checked_fn_root)];
    const function = switch (payload) {
        .function => |function| function,
        else => compileTimeFinalizationInvariant("promoted callable checked root was not a function"),
    };
    if (function.args.len == 0) return &.{};

    const params = try allocator.alloc(checked_artifact.PromotedWrapperParam, function.args.len);
    for (function.args, 0..) |arg, i| {
        params[i] = .{
            .index = @intCast(i),
            .checked_ty = arg,
            .source_ty = artifact.checked_types.roots[@intFromEnum(arg)].key,
        };
    }
    return params;
}

fn promotedWrapperCallArgs(
    allocator: Allocator,
    param_count: usize,
) Allocator.Error![]const checked_artifact.PromotedWrapperArg {
    if (param_count == 0) return &.{};
    const args = try allocator.alloc(checked_artifact.PromotedWrapperArg, param_count);
    for (args, 0..) |*arg, i| {
        arg.* = .{ .param = @intCast(i) };
    }
    return args;
}

fn captureSlotValue(
    layouts: *const layout_mod.Store,
    selected: SelectedFiniteCallableResult,
    slot_index: u32,
) PhysicalValue {
    const slot_count = selected.planned_member.capture_slots.len;
    const layout_idx = payloadLayoutForTagArg(layouts, selected.payload_layout, slot_count, slot_index);
    const layout = layouts.getLayout(layout_idx);
    const offset = payloadOffsetForTagArg(layouts, selected.payload_layout, slot_count, slot_index);
    return .{
        .layout_idx = layout_idx,
        .value = if (layouts.layoutSize(layout) == 0) Value.zst else selected.payload_value.offset(offset),
    };
}

const PrivateCaptureBuilder = struct {
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    layouts: *const layout_mod.Store,
    callable_set_descriptors: []const repr.CanonicalCallableSetDescriptor,
    owner: ?checked_artifact.PromotedProcedureRef,
    promotion_context: ?PromotedCallablePublicationContext,
    dependencies: ?*ConcreteDependencyCollector = null,
    next_private_const: u32 = 0,
    active: std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.PrivateCaptureNodeId),
    erased_active: std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.ErasedCaptureExecutableMaterializationNodeId),

    fn deinit(self: *PrivateCaptureBuilder) void {
        self.erased_active.deinit();
        self.active.deinit();
    }

    fn captureRef(
        self: *PrivateCaptureBuilder,
        source_ty: canonical.CanonicalTypeKey,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
        capture_index: u32,
    ) Allocator.Error!checked_artifact.PrivateCaptureRef {
        const checked_root = self.artifact.checked_types.rootForKey(source_ty) orelse {
            compileTimeFinalizationInvariant("private capture source type was not published in checked type store");
        };
        const source_scheme = try self.artifact.checked_types.ensureSchemeForRoot(self.allocator, checked_root);
        const owner = self.owner orelse compileTimeFinalizationInvariant("private capture ref construction requires a promoted procedure owner");
        if (@import("builtin").mode == .Debug) {
            self.verifyTopLevelCapturePlanSource(plan_id, source_ty);
        }
        return .{
            .artifact = self.artifact.key,
            .owner = .{
                .promoted_proc = owner,
                .capture_index = capture_index,
            },
            .node = try self.captureNode(plan_id, physical),
            .source_scheme = source_scheme,
        };
    }

    fn verifyTopLevelCapturePlanSource(
        self: *PrivateCaptureBuilder,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        source_ty: canonical.CanonicalTypeKey,
    ) void {
        const plan = self.artifact.comptime_plans.captureSlot(plan_id);
        switch (plan) {
            .serializable_leaf => |leaf| {
                if (!std.meta.eql(leaf.requested_source_ty.bytes, source_ty.bytes)) {
                    compileTimeFinalizationInvariant("private capture serializable leaf source type disagrees with descriptor capture slot");
                }
            },
            .callable_leaf => |result_plan| switch (self.artifact.comptime_plans.callableResult(result_plan)) {
                .finite => |finite| {
                    if (!std.meta.eql(finite.source_fn_ty.bytes, source_ty.bytes)) {
                        compileTimeFinalizationInvariant("private capture callable leaf source type disagrees with descriptor capture slot");
                    }
                },
                .erased => |erased| {
                    if (!std.meta.eql(erased.source_fn_ty.bytes, source_ty.bytes)) {
                        compileTimeFinalizationInvariant("private capture erased callable leaf source type disagrees with descriptor capture slot");
                    }
                },
            },
            .callable_schema => |schema| {
                if (!std.meta.eql(schema.bytes, source_ty.bytes)) {
                    compileTimeFinalizationInvariant("private capture callable schema source type disagrees with descriptor capture slot");
                }
            },
            .pending,
            .record,
            .tuple,
            .tag_union,
            .list,
            .box,
            .nominal,
            .recursive_ref,
            => {},
        }
    }

    fn captureNode(
        self: *PrivateCaptureBuilder,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.PrivateCaptureNodeId {
        if (self.active.get(plan_id)) |active| {
            return try self.artifact.comptime_plans.appendPrivateCapture(self.allocator, .{ .recursive_ref = active });
        }

        const node_id = try self.artifact.comptime_plans.reservePrivateCapture(self.allocator);
        try self.active.put(plan_id, node_id);
        errdefer _ = self.active.remove(plan_id);

        const node = try self.buildNode(plan_id, physical);
        self.artifact.comptime_plans.fillPrivateCapture(node_id, node);
        _ = self.active.remove(plan_id);
        return node_id;
    }

    fn buildNode(
        self: *PrivateCaptureBuilder,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.PrivateCaptureNode {
        const plan = self.artifact.comptime_plans.captureSlot(plan_id);
        return switch (plan) {
            .pending => compileTimeFinalizationInvariant("private capture reification reached pending capture plan"),
            .serializable_leaf => |leaf| .{ .const_instance_leaf = try self.serializableLeaf(leaf, physical) },
            .callable_leaf => |result_plan| .{ .finite_callable_leaf = try self.callableLeaf(result_plan, physical) },
            .callable_schema => compileTimeFinalizationInvariant("private capture reification reached callable schema without a value"),
            .record => |fields| .{ .record = try self.record(fields, physical) },
            .tuple => |items| .{ .tuple = try self.tuple(items, physical) },
            .tag_union => |variants| .{ .tag_union = try self.tagUnion(variants, physical) },
            .list => |list_plan| .{ .list = try self.list(list_plan.elem, physical) },
            .box => |payload| .{ .box = try self.box(payload, physical) },
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .backing = try self.captureNode(nominal.backing, physical),
            } },
            .recursive_ref => |target| .{ .recursive_ref = try self.captureNode(target, physical) },
        };
    }

    fn executablePlan(
        self: *PrivateCaptureBuilder,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        return .{ .node = try self.executableNode(plan_id, physical) };
    }

    fn executableNode(
        self: *PrivateCaptureBuilder,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationNodeId {
        if (self.erased_active.get(plan_id)) |active| {
            return try self.artifact.comptime_plans.appendErasedCaptureExecutableMaterializationNode(self.allocator, .{ .recursive_ref = active });
        }

        const node_id = try self.artifact.comptime_plans.reserveErasedCaptureExecutableMaterializationNode(self.allocator);
        try self.erased_active.put(plan_id, node_id);
        errdefer _ = self.erased_active.remove(plan_id);

        const node = try self.buildExecutableNode(plan_id, physical);
        self.artifact.comptime_plans.fillErasedCaptureExecutableMaterializationNode(node_id, node);
        _ = self.erased_active.remove(plan_id);
        return node_id;
    }

    fn buildExecutableNode(
        self: *PrivateCaptureBuilder,
        plan_id: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationNode {
        const plan = self.artifact.comptime_plans.captureSlot(plan_id);
        return switch (plan) {
            .pending => compileTimeFinalizationInvariant("erased capture executable materialization reached pending capture plan"),
            .serializable_leaf => |leaf| try self.executableSerializableLeaf(leaf, physical),
            .callable_leaf => |result_plan| try self.executableCallableLeaf(result_plan, physical),
            .callable_schema => compileTimeFinalizationInvariant("erased capture materialization reached callable schema without a value"),
            .record => |fields| .{ .record = try self.executableRecord(fields, physical) },
            .tuple => |items| .{ .tuple = try self.executableTuple(items, physical) },
            .tag_union => |variants| .{ .tag_union = try self.executableTagUnion(variants, physical) },
            .list => |list_plan| .{ .list = try self.executableList(list_plan.elem, physical) },
            .box => |payload| .{ .box = try self.executableBox(payload, physical) },
            .nominal => |nominal| .{ .nominal = .{
                .nominal = nominal.nominal,
                .backing = try self.executablePlan(nominal.backing, physical),
            } },
            .recursive_ref => |target| .{ .recursive_ref = try self.executableNode(target, physical) },
        };
    }

    fn executableCallableLeaf(
        self: *PrivateCaptureBuilder,
        result_plan: checked_artifact.CallableResultPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationNode {
        return switch (self.artifact.comptime_plans.callableResult(result_plan)) {
            .finite => .{ .finite_callable_set = try materializedFiniteCallableSetValue(
                self.allocator,
                self.lowered,
                self,
                result_plan,
                physical.layout_idx,
                physical.value,
            ) },
            .erased => |erased| .{ .erased_callable = try materializedErasedCallableValue(
                self,
                erased,
                physical.layout_idx,
                physical.value,
            ) },
        };
    }

    fn serializableLeaf(
        self: *PrivateCaptureBuilder,
        leaf: checked_artifact.SerializableCaptureLeafPlan,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.PrivateCaptureConstLeaf {
        const capture_index = self.next_private_const;
        self.next_private_const += 1;
        const owner = self.owner orelse compileTimeFinalizationInvariant("private serializable capture leaf requires a promoted procedure owner");

        const const_ref = try self.artifact.const_templates.reservePromotedCapture(
            self.allocator,
            self.artifact.key,
            owner,
            capture_index,
            leaf.source_scheme,
        );

        var leaf_dependencies = ConcreteDependencyCollector.init(self.allocator, self.artifact);
        defer leaf_dependencies.deinit();

        var reifier = ComptimeReifier{
            .allocator = self.allocator,
            .artifact = self.artifact,
            .values = &self.artifact.comptime_values,
            .plans = &self.artifact.comptime_plans,
            .checked_types = &self.artifact.checked_types,
            .layouts = self.layouts,
            .lowered = self.lowered,
            .callable_set_descriptors = self.callable_set_descriptors,
            .active_schemas = std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, checked_artifact.ComptimeSchemaId).init(self.allocator),
            .promotion_context = self.promotion_context,
            .dependencies = &leaf_dependencies,
        };
        defer reifier.deinit();
        const reified = try reifier.reifyPlan(leaf.reification_plan, physical.layout_idx, physical.value);

        self.artifact.const_templates.fillValueGraph(const_ref, .{
            .schema = reified.schema,
            .value = reified.value,
        });

        const requested_source_ty_payload = self.artifact.checked_types.rootForKey(leaf.requested_source_ty) orelse {
            compileTimeFinalizationInvariant("serializable private capture leaf requested type has no checked payload");
        };
        const instance_ref = try self.artifact.const_instances.reserveRequest(self.allocator, &self.artifact.checked_types, .{
            .key = .{
                .const_ref = const_ref,
                .requested_source_ty = leaf.requested_source_ty,
            },
            .requested_source_ty_payload = requested_source_ty_payload,
        });
        const instance_key = checked_artifact.ConstInstantiationKey{
            .const_ref = const_ref,
            .requested_source_ty = leaf.requested_source_ty,
        };
        self.artifact.const_instances.fill(instance_ref, .{
            .schema = reified.schema,
            .value = reified.value,
            .dependency_summary = try leaf_dependencies.appendSummary(),
            .reification_plan = leaf.reification_plan,
            .generated_procedures = try generatedProceduresForConstInstance(
                self.allocator,
                self.artifact,
                instance_key,
            ),
        });
        if (self.dependencies) |dependencies| try dependencies.appendConstInstance(instance_ref);

        return .{
            .const_ref = const_ref,
            .const_instance = instance_ref,
            .requested_source_ty = leaf.requested_source_ty,
            .schema = reified.schema,
            .mode = if (try constGraphContainsCallableSlots(
                self.allocator,
                &self.artifact.comptime_plans,
                leaf.reification_plan,
            ))
                .general_may_contain_callable_slots
            else
                .pure_no_callable_slots,
        };
    }

    fn executableSerializableLeaf(
        self: *PrivateCaptureBuilder,
        leaf: checked_artifact.SerializableCaptureLeafPlan,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationNode {
        var leaf_dependencies = ConcreteDependencyCollector.init(self.allocator, self.artifact);
        defer leaf_dependencies.deinit();

        var reifier = ComptimeReifier{
            .allocator = self.allocator,
            .artifact = self.artifact,
            .values = &self.artifact.comptime_values,
            .plans = &self.artifact.comptime_plans,
            .checked_types = &self.artifact.checked_types,
            .layouts = self.layouts,
            .lowered = self.lowered,
            .callable_set_descriptors = self.callable_set_descriptors,
            .active_schemas = std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, checked_artifact.ComptimeSchemaId).init(self.allocator),
            .promotion_context = self.promotion_context,
            .dependencies = &leaf_dependencies,
        };
        defer reifier.deinit();
        const reified = try reifier.reifyPlan(leaf.reification_plan, physical.layout_idx, physical.value);
        if (try constGraphContainsCallableSlots(self.allocator, &self.artifact.comptime_plans, leaf.reification_plan)) {
            const capture_index = self.next_private_const;
            self.next_private_const += 1;
            const owner = self.owner orelse compileTimeFinalizationInvariant("callable-containing executable capture const leaf requires a promoted procedure owner");
            const const_ref = try self.artifact.const_templates.reservePromotedCapture(
                self.allocator,
                self.artifact.key,
                owner,
                capture_index,
                leaf.source_scheme,
            );
            self.artifact.const_templates.fillValueGraph(const_ref, .{
                .schema = reified.schema,
                .value = reified.value,
            });
            const requested_source_ty_payload = self.artifact.checked_types.rootForKey(leaf.requested_source_ty) orelse {
                compileTimeFinalizationInvariant("executable private capture leaf requested type has no checked payload");
            };
            const instance_ref = try self.artifact.const_instances.reserveRequest(self.allocator, &self.artifact.checked_types, .{
                .key = .{
                    .const_ref = const_ref,
                    .requested_source_ty = leaf.requested_source_ty,
                },
                .requested_source_ty_payload = requested_source_ty_payload,
            });
            const instance_key = checked_artifact.ConstInstantiationKey{
                .const_ref = const_ref,
                .requested_source_ty = leaf.requested_source_ty,
            };
            self.artifact.const_instances.fill(instance_ref, .{
                .schema = reified.schema,
                .value = reified.value,
                .dependency_summary = try leaf_dependencies.appendSummary(),
                .reification_plan = leaf.reification_plan,
                .generated_procedures = try generatedProceduresForConstInstance(
                    self.allocator,
                    self.artifact,
                    instance_key,
                ),
            });
            if (self.dependencies) |dependencies| try dependencies.appendConstInstance(instance_ref);
            return .{ .const_instance = instance_ref };
        }
        return .{ .pure_value = .{
            .schema = reified.schema,
            .value = reified.value,
            .no_reachable_callable_slots = .checked_artifact_verified,
        } };
    }

    fn callableLeaf(
        self: *PrivateCaptureBuilder,
        result_plan: checked_artifact.CallableResultPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.FiniteCallableLeafInstance {
        const context = self.promotion_context orelse compileTimeFinalizationInvariant("captured callable leaf promotion requires explicit promoted procedure provenance");
        const published = switch (self.artifact.comptime_plans.callableResult(result_plan)) {
            .finite => blk: {
                const selected = selectFiniteCallableResult(
                    &self.artifact.comptime_plans,
                    self.callable_set_descriptors,
                    self.layouts,
                    result_plan,
                    physical.layout_idx,
                    physical.value,
                );
                const checked_fn_root = self.artifact.checked_types.rootForKey(selected.result_plan.source_fn_ty) orelse {
                    compileTimeFinalizationInvariant("captured finite callable leaf source function type was not published in checked type store");
                };
                break :blk try publishCallableResult(
                    self.allocator,
                    self.artifact,
                    self.lowered,
                    context,
                    checked_fn_root,
                    result_plan,
                    selected,
                );
            },
            .erased => |erased| blk: {
                const checked_fn_root = self.artifact.checked_types.rootForKey(erased.source_fn_ty) orelse {
                    compileTimeFinalizationInvariant("captured erased callable leaf source function type was not published in checked type store");
                };
                break :blk try publishErasedCallableResult(
                    self.allocator,
                    self.artifact,
                    self.lowered,
                    context,
                    checked_fn_root,
                    physical.layout_idx,
                    physical.value,
                    result_plan,
                    erased,
                );
            },
        };
        if (self.dependencies) |dependencies| try dependencies.appendProcedureCallable(published.proc_value);
        return .{ .proc_value = published.proc_value };
    }

    fn record(
        self: *PrivateCaptureBuilder,
        fields: []const checked_artifact.CaptureRecordFieldPlan,
        physical: PhysicalValue,
    ) Allocator.Error![]const checked_artifact.PrivateCaptureRecordField {
        if (fields.len == 0) return &.{};
        const aggregate = self.logicalAggregateValue(physical, .struct_);
        const layout = self.layouts.getLayout(aggregate.layout_idx);
        if (layout.tag != .struct_) compileTimeFinalizationInvariant("private record capture did not lower to struct layout");

        const out = try self.allocator.alloc(checked_artifact.PrivateCaptureRecordField, fields.len);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .field = field.field,
                .value = try self.captureNode(field.value, structFieldValue(self.layouts, layout, aggregate.value, @intCast(i))),
            };
        }
        return out;
    }

    fn tuple(
        self: *PrivateCaptureBuilder,
        items: []const checked_artifact.CaptureTupleElemPlan,
        physical: PhysicalValue,
    ) Allocator.Error![]const checked_artifact.PrivateCaptureNodeId {
        if (items.len == 0) return &.{};
        const aggregate = self.logicalAggregateValue(physical, .struct_);
        const layout = self.layouts.getLayout(aggregate.layout_idx);
        if (layout.tag != .struct_) compileTimeFinalizationInvariant("private tuple capture did not lower to struct layout");

        const out = try self.allocator.alloc(checked_artifact.PrivateCaptureNodeId, items.len);
        for (items, 0..) |item, i| {
            if (item.index != @as(u32, @intCast(i))) {
                compileTimeFinalizationInvariant("private tuple capture plan indices are not canonical");
            }
            out[i] = try self.captureNode(item.value, structFieldValue(self.layouts, layout, aggregate.value, @intCast(i)));
        }
        return out;
    }

    fn tagUnion(
        self: *PrivateCaptureBuilder,
        variants: []const checked_artifact.CaptureTagVariantPlan,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.PrivateCaptureTagNode {
        const aggregate = self.logicalAggregateValue(physical, .tag_union);
        const layout = self.layouts.getLayout(aggregate.layout_idx);
        if (layout.tag != .tag_union) compileTimeFinalizationInvariant("private tag capture did not lower to tag-union layout");
        const info = self.layouts.getTagUnionInfo(layout);
        const discriminant = info.data.readDiscriminant(aggregate.value.ptr);
        if (discriminant >= variants.len) {
            compileTimeFinalizationInvariant("private tag capture discriminant exceeded capture plan variants");
        }

        const active = variants[discriminant];
        const active_payload_layout = info.variants.get(@intCast(discriminant)).payload_layout;
        const payloads = try self.allocator.alloc(checked_artifact.PrivateCaptureTagPayload, active.payloads.len);
        for (active.payloads, 0..) |payload, i| {
            payloads[i] = .{
                .index = payload.index,
                .value = try self.captureNode(
                    payload.value,
                    tagPayloadValue(self.layouts, active_payload_layout, aggregate.value, active.payloads.len, @intCast(i)),
                ),
            };
        }
        return .{
            .tag = active.tag,
            .payloads = payloads,
        };
    }

    fn list(
        self: *PrivateCaptureBuilder,
        elem_plan: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error![]const checked_artifact.PrivateCaptureNodeId {
        const layout = self.layouts.getLayout(physical.layout_idx);
        const elem_layout_idx = switch (layout.tag) {
            .list => layout.data.list,
            .list_of_zst => layout_mod.Idx.zst,
            .zst => return &.{},
            else => compileTimeFinalizationInvariant("private List(T) capture did not lower to list layout"),
        };
        const elem_layout = self.layouts.getLayout(elem_layout_idx);
        const elem_size: usize = @intCast(self.layouts.layoutSize(elem_layout));
        const roc_list: *const RocList = @ptrCast(@alignCast(physical.value.ptr));
        if (roc_list.len() == 0) return &.{};

        const out = try self.allocator.alloc(checked_artifact.PrivateCaptureNodeId, roc_list.len());
        var i: usize = 0;
        while (i < roc_list.len()) : (i += 1) {
            const elem_value = if (elem_size == 0)
                Value.zst
            else
                Value{ .ptr = (roc_list.bytes orelse compileTimeFinalizationInvariant("non-empty private list capture had null bytes")) + i * elem_size };
            out[i] = try self.captureNode(elem_plan, .{
                .layout_idx = elem_layout_idx,
                .value = elem_value,
            });
        }
        return out;
    }

    fn box(
        self: *PrivateCaptureBuilder,
        payload_plan: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.PrivateCaptureNodeId {
        const layout = self.layouts.getLayout(physical.layout_idx);
        const payload = switch (layout.tag) {
            .box => PhysicalValue{
                .layout_idx = layout.data.box,
                .value = .{ .ptr = physical.value.read(?[*]u8) orelse compileTimeFinalizationInvariant("private Box(T) capture had null payload") },
            },
            .box_of_zst => PhysicalValue{ .layout_idx = .zst, .value = Value.zst },
            else => compileTimeFinalizationInvariant("private Box(T) capture did not lower to box layout"),
        };
        return try self.captureNode(payload_plan, payload);
    }

    fn executableRecord(
        self: *PrivateCaptureBuilder,
        fields: []const checked_artifact.CaptureRecordFieldPlan,
        physical: PhysicalValue,
    ) Allocator.Error![]const checked_artifact.ErasedCaptureExecutableMaterializationRecordField {
        if (fields.len == 0) return &.{};
        const aggregate = self.logicalAggregateValue(physical, .struct_);
        const layout = self.layouts.getLayout(aggregate.layout_idx);
        if (layout.tag != .struct_) compileTimeFinalizationInvariant("erased capture record materialization did not lower to struct layout");

        const out = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationRecordField, fields.len);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .field = field.field,
                .value = try self.executablePlan(field.value, structFieldValue(self.layouts, layout, aggregate.value, @intCast(i))),
            };
        }
        return out;
    }

    fn executableTuple(
        self: *PrivateCaptureBuilder,
        items: []const checked_artifact.CaptureTupleElemPlan,
        physical: PhysicalValue,
    ) Allocator.Error![]const checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        if (items.len == 0) return &.{};
        const aggregate = self.logicalAggregateValue(physical, .struct_);
        const layout = self.layouts.getLayout(aggregate.layout_idx);
        if (layout.tag != .struct_) compileTimeFinalizationInvariant("erased capture tuple materialization did not lower to struct layout");

        const out = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationPlan, items.len);
        for (items, 0..) |item, i| {
            if (item.index != @as(u32, @intCast(i))) {
                compileTimeFinalizationInvariant("erased capture tuple materialization plan indices are not canonical");
            }
            out[i] = try self.executablePlan(item.value, structFieldValue(self.layouts, layout, aggregate.value, @intCast(i)));
        }
        return out;
    }

    fn executableTagUnion(
        self: *PrivateCaptureBuilder,
        variants: []const checked_artifact.CaptureTagVariantPlan,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationTagNode {
        const aggregate = self.logicalAggregateValue(physical, .tag_union);
        const layout = self.layouts.getLayout(aggregate.layout_idx);
        if (layout.tag != .tag_union) compileTimeFinalizationInvariant("erased capture tag materialization did not lower to tag-union layout");
        const info = self.layouts.getTagUnionInfo(layout);
        const discriminant = info.data.readDiscriminant(aggregate.value.ptr);
        if (discriminant >= variants.len) {
            compileTimeFinalizationInvariant("erased capture tag materialization discriminant exceeded capture plan variants");
        }

        const active = variants[discriminant];
        const active_payload_layout = info.variants.get(@intCast(discriminant)).payload_layout;
        const payloads = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationTagPayload, active.payloads.len);
        for (active.payloads, 0..) |payload, i| {
            payloads[i] = .{
                .index = payload.index,
                .value = try self.executablePlan(
                    payload.value,
                    tagPayloadValue(self.layouts, active_payload_layout, aggregate.value, active.payloads.len, @intCast(i)),
                ),
            };
        }
        return .{
            .tag = active.tag,
            .payloads = payloads,
        };
    }

    fn executableList(
        self: *PrivateCaptureBuilder,
        elem_plan: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error![]const checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        const layout = self.layouts.getLayout(physical.layout_idx);
        const elem_layout_idx = switch (layout.tag) {
            .list => layout.data.list,
            .list_of_zst => layout_mod.Idx.zst,
            .zst => return &.{},
            else => compileTimeFinalizationInvariant("erased capture List(T) materialization did not lower to list layout"),
        };
        const elem_layout = self.layouts.getLayout(elem_layout_idx);
        const elem_size: usize = @intCast(self.layouts.layoutSize(elem_layout));
        const roc_list: *const RocList = @ptrCast(@alignCast(physical.value.ptr));
        if (roc_list.len() == 0) return &.{};

        const out = try self.allocator.alloc(checked_artifact.ErasedCaptureExecutableMaterializationPlan, roc_list.len());
        var i: usize = 0;
        while (i < roc_list.len()) : (i += 1) {
            const elem_value = if (elem_size == 0)
                Value.zst
            else
                Value{ .ptr = (roc_list.bytes orelse compileTimeFinalizationInvariant("non-empty erased capture list had null bytes")) + i * elem_size };
            out[i] = try self.executablePlan(elem_plan, .{
                .layout_idx = elem_layout_idx,
                .value = elem_value,
            });
        }
        return out;
    }

    fn executableBox(
        self: *PrivateCaptureBuilder,
        payload_plan: checked_artifact.CaptureSlotReificationPlanId,
        physical: PhysicalValue,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        const layout = self.layouts.getLayout(physical.layout_idx);
        const payload = switch (layout.tag) {
            .box => PhysicalValue{
                .layout_idx = layout.data.box,
                .value = .{ .ptr = physical.value.read(?[*]u8) orelse compileTimeFinalizationInvariant("erased capture Box(T) materialization had null payload") },
            },
            .box_of_zst => PhysicalValue{ .layout_idx = .zst, .value = Value.zst },
            else => compileTimeFinalizationInvariant("erased capture Box(T) materialization did not lower to box layout"),
        };
        return try self.executablePlan(payload_plan, payload);
    }

    fn logicalAggregateValue(
        self: *const PrivateCaptureBuilder,
        physical: PhysicalValue,
        expected_tag: layout_mod.LayoutTag,
    ) PhysicalValue {
        const layout = self.layouts.getLayout(physical.layout_idx);
        switch (layout.tag) {
            .box => {
                const payload = physical.value.read(?[*]u8) orelse compileTimeFinalizationInvariant("private capture aggregate used boxed layout with null payload");
                const inner_layout = self.layouts.getLayout(layout.data.box);
                if (inner_layout.tag != expected_tag) {
                    compileTimeFinalizationInvariant("private capture boxed aggregate did not contain expected layout");
                }
                return .{ .layout_idx = layout.data.box, .value = .{ .ptr = payload } };
            },
            .box_of_zst => {
                if (expected_tag != .zst) {
                    compileTimeFinalizationInvariant("private capture used box_of_zst for non-ZST aggregate");
                }
                return .{ .layout_idx = .zst, .value = Value.zst };
            },
            else => return physical,
        }
    }
};

fn structFieldValue(
    layouts: *const layout_mod.Store,
    struct_layout: layout_mod.Layout,
    value: Value,
    field_index: u32,
) PhysicalValue {
    if (struct_layout.tag != .struct_) {
        compileTimeFinalizationInvariant("private capture field read expected struct layout");
    }
    const field_layout_idx = layouts.getStructFieldLayoutByOriginalIndex(struct_layout.data.struct_.idx, field_index);
    const field_layout = layouts.getLayout(field_layout_idx);
    const offset = layouts.getStructFieldOffsetByOriginalIndex(struct_layout.data.struct_.idx, field_index);
    return .{
        .layout_idx = field_layout_idx,
        .value = if (layouts.layoutSize(field_layout) == 0) Value.zst else value.offset(offset),
    };
}

fn tagPayloadValue(
    layouts: *const layout_mod.Store,
    variant_layout_idx: layout_mod.Idx,
    value: Value,
    payload_count: usize,
    payload_index: u32,
) PhysicalValue {
    const payload_layout_idx = payloadLayoutForTagArg(layouts, variant_layout_idx, payload_count, payload_index);
    const payload_layout = layouts.getLayout(payload_layout_idx);
    const offset = payloadOffsetForTagArg(layouts, variant_layout_idx, payload_count, payload_index);
    return .{
        .layout_idx = payload_layout_idx,
        .value = if (layouts.layoutSize(payload_layout) == 0) Value.zst else value.offset(offset),
    };
}

const SelectedFiniteCallableSetMember = struct {
    member: check.CanonicalNames.CallableSetMemberId,
    payload_layout: layout_mod.Idx,
};

fn selectFiniteCallableSetMember(
    layouts: *const layout_mod.Store,
    layout_idx: layout_mod.Idx,
    layout: layout_mod.Layout,
    value: Value,
    members: []const checked_artifact.CallableResultMemberPlan,
) SelectedFiniteCallableSetMember {
    if (layout.tag == .tag_union) {
        const info = layouts.getTagUnionInfo(layout);
        const discriminant = info.data.readDiscriminant(value.ptr);
        if (discriminant >= members.len) {
            compileTimeFinalizationInvariant("finite compile-time callable result discriminant exceeded member count");
        }
        return .{
            .member = @enumFromInt(discriminant),
            .payload_layout = info.variants.get(@intCast(discriminant)).payload_layout,
        };
    }

    if (members.len != 1) {
        compileTimeFinalizationInvariant("multi-member finite compile-time callable result did not lower to tag-union layout");
    }
    return .{
        .member = members[0].member,
        .payload_layout = layout_idx,
    };
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

fn runtimeCallableSetDescriptor(
    descriptors: []const repr.CanonicalCallableSetDescriptor,
    key: check.CanonicalNames.CanonicalCallableSetKey,
) ?*const repr.CanonicalCallableSetDescriptor {
    for (descriptors) |*descriptor| {
        if (mir.LambdaSolved.Representation.callableSetKeyEql(descriptor.key, key)) return descriptor;
    }
    return null;
}

fn runtimeCallableSetMember(
    descriptor: *const repr.CanonicalCallableSetDescriptor,
    member_id: check.CanonicalNames.CallableSetMemberId,
) ?*const repr.CanonicalCallableSetMember {
    for (descriptor.members) |*member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn persistedCallableSetDescriptor(
    descriptors: []const check.CanonicalNames.CanonicalCallableSetDescriptor,
    key: check.CanonicalNames.CanonicalCallableSetKey,
) ?*const check.CanonicalNames.CanonicalCallableSetDescriptor {
    for (descriptors) |*descriptor| {
        if (mir.LambdaSolved.Representation.callableSetKeyEql(descriptor.key, key)) return descriptor;
    }
    return null;
}

fn persistedCallableSetMember(
    descriptor: *const check.CanonicalNames.CanonicalCallableSetDescriptor,
    member_id: check.CanonicalNames.CallableSetMemberId,
) ?*const check.CanonicalNames.CanonicalCallableSetMember {
    for (descriptor.members) |*member| {
        if (member.member == member_id) return member;
    }
    return null;
}

fn comptimeValuePathKeyForCallableResult(
    result_plan: checked_artifact.CallableResultPlanId,
) checked_artifact.ComptimeValuePathKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashPathTag(&hasher, "comptime-value-callable-result");
    hashPathU32(&hasher, @intFromEnum(result_plan));
    return .{ .bytes = hasher.finalResult() };
}

fn promotedCallablePathKeyForCallableResult(
    result_plan: checked_artifact.CallableResultPlanId,
) checked_artifact.PromotedCallablePathKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashPathTag(&hasher, "promoted-callable-result");
    hashPathU32(&hasher, @intFromEnum(result_plan));
    return .{ .bytes = hasher.finalResult() };
}

fn privateCapturePathKeyForCallableResult(
    result_plan: checked_artifact.CallableResultPlanId,
) checked_artifact.PrivateCapturePathKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hashPathTag(&hasher, "private-capture-callable-result");
    hashPathU32(&hasher, @intFromEnum(result_plan));
    return .{ .bytes = hasher.finalResult() };
}

fn hashPathTag(hasher: *std.crypto.hash.sha2.Sha256, tag: []const u8) void {
    hashPathU32(hasher, @intCast(tag.len));
    hasher.update(tag);
}

fn hashPathU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    bytes = .{
        @as(u8, @truncate(value)),
        @as(u8, @truncate(value >> 8)),
        @as(u8, @truncate(value >> 16)),
        @as(u8, @truncate(value >> 24)),
    };
    hasher.update(&bytes);
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
    artifact: ?*checked_artifact.CheckedModuleArtifact = null,
    values: *checked_artifact.CompileTimeValueStore,
    plans: *const checked_artifact.CompileTimePlanStore,
    checked_types: *const checked_artifact.CheckedTypeStore,
    layouts: *const layout_mod.Store,
    lowered: ?*const lir.CheckedPipeline.LoweredProgram = null,
    callable_set_descriptors: []const repr.CanonicalCallableSetDescriptor,
    active_schemas: std.AutoHashMap(checked_artifact.ConstGraphReificationPlanId, checked_artifact.ComptimeSchemaId),
    promotion_context: ?PromotedCallablePublicationContext = null,
    dependencies: ?*ConcreteDependencyCollector = null,

    fn deinit(self: *ComptimeReifier) void {
        self.active_schemas.deinit();
    }

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
            .callable_schema => reifierInvariant("compile-time reification reached function schema without a callable value"),
            .recursive_ref => |ref| self.reifyPlan(ref, layout_idx, value),
        };
    }

    fn schemaForPlan(
        self: *ComptimeReifier,
        plan_id: checked_artifact.ConstGraphReificationPlanId,
    ) Allocator.Error!checked_artifact.ComptimeSchemaId {
        const plan = self.plans.constGraph(plan_id);
        if (plan == .recursive_ref) {
            return self.schemaForPlan(plan.recursive_ref);
        }

        if (self.active_schemas.get(plan_id)) |active| return active;
        const schema_id = try self.values.addSchema(.pending);
        try self.active_schemas.put(plan_id, schema_id);
        errdefer _ = self.active_schemas.remove(plan_id);

        const schema = try self.schemaForPlanPayload(plan);
        self.values.overwriteSchema(schema_id, schema);
        _ = self.active_schemas.remove(plan_id);
        return schema_id;
    }

    fn schemaForPlanPayload(
        self: *ComptimeReifier,
        plan: checked_artifact.ConstGraphReificationPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchema {
        return switch (plan) {
            .pending => reifierInvariant("compile-time schema construction reached pending const graph plan"),
            .scalar => |checked_ty| self.schemaForScalarCheckedType(checked_ty),
            .string => .str,
            .list => |list| .{ .list = try self.schemaForPlan(list.elem) },
            .box => |box| .{ .box = try self.schemaForPlan(box.payload) },
            .tuple => |items| try self.schemaForTuplePlan(items),
            .record => |fields| try self.schemaForRecordPlan(fields),
            .tag_union => |variants| try self.schemaForTagUnionPlan(variants),
            .transparent_alias => |alias| .{ .alias = .{
                .type_name = alias.alias,
                .backing = try self.schemaForPlan(alias.backing),
            } },
            .nominal => |nominal| .{ .nominal = .{
                .type_name = nominal.nominal,
                .backing = try self.schemaForPlan(nominal.backing),
                .is_opaque = false,
            } },
            .callable_leaf => |leaf| self.schemaForCallableLeaf(leaf),
            .callable_schema => |source_fn_ty| .{ .callable = source_fn_ty },
            .recursive_ref => reifierInvariant("compile-time schema payload construction reached recursive ref"),
        };
    }

    fn schemaForScalarCheckedType(
        self: *ComptimeReifier,
        checked_ty: checked_artifact.CheckedTypeId,
    ) checked_artifact.ComptimeSchema {
        const nominal = switch (self.checkedPayload(checked_ty)) {
            .nominal => |nominal| nominal,
            else => reifierInvariant("scalar const graph plan did not reference a nominal scalar type"),
        };
        const builtin_nominal = nominal.builtin orelse reifierInvariant("scalar const graph plan referenced non-builtin nominal type");
        return switch (builtin_nominal) {
            .u8 => .{ .int = .u8 },
            .i8 => .{ .int = .i8 },
            .u16 => .{ .int = .u16 },
            .i16 => .{ .int = .i16 },
            .u32 => .{ .int = .u32 },
            .i32 => .{ .int = .i32 },
            .u64 => .{ .int = .u64 },
            .i64 => .{ .int = .i64 },
            .u128 => .{ .int = .u128 },
            .i128 => .{ .int = .i128 },
            .f32 => .{ .frac = .f32 },
            .f64 => .{ .frac = .f64 },
            .dec => .{ .frac = .dec },
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
            .already_resolved => |resolved| blk: {
                if (self.dependencies) |dependencies| try dependencies.appendCallableLeafInstance(resolved);
                break :blk .{
                    .schema = try self.values.addSchema(.{ .callable = callableLeafSourceFnTy(resolved) }),
                    .value = try self.values.addValue(.{ .callable = resolved }),
                };
            },
            .finite => |result_plan| blk: {
                const selected_callable = selectFiniteCallableResult(
                    self.plans,
                    self.callable_set_descriptors,
                    self.layouts,
                    result_plan,
                    layout_idx,
                    value,
                );
                const callable_leaf = try self.callableLeafInstance(result_plan, selected_callable);
                if (self.dependencies) |dependencies| try dependencies.appendCallableLeafInstance(callable_leaf);
                break :blk .{
                    .schema = try self.values.addSchema(.{ .callable = callableLeafSourceFnTy(callable_leaf) }),
                    .value = try self.values.addValue(.{ .callable = callable_leaf }),
                };
            },
            .erased_boxed => |result_plan| blk: {
                const erased = switch (self.plans.callableResult(result_plan)) {
                    .finite => reifierInvariant("erased boxed callable leaf referenced a finite callable result plan"),
                    .erased => |erased| erased,
                };
                const artifact = self.artifact orelse reifierInvariant("erased boxed callable leaf reification requires mutable checked artifact");
                const lowered = self.lowered orelse reifierInvariant("erased boxed callable leaf reification requires lowered LIR context");
                const code = resolveErasedCallableResultCode(
                    artifact,
                    lowered,
                    erased,
                    layout_idx,
                    value,
                );
                if (self.dependencies) |dependencies| try dependencies.collectErasedCodeRef(code);
                const callable_leaf = checked_artifact.CallableLeafInstance{ .erased_boxed = .{
                    .source_fn_ty = erased.source_fn_ty,
                    .sig_key = erased.sig_key,
                    .provenance = try cloneBoxBoundarySpan(self.allocator, erased.provenance),
                    .code = code,
                    .capture = try self.materializeErasedCallableLeafCapture(erased, layout_idx, value),
                } };
                break :blk .{
                    .schema = try self.values.addSchema(.{ .callable = erased.source_fn_ty }),
                    .value = try self.values.addValue(.{ .callable = callable_leaf }),
                };
            },
        };
    }

    fn materializeErasedCallableLeafCapture(
        self: *ComptimeReifier,
        erased: checked_artifact.ErasedCallableResultPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!checked_artifact.ErasedCaptureExecutableMaterializationPlan {
        const artifact = self.artifact orelse reifierInvariant("erased boxed callable leaf reification requires mutable checked artifact");
        const lowered = self.lowered orelse reifierInvariant("erased boxed callable leaf reification requires lowered LIR context");
        var capture_builder = PrivateCaptureBuilder{
            .allocator = self.allocator,
            .artifact = artifact,
            .lowered = lowered,
            .layouts = self.layouts,
            .callable_set_descriptors = self.callable_set_descriptors,
            .owner = null,
            .promotion_context = self.promotion_context,
            .dependencies = self.dependencies,
            .active = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.PrivateCaptureNodeId).init(self.allocator),
            .erased_active = std.AutoHashMap(checked_artifact.CaptureSlotReificationPlanId, checked_artifact.ErasedCaptureExecutableMaterializationNodeId).init(self.allocator),
        };
        defer capture_builder.deinit();
        return try materializeErasedPromotedCapture(
            self.allocator,
            artifact,
            lowered,
            &capture_builder,
            erased,
            layout_idx,
            value,
        );
    }

    fn schemaForCallableLeaf(
        self: *ComptimeReifier,
        leaf: checked_artifact.CallableLeafReificationPlan,
    ) checked_artifact.ComptimeSchema {
        return switch (leaf) {
            .already_resolved => |resolved| .{ .callable = callableLeafSourceFnTy(resolved) },
            .finite => |result_plan| switch (self.plans.callableResult(result_plan)) {
                .finite => |finite| .{ .callable = finite.source_fn_ty },
                .erased => |erased| .{ .callable = erased.source_fn_ty },
            },
            .erased_boxed => |result_plan| switch (self.plans.callableResult(result_plan)) {
                .finite => reifierInvariant("erased boxed callable leaf referenced a finite callable result plan"),
                .erased => |erased| .{ .callable = erased.source_fn_ty },
            },
        };
    }

    fn callableLeafInstance(
        self: *ComptimeReifier,
        result_plan: checked_artifact.CallableResultPlanId,
        selected: SelectedFiniteCallableResult,
    ) Allocator.Error!checked_artifact.CallableLeafInstance {
        if (!selectedFiniteCallableRequiresPromotion(selected)) {
            return .{ .finite = .{ .proc_value = closedFiniteCallableLeafFromSelectedCallableResult(selected) } };
        }

        const artifact = self.artifact orelse reifierInvariant("captured callable leaf reification requires mutable checked artifact");
        const lowered = self.lowered orelse reifierInvariant("captured callable leaf reification requires lowered LIR context");
        const context = self.promotion_context orelse reifierInvariant("captured callable leaf reification requires explicit promoted procedure provenance");
        const checked_fn_root = artifact.checked_types.rootForKey(selected.result_plan.source_fn_ty) orelse {
            reifierInvariant("captured callable leaf source function type was not published in checked type store");
        };
        const published = try publishCallableResult(
            self.allocator,
            artifact,
            lowered,
            context,
            checked_fn_root,
            result_plan,
            selected,
        );
        return .{ .finite = .{ .proc_value = published.proc_value } };
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
        if (layout.tag == .zst) return try self.reifyZstRecordPlan(fields);
        if (layout.tag != .struct_ and fields.len == 1) return try self.reifySingleFieldRecordPlan(fields[0], physical.layout_idx, physical.value);
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

    fn reifyZstRecordPlan(
        self: *ComptimeReifier,
        fields: []const checked_artifact.ConstRecordFieldPlan,
    ) Allocator.Error!ReifiedValue {
        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        const value_fields = try self.allocator.alloc(checked_artifact.ComptimeValueId, fields.len);
        errdefer self.allocator.free(value_fields);

        for (fields, 0..) |field, i| {
            const reified = try self.reifyPlan(field.value, .zst, Value.zst);
            schema_fields[i] = .{ .name = field.field, .schema = reified.schema };
            value_fields[i] = reified.value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .record = schema_fields }),
            .value = try self.values.addValue(.{ .record = value_fields }),
        };
    }

    fn reifySingleFieldRecordPlan(
        self: *ComptimeReifier,
        field: checked_artifact.ConstRecordFieldPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const reified = try self.reifyPlan(field.value, layout_idx, value);

        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, 1);
        errdefer self.allocator.free(schema_fields);
        const value_fields = try self.allocator.alloc(checked_artifact.ComptimeValueId, 1);
        errdefer self.allocator.free(value_fields);
        schema_fields[0] = .{ .name = field.field, .schema = reified.schema };
        value_fields[0] = reified.value;

        return .{
            .schema = try self.values.addSchema(.{ .record = schema_fields }),
            .value = try self.values.addValue(.{ .record = value_fields }),
        };
    }

    fn schemaForRecordPlan(
        self: *ComptimeReifier,
        fields: []const checked_artifact.ConstRecordFieldPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchema {
        if (fields.len == 0) return .zst;

        const schema_fields = try self.allocator.alloc(checked_artifact.ComptimeFieldSchema, fields.len);
        errdefer self.allocator.free(schema_fields);
        for (fields, 0..) |field, i| {
            schema_fields[i] = .{
                .name = field.field,
                .schema = try self.schemaForPlan(field.value),
            };
        }
        return .{ .record = schema_fields };
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
        if (layout.tag == .zst) return try self.reifyZstTuplePlan(items);
        if (layout.tag != .struct_ and items.len == 1) return try self.reifySingleElemTuplePlan(items[0], physical.layout_idx, physical.value);
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

    fn reifyZstTuplePlan(
        self: *ComptimeReifier,
        items: []const checked_artifact.ConstTupleElemPlan,
    ) Allocator.Error!ReifiedValue {
        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        const values = try self.allocator.alloc(checked_artifact.ComptimeValueId, items.len);
        errdefer self.allocator.free(values);

        for (items, 0..) |item, i| {
            const reified = try self.reifyPlan(item.value, .zst, Value.zst);
            schemas[i] = reified.schema;
            values[i] = reified.value;
        }

        return .{
            .schema = try self.values.addSchema(.{ .tuple = schemas }),
            .value = try self.values.addValue(.{ .tuple = values }),
        };
    }

    fn reifySingleElemTuplePlan(
        self: *ComptimeReifier,
        item: checked_artifact.ConstTupleElemPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const reified = try self.reifyPlan(item.value, layout_idx, value);

        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, 1);
        errdefer self.allocator.free(schemas);
        const values = try self.allocator.alloc(checked_artifact.ComptimeValueId, 1);
        errdefer self.allocator.free(values);
        schemas[0] = reified.schema;
        values[0] = reified.value;

        return .{
            .schema = try self.values.addSchema(.{ .tuple = schemas }),
            .value = try self.values.addValue(.{ .tuple = values }),
        };
    }

    fn schemaForTuplePlan(
        self: *ComptimeReifier,
        items: []const checked_artifact.ConstTupleElemPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchema {
        if (items.len == 0) return .zst;

        const schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, items.len);
        errdefer self.allocator.free(schemas);
        for (items, 0..) |item, i| {
            schemas[i] = try self.schemaForPlan(item.value);
        }
        return .{ .tuple = schemas };
    }

    fn reifyTagUnionPlan(
        self: *ComptimeReifier,
        variants_plan: []const checked_artifact.ConstTagVariantPlan,
        layout_idx: layout_mod.Idx,
        value: Value,
    ) Allocator.Error!ReifiedValue {
        const physical = self.logicalAggregateValue(layout_idx, value, .tag_union);
        const layout = self.layouts.getLayout(physical.layout_idx);
        if (layout.tag == .zst) return try self.reifyZstTagUnionPlan(variants_plan);
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

    fn reifyZstTagUnionPlan(
        self: *ComptimeReifier,
        variants_plan: []const checked_artifact.ConstTagVariantPlan,
    ) Allocator.Error!ReifiedValue {
        if (variants_plan.len != 1) reifierInvariant("ZST tag-union const graph plan must have exactly one variant");

        const variants = try self.allocator.alloc(checked_artifact.ComptimeVariantSchema, variants_plan.len);
        errdefer {
            for (variants) |variant| self.allocator.free(variant.payloads);
            self.allocator.free(variants);
        }

        const active_variant = variants_plan[0];
        const payload_values = try self.allocator.alloc(checked_artifact.ComptimeValueId, active_variant.payloads.len);
        errdefer self.allocator.free(payload_values);

        {
            const payload_schemas = try self.allocator.alloc(checked_artifact.ComptimeSchemaId, active_variant.payloads.len);
            errdefer self.allocator.free(payload_schemas);
            for (active_variant.payloads, 0..) |payload_plan, payload_i| {
                const reified = try self.reifyPlan(payload_plan.value, .zst, Value.zst);
                payload_schemas[payload_i] = reified.schema;
                payload_values[payload_i] = reified.value;
            }
            variants[0] = .{ .name = active_variant.tag, .payloads = payload_schemas };
        }

        return .{
            .schema = try self.values.addSchema(.{ .tag_union = variants }),
            .value = try self.values.addValue(.{ .tag_union = .{
                .variant_index = 0,
                .payloads = payload_values,
            } }),
        };
    }

    fn schemaForTagUnionPlan(
        self: *ComptimeReifier,
        variants_plan: []const checked_artifact.ConstTagVariantPlan,
    ) Allocator.Error!checked_artifact.ComptimeSchema {
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

        return .{ .tag_union = variants };
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
    const variant_layout = layouts.getLayout(variant_layout_idx);
    if (arg_count == 1) {
        if (variant_layout.tag == .struct_ and layouts.getStructInfo(variant_layout).fields.len == 1) {
            return layouts.getStructFieldLayoutByOriginalIndex(variant_layout.data.struct_.idx, 0);
        }
        return variant_layout_idx;
    }
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
