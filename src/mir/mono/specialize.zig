//! Specialization-driven mono MIR construction state.
//!
//! This is the only API that may turn checked procedure templates into mono MIR
//! procedures. It reserves procedure identities before body lowering so recursive
//! and mutually-recursive mono specializations cannot allocate duplicate
//! procedure values.

const std = @import("std");
const check = @import("check");
const base = @import("base");
const can = @import("can");
const symbol_mod = @import("symbol");

const Ast = @import("ast.zig");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const ArtifactNames = @import("../artifact_names.zig");
const Hosted = @import("../hosted.zig");
const mir_ids = @import("../ids.zig");
const Type = @import("type.zig");
const debug = @import("../debug_verify.zig");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;
const static_dispatch = check.StaticDispatchRegistry;
const CIR = can.CIR;

/// Public `MonoProcHandle` declaration.
pub const MonoProcHandle = enum(u32) { _ };

/// Public `MonoSpecializationReason` declaration.
pub const MonoSpecializationReason = union(enum) {
    root: checked_artifact.RootRequest,
    const_instance: checked_artifact.ConstInstantiationRequest,
    callable_binding_instance: checked_artifact.CallableBindingInstantiationRequest,
    call_proc: checked_artifact.CheckedExprId,
    proc_value: checked_artifact.CheckedExprId,
    static_dispatch_target: checked_artifact.StaticDispatchPlanId,
    iterator_dispatch_target: checked_artifact.IteratorForPlanId,
    comptime_dependency_summary: checked_artifact.ComptimeDependencySummaryId,
    promoted_callable_wrapper: canonical.PromotedCallableWrapperId,
    private_capture_callable_leaf: checked_artifact.PrivateCaptureNodeId,
    semantic_instantiation_procedure: checked_artifact.SemanticInstantiationProcedureId,
    erased_promoted_wrapper_code: canonical.ProcedureTemplateRef,
    erased_finite_capture_member: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
    str_inspect_nested: canonical.ProcedureTemplateRef,
    str_inspect_custom: canonical.ProcedureTemplateRef,
};

/// Public `MaterializationDependencyRoot` declaration.
pub const MaterializationDependencyRoot = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    summary: checked_artifact.ComptimeDependencySummaryId,
};

/// Public `Input` declaration.
pub const Input = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
    mode: LoweringMode = .runnable,
    checking_artifact_sink: ?*checked_artifact.CheckedModuleArtifact = null,
    materialization_dependencies: []const MaterializationDependencyRoot = &.{},
};

/// Public `LoweringMode` declaration.
pub const LoweringMode = enum {
    runnable,
    comptime_dependency_summary,
};

/// Public `MonoSpecializationRequest` declaration.
pub const MonoSpecializationRequest = struct {
    template: canonical.ProcedureTemplateRef,
    callable_template: ?canonical.CallableProcedureTemplateRef = null,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    reason: MonoSpecializationReason,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView = null,
    allow_return_widening: bool = false,
};

/// Public `ReservedState` declaration.
pub const ReservedState = enum {
    reserved,
    lowering,
    lowered,
};

/// Public `ReservedMonoProc` declaration.
pub const ReservedMonoProc = struct {
    proc: canonical.MonoSpecializedProcRef,
    callable_template: canonical.CallableProcedureTemplateRef,
    local_handle: MonoProcHandle,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView,
    allow_return_widening: bool,
    state: ReservedState,
};

const ConcreteConstProducer = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    payload: checked_artifact.CheckedTypeId,
    key: canonical.CanonicalTypeKey,
};

const ConcreteDependencyTypeProjection = union(enum) {
    producer: ConcreteConstProducer,
    concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
};

const InputDependencyViews = struct {
    views: []const checked_artifact.ImportedModuleView,
    owned: []checked_artifact.ImportedModuleView = &.{},

    fn deinit(self: *InputDependencyViews, allocator: Allocator) void {
        if (self.owned.len > 0) allocator.free(self.owned);
        self.* = .{ .views = &.{} };
    }
};

fn mirProcedureRefFromReserved(reserved: ReservedMonoProc) canonical.MirProcedureRef {
    return .{
        .proc = reserved.proc.proc,
        .callable = .{
            .template = reserved.callable_template,
            .source_fn_ty = reserved.proc.specialization.requested_mono_fn_ty,
        },
    };
}

/// Public `Proc` declaration.
pub const Proc = struct {
    key: canonical.MonoSpecializationKey,
    proc: canonical.MirProcedureRef,
    local_handle: MonoProcHandle,
    fn_ty: Type.TypeId,
    source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    body: Ast.DefId,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView = null,
};

/// Public `Program` declaration.
pub const Program = struct {
    allocator: Allocator,
    root_artifact_key: checked_artifact.CheckedModuleArtifactKey,
    canonical_names: canonical.CanonicalNameStore,
    concrete_source_types: ConcreteSourceType.Store,
    literal_pool: mir_ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    executable_synthetic_procs: std.ArrayList(mir_ids.ExecutableSyntheticProc),
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    root_metadata: std.ArrayList(mir_ids.RootMetadata),
    concrete_dependency_type_projections: std.ArrayList(ConcreteDependencyTypeProjection),
    bool_source_ty: ?ConcreteSourceType.ConcreteSourceTypeRef,

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .root_artifact_key = .{},
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .literal_pool = mir_ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .executable_synthetic_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
            .concrete_dependency_type_projections = .empty,
            .bool_source_ty = null,
        };
    }

    pub fn deinit(self: *Program) void {
        self.concrete_dependency_type_projections.deinit(self.allocator);
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.executable_synthetic_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.concrete_source_types.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }

    pub fn addProc(
        self: *Program,
        key: canonical.MonoSpecializationKey,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        body: Ast.DefId,
    ) Allocator.Error!void {
        try self.procs.append(self.allocator, .{
            .key = key,
            .proc = mirProcedureRefFromReserved(reserved),
            .local_handle = reserved.local_handle,
            .fn_ty = fn_ty,
            .source_fn_ty_payload = reserved.requested_fn_ty,
            .body = body,
            .imported_closure = reserved.imported_closure,
        });
    }

    pub fn addExecutableSyntheticProc(
        self: *Program,
        proc: mir_ids.ExecutableSyntheticProc,
    ) Allocator.Error!void {
        for (self.executable_synthetic_procs.items) |existing| {
            if (canonical.mirProcedureRefEql(existing.source_proc, proc.source_proc)) return;
        }
        try self.executable_synthetic_procs.append(self.allocator, proc);
    }

    pub fn addPatternBinderSymbol(
        self: *Program,
        binder: checked_artifact.PatternBinderId,
    ) Allocator.Error!Ast.Symbol {
        return try self.symbols.add(base.Ident.Idx.NONE, .{ .checked_pattern_binder = .{
            .binder_idx = @intFromEnum(binder),
        } });
    }

    pub fn addProcSymbol(
        self: *Program,
        handle: MonoProcHandle,
    ) Allocator.Error!Ast.Symbol {
        return try self.symbols.add(base.Ident.Idx.NONE, .{ .specialized_top_level_def = .{
            .source_symbol = @intFromEnum(handle),
        } });
    }

    pub fn addSyntheticSymbol(self: *Program) Allocator.Error!Ast.Symbol {
        return try self.symbols.add(base.Ident.Idx.NONE, .synthetic);
    }

    pub fn addSpecializedLocalFnSymbol(self: *Program, source: Ast.Symbol) Allocator.Error!Ast.Symbol {
        return try self.symbols.add(base.Ident.Idx.NONE, .{ .specialized_local_fn = .{
            .source_symbol = @intFromEnum(source),
        } });
    }

    pub fn boolSourceTypeRef(self: *Program) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        if (self.bool_source_ty) |ref| return ref;

        const builtin_module = try self.canonical_names.internModuleName("Builtin");
        const bool_name = try self.canonical_names.internTypeName("Bool");
        const false_label = try self.canonical_names.internTagLabel("False");
        const true_label = try self.canonical_names.internTagLabel("True");

        const empty_root = try self.concrete_source_types.reservePendingLocalRoot();
        self.concrete_source_types.fillLocalRoot(empty_root, .empty_tag_union);
        var empty_key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.canonical_names,
            self.concrete_source_types.local_payloads.items,
        );
        defer empty_key_builder.deinit();
        _ = try self.concrete_source_types.sealLocalRoot(empty_root, try empty_key_builder.keyForRoot(empty_root));

        const tags = try self.allocator.alloc(checked_artifact.CheckedTag, 2);
        tags[0] = .{ .name = false_label, .args = &.{} };
        tags[1] = .{ .name = true_label, .args = &.{} };

        const union_root = try self.concrete_source_types.reservePendingLocalRoot();
        self.concrete_source_types.fillLocalRoot(union_root, .{ .tag_union = .{
            .tags = tags,
            .ext = empty_root,
        } });
        var union_key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.canonical_names,
            self.concrete_source_types.local_payloads.items,
        );
        defer union_key_builder.deinit();
        _ = try self.concrete_source_types.sealLocalRoot(union_root, try union_key_builder.keyForRoot(union_root));

        const bool_root = try self.concrete_source_types.reservePendingLocalRoot();
        self.concrete_source_types.fillLocalRoot(bool_root, .{ .nominal = .{
            .name = bool_name,
            .origin_module = builtin_module,
            .builtin = .bool,
            .is_opaque = false,
            .backing = union_root,
            .representation = .{ .builtin = .bool },
            .args = &.{},
        } });
        var bool_key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.canonical_names,
            self.concrete_source_types.local_payloads.items,
        );
        defer bool_key_builder.deinit();
        const bool_key = try bool_key_builder.keyForRoot(bool_root);
        const bool_ref = try self.concrete_source_types.sealLocalRoot(bool_root, bool_key);
        self.bool_source_ty = bool_ref;
        return bool_ref;
    }
};

/// Public `run` function.
pub fn run(
    allocator: Allocator,
    input: Input,
    roots: []const checked_artifact.LoweringEntrypointRequest,
) Allocator.Error!Program {
    var program = Program.init(allocator);
    errdefer program.deinit();
    program.root_artifact_key = input.root.artifact.key;

    var queue = Queue.init(allocator);
    defer queue.deinit();
    var name_resolver = ArtifactNames.ArtifactNameResolver.init(
        &program.canonical_names,
        input.root.artifact,
        input.imports,
        input.root.relation_artifacts,
    );

    for (roots, 0..) |root, root_index| {
        const seed = try specializationSeedForEntrypoint(
            allocator,
            input,
            &program,
            &name_resolver,
            root,
            @intCast(root_index),
        ) orelse continue;
        const template_ref = try name_resolver.procedureTemplateRef(seed.template);
        const template_lookup = checkedTemplateForKey(input, template_ref, seed.imported_closure);
        const root_template_types = checkedTypesForKey(input, template_lookup.artifact) orelse {
            invariantViolation("mono root specialization template artifact checked types were unavailable");
        };
        var root_type_instantiator = TypeInstantiator.init(
            allocator,
            input,
            &program,
            root_template_types,
            &name_resolver,
            template_lookup.artifact,
        );
        defer root_type_instantiator.deinit();
        try root_type_instantiator.buildFromRequest(template_lookup.template.checked_fn_root, seed.requested_fn_ty, seed.allow_return_widening);
        const request = MonoSpecializationRequest{
            .template = template_ref,
            .requested_fn_ty = seed.requested_fn_ty,
            .reason = seed.reason,
            .imported_closure = seed.imported_closure,
            .allow_return_widening = seed.allow_return_widening,
        };
        const reserved = try queue.reserve(&program.concrete_source_types, request);
        try program.root_procs.append(allocator, mirProcedureRefFromReserved(reserved));
        try program.root_metadata.append(allocator, seed.metadata);
    }

    if (input.materialization_dependencies.len != 0) {
        var dependency_state = ConcreteDependencyReservationState.init(allocator);
        defer dependency_state.deinit();
        for (input.materialization_dependencies) |dependency| {
            try reserveComptimeDependencySummaryDependencies(
                input,
                &program,
                &queue,
                &dependency_state,
                dependency.owner,
                dependency.summary,
            );
        }
    }

    while (queue.pending.items.len != 0) {
        const key = queue.pending.orderedRemove(0);
        queue.markLowering(key);
        const reserved = queue.requested.get(key) orelse unreachable;
        const template_lookup = checkedTemplateForKey(input, key.template, reserved.imported_closure);
        if (executableSyntheticProcForReserved(input, key, reserved, template_lookup)) |synthetic| {
            try reserveExecutableSyntheticProcDependencies(allocator, input, &program, &queue, template_lookup.artifact, synthetic);
            try program.addExecutableSyntheticProc(synthetic);
            queue.markLowered(key);
            continue;
        }
        const current_template_types = checkedTypesForKey(input, template_lookup.artifact) orelse {
            invariantViolation("mono specialization template artifact checked types were unavailable");
        };
        var type_instantiator = TypeInstantiator.init(allocator, input, &program, current_template_types, &name_resolver, template_lookup.artifact);
        defer type_instantiator.deinit();
        try type_instantiator.buildFromRequest(template_lookup.template.checked_fn_root, reserved.requested_fn_ty, reserved.allow_return_widening);
        var graph_builder = MonoSpecializationGraphBuilder.init(allocator, input, &program, template_lookup, &type_instantiator, &name_resolver, &queue);
        defer graph_builder.deinit();
        const lowered = try graph_builder.lowerTemplateBody(reserved);
        try program.addProc(key, reserved, lowered.fn_ty, lowered.body);
        queue.markLowered(key);
    }

    try publishConcreteDependencyTypeProjections(allocator, input, &program);

    if (@import("builtin").mode == .Debug) verifyProgram(&program);
    return program;
}

fn publishConcreteDependencyTypeProjections(
    allocator: Allocator,
    input: Input,
    program: *Program,
) Allocator.Error!void {
    if (program.concrete_dependency_type_projections.items.len == 0) return;

    const artifact_sink = input.checking_artifact_sink orelse {
        invariantViolation("compile-time dependency summary recorded concrete dependency types without a mutable checked artifact sink");
    };

    var dependency_views = try inputDependencyViews(allocator, input);
    defer dependency_views.deinit(allocator);

    var projector = checked_artifact.CheckedTypeProjector.init(allocator, artifact_sink, dependency_views.views);
    defer projector.deinit();

    for (program.concrete_dependency_type_projections.items) |projection| {
        switch (projection) {
            .producer => |producer| try publishConcreteConstProducerType(
                artifact_sink,
                dependency_views.views,
                &projector,
                producer,
            ),
            .concrete_ref => |concrete_ref| try publishConcreteConstDependencyType(
                artifact_sink,
                dependency_views.views,
                &projector,
                program,
                concrete_ref,
            ),
        }
    }
}

fn publishConcreteConstProducerType(
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    projector: *checked_artifact.CheckedTypeProjector,
    producer: ConcreteConstProducer,
) Allocator.Error!void {
    if (artifact_sink.checked_types.rootForKey(producer.key) != null) return;

    if (std.mem.eql(u8, &producer.artifact.bytes, &artifact_sink.key.bytes)) {
        _ = try projector.projectCheckedTypeViewRoot(artifact_sink.checked_types.view(), producer.payload);
        return;
    }

    for (dependency_views) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &producer.artifact.bytes)) continue;
        const projected = try projector.projectImportedCheckedType(imported, producer.payload);
        verifyProjectedCheckedTypeKey(
            artifact_sink,
            projected,
            producer.key,
            "compile-time dependency summary concrete const producer payload key disagrees with imported artifact",
        );
        return;
    }
    if (@import("builtin").mode == .Debug) {
        const import0 = if (dependency_views.len > 0) dependency_views[0].key.bytes else [_]u8{0} ** 32;
        const import1 = if (dependency_views.len > 1) dependency_views[1].key.bytes else [_]u8{0} ** 32;
        const import2 = if (dependency_views.len > 2) dependency_views[2].key.bytes else [_]u8{0} ** 32;
        std.debug.panic(
            "compile-time dependency summary concrete const producer referenced an unknown import: producer={any} root={any} imports={d} import0={any} import1={any} import2={any}",
            .{
                producer.artifact.bytes,
                artifact_sink.key.bytes,
                dependency_views.len,
                import0,
                import1,
                import2,
            },
        );
    }
    invariantViolation("compile-time dependency summary concrete const producer referenced an unknown import");
}

fn publishConcreteConstDependencyType(
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    dependency_views: []const checked_artifact.ImportedModuleView,
    projector: *checked_artifact.CheckedTypeProjector,
    program: *Program,
    concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
) Allocator.Error!void {
    const root = program.concrete_source_types.root(concrete_ref);
    if (artifact_sink.checked_types.rootForKey(root.key) != null) return;

    switch (root.source) {
        .local => |local| {
            _ = try projector.projectCheckedTypeViewRootWithNames(
                program.concrete_source_types.localView(),
                &program.canonical_names,
                local,
            );
        },
        .artifact => |artifact_ref| {
            if (std.mem.eql(u8, &artifact_ref.artifact.bytes, &artifact_sink.key.bytes)) {
                if (artifact_sink.checked_types.rootForKey(root.key) != null) return;
                invariantViolation("compile-time dependency summary concrete artifact type was missing from the artifact sink");
            }
            for (dependency_views) |imported| {
                if (!std.mem.eql(u8, &imported.key.bytes, &artifact_ref.artifact.bytes)) continue;
                const projected = try projector.projectImportedCheckedType(imported, artifact_ref.ty);
                verifyProjectedCheckedTypeKey(
                    artifact_sink,
                    projected,
                    root.key,
                    "compile-time dependency summary concrete imported payload key disagrees with concrete source type",
                );
                return;
            }
            invariantViolation("compile-time dependency summary concrete artifact type referenced an unknown import");
        },
    }
}

fn verifyProjectedCheckedTypeKey(
    artifact_sink: *checked_artifact.CheckedModuleArtifact,
    projected: checked_artifact.CheckedTypeId,
    expected_key: canonical.CanonicalTypeKey,
    comptime message: []const u8,
) void {
    const index: usize = @intFromEnum(projected);
    if (index >= artifact_sink.checked_types.roots.len) {
        invariantViolation("compile-time dependency summary projected checked type was missing from artifact sink");
    }
    if (!std.meta.eql(artifact_sink.checked_types.roots[index].key.bytes, expected_key.bytes)) {
        invariantViolation(message);
    }
}

fn inputDependencyViews(
    allocator: Allocator,
    input: Input,
) Allocator.Error!InputDependencyViews {
    if (input.root.relation_artifacts.len == 0) {
        return .{ .views = input.imports };
    }

    const views = try allocator.alloc(
        checked_artifact.ImportedModuleView,
        input.imports.len + input.root.relation_artifacts.len,
    );
    @memcpy(views[0..input.imports.len], input.imports);
    @memcpy(views[input.imports.len..], input.root.relation_artifacts);
    return .{
        .views = views,
        .owned = views,
    };
}

const CheckedTemplateLookup = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    checked_types: checked_artifact.CheckedTypeStoreView,
    checked_bodies: checked_artifact.CheckedBodyStoreView,
    resolved_value_refs: *const checked_artifact.ResolvedValueRefTable,
    nested_proc_sites: *const checked_artifact.NestedProcSiteTable,
    hosted_procs: *const checked_artifact.HostedProcTable,
    intrinsic_wrappers: *const checked_artifact.IntrinsicWrapperTable,
    promoted_callable_wrappers: *const checked_artifact.PromotedCallableWrapperTable,
    promoted_callable_body_plans: *const checked_artifact.PromotedCallableBodyPlanTable,
    executable_type_payloads: *const checked_artifact.ExecutableTypePayloadStore,
    executable_value_transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    comptime_plans: *const checked_artifact.CompileTimePlanStore,
    comptime_values: *const checked_artifact.CompileTimeValueStore,
    entry_wrappers: ?*const checked_artifact.EntryWrapperTable,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView,
    template: checked_artifact.CheckedProcedureTemplate,
};

fn checkedTemplateForKey(
    input: Input,
    template_ref: canonical.ProcedureTemplateRef,
    access_closure: ?checked_artifact.ImportedTemplateClosureView,
) CheckedTemplateLookup {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &template_ref.artifact.bytes)) {
        return .{
            .artifact = input.root.artifact.key,
            .checked_types = input.root.artifact.checked_types.view(),
            .checked_bodies = input.root.artifact.checked_bodies.view(),
            .resolved_value_refs = &input.root.artifact.resolved_value_refs,
            .nested_proc_sites = &input.root.artifact.nested_proc_sites,
            .hosted_procs = &input.root.artifact.hosted_procs,
            .intrinsic_wrappers = &input.root.artifact.intrinsic_wrappers,
            .promoted_callable_wrappers = &input.root.artifact.promoted_callable_wrappers,
            .promoted_callable_body_plans = &input.root.artifact.promoted_callable_body_plans,
            .executable_type_payloads = &input.root.artifact.executable_type_payloads,
            .executable_value_transforms = &input.root.artifact.executable_value_transforms,
            .comptime_plans = &input.root.artifact.comptime_plans,
            .comptime_values = &input.root.artifact.comptime_values,
            .entry_wrappers = &input.root.artifact.entry_wrappers,
            .imported_closure = null,
            .template = input.root.artifact.checked_procedure_templates.get(template_ref.template),
        };
    }

    for (input.imports) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &template_ref.artifact.bytes)) continue;
        for (imported.exported_procedure_templates.templates) |exported| {
            if (exported.template.template == template_ref.template) {
                return .{
                    .artifact = imported.key,
                    .checked_types = imported.checked_types,
                    .checked_bodies = imported.checked_bodies,
                    .resolved_value_refs = imported.resolved_value_refs,
                    .nested_proc_sites = imported.nested_proc_sites,
                    .hosted_procs = imported.hosted_procs,
                    .intrinsic_wrappers = imported.intrinsic_wrappers,
                    .promoted_callable_wrappers = imported.promoted_callable_wrappers,
                    .promoted_callable_body_plans = imported.promoted_callable_body_plans,
                    .executable_type_payloads = imported.executable_type_payloads,
                    .executable_value_transforms = imported.executable_value_transforms,
                    .comptime_plans = imported.comptime_plans,
                    .comptime_values = imported.comptime_values,
                    .entry_wrappers = imported.entry_wrappers,
                    .imported_closure = exported.template_closure,
                    .template = exported.template_data,
                };
            }
        }
        if (access_closure) |closure| {
            if (importedClosureContainsProcedureTemplate(closure, template_ref)) {
                return .{
                    .artifact = imported.key,
                    .checked_types = imported.checked_types,
                    .checked_bodies = imported.checked_bodies,
                    .resolved_value_refs = imported.resolved_value_refs,
                    .nested_proc_sites = imported.nested_proc_sites,
                    .hosted_procs = imported.hosted_procs,
                    .intrinsic_wrappers = imported.intrinsic_wrappers,
                    .promoted_callable_wrappers = imported.promoted_callable_wrappers,
                    .promoted_callable_body_plans = imported.promoted_callable_body_plans,
                    .executable_type_payloads = imported.executable_type_payloads,
                    .executable_value_transforms = imported.executable_value_transforms,
                    .comptime_plans = imported.comptime_plans,
                    .comptime_values = imported.comptime_values,
                    .entry_wrappers = imported.entry_wrappers,
                    .imported_closure = closure,
                    .template = imported.checked_procedure_templates.get(template_ref.template),
                };
            }
        }
        if (exportedConstEvalTemplateContains(imported.exported_const_templates, template_ref)) {
            return .{
                .artifact = imported.key,
                .checked_types = imported.checked_types,
                .checked_bodies = imported.checked_bodies,
                .resolved_value_refs = imported.resolved_value_refs,
                .nested_proc_sites = imported.nested_proc_sites,
                .hosted_procs = imported.hosted_procs,
                .intrinsic_wrappers = imported.intrinsic_wrappers,
                .promoted_callable_wrappers = imported.promoted_callable_wrappers,
                .promoted_callable_body_plans = imported.promoted_callable_body_plans,
                .executable_type_payloads = imported.executable_type_payloads,
                .executable_value_transforms = imported.executable_value_transforms,
                .comptime_plans = imported.comptime_plans,
                .comptime_values = imported.comptime_values,
                .entry_wrappers = imported.entry_wrappers,
                .imported_closure = null,
                .template = imported.checked_procedure_templates.get(template_ref.template),
            };
        }
        debug.invariant(false, "mono specialization invariant violated: imported template was not exported or present in the imported closure");
        unreachable;
    }

    for (input.root.relation_artifacts) |related| {
        if (!std.mem.eql(u8, &related.key.bytes, &template_ref.artifact.bytes)) continue;
        for (related.exported_procedure_templates.templates) |exported| {
            if (exported.template.template == template_ref.template) {
                return .{
                    .artifact = related.key,
                    .checked_types = related.checked_types,
                    .checked_bodies = related.checked_bodies,
                    .resolved_value_refs = related.resolved_value_refs,
                    .nested_proc_sites = related.nested_proc_sites,
                    .hosted_procs = related.hosted_procs,
                    .intrinsic_wrappers = related.intrinsic_wrappers,
                    .promoted_callable_wrappers = related.promoted_callable_wrappers,
                    .promoted_callable_body_plans = related.promoted_callable_body_plans,
                    .executable_type_payloads = related.executable_type_payloads,
                    .executable_value_transforms = related.executable_value_transforms,
                    .comptime_plans = related.comptime_plans,
                    .comptime_values = related.comptime_values,
                    .entry_wrappers = related.entry_wrappers,
                    .imported_closure = exported.template_closure,
                    .template = exported.template_data,
                };
            }
        }
        if (access_closure) |closure| {
            if (importedClosureContainsProcedureTemplate(closure, template_ref)) {
                return .{
                    .artifact = related.key,
                    .checked_types = related.checked_types,
                    .checked_bodies = related.checked_bodies,
                    .resolved_value_refs = related.resolved_value_refs,
                    .nested_proc_sites = related.nested_proc_sites,
                    .hosted_procs = related.hosted_procs,
                    .intrinsic_wrappers = related.intrinsic_wrappers,
                    .promoted_callable_wrappers = related.promoted_callable_wrappers,
                    .promoted_callable_body_plans = related.promoted_callable_body_plans,
                    .executable_type_payloads = related.executable_type_payloads,
                    .executable_value_transforms = related.executable_value_transforms,
                    .comptime_plans = related.comptime_plans,
                    .comptime_values = related.comptime_values,
                    .entry_wrappers = related.entry_wrappers,
                    .imported_closure = closure,
                    .template = related.checked_procedure_templates.get(template_ref.template),
                };
            }
        }
        if (exportedConstEvalTemplateContains(related.exported_const_templates, template_ref)) {
            return .{
                .artifact = related.key,
                .checked_types = related.checked_types,
                .checked_bodies = related.checked_bodies,
                .resolved_value_refs = related.resolved_value_refs,
                .nested_proc_sites = related.nested_proc_sites,
                .hosted_procs = related.hosted_procs,
                .intrinsic_wrappers = related.intrinsic_wrappers,
                .promoted_callable_wrappers = related.promoted_callable_wrappers,
                .promoted_callable_body_plans = related.promoted_callable_body_plans,
                .executable_type_payloads = related.executable_type_payloads,
                .executable_value_transforms = related.executable_value_transforms,
                .comptime_plans = related.comptime_plans,
                .comptime_values = related.comptime_values,
                .entry_wrappers = related.entry_wrappers,
                .imported_closure = null,
                .template = related.checked_procedure_templates.get(template_ref.template),
            };
        }
        if (access_closure) |closure| {
            debug.invariantFmt(
                false,
                "mono specialization invariant violated: relation template {d} was not exported or present in the relation closure with {d} procedure templates",
                .{ @intFromEnum(template_ref.template), closure.checked_procedure_templates.len },
            );
        }
        debug.invariantFmt(
            false,
            "mono specialization invariant violated: relation template {d} was not exported and no relation closure was supplied",
            .{@intFromEnum(template_ref.template)},
        );
        unreachable;
    }

    debug.invariant(false, "mono specialization invariant violated: template artifact was not available to lowering");
    unreachable;
}

fn importedClosureContainsProcedureTemplate(
    closure: checked_artifact.ImportedTemplateClosureView,
    template_ref: canonical.ProcedureTemplateRef,
) bool {
    for (closure.checked_procedure_templates) |listed| {
        if (std.mem.eql(u8, &listed.artifact.bytes, &template_ref.artifact.bytes) and listed.template == template_ref.template) return true;
    }
    return false;
}

fn exportedConstEvalTemplateContains(
    exported_const_templates: checked_artifact.ExportedConstTemplateView,
    template_ref: canonical.ProcedureTemplateRef,
) bool {
    for (exported_const_templates.templates) |exported| {
        switch (exported.template.state) {
            .eval_template => |eval| {
                if (std.mem.eql(u8, &eval.entry_template.artifact.bytes, &template_ref.artifact.bytes) and
                    eval.entry_template.template == template_ref.template)
                {
                    return true;
                }
            },
            .value_graph_template,
            .reserved,
            => {},
        }
    }
    return false;
}

const EntrypointSeed = struct {
    template: canonical.ProcedureTemplateRef,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    metadata: mir_ids.RootMetadata,
    reason: MonoSpecializationReason,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView = null,
    allow_return_widening: bool = false,
};

const RootTemplateSelection = struct {
    template: canonical.ProcedureTemplateRef,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView = null,
};

fn specializationSeedForEntrypoint(
    allocator: Allocator,
    input: Input,
    program: *Program,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    entrypoint: checked_artifact.LoweringEntrypointRequest,
    order: u32,
) Allocator.Error!?EntrypointSeed {
    return switch (entrypoint) {
        .root => |root| root_seed: {
            const requested_fn_ty = try program.concrete_source_types.registerArtifactRoot(
                input.root.artifact.key,
                input.root.artifact.checked_types.view(),
                root.checked_type,
            );
            const selection = templateForRoot(input, root, &program.concrete_source_types, requested_fn_ty) orelse break :root_seed null;
            break :root_seed .{
                .template = selection.template,
                .requested_fn_ty = requested_fn_ty,
                .metadata = rootMetadataFromChecked(root),
                .reason = .{ .root = root },
                .imported_closure = selection.imported_closure,
                .allow_return_widening = std.meta.activeTag(root.source) == .required_binding,
            };
        },
        .const_instance => |request| const_seed: {
            const selection = constEvalEntryTemplateForRequest(input, request) orelse {
                invariantViolation("mono specialization compile-time const instance did not name an eval template");
            };
            break :const_seed .{
                .template = selection.template,
                .requested_fn_ty = try compileTimeEntryFunctionTypeForReturn(
                    allocator,
                    input,
                    program,
                    name_resolver,
                    request.requested_source_ty_payload,
                ),
                .metadata = compileTimeMetadata(order, .compile_time_constant),
                .reason = .{ .const_instance = request },
                .imported_closure = selection.imported_closure,
            };
        },
        .callable_binding_instance => |request| callable_seed: {
            const selection = callableEvalEntryTemplateForRequest(input, request) orelse {
                invariantViolation("mono specialization compile-time callable binding instance did not name a callable eval template");
            };
            break :callable_seed .{
                .template = selection.template,
                .requested_fn_ty = try compileTimeEntryFunctionTypeForReturn(
                    allocator,
                    input,
                    program,
                    name_resolver,
                    request.requested_source_fn_ty_payload,
                ),
                .metadata = compileTimeMetadata(order, .compile_time_callable),
                .reason = .{ .callable_binding_instance = request },
                .imported_closure = selection.imported_closure,
            };
        },
    };
}

fn compileTimeMetadata(order: u32, kind: mir_ids.RootKind) mir_ids.RootMetadata {
    return .{
        .order = order,
        .kind = kind,
        .abi = .compile_time,
        .exposure = .private,
    };
}

fn compileTimeEntryFunctionTypeForReturn(
    allocator: Allocator,
    input: Input,
    program: *Program,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    return_ty: checked_artifact.CheckedTypeId,
) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
    var materializer = TypeInstantiator.init(
        allocator,
        input,
        program,
        input.root.artifact.checked_types.view(),
        name_resolver,
        input.root.artifact.key,
    );
    defer materializer.deinit();
    const ret_ref = try materializer.freshConcreteRefForCheckedViewRoot(
        input.root.artifact.key,
        input.root.artifact.checked_types.view(),
        return_ty,
    );
    const local_ret = switch (program.concrete_source_types.root(ret_ref).source) {
        .local => |local| local,
        .artifact => invariantViolation("compile-time entry return clone was not registered as a local concrete source type"),
    };
    const local_fn = try program.concrete_source_types.reservePendingLocalRoot();
    program.concrete_source_types.fillLocalRoot(local_fn, .{ .function = .{
        .kind = .pure,
        .args = &.{},
        .ret = local_ret,
        .needs_instantiation = false,
    } });
    var key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
        allocator,
        &program.canonical_names,
        program.concrete_source_types.local_payloads.items,
    );
    defer key_builder.deinit();
    return try program.concrete_source_types.sealLocalRoot(local_fn, try key_builder.keyForRoot(local_fn));
}

fn constEvalEntryTemplateForRequest(
    input: Input,
    request: checked_artifact.ConstInstantiationRequest,
) ?RootTemplateSelection {
    const templates = constTemplatesForKey(input, request.key.const_ref.artifact) orelse {
        debug.invariant(false, "mono specialization invariant violated: const instance template artifact was not available");
        unreachable;
    };
    const template = templates.get(request.key.const_ref);
    return switch (template.state) {
        .eval_template => |eval| .{
            .template = eval.entry_template,
            .imported_closure = relationClosureForConstRef(input, request.key.const_ref) orelse
                exportedClosureForConstRef(input, request.key.const_ref),
        },
        .value_graph_template => null,
        .reserved => {
            debug.invariant(false, "mono specialization invariant violated: const instance reached unsealed template");
            unreachable;
        },
    };
}

fn relationClosureForConstRef(
    input: Input,
    target_const: checked_artifact.ConstRef,
) ?checked_artifact.ImportedTemplateClosureView {
    for (input.root.artifact.platform_required_bindings.bindings) |binding| {
        const closure = switch (binding.value_use) {
            .const_value => |platform_const| platform_const.relation_template_closure,
            .procedure_value => |procedure| procedure.relation_template_closure,
        };
        if (importedClosureContainsConstRef(closure, target_const)) return closure;
    }
    return null;
}

fn importedClosureContainsConstRef(
    closure: checked_artifact.ImportedTemplateClosureView,
    target_const: checked_artifact.ConstRef,
) bool {
    for (closure.const_templates) |listed| {
        if (constRefEql(listed, target_const)) return true;
    }
    return false;
}

fn exportedClosureForConstRef(
    input: Input,
    target_const: checked_artifact.ConstRef,
) ?checked_artifact.ImportedTemplateClosureView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &target_const.artifact.bytes)) return null;

    for (input.imports) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &target_const.artifact.bytes)) continue;
        for (imported.exported_const_templates.templates) |exported| {
            if (constRefEql(exported.const_ref, target_const)) return exported.template_closure;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (!std.mem.eql(u8, &related.key.bytes, &target_const.artifact.bytes)) continue;
        for (related.exported_const_templates.templates) |exported| {
            if (constRefEql(exported.const_ref, target_const)) return exported.template_closure;
        }
    }
    return null;
}

fn constRefEql(a: checked_artifact.ConstRef, b: checked_artifact.ConstRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        std.meta.eql(a.owner, b.owner) and
        a.template == b.template and
        std.mem.eql(u8, &a.source_scheme.bytes, &b.source_scheme.bytes);
}

fn callableEvalEntryTemplateForRequest(
    input: Input,
    request: checked_artifact.CallableBindingInstantiationRequest,
) ?RootTemplateSelection {
    const OwnerAndBinding = struct {
        owner: checked_artifact.CheckedModuleArtifactKey,
        binding: checked_artifact.TopLevelProcedureBindingRef,
        imported_closure: ?checked_artifact.ImportedTemplateClosureView = null,
    };
    const owner_and_binding: OwnerAndBinding = switch (request.key.binding) {
        .top_level => |binding| .{
            .owner = binding.artifact,
            .binding = binding.binding,
            .imported_closure = relationClosureForTopLevelBindingRef(input, binding),
        },
        .platform_required => |required| .{
            .owner = required.artifact,
            .binding = required.procedure_binding,
            .imported_closure = relationClosureForProcedureBindingRef(input, request.key.binding),
        },
        .imported => |imported| {
            const view = importedProcedureBindingViewForRef(input, imported) orelse {
                debug.invariant(false, "mono specialization invariant violated: imported callable eval binding was not available");
                unreachable;
            };
            return switch (view.body) {
                .callable_eval_template => |template_id| .{
                    .template = callableEvalEntryTemplateForKey(input, imported.artifact, template_id),
                    .imported_closure = view.template_closure,
                },
                .direct_template => null,
            };
        },
        .hosted,
        .promoted,
        => return null,
    };
    const bindings = topLevelProcedureBindingsForKey(input, owner_and_binding.owner) orelse {
        debug.invariant(false, "mono specialization invariant violated: callable eval binding owner artifact was not available");
        unreachable;
    };
    const binding = bindings.get(owner_and_binding.binding);
    return switch (binding.body) {
        .callable_eval_template => |template_id| .{
            .template = callableEvalEntryTemplateForKey(input, owner_and_binding.owner, template_id),
            .imported_closure = owner_and_binding.imported_closure,
        },
        .direct_template => null,
    };
}

fn relationClosureForProcedureBindingRef(
    input: Input,
    binding_ref: checked_artifact.ProcedureBindingRef,
) ?checked_artifact.ImportedTemplateClosureView {
    return switch (binding_ref) {
        .platform_required => |required| blk: {
            for (input.root.artifact.platform_required_bindings.bindings) |binding| {
                const procedure_use = switch (binding.value_use) {
                    .procedure_value => |procedure| procedure,
                    .const_value => continue,
                };
                if (!requiredAppProcedureRefEql(procedure_use.procedure.binding, required)) continue;
                break :blk procedure_use.relation_template_closure;
            }
            debug.invariant(false, "mono specialization invariant violated: platform-required callable eval binding had no relation closure");
            unreachable;
        },
        else => null,
    };
}

fn relationClosureForTopLevelBindingRef(
    input: Input,
    binding_ref: checked_artifact.ArtifactTopLevelProcedureBindingRef,
) ?checked_artifact.ImportedTemplateClosureView {
    if (std.mem.eql(u8, &binding_ref.artifact.bytes, &input.root.artifact.key.bytes)) return null;
    const bindings = topLevelProcedureBindingsForKey(input, binding_ref.artifact) orelse {
        debug.invariant(false, "mono specialization invariant violated: top-level callable eval binding owner artifact was not available");
        unreachable;
    };
    const binding = bindings.get(binding_ref.binding);
    const template_id = switch (binding.body) {
        .direct_template => return null,
        .callable_eval_template => |template| template,
    };
    for (input.root.artifact.platform_required_bindings.bindings) |platform_binding| {
        const closure = switch (platform_binding.value_use) {
            .procedure_value => |procedure| procedure.relation_template_closure,
            .const_value => |const_value| const_value.relation_template_closure,
        };
        if (importedClosureContainsCallableEvalTemplate(closure, binding_ref.artifact, template_id)) {
            return closure;
        }
    }
    debug.invariant(false, "mono specialization invariant violated: non-local top-level callable eval binding had no relation closure");
    unreachable;
}

fn importedClosureContainsCallableEvalTemplate(
    closure: checked_artifact.ImportedTemplateClosureView,
    artifact: checked_artifact.CheckedModuleArtifactKey,
    template_id: checked_artifact.CallableEvalTemplateId,
) bool {
    for (closure.callable_eval_templates) |template| {
        if (std.mem.eql(u8, &template.artifact.bytes, &artifact.bytes) and template.template == template_id) return true;
    }
    return false;
}

fn requiredAppProcedureRefEql(
    binding_ref: checked_artifact.ProcedureBindingRef,
    required: checked_artifact.RequiredAppProcedureRef,
) bool {
    const actual = switch (binding_ref) {
        .platform_required => |actual| actual,
        else => return false,
    };
    return std.mem.eql(u8, &actual.artifact.bytes, &required.artifact.bytes) and
        topLevelValueRefEql(actual.app_value, required.app_value) and
        actual.procedure_binding == required.procedure_binding;
}

fn topLevelValueRefEql(
    a: checked_artifact.TopLevelValueRef,
    b: checked_artifact.TopLevelValueRef,
) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and a.pattern == b.pattern;
}

fn executableSyntheticProcForReserved(
    input: Input,
    key: canonical.MonoSpecializationKey,
    reserved: ReservedMonoProc,
    template_lookup: CheckedTemplateLookup,
) ?mir_ids.ExecutableSyntheticProc {
    return switch (template_lookup.template.body) {
        .promoted_callable_wrapper => |wrapper_id| blk: {
            const wrapper = template_lookup.promoted_callable_wrappers.get(wrapper_id);
            const body_plan = template_lookup.promoted_callable_body_plans.get(wrapper.body_plan);
            break :blk switch (body_plan) {
                .erased => |erased| erased_blk: {
                    if (!std.mem.eql(u8, &erased.sealed.boundary.source_fn_ty.bytes, &key.requested_mono_fn_ty.bytes)) {
                        invariantViolation("erased promoted callable wrapper source function type disagrees with mono specialization request");
                    }
                    const current_checked_types = checkedTypesForKey(input, template_lookup.artifact) orelse {
                        invariantViolation("mono synthetic executable procedure referenced unavailable checked types");
                    };
                    break :erased_blk mir_ids.ExecutableSyntheticProc{
                        .artifact = template_lookup.artifact,
                        .source_proc = mirProcedureRefFromReserved(reserved),
                        .template = key.template,
                        .signature = executableSyntheticSignatureForErased(current_checked_types, erased),
                        .executable_type_payloads = template_lookup.executable_type_payloads,
                        .executable_value_transforms = template_lookup.executable_value_transforms,
                        .comptime_plans = template_lookup.comptime_plans,
                        .comptime_values = template_lookup.comptime_values,
                        .body = .{ .erased_promoted_wrapper = erased },
                    };
                },
                .finite => null,
                .pending => invariantViolation("mono specialization reached unsealed promoted callable wrapper body plan"),
            };
        },
        else => null,
    };
}

fn executableSyntheticSignatureForErased(
    checked_types: checked_artifact.CheckedTypeStoreView,
    erased: checked_artifact.ErasedPromotedWrapperBodyPlan,
) mir_ids.ExecutableSyntheticProcSignaturePlan {
    const checked_root = checkedTypeRootForKey(checked_types, erased.sealed.boundary.source_fn_ty) orelse {
        invariantViolation("erased promoted wrapper source function type was not published in checked type roots");
    };
    return .{
        .source_fn_ty = erased.sealed.boundary.source_fn_ty,
        .source_fn_ty_payload = checked_root,
        .params = erased.params,
        .ret_source_ty = checkedFunctionReturnSourceType(checked_types, checked_root),
        .ret_source_ty_payload = checkedFunctionReturnPayload(checked_types, checked_root),
    };
}

fn checkedFunctionReturnSourceType(
    checked_types: checked_artifact.CheckedTypeStoreView,
    checked_root: checked_artifact.CheckedTypeId,
) canonical.CanonicalTypeKey {
    const payload = checked_types.payloads[@intFromEnum(checked_root)];
    const ret = switch (payload) {
        .alias => |alias| return checkedFunctionReturnSourceType(checked_types, alias.backing),
        .function => |function| function.ret,
        else => invariantViolation("erased promoted wrapper source function type payload was not a function"),
    };
    return checked_types.roots[@intFromEnum(ret)].key;
}

fn checkedFunctionReturnPayload(
    checked_types: checked_artifact.CheckedTypeStoreView,
    checked_root: checked_artifact.CheckedTypeId,
) checked_artifact.CheckedTypeId {
    const payload = checked_types.payloads[@intFromEnum(checked_root)];
    return switch (payload) {
        .alias => |alias| checkedFunctionReturnPayload(checked_types, alias.backing),
        .function => |function| function.ret,
        else => invariantViolation("erased promoted wrapper source function type payload was not a function"),
    };
}

const PrivateCaptureDependencyKey = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    node: checked_artifact.PrivateCaptureNodeId,
};

const ErasedCaptureExecutableMaterializationDependencyKey = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    node: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
};

const ConstInstanceDependencyKey = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    instance: checked_artifact.ConstInstanceId,
};

const CallableBindingInstanceDependencyKey = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    instance: checked_artifact.CallableBindingInstanceId,
};

const ComptimeSummaryDependencyKey = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    summary: checked_artifact.ComptimeDependencySummaryId,
};

const SemanticInstantiationProcedureDependencyKey = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    procedure: checked_artifact.SemanticInstantiationProcedureId,
};

const ConcreteDependencyReservationState = struct {
    const_instances: std.AutoHashMap(ConstInstanceDependencyKey, void),
    callable_binding_instances: std.AutoHashMap(CallableBindingInstanceDependencyKey, void),
    comptime_summaries: std.AutoHashMap(ComptimeSummaryDependencyKey, void),
    semantic_procedures: std.AutoHashMap(SemanticInstantiationProcedureDependencyKey, void),

    fn init(allocator: Allocator) ConcreteDependencyReservationState {
        return .{
            .const_instances = std.AutoHashMap(ConstInstanceDependencyKey, void).init(allocator),
            .callable_binding_instances = std.AutoHashMap(CallableBindingInstanceDependencyKey, void).init(allocator),
            .comptime_summaries = std.AutoHashMap(ComptimeSummaryDependencyKey, void).init(allocator),
            .semantic_procedures = std.AutoHashMap(SemanticInstantiationProcedureDependencyKey, void).init(allocator),
        };
    }

    fn deinit(self: *ConcreteDependencyReservationState) void {
        self.semantic_procedures.deinit();
        self.comptime_summaries.deinit();
        self.callable_binding_instances.deinit();
        self.const_instances.deinit();
    }
};

const ExecutableSyntheticDependencyState = struct {
    private_captures: std.AutoHashMap(PrivateCaptureDependencyKey, void),
    erased_materializations: std.AutoHashMap(ErasedCaptureExecutableMaterializationDependencyKey, void),
    concrete_dependencies: ConcreteDependencyReservationState,

    fn init(allocator: Allocator) ExecutableSyntheticDependencyState {
        return .{
            .private_captures = std.AutoHashMap(PrivateCaptureDependencyKey, void).init(allocator),
            .erased_materializations = std.AutoHashMap(ErasedCaptureExecutableMaterializationDependencyKey, void).init(allocator),
            .concrete_dependencies = ConcreteDependencyReservationState.init(allocator),
        };
    }

    fn deinit(self: *ExecutableSyntheticDependencyState) void {
        self.concrete_dependencies.deinit();
        self.erased_materializations.deinit();
        self.private_captures.deinit();
    }
};

fn reserveExecutableSyntheticProcDependencies(
    allocator: Allocator,
    input: Input,
    program: *Program,
    queue: *Queue,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    synthetic: mir_ids.ExecutableSyntheticProc,
) Allocator.Error!void {
    const owner_checked_types = checkedTypesForKey(input, owner_artifact) orelse {
        invariantViolation("mono synthetic executable procedure referenced unavailable owner checked types");
    };
    _ = try program.concrete_source_types.registerArtifactRoot(
        owner_artifact,
        owner_checked_types,
        synthetic.signature.source_fn_ty_payload,
    );
    for (synthetic.signature.params) |param| {
        _ = try program.concrete_source_types.registerArtifactRoot(
            owner_artifact,
            owner_checked_types,
            param.checked_ty,
        );
    }
    _ = try program.concrete_source_types.registerArtifactRoot(
        owner_artifact,
        owner_checked_types,
        synthetic.signature.ret_source_ty_payload,
    );

    var state = ExecutableSyntheticDependencyState.init(allocator);
    defer state.deinit();

    switch (synthetic.body) {
        .erased_promoted_wrapper => |erased| {
            try reserveErasedPromotedWrapperCodeDependency(input, program, queue, owner_artifact, erased, .{ .erased_promoted_wrapper_code = synthetic.template });
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, &state, owner_artifact, synthetic.comptime_plans, erased.sealed.capture);
            switch (erased.hidden_capture_arg) {
                .none => {},
                .materialized_capture => |capture| try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, &state, owner_artifact, synthetic.comptime_plans, capture),
            }
        },
    }
}

fn reserveErasedPromotedWrapperCodeDependency(
    input: Input,
    program: *Program,
    queue: *Queue,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    erased: checked_artifact.ErasedPromotedWrapperBodyPlan,
    reason: MonoSpecializationReason,
) Allocator.Error!void {
    switch (erased.sealed.code) {
        .direct_proc => |direct| try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner_artifact, .{
            .proc_value = direct.code.proc_value,
            .source_fn_ty_payload = erased.sealed.boundary.checked_fn_root,
        }, reason),
        .finite_adapter => |finite| {
            if (finite.members.len == 0) {
                invariantViolation("mono dependency reservation reached persisted finite-set adapter with no member targets");
            }
            const descriptor = callableSetDescriptorForKey(input, finite.adapter_key.callable_set_key) orelse {
                invariantViolation("mono dependency reservation reached persisted finite-set adapter with no callable-set descriptor");
            };
            if (descriptor.members.len != finite.members.len) {
                invariantViolation("mono dependency reservation persisted finite-set adapter target count differs from descriptor");
            }
            for (descriptor.members, finite.members) |descriptor_member, sealed_member| {
                validatePersistedFiniteAdapterMemberTarget(descriptor_member, sealed_member.target_key);
                try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner_artifact, .{
                    .proc_value = sealed_member.proc_value,
                    .source_fn_ty_payload = sealed_member.member_proc_source_fn_ty_payload,
                    .lifted_owner_source_fn_ty_payload = sealed_member.member_lifted_owner_source_fn_ty_payload,
                }, reason);
            }
        },
    }
}

fn validatePersistedFiniteAdapterMemberTarget(
    member: canonical.CanonicalCallableSetMember,
    target: canonical.ExecutableSpecializationKey,
) void {
    if (member.source_proc.proc.proc_base != target.base) {
        invariantViolation("mono persisted finite-set adapter member target base differs from descriptor member");
    }
    if (!std.mem.eql(u8, &member.proc_value.source_fn_ty.bytes, &target.requested_fn_ty.bytes)) {
        invariantViolation("mono persisted finite-set adapter member target source type differs from procedure value");
    }
}

fn reserveSealedErasedCallableDependency(
    input: Input,
    program: *Program,
    queue: *Queue,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    sealed: checked_artifact.SealedErasedCallableValue,
    reason: MonoSpecializationReason,
) Allocator.Error!void {
    switch (sealed.code) {
        .direct_proc => |direct| try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner_artifact, .{
            .proc_value = direct.code.proc_value,
            .source_fn_ty_payload = sealed.boundary.checked_fn_root,
        }, reason),
        .finite_adapter => |finite| {
            if (finite.members.len == 0) {
                invariantViolation("mono dependency reservation reached sealed finite adapter with no members");
            }
            for (finite.members) |member| {
                try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner_artifact, .{
                    .proc_value = member.proc_value,
                    .source_fn_ty_payload = member.member_proc_source_fn_ty_payload,
                    .lifted_owner_source_fn_ty_payload = member.member_lifted_owner_source_fn_ty_payload,
                }, reason);
            }
        },
    }
}

fn remapDependencyCallable(
    input: Input,
    program: *Program,
    callable: canonical.ProcedureCallableRef,
) Allocator.Error!canonical.ProcedureCallableRef {
    var name_resolver = ArtifactNames.ArtifactNameResolver.init(
        &program.canonical_names,
        input.root.artifact,
        input.imports,
        input.root.relation_artifacts,
    );
    return try name_resolver.procedureCallableRef(callable);
}

fn callableSetDescriptorForKey(
    input: Input,
    key: canonical.CanonicalCallableSetKey,
) ?*const canonical.CanonicalCallableSetDescriptor {
    if (input.root.artifact.callable_set_descriptors.descriptorFor(key)) |descriptor| return descriptor;
    for (input.imports) |import| {
        if (import.callable_set_descriptors.descriptorFor(key)) |descriptor| return descriptor;
    }
    return null;
}

fn reserveErasedCaptureExecutableMaterializationPlanDependencies(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ExecutableSyntheticDependencyState,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    plans: *const checked_artifact.CompileTimePlanStore,
    capture: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) Allocator.Error!void {
    switch (capture) {
        .none,
        .zero_sized_typed,
        => {},
        .node => |node| try reserveErasedCaptureExecutableMaterializationNodeDependencies(input, program, queue, state, owner_artifact, plans, node),
    }
}

fn reserveErasedCaptureExecutableMaterializationNodeDependencies(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ExecutableSyntheticDependencyState,
    owner_artifact: checked_artifact.CheckedModuleArtifactKey,
    plans: *const checked_artifact.CompileTimePlanStore,
    node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
) Allocator.Error!void {
    const visit_key = ErasedCaptureExecutableMaterializationDependencyKey{
        .artifact = owner_artifact,
        .node = node_id,
    };
    const visit = try state.erased_materializations.getOrPut(visit_key);
    if (visit.found_existing) return;

    switch (plans.erasedCaptureExecutableMaterializationNode(node_id)) {
        .pending => invariantViolation("mono dependency reservation reached pending erased capture materialization node"),
        .const_instance => |const_instance| try reserveConstInstanceRefDependencies(
            input,
            program,
            queue,
            &state.concrete_dependencies,
            const_instance,
        ),
        .pure_const,
        .pure_value,
        => {},
        .finite_callable_set => |finite| {
            try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner_artifact, .{
                .proc_value = finite.selected_proc,
                .source_fn_ty_payload = finite.member_proc_source_fn_ty_payload,
                .lifted_owner_source_fn_ty_payload = finite.member_lifted_owner_source_fn_ty_payload,
                .proc_template_closure = finite.member_proc_template_closure,
                .lifted_owner_template_closure = finite.member_lifted_owner_template_closure,
            }, .{ .erased_finite_capture_member = node_id });
            for (finite.captures) |capture| {
                try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, capture);
            }
        },
        .erased_callable => |erased| {
            try reserveSealedErasedCallableDependency(input, program, queue, owner_artifact, erased.sealed, .{ .erased_finite_capture_member = node_id });
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, erased.sealed.capture);
        },
        .record => |fields| for (fields) |field| {
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, field.value);
        },
        .tuple => |items| for (items) |item| {
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, item);
        },
        .tag_union => |tag| for (tag.payloads) |payload| {
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, payload.value);
        },
        .list => |items| for (items) |item| {
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, item);
        },
        .box => |payload| try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, payload),
        .nominal => |nominal| try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, nominal.backing),
        .recursive_ref => |ref| try reserveErasedCaptureExecutableMaterializationNodeDependencies(input, program, queue, state, owner_artifact, plans, ref),
    }
}

fn reservePrivateCaptureNodeDependencies(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ExecutableSyntheticDependencyState,
    artifact: checked_artifact.CheckedModuleArtifactKey,
    plans: *const checked_artifact.CompileTimePlanStore,
    node_id: checked_artifact.PrivateCaptureNodeId,
) Allocator.Error!void {
    const visit_key = PrivateCaptureDependencyKey{
        .artifact = artifact,
        .node = node_id,
    };
    const visit = try state.private_captures.getOrPut(visit_key);
    if (visit.found_existing) return;

    switch (plans.privateCapture(node_id)) {
        .pending => invariantViolation("mono dependency reservation reached pending private capture node"),
        .const_instance_leaf => |leaf| try reserveConstInstanceRefDependencies(
            input,
            program,
            queue,
            &state.concrete_dependencies,
            leaf.const_instance,
        ),
        .finite_callable_leaf => |leaf| try reserveCallableLeafDependency(input, program, queue, artifact, leaf, .{ .private_capture_callable_leaf = node_id }),
        .record => |fields| for (fields) |field| {
            try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, field.value);
        },
        .tuple => |items| for (items) |item| {
            try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, item);
        },
        .tag_union => |tag| for (tag.payloads) |payload| {
            try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, payload.value);
        },
        .list => |items| for (items) |item| {
            try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, item);
        },
        .box => |payload| try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, payload),
        .nominal => |nominal| try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, nominal.backing),
        .recursive_ref => |ref| try reservePrivateCaptureNodeDependencies(input, program, queue, state, artifact, plans, ref),
    }
}

fn reserveCallableLeafDependency(
    input: Input,
    program: *Program,
    queue: *Queue,
    owner: checked_artifact.CheckedModuleArtifactKey,
    leaf: checked_artifact.FiniteCallableLeafInstance,
    reason: MonoSpecializationReason,
) Allocator.Error!void {
    try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner, .{
        .proc_value = leaf.proc_value,
        .source_fn_ty_payload = leaf.source_fn_ty_payload,
        .lifted_owner_source_fn_ty_payload = leaf.lifted_owner_source_fn_ty_payload,
        .proc_template_closure = leaf.proc_template_closure,
        .lifted_owner_template_closure = leaf.lifted_owner_template_closure,
    }, reason);
}

fn reserveConstInstanceRefDependencies(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ConcreteDependencyReservationState,
    ref: checked_artifact.ConstInstanceRef,
) Allocator.Error!void {
    const visit_key = ConstInstanceDependencyKey{
        .owner = ref.owner,
        .instance = ref.instance,
    };
    const visit = try state.const_instances.getOrPut(visit_key);
    if (visit.found_existing) return;

    const instance = constInstanceForRef(input, ref);
    const summary = instance.dependency_summary orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: constant instance had no concrete dependency summary");
        unreachable;
    };
    try reserveComptimeDependencySummaryDependencies(input, program, queue, state, ref.owner, summary);
    for (instance.generated_procedures) |procedure| {
        try reserveSemanticInstantiationProcedureDependency(input, program, queue, state, ref.owner, procedure);
    }
}

fn reserveCallableBindingInstanceRefDependencies(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ConcreteDependencyReservationState,
    ref: checked_artifact.CallableBindingInstanceRef,
) Allocator.Error!void {
    const visit_key = CallableBindingInstanceDependencyKey{
        .owner = ref.owner,
        .instance = ref.instance,
    };
    const visit = try state.callable_binding_instances.getOrPut(visit_key);
    if (visit.found_existing) return;

    const record = callableBindingInstanceRecordForRef(input, ref);
    const instance = switch (record.state) {
        .evaluated => |evaluated| evaluated,
        .reserved, .evaluating => invariantViolation("callable binding instance was consumed before it was sealed"),
    };
    const reason = MonoSpecializationReason{ .comptime_dependency_summary = instance.dependency_summary };
    _ = try reserveCallableProcedureDependencyWithPayloads(
        input,
        program,
        queue,
        ref.owner,
        .{
            .proc_value = instance.proc_value,
            .source_fn_ty_payload = record.requested_source_fn_ty_payload,
            .proc_template_closure = closureForCallableBindingInstanceRef(input, ref),
        },
        reason,
    );
    try reserveComptimeDependencySummaryDependencies(input, program, queue, state, ref.owner, instance.dependency_summary);
    for (instance.generated_procedures) |procedure| {
        try reserveSemanticInstantiationProcedureDependency(input, program, queue, state, ref.owner, procedure);
    }
}

fn closureForCallableBindingInstanceRef(
    input: Input,
    ref: checked_artifact.CallableBindingInstanceRef,
) ?checked_artifact.ImportedTemplateClosureView {
    return switch (ref.key.binding) {
        .imported => |imported| blk: {
            const view = importedProcedureBindingViewForRef(input, imported) orelse {
                debug.invariant(false, "mono dependency reservation invariant violated: imported callable binding instance had no imported binding view");
                unreachable;
            };
            break :blk view.template_closure;
        },
        .platform_required => relationClosureForProcedureBindingRef(input, ref.key.binding),
        .top_level => |binding| relationClosureForTopLevelBindingRef(input, binding),
        .hosted,
        .promoted,
        => null,
    };
}

fn reserveComptimeDependencySummaryDependencies(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ConcreteDependencyReservationState,
    owner: checked_artifact.CheckedModuleArtifactKey,
    summary_id: checked_artifact.ComptimeDependencySummaryId,
) Allocator.Error!void {
    const visit_key = ComptimeSummaryDependencyKey{
        .owner = owner,
        .summary = summary_id,
    };
    const visit = try state.comptime_summaries.getOrPut(visit_key);
    if (visit.found_existing) return;

    const dependencies = comptimeDependenciesForKey(input, owner) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: dependency summary owner artifact was not available");
        unreachable;
    };
    const summary_index = @intFromEnum(summary_id);
    if (summary_index >= dependencies.summaries.len) {
        debug.invariant(false, "mono dependency reservation invariant violated: dependency summary id was out of range");
        unreachable;
    }
    const summary = dependencies.summaries[summary_index];
    const reason = MonoSpecializationReason{ .comptime_dependency_summary = summary_id };

    for (summary.concrete_values) |value| {
        switch (value) {
            .const_instance => |request| {
                const ref = constInstanceForKey(input, owner, request.key) orelse {
                    debug.invariant(false, "mono dependency reservation invariant violated: dependency summary referenced an unsealed constant instance");
                    unreachable;
                };
                try reserveConstInstanceRefDependencies(input, program, queue, state, ref);
            },
            .callable_binding_instance => |request| {
                const ref = callableBindingInstanceForKey(input, owner, request.key) orelse {
                    debug.invariant(false, "mono dependency reservation invariant violated: dependency summary referenced an unsealed callable binding instance");
                    unreachable;
                };
                try reserveCallableBindingInstanceRefDependencies(input, program, queue, state, ref);
            },
            .procedure_callable_with_payloads => |dependency| {
                try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner, dependency, reason);
            },
        }
    }
}

fn reserveCallableProcedureDependencyWithPayloads(
    input: Input,
    program: *Program,
    queue: *Queue,
    dependency_owner: checked_artifact.CheckedModuleArtifactKey,
    dependency: checked_artifact.ProcedureCallableDependency,
    reason: MonoSpecializationReason,
) Allocator.Error!void {
    const lowering_callable = try remapDependencyCallable(input, program, dependency.proc_value);
    const source_ref = try registerDependencyPayload(
        input,
        program,
        dependency_owner,
        dependency.source_fn_ty_payload,
        lowering_callable.source_fn_ty,
        "mono dependency reservation callable source type payload disagreed with callable source type",
    );
    var concrete_callable = lowering_callable;
    concrete_callable.source_fn_ty = program.concrete_source_types.key(source_ref);
    switch (concrete_callable.template) {
        .checked,
        .synthetic,
        => _ = try reserveLoweringProcedureCallableDependencyWithConcreteRef(input, program, queue, concrete_callable, source_ref, dependency.proc_template_closure, reason),
        .lifted => |lifted| {
            const owner_payload = dependency.lifted_owner_source_fn_ty_payload orelse {
                debug.invariant(false, "mono dependency reservation lifted callable dependency had no owner source type payload");
                unreachable;
            };
            const owner_ref = try registerDependencyPayload(
                input,
                program,
                dependency_owner,
                owner_payload,
                lifted.owner_mono_specialization.requested_mono_fn_ty,
                "mono dependency reservation lifted callable owner source type payload disagreed with owner specialization",
            );
            _ = try queue.reserve(&program.concrete_source_types, .{
                .template = lifted.owner_mono_specialization.template,
                .requested_fn_ty = owner_ref,
                .reason = reason,
                .imported_closure = dependency.lifted_owner_template_closure,
            });
        },
    }
}

fn registerDependencyPayload(
    input: Input,
    program: *Program,
    owner: checked_artifact.CheckedModuleArtifactKey,
    checked_ty: checked_artifact.CheckedTypeId,
    expected_key: canonical.CanonicalTypeKey,
    comptime mismatch_message: []const u8,
) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
    const checked_types = checkedTypesForKey(input, owner) orelse {
        debug.invariant(false, "mono dependency reservation explicit payload owner artifact was not available");
        unreachable;
    };
    const ref = try program.concrete_source_types.registerArtifactRoot(owner, checked_types, checked_ty);
    const key = program.concrete_source_types.key(ref);
    if (!std.mem.eql(u8, &key.bytes, &expected_key.bytes)) {
        invariantViolation(mismatch_message);
    }
    return ref;
}

fn reserveSemanticInstantiationProcedureDependency(
    input: Input,
    program: *Program,
    queue: *Queue,
    state: *ConcreteDependencyReservationState,
    owner: checked_artifact.CheckedModuleArtifactKey,
    procedure_id: checked_artifact.SemanticInstantiationProcedureId,
) Allocator.Error!void {
    const visit_key = SemanticInstantiationProcedureDependencyKey{
        .owner = owner,
        .procedure = procedure_id,
    };
    const visit = try state.semantic_procedures.getOrPut(visit_key);
    if (visit.found_existing) return;

    const procedures = semanticInstantiationProceduresForKey(input, owner) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: semantic procedure owner artifact was not available");
        unreachable;
    };
    const proc_index = @intFromEnum(procedure_id);
    if (proc_index >= procedures.procedures.len) {
        debug.invariant(false, "mono dependency reservation invariant violated: semantic procedure id was out of range");
        unreachable;
    }
    const record = procedures.procedures[proc_index];
    if (record.id != procedure_id) {
        debug.invariant(false, "mono dependency reservation invariant violated: semantic procedure table id was not canonical");
        unreachable;
    }
    const procedure = switch (record.state) {
        .sealed => |sealed| sealed,
        .reserved => {
            debug.invariant(false, "mono dependency reservation invariant violated: semantic procedure was not sealed");
            unreachable;
        },
    };
    const callable = canonical.ProcedureCallableRef{
        .template = procedure.template,
        .source_fn_ty = semanticInstantiationProcedureSourceTy(record.key),
    };
    const dependency = checked_artifact.ProcedureCallableDependency{
        .proc_value = callable,
        .source_fn_ty_payload = semanticInstantiationProcedureSourceTyPayload(record.key),
    };
    const owner_checked_types = checkedTypesForKey(input, owner) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: semantic procedure checked type owner was not available");
        unreachable;
    };
    const payload_key = checkedTypeKey(owner_checked_types, dependency.source_fn_ty_payload);
    if (!std.mem.eql(u8, &payload_key.bytes, &callable.source_fn_ty.bytes)) {
        invariantViolation("mono dependency reservation semantic procedure payload disagreed with procedure source type");
    }
    try reserveCallableProcedureDependencyWithPayloads(input, program, queue, owner, dependency, .{ .semantic_instantiation_procedure = procedure_id });
}

fn reserveLoweringProcedureCallableDependencyWithConcreteRef(
    _: Input,
    program: *Program,
    queue: *Queue,
    callable: canonical.ProcedureCallableRef,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView,
    reason: MonoSpecializationReason,
) Allocator.Error!canonical.MirProcedureRef {
    const requested_key = program.concrete_source_types.key(requested_fn_ty);
    if (!std.mem.eql(u8, &requested_key.bytes, &callable.source_fn_ty.bytes)) {
        invariantViolation("mono dependency reservation explicit callable source type disagreed with callable occurrence");
    }
    var concrete_callable = callable;
    concrete_callable.source_fn_ty = requested_key;
    const template = checkedTemplateFromCallableTemplate(callable.template);
    const reserved = try queue.reserve(&program.concrete_source_types, .{
        .template = template,
        .callable_template = concrete_callable.template,
        .requested_fn_ty = requested_fn_ty,
        .reason = reason,
        .imported_closure = imported_closure,
    });
    return .{
        .proc = reserved.proc.proc,
        .callable = concrete_callable,
    };
}

fn artifactKeyForRef(
    input: Input,
    artifact: canonical.ArtifactRef,
) ?checked_artifact.CheckedModuleArtifactKey {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &artifact.bytes)) return input.root.artifact.key;
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &artifact.bytes)) return imported.key;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &artifact.bytes)) return related.key;
    }
    return null;
}

const CanonicalNameSortContext = struct {
    names: *const canonical.CanonicalNameStore,
};

fn typeFieldNameLessThan(ctx: CanonicalNameSortContext, a: Type.Field, b: Type.Field) bool {
    const left = ctx.names.recordFieldLabelText(a.name);
    const right = ctx.names.recordFieldLabelText(b.name);
    if (!std.mem.eql(u8, left, right)) return std.mem.lessThan(u8, left, right);
    return @intFromEnum(a.name) < @intFromEnum(b.name);
}

fn typeTagNameLessThan(ctx: CanonicalNameSortContext, a: Type.Tag, b: Type.Tag) bool {
    const left = ctx.names.tagLabelText(a.name);
    const right = ctx.names.tagLabelText(b.name);
    if (!std.mem.eql(u8, left, right)) return std.mem.lessThan(u8, left, right);
    return @intFromEnum(a.name) < @intFromEnum(b.name);
}

fn checkedRecordFieldNameLessThan(
    ctx: CanonicalNameSortContext,
    a: checked_artifact.CheckedRecordField,
    b: checked_artifact.CheckedRecordField,
) bool {
    const left = ctx.names.recordFieldLabelText(a.name);
    const right = ctx.names.recordFieldLabelText(b.name);
    if (!std.mem.eql(u8, left, right)) return std.mem.lessThan(u8, left, right);
    return @intFromEnum(a.name) < @intFromEnum(b.name);
}

fn checkedTagNameLessThan(
    ctx: CanonicalNameSortContext,
    a: checked_artifact.CheckedTag,
    b: checked_artifact.CheckedTag,
) bool {
    const left = ctx.names.tagLabelText(a.name);
    const right = ctx.names.tagLabelText(b.name);
    if (!std.mem.eql(u8, left, right)) return std.mem.lessThan(u8, left, right);
    return @intFromEnum(a.name) < @intFromEnum(b.name);
}

const TypeInstantiator = struct {
    allocator: Allocator,
    input: Input,
    program: *Program,
    template_types: checked_artifact.CheckedTypeStoreView,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    template_artifact: checked_artifact.CheckedModuleArtifactKey,
    substitutions: std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef),
    defaulted_numeric_substitutions: std.AutoHashMap(checked_artifact.CheckedTypeId, void),
    concrete_variable_substitutions: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, ConcreteSourceType.ConcreteSourceTypeRef),
    lowered_concrete: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, Type.TypeId),
    materialized_template_roots: std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    materialized_concrete_roots: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId),
    runtime_concrete_roots: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId),
    concrete_template_refs: std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef),
    row_clone_template_roots: std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    row_clone_concrete_roots: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId),

    const ConcreteRecordFieldEntry = struct {
        name: canonical.RecordFieldLabelId,
        owner: ConcreteSourceType.ConcreteSourceTypeRef,
        ty: checked_artifact.CheckedTypeId,
    };

    const ConcreteRecordTail = struct {
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        is_open: bool,
    };

    const ConcreteTagEntry = struct {
        name: canonical.TagLabelId,
        owner: ConcreteSourceType.ConcreteSourceTypeRef,
        tag: checked_artifact.CheckedTag,
    };

    const ConcreteTagTail = struct {
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        is_open: bool,
    };

    fn init(
        allocator: Allocator,
        input: Input,
        program: *Program,
        template_types: checked_artifact.CheckedTypeStoreView,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
        template_artifact: checked_artifact.CheckedModuleArtifactKey,
    ) TypeInstantiator {
        return .{
            .allocator = allocator,
            .input = input,
            .program = program,
            .template_types = template_types,
            .name_resolver = name_resolver,
            .template_artifact = template_artifact,
            .substitutions = std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .defaulted_numeric_substitutions = std.AutoHashMap(checked_artifact.CheckedTypeId, void).init(allocator),
            .concrete_variable_substitutions = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .lowered_concrete = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, Type.TypeId).init(allocator),
            .materialized_template_roots = std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId).init(allocator),
            .materialized_concrete_roots = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId).init(allocator),
            .runtime_concrete_roots = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId).init(allocator),
            .concrete_template_refs = std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .row_clone_template_roots = std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId).init(allocator),
            .row_clone_concrete_roots = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId).init(allocator),
        };
    }

    fn deinit(self: *TypeInstantiator) void {
        self.row_clone_concrete_roots.deinit();
        self.row_clone_template_roots.deinit();
        self.concrete_template_refs.deinit();
        self.runtime_concrete_roots.deinit();
        self.materialized_concrete_roots.deinit();
        self.materialized_template_roots.deinit();
        self.lowered_concrete.deinit();
        self.concrete_variable_substitutions.deinit();
        self.defaulted_numeric_substitutions.deinit();
        self.substitutions.deinit();
    }

    fn buildFromRequest(
        self: *TypeInstantiator,
        template_fn_root: checked_artifact.CheckedTypeId,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
        allow_return_widening: bool,
    ) Allocator.Error!void {
        if (allow_return_widening) {
            try self.unifyFunctionTemplateWithConcreteAllowingReturnWidening(template_fn_root, requested_fn_ty);
        } else {
            try self.unifyTemplateWithConcrete(template_fn_root, requested_fn_ty);
        }
    }

    fn unifyFunctionTemplateWithConcreteAllowingReturnWidening(
        self: *TypeInstantiator,
        template_fn_root: checked_artifact.CheckedTypeId,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const template = self.templatePayload(template_fn_root);
        const requested = self.concretePayload(requested_fn_ty);
        switch (template) {
            .alias => |alias| return try self.unifyFunctionTemplateWithConcreteAllowingReturnWidening(alias.backing, requested_fn_ty),
            .function => |func| switch (requested) {
                .alias => |alias| return try self.unifyFunctionTemplateWithConcreteAllowingReturnWidening(
                    template_fn_root,
                    try self.concreteAliasBackingRef(requested_fn_ty, alias),
                ),
                .function => |requested_func| {
                    if (func.args.len != requested_func.args.len) {
                        invariantViolation("mono specialization concrete function shape mismatch");
                    }
                    try self.unifyTypeLists(func.args, requested_fn_ty, requested_func.args);
                    try self.unifyReturnWithConcreteAllowingTagWidening(func.ret, try self.concreteChildRef(requested_fn_ty, requested_func.ret));
                },
                else => invariantViolation("mono specialization expected a concrete function"),
            },
            else => try self.unifyTemplateWithConcrete(template_fn_root, requested_fn_ty),
        }
    }

    fn unifyReturnWithConcreteAllowingTagWidening(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        requested: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const template_payload = self.templatePayload(template_id);
        switch (template_payload) {
            .alias => |alias| return try self.unifyReturnWithConcreteAllowingTagWidening(alias.backing, requested),
            .nominal => |nominal| {
                if (nominal.builtin != null or nominal.is_opaque) return try self.unifyTemplateWithConcrete(template_id, requested);
                return try self.unifyReturnWithConcreteAllowingTagWidening(nominal.backing, try self.transparentConcreteNominalBackingOrSelf(requested));
            },
            .tag_union,
            .empty_tag_union,
            => return try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                try self.concreteRefForTemplateTypePreservingVariables(template_id),
                requested,
            ),
            .tuple => |items| {
                const requested_payload = self.concretePayload(requested);
                const requested_items = switch (requested_payload) {
                    .tuple => |tuple| tuple,
                    else => return try self.unifyTemplateWithConcrete(template_id, requested),
                };
                if (items.len != requested_items.len) invariantViolation("mono specialization concrete tuple shape mismatch");
                for (items, requested_items) |item, requested_item| {
                    try self.unifyReturnWithConcreteAllowingTagWidening(item, try self.concreteChildRef(requested, requested_item));
                }
            },
            else => try self.unifyTemplateWithConcrete(template_id, requested),
        }
    }

    fn fork(self: *const TypeInstantiator) Allocator.Error!TypeInstantiator {
        var child = TypeInstantiator.init(
            self.allocator,
            self.input,
            self.program,
            self.templateTypes(),
            self.name_resolver,
            self.template_artifact,
        );
        errdefer child.deinit();

        try child.substitutions.ensureTotalCapacity(self.substitutions.count());
        var substitutions = self.substitutions.iterator();
        while (substitutions.next()) |entry| {
            child.substitutions.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.defaulted_numeric_substitutions.ensureTotalCapacity(self.defaulted_numeric_substitutions.count());
        var defaulted_numeric = self.defaulted_numeric_substitutions.iterator();
        while (defaulted_numeric.next()) |entry| {
            child.defaulted_numeric_substitutions.putAssumeCapacity(entry.key_ptr.*, {});
        }

        try child.concrete_variable_substitutions.ensureTotalCapacity(self.concrete_variable_substitutions.count());
        var concrete_vars = self.concrete_variable_substitutions.iterator();
        while (concrete_vars.next()) |entry| {
            child.concrete_variable_substitutions.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.lowered_concrete.ensureTotalCapacity(self.lowered_concrete.count());
        var lowered_concrete = self.lowered_concrete.iterator();
        while (lowered_concrete.next()) |entry| {
            child.lowered_concrete.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.materialized_template_roots.ensureTotalCapacity(self.materialized_template_roots.count());
        var materialized_template = self.materialized_template_roots.iterator();
        while (materialized_template.next()) |entry| {
            child.materialized_template_roots.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.materialized_concrete_roots.ensureTotalCapacity(self.materialized_concrete_roots.count());
        var materialized_concrete = self.materialized_concrete_roots.iterator();
        while (materialized_concrete.next()) |entry| {
            child.materialized_concrete_roots.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.runtime_concrete_roots.ensureTotalCapacity(self.runtime_concrete_roots.count());
        var runtime_concrete = self.runtime_concrete_roots.iterator();
        while (runtime_concrete.next()) |entry| {
            child.runtime_concrete_roots.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.concrete_template_refs.ensureTotalCapacity(self.concrete_template_refs.count());
        var concrete_template = self.concrete_template_refs.iterator();
        while (concrete_template.next()) |entry| {
            child.concrete_template_refs.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.row_clone_template_roots.ensureTotalCapacity(self.row_clone_template_roots.count());
        var cloned_templates = self.row_clone_template_roots.iterator();
        while (cloned_templates.next()) |entry| {
            child.row_clone_template_roots.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.row_clone_concrete_roots.ensureTotalCapacity(self.row_clone_concrete_roots.count());
        var cloned_concrete = self.row_clone_concrete_roots.iterator();
        while (cloned_concrete.next()) |entry| {
            child.row_clone_concrete_roots.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        return child;
    }

    fn lowerTemplateType(self: *TypeInstantiator, id: checked_artifact.CheckedTypeId) Allocator.Error!Type.TypeId {
        return try self.lowerConcreteRef(try self.concreteRefForTemplateType(id));
    }

    fn concreteRefForTemplateType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        if (self.substitutions.get(id)) |concrete| return self.resolveConcreteRef(concrete);
        if (self.concrete_template_refs.get(id)) |existing| return self.resolveConcreteRef(existing);

        const local_root = try self.materializeTemplateType(id);
        var key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.program.canonical_names,
            self.program.concrete_source_types.local_payloads.items,
        );
        defer key_builder.deinit();
        const key = try key_builder.keyForRoot(local_root);
        const concrete = try self.program.concrete_source_types.sealLocalRoot(local_root, key);
        try self.concrete_template_refs.put(id, concrete);
        return concrete;
    }

    fn concreteRefForTemplateTypePreservingVariables(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        if (self.substitutions.get(id)) |concrete| return self.resolveConcreteRef(concrete);
        const local = try self.cloneTemplateTypeForRowEquation(id);
        return try self.program.concrete_source_types.registerLocalRoot(local);
    }

    fn freshConcreteRefForCheckedViewRoot(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        source_root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        var cloned = std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId).init(self.allocator);
        defer cloned.deinit();

        const local_root = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, source_root, &cloned);
        try self.sealReachableMaterializedLocalGraph(local_root);
        return try self.program.concrete_source_types.registerLocalRoot(local_root);
    }

    fn cloneCheckedViewTypeForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        source_id: checked_artifact.CheckedTypeId,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const index: usize = @intFromEnum(source_id);
        if (index >= source.roots.len or index >= source.payloads.len) {
            invariantViolation("mono specialization method target callable referenced a missing checked type root");
        }
        if (cloned.get(source_id)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try cloned.put(source_id, local_root);

        const payload = try self.cloneCheckedViewPayloadForFreshInstantiation(
            source_artifact,
            source,
            source.payloads[index],
            cloned,
        );
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        return local_root;
    }

    fn cloneCheckedViewPayloadForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        payload: checked_artifact.CheckedTypePayload,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization method target callable reached pending checked type payload"),
            .flex => |flex| .{ .flex = try self.cloneCheckedViewVariableForFreshInstantiation(source_artifact, source, flex, cloned) },
            .rigid => |rigid| .{ .rigid = try self.cloneCheckedViewVariableForFreshInstantiation(source_artifact, source, rigid, cloned) },
            .alias => |alias| .{ .alias = .{
                .name = try self.name_resolver.typeName(source_artifact, alias.name),
                .origin_module = try self.name_resolver.moduleName(source_artifact, alias.origin_module),
                .backing = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, alias.backing, cloned),
                .args = try self.cloneCheckedViewTypeIdsForFreshInstantiation(source_artifact, source, alias.args, cloned),
            } },
            .record_unbound => |fields| .{ .record_unbound = try self.cloneCheckedViewRecordFieldsForFreshInstantiation(source_artifact, source, fields, cloned) },
            .record => |record| .{ .record = .{
                .fields = try self.cloneCheckedViewRecordFieldsForFreshInstantiation(source_artifact, source, record.fields, cloned),
                .ext = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, record.ext, cloned),
            } },
            .tuple => |items| .{ .tuple = try self.cloneCheckedViewTypeIdsForFreshInstantiation(source_artifact, source, items, cloned) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.name_resolver.typeName(source_artifact, nominal.name),
                .origin_module = try self.name_resolver.moduleName(source_artifact, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, nominal.backing, cloned),
                .representation = nominal.representation,
                .args = try self.cloneCheckedViewTypeIdsForFreshInstantiation(source_artifact, source, nominal.args, cloned),
            } },
            .function => |func| .{ .function = .{
                .kind = checked_artifact.finalizedFunctionKind(func.kind),
                .args = try self.cloneCheckedViewTypeIdsForFreshInstantiation(source_artifact, source, func.args, cloned),
                .ret = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, func.ret, cloned),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.cloneCheckedViewTagsForFreshInstantiation(source_artifact, source, tag_union.tags, cloned),
                .ext = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, tag_union.ext, cloned),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn cloneCheckedViewTypeIdsForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        ids: []const checked_artifact.CheckedTypeId,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, id, cloned);
        }
        return out;
    }

    fn cloneCheckedViewRecordFieldsForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        fields: []const checked_artifact.CheckedRecordField,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.name_resolver.recordFieldLabel(source_artifact, field.name),
                .ty = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, field.ty, cloned),
            };
        }
        return out;
    }

    fn cloneCheckedViewTagsForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        tags: []const checked_artifact.CheckedTag,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.name_resolver.tagLabel(source_artifact, tag.name),
                .args = try self.cloneCheckedViewTypeIdsForFreshInstantiation(source_artifact, source, tag.args, cloned),
            };
        }
        return out;
    }

    fn cloneCheckedViewVariableForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        variable: checked_artifact.CheckedTypeVariable,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error!checked_artifact.CheckedTypeVariable {
        return .{
            .name = try self.copyOptionalText(variable.name),
            .constraints = try self.cloneCheckedViewConstraintsForFreshInstantiation(source_artifact, source, variable.constraints, cloned),
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn cloneCheckedViewConstraintsForFreshInstantiation(
        self: *TypeInstantiator,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source: checked_artifact.CheckedTypeStoreView,
        constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
        cloned: *std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    ) Allocator.Error![]const checked_artifact.CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedStaticDispatchConstraint, constraints.len);
        errdefer self.allocator.free(out);
        for (constraints, 0..) |constraint, i| {
            out[i] = .{
                .fn_name = try self.name_resolver.methodName(source_artifact, constraint.fn_name),
                .fn_ty = try self.cloneCheckedViewTypeForFreshInstantiation(source_artifact, source, constraint.fn_ty, cloned),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            };
        }
        return out;
    }

    fn cloneTemplateTypeForRowEquation(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        if (self.substitutions.get(id)) |concrete| return try self.cloneConcreteTypeForRowEquation(concrete);
        if (self.row_clone_template_roots.get(id)) |existing| return existing;

        switch (self.templatePayload(id)) {
            .flex => |flex| return try self.cloneTemplateVariableForRowEquation(id, .flex, flex),
            .rigid => |rigid| return try self.cloneTemplateVariableForRowEquation(id, .rigid, rigid),
            else => {},
        }

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.row_clone_template_roots.put(id, local_root);
        errdefer _ = self.row_clone_template_roots.remove(id);

        const payload = try self.cloneTemplatePayloadForRowEquation(self.templatePayload(id));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealReachableMaterializedLocalGraph(local_root);
        return local_root;
    }

    fn cloneTemplateVariableForRowEquation(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
        comptime kind: enum { flex, rigid },
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.row_clone_template_roots.put(id, local_root);
        errdefer _ = self.row_clone_template_roots.remove(id);

        const payload: checked_artifact.CheckedTypePayload = switch (kind) {
            .flex => .{ .flex = try self.cloneTemplateVariableForRowEquationPayload(variable) },
            .rigid => .{ .rigid = try self.cloneTemplateVariableForRowEquationPayload(variable) },
        };
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        const concrete = try self.sealMaterializedLocalRootRef(local_root);
        try self.substitutions.put(id, concrete);
        self.clearLoweredTypeCaches();
        return local_root;
    }

    fn cloneConcreteTypeForRowEquation(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const resolved = self.resolveConcreteRef(ref);
        const root = self.program.concrete_source_types.root(resolved);
        switch (root.source) {
            .local => |local| switch (self.concretePayload(resolved)) {
                .record_unbound => {},
                else => return local,
            },
            .artifact => {},
        }
        if (self.row_clone_concrete_roots.get(resolved)) |existing| return existing;

        switch (self.concretePayload(resolved)) {
            .flex => |flex| return try self.cloneConcreteVariableForRowEquation(resolved, .flex, flex),
            .rigid => |rigid| return try self.cloneConcreteVariableForRowEquation(resolved, .rigid, rigid),
            else => {},
        }

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.row_clone_concrete_roots.put(resolved, local_root);
        errdefer _ = self.row_clone_concrete_roots.remove(resolved);

        const payload = try self.cloneConcretePayloadForRowEquation(resolved, self.concretePayload(resolved));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealReachableMaterializedLocalGraph(local_root);
        return local_root;
    }

    fn cloneConcreteVariableForRowEquation(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        comptime kind: enum { flex, rigid },
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.row_clone_concrete_roots.put(ref, local_root);
        errdefer _ = self.row_clone_concrete_roots.remove(ref);

        const payload: checked_artifact.CheckedTypePayload = switch (kind) {
            .flex => .{ .flex = try self.cloneConcreteVariableForRowEquationPayload(ref, variable) },
            .rigid => .{ .rigid = try self.cloneConcreteVariableForRowEquationPayload(ref, variable) },
        };
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        const concrete = try self.sealMaterializedLocalRootRef(local_root);
        try self.bindConcreteVariable(ref, concrete);
        return local_root;
    }

    fn cloneTemplatePayloadForRowEquation(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished checked type payload"),
            .flex => |flex| .{ .flex = try self.cloneTemplateVariableForRowEquationPayload(flex) },
            .rigid => |rigid| .{ .rigid = try self.cloneTemplateVariableForRowEquationPayload(rigid) },
            .alias => |alias| .{ .alias = .{
                .name = try self.name_resolver.typeName(self.template_artifact, alias.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, alias.origin_module),
                .backing = try self.cloneTemplateTypeForRowEquation(alias.backing),
                .args = try self.cloneTemplateTypeIdsForRowEquation(alias.args),
            } },
            .record_unbound => |fields| .{ .record = .{
                .fields = try self.cloneTemplateRecordFieldsForRowEquation(fields),
                .ext = try self.materializeSyntheticPayload(.{ .flex = .{} }),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.cloneTemplateRecordFieldsForRowEquation(record.fields),
                .ext = try self.cloneTemplateTypeForRowEquation(record.ext),
            } },
            .tuple => |items| .{ .tuple = try self.cloneTemplateTypeIdsForRowEquation(items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.name_resolver.typeName(self.template_artifact, nominal.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.cloneTemplateTypeForRowEquation(nominal.backing),
                .representation = nominal.representation,
                .args = try self.cloneTemplateTypeIdsForRowEquation(nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = checked_artifact.finalizedFunctionKind(func.kind),
                .args = try self.cloneTemplateTypeIdsForRowEquation(func.args),
                .ret = try self.cloneTemplateTypeForRowEquation(func.ret),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.cloneTemplateTagsForRowEquation(tag_union.tags),
                .ext = try self.cloneTemplateTypeForRowEquation(tag_union.ext),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn cloneConcretePayloadForRowEquation(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished concrete checked type payload"),
            .flex => |flex| .{ .flex = try self.cloneConcreteVariableForRowEquationPayload(ref, flex) },
            .rigid => |rigid| .{ .rigid = try self.cloneConcreteVariableForRowEquationPayload(ref, rigid) },
            .alias => |alias| .{ .alias = .{
                .name = try self.typeNameForConcreteRef(ref, alias.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, alias.origin_module),
                .backing = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(ref, alias.backing)),
                .args = try self.cloneConcreteTypeIdsForRowEquation(ref, alias.args),
            } },
            .record_unbound => |fields| .{ .record = .{
                .fields = try self.cloneConcreteRecordFieldsForRowEquation(ref, fields),
                .ext = try self.materializeSyntheticPayload(.{ .flex = .{} }),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.cloneConcreteRecordFieldsForRowEquation(ref, record.fields),
                .ext = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(ref, record.ext)),
            } },
            .tuple => |items| .{ .tuple = try self.cloneConcreteTypeIdsForRowEquation(ref, items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.typeNameForConcreteRef(ref, nominal.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(ref, nominal.backing)),
                .representation = nominal.representation,
                .args = try self.cloneConcreteTypeIdsForRowEquation(ref, nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = checked_artifact.finalizedFunctionKind(func.kind),
                .args = try self.cloneConcreteTypeIdsForRowEquation(ref, func.args),
                .ret = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(ref, func.ret)),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.cloneConcreteTagsForRowEquation(ref, tag_union.tags),
                .ext = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(ref, tag_union.ext)),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn cloneTemplateTypeIdsForRowEquation(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.cloneTemplateTypeForRowEquation(id);
        }
        return out;
    }

    fn cloneConcreteTypeIdsForRowEquation(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(parent, id));
        }
        return out;
    }

    fn cloneTemplateRecordFieldsForRowEquation(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.cloneTemplateTypeForRowEquation(field.ty),
            };
        }
        return out;
    }

    fn cloneConcreteRecordFieldsForRowEquation(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.recordFieldNameForConcreteRef(parent, field.name),
                .ty = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(parent, field.ty)),
            };
        }
        return out;
    }

    fn cloneTemplateTagsForRowEquation(
        self: *TypeInstantiator,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.name_resolver.tagLabel(self.template_artifact, tag.name),
                .args = try self.cloneTemplateTypeIdsForRowEquation(tag.args),
            };
        }
        return out;
    }

    fn cloneConcreteTagsForRowEquation(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.tagNameForConcreteRef(parent, tag.name),
                .args = try self.cloneConcreteTypeIdsForRowEquation(parent, tag.args),
            };
        }
        return out;
    }

    fn cloneTemplateVariableForRowEquationPayload(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeVariable {
        return .{
            .name = try self.copyOptionalText(variable.name),
            .constraints = try self.cloneTemplateConstraintsForRowEquation(variable.constraints),
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn cloneConcreteVariableForRowEquationPayload(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeVariable {
        return .{
            .name = try self.copyOptionalText(variable.name),
            .constraints = try self.cloneConcreteConstraintsForRowEquation(ref, variable.constraints),
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn copyTemplateVariable(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeVariable {
        return .{
            .name = try self.copyOptionalText(variable.name),
            .constraints = &.{},
            .numeric_default_phase = variable.numeric_default_phase,
        };
    }

    fn cloneTemplateConstraintsForRowEquation(
        self: *TypeInstantiator,
        constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
    ) Allocator.Error![]const checked_artifact.CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        var out = std.ArrayList(checked_artifact.CheckedStaticDispatchConstraint).empty;
        errdefer out.deinit(self.allocator);
        const is_eq = try self.program.canonical_names.internMethodName("is_eq");
        for (constraints) |constraint| {
            const method = try self.name_resolver.methodName(self.template_artifact, constraint.fn_name);
            if (method == is_eq) continue;
            try out.append(self.allocator, .{
                .fn_name = method,
                .fn_ty = try self.cloneTemplateTypeForRowEquation(constraint.fn_ty),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            });
        }
        return try out.toOwnedSlice(self.allocator);
    }

    fn cloneConcreteConstraintsForRowEquation(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
    ) Allocator.Error![]const checked_artifact.CheckedStaticDispatchConstraint {
        if (constraints.len == 0) return &.{};
        var out = std.ArrayList(checked_artifact.CheckedStaticDispatchConstraint).empty;
        errdefer out.deinit(self.allocator);
        const is_eq = try self.program.canonical_names.internMethodName("is_eq");
        for (constraints) |constraint| {
            const method = try self.methodNameForConcreteRef(ref, constraint.fn_name);
            if (method == is_eq) continue;
            try out.append(self.allocator, .{
                .fn_name = method,
                .fn_ty = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(ref, constraint.fn_ty)),
                .origin = constraint.origin,
                .binop_negated = constraint.binop_negated,
                .num_literal = constraint.num_literal,
            });
        }
        return try out.toOwnedSlice(self.allocator);
    }

    fn copyOptionalText(self: *TypeInstantiator, maybe_text: ?[]const u8) Allocator.Error!?[]const u8 {
        if (maybe_text) |text| return try self.allocator.dupe(u8, text);
        return null;
    }

    fn materializeTemplateType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        if (self.substitutions.get(id)) |concrete| return try self.materializeConcreteRef(concrete);
        if (self.materialized_template_roots.get(id)) |existing| return existing;

        switch (self.templatePayload(id)) {
            .flex => |flex| return try self.materializeTemplateVariableType(id, .{ .flex = try self.copyTemplateVariable(flex) }),
            .rigid => |rigid| return try self.materializeTemplateVariableType(id, .{ .rigid = try self.copyTemplateVariable(rigid) }),
            else => {},
        }

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_template_roots.put(id, local_root);
        errdefer _ = self.materialized_template_roots.remove(id);

        const payload = try self.materializeTemplatePayload(self.templatePayload(id));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealReachableMaterializedLocalGraph(local_root);
        return local_root;
    }

    fn materializeTemplateVariableType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_template_roots.put(id, local_root);
        errdefer _ = self.materialized_template_roots.remove(id);

        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        const concrete = try self.sealMaterializedLocalRootRef(local_root);
        try self.substitutions.put(id, concrete);
        self.clearLoweredTypeCaches();
        return local_root;
    }

    fn materializeTemplatePayload(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished checked type payload"),
            .flex => |flex| .{ .flex = try self.copyTemplateVariable(flex) },
            .rigid => |rigid| .{ .rigid = try self.copyTemplateVariable(rigid) },
            .alias => |alias| .{ .alias = .{
                .name = try self.name_resolver.typeName(self.template_artifact, alias.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, alias.origin_module),
                .backing = try self.materializeTemplateType(alias.backing),
                .args = try self.materializeTemplateTypeIds(alias.args),
            } },
            .record_unbound => |fields| .{ .record = .{
                .fields = try self.materializeTemplateRecordFields(fields),
                .ext = try self.materializeSyntheticPayload(.empty_record),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.materializeTemplateRecordFields(record.fields),
                .ext = try self.materializeTemplateRecordExt(record.ext),
            } },
            .tuple => |items| .{ .tuple = try self.materializeTemplateTypeIds(items) },
            .nominal => |nominal| try self.materializeTemplateNominalPayload(nominal),
            .function => |func| .{ .function = .{
                .kind = checked_artifact.finalizedFunctionKind(func.kind),
                .args = try self.materializeTemplateTypeIds(func.args),
                .ret = try self.materializeTemplateType(func.ret),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| blk: {
                const tags = try self.materializeTemplateTags(tag_union.tags);
                errdefer {
                    for (tags) |tag| self.allocator.free(tag.args);
                    self.allocator.free(tags);
                }
                const ext = try self.materializeTemplateTagUnionExt(tag_union.ext);
                break :blk .{ .tag_union = .{
                    .tags = tags,
                    .ext = ext,
                } };
            },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn materializeTemplateRecordExt(
        self: *TypeInstantiator,
        ext: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        if (self.substitutions.get(ext)) |concrete| return try self.materializeConcreteRecordExt(concrete);
        return switch (self.templatePayload(ext)) {
            .flex => |flex| try self.materializeEmptyRecordRowTail(flex),
            .rigid => |rigid| try self.materializeEmptyRecordRowTail(rigid),
            else => try self.materializeTemplateType(ext),
        };
    }

    fn materializeTemplateTagUnionExt(
        self: *TypeInstantiator,
        ext: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        if (self.substitutions.get(ext)) |concrete| return try self.materializeConcreteTagUnionExt(concrete);
        return switch (self.templatePayload(ext)) {
            .flex => |flex| try self.materializeEmptyTagUnionRowTail(flex),
            .rigid => |rigid| try self.materializeEmptyTagUnionRowTail(rigid),
            else => try self.materializeTemplateType(ext),
        };
    }

    fn materializeTemplateNominalPayload(
        self: *TypeInstantiator,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        const args = try self.materializeTemplateTypeIds(nominal.args);
        errdefer self.allocator.free(args);

        const backing = try self.materializeTemplateNominalBacking(nominal);

        return .{ .nominal = .{
            .name = try self.name_resolver.typeName(self.template_artifact, nominal.name),
            .origin_module = try self.name_resolver.moduleName(self.template_artifact, nominal.origin_module),
            .builtin = nominal.builtin,
            .is_opaque = nominal.is_opaque,
            .backing = backing,
            .representation = nominal.representation,
            .args = args,
        } };
    }

    fn materializeTemplateNominalBacking(
        self: *TypeInstantiator,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return switch (nominal.representation) {
            .builtin,
            .local_declaration,
            .imported_declaration,
            => try self.materializeTemplateType(nominal.backing),
            .local_box_payload_capability => |capability_ref| blk: {
                const capabilities = interfaceCapabilitiesForKey(self.input, self.template_artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: template nominal artifact had no interface capabilities");
                    unreachable;
                };
                const capability = capabilities.boxPayloadCapability(capability_ref.capability);
                break :blk try self.materializeTemplateType(capability.backing_ty);
            },
            .imported_box_payload_capability => |capability_ref| blk: {
                const capabilities = interfaceCapabilitiesForKey(self.input, capability_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: imported nominal capability artifact was not available");
                    unreachable;
                };
                const capability = capabilities.boxPayloadCapability(capability_ref.capability);
                break :blk try self.materializeTemplateType(capability.backing_ty);
            },
            .opaque_without_backing => invariantViolation("mono specialization attempted to materialize an opaque nominal backing without representation authority"),
        };
    }

    fn materializeTemplateTypeIds(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.materializeTemplateType(id);
        }
        return out;
    }

    fn materializeTemplateRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.materializeTemplateType(field.ty),
            };
        }
        return out;
    }

    fn materializeTemplateTags(
        self: *TypeInstantiator,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.name_resolver.tagLabel(self.template_artifact, tag.name),
                .args = try self.materializeTemplateTypeIds(tag.args),
            };
        }
        return out;
    }

    fn nameSortContext(self: *const TypeInstantiator) CanonicalNameSortContext {
        return .{ .names = &self.program.canonical_names };
    }

    fn collectConcreteRecordFields(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(Type.Field),
    ) Allocator.Error!void {
        const payload = self.concretePayload(ref);
        switch (payload) {
            .alias => |alias| try self.collectConcreteRecordFields(try self.concreteAliasBackingRef(ref, alias), out),
            .empty_record => {},
            .flex => |flex| try self.verifyClosableRowTail(flex),
            .record_unbound => |fields| {
                for (fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.recordFieldNameForConcreteRef(ref, field.name),
                        .ty = try self.lowerConcreteRef(try self.concreteChildRef(ref, field.ty)),
                    });
                }
            },
            .record => |record| {
                for (record.fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.recordFieldNameForConcreteRef(ref, field.name),
                        .ty = try self.lowerConcreteRef(try self.concreteChildRef(ref, field.ty)),
                    });
                }
                try self.collectConcreteRecordFields(try self.concreteChildRef(ref, record.ext), out);
            },
            else => invariantViolation("mono specialization concrete record extension resolved to a non-record type"),
        }
    }

    fn collectConcreteTags(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(Type.Tag),
    ) Allocator.Error!void {
        const payload = self.concretePayload(ref);
        switch (payload) {
            .alias => |alias| try self.collectConcreteTags(try self.concreteAliasBackingRef(ref, alias), out),
            .empty_tag_union => {},
            .flex => |flex| try self.verifyClosableRowTail(flex),
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    const name = try self.tagNameForConcreteRef(ref, tag.name);
                    if (containsTagName(out.items, name)) continue;
                    try out.append(self.allocator, .{
                        .name = name,
                        .args = try self.lowerConcreteTypeIds(ref, tag.args),
                    });
                }
                try self.collectConcreteTags(try self.concreteChildRef(ref, tag_union.ext), out);
            },
            .nominal => |nominal| try self.collectConcreteTags(try self.concreteNominalBackingRef(ref, nominal), out),
            else => invariantViolation("mono specialization concrete tag-union extension resolved to a non-tag-union type"),
        }
    }

    fn containsTagName(tags: []const Type.Tag, name: canonical.TagLabelId) bool {
        for (tags) |tag| {
            if (tag.name == name) return true;
        }
        return false;
    }

    fn lowerConcreteTypeIds(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const Type.TypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.lowerConcreteRef(try self.concreteChildRef(parent, id));
        }
        return out;
    }

    fn lowerArtifactRef(
        self: *TypeInstantiator,
        ref: checked_artifact.ArtifactCheckedTypeRef,
    ) Allocator.Error!Type.TypeId {
        const checked_types = checkedTypesForKey(self.input, ref.artifact) orelse {
            debug.invariant(false, "mono specialization invariant violated: concrete type ref artifact was not available");
            unreachable;
        };
        const concrete_ref = try self.program.concrete_source_types.registerArtifactRoot(
            ref.artifact,
            checked_types,
            ref.ty,
        );
        return try self.lowerConcreteRef(concrete_ref);
    }

    fn lowerConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Type.TypeId {
        const resolved = self.resolveConcreteRef(ref);
        if (self.lowered_concrete.get(resolved)) |existing| return existing;

        const placeholder = try self.program.types.addType(.placeholder);
        try self.lowered_concrete.put(resolved, placeholder);
        const lowered = try self.lowerConcretePayload(resolved, self.concretePayload(resolved));
        self.program.types.setType(placeholder, lowered);
        self.program.types.debugValidateTypeGraph(placeholder);
        return try self.program.types.internTypeId(placeholder);
    }

    fn lowerConcretePayload(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished concrete checked type payload"),
            .flex => |flex| try self.lowerClosableConcreteVariableType(ref, flex),
            .rigid => |rigid| try self.lowerClosableConcreteVariableType(ref, rigid),
            .alias => |alias| .{ .link = try self.lowerConcreteRef(try self.concreteAliasBackingRef(ref, alias)) },
            .record_unbound,
            .record,
            .empty_record,
            => try self.lowerConcreteRecord(ref),
            .tuple => |items| .{ .tuple = try self.lowerConcreteTypeIds(ref, items) },
            .nominal => |nominal| try self.lowerConcreteNominal(ref, nominal),
            .function => |func| .{ .func = .{
                .args = try self.lowerConcreteTypeIds(ref, func.args),
                .lambdas = &.{},
                .ret = try self.lowerConcreteRef(try self.concreteChildRef(ref, func.ret)),
            } },
            .tag_union,
            .empty_tag_union,
            => try self.lowerConcreteTagUnion(ref),
        };
    }

    fn lowerConcreteRecord(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Type.Content {
        var fields = std.ArrayList(Type.Field).empty;
        errdefer fields.deinit(self.allocator);
        try self.collectConcreteRecordFields(ref, &fields);
        std.mem.sort(Type.Field, fields.items, self.nameSortContext(), typeFieldNameLessThan);
        return .{ .record = .{ .fields = try fields.toOwnedSlice(self.allocator) } };
    }

    fn lowerConcreteTagUnion(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Type.Content {
        var tags = std.ArrayList(Type.Tag).empty;
        errdefer {
            for (tags.items) |tag| self.allocator.free(tag.args);
            tags.deinit(self.allocator);
        }
        try self.collectConcreteTags(ref, &tags);
        std.mem.sort(Type.Tag, tags.items, self.nameSortContext(), typeTagNameLessThan);
        return .{ .tag_union = .{ .tags = try tags.toOwnedSlice(self.allocator) } };
    }

    fn lowerConcreteNominal(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!Type.Content {
        if (nominal.builtin) |builtin_nominal| {
            switch (builtin_nominal) {
                .bool => return .{ .link = try self.lowerConcreteRef(try self.concreteChildRef(ref, nominal.backing)) },
                .str => return .{ .primitive = .str },
                .u8 => return .{ .primitive = .u8 },
                .i8 => return .{ .primitive = .i8 },
                .u16 => return .{ .primitive = .u16 },
                .i16 => return .{ .primitive = .i16 },
                .u32 => return .{ .primitive = .u32 },
                .i32 => return .{ .primitive = .i32 },
                .u64 => return .{ .primitive = .u64 },
                .i64 => return .{ .primitive = .i64 },
                .u128 => return .{ .primitive = .u128 },
                .i128 => return .{ .primitive = .i128 },
                .f32 => return .{ .primitive = .f32 },
                .f64 => return .{ .primitive = .f64 },
                .dec => return .{ .primitive = .dec },
                .list => {
                    if (nominal.args.len != 1) invariantViolation("List nominal type did not have exactly one argument");
                    return .{ .list = try self.lowerConcreteRef(try self.concreteChildRef(ref, nominal.args[0])) };
                },
                .box => {
                    if (nominal.args.len != 1) invariantViolation("Box nominal type did not have exactly one argument");
                    return .{ .box = try self.lowerConcreteRef(try self.concreteChildRef(ref, nominal.args[0])) };
                },
            }
        }

        const source_ref = try self.runtimeConcreteRef(ref);
        return .{ .nominal = .{
            .nominal = .{
                .module_name = try self.moduleNameForConcreteRef(ref, nominal.origin_module),
                .type_name = try self.typeNameForConcreteRef(ref, nominal.name),
            },
            .source_ty = self.program.concrete_source_types.key(source_ref),
            .source_ty_payload = source_ref,
            .is_opaque = nominal.is_opaque,
            .args = try self.lowerConcreteTypeIds(ref, nominal.args),
            .backing = try self.lowerConcreteRef(try self.concreteNominalBackingRef(ref, nominal)),
        } };
    }

    fn unifyTemplateWithConcrete(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        switch (self.templatePayload(template_id)) {
            .flex, .rigid => {
                const resolved_concrete = self.resolveConcreteRef(concrete);
                switch (self.concretePayload(resolved_concrete)) {
                    .flex, .rigid => {
                        if (self.substitutions.get(template_id)) |existing| {
                            try self.bindConcreteVariable(resolved_concrete, existing);
                        } else {
                            if (self.templateVariableAcceptsDelayedNumericDefault(template_id)) {
                                const numeric_ref = try self.concreteRefForTemplateTypePreservingVariables(template_id);
                                try self.bindConcreteVariable(resolved_concrete, numeric_ref);
                                return;
                            }
                            try self.bindTemplateVariable(template_id, resolved_concrete);
                        }
                        return;
                    },
                    else => {},
                }
                try self.bindTemplateVariable(template_id, concrete);
                return;
            },
            .alias => |alias| {
                try self.unifyTemplateWithConcrete(alias.backing, concrete);
                return;
            },
            else => {},
        }

        switch (self.concretePayload(concrete)) {
            .flex, .rigid => {
                try self.bindConcreteVariable(concrete, try self.concreteRefForTemplateTypePreservingVariables(template_id));
                return;
            },
            .alias => |alias| {
                try self.unifyTemplateWithConcrete(template_id, try self.concreteAliasBackingRef(concrete, alias));
                return;
            },
            else => {},
        }

        try self.unifyConcretePayload(template_id, concrete);
    }

    fn unifyConcretePayload(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const template = self.templatePayload(template_id);
        const concrete_payload = self.concretePayload(concrete);
        switch (template) {
            .record => switch (concrete_payload) {
                .record, .record_unbound, .empty_record, .nominal => {
                    try self.unifyConcreteRefs(try self.concreteRefForTemplateTypePreservingVariables(template_id), concrete);
                },
                else => invariantViolation("mono specialization expected a concrete record"),
            },
            .record_unbound => switch (concrete_payload) {
                .record, .record_unbound, .empty_record => {
                    try self.unifyConcreteRefs(try self.concreteRefForTemplateTypePreservingVariables(template_id), concrete);
                },
                .nominal => |nominal| try self.unifyTemplateWithConcrete(template_id, try self.concreteNominalBackingRef(concrete, nominal)),
                else => invariantViolation("mono specialization expected a concrete record"),
            },
            .tuple => |items| switch (concrete_payload) {
                .tuple => |concrete_items| try self.unifyTypeLists(items, concrete, concrete_items),
                else => invariantViolation("mono specialization expected a concrete tuple"),
            },
            .nominal => |nominal| switch (concrete_payload) {
                .nominal => |concrete_nominal| try self.unifyNominals(nominal, concrete, concrete_nominal),
                else => try self.unifyTemplateWithConcrete(nominal.backing, concrete),
            },
            .function => |func| switch (concrete_payload) {
                .function => |concrete_func| {
                    try self.unifyTypeLists(func.args, concrete, concrete_func.args);
                    try self.unifyTemplateWithConcrete(func.ret, try self.concreteChildRef(concrete, concrete_func.ret));
                },
                else => invariantViolation("mono specialization expected a concrete function"),
            },
            .empty_record => switch (concrete_payload) {
                .empty_record => {},
                .record, .record_unbound, .nominal => {
                    try self.unifyConcreteRefs(try self.concreteRefForTemplateTypePreservingVariables(template_id), concrete);
                },
                else => invariantViolation("mono specialization expected an empty concrete record"),
            },
            .tag_union => switch (concrete_payload) {
                .tag_union, .empty_tag_union, .nominal => {
                    try self.unifyConcreteRefs(try self.concreteRefForTemplateTypePreservingVariables(template_id), concrete);
                },
                else => invariantViolation("mono specialization expected a concrete tag union"),
            },
            .empty_tag_union => switch (concrete_payload) {
                .empty_tag_union => {},
                .tag_union, .nominal => {
                    try self.unifyConcreteRefs(try self.concreteRefForTemplateTypePreservingVariables(template_id), concrete);
                },
                else => invariantViolation("mono specialization expected an empty concrete tag union"),
            },
            .pending, .flex, .rigid, .alias => unreachable,
        }
    }

    fn unifyNominals(
        self: *TypeInstantiator,
        template: checked_artifact.CheckedNominalType,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: checked_artifact.CheckedNominalType,
    ) Allocator.Error!void {
        if (template.builtin != concrete.builtin or template.args.len != concrete.args.len) {
            invariantViolation("mono specialization nominal mismatch");
        }
        if (template.builtin == null) {
            const template_key = canonical.NominalTypeKey{
                .module_name = try self.name_resolver.moduleName(self.template_artifact, template.origin_module),
                .type_name = try self.name_resolver.typeName(self.template_artifact, template.name),
            };
            const concrete_key = try self.nominalKeyForConcreteRef(concrete_ref, concrete.origin_module, concrete.name);
            if (template_key.module_name != concrete_key.module_name or template_key.type_name != concrete_key.type_name) {
                invariantViolation("mono specialization nominal mismatch");
            }
        }
        try self.unifyTypeLists(template.args, concrete_ref, concrete.args);
    }

    fn unifyTypeLists(
        self: *TypeInstantiator,
        template: []const checked_artifact.CheckedTypeId,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error!void {
        if (template.len != concrete.len) invariantViolation("mono specialization type arity mismatch");
        for (template, concrete) |template_id, concrete_id| {
            try self.unifyTemplateWithConcrete(template_id, try self.concreteChildRef(concrete_ref, concrete_id));
        }
    }

    fn bindTemplateVariable(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const resolved_concrete = self.resolveConcreteRef(concrete);
        const delayed_numeric_default = self.templateVariableAcceptsDelayedNumericDefault(template_id) and
            try self.concreteRefIsBuiltinDec(resolved_concrete);
        if (self.substitutions.get(template_id)) |existing| {
            const resolved_existing = self.resolveConcreteRef(existing);
            const existing_key = self.program.concrete_source_types.key(resolved_existing);
            const concrete_key = self.program.concrete_source_types.key(resolved_concrete);
            if (!std.mem.eql(u8, &existing_key.bytes, &concrete_key.bytes)) {
                if (self.defaulted_numeric_substitutions.contains(template_id)) {
                    if (delayed_numeric_default) return;
                    try self.substitutions.put(template_id, resolved_concrete);
                    _ = self.defaulted_numeric_substitutions.remove(template_id);
                    self.clearDerivedTypeCaches();
                    return;
                }
                if (delayed_numeric_default) return;
                try self.unifyConcreteRefs(resolved_existing, resolved_concrete);
                if (self.preferredNominalBinding(resolved_existing, resolved_concrete)) |preferred| {
                    try self.substitutions.put(template_id, preferred);
                    self.clearDerivedTypeCaches();
                }
            }
            return;
        }
        try self.substitutions.put(template_id, resolved_concrete);
        self.clearDerivedTypeCaches();
        if (delayed_numeric_default) try self.defaulted_numeric_substitutions.put(template_id, {});
    }

    fn unifyConcreteRefs(
        self: *TypeInstantiator,
        lhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const lhs = self.resolveConcreteRef(lhs_ref);
        const rhs = self.resolveConcreteRef(rhs_ref);
        if (lhs == rhs) return;

        const lhs_payload = self.concretePayload(lhs);
        switch (lhs_payload) {
            .flex,
            .rigid,
            => {
                try self.bindConcreteVariable(lhs, rhs);
                return;
            },
            .alias => |alias| {
                try self.unifyConcreteRefs(try self.concreteAliasBackingRef(lhs, alias), rhs);
                return;
            },
            else => {},
        }

        const rhs_payload = self.concretePayload(rhs);
        switch (rhs_payload) {
            .flex,
            .rigid,
            => {
                try self.bindConcreteVariable(rhs, lhs);
                return;
            },
            .alias => |alias| {
                try self.unifyConcreteRefs(lhs, try self.concreteAliasBackingRef(rhs, alias));
                return;
            },
            else => {},
        }

        try self.unifyConcretePayloads(lhs, lhs_payload, rhs, rhs_payload);
    }

    fn transparentConcreteNominalBackingOrSelf(
        self: *TypeInstantiator,
        source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const source = self.resolveConcreteRef(source_ref);
        return switch (self.concretePayload(source)) {
            .alias => |alias| try self.transparentConcreteNominalBackingOrSelf(try self.concreteAliasBackingRef(source, alias)),
            .nominal => |nominal| if (nominal.builtin == null and !nominal.is_opaque)
                try self.concreteNominalBackingRef(source, nominal)
            else
                source,
            else => source,
        };
    }

    fn unifyConcreteRefsAllowingRequestedTagSuperset(
        self: *TypeInstantiator,
        template_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        requested_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const template = self.resolveConcreteRef(template_ref);
        if (template == self.resolveConcreteRef(requested_ref)) return;

        switch (self.concretePayload(template)) {
            .flex,
            .rigid,
            => return try self.bindConcreteVariable(template, requested_ref),
            .alias => |alias| return try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                try self.concreteAliasBackingRef(template, alias),
                requested_ref,
            ),
            .nominal => |nominal| {
                if (nominal.builtin != null or nominal.is_opaque) return try self.unifyConcreteRefs(template, requested_ref);
                return try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                    try self.concreteNominalBackingRef(template, nominal),
                    try self.transparentConcreteNominalBackingOrSelf(requested_ref),
                );
            },
            else => {},
        }

        const requested = self.resolveConcreteRef(requested_ref);
        switch (self.concretePayload(requested)) {
            .flex,
            .rigid,
            => return try self.bindConcreteVariable(requested, template),
            .alias => |alias| return try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                template,
                try self.concreteAliasBackingRef(requested, alias),
            ),
            .nominal => |nominal| {
                if (nominal.builtin != null or nominal.is_opaque) return try self.unifyConcreteRefs(template, requested);
                return try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                    template,
                    try self.concreteNominalBackingRef(requested, nominal),
                );
            },
            else => {},
        }

        const template_payload = self.concretePayload(template);
        const requested_payload = self.concretePayload(requested);
        switch (template_payload) {
            .tag_union,
            .empty_tag_union,
            => switch (requested_payload) {
                .tag_union,
                .empty_tag_union,
                => try self.unifyConcreteTagRowsAllowingRequestedSuperset(template, requested),
                else => try self.unifyConcreteRefs(template, requested),
            },
            .tuple => |items| switch (requested_payload) {
                .tuple => |requested_items| {
                    if (items.len != requested_items.len) invariantViolation("mono specialization concrete tuple shape mismatch");
                    for (items, requested_items) |item, requested_item| {
                        try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                            try self.concreteChildRef(template, item),
                            try self.concreteChildRef(requested, requested_item),
                        );
                    }
                },
                else => try self.unifyConcreteRefs(template, requested),
            },
            else => try self.unifyConcreteRefs(template, requested),
        }
    }

    fn unifyConcretePayloads(
        self: *TypeInstantiator,
        lhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        lhs: checked_artifact.CheckedTypePayload,
        rhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!void {
        switch (lhs) {
            .record => switch (rhs) {
                .record, .record_unbound, .empty_record => try self.unifyConcreteRecordRows(lhs_ref, rhs_ref),
                .nominal => |nominal| try self.unifyConcreteRefs(lhs_ref, try self.concreteNominalBackingRef(rhs_ref, nominal)),
                else => invariantViolation("mono specialization expected matching concrete records"),
            },
            .record_unbound => switch (rhs) {
                .record, .record_unbound, .empty_record => try self.unifyConcreteRecordRows(lhs_ref, rhs_ref),
                else => invariantViolation("mono specialization expected matching concrete record rows"),
            },
            .tuple => |items| switch (rhs) {
                .tuple => |rhs_items| try self.unifyConcreteTypeLists(lhs_ref, items, rhs_ref, rhs_items),
                .nominal => |nominal| try self.unifyConcreteRefs(lhs_ref, try self.concreteNominalBackingRef(rhs_ref, nominal)),
                else => invariantViolation("mono specialization expected matching concrete tuples"),
            },
            .nominal => |nominal| switch (rhs) {
                .nominal => |rhs_nominal| try self.unifyConcreteNominals(lhs_ref, nominal, rhs_ref, rhs_nominal),
                else => try self.unifyConcreteRefs(try self.concreteNominalBackingRef(lhs_ref, nominal), rhs_ref),
            },
            .function => |func| switch (rhs) {
                .function => |rhs_func| {
                    if (func.kind != rhs_func.kind or func.args.len != rhs_func.args.len) {
                        invariantViolation("mono specialization concrete function shape mismatch");
                    }
                    try self.unifyConcreteTypeLists(lhs_ref, func.args, rhs_ref, rhs_func.args);
                    try self.unifyConcreteRefs(
                        try self.concreteChildRef(lhs_ref, func.ret),
                        try self.concreteChildRef(rhs_ref, rhs_func.ret),
                    );
                },
                else => invariantViolation("mono specialization expected matching concrete functions"),
            },
            .empty_record => switch (rhs) {
                .empty_record => {},
                .record, .record_unbound => try self.unifyConcreteRecordRows(lhs_ref, rhs_ref),
                .nominal => |nominal| try self.unifyConcreteRefs(lhs_ref, try self.concreteNominalBackingRef(rhs_ref, nominal)),
                else => invariantViolation("mono specialization expected matching empty concrete records"),
            },
            .tag_union => switch (rhs) {
                .tag_union => try self.unifyConcreteTagRows(lhs_ref, rhs_ref),
                .empty_tag_union => try self.unifyConcreteTagRows(lhs_ref, rhs_ref),
                .nominal => |nominal| try self.unifyConcreteRefs(lhs_ref, try self.concreteNominalBackingRef(rhs_ref, nominal)),
                else => invariantViolation("mono specialization expected matching concrete tag unions"),
            },
            .empty_tag_union => switch (rhs) {
                .empty_tag_union => {},
                .tag_union => try self.unifyConcreteTagRows(lhs_ref, rhs_ref),
                .nominal => |nominal| try self.unifyConcreteRefs(lhs_ref, try self.concreteNominalBackingRef(rhs_ref, nominal)),
                else => invariantViolation("mono specialization expected matching empty concrete tag unions"),
            },
            .pending, .flex, .rigid, .alias => unreachable,
        }
    }

    fn preferredNominalBinding(
        self: *TypeInstantiator,
        lhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) ?ConcreteSourceType.ConcreteSourceTypeRef {
        const lhs = self.resolveConcreteRef(lhs_ref);
        if (self.concretePayload(lhs) == .nominal) return lhs;
        const rhs = self.resolveConcreteRef(rhs_ref);
        if (self.concretePayload(rhs) == .nominal) return rhs;
        return null;
    }

    fn unifyConcreteRecordRows(
        self: *TypeInstantiator,
        lhs_row: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs_row: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        var lhs_entries = std.ArrayList(ConcreteRecordFieldEntry).empty;
        defer lhs_entries.deinit(self.allocator);
        const lhs_tail = try self.collectConcreteRecordRowEntries(lhs_row, &lhs_entries);

        var rhs_entries = std.ArrayList(ConcreteRecordFieldEntry).empty;
        defer rhs_entries.deinit(self.allocator);
        const rhs_tail = try self.collectConcreteRecordRowEntries(rhs_row, &rhs_entries);

        const rhs_consumed = try self.allocator.alloc(bool, rhs_entries.items.len);
        defer self.allocator.free(rhs_consumed);
        @memset(rhs_consumed, false);

        var lhs_only = std.ArrayList(ConcreteRecordFieldEntry).empty;
        defer lhs_only.deinit(self.allocator);

        for (lhs_entries.items) |lhs_entry| {
            if (findUnconsumedConcreteRecordFieldEntry(rhs_entries.items, rhs_consumed, lhs_entry.name)) |rhs_index| {
                rhs_consumed[rhs_index] = true;
                const rhs_entry = rhs_entries.items[rhs_index];
                try self.unifyConcreteRefs(
                    try self.concreteChildRef(lhs_entry.owner, lhs_entry.ty),
                    try self.concreteChildRef(rhs_entry.owner, rhs_entry.ty),
                );
            } else {
                try lhs_only.append(self.allocator, lhs_entry);
            }
        }

        var rhs_only = std.ArrayList(ConcreteRecordFieldEntry).empty;
        defer rhs_only.deinit(self.allocator);
        for (rhs_entries.items, 0..) |rhs_entry, index| {
            if (!rhs_consumed[index]) try rhs_only.append(self.allocator, rhs_entry);
        }

        try self.reconcileConcreteRecordTails(lhs_tail, lhs_only.items, rhs_tail, rhs_only.items);
    }

    fn collectConcreteRecordRowEntries(
        self: *TypeInstantiator,
        row: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(ConcreteRecordFieldEntry),
    ) Allocator.Error!ConcreteRecordTail {
        var current = row;
        while (true) {
            switch (self.concretePayload(current)) {
                .alias => |alias| current = try self.concreteAliasBackingRef(current, alias),
                .nominal => |nominal| current = try self.concreteNominalBackingRef(current, nominal),
                .empty_record => return .{ .ref = current, .is_open = false },
                .record_unbound => {
                    current = try self.program.concrete_source_types.registerLocalRoot(
                        try self.cloneConcreteTypeForRowEquation(current),
                    );
                },
                .flex, .rigid => return .{ .ref = current, .is_open = true },
                .record => |record| {
                    for (record.fields) |field| {
                        try out.append(self.allocator, .{
                            .name = try self.recordFieldNameForConcreteRef(current, field.name),
                            .owner = current,
                            .ty = field.ty,
                        });
                    }
                    current = try self.concreteChildRef(current, record.ext);
                },
                else => invariantViolation("mono specialization concrete record row resolved to a non-record type"),
            }
        }
    }

    fn reconcileConcreteRecordTails(
        self: *TypeInstantiator,
        lhs_tail: ConcreteRecordTail,
        lhs_only: []const ConcreteRecordFieldEntry,
        rhs_tail: ConcreteRecordTail,
        rhs_only: []const ConcreteRecordFieldEntry,
    ) Allocator.Error!void {
        if (!lhs_tail.is_open and rhs_only.len != 0) {
            invariantViolation("mono specialization closed concrete record row was missing required fields");
        }
        if (!rhs_tail.is_open and lhs_only.len != 0) {
            invariantViolation("mono specialization closed concrete record row was missing required fields");
        }

        if (lhs_tail.is_open and rhs_tail.is_open) {
            if (lhs_only.len == 0 and rhs_only.len == 0) {
                try self.unifyConcreteRefs(lhs_tail.ref, rhs_tail.ref);
                return;
            }
            const shared_tail = try self.freshConcreteRecordTailRef();
            try self.bindOpenConcreteRecordTail(lhs_tail.ref, rhs_only, shared_tail);
            try self.bindOpenConcreteRecordTail(rhs_tail.ref, lhs_only, shared_tail);
            return;
        }

        if (lhs_tail.is_open) {
            try self.bindOpenConcreteRecordTail(lhs_tail.ref, rhs_only, rhs_tail.ref);
        }
        if (rhs_tail.is_open) {
            try self.bindOpenConcreteRecordTail(rhs_tail.ref, lhs_only, lhs_tail.ref);
        }
    }

    fn bindOpenConcreteRecordTail(
        self: *TypeInstantiator,
        tail: ConcreteSourceType.ConcreteSourceTypeRef,
        entries: []const ConcreteRecordFieldEntry,
        residual_tail: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        if (entries.len == 0) {
            try self.bindConcreteVariable(tail, residual_tail);
            return;
        }
        try self.bindConcreteVariable(tail, try self.buildConcreteRecordRow(entries, residual_tail));
    }

    fn buildConcreteRecordRow(
        self: *TypeInstantiator,
        entries: []const ConcreteRecordFieldEntry,
        residual_tail: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const fields = try self.allocator.alloc(checked_artifact.CheckedRecordField, entries.len);
        errdefer self.allocator.free(fields);
        for (entries, 0..) |entry, i| {
            fields[i] = .{
                .name = entry.name,
                .ty = try self.cloneConcreteTypeForRowEquation(try self.concreteChildRef(entry.owner, entry.ty)),
            };
        }
        std.mem.sort(checked_artifact.CheckedRecordField, fields, self.nameSortContext(), checkedRecordFieldNameLessThan);

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        self.program.concrete_source_types.fillLocalRoot(local_root, .{ .record = .{
            .fields = fields,
            .ext = try self.cloneConcreteTypeForRowEquation(residual_tail),
        } });
        return try self.sealMaterializedLocalRootRef(local_root);
    }

    fn freshConcreteRecordTailRef(
        self: *TypeInstantiator,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        return try self.materializeSyntheticPayloadRef(.{ .flex = .{} });
    }

    fn findUnconsumedConcreteRecordFieldEntry(
        entries: []const ConcreteRecordFieldEntry,
        consumed: []const bool,
        name: canonical.RecordFieldLabelId,
    ) ?usize {
        for (entries, 0..) |entry, index| {
            if (!consumed[index] and entry.name == name) return index;
        }
        return null;
    }

    fn unifyConcreteNominals(
        self: *TypeInstantiator,
        lhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        lhs: checked_artifact.CheckedNominalType,
        rhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs: checked_artifact.CheckedNominalType,
    ) Allocator.Error!void {
        if (lhs.builtin != rhs.builtin or lhs.args.len != rhs.args.len) {
            invariantViolation("mono specialization concrete nominal mismatch");
        }
        if (lhs.builtin == null) {
            const lhs_key = try self.nominalKeyForConcreteRef(lhs_ref, lhs.origin_module, lhs.name);
            const rhs_key = try self.nominalKeyForConcreteRef(rhs_ref, rhs.origin_module, rhs.name);
            if (lhs_key.module_name != rhs_key.module_name or lhs_key.type_name != rhs_key.type_name) {
                invariantViolation("mono specialization concrete nominal mismatch");
            }
        }
        try self.unifyConcreteTypeLists(lhs_ref, lhs.args, rhs_ref, rhs.args);
    }

    fn unifyConcreteTagRows(
        self: *TypeInstantiator,
        lhs_row: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs_row: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        var lhs_entries = std.ArrayList(ConcreteTagEntry).empty;
        defer lhs_entries.deinit(self.allocator);
        const lhs_tail = try self.collectConcreteTagEntries(lhs_row, &lhs_entries);

        var rhs_entries = std.ArrayList(ConcreteTagEntry).empty;
        defer rhs_entries.deinit(self.allocator);
        const rhs_tail = try self.collectConcreteTagEntries(rhs_row, &rhs_entries);

        const rhs_consumed = try self.allocator.alloc(bool, rhs_entries.items.len);
        defer self.allocator.free(rhs_consumed);
        @memset(rhs_consumed, false);

        var lhs_only = std.ArrayList(ConcreteTagEntry).empty;
        defer lhs_only.deinit(self.allocator);

        for (lhs_entries.items) |lhs_entry| {
            if (findUnconsumedConcreteTagEntry(rhs_entries.items, rhs_consumed, lhs_entry.name)) |rhs_index| {
                rhs_consumed[rhs_index] = true;
                const rhs_entry = rhs_entries.items[rhs_index];
                try self.unifyConcreteTypeLists(lhs_entry.owner, lhs_entry.tag.args, rhs_entry.owner, rhs_entry.tag.args);
            } else {
                try lhs_only.append(self.allocator, lhs_entry);
            }
        }

        var rhs_only = std.ArrayList(ConcreteTagEntry).empty;
        defer rhs_only.deinit(self.allocator);
        for (rhs_entries.items, 0..) |rhs_entry, index| {
            if (!rhs_consumed[index]) try rhs_only.append(self.allocator, rhs_entry);
        }

        try self.reconcileConcreteTagTails(lhs_tail, lhs_only.items, rhs_tail, rhs_only.items);
    }

    fn unifyConcreteTagRowsAllowingRequestedSuperset(
        self: *TypeInstantiator,
        template_row: ConcreteSourceType.ConcreteSourceTypeRef,
        requested_row: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        var template_entries = std.ArrayList(ConcreteTagEntry).empty;
        defer template_entries.deinit(self.allocator);
        const template_tail = try self.collectConcreteTagEntries(template_row, &template_entries);

        var requested_entries = std.ArrayList(ConcreteTagEntry).empty;
        defer requested_entries.deinit(self.allocator);
        const requested_tail = try self.collectConcreteTagEntries(requested_row, &requested_entries);

        const requested_consumed = try self.allocator.alloc(bool, requested_entries.items.len);
        defer self.allocator.free(requested_consumed);
        @memset(requested_consumed, false);

        var template_only = std.ArrayList(ConcreteTagEntry).empty;
        defer template_only.deinit(self.allocator);

        for (template_entries.items) |template_entry| {
            if (findUnconsumedConcreteTagEntry(requested_entries.items, requested_consumed, template_entry.name)) |requested_index| {
                requested_consumed[requested_index] = true;
                const requested_entry = requested_entries.items[requested_index];
                if (template_entry.tag.args.len != requested_entry.tag.args.len) {
                    invariantViolation("mono specialization concrete tag payload arity mismatch");
                }
                for (template_entry.tag.args, requested_entry.tag.args) |template_arg, requested_arg| {
                    try self.unifyConcreteRefsAllowingRequestedTagSuperset(
                        try self.concreteChildRef(template_entry.owner, template_arg),
                        try self.concreteChildRef(requested_entry.owner, requested_arg),
                    );
                }
            } else {
                try template_only.append(self.allocator, template_entry);
            }
        }

        var requested_only = std.ArrayList(ConcreteTagEntry).empty;
        defer requested_only.deinit(self.allocator);
        for (requested_entries.items, 0..) |requested_entry, index| {
            if (!requested_consumed[index]) try requested_only.append(self.allocator, requested_entry);
        }

        try self.reconcileConcreteTagTailsAllowingRequestedSuperset(
            template_tail,
            template_only.items,
            requested_tail,
            requested_only.items,
        );
    }

    fn collectConcreteTagEntries(
        self: *TypeInstantiator,
        row: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(ConcreteTagEntry),
    ) Allocator.Error!ConcreteTagTail {
        var current = row;
        while (true) {
            switch (self.concretePayload(current)) {
                .alias => |alias| current = try self.concreteAliasBackingRef(current, alias),
                .nominal => |nominal| current = try self.concreteNominalBackingRef(current, nominal),
                .empty_tag_union => return .{ .ref = current, .is_open = false },
                .flex, .rigid => return .{ .ref = current, .is_open = true },
                .tag_union => |tag_union| {
                    for (tag_union.tags) |tag| {
                        try out.append(self.allocator, .{
                            .name = try self.tagNameForConcreteRef(current, tag.name),
                            .owner = current,
                            .tag = tag,
                        });
                    }
                    current = try self.concreteChildRef(current, tag_union.ext);
                },
                else => invariantViolation("mono specialization concrete tag row resolved to a non-tag-union type"),
            }
        }
    }

    fn reconcileConcreteTagTails(
        self: *TypeInstantiator,
        lhs_tail: ConcreteTagTail,
        lhs_only: []const ConcreteTagEntry,
        rhs_tail: ConcreteTagTail,
        rhs_only: []const ConcreteTagEntry,
    ) Allocator.Error!void {
        if (!lhs_tail.is_open and rhs_only.len != 0) {
            invariantViolation("mono specialization closed concrete tag row was missing required tags");
        }
        if (!rhs_tail.is_open and lhs_only.len != 0) {
            invariantViolation("mono specialization closed concrete tag row was missing required tags");
        }

        if (lhs_tail.is_open and rhs_tail.is_open) {
            if (lhs_only.len == 0 and rhs_only.len == 0) {
                try self.unifyConcreteRefs(lhs_tail.ref, rhs_tail.ref);
                return;
            }
            const shared_tail = try self.freshConcreteTagTailRef();
            try self.bindOpenConcreteTagTail(lhs_tail.ref, rhs_only, shared_tail);
            try self.bindOpenConcreteTagTail(rhs_tail.ref, lhs_only, shared_tail);
            return;
        }

        if (lhs_tail.is_open) {
            try self.bindOpenConcreteTagTail(lhs_tail.ref, rhs_only, rhs_tail.ref);
        }
        if (rhs_tail.is_open) {
            try self.bindOpenConcreteTagTail(rhs_tail.ref, lhs_only, lhs_tail.ref);
        }
    }

    fn reconcileConcreteTagTailsAllowingRequestedSuperset(
        self: *TypeInstantiator,
        template_tail: ConcreteTagTail,
        template_only: []const ConcreteTagEntry,
        requested_tail: ConcreteTagTail,
        requested_only: []const ConcreteTagEntry,
    ) Allocator.Error!void {
        if (!requested_tail.is_open and template_only.len != 0) {
            invariantViolation("mono specialization requested return tag row was missing app-produced tags");
        }

        if (template_tail.is_open and requested_tail.is_open) {
            if (template_only.len == 0 and requested_only.len == 0) {
                try self.unifyConcreteRefs(template_tail.ref, requested_tail.ref);
                return;
            }
            const shared_tail = try self.freshConcreteTagTailRef();
            try self.bindOpenConcreteTagTail(template_tail.ref, requested_only, shared_tail);
            try self.bindOpenConcreteTagTail(requested_tail.ref, template_only, shared_tail);
            return;
        }

        if (template_tail.is_open) {
            try self.bindOpenConcreteTagTail(template_tail.ref, requested_only, requested_tail.ref);
        }
        if (requested_tail.is_open) {
            try self.bindOpenConcreteTagTail(requested_tail.ref, template_only, template_tail.ref);
        }
    }

    fn bindOpenConcreteTagTail(
        self: *TypeInstantiator,
        tail: ConcreteSourceType.ConcreteSourceTypeRef,
        entries: []const ConcreteTagEntry,
        residual_tail: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        if (entries.len == 0) {
            try self.bindConcreteVariable(tail, residual_tail);
            return;
        }
        try self.bindConcreteVariable(tail, try self.buildConcreteTagRow(entries, residual_tail));
    }

    fn buildConcreteTagRow(
        self: *TypeInstantiator,
        entries: []const ConcreteTagEntry,
        residual_tail: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const tags = try self.allocator.alloc(checked_artifact.CheckedTag, entries.len);
        for (tags) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (tags) |tag| self.allocator.free(tag.args);
            self.allocator.free(tags);
        }
        for (entries, 0..) |entry, i| {
            tags[i] = .{
                .name = entry.name,
                .args = try self.cloneConcreteTypeIdsForRowEquation(entry.owner, entry.tag.args),
            };
        }
        std.mem.sort(checked_artifact.CheckedTag, tags, self.nameSortContext(), checkedTagNameLessThan);

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        self.program.concrete_source_types.fillLocalRoot(local_root, .{ .tag_union = .{
            .tags = tags,
            .ext = try self.cloneConcreteTypeForRowEquation(residual_tail),
        } });
        return try self.sealMaterializedLocalRootRef(local_root);
    }

    fn freshConcreteTagTailRef(
        self: *TypeInstantiator,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        self.program.concrete_source_types.fillLocalRoot(local_root, .{ .flex = .{} });
        return try self.sealMaterializedLocalRootRef(local_root);
    }

    fn findUnconsumedConcreteTagEntry(
        entries: []const ConcreteTagEntry,
        consumed: []const bool,
        name: canonical.TagLabelId,
    ) ?usize {
        for (entries, 0..) |entry, index| {
            if (!consumed[index] and entry.name == name) return index;
        }
        return null;
    }

    fn unifyConcreteTypeLists(
        self: *TypeInstantiator,
        lhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        lhs: []const checked_artifact.CheckedTypeId,
        rhs_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        rhs: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error!void {
        if (lhs.len != rhs.len) invariantViolation("mono specialization concrete type arity mismatch");
        for (lhs, rhs) |lhs_id, rhs_id| {
            try self.unifyConcreteRefs(
                try self.concreteChildRef(lhs_ref, lhs_id),
                try self.concreteChildRef(rhs_ref, rhs_id),
            );
        }
    }

    fn bindConcreteVariable(
        self: *TypeInstantiator,
        variable: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        const resolved_concrete = self.resolveConcreteRef(concrete);
        if (variable == resolved_concrete) return;
        if (self.concrete_variable_substitutions.get(variable)) |existing| {
            const resolved_existing = self.resolveConcreteRef(existing);
            const existing_key = self.program.concrete_source_types.key(resolved_existing);
            const concrete_key = self.program.concrete_source_types.key(resolved_concrete);
            if (!std.mem.eql(u8, &existing_key.bytes, &concrete_key.bytes)) {
                try self.unifyConcreteRefs(resolved_existing, resolved_concrete);
            }
            return;
        }
        try self.concrete_variable_substitutions.put(variable, resolved_concrete);
        self.clearDerivedTypeCaches();
    }

    fn absorbConcreteVariableSubstitutionsFrom(
        self: *TypeInstantiator,
        child: *TypeInstantiator,
    ) Allocator.Error!void {
        var substitutions = child.concrete_variable_substitutions.iterator();
        while (substitutions.next()) |entry| {
            try self.bindConcreteVariable(entry.key_ptr.*, entry.value_ptr.*);
        }
    }

    fn clearLoweredTypeCaches(self: *TypeInstantiator) void {
        self.lowered_concrete.clearRetainingCapacity();
        self.materialized_template_roots.clearRetainingCapacity();
        self.materialized_concrete_roots.clearRetainingCapacity();
        self.runtime_concrete_roots.clearRetainingCapacity();
        self.concrete_template_refs.clearRetainingCapacity();
    }

    fn clearDerivedTypeCaches(self: *TypeInstantiator) void {
        self.clearLoweredTypeCaches();
        self.row_clone_template_roots.clearRetainingCapacity();
        self.row_clone_concrete_roots.clearRetainingCapacity();
    }

    fn templateVariableAcceptsDelayedNumericDefault(
        self: *const TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
    ) bool {
        return switch (self.templatePayload(template_id)) {
            .flex => |flex| self.isMonoSpecializationNumericFlex(flex),
            .rigid => |rigid| self.isMonoSpecializationNumericFlex(rigid),
            else => false,
        };
    }

    fn concreteRefIsBuiltinDec(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!bool {
        var current = ref;
        while (true) {
            switch (self.concretePayload(current)) {
                .alias => |alias| current = try self.concreteAliasBackingRef(current, alias),
                .nominal => |nominal| return nominal.builtin == .dec,
                else => return false,
            }
        }
    }

    fn templatePayload(self: *const TypeInstantiator, id: checked_artifact.CheckedTypeId) checked_artifact.CheckedTypePayload {
        const template_types = self.templateTypes();
        const raw = @intFromEnum(id);
        if (raw >= template_types.payloads.len) invariantViolation("mono specialization template type id was outside published payloads");
        return template_types.payloads[raw];
    }

    fn templateTypes(self: *const TypeInstantiator) checked_artifact.CheckedTypeStoreView {
        return checkedTypesForKey(self.input, self.template_artifact) orelse {
            invariantViolation("mono specialization template artifact checked types were unavailable");
        };
    }

    fn resolveConcreteRef(
        self: *const TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) ConcreteSourceType.ConcreteSourceTypeRef {
        var current = ref;
        while (self.concrete_variable_substitutions.get(current)) |substitution| {
            current = substitution;
        }
        return current;
    }

    fn concretePayload(self: *const TypeInstantiator, ref: ConcreteSourceType.ConcreteSourceTypeRef) checked_artifact.CheckedTypePayload {
        const resolved = self.resolveConcreteRef(ref);
        const root = self.program.concrete_source_types.root(resolved);
        return switch (root.source) {
            .artifact => |artifact_ref| blk: {
                const checked_types = checkedTypesForKey(self.input, artifact_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: concrete type artifact was not available");
                    unreachable;
                };
                const raw = @intFromEnum(artifact_ref.ty);
                if (raw >= checked_types.payloads.len) invariantViolation("mono specialization concrete type id was outside published payloads");
                break :blk checked_types.payloads[raw];
            },
            .local => |local| blk: {
                const local_view = self.program.concrete_source_types.localView();
                const raw = @intFromEnum(local);
                if (raw >= local_view.payloads.len) invariantViolation("mono specialization local concrete type id was outside payloads");
                break :blk local_view.payloads[raw];
            },
        };
    }

    fn concreteChildRef(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        child: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const resolved = self.resolveConcreteRef(parent);
        const root = self.program.concrete_source_types.root(resolved);
        return switch (root.source) {
            .artifact => |artifact_ref| blk: {
                const checked_types = checkedTypesForKey(self.input, artifact_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: concrete child artifact was not available");
                    unreachable;
                };
                break :blk try self.program.concrete_source_types.registerArtifactRoot(
                    artifact_ref.artifact,
                    checked_types,
                    child,
                );
            },
            .local => try self.program.concrete_source_types.registerLocalRoot(child),
        };
    }

    fn concreteAliasBackingRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        alias: checked_artifact.CheckedAliasType,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        return try self.concreteChildRef(ref, alias.backing);
    }

    fn concreteNominalBackingRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const resolved = self.resolveConcreteRef(ref);
        const root = self.program.concrete_source_types.root(resolved);
        switch (root.source) {
            .local => return switch (nominal.representation) {
                .builtin,
                .local_declaration,
                .imported_declaration,
                => try self.concreteChildRef(resolved, nominal.backing),
                .local_box_payload_capability,
                .imported_box_payload_capability,
                => try self.concreteChildRef(resolved, nominal.backing),
                .opaque_without_backing => invariantViolation("mono specialization attempted to inspect an opaque local nominal backing"),
            },
            .artifact => |artifact_ref| return try self.artifactNominalCapabilityBackingRef(artifact_ref),
        }
    }

    fn artifactNominalCapabilityBackingRef(
        self: *TypeInstantiator,
        artifact_ref: checked_artifact.ArtifactCheckedTypeRef,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const checked_types = checkedTypesForKey(self.input, artifact_ref.artifact) orelse {
            debug.invariant(false, "mono specialization invariant violated: nominal backing artifact had no checked types");
            unreachable;
        };
        const payload_index: usize = @intFromEnum(artifact_ref.ty);
        if (payload_index >= checked_types.payloads.len) {
            invariantViolation("mono specialization nominal backing artifact ref was out of range");
        }
        const nominal = switch (checked_types.payloads[payload_index]) {
            .nominal => |payload| payload,
            else => invariantViolation("mono specialization nominal backing artifact ref was not nominal"),
        };
        const backing_artifact, const backing_ty = switch (nominal.representation) {
            .builtin,
            .local_declaration,
            .imported_declaration,
            => .{ artifact_ref.artifact, nominal.backing },
            .local_box_payload_capability => |capability_ref| blk: {
                const capabilities = interfaceCapabilitiesForKey(self.input, artifact_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: nominal capability artifact was not available");
                    unreachable;
                };
                const capability = capabilities.boxPayloadCapability(capability_ref.capability);
                break :blk .{ artifact_ref.artifact, capability.backing_ty };
            },
            .imported_box_payload_capability => |capability_ref| blk: {
                const capabilities = interfaceCapabilitiesForKey(self.input, capability_ref.artifact) orelse {
                    debug.invariant(false, "mono specialization invariant violated: imported nominal capability artifact was not available");
                    unreachable;
                };
                const capability = capabilities.boxPayloadCapability(capability_ref.capability);
                break :blk .{ capability_ref.artifact, capability.backing_ty };
            },
            .opaque_without_backing => invariantViolation("mono specialization attempted to inspect an opaque artifact nominal backing"),
        };
        const backing_checked_types = checkedTypesForKey(self.input, backing_artifact) orelse {
            debug.invariant(false, "mono specialization invariant violated: nominal backing owner artifact had no checked types");
            unreachable;
        };
        const backing = try self.program.concrete_source_types.registerArtifactRoot(
            backing_artifact,
            backing_checked_types,
            backing_ty,
        );
        return try self.runtimeConcreteRef(backing);
    }

    fn materializeConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const resolved = self.resolveConcreteRef(ref);
        const root = self.program.concrete_source_types.root(resolved);
        switch (root.source) {
            .local => |local| return local,
            .artifact => {},
        }
        if (self.materialized_concrete_roots.get(resolved)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_concrete_roots.put(resolved, local_root);
        errdefer _ = self.materialized_concrete_roots.remove(resolved);

        const payload = try self.materializeConcretePayload(resolved, self.concretePayload(resolved));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealReachableMaterializedLocalGraph(local_root);
        return local_root;
    }

    fn runtimeConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const root = try self.materializeRuntimeConcreteRoot(ref);
        const runtime_ref = try self.program.concrete_source_types.registerLocalRoot(root);
        try self.runtime_concrete_roots.put(runtime_ref, root);
        try self.program.concrete_source_types.publishKeyOwner(runtime_ref);
        return runtime_ref;
    }

    fn materializeRuntimeConcreteRoot(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const resolved = self.resolveConcreteRef(ref);
        if (self.runtime_concrete_roots.get(resolved)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.runtime_concrete_roots.put(resolved, local_root);
        errdefer _ = self.runtime_concrete_roots.remove(resolved);
        const local_ref = try self.program.concrete_source_types.registerLocalRoot(local_root);
        try self.runtime_concrete_roots.put(local_ref, local_root);
        errdefer _ = self.runtime_concrete_roots.remove(local_ref);

        const payload = try self.materializeRuntimeConcretePayload(resolved, self.concretePayload(resolved));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealReachableMaterializedLocalGraph(local_root);
        return local_root;
    }

    fn materializeRuntimeConcretePayload(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished runtime concrete checked type payload"),
            .flex => |flex| try self.materializeClosableConcreteVariableType(ref, flex),
            .rigid => |rigid| try self.materializeClosableConcreteVariableType(ref, rigid),
            .alias => |alias| .{ .alias = .{
                .name = try self.typeNameForConcreteRef(ref, alias.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, alias.origin_module),
                .backing = try self.materializeRuntimeConcreteRoot(try self.concreteAliasBackingRef(ref, alias)),
                .args = try self.materializeRuntimeConcreteTypeIds(ref, alias.args),
            } },
            .record_unbound => |fields| .{ .record = .{
                .fields = try self.materializeRuntimeConcreteRecordFields(ref, fields),
                .ext = try self.materializeSyntheticPayload(.empty_record),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.materializeRuntimeConcreteRecordFields(ref, record.fields),
                .ext = try self.materializeRuntimeConcreteRecordExt(try self.concreteChildRef(ref, record.ext)),
            } },
            .tuple => |items| .{ .tuple = try self.materializeRuntimeConcreteTypeIds(ref, items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.typeNameForConcreteRef(ref, nominal.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.materializeRuntimeConcreteRoot(try self.concreteNominalBackingRef(ref, nominal)),
                .representation = nominal.representation,
                .args = try self.materializeRuntimeConcreteTypeIds(ref, nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = checked_artifact.finalizedFunctionKind(func.kind),
                .args = try self.materializeRuntimeConcreteTypeIds(ref, func.args),
                .ret = try self.materializeRuntimeConcreteRoot(try self.concreteChildRef(ref, func.ret)),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.materializeRuntimeConcreteTags(ref, tag_union.tags),
                .ext = try self.materializeRuntimeConcreteTagUnionExt(try self.concreteChildRef(ref, tag_union.ext)),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn materializeRuntimeConcreteRecordExt(
        self: *TypeInstantiator,
        ext: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return switch (self.concretePayload(ext)) {
            .flex => |flex| try self.materializeEmptyRecordRowTail(flex),
            .rigid => |rigid| try self.materializeEmptyRecordRowTail(rigid),
            else => try self.materializeRuntimeConcreteRoot(ext),
        };
    }

    fn materializeRuntimeConcreteTagUnionExt(
        self: *TypeInstantiator,
        ext: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return switch (self.concretePayload(ext)) {
            .flex => |flex| try self.materializeEmptyTagUnionRowTail(flex),
            .rigid => |rigid| try self.materializeEmptyTagUnionRowTail(rigid),
            else => try self.materializeRuntimeConcreteRoot(ext),
        };
    }

    fn materializeRuntimeConcreteTypeIds(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.materializeRuntimeConcreteRoot(try self.concreteChildRef(parent, id));
        }
        return out;
    }

    fn materializeRuntimeConcreteRecordFields(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.recordFieldNameForConcreteRef(parent, field.name),
                .ty = try self.materializeRuntimeConcreteRoot(try self.concreteChildRef(parent, field.ty)),
            };
        }
        return out;
    }

    fn materializeRuntimeConcreteTags(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.tagNameForConcreteRef(parent, tag.name),
                .args = try self.materializeRuntimeConcreteTypeIds(parent, tag.args),
            };
        }
        return out;
    }

    fn sealMaterializedLocalRootRef(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        var key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.program.canonical_names,
            self.program.concrete_source_types.local_payloads.items,
        );
        defer key_builder.deinit();
        return try self.program.concrete_source_types.sealLocalRoot(root, try key_builder.keyForRoot(root));
    }

    fn sealMaterializedLocalRoot(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!void {
        _ = try self.sealMaterializedLocalRootRef(root);
    }

    fn sealReachableMaterializedLocalGraph(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!void {
        if (try self.localGraphHasPendingPayload(root)) return;

        var reachable = std.ArrayList(checked_artifact.CheckedTypeId).empty;
        defer reachable.deinit(self.allocator);

        var visited = std.AutoHashMap(checked_artifact.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();

        try self.collectReachableLocalTypes(root, &visited, &reachable);
        for (reachable.items) |local_root| {
            try self.sealMaterializedLocalRoot(local_root);
        }
    }

    fn localGraphHasPendingPayload(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!bool {
        var visited = std.AutoHashMap(checked_artifact.CheckedTypeId, void).init(self.allocator);
        defer visited.deinit();
        return try self.localGraphHasPendingPayloadHelp(root, &visited);
    }

    fn localGraphHasPendingPayloadHelp(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
    ) Allocator.Error!bool {
        if (visited.contains(root)) return false;
        try visited.put(root, {});

        const payload = self.localConcretePayload(root);
        return switch (payload) {
            .pending => true,
            .flex => |flex| try self.localConstraintsHavePendingPayload(flex.constraints, visited),
            .rigid => |rigid| try self.localConstraintsHavePendingPayload(rigid.constraints, visited),
            .alias => |alias| blk: {
                if (try self.localGraphHasPendingPayloadHelp(alias.backing, visited)) break :blk true;
                break :blk try self.localTypeIdsHavePendingPayload(alias.args, visited);
            },
            .record_unbound => |fields| try self.localRecordFieldsHavePendingPayload(fields, visited),
            .record => |record| blk: {
                if (try self.localRecordFieldsHavePendingPayload(record.fields, visited)) break :blk true;
                break :blk try self.localGraphHasPendingPayloadHelp(record.ext, visited);
            },
            .tuple => |items| try self.localTypeIdsHavePendingPayload(items, visited),
            .nominal => |nominal| blk: {
                if (try self.localGraphHasPendingPayloadHelp(nominal.backing, visited)) break :blk true;
                break :blk try self.localTypeIdsHavePendingPayload(nominal.args, visited);
            },
            .function => |function| blk: {
                if (try self.localTypeIdsHavePendingPayload(function.args, visited)) break :blk true;
                break :blk try self.localGraphHasPendingPayloadHelp(function.ret, visited);
            },
            .empty_record,
            .empty_tag_union,
            => false,
            .tag_union => |tag_union| blk: {
                for (tag_union.tags) |tag| {
                    if (try self.localTypeIdsHavePendingPayload(tag.args, visited)) break :blk true;
                }
                break :blk try self.localGraphHasPendingPayloadHelp(tag_union.ext, visited);
            },
        };
    }

    fn localConstraintsHavePendingPayload(
        self: *TypeInstantiator,
        constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (constraints) |constraint| {
            if (try self.localGraphHasPendingPayloadHelp(constraint.fn_ty, visited)) return true;
        }
        return false;
    }

    fn localTypeIdsHavePendingPayload(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (ids) |id| {
            if (try self.localGraphHasPendingPayloadHelp(id, visited)) return true;
        }
        return false;
    }

    fn localRecordFieldsHavePendingPayload(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
    ) Allocator.Error!bool {
        for (fields) |field| {
            if (try self.localGraphHasPendingPayloadHelp(field.ty, visited)) return true;
        }
        return false;
    }

    fn collectReachableLocalTypes(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        out: *std.ArrayList(checked_artifact.CheckedTypeId),
    ) Allocator.Error!void {
        if (visited.contains(root)) return;
        try visited.put(root, {});
        try out.append(self.allocator, root);

        const payload = self.localConcretePayload(root);
        switch (payload) {
            .pending => invariantViolation("mono specialization tried to seal a pending local concrete source type"),
            .flex => |flex| try self.collectReachableLocalConstraints(flex.constraints, visited, out),
            .rigid => |rigid| try self.collectReachableLocalConstraints(rigid.constraints, visited, out),
            .alias => |alias| {
                try self.collectReachableLocalTypes(alias.backing, visited, out);
                try self.collectReachableLocalTypeIds(alias.args, visited, out);
            },
            .record_unbound => |fields| try self.collectReachableLocalRecordFields(fields, visited, out),
            .record => |record| {
                try self.collectReachableLocalRecordFields(record.fields, visited, out);
                try self.collectReachableLocalTypes(record.ext, visited, out);
            },
            .tuple => |items| try self.collectReachableLocalTypeIds(items, visited, out),
            .nominal => |nominal| {
                try self.collectReachableLocalTypes(nominal.backing, visited, out);
                try self.collectReachableLocalTypeIds(nominal.args, visited, out);
            },
            .function => |function| {
                try self.collectReachableLocalTypeIds(function.args, visited, out);
                try self.collectReachableLocalTypes(function.ret, visited, out);
            },
            .empty_record,
            .empty_tag_union,
            => {},
            .tag_union => |tag_union| {
                for (tag_union.tags) |tag| {
                    try self.collectReachableLocalTypeIds(tag.args, visited, out);
                }
                try self.collectReachableLocalTypes(tag_union.ext, visited, out);
            },
        }
    }

    fn collectReachableLocalConstraints(
        self: *TypeInstantiator,
        constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        out: *std.ArrayList(checked_artifact.CheckedTypeId),
    ) Allocator.Error!void {
        for (constraints) |constraint| {
            try self.collectReachableLocalTypes(constraint.fn_ty, visited, out);
        }
    }

    fn collectReachableLocalTypeIds(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        out: *std.ArrayList(checked_artifact.CheckedTypeId),
    ) Allocator.Error!void {
        for (ids) |id| {
            try self.collectReachableLocalTypes(id, visited, out);
        }
    }

    fn collectReachableLocalRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        visited: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        out: *std.ArrayList(checked_artifact.CheckedTypeId),
    ) Allocator.Error!void {
        for (fields) |field| {
            try self.collectReachableLocalTypes(field.ty, visited, out);
        }
    }

    fn localConcretePayload(
        self: *TypeInstantiator,
        root: checked_artifact.CheckedTypeId,
    ) checked_artifact.CheckedTypePayload {
        const raw = @intFromEnum(root);
        const local_payloads = self.program.concrete_source_types.local_payloads.items;
        if (raw >= local_payloads.len) invariantViolation("mono specialization local concrete source type referenced missing payload");
        return local_payloads[raw];
    }

    fn materializeEmptyRecordRowTail(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        try self.verifyClosableRowTail(variable);
        return try self.materializeSyntheticPayload(.empty_record);
    }

    fn materializeEmptyTagUnionRowTail(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        try self.verifyClosableRowTail(variable);
        return try self.materializeSyntheticPayload(.empty_tag_union);
    }

    fn materializeClosableVariableType(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        if (self.isMonoSpecializationNumericFlex(variable)) return try self.materializeDefaultDecPayload();
        try self.verifyClosableVariable(variable);
        return .empty_record;
    }

    fn materializeClosableConcreteVariableType(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        if (self.isMonoSpecializationNumericFlex(variable)) return try self.materializeDefaultDecPayload();
        try self.verifyClosableConcreteVariable(ref, variable);
        return .empty_record;
    }

    fn lowerClosableConcreteVariableType(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!Type.Content {
        if (self.isMonoSpecializationNumericFlex(variable)) return .{ .primitive = .dec };
        try self.verifyClosableConcreteVariable(ref, variable);
        return .{ .record = .{ .fields = &.{} } };
    }

    fn materializeDefaultDecPayload(
        self: *TypeInstantiator,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        const backing = try self.materializeSyntheticPayload(.empty_tag_union);
        return .{ .nominal = .{
            .name = try self.program.canonical_names.internTypeName("Builtin.Num.Dec"),
            .origin_module = try self.program.canonical_names.internModuleName("Builtin"),
            .builtin = .dec,
            .is_opaque = true,
            .backing = backing,
            .representation = .{ .builtin = .dec },
            .args = &.{},
        } };
    }

    fn materializeSyntheticPayload(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const root = try self.program.concrete_source_types.reservePendingLocalRoot();
        self.program.concrete_source_types.fillLocalRoot(root, payload);
        try self.sealMaterializedLocalRoot(root);
        return root;
    }

    fn materializeSyntheticPayloadRef(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const root = try self.materializeSyntheticPayload(payload);
        return try self.program.concrete_source_types.registerLocalRoot(root);
    }

    fn verifyClosableRowTail(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!void {
        try self.verifyClosableVariable(variable);
    }

    fn verifyClosableVariable(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!void {
        if (variable.constraints.len == 0) return;
        if (self.isMonoSpecializationNumericFlex(variable)) return;
        if (try self.isEqualityOnlyVariable(variable)) return;
        invariantViolation("mono specialization reached a constrained type variable where a concrete runtime type was required");
    }

    fn verifyClosableConcreteVariable(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!void {
        if (variable.constraints.len == 0) return;
        if (self.isMonoSpecializationNumericFlex(variable)) return;
        if (try self.isConcreteEqualityOnlyVariable(ref, variable)) return;
        invariantViolation("mono specialization reached a constrained concrete type variable where a concrete runtime type was required");
    }

    fn concreteRefForMethodTargetCallable(
        self: *TypeInstantiator,
        method_target: static_dispatch.MethodTarget,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        const template = method_target.template orelse invariantViolation("mono static dispatch method target did not publish a checked procedure template");
        const target_artifact = artifactKeyForRef(self.input, template.artifact) orelse {
            debug.invariant(false, "mono static dispatch method target artifact was not available");
            unreachable;
        };
        const checked_types = checkedTypesForKey(self.input, target_artifact) orelse {
            debug.invariant(false, "mono static dispatch method target checked types were not available");
            unreachable;
        };
        return try self.freshConcreteRefForCheckedViewRoot(
            target_artifact,
            checked_types,
            method_target.callable_ty,
        );
    }

    fn isEqualityOnlyVariable(
        self: *TypeInstantiator,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!bool {
        if (variable.constraints.len == 0) return false;
        const is_eq = try self.program.canonical_names.internMethodName("is_eq");
        for (variable.constraints) |constraint| {
            const method = try self.name_resolver.methodName(self.template_artifact, constraint.fn_name);
            if (method != is_eq) return false;
        }
        return true;
    }

    fn isConcreteEqualityOnlyVariable(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        variable: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!bool {
        if (variable.constraints.len == 0) return false;
        const is_eq = try self.program.canonical_names.internMethodName("is_eq");
        for (variable.constraints) |constraint| {
            const method = try self.methodNameForConcreteRef(ref, constraint.fn_name);
            if (method != is_eq) return false;
        }
        return true;
    }

    fn isMonoSpecializationNumericFlex(
        _: *const TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) bool {
        if (flex.numeric_default_phase != .mono_specialization) return false;
        return true;
    }

    fn materializeConcretePayload(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished concrete checked type payload"),
            .flex => |flex| try self.materializeClosableVariableType(flex),
            .rigid => |rigid| try self.materializeClosableVariableType(rigid),
            .alias => |alias| .{ .alias = .{
                .name = try self.typeNameForConcreteRef(ref, alias.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, alias.origin_module),
                .backing = try self.materializeConcreteRef(try self.concreteAliasBackingRef(ref, alias)),
                .args = try self.materializeConcreteTypeIds(ref, alias.args),
            } },
            .record_unbound => |fields| .{ .record = .{
                .fields = try self.materializeConcreteRecordFields(ref, fields),
                .ext = try self.materializeSyntheticPayload(.empty_record),
            } },
            .record => |record| .{ .record = .{
                .fields = try self.materializeConcreteRecordFields(ref, record.fields),
                .ext = try self.materializeConcreteRecordExt(try self.concreteChildRef(ref, record.ext)),
            } },
            .tuple => |items| .{ .tuple = try self.materializeConcreteTypeIds(ref, items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.typeNameForConcreteRef(ref, nominal.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.materializeConcreteRef(try self.concreteNominalBackingRef(ref, nominal)),
                .representation = nominal.representation,
                .args = try self.materializeConcreteTypeIds(ref, nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = checked_artifact.finalizedFunctionKind(func.kind),
                .args = try self.materializeConcreteTypeIds(ref, func.args),
                .ret = try self.materializeConcreteRef(try self.concreteChildRef(ref, func.ret)),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.materializeConcreteTags(ref, tag_union.tags),
                .ext = try self.materializeConcreteTagUnionExt(try self.concreteChildRef(ref, tag_union.ext)),
            } },
            .empty_tag_union => .empty_tag_union,
        };
    }

    fn materializeConcreteRecordExt(
        self: *TypeInstantiator,
        ext: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return switch (self.concretePayload(ext)) {
            .flex => |flex| try self.materializeEmptyRecordRowTail(flex),
            .rigid => |rigid| try self.materializeEmptyRecordRowTail(rigid),
            else => try self.materializeConcreteRef(ext),
        };
    }

    fn materializeConcreteTagUnionExt(
        self: *TypeInstantiator,
        ext: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return switch (self.concretePayload(ext)) {
            .flex => |flex| try self.materializeEmptyTagUnionRowTail(flex),
            .rigid => |rigid| try self.materializeEmptyTagUnionRowTail(rigid),
            else => try self.materializeConcreteRef(ext),
        };
    }

    fn materializeConcreteTypeIds(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const checked_artifact.CheckedTypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.materializeConcreteRef(try self.concreteChildRef(parent, id));
        }
        return out;
    }

    fn materializeConcreteRecordFields(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const checked_artifact.CheckedRecordField {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedRecordField, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.recordFieldNameForConcreteRef(parent, field.name),
                .ty = try self.materializeConcreteRef(try self.concreteChildRef(parent, field.ty)),
            };
        }
        return out;
    }

    fn materializeConcreteTags(
        self: *TypeInstantiator,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        tags: []const checked_artifact.CheckedTag,
    ) Allocator.Error![]const checked_artifact.CheckedTag {
        if (tags.len == 0) return &.{};
        const out = try self.allocator.alloc(checked_artifact.CheckedTag, tags.len);
        for (out) |*tag| tag.* = .{ .name = undefined, .args = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.args);
            self.allocator.free(out);
        }
        for (tags, 0..) |tag, i| {
            out[i] = .{
                .name = try self.tagNameForConcreteRef(parent, tag.name),
                .args = try self.materializeConcreteTypeIds(parent, tag.args),
            };
        }
        return out;
    }

    fn recordFieldNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        const root = self.program.concrete_source_types.root(self.resolveConcreteRef(ref));
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.recordFieldLabel(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn tagNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        const root = self.program.concrete_source_types.root(self.resolveConcreteRef(ref));
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.tagLabel(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn methodNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        const root = self.program.concrete_source_types.root(self.resolveConcreteRef(ref));
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.methodName(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn moduleNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        const root = self.program.concrete_source_types.root(self.resolveConcreteRef(ref));
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.moduleName(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn typeNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.TypeNameId,
    ) Allocator.Error!canonical.TypeNameId {
        const root = self.program.concrete_source_types.root(self.resolveConcreteRef(ref));
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.typeName(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn nominalKeyForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        module_name: canonical.ModuleNameId,
        type_name: canonical.TypeNameId,
    ) Allocator.Error!canonical.NominalTypeKey {
        return .{
            .module_name = try self.moduleNameForConcreteRef(ref, module_name),
            .type_name = try self.typeNameForConcreteRef(ref, type_name),
        };
    }

    fn findConcreteTag(
        self: *TypeInstantiator,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
        tags: []const checked_artifact.CheckedTag,
        name: canonical.TagLabelId,
    ) Allocator.Error!?checked_artifact.CheckedTag {
        for (tags) |tag| {
            if ((try self.tagNameForConcreteRef(concrete, tag.name)) == name) return tag;
        }
        return null;
    }
};

const PrivateCaptureLoweringKey = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    node: checked_artifact.PrivateCaptureNodeId,
    checked_ty: checked_artifact.CheckedTypeId,
};

const ArtifactCheckedTypeKey = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    checked_ty: checked_artifact.CheckedTypeId,
};

const LocalProcDecl = struct {
    pattern: checked_artifact.CheckedPatternId,
    expr: checked_artifact.CheckedExprId,
    owner_generated_stmts: *std.ArrayList(Ast.StmtId),
};

const LocalProcDeclRestore = struct {
    binder: checked_artifact.PatternBinderId,
    previous: ?LocalProcDecl,
};

const LocalProcInstanceKey = struct {
    binder: checked_artifact.PatternBinderId,
    source_fn_ty: canonical.CanonicalTypeKey,
};

const LocalProcEmissionState = enum {
    pending,
    emitting,
    emitted,
};

const LocalProcInstance = struct {
    symbol: Ast.Symbol,
    body: MonoBodyInstanceId,
    source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    emission: LocalProcEmissionState = .pending,
};

const LocalProcInstanceUse = struct {
    symbol: Ast.Symbol,
    source_fn_ty: canonical.CanonicalTypeKey,
    source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
};

const IteratorDispatchKind = enum {
    iter,
    next,
};

const IteratorDispatchKey = struct {
    plan: checked_artifact.IteratorForPlanId,
    kind: IteratorDispatchKind,
};

const StrInspectFinalizationKey = struct {
    source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ret_ty: Type.TypeId,
};

const FinalizedStrInspectDispatch = union(enum) {
    default,
    custom: struct {
        method: canonical.MethodNameId,
        owner: static_dispatch.MethodOwner,
        target: static_dispatch.MethodTarget,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
        callable_ty: Type.TypeId,
        source_fn_ty: canonical.CanonicalTypeKey,
    },
};

const FinalizedStrInspectCallTarget = struct {
    template: canonical.ProcedureTemplateRef,
    imported_closure: ?checked_artifact.ImportedTemplateClosureView,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    fn_ty: Type.TypeId,
    source_fn_ty: canonical.CanonicalTypeKey,
};

const ConcreteTypeInfo = struct {
    ty: Type.TypeId,
    source_ty: canonical.CanonicalTypeKey,
    source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
};

const ConcreteSourceChildKind = struct {
    tag: enum {
        alias_backing,
        nominal_backing,
        function_arg,
        function_return,
        record_field,
        record_ext,
        tuple_elem,
        tag_payload,
        tag_union_ext,
        list_elem,
        box_payload,
    },
    a: u32 = 0,
    b: u32 = 0,
};

const ConcreteSourceChildKey = struct {
    parent: ConcreteSourceType.ConcreteSourceTypeRef,
    kind: ConcreteSourceChildKind,
};

const MonoLoweringMode = enum {
    graph_builder,
    body_emitter,
};

const MonoGraphBuilderState = struct {
    type_instantiator: *TypeInstantiator,
};

const StaticDispatchResolution = union(enum) {
    method_target: struct {
        owner: static_dispatch.MethodOwner,
        target: static_dispatch.MethodTarget,
    },
    structural_equality,
};

const FinalizedStaticDispatch = struct {
    method: canonical.MethodNameId,
    dispatcher_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    callable_ty: Type.TypeId,
    ret_ty: ConcreteTypeInfo,
    arg_infos: []const ConcreteTypeInfo,
    resolution: StaticDispatchResolution,

    fn deinit(self: *FinalizedStaticDispatch, allocator: Allocator) void {
        allocator.free(self.arg_infos);
        self.* = undefined;
    }
};

const FinalizedPromotedWrapper = struct {
    wrapper_source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    wrapper_source_key: canonical.CanonicalTypeKey,
    member_source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    param_infos: []const ConcreteTypeInfo,

    fn deinit(self: *FinalizedPromotedWrapper, allocator: Allocator) void {
        if (self.param_infos.len != 0) allocator.free(self.param_infos);
        self.* = undefined;
    }
};

const CallInstantiationInfo = struct {
    concrete_fn: ConcreteSourceType.ConcreteSourceTypeRef,
    func_ty: Type.TypeId,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    ret_ty: ConcreteTypeInfo,
    arg_infos: []const ConcreteTypeInfo,

    fn deinit(self: *CallInstantiationInfo, allocator: Allocator) void {
        if (self.arg_infos.len != 0) allocator.free(self.arg_infos);
        self.* = undefined;
    }
};

const LoweredStaticDispatchCall = struct {
    expr: Ast.ExprId,
    ret_ty: ConcreteTypeInfo,
};

const LoweredTemplateBody = struct {
    fn_ty: Type.TypeId,
    body: Ast.DefId,
};

const IteratorLoopStateOperand = struct {
    symbol: Ast.Symbol,
    info: ConcreteTypeInfo,
};

const ExprExpectedType = union(enum) {
    checked: checked_artifact.CheckedTypeId,
    concrete: ConcreteTypeInfo,
};

const ParamDestructure = struct {
    symbol: Ast.Symbol,
    pattern: checked_artifact.CheckedPatternId,
    param_ty: ConcreteTypeInfo,
};

const MutableParamInit = struct {
    param_symbol: Ast.Symbol,
    bind: Ast.TypedSymbol,
};

const PatternBinderAction = enum {
    declaration,
    reassignment,
};

const LoweredParamBundle = struct {
    args: Ast.Span(Ast.TypedSymbol),
    destructures: []ParamDestructure,
    mutable_inits: []MutableParamInit,
};

const MonoBodyInstanceId = enum(u32) {
    root = 0,
    _,
};

const MonoBodyInstance = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    fn_ty: Type.TypeId,
    params: []const ConcreteTypeInfo,
    ret_ty: ConcreteTypeInfo,
    kind: union(enum) {
        root,
        local_proc: struct {
            owner: MonoBodyInstanceId,
            binder: checked_artifact.PatternBinderId,
            expr: checked_artifact.CheckedExprId,
            site: canonical.NestedProcSiteId,
            args: []const checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        },
        closure_value: struct {
            owner: MonoBodyInstanceId,
            expr: checked_artifact.CheckedExprId,
            site: canonical.NestedProcSiteId,
            args: []const checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        },
    },
};

const ScopedExpr = struct {
    body: MonoBodyInstanceId,
    expr: checked_artifact.CheckedExprId,
};

const ScopedPattern = struct {
    body: MonoBodyInstanceId,
    pattern: checked_artifact.CheckedPatternId,
};

const ScopedBinder = struct {
    body: MonoBodyInstanceId,
    binder: checked_artifact.PatternBinderId,
};

const FinalizedMonoSpecializationGraph = struct {
    allocator: Allocator,
    body_instances: std.AutoHashMap(MonoBodyInstanceId, MonoBodyInstance),
    local_symbol_types: std.AutoHashMap(ScopedBinder, ConcreteTypeInfo),
    local_type_demands: std.AutoHashMap(ScopedBinder, ConcreteSourceType.ConcreteSourceTypeRef),
    expr_type_demands: std.AutoHashMap(ScopedExpr, ConcreteSourceType.ConcreteSourceTypeRef),
    expr_types: std.AutoHashMap(ScopedExpr, ConcreteTypeInfo),
    pattern_types: std.AutoHashMap(ScopedPattern, ConcreteTypeInfo),
    concrete_type_infos: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, ConcreteTypeInfo),
    concrete_source_children: std.AutoHashMap(ConcreteSourceChildKey, ConcreteSourceType.ConcreteSourceTypeRef),
    artifact_type_infos: std.AutoHashMap(ArtifactCheckedTypeKey, ConcreteTypeInfo),
    local_proc_instances: std.AutoHashMap(LocalProcInstanceKey, LocalProcInstance),
    closure_instances: std.AutoHashMap(ScopedExpr, MonoBodyInstanceId),
    finalized_calls: std.AutoHashMap(ScopedExpr, CallInstantiationInfo),
    finalized_static_dispatches: std.AutoHashMap(checked_artifact.StaticDispatchPlanId, FinalizedStaticDispatch),
    finalized_iterator_dispatches: std.AutoHashMap(IteratorDispatchKey, FinalizedStaticDispatch),
    finalized_str_inspects: std.AutoHashMap(StrInspectFinalizationKey, FinalizedStrInspectDispatch),
    finalized_str_inspect_calls: std.AutoHashMap(StrInspectFinalizationKey, FinalizedStrInspectCallTarget),
    finalized_promoted_wrappers: std.AutoHashMap(canonical.PromotedCallableWrapperId, FinalizedPromotedWrapper),
    finalized_summary_payloads: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId),

    fn init(allocator: Allocator) FinalizedMonoSpecializationGraph {
        return .{
            .allocator = allocator,
            .body_instances = std.AutoHashMap(MonoBodyInstanceId, MonoBodyInstance).init(allocator),
            .local_symbol_types = std.AutoHashMap(ScopedBinder, ConcreteTypeInfo).init(allocator),
            .local_type_demands = std.AutoHashMap(ScopedBinder, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .expr_type_demands = std.AutoHashMap(ScopedExpr, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .expr_types = std.AutoHashMap(ScopedExpr, ConcreteTypeInfo).init(allocator),
            .pattern_types = std.AutoHashMap(ScopedPattern, ConcreteTypeInfo).init(allocator),
            .concrete_type_infos = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, ConcreteTypeInfo).init(allocator),
            .concrete_source_children = std.AutoHashMap(ConcreteSourceChildKey, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
            .artifact_type_infos = std.AutoHashMap(ArtifactCheckedTypeKey, ConcreteTypeInfo).init(allocator),
            .local_proc_instances = std.AutoHashMap(LocalProcInstanceKey, LocalProcInstance).init(allocator),
            .closure_instances = std.AutoHashMap(ScopedExpr, MonoBodyInstanceId).init(allocator),
            .finalized_calls = std.AutoHashMap(ScopedExpr, CallInstantiationInfo).init(allocator),
            .finalized_static_dispatches = std.AutoHashMap(checked_artifact.StaticDispatchPlanId, FinalizedStaticDispatch).init(allocator),
            .finalized_iterator_dispatches = std.AutoHashMap(IteratorDispatchKey, FinalizedStaticDispatch).init(allocator),
            .finalized_str_inspects = std.AutoHashMap(StrInspectFinalizationKey, FinalizedStrInspectDispatch).init(allocator),
            .finalized_str_inspect_calls = std.AutoHashMap(StrInspectFinalizationKey, FinalizedStrInspectCallTarget).init(allocator),
            .finalized_promoted_wrappers = std.AutoHashMap(canonical.PromotedCallableWrapperId, FinalizedPromotedWrapper).init(allocator),
            .finalized_summary_payloads = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId).init(allocator),
        };
    }

    fn deinit(self: *FinalizedMonoSpecializationGraph) void {
        var bodies = self.body_instances.valueIterator();
        while (bodies.next()) |body| {
            if (body.params.len != 0) self.allocator.free(body.params);
        }
        var promoted_wrappers = self.finalized_promoted_wrappers.valueIterator();
        while (promoted_wrappers.next()) |wrapper| wrapper.deinit(self.allocator);
        self.finalized_promoted_wrappers.deinit();
        self.finalized_summary_payloads.deinit();
        self.finalized_str_inspect_calls.deinit();
        self.finalized_str_inspects.deinit();
        var iterator_dispatches = self.finalized_iterator_dispatches.valueIterator();
        while (iterator_dispatches.next()) |dispatch| dispatch.deinit(self.allocator);
        self.finalized_iterator_dispatches.deinit();
        var static_dispatches = self.finalized_static_dispatches.valueIterator();
        while (static_dispatches.next()) |dispatch| dispatch.deinit(self.allocator);
        self.finalized_static_dispatches.deinit();
        var calls = self.finalized_calls.valueIterator();
        while (calls.next()) |call| call.deinit(self.allocator);
        self.finalized_calls.deinit();
        self.closure_instances.deinit();
        self.local_proc_instances.deinit();
        self.artifact_type_infos.deinit();
        self.concrete_source_children.deinit();
        self.concrete_type_infos.deinit();
        self.pattern_types.deinit();
        self.expr_types.deinit();
        self.expr_type_demands.deinit();
        self.local_type_demands.deinit();
        self.local_symbol_types.deinit();
        self.body_instances.deinit();
        self.* = undefined;
    }
};

fn MonoSpecializationLowering(comptime mode: MonoLoweringMode) type {
    return struct {
        const Self = @This();

        allocator: Allocator,
        input: Input,
        program: *Program,
        template_lookup: CheckedTemplateLookup,
        graph_builder_state: if (mode == .graph_builder) MonoGraphBuilderState else void,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
        queue: *Queue,
        graph: if (mode == .graph_builder) FinalizedMonoSpecializationGraph else *const FinalizedMonoSpecializationGraph,
        local_symbols: std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol),
        local_proc_decls: std.AutoHashMap(checked_artifact.PatternBinderId, LocalProcDecl),
        local_proc_instances: std.AutoHashMap(LocalProcInstanceKey, LocalProcInstance),
        lowered_private_captures: std.AutoHashMap(PrivateCaptureLoweringKey, Ast.ExprId),
        active_private_captures: std.AutoHashMap(PrivateCaptureLoweringKey, void),
        current_body: MonoBodyInstanceId,
        next_body_instance: u32,
        current_return_type: ?ConcreteTypeInfo,
        current_return_source_ref: ?ConcreteSourceType.ConcreteSourceTypeRef,

        fn init(
            allocator: Allocator,
            input: Input,
            program: *Program,
            template_lookup: CheckedTemplateLookup,
            type_instantiator: *TypeInstantiator,
            name_resolver: *ArtifactNames.ArtifactNameResolver,
            queue: *Queue,
        ) Self {
            if (mode != .graph_builder) {
                @compileError("MonoBodyEmitter is created with initEmitter, not init");
            }
            return .{
                .allocator = allocator,
                .input = input,
                .program = program,
                .template_lookup = template_lookup,
                .graph_builder_state = .{ .type_instantiator = type_instantiator },
                .name_resolver = name_resolver,
                .queue = queue,
                .graph = FinalizedMonoSpecializationGraph.init(allocator),
                .local_symbols = std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol).init(allocator),
                .local_proc_decls = std.AutoHashMap(checked_artifact.PatternBinderId, LocalProcDecl).init(allocator),
                .local_proc_instances = std.AutoHashMap(LocalProcInstanceKey, LocalProcInstance).init(allocator),
                .lowered_private_captures = std.AutoHashMap(PrivateCaptureLoweringKey, Ast.ExprId).init(allocator),
                .active_private_captures = std.AutoHashMap(PrivateCaptureLoweringKey, void).init(allocator),
                .current_body = .root,
                .next_body_instance = 1,
                .current_return_type = null,
                .current_return_source_ref = null,
            };
        }

        fn initEmitter(
            allocator: Allocator,
            input: Input,
            program: *Program,
            template_lookup: CheckedTemplateLookup,
            graph: *const FinalizedMonoSpecializationGraph,
            name_resolver: *ArtifactNames.ArtifactNameResolver,
            queue: *Queue,
        ) Allocator.Error!Self {
            if (mode != .body_emitter) {
                @compileError("MonoSpecializationGraphBuilder is created with init, not initEmitter");
            }
            var emitter = Self{
                .allocator = allocator,
                .input = input,
                .program = program,
                .template_lookup = template_lookup,
                .graph_builder_state = {},
                .name_resolver = name_resolver,
                .queue = queue,
                .graph = graph,
                .local_symbols = std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol).init(allocator),
                .local_proc_decls = std.AutoHashMap(checked_artifact.PatternBinderId, LocalProcDecl).init(allocator),
                .local_proc_instances = std.AutoHashMap(LocalProcInstanceKey, LocalProcInstance).init(allocator),
                .lowered_private_captures = std.AutoHashMap(PrivateCaptureLoweringKey, Ast.ExprId).init(allocator),
                .active_private_captures = std.AutoHashMap(PrivateCaptureLoweringKey, void).init(allocator),
                .current_body = .root,
                .next_body_instance = 1,
                .current_return_type = null,
                .current_return_source_ref = null,
            };
            errdefer emitter.deinit();

            try emitter.local_proc_instances.ensureTotalCapacity(@intCast(graph.local_proc_instances.count()));
            var instances = graph.local_proc_instances.iterator();
            while (instances.next()) |entry| {
                emitter.local_proc_instances.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
            }

            return emitter;
        }

        fn deinit(self: *Self) void {
            self.active_private_captures.deinit();
            self.lowered_private_captures.deinit();
            self.local_proc_instances.deinit();
            self.local_proc_decls.deinit();
            self.local_symbols.deinit();
            if (mode == .graph_builder) {
                self.graph.deinit();
            }
        }

        fn lowerTemplateBody(
            self: *Self,
            reserved: ReservedMonoProc,
        ) Allocator.Error!LoweredTemplateBody {
            if (mode != .graph_builder) {
                @compileError("only MonoSpecializationGraphBuilder finalizes a template body graph");
            }
            try self.finalizeTemplateBodyGraph(reserved);
            const root_body = self.graph.body_instances.get(.root) orelse {
                invariantViolation("mono graph finalization completed without publishing a root body instance");
            };
            var emitter = try MonoBodyEmitter.initEmitter(self.allocator, self.input, self.program, self.template_lookup, &self.graph, self.name_resolver, self.queue);
            defer emitter.deinit();
            return .{
                .fn_ty = root_body.fn_ty,
                .body = try emitter.lowerFinalizedTemplateBody(reserved, root_body.fn_ty),
            };
        }

        fn lowerFinalizedTemplateBody(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
        ) Allocator.Error!Ast.DefId {
            if (mode != .body_emitter) {
                @compileError("only MonoBodyEmitter emits mono MIR from a finalized graph");
            }
            return switch (self.template_lookup.template.body) {
                .checked_body => |body_id| try self.lowerCheckedBody(reserved, fn_ty, body_id),
                .entry_wrapper => |wrapper_id| try self.lowerEntryWrapperDef(reserved, fn_ty, wrapper_id),
                .intrinsic_wrapper => |wrapper_id| try self.lowerIntrinsicWrapperDef(reserved, fn_ty, wrapper_id),
                .promoted_callable_wrapper => |wrapper_id| try self.lowerPromotedCallableWrapperDef(reserved, fn_ty, wrapper_id),
            };
        }

        fn graphInstantiator(self: *const Self) *TypeInstantiator {
            if (mode == .body_emitter) {
                @compileError("MonoBodyEmitter cannot access graph-finalization type instantiation state");
            }
            return self.graph_builder_state.type_instantiator;
        }

        fn finalizeTemplateBodyGraph(
            self: *Self,
            reserved: ReservedMonoProc,
        ) Allocator.Error!void {
            if (self.graph.body_instances.contains(.root)) {
                invariantViolation("mono graph finalized root body more than once");
            }
            const ret_ref = try self.sourceReturnRefFromFunction(reserved.requested_fn_ty);

            const previous_body = self.current_body;
            self.current_body = .root;
            defer self.current_body = previous_body;

            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            self.current_return_type = null;
            self.current_return_source_ref = ret_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;

            switch (self.template_lookup.template.body) {
                .checked_body => |body_id| try self.finalizeCheckedBodyGraph(reserved, body_id, ret_ref),
                .entry_wrapper => |wrapper_id| try self.finalizeEntryWrapperGraph(wrapper_id, ret_ref),
                .intrinsic_wrapper => |wrapper_id| blk: {
                    const runtime_fn_ref = try self.graphInstantiator().runtimeConcreteRef(reserved.requested_fn_ty);
                    const fn_ty = try self.graphInstantiator().lowerConcreteRef(runtime_fn_ref);
                    try self.finalizeIntrinsicWrapperGraph(runtime_fn_ref, fn_ty, wrapper_id);
                    break :blk;
                },
                .promoted_callable_wrapper => |wrapper_id| try self.finalizePromotedCallableWrapperGraph(reserved, wrapper_id),
            }
            try self.publishLocalConcreteDemands();
            try self.publishExprConcreteDemands();

            const runtime_fn_ref = try self.graphInstantiator().runtimeConcreteRef(reserved.requested_fn_ty);
            const fn_ty = try self.graphInstantiator().lowerConcreteRef(runtime_fn_ref);
            const params = try self.paramTypesFromConcreteFunction(runtime_fn_ref);
            const ret_ty = try self.returnTypeFromConcreteFunction(runtime_fn_ref);
            try self.graph.body_instances.put(.root, .{
                .source_fn_ty = self.program.concrete_source_types.key(runtime_fn_ref),
                .source_fn_ty_payload = runtime_fn_ref,
                .fn_ty = fn_ty,
                .params = params,
                .ret_ty = ret_ty,
                .kind = .root,
            });
        }

        fn finalizeCheckedBodyGraph(
            self: *Self,
            reserved: ReservedMonoProc,
            body_id: checked_artifact.CheckedBodyId,
            ret_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const body = self.checkedBody(body_id);
            const root = self.checkedExpr(body.root_expr);
            switch (root.data) {
                .lambda => |lambda| try self.finalizeLambdaBodyGraph(lambda.args, lambda.body, reserved.requested_fn_ty, ret_ref),
                .closure => |closure| {
                    const lambda_expr = self.checkedExpr(closure.lambda);
                    switch (lambda_expr.data) {
                        .lambda => |lambda| try self.finalizeLambdaBodyGraph(lambda.args, lambda.body, reserved.requested_fn_ty, ret_ref),
                        else => invariantViolation("mono graph finalization expected checked closure to reference a lambda body"),
                    }
                },
                .hosted_lambda => |hosted| try self.finalizeHostedBodyGraph(hosted.args, reserved.requested_fn_ty),
                .anno_only => invariantViolation("mono graph finalization reached annotation-only procedure body without checked backing expression"),
                else => invariantViolation("mono graph finalization expected a checked procedure body to be a lambda-like expression"),
            }
        }

        fn finalizeEntryWrapperGraph(
            self: *Self,
            wrapper_id: canonical.EntryWrapperId,
            ret_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const entry_wrappers = self.template_lookup.entry_wrappers orelse {
                debug.invariant(false, "mono graph finalization invariant violated: entry wrapper template came from a view without entry wrappers");
                unreachable;
            };
            const wrapper = entry_wrappers.get(wrapper_id);
            try self.collectExprDemand(wrapper.body_expr, ret_ref);
        }

        fn finalizeIntrinsicWrapperGraph(
            self: *Self,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
            fn_ty: Type.TypeId,
            wrapper_id: canonical.IntrinsicWrapperId,
        ) Allocator.Error!void {
            const wrapper = self.template_lookup.intrinsic_wrappers.get(wrapper_id);
            const fn_content = self.program.types.getType(fn_ty);
            const func = switch (fn_content) {
                .func => |func| func,
                else => invariantViolation("mono graph finalization expected intrinsic wrapper type to be a function"),
            };
            const param_infos = try self.paramTypesFromConcreteFunction(requested_fn_ty);
            defer if (param_infos.len != 0) self.allocator.free(param_infos);
            switch (wrapper.intrinsic) {
                .str_inspect => {
                    if (param_infos.len != 1) {
                        invariantViolation("mono graph finalization expected Str.inspect intrinsic to have exactly one argument");
                    }
                    try self.finalizeStrInspectIntrinsic(func.ret, param_infos[0]);
                },
                .structural_eq => {
                    if (param_infos.len != 2) {
                        invariantViolation("mono graph finalization expected structural-equality intrinsic to have exactly two arguments");
                    }
                },
            }
        }

        fn finalizeHostedBodyGraph(
            self: *Self,
            arg_patterns: []const checked_artifact.CheckedPatternId,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const param_refs = try self.sourceParamRefsFromFunction(source_fn);
            defer if (param_refs.len != 0) self.allocator.free(param_refs);
            if (arg_patterns.len != param_refs.len) {
                invariantViolation("mono graph finalization hosted parameter count disagreed with requested function type");
            }
            for (arg_patterns, param_refs) |pattern, param_ref| {
                try self.recordPatternDemand(pattern, param_ref);
            }
        }

        fn finalizeLambdaBodyGraph(
            self: *Self,
            arg_patterns: []const checked_artifact.CheckedPatternId,
            body_expr: checked_artifact.CheckedExprId,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
            ret_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const param_refs = try self.sourceParamRefsFromFunction(source_fn);
            defer if (param_refs.len != 0) self.allocator.free(param_refs);
            if (arg_patterns.len != param_refs.len) {
                invariantViolation("mono graph finalization lambda parameter count disagreed with requested function type");
            }
            for (arg_patterns, param_refs) |pattern, param_ref| {
                try self.recordPatternDemand(pattern, param_ref);
            }
            try self.collectExprDemand(body_expr, ret_ref);
        }

        fn lowerIntrinsicWrapperDef(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
            wrapper_id: canonical.IntrinsicWrapperId,
        ) Allocator.Error!Ast.DefId {
            const wrapper = self.template_lookup.intrinsic_wrappers.get(wrapper_id);
            if (wrapper.checked_fn_root != self.template_lookup.template.checked_fn_root) {
                invariantViolation("mono body lowering reached intrinsic wrapper with mismatched checked function root");
            }

            const fn_content = self.program.types.getType(fn_ty);
            const func = switch (fn_content) {
                .func => |func| func,
                else => invariantViolation("mono body lowering expected intrinsic wrapper type to be a function"),
            };
            const root_body = self.graph.body_instances.get(.root) orelse {
                invariantViolation("mono body emission intrinsic wrapper had no finalized root body");
            };
            const source_ty = root_body.source_fn_ty;
            const param_infos = root_body.params;
            if (param_infos.len != func.args.len) {
                invariantViolation("mono body lowering intrinsic wrapper parameter count disagreed with concrete source type");
            }
            const params = try self.lowerIntrinsicParamBundle(param_infos);
            defer if (params.exprs.len > 0) self.allocator.free(params.exprs);
            const body = switch (wrapper.intrinsic) {
                .str_inspect => blk: {
                    if (params.exprs.len != 1) {
                        invariantViolation("mono body lowering expected Str.inspect intrinsic to have exactly one argument");
                    }
                    break :blk try self.lowerStrInspectIntrinsic(func.ret, params.exprs[0], param_infos[0]);
                },
                .structural_eq => blk: {
                    if (params.exprs.len != 2) {
                        invariantViolation("mono body lowering expected structural-equality intrinsic to have exactly two arguments");
                    }
                    break :blk try self.program.ast.addExpr(func.ret, .{ .structural_eq = .{
                        .lhs = params.exprs[0],
                        .rhs = params.exprs[1],
                    } });
                },
            };
            const bind = Ast.TypedSymbol{
                .ty = fn_ty,
                .source_ty = source_ty,
                .source_ty_payload = root_body.source_fn_ty_payload,
                .symbol = try self.program.addProcSymbol(reserved.local_handle),
            };
            return try self.program.ast.addDef(.{
                .proc = mirProcedureRefFromReserved(reserved),
                .debug_name = null,
                .value = .{ .fn_ = .{
                    .source_fn_ty = source_ty,
                    .source_fn_ty_payload = root_body.source_fn_ty_payload,
                    .recursive = false,
                    .bind = bind,
                    .args = params.args,
                    .body = body,
                } },
            });
        }

        const IntrinsicParamBundle = struct {
            args: Ast.Span(Ast.TypedSymbol),
            exprs: []const Ast.ExprId,
        };

        fn lowerIntrinsicParamBundle(
            self: *Self,
            arg_infos: []const ConcreteTypeInfo,
        ) Allocator.Error!IntrinsicParamBundle {
            if (arg_infos.len == 0) return .{
                .args = Ast.Span(Ast.TypedSymbol).empty(),
                .exprs = &.{},
            };
            const args = try self.allocator.alloc(Ast.TypedSymbol, arg_infos.len);
            defer self.allocator.free(args);
            const exprs = try self.allocator.alloc(Ast.ExprId, arg_infos.len);
            errdefer self.allocator.free(exprs);

            for (arg_infos, 0..) |arg_info, i| {
                const symbol = try self.program.addSyntheticSymbol();
                args[i] = .{
                    .ty = arg_info.ty,
                    .source_ty = arg_info.source_ty,
                    .source_ty_payload = arg_info.source_ref,
                    .symbol = symbol,
                };
                exprs[i] = try self.program.ast.addExprWithSourcePayload(arg_info.ty, arg_info.source_ty, arg_info.source_ref, .{ .var_ = symbol });
            }

            return .{
                .args = try self.program.ast.addTypedSymbolSpan(args),
                .exprs = exprs,
            };
        }

        fn lowerStrInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
        ) Allocator.Error!Ast.ExprId {
            if (try self.lowerCustomStrInspectCallIfAvailable(ret_ty, arg_expr, arg_info)) |custom| return custom;

            return try self.lowerDefaultStrInspectIntrinsic(ret_ty, arg_expr, arg_info, self.program.types.getTypePreservingNominal(arg_info.ty));
        }

        fn lowerDefaultStrInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            content: Type.Content,
        ) Allocator.Error!Ast.ExprId {
            return switch (content) {
                .primitive => |prim| switch (prim) {
                    .str => try self.lowerUnaryIntrinsicLowLevel(ret_ty, .str_inspect, arg_expr),
                    .bool => try self.lowerBoolInspectIntrinsic(ret_ty, arg_expr),
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
                    => try self.lowerUnaryIntrinsicLowLevel(ret_ty, .num_to_str, arg_expr),
                    .erased => invariantViolation("Str.inspect intrinsic cannot inspect erased values directly"),
                },
                .tuple => |items| try self.lowerTupleInspectIntrinsic(ret_ty, arg_expr, arg_info, items),
                .list => |elem_ty| try self.lowerListInspectIntrinsic(ret_ty, arg_expr, arg_info, elem_ty),
                .box => |payload_ty| try self.lowerBoxInspectIntrinsic(ret_ty, arg_expr, arg_info, payload_ty),
                .record => |record| try self.lowerRecordInspectIntrinsic(ret_ty, arg_expr, arg_info, record.fields),
                .tag_union => |tag_union| try self.lowerTagUnionInspectIntrinsic(ret_ty, arg_expr, arg_info, tag_union.tags),
                .nominal => |nominal| try self.lowerNominalInspectIntrinsic(ret_ty, arg_expr, arg_info, nominal),
                .func => try self.lowerStringLiteralExpr(ret_ty, "<function>"),
                .link => invariantViolation("Str.inspect intrinsic reached an unresolved mono type link"),
                .placeholder => invariantViolation("Str.inspect intrinsic reached an unresolved mono type placeholder"),
                .unbd => invariantViolation("Str.inspect intrinsic reached an unresolved mono type variable"),
            };
        }

        const StrInspectCallTarget = struct {
            proc: canonical.MirProcedureRef,
            fn_ty: Type.TypeId,
            source_fn_ty: canonical.CanonicalTypeKey,
            source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
        };

        const StrInspectTemplate = struct {
            template: canonical.ProcedureTemplateRef,
            checked_fn_root: checked_artifact.CheckedTypeId,
            imported_closure: ?checked_artifact.ImportedTemplateClosureView,
        };

        fn finalizeStrInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_info: ConcreteTypeInfo,
        ) Allocator.Error!void {
            const key: StrInspectFinalizationKey = .{
                .source_ref = arg_info.source_ref,
                .ret_ty = ret_ty,
            };
            if (self.graph.finalized_str_inspects.contains(key)) return;

            const owner = (try self.methodOwnerForInspectSourceTypeMaybe(arg_info.source_ref)) orelse {
                try self.graph.finalized_str_inspects.put(key, .default);
                try self.finalizeDefaultStrInspectIntrinsic(ret_ty, arg_info, self.program.types.getTypePreservingNominal(arg_info.ty));
                return;
            };
            const method = try self.toInspectMethodName();
            const method_target = (try self.lookupMethodTarget(owner, method)) orelse {
                try self.graph.finalized_str_inspects.put(key, .default);
                try self.finalizeDefaultStrInspectIntrinsic(ret_ty, arg_info, self.program.types.getTypePreservingNominal(arg_info.ty));
                return;
            };

            const inspect = try self.strInspectTemplate();
            const requested_fn_ty = try self.strInspectFunctionTypeForArg(inspect, arg_info.source_ref);
            const target_callable = try self.concreteRefForMethodTargetCallable(method_target);
            try self.graphInstantiator().unifyConcreteRefs(requested_fn_ty, target_callable);

            try self.graph.finalized_str_inspects.put(key, .{ .custom = .{
                .method = method,
                .owner = owner,
                .target = method_target,
                .requested_fn_ty = requested_fn_ty,
                .callable_ty = try self.monoFunctionTypeForStrInspectCall(arg_info.ty, ret_ty),
                .source_fn_ty = self.program.concrete_source_types.key(requested_fn_ty),
            } });
        }

        fn finalizeDefaultStrInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_info: ConcreteTypeInfo,
            content: Type.Content,
        ) Allocator.Error!void {
            switch (content) {
                .primitive,
                .func,
                => {},
                .tuple => |items| {
                    const source_items = try self.concreteTupleElementInfos(arg_info.source_ref, items.len);
                    defer if (source_items.len != 0) self.allocator.free(source_items);
                    for (items, source_items) |item_ty, source_item| {
                        try self.finalizeStrInspectCallTarget(.{
                            .ty = item_ty,
                            .source_ty = source_item.source_ty,
                            .source_ref = source_item.source_ref,
                        }, ret_ty);
                    }
                },
                .list => |elem_ty| {
                    _ = try self.boolConcreteTypeInfo();
                    const source_elem = try self.listElementTypeFromConcrete(arg_info.source_ref);
                    try self.finalizeStrInspectCallTarget(.{
                        .ty = elem_ty,
                        .source_ty = source_elem.source_ty,
                        .source_ref = source_elem.source_ref,
                    }, ret_ty);
                },
                .box => |payload_ty| try self.finalizeStrInspectCallTarget(try self.boxPayloadTypeFromConcrete(arg_info.source_ref, payload_ty), ret_ty),
                .record => |record| {
                    for (record.fields) |field| {
                        const field_ref = try self.concreteRecordFieldRef(arg_info.source_ref, field.name);
                        const source_field = try self.concreteTypeInfoForRef(field_ref);
                        try self.finalizeStrInspectCallTarget(.{
                            .ty = field.ty,
                            .source_ty = source_field.source_ty,
                            .source_ref = source_field.source_ref,
                        }, ret_ty);
                    }
                },
                .tag_union => |tag_union| {
                    for (tag_union.tags) |tag| {
                        const payload_infos = try self.concreteTagPayloadInfosForUnionType(arg_info.source_ref, tag.name);
                        defer if (payload_infos.len > 0) self.allocator.free(payload_infos);
                        if (payload_infos.len != tag.args.len) invariantViolation("mono graph finalization Str.inspect tag payload source count disagreed with tag type");
                        for (tag.args, payload_infos) |payload_ty, payload_info| {
                            try self.finalizeStrInspectCallTarget(.{
                                .ty = payload_ty,
                                .source_ty = payload_info.source_ty,
                                .source_ref = payload_info.source_ref,
                            }, ret_ty);
                        }
                    }
                },
                .nominal => |nominal| {
                    if (!nominal.is_opaque) {
                        try self.finalizeDefaultStrInspectIntrinsic(ret_ty, arg_info, self.program.types.getType(nominal.backing));
                    }
                },
                .link => invariantViolation("mono graph finalization Str.inspect reached an unresolved mono type link"),
                .placeholder => invariantViolation("mono graph finalization Str.inspect reached an unresolved mono type placeholder"),
                .unbd => invariantViolation("mono graph finalization Str.inspect reached an unresolved mono type variable"),
            }
        }

        fn finalizeStrInspectCallTarget(
            self: *Self,
            arg_info: ConcreteTypeInfo,
            ret_ty: Type.TypeId,
        ) Allocator.Error!void {
            const key: StrInspectFinalizationKey = .{
                .source_ref = arg_info.source_ref,
                .ret_ty = ret_ty,
            };
            if (self.graph.finalized_str_inspect_calls.contains(key)) return;
            const inspect = try self.strInspectTemplate();
            const requested_fn_ty = try self.strInspectFunctionTypeForArg(inspect, arg_info.source_ref);
            try self.graph.finalized_str_inspect_calls.put(key, .{
                .template = inspect.template,
                .imported_closure = inspect.imported_closure,
                .requested_fn_ty = requested_fn_ty,
                .fn_ty = try self.monoFunctionTypeForStrInspectCall(arg_info.ty, ret_ty),
                .source_fn_ty = self.program.concrete_source_types.key(requested_fn_ty),
            });
        }

        fn finalizedStrInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_info: ConcreteTypeInfo,
        ) FinalizedStrInspectDispatch {
            return self.graph.finalized_str_inspects.get(.{
                .source_ref = arg_info.source_ref,
                .ret_ty = ret_ty,
            }) orelse {
                invariantViolation("mono body emission reached Str.inspect before graph finalization published it");
            };
        }

        fn finalizedStrInspectCallTarget(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_info: ConcreteTypeInfo,
        ) FinalizedStrInspectCallTarget {
            return self.graph.finalized_str_inspect_calls.get(.{
                .source_ref = arg_info.source_ref,
                .ret_ty = ret_ty,
            }) orelse {
                invariantViolation("mono body emission reached generated Str.inspect call before graph finalization published it");
            };
        }

        fn lowerStrInspectCall(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
        ) Allocator.Error!Ast.ExprId {
            const target = try self.strInspectCallTarget(arg_info, ret_ty);
            const args = [_]Ast.ExprId{arg_expr};
            return try self.program.ast.addExpr(ret_ty, .{ .call_proc = .{
                .proc = target.proc,
                .args = try self.program.ast.addExprSpan(&args),
                .requested_fn_ty = target.fn_ty,
                .requested_source_fn_ty = target.source_fn_ty,
                .requested_source_fn_ty_payload = target.source_fn_ty_payload,
            } });
        }

        fn lowerCustomStrInspectCallIfAvailable(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
        ) Allocator.Error!?Ast.ExprId {
            const finalized = switch (self.finalizedStrInspectIntrinsic(ret_ty, arg_info)) {
                .default => return null,
                .custom => |custom| custom,
            };

            const inspect = try self.strInspectTemplate();
            const template = try self.name_resolver.procedureTemplateRef(finalized.target.template orelse invariantViolation("mono Str.inspect custom method target did not publish a checked procedure template"));
            const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                .template = template,
                .requested_fn_ty = finalized.requested_fn_ty,
                .reason = .{ .str_inspect_custom = inspect.template },
                .imported_closure = if (self.template_lookup.imported_closure) |closure|
                    if (importedClosureContainsProcedureTemplate(closure, template)) closure else null
                else
                    null,
            });
            const args = [_]Ast.ExprId{arg_expr};
            return try self.program.ast.addExpr(ret_ty, .{ .call_proc = .{
                .proc = mirProcedureRefFromReserved(reserved),
                .args = try self.program.ast.addExprSpan(&args),
                .requested_fn_ty = finalized.callable_ty,
                .requested_source_fn_ty = finalized.source_fn_ty,
                .requested_source_fn_ty_payload = finalized.requested_fn_ty,
            } });
        }

        fn strInspectCallTarget(
            self: *Self,
            arg_info: ConcreteTypeInfo,
            ret_ty: Type.TypeId,
        ) Allocator.Error!StrInspectCallTarget {
            const finalized = self.finalizedStrInspectCallTarget(ret_ty, arg_info);
            const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                .template = finalized.template,
                .requested_fn_ty = finalized.requested_fn_ty,
                .reason = .{ .str_inspect_nested = finalized.template },
                .imported_closure = finalized.imported_closure,
            });
            return .{
                .proc = mirProcedureRefFromReserved(reserved),
                .fn_ty = finalized.fn_ty,
                .source_fn_ty = finalized.source_fn_ty,
                .source_fn_ty_payload = finalized.requested_fn_ty,
            };
        }

        fn monoFunctionTypeForStrInspectCall(
            self: *Self,
            arg_ty: Type.TypeId,
            ret_ty: Type.TypeId,
        ) Allocator.Error!Type.TypeId {
            const args = try self.allocator.alloc(Type.TypeId, 1);
            args[0] = arg_ty;
            return try self.program.types.internResolved(.{ .func = .{
                .args = args,
                .lambdas = &.{},
                .ret = ret_ty,
            } });
        }

        fn strInspectFunctionTypeForArg(
            self: *Self,
            inspect: StrInspectTemplate,
            arg_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const ret_ref = try self.strInspectReturnSourceRef(inspect);
            const args = try self.allocator.alloc(checked_artifact.CheckedTypeId, 1);
            errdefer self.allocator.free(args);
            args[0] = try self.graphInstantiator().materializeConcreteRef(arg_ref);
            const ret = try self.graphInstantiator().materializeConcreteRef(ret_ref);

            const fn_root = try self.program.concrete_source_types.reservePendingLocalRoot();
            self.program.concrete_source_types.fillLocalRoot(fn_root, .{ .function = .{
                .kind = .pure,
                .args = args,
                .ret = ret,
                .needs_instantiation = false,
            } });
            return try self.graphInstantiator().sealMaterializedLocalRootRef(fn_root);
        }

        fn strInspectReturnSourceRef(
            self: *Self,
            inspect: StrInspectTemplate,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const checked_types = checkedTypesForKey(self.input, .{ .bytes = inspect.template.artifact.bytes }) orelse {
                debug.invariant(false, "mono body lowering invariant violated: Str.inspect template artifact was not available");
                unreachable;
            };
            var current = try self.program.concrete_source_types.registerArtifactRoot(
                .{ .bytes = inspect.template.artifact.bytes },
                checked_types,
                inspect.checked_fn_root,
            );
            while (true) {
                switch (self.graphInstantiator().concretePayload(current)) {
                    .alias => |alias| current = try self.graphInstantiator().concreteAliasBackingRef(current, alias),
                    .function => |function| return try self.graphInstantiator().concreteChildRef(current, function.ret),
                    else => invariantViolation("mono body lowering expected Str.inspect template type to be a function"),
                }
            }
        }

        fn strInspectTemplate(self: *Self) Allocator.Error!StrInspectTemplate {
            if (try self.currentTemplateIsStrInspect()) |current| return current;
            if (try self.strInspectTemplateInRoot()) |found| return found;
            for (self.input.imports) |imported| {
                if (try self.strInspectTemplateInImported(imported)) |found| return found;
            }
            for (self.input.root.relation_artifacts) |related| {
                if (try self.strInspectTemplateInImported(related)) |found| return found;
            }
            invariantViolation("mono body lowering could not find published Builtin.Str.inspect intrinsic wrapper");
        }

        fn currentTemplateIsStrInspect(self: *Self) Allocator.Error!?StrInspectTemplate {
            switch (self.template_lookup.template.body) {
                .intrinsic_wrapper => |wrapper_id| {
                    const wrapper = self.template_lookup.intrinsic_wrappers.get(wrapper_id);
                    if (wrapper.intrinsic != .str_inspect) return null;
                    const template = canonical.ProcedureTemplateRef{
                        .artifact = .{ .bytes = self.template_lookup.artifact.bytes },
                        .proc_base = self.template_lookup.template.proc_base,
                        .template = self.template_lookup.template.template_id,
                    };
                    return .{
                        .template = try self.name_resolver.procedureTemplateRef(template),
                        .checked_fn_root = wrapper.checked_fn_root,
                        .imported_closure = self.template_lookup.imported_closure,
                    };
                },
                else => return null,
            }
        }

        fn strInspectTemplateInRoot(self: *Self) Allocator.Error!?StrInspectTemplate {
            for (self.input.root.artifact.intrinsic_wrappers.wrappers) |wrapper| {
                if (wrapper.intrinsic != .str_inspect) continue;
                return .{
                    .template = try self.name_resolver.procedureTemplateRef(wrapper.template),
                    .checked_fn_root = wrapper.checked_fn_root,
                    .imported_closure = null,
                };
            }
            return null;
        }

        fn strInspectTemplateInImported(
            self: *Self,
            imported: checked_artifact.ImportedModuleView,
        ) Allocator.Error!?StrInspectTemplate {
            for (imported.intrinsic_wrappers.wrappers) |wrapper| {
                if (wrapper.intrinsic != .str_inspect) continue;
                const closure = self.exportedTemplateClosureForImported(imported, wrapper.template) orelse {
                    debug.invariant(false, "mono body lowering invariant violated: imported Builtin.Str.inspect intrinsic wrapper was not exported");
                    unreachable;
                };
                return .{
                    .template = try self.name_resolver.procedureTemplateRef(wrapper.template),
                    .checked_fn_root = wrapper.checked_fn_root,
                    .imported_closure = closure,
                };
            }
            return null;
        }

        fn exportedTemplateClosureForImported(
            _: *const Self,
            imported: checked_artifact.ImportedModuleView,
            template: canonical.ProcedureTemplateRef,
        ) ?checked_artifact.ImportedTemplateClosureView {
            for (imported.exported_procedure_templates.templates) |exported| {
                if (exported.template.template == template.template) return exported.template_closure;
            }
            return null;
        }

        fn lowerUnaryIntrinsicLowLevel(
            self: *Self,
            ret_ty: Type.TypeId,
            op: base.LowLevel,
            arg_expr: Ast.ExprId,
        ) Allocator.Error!Ast.ExprId {
            const args = [_]Ast.ExprId{arg_expr};
            return try self.program.ast.addExpr(ret_ty, .{ .low_level = .{
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = ret_ty,
            } });
        }

        fn lowerBoolInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
        ) Allocator.Error!Ast.ExprId {
            const true_expr = try self.program.ast.addExpr(ret_ty, .{ .str_lit = try self.program.literal_pool.intern("True") });
            const false_expr = try self.program.ast.addExpr(ret_ty, .{ .str_lit = try self.program.literal_pool.intern("False") });
            return try self.program.ast.addExpr(ret_ty, .{ .if_ = .{
                .cond = arg_expr,
                .then_body = true_expr,
                .else_body = false_expr,
            } });
        }

        fn lowerTupleInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            items: []const Type.TypeId,
        ) Allocator.Error!Ast.ExprId {
            const source_items = try self.concreteTupleElementInfos(arg_info.source_ref, items.len);
            defer if (source_items.len > 0) self.allocator.free(source_items);
            var current = try self.lowerStringLiteralExpr(ret_ty, "(");
            for (items, source_items, 0..) |item_ty, source_item, i| {
                const item_info = ConcreteTypeInfo{
                    .ty = item_ty,
                    .source_ty = source_item.source_ty,
                    .source_ref = source_item.source_ref,
                };
                if (i != 0) current = try self.lowerStrConcatBytes(ret_ty, current, ", ");
                const item_expr = try self.program.ast.addExprWithSourcePayload(item_info.ty, item_info.source_ty, item_info.source_ref, .{ .tuple_access = .{
                    .tuple = arg_expr,
                    .elem_index = @intCast(i),
                } });
                const inspected = try self.lowerStrInspectCall(ret_ty, item_expr, item_info);
                current = try self.lowerStrConcatExpr(ret_ty, current, inspected);
            }
            return try self.lowerStrConcatBytes(ret_ty, current, ")");
        }

        fn lowerRecordInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            fields: []const Type.Field,
        ) Allocator.Error!Ast.ExprId {
            if (fields.len == 0) return try self.lowerStringLiteralExpr(ret_ty, "{}");

            var current = try self.lowerStringLiteralExpr(ret_ty, "{ ");
            for (fields, 0..) |field, i| {
                const field_ref = try self.concreteRecordFieldRef(arg_info.source_ref, field.name);
                const source_field = try self.concreteTypeInfoForRef(field_ref);
                const field_info = ConcreteTypeInfo{
                    .ty = field.ty,
                    .source_ty = source_field.source_ty,
                    .source_ref = source_field.source_ref,
                };
                if (i != 0) current = try self.lowerStrConcatBytes(ret_ty, current, ", ");
                current = try self.lowerStrConcatBytes(ret_ty, current, self.program.canonical_names.recordFieldLabelText(field.name));
                current = try self.lowerStrConcatBytes(ret_ty, current, ": ");
                const field_expr = try self.program.ast.addExprWithSourcePayload(field_info.ty, field_info.source_ty, field_info.source_ref, .{ .access = .{
                    .record = arg_expr,
                    .field = field.name,
                    .field_index = @intCast(i),
                } });
                const inspected = try self.lowerStrInspectCall(ret_ty, field_expr, field_info);
                current = try self.lowerStrConcatExpr(ret_ty, current, inspected);
            }
            return try self.lowerStrConcatBytes(ret_ty, current, " }");
        }

        fn lowerListInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            elem_ty: Type.TypeId,
        ) Allocator.Error!Ast.ExprId {
            const source_elem = try self.listElementTypeFromConcrete(arg_info.source_ref);
            const elem_info = ConcreteTypeInfo{
                .ty = elem_ty,
                .source_ty = source_elem.source_ty,
                .source_ref = source_elem.source_ref,
            };
            const unit_ty = try self.ensureUnitType();
            const bool_info = try self.boolConcreteTypeInfo();
            const bool_ty = bool_info.ty;
            const u64_ty = try self.program.types.internResolved(.{ .primitive = .u64 });

            const result_symbol = try self.program.addSyntheticSymbol();
            const first_symbol = try self.program.addSyntheticSymbol();
            const list_symbol = try self.program.addSyntheticSymbol();
            const len_symbol = try self.program.addSyntheticSymbol();
            const index_symbol = try self.program.addSyntheticSymbol();
            const elem_symbol = try self.program.addSyntheticSymbol();

            const result_decl = try self.program.ast.addStmt(.{ .var_decl = .{
                .bind = .{
                    .ty = ret_ty,
                    .source_ty = .{},
                    .source_ty_payload = null,
                    .symbol = result_symbol,
                },
                .body = try self.lowerStringLiteralExpr(ret_ty, "["),
            } });
            const first_decl = try self.program.ast.addStmt(.{ .var_decl = .{
                .bind = .{
                    .ty = bool_ty,
                    .source_ty = bool_info.source_ty,
                    .source_ty_payload = bool_info.source_ref,
                    .symbol = first_symbol,
                },
                .body = try self.lowerBoolLiteral(bool_info, true),
            } });
            const list_decl = try self.program.ast.addStmt(.{ .decl = .{
                .bind = .{
                    .ty = arg_info.ty,
                    .source_ty = arg_info.source_ty,
                    .source_ty_payload = arg_info.source_ref,
                    .symbol = list_symbol,
                },
                .body = arg_expr,
            } });
            const list_for_len = try self.program.ast.addExprWithSourcePayload(arg_info.ty, arg_info.source_ty, arg_info.source_ref, .{ .var_ = list_symbol });
            const len_args = [_]Ast.ExprId{list_for_len};
            const len_decl = try self.program.ast.addStmt(.{ .decl = .{
                .bind = .{
                    .ty = u64_ty,
                    .source_ty = .{},
                    .symbol = len_symbol,
                },
                .body = try self.program.ast.addExpr(u64_ty, .{ .low_level = .{
                    .op = .list_len,
                    .rc_effect = base.LowLevel.list_len.rcEffect(),
                    .args = try self.program.ast.addExprSpan(&len_args),
                    .source_constraint_ty = u64_ty,
                } }),
            } });
            const index_decl = try self.program.ast.addStmt(.{ .var_decl = .{
                .bind = .{
                    .ty = u64_ty,
                    .source_ty = .{},
                    .symbol = index_symbol,
                },
                .body = try self.program.ast.addExpr(u64_ty, .{ .int_lit = 0 }),
            } });

            const elem_expr = try self.program.ast.addExprWithSourcePayload(elem_info.ty, elem_info.source_ty, elem_info.source_ref, .{ .var_ = elem_symbol });
            const inspected_elem = try self.lowerStrInspectCall(ret_ty, elem_expr, elem_info);
            const result_expr_for_first = try self.program.ast.addExpr(ret_ty, .{ .var_ = result_symbol });
            const first_append = try self.lowerStrConcatExpr(ret_ty, result_expr_for_first, inspected_elem);
            const first_stmts = [_]Ast.StmtId{
                try self.program.ast.addStmt(.{ .reassign = .{
                    .target = first_symbol,
                    .body = try self.lowerBoolLiteral(bool_info, false),
                } }),
                try self.program.ast.addStmt(.{ .reassign = .{
                    .target = result_symbol,
                    .body = first_append,
                } }),
            };
            const unit_first = try self.program.ast.addExpr(unit_ty, .unit);
            const then_body = try self.program.ast.addExpr(unit_ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&first_stmts),
                .final_expr = unit_first,
            } });

            const result_expr_for_rest = try self.program.ast.addExpr(ret_ty, .{ .var_ = result_symbol });
            const with_separator = try self.lowerStrConcatBytes(ret_ty, result_expr_for_rest, ", ");
            const rest_append = try self.lowerStrConcatExpr(ret_ty, with_separator, inspected_elem);
            const rest_stmts = [_]Ast.StmtId{
                try self.program.ast.addStmt(.{ .reassign = .{
                    .target = result_symbol,
                    .body = rest_append,
                } }),
            };
            const unit_rest = try self.program.ast.addExpr(unit_ty, .unit);
            const else_body = try self.program.ast.addExpr(unit_ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&rest_stmts),
                .final_expr = unit_rest,
            } });

            const first_cond = try self.program.ast.addExpr(bool_ty, .{ .var_ = first_symbol });
            const body = try self.program.ast.addExpr(unit_ty, .{ .if_ = .{
                .cond = first_cond,
                .then_body = then_body,
                .else_body = else_body,
            } });
            const list_for_get = try self.program.ast.addExprWithSourcePayload(arg_info.ty, arg_info.source_ty, arg_info.source_ref, .{ .var_ = list_symbol });
            const index_for_get = try self.program.ast.addExpr(u64_ty, .{ .var_ = index_symbol });
            const get_args = [_]Ast.ExprId{ list_for_get, index_for_get };
            const elem_decl = try self.program.ast.addStmt(.{ .decl = .{
                .bind = .{
                    .ty = elem_info.ty,
                    .source_ty = elem_info.source_ty,
                    .source_ty_payload = elem_info.source_ref,
                    .symbol = elem_symbol,
                },
                .body = try self.program.ast.addExprWithSourcePayload(elem_info.ty, elem_info.source_ty, elem_info.source_ref, .{ .low_level = .{
                    .op = .list_get_unsafe,
                    .rc_effect = base.LowLevel.list_get_unsafe.rcEffect(),
                    .args = try self.program.ast.addExprSpan(&get_args),
                    .source_constraint_ty = elem_info.ty,
                } }),
            } });
            const index_for_add = try self.program.ast.addExpr(u64_ty, .{ .var_ = index_symbol });
            const one = try self.program.ast.addExpr(u64_ty, .{ .int_lit = 1 });
            const add_args = [_]Ast.ExprId{ index_for_add, one };
            const increment_stmt = try self.program.ast.addStmt(.{ .reassign = .{
                .target = index_symbol,
                .body = try self.program.ast.addExpr(u64_ty, .{ .low_level = .{
                    .op = .num_plus,
                    .rc_effect = base.LowLevel.num_plus.rcEffect(),
                    .args = try self.program.ast.addExprSpan(&add_args),
                    .source_constraint_ty = u64_ty,
                } }),
            } });
            const body_stmt = try self.program.ast.addStmt(.{ .expr = body });
            const while_unit = try self.program.ast.addExpr(unit_ty, .unit);
            const while_body_stmts = [_]Ast.StmtId{ elem_decl, body_stmt, increment_stmt };
            const while_body = try self.program.ast.addExpr(unit_ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&while_body_stmts),
                .final_expr = while_unit,
            } });
            const index_for_cond = try self.program.ast.addExpr(u64_ty, .{ .var_ = index_symbol });
            const len_for_cond = try self.program.ast.addExpr(u64_ty, .{ .var_ = len_symbol });
            const cond_args = [_]Ast.ExprId{ index_for_cond, len_for_cond };
            const while_stmt = try self.program.ast.addStmt(.{ .while_ = .{
                .cond = try self.program.ast.addExpr(bool_ty, .{ .low_level = .{
                    .op = .num_is_lt,
                    .rc_effect = base.LowLevel.num_is_lt.rcEffect(),
                    .args = try self.program.ast.addExprSpan(&cond_args),
                    .source_constraint_ty = u64_ty,
                } }),
                .body = while_body,
            } });

            const result_before_close = try self.program.ast.addExpr(ret_ty, .{ .var_ = result_symbol });
            const final = try self.lowerStrConcatBytes(ret_ty, result_before_close, "]");
            const stmts = [_]Ast.StmtId{ result_decl, first_decl, list_decl, len_decl, index_decl, while_stmt };
            return try self.program.ast.addExpr(ret_ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&stmts),
                .final_expr = final,
            } });
        }

        fn lowerBoxInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            payload_ty: Type.TypeId,
        ) Allocator.Error!Ast.ExprId {
            const payload_info = try self.boxPayloadTypeFromConcrete(arg_info.source_ref, payload_ty);
            const args = [_]Ast.ExprId{arg_expr};
            const unboxed = try self.program.ast.addExprWithSourcePayload(payload_info.ty, payload_info.source_ty, payload_info.source_ref, .{ .low_level = .{
                .op = .box_unbox,
                .rc_effect = base.LowLevel.box_unbox.rcEffect(),
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = payload_info.ty,
            } });
            const inspected = try self.lowerStrInspectCall(ret_ty, unboxed, payload_info);
            const with_open = try self.lowerStrConcatExpr(ret_ty, try self.lowerStringLiteralExpr(ret_ty, "Box("), inspected);
            return try self.lowerStrConcatBytes(ret_ty, with_open, ")");
        }

        fn lowerNominalInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            nominal: Type.Nominal,
        ) Allocator.Error!Ast.ExprId {
            if (nominal.is_opaque) return try self.lowerStringLiteralExpr(ret_ty, "<opaque>");

            return try self.lowerDefaultStrInspectIntrinsic(
                ret_ty,
                arg_expr,
                arg_info,
                self.program.types.getType(nominal.backing),
            );
        }

        fn lowerTagUnionInspectIntrinsic(
            self: *Self,
            ret_ty: Type.TypeId,
            arg_expr: Ast.ExprId,
            arg_info: ConcreteTypeInfo,
            tags: []const Type.Tag,
        ) Allocator.Error!Ast.ExprId {
            if (tags.len == 0) invariantViolation("Str.inspect intrinsic reached an uninhabited tag union");

            var branches = std.ArrayList(Ast.Branch).empty;
            defer branches.deinit(self.allocator);
            for (tags, 0..) |tag, tag_index| {
                const payload_infos = if (tag.args.len == 0)
                    &[_]ConcreteTypeInfo{}
                else
                    try self.concreteTagPayloadInfosForUnionType(arg_info.source_ref, tag.name);
                defer if (payload_infos.len > 0) self.allocator.free(payload_infos);
                if (payload_infos.len != tag.args.len) invariantViolation("Str.inspect tag payload source count disagreed with tag type");

                const payload_pats = try self.allocator.alloc(Ast.PatId, tag.args.len);
                defer self.allocator.free(payload_pats);
                const payload_exprs = try self.allocator.alloc(Ast.ExprId, tag.args.len);
                defer self.allocator.free(payload_exprs);
                const actual_payload_infos = try self.allocator.alloc(ConcreteTypeInfo, tag.args.len);
                defer self.allocator.free(actual_payload_infos);

                for (tag.args, 0..) |payload_ty, payload_index| {
                    actual_payload_infos[payload_index] = .{
                        .ty = payload_ty,
                        .source_ty = payload_infos[payload_index].source_ty,
                        .source_ref = payload_infos[payload_index].source_ref,
                    };
                    const symbol = try self.program.addSyntheticSymbol();
                    payload_pats[payload_index] = try self.program.ast.addPat(.{
                        .ty = payload_ty,
                        .source_ty = actual_payload_infos[payload_index].source_ty,
                        .source_ty_payload = actual_payload_infos[payload_index].source_ref,
                        .data = .{ .var_ = symbol },
                    });
                    payload_exprs[payload_index] = try self.program.ast.addExprWithSourcePayload(payload_ty, actual_payload_infos[payload_index].source_ty, actual_payload_infos[payload_index].source_ref, .{ .var_ = symbol });
                }

                const pat = try self.program.ast.addPat(.{
                    .ty = arg_info.ty,
                    .source_ty = arg_info.source_ty,
                    .source_ty_payload = arg_info.source_ref,
                    .data = .{ .tag = .{
                        .name = tag.name,
                        .discriminant = @intCast(tag_index),
                        .args = try self.program.ast.addPatSpan(payload_pats),
                    } },
                });

                try branches.append(self.allocator, .{
                    .pat = pat,
                    .body = try self.lowerTagInspectBranch(ret_ty, tag, payload_exprs, actual_payload_infos),
                });
            }

            return try self.program.ast.addExpr(ret_ty, .{ .match_ = .{
                .cond = arg_expr,
                .branches = try self.program.ast.addBranchSpan(branches.items),
                .is_try_suffix = false,
            } });
        }

        fn lowerTagInspectBranch(
            self: *Self,
            ret_ty: Type.TypeId,
            tag: Type.Tag,
            payload_exprs: []const Ast.ExprId,
            payload_infos: []const ConcreteTypeInfo,
        ) Allocator.Error!Ast.ExprId {
            const tag_name = self.program.canonical_names.tagLabelText(tag.name);
            if (tag.args.len != payload_exprs.len) invariantViolation("Str.inspect tag payload count disagreed with tag type");
            if (payload_infos.len != payload_exprs.len) invariantViolation("Str.inspect tag payload source count disagreed with tag type");
            if (tag.args.len == 0) return try self.lowerStringLiteralExpr(ret_ty, tag_name);

            var current = try self.lowerStringLiteralExpr(ret_ty, tag_name);
            current = try self.lowerStrConcatBytes(ret_ty, current, "(");
            for (payload_infos, payload_exprs, 0..) |payload_info, payload_expr, i| {
                if (i != 0) current = try self.lowerStrConcatBytes(ret_ty, current, ", ");
                const inspected = try self.lowerStrInspectCall(ret_ty, payload_expr, payload_info);
                current = try self.lowerStrConcatExpr(ret_ty, current, inspected);
            }
            return try self.lowerStrConcatBytes(ret_ty, current, ")");
        }

        fn lowerStringLiteralExpr(
            self: *Self,
            ret_ty: Type.TypeId,
            bytes: []const u8,
        ) Allocator.Error!Ast.ExprId {
            return try self.program.ast.addExpr(ret_ty, .{ .str_lit = try self.program.literal_pool.intern(bytes) });
        }

        fn lowerStrConcatBytes(
            self: *Self,
            ret_ty: Type.TypeId,
            lhs: Ast.ExprId,
            rhs_bytes: []const u8,
        ) Allocator.Error!Ast.ExprId {
            return try self.lowerStrConcatExpr(ret_ty, lhs, try self.lowerStringLiteralExpr(ret_ty, rhs_bytes));
        }

        fn lowerStrConcatExpr(
            self: *Self,
            ret_ty: Type.TypeId,
            lhs: Ast.ExprId,
            rhs: Ast.ExprId,
        ) Allocator.Error!Ast.ExprId {
            const args = [_]Ast.ExprId{ lhs, rhs };
            return try self.program.ast.addExpr(ret_ty, .{ .low_level = .{
                .op = .str_concat,
                .rc_effect = base.LowLevel.str_concat.rcEffect(),
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = ret_ty,
            } });
        }

        fn finalizePromotedCallableWrapperGraph(
            self: *Self,
            reserved: ReservedMonoProc,
            wrapper_id: canonical.PromotedCallableWrapperId,
        ) Allocator.Error!void {
            if (self.graph.finalized_promoted_wrappers.contains(wrapper_id)) {
                invariantViolation("mono graph finalized one promoted callable wrapper more than once");
            }
            const wrapper = self.template_lookup.promoted_callable_wrappers.get(wrapper_id);
            const body_plan = self.template_lookup.promoted_callable_body_plans.get(wrapper.body_plan);
            const finite = switch (body_plan) {
                .finite => |finite| finite,
                .erased => invariantViolation("mono graph finalization reached executable-owned erased promoted callable wrapper"),
                .pending => invariantViolation("mono graph finalization reached unsealed promoted callable wrapper body plan"),
            };

            const wrapper_source_ref = try self.graphInstantiator().concreteRefForTemplateType(self.template_lookup.template.checked_fn_root);
            const wrapper_source_key = self.program.concrete_source_types.key(wrapper_source_ref);
            if (!std.mem.eql(u8, &reserved.proc.specialization.requested_mono_fn_ty.bytes, &wrapper_source_key.bytes)) {
                invariantViolation("mono graph finalization promoted callable wrapper source function type disagrees with mono specialization request");
            }

            const member_source_ref = try self.graphInstantiator().concreteRefForTemplateType(finite.member_proc_source_fn_ty_payload);
            const param_infos = try self.finalizePromotedWrapperParams(finite.params);
            errdefer if (param_infos.len != 0) self.allocator.free(param_infos);
            for (finite.captures) |capture| try self.finalizePrivateCaptureRef(capture);
            for (finite.call_args) |arg| switch (arg) {
                .param => {},
                .private_capture => |capture| try self.finalizePrivateCaptureRef(capture),
            };
            try self.graph.finalized_promoted_wrappers.put(wrapper_id, .{
                .wrapper_source_ref = wrapper_source_ref,
                .wrapper_source_key = wrapper_source_key,
                .member_source_ref = member_source_ref,
                .param_infos = param_infos,
            });
        }

        fn finalizePromotedWrapperParams(
            self: *Self,
            params: []const checked_artifact.PromotedWrapperParam,
        ) Allocator.Error![]const ConcreteTypeInfo {
            if (params.len == 0) return &.{};
            const out = try self.allocator.alloc(ConcreteTypeInfo, params.len);
            errdefer self.allocator.free(out);
            const seen = try self.allocator.alloc(bool, params.len);
            defer self.allocator.free(seen);
            @memset(seen, false);

            for (params) |param| {
                const index: usize = @intCast(param.index);
                if (index >= params.len or seen[index]) {
                    invariantViolation("mono graph finalization promoted callable wrapper params are not a dense unique index set");
                }
                const source_ref = try self.graphInstantiator().concreteRefForTemplateType(param.checked_ty);
                const info = try self.runtimeConcreteTypeInfo(source_ref);
                if (!std.mem.eql(u8, &info.source_ty.bytes, &param.source_ty.bytes)) {
                    invariantViolation("mono graph finalization promoted wrapper param source type disagreed with checked plan");
                }
                out[index] = info;
                seen[index] = true;
            }
            for (seen) |was_seen| {
                if (!was_seen) invariantViolation("mono graph finalization promoted callable wrapper omitted a parameter index");
            }
            return out;
        }

        fn finalizedPromotedWrapper(
            self: *const Self,
            wrapper_id: canonical.PromotedCallableWrapperId,
        ) FinalizedPromotedWrapper {
            return self.graph.finalized_promoted_wrappers.get(wrapper_id) orelse {
                invariantViolation("mono body emission reached promoted callable wrapper before graph finalization published it");
            };
        }

        fn finalizePrivateCaptureRef(
            self: *Self,
            capture: checked_artifact.PrivateCaptureRef,
        ) Allocator.Error!void {
            try self.finalizePrivateCaptureNode(capture.artifact, capture.node, capture.source_ty_payload);
        }

        fn finalizePrivateCaptureNode(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            node_id: checked_artifact.PrivateCaptureNodeId,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!void {
            const lowering_key = PrivateCaptureLoweringKey{
                .artifact = artifact,
                .node = node_id,
                .checked_ty = checked_ty,
            };
            if (self.active_private_captures.contains(lowering_key)) {
                invariantViolation("mono graph finalization reached a cyclic private capture value before it had an explicit bound value");
            }
            try self.active_private_captures.put(lowering_key, {});
            defer _ = self.active_private_captures.remove(lowering_key);

            _ = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            const plans = comptimePlansForKey(self.input, artifact) orelse {
                debug.invariant(false, "mono graph finalization invariant violated: private capture plan artifact was not available");
                unreachable;
            };
            const node = plans.privateCapture(node_id);
            const checked_types = checkedTypesForKey(self.input, artifact) orelse {
                debug.invariant(false, "mono graph finalization invariant violated: private capture type artifact was not available");
                unreachable;
            };
            switch (node) {
                .pending => invariantViolation("mono graph finalization reached pending private capture node"),
                .const_instance_leaf,
                .finite_callable_leaf,
                => {},
                .record => |fields| {
                    for (fields) |field| {
                        const field_ty = privateRecordFieldTypeForType(checked_types, checked_ty, field.field) orelse {
                            invariantViolation("private capture record field is not present in checked type");
                        };
                        try self.finalizePrivateCaptureNode(artifact, field.value, field_ty);
                    }
                },
                .tuple => |items| {
                    const elem_tys = privateTupleElems(checked_types, checked_ty);
                    if (elem_tys.len != items.len) invariantViolation("private capture tuple arity disagrees with checked type");
                    for (items, elem_tys) |item, elem_ty| {
                        try self.finalizePrivateCaptureNode(artifact, item, elem_ty);
                    }
                },
                .tag_union => |tag| {
                    const checked_tag = privateTagTypeForType(checked_types, checked_ty, tag.tag) orelse {
                        invariantViolation("private capture tag label is not present in checked type");
                    };
                    if (checked_tag.args.len != tag.payloads.len) {
                        invariantViolation("private capture tag payload count disagrees with checked type");
                    }
                    for (tag.payloads, checked_tag.args) |payload_ref, payload_ty| {
                        try self.finalizePrivateCaptureNode(artifact, payload_ref.value, payload_ty);
                    }
                },
                .list => |items| {
                    const elem_ty = privateBuiltinArgType(checked_types, checked_ty, .list);
                    for (items) |item| try self.finalizePrivateCaptureNode(artifact, item, elem_ty);
                },
                .box => |payload| try self.finalizePrivateCaptureNode(
                    artifact,
                    payload,
                    privateBuiltinArgType(checked_types, checked_ty, .box),
                ),
                .nominal => |nominal| switch (checkedTypePayload(checked_types, checked_ty)) {
                    .nominal => |nominal_ty| try self.finalizePrivateCaptureNode(artifact, nominal.backing, nominal_ty.backing),
                    .alias => |alias| try self.finalizePrivateCaptureNode(artifact, nominal.backing, alias.backing),
                    else => invariantViolation("private capture nominal node had non-nominal checked type"),
                },
                .recursive_ref => |ref| try self.finalizePrivateCaptureNode(artifact, ref, checked_ty),
            }
        }

        fn lowerPromotedCallableWrapperDef(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
            wrapper_id: canonical.PromotedCallableWrapperId,
        ) Allocator.Error!Ast.DefId {
            const wrapper = self.template_lookup.promoted_callable_wrappers.get(wrapper_id);
            const body_plan = self.template_lookup.promoted_callable_body_plans.get(wrapper.body_plan);
            const finalized = self.finalizedPromotedWrapper(wrapper_id);
            const lowered = switch (body_plan) {
                .finite => |finite| blk: {
                    const params = try self.lowerPromotedWrapperParamBundle(finite.params, finalized.param_infos);
                    defer if (params.exprs.len > 0) self.allocator.free(params.exprs);
                    break :blk PromotedWrapperLowering{
                        .args = params.args,
                        .body = try self.lowerFinitePromotedCallableWrapperBody(reserved, fn_ty, wrapper_id, finite, finalized, params.exprs),
                    };
                },
                .erased => invariantViolation("mono body lowering reached executable-owned erased promoted callable wrapper"),
                .pending => invariantViolation("mono body lowering reached unsealed promoted callable wrapper body plan"),
            };
            const bind = Ast.TypedSymbol{
                .ty = fn_ty,
                .source_ty = reserved.proc.specialization.requested_mono_fn_ty,
                .source_ty_payload = reserved.requested_fn_ty,
                .symbol = try self.program.addProcSymbol(reserved.local_handle),
            };
            return try self.program.ast.addDef(.{
                .proc = mirProcedureRefFromReserved(reserved),
                .debug_name = null,
                .value = .{ .fn_ = .{
                    .source_fn_ty = reserved.proc.specialization.requested_mono_fn_ty,
                    .source_fn_ty_payload = reserved.requested_fn_ty,
                    .recursive = false,
                    .bind = bind,
                    .args = lowered.args,
                    .body = lowered.body,
                } },
            });
        }

        const PromotedWrapperLowering = struct {
            args: Ast.Span(Ast.TypedSymbol),
            body: Ast.ExprId,
        };

        fn lowerFinitePromotedCallableWrapperBody(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
            wrapper_id: canonical.PromotedCallableWrapperId,
            finite: checked_artifact.FinitePromotedWrapperBodyPlan,
            finalized: FinalizedPromotedWrapper,
            params: []const Ast.ExprId,
        ) Allocator.Error!Ast.ExprId {
            if (!std.mem.eql(u8, &reserved.proc.specialization.requested_mono_fn_ty.bytes, &finalized.wrapper_source_key.bytes)) {
                invariantViolation("promoted callable wrapper instantiated source function type disagrees with mono specialization request");
            }
            if (finite.member_capture_slots.len != finite.captures.len) {
                invariantViolation("promoted callable wrapper capture refs disagree with member capture slots");
            }

            const capture_args = try self.allocator.alloc(Ast.CaptureArg, finite.captures.len);
            defer self.allocator.free(capture_args);
            for (finite.captures, 0..) |capture, i| {
                const slot = finite.member_capture_slots[i];
                if (slot.slot != @as(u32, @intCast(i))) {
                    invariantViolation("promoted callable wrapper member capture slot order is not canonical");
                }
                capture_args[i] = .{
                    .slot = slot.slot,
                    .symbol = try self.program.addSyntheticSymbol(),
                    .expr = try self.lowerPrivateCaptureExpr(capture),
                };
            }

            const member_proc = try self.reserveFinitePromotedWrapperMemberProcedure(finite, finalized.member_source_ref, .{ .promoted_callable_wrapper = wrapper_id });
            var member_target = try self.lowerPromotedMemberTarget(finite.member_target, finite.member_proc, member_proc);
            var member_target_owned = true;
            errdefer if (member_target_owned) deinitExecutableSpecializationKeyForMono(self.allocator, &member_target);
            const proc_value = try self.program.ast.addExprWithSourcePayload(fn_ty, finalized.wrapper_source_key, finalized.wrapper_source_ref, .{ .proc_value = .{
                .proc = member_proc,
                .published_proc = publishedMirProcedureRefForCallable(finite.member_proc),
                .captures = try self.program.ast.addCaptureArgSpan(capture_args),
                .fn_ty = fn_ty,
                .forced_target = .{
                    .key = member_target,
                    .artifact = self.template_lookup.artifact,
                    .payloads = self.template_lookup.executable_type_payloads,
                    .promoted_wrapper = finite.member_target_promoted_wrapper,
                },
            } });
            member_target_owned = false;

            const call_args = try self.allocator.alloc(Ast.ExprId, finite.call_args.len);
            defer self.allocator.free(call_args);
            for (finite.call_args, 0..) |arg, i| {
                call_args[i] = switch (arg) {
                    .param => |index| blk: {
                        const param_index: usize = @intCast(index);
                        if (param_index >= params.len) {
                            invariantViolation("promoted callable wrapper call arg referenced a missing parameter");
                        }
                        break :blk params[param_index];
                    },
                    .private_capture => |capture| try self.lowerPrivateCaptureExpr(capture),
                };
            }

            const root_body = self.graph.body_instances.get(.root) orelse {
                invariantViolation("mono body emission promoted wrapper had no finalized root body");
            };
            const ret_ty = root_body.ret_ty;
            return try self.program.ast.addExprWithSourcePayload(ret_ty.ty, ret_ty.source_ty, ret_ty.source_ref, .{ .call_value = .{
                .func = proc_value,
                .args = try self.program.ast.addExprSpan(call_args),
                .requested_fn_ty = fn_ty,
                .requested_source_fn_ty = finalized.wrapper_source_key,
                .requested_source_fn_ty_payload = reserved.requested_fn_ty,
            } });
        }

        fn reserveFinitePromotedWrapperMemberProcedure(
            self: *Self,
            finite: checked_artifact.FinitePromotedWrapperBodyPlan,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
            reason: MonoSpecializationReason,
        ) Allocator.Error!canonical.MirProcedureRef {
            const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
            const payload_key = checkedTypeKey(self.templateCheckedTypes(), finite.member_proc_source_fn_ty_payload);
            if (!std.mem.eql(u8, &payload_key.bytes, &finite.member_proc.source_fn_ty.bytes)) {
                invariantViolation("promoted callable wrapper member source type payload disagrees with member procedure");
            }

            const remapped_callable = try self.name_resolver.procedureCallableRef(finite.member_proc);
            var concrete_callable = remapped_callable;
            concrete_callable.source_fn_ty = requested_key;
            return switch (concrete_callable.template) {
                .checked,
                .synthetic,
                => blk: {
                    const template = checkedTemplateFromCallableTemplate(concrete_callable.template);
                    const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                        .template = template,
                        .callable_template = concrete_callable.template,
                        .requested_fn_ty = requested_fn_ty,
                        .reason = reason,
                        .imported_closure = finite.member_proc_template_closure,
                    });
                    break :blk .{
                        .proc = reserved.proc.proc,
                        .callable = concrete_callable,
                    };
                },
                .lifted => |lifted| try self.reserveFinitePromotedWrapperLiftedMemberProcedure(
                    concrete_callable,
                    lifted,
                    finite.member_lifted_owner_source_fn_ty_payload orelse {
                        invariantViolation("promoted callable wrapper lifted member has no owner source type payload");
                    },
                    finite.member_lifted_owner_template_closure,
                    reason,
                ),
            };
        }

        fn reserveFinitePromotedWrapperLiftedMemberProcedure(
            self: *Self,
            callable: canonical.ProcedureCallableRef,
            lifted: canonical.LiftedProcedureTemplateRef,
            owner_source_fn_ty_payload: checked_artifact.CheckedTypeId,
            imported_closure: ?checked_artifact.ImportedTemplateClosureView,
            reason: MonoSpecializationReason,
        ) Allocator.Error!canonical.MirProcedureRef {
            const owner_key = lifted.owner_mono_specialization;
            const owner_requested_key = checkedTypeKey(self.templateCheckedTypes(), owner_source_fn_ty_payload);
            if (!std.mem.eql(u8, &owner_requested_key.bytes, &owner_key.requested_mono_fn_ty.bytes)) {
                invariantViolation("promoted callable wrapper lifted owner source type payload disagrees with owner specialization");
            }
            const owner_requested_fn_ty = try self.program.concrete_source_types.registerArtifactRoot(
                self.template_lookup.artifact,
                self.templateCheckedTypes(),
                owner_source_fn_ty_payload,
            );
            const owner_reserved_key = self.program.concrete_source_types.key(owner_requested_fn_ty);
            if (!std.mem.eql(u8, &owner_reserved_key.bytes, &owner_key.requested_mono_fn_ty.bytes)) {
                invariantViolation("promoted callable wrapper lifted owner source function type disagrees with registered payload");
            }

            _ = try self.queue.reserve(&self.program.concrete_source_types, .{
                .template = owner_key.template,
                .requested_fn_ty = owner_requested_fn_ty,
                .reason = reason,
                .imported_closure = imported_closure,
            });

            const owner_base = self.program.canonical_names.procBase(owner_key.template.proc_base);
            const proc_base = try self.program.canonical_names.internProcBase(.{
                .module_name = owner_base.module_name,
                .export_name = null,
                .kind = .checked_source,
                .ordinal = @intFromEnum(lifted.site),
                .nested_proc_site = .{
                    .owner_template = owner_key.template,
                    .site = lifted.site,
                },
                .owner_mono_specialization = owner_key,
            });
            return .{
                .proc = .{
                    .artifact = owner_key.template.artifact,
                    .proc_base = proc_base,
                },
                .callable = callable,
            };
        }

        fn lowerPromotedMemberTarget(
            self: *Self,
            target: checked_artifact.CallableResultMemberTargetPlan,
            member_proc_value: canonical.ProcedureCallableRef,
            reserved_member_proc: canonical.MirProcedureRef,
        ) Allocator.Error!canonical.ExecutableSpecializationKey {
            return switch (target) {
                .artifact_owned => |key| blk: {
                    const member_target_artifact = checked_artifact.CheckedModuleArtifactKey{
                        .bytes = callableTemplateArtifact(member_proc_value.template).bytes,
                    };
                    var remapped = try self.remapExecutableSpecializationKeyForArtifact(key, member_target_artifact);
                    remapped.requested_fn_ty = reserved_member_proc.callable.source_fn_ty;
                    break :blk remapped;
                },
                .member_proc_relative => |endpoint| .{
                    .base = reserved_member_proc.proc.proc_base,
                    .requested_fn_ty = reserved_member_proc.callable.source_fn_ty,
                    .exec_arg_tys = if (endpoint.exec_arg_tys.len == 0)
                        &.{}
                    else
                        try self.allocator.dupe(canonical.CanonicalExecValueTypeKey, endpoint.exec_arg_tys),
                    .exec_ret_ty = endpoint.exec_ret_ty,
                    .callable_repr_mode = endpoint.callable_repr_mode,
                    .capture_shape_key = endpoint.capture_shape_key,
                },
            };
        }

        fn remapExecutableSpecializationKeyForArtifact(
            self: *Self,
            key: canonical.ExecutableSpecializationKey,
            artifact: checked_artifact.CheckedModuleArtifactKey,
        ) Allocator.Error!canonical.ExecutableSpecializationKey {
            var out = try cloneExecutableSpecializationKeyForMono(self.allocator, key);
            errdefer deinitExecutableSpecializationKeyForMono(self.allocator, &out);
            out.base = try self.name_resolver.procBase(artifact, key.base);
            return out;
        }

        const PromotedWrapperParamBundle = struct {
            args: Ast.Span(Ast.TypedSymbol),
            exprs: []const Ast.ExprId,
        };

        fn lowerPromotedWrapperParamBundle(
            self: *Self,
            params: []const checked_artifact.PromotedWrapperParam,
            param_infos: []const ConcreteTypeInfo,
        ) Allocator.Error!PromotedWrapperParamBundle {
            if (params.len != param_infos.len) {
                invariantViolation("mono body emission promoted wrapper param count disagreed with finalized graph");
            }
            if (params.len == 0) return .{
                .args = Ast.Span(Ast.TypedSymbol).empty(),
                .exprs = &.{},
            };
            const lowered_args = try self.allocator.alloc(Ast.TypedSymbol, params.len);
            defer self.allocator.free(lowered_args);
            const lowered_exprs = try self.allocator.alloc(Ast.ExprId, params.len);
            errdefer self.allocator.free(lowered_exprs);
            const seen = try self.allocator.alloc(bool, params.len);
            defer self.allocator.free(seen);
            @memset(seen, false);

            for (params) |param| {
                const index: usize = @intCast(param.index);
                if (index >= params.len or seen[index]) {
                    invariantViolation("promoted callable wrapper params are not a dense unique index set");
                }
                const info = param_infos[index];
                const symbol = try self.program.addSyntheticSymbol();
                lowered_args[index] = .{
                    .ty = info.ty,
                    .source_ty = info.source_ty,
                    .source_ty_payload = info.source_ref,
                    .symbol = symbol,
                };
                lowered_exprs[index] = try self.program.ast.addExprWithSourcePayload(info.ty, info.source_ty, info.source_ref, .{ .var_ = symbol });
                seen[index] = true;
            }
            for (seen) |was_seen| {
                if (!was_seen) invariantViolation("promoted callable wrapper omitted a parameter index");
            }
            return .{
                .args = try self.program.ast.addTypedSymbolSpan(lowered_args),
                .exprs = lowered_exprs,
            };
        }

        fn lowerPrivateCaptureExpr(
            self: *Self,
            capture: checked_artifact.PrivateCaptureRef,
        ) Allocator.Error!Ast.ExprId {
            const checked_types = checkedTypesForKey(self.input, capture.artifact) orelse {
                debug.invariant(false, "mono body lowering invariant violated: private capture artifact was not available");
                unreachable;
            };
            const root_index: usize = @intFromEnum(capture.source_ty_payload);
            if (root_index >= checked_types.roots.len) {
                invariantViolation("private capture source type payload is outside checked type roots");
            }
            return try self.lowerPrivateCaptureNode(capture.artifact, capture.node, capture.source_ty_payload);
        }

        fn lowerPrivateCaptureNode(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            node_id: checked_artifact.PrivateCaptureNodeId,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!Ast.ExprId {
            const lowering_key = PrivateCaptureLoweringKey{
                .artifact = artifact,
                .node = node_id,
                .checked_ty = checked_ty,
            };
            if (self.lowered_private_captures.get(lowering_key)) |existing| return existing;
            if (self.active_private_captures.contains(lowering_key)) {
                invariantViolation("mono body lowering reached a cyclic private capture value before it had an explicit bound value");
            }
            try self.active_private_captures.put(lowering_key, {});
            errdefer _ = self.active_private_captures.remove(lowering_key);

            const plans = comptimePlansForKey(self.input, artifact) orelse {
                debug.invariant(false, "mono body lowering invariant violated: private capture plan artifact was not available");
                unreachable;
            };
            const node = plans.privateCapture(node_id);
            const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            const ty = type_info.ty;
            const source_ty = type_info.source_ty;

            const lowered = switch (node) {
                .pending => invariantViolation("mono body lowering reached pending private capture node"),
                .const_instance_leaf => |leaf| try self.lowerPrivateConstInstanceLeaf(ty, leaf),
                .finite_callable_leaf => |leaf| try self.lowerPrivateCallableLeaf(ty, source_ty, node_id, leaf),
                .record => |fields| try self.lowerPrivateRecordCapture(artifact, ty, checked_ty, fields),
                .tuple => |items| try self.lowerPrivateTupleCapture(artifact, ty, checked_ty, items),
                .tag_union => |tag| try self.lowerPrivateTagCapture(artifact, ty, checked_ty, tag),
                .list => |items| try self.lowerPrivateListCapture(artifact, ty, checked_ty, items),
                .box => |payload| try self.lowerPrivateBoxCapture(artifact, ty, checked_ty, payload),
                .nominal => |nominal| try self.lowerPrivateNominalCapture(artifact, ty, checked_ty, nominal),
                .recursive_ref => |ref| try self.lowerPrivateCaptureNode(artifact, ref, checked_ty),
            };
            _ = self.active_private_captures.remove(lowering_key);
            try self.lowered_private_captures.put(lowering_key, lowered);
            return lowered;
        }

        fn lowerPrivateConstInstanceLeaf(
            self: *Self,
            ty: Type.TypeId,
            leaf: checked_artifact.PrivateCaptureConstLeaf,
        ) Allocator.Error!Ast.ExprId {
            if (!checked_artifact.constInstantiationKeyEql(leaf.const_instance.key, .{
                .const_ref = leaf.const_ref,
                .requested_source_ty = leaf.requested_source_ty,
            })) {
                invariantViolation("private capture const leaf instance key disagrees with published const ref and requested source type");
            }
            var dependency_state = ConcreteDependencyReservationState.init(self.allocator);
            defer dependency_state.deinit();
            try reserveConstInstanceRefDependencies(self.input, self.program, self.queue, &dependency_state, leaf.const_instance);
            const source_ref = try self.concreteSourceTypeForCheckedKey(leaf.const_ref.artifact, leaf.requested_source_ty);
            return try self.program.ast.addExprWithSourcePayload(ty, leaf.requested_source_ty, source_ref, .{ .const_instance = leaf.const_instance });
        }

        fn lowerPrivateCallableLeaf(
            self: *Self,
            ty: Type.TypeId,
            source_ty: canonical.CanonicalTypeKey,
            node_id: checked_artifact.PrivateCaptureNodeId,
            leaf: checked_artifact.FiniteCallableLeafInstance,
        ) Allocator.Error!Ast.ExprId {
            if (!std.mem.eql(u8, &leaf.proc_value.source_fn_ty.bytes, &source_ty.bytes)) {
                invariantViolation("private finite callable leaf source function type disagrees with materialization type");
            }
            const template_artifact = callableTemplateArtifact(leaf.proc_value.template);
            const artifact = artifactKeyForRef(self.input, template_artifact) orelse {
                debug.invariant(false, "mono body lowering invariant violated: private callable leaf template artifact was not available");
                unreachable;
            };
            const concrete = try self.concreteSourceTypeForCheckedKey(artifact, source_ty);
            const proc = try self.reserveCallableProcedure(
                leaf.proc_value,
                concrete,
                .{ .private_capture_callable_leaf = node_id },
            );
            return try self.program.ast.addExprWithSourcePayload(ty, source_ty, concrete, .{ .proc_value = .{
                .proc = proc,
                .published_proc = publishedMirProcedureRefForCallable(leaf.proc_value),
                .captures = Ast.Span(Ast.CaptureArg).empty(),
                .fn_ty = ty,
            } });
        }

        fn lowerPrivateRecordCapture(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            ty: Type.TypeId,
            checked_ty: checked_artifact.CheckedTypeId,
            fields: []const checked_artifact.PrivateCaptureRecordField,
        ) Allocator.Error!Ast.ExprId {
            const checked_types = checkedTypesForKey(self.input, artifact) orelse unreachable;
            const record_field_count = privateRecordFieldCount(checked_types, checked_ty);
            if (record_field_count != fields.len) {
                invariantViolation("private capture record field count disagrees with checked type");
            }
            const lowered = try self.allocator.alloc(Ast.FieldExpr, fields.len);
            defer self.allocator.free(lowered);
            for (fields, 0..) |field, i| {
                const field_ty = privateRecordFieldTypeForType(checked_types, checked_ty, field.field) orelse {
                    invariantViolation("private capture record field is not present in checked type");
                };
                lowered[i] = .{
                    .field = field.field,
                    .value = try self.lowerPrivateCaptureNode(artifact, field.value, field_ty),
                };
            }
            const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            return try self.program.ast.addExprWithSourcePayload(
                ty,
                type_info.source_ty,
                type_info.source_ref,
                .{ .record = try self.program.ast.addFieldExprSpan(lowered) },
            );
        }

        fn lowerPrivateTupleCapture(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            ty: Type.TypeId,
            checked_ty: checked_artifact.CheckedTypeId,
            items: []const checked_artifact.PrivateCaptureNodeId,
        ) Allocator.Error!Ast.ExprId {
            const checked_types = checkedTypesForKey(self.input, artifact) orelse unreachable;
            const elem_tys = privateTupleElems(checked_types, checked_ty);
            if (elem_tys.len != items.len) {
                invariantViolation("private capture tuple arity disagrees with checked type");
            }
            const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
            defer self.allocator.free(lowered);
            for (items, 0..) |item, i| {
                lowered[i] = try self.lowerPrivateCaptureNode(artifact, item, elem_tys[i]);
            }
            const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            return try self.program.ast.addExprWithSourcePayload(
                ty,
                type_info.source_ty,
                type_info.source_ref,
                .{ .tuple = try self.program.ast.addExprSpan(lowered) },
            );
        }

        fn lowerPrivateTagCapture(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            ty: Type.TypeId,
            checked_ty: checked_artifact.CheckedTypeId,
            tag: checked_artifact.PrivateCaptureTagNode,
        ) Allocator.Error!Ast.ExprId {
            const checked_types = checkedTypesForKey(self.input, artifact) orelse unreachable;
            const checked_tag = privateTagTypeForType(checked_types, checked_ty, tag.tag) orelse {
                invariantViolation("private capture tag label is not present in checked type");
            };
            if (checked_tag.args.len != tag.payloads.len) {
                invariantViolation("private capture tag payload count disagrees with checked type");
            }
            const lowered = try self.allocator.alloc(Ast.ExprId, tag.payloads.len);
            defer self.allocator.free(lowered);
            for (tag.payloads, 0..) |payload_ref, i| {
                if (payload_ref.index != @as(u32, @intCast(i))) {
                    invariantViolation("private capture tag payloads are not in canonical index order");
                }
                lowered[i] = try self.lowerPrivateCaptureNode(artifact, payload_ref.value, checked_tag.args[i]);
            }
            const tag_info = self.tagInfoForUnionType(ty, tag.tag);
            if (tag_info.payload_count != lowered.len) {
                invariantViolation("private capture tag payload count disagrees with finalized tag info");
            }
            const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            return try self.program.ast.addExprWithSourcePayload(ty, type_info.source_ty, type_info.source_ref, .{ .tag = .{
                .name = tag.tag,
                .discriminant = tag_info.discriminant,
                .args = try self.program.ast.addExprSpan(lowered),
                .constructor_ty = ty,
            } });
        }

        fn lowerPrivateListCapture(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            ty: Type.TypeId,
            checked_ty: checked_artifact.CheckedTypeId,
            items: []const checked_artifact.PrivateCaptureNodeId,
        ) Allocator.Error!Ast.ExprId {
            const elem_ty = privateBuiltinArgType(checkedTypesForKey(self.input, artifact) orelse unreachable, checked_ty, .list);
            const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
            defer self.allocator.free(lowered);
            for (items, 0..) |item, i| {
                lowered[i] = try self.lowerPrivateCaptureNode(artifact, item, elem_ty);
            }
            const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            return try self.program.ast.addExprWithSourcePayload(
                ty,
                type_info.source_ty,
                type_info.source_ref,
                .{ .list = try self.program.ast.addExprSpan(lowered) },
            );
        }

        fn lowerPrivateBoxCapture(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            ty: Type.TypeId,
            checked_ty: checked_artifact.CheckedTypeId,
            payload: checked_artifact.PrivateCaptureNodeId,
        ) Allocator.Error!Ast.ExprId {
            const payload_ty = privateBuiltinArgType(checkedTypesForKey(self.input, artifact) orelse unreachable, checked_ty, .box);
            const child = try self.lowerPrivateCaptureNode(artifact, payload, payload_ty);
            const args = [_]Ast.ExprId{child};
            const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
            return try self.program.ast.addExprWithSourcePayload(ty, type_info.source_ty, type_info.source_ref, .{ .low_level = .{
                .op = .box_box,
                .rc_effect = base.LowLevel.box_box.rcEffect(),
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = ty,
            } });
        }

        fn lowerPrivateNominalCapture(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            ty: Type.TypeId,
            checked_ty: checked_artifact.CheckedTypeId,
            nominal: anytype,
        ) Allocator.Error!Ast.ExprId {
            const checked_types = checkedTypesForKey(self.input, artifact) orelse unreachable;
            const payload = checkedTypePayload(checked_types, checked_ty);
            return switch (payload) {
                .nominal => |nominal_ty| blk: {
                    const backing = try self.lowerPrivateCaptureNode(artifact, nominal.backing, nominal_ty.backing);
                    const type_info = try self.artifactCheckedTypeInfo(artifact, checked_ty);
                    break :blk try self.program.ast.addExprWithSourcePayload(
                        ty,
                        type_info.source_ty,
                        type_info.source_ref,
                        .{ .nominal_reinterpret = backing },
                    );
                },
                .alias => |alias| try self.lowerPrivateCaptureNode(artifact, nominal.backing, alias.backing),
                else => invariantViolation("private capture nominal node had non-nominal checked type"),
            };
        }

        fn artifactCheckedTypeInfo(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!ConcreteTypeInfo {
            const key = ArtifactCheckedTypeKey{ .artifact = artifact, .checked_ty = checked_ty };
            if (self.graph.artifact_type_infos.get(key)) |info| return info;
            if (mode == .body_emitter) {
                invariantViolation("mono body emission reached artifact checked type before graph finalization published it");
            } else {
                const checked_types = checkedTypesForKey(self.input, artifact) orelse {
                    debug.invariant(false, "mono graph finalization invariant violated: artifact checked type owner was not available");
                    unreachable;
                };
                const source_ref = try self.program.concrete_source_types.registerArtifactRoot(artifact, checked_types, checked_ty);
                const info = ConcreteTypeInfo{
                    .ty = try self.graphInstantiator().lowerArtifactRef(.{
                        .artifact = artifact,
                        .ty = checked_ty,
                    }),
                    .source_ty = checkedTypeKey(checked_types, checked_ty),
                    .source_ref = source_ref,
                };
                try self.graph.artifact_type_infos.put(key, info);
                return info;
            }
        }

        fn concreteSourceTypeForCheckedKey(
            self: *Self,
            artifact: checked_artifact.CheckedModuleArtifactKey,
            source_ty: canonical.CanonicalTypeKey,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const checked_types = checkedTypesForKey(self.input, artifact) orelse {
                debug.invariant(false, "mono body lowering invariant violated: callable leaf artifact was not available");
                unreachable;
            };
            const checked_ty = checkedTypeRootForKey(checked_types, source_ty) orelse {
                debug.invariant(false, "mono body lowering invariant violated: callable leaf source type key has no checked payload");
                unreachable;
            };
            return try self.program.concrete_source_types.registerArtifactRoot(artifact, checked_types, checked_ty);
        }

        fn reserveCallableProcedure(
            self: *Self,
            callable: canonical.ProcedureCallableRef,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
            reason: MonoSpecializationReason,
        ) Allocator.Error!canonical.MirProcedureRef {
            const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
            if (!std.mem.eql(u8, &requested_key.bytes, &callable.source_fn_ty.bytes)) {
                invariantViolation("callable procedure reservation source function type disagrees with requested mono type");
            }
            const remapped_callable = try self.name_resolver.procedureCallableRef(callable);
            var concrete_callable = remapped_callable;
            concrete_callable.source_fn_ty = requested_key;
            return switch (concrete_callable.template) {
                .checked,
                .synthetic,
                => blk: {
                    const template = checkedTemplateFromCallableTemplate(concrete_callable.template);
                    const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                        .template = template,
                        .callable_template = concrete_callable.template,
                        .requested_fn_ty = requested_fn_ty,
                        .reason = reason,
                        .imported_closure = self.importedClosureForTemplate(template),
                    });
                    break :blk .{
                        .proc = reserved.proc.proc,
                        .callable = concrete_callable,
                    };
                },
                .lifted => invariantViolation("mono body lowering reached lifted callable reservation without an explicit owner source type payload"),
            };
        }

        fn importedClosureForTemplate(
            self: *const Self,
            template: canonical.ProcedureTemplateRef,
        ) ?checked_artifact.ImportedTemplateClosureView {
            if (self.template_lookup.imported_closure) |closure| {
                if (importedClosureContainsProcedureTemplate(closure, template)) return closure;
            }
            return null;
        }

        fn lowerEntryWrapperDef(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
            wrapper_id: canonical.EntryWrapperId,
        ) Allocator.Error!Ast.DefId {
            const entry_wrappers = self.template_lookup.entry_wrappers orelse {
                debug.invariant(false, "mono body lowering invariant violated: entry wrapper template came from a view without entry wrappers");
                unreachable;
            };
            const wrapper = entry_wrappers.get(wrapper_id);
            const root_body = self.graph.body_instances.get(.root) orelse {
                invariantViolation("mono body emission entry wrapper had no finalized root body");
            };
            const ret_ty = root_body.ret_ty;
            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            self.current_return_type = ret_ty;
            self.current_return_source_ref = ret_ty.source_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;
            const body = try self.lowerExprConcreteExpected(wrapper.body_expr, ret_ty);
            const bind = Ast.TypedSymbol{
                .ty = fn_ty,
                .source_ty = root_body.source_fn_ty,
                .source_ty_payload = root_body.source_fn_ty_payload,
                .symbol = try self.program.addProcSymbol(reserved.local_handle),
            };
            return try self.program.ast.addDef(.{
                .proc = mirProcedureRefFromReserved(reserved),
                .debug_name = null,
                .value = .{ .fn_ = .{
                    .source_fn_ty = root_body.source_fn_ty,
                    .source_fn_ty_payload = root_body.source_fn_ty_payload,
                    .recursive = false,
                    .bind = bind,
                    .args = Ast.Span(Ast.TypedSymbol).empty(),
                    .body = body,
                } },
            });
        }

        fn lowerCheckedBody(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
            body_id: checked_artifact.CheckedBodyId,
        ) Allocator.Error!Ast.DefId {
            const body = self.checkedBody(body_id);
            const root = self.checkedExpr(body.root_expr);
            return switch (root.data) {
                .lambda => |lambda| try self.lowerLambdaDef(reserved, fn_ty, lambda.args, lambda.body),
                .closure => |closure| blk: {
                    const lambda_expr = self.checkedExpr(closure.lambda);
                    switch (lambda_expr.data) {
                        .lambda => |lambda| break :blk try self.lowerLambdaDef(reserved, fn_ty, lambda.args, lambda.body),
                        else => invariantViolation("mono body lowering expected checked closure to reference a lambda body"),
                    }
                },
                .hosted_lambda => |hosted| try self.lowerHostedDef(reserved, hosted.symbol_name, hosted.args),
                .anno_only => invariantViolation("mono body lowering reached annotation-only procedure body without checked backing expression"),
                else => invariantViolation("mono body lowering expected a checked procedure body to be a lambda-like expression"),
            };
        }

        fn lowerHostedDef(
            self: *Self,
            reserved: ReservedMonoProc,
            symbol_name: canonical.ExternalSymbolNameId,
            arg_patterns: []const checked_artifact.CheckedPatternId,
        ) Allocator.Error!Ast.DefId {
            const root_body = self.graph.body_instances.get(.root) orelse {
                invariantViolation("mono body emission hosted definition had no finalized root body");
            };
            const args = try self.lowerParamSpanFromFunction(arg_patterns, root_body.source_fn_ty_payload);
            const ret_info = root_body.ret_ty;
            const hosted = try self.hostedProcForReserved(reserved.proc.proc, symbol_name);
            return try self.program.ast.addDef(.{
                .proc = mirProcedureRefFromReserved(reserved),
                .debug_name = null,
                .value = .{ .hosted_fn = .{
                    .proc = reserved.proc.proc,
                    .args = args,
                    .ret_ty = ret_info.ty,
                    .hosted = hosted,
                } },
            });
        }

        fn hostedProcForReserved(
            self: *Self,
            proc: canonical.ProcedureValueRef,
            symbol_name: canonical.ExternalSymbolNameId,
        ) Allocator.Error!Hosted.Proc {
            for (self.template_lookup.hosted_procs.procs) |hosted| {
                const lowering_proc = try self.name_resolver.procedureValueRef(hosted.proc);
                if (!canonical.procedureValueRefEql(lowering_proc, proc)) continue;
                if (hosted.external_symbol_name != symbol_name) {
                    invariantViolation("mono body lowering found hosted procedure metadata with a mismatched external symbol name");
                }
                return .{
                    .external_symbol_name = try self.name_resolver.externalSymbolName(
                        self.template_lookup.artifact,
                        hosted.external_symbol_name,
                    ),
                    .dispatch_index = self.hostedGlobalDispatchIndex(self.template_lookup.artifact, hosted),
                };
            }

            invariantViolation("mono body lowering expected hosted procedure metadata published in the checked artifact");
        }

        fn hostedGlobalDispatchIndex(
            self: *Self,
            target_artifact: checked_artifact.CheckedModuleArtifactKey,
            target: checked_artifact.HostedProc,
        ) u32 {
            var index: u32 = 0;
            var found = false;

            self.countHostedDispatchEntriesBefore(target_artifact, target, self.input.root.artifact.key, &self.input.root.artifact.hosted_procs, &index, &found);
            for (self.input.imports) |view| {
                self.countHostedDispatchEntriesBefore(target_artifact, target, view.key, view.hosted_procs, &index, &found);
            }
            for (self.input.root.relation_artifacts) |view| {
                self.countHostedDispatchEntriesBefore(target_artifact, target, view.key, view.hosted_procs, &index, &found);
            }

            if (!found) {
                invariantViolation("mono body lowering could not find hosted procedure in the global hosted dispatch catalog");
            }
            return index;
        }

        fn countHostedDispatchEntriesBefore(
            _: *Self,
            target_artifact: checked_artifact.CheckedModuleArtifactKey,
            target: checked_artifact.HostedProc,
            candidate_artifact: checked_artifact.CheckedModuleArtifactKey,
            candidates: *const checked_artifact.HostedProcTable,
            index: *u32,
            found: *bool,
        ) void {
            for (candidates.procs) |candidate| {
                if (std.mem.eql(u8, &candidate_artifact.bytes, &target_artifact.bytes) and
                    candidate.def_idx == target.def_idx)
                {
                    found.* = true;
                    continue;
                }
                if (hostedDispatchOrderLess(candidate, target)) {
                    index.* += 1;
                }
            }
        }

        fn hostedDispatchOrderLess(
            candidate: checked_artifact.HostedProc,
            target: checked_artifact.HostedProc,
        ) bool {
            return switch (std.mem.order(u8, candidate.order_key, target.order_key)) {
                .lt => true,
                .gt => false,
                .eq => @intFromEnum(candidate.def_idx) < @intFromEnum(target.def_idx),
            };
        }

        fn lowerLambdaDef(
            self: *Self,
            reserved: ReservedMonoProc,
            fn_ty: Type.TypeId,
            arg_patterns: []const checked_artifact.CheckedPatternId,
            body_expr: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.DefId {
            const root_body = self.graph.body_instances.get(.root) orelse {
                invariantViolation("mono body emission lambda definition had no finalized root body");
            };
            const params = try self.lowerParamBundleFromFunction(arg_patterns, root_body.source_fn_ty_payload);
            defer self.deinitParamBundle(params);
            const ret_ty = root_body.ret_ty;
            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            self.current_return_type = ret_ty;
            self.current_return_source_ref = ret_ty.source_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;
            const body = try self.lowerBodyWithParamSetup(body_expr, ret_ty, params);
            const bind = Ast.TypedSymbol{
                .ty = fn_ty,
                .source_ty = root_body.source_fn_ty,
                .source_ty_payload = root_body.source_fn_ty_payload,
                .symbol = try self.program.addProcSymbol(reserved.local_handle),
            };
            return try self.program.ast.addDef(.{
                .proc = mirProcedureRefFromReserved(reserved),
                .debug_name = null,
                .value = .{ .fn_ = .{
                    .source_fn_ty = root_body.source_fn_ty,
                    .source_fn_ty_payload = root_body.source_fn_ty_payload,
                    .recursive = false,
                    .bind = bind,
                    .args = params.args,
                    .body = body,
                } },
            });
        }

        fn lowerParamSpanFromFunction(
            self: *Self,
            patterns: []const checked_artifact.CheckedPatternId,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!Ast.Span(Ast.TypedSymbol) {
            const bundle = try self.lowerParamBundleFromFunction(patterns, source_fn);
            defer self.deinitParamBundle(bundle);
            if (bundle.destructures.len != 0) {
                invariantViolation("mono body lowering reached destructuring procedure parameters without a body to destructure");
            }
            if (bundle.mutable_inits.len != 0) {
                invariantViolation("mono body lowering reached mutable procedure parameters without a body to initialize");
            }
            return bundle.args;
        }

        fn lowerParamBundleFromFunction(
            self: *Self,
            patterns: []const checked_artifact.CheckedPatternId,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!LoweredParamBundle {
            const body_instance = self.graph.body_instances.get(self.current_body) orelse {
                invariantViolation("mono body emission reached procedure parameters without a finalized body instance");
            };
            if (body_instance.source_fn_ty_payload != source_fn) {
                invariantViolation("mono body emission procedure parameter source function disagreed with finalized body instance");
            }
            const param_types = body_instance.params;
            if (patterns.len != param_types.len) {
                invariantViolation("mono body lowering procedure parameter count disagreed with requested function type");
            }
            if (patterns.len == 0) return .{
                .args = Ast.Span(Ast.TypedSymbol).empty(),
                .destructures = &.{},
                .mutable_inits = &.{},
            };
            const args = try self.allocator.alloc(Ast.TypedSymbol, patterns.len);
            defer self.allocator.free(args);
            var destructures = std.ArrayList(ParamDestructure).empty;
            errdefer destructures.deinit(self.allocator);
            var mutable_inits = std.ArrayList(MutableParamInit).empty;
            errdefer mutable_inits.deinit(self.allocator);
            for (patterns, param_types, 0..) |pattern, param_ty, i| {
                args[i] = try self.lowerEntryParamPatternWithType(pattern, param_ty, &destructures, &mutable_inits);
            }
            return .{
                .args = try self.program.ast.addTypedSymbolSpan(args),
                .destructures = if (destructures.items.len == 0) &.{} else try destructures.toOwnedSlice(self.allocator),
                .mutable_inits = if (mutable_inits.items.len == 0) &.{} else try mutable_inits.toOwnedSlice(self.allocator),
            };
        }

        fn deinitParamBundle(self: *Self, bundle: LoweredParamBundle) void {
            if (bundle.destructures.len != 0) self.allocator.free(bundle.destructures);
            if (bundle.mutable_inits.len != 0) self.allocator.free(bundle.mutable_inits);
        }

        fn lowerEntryParamPatternWithType(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            param_ty: ConcreteTypeInfo,
            destructures: *std.ArrayList(ParamDestructure),
            mutable_inits: *std.ArrayList(MutableParamInit),
        ) Allocator.Error!Ast.TypedSymbol {
            const pattern = self.checkedPattern(pattern_id);
            const symbol = switch (pattern.data) {
                .assign => |binder| blk: {
                    try self.recordConcreteTypeForBinder(binder, param_ty);
                    const binder_symbol = try self.symbolForBinder(binder);
                    if (!self.patternBinderIsReassignable(binder)) {
                        break :blk binder_symbol;
                    }

                    const param_symbol = try self.program.addSyntheticSymbol();
                    try mutable_inits.append(self.allocator, .{
                        .param_symbol = param_symbol,
                        .bind = .{
                            .ty = param_ty.ty,
                            .source_ty = param_ty.source_ty,
                            .source_ty_payload = param_ty.source_ref,
                            .symbol = binder_symbol,
                        },
                    });
                    break :blk param_symbol;
                },
                .underscore => try self.program.addSyntheticSymbol(),
                else => blk: {
                    const synthetic = try self.program.addSyntheticSymbol();
                    try destructures.append(self.allocator, .{
                        .symbol = synthetic,
                        .pattern = pattern_id,
                        .param_ty = param_ty,
                    });
                    break :blk synthetic;
                },
            };
            return .{
                .ty = param_ty.ty,
                .source_ty = param_ty.source_ty,
                .source_ty_payload = param_ty.source_ref,
                .symbol = symbol,
            };
        }

        fn lowerBodyWithParamSetup(
            self: *Self,
            body_expr: checked_artifact.CheckedExprId,
            ret_ty: ConcreteTypeInfo,
            params: LoweredParamBundle,
        ) Allocator.Error!Ast.ExprId {
            const body = try self.lowerBodyWithParamDestructures(body_expr, ret_ty, params.destructures);
            if (params.mutable_inits.len == 0) return body;

            const stmts = try self.allocator.alloc(Ast.StmtId, params.mutable_inits.len);
            defer self.allocator.free(stmts);

            for (params.mutable_inits, 0..) |param_init, i| {
                const param_expr = try self.program.ast.addExprWithSourcePayload(
                    param_init.bind.ty,
                    param_init.bind.source_ty,
                    param_init.bind.source_ty_payload,
                    .{ .var_ = param_init.param_symbol },
                );
                stmts[i] = try self.program.ast.addStmt(.{ .var_decl = .{
                    .bind = param_init.bind,
                    .body = param_expr,
                } });
            }

            return try self.program.ast.addExprWithSourcePayload(ret_ty.ty, ret_ty.source_ty, ret_ty.source_ref, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(stmts),
                .final_expr = body,
            } });
        }

        fn lowerBodyWithParamDestructures(
            self: *Self,
            body_expr: checked_artifact.CheckedExprId,
            ret_ty: ConcreteTypeInfo,
            destructures: []const ParamDestructure,
        ) Allocator.Error!Ast.ExprId {
            return try self.lowerBodyWithParamDestructuresFromIndex(body_expr, ret_ty, destructures, 0);
        }

        fn lowerBodyWithParamDestructuresFromIndex(
            self: *Self,
            body_expr: checked_artifact.CheckedExprId,
            ret_ty: ConcreteTypeInfo,
            destructures: []const ParamDestructure,
            index: usize,
        ) Allocator.Error!Ast.ExprId {
            if (index >= destructures.len) return try self.lowerExprConcreteExpected(body_expr, ret_ty);

            const destructure = destructures[index];
            const cond = try self.program.ast.addExprWithSourcePayload(destructure.param_ty.ty, destructure.param_ty.source_ty, destructure.param_ty.source_ref, .{
                .var_ = destructure.symbol,
            });
            const pat = try self.lowerPatternWithRemaps(destructure.param_ty, destructure.pattern, &.{});
            const body = try self.lowerBodyWithParamDestructuresFromIndex(body_expr, ret_ty, destructures, index + 1);
            const branch = Ast.Branch{
                .pat = pat,
                .guard = null,
                .body = body,
                .degenerate = false,
            };
            return try self.program.ast.addExprWithSourcePayload(ret_ty.ty, ret_ty.source_ty, ret_ty.source_ref, .{ .match_ = .{
                .cond = cond,
                .branches = try self.program.ast.addBranchSpan(&.{branch}),
                .is_try_suffix = false,
            } });
        }

        fn paramTypesFromConcreteFunction(
            self: *Self,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error![]ConcreteTypeInfo {
            const refs = try self.sourceParamRefsFromFunction(source_fn);
            defer if (refs.len != 0) self.allocator.free(refs);
            return try self.concreteTypeInfosForRefs(refs);
        }

        fn sourceParamRefsFromFunction(
            self: *Self,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error![]const ConcreteSourceType.ConcreteSourceTypeRef {
            var current = source_fn;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| {
                        current = try self.concreteAliasBackingChildRef(current, alias);
                    },
                    .function => |function| {
                        const out = try self.allocator.alloc(ConcreteSourceType.ConcreteSourceTypeRef, function.args.len);
                        errdefer self.allocator.free(out);
                        for (function.args, 0..) |arg, i| {
                            out[i] = try self.concreteSourceChildRef(current, .{ .tag = .function_arg, .a = @intCast(i) }, arg);
                        }
                        return out;
                    },
                    else => invariantViolation("mono body lowering expected requested procedure type to be a function"),
                }
            }
        }

        fn returnTypeFromConcreteFunction(
            self: *Self,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteTypeInfo {
            const fn_ref = try self.concreteFunctionRef(source_fn) orelse
                invariantViolation("mono body lowering expected requested procedure type to be a function");
            return try self.concreteFunctionReturnType(fn_ref);
        }

        fn concreteFunctionRef(
            self: *Self,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!?ConcreteSourceType.ConcreteSourceTypeRef {
            var current = source_fn;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                    .function => return current,
                    else => return null,
                }
            }
        }

        fn concreteFunctionReturnType(
            self: *Self,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteTypeInfo {
            return try self.runtimeConcreteTypeInfo(try self.sourceReturnRefFromFunction(source_fn));
        }

        fn sourceReturnRefFromFunction(
            self: *Self,
            source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            var current = source_fn;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                    .function => |function| return try self.concreteSourceChildRef(current, .{ .tag = .function_return }, function.ret),
                    else => invariantViolation("mono body lowering expected concrete function source type"),
                }
            }
        }

        fn concreteTypeInfoForChecked(
            self: *Self,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!ConcreteTypeInfo {
            if (mode == .body_emitter) {
                invariantViolation("mono body emission attempted to instantiate a checked type");
            } else {
                const source_ref = try self.graphInstantiator().concreteRefForTemplateType(checked_ty);
                return try self.runtimeConcreteTypeInfo(source_ref);
            }
        }

        fn concreteSourceRefForCheckedPreservingVariables(
            self: *Self,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            if (mode == .body_emitter) {
                invariantViolation("mono body emission attempted to clone a checked source type");
            } else {
                return try self.graphInstantiator().concreteRefForTemplateTypePreservingVariables(checked_ty);
            }
        }

        fn sourceRefForDemandedExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            if (self.finalizedDemandSourceRefForExpr(expr_id)) |source_ref| return source_ref;
            return try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(expr_id).ty);
        }

        fn finalizedDemandSourceRefForExpr(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) ?ConcreteSourceType.ConcreteSourceTypeRef {
            if (self.graph.expr_type_demands.get(self.scopedExpr(expr_id))) |source_ref| return source_ref;
            if (self.demandedLocalSourceRefForExpr(expr_id)) |source_ref| return source_ref;
            if (self.graph.expr_types.get(self.scopedExpr(expr_id))) |info| return info.source_ref;
            return null;
        }

        fn demandedLocalSourceRefForExpr(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) ?ConcreteSourceType.ConcreteSourceTypeRef {
            const expr = self.checkedExpr(expr_id);
            const ref_id = switch (expr.data) {
                .lookup_local => |lookup| lookup.resolved orelse return null,
                else => return null,
            };
            const record = self.resolvedValueRef(ref_id);
            const binder = switch (record.ref) {
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                => |local| local.binder,
                else => return null,
            };
            if (self.graph.local_type_demands.get(self.scopedBinder(binder))) |source_ref| return source_ref;
            if (self.graph.local_symbol_types.get(self.scopedBinder(binder))) |existing| return existing.source_ref;
            return null;
        }

        fn runtimeConcreteTypeInfo(
            self: *Self,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteTypeInfo {
            if (self.graph.concrete_type_infos.get(source_ref)) |info| return info;
            if (mode == .body_emitter) {
                invariantViolation("mono body emission reached concrete source type before graph finalization published it");
            } else {
                const runtime_ref = try self.graphInstantiator().runtimeConcreteRef(source_ref);
                if (self.graph.concrete_type_infos.get(runtime_ref)) |info| {
                    try self.graph.concrete_type_infos.put(source_ref, info);
                    return info;
                }
                const info = ConcreteTypeInfo{
                    .ty = try self.graphInstantiator().lowerConcreteRef(runtime_ref),
                    .source_ty = self.program.concrete_source_types.key(runtime_ref),
                    .source_ref = runtime_ref,
                };
                try self.graph.concrete_type_infos.put(runtime_ref, info);
                try self.graph.concrete_type_infos.put(source_ref, info);
                return info;
            }
        }

        fn publishRuntimeConcreteTypeInfo(
            self: *Self,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteTypeInfo {
            if (mode == .body_emitter) return try self.runtimeConcreteTypeInfo(source_ref);

            const runtime_ref = try self.graphInstantiator().runtimeConcreteRef(source_ref);
            const info = if (self.graph.concrete_type_infos.get(runtime_ref)) |existing| existing else blk: {
                const published = ConcreteTypeInfo{
                    .ty = try self.graphInstantiator().lowerConcreteRef(runtime_ref),
                    .source_ty = self.program.concrete_source_types.key(runtime_ref),
                    .source_ref = runtime_ref,
                };
                try self.graph.concrete_type_infos.put(runtime_ref, published);
                break :blk published;
            };
            try self.graph.concrete_type_infos.put(source_ref, info);
            return info;
        }

        fn finalizedExprTypeInfo(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) ConcreteTypeInfo {
            return self.graph.expr_types.get(self.scopedExpr(expr_id)) orelse {
                invariantViolation("mono body emission reached expression before graph finalization published its concrete type");
            };
        }

        fn finalizedPatternTypeInfo(
            self: *const Self,
            pattern_id: checked_artifact.CheckedPatternId,
        ) ConcreteTypeInfo {
            return self.graph.pattern_types.get(self.scopedPattern(pattern_id)) orelse {
                invariantViolation("mono body emission reached pattern before graph finalization published its concrete type");
            };
        }

        fn concreteResultTypeForExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!ConcreteTypeInfo {
            if (self.graph.expr_types.get(self.scopedExpr(expr_id))) |info| return info;
            if (self.concreteTypeForLookupExpr(expr_id)) |lookup_ty| return lookup_ty;
            const expr = self.checkedExpr(expr_id);
            return switch (expr.data) {
                .call => self.finalizedCall(expr_id).ret_ty,
                .dispatch_call => |plan| blk: {
                    break :blk self.finalizedStaticDispatchResultType(
                        plan orelse invariantViolation("checked dispatch call reached mono without a StaticDispatchCallPlan"),
                    );
                },
                .method_eq => |plan| blk: {
                    break :blk self.finalizedStaticDispatchResultType(
                        plan orelse invariantViolation("checked method equality reached mono without a StaticDispatchCallPlan"),
                    );
                },
                .type_dispatch_call => |plan| blk: {
                    break :blk self.finalizedStaticDispatchResultType(
                        plan orelse invariantViolation("checked type dispatch call reached mono without a StaticDispatchCallPlan"),
                    );
                },
                .match_ => |match| if (match.is_try_suffix)
                    try self.trySuffixResultType(match.cond)
                else
                    invariantViolation("mono body emission reached match expression without finalized concrete result type"),
                else => invariantViolation("mono body emission reached expression without finalized concrete result type"),
            };
        }

        fn trySuffixResultType(
            self: *Self,
            cond: checked_artifact.CheckedExprId,
        ) Allocator.Error!ConcreteTypeInfo {
            const cond_info = try self.concreteResultTypeForExpr(cond);
            const ok_label = try self.program.canonical_names.internTagLabel("Ok");
            const payload_infos = try self.concreteTagPayloadInfosForUnionType(cond_info.source_ref, ok_label);
            defer if (payload_infos.len != 0) self.allocator.free(payload_infos);
            if (payload_infos.len != 1) {
                invariantViolation("mono body lowering expected try suffix Ok branch to have exactly one payload");
            }
            return payload_infos[0];
        }

        fn lowerParamPattern(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
        ) Allocator.Error!Ast.TypedSymbol {
            const pattern = self.checkedPattern(pattern_id);
            const concrete_ty = self.concreteTypeForPatternBinder(pattern_id) orelse self.graph.pattern_types.get(self.scopedPattern(pattern_id)) orelse {
                invariantViolation("mono body emission reached parameter pattern before graph finalization published its concrete type");
            };
            const symbol = if (self.binderForSimplePatternMaybe(pattern.data)) |binder| blk: {
                try self.recordConcreteTypeForBinder(binder, concrete_ty);
                break :blk try self.symbolForBinder(binder);
            } else try self.program.addSyntheticSymbol();
            return .{
                .ty = concrete_ty.ty,
                .source_ty = concrete_ty.source_ty,
                .source_ty_payload = concrete_ty.source_ref,
                .symbol = symbol,
            };
        }

        fn lowerParamPatternWithType(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            param_ty: ConcreteTypeInfo,
        ) Allocator.Error!Ast.TypedSymbol {
            const pattern = self.checkedPattern(pattern_id);
            const symbol = if (self.binderForSimplePatternMaybe(pattern.data)) |binder| blk: {
                try self.recordConcreteTypeForBinder(binder, param_ty);
                break :blk try self.symbolForBinder(binder);
            } else try self.program.addSyntheticSymbol();
            return .{
                .ty = param_ty.ty,
                .source_ty = param_ty.source_ty,
                .source_ty_payload = param_ty.source_ref,
                .symbol = symbol,
            };
        }

        fn binderForSimplePattern(
            self: *Self,
            data: checked_artifact.CheckedPatternData,
        ) checked_artifact.PatternBinderId {
            return self.binderForSimplePatternMaybe(data) orelse
                invariantViolation("mono body lowering requires destructuring parameters to be lowered into explicit local bindings before procedure entry");
        }

        fn binderForSimplePatternMaybe(
            _: *Self,
            data: checked_artifact.CheckedPatternData,
        ) ?checked_artifact.PatternBinderId {
            return switch (data) {
                .assign => |binder| binder,
                .as => |as| as.binder,
                .underscore => null,
                else => null,
            };
        }

        fn symbolForBinder(
            self: *Self,
            binder: checked_artifact.PatternBinderId,
        ) Allocator.Error!Ast.Symbol {
            if (self.local_symbols.get(binder)) |symbol| return symbol;
            const symbol = try self.program.addPatternBinderSymbol(binder);
            try self.local_symbols.put(binder, symbol);
            return symbol;
        }

        fn scopedExpr(
            self: *const Self,
            expr: checked_artifact.CheckedExprId,
        ) ScopedExpr {
            return .{ .body = self.current_body, .expr = expr };
        }

        fn scopedPattern(
            self: *const Self,
            pattern: checked_artifact.CheckedPatternId,
        ) ScopedPattern {
            return .{ .body = self.current_body, .pattern = pattern };
        }

        fn scopedBinder(
            self: *const Self,
            binder: checked_artifact.PatternBinderId,
        ) ScopedBinder {
            return .{ .body = self.current_body, .binder = binder };
        }

        fn concreteSourceChildRef(
            self: *Self,
            parent: ConcreteSourceType.ConcreteSourceTypeRef,
            kind: ConcreteSourceChildKind,
            checked_child: checked_artifact.CheckedTypeId,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const key = ConcreteSourceChildKey{ .parent = parent, .kind = kind };
            if (self.graph.concrete_source_children.get(key)) |child| return child;
            if (mode == .body_emitter) {
                invariantViolation("mono body emission reached concrete source child before graph finalization published it");
            } else {
                const child = try self.graphInstantiator().concreteChildRef(parent, checked_child);
                try self.graph.concrete_source_children.put(key, child);
                return child;
            }
        }

        fn concreteAliasBackingChildRef(
            self: *Self,
            parent: ConcreteSourceType.ConcreteSourceTypeRef,
            alias: checked_artifact.CheckedAliasType,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            return try self.concreteSourceChildRef(parent, .{ .tag = .alias_backing }, alias.backing);
        }

        fn concreteNominalBackingChildRef(
            self: *Self,
            parent: ConcreteSourceType.ConcreteSourceTypeRef,
            nominal: checked_artifact.CheckedNominalType,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const key = ConcreteSourceChildKey{ .parent = parent, .kind = .{ .tag = .nominal_backing } };
            if (self.graph.concrete_source_children.get(key)) |child| return child;
            if (mode == .body_emitter) {
                invariantViolation("mono body emission reached nominal backing before graph finalization published it");
            } else {
                const child = try self.graphInstantiator().concreteNominalBackingRef(parent, nominal);
                try self.graph.concrete_source_children.put(key, child);
                return child;
            }
        }

        fn recordConcreteTypeForBinder(
            self: *Self,
            binder: checked_artifact.PatternBinderId,
            ty: ConcreteTypeInfo,
        ) Allocator.Error!void {
            const scoped = self.scopedBinder(binder);
            if (mode == .body_emitter) {
                const existing = self.graph.local_symbol_types.get(scoped) orelse {
                    invariantViolation("mono body emission reached binder before graph finalization published its concrete type");
                };
                if (!self.program.types.equalIds(existing.ty, ty.ty) or existing.source_ref != ty.source_ref) {
                    invariantViolation("mono body emission binder type disagreed with finalized graph");
                }
                return;
            }
            try self.graph.local_symbol_types.put(scoped, ty);
        }

        fn recordConcreteDemandForBinder(
            self: *Self,
            binder: checked_artifact.PatternBinderId,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            if (self.local_proc_decls.contains(binder)) return;
            const scoped_binder = self.scopedBinder(binder);
            if (self.graph.local_type_demands.getPtr(scoped_binder)) |existing| {
                try self.graphInstantiator().unifyConcreteRefs(existing.*, source_ref);
                return;
            }
            if (self.graph.local_symbol_types.get(scoped_binder)) |existing| {
                try self.graphInstantiator().unifyConcreteRefs(existing.source_ref, source_ref);
                try self.graph.local_symbol_types.put(scoped_binder, try self.runtimeConcreteTypeInfo(existing.source_ref));
                return;
            }
            try self.graph.local_type_demands.put(scoped_binder, source_ref);
        }

        fn publishLocalConcreteDemands(self: *Self) Allocator.Error!void {
            var demands = self.graph.local_type_demands.iterator();
            while (demands.next()) |entry| {
                const scoped_binder = entry.key_ptr.*;
                if (scoped_binder.body != self.current_body) continue;
                if (self.graph.local_symbol_types.contains(scoped_binder)) continue;
                try self.recordConcreteTypeForBinder(
                    scoped_binder.binder,
                    try self.publishRuntimeConcreteTypeInfo(entry.value_ptr.*),
                );
            }
        }

        fn publishExprConcreteDemands(self: *Self) Allocator.Error!void {
            var demands = self.graph.expr_type_demands.iterator();
            while (demands.next()) |entry| {
                const scoped_expr = entry.key_ptr.*;
                if (scoped_expr.body != self.current_body) continue;
                try self.graph.expr_types.put(scoped_expr, try self.publishRuntimeConcreteTypeInfo(entry.value_ptr.*));
            }
        }

        fn concreteTypeForLookupExpr(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) ?ConcreteTypeInfo {
            const expr = self.checkedExpr(expr_id);
            const ref_id = switch (expr.data) {
                .lookup_local => |lookup| lookup.resolved orelse return null,
                else => return null,
            };
            const record = self.resolvedValueRef(ref_id);
            const binder = switch (record.ref) {
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                => |local| local.binder,
                else => return null,
            };
            return self.graph.local_symbol_types.get(self.scopedBinder(binder));
        }

        fn concreteTypeForConstLookupExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?ConcreteTypeInfo {
            const expr = self.checkedExpr(expr_id);
            const ref_id = switch (expr.data) {
                .lookup_local => |lookup| lookup.resolved orelse return null,
                .lookup_external => |lookup| lookup orelse return null,
                .lookup_required => |lookup| lookup orelse return null,
                else => return null,
            };
            const record = self.resolvedValueRef(ref_id);
            const const_use = switch (record.ref) {
                .top_level_const,
                .imported_const,
                => |use| use,
                .platform_required_const => |required| required.const_use,
                else => return null,
            };
            const payload = const_use.requested_source_ty_payload orelse return null;
            return try self.concreteTypeInfoForChecked(payload);
        }

        fn concreteTypeForPatternBinder(
            self: *const Self,
            pattern_id: checked_artifact.CheckedPatternId,
        ) ?ConcreteTypeInfo {
            const pattern = self.checkedPattern(pattern_id);
            const binder = switch (pattern.data) {
                .assign => |binder| binder,
                .as => |as| as.binder,
                else => return null,
            };
            return self.graph.local_symbol_types.get(self.scopedBinder(binder));
        }

        fn recordConcreteTypeForLocalLookup(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            ty: ConcreteTypeInfo,
        ) Allocator.Error!void {
            const expr = self.checkedExpr(expr_id);
            const ref_id = switch (expr.data) {
                .lookup_local => |lookup| lookup.resolved orelse return,
                else => return,
            };
            const record = self.resolvedValueRef(ref_id);
            const binder = switch (record.ref) {
                .local_proc => return,
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                => |local| if (self.local_proc_decls.contains(local.binder)) return else local.binder,
                else => return,
            };
            if (mode == .body_emitter) {
                const existing = self.graph.local_symbol_types.get(self.scopedBinder(binder)) orelse {
                    invariantViolation("mono body emission reached local lookup before graph finalization published its concrete type");
                };
                if (!self.program.types.equalIds(existing.ty, ty.ty)) {
                    invariantViolation("mono body emission local lookup type disagreed with finalized graph");
                }
                const finalized_expr = self.graph.expr_types.get(self.scopedExpr(expr_id)) orelse {
                    invariantViolation("mono body emission reached local lookup before graph finalization published the lookup occurrence type");
                };
                if (!self.program.types.equalIds(finalized_expr.ty, ty.ty)) {
                    invariantViolation("mono body emission local lookup occurrence type disagreed with finalized graph");
                }
                return;
            }
            try self.recordConcreteTypeForBinder(binder, ty);
        }

        fn lowerExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            if (self.concreteTypeForLookupExpr(expr_id)) |lookup_ty| {
                return try self.lowerExprConcreteExpected(expr_id, lookup_ty);
            }
            return try self.lowerExprExpected(expr_id, self.checkedExpr(expr_id).ty);
        }

        fn lowerExprExpected(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            expected_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!Ast.ExprId {
            return try self.lowerExprWithExpected(expr_id, .{ .checked = expected_ty });
        }

        fn lowerExprConcreteExpected(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            expected_ty: ConcreteTypeInfo,
        ) Allocator.Error!Ast.ExprId {
            return try self.lowerExprWithExpected(expr_id, .{ .concrete = expected_ty });
        }

        fn lowerReturnValue(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const return_type = self.current_return_type orelse {
                invariantViolation("mono body lowering reached return without an enclosing procedure return type");
            };
            return try self.lowerExprConcreteExpected(expr_id, return_type);
        }

        fn expectedTypeInfo(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            expected_ty: ExprExpectedType,
        ) Allocator.Error!ConcreteTypeInfo {
            return switch (expected_ty) {
                .checked => self.finalizedExprTypeInfo(expr_id),
                .concrete => |concrete| concrete,
            };
        }

        fn lowerExprWithExpected(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            expected_ty: ExprExpectedType,
        ) Allocator.Error!Ast.ExprId {
            const expr = self.checkedExpr(expr_id);
            const expected_info = try self.expectedTypeInfo(expr_id, expected_ty);
            try self.recordConcreteTypeForLocalLookup(expr_id, expected_info);
            const ty = expected_info.ty;
            const source_ty = expected_info.source_ty;
            const lowered = switch (expr.data) {
                .num => |num| try self.lowerIntegerLiteralExpr(ty, num.value),
                .typed_int => |num| try self.lowerIntegerLiteralExpr(ty, num.value),
                .frac_f32 => |frac| try self.lowerF32LiteralExpr(ty, frac.value),
                .frac_f64 => |frac| try self.lowerF64LiteralExpr(ty, frac.value),
                .dec => |dec| try self.lowerScaledDecimalLiteralExpr(ty, dec.value.num),
                .dec_small => |dec| try self.lowerScaledDecimalLiteralExpr(ty, dec.value.toRocDec().num),
                .typed_frac => |frac| try self.lowerScaledDecimalLiteralExpr(ty, frac.value.toI128()),
                .str_segment => |literal| try self.program.ast.addExpr(ty, .{ .str_lit = try self.lowerCheckedStringLiteral(literal) }),
                .str => |segments| try self.lowerStringExpr(ty, segments),
                .bytes_literal => |literal| try self.lowerBytesLiteral(ty, literal),
                .lookup_local => |lookup| try self.lowerResolvedLookup(expected_info, lookup.resolved orelse invariantViolation("checked lookup_local reached mono without a resolved value ref")),
                .lookup_external => |ref_id| try self.lowerResolvedLookup(expected_info, ref_id orelse invariantViolation("checked lookup_external reached mono without a resolved value ref")),
                .lookup_required => |ref_id| try self.lowerResolvedLookup(expected_info, ref_id orelse invariantViolation("checked lookup_required reached mono without a resolved value ref")),
                .list => |items| try self.lowerList(ty, expected_info.source_ref, items),
                .empty_list => try self.program.ast.addExpr(ty, .{ .list = Ast.Span(Ast.ExprId).empty() }),
                .tuple => |items| try self.lowerTuple(ty, items),
                .block => |block| try self.lowerBlock(ty, block.statements, block.final_expr, expected_ty),
                .record => |record| try self.lowerRecord(ty, expected_info.source_ref, record),
                .empty_record => try self.program.ast.addExpr(ty, .{ .record = Ast.Span(Ast.FieldExpr).empty() }),
                .lambda => |lambda| try self.lowerClosureExpr(expr_id, lambda.args, lambda.body),
                .call => |call| try self.lowerCall(expected_info, expr_id, call),
                .structural_eq => |eq| try self.lowerStructuralEq(ty, eq),
                .unary_not => |child| blk: {
                    const value = try self.lowerBoolConditionExpr(child);
                    break :blk try self.program.ast.addExpr(ty, .{ .bool_not = value });
                },
                .if_ => |if_| try self.lowerIf(ty, if_.branches, if_.final_else, expected_ty),
                .match_ => |match_| try self.lowerMatch(ty, match_, expected_ty),
                .tag => |tag| try self.lowerTag(ty, expected_info.source_ref, try self.tagLabel(tag.name), tag.args),
                .zero_argument_tag => |tag| blk: {
                    break :blk try self.lowerTag(ty, expected_info.source_ref, try self.tagLabel(tag.name), &.{});
                },
                .closure => |closure| try self.lowerCheckedClosureExpr(expr_id, closure),
                .field_access => |access| try self.lowerFieldAccess(expected_info, access.receiver, try self.recordFieldLabel(access.field_name)),
                .tuple_access => |access| blk: {
                    const tuple = try self.lowerExpr(access.tuple);
                    break :blk try self.program.ast.addExpr(ty, .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                    } });
                },
                .return_ => |ret| blk: {
                    const child = try self.lowerReturnValue(ret.expr);
                    break :blk try self.program.ast.addExpr(ty, .{ .return_ = child });
                },
                .binop => |binop| try self.lowerBinop(expected_info, binop),
                .unary_minus => |child| try self.lowerUnaryMinus(expected_info, child),
                .for_ => |for_| try self.lowerForExpr(
                    ty,
                    for_.plan orelse invariantViolation("checked for expression reached mono without an iterator-for plan"),
                    for_.pattern,
                    for_.body,
                ),
                .run_low_level => |run_low_level| try self.lowerRunLowLevel(ty, run_low_level.op, run_low_level.args),
                .nominal => |nominal| blk: {
                    const backing_info = try self.concreteNominalBackingInfo(expected_info);
                    const backing = try self.lowerExprConcreteExpected(nominal.backing_expr, backing_info);
                    break :blk try self.program.ast.addExpr(ty, .{ .nominal_reinterpret = backing });
                },
                .dispatch_call => |plan| try self.lowerStaticDispatch(expected_info, plan orelse invariantViolation("checked dispatch call reached mono without a StaticDispatchCallPlan")),
                .method_eq => |plan| try self.lowerStaticDispatch(expected_info, plan orelse invariantViolation("checked method equality reached mono without a StaticDispatchCallPlan")),
                .type_dispatch_call => |plan| try self.lowerStaticDispatch(expected_info, plan orelse invariantViolation("checked type dispatch call reached mono without a StaticDispatchCallPlan")),
                .hosted_lambda => invariantViolation("mono body lowering reached hosted lambda as an expression; hosted lambdas must be published as procedure templates"),
                .dbg => |child| try self.lowerDbgExpression(ty, child),
                .expect => |child| blk: {
                    const condition = try self.lowerBoolConditionExpr(child);
                    const expect_stmt = try self.program.ast.addStmt(.{ .expect = condition });
                    const stmts = try self.program.ast.addStmtSpan(&.{expect_stmt});
                    const unit = try self.program.ast.addExpr(ty, .unit);
                    break :blk try self.program.ast.addExpr(ty, .{ .block = .{
                        .stmts = stmts,
                        .final_expr = unit,
                    } });
                },
                .runtime_error => try self.program.ast.addExpr(ty, .runtime_error),
                .crash => |literal| try self.program.ast.addExpr(ty, .{ .crash = try self.lowerCheckedStringLiteral(literal) }),
                .ellipsis, .anno_only, .pending => invariantViolation("mono body lowering received a non-runtime checked expression form"),
            };
            const lowered_expr = self.program.ast.getExpr(lowered);
            if (sourceTyIsEmpty(lowered_expr.source_ty) or lowered_expr.source_ty_payload == null) {
                self.program.ast.setExprSourceInfo(lowered, source_ty, expected_info.source_ref);
            }
            return lowered;
        }

        fn lowerBoolConditionExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const expr = self.checkedExpr(expr_id);
            const bool_info = try self.boolConcreteTypeInfo();
            const bool_ty = bool_info.ty;
            const lowered = switch (expr.data) {
                .tag => |tag| try self.lowerTag(bool_ty, bool_info.source_ref, try self.tagLabel(tag.name), tag.args),
                .zero_argument_tag => |tag| blk: {
                    break :blk try self.lowerTag(bool_ty, bool_info.source_ref, try self.tagLabel(tag.name), &.{});
                },
                .unary_not => |child| blk: {
                    const value = try self.lowerBoolConditionExpr(child);
                    break :blk try self.program.ast.addExpr(bool_ty, .{ .bool_not = value });
                },
                else => return try self.lowerExprConcreteExpected(expr_id, bool_info),
            };
            self.program.ast.setExprSourceInfo(lowered, bool_info.source_ty, bool_info.source_ref);
            return lowered;
        }

        fn boolConcreteTypeInfo(self: *Self) Allocator.Error!ConcreteTypeInfo {
            const source_ref = try self.program.boolSourceTypeRef();
            return try self.runtimeConcreteTypeInfo(source_ref);
        }

        fn lowerBoolLiteral(
            self: *Self,
            bool_info: ConcreteTypeInfo,
            literal: bool,
        ) Allocator.Error!Ast.ExprId {
            const label = try self.program.canonical_names.internTagLabel(if (literal) "True" else "False");
            return try self.lowerTag(bool_info.ty, bool_info.source_ref, label, &.{});
        }

        fn ensureUnitType(self: *Self) Allocator.Error!Type.TypeId {
            return try self.program.types.internResolved(.{ .record = .{ .fields = &.{} } });
        }

        fn ensureStrType(self: *Self) Allocator.Error!Type.TypeId {
            return try self.program.types.internResolved(.{ .primitive = .str });
        }

        fn lowerDbgExpression(
            self: *Self,
            unit_ty: Type.TypeId,
            child: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const child_info = try self.concreteResultTypeForExpr(child);
            const value_expr = try self.lowerExprConcreteExpected(child, child_info);
            const value_symbol = try self.program.addSyntheticSymbol();

            const value_decl = try self.program.ast.addStmt(.{ .decl = .{
                .bind = .{
                    .ty = child_info.ty,
                    .source_ty = child_info.source_ty,
                    .source_ty_payload = child_info.source_ref,
                    .symbol = value_symbol,
                },
                .body = value_expr,
            } });

            const value_ref = try self.program.ast.addExprWithSourcePayload(child_info.ty, child_info.source_ty, child_info.source_ref, .{ .var_ = value_symbol });
            const msg = try self.lowerStrInspectCall(try self.ensureStrType(), value_ref, child_info);
            const debug_stmt = try self.program.ast.addStmt(.{ .debug = msg });
            const unit = try self.program.ast.addExpr(unit_ty, .unit);
            const stmts = [_]Ast.StmtId{ value_decl, debug_stmt };
            return try self.program.ast.addExpr(unit_ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&stmts),
                .final_expr = unit,
            } });
        }

        fn lowerIntegerLiteralExpr(
            self: *Self,
            ty: Type.TypeId,
            value: CIR.IntValue,
        ) Allocator.Error!Ast.ExprId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
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
                    => try self.program.ast.addExpr(ty, .{ .int_lit = @as(i128, @bitCast(value.bytes)) }),
                    .f32 => try self.program.ast.addExpr(ty, .{ .frac_f32_lit = @floatCast(intValueToF64(value)) }),
                    .f64 => try self.program.ast.addExpr(ty, .{ .frac_f64_lit = intValueToF64(value) }),
                    .dec => try self.program.ast.addExpr(ty, .{ .dec_lit = intValueToScaledDec(value) }),
                    else => invariantViolation("mono body lowering reached integer literal with non-numeric primitive type"),
                },
                else => invariantViolation("mono body lowering reached integer literal with non-primitive result type"),
            };
        }

        fn lowerScaledDecimalLiteralExpr(
            self: *Self,
            ty: Type.TypeId,
            scaled_value: i128,
        ) Allocator.Error!Ast.ExprId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
                    .f32 => try self.program.ast.addExpr(ty, .{ .frac_f32_lit = @floatCast(scaledDecToF64(scaled_value)) }),
                    .f64 => try self.program.ast.addExpr(ty, .{ .frac_f64_lit = scaledDecToF64(scaled_value) }),
                    .dec => try self.program.ast.addExpr(ty, .{ .dec_lit = scaled_value }),
                    else => invariantViolation("mono body lowering reached decimal literal with non-fractional primitive type"),
                },
                else => invariantViolation("mono body lowering reached decimal literal with non-primitive result type"),
            };
        }

        fn lowerF32LiteralExpr(
            self: *Self,
            ty: Type.TypeId,
            value: f32,
        ) Allocator.Error!Ast.ExprId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
                    .f32 => try self.program.ast.addExpr(ty, .{ .frac_f32_lit = value }),
                    .f64 => try self.program.ast.addExpr(ty, .{ .frac_f64_lit = @floatCast(value) }),
                    .dec => invariantViolation("mono body lowering reached binary fraction literal with Dec result type after type checking"),
                    else => invariantViolation("mono body lowering reached binary fraction literal with non-fractional primitive type"),
                },
                else => invariantViolation("mono body lowering reached binary fraction literal with non-primitive result type"),
            };
        }

        fn lowerF64LiteralExpr(
            self: *Self,
            ty: Type.TypeId,
            value: f64,
        ) Allocator.Error!Ast.ExprId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
                    .f32 => try self.program.ast.addExpr(ty, .{ .frac_f32_lit = @floatCast(value) }),
                    .f64 => try self.program.ast.addExpr(ty, .{ .frac_f64_lit = value }),
                    .dec => invariantViolation("mono body lowering reached binary fraction literal with Dec result type after type checking"),
                    else => invariantViolation("mono body lowering reached binary fraction literal with non-fractional primitive type"),
                },
                else => invariantViolation("mono body lowering reached binary fraction literal with non-primitive result type"),
            };
        }

        fn lowerStringExpr(
            self: *Self,
            ty: Type.TypeId,
            segments: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            if (segments.len == 0) invariantViolation("mono body lowering received string expression with no segments");

            var current = try self.lowerExpr(segments[0]);
            for (segments[1..]) |segment| {
                const rhs = try self.lowerExpr(segment);
                const args = [_]Ast.ExprId{ current, rhs };
                current = try self.program.ast.addExpr(ty, .{ .low_level = .{
                    .op = .str_concat,
                    .rc_effect = base.LowLevel.str_concat.rcEffect(),
                    .args = try self.program.ast.addExprSpan(&args),
                    .source_constraint_ty = ty,
                } });
            }
            return current;
        }

        fn lowerBytesLiteral(
            self: *Self,
            ty: Type.TypeId,
            literal: checked_artifact.CheckedStringLiteralId,
        ) Allocator.Error!Ast.ExprId {
            const elem_ty = switch (self.program.types.getType(ty)) {
                .list => |elem| elem,
                else => invariantViolation("mono body lowering bytes literal expected List(U8) type"),
            };
            const bytes = self.checkedStringLiteral(literal);
            if (bytes.len == 0) return try self.program.ast.addExpr(ty, .{ .list = Ast.Span(Ast.ExprId).empty() });

            const elems = try self.allocator.alloc(Ast.ExprId, bytes.len);
            defer self.allocator.free(elems);
            for (bytes, 0..) |byte, i| {
                elems[i] = try self.program.ast.addExpr(elem_ty, .{ .int_lit = @intCast(byte) });
            }
            return try self.program.ast.addExpr(ty, .{ .list = try self.program.ast.addExprSpan(elems) });
        }

        fn lowerResolvedLookup(
            self: *Self,
            expected: ConcreteTypeInfo,
            ref_id: checked_artifact.ResolvedValueRefId,
        ) Allocator.Error!Ast.ExprId {
            const ty = expected.ty;
            const record = self.resolvedValueRef(ref_id);
            return switch (record.ref) {
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                => |local| if (self.local_proc_decls.contains(local.binder))
                    try self.lowerLocalProcLookup(local.binder, expected)
                else
                    try self.program.ast.addExprWithSourcePayload(
                        expected.ty,
                        expected.source_ty,
                        expected.source_ref,
                        .{ .var_ = try self.symbolForBinder(local.binder) },
                    ),
                .local_proc => |local| try self.lowerLocalProcLookup(local.binder, expected),
                .top_level_const,
                .imported_const,
                => |const_use| try self.lowerConstUse(expected, const_use, record.checked_ty),
                .platform_required_const => |required| try self.lowerConstUse(expected, required.const_use, record.checked_ty),
                .top_level_proc,
                .imported_proc,
                .hosted_proc,
                .promoted_top_level_proc,
                => |proc_use| proc_value_blk: {
                    const requested_fn_ty = expected.source_ref;
                    if (try self.summaryPendingLocalRootForProcedureUse(proc_use, requested_fn_ty)) |root| {
                        break :proc_value_blk try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .pending_local_root = root });
                    }
                    if (try self.summaryPendingCallableBindingInstanceForProcedureUse(proc_use, requested_fn_ty)) |request| {
                        break :proc_value_blk try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .pending_callable_instance = request });
                    }
                    const callable = try self.procedureCallableForUse(proc_use, requested_fn_ty);
                    break :proc_value_blk try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .proc_value = .{
                        .proc = try self.reserveCallableProcedure(callable, requested_fn_ty, .{ .proc_value = record.expr }),
                        .published_proc = publishedMirProcedureRefForCallable(callable),
                        .captures = Ast.Span(Ast.CaptureArg).empty(),
                        .fn_ty = ty,
                    } });
                },
                .platform_required_proc => |required| proc_value_blk: {
                    const proc_use = required.procedure;
                    const requested_fn_ty = expected.source_ref;
                    if (try self.summaryPendingLocalRootForProcedureUse(proc_use, requested_fn_ty)) |root| {
                        break :proc_value_blk try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .pending_local_root = root });
                    }
                    if (try self.summaryPendingCallableBindingInstanceForProcedureUse(proc_use, requested_fn_ty)) |request| {
                        break :proc_value_blk try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .pending_callable_instance = request });
                    }
                    const callable = try self.procedureCallableForUse(proc_use, requested_fn_ty);
                    break :proc_value_blk try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .proc_value = .{
                        .proc = try self.reserveCallableProcedure(callable, requested_fn_ty, .{ .proc_value = record.expr }),
                        .published_proc = publishedMirProcedureRefForCallable(callable),
                        .captures = Ast.Span(Ast.CaptureArg).empty(),
                        .fn_ty = ty,
                    } });
                },
                .platform_required_declaration => invariantViolation("mono body lowering reached platform-required declaration lookup as a runtime value"),
            };
        }

        fn lowerLocalProcLookup(
            self: *Self,
            binder: checked_artifact.PatternBinderId,
            expected: ConcreteTypeInfo,
        ) Allocator.Error!Ast.ExprId {
            const instance = try self.ensureLocalProcInstanceForConcrete(binder, expected.source_ref);
            return try self.program.ast.addExprWithSourcePayload(
                expected.ty,
                instance.source_fn_ty,
                instance.source_fn_ty_payload,
                .{ .var_ = instance.symbol },
            );
        }

        fn ensureLocalProcInstanceForConcrete(
            self: *Self,
            binder: checked_artifact.PatternBinderId,
            concrete_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!LocalProcInstanceUse {
            const source_fn_ty = self.program.concrete_source_types.key(concrete_fn);
            const key = LocalProcInstanceKey{
                .binder = binder,
                .source_fn_ty = source_fn_ty,
            };
            const decl = self.local_proc_decls.get(binder) orelse {
                invariantViolation("mono body lowering reached local procedure lookup without a published local procedure declaration");
            };
            const entry = self.local_proc_instances.getPtr(key) orelse {
                invariantViolation("mono body emission reached local procedure instance before graph finalization published it");
            };
            switch (entry.emission) {
                .emitted,
                .emitting,
                => return .{
                    .symbol = entry.symbol,
                    .source_fn_ty = key.source_fn_ty,
                    .source_fn_ty_payload = entry.source_fn_ty_payload,
                },
                .pending => {},
            }

            entry.emission = .emitting;
            errdefer {
                const restoring = self.local_proc_instances.getPtr(key) orelse unreachable;
                if (restoring.emission == .emitting) restoring.emission = .pending;
            }

            const symbol = entry.symbol;
            const body = entry.body;
            const source_fn_ty_payload = entry.source_fn_ty_payload;
            const local_fn = try self.lowerLocalProcInstance(decl, symbol, body);
            const stmt = try self.program.ast.addStmt(.{ .local_fn = local_fn });
            try decl.owner_generated_stmts.append(self.allocator, stmt);
            const completed = self.local_proc_instances.getPtr(key) orelse unreachable;
            if (completed.emission != .emitting) {
                invariantViolation("mono body emission local procedure state changed while emitting its body");
            }
            completed.emission = .emitted;
            return .{
                .symbol = symbol,
                .source_fn_ty = key.source_fn_ty,
                .source_fn_ty_payload = source_fn_ty_payload,
            };
        }

        fn reserveBodyInstance(self: *Self) MonoBodyInstanceId {
            const body: MonoBodyInstanceId = @enumFromInt(self.next_body_instance);
            self.next_body_instance += 1;
            return body;
        }

        fn finalizeLocalProcInstanceForConcrete(
            self: *Self,
            binder: checked_artifact.PatternBinderId,
            concrete_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!LocalProcInstance {
            const source_fn_ty = self.program.concrete_source_types.key(concrete_fn);
            const key = LocalProcInstanceKey{
                .binder = binder,
                .source_fn_ty = source_fn_ty,
            };
            if (self.graph.local_proc_instances.get(key)) |existing| return existing;

            const decl = self.local_proc_decls.get(binder) orelse {
                invariantViolation("mono graph finalization reached local procedure lookup without a published local procedure declaration");
            };
            const lambda = self.localProcLambda(decl.expr);
            const source_symbol = try self.symbolForBinder(binder);
            const instance_symbol = try self.program.addSpecializedLocalFnSymbol(source_symbol);
            const body_instance = self.reserveBodyInstance();
            const instance = LocalProcInstance{
                .symbol = instance_symbol,
                .body = body_instance,
                .source_fn_ty_payload = concrete_fn,
            };
            try self.graph.local_proc_instances.put(key, instance);
            errdefer _ = self.graph.local_proc_instances.remove(key);

            const previous_body = self.current_body;
            self.current_body = body_instance;
            defer self.current_body = previous_body;

            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;

            var instantiator = TypeInstantiator.init(
                self.allocator,
                self.input,
                self.program,
                self.graphInstantiator().templateTypes(),
                self.name_resolver,
                self.graphInstantiator().template_artifact,
            );
            defer instantiator.deinit();

            const previous = self.graphInstantiator();
            self.graph_builder_state.type_instantiator = &instantiator;
            defer self.graph_builder_state.type_instantiator = previous;

            try self.graphInstantiator().unifyTemplateWithConcrete(self.checkedExpr(decl.expr).ty, concrete_fn);

            const fn_ty = try self.graphInstantiator().lowerConcreteRef(concrete_fn);
            const params = try self.paramTypesFromConcreteFunction(concrete_fn);
            const ret_ty = try self.returnTypeFromConcreteFunction(concrete_fn);
            self.current_return_type = ret_ty;
            self.current_return_source_ref = ret_ty.source_ref;

            const param_refs = try self.sourceParamRefsFromFunction(concrete_fn);
            defer if (param_refs.len != 0) self.allocator.free(param_refs);
            if (param_refs.len != lambda.args.len) {
                invariantViolation("mono graph finalization local procedure parameter count disagreed with requested function type");
            }
            for (lambda.args, param_refs) |arg, param_ref| {
                try self.recordPatternDemand(arg, param_ref);
            }

            try self.graph.body_instances.put(body_instance, .{
                .source_fn_ty = source_fn_ty,
                .source_fn_ty_payload = concrete_fn,
                .fn_ty = fn_ty,
                .params = params,
                .ret_ty = ret_ty,
                .kind = .{ .local_proc = .{
                    .owner = previous_body,
                    .binder = binder,
                    .expr = decl.expr,
                    .site = self.nestedProcSite(decl.expr, lambda.kind),
                    .args = lambda.args,
                    .body = lambda.body,
                } },
            });
            try self.collectExprDemand(lambda.body, ret_ty.source_ref);
            try self.publishLocalConcreteDemands();
            try self.publishExprConcreteDemands();

            return instance;
        }

        fn lowerLocalProcInstance(
            self: *Self,
            decl: LocalProcDecl,
            instance_symbol: Ast.Symbol,
            body_instance: MonoBodyInstanceId,
        ) Allocator.Error!Ast.LetFn {
            const instance = self.graph.body_instances.get(body_instance) orelse {
                invariantViolation("mono body emission reached local procedure without finalized body instance");
            };
            const local = switch (instance.kind) {
                .local_proc => |local| local,
                .root => invariantViolation("mono body emission attempted to emit root body as local procedure"),
                .closure_value => invariantViolation("mono body emission attempted to emit closure body as local procedure"),
            };
            if (local.expr != decl.expr) {
                invariantViolation("mono body emission local procedure declaration disagreed with finalized body instance");
            }

            const previous_body = self.current_body;
            self.current_body = body_instance;
            defer self.current_body = previous_body;

            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            self.current_return_type = instance.ret_ty;
            self.current_return_source_ref = instance.ret_ty.source_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;
            const params = try self.lowerParamBundleFromFunction(local.args, instance.source_fn_ty_payload);
            defer self.deinitParamBundle(params);
            return .{
                .site = local.site,
                .source_fn_ty = instance.source_fn_ty,
                .source_fn_ty_payload = instance.source_fn_ty_payload,
                .recursive = false,
                .bind = .{
                    .ty = instance.fn_ty,
                    .source_ty = instance.source_fn_ty,
                    .source_ty_payload = instance.source_fn_ty_payload,
                    .symbol = instance_symbol,
                },
                .args = params.args,
                .body = try self.lowerBodyWithParamSetup(local.body, instance.ret_ty, params),
            };
        }

        const LocalProcLambda = struct {
            kind: checked_artifact.NestedProcKind,
            args: []const checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        };

        fn localProcLambda(self: *Self, expr_id: checked_artifact.CheckedExprId) LocalProcLambda {
            const expr = self.checkedExpr(expr_id);
            return switch (expr.data) {
                .lambda => |lambda| .{
                    .kind = .local_function,
                    .args = lambda.args,
                    .body = lambda.body,
                },
                .closure => |closure| blk: {
                    const lambda_expr = self.checkedExpr(closure.lambda);
                    switch (lambda_expr.data) {
                        .lambda => |lambda| break :blk .{
                            .kind = .closure,
                            .args = lambda.args,
                            .body = lambda.body,
                        },
                        else => invariantViolation("mono body lowering expected local closure declaration to reference a checked lambda"),
                    }
                },
                else => invariantViolation("mono body lowering expected local procedure declaration to reference a lambda-like expression"),
            };
        }

        fn localProcDeclForStatement(
            self: *Self,
            statement: checked_artifact.CheckedStatement,
        ) ?struct { pattern: checked_artifact.CheckedPatternId, expr: checked_artifact.CheckedExprId } {
            return switch (statement.data) {
                .decl => |decl| switch (self.checkedExpr(decl.expr).data) {
                    .lambda, .closure => .{ .pattern = decl.pattern, .expr = decl.expr },
                    else => null,
                },
                else => null,
            };
        }

        fn restoreLocalProcDecls(
            self: *Self,
            restorations: []const LocalProcDeclRestore,
        ) void {
            var i = restorations.len;
            while (i > 0) {
                i -= 1;
                const restore = restorations[i];
                if (restore.previous) |previous| {
                    self.local_proc_decls.put(restore.binder, previous) catch unreachable;
                } else {
                    _ = self.local_proc_decls.remove(restore.binder);
                }
            }
        }

        fn finalizeConstUseForExpected(
            self: *Self,
            const_use: checked_artifact.ConstUseTemplate,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            _ = const_use.requested_source_ty_payload orelse return;
            const concrete_info = try self.runtimeConcreteTypeInfo(expected_ref);
            const requested_key = concrete_info.source_ty;
            const key = checked_artifact.ConstInstantiationKey{
                .const_ref = const_use.const_ref,
                .requested_source_ty = requested_key,
            };
            if (constInstanceForKey(self.input, self.input.root.artifact.key, key) != null) return;
            if (constInstanceForKey(self.input, const_use.const_ref.artifact, key) != null) return;

            switch (self.input.mode) {
                .runnable => return,
                .comptime_dependency_summary => {},
            }
            if (self.graph.finalized_summary_payloads.contains(concrete_info.source_ref)) return;

            if (try self.concreteConstProducer(const_use.const_ref)) |producer| {
                try self.recordConcreteConstProducerType(producer);
                if (!std.mem.eql(u8, &producer.key.bytes, &requested_key.bytes)) {
                    try self.recordConcreteConstDependencyType(concrete_info.source_ref);
                }
            } else {
                try self.recordConcreteConstDependencyType(concrete_info.source_ref);
            }
            _ = try self.finalizeSummaryPayloadForConcreteType(
                concrete_info.source_ref,
                requested_key,
                "mono dependency-summary constant request payload key disagrees with requested type",
            );
        }

        fn finalizeSummaryPayloadForConcreteType(
            self: *Self,
            concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            expected_key: canonical.CanonicalTypeKey,
            comptime mismatch_message: []const u8,
        ) Allocator.Error!checked_artifact.CheckedTypeId {
            if (mode != .graph_builder) {
                @compileError("only MonoSpecializationGraphBuilder publishes dependency-summary checked payloads");
            }
            if (self.graph.finalized_summary_payloads.get(concrete_ref)) |existing| return existing;
            const payload = try self.checkedPayloadForConcreteSummaryType(concrete_ref, expected_key, mismatch_message);
            try self.graph.finalized_summary_payloads.put(concrete_ref, payload);
            return payload;
        }

        fn finalizedSummaryPayload(
            self: *Self,
            concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) checked_artifact.CheckedTypeId {
            return self.graph.finalized_summary_payloads.get(concrete_ref) orelse {
                invariantViolation("mono body emission reached dependency-summary request before graph finalization published its requested payload");
            };
        }

        fn lowerConstUse(
            self: *Self,
            expected: ConcreteTypeInfo,
            const_use: checked_artifact.ConstUseTemplate,
            _: checked_artifact.CheckedTypeId,
        ) Allocator.Error!Ast.ExprId {
            _ = const_use.requested_source_ty_payload orelse {
                debug.invariant(false, "mono body lowering invariant violated: constant use had no requested source type payload");
                unreachable;
            };
            const concrete_info = try self.runtimeConcreteTypeInfo(expected.source_ref);
            const concrete_ref = concrete_info.source_ref;
            const requested_key = concrete_info.source_ty;
            if (!std.mem.eql(u8, &requested_key.bytes, &expected.source_ty.bytes)) {
                invariantViolation("mono body lowering constant use concrete payload key disagrees with expected source type");
            }
            const key = checked_artifact.ConstInstantiationKey{
                .const_ref = const_use.const_ref,
                .requested_source_ty = requested_key,
            };
            const instance = constInstanceForKey(self.input, self.input.root.artifact.key, key) orelse
                constInstanceForKey(self.input, const_use.const_ref.artifact, key) orelse {
                switch (self.input.mode) {
                    .comptime_dependency_summary => {
                        return try self.program.ast.addExpr(expected.ty, .{ .const_ref = .{
                            .key = key,
                            .requested_source_ty_payload = self.finalizedSummaryPayload(concrete_ref),
                        } });
                    },
                    .runnable => {
                        debug.invariant(false, "mono body lowering invariant violated: constant use had no sealed concrete instance in the requesting artifact");
                        unreachable;
                    },
                }
            };
            var dependency_state = ConcreteDependencyReservationState.init(self.allocator);
            defer dependency_state.deinit();
            try reserveConstInstanceRefDependencies(self.input, self.program, self.queue, &dependency_state, instance);
            return try self.program.ast.addExpr(expected.ty, .{ .const_instance = instance });
        }

        fn concreteConstProducer(
            self: *Self,
            ref: checked_artifact.ConstRef,
        ) Allocator.Error!?ConcreteConstProducer {
            const checked_types = checkedTypesForKey(self.input, ref.artifact) orelse {
                invariantViolation("mono body lowering constant use referenced unavailable producer artifact");
            };
            const scheme = checkedTypeSchemeForKey(checked_types, ref.source_scheme) orelse {
                invariantViolation("mono body lowering constant use referenced unavailable producer source scheme");
            };
            if (!try checkedTypeViewIsConcreteConstProducerScheme(self.allocator, checked_types, scheme.root)) return null;

            const root_index: usize = @intFromEnum(scheme.root);
            if (root_index >= checked_types.roots.len) {
                invariantViolation("mono body lowering concrete const producer scheme root was missing");
            }
            return .{
                .artifact = ref.artifact,
                .payload = scheme.root,
                .key = checked_types.roots[root_index].key,
            };
        }

        fn recordConcreteConstProducerType(
            self: *Self,
            producer: ConcreteConstProducer,
        ) Allocator.Error!void {
            _ = self.input.checking_artifact_sink orelse {
                invariantViolation("compile-time dependency summary constant use had no mutable checked artifact sink");
            };
            try self.program.concrete_dependency_type_projections.append(self.allocator, .{ .producer = producer });
        }

        fn recordConcreteConstDependencyType(
            self: *Self,
            concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            _ = self.input.checking_artifact_sink orelse {
                invariantViolation("compile-time dependency summary constant use had no mutable checked artifact sink");
            };
            try self.program.concrete_dependency_type_projections.append(self.allocator, .{ .concrete_ref = concrete_ref });
        }

        fn checkedPayloadForConcreteSummaryType(
            self: *Self,
            concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            expected_key: canonical.CanonicalTypeKey,
            comptime mismatch_message: []const u8,
        ) Allocator.Error!checked_artifact.CheckedTypeId {
            const artifact_sink = self.input.checking_artifact_sink orelse {
                invariantViolation("compile-time dependency summary requested a checked type payload without a mutable checked artifact sink");
            };

            const local_root = try self.graphInstantiator().materializeConcreteRef(concrete_ref);
            const local_view = self.program.concrete_source_types.localView();
            const root_index = @intFromEnum(local_root);
            if (root_index >= local_view.roots.len) {
                invariantViolation("compile-time dependency summary materialized type payload outside local type view");
            }
            if (!std.meta.eql(local_view.roots[root_index].key.bytes, expected_key.bytes)) {
                invariantViolation(mismatch_message);
            }

            var dependency_views = try inputDependencyViews(self.allocator, self.input);
            defer dependency_views.deinit(self.allocator);

            var projector = checked_artifact.CheckedTypeProjector.init(self.allocator, artifact_sink, dependency_views.views);
            defer projector.deinit();

            const projected = try projector.projectCheckedTypeViewRootWithNames(
                local_view,
                &self.program.canonical_names,
                local_root,
            );
            const projected_index = @intFromEnum(projected);
            if (projected_index >= artifact_sink.checked_types.roots.len or
                !std.meta.eql(artifact_sink.checked_types.roots[projected_index].key.bytes, expected_key.bytes))
            {
                invariantViolation("compile-time dependency summary projected checked payload key disagrees with requested type");
            }
            return projected;
        }

        fn lowerList(
            self: *Self,
            ty: Type.TypeId,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            items: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const elem_ty = try self.listElementTypeFromConcrete(source_ref);
            const span = try self.lowerExprSpanSameConcrete(items, elem_ty);
            return try self.program.ast.addExpr(ty, .{ .list = span });
        }

        fn listElementTypeFromConcrete(
            self: *Self,
            source_list: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteTypeInfo {
            return try self.runtimeConcreteTypeInfo(try self.listElementSourceRef(source_list));
        }

        fn listElementSourceRef(
            self: *Self,
            source_list: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            var current = source_list;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                    .nominal => |nominal| {
                        if (nominal.builtin != .list or nominal.args.len != 1) {
                            invariantViolation("mono body lowering expected concrete list type for list literal");
                        }
                        return try self.concreteSourceChildRef(current, .{ .tag = .list_elem }, nominal.args[0]);
                    },
                    else => invariantViolation("mono body lowering expected concrete list type for list literal"),
                }
            }
        }

        fn boxPayloadTypeFromConcrete(
            self: *Self,
            source_box: ConcreteSourceType.ConcreteSourceTypeRef,
            payload_ty: Type.TypeId,
        ) Allocator.Error!ConcreteTypeInfo {
            var current = source_box;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                    .nominal => |nominal| {
                        if (nominal.builtin != .box or nominal.args.len != 1) {
                            invariantViolation("mono body lowering expected concrete Box(T) type for Box inspect");
                        }
                        const payload_ref = try self.concreteSourceChildRef(current, .{ .tag = .box_payload }, nominal.args[0]);
                        const payload_info = try self.runtimeConcreteTypeInfo(payload_ref);
                        return .{
                            .ty = payload_ty,
                            .source_ty = payload_info.source_ty,
                            .source_ref = payload_info.source_ref,
                        };
                    },
                    else => invariantViolation("mono body lowering expected concrete Box(T) type for Box inspect"),
                }
            }
        }

        fn lowerTuple(
            self: *Self,
            ty: Type.TypeId,
            items: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const span = try self.lowerExprSpan(items);
            return try self.program.ast.addExpr(ty, .{ .tuple = span });
        }

        fn lowerBlock(
            self: *Self,
            ty: Type.TypeId,
            statements: []const checked_artifact.CheckedStatementId,
            final_expr: checked_artifact.CheckedExprId,
            final_expected_ty: ExprExpectedType,
        ) Allocator.Error!Ast.ExprId {
            var generated_local_fns = std.ArrayList(Ast.StmtId).empty;
            defer generated_local_fns.deinit(self.allocator);

            var restorations = std.ArrayList(LocalProcDeclRestore).empty;
            defer restorations.deinit(self.allocator);
            var restored_decls = false;
            errdefer if (!restored_decls) self.restoreLocalProcDecls(restorations.items);

            try self.registerBlockLocalProcDecls(statements, &generated_local_fns, &restorations);

            var lowered_stmts = std.ArrayList(Ast.StmtId).empty;
            defer lowered_stmts.deinit(self.allocator);
            var can_reach_final = true;
            for (statements) |statement_id| {
                const statement = self.checkedStatement(statement_id);
                if (self.localProcDeclForStatement(statement) != null) continue;
                try self.lowerStmtInto(statement_id, &lowered_stmts);
                if (!self.checkedStatementCanCompleteNormally(statement_id)) {
                    can_reach_final = false;
                    break;
                }
            }

            const final = if (can_reach_final)
                try self.lowerExprWithExpected(final_expr, final_expected_ty)
            else
                try self.program.ast.addExpr(ty, .runtime_error);

            const generated_count = generated_local_fns.items.len;
            const normal_count = lowered_stmts.items.len;
            const combined = try self.allocator.alloc(Ast.StmtId, generated_count + normal_count);
            defer self.allocator.free(combined);
            @memcpy(combined[0..generated_count], generated_local_fns.items);
            @memcpy(combined[generated_count..], lowered_stmts.items);
            const stmt_span = try self.program.ast.addStmtSpan(combined);

            self.restoreLocalProcDecls(restorations.items);
            restored_decls = true;
            return try self.program.ast.addExpr(ty, .{ .block = .{
                .stmts = stmt_span,
                .final_expr = final,
            } });
        }

        fn registerBlockLocalProcDecls(
            self: *Self,
            statements: []const checked_artifact.CheckedStatementId,
            generated_local_fns: *std.ArrayList(Ast.StmtId),
            restorations: *std.ArrayList(LocalProcDeclRestore),
        ) Allocator.Error!void {
            for (statements) |statement_id| {
                const statement = self.checkedStatement(statement_id);
                const decl = self.localProcDeclForStatement(statement) orelse continue;
                const binder = self.binderForSimplePattern(self.checkedPattern(decl.pattern).data);
                const previous = try self.local_proc_decls.fetchPut(binder, .{
                    .pattern = decl.pattern,
                    .expr = decl.expr,
                    .owner_generated_stmts = generated_local_fns,
                });
                try restorations.append(self.allocator, .{
                    .binder = binder,
                    .previous = if (previous) |entry| entry.value else null,
                });
            }
        }

        fn collectBlockLocalDemandsWithSourceRef(
            self: *Self,
            statements: []const checked_artifact.CheckedStatementId,
            final_expr: checked_artifact.CheckedExprId,
            final_expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            var generated_local_fns = std.ArrayList(Ast.StmtId).empty;
            defer generated_local_fns.deinit(self.allocator);

            var restorations = std.ArrayList(LocalProcDeclRestore).empty;
            defer restorations.deinit(self.allocator);

            try self.registerBlockLocalProcDecls(statements, &generated_local_fns, &restorations);
            defer self.restoreLocalProcDecls(restorations.items);

            var can_reach_final = true;
            for (statements) |statement_id| {
                const statement = self.checkedStatement(statement_id);
                if (self.localProcDeclForStatement(statement) != null) continue;
                try self.collectStatementDemand(statement);
                if (!self.checkedStatementCanCompleteNormally(statement_id)) {
                    can_reach_final = false;
                    break;
                }
            }
            if (can_reach_final) {
                try self.collectExprDemand(final_expr, final_expected_ref);
            }
        }

        fn collectStatementDemand(
            self: *Self,
            statement: checked_artifact.CheckedStatement,
        ) Allocator.Error!void {
            switch (statement.data) {
                .decl => |decl| try self.collectPatternedExprDemand(decl.pattern, decl.expr),
                .var_ => |var_| try self.collectPatternedExprDemand(var_.pattern, var_.expr),
                .reassign => |reassign| try self.collectPatternedExprDemand(reassign.pattern, reassign.expr),
                .dbg => |expr| {
                    const expr_ref = try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(expr).ty);
                    try self.collectExprDemand(expr, expr_ref);
                    try self.finalizeStrInspectCallTarget(try self.runtimeConcreteTypeInfo(expr_ref), try self.ensureStrType());
                },
                .expr => |expr| try self.collectExprDemand(expr, try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(expr).ty)),
                .expect => |expr| {
                    const bool_info = try self.boolConcreteTypeInfo();
                    try self.collectExprDemand(expr, bool_info.source_ref);
                },
                .for_ => |for_| try self.collectForDemand(
                    for_.plan orelse invariantViolation("checked for statement reached mono without an iterator-for plan"),
                    for_.pattern,
                    for_.body,
                ),
                .while_ => |while_| {
                    const bool_info = try self.boolConcreteTypeInfo();
                    try self.collectExprDemand(while_.cond, bool_info.source_ref);
                    try self.collectExprDemand(while_.body, try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(while_.body).ty));
                },
                .return_ => |ret| {
                    const return_source_ref = self.current_return_source_ref orelse {
                        invariantViolation("mono body lowering reached return without an enclosing procedure return type");
                    };
                    try self.collectExprDemand(ret.expr, return_source_ref);
                },
                .crash,
                .break_,
                .runtime_error,
                .import_,
                .alias_decl,
                .nominal_decl,
                .type_anno,
                .type_var_alias,
                => {},
                .pending => invariantViolation("mono body lowering reached pending checked statement while collecting local demand"),
            }
        }

        fn collectPatternedExprDemand(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!void {
            const pattern = self.checkedPattern(pattern_id);
            const source_ref = try self.concreteSourceRefForCheckedPreservingVariables(pattern.ty);
            try self.recordPatternDemand(pattern_id, source_ref);
            try self.collectExprDemand(
                expr_id,
                self.demandedSourceRefForPattern(pattern_id) orelse source_ref,
            );
        }

        fn demandedSourceRefForPattern(
            self: *const Self,
            pattern_id: checked_artifact.CheckedPatternId,
        ) ?ConcreteSourceType.ConcreteSourceTypeRef {
            const pattern = self.checkedPattern(pattern_id);
            const binder = switch (pattern.data) {
                .assign => |assign| assign,
                .as => |as| as.binder,
                else => return null,
            };
            if (self.graph.local_type_demands.get(self.scopedBinder(binder))) |source_ref| return source_ref;
            if (self.graph.local_symbol_types.get(self.scopedBinder(binder))) |existing| return existing.source_ref;
            return null;
        }

        fn collectExprDemand(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const expr = self.checkedExpr(expr_id);
            try self.recordExprDemand(expr_id, expected_ref);
            try self.graphInstantiator().unifyTemplateWithConcrete(expr.ty, expected_ref);
            switch (expr.data) {
                .lookup_local => |lookup| try self.collectResolvedLookupDemand(
                    lookup.resolved orelse invariantViolation("checked lookup_local reached mono without a resolved value ref"),
                    expected_ref,
                ),
                .lookup_external => |lookup| try self.collectResolvedLookupDemand(
                    lookup orelse invariantViolation("checked lookup_external reached mono without a resolved value ref"),
                    expected_ref,
                ),
                .lookup_required => |lookup| try self.collectResolvedLookupDemand(
                    lookup orelse invariantViolation("checked lookup_required reached mono without a resolved value ref"),
                    expected_ref,
                ),
                .list => |items| {
                    const runtime_expected = try self.runtimeConcreteTypeInfo(expected_ref);
                    const item_ref = try self.listElementSourceRef(runtime_expected.source_ref);
                    _ = try self.runtimeConcreteTypeInfo(item_ref);
                    for (items) |item| try self.collectExprDemand(item, item_ref);
                },
                .empty_list => {},
                .str => |segments| for (segments) |segment| try self.collectExprDemand(segment, expected_ref),
                .tuple => |items| {
                    const item_refs = try self.concreteTupleElementRefs(expected_ref, items.len);
                    defer if (item_refs.len != 0) self.allocator.free(item_refs);
                    for (items, item_refs) |item, item_ref| {
                        _ = try self.runtimeConcreteTypeInfo(item_ref);
                        try self.collectExprDemand(item, item_ref);
                    }
                },
                .record => |record| {
                    const runtime_expected = try self.runtimeConcreteTypeInfo(expected_ref);
                    for (record.fields) |field| {
                        const field_ref = try self.concreteRecordFieldRef(runtime_expected.source_ref, try self.recordFieldLabel(field.label));
                        _ = try self.runtimeConcreteTypeInfo(field_ref);
                        try self.collectExprDemand(field.value, field_ref);
                    }
                    if (record.ext) |ext| try self.collectExprDemand(ext, runtime_expected.source_ref);
                },
                .empty_record => {},
                .tag => |tag| {
                    const runtime_expected = try self.runtimeConcreteTypeInfo(expected_ref);
                    const payload_refs = try self.concreteTagPayloadRefsForUnionType(runtime_expected.source_ref, try self.tagLabel(tag.name));
                    defer if (payload_refs.len != 0) self.allocator.free(payload_refs);
                    if (payload_refs.len != tag.args.len) invariantViolation("mono demand collection tag payload arity disagreed with checked expression");
                    for (tag.args, payload_refs) |arg, payload_ref| {
                        _ = try self.runtimeConcreteTypeInfo(payload_ref);
                        try self.collectExprDemand(arg, payload_ref);
                    }
                },
                .zero_argument_tag => {},
                .nominal => |nominal| {
                    const runtime_expected = try self.runtimeConcreteTypeInfo(expected_ref);
                    const backing_ref = try self.concreteNominalBackingRef(runtime_expected.source_ref);
                    _ = try self.runtimeConcreteTypeInfo(backing_ref);
                    try self.collectExprDemand(nominal.backing_expr, backing_ref);
                },
                .block => |block| try self.collectBlockLocalDemandsWithSourceRef(
                    block.statements,
                    block.final_expr,
                    expected_ref,
                ),
                .if_ => |if_| {
                    const bool_info = try self.boolConcreteTypeInfo();
                    for (if_.branches) |branch| {
                        try self.collectExprDemand(branch.cond, bool_info.source_ref);
                        try self.collectExprDemand(branch.body, expected_ref);
                    }
                    try self.collectExprDemand(if_.final_else, expected_ref);
                },
                .match_ => |match_| try self.collectMatchDemand(match_, expected_ref),
                .lambda => |lambda| try self.collectLambdaDemand(expected_ref, expr_id, .local_function, lambda.args, lambda.body),
                .closure => |closure| {
                    const lambda_expr = self.checkedExpr(closure.lambda);
                    switch (lambda_expr.data) {
                        .lambda => |lambda| try self.collectLambdaDemand(expected_ref, expr_id, .closure, lambda.args, lambda.body),
                        else => invariantViolation("mono demand collection expected closure to reference a checked lambda"),
                    }
                },
                .call => |call| try self.collectCallDemand(expr_id, call, expected_ref),
                .dispatch_call => |plan| try self.collectStaticDispatchDemand(
                    expected_ref,
                    plan orelse invariantViolation("checked dispatch call reached mono without a StaticDispatchCallPlan"),
                ),
                .method_eq => |plan| try self.collectStaticDispatchDemand(
                    expected_ref,
                    plan orelse invariantViolation("checked method equality reached mono without a StaticDispatchCallPlan"),
                ),
                .type_dispatch_call => |plan| try self.collectStaticDispatchDemand(
                    expected_ref,
                    plan orelse invariantViolation("checked type dispatch call reached mono without a StaticDispatchCallPlan"),
                ),
                .field_access => |access| {
                    const receiver_ref = try self.sourceRefForDemandedExpr(access.receiver);
                    try self.graphInstantiator().unifyTemplateWithConcrete(
                        self.checkedExpr(access.receiver).ty,
                        receiver_ref,
                    );
                    try self.graphInstantiator().unifyConcreteRefs(
                        try self.concreteRecordFieldRef(receiver_ref, try self.recordFieldLabel(access.field_name)),
                        expected_ref,
                    );
                    _ = try self.runtimeConcreteTypeInfo(expected_ref);
                    try self.collectExprDemand(access.receiver, receiver_ref);
                },
                .tuple_access => |access| try self.collectExprDemand(
                    access.tuple,
                    try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(access.tuple).ty),
                ),
                .structural_eq => |eq| {
                    const operand_ref = try self.sourceRefForDemandedExpr(eq.lhs);
                    try self.collectExprDemand(eq.lhs, operand_ref);
                    try self.collectExprDemand(eq.rhs, operand_ref);
                },
                .binop => |binop| try self.collectBinopDemand(binop, expected_ref),
                .unary_minus => |child| try self.collectExprDemand(child, expected_ref),
                .unary_not => |child| {
                    const bool_info = try self.boolConcreteTypeInfo();
                    try self.collectExprDemand(child, bool_info.source_ref);
                },
                .return_ => |ret| {
                    const return_source_ref = self.current_return_source_ref orelse {
                        invariantViolation("mono demand collection reached return without an enclosing procedure return type");
                    };
                    try self.collectExprDemand(ret.expr, return_source_ref);
                },
                .for_ => |for_| try self.collectForDemand(
                    for_.plan orelse invariantViolation("checked for expression reached mono without an iterator-for plan"),
                    for_.pattern,
                    for_.body,
                ),
                .run_low_level => |run_low_level| {
                    for (run_low_level.args) |arg| {
                        try self.collectExprDemand(arg, try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(arg).ty));
                    }
                },
                .dbg => |child| {
                    const child_ref = try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(child).ty);
                    try self.collectExprDemand(child, child_ref);
                    try self.finalizeStrInspectCallTarget(try self.runtimeConcreteTypeInfo(child_ref), try self.ensureStrType());
                },
                .expect => |child| try self.collectExprDemand(child, try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(child).ty)),
                .num,
                .frac_f32,
                .frac_f64,
                .dec,
                .dec_small,
                .typed_int,
                .typed_frac,
                .str_segment,
                .bytes_literal,
                .runtime_error,
                .crash,
                .hosted_lambda,
                => {},
                .ellipsis, .anno_only, .pending => invariantViolation("mono demand collection received a non-runtime checked expression form"),
            }
        }

        fn recordExprDemand(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const scoped_expr = self.scopedExpr(expr_id);
            if (self.graph.expr_type_demands.getPtr(scoped_expr)) |existing| {
                try self.graphInstantiator().unifyConcreteRefs(existing.*, source_ref);
                existing.* = source_ref;
                return;
            }
            if (self.graph.expr_types.get(scoped_expr)) |existing| {
                try self.graphInstantiator().unifyConcreteRefs(existing.source_ref, source_ref);
                try self.graph.expr_type_demands.put(scoped_expr, source_ref);
                return;
            }
            try self.graph.expr_type_demands.put(scoped_expr, source_ref);
        }

        fn collectBinopDemand(
            self: *Self,
            binop: anytype,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            switch (binop.op) {
                .add,
                .sub,
                .mul,
                .div,
                .rem,
                .div_trunc,
                => {
                    try self.collectExprDemand(binop.lhs, expected_ref);
                    try self.collectExprDemand(binop.rhs, expected_ref);
                },
                .lt,
                .gt,
                .le,
                .ge,
                .eq,
                .ne,
                => {
                    const operand_ref = try self.sourceRefForDemandedExpr(binop.lhs);
                    try self.collectExprDemand(binop.lhs, operand_ref);
                    try self.collectExprDemand(binop.rhs, operand_ref);
                },
                .@"and",
                .@"or",
                => {
                    const bool_info = try self.boolConcreteTypeInfo();
                    try self.collectExprDemand(binop.lhs, bool_info.source_ref);
                    try self.collectExprDemand(binop.rhs, bool_info.source_ref);
                },
            }
        }

        fn collectMatchDemand(
            self: *Self,
            match_: anytype,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            if (match_.is_try_suffix) {
                try self.collectTrySuffixMatchDemand(match_, expected_ref);
                return;
            }

            const cond_ref = try self.sourceRefForDemandedExpr(match_.cond);
            for (match_.branches) |branch| {
                for (branch.patterns) |branch_pattern| {
                    try self.recordPatternDemandWithRemaps(branch_pattern.pattern, cond_ref, branch_pattern.binder_remaps);
                }
            }
            try self.collectExprDemand(match_.cond, cond_ref);
            for (match_.branches) |branch| {
                for (branch.patterns) |branch_pattern| {
                    if (branch.guard) |guard| {
                        const bool_info = try self.boolConcreteTypeInfo();
                        try self.collectExprDemand(guard, bool_info.source_ref);
                    }
                    if (!branch_pattern.degenerate) {
                        try self.collectExprDemand(branch.value, expected_ref);
                    }
                }
            }
        }

        fn collectTrySuffixMatchDemand(
            self: *Self,
            match_: anytype,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const cond_ref = try self.sourceRefForDemandedExpr(match_.cond);
            for (match_.branches) |branch| {
                for (branch.patterns) |branch_pattern| {
                    try self.recordPatternDemandWithRemaps(branch_pattern.pattern, cond_ref, branch_pattern.binder_remaps);
                    if (branch.guard) |guard| {
                        const bool_info = try self.boolConcreteTypeInfo();
                        try self.collectExprDemand(guard, bool_info.source_ref);
                    }
                    if (!branch_pattern.degenerate) {
                        try self.collectExprDemand(branch.value, expected_ref);
                    }
                }
            }
            try self.collectExprDemand(match_.cond, cond_ref);
        }

        fn collectLambdaDemand(
            self: *Self,
            source_fn_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            site_expr: checked_artifact.CheckedExprId,
            site_kind: checked_artifact.NestedProcKind,
            args: []const checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        ) Allocator.Error!void {
            const scoped_site = self.scopedExpr(site_expr);
            if (self.graph.closure_instances.contains(scoped_site)) return;

            const body_instance = self.reserveBodyInstance();
            try self.graph.closure_instances.put(scoped_site, body_instance);
            errdefer _ = self.graph.closure_instances.remove(scoped_site);

            const previous_body = self.current_body;
            self.current_body = body_instance;
            defer self.current_body = previous_body;

            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            const ret_ref = try self.sourceReturnRefFromFunction(source_fn_ref);
            self.current_return_type = null;
            self.current_return_source_ref = ret_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;

            const param_refs = try self.sourceParamRefsFromFunction(source_fn_ref);
            defer if (param_refs.len != 0) self.allocator.free(param_refs);
            if (param_refs.len != args.len) invariantViolation("mono demand collection lambda arity disagreed with expected function type");
            for (args, param_refs) |arg, param_ref| {
                try self.recordPatternDemand(arg, param_ref);
            }
            try self.collectExprDemand(body, ret_ref);

            const runtime_fn_ref = try self.graphInstantiator().runtimeConcreteRef(source_fn_ref);
            const runtime_ty = try self.graphInstantiator().lowerConcreteRef(runtime_fn_ref);
            const params = try self.paramTypesFromConcreteFunction(runtime_fn_ref);
            const ret_ty = try self.returnTypeFromConcreteFunction(runtime_fn_ref);
            try self.publishFinalizedParamPatternInfos(args, params);
            try self.publishLocalConcreteDemands();
            try self.publishExprConcreteDemands();
            try self.graph.body_instances.put(body_instance, .{
                .source_fn_ty = self.program.concrete_source_types.key(runtime_fn_ref),
                .source_fn_ty_payload = runtime_fn_ref,
                .fn_ty = runtime_ty,
                .params = params,
                .ret_ty = ret_ty,
                .kind = .{ .closure_value = .{
                    .owner = previous_body,
                    .expr = site_expr,
                    .site = self.nestedProcSite(site_expr, site_kind),
                    .args = args,
                    .body = body,
                } },
            });
        }

        fn collectCallDemand(
            self: *Self,
            call_expr: checked_artifact.CheckedExprId,
            call: anytype,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const proc_use = self.procedureUseForExpr(call.func);
            const local_proc_use = try self.localProcUseForExpr(call.func);

            var call_instantiator = try self.graphInstantiator().fork();
            defer call_instantiator.deinit();

            const previous_instantiator = self.graphInstantiator();
            self.graph_builder_state.type_instantiator = &call_instantiator;
            defer self.graph_builder_state.type_instantiator = previous_instantiator;

            const source_fn_ty_payload = self.callSourceFnPayload(call.func, call.source_fn_ty_payload);
            try self.bindKnownCallCalleeType(source_fn_ty_payload, call.func);
            try self.bindKnownCallArgumentDemandTypes(source_fn_ty_payload, call.args);
            try self.unifyFunctionReturnWithConcrete(source_fn_ty_payload, expected_ref);

            const concrete_fn = try self.graphInstantiator().concreteRefForTemplateType(source_fn_ty_payload);
            const source_arg_refs = try self.sourceParamRefsFromFunction(concrete_fn);
            defer if (source_arg_refs.len != 0) self.allocator.free(source_arg_refs);
            if (source_arg_refs.len != call.args.len) invariantViolation("mono demand collection call argument count disagreed with checked function type");
            for (call.args, source_arg_refs) |arg, source_arg_ref| {
                try self.collectExprDemand(arg, source_arg_ref);
            }
            if (proc_use == null and local_proc_use == null) {
                try self.collectExprDemand(call.func, concrete_fn);
            }

            const finalized = try self.runtimeCallInstantiation(.{
                .concrete_fn = concrete_fn,
                .func_ty = try self.graphInstantiator().lowerConcreteRef(concrete_fn),
                .requested_source_fn_ty = self.program.concrete_source_types.key(concrete_fn),
                .ret_ty = try self.returnTypeFromConcreteFunction(concrete_fn),
                .arg_infos = &.{},
            });
            if (local_proc_use) |local_proc| {
                _ = try self.finalizeLocalProcInstanceForConcrete(local_proc.binder, finalized.concrete_fn);
            }
            if (proc_use) |use| {
                _ = try self.summaryPendingCallableBindingInstanceForProcedureUse(use, finalized.concrete_fn);
            }

            try previous_instantiator.absorbConcreteVariableSubstitutionsFrom(&call_instantiator);
            try self.rememberFinalizedCall(call_expr, finalized);
        }

        fn rememberFinalizedCall(
            self: *Self,
            call_expr: checked_artifact.CheckedExprId,
            call: CallInstantiationInfo,
        ) Allocator.Error!void {
            const scoped_expr = self.scopedExpr(call_expr);
            if (self.graph.finalized_calls.contains(scoped_expr)) {
                invariantViolation("mono graph finalized one call expression more than once");
            }
            try self.graph.finalized_calls.put(scoped_expr, call);
        }

        fn finalizedCall(
            self: *Self,
            call_expr: checked_artifact.CheckedExprId,
        ) CallInstantiationInfo {
            return self.graph.finalized_calls.get(self.scopedExpr(call_expr)) orelse {
                invariantViolation("mono body emission reached call before graph finalization published it");
            };
        }

        fn collectStaticDispatchDemand(
            self: *Self,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            plan_id: checked_artifact.StaticDispatchPlanId,
        ) Allocator.Error!void {
            var dispatch_instantiator = try self.graphInstantiator().fork();
            defer dispatch_instantiator.deinit();

            const previous_instantiator = self.graphInstantiator();
            self.graph_builder_state.type_instantiator = &dispatch_instantiator;
            defer self.graph_builder_state.type_instantiator = previous_instantiator;

            const plan = self.staticDispatchPlan(plan_id);
            try self.unifyFunctionReturnWithConcrete(plan.callable_ty, expected_ref);
            try self.bindStaticDispatchDemandArgumentTypes(plan);

            const dispatcher_info = try self.staticDispatchDispatcherTypeFromDemand(plan);
            try self.graphInstantiator().unifyTemplateWithConcrete(plan.dispatcher_ty, dispatcher_info);

            const method = try self.methodName(plan.method);
            const owner = try self.methodOwnerForDispatcherSourceTypeMaybe(dispatcher_info);
            const target = if (owner) |method_owner| try self.lookupMethodTarget(method_owner, method) else null;

            var resolution: ?StaticDispatchResolution = null;
            if (target) |method_target| {
                const target_callable = try self.concreteRefForMethodTargetCallable(method_target);
                try self.graphInstantiator().unifyTemplateWithConcrete(plan.callable_ty, target_callable);
                resolution = .{ .method_target = .{
                    .owner = owner orelse invariantViolation("mono finalized dispatch target had no owner"),
                    .target = method_target,
                } };
            } else switch (plan.result_mode) {
                .value => invariantViolation("mono static dispatch value call had no checked method target"),
                .equality => |equality| {
                    if (!equality.structural_allowed) invariantViolation("mono static dispatch equality had no checked method target and structural equality is not allowed");
                    resolution = .structural_equality;
                },
            }

            const requested_fn_ty = try self.graphInstantiator().concreteRefForTemplateType(plan.callable_ty);
            const finalized = FinalizedStaticDispatch{
                .method = method,
                .dispatcher_ref = dispatcher_info,
                .requested_fn_ty = requested_fn_ty,
                .callable_ty = try self.graphInstantiator().lowerConcreteRef(requested_fn_ty),
                .ret_ty = try self.returnTypeFromConcreteFunction(requested_fn_ty),
                .arg_infos = try self.paramTypesFromConcreteFunction(requested_fn_ty),
                .resolution = resolution orelse invariantViolation("mono static dispatch finalization did not publish a resolution"),
            };
            try self.collectExprSpanConcreteDemand(plan.args, finalized.arg_infos);
            try self.rememberFinalizedStaticDispatch(plan_id, finalized);

            try previous_instantiator.absorbConcreteVariableSubstitutionsFrom(&dispatch_instantiator);
        }

        fn rememberFinalizedStaticDispatch(
            self: *Self,
            plan_id: checked_artifact.StaticDispatchPlanId,
            dispatch: FinalizedStaticDispatch,
        ) Allocator.Error!void {
            if (self.graph.finalized_static_dispatches.contains(plan_id)) {
                invariantViolation("mono graph finalized one static dispatch plan more than once");
            }
            try self.graph.finalized_static_dispatches.put(plan_id, dispatch);
        }

        fn finalizedStaticDispatch(
            self: *Self,
            plan_id: checked_artifact.StaticDispatchPlanId,
        ) FinalizedStaticDispatch {
            return self.graph.finalized_static_dispatches.get(plan_id) orelse {
                invariantViolation("mono body emission reached static dispatch before graph finalization published it");
            };
        }

        fn bindStaticDispatchDemandArgumentTypes(
            self: *Self,
            plan: static_dispatch.StaticDispatchCallPlan,
        ) Allocator.Error!void {
            const param_templates = try self.templateFunctionArgTypes(plan.callable_ty);
            if (param_templates.len != plan.args.len) invariantViolation("mono static dispatch argument count disagreed with checked callable type");

            const expr = self.checkedExpr(plan.expr);
            const dispatcher_from_first_arg = switch (expr.data) {
                .type_dispatch_call => false,
                else => plan.args.len != 0,
            };
            var dispatcher_bound_from_argument = false;
            for (plan.args, param_templates, 0..) |arg, param_template, index| {
                const arg_ref = if (self.demandedLocalSourceRefForExpr(arg)) |known_arg_ref| blk: {
                    try self.graphInstantiator().unifyTemplateWithConcrete(param_template, known_arg_ref);
                    break :blk known_arg_ref;
                } else if (try self.knownDemandSourceRefForExpr(arg)) |known_arg_ref| blk: {
                    try self.graphInstantiator().unifyTemplateWithConcrete(param_template, known_arg_ref);
                    break :blk known_arg_ref;
                } else try self.concreteSourceRefForCheckedPreservingVariables(param_template);

                if (dispatcher_from_first_arg and index == 0) {
                    try self.graphInstantiator().unifyTemplateWithConcrete(plan.dispatcher_ty, arg_ref);
                    dispatcher_bound_from_argument = true;
                }
            }

            switch (expr.data) {
                .type_dispatch_call => {},
                else => if (!dispatcher_bound_from_argument and plan.args.len == 0) {
                    invariantViolation("mono static dispatch plan without source dispatch syntax had no dispatcher argument");
                },
            }
        }

        fn collectExprSpanConcreteDemand(
            self: *Self,
            exprs: []const checked_artifact.CheckedExprId,
            infos: []const ConcreteTypeInfo,
        ) Allocator.Error!void {
            if (exprs.len != infos.len) invariantViolation("mono graph finalization expression span arity disagreed with finalized concrete info span");
            for (exprs, infos) |expr, info| {
                try self.collectExprDemand(expr, info.source_ref);
            }
        }

        fn collectIteratorDispatchDemand(
            self: *Self,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            plan_id: checked_artifact.IteratorForPlanId,
            kind: IteratorDispatchKind,
            obligation: static_dispatch.IteratorDispatchObligation,
        ) Allocator.Error!void {
            var dispatch_instantiator = try self.graphInstantiator().fork();
            defer dispatch_instantiator.deinit();

            const previous_instantiator = self.graphInstantiator();
            self.graph_builder_state.type_instantiator = &dispatch_instantiator;
            defer self.graph_builder_state.type_instantiator = previous_instantiator;

            try self.unifyFunctionReturnWithConcrete(obligation.callable_ty, expected_ref);

            const param_templates = try self.templateFunctionArgTypes(obligation.callable_ty);
            if (param_templates.len != obligation.args.len) invariantViolation("mono iterator dispatch argument count disagreed with checked callable type");

            if (obligation.dispatcher_arg_index >= obligation.args.len) {
                invariantViolation("mono iterator dispatch dispatcher index was outside operand metadata");
            }

            var dispatcher_ref: ?ConcreteSourceType.ConcreteSourceTypeRef = null;
            for (obligation.args, param_templates, 0..) |arg, param_template, index| {
                const arg_ref = switch (arg) {
                    .checked_expr => |expr| blk: {
                        const known_arg_ref = if (self.demandedLocalSourceRefForExpr(expr)) |known|
                            known
                        else if (try self.knownDemandSourceRefForExpr(expr)) |known|
                            known
                        else
                            try self.concreteSourceRefForCheckedPreservingVariables(param_template);
                        try self.graphInstantiator().unifyTemplateWithConcrete(param_template, known_arg_ref);
                        break :blk known_arg_ref;
                    },
                    .loop_iterator_state => invariantViolation("mono iterator dispatch demand needs explicit concrete metadata for compiler-created loop state"),
                };
                if (index == obligation.dispatcher_arg_index) dispatcher_ref = arg_ref;
            }

            const dispatcher = dispatcher_ref orelse invariantViolation("mono iterator dispatch obligation had no dispatcher argument");
            try self.graphInstantiator().unifyTemplateWithConcrete(obligation.dispatcher_ty, dispatcher);
            const finalized = try self.finalizedValueDispatch(
                obligation.method,
                dispatcher,
                obligation.dispatcher_ty,
                obligation.callable_ty,
            );
            try self.collectIteratorDispatchOperandDemands(obligation.args, finalized.arg_infos);
            try self.rememberFinalizedIteratorDispatch(
                .{ .plan = plan_id, .kind = kind },
                finalized,
            );

            try previous_instantiator.absorbConcreteVariableSubstitutionsFrom(&dispatch_instantiator);
        }

        fn collectIteratorDispatchDemandWithConcreteArgs(
            self: *Self,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            plan_id: checked_artifact.IteratorForPlanId,
            kind: IteratorDispatchKind,
            obligation: static_dispatch.IteratorDispatchObligation,
            arg_refs: []const ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            var dispatch_instantiator = try self.graphInstantiator().fork();
            defer dispatch_instantiator.deinit();

            const previous_instantiator = self.graphInstantiator();
            self.graph_builder_state.type_instantiator = &dispatch_instantiator;
            defer self.graph_builder_state.type_instantiator = previous_instantiator;

            try self.unifyFunctionReturnWithConcrete(obligation.callable_ty, expected_ref);

            const param_templates = try self.templateFunctionArgTypes(obligation.callable_ty);
            if (param_templates.len != arg_refs.len) invariantViolation("mono iterator dispatch argument count disagreed with checked callable type");
            if (obligation.args.len != arg_refs.len) invariantViolation("mono iterator dispatch operand count disagreed with concrete argument metadata");

            for (arg_refs, param_templates) |arg_ref, param_template| {
                try self.graphInstantiator().unifyTemplateWithConcrete(param_template, arg_ref);
            }

            if (obligation.dispatcher_arg_index >= arg_refs.len) invariantViolation("mono iterator dispatch dispatcher index was outside argument metadata");
            const dispatcher_ref = arg_refs[obligation.dispatcher_arg_index];
            try self.graphInstantiator().unifyTemplateWithConcrete(obligation.dispatcher_ty, dispatcher_ref);
            const finalized = try self.finalizedValueDispatch(
                obligation.method,
                dispatcher_ref,
                obligation.dispatcher_ty,
                obligation.callable_ty,
            );
            try self.collectIteratorDispatchOperandDemands(obligation.args, finalized.arg_infos);
            try self.rememberFinalizedIteratorDispatch(
                .{ .plan = plan_id, .kind = kind },
                finalized,
            );

            try previous_instantiator.absorbConcreteVariableSubstitutionsFrom(&dispatch_instantiator);
        }

        fn collectIteratorDispatchOperandDemands(
            self: *Self,
            operands: []const static_dispatch.IteratorDispatchOperand,
            infos: []const ConcreteTypeInfo,
        ) Allocator.Error!void {
            if (operands.len != infos.len) invariantViolation("mono iterator dispatch operand count disagreed with finalized concrete info span");
            for (operands, infos) |operand, info| {
                switch (operand) {
                    .checked_expr => |expr| try self.collectExprDemand(expr, info.source_ref),
                    .loop_iterator_state => {},
                }
            }
        }

        fn resolveValueDispatchResolution(
            self: *Self,
            method_id: canonical.MethodNameId,
            dispatcher_ty: checked_artifact.CheckedTypeId,
            callable_ty: checked_artifact.CheckedTypeId,
            dispatcher_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!StaticDispatchResolution {
            try self.graphInstantiator().unifyTemplateWithConcrete(dispatcher_ty, dispatcher_ref);
            const method = try self.methodName(method_id);
            const owner = try self.methodOwnerForDispatcherSourceTypeMaybe(dispatcher_ref);
            const target = if (owner) |method_owner| try self.lookupMethodTarget(method_owner, method) else null;
            if (target) |method_target| {
                const target_callable = try self.concreteRefForMethodTargetCallable(method_target);
                try self.graphInstantiator().unifyTemplateWithConcrete(callable_ty, target_callable);
                return .{ .method_target = .{
                    .owner = owner orelse invariantViolation("mono finalized value dispatch target had no owner"),
                    .target = method_target,
                } };
            }
            invariantViolation("mono value dispatch had no checked method target");
        }

        fn finalizedValueDispatch(
            self: *Self,
            method_id: canonical.MethodNameId,
            dispatcher_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            dispatcher_ty: checked_artifact.CheckedTypeId,
            callable_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!FinalizedStaticDispatch {
            const method = try self.methodName(method_id);
            const resolution = try self.resolveValueDispatchResolution(method_id, dispatcher_ty, callable_ty, dispatcher_ref);
            const requested_fn_ty = try self.graphInstantiator().concreteRefForTemplateType(callable_ty);
            return .{
                .method = method,
                .dispatcher_ref = dispatcher_ref,
                .requested_fn_ty = requested_fn_ty,
                .callable_ty = try self.graphInstantiator().lowerConcreteRef(requested_fn_ty),
                .ret_ty = try self.returnTypeFromConcreteFunction(requested_fn_ty),
                .arg_infos = try self.paramTypesFromConcreteFunction(requested_fn_ty),
                .resolution = resolution,
            };
        }

        fn rememberFinalizedIteratorDispatch(
            self: *Self,
            key: IteratorDispatchKey,
            dispatch: FinalizedStaticDispatch,
        ) Allocator.Error!void {
            if (self.graph.finalized_iterator_dispatches.contains(key)) {
                invariantViolation("mono graph finalized one iterator dispatch obligation more than once");
            }
            try self.graph.finalized_iterator_dispatches.put(key, dispatch);
        }

        fn finalizedIteratorDispatch(
            self: *Self,
            key: IteratorDispatchKey,
        ) FinalizedStaticDispatch {
            return self.graph.finalized_iterator_dispatches.get(key) orelse {
                invariantViolation("mono body emission reached iterator dispatch before graph finalization published it");
            };
        }

        fn staticDispatchDispatcherTypeFromDemand(
            self: *Self,
            plan: static_dispatch.StaticDispatchCallPlan,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const expr = self.checkedExpr(plan.expr);
            switch (expr.data) {
                .dispatch_call,
                .method_eq,
                .type_dispatch_call,
                => {},
                else => if (plan.args.len == 0) {
                    invariantViolation("mono static dispatch plan without source dispatch syntax had no dispatcher argument");
                },
            }
            return try self.concreteSourceRefForCheckedPreservingVariables(plan.dispatcher_ty);
        }

        fn collectForDemand(
            self: *Self,
            plan_id: checked_artifact.IteratorForPlanId,
            pattern: checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        ) Allocator.Error!void {
            const plan = self.iteratorForPlan(plan_id);
            const iterator_ref = try self.concreteSourceRefForCheckedPreservingVariables(plan.iterator_ty);
            const step_ref = try self.concreteSourceRefForCheckedPreservingVariables(plan.step_ty);
            try self.collectIteratorDispatchDemand(iterator_ref, plan_id, .iter, plan.iter);
            const next_arg_refs = [_]ConcreteSourceType.ConcreteSourceTypeRef{iterator_ref};
            try self.collectIteratorDispatchDemandWithConcreteArgs(step_ref, plan_id, .next, plan.next, &next_arg_refs);
            const next_dispatch = self.finalizedIteratorDispatch(.{ .plan = plan_id, .kind = .next });
            try self.publishIteratorStepShape(next_dispatch.ret_ty.source_ref);
            _ = try self.boolConcreteTypeInfo();
            const item_ref = try self.iteratorForItemRefFromStep(next_dispatch.ret_ty.source_ref);
            try self.graphInstantiator().unifyTemplateWithConcrete(plan.item_ty, item_ref);
            try self.recordPatternDemand(pattern, item_ref);
            try self.collectExprDemand(body, try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(body).ty));
        }

        fn publishIteratorStepShape(
            self: *Self,
            step_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const one_label = try self.program.canonical_names.internTagLabel("One");
            const skip_label = try self.program.canonical_names.internTagLabel("Skip");
            const item_label = try self.program.canonical_names.internRecordFieldLabel("item");
            const count_label = try self.program.canonical_names.internRecordFieldLabel("count");
            const rest_label = try self.program.canonical_names.internRecordFieldLabel("rest");

            const one_payload_refs = try self.concreteTagPayloadRefsForUnionType(step_ref, one_label);
            defer if (one_payload_refs.len != 0) self.allocator.free(one_payload_refs);
            if (one_payload_refs.len != 1) invariantViolation("mono iterator-for One step did not have exactly one source payload");
            const one_payload_info = try self.runtimeConcreteTypeInfo(one_payload_refs[0]);
            _ = try self.runtimeConcreteTypeInfo(try self.concreteRecordFieldRef(one_payload_info.source_ref, item_label));
            _ = try self.runtimeConcreteTypeInfo(try self.concreteRecordFieldRef(one_payload_info.source_ref, rest_label));

            const skip_payload_refs = try self.concreteTagPayloadRefsForUnionType(step_ref, skip_label);
            defer if (skip_payload_refs.len != 0) self.allocator.free(skip_payload_refs);
            if (skip_payload_refs.len != 1) invariantViolation("mono iterator-for Skip step did not have exactly one source payload");
            const skip_payload_info = try self.runtimeConcreteTypeInfo(skip_payload_refs[0]);
            _ = try self.runtimeConcreteTypeInfo(try self.concreteRecordFieldRef(skip_payload_info.source_ref, count_label));
            _ = try self.runtimeConcreteTypeInfo(try self.concreteRecordFieldRef(skip_payload_info.source_ref, rest_label));
        }

        fn iteratorForItemRefFromStep(
            self: *Self,
            step_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const one_label = try self.program.canonical_names.internTagLabel("One");
            const payload_refs = try self.concreteTagPayloadRefsForUnionType(step_ref, one_label);
            defer if (payload_refs.len != 0) self.allocator.free(payload_refs);
            if (payload_refs.len != 1) invariantViolation("mono iterator-for One step did not have exactly one source payload");

            const item_label = try self.program.canonical_names.internRecordFieldLabel("item");
            return try self.concreteRecordFieldRef(payload_refs[0], item_label);
        }

        fn recordLocalLookupDemand(
            self: *Self,
            ref_id: checked_artifact.ResolvedValueRefId,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const record = self.resolvedValueRef(ref_id);
            const binder = switch (record.ref) {
                .local_proc => |local| {
                    _ = try self.finalizeLocalProcInstanceForConcrete(local.binder, expected_ref);
                    return;
                },
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                => |local| if (self.local_proc_decls.contains(local.binder)) {
                    _ = try self.finalizeLocalProcInstanceForConcrete(local.binder, expected_ref);
                    return;
                } else local.binder,
                else => return,
            };
            try self.recordConcreteDemandForBinder(binder, expected_ref);
        }

        fn collectResolvedLookupDemand(
            self: *Self,
            ref_id: checked_artifact.ResolvedValueRefId,
            expected_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            const record = self.resolvedValueRef(ref_id);
            switch (record.ref) {
                .top_level_const,
                .imported_const,
                => |const_use| try self.finalizeConstUseForExpected(const_use, expected_ref),
                .platform_required_const => |required| try self.finalizeConstUseForExpected(required.const_use, expected_ref),
                .top_level_proc,
                .imported_proc,
                .hosted_proc,
                .promoted_top_level_proc,
                => |proc_use| {
                    _ = try self.summaryPendingCallableBindingInstanceForProcedureUse(proc_use, expected_ref);
                },
                .platform_required_proc => |required| {
                    _ = try self.summaryPendingCallableBindingInstanceForProcedureUse(required.procedure, expected_ref);
                },
                else => try self.recordLocalLookupDemand(ref_id, expected_ref),
            }
        }

        fn recordPatternDemand(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            try self.recordPatternDemandWithRemaps(pattern_id, source_ref, &.{});
        }

        fn publishFinalizedParamPatternInfos(
            self: *Self,
            patterns: []const checked_artifact.CheckedPatternId,
            infos: []const ConcreteTypeInfo,
        ) Allocator.Error!void {
            if (patterns.len != infos.len) invariantViolation("mono graph finalization parameter pattern count disagreed with finalized parameter infos");
            for (patterns, infos) |pattern, info| {
                try self.publishFinalizedPatternInfoWithRemaps(pattern, info, &.{});
            }
        }

        fn publishFinalizedPatternInfoWithRemaps(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            info: ConcreteTypeInfo,
            binder_remaps: []const checked_artifact.CheckedAlternativeBinderRemap,
        ) Allocator.Error!void {
            const pattern = self.checkedPattern(pattern_id);
            try self.graph.pattern_types.put(self.scopedPattern(pattern_id), info);
            switch (pattern.data) {
                .assign => |binder| {
                    try self.recordConcreteTypeForBinder(self.representativeBinderForCandidate(binder, binder_remaps), info);
                },
                .as => |as| {
                    try self.recordConcreteTypeForBinder(self.representativeBinderForCandidate(as.binder, binder_remaps), info);
                    try self.publishFinalizedPatternInfoWithRemaps(as.pattern, info, binder_remaps);
                },
                .nominal => |nominal| {
                    try self.publishFinalizedPatternInfoWithRemaps(
                        nominal.backing_pattern,
                        try self.concreteNominalBackingInfo(info),
                        binder_remaps,
                    );
                },
                .record_destructure => |destructs| {
                    for (destructs) |destruct| {
                        switch (destruct.kind) {
                            .required, .sub_pattern => |field_pattern| {
                                try self.publishFinalizedPatternInfoWithRemaps(
                                    field_pattern,
                                    try self.concreteRecordFieldInfo(info.source_ref, try self.recordFieldLabel(destruct.label)),
                                    binder_remaps,
                                );
                            },
                            .rest => |rest_pattern| {
                                const rest_info = if (self.graph.local_type_demands.get(self.scopedPatternBinder(rest_pattern))) |demanded_ref|
                                    try self.runtimeConcreteTypeInfo(demanded_ref)
                                else
                                    try self.runtimeConcreteTypeInfo(try self.concreteSourceRefForCheckedPreservingVariables(self.checkedPattern(rest_pattern).ty));
                                try self.publishFinalizedPatternInfoWithRemaps(rest_pattern, rest_info, binder_remaps);
                            },
                        }
                    }
                },
                .tuple => |items| {
                    const item_infos = try self.concreteTupleElementInfos(info.source_ref, items.len);
                    defer if (item_infos.len != 0) self.allocator.free(item_infos);
                    for (items, item_infos) |item, item_info| {
                        try self.publishFinalizedPatternInfoWithRemaps(item, item_info, binder_remaps);
                    }
                },
                .applied_tag => |tag| {
                    const payload_infos = try self.concreteTagPayloadInfosForUnionType(info.source_ref, try self.tagLabel(tag.name));
                    defer if (payload_infos.len != 0) self.allocator.free(payload_infos);
                    if (payload_infos.len != tag.args.len) invariantViolation("mono graph finalization tag parameter pattern payload arity disagreed with finalized type");
                    for (tag.args, payload_infos) |arg, payload_info| {
                        try self.publishFinalizedPatternInfoWithRemaps(arg, payload_info, binder_remaps);
                    }
                },
                .list => |list| {
                    const item_info = try self.listElementTypeFromConcrete(info.source_ref);
                    for (list.patterns) |item| {
                        try self.publishFinalizedPatternInfoWithRemaps(item, item_info, binder_remaps);
                    }
                    if (list.rest) |rest| {
                        if (rest.pattern) |rest_pattern| {
                            try self.publishFinalizedPatternInfoWithRemaps(rest_pattern, info, binder_remaps);
                        }
                    }
                },
                .underscore,
                .num_literal,
                .small_dec_literal,
                .dec_literal,
                .frac_f32_literal,
                .frac_f64_literal,
                .str_literal,
                => {},
                .runtime_error => invariantViolation("mono graph finalization reached runtime_error checked pattern"),
                .pending => invariantViolation("mono graph finalization reached an unresolved checked pattern"),
            }
        }

        fn scopedPatternBinder(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
        ) ScopedBinder {
            const pattern = self.checkedPattern(pattern_id);
            const binder = self.binderForSimplePatternMaybe(pattern.data) orelse {
                invariantViolation("mono graph finalization expected a simple binder pattern");
            };
            return self.scopedBinder(binder);
        }

        fn recordPatternDemandWithRemaps(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            binder_remaps: []const checked_artifact.CheckedAlternativeBinderRemap,
        ) Allocator.Error!void {
            const pattern = self.checkedPattern(pattern_id);
            try self.graphInstantiator().unifyTemplateWithConcrete(pattern.ty, source_ref);
            switch (pattern.data) {
                .assign => |binder| {
                    try self.recordConcreteDemandForBinder(
                        self.representativeBinderForCandidate(binder, binder_remaps),
                        source_ref,
                    );
                    return;
                },
                .as => |as| {
                    try self.recordConcreteDemandForBinder(
                        self.representativeBinderForCandidate(as.binder, binder_remaps),
                        source_ref,
                    );
                    try self.recordPatternDemandWithRemaps(as.pattern, source_ref, binder_remaps);
                    return;
                },
                .underscore => return,
                else => {},
            }
            const pattern_info = try self.runtimeConcreteTypeInfo(source_ref);
            const pattern_ref = pattern_info.source_ref;
            try self.graph.pattern_types.put(self.scopedPattern(pattern_id), pattern_info);
            switch (pattern.data) {
                .nominal => |nominal| try self.recordPatternDemandWithRemaps(
                    nominal.backing_pattern,
                    try self.concreteNominalBackingRef(pattern_ref),
                    binder_remaps,
                ),
                .record_destructure => |destructs| {
                    for (destructs) |destruct| {
                        const child_pattern = switch (destruct.kind) {
                            .required, .sub_pattern => |field_pattern| field_pattern,
                            .rest => |rest_pattern| rest_pattern,
                        };
                        try self.recordPatternDemandWithRemaps(
                            child_pattern,
                            if (destruct.kind == .rest)
                                try self.concreteSourceRefForCheckedPreservingVariables(self.checkedPattern(child_pattern).ty)
                            else
                                try self.concreteRecordFieldRef(pattern_ref, try self.recordFieldLabel(destruct.label)),
                            binder_remaps,
                        );
                    }
                },
                .tuple => |items| {
                    const item_refs = try self.concreteTupleElementRefs(pattern_ref, items.len);
                    defer if (item_refs.len != 0) self.allocator.free(item_refs);
                    for (items, item_refs) |item, item_ref| {
                        try self.recordPatternDemandWithRemaps(item, item_ref, binder_remaps);
                    }
                },
                .applied_tag => |tag| {
                    const payload_refs = try self.concreteTagPayloadRefsForUnionType(pattern_ref, try self.tagLabel(tag.name));
                    defer if (payload_refs.len != 0) self.allocator.free(payload_refs);
                    if (payload_refs.len != tag.args.len) invariantViolation("mono demand collection tag pattern payload arity disagreed with checked pattern");
                    for (tag.args, payload_refs) |arg, payload_ref| {
                        try self.recordPatternDemandWithRemaps(arg, payload_ref, binder_remaps);
                    }
                },
                .list => |list| {
                    const item_ref = try self.listElementSourceRef(pattern_ref);
                    _ = try self.runtimeConcreteTypeInfo(item_ref);
                    for (list.patterns) |item| {
                        try self.recordPatternDemandWithRemaps(item, item_ref, binder_remaps);
                    }
                    if (list.rest) |rest| {
                        if (rest.pattern) |rest_pattern| try self.recordPatternDemandWithRemaps(rest_pattern, pattern_ref, binder_remaps);
                    }
                },
                .underscore,
                .assign,
                .as,
                .num_literal,
                .small_dec_literal,
                .dec_literal,
                .frac_f32_literal,
                .frac_f64_literal,
                .str_literal,
                => {},
                .runtime_error => invariantViolation("mono demand collection reached runtime_error checked pattern"),
                .pending => invariantViolation("mono demand collection reached an unresolved checked pattern"),
            }
        }

        fn checkedStatementCanCompleteNormally(
            self: *const Self,
            statement_id: checked_artifact.CheckedStatementId,
        ) bool {
            const statement = self.checkedStatement(statement_id);
            return switch (statement.data) {
                .decl => |decl| self.checkedExprCanCompleteNormally(decl.expr),
                .var_ => |var_| self.checkedExprCanCompleteNormally(var_.expr),
                .reassign => |reassign| self.checkedExprCanCompleteNormally(reassign.expr),
                .dbg => |expr| self.checkedExprCanCompleteNormally(expr),
                .expr => |expr| self.checkedExprCanCompleteNormally(expr),
                .expect => |expr| self.checkedExprCanCompleteNormally(expr),
                .for_ => |for_| self.checkedExprCanCompleteNormally(for_.expr),
                .while_ => |while_| self.checkedExprCanCompleteNormally(while_.cond),
                .crash,
                .return_,
                .break_,
                .runtime_error,
                => false,
                .import_,
                .alias_decl,
                .nominal_decl,
                .type_anno,
                .type_var_alias,
                => true,
                .pending => invariantViolation("mono body lowering reached pending checked statement while checking completion"),
            };
        }

        fn checkedExprCanCompleteNormally(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) bool {
            const expr = self.checkedExpr(expr_id);
            return switch (expr.data) {
                .runtime_error,
                .crash,
                .return_,
                => false,
                .block => |block| self.checkedBlockCanCompleteNormally(block.statements, block.final_expr),
                .if_ => |if_| self.checkedIfCanCompleteNormally(if_.branches, if_.final_else),
                .match_ => |match_| self.checkedExprCanCompleteNormally(match_.cond) and
                    self.checkedAnyMatchBranchCanCompleteNormally(match_.branches),
                .call => |call| self.checkedExprCanCompleteNormally(call.func) and
                    self.checkedExprSpanCanCompleteNormally(call.args),
                .record => |record| (record.ext == null or self.checkedExprCanCompleteNormally(record.ext.?)) and
                    self.checkedRecordFieldsCanCompleteNormally(record.fields),
                .list => |items| self.checkedExprSpanCanCompleteNormally(items),
                .tuple => |items| self.checkedExprSpanCanCompleteNormally(items),
                .tag => |tag| self.checkedExprSpanCanCompleteNormally(tag.args),
                .str => |segments| self.checkedExprSpanCanCompleteNormally(segments),
                .run_low_level => |run_low_level| self.checkedExprSpanCanCompleteNormally(run_low_level.args),
                .nominal => |nominal| self.checkedExprCanCompleteNormally(nominal.backing_expr),
                .closure => |closure| self.checkedExprCanCompleteNormally(closure.lambda),
                .lambda => |lambda| self.checkedExprCanCompleteNormally(lambda.body),
                .binop => |binop| self.checkedExprCanCompleteNormally(binop.lhs) and
                    self.checkedExprCanCompleteNormally(binop.rhs),
                .unary_minus,
                .unary_not,
                .dbg,
                .expect,
                => |child| self.checkedExprCanCompleteNormally(child),
                .field_access => |access| self.checkedExprCanCompleteNormally(access.receiver),
                .structural_eq => |eq| self.checkedExprCanCompleteNormally(eq.lhs) and
                    self.checkedExprCanCompleteNormally(eq.rhs),
                .tuple_access => |access| self.checkedExprCanCompleteNormally(access.tuple),
                .for_ => |for_| self.checkedExprCanCompleteNormally(for_.expr),
                .hosted_lambda => true,
                .num,
                .frac_f32,
                .frac_f64,
                .dec,
                .dec_small,
                .typed_int,
                .typed_frac,
                .str_segment,
                .bytes_literal,
                .lookup_local,
                .lookup_external,
                .lookup_required,
                .empty_list,
                .empty_record,
                .zero_argument_tag,
                .dispatch_call,
                .method_eq,
                .type_dispatch_call,
                .ellipsis,
                .anno_only,
                => true,
                .pending => invariantViolation("mono body lowering reached pending checked expression while checking completion"),
            };
        }

        fn checkedBlockCanCompleteNormally(
            self: *const Self,
            statements: []const checked_artifact.CheckedStatementId,
            final_expr: checked_artifact.CheckedExprId,
        ) bool {
            for (statements) |statement_id| {
                if (!self.checkedStatementCanCompleteNormally(statement_id)) return false;
            }
            return self.checkedExprCanCompleteNormally(final_expr);
        }

        fn checkedIfCanCompleteNormally(
            self: *const Self,
            branches: []const checked_artifact.CheckedIfBranch,
            final_else: checked_artifact.CheckedExprId,
        ) bool {
            var any_body_completes = false;
            for (branches) |branch| {
                if (!self.checkedExprCanCompleteNormally(branch.cond)) return false;
                if (self.checkedExprCanCompleteNormally(branch.body)) any_body_completes = true;
            }
            return any_body_completes or self.checkedExprCanCompleteNormally(final_else);
        }

        fn checkedAnyMatchBranchCanCompleteNormally(
            self: *const Self,
            branches: []const checked_artifact.CheckedMatchBranch,
        ) bool {
            for (branches) |branch| {
                if (branch.guard) |guard| {
                    if (!self.checkedExprCanCompleteNormally(guard)) continue;
                }
                if (self.checkedExprCanCompleteNormally(branch.value)) return true;
            }
            return false;
        }

        fn checkedExprSpanCanCompleteNormally(
            self: *const Self,
            exprs: []const checked_artifact.CheckedExprId,
        ) bool {
            for (exprs) |expr_id| {
                if (!self.checkedExprCanCompleteNormally(expr_id)) return false;
            }
            return true;
        }

        fn checkedRecordFieldsCanCompleteNormally(
            self: *const Self,
            fields: []const checked_artifact.CheckedRecordExprField,
        ) bool {
            for (fields) |field| {
                if (!self.checkedExprCanCompleteNormally(field.value)) return false;
            }
            return true;
        }

        fn lowerStmtInto(
            self: *Self,
            statement_id: checked_artifact.CheckedStatementId,
            out: *std.ArrayList(Ast.StmtId),
        ) Allocator.Error!void {
            const statement = self.checkedStatement(statement_id);
            if (!checkedStatementIsRuntimeLoweringVisible(statement)) return;
            switch (statement.data) {
                .decl => |decl| {
                    if (self.localProcDeclForStatement(statement) != null) {
                        try out.append(self.allocator, try self.lowerStmt(statement_id));
                        return;
                    }
                    try self.lowerDeclPatternStmtInto(decl.pattern, decl.expr, out);
                },
                .var_ => |var_| try self.lowerVarPatternStmtInto(var_.pattern, var_.expr, out),
                .reassign => |reassign| try self.lowerReassignPatternStmtInto(reassign.pattern, reassign.expr, out),
                else => try out.append(self.allocator, try self.lowerStmt(statement_id)),
            }
        }

        fn checkedStatementIsRuntimeLoweringVisible(statement: checked_artifact.CheckedStatement) bool {
            return switch (statement.data) {
                .import_,
                .alias_decl,
                .nominal_decl,
                .type_anno,
                .type_var_alias,
                => false,

                .pending => invariantViolation("mono body lowering reached pending checked statement while projecting runtime statements"),

                .decl,
                .var_,
                .reassign,
                .dbg,
                .expr,
                .expect,
                .crash,
                .return_,
                .break_,
                .for_,
                .while_,
                .runtime_error,
                => true,
            };
        }

        fn lowerDeclPatternStmtInto(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            expr_id: checked_artifact.CheckedExprId,
            out: *std.ArrayList(Ast.StmtId),
        ) Allocator.Error!void {
            const pattern = self.checkedPattern(pattern_id);
            const source_info = self.concreteTypeForPatternBinder(pattern_id) orelse
                self.finalizedPatternTypeInfo(pattern_id);

            if (self.patternCanLowerAsSingleDeclaration(pattern.data)) {
                const bind = try self.lowerParamPatternWithType(pattern_id, source_info);
                const body = try self.lowerExprConcreteExpected(expr_id, source_info);
                try out.append(self.allocator, try self.program.ast.addStmt(.{ .decl = .{
                    .bind = bind,
                    .body = body,
                } }));
                return;
            }

            const body = try self.lowerExprConcreteExpected(expr_id, source_info);
            const source_symbol = try self.program.addSyntheticSymbol();
            try out.append(self.allocator, try self.program.ast.addStmt(.{ .decl = .{
                .bind = .{
                    .ty = source_info.ty,
                    .source_ty = source_info.source_ty,
                    .source_ty_payload = source_info.source_ref,
                    .symbol = source_symbol,
                },
                .body = body,
            } }));

            const source_expr = try self.program.ast.addExprWithSourcePayload(source_info.ty, source_info.source_ty, source_info.source_ref, .{ .var_ = source_symbol });
            try self.appendPatternActions(out, pattern_id, source_info, source_expr, .declaration);
        }

        fn lowerVarPatternStmtInto(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            expr_id: checked_artifact.CheckedExprId,
            out: *std.ArrayList(Ast.StmtId),
        ) Allocator.Error!void {
            const source_info = self.concreteTypeForPatternBinder(pattern_id) orelse
                self.finalizedPatternTypeInfo(pattern_id);
            const bind = try self.lowerParamPatternWithType(pattern_id, source_info);
            const body = try self.lowerExprConcreteExpected(expr_id, source_info);
            try out.append(self.allocator, try self.program.ast.addStmt(.{ .var_decl = .{
                .bind = bind,
                .body = body,
            } }));
        }

        fn patternCanLowerAsSingleDeclaration(
            _: *const Self,
            data: checked_artifact.CheckedPatternData,
        ) bool {
            return switch (data) {
                .assign,
                .underscore,
                => true,
                else => false,
            };
        }

        fn lowerReassignPatternStmtInto(
            self: *Self,
            pattern_id: checked_artifact.CheckedPatternId,
            expr_id: checked_artifact.CheckedExprId,
            out: *std.ArrayList(Ast.StmtId),
        ) Allocator.Error!void {
            const source_info = self.concreteTypeForPatternBinder(pattern_id) orelse
                self.finalizedPatternTypeInfo(pattern_id);
            const body = try self.lowerExprConcreteExpected(expr_id, source_info);

            const source_symbol = try self.program.addSyntheticSymbol();
            try out.append(self.allocator, try self.program.ast.addStmt(.{ .decl = .{
                .bind = .{
                    .ty = source_info.ty,
                    .source_ty = source_info.source_ty,
                    .source_ty_payload = source_info.source_ref,
                    .symbol = source_symbol,
                },
                .body = body,
            } }));

            const source_expr = try self.program.ast.addExprWithSourcePayload(source_info.ty, source_info.source_ty, source_info.source_ref, .{ .var_ = source_symbol });
            try self.appendPatternActions(out, pattern_id, source_info, source_expr, .reassignment);
        }

        fn appendPatternActions(
            self: *Self,
            out: *std.ArrayList(Ast.StmtId),
            pattern_id: checked_artifact.CheckedPatternId,
            source_info: ConcreteTypeInfo,
            source_expr: Ast.ExprId,
            action: PatternBinderAction,
        ) Allocator.Error!void {
            const pattern = self.checkedPattern(pattern_id);
            switch (pattern.data) {
                .assign => |binder| try self.appendPatternBinderAction(out, binder, source_info, source_expr, action),
                .as => |as| {
                    try self.appendPatternBinderAction(out, as.binder, source_info, source_expr, action);
                    try self.appendPatternActions(out, as.pattern, source_info, source_expr, action);
                },
                .nominal => |nominal| {
                    const backing_info = try self.concreteNominalBackingInfo(source_info);
                    const backing_expr = try self.program.ast.addExprWithSourcePayload(backing_info.ty, backing_info.source_ty, backing_info.source_ref, .{
                        .nominal_reinterpret = source_expr,
                    });
                    try self.appendPatternActions(out, nominal.backing_pattern, backing_info, backing_expr, action);
                },
                .record_destructure => |destructs| {
                    for (destructs) |destruct| {
                        const child_pattern = switch (destruct.kind) {
                            .required, .sub_pattern => |field_pattern| field_pattern,
                            .rest => invariantViolation("mono body lowering requires published decision-plan metadata for record-rest declaration/reassignment patterns"),
                        };
                        const label = try self.recordFieldLabel(destruct.label);
                        const field_info = try self.concreteRecordFieldInfo(source_info.source_ref, label);
                        const field_expr = try self.program.ast.addExprWithSourcePayload(field_info.ty, field_info.source_ty, field_info.source_ref, .{ .access = .{
                            .record = source_expr,
                            .field = label,
                            .field_index = self.recordFieldIndex(source_info.ty, label),
                        } });
                        try self.appendPatternActions(out, child_pattern, field_info, field_expr, action);
                    }
                },
                .tuple => |items| {
                    const item_infos = try self.concreteTupleElementInfos(source_info.source_ref, items.len);
                    defer if (item_infos.len != 0) self.allocator.free(item_infos);
                    for (items, item_infos, 0..) |item_pattern, item_info, i| {
                        const item_expr = try self.program.ast.addExprWithSourcePayload(item_info.ty, item_info.source_ty, item_info.source_ref, .{ .tuple_access = .{
                            .tuple = source_expr,
                            .elem_index = @intCast(i),
                        } });
                        try self.appendPatternActions(out, item_pattern, item_info, item_expr, action);
                    }
                },
                .applied_tag => |tag| {
                    const tag_name = try self.tagLabel(tag.name);
                    if (!self.tagPatternIsIrrefutable(source_info.ty, tag_name)) {
                        invariantViolation("mono body lowering requires published decision-plan metadata for refutable tag declaration/reassignment patterns");
                    }
                    const payload_infos = try self.concreteTagPayloadInfosForUnionType(source_info.source_ref, tag_name);
                    defer if (payload_infos.len != 0) self.allocator.free(payload_infos);
                    if (payload_infos.len != tag.args.len) {
                        invariantViolation("mono body lowering tag declaration/reassignment payload arity did not match resolved source type");
                    }
                    const tag_info = self.tagInfoForUnionType(source_info.ty, tag_name);
                    for (tag.args, payload_infos, 0..) |payload_pattern, payload_info, i| {
                        const payload_expr = try self.program.ast.addExprWithSourcePayload(payload_info.ty, payload_info.source_ty, payload_info.source_ref, .{ .tag_payload = .{
                            .tag_union = source_expr,
                            .tag_name = tag_name,
                            .tag_discriminant = tag_info.discriminant,
                            .payload_index = @intCast(i),
                        } });
                        try self.appendPatternActions(out, payload_pattern, payload_info, payload_expr, action);
                    }
                },
                .underscore => {},
                .list,
                .num_literal,
                .small_dec_literal,
                .dec_literal,
                .frac_f32_literal,
                .frac_f64_literal,
                .str_literal,
                => invariantViolation("mono body lowering requires published decision-plan metadata for refutable declaration/reassignment patterns"),
                .runtime_error => invariantViolation("mono body lowering reached runtime_error declaration/reassignment pattern"),
                .pending => invariantViolation("mono body lowering reached an unresolved declaration/reassignment pattern"),
            }
        }

        fn appendPatternBinderAction(
            self: *Self,
            out: *std.ArrayList(Ast.StmtId),
            binder: checked_artifact.PatternBinderId,
            source_info: ConcreteTypeInfo,
            source_expr: Ast.ExprId,
            action: PatternBinderAction,
        ) Allocator.Error!void {
            return switch (action) {
                .declaration => {
                    try self.recordConcreteTypeForBinder(binder, source_info);
                    const symbol = try self.symbolForBinder(binder);
                    try out.append(self.allocator, try self.program.ast.addStmt(.{ .decl = .{
                        .bind = .{
                            .ty = source_info.ty,
                            .source_ty = source_info.source_ty,
                            .source_ty_payload = source_info.source_ref,
                            .symbol = symbol,
                        },
                        .body = source_expr,
                    } }));
                },
                .reassignment => {
                    const existing_symbol = self.local_symbols.get(binder);
                    try self.recordConcreteTypeForBinder(binder, source_info);
                    const symbol = existing_symbol orelse try self.symbolForBinder(binder);
                    const stmt = if (existing_symbol != null)
                        Ast.Stmt{ .reassign = .{
                            .target = symbol,
                            .body = source_expr,
                        } }
                    else
                        Ast.Stmt{ .decl = .{
                            .bind = .{
                                .ty = source_info.ty,
                                .source_ty = source_info.source_ty,
                                .source_ty_payload = source_info.source_ref,
                                .symbol = symbol,
                            },
                            .body = source_expr,
                        } };
                    try out.append(self.allocator, try self.program.ast.addStmt(stmt));
                },
            };
        }

        fn tagPatternIsIrrefutable(
            self: *Self,
            ty: Type.TypeId,
            tag_name: canonical.TagLabelId,
        ) bool {
            return switch (self.program.types.getType(ty)) {
                .tag_union => |tag_union| tag_union.tags.len == 1 and tag_union.tags[0].name == tag_name,
                else => false,
            };
        }

        fn lowerRecord(
            self: *Self,
            ty: Type.TypeId,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            record: anytype,
        ) Allocator.Error!Ast.ExprId {
            if (record.ext) |ext| return try self.lowerRecordUpdate(ty, ext, record.fields);
            if (record.fields.len == 0) return try self.program.ast.addExpr(ty, .{ .record = Ast.Span(Ast.FieldExpr).empty() });
            const fields = try self.allocator.alloc(Ast.FieldExpr, record.fields.len);
            defer self.allocator.free(fields);
            for (record.fields, 0..) |field, i| {
                const label = try self.recordFieldLabel(field.label);
                const field_info = try self.concreteRecordFieldInfo(source_ref, label);
                fields[i] = .{
                    .field = label,
                    .value = try self.lowerExprConcreteExpected(field.value, field_info),
                };
            }
            return try self.program.ast.addExpr(ty, .{ .record = try self.program.ast.addFieldExprSpan(fields) });
        }

        fn concreteRecordFieldInfo(
            self: *Self,
            record_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            field_name: canonical.RecordFieldLabelId,
        ) Allocator.Error!ConcreteTypeInfo {
            const field_ref = try self.concreteRecordFieldRef(record_ref, field_name);
            return try self.runtimeConcreteTypeInfo(field_ref);
        }

        fn concreteRecordFieldRef(
            self: *Self,
            record_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            field_name: canonical.RecordFieldLabelId,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            const requested_key = ConcreteSourceChildKey{ .parent = record_ref, .kind = .{
                .tag = .record_field,
                .a = @intFromEnum(field_name),
            } };
            if (self.graph.concrete_source_children.get(requested_key)) |field_ref| return field_ref;
            if (mode == .body_emitter) {
                invariantViolation("mono body emission reached record field child before graph finalization published it");
            } else {
                var current = record_ref;
                while (true) {
                    switch (self.concretePayload(current)) {
                        .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                        .nominal => current = try self.concreteNominalBackingRef(current),
                        .record_unbound => |fields| {
                            const field_ref = try self.concreteRecordFieldRefInFields(current, fields, field_name);
                            try self.graph.concrete_source_children.put(requested_key, field_ref);
                            return field_ref;
                        },
                        .record => |record| {
                            if (try self.concreteRecordFieldRefInFieldsMaybe(current, record.fields, field_name)) |field_ref| {
                                try self.graph.concrete_source_children.put(requested_key, field_ref);
                                return field_ref;
                            }
                            current = try self.concreteSourceChildRef(current, .{ .tag = .record_ext }, record.ext);
                        },
                        else => invariantViolation("mono body lowering expected concrete record type"),
                    }
                }
            }
        }

        fn concreteRecordFieldRefInFields(
            self: *Self,
            owner: ConcreteSourceType.ConcreteSourceTypeRef,
            fields: []const checked_artifact.CheckedRecordField,
            field_name: canonical.RecordFieldLabelId,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            return (try self.concreteRecordFieldRefInFieldsMaybe(owner, fields, field_name)) orelse
                invariantViolation("mono body lowering could not find concrete record field");
        }

        fn concreteRecordFieldRefInFieldsMaybe(
            self: *Self,
            owner: ConcreteSourceType.ConcreteSourceTypeRef,
            fields: []const checked_artifact.CheckedRecordField,
            field_name: canonical.RecordFieldLabelId,
        ) Allocator.Error!?ConcreteSourceType.ConcreteSourceTypeRef {
            for (fields) |field| {
                if (try self.recordFieldNameForConcreteRef(owner, field.name) != field_name) continue;
                return try self.concreteSourceChildRef(owner, .{
                    .tag = .record_field,
                    .a = @intFromEnum(field_name),
                }, field.ty);
            }
            return null;
        }

        fn lowerRecordUpdate(
            self: *Self,
            ty: Type.TypeId,
            ext: checked_artifact.CheckedExprId,
            update_fields: []const checked_artifact.CheckedRecordExprField,
        ) Allocator.Error!Ast.ExprId {
            const ext_info = try self.concreteResultTypeForExpr(ext);
            const ext_ty = ext_info.ty;
            const ext_source_ty = ext_info.source_ty;
            const ext_expr = try self.lowerExpr(ext);
            const ext_symbol = try self.program.symbols.add(base.Ident.Idx.NONE, .synthetic);
            const ext_var = try self.program.ast.addExprWithSourcePayload(
                ext_ty,
                ext_source_ty,
                ext_info.source_ref,
                .{ .var_ = ext_symbol },
            );

            const record_ty = switch (self.program.types.getType(ty)) {
                .record => |record| record,
                else => invariantViolation("mono body lowering record update expected record result type"),
            };

            const fields = try self.allocator.alloc(Ast.FieldExpr, record_ty.fields.len);
            defer self.allocator.free(fields);
            var field_count: usize = 0;

            for (update_fields) |field| {
                const label = try self.recordFieldLabel(field.label);
                if (try self.recordUpdateHasRemappedField(update_fields[0..field_count], label)) {
                    invariantViolation("mono body lowering record update contained duplicate field labels");
                }
                _ = self.recordFieldIndex(ty, label);
                if (field_count >= fields.len) invariantViolation("mono body lowering record update had more fields than its result type");
                fields[field_count] = .{
                    .field = label,
                    .value = try self.lowerExpr(field.value),
                };
                field_count += 1;
            }

            for (record_ty.fields) |field| {
                if (try self.recordUpdateHasRemappedField(update_fields, field.name)) continue;
                if (field_count >= fields.len) invariantViolation("mono body lowering record update had more fields than its result type");
                fields[field_count] = .{
                    .field = field.name,
                    .value = try self.program.ast.addExpr(field.ty, .{ .access = .{
                        .record = ext_var,
                        .field = field.name,
                        .field_index = self.recordFieldIndex(ext_ty, field.name),
                    } }),
                };
                field_count += 1;
            }

            if (field_count != fields.len) invariantViolation("mono body lowering record update did not produce every result field exactly once");

            const rest = try self.program.ast.addExpr(ty, .{
                .record = try self.program.ast.addFieldExprSpan(fields),
            });
            return try self.program.ast.addExpr(ty, .{ .let_ = .{
                .def = .{ .let_val = .{
                    .bind = .{
                        .ty = ext_ty,
                        .source_ty = ext_source_ty,
                        .source_ty_payload = ext_info.source_ref,
                        .symbol = ext_symbol,
                    },
                    .body = ext_expr,
                } },
                .rest = rest,
            } });
        }

        fn lowerClosureExpr(
            self: *Self,
            site_expr: checked_artifact.CheckedExprId,
            arg_patterns: []const checked_artifact.CheckedPatternId,
            body_expr: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const body_instance = self.graph.closure_instances.get(self.scopedExpr(site_expr)) orelse {
                invariantViolation("mono body emission reached closure before graph finalization published its body instance");
            };
            const instance = self.graph.body_instances.get(body_instance) orelse {
                invariantViolation("mono body emission reached closure with missing finalized body instance");
            };
            const closure = switch (instance.kind) {
                .closure_value => |closure| closure,
                .root => invariantViolation("mono body emission attempted to emit root body as closure"),
                .local_proc => invariantViolation("mono body emission attempted to emit local procedure body as closure"),
            };
            if (closure.expr != site_expr or closure.body != body_expr or closure.args.len != arg_patterns.len) {
                invariantViolation("mono body emission closure expression disagreed with finalized body instance");
            }

            const previous_body = self.current_body;
            self.current_body = body_instance;
            defer self.current_body = previous_body;

            const params = try self.lowerParamBundleFromFunction(arg_patterns, instance.source_fn_ty_payload);
            defer self.deinitParamBundle(params);
            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            self.current_return_type = instance.ret_ty;
            self.current_return_source_ref = instance.ret_ty.source_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;
            const body = try self.lowerBodyWithParamSetup(body_expr, instance.ret_ty, params);
            return try self.program.ast.addExprWithSourcePayload(instance.fn_ty, instance.source_fn_ty, instance.source_fn_ty_payload, .{ .clos = .{
                .site = closure.site,
                .source_fn_ty = instance.source_fn_ty,
                .source_fn_ty_payload = instance.source_fn_ty_payload,
                .args = params.args,
                .body = body,
            } });
        }

        fn lowerCheckedClosureExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
            closure: anytype,
        ) Allocator.Error!Ast.ExprId {
            const lambda_expr = self.checkedExpr(closure.lambda);
            return switch (lambda_expr.data) {
                .lambda => |lambda| try self.lowerClosureExpr(expr_id, lambda.args, lambda.body),
                else => invariantViolation("mono body lowering expected closure expression to reference a checked lambda"),
            };
        }

        fn runtimeCallInstantiation(
            self: *Self,
            instantiated: CallInstantiationInfo,
        ) Allocator.Error!CallInstantiationInfo {
            const source_arg_refs = try self.sourceParamRefsFromFunction(instantiated.concrete_fn);
            defer if (source_arg_refs.len != 0) self.allocator.free(source_arg_refs);
            const runtime_fn = try self.graphInstantiator().runtimeConcreteRef(instantiated.concrete_fn);
            const arg_infos = try self.paramTypesFromConcreteFunction(runtime_fn);
            errdefer if (arg_infos.len != 0) self.allocator.free(arg_infos);
            if (arg_infos.len != source_arg_refs.len) {
                invariantViolation("mono call finalization runtime function parameter count disagreed with source function");
            }
            for (arg_infos, source_arg_refs) |*arg_info, source_ref| {
                if ((try self.concreteFunctionRef(source_ref)) != null) {
                    arg_info.source_ty = self.program.concrete_source_types.key(source_ref);
                    arg_info.source_ref = source_ref;
                }
            }
            return .{
                .concrete_fn = runtime_fn,
                .func_ty = try self.graphInstantiator().lowerConcreteRef(runtime_fn),
                .requested_source_fn_ty = self.program.concrete_source_types.key(runtime_fn),
                .ret_ty = try self.returnTypeFromConcreteFunction(runtime_fn),
                .arg_infos = arg_infos,
            };
        }

        fn lowerCall(
            self: *Self,
            expected: ConcreteTypeInfo,
            call_expr: checked_artifact.CheckedExprId,
            call: anytype,
        ) Allocator.Error!Ast.ExprId {
            const finalized = self.finalizedCall(call_expr);
            if (!self.program.types.equalIds(finalized.ret_ty.ty, expected.ty)) {
                invariantViolation("mono body emission call result type disagreed with concrete expected type");
            }
            if (self.procedureUseForExpr(call.func)) |proc_use| {
                const args = try self.lowerExprSpanConcrete(call.args, finalized.arg_infos);
                if (try self.summaryPendingLocalRootForProcedureUse(proc_use, finalized.concrete_fn)) |root| {
                    return try self.lowerPendingLocalRootCall(expected, root, args);
                }
                if (try self.summaryPendingCallableBindingInstanceForProcedureUse(proc_use, finalized.concrete_fn)) |request| {
                    return try self.lowerPendingCallableInstanceCall(expected, request, args);
                }
                const proc = try self.reserveProcedureUseForConcrete(proc_use, finalized.concrete_fn, .{ .call_proc = call_expr });
                return try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .call_proc = .{
                    .proc = proc,
                    .args = args,
                    .requested_fn_ty = finalized.func_ty,
                    .requested_source_fn_ty = finalized.requested_source_fn_ty,
                    .requested_source_fn_ty_payload = finalized.concrete_fn,
                } });
            }
            if (try self.localProcUseForExpr(call.func)) |local_proc| {
                const args = try self.lowerExprSpanConcrete(call.args, finalized.arg_infos);
                const instance = try self.ensureLocalProcInstanceForConcrete(local_proc.binder, finalized.concrete_fn);
                const func = try self.program.ast.addExprWithSourcePayload(
                    finalized.func_ty,
                    instance.source_fn_ty,
                    instance.source_fn_ty_payload,
                    .{ .var_ = instance.symbol },
                );
                return try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .call_value = .{
                    .func = func,
                    .args = args,
                    .requested_fn_ty = finalized.func_ty,
                    .requested_source_fn_ty = finalized.requested_source_fn_ty,
                    .requested_source_fn_ty_payload = finalized.concrete_fn,
                } });
            }

            const func_info = ConcreteTypeInfo{
                .ty = finalized.func_ty,
                .source_ty = finalized.requested_source_fn_ty,
                .source_ref = finalized.concrete_fn,
            };
            const func = try self.lowerExprConcreteExpected(call.func, func_info);
            const args = try self.lowerExprSpanConcrete(call.args, finalized.arg_infos);

            return try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .call_value = .{
                .func = func,
                .args = args,
                .requested_fn_ty = finalized.func_ty,
                .requested_source_fn_ty = finalized.requested_source_fn_ty,
                .requested_source_fn_ty_payload = finalized.concrete_fn,
            } });
        }

        fn bindKnownCallArgumentDemandTypes(
            self: *Self,
            source_fn_ty: checked_artifact.CheckedTypeId,
            args: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!void {
            const param_templates = try self.templateFunctionArgTypes(source_fn_ty);
            if (param_templates.len != args.len) invariantViolation("mono demand collection call argument count disagreed with checked function type");
            for (args, param_templates) |arg, param_template| {
                const arg_ref = (try self.knownDemandSourceRefForExpr(arg)) orelse continue;
                try self.graphInstantiator().unifyTemplateWithConcrete(param_template, arg_ref);
            }
        }

        fn bindKnownCallCalleeType(
            self: *Self,
            source_fn_ty: checked_artifact.CheckedTypeId,
            func: checked_artifact.CheckedExprId,
        ) Allocator.Error!void {
            if (!self.checkedTypeIsFunction(source_fn_ty)) return;
            const callee_ty = (try self.knownConcreteFunctionTypeForExpr(func)) orelse return;
            try self.graphInstantiator().unifyTemplateWithConcrete(source_fn_ty, callee_ty.source_ref);
        }

        fn checkedTypeIsFunction(
            self: *Self,
            checked_ty: checked_artifact.CheckedTypeId,
        ) bool {
            var current = checked_ty;
            while (true) {
                switch (self.graphInstantiator().templatePayload(current)) {
                    .alias => |alias| current = alias.backing,
                    .function => return true,
                    else => return false,
                }
            }
        }

        fn templateFunctionArgTypes(
            self: *Self,
            source_fn_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error![]const checked_artifact.CheckedTypeId {
            var current = source_fn_ty;
            while (true) {
                switch (self.graphInstantiator().templatePayload(current)) {
                    .alias => |alias| current = alias.backing,
                    .function => |func| return func.args,
                    else => invariantViolation("mono body lowering expected checked call source type to be a function"),
                }
            }
        }

        fn knownConcreteResultTypeForExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?ConcreteTypeInfo {
            if (try self.publishedConcreteResultTypeForExpr(expr_id)) |published| return published;
            if (try self.exprPublishesMonoConcreteType(expr_id)) {
                if (mode == .graph_builder) {
                    const source_ref = try self.concreteSourceRefForCheckedPreservingVariables(self.checkedExpr(expr_id).ty);
                    return try self.runtimeConcreteTypeInfo(source_ref);
                }
                return try self.concreteResultTypeForExpr(expr_id);
            }
            return null;
        }

        fn publishedConcreteResultTypeForExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?ConcreteTypeInfo {
            const expr = self.checkedExpr(expr_id);
            if (self.concreteTypeForLookupExpr(expr_id)) |lookup_ty| return lookup_ty;
            if (try self.concreteTypeForConstLookupExpr(expr_id)) |const_ty| return const_ty;
            return switch (expr.data) {
                .field_access => |access| blk: {
                    const receiver = (try self.publishedConcreteResultTypeForExpr(access.receiver)) orelse break :blk null;
                    break :blk try self.concreteRecordFieldInfo(receiver.source_ref, try self.recordFieldLabel(access.field_name));
                },
                else => null,
            };
        }

        fn knownDemandSourceRefForExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?ConcreteSourceType.ConcreteSourceTypeRef {
            if (self.graph.expr_type_demands.get(self.scopedExpr(expr_id))) |source_ref| return source_ref;
            if (self.demandedLocalSourceRefForExpr(expr_id)) |source_ref| return source_ref;
            if (self.concreteTypeForLookupExpr(expr_id)) |lookup_ty| return lookup_ty.source_ref;
            if (try self.concreteTypeForConstLookupExpr(expr_id)) |const_ty| return const_ty.source_ref;

            const expr = self.checkedExpr(expr_id);
            return switch (expr.data) {
                .field_access => |access| blk: {
                    const receiver_ref = (try self.knownDemandSourceRefForExpr(access.receiver)) orelse break :blk null;
                    break :blk try self.concreteRecordFieldRef(receiver_ref, try self.recordFieldLabel(access.field_name));
                },
                else => if (try self.exprPublishesMonoConcreteType(expr_id)) blk: {
                    if (mode == .graph_builder) {
                        break :blk (try self.runtimeConcreteTypeInfo(
                            try self.concreteSourceRefForCheckedPreservingVariables(expr.ty),
                        )).source_ref;
                    }
                    break :blk (try self.concreteResultTypeForExpr(expr_id)).source_ref;
                } else null,
            };
        }

        fn knownConcreteFunctionTypeForExpr(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?ConcreteTypeInfo {
            const known = (try self.publishedConcreteResultTypeForExpr(expr_id)) orelse return null;
            const function_ref = try self.concreteFunctionRef(known.source_ref) orelse return null;
            return try self.runtimeConcreteTypeInfo(function_ref);
        }

        fn exprPublishesMonoConcreteType(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!bool {
            if (try self.exprPublishesClosedConcreteType(expr_id)) {
                return true;
            }
            const expr = self.checkedExpr(expr_id);
            return switch (expr.data) {
                .num => |num| switch (num.kind) {
                    .num_unbound,
                    .int_unbound,
                    => try self.checkedTypeIsMonoDefaultableNumeric(expr.ty),
                    else => false,
                },
                .list => |items| items.len != 0 and try self.exprSpanPublishesMonoConcreteTypes(items),
                .tuple => |items| try self.exprSpanPublishesMonoConcreteTypes(items),
                .record => |record| {
                    for (record.fields) |field| {
                        if (!try self.exprPublishesMonoConcreteType(field.value)) return false;
                    }
                    if (record.ext) |ext| return try self.exprPublishesMonoConcreteType(ext);
                    return true;
                },
                .tag => |tag| try self.exprSpanPublishesMonoConcreteTypes(tag.args),
                .nominal => |nominal| try self.exprPublishesMonoConcreteType(nominal.backing_expr),
                else => false,
            };
        }

        fn exprSpanPublishesMonoConcreteTypes(
            self: *Self,
            exprs: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!bool {
            for (exprs) |expr| {
                if (!try self.exprPublishesMonoConcreteType(expr)) return false;
            }
            return true;
        }

        fn checkedTypeIsMonoDefaultableNumeric(
            self: *Self,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!bool {
            var current = checked_ty;
            while (true) {
                switch (self.graphInstantiator().templatePayload(current)) {
                    .alias => |alias| current = alias.backing,
                    .flex => |flex| return self.graphInstantiator().isMonoSpecializationNumericFlex(flex),
                    .rigid => |rigid| return self.graphInstantiator().isMonoSpecializationNumericFlex(rigid),
                    else => return false,
                }
            }
        }

        fn exprPublishesClosedConcreteType(
            self: *Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!bool {
            const expr = self.checkedExpr(expr_id);
            if (!try self.checkedTypeIsClosedConcrete(expr.ty)) return false;
            return switch (expr.data) {
                .typed_int,
                .typed_frac,
                .str_segment,
                .bytes_literal,
                .empty_record,
                .zero_argument_tag,
                => true,
                .num => |num| switch (num.kind) {
                    .num_unbound,
                    .int_unbound,
                    => false,
                    else => true,
                },
                .frac_f32 => |frac| frac.has_suffix,
                .frac_f64 => |frac| frac.has_suffix,
                .dec => |dec| dec.has_suffix,
                .dec_small => |dec| dec.has_suffix,
                .str => |segments| try self.exprSpanPublishesClosedConcreteTypes(segments),
                .list => |items| items.len != 0 and try self.exprSpanPublishesClosedConcreteTypes(items),
                .tuple => |items| try self.exprSpanPublishesClosedConcreteTypes(items),
                .record => |record| {
                    for (record.fields) |field| {
                        if (!try self.exprPublishesClosedConcreteType(field.value)) return false;
                    }
                    if (record.ext) |ext| return try self.exprPublishesClosedConcreteType(ext);
                    return true;
                },
                .tag => |tag| try self.exprSpanPublishesClosedConcreteTypes(tag.args),
                .nominal => |nominal| try self.exprPublishesClosedConcreteType(nominal.backing_expr),
                else => false,
            };
        }

        fn exprSpanPublishesClosedConcreteTypes(
            self: *Self,
            exprs: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!bool {
            for (exprs) |expr| {
                if (!try self.exprPublishesClosedConcreteType(expr)) return false;
            }
            return true;
        }

        fn checkedTypeIsClosedConcrete(
            self: *Self,
            checked_ty: checked_artifact.CheckedTypeId,
        ) Allocator.Error!bool {
            var active = std.AutoHashMap(checked_artifact.CheckedTypeId, void).init(self.allocator);
            defer active.deinit();
            return try self.checkedTypeIsClosedConcreteInner(checked_ty, &active);
        }

        fn checkedTypeIsClosedConcreteInner(
            self: *Self,
            checked_ty: checked_artifact.CheckedTypeId,
            active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        ) Allocator.Error!bool {
            if (active.contains(checked_ty)) return true;
            try active.put(checked_ty, {});
            defer _ = active.remove(checked_ty);

            return switch (self.graphInstantiator().templatePayload(checked_ty)) {
                .pending,
                .flex,
                .rigid,
                => false,
                .empty_record,
                .empty_tag_union,
                => true,
                .alias => |alias| try self.checkedTypeIsClosedConcreteInner(alias.backing, active),
                .record => |record| (try self.checkedTypeSpanIsClosedConcrete(record.fields, active)) and
                    try self.checkedTypeIsClosedConcreteInner(record.ext, active),
                .record_unbound => false,
                .tuple => |items| try self.checkedTypeIdSpanIsClosedConcrete(items, active),
                .nominal => |nominal| try self.checkedTypeIdSpanIsClosedConcrete(nominal.args, active),
                .function => |function| !function.needs_instantiation and
                    (try self.checkedTypeIdSpanIsClosedConcrete(function.args, active)) and
                    try self.checkedTypeIsClosedConcreteInner(function.ret, active),
                .tag_union => |tag_union| (try self.checkedTagsAreClosedConcrete(tag_union.tags, active)) and
                    try self.checkedTypeIsClosedConcreteInner(tag_union.ext, active),
            };
        }

        fn checkedTypeSpanIsClosedConcrete(
            self: *Self,
            fields: []const checked_artifact.CheckedRecordField,
            active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        ) Allocator.Error!bool {
            for (fields) |field| {
                if (!try self.checkedTypeIsClosedConcreteInner(field.ty, active)) return false;
            }
            return true;
        }

        fn checkedTypeIdSpanIsClosedConcrete(
            self: *Self,
            items: []const checked_artifact.CheckedTypeId,
            active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        ) Allocator.Error!bool {
            for (items) |item| {
                if (!try self.checkedTypeIsClosedConcreteInner(item, active)) return false;
            }
            return true;
        }

        fn checkedTagsAreClosedConcrete(
            self: *Self,
            tags: []const checked_artifact.CheckedTag,
            active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
        ) Allocator.Error!bool {
            for (tags) |tag| {
                if (!try self.checkedTypeIdSpanIsClosedConcrete(tag.args, active)) return false;
            }
            return true;
        }

        fn unifyFunctionReturnWithConcrete(
            self: *Self,
            fn_ty: checked_artifact.CheckedTypeId,
            expected_ret: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!void {
            var current = fn_ty;
            while (true) {
                switch (self.graphInstantiator().templatePayload(current)) {
                    .alias => |alias| current = alias.backing,
                    .function => |func| {
                        try self.graphInstantiator().unifyTemplateWithConcrete(func.ret, expected_ret);
                        return;
                    },
                    else => invariantViolation("mono body lowering expected callable source type to be a function"),
                }
            }
        }

        fn lowerStructuralEq(
            self: *Self,
            ty: Type.TypeId,
            eq: anytype,
        ) Allocator.Error!Ast.ExprId {
            const lhs = try self.lowerExpr(eq.lhs);
            const rhs = try self.lowerExpr(eq.rhs);
            const structural = try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
            if (!eq.negated) return structural;
            return try self.program.ast.addExpr(ty, .{ .bool_not = structural });
        }

        fn lowerStaticDispatch(
            self: *Self,
            expected: ConcreteTypeInfo,
            plan_id: checked_artifact.StaticDispatchPlanId,
        ) Allocator.Error!Ast.ExprId {
            const plan = self.staticDispatchPlan(plan_id);
            const finalized = self.finalizedStaticDispatch(plan_id);
            if (!self.program.types.equalIds(finalized.ret_ty.ty, expected.ty)) {
                invariantViolation("mono static dispatch finalized result type did not match body emission expectation");
            }

            if (finalized.arg_infos.len != plan.args.len) invariantViolation("mono static dispatch argument count did not match finalized callable arity");
            const lowered_args = try self.lowerExprSpanConcrete(plan.args, finalized.arg_infos);
            const arg_items = self.program.ast.sliceExprSpan(lowered_args);

            return switch (finalized.resolution) {
                .method_target => |method| blk: {
                    const template = try self.name_resolver.procedureTemplateRef(method.target.template orelse invariantViolation("mono finalized static dispatch target did not publish a checked procedure template"));
                    const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                        .template = template,
                        .requested_fn_ty = finalized.requested_fn_ty,
                        .reason = .{ .static_dispatch_target = plan_id },
                        .imported_closure = if (self.template_lookup.imported_closure) |closure|
                            if (importedClosureContainsProcedureTemplate(closure, template)) closure else null
                        else
                            null,
                    });
                    const call_expr = try self.program.ast.addExpr(finalized.ret_ty.ty, .{ .call_proc = .{
                        .proc = mirProcedureRefFromReserved(reserved),
                        .args = lowered_args,
                        .requested_fn_ty = finalized.callable_ty,
                        .requested_source_fn_ty = self.program.concrete_source_types.key(finalized.requested_fn_ty),
                        .requested_source_fn_ty_payload = finalized.requested_fn_ty,
                    } });
                    self.program.ast.setExprSourceInfo(call_expr, finalized.ret_ty.source_ty, finalized.ret_ty.source_ref);
                    break :blk switch (plan.result_mode) {
                        .value => call_expr,
                        .equality => |equality| if (equality.negated)
                            try self.program.ast.addExpr(expected.ty, .{ .bool_not = call_expr })
                        else
                            call_expr,
                    };
                },
                .structural_equality => blk: {
                    if (plan.args.len != 2) invariantViolation("mono finalized structural equality did not have exactly two operands");
                    const structural = try self.program.ast.addExpr(expected.ty, .{ .structural_eq = .{ .lhs = arg_items[0], .rhs = arg_items[1] } });
                    break :blk switch (plan.result_mode) {
                        .value => invariantViolation("mono finalized value dispatch cannot use structural equality"),
                        .equality => |equality| if (equality.negated)
                            try self.program.ast.addExpr(expected.ty, .{ .bool_not = structural })
                        else
                            structural,
                    };
                },
            };
        }

        fn finalizedStaticDispatchResultType(
            self: *Self,
            plan_id: checked_artifact.StaticDispatchPlanId,
        ) ConcreteTypeInfo {
            return self.finalizedStaticDispatch(plan_id).ret_ty;
        }

        fn lowerIteratorDispatchExpected(
            self: *Self,
            expected: ConcreteTypeInfo,
            plan_id: checked_artifact.IteratorForPlanId,
            kind: IteratorDispatchKind,
            obligation: static_dispatch.IteratorDispatchObligation,
            loop_state: ?IteratorLoopStateOperand,
        ) Allocator.Error!LoweredStaticDispatchCall {
            const finalized = self.finalizedIteratorDispatch(.{ .plan = plan_id, .kind = kind });
            if (!self.program.types.equalIds(finalized.ret_ty.ty, expected.ty)) {
                invariantViolation("mono iterator dispatch finalized result type did not match body emission expectation");
            }
            if (finalized.arg_infos.len != obligation.args.len) invariantViolation("mono iterator dispatch requested function argument count disagreed with operand metadata");

            const lowered_arg_items = try self.allocator.alloc(Ast.ExprId, obligation.args.len);
            defer self.allocator.free(lowered_arg_items);

            for (obligation.args, finalized.arg_infos, 0..) |operand, param_info, index| {
                lowered_arg_items[index] = switch (operand) {
                    .checked_expr => |expr| try self.lowerExprConcreteExpected(expr, param_info),
                    .loop_iterator_state => blk: {
                        const state = loop_state orelse invariantViolation("mono iterator dispatch loop-state operand had no compiler-created value");
                        if (!self.program.types.equalIds(state.info.ty, param_info.ty)) {
                            invariantViolation("mono iterator dispatch loop-state type did not match callable type");
                        }
                        break :blk try self.program.ast.addExprWithSourcePayload(param_info.ty, param_info.source_ty, param_info.source_ref, .{ .var_ = state.symbol });
                    },
                };
            }

            const lowered_args = try self.program.ast.addExprSpan(lowered_arg_items);

            const method = switch (finalized.resolution) {
                .method_target => |method| method,
                .structural_equality => invariantViolation("mono iterator dispatch cannot resolve to structural equality"),
            };
            const template = try self.name_resolver.procedureTemplateRef(method.target.template orelse invariantViolation("mono iterator dispatch method target did not publish a checked procedure template"));
            const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                .template = template,
                .requested_fn_ty = finalized.requested_fn_ty,
                .reason = .{ .iterator_dispatch_target = plan_id },
                .imported_closure = if (self.template_lookup.imported_closure) |closure|
                    if (importedClosureContainsProcedureTemplate(closure, template)) closure else null
                else
                    null,
            });
            const call_expr = try self.program.ast.addExpr(finalized.ret_ty.ty, .{ .call_proc = .{
                .proc = mirProcedureRefFromReserved(reserved),
                .args = lowered_args,
                .requested_fn_ty = finalized.callable_ty,
                .requested_source_fn_ty = self.program.concrete_source_types.key(finalized.requested_fn_ty),
                .requested_source_fn_ty_payload = finalized.requested_fn_ty,
            } });
            self.program.ast.setExprSourceInfo(call_expr, finalized.ret_ty.source_ty, finalized.ret_ty.source_ref);
            return .{ .expr = call_expr, .ret_ty = finalized.ret_ty };
        }

        fn concreteRefForMethodTargetCallable(
            self: *Self,
            method_target: static_dispatch.MethodTarget,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            return try self.graphInstantiator().concreteRefForMethodTargetCallable(method_target);
        }

        fn methodOwnerForDispatcherSourceTypeMaybe(
            self: *Self,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!?static_dispatch.MethodOwner {
            var current = source_ref;
            while (true) {
                current = self.graphInstantiator().resolveConcreteRef(current);
                switch (self.graphInstantiator().concretePayload(current)) {
                    .alias => |alias| {
                        current = try self.graphInstantiator().concreteAliasBackingRef(current, alias);
                    },
                    .nominal => |nominal| {
                        if (nominal.builtin) |builtin| {
                            return .{ .builtin = methodOwnerForCheckedBuiltinNominal(builtin) };
                        }
                        return .{ .nominal = try self.graphInstantiator().nominalKeyForConcreteRef(
                            current,
                            nominal.origin_module,
                            nominal.name,
                        ) };
                    },
                    .record,
                    .tuple,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    => return null,
                    .flex => |flex| {
                        try self.verifyDispatcherVariableHasNoRuntimeOwnerConstraint(current, flex);
                        return null;
                    },
                    .rigid => |rigid| {
                        try self.verifyDispatcherVariableHasNoRuntimeOwnerConstraint(current, rigid);
                        return null;
                    },
                    .pending,
                    .record_unbound,
                    .function,
                    => invariantViolation("mono static dispatch dispatcher source type did not resolve to an allowed method owner"),
                }
            }
        }

        fn verifyDispatcherVariableHasNoRuntimeOwnerConstraint(
            self: *Self,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            variable: checked_artifact.CheckedTypeVariable,
        ) Allocator.Error!void {
            if (variable.constraints.len == 0) return;
            if (self.graphInstantiator().isMonoSpecializationNumericFlex(variable)) return;
            if (try self.graphInstantiator().isConcreteEqualityOnlyVariable(source_ref, variable)) return;
            invariantViolation("mono finalized dispatch reached a constrained variable without concrete dispatcher evidence");
        }

        fn methodOwnerForInspectSourceTypeMaybe(
            self: *Self,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!?static_dispatch.MethodOwner {
            var current = source_ref;
            while (true) {
                current = self.graphInstantiator().resolveConcreteRef(current);
                switch (self.graphInstantiator().concretePayload(current)) {
                    .alias => |alias| {
                        current = try self.graphInstantiator().concreteAliasBackingRef(current, alias);
                    },
                    .nominal => |nominal| {
                        if (nominal.builtin) |builtin| {
                            return .{ .builtin = methodOwnerForCheckedBuiltinNominal(builtin) };
                        }
                        return .{ .nominal = try self.graphInstantiator().nominalKeyForConcreteRef(
                            current,
                            nominal.origin_module,
                            nominal.name,
                        ) };
                    },
                    .record,
                    .tuple,
                    .empty_record,
                    .tag_union,
                    .empty_tag_union,
                    .record_unbound,
                    .function,
                    => return null,
                    .pending,
                    .flex,
                    .rigid,
                    => invariantViolation("mono Str.inspect source type did not resolve before custom inspect lookup"),
                }
            }
        }

        fn lookupMethodTarget(
            self: *Self,
            owner: static_dispatch.MethodOwner,
            method: canonical.MethodNameId,
        ) Allocator.Error!?static_dispatch.MethodTarget {
            return try lookupStaticDispatchMethodTarget(
                self.input,
                self.name_resolver,
                owner,
                method,
            );
        }

        fn lowerIf(
            self: *Self,
            ty: Type.TypeId,
            branches: []const checked_artifact.CheckedIfBranch,
            final_else: checked_artifact.CheckedExprId,
            expected_result_ty: ExprExpectedType,
        ) Allocator.Error!Ast.ExprId {
            var current = try self.lowerExprWithExpected(final_else, expected_result_ty);
            var i = branches.len;
            while (i > 0) {
                i -= 1;
                current = try self.program.ast.addExpr(ty, .{ .if_ = .{
                    .cond = try self.lowerBoolConditionExpr(branches[i].cond),
                    .then_body = try self.lowerExprWithExpected(branches[i].body, expected_result_ty),
                    .else_body = current,
                } });
            }
            return current;
        }

        fn lowerMatch(
            self: *Self,
            ty: Type.TypeId,
            match_: anytype,
            expected_result_ty: ExprExpectedType,
        ) Allocator.Error!Ast.ExprId {
            const cond_info = try self.concreteResultTypeForExpr(match_.cond);
            const cond = try self.lowerExprConcreteExpected(match_.cond, cond_info);
            if (match_.branches.len == 0) invariantViolation("mono body lowering received a checked match with no branches");

            var branches = std.ArrayList(Ast.Branch).empty;
            defer branches.deinit(self.allocator);
            for (match_.branches) |branch| {
                if (branch.patterns.len == 0) invariantViolation("mono body lowering received a checked match branch with no alternatives");
                for (branch.patterns) |branch_pattern| {
                    try branches.append(self.allocator, .{
                        .pat = try self.lowerPatternWithRemaps(cond_info, branch_pattern.pattern, branch_pattern.binder_remaps),
                        .guard = if (branch_pattern.degenerate or branch.guard == null) null else try self.lowerBoolConditionExpr(branch.guard.?),
                        .body = if (branch_pattern.degenerate) try self.program.ast.addExpr(ty, .runtime_error) else try self.lowerExprWithExpected(branch.value, expected_result_ty),
                        .degenerate = branch_pattern.degenerate,
                    });
                }
            }

            return try self.program.ast.addExpr(ty, .{ .match_ = .{
                .cond = cond,
                .branches = try self.program.ast.addBranchSpan(branches.items),
                .is_try_suffix = match_.is_try_suffix,
            } });
        }

        fn lowerTag(
            self: *Self,
            ty: Type.TypeId,
            source_ref: ?ConcreteSourceType.ConcreteSourceTypeRef,
            name: canonical.TagLabelId,
            args: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const tag_info = self.tagInfoForUnionType(ty, name);
            if (tag_info.payload_count != args.len) invariantViolation("mono body lowering tag constructor arity did not match its resolved type");
            const payload_types = if (args.len == 0)
                &[_]ConcreteTypeInfo{}
            else
                try self.concreteTagPayloadInfosForUnionType(
                    source_ref orelse invariantViolation("mono body lowering tag constructor had no concrete source type"),
                    name,
                );
            defer if (payload_types.len != 0) self.allocator.free(payload_types);
            if (payload_types.len != args.len) invariantViolation("mono body lowering concrete tag payload arity did not match resolved type");
            const lowered_args = try self.lowerExprSpanConcrete(args, payload_types);
            const final_info = if (source_ref) |ref| try self.runtimeConcreteTypeInfo(ref) else ConcreteTypeInfo{
                .ty = ty,
                .source_ty = .{},
                .source_ref = undefined,
            };
            const final_tag_info = self.tagInfoForUnionType(final_info.ty, name);
            return try self.program.ast.addExprWithSourcePayload(final_info.ty, final_info.source_ty, final_info.source_ref, .{ .tag = .{
                .name = name,
                .discriminant = final_tag_info.discriminant,
                .args = lowered_args,
                .constructor_ty = final_info.ty,
            } });
        }

        fn lowerFieldAccess(
            self: *Self,
            field_info: ConcreteTypeInfo,
            receiver: checked_artifact.CheckedExprId,
            field_name: canonical.RecordFieldLabelId,
        ) Allocator.Error!Ast.ExprId {
            const ty = field_info.ty;
            const receiver_ty = try self.concreteResultTypeForExpr(receiver);
            const record = try self.lowerExprConcreteExpected(receiver, receiver_ty);
            const record_ty = self.program.ast.getExpr(record).ty;
            return try self.program.ast.addExpr(ty, .{ .access = .{
                .record = record,
                .field = field_name,
                .field_index = self.recordFieldIndex(record_ty, field_name),
            } });
        }

        fn lowerForExpr(
            self: *Self,
            ty: Type.TypeId,
            plan_id: checked_artifact.IteratorForPlanId,
            pattern: checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const plan = self.iteratorForPlan(plan_id);
            const iterator_info = self.finalizedIteratorDispatch(.{ .plan = plan_id, .kind = .iter }).ret_ty;
            const iter_call = try self.lowerIteratorDispatchExpected(
                iterator_info,
                plan_id,
                .iter,
                plan.iter,
                null,
            );

            const iterator_symbol = try self.program.addSyntheticSymbol();
            const iterator_decl = try self.program.ast.addStmt(.{ .var_decl = .{
                .bind = .{
                    .ty = iter_call.ret_ty.ty,
                    .source_ty = iter_call.ret_ty.source_ty,
                    .source_ty_payload = iter_call.ret_ty.source_ref,
                    .symbol = iterator_symbol,
                },
                .body = iter_call.expr,
            } });

            const while_body = try self.lowerIteratorForWhileBody(
                plan_id,
                plan,
                pattern,
                body,
                iterator_symbol,
                iter_call.ret_ty,
                ty,
            );
            const bool_info = try self.boolConcreteTypeInfo();
            const true_expr = try self.lowerBoolLiteral(bool_info, true);
            const while_stmt = try self.program.ast.addStmt(.{ .while_ = .{
                .cond = true_expr,
                .body = while_body,
            } });
            const unit = try self.program.ast.addExpr(ty, .unit);
            return try self.program.ast.addExpr(ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&.{ iterator_decl, while_stmt }),
                .final_expr = unit,
            } });
        }

        fn lowerIteratorForWhileBody(
            self: *Self,
            plan_id: checked_artifact.IteratorForPlanId,
            plan: static_dispatch.IteratorForPlan,
            pattern: checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
            iterator_symbol: Ast.Symbol,
            iterator_info: ConcreteTypeInfo,
            unit_ty: Type.TypeId,
        ) Allocator.Error!Ast.ExprId {
            const step_info = self.finalizedIteratorDispatch(.{ .plan = plan_id, .kind = .next }).ret_ty;
            const next_call = try self.lowerIteratorDispatchExpected(
                step_info,
                plan_id,
                .next,
                plan.next,
                .{ .symbol = iterator_symbol, .info = iterator_info },
            );
            const branches = [_]Ast.Branch{
                try self.lowerIteratorDoneBranch(step_info, unit_ty),
                try self.lowerIteratorOneBranch(pattern, body, iterator_symbol, iterator_info, step_info, unit_ty),
                try self.lowerIteratorSkipBranch(iterator_symbol, iterator_info, step_info, unit_ty),
            };
            return try self.program.ast.addExprWithSourcePayload(unit_ty, .{}, null, .{ .match_ = .{
                .cond = next_call.expr,
                .branches = try self.program.ast.addBranchSpan(&branches),
                .is_try_suffix = false,
            } });
        }

        fn lowerIteratorDoneBranch(
            self: *Self,
            step_info: ConcreteTypeInfo,
            unit_ty: Type.TypeId,
        ) Allocator.Error!Ast.Branch {
            const done_label = try self.program.canonical_names.internTagLabel("Done");
            const done_info = self.tagInfoForUnionType(step_info.ty, done_label);
            if (done_info.payload_count != 0) invariantViolation("mono iterator-for Done step had payloads");
            return .{
                .pat = try self.program.ast.addPat(.{
                    .ty = step_info.ty,
                    .source_ty = step_info.source_ty,
                    .source_ty_payload = step_info.source_ref,
                    .data = .{ .tag = .{
                        .name = done_label,
                        .discriminant = done_info.discriminant,
                        .args = Ast.Span(Ast.PatId).empty(),
                    } },
                }),
                .body = try self.lowerIteratorBreakBody(unit_ty),
            };
        }

        fn lowerIteratorSkipBranch(
            self: *Self,
            iterator_symbol: Ast.Symbol,
            iterator_info: ConcreteTypeInfo,
            step_info: ConcreteTypeInfo,
            unit_ty: Type.TypeId,
        ) Allocator.Error!Ast.Branch {
            const skip_label = try self.program.canonical_names.internTagLabel("Skip");
            const skip_info = self.tagInfoForUnionType(step_info.ty, skip_label);
            if (skip_info.payload_count != 1) invariantViolation("mono iterator-for Skip step did not have exactly one payload");
            const payload_infos = try self.concreteTagPayloadInfosForUnionType(step_info.source_ref, skip_label);
            defer if (payload_infos.len != 0) self.allocator.free(payload_infos);
            if (payload_infos.len != 1) invariantViolation("mono iterator-for Skip source payload count disagreed with runtime type");

            const count_label = try self.program.canonical_names.internRecordFieldLabel("count");
            const rest_label = try self.program.canonical_names.internRecordFieldLabel("rest");
            const count_info = try self.concreteRecordFieldInfo(payload_infos[0].source_ref, count_label);
            const rest_info = try self.concreteRecordFieldInfo(payload_infos[0].source_ref, rest_label);
            if (!self.program.types.equalIds(rest_info.ty, iterator_info.ty)) {
                invariantViolation("mono iterator-for Skip rest field type did not match iterator state type");
            }

            const rest_symbol = try self.program.addSyntheticSymbol();
            const fields = [_]Ast.RecordFieldPattern{
                .{
                    .field = count_label,
                    .pattern = try self.program.ast.addPat(.{
                        .ty = count_info.ty,
                        .source_ty = count_info.source_ty,
                        .source_ty_payload = count_info.source_ref,
                        .data = .wildcard,
                    }),
                },
                .{
                    .field = rest_label,
                    .pattern = try self.program.ast.addPat(.{
                        .ty = rest_info.ty,
                        .source_ty = rest_info.source_ty,
                        .source_ty_payload = rest_info.source_ref,
                        .data = .{ .var_ = rest_symbol },
                    }),
                },
            };
            const payload_pat = try self.program.ast.addPat(.{
                .ty = payload_infos[0].ty,
                .source_ty = payload_infos[0].source_ty,
                .source_ty_payload = payload_infos[0].source_ref,
                .data = .{ .record = .{
                    .fields = try self.program.ast.addRecordFieldPatternSpan(&fields),
                    .rest = null,
                } },
            });
            const rest_expr = try self.program.ast.addExprWithSourcePayload(iterator_info.ty, iterator_info.source_ty, iterator_info.source_ref, .{ .var_ = rest_symbol });
            const reassign = try self.program.ast.addStmt(.{ .reassign = .{
                .target = iterator_symbol,
                .body = rest_expr,
            } });
            const unit = try self.program.ast.addExpr(unit_ty, .unit);
            return .{
                .pat = try self.program.ast.addPat(.{
                    .ty = step_info.ty,
                    .source_ty = step_info.source_ty,
                    .source_ty_payload = step_info.source_ref,
                    .data = .{ .tag = .{
                        .name = skip_label,
                        .discriminant = skip_info.discriminant,
                        .args = try self.program.ast.addPatSpan(&.{payload_pat}),
                    } },
                }),
                .body = try self.program.ast.addExpr(unit_ty, .{ .block = .{
                    .stmts = try self.program.ast.addStmtSpan(&.{reassign}),
                    .final_expr = unit,
                } }),
            };
        }

        fn lowerIteratorOneBranch(
            self: *Self,
            pattern: checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
            iterator_symbol: Ast.Symbol,
            iterator_info: ConcreteTypeInfo,
            step_info: ConcreteTypeInfo,
            unit_ty: Type.TypeId,
        ) Allocator.Error!Ast.Branch {
            const one_label = try self.program.canonical_names.internTagLabel("One");
            const one_info = self.tagInfoForUnionType(step_info.ty, one_label);
            if (one_info.payload_count != 1) invariantViolation("mono iterator-for One step did not have exactly one payload");
            const payload_infos = try self.concreteTagPayloadInfosForUnionType(step_info.source_ref, one_label);
            defer if (payload_infos.len != 0) self.allocator.free(payload_infos);
            if (payload_infos.len != 1) invariantViolation("mono iterator-for One source payload count disagreed with runtime type");

            const item_label = try self.program.canonical_names.internRecordFieldLabel("item");
            const rest_label = try self.program.canonical_names.internRecordFieldLabel("rest");
            const item_info = try self.concreteRecordFieldInfo(payload_infos[0].source_ref, item_label);
            const rest_info = try self.concreteRecordFieldInfo(payload_infos[0].source_ref, rest_label);
            if (!self.program.types.equalIds(rest_info.ty, iterator_info.ty)) {
                invariantViolation("mono iterator-for rest field type did not match iterator state type");
            }

            const rest_symbol = try self.program.addSyntheticSymbol();
            const rest_pat = try self.program.ast.addPat(.{
                .ty = rest_info.ty,
                .source_ty = rest_info.source_ty,
                .source_ty_payload = rest_info.source_ref,
                .data = .{ .var_ = rest_symbol },
            });
            const fields = [_]Ast.RecordFieldPattern{
                .{
                    .field = item_label,
                    .pattern = try self.lowerPattern(item_info, pattern),
                },
                .{
                    .field = rest_label,
                    .pattern = rest_pat,
                },
            };
            const payload_pat = try self.program.ast.addPat(.{
                .ty = payload_infos[0].ty,
                .source_ty = payload_infos[0].source_ty,
                .source_ty_payload = payload_infos[0].source_ref,
                .data = .{ .record = .{
                    .fields = try self.program.ast.addRecordFieldPatternSpan(&fields),
                    .rest = null,
                } },
            });
            const one_pat = try self.program.ast.addPat(.{
                .ty = step_info.ty,
                .source_ty = step_info.source_ty,
                .source_ty_payload = step_info.source_ref,
                .data = .{ .tag = .{
                    .name = one_label,
                    .discriminant = one_info.discriminant,
                    .args = try self.program.ast.addPatSpan(&.{payload_pat}),
                } },
            });

            const rest_expr = try self.program.ast.addExprWithSourcePayload(iterator_info.ty, iterator_info.source_ty, iterator_info.source_ref, .{ .var_ = rest_symbol });
            const reassign = try self.program.ast.addStmt(.{ .reassign = .{
                .target = iterator_symbol,
                .body = rest_expr,
            } });
            const body_stmt = try self.program.ast.addStmt(.{ .expr = try self.lowerExpr(body) });
            const unit = try self.program.ast.addExpr(unit_ty, .unit);
            return .{
                .pat = one_pat,
                .body = try self.program.ast.addExpr(unit_ty, .{ .block = .{
                    .stmts = try self.program.ast.addStmtSpan(&.{ reassign, body_stmt }),
                    .final_expr = unit,
                } }),
            };
        }

        fn lowerIteratorBreakBody(
            self: *Self,
            unit_ty: Type.TypeId,
        ) Allocator.Error!Ast.ExprId {
            const break_stmt = try self.program.ast.addStmt(.break_);
            const unit = try self.program.ast.addExpr(unit_ty, .unit);
            return try self.program.ast.addExpr(unit_ty, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(&.{break_stmt}),
                .final_expr = unit,
            } });
        }

        fn lowerRunLowLevel(
            self: *Self,
            ty: Type.TypeId,
            op: base.LowLevel,
            args: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            return try self.program.ast.addExpr(ty, .{ .low_level = .{
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.lowerExprSpan(args),
                .source_constraint_ty = ty,
            } });
        }

        fn lowerBinop(
            self: *Self,
            expected: ConcreteTypeInfo,
            binop: anytype,
        ) Allocator.Error!Ast.ExprId {
            const ty = expected.ty;
            return switch (binop.op) {
                .add => try self.lowerBinaryLowLevel(expected, .num_plus, binop.lhs, binop.rhs),
                .sub => try self.lowerBinaryLowLevel(expected, .num_minus, binop.lhs, binop.rhs),
                .mul => try self.lowerBinaryLowLevel(expected, .num_times, binop.lhs, binop.rhs),
                .div => try self.lowerBinaryLowLevel(expected, .num_div_by, binop.lhs, binop.rhs),
                .rem => try self.lowerBinaryLowLevel(expected, .num_rem_by, binop.lhs, binop.rhs),
                .div_trunc => try self.lowerBinaryLowLevel(expected, .num_div_trunc_by, binop.lhs, binop.rhs),
                .lt => try self.lowerBinaryLowLevel(expected, .num_is_lt, binop.lhs, binop.rhs),
                .gt => try self.lowerBinaryLowLevel(expected, .num_is_gt, binop.lhs, binop.rhs),
                .le => try self.lowerBinaryLowLevel(expected, .num_is_lte, binop.lhs, binop.rhs),
                .ge => try self.lowerBinaryLowLevel(expected, .num_is_gte, binop.lhs, binop.rhs),
                .eq => blk: {
                    const lhs = try self.lowerExpr(binop.lhs);
                    const rhs = try self.lowerExpr(binop.rhs);
                    break :blk try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
                },
                .ne => blk: {
                    const lhs = try self.lowerExpr(binop.lhs);
                    const rhs = try self.lowerExpr(binop.rhs);
                    const eq = try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
                    break :blk try self.program.ast.addExpr(ty, .{ .bool_not = eq });
                },
                .@"and" => blk: {
                    const bool_info = try self.boolConcreteTypeInfo();
                    const false_expr = try self.lowerBoolLiteral(bool_info, false);
                    break :blk try self.program.ast.addExpr(ty, .{ .if_ = .{
                        .cond = try self.lowerBoolConditionExpr(binop.lhs),
                        .then_body = try self.lowerBoolConditionExpr(binop.rhs),
                        .else_body = false_expr,
                    } });
                },
                .@"or" => blk: {
                    const bool_info = try self.boolConcreteTypeInfo();
                    const true_expr = try self.lowerBoolLiteral(bool_info, true);
                    break :blk try self.program.ast.addExpr(ty, .{ .if_ = .{
                        .cond = try self.lowerBoolConditionExpr(binop.lhs),
                        .then_body = true_expr,
                        .else_body = try self.lowerBoolConditionExpr(binop.rhs),
                    } });
                },
            };
        }

        fn lowerBinaryLowLevel(
            self: *Self,
            result: ConcreteTypeInfo,
            op: base.LowLevel,
            lhs_expr: checked_artifact.CheckedExprId,
            rhs_expr: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const operand_info = try self.lowLevelBinaryOperandInfo(result, op, lhs_expr, rhs_expr);
            const lhs = try self.lowerExprConcreteExpected(lhs_expr, operand_info);
            const rhs = try self.lowerExprConcreteExpected(rhs_expr, operand_info);
            const args = [_]Ast.ExprId{ lhs, rhs };
            return try self.program.ast.addExpr(result.ty, .{ .low_level = .{
                .op = op,
                .rc_effect = op.rcEffect(),
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = operand_info.ty,
            } });
        }

        fn lowLevelBinaryOperandInfo(
            self: *Self,
            result: ConcreteTypeInfo,
            op: base.LowLevel,
            lhs_expr: checked_artifact.CheckedExprId,
            rhs_expr: checked_artifact.CheckedExprId,
        ) Allocator.Error!ConcreteTypeInfo {
            switch (op) {
                .num_plus,
                .num_minus,
                .num_times,
                .num_div_by,
                .num_rem_by,
                .num_div_trunc_by,
                => return result,
                .num_is_lt,
                .num_is_gt,
                .num_is_lte,
                .num_is_gte,
                => {},
                else => invariantViolation("mono body lowering requested binary operand type for a non-binary numeric low-level op"),
            }

            if (mode == .body_emitter) {
                const operand_ref = self.finalizedDemandSourceRefForExpr(lhs_expr) orelse
                    self.finalizedDemandSourceRefForExpr(rhs_expr) orelse
                    invariantViolation("mono body emission reached comparison operands before graph finalization published their concrete demand");
                return try self.runtimeConcreteTypeInfo(operand_ref);
            }
            if (try self.knownConcreteResultTypeForExpr(lhs_expr)) |lhs_info| return lhs_info;
            if (try self.knownConcreteResultTypeForExpr(rhs_expr)) |rhs_info| return rhs_info;
            return try self.concreteResultTypeForExpr(lhs_expr);
        }

        fn lowerUnaryMinus(
            self: *Self,
            expected: ConcreteTypeInfo,
            child_expr: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.ExprId {
            const child = try self.lowerExprConcreteExpected(child_expr, expected);
            const args = [_]Ast.ExprId{child};
            return try self.program.ast.addExpr(expected.ty, .{ .low_level = .{
                .op = .num_negate,
                .rc_effect = base.LowLevel.num_negate.rcEffect(),
                .args = try self.program.ast.addExprSpan(&args),
                .source_constraint_ty = expected.ty,
            } });
        }

        fn lowerPattern(
            self: *Self,
            expected: ConcreteTypeInfo,
            pattern_id: checked_artifact.CheckedPatternId,
        ) Allocator.Error!Ast.PatId {
            return try self.lowerPatternWithRemaps(expected, pattern_id, &.{});
        }

        fn lowerPatternWithRemaps(
            self: *Self,
            expected: ConcreteTypeInfo,
            pattern_id: checked_artifact.CheckedPatternId,
            binder_remaps: []const checked_artifact.CheckedAlternativeBinderRemap,
        ) Allocator.Error!Ast.PatId {
            const pattern = self.checkedPattern(pattern_id);
            const ty = expected.ty;
            const lowered = switch (pattern.data) {
                .assign => |binder| blk: {
                    const representative = self.representativeBinderForCandidate(binder, binder_remaps);
                    const binder_expected = if (self.graph.local_type_demands.get(self.scopedBinder(representative))) |demanded_ref|
                        try self.runtimeConcreteTypeInfo(demanded_ref)
                    else
                        expected;
                    try self.recordConcreteTypeForBinder(representative, binder_expected);
                    break :blk try self.program.ast.addPat(.{ .ty = binder_expected.ty, .data = .{
                        .var_ = try self.symbolForBinder(representative),
                    } });
                },
                .as => |as| {
                    const representative = self.representativeBinderForCandidate(as.binder, binder_remaps);
                    const binder_expected = if (self.graph.local_type_demands.get(self.scopedBinder(representative))) |demanded_ref|
                        try self.runtimeConcreteTypeInfo(demanded_ref)
                    else
                        expected;
                    try self.recordConcreteTypeForBinder(representative, binder_expected);
                    const symbol = try self.symbolForBinder(representative);
                    const nested = try self.lowerPatternWithRemaps(binder_expected, as.pattern, binder_remaps);
                    return try self.program.ast.addPat(.{ .ty = binder_expected.ty, .data = .{ .as = .{
                        .pattern = nested,
                        .symbol = symbol,
                    } } });
                },
                .applied_tag => |tag| blk: {
                    const tag_name = try self.tagLabel(tag.name);
                    const tag_info = self.tagInfoForUnionType(ty, tag_name);
                    if (tag_info.payload_count != tag.args.len) invariantViolation("mono body lowering tag pattern arity did not match its resolved type");
                    const payload_types = if (tag.args.len == 0)
                        &[_]ConcreteTypeInfo{}
                    else
                        try self.concreteTagPayloadInfosForUnionType(expected.source_ref, tag_name);
                    defer if (payload_types.len != 0) self.allocator.free(payload_types);
                    if (payload_types.len != tag.args.len) invariantViolation("mono body lowering concrete tag pattern payload arity did not match resolved type");
                    const args = try self.allocator.alloc(Ast.PatId, tag.args.len);
                    defer self.allocator.free(args);
                    for (tag.args, 0..) |arg, i| {
                        args[i] = try self.lowerPatternWithRemaps(payload_types[i], arg, binder_remaps);
                    }
                    break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .tag = .{
                        .name = tag_name,
                        .discriminant = tag_info.discriminant,
                        .args = try self.program.ast.addPatSpan(args),
                    } } });
                },
                .nominal => |nominal| blk: {
                    const backing = try self.concreteNominalBackingInfo(expected);
                    break :blk try self.program.ast.addPat(.{
                        .ty = ty,
                        .data = .{ .nominal = try self.lowerPatternWithRemaps(backing, nominal.backing_pattern, binder_remaps) },
                    });
                },
                .record_destructure => |destructs| blk: {
                    const fields = try self.allocator.alloc(Ast.RecordFieldPattern, destructs.len);
                    defer self.allocator.free(fields);
                    var field_count: usize = 0;
                    var rest: ?Ast.PatId = null;
                    for (destructs) |destruct| {
                        switch (destruct.kind) {
                            .required, .sub_pattern => |field_pattern| {
                                const label = try self.recordFieldLabel(destruct.label);
                                const field_info = try self.concreteRecordFieldInfo(expected.source_ref, label);
                                fields[field_count] = .{
                                    .field = label,
                                    .pattern = try self.lowerPatternWithRemaps(field_info, field_pattern, binder_remaps),
                                };
                                field_count += 1;
                            },
                            .rest => |rest_pattern| {
                                if (rest != null) invariantViolation("mono body lowering record pattern had duplicate rest binders");
                                const rest_ty = self.finalizedPatternTypeInfo(rest_pattern);
                                rest = try self.lowerPatternWithRemaps(rest_ty, rest_pattern, binder_remaps);
                            },
                        }
                    }
                    break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .record = .{
                        .fields = try self.program.ast.addRecordFieldPatternSpan(fields[0..field_count]),
                        .rest = rest,
                    } } });
                },
                .tuple => |items| blk: {
                    const item_types = try self.concreteTupleElementInfos(expected.source_ref, items.len);
                    defer if (item_types.len != 0) self.allocator.free(item_types);
                    const lowered = try self.allocator.alloc(Ast.PatId, items.len);
                    defer self.allocator.free(lowered);
                    for (items, 0..) |item, i| {
                        lowered[i] = try self.lowerPatternWithRemaps(item_types[i], item, binder_remaps);
                    }
                    break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .tuple = try self.program.ast.addPatSpan(lowered) } });
                },
                .list => |list| blk: {
                    const elem_ty = try self.listElementTypeFromConcrete(expected.source_ref);
                    const lowered = try self.allocator.alloc(Ast.PatId, list.patterns.len);
                    defer self.allocator.free(lowered);
                    for (list.patterns, 0..) |item, i| {
                        lowered[i] = try self.lowerPatternWithRemaps(elem_ty, item, binder_remaps);
                    }
                    const rest: ?Ast.ListRestPattern = if (list.rest) |rest_info| .{
                        .index = rest_info.index,
                        .pattern = if (rest_info.pattern) |rest_pattern| try self.lowerPatternWithRemaps(expected, rest_pattern, binder_remaps) else null,
                    } else null;
                    break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .list = .{
                        .items = try self.program.ast.addPatSpan(lowered),
                        .rest = rest,
                    } } });
                },
                .num_literal => |num| try self.lowerIntegerLiteralPattern(ty, num.value),
                .small_dec_literal => |dec| try self.lowerScaledDecimalLiteralPattern(ty, dec.value.toRocDec().num),
                .dec_literal => |dec| try self.lowerScaledDecimalLiteralPattern(ty, dec.value.num),
                .frac_f32_literal => |value| try self.lowerF32LiteralPattern(ty, value),
                .frac_f64_literal => |value| try self.lowerF64LiteralPattern(ty, value),
                .str_literal => |literal| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .str_lit = try self.lowerCheckedStringLiteral(literal) } }),
                .underscore => try self.program.ast.addPat(.{ .ty = ty, .data = .wildcard }),
                .runtime_error => invariantViolation("mono body lowering reached runtime_error checked pattern"),
                .pending => invariantViolation("mono body lowering reached an unresolved checked pattern"),
            };
            self.program.ast.pats.items[@intFromEnum(lowered)].source_ty = expected.source_ty;
            self.program.ast.pats.items[@intFromEnum(lowered)].source_ty_payload = expected.source_ref;
            return lowered;
        }

        fn lowerIntegerLiteralPattern(
            self: *Self,
            ty: Type.TypeId,
            value: CIR.IntValue,
        ) Allocator.Error!Ast.PatId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
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
                    => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .int_lit = @as(i128, @bitCast(value.bytes)) } }),
                    .f32 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f32_lit = @floatCast(intValueToF64(value)) } }),
                    .f64 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f64_lit = intValueToF64(value) } }),
                    .dec => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .dec_lit = intValueToScaledDec(value) } }),
                    else => invariantViolation("mono body lowering reached integer literal pattern with non-numeric primitive type"),
                },
                else => invariantViolation("mono body lowering reached integer literal pattern with non-primitive result type"),
            };
        }

        fn lowerScaledDecimalLiteralPattern(
            self: *Self,
            ty: Type.TypeId,
            scaled_value: i128,
        ) Allocator.Error!Ast.PatId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
                    .f32 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f32_lit = @floatCast(scaledDecToF64(scaled_value)) } }),
                    .f64 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f64_lit = scaledDecToF64(scaled_value) } }),
                    .dec => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .dec_lit = scaled_value } }),
                    else => invariantViolation("mono body lowering reached decimal literal pattern with non-fractional primitive type"),
                },
                else => invariantViolation("mono body lowering reached decimal literal pattern with non-primitive result type"),
            };
        }

        fn lowerF32LiteralPattern(
            self: *Self,
            ty: Type.TypeId,
            value: f32,
        ) Allocator.Error!Ast.PatId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
                    .f32 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f32_lit = value } }),
                    .f64 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f64_lit = @floatCast(value) } }),
                    .dec => invariantViolation("mono body lowering reached binary fraction literal pattern with Dec result type after type checking"),
                    else => invariantViolation("mono body lowering reached binary fraction literal pattern with non-fractional primitive type"),
                },
                else => invariantViolation("mono body lowering reached binary fraction literal pattern with non-primitive result type"),
            };
        }

        fn lowerF64LiteralPattern(
            self: *Self,
            ty: Type.TypeId,
            value: f64,
        ) Allocator.Error!Ast.PatId {
            return switch (self.program.types.getType(ty)) {
                .primitive => |prim| switch (prim) {
                    .f32 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f32_lit = @floatCast(value) } }),
                    .f64 => try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f64_lit = value } }),
                    .dec => invariantViolation("mono body lowering reached binary fraction literal pattern with Dec result type after type checking"),
                    else => invariantViolation("mono body lowering reached binary fraction literal pattern with non-fractional primitive type"),
                },
                else => invariantViolation("mono body lowering reached binary fraction literal pattern with non-primitive result type"),
            };
        }

        fn representativeBinderForCandidate(
            _: *const Self,
            binder: checked_artifact.PatternBinderId,
            binder_remaps: []const checked_artifact.CheckedAlternativeBinderRemap,
        ) checked_artifact.PatternBinderId {
            for (binder_remaps) |remap| {
                if (remap.candidate_binder == binder) return remap.representative_binder;
            }
            return binder;
        }

        fn nominalBackingType(self: *Self, nominal_ty: Type.TypeId) Type.TypeId {
            return switch (self.program.types.getTypePreservingNominal(nominal_ty)) {
                .nominal => |nominal| nominal.backing,
                else => nominal_ty,
            };
        }

        fn concreteNominalBackingInfo(
            self: *Self,
            nominal_info: ConcreteTypeInfo,
        ) Allocator.Error!ConcreteTypeInfo {
            const backing_ref = try self.concreteNominalBackingRef(nominal_info.source_ref);
            const backing_info = try self.runtimeConcreteTypeInfo(backing_ref);
            return .{
                .ty = self.nominalBackingType(nominal_info.ty),
                .source_ty = backing_info.source_ty,
                .source_ref = backing_info.source_ref,
            };
        }

        fn concreteNominalBackingRef(
            self: *Self,
            nominal_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
            var current = nominal_ref;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                    .nominal => |nominal| {
                        return try self.concreteNominalBackingChildRef(current, nominal);
                    },
                    else => invariantViolation("mono body lowering expected concrete nominal type for nominal pattern"),
                }
            }
        }

        fn concreteTupleElementInfos(
            self: *Self,
            tuple_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            expected_len: usize,
        ) Allocator.Error![]const ConcreteTypeInfo {
            const refs = try self.concreteTupleElementRefs(tuple_ref, expected_len);
            defer if (refs.len != 0) self.allocator.free(refs);
            return try self.concreteTypeInfosForRefs(refs);
        }

        fn concreteTupleElementRefs(
            self: *Self,
            tuple_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            expected_len: usize,
        ) Allocator.Error![]const ConcreteSourceType.ConcreteSourceTypeRef {
            var current = tuple_ref;
            while (true) {
                switch (self.concretePayload(current)) {
                    .alias => |alias| current = try self.concreteAliasBackingChildRef(current, alias),
                    .tuple => |items| {
                        if (items.len != expected_len) invariantViolation("mono body lowering tuple pattern arity did not match concrete source type");
                        const out = try self.allocator.alloc(ConcreteSourceType.ConcreteSourceTypeRef, items.len);
                        errdefer self.allocator.free(out);
                        for (items, 0..) |item, i| {
                            out[i] = try self.concreteSourceChildRef(current, .{ .tag = .tuple_elem, .a = @intCast(i) }, item);
                        }
                        return out;
                    },
                    else => invariantViolation("mono body lowering expected concrete tuple type for tuple pattern"),
                }
            }
        }

        const TagInfo = struct {
            discriminant: u16,
            payload_count: usize,
            payload_types: []const Type.TypeId,
        };

        fn tagInfoForUnionType(
            self: *Self,
            union_ty: Type.TypeId,
            name: canonical.TagLabelId,
        ) TagInfo {
            return switch (self.program.types.getType(union_ty)) {
                .tag_union => |tag_union| {
                    for (tag_union.tags, 0..) |tag, i| {
                        if (tag.name == name) return .{
                            .discriminant = @intCast(i),
                            .payload_count = tag.args.len,
                            .payload_types = tag.args,
                        };
                    }
                    invariantViolation("mono body lowering could not find tag constructor in resolved union type");
                },
                else => invariantViolation("mono body lowering expected a resolved tag-union type"),
            };
        }

        fn concreteTagPayloadInfosForUnionType(
            self: *Self,
            union_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            name: canonical.TagLabelId,
        ) Allocator.Error![]const ConcreteTypeInfo {
            const refs = try self.concreteTagPayloadRefsForUnionType(union_ref, name);
            defer if (refs.len != 0) self.allocator.free(refs);
            return try self.concreteTypeInfosForRefs(refs);
        }

        fn concreteTagPayloadRefsForUnionType(
            self: *Self,
            union_ref: ConcreteSourceType.ConcreteSourceTypeRef,
            name: canonical.TagLabelId,
        ) Allocator.Error![]const ConcreteSourceType.ConcreteSourceTypeRef {
            if (mode == .body_emitter) {
                var out = std.ArrayList(ConcreteSourceType.ConcreteSourceTypeRef).empty;
                errdefer out.deinit(self.allocator);
                var index: u32 = 0;
                while (self.graph.concrete_source_children.get(.{ .parent = union_ref, .kind = .{
                    .tag = .tag_payload,
                    .a = @intFromEnum(name),
                    .b = index,
                } })) |payload_ref| : (index += 1) {
                    try out.append(self.allocator, payload_ref);
                }
                if (out.items.len != 0) return try out.toOwnedSlice(self.allocator);
                out.deinit(self.allocator);
                return switch (self.concretePayload(union_ref)) {
                    .alias => |alias| try self.concreteTagPayloadRefsForUnionType(try self.concreteAliasBackingChildRef(union_ref, alias), name),
                    .nominal => try self.concreteTagPayloadRefsForUnionType(try self.concreteNominalBackingRef(union_ref), name),
                    else => invariantViolation("mono body emission reached tag payload children before graph finalization published them"),
                };
            } else {
                return switch (self.concretePayload(union_ref)) {
                    .alias => |alias| try self.concreteTagPayloadRefsForUnionType(try self.concreteAliasBackingChildRef(union_ref, alias), name),
                    .nominal => try self.concreteTagPayloadRefsForUnionType(try self.concreteNominalBackingRef(union_ref), name),
                    .tag_union => |tag_union| blk: {
                        if (try self.findConcreteTag(union_ref, tag_union.tags, name)) |tag| {
                            const out = try self.allocator.alloc(ConcreteSourceType.ConcreteSourceTypeRef, tag.args.len);
                            errdefer self.allocator.free(out);
                            for (tag.args, 0..) |arg, i| {
                                out[i] = try self.concreteSourceChildRef(union_ref, .{
                                    .tag = .tag_payload,
                                    .a = @intFromEnum(name),
                                    .b = @intCast(i),
                                }, arg);
                            }
                            break :blk out;
                        }
                        const ext = try self.concreteSourceChildRef(union_ref, .{ .tag = .tag_union_ext }, tag_union.ext);
                        const ext_payloads = try self.concreteTagPayloadRefsForUnionType(ext, name);
                        errdefer if (ext_payloads.len != 0) self.allocator.free(ext_payloads);
                        for (ext_payloads, 0..) |payload_ref, i| {
                            try self.graph.concrete_source_children.put(.{ .parent = union_ref, .kind = .{
                                .tag = .tag_payload,
                                .a = @intFromEnum(name),
                                .b = @intCast(i),
                            } }, payload_ref);
                        }
                        break :blk ext_payloads;
                    },
                    .empty_tag_union => invariantViolation("mono body lowering concrete tag constructor was missing from source union type"),
                    .flex => |flex| {
                        try self.graphInstantiator().verifyClosableRowTail(flex);
                        invariantViolation("mono body lowering concrete tag constructor was missing from closed source union tail");
                    },
                    else => invariantViolation("mono body lowering expected concrete source tag-union type"),
                };
            }
        }

        fn concreteTypeInfosForRefs(
            self: *Self,
            refs: []const ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error![]ConcreteTypeInfo {
            if (refs.len == 0) return &.{};
            const out = try self.allocator.alloc(ConcreteTypeInfo, refs.len);
            errdefer self.allocator.free(out);
            for (refs, 0..) |source_ref, i| {
                out[i] = try self.runtimeConcreteTypeInfo(source_ref);
            }
            return out;
        }

        fn concreteTypeInfoForRef(
            self: *Self,
            source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!ConcreteTypeInfo {
            return try self.runtimeConcreteTypeInfo(source_ref);
        }

        fn recordFieldIndex(
            self: *Self,
            record_ty: Type.TypeId,
            field_name: canonical.RecordFieldLabelId,
        ) u16 {
            return switch (self.program.types.getType(record_ty)) {
                .record => |record| {
                    for (record.fields, 0..) |field, i| {
                        if (field.name == field_name) return @intCast(i);
                    }
                    invariantViolation("mono body lowering could not find field in resolved record type");
                },
                else => invariantViolation("mono body lowering expected a resolved record type"),
            };
        }

        fn lowerExprSpan(
            self: *Self,
            exprs: []const checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.Span(Ast.ExprId) {
            if (exprs.len == 0) return Ast.Span(Ast.ExprId).empty();
            const lowered = try self.allocator.alloc(Ast.ExprId, exprs.len);
            defer self.allocator.free(lowered);
            for (exprs, 0..) |expr, i| {
                lowered[i] = try self.lowerExpr(expr);
            }
            return try self.program.ast.addExprSpan(lowered);
        }

        fn lowerExprSpanConcrete(
            self: *Self,
            exprs: []const checked_artifact.CheckedExprId,
            expected_types: []const ConcreteTypeInfo,
        ) Allocator.Error!Ast.Span(Ast.ExprId) {
            if (exprs.len != expected_types.len) invariantViolation("mono body lowering expression span arity did not match concrete expected types");
            if (exprs.len == 0) return Ast.Span(Ast.ExprId).empty();
            const lowered = try self.allocator.alloc(Ast.ExprId, exprs.len);
            defer self.allocator.free(lowered);
            for (exprs, expected_types, 0..) |expr, expected_ty, i| {
                lowered[i] = try self.lowerExprConcreteExpected(expr, expected_ty);
            }
            return try self.program.ast.addExprSpan(lowered);
        }

        fn lowerExprSpanSameConcrete(
            self: *Self,
            exprs: []const checked_artifact.CheckedExprId,
            expected_ty: ConcreteTypeInfo,
        ) Allocator.Error!Ast.Span(Ast.ExprId) {
            if (exprs.len == 0) return Ast.Span(Ast.ExprId).empty();
            const lowered = try self.allocator.alloc(Ast.ExprId, exprs.len);
            defer self.allocator.free(lowered);
            for (exprs, 0..) |expr, i| {
                lowered[i] = try self.lowerExprConcreteExpected(expr, expected_ty);
            }
            return try self.program.ast.addExprSpan(lowered);
        }

        fn lowerStmt(
            self: *Self,
            statement_id: checked_artifact.CheckedStatementId,
        ) Allocator.Error!Ast.StmtId {
            const statement = self.checkedStatement(statement_id);
            return switch (statement.data) {
                .decl => |decl| blk: {
                    const body_ty = self.concreteTypeForPatternBinder(decl.pattern) orelse
                        try self.concreteResultTypeForExpr(decl.expr);
                    const bind = try self.lowerParamPatternWithType(decl.pattern, body_ty);
                    if (try self.lowerLocalFunctionDecl(bind, decl.expr)) |local_fn| {
                        break :blk try self.program.ast.addStmt(.{ .local_fn = local_fn });
                    }
                    const body = try self.lowerExprConcreteExpected(decl.expr, body_ty);
                    break :blk try self.program.ast.addStmt(.{ .decl = .{ .bind = bind, .body = body } });
                },
                .var_ => |var_| blk: {
                    const bind = try self.lowerParamPattern(var_.pattern);
                    const body = try self.lowerExprExpected(var_.expr, self.checkedPattern(var_.pattern).ty);
                    break :blk try self.program.ast.addStmt(.{ .var_decl = .{ .bind = bind, .body = body } });
                },
                .reassign => |reassign| blk: {
                    const bind = try self.lowerParamPattern(reassign.pattern);
                    const body = try self.lowerExprExpected(reassign.expr, self.checkedPattern(reassign.pattern).ty);
                    break :blk try self.program.ast.addStmt(.{ .reassign = .{
                        .target = bind.symbol,
                        .body = body,
                    } });
                },
                .dbg => |expr| try self.program.ast.addStmt(.{ .expr = try self.lowerDbgExpression(try self.ensureUnitType(), expr) }),
                .expr => |expr| try self.program.ast.addStmt(.{ .expr = try self.lowerExpr(expr) }),
                .expect => |expr| try self.program.ast.addStmt(.{ .expect = try self.lowerBoolConditionExpr(expr) }),
                .crash => |literal| try self.program.ast.addStmt(.{ .crash = try self.lowerCheckedStringLiteral(literal) }),
                .return_ => |ret| try self.program.ast.addStmt(.{ .return_ = try self.lowerReturnValue(ret.expr) }),
                .break_ => try self.program.ast.addStmt(.break_),
                .for_ => |for_| try self.lowerForStmt(
                    for_.plan orelse invariantViolation("checked for statement reached mono without an iterator-for plan"),
                    for_.pattern,
                    for_.body,
                ),
                .while_ => |while_| try self.program.ast.addStmt(.{ .while_ = .{
                    .cond = try self.lowerBoolConditionExpr(while_.cond),
                    .body = try self.lowerExpr(while_.body),
                } }),
                .import_,
                .alias_decl,
                .nominal_decl,
                .type_anno,
                .type_var_alias,
                .runtime_error,
                .pending,
                => invariantViolation("mono body lowering received a non-runtime checked statement form"),
            };
        }

        fn lowerForStmt(
            self: *Self,
            plan_id: checked_artifact.IteratorForPlanId,
            pattern: checked_artifact.CheckedPatternId,
            body: checked_artifact.CheckedExprId,
        ) Allocator.Error!Ast.StmtId {
            return try self.program.ast.addStmt(.{
                .expr = try self.lowerForExpr(
                    try self.ensureUnitType(),
                    plan_id,
                    pattern,
                    body,
                ),
            });
        }

        fn lowerLocalFunctionDecl(
            self: *Self,
            bind: Ast.TypedSymbol,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?Ast.LetFn {
            const expr = self.checkedExpr(expr_id);
            const LocalFunctionShape = struct {
                args: []const checked_artifact.CheckedPatternId,
                body: checked_artifact.CheckedExprId,
            };
            const shape: LocalFunctionShape = switch (expr.data) {
                .lambda => |lambda| .{
                    .args = lambda.args,
                    .body = lambda.body,
                },
                .closure => |closure| blk: {
                    const lambda_expr = self.checkedExpr(closure.lambda);
                    switch (lambda_expr.data) {
                        .lambda => |lambda| break :blk .{
                            .args = lambda.args,
                            .body = lambda.body,
                        },
                        else => invariantViolation("mono body lowering expected local closure declaration to reference a checked lambda"),
                    }
                },
                else => return null,
            };
            const body_instance = self.graph.closure_instances.get(self.scopedExpr(expr_id)) orelse {
                invariantViolation("mono body emission reached local function declaration before graph finalization published its body instance");
            };
            const instance = self.graph.body_instances.get(body_instance) orelse {
                invariantViolation("mono body emission reached local function declaration with missing finalized body instance");
            };
            const closure = switch (instance.kind) {
                .closure_value => |closure| closure,
                .root => invariantViolation("mono body emission attempted to emit root body as local function"),
                .local_proc => invariantViolation("mono body emission attempted to emit local procedure body as local function"),
            };
            if (closure.expr != expr_id or closure.body != shape.body or closure.args.len != shape.args.len) {
                invariantViolation("mono body emission local function declaration disagreed with finalized body instance");
            }

            const previous_body = self.current_body;
            self.current_body = body_instance;
            defer self.current_body = previous_body;

            const previous_return_type = self.current_return_type;
            const previous_return_source_ref = self.current_return_source_ref;
            self.current_return_type = instance.ret_ty;
            self.current_return_source_ref = instance.ret_ty.source_ref;
            defer self.current_return_type = previous_return_type;
            defer self.current_return_source_ref = previous_return_source_ref;

            const params = try self.lowerParamBundleFromFunction(shape.args, instance.source_fn_ty_payload);
            defer self.deinitParamBundle(params);
            return .{
                .site = closure.site,
                .source_fn_ty = instance.source_fn_ty,
                .source_fn_ty_payload = instance.source_fn_ty_payload,
                .recursive = false,
                .bind = bind,
                .args = params.args,
                .body = try self.lowerBodyWithParamSetup(shape.body, instance.ret_ty, params),
            };
        }

        fn lowerCheckedStringLiteral(
            self: *Self,
            literal: checked_artifact.CheckedStringLiteralId,
        ) Allocator.Error!mir_ids.ProgramLiteralId {
            return try self.program.literal_pool.intern(self.checkedStringLiteral(literal));
        }

        fn recordFieldLabel(
            self: *Self,
            label: canonical.RecordFieldLabelId,
        ) Allocator.Error!canonical.RecordFieldLabelId {
            return try self.name_resolver.recordFieldLabel(self.template_lookup.artifact, label);
        }

        fn tagLabel(
            self: *Self,
            label: canonical.TagLabelId,
        ) Allocator.Error!canonical.TagLabelId {
            return try self.name_resolver.tagLabel(self.template_lookup.artifact, label);
        }

        fn methodName(
            self: *Self,
            method: canonical.MethodNameId,
        ) Allocator.Error!canonical.MethodNameId {
            return try self.name_resolver.methodName(self.template_lookup.artifact, method);
        }

        fn toInspectMethodName(self: *Self) Allocator.Error!canonical.MethodNameId {
            return try self.program.canonical_names.internMethodName("to_inspect");
        }

        fn recordUpdateHasRemappedField(
            self: *Self,
            fields: []const checked_artifact.CheckedRecordExprField,
            label: canonical.RecordFieldLabelId,
        ) Allocator.Error!bool {
            for (fields) |field| {
                if ((try self.recordFieldLabel(field.label)) == label) return true;
            }
            return false;
        }

        fn checkedStringLiteral(
            self: *Self,
            literal: checked_artifact.CheckedStringLiteralId,
        ) []const u8 {
            const raw = @intFromEnum(literal);
            if (raw >= self.template_lookup.checked_bodies.string_literals.len) {
                invariantViolation("mono body lowering received a checked string literal outside the owning checked body store");
            }
            return self.template_lookup.checked_bodies.string_literals[raw];
        }

        fn checkedBody(self: *const Self, id: checked_artifact.CheckedBodyId) checked_artifact.CheckedBody {
            const raw = @intFromEnum(id);
            if (raw >= self.template_lookup.checked_bodies.bodies.len) invariantViolation("mono body lowering received body id outside checked body store");
            return self.template_lookup.checked_bodies.bodies[raw];
        }

        fn templateCheckedTypes(self: *const Self) checked_artifact.CheckedTypeStoreView {
            return checkedTypesForKey(self.input, self.template_lookup.artifact) orelse {
                invariantViolation("mono body lowering template artifact checked types were unavailable");
            };
        }

        fn concretePayload(
            self: *const Self,
            ref: ConcreteSourceType.ConcreteSourceTypeRef,
        ) checked_artifact.CheckedTypePayload {
            const root = self.program.concrete_source_types.root(ref);
            return switch (root.source) {
                .artifact => |artifact_ref| blk: {
                    const checked_types = checkedTypesForKey(self.input, artifact_ref.artifact) orelse {
                        debug.invariant(false, "mono invariant violated: concrete source type artifact was not available");
                        unreachable;
                    };
                    const raw = @intFromEnum(artifact_ref.ty);
                    if (raw >= checked_types.payloads.len) invariantViolation("mono concrete source type id was outside published payloads");
                    break :blk checked_types.payloads[raw];
                },
                .local => |local| blk: {
                    const local_view = self.program.concrete_source_types.localView();
                    const raw = @intFromEnum(local);
                    if (raw >= local_view.payloads.len) invariantViolation("mono local concrete source type id was outside payloads");
                    break :blk local_view.payloads[raw];
                },
            };
        }

        fn recordFieldNameForConcreteRef(
            self: *Self,
            ref: ConcreteSourceType.ConcreteSourceTypeRef,
            name: canonical.RecordFieldLabelId,
        ) Allocator.Error!canonical.RecordFieldLabelId {
            const root = self.program.concrete_source_types.root(ref);
            return switch (root.source) {
                .artifact => |artifact_ref| try self.name_resolver.recordFieldLabel(artifact_ref.artifact, name),
                .local => name,
            };
        }

        fn tagNameForConcreteRef(
            self: *Self,
            ref: ConcreteSourceType.ConcreteSourceTypeRef,
            name: canonical.TagLabelId,
        ) Allocator.Error!canonical.TagLabelId {
            const root = self.program.concrete_source_types.root(ref);
            return switch (root.source) {
                .artifact => |artifact_ref| try self.name_resolver.tagLabel(artifact_ref.artifact, name),
                .local => name,
            };
        }

        fn findConcreteTag(
            self: *Self,
            concrete: ConcreteSourceType.ConcreteSourceTypeRef,
            tags: []const checked_artifact.CheckedTag,
            name: canonical.TagLabelId,
        ) Allocator.Error!?checked_artifact.CheckedTag {
            for (tags) |tag| {
                if ((try self.tagNameForConcreteRef(concrete, tag.name)) == name) return tag;
            }
            return null;
        }

        fn checkedExpr(self: *const Self, id: checked_artifact.CheckedExprId) checked_artifact.CheckedExpr {
            const raw = @intFromEnum(id);
            if (raw >= self.template_lookup.checked_bodies.exprs.len) invariantViolation("mono body lowering received expr id outside checked body store");
            return self.template_lookup.checked_bodies.exprs[raw];
        }

        fn checkedPattern(self: *const Self, id: checked_artifact.CheckedPatternId) checked_artifact.CheckedPattern {
            const raw = @intFromEnum(id);
            if (raw >= self.template_lookup.checked_bodies.patterns.len) invariantViolation("mono body lowering received pattern id outside checked body store");
            return self.template_lookup.checked_bodies.patterns[raw];
        }

        fn patternBinderIsReassignable(self: *const Self, binder: checked_artifact.PatternBinderId) bool {
            const raw = @intFromEnum(binder);
            if (raw >= self.template_lookup.checked_bodies.pattern_binders.len) {
                invariantViolation("mono body lowering received pattern binder id outside checked body store");
            }
            return self.template_lookup.checked_bodies.pattern_binders[raw].reassignable;
        }

        fn checkedStatement(self: *const Self, id: checked_artifact.CheckedStatementId) checked_artifact.CheckedStatement {
            const raw = @intFromEnum(id);
            if (raw >= self.template_lookup.checked_bodies.statements.len) invariantViolation("mono body lowering received statement id outside checked body store");
            return self.template_lookup.checked_bodies.statements[raw];
        }

        fn staticDispatchPlan(self: *const Self, id: checked_artifact.StaticDispatchPlanId) static_dispatch.StaticDispatchCallPlan {
            const table = staticDispatchPlansForKey(self.input, self.template_lookup.artifact) orelse {
                debug.invariant(false, "mono body lowering invariant violated: static dispatch plan artifact was not available");
                unreachable;
            };
            const raw = @intFromEnum(id);
            if (raw >= table.plans.len) invariantViolation("mono body lowering received static dispatch plan id outside table");
            return table.plans[raw];
        }

        fn iteratorForPlan(self: *const Self, id: checked_artifact.IteratorForPlanId) static_dispatch.IteratorForPlan {
            const table = staticDispatchPlansForKey(self.input, self.template_lookup.artifact) orelse {
                debug.invariant(false, "mono body lowering invariant violated: static dispatch plan artifact was not available");
                unreachable;
            };
            const raw = @intFromEnum(id);
            if (raw >= table.iterator_for_plans.len) invariantViolation("mono body lowering received iterator-for plan id outside table");
            return table.iterator_for_plans[raw];
        }

        fn resolvedValueRef(self: *const Self, id: checked_artifact.ResolvedValueRefId) checked_artifact.ResolvedValueRefRecord {
            const raw = @intFromEnum(id);
            if (raw >= self.template_lookup.resolved_value_refs.records.len) invariantViolation("mono body lowering received resolved value ref id outside table");
            return self.template_lookup.resolved_value_refs.records[raw];
        }

        fn nestedProcSite(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
            kind: checked_artifact.NestedProcKind,
        ) canonical.NestedProcSiteId {
            const refs = self.template_lookup.nested_proc_sites.template_refs;
            const start = self.template_lookup.template.nested_proc_sites.start;
            const len = self.template_lookup.template.nested_proc_sites.len;
            if (start + len > refs.len) invariantViolation("mono body lowering received nested procedure site ref outside table");
            for (refs[start..][0..len]) |site_id| {
                const raw = @intFromEnum(site_id);
                if (raw >= self.template_lookup.nested_proc_sites.sites.len) invariantViolation("mono body lowering received nested procedure site outside table");
                const site = self.template_lookup.nested_proc_sites.sites[raw];
                if (site.kind == kind and site.checked_expr != null and site.checked_expr.? == expr_id) return site_id;
            }
            invariantViolation("mono body lowering could not find published nested procedure site for closure/local function");
        }

        fn procedureUseForExpr(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) ?checked_artifact.ProcedureUseTemplate {
            const expr = self.checkedExpr(expr_id);
            const ref_id = switch (expr.data) {
                .lookup_local => |lookup| lookup.resolved orelse return null,
                .lookup_external => |maybe_ref| maybe_ref orelse return null,
                .lookup_required => |maybe_ref| maybe_ref orelse return null,
                else => return null,
            };
            const record = self.resolvedValueRef(ref_id);
            return switch (record.ref) {
                .top_level_proc,
                .imported_proc,
                .hosted_proc,
                .promoted_top_level_proc,
                => |proc_use| proc_use,
                .platform_required_proc => |required| required.procedure,
                else => null,
            };
        }

        fn callSourceFnPayload(
            self: *const Self,
            func_expr: checked_artifact.CheckedExprId,
            checked_call_fn_ty: checked_artifact.CheckedTypeId,
        ) checked_artifact.CheckedTypeId {
            const proc_use = self.procedureUseForExpr(func_expr) orelse return checked_call_fn_ty;
            return proc_use.source_fn_ty_payload orelse invariantViolation("mono body lowering reached a procedure call without a published source function payload");
        }

        fn localProcUseForExpr(
            self: *const Self,
            expr_id: checked_artifact.CheckedExprId,
        ) Allocator.Error!?struct { binder: checked_artifact.PatternBinderId } {
            const expr = self.checkedExpr(expr_id);
            const ref_id = switch (expr.data) {
                .lookup_local => |lookup| lookup.resolved orelse return null,
                else => return null,
            };
            const record = self.resolvedValueRef(ref_id);
            return switch (record.ref) {
                .local_proc => |local| .{ .binder = local.binder },
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                => |local| if (self.local_proc_decls.contains(local.binder))
                    .{ .binder = local.binder }
                else
                    null,
                else => null,
            };
        }

        fn summaryPendingLocalRootForProcedureUse(
            self: *Self,
            use: checked_artifact.ProcedureUseTemplate,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!?checked_artifact.ComptimeRootId {
            if (self.input.mode != .comptime_dependency_summary) return null;
            if (!std.mem.eql(u8, &self.template_lookup.artifact.bytes, &self.input.root.artifact.key.bytes)) return null;

            const binding_ref = switch (use.binding) {
                .top_level => |binding| binding,
                else => return null,
            };
            if (!std.mem.eql(u8, &binding_ref.artifact.bytes, &self.input.root.artifact.key.bytes)) return null;
            const binding = self.input.root.artifact.top_level_procedure_bindings.get(binding_ref.binding);
            switch (binding.body) {
                .direct_template => return null,
                .callable_eval_template => {},
            }

            const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
            const key = checked_artifact.CallableBindingInstantiationKey{
                .binding = .{ .top_level = binding_ref },
                .requested_source_fn_ty = requested_key,
            };
            if (callableBindingInstanceForKey(self.input, self.input.root.artifact.key, key) != null) {
                return null;
            }

            const root = self.localCallableRootForBinding(binding_ref.binding);
            if (!self.localCompileTimeRootHasRequest(root, .compile_time_callable)) return null;
            return root;
        }

        fn summaryPendingCallableBindingInstanceForProcedureUse(
            self: *Self,
            use: checked_artifact.ProcedureUseTemplate,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!?checked_artifact.CallableBindingInstantiationRequest {
            if (self.input.mode != .comptime_dependency_summary) return null;

            const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
            const key = self.callableBindingInstantiationKeyForProcedureUse(use, requested_key) orelse return null;
            if (callableBindingInstanceForKey(self.input, self.input.root.artifact.key, key) != null) {
                return null;
            }
            const requested_payload = if (mode == .graph_builder)
                try self.finalizeSummaryPayloadForConcreteType(
                    requested_fn_ty,
                    requested_key,
                    "mono dependency-summary callable request payload key disagrees with requested function type",
                )
            else
                self.finalizedSummaryPayload(requested_fn_ty);
            return .{
                .key = key,
                .requested_source_fn_ty_payload = requested_payload,
            };
        }

        fn callableBindingInstantiationKeyForProcedureUse(
            self: *Self,
            use: checked_artifact.ProcedureUseTemplate,
            requested_key: canonical.CanonicalTypeKey,
        ) ?checked_artifact.CallableBindingInstantiationKey {
            const binding_key: checked_artifact.ProcedureBindingRef = switch (use.binding) {
                .top_level => |binding_ref| blk: {
                    const bindings = topLevelProcedureBindingsForKey(self.input, binding_ref.artifact) orelse {
                        debug.invariant(false, "mono dependency-summary lowering found top-level procedure binding for unavailable artifact");
                        unreachable;
                    };
                    switch (bindings.get(binding_ref.binding).body) {
                        .direct_template => return null,
                        .callable_eval_template => {},
                    }
                    break :blk .{ .top_level = binding_ref };
                },
                .imported => |imported| blk: {
                    const binding = importedProcedureBindingViewForRef(self.input, imported) orelse {
                        debug.invariant(false, "mono dependency-summary lowering found imported procedure binding with no published view");
                        unreachable;
                    };
                    switch (binding.body) {
                        .direct_template => return null,
                        .callable_eval_template => {},
                    }
                    break :blk .{ .imported = imported };
                },
                .platform_required => |required| blk: {
                    const bindings = topLevelProcedureBindingsForKey(self.input, required.artifact) orelse {
                        debug.invariant(false, "mono dependency-summary lowering found platform-required procedure binding for unavailable app artifact");
                        unreachable;
                    };
                    switch (bindings.get(required.procedure_binding).body) {
                        .direct_template => return null,
                        .callable_eval_template => {},
                    }
                    break :blk .{ .platform_required = required };
                },
                .hosted,
                .promoted,
                => return null,
            };
            return .{
                .binding = binding_key,
                .requested_source_fn_ty = requested_key,
            };
        }

        fn localCallableRootForBinding(
            self: *const Self,
            binding_ref: checked_artifact.TopLevelProcedureBindingRef,
        ) checked_artifact.ComptimeRootId {
            for (self.input.root.artifact.top_level_values.entries) |entry| {
                const candidate = switch (entry.value) {
                    .procedure_binding => |candidate| candidate,
                    .const_ref => continue,
                };
                if (candidate != binding_ref) continue;

                const root = self.input.root.artifact.compile_time_roots.lookupIdByPattern(entry.pattern) orelse {
                    debug.invariant(false, "mono dependency-summary lowering found a callable-eval binding with no local compile-time root");
                    unreachable;
                };
                if (self.input.root.artifact.compile_time_roots.root(root).kind != .callable_binding) {
                    debug.invariant(false, "mono dependency-summary lowering mapped callable-eval binding to a non-callable local root");
                    unreachable;
                }
                return root;
            }

            debug.invariant(false, "mono dependency-summary lowering found a callable-eval binding with no top-level value entry");
            unreachable;
        }

        fn localCompileTimeRootHasRequest(
            self: *const Self,
            root_id: checked_artifact.ComptimeRootId,
            kind: checked_artifact.RootRequestKind,
        ) bool {
            const root = self.input.root.artifact.compile_time_roots.root(root_id);
            for (self.input.root.artifact.root_requests.requests) |request| {
                if (request.abi != .compile_time) continue;
                if (request.kind != kind) continue;
                if (rootSourcesEqual(root.source, request.source)) return true;
            }
            return false;
        }

        fn rootSourcesEqual(a: checked_artifact.RootSource, b: checked_artifact.RootSource) bool {
            if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
            return switch (a) {
                .def => |left| left == b.def,
                .expr => |left| left == b.expr,
                .statement => |left| left == b.statement,
                .required_binding => |left| left == b.required_binding,
            };
        }

        fn lowerPendingLocalRootCall(
            self: *Self,
            expected: ConcreteTypeInfo,
            root: checked_artifact.ComptimeRootId,
            args: Ast.Span(Ast.ExprId),
        ) Allocator.Error!Ast.ExprId {
            const pending = try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .pending_local_root = root });
            const arg_exprs = self.program.ast.sliceExprSpan(args);
            if (arg_exprs.len == 0) return pending;

            const stmts = try self.allocator.alloc(Ast.StmtId, arg_exprs.len);
            defer self.allocator.free(stmts);
            for (arg_exprs, 0..) |arg, i| {
                stmts[i] = try self.program.ast.addStmt(.{ .expr = arg });
            }
            return try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(stmts),
                .final_expr = pending,
            } });
        }

        fn lowerPendingCallableInstanceCall(
            self: *Self,
            expected: ConcreteTypeInfo,
            request: checked_artifact.CallableBindingInstantiationRequest,
            args: Ast.Span(Ast.ExprId),
        ) Allocator.Error!Ast.ExprId {
            const pending = try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .pending_callable_instance = request });
            const arg_exprs = self.program.ast.sliceExprSpan(args);
            if (arg_exprs.len == 0) return pending;

            const stmts = try self.allocator.alloc(Ast.StmtId, arg_exprs.len);
            defer self.allocator.free(stmts);
            for (arg_exprs, 0..) |arg, i| {
                stmts[i] = try self.program.ast.addStmt(.{ .expr = arg });
            }
            return try self.program.ast.addExprWithSourcePayload(expected.ty, expected.source_ty, expected.source_ref, .{ .block = .{
                .stmts = try self.program.ast.addStmtSpan(stmts),
                .final_expr = pending,
            } });
        }

        fn reserveProcedureUseForConcrete(
            self: *Self,
            use: checked_artifact.ProcedureUseTemplate,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
            reason: MonoSpecializationReason,
        ) Allocator.Error!canonical.MirProcedureRef {
            const callable = try self.name_resolver.procedureCallableRef(try self.procedureCallableForUse(use, requested_fn_ty));
            const template = checkedTemplateFromCallableTemplate(callable.template);
            const imported_closure = self.importedClosureForProcedureUse(use, template);
            const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                .template = template,
                .callable_template = callable.template,
                .requested_fn_ty = requested_fn_ty,
                .reason = reason,
                .imported_closure = imported_closure,
                .allow_return_widening = std.meta.activeTag(use.binding) == .platform_required,
            });
            return .{
                .proc = reserved.proc.proc,
                .callable = callable,
            };
        }

        fn importedClosureForProcedureUse(
            self: *const Self,
            use: checked_artifact.ProcedureUseTemplate,
            template: canonical.ProcedureTemplateRef,
        ) ?checked_artifact.ImportedTemplateClosureView {
            if (self.template_lookup.imported_closure) |closure| {
                if (importedClosureContainsProcedureTemplate(closure, template)) return closure;
            }

            return switch (use.binding) {
                .imported => |imported| self.importedClosureForImportedBinding(imported, template),
                .platform_required => |required| self.importedClosureForPlatformRequiredBinding(required, template),
                else => null,
            };
        }

        fn importedClosureForPlatformRequiredBinding(
            self: *const Self,
            required: checked_artifact.RequiredAppProcedureRef,
            template: canonical.ProcedureTemplateRef,
        ) ?checked_artifact.ImportedTemplateClosureView {
            for (self.input.root.artifact.platform_required_bindings.bindings) |binding| {
                const proc_use = switch (binding.value_use) {
                    .procedure_value => |procedure| procedure,
                    .const_value => continue,
                };
                if (!checked_artifact.procedureBindingRefEql(
                    proc_use.procedure.binding,
                    .{ .platform_required = required },
                )) continue;
                if (importedClosureContainsProcedureTemplate(proc_use.relation_template_closure, template)) {
                    return proc_use.relation_template_closure;
                }
            }
            return null;
        }

        fn importedClosureForImportedBinding(
            self: *const Self,
            imported: checked_artifact.ImportedProcedureBindingRef,
            template: canonical.ProcedureTemplateRef,
        ) ?checked_artifact.ImportedTemplateClosureView {
            for (self.input.imports) |view| {
                if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and
                        binding.binding.pattern == imported.pattern and
                        importedClosureContainsProcedureTemplate(binding.template_closure, template))
                    {
                        return binding.template_closure;
                    }
                }
            }
            for (self.input.root.relation_artifacts) |view| {
                if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and
                        binding.binding.pattern == imported.pattern and
                        importedClosureContainsProcedureTemplate(binding.template_closure, template))
                    {
                        return binding.template_closure;
                    }
                }
            }
            return null;
        }

        fn procedureCallableForUse(
            self: *Self,
            use: checked_artifact.ProcedureUseTemplate,
            requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
        ) Allocator.Error!canonical.ProcedureCallableRef {
            const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
            return switch (use.binding) {
                .top_level => |binding_ref| try self.callableFromTopLevelBinding(
                    binding_ref.artifact,
                    topLevelProcedureBindingsForKey(self.input, binding_ref.artifact) orelse {
                        debug.invariant(false, "mono body lowering invariant violated: callable artifact has no top-level procedure binding table");
                        unreachable;
                    },
                    binding_ref.binding,
                    .{ .top_level = binding_ref },
                    requested_key,
                ),
                .imported => |imported| try self.callableFromImportedProcedureBinding(imported, requested_key),
                .hosted => |hosted| .{
                    .template = .{ .checked = hosted.template },
                    .source_fn_ty = requested_key,
                },
                .platform_required => |required| try self.callableFromTopLevelBinding(
                    required.artifact,
                    topLevelProcedureBindingsForKey(self.input, required.artifact) orelse {
                        debug.invariant(false, "mono body lowering invariant violated: platform-required artifact has no procedure binding table");
                        unreachable;
                    },
                    required.procedure_binding,
                    .{ .platform_required = required },
                    requested_key,
                ),
                .promoted => |promoted| blk: {
                    const promoted_record = self.promotedProcedureForRef(promoted);
                    break :blk .{
                        .template = .{ .synthetic = .{ .template = promoted_record.template } },
                        .source_fn_ty = requested_key,
                    };
                },
            };
        }

        fn promotedProcedureForRef(
            self: *const Self,
            promoted: checked_artifact.PromotedProcedureRef,
        ) checked_artifact.PromotedProcedure {
            if (self.input.root.artifact.module_identity.module_idx == promoted.module_idx) {
                if (self.input.root.artifact.promoted_procedures.get(promoted)) |procedure| return procedure;
            }
            for (self.input.imports) |view| {
                if (view.module_identity.module_idx != promoted.module_idx) continue;
                if (view.promoted_procedures.get(promoted)) |procedure| return procedure;
            }
            for (self.input.root.relation_artifacts) |view| {
                if (view.module_identity.module_idx != promoted.module_idx) continue;
                if (view.promoted_procedures.get(promoted)) |procedure| return procedure;
            }
            invariantViolation("mono body lowering could not find promoted procedure in published artifact views");
        }

        fn callableFromTopLevelBinding(
            self: *Self,
            _: checked_artifact.CheckedModuleArtifactKey,
            bindings: *const checked_artifact.TopLevelProcedureBindingTable,
            binding_ref: checked_artifact.TopLevelProcedureBindingRef,
            binding_key: checked_artifact.ProcedureBindingRef,
            requested_key: canonical.CanonicalTypeKey,
        ) Allocator.Error!canonical.ProcedureCallableRef {
            const binding = bindings.get(binding_ref);
            return switch (binding.body) {
                .direct_template => |direct| .{
                    .template = direct.template,
                    .source_fn_ty = requested_key,
                },
                .callable_eval_template => try self.callableFromCallableBindingInstance(self.input.root.artifact.key, binding_key, requested_key),
            };
        }

        fn callableFromImportedProcedureBinding(
            self: *Self,
            imported: checked_artifact.ImportedProcedureBindingRef,
            requested_key: canonical.CanonicalTypeKey,
        ) Allocator.Error!canonical.ProcedureCallableRef {
            for (self.input.imports) |view| {
                if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and
                        binding.binding.pattern == imported.pattern)
                    {
                        return switch (binding.body) {
                            .direct_template => |direct| .{
                                .template = direct.template,
                                .source_fn_ty = requested_key,
                            },
                            .callable_eval_template => try self.callableFromCallableBindingInstance(
                                self.input.root.artifact.key,
                                .{ .imported = imported },
                                requested_key,
                            ),
                        };
                    }
                }
            }
            for (self.input.root.relation_artifacts) |view| {
                if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and
                        binding.binding.pattern == imported.pattern)
                    {
                        return switch (binding.body) {
                            .direct_template => |direct| .{
                                .template = direct.template,
                                .source_fn_ty = requested_key,
                            },
                            .callable_eval_template => try self.callableFromCallableBindingInstance(
                                self.input.root.artifact.key,
                                .{ .imported = imported },
                                requested_key,
                            ),
                        };
                    }
                }
            }
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "mono body lowering could not find imported procedure binding in published artifact views: artifact={any} def={d} pattern={d}",
                    .{
                        imported.artifact.bytes,
                        @intFromEnum(imported.def),
                        @intFromEnum(imported.pattern),
                    },
                );
            }
            unreachable;
        }

        fn callableFromCallableBindingInstance(
            self: *Self,
            owner: checked_artifact.CheckedModuleArtifactKey,
            binding: checked_artifact.ProcedureBindingRef,
            requested_key: canonical.CanonicalTypeKey,
        ) Allocator.Error!canonical.ProcedureCallableRef {
            const key = checked_artifact.CallableBindingInstantiationKey{
                .binding = binding,
                .requested_source_fn_ty = requested_key,
            };
            const ref = callableBindingInstanceForKey(self.input, owner, key) orelse {
                debug.invariant(false, "mono body lowering invariant violated: callable-eval procedure binding had no sealed concrete instance for requested function type");
                unreachable;
            };
            var dependency_state = ConcreteDependencyReservationState.init(self.allocator);
            defer dependency_state.deinit();
            try reserveCallableBindingInstanceRefDependencies(self.input, self.program, self.queue, &dependency_state, ref);

            const instance = callableBindingInstanceForRef(self.input, ref);
            if (!std.mem.eql(u8, &instance.proc_value.source_fn_ty.bytes, &requested_key.bytes)) {
                debug.invariant(false, "mono body lowering invariant violated: callable-eval instance source function type disagrees with requested type");
                unreachable;
            }
            return instance.proc_value;
        }
    };
}

const MonoSpecializationGraphBuilder = MonoSpecializationLowering(.graph_builder);
const MonoBodyEmitter = MonoSpecializationLowering(.body_emitter);

const dec_scale: i128 = 1_000_000_000_000_000_000;

fn intValueToScaledDec(value: CIR.IntValue) i128 {
    return switch (value.kind) {
        .i128 => {
            const raw = @as(i128, @bitCast(value.bytes));
            return std.math.mul(i128, raw, dec_scale) catch
                invariantViolation("mono body lowering reached integer literal outside Dec range after type checking");
        },
        .u128 => {
            const raw = @as(u128, @bitCast(value.bytes));
            const max_dec_int: u128 = @intCast(@divTrunc(std.math.maxInt(i128), dec_scale));
            if (raw > max_dec_int) {
                invariantViolation("mono body lowering reached unsigned integer literal outside Dec range after type checking");
            }
            return @as(i128, @intCast(raw)) * dec_scale;
        },
    };
}

fn intValueToF64(value: CIR.IntValue) f64 {
    return switch (value.kind) {
        .i128 => @floatFromInt(@as(i128, @bitCast(value.bytes))),
        .u128 => @floatFromInt(@as(u128, @bitCast(value.bytes))),
    };
}

fn scaledDecToF64(value: i128) f64 {
    return @as(f64, @floatFromInt(value)) / 1_000_000_000_000_000_000.0;
}

fn sourceTyIsEmpty(key: canonical.CanonicalTypeKey) bool {
    for (key.bytes) |byte| {
        if (byte != 0) return false;
    }
    return true;
}

fn invariantViolation(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

fn checkedTemplateFromCallableTemplate(
    template: canonical.CallableProcedureTemplateRef,
) canonical.ProcedureTemplateRef {
    return switch (template) {
        .checked => |checked| checked,
        .synthetic => |synthetic| synthetic.template,
        .lifted => invariantViolation("mono specialization received a lifted procedure template before lifted MIR"),
    };
}

fn deinitExecutableSpecializationKeyForMono(
    allocator: Allocator,
    key: *canonical.ExecutableSpecializationKey,
) void {
    if (key.exec_arg_tys.len > 0) allocator.free(key.exec_arg_tys);
    key.exec_arg_tys = &.{};
}

fn cloneExecutableSpecializationKeyForMono(
    allocator: Allocator,
    key: canonical.ExecutableSpecializationKey,
) Allocator.Error!canonical.ExecutableSpecializationKey {
    return .{
        .base = key.base,
        .requested_fn_ty = key.requested_fn_ty,
        .exec_arg_tys = if (key.exec_arg_tys.len == 0)
            &.{}
        else
            try allocator.dupe(canonical.CanonicalExecValueTypeKey, key.exec_arg_tys),
        .exec_ret_ty = key.exec_ret_ty,
        .callable_repr_mode = key.callable_repr_mode,
        .capture_shape_key = key.capture_shape_key,
    };
}

fn publishedMirProcedureRefForCallable(
    callable: canonical.ProcedureCallableRef,
) ?canonical.MirProcedureRef {
    const template = switch (callable.template) {
        .checked => |checked| checked,
        .synthetic => |synthetic| synthetic.template,
        .lifted => return null,
    };
    return .{
        .proc = .{
            .artifact = template.artifact,
            .proc_base = template.proc_base,
        },
        .callable = callable,
    };
}

fn callableTemplateArtifact(template: canonical.CallableProcedureTemplateRef) canonical.ArtifactRef {
    return switch (template) {
        .checked => |checked| checked.artifact,
        .synthetic => |synthetic| synthetic.template.artifact,
        .lifted => |lifted| lifted.owner_mono_specialization.template.artifact,
    };
}

fn checkedTypesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.CheckedTypeStoreView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) return input.root.artifact.checked_types.view();
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.checked_types;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.checked_types;
    }
    return null;
}

fn interfaceCapabilitiesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.ModuleInterfaceCapabilities {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) return &input.root.artifact.interface_capabilities;
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.interface_capabilities;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.interface_capabilities;
    }
    return null;
}

fn checkedTypeSchemeForKey(
    checked_types: checked_artifact.CheckedTypeStoreView,
    key: canonical.CanonicalTypeSchemeKey,
) ?checked_artifact.CheckedTypeScheme {
    for (checked_types.schemes) |scheme| {
        if (std.mem.eql(u8, &scheme.key.bytes, &key.bytes)) return scheme;
    }
    return null;
}

fn checkedTypeRootForKey(
    checked_types: checked_artifact.CheckedTypeStoreView,
    key: canonical.CanonicalTypeKey,
) ?checked_artifact.CheckedTypeId {
    for (checked_types.roots) |root| {
        if (std.mem.eql(u8, &root.key.bytes, &key.bytes)) return root.id;
    }
    return null;
}

fn checkedTypeKey(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
) canonical.CanonicalTypeKey {
    const index: usize = @intFromEnum(ty);
    if (index >= checked_types.roots.len) {
        invariantViolation("checked type key lookup referenced a missing root");
    }
    return checked_types.roots[index].key;
}

fn checkedTypePayload(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
) checked_artifact.CheckedTypePayload {
    const index: usize = @intFromEnum(ty);
    if (index >= checked_types.payloads.len) {
        invariantViolation("checked type payload lookup referenced a missing payload");
    }
    return checked_types.payloads[index];
}

fn checkedTypeViewIsConcreteConstProducerScheme(
    allocator: Allocator,
    checked_types: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(checked_artifact.CheckedTypeId, void).init(allocator);
    defer active.deinit();
    return try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, root, &active);
}

fn checkedTypeViewIsConcreteConstProducerSchemeInner(
    checked_types: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
    active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
) Allocator.Error!bool {
    if (active.contains(root)) return true;
    try active.put(root, {});
    defer _ = active.remove(root);

    return switch (checkedTypePayload(checked_types, root)) {
        .pending => invariantViolation("mono body lowering const producer checked type was pending"),
        .flex,
        .rigid,
        => false,
        .empty_record,
        .empty_tag_union,
        => true,
        .alias => |alias| (try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, alias.backing, active)) and
            try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, alias.args, active),
        .record => |record| (try checkedTypeViewFieldsAreConcreteConstProducerScheme(checked_types, record.fields, active)) and
            try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, record.ext, active),
        .record_unbound => |fields| checkedTypeViewFieldsAreConcreteConstProducerScheme(checked_types, fields, active),
        .tuple => |items| checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, items, active),
        .nominal => |nominal| blk: {
            if (!try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, nominal.args, active)) break :blk false;
            if (nominal.builtin != null) break :blk true;
            break :blk try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, nominal.backing, active);
        },
        .function => |function| !function.needs_instantiation and
            (try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, function.args, active)) and
            try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, function.ret, active),
        .tag_union => |tag_union| (try checkedTypeViewTagsAreConcreteConstProducerScheme(checked_types, tag_union.tags, active)) and
            try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, tag_union.ext, active),
    };
}

fn checkedTypeViewSpanIsConcreteConstProducerScheme(
    checked_types: checked_artifact.CheckedTypeStoreView,
    items: []const checked_artifact.CheckedTypeId,
    active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
) Allocator.Error!bool {
    for (items) |item| {
        if (!try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, item, active)) return false;
    }
    return true;
}

fn checkedTypeViewFieldsAreConcreteConstProducerScheme(
    checked_types: checked_artifact.CheckedTypeStoreView,
    fields: []const checked_artifact.CheckedRecordField,
    active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
) Allocator.Error!bool {
    for (fields) |field| {
        if (!try checkedTypeViewIsConcreteConstProducerSchemeInner(checked_types, field.ty, active)) return false;
    }
    return true;
}

fn checkedTypeViewTagsAreConcreteConstProducerScheme(
    checked_types: checked_artifact.CheckedTypeStoreView,
    tags: []const checked_artifact.CheckedTag,
    active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
) Allocator.Error!bool {
    for (tags) |tag| {
        if (!try checkedTypeViewSpanIsConcreteConstProducerScheme(checked_types, tag.args, active)) return false;
    }
    return true;
}

fn privateRecordFieldCount(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
) usize {
    return switch (checkedTypePayload(checked_types, ty)) {
        .record => |record| record.fields.len + privateRecordFieldCount(checked_types, record.ext),
        .record_unbound => |fields| fields.len,
        .empty_record => 0,
        .alias => |alias| privateRecordFieldCount(checked_types, alias.backing),
        else => invariantViolation("private capture record node had non-record checked type"),
    };
}

fn privateRecordFieldTypeForType(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
    label: canonical.RecordFieldLabelId,
) ?checked_artifact.CheckedTypeId {
    return switch (checkedTypePayload(checked_types, ty)) {
        .record => |record| {
            for (record.fields) |field| {
                if (field.name == label) return field.ty;
            }
            return privateRecordFieldTypeForType(checked_types, record.ext, label);
        },
        .record_unbound => |fields| {
            for (fields) |field| {
                if (field.name == label) return field.ty;
            }
            return null;
        },
        .empty_record => null,
        .alias => |alias| privateRecordFieldTypeForType(checked_types, alias.backing, label),
        else => invariantViolation("private capture record node had non-record checked type"),
    };
}

fn privateTupleElems(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
) []const checked_artifact.CheckedTypeId {
    return switch (checkedTypePayload(checked_types, ty)) {
        .tuple => |tuple| tuple,
        .alias => |alias| privateTupleElems(checked_types, alias.backing),
        else => invariantViolation("private capture tuple node had non-tuple checked type"),
    };
}

fn privateTagTypeForType(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
    label: canonical.TagLabelId,
) ?checked_artifact.CheckedTag {
    return switch (checkedTypePayload(checked_types, ty)) {
        .tag_union => |tag_union| {
            for (tag_union.tags) |tag| {
                if (tag.name == label) return tag;
            }
            return privateTagTypeForType(checked_types, tag_union.ext, label);
        },
        .empty_tag_union => null,
        .alias => |alias| privateTagTypeForType(checked_types, alias.backing, label),
        else => invariantViolation("private capture tag node had non-tag-union checked type"),
    };
}

fn privateBuiltinArgType(
    checked_types: checked_artifact.CheckedTypeStoreView,
    ty: checked_artifact.CheckedTypeId,
    builtin: checked_artifact.CheckedBuiltinNominal,
) checked_artifact.CheckedTypeId {
    return switch (checkedTypePayload(checked_types, ty)) {
        .alias => |alias| privateBuiltinArgType(checked_types, alias.backing, builtin),
        .nominal => |nominal| blk: {
            if (nominal.builtin == null or nominal.builtin.? != builtin or nominal.args.len != 1) {
                invariantViolation("private capture builtin container node had incompatible checked type");
            }
            break :blk nominal.args[0];
        },
        else => invariantViolation("private capture builtin container node had non-nominal checked type"),
    };
}

fn comptimePlansForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.CompileTimePlanStore {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) return &input.root.artifact.comptime_plans;
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.comptime_plans;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.comptime_plans;
    }
    return null;
}

fn constInstancesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.ConstInstantiationStoreView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) return input.root.artifact.const_instances.view();
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.const_instances;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.const_instances;
    }
    return null;
}

fn comptimeDependenciesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.ComptimeDependencySummaryStoreView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) return input.root.artifact.comptime_dependencies.view();
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.comptime_dependencies;
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.comptime_dependencies;
    }
    return null;
}

fn constInstanceForKey(
    input: Input,
    owner: checked_artifact.CheckedModuleArtifactKey,
    key: checked_artifact.ConstInstantiationKey,
) ?checked_artifact.ConstInstanceRef {
    const view = constInstancesForKey(input, owner) orelse return null;
    for (view.instances) |record| {
        if (!monoConstInstantiationKeyEql(record.key, key)) continue;
        switch (record.state) {
            .evaluated => return .{
                .owner = view.owner,
                .key = key,
                .instance = record.id,
            },
            .reserved, .evaluating => invariantViolation("constant instance was consumed before it was sealed"),
        }
    }
    return null;
}

fn constInstanceForRef(
    input: Input,
    ref: checked_artifact.ConstInstanceRef,
) checked_artifact.ConstInstance {
    const view = constInstancesForKey(input, ref.owner) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: constant instance owner artifact was not available");
        unreachable;
    };
    const idx = @intFromEnum(ref.instance);
    if (idx >= view.instances.len) {
        debug.invariant(false, "mono dependency reservation invariant violated: constant instance id was out of range");
        unreachable;
    }
    const record = view.instances[idx];
    if (record.id != ref.instance or !checked_artifact.constInstantiationKeyEql(record.key, ref.key)) {
        debug.invariant(false, "mono dependency reservation invariant violated: constant instance ref did not match its table row");
        unreachable;
    }
    return switch (record.state) {
        .evaluated => |instance| instance,
        .reserved, .evaluating => invariantViolation("constant instance was consumed before it was sealed"),
    };
}

fn monoConstInstantiationKeyEql(
    a: checked_artifact.ConstInstantiationKey,
    b: checked_artifact.ConstInstantiationKey,
) bool {
    return monoConstRefEql(a.const_ref, b.const_ref) and
        std.mem.eql(u8, &a.requested_source_ty.bytes, &b.requested_source_ty.bytes);
}

fn monoConstRefEql(a: checked_artifact.ConstRef, b: checked_artifact.ConstRef) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        monoConstOwnerEql(a.owner, b.owner) and
        a.template == b.template and
        std.mem.eql(u8, &a.source_scheme.bytes, &b.source_scheme.bytes);
}

fn monoConstOwnerEql(a: checked_artifact.ConstOwner, b: checked_artifact.ConstOwner) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .top_level_binding => |left| blk: {
            const right = b.top_level_binding;
            break :blk left.module_idx == right.module_idx and
                left.pattern == right.pattern;
        },
        .promoted_capture => |left| blk: {
            const right = b.promoted_capture;
            break :blk left.capture_index == right.capture_index and
                left.promoted_proc.module_idx == right.promoted_proc.module_idx and
                canonical.procedureValueRefEql(left.promoted_proc.proc, right.promoted_proc.proc);
        },
    };
}

fn templateForRoot(
    input: Input,
    root: checked_artifact.RootRequest,
    concrete_source_types: *const ConcreteSourceType.Store,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
) ?RootTemplateSelection {
    if (root.procedure_template) |template| return .{ .template = template };

    const artifact = input.root.artifact;
    const requested_key = concrete_source_types.key(requested_fn_ty);
    switch (root.source) {
        .def => |def_idx| {
            if (artifact.checked_procedure_templates.lookupByDef(def_idx)) |template| return .{ .template = template };
            const top_level = artifact.top_level_values.lookupByDef(def_idx) orelse return null;
            return switch (top_level.value) {
                .const_ref => null,
                .procedure_binding => |binding_ref| blk: {
                    const template = templateFromRootTopLevelBinding(
                        input,
                        input.root.artifact.key,
                        &artifact.top_level_procedure_bindings,
                        binding_ref,
                        .{ .top_level = .{
                            .artifact = input.root.artifact.key,
                            .binding = binding_ref,
                        } },
                        requested_key,
                    ) orelse break :blk null;
                    break :blk .{ .template = template };
                },
            };
        },
        .required_binding => |binding_id| {
            const binding = artifact.platform_required_bindings.lookupByBindingId(binding_id) orelse {
                debug.invariantFmt(
                    false,
                    "mono specialization invariant violated: platform-required root {d} has no sealed binding",
                    .{binding_id},
                );
                unreachable;
            };
            return switch (binding.value_use) {
                .const_value => null,
                .procedure_value => |proc_use| blk: {
                    const template = templateForProcedureUse(input, proc_use.procedure, requested_key) orelse break :blk null;
                    break :blk .{
                        .template = template,
                        .imported_closure = proc_use.relation_template_closure,
                    };
                },
            };
        },
        .expr, .statement => return null,
    }
}

fn rootMetadataFromChecked(root: checked_artifact.RootRequest) mir_ids.RootMetadata {
    return .{
        .order = root.order,
        .kind = switch (root.kind) {
            .runtime_entrypoint => .runtime_entrypoint,
            .provided_export => .provided_export,
            .platform_required_binding => .platform_required_binding,
            .hosted_export => .hosted_export,
            .test_expect => .test_expect,
            .repl_expr => .repl_expr,
            .dev_expr => .dev_expr,
            .compile_time_constant => .compile_time_constant,
            .compile_time_callable => .compile_time_callable,
        },
        .abi = switch (root.abi) {
            .roc => .roc,
            .platform => .platform,
            .hosted => .hosted,
            .test_expect => .test_expect,
            .compile_time => .compile_time,
        },
        .exposure = switch (root.exposure) {
            .private => .private,
            .exported => .exported,
            .platform_required => .platform_required,
            .hosted => .hosted,
        },
    };
}

fn templateForProcedureUse(
    input: Input,
    proc_use: checked_artifact.ProcedureUseTemplate,
    requested_key: canonical.CanonicalTypeKey,
) ?canonical.ProcedureTemplateRef {
    return switch (proc_use.binding) {
        .top_level => |binding_ref| templateFromRootTopLevelBinding(
            input,
            input.root.artifact.key,
            topLevelProcedureBindingsForKey(input, binding_ref.artifact) orelse {
                debug.invariant(false, "mono specialization invariant violated: top-level procedure binding references unavailable artifact");
                unreachable;
            },
            binding_ref.binding,
            .{ .top_level = binding_ref },
            requested_key,
        ),
        .platform_required => |required| {
            const bindings = topLevelProcedureBindingsForKey(input, required.artifact) orelse {
                debug.invariant(false, "mono specialization invariant violated: platform-required procedure binding references unavailable app artifact");
                unreachable;
            };
            return templateFromRootTopLevelBinding(
                input,
                input.root.artifact.key,
                bindings,
                required.procedure_binding,
                .{ .platform_required = required },
                requested_key,
            );
        },
        .hosted => |hosted| hosted.template,
        .imported, .promoted => {
            debug.invariant(false, "mono specialization invariant violated: platform-required root resolved to unsupported procedure binding kind");
            unreachable;
        },
    };
}

fn templateFromRootTopLevelBinding(
    input: Input,
    owner: checked_artifact.CheckedModuleArtifactKey,
    bindings: *const checked_artifact.TopLevelProcedureBindingTable,
    binding_ref: checked_artifact.TopLevelProcedureBindingRef,
    binding_key: checked_artifact.ProcedureBindingRef,
    requested_key: canonical.CanonicalTypeKey,
) ?canonical.ProcedureTemplateRef {
    const binding = bindings.get(binding_ref);
    return switch (binding.body) {
        .direct_template => |direct| checkedTemplateFromCallableTemplate(direct.template),
        .callable_eval_template => templateFromCallableBindingInstanceForRoot(input, owner, binding_key, requested_key),
    };
}

fn templateFromCallableBindingInstanceForRoot(
    input: Input,
    owner: checked_artifact.CheckedModuleArtifactKey,
    binding: checked_artifact.ProcedureBindingRef,
    requested_key: canonical.CanonicalTypeKey,
) canonical.ProcedureTemplateRef {
    const store = callableBindingInstancesForKey(input, owner) orelse {
        debug.invariant(false, "mono specialization invariant violated: root callable-eval binding instance owner artifact was not available");
        unreachable;
    };
    const key = checked_artifact.CallableBindingInstantiationKey{
        .binding = binding,
        .requested_source_fn_ty = requested_key,
    };
    for (store.instances) |record| {
        if (!checked_artifact.callableBindingInstantiationKeyEql(record.key, key)) continue;
        const instance = switch (record.state) {
            .evaluated => |evaluated| evaluated,
            .reserved, .evaluating => {
                debug.invariant(false, "mono specialization invariant violated: root callable-eval binding instance was not sealed before lowering");
                unreachable;
            },
        };
        if (!std.mem.eql(u8, &instance.proc_value.source_fn_ty.bytes, &requested_key.bytes)) {
            debug.invariant(false, "mono specialization invariant violated: root callable-eval instance source function type disagrees with requested type");
            unreachable;
        }
        return checkedTemplateFromCallableTemplate(instance.proc_value.template);
    }

    debug.invariant(false, "mono specialization invariant violated: root callable-eval procedure binding had no sealed concrete instance for requested function type");
    unreachable;
}

fn topLevelProcedureBindingsForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.TopLevelProcedureBindingTable {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.top_level_procedure_bindings;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.top_level_procedure_bindings;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.top_level_procedure_bindings;
        }
    }
    return null;
}

fn constTemplatesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.ConstTemplateTable {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.const_templates;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.const_templates;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.const_templates;
        }
    }
    return null;
}

fn callableEvalTemplatesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.CallableEvalTemplateTableView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return input.root.artifact.callable_eval_templates.view();
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.callable_eval_templates;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.callable_eval_templates;
        }
    }
    return null;
}

fn entryWrappersForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.EntryWrapperTable {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.entry_wrappers;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.entry_wrappers;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.entry_wrappers;
        }
    }
    return null;
}

fn callableEvalEntryTemplateForKey(
    input: Input,
    owner: checked_artifact.CheckedModuleArtifactKey,
    template_id: checked_artifact.CallableEvalTemplateId,
) canonical.ProcedureTemplateRef {
    const templates = callableEvalTemplatesForKey(input, owner) orelse {
        debug.invariant(false, "mono specialization invariant violated: callable eval template owner artifact was not available");
        unreachable;
    };
    const raw = @intFromEnum(template_id);
    if (raw >= templates.templates.len) {
        debug.invariant(false, "mono specialization invariant violated: callable eval template id was out of range");
        unreachable;
    }
    const template = templates.templates[raw];
    const entry_wrappers = entryWrappersForKey(input, owner) orelse {
        debug.invariant(false, "mono specialization invariant violated: callable eval template entry wrapper owner artifact was not available");
        unreachable;
    };
    const wrapper = entry_wrappers.lookupByRoot(template.root) orelse {
        debug.invariant(false, "mono specialization invariant violated: callable eval template had no entry wrapper");
        unreachable;
    };
    return wrapper.template;
}

fn importedProcedureBindingViewForRef(
    input: Input,
    binding: checked_artifact.ImportedProcedureBindingRef,
) ?checked_artifact.ImportedProcedureBindingView {
    for (input.imports) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &binding.artifact.bytes)) continue;
        for (imported.exported_procedure_bindings.bindings) |view| {
            if (importedProcedureBindingRefEql(view.binding, binding)) return view;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (!std.mem.eql(u8, &related.key.bytes, &binding.artifact.bytes)) continue;
        for (related.exported_procedure_bindings.bindings) |view| {
            if (importedProcedureBindingRefEql(view.binding, binding)) return view;
        }
    }
    return null;
}

fn importedProcedureBindingRefEql(
    a: checked_artifact.ImportedProcedureBindingRef,
    b: checked_artifact.ImportedProcedureBindingRef,
) bool {
    return std.mem.eql(u8, &a.artifact.bytes, &b.artifact.bytes) and
        a.def == b.def and
        a.pattern == b.pattern;
}

fn callableBindingInstancesForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.CallableBindingInstantiationStoreView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return input.root.artifact.callable_binding_instances.view();
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.callable_binding_instances;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.callable_binding_instances;
        }
    }
    return null;
}

fn callableBindingInstanceForKey(
    input: Input,
    owner: checked_artifact.CheckedModuleArtifactKey,
    key: checked_artifact.CallableBindingInstantiationKey,
) ?checked_artifact.CallableBindingInstanceRef {
    const view = callableBindingInstancesForKey(input, owner) orelse return null;
    for (view.instances) |record| {
        if (!checked_artifact.callableBindingInstantiationKeyEql(record.key, key)) continue;
        switch (record.state) {
            .evaluated => return .{
                .owner = view.owner,
                .key = key,
                .instance = record.id,
            },
            .reserved, .evaluating => invariantViolation("callable binding instance was consumed before it was sealed"),
        }
    }
    return null;
}

fn callableBindingInstanceForRef(
    input: Input,
    ref: checked_artifact.CallableBindingInstanceRef,
) checked_artifact.CallableBindingInstance {
    const record = callableBindingInstanceRecordForRef(input, ref);
    return switch (record.state) {
        .evaluated => |instance| instance,
        .reserved, .evaluating => invariantViolation("callable binding instance was consumed before it was sealed"),
    };
}

fn callableBindingInstanceRecordForRef(
    input: Input,
    ref: checked_artifact.CallableBindingInstanceRef,
) checked_artifact.CallableBindingInstantiationRecord {
    const view = callableBindingInstancesForKey(input, ref.owner) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: callable binding instance owner artifact was not available");
        unreachable;
    };
    const idx = @intFromEnum(ref.instance);
    if (idx >= view.instances.len) {
        debug.invariant(false, "mono dependency reservation invariant violated: callable binding instance id was out of range");
        unreachable;
    }
    const record = view.instances[idx];
    if (record.id != ref.instance or !checked_artifact.callableBindingInstantiationKeyEql(record.key, ref.key)) {
        debug.invariant(false, "mono dependency reservation invariant violated: callable binding instance ref did not match its table row");
        unreachable;
    }
    return record;
}

fn semanticInstantiationProceduresForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.SemanticInstantiationProcedureTableView {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return input.root.artifact.semantic_instantiation_procedures.view();
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.semantic_instantiation_procedures;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.semantic_instantiation_procedures;
        }
    }
    return null;
}

fn semanticInstantiationProcedureSourceTy(
    key: checked_artifact.SemanticInstantiationProcedureKey,
) canonical.CanonicalTypeKey {
    return switch (key) {
        .const_instance_callable_leaf => |leaf| leaf.source_fn_ty,
        .callable_binding_promoted_leaf => |leaf| leaf.source_fn_ty,
        .private_capture_callable_leaf => |leaf| leaf.source_fn_ty,
    };
}

fn semanticInstantiationProcedureSourceTyPayload(
    key: checked_artifact.SemanticInstantiationProcedureKey,
) checked_artifact.CheckedTypeId {
    return switch (key) {
        .const_instance_callable_leaf => |leaf| leaf.source_fn_ty_payload,
        .callable_binding_promoted_leaf => |leaf| leaf.source_fn_ty_payload,
        .private_capture_callable_leaf => |leaf| leaf.source_fn_ty_payload,
    };
}

fn staticDispatchPlansForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const static_dispatch.StaticDispatchPlanTable {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.static_dispatch_plans;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.static_dispatch_plans;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.static_dispatch_plans;
        }
    }
    return null;
}

fn lookupStaticDispatchMethodTarget(
    input: Input,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    owner: static_dispatch.MethodOwner,
    method: canonical.MethodNameId,
) Allocator.Error!?static_dispatch.MethodTarget {
    var found: ?static_dispatch.MethodTarget = null;
    try lookupStaticDispatchMethodTargetInRegistry(
        name_resolver,
        input.root.artifact.key,
        &input.root.artifact.method_registry,
        owner,
        method,
        &found,
    );
    for (input.imports) |imported| {
        try lookupStaticDispatchMethodTargetInRegistry(
            name_resolver,
            imported.key,
            imported.method_registry,
            owner,
            method,
            &found,
        );
    }
    for (input.root.relation_artifacts) |related| {
        try lookupStaticDispatchMethodTargetInRegistry(
            name_resolver,
            related.key,
            related.method_registry,
            owner,
            method,
            &found,
        );
    }
    return found;
}

fn lookupStaticDispatchMethodTargetInRegistry(
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    registry_artifact: checked_artifact.CheckedModuleArtifactKey,
    registry: *const static_dispatch.MethodRegistry,
    owner: static_dispatch.MethodOwner,
    method: canonical.MethodNameId,
    found: *?static_dispatch.MethodTarget,
) Allocator.Error!void {
    for (registry.entries) |entry| {
        const entry_owner = try name_resolver.methodOwner(registry_artifact, entry.key.owner);
        const entry_method = try name_resolver.methodName(registry_artifact, entry.key.method);
        if (!methodOwnerEql(entry_owner, owner) or entry_method != method) continue;
        if (found.*) |existing| {
            if (!methodTargetEql(existing, entry.target)) {
                invariantViolation("mono static dispatch found multiple checked method targets for one owner and method");
            }
            continue;
        }
        found.* = entry.target;
    }
}

fn methodTargetEql(a: static_dispatch.MethodTarget, b: static_dispatch.MethodTarget) bool {
    if (a.module_idx != b.module_idx or a.def_idx != b.def_idx) return false;
    if (!canonical.procedureValueRefEql(a.proc, b.proc)) return false;
    if (a.callable_ty != b.callable_ty) return false;
    return if (a.template) |a_template|
        if (b.template) |b_template| canonical.procedureTemplateRefEql(a_template, b_template) else false
    else
        b.template == null;
}

fn methodOwnerForCheckedBuiltinNominal(
    builtin: checked_artifact.CheckedBuiltinNominal,
) static_dispatch.BuiltinOwner {
    return switch (builtin) {
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
        .list => .list,
        .box => .box,
    };
}

fn methodOwnerEql(a: static_dispatch.MethodOwner, b: static_dispatch.MethodOwner) bool {
    return switch (a) {
        .nominal => |a_nominal| switch (b) {
            .nominal => |b_nominal| a_nominal.module_name == b_nominal.module_name and
                a_nominal.type_name == b_nominal.type_name,
            else => false,
        },
        .builtin => |a_builtin| switch (b) {
            .builtin => |b_builtin| a_builtin == b_builtin,
            else => false,
        },
    };
}

fn verifyProgram(program: *const Program) void {
    if (@import("builtin").mode != .Debug) return;
    std.debug.assert(program.root_procs.items.len == program.root_metadata.items.len);
    for (program.root_procs.items) |root| {
        var found = false;
        for (program.procs.items) |proc| {
            if (canonical.mirProcedureRefEql(proc.proc, root)) {
                found = true;
                break;
            }
        }
        if (!found) {
            for (program.executable_synthetic_procs.items) |proc| {
                if (canonical.mirProcedureRefEql(proc.source_proc, root)) {
                    found = true;
                    break;
                }
            }
        }
        std.debug.assert(found);
    }
}

/// Public `Queue` declaration.
pub const Queue = struct {
    allocator: Allocator,
    requested: std.AutoHashMap(canonical.MonoSpecializationKey, ReservedMonoProc),
    pending: std.ArrayList(canonical.MonoSpecializationKey),

    pub fn init(allocator: Allocator) Queue {
        return .{
            .allocator = allocator,
            .requested = std.AutoHashMap(canonical.MonoSpecializationKey, ReservedMonoProc).init(allocator),
            .pending = .empty,
        };
    }

    pub fn deinit(self: *Queue) void {
        self.pending.deinit(self.allocator);
        self.requested.deinit();
        self.* = Queue.init(self.allocator);
    }

    pub fn reserve(
        self: *Queue,
        concrete_source_types: *const ConcreteSourceType.Store,
        request: MonoSpecializationRequest,
    ) Allocator.Error!ReservedMonoProc {
        const requested_mono_fn_ty = concrete_source_types.key(request.requested_fn_ty);
        const key = canonical.MonoSpecializationKey{
            .template = request.template,
            .requested_mono_fn_ty = requested_mono_fn_ty,
        };
        const callable_template = request.callable_template orelse canonical.CallableProcedureTemplateRef{ .checked = request.template };
        if (self.requested.getPtr(key)) |existing| {
            if (!canonical.callableProcedureTemplateRefEql(existing.callable_template, callable_template)) {
                debug.invariant(false, "mono specialization invariant violated: same specialization key registered with a different callable template identity");
                unreachable;
            }
            if (request.allow_return_widening and !existing.allow_return_widening) {
                if (existing.state != .reserved) {
                    debug.invariant(false, "mono specialization invariant violated: return widening was requested after specialization lowering had started");
                    unreachable;
                }
                existing.allow_return_widening = true;
            }
            if (existing.imported_closure == null and request.imported_closure != null) {
                existing.imported_closure = request.imported_closure;
            }
            return existing.*;
        }

        const reserved = ReservedMonoProc{
            .proc = .{
                .proc = .{ .artifact = request.template.artifact, .proc_base = request.template.proc_base },
                .specialization = key,
            },
            .callable_template = callable_template,
            .local_handle = @enumFromInt(@as(u32, @intCast(self.requested.count()))),
            .requested_fn_ty = request.requested_fn_ty,
            .imported_closure = request.imported_closure,
            .allow_return_widening = request.allow_return_widening,
            .state = .reserved,
        };
        try self.requested.put(key, reserved);
        try self.pending.append(self.allocator, key);
        return reserved;
    }

    pub fn markLowering(self: *Queue, key: canonical.MonoSpecializationKey) void {
        const entry = self.requested.getPtr(key) orelse unreachable;
        switch (entry.state) {
            .reserved => entry.state = .lowering,
            .lowering, .lowered => unreachable,
        }
    }

    pub fn markLowered(self: *Queue, key: canonical.MonoSpecializationKey) void {
        const entry = self.requested.getPtr(key) orelse unreachable;
        switch (entry.state) {
            .lowering => entry.state = .lowered,
            .reserved, .lowered => unreachable,
        }
    }
};

test "mono specialization queue reserves once" {
    var queue = Queue.init(std.testing.allocator);
    defer queue.deinit();
    var concrete = ConcreteSourceType.Store.init(std.testing.allocator);
    defer concrete.deinit();

    const requested_key = canonical.CanonicalTypeKey{ .bytes = [_]u8{1} ** 32 };
    const requested_checked_ty = try concrete.reserveLocalRoot(requested_key);
    const requested_source_ty = try concrete.sealLocalRoot(requested_checked_ty, requested_key);

    const first_proc_base_index: u32 = 0;
    const first_template_index: u32 = 0;
    const template = canonical.ProcedureTemplateRef{
        .proc_base = @enumFromInt(first_proc_base_index),
        .template = @enumFromInt(first_template_index),
    };
    const first_summary_index: u32 = 0;
    const request = MonoSpecializationRequest{
        .template = template,
        .requested_fn_ty = requested_source_ty,
        .reason = .{ .comptime_dependency_summary = @enumFromInt(first_summary_index) },
    };

    const first = try queue.reserve(&concrete, request);
    const second = try queue.reserve(&concrete, request);
    try std.testing.expectEqual(first.local_handle, second.local_handle);
    try std.testing.expectEqual(@as(usize, 1), queue.pending.items.len);
}

test "mono specialization queue accepts equivalent payload refs for one key" {
    var queue = Queue.init(std.testing.allocator);
    defer queue.deinit();
    var concrete = ConcreteSourceType.Store.init(std.testing.allocator);
    defer concrete.deinit();

    const requested_key = canonical.CanonicalTypeKey{ .bytes = [_]u8{2} ** 32 };
    const first_checked_ty = try concrete.reserveLocalRoot(requested_key);
    const first_source_ty = try concrete.sealLocalRoot(first_checked_ty, requested_key);

    const second_checked_ty: checked_artifact.CheckedTypeId = @enumFromInt(@as(u32, @intCast(concrete.local_roots.items.len)));
    try concrete.roots.append(std.testing.allocator, .{
        .key = requested_key,
        .source = .{
            .artifact = .{ .artifact = .{ .bytes = [_]u8{3} ** 32 }, .ty = second_checked_ty },
        },
    });
    const second_source_ty: ConcreteSourceType.ConcreteSourceTypeRef = @enumFromInt(@as(u32, @intCast(concrete.roots.items.len - 1)));

    const first_proc_base_index: u32 = 0;
    const first_template_index: u32 = 0;
    const template = canonical.ProcedureTemplateRef{
        .proc_base = @enumFromInt(first_proc_base_index),
        .template = @enumFromInt(first_template_index),
    };
    const first_summary_index: u32 = 0;
    const first_request = MonoSpecializationRequest{
        .template = template,
        .requested_fn_ty = first_source_ty,
        .reason = .{ .comptime_dependency_summary = @enumFromInt(first_summary_index) },
    };
    const second_request = MonoSpecializationRequest{
        .template = template,
        .requested_fn_ty = second_source_ty,
        .reason = .{ .comptime_dependency_summary = @enumFromInt(first_summary_index) },
    };

    const first = try queue.reserve(&concrete, first_request);
    const second = try queue.reserve(&concrete, second_request);
    try std.testing.expectEqual(first.local_handle, second.local_handle);
    try std.testing.expectEqual(first.requested_fn_ty, second.requested_fn_ty);
    try std.testing.expectEqual(@as(usize, 1), queue.pending.items.len);
}
