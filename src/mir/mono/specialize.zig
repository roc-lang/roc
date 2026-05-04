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
const LowerType = @import("lower_type.zig");
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
    comptime_dependency_summary: checked_artifact.ComptimeDependencySummaryId,
    promoted_callable_wrapper: canonical.PromotedCallableWrapperId,
    private_capture_callable_leaf: checked_artifact.PrivateCaptureNodeId,
    semantic_instantiation_procedure: checked_artifact.SemanticInstantiationProcedureId,
    erased_promoted_wrapper_code: canonical.ProcedureTemplateRef,
    erased_finite_capture_member: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
};

/// Public `Input` declaration.
pub const Input = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
    mode: LoweringMode = .runnable,
};

/// Public `LoweringMode` declaration.
pub const LoweringMode = enum {
    runnable,
    comptime_dependency_summary,
};

/// Public `MonoSpecializationRequest` declaration.
pub const MonoSpecializationRequest = struct {
    template: canonical.ProcedureTemplateRef,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    reason: MonoSpecializationReason,
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
    local_handle: MonoProcHandle,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    state: ReservedState,
};

/// Public `Proc` declaration.
pub const Proc = struct {
    key: canonical.MonoSpecializationKey,
    proc: canonical.MirProcedureRef,
    local_handle: MonoProcHandle,
    fn_ty: Type.TypeId,
    body: Ast.DefId,
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
    bool_source_ty: ?canonical.CanonicalTypeKey,

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
            .bool_source_ty = null,
        };
    }

    pub fn deinit(self: *Program) void {
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
            .proc = canonical.mirProcedureRefFromMono(reserved.proc),
            .local_handle = reserved.local_handle,
            .fn_ty = fn_ty,
            .body = body,
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

    pub fn boolSourceTypeKey(self: *Program) Allocator.Error!canonical.CanonicalTypeKey {
        if (self.bool_source_ty) |key| return key;

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
            .args = &.{},
        } });
        var bool_key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
            self.allocator,
            &self.canonical_names,
            self.concrete_source_types.local_payloads.items,
        );
        defer bool_key_builder.deinit();
        const bool_key = try bool_key_builder.keyForRoot(bool_root);
        _ = try self.concrete_source_types.sealLocalRoot(bool_root, bool_key);
        self.bool_source_ty = bool_key;
        return bool_key;
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
        const request = MonoSpecializationRequest{
            .template = try name_resolver.procedureTemplateRef(seed.template),
            .requested_fn_ty = seed.requested_fn_ty,
            .reason = seed.reason,
        };
        const reserved = try queue.reserve(&program.concrete_source_types, request);
        try program.root_procs.append(allocator, canonical.mirProcedureRefFromMono(reserved.proc));
        try program.root_metadata.append(allocator, seed.metadata);
    }

    while (queue.pending.items.len != 0) {
        const key = queue.pending.orderedRemove(0);
        queue.markLowering(key);
        const reserved = queue.requested.get(key) orelse unreachable;
        const template_lookup = checkedTemplateForKey(input, key.template);
        if (executableSyntheticProcForReserved(key, reserved, template_lookup)) |synthetic| {
            try reserveExecutableSyntheticProcDependencies(allocator, input, &program, &queue, template_lookup.artifact, synthetic);
            try program.addExecutableSyntheticProc(synthetic);
            queue.markLowered(key);
            continue;
        }
        var type_instantiator = TypeInstantiator.init(allocator, input, &program, template_lookup.checked_types, &name_resolver, template_lookup.artifact);
        defer type_instantiator.deinit();
        try type_instantiator.buildFromRequest(template_lookup.template.checked_fn_root, reserved.requested_fn_ty);
        const fn_ty = try type_instantiator.lowerTemplateType(template_lookup.template.checked_fn_root);
        var body_lowerer = BodyLowerer.init(allocator, input, &program, template_lookup, &type_instantiator, &name_resolver, &queue);
        defer body_lowerer.deinit();
        const body = try body_lowerer.lowerTemplateBody(reserved, fn_ty);
        try program.addProc(key, reserved, fn_ty, body);
        queue.markLowered(key);
    }

    if (@import("builtin").mode == .Debug) verifyProgram(&program);
    return program;
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
    template: checked_artifact.CheckedProcedureTemplate,
};

fn checkedTemplateForKey(
    input: Input,
    template_ref: canonical.ProcedureTemplateRef,
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
                    .template = exported.template_data,
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
                    .template = exported.template_data,
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
                .template = related.checked_procedure_templates.get(template_ref.template),
            };
        }
        debug.invariant(false, "mono specialization invariant violated: relation template was not exported or present in the relation closure");
        unreachable;
    }

    debug.invariant(false, "mono specialization invariant violated: template artifact was not available to lowering");
    unreachable;
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
            const template = templateForRoot(input, root, &program.concrete_source_types, requested_fn_ty) orelse break :root_seed null;
            break :root_seed .{
                .template = template,
                .requested_fn_ty = requested_fn_ty,
                .metadata = rootMetadataFromChecked(root),
                .reason = .{ .root = root },
            };
        },
        .const_instance => |request| const_seed: {
            const template = constEvalEntryTemplateForRequest(input, request) orelse {
                invariantViolation("mono specialization compile-time const instance did not name an eval template");
            };
            break :const_seed .{
                .template = template,
                .requested_fn_ty = try compileTimeEntryFunctionTypeForReturn(
                    allocator,
                    input,
                    program,
                    name_resolver,
                    request.requested_source_ty_payload,
                ),
                .metadata = compileTimeMetadata(order, .compile_time_constant),
                .reason = .{ .const_instance = request },
            };
        },
        .callable_binding_instance => |request| callable_seed: {
            const template = callableEvalEntryTemplateForRequest(input, request) orelse {
                invariantViolation("mono specialization compile-time callable binding instance did not name a callable eval template");
            };
            break :callable_seed .{
                .template = template,
                .requested_fn_ty = try compileTimeFunctionTypeForRequest(
                    allocator,
                    input,
                    program,
                    name_resolver,
                    request.requested_source_fn_ty_payload,
                ),
                .metadata = compileTimeMetadata(order, .compile_time_callable),
                .reason = .{ .callable_binding_instance = request },
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
    const ret_ref = try program.concrete_source_types.registerArtifactRoot(
        input.root.artifact.key,
        input.root.artifact.checked_types.view(),
        return_ty,
    );
    var materializer = TypeInstantiator.init(
        allocator,
        input,
        program,
        input.root.artifact.checked_types.view(),
        name_resolver,
        input.root.artifact.key,
    );
    defer materializer.deinit();
    const local_ret = try materializer.materializeConcreteRef(ret_ref);
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

fn compileTimeFunctionTypeForRequest(
    allocator: Allocator,
    input: Input,
    program: *Program,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    requested_fn_ty: checked_artifact.CheckedTypeId,
) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
    const concrete = try program.concrete_source_types.registerArtifactRoot(
        input.root.artifact.key,
        input.root.artifact.checked_types.view(),
        requested_fn_ty,
    );
    var materializer = TypeInstantiator.init(
        allocator,
        input,
        program,
        input.root.artifact.checked_types.view(),
        name_resolver,
        input.root.artifact.key,
    );
    defer materializer.deinit();
    const local_root = try materializer.materializeConcreteRef(concrete);
    var key_builder = ConcreteSourceType.PayloadKeyBuilder.init(
        allocator,
        &program.canonical_names,
        program.concrete_source_types.local_payloads.items,
    );
    defer key_builder.deinit();
    return try program.concrete_source_types.sealLocalRoot(local_root, try key_builder.keyForRoot(local_root));
}

fn constEvalEntryTemplateForRequest(
    input: Input,
    request: checked_artifact.ConstInstantiationRequest,
) ?canonical.ProcedureTemplateRef {
    const templates = constTemplatesForKey(input, request.key.const_ref.artifact) orelse {
        debug.invariant(false, "mono specialization invariant violated: const instance template artifact was not available");
        unreachable;
    };
    const template = templates.get(request.key.const_ref);
    return switch (template.state) {
        .eval_template => |eval| eval.entry_template,
        .value_graph_template => null,
        .reserved => {
            debug.invariant(false, "mono specialization invariant violated: const instance reached unsealed template");
            unreachable;
        },
    };
}

fn callableEvalEntryTemplateForRequest(
    input: Input,
    request: checked_artifact.CallableBindingInstantiationRequest,
) ?canonical.ProcedureTemplateRef {
    const OwnerAndBinding = struct {
        owner: checked_artifact.CheckedModuleArtifactKey,
        binding: checked_artifact.TopLevelProcedureBindingRef,
    };
    const owner_and_binding: OwnerAndBinding = switch (request.key.binding) {
        .top_level => |binding| .{
            .owner = input.root.artifact.key,
            .binding = binding,
        },
        .platform_required => |required| .{
            .owner = required.artifact,
            .binding = required.procedure_binding,
        },
        .imported => |imported| {
            const view = importedProcedureBindingViewForRef(input, imported) orelse {
                debug.invariant(false, "mono specialization invariant violated: imported callable eval binding was not available");
                unreachable;
            };
            return switch (view.body) {
                .callable_eval_template => |template_id| callableEvalEntryTemplateForKey(input, imported.artifact, template_id),
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
        .callable_eval_template => |template_id| callableEvalEntryTemplateForKey(input, owner_and_binding.owner, template_id),
        .direct_template => null,
    };
}

fn executableSyntheticProcForReserved(
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
                    if (!std.mem.eql(u8, &erased.source_fn_ty.bytes, &key.requested_mono_fn_ty.bytes)) {
                        invariantViolation("erased promoted callable wrapper source function type disagrees with mono specialization request");
                    }
                    break :erased_blk mir_ids.ExecutableSyntheticProc{
                        .artifact = template_lookup.artifact,
                        .source_proc = canonical.mirProcedureRefFromMono(reserved.proc),
                        .template = key.template,
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
    var state = ExecutableSyntheticDependencyState.init(allocator);
    defer state.deinit();

    switch (synthetic.body) {
        .erased_promoted_wrapper => |erased| {
            try reserveErasedCodeRefDependency(input, program, queue, erased.code, .{ .erased_promoted_wrapper_code = synthetic.template });
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, &state, owner_artifact, synthetic.comptime_plans, erased.capture);
            switch (erased.hidden_capture_arg) {
                .none => {},
                .materialized_capture => |capture| try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, &state, owner_artifact, synthetic.comptime_plans, capture),
            }
        },
    }
}

fn reserveErasedCodeRefDependency(
    input: Input,
    program: *Program,
    queue: *Queue,
    code: canonical.ErasedCallableCodeRef,
    reason: MonoSpecializationReason,
) Allocator.Error!void {
    switch (code) {
        .direct_proc_value => |direct| _ = try reserveProcedureCallableDependency(input, program, queue, direct.proc_value, reason),
        .finite_set_adapter => |adapter| {
            const descriptor = callableSetDescriptorForKey(input, adapter.callable_set_key) orelse {
                invariantViolation("mono dependency reservation reached finite-set adapter with no callable-set descriptor");
            };
            for (descriptor.members) |member| {
                _ = try reserveProcedureCallableDependency(input, program, queue, member.proc_value, reason);
            }
        },
    }
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
            const descriptor = callableSetDescriptorForKey(input, finite.callable_set_key) orelse {
                invariantViolation("mono dependency reservation reached materialized finite callable set with no descriptor");
            };
            for (descriptor.members) |member| {
                if (member.member == finite.selected_member) {
                    _ = try reserveProcedureCallableDependency(input, program, queue, member.proc_value, .{ .erased_finite_capture_member = node_id });
                    break;
                }
            } else {
                invariantViolation("mono dependency reservation reached materialized finite callable set with missing selected member");
            }
            for (finite.captures) |capture| {
                try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, capture);
            }
        },
        .erased_callable => |erased| {
            try reserveErasedCodeRefDependency(input, program, queue, erased.code, .{ .erased_finite_capture_member = node_id });
            try reserveErasedCaptureExecutableMaterializationPlanDependencies(input, program, queue, state, owner_artifact, plans, erased.capture);
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
        .finite_callable_leaf => |leaf| try reserveCallableLeafDependency(input, program, queue, leaf, .{ .private_capture_callable_leaf = node_id }),
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
    leaf: checked_artifact.FiniteCallableLeafInstance,
    reason: MonoSpecializationReason,
) Allocator.Error!void {
    _ = try reserveProcedureCallableDependency(input, program, queue, leaf.proc_value, reason);
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

    const instance = callableBindingInstanceForRef(input, ref);
    const reason = MonoSpecializationReason{ .comptime_dependency_summary = instance.dependency_summary };
    _ = try reserveProcedureCallableDependency(input, program, queue, instance.proc_value, reason);
    try reserveComptimeDependencySummaryDependencies(input, program, queue, state, ref.owner, instance.dependency_summary);
    for (instance.generated_procedures) |procedure| {
        try reserveSemanticInstantiationProcedureDependency(input, program, queue, state, ref.owner, procedure);
    }
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
            .const_instance => |key| {
                const ref = constInstanceForKey(input, owner, key) orelse {
                    debug.invariant(false, "mono dependency reservation invariant violated: dependency summary referenced an unsealed constant instance");
                    unreachable;
                };
                try reserveConstInstanceRefDependencies(input, program, queue, state, ref);
            },
            .callable_binding_instance => |key| {
                const ref = callableBindingInstanceForKey(input, owner, key) orelse {
                    debug.invariant(false, "mono dependency reservation invariant violated: dependency summary referenced an unsealed callable binding instance");
                    unreachable;
                };
                try reserveCallableBindingInstanceRefDependencies(input, program, queue, state, ref);
            },
            .procedure_callable => |callable| {
                _ = try reserveProcedureCallableDependency(input, program, queue, callable, reason);
            },
        }
    }
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
    const reserved = try reserveProcedureCallableDependency(input, program, queue, callable, .{ .semantic_instantiation_procedure = procedure_id });
    if (!canonical.procedureValueRefEql(reserved.proc, procedure.proc_value)) {
        debug.invariant(false, "mono dependency reservation invariant violated: semantic procedure proc value disagreed with reserved procedure");
        unreachable;
    }
}

fn reserveProcedureCallableDependency(
    input: Input,
    program: *Program,
    queue: *Queue,
    callable: canonical.ProcedureCallableRef,
    reason: MonoSpecializationReason,
) Allocator.Error!canonical.MirProcedureRef {
    const template = checkedTemplateFromCallableTemplate(callable.template);
    const artifact = artifactKeyForRef(input, template.artifact) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: callable template artifact was not available");
        unreachable;
    };
    const checked_types = checkedTypesForKey(input, artifact) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: callable template checked types were not available");
        unreachable;
    };
    const checked_ty = checkedTypeRootForKey(checked_types, callable.source_fn_ty) orelse {
        debug.invariant(false, "mono dependency reservation invariant violated: callable source function type was not published");
        unreachable;
    };
    const requested_fn_ty = try program.concrete_source_types.registerArtifactRoot(artifact, checked_types, checked_ty);
    const requested_key = program.concrete_source_types.key(requested_fn_ty);
    if (!std.mem.eql(u8, &requested_key.bytes, &callable.source_fn_ty.bytes)) {
        invariantViolation("mono dependency reservation source function type disagrees with callable occurrence");
    }
    const reserved = try queue.reserve(&program.concrete_source_types, .{
        .template = template,
        .requested_fn_ty = requested_fn_ty,
        .reason = reason,
    });
    return .{
        .proc = reserved.proc.proc,
        .callable = callable,
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

const TypeInstantiator = struct {
    allocator: Allocator,
    input: Input,
    program: *Program,
    template_types: checked_artifact.CheckedTypeStoreView,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    template_artifact: checked_artifact.CheckedModuleArtifactKey,
    substitutions: std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef),
    lowered_template: std.AutoHashMap(checked_artifact.CheckedTypeId, Type.TypeId),
    materialized_template_roots: std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId),
    materialized_concrete_roots: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId),
    concrete_template_refs: std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef),

    const ConcreteRecordFieldEntry = struct {
        name: canonical.RecordFieldLabelId,
        owner: ConcreteSourceType.ConcreteSourceTypeRef,
        ty: checked_artifact.CheckedTypeId,
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
            .lowered_template = std.AutoHashMap(checked_artifact.CheckedTypeId, Type.TypeId).init(allocator),
            .materialized_template_roots = std.AutoHashMap(checked_artifact.CheckedTypeId, checked_artifact.CheckedTypeId).init(allocator),
            .materialized_concrete_roots = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, checked_artifact.CheckedTypeId).init(allocator),
            .concrete_template_refs = std.AutoHashMap(checked_artifact.CheckedTypeId, ConcreteSourceType.ConcreteSourceTypeRef).init(allocator),
        };
    }

    fn deinit(self: *TypeInstantiator) void {
        self.concrete_template_refs.deinit();
        self.materialized_concrete_roots.deinit();
        self.materialized_template_roots.deinit();
        self.lowered_template.deinit();
        self.substitutions.deinit();
    }

    fn buildFromRequest(
        self: *TypeInstantiator,
        template_fn_root: checked_artifact.CheckedTypeId,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        try self.unifyTemplateWithConcrete(template_fn_root, requested_fn_ty);
    }

    fn fork(self: *const TypeInstantiator) Allocator.Error!TypeInstantiator {
        var child = TypeInstantiator.init(
            self.allocator,
            self.input,
            self.program,
            self.template_types,
            self.name_resolver,
            self.template_artifact,
        );
        errdefer child.deinit();

        try child.substitutions.ensureTotalCapacity(self.substitutions.count());
        var substitutions = self.substitutions.iterator();
        while (substitutions.next()) |entry| {
            child.substitutions.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        try child.lowered_template.ensureTotalCapacity(self.lowered_template.count());
        var lowered = self.lowered_template.iterator();
        while (lowered.next()) |entry| {
            child.lowered_template.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
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

        try child.concrete_template_refs.ensureTotalCapacity(self.concrete_template_refs.count());
        var concrete_template = self.concrete_template_refs.iterator();
        while (concrete_template.next()) |entry| {
            child.concrete_template_refs.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
        }

        return child;
    }

    fn lowerTemplateType(self: *TypeInstantiator, id: checked_artifact.CheckedTypeId) Allocator.Error!Type.TypeId {
        if (self.substitutions.get(id)) |concrete| {
            return try self.lowerConcreteRef(concrete);
        }
        if (self.lowered_template.get(id)) |existing| return existing;

        const placeholder = try self.program.types.addType(.placeholder);
        try self.lowered_template.put(id, placeholder);
        const lowered = try self.lowerTemplatePayload(id, self.templatePayload(id));
        self.program.types.setType(placeholder, lowered);
        self.program.types.debugValidateTypeGraph(placeholder);
        return try self.program.types.internTypeId(placeholder);
    }

    fn concreteRefForTemplateType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceType.ConcreteSourceTypeRef {
        if (self.substitutions.get(id)) |concrete| return concrete;
        if (self.concrete_template_refs.get(id)) |existing| return existing;

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

    fn materializeTemplateType(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        if (self.substitutions.get(id)) |concrete| return try self.materializeConcreteRef(concrete);
        if (self.materialized_template_roots.get(id)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_template_roots.put(id, local_root);
        errdefer _ = self.materialized_template_roots.remove(id);

        const payload = try self.materializeTemplatePayload(self.templatePayload(id));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealMaterializedLocalRoot(local_root);
        return local_root;
    }

    fn materializeTemplatePayload(
        self: *TypeInstantiator,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished checked type payload"),
            .flex => |flex| try self.materializeUnconstrainedFlexType(flex),
            .rigid => invariantViolation("mono specialization reached an unmapped rigid type variable while materializing a concrete source type"),
            .alias => |alias| .{ .alias = .{
                .name = try self.name_resolver.typeName(self.template_artifact, alias.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, alias.origin_module),
                .backing = try self.materializeTemplateType(alias.backing),
                .args = try self.materializeTemplateTypeIds(alias.args),
            } },
            .record_unbound => |fields| .{ .record_unbound = try self.materializeTemplateRecordFields(fields) },
            .record => |record| .{ .record = .{
                .fields = try self.materializeTemplateRecordFields(record.fields),
                .ext = try self.materializeTemplateRecordExt(record.ext),
            } },
            .tuple => |items| .{ .tuple = try self.materializeTemplateTypeIds(items) },
            .nominal => |nominal| .{ .nominal = .{
                .name = try self.name_resolver.typeName(self.template_artifact, nominal.name),
                .origin_module = try self.name_resolver.moduleName(self.template_artifact, nominal.origin_module),
                .builtin = nominal.builtin,
                .is_opaque = nominal.is_opaque,
                .backing = try self.materializeTemplateType(nominal.backing),
                .args = try self.materializeTemplateTypeIds(nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = func.kind,
                .args = try self.materializeTemplateTypeIds(func.args),
                .ret = try self.materializeTemplateType(func.ret),
                .needs_instantiation = false,
            } },
            .empty_record => .empty_record,
            .tag_union => |tag_union| .{ .tag_union = .{
                .tags = try self.materializeTemplateTags(tag_union.tags),
                .ext = try self.materializeTemplateTagUnionExt(tag_union.ext),
            } },
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
            else => try self.materializeTemplateType(ext),
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
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
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

    fn lowerTemplatePayload(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished checked type payload"),
            .flex => |flex| self.lowerUnconstrainedFlexType(flex),
            .rigid => invariantViolation("mono specialization reached an unmapped rigid type variable"),
            .alias => |alias| .{ .link = try self.lowerTemplateType(alias.backing) },
            .record_unbound => |fields| .{ .record = .{ .fields = try self.lowerTemplateRecordFieldsOnly(fields) } },
            .record => |record| .{ .record = .{ .fields = try self.lowerTemplateRecord(record) } },
            .tuple => |elems| .{ .tuple = try self.lowerTemplateTypeIds(elems) },
            .nominal => |nominal| try self.lowerTemplateNominal(id, nominal),
            .function => |func| .{ .func = .{
                .args = try self.lowerTemplateTypeIds(func.args),
                .lambdas = &.{},
                .ret = try self.lowerTemplateType(func.ret),
            } },
            .empty_record => .{ .record = .{ .fields = &.{} } },
            .tag_union => |tag_union| .{ .tag_union = .{ .tags = try self.lowerTemplateTagUnion(tag_union) } },
            .empty_tag_union => .{ .tag_union = .{ .tags = &.{} } },
        };
    }

    fn lowerTemplateTypeIds(
        self: *TypeInstantiator,
        ids: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const Type.TypeId {
        if (ids.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, ids.len);
        errdefer self.allocator.free(out);
        for (ids, 0..) |id, i| {
            out[i] = try self.lowerTemplateType(id);
        }
        return out;
    }

    fn lowerTemplateRecordFieldsOnly(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error![]const Type.Field {
        if (fields.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.Field, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.lowerTemplateType(field.ty),
            };
        }
        return out;
    }

    fn lowerTemplateRecord(
        self: *TypeInstantiator,
        record: checked_artifact.CheckedRecordType,
    ) Allocator.Error![]const Type.Field {
        var fields = std.ArrayList(Type.Field).empty;
        errdefer fields.deinit(self.allocator);
        try self.collectTemplateRecordFields(record.fields, record.ext, &fields);
        std.mem.sort(Type.Field, fields.items, {}, struct {
            fn lessThan(_: void, a: Type.Field, b: Type.Field) bool {
                return @intFromEnum(a.name) < @intFromEnum(b.name);
            }
        }.lessThan);
        return try fields.toOwnedSlice(self.allocator);
    }

    fn collectTemplateRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        ext: checked_artifact.CheckedTypeId,
        out: *std.ArrayList(Type.Field),
    ) Allocator.Error!void {
        for (fields) |field| {
            try out.append(self.allocator, .{
                .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                .ty = try self.lowerTemplateType(field.ty),
            });
        }

        if (self.substitutions.get(ext)) |concrete| {
            try self.collectConcreteRecordFields(concrete, out);
            return;
        }

        const ext_payload = self.templatePayload(ext);
        switch (ext_payload) {
            .alias => |alias| try self.collectTemplateRecordFields(&.{}, alias.backing, out),
            .empty_record => {},
            .flex => |flex| self.verifyClosableRowTail(flex),
            .record_unbound => |ext_fields| {
                for (ext_fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name),
                        .ty = try self.lowerTemplateType(field.ty),
                    });
                }
            },
            .record => |ext_record| try self.collectTemplateRecordFields(ext_record.fields, ext_record.ext, out),
            else => invariantViolation("mono specialization record extension resolved to a non-record type"),
        }
    }

    fn collectConcreteRecordFields(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(Type.Field),
    ) Allocator.Error!void {
        const payload = self.concretePayload(ref);
        switch (payload) {
            .alias => |alias| try self.collectConcreteRecordFields(try self.concreteChildRef(ref, alias.backing), out),
            .empty_record => {},
            .flex => |flex| self.verifyClosableRowTail(flex),
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

    fn collectConcreteRecordFieldEntries(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(ConcreteRecordFieldEntry),
    ) Allocator.Error!void {
        const payload = self.concretePayload(ref);
        switch (payload) {
            .alias => |alias| try self.collectConcreteRecordFieldEntries(try self.concreteChildRef(ref, alias.backing), out),
            .empty_record => {},
            .flex => |flex| self.verifyClosableRowTail(flex),
            .record_unbound => |fields| {
                for (fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.recordFieldNameForConcreteRef(ref, field.name),
                        .owner = ref,
                        .ty = field.ty,
                    });
                }
            },
            .record => |record| {
                for (record.fields) |field| {
                    try out.append(self.allocator, .{
                        .name = try self.recordFieldNameForConcreteRef(ref, field.name),
                        .owner = ref,
                        .ty = field.ty,
                    });
                }
                try self.collectConcreteRecordFieldEntries(try self.concreteChildRef(ref, record.ext), out);
            },
            else => invariantViolation("mono specialization concrete record extension resolved to a non-record type"),
        }
    }

    fn templateRecordExtIsClosed(
        self: *TypeInstantiator,
        ext: checked_artifact.CheckedTypeId,
    ) bool {
        if (self.substitutions.get(ext)) |_| return false;
        return switch (self.templatePayload(ext)) {
            .alias => |alias| self.templateRecordExtIsClosed(alias.backing),
            .empty_record => true,
            else => false,
        };
    }

    fn lowerTemplateTagUnion(
        self: *TypeInstantiator,
        tag_union: checked_artifact.CheckedTagUnionType,
    ) Allocator.Error![]const Type.Tag {
        var tags = std.ArrayList(Type.Tag).empty;
        errdefer {
            for (tags.items) |tag| self.allocator.free(tag.args);
            tags.deinit(self.allocator);
        }
        try self.collectTemplateTags(tag_union.tags, tag_union.ext, &tags);
        std.mem.sort(Type.Tag, tags.items, {}, struct {
            fn lessThan(_: void, a: Type.Tag, b: Type.Tag) bool {
                return @intFromEnum(a.name) < @intFromEnum(b.name);
            }
        }.lessThan);
        return try tags.toOwnedSlice(self.allocator);
    }

    fn collectTemplateTags(
        self: *TypeInstantiator,
        tags: []const checked_artifact.CheckedTag,
        ext: checked_artifact.CheckedTypeId,
        out: *std.ArrayList(Type.Tag),
    ) Allocator.Error!void {
        for (tags) |tag| {
            const name = try self.name_resolver.tagLabel(self.template_artifact, tag.name);
            if (containsTagName(out.items, name)) continue;
            try out.append(self.allocator, .{
                .name = name,
                .args = try self.lowerTemplateTypeIds(tag.args),
            });
        }

        if (self.substitutions.get(ext)) |concrete| {
            try self.collectConcreteTags(concrete, out);
            return;
        }

        const ext_payload = self.templatePayload(ext);
        switch (ext_payload) {
            .alias => |alias| try self.collectTemplateTags(&.{}, alias.backing, out),
            .nominal => |nominal| try self.collectTemplateTags(&.{}, nominal.backing, out),
            .empty_tag_union => {},
            .flex => |flex| self.verifyClosableRowTail(flex),
            .tag_union => |ext_tags| try self.collectTemplateTags(ext_tags.tags, ext_tags.ext, out),
            else => invariantViolation("mono specialization tag-union extension resolved to a non-tag-union type"),
        }
    }

    fn collectConcreteTags(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        out: *std.ArrayList(Type.Tag),
    ) Allocator.Error!void {
        const payload = self.concretePayload(ref);
        switch (payload) {
            .alias => |alias| try self.collectConcreteTags(try self.concreteChildRef(ref, alias.backing), out),
            .empty_tag_union => {},
            .flex => |flex| self.verifyClosableRowTail(flex),
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
            .nominal => |nominal| try self.collectConcreteTags(try self.concreteChildRef(ref, nominal.backing), out),
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

    fn lowerTemplateNominal(
        self: *TypeInstantiator,
        id: checked_artifact.CheckedTypeId,
        nominal: checked_artifact.CheckedNominalType,
    ) Allocator.Error!Type.Content {
        if (nominal.builtin) |builtin_nominal| {
            switch (builtin_nominal) {
                .bool => return .{ .primitive = .bool },
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
                    return .{ .list = try self.lowerTemplateType(nominal.args[0]) };
                },
                .box => {
                    if (nominal.args.len != 1) invariantViolation("Box nominal type did not have exactly one argument");
                    return .{ .box = try self.lowerTemplateType(nominal.args[0]) };
                },
            }
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = try self.name_resolver.moduleName(self.template_artifact, nominal.origin_module),
                .type_name = try self.name_resolver.typeName(self.template_artifact, nominal.name),
            },
            .source_ty = self.template_types.roots[@intFromEnum(id)].key,
            .is_opaque = nominal.is_opaque,
            .args = try self.lowerTemplateTypeIds(nominal.args),
            .backing = try self.lowerTemplateType(nominal.backing),
        } };
    }

    fn lowerArtifactRef(
        self: *TypeInstantiator,
        ref: checked_artifact.ArtifactCheckedTypeRef,
    ) Allocator.Error!Type.TypeId {
        const checked_types = checkedTypesForKey(self.input, ref.artifact) orelse {
            debug.invariant(false, "mono specialization invariant violated: concrete type ref artifact was not available");
            unreachable;
        };
        var lowerer = LowerType.Lowerer.initWithResolver(
            self.allocator,
            checked_types,
            &self.program.types,
            self.name_resolver,
            ref.artifact,
        );
        defer lowerer.deinit();
        return try lowerer.lowerChecked(ref.ty);
    }

    fn lowerConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Type.TypeId {
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.lowerArtifactRef(artifact_ref),
            .local => |local| blk: {
                var lowerer = LowerType.Lowerer.init(
                    self.allocator,
                    self.program.concrete_source_types.localView(),
                    &self.program.types,
                );
                defer lowerer.deinit();
                break :blk try lowerer.lowerChecked(local);
            },
        };
    }

    fn unifyTemplateWithConcrete(
        self: *TypeInstantiator,
        template_id: checked_artifact.CheckedTypeId,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        switch (self.templatePayload(template_id)) {
            .flex, .rigid => {
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
            .alias => |alias| {
                try self.unifyTemplateWithConcrete(template_id, try self.concreteChildRef(concrete, alias.backing));
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
            .record => |record| switch (concrete_payload) {
                .record => |concrete_record| try self.unifyRecords(record, concrete, concrete_record),
                .empty_record => if (record.fields.len != 0) invariantViolation("mono specialization record arity mismatch"),
                else => invariantViolation("mono specialization expected a concrete record"),
            },
            .record_unbound => |fields| switch (concrete_payload) {
                .record, .record_unbound => try self.unifyRecordFieldSet(fields, concrete),
                .empty_record => if (fields.len != 0) invariantViolation("mono specialization record arity mismatch"),
                else => invariantViolation("mono specialization expected a concrete record row"),
            },
            .tuple => |items| switch (concrete_payload) {
                .tuple => |concrete_items| try self.unifyTypeLists(items, concrete, concrete_items),
                else => invariantViolation("mono specialization expected a concrete tuple"),
            },
            .nominal => |nominal| switch (concrete_payload) {
                .nominal => |concrete_nominal| try self.unifyNominals(nominal, concrete, concrete_nominal),
                else => invariantViolation("mono specialization expected a concrete nominal"),
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
                .record => |record| if (record.fields.len != 0) invariantViolation("mono specialization empty record mismatch"),
                else => invariantViolation("mono specialization expected an empty concrete record"),
            },
            .tag_union => |tag_union| switch (concrete_payload) {
                .tag_union => |concrete_tag_union| try self.unifyTagUnions(tag_union, concrete, concrete_tag_union),
                .empty_tag_union => if (tag_union.tags.len != 0) invariantViolation("mono specialization tag-union mismatch"),
                else => invariantViolation("mono specialization expected a concrete tag union"),
            },
            .empty_tag_union => switch (concrete_payload) {
                .empty_tag_union => {},
                .tag_union => |tag_union| if (tag_union.tags.len != 0) invariantViolation("mono specialization empty tag-union mismatch"),
                else => invariantViolation("mono specialization expected an empty concrete tag union"),
            },
            .pending, .flex, .rigid, .alias => unreachable,
        }
    }

    fn unifyRecords(
        self: *TypeInstantiator,
        template: checked_artifact.CheckedRecordType,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete_record: checked_artifact.CheckedRecordType,
    ) Allocator.Error!void {
        if (self.templateRecordExtIsClosed(template.ext)) {
            try self.unifyClosedRecordFields(template.fields, concrete_ref);
            return;
        }
        try self.unifyRecordFields(template.fields, concrete_ref, concrete_record.fields);
        try self.unifyTemplateWithConcrete(template.ext, try self.concreteChildRef(concrete_ref, concrete_record.ext));
    }

    fn unifyClosedRecordFields(
        self: *TypeInstantiator,
        template_fields: []const checked_artifact.CheckedRecordField,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        var concrete_fields = std.ArrayList(ConcreteRecordFieldEntry).empty;
        defer concrete_fields.deinit(self.allocator);
        try self.collectConcreteRecordFieldEntries(concrete, &concrete_fields);

        const consumed = try self.allocator.alloc(bool, concrete_fields.items.len);
        defer self.allocator.free(consumed);
        @memset(consumed, false);

        for (template_fields) |field| {
            const expected_name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name);
            var matched: ?usize = null;
            for (concrete_fields.items, 0..) |entry, i| {
                if (!consumed[i] and entry.name == expected_name) {
                    matched = i;
                    break;
                }
            }
            const index = matched orelse {
                invariantViolation("mono specialization record field was missing in concrete type");
            };
            consumed[index] = true;
            const entry = concrete_fields.items[index];
            try self.unifyTemplateWithConcrete(field.ty, try self.concreteChildRef(entry.owner, entry.ty));
        }

        for (consumed) |was_consumed| {
            if (!was_consumed) invariantViolation("mono specialization closed record concrete type had an extra field");
        }
    }

    fn unifyRecordFieldSet(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!void {
        switch (self.concretePayload(concrete)) {
            .record => |record| try self.unifyRecordFields(fields, concrete, record.fields),
            .record_unbound => |concrete_fields| try self.unifyRecordFields(fields, concrete, concrete_fields),
            else => invariantViolation("mono specialization expected concrete record fields"),
        }
    }

    fn unifyRecordFields(
        self: *TypeInstantiator,
        fields: []const checked_artifact.CheckedRecordField,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete_fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error!void {
        for (fields) |field| {
            const expected_name = try self.name_resolver.recordFieldLabel(self.template_artifact, field.name);
            const concrete_field = (try self.findConcreteRecordField(concrete, concrete_fields, expected_name)) orelse {
                invariantViolation("mono specialization record field was missing in concrete type");
            };
            try self.unifyTemplateWithConcrete(field.ty, try self.concreteChildRef(concrete, concrete_field.ty));
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

    fn unifyTagUnions(
        self: *TypeInstantiator,
        template: checked_artifact.CheckedTagUnionType,
        concrete_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        concrete: checked_artifact.CheckedTagUnionType,
    ) Allocator.Error!void {
        for (template.tags) |tag| {
            const expected_name = try self.name_resolver.tagLabel(self.template_artifact, tag.name);
            const concrete_tag = (try self.findConcreteTag(concrete_ref, concrete.tags, expected_name)) orelse {
                invariantViolation("mono specialization tag was missing in concrete type");
            };
            try self.unifyTypeLists(tag.args, concrete_ref, concrete_tag.args);
        }
        try self.unifyTemplateWithConcrete(template.ext, try self.concreteChildRef(concrete_ref, concrete.ext));
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
        if (self.substitutions.get(template_id)) |existing| {
            const existing_key = self.program.concrete_source_types.key(existing);
            const concrete_key = self.program.concrete_source_types.key(concrete);
            if (!std.mem.eql(u8, &existing_key.bytes, &concrete_key.bytes)) {
                invariantViolation("mono specialization generic variable mapped to incompatible concrete types");
            }
            return;
        }
        try self.substitutions.put(template_id, concrete);
    }

    fn templatePayload(self: *const TypeInstantiator, id: checked_artifact.CheckedTypeId) checked_artifact.CheckedTypePayload {
        const raw = @intFromEnum(id);
        if (raw >= self.template_types.payloads.len) invariantViolation("mono specialization template type id was outside published payloads");
        return self.template_types.payloads[raw];
    }

    fn concretePayload(self: *const TypeInstantiator, ref: ConcreteSourceType.ConcreteSourceTypeRef) checked_artifact.CheckedTypePayload {
        const root = self.program.concrete_source_types.root(ref);
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
        const root = self.program.concrete_source_types.root(parent);
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

    fn materializeConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const root = self.program.concrete_source_types.root(ref);
        switch (root.source) {
            .local => |local| return local,
            .artifact => {},
        }
        if (self.materialized_concrete_roots.get(ref)) |existing| return existing;

        const local_root = try self.program.concrete_source_types.reservePendingLocalRoot();
        try self.materialized_concrete_roots.put(ref, local_root);
        errdefer _ = self.materialized_concrete_roots.remove(ref);

        const payload = try self.materializeConcretePayload(ref, self.concretePayload(ref));
        self.program.concrete_source_types.fillLocalRoot(local_root, payload);
        try self.sealMaterializedLocalRoot(local_root);
        return local_root;
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

    fn materializeEmptyRecordRowTail(
        self: *TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        self.verifyClosableRowTail(flex);
        return try self.materializeSyntheticPayload(.empty_record);
    }

    fn materializeEmptyTagUnionRowTail(
        self: *TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        self.verifyClosableRowTail(flex);
        return try self.materializeSyntheticPayload(.empty_tag_union);
    }

    fn materializeUnconstrainedFlexType(
        self: *TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        self.verifyUnconstrainedFlex(flex);
        return .empty_record;
    }

    fn lowerUnconstrainedFlexType(
        self: *TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) Type.Content {
        self.verifyUnconstrainedFlex(flex);
        return .{ .record = .{ .fields = &.{} } };
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

    fn verifyClosableRowTail(
        self: *TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) void {
        self.verifyUnconstrainedFlex(flex);
    }

    fn verifyUnconstrainedFlex(
        self: *TypeInstantiator,
        flex: checked_artifact.CheckedTypeVariable,
    ) void {
        _ = self;
        if (flex.constraints.len == 0) return;
        invariantViolation("mono specialization reached a constrained flex variable where a concrete runtime type was required");
    }

    fn materializeConcretePayload(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        payload: checked_artifact.CheckedTypePayload,
    ) Allocator.Error!checked_artifact.CheckedTypePayload {
        return switch (payload) {
            .pending => invariantViolation("mono specialization received an unpublished concrete checked type payload"),
            .flex => |flex| try self.materializeUnconstrainedFlexType(flex),
            .rigid => invariantViolation("mono specialization reached an unsolved rigid type variable in a concrete source type"),
            .alias => |alias| .{ .alias = .{
                .name = try self.typeNameForConcreteRef(ref, alias.name),
                .origin_module = try self.moduleNameForConcreteRef(ref, alias.origin_module),
                .backing = try self.materializeConcreteRef(try self.concreteChildRef(ref, alias.backing)),
                .args = try self.materializeConcreteTypeIds(ref, alias.args),
            } },
            .record_unbound => |fields| .{ .record_unbound = try self.materializeConcreteRecordFields(ref, fields) },
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
                .backing = try self.materializeConcreteRef(try self.concreteChildRef(ref, nominal.backing)),
                .args = try self.materializeConcreteTypeIds(ref, nominal.args),
            } },
            .function => |func| .{ .function = .{
                .kind = func.kind,
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
            else => try self.materializeConcreteRef(ext),
        };
    }

    fn materializeConcreteTagUnionExt(
        self: *TypeInstantiator,
        ext: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return switch (self.concretePayload(ext)) {
            .flex => |flex| try self.materializeEmptyTagUnionRowTail(flex),
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
        for (out) |*tag| tag.* = .{ .name = @enumFromInt(0), .args = &.{} };
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
        const root = self.program.concrete_source_types.root(ref);
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
        const root = self.program.concrete_source_types.root(ref);
        return switch (root.source) {
            .artifact => |artifact_ref| try self.name_resolver.tagLabel(artifact_ref.artifact, name),
            .local => name,
        };
    }

    fn moduleNameForConcreteRef(
        self: *TypeInstantiator,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.ModuleNameId,
    ) Allocator.Error!canonical.ModuleNameId {
        const root = self.program.concrete_source_types.root(ref);
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
        const root = self.program.concrete_source_types.root(ref);
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

    fn findConcreteRecordField(
        self: *TypeInstantiator,
        concrete: ConcreteSourceType.ConcreteSourceTypeRef,
        fields: []const checked_artifact.CheckedRecordField,
        name: canonical.RecordFieldLabelId,
    ) Allocator.Error!?checked_artifact.CheckedRecordField {
        for (fields) |field| {
            if ((try self.recordFieldNameForConcreteRef(concrete, field.name)) == name) return field;
        }
        return null;
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

const BodyLowerer = struct {
    allocator: Allocator,
    input: Input,
    program: *Program,
    template_lookup: CheckedTemplateLookup,
    type_instantiator: *TypeInstantiator,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    queue: *Queue,
    local_symbols: std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol),
    local_symbol_types: std.AutoHashMap(checked_artifact.PatternBinderId, ConcreteTypeInfo),
    local_proc_decls: std.AutoHashMap(checked_artifact.PatternBinderId, LocalProcDecl),
    local_proc_instances: std.AutoHashMap(LocalProcInstanceKey, Ast.Symbol),
    lowered_private_captures: std.AutoHashMap(PrivateCaptureLoweringKey, Ast.ExprId),
    active_private_captures: std.AutoHashMap(PrivateCaptureLoweringKey, void),

    fn init(
        allocator: Allocator,
        input: Input,
        program: *Program,
        template_lookup: CheckedTemplateLookup,
        type_instantiator: *TypeInstantiator,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
        queue: *Queue,
    ) BodyLowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .program = program,
            .template_lookup = template_lookup,
            .type_instantiator = type_instantiator,
            .name_resolver = name_resolver,
            .queue = queue,
            .local_symbols = std.AutoHashMap(checked_artifact.PatternBinderId, Ast.Symbol).init(allocator),
            .local_symbol_types = std.AutoHashMap(checked_artifact.PatternBinderId, ConcreteTypeInfo).init(allocator),
            .local_proc_decls = std.AutoHashMap(checked_artifact.PatternBinderId, LocalProcDecl).init(allocator),
            .local_proc_instances = std.AutoHashMap(LocalProcInstanceKey, Ast.Symbol).init(allocator),
            .lowered_private_captures = std.AutoHashMap(PrivateCaptureLoweringKey, Ast.ExprId).init(allocator),
            .active_private_captures = std.AutoHashMap(PrivateCaptureLoweringKey, void).init(allocator),
        };
    }

    fn deinit(self: *BodyLowerer) void {
        self.active_private_captures.deinit();
        self.lowered_private_captures.deinit();
        self.local_proc_instances.deinit();
        self.local_proc_decls.deinit();
        self.local_symbol_types.deinit();
        self.local_symbols.deinit();
    }

    fn lowerTemplateBody(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.DefId {
        return switch (self.template_lookup.template.body) {
            .checked_body => |body_id| try self.lowerCheckedBody(reserved, fn_ty, body_id),
            .entry_wrapper => |wrapper_id| try self.lowerEntryWrapperDef(reserved, fn_ty, wrapper_id),
            .intrinsic_wrapper => |wrapper_id| try self.lowerIntrinsicWrapperDef(reserved, fn_ty, wrapper_id),
            .promoted_callable_wrapper => |wrapper_id| try self.lowerPromotedCallableWrapperDef(reserved, fn_ty, wrapper_id),
        };
    }

    fn lowerIntrinsicWrapperDef(
        self: *BodyLowerer,
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
        const source_ty = reserved.proc.specialization.requested_mono_fn_ty;
        const params = try self.lowerIntrinsicParamBundle(func.args);
        defer if (params.exprs.len > 0) self.allocator.free(params.exprs);
        const body = switch (wrapper.intrinsic) {
            .str_inspect => blk: {
                if (params.exprs.len != 1) {
                    invariantViolation("mono body lowering expected Str.inspect intrinsic to have exactly one argument");
                }
                break :blk try self.lowerStrInspectIntrinsic(func.ret, params.exprs[0], func.args[0]);
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
            .symbol = try self.program.addProcSymbol(reserved.local_handle),
        };
        return try self.program.ast.addDef(.{
            .proc = canonical.mirProcedureRefFromMono(reserved.proc),
            .debug_name = null,
            .value = .{ .fn_ = .{
                .source_fn_ty = source_ty,
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
        self: *BodyLowerer,
        arg_tys: []const Type.TypeId,
    ) Allocator.Error!IntrinsicParamBundle {
        if (arg_tys.len == 0) return .{
            .args = Ast.Span(Ast.TypedSymbol).empty(),
            .exprs = &.{},
        };
        const args = try self.allocator.alloc(Ast.TypedSymbol, arg_tys.len);
        defer self.allocator.free(args);
        const exprs = try self.allocator.alloc(Ast.ExprId, arg_tys.len);
        errdefer self.allocator.free(exprs);

        for (arg_tys, 0..) |arg_ty, i| {
            const symbol = try self.program.addSyntheticSymbol();
            args[i] = .{
                .ty = arg_ty,
                .source_ty = .{},
                .symbol = symbol,
            };
            exprs[i] = try self.program.ast.addExpr(arg_ty, .{ .var_ = symbol });
        }

        return .{
            .args = try self.program.ast.addTypedSymbolSpan(args),
            .exprs = exprs,
        };
    }

    fn lowerStrInspectIntrinsic(
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        arg_expr: Ast.ExprId,
        arg_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (self.program.types.getType(arg_ty)) {
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
            .tuple => |items| try self.lowerTupleInspectIntrinsic(ret_ty, arg_expr, items),
            .record => |record| try self.lowerRecordInspectIntrinsic(ret_ty, arg_expr, record.fields),
            .tag_union => |tag_union| try self.lowerTagUnionInspectIntrinsic(ret_ty, arg_expr, arg_ty, tag_union.tags),
            else => invariantViolation("Str.inspect intrinsic reached unsupported mono argument type"),
        };
    }

    fn lowerUnaryIntrinsicLowLevel(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        arg_expr: Ast.ExprId,
        items: []const Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        var current = try self.lowerStringLiteralExpr(ret_ty, "(");
        for (items, 0..) |item_ty, i| {
            if (i != 0) current = try self.lowerStrConcatBytes(ret_ty, current, ", ");
            const item_expr = try self.program.ast.addExpr(item_ty, .{ .tuple_access = .{
                .tuple = arg_expr,
                .elem_index = @intCast(i),
            } });
            const inspected = try self.lowerStrInspectIntrinsic(ret_ty, item_expr, item_ty);
            current = try self.lowerStrConcatExpr(ret_ty, current, inspected);
        }
        return try self.lowerStrConcatBytes(ret_ty, current, ")");
    }

    fn lowerRecordInspectIntrinsic(
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        arg_expr: Ast.ExprId,
        fields: []const Type.Field,
    ) Allocator.Error!Ast.ExprId {
        if (fields.len == 0) return try self.lowerStringLiteralExpr(ret_ty, "{}");

        var current = try self.lowerStringLiteralExpr(ret_ty, "{ ");
        for (fields, 0..) |field, i| {
            if (i != 0) current = try self.lowerStrConcatBytes(ret_ty, current, ", ");
            current = try self.lowerStrConcatBytes(ret_ty, current, self.program.canonical_names.recordFieldLabelText(field.name));
            current = try self.lowerStrConcatBytes(ret_ty, current, ": ");
            const field_expr = try self.program.ast.addExpr(field.ty, .{ .access = .{
                .record = arg_expr,
                .field = field.name,
                .field_index = @intCast(i),
            } });
            const inspected = try self.lowerStrInspectIntrinsic(ret_ty, field_expr, field.ty);
            current = try self.lowerStrConcatExpr(ret_ty, current, inspected);
        }
        return try self.lowerStrConcatBytes(ret_ty, current, " }");
    }

    fn lowerTagUnionInspectIntrinsic(
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        arg_expr: Ast.ExprId,
        arg_ty: Type.TypeId,
        tags: []const Type.Tag,
    ) Allocator.Error!Ast.ExprId {
        if (tags.len == 0) invariantViolation("Str.inspect intrinsic reached an uninhabited tag union");

        var branches = std.ArrayList(Ast.Branch).empty;
        defer branches.deinit(self.allocator);
        for (tags, 0..) |tag, tag_index| {
            const payload_pats = try self.allocator.alloc(Ast.PatId, tag.args.len);
            defer self.allocator.free(payload_pats);
            const payload_exprs = try self.allocator.alloc(Ast.ExprId, tag.args.len);
            defer self.allocator.free(payload_exprs);

            for (tag.args, 0..) |payload_ty, payload_index| {
                const symbol = try self.program.addSyntheticSymbol();
                payload_pats[payload_index] = try self.program.ast.addPat(.{
                    .ty = payload_ty,
                    .data = .{ .var_ = symbol },
                });
                payload_exprs[payload_index] = try self.program.ast.addExpr(payload_ty, .{ .var_ = symbol });
            }

            const pat = try self.program.ast.addPat(.{
                .ty = arg_ty,
                .data = .{ .tag = .{
                    .name = tag.name,
                    .discriminant = @intCast(tag_index),
                    .args = try self.program.ast.addPatSpan(payload_pats),
                } },
            });

            try branches.append(self.allocator, .{
                .pat = pat,
                .body = try self.lowerTagInspectBranch(ret_ty, tag, payload_exprs),
            });
        }

        return try self.program.ast.addExpr(ret_ty, .{ .match_ = .{
            .cond = arg_expr,
            .branches = try self.program.ast.addBranchSpan(branches.items),
            .is_try_suffix = false,
        } });
    }

    fn lowerTagInspectBranch(
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        tag: Type.Tag,
        payload_exprs: []const Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const tag_name = self.program.canonical_names.tagLabelText(tag.name);
        if (tag.args.len != payload_exprs.len) invariantViolation("Str.inspect tag payload count disagreed with tag type");
        if (tag.args.len == 0) return try self.lowerStringLiteralExpr(ret_ty, tag_name);

        var current = try self.lowerStringLiteralExpr(ret_ty, tag_name);
        current = try self.lowerStrConcatBytes(ret_ty, current, "(");
        for (tag.args, payload_exprs, 0..) |payload_ty, payload_expr, i| {
            if (i != 0) current = try self.lowerStrConcatBytes(ret_ty, current, ", ");
            const inspected = try self.lowerStrInspectIntrinsic(ret_ty, payload_expr, payload_ty);
            current = try self.lowerStrConcatExpr(ret_ty, current, inspected);
        }
        return try self.lowerStrConcatBytes(ret_ty, current, ")");
    }

    fn lowerStringLiteralExpr(
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        bytes: []const u8,
    ) Allocator.Error!Ast.ExprId {
        return try self.program.ast.addExpr(ret_ty, .{ .str_lit = try self.program.literal_pool.intern(bytes) });
    }

    fn lowerStrConcatBytes(
        self: *BodyLowerer,
        ret_ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs_bytes: []const u8,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerStrConcatExpr(ret_ty, lhs, try self.lowerStringLiteralExpr(ret_ty, rhs_bytes));
    }

    fn lowerStrConcatExpr(
        self: *BodyLowerer,
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

    fn lowerPromotedCallableWrapperDef(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        wrapper_id: canonical.PromotedCallableWrapperId,
    ) Allocator.Error!Ast.DefId {
        const wrapper = self.template_lookup.promoted_callable_wrappers.get(wrapper_id);
        const body_plan = self.template_lookup.promoted_callable_body_plans.get(wrapper.body_plan);
        const lowered = switch (body_plan) {
            .finite => |finite| blk: {
                const params = try self.lowerPromotedWrapperParamBundle(finite.params);
                defer if (params.exprs.len > 0) self.allocator.free(params.exprs);
                break :blk PromotedWrapperLowering{
                    .args = params.args,
                    .body = try self.lowerFinitePromotedCallableWrapperBody(reserved, fn_ty, wrapper_id, finite, params.exprs),
                };
            },
            .erased => invariantViolation("mono body lowering reached executable-owned erased promoted callable wrapper"),
            .pending => invariantViolation("mono body lowering reached unsealed promoted callable wrapper body plan"),
        };
        const bind = Ast.TypedSymbol{
            .ty = fn_ty,
            .source_ty = reserved.proc.specialization.requested_mono_fn_ty,
            .symbol = try self.program.addProcSymbol(reserved.local_handle),
        };
        return try self.program.ast.addDef(.{
            .proc = canonical.mirProcedureRefFromMono(reserved.proc),
            .debug_name = null,
            .value = .{ .fn_ = .{
                .source_fn_ty = reserved.proc.specialization.requested_mono_fn_ty,
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
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        wrapper_id: canonical.PromotedCallableWrapperId,
        finite: checked_artifact.FinitePromotedWrapperBodyPlan,
        params: []const Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        if (!std.mem.eql(u8, &reserved.proc.specialization.requested_mono_fn_ty.bytes, &finite.source_fn_ty.bytes)) {
            invariantViolation("promoted callable wrapper source function type disagrees with mono specialization request");
        }
        if (!std.mem.eql(u8, &finite.member_proc.source_fn_ty.bytes, &finite.source_fn_ty.bytes)) {
            invariantViolation("promoted callable wrapper member source function type disagrees with wrapper source function type");
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

        const member_proc = try self.reserveCallableProcedure(
            finite.member_proc,
            reserved.requested_fn_ty,
            .{ .promoted_callable_wrapper = wrapper_id },
        );
        const proc_value = try self.program.ast.addExprWithSource(fn_ty, finite.source_fn_ty, .{ .proc_value = .{
            .proc = member_proc,
            .captures = try self.program.ast.addCaptureArgSpan(capture_args),
            .fn_ty = fn_ty,
        } });

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

        return try self.program.ast.addExprWithSource(fn_ty, finite.source_fn_ty, .{ .call_value = .{
            .func = proc_value,
            .args = try self.program.ast.addExprSpan(call_args),
            .requested_fn_ty = fn_ty,
            .requested_source_fn_ty = finite.source_fn_ty,
        } });
    }

    const PromotedWrapperParamBundle = struct {
        args: Ast.Span(Ast.TypedSymbol),
        exprs: []const Ast.ExprId,
    };

    fn lowerPromotedWrapperParamBundle(
        self: *BodyLowerer,
        params: []const checked_artifact.PromotedWrapperParam,
    ) Allocator.Error!PromotedWrapperParamBundle {
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
            const ty = try self.type_instantiator.lowerTemplateType(param.checked_ty);
            const symbol = try self.program.addSyntheticSymbol();
            lowered_args[index] = .{
                .ty = ty,
                .source_ty = param.source_ty,
                .symbol = symbol,
            };
            lowered_exprs[index] = try self.program.ast.addExprWithSource(ty, param.source_ty, .{ .var_ = symbol });
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
        self: *BodyLowerer,
        capture: checked_artifact.PrivateCaptureRef,
    ) Allocator.Error!Ast.ExprId {
        const checked_types = checkedTypesForKey(self.input, capture.artifact) orelse {
            debug.invariant(false, "mono body lowering invariant violated: private capture artifact was not available");
            unreachable;
        };
        const scheme = checkedTypeSchemeForKey(checked_types, capture.source_scheme) orelse {
            debug.invariant(false, "mono body lowering invariant violated: private capture source scheme was not available");
            unreachable;
        };
        if (scheme.generalized_vars.len != 0) {
            invariantViolation("mono body lowering reached generalized private capture without a concrete instantiation");
        }
        const root_index: usize = @intFromEnum(scheme.root);
        if (root_index >= checked_types.roots.len) {
            invariantViolation("private capture source scheme root is outside checked type roots");
        }
        return try self.lowerPrivateCaptureNode(capture.artifact, capture.node, scheme.root);
    }

    fn lowerPrivateCaptureNode(
        self: *BodyLowerer,
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
        const checked_types = checkedTypesForKey(self.input, artifact) orelse {
            debug.invariant(false, "mono body lowering invariant violated: private capture type artifact was not available");
            unreachable;
        };
        const ty = try self.lowerArtifactCheckedType(artifact, checked_ty);
        const source_ty = checkedTypeKey(checked_types, checked_ty);

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
        self: *BodyLowerer,
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
        return try self.program.ast.addExprWithSource(ty, leaf.requested_source_ty, .{ .const_instance = leaf.const_instance });
    }

    fn lowerPrivateCallableLeaf(
        self: *BodyLowerer,
        ty: Type.TypeId,
        source_ty: canonical.CanonicalTypeKey,
        node_id: checked_artifact.PrivateCaptureNodeId,
        leaf: checked_artifact.FiniteCallableLeafInstance,
    ) Allocator.Error!Ast.ExprId {
        if (!std.mem.eql(u8, &leaf.proc_value.source_fn_ty.bytes, &source_ty.bytes)) {
            invariantViolation("private finite callable leaf source function type disagrees with materialization type");
        }
        const template = checkedTemplateFromCallableTemplate(leaf.proc_value.template);
        const artifact = artifactKeyForRef(self.input, template.artifact) orelse {
            debug.invariant(false, "mono body lowering invariant violated: private callable leaf template artifact was not available");
            unreachable;
        };
        const concrete = try self.concreteSourceTypeForCheckedKey(artifact, source_ty);
        const proc = try self.reserveCallableProcedure(
            leaf.proc_value,
            concrete,
            .{ .private_capture_callable_leaf = node_id },
        );
        return try self.program.ast.addExprWithSource(ty, source_ty, .{ .proc_value = .{
            .proc = proc,
            .captures = Ast.Span(Ast.CaptureArg).empty(),
            .fn_ty = ty,
        } });
    }

    fn lowerPrivateRecordCapture(
        self: *BodyLowerer,
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
        return try self.program.ast.addExprWithSource(
            ty,
            checkedTypeKey(checked_types, checked_ty),
            .{ .record = try self.program.ast.addFieldExprSpan(lowered) },
        );
    }

    fn lowerPrivateTupleCapture(
        self: *BodyLowerer,
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
        return try self.program.ast.addExprWithSource(
            ty,
            checkedTypeKey(checked_types, checked_ty),
            .{ .tuple = try self.program.ast.addExprSpan(lowered) },
        );
    }

    fn lowerPrivateTagCapture(
        self: *BodyLowerer,
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
        return try self.program.ast.addExprWithSource(ty, checkedTypeKey(checked_types, checked_ty), .{ .tag = .{
            .name = tag.tag,
            .discriminant = tag_info.discriminant,
            .args = try self.program.ast.addExprSpan(lowered),
            .constructor_ty = ty,
        } });
    }

    fn lowerPrivateListCapture(
        self: *BodyLowerer,
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
        return try self.program.ast.addExprWithSource(
            ty,
            checkedTypeKey(checkedTypesForKey(self.input, artifact) orelse unreachable, checked_ty),
            .{ .list = try self.program.ast.addExprSpan(lowered) },
        );
    }

    fn lowerPrivateBoxCapture(
        self: *BodyLowerer,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        ty: Type.TypeId,
        checked_ty: checked_artifact.CheckedTypeId,
        payload: checked_artifact.PrivateCaptureNodeId,
    ) Allocator.Error!Ast.ExprId {
        const payload_ty = privateBuiltinArgType(checkedTypesForKey(self.input, artifact) orelse unreachable, checked_ty, .box);
        const child = try self.lowerPrivateCaptureNode(artifact, payload, payload_ty);
        const args = [_]Ast.ExprId{child};
        return try self.program.ast.addExprWithSource(ty, checkedTypeKey(checkedTypesForKey(self.input, artifact) orelse unreachable, checked_ty), .{ .low_level = .{
            .op = .box_box,
            .rc_effect = base.LowLevel.box_box.rcEffect(),
            .args = try self.program.ast.addExprSpan(&args),
            .source_constraint_ty = ty,
        } });
    }

    fn lowerPrivateNominalCapture(
        self: *BodyLowerer,
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
                break :blk try self.program.ast.addExprWithSource(
                    ty,
                    checkedTypeKey(checked_types, checked_ty),
                    .{ .nominal_reinterpret = backing },
                );
            },
            .alias => |alias| try self.lowerPrivateCaptureNode(artifact, nominal.backing, alias.backing),
            else => invariantViolation("private capture nominal node had non-nominal checked type"),
        };
    }

    fn lowerArtifactCheckedType(
        self: *BodyLowerer,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!Type.TypeId {
        return try self.type_instantiator.lowerArtifactRef(.{
            .artifact = artifact,
            .ty = checked_ty,
        });
    }

    fn concreteSourceTypeForCheckedKey(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        callable: canonical.ProcedureCallableRef,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
        reason: MonoSpecializationReason,
    ) Allocator.Error!canonical.MirProcedureRef {
        const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
        if (!std.mem.eql(u8, &requested_key.bytes, &callable.source_fn_ty.bytes)) {
            invariantViolation("callable procedure reservation source function type disagrees with requested mono type");
        }
        const remapped_callable = try self.name_resolver.procedureCallableRef(callable);
        const template = checkedTemplateFromCallableTemplate(remapped_callable.template);
        const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
            .template = template,
            .requested_fn_ty = requested_fn_ty,
            .reason = reason,
        });
        return .{
            .proc = reserved.proc.proc,
            .callable = remapped_callable,
        };
    }

    fn lowerEntryWrapperDef(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        wrapper_id: canonical.EntryWrapperId,
    ) Allocator.Error!Ast.DefId {
        const entry_wrappers = self.template_lookup.entry_wrappers orelse {
            debug.invariant(false, "mono body lowering invariant violated: entry wrapper template came from a view without entry wrappers");
            unreachable;
        };
        const wrapper = entry_wrappers.get(wrapper_id);
        const body = try self.lowerExpr(wrapper.body_expr);
        const bind = Ast.TypedSymbol{
            .ty = fn_ty,
            .source_ty = reserved.proc.specialization.requested_mono_fn_ty,
            .symbol = try self.program.addProcSymbol(reserved.local_handle),
        };
        return try self.program.ast.addDef(.{
            .proc = canonical.mirProcedureRefFromMono(reserved.proc),
            .debug_name = null,
            .value = .{ .fn_ = .{
                .source_fn_ty = reserved.proc.specialization.requested_mono_fn_ty,
                .recursive = false,
                .bind = bind,
                .args = Ast.Span(Ast.TypedSymbol).empty(),
                .body = body,
            } },
        });
    }

    fn lowerCheckedBody(
        self: *BodyLowerer,
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
            .hosted_lambda => |hosted| try self.lowerHostedDef(reserved, fn_ty, hosted.symbol_name, hosted.args),
            .anno_only => invariantViolation("mono body lowering reached annotation-only procedure body without checked backing expression"),
            else => invariantViolation("mono body lowering expected a checked procedure body to be a lambda-like expression"),
        };
    }

    fn lowerHostedDef(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        symbol_name: canonical.ExternalSymbolNameId,
        arg_patterns: []const checked_artifact.CheckedPatternId,
    ) Allocator.Error!Ast.DefId {
        const args = try self.lowerParamSpanFromFunction(arg_patterns, reserved.requested_fn_ty);
        const hosted = try self.hostedProcForReserved(reserved.proc.proc, symbol_name);
        return try self.program.ast.addDef(.{
            .proc = canonical.mirProcedureRefFromMono(reserved.proc),
            .debug_name = null,
            .value = .{ .hosted_fn = .{
                .proc = reserved.proc.proc,
                .args = args,
                .ret_ty = self.functionReturnType(fn_ty),
                .hosted = hosted,
            } },
        });
    }

    fn functionReturnType(self: *const BodyLowerer, fn_ty: Type.TypeId) Type.TypeId {
        return switch (self.program.types.getTypePreservingNominal(fn_ty)) {
            .func => |func| func.ret,
            else => invariantViolation("mono body lowering expected hosted procedure type to be a function"),
        };
    }

    fn hostedProcForReserved(
        self: *BodyLowerer,
        proc: canonical.ProcedureValueRef,
        symbol_name: canonical.ExternalSymbolNameId,
    ) Allocator.Error!Hosted.Proc {
        for (self.template_lookup.hosted_procs.procs) |hosted| {
            if (!canonical.procedureValueRefEql(hosted.proc, proc)) continue;
            if (hosted.external_symbol_name != symbol_name) {
                invariantViolation("mono body lowering found hosted procedure metadata with a mismatched external symbol name");
            }
            return .{
                .external_symbol_name = try self.name_resolver.externalSymbolName(
                    self.template_lookup.artifact,
                    hosted.external_symbol_name,
                ),
                .dispatch_index = hosted.deterministic_index,
            };
        }

        invariantViolation("mono body lowering expected hosted procedure metadata published in the checked artifact");
    }

    fn lowerLambdaDef(
        self: *BodyLowerer,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
        arg_patterns: []const checked_artifact.CheckedPatternId,
        body_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.DefId {
        const args = try self.lowerParamSpanFromFunction(arg_patterns, reserved.requested_fn_ty);
        const ret_ty = try self.returnTypeFromConcreteFunction(reserved.requested_fn_ty);
        const body = try self.lowerExprConcreteExpected(body_expr, ret_ty);
        const bind = Ast.TypedSymbol{
            .ty = fn_ty,
            .source_ty = reserved.proc.specialization.requested_mono_fn_ty,
            .symbol = try self.program.addProcSymbol(reserved.local_handle),
        };
        return try self.program.ast.addDef(.{
            .proc = canonical.mirProcedureRefFromMono(reserved.proc),
            .debug_name = null,
            .value = .{ .fn_ = .{
                .source_fn_ty = reserved.proc.specialization.requested_mono_fn_ty,
                .recursive = false,
                .bind = bind,
                .args = args,
                .body = body,
            } },
        });
    }

    const ConcreteTypeInfo = struct {
        ty: Type.TypeId,
        source_ty: canonical.CanonicalTypeKey,
        source_ref: ConcreteSourceType.ConcreteSourceTypeRef,
    };

    const ExprExpectedType = union(enum) {
        checked: checked_artifact.CheckedTypeId,
        concrete: ConcreteTypeInfo,
    };

    fn lowerParamSpanFromFunction(
        self: *BodyLowerer,
        patterns: []const checked_artifact.CheckedPatternId,
        source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Ast.Span(Ast.TypedSymbol) {
        const param_types = try self.paramTypesFromConcreteFunction(source_fn);
        defer self.allocator.free(param_types);
        if (patterns.len != param_types.len) {
            invariantViolation("mono body lowering procedure parameter count disagreed with requested function type");
        }
        if (patterns.len == 0) return Ast.Span(Ast.TypedSymbol).empty();
        const args = try self.allocator.alloc(Ast.TypedSymbol, patterns.len);
        defer self.allocator.free(args);
        for (patterns, param_types, 0..) |pattern, param_ty, i| {
            args[i] = try self.lowerParamPatternWithType(pattern, param_ty);
        }
        return try self.program.ast.addTypedSymbolSpan(args);
    }

    fn paramTypesFromConcreteFunction(
        self: *BodyLowerer,
        source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error![]ConcreteTypeInfo {
        var current = source_fn;
        while (true) {
            switch (self.type_instantiator.concretePayload(current)) {
                .alias => |alias| {
                    current = try self.type_instantiator.concreteChildRef(current, alias.backing);
                },
                .function => |function| {
                    const out = try self.allocator.alloc(ConcreteTypeInfo, function.args.len);
                    errdefer self.allocator.free(out);
                    for (function.args, 0..) |arg, i| {
                        const arg_ref = try self.type_instantiator.concreteChildRef(current, arg);
                        out[i] = .{
                            .ty = try self.type_instantiator.lowerConcreteRef(arg_ref),
                            .source_ty = self.program.concrete_source_types.key(arg_ref),
                            .source_ref = arg_ref,
                        };
                    }
                    return out;
                },
                else => invariantViolation("mono body lowering expected requested procedure type to be a function"),
            }
        }
    }

    fn returnTypeFromConcreteFunction(
        self: *BodyLowerer,
        source_fn: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!ConcreteTypeInfo {
        var current = source_fn;
        while (true) {
            switch (self.type_instantiator.concretePayload(current)) {
                .alias => |alias| {
                    current = try self.type_instantiator.concreteChildRef(current, alias.backing);
                },
                .function => |function| {
                    const ret_ref = try self.type_instantiator.concreteChildRef(current, function.ret);
                    return .{
                        .ty = try self.type_instantiator.lowerConcreteRef(ret_ref),
                        .source_ty = self.program.concrete_source_types.key(ret_ref),
                        .source_ref = ret_ref,
                    };
                },
                else => invariantViolation("mono body lowering expected requested procedure type to be a function"),
            }
        }
    }

    fn lowerParamPattern(
        self: *BodyLowerer,
        pattern_id: checked_artifact.CheckedPatternId,
    ) Allocator.Error!Ast.TypedSymbol {
        const pattern = self.checkedPattern(pattern_id);
        const binder = self.binderForSimplePattern(pattern.data);
        const source_ref = try self.type_instantiator.concreteRefForTemplateType(pattern.ty);
        const concrete_ty = ConcreteTypeInfo{
            .ty = try self.type_instantiator.lowerConcreteRef(source_ref),
            .source_ty = self.program.concrete_source_types.key(source_ref),
            .source_ref = source_ref,
        };
        try self.recordConcreteTypeForBinder(binder, concrete_ty);
        return .{
            .ty = concrete_ty.ty,
            .source_ty = concrete_ty.source_ty,
            .symbol = try self.symbolForBinder(binder),
        };
    }

    fn lowerParamPatternWithType(
        self: *BodyLowerer,
        pattern_id: checked_artifact.CheckedPatternId,
        param_ty: ConcreteTypeInfo,
    ) Allocator.Error!Ast.TypedSymbol {
        const pattern = self.checkedPattern(pattern_id);
        const binder = self.binderForSimplePattern(pattern.data);
        try self.recordConcreteTypeForBinder(binder, param_ty);
        return .{
            .ty = param_ty.ty,
            .source_ty = param_ty.source_ty,
            .symbol = try self.symbolForBinder(binder),
        };
    }

    fn binderForSimplePattern(
        self: *BodyLowerer,
        data: checked_artifact.CheckedPatternData,
    ) checked_artifact.PatternBinderId {
        _ = self;
        return switch (data) {
            .assign => |binder| binder,
            .as => |as| as.binder,
            else => invariantViolation("mono body lowering requires destructuring parameters to be lowered into explicit local bindings before procedure entry"),
        };
    }

    fn symbolForBinder(
        self: *BodyLowerer,
        binder: checked_artifact.PatternBinderId,
    ) Allocator.Error!Ast.Symbol {
        if (self.local_symbols.get(binder)) |symbol| return symbol;
        const symbol = try self.program.addPatternBinderSymbol(binder);
        try self.local_symbols.put(binder, symbol);
        return symbol;
    }

    fn recordConcreteTypeForBinder(
        self: *BodyLowerer,
        binder: checked_artifact.PatternBinderId,
        ty: ConcreteTypeInfo,
    ) Allocator.Error!void {
        try self.local_symbol_types.put(binder, ty);
    }

    fn concreteTypeForLookupExpr(
        self: *const BodyLowerer,
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
        return self.local_symbol_types.get(binder);
    }

    fn lowerExpr(
        self: *BodyLowerer,
        expr_id: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        if (self.concreteTypeForLookupExpr(expr_id)) |lookup_ty| {
            return try self.lowerExprConcreteExpected(expr_id, lookup_ty);
        }
        return try self.lowerExprExpected(expr_id, self.checkedExpr(expr_id).ty);
    }

    fn lowerExprExpected(
        self: *BodyLowerer,
        expr_id: checked_artifact.CheckedExprId,
        expected_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerExprWithExpected(expr_id, .{ .checked = expected_ty });
    }

    fn lowerExprConcreteExpected(
        self: *BodyLowerer,
        expr_id: checked_artifact.CheckedExprId,
        expected_ty: ConcreteTypeInfo,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerExprWithExpected(expr_id, .{ .concrete = expected_ty });
    }

    fn expectedTypeInfo(
        self: *BodyLowerer,
        expected_ty: ExprExpectedType,
    ) Allocator.Error!ConcreteTypeInfo {
        return switch (expected_ty) {
            .checked => |checked_ty| blk: {
                const source_ref = try self.type_instantiator.concreteRefForTemplateType(checked_ty);
                break :blk .{
                    .ty = try self.type_instantiator.lowerConcreteRef(source_ref),
                    .source_ty = self.program.concrete_source_types.key(source_ref),
                    .source_ref = source_ref,
                };
            },
            .concrete => |concrete| concrete,
        };
    }

    fn lowerExprWithExpected(
        self: *BodyLowerer,
        expr_id: checked_artifact.CheckedExprId,
        expected_ty: ExprExpectedType,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.checkedExpr(expr_id);
        const expected_info = try self.expectedTypeInfo(expected_ty);
        try self.type_instantiator.unifyTemplateWithConcrete(expr.ty, expected_info.source_ref);
        const ty = expected_info.ty;
        const source_ty = expected_info.source_ty;
        const lowered = switch (expr.data) {
            .num => |num| try self.lowerIntegerLiteralExpr(ty, num.value),
            .typed_int => |num| try self.lowerIntegerLiteralExpr(ty, num.value),
            .frac_f32 => |frac| try self.program.ast.addExpr(ty, .{ .frac_f32_lit = frac.value }),
            .frac_f64 => |frac| try self.program.ast.addExpr(ty, .{ .frac_f64_lit = frac.value }),
            .dec => |dec| try self.lowerScaledDecimalLiteralExpr(ty, dec.value.num),
            .dec_small => |dec| try self.lowerScaledDecimalLiteralExpr(ty, dec.value.toRocDec().num),
            .typed_frac => |frac| try self.lowerScaledDecimalLiteralExpr(ty, frac.value.toI128()),
            .str_segment => |literal| try self.program.ast.addExpr(ty, .{ .str_lit = try self.lowerCheckedStringLiteral(literal) }),
            .str => |segments| try self.lowerStringExpr(ty, segments),
            .bytes_literal => |literal| try self.lowerBytesLiteral(ty, literal),
            .lookup_local => |lookup| try self.lowerResolvedLookup(ty, lookup.resolved orelse invariantViolation("checked lookup_local reached mono without a resolved value ref")),
            .lookup_external => |ref_id| try self.lowerResolvedLookup(ty, ref_id orelse invariantViolation("checked lookup_external reached mono without a resolved value ref")),
            .lookup_required => |ref_id| try self.lowerResolvedLookup(ty, ref_id orelse invariantViolation("checked lookup_required reached mono without a resolved value ref")),
            .list => |items| try self.lowerList(ty, items),
            .empty_list => try self.program.ast.addExpr(ty, .{ .list = Ast.Span(Ast.ExprId).empty() }),
            .tuple => |items| try self.lowerTuple(ty, items),
            .block => |block| try self.lowerBlock(ty, block.statements, block.final_expr, expected_ty),
            .record => |record| try self.lowerRecord(ty, record),
            .empty_record => try self.program.ast.addExpr(ty, .{ .record = Ast.Span(Ast.FieldExpr).empty() }),
            .lambda => |lambda| try self.lowerClosureExpr(ty, expr_id, .local_function, lambda.args, lambda.body),
            .call => |call| try self.lowerCall(ty, expr_id, call),
            .structural_eq => |eq| try self.lowerStructuralEq(ty, eq),
            .unary_not => |child| blk: {
                const value = try self.lowerBoolConditionExpr(child);
                break :blk try self.program.ast.addExpr(ty, .{ .bool_not = value });
            },
            .if_ => |if_| try self.lowerIf(ty, if_.branches, if_.final_else, expected_ty),
            .match_ => |match_| try self.lowerMatch(ty, match_, expected_ty),
            .tag => |tag| try self.lowerTag(ty, expected_info.source_ref, try self.tagLabel(tag.name), tag.args),
            .zero_argument_tag => |tag| blk: {
                _ = tag.closure_name;
                break :blk try self.lowerTag(ty, expected_info.source_ref, try self.tagLabel(tag.name), &.{});
            },
            .closure => |closure| try self.lowerCheckedClosureExpr(ty, expr_id, closure),
            .field_access => |access| try self.lowerFieldAccess(ty, access.receiver, try self.recordFieldLabel(access.field_name)),
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                break :blk try self.program.ast.addExpr(ty, .{ .tuple_access = .{
                    .tuple = tuple,
                    .elem_index = access.elem_index,
                } });
            },
            .return_ => |ret| blk: {
                _ = ret.lambda;
                _ = ret.context;
                const child = try self.lowerExprWithExpected(ret.expr, expected_ty);
                break :blk try self.program.ast.addExpr(ty, .{ .return_ = child });
            },
            .binop => |binop| try self.lowerBinop(ty, binop),
            .unary_minus => |child| try self.lowerUnaryMinus(ty, child),
            .for_ => |for_| try self.lowerForExpr(ty, for_.pattern, for_.expr, for_.body),
            .run_low_level => |run_low_level| try self.lowerRunLowLevel(ty, run_low_level.op, run_low_level.args),
            .nominal => |nominal| blk: {
                _ = nominal.backing_type;
                const backing = try self.lowerExpr(nominal.backing_expr);
                break :blk try self.program.ast.addExpr(ty, .{ .nominal_reinterpret = backing });
            },
            .dispatch_call => |plan| try self.lowerStaticDispatch(ty, plan orelse invariantViolation("checked dispatch call reached mono without a StaticDispatchCallPlan")),
            .method_eq => |plan| try self.lowerStaticDispatch(ty, plan orelse invariantViolation("checked method equality reached mono without a StaticDispatchCallPlan")),
            .type_dispatch_call => |plan| try self.lowerStaticDispatch(ty, plan orelse invariantViolation("checked type dispatch call reached mono without a StaticDispatchCallPlan")),
            .hosted_lambda => invariantViolation("mono body lowering reached hosted lambda as an expression; hosted lambdas must be published as procedure templates"),
            .dbg => |child| blk: {
                const value = try self.lowerExpr(child);
                break :blk try self.program.ast.addExpr(ty, .{ .inspect = value });
            },
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
        self.program.ast.setExprSourceTy(lowered, source_ty);
        return lowered;
    }

    fn lowerBoolConditionExpr(
        self: *BodyLowerer,
        expr_id: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.checkedExpr(expr_id);
        const bool_ty = try self.ensurePrimitiveBoolType();
        const bool_source_ty = try self.program.boolSourceTypeKey();
        const lowered = switch (expr.data) {
            .tag => |tag| try self.lowerTag(bool_ty, null, try self.tagLabel(tag.name), tag.args),
            .zero_argument_tag => |tag| blk: {
                _ = tag.closure_name;
                break :blk try self.lowerTag(bool_ty, null, try self.tagLabel(tag.name), &.{});
            },
            .unary_not => |child| blk: {
                const value = try self.lowerBoolConditionExpr(child);
                break :blk try self.program.ast.addExpr(bool_ty, .{ .bool_not = value });
            },
            else => return try self.lowerExpr(expr_id),
        };
        self.program.ast.setExprSourceTy(lowered, bool_source_ty);
        return lowered;
    }

    fn ensurePrimitiveBoolType(self: *BodyLowerer) Allocator.Error!Type.TypeId {
        return try self.program.types.internResolved(.{ .primitive = .bool });
    }

    fn lowerIntegerLiteralExpr(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
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

    fn lowerStringExpr(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        ty: Type.TypeId,
        ref_id: checked_artifact.ResolvedValueRefId,
    ) Allocator.Error!Ast.ExprId {
        const record = self.resolvedValueRef(ref_id);
        return switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            => |local| if (self.local_proc_decls.contains(local.binder))
                try self.lowerLocalProcLookup(ty, record, local)
            else
                try self.program.ast.addExpr(ty, .{ .var_ = try self.symbolForBinder(local.binder) }),
            .local_proc => |local| try self.lowerLocalProcLookup(ty, record, local),
            .top_level_const,
            .imported_const,
            .platform_required_const,
            => |const_use| try self.lowerConstUse(ty, const_use, record.checked_ty),
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_proc,
            .promoted_top_level_proc,
            => |proc_use| try self.program.ast.addExpr(ty, .{ .proc_value = .{
                .proc = try self.reserveProcedureUse(proc_use, record.checked_ty, .{ .proc_value = record.expr }),
                .captures = Ast.Span(Ast.CaptureArg).empty(),
                .fn_ty = ty,
            } }),
            .platform_required_declaration => invariantViolation("mono body lowering reached platform-required declaration lookup as a runtime value"),
        };
    }

    fn lowerLocalProcLookup(
        self: *BodyLowerer,
        ty: Type.TypeId,
        record: checked_artifact.ResolvedValueRefRecord,
        local: checked_artifact.LocalBindingRef,
    ) Allocator.Error!Ast.ExprId {
        const symbol = try self.ensureLocalProcInstance(local.binder, record.checked_ty);
        return try self.program.ast.addExpr(ty, .{ .var_ = symbol });
    }

    fn ensureLocalProcInstance(
        self: *BodyLowerer,
        binder: checked_artifact.PatternBinderId,
        use_checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!Ast.Symbol {
        const concrete_fn = try self.type_instantiator.concreteRefForTemplateType(use_checked_ty);
        return try self.ensureLocalProcInstanceForConcrete(binder, concrete_fn);
    }

    fn ensureLocalProcInstanceForConcrete(
        self: *BodyLowerer,
        binder: checked_artifact.PatternBinderId,
        concrete_fn: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Ast.Symbol {
        const source_fn_ty = self.program.concrete_source_types.key(concrete_fn);
        const key = LocalProcInstanceKey{
            .binder = binder,
            .source_fn_ty = source_fn_ty,
        };
        const decl = self.local_proc_decls.get(binder) orelse {
            invariantViolation("mono body lowering reached local procedure lookup without a published local procedure declaration");
        };
        if (self.local_proc_instances.get(key)) |existing| return existing;

        const source_symbol = try self.symbolForBinder(binder);
        const instance_symbol = try self.program.addSpecializedLocalFnSymbol(source_symbol);
        try self.local_proc_instances.put(key, instance_symbol);
        errdefer _ = self.local_proc_instances.remove(key);

        const local_fn = try self.lowerLocalProcInstance(decl, concrete_fn, source_fn_ty, instance_symbol);
        const stmt = try self.program.ast.addStmt(.{ .local_fn = local_fn });
        try decl.owner_generated_stmts.append(self.allocator, stmt);
        return instance_symbol;
    }

    fn lowerLocalProcInstance(
        self: *BodyLowerer,
        decl: LocalProcDecl,
        concrete_fn: ConcreteSourceType.ConcreteSourceTypeRef,
        source_fn_ty: canonical.CanonicalTypeKey,
        instance_symbol: Ast.Symbol,
    ) Allocator.Error!Ast.LetFn {
        const lambda = self.localProcLambda(decl.expr);

        var instantiator = try self.type_instantiator.fork();
        defer instantiator.deinit();
        try instantiator.unifyTemplateWithConcrete(self.checkedExpr(decl.expr).ty, concrete_fn);

        const previous = self.type_instantiator;
        self.type_instantiator = &instantiator;
        defer self.type_instantiator = previous;

        const fn_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(decl.expr).ty);
        return .{
            .site = self.nestedProcSite(decl.expr, lambda.kind),
            .source_fn_ty = source_fn_ty,
            .recursive = false,
            .bind = .{
                .ty = fn_ty,
                .source_ty = source_fn_ty,
                .symbol = instance_symbol,
            },
            .args = try self.lowerParamSpanFromFunction(lambda.args, concrete_fn),
            .body = try self.lowerExprConcreteExpected(lambda.body, try self.returnTypeFromConcreteFunction(concrete_fn)),
        };
    }

    const LocalProcLambda = struct {
        kind: checked_artifact.NestedProcKind,
        args: []const checked_artifact.CheckedPatternId,
        body: checked_artifact.CheckedExprId,
    };

    fn localProcLambda(self: *BodyLowerer, expr_id: checked_artifact.CheckedExprId) LocalProcLambda {
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
        self: *BodyLowerer,
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
        self: *BodyLowerer,
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

    fn lowerConstUse(
        self: *BodyLowerer,
        ty: Type.TypeId,
        const_use: checked_artifact.ConstUseTemplate,
        checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!Ast.ExprId {
        const concrete_ref = try self.type_instantiator.concreteRefForTemplateType(checked_ty);
        const requested_key = self.program.concrete_source_types.key(concrete_ref);
        const key = checked_artifact.ConstInstantiationKey{
            .const_ref = const_use.const_ref,
            .requested_source_ty = requested_key,
        };
        const instance = constInstanceForKey(self.input, self.input.root.artifact.key, key) orelse {
            switch (self.input.mode) {
                .comptime_dependency_summary => return try self.program.ast.addExpr(ty, .{ .const_ref = key }),
                .runnable => {
                    debug.invariant(false, "mono body lowering invariant violated: constant use had no sealed concrete instance in the requesting artifact");
                    unreachable;
                },
            }
        };
        var dependency_state = ConcreteDependencyReservationState.init(self.allocator);
        defer dependency_state.deinit();
        try reserveConstInstanceRefDependencies(self.input, self.program, self.queue, &dependency_state, instance);
        return try self.program.ast.addExpr(ty, .{ .const_instance = instance });
    }

    fn lowerList(
        self: *BodyLowerer,
        ty: Type.TypeId,
        items: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const span = try self.lowerExprSpan(items);
        return try self.program.ast.addExpr(ty, .{ .list = span });
    }

    fn lowerTuple(
        self: *BodyLowerer,
        ty: Type.TypeId,
        items: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const span = try self.lowerExprSpan(items);
        return try self.program.ast.addExpr(ty, .{ .tuple = span });
    }

    fn lowerBlock(
        self: *BodyLowerer,
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

        for (statements) |statement_id| {
            const statement = self.checkedStatement(statement_id);
            const decl = self.localProcDeclForStatement(statement) orelse continue;
            const binder = self.binderForSimplePattern(self.checkedPattern(decl.pattern).data);
            const previous = try self.local_proc_decls.fetchPut(binder, .{
                .pattern = decl.pattern,
                .expr = decl.expr,
                .owner_generated_stmts = &generated_local_fns,
            });
            try restorations.append(self.allocator, .{
                .binder = binder,
                .previous = if (previous) |entry| entry.value else null,
            });
        }

        var lowered_stmts = std.ArrayList(Ast.StmtId).empty;
        defer lowered_stmts.deinit(self.allocator);
        for (statements) |statement_id| {
            const statement = self.checkedStatement(statement_id);
            if (self.localProcDeclForStatement(statement) != null) continue;
            try lowered_stmts.append(self.allocator, try self.lowerStmt(statement_id));
        }

        const final = try self.lowerExprWithExpected(final_expr, final_expected_ty);
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

    fn lowerRecord(
        self: *BodyLowerer,
        ty: Type.TypeId,
        record: anytype,
    ) Allocator.Error!Ast.ExprId {
        if (record.ext) |ext| return try self.lowerRecordUpdate(ty, ext, record.fields);
        if (record.fields.len == 0) return try self.program.ast.addExpr(ty, .{ .record = Ast.Span(Ast.FieldExpr).empty() });
        const fields = try self.allocator.alloc(Ast.FieldExpr, record.fields.len);
        defer self.allocator.free(fields);
        for (record.fields, 0..) |field, i| {
            fields[i] = .{
                .field = try self.recordFieldLabel(field.label),
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.program.ast.addExpr(ty, .{ .record = try self.program.ast.addFieldExprSpan(fields) });
    }

    fn lowerRecordUpdate(
        self: *BodyLowerer,
        ty: Type.TypeId,
        ext: checked_artifact.CheckedExprId,
        update_fields: []const checked_artifact.CheckedRecordExprField,
    ) Allocator.Error!Ast.ExprId {
        const ext_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(ext).ty);
        const ext_source_ty = try self.sourceTypeKey(self.checkedExpr(ext).ty);
        const ext_expr = try self.lowerExpr(ext);
        const ext_symbol = try self.program.symbols.add(base.Ident.Idx.NONE, .synthetic);
        const ext_var = try self.program.ast.addExprWithSource(ext_ty, ext_source_ty, .{ .var_ = ext_symbol });

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
                    .symbol = ext_symbol,
                },
                .body = ext_expr,
            } },
            .rest = rest,
        } });
    }

    fn lowerClosureExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        site_expr: checked_artifact.CheckedExprId,
        site_kind: checked_artifact.NestedProcKind,
        arg_patterns: []const checked_artifact.CheckedPatternId,
        body_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const source_fn_ref = try self.type_instantiator.concreteRefForTemplateType(self.checkedExpr(site_expr).ty);
        const args = try self.lowerParamSpanFromFunction(arg_patterns, source_fn_ref);
        const body = try self.lowerExprConcreteExpected(body_expr, try self.returnTypeFromConcreteFunction(source_fn_ref));
        const source_fn_ty = self.program.concrete_source_types.key(source_fn_ref);
        return try self.program.ast.addExpr(ty, .{ .clos = .{
            .site = self.nestedProcSite(site_expr, site_kind),
            .source_fn_ty = source_fn_ty,
            .args = args,
            .body = body,
        } });
    }

    fn lowerCheckedClosureExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        expr_id: checked_artifact.CheckedExprId,
        closure: anytype,
    ) Allocator.Error!Ast.ExprId {
        _ = closure.captures;
        _ = closure.tag_name;
        const lambda_expr = self.checkedExpr(closure.lambda);
        return switch (lambda_expr.data) {
            .lambda => |lambda| try self.lowerClosureExpr(ty, expr_id, .closure, lambda.args, lambda.body),
            else => invariantViolation("mono body lowering expected closure expression to reference a checked lambda"),
        };
    }

    fn lowerCall(
        self: *BodyLowerer,
        ty: Type.TypeId,
        call_expr: checked_artifact.CheckedExprId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        _ = call.called_via;
        if (self.procedureUseForExpr(call.func)) |proc_use| {
            const args = try self.lowerExprSpan(call.args);
            const concrete_fn = try self.type_instantiator.concreteRefForTemplateType(call.source_fn_ty_payload);
            const func_ty = try self.type_instantiator.lowerConcreteRef(concrete_fn);
            const requested_source_fn_ty = self.program.concrete_source_types.key(concrete_fn);
            const proc = try self.reserveProcedureUse(proc_use, call.source_fn_ty_payload, .{ .call_proc = call_expr });
            return try self.program.ast.addExpr(ty, .{ .call_proc = .{
                .proc = proc,
                .args = args,
                .requested_fn_ty = func_ty,
                .requested_source_fn_ty = requested_source_fn_ty,
            } });
        }
        if (try self.localProcUseForExpr(call.func)) |local_proc| {
            const concrete_fn = try self.type_instantiator.concreteRefForTemplateType(call.source_fn_ty_payload);
            const func_ty = try self.type_instantiator.lowerConcreteRef(concrete_fn);
            const requested_source_fn_ty = self.program.concrete_source_types.key(concrete_fn);
            const symbol = try self.ensureLocalProcInstanceForConcrete(local_proc.binder, concrete_fn);
            const func = try self.program.ast.addExpr(func_ty, .{ .var_ = symbol });
            const args = try self.lowerExprSpan(call.args);
            return try self.program.ast.addExpr(ty, .{ .call_value = .{
                .func = func,
                .args = args,
                .requested_fn_ty = func_ty,
                .requested_source_fn_ty = requested_source_fn_ty,
            } });
        }

        const func = try self.lowerExpr(call.func);
        const args = try self.lowerExprSpan(call.args);
        const concrete_fn = try self.type_instantiator.concreteRefForTemplateType(call.source_fn_ty_payload);
        const func_ty = try self.type_instantiator.lowerConcreteRef(concrete_fn);
        const requested_source_fn_ty = self.program.concrete_source_types.key(concrete_fn);
        return try self.program.ast.addExpr(ty, .{ .call_value = .{
            .func = func,
            .args = args,
            .requested_fn_ty = func_ty,
            .requested_source_fn_ty = requested_source_fn_ty,
        } });
    }

    fn lowerStructuralEq(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        ty: Type.TypeId,
        plan_id: checked_artifact.StaticDispatchPlanId,
    ) Allocator.Error!Ast.ExprId {
        const plan = self.staticDispatchPlan(plan_id);
        const callable_ty = try self.type_instantiator.lowerTemplateType(plan.callable_ty);
        const callable = switch (self.program.types.getType(callable_ty)) {
            .func => |func| func,
            else => invariantViolation("mono static dispatch callable type did not resolve to a fixed-arity function"),
        };
        if (callable.args.len != plan.args.len) invariantViolation("mono static dispatch argument count did not match callable arity");

        const lowered_args = try self.lowerExprSpan(plan.args);
        const arg_items = self.program.ast.sliceExprSpan(lowered_args);
        for (arg_items, callable.args) |arg, expected_arg_ty| {
            const actual_arg_ty = self.program.ast.getExpr(arg).ty;
            if (!self.program.types.equalIds(actual_arg_ty, expected_arg_ty)) {
                invariantViolation("mono static dispatch argument type did not match callable type");
            }
        }

        const dispatcher_ty = try self.type_instantiator.lowerTemplateType(plan.dispatcher_ty);
        const owner = methodOwnerForDispatcherType(&self.program.types, dispatcher_ty);
        const method = try self.methodName(plan.method);
        const target = try self.lookupMethodTarget(owner, method);

        if (target) |method_target| {
            const template = try self.name_resolver.procedureTemplateRef(method_target.template orelse invariantViolation("mono static dispatch method target did not publish a checked procedure template"));
            const requested_fn_ty = try self.type_instantiator.concreteRefForTemplateType(plan.callable_ty);
            const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
                .template = template,
                .requested_fn_ty = requested_fn_ty,
                .reason = .{ .static_dispatch_target = plan_id },
            });
            const call_expr = try self.program.ast.addExpr(ty, .{ .call_proc = .{
                .proc = canonical.mirProcedureRefFromMono(reserved.proc),
                .args = lowered_args,
                .requested_fn_ty = callable_ty,
                .requested_source_fn_ty = self.program.concrete_source_types.key(requested_fn_ty),
            } });
            return switch (plan.result_mode) {
                .value => call_expr,
                .equality => |equality| if (equality.negated)
                    try self.program.ast.addExpr(ty, .{ .bool_not = call_expr })
                else
                    call_expr,
            };
        }

        return switch (plan.result_mode) {
            .value => invariantViolation("mono static dispatch value call had no checked method target"),
            .equality => |equality| blk: {
                if (!equality.structural_allowed) invariantViolation("mono static dispatch equality had no checked method target and structural equality is not allowed");
                if (plan.args.len != 2) invariantViolation("mono static dispatch equality did not have exactly two operands");
                const lhs = arg_items[0];
                const rhs = arg_items[1];
                const structural = try self.program.ast.addExpr(ty, .{ .structural_eq = .{ .lhs = lhs, .rhs = rhs } });
                if (!equality.negated) break :blk structural;
                break :blk try self.program.ast.addExpr(ty, .{ .bool_not = structural });
            },
        };
    }

    fn lookupMethodTarget(
        self: *BodyLowerer,
        owner: static_dispatch.MethodOwner,
        method: canonical.MethodNameId,
    ) Allocator.Error!?static_dispatch.MethodTarget {
        return try lookupStaticDispatchMethodTarget(
            self.input,
            self.name_resolver,
            self.template_lookup.artifact,
            owner,
            method,
        );
    }

    fn lowerIf(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        ty: Type.TypeId,
        match_: anytype,
        expected_result_ty: ExprExpectedType,
    ) Allocator.Error!Ast.ExprId {
        const cond = try self.lowerExpr(match_.cond);
        if (match_.branches.len == 0) invariantViolation("mono body lowering received a checked match with no branches");

        var branches = std.ArrayList(Ast.Branch).empty;
        defer branches.deinit(self.allocator);
        const cond_ty = self.program.ast.getExpr(cond).ty;
        for (match_.branches) |branch| {
            if (branch.patterns.len == 0) invariantViolation("mono body lowering received a checked match branch with no alternatives");
            for (branch.patterns) |branch_pattern| {
                try branches.append(self.allocator, .{
                    .pat = try self.lowerPatternWithRemaps(cond_ty, branch_pattern.pattern, branch_pattern.binder_remaps),
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
        self: *BodyLowerer,
        ty: Type.TypeId,
        source_ref: ?ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.TagLabelId,
        args: []const checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        if (self.boolLiteralForTagName(ty, name)) |literal| {
            if (args.len != 0) invariantViolation("mono body lowering Bool tag constructor cannot have a payload");
            return try self.program.ast.addExpr(ty, .{ .bool_lit = literal });
        }

        const tag_info = self.tagInfoForUnionType(ty, name);
        if (tag_info.payload_count != args.len) invariantViolation("mono body lowering tag constructor arity did not match its resolved type");
        const payload_types = try self.concreteTagPayloadInfosForUnionType(
            source_ref orelse invariantViolation("mono body lowering tag constructor had no concrete source type"),
            name,
        );
        defer if (payload_types.len != 0) self.allocator.free(payload_types);
        if (payload_types.len != args.len) invariantViolation("mono body lowering concrete tag payload arity did not match resolved type");
        return try self.program.ast.addExpr(ty, .{ .tag = .{
            .name = name,
            .discriminant = tag_info.discriminant,
            .args = try self.lowerExprSpanConcrete(args, payload_types),
            .constructor_ty = ty,
        } });
    }

    fn lowerFieldAccess(
        self: *BodyLowerer,
        ty: Type.TypeId,
        receiver: checked_artifact.CheckedExprId,
        field_name: canonical.RecordFieldLabelId,
    ) Allocator.Error!Ast.ExprId {
        const record = try self.lowerExpr(receiver);
        const record_ty = self.program.ast.getExpr(record).ty;
        return try self.program.ast.addExpr(ty, .{ .access = .{
            .record = record,
            .field = field_name,
            .field_index = self.recordFieldIndex(record_ty, field_name),
        } });
    }

    fn lowerForExpr(
        self: *BodyLowerer,
        ty: Type.TypeId,
        pattern: checked_artifact.CheckedPatternId,
        iterable: checked_artifact.CheckedExprId,
        body: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const pattern_ty = try self.type_instantiator.lowerTemplateType(self.checkedPattern(pattern).ty);
        return try self.program.ast.addExpr(ty, .{ .for_ = .{
            .patt = try self.lowerPattern(pattern_ty, pattern),
            .iterable = try self.lowerExpr(iterable),
            .body = try self.lowerExpr(body),
        } });
    }

    fn lowerRunLowLevel(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        ty: Type.TypeId,
        binop: anytype,
    ) Allocator.Error!Ast.ExprId {
        return switch (binop.op) {
            .add => try self.lowerBinaryLowLevel(ty, .num_plus, binop.lhs, binop.rhs),
            .sub => try self.lowerBinaryLowLevel(ty, .num_minus, binop.lhs, binop.rhs),
            .mul => try self.lowerBinaryLowLevel(ty, .num_times, binop.lhs, binop.rhs),
            .div => try self.lowerBinaryLowLevel(ty, .num_div_by, binop.lhs, binop.rhs),
            .rem => try self.lowerBinaryLowLevel(ty, .num_rem_by, binop.lhs, binop.rhs),
            .div_trunc => try self.lowerBinaryLowLevel(ty, .num_div_trunc_by, binop.lhs, binop.rhs),
            .lt => try self.lowerBinaryLowLevel(ty, .num_is_lt, binop.lhs, binop.rhs),
            .gt => try self.lowerBinaryLowLevel(ty, .num_is_gt, binop.lhs, binop.rhs),
            .le => try self.lowerBinaryLowLevel(ty, .num_is_lte, binop.lhs, binop.rhs),
            .ge => try self.lowerBinaryLowLevel(ty, .num_is_gte, binop.lhs, binop.rhs),
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
                const false_expr = try self.program.ast.addExpr(ty, .{ .bool_lit = false });
                break :blk try self.program.ast.addExpr(ty, .{ .if_ = .{
                    .cond = try self.lowerBoolConditionExpr(binop.lhs),
                    .then_body = try self.lowerBoolConditionExpr(binop.rhs),
                    .else_body = false_expr,
                } });
            },
            .@"or" => blk: {
                const true_expr = try self.program.ast.addExpr(ty, .{ .bool_lit = true });
                break :blk try self.program.ast.addExpr(ty, .{ .if_ = .{
                    .cond = try self.lowerBoolConditionExpr(binop.lhs),
                    .then_body = true_expr,
                    .else_body = try self.lowerBoolConditionExpr(binop.rhs),
                } });
            },
        };
    }

    fn lowerBinaryLowLevel(
        self: *BodyLowerer,
        ty: Type.TypeId,
        op: base.LowLevel,
        lhs_expr: checked_artifact.CheckedExprId,
        rhs_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const lhs = try self.lowerExpr(lhs_expr);
        const rhs = try self.lowerExpr(rhs_expr);
        const args = [_]Ast.ExprId{ lhs, rhs };
        const source_constraint_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(lhs_expr).ty);
        return try self.program.ast.addExpr(ty, .{ .low_level = .{
            .op = op,
            .rc_effect = op.rcEffect(),
            .args = try self.program.ast.addExprSpan(&args),
            .source_constraint_ty = source_constraint_ty,
        } });
    }

    fn lowerUnaryMinus(
        self: *BodyLowerer,
        ty: Type.TypeId,
        child_expr: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const child = try self.lowerExpr(child_expr);
        const args = [_]Ast.ExprId{child};
        const source_constraint_ty = try self.type_instantiator.lowerTemplateType(self.checkedExpr(child_expr).ty);
        return try self.program.ast.addExpr(ty, .{ .low_level = .{
            .op = .num_negate,
            .rc_effect = base.LowLevel.num_negate.rcEffect(),
            .args = try self.program.ast.addExprSpan(&args),
            .source_constraint_ty = source_constraint_ty,
        } });
    }

    fn lowerPattern(
        self: *BodyLowerer,
        expected_ty: Type.TypeId,
        pattern_id: checked_artifact.CheckedPatternId,
    ) Allocator.Error!Ast.PatId {
        return try self.lowerPatternWithRemaps(expected_ty, pattern_id, &.{});
    }

    fn lowerPatternWithRemaps(
        self: *BodyLowerer,
        expected_ty: Type.TypeId,
        pattern_id: checked_artifact.CheckedPatternId,
        binder_remaps: []const checked_artifact.CheckedAlternativeBinderRemap,
    ) Allocator.Error!Ast.PatId {
        const pattern = self.checkedPattern(pattern_id);
        const ty = try self.type_instantiator.lowerTemplateType(pattern.ty);
        const source_ty = try self.sourceTypeKey(pattern.ty);
        if (!self.program.types.equalIds(ty, expected_ty)) invariantViolation("mono body lowering pattern type did not match its scrutinee type");
        const lowered = switch (pattern.data) {
            .assign => |binder| try self.program.ast.addPat(.{ .ty = ty, .data = .{
                .var_ = try self.symbolForBinder(self.representativeBinderForCandidate(binder, binder_remaps)),
            } }),
            .as => |as| {
                const symbol = try self.symbolForBinder(self.representativeBinderForCandidate(as.binder, binder_remaps));
                const nested = try self.lowerPatternWithRemaps(ty, as.pattern, binder_remaps);
                return try self.program.ast.addPat(.{ .ty = ty, .data = .{ .as = .{
                    .pattern = nested,
                    .symbol = symbol,
                } } });
            },
            .applied_tag => |tag| blk: {
                const tag_name = try self.tagLabel(tag.name);
                if (self.boolLiteralForTagName(ty, tag_name)) |literal| {
                    if (tag.args.len != 0) invariantViolation("mono body lowering Bool tag pattern cannot have a payload");
                    break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .bool_lit = literal } });
                }

                const tag_info = self.tagInfoForUnionType(ty, tag_name);
                if (tag_info.payload_count != tag.args.len) invariantViolation("mono body lowering tag pattern arity did not match its resolved type");
                const args = try self.allocator.alloc(Ast.PatId, tag.args.len);
                defer self.allocator.free(args);
                for (tag.args, 0..) |arg, i| {
                    args[i] = try self.lowerPatternWithRemaps(tag_info.payload_types[i], arg, binder_remaps);
                }
                break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .tag = .{
                    .name = tag_name,
                    .discriminant = tag_info.discriminant,
                    .args = try self.program.ast.addPatSpan(args),
                } } });
            },
            .nominal => |nominal| blk: {
                const backing = self.nominalBackingType(ty);
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
                            const label = destruct.label;
                            fields[field_count] = .{
                                .field = label,
                                .pattern = try self.lowerPatternWithRemaps(self.recordFieldType(ty, label), field_pattern, binder_remaps),
                            };
                            field_count += 1;
                        },
                        .rest => |rest_pattern| {
                            if (rest != null) invariantViolation("mono body lowering record pattern had duplicate rest binders");
                            const rest_checked = self.checkedPattern(rest_pattern);
                            const rest_ty = try self.type_instantiator.lowerTemplateType(rest_checked.ty);
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
                const item_types = self.tupleElementTypes(ty, items.len);
                const lowered = try self.allocator.alloc(Ast.PatId, items.len);
                defer self.allocator.free(lowered);
                for (items, 0..) |item, i| {
                    lowered[i] = try self.lowerPatternWithRemaps(item_types[i], item, binder_remaps);
                }
                break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .tuple = try self.program.ast.addPatSpan(lowered) } });
            },
            .list => |list| blk: {
                const elem_ty = self.listElementType(ty);
                const lowered = try self.allocator.alloc(Ast.PatId, list.patterns.len);
                defer self.allocator.free(lowered);
                for (list.patterns, 0..) |item, i| {
                    lowered[i] = try self.lowerPatternWithRemaps(elem_ty, item, binder_remaps);
                }
                const rest: ?Ast.ListRestPattern = if (list.rest) |rest_info| .{
                    .index = rest_info.index,
                    .pattern = if (rest_info.pattern) |rest_pattern| try self.lowerPatternWithRemaps(ty, rest_pattern, binder_remaps) else null,
                } else null;
                break :blk try self.program.ast.addPat(.{ .ty = ty, .data = .{ .list = .{
                    .items = try self.program.ast.addPatSpan(lowered),
                    .rest = rest,
                } } });
            },
            .num_literal => |num| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .int_lit = num.value.toI128() } }),
            .small_dec_literal => |dec| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .dec_lit = dec.value.toRocDec().num } }),
            .dec_literal => |dec| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .dec_lit = dec.value.num } }),
            .frac_f32_literal => |value| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f32_lit = value } }),
            .frac_f64_literal => |value| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .frac_f64_lit = value } }),
            .str_literal => |literal| try self.program.ast.addPat(.{ .ty = ty, .data = .{ .str_lit = try self.lowerCheckedStringLiteral(literal) } }),
            .underscore => try self.program.ast.addPat(.{ .ty = ty, .data = .wildcard }),
            .runtime_error => invariantViolation("mono body lowering reached runtime_error checked pattern"),
            .pending => invariantViolation("mono body lowering reached an unresolved checked pattern"),
        };
        self.program.ast.pats.items[@intFromEnum(lowered)].source_ty = source_ty;
        return lowered;
    }

    fn representativeBinderForCandidate(
        self: *const BodyLowerer,
        binder: checked_artifact.PatternBinderId,
        binder_remaps: []const checked_artifact.CheckedAlternativeBinderRemap,
    ) checked_artifact.PatternBinderId {
        _ = self;
        for (binder_remaps) |remap| {
            if (remap.candidate_binder == binder) return remap.representative_binder;
        }
        return binder;
    }

    fn recordFieldType(
        self: *BodyLowerer,
        record_ty: Type.TypeId,
        field_name: canonical.RecordFieldLabelId,
    ) Type.TypeId {
        return switch (self.program.types.getType(record_ty)) {
            .record => |record| {
                for (record.fields) |field| {
                    if (field.name == field_name) return field.ty;
                }
                invariantViolation("mono body lowering could not find record pattern field in resolved record type");
            },
            else => invariantViolation("mono body lowering expected a resolved record type for record pattern"),
        };
    }

    fn nominalBackingType(self: *BodyLowerer, nominal_ty: Type.TypeId) Type.TypeId {
        return switch (self.program.types.getTypePreservingNominal(nominal_ty)) {
            .nominal => |nominal| nominal.backing,
            else => nominal_ty,
        };
    }

    fn tupleElementTypes(self: *BodyLowerer, tuple_ty: Type.TypeId, expected_len: usize) []const Type.TypeId {
        switch (self.program.types.getType(tuple_ty)) {
            .tuple => |items| {
                if (items.len != expected_len) invariantViolation("mono body lowering tuple pattern arity did not match its resolved type");
                return items;
            },
            else => invariantViolation("mono body lowering expected a resolved tuple type"),
        }
    }

    fn listElementType(self: *BodyLowerer, list_ty: Type.TypeId) Type.TypeId {
        return switch (self.program.types.getType(list_ty)) {
            .list => |elem| elem,
            else => invariantViolation("mono body lowering expected a resolved List(T) type"),
        };
    }

    const TagInfo = struct {
        discriminant: u16,
        payload_count: usize,
        payload_types: []const Type.TypeId,
    };

    fn boolLiteralForTagName(
        self: *BodyLowerer,
        ty: Type.TypeId,
        name: canonical.TagLabelId,
    ) ?bool {
        switch (self.program.types.getType(ty)) {
            .primitive => |prim| {
                if (prim != .bool) return null;
                const text = self.program.canonical_names.tagLabelText(name);
                if (std.mem.eql(u8, text, "True")) return true;
                if (std.mem.eql(u8, text, "False")) return false;
                invariantViolation("mono body lowering Bool constructor was neither True nor False");
            },
            else => return null,
        }
    }

    fn tagInfoForUnionType(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
        union_ref: ConcreteSourceType.ConcreteSourceTypeRef,
        name: canonical.TagLabelId,
    ) Allocator.Error![]const ConcreteTypeInfo {
        return switch (self.type_instantiator.concretePayload(union_ref)) {
            .alias => |alias| try self.concreteTagPayloadInfosForUnionType(try self.type_instantiator.concreteChildRef(union_ref, alias.backing), name),
            .nominal => |nominal| try self.concreteTagPayloadInfosForUnionType(try self.type_instantiator.concreteChildRef(union_ref, nominal.backing), name),
            .tag_union => |tag_union| blk: {
                if (try self.type_instantiator.findConcreteTag(union_ref, tag_union.tags, name)) |tag| {
                    break :blk try self.concreteTypeInfosForChildren(union_ref, tag.args);
                }
                break :blk try self.concreteTagPayloadInfosForUnionType(try self.type_instantiator.concreteChildRef(union_ref, tag_union.ext), name);
            },
            .empty_tag_union => invariantViolation("mono body lowering concrete tag constructor was missing from source union type"),
            .flex => |flex| {
                self.type_instantiator.verifyClosableRowTail(flex);
                invariantViolation("mono body lowering concrete tag constructor was missing from closed source union tail");
            },
            else => invariantViolation("mono body lowering expected concrete source tag-union type"),
        };
    }

    fn concreteTypeInfosForChildren(
        self: *BodyLowerer,
        parent: ConcreteSourceType.ConcreteSourceTypeRef,
        children: []const checked_artifact.CheckedTypeId,
    ) Allocator.Error![]const ConcreteTypeInfo {
        if (children.len == 0) return &.{};
        const out = try self.allocator.alloc(ConcreteTypeInfo, children.len);
        errdefer self.allocator.free(out);
        for (children, 0..) |child, i| {
            const source_ref = try self.type_instantiator.concreteChildRef(parent, child);
            out[i] = .{
                .ty = try self.type_instantiator.lowerConcreteRef(source_ref),
                .source_ty = self.program.concrete_source_types.key(source_ref),
                .source_ref = source_ref,
            };
        }
        return out;
    }

    fn recordFieldIndex(
        self: *BodyLowerer,
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
        self: *BodyLowerer,
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
        self: *BodyLowerer,
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

    fn lowerStmt(
        self: *BodyLowerer,
        statement_id: checked_artifact.CheckedStatementId,
    ) Allocator.Error!Ast.StmtId {
        const statement = self.checkedStatement(statement_id);
        return switch (statement.data) {
            .decl => |decl| blk: {
                const bind = try self.lowerParamPattern(decl.pattern);
                if (try self.lowerLocalFunctionDecl(bind, decl.expr)) |local_fn| {
                    break :blk try self.program.ast.addStmt(.{ .local_fn = local_fn });
                }
                const body = try self.lowerExprExpected(decl.expr, self.checkedPattern(decl.pattern).ty);
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
            .dbg => |expr| try self.program.ast.addStmt(.{ .debug = try self.lowerExpr(expr) }),
            .expr => |expr| try self.program.ast.addStmt(.{ .expr = try self.lowerExpr(expr) }),
            .expect => |expr| try self.program.ast.addStmt(.{ .expect = try self.lowerBoolConditionExpr(expr) }),
            .crash => |literal| try self.program.ast.addStmt(.{ .crash = try self.lowerCheckedStringLiteral(literal) }),
            .return_ => |ret| try self.program.ast.addStmt(.{ .return_ = try self.lowerExpr(ret.expr) }),
            .break_ => try self.program.ast.addStmt(.break_),
            .for_ => |for_| try self.lowerForStmt(for_.pattern, for_.expr, for_.body),
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
        self: *BodyLowerer,
        pattern: checked_artifact.CheckedPatternId,
        iterable: checked_artifact.CheckedExprId,
        body: checked_artifact.CheckedExprId,
    ) Allocator.Error!Ast.StmtId {
        const pattern_ty = try self.type_instantiator.lowerTemplateType(self.checkedPattern(pattern).ty);
        return try self.program.ast.addStmt(.{ .for_ = .{
            .patt = try self.lowerPattern(pattern_ty, pattern),
            .iterable = try self.lowerExpr(iterable),
            .body = try self.lowerExpr(body),
        } });
    }

    fn lowerLocalFunctionDecl(
        self: *BodyLowerer,
        bind: Ast.TypedSymbol,
        expr_id: checked_artifact.CheckedExprId,
    ) Allocator.Error!?Ast.LetFn {
        const expr = self.checkedExpr(expr_id);
        return switch (expr.data) {
            .lambda => |lambda| blk: {
                const source_fn_ref = try self.type_instantiator.concreteRefForTemplateType(expr.ty);
                break :blk .{
                    .site = self.nestedProcSite(expr_id, .local_function),
                    .source_fn_ty = self.program.concrete_source_types.key(source_fn_ref),
                    .recursive = false,
                    .bind = bind,
                    .args = try self.lowerParamSpanFromFunction(lambda.args, source_fn_ref),
                    .body = try self.lowerExprConcreteExpected(lambda.body, try self.returnTypeFromConcreteFunction(source_fn_ref)),
                };
            },
            .closure => |closure| blk: {
                const source_fn_ref = try self.type_instantiator.concreteRefForTemplateType(expr.ty);
                const lambda_expr = self.checkedExpr(closure.lambda);
                switch (lambda_expr.data) {
                    .lambda => |lambda| break :blk .{
                        .site = self.nestedProcSite(expr_id, .closure),
                        .source_fn_ty = self.program.concrete_source_types.key(source_fn_ref),
                        .recursive = false,
                        .bind = bind,
                        .args = try self.lowerParamSpanFromFunction(lambda.args, source_fn_ref),
                        .body = try self.lowerExprConcreteExpected(lambda.body, try self.returnTypeFromConcreteFunction(source_fn_ref)),
                    },
                    else => invariantViolation("mono body lowering expected local closure declaration to reference a checked lambda"),
                }
            },
            else => null,
        };
    }

    fn lowerCheckedStringLiteral(
        self: *BodyLowerer,
        literal: checked_artifact.CheckedStringLiteralId,
    ) Allocator.Error!mir_ids.ProgramLiteralId {
        return try self.program.literal_pool.intern(self.checkedStringLiteral(literal));
    }

    fn recordFieldLabel(
        self: *BodyLowerer,
        label: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.name_resolver.recordFieldLabel(self.template_lookup.artifact, label);
    }

    fn tagLabel(
        self: *BodyLowerer,
        label: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.name_resolver.tagLabel(self.template_lookup.artifact, label);
    }

    fn methodName(
        self: *BodyLowerer,
        method: canonical.MethodNameId,
    ) Allocator.Error!canonical.MethodNameId {
        return try self.name_resolver.methodName(self.template_lookup.artifact, method);
    }

    fn recordUpdateHasRemappedField(
        self: *BodyLowerer,
        fields: []const checked_artifact.CheckedRecordExprField,
        label: canonical.RecordFieldLabelId,
    ) Allocator.Error!bool {
        for (fields) |field| {
            if ((try self.recordFieldLabel(field.label)) == label) return true;
        }
        return false;
    }

    fn checkedStringLiteral(
        self: *BodyLowerer,
        literal: checked_artifact.CheckedStringLiteralId,
    ) []const u8 {
        const raw = @intFromEnum(literal);
        if (raw >= self.template_lookup.checked_bodies.string_literals.len) {
            invariantViolation("mono body lowering received a checked string literal outside the owning checked body store");
        }
        return self.template_lookup.checked_bodies.string_literals[raw];
    }

    fn checkedBody(self: *const BodyLowerer, id: checked_artifact.CheckedBodyId) checked_artifact.CheckedBody {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.bodies.len) invariantViolation("mono body lowering received body id outside checked body store");
        return self.template_lookup.checked_bodies.bodies[raw];
    }

    fn checkedExpr(self: *const BodyLowerer, id: checked_artifact.CheckedExprId) checked_artifact.CheckedExpr {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.exprs.len) invariantViolation("mono body lowering received expr id outside checked body store");
        return self.template_lookup.checked_bodies.exprs[raw];
    }

    fn checkedPattern(self: *const BodyLowerer, id: checked_artifact.CheckedPatternId) checked_artifact.CheckedPattern {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.patterns.len) invariantViolation("mono body lowering received pattern id outside checked body store");
        return self.template_lookup.checked_bodies.patterns[raw];
    }

    fn checkedStatement(self: *const BodyLowerer, id: checked_artifact.CheckedStatementId) checked_artifact.CheckedStatement {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.checked_bodies.statements.len) invariantViolation("mono body lowering received statement id outside checked body store");
        return self.template_lookup.checked_bodies.statements[raw];
    }

    fn staticDispatchPlan(self: *const BodyLowerer, id: checked_artifact.StaticDispatchPlanId) static_dispatch.StaticDispatchCallPlan {
        const table = staticDispatchPlansForKey(self.input, self.template_lookup.artifact) orelse {
            debug.invariant(false, "mono body lowering invariant violated: static dispatch plan artifact was not available");
            unreachable;
        };
        const raw = @intFromEnum(id);
        if (raw >= table.plans.len) invariantViolation("mono body lowering received static dispatch plan id outside table");
        return table.plans[raw];
    }

    fn resolvedValueRef(self: *const BodyLowerer, id: checked_artifact.ResolvedValueRefId) checked_artifact.ResolvedValueRefRecord {
        const raw = @intFromEnum(id);
        if (raw >= self.template_lookup.resolved_value_refs.records.len) invariantViolation("mono body lowering received resolved value ref id outside table");
        return self.template_lookup.resolved_value_refs.records[raw];
    }

    fn nestedProcSite(
        self: *const BodyLowerer,
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

    fn sourceTypeKey(
        self: *BodyLowerer,
        checked_ty: checked_artifact.CheckedTypeId,
    ) Allocator.Error!canonical.CanonicalTypeKey {
        const concrete = try self.type_instantiator.concreteRefForTemplateType(checked_ty);
        return self.program.concrete_source_types.key(concrete);
    }

    fn procedureUseForExpr(
        self: *const BodyLowerer,
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
            .platform_required_proc,
            .promoted_top_level_proc,
            => |proc_use| proc_use,
            else => null,
        };
    }

    fn localProcUseForExpr(
        self: *const BodyLowerer,
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

    fn reserveProcedureUse(
        self: *BodyLowerer,
        use: checked_artifact.ProcedureUseTemplate,
        checked_fn_ty: checked_artifact.CheckedTypeId,
        reason: MonoSpecializationReason,
    ) Allocator.Error!canonical.MirProcedureRef {
        const requested_fn_ty = try self.type_instantiator.concreteRefForTemplateType(checked_fn_ty);
        const template = try self.name_resolver.procedureTemplateRef(try self.procedureTemplateForUse(use, requested_fn_ty));
        const reserved = try self.queue.reserve(&self.program.concrete_source_types, .{
            .template = template,
            .requested_fn_ty = requested_fn_ty,
            .reason = reason,
        });
        return canonical.mirProcedureRefFromMono(reserved.proc);
    }

    fn procedureTemplateForUse(
        self: *BodyLowerer,
        use: checked_artifact.ProcedureUseTemplate,
        requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!canonical.ProcedureTemplateRef {
        const requested_key = self.program.concrete_source_types.key(requested_fn_ty);
        return switch (use.binding) {
            .top_level => |binding_ref| try self.templateFromTopLevelBinding(
                self.template_lookup.artifact,
                topLevelProcedureBindingsForKey(self.input, self.template_lookup.artifact) orelse {
                    debug.invariant(false, "mono body lowering invariant violated: template artifact has no top-level procedure binding table");
                    unreachable;
                },
                binding_ref,
                .{ .top_level = binding_ref },
                requested_key,
            ),
            .imported => |imported| try self.templateFromImportedProcedureBinding(imported, requested_key),
            .hosted => |hosted| hosted.template,
            .platform_required => |required| try self.templateFromTopLevelBinding(
                required.artifact,
                topLevelProcedureBindingsForKey(self.input, required.artifact) orelse {
                    debug.invariant(false, "mono body lowering invariant violated: platform-required artifact has no procedure binding table");
                    unreachable;
                },
                required.procedure_binding,
                .{ .platform_required = required },
                requested_key,
            ),
            .promoted => |promoted| self.templateFromPromotedProcedure(promoted),
        };
    }

    fn templateFromPromotedProcedure(
        self: *const BodyLowerer,
        promoted: checked_artifact.PromotedProcedureRef,
    ) canonical.ProcedureTemplateRef {
        if (self.input.root.artifact.module_identity.module_idx == promoted.module_idx) {
            if (promotedProcedureTemplate(&self.input.root.artifact.promoted_procedures, promoted)) |template| return template;
        }
        for (self.input.imports) |view| {
            if (view.module_identity.module_idx != promoted.module_idx) continue;
            if (promotedProcedureTemplate(view.promoted_procedures, promoted)) |template| return template;
        }
        for (self.input.root.relation_artifacts) |view| {
            if (view.module_identity.module_idx != promoted.module_idx) continue;
            if (promotedProcedureTemplate(view.promoted_procedures, promoted)) |template| return template;
        }
        invariantViolation("mono body lowering could not find promoted procedure in published artifact views");
    }

    fn templateFromTopLevelBinding(
        self: *BodyLowerer,
        owner: checked_artifact.CheckedModuleArtifactKey,
        bindings: *const checked_artifact.TopLevelProcedureBindingTable,
        binding_ref: checked_artifact.TopLevelProcedureBindingRef,
        binding_key: checked_artifact.ProcedureBindingRef,
        requested_key: canonical.CanonicalTypeKey,
    ) Allocator.Error!canonical.ProcedureTemplateRef {
        const binding = bindings.get(binding_ref);
        return switch (binding.body) {
            .direct_template => |direct| checkedTemplateFromCallableTemplate(direct.template),
            .callable_eval_template => try self.templateFromCallableBindingInstance(owner, binding_key, requested_key),
        };
    }

    fn templateFromImportedProcedureBinding(
        self: *BodyLowerer,
        imported: checked_artifact.ImportedProcedureBindingRef,
        requested_key: canonical.CanonicalTypeKey,
    ) Allocator.Error!canonical.ProcedureTemplateRef {
        for (self.input.imports) |view| {
            if (!std.mem.eql(u8, &view.key.bytes, &imported.artifact.bytes)) continue;
            for (view.exported_procedure_bindings.bindings) |binding| {
                if (binding.binding.module_idx == imported.module_idx and
                    binding.binding.def == imported.def and
                    binding.binding.pattern == imported.pattern)
                {
                    return switch (binding.body) {
                        .direct_template => |direct| checkedTemplateFromCallableTemplate(direct.template),
                        .callable_eval_template => try self.templateFromCallableBindingInstance(
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
                if (binding.binding.module_idx == imported.module_idx and
                    binding.binding.def == imported.def and
                    binding.binding.pattern == imported.pattern)
                {
                    return switch (binding.body) {
                        .direct_template => |direct| checkedTemplateFromCallableTemplate(direct.template),
                        .callable_eval_template => try self.templateFromCallableBindingInstance(
                            self.input.root.artifact.key,
                            .{ .imported = imported },
                            requested_key,
                        ),
                    };
                }
            }
        }
        invariantViolation("mono body lowering could not find imported procedure binding in published artifact views");
    }

    fn templateFromCallableBindingInstance(
        self: *BodyLowerer,
        owner: checked_artifact.CheckedModuleArtifactKey,
        binding: checked_artifact.ProcedureBindingRef,
        requested_key: canonical.CanonicalTypeKey,
    ) Allocator.Error!canonical.ProcedureTemplateRef {
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
        return checkedTemplateFromCallableTemplate(instance.proc_value.template);
    }
};

fn promotedProcedureTemplate(
    table: *const checked_artifact.PromotedProcedureTable,
    ref: checked_artifact.PromotedProcedureRef,
) ?canonical.ProcedureTemplateRef {
    for (table.procedures) |procedure| {
        if (canonical.procedureValueRefEql(procedure.proc, ref.proc)) return procedure.template;
    }
    return null;
}

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
) ?canonical.ProcedureTemplateRef {
    if (root.procedure_template) |template| return template;

    const artifact = input.root.artifact;
    const requested_key = concrete_source_types.key(requested_fn_ty);
    switch (root.source) {
        .def => |def_idx| {
            if (artifact.checked_procedure_templates.lookupByDef(def_idx)) |template| return template;
            const top_level = artifact.top_level_values.lookupByDef(def_idx) orelse return null;
            return switch (top_level.value) {
                .const_ref => null,
                .procedure_binding => |binding_ref| templateFromRootTopLevelBinding(
                    input,
                    input.root.artifact.key,
                    &artifact.top_level_procedure_bindings,
                    binding_ref,
                    .{ .top_level = binding_ref },
                    requested_key,
                ),
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
                .procedure_value => |proc_use| templateForProcedureUse(input, proc_use, requested_key),
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
            &input.root.artifact.top_level_procedure_bindings,
            binding_ref,
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
                required.artifact,
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
        a.module_idx == b.module_idx and
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
    return switch (record.state) {
        .evaluated => |instance| instance,
        .reserved, .evaluating => invariantViolation("callable binding instance was consumed before it was sealed"),
    };
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

fn methodRegistryForKey(
    input: Input,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const static_dispatch.MethodRegistry {
    if (std.mem.eql(u8, &input.root.artifact.key.bytes, &key.bytes)) {
        return &input.root.artifact.method_registry;
    }
    for (input.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) {
            return imported.method_registry;
        }
    }
    for (input.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) {
            return related.method_registry;
        }
    }
    return null;
}

fn lookupStaticDispatchMethodTarget(
    input: Input,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    registry_artifact: checked_artifact.CheckedModuleArtifactKey,
    owner: static_dispatch.MethodOwner,
    method: canonical.MethodNameId,
) Allocator.Error!?static_dispatch.MethodTarget {
    const registry = methodRegistryForKey(input, registry_artifact) orelse {
        debug.invariant(false, "mono static-dispatch invariant violated: method registry artifact was not available");
        unreachable;
    };
    for (registry.entries) |entry| {
        const entry_owner = try name_resolver.methodOwner(registry_artifact, entry.key.owner);
        const entry_method = try name_resolver.methodName(registry_artifact, entry.key.method);
        if (methodOwnerEql(entry_owner, owner) and entry_method == method) return entry.target;
    }
    return null;
}

fn methodOwnerForDispatcherType(
    types: *const Type.Store,
    ty: Type.TypeId,
) static_dispatch.MethodOwner {
    return switch (types.getTypePreservingNominal(ty)) {
        .nominal => |nominal| .{ .nominal = nominal.nominal },
        .primitive => |prim| .{ .builtin = switch (prim) {
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
            .erased => invariantViolation("mono static dispatch reached erased function representation before lambda solving"),
        } },
        .list => .{ .builtin = .list },
        .box => .{ .builtin = .box },
        .link => unreachable,
        .placeholder,
        .unbd,
        .func,
        .tuple,
        .tag_union,
        .record,
        => invariantViolation("mono static dispatch dispatcher type did not resolve to an allowed method owner"),
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
        if (self.requested.get(key)) |existing| {
            if (existing.requested_fn_ty != request.requested_fn_ty) {
                debug.invariant(false, "mono specialization invariant violated: same specialization key registered with a different concrete payload ref");
                unreachable;
            }
            return existing;
        }

        const reserved = ReservedMonoProc{
            .proc = .{
                .proc = .{ .artifact = request.template.artifact, .proc_base = request.template.proc_base },
                .specialization = key,
            },
            .local_handle = @enumFromInt(@as(u32, @intCast(self.requested.count()))),
            .requested_fn_ty = request.requested_fn_ty,
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
    const owned_key = try std.testing.allocator.dupe(u8, requested_key.bytes[0..]);
    try concrete.roots.append(std.testing.allocator, .{
        .key = requested_key,
        .source = .{
            .artifact = .{},
            .ty = @enumFromInt(0),
        },
    });
    try concrete.by_key.put(owned_key, @enumFromInt(0));

    const template = canonical.ProcedureTemplateRef{
        .proc_base = @enumFromInt(0),
        .template = @enumFromInt(0),
    };
    const request = MonoSpecializationRequest{
        .template = template,
        .requested_fn_ty = @enumFromInt(0),
        .reason = .{ .comptime_dependency_summary = @enumFromInt(0) },
    };

    const first = try queue.reserve(&concrete, request);
    const second = try queue.reserve(&concrete, request);
    try std.testing.expectEqual(first.local_handle, second.local_handle);
    try std.testing.expectEqual(@as(usize, 1), queue.pending.items.len);
}
