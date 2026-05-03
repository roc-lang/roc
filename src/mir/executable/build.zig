//! Executable MIR construction state.

const std = @import("std");
const builtin = @import("builtin");
const check = @import("check");
const types = @import("types");
const symbol_mod = @import("symbol");
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

pub const ArtifactViews = struct {
    root: ?checked_artifact.LoweringModuleView = null,
    imports: []const checked_artifact.ImportedModuleView = &.{},
};

const MaterializationStores = struct {
    plans: *const checked_artifact.CompileTimePlanStore,
    values: *const checked_artifact.CompileTimeValueStore,
};

const PublishedTransformContext = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    materialization: MaterializationStores,
    executable_type_payloads: *const checked_artifact.ExecutableTypePayloadStore,
    executable_value_transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
};

pub const Proc = struct {
    executable_proc: Ast.ExecutableProcId,
    origin: Ast.ProcOrigin,
    body: Ast.DefId,
};

const ErasedAdapterProcReservation = struct {
    key: repr.ErasedAdapterKey,
    executable_proc: Ast.ExecutableProcId,
};

const ConstInstanceAdapterVisitKey = struct {
    owner: [32]u8,
    instance: checked_artifact.ConstInstanceId,
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
    erased_adapter_procs: std.ArrayList(ErasedAdapterProcReservation),
    root_procs: std.ArrayList(Ast.ExecutableProcId),
    root_metadata: std.ArrayList(ids.RootMetadata),
    callable_set_descriptors: []const repr.CanonicalCallableSetDescriptor = &.{},
    artifact_views: ArtifactViews = .{},
    layouts: ?Layouts.Layouts = null,

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
            .erased_adapter_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        if (self.layouts) |*layouts| layouts.deinit();
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.erased_adapter_procs.deinit(self.allocator);
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
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    var proc_map = std.AutoHashMap(canonical.MirProcedureRef, Ast.ExecutableProcId).init(allocator);
    defer proc_map.deinit();
    var proc_instance_map = std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId).init(allocator);
    defer proc_instance_map.deinit();
    var proc_exec_map = std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId).init(allocator);
    defer proc_exec_map.deinit();
    const normal_proc_count = input.procs.items.len;
    const executable_synthetic_proc_count = input.executable_synthetic_procs.items.len;
    var erased_adapter_keys = try collectErasedAdapterKeys(allocator, &input, program.artifact_views);
    defer erased_adapter_keys.deinit(allocator);
    const erased_adapter_proc_count = erased_adapter_keys.items.len;
    const total_proc_count = normal_proc_count + executable_synthetic_proc_count + erased_adapter_proc_count;

    try proc_map.ensureTotalCapacity(@intCast(total_proc_count));
    try proc_instance_map.ensureTotalCapacity(@intCast(input.procs.items.len));
    try proc_exec_map.ensureTotalCapacity(@intCast(input.procs.items.len));
    for (input.procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(i)));
        proc_map.putAssumeCapacity(proc.proc, executable_proc);
        proc_instance_map.putAssumeCapacity(proc.proc, proc.representation_instance);
        proc_exec_map.putAssumeCapacity(proc.representation_instance, executable_proc);
    }
    for (input.executable_synthetic_procs.items, 0..) |proc, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + i)));
        proc_map.putAssumeCapacity(proc.source_proc, executable_proc);
    }
    try program.erased_adapter_procs.ensureTotalCapacity(allocator, erased_adapter_proc_count);
    for (erased_adapter_keys.items, 0..) |adapter, i| {
        const executable_proc: Ast.ExecutableProcId = @enumFromInt(@as(u32, @intCast(normal_proc_count + executable_synthetic_proc_count + i)));
        program.erased_adapter_procs.appendAssumeCapacity(.{
            .key = adapter,
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
            .session_type_lowerer = SessionTypeLowerer.init(allocator, &representation_store.session_executable_type_payloads, &program.types),
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
            .proc_map = &proc_map,
            .proc_instance_map = &proc_instance_map,
            .proc_exec_map = &proc_exec_map,
            .erased_adapter_procs = program.erased_adapter_procs.items,
            .active_callable_sets = std.ArrayList(ActiveCallableSetType).empty,
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
    for (program.erased_adapter_procs.items) |adapter| {
        program.procs.appendAssumeCapacity(.{
            .executable_proc = adapter.executable_proc,
            .origin = .{ .erased_adapter = adapter.key },
            .body = try lowerErasedFiniteSetAdapterProc(allocator, &program, &input, &type_lowerer, &proc_map, &proc_instance_map, &proc_exec_map, adapter.key, adapter.executable_proc),
        });
    }

    for (input.root_procs.items, input.root_metadata.items) |root, metadata| {
        const executable_root = executableProcForSource(&program, root) orelse {
            debug.invariant(false, "executable build invariant violated: root source proc has no executable proc");
            unreachable;
        };
        try program.root_procs.append(allocator, executable_root);
        try program.root_metadata.append(allocator, metadata);
    }

    input.deinit();
    return program;
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

fn collectErasedAdapterKeys(
    allocator: Allocator,
    input: *const LambdaSolved.Solve.Program,
    artifact_views: ArtifactViews,
) Allocator.Error!std.ArrayList(repr.ErasedAdapterKey) {
    var adapters = std.ArrayList(repr.ErasedAdapterKey).empty;
    errdefer adapters.deinit(allocator);
    var visited_const_instances = std.AutoHashMap(ConstInstanceAdapterVisitKey, void).init(allocator);
    defer visited_const_instances.deinit();

    for (input.solve_sessions.items) |*session| {
        for (session.representation_store.callable_emission_plans) |plan| {
            switch (plan) {
                .already_erased => |erased| try collectErasedCodeRefAdapter(allocator, &adapters, erased.code),
                .erase_finite_set => |erase| try appendErasedAdapterKey(allocator, &adapters, erase.adapter),
                .finite,
                .erase_proc_value,
                => {},
            }
        }
        for (session.representation_store.session_value_transforms.plans.items) |plan| {
            try collectSessionValueTransformAdapters(
                allocator,
                &adapters,
                artifact_views,
                &session.representation_store,
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
            else => {},
        }
    }

    for (input.executable_synthetic_procs.items) |synthetic| {
        switch (synthetic.body) {
            .erased_promoted_wrapper => |erased| {
                try collectErasedCodeRefAdapter(allocator, &adapters, erased.code);
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
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    code: canonical.ErasedCallableCodeRef,
) Allocator.Error!void {
    switch (code) {
        .direct_proc_value => {},
        .finite_set_adapter => |adapter| try appendErasedAdapterKey(allocator, adapters, adapter),
    }
}

fn collectSessionValueTransformAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    artifact_views: ArtifactViews,
    store: *const repr.RepresentationStore,
    plan: repr.SessionExecutableValueTransformPlan,
) Allocator.Error!void {
    switch (plan.op) {
        .identity,
        .already_erased_callable,
        => {},
        .callable_to_erased => |callable| switch (callable) {
            .finite_value => |finite| try appendErasedAdapterKey(allocator, adapters, finite.adapter),
            .proc_value => {},
        },
        .record => |fields| for (fields) |field| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, field.transform);
        },
        .tuple => |items| for (items) |item| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, item.transform);
        },
        .tag_union => |cases| for (cases) |case| {
            for (case.payloads) |payload| {
                try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, payload.transform);
            }
        },
        .nominal => |nominal| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, nominal.backing),
        .list => |list| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, list.elem),
        .box_payload => |box| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, box.payload),
        .structural_bridge => |bridge| try collectSessionStructuralBridgeAdapters(allocator, adapters, artifact_views, store, bridge),
    }
}

fn collectExecutableValueTransformRefAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    artifact_views: ArtifactViews,
    store: *const repr.RepresentationStore,
    transform: checked_artifact.ExecutableValueTransformRef,
) Allocator.Error!void {
    switch (transform) {
        .session => |id| try collectSessionValueTransformAdapters(
            allocator,
            adapters,
            artifact_views,
            store,
            store.sessionExecutableValueTransform(id),
        ),
        .published => |published| {
            const context = resolvePublishedTransformContextInArtifactViews(artifact_views, published.artifact);
            try collectPublishedValueTransformAdapters(
                allocator,
                adapters,
                artifact_views,
                context.executable_value_transforms,
                published.transform,
            );
        },
    }
}

fn collectSessionStructuralBridgeAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    artifact_views: ArtifactViews,
    store: *const repr.RepresentationStore,
    bridge: repr.SessionExecutableStructuralBridgePlan,
) Allocator.Error!void {
    switch (bridge) {
        .direct,
        .zst,
        .list_reinterpret,
        .nominal_reinterpret,
        => {},
        .box_unbox => |child| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, child),
        .box_box => |child| try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, child),
        .singleton_to_tag_union => |singleton| if (singleton.value_transform) |child| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, child);
        },
        .tag_union_to_singleton => |singleton| if (singleton.value_transform) |child| {
            try collectExecutableValueTransformRefAdapters(allocator, adapters, artifact_views, store, child);
        },
    }
}

fn collectPublishedValueTransformAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    artifact_views: ArtifactViews,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    transform_id: checked_artifact.ExecutableValueTransformPlanId,
) Allocator.Error!void {
    const plan = transforms.get(transform_id);
    switch (plan.op) {
        .identity,
        .already_erased_callable,
        => {},
        .callable_to_erased => |callable| switch (callable) {
            .finite_value => |finite| try appendErasedAdapterKey(allocator, adapters, finite.adapter_key),
            .proc_value => {},
        },
        .record => |fields| for (fields) |field| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, field.transform);
        },
        .tuple => |items| for (items) |item| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, item.transform);
        },
        .tag_union => |cases| for (cases) |case| {
            for (case.payloads) |payload| {
                try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, payload.transform);
            }
        },
        .nominal => |nominal| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, nominal.backing),
        .list => |list| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, list.elem),
        .box_payload => |box| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, box.payload),
        .structural_bridge => |bridge| try collectPublishedStructuralBridgeAdapters(allocator, adapters, artifact_views, transforms, bridge),
    }
}

fn collectPublishedStructuralBridgeAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    artifact_views: ArtifactViews,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    bridge: checked_artifact.ExecutableStructuralBridgePlan,
) Allocator.Error!void {
    switch (bridge) {
        .direct,
        .zst,
        .list_reinterpret,
        .nominal_reinterpret,
        => {},
        .box_unbox => |child| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, child),
        .box_box => |child| try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, child),
        .singleton_to_tag_union => |singleton| if (singleton.value_transform) |child| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, child);
        },
        .tag_union_to_singleton => |singleton| if (singleton.value_transform) |child| {
            try collectPublishedValueTransformAdapters(allocator, adapters, artifact_views, transforms, child);
        },
    }
}

fn collectErasedCaptureMaterializationAdapters(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
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
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
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
            try collectErasedCodeRefAdapter(allocator, adapters, erased.code);
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
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
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
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
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
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    artifact_views: ArtifactViews,
    visited_const_instances: *std.AutoHashMap(ConstInstanceAdapterVisitKey, void),
    plans: *const checked_artifact.CompileTimePlanStore,
    leaf: checked_artifact.CallableLeafInstance,
) Allocator.Error!void {
    switch (leaf) {
        .finite => {},
        .erased_boxed => |erased| {
            try collectErasedCodeRefAdapter(allocator, adapters, erased.code);
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

fn appendErasedAdapterKey(
    allocator: Allocator,
    adapters: *std.ArrayList(repr.ErasedAdapterKey),
    adapter: repr.ErasedAdapterKey,
) Allocator.Error!void {
    for (adapters.items) |existing| {
        if (erasedAdapterKeyEql(existing, adapter)) return;
    }
    try adapters.append(allocator, adapter);
}

fn erasedAdapterKeyEql(a: repr.ErasedAdapterKey, b: repr.ErasedAdapterKey) bool {
    return repr.canonicalTypeKeyEql(a.source_fn_ty, b.source_fn_ty) and
        repr.callableSetKeyEql(a.callable_set_key, b.callable_set_key) and
        repr.erasedFnSigKeyEql(a.erased_fn_sig_key, b.erased_fn_sig_key) and
        repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
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

fn lowerErasedFiniteSetAdapterProc(
    allocator: Allocator,
    program: *Program,
    input: *const LambdaSolved.Solve.Program,
    type_lowerer: *TypeLowerer,
    proc_map: *const std.AutoHashMap(canonical.MirProcedureRef, Ast.ExecutableProcId),
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    proc_exec_map: *const std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId),
    adapter: repr.ErasedAdapterKey,
    executable_proc: Ast.ExecutableProcId,
) Allocator.Error!Ast.DefId {
    const descriptor = programCallableSetDescriptor(program, adapter.callable_set_key) orelse {
        executableInvariant("executable erased finite-set adapter has no callable-set descriptor");
    };
    if (descriptor.members.len == 0) {
        executableInvariant("executable erased finite-set adapter descriptor has no members");
    }

    const first_member = descriptor.members[0];
    const first_instance_id = proc_instance_map.get(first_member.source_proc) orelse {
        executableInvariant("executable erased finite-set adapter first member has no representation instance");
    };
    const first_instance = &input.proc_instances.items[@intFromEnum(first_instance_id)];
    if (!repr.canonicalTypeKeyEql(first_instance.executable_specialization_key.requested_fn_ty, adapter.source_fn_ty)) {
        executableInvariant("executable erased finite-set adapter source function type differs from first member specialization");
    }

    const value_store = &input.value_stores.items[@intFromEnum(first_instance.value_store)];
    const representation_store = &input.solve_sessions.items[@intFromEnum(first_instance.solve_session)].representation_store;
    var builder = BodyBuilder{
        .allocator = allocator,
        .program = program,
        .input = &input.ast,
        .output = &program.ast,
        .canonical_names = &program.canonical_names,
        .type_lowerer = type_lowerer,
        .session_type_lowerer = SessionTypeLowerer.init(allocator, &representation_store.session_executable_type_payloads, &program.types),
        .value_store = value_store,
        .representation_store = representation_store,
        .callable_set_descriptors = program.callable_set_descriptors,
        .env = std.AutoHashMap(repr.BindingInfoId, Ast.ExecutableValueRef).init(allocator),
        .expr_map = std.AutoHashMap(LambdaSolved.Ast.ExprId, Ast.ExprId).init(allocator),
        .executable_proc = executable_proc,
        .source_proc = first_member.source_proc,
        .representation_instance = first_instance_id,
        .proc_instance = first_instance,
        .proc_instances = input.proc_instances.items,
        .solve_sessions = input.solve_sessions.items,
        .value_stores = input.value_stores.items,
        .proc_map = proc_map,
        .proc_instance_map = proc_instance_map,
        .proc_exec_map = proc_exec_map,
        .erased_adapter_procs = program.erased_adapter_procs.items,
        .active_callable_sets = std.ArrayList(ActiveCallableSetType).empty,
    };
    defer builder.deinit();

    const public_params = value_store.sliceValueSpan(first_instance.public_roots.params);
    const hidden_capture_ty = try builder.lowerFiniteSetAdapterCaptureType(adapter);
    if (hidden_capture_ty == null and descriptor.members.len != 1) {
        executableInvariant("executable erased finite-set adapter without hidden capture cannot dispatch a multi-member callable set");
    }

    const hidden_capture_arg_count: usize = if (hidden_capture_ty == null) 0 else 1;
    const typed_arg_count = public_params.len + hidden_capture_arg_count;
    const typed_args = try allocator.alloc(Ast.TypedValue, typed_arg_count);
    defer allocator.free(typed_args);
    for (public_params, 0..) |param, i| {
        const param_info = value_store.values.items[@intFromEnum(param)];
        typed_args[i] = .{
            .ty = try builder.lowerExecutableValueTypeInStore(param_info.logical_ty, param, value_store, representation_store),
            .value = program.ast.freshValueRef(),
        };
    }

    const hidden_capture_value: ?Ast.ExecutableValueRef = if (hidden_capture_ty) |capture_ty| blk: {
        const value = program.ast.freshValueRef();
        typed_args[public_params.len] = .{
            .ty = capture_ty,
            .value = value,
        };
        break :blk value;
    } else null;

    const args = try program.ast.addTypedValueSpan(typed_args);
    const ret_info = value_store.values.items[@intFromEnum(first_instance.public_roots.ret)];
    const result_ty = try builder.lowerExecutableValueTypeInStore(
        ret_info.logical_ty,
        first_instance.public_roots.ret,
        value_store,
        representation_store,
    );
    const body = try lowerErasedFiniteSetAdapterBody(
        allocator,
        program,
        &builder,
        adapter,
        descriptor,
        typed_args[0..public_params.len],
        hidden_capture_value,
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
    explicit_args: []const Ast.TypedValue,
    hidden_capture: ?Ast.ExecutableValueRef,
    result_ty: Type.TypeId,
) Allocator.Error!Ast.ExprId {
    if (!repr.callableSetKeyEql(descriptor.key, adapter.callable_set_key)) {
        executableInvariant("executable erased finite-set adapter descriptor key differs from adapter key");
    }

    const callee_value = hidden_capture orelse blk: {
        const member = descriptor.members[0];
        if (member.capture_slots.len != 0) {
            executableInvariant("executable erased finite-set adapter without hidden capture cannot synthesize captured callable set");
        }
        const callable_set_ty = try builder.lowerFiniteCallableSetType(adapter.callable_set_key, builder.representation_store);
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
        const final_call = try lowerErasedFiniteSetAdapterCallableMatch(
            allocator,
            program,
            builder,
            adapter,
            descriptor,
            explicit_args,
            value,
            result_ty,
        );
        return try program.ast.addExpr(result_ty, program.ast.getExpr(final_call).value, .{ .block = .{
            .stmts = try program.ast.addStmtSpan(&.{stmt}),
            .final_expr = final_call,
        } });
    };

    return try lowerErasedFiniteSetAdapterCallableMatch(
        allocator,
        program,
        builder,
        adapter,
        descriptor,
        explicit_args,
        callee_value,
        result_ty,
    );
}

fn lowerErasedFiniteSetAdapterCallableMatch(
    allocator: Allocator,
    program: *Program,
    builder: *BodyBuilder,
    adapter: repr.ErasedAdapterKey,
    descriptor: *const repr.CanonicalCallableSetDescriptor,
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
    for (descriptor.members, 0..) |member, i| {
        if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, adapter.source_fn_ty)) {
            executableInvariant("executable erased finite-set adapter member source type differs from adapter key");
        }
        const executable_proc = builder.proc_map.get(member.source_proc) orelse {
            executableInvariant("executable erased finite-set adapter member target was not reserved");
        };
        const target_instance_id = builder.proc_instance_map.get(member.source_proc) orelse {
            executableInvariant("executable erased finite-set adapter member target has no representation instance");
        };
        const target_instance = builder.proc_instances[@intFromEnum(target_instance_id)];
        if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, adapter.source_fn_ty)) {
            executableInvariant("executable erased finite-set adapter member target specialization source type differs from adapter key");
        }

        const capture_payload_ty = try builder.lowerCallableSetMemberPayloadType(member);
        const capture_payload = if (capture_payload_ty != null) program.ast.freshValueRef() else null;
        const capture_arg_len: usize = if (capture_payload == null) 0 else 1;
        const direct_args = try allocator.alloc(Ast.DirectCallArg, explicit_args.len + capture_arg_len);
        defer allocator.free(direct_args);
        for (explicit_args, 0..) |arg, arg_i| {
            direct_args[arg_i] = .{ .value = arg.value };
        }
        if (capture_payload) |payload| {
            direct_args[explicit_args.len] = .{ .value = payload };
        }
        const direct_args_span = try program.ast.addDirectCallArgSpan(direct_args);
        const branch_body = try builder.lowerCallableMatchBranchBody(
            member.source_proc,
            target_instance,
            executable_proc,
            direct_args_span,
            result_ty,
            null,
        );

        branches[i] = .{
            .member = .{
                .callable_set_key = adapter.callable_set_key,
                .member_index = member.member,
            },
            .source_fn_ty = member.proc_value.source_fn_ty,
            .capture_payload = capture_payload,
            .capture_payload_ty = capture_payload_ty,
            .executable_specialization_key = try repr.cloneExecutableSpecializationKey(allocator, target_instance.executable_specialization_key),
            .executable_proc = executable_proc,
            .direct_args = direct_args_span,
            .body = branch_body,
        };
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
        &program.types,
        &program.row_shapes,
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
        .{
            .plans = synthetic.comptime_plans,
            .values = synthetic.comptime_values,
        },
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
            break :blk try program.ast.addExpr(wrapper_ret_ty, result_value, .{ .value_ref = result_value });
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
        .record => |fields| try applyRecordValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            fields,
            value,
        ),
        .tuple => |items| try applyTupleValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            items,
            value,
        ),
        .nominal => |nominal| try applyNominalValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            nominal,
            value,
        ),
        .tag_union => |cases| try applyTagUnionValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            cases,
            value,
        ),
        .list => |list| try applyListValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            list.elem,
            value,
        ),
        .box_payload => |box| try applyBoxValueTransform(
            program,
            materialization,
            published_types,
            transforms,
            stmts,
            plan,
            box,
            value,
        ),
        .callable_to_erased => |callable| try applyCallableToErasedValueTransform(
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

    const source_expr = try program.ast.addExpr(from_ty, value, .{ .value_ref = value });
    const seen = try program.allocator.alloc(bool, fields.len);
    defer program.allocator.free(seen);
    @memset(seen, false);

    const output_fields = try program.allocator.alloc(Ast.RecordFieldExpr, target.fields.len);
    defer program.allocator.free(output_fields);
    for (target.fields, 0..) |target_field, target_i| {
        const label = program.row_shapes.recordField(target_field.field).label;
        const field_plan = findValueTransformRecordField(fields, label, seen) orelse {
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
            .expr = try program.ast.addExpr(target_field.ty, transformed, .{ .value_ref = transformed }),
            .ty = target_field.ty,
            .value = transformed,
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
    fields: []const checked_artifact.ValueTransformRecordField,
    label: canonical.RecordFieldLabelId,
    seen: []bool,
) ?checked_artifact.ValueTransformRecordField {
    for (fields, 0..) |field, i| {
        if (field.field != label) continue;
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
    if (!nominalTypeKeyEql(target.nominal, nominal.nominal)) {
        executableInvariant("nominal value transform target nominal differs from plan");
    }

    const source_expr = try program.ast.addExpr(from_ty, value, .{ .value_ref = value });
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
    const transformed_expr = try program.ast.addExpr(target.backing, transformed_backing, .{ .value_ref = transformed_backing });
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

    const tuple_expr = try program.ast.addExpr(from_ty, value, .{ .value_ref = value });
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
        output_items[i] = try program.ast.addExpr(target_item_ty, transformed, .{ .value_ref = transformed });
    }
    verifyAllSeen(seen, "tuple value transform had an extra element transform");

    const tuple_value = program.ast.freshValueRef();
    const result_expr = try program.ast.addExpr(to_ty, tuple_value, .{ .tuple = try program.ast.addExprSpan(output_items) });
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
    const transformed_expr = try program.ast.addExpr(target_elem_ty, transformed_elem, .{ .value_ref = transformed_elem });
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
        const case = findValueTransformTagCase(cases, source_label, seen_cases) orelse {
            executableInvariant("tag-union value transform omitted a source tag case");
        };
        const target_tag = tagTypeForLabel(program, target, case.target_tag);

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

    const source_expr = try program.ast.addExpr(source_union_ty, value, .{ .value_ref = value });
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
            .expr = try program.ast.addExpr(target_payload.ty, transformed, .{ .value_ref = transformed }),
            .ty = target_payload.ty,
            .value = transformed,
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
    cases: []const checked_artifact.ValueTransformTagCase,
    source_label: canonical.TagLabelId,
    seen: []bool,
) ?checked_artifact.ValueTransformTagCase {
    for (cases, 0..) |case, i| {
        if (case.source_tag != source_label) continue;
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

    const source_expr = try program.ast.addExpr(source_box_ty, value, .{ .value_ref = value });
    const unboxed_value = program.ast.freshValueRef();
    const args = [_]Ast.ExprId{source_expr};
    const unboxed_expr = try program.ast.addExpr(payload_ty, unboxed_value, .{ .low_level = .{
        .op = .box_unbox,
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

    const payload_expr = try program.ast.addExpr(payload_ty, payload, .{ .value_ref = payload });
    const boxed_value = program.ast.freshValueRef();
    const args = [_]Ast.ExprId{payload_expr};
    const boxed_expr = try program.ast.addExpr(target_box_ty, boxed_value, .{ .low_level = .{
        .op = .box_box,
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

            const hidden_capture = if (erased_ty.capture_ty == null) null else value;
            const packed_value = program.ast.freshValueRef();
            const packed_expr = try program.ast.addExpr(result_ty, packed_value, .{ .packed_erased_fn = .{
                .sig_key = finite.adapter_key.erased_fn_sig_key,
                .code = executableProcForErasedAdapter(program, finite.adapter_key),
                .capture = hidden_capture,
                .capture_ty = erased_ty.capture_ty,
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
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    transform_id: checked_artifact.ExecutableValueTransformPlanId,
    structural: checked_artifact.ExecutableStructuralBridgePlan,
) Allocator.Error!Ast.BridgeId {
    const plan = transforms.get(transform_id);
    const from_ty = try published_types.lower(plan.from.ty, plan.from.key);
    const to_ty = try published_types.lower(plan.to.ty, plan.to.key);
    return try lowerExecutableStructuralBridgePlan(program, published_types, transforms, from_ty, to_ty, structural);
}

fn lowerExecutableValueChildBridge(
    program: *Program,
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    child: checked_artifact.ExecutableValueTransformPlanId,
) Allocator.Error!Ast.BridgeId {
    const plan = transforms.get(child);
    return switch (plan.op) {
        .identity => try program.ast.addBridgePlan(.direct),
        .structural_bridge => |structural| try lowerPublishedExecutableValueTransformAsBridge(
            program,
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
    published_types: *PublishedTypeLowerer,
    transforms: *const checked_artifact.ExecutableValueTransformPlanStore,
    from_ty: Type.TypeId,
    to_ty: Type.TypeId,
    op: checked_artifact.ExecutableStructuralBridgePlan,
) Allocator.Error!Ast.BridgeId {
    const plan: Ast.BridgePlan = switch (op) {
        .direct => .direct,
        .zst => .zst,
        .list_reinterpret => .list_reinterpret,
        .nominal_reinterpret => .nominal_reinterpret,
        .box_unbox => |child| .{ .box_unbox = try lowerExecutableValueChildBridge(program, published_types, transforms, child) },
        .box_box => |child| .{ .box_box = try lowerExecutableValueChildBridge(program, published_types, transforms, child) },
        .singleton_to_tag_union => |singleton| .{ .singleton_to_tag_union = .{
            .source_payload = from_ty,
            .target_discriminant = try tagDiscriminantForLabel(program, to_ty, singleton.target_tag),
            .payload_plan = if (singleton.value_transform) |payload| try lowerExecutableValueChildBridge(program, published_types, transforms, payload) else null,
        } },
        .tag_union_to_singleton => |singleton| .{ .tag_union_to_singleton = .{
            .target_payload = to_ty,
            .source_discriminant = try tagDiscriminantForLabel(program, from_ty, singleton.source_tag),
            .payload_plan = if (singleton.value_transform) |payload| try lowerExecutableValueChildBridge(program, published_types, transforms, payload) else null,
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
                .materialization = .{
                    .plans = &root.artifact.comptime_plans,
                    .values = &root.artifact.comptime_values,
                },
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
        .materialization = .{
            .plans = view.comptime_plans,
            .values = view.comptime_values,
        },
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
                .{
                    .plans = &root.artifact.comptime_plans,
                    .values = &root.artifact.comptime_values,
                },
                root.artifact.const_instances.view(),
                ref,
            );
        }
        for (root.relation_artifacts) |related| {
            if (!artifactKeyEql(related.key, ref.owner)) continue;
            return resolveConstInstanceInView(.{
                .plans = related.comptime_plans,
                .values = related.comptime_values,
            }, related.const_instances, ref);
        }
    }
    for (artifact_views.imports) |imported| {
        if (!artifactKeyEql(imported.key, ref.owner)) continue;
        return resolveConstInstanceInView(.{
            .plans = imported.comptime_plans,
            .values = imported.comptime_values,
        }, imported.const_instances, ref);
    }
    executableInvariant("executable constant materialization referenced an artifact that was not published to executable MIR");
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
    return try program.ast.addExpr(expected_ty, out, .{ .list = try program.ast.addExprSpan(exprs) });
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
    return try program.ast.addExpr(expected_ty, out, .{ .tuple = try program.ast.addExprSpan(exprs) });
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
    if (schema_fields.len != value_fields.len or record_ty.fields.len != value_fields.len) {
        executableInvariant("executable pure compile-time record materialization field count mismatch");
    }
    const seen = try allocator.alloc(bool, schema_fields.len);
    defer allocator.free(seen);
    @memset(seen, false);
    const output_fields = try allocator.alloc(Ast.RecordFieldExpr, record_ty.fields.len);
    defer allocator.free(output_fields);
    for (record_ty.fields, 0..) |expected_field, expected_i| {
        const expected_label = program.row_shapes.recordField(expected_field.field).label;
        const materialized = findPureComptimeRecordField(schema_fields, value_fields, expected_label, seen) orelse {
            executableInvariant("executable pure compile-time record materialization missing expected field");
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
        };
    }
    verifyAllSeen(seen, "executable pure compile-time record materialization had extra field");
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
    schemas: []const checked_artifact.ComptimeFieldSchema,
    values: []const checked_artifact.ComptimeValueId,
    label: canonical.RecordFieldLabelId,
    seen: []bool,
) ?PureComptimeField {
    for (schemas, 0..) |schema, i| {
        if (schema.name != label) continue;
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
        else => executableInvariant("executable pure compile-time tag materialization expected tag-union type"),
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
    const selected = findPureComptimeTagType(program, tag_union_ty, schema_variant.name) orelse {
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
    if (!nominalTypeKeyEql(expected_nominal.nominal, nominal)) {
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
        const materialized = findErasedCaptureRecordField(fields, expected_label, seen) orelse {
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
    fields: []const checked_artifact.ErasedCaptureExecutableMaterializationRecordField,
    expected_label: canonical.RecordFieldLabelId,
    seen: []bool,
) ?checked_artifact.ErasedCaptureExecutableMaterializationRecordField {
    for (fields, 0..) |field, i| {
        if (field.field != expected_label) continue;
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
    return try program.ast.addExpr(expected_ty, value, .{ .tuple = try program.ast.addExprSpan(exprs) });
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
    const selected = findErasedCaptureTagType(program, tag_union_ty, tag.tag) orelse {
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
    return try program.ast.addExpr(expected_ty, value, .{ .list = try program.ast.addExprSpan(exprs) });
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
    if (!nominalTypeKeyEql(expected_nominal.nominal, nominal.nominal)) {
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
    const descriptor_member = programCallableSetMember(program, finite.callable_set_key, finite.selected_member) orelse {
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
    const packed = try program.ast.addExpr(expected_ty, value, .{ .packed_erased_fn = .{
        .sig_key = erased.sig_key,
        .code = executableProcForErasedCode(program, erased.code),
        .capture = capture_ref,
        .capture_ty = erased_ty.capture_ty,
        .capture_shape = erased_ty.capture_shape,
    } });
    if (stmt_ids.len == 0) return packed;
    return try program.ast.addExpr(expected_ty, value, .{ .block = .{
        .stmts = try program.ast.addStmtSpan(stmt_ids),
        .final_expr = packed,
    } });
}

fn executableProcForErasedCode(
    program: *const Program,
    code: canonical.ErasedCallableCodeRef,
) Ast.ExecutableProcId {
    return switch (code) {
        .direct_proc_value => |direct| executableProcForCallable(program, direct.proc_value),
        .finite_set_adapter => |adapter| executableProcForErasedAdapter(program, adapter),
    };
}

fn executableProcForCallable(
    program: *const Program,
    callable: canonical.ProcedureCallableRef,
) Ast.ExecutableProcId {
    for (program.procs.items) |proc| {
        const source = switch (proc.origin) {
            .source => |source| source,
            .erased_adapter => continue,
        };
        if (canonical.procedureCallableRefEql(source.callable, callable)) return proc.executable_proc;
    }
    executableInvariant("executable erased promoted wrapper referenced an unreserved callable procedure");
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

const PublishedTypeLowerer = struct {
    allocator: Allocator,
    payloads: *const checked_artifact.ExecutableTypePayloadStore,
    output: *Type.Store,
    row_shapes: *MonoRow.Store,
    active: std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, Type.TypeId),

    fn init(
        allocator: Allocator,
        payloads: *const checked_artifact.ExecutableTypePayloadStore,
        output: *Type.Store,
        row_shapes: *MonoRow.Store,
    ) PublishedTypeLowerer {
        return .{
            .allocator = allocator,
            .payloads = payloads,
            .output = output,
            .row_shapes = row_shapes,
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
        return try self.lowerPayload(ref.payload);
    }

    fn lowerPayload(
        self: *PublishedTypeLowerer,
        id: checked_artifact.ExecutableTypePayloadId,
    ) Allocator.Error!Type.TypeId {
        if (self.active.get(id)) |existing| return existing;

        const ty = try self.output.addType(.placeholder);
        try self.active.put(id, ty);
        errdefer _ = self.active.remove(id);

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
                .nominal = nominal.nominal,
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
            .recursive_ref => |ref| .{ .link = try self.lowerPayload(ref) },
        };
    }

    fn lowerRecordPayload(
        self: *PublishedTypeLowerer,
        fields: []const checked_artifact.ExecutableRecordFieldPayload,
    ) Allocator.Error!Type.Content {
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, fields.len);
        defer self.allocator.free(labels);
        for (fields, 0..) |field, i| labels[i] = field.field;
        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        const shape_fields = self.row_shapes.recordShapeFields(shape);
        if (shape_fields.len != fields.len) {
            executableInvariant("executable published record payload shape arity mismatch");
        }

        const out = try self.allocator.alloc(Type.RecordFieldType, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            out[i] = .{
                .field = shape_fields[i],
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
        const descriptors = try self.allocator.alloc(MonoRow.TagShapeDescriptor, variants.len);
        defer self.allocator.free(descriptors);
        for (variants, 0..) |variant, i| {
            descriptors[i] = .{
                .name = variant.tag,
                .payload_arity = @intCast(variant.payloads.len),
            };
        }
        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        const shape_tags = self.row_shapes.tagUnionTags(shape);
        if (shape_tags.len != variants.len) executableInvariant("executable published tag payload shape arity mismatch");

        const out = try self.allocator.alloc(Type.TagType, variants.len);
        for (out) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (out) |tag| self.allocator.free(tag.payloads);
            self.allocator.free(out);
        }
        for (variants, 0..) |variant, i| {
            const payloads = try self.lowerTagPayloads(shape_tags[i], variant.payloads);
            out[i] = .{
                .tag = shape_tags[i],
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
};

const SessionTypeLowerer = struct {
    allocator: Allocator,
    payloads: *const repr.SessionExecutableTypePayloadStore,
    output: *Type.Store,
    active: std.AutoHashMap(repr.SessionExecutableTypePayloadId, Type.TypeId),

    fn init(
        allocator: Allocator,
        payloads: *const repr.SessionExecutableTypePayloadStore,
        output: *Type.Store,
    ) SessionTypeLowerer {
        return .{
            .allocator = allocator,
            .payloads = payloads,
            .output = output,
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
        return try self.lowerPayload(ref.payload);
    }

    fn lowerPayload(
        self: *SessionTypeLowerer,
        id: repr.SessionExecutableTypePayloadId,
    ) Allocator.Error!Type.TypeId {
        if (self.active.get(id)) |existing| return existing;

        const ty = try self.output.addType(.placeholder);
        try self.active.put(id, ty);
        errdefer _ = self.active.remove(id);

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
        };
    }

    fn deinit(self: *TypeLowerer) void {
        self.active.deinit();
    }

    fn lowerType(self: *TypeLowerer, source: LambdaSolved.Type.TypeVarId) Allocator.Error!Type.TypeId {
        const root = self.input.unlinkConst(source);
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
        const shape_fields = self.row_shapes.recordShapeFields(shape);
        if (shape_fields.len != source_fields.len) executableInvariant("executable type lowering record shape arity mismatch");

        const fields = try self.allocator.alloc(Type.RecordFieldType, source_fields.len);
        errdefer self.allocator.free(fields);
        for (source_fields, 0..) |field, i| {
            fields[i] = .{
                .field = shape_fields[i],
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
        const descriptors = try self.allocator.alloc(MonoRow.TagShapeDescriptor, source_tags.len);
        defer self.allocator.free(descriptors);
        for (source_tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = tag.args.len,
            };
        }

        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        const shape_tags = self.row_shapes.tagUnionTags(shape);
        if (shape_tags.len != source_tags.len) executableInvariant("executable type lowering tag-union shape arity mismatch");

        const tags = try self.allocator.alloc(Type.TagType, source_tags.len);
        for (tags) |*tag| tag.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags[0..source_tags.len]) |tag| {
                if (tag.payloads.len > 0) self.allocator.free(tag.payloads);
            }
            self.allocator.free(tags);
        }
        for (source_tags, 0..) |source_tag, i| {
            const source_payload_tys = self.input.sliceTypeVarSpan(source_tag.args);
            const shape_payloads = self.row_shapes.tagPayloads(shape_tags[i]);
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
                .tag = shape_tags[i],
                .payloads = payloads,
            };
        }

        return .{ .tag_union = .{
            .shape = shape,
            .tags = tags,
        } };
    }
};

const ActiveCallableSetType = struct {
    key: repr.CanonicalCallableSetKey,
    ty: Type.TypeId,
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
    proc_map: *const std.AutoHashMap(canonical.MirProcedureRef, Ast.ExecutableProcId),
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    proc_exec_map: *const std.AutoHashMap(repr.ProcRepresentationInstanceId, Ast.ExecutableProcId),
    erased_adapter_procs: []const ErasedAdapterProcReservation,
    active_callable_sets: std.ArrayList(ActiveCallableSetType),
    capture_record_arg: ?Ast.TypedValue = null,

    fn deinit(self: *BodyBuilder) void {
        self.session_type_lowerer.deinit();
        self.active_callable_sets.deinit(self.allocator);
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
            .run => |run| blk: {
                self.capture_record_arg = null;
                const body = try self.lowerExpr(run.body);
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
        const ref = self.representation_store.session_executable_type_payloads.refForKey(key) orelse {
            executableInvariant("executable session type key has no published payload");
        };
        return try self.session_type_lowerer.lower(ref, key);
    }

    fn lowerExecutableValueTypeInStore(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        value_info_id: repr.ValueInfoId,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        const value_info = value_store.values.items[@intFromEnum(value_info_id)];
        if (value_info.callable) |callable| {
            const emission = representation_store.callableEmissionPlan(callable.emission_plan);
            return switch (emission) {
                .finite => |key| try self.lowerFiniteCallableSetType(key, representation_store),
                .already_erased => |erased| try self.type_lowerer.output.addType(.{ .erased_fn = .{
                    .sig_key = erased.sig_key,
                    .capture_shape = erased.capture_shape_key,
                    .capture_ty = try self.lowerAlreadyErasedCaptureType(erased, value_store, representation_store),
                } }),
                .erase_proc_value => |erase| try self.type_lowerer.output.addType(.{ .erased_fn = .{
                    .sig_key = erase.erased_fn_sig_key,
                    .capture_shape = erase.capture_shape_key,
                    .capture_ty = try self.lowerProcValueErasedCaptureType(erase),
                } }),
                .erase_finite_set => |erase| try self.type_lowerer.output.addType(.{ .erased_fn = .{
                    .sig_key = erase.adapter.erased_fn_sig_key,
                    .capture_shape = erase.adapter.capture_shape_key,
                    .capture_ty = try self.lowerFiniteSetAdapterCaptureType(erase.adapter),
                } }),
            };
        }
        if (value_info.aggregate) |aggregate| {
            if (value_store != self.value_store or representation_store != self.representation_store) {
                return try self.lowerForeignAggregateExecutableValueType(logical_ty, aggregate, value_store, representation_store);
            }
            return try self.lowerAggregateExecutableValueType(logical_ty, aggregate);
        }
        return try self.type_lowerer.lowerType(logical_ty);
    }

    fn lowerFiniteCallableSetType(
        self: *BodyBuilder,
        key: repr.CanonicalCallableSetKey,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        for (self.active_callable_sets.items) |active| {
            if (repr.callableSetKeyEql(active.key, key)) return active.ty;
        }

        _ = representation_store;
        const descriptor = callableSetDescriptorFromSlice(self.callable_set_descriptors, key) orelse {
            executableInvariant("executable callable-set type has no descriptor");
        };
        const ty = try self.type_lowerer.output.addType(.placeholder);
        try self.active_callable_sets.append(self.allocator, .{
            .key = key,
            .ty = ty,
        });
        errdefer _ = self.active_callable_sets.pop();

        const members = try self.allocator.alloc(Type.CallableSetMemberType, descriptor.members.len);
        errdefer self.allocator.free(members);
        for (descriptor.members, 0..) |member, i| {
            members[i] = .{
                .member = member.member,
                .payload_ty = try self.lowerCallableSetMemberPayloadType(member),
            };
        }

        self.type_lowerer.output.types.items[@intFromEnum(ty)] = .{ .callable_set = .{
            .key = key,
            .members = members,
        } };
        _ = self.active_callable_sets.pop();
        return ty;
    }

    fn lowerCallableSetMemberPayloadType(
        self: *BodyBuilder,
        member: repr.CanonicalCallableSetMember,
    ) Allocator.Error!?Type.TypeId {
        if (member.capture_slots.len == 0) return null;

        const target_instance_id = self.proc_instance_map.get(member.source_proc) orelse {
            executableInvariant("executable callable-set member has no reserved representation instance");
        };
        const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
        const target_value_store = &self.value_stores[@intFromEnum(target_instance.value_store)];
        const target_representation_store = &self.solve_sessions[@intFromEnum(target_instance.solve_session)].representation_store;
        const captures = target_value_store.sliceValueSpan(target_instance.public_roots.captures);
        if (captures.len != member.capture_slots.len) {
            executableInvariant("executable callable-set member capture count differs from target procedure captures");
        }

        const items = try self.allocator.alloc(Type.TypeId, captures.len);
        errdefer self.allocator.free(items);
        const seen = try self.allocator.alloc(bool, captures.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (member.capture_slots) |slot| {
            const index: usize = @intCast(slot.slot);
            if (index >= captures.len) executableInvariant("executable callable-set member capture slot exceeded target capture arity");
            if (seen[index]) executableInvariant("executable callable-set member capture slot was duplicated");
            const capture = captures[index];
            const info = target_value_store.values.items[@intFromEnum(capture)];
            if (builtin.mode == .Debug) {
                const actual_key = try repr.execValueTypeKeyForValue(
                    self.allocator,
                    self.canonical_names,
                    self.type_lowerer.input,
                    target_representation_store,
                    target_value_store,
                    capture,
                );
                if (!repr.canonicalExecValueTypeKeyEql(slot.exec_value_ty, actual_key)) {
                    executableInvariant("executable callable-set member capture slot executable type key differs from target capture value");
                }
            }
            items[index] = try self.lowerExecutableValueTypeInStore(
                info.logical_ty,
                capture,
                target_value_store,
                target_representation_store,
            );
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable callable-set member capture slots were not dense");
        }

        return try self.type_lowerer.output.addType(.{ .tuple = items });
    }

    fn lowerForeignAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        aggregate: repr.AggregateValueInfo,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        return switch (aggregate) {
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.RecordFieldType, record.fields.len);
                errdefer if (fields.len > 0) self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    const child = value_store.values.items[@intFromEnum(field.value)];
                    fields[i] = .{
                        .field = field.field,
                        .ty = try self.lowerExecutableValueTypeInStore(child.logical_ty, field.value, value_store, representation_store),
                    };
                }
                break :blk try self.type_lowerer.output.addType(.{ .record = .{
                    .shape = record.shape,
                    .fields = fields,
                } });
            },
            .tuple => |tuple| blk: {
                const items = try self.allocator.alloc(Type.TypeId, tuple.len);
                errdefer if (items.len > 0) self.allocator.free(items);
                const seen = try self.allocator.alloc(bool, tuple.len);
                defer self.allocator.free(seen);
                @memset(seen, false);

                for (tuple) |elem| {
                    const index: usize = @intCast(elem.index);
                    if (index >= tuple.len) executableInvariant("executable foreign aggregate tuple element index exceeded tuple arity");
                    if (seen[index]) executableInvariant("executable foreign aggregate tuple had duplicate element index");
                    const child = value_store.values.items[@intFromEnum(elem.value)];
                    items[index] = try self.lowerExecutableValueTypeInStore(child.logical_ty, elem.value, value_store, representation_store);
                    seen[index] = true;
                }
                for (seen) |was_seen| {
                    if (!was_seen) executableInvariant("executable foreign aggregate tuple did not provide every element");
                }

                break :blk try self.type_lowerer.output.addType(.{ .tuple = items });
            },
            .tag => |tag| try self.lowerForeignTagAggregateExecutableValueType(logical_ty, tag, value_store, representation_store),
            .list => |list| try self.lowerForeignListAggregateExecutableValueType(logical_ty, list, value_store, representation_store),
        };
    }

    fn lowerForeignListAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        list: anytype,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        if (list.elems.len == 0) {
            const elem_ty = try self.lowerLogicalListElemType(logical_ty);
            return try self.type_lowerer.output.addType(.{ .list = elem_ty });
        }
        const first = value_store.values.items[@intFromEnum(list.elems[0])];
        const elem_ty = try self.lowerExecutableValueTypeInStore(first.logical_ty, list.elems[0], value_store, representation_store);
        return try self.type_lowerer.output.addType(.{ .list = elem_ty });
    }

    fn lowerForeignTagAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        tag: anytype,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!Type.TypeId {
        const source_tags = self.logicalTagUnionTags(logical_ty);
        const shape_tags = self.type_lowerer.row_shapes.tagUnionTags(tag.union_shape);
        if (shape_tags.len != source_tags.len) {
            executableInvariant("executable foreign aggregate tag metadata shape does not match logical type arity");
        }

        const tags = try self.allocator.alloc(Type.TagType, shape_tags.len);
        for (tags) |*item| item.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags) |item| {
                if (item.payloads.len > 0) self.allocator.free(item.payloads);
            }
            self.allocator.free(tags);
        }

        const seen_tags = try self.allocator.alloc(bool, shape_tags.len);
        defer self.allocator.free(seen_tags);
        @memset(seen_tags, false);

        for (shape_tags) |shape_tag| {
            const shape_tag_info = self.type_lowerer.row_shapes.tag(shape_tag);
            const tag_index: usize = @intCast(shape_tag_info.logical_index);
            if (tag_index >= source_tags.len) {
                executableInvariant("executable foreign aggregate tag logical index exceeded logical type arity");
            }
            if (seen_tags[tag_index]) {
                executableInvariant("executable foreign aggregate tag metadata saw duplicate tag logical index");
            }

            const payloads = if (shape_tag == tag.tag)
                try self.lowerForeignSelectedTagPayloadTypes(tag, value_store, representation_store)
            else
                try self.lowerLogicalTagPayloadTypes(shape_tag, source_tags[tag_index]);
            tags[tag_index] = .{
                .tag = shape_tag,
                .payloads = payloads,
            };
            seen_tags[tag_index] = true;
        }
        for (seen_tags) |was_seen| {
            if (!was_seen) executableInvariant("executable foreign aggregate tag metadata did not provide every logical tag");
        }

        return try self.type_lowerer.output.addType(.{ .tag_union = .{
            .shape = tag.union_shape,
            .tags = tags,
        } });
    }

    fn lowerForeignSelectedTagPayloadTypes(
        self: *BodyBuilder,
        tag: anytype,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error![]const Type.TagPayloadType {
        const shape_payloads = self.type_lowerer.row_shapes.tagPayloads(tag.tag);
        if (shape_payloads.len == 0) return &.{};
        const payloads = try self.allocator.alloc(Type.TagPayloadType, shape_payloads.len);
        errdefer self.allocator.free(payloads);
        const seen = try self.allocator.alloc(bool, shape_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tag.payloads) |payload| {
            const payload_info = self.type_lowerer.row_shapes.tagPayload(payload.payload);
            if (payload_info.tag != tag.tag) {
                executableInvariant("executable foreign aggregate selected tag payload belongs to a different tag");
            }
            const payload_index: usize = @intCast(payload_info.logical_index);
            if (payload_index >= shape_payloads.len) {
                executableInvariant("executable foreign aggregate selected tag payload index exceeded tag arity");
            }
            if (seen[payload_index]) {
                executableInvariant("executable foreign aggregate selected tag payload was duplicated");
            }
            if (payload.payload != shape_payloads[payload_index]) {
                executableInvariant("executable foreign aggregate selected tag payload id differs from row shape logical slot");
            }
            const child = value_store.values.items[@intFromEnum(payload.value)];
            payloads[payload_index] = .{
                .payload = shape_payloads[payload_index],
                .ty = try self.lowerExecutableValueTypeInStore(child.logical_ty, payload.value, value_store, representation_store),
            };
            seen[payload_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable foreign aggregate selected tag did not provide every payload");
        }
        return payloads;
    }

    fn lowerAlreadyErasedCaptureType(
        self: *BodyBuilder,
        erased: repr.AlreadyErasedCallablePlan,
        value_store: *const repr.ValueInfoStore,
        representation_store: *const repr.RepresentationStore,
    ) Allocator.Error!?Type.TypeId {
        return switch (erased.capture) {
            .none => blk: {
                if (erased.sig_key.capture_ty != null) {
                    executableInvariant("executable already-erased capture plan is none but signature has hidden capture type");
                }
                break :blk null;
            },
            .zero_sized_ty => |logical_ty| blk: {
                const capture_key = erased.sig_key.capture_ty orelse {
                    executableInvariant("executable already-erased zero-sized capture has no hidden capture type key");
                };
                if (builtin.mode == .Debug) {
                    const actual_key = try repr.execValueTypeKey(
                        self.allocator,
                        self.canonical_names,
                        self.type_lowerer.input,
                        logical_ty,
                    );
                    if (!repr.canonicalExecValueTypeKeyEql(actual_key, capture_key)) {
                        executableInvariant("executable already-erased zero-sized capture type disagrees with signature key");
                    }
                }
                break :blk try self.type_lowerer.lowerType(logical_ty);
            },
            .value => |capture_value| blk: {
                const capture_key = erased.sig_key.capture_ty orelse {
                    executableInvariant("executable already-erased capture value has no hidden capture type key");
                };
                const capture_info = value_store.values.items[@intFromEnum(capture_value)];
                if (builtin.mode == .Debug) {
                    const actual_key = try repr.execValueTypeKeyForValue(
                        self.allocator,
                        self.canonical_names,
                        self.type_lowerer.input,
                        representation_store,
                        value_store,
                        capture_value,
                    );
                    if (!repr.canonicalExecValueTypeKeyEql(actual_key, capture_key)) {
                        executableInvariant("executable already-erased capture value type disagrees with signature key");
                    }
                }
                break :blk try self.lowerExecutableValueTypeInStore(
                    capture_info.logical_ty,
                    capture_value,
                    value_store,
                    representation_store,
                );
            },
        };
    }

    fn lowerProcValueErasedCaptureType(
        self: *BodyBuilder,
        erase: repr.ProcValueErasePlan,
    ) Allocator.Error!?Type.TypeId {
        if (erase.erased_fn_sig_key.capture_ty == null) {
            if (erase.capture_slots.len != 0) executableInvariant("executable proc-value erase plan has captures but no hidden capture type");
            return null;
        }

        const items: []Type.TypeId = if (erase.capture_slots.len == 0)
            &.{}
        else
            try self.allocator.alloc(Type.TypeId, erase.capture_slots.len);
        errdefer if (items.len > 0) self.allocator.free(items);

        const seen: []bool = if (erase.capture_slots.len == 0)
            &.{}
        else
            try self.allocator.alloc(bool, erase.capture_slots.len);
        defer if (seen.len > 0) self.allocator.free(seen);
        if (seen.len > 0) @memset(seen, false);

        for (erase.capture_slots) |slot| {
            const index: usize = @intCast(slot.slot);
            if (index >= erase.capture_slots.len) executableInvariant("executable proc-value erase capture slot exceeds source capture arity");
            if (seen[index]) executableInvariant("executable proc-value erase capture slot was duplicated");
            items[index] = try self.lowerSessionExecutableTypeKey(slot.exec_value_ty);
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable proc-value erase plan did not provide every hidden capture slot");
        }

        return try self.type_lowerer.output.addType(.{ .tuple = items });
    }

    fn lowerFiniteSetAdapterCaptureType(
        self: *BodyBuilder,
        adapter: repr.ErasedAdapterKey,
    ) Allocator.Error!?Type.TypeId {
        if (adapter.erased_fn_sig_key.capture_ty == null) return null;
        return try self.lowerFiniteCallableSetType(adapter.callable_set_key, self.representation_store);
    }

    fn lowerAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        aggregate: repr.AggregateValueInfo,
    ) Allocator.Error!Type.TypeId {
        return switch (aggregate) {
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.RecordFieldType, record.fields.len);
                errdefer if (fields.len > 0) self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    const child = self.value_store.values.items[@intFromEnum(field.value)];
                    fields[i] = .{
                        .field = field.field,
                        .ty = try self.lowerExecutableValueType(child.logical_ty, field.value),
                    };
                }
                break :blk try self.type_lowerer.output.addType(.{ .record = .{
                    .shape = record.shape,
                    .fields = fields,
                } });
            },
            .tuple => |tuple| blk: {
                const items = try self.allocator.alloc(Type.TypeId, tuple.len);
                errdefer if (items.len > 0) self.allocator.free(items);
                const seen = try self.allocator.alloc(bool, tuple.len);
                defer self.allocator.free(seen);
                @memset(seen, false);

                for (tuple) |elem| {
                    const index: usize = @intCast(elem.index);
                    if (index >= tuple.len) executableInvariant("executable aggregate tuple element index exceeded tuple arity");
                    if (seen[index]) executableInvariant("executable aggregate tuple had duplicate element index");
                    const child = self.value_store.values.items[@intFromEnum(elem.value)];
                    items[index] = try self.lowerExecutableValueType(child.logical_ty, elem.value);
                    seen[index] = true;
                }
                for (seen) |was_seen| {
                    if (!was_seen) executableInvariant("executable aggregate tuple did not provide every element");
                }

                break :blk try self.type_lowerer.output.addType(.{ .tuple = items });
            },
            .tag => |tag| try self.lowerTagAggregateExecutableValueType(logical_ty, tag),
            .list => |list| try self.lowerListAggregateExecutableValueType(logical_ty, list),
        };
    }

    fn lowerListAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        list: anytype,
    ) Allocator.Error!Type.TypeId {
        if (list.elems.len == 0) {
            const elem_ty = try self.lowerLogicalListElemType(logical_ty);
            return try self.type_lowerer.output.addType(.{ .list = elem_ty });
        }

        const first = self.value_store.values.items[@intFromEnum(list.elems[0])];
        if (builtin.mode == .Debug) {
            const first_key = try repr.execValueTypeKeyForValue(
                self.allocator,
                self.canonical_names,
                self.type_lowerer.input,
                self.representation_store,
                self.value_store,
                list.elems[0],
            );
            for (list.elems[1..]) |elem| {
                const elem_key = try repr.execValueTypeKeyForValue(
                    self.allocator,
                    self.canonical_names,
                    self.type_lowerer.input,
                    self.representation_store,
                    self.value_store,
                    elem,
                );
                if (!repr.canonicalExecValueTypeKeyEql(first_key, elem_key)) {
                    executableInvariant("executable aggregate list elements have different executable representations");
                }
            }
        }
        const elem_ty = try self.lowerExecutableValueType(first.logical_ty, list.elems[0]);
        return try self.type_lowerer.output.addType(.{ .list = elem_ty });
    }

    fn lowerLogicalListElemType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
    ) Allocator.Error!Type.TypeId {
        const root = self.type_lowerer.input.unlinkConst(logical_ty);
        return switch (self.type_lowerer.input.getNode(root)) {
            .link => unreachable,
            .nominal => |nominal| try self.lowerLogicalListElemType(nominal.backing),
            .content => |content| switch (content) {
                .list => |elem| try self.type_lowerer.lowerType(elem),
                else => executableInvariant("executable aggregate list metadata attached to non-list logical type"),
            },
            else => executableInvariant("executable aggregate list metadata attached to unresolved logical type"),
        };
    }

    fn lowerTagAggregateExecutableValueType(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
        tag: anytype,
    ) Allocator.Error!Type.TypeId {
        const source_tags = self.logicalTagUnionTags(logical_ty);
        const shape_tags = self.type_lowerer.row_shapes.tagUnionTags(tag.union_shape);
        if (shape_tags.len != source_tags.len) {
            executableInvariant("executable aggregate tag metadata shape does not match logical type arity");
        }

        const tags = try self.allocator.alloc(Type.TagType, shape_tags.len);
        for (tags) |*item| item.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (tags) |item| {
                if (item.payloads.len > 0) self.allocator.free(item.payloads);
            }
            self.allocator.free(tags);
        }

        const seen_tags = try self.allocator.alloc(bool, shape_tags.len);
        defer self.allocator.free(seen_tags);
        @memset(seen_tags, false);

        for (shape_tags) |shape_tag| {
            const shape_tag_info = self.type_lowerer.row_shapes.tag(shape_tag);
            const tag_index: usize = @intCast(shape_tag_info.logical_index);
            if (tag_index >= source_tags.len) {
                executableInvariant("executable aggregate tag logical index exceeded logical type arity");
            }
            if (seen_tags[tag_index]) {
                executableInvariant("executable aggregate tag metadata saw duplicate tag logical index");
            }

            const payloads = if (shape_tag == tag.tag)
                try self.lowerSelectedTagPayloadTypes(tag)
            else
                try self.lowerLogicalTagPayloadTypes(shape_tag, source_tags[tag_index]);
            tags[tag_index] = .{
                .tag = shape_tag,
                .payloads = payloads,
            };
            seen_tags[tag_index] = true;
        }
        for (seen_tags) |was_seen| {
            if (!was_seen) executableInvariant("executable aggregate tag metadata did not provide every logical tag");
        }

        return try self.type_lowerer.output.addType(.{ .tag_union = .{
            .shape = tag.union_shape,
            .tags = tags,
        } });
    }

    fn logicalTagUnionTags(
        self: *BodyBuilder,
        logical_ty: LambdaSolved.Type.TypeVarId,
    ) []const LambdaSolved.Type.Tag {
        const root = self.type_lowerer.input.unlinkConst(logical_ty);
        return switch (self.type_lowerer.input.getNode(root)) {
            .link => unreachable,
            .nominal => |nominal| self.logicalTagUnionTags(nominal.backing),
            .content => |content| switch (content) {
                .tag_union => |tag_union| self.type_lowerer.input.sliceTags(tag_union.tags),
                else => executableInvariant("executable aggregate tag metadata attached to non-tag-union logical type"),
            },
            else => executableInvariant("executable aggregate tag metadata attached to unresolved logical type"),
        };
    }

    fn lowerLogicalTagPayloadTypes(
        self: *BodyBuilder,
        tag_id: MonoRow.TagId,
        source_tag: LambdaSolved.Type.Tag,
    ) Allocator.Error![]const Type.TagPayloadType {
        const source_payload_tys = self.type_lowerer.input.sliceTypeVarSpan(source_tag.args);
        const shape_payloads = self.type_lowerer.row_shapes.tagPayloads(tag_id);
        if (shape_payloads.len != source_payload_tys.len) {
            executableInvariant("executable aggregate tag logical payload arity differs from row shape");
        }
        if (shape_payloads.len == 0) return &.{};

        const payloads = try self.allocator.alloc(Type.TagPayloadType, shape_payloads.len);
        errdefer self.allocator.free(payloads);
        for (shape_payloads, 0..) |payload_id, i| {
            payloads[i] = .{
                .payload = payload_id,
                .ty = try self.type_lowerer.lowerType(source_payload_tys[i]),
            };
        }
        return payloads;
    }

    fn lowerSelectedTagPayloadTypes(
        self: *BodyBuilder,
        tag: anytype,
    ) Allocator.Error![]const Type.TagPayloadType {
        const shape_payloads = self.type_lowerer.row_shapes.tagPayloads(tag.tag);
        if (shape_payloads.len == 0) return &.{};
        const payloads = try self.allocator.alloc(Type.TagPayloadType, shape_payloads.len);
        errdefer self.allocator.free(payloads);
        const seen = try self.allocator.alloc(bool, shape_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tag.payloads) |payload| {
            const payload_info = self.type_lowerer.row_shapes.tagPayload(payload.payload);
            if (payload_info.tag != tag.tag) {
                executableInvariant("executable aggregate selected tag payload belongs to a different tag");
            }
            const payload_index: usize = @intCast(payload_info.logical_index);
            if (payload_index >= shape_payloads.len) {
                executableInvariant("executable aggregate selected tag payload index exceeded tag arity");
            }
            if (seen[payload_index]) {
                executableInvariant("executable aggregate selected tag payload was duplicated");
            }
            if (payload.payload != shape_payloads[payload_index]) {
                executableInvariant("executable aggregate selected tag payload id differs from row shape logical slot");
            }
            const child = self.value_store.values.items[@intFromEnum(payload.value)];
            payloads[payload_index] = .{
                .payload = shape_payloads[payload_index],
                .ty = try self.lowerExecutableValueType(child.logical_ty, payload.value),
            };
            seen[payload_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable aggregate selected tag did not provide every payload");
        }
        return payloads;
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

    fn lowerExpr(self: *BodyBuilder, expr_id: LambdaSolved.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.exprs.items[@intFromEnum(expr_id)];
        const lowered = switch (expr.data) {
            .var_ => |var_| blk: {
                const value = self.env.get(var_.binding_info) orelse executableInvariant("executable variable occurrence has no lowered binding value");
                const binding = self.value_store.bindings.items[@intFromEnum(var_.binding_info)];
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, binding.value),
                    value,
                    .{ .value_ref = value },
                );
            },
            .int_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .int_lit = literal }),
            .frac_f32_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .frac_f32_lit = literal }),
            .frac_f64_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .frac_f64_lit = literal }),
            .dec_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .dec_lit = literal }),
            .str_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .str_lit = literal }),
            .bool_lit => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .bool_lit = literal }),
            .unit => try self.addValueExpr(expr.ty, expr.value_info, .unit),
            .const_instance => |const_instance| blk: {
                const resolved = resolveConstInstanceForExecutable(self.program, const_instance);
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
            .record => |record| blk: {
                const fields = try self.lowerRecordFields(record.assembly_order);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .record = .{
                        .shape = record.shape,
                        .fields = fields,
                    } },
                );
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerExpr(backing);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .nominal_reinterpret = lowered_backing },
                );
            },
            .tag => |tag| blk: {
                const payloads = try self.lowerTagPayloadValues(tag.assembly_order);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tag = .{
                        .union_shape = tag.union_shape,
                        .tag = tag.tag,
                        .payloads = payloads,
                    } },
                );
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .access = .{
                        .record = record,
                        .field = access.field,
                    } },
                );
            },
            .let_ => |let_| blk: {
                const body = try self.lowerExpr(let_.body);
                const previous = try self.env.fetchPut(let_.bind.binding_info, self.exprValue(body));
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.binding_info, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.binding_info);
                    }
                }
                const rest = try self.lowerExpr(let_.rest);
                const stmt = try self.output.addStmt(.{ .decl = .{
                    .value = self.exprValue(body),
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
                const items_span = try self.lowerExprIds(items);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tuple = items_span },
                );
            },
            .list => |items| blk: {
                const items_span = try self.lowerExprIds(items);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .list = items_span },
                );
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                    } },
                );
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
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
                const args = try self.lowerExprIds(low_level.args);
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .low_level = .{
                        .op = low_level.op,
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
                break :blk try self.output.addExpr(
                    try self.lowerExecutableValueType(expr.ty, expr.value_info),
                    self.output.freshValueRef(),
                    .{ .bool_not = lowered_child },
                );
            },
            .crash => |literal| try self.addValueExpr(expr.ty, expr.value_info, .{ .crash = literal }),
            .runtime_error => try self.addValueExpr(expr.ty, expr.value_info, .runtime_error),
            .match_ => |match_| blk: {
                const cond = try self.lowerExpr(match_.cond);
                const scrutinee_exprs = [_]Ast.ExprId{cond};
                const scrutinees = [_]Ast.ExecutableValueRef{self.exprValue(cond)};
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const result_ty = try self.lowerExecutableValueType(expr.ty, expr.value_info);
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
                break :blk try self.output.addExpr(
                    result_ty,
                    self.output.freshValueRef(),
                    .{ .if_ = .{
                        .cond = cond,
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
            .inspect => |child| blk: {
                const value = try self.lowerExpr(child);
                const debug_stmt = try self.output.addStmt(.{ .debug = value });
                const stmts = try self.output.addStmtSpan(&.{debug_stmt});
                break :blk try self.output.addExpr(
                    self.output.getExpr(value).ty,
                    self.exprValue(value),
                    .{ .block = .{
                        .stmts = stmts,
                        .final_expr = value,
                    } },
                );
            },
        };
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    const PendingPatternTest = struct {
        path_value: Ast.PatternPathValuePlanId,
        test: Ast.PatternTest,
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
        const body = try self.lowerExpr(expr);
        const boundary = self.returnTransformBoundary(return_info);
        self.verifyReturnBoundary(boundary, return_info);
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        const return_value = try self.applyValueTransformBoundary(&stmt_ids, boundary, self.exprValue(body));
        const return_ty = try self.lowerSessionExecutableEndpointType(boundary.to_endpoint);
        const final_expr = try self.output.addExpr(return_ty, return_value, .{ .value_ref = return_value });
        if (stmt_ids.items.len == 0) return final_expr;

        return try self.output.addExpr(return_ty, return_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
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
        const branch_ids = self.output.branch_ids.items[branches.start..][0..branches.len];
        const join = self.value_store.joins.items[@intFromEnum(join_id)];
        const inputs = self.value_store.sliceJoinInputSpan(join.inputs);
        const transforms = self.value_store.sliceValueTransformBoundarySpan(join.input_transforms);
        if (inputs.len != transforms.len or inputs.len > branch_ids.len) {
            executableInvariant("executable source_match join transform count differs from published join inputs");
        }

        const out = try self.allocator.alloc(Ast.BranchId, branch_ids.len);
        defer self.allocator.free(out);
        var input_index: usize = 0;
        for (branch_ids, 0..) |branch_id, i| {
            const branch = self.output.branches.items[@intFromEnum(branch_id)];
            if (!self.executableExprReturnsValue(branch.body)) {
                out[i] = branch_id;
                continue;
            }
            if (input_index >= inputs.len) {
                executableInvariant("executable source_match returning branch has no published join input");
            }
            const input = inputs[input_index];
            const transform_id = transforms[input_index];
            const source = switch (input.source) {
                .source_match_branch => |source| source,
                else => executableInvariant("executable source_match join input has non-match source"),
            };
            if (@intFromEnum(source.branch) != @as(u32, @intCast(i))) {
                executableInvariant("executable source_match join input branch id differs from branch order");
            }
            const boundary = self.representation_store.valueTransformBoundary(transform_id);
            self.verifyJoinInputBoundary(boundary, input);
            out[i] = try self.output.addBranch(.{
                .pat = branch.pat,
                .guard = branch.guard,
                .body = try self.wrapJoinInputBody(branch.body, boundary, result_ty),
                .degenerate = branch.degenerate,
            });
            input_index += 1;
        }
        if (input_index != inputs.len) {
            executableInvariant("executable source_match join inputs were not consumed by returning branches");
        }
        return try self.output.addBranchSpan(out);
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

        const body_value = self.exprValue(body);
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = body_value,
            .body = body,
        } }));
        const result_value = try self.applyValueTransformBoundary(&stmt_ids, boundary, body_value);
        const final_expr = try self.output.addExpr(result_ty, result_value, .{ .value_ref = result_value });
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
            => false,
            else => true,
        };
    }

    fn returnTransformBoundary(
        self: *BodyBuilder,
        return_info_id: repr.ReturnInfoId,
    ) repr.ValueTransformBoundary {
        const index = @intFromEnum(return_info_id);
        if (index >= self.value_store.returns.items.len) {
            executableInvariant("executable return referenced missing return info");
        }
        const ret = self.value_store.returns.items[index];
        const boundary_id = ret.transform orelse {
            executableInvariant("executable return reached unfinalized return transform");
        };
        return self.representation_store.valueTransformBoundary(boundary_id);
    }

    fn verifyReturnBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        return_info_id: repr.ReturnInfoId,
    ) void {
        const ret = self.value_store.returns.items[@intFromEnum(return_info_id)];
        const actual_return = switch (boundary.kind) {
            .return_value => |return_value| return_value,
            else => executableInvariant("executable return boundary has non-return kind"),
        };
        if (@intFromEnum(actual_return) != @intFromEnum(return_info_id)) {
            executableInvariant("executable return boundary points at a different return");
        }
        if (boundary.from_value != ret.value) {
            executableInvariant("executable return boundary source value differs from return info");
        }
        const from = switch (boundary.from_endpoint.owner) {
            .local_value => |value| value,
            else => executableInvariant("executable return boundary source endpoint is not local_value"),
        };
        if (from != ret.value) {
            executableInvariant("executable return boundary source endpoint differs from return info");
        }
        switch (boundary.to_endpoint.owner) {
            .procedure_return => {},
            else => executableInvariant("executable return boundary target endpoint is not procedure_return"),
        }
        _ = self;
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

        var leaves = std.ArrayList(Ast.DecisionLeafId).empty;
        defer leaves.deinit(self.allocator);
        const root = (try self.buildSourceMatchDecisionCascade(branch_ids, 0, root_path, &path_plans, &leaves)) orelse
            executableInvariant("executable source_match decision plan had no reachable root");

        return try self.output.addPatternDecisionPlan(.{
            .scrutinees = scrutinees,
            .path_value_plans = try self.output.addPatternPathValuePlanSpan(path_plans.items),
            .root = root,
            .leaves = try self.output.addDecisionLeafSpan(leaves.items),
            .branches = branches,
        });
    }

    fn buildSourceMatchDecisionCascade(
        self: *BodyBuilder,
        branch_ids: []const Ast.BranchId,
        index: usize,
        root_path: Ast.PatternPathValuePlanId,
        path_plans: *std.ArrayList(Ast.PatternPathValuePlanId),
        leaves: *std.ArrayList(Ast.DecisionLeafId),
    ) Allocator.Error!?Ast.DecisionNodeId {
        if (index >= branch_ids.len) return null;

        const fallback = try self.buildSourceMatchDecisionCascade(branch_ids, index + 1, root_path, path_plans, leaves);
        const branch_id = branch_ids[index];
        const branch = self.output.branches.items[@intFromEnum(branch_id)];

        var tests = std.ArrayList(PendingPatternTest).empty;
        defer tests.deinit(self.allocator);
        var bindings = std.ArrayList(Ast.PatternBinding).empty;
        defer bindings.deinit(self.allocator);
        try self.collectPatternDecisionData(branch.pat, root_path, &tests, &bindings, path_plans);

        const leaf_id = try self.output.addDecisionLeaf(.{
            .branch = branch_id,
            .degenerate = branch.degenerate,
            .guard = branch.guard,
            .body = branch.body,
            .fallback = fallback,
            .bindings = try self.output.addPatternBindingSpan(bindings.items),
        });
        try leaves.append(self.allocator, leaf_id);

        var next = try self.output.addDecisionNode(.{ .leaf = leaf_id });
        var test_i = tests.items.len;
        while (test_i > 0) {
            test_i -= 1;
            const pending = tests.items[test_i];
            const edges = [_]Ast.DecisionEdge{.{
                .test = pending.test,
                .next = next,
            }};
            next = try self.output.addDecisionNode(.{ .test = .{
                .path_value = pending.path_value,
                .edges = try self.output.addDecisionEdgeSpan(&edges),
                .default = fallback,
            } });
        }

        return next;
    }

    fn addPatternPathValuePlan(
        self: *BodyBuilder,
        path_plans: *std.ArrayList(Ast.PatternPathValuePlanId),
        plan: Ast.PatternPathValuePlan,
    ) Allocator.Error!Ast.PatternPathValuePlanId {
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
            .bind => |bind| try bindings.append(self.allocator, .{
                .binder = bind,
                .source = path_value,
                .ty = pat.ty,
            }),
            .as => |as| {
                try bindings.append(self.allocator, .{
                    .binder = as.bind,
                    .source = path_value,
                    .ty = pat.ty,
                });
                try self.collectPatternDecisionData(as.pattern, path_value, tests, bindings, path_plans);
            },
            .nominal => |child| try self.collectPatternDecisionData(child, path_value, tests, bindings, path_plans),
            .bool_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .bool_literal = literal } }),
            .int_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .int_literal = literal } }),
            .frac_f32_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .float_f32_literal = literal } }),
            .frac_f64_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .float_f64_literal = literal } }),
            .dec_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .decimal_literal = literal } }),
            .str_lit => |literal| try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .str_literal = literal } }),
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
                try tests.append(self.allocator, .{ .path_value = path_value, .test = .{ .tag = tag.tag } });
                const payload_patterns = self.output.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                if (payload_patterns.len == 0) return;

                const payload_record_ty = try self.payloadRecordTypeForPattern(pat.ty, tag.tag);
                const payload_record_path = try self.addChildPatternPathValuePlan(
                    path_plans,
                    path_value,
                    .{ .tag_payload_record = tag.tag },
                    .{ .tag_payload_record = .{ .parent = path_value, .tag = tag.tag } },
                    payload_record_ty,
                );
                for (payload_patterns) |payload_pattern| {
                    const child = self.output.pats.items[@intFromEnum(payload_pattern.pattern)];
                    const child_path = try self.addChildPatternPathValuePlan(
                        path_plans,
                        payload_record_path,
                        .{ .tag_payload = payload_pattern.payload },
                        .{ .tag_payload_field = .{ .parent_payload_record = payload_record_path, .payload = payload_pattern.payload } },
                        child.ty,
                    );
                    try self.collectPatternDecisionData(payload_pattern.pattern, child_path, tests, bindings, path_plans);
                }
            },
            .list => |list| {
                const item_ids = self.output.pat_ids.items[list.items.start..][0..list.items.len];
                try tests.append(self.allocator, .{
                    .path_value = path_value,
                    .test = if (list.rest == null)
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
        const ty = try self.type_lowerer.lowerType(pat.ty);
        return try self.lowerPatScopedWithType(pat_id, ty, saved);
    }

    fn lowerPatScopedWithType(
        self: *BodyBuilder,
        pat_id: LambdaSolved.Ast.PatId,
        ty: Type.TypeId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.pats.items[@intFromEnum(pat_id)];
        return try self.output.addPat(.{ .ty = ty, .data = switch (pat.data) {
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
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads, saved),
            } },
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

    fn lowerBranch(self: *BodyBuilder, branch_id: LambdaSolved.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.branches.items[@intFromEnum(branch_id)];
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

    fn lowerStmt(self: *BodyBuilder, stmt_id: LambdaSolved.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.stmts.items[@intFromEnum(stmt_id)];
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                try self.env.put(decl.bind.binding_info, self.exprValue(body));
                break :blk .{ .decl = .{
                    .value = self.exprValue(body),
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                try self.env.put(decl.bind.binding_info, self.exprValue(body));
                break :blk .{ .decl = .{
                    .value = self.exprValue(body),
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

    fn lowerBranchSpan(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        if (span.len == 0) return Ast.Span(Ast.BranchId).empty();
        const input_items = self.input.branch_ids.items[span.start..][0..span.len];
        const branches = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(branches);
        for (input_items, 0..) |branch, i| {
            branches[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(branches);
    }

    fn lowerTagPayloadPatternSpan(
        self: *BodyBuilder,
        span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const input_items = self.input.tag_payload_patterns.items[span.start..][0..span.len];
        const payloads = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(payloads);
        for (input_items, 0..) |payload, i| {
            payloads[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPatScoped(payload.pattern, saved),
            };
        }
        return try self.output.addTagPayloadPatternSpan(payloads);
    }

    fn lowerRecordFields(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldExpr) {
        if (span.len == 0) return Ast.Span(Ast.RecordFieldExpr).empty();
        const input_items = self.input.record_field_assemblies.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.RecordFieldExpr, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |field, i| {
            const lowered = try self.lowerExpr(field.value);
            values[i] = .{
                .field = field.field,
                .expr = lowered,
                .ty = self.output.getExpr(lowered).ty,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addRecordFieldExprSpan(values);
    }

    fn lowerTagPayloadValues(self: *BodyBuilder, span: LambdaSolved.Ast.Span(LambdaSolved.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadExpr) {
        if (span.len == 0) return Ast.Span(Ast.TagPayloadExpr).empty();
        const input_items = self.input.tag_payload_assemblies.items[span.start..][0..span.len];
        const values = try self.allocator.alloc(Ast.TagPayloadExpr, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |payload, i| {
            const lowered = try self.lowerExpr(payload.value);
            values[i] = .{
                .payload = payload.payload,
                .expr = lowered,
                .ty = self.output.getExpr(lowered).ty,
                .value = self.exprValue(lowered),
            };
        }
        return try self.output.addTagPayloadExprSpan(values);
    }

    fn lowerCallProc(
        self: *BodyBuilder,
        source_ty: LambdaSolved.Type.TypeVarId,
        call: anytype,
    ) Allocator.Error!Ast.ExprId {
        const target_proc = self.proc_map.get(call.proc) orelse executableInvariant("executable call_proc target was not reserved before body lowering");
        const target_instance_id = self.proc_instance_map.get(call.proc) orelse executableInvariant("executable call_proc target has no representation instance");
        const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];

        const arg_items = self.input.expr_ids.items[call.args.start..][0..call.args.len];
        const call_site = self.value_store.call_sites.items[@intFromEnum(call.call_site)];
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable call_proc call-site requested source type differs from expression");
        }
        if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, call_site.requested_source_fn_ty)) {
            executableInvariant("executable call_proc target specialization source type differs from call site");
        }
        switch (call_site.dispatch) {
            .call_proc => |target| {
                if (target != target_instance_id) {
                    executableInvariant("executable call_proc dispatch target differs from expression target");
                }
            },
            .call_value_finite,
            .call_value_erased,
            => executableInvariant("executable call_proc reached non-procedure call-site dispatch"),
        }

        const arg_transform_ids = self.value_store.sliceValueTransformBoundarySpan(call_site.arg_transforms);
        if (arg_transform_ids.len != arg_items.len) {
            executableInvariant("executable call_proc argument transform count differs from call arity");
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
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } }));
            const boundary = self.representation_store.valueTransformBoundary(arg_transform_ids[i]);
            self.verifyCallProcArgBoundary(boundary, call.call_site, target_instance_id, target_instance, @intCast(i));
            direct_args[i] = .{ .value = try self.applyValueTransformBoundary(&stmt_ids, boundary, value) };
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
        const final_expr = try self.output.addExpr(result_ty, result_value, .{ .value_ref = result_value });

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
    }

    fn verifyCallProcArgBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        call_site_id: repr.CallSiteInfoId,
        target_instance_id: repr.ProcRepresentationInstanceId,
        target_instance: repr.ProcRepresentationInstance,
        index: u32,
    ) void {
        _ = self;
        const kind = switch (boundary.kind) {
            .call_arg => |call_arg| call_arg,
            else => executableInvariant("executable call_proc argument transform has non-call-arg boundary kind"),
        };
        if (kind.call != call_site_id or kind.arg_index != index) {
            executableInvariant("executable call_proc argument transform boundary kind differs from call site");
        }
        switch (boundary.from_endpoint.owner) {
            .local_value => {},
            else => executableInvariant("executable call_proc argument transform source is not a local value"),
        }
        const to = switch (boundary.to_endpoint.owner) {
            .procedure_param => |param| param,
            else => executableInvariant("executable call_proc argument transform target is not a procedure parameter"),
        };
        if (to.instance != target_instance_id or to.index != index) {
            executableInvariant("executable call_proc argument transform target differs from target procedure");
        }
        const arg_index: usize = @intCast(index);
        if (arg_index >= target_instance.executable_specialization_key.exec_arg_tys.len) {
            executableInvariant("executable call_proc argument transform index exceeds target arity");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
            executableInvariant("executable call_proc argument endpoint key differs from target specialization");
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
            const raw_value = self.exprValue(lowered);
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = raw_value,
                .body = lowered,
            } }));
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
        const hidden_capture_ty = try self.lowerFiniteSetAdapterCaptureType(erase.adapter);
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
            const raw_value = self.exprValue(lowered);
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = raw_value,
                .body = lowered,
            } }));
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

        const selected_executable_proc = self.executableProcForSpecializationKey(erase.executable_specialization_key);

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
            const raw_value = self.exprValue(lowered);
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = raw_value,
                .body = lowered,
            } }));
            const value = try self.applyValueTransformBoundary(&stmt_ids, boundary, raw_value);
            const transformed_ty = try self.lowerSessionExecutableEndpointType(boundary.to_endpoint);
            lowered_captures[slot_index] = try self.output.addExpr(transformed_ty, value, .{ .value_ref = value });
            seen[slot_index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) executableInvariant("executable proc-value erase plan did not provide every capture slot");
        }

        const capture_value: ?Ast.ExecutableValueRef = if (capture_ty) |ty| blk: {
            const capture_expr = if (lowered_captures.len == 0)
                try self.output.addExpr(ty, self.output.freshValueRef(), .unit)
            else
                try self.output.addExpr(ty, self.output.freshValueRef(), .{ .tuple = try self.output.addExprSpan(lowered_captures) });
            const value = self.exprValue(capture_expr);
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = capture_expr,
            } }));
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
        const func_value = self.exprValue(func);
        const call_site = self.value_store.call_sites.items[@intFromEnum(call.call_site)];
        if (!repr.canonicalTypeKeyEql(call_site.requested_source_fn_ty, call.requested_source_fn_ty)) {
            executableInvariant("executable call_value call-site requested source type differs from expression");
        }
        const callable_set_key = switch (call_site.dispatch) {
            .call_value_finite => |key| key,
            .call_value_erased => |sig_key| return try self.lowerCallValueErased(source_ty, call, func, func_value, call.call_site, call_site, sig_key),
            .call_proc => executableInvariant("executable call_value reached procedure call-site dispatch"),
        };
        const func_value_info_id = self.input.exprs.items[@intFromEnum(call.func)].value_info;
        const func_value_info = self.value_store.values.items[@intFromEnum(func_value_info_id)];
        const callable = func_value_info.callable orelse executableInvariant("executable call_value callee has no callable metadata");
        const emission = self.representation_store.callableEmissionPlan(callable.emission_plan);
        switch (emission) {
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
        stmt_ids[0] = try self.output.addStmt(.{ .decl = .{
            .value = func_value,
            .body = func,
        } });
        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            arg_values[i] = value;
            stmt_ids[i + 1] = try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } });
        }

        const requested_source_fn_ty = call_site.requested_source_fn_ty;
        const result_ty = try self.lowerExecutableValueType(source_ty, call_site.result);
        const branch_result_ids = self.value_store.sliceValueTransformBoundarySpan(call_site.branch_result_transforms);
        if (branch_result_ids.len != descriptor.members.len) {
            executableInvariant("executable call_value finite branch result transform count differs from callable-set member count");
        }
        const branches = try self.allocator.alloc(Ast.CallableMatchBranch, descriptor.members.len);
        defer self.allocator.free(branches);
        for (descriptor.members, 0..) |member, i| {
            if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value callable-set member source type differs from call site");
            }
            const target = member.source_proc;
            const executable_proc = self.proc_map.get(target) orelse executableInvariant("executable call_value member target was not reserved");
            const target_instance_id = self.proc_instance_map.get(target) orelse executableInvariant("executable call_value member target has no representation instance");
            const target_instance = self.proc_instances[@intFromEnum(target_instance_id)];
            if (!repr.canonicalTypeKeyEql(target_instance.executable_specialization_key.requested_fn_ty, requested_source_fn_ty)) {
                executableInvariant("executable call_value member target specialization source type differs from call site");
            }
            const capture_arg_len: usize = if (member.capture_slots.len == 0) 0 else 1;
            const capture_payload_ty = try self.lowerCallableSetMemberPayloadType(member);
            const capture_payload = if (capture_payload_ty != null) self.output.freshValueRef() else null;
            const direct_args = try self.allocator.alloc(Ast.DirectCallArg, arg_values.len + capture_arg_len);
            defer self.allocator.free(direct_args);
            for (arg_values, 0..) |arg_value, arg_i| {
                direct_args[arg_i] = .{ .value = arg_value };
            }
            if (capture_payload) |payload_value| {
                direct_args[arg_values.len] = .{ .value = payload_value };
            }
            const direct_args_span = try self.output.addDirectCallArgSpan(direct_args);
            const member_ref: repr.CallableSetMemberRef = .{
                .callable_set_key = callable_set_key,
                .member_index = member.member,
            };
            const result_boundary = self.representation_store.valueTransformBoundary(branch_result_ids[i]);
            self.verifyCallableMatchBranchResultBoundary(
                result_boundary,
                call.call_site,
                member_ref,
                target_instance_id,
                target_instance,
                call_site.result,
            );
            const branch_body = try self.lowerCallableMatchBranchBody(
                target,
                target_instance,
                executable_proc,
                direct_args_span,
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
                .direct_args = direct_args_span,
                .body = branch_body,
            };
        }

        const result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(result_ty, result_value, .{ .callable_match = .{
            .callable_set_key = callable_set_key,
            .requested_source_fn_ty = requested_source_fn_ty,
            .callee = func_value,
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
        func_value: Ast.ExecutableValueRef,
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
        const arg_transform_ids = self.value_store.sliceValueTransformBoundarySpan(call_site.arg_transforms);
        if (arg_transform_ids.len != arg_items.len) {
            executableInvariant("executable erased call_value argument transform count differs from call arity");
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
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = func_value,
            .body = func,
        } }));
        for (arg_items, 0..) |arg, i| {
            const lowered = try self.lowerExpr(arg);
            const value = self.exprValue(lowered);
            try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
                .value = value,
                .body = lowered,
            } }));
            const boundary = self.representation_store.valueTransformBoundary(arg_transform_ids[i]);
            self.verifyErasedCallRawArgBoundary(boundary, call_site_id, @intCast(i), sig_key);
            arg_values[i] = try self.applyValueTransformBoundary(&stmt_ids, boundary, value);
        }

        const raw_result_ty = try self.lowerSessionExecutableEndpointType(result_boundary.from_endpoint);
        const raw_result_value = self.output.freshValueRef();
        const final_call = try self.output.addExpr(raw_result_ty, raw_result_value, .{ .call_erased = .{
            .func = func_value,
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
        const final_expr = try self.output.addExpr(result_ty, result_value, .{ .value_ref = result_value });

        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
    }

    fn lowerCallableMatchBranchBody(
        self: *BodyBuilder,
        source_proc: canonical.MirProcedureRef,
        target_instance: repr.ProcRepresentationInstance,
        executable_proc: Ast.ExecutableProcId,
        direct_args: Ast.Span(Ast.DirectCallArg),
        result_ty: Ast.TypeId,
        result_boundary: ?repr.ValueTransformBoundary,
    ) Allocator.Error!Ast.ExprId {
        var stmt_ids = std.ArrayList(Ast.StmtId).empty;
        defer stmt_ids.deinit(self.allocator);

        const raw_result_ty = if (result_boundary) |boundary|
            try self.lowerSessionExecutableEndpointType(boundary.from_endpoint)
        else
            result_ty;
        const raw_result_value = self.output.freshValueRef();
        const direct_call = try self.output.addExpr(raw_result_ty, raw_result_value, .{ .call_direct = .{
            .source = source_proc.proc,
            .executable_specialization_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_instance.executable_specialization_key),
            .executable_proc = executable_proc,
            .direct_args = direct_args,
        } });
        try stmt_ids.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = raw_result_value,
            .body = direct_call,
        } }));

        const result_value = if (result_boundary) |boundary|
            try self.applyValueTransformBoundary(&stmt_ids, boundary, raw_result_value)
        else
            raw_result_value;
        const final_expr = try self.output.addExpr(result_ty, result_value, .{ .value_ref = result_value });
        return try self.output.addExpr(result_ty, result_value, .{ .block = .{
            .stmts = try self.output.addStmtSpan(stmt_ids.items),
            .final_expr = final_expr,
        } });
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

    fn verifyErasedCallRawArgBoundary(
        self: *BodyBuilder,
        boundary: repr.ValueTransformBoundary,
        call_site_id: repr.CallSiteInfoId,
        index: u32,
        sig_key: repr.ErasedFnSigKey,
    ) void {
        const abi = self.representation_store.erased_fn_abis.abiFor(sig_key.abi) orelse {
            executableInvariant("executable erased call argument transform references missing ABI payload");
        };
        if (index >= abi.arg_exec_keys.len) executableInvariant("executable erased call argument transform index exceeds ABI arity");
        const to = switch (boundary.to_endpoint.owner) {
            .call_raw_arg => |raw| raw,
            else => executableInvariant("executable erased call argument transform target is not call_raw_arg"),
        };
        if (to.call != call_site_id or to.index != index) {
            executableInvariant("executable erased call argument transform target differs from call site");
        }
        if (!repr.canonicalExecValueTypeKeyEql(boundary.to_endpoint.exec_ty.key, abi.arg_exec_keys[index])) {
            executableInvariant("executable erased call argument endpoint key differs from ABI payload");
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
            &self.program.types,
            &self.program.row_shapes,
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

        const source_expr = try self.output.addExpr(from_ty, value, .{ .value_ref = value });
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
                .expr = try self.output.addExpr(target_field.ty, transformed, .{ .value_ref = transformed }),
                .ty = target_field.ty,
                .value = transformed,
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

        const tuple_expr = try self.output.addExpr(from_ty, value, .{ .value_ref = value });
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
            output_items[i] = try self.output.addExpr(target_item_ty, transformed, .{ .value_ref = transformed });
        }
        verifyAllSeen(seen, "session tuple value transform had an extra element transform");

        const tuple_value = self.output.freshValueRef();
        const result_expr = try self.output.addExpr(to_ty, tuple_value, .{ .tuple = try self.output.addExprSpan(output_items) });
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

        const source_expr = try self.output.addExpr(from_ty, value, .{ .value_ref = value });
        const backing_value = self.output.freshValueRef();
        const backing_expr = try self.output.addExpr(source.backing, backing_value, .{ .nominal_reinterpret = source_expr });
        try stmts.append(self.allocator, try self.output.addStmt(.{ .decl = .{
            .value = backing_value,
            .body = backing_expr,
        } }));

        const transformed_backing = try self.applyExecutableValueTransformRef(stmts, nominal.backing, backing_value);
        const transformed_expr = try self.output.addExpr(target.backing, transformed_backing, .{ .value_ref = transformed_backing });
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

        const source_elem = self.output.freshValueRef();
        var body_stmts = std.ArrayList(Ast.StmtId).empty;
        defer body_stmts.deinit(self.allocator);

        const transformed_elem = try self.applyExecutableValueTransformRef(&body_stmts, elem_transform, source_elem);
        const transformed_expr = try self.output.addExpr(target_elem_ty, transformed_elem, .{ .value_ref = transformed_elem });
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

        const source_expr = try self.output.addExpr(source_union_ty, value, .{ .value_ref = value });
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
                .expr = try self.output.addExpr(target_payload.ty, transformed, .{ .value_ref = transformed }),
                .ty = target_payload.ty,
                .value = transformed,
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
                    .identity => try self.output.addBridgePlan(.direct),
                    .structural_bridge => |structural| try self.lowerSessionExecutableValueTransformAsBridge(plan, structural),
                    else => executableInvariant("session structural bridge child was not a bridge transform"),
                };
            },
            .published => |published| blk: {
                const context = resolvePublishedTransformContext(self.program, published);
                var published_types = PublishedTypeLowerer.init(
                    self.allocator,
                    context.executable_type_payloads,
                    &self.program.types,
                    &self.program.row_shapes,
                );
                defer published_types.deinit();
                break :blk try lowerExecutableValueChildBridge(
                    self.program,
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
        const plan: Ast.BridgePlan = switch (op) {
            .direct => .direct,
            .zst => .zst,
            .list_reinterpret => .list_reinterpret,
            .nominal_reinterpret => .nominal_reinterpret,
            .box_unbox => |child| .{ .box_unbox = try self.lowerSessionExecutableValueChildBridge(child) },
            .box_box => |child| .{ .box_box = try self.lowerSessionExecutableValueChildBridge(child) },
            .singleton_to_tag_union => |singleton| .{ .singleton_to_tag_union = .{
                .source_payload = from_ty,
                .target_discriminant = try tagDiscriminantForId(self.program, to_ty, singleton.target_tag),
                .payload_plan = if (singleton.value_transform) |payload| try self.lowerSessionExecutableValueChildBridge(payload) else null,
            } },
            .tag_union_to_singleton => |singleton| .{ .tag_union_to_singleton = .{
                .target_payload = to_ty,
                .source_discriminant = try tagDiscriminantForId(self.program, from_ty, singleton.source_tag),
                .payload_plan = if (singleton.value_transform) |payload| try self.lowerSessionExecutableValueChildBridge(payload) else null,
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
                const hidden_capture = if (erased_ty.capture_ty == null) null else value;
                const packed_value = self.output.freshValueRef();
                const packed_expr = try self.output.addExpr(result_ty, packed_value, .{ .packed_erased_fn = .{
                    .sig_key = finite.adapter.erased_fn_sig_key,
                    .code = executableProcForErasedAdapter(self.program, finite.adapter),
                    .capture = hidden_capture,
                    .capture_ty = erased_ty.capture_ty,
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
                const sig_has_capture = sig_key.capture_ty != null;
                const type_has_capture = erased.capture_ty != null;
                if (sig_has_capture != type_has_capture) {
                    executableInvariant("executable erased call callee type hidden capture disagrees with signature");
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
};

fn executableInvariant(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

fn executableProcForSource(program: *const Program, source_proc: canonical.MirProcedureRef) ?Ast.ExecutableProcId {
    for (program.procs.items) |proc| {
        const source = switch (proc.origin) {
            .source => |source| source,
            .erased_adapter => continue,
        };
        if (canonical.mirProcedureRefEql(source, source_proc)) return proc.executable_proc;
    }
    return null;
}

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
        .call_raw_result => |call| switch (b) {
            .call_raw_result => |other| call == other,
            else => false,
        },
        .callable_match_branch_raw_result => |branch| switch (b) {
            .callable_match_branch_raw_result => |other|
                branch.call == other.call and
                branch.member.member_index == other.member.member_index and
                repr.callableSetKeyEql(branch.member.callable_set_key, other.member.callable_set_key),
            else => false,
        },
        .transform_child => |child| switch (b) {
            .transform_child => |other|
                child.scope == other.scope and
                child.side == other.side and
                child.path == other.path,
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
