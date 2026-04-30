//! Specialization-driven mono MIR construction state.
//!
//! This is the only API that may turn checked procedure templates into mono MIR
//! procedures. It reserves procedure identities before body lowering so recursive
//! and mutually-recursive mono specializations cannot allocate duplicate
//! procedure values.

const std = @import("std");
const check = @import("check");

const Ast = @import("ast.zig");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const LowerType = @import("lower_type.zig");
const Type = @import("type.zig");
const debug = @import("../debug_verify.zig");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

pub const MonoProcHandle = enum(u32) { _ };

pub const MonoSpecializationReason = union(enum) {
    root: checked_artifact.RootRequest,
    call_proc: Ast.ExprId,
    proc_value: Ast.ExprId,
    static_dispatch_target: checked_artifact.RootSource,
    comptime_dependency_summary: u32,
};

pub const Input = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
};

pub const MonoSpecializationRequest = struct {
    template: canonical.ProcedureTemplateRef,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    reason: MonoSpecializationReason,
};

pub const ReservedState = enum {
    reserved,
    lowering,
    lowered,
};

pub const ReservedMonoProc = struct {
    proc: canonical.ProcedureValueRef,
    local_handle: MonoProcHandle,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
    state: ReservedState,
};

pub const Proc = struct {
    key: canonical.MonoSpecializationKey,
    proc: canonical.ProcedureValueRef,
    local_handle: MonoProcHandle,
    fn_ty: Type.TypeId,
    body: ?Ast.DefId = null,
};

pub const Program = struct {
    allocator: Allocator,
    root_artifact_key: checked_artifact.CheckedModuleArtifactKey,
    concrete_source_types: ConcreteSourceType.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    root_procs: std.ArrayList(canonical.ProcedureValueRef),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .root_artifact_key = .{},
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .root_procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.root_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.concrete_source_types.deinit();
        self.* = Program.init(self.allocator);
    }

    pub fn addProc(
        self: *Program,
        key: canonical.MonoSpecializationKey,
        reserved: ReservedMonoProc,
        fn_ty: Type.TypeId,
    ) Allocator.Error!void {
        try self.procs.append(self.allocator, .{
            .key = key,
            .proc = reserved.proc,
            .local_handle = reserved.local_handle,
            .fn_ty = fn_ty,
        });
    }
};

pub fn run(
    allocator: Allocator,
    input: Input,
    roots: []const checked_artifact.RootRequest,
) Allocator.Error!Program {
    var program = Program.init(allocator);
    errdefer program.deinit();
    program.root_artifact_key = input.root.artifact.key;

    var queue = Queue.init(allocator);
    defer queue.deinit();

    for (roots) |root| {
        const template = templateForRoot(input, root) orelse continue;
        const requested_fn_ty = try program.concrete_source_types.registerArtifactRoot(
            input.root.artifact.key,
            input.root.artifact.checked_types.view(),
            root.checked_type,
        );
        const request = MonoSpecializationRequest{
            .template = template,
            .requested_fn_ty = requested_fn_ty,
            .reason = .{ .root = root },
        };
        const reserved = try queue.reserve(&program.concrete_source_types, request);
        try program.root_procs.append(allocator, reserved.proc);
    }

    while (queue.pending.items.len != 0) {
        const key = queue.pending.orderedRemove(0);
        queue.markLowering(key);
        const reserved = queue.requested.get(key) orelse unreachable;
        _ = checkedTemplateForKey(input, key.template);
        const fn_ty = try lowerConcreteFnType(allocator, input, &program, reserved.requested_fn_ty);
        try program.addProc(key, reserved, fn_ty);
        queue.markLowered(key);
    }

    verifyProgram(&program);
    return program;
}

const CheckedTemplateLookup = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    checked_types: checked_artifact.CheckedTypeStoreView,
    checked_bodies: checked_artifact.CheckedBodyStoreView,
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
            .template = input.root.artifact.checked_procedure_templates.get(template_ref.template),
        };
    }

    for (input.imports) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &template_ref.artifact.bytes)) continue;
        for (imported.exported_procedure_templates.templates) |exported| {
            if (exported.template.proc_base == template_ref.proc_base and
                exported.template.template == template_ref.template)
            {
                return .{
                    .artifact = imported.key,
                    .checked_types = imported.checked_types,
                    .checked_bodies = imported.checked_bodies,
                    .template = exported.template_data,
                };
            }
        }
        debug.invariant(false, "mono specialization invariant violated: imported template was not exported or present in the imported closure");
        unreachable;
    }

    for (input.root.relation_artifacts) |related| {
        if (!std.mem.eql(u8, &related.key.bytes, &template_ref.artifact.bytes)) continue;
        for (related.exported_procedure_templates.templates) |exported| {
            if (exported.template.proc_base == template_ref.proc_base and
                exported.template.template == template_ref.template)
            {
                return .{
                    .artifact = related.key,
                    .checked_types = related.checked_types,
                    .checked_bodies = related.checked_bodies,
                    .template = exported.template_data,
                };
            }
        }
        debug.invariant(false, "mono specialization invariant violated: relation template was not exported or present in the relation closure");
        unreachable;
    }

    debug.invariant(false, "mono specialization invariant violated: template artifact was not available to lowering");
    unreachable;
}

fn lowerConcreteFnType(
    allocator: Allocator,
    input: Input,
    program: *Program,
    requested_fn_ty: ConcreteSourceType.ConcreteSourceTypeRef,
) Allocator.Error!Type.TypeId {
    const concrete = program.concrete_source_types.root(requested_fn_ty);
    const checked_types = checkedTypesForKey(input, concrete.source.artifact) orelse {
        debug.invariant(false, "mono specialization invariant violated: concrete source type payload references unavailable artifact");
        unreachable;
    };
    var lowerer = LowerType.Lowerer.init(
        allocator,
        checked_types,
        &program.types,
    );
    defer lowerer.deinit();
    return try lowerer.lowerChecked(concrete.source.ty);
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

fn templateForRoot(
    input: Input,
    root: checked_artifact.RootRequest,
) ?canonical.ProcedureTemplateRef {
    const artifact = input.root.artifact;
    switch (root.source) {
        .def => |def_idx| return artifact.checked_procedure_templates.lookupByDef(def_idx),
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
                .procedure_value => |proc_use| templateForProcedureUse(input, proc_use),
            };
        },
        .expr, .statement => return null,
    }
}

fn templateForProcedureUse(
    input: Input,
    proc_use: checked_artifact.ProcedureUseTemplate,
) ?canonical.ProcedureTemplateRef {
    return switch (proc_use.binding) {
        .top_level => |binding_ref| templateFromTopLevelBinding(
            &input.root.artifact.top_level_procedure_bindings,
            binding_ref,
        ),
        .platform_required => |required| {
            const bindings = topLevelProcedureBindingsForKey(input, required.artifact) orelse {
                debug.invariant(false, "mono specialization invariant violated: platform-required procedure binding references unavailable app artifact");
                unreachable;
            };
            return templateFromTopLevelBinding(
                bindings,
                required.procedure_binding,
            );
        },
        .imported, .hosted, .promoted => {
            debug.invariant(false, "mono specialization invariant violated: platform-required root resolved to unsupported procedure binding kind");
            unreachable;
        },
    };
}

fn templateFromTopLevelBinding(
    bindings: *const checked_artifact.TopLevelProcedureBindingTable,
    binding_ref: checked_artifact.TopLevelProcedureBindingRef,
) ?canonical.ProcedureTemplateRef {
    const binding = bindings.get(binding_ref);
    return switch (binding.body) {
        .direct_template => |direct| switch (direct.template) {
            .checked => |template| template,
            .lifted, .synthetic => {
                debug.invariant(false, "mono specialization invariant violated: root procedure binding must have a checked template before mono lowering");
                unreachable;
            },
        },
        .callable_eval_template => {
            debug.invariant(false, "mono specialization invariant violated: callable-eval platform-required roots need a sealed concrete callable binding instance before mono lowering");
            unreachable;
        },
    };
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

fn verifyProgram(program: *const Program) void {
    if (@import("builtin").mode != .Debug) return;
    for (program.root_procs.items) |root| {
        var found = false;
        for (program.procs.items) |proc| {
            if (std.mem.eql(u8, &proc.proc.artifact.bytes, &root.artifact.bytes) and
                proc.proc.proc_base == root.proc_base)
            {
                found = true;
                break;
            }
        }
        std.debug.assert(found);
    }
}

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
            .proc = .{ .artifact = request.template.artifact, .proc_base = request.template.proc_base },
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
        .reason = .{ .comptime_dependency_summary = 0 },
    };

    const first = try queue.reserve(&concrete, request);
    const second = try queue.reserve(&concrete, request);
    try std.testing.expectEqual(first.local_handle, second.local_handle);
    try std.testing.expectEqual(@as(usize, 1), queue.pending.items.len);
}
