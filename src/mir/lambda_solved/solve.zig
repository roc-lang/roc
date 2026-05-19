//! Lambda-solved MIR construction state.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const symbol_mod = @import("symbol");
const ArtifactNames = @import("../artifact_names.zig");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const Lifted = @import("../lifted/mod.zig");
const MonoLowerType = @import("../mono/lower_type.zig");
const MonoRow = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const repr = @import("representation.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

/// Public `ArtifactViews` declaration.
pub const ArtifactViews = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
    collect_provided_data_exports: bool = false,
};

/// Public `Proc` declaration.
pub const Proc = struct {
    proc: canonical.MirProcedureRef,
    body: Ast.DefId,
    representation_instance: repr.ProcRepresentationInstanceId,
};

const ProcBuildRecord = struct {
    proc: canonical.MirProcedureRef,
    kind: ProcBuildRecordKind,
    body: Ast.DefId = @enumFromInt(std.math.maxInt(u32)),
    representation_instance: repr.ProcRepresentationInstanceId,
    solve_session: repr.RepresentationSolveSessionId,
    recursive_group_anchor: ?repr.ProcRepresentationInstanceId = null,
    value_store: repr.ValueInfoStoreId,
    owner: ProcedureInstanceOwner,
    public_roots: repr.ProcPublicValueRoots = undefined,
    has_public_roots: bool = false,
    materialized: bool = true,
    built: bool = false,
};

const ProcBuildRecordKind = union(enum) {
    normal,
    executable_synthetic: u32,
};

const SealedErasedAdapterMemberReservation = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    payloads: *const checked_artifact.ExecutableTypePayloadStore,
    target_key: repr.ExecutableSpecializationKey,
    provenance: []const repr.BoxErasureProvenance,
};

/// Public `ExecutableSyntheticProcInstance` declaration.
pub const ExecutableSyntheticProcInstance = struct {
    source_proc: canonical.MirProcedureRef,
    synthetic_index: u32,
    representation_instance: repr.ProcRepresentationInstanceId,
};

const ProcedureCaptureSource = struct {
    target_instance: repr.ProcRepresentationInstanceId,
    slot: u32,
    source_store: repr.ValueInfoStoreId,
    source_value: repr.ValueInfoId,
};

/// Public `Program` declaration.
pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    concrete_source_types: ConcreteSourceType.Store,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    executable_synthetic_procs: std.ArrayList(ids.ExecutableSyntheticProc),
    executable_synthetic_proc_instances: std.ArrayList(ExecutableSyntheticProcInstance),
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    root_instances: std.ArrayList(repr.ProcRepresentationInstanceId),
    root_metadata: std.ArrayList(ids.RootMetadata),
    solve_sessions: std.ArrayList(repr.RepresentationSolveSession),
    proc_instances: std.ArrayList(repr.ProcRepresentationInstance),
    value_stores: std.ArrayList(repr.ValueInfoStore),
    procedure_capture_sources: std.ArrayList(ProcedureCaptureSource),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .concrete_source_types = ConcreteSourceType.Store.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .executable_synthetic_procs = .empty,
            .executable_synthetic_proc_instances = .empty,
            .root_procs = .empty,
            .root_instances = .empty,
            .root_metadata = .empty,
            .solve_sessions = .empty,
            .proc_instances = .empty,
            .value_stores = .empty,
            .procedure_capture_sources = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.procedure_capture_sources.deinit(self.allocator);
        for (self.value_stores.items) |*store| {
            store.deinit();
        }
        self.value_stores.deinit(self.allocator);
        for (self.proc_instances.items) |*instance| {
            repr.deinitProcRepresentationInstance(self.allocator, instance);
        }
        self.proc_instances.deinit(self.allocator);
        for (self.solve_sessions.items) |*session| {
            session.deinit(self.allocator);
        }
        self.solve_sessions.deinit(self.allocator);
        self.root_metadata.deinit(self.allocator);
        self.root_instances.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.executable_synthetic_proc_instances.deinit(self.allocator);
        self.executable_synthetic_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.concrete_source_types.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

const ProcedureInstanceOwner = union(enum) {
    root: u32,
    direct_call: struct {
        caller: repr.ProcRepresentationInstanceId,
        call_site: repr.CallSiteInfoId,
    },
    proc_value: struct {
        owner: repr.ProcRepresentationInstanceId,
        value: repr.ValueInfoId,
        forced_target: ?ids.ProcValueExecutableTarget = null,
    },
    recursive_group_member: struct {
        anchor: repr.ProcRepresentationInstanceId,
    },
    executable_erased_adapter_member: struct {
        synthetic_index: u32,
        member_index: u32,
    },
    sealed_erased_adapter_member: u32,
    finite_erased_adapter_member: struct {
        emission_plan: repr.CallableValueEmissionPlanId,
        member_index: u32,
    },
    finite_erased_adapter_demand_member: struct {
        demand: repr.FiniteErasedAdapterDemandId,
        member_index: u32,
    },
};

fn procedureInstanceOwnerIsMaterialized(owner: ProcedureInstanceOwner) bool {
    return switch (owner) {
        .proc_value => false,
        .root,
        .direct_call,
        .recursive_group_member,
        .executable_erased_adapter_member,
        .sealed_erased_adapter_member,
        .finite_erased_adapter_member,
        .finite_erased_adapter_demand_member,
        => true,
    };
}

const ProcedureInstanceReservation = struct {
    proc: canonical.MirProcedureRef,
    solve_session: repr.RepresentationSolveSessionId,
    owner: ProcedureInstanceOwner,
    instance: repr.ProcRepresentationInstanceId,
};

const ProcSccInfo = struct {
    group: u32,
    recursive: bool,
};

const RecursiveGroupMemberReservation = struct {
    anchor: repr.ProcRepresentationInstanceId,
    proc: canonical.MirProcedureRef,
    instance: repr.ProcRepresentationInstanceId,
};

const MirProcedureRefIndexMap = std.HashMap(canonical.MirProcedureRef, u32, MirProcedureRefContext, std.hash_map.default_max_load_percentage);
const ProcedureCallableRefIndexMap = std.HashMap(canonical.ProcedureCallableRef, canonical.MirProcedureRef, ProcedureCallableRefContext, std.hash_map.default_max_load_percentage);

const MirProcedureRefContext = struct {
    pub fn hash(_: MirProcedureRefContext, key: canonical.MirProcedureRef) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hashMirProcedureRef(&hasher, key);
        return hasher.final();
    }

    pub fn eql(_: MirProcedureRefContext, a: canonical.MirProcedureRef, b: canonical.MirProcedureRef) bool {
        return canonical.mirProcedureRefEql(a, b);
    }
};

const ProcedureCallableRefContext = struct {
    pub fn hash(_: ProcedureCallableRefContext, key: canonical.ProcedureCallableRef) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hashProcedureCallableRef(&hasher, key);
        return hasher.final();
    }

    pub fn eql(_: ProcedureCallableRefContext, a: canonical.ProcedureCallableRef, b: canonical.ProcedureCallableRef) bool {
        return canonical.procedureCallableRefEql(a, b);
    }
};

fn buildProcIndexMap(
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
) Allocator.Error!MirProcedureRefIndexMap {
    var map = MirProcedureRefIndexMap.init(allocator);
    errdefer map.deinit();
    try map.ensureTotalCapacity(@intCast(input.procs.items.len));
    for (input.procs.items, 0..) |proc, raw_index| {
        const entry = try map.getOrPut(proc.proc);
        if (entry.found_existing) {
            lambdaInvariant("lambda-solved direct-call proc index map saw duplicate procedure identity");
        }
        entry.value_ptr.* = @intCast(raw_index);
    }
    return map;
}

fn buildProcCallableIndexMap(
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
) Allocator.Error!ProcedureCallableRefIndexMap {
    var map = ProcedureCallableRefIndexMap.init(allocator);
    errdefer map.deinit();
    try map.ensureTotalCapacity(@intCast(input.procs.items.len + input.executable_synthetic_procs.items.len));
    for (input.procs.items) |proc| {
        try putProcCallableIndex(&map, proc.proc.callable, proc.proc);
    }
    for (input.executable_synthetic_procs.items) |proc| {
        try putProcCallableIndex(&map, proc.source_proc.callable, proc.source_proc);
    }
    return map;
}

fn putProcCallableIndex(
    map: *ProcedureCallableRefIndexMap,
    callable: canonical.ProcedureCallableRef,
    proc: canonical.MirProcedureRef,
) Allocator.Error!void {
    const entry = try map.getOrPut(callable);
    if (entry.found_existing) {
        if (!canonical.mirProcedureRefEql(entry.value_ptr.*, proc)) {
            lambdaInvariant("lambda-solved callable procedure index saw one callable identity with multiple procedure identities");
        }
        return;
    }
    entry.value_ptr.* = proc;
}

fn buildExecutableSyntheticProcIndexMap(
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
) Allocator.Error!MirProcedureRefIndexMap {
    var map = MirProcedureRefIndexMap.init(allocator);
    errdefer map.deinit();
    try map.ensureTotalCapacity(@intCast(input.executable_synthetic_procs.items.len));
    for (input.executable_synthetic_procs.items, 0..) |proc, raw_index| {
        const entry = try map.getOrPut(proc.source_proc);
        if (entry.found_existing) {
            lambdaInvariant("lambda-solved executable synthetic proc index map saw duplicate procedure identity");
        }
        entry.value_ptr.* = @intCast(raw_index);
    }
    return map;
}

fn buildDirectCallSccInfo(
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
    proc_indices: *const MirProcedureRefIndexMap,
    executable_synthetic_indices: *const MirProcedureRefIndexMap,
) Allocator.Error![]ProcSccInfo {
    var adjacency: std.ArrayList(std.ArrayList(u32)) = .empty;
    defer {
        for (adjacency.items) |*edges| edges.deinit(allocator);
        adjacency.deinit(allocator);
    }

    for (input.procs.items) |_| {
        try adjacency.append(allocator, .empty);
    }

    for (input.procs.items, 0..) |proc, raw_index| {
        for (proc.direct_calls.get(input.direct_call_targets.items)) |target| {
            const target_index = proc_indices.get(target) orelse {
                if (executable_synthetic_indices.get(target) != null) continue;
                lambdaInvariant("lambda-solved direct-call metadata referenced missing procedure");
            };
            try adjacency.items[raw_index].append(allocator, target_index);
        }
    }

    const proc_count = input.procs.items.len;
    const infos = try allocator.alloc(ProcSccInfo, proc_count);
    errdefer allocator.free(infos);
    const indices = try allocator.alloc(i32, proc_count);
    defer allocator.free(indices);
    const lowlinks = try allocator.alloc(u32, proc_count);
    defer allocator.free(lowlinks);
    const on_stack = try allocator.alloc(bool, proc_count);
    defer allocator.free(on_stack);
    @memset(indices, -1);
    @memset(lowlinks, 0);
    @memset(on_stack, false);

    var tarjan = DirectCallTarjan{
        .allocator = allocator,
        .adjacency = adjacency.items,
        .infos = infos,
        .indices = indices,
        .lowlinks = lowlinks,
        .on_stack = on_stack,
        .stack = .empty,
    };
    defer tarjan.stack.deinit(allocator);

    for (0..proc_count) |raw_index| {
        if (tarjan.indices[raw_index] == -1) {
            try tarjan.strongConnect(@intCast(raw_index));
        }
    }
    return infos;
}

const DirectCallTarjan = struct {
    allocator: Allocator,
    adjacency: []const std.ArrayList(u32),
    infos: []ProcSccInfo,
    indices: []i32,
    lowlinks: []u32,
    on_stack: []bool,
    stack: std.ArrayList(u32),
    next_index: u32 = 0,
    next_group: u32 = 0,

    fn strongConnect(self: *DirectCallTarjan, node: u32) Allocator.Error!void {
        const node_index: usize = @intCast(node);
        self.indices[node_index] = @intCast(self.next_index);
        self.lowlinks[node_index] = self.next_index;
        self.next_index += 1;
        try self.stack.append(self.allocator, node);
        self.on_stack[node_index] = true;

        for (self.adjacency[node_index].items) |target| {
            const target_index: usize = @intCast(target);
            if (self.indices[target_index] == -1) {
                try self.strongConnect(target);
                self.lowlinks[node_index] = @min(self.lowlinks[node_index], self.lowlinks[target_index]);
            } else if (self.on_stack[target_index]) {
                self.lowlinks[node_index] = @min(self.lowlinks[node_index], @as(u32, @intCast(self.indices[target_index])));
            }
        }

        if (self.lowlinks[node_index] != @as(u32, @intCast(self.indices[node_index]))) return;

        var component: std.ArrayList(u32) = .empty;
        defer component.deinit(self.allocator);
        while (true) {
            const member = self.stack.pop() orelse {
                lambdaInvariant("lambda-solved direct-call SCC stack underflow");
            };
            self.on_stack[@intCast(member)] = false;
            try component.append(self.allocator, member);
            if (member == node) break;
        }

        const recursive = component.items.len > 1 or self.componentHasSelfLoop(component.items);
        for (component.items) |member| {
            self.infos[@intCast(member)] = .{
                .group = self.next_group,
                .recursive = recursive,
            };
        }
        self.next_group += 1;
    }

    fn componentHasSelfLoop(self: *const DirectCallTarjan, component: []const u32) bool {
        if (component.len != 1) return false;
        const member = component[0];
        for (self.adjacency[@intCast(member)].items) |target| {
            if (target == member) return true;
        }
        return false;
    }
};

fn hashMirProcedureRef(hasher: *std.hash.Wyhash, ref: canonical.MirProcedureRef) void {
    hashProcedureValueRef(hasher, ref.proc);
    hashProcedureCallableRef(hasher, ref.callable);
}

fn hashProcedureValueRef(hasher: *std.hash.Wyhash, ref: canonical.ProcedureValueRef) void {
    hasher.update(&ref.artifact.bytes);
    hashEnum(hasher, ref.proc_base);
}

fn hashProcedureTemplateRef(hasher: *std.hash.Wyhash, ref: canonical.ProcedureTemplateRef) void {
    hasher.update(&ref.artifact.bytes);
    hashEnum(hasher, ref.proc_base);
    hashEnum(hasher, ref.template);
}

fn hashMonoSpecializationKey(hasher: *std.hash.Wyhash, key: canonical.MonoSpecializationKey) void {
    hashProcedureTemplateRef(hasher, key.template);
    hasher.update(&key.requested_mono_fn_ty.bytes);
}

fn hashProcedureCallableRef(hasher: *std.hash.Wyhash, ref: canonical.ProcedureCallableRef) void {
    hashCallableProcedureTemplateRef(hasher, ref.template);
    hasher.update(&ref.source_fn_ty.bytes);
}

fn hashCallableProcedureTemplateRef(hasher: *std.hash.Wyhash, ref: canonical.CallableProcedureTemplateRef) void {
    switch (ref) {
        .checked => |checked| {
            hashByte(hasher, 0);
            hashProcedureTemplateRef(hasher, checked);
        },
        .lifted => |lifted| {
            hashByte(hasher, 1);
            hashMonoSpecializationKey(hasher, lifted.owner_mono_specialization);
            hashEnum(hasher, lifted.site);
        },
        .synthetic => |synthetic| {
            hashByte(hasher, 2);
            hashProcedureTemplateRef(hasher, synthetic.template);
        },
    }
}

fn hashEnum(hasher: *std.hash.Wyhash, value: anytype) void {
    const raw: u32 = @intFromEnum(value);
    hasher.update(std.mem.asBytes(&raw));
}

fn hashByte(hasher: *std.hash.Wyhash, value: u8) void {
    hasher.update(std.mem.asBytes(&value));
}

const ProcedureInstanceRegistry = struct {
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
    program: *Program,
    artifact_views: ArtifactViews,
    type_importer: *TypeImporter,
    records: *std.ArrayList(ProcBuildRecord),
    proc_indices: MirProcedureRefIndexMap,
    proc_callable_indices: ProcedureCallableRefIndexMap,
    executable_synthetic_indices: MirProcedureRefIndexMap,
    proc_sccs: []ProcSccInfo,
    reservations: std.ArrayList(ProcedureInstanceReservation),
    recursive_group_members: std.ArrayList(RecursiveGroupMemberReservation),
    sealed_erased_adapter_members: std.ArrayList(SealedErasedAdapterMemberReservation),
    pending: std.ArrayList(repr.ProcRepresentationInstanceId),
    active: std.ArrayList(repr.ProcRepresentationInstanceId),
    session_members: std.ArrayList(std.ArrayList(repr.ProcRepresentationInstanceId)),

    fn init(
        allocator: Allocator,
        input: *const Lifted.Lift.Program,
        program: *Program,
        artifact_views: ArtifactViews,
        type_importer: *TypeImporter,
        records: *std.ArrayList(ProcBuildRecord),
    ) Allocator.Error!ProcedureInstanceRegistry {
        var proc_indices = try buildProcIndexMap(allocator, input);
        errdefer proc_indices.deinit();
        var proc_callable_indices = try buildProcCallableIndexMap(allocator, input);
        errdefer proc_callable_indices.deinit();
        var executable_synthetic_indices = try buildExecutableSyntheticProcIndexMap(allocator, input);
        errdefer executable_synthetic_indices.deinit();
        const proc_sccs = try buildDirectCallSccInfo(allocator, input, &proc_indices, &executable_synthetic_indices);
        return .{
            .allocator = allocator,
            .input = input,
            .program = program,
            .artifact_views = artifact_views,
            .type_importer = type_importer,
            .records = records,
            .proc_indices = proc_indices,
            .proc_callable_indices = proc_callable_indices,
            .executable_synthetic_indices = executable_synthetic_indices,
            .proc_sccs = proc_sccs,
            .reservations = .empty,
            .recursive_group_members = .empty,
            .sealed_erased_adapter_members = .empty,
            .pending = .empty,
            .active = .empty,
            .session_members = .empty,
        };
    }

    fn deinit(self: *ProcedureInstanceRegistry) void {
        for (self.session_members.items) |*members| {
            members.deinit(self.allocator);
        }
        self.session_members.deinit(self.allocator);
        self.active.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        for (self.sealed_erased_adapter_members.items) |*member| {
            repr.deinitExecutableSpecializationKey(self.allocator, &member.target_key);
            if (member.provenance.len > 0) self.allocator.free(member.provenance);
        }
        self.sealed_erased_adapter_members.deinit(self.allocator);
        self.recursive_group_members.deinit(self.allocator);
        self.reservations.deinit(self.allocator);
        self.allocator.free(self.proc_sccs);
        self.executable_synthetic_indices.deinit();
        self.proc_callable_indices.deinit();
        self.proc_indices.deinit();
    }

    fn createSession(self: *ProcedureInstanceRegistry) Allocator.Error!repr.RepresentationSolveSessionId {
        const session_id: repr.RepresentationSolveSessionId = @enumFromInt(@as(u32, @intCast(self.program.solve_sessions.items.len)));
        try self.program.solve_sessions.append(self.allocator, .{
            .members = &.{},
            .representation_store = repr.RepresentationStore.init(self.allocator),
            .state = .building,
        });
        try self.session_members.append(self.allocator, .empty);
        return session_id;
    }

    fn finalizeSessions(self: *ProcedureInstanceRegistry) Allocator.Error!void {
        if (self.session_members.items.len != self.program.solve_sessions.items.len) {
            lambdaInvariant("lambda-solved procedure instance registry session count disagrees with program sessions");
        }
        for (self.session_members.items, 0..) |*members, raw_session| {
            const session = &self.program.solve_sessions.items[raw_session];
            if (session.members.len > 0) {
                self.allocator.free(session.members);
            }
            session.members = if (members.items.len == 0)
                &.{}
            else
                try self.allocator.dupe(repr.ProcRepresentationInstanceId, members.items);
        }
    }

    fn reserveRoot(
        self: *ProcedureInstanceRegistry,
        proc: canonical.MirProcedureRef,
        session_id: repr.RepresentationSolveSessionId,
        root_index: u32,
    ) Allocator.Error!repr.ProcRepresentationInstanceId {
        return try self.reserve(proc, session_id, .{ .root = root_index }, null);
    }

    fn reserveDirectCall(
        self: *ProcedureInstanceRegistry,
        caller: repr.ProcRepresentationInstanceId,
        call_site: repr.CallSiteInfoId,
        proc: canonical.MirProcedureRef,
    ) Allocator.Error!repr.ProcRepresentationInstanceId {
        const caller_record = self.procRecord(caller);
        if (self.recursiveGroupAnchorForCall(caller_record, proc)) |anchor| {
            return try self.reserveRecursiveGroupMember(anchor, caller_record.solve_session, proc);
        }
        if (self.activeInstanceForProc(caller_record.solve_session, proc)) |active| return active;
        return try self.reserve(proc, caller_record.solve_session, .{ .direct_call = .{
            .caller = caller,
            .call_site = call_site,
        } }, null);
    }

    fn reserveProcValue(
        self: *ProcedureInstanceRegistry,
        owner: repr.ProcRepresentationInstanceId,
        value: repr.ValueInfoId,
        proc: canonical.MirProcedureRef,
        forced_target: ?ids.ProcValueExecutableTarget,
    ) Allocator.Error!repr.ProcRepresentationInstanceId {
        const owner_record = self.procRecord(owner);
        if (forced_target == null) {
            if (self.activeInstanceForProc(owner_record.solve_session, proc)) |active| return active;
            if (self.existingProcValueInstance(owner_record.solve_session, proc)) |existing| return existing;
        }
        return try self.reserve(proc, owner_record.solve_session, .{ .proc_value = .{
            .owner = owner,
            .value = value,
            .forced_target = forced_target,
        } }, null);
    }

    fn reserve(
        self: *ProcedureInstanceRegistry,
        requested_proc: canonical.MirProcedureRef,
        session_id: repr.RepresentationSolveSessionId,
        owner: ProcedureInstanceOwner,
        recursive_group_anchor: ?repr.ProcRepresentationInstanceId,
    ) Allocator.Error!repr.ProcRepresentationInstanceId {
        const proc = self.canonicalProcedureForReservation(requested_proc);
        for (self.reservations.items) |reservation| {
            if (reservation.solve_session == session_id and
                canonical.mirProcedureRefEql(reservation.proc, proc) and
                procedureInstanceOwnerEql(reservation.owner, owner))
            {
                return reservation.instance;
            }
        }

        const kind: ProcBuildRecordKind = if (self.proc_indices.get(proc) != null)
            .normal
        else if (self.executable_synthetic_indices.get(proc)) |synthetic_index|
            .{ .executable_synthetic = synthetic_index }
        else {
            var same_template_count: usize = 0;
            var same_callable_count: usize = 0;
            var same_template_proc_base: u32 = 0;
            const same_template_requested_proc_base: u32 = @intFromEnum(proc.proc.proc_base);
            for (self.input.procs.items) |input_proc| {
                if (canonical.callableProcedureTemplateRefEql(input_proc.proc.callable.template, proc.callable.template)) {
                    same_template_count += 1;
                    same_template_proc_base = @intFromEnum(input_proc.proc.proc.proc_base);
                    if (canonical.procedureCallableRefEql(input_proc.proc.callable, proc.callable)) {
                        same_callable_count += 1;
                    }
                }
            }
            lambdaInvariantFmt(
                "lambda-solved procedure instance registry referenced missing procedure (callable_template={s}, input_procs={d}, synthetic_procs={d}, same_template_procs={d}, same_callable_procs={d}, requested_proc_base={d}, matching_proc_base={d})",
                .{
                    @tagName(std.meta.activeTag(proc.callable.template)),
                    self.input.procs.items.len,
                    self.input.executable_synthetic_procs.items.len,
                    same_template_count,
                    same_callable_count,
                    same_template_requested_proc_base,
                    same_template_proc_base,
                },
            );
        };
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(self.records.items.len)));
        const value_store_id: repr.ValueInfoStoreId = @enumFromInt(@as(u32, @intCast(self.program.value_stores.items.len)));
        try self.program.value_stores.append(self.allocator, repr.ValueInfoStore.init(self.allocator));
        const materialized = procedureInstanceOwnerIsMaterialized(owner);
        const anchor = recursive_group_anchor orelse switch (kind) {
            .normal => if (self.procIsRecursive(proc)) instance else null,
            .executable_synthetic => null,
        };
        try self.records.append(self.allocator, .{
            .proc = proc,
            .kind = kind,
            .representation_instance = instance,
            .solve_session = session_id,
            .recursive_group_anchor = anchor,
            .value_store = value_store_id,
            .owner = owner,
            .materialized = materialized,
        });
        try self.reservations.append(self.allocator, .{
            .proc = proc,
            .solve_session = session_id,
            .owner = owner,
            .instance = instance,
        });
        try self.session_members.items[@intFromEnum(session_id)].append(self.allocator, instance);
        if (anchor) |group_anchor| {
            try self.recursive_group_members.append(self.allocator, .{
                .anchor = group_anchor,
                .proc = proc,
                .instance = instance,
            });
        }
        switch (kind) {
            .normal => try self.pending.append(self.allocator, instance),
            .executable_synthetic => try self.buildExecutableSyntheticInstance(instance),
        }
        return instance;
    }

    fn canonicalProcedureForReservation(
        self: *ProcedureInstanceRegistry,
        requested_proc: canonical.MirProcedureRef,
    ) canonical.MirProcedureRef {
        if (self.proc_indices.get(requested_proc) != null) return requested_proc;
        if (self.executable_synthetic_indices.get(requested_proc) != null) return requested_proc;

        var found: ?canonical.MirProcedureRef = null;
        for (self.input.procs.items) |input_proc| {
            if (!canonical.procedureValueRefEql(input_proc.proc.proc, requested_proc.proc)) continue;
            if (!canonical.callableProcedureTemplateRefEql(input_proc.proc.callable.template, requested_proc.callable.template)) continue;
            if (found != null) {
                lambdaInvariant("lambda-solved procedure body identity matched multiple lifted procedures");
            }
            found = input_proc.proc;
        }
        return found orelse requested_proc;
    }

    fn buildPending(self: *ProcedureInstanceRegistry) Allocator.Error!void {
        var index: usize = 0;
        while (index < self.pending.items.len) : (index += 1) {
            try self.buildInstance(self.pending.items[index]);
        }
        for (self.records.items) |record| {
            if (!record.built) lambdaInvariant("lambda-solved procedure instance registry left an unbuilt procedure instance");
        }
    }

    fn materializeInstance(
        self: *ProcedureInstanceRegistry,
        instance: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!bool {
        const record = &self.records.items[@intFromEnum(instance)];
        if (record.materialized) return false;
        record.materialized = true;
        record.built = false;
        try self.pending.append(self.allocator, instance);
        return true;
    }

    fn materializeExecutableDemands(self: *ProcedureInstanceRegistry) Allocator.Error!bool {
        var changed = false;
        for (self.records.items) |*record| {
            const value_store = &self.program.value_stores.items[@intFromEnum(record.value_store)];
            const store = &self.program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;

            for (value_store.values.items) |value_info| {
                if (!value_store.valueSourceMatchBranchReachable(value_info)) continue;
                const callable = value_info.callable orelse continue;
                const emission = store.callableEmissionPlan(callable.emission_plan);
                switch (emission) {
                    .erase_proc_value => |erase| {
                        changed = (try self.materializeInstance(erase.target_instance)) or changed;
                    },
                    .pending_proc_value,
                    .finite,
                    .erase_finite_set,
                    .already_erased,
                    => {},
                }
            }

            for (value_store.call_sites.items) |call_site| {
                if (!value_store.callSiteSourceMatchBranchReachable(call_site)) continue;
                const callee = call_site.callee orelse continue;
                const callee_info = value_store.values.items[@intFromEnum(callee)];
                const callee_group = self.callableGroupForValueInfo(store, callee_info, callee_info.callable) orelse continue;
                if (callee_info.callable == null) {
                    for (store.callableGroupWorkMembers(callee_group)) |member| {
                        changed = (try self.materializeInstance(member.target_instance)) or changed;
                    }
                    continue;
                }
                const callable = callee_info.callable.?;
                const emission = store.callableEmissionPlan(callable.emission_plan);
                const finite_key = switch (emission) {
                    .finite => |key| key,
                    .pending_proc_value => {
                        for (store.callableGroupWorkMembers(callee_group)) |member| {
                            changed = (try self.materializeInstance(member.target_instance)) or changed;
                        }
                        continue;
                    },
                    else => continue,
                };
                const descriptor = store.callableSetDescriptor(finite_key) orelse {
                    lambdaInvariant("lambda-solved finite call-value materialization referenced missing callable-set descriptor");
                };
                for (descriptor.members) |member| {
                    changed = (try self.materializeInstance(member.target_instance)) or changed;
                }
            }
        }
        return changed;
    }

    fn callableGroupForValueInfo(
        self: *ProcedureInstanceRegistry,
        store: *const repr.RepresentationStore,
        value_info: repr.ValueInfo,
        callable: ?repr.CallableValueInfo,
    ) ?repr.RepresentationGroupId {
        if (callable) |info| return store.groupForRoot(info.callable_root);
        if (!self.valueHasFunctionType(value_info.logical_ty)) return null;
        const callable_root = store.solvedStructuralChildRoot(value_info.root, .function_callable) orelse value_info.root;
        return store.groupForRoot(callable_root);
    }

    fn valueHasFunctionType(
        self: *ProcedureInstanceRegistry,
        ty: Type.TypeVarId,
    ) bool {
        const root = self.program.types.unlinkConst(ty);
        return switch (self.program.types.getNode(root)) {
            .content => |content| switch (content) {
                .func => true,
                else => false,
            },
            else => false,
        };
    }

    fn reserveFiniteErasedAdapterMembers(self: *ProcedureInstanceRegistry) Allocator.Error!bool {
        var changed = false;
        for (self.program.solve_sessions.items, 0..) |*session, raw_session| {
            const session_id: repr.RepresentationSolveSessionId = @enumFromInt(@as(u32, @intCast(raw_session)));
            for (session.representation_store.callable_emission_plans, 0..) |plan, raw_plan| {
                const erase = switch (plan) {
                    .erase_finite_set => |erase| erase,
                    else => continue,
                };
                const descriptor = session.representation_store.callableSetDescriptor(erase.adapter.callable_set_key) orelse {
                    lambdaInvariant("lambda-solved finite erased adapter member reservation has no callable-set descriptor");
                };
                if (descriptor.members.len == 0) {
                    lambdaInvariant("lambda-solved finite erased adapter member reservation reached empty descriptor");
                }
                if (descriptor.members.len != erase.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter member target count differs from descriptor");
                }
                const emission_plan: repr.CallableValueEmissionPlanId = @enumFromInt(@as(u32, @intCast(raw_plan)));
                for (descriptor.members, erase.member_targets, 0..) |member, target_key, raw_member| {
                    validatePersistedFiniteAdapterMemberTarget(member, target_key);
                    if (self.existingFiniteErasedAdapterMember(session_id, member.source_proc, target_key) != null) continue;
                    const before = self.records.items.len;
                    _ = try self.reserve(member.source_proc, session_id, .{ .finite_erased_adapter_member = .{
                        .emission_plan = emission_plan,
                        .member_index = @intCast(raw_member),
                    } }, null);
                    if (self.records.items.len != before) changed = true;
                }
            }
            for (session.representation_store.finite_erased_adapter_demands, 0..) |demand, raw_demand| {
                const descriptor = session.representation_store.callableSetDescriptor(demand.adapter.callable_set_key) orelse {
                    lambdaInvariant("lambda-solved finite erased adapter demand member reservation has no callable-set descriptor");
                };
                if (descriptor.members.len == 0) {
                    lambdaInvariant("lambda-solved finite erased adapter demand member reservation reached empty descriptor");
                }
                if (descriptor.members.len != demand.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand target count differs from descriptor");
                }
                const demand_id: repr.FiniteErasedAdapterDemandId = @enumFromInt(@as(u32, @intCast(raw_demand)));
                for (descriptor.members, demand.member_targets, 0..) |member, target_key, raw_member| {
                    validatePersistedFiniteAdapterMemberTarget(member, target_key);
                    if (self.existingFiniteErasedAdapterMember(session_id, member.source_proc, target_key) != null) continue;
                    const before = self.records.items.len;
                    _ = try self.reserve(member.source_proc, session_id, .{ .finite_erased_adapter_demand_member = .{
                        .demand = demand_id,
                        .member_index = @intCast(raw_member),
                    } }, null);
                    if (self.records.items.len != before) changed = true;
                }
            }
        }
        return changed;
    }

    fn existingFiniteErasedAdapterMember(
        self: *const ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        proc: canonical.MirProcedureRef,
        target_key: repr.ExecutableSpecializationKey,
    ) ?repr.ProcRepresentationInstanceId {
        for (self.reservations.items) |reservation| {
            if (reservation.solve_session != session_id) continue;
            if (!canonical.mirProcedureRefEql(reservation.proc, proc)) continue;
            const existing_key = self.finiteErasedAdapterMemberTargetKey(reservation) orelse continue;
            if (repr.executableSpecializationKeyEql(existing_key, target_key)) return reservation.instance;
        }
        return null;
    }

    fn finiteErasedAdapterMemberTargetKey(
        self: *const ProcedureInstanceRegistry,
        reservation: ProcedureInstanceReservation,
    ) ?repr.ExecutableSpecializationKey {
        const member = switch (reservation.owner) {
            .finite_erased_adapter_member => |member| member,
            .finite_erased_adapter_demand_member => |member| {
                const session_index = @intFromEnum(reservation.solve_session);
                if (session_index >= self.program.solve_sessions.items.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand reservation referenced out-of-range session");
                }
                const store = &self.program.solve_sessions.items[session_index].representation_store;
                const demand = store.finiteErasedAdapterDemand(member.demand);
                if (member.member_index >= demand.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand reservation target index is out of range");
                }
                return demand.member_targets[member.member_index];
            },
            else => return null,
        };
        const session_index = @intFromEnum(reservation.solve_session);
        if (session_index >= self.program.solve_sessions.items.len) {
            lambdaInvariant("lambda-solved finite erased adapter member reservation referenced out-of-range session");
        }
        const store = &self.program.solve_sessions.items[session_index].representation_store;
        const plan = store.callableEmissionPlan(member.emission_plan);
        const erase = switch (plan) {
            .erase_finite_set => |erase| erase,
            else => lambdaInvariant("lambda-solved finite erased adapter member reservation referenced non-erased emission plan"),
        };
        if (member.member_index >= erase.member_targets.len) {
            lambdaInvariant("lambda-solved finite erased adapter member reservation target index is out of range");
        }
        return erase.member_targets[member.member_index];
    }

    fn buildInstance(
        self: *ProcedureInstanceRegistry,
        instance: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!void {
        const record = &self.records.items[@intFromEnum(instance)];
        if (record.built) return;
        switch (record.kind) {
            .normal => {},
            .executable_synthetic => lambdaInvariant("lambda-solved tried to body-lower an executable synthetic procedure"),
        }
        const input_proc = self.inputProc(record.proc);
        try self.active.append(self.allocator, instance);
        defer _ = self.active.pop();

        const session_index = @intFromEnum(record.solve_session);
        const value_store_index = @intFromEnum(record.value_store);
        var solver = BodySolver{
            .allocator = self.allocator,
            .input = &self.input.ast,
            .output = &self.program.ast,
            .canonical_names = &self.program.canonical_names,
            .row_shapes = &self.program.row_shapes,
            .symbols = &self.program.symbols,
            .type_importer = self.type_importer,
            .concrete_source_types = &self.program.concrete_source_types,
            .artifact_views = self.artifact_views,
            .representation_store = &self.program.solve_sessions.items[session_index].representation_store,
            .value_store = &self.program.value_stores.items[value_store_index],
            .env = std.AutoHashMap(Ast.Symbol, repr.BindingInfoId).init(self.allocator),
            .expr_map = std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId).init(self.allocator),
            .instance = instance,
            .registry = self,
        };
        defer solver.deinit();

        if (!record.materialized) {
            const roots = try solver.lowerDefPublicRoots(input_proc.body);
            const completed_record = &self.records.items[@intFromEnum(instance)];
            completed_record.public_roots = roots;
            completed_record.has_public_roots = true;
            completed_record.built = true;
            return;
        }

        solver.existing_public_roots = if (record.has_public_roots) record.public_roots else null;
        const body = try solver.lowerDef(input_proc.body);
        const roots = solver.public_roots orelse lambdaInvariant("lambda-solved MIR built a procedure without public roots");
        const lowered_record = self.procRecord(instance);
        try solver.appendExecutableDependencyRequirements(lowered_record.owner, roots);
        const completed_record = &self.records.items[@intFromEnum(instance)];
        completed_record.body = body;
        completed_record.public_roots = roots;
        completed_record.has_public_roots = true;
        completed_record.built = true;
    }

    fn buildExecutableSyntheticInstance(
        self: *ProcedureInstanceRegistry,
        instance: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!void {
        const record = &self.records.items[@intFromEnum(instance)];
        if (record.built) return;
        const synthetic_index = switch (record.kind) {
            .normal => lambdaInvariant("lambda-solved tried to signature-lower a normal procedure"),
            .executable_synthetic => |index| index,
        };
        const synthetic = self.input.executable_synthetic_procs.items[synthetic_index];
        const session_index = @intFromEnum(record.solve_session);
        const value_store_index = @intFromEnum(record.value_store);
        var solver = BodySolver{
            .allocator = self.allocator,
            .input = &self.input.ast,
            .output = &self.program.ast,
            .canonical_names = &self.program.canonical_names,
            .row_shapes = &self.program.row_shapes,
            .symbols = &self.program.symbols,
            .type_importer = self.type_importer,
            .concrete_source_types = &self.program.concrete_source_types,
            .artifact_views = self.artifact_views,
            .representation_store = &self.program.solve_sessions.items[session_index].representation_store,
            .value_store = &self.program.value_stores.items[value_store_index],
            .env = std.AutoHashMap(Ast.Symbol, repr.BindingInfoId).init(self.allocator),
            .expr_map = std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId).init(self.allocator),
            .instance = instance,
            .registry = self,
        };
        defer solver.deinit();

        record.public_roots = try solver.lowerExecutableSyntheticSignature(synthetic);
        try self.reserveExecutableSyntheticCodeDependencies(record.solve_session, synthetic_index, synthetic);
        record.has_public_roots = true;
        record.built = true;
    }

    fn reserveExecutableSyntheticCodeDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        synthetic_index: u32,
        synthetic: ids.ExecutableSyntheticProc,
    ) Allocator.Error!void {
        switch (synthetic.body) {
            .erased_promoted_wrapper => |erased| switch (erased.sealed.code) {
                .direct_proc => {},
                .finite_adapter => |finite| {
                    if (finite.members.len == 0) {
                        lambdaInvariant("lambda-solved executable synthetic finite adapter has no member targets");
                    }
                    const descriptors = callableSetDescriptorsForArtifactViews(self.artifact_views, synthetic.artifact) orelse {
                        lambdaInvariant("lambda-solved executable synthetic finite adapter artifact descriptors are unavailable");
                    };
                    const descriptor = descriptors.descriptorFor(finite.adapter_key.callable_set_key) orelse {
                        lambdaInvariant("lambda-solved executable synthetic finite adapter descriptor is unavailable");
                    };
                    if (descriptor.members.len != finite.members.len) {
                        lambdaInvariant("lambda-solved executable synthetic finite adapter member target count differs from descriptor");
                    }
                    for (descriptor.members, finite.members, 0..) |descriptor_member, sealed_member, member_index| {
                        validatePersistedFiniteAdapterMemberTarget(descriptor_member, sealed_member.target_key);
                        const source_proc = try self.type_importer.name_resolver.mirProcedureRef(sealed_member.source_proc);
                        _ = try self.reserve(source_proc, session_id, .{ .executable_erased_adapter_member = .{
                            .synthetic_index = synthetic_index,
                            .member_index = @intCast(member_index),
                        } }, null);
                    }
                },
            },
        }
    }

    fn reserveSealedErasedCallableValueDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        sealed: checked_artifact.SealedErasedCallableValue,
    ) Allocator.Error!void {
        switch (sealed.code) {
            .direct_proc => {},
            .finite_adapter => |finite| {
                if (finite.members.len == 0) {
                    lambdaInvariant("lambda-solved const-backed sealed finite adapter had no member targets");
                }
                const descriptors = callableSetDescriptorsForArtifactViews(self.artifact_views, owner) orelse {
                    lambdaInvariant("lambda-solved const-backed sealed finite adapter artifact descriptors are unavailable");
                };
                const descriptor = descriptors.descriptorFor(finite.adapter_key.callable_set_key) orelse {
                    lambdaInvariant("lambda-solved const-backed sealed finite adapter descriptor is unavailable");
                };
                if (descriptor.members.len != finite.members.len) {
                    lambdaInvariant("lambda-solved const-backed sealed finite adapter member target count differs from descriptor");
                }
                const payloads = executablePayloadsForArtifactViews(self.artifact_views, owner) orelse {
                    lambdaInvariant("lambda-solved const-backed sealed finite adapter executable payloads are unavailable");
                };
                for (descriptor.members, finite.members) |descriptor_member, sealed_member| {
                    validatePersistedFiniteAdapterMemberTarget(descriptor_member, sealed_member.target_key);
                    const source_proc = try self.type_importer.name_resolver.mirProcedureRef(sealed_member.source_proc);
                    _ = try self.reserveSealedErasedAdapterMember(
                        session_id,
                        owner,
                        payloads,
                        source_proc,
                        sealed_member.target_key,
                        sealed.boundary.provenance,
                    );
                }
            },
        }
    }

    fn reserveSealedErasedAdapterMember(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        payloads: *const checked_artifact.ExecutableTypePayloadStore,
        source_proc: canonical.MirProcedureRef,
        target_key: repr.ExecutableSpecializationKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.ProcRepresentationInstanceId {
        if (provenance.len == 0) {
            lambdaInvariant("lambda-solved const-backed sealed adapter member had no Box(T) provenance");
        }
        for (self.reservations.items) |reservation| {
            if (reservation.solve_session != session_id) continue;
            if (!canonical.mirProcedureRefEql(reservation.proc, source_proc)) continue;
            const dep_index = switch (reservation.owner) {
                .sealed_erased_adapter_member => |index| index,
                else => continue,
            };
            const existing = self.sealed_erased_adapter_members.items[dep_index];
            if (std.meta.eql(existing.artifact.bytes, owner.bytes) and
                repr.executableSpecializationKeyEql(existing.target_key, target_key))
            {
                return reservation.instance;
            }
        }

        var owned_key = try repr.cloneExecutableSpecializationKey(self.allocator, target_key);
        errdefer repr.deinitExecutableSpecializationKey(self.allocator, &owned_key);
        const owned_provenance = try self.allocator.dupe(repr.BoxErasureProvenance, provenance);
        errdefer self.allocator.free(owned_provenance);
        const dependency_index: u32 = @intCast(self.sealed_erased_adapter_members.items.len);
        try self.sealed_erased_adapter_members.append(self.allocator, .{
            .artifact = owner,
            .payloads = payloads,
            .target_key = owned_key,
            .provenance = owned_provenance,
        });
        owned_key = undefined;
        return try self.reserve(source_proc, session_id, .{ .sealed_erased_adapter_member = dependency_index }, null);
    }

    fn reserveProvidedDataExportDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
    ) Allocator.Error!void {
        var seen_values = std.AutoHashMap(ProvidedDataValueVisitKey, void).init(self.allocator);
        defer seen_values.deinit();
        var seen_nodes = std.AutoHashMap(ProvidedDataMaterializationNodeVisitKey, void).init(self.allocator);
        defer seen_nodes.deinit();
        var seen_const_instances = std.AutoHashMap(ProvidedDataConstInstanceVisitKey, void).init(self.allocator);
        defer seen_const_instances.deinit();

        const materialization = ConstMaterializationView{
            .owner = self.artifact_views.root.artifact.key,
            .names = &self.artifact_views.root.artifact.canonical_names,
            .values = &self.artifact_views.root.artifact.comptime_values,
        };
        for (self.artifact_views.root.artifact.provided_exports.exports) |provided| {
            const data = switch (provided) {
                .procedure => continue,
                .data => |data| data,
            };
            const binding = self.artifact_views.root.artifact.comptime_values.lookupBinding(data.pattern) orelse {
                lambdaInvariant("lambda-solved provided data export dependency had no published compile-time value");
            };
            try self.reserveComptimeValueMaterializationDependencies(
                session_id,
                materialization,
                binding.schema,
                binding.value,
                &seen_values,
                &seen_nodes,
                &seen_const_instances,
            );
        }
    }

    fn reserveComptimeValueMaterializationDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        materialization: ConstMaterializationView,
        schema_id: checked_artifact.ComptimeSchemaId,
        value_id: checked_artifact.ComptimeValueId,
        seen_values: *std.AutoHashMap(ProvidedDataValueVisitKey, void),
        seen_nodes: *std.AutoHashMap(ProvidedDataMaterializationNodeVisitKey, void),
        seen_const_instances: *std.AutoHashMap(ProvidedDataConstInstanceVisitKey, void),
    ) Allocator.Error!void {
        const visit = try seen_values.getOrPut(.{
            .owner = materialization.owner.bytes,
            .schema = schema_id,
            .value = value_id,
        });
        if (visit.found_existing) return;

        const schema = constSchemaForMaterialization(materialization, schema_id);
        const value = constValueForMaterialization(materialization, value_id);
        switch (schema) {
            .pending => lambdaInvariant("lambda-solved provided data export dependency reached pending compile-time schema"),
            .zst,
            .int,
            .frac,
            .str,
            => {},
            .list => |elem_schema| {
                const items = switch (value) {
                    .list => |items| items,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached list schema/value mismatch"),
                };
                for (items) |item| {
                    try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, elem_schema, item, seen_values, seen_nodes, seen_const_instances);
                }
            },
            .box => |payload_schema| {
                const payload = switch (value) {
                    .box => |payload| payload,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached Box(T) schema/value mismatch"),
                };
                try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, payload_schema, payload, seen_values, seen_nodes, seen_const_instances);
            },
            .tuple => |schemas| {
                const items = switch (value) {
                    .tuple => |items| items,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached tuple schema/value mismatch"),
                };
                if (schemas.len != items.len) lambdaInvariant("lambda-solved provided data export dependency reached tuple arity mismatch");
                for (schemas, items) |item_schema, item| {
                    try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, item_schema, item, seen_values, seen_nodes, seen_const_instances);
                }
            },
            .record => |fields| {
                const values = switch (value) {
                    .record => |values| values,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached record schema/value mismatch"),
                };
                if (fields.len != values.len) lambdaInvariant("lambda-solved provided data export dependency reached record field count mismatch");
                for (fields, values) |field, field_value| {
                    try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, field.schema, field_value, seen_values, seen_nodes, seen_const_instances);
                }
            },
            .tag_union => |variants| {
                const tag = switch (value) {
                    .tag_union => |tag| tag,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached tag-union schema/value mismatch"),
                };
                const index: usize = @intCast(tag.variant_index);
                if (index >= variants.len) lambdaInvariant("lambda-solved provided data export dependency reached tag index outside schema");
                const variant = variants[index];
                if (variant.payloads.len != tag.payloads.len) lambdaInvariant("lambda-solved provided data export dependency reached tag payload count mismatch");
                for (variant.payloads, tag.payloads) |payload_schema, payload| {
                    try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, payload_schema, payload, seen_values, seen_nodes, seen_const_instances);
                }
            },
            .alias => |alias| {
                const backing = switch (value) {
                    .alias => |backing| backing,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached alias schema/value mismatch"),
                };
                try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, alias.backing, backing, seen_values, seen_nodes, seen_const_instances);
            },
            .nominal => |nominal| {
                const backing = switch (value) {
                    .nominal => |backing| backing,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached nominal schema/value mismatch"),
                };
                try self.reserveComptimeValueMaterializationDependencies(session_id, materialization, nominal.backing, backing, seen_values, seen_nodes, seen_const_instances);
            },
            .callable => {
                const leaf = switch (value) {
                    .callable => |leaf| leaf,
                    else => lambdaInvariant("lambda-solved provided data export dependency reached callable schema/value mismatch"),
                };
                try self.reserveCallableLeafMaterializationDependencies(
                    session_id,
                    materialization.owner,
                    leaf,
                    seen_values,
                    seen_nodes,
                    seen_const_instances,
                );
            },
        }
    }

    fn reserveCallableLeafMaterializationDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        leaf: checked_artifact.CallableLeafInstance,
        seen_values: *std.AutoHashMap(ProvidedDataValueVisitKey, void),
        seen_nodes: *std.AutoHashMap(ProvidedDataMaterializationNodeVisitKey, void),
        seen_const_instances: *std.AutoHashMap(ProvidedDataConstInstanceVisitKey, void),
    ) Allocator.Error!void {
        switch (leaf) {
            .finite => {},
            .erased_boxed => |erased| {
                try self.reserveSealedErasedCallableValueDependencies(session_id, owner, erased.sealed);
                try self.reserveErasedCaptureMaterializationDependencies(
                    session_id,
                    owner,
                    erased.sealed.capture,
                    seen_values,
                    seen_nodes,
                    seen_const_instances,
                );
            },
        }
    }

    fn reserveErasedCaptureMaterializationDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        capture: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
        seen_values: *std.AutoHashMap(ProvidedDataValueVisitKey, void),
        seen_nodes: *std.AutoHashMap(ProvidedDataMaterializationNodeVisitKey, void),
        seen_const_instances: *std.AutoHashMap(ProvidedDataConstInstanceVisitKey, void),
    ) Allocator.Error!void {
        switch (capture) {
            .none,
            .zero_sized_typed,
            => {},
            .node => |node| try self.reserveErasedCaptureMaterializationNodeDependencies(
                session_id,
                owner,
                node,
                seen_values,
                seen_nodes,
                seen_const_instances,
            ),
        }
    }

    fn reserveErasedCaptureMaterializationNodeDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        node_id: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
        seen_values: *std.AutoHashMap(ProvidedDataValueVisitKey, void),
        seen_nodes: *std.AutoHashMap(ProvidedDataMaterializationNodeVisitKey, void),
        seen_const_instances: *std.AutoHashMap(ProvidedDataConstInstanceVisitKey, void),
    ) Allocator.Error!void {
        const visit = try seen_nodes.getOrPut(.{
            .owner = owner.bytes,
            .node = node_id,
        });
        if (visit.found_existing) return;

        const plans = compileTimePlansForArtifactViews(self.artifact_views, owner) orelse {
            lambdaInvariant("lambda-solved provided data export dependency referenced unavailable materialization plan owner");
        };
        const node = plans.erasedCaptureExecutableMaterializationNode(node_id);
        switch (node) {
            .pending => lambdaInvariant("lambda-solved provided data export dependency reached pending erased capture materialization node"),
            .pure_const,
            .pure_value,
            => {},
            .const_instance => |const_instance| try self.reserveConstInstanceMaterializationDependencies(
                session_id,
                const_instance,
                seen_values,
                seen_nodes,
                seen_const_instances,
            ),
            .finite_callable_set => |finite| for (finite.captures) |nested| {
                try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, nested, seen_values, seen_nodes, seen_const_instances);
            },
            .erased_callable => |erased| {
                try self.reserveSealedErasedCallableValueDependencies(session_id, owner, erased.sealed);
                try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, erased.sealed.capture, seen_values, seen_nodes, seen_const_instances);
            },
            .record => |fields| for (fields) |field| {
                try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, field.value, seen_values, seen_nodes, seen_const_instances);
            },
            .tuple => |items| for (items) |item| {
                try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, item, seen_values, seen_nodes, seen_const_instances);
            },
            .tag_union => |tag| for (tag.payloads) |payload| {
                try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, payload.value, seen_values, seen_nodes, seen_const_instances);
            },
            .list => |items| for (items) |item| {
                try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, item, seen_values, seen_nodes, seen_const_instances);
            },
            .box => |payload| try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, payload, seen_values, seen_nodes, seen_const_instances),
            .nominal => |nominal| try self.reserveErasedCaptureMaterializationDependencies(session_id, owner, nominal.backing, seen_values, seen_nodes, seen_const_instances),
            .recursive_ref => |ref| try self.reserveErasedCaptureMaterializationNodeDependencies(session_id, owner, ref, seen_values, seen_nodes, seen_const_instances),
        }
    }

    fn reserveConstInstanceMaterializationDependencies(
        self: *ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        ref: checked_artifact.ConstInstanceRef,
        seen_values: *std.AutoHashMap(ProvidedDataValueVisitKey, void),
        seen_nodes: *std.AutoHashMap(ProvidedDataMaterializationNodeVisitKey, void),
        seen_const_instances: *std.AutoHashMap(ProvidedDataConstInstanceVisitKey, void),
    ) Allocator.Error!void {
        const visit = try seen_const_instances.getOrPut(.{
            .owner = ref.owner.bytes,
            .instance = ref.instance,
        });
        if (visit.found_existing) return;

        const resolved = resolveConstInstanceForArtifactViews(self.artifact_views, ref);
        try self.reserveComptimeValueMaterializationDependencies(
            session_id,
            resolved.materialization,
            resolved.instance.schema,
            resolved.instance.value,
            seen_values,
            seen_nodes,
            seen_const_instances,
        );
    }

    fn recursiveGroupAnchorForCall(
        self: *const ProcedureInstanceRegistry,
        caller_record: *const ProcBuildRecord,
        target_proc: canonical.MirProcedureRef,
    ) ?repr.ProcRepresentationInstanceId {
        const anchor = caller_record.recursive_group_anchor orelse return null;
        if (!self.sameRecursiveDirectCallScc(caller_record.proc, target_proc)) return null;
        return anchor;
    }

    fn reserveRecursiveGroupMember(
        self: *ProcedureInstanceRegistry,
        anchor: repr.ProcRepresentationInstanceId,
        session_id: repr.RepresentationSolveSessionId,
        proc: canonical.MirProcedureRef,
    ) Allocator.Error!repr.ProcRepresentationInstanceId {
        for (self.recursive_group_members.items) |member| {
            if (member.anchor == anchor and canonical.mirProcedureRefEql(member.proc, proc)) {
                return member.instance;
            }
        }
        return try self.reserve(proc, session_id, .{ .recursive_group_member = .{ .anchor = anchor } }, anchor);
    }

    fn activeInstanceForProc(
        self: *const ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        proc: canonical.MirProcedureRef,
    ) ?repr.ProcRepresentationInstanceId {
        var i = self.active.items.len;
        while (i > 0) {
            i -= 1;
            const instance = self.active.items[i];
            const active_record = self.procRecord(instance);
            if (active_record.solve_session == session_id and canonical.mirProcedureRefEql(active_record.proc, proc)) {
                return instance;
            }
        }
        return null;
    }

    fn procIsRecursive(self: *const ProcedureInstanceRegistry, proc: canonical.MirProcedureRef) bool {
        const index = self.inputProcIndex(proc);
        return self.proc_sccs[index].recursive;
    }

    fn sameRecursiveDirectCallScc(
        self: *const ProcedureInstanceRegistry,
        a: canonical.MirProcedureRef,
        b: canonical.MirProcedureRef,
    ) bool {
        const a_index = self.proc_indices.get(a) orelse return false;
        const b_index = self.proc_indices.get(b) orelse return false;
        const a_info = self.proc_sccs[a_index];
        if (!a_info.recursive) return false;
        const b_info = self.proc_sccs[b_index];
        return b_info.recursive and a_info.group == b_info.group;
    }

    fn existingProcValueInstance(
        self: *const ProcedureInstanceRegistry,
        session_id: repr.RepresentationSolveSessionId,
        proc: canonical.MirProcedureRef,
    ) ?repr.ProcRepresentationInstanceId {
        for (self.reservations.items) |reservation| {
            if (reservation.solve_session != session_id) continue;
            if (!canonical.mirProcedureRefEql(reservation.proc, proc)) continue;
            switch (reservation.owner) {
                .proc_value => return reservation.instance,
                else => {},
            }
        }
        return null;
    }

    fn inputProcIndex(
        self: *const ProcedureInstanceRegistry,
        proc: canonical.MirProcedureRef,
    ) usize {
        if (self.proc_indices.get(proc)) |index| return index;
        lambdaInvariant("lambda-solved procedure instance registry referenced missing lifted procedure");
    }

    fn procRecord(
        self: *const ProcedureInstanceRegistry,
        instance: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(instance);
        if (index >= self.records.items.len) {
            lambdaInvariant("lambda-solved procedure instance registry referenced out-of-range instance");
        }
        return &self.records.items[index];
    }

    fn inputProc(
        self: *const ProcedureInstanceRegistry,
        proc: canonical.MirProcedureRef,
    ) Lifted.Lift.Proc {
        return self.input.procs.items[self.inputProcIndex(proc)];
    }

    fn procForCallable(
        self: *const ProcedureInstanceRegistry,
        callable: canonical.ProcedureCallableRef,
    ) ?canonical.MirProcedureRef {
        return self.proc_callable_indices.get(callable);
    }
};

fn procedureInstanceOwnerEql(a: ProcedureInstanceOwner, b: ProcedureInstanceOwner) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    return switch (a) {
        .root => |a_root| switch (b) {
            .root => |b_root| a_root == b_root,
            else => false,
        },
        .direct_call => |a_call| switch (b) {
            .direct_call => |b_call| a_call.caller == b_call.caller and a_call.call_site == b_call.call_site,
            else => false,
        },
        .proc_value => |a_value| switch (b) {
            .proc_value => |b_value| a_value.owner == b_value.owner and
                a_value.value == b_value.value and
                procValueExecutableTargetEql(a_value.forced_target, b_value.forced_target),
            else => false,
        },
        .recursive_group_member => |a_member| switch (b) {
            .recursive_group_member => |b_member| a_member.anchor == b_member.anchor,
            else => false,
        },
        .executable_erased_adapter_member => |a_member| switch (b) {
            .executable_erased_adapter_member => |b_member| a_member.synthetic_index == b_member.synthetic_index and
                a_member.member_index == b_member.member_index,
            else => false,
        },
        .sealed_erased_adapter_member => |a_member| switch (b) {
            .sealed_erased_adapter_member => |b_member| a_member == b_member,
            else => false,
        },
        .finite_erased_adapter_member => |a_member| switch (b) {
            .finite_erased_adapter_member => |b_member| a_member.emission_plan == b_member.emission_plan and
                a_member.member_index == b_member.member_index,
            else => false,
        },
        .finite_erased_adapter_demand_member => |a_member| switch (b) {
            .finite_erased_adapter_demand_member => |b_member| a_member.demand == b_member.demand and
                a_member.member_index == b_member.member_index,
            else => false,
        },
    };
}

fn procValueExecutableTargetEql(
    a: ?ids.ProcValueExecutableTarget,
    b: ?ids.ProcValueExecutableTarget,
) bool {
    if (a == null or b == null) return a == null and b == null;
    const left = a.?;
    const right = b.?;
    if (!repr.executableSpecializationKeyEql(left.key, right.key)) return false;
    if (!std.mem.eql(u8, &left.artifact.bytes, &right.artifact.bytes)) return false;
    if ((left.promoted_wrapper == null) != (right.promoted_wrapper == null)) return false;
    if (left.promoted_wrapper) |left_wrapper| {
        if (!canonical.mirProcedureRefEql(left_wrapper, right.promoted_wrapper.?)) return false;
    }
    return true;
}

fn constBackedValueInfoEql(a: repr.ConstBackedValueInfo, b: repr.ConstBackedValueInfo) bool {
    return artifactKeyEql(a.const_instance.owner, b.const_instance.owner) and
        checked_artifact.constInstantiationKeyEql(a.const_instance.key, b.const_instance.key) and
        a.const_instance.instance == b.const_instance.instance and
        a.schema == b.schema and
        a.value == b.value;
}

fn validatePersistedFiniteAdapterMemberTarget(
    member: anytype,
    target: canonical.ExecutableSpecializationKey,
) void {
    if (member.source_proc.proc.proc_base != target.base) {
        lambdaInvariant("lambda-solved persisted finite-set adapter member target base differs from descriptor member");
    }
    if (!repr.canonicalTypeKeyEql(member.proc_value.source_fn_ty, target.requested_fn_ty)) {
        lambdaInvariant("lambda-solved persisted finite-set adapter member target source type differs from procedure value");
    }
}

fn finiteErasedAdapterMemberTargetsForAbi(
    allocator: Allocator,
    members: []const repr.CanonicalCallableSetMember,
    abi: *const repr.ErasedFnAbi,
) Allocator.Error![]const repr.ExecutableSpecializationKey {
    if (members.len == 0) lambdaInvariant("lambda-solved finite erased adapter target publication reached empty descriptor");
    const out = try allocator.alloc(repr.ExecutableSpecializationKey, members.len);
    var initialized: usize = 0;
    errdefer {
        for (out[0..initialized]) |*key| repr.deinitExecutableSpecializationKey(allocator, key);
        allocator.free(out);
    }
    for (members, 0..) |member, i| {
        out[i] = .{
            .base = member.source_proc.proc.proc_base,
            .requested_fn_ty = member.proc_value.source_fn_ty,
            .exec_arg_tys = if (abi.arg_exec_keys.len == 0)
                &.{}
            else
                try allocator.dupe(repr.CanonicalExecValueTypeKey, abi.arg_exec_keys),
            .exec_ret_ty = abi.ret_exec_key,
            .callable_repr_mode = .direct,
            .capture_shape_key = member.capture_shape_key,
        };
        initialized += 1;
    }
    return out;
}

fn cloneExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const repr.ExecutableSpecializationKey,
) Allocator.Error![]const repr.ExecutableSpecializationKey {
    if (keys.len == 0) return &.{};
    const out = try allocator.alloc(repr.ExecutableSpecializationKey, keys.len);
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

fn cloneExecValueTypeKeySlice(
    allocator: Allocator,
    keys: []const repr.CanonicalExecValueTypeKey,
) Allocator.Error![]const repr.CanonicalExecValueTypeKey {
    if (keys.len == 0) return &.{};
    return try allocator.dupe(repr.CanonicalExecValueTypeKey, keys);
}

fn deinitExecutableSpecializationKeySlice(
    allocator: Allocator,
    keys: []const repr.ExecutableSpecializationKey,
) void {
    for (keys) |target| {
        var key = target;
        repr.deinitExecutableSpecializationKey(allocator, &key);
    }
    if (keys.len > 0) allocator.free(keys);
}

fn deinitLocalFiniteSetErasePlan(allocator: Allocator, plan: *repr.FiniteSetErasePlan) void {
    deinitExecutableSpecializationKeySlice(allocator, plan.member_targets);
    deinitLocalFiniteSetEraseBranches(allocator, plan.branches);
    if (plan.provenance.len > 0) allocator.free(plan.provenance);
    plan.* = undefined;
}

fn deinitLocalFiniteSetEraseBranches(
    allocator: Allocator,
    branches: []const repr.FiniteSetEraseAdapterBranchPlan,
) void {
    for (branches) |branch| {
        if (branch.arg_transforms.len > 0) allocator.free(branch.arg_transforms);
        if (branch.capture_transforms.len > 0) allocator.free(branch.capture_transforms);
    }
    if (branches.len > 0) allocator.free(branches);
}

fn cloneLocalFiniteSetEraseBranches(
    allocator: Allocator,
    branches: []const repr.FiniteSetEraseAdapterBranchPlan,
) Allocator.Error![]const repr.FiniteSetEraseAdapterBranchPlan {
    if (branches.len == 0) return &.{};
    const cloned = try allocator.alloc(repr.FiniteSetEraseAdapterBranchPlan, branches.len);
    @memset(cloned, .{
        .member = .{
            .callable_set_key = .{ .bytes = [_]u8{0} ** 32 },
            .member_index = undefined,
        },
        .target_instance = undefined,
        .arg_transforms = &.{},
        .capture_transforms = &.{},
        .result_transform = null,
    });
    errdefer deinitLocalFiniteSetEraseBranches(allocator, cloned);
    for (branches, 0..) |branch, i| {
        cloned[i] = .{
            .member = branch.member,
            .target_instance = branch.target_instance,
            .arg_transforms = if (branch.arg_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(repr.ValueTransformBoundaryId, branch.arg_transforms),
            .capture_transforms = if (branch.capture_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(repr.ValueTransformBoundaryId, branch.capture_transforms),
            .result_transform = branch.result_transform,
        };
    }
    return cloned;
}

/// Public `run` function.
pub fn run(
    allocator: Allocator,
    lifted: Lifted.Lift.Program,
    artifact_views: ArtifactViews,
) Allocator.Error!Program {
    var input = lifted;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.canonical_names;
    input.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.concrete_source_types = input.concrete_source_types;
    input.concrete_source_types = ConcreteSourceType.Store.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.solve_sessions.ensureTotalCapacity(allocator, input.root_procs.items.len);
    try program.proc_instances.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.value_stores.ensureTotalCapacity(allocator, input.procs.items.len);
    var proc_build_records = std.ArrayList(ProcBuildRecord).empty;
    defer proc_build_records.deinit(allocator);
    try proc_build_records.ensureTotalCapacity(allocator, input.procs.items.len);

    var name_resolver = ArtifactNames.ArtifactNameResolver.init(
        &program.canonical_names,
        artifact_views.root.artifact,
        artifact_views.imports,
        artifact_views.root.relation_artifacts,
    );
    var type_importer = TypeImporter.init(
        allocator,
        &input.types,
        &program.types,
        &program.concrete_source_types,
        &name_resolver,
        artifact_views,
        true,
    );
    defer type_importer.deinit();

    var registry = try ProcedureInstanceRegistry.init(allocator, &input, &program, artifact_views, &type_importer, &proc_build_records);
    defer registry.deinit();
    for (input.root_procs.items, 0..) |root_proc, raw_root| {
        const session_id = try registry.createSession();
        const root_instance = try registry.reserveRoot(root_proc, session_id, @intCast(raw_root));
        try program.root_instances.append(allocator, root_instance);
    }
    if (input.root_procs.items.len == 0) {
        const session_id = try registry.createSession();
        for (input.procs.items, 0..) |proc, raw_proc| {
            _ = try registry.reserveRoot(proc.proc, session_id, @intCast(raw_proc));
        }
    }
    if (artifact_views.collect_provided_data_exports) {
        const session_id = try registry.createSession();
        try registry.reserveProvidedDataExportDependencies(session_id);
    }
    try registry.buildPending();
    try registry.finalizeSessions();
    try program.executable_synthetic_procs.appendSlice(allocator, input.executable_synthetic_procs.items);
    _ = try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items, true);
    while (true) {
        try solveRepresentationSessions(&program, proc_build_records.items);
        try propagatePendingComptimeDependencyOrigins(&program, proc_build_records.items);
        try assignCallableEmissionPlans(&program, proc_build_records.items, artifact_views, .allow_pending_call_values);
        const proc_value_requirements_changed = try appendProcValueOwnerErasureRequirements(&program, proc_build_records.items);
        const call_boundary_requirements_changed = try appendCallBoundaryErasureRequirements(&program, proc_build_records.items);
        const adapter_demands_changed = try publishValueTransformAdapterDemandsFromSolvedFlow(&program, proc_build_records.items);
        if (proc_value_requirements_changed or call_boundary_requirements_changed or adapter_demands_changed) {
            continue;
        }
        const finite_members_changed = try registry.reserveFiniteErasedAdapterMembers();
        if (finite_members_changed) {
            try registry.buildPending();
            try registry.finalizeSessions();
            _ = try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items, true);
            continue;
        }
        const executable_demands_changed = try registry.materializeExecutableDemands();
        if (executable_demands_changed) {
            try registry.buildPending();
            try registry.finalizeSessions();
            _ = try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items, true);
            continue;
        }
        if (!try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items, false)) break;
    }
    while (true) {
        try solveRepresentationSessions(&program, proc_build_records.items);
        try finalizeSourceMatchBranchReachability(&program, proc_build_records.items);
        try propagatePendingComptimeDependencyOrigins(&program, proc_build_records.items);
        try assignCallableEmissionPlans(&program, proc_build_records.items, artifact_views, .strict);
        const proc_value_requirements_changed = try appendProcValueOwnerErasureRequirements(&program, proc_build_records.items);
        const call_boundary_requirements_changed = try appendCallBoundaryErasureRequirements(&program, proc_build_records.items);
        const adapter_demands_changed = try publishValueTransformAdapterDemandsFromSolvedFlow(&program, proc_build_records.items);
        if (proc_value_requirements_changed or call_boundary_requirements_changed or adapter_demands_changed) {
            continue;
        }
        const finite_members_changed = try registry.reserveFiniteErasedAdapterMembers();
        if (finite_members_changed) {
            try registry.buildPending();
            try registry.finalizeSessions();
            _ = try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items, true);
            continue;
        }
        const executable_demands_changed = try registry.materializeExecutableDemands();
        if (executable_demands_changed) {
            try registry.buildPending();
            try registry.finalizeSessions();
            _ = try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items, true);
            continue;
        }
        break;
    }
    try sealProcRepresentationInstances(
        &program,
        proc_build_records.items,
        input.executable_synthetic_procs.items,
        registry.sealed_erased_adapter_members.items,
    );
    try publishSessionExecutableTypePayloads(&program, artifact_views);
    try finalizeBoxPayloadRepresentationPlans(&program, proc_build_records.items, artifact_views);
    try finalizeValueTransformBoundaries(&program, artifact_views);
    try publishSessionExecutableTypePayloads(&program, artifact_views);
    if (@import("builtin").mode == .Debug) {
        verifySealedLambdaSolvedProgram(&program);
        for (program.solve_sessions.items) |*session| {
            session.representation_store.verifySealed();
        }
    }
    for (program.solve_sessions.items) |*session| {
        session.state = .sealed;
    }
    try program.root_procs.appendSlice(allocator, input.root_procs.items);
    try program.root_metadata.appendSlice(allocator, input.root_metadata.items);

    input.deinit();
    return program;
}

fn verifySealedLambdaSolvedProgram(program: *const Program) void {
    if (@import("builtin").mode != .Debug) return;
    for (program.value_stores.items, 0..) |value_store, raw_store| {
        const value_store_id: repr.ValueInfoStoreId = @enumFromInt(@as(u32, @intCast(raw_store)));
        const require_exec_ty = valueStoreHasMaterializedProcInstance(program, value_store_id);
        for (value_store.values.items, 0..) |value, raw_value| {
            verifyConcreteSourcePayload(program, value.source_ty, value.source_ty_payload, "lambda-solved value");
            if (value.source_match_branch) |branch_ref| {
                if (value_store.sourceMatchBranchReachabilityId(branch_ref) == null) {
                    lambdaInvariant("lambda-solved sealed program contains a value with an unpublished source-match branch");
                }
            }
            if (value.value_alias_source) |source| {
                if (@intFromEnum(source) >= value_store.values.items.len) {
                    lambdaInvariant("lambda-solved value alias source points outside the value store");
                }
            }
            if (value.projection_info) |projection_info| {
                const projection_index = @intFromEnum(projection_info);
                if (projection_index >= value_store.projections.items.len) {
                    lambdaInvariant("lambda-solved value projection metadata points outside the projection store");
                }
                const projection = value_store.projections.items[projection_index];
                const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
                if (projection.result != value_id) {
                    lambdaInvariant("lambda-solved value projection metadata is attached to a different result value");
                }
            }
            if (value.join_info) |join_info| {
                const join_index = @intFromEnum(join_info);
                if (join_index >= value_store.joins.items.len) {
                    lambdaInvariant("lambda-solved value join metadata points outside the join store");
                }
                const join = value_store.joins.items[join_index];
                const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
                if (join.result != value_id) {
                    lambdaInvariant("lambda-solved value join metadata is attached to a different result value");
                }
            }
            if (value.solved_group == null) {
                lambdaInvariant("lambda-solved sealed program contains a value without a solved representation group");
            }
            if (!value_store.valueSourceMatchBranchReachable(value)) continue;
            if (value.pending_comptime_dependency_origin) continue;
            if (require_exec_ty and value.exec_ty == null) {
                lambdaInvariant("lambda-solved sealed program contains a value without a published executable type endpoint");
            }
        }
        for (value_store.call_sites.items) |call_site| {
            if (call_site.source_match_branch) |branch_ref| {
                if (value_store.sourceMatchBranchReachabilityId(branch_ref) == null) {
                    lambdaInvariant("lambda-solved sealed program contains a call site with an unpublished source-match branch");
                }
            }
            if (!value_store.callSiteSourceMatchBranchReachable(call_site)) continue;
            if (call_site.dispatch == null) {
                lambdaInvariant("lambda-solved sealed program contains an unresolved call-site dispatch");
            }
        }
    }
    for (program.solve_sessions.items) |session| {
        if (!session.representation_store.hasPublishedSolvedGroups()) {
            lambdaInvariant("lambda-solved sealed program contains a solve session without a complete root group table");
        }
        for (session.representation_store.box_boundaries) |boundary| {
            verifyConcreteSourcePayload(program, boundary.box_ty, boundary.box_ty_payload, "lambda-solved BoxBoundary box type");
            verifyConcreteSourcePayload(program, boundary.payload_source_ty, boundary.payload_source_ty_payload, "lambda-solved BoxBoundary payload source type");
            verifyConcreteSourcePayload(program, boundary.payload_boundary_ty, boundary.payload_boundary_ty_payload, "lambda-solved BoxBoundary payload boundary type");
        }
    }
}

fn valueStoreHasMaterializedProcInstance(
    program: *const Program,
    value_store: repr.ValueInfoStoreId,
) bool {
    for (program.proc_instances.items) |instance| {
        if (instance.value_store == value_store and instance.materialized) return true;
    }
    return false;
}

fn verifyConcreteSourcePayload(
    program: *const Program,
    key: canonical.CanonicalTypeKey,
    payload: ?ConcreteSourceType.ConcreteSourceTypeRef,
    comptime context: []const u8,
) void {
    if (isEmptyCanonicalTypeKey(key)) {
        if (payload != null) lambdaInvariant(context ++ " had a concrete source type payload for an empty key");
        return;
    }
    const ref = payload orelse lambdaInvariant(context ++ " had a source type key without a concrete source type payload");
    const payload_key = program.concrete_source_types.key(ref);
    if (!repr.canonicalTypeKeyEql(payload_key, key)) {
        lambdaInvariant(context ++ " concrete source type payload key disagrees with source type key");
    }
}

fn publishSessionExecutableTypePayloads(program: *Program, artifact_views: ArtifactViews) Allocator.Error!void {
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        var publisher = SessionExecutablePayloadPublisher{
            .program = program,
            .artifact_views = artifact_views,
            .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
            .session = session,
        };
        try publisher.publish();
    }
}

const SessionExecutablePayloadPublisher = struct {
    program: *Program,
    artifact_views: ArtifactViews,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,

    fn publish(self: *SessionExecutablePayloadPublisher) Allocator.Error!void {
        try self.publishFiniteAdapterHiddenCaptures();
        try self.publishProcValueEraseHiddenCaptures();
        try self.publishCallableSetMemberCaptures();
        try self.publishLocalValues();
    }

    fn publishFiniteAdapterHiddenCaptures(self: *SessionExecutablePayloadPublisher) Allocator.Error!void {
        for (self.representationStore().callable_emission_plans) |plan| {
            const erase = switch (plan) {
                .erase_finite_set => |erase| erase,
                else => continue,
            };
            const capture_key = erase.adapter.erased_fn_sig_key.capture_ty orelse continue;
            const endpoint = try repr.sessionExecutableTypeEndpointForCallableSetIntoStore(
                self.program.allocator,
                &self.program.canonical_names,
                &self.program.row_shapes,
                &self.program.types,
                self.representationStore(),
                &self.representationStore().session_executable_type_payloads,
                erase.adapter.callable_set_key,
                capture_key,
            );
            if (!repr.canonicalExecValueTypeKeyEql(endpoint.key, capture_key)) {
                lambdaInvariant("lambda-solved finite adapter hidden capture payload key differs from adapter signature");
            }
        }
    }

    fn publishProcValueEraseHiddenCaptures(self: *SessionExecutablePayloadPublisher) Allocator.Error!void {
        for (self.representationStore().callable_emission_plans) |plan| {
            const erase = switch (plan) {
                .erase_proc_value => |erase| erase,
                else => continue,
            };
            if (erase.capture_slots.len == 0) continue;
            const capture_key = erase.erased_fn_sig_key.capture_ty orelse {
                lambdaInvariant("lambda-solved proc-value erased payload had captures but no hidden capture key");
            };
            const endpoint = try repr.sessionExecutableTypeEndpointForCaptureSlotsIntoStore(
                self.program.allocator,
                &self.program.canonical_names,
                &self.program.row_shapes,
                &self.program.types,
                self.representationStore(),
                &self.representationStore().session_executable_type_payloads,
                erase.capture_slots,
                capture_key,
            );
            if (!repr.canonicalExecValueTypeKeyEql(endpoint.key, capture_key)) {
                lambdaInvariant("lambda-solved proc-value hidden capture payload key differs from erase plan");
            }
        }
    }

    fn publishCallableSetMemberCaptures(self: *SessionExecutablePayloadPublisher) Allocator.Error!void {
        for (self.representationStore().callable_set_descriptors) |descriptor| {
            if (!self.callableSetDescriptorIsLive(descriptor.key)) continue;
            if (descriptor.members.len == 0) {
                lambdaInvariant("lambda-solved executable payload publication reached empty callable-set descriptor");
            }
            for (descriptor.members) |member| {
                if (member.capture_slots.len == 0) continue;
                const target_instance = self.procInstance(member.target_instance);
                const target_value_store = self.mutableValueStoreFor(target_instance);
                const target_captures = target_value_store.sliceValueSpan(target_instance.public_roots.captures);
                if (target_captures.len != member.capture_slots.len) {
                    lambdaInvariant("lambda-solved callable-set member capture payload publication saw mismatched capture arity");
                }
                const seen = try self.program.allocator.alloc(bool, target_captures.len);
                defer self.program.allocator.free(seen);
                @memset(seen, false);
                for (member.capture_slots) |slot| {
                    const index: usize = @intCast(slot.slot);
                    if (index >= target_captures.len) {
                        lambdaInvariant("lambda-solved callable-set member capture payload publication saw out-of-range capture slot");
                    }
                    if (seen[index]) {
                        lambdaInvariant("lambda-solved callable-set member capture payload publication saw duplicate capture slot");
                    }
                    const endpoint = try self.publishTargetValueInOwningSession(target_instance, target_value_store, target_captures[index]);
                    target_value_store.values.items[@intFromEnum(target_captures[index])].exec_ty = endpoint;
                    if (!repr.canonicalExecValueTypeKeyEql(endpoint.key, slot.exec_value_ty)) {
                        const payload = self.representationStoreFor(target_instance).session_executable_type_payloads.get(endpoint.ty.payload);
                        lambdaInvariantFmt(
                            "lambda-solved callable-set member capture payload key differs from member schema: session={d} target_session={d} descriptor_key={x:0>2}{x:0>2}{x:0>2}{x:0>2} slot={d} member={d} target_instance={d} endpoint_payload={s} endpoint_key={x:0>2}{x:0>2}{x:0>2}{x:0>2} slot_key={x:0>2}{x:0>2}{x:0>2}{x:0>2} descriptor_live={} construction_live={} emission_live={} demand_live={}",
                            .{
                                @intFromEnum(self.session_id),
                                @intFromEnum(target_instance.solve_session),
                                descriptor.key.bytes[0],
                                descriptor.key.bytes[1],
                                descriptor.key.bytes[2],
                                descriptor.key.bytes[3],
                                slot.slot,
                                @intFromEnum(member.member),
                                @intFromEnum(member.target_instance),
                                @tagName(payload),
                                endpoint.key.bytes[0],
                                endpoint.key.bytes[1],
                                endpoint.key.bytes[2],
                                endpoint.key.bytes[3],
                                slot.exec_value_ty.bytes[0],
                                slot.exec_value_ty.bytes[1],
                                slot.exec_value_ty.bytes[2],
                                slot.exec_value_ty.bytes[3],
                                self.callableSetDescriptorIsLive(descriptor.key),
                                self.callableSetDescriptorReferencedByConstruction(descriptor.key),
                                self.callableSetDescriptorReferencedByEmission(descriptor.key),
                                self.callableSetDescriptorReferencedByDemand(descriptor.key),
                            },
                        );
                    }
                    seen[index] = true;
                }
                for (seen) |was_seen| {
                    if (!was_seen) lambdaInvariant("lambda-solved callable-set member capture slots were not dense during payload publication");
                }
            }
        }
    }

    fn callableSetDescriptorIsLive(
        self: *SessionExecutablePayloadPublisher,
        key: repr.CanonicalCallableSetKey,
    ) bool {
        return self.callableSetDescriptorReferencedByEmission(key) or
            self.callableSetDescriptorReferencedByDemand(key) or
            self.callableSetDescriptorReferencedByConstruction(key);
    }

    fn callableSetDescriptorReferencedByEmission(
        self: *SessionExecutablePayloadPublisher,
        key: repr.CanonicalCallableSetKey,
    ) bool {
        for (self.representationStore().callable_group_emissions) |maybe_emission| {
            const emission = maybe_emission orelse continue;
            if (self.callableEmissionReferencesSet(emission, key)) return true;
        }
        for (self.session.members) |instance_id| {
            const instance = self.procInstance(instance_id);
            if (!instance.materialized) continue;
            const value_store = self.valueStoreFor(instance);
            for (value_store.values.items) |value| {
                const callable = value.callable orelse continue;
                if (self.callableEmissionReferencesSet(callable.emission_plan, key)) return true;
            }
        }
        return false;
    }

    fn callableEmissionReferencesSet(
        self: *SessionExecutablePayloadPublisher,
        emission: repr.CallableValueEmissionPlanId,
        key: repr.CanonicalCallableSetKey,
    ) bool {
        return switch (self.representationStore().callableEmissionPlan(emission)) {
            .finite => |finite_key| repr.callableSetKeyEql(finite_key, key),
            .erase_finite_set => |erase| repr.callableSetKeyEql(erase.adapter.callable_set_key, key),
            .pending_proc_value,
            .already_erased,
            .erase_proc_value,
            => false,
        };
    }

    fn callableSetDescriptorReferencedByDemand(
        self: *SessionExecutablePayloadPublisher,
        key: repr.CanonicalCallableSetKey,
    ) bool {
        for (self.representationStore().finite_erased_adapter_demands) |demand| {
            if (repr.callableSetKeyEql(demand.adapter.callable_set_key, key)) return true;
        }
        return false;
    }

    fn callableSetDescriptorReferencedByConstruction(
        self: *SessionExecutablePayloadPublisher,
        key: repr.CanonicalCallableSetKey,
    ) bool {
        for (self.representationStore().callable_construction_plans) |construction| {
            if (repr.callableSetKeyEql(construction.callable_set_key, key)) return true;
        }
        return false;
    }

    fn publishLocalValues(self: *SessionExecutablePayloadPublisher) Allocator.Error!void {
        for (self.session.members) |instance_id| {
            const instance = self.procInstance(instance_id);
            if (instance.solve_session != self.session_id) {
                lambdaInvariant("lambda-solved executable payload publication session member pointed at another session");
            }
            const value_store = self.mutableValueStoreFor(instance);
            try self.publishProcedureBoundaryValues(instance_id, instance, value_store);
            if (!instance.materialized) continue;
            for (value_store.values.items, 0..) |value_info, raw_value| {
                if (!value_store.valueSourceMatchBranchReachable(value_info)) continue;
                if (value_info.pending_comptime_dependency_origin) continue;
                if (value_info.exec_ty != null) continue;
                const value: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
                const endpoint = try repr.sessionExecutableTypeEndpointForValue(
                    self.program.allocator,
                    &self.program.canonical_names,
                    &self.program.row_shapes,
                    &self.program.types,
                    self.representationStore(),
                    value_store,
                    value,
                );
                value_store.values.items[raw_value].exec_ty = endpoint;
            }
        }
    }

    fn publishProcedureBoundaryValues(
        self: *SessionExecutablePayloadPublisher,
        instance_id: repr.ProcRepresentationInstanceId,
        instance: *const repr.ProcRepresentationInstance,
        value_store: *repr.ValueInfoStore,
    ) Allocator.Error!void {
        const params = value_store.sliceValueSpan(instance.public_roots.params);
        if (params.len != instance.executable_specialization_key.exec_arg_tys.len) {
            lambdaInvariant("lambda-solved executable payload publication param arity differs from specialization key");
        }
        for (params, instance.executable_specialization_key.exec_arg_tys) |param, expected_key| {
            value_store.values.items[@intFromEnum(param)].exec_ty = try self.boundaryEndpointForExpectedKey(
                instance_id,
                instance,
                value_store,
                param,
                expected_key,
            );
        }
        value_store.values.items[@intFromEnum(instance.public_roots.ret)].exec_ty = try self.boundaryEndpointForExpectedKey(
            instance_id,
            instance,
            value_store,
            instance.public_roots.ret,
            instance.executable_specialization_key.exec_ret_ty,
        );
    }

    fn boundaryEndpointForExpectedKey(
        self: *SessionExecutablePayloadPublisher,
        _: repr.ProcRepresentationInstanceId,
        instance: *const repr.ProcRepresentationInstance,
        value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        if (instance.boundary_payloads) |boundary_payloads| {
            return try self.importArtifactBoundaryEndpoint(boundary_payloads, expected_key);
        }
        if (self.representationStore().session_executable_type_payloads.refForKey(expected_key)) |published| {
            return .{
                .ty = published,
                .key = expected_key,
            };
        }
        const computed = try repr.sessionExecutableTypeEndpointForValue(
            self.program.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStore(),
            value_store,
            value,
        );
        if (!repr.canonicalExecValueTypeKeyEql(computed.key, expected_key)) {
            lambdaInvariant("lambda-solved public procedure boundary endpoint key differs from executable specialization key");
        }
        return computed;
    }

    fn importArtifactBoundaryEndpoint(
        self: *SessionExecutablePayloadPublisher,
        boundary_payloads: repr.ProcBoundaryExecutablePayloads,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        const source_ref = boundary_payloads.payloads.refForKey(.{ .bytes = boundary_payloads.artifact.bytes }, expected_key) orelse {
            lambdaInvariant("lambda-solved adapter-owned public boundary key has no published executable payload");
        };
        const source_names = canonicalNamesForArtifactViews(self.artifact_views, boundary_payloads.artifact);
        var importer = ArtifactExecutablePayloadImporter.init(
            self.program.allocator,
            source_names,
            &self.program.canonical_names,
            &self.program.row_shapes,
            boundary_payloads.artifact,
            boundary_payloads.payloads,
            &self.representationStore().session_executable_type_payloads,
        );
        defer importer.deinit();
        return try importer.importRef(source_ref, expected_key);
    }

    fn publishTargetValue(
        self: *SessionExecutablePayloadPublisher,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        return try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.program.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_instance),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            value,
        );
    }

    fn publishTargetValueInOwningSession(
        self: *SessionExecutablePayloadPublisher,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        return try repr.sessionExecutableTypeEndpointForValue(
            self.program.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.mutableRepresentationStoreFor(target_instance),
            target_value_store,
            value,
        );
    }

    fn representationStore(self: *SessionExecutablePayloadPublisher) *repr.RepresentationStore {
        return &self.session.representation_store;
    }

    fn representationStoreFor(
        self: *SessionExecutablePayloadPublisher,
        instance: *const repr.ProcRepresentationInstance,
    ) *const repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
    }

    fn mutableRepresentationStoreFor(
        self: *SessionExecutablePayloadPublisher,
        instance: *const repr.ProcRepresentationInstance,
    ) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
    }

    fn valueStoreFor(
        self: *SessionExecutablePayloadPublisher,
        instance: *const repr.ProcRepresentationInstance,
    ) *const repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(instance.value_store)];
    }

    fn mutableValueStoreFor(
        self: *SessionExecutablePayloadPublisher,
        instance: *const repr.ProcRepresentationInstance,
    ) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(instance.value_store)];
    }

    fn procInstance(
        self: *SessionExecutablePayloadPublisher,
        id: repr.ProcRepresentationInstanceId,
    ) *const repr.ProcRepresentationInstance {
        const index = @intFromEnum(id);
        if (index >= self.program.proc_instances.items.len) {
            lambdaInvariant("lambda-solved executable payload publication referenced out-of-range procedure instance");
        }
        return &self.program.proc_instances.items[index];
    }
};

const ArtifactExecutablePayloadImporter = struct {
    allocator: Allocator,
    source_names: *const canonical.CanonicalNameStore,
    target_names: *canonical.CanonicalNameStore,
    row_shapes: *MonoRow.Store,
    source_artifact: checked_artifact.CheckedModuleArtifactKey,
    source_payloads: *const checked_artifact.ExecutableTypePayloadStore,
    target_payloads: *repr.SessionExecutableTypePayloadStore,
    active: std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, repr.SessionExecutableTypePayloadId),

    fn init(
        allocator: Allocator,
        source_names: *const canonical.CanonicalNameStore,
        target_names: *canonical.CanonicalNameStore,
        row_shapes: *MonoRow.Store,
        source_artifact: checked_artifact.CheckedModuleArtifactKey,
        source_payloads: *const checked_artifact.ExecutableTypePayloadStore,
        target_payloads: *repr.SessionExecutableTypePayloadStore,
    ) ArtifactExecutablePayloadImporter {
        return .{
            .allocator = allocator,
            .source_names = source_names,
            .target_names = target_names,
            .row_shapes = row_shapes,
            .source_artifact = source_artifact,
            .source_payloads = source_payloads,
            .target_payloads = target_payloads,
            .active = std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, repr.SessionExecutableTypePayloadId).init(allocator),
        };
    }

    fn deinit(self: *ArtifactExecutablePayloadImporter) void {
        self.active.deinit();
    }

    fn importRef(
        self: *ArtifactExecutablePayloadImporter,
        source_ref: checked_artifact.ExecutableTypePayloadRef,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        if (!artifactRefEql(source_ref.artifact, self.source_artifact)) {
            lambdaInvariant("lambda-solved artifact executable payload import saw a ref owned by another artifact");
        }
        return try self.importPayloadId(source_ref.payload, expected_key);
    }

    fn importPayloadId(
        self: *ArtifactExecutablePayloadImporter,
        source_id: checked_artifact.ExecutableTypePayloadId,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        const source_key = self.source_payloads.keyFor(source_id);
        if (!repr.canonicalExecValueTypeKeyEql(source_key, expected_key)) {
            lambdaInvariant("lambda-solved artifact executable payload import key differs from expected key");
        }
        if (self.active.get(source_id)) |active_id| {
            return .{
                .ty = .{ .payload = active_id },
                .key = expected_key,
            };
        }
        if (self.target_payloads.refForKey(expected_key)) |existing| {
            if (self.target_payloads.isPending(existing.payload)) {
                lambdaInvariant("lambda-solved artifact executable payload import found a pending payload that was not active");
            }
            return .{
                .ty = existing,
                .key = expected_key,
            };
        }

        const target_id = try self.target_payloads.reserve(self.allocator, expected_key);
        try self.active.put(source_id, target_id);
        errdefer _ = self.active.remove(source_id);

        const payload = try self.importPayload(self.source_payloads.get(source_id));
        self.target_payloads.fill(self.allocator, target_id, payload);
        _ = self.active.remove(source_id);
        return .{
            .ty = .{ .payload = target_id },
            .key = expected_key,
        };
    }

    fn importPayload(
        self: *ArtifactExecutablePayloadImporter,
        payload: checked_artifact.ExecutableTypePayload,
    ) Allocator.Error!repr.SessionExecutableTypePayload {
        return switch (payload) {
            .pending => lambdaInvariant("lambda-solved artifact executable payload import reached pending payload"),
            .primitive => |prim| .{ .primitive = prim },
            .record => |fields| .{ .record = try self.importRecordPayload(fields) },
            .tuple => |items| .{ .tuple = try self.importTuplePayload(items) },
            .tag_union => |variants| .{ .tag_union = try self.importTagUnionPayload(variants) },
            .list => |child| .{ .list = try self.importChildPayload(child) },
            .box => |child| .{ .box = try self.importChildPayload(child) },
            .nominal => |nominal| .{ .nominal = try self.importNominalPayload(nominal) },
            .callable_set => |callable_set| .{ .callable_set = try self.importCallableSetPayload(callable_set) },
            .erased_fn => |erased| .{ .erased_fn = try self.importErasedFnPayload(erased) },
            .vacant_callable_slot => .vacant_callable_slot,
            .recursive_ref => |recursive| .{ .recursive_ref = try self.importRecursiveRef(recursive) },
        };
    }

    fn importChildPayload(
        self: *ArtifactExecutablePayloadImporter,
        child: checked_artifact.ExecutableTypePayloadChild,
    ) Allocator.Error!repr.SessionExecutableTypePayloadChild {
        const imported = try self.importRef(child.ty, child.key);
        return .{
            .ty = imported.ty,
            .key = imported.key,
        };
    }

    fn importRecordPayload(
        self: *ArtifactExecutablePayloadImporter,
        fields: []const checked_artifact.ExecutableRecordFieldPayload,
    ) Allocator.Error!repr.SessionExecutableRecordPayload {
        if (fields.len == 0) {
            const shape = try self.row_shapes.internRecordShapeFromLabels(&.{});
            return .{ .shape = shape, .fields = &.{} };
        }
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, fields.len);
        defer self.allocator.free(labels);
        for (fields, 0..) |field, i| labels[i] = try self.recordFieldLabel(field.field);

        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        if (self.row_shapes.recordShapeFields(shape).len != fields.len) {
            lambdaInvariant("lambda-solved artifact executable record payload shape arity mismatch");
        }

        const out = try self.allocator.alloc(repr.SessionExecutableRecordFieldPayload, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            const child = try self.importRef(field.ty, field.key);
            const target_label = labels[i];
            out[i] = .{
                .field = self.recordFieldInShape(shape, target_label),
                .ty = child.ty,
                .key = child.key,
            };
        }
        return .{ .shape = shape, .fields = out };
    }

    fn importTuplePayload(
        self: *ArtifactExecutablePayloadImporter,
        items: []const checked_artifact.ExecutableTupleElemPayload,
    ) Allocator.Error![]const repr.SessionExecutableTupleElemPayload {
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(repr.SessionExecutableTupleElemPayload, items.len);
        errdefer self.allocator.free(out);
        for (items, 0..) |item, i| {
            if (item.index != i) lambdaInvariant("lambda-solved artifact executable tuple payload indexes are not canonical");
            const child = try self.importRef(item.ty, item.key);
            out[i] = .{
                .index = item.index,
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn importTagUnionPayload(
        self: *ArtifactExecutablePayloadImporter,
        variants: []const checked_artifact.ExecutableTagVariantPayload,
    ) Allocator.Error!repr.SessionExecutableTagUnionPayload {
        if (variants.len == 0) {
            const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(&.{});
            return .{ .shape = shape, .variants = &.{} };
        }
        const descriptors = try self.allocator.alloc(MonoRow.Store.TagShapeDescriptor, variants.len);
        defer self.allocator.free(descriptors);
        for (variants, 0..) |variant, i| {
            descriptors[i] = .{
                .name = try self.tagLabel(variant.tag),
                .payload_arity = @intCast(variant.payloads.len),
            };
        }

        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        if (self.row_shapes.tagUnionTags(shape).len != variants.len) {
            lambdaInvariant("lambda-solved artifact executable tag payload shape arity mismatch");
        }

        const out = try self.allocator.alloc(repr.SessionExecutableTagVariantPayload, variants.len);
        for (out) |*variant| variant.* = .{ .tag = undefined, .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }
        for (variants, 0..) |variant, i| {
            const target_label = descriptors[i].name;
            const target_tag = self.tagInShape(shape, target_label);
            out[i] = .{
                .tag = target_tag,
                .payloads = try self.importTagPayloads(target_tag, variant.payloads),
            };
        }
        return .{ .shape = shape, .variants = out };
    }

    fn recordFieldInShape(
        self: *ArtifactExecutablePayloadImporter,
        shape: MonoRow.RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) MonoRow.RecordFieldId {
        for (self.row_shapes.recordShapeFields(shape)) |field_id| {
            if (self.row_shapes.recordField(field_id).label == label) return field_id;
        }
        lambdaInvariant("lambda-solved artifact executable record payload label missing from interned shape");
    }

    fn tagInShape(
        self: *ArtifactExecutablePayloadImporter,
        shape: MonoRow.TagUnionShapeId,
        label: canonical.TagLabelId,
    ) MonoRow.TagId {
        for (self.row_shapes.tagUnionTags(shape)) |tag_id| {
            if (self.row_shapes.tag(tag_id).label == label) return tag_id;
        }
        lambdaInvariant("lambda-solved artifact executable tag payload label missing from interned shape");
    }

    fn importTagPayloads(
        self: *ArtifactExecutablePayloadImporter,
        tag: MonoRow.TagId,
        payloads: []const checked_artifact.ExecutableTagPayload,
    ) Allocator.Error![]const repr.SessionExecutableTagPayload {
        if (payloads.len == 0) return &.{};
        const shape_payloads = self.row_shapes.tagPayloads(tag);
        if (shape_payloads.len != payloads.len) {
            lambdaInvariant("lambda-solved artifact executable tag payload arity mismatch");
        }
        const out = try self.allocator.alloc(repr.SessionExecutableTagPayload, payloads.len);
        errdefer self.allocator.free(out);
        for (payloads, 0..) |payload, i| {
            if (payload.index != i) lambdaInvariant("lambda-solved artifact executable tag payload indexes are not canonical");
            const child = try self.importRef(payload.ty, payload.key);
            out[i] = .{
                .payload = shape_payloads[i],
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn importNominalPayload(
        self: *ArtifactExecutablePayloadImporter,
        nominal: checked_artifact.ExecutableNominalPayload,
    ) Allocator.Error!repr.SessionExecutableNominalPayload {
        const backing = try self.importRef(nominal.backing, nominal.backing_key);
        return .{
            .nominal = try self.nominalTypeKey(nominal.nominal),
            .source_ty = nominal.source_ty,
            .is_opaque = false,
            .backing = backing.ty,
            .backing_key = backing.key,
        };
    }

    fn importCallableSetPayload(
        self: *ArtifactExecutablePayloadImporter,
        callable_set: checked_artifact.ExecutableCallableSetPayload,
    ) Allocator.Error!repr.SessionExecutableCallableSetPayload {
        if (callable_set.members.len == 0) return .{
            .key = callable_set.key,
            .members = &.{},
        };
        const members = try self.allocator.alloc(repr.SessionExecutableCallableSetMemberPayload, callable_set.members.len);
        errdefer self.allocator.free(members);
        for (callable_set.members, 0..) |member, i| {
            members[i] = .{
                .member = member.member,
                .payload_ty = null,
                .payload_ty_key = null,
            };
            if (member.payload_ty) |payload_ty| {
                const payload_key = member.payload_ty_key orelse {
                    lambdaInvariant("lambda-solved artifact executable callable-set member payload has no key");
                };
                const payload = try self.importRef(payload_ty, payload_key);
                members[i].payload_ty = payload.ty;
                members[i].payload_ty_key = payload.key;
            }
        }
        return .{
            .key = callable_set.key,
            .members = members,
        };
    }

    fn importErasedFnPayload(
        self: *ArtifactExecutablePayloadImporter,
        erased: checked_artifact.ExecutableErasedFnPayload,
    ) Allocator.Error!repr.SessionExecutableErasedFnPayload {
        const capture = if (erased.capture_ty) |capture_ty| blk: {
            const capture_key = erased.capture_ty_key orelse {
                lambdaInvariant("lambda-solved artifact executable erased payload capture has no key");
            };
            break :blk try self.importRef(capture_ty, capture_key);
        } else null;
        return .{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .capture_ty = if (capture) |item| item.ty else null,
            .capture_ty_key = if (capture) |item| item.key else null,
        };
    }

    fn importRecursiveRef(
        self: *ArtifactExecutablePayloadImporter,
        ref: checked_artifact.ExecutableTypePayloadId,
    ) Allocator.Error!repr.SessionExecutableTypePayloadId {
        if (self.active.get(ref)) |active_id| return active_id;
        const key = self.source_payloads.keyFor(ref);
        const imported = try self.importPayloadId(ref, key);
        return imported.ty.payload;
    }

    fn recordFieldLabel(
        self: *ArtifactExecutablePayloadImporter,
        label: canonical.RecordFieldLabelId,
    ) Allocator.Error!canonical.RecordFieldLabelId {
        return try self.target_names.internRecordFieldLabel(self.source_names.recordFieldLabelText(label));
    }

    fn tagLabel(
        self: *ArtifactExecutablePayloadImporter,
        tag: canonical.TagLabelId,
    ) Allocator.Error!canonical.TagLabelId {
        return try self.target_names.internTagLabel(self.source_names.tagLabelText(tag));
    }

    fn nominalTypeKey(
        self: *ArtifactExecutablePayloadImporter,
        nominal: canonical.NominalTypeKey,
    ) Allocator.Error!canonical.NominalTypeKey {
        return .{
            .module_name = try self.target_names.internModuleName(self.source_names.moduleNameText(nominal.module_name)),
            .type_name = try self.target_names.internTypeName(self.source_names.typeNameText(nominal.type_name)),
        };
    }
};

fn appendProcValueOwnerErasureRequirements(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!bool {
    var changed = false;
    for (records) |record| {
        const owner = switch (record.owner) {
            .proc_value => |owner| owner,
            else => continue,
        };
        const requirement_plan = try procValueOwnerErasureRequirementPlan(program, records, record, owner.owner, owner.value) orelse continue;
        const value_store = &program.value_stores.items[@intFromEnum(record.value_store)];
        const store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
        const params = value_store.sliceValueSpan(record.public_roots.params);
        if (params.len != requirement_plan.key.exec_arg_tys.len) {
            lambdaInvariant("lambda-solved proc-value erased requirement key arity differs from public roots");
        }
        for (params, requirement_plan.key.exec_arg_tys) |param, exec_key| {
            if (!try executableTypeKeyContainsErasedFnInStore(program.allocator, store, exec_key)) continue;
            for (requirement_plan.provenance) |provenance| {
                changed = (try appendRequireBoxErasedIfMissing(store, value_store.values.items[@intFromEnum(param)].root, provenance)) or changed;
            }
        }
        if (try executableTypeKeyContainsErasedFnInStore(program.allocator, store, requirement_plan.key.exec_ret_ty)) {
            for (requirement_plan.provenance) |provenance| {
                changed = (try appendRequireBoxErasedIfMissing(store, value_store.values.items[@intFromEnum(record.public_roots.ret)].root, provenance)) or changed;
            }
        }
    }
    return changed;
}

fn appendCallBoundaryErasureRequirements(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!bool {
    var changed = false;
    for (records) |record| {
        const value_store = &program.value_stores.items[@intFromEnum(record.value_store)];
        const store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
        for (value_store.call_sites.items) |call_site| {
            if (!value_store.callSiteSourceMatchBranchReachable(call_site)) continue;
            const dispatch = call_site.dispatch orelse continue;
            switch (dispatch) {
                .call_proc => |target| {
                    changed = (try appendDirectCallBoundaryErasureRequirements(
                        program,
                        records,
                        store,
                        value_store,
                        call_site,
                        target,
                    )) or changed;
                },
                .call_value_finite => |plan_id| {
                    const plan = value_store.callValueFiniteDispatchPlan(plan_id);
                    for (value_store.sliceCallValueFiniteDispatchBranches(plan.branches)) |branch| {
                        changed = (try appendDirectCallBoundaryErasureRequirements(
                            program,
                            records,
                            store,
                            value_store,
                            call_site,
                            branch.target_instance,
                        )) or changed;
                    }
                },
                .call_value_erased => |sig_key| {
                    changed = (try appendErasedCallValueBoundaryErasureRequirements(
                        program,
                        store,
                        value_store,
                        call_site,
                        sig_key,
                    )) or changed;
                },
                .pending_comptime_dependency_call,
                => {},
            }
        }
    }
    return changed;
}

fn appendDirectCallBoundaryErasureRequirements(
    program: *Program,
    records: []const ProcBuildRecord,
    store: *repr.RepresentationStore,
    caller_value_store: *const repr.ValueInfoStore,
    call_site: repr.CallSiteInfo,
    target: repr.ProcRepresentationInstanceId,
) Allocator.Error!bool {
    const target_plan = try procedureBoundaryErasureRequirementPlan(program, records, target) orelse return false;
    const target_index = @intFromEnum(target);
    if (target_index >= records.len) {
        lambdaInvariant("lambda-solved call-boundary erased requirement referenced out-of-range target");
    }
    const target_store = &program.solve_sessions.items[@intFromEnum(records[target_index].solve_session)].representation_store;
    const args = caller_value_store.sliceValueSpan(call_site.args);
    if (args.len != target_plan.key.exec_arg_tys.len) {
        lambdaInvariant("lambda-solved call-boundary erased requirement arity differs from target key");
    }
    var changed = false;
    for (args, target_plan.key.exec_arg_tys) |arg, exec_key| {
        if (!try executableTypeKeyContainsErasedFnInStore(program.allocator, target_store, exec_key)) continue;
        for (target_plan.provenance) |provenance| {
            changed = (try appendRequireBoxErasedIfMissing(store, caller_value_store.values.items[@intFromEnum(arg)].root, provenance)) or changed;
        }
    }
    return changed;
}

fn appendErasedCallValueBoundaryErasureRequirements(
    program: *Program,
    store: *repr.RepresentationStore,
    caller_value_store: *const repr.ValueInfoStore,
    call_site: repr.CallSiteInfo,
    sig_key: repr.ErasedFnSigKey,
) Allocator.Error!bool {
    const callee = call_site.callee orelse {
        lambdaInvariant("lambda-solved erased call-boundary requirement has no callee value");
    };
    const callee_info = caller_value_store.values.items[@intFromEnum(callee)];
    const callable = callee_info.callable orelse {
        lambdaInvariant("lambda-solved erased call-boundary requirement callee has no callable emission");
    };
    const provenance = erasedCallBoundaryProvenance(store, callable.emission_plan, sig_key);
    if (provenance.len == 0) {
        lambdaInvariant("lambda-solved erased call-boundary requirement has no Box(T) provenance");
    }
    const abi = store.erased_fn_abis.abiFor(sig_key.abi) orelse {
        lambdaInvariant("lambda-solved erased call-boundary requirement referenced missing ABI");
    };
    const args = caller_value_store.sliceValueSpan(call_site.args);
    if (args.len != abi.arg_exec_keys.len) {
        lambdaInvariant("lambda-solved erased call-boundary requirement arity differs from ABI");
    }
    var changed = false;
    for (args, abi.arg_exec_keys) |arg, exec_key| {
        if (!try executableTypeKeyContainsErasedFnInStore(program.allocator, store, exec_key)) continue;
        for (provenance) |item| {
            changed = (try appendRequireBoxErasedIfMissing(store, caller_value_store.values.items[@intFromEnum(arg)].root, item)) or changed;
        }
    }
    return changed;
}

fn erasedCallBoundaryProvenance(
    store: *const repr.RepresentationStore,
    emission_plan: repr.CallableValueEmissionPlanId,
    sig_key: repr.ErasedFnSigKey,
) []const repr.BoxErasureProvenance {
    return switch (store.callableEmissionPlan(emission_plan)) {
        .already_erased => |erased| blk: {
            if (!repr.erasedFnSigKeyEql(erased.sig_key, sig_key)) {
                lambdaInvariant("lambda-solved erased call-boundary already-erased signature differs from call site");
            }
            break :blk erased.provenance;
        },
        .erase_proc_value => |erase| blk: {
            if (!repr.erasedFnSigKeyEql(erase.erased_fn_sig_key, sig_key)) {
                lambdaInvariant("lambda-solved erased call-boundary proc-value signature differs from call site");
            }
            break :blk erase.provenance;
        },
        .erase_finite_set => |erase| blk: {
            if (!repr.erasedFnSigKeyEql(erase.adapter.erased_fn_sig_key, sig_key)) {
                lambdaInvariant("lambda-solved erased call-boundary finite-set signature differs from call site");
            }
            break :blk erase.provenance;
        },
        .finite,
        .pending_proc_value,
        => lambdaInvariant("lambda-solved erased call-boundary reached non-erased callable emission"),
    };
}

fn publishValueTransformAdapterDemandsFromSolvedFlow(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!bool {
    var changed = false;
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        var publisher = FiniteErasedAdapterDemandPublisher{
            .allocator = program.allocator,
            .program = program,
            .records = records,
            .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
            .store = &session.representation_store,
        };
        changed = (try publisher.publish()) or changed;
    }
    return changed;
}

const FiniteErasedAdapterDemandPublisher = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    session_id: repr.RepresentationSolveSessionId,
    store: *repr.RepresentationStore,

    fn publish(self: *FiniteErasedAdapterDemandPublisher) Allocator.Error!bool {
        var changed = false;
        for (self.store.callable_emission_plans) |emission| {
            const erase = switch (emission) {
                .erase_finite_set => |erase| erase,
                else => continue,
            };
            changed = (try self.publishForAdapter(erase.adapter, erase.result_ty, erase.member_targets, erase.provenance)) or changed;
        }

        var demand_index: usize = 0;
        while (demand_index < self.store.finite_erased_adapter_demands.len) : (demand_index += 1) {
            const demand = self.store.finite_erased_adapter_demands[demand_index];
            changed = (try self.publishForAdapter(demand.adapter, demand.result_ty, demand.member_targets, demand.provenance)) or changed;
        }
        return changed;
    }

    fn publishForAdapter(
        self: *FiniteErasedAdapterDemandPublisher,
        adapter: repr.ErasedAdapterKey,
        _: repr.CanonicalExecValueTypeKey,
        member_targets: []const repr.ExecutableSpecializationKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!bool {
        if (provenance.len == 0) {
            lambdaInvariant("lambda-solved finite erased adapter demand publication has no Box(T) provenance");
        }
        const descriptor = self.store.callableSetDescriptor(adapter.callable_set_key) orelse {
            lambdaInvariant("lambda-solved finite erased adapter demand publication referenced missing callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite erased adapter demand publication reached empty descriptor");
        }
        if (descriptor.members.len != member_targets.len) {
            lambdaInvariant("lambda-solved finite erased adapter demand publication target count differs from descriptor");
        }
        const abi = self.store.erased_fn_abis.abiFor(adapter.erased_fn_sig_key.abi) orelse {
            lambdaInvariant("lambda-solved finite erased adapter demand publication referenced unpublished erased ABI");
        };

        var changed = false;
        for (descriptor.members, member_targets) |member, target_key| {
            validatePersistedFiniteAdapterMemberTarget(member, target_key);
            const target_record = self.recordForFiniteAdapterTarget(member.source_proc, target_key) orelse continue;
            const target_value_store = self.valueStoreFor(target_record);
            const target_params = target_value_store.sliceValueSpan(target_record.public_roots.params);
            if (target_params.len != abi.arg_exec_keys.len or target_params.len != target_key.exec_arg_tys.len) {
                lambdaInvariant("lambda-solved finite erased adapter demand publication arity differs from ABI or target key");
            }
            for (target_params, abi.arg_exec_keys) |target_param, raw_arg_key| {
                const target_endpoint = try self.publishTargetValue(target_record, target_value_store, target_param);
                changed = (try self.publishForKeyPair(raw_arg_key, target_endpoint.key, provenance)) or changed;
            }

            const target_captures = target_value_store.sliceValueSpan(target_record.public_roots.captures);
            if (target_captures.len != member.capture_slots.len) {
                lambdaInvariant("lambda-solved finite erased adapter demand publication capture arity differs from descriptor");
            }
            for (member.capture_slots, target_captures) |slot, target_capture| {
                const index: usize = @intCast(slot.slot);
                if (index >= target_captures.len or target_captures[index] != target_capture) {
                    lambdaInvariant("lambda-solved finite erased adapter demand publication capture slots are not canonical");
                }
                const target_endpoint = try self.publishTargetValue(target_record, target_value_store, target_capture);
                changed = (try self.publishForKeyPair(slot.exec_value_ty, target_endpoint.key, provenance)) or changed;
            }

            const target_ret = try self.publishTargetValue(target_record, target_value_store, target_record.public_roots.ret);
            changed = (try self.publishForKeyPair(target_ret.key, abi.ret_exec_key, provenance)) or changed;
        }
        return changed;
    }

    fn recordForFiniteAdapterTarget(
        self: *FiniteErasedAdapterDemandPublisher,
        proc: canonical.MirProcedureRef,
        target_key: repr.ExecutableSpecializationKey,
    ) ?*const ProcBuildRecord {
        for (self.records) |*record| {
            if (record.solve_session != self.session_id) continue;
            if (!record.materialized or !record.built or !record.has_public_roots) continue;
            if (!canonical.mirProcedureRefEql(record.proc, proc)) continue;
            const existing_key = self.finiteAdapterTargetKeyForRecord(record) orelse continue;
            if (repr.executableSpecializationKeyEql(existing_key, target_key)) return record;
        }
        return null;
    }

    fn finiteAdapterTargetKeyForRecord(
        self: *FiniteErasedAdapterDemandPublisher,
        record: *const ProcBuildRecord,
    ) ?repr.ExecutableSpecializationKey {
        return switch (record.owner) {
            .finite_erased_adapter_member => |member| blk: {
                const plan = self.store.callableEmissionPlan(member.emission_plan);
                const erase = switch (plan) {
                    .erase_finite_set => |erase| erase,
                    else => lambdaInvariant("lambda-solved finite erased adapter demand publication referenced non-erased emission plan"),
                };
                if (member.member_index >= erase.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand publication member index is out of range");
                }
                break :blk erase.member_targets[member.member_index];
            },
            .finite_erased_adapter_demand_member => |member| blk: {
                const demand = self.store.finiteErasedAdapterDemand(member.demand);
                if (member.member_index >= demand.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand publication demand member index is out of range");
                }
                break :blk demand.member_targets[member.member_index];
            },
            else => null,
        };
    }

    fn publishTargetValue(
        self: *FiniteErasedAdapterDemandPublisher,
        target_record: *const ProcBuildRecord,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        return try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_record),
            &self.store.session_executable_type_payloads,
            target_value_store,
            value,
        );
    }

    fn publishForKeyPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source_key: repr.CanonicalExecValueTypeKey,
        target_key: repr.CanonicalExecValueTypeKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!bool {
        if (repr.canonicalExecValueTypeKeyEql(source_key, target_key)) return false;
        const source_ref = self.store.session_executable_type_payloads.refForKey(source_key) orelse {
            lambdaInvariant("lambda-solved finite erased adapter demand source key has no executable payload");
        };
        const target_ref = self.store.session_executable_type_payloads.refForKey(target_key) orelse {
            lambdaInvariant("lambda-solved finite erased adapter demand target key has no executable payload");
        };
        var visited = std.AutoHashMap(u64, void).init(self.allocator);
        defer visited.deinit();
        return try self.publishForPayloadPair(source_ref, source_key, target_ref, target_key, provenance, &visited);
    }

    fn publishForPayloadPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source_ref: repr.SessionExecutableTypePayloadRef,
        source_key: repr.CanonicalExecValueTypeKey,
        target_ref: repr.SessionExecutableTypePayloadRef,
        target_key: repr.CanonicalExecValueTypeKey,
        provenance: []const repr.BoxErasureProvenance,
        visited: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        if (repr.canonicalExecValueTypeKeyEql(source_key, target_key)) return false;
        const visit_key = (@as(u64, @intFromEnum(source_ref.payload)) << 32) | @as(u64, @intFromEnum(target_ref.payload));
        if (visited.contains(visit_key)) return false;
        try visited.put(visit_key, {});

        const source_payload = self.store.session_executable_type_payloads.get(source_ref.payload);
        const target_payload = self.store.session_executable_type_payloads.get(target_ref.payload);
        return switch (source_payload) {
            .record => |source| switch (target_payload) {
                .record => |target| try self.publishRecordPayloadPair(source, target, provenance, visited),
                else => false,
            },
            .tuple => |source| switch (target_payload) {
                .tuple => |target| try self.publishTuplePayloadPair(source, target, provenance, visited),
                else => false,
            },
            .tag_union => |source| switch (target_payload) {
                .tag_union => |target| try self.publishTagUnionPayloadPair(source, target, provenance, visited),
                .nominal => |target| try self.publishForPayloadPair(source_ref, source_key, target.backing, target.backing_key, provenance, visited),
                else => false,
            },
            .nominal => |source| switch (target_payload) {
                .nominal => |target| try self.publishForPayloadPair(source.backing, source.backing_key, target.backing, target.backing_key, provenance, visited),
                .tag_union,
                .record,
                .tuple,
                => try self.publishForPayloadPair(source.backing, source.backing_key, target_ref, target_key, provenance, visited),
                else => false,
            },
            .list => |source| switch (target_payload) {
                .list => |target| try self.publishForPayloadPair(source.ty, source.key, target.ty, target.key, provenance, visited),
                else => false,
            },
            .box => |source| switch (target_payload) {
                .box => |target| try self.publishForPayloadPair(source.ty, source.key, target.ty, target.key, provenance, visited),
                else => false,
            },
            .callable_set => |source| switch (target_payload) {
                .erased_fn => |target| try self.publishFiniteCallableToErasedDemand(source, target, target_key, provenance),
                .callable_set => |target| try self.publishCallableSetPayloadPair(source, target, provenance, visited),
                else => false,
            },
            .erased_fn => |source| switch (target_payload) {
                .erased_fn => |target| try self.publishErasedPayloadPair(source, target, provenance, visited),
                else => false,
            },
            .recursive_ref => |source| {
                const ref: repr.SessionExecutableTypePayloadRef = .{ .payload = source };
                return try self.publishForPayloadPair(ref, source_key, target_ref, target_key, provenance, visited);
            },
            .pending,
            .primitive,
            .vacant_callable_slot,
            => false,
        };
    }

    fn publishFiniteCallableToErasedDemand(
        self: *FiniteErasedAdapterDemandPublisher,
        source: repr.SessionExecutableCallableSetPayload,
        target: repr.SessionExecutableErasedFnPayload,
        result_ty: repr.CanonicalExecValueTypeKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!bool {
        if (provenance.len == 0) {
            lambdaInvariant("lambda-solved finite callable adapter demand has no Box(T) provenance");
        }
        const descriptor = self.store.callableSetDescriptor(source.key) orelse {
            lambdaInvariant("lambda-solved finite callable adapter demand referenced missing callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite callable adapter demand reached empty descriptor");
        }
        const abi = self.store.erased_fn_abis.abiFor(target.sig_key.abi) orelse {
            lambdaInvariant("lambda-solved finite callable adapter demand referenced unpublished erased ABI");
        };
        const member_targets = try finiteErasedAdapterMemberTargetsForAbi(self.allocator, descriptor.members, abi);
        defer deinitExecutableSpecializationKeySlice(self.allocator, member_targets);
        const before = self.store.finite_erased_adapter_demands.len;
        _ = try self.store.ensureFiniteErasedAdapterDemand(.{
            .adapter = .{
                .source_fn_ty = target.sig_key.source_fn_ty,
                .callable_set_key = source.key,
                .erased_fn_sig_key = target.sig_key,
                .capture_shape_key = target.capture_shape_key,
            },
            .result_ty = result_ty,
            .member_targets = member_targets,
            .provenance = provenance,
        });
        return self.store.finite_erased_adapter_demands.len != before;
    }

    fn publishRecordPayloadPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source: repr.SessionExecutableRecordPayload,
        target: repr.SessionExecutableRecordPayload,
        provenance: []const repr.BoxErasureProvenance,
        visited: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        var changed = false;
        for (target.fields) |target_field| {
            const label = self.program.row_shapes.recordField(target_field.field).label;
            const source_field = self.recordFieldPayloadByLabel(source, label) orelse continue;
            changed = (try self.publishForPayloadPair(source_field.ty, source_field.key, target_field.ty, target_field.key, provenance, visited)) or changed;
        }
        return changed;
    }

    fn publishTuplePayloadPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source: []const repr.SessionExecutableTupleElemPayload,
        target: []const repr.SessionExecutableTupleElemPayload,
        provenance: []const repr.BoxErasureProvenance,
        visited: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        if (source.len != target.len) return false;
        var changed = false;
        for (target, 0..) |target_elem, i| {
            const source_elem = source[i];
            if (source_elem.index != target_elem.index) continue;
            changed = (try self.publishForPayloadPair(source_elem.ty, source_elem.key, target_elem.ty, target_elem.key, provenance, visited)) or changed;
        }
        return changed;
    }

    fn publishTagUnionPayloadPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source: repr.SessionExecutableTagUnionPayload,
        target: repr.SessionExecutableTagUnionPayload,
        provenance: []const repr.BoxErasureProvenance,
        visited: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        var changed = false;
        for (source.variants) |source_variant| {
            const label = self.program.row_shapes.tag(source_variant.tag).label;
            const target_variant = self.tagVariantPayloadByLabel(target, label) orelse continue;
            for (target_variant.payloads) |target_payload| {
                const target_index = self.program.row_shapes.tagPayload(target_payload.payload).logical_index;
                const source_payload = self.tagPayloadByLogicalIndex(source_variant, target_index) orelse continue;
                changed = (try self.publishForPayloadPair(source_payload.ty, source_payload.key, target_payload.ty, target_payload.key, provenance, visited)) or changed;
            }
        }
        return changed;
    }

    fn publishCallableSetPayloadPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source: repr.SessionExecutableCallableSetPayload,
        target: repr.SessionExecutableCallableSetPayload,
        provenance: []const repr.BoxErasureProvenance,
        visited: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        var changed = false;
        for (target.members) |target_member| {
            const source_member = self.callableSetMemberPayloadById(source, target_member.member) orelse continue;
            if (source_member.payload_ty == null or target_member.payload_ty == null) continue;
            changed = (try self.publishForPayloadPair(
                source_member.payload_ty.?,
                source_member.payload_ty_key.?,
                target_member.payload_ty.?,
                target_member.payload_ty_key.?,
                provenance,
                visited,
            )) or changed;
        }
        return changed;
    }

    fn publishErasedPayloadPair(
        self: *FiniteErasedAdapterDemandPublisher,
        source: repr.SessionExecutableErasedFnPayload,
        target: repr.SessionExecutableErasedFnPayload,
        provenance: []const repr.BoxErasureProvenance,
        visited: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        if (source.capture_ty == null or target.capture_ty == null) return false;
        if (source.capture_ty_key == null or target.capture_ty_key == null) {
            lambdaInvariant("lambda-solved erased payload demand publication has capture payload without capture key");
        }
        return try self.publishForPayloadPair(
            source.capture_ty.?,
            source.capture_ty_key.?,
            target.capture_ty.?,
            target.capture_ty_key.?,
            provenance,
            visited,
        );
    }

    fn recordFieldPayloadByLabel(
        self: *FiniteErasedAdapterDemandPublisher,
        record: repr.SessionExecutableRecordPayload,
        label: canonical.RecordFieldLabelId,
    ) ?repr.SessionExecutableRecordFieldPayload {
        for (record.fields) |field| {
            if (self.program.row_shapes.recordField(field.field).label == label) return field;
        }
        return null;
    }

    fn tagVariantPayloadByLabel(
        self: *FiniteErasedAdapterDemandPublisher,
        tag_union: repr.SessionExecutableTagUnionPayload,
        label: canonical.TagLabelId,
    ) ?repr.SessionExecutableTagVariantPayload {
        for (tag_union.variants) |variant| {
            if (self.program.row_shapes.tag(variant.tag).label == label) return variant;
        }
        return null;
    }

    fn tagPayloadByLogicalIndex(
        self: *FiniteErasedAdapterDemandPublisher,
        variant: repr.SessionExecutableTagVariantPayload,
        logical_index: u32,
    ) ?repr.SessionExecutableTagPayload {
        for (variant.payloads) |payload| {
            if (self.program.row_shapes.tagPayload(payload.payload).logical_index == logical_index) return payload;
        }
        return null;
    }

    fn callableSetMemberPayloadById(
        _: *FiniteErasedAdapterDemandPublisher,
        set: repr.SessionExecutableCallableSetPayload,
        member_id: repr.CallableSetMemberId,
    ) ?repr.SessionExecutableCallableSetMemberPayload {
        for (set.members) |member| {
            if (member.member == member_id) return member;
        }
        return null;
    }

    fn valueStoreFor(
        self: *FiniteErasedAdapterDemandPublisher,
        record: *const ProcBuildRecord,
    ) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(record.value_store)];
    }

    fn representationStoreFor(
        self: *FiniteErasedAdapterDemandPublisher,
        record: *const ProcBuildRecord,
    ) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
    }
};

const ProcValueOwnerErasureRequirementPlan = struct {
    key: repr.ExecutableSpecializationKey,
    provenance: []const repr.BoxErasureProvenance,
};

fn procedureBoundaryErasureRequirementPlan(
    program: *Program,
    records: []const ProcBuildRecord,
    instance: repr.ProcRepresentationInstanceId,
) Allocator.Error!?ProcValueOwnerErasureRequirementPlan {
    const index = @intFromEnum(instance);
    if (index >= records.len) {
        lambdaInvariant("lambda-solved boundary erasure requirement referenced out-of-range procedure instance");
    }
    const record = records[index];
    return switch (record.owner) {
        .proc_value => |owner| try procValueOwnerErasureRequirementPlan(program, records, record, owner.owner, owner.value),
        .finite_erased_adapter_member => |member| blk: {
            const store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
            const plan = store.callableEmissionPlan(member.emission_plan);
            const erase = switch (plan) {
                .erase_finite_set => |erase| erase,
                else => lambdaInvariant("lambda-solved finite adapter boundary requirement referenced non-erased emission plan"),
            };
            if (member.member_index >= erase.member_targets.len) {
                lambdaInvariant("lambda-solved finite adapter boundary requirement member index is out of range");
            }
            if (erase.provenance.len == 0) {
                lambdaInvariant("lambda-solved finite adapter boundary requirement has no Box(T) provenance");
            }
            break :blk .{
                .key = erase.member_targets[member.member_index],
                .provenance = erase.provenance,
            };
        },
        .finite_erased_adapter_demand_member => |member| blk: {
            const store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
            const demand = store.finiteErasedAdapterDemand(member.demand);
            if (member.member_index >= demand.member_targets.len) {
                lambdaInvariant("lambda-solved finite adapter demand boundary requirement member index is out of range");
            }
            if (demand.provenance.len == 0) {
                lambdaInvariant("lambda-solved finite adapter demand boundary requirement has no Box(T) provenance");
            }
            break :blk .{
                .key = demand.member_targets[member.member_index],
                .provenance = demand.provenance,
            };
        },
        .executable_erased_adapter_member,
        .sealed_erased_adapter_member,
        .root,
        .direct_call,
        .recursive_group_member,
        => null,
    };
}

fn procValueOwnerErasureRequirementPlan(
    program: *Program,
    records: []const ProcBuildRecord,
    record: ProcBuildRecord,
    owner_instance: repr.ProcRepresentationInstanceId,
    owner_value: repr.ValueInfoId,
) Allocator.Error!?ProcValueOwnerErasureRequirementPlan {
    const owner_index = @intFromEnum(owner_instance);
    if (owner_index >= records.len) {
        lambdaInvariant("lambda-solved proc-value erasure requirement owner referenced an out-of-range procedure instance");
    }
    const owner_record = records[owner_index];
    if (owner_record.solve_session != record.solve_session) {
        lambdaInvariant("lambda-solved proc-value erasure requirement crossed solve sessions");
    }
    const owner_value_store = &program.value_stores.items[@intFromEnum(owner_record.value_store)];
    const value_index = @intFromEnum(owner_value);
    if (value_index >= owner_value_store.values.items.len) {
        lambdaInvariant("lambda-solved proc-value erasure requirement referenced an out-of-range value");
    }
    const callable = owner_value_store.values.items[value_index].callable orelse return null;
    const store = &program.solve_sessions.items[@intFromEnum(owner_record.solve_session)].representation_store;
    return switch (store.callableEmissionPlan(callable.emission_plan)) {
        .finite,
        .pending_proc_value,
        => null,
        .already_erased => |erased| blk: {
            if (erased.provenance.len == 0) {
                lambdaInvariant("lambda-solved already-erased proc-value requirement has no Box(T) provenance");
            }
            break :blk .{
                .key = try alreadyErasedProcValueExecutableSpecializationKey(program, store, record, erased),
                .provenance = erased.provenance,
            };
        },
        .erase_proc_value => |erase| blk: {
            if (erase.target_instance != record.representation_instance) {
                lambdaInvariant("lambda-solved proc-value erased requirement target differs from owner instance");
            }
            if (erase.provenance.len == 0) {
                lambdaInvariant("lambda-solved proc-value erased requirement has no Box(T) provenance");
            }
            break :blk .{
                .key = erase.executable_specialization_key,
                .provenance = erase.provenance,
            };
        },
        .erase_finite_set => |erase| blk: {
            if (erase.provenance.len == 0) {
                lambdaInvariant("lambda-solved finite proc-value erased requirement has no Box(T) provenance");
            }
            const construction_id = callable.construction_plan orelse {
                lambdaInvariant("lambda-solved finite proc-value erased requirement has no construction plan");
            };
            const construction = store.callableConstructionPlan(construction_id);
            const descriptor = store.callableSetDescriptor(erase.adapter.callable_set_key) orelse {
                lambdaInvariant("lambda-solved finite proc-value erased requirement has no callable-set descriptor");
            };
            if (descriptor.members.len != erase.member_targets.len) {
                lambdaInvariant("lambda-solved finite proc-value erased requirement target count differs from descriptor");
            }
            for (descriptor.members, erase.member_targets) |member, target_key| {
                if (member.member != construction.selected_member) continue;
                if (member.target_instance != record.representation_instance) {
                    lambdaInvariant("lambda-solved finite proc-value erased requirement selected member targets another instance");
                }
                if (!canonical.mirProcedureRefEql(member.source_proc, record.proc)) {
                    lambdaInvariant("lambda-solved finite proc-value erased requirement selected member targets another procedure");
                }
                break :blk .{
                    .key = target_key,
                    .provenance = erase.provenance,
                };
            }
            lambdaInvariant("lambda-solved finite proc-value erased requirement selected member is missing from descriptor");
        },
    };
}

fn alreadyErasedProcValueExecutableSpecializationKey(
    program: *Program,
    store: *const repr.RepresentationStore,
    record: ProcBuildRecord,
    erased: repr.AlreadyErasedCallablePlan,
) Allocator.Error!repr.ExecutableSpecializationKey {
    if (!record.has_public_roots) {
        lambdaInvariant("lambda-solved already-erased proc-value target has no public roots");
    }
    if (!repr.canonicalTypeKeyEql(record.proc.callable.source_fn_ty, erased.sig_key.source_fn_ty)) {
        lambdaInvariant("lambda-solved already-erased proc-value source type differs from target procedure");
    }
    const abi = store.erased_fn_abis.abiFor(erased.sig_key.abi) orelse {
        lambdaInvariant("lambda-solved already-erased proc-value referenced missing ABI");
    };
    return .{
        .base = record.proc.proc.proc_base,
        .requested_fn_ty = erased.sig_key.source_fn_ty,
        .exec_arg_tys = abi.arg_exec_keys,
        .exec_ret_ty = abi.ret_exec_key,
        .callable_repr_mode = .direct,
        .capture_shape_key = try repr.captureShapeKeyForValues(
            program.allocator,
            &program.canonical_names,
            &program.row_shapes,
            &program.types,
            store,
            &program.value_stores.items[@intFromEnum(record.value_store)],
            record.public_roots.captures,
        ),
    };
}

fn appendRequireBoxErasedIfMissing(
    store: *repr.RepresentationStore,
    payload_root: repr.RepRootId,
    provenance: repr.BoxErasureProvenance,
) Allocator.Error!bool {
    for (store.representation_requirements.items) |requirement| {
        const existing = switch (requirement) {
            .require_box_erased => |erased| erased,
        };
        if (existing.payload_root == payload_root and boxErasureProvenanceEql(existing.provenance, provenance)) {
            return false;
        }
    }
    _ = try store.appendRepresentationRequirement(.{ .require_box_erased = .{
        .payload_root = payload_root,
        .provenance = provenance,
    } });
    return true;
}

fn executableTypeKeyContainsErasedFnInStore(
    allocator: Allocator,
    store: *const repr.RepresentationStore,
    key: canonical.CanonicalExecValueTypeKey,
) Allocator.Error!bool {
    const ref = store.session_executable_type_payloads.refForKey(key) orelse {
        lambdaInvariant("lambda-solved proc-value erased requirement target key has no executable payload");
    };
    var visited = std.AutoHashMap(repr.SessionExecutableTypePayloadId, void).init(allocator);
    defer visited.deinit();
    return try executablePayloadContainsErasedFnInStore(store, ref.payload, &visited);
}

fn executablePayloadContainsErasedFnInStore(
    store: *const repr.RepresentationStore,
    payload_id: repr.SessionExecutableTypePayloadId,
    visited: *std.AutoHashMap(repr.SessionExecutableTypePayloadId, void),
) Allocator.Error!bool {
    if (visited.contains(payload_id)) return false;
    try visited.put(payload_id, {});
    const payload = store.session_executable_type_payloads.get(payload_id);
    switch (payload) {
        .erased_fn => return true,
        .record => |record| {
            for (record.fields) |field| {
                if (try executablePayloadContainsErasedFnInStore(store, field.ty.payload, visited)) return true;
            }
            return false;
        },
        .tuple => |items| {
            for (items) |item| {
                if (try executablePayloadContainsErasedFnInStore(store, item.ty.payload, visited)) return true;
            }
            return false;
        },
        .tag_union => |tag_union| {
            for (tag_union.variants) |variant| {
                for (variant.payloads) |tag_payload| {
                    if (try executablePayloadContainsErasedFnInStore(store, tag_payload.ty.payload, visited)) return true;
                }
            }
            return false;
        },
        .list => |list| return try executablePayloadContainsErasedFnInStore(store, list.ty.payload, visited),
        .box => |box| return try executablePayloadContainsErasedFnInStore(store, box.ty.payload, visited),
        .nominal => |nominal| return try executablePayloadContainsErasedFnInStore(store, nominal.backing.payload, visited),
        .callable_set => |callable_set| {
            for (callable_set.members) |member| {
                const member_payload = member.payload_ty orelse continue;
                if (try executablePayloadContainsErasedFnInStore(store, member_payload.payload, visited)) return true;
            }
            return false;
        },
        .recursive_ref => |recursive| return try executablePayloadContainsErasedFnInStore(store, recursive, visited),
        .pending => lambdaInvariant("lambda-solved proc-value erased requirement referenced pending executable payload"),
        .primitive,
        .vacant_callable_slot,
        => return false,
    }
}

fn sealProcRepresentationInstances(
    program: *Program,
    records: []const ProcBuildRecord,
    executable_synthetic_procs: []const ids.ExecutableSyntheticProc,
    sealed_erased_adapter_members: []const SealedErasedAdapterMemberReservation,
) Allocator.Error!void {
    for (records) |record| {
        const session_index = @intFromEnum(record.solve_session);
        const value_store_index = @intFromEnum(record.value_store);
        const executable_key = switch (record.kind) {
            .normal => switch (record.owner) {
                .executable_erased_adapter_member => |member| blk: {
                    const synthetic = executable_synthetic_procs[member.synthetic_index];
                    const erased = switch (synthetic.body) {
                        .erased_promoted_wrapper => |erased| erased,
                    };
                    const finite = switch (erased.sealed.code) {
                        .direct_proc => lambdaInvariant("lambda-solved executable adapter member seal reached direct erased code"),
                        .finite_adapter => |finite| finite,
                    };
                    if (member.member_index >= finite.members.len) {
                        lambdaInvariant("lambda-solved executable adapter member seal index is out of range");
                    }
                    break :blk try executableAdapterMemberSpecializationKey(
                        program.allocator,
                        erased.executable_signature,
                        finite.members[member.member_index].target_key,
                    );
                },
                .sealed_erased_adapter_member => |member| blk: {
                    if (member >= sealed_erased_adapter_members.len) {
                        lambdaInvariant("lambda-solved sealed adapter member seal index is out of range");
                    }
                    break :blk try repr.cloneExecutableSpecializationKey(
                        program.allocator,
                        sealed_erased_adapter_members[member].target_key,
                    );
                },
                .finite_erased_adapter_member => |member| blk: {
                    const store = &program.solve_sessions.items[session_index].representation_store;
                    const plan = store.callableEmissionPlan(member.emission_plan);
                    const erase = switch (plan) {
                        .erase_finite_set => |erase| erase,
                        else => lambdaInvariant("lambda-solved finite adapter member seal referenced non-erased emission plan"),
                    };
                    if (member.member_index >= erase.member_targets.len) {
                        lambdaInvariant("lambda-solved finite adapter member seal index is out of range");
                    }
                    break :blk try repr.cloneExecutableSpecializationKey(
                        program.allocator,
                        erase.member_targets[member.member_index],
                    );
                },
                .finite_erased_adapter_demand_member => |member| blk: {
                    const store = &program.solve_sessions.items[session_index].representation_store;
                    const demand = store.finiteErasedAdapterDemand(member.demand);
                    if (member.member_index >= demand.member_targets.len) {
                        lambdaInvariant("lambda-solved finite adapter demand member seal index is out of range");
                    }
                    break :blk try repr.cloneExecutableSpecializationKey(
                        program.allocator,
                        demand.member_targets[member.member_index],
                    );
                },
                .proc_value => |owner| blk: {
                    if (owner.forced_target) |target| {
                        break :blk try repr.cloneExecutableSpecializationKey(program.allocator, target.key);
                    }
                    if (try executableSpecializationKeyForProcValueOwner(
                        program,
                        records,
                        record,
                        owner.owner,
                        owner.value,
                    )) |key| break :blk key;
                    break :blk try repr.executableSpecializationKeyForProc(
                        program.allocator,
                        &program.canonical_names,
                        &program.row_shapes,
                        &program.types,
                        &program.solve_sessions.items[session_index].representation_store,
                        &program.value_stores.items[value_store_index],
                        record.proc,
                        record.public_roots,
                    );
                },
                else => try repr.executableSpecializationKeyForProc(
                    program.allocator,
                    &program.canonical_names,
                    &program.row_shapes,
                    &program.types,
                    &program.solve_sessions.items[session_index].representation_store,
                    &program.value_stores.items[value_store_index],
                    record.proc,
                    record.public_roots,
                ),
            },
            .executable_synthetic => |synthetic_index| blk: {
                const synthetic = executable_synthetic_procs[synthetic_index];
                const key = switch (synthetic.body) {
                    .erased_promoted_wrapper => |erased| erased.executable_signature.specialization_key,
                };
                break :blk try repr.cloneExecutableSpecializationKey(program.allocator, key);
            },
        };
        const boundary_payloads: ?repr.ProcBoundaryExecutablePayloads = switch (record.kind) {
            .normal => switch (record.owner) {
                .executable_erased_adapter_member => |member| blk: {
                    const synthetic = executable_synthetic_procs[member.synthetic_index];
                    break :blk .{
                        .artifact = synthetic.artifact,
                        .payloads = synthetic.executable_type_payloads,
                        .promoted_wrapper = synthetic.source_proc,
                    };
                },
                .sealed_erased_adapter_member => |member| blk: {
                    if (member >= sealed_erased_adapter_members.len) {
                        lambdaInvariant("lambda-solved sealed adapter member boundary payload index is out of range");
                    }
                    const dependency = sealed_erased_adapter_members[member];
                    break :blk .{
                        .artifact = dependency.artifact,
                        .payloads = dependency.payloads,
                    };
                },
                .proc_value => |owner| blk: {
                    const target = owner.forced_target orelse break :blk null;
                    break :blk .{
                        .artifact = target.artifact,
                        .payloads = target.payloads,
                        .promoted_wrapper = target.promoted_wrapper,
                    };
                },
                else => null,
            },
            .executable_synthetic => |synthetic_index| blk: {
                const synthetic = executable_synthetic_procs[synthetic_index];
                break :blk .{
                    .artifact = synthetic.artifact,
                    .payloads = synthetic.executable_type_payloads,
                    .promoted_wrapper = synthetic.source_proc,
                };
            },
        };
        var boundary_provenance = try cloneBoundaryProvenanceForSealedInstance(
            program,
            records,
            record,
            executable_synthetic_procs,
            sealed_erased_adapter_members,
        );
        errdefer if (boundary_provenance.len > 0) program.allocator.free(boundary_provenance);
        try program.proc_instances.append(program.allocator, .{
            .proc = record.proc,
            .executable_specialization_key = executable_key,
            .solve_session = record.solve_session,
            .value_store = record.value_store,
            .public_roots = record.public_roots,
            .boundary_payloads = boundary_payloads,
            .boundary_provenance = boundary_provenance,
            .materialized = record.materialized,
        });
        boundary_provenance = &.{};
        switch (record.kind) {
            .normal => if (record.materialized) try program.procs.append(program.allocator, .{
                .proc = record.proc,
                .body = record.body,
                .representation_instance = record.representation_instance,
            }),
            .executable_synthetic => |synthetic_index| try program.executable_synthetic_proc_instances.append(program.allocator, .{
                .source_proc = record.proc,
                .synthetic_index = synthetic_index,
                .representation_instance = record.representation_instance,
            }),
        }
    }
}

fn cloneBoundaryProvenanceForSealedInstance(
    program: *Program,
    records: []const ProcBuildRecord,
    record: ProcBuildRecord,
    executable_synthetic_procs: []const ids.ExecutableSyntheticProc,
    sealed_erased_adapter_members: []const SealedErasedAdapterMemberReservation,
) Allocator.Error![]const repr.BoxErasureProvenance {
    return switch (record.kind) {
        .executable_synthetic => |synthetic_index| blk: {
            const synthetic = executable_synthetic_procs[synthetic_index];
            break :blk try cloneSingleBoundaryProvenance(program.allocator, .{ .promoted_wrapper = synthetic.source_proc });
        },
        .normal => switch (record.owner) {
            .executable_erased_adapter_member => |member| blk: {
                const synthetic = executable_synthetic_procs[member.synthetic_index];
                break :blk try cloneSingleBoundaryProvenance(program.allocator, .{ .promoted_wrapper = synthetic.source_proc });
            },
            .sealed_erased_adapter_member => |member| blk: {
                if (member >= sealed_erased_adapter_members.len) {
                    lambdaInvariant("lambda-solved sealed adapter boundary provenance index is out of range");
                }
                const provenance = sealed_erased_adapter_members[member].provenance;
                if (provenance.len == 0) {
                    lambdaInvariant("lambda-solved sealed adapter boundary provenance is empty");
                }
                break :blk try program.allocator.dupe(repr.BoxErasureProvenance, provenance);
            },
            .proc_value => |owner| blk: {
                if (owner.forced_target) |target| {
                    const wrapper = target.promoted_wrapper orelse break :blk &.{};
                    break :blk try cloneSingleBoundaryProvenance(program.allocator, .{ .promoted_wrapper = wrapper });
                }
                const plan = try procValueOwnerErasureRequirementPlan(
                    program,
                    records,
                    record,
                    owner.owner,
                    owner.value,
                ) orelse break :blk &.{};
                if (plan.provenance.len == 0) break :blk &.{};
                break :blk try program.allocator.dupe(repr.BoxErasureProvenance, plan.provenance);
            },
            .finite_erased_adapter_member => |member| blk: {
                const store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
                const plan = store.callableEmissionPlan(member.emission_plan);
                const erase = switch (plan) {
                    .erase_finite_set => |erase| erase,
                    else => lambdaInvariant("lambda-solved finite adapter boundary provenance referenced non-erased emission plan"),
                };
                if (erase.provenance.len == 0) {
                    lambdaInvariant("lambda-solved finite adapter boundary provenance is empty");
                }
                break :blk try program.allocator.dupe(repr.BoxErasureProvenance, erase.provenance);
            },
            .finite_erased_adapter_demand_member => |member| blk: {
                const store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
                const demand = store.finiteErasedAdapterDemand(member.demand);
                if (demand.provenance.len == 0) {
                    lambdaInvariant("lambda-solved finite adapter demand boundary provenance is empty");
                }
                break :blk try program.allocator.dupe(repr.BoxErasureProvenance, demand.provenance);
            },
            .root,
            .direct_call,
            .recursive_group_member,
            => &.{},
        },
    };
}

fn cloneSingleBoundaryProvenance(
    allocator: Allocator,
    provenance: repr.BoxErasureProvenance,
) Allocator.Error![]const repr.BoxErasureProvenance {
    const out = try allocator.alloc(repr.BoxErasureProvenance, 1);
    out[0] = provenance;
    return out;
}

fn executableAdapterMemberSpecializationKey(
    allocator: Allocator,
    signature: checked_artifact.ErasedPromotedProcedureExecutableSignature,
    target: canonical.ExecutableSpecializationKey,
) Allocator.Error!canonical.ExecutableSpecializationKey {
    if (target.exec_arg_tys.len != signature.specialization_key.exec_arg_tys.len) {
        lambdaInvariant("lambda-solved executable adapter member target arity differs from promoted wrapper signature");
    }
    if (!repr.canonicalTypeKeyEql(target.requested_fn_ty, signature.source_fn_ty)) {
        lambdaInvariant("lambda-solved executable adapter member target source function type differs from promoted wrapper signature");
    }
    return .{
        .base = target.base,
        .requested_fn_ty = target.requested_fn_ty,
        .exec_arg_tys = try cloneExecValueTypeKeySlice(allocator, signature.specialization_key.exec_arg_tys),
        .exec_ret_ty = signature.specialization_key.exec_ret_ty,
        .callable_repr_mode = target.callable_repr_mode,
        .capture_shape_key = target.capture_shape_key,
    };
}

fn executableSpecializationKeyForProcValueOwner(
    program: *Program,
    records: []const ProcBuildRecord,
    record: ProcBuildRecord,
    owner_instance: repr.ProcRepresentationInstanceId,
    owner_value: repr.ValueInfoId,
) Allocator.Error!?repr.ExecutableSpecializationKey {
    const owner_index = @intFromEnum(owner_instance);
    if (owner_index >= records.len) {
        lambdaInvariant("lambda-solved proc-value owner referenced an out-of-range procedure instance");
    }
    const owner_record = records[owner_index];
    if (owner_record.solve_session != record.solve_session) {
        lambdaInvariant("lambda-solved proc-value owner crossed solve sessions");
    }
    const owner_value_store = &program.value_stores.items[@intFromEnum(owner_record.value_store)];
    const value_index = @intFromEnum(owner_value);
    if (value_index >= owner_value_store.values.items.len) {
        lambdaInvariant("lambda-solved proc-value owner referenced an out-of-range value");
    }
    const callable = owner_value_store.values.items[value_index].callable orelse {
        lambdaInvariant("lambda-solved proc-value owner value has no callable emission plan");
    };
    const store = &program.solve_sessions.items[@intFromEnum(owner_record.solve_session)].representation_store;
    return switch (store.callableEmissionPlan(callable.emission_plan)) {
        .finite => null,
        .pending_proc_value => lambdaInvariant("lambda-solved proc-value owner reached sealing with pending callable emission"),
        .already_erased => |erased| blk: {
            if (erased.provenance.len == 0) {
                lambdaInvariant("lambda-solved already-erased proc-value owner has no Box(T) provenance");
            }
            const key = try alreadyErasedProcValueExecutableSpecializationKey(program, store, record, erased);
            break :blk try repr.cloneExecutableSpecializationKey(program.allocator, key);
        },
        .erase_proc_value => |erase| blk: {
            if (erase.target_instance != record.representation_instance) {
                lambdaInvariant("lambda-solved proc-value erasure plan target differs from proc-value owner instance");
            }
            break :blk try repr.cloneExecutableSpecializationKey(program.allocator, erase.executable_specialization_key);
        },
        .erase_finite_set => |erase| blk: {
            const construction_id = callable.construction_plan orelse {
                lambdaInvariant("lambda-solved erased finite proc-value owner has no construction plan");
            };
            const construction = store.callableConstructionPlan(construction_id);
            const descriptor = store.callableSetDescriptor(erase.adapter.callable_set_key) orelse {
                lambdaInvariant("lambda-solved erased finite proc-value owner has no callable-set descriptor");
            };
            if (descriptor.members.len != erase.member_targets.len) {
                lambdaInvariant("lambda-solved erased finite proc-value owner target count differs from descriptor");
            }
            for (descriptor.members, erase.member_targets) |member, target_key| {
                if (member.member != construction.selected_member) continue;
                if (member.target_instance != record.representation_instance) {
                    lambdaInvariant("lambda-solved erased finite proc-value owner selected member targets another instance");
                }
                if (!canonical.mirProcedureRefEql(member.source_proc, record.proc)) {
                    lambdaInvariant("lambda-solved erased finite proc-value owner selected member targets another procedure");
                }
                break :blk try repr.cloneExecutableSpecializationKey(program.allocator, target_key);
            }
            lambdaInvariant("lambda-solved erased finite proc-value owner selected member is missing from descriptor");
        },
    };
}

fn appendCrossProcedureRepresentationEdges(
    program: *Program,
    records: []const ProcBuildRecord,
    include_proc_value_edges: bool,
) Allocator.Error!bool {
    var changed = false;
    for (records) |*record| {
        var linker = CrossProcedureRepresentationLinker{
            .program = program,
            .records = records,
            .record = record,
            .representation_store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store,
            .value_store = &program.value_stores.items[@intFromEnum(record.value_store)],
        };
        if (try linker.appendCallSiteEdges()) changed = true;
        if (include_proc_value_edges) try linker.appendProcValueEdges();
    }
    return changed;
}

const CrossProcedureRepresentationLinker = struct {
    program: *Program,
    records: []const ProcBuildRecord,
    record: *const ProcBuildRecord,
    representation_store: *repr.RepresentationStore,
    value_store: *repr.ValueInfoStore,

    const PendingCallValueEdgeProgress = struct {
        changed: bool = false,
        resolved: bool = false,
    };

    fn appendCallSiteEdges(self: *CrossProcedureRepresentationLinker) Allocator.Error!bool {
        var changed = false;
        for (self.value_store.call_sites.items) |*call_site| {
            if (!self.value_store.callSiteSourceMatchBranchReachable(call_site.*)) continue;
            if (call_site.representation_edges_resolved) continue;
            const dispatch = call_site.dispatch orelse {
                const callee = call_site.callee orelse lambdaInvariant("lambda-solved unresolved call site has no callee");
                const progress = try self.appendPendingCallValueEdges(call_site, callee);
                if (progress.changed) changed = true;
                if (progress.resolved) {
                    call_site.representation_edges_resolved = true;
                }
                continue;
            };
            switch (dispatch) {
                .call_proc => |target| {
                    _ = try self.appendDirectCallEdges(call_site.*, target);
                    call_site.representation_edges_resolved = true;
                    changed = true;
                },
                .call_value_finite => |plan_id| {
                    const plan = self.value_store.callValueFiniteDispatchPlan(plan_id);
                    for (self.value_store.sliceCallValueFiniteDispatchBranches(plan.branches)) |branch| {
                        _ = try self.appendDirectCallEdges(call_site.*, branch.target_instance);
                    }
                    call_site.representation_edges_resolved = true;
                    changed = true;
                },
                .call_value_erased => call_site.representation_edges_resolved = true,
                .pending_comptime_dependency_call => call_site.representation_edges_resolved = true,
            }
        }
        return changed;
    }

    fn appendDirectCallEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: repr.CallSiteInfo,
        target_id: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!bool {
        var changed = false;
        const target = self.procInstance(target_id);
        const args = self.value_store.sliceValueSpan(call_site.args);
        const params = self.valueStoreFor(target).sliceValueSpan(target.public_roots.params);
        if (args.len != params.len) {
            lambdaInvariant("lambda-solved call_proc representation edge arity differs from target params");
        }
        for (args, params, 0..) |arg, param, i| {
            changed = (try self.appendRepresentationEdgeIfMissing(.{
                .from = .{ .local = self.valueRoot(arg) },
                .to = .{ .procedure_public = self.publicRootRef(target_id, target, param) },
                .kind = .{ .call_arg = @intCast(i) },
            })) or changed;
        }
        changed = (try self.appendRepresentationEdgeIfMissing(.{
            .from = .{ .procedure_function_root = self.publicFunctionRootRef(target_id, target.public_roots.function_root) },
            .to = .{ .local = call_site.requested_fn_root },
            .kind = .value_alias,
        })) or changed;
        changed = (try self.appendRepresentationEdgeIfMissing(.{
            .from = .{ .procedure_public = self.publicRootRef(target_id, target, target.public_roots.ret) },
            .to = .{ .local = self.valueRoot(call_site.result) },
            .kind = .call_result,
        })) or changed;
        return changed;
    }

    fn appendFiniteCallValueEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: repr.CallSiteInfo,
        key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!bool {
        const descriptor = self.representation_store.callableSetDescriptor(key) orelse {
            lambdaInvariant("lambda-solved finite call_value representation edge has no callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite call_value representation edge reached empty callable-set descriptor");
        }
        var changed = false;
        for (descriptor.members) |member| {
            changed = (try self.appendDirectCallEdges(call_site, member.target_instance)) or changed;
        }
        return changed;
    }

    fn appendCallableGroupWorksetCallValueEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: repr.CallSiteInfo,
        group: repr.RepresentationGroupId,
    ) Allocator.Error!?bool {
        const members = self.representation_store.callableGroupWorkMembers(group);
        if (members.len == 0) return null;
        var changed = false;
        for (members) |member| {
            changed = (try self.appendDirectCallEdges(call_site, member.target_instance)) or changed;
        }
        return changed;
    }

    fn appendPendingCallValueEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: *repr.CallSiteInfo,
        callee: repr.ValueInfoId,
    ) Allocator.Error!PendingCallValueEdgeProgress {
        const value_info = self.value_store.values.items[@intFromEnum(callee)];
        const callable = value_info.callable orelse {
            if (self.valueHasFunctionType(value_info.logical_ty)) {
                if (!self.representation_store.hasPublishedSolvedGroups()) return .{};
                const group = self.callableGroupForValueInfo(value_info, null);
                if (try self.appendCallableGroupWorksetCallValueEdges(call_site.*, group)) |edge_changed| {
                    return .{ .changed = edge_changed, .resolved = false };
                }
                return .{};
            }
            lambdaInvariant("lambda-solved pending call_value has non-callable callee");
        };
        switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .finite => |key| {
                _ = try self.appendFiniteCallValueEdges(call_site.*, key);
                return .{ .changed = true, .resolved = true };
            },
            .pending_proc_value => {
                if (!self.representation_store.hasPublishedSolvedGroups()) return .{};
                const group = self.callableGroupForValueInfo(value_info, callable);
                if (try self.appendCallableGroupWorksetCallValueEdges(call_site.*, group)) |edge_changed| {
                    return .{ .changed = edge_changed, .resolved = false };
                }
                return .{};
            },
            .already_erased => |erased| {
                call_site.dispatch = .{ .call_value_erased = erased.sig_key };
                return .{ .changed = true, .resolved = true };
            },
            .erase_finite_set => |erase| {
                call_site.dispatch = .{ .call_value_erased = erase.adapter.erased_fn_sig_key };
                return .{ .changed = true, .resolved = true };
            },
            .erase_proc_value => |erase| {
                call_site.dispatch = .{ .call_value_erased = erase.erased_fn_sig_key };
                return .{ .changed = true, .resolved = true };
            },
        }
    }

    fn valueHasFunctionType(
        self: *CrossProcedureRepresentationLinker,
        ty: Type.TypeVarId,
    ) bool {
        const root = self.program.types.unlinkConst(ty);
        return switch (self.program.types.getNode(root)) {
            .content => |content| switch (content) {
                .func => true,
                else => false,
            },
            else => false,
        };
    }

    fn callableGroupForValueInfo(
        self: *CrossProcedureRepresentationLinker,
        value_info: repr.ValueInfo,
        callable: ?repr.CallableValueInfo,
    ) repr.RepresentationGroupId {
        if (callable) |info| return self.representation_store.groupForRoot(info.callable_root);
        const callable_root = self.representation_store.solvedStructuralChildRoot(value_info.root, .function_callable) orelse value_info.root;
        return self.representation_store.groupForRoot(callable_root);
    }

    fn appendProcValueEdges(self: *CrossProcedureRepresentationLinker) Allocator.Error!void {
        for (self.value_store.values.items) |value_info| {
            const callable = value_info.callable orelse continue;
            const source = switch (callable.source) {
                .proc_value => |source| source,
                else => continue,
            };
            const target_id = source.target_instance;
            const target = self.procInstance(target_id);
            const source_captures = source.captures;
            const target_captures = self.valueStoreFor(target).sliceValueSpan(target.public_roots.captures);
            if (source_captures.len != target_captures.len) {
                lambdaInvariant("lambda-solved proc_value representation edge capture arity differs from target captures");
            }
            const target_params = self.valueStoreFor(target).sliceValueSpan(target.public_roots.params);
            for (target_params, 0..) |target_param, i| {
                const arg_root = self.structuralChildRoot(callable.whole_function_root, .{ .function_arg = @intCast(i) });
                _ = try self.appendRepresentationEdgeIfMissing(.{
                    .from = .{ .procedure_public = self.publicRootRef(target_id, target, target_param) },
                    .to = .{ .local = arg_root },
                    .kind = .{ .call_arg = @intCast(i) },
                });
            }
            const return_root = self.structuralChildRoot(callable.whole_function_root, .function_return);
            _ = try self.appendRepresentationEdgeIfMissing(.{
                .from = .{ .local = return_root },
                .to = .{ .procedure_public = self.publicRootRef(target_id, target, target.public_roots.ret) },
                .kind = .call_result,
            });
            for (source_captures, target_captures, 0..) |source_capture, target_capture, raw_slot| {
                try self.publishProcedureCaptureSource(target_id, @intCast(raw_slot), source_capture);
                _ = try self.appendRepresentationEdgeIfMissing(.{
                    .from = .{ .local = self.valueRoot(source_capture) },
                    .to = .{ .procedure_public = self.publicRootRef(target_id, target, target_capture) },
                    .kind = .capture_value,
                });
            }
        }
    }

    fn appendRepresentationEdgeIfMissing(
        self: *CrossProcedureRepresentationLinker,
        edge: repr.RepresentationEdge,
    ) Allocator.Error!bool {
        for (self.representation_store.representation_edges.items) |existing| {
            if (representationEdgeEql(existing, edge)) return false;
        }
        _ = try self.representation_store.appendRepresentationEdge(edge);
        return true;
    }

    fn structuralChildRoot(
        self: *CrossProcedureRepresentationLinker,
        parent: repr.RepRootId,
        kind: repr.RepresentationChildKind,
    ) repr.RepRootId {
        for (self.representation_store.representation_edges.items) |edge| {
            const from = switch (edge.from) {
                .local => |local| local,
                .procedure_public,
                .procedure_function_root,
                => continue,
            };
            if (from != parent) continue;
            const child_kind = switch (edge.kind) {
                .child => |child| child,
                else => continue,
            };
            if (!representationChildKindMatches(child_kind, kind)) continue;
            return switch (edge.to) {
                .local => |local| local,
                .procedure_public,
                .procedure_function_root,
                => lambdaInvariant("lambda-solved structural child edge targets procedure-public root"),
            };
        }
        lambdaInvariant("lambda-solved cross-procedure structural child root is missing");
    }

    fn publishProcedureCaptureSource(
        self: *CrossProcedureRepresentationLinker,
        target_instance: repr.ProcRepresentationInstanceId,
        slot: u32,
        source_value: repr.ValueInfoId,
    ) Allocator.Error!void {
        const source: ProcedureCaptureSource = .{
            .target_instance = target_instance,
            .slot = slot,
            .source_store = self.record.value_store,
            .source_value = source_value,
        };
        for (self.program.procedure_capture_sources.items) |existing| {
            if (existing.target_instance == source.target_instance and
                existing.slot == source.slot and
                existing.source_store == source.source_store and
                existing.source_value == source.source_value)
            {
                return;
            }
        }
        try self.program.procedure_capture_sources.append(self.program.allocator, source);
    }

    fn publicRootRef(
        self: *CrossProcedureRepresentationLinker,
        target_id: repr.ProcRepresentationInstanceId,
        target: *const ProcBuildRecord,
        value: repr.ValueInfoId,
    ) repr.ProcPublicRootRef {
        return .{
            .instance = target_id,
            .value = value,
            .rep_root = self.valueStoreFor(target).values.items[@intFromEnum(value)].root,
        };
    }

    fn publicFunctionRootRef(
        _: *CrossProcedureRepresentationLinker,
        target_id: repr.ProcRepresentationInstanceId,
        root: repr.RepRootId,
    ) repr.ProcPublicFunctionRootRef {
        return .{
            .instance = target_id,
            .rep_root = root,
        };
    }

    fn procInstance(
        self: *CrossProcedureRepresentationLinker,
        id: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(id);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved cross-procedure representation edge referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn valueStoreFor(
        self: *CrossProcedureRepresentationLinker,
        instance: *const ProcBuildRecord,
    ) *const repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(instance.value_store)];
    }

    fn valueRoot(self: *CrossProcedureRepresentationLinker, value: repr.ValueInfoId) repr.RepRootId {
        return self.value_store.values.items[@intFromEnum(value)].root;
    }
};

fn solveRepresentationSessions(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!void {
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        session.state = .solving;
        {
            var solver = RepresentationGroupSolver{
                .allocator = program.allocator,
                .program = program,
                .records = records,
                .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
                .session = session,
                .parents = &.{},
                .ranks = &.{},
                .groups = std.AutoHashMap(u32, repr.RepresentationGroupId).init(program.allocator),
            };
            defer solver.deinit();
            try solver.solve();
        }
    }
}

const RepresentationGroupSolver = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,
    parents: []u32,
    ranks: []u8,
    groups: std.AutoHashMap(u32, repr.RepresentationGroupId),

    fn deinit(self: *RepresentationGroupSolver) void {
        self.groups.deinit();
        if (self.ranks.len > 0) self.allocator.free(self.ranks);
        if (self.parents.len > 0) self.allocator.free(self.parents);
    }

    fn solve(self: *RepresentationGroupSolver) Allocator.Error!void {
        try self.initUnionFind();
        self.unionValueFlowEdges();
        try self.closeStructuralProjectionGroups();
        try self.assignValueGroups();
    }

    fn unionValueFlowEdges(self: *RepresentationGroupSolver) void {
        for (self.session.representation_store.representation_edges.items) |edge| {
            if (!self.edgeUnionsValueFlow(edge)) continue;
            const from = self.endpointRootInSession(edge.from) orelse continue;
            const to = self.endpointRootInSession(edge.to) orelse continue;
            _ = self.unionRoots(from, to);
        }
    }

    const StructuralProjectionKind = struct {
        tag: enum {
            function_arg,
            function_return,
            function_callable,
            record_field,
            tuple_elem,
            tag_payload,
            list_elem,
            box_payload,
            nominal_backing,
        },
        a: u32 = 0,
        b: u32 = 0,
    };

    const StructuralProjectionGroup = struct {
        parent_representative: u32,
        kind: StructuralProjectionKind,
    };

    fn closeStructuralProjectionGroups(self: *RepresentationGroupSolver) Allocator.Error!void {
        var changed = true;
        while (changed) {
            changed = false;
            var groups = std.AutoHashMap(StructuralProjectionGroup, repr.RepRootId).init(self.allocator);
            defer groups.deinit();

            for (self.session.representation_store.representation_edges.items) |edge| {
                const kind = self.structuralProjectionKind(edge.kind) orelse continue;
                const parent = self.endpointRootInSession(edge.from) orelse continue;
                const child = self.endpointRootInSession(edge.to) orelse continue;
                const parent_index = @intFromEnum(parent);
                if (parent_index >= self.parents.len) {
                    lambdaInvariant("lambda-solved representation solver reached an out-of-range structural parent root");
                }
                const group: StructuralProjectionGroup = .{
                    .parent_representative = self.find(parent_index),
                    .kind = kind,
                };
                if (groups.get(group)) |existing| {
                    if (self.unionRoots(existing, child)) changed = true;
                } else {
                    try groups.put(group, child);
                }
            }
        }
    }

    fn structuralProjectionKind(
        _: *RepresentationGroupSolver,
        kind: repr.RepresentationEdgeKind,
    ) ?StructuralProjectionKind {
        return switch (kind) {
            .child => |child| structuralProjectionKindFromChild(child),
            .value_alias,
            .value_move,
            .branch_join,
            .loop_phi,
            .mutable_version,
            .call_arg,
            .call_result,
            .capture_value,
            => null,
        };
    }

    fn structuralProjectionKindFromChild(kind: repr.RepresentationChildKind) StructuralProjectionKind {
        return switch (kind) {
            .function_arg => |index| .{
                .tag = .function_arg,
                .a = index,
            },
            .function_return => .{ .tag = .function_return },
            .function_callable => .{ .tag = .function_callable },
            .record_field => |field| .{
                .tag = .record_field,
                .a = @intFromEnum(field),
            },
            .tuple_elem => |index| .{
                .tag = .tuple_elem,
                .a = index,
            },
            .tag_payload => |payload| .{
                .tag = .tag_payload,
                .a = @intFromEnum(payload),
            },
            .list_elem => .{ .tag = .list_elem },
            .box_payload => .{ .tag = .box_payload },
            .nominal_backing => |nominal| .{
                .tag = .nominal_backing,
                .a = @intFromEnum(nominal.module_name),
                .b = @intFromEnum(nominal.type_name),
            },
        };
    }

    fn initUnionFind(self: *RepresentationGroupSolver) Allocator.Error!void {
        const len = self.session.representation_store.roots_len;
        self.parents = if (len == 0) &.{} else try self.allocator.alloc(u32, len);
        errdefer if (self.parents.len > 0) self.allocator.free(self.parents);
        self.ranks = if (len == 0) &.{} else try self.allocator.alloc(u8, len);
        errdefer if (self.ranks.len > 0) self.allocator.free(self.ranks);
        for (self.parents, 0..) |*parent, i| {
            parent.* = @intCast(i);
        }
        @memset(self.ranks, 0);
    }

    fn edgeUnionsValueFlow(_: *RepresentationGroupSolver, edge: repr.RepresentationEdge) bool {
        return switch (edge.kind) {
            .value_alias,
            .value_move,
            .branch_join,
            .loop_phi,
            .mutable_version,
            .call_arg,
            .call_result,
            .capture_value,
            => true,
            .child => false,
        };
    }

    fn endpointRootInSession(self: *RepresentationGroupSolver, endpoint: repr.RepresentationEndpoint) ?repr.RepRootId {
        return switch (endpoint) {
            .local => |root| root,
            .procedure_public => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
            .procedure_function_root => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
        };
    }

    fn recordForInstance(
        self: *RepresentationGroupSolver,
        instance: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(instance);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved representation solver referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn unionRoots(self: *RepresentationGroupSolver, a: repr.RepRootId, b: repr.RepRootId) bool {
        const a_index = @intFromEnum(a);
        const b_index = @intFromEnum(b);
        if (a_index >= self.parents.len or b_index >= self.parents.len) {
            lambdaInvariant("lambda-solved representation solver reached an out-of-range root");
        }
        const a_rep = self.find(a_index);
        const b_rep = self.find(b_index);
        if (a_rep == b_rep) return false;
        if (self.ranks[a_rep] < self.ranks[b_rep]) {
            self.parents[a_rep] = b_rep;
        } else if (self.ranks[a_rep] > self.ranks[b_rep]) {
            self.parents[b_rep] = a_rep;
        } else {
            self.parents[b_rep] = a_rep;
            self.ranks[a_rep] += 1;
        }
        return true;
    }

    fn find(self: *RepresentationGroupSolver, index: u32) u32 {
        var current = index;
        while (self.parents[current] != current) {
            current = self.parents[current];
        }
        const root = current;
        current = index;
        while (self.parents[current] != current) {
            const next = self.parents[current];
            self.parents[current] = root;
            current = next;
        }
        return root;
    }

    fn assignValueGroups(self: *RepresentationGroupSolver) Allocator.Error!void {
        self.session.representation_store.resetSolvedGroups();
        const root_count: usize = @intCast(self.session.representation_store.roots_len);
        const root_groups = try self.allocator.alloc(repr.RepresentationGroupId, root_count);
        errdefer self.allocator.free(root_groups);

        for (root_groups, 0..) |*group, raw_root| {
            const root: repr.RepRootId = @enumFromInt(@as(u32, @intCast(raw_root)));
            group.* = try self.groupForRoot(root);
        }
        try self.session.representation_store.publishRootGroups(root_groups);

        for (self.session.members) |member| {
            const record = self.recordForInstance(member);
            const value_store = &self.program.value_stores.items[@intFromEnum(record.value_store)];
            for (value_store.values.items) |*value| {
                value.solved_group = self.session.representation_store.groupForRoot(value.root);
            }
        }
    }

    fn groupForRoot(
        self: *RepresentationGroupSolver,
        root: repr.RepRootId,
    ) Allocator.Error!repr.RepresentationGroupId {
        const root_index = @intFromEnum(root);
        if (root_index >= self.parents.len) {
            lambdaInvariant("lambda-solved representation solver assigned group for out-of-range root");
        }
        const representative = self.find(root_index);
        if (self.groups.get(representative)) |existing| return existing;
        const group = self.session.representation_store.reserveGroup();
        try self.groups.put(representative, group);
        return group;
    }
};

fn propagatePendingComptimeDependencyOrigins(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!void {
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        if (!session.representation_store.hasPublishedSolvedGroups()) continue;
        {
            var propagator = PendingComptimeDependencyPropagator{
                .allocator = program.allocator,
                .program = program,
                .records = records,
                .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
                .session = session,
                .pending_groups = &.{},
            };
            defer propagator.deinit();
            try propagator.propagate();
        }
    }
}

const PendingComptimeDependencyPropagator = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,
    pending_groups: []bool,

    fn deinit(self: *PendingComptimeDependencyPropagator) void {
        if (self.pending_groups.len > 0) self.allocator.free(self.pending_groups);
    }

    fn propagate(self: *PendingComptimeDependencyPropagator) Allocator.Error!void {
        const group_count: usize = @intCast(self.session.representation_store.groups_len);
        self.pending_groups = if (group_count == 0) &.{} else try self.allocator.alloc(bool, group_count);
        @memset(self.pending_groups, false);

        var changed = self.seedGroupsFromPendingValues();
        while (changed) {
            changed = false;
            if (self.propagateAcrossRepresentationEdges()) changed = true;
            if (try self.propagateAcrossPendingCallValues()) changed = true;
        }
        self.publishPendingValuesFromGroups();
    }

    fn seedGroupsFromPendingValues(self: *PendingComptimeDependencyPropagator) bool {
        var changed = false;
        for (self.session.members) |instance_id| {
            const record = self.recordForInstance(instance_id);
            if (!record.materialized) continue;
            const value_store = self.valueStoreFor(record);
            for (value_store.values.items) |value| {
                if (!value.pending_comptime_dependency_origin) continue;
                const group = value.solved_group orelse {
                    lambdaInvariant("lambda-solved pending compile-time dependency value had no solved group");
                };
                changed = self.markGroup(group) or changed;
            }
        }
        return changed;
    }

    fn propagateAcrossRepresentationEdges(self: *PendingComptimeDependencyPropagator) bool {
        var changed = false;
        for (self.session.representation_store.representation_edges.items) |edge| {
            if (!self.edgePropagatesPendingComptimeDependency(edge)) continue;
            const from = self.endpointRootInSession(edge.from) orelse continue;
            const to = self.endpointRootInSession(edge.to) orelse continue;
            const from_group = self.session.representation_store.groupForRoot(from);
            if (!self.groupPending(from_group)) continue;
            const to_group = self.session.representation_store.groupForRoot(to);
            changed = self.markGroup(to_group) or changed;
        }
        return changed;
    }

    fn propagateAcrossPendingCallValues(self: *PendingComptimeDependencyPropagator) Allocator.Error!bool {
        var changed = false;
        for (self.session.members) |instance_id| {
            const record = self.recordForInstance(instance_id);
            if (!record.materialized) continue;
            const value_store = self.mutableValueStoreFor(record);
            for (value_store.call_sites.items) |*call_site| {
                if (!value_store.callSiteSourceMatchBranchReachable(call_site.*)) continue;
                const callee = call_site.callee orelse continue;
                const callee_info = value_store.values.items[@intFromEnum(callee)];
                const callee_group = callee_info.solved_group orelse {
                    lambdaInvariant("lambda-solved call_value callee had no solved group");
                };
                if (!self.groupPending(callee_group)) continue;

                if (call_site.dispatch) |dispatch| {
                    switch (dispatch) {
                        .pending_comptime_dependency_call => {},
                        .call_proc,
                        .call_value_finite,
                        .call_value_erased,
                        => lambdaInvariant("lambda-solved pending compile-time dependency call_value already had executable dispatch"),
                    }
                }
                call_site.dispatch = .pending_comptime_dependency_call;

                const result_info = value_store.values.items[@intFromEnum(call_site.result)];
                const result_group = result_info.solved_group orelse {
                    lambdaInvariant("lambda-solved call_value result had no solved group");
                };
                changed = self.markGroup(result_group) or changed;
            }
        }
        return changed;
    }

    fn publishPendingValuesFromGroups(self: *PendingComptimeDependencyPropagator) void {
        for (self.session.members) |instance_id| {
            const record = self.recordForInstance(instance_id);
            if (!record.materialized) continue;
            const value_store = self.mutableValueStoreFor(record);
            for (value_store.values.items) |*value| {
                const group = value.solved_group orelse {
                    lambdaInvariant("lambda-solved value had no solved group while publishing pending compile-time dependency origins");
                };
                if (self.groupPending(group)) {
                    value.pending_comptime_dependency_origin = true;
                }
            }
        }
    }

    fn groupPending(self: *const PendingComptimeDependencyPropagator, group: repr.RepresentationGroupId) bool {
        const group_index: usize = @intFromEnum(group);
        if (group_index >= self.pending_groups.len) {
            lambdaInvariant("lambda-solved pending compile-time dependency group was out of range");
        }
        return self.pending_groups[group_index];
    }

    fn markGroup(self: *PendingComptimeDependencyPropagator, group: repr.RepresentationGroupId) bool {
        const group_index: usize = @intFromEnum(group);
        if (group_index >= self.pending_groups.len) {
            lambdaInvariant("lambda-solved pending compile-time dependency group was out of range");
        }
        if (self.pending_groups[group_index]) return false;
        self.pending_groups[group_index] = true;
        return true;
    }

    fn endpointRootInSession(
        self: *PendingComptimeDependencyPropagator,
        endpoint: repr.RepresentationEndpoint,
    ) ?repr.RepRootId {
        return switch (endpoint) {
            .local => |root| root,
            .procedure_public => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
            .procedure_function_root => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
        };
    }

    fn edgePropagatesPendingComptimeDependency(
        _: *PendingComptimeDependencyPropagator,
        edge: repr.RepresentationEdge,
    ) bool {
        return switch (edge.kind) {
            .value_alias,
            .value_move,
            .branch_join,
            .loop_phi,
            .mutable_version,
            .call_arg,
            .call_result,
            .capture_value,
            => true,
            .child => true,
        };
    }

    fn recordForInstance(
        self: *const PendingComptimeDependencyPropagator,
        instance: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(instance);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved pending compile-time dependency propagation referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn valueStoreFor(
        self: *const PendingComptimeDependencyPropagator,
        record: *const ProcBuildRecord,
    ) *const repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(record.value_store)];
    }

    fn mutableValueStoreFor(
        self: *PendingComptimeDependencyPropagator,
        record: *const ProcBuildRecord,
    ) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(record.value_store)];
    }
};

fn finalizeSourceMatchBranchReachability(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!void {
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        var finalizer = SourceMatchReachabilityFinalizer{
            .allocator = program.allocator,
            .program = program,
            .records = records,
            .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
            .session = session,
        };
        try finalizer.finalize();
    }
}

const SourceMatchReachabilityFinalizer = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,

    const TagSelection = struct {
        union_shape: MonoRow.TagUnionShapeId,
        tag: MonoRow.TagId,
    };

    const SelectedTagSummary = struct {
        tags: std.ArrayList(TagSelection),
        unknown: bool = false,

        fn init() SelectedTagSummary {
            return .{ .tags = .empty };
        }

        fn unknownSummary() SelectedTagSummary {
            return .{ .tags = .empty, .unknown = true };
        }

        fn deinit(self: *SelectedTagSummary, allocator: Allocator) void {
            self.tags.deinit(allocator);
        }

        fn add(self: *SelectedTagSummary, allocator: Allocator, selection: TagSelection) Allocator.Error!void {
            if (self.unknown) return;
            for (self.tags.items) |existing| {
                if (sameTagSelection(existing, selection)) return;
            }
            try self.tags.append(allocator, selection);
        }

        fn mergeExact(self: *SelectedTagSummary, allocator: Allocator, other: SelectedTagSummary) Allocator.Error!void {
            if (self.unknown) return;
            if (other.unknown or other.tags.items.len == 0) {
                self.unknown = true;
                self.tags.clearRetainingCapacity();
                return;
            }
            for (other.tags.items) |selection| {
                try self.add(allocator, selection);
            }
        }
    };

    const SelectedTagPathStep = union(enum) {
        record_field: MonoRow.RecordFieldId,
        tuple_elem: u32,
        tag_payload: MonoRow.TagPayloadId,
        list_elem,
        nominal_backing,
    };

    fn finalize(self: *SourceMatchReachabilityFinalizer) Allocator.Error!void {
        for (self.session.members) |instance| {
            const record = self.recordForInstance(instance);
            if (!record.materialized) continue;
            switch (record.kind) {
                .normal => {},
                .executable_synthetic => continue,
            }
            const value_store = self.valueStoreFor(record);
            const def = self.program.ast.defs.items[@intFromEnum(record.body)];
            switch (def.value) {
                .fn_ => |fn_def| try self.finalizeExpr(fn_def.body, value_store),
                .val => |expr| try self.finalizeExpr(expr, value_store),
                .run => |run_def| try self.finalizeExpr(run_def.body, value_store),
                .hosted_fn => {},
            }
        }
    }

    fn finalizeExpr(
        self: *SourceMatchReachabilityFinalizer,
        expr_id: Ast.ExprId,
        value_store: *repr.ValueInfoStore,
    ) Allocator.Error!void {
        const expr = self.program.ast.exprs.items[@intFromEnum(expr_id)];
        switch (expr.data) {
            .match_ => |match_| {
                try self.finalizeExpr(match_.cond, value_store);
                const branch_ids = self.program.ast.branch_ids.items[match_.branches.start..][0..match_.branches.len];
                for (branch_ids) |branch_id| {
                    const branch = self.program.ast.branches.items[@intFromEnum(branch_id)];
                    const branch_ref = branch.source_match_branch orelse {
                        lambdaInvariant("lambda-solved source-match branch had no published branch identity");
                    };
                    const scrutinee = self.program.ast.exprs.items[@intFromEnum(match_.cond)].value_info;
                    var path = std.ArrayList(SelectedTagPathStep).empty;
                    defer path.deinit(self.allocator);
                    const reachable = try self.patternReachableAtPath(branch.pat, scrutinee, &path, value_store);
                    value_store.setSourceMatchBranchReachable(branch_ref, reachable);
                    if (!reachable) continue;
                    if (branch.guard) |guard| try self.finalizeExpr(guard, value_store);
                    try self.finalizeExpr(branch.body, value_store);
                }
            },
            .tag => |tag| for (self.program.ast.sliceTagPayloadEvalSpan(tag.eval_order)) |payload| try self.finalizeExpr(payload.value, value_store),
            .record => |record| for (self.program.ast.sliceRecordFieldEvalSpan(record.eval_order)) |field| try self.finalizeExpr(field.value, value_store),
            .nominal_reinterpret => |child| try self.finalizeExpr(child, value_store),
            .access => |access| try self.finalizeExpr(access.record, value_store),
            .structural_eq => |eq| {
                try self.finalizeExpr(eq.lhs, value_store);
                try self.finalizeExpr(eq.rhs, value_store);
            },
            .bool_not => |child| try self.finalizeExpr(child, value_store),
            .let_ => |let_| {
                try self.finalizeExpr(let_.body, value_store);
                try self.finalizeExpr(let_.rest, value_store);
            },
            .call_value => |call| {
                try self.finalizeExpr(call.func, value_store);
                for (self.program.ast.sliceExprSpan(call.args)) |arg| try self.finalizeExpr(arg, value_store);
            },
            .call_proc => |call| for (self.program.ast.sliceExprSpan(call.args)) |arg| try self.finalizeExpr(arg, value_store),
            .proc_value => |proc_value| for (self.program.ast.sliceCaptureArgSpan(proc_value.captures)) |capture| try self.finalizeExpr(capture.expr, value_store),
            .low_level => |low_level| for (self.program.ast.sliceExprSpan(low_level.args)) |arg| try self.finalizeExpr(arg, value_store),
            .if_ => |if_| {
                try self.finalizeExpr(if_.cond, value_store);
                try self.finalizeExpr(if_.then_body, value_store);
                try self.finalizeExpr(if_.else_body, value_store);
            },
            .block => |block| {
                for (self.program.ast.sliceStmtSpan(block.stmts)) |stmt| try self.finalizeStmt(stmt, value_store);
                try self.finalizeExpr(block.final_expr, value_store);
            },
            .tuple,
            .list,
            => |items| for (self.program.ast.sliceExprSpan(items)) |item| try self.finalizeExpr(item, value_store),
            .tag_payload => |payload| try self.finalizeExpr(payload.tag_union, value_store),
            .tuple_access => |access| try self.finalizeExpr(access.tuple, value_store),
            .return_ => |ret| try self.finalizeExpr(ret.expr, value_store),
            .for_ => |for_| {
                try self.finalizeExpr(for_.iterable, value_store);
                try self.finalizeExpr(for_.body, value_store);
            },
            .var_,
            .capture_ref,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .const_instance,
            .const_ref,
            .pending_callable_instance,
            .pending_local_root,
            .crash,
            .runtime_error,
            => {},
        }
    }

    fn finalizeStmt(
        self: *SourceMatchReachabilityFinalizer,
        stmt_id: Ast.StmtId,
        value_store: *repr.ValueInfoStore,
    ) Allocator.Error!void {
        const stmt = self.program.ast.stmts.items[@intFromEnum(stmt_id)];
        switch (stmt) {
            .decl => |decl| try self.finalizeExpr(decl.body, value_store),
            .var_decl => |decl| try self.finalizeExpr(decl.body, value_store),
            .reassign => |reassign| try self.finalizeExpr(reassign.body, value_store),
            .expr,
            .debug,
            .expect,
            => |expr| try self.finalizeExpr(expr, value_store),
            .return_ => |ret| try self.finalizeExpr(ret.expr, value_store),
            .for_ => |for_| {
                try self.finalizeExpr(for_.iterable, value_store);
                try self.finalizeExpr(for_.body, value_store);
            },
            .while_ => |while_| {
                try self.finalizeExpr(while_.cond, value_store);
                try self.finalizeExpr(while_.body, value_store);
            },
            .crash,
            .break_,
            => {},
        }
    }

    fn patternReachableAtPath(
        self: *SourceMatchReachabilityFinalizer,
        pat_id: Ast.PatId,
        source_value: repr.ValueInfoId,
        path: *std.ArrayList(SelectedTagPathStep),
        value_store: *const repr.ValueInfoStore,
    ) Allocator.Error!bool {
        const pat = self.program.ast.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .tag => |tag| {
                if (!try self.valuePathCanContainTag(source_value, path.items, value_store, .{
                    .union_shape = tag.union_shape,
                    .tag = tag.tag,
                })) return false;
                const payloads = self.program.ast.sliceTagPayloadPatternSpan(tag.payloads);
                for (payloads) |payload| {
                    try path.append(self.allocator, .{ .tag_payload = payload.payload });
                    defer _ = path.pop();
                    if (!try self.patternReachableAtPath(payload.pattern, source_value, path, value_store)) return false;
                }
            },
            .nominal => |child| {
                try path.append(self.allocator, .nominal_backing);
                defer _ = path.pop();
                return try self.patternReachableAtPath(child, source_value, path, value_store);
            },
            .tuple => |items| for (self.program.ast.slicePatSpan(items), 0..) |item, i| {
                try path.append(self.allocator, .{ .tuple_elem = @intCast(i) });
                defer _ = path.pop();
                if (!try self.patternReachableAtPath(item, source_value, path, value_store)) return false;
            },
            .record => |record| {
                for (self.program.ast.sliceRecordFieldPatternSpan(record.fields)) |field| {
                    try path.append(self.allocator, .{ .record_field = field.field });
                    defer _ = path.pop();
                    if (!try self.patternReachableAtPath(field.pattern, source_value, path, value_store)) return false;
                }
            },
            .list => |list| {
                for (self.program.ast.slicePatSpan(list.items)) |item| {
                    try path.append(self.allocator, .list_elem);
                    defer _ = path.pop();
                    if (!try self.patternReachableAtPath(item, source_value, path, value_store)) return false;
                }
            },
            .as => |as| return try self.patternReachableAtPath(as.pattern, source_value, path, value_store),
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .var_,
            .wildcard,
            => {},
        }
        return true;
    }

    fn valuePathCanContainTag(
        self: *SourceMatchReachabilityFinalizer,
        value: repr.ValueInfoId,
        path: []const SelectedTagPathStep,
        value_store: *const repr.ValueInfoStore,
        selection: TagSelection,
    ) Allocator.Error!bool {
        var summary = try self.selectedTagSummary(value, path, value_store, value_store.values.items.len + path.len + 1);
        defer summary.deinit(self.allocator);
        if (summary.unknown or summary.tags.items.len == 0) return true;
        for (summary.tags.items) |tag| {
            if (sameTagSelection(tag, selection)) return true;
        }
        return false;
    }

    fn selectedTagSummary(
        self: *SourceMatchReachabilityFinalizer,
        value: repr.ValueInfoId,
        path: []const SelectedTagPathStep,
        value_store: *const repr.ValueInfoStore,
        remaining: usize,
    ) Allocator.Error!SelectedTagSummary {
        if (remaining == 0) return SelectedTagSummary.unknownSummary();
        const info = value_store.values.items[@intFromEnum(value)];
        if (info.value_alias_source) |source| {
            return try self.selectedTagSummary(source, path, value_store, remaining - 1);
        }
        if (info.projection_info) |projection_id| {
            const projection = value_store.projections.items[@intFromEnum(projection_id)];
            return try self.selectedTagSummaryWithPrependedStep(
                projection.source,
                projectionStep(projection.kind),
                path,
                value_store,
                remaining - 1,
            );
        }
        if (info.nominal_backing_value) |backing| {
            if (path.len == 0) return SelectedTagSummary.unknownSummary();
            switch (path[0]) {
                .nominal_backing => return try self.selectedTagSummary(backing, path[1..], value_store, remaining - 1),
                else => {},
            }
        }
        if (info.join_info) |join_id| {
            return try self.joinSelectedTagSummary(join_id, path, value_store, remaining - 1);
        }
        switch (self.session.representation_store.rootKind(info.root)) {
            .procedure_capture => |capture| {
                return try self.procedureCaptureSelectedTagSummary(capture.instance, capture.slot, path, remaining - 1);
            },
            else => {},
        }
        if (info.aggregate) |aggregate| {
            return try self.aggregateSelectedTagSummary(aggregate, path, value_store, remaining - 1);
        }
        return SelectedTagSummary.unknownSummary();
    }

    fn selectedTagSummaryWithPrependedStep(
        self: *SourceMatchReachabilityFinalizer,
        value: repr.ValueInfoId,
        step: SelectedTagPathStep,
        suffix: []const SelectedTagPathStep,
        value_store: *const repr.ValueInfoStore,
        remaining: usize,
    ) Allocator.Error!SelectedTagSummary {
        var path = std.ArrayList(SelectedTagPathStep).empty;
        defer path.deinit(self.allocator);
        try path.append(self.allocator, step);
        try path.appendSlice(self.allocator, suffix);
        return try self.selectedTagSummary(value, path.items, value_store, remaining);
    }

    fn joinSelectedTagSummary(
        self: *SourceMatchReachabilityFinalizer,
        join_id: repr.JoinInfoId,
        path: []const SelectedTagPathStep,
        value_store: *const repr.ValueInfoStore,
        remaining: usize,
    ) Allocator.Error!SelectedTagSummary {
        const join = value_store.joins.items[@intFromEnum(join_id)];
        var result = SelectedTagSummary.init();
        var saw_input = false;
        for (value_store.sliceJoinInputSpan(join.inputs)) |input| {
            if (!joinInputReachable(input, value_store)) continue;
            var input_summary = try self.selectedTagSummary(input.value, path, value_store, remaining);
            defer input_summary.deinit(self.allocator);
            try result.mergeExact(self.allocator, input_summary);
            if (result.unknown) return result;
            saw_input = true;
        }
        if (!saw_input) result.unknown = true;
        return result;
    }

    fn procedureCaptureSelectedTagSummary(
        self: *SourceMatchReachabilityFinalizer,
        target_instance: repr.ProcRepresentationInstanceId,
        slot: u32,
        path: []const SelectedTagPathStep,
        remaining: usize,
    ) Allocator.Error!SelectedTagSummary {
        var result = SelectedTagSummary.init();
        var saw_source = false;
        for (self.program.procedure_capture_sources.items) |source| {
            if (source.target_instance != target_instance or source.slot != slot) continue;
            const source_store = self.valueStoreById(source.source_store);
            const source_info = source_store.values.items[@intFromEnum(source.source_value)];
            if (!source_store.valueSourceMatchBranchReachable(source_info)) continue;
            var source_summary = try self.selectedTagSummary(source.source_value, path, source_store, remaining);
            defer source_summary.deinit(self.allocator);
            try result.mergeExact(self.allocator, source_summary);
            if (result.unknown) return result;
            saw_source = true;
        }
        if (!saw_source) result.unknown = true;
        return result;
    }

    fn aggregateSelectedTagSummary(
        self: *SourceMatchReachabilityFinalizer,
        aggregate: repr.AggregateValueInfo,
        path: []const SelectedTagPathStep,
        value_store: *const repr.ValueInfoStore,
        remaining: usize,
    ) Allocator.Error!SelectedTagSummary {
        if (path.len == 0) {
            var summary = SelectedTagSummary.init();
            switch (aggregate) {
                .tag => |tag| try summary.add(self.allocator, .{
                    .union_shape = tag.union_shape,
                    .tag = tag.tag,
                }),
                else => summary.unknown = true,
            }
            return summary;
        }
        const child = aggregateChildValue(aggregate, path[0]) orelse return SelectedTagSummary.unknownSummary();
        return try self.selectedTagSummary(child, path[1..], value_store, remaining);
    }

    fn aggregateChildValue(
        aggregate: repr.AggregateValueInfo,
        step: SelectedTagPathStep,
    ) ?repr.ValueInfoId {
        return switch (step) {
            .record_field => |field| switch (aggregate) {
                .record => |record| for (record.fields) |field_info| {
                    if (field_info.field == field) break field_info.value;
                } else null,
                else => null,
            },
            .tuple_elem => |index| switch (aggregate) {
                .tuple => |tuple| for (tuple) |elem| {
                    if (elem.index == index) break elem.value;
                } else null,
                else => null,
            },
            .tag_payload => |payload| switch (aggregate) {
                .tag => |tag| for (tag.payloads) |payload_info| {
                    if (payload_info.payload == payload) break payload_info.value;
                } else null,
                else => null,
            },
            .list_elem => switch (aggregate) {
                .list => |list| if (list.elems.len == 1) list.elems[0].value else null,
                else => null,
            },
            .nominal_backing => null,
        };
    }

    fn joinInputReachable(input: repr.JoinInputInfo, value_store: *const repr.ValueInfoStore) bool {
        return switch (input.source) {
            .source_match_branch => |branch| value_store.sourceMatchBranchReachable(.{
                .match = branch.match,
                .branch = branch.branch,
                .alternative = branch.alternative,
            }),
            .if_branch,
            .loop_phi,
            => true,
        };
    }

    fn projectionStep(kind: repr.ProjectionKind) SelectedTagPathStep {
        return switch (kind) {
            .record_field => |field| .{ .record_field = field },
            .tuple_elem => |index| .{ .tuple_elem = index },
            .tag_payload => |payload| .{ .tag_payload = payload },
        };
    }

    fn sameTagSelection(a: TagSelection, b: TagSelection) bool {
        return a.union_shape == b.union_shape and a.tag == b.tag;
    }

    fn recordForInstance(
        self: *SourceMatchReachabilityFinalizer,
        instance: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(instance);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved source-match reachability referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn valueStoreById(
        self: *SourceMatchReachabilityFinalizer,
        store_id: repr.ValueInfoStoreId,
    ) *repr.ValueInfoStore {
        const index = @intFromEnum(store_id);
        if (index >= self.program.value_stores.items.len) {
            lambdaInvariant("lambda-solved source-match reachability referenced missing value store");
        }
        return &self.program.value_stores.items[index];
    }

    fn valueStoreFor(
        self: *SourceMatchReachabilityFinalizer,
        record: *const ProcBuildRecord,
    ) *repr.ValueInfoStore {
        return self.valueStoreById(record.value_store);
    }
};

const CallableEmissionAssignmentMode = enum {
    allow_pending_call_values,
    strict,
};

fn assignCallableEmissionPlans(
    program: *Program,
    records: []const ProcBuildRecord,
    artifact_views: ArtifactViews,
    mode: CallableEmissionAssignmentMode,
) Allocator.Error!void {
    var pass: usize = 0;
    while (true) : (pass += 1) {
        if (pass > 128) {
            lambdaInvariant("lambda-solved callable emission assignment did not converge");
        }
        var changed = false;
        for (program.solve_sessions.items, 0..) |*session, raw_session| {
            var assigner = CallableEmissionAssigner{
                .allocator = program.allocator,
                .program = program,
                .records = records,
                .artifact_views = artifact_views,
                .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
                .session = session,
                .group_sets = .empty,
                .group_set_index = std.AutoHashMap(repr.RepresentationGroupId, usize).init(program.allocator),
                .group_publish_states = std.AutoHashMap(repr.RepresentationGroupId, CallableGroupPublishState).init(program.allocator),
                .erased_groups = .empty,
                .erased_group_index = std.AutoHashMap(repr.RepresentationGroupId, usize).init(program.allocator),
                .function_roots = .empty,
                .function_root_index = std.AutoHashMap(repr.RepresentationGroupId, usize).init(program.allocator),
                .mode = mode,
            };
            errdefer assigner.deinit();
            try assigner.assign();
            changed = changed or assigner.changed;
            assigner.deinit();
        }
        if (!changed) break;
    }
}

const CallableGroupSet = struct {
    group: repr.RepresentationGroupId,
    members: std.ArrayList(repr.CanonicalCallableSetMember),
    key: ?repr.CanonicalCallableSetKey = null,
};

const ProcValueCallableSource = struct {
    proc: canonical.MirProcedureRef,
    published_proc: ?canonical.MirProcedureRef = null,
    target_instance: repr.ProcRepresentationInstanceId,
    captures: []const repr.ValueInfoId,
    fn_ty: canonical.CanonicalTypeKey,
    source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
};

const CallableGroupPublishState = enum {
    resolving,
    published,
};

const ErasedGroupProvenance = struct {
    group: repr.RepresentationGroupId,
    provenance: std.ArrayList(repr.BoxErasureProvenance),
};

const FunctionRootForGroup = struct {
    group: repr.RepresentationGroupId,
    root: repr.RepRootId,
};

const CapturedPayloadValueKey = struct {
    value_store: repr.ValueInfoStoreId,
    value: repr.ValueInfoId,
};

const CallableEmissionAssigner = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    artifact_views: ArtifactViews,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,
    group_sets: std.ArrayList(CallableGroupSet),
    group_set_index: std.AutoHashMap(repr.RepresentationGroupId, usize),
    group_publish_states: std.AutoHashMap(repr.RepresentationGroupId, CallableGroupPublishState),
    erased_groups: std.ArrayList(ErasedGroupProvenance),
    erased_group_index: std.AutoHashMap(repr.RepresentationGroupId, usize),
    function_roots: std.ArrayList(FunctionRootForGroup),
    function_root_index: std.AutoHashMap(repr.RepresentationGroupId, usize),
    mode: CallableEmissionAssignmentMode,
    changed: bool = false,

    fn deinit(self: *CallableEmissionAssigner) void {
        self.function_root_index.deinit();
        self.function_roots.deinit(self.allocator);
        for (self.erased_groups.items) |*entry| entry.provenance.deinit(self.allocator);
        self.erased_group_index.deinit();
        self.erased_groups.deinit(self.allocator);
        for (self.group_sets.items) |*entry| {
            for (entry.members.items) |member| {
                if (member.capture_slots.len > 0) self.allocator.free(member.capture_slots);
            }
            entry.members.deinit(self.allocator);
        }
        self.group_set_index.deinit();
        self.group_sets.deinit(self.allocator);
        self.group_publish_states.deinit();
    }

    fn assign(self: *CallableEmissionAssigner) Allocator.Error!void {
        try self.collectFunctionRootMetadata();
        try self.seedAlreadyErasedCallableGroupEmissions();
        try self.collectBoxErasureRequirements();
        try self.publishGroupErasureProvenance();
        try self.collectFiniteCallableContributions();
        if (self.mode == .allow_pending_call_values) return;
        try self.publishCallableGroupSets();
        try self.publishTypeOnlyErasedFunctionGroupEmissions();
        try self.assignValueEmissionPlans();
    }

    fn collectFunctionRootMetadata(self: *CallableEmissionAssigner) Allocator.Error!void {
        var raw_root: u32 = 0;
        while (raw_root < self.representationStore().roots_len) : (raw_root += 1) {
            const root: repr.RepRootId = @enumFromInt(raw_root);
            const info = self.representationStore().rootTypeInfo(root) orelse continue;
            if (!self.valueHasFunctionType(info.logical_ty)) continue;
            const group = self.callableGroupForFunctionRoot(root) orelse continue;
            if (self.function_root_index.get(group)) |existing_index| {
                const existing = self.representationStore().rootTypeInfo(self.function_roots.items[existing_index].root) orelse {
                    lambdaInvariant("lambda-solved function root metadata referenced root without type info");
                };
                if (existing.source_root == null and info.source_root != null) {
                    self.function_roots.items[existing_index].root = root;
                }
                continue;
            }
            const index = self.function_roots.items.len;
            try self.function_roots.append(self.allocator, .{
                .group = group,
                .root = root,
            });
            try self.function_root_index.put(group, index);
        }
    }

    fn representationStore(self: *CallableEmissionAssigner) *repr.RepresentationStore {
        return &self.session.representation_store;
    }

    fn collectFiniteCallableContributions(self: *CallableEmissionAssigner) Allocator.Error!void {
        for (self.session.members) |instance| {
            const record = self.recordForInstance(instance);
            if (!record.materialized) continue;
            const value_store = self.valueStoreFor(record);
            for (value_store.values.items) |value_info| {
                if (!value_store.valueSourceMatchBranchReachable(value_info)) continue;
                const callable = value_info.callable orelse continue;
                const group = self.callableGroupForValueInfo(value_info, callable) orelse
                    lambdaInvariant("lambda-solved callable value reached emission assignment without a solved callable group");
                switch (self.representationStore().callableEmissionPlan(callable.emission_plan)) {
                    .finite => |key| {
                        switch (callable.source) {
                            .proc_value => |source| {
                                try self.addGroupCallableWorkMember(group, try self.workMemberForProcValueSource(source));
                            },
                            .finite_set,
                            .erased_adapter,
                            => try self.addCallableSetDescriptorWorkMembers(group, key),
                            .already_erased => {},
                        }
                    },
                    .pending_proc_value => {
                        const source = switch (callable.source) {
                            .proc_value => |source| source,
                            else => lambdaInvariant("lambda-solved pending proc-value emission has non-proc callable source"),
                        };
                        try self.addGroupCallableWorkMember(group, try self.workMemberForProcValueSource(source));
                    },
                    .erase_proc_value => {
                        const source = switch (callable.source) {
                            .proc_value => |source| source,
                            else => lambdaInvariant("lambda-solved proc-value erase emission has non-proc callable source"),
                        };
                        try self.addGroupCallableWorkMember(group, try self.workMemberForProcValueSource(source));
                    },
                    .erase_finite_set => |erase| {
                        switch (callable.source) {
                            .proc_value => |source| {
                                try self.addGroupCallableWorkMember(group, try self.workMemberForProcValueSource(source));
                            },
                            .finite_set,
                            .erased_adapter,
                            => try self.addCallableSetDescriptorWorkMembers(group, erase.adapter.callable_set_key),
                            .already_erased => {},
                        }
                    },
                    .already_erased => continue,
                }
            }
        }
    }

    fn addCallableSetDescriptorWorkMembers(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
        key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!void {
        const descriptor = self.representationStore().callableSetDescriptor(key) orelse {
            lambdaInvariant("lambda-solved callable emission referenced a missing callable-set descriptor");
        };
        for (descriptor.members) |member| {
            try self.addGroupCallableWorkMember(group, workMemberFromCanonicalMember(member));
        }
    }

    fn addGroupCallableWorkMember(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
        member: repr.CallableGroupWorkMember,
    ) Allocator.Error!void {
        if (try self.representationStore().addCallableGroupWorkMember(group, member)) {
            self.changed = true;
        }
    }

    fn addGroupCallableMember(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
        member: repr.CanonicalCallableSetMember,
    ) Allocator.Error!void {
        const set = try self.groupSetFor(group);
        for (set.members.items) |*existing| {
            if (callableSetMemberEquivalent(existing.*, member)) return;
            if (callableSetMemberSameIdentity(existing.*, member)) {
                lambdaInvariant("lambda-solved callable-group member identity was added twice with different payloads");
            }
        }
        const capture_slots = if (member.capture_slots.len == 0)
            &.{}
        else
            try self.allocator.dupe(repr.CallableSetCaptureSlot, member.capture_slots);
        errdefer if (capture_slots.len > 0) self.allocator.free(capture_slots);
        try set.members.append(self.allocator, .{
            .member = member.member,
            .proc_value = member.proc_value,
            .source_fn_ty_payload = member.source_fn_ty_payload,
            .source_proc = member.source_proc,
            .published_proc_value = member.published_proc_value,
            .published_source_proc = member.published_source_proc,
            .lifted_owner_source_fn_ty_payload = member.lifted_owner_source_fn_ty_payload,
            .target_instance = member.target_instance,
            .capture_slots = capture_slots,
            .capture_shape_key = member.capture_shape_key,
        });
    }

    fn groupSetFor(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) Allocator.Error!*CallableGroupSet {
        if (self.group_set_index.get(group)) |index| return &self.group_sets.items[index];
        const index = self.group_sets.items.len;
        try self.group_sets.append(self.allocator, .{
            .group = group,
            .members = .empty,
        });
        try self.group_set_index.put(group, index);
        return &self.group_sets.items[index];
    }

    fn publishCallableGroupSets(self: *CallableEmissionAssigner) Allocator.Error!void {
        var raw_set: usize = 0;
        while (raw_set < self.representationStore().callableGroupWorkSetCount()) : (raw_set += 1) {
            const workset = self.representationStore().callableGroupWorkSetAt(raw_set);
            if (!try self.ensureGroupSetPublished(workset.group)) {
                lambdaInvariant("lambda-solved strict callable emission assignment left a callable group unpublished");
            }
        }
    }

    fn ensureGroupSetPublished(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) Allocator.Error!bool {
        if (self.group_publish_states.get(group)) |state| {
            switch (state) {
                .published => return true,
                .resolving => lambdaInvariant("lambda-solved recursive callable-group emission requires recursive callable-set SCC support"),
            }
        }
        try self.group_publish_states.put(group, .resolving);
        errdefer _ = self.group_publish_states.remove(group);

        const work_members = self.representationStore().callableGroupWorkMembers(group);
        if (work_members.len == 0) {
            if (self.mode == .allow_pending_call_values) {
                _ = self.group_publish_states.remove(group);
                return false;
            }
            lambdaInvariant("lambda-solved callable group has no callable work table");
        }
        if (!try self.populateGroupSetFromWorkMembers(group, work_members)) {
            _ = self.group_publish_states.remove(group);
            return false;
        }
        const set = self.callableGroupSet(group) orelse
            lambdaInvariant("lambda-solved callable group work table did not produce a callable-set record");
        if (set.members.items.len == 0) {
            if (self.mode == .allow_pending_call_values) {
                _ = self.group_publish_states.remove(group);
                return false;
            }
            lambdaInvariant("lambda-solved callable group set has no members");
        }
        for (set.members.items, 0..) |*member, semantic_member_index| {
            member.member = @enumFromInt(@as(u32, @intCast(semantic_member_index)));
        }
        const group_key = try self.representationStore().internCallableSetDescriptor(set.members.items);
        set.key = group_key;
        if (self.representationStore().callableGroupEmission(group) == null) {
            _ = try self.ensureCallableGroupEmission(set, group_key, self.erasedProvenance(group));
        }
        try self.group_publish_states.put(group, .published);
        return true;
    }

    fn populateGroupSetFromWorkMembers(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
        work_members: []const repr.CallableGroupWorkMember,
    ) Allocator.Error!bool {
        for (work_members) |work| {
            if (!try self.ensureTargetCaptureCallableDependenciesPublished(work.target_instance)) return false;
        }
        for (work_members) |work| {
            const member = try self.memberForWorkMember(work);
            defer if (member.capture_slots.len > 0) self.allocator.free(member.capture_slots);
            try self.addGroupCallableMember(group, member);
        }
        return true;
    }

    fn ensureProcValueSourceCaptureCallableDependenciesPublished(
        self: *CallableEmissionAssigner,
        source: ProcValueCallableSource,
    ) Allocator.Error!bool {
        const target_record = self.recordForInstance(source.target_instance);
        const target_value_store = self.valueStoreFor(target_record);
        const target_captures = target_value_store.sliceValueSpan(target_record.public_roots.captures);
        if (source.captures.len != target_captures.len) {
            lambdaInvariant("lambda-solved proc-value callable capture arity differs from target captures");
        }
        return try self.ensureTargetCaptureCallableDependenciesPublished(source.target_instance);
    }

    fn ensureTargetCaptureCallableDependenciesPublished(
        self: *CallableEmissionAssigner,
        target_instance: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!bool {
        const target_record = self.recordForInstance(target_instance);
        const target_value_store = self.valueStoreFor(target_record);
        const target_captures = target_value_store.sliceValueSpan(target_record.public_roots.captures);
        var visited = std.AutoHashMap(CapturedPayloadValueKey, void).init(self.allocator);
        defer visited.deinit();
        for (target_captures) |target_capture| {
            if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(
                target_record,
                target_value_store,
                target_capture,
                &visited,
            )) return false;
        }
        return true;
    }

    fn ensureCapturedPayloadValueCallableDependenciesPublished(
        self: *CallableEmissionAssigner,
        target_record: *const ProcBuildRecord,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
        visited: *std.AutoHashMap(CapturedPayloadValueKey, void),
    ) Allocator.Error!bool {
        const visit_key = CapturedPayloadValueKey{
            .value_store = target_record.value_store,
            .value = value,
        };
        const visit = try visited.getOrPut(visit_key);
        if (visit.found_existing) return true;

        const value_info = target_value_store.values.items[@intFromEnum(value)];

        if (value_info.value_alias_source) |source| {
            if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, source, visited)) return false;
        }
        if (value_info.nominal_backing_value) |backing| {
            if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, backing, visited)) return false;
        }
        if (value_info.projection_info) |projection_id| {
            const projection = target_value_store.projections.items[@intFromEnum(projection_id)];
            if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, projection.source, visited)) return false;
        }
        if (value_info.join_info) |join_id| {
            const join = target_value_store.joins.items[@intFromEnum(join_id)];
            const inputs = target_value_store.sliceJoinInputSpan(join.inputs);
            for (inputs) |input| {
                if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, input.value, visited)) return false;
            }
        }
        if (value_info.boxed) |boxed| {
            if (boxed.payload_value) |payload| {
                if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, payload, visited)) return false;
            }
        }
        if (value_info.aggregate) |aggregate| {
            switch (aggregate) {
                .record => |record| {
                    for (record.fields) |field| {
                        if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, field.value, visited)) return false;
                    }
                },
                .tuple => |items| {
                    for (items) |item| {
                        if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, item.value, visited)) return false;
                    }
                },
                .tag => |tag| {
                    for (tag.payloads) |payload| {
                        if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, payload.value, visited)) return false;
                    }
                },
                .list => |list| {
                    for (list.elems) |elem| {
                        if (!try self.ensureCapturedPayloadValueCallableDependenciesPublished(target_record, target_value_store, elem.value, visited)) return false;
                    }
                },
            }
        }

        if (!self.valueHasFunctionType(value_info.logical_ty)) return true;
        const capture_group = self.callableGroupForValueInfo(value_info, value_info.callable) orelse
            lambdaInvariant("lambda-solved captured function value has no solved callable group");
        return try self.ensureCapturedCallableGroupPublished(capture_group);
    }

    fn ensureCapturedCallableGroupPublished(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) Allocator.Error!bool {
        if (self.representationStore().callableGroupEmission(group) != null) return true;
        if (self.representationStore().callableGroupWorkMembers(group).len > 0) {
            return try self.ensureGroupSetPublished(group);
        }
        if (self.callableGroupSet(group) != null) return try self.ensureGroupSetPublished(group);
        if (self.erasedProvenance(group) != null) {
            try self.ensureTypeOnlyErasedFunctionGroupEmission(group);
            return self.representationStore().callableGroupEmission(group) != null;
        }
        return false;
    }

    fn collectBoxErasureRequirements(self: *CallableEmissionAssigner) Allocator.Error!void {
        for (self.representationStore().representation_requirements.items) |requirement| {
            switch (requirement) {
                .require_box_erased => |erasure| {
                    switch (erasure.provenance) {
                        .local_box_boundary => |boundary_id| {
                            _ = self.boxBoundary(boundary_id);
                        },
                        .const_graph_box => {},
                        .promoted_wrapper => {},
                    }
                    try self.markErasedPayloadRoot(erasure.payload_root, erasure.provenance);
                },
            }
        }
    }

    fn publishGroupErasureProvenance(self: *CallableEmissionAssigner) Allocator.Error!void {
        for (self.erased_groups.items) |entry| {
            try self.representationStore().publishGroupErasureProvenance(entry.group, entry.provenance.items);
        }
    }

    fn seedAlreadyErasedCallableGroupEmissions(self: *CallableEmissionAssigner) Allocator.Error!void {
        for (self.session.members) |instance| {
            const record = self.recordForInstance(instance);
            if (!record.materialized) continue;
            const value_store = self.valueStoreFor(record);
            for (value_store.values.items) |value_info| {
                if (!value_store.valueSourceMatchBranchReachable(value_info)) continue;
                const callable = value_info.callable orelse continue;
                switch (self.representationStore().callableEmissionPlan(callable.emission_plan)) {
                    .already_erased => {
                        const group = self.callableGroupForValueInfo(value_info, callable) orelse
                            lambdaInvariant("lambda-solved already-erased callable seed reached value without a solved callable group");
                        _ = try self.publishAlreadyErasedCallableGroupEmission(group, callable.emission_plan);
                    },
                    .pending_proc_value,
                    .finite,
                    .erase_proc_value,
                    .erase_finite_set,
                    => {},
                }
            }
        }
    }

    fn markErasedPayloadRoot(
        self: *CallableEmissionAssigner,
        root: repr.RepRootId,
        provenance: repr.BoxErasureProvenance,
    ) Allocator.Error!void {
        const start_group = self.groupForRoot(root) orelse return;
        var visited = std.AutoHashMap(repr.RepresentationGroupId, void).init(self.allocator);
        defer visited.deinit();
        var stack = std.ArrayList(repr.RepresentationGroupId).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, start_group);

        while (stack.pop()) |current| {
            if (visited.contains(current)) continue;
            try visited.put(current, {});
            try self.addErasedGroupProvenance(current, provenance);
            for (self.representationStore().representation_edges.items) |edge| {
                if (!edgePropagatesBoxErasure(edge.kind)) continue;
                const from_root = self.endpointRootInSession(edge.from) orelse continue;
                const to_root = self.endpointRootInSession(edge.to) orelse continue;
                const from = self.groupForRoot(from_root) orelse continue;
                const to = self.groupForRoot(to_root) orelse continue;
                switch (edge.kind) {
                    .child => |child| switch (child) {
                        .function_arg => {
                            if (to == current) try stack.append(self.allocator, from);
                        },
                        .box_payload => {
                            if (from == current) try stack.append(self.allocator, to);
                            if (to == current) try stack.append(self.allocator, from);
                        },
                        else => {
                            if (from == current) try stack.append(self.allocator, to);
                        },
                    },
                    else => {
                        if (from == current) try stack.append(self.allocator, to);
                    },
                }
            }
        }
    }

    fn publishTypeOnlyErasedFunctionGroupEmissions(self: *CallableEmissionAssigner) Allocator.Error!void {
        for (self.erased_groups.items) |entry| {
            try self.ensureTypeOnlyErasedFunctionGroupEmission(entry.group);
        }
    }

    fn ensureTypeOnlyErasedFunctionGroupEmission(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) Allocator.Error!void {
        if (self.representationStore().callableGroupEmission(group) != null) return;
        if (self.representationStore().callableGroupWorkMembers(group).len > 0) return;
        if (self.callableGroupSet(group) != null) return;
        const provenance = self.erasedProvenance(group) orelse return;
        if (provenance.len == 0) return;
        const root_index = self.function_root_index.get(group) orelse return;
        const root = self.function_roots.items[root_index].root;
        const info = self.representationStore().rootTypeInfo(root) orelse {
            lambdaInvariant("lambda-solved erased function group root has no published type info");
        };
        const endpoint = try self.publishErasedBoundaryEndpointForRootInfo(info);
        const payload = self.representationStore().session_executable_type_payloads.get(endpoint.ty.payload);
        const erased = switch (payload) {
            .erased_fn => |erased| erased,
            else => lambdaInvariant("lambda-solved type-only erased function root did not publish erased callable payload"),
        };
        const plan = repr.AlreadyErasedCallablePlan{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .result_ty = endpoint.key,
            .capture = .none,
            .provenance = provenance,
        };
        const emission = try self.representationStore().appendAlreadyErasedCallableEmissionPlan(plan);
        try self.representationStore().publishCallableGroupEmission(group, emission);
        self.changed = true;
    }

    fn addErasedGroupProvenance(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
        provenance: repr.BoxErasureProvenance,
    ) Allocator.Error!void {
        const entry = try self.erasedGroupFor(group);
        for (entry.provenance.items) |existing| {
            if (boxErasureProvenanceEql(existing, provenance)) return;
        }
        try entry.provenance.append(self.allocator, provenance);
    }

    fn erasedGroupFor(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) Allocator.Error!*ErasedGroupProvenance {
        if (self.erased_group_index.get(group)) |index| return &self.erased_groups.items[index];
        const index = self.erased_groups.items.len;
        try self.erased_groups.append(self.allocator, .{
            .group = group,
            .provenance = .empty,
        });
        try self.erased_group_index.put(group, index);
        return &self.erased_groups.items[index];
    }

    fn assignValueEmissionPlans(self: *CallableEmissionAssigner) Allocator.Error!void {
        for (self.session.members) |instance| {
            const record = self.recordForInstance(instance);
            if (!record.materialized) continue;
            const value_store = self.valueStoreFor(record);
            for (value_store.values.items, 0..) |*value_info, raw_value| {
                if (!value_store.valueSourceMatchBranchReachable(value_info.*)) continue;
                if (value_info.callable) |existing| {
                    switch (self.representationStore().callableEmissionPlan(existing.emission_plan)) {
                        .already_erased => {
                            const group = self.callableGroupForValueInfo(value_info.*, existing) orelse
                                lambdaInvariant("lambda-solved already-erased callable value reached emission assignment without a solved callable group");
                            const canonical_emission = try self.publishAlreadyErasedCallableGroupEmission(group, existing.emission_plan);
                            const already_erased = switch (self.representationStore().callableEmissionPlan(canonical_emission)) {
                                .already_erased => |erased| erased,
                                else => unreachable,
                            };
                            if (already_erased.provenance.len > 0) {
                                try self.representationStore().publishGroupErasureProvenance(group, already_erased.provenance);
                            }
                            var canonical_callable = existing;
                            switch (existing.source) {
                                .proc_value => |source| {
                                    const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
                                    const erase = try self.procValueErasePlanForAlreadyErasedSource(value_id, .{
                                        .proc = source.proc,
                                        .published_proc = source.published_proc,
                                        .target_instance = source.target_instance,
                                        .captures = source.captures,
                                        .fn_ty = source.fn_ty,
                                        .source_fn_ty_payload = source.source_fn_ty_payload,
                                    }, already_erased);
                                    defer if (erase.capture_slots.len > 0) self.allocator.free(erase.capture_slots);
                                    canonical_callable.emission_plan = try self.representationStore().appendProcValueEraseEmissionPlan(erase);
                                    value_info.callable = canonical_callable;
                                    self.changed = true;
                                    continue;
                                },
                                .finite_set,
                                .already_erased,
                                .erased_adapter,
                                => {},
                            }
                            if (canonical_emission != existing.emission_plan) {
                                canonical_callable.emission_plan = canonical_emission;
                                value_info.callable = canonical_callable;
                                self.changed = true;
                            }
                            continue;
                        },
                        .pending_proc_value,
                        .finite,
                        .erase_proc_value,
                        .erase_finite_set,
                        => {},
                    }
                }
                if (value_info.callable == null and !self.valueHasFunctionType(value_info.logical_ty)) continue;
                const group = self.callableGroupForValueInfo(value_info.*, value_info.callable) orelse
                    lambdaInvariant("lambda-solved callable value reached emission assignment without a solved callable group");
                const group_set = self.callableGroupSet(group) orelse {
                    if (value_info.callable == null and self.valueHasFunctionType(value_info.logical_ty)) {
                        if (self.representationStore().callableGroupEmission(group)) |emission| {
                            value_info.callable = try self.adoptPublishedGroupEmission(value_info.*, emission);
                            self.changed = true;
                            continue;
                        }
                        if (self.erasedProvenance(group)) |provenance| {
                            const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
                            const callable = try self.synthesizeAlreadyErasedCallableInfo(record, value_store, value_id, value_info.*, provenance);
                            value_info.callable = callable;
                            self.changed = true;
                            if (self.representationStore().callableGroupEmission(group) == null) {
                                try self.representationStore().publishCallableGroupEmission(group, callable.emission_plan);
                                self.changed = true;
                            }
                            continue;
                        }
                    }
                    if (value_info.pending_comptime_dependency_origin) continue;
                    if (self.mode == .allow_pending_call_values and value_info.callable == null) continue;
                    if (value_info.projection_info) |projection_id| {
                        const projection = value_store.projections.items[@intFromEnum(projection_id)];
                        const source_info = value_store.values.items[@intFromEnum(projection.source)];
                        lambdaInvariantFmt(
                            "lambda-solved function-typed value {d} in instance {d} owner {s} materialized={} root={s} branch={} solved to group {d} with no finite callable members (callable={}, projection={}, alias={}); projection source={d} kind={s} source_callable={} source_aggregate={} source_alias={} source_root={s}",
                            .{
                                raw_value,
                                @intFromEnum(instance),
                                @tagName(record.owner),
                                record.materialized,
                                @tagName(self.representationStore().rootKind(value_info.root)),
                                value_info.source_match_branch != null,
                                @intFromEnum(group),
                                value_info.callable != null,
                                value_info.projection_info != null,
                                value_info.value_alias_source != null,
                                @intFromEnum(projection.source),
                                @tagName(std.meta.activeTag(projection.kind)),
                                source_info.callable != null,
                                source_info.aggregate != null,
                                source_info.value_alias_source != null,
                                @tagName(self.representationStore().rootKind(source_info.root)),
                            },
                        );
                    }
                    lambdaInvariantFmt(
                        "lambda-solved function-typed value {d} in instance {d} owner {s} materialized={} root={s} branch={} solved to group {d} with no finite callable members (callable={}, projection={}, alias={})",
                        .{
                            raw_value,
                            @intFromEnum(instance),
                            @tagName(record.owner),
                            record.materialized,
                            @tagName(self.representationStore().rootKind(value_info.root)),
                            value_info.source_match_branch != null,
                            @intFromEnum(group),
                            value_info.callable != null,
                            value_info.projection_info != null,
                            value_info.value_alias_source != null,
                        },
                    );
                };
                const group_key = group_set.key orelse {
                    if (self.mode == .allow_pending_call_values) continue;
                    lambdaInvariant("lambda-solved callable group set was not interned before emission assignment");
                };
                const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
                const provenance = self.erasedProvenance(group);
                const group_emission = try self.ensureCallableGroupEmission(group_set, group_key, provenance);
                var synthesized_callable = false;
                var callable = if (value_info.callable) |existing| existing else blk: {
                    synthesized_callable = true;
                    break :blk try self.synthesizeCallableInfo(value_info.*, group_key);
                };
                if (synthesized_callable) self.changed = true;
                const current_emission = self.representationStore().callableEmissionPlan(callable.emission_plan);
                switch (current_emission) {
                    .pending_proc_value => |construction_id| {
                        const attached = callable.construction_plan orelse {
                            lambdaInvariant("lambda-solved pending proc-value emission has no construction plan");
                        };
                        if (attached != construction_id) {
                            lambdaInvariant("lambda-solved pending proc-value emission is not attached to its construction plan");
                        }
                    },
                    .finite => {},
                    .already_erased,
                    .erase_proc_value,
                    => continue,
                    .erase_finite_set => {},
                }
                try self.rewriteCallableConstructionPlan(&callable, value_id, group_set, group_key);
                switch (current_emission) {
                    .pending_proc_value => {
                        self.representationStore().sealPendingProcValueEmissionPlanAsFinite(callable.emission_plan, group_key);
                        self.changed = true;
                    },
                    .finite => |key| if (!repr.callableSetKeyEql(key, group_key)) {
                        lambdaInvariant("lambda-solved finite callable emission key changed after publication");
                    },
                    .erase_finite_set => {},
                    .already_erased,
                    .erase_proc_value,
                    => unreachable,
                }
                if (callable.emission_plan != group_emission) {
                    callable.emission_plan = group_emission;
                    self.changed = true;
                }
                value_info.callable = callable;
            }
        }
    }

    fn procValueErasePlanForAlreadyErasedSource(
        self: *CallableEmissionAssigner,
        value_id: repr.ValueInfoId,
        source: ProcValueCallableSource,
        erased: repr.AlreadyErasedCallablePlan,
    ) Allocator.Error!repr.ProcValueErasePlan {
        if (erased.provenance.len == 0) {
            lambdaInvariant("lambda-solved direct proc-value erased emission has no Box(T) provenance");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, erased.sig_key.source_fn_ty)) {
            lambdaInvariant("lambda-solved direct proc-value erased emission source type differs from erased signature");
        }
        if (!try self.ensureProcValueSourceCaptureCallableDependenciesPublished(source)) {
            lambdaInvariant("lambda-solved proc-value erase plan capture dependencies were not solved before capture schema computation");
        }
        const target_record = self.recordForInstance(source.target_instance);
        if (!mirProcedureBodyIdentityEql(target_record.proc, source.proc)) {
            lambdaInvariant("lambda-solved direct proc-value erased emission target instance has a different procedure body identity");
        }
        const target_value_store = self.valueStoreFor(target_record);
        const target_captures = target_value_store.sliceValueSpan(target_record.public_roots.captures);
        if (source.captures.len != target_captures.len) {
            lambdaInvariant("lambda-solved direct proc-value erased emission capture arity differs from target captures");
        }
        const capture_slots = try self.captureSlotsForTargetValues(target_record, target_captures);
        errdefer if (capture_slots.len > 0) self.allocator.free(capture_slots);
        const capture_shape_key = repr.captureShapeKeyForSlots(capture_slots);
        var erased_fn_sig_key = erased.sig_key;
        if (capture_slots.len != 0) {
            erased_fn_sig_key.capture_ty = repr.captureTupleExecKeyForSlots(capture_slots);
        }
        const abi = self.representationStore().erased_fn_abis.abiFor(erased.sig_key.abi) orelse {
            lambdaInvariant("lambda-solved direct proc-value erased emission referenced missing ABI");
        };

        return .{
            .source_value = value_id,
            .proc_value = source.proc.callable,
            .target_instance = source.target_instance,
            .erased_fn_sig_key = erased_fn_sig_key,
            .executable_specialization_key = .{
                .base = source.proc.proc.proc_base,
                .requested_fn_ty = erased_fn_sig_key.source_fn_ty,
                .exec_arg_tys = abi.arg_exec_keys,
                .exec_ret_ty = abi.ret_exec_key,
                .callable_repr_mode = .direct,
                .capture_shape_key = capture_shape_key,
            },
            .capture_shape_key = capture_shape_key,
            .capture_slots = capture_slots,
            .provenance = erased.provenance,
        };
    }

    fn publishAlreadyErasedCallableGroupEmission(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
        emission_plan: repr.CallableValueEmissionPlanId,
    ) Allocator.Error!repr.CallableValueEmissionPlanId {
        if (self.representationStore().callableGroupEmission(group)) |existing| {
            if (self.callableEmissionPlansEquivalent(existing, emission_plan)) {
                const existing_plan = self.representationStore().callableEmissionPlan(existing);
                const current_plan = self.representationStore().callableEmissionPlan(emission_plan);
                switch (existing_plan) {
                    .already_erased => |existing_erased| switch (current_plan) {
                        .already_erased => |erased| {
                            if (!boxErasureProvenanceSliceContainsAll(existing_erased.provenance, erased.provenance)) {
                                self.changed = true;
                            }
                            try self.representationStore().mergeCallableEmissionPlanProvenance(existing, erased.provenance);
                        },
                        else => lambdaInvariant("lambda-solved already-erased group emission merge reached non-erased current plan"),
                    },
                    else => lambdaInvariant("lambda-solved already-erased group emission merge reached non-erased existing plan"),
                }
                return existing;
            }
            const existing_plan = self.representationStore().callableEmissionPlan(existing);
            const current_plan = self.representationStore().callableEmissionPlan(emission_plan);
            const mismatch = switch (existing_plan) {
                .already_erased => |left| switch (current_plan) {
                    .already_erased => |right| alreadyErasedCallablePlanMismatch(left, right),
                    else => "plan_tag",
                },
                else => "plan_tag",
            };
            lambdaInvariantFmt(
                "lambda-solved callable group emission was published with incompatible plans: {s} then {s}; mismatch={s}",
                .{ @tagName(std.meta.activeTag(existing_plan)), @tagName(std.meta.activeTag(current_plan)), mismatch },
            );
        }
        try self.representationStore().publishCallableGroupEmission(group, emission_plan);
        self.changed = true;
        return emission_plan;
    }

    fn synthesizeCallableInfo(
        self: *CallableEmissionAssigner,
        value_info: repr.ValueInfo,
        group_key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!repr.CallableValueInfo {
        if (!self.valueHasFunctionType(value_info.logical_ty)) {
            lambdaInvariant("lambda-solved attempted to synthesize callable metadata for a non-function value");
        }
        const callable_root = self.callableRootForFunctionRoot(value_info.root);
        return .{
            .whole_function_root = value_info.root,
            .callable_root = callable_root,
            .source = .{ .finite_set = group_key },
            .emission_plan = try self.representationStore().appendFiniteCallableEmissionPlan(group_key),
            .construction_plan = null,
        };
    }

    fn adoptPublishedGroupEmission(
        self: *CallableEmissionAssigner,
        value_info: repr.ValueInfo,
        emission: repr.CallableValueEmissionPlanId,
    ) Allocator.Error!repr.CallableValueInfo {
        return switch (self.representationStore().callableEmissionPlan(emission)) {
            .finite,
            .erase_finite_set,
            => try self.adoptFiniteGroupEmission(value_info, emission),
            .already_erased => try self.adoptAlreadyErasedGroupEmission(value_info, emission),
            .pending_proc_value,
            .erase_proc_value,
            => lambdaInvariant("lambda-solved unconstructed function value reached a non-group callable emission"),
        };
    }

    fn adoptFiniteGroupEmission(
        self: *CallableEmissionAssigner,
        value_info: repr.ValueInfo,
        emission: repr.CallableValueEmissionPlanId,
    ) Allocator.Error!repr.CallableValueInfo {
        const group_key = switch (self.representationStore().callableEmissionPlan(emission)) {
            .finite => |key| key,
            .erase_finite_set => |erase| erase.adapter.callable_set_key,
            else => lambdaInvariant("lambda-solved finite callable value reached non-finite group emission"),
        };
        if (!self.valueHasFunctionType(value_info.logical_ty)) {
            lambdaInvariant("lambda-solved finite group emission was attached to a non-function value");
        }
        const callable_root = self.callableRootForFunctionRoot(value_info.root);
        return .{
            .whole_function_root = value_info.root,
            .callable_root = callable_root,
            .source = .{ .finite_set = group_key },
            .emission_plan = emission,
            .construction_plan = null,
        };
    }

    fn adoptAlreadyErasedGroupEmission(
        self: *CallableEmissionAssigner,
        value_info: repr.ValueInfo,
        emission: repr.CallableValueEmissionPlanId,
    ) Allocator.Error!repr.CallableValueInfo {
        const erased = switch (self.representationStore().callableEmissionPlan(emission)) {
            .already_erased => |erased| erased,
            else => lambdaInvariant("lambda-solved erased callable value reached non-erased group emission"),
        };
        if (!self.valueHasFunctionType(value_info.logical_ty)) {
            lambdaInvariant("lambda-solved erased group emission was attached to a non-function value");
        }
        const callable_root = self.callableRootForFunctionRoot(value_info.root);
        return .{
            .whole_function_root = value_info.root,
            .callable_root = callable_root,
            .source = .{ .already_erased = .{
                .sig_key = erased.sig_key,
                .capture_shape_key = erased.capture_shape_key,
                .result_ty = erased.result_ty,
                .capture = erased.capture,
                .provenance = &.{},
            } },
            .emission_plan = emission,
            .construction_plan = null,
        };
    }

    fn synthesizeAlreadyErasedCallableInfo(
        self: *CallableEmissionAssigner,
        _: *const ProcBuildRecord,
        value_store: *const repr.ValueInfoStore,
        value_id: repr.ValueInfoId,
        value_info: repr.ValueInfo,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.CallableValueInfo {
        const endpoint = try self.publishTargetErasedBoundaryEndpoint(value_store, value_id);
        const payload = self.representationStore().session_executable_type_payloads.get(endpoint.ty.payload);
        const erased = switch (payload) {
            .erased_fn => |erased| erased,
            else => lambdaInvariant("lambda-solved erased function slot endpoint did not publish erased callable payload"),
        };

        const plan = repr.AlreadyErasedCallablePlan{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .result_ty = endpoint.key,
            .capture = .none,
            .provenance = provenance,
        };
        const emission = try self.representationStore().appendAlreadyErasedCallableEmissionPlan(plan);
        const callable_root = self.callableRootForFunctionRoot(value_info.root);
        return .{
            .whole_function_root = value_info.root,
            .callable_root = callable_root,
            .source = .{ .already_erased = .{
                .sig_key = erased.sig_key,
                .capture_shape_key = erased.capture_shape_key,
                .result_ty = endpoint.key,
                .capture = .none,
                .provenance = &.{},
            } },
            .emission_plan = emission,
            .construction_plan = null,
        };
    }

    fn ensureCallableGroupEmission(
        self: *CallableEmissionAssigner,
        group_set: *const CallableGroupSet,
        group_key: repr.CanonicalCallableSetKey,
        provenance: ?[]const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.CallableValueEmissionPlanId {
        if (self.representationStore().callableGroupEmission(group_set.group)) |existing| {
            if (self.callableGroupEmissionMatches(existing, group_key, provenance)) return existing;
            const existing_plan = self.representationStore().callableEmissionPlan(existing);
            if (provenance) |boundaries| {
                switch (existing_plan) {
                    .already_erased => |erased| {
                        if (erased.provenance.len > 0) {
                            try self.representationStore().publishGroupErasureProvenance(group_set.group, erased.provenance);
                        }
                        try self.representationStore().mergeCallableEmissionPlanProvenance(existing, boundaries);
                        try self.representationStore().publishGroupErasureProvenance(group_set.group, boundaries);
                        return existing;
                    },
                    .erase_finite_set,
                    .finite,
                    => lambdaInvariant("lambda-solved callable group erasure arrived after finite emission publication"),
                    .pending_proc_value,
                    .erase_proc_value,
                    => lambdaInvariant("lambda-solved finite callable group emission merged with incompatible existing emission plan"),
                }
            } else {
                switch (existing_plan) {
                    .finite => |key| if (!repr.callableSetKeyEql(key, group_key)) {
                        lambdaInvariant("lambda-solved finite callable group emission key changed after publication");
                    },
                    .erase_finite_set => |erase| {
                        if (!repr.callableSetKeyEql(erase.adapter.callable_set_key, group_key)) {
                            lambdaInvariant("lambda-solved erased finite callable group emission key changed after publication");
                        }
                        return existing;
                    },
                    .pending_proc_value,
                    .already_erased,
                    .erase_proc_value,
                    => lambdaInvariant("lambda-solved finite callable group emission merged with incompatible existing emission plan"),
                }
            }
            return existing;
        }
        const emission = if (provenance) |boundaries|
            try self.appendFiniteSetErasePlan(group_set, group_key, boundaries)
        else
            try self.representationStore().appendFiniteCallableEmissionPlan(group_key);
        try self.representationStore().publishCallableGroupEmission(group_set.group, emission);
        self.changed = true;
        return emission;
    }

    fn callableGroupEmissionMatches(
        self: *CallableEmissionAssigner,
        emission: repr.CallableValueEmissionPlanId,
        group_key: repr.CanonicalCallableSetKey,
        provenance: ?[]const repr.BoxErasureProvenance,
    ) bool {
        return switch (self.representationStore().callableEmissionPlan(emission)) {
            .finite => |key| provenance == null and repr.callableSetKeyEql(key, group_key),
            .erase_finite_set => |erase| blk: {
                const boundaries = provenance orelse break :blk false;
                if (!repr.callableSetKeyEql(erase.adapter.callable_set_key, group_key)) break :blk false;
                break :blk boxErasureProvenanceSliceEql(erase.provenance, boundaries);
            },
            .pending_proc_value,
            .already_erased,
            .erase_proc_value,
            => false,
        };
    }

    fn callableEmissionPlansEquivalent(
        self: *CallableEmissionAssigner,
        left_id: repr.CallableValueEmissionPlanId,
        right_id: repr.CallableValueEmissionPlanId,
    ) bool {
        if (left_id == right_id) return true;
        const left = self.representationStore().callableEmissionPlan(left_id);
        const right = self.representationStore().callableEmissionPlan(right_id);
        return switch (left) {
            .already_erased => |left_erased| switch (right) {
                .already_erased => |right_erased| alreadyErasedCallablePlanEquivalent(left_erased, right_erased),
                else => false,
            },
            else => false,
        };
    }

    fn valueHasFunctionType(
        self: *CallableEmissionAssigner,
        ty: Type.TypeVarId,
    ) bool {
        const root = self.program.types.unlinkConst(ty);
        return switch (self.program.types.getNode(root)) {
            .content => |content| switch (content) {
                .func => true,
                else => false,
            },
            else => false,
        };
    }

    fn rewriteCallableConstructionPlan(
        self: *CallableEmissionAssigner,
        callable: *const repr.CallableValueInfo,
        value_id: repr.ValueInfoId,
        group_set: *const CallableGroupSet,
        group_key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!void {
        const construction_id = callable.construction_plan orelse return;
        const construction = self.representationStore().callableConstructionPlanPtr(construction_id);
        if (construction.result != value_id) {
            lambdaInvariant("lambda-solved callable construction plan is attached to a different value during emission assignment");
        }
        const selected = self.currentSelectedMember(callable.*, construction.*, group_set);
        const selected_member = self.memberInGroupSet(group_set, selected);
        construction.callable_set_key = group_key;
        construction.selected_member = selected_member.member;
        construction.target_instance = selected_member.target_instance;
    }

    fn currentSelectedMember(
        self: *CallableEmissionAssigner,
        callable: repr.CallableValueInfo,
        construction: repr.CallableSetConstructionPlan,
        group_set: *const CallableGroupSet,
    ) repr.CanonicalCallableSetMember {
        switch (callable.source) {
            .proc_value => return selectedMemberFromProcValueConstruction(callable, construction, group_set),
            .finite_set,
            .already_erased,
            .erased_adapter,
            => {},
        }

        const emission = self.representationStore().callableEmissionPlan(callable.emission_plan);
        switch (emission) {
            .pending_proc_value => {
                lambdaInvariant("lambda-solved pending proc-value emission has non-proc callable source");
            },
            .finite => |current_key| {
                const member = self.representationStore().callableSetMember(current_key, construction.selected_member) orelse {
                    lambdaInvariant("lambda-solved callable construction selected a missing current callable-set member");
                };
                return member.*;
            },
            .erase_finite_set => |erase| {
                const member = self.representationStore().callableSetMember(erase.adapter.callable_set_key, construction.selected_member) orelse {
                    lambdaInvariant("lambda-solved callable construction selected a missing current callable-set member");
                };
                return member.*;
            },
            .erase_proc_value,
            .already_erased,
            => lambdaInvariant("lambda-solved callable construction reached non-finite emission before assignment"),
        }
    }

    fn selectedMemberFromProcValueConstruction(
        callable: repr.CallableValueInfo,
        construction: repr.CallableSetConstructionPlan,
        group_set: *const CallableGroupSet,
    ) repr.CanonicalCallableSetMember {
        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => lambdaInvariant("lambda-solved proc-value construction has non-proc callable source"),
        };
        if (source.target_instance != construction.target_instance) {
            lambdaInvariant("lambda-solved proc-value construction target differs from source target");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, construction.source_fn_ty)) {
            lambdaInvariant("lambda-solved proc-value construction function type differs from source type");
        }
        if (callable.construction_plan == null) {
            lambdaInvariant("lambda-solved proc-value construction source has no construction plan");
        }
        for (group_set.members.items) |member| {
            if (member.target_instance != source.target_instance) continue;
            if (!mirProcedureBodyIdentityEql(member.source_proc, source.proc)) continue;
            if (!canonical.procedureCallableRefEql(member.proc_value, source.proc.callable)) continue;
            return member;
        }
        lambdaInvariant("lambda-solved proc-value construction selected member missing from solved callable set");
    }

    fn memberInGroupSet(
        _: *CallableEmissionAssigner,
        group_set: *const CallableGroupSet,
        selected: repr.CanonicalCallableSetMember,
    ) repr.CanonicalCallableSetMember {
        for (group_set.members.items) |member| {
            if (callableSetMemberEquivalent(member, selected)) return member;
        }
        lambdaInvariant("lambda-solved callable construction selected member missing from solved group callable set");
    }

    fn workMemberForProcValueSource(
        self: *CallableEmissionAssigner,
        source: anytype,
    ) Allocator.Error!repr.CallableGroupWorkMember {
        const target_id = source.target_instance;
        const target_record = self.recordForInstance(target_id);
        if (!mirProcedureBodyIdentityEql(target_record.proc, source.proc)) {
            lambdaInvariant("lambda-solved proc-value callable target instance has a different procedure body identity");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, source.proc.callable.source_fn_ty)) {
            lambdaInvariant("lambda-solved proc-value callable source function type differs from procedure callable type");
        }

        const target_value_store = self.valueStoreFor(target_record);
        const target_captures = target_value_store.sliceValueSpan(target_record.public_roots.captures);
        if (source.captures.len != target_captures.len) {
            lambdaInvariant("lambda-solved proc-value callable capture arity differs from target captures");
        }

        return .{
            .proc_value = source.proc.callable,
            .source_fn_ty_payload = source.source_fn_ty_payload,
            .source_proc = target_record.proc,
            .published_proc_value = if (source.published_proc) |published| published.callable else null,
            .published_source_proc = source.published_proc,
            .lifted_owner_source_fn_ty_payload = switch (source.proc.callable.template) {
                .lifted => |lifted| self.concreteSourcePayloadForKey(
                    lifted.owner_mono_specialization.requested_mono_fn_ty,
                    "lambda-solved lifted callable owner source function type has no concrete payload",
                ),
                .checked,
                .synthetic,
                => null,
            },
            .target_instance = target_id,
        };
    }

    fn memberForWorkMember(
        self: *CallableEmissionAssigner,
        work: repr.CallableGroupWorkMember,
    ) Allocator.Error!repr.CanonicalCallableSetMember {
        const target_record = self.recordForInstance(work.target_instance);
        if (!mirProcedureBodyIdentityEql(target_record.proc, work.source_proc)) {
            lambdaInvariant("lambda-solved callable group work member target instance has a different procedure body identity");
        }
        const target_value_store = self.valueStoreFor(target_record);
        const target_captures = target_value_store.sliceValueSpan(target_record.public_roots.captures);
        const capture_slots = try self.captureSlotsForTargetValues(target_record, target_captures);
        errdefer if (capture_slots.len > 0) self.allocator.free(capture_slots);
        const capture_shape_key = repr.captureShapeKeyForSlots(capture_slots);
        return .{
            .member = canonical.onlyCallableSetMemberId(),
            .proc_value = work.proc_value,
            .source_fn_ty_payload = work.source_fn_ty_payload,
            .source_proc = target_record.proc,
            .published_proc_value = work.published_proc_value,
            .published_source_proc = work.published_source_proc,
            .lifted_owner_source_fn_ty_payload = work.lifted_owner_source_fn_ty_payload,
            .target_instance = work.target_instance,
            .capture_slots = capture_slots,
            .capture_shape_key = capture_shape_key,
        };
    }

    fn workMemberFromCanonicalMember(member: repr.CanonicalCallableSetMember) repr.CallableGroupWorkMember {
        return .{
            .proc_value = member.proc_value,
            .source_fn_ty_payload = member.source_fn_ty_payload,
            .source_proc = member.source_proc,
            .published_proc_value = member.published_proc_value,
            .published_source_proc = member.published_source_proc,
            .lifted_owner_source_fn_ty_payload = member.lifted_owner_source_fn_ty_payload,
            .target_instance = member.target_instance,
        };
    }

    fn concreteSourcePayloadForKey(
        self: *CallableEmissionAssigner,
        key: canonical.CanonicalTypeKey,
        comptime missing_message: []const u8,
    ) ConcreteSourceType.ConcreteSourceTypeRef {
        if (self.program.concrete_source_types.refForKey(key)) |payload| return payload;
        lambdaInvariant(missing_message);
    }

    fn appendFiniteSetErasePlan(
        self: *CallableEmissionAssigner,
        group_set: *const CallableGroupSet,
        group_key: repr.CanonicalCallableSetKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.CallableValueEmissionPlanId {
        const erase = try self.finiteSetErasePlan(group_set, group_key, provenance);
        defer deinitExecutableSpecializationKeySlice(self.allocator, erase.member_targets);
        return try self.representationStore().appendFiniteSetEraseEmissionPlan(erase);
    }

    fn finiteSetErasePlan(
        self: *CallableEmissionAssigner,
        group_set: *const CallableGroupSet,
        group_key: repr.CanonicalCallableSetKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.FiniteSetErasePlan {
        const first_member = group_set.members.items[0];
        const hidden_capture_key = repr.finiteCallableSetExecValueTypeKey(group_key);
        const hidden_capture_keys = [_]repr.CanonicalExecValueTypeKey{hidden_capture_key};
        const capture_shape_key = repr.captureShapeKeyForExecKeys(&hidden_capture_keys);
        const sig_key = try self.erasedSignatureForTargetProc(
            first_member.proc_value.source_fn_ty,
            self.recordForInstance(first_member.target_instance),
            hidden_capture_key,
        );
        const adapter = repr.ErasedAdapterKey{
            .source_fn_ty = first_member.proc_value.source_fn_ty,
            .callable_set_key = group_key,
            .erased_fn_sig_key = sig_key,
            .capture_shape_key = capture_shape_key,
        };
        const abi = self.representationStore().erased_fn_abis.abiFor(sig_key.abi) orelse {
            lambdaInvariant("lambda-solved finite erased adapter signature referenced missing ABI");
        };
        const member_targets = try finiteErasedAdapterMemberTargetsForAbi(self.allocator, group_set.members.items, abi);
        return .{
            .adapter = adapter,
            .result_ty = repr.erasedCallableExecValueTypeKey(sig_key),
            .member_targets = member_targets,
            .provenance = provenance,
        };
    }

    fn erasedSignatureForTargetProc(
        self: *CallableEmissionAssigner,
        source_fn_ty: canonical.CanonicalTypeKey,
        target_record: *const ProcBuildRecord,
        capture_ty: ?repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.ErasedFnSigKey {
        const target_value_store = self.valueStoreFor(target_record);
        const params = target_value_store.sliceValueSpan(target_record.public_roots.params);
        const source_function = checkedFunctionSourceForKey(&self.program.concrete_source_types, self.artifact_views, &self.program.canonical_names, source_fn_ty);
        if (source_function.function.args.len != params.len) {
            lambdaInvariant("lambda-solved erased adapter source function arity differs from target params");
        }
        const arg_keys: []repr.CanonicalExecValueTypeKey = if (params.len == 0)
            &.{}
        else
            try self.allocator.alloc(repr.CanonicalExecValueTypeKey, params.len);
        defer if (arg_keys.len > 0) self.allocator.free(arg_keys);
        const arg_abis: []canonical.ErasedValueAbi = if (params.len == 0)
            &.{}
        else
            try self.allocator.alloc(canonical.ErasedValueAbi, params.len);
        defer if (arg_abis.len > 0) self.allocator.free(arg_abis);

        for (params, 0..) |param, i| {
            const endpoint = try self.publishTargetErasedBoundaryEndpointFromCheckedRoot(
                target_value_store,
                param,
                source_function.names,
                source_function.view,
                source_function.function.args[i],
            );
            arg_keys[i] = endpoint.key;
            arg_abis[i] = .ordinary_roc_value;
        }
        const ret_endpoint = try self.publishTargetErasedBoundaryEndpointFromCheckedRoot(
            target_value_store,
            target_record.public_roots.ret,
            source_function.names,
            source_function.view,
            source_function.function.ret,
        );
        const abi_key = try self.representationStore().erased_fn_abis.append(self.allocator, .{
            .fixed_arity = @intCast(params.len),
            .arg_exec_keys = arg_keys,
            .ret_exec_key = ret_endpoint.key,
            .arg_abis = arg_abis,
            .capture_arg = .ordinary_roc_value,
        });
        return .{
            .source_fn_ty = source_fn_ty,
            .abi = abi_key,
            .capture_ty = capture_ty,
        };
    }

    fn publishTargetExecutableEndpoint(
        self: *CallableEmissionAssigner,
        target_record: *const ProcBuildRecord,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        return try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_record),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            value,
        );
    }

    fn publishTargetErasedBoundaryEndpoint(
        self: *CallableEmissionAssigner,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        const info = target_value_store.values.items[@intFromEnum(value)];
        const source_payload = info.source_ty_payload orelse {
            lambdaInvariant("lambda-solved erased boundary endpoint value has no checked source type payload");
        };
        const source_view = concreteSourceTypeViewForRef(&self.program.concrete_source_types, self.artifact_views, &self.program.canonical_names, source_payload);
        return try repr.sessionExecutableTypeEndpointForErasedBoundaryTypeIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStore(),
            &self.representationStore().session_executable_type_payloads,
            info.logical_ty,
            info.source_ty,
            source_view.names,
            source_view.view,
            source_view.root,
        );
    }

    fn publishErasedBoundaryEndpointForRootInfo(
        self: *CallableEmissionAssigner,
        info: repr.RepresentationRootTypeInfo,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        if (info.source_root) |source_root| {
            const source = self.sourceViewForRoot(source_root);
            return try repr.sessionExecutableTypeEndpointForErasedBoundaryTypeIntoStore(
                self.allocator,
                &self.program.canonical_names,
                &self.program.row_shapes,
                &self.program.types,
                self.representationStore(),
                &self.representationStore().session_executable_type_payloads,
                info.logical_ty,
                source_root.key,
                source.names,
                source.view,
                source.root,
            );
        }
        return try repr.sessionExecutableTypeEndpointForErasedBoundaryTypeIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStore(),
            &self.representationStore().session_executable_type_payloads,
            info.logical_ty,
            info.source_ty,
            null,
            null,
            null,
        );
    }

    fn sourceViewForRoot(
        self: *CallableEmissionAssigner,
        root: ConcreteSourceType.ConcreteSourceTypeRoot,
    ) ConcreteSourceTypeView {
        return switch (root.source) {
            .local => |local| .{
                .names = &self.program.canonical_names,
                .view = self.program.concrete_source_types.localView(),
                .root = local,
            },
            .artifact => |artifact| artifactCheckedTypeSourceForArtifactViews(self.artifact_views, artifact.artifact, artifact.ty),
        };
    }

    fn publishTargetErasedBoundaryEndpointFromCheckedRoot(
        self: *CallableEmissionAssigner,
        target_value_store: *const repr.ValueInfoStore,
        value: repr.ValueInfoId,
        source_names: *const canonical.CanonicalNameStore,
        source_view: checked_artifact.CheckedTypeStoreView,
        source_root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        const info = target_value_store.values.items[@intFromEnum(value)];
        return try repr.sessionExecutableTypeEndpointForErasedBoundaryTypeIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStore(),
            &self.representationStore().session_executable_type_payloads,
            info.logical_ty,
            checkedTypeRootKey(source_view, source_root),
            source_names,
            source_view,
            source_root,
        );
    }

    fn captureSlotsForTargetValues(
        self: *CallableEmissionAssigner,
        target_record: *const ProcBuildRecord,
        values: []const repr.ValueInfoId,
    ) Allocator.Error![]const repr.CallableSetCaptureSlot {
        const target_value_store = self.valueStoreFor(target_record);
        if (values.len == 0) return &.{};

        const slots = try self.allocator.alloc(repr.CallableSetCaptureSlot, values.len);
        errdefer self.allocator.free(slots);

        for (values, 0..) |value, i| {
            const endpoint = try self.publishTargetExecutableEndpoint(target_record, target_value_store, value);
            const value_info = target_value_store.values.items[@intFromEnum(value)];
            if (isEmptyCanonicalTypeKey(value_info.source_ty)) {
                lambdaInvariant("lambda-solved capture slot reached callable-set construction without an explicit source type key");
            }
            if (value_info.exec_ty) |existing| {
                if (!repr.canonicalExecValueTypeKeyEql(existing.key, endpoint.key)) {
                    lambdaInvariant("lambda-solved capture slot executable endpoint differs from already-published value endpoint");
                }
            }
            slots[i] = .{
                .slot = @intCast(i),
                .source_ty = value_info.source_ty,
                .exec_value_ty = endpoint.key,
            };
        }
        return slots;
    }

    fn callableGroupSet(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) ?*CallableGroupSet {
        const index = self.group_set_index.get(group) orelse return null;
        return &self.group_sets.items[index];
    }

    fn erasedProvenance(
        self: *CallableEmissionAssigner,
        group: repr.RepresentationGroupId,
    ) ?[]const repr.BoxErasureProvenance {
        const index = self.erased_group_index.get(group) orelse return null;
        return self.erased_groups.items[index].provenance.items;
    }

    fn callableGroupForValueInfo(
        self: *CallableEmissionAssigner,
        value_info: repr.ValueInfo,
        callable: ?repr.CallableValueInfo,
    ) ?repr.RepresentationGroupId {
        if (callable) |info| return self.groupForRoot(info.callable_root);
        if (!self.valueHasFunctionType(value_info.logical_ty)) return value_info.solved_group;
        return self.callableGroupForFunctionRoot(value_info.root);
    }

    fn callableGroupForFunctionRoot(
        self: *CallableEmissionAssigner,
        root: repr.RepRootId,
    ) ?repr.RepresentationGroupId {
        const callable_root = self.representationStore().solvedStructuralChildRoot(root, .function_callable) orelse root;
        return self.groupForRoot(callable_root);
    }

    fn callableRootForFunctionRoot(
        self: *CallableEmissionAssigner,
        root: repr.RepRootId,
    ) repr.RepRootId {
        return self.representationStore().solvedStructuralChildRoot(root, .function_callable) orelse root;
    }

    fn groupForRoot(self: *CallableEmissionAssigner, root: repr.RepRootId) ?repr.RepresentationGroupId {
        if (@intFromEnum(root) >= self.representationStore().roots_len) return null;
        return self.representationStore().groupForRoot(root);
    }

    fn endpointRootInSession(
        self: *CallableEmissionAssigner,
        endpoint: repr.RepresentationEndpoint,
    ) ?repr.RepRootId {
        return switch (endpoint) {
            .local => |root| root,
            .procedure_public => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
            .procedure_function_root => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
        };
    }

    fn boxBoundary(
        self: *CallableEmissionAssigner,
        boundary: repr.BoxBoundaryId,
    ) repr.BoxBoundary {
        const index = @intFromEnum(boundary);
        if (index >= self.representationStore().box_boundaries.len) {
            lambdaInvariant("lambda-solved erased requirement referenced missing BoxBoundary");
        }
        return self.representationStore().box_boundaries[index];
    }

    fn recordForInstance(
        self: *CallableEmissionAssigner,
        id: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(id);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved callable emission assignment referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn valueStoreFor(
        self: *CallableEmissionAssigner,
        record: *const ProcBuildRecord,
    ) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(record.value_store)];
    }

    fn representationStoreFor(
        self: *CallableEmissionAssigner,
        record: *const ProcBuildRecord,
    ) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store;
    }
};

const BoxPayloadValueRef = struct {
    record: *const ProcBuildRecord,
    value_store: *const repr.ValueInfoStore,
    value: repr.ValueInfoId,
};

const NominalCapabilityResolution = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    is_root: bool,
    capability: checked_artifact.BoxPayloadCapabilityEntry,
    opaque_proof: ?checked_artifact.OpaqueAtomicProofEntry,
};

fn finalizeBoxPayloadRepresentationPlans(
    program: *Program,
    records: []const ProcBuildRecord,
    artifact_views: ArtifactViews,
) Allocator.Error!void {
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        {
            var finalizer = BoxPayloadPlanFinalizer{
                .allocator = program.allocator,
                .program = program,
                .records = records,
                .artifact_views = artifact_views,
                .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
                .session = session,
                .active_payload_plans = std.AutoHashMap(repr.SessionExecutableTypePayloadId, repr.BoxPayloadRepresentationPlanId).init(program.allocator),
                .completed_payload_plans = std.AutoHashMap(repr.SessionExecutableTypePayloadId, repr.BoxPayloadRepresentationPlanId).init(program.allocator),
            };
            defer finalizer.deinit();
            try finalizer.finalize();
        }
    }
}

const BoxPayloadPlanFinalizer = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    artifact_views: ArtifactViews,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,
    active_payload_plans: std.AutoHashMap(repr.SessionExecutableTypePayloadId, repr.BoxPayloadRepresentationPlanId),
    completed_payload_plans: std.AutoHashMap(repr.SessionExecutableTypePayloadId, repr.BoxPayloadRepresentationPlanId),

    fn deinit(self: *BoxPayloadPlanFinalizer) void {
        self.active_payload_plans.deinit();
        self.completed_payload_plans.deinit();
    }

    fn finalize(self: *BoxPayloadPlanFinalizer) Allocator.Error!void {
        for (self.representationStore().box_boundaries, 0..) |boundary, raw_boundary| {
            self.active_payload_plans.clearRetainingCapacity();
            self.completed_payload_plans.clearRetainingCapacity();
            const plan_start_len = self.representationStore().box_payload_plans.len;

            const boundary_id: repr.BoxBoundaryId = @enumFromInt(@as(u32, @intCast(raw_boundary)));
            const payload_root = switch (boundary.direction) {
                .box => boundary.source_root,
                .unbox => boundary.boundary_root,
            };
            const payload_value = self.valueForRoot(payload_root) orelse {
                lambdaInvariant("lambda-solved BoxBoundary payload root has no published value metadata");
            };
            const endpoint = try repr.sessionExecutableTypeEndpointForValue(
                self.allocator,
                &self.program.canonical_names,
                &self.program.row_shapes,
                &self.program.types,
                self.representationStore(),
                payload_value.value_store,
                payload_value.value,
            );
            const root_plan = try self.planForPayload(endpoint.ty);
            var visiting = std.AutoHashMap(repr.BoxPayloadRepresentationPlanId, void).init(self.allocator);
            defer visiting.deinit();
            const plan = if (try self.planIdNeedsMaterialization(root_plan, &visiting))
                repr.BoxPayloadRepresentationPlan{ .recursive_ref = root_plan }
            else blk: {
                try self.representationStore().truncateBoxPayloadPlans(plan_start_len);
                break :blk repr.BoxPayloadRepresentationPlan.unchanged;
            };
            self.representationStore().setBoxBoundaryPayloadPlan(boundary_id, plan);
        }
    }

    fn planForPayload(
        self: *BoxPayloadPlanFinalizer,
        payload_ref: repr.SessionExecutableTypePayloadRef,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlanId {
        if (self.completed_payload_plans.get(payload_ref.payload)) |completed| return completed;
        if (self.active_payload_plans.get(payload_ref.payload)) |active| return active;

        const plan_id = try self.representationStore().appendBoxPayloadPlan(.unchanged);
        try self.active_payload_plans.put(payload_ref.payload, plan_id);
        const plan = try self.planForPayloadInner(payload_ref);
        self.representationStore().setBoxPayloadPlan(plan_id, plan);
        _ = self.active_payload_plans.remove(payload_ref.payload);
        try self.completed_payload_plans.put(payload_ref.payload, plan_id);
        return plan_id;
    }

    fn planForPayloadInner(
        self: *BoxPayloadPlanFinalizer,
        payload_ref: repr.SessionExecutableTypePayloadRef,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        return switch (self.representationStore().session_executable_type_payloads.get(payload_ref.payload)) {
            .pending => lambdaInvariant("lambda-solved boxed payload planning reached pending executable payload"),
            .primitive,
            .callable_set,
            => .unchanged,
            .vacant_callable_slot => lambdaInvariant("lambda-solved boxed payload planning reached vacant callable slot without explicit value metadata"),
            .erased_fn => |erased| .{ .function_erased = .{
                .source_fn_ty = erased.sig_key.source_fn_ty,
                .sig_key = erased.sig_key,
            } },
            .record => |record| try self.recordPlan(record),
            .tuple => |items| try self.tuplePlan(items),
            .tag_union => |tag_union| try self.tagUnionPlan(tag_union),
            .list => |child| try self.listPlan(child),
            .box => |child| try self.nestedBoxPlan(child),
            .nominal => |nominal| try self.nominalPlan(nominal),
            .recursive_ref => lambdaInvariant("lambda-solved boxed payload planning reached recursive executable payload without a published recursion binder"),
        };
    }

    fn recordPlan(
        self: *BoxPayloadPlanFinalizer,
        record: repr.SessionExecutableRecordPayload,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        if (record.fields.len == 0) return .unchanged;
        const fields = try self.allocator.alloc(repr.BoxPayloadFieldPlan, record.fields.len);
        errdefer self.allocator.free(fields);
        for (record.fields, 0..) |field, i| {
            fields[i] = .{
                .field = field.field,
                .plan = try self.planForPayload(field.ty),
            };
        }
        return .{ .record = fields };
    }

    fn tuplePlan(
        self: *BoxPayloadPlanFinalizer,
        items: []const repr.SessionExecutableTupleElemPayload,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        if (items.len == 0) return .unchanged;
        const elems = try self.allocator.alloc(repr.BoxPayloadTupleElemPlan, items.len);
        errdefer self.allocator.free(elems);
        for (items, 0..) |item, i| {
            elems[i] = .{
                .index = item.index,
                .plan = try self.planForPayload(item.ty),
            };
        }
        return .{ .tuple = elems };
    }

    fn tagUnionPlan(
        self: *BoxPayloadPlanFinalizer,
        tag_union: repr.SessionExecutableTagUnionPayload,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        if (tag_union.variants.len == 0) return .unchanged;
        const variants = try self.allocator.alloc(repr.BoxPayloadTagPlan, tag_union.variants.len);
        for (variants) |*variant| variant.* = .{ .tag = undefined, .payloads = &.{} };
        errdefer {
            for (variants) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(variants);
        }

        for (tag_union.variants, 0..) |variant, i| {
            const payloads = try self.allocator.alloc(repr.BoxPayloadTagPayloadPlan, variant.payloads.len);
            errdefer self.allocator.free(payloads);
            for (variant.payloads, 0..) |payload, payload_i| {
                payloads[payload_i] = .{
                    .payload = payload.payload,
                    .plan = try self.planForPayload(payload.ty),
                };
            }
            variants[i] = .{
                .tag = variant.tag,
                .payloads = payloads,
            };
        }
        return .{ .tag_union = variants };
    }

    fn listPlan(
        self: *BoxPayloadPlanFinalizer,
        child: repr.SessionExecutableTypePayloadChild,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        return .{ .list = try self.planForPayload(child.ty) };
    }

    fn nestedBoxPlan(
        self: *BoxPayloadPlanFinalizer,
        child: repr.SessionExecutableTypePayloadChild,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        return .{ .nested_box = try self.planForPayload(child.ty) };
    }

    fn nominalPlan(
        self: *BoxPayloadPlanFinalizer,
        nominal: repr.SessionExecutableNominalPayload,
    ) Allocator.Error!repr.BoxPayloadRepresentationPlan {
        const capability = self.nominalCapability(nominal) orelse {
            lambdaInvariant("lambda-solved imported/private nominal boxed payload traversal has no published interface capability");
        };
        if (capability.opaque_proof) |proof| {
            return .{ .nominal = .{ .opaque_atomic = .{
                .nominal = nominal.nominal,
                .source_ty = nominal.source_ty,
                .proof = .{
                    .artifact = capability.artifact,
                    .proof = proof.id,
                },
            } } };
        }

        const backing_plan_id = try self.planForPayload(nominal.backing);

        const nominal_plan: repr.NominalPayloadRepresentation = if (capability.is_root)
            .{ .transparent_backing = .{
                .nominal = nominal.nominal,
                .source_ty = nominal.source_ty,
                .backing_plan = backing_plan_id,
            } }
        else
            .{ .imported_capability = .{
                .nominal = nominal.nominal,
                .source_ty = nominal.source_ty,
                .capability = .{
                    .artifact = capability.artifact,
                    .capability = capability.capability.id,
                },
                .backing_plan = backing_plan_id,
            } };
        return .{ .nominal = nominal_plan };
    }

    fn nominalCapability(
        self: *BoxPayloadPlanFinalizer,
        nominal: repr.SessionExecutableNominalPayload,
    ) ?NominalCapabilityResolution {
        if (self.capabilityInArtifact(
            self.artifact_views.root.artifact.key,
            true,
            &self.artifact_views.root.artifact.interface_capabilities,
            nominal.source_ty,
        )) |capability| return capability;

        for (self.artifact_views.root.relation_artifacts) |related| {
            if (self.capabilityInArtifact(related.key, false, related.interface_capabilities, nominal.source_ty)) |capability| return capability;
        }
        for (self.artifact_views.imports) |imported| {
            if (self.capabilityInArtifact(imported.key, false, imported.interface_capabilities, nominal.source_ty)) |capability| return capability;
        }
        return null;
    }

    fn capabilityInArtifact(
        _: *BoxPayloadPlanFinalizer,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        is_root: bool,
        capabilities: *const checked_artifact.ModuleInterfaceCapabilities,
        source_ty: canonical.CanonicalTypeKey,
    ) ?NominalCapabilityResolution {
        const capability = capabilities.boxPayloadCapabilityForSource(source_ty) orelse return null;
        return .{
            .artifact = artifact,
            .is_root = is_root,
            .capability = capability,
            .opaque_proof = capabilities.opaqueAtomicProofForSource(source_ty),
        };
    }

    fn planIdNeedsMaterialization(
        self: *BoxPayloadPlanFinalizer,
        plan_id: repr.BoxPayloadRepresentationPlanId,
        visiting: *std.AutoHashMap(repr.BoxPayloadRepresentationPlanId, void),
    ) Allocator.Error!bool {
        if (visiting.contains(plan_id)) return false;
        const index: usize = @intFromEnum(plan_id);
        if (index >= self.representationStore().box_payload_plans.len) {
            lambdaInvariant("lambda-solved boxed payload plan referenced missing plan id");
        }
        try visiting.put(plan_id, {});
        defer _ = visiting.remove(plan_id);
        return try self.planNeedsMaterialization(self.representationStore().box_payload_plans[index], visiting);
    }

    fn planNeedsMaterialization(
        self: *BoxPayloadPlanFinalizer,
        plan: repr.BoxPayloadRepresentationPlan,
        visiting: *std.AutoHashMap(repr.BoxPayloadRepresentationPlanId, void),
    ) Allocator.Error!bool {
        return switch (plan) {
            .unchanged => false,
            .function_erased => true,
            .record => |fields| for (fields) |field| {
                if (try self.planIdNeedsMaterialization(field.plan, visiting)) break true;
            } else false,
            .tuple => |items| for (items) |item| {
                if (try self.planIdNeedsMaterialization(item.plan, visiting)) break true;
            } else false,
            .tag_union => |variants| blk: {
                for (variants) |variant| {
                    for (variant.payloads) |payload| {
                        if (try self.planIdNeedsMaterialization(payload.plan, visiting)) break :blk true;
                    }
                }
                break :blk false;
            },
            .list => |child| try self.planIdNeedsMaterialization(child, visiting),
            .nested_box => |child| try self.planIdNeedsMaterialization(child, visiting),
            .nominal => |nominal| switch (nominal) {
                .transparent_backing => |backing| try self.planIdNeedsMaterialization(backing.backing_plan, visiting),
                .imported_capability => |backing| try self.planIdNeedsMaterialization(backing.backing_plan, visiting),
                .opaque_atomic,
                .hosted_abi,
                .platform_abi,
                => false,
            },
            .recursive_ref => |ref| try self.planIdNeedsMaterialization(ref, visiting),
        };
    }

    fn valueForRoot(self: *BoxPayloadPlanFinalizer, root: repr.RepRootId) ?BoxPayloadValueRef {
        for (self.session.members) |instance| {
            const record = self.recordForInstance(instance);
            const value_store = self.valueStoreFor(record);
            for (value_store.values.items, 0..) |value, raw_value| {
                if (value.root == root) {
                    return .{
                        .record = record,
                        .value_store = value_store,
                        .value = @enumFromInt(@as(u32, @intCast(raw_value))),
                    };
                }
            }
        }
        return null;
    }

    fn recordForInstance(
        self: *BoxPayloadPlanFinalizer,
        id: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(id);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved boxed payload plan referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn valueStoreFor(
        self: *BoxPayloadPlanFinalizer,
        record: *const ProcBuildRecord,
    ) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(record.value_store)];
    }

    fn representationStore(self: *BoxPayloadPlanFinalizer) *repr.RepresentationStore {
        return &self.session.representation_store;
    }
};

fn edgePropagatesBoxErasure(kind: repr.RepresentationEdgeKind) bool {
    return switch (kind) {
        .value_alias,
        .value_move,
        .branch_join,
        .loop_phi,
        .mutable_version,
        .call_arg,
        .call_result,
        .capture_value,
        => false,
        .child => true,
    };
}

fn representationEdgeEql(
    left: repr.RepresentationEdge,
    right: repr.RepresentationEdge,
) bool {
    return representationEndpointEql(left.from, right.from) and
        representationEndpointEql(left.to, right.to) and
        representationEdgeKindEql(left.kind, right.kind);
}

fn representationEndpointEql(
    left: repr.RepresentationEndpoint,
    right: repr.RepresentationEndpoint,
) bool {
    return switch (left) {
        .local => |a| switch (right) {
            .local => |b| a == b,
            else => false,
        },
        .procedure_public => |a| switch (right) {
            .procedure_public => |b| a.instance == b.instance and a.value == b.value and a.rep_root == b.rep_root,
            else => false,
        },
        .procedure_function_root => |a| switch (right) {
            .procedure_function_root => |b| a.instance == b.instance and a.rep_root == b.rep_root,
            else => false,
        },
    };
}

fn representationEdgeKindEql(
    left: repr.RepresentationEdgeKind,
    right: repr.RepresentationEdgeKind,
) bool {
    return switch (left) {
        .value_alias => switch (right) {
            .value_alias => true,
            else => false,
        },
        .value_move => switch (right) {
            .value_move => true,
            else => false,
        },
        .branch_join => switch (right) {
            .branch_join => true,
            else => false,
        },
        .loop_phi => switch (right) {
            .loop_phi => true,
            else => false,
        },
        .mutable_version => switch (right) {
            .mutable_version => true,
            else => false,
        },
        .call_arg => |a| switch (right) {
            .call_arg => |b| a == b,
            else => false,
        },
        .call_result => switch (right) {
            .call_result => true,
            else => false,
        },
        .capture_value => switch (right) {
            .capture_value => true,
            else => false,
        },
        .child => |a| switch (right) {
            .child => |b| representationChildKindMatches(a, b),
            else => false,
        },
    };
}

fn representationChildKindMatches(
    left: repr.RepresentationChildKind,
    right: repr.RepresentationChildKind,
) bool {
    return switch (left) {
        .function_arg => |a| switch (right) {
            .function_arg => |b| a == b,
            else => false,
        },
        .function_return => switch (right) {
            .function_return => true,
            else => false,
        },
        .function_callable => switch (right) {
            .function_callable => true,
            else => false,
        },
        .record_field => |a| switch (right) {
            .record_field => |b| a == b,
            else => false,
        },
        .tuple_elem => |a| switch (right) {
            .tuple_elem => |b| a == b,
            else => false,
        },
        .tag_payload => |a| switch (right) {
            .tag_payload => |b| a == b,
            else => false,
        },
        .list_elem => switch (right) {
            .list_elem => true,
            else => false,
        },
        .box_payload => switch (right) {
            .box_payload => true,
            else => false,
        },
        .nominal_backing => |a| switch (right) {
            .nominal_backing => |b| a.module_name == b.module_name and a.type_name == b.type_name,
            else => false,
        },
    };
}

fn boxErasureProvenanceEql(
    a: repr.BoxErasureProvenance,
    b: repr.BoxErasureProvenance,
) bool {
    return switch (a) {
        .local_box_boundary => |left| switch (b) {
            .local_box_boundary => |right| left == right,
            .const_graph_box, .promoted_wrapper => false,
        },
        .const_graph_box => |left| switch (b) {
            .const_graph_box => |right| std.mem.eql(u8, left.artifact.bytes[0..], right.artifact.bytes[0..]) and left.box_plan == right.box_plan,
            .local_box_boundary,
            .promoted_wrapper,
            => false,
        },
        .promoted_wrapper => |left| switch (b) {
            .const_graph_box, .local_box_boundary => false,
            .promoted_wrapper => |right| canonical.mirProcedureRefEql(left, right),
        },
    };
}

fn boxErasureProvenanceSliceEql(
    a: []const repr.BoxErasureProvenance,
    b: []const repr.BoxErasureProvenance,
) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (!boxErasureProvenanceEql(left, right)) return false;
    }
    return true;
}

fn boxErasureProvenanceSliceContains(
    items: []const repr.BoxErasureProvenance,
    needle: repr.BoxErasureProvenance,
) bool {
    for (items) |item| {
        if (boxErasureProvenanceEql(item, needle)) return true;
    }
    return false;
}

fn boxErasureProvenanceSetEql(
    a: []const repr.BoxErasureProvenance,
    b: []const repr.BoxErasureProvenance,
) bool {
    if (a.len != b.len) return false;
    for (a) |item| {
        if (!boxErasureProvenanceSliceContains(b, item)) return false;
    }
    return true;
}

fn boxErasureProvenanceSliceContainsAll(
    haystack: []const repr.BoxErasureProvenance,
    needles: []const repr.BoxErasureProvenance,
) bool {
    for (needles) |needle| {
        if (!boxErasureProvenanceSliceContains(haystack, needle)) return false;
    }
    return true;
}

fn alreadyErasedCallablePlanEquivalent(
    a: repr.AlreadyErasedCallablePlan,
    b: repr.AlreadyErasedCallablePlan,
) bool {
    return canonical.erasedFnAbiKeyEql(a.sig_key.abi, b.sig_key.abi) and
        repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key) and
        alreadyErasedResultMatchesSig(a) and
        alreadyErasedResultMatchesSig(b) and
        alreadyErasedCapturePlanEquivalent(a.capture, b.capture);
}

fn alreadyErasedCallablePlanMismatch(
    a: repr.AlreadyErasedCallablePlan,
    b: repr.AlreadyErasedCallablePlan,
) []const u8 {
    if (!canonical.erasedFnAbiKeyEql(a.sig_key.abi, b.sig_key.abi)) return "abi_key";
    if (!repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key)) return "capture_shape_key";
    if (!alreadyErasedResultMatchesSig(a) or !alreadyErasedResultMatchesSig(b)) return "result_ty";
    if (!alreadyErasedCapturePlanEquivalent(a.capture, b.capture)) return "capture";
    return "none";
}

fn alreadyErasedResultMatchesSig(plan: repr.AlreadyErasedCallablePlan) bool {
    return repr.canonicalExecValueTypeKeyEql(plan.result_ty, repr.erasedCallableExecValueTypeKey(plan.sig_key));
}

fn alreadyErasedCapturePlanEquivalent(
    a: repr.AlreadyErasedCapturePlan,
    b: repr.AlreadyErasedCapturePlan,
) bool {
    return switch (a) {
        .none => b == .none,
        .zero_sized_ty => |left| switch (b) {
            .zero_sized_ty => |right| left == right,
            else => false,
        },
        .executable_key => |left| switch (b) {
            .executable_key => |right| repr.canonicalExecValueTypeKeyEql(left, right),
            else => false,
        },
        .materialized_capture => |left| switch (b) {
            .materialized_capture => |right| artifactKeyEql(left.owner, right.owner) and erasedCaptureMaterializationPlanEql(left.capture, right.capture),
            else => false,
        },
        .value => |left| switch (b) {
            .value => |right| left == right,
            else => false,
        },
    };
}

fn erasedCaptureMaterializationPlanEql(
    left: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
    right: checked_artifact.ErasedCaptureExecutableMaterializationPlan,
) bool {
    return switch (left) {
        .none => right == .none,
        .zero_sized_typed => |left_key| switch (right) {
            .zero_sized_typed => |right_key| repr.canonicalExecValueTypeKeyEql(left_key, right_key),
            else => false,
        },
        .node => |left_node| switch (right) {
            .node => |right_node| left_node == right_node,
            else => false,
        },
    };
}

fn mirProcedureBodyIdentityEql(
    a: canonical.MirProcedureRef,
    b: canonical.MirProcedureRef,
) bool {
    return canonical.procedureValueRefEql(a.proc, b.proc) and
        canonical.callableProcedureTemplateRefEql(a.callable.template, b.callable.template);
}

fn callableSetMemberEquivalent(
    a: repr.CanonicalCallableSetMember,
    b: repr.CanonicalCallableSetMember,
) bool {
    if (!callableSetMemberSameIdentity(a, b)) return false;
    if (a.source_fn_ty_payload != b.source_fn_ty_payload) return false;
    if ((a.published_proc_value == null) != (b.published_proc_value == null)) return false;
    if (a.published_proc_value) |left| {
        if (!canonical.procedureCallableRefEql(left, b.published_proc_value.?)) return false;
    }
    if ((a.published_source_proc == null) != (b.published_source_proc == null)) return false;
    if (a.published_source_proc) |left| {
        if (!canonical.mirProcedureRefEql(left, b.published_source_proc.?)) return false;
    }
    if (a.lifted_owner_source_fn_ty_payload != b.lifted_owner_source_fn_ty_payload) return false;
    if (!repr.captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key)) return false;
    if (a.capture_slots.len != b.capture_slots.len) return false;
    for (a.capture_slots, b.capture_slots) |left, right| {
        if (left.slot != right.slot) return false;
        if (!repr.canonicalTypeKeyEql(left.source_ty, right.source_ty)) return false;
        if (!repr.canonicalExecValueTypeKeyEql(left.exec_value_ty, right.exec_value_ty)) return false;
    }
    return true;
}

fn callableSetMemberSameIdentity(
    a: repr.CanonicalCallableSetMember,
    b: repr.CanonicalCallableSetMember,
) bool {
    if (!canonical.mirProcedureRefEql(a.source_proc, b.source_proc)) return false;
    if (!canonical.procedureCallableRefEql(a.proc_value, b.proc_value)) return false;
    if (a.target_instance != b.target_instance) return false;
    return true;
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
        .erased_proc_value_adapter_arg => |arg| switch (b) {
            .erased_proc_value_adapter_arg => |other| arg.emission_plan == other.emission_plan and
                arg.source_value == other.source_value and
                canonical.procedureCallableRefEql(arg.proc_value, other.proc_value) and
                repr.erasedFnSigKeyEql(arg.erased_fn_sig_key, other.erased_fn_sig_key) and
                arg.index == other.index,
            else => false,
        },
        .erased_finite_adapter_arg => |arg| switch (b) {
            .erased_finite_adapter_arg => |other| repr.erasedAdapterKeyEql(arg.adapter, other.adapter) and
                repr.callableSetKeyEql(arg.member.callable_set_key, other.member.callable_set_key) and
                arg.member.member_index == other.member.member_index and
                arg.index == other.index,
            else => false,
        },
        .erased_finite_adapter_capture => |capture| switch (b) {
            .erased_finite_adapter_capture => |other| repr.erasedAdapterKeyEql(capture.adapter, other.adapter) and
                repr.callableSetKeyEql(capture.member.callable_set_key, other.member.callable_set_key) and
                capture.member.member_index == other.member.member_index and
                capture.slot == other.slot,
            else => false,
        },
        .erased_finite_adapter_result => |result| switch (b) {
            .erased_finite_adapter_result => |other| repr.erasedAdapterKeyEql(result.adapter, other.adapter) and
                repr.callableSetKeyEql(result.member.callable_set_key, other.member.callable_set_key) and
                result.member.member_index == other.member.member_index,
            else => false,
        },
        .call_raw_result => |call| switch (b) {
            .call_raw_result => |other| call == other,
            else => false,
        },
        .projection_slot => |projection| switch (b) {
            .projection_slot => |other| projection == other,
            else => false,
        },
        .consumer_use => |owner| switch (b) {
            .consumer_use => |other| consumerUseOwnerEql(owner, other),
            else => false,
        },
        .transform_child => |child| switch (b) {
            .transform_child => |other| child.scope == other.scope and
                child.side == other.side and
                child.path == other.path,
            else => false,
        },
    };
}

fn consumerUseOwnerEql(
    a: repr.ConsumerUseOwner,
    b: repr.ConsumerUseOwner,
) bool {
    return switch (a) {
        .return_value => |ret| switch (b) {
            .return_value => |other| ret == other,
            else => false,
        },
        .call_arg => |arg| switch (b) {
            .call_arg => |other| arg.call == other.call and arg.arg_index == other.arg_index,
            else => false,
        },
        .record_field => |field| switch (b) {
            .record_field => |other| field.parent == other.parent and field.field == other.field,
            else => false,
        },
        .tuple_elem => |elem| switch (b) {
            .tuple_elem => |other| elem.parent == other.parent and elem.index == other.index,
            else => false,
        },
        .tag_payload => |payload| switch (b) {
            .tag_payload => |other| payload.parent == other.parent and
                payload.tag == other.tag and
                payload.payload == other.payload,
            else => false,
        },
        .list_elem => |elem| switch (b) {
            .list_elem => |other| elem.parent == other.parent and elem.index == other.index,
            else => false,
        },
        .nominal_backing => |backing| switch (b) {
            .nominal_backing => |other| backing.parent == other.parent and
                backing.nominal.module_name == other.nominal.module_name and
                backing.nominal.type_name == other.nominal.type_name,
            else => false,
        },
        .if_branch_result => |branch| switch (b) {
            .if_branch_result => |other| branch.parent == other.parent and
                branch.join == other.join and
                branch.branch == other.branch,
            else => false,
        },
        .source_match_branch_result => |branch| switch (b) {
            .source_match_branch_result => |other| branch.parent == other.parent and
                branch.join == other.join and
                branch.branch_index == other.branch_index,
            else => false,
        },
    };
}

fn finalizeValueTransformBoundaries(program: *Program, artifact_views: ArtifactViews) Allocator.Error!void {
    for (program.proc_instances.items, 0..) |*instance, raw_instance| {
        if (!instance.materialized) continue;
        var finalizer = ValueTransformFinalizer{
            .allocator = program.allocator,
            .program = program,
            .artifact_views = artifact_views,
            .instance_id = @enumFromInt(@as(u32, @intCast(raw_instance))),
            .instance = instance,
        };
        try finalizer.finalizeValueAliases();
        try finalizer.finalizeProjections();
        try finalizer.finalizeCallSites();
        try finalizer.finalizeCallableConstructions();
        try finalizer.finalizeProcValueErasePlans();
        try finalizer.finalizeFiniteSetErasePlans();
        try finalizer.finalizeJoins();
        try finalizer.finalizeConstructionConsumerUses();
        finalizer.verifyReturnsFinalized();
    }
}

const ValueTransformFinalizer = struct {
    allocator: Allocator,
    program: *Program,
    artifact_views: ArtifactViews,
    instance_id: repr.ProcRepresentationInstanceId,
    instance: *const repr.ProcRepresentationInstance,

    fn finalizeValueAliases(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.values.items, 0..) |*value, raw_value| {
            if (!value_store.valueSourceMatchBranchReachable(value.*)) {
                if (value.value_alias_transform != null) {
                    lambdaInvariant("lambda-solved unreachable source-match value alias has an executable transform");
                }
                continue;
            }
            if (value.pending_comptime_dependency_origin) {
                if (value.value_alias_transform != null) {
                    lambdaInvariant("lambda-solved pending compile-time dependency value alias has an executable transform");
                }
                continue;
            }
            if (!value.value_alias_needs_executable_transform) {
                if (value.value_alias_transform != null) {
                    lambdaInvariant("lambda-solved non-materialized value alias has an executable transform");
                }
                continue;
            }
            const source = value.value_alias_source orelse {
                lambdaInvariant("lambda-solved materialized value alias has no alias source");
            };
            if (value.value_alias_transform != null) {
                lambdaInvariant("lambda-solved value alias transform was finalized twice");
            }
            const result: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
            const to = try self.localEndpoint(result);
            if (self.endpointIsVacantCallableSlot(to)) {
                lambdaInvariant("lambda-solved materialized value alias endpoint is vacant");
            }
            const from = try self.localEndpoint(source);
            if (self.endpointIsVacantCallableSlot(from)) {
                lambdaInvariant("lambda-solved executable value alias source is vacant but result is materialized");
            }
            const kind: repr.ValueTransformBoundaryKind = .{ .value_alias = .{
                .source = source,
                .result = result,
            } };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            const boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = source,
                .to_value = result,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
            value.value_alias_transform = boundary;
        }
    }

    fn finalizeProjections(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.projections.items, 0..) |*projection, raw_projection| {
            const projection_id: repr.ProjectionInfoId = @enumFromInt(@as(u32, @intCast(raw_projection)));
            const result_info = value_store.values.items[@intFromEnum(projection.result)];
            if (!value_store.valueSourceMatchBranchReachable(result_info)) {
                if (projection.result_transform != null) {
                    lambdaInvariant("lambda-solved unreachable projection has an executable transform");
                }
                continue;
            }
            if (result_info.pending_comptime_dependency_origin) {
                if (projection.result_transform != null) {
                    lambdaInvariant("lambda-solved pending compile-time dependency projection has an executable transform");
                }
                continue;
            }
            if (projection.result_transform != null) {
                lambdaInvariant("lambda-solved projection transform was finalized twice");
            }

            const from = try self.projectionSlotEndpoint(projection_id, projection.*);
            projection.endpoint_kind = from.kind;
            const to = try self.localEndpoint(projection.result);
            const kind: repr.ValueTransformBoundaryKind = .{ .projection_result = projection_id };
            const transform = try self.appendExistingValueTransform(kind, from.endpoint, to);
            const boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = projection.source,
                .to_value = projection.result,
                .from_endpoint = from.endpoint,
                .to_endpoint = to,
                .transform = transform,
            });
            projection.result_transform = boundary;
        }
    }

    fn endpointIsVacantCallableSlot(
        self: *ValueTransformFinalizer,
        endpoint: repr.SessionExecutableValueEndpoint,
    ) bool {
        return switch (self.sessionPayload(endpoint.exec_ty.ty)) {
            .vacant_callable_slot => true,
            else => false,
        };
    }

    fn finalizeCallSites(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.call_sites.items, 0..) |*call_site, raw_call_site| {
            if (!value_store.callSiteSourceMatchBranchReachable(call_site.*)) continue;
            const call_site_id: repr.CallSiteInfoId = @enumFromInt(@as(u32, @intCast(raw_call_site)));
            const dispatch = call_site.dispatch orelse {
                const callee = call_site.callee orelse lambdaInvariant("lambda-solved unresolved call site has no callee");
                try self.finalizePendingCallValue(call_site_id, call_site, callee);
                continue;
            };
            switch (dispatch) {
                .call_proc => |target| try self.finalizeCallProc(call_site_id, call_site, target),
                .call_value_finite => |plan| try self.verifyFinalizedCallValueFinite(call_site_id, call_site, plan),
                .call_value_erased => |sig_key| try self.finalizeCallValueErased(call_site_id, call_site, sig_key),
                .pending_comptime_dependency_call => {},
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
            const reachable_inputs = try self.allocator.alloc(repr.JoinInputInfo, inputs.len);
            defer self.allocator.free(reachable_inputs);

            const result_to = try self.localEndpoint(join.result);
            var input_len: usize = 0;
            for (inputs) |input| {
                if (!self.joinInputReachable(input)) continue;
                const from = try self.localEndpoint(input.value);
                const kind = self.joinBoundaryKind(join_id, input.source);
                const transform = try self.appendExistingValueTransform(kind, from, result_to);
                reachable_inputs[input_len] = input;
                boundaries[input_len] = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = kind,
                    .from_value = input.value,
                    .to_value = join.result,
                    .from_endpoint = from,
                    .to_endpoint = result_to,
                    .transform = transform,
                });
                input_len += 1;
            }

            join.inputs = try self.valueStore().addJoinInputSpan(reachable_inputs[0..input_len]);
            join.input_transforms = try self.valueStore().addValueTransformBoundarySpan(boundaries[0..input_len]);
        }
    }

    fn joinInputReachable(
        self: *ValueTransformFinalizer,
        input: repr.JoinInputInfo,
    ) bool {
        return switch (input.source) {
            .source_match_branch => |branch| self.valueStore().sourceMatchBranchReachable(.{
                .match = branch.match,
                .branch = branch.branch,
                .alternative = branch.alternative,
            }),
            .if_branch,
            .loop_phi,
            => true,
        };
    }

    fn verifyReturnsFinalized(self: *ValueTransformFinalizer) void {
        const value_store = self.valueStore();
        for (value_store.returns.items) |ret| {
            const value = value_store.values.items[@intFromEnum(ret.value)];
            if (!value_store.valueSourceMatchBranchReachable(value)) continue;
            if (value.pending_comptime_dependency_origin) continue;
            if (ret.consumer_use == null) {
                lambdaInvariant("lambda-solved value transform finalization reached a return with no consumer-use plan");
            }
        }
    }

    fn finalizeConstructionConsumerUses(self: *ValueTransformFinalizer) Allocator.Error!void {
        const body = self.procBodyExprOrNull() orelse return;
        try self.finalizeExprConstructionUsesAtEndpoint(body, null);
    }

    fn procBodyExprOrNull(self: *ValueTransformFinalizer) ?Ast.ExprId {
        for (self.program.procs.items) |proc| {
            if (proc.representation_instance != self.instance_id) continue;
            const def = self.program.ast.defs.items[@intFromEnum(proc.body)];
            return switch (def.value) {
                .fn_ => |fn_| fn_.body,
                .val => |body| body,
                .run => |run_def| run_def.body,
                .hosted_fn => null,
            };
        }
        return null;
    }

    fn recordAssemblyEvalForFinalizer(
        evals: []const Ast.RecordFieldEval,
        assembly: Ast.RecordFieldAssembly,
    ) Ast.RecordFieldEval {
        if (assembly.eval_index >= evals.len) {
            lambdaInvariant("lambda-solved consumer-use record assembly referenced eval index outside eval order");
        }
        const evaluated = evals[assembly.eval_index];
        if (evaluated.field != assembly.field) {
            lambdaInvariant("lambda-solved consumer-use record assembly field disagreed with eval-order field");
        }
        return evaluated;
    }

    fn tagAssemblyEvalForFinalizer(
        evals: []const Ast.TagPayloadEval,
        assembly: Ast.TagPayloadAssembly,
    ) Ast.TagPayloadEval {
        if (assembly.eval_index >= evals.len) {
            lambdaInvariant("lambda-solved consumer-use tag assembly referenced eval index outside eval order");
        }
        const evaluated = evals[assembly.eval_index];
        if (evaluated.payload != assembly.payload) {
            lambdaInvariant("lambda-solved consumer-use tag assembly payload disagreed with eval-order payload");
        }
        return evaluated;
    }

    fn finalizeExprConstructionUsesAtEndpoint(
        self: *ValueTransformFinalizer,
        expr_id: Ast.ExprId,
        expected: ?repr.SessionExecutableValueEndpoint,
    ) Allocator.Error!void {
        return try self.finalizeExprConstructionUsesAtEndpointWithProvenance(expr_id, expected, &.{});
    }

    fn finalizeExprConstructionUsesAtEndpointWithProvenance(
        self: *ValueTransformFinalizer,
        expr_id: Ast.ExprId,
        expected: ?repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!void {
        const expr = self.program.ast.exprs.items[@intFromEnum(expr_id)];
        const value_info = self.valueStore().values.items[@intFromEnum(expr.value_info)];
        if (!self.valueStore().valueSourceMatchBranchReachable(value_info)) return;
        if (value_info.pending_comptime_dependency_origin) return;
        const parent_endpoint = expected orelse try self.localEndpoint(expr.value_info);
        switch (expr.data) {
            .record => |record| {
                const evals = self.program.ast.record_field_evals.items[record.eval_order.start..][0..record.eval_order.len];
                const fields = self.program.ast.record_field_assemblies.items[record.assembly_order.start..][0..record.assembly_order.len];
                for (fields) |field| {
                    const field_value = recordAssemblyEvalForFinalizer(evals, field).value;
                    const owner: repr.ConsumerUseOwner = .{ .record_field = .{
                        .parent = expr.value_info,
                        .field = field.field,
                    } };
                    const child_endpoint = try self.recordFieldConsumerEndpoint(parent_endpoint, owner, field.field);
                    const use_id = try self.publishConsumerUseWithProvenance(field_value, owner, child_endpoint, provenance);
                    self.setRecordFieldConsumerUse(expr.value_info, field.field, use_id);
                    if (self.consumerUsePushesEndpoint(use_id)) {
                        try self.finalizeExprConstructionUsesAtEndpointWithProvenance(field_value, child_endpoint, provenance);
                    }
                }
            },
            .tag => |tag| {
                const evals = self.program.ast.tag_payload_evals.items[tag.eval_order.start..][0..tag.eval_order.len];
                const payloads = self.program.ast.tag_payload_assemblies.items[tag.assembly_order.start..][0..tag.assembly_order.len];
                for (payloads) |payload| {
                    const payload_value = tagAssemblyEvalForFinalizer(evals, payload).value;
                    const owner: repr.ConsumerUseOwner = .{ .tag_payload = .{
                        .parent = expr.value_info,
                        .tag = tag.tag,
                        .payload = payload.payload,
                    } };
                    const child_endpoint = try self.tagPayloadConsumerEndpoint(parent_endpoint, owner, tag.tag, payload.payload);
                    const use_id = try self.publishConsumerUseWithProvenance(payload_value, owner, child_endpoint, provenance);
                    self.setTagPayloadConsumerUse(expr.value_info, payload.payload, use_id);
                    if (self.consumerUsePushesEndpoint(use_id)) {
                        try self.finalizeExprConstructionUsesAtEndpointWithProvenance(payload_value, child_endpoint, provenance);
                    }
                }
            },
            .tuple => |items| {
                const elems = self.program.ast.expr_ids.items[items.start..][0..items.len];
                for (elems, 0..) |child, i| {
                    const owner: repr.ConsumerUseOwner = .{ .tuple_elem = .{
                        .parent = expr.value_info,
                        .index = @intCast(i),
                    } };
                    const child_endpoint = try self.tupleElemConsumerEndpoint(parent_endpoint, owner, @intCast(i));
                    const use_id = try self.publishConsumerUseWithProvenance(child, owner, child_endpoint, provenance);
                    self.setTupleElemConsumerUse(expr.value_info, @intCast(i), use_id);
                    if (self.consumerUsePushesEndpoint(use_id)) {
                        try self.finalizeExprConstructionUsesAtEndpointWithProvenance(child, child_endpoint, provenance);
                    }
                }
            },
            .list => |items| {
                const elems = self.program.ast.expr_ids.items[items.start..][0..items.len];
                for (elems, 0..) |child, i| {
                    const owner: repr.ConsumerUseOwner = .{ .list_elem = .{
                        .parent = expr.value_info,
                        .index = @intCast(i),
                    } };
                    const child_endpoint = try self.listElemConsumerEndpoint(parent_endpoint, owner);
                    const use_id = try self.publishConsumerUseWithProvenance(child, owner, child_endpoint, provenance);
                    self.setListElemConsumerUse(expr.value_info, @intCast(i), use_id);
                    if (self.consumerUsePushesEndpoint(use_id)) {
                        try self.finalizeExprConstructionUsesAtEndpointWithProvenance(child, child_endpoint, provenance);
                    }
                }
            },
            .nominal_reinterpret => |backing| {
                const payload = self.resolvedSessionPayload(parent_endpoint.exec_ty.ty);
                const owner: repr.ConsumerUseOwner = .{ .nominal_backing = .{
                    .parent = expr.value_info,
                    .nominal = self.nominalKeyForReinterpretExpr(expr, backing),
                } };
                const backing_endpoint = switch (payload) {
                    .nominal => try self.nominalBackingConsumerEndpoint(parent_endpoint, owner),
                    else => parent_endpoint,
                };
                const use_id = try self.publishConsumerUseWithProvenance(
                    backing,
                    owner,
                    backing_endpoint,
                    provenance,
                );
                self.valueStore().values.items[@intFromEnum(expr.value_info)]
                    .nominal_backing_consumer_use = use_id;
                if (self.consumerUsePushesEndpoint(use_id)) {
                    try self.finalizeExprConstructionUsesAtEndpointWithProvenance(backing, backing_endpoint, provenance);
                }
            },
            .let_ => |let_| {
                const bind_endpoint = try self.bindingEndpointOrNull(let_.bind.binding_info);
                try self.finalizeExprConstructionUsesAtEndpoint(let_.body, bind_endpoint);
                try self.finalizeExprConstructionUsesAtEndpointWithProvenance(let_.rest, expected, provenance);
            },
            .block => |block| {
                const stmts = self.program.ast.stmt_ids.items[block.stmts.start..][0..block.stmts.len];
                var final_expr_reachable = true;
                for (stmts) |stmt| {
                    try self.finalizeStmtConstructionUses(stmt);
                    if (final_expr_reachable and !self.stmtCanCompleteNormally(stmt)) {
                        final_expr_reachable = false;
                    }
                }
                if (final_expr_reachable) {
                    try self.finalizeExprConstructionUsesAtEndpointWithProvenance(block.final_expr, expected, provenance);
                } else {
                    try self.finalizeExprConstructionUsesAtEndpoint(block.final_expr, null);
                }
            },
            .if_ => |if_| {
                try self.finalizeExprConstructionUsesAtEndpoint(if_.cond, null);
                if (expected) |endpoint| {
                    try self.finalizeContextualIfBranchConsumerUses(expr.value_info, if_, endpoint, provenance);
                } else {
                    try self.finalizeExprConstructionUsesAtEndpoint(if_.then_body, null);
                    try self.finalizeExprConstructionUsesAtEndpoint(if_.else_body, null);
                }
            },
            .match_ => |match_| {
                try self.finalizeExprConstructionUsesAtEndpoint(match_.cond, null);
                const branches = self.program.ast.branch_ids.items[match_.branches.start..][0..match_.branches.len];
                for (branches) |branch_id| {
                    const branch = self.program.ast.branches.items[@intFromEnum(branch_id)];
                    if (branch.source_match_branch) |branch_ref| {
                        if (!self.valueStore().sourceMatchBranchReachable(branch_ref)) continue;
                    }
                    if (branch.guard) |guard| try self.finalizeExprConstructionUsesAtEndpoint(guard, null);
                }
                if (expected) |endpoint| {
                    try self.finalizeContextualMatchBranchConsumerUses(expr.value_info, match_, endpoint, provenance);
                } else {
                    for (branches) |branch_id| {
                        const branch = self.program.ast.branches.items[@intFromEnum(branch_id)];
                        if (branch.source_match_branch) |branch_ref| {
                            if (!self.valueStore().sourceMatchBranchReachable(branch_ref)) continue;
                        }
                        try self.finalizeExprConstructionUsesAtEndpoint(branch.body, null);
                    }
                }
            },
            .access => |access| try self.finalizeExprConstructionUsesAtEndpoint(access.record, null),
            .structural_eq => |eq| {
                try self.finalizeExprConstructionUsesAtEndpoint(eq.lhs, null);
                try self.finalizeExprConstructionUsesAtEndpoint(eq.rhs, null);
            },
            .bool_not => |child| try self.finalizeExprConstructionUsesAtEndpoint(child, null),
            .call_value => |call| {
                const call_site = self.valueStore().call_sites.items[@intFromEnum(call.call_site)];
                const dispatch = call_site.dispatch orelse {
                    lambdaInvariant("lambda-solved consumer-use finalization reached unresolved call_value dispatch");
                };
                switch (dispatch) {
                    .pending_comptime_dependency_call => {
                        try self.finalizeExprSpanConstructionUses(call.args);
                        return;
                    },
                    else => {},
                }
                try self.finalizeExprConstructionUsesAtEndpoint(call.func, null);
                switch (dispatch) {
                    .call_value_erased => try self.finalizeCallArgConsumerUses(call.call_site, call.args),
                    .call_value_finite,
                    .call_proc,
                    => try self.finalizeExprSpanConstructionUses(call.args),
                    .pending_comptime_dependency_call => unreachable,
                }
            },
            .call_proc => |call| try self.finalizeCallArgConsumerUses(call.call_site, call.args),
            .proc_value => |proc_value| {
                const captures = self.program.ast.capture_args.items[proc_value.captures.start..][0..proc_value.captures.len];
                for (captures) |capture| try self.finalizeExprConstructionUsesAtEndpoint(capture.expr, null);
            },
            .low_level => |low_level| try self.finalizeExprSpanConstructionUses(low_level.args),
            .tag_payload => |payload| try self.finalizeExprConstructionUsesAtEndpoint(payload.tag_union, null),
            .tuple_access => |access| try self.finalizeExprConstructionUsesAtEndpoint(access.tuple, null),
            .return_ => |ret| {
                try self.finalizeReturnConsumerUse(ret.return_info, ret.expr);
            },
            .for_ => |for_| {
                try self.finalizeExprConstructionUsesAtEndpoint(for_.iterable, null);
                try self.finalizeExprConstructionUsesAtEndpoint(for_.body, null);
            },
            .var_,
            .capture_ref,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .bool_lit,
            .unit,
            .const_instance,
            .const_ref,
            .pending_callable_instance,
            .pending_local_root,
            .crash,
            .runtime_error,
            => {},
        }
    }

    fn finalizeExprSpanConstructionUses(
        self: *ValueTransformFinalizer,
        span: Ast.Span(Ast.ExprId),
    ) Allocator.Error!void {
        const exprs = self.program.ast.expr_ids.items[span.start..][0..span.len];
        for (exprs) |expr| try self.finalizeExprConstructionUsesAtEndpoint(expr, null);
    }

    fn finalizeContextualIfBranchConsumerUses(
        self: *ValueTransformFinalizer,
        parent_value: repr.ValueInfoId,
        if_: anytype,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!void {
        const join_index = @intFromEnum(if_.join_info);
        if (join_index >= self.valueStore().joins.items.len) {
            lambdaInvariant("lambda-solved contextual if consumer-use referenced missing join");
        }
        if (self.valueStore().joins.items[join_index].kind != .if_expr) {
            lambdaInvariant("lambda-solved contextual if consumer-use referenced non-if join");
        }
        if (!self.valueStore().joins.items[join_index].contextual_consumer_uses.isEmpty()) return;

        const inputs = self.valueStore().sliceJoinInputSpan(self.valueStore().joins.items[join_index].inputs);
        const use_ids = try self.allocator.alloc(repr.ConsumerUsePlanId, inputs.len);
        defer self.allocator.free(use_ids);

        var saw_then = false;
        var saw_else = false;
        for (inputs, 0..) |input, i| {
            const source = switch (input.source) {
                .if_branch => |branch| branch,
                else => lambdaInvariant("lambda-solved contextual if consumer-use saw non-if join input"),
            };
            const branch_expr = switch (source.branch) {
                .then_ => if_.then_body,
                .else_ => if_.else_body,
            };
            if (!self.exprCanCompleteNormally(branch_expr)) {
                lambdaInvariant("lambda-solved contextual if consumer-use input referenced non-completing branch");
            }
            if (input.value != self.exprValue(branch_expr)) {
                lambdaInvariant("lambda-solved contextual if consumer-use branch value differs from join input");
            }
            const owner: repr.ConsumerUseOwner = .{ .if_branch_result = .{
                .parent = parent_value,
                .join = if_.join_info,
                .branch = source.branch,
            } };
            const use_id = try self.publishConsumerUseWithProvenance(branch_expr, owner, expected_endpoint, provenance);
            self.verifyPublishedConsumerUse(use_id, owner, self.exprValue(branch_expr), expected_endpoint);
            use_ids[i] = use_id;
            if (self.consumerUsePushesEndpoint(use_id)) {
                try self.finalizeExprConstructionUsesAtEndpointWithProvenance(branch_expr, expected_endpoint, provenance);
            }
            switch (source.branch) {
                .then_ => {
                    if (saw_then) lambdaInvariant("lambda-solved contextual if consumer-use saw duplicate then branch");
                    saw_then = true;
                },
                .else_ => {
                    if (saw_else) lambdaInvariant("lambda-solved contextual if consumer-use saw duplicate else branch");
                    saw_else = true;
                },
            }
        }

        try self.finalizeNonInputIfBranch(if_.then_body, saw_then);
        try self.finalizeNonInputIfBranch(if_.else_body, saw_else);

        self.valueStore().joins.items[join_index].contextual_consumer_uses =
            try self.valueStore().addConsumerUsePlanSpan(use_ids);
    }

    fn finalizeNonInputIfBranch(
        self: *ValueTransformFinalizer,
        branch_expr: Ast.ExprId,
        saw_input: bool,
    ) Allocator.Error!void {
        if (saw_input) return;
        if (self.exprCanCompleteNormally(branch_expr)) {
            lambdaInvariant("lambda-solved contextual if consumer-use missing normally-completing branch input");
        }
        try self.finalizeExprConstructionUsesAtEndpoint(branch_expr, null);
    }

    fn finalizeContextualMatchBranchConsumerUses(
        self: *ValueTransformFinalizer,
        parent_value: repr.ValueInfoId,
        match_: anytype,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!void {
        const join_index = @intFromEnum(match_.join_info);
        if (join_index >= self.valueStore().joins.items.len) {
            lambdaInvariant("lambda-solved contextual match consumer-use referenced missing join");
        }
        if (self.valueStore().joins.items[join_index].kind != .match_expr) {
            lambdaInvariant("lambda-solved contextual match consumer-use referenced non-match join");
        }
        if (!self.valueStore().joins.items[join_index].contextual_consumer_uses.isEmpty()) return;

        const branch_ids = self.program.ast.branch_ids.items[match_.branches.start..][0..match_.branches.len];
        const seen = try self.allocator.alloc(bool, branch_ids.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        const inputs = self.valueStore().sliceJoinInputSpan(self.valueStore().joins.items[join_index].inputs);
        const use_ids = try self.allocator.alloc(repr.ConsumerUsePlanId, inputs.len);
        defer self.allocator.free(use_ids);

        for (inputs, 0..) |input, i| {
            const source = switch (input.source) {
                .source_match_branch => |branch| branch,
                else => lambdaInvariant("lambda-solved contextual match consumer-use saw non-match join input"),
            };
            const branch_index: usize = @intCast(@intFromEnum(source.branch));
            if (branch_index >= branch_ids.len) {
                lambdaInvariant("lambda-solved contextual match consumer-use branch index is outside branch span");
            }
            if (seen[branch_index]) {
                lambdaInvariant("lambda-solved contextual match consumer-use saw duplicate branch input");
            }
            const branch = self.program.ast.branches.items[@intFromEnum(branch_ids[branch_index])];
            if (!self.exprCanCompleteNormally(branch.body)) {
                lambdaInvariant("lambda-solved contextual match consumer-use input referenced non-completing branch");
            }
            if (input.value != self.exprValue(branch.body)) {
                lambdaInvariant("lambda-solved contextual match consumer-use branch value differs from join input");
            }
            const owner: repr.ConsumerUseOwner = .{ .source_match_branch_result = .{
                .parent = parent_value,
                .join = match_.join_info,
                .branch_index = @intCast(branch_index),
            } };
            const use_id = try self.publishConsumerUseWithProvenance(branch.body, owner, expected_endpoint, provenance);
            self.verifyPublishedConsumerUse(use_id, owner, self.exprValue(branch.body), expected_endpoint);
            use_ids[i] = use_id;
            if (self.consumerUsePushesEndpoint(use_id)) {
                try self.finalizeExprConstructionUsesAtEndpointWithProvenance(branch.body, expected_endpoint, provenance);
            }
            seen[branch_index] = true;
        }

        for (branch_ids, 0..) |branch_id, branch_index| {
            if (seen[branch_index]) continue;
            const branch = self.program.ast.branches.items[@intFromEnum(branch_id)];
            if (branch.source_match_branch) |branch_ref| {
                if (!self.valueStore().sourceMatchBranchReachable(branch_ref)) continue;
            }
            if (self.exprCanCompleteNormally(branch.body)) {
                lambdaInvariant("lambda-solved contextual match consumer-use missing normally-completing branch input");
            }
            try self.finalizeExprConstructionUsesAtEndpoint(branch.body, null);
        }

        self.valueStore().joins.items[join_index].contextual_consumer_uses =
            try self.valueStore().addConsumerUsePlanSpan(use_ids);
    }

    fn finalizeStmtConstructionUses(
        self: *ValueTransformFinalizer,
        stmt_id: Ast.StmtId,
    ) Allocator.Error!void {
        const stmt = self.program.ast.stmts.items[@intFromEnum(stmt_id)];
        switch (stmt) {
            .decl => |decl| try self.finalizeExprConstructionUsesAtEndpoint(
                decl.body,
                try self.bindingEndpointOrNull(decl.bind.binding_info),
            ),
            .var_decl => |decl| try self.finalizeExprConstructionUsesAtEndpoint(
                decl.body,
                try self.bindingEndpointOrNull(decl.bind.binding_info),
            ),
            .reassign => |reassign| try self.finalizeExprConstructionUsesAtEndpoint(
                reassign.body,
                try self.bindingEndpointOrNull(reassign.version),
            ),
            .expr, .debug, .expect => |expr| try self.finalizeExprConstructionUsesAtEndpoint(expr, null),
            .return_ => |ret| {
                try self.finalizeReturnConsumerUse(ret.return_info, ret.expr);
            },
            .for_ => |for_| {
                try self.finalizeExprConstructionUsesAtEndpoint(for_.iterable, null);
                try self.finalizeExprConstructionUsesAtEndpoint(for_.body, null);
            },
            .while_ => |while_| {
                try self.finalizeExprConstructionUsesAtEndpoint(while_.cond, null);
                try self.finalizeExprConstructionUsesAtEndpoint(while_.body, null);
            },
            .crash,
            .break_,
            => {},
        }
    }

    fn bindingEndpointOrNull(
        self: *ValueTransformFinalizer,
        binding_info: repr.BindingInfoId,
    ) Allocator.Error!?repr.SessionExecutableValueEndpoint {
        const binding_index = @intFromEnum(binding_info);
        if (binding_index >= self.valueStore().bindings.items.len) {
            lambdaInvariant("lambda-solved consumer-use binding endpoint referenced missing binding");
        }
        const binding = self.valueStore().bindings.items[binding_index];
        const value_info = self.valueStore().values.items[@intFromEnum(binding.value)];
        if (value_info.pending_comptime_dependency_origin) return null;
        return try self.localEndpoint(binding.value);
    }

    fn publishConsumerUseWithProvenance(
        self: *ValueTransformFinalizer,
        child_expr: Ast.ExprId,
        owner: repr.ConsumerUseOwner,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.ConsumerUsePlanId {
        return try self.publishConsumerUseWithExistingBoundary(child_expr, owner, expected_endpoint, null, provenance);
    }

    fn publishConsumerUseWithOwnedExistingBoundary(
        self: *ValueTransformFinalizer,
        child_expr: Ast.ExprId,
        owner: repr.ConsumerUseOwner,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
        existing_boundary_kind: repr.ValueTransformBoundaryKind,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.ConsumerUsePlanId {
        const child = self.program.ast.exprs.items[@intFromEnum(child_expr)];
        const child_value = child.value_info;
        const initial_lowering: repr.ConsumerUseLowering = switch (child.data) {
            .record,
            .tag,
            .tuple,
            .list,
            .nominal_reinterpret,
            => .construct_directly,
            .let_,
            .block,
            .if_,
            .match_,
            => .lower_control_flow_contextually,
            else => .construct_directly,
        };
        const use_id = try self.representationStore().appendConsumerUsePlan(.{
            .owner = owner,
            .child_value = child_value,
            .expected_endpoint = expected_endpoint,
            .lowering = initial_lowering,
        });
        switch (child.data) {
            .record,
            .tag,
            .tuple,
            .list,
            .nominal_reinterpret,
            .let_,
            .block,
            .if_,
            .match_,
            => return use_id,
            else => {
                const from = try self.localEndpoint(child_value);
                const transform = try self.appendExistingValueTransformWithProvenance(existing_boundary_kind, from, expected_endpoint, provenance);
                const boundary = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = existing_boundary_kind,
                    .from_value = child_value,
                    .to_value = child_value,
                    .from_endpoint = from,
                    .to_endpoint = expected_endpoint,
                    .transform = transform,
                });
                self.representationStore().setConsumerUsePlanLowering(use_id, .{ .existing_value = boundary });
                try self.finalizeExprConstructionUsesAtEndpoint(child_expr, null);
                return use_id;
            },
        }
    }

    fn publishConsumerUseWithExistingBoundary(
        self: *ValueTransformFinalizer,
        child_expr: Ast.ExprId,
        owner: repr.ConsumerUseOwner,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
        existing_boundary: ?repr.ValueTransformBoundaryId,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.ConsumerUsePlanId {
        const child = self.program.ast.exprs.items[@intFromEnum(child_expr)];
        const child_value = child.value_info;
        const initial_lowering: repr.ConsumerUseLowering = switch (child.data) {
            .record,
            .tag,
            .tuple,
            .list,
            .nominal_reinterpret,
            => .construct_directly,
            .let_,
            .block,
            .if_,
            .match_,
            => .lower_control_flow_contextually,
            else => .construct_directly,
        };
        const use_id = try self.representationStore().appendConsumerUsePlan(.{
            .owner = owner,
            .child_value = child_value,
            .expected_endpoint = expected_endpoint,
            .lowering = initial_lowering,
        });
        switch (initial_lowering) {
            .construct_directly,
            .lower_control_flow_contextually,
            => {},
            .existing_value => unreachable,
        }
        switch (child.data) {
            .record,
            .tag,
            .tuple,
            .list,
            .nominal_reinterpret,
            .let_,
            .block,
            .if_,
            .match_,
            => {
                if (existing_boundary) |boundary_id| {
                    const boundary = self.representationStore().valueTransformBoundary(boundary_id);
                    if (boundary.from_value != child_value) {
                        lambdaInvariant("lambda-solved consumer-use existing call boundary source value differs from child");
                    }
                    if (!sessionExecutableValueEndpointEql(boundary.to_endpoint, expected_endpoint)) {
                        lambdaInvariant("lambda-solved consumer-use existing call boundary target endpoint differs from expected endpoint");
                    }
                }
                return use_id;
            },
            else => {
                const boundary = if (existing_boundary) |boundary_id| blk: {
                    const existing = self.representationStore().valueTransformBoundary(boundary_id);
                    if (existing.from_value != child_value) {
                        lambdaInvariant("lambda-solved consumer-use existing call boundary source value differs from child");
                    }
                    if (!sessionExecutableValueEndpointEql(existing.to_endpoint, expected_endpoint)) {
                        lambdaInvariant("lambda-solved consumer-use existing call boundary target endpoint differs from expected endpoint");
                    }
                    break :blk boundary_id;
                } else blk: {
                    const from = try self.localEndpoint(child_value);
                    const transform = try self.appendExistingValueTransformWithProvenance(.{ .consumer_use = use_id }, from, expected_endpoint, provenance);
                    break :blk try self.representationStore().appendValueTransformBoundary(.{
                        .kind = .{ .consumer_use = use_id },
                        .from_value = child_value,
                        .to_value = child_value,
                        .from_endpoint = from,
                        .to_endpoint = expected_endpoint,
                        .transform = transform,
                    });
                };
                self.representationStore().setConsumerUsePlanLowering(use_id, .{ .existing_value = boundary });
                try self.finalizeExprConstructionUsesAtEndpoint(child_expr, null);
                return use_id;
            },
        }
    }

    fn finalizeCallArgConsumerUses(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        args_span: Ast.Span(Ast.ExprId),
    ) Allocator.Error!void {
        const call_site_index = @intFromEnum(call_site_id);
        if (call_site_index >= self.valueStore().call_sites.items.len) {
            lambdaInvariant("lambda-solved call argument consumer-use referenced missing call site");
        }
        const arg_exprs = self.program.ast.expr_ids.items[args_span.start..][0..args_span.len];
        const call_site = self.valueStore().call_sites.items[call_site_index];
        if (!call_site.arg_consumer_uses.isEmpty()) return;
        const call_args = self.valueStore().sliceValueSpan(call_site.args);
        if (call_args.len != arg_exprs.len) {
            lambdaInvariant("lambda-solved call argument consumer-use count differs from call arity");
        }
        const arg_use_ids = try self.allocator.alloc(repr.ConsumerUsePlanId, arg_exprs.len);
        defer self.allocator.free(arg_use_ids);

        const dispatch = call_site.dispatch orelse {
            lambdaInvariant("lambda-solved call argument consumer-use reached unresolved call-site dispatch");
        };
        switch (dispatch) {
            .call_proc => |target_id| {
                const target_instance = self.procInstance(target_id);
                const target_params = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.params);
                if (call_args.len != target_params.len or call_args.len != target_instance.executable_specialization_key.exec_arg_tys.len) {
                    lambdaInvariant("lambda-solved call argument consumer-use saw target arity mismatch");
                }
                const provenance = self.procedureBoundaryProvenance(target_instance);
                for (arg_exprs, call_args, target_params, 0..) |arg_expr, arg_value, target_param, raw_i| {
                    if (self.exprValue(arg_expr) != arg_value) {
                        lambdaInvariant("lambda-solved call argument consumer-use expression value differs from call-site argument metadata");
                    }
                    const expected_endpoint = try self.targetParamEndpoint(target_id, target_instance, target_param, @intCast(raw_i));
                    const owner: repr.ConsumerUseOwner = .{ .call_arg = .{
                        .call = call_site_id,
                        .arg_index = @intCast(raw_i),
                    } };
                    const kind: repr.ValueTransformBoundaryKind = .{ .call_arg = .{
                        .call = call_site_id,
                        .arg_index = @intCast(raw_i),
                    } };
                    arg_use_ids[raw_i] = try self.publishConsumerUseWithOwnedExistingBoundary(
                        arg_expr,
                        owner,
                        expected_endpoint,
                        kind,
                        provenance,
                    );
                    const plan = self.representationStore().consumerUsePlan(arg_use_ids[raw_i]);
                    if (!sessionExecutableValueEndpointEql(plan.expected_endpoint, expected_endpoint)) {
                        lambdaInvariant("lambda-solved call argument consumer-use endpoint differs from target endpoint");
                    }
                    if (!consumerUseOwnerEql(plan.owner, owner)) {
                        lambdaInvariant("lambda-solved call argument consumer-use owner differs from call boundary");
                    }
                    if (self.consumerUsePushesEndpoint(arg_use_ids[raw_i])) {
                        try self.finalizeExprConstructionUsesAtEndpointWithProvenance(arg_expr, expected_endpoint, provenance);
                    }
                }
            },
            .call_value_erased => |sig_key| {
                const abi = self.representationStore().erased_fn_abis.abiFor(sig_key.abi) orelse {
                    lambdaInvariant("lambda-solved erased call argument consumer-use referenced an unpublished ABI");
                };
                if (call_args.len != abi.arg_exec_keys.len or call_args.len != abi.fixed_arity) {
                    lambdaInvariant("lambda-solved erased call argument consumer-use saw ABI arity mismatch");
                }
                for (arg_exprs, call_args, 0..) |arg_expr, arg_value, raw_i| {
                    if (self.exprValue(arg_expr) != arg_value) {
                        lambdaInvariant("lambda-solved erased call argument consumer-use expression value differs from call-site argument metadata");
                    }
                    const from = try self.localEndpoint(arg_value);
                    const expected_endpoint = self.rawArgEndpoint(call_site_id, @intCast(raw_i), from.logical_ty, abi.arg_exec_keys[raw_i]);
                    const owner: repr.ConsumerUseOwner = .{ .call_arg = .{
                        .call = call_site_id,
                        .arg_index = @intCast(raw_i),
                    } };
                    const kind: repr.ValueTransformBoundaryKind = .{ .call_arg = .{
                        .call = call_site_id,
                        .arg_index = @intCast(raw_i),
                    } };
                    arg_use_ids[raw_i] = try self.publishConsumerUseWithOwnedExistingBoundary(
                        arg_expr,
                        owner,
                        expected_endpoint,
                        kind,
                        &.{},
                    );
                    const plan = self.representationStore().consumerUsePlan(arg_use_ids[raw_i]);
                    if (!sessionExecutableValueEndpointEql(plan.expected_endpoint, expected_endpoint)) {
                        lambdaInvariant("lambda-solved erased call argument consumer-use endpoint differs from ABI endpoint");
                    }
                    if (!consumerUseOwnerEql(plan.owner, owner)) {
                        lambdaInvariant("lambda-solved erased call argument consumer-use owner differs from call boundary");
                    }
                    if (self.consumerUsePushesEndpoint(arg_use_ids[raw_i])) {
                        try self.finalizeExprConstructionUsesAtEndpointWithProvenance(arg_expr, expected_endpoint, &.{});
                    }
                }
            },
            .call_value_finite,
            .pending_comptime_dependency_call,
            => lambdaInvariant("lambda-solved call argument consumer-use reached call form without a single argument endpoint"),
        }
        self.valueStore().call_sites.items[call_site_index].arg_consumer_uses =
            try self.valueStore().addConsumerUsePlanSpan(arg_use_ids);
    }

    fn finalizeReturnConsumerUse(
        self: *ValueTransformFinalizer,
        return_info_id: repr.ReturnInfoId,
        expr_id: Ast.ExprId,
    ) Allocator.Error!void {
        const return_index = @intFromEnum(return_info_id);
        if (return_index >= self.valueStore().returns.items.len) {
            lambdaInvariant("lambda-solved return consumer-use referenced missing return info");
        }
        const expr = self.program.ast.exprs.items[@intFromEnum(expr_id)];
        if (!self.valueStore().valueSourceMatchBranchReachable(self.valueStore().values.items[@intFromEnum(expr.value_info)])) return;
        if (self.valueStore().returns.items[return_index].consumer_use != null) return;

        const expected_endpoint = try self.targetReturnEndpoint(self.instance_id, self.instance);
        const provenance = self.procedureBoundaryProvenance(self.instance);
        const use_id = try self.publishReturnConsumerUse(return_info_id, expr_id, expected_endpoint, provenance);
        self.valueStore().returns.items[return_index].consumer_use = use_id;

        const plan = self.representationStore().consumerUsePlan(use_id);
        switch (plan.lowering) {
            .construct_directly,
            .lower_control_flow_contextually,
            => try self.finalizeExprConstructionUsesAtEndpointWithProvenance(expr_id, expected_endpoint, provenance),
            .existing_value => |boundary| {
                self.valueStore().returns.items[return_index].transform = boundary;
                try self.finalizeExprConstructionUsesAtEndpoint(expr_id, null);
            },
        }
    }

    fn publishReturnConsumerUse(
        self: *ValueTransformFinalizer,
        return_info_id: repr.ReturnInfoId,
        child_expr: Ast.ExprId,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.ConsumerUsePlanId {
        const child = self.program.ast.exprs.items[@intFromEnum(child_expr)];
        const child_value = child.value_info;
        const return_info = self.valueStore().returns.items[@intFromEnum(return_info_id)];
        if (return_info.value != child_value) {
            lambdaInvariant("lambda-solved return consumer-use child value differs from return info");
        }

        const owner: repr.ConsumerUseOwner = .{ .return_value = return_info_id };
        const initial_lowering: repr.ConsumerUseLowering = switch (child.data) {
            .record,
            .tag,
            .tuple,
            .list,
            .nominal_reinterpret,
            => .construct_directly,
            .let_,
            .block,
            .if_,
            .match_,
            => .lower_control_flow_contextually,
            else => .construct_directly,
        };
        const use_id = try self.representationStore().appendConsumerUsePlan(.{
            .owner = owner,
            .child_value = child_value,
            .expected_endpoint = expected_endpoint,
            .lowering = initial_lowering,
        });
        switch (child.data) {
            .record,
            .tag,
            .tuple,
            .list,
            .nominal_reinterpret,
            .let_,
            .block,
            .if_,
            .match_,
            => return use_id,
            else => {
                const from = try self.localEndpoint(child_value);
                const kind: repr.ValueTransformBoundaryKind = .{ .return_value = return_info_id };
                const transform = try self.appendExistingValueTransformWithProvenance(kind, from, expected_endpoint, provenance);
                const boundary = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = kind,
                    .from_value = child_value,
                    .to_value = self.instance.public_roots.ret,
                    .from_endpoint = from,
                    .to_endpoint = expected_endpoint,
                    .transform = transform,
                });
                self.representationStore().setConsumerUsePlanLowering(use_id, .{ .existing_value = boundary });
                return use_id;
            },
        }
    }

    fn consumerUsePushesEndpoint(
        self: *ValueTransformFinalizer,
        use_id: repr.ConsumerUsePlanId,
    ) bool {
        return switch (self.representationStore().consumerUsePlan(use_id).lowering) {
            .construct_directly,
            .lower_control_flow_contextually,
            => true,
            .existing_value => false,
        };
    }

    fn verifyPublishedConsumerUse(
        self: *ValueTransformFinalizer,
        use_id: repr.ConsumerUsePlanId,
        owner: repr.ConsumerUseOwner,
        child_value: repr.ValueInfoId,
        expected_endpoint: repr.SessionExecutableValueEndpoint,
    ) void {
        const plan = self.representationStore().consumerUsePlan(use_id);
        if (!consumerUseOwnerEql(plan.owner, owner)) {
            lambdaInvariant("lambda-solved contextual consumer-use owner differs from published owner");
        }
        if (plan.child_value != child_value) {
            lambdaInvariant("lambda-solved contextual consumer-use child value differs from branch value");
        }
        if (!sessionExecutableValueEndpointEql(plan.expected_endpoint, expected_endpoint)) {
            lambdaInvariant("lambda-solved contextual consumer-use endpoint differs from expected endpoint");
        }
    }

    const ProjectionSlotEndpoint = struct {
        endpoint: repr.SessionExecutableValueEndpoint,
        kind: repr.ProjectionKind,
    };

    fn projectionSlotEndpoint(
        self: *ValueTransformFinalizer,
        projection_id: repr.ProjectionInfoId,
        projection: repr.ProjectionInfo,
    ) Allocator.Error!ProjectionSlotEndpoint {
        const parent = try self.localEndpoint(projection.source);
        var endpoint_kind: repr.ProjectionKind = projection.kind;
        const owner: repr.ConsumerUseOwner = switch (projection.kind) {
            .record_field => |field| .{ .record_field = .{
                .parent = projection.source,
                .field = field,
            } },
            .tuple_elem => |index| .{ .tuple_elem = .{
                .parent = projection.source,
                .index = index,
            } },
            .tag_payload => |payload| blk: {
                const tag = self.program.row_shapes.tagPayload(payload).tag;
                break :blk .{ .tag_payload = .{
                    .parent = projection.source,
                    .tag = tag,
                    .payload = payload,
                } };
            },
        };
        var endpoint = switch (projection.kind) {
            .record_field => |field| blk: {
                const resolved = try self.recordFieldProjectionEndpoint(parent, owner, field);
                endpoint_kind = .{ .record_field = resolved.field };
                break :blk resolved.endpoint;
            },
            .tuple_elem => |index| try self.tupleElemConsumerEndpoint(parent, owner, index),
            .tag_payload => |payload| blk: {
                const resolved = try self.tagPayloadProjectionEndpoint(parent, owner, self.program.row_shapes.tagPayload(payload).tag, payload);
                endpoint_kind = .{ .tag_payload = resolved.payload };
                break :blk resolved.endpoint;
            },
        };
        endpoint.owner = .{ .projection_slot = projection_id };
        return .{ .endpoint = endpoint, .kind = endpoint_kind };
    }

    fn recordFieldConsumerEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
        source_field: MonoRow.RecordFieldId,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        return (try self.recordFieldProjectionEndpoint(parent, owner, source_field)).endpoint;
    }

    const RecordFieldProjectionEndpoint = struct {
        endpoint: repr.SessionExecutableValueEndpoint,
        field: MonoRow.RecordFieldId,
    };

    fn recordFieldProjectionEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
        source_field: MonoRow.RecordFieldId,
    ) Allocator.Error!RecordFieldProjectionEndpoint {
        const payload = self.resolvedSessionPayload(parent.exec_ty.ty);
        var logical_record_ty = parent.logical_ty;
        const record = switch (payload) {
            .record => |record| record,
            .nominal => |nominal| blk: {
                logical_record_ty = try self.nominalBackingOrAlreadyBackingLogicalType(parent.logical_ty, nominal.nominal);
                break :blk switch (self.resolvedSessionPayload(nominal.backing)) {
                    .record => |record| record,
                    else => lambdaInvariant("lambda-solved consumer-use record field nominal endpoint had non-record backing"),
                };
            },
            else => lambdaInvariant("lambda-solved consumer-use record field expected record endpoint"),
        };
        const label = self.program.row_shapes.recordField(source_field).label;
        const target_field = self.recordFieldPayloadByLabel(record, label) orelse {
            lambdaInvariant("lambda-solved consumer-use record field missing from expected endpoint");
        };
        return .{ .endpoint = .{
            .owner = .{ .consumer_use = owner },
            .logical_ty = try self.recordFieldLogicalType(logical_record_ty, target_field.field),
            .exec_ty = .{
                .ty = target_field.ty,
                .key = target_field.key,
            },
        }, .field = target_field.field };
    }

    fn tupleElemConsumerEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
        index: u32,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const payload = self.resolvedSessionPayload(parent.exec_ty.ty);
        var logical_tuple_ty = parent.logical_ty;
        const elems = switch (payload) {
            .tuple => |elems| elems,
            .nominal => |nominal| blk: {
                logical_tuple_ty = try self.nominalBackingOrAlreadyBackingLogicalType(parent.logical_ty, nominal.nominal);
                break :blk switch (self.resolvedSessionPayload(nominal.backing)) {
                    .tuple => |elems| elems,
                    else => lambdaInvariant("lambda-solved consumer-use tuple element nominal endpoint had non-tuple backing"),
                };
            },
            else => lambdaInvariant("lambda-solved consumer-use tuple element expected tuple endpoint"),
        };
        const raw_index: usize = @intCast(index);
        if (raw_index >= elems.len or elems[raw_index].index != index) {
            lambdaInvariant("lambda-solved consumer-use tuple element index missing from expected endpoint");
        }
        const elem = elems[raw_index];
        return .{
            .owner = .{ .consumer_use = owner },
            .logical_ty = try self.tupleElemLogicalType(logical_tuple_ty, index),
            .exec_ty = .{
                .ty = elem.ty,
                .key = elem.key,
            },
        };
    }

    fn tagPayloadConsumerEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
        source_tag: MonoRow.TagId,
        source_payload: MonoRow.TagPayloadId,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        return (try self.tagPayloadProjectionEndpoint(parent, owner, source_tag, source_payload)).endpoint;
    }

    const TagPayloadProjectionEndpoint = struct {
        endpoint: repr.SessionExecutableValueEndpoint,
        payload: MonoRow.TagPayloadId,
    };

    fn tagPayloadProjectionEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
        source_tag: MonoRow.TagId,
        source_payload: MonoRow.TagPayloadId,
    ) Allocator.Error!TagPayloadProjectionEndpoint {
        const payload = self.resolvedSessionPayload(parent.exec_ty.ty);
        var logical_tag_union_ty = parent.logical_ty;
        const tag_union = switch (payload) {
            .tag_union => |tag_union| tag_union,
            .nominal => |nominal| blk: {
                logical_tag_union_ty = try self.nominalBackingOrAlreadyBackingLogicalType(parent.logical_ty, nominal.nominal);
                break :blk switch (self.resolvedSessionPayload(nominal.backing)) {
                    .tag_union => |tag_union| tag_union,
                    else => |backing_payload| lambdaInvariantFmt(
                        "lambda-solved consumer-use tag payload nominal endpoint had {s} backing",
                        .{@tagName(backing_payload)},
                    ),
                };
            },
            else => |actual_payload| lambdaInvariantFmt(
                "lambda-solved consumer-use tag payload expected tag-union endpoint for tag {s}, found {s}",
                .{ self.program.canonical_names.tagLabelText(self.program.row_shapes.tag(source_tag).label), @tagName(actual_payload) },
            ),
        };
        const tag_label = self.program.row_shapes.tag(source_tag).label;
        const target_tag = self.tagVariantPayloadByLabel(tag_union, tag_label) orelse {
            lambdaInvariant("lambda-solved consumer-use tag missing from expected endpoint");
        };
        const payload_index = self.program.row_shapes.tagPayload(source_payload).logical_index;
        const raw_payload_index: usize = @intCast(payload_index);
        if (raw_payload_index >= target_tag.payloads.len) {
            lambdaInvariant("lambda-solved consumer-use tag payload index missing from expected endpoint");
        }
        const target_payload = target_tag.payloads[raw_payload_index];
        if (self.program.row_shapes.tagPayload(target_payload.payload).logical_index != payload_index) {
            lambdaInvariant("lambda-solved consumer-use tag payload endpoint is not in logical order");
        }
        const child_logical_ty = try self.tagPayloadLogicalType(logical_tag_union_ty, target_tag.tag, payload_index);
        return .{ .endpoint = .{
            .owner = .{ .consumer_use = owner },
            .logical_ty = child_logical_ty,
            .exec_ty = .{
                .ty = target_payload.ty,
                .key = target_payload.key,
            },
        }, .payload = target_payload.payload };
    }

    fn listElemConsumerEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const payload = self.resolvedSessionPayload(parent.exec_ty.ty);
        var logical_list_ty = parent.logical_ty;
        const elem = switch (payload) {
            .list => |elem| elem,
            .nominal => |nominal| blk: {
                logical_list_ty = try self.nominalBackingOrAlreadyBackingLogicalType(parent.logical_ty, nominal.nominal);
                break :blk switch (self.resolvedSessionPayload(nominal.backing)) {
                    .list => |elem| elem,
                    else => lambdaInvariant("lambda-solved consumer-use list element nominal endpoint had non-list backing"),
                };
            },
            else => lambdaInvariant("lambda-solved consumer-use list element expected list endpoint"),
        };
        return .{
            .owner = .{ .consumer_use = owner },
            .logical_ty = try self.listElemLogicalType(logical_list_ty),
            .exec_ty = .{
                .ty = elem.ty,
                .key = elem.key,
            },
        };
    }

    fn nominalBackingConsumerEndpoint(
        self: *ValueTransformFinalizer,
        parent: repr.SessionExecutableValueEndpoint,
        owner: repr.ConsumerUseOwner,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const payload = self.resolvedSessionPayload(parent.exec_ty.ty);
        return switch (payload) {
            .nominal => |nominal| .{
                .owner = .{ .consumer_use = owner },
                .logical_ty = try self.nominalBackingOrAlreadyBackingLogicalType(parent.logical_ty, nominal.nominal),
                .exec_ty = .{
                    .ty = nominal.backing,
                    .key = nominal.backing_key,
                },
            },
            .tag_union,
            .record,
            .tuple,
            .list,
            .primitive,
            .box,
            .callable_set,
            .erased_fn,
            .vacant_callable_slot,
            .pending,
            => .{
                .owner = .{ .consumer_use = owner },
                .logical_ty = parent.logical_ty,
                .exec_ty = parent.exec_ty,
            },
            .recursive_ref => lambdaInvariant("lambda-solved nominal backing consumer endpoint saw unresolved recursive payload"),
        };
    }

    fn nominalKeyForReinterpretExpr(
        self: *ValueTransformFinalizer,
        expr: Ast.Expr,
        backing_expr_id: Ast.ExprId,
    ) canonical.NominalTypeKey {
        if (self.nominalKeyForLogicalType(expr.ty)) |nominal| return nominal;

        const value = self.valueStore().values.items[@intFromEnum(expr.value_info)];
        if (value.source_ty_payload) |source_payload| {
            if (self.nominalKeyForSourcePayload(source_payload)) |nominal| return nominal;
        }

        const backing_expr = self.program.ast.exprs.items[@intFromEnum(backing_expr_id)];
        if (self.nominalKeyForLogicalType(backing_expr.ty)) |nominal| return nominal;

        const backing_value = self.valueStore().values.items[@intFromEnum(backing_expr.value_info)];
        if (backing_value.source_ty_payload) |source_payload| {
            if (self.nominalKeyForSourcePayload(source_payload)) |nominal| return nominal;
        }

        lambdaInvariant("lambda-solved nominal reinterpret consumer-use had no explicit nominal identity");
    }

    fn nominalKeyForLogicalType(
        self: *ValueTransformFinalizer,
        ty: Type.TypeVarId,
    ) ?canonical.NominalTypeKey {
        const root = self.program.types.unlinkConst(ty);
        return switch (self.program.types.getNode(root)) {
            .nominal => |nominal| nominal.nominal,
            .content => null,
            else => lambdaInvariant("lambda-solved nominal reinterpret consumer-use had unresolved result type"),
        };
    }

    fn nominalKeyForSourcePayload(
        self: *ValueTransformFinalizer,
        source_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    ) ?canonical.NominalTypeKey {
        const source = concreteSourceTypeViewForRef(&self.program.concrete_source_types, self.artifact_views, &self.program.canonical_names, source_payload);
        var current = source.root;
        while (true) {
            switch (checkedTypePayload(source.view, current)) {
                .alias => |alias| current = alias.backing,
                .nominal => |nominal| return .{
                    .module_name = nominal.origin_module,
                    .type_name = nominal.name,
                },
                else => return null,
            }
        }
    }

    fn setRecordFieldConsumerUse(
        self: *ValueTransformFinalizer,
        parent: repr.ValueInfoId,
        field: MonoRow.RecordFieldId,
        use_id: repr.ConsumerUsePlanId,
    ) void {
        const value_info = &self.valueStore().values.items[@intFromEnum(parent)];
        const aggregate = value_info.aggregate orelse lambdaInvariant("lambda-solved consumer-use record parent had no aggregate metadata");
        switch (aggregate) {
            .record => |record| {
                for (record.fields) |*candidate| {
                    if (candidate.field == field) {
                        candidate.consumer_use = use_id;
                        return;
                    }
                }
            },
            else => lambdaInvariant("lambda-solved consumer-use record parent had non-record aggregate metadata"),
        }
        lambdaInvariant("lambda-solved consumer-use record field missing from aggregate metadata");
    }

    fn setTupleElemConsumerUse(
        self: *ValueTransformFinalizer,
        parent: repr.ValueInfoId,
        index: u32,
        use_id: repr.ConsumerUsePlanId,
    ) void {
        const value_info = &self.valueStore().values.items[@intFromEnum(parent)];
        const aggregate = value_info.aggregate orelse lambdaInvariant("lambda-solved consumer-use tuple parent had no aggregate metadata");
        switch (aggregate) {
            .tuple => |elems| {
                for (elems) |*candidate| {
                    if (candidate.index == index) {
                        candidate.consumer_use = use_id;
                        return;
                    }
                }
            },
            else => lambdaInvariant("lambda-solved consumer-use tuple parent had non-tuple aggregate metadata"),
        }
        lambdaInvariant("lambda-solved consumer-use tuple element missing from aggregate metadata");
    }

    fn setTagPayloadConsumerUse(
        self: *ValueTransformFinalizer,
        parent: repr.ValueInfoId,
        payload: MonoRow.TagPayloadId,
        use_id: repr.ConsumerUsePlanId,
    ) void {
        const value_info = &self.valueStore().values.items[@intFromEnum(parent)];
        const aggregate = value_info.aggregate orelse lambdaInvariant("lambda-solved consumer-use tag parent had no aggregate metadata");
        switch (aggregate) {
            .tag => |tag| {
                for (tag.payloads) |*candidate| {
                    if (candidate.payload == payload) {
                        candidate.consumer_use = use_id;
                        return;
                    }
                }
            },
            else => lambdaInvariant("lambda-solved consumer-use tag parent had non-tag aggregate metadata"),
        }
        lambdaInvariant("lambda-solved consumer-use tag payload missing from aggregate metadata");
    }

    fn setListElemConsumerUse(
        self: *ValueTransformFinalizer,
        parent: repr.ValueInfoId,
        index: u32,
        use_id: repr.ConsumerUsePlanId,
    ) void {
        const value_info = &self.valueStore().values.items[@intFromEnum(parent)];
        const aggregate = value_info.aggregate orelse lambdaInvariant("lambda-solved consumer-use list parent had no aggregate metadata");
        switch (aggregate) {
            .list => |list| {
                for (list.elems) |*candidate| {
                    if (candidate.index == index) {
                        candidate.consumer_use = use_id;
                        return;
                    }
                }
            },
            else => lambdaInvariant("lambda-solved consumer-use list parent had non-list aggregate metadata"),
        }
    }

    fn joinBoundaryKind(
        _: *ValueTransformFinalizer,
        _: repr.JoinInfoId,
        source: repr.JoinInputSource,
    ) repr.ValueTransformBoundaryKind {
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

    fn finalizePendingCallValue(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        callee: repr.ValueInfoId,
    ) Allocator.Error!void {
        const value_info = self.valueStore().values.items[@intFromEnum(callee)];
        const callable = value_info.callable orelse lambdaInvariant("lambda-solved call_value callee has no callable representation");
        switch (self.representationStore().callableEmissionPlan(callable.emission_plan)) {
            .pending_proc_value => lambdaInvariant("lambda-solved pending callable emission reached call_value finalization"),
            .finite => |key| {
                const plan = try self.finalizeCallValueFinite(call_site_id, call_site, key);
                call_site.dispatch = .{ .call_value_finite = plan };
            },
            .already_erased => |erased| {
                call_site.dispatch = .{ .call_value_erased = erased.sig_key };
                try self.finalizeCallValueErased(call_site_id, call_site, erased.sig_key);
            },
            .erase_finite_set => |erase| {
                call_site.dispatch = .{ .call_value_erased = erase.adapter.erased_fn_sig_key };
                try self.finalizeCallValueErased(call_site_id, call_site, erase.adapter.erased_fn_sig_key);
            },
            .erase_proc_value => |erase| {
                call_site.dispatch = .{ .call_value_erased = erase.erased_fn_sig_key };
                try self.finalizeCallValueErased(call_site_id, call_site, erase.erased_fn_sig_key);
            },
        }
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

        const provenance = self.procedureBoundaryProvenance(target_instance);

        const result_from = try self.targetReturnEndpoint(target_id, target_instance);
        const result_to = try self.localEndpoint(call_site.result);
        const result_kind: repr.ValueTransformBoundaryKind = .{ .call_result = call_site_id };
        const result_transform = try self.appendExistingValueTransformWithProvenance(result_kind, result_from, result_to, provenance);
        const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
            .kind = result_kind,
            .from_value = target_instance.public_roots.ret,
            .to_value = call_site.result,
            .from_endpoint = result_from,
            .to_endpoint = result_to,
            .transform = result_transform,
        });

        call_site.result_transform = result_boundary;
    }

    fn finalizeCallValueFinite(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        callable_set_key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!repr.CallValueFiniteDispatchPlanId {
        self.verifyCallSiteUnfinalized(call_site);
        const descriptor = self.representationStore().callableSetDescriptor(callable_set_key) orelse {
            lambdaInvariant("lambda-solved finite call boundary finalization referenced a missing callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite call boundary finalization saw empty callable-set descriptor");
        }

        const args = self.valueStore().sliceValueSpan(call_site.args);
        const branches = try self.allocator.alloc(repr.CallValueFiniteDispatchBranch, descriptor.members.len);
        defer self.allocator.free(branches);

        const result_to = try self.localEndpoint(call_site.result);
        for (descriptor.members, 0..) |member, i| {
            const target_id = member.target_instance;
            const target_instance = self.procInstance(target_id);
            const target_params = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.params);
            if (args.len != target_params.len or args.len != target_instance.executable_specialization_key.exec_arg_tys.len) {
                lambdaInvariant("lambda-solved finite call boundary finalization saw target arity mismatch");
            }
            const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, args.len);
            defer self.allocator.free(arg_boundaries);
            const member_ref: repr.CallableSetMemberRef = .{
                .callable_set_key = callable_set_key,
                .member_index = member.member,
            };
            for (args, target_params, 0..) |arg, target_param, arg_i| {
                const from = try self.localEndpoint(arg);
                const to = try self.targetParamEndpoint(target_id, target_instance, target_param, @intCast(arg_i));
                const kind: repr.ValueTransformBoundaryKind = .{ .callable_match_branch_arg = .{
                    .call = call_site_id,
                    .member = member_ref,
                    .arg_index = @intCast(arg_i),
                } };
                const transform = try self.appendExistingValueTransform(kind, from, to);
                arg_boundaries[arg_i] = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = kind,
                    .from_value = arg,
                    .to_value = target_param,
                    .from_endpoint = from,
                    .to_endpoint = to,
                    .transform = transform,
                });
            }

            const result_from = try self.targetReturnEndpoint(target_id, target_instance);
            const kind: repr.ValueTransformBoundaryKind = .{ .callable_match_branch_result = .{
                .call = call_site_id,
                .member = member_ref,
            } };
            const result_transform = try self.appendExistingValueTransform(kind, result_from, result_to);
            const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = target_instance.public_roots.ret,
                .to_value = call_site.result,
                .from_endpoint = result_from,
                .to_endpoint = result_to,
                .transform = result_transform,
            });
            branches[i] = .{
                .member = member_ref,
                .target_instance = target_id,
                .arg_transforms = try self.valueStore().addValueTransformBoundarySpan(arg_boundaries),
                .result_transform = result_boundary,
            };
        }

        return try self.valueStore().addCallValueFiniteDispatchPlan(.{
            .callable_set_key = callable_set_key,
            .branches = try self.valueStore().addCallValueFiniteDispatchBranchSpan(branches),
        });
    }

    fn verifyFinalizedCallValueFinite(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *const repr.CallSiteInfo,
        plan_id: repr.CallValueFiniteDispatchPlanId,
    ) Allocator.Error!void {
        const plan = self.valueStore().callValueFiniteDispatchPlan(plan_id);
        const branches = self.valueStore().sliceCallValueFiniteDispatchBranches(plan.branches);
        if (branches.len == 0) {
            lambdaInvariant("lambda-solved finalized finite call dispatch plan has no branches");
        }
        const args = self.valueStore().sliceValueSpan(call_site.args);
        for (branches) |branch| {
            const arg_transforms = self.valueStore().sliceValueTransformBoundarySpan(branch.arg_transforms);
            if (arg_transforms.len != args.len) {
                lambdaInvariant("lambda-solved finalized finite call dispatch branch argument transform count differs from call arity");
            }
            for (arg_transforms, 0..) |boundary_id, arg_i| {
                const boundary = self.representationStore().valueTransformBoundary(boundary_id);
                const kind = switch (boundary.kind) {
                    .callable_match_branch_arg => |kind| kind,
                    else => lambdaInvariant("lambda-solved finalized finite call dispatch argument boundary has wrong kind"),
                };
                if (kind.call != call_site_id or
                    kind.arg_index != @as(u32, @intCast(arg_i)) or
                    !repr.callableSetKeyEql(kind.member.callable_set_key, branch.member.callable_set_key) or
                    kind.member.member_index != branch.member.member_index)
                {
                    lambdaInvariant("lambda-solved finalized finite call dispatch argument boundary points at a different branch");
                }
            }
            const result_boundary = self.representationStore().valueTransformBoundary(branch.result_transform);
            const kind = switch (result_boundary.kind) {
                .callable_match_branch_result => |kind| kind,
                else => lambdaInvariant("lambda-solved finalized finite call dispatch result boundary has wrong kind"),
            };
            if (kind.call != call_site_id or
                !repr.callableSetKeyEql(kind.member.callable_set_key, branch.member.callable_set_key) or
                kind.member.member_index != branch.member.member_index)
            {
                lambdaInvariant("lambda-solved finalized finite call dispatch result boundary points at a different branch");
            }
        }
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

        const result_to = try self.localEndpoint(call_site.result);
        const result_from = self.rawResultEndpoint(call_site_id, result_to.logical_ty, abi.ret_exec_key);
        const result_kind: repr.ValueTransformBoundaryKind = .{ .call_result = call_site_id };
        const result_transform = try self.appendExistingValueTransform(result_kind, result_from, result_to);
        const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
            .kind = result_kind,
            .from_value = call_site.result,
            .to_value = call_site.result,
            .from_endpoint = result_from,
            .to_endpoint = result_to,
            .transform = result_transform,
        });

        call_site.result_transform = result_boundary;
    }

    fn finalizeCallableConstructions(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.values.items, 0..) |value_info, raw_value| {
            if (!value_store.valueSourceMatchBranchReachable(value_info)) continue;
            const callable = value_info.callable orelse continue;
            const construction_id = callable.construction_plan orelse continue;
            const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
            try self.finalizeCallableConstruction(value_id, construction_id);
        }
    }

    fn finalizeCallableConstruction(
        self: *ValueTransformFinalizer,
        value_id: repr.ValueInfoId,
        construction_id: repr.CallableSetConstructionPlanId,
    ) Allocator.Error!void {
        const construction_snapshot = self.representationStore().callableConstructionPlan(construction_id);
        if (construction_snapshot.result != value_id) {
            lambdaInvariant("lambda-solved callable construction finalization reached a construction attached to a different value");
        }
        if (construction_snapshot.capture_transforms.len != 0) {
            lambdaInvariant("lambda-solved callable construction finalization reached already-finalized capture transforms");
        }

        const member = self.representationStore().callableSetMember(construction_snapshot.callable_set_key, construction_snapshot.selected_member) orelse {
            lambdaInvariant("lambda-solved callable construction finalization selected a missing callable-set member");
        };
        const target_id = construction_snapshot.target_instance;
        const target_instance = self.procInstance(target_id);
        if (target_id != member.target_instance) {
            const member_instance = self.procInstance(member.target_instance);
            lambdaInvariantFmt(
                "lambda-solved callable construction target instance differs from selected member instance: construction={d} value={d} target={d} target_materialized={} member_target={d} member_materialized={} selected_member={d} same_proc={} same_callable={} same_exec_key={}",
                .{
                    @intFromEnum(construction_id),
                    @intFromEnum(value_id),
                    @intFromEnum(target_id),
                    target_instance.materialized,
                    @intFromEnum(member.target_instance),
                    member_instance.materialized,
                    @intFromEnum(construction_snapshot.selected_member),
                    canonical.mirProcedureRefEql(target_instance.proc, member_instance.proc),
                    canonical.procedureCallableRefEql(target_instance.proc.callable, member_instance.proc.callable),
                    repr.executableSpecializationKeyEql(target_instance.executable_specialization_key, member_instance.executable_specialization_key),
                },
            );
        }
        if (!canonical.mirProcedureRefEql(target_instance.proc, member.source_proc)) {
            lambdaInvariant("lambda-solved callable construction target instance differs from selected member source");
        }
        const target_captures = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.captures);
        const source_captures = construction_snapshot.capture_values;
        if (source_captures.len != target_captures.len or source_captures.len != member.capture_slots.len) {
            lambdaInvariant("lambda-solved callable construction finalization saw capture arity mismatch");
        }

        const boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, source_captures.len);
        defer self.allocator.free(boundaries);

        for (source_captures, target_captures, 0..) |source_capture, target_capture, i| {
            const slot = member.capture_slots[i];
            if (slot.slot != @as(u32, @intCast(i))) {
                lambdaInvariant("lambda-solved callable construction finalization saw non-canonical capture slot");
            }
            const from = try self.localEndpoint(source_capture);
            const to = try self.targetCaptureEndpoint(target_id, target_instance, target_capture, slot.slot, slot.exec_value_ty);
            const capture_boundary = try self.representationStore().reserveCaptureBoundary(.{
                .owner = .{ .callable_set_construction = .{
                    .construction = construction_id,
                    .selected_member = .{
                        .callable_set_key = construction_snapshot.callable_set_key,
                        .member_index = construction_snapshot.selected_member,
                    },
                } },
                .target_instance = target_id,
                .slot = slot.slot,
                .source_capture_value = source_capture,
                .target_capture_value = target_capture,
                .boundary = @enumFromInt(std.math.maxInt(u32)),
            });
            const kind: repr.ValueTransformBoundaryKind = .{ .capture_value = capture_boundary };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            const boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = source_capture,
                .to_value = target_capture,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
            self.representationStore().fillCaptureBoundary(capture_boundary, boundary);
            boundaries[i] = boundary;
        }

        try self.representationStore().setCallableConstructionCaptureTransforms(construction_id, boundaries);
    }

    fn finalizeProcValueErasePlans(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.values.items, 0..) |value_info, raw_value| {
            if (!value_store.valueSourceMatchBranchReachable(value_info)) continue;
            const callable = value_info.callable orelse continue;
            const emission = self.representationStore().callableEmissionPlan(callable.emission_plan);
            const erase = switch (emission) {
                .erase_proc_value => |erase| erase,
                .pending_proc_value => lambdaInvariant("lambda-solved pending callable emission reached proc-value erase finalization"),
                else => continue,
            };
            const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
            try self.finalizeProcValueErasePlan(value_id, callable, callable.emission_plan, erase);
        }
    }

    fn finalizeProcValueErasePlan(
        self: *ValueTransformFinalizer,
        value_id: repr.ValueInfoId,
        callable: repr.CallableValueInfo,
        emission_plan_id: repr.CallableValueEmissionPlanId,
        erase: repr.ProcValueErasePlan,
    ) Allocator.Error!void {
        if (erase.source_value != value_id) {
            lambdaInvariant("lambda-solved proc-value erase finalization reached a plan attached to a different value");
        }
        if (erase.adapter_arg_transforms.len != 0) {
            lambdaInvariant("lambda-solved proc-value erase finalization reached already-finalized adapter arg transforms");
        }
        if (erase.capture_transforms.len != 0) {
            lambdaInvariant("lambda-solved proc-value erase finalization reached already-finalized capture transforms");
        }

        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => lambdaInvariant("lambda-solved proc-value erase finalization reached a non-proc callable source"),
        };
        if (!canonical.procedureCallableRefEql(source.proc.callable, erase.proc_value)) {
            lambdaInvariant("lambda-solved proc-value erase finalization source procedure differs from erase plan");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, erase.proc_value.source_fn_ty)) {
            lambdaInvariant("lambda-solved proc-value erase finalization source function type differs from erase plan");
        }

        const target_instance = self.procInstance(erase.target_instance);
        const abi = self.representationStore().erased_fn_abis.abiFor(erase.erased_fn_sig_key.abi) orelse {
            lambdaInvariant("lambda-solved proc-value erase finalization referenced an unpublished ABI");
        };
        const target_params = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.params);
        if (target_params.len != abi.arg_exec_keys.len or
            target_params.len != abi.fixed_arity or
            target_params.len != target_instance.executable_specialization_key.exec_arg_tys.len)
        {
            lambdaInvariant("lambda-solved proc-value erase finalization saw adapter arg arity mismatch");
        }

        const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, target_params.len);
        defer self.allocator.free(arg_boundaries);

        for (target_params, abi.arg_exec_keys, 0..) |target_param, raw_arg_key, arg_i| {
            const from = self.rawErasedProcValueAdapterArgEndpoint(
                emission_plan_id,
                value_id,
                erase,
                target_instance,
                target_param,
                @intCast(arg_i),
                raw_arg_key,
            );
            const to = try self.targetParamEndpoint(erase.target_instance, target_instance, target_param, @intCast(arg_i));
            const kind: repr.ValueTransformBoundaryKind = .{ .erased_proc_value_adapter_arg = .{
                .emission_plan = emission_plan_id,
                .source_value = value_id,
                .proc_value = erase.proc_value,
                .erased_fn_sig_key = erase.erased_fn_sig_key,
                .index = @intCast(arg_i),
            } };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            arg_boundaries[arg_i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = target_param,
                .to_value = target_param,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }
        try self.representationStore().setProcValueEraseAdapterArgTransforms(emission_plan_id, arg_boundaries);

        const target_captures = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.captures);
        const source_captures = source.captures;
        if (source_captures.len != target_captures.len or source_captures.len != erase.capture_slots.len) {
            lambdaInvariant("lambda-solved proc-value erase finalization saw capture arity mismatch");
        }

        const boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, source_captures.len);
        defer self.allocator.free(boundaries);

        for (source_captures, target_captures, 0..) |source_capture, target_capture, i| {
            const slot = erase.capture_slots[i];
            if (slot.slot != @as(u32, @intCast(i))) {
                lambdaInvariant("lambda-solved proc-value erase finalization saw non-canonical capture slot");
            }
            const from = try self.localEndpoint(source_capture);
            const to = try self.targetCaptureEndpoint(erase.target_instance, target_instance, target_capture, slot.slot, slot.exec_value_ty);
            const capture_boundary = try self.representationStore().reserveCaptureBoundary(.{
                .owner = .{ .proc_value_erase = .{
                    .emission_plan = emission_plan_id,
                    .source_value = value_id,
                    .proc_value = erase.proc_value,
                    .erased_fn_sig_key = erase.erased_fn_sig_key,
                } },
                .target_instance = erase.target_instance,
                .slot = slot.slot,
                .source_capture_value = source_capture,
                .target_capture_value = target_capture,
                .boundary = @enumFromInt(std.math.maxInt(u32)),
            });
            const kind: repr.ValueTransformBoundaryKind = .{ .capture_value = capture_boundary };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            const boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = source_capture,
                .to_value = target_capture,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
            self.representationStore().fillCaptureBoundary(capture_boundary, boundary);
            boundaries[i] = boundary;
        }

        try self.representationStore().setProcValueEraseCaptureTransforms(emission_plan_id, boundaries);
    }

    fn finalizeFiniteSetErasePlans(self: *ValueTransformFinalizer) Allocator.Error!void {
        for (self.representationStore().callable_emission_plans, 0..) |emission, raw_emission| {
            const erase = switch (emission) {
                .erase_finite_set => |erase| erase,
                .pending_proc_value => lambdaInvariant("lambda-solved pending callable emission reached finite-set erase finalization"),
                else => continue,
            };
            if (erase.branches.len != 0) continue;

            const branches = try self.buildFiniteSetEraseAdapterBranches(erase.adapter, erase.member_targets, erase.provenance);
            defer deinitLocalFiniteSetEraseBranches(self.allocator, branches);
            const emission_plan: repr.CallableValueEmissionPlanId = @enumFromInt(@as(u32, @intCast(raw_emission)));
            try self.representationStore().setFiniteSetEraseAdapterBranches(emission_plan, branches);
        }
    }

    fn buildFiniteSetEraseAdapterBranches(
        self: *ValueTransformFinalizer,
        adapter: repr.ErasedAdapterKey,
        member_targets: []const repr.ExecutableSpecializationKey,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error![]const repr.FiniteSetEraseAdapterBranchPlan {
        const descriptor = self.representationStore().callableSetDescriptor(adapter.callable_set_key) orelse {
            lambdaInvariant("lambda-solved finite-set erased adapter branch finalization referenced missing callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite-set erased adapter branch finalization reached empty descriptor");
        }
        if (descriptor.members.len != member_targets.len) {
            lambdaInvariant("lambda-solved finite-set erased adapter branch target count differs from descriptor");
        }
        const abi = self.representationStore().erased_fn_abis.abiFor(adapter.erased_fn_sig_key.abi) orelse {
            lambdaInvariant("lambda-solved finite-set erased adapter branch finalization referenced an unpublished ABI");
        };

        const branches = try self.allocator.alloc(repr.FiniteSetEraseAdapterBranchPlan, descriptor.members.len);
        @memset(branches, .{
            .member = .{
                .callable_set_key = adapter.callable_set_key,
                .member_index = undefined,
            },
            .target_instance = undefined,
            .arg_transforms = &.{},
            .capture_transforms = &.{},
            .result_transform = null,
        });
        errdefer deinitLocalFiniteSetEraseBranches(self.allocator, branches);

        for (descriptor.members, member_targets, 0..) |member, target_key, raw_member| {
            validatePersistedFiniteAdapterMemberTarget(member, target_key);
            const target_id = self.procInstanceForExecutableSpecializationKey(target_key) orelse {
                lambdaInvariant("lambda-solved finite-set erased adapter branch target instance was not materialized");
            };
            const target_instance = self.procInstance(target_id);
            if (!canonical.mirProcedureRefEql(target_instance.proc, member.source_proc)) {
                lambdaInvariant("lambda-solved finite-set erased adapter branch target instance differs from descriptor source procedure");
            }
            const target_params = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.params);
            if (target_params.len != abi.arg_exec_keys.len or
                target_params.len != abi.fixed_arity or
                target_params.len != target_instance.executable_specialization_key.exec_arg_tys.len)
            {
                lambdaInvariant("lambda-solved finite-set erased adapter branch arity differs from erased ABI or target specialization");
            }

            const member_ref: repr.CallableSetMemberRef = .{
                .callable_set_key = adapter.callable_set_key,
                .member_index = member.member,
            };
            const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, target_params.len);
            errdefer if (arg_boundaries.len > 0) self.allocator.free(arg_boundaries);
            for (target_params, abi.arg_exec_keys, 0..) |target_param, raw_key, arg_i| {
                const from = self.rawErasedFiniteAdapterArgEndpoint(
                    adapter,
                    member_ref,
                    target_instance,
                    target_param,
                    @intCast(arg_i),
                    raw_key,
                );
                const to = try self.targetParamEndpoint(target_id, target_instance, target_param, @intCast(arg_i));
                const kind: repr.ValueTransformBoundaryKind = .{ .erased_finite_adapter_arg = .{
                    .adapter = adapter,
                    .member = member_ref,
                    .index = @intCast(arg_i),
                } };
                const transform = try self.appendExistingValueTransformWithProvenance(kind, from, to, provenance);
                arg_boundaries[arg_i] = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = kind,
                    .from_value = target_param,
                    .to_value = target_param,
                    .from_endpoint = from,
                    .to_endpoint = to,
                    .transform = transform,
                });
            }

            const target_captures = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.captures);
            if (target_captures.len != member.capture_slots.len) {
                lambdaInvariant("lambda-solved finite-set erased adapter branch capture arity differs from target specialization");
            }
            const capture_boundaries: []repr.ValueTransformBoundaryId = if (member.capture_slots.len == 0)
                &.{}
            else
                try self.allocator.alloc(repr.ValueTransformBoundaryId, member.capture_slots.len);
            errdefer if (capture_boundaries.len > 0) self.allocator.free(capture_boundaries);
            for (member.capture_slots, target_captures, 0..) |slot, target_capture, slot_i| {
                if (slot.slot != @as(u32, @intCast(slot_i))) {
                    lambdaInvariant("lambda-solved finite-set erased adapter branch capture slots are not canonical");
                }
                const from = self.rawErasedFiniteAdapterCaptureEndpoint(
                    adapter,
                    member_ref,
                    target_instance,
                    target_capture,
                    slot.slot,
                    slot.exec_value_ty,
                );
                const to = try self.targetCaptureEndpointFromActualValue(target_id, target_instance, target_capture, slot.slot);
                const kind: repr.ValueTransformBoundaryKind = .{ .erased_finite_adapter_capture = .{
                    .adapter = adapter,
                    .member = member_ref,
                    .slot = slot.slot,
                } };
                const transform = try self.appendExistingValueTransformWithProvenance(kind, from, to, provenance);
                capture_boundaries[slot_i] = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = kind,
                    .from_value = target_capture,
                    .to_value = target_capture,
                    .from_endpoint = from,
                    .to_endpoint = to,
                    .transform = transform,
                });
            }

            const result_from = try self.targetReturnEndpoint(target_id, target_instance);
            const result_to = self.rawErasedFiniteAdapterResultEndpoint(
                adapter,
                member_ref,
                target_instance,
                abi.ret_exec_key,
            );
            const result_kind: repr.ValueTransformBoundaryKind = .{ .erased_finite_adapter_result = .{
                .adapter = adapter,
                .member = member_ref,
            } };
            const result_transform = try self.appendExistingValueTransform(result_kind, result_from, result_to);
            const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = result_kind,
                .from_value = target_instance.public_roots.ret,
                .to_value = target_instance.public_roots.ret,
                .from_endpoint = result_from,
                .to_endpoint = result_to,
                .transform = result_transform,
            });

            branches[raw_member] = .{
                .member = member_ref,
                .target_instance = target_id,
                .arg_transforms = arg_boundaries,
                .capture_transforms = capture_boundaries,
                .result_transform = result_boundary,
            };
        }

        return branches;
    }

    fn procInstanceForExecutableSpecializationKey(
        self: *const ValueTransformFinalizer,
        key: repr.ExecutableSpecializationKey,
    ) ?repr.ProcRepresentationInstanceId {
        for (self.program.proc_instances.items, 0..) |instance, raw| {
            if (!instance.materialized) continue;
            if (repr.executableSpecializationKeyEql(instance.executable_specialization_key, key)) {
                return @enumFromInt(@as(u32, @intCast(raw)));
            }
        }
        return null;
    }

    fn verifyCallSiteUnfinalized(
        _: *ValueTransformFinalizer,
        call_site: *const repr.CallSiteInfo,
    ) void {
        if (!call_site.arg_transforms.isEmpty() or
            call_site.result_transform != null)
        {
            lambdaInvariant("lambda-solved value transform finalization reached an already-finalized call site");
        }
    }

    fn appendExistingValueTransform(
        self: *ValueTransformFinalizer,
        kind: repr.ValueTransformBoundaryKind,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        return try self.appendExistingValueTransformWithProvenance(kind, from, to, &.{});
    }

    fn appendExistingValueTransformWithProvenance(
        self: *ValueTransformFinalizer,
        kind: repr.ValueTransformBoundaryKind,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const scope = try self.representationStore().appendTransformEndpointScope(.{
            .root_kind = kind,
            .root_from = from,
            .root_to = to,
        });
        return try self.planValueTransform(scope, from, to, provenance);
    }

    fn planValueTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (!repr.canonicalExecValueTypeKeyEql(from.exec_ty.key, to.exec_ty.key)) {
            return try self.planNonIdentityValueTransform(scope, from, to, provenance);
        }
        return try self.appendSessionValueTransform(scope, from, to, .none, .identity);
    }

    fn planNonIdentityValueTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const from_payload = self.resolvedSessionPayload(from.exec_ty.ty);
        const to_payload = self.resolvedSessionPayload(to.exec_ty.ty);
        return switch (from_payload) {
            .record => |source| switch (to_payload) {
                .record => |target| try self.planRecordTransform(scope, from, to, source, target, provenance),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .tuple => |source| switch (to_payload) {
                .tuple => |target| try self.planTupleTransform(scope, from, to, source, target, provenance),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .tag_union => |source| switch (to_payload) {
                .tag_union => |target| try self.planTagUnionTransform(scope, from, to, source, target, provenance),
                .primitive => self.transformPayloadInvariant(from, to, from_payload, to_payload),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .nominal => |source| switch (to_payload) {
                .nominal => |target| try self.planNominalTransform(scope, from, to, source, target, provenance),
                .pending,
                .recursive_ref,
                => self.transformPayloadInvariant(from, to, from_payload, to_payload),
                else => try self.planNominalToBackingTransform(scope, from, to, source, provenance),
            },
            .list => |source| switch (to_payload) {
                .list => |target| try self.planListTransform(scope, from, to, source, target, provenance),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .box => |source| switch (to_payload) {
                .box => |target| try self.planBoxTransform(scope, from, to, source, target, .box_to_box, provenance),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .callable_set => |source| switch (to_payload) {
                .erased_fn => |target| try self.planFiniteCallableToErasedTransform(scope, from, to, source, target, provenance),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .erased_fn => |source| switch (to_payload) {
                .erased_fn => |target| try self.planAlreadyErasedCallableTransform(scope, from, to, source, target),
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .primitive => switch (to_payload) {
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .vacant_callable_slot => switch (to_payload) {
                .nominal => |target| try self.planBackingToNominalTransform(scope, from, to, target, provenance),
                else => self.transformPayloadInvariant(from, to, from_payload, to_payload),
            },
            .pending,
            .recursive_ref,
            => self.transformPayloadInvariant(from, to, from_payload, to_payload),
        };
    }

    fn appendSessionValueTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: checked_artifact.ValueTransformProvenance,
        op: repr.SessionExecutableValueTransformOp,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const id = try self.representationStore().appendSessionExecutableValueTransform(.{
            .scope = scope,
            .from = from,
            .to = to,
            .provenance = provenance,
            .op = op,
        });
        return .{ .session = id };
    }

    fn sessionPayload(
        self: *ValueTransformFinalizer,
        ref: repr.SessionExecutableTypePayloadRef,
    ) repr.SessionExecutableTypePayload {
        return self.representationStore().session_executable_type_payloads.get(ref.payload);
    }

    fn resolvedSessionPayload(
        self: *ValueTransformFinalizer,
        ref: repr.SessionExecutableTypePayloadRef,
    ) repr.SessionExecutableTypePayload {
        const payloads = &self.representationStore().session_executable_type_payloads;
        var current = ref.payload;
        var remaining = payloads.entries.len;
        while (remaining != 0) : (remaining -= 1) {
            switch (payloads.get(current)) {
                .recursive_ref => |next| current = next,
                else => |payload| return payload,
            }
        }
        lambdaInvariant("lambda-solved recursive executable payload reference did not reach a concrete payload");
    }

    fn transformPayloadInvariant(
        _: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        from_payload: repr.SessionExecutableTypePayload,
        to_payload: repr.SessionExecutableTypePayload,
    ) noreturn {
        const from_key_hex = std.fmt.bytesToHex(from.exec_ty.key.bytes, .lower);
        const to_key_hex = std.fmt.bytesToHex(to.exec_ty.key.bytes, .lower);
        lambdaInvariantFmt(
            "lambda-solved value transform has incompatible executable payloads: {s}({s}, key={s}) -> {s}({s}, key={s})",
            .{
                @tagName(from_payload),
                @tagName(std.meta.activeTag(from.owner)),
                &from_key_hex,
                @tagName(to_payload),
                @tagName(std.meta.activeTag(to.owner)),
                &to_key_hex,
            },
        );
    }

    fn planRecordTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableRecordPayload,
        target: repr.SessionExecutableRecordPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const fields = try self.allocator.alloc(repr.SessionValueTransformRecordField, target.fields.len);
        defer self.allocator.free(fields);

        for (target.fields, 0..) |target_field, i| {
            const label = self.program.row_shapes.recordField(target_field.field).label;
            const source_field = self.recordFieldPayloadByLabel(source, label) orelse {
                lambdaInvariant("lambda-solved record transform target field has no source field");
            };
            if (source_field.field != target_field.field) {
                lambdaInvariant("lambda-solved record transform reached distinct source/target field ids");
            }

            const from_child = try self.transformChildEndpoint(
                scope,
                from,
                .from,
                .{ .record_field = source_field.field },
                try self.recordFieldLogicalType(from.logical_ty, source_field.field),
                source_field.ty,
                source_field.key,
            );
            const to_child = try self.transformChildEndpoint(
                scope,
                to,
                .to,
                .{ .record_field = target_field.field },
                try self.recordFieldLogicalType(to.logical_ty, target_field.field),
                target_field.ty,
                target_field.key,
            );
            fields[i] = .{
                .field = target_field.field,
                .transform = try self.planValueTransform(scope, from_child, to_child, provenance),
            };
        }

        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .record = fields,
        });
    }

    fn planTupleTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: []const repr.SessionExecutableTupleElemPayload,
        target: []const repr.SessionExecutableTupleElemPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (source.len != target.len) {
            lambdaInvariant("lambda-solved tuple transform arity mismatch");
        }
        const elems = try self.allocator.alloc(repr.SessionValueTransformTupleElem, target.len);
        defer self.allocator.free(elems);

        for (target, 0..) |target_elem, i| {
            const source_elem = source[i];
            if (source_elem.index != target_elem.index) {
                lambdaInvariant("lambda-solved tuple transform source/target element index mismatch");
            }
            const from_child = try self.transformChildEndpoint(
                scope,
                from,
                .from,
                .{ .tuple_elem = source_elem.index },
                try self.tupleElemLogicalType(from.logical_ty, source_elem.index),
                source_elem.ty,
                source_elem.key,
            );
            const to_child = try self.transformChildEndpoint(
                scope,
                to,
                .to,
                .{ .tuple_elem = target_elem.index },
                try self.tupleElemLogicalType(to.logical_ty, target_elem.index),
                target_elem.ty,
                target_elem.key,
            );
            elems[i] = .{
                .index = target_elem.index,
                .transform = try self.planValueTransform(scope, from_child, to_child, provenance),
            };
        }

        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .tuple = elems,
        });
    }

    fn planTagUnionTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableTagUnionPayload,
        target: repr.SessionExecutableTagUnionPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const cases = try self.allocator.alloc(repr.SessionValueTransformTagCase, source.variants.len);
        @memset(cases, .{
            .source_tag = undefined,
            .target_tag = undefined,
            .payloads = &.{},
        });
        defer {
            for (cases) |case| {
                if (case.payloads.len > 0) self.allocator.free(case.payloads);
            }
            self.allocator.free(cases);
        }

        for (source.variants, 0..) |source_variant, i| {
            const source_label = self.program.row_shapes.tag(source_variant.tag).label;
            const target_variant = self.tagVariantPayloadByLabel(target, source_label) orelse {
                lambdaInvariant("lambda-solved tag transform source tag has no target tag");
            };
            if (source_variant.payloads.len != target_variant.payloads.len) {
                lambdaInvariant("lambda-solved tag transform payload arity mismatch");
            }

            const payloads = try self.allocator.alloc(repr.SessionValueTransformTagPayloadEdge, target_variant.payloads.len);
            errdefer self.allocator.free(payloads);
            for (target_variant.payloads, 0..) |target_payload, payload_i| {
                const source_payload = source_variant.payloads[payload_i];
                const source_index = self.program.row_shapes.tagPayload(source_payload.payload).logical_index;
                const target_index = self.program.row_shapes.tagPayload(target_payload.payload).logical_index;
                if (source_index != target_index) {
                    lambdaInvariant("lambda-solved tag transform source/target payload index mismatch");
                }

                const from_child = try self.transformChildEndpoint(
                    scope,
                    from,
                    .from,
                    .{ .tag_payload = .{ .tag = source_variant.tag, .payload_index = source_index } },
                    try self.tagPayloadLogicalType(from.logical_ty, source_variant.tag, source_index),
                    source_payload.ty,
                    source_payload.key,
                );
                const to_child = try self.transformChildEndpoint(
                    scope,
                    to,
                    .to,
                    .{ .tag_payload = .{ .tag = target_variant.tag, .payload_index = target_index } },
                    try self.tagPayloadLogicalType(to.logical_ty, target_variant.tag, target_index),
                    target_payload.ty,
                    target_payload.key,
                );
                payloads[payload_i] = .{
                    .source_payload_index = source_index,
                    .target_payload_index = target_index,
                    .transform = try self.planValueTransform(scope, from_child, to_child, provenance),
                };
            }

            cases[i] = .{
                .source_tag = source_variant.tag,
                .target_tag = target_variant.tag,
                .payloads = payloads,
            };
        }

        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .tag_union = cases,
        });
    }

    fn planNominalToBackingTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableNominalPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (!repr.canonicalExecValueTypeKeyEql(source.backing_key, to.exec_ty.key)) {
            self.transformPayloadInvariant(from, to, self.resolvedSessionPayload(from.exec_ty.ty), self.resolvedSessionPayload(to.exec_ty.ty));
        }
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .structural_bridge = .nominal_reinterpret,
        });
    }

    fn planBackingToNominalTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        target: repr.SessionExecutableNominalPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (!repr.canonicalExecValueTypeKeyEql(from.exec_ty.key, target.backing_key)) {
            self.transformPayloadInvariant(from, to, self.resolvedSessionPayload(from.exec_ty.ty), self.resolvedSessionPayload(to.exec_ty.ty));
        }
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .structural_bridge = .nominal_reinterpret,
        });
    }

    fn planNominalTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableNominalPayload,
        target: repr.SessionExecutableNominalPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (source.nominal.module_name != target.nominal.module_name or
            source.nominal.type_name != target.nominal.type_name)
        {
            lambdaInvariant("lambda-solved nominal transform source/target nominal mismatch");
        }
        const from_child = try self.transformChildEndpoint(
            scope,
            from,
            .from,
            .{ .nominal_backing = source.nominal },
            try self.nominalBackingLogicalType(from.logical_ty, source.nominal),
            source.backing,
            source.backing_key,
        );
        const to_child = try self.transformChildEndpoint(
            scope,
            to,
            .to,
            .{ .nominal_backing = target.nominal },
            try self.nominalBackingLogicalType(to.logical_ty, target.nominal),
            target.backing,
            target.backing_key,
        );
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{ .nominal = .{
            .nominal = target.nominal,
            .source_ty = target.source_ty,
            .backing = try self.planValueTransform(scope, from_child, to_child, provenance),
        } });
    }

    fn planListTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableTypePayloadChild,
        target: repr.SessionExecutableTypePayloadChild,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const from_child = try self.transformChildEndpoint(
            scope,
            from,
            .from,
            .list_elem,
            try self.listElemLogicalType(from.logical_ty),
            source.ty,
            source.key,
        );
        const to_child = try self.transformChildEndpoint(
            scope,
            to,
            .to,
            .list_elem,
            try self.listElemLogicalType(to.logical_ty),
            target.ty,
            target.key,
        );
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{ .list = .{
            .elem = try self.planValueTransform(scope, from_child, to_child, provenance),
        } });
    }

    fn planBoxTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableTypePayloadChild,
        target: repr.SessionExecutableTypePayloadChild,
        kind: checked_artifact.BoxPayloadTransformKind,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const boundary = self.boxBoundaryForEndpoints(from, to);
        const inherited_provenance = if (provenance.len == 0 and boundary == null)
            self.endpointBoxErasureProvenance(from, to)
        else
            provenance;
        var child_provenance_owned = false;
        const child_provenance = if (boundary) |local_boundary| blk: {
            const extended = try self.extendBoxProvenance(inherited_provenance, local_boundary);
            child_provenance_owned = extended.len != inherited_provenance.len or
                (extended.len > 0 and extended.ptr != inherited_provenance.ptr);
            break :blk extended;
        } else inherited_provenance;
        defer if (child_provenance_owned) self.allocator.free(child_provenance);

        const from_child = try self.transformChildEndpoint(
            scope,
            from,
            .from,
            .box_payload,
            try self.boxPayloadLogicalType(from.logical_ty),
            source.ty,
            source.key,
        );
        const to_child = try self.transformChildEndpoint(
            scope,
            to,
            .to,
            .box_payload,
            try self.boxPayloadLogicalType(to.logical_ty),
            target.ty,
            target.key,
        );
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(child_provenance), .{ .box_payload = .{
            .boundary = boundary,
            .kind = kind,
            .payload = try self.planValueTransform(scope, from_child, to_child, child_provenance),
        } });
    }

    fn endpointBoxErasureProvenance(
        self: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
    ) []const repr.BoxErasureProvenance {
        const from_provenance = self.endpointErasureProvenance(from);
        const to_provenance = self.endpointErasureProvenance(to);
        if (from_provenance.len == 0) return to_provenance;
        if (to_provenance.len == 0) return from_provenance;
        if (!boxErasureProvenanceSetEql(from_provenance, to_provenance)) {
            lambdaInvariant("lambda-solved box transform endpoints have conflicting Box-erasure provenance");
        }
        return from_provenance;
    }

    fn endpointErasureProvenance(
        self: *ValueTransformFinalizer,
        endpoint: repr.SessionExecutableValueEndpoint,
    ) []const repr.BoxErasureProvenance {
        return switch (endpoint.owner) {
            .local_value => |value| self.valueErasureProvenance(self.instance, value),
            .procedure_param => |param| blk: {
                const instance = self.procInstance(param.instance);
                const params = self.valueStoreFor(instance).sliceValueSpan(instance.public_roots.params);
                const raw_index: usize = @intCast(param.index);
                if (raw_index >= params.len) {
                    lambdaInvariant("lambda-solved procedure param endpoint provenance index out of range");
                }
                break :blk self.valueErasureProvenance(instance, params[raw_index]);
            },
            .procedure_return => |instance_id| blk: {
                const instance = self.procInstance(instance_id);
                break :blk self.valueErasureProvenance(instance, instance.public_roots.ret);
            },
            .procedure_capture => |capture| blk: {
                const instance = self.procInstance(capture.instance);
                const captures = self.valueStoreFor(instance).sliceValueSpan(instance.public_roots.captures);
                const raw_slot: usize = @intCast(capture.slot);
                if (raw_slot >= captures.len) {
                    lambdaInvariant("lambda-solved procedure capture endpoint provenance slot out of range");
                }
                break :blk self.valueErasureProvenance(instance, captures[raw_slot]);
            },
            .projection_slot => |projection| self.projectionSlotErasureProvenance(projection),
            .call_raw_arg,
            .erased_proc_value_adapter_arg,
            .erased_finite_adapter_arg,
            .erased_finite_adapter_capture,
            .erased_finite_adapter_result,
            .call_raw_result,
            .consumer_use,
            .transform_child,
            => &.{},
        };
    }

    fn valueErasureProvenance(
        self: *ValueTransformFinalizer,
        instance: *const repr.ProcRepresentationInstance,
        value: repr.ValueInfoId,
    ) []const repr.BoxErasureProvenance {
        const store = self.representationStoreFor(instance);
        const value_store = self.valueStoreFor(instance);
        const raw_value: usize = @intFromEnum(value);
        if (raw_value >= value_store.values.items.len) {
            lambdaInvariant("lambda-solved endpoint provenance referenced missing value");
        }
        const root = value_store.values.items[raw_value].root;
        return self.rootErasureProvenance(store, root);
    }

    fn projectionSlotErasureProvenance(
        self: *ValueTransformFinalizer,
        projection_id: repr.ProjectionInfoId,
    ) []const repr.BoxErasureProvenance {
        const raw_projection: usize = @intFromEnum(projection_id);
        if (raw_projection >= self.valueStore().projections.items.len) {
            lambdaInvariant("lambda-solved projection endpoint provenance referenced missing projection");
        }
        const projection = self.valueStore().projections.items[raw_projection];
        const source_info = self.valueStore().values.items[@intFromEnum(projection.source)];
        const edge_kind: repr.RepresentationChildKind = switch (projection.endpoint_kind orelse projection.kind) {
            .record_field => |field| .{ .record_field = field },
            .tuple_elem => |index| .{ .tuple_elem = index },
            .tag_payload => |payload| .{ .tag_payload = payload },
        };
        const child_root = self.representationStore().solvedStructuralChildRoot(source_info.root, edge_kind) orelse return &.{};
        return self.rootErasureProvenance(self.representationStore(), child_root);
    }

    fn rootErasureProvenance(
        _: *ValueTransformFinalizer,
        store: *const repr.RepresentationStore,
        root: repr.RepRootId,
    ) []const repr.BoxErasureProvenance {
        return store.groupErasureProvenance(store.groupForRoot(root));
    }

    fn planFiniteCallableToErasedTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableCallableSetPayload,
        target: repr.SessionExecutableErasedFnPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        var plan = try self.selectedFiniteCallableErasurePlan(from, to, source, target, provenance);
        defer deinitLocalFiniteSetErasePlan(self.allocator, &plan);
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .callable_to_erased = .{ .finite_value = plan },
        });
    }

    fn selectedFiniteCallableErasurePlan(
        self: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableCallableSetPayload,
        target: repr.SessionExecutableErasedFnPayload,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error!repr.FiniteSetErasePlan {
        const value_id = switch (from.owner) {
            .local_value => |value| value,
            .transform_child,
            .erased_finite_adapter_capture,
            .projection_slot,
            => {
                if (provenance.len == 0) {
                    lambdaInvariant("lambda-solved non-local finite callable erasure has no Box(T) provenance");
                }
                const adapter = repr.ErasedAdapterKey{
                    .source_fn_ty = target.sig_key.source_fn_ty,
                    .callable_set_key = source.key,
                    .erased_fn_sig_key = target.sig_key,
                    .capture_shape_key = target.capture_shape_key,
                };
                const member_targets = try self.finiteErasedAdapterMemberTargetsForCallableSet(source.key, target.sig_key);
                errdefer deinitExecutableSpecializationKeySlice(self.allocator, member_targets);
                const branches = try self.buildFiniteSetEraseAdapterBranches(adapter, member_targets, provenance);
                errdefer deinitLocalFiniteSetEraseBranches(self.allocator, branches);
                const owned_provenance = try self.cloneBoxErasureProvenance(provenance);
                errdefer if (owned_provenance.len > 0) self.allocator.free(owned_provenance);
                return .{
                    .adapter = adapter,
                    .result_ty = to.exec_ty.key,
                    .member_targets = member_targets,
                    .branches = branches,
                    .provenance = owned_provenance,
                };
            },
            else => lambdaInvariant("lambda-solved finite callable erasure reached a non-local source without an assigned emission plan"),
        };
        const value_info = self.valueStore().values.items[@intFromEnum(value_id)];
        const callable = value_info.callable orelse {
            lambdaInvariant("lambda-solved finite callable erasure source value has no callable metadata");
        };
        const emission = self.representationStore().callableEmissionPlan(callable.emission_plan);
        const erase = switch (emission) {
            .erase_finite_set => |erase| erase,
            .finite => |callable_set_key| blk: {
                if (provenance.len == 0) {
                    lambdaInvariant("lambda-solved finite callable erasure reached a callable occurrence before erased emission plans were assigned");
                }
                if (!repr.callableSetKeyEql(callable_set_key, source.key)) {
                    lambdaInvariant("lambda-solved contextual finite callable erasure source key differs from finite emission key");
                }
                const member_targets = try self.finiteErasedAdapterMemberTargetsForCallableSet(source.key, target.sig_key);
                errdefer deinitExecutableSpecializationKeySlice(self.allocator, member_targets);
                const adapter = repr.ErasedAdapterKey{
                    .source_fn_ty = target.sig_key.source_fn_ty,
                    .callable_set_key = source.key,
                    .erased_fn_sig_key = target.sig_key,
                    .capture_shape_key = target.capture_shape_key,
                };
                const branches = try self.buildFiniteSetEraseAdapterBranches(adapter, member_targets, provenance);
                errdefer deinitLocalFiniteSetEraseBranches(self.allocator, branches);
                const owned_provenance = try self.cloneBoxErasureProvenance(provenance);
                errdefer if (owned_provenance.len > 0) self.allocator.free(owned_provenance);
                break :blk repr.FiniteSetErasePlan{
                    .adapter = adapter,
                    .result_ty = to.exec_ty.key,
                    .member_targets = member_targets,
                    .branches = branches,
                    .provenance = owned_provenance,
                };
            },
            .pending_proc_value => lambdaInvariant("lambda-solved finite callable erasure reached a pending proc-value emission"),
            .erase_proc_value => lambdaInvariant("lambda-solved finite callable erasure reached a direct proc-value erase occurrence; its source endpoint should already be erased"),
            .already_erased => lambdaInvariant("lambda-solved finite callable erasure reached an already-erased callable occurrence"),
        };
        if (!repr.callableSetKeyEql(erase.adapter.callable_set_key, source.key)) {
            lambdaInvariant("lambda-solved finite callable erasure selected adapter for a different callable-set key");
        }
        if (!repr.erasedFnSigKeyEql(erase.adapter.erased_fn_sig_key, target.sig_key)) {
            lambdaInvariant("lambda-solved finite callable erasure selected adapter with a different erased signature");
        }
        if (!repr.captureShapeKeyEql(erase.adapter.capture_shape_key, target.capture_shape_key)) {
            lambdaInvariant("lambda-solved finite callable erasure selected adapter with a different capture shape");
        }
        const member_targets = try cloneExecutableSpecializationKeySlice(self.allocator, erase.member_targets);
        errdefer deinitExecutableSpecializationKeySlice(self.allocator, member_targets);
        const branches = if (erase.branches.len == 0)
            try self.buildFiniteSetEraseAdapterBranches(erase.adapter, erase.member_targets, erase.provenance)
        else
            try cloneLocalFiniteSetEraseBranches(self.allocator, erase.branches);
        errdefer deinitLocalFiniteSetEraseBranches(self.allocator, branches);
        const owned_provenance = try self.cloneBoxErasureProvenance(erase.provenance);
        errdefer if (owned_provenance.len > 0) self.allocator.free(owned_provenance);
        return .{
            .adapter = erase.adapter,
            .result_ty = erase.result_ty,
            .member_targets = member_targets,
            .branches = branches,
            .provenance = owned_provenance,
        };
    }

    fn finiteErasedAdapterMemberTargetsForCallableSet(
        self: *ValueTransformFinalizer,
        callable_set_key: repr.CanonicalCallableSetKey,
        sig_key: repr.ErasedFnSigKey,
    ) Allocator.Error![]const repr.ExecutableSpecializationKey {
        const descriptor = self.representationStore().callableSetDescriptor(callable_set_key) orelse {
            lambdaInvariant("lambda-solved finite callable erasure transform referenced missing callable-set descriptor");
        };
        const abi = self.representationStore().erased_fn_abis.abiFor(sig_key.abi) orelse {
            lambdaInvariant("lambda-solved finite callable erasure transform referenced missing erased ABI");
        };
        return try finiteErasedAdapterMemberTargetsForAbi(self.allocator, descriptor.members, abi);
    }

    fn cloneBoxErasureProvenance(
        self: *ValueTransformFinalizer,
        provenance: []const repr.BoxErasureProvenance,
    ) Allocator.Error![]const repr.BoxErasureProvenance {
        if (provenance.len == 0) return &.{};
        return try self.allocator.dupe(repr.BoxErasureProvenance, provenance);
    }

    fn planAlreadyErasedCallableTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableErasedFnPayload,
        target: repr.SessionExecutableErasedFnPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const plan: checked_artifact.AlreadyErasedCallableTransformPlan = if (repr.erasedFnSigKeyEql(source.sig_key, target.sig_key))
            .{ .exact = target.sig_key }
        else if (std.mem.eql(u8, source.sig_key.abi.bytes[0..], target.sig_key.abi.bytes[0..]))
            .{ .same_abi_retype = .{
                .source_sig = source.sig_key,
                .target_sig = target.sig_key,
            } }
        else
            lambdaInvariantFmt(
                "lambda-solved already-erased callable transform changed erased ABI: same_source_fn={} same_abi={}",
                .{
                    repr.canonicalTypeKeyEql(source.sig_key.source_fn_ty, target.sig_key.source_fn_ty),
                    std.mem.eql(u8, source.sig_key.abi.bytes[0..], target.sig_key.abi.bytes[0..]),
                },
            );
        return try self.appendSessionValueTransform(scope, from, to, .none, .{
            .already_erased_callable = plan,
        });
    }

    fn provenanceFor(
        _: *ValueTransformFinalizer,
        provenance: []const repr.BoxErasureProvenance,
    ) checked_artifact.ValueTransformProvenance {
        return if (provenance.len == 0) .none else .{ .box_erasure = provenance };
    }

    fn extendBoxProvenance(
        self: *ValueTransformFinalizer,
        provenance: []const repr.BoxErasureProvenance,
        boundary: repr.BoxBoundaryId,
    ) Allocator.Error![]const repr.BoxErasureProvenance {
        const local = repr.BoxErasureProvenance{ .local_box_boundary = boundary };
        for (provenance) |existing| {
            if (boxErasureProvenanceEql(existing, local)) return provenance;
        }
        const out = try self.allocator.alloc(repr.BoxErasureProvenance, provenance.len + 1);
        @memcpy(out[0..provenance.len], provenance);
        out[provenance.len] = local;
        return out;
    }

    fn transformChildEndpoint(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        parent: repr.SessionExecutableValueEndpoint,
        side: repr.TransformEndpointSide,
        step: repr.TransformEndpointPathStep,
        logical_ty: Type.TypeVarId,
        payload: repr.SessionExecutableTypePayloadRef,
        key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const path = try self.transformChildPath(scope, parent, side, step);
        return .{
            .owner = .{ .transform_child = .{
                .scope = scope,
                .side = side,
                .path = path,
            } },
            .logical_ty = logical_ty,
            .exec_ty = .{
                .ty = payload,
                .key = key,
            },
        };
    }

    fn transformChildPath(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        parent: repr.SessionExecutableValueEndpoint,
        side: repr.TransformEndpointSide,
        step: repr.TransformEndpointPathStep,
    ) Allocator.Error!repr.TransformEndpointPathId {
        const existing = switch (parent.owner) {
            .transform_child => |child| blk: {
                if (child.scope != scope or child.side != side) {
                    lambdaInvariant("lambda-solved transform child endpoint scope mismatch");
                }
                break :blk self.representationStore().transformEndpointPath(child.path);
            },
            else => &.{},
        };
        const steps = try self.allocator.alloc(repr.TransformEndpointPathStep, existing.len + 1);
        defer self.allocator.free(steps);
        @memcpy(steps[0..existing.len], existing);
        steps[existing.len] = step;
        return try self.representationStore().appendTransformEndpointPath(steps);
    }

    fn recordFieldPayloadByLabel(
        self: *ValueTransformFinalizer,
        record: repr.SessionExecutableRecordPayload,
        label: canonical.RecordFieldLabelId,
    ) ?repr.SessionExecutableRecordFieldPayload {
        for (record.fields) |field| {
            if (self.program.row_shapes.recordField(field.field).label == label) return field;
        }
        return null;
    }

    fn tagVariantPayloadByLabel(
        self: *ValueTransformFinalizer,
        tag_union: repr.SessionExecutableTagUnionPayload,
        label: canonical.TagLabelId,
    ) ?repr.SessionExecutableTagVariantPayload {
        for (tag_union.variants) |variant| {
            if (self.program.row_shapes.tag(variant.tag).label == label) return variant;
        }
        return null;
    }

    fn recordFieldLogicalType(
        self: *ValueTransformFinalizer,
        record_ty: Type.TypeVarId,
        field: MonoRow.RecordFieldId,
    ) Allocator.Error!Type.TypeVarId {
        const label = self.program.row_shapes.recordField(field).label;
        const root = self.program.types.unlinkConst(record_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved record transform endpoint has non-record logical type"),
        };
        const record = switch (content) {
            .record => |record| record,
            else => lambdaInvariant("lambda-solved record transform endpoint has non-record content"),
        };
        const fields = self.program.types.sliceFields(record.fields);
        for (fields) |candidate| {
            if (candidate.name == label) return candidate.ty;
        }
        lambdaInvariant("lambda-solved record transform field missing from logical type");
    }

    fn tupleElemLogicalType(
        self: *ValueTransformFinalizer,
        tuple_ty: Type.TypeVarId,
        elem: u32,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(tuple_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved tuple transform endpoint has non-tuple logical type"),
        };
        const tuple = switch (content) {
            .tuple => |tuple| self.program.types.sliceTypeVarSpan(tuple),
            else => lambdaInvariant("lambda-solved tuple transform endpoint has non-tuple content"),
        };
        const index: usize = @intCast(elem);
        if (index >= tuple.len) lambdaInvariant("lambda-solved tuple transform element index out of range");
        return tuple[index];
    }

    fn tagPayloadLogicalType(
        self: *ValueTransformFinalizer,
        tag_union_ty: Type.TypeVarId,
        tag: MonoRow.TagId,
        payload_index: u32,
    ) Allocator.Error!Type.TypeVarId {
        const label = self.program.row_shapes.tag(tag).label;
        const root = self.program.types.unlinkConst(tag_union_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved tag transform endpoint has non-tag logical type"),
        };
        const tag_union = switch (content) {
            .tag_union => |tag_union| tag_union,
            else => lambdaInvariant("lambda-solved tag transform endpoint has non-tag content"),
        };
        const tags = self.program.types.sliceTags(tag_union.tags);
        for (tags) |candidate| {
            if (candidate.name != label) continue;
            const args = self.program.types.sliceTypeVarSpan(candidate.args);
            const index: usize = @intCast(payload_index);
            if (index >= args.len) lambdaInvariant("lambda-solved tag transform payload index out of range");
            return args[index];
        }
        lambdaInvariant("lambda-solved tag transform tag missing from logical type");
    }

    fn nominalBackingLogicalType(
        self: *ValueTransformFinalizer,
        nominal_ty: Type.TypeVarId,
        nominal_key: canonical.NominalTypeKey,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(nominal_ty);
        return switch (self.program.types.getNode(root)) {
            .nominal => |nominal| blk: {
                if (nominal.nominal.module_name != nominal_key.module_name or
                    nominal.nominal.type_name != nominal_key.type_name)
                {
                    lambdaInvariant("lambda-solved nominal transform logical nominal mismatch");
                }
                break :blk nominal.backing;
            },
            else => lambdaInvariant("lambda-solved nominal transform endpoint has non-nominal logical type"),
        };
    }

    fn nominalBackingOrAlreadyBackingLogicalType(
        self: *ValueTransformFinalizer,
        logical_ty: Type.TypeVarId,
        nominal_key: canonical.NominalTypeKey,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(logical_ty);
        return switch (self.program.types.getNode(root)) {
            .nominal => |nominal| blk: {
                if (nominal.nominal.module_name != nominal_key.module_name or
                    nominal.nominal.type_name != nominal_key.type_name)
                {
                    lambdaInvariant("lambda-solved nominal endpoint logical nominal mismatch");
                }
                break :blk nominal.backing;
            },
            .content => logical_ty,
            else => lambdaInvariant("lambda-solved nominal endpoint has unresolved logical type"),
        };
    }

    fn listElemLogicalType(
        self: *ValueTransformFinalizer,
        list_ty: Type.TypeVarId,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(list_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved list transform endpoint has non-list logical type"),
        };
        return switch (content) {
            .list => |elem| elem,
            else => lambdaInvariant("lambda-solved list transform endpoint has non-list content"),
        };
    }

    fn boxPayloadLogicalType(
        self: *ValueTransformFinalizer,
        box_ty: Type.TypeVarId,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(box_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved box transform endpoint has non-box logical type"),
        };
        return switch (content) {
            .box => |payload| payload,
            else => lambdaInvariant("lambda-solved box transform endpoint has non-box content"),
        };
    }

    fn boxBoundaryForEndpoints(
        self: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
    ) ?repr.BoxBoundaryId {
        if (self.boxBoundaryForEndpoint(from)) |boundary| return boundary;
        return self.boxBoundaryForEndpoint(to);
    }

    fn boxBoundaryForEndpoint(
        self: *ValueTransformFinalizer,
        endpoint: repr.SessionExecutableValueEndpoint,
    ) ?repr.BoxBoundaryId {
        return switch (endpoint.owner) {
            .local_value => |value| blk: {
                const info = self.valueStore().values.items[@intFromEnum(value)];
                break :blk if (info.boxed) |boxed| boxed.boundary else null;
            },
            else => null,
        };
    }

    fn localEndpoint(
        self: *ValueTransformFinalizer,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const info = self.valueStore().values.items[@intFromEnum(value)];
        if (info.exec_ty) |exec_ty| {
            return .{
                .owner = .{ .local_value = value },
                .logical_ty = info.logical_ty,
                .exec_ty = exec_ty,
            };
        }
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
        const arg_index: usize = @intCast(index);
        const expected_key = target_instance.executable_specialization_key.exec_arg_tys[arg_index];
        const exec_ty = try self.targetEndpointForExpectedKey(
            target_instance,
            target_value_store,
            target_value,
            expected_key,
            "lambda-solved target procedure parameter endpoint key differs from target executable specialization",
        );
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
        const expected_key = target_instance.executable_specialization_key.exec_ret_ty;
        const exec_ty = try self.targetEndpointForExpectedKey(
            target_instance,
            target_value_store,
            target_value,
            expected_key,
            "lambda-solved target procedure return endpoint key differs from target executable specialization",
        );
        return .{
            .owner = .{ .procedure_return = target_id },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn targetCaptureEndpoint(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        slot: u32,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        const exec_ty = try self.targetEndpointForExpectedKey(
            target_instance,
            target_value_store,
            target_value,
            expected_key,
            "lambda-solved target procedure capture endpoint key differs from callable-set member schema",
        );
        return .{
            .owner = .{ .procedure_capture = .{
                .instance = target_id,
                .slot = slot,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn targetCaptureEndpointFromActualValue(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        slot: u32,
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
        return .{
            .owner = .{ .procedure_capture = .{
                .instance = target_id,
                .slot = slot,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn targetEndpointForExpectedKey(
        self: *ValueTransformFinalizer,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value_store: *const repr.ValueInfoStore,
        target_value: repr.ValueInfoId,
        expected_key: repr.CanonicalExecValueTypeKey,
        comptime mismatch_message: []const u8,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        if (target_instance.boundary_payloads) |boundary_payloads| {
            return try self.importArtifactBoundaryEndpoint(boundary_payloads, expected_key);
        }
        if (self.representationStore().session_executable_type_payloads.refForKey(expected_key)) |published| {
            return .{
                .ty = published,
                .key = expected_key,
            };
        }
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
        if (!repr.canonicalExecValueTypeKeyEql(exec_ty.key, expected_key)) {
            lambdaInvariant(mismatch_message);
        }
        return exec_ty;
    }

    fn importArtifactBoundaryEndpoint(
        self: *ValueTransformFinalizer,
        boundary_payloads: repr.ProcBoundaryExecutablePayloads,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        const source_ref = boundary_payloads.payloads.refForKey(.{ .bytes = boundary_payloads.artifact.bytes }, expected_key) orelse {
            lambdaInvariant("lambda-solved call boundary key has no published executable payload");
        };
        const source_names = canonicalNamesForArtifactViews(self.artifact_views, boundary_payloads.artifact);
        var importer = ArtifactExecutablePayloadImporter.init(
            self.allocator,
            source_names,
            &self.program.canonical_names,
            &self.program.row_shapes,
            boundary_payloads.artifact,
            boundary_payloads.payloads,
            &self.representationStore().session_executable_type_payloads,
        );
        defer importer.deinit();
        return try importer.importRef(source_ref, expected_key);
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

    fn rawErasedProcValueAdapterArgEndpoint(
        self: *ValueTransformFinalizer,
        emission_plan_id: repr.CallableValueEmissionPlanId,
        source_value: repr.ValueInfoId,
        erase: repr.ProcValueErasePlan,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        index: u32,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        return .{
            .owner = .{ .erased_proc_value_adapter_arg = .{
                .emission_plan = emission_plan_id,
                .source_value = source_value,
                .proc_value = erase.proc_value,
                .erased_fn_sig_key = erase.erased_fn_sig_key,
                .index = index,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn rawErasedFiniteAdapterArgEndpoint(
        self: *ValueTransformFinalizer,
        adapter: repr.ErasedAdapterKey,
        member: repr.CallableSetMemberRef,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        index: u32,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        return .{
            .owner = .{ .erased_finite_adapter_arg = .{
                .adapter = adapter,
                .member = member,
                .index = index,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn rawErasedFiniteAdapterCaptureEndpoint(
        self: *ValueTransformFinalizer,
        adapter: repr.ErasedAdapterKey,
        member: repr.CallableSetMemberRef,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        slot: u32,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        return .{
            .owner = .{ .erased_finite_adapter_capture = .{
                .adapter = adapter,
                .member = member,
                .slot = slot,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn rawErasedFiniteAdapterResultEndpoint(
        self: *ValueTransformFinalizer,
        adapter: repr.ErasedAdapterKey,
        member: repr.CallableSetMemberRef,
        target_instance: *const repr.ProcRepresentationInstance,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_instance.public_roots.ret)];
        return .{
            .owner = .{ .erased_finite_adapter_result = .{
                .adapter = adapter,
                .member = member,
            } },
            .logical_ty = target_info.logical_ty,
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

    fn procedureBoundaryProvenance(
        _: *ValueTransformFinalizer,
        target_instance: *const repr.ProcRepresentationInstance,
    ) []const repr.BoxErasureProvenance {
        return target_instance.boundary_provenance;
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

    fn exprValue(self: *const ValueTransformFinalizer, expr_id: Ast.ExprId) repr.ValueInfoId {
        return self.program.ast.exprs.items[@intFromEnum(expr_id)].value_info;
    }

    fn exprCanCompleteNormally(self: *const ValueTransformFinalizer, expr_id: Ast.ExprId) bool {
        return switch (self.program.ast.exprs.items[@intFromEnum(expr_id)].data) {
            .return_,
            .crash,
            .runtime_error,
            => false,
            .block => |block| self.blockCanCompleteNormally(block.stmts, block.final_expr),
            .if_ => |if_| self.exprCanCompleteNormally(if_.then_body) or
                self.exprCanCompleteNormally(if_.else_body),
            .match_ => |match_| self.anyBranchCanCompleteNormally(match_.branches),
            else => true,
        };
    }

    fn blockCanCompleteNormally(
        self: *const ValueTransformFinalizer,
        stmts: Ast.Span(Ast.StmtId),
        final_expr: Ast.ExprId,
    ) bool {
        const stmt_ids = self.program.ast.stmt_ids.items[stmts.start..][0..stmts.len];
        for (stmt_ids) |stmt_id| {
            if (!self.stmtCanCompleteNormally(stmt_id)) return false;
        }
        return self.exprCanCompleteNormally(final_expr);
    }

    fn stmtCanCompleteNormally(self: *const ValueTransformFinalizer, stmt_id: Ast.StmtId) bool {
        return switch (self.program.ast.stmts.items[@intFromEnum(stmt_id)]) {
            .decl => |decl| self.exprCanCompleteNormally(decl.body),
            .var_decl => |decl| self.exprCanCompleteNormally(decl.body),
            .reassign => |reassign| self.exprCanCompleteNormally(reassign.body),
            .expr => |expr| self.exprCanCompleteNormally(expr),
            .debug => |expr| self.exprCanCompleteNormally(expr),
            .expect => |expr| self.exprCanCompleteNormally(expr),
            .crash,
            .return_,
            .break_,
            => false,
            .for_,
            .while_,
            => true,
        };
    }

    fn anyBranchCanCompleteNormally(
        self: *const ValueTransformFinalizer,
        branches: Ast.Span(Ast.BranchId),
    ) bool {
        const branch_ids = self.program.ast.branch_ids.items[branches.start..][0..branches.len];
        for (branch_ids) |branch_id| {
            if (self.exprCanCompleteNormally(self.program.ast.branches.items[@intFromEnum(branch_id)].body)) return true;
        }
        return false;
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
    concrete_source_types: *const ConcreteSourceType.Store,
    name_resolver: *ArtifactNames.ArtifactNameResolver,
    artifact_views: ArtifactViews,
    use_concrete_source_payloads: bool,
    active: std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId),
    concrete_active: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, Type.TypeVarId),
    concrete_imported: std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, Type.TypeVarId),

    fn init(
        allocator: Allocator,
        input: *const Lifted.Type.Store,
        output: *Type.Store,
        concrete_source_types: *const ConcreteSourceType.Store,
        name_resolver: *ArtifactNames.ArtifactNameResolver,
        artifact_views: ArtifactViews,
        use_concrete_source_payloads: bool,
    ) TypeImporter {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .concrete_source_types = concrete_source_types,
            .name_resolver = name_resolver,
            .artifact_views = artifact_views,
            .use_concrete_source_payloads = use_concrete_source_payloads,
            .active = std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId).init(allocator),
            .concrete_active = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, Type.TypeVarId).init(allocator),
            .concrete_imported = std.AutoHashMap(ConcreteSourceType.ConcreteSourceTypeRef, Type.TypeVarId).init(allocator),
        };
    }

    fn deinit(self: *TypeImporter) void {
        self.concrete_imported.deinit();
        self.concrete_active.deinit();
        self.active.deinit();
    }

    fn importType(self: *TypeImporter, source: Lifted.Type.TypeId) Allocator.Error!Type.TypeVarId {
        return try self.importTypeWithConcretePayloadMode(source, self.use_concrete_source_payloads);
    }

    fn importTypeRaw(self: *TypeImporter, source: Lifted.Type.TypeId) Allocator.Error!Type.TypeVarId {
        return try self.importTypeWithConcretePayloadMode(source, false);
    }

    fn importTypeWithConcretePayloadMode(
        self: *TypeImporter,
        source: Lifted.Type.TypeId,
        use_concrete_source_payloads: bool,
    ) Allocator.Error!Type.TypeVarId {
        switch (self.input.getTypePreservingNominal(source)) {
            .link => |next| return try self.importTypeWithConcretePayloadMode(next, use_concrete_source_payloads),
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
                    args[i] = try self.importTypeWithConcretePayloadMode(arg, use_concrete_source_payloads);
                }
                const ret = try self.importTypeWithConcretePayloadMode(func.ret, use_concrete_source_payloads);
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
                    args[i] = try self.importTypeWithConcretePayloadMode(arg, use_concrete_source_payloads);
                }
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .source_ty = nominal.source_ty,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.output.addTypeVarSpan(args),
                    .backing = if (use_concrete_source_payloads)
                        try self.concreteNominalBacking(nominal)
                    else
                        try self.importTypeWithConcretePayloadMode(nominal.backing, false),
                } };
            },
            .list => |elem| .{ .content = .{ .list = try self.importTypeWithConcretePayloadMode(elem, use_concrete_source_payloads) } },
            .box => |elem| .{ .content = .{ .box = try self.importTypeWithConcretePayloadMode(elem, use_concrete_source_payloads) } },
            .tuple => |elems| blk: {
                const items = try self.allocator.alloc(Type.TypeVarId, elems.len);
                defer self.allocator.free(items);
                for (elems, 0..) |elem, i| {
                    items[i] = try self.importTypeWithConcretePayloadMode(elem, use_concrete_source_payloads);
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
                        args[j] = try self.importTypeWithConcretePayloadMode(arg, use_concrete_source_payloads);
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
                        .ty = try self.importTypeWithConcretePayloadMode(field.ty, use_concrete_source_payloads),
                    };
                }
                break :blk .{ .content = .{ .record = .{ .fields = try self.output.addFields(fields) } } };
            },
        };

        self.output.setNode(target, node);
        _ = self.active.remove(source);
        return target;
    }

    fn concreteNominalBacking(
        self: *TypeImporter,
        nominal: anytype,
    ) Allocator.Error!Type.TypeVarId {
        if (isEmptyCanonicalTypeKey(nominal.source_ty)) {
            return try self.importTypeWithConcretePayloadMode(nominal.backing, true);
        }
        const concrete = self.concrete_source_types.refForKey(nominal.source_ty) orelse {
            lambdaInvariant("lambda-solved nominal source type key has no concrete source type payload");
        };
        const imported = try self.importConcreteRef(concrete);
        const imported_root = self.output.unlinkConst(imported);
        return switch (self.output.getNode(imported_root)) {
            .nominal => |imported_nominal| blk: {
                if (imported_nominal.nominal.module_name != nominal.nominal.module_name or
                    imported_nominal.nominal.type_name != nominal.nominal.type_name)
                {
                    lambdaInvariant("lambda-solved concrete nominal payload identity differs from occurrence nominal");
                }
                break :blk imported_nominal.backing;
            },
            else => try self.importTypeWithConcretePayloadMode(nominal.backing, true),
        };
    }

    fn importConcreteRef(
        self: *TypeImporter,
        ref: ConcreteSourceType.ConcreteSourceTypeRef,
    ) Allocator.Error!Type.TypeVarId {
        if (self.concrete_imported.get(ref)) |existing| return existing;
        if (self.concrete_active.get(ref)) |active| return active;

        const target = try self.output.freshUnbd();
        try self.concrete_active.put(ref, target);
        errdefer _ = self.concrete_active.remove(ref);

        var temp_store = Lifted.Type.Store.init(self.allocator);
        defer temp_store.deinit();

        const root = self.concrete_source_types.root(ref);
        const temp_ty = switch (root.source) {
            .artifact => |artifact_ref| blk: {
                const checked_types = checkedTypesForArtifactViews(self.artifact_views, artifact_ref.artifact) orelse {
                    lambdaInvariant("lambda-solved concrete source type artifact was not available");
                };
                var lowerer = MonoLowerType.Lowerer.initWithResolver(
                    self.allocator,
                    checked_types,
                    &temp_store,
                    self.name_resolver,
                    artifact_ref.artifact,
                );
                defer lowerer.deinit();
                break :blk try lowerer.lowerChecked(artifact_ref.ty);
            },
            .local => |local| blk: {
                var lowerer = MonoLowerType.Lowerer.init(
                    self.allocator,
                    self.concrete_source_types.localView(),
                    &temp_store,
                    self.name_resolver.lowering_names,
                );
                defer lowerer.deinit();
                break :blk try lowerer.lowerChecked(local);
            },
        };

        var raw_importer = TypeImporter.init(
            self.allocator,
            &temp_store,
            self.output,
            self.concrete_source_types,
            self.name_resolver,
            self.artifact_views,
            false,
        );
        defer raw_importer.deinit();

        const imported = try raw_importer.importTypeRaw(temp_ty);
        self.output.setNode(target, .{ .link = imported });
        _ = self.concrete_active.remove(ref);
        try self.concrete_imported.put(ref, target);
        return target;
    }
};

fn checkedTypesForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?checked_artifact.CheckedTypeStoreView {
    if (std.mem.eql(u8, &views.root.artifact.key.bytes, &key.bytes)) return views.root.artifact.checked_types.view();
    for (views.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.checked_types;
    }
    for (views.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.checked_types;
    }
    return null;
}

fn executablePayloadsForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.ExecutableTypePayloadStore {
    if (std.mem.eql(u8, &views.root.artifact.key.bytes, &key.bytes)) return &views.root.artifact.executable_type_payloads;
    for (views.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.executable_type_payloads;
    }
    for (views.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.executable_type_payloads;
    }
    return null;
}

fn compileTimePlansForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.CompileTimePlanStore {
    if (std.mem.eql(u8, &views.root.artifact.key.bytes, &key.bytes)) return &views.root.artifact.comptime_plans;
    for (views.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.comptime_plans;
    }
    for (views.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.comptime_plans;
    }
    return null;
}

fn erasedFnAbisForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const canonical.ErasedFnAbiStore {
    if (std.mem.eql(u8, &views.root.artifact.key.bytes, &key.bytes)) return &views.root.artifact.erased_fn_abis;
    for (views.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.erased_fn_abis;
    }
    for (views.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.erased_fn_abis;
    }
    return null;
}

fn canonicalNamesForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
) *const canonical.CanonicalNameStore {
    if (artifactKeyEql(views.root.artifact.key, key)) return &views.root.artifact.canonical_names;
    for (views.imports) |imported| {
        if (artifactKeyEql(imported.key, key)) return imported.canonical_names;
    }
    for (views.root.relation_artifacts) |related| {
        if (artifactKeyEql(related.key, key)) return related.canonical_names;
    }
    lambdaInvariant("lambda-solved artifact executable payload referenced unavailable canonical names");
}

fn erasedCallableLeafCaptureShapeKey(
    erased: checked_artifact.ErasedCallableLeafInstance,
) canonical.CaptureShapeKey {
    return switch (erased.sealed.code) {
        .direct_proc => |direct| direct.code.capture_shape_key,
        .finite_adapter => |finite| finite.adapter_key.capture_shape_key,
    };
}

fn artifactKeyEql(
    left: checked_artifact.CheckedModuleArtifactKey,
    right: checked_artifact.CheckedModuleArtifactKey,
) bool {
    return std.mem.eql(u8, &left.bytes, &right.bytes);
}

fn artifactRefEql(
    left: canonical.ArtifactRef,
    right: checked_artifact.CheckedModuleArtifactKey,
) bool {
    return std.mem.eql(u8, &left.bytes, &right.bytes);
}

fn artifactCheckedTypeSourceForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
    root: checked_artifact.CheckedTypeId,
) ConcreteSourceTypeView {
    if (std.mem.eql(u8, &views.root.artifact.key.bytes, &key.bytes)) return .{
        .names = &views.root.artifact.canonical_names,
        .view = views.root.artifact.checked_types.view(),
        .root = root,
    };
    for (views.imports) |imported| {
        if (!std.mem.eql(u8, &imported.key.bytes, &key.bytes)) continue;
        return .{
            .names = imported.canonical_names,
            .view = imported.checked_types,
            .root = root,
        };
    }
    for (views.root.relation_artifacts) |related| {
        if (!std.mem.eql(u8, &related.key.bytes, &key.bytes)) continue;
        return .{
            .names = related.canonical_names,
            .view = related.checked_types,
            .root = root,
        };
    }
    lambdaInvariant("lambda-solved concrete source type artifact was not available");
}

const ConcreteSourceTypeView = struct {
    names: *const canonical.CanonicalNameStore,
    view: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
};

const CheckedFunctionSource = struct {
    names: *const canonical.CanonicalNameStore,
    view: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
    function: checked_artifact.CheckedFunctionType,
};

fn concreteSourceTypeViewForRef(
    concrete_source_types: *const ConcreteSourceType.Store,
    views: ArtifactViews,
    local_names: *const canonical.CanonicalNameStore,
    ref: ConcreteSourceType.ConcreteSourceTypeRef,
) ConcreteSourceTypeView {
    const root = concrete_source_types.root(ref);
    return switch (root.source) {
        .local => |local| .{
            .names = local_names,
            .view = concrete_source_types.localView(),
            .root = local,
        },
        .artifact => |artifact_ref| artifactCheckedTypeSourceForArtifactViews(views, artifact_ref.artifact, artifact_ref.ty),
    };
}

fn checkedTypePayload(
    view: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
) checked_artifact.CheckedTypePayload {
    const index = @intFromEnum(root);
    if (index >= view.payloads.len) {
        lambdaInvariant("lambda-solved checked source type payload id is out of range");
    }
    return view.payloads[index];
}

fn checkedTypeRootKey(
    view: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
) canonical.CanonicalTypeKey {
    const index = @intFromEnum(root);
    if (index >= view.roots.len) {
        lambdaInvariant("lambda-solved checked source type root id is out of range");
    }
    return view.roots[index].key;
}

fn resolvedCheckedFunctionSource(
    names: *const canonical.CanonicalNameStore,
    view: checked_artifact.CheckedTypeStoreView,
    root: checked_artifact.CheckedTypeId,
) CheckedFunctionSource {
    var current = root;
    while (true) {
        switch (checkedTypePayload(view, current)) {
            .alias => |alias| current = alias.backing,
            .function => |function| return .{
                .names = names,
                .view = view,
                .root = current,
                .function = function,
            },
            else => lambdaInvariant("lambda-solved erased adapter source type key did not resolve to a function"),
        }
    }
}

fn checkedFunctionSourceForKey(
    concrete_source_types: *const ConcreteSourceType.Store,
    views: ArtifactViews,
    local_names: *const canonical.CanonicalNameStore,
    source_fn_ty: canonical.CanonicalTypeKey,
) CheckedFunctionSource {
    if (isEmptyCanonicalTypeKey(source_fn_ty)) {
        lambdaInvariant("lambda-solved erased adapter source function key is empty");
    }
    const ref = concrete_source_types.refForKey(source_fn_ty) orelse {
        lambdaInvariant("lambda-solved erased adapter source function key has no checked payload");
    };
    const concrete = concreteSourceTypeViewForRef(concrete_source_types, views, local_names, ref);
    return resolvedCheckedFunctionSource(concrete.names, concrete.view, concrete.root);
}

fn callableSetDescriptorsForArtifactViews(
    views: ArtifactViews,
    key: checked_artifact.CheckedModuleArtifactKey,
) ?*const checked_artifact.CallableSetDescriptorStore {
    if (std.mem.eql(u8, &views.root.artifact.key.bytes, &key.bytes)) return &views.root.artifact.callable_set_descriptors;
    for (views.imports) |imported| {
        if (std.mem.eql(u8, &imported.key.bytes, &key.bytes)) return imported.callable_set_descriptors;
    }
    for (views.root.relation_artifacts) |related| {
        if (std.mem.eql(u8, &related.key.bytes, &key.bytes)) return related.callable_set_descriptors;
    }
    return null;
}

const ConstMaterializationView = struct {
    owner: checked_artifact.CheckedModuleArtifactKey,
    names: *const canonical.CanonicalNameStore,
    values: *const checked_artifact.CompileTimeValueStore,
};

const ResolvedConstBackedValue = struct {
    materialization: ConstMaterializationView,
    schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValueId,
};

const ResolvedConstInstance = struct {
    materialization: ConstMaterializationView,
    instance: checked_artifact.ConstInstance,
};

const ProvidedDataValueVisitKey = struct {
    owner: [32]u8,
    schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValueId,
};

const ProvidedDataMaterializationNodeVisitKey = struct {
    owner: [32]u8,
    node: checked_artifact.ErasedCaptureExecutableMaterializationNodeId,
};

const ProvidedDataConstInstanceVisitKey = struct {
    owner: [32]u8,
    instance: checked_artifact.ConstInstanceId,
};

fn constSchemaForMaterialization(
    materialization: ConstMaterializationView,
    id: checked_artifact.ComptimeSchemaId,
) checked_artifact.ComptimeSchema {
    const index: usize = @intFromEnum(id);
    if (index >= materialization.values.schemas.items.len) {
        lambdaInvariant("lambda-solved provided data export dependency schema id out of range");
    }
    return materialization.values.schemas.items[index];
}

fn constValueForMaterialization(
    materialization: ConstMaterializationView,
    id: checked_artifact.ComptimeValueId,
) checked_artifact.ComptimeValue {
    const index: usize = @intFromEnum(id);
    if (index >= materialization.values.values.items.len) {
        lambdaInvariant("lambda-solved provided data export dependency value id out of range");
    }
    return materialization.values.values.items[index];
}

fn constMaterializationForArtifactViews(
    views: ArtifactViews,
    owner: checked_artifact.CheckedModuleArtifactKey,
) ConstMaterializationView {
    if (artifactKeyEql(views.root.artifact.key, owner)) {
        return .{
            .owner = views.root.artifact.key,
            .names = &views.root.artifact.canonical_names,
            .values = &views.root.artifact.comptime_values,
        };
    }
    for (views.imports) |imported| {
        if (!artifactKeyEql(imported.key, owner)) continue;
        return .{
            .owner = imported.key,
            .names = imported.canonical_names,
            .values = imported.comptime_values,
        };
    }
    for (views.root.relation_artifacts) |related| {
        if (!artifactKeyEql(related.key, owner)) continue;
        return .{
            .owner = related.key,
            .names = related.canonical_names,
            .values = related.comptime_values,
        };
    }
    lambdaInvariant("lambda-solved provided data export dependency referenced unavailable materialization owner");
}

fn resolveConstInstanceForArtifactViews(
    views: ArtifactViews,
    ref: checked_artifact.ConstInstanceRef,
) ResolvedConstInstance {
    const materialization = constMaterializationForArtifactViews(views, ref.owner);
    const instances = constInstancesForArtifactViews(views, ref.owner);
    if (!artifactKeyEql(instances.owner, ref.owner)) {
        lambdaInvariant("lambda-solved provided data export dependency const instance view had wrong owner");
    }
    const index: usize = @intFromEnum(ref.instance);
    if (index >= instances.instances.len) {
        lambdaInvariant("lambda-solved provided data export dependency const instance id out of range");
    }
    const record = instances.instances[index];
    if (!checked_artifact.constInstantiationKeyEql(record.key, ref.key)) {
        lambdaInvariant("lambda-solved provided data export dependency const instance key mismatch");
    }
    const instance = switch (record.state) {
        .evaluated => |evaluated| evaluated,
        .reserved,
        .evaluating,
        => lambdaInvariant("lambda-solved provided data export dependency reached unsealed const instance"),
    };
    return .{
        .materialization = materialization,
        .instance = instance,
    };
}

fn constInstancesForArtifactViews(
    views: ArtifactViews,
    owner: checked_artifact.CheckedModuleArtifactKey,
) checked_artifact.ConstInstantiationStoreView {
    if (artifactKeyEql(views.root.artifact.key, owner)) return views.root.artifact.const_instances.view();
    for (views.imports) |imported| {
        if (artifactKeyEql(imported.key, owner)) return imported.const_instances;
    }
    for (views.root.relation_artifacts) |related| {
        if (artifactKeyEql(related.key, owner)) return related.const_instances;
    }
    lambdaInvariant("lambda-solved provided data export dependency referenced unavailable const instances");
}

const ConstBackedProjectionResult = struct {
    value: repr.ValueInfoId,
    projection: repr.ProjectionInfoId,
};

const BodySolver = struct {
    allocator: Allocator,
    input: *const Lifted.Ast.Store,
    output: *Ast.Store,
    canonical_names: *const canonical.CanonicalNameStore,
    row_shapes: *MonoRow.Store,
    symbols: *const symbol_mod.Store,
    type_importer: *TypeImporter,
    concrete_source_types: *const ConcreteSourceType.Store,
    artifact_views: ArtifactViews,
    representation_store: *repr.RepresentationStore,
    value_store: *repr.ValueInfoStore,
    env: std.AutoHashMap(Ast.Symbol, repr.BindingInfoId),
    expr_map: std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId),
    instance: repr.ProcRepresentationInstanceId,
    registry: *ProcedureInstanceRegistry,
    public_roots: ?repr.ProcPublicValueRoots = null,
    existing_public_roots: ?repr.ProcPublicValueRoots = null,
    active_captures: ?repr.Span(repr.ValueInfoId) = null,
    active_return_value: ?repr.ValueInfoId = null,
    active_source_match_branch: ?repr.SourceMatchBranchRef = null,
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
                    const existing = self.existing_public_roots;
                    const lowered_args = if (existing) |roots|
                        try self.lowerParamSpanWithExistingValues(fn_.args, roots.params)
                    else
                        try self.lowerParamSpan(fn_.args);
                    const capture_values = if (existing) |roots|
                        try self.lowerCaptureSlotRootsWithExistingValues(fn_.captures, roots.captures)
                    else
                        try self.lowerCaptureSlotRoots(fn_.captures);
                    const expected_ret = try self.expectedReturnFromExpr(fn_.body);
                    const public_ret = if (existing) |roots| roots.ret else try self.newValue(expected_ret.ty, expected_ret.source_ty);
                    try self.publishProcedureReturnRootKind(public_ret);
                    const previous_captures = self.active_captures;
                    self.active_captures = capture_values;
                    defer self.active_captures = previous_captures;
                    const previous_return = self.active_return_value;
                    self.active_return_value = public_ret;
                    defer self.active_return_value = previous_return;
                    const raw_body = try self.lowerExpr(fn_.body);
                    const body = try self.wrapImplicitReturn(raw_body, expected_ret, public_ret);
                    const function_root = if (existing) |roots| roots.function_root else self.representation_store.reserveRoot();
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = public_ret,
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
                    const existing = self.existing_public_roots;
                    const lowered_args = if (existing) |roots|
                        try self.lowerParamSpanWithExistingValues(hosted.args, roots.params)
                    else
                        try self.lowerParamSpan(hosted.args);
                    const ret_ty = try self.type_importer.importType(hosted.ret_ty);
                    const ret = if (existing) |roots| roots.ret else try self.newValue(ret_ty, .{});
                    try self.publishProcedureReturnRootKind(ret);
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = ret,
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = if (existing) |roots| roots.function_root else self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .hosted_fn = .{
                        .proc = hosted.proc,
                        .args = lowered_args.symbols,
                        .ret_ty = ret_ty,
                        .hosted = hosted.hosted,
                    } };
                },
                .val => |expr| blk: {
                    const existing = self.existing_public_roots;
                    const expected_ret = try self.expectedReturnFromExpr(expr);
                    const public_ret = if (existing) |roots| roots.ret else try self.newValue(expected_ret.ty, expected_ret.source_ty);
                    try self.publishProcedureReturnRootKind(public_ret);
                    const previous_return = self.active_return_value;
                    self.active_return_value = public_ret;
                    defer self.active_return_value = previous_return;
                    const raw_body = try self.lowerExpr(expr);
                    const body = try self.wrapImplicitReturn(raw_body, expected_ret, public_ret);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = public_ret,
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = if (existing) |roots| roots.function_root else self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .val = body };
                },
                .run => |run_def| blk: {
                    const existing = self.existing_public_roots;
                    const expected_ret = try self.expectedReturnFromExpr(run_def.body);
                    const public_ret = if (existing) |roots| roots.ret else try self.newValue(expected_ret.ty, expected_ret.source_ty);
                    try self.publishProcedureReturnRootKind(public_ret);
                    const previous_return = self.active_return_value;
                    self.active_return_value = public_ret;
                    defer self.active_return_value = previous_return;
                    const raw_body = try self.lowerExpr(run_def.body);
                    const body = try self.wrapImplicitReturn(raw_body, expected_ret, public_ret);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = public_ret,
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = if (existing) |roots| roots.function_root else self.representation_store.reserveRoot(),
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

    const ExpectedReturn = struct {
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
    };

    fn lowerDefPublicRoots(self: *BodySolver, def_id: Lifted.Ast.DefId) Allocator.Error!repr.ProcPublicValueRoots {
        const def = self.input.getDef(def_id);
        return switch (def.value) {
            .fn_ => |fn_| blk: {
                const lowered_args = try self.lowerParamSpan(fn_.args);
                const captures = try self.lowerCaptureSlotRoots(fn_.captures);
                const expected_ret = try self.expectedReturnFromExpr(fn_.body);
                const ret = try self.newValue(expected_ret.ty, expected_ret.source_ty);
                try self.publishProcedureReturnRootKind(ret);
                break :blk .{
                    .params = lowered_args.values,
                    .ret = ret,
                    .captures = captures,
                    .function_root = self.representation_store.reserveRoot(),
                };
            },
            .hosted_fn => |hosted| blk: {
                const lowered_args = try self.lowerParamSpan(hosted.args);
                const ret_ty = try self.type_importer.importType(hosted.ret_ty);
                const ret = try self.newValue(ret_ty, .{});
                try self.publishProcedureReturnRootKind(ret);
                break :blk .{
                    .params = lowered_args.values,
                    .ret = ret,
                    .captures = repr.Span(repr.ValueInfoId).empty(),
                    .function_root = self.representation_store.reserveRoot(),
                };
            },
            .val => |expr| blk: {
                const expected_ret = try self.expectedReturnFromExpr(expr);
                const ret = try self.newValue(expected_ret.ty, expected_ret.source_ty);
                try self.publishProcedureReturnRootKind(ret);
                break :blk .{
                    .params = repr.Span(repr.ValueInfoId).empty(),
                    .ret = ret,
                    .captures = repr.Span(repr.ValueInfoId).empty(),
                    .function_root = self.representation_store.reserveRoot(),
                };
            },
            .run => |run_def| blk: {
                const expected_ret = try self.expectedReturnFromExpr(run_def.body);
                const ret = try self.newValue(expected_ret.ty, expected_ret.source_ty);
                try self.publishProcedureReturnRootKind(ret);
                break :blk .{
                    .params = repr.Span(repr.ValueInfoId).empty(),
                    .ret = ret,
                    .captures = repr.Span(repr.ValueInfoId).empty(),
                    .function_root = self.representation_store.reserveRoot(),
                };
            },
        };
    }

    fn lowerExecutableSyntheticSignature(
        self: *BodySolver,
        synthetic: ids.ExecutableSyntheticProc,
    ) Allocator.Error!repr.ProcPublicValueRoots {
        const signature = synthetic.signature;
        const concrete = self.concrete_source_types.refForKey(signature.source_fn_ty) orelse {
            lambdaInvariant("lambda-solved executable synthetic signature source function type has no concrete payload");
        };
        const fn_ty = try self.type_importer.importConcreteRef(concrete);
        const fn_root = self.type_importer.output.unlinkConst(fn_ty);
        const func = switch (self.type_importer.output.getNode(fn_root)) {
            .content => |content| switch (content) {
                .func => |func| func,
                else => lambdaInvariant("lambda-solved executable synthetic signature source type was not a function"),
            },
            else => lambdaInvariant("lambda-solved executable synthetic signature source type was not a function"),
        };
        const arg_tys = self.type_importer.output.sliceTypeVarSpan(func.args);
        if (arg_tys.len != signature.params.len) {
            lambdaInvariant("lambda-solved executable synthetic signature param arity disagrees with source function type");
        }

        const param_values: []repr.ValueInfoId = if (arg_tys.len == 0)
            &.{}
        else
            try self.allocator.alloc(repr.ValueInfoId, arg_tys.len);
        defer if (param_values.len > 0) self.allocator.free(param_values);
        const erased = switch (synthetic.body) {
            .erased_promoted_wrapper => |erased| erased,
        };
        const exec_signature = erased.executable_signature;
        const provenance = repr.BoxErasureProvenance{ .promoted_wrapper = synthetic.source_proc };
        for (arg_tys, signature.params, 0..) |arg_ty, param, i| {
            param_values[i] = try self.newValue(arg_ty, param.source_ty);
            try self.publishProcedureParamRootKind(param_values[i], @intCast(i));
            try self.attachExecutableSyntheticAlreadyErasedCallable(
                synthetic,
                param_values[i],
                exec_signature.wrapper_params[i].exec_ty,
                exec_signature.wrapper_params[i].exec_ty_key,
                provenance,
            );
        }
        const ret = try self.newValue(func.ret, signature.ret_source_ty);
        try self.publishProcedureReturnRootKind(ret);
        try self.attachExecutableSyntheticAlreadyErasedCallable(
            synthetic,
            ret,
            exec_signature.wrapper_ret,
            exec_signature.wrapper_ret_key,
            provenance,
        );

        try self.appendExecutableSyntheticErasureRequirements(synthetic, param_values, ret);

        return .{
            .params = try self.value_store.addValueSpan(param_values),
            .ret = ret,
            .captures = repr.Span(repr.ValueInfoId).empty(),
            .function_root = self.representation_store.reserveRoot(),
        };
    }

    fn attachExecutableSyntheticAlreadyErasedCallable(
        self: *BodySolver,
        synthetic: ids.ExecutableSyntheticProc,
        value: repr.ValueInfoId,
        exec_ty: checked_artifact.ExecutableTypePayloadRef,
        exec_ty_key: canonical.CanonicalExecValueTypeKey,
        provenance: repr.BoxErasureProvenance,
    ) Allocator.Error!void {
        if (!std.mem.eql(u8, &exec_ty.artifact.bytes, &synthetic.artifact.bytes)) {
            lambdaInvariant("lambda-solved executable synthetic erased payload ref points at a different artifact");
        }
        const payload_key = synthetic.executable_type_payloads.keyFor(exec_ty.payload);
        if (!repr.canonicalExecValueTypeKeyEql(payload_key, exec_ty_key)) {
            lambdaInvariant("lambda-solved executable synthetic erased payload key differs from signature key");
        }
        const erased = switch (synthetic.executable_type_payloads.get(exec_ty.payload)) {
            .erased_fn => |erased| erased,
            else => return,
        };

        const provenance_items = [_]repr.BoxErasureProvenance{provenance};
        const plan = repr.AlreadyErasedCallablePlan{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .result_ty = exec_ty_key,
            .capture = .none,
            .provenance = &provenance_items,
        };
        const emission = try self.representation_store.appendAlreadyErasedCallableEmissionPlan(plan);
        const value_info = &self.value_store.values.items[@intFromEnum(value)];
        if (!self.valueHasFunctionType(value_info.logical_ty)) {
            lambdaInvariant("lambda-solved executable synthetic erased payload was attached to a non-function value");
        }
        if (value_info.callable != null) {
            lambdaInvariant("lambda-solved executable synthetic value already had callable metadata");
        }
        value_info.callable = .{
            .whole_function_root = value_info.root,
            .callable_root = self.structuralChildRoot(value_info.root, .function_callable),
            .source = .{ .already_erased = .{
                .sig_key = erased.sig_key,
                .capture_shape_key = erased.capture_shape_key,
                .result_ty = exec_ty_key,
                .capture = .none,
                .provenance = &.{},
            } },
            .emission_plan = emission,
            .construction_plan = null,
        };
    }

    fn valueHasFunctionType(
        self: *const BodySolver,
        ty: Type.TypeVarId,
    ) bool {
        const root = self.type_importer.output.unlinkConst(ty);
        return switch (self.type_importer.output.getNode(root)) {
            .content => |content| switch (content) {
                .func => true,
                else => false,
            },
            else => false,
        };
    }

    fn appendExecutableSyntheticErasureRequirements(
        self: *BodySolver,
        synthetic: ids.ExecutableSyntheticProc,
        param_values: []const repr.ValueInfoId,
        ret: repr.ValueInfoId,
    ) Allocator.Error!void {
        const erased = switch (synthetic.body) {
            .erased_promoted_wrapper => |erased| erased,
        };
        const exec_signature = erased.executable_signature;
        if (exec_signature.wrapper_params.len != param_values.len) {
            lambdaInvariant("lambda-solved executable synthetic erased wrapper param arity differs from bodyless signature");
        }
        const provenance = repr.BoxErasureProvenance{ .promoted_wrapper = synthetic.source_proc };
        for (exec_signature.wrapper_params, param_values) |param, value| {
            if (!try self.executableSyntheticPayloadContainsErasedFn(synthetic, param.exec_ty)) continue;
            _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                .payload_root = self.valueRoot(value),
                .provenance = provenance,
            } });
        }
        if (try self.executableSyntheticPayloadContainsErasedFn(synthetic, exec_signature.wrapper_ret)) {
            _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                .payload_root = self.valueRoot(ret),
                .provenance = provenance,
            } });
        }
    }

    fn appendExecutableDependencyRequirements(
        self: *BodySolver,
        owner: ProcedureInstanceOwner,
        roots: repr.ProcPublicValueRoots,
    ) Allocator.Error!void {
        switch (owner) {
            .executable_erased_adapter_member => |member| {
                const synthetic = self.registry.input.executable_synthetic_procs.items[member.synthetic_index];
                const erased = switch (synthetic.body) {
                    .erased_promoted_wrapper => |erased| erased,
                };
                const finite = switch (erased.sealed.code) {
                    .direct_proc => lambdaInvariant("lambda-solved executable adapter member requirement reached direct erased code"),
                    .finite_adapter => |finite| finite,
                };
                if (member.member_index >= finite.members.len) {
                    lambdaInvariant("lambda-solved executable adapter member requirement index is out of range");
                }
                const target_key = finite.members[member.member_index].target_key;
                const params = self.value_store.sliceValueSpan(roots.params);
                if (params.len != target_key.exec_arg_tys.len) {
                    lambdaInvariant("lambda-solved executable adapter member target key arity differs from public roots");
                }
                const provenance = repr.BoxErasureProvenance{ .promoted_wrapper = synthetic.source_proc };
                for (params, target_key.exec_arg_tys) |param, exec_key| {
                    if (!try self.executableSyntheticTypeKeyContainsErasedFn(synthetic, exec_key)) continue;
                    _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                        .payload_root = self.valueRoot(param),
                        .provenance = provenance,
                    } });
                }
                if (try self.executableSyntheticTypeKeyContainsErasedFn(synthetic, target_key.exec_ret_ty)) {
                    _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                        .payload_root = self.valueRoot(roots.ret),
                        .provenance = provenance,
                    } });
                }
            },
            .sealed_erased_adapter_member => |member| {
                if (member >= self.registry.sealed_erased_adapter_members.items.len) {
                    lambdaInvariant("lambda-solved sealed adapter member requirement index is out of range");
                }
                const dependency = self.registry.sealed_erased_adapter_members.items[member];
                const target_key = dependency.target_key;
                const params = self.value_store.sliceValueSpan(roots.params);
                if (params.len != target_key.exec_arg_tys.len) {
                    lambdaInvariant("lambda-solved sealed adapter member target key arity differs from public roots");
                }
                for (params, target_key.exec_arg_tys) |param, exec_key| {
                    if (!try self.executableArtifactTypeKeyContainsErasedFn(dependency.payloads, dependency.artifact, exec_key)) continue;
                    for (dependency.provenance) |provenance| {
                        _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                            .payload_root = self.valueRoot(param),
                            .provenance = provenance,
                        } });
                    }
                }
                if (try self.executableArtifactTypeKeyContainsErasedFn(dependency.payloads, dependency.artifact, target_key.exec_ret_ty)) {
                    for (dependency.provenance) |provenance| {
                        _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                            .payload_root = self.valueRoot(roots.ret),
                            .provenance = provenance,
                        } });
                    }
                }
            },
            .finite_erased_adapter_member => |member| {
                const plan = self.representation_store.callableEmissionPlan(member.emission_plan);
                const erase = switch (plan) {
                    .erase_finite_set => |erase| erase,
                    else => lambdaInvariant("lambda-solved finite erased adapter member owner referenced non-erased emission plan"),
                };
                if (member.member_index >= erase.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter member requirement index is out of range");
                }
                const target_key = erase.member_targets[member.member_index];
                const params = self.value_store.sliceValueSpan(roots.params);
                if (params.len != target_key.exec_arg_tys.len) {
                    lambdaInvariant("lambda-solved finite erased adapter member target key arity differs from public roots");
                }
                for (params, target_key.exec_arg_tys) |param, exec_key| {
                    if (!try self.executableTypeKeyContainsErasedFn(exec_key)) continue;
                    for (erase.provenance) |provenance| {
                        _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                            .payload_root = self.valueRoot(param),
                            .provenance = provenance,
                        } });
                    }
                }
                if (try self.executableTypeKeyContainsErasedFn(target_key.exec_ret_ty)) {
                    for (erase.provenance) |provenance| {
                        _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                            .payload_root = self.valueRoot(roots.ret),
                            .provenance = provenance,
                        } });
                    }
                }
            },
            .finite_erased_adapter_demand_member => |member| {
                const demand = self.representation_store.finiteErasedAdapterDemand(member.demand);
                if (member.member_index >= demand.member_targets.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand member requirement index is out of range");
                }
                const target_key = demand.member_targets[member.member_index];
                const params = self.value_store.sliceValueSpan(roots.params);
                if (params.len != target_key.exec_arg_tys.len) {
                    lambdaInvariant("lambda-solved finite erased adapter demand member target key arity differs from public roots");
                }
                for (params, target_key.exec_arg_tys) |param, exec_key| {
                    if (!try self.executableTypeKeyContainsErasedFn(exec_key)) continue;
                    for (demand.provenance) |provenance| {
                        _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                            .payload_root = self.valueRoot(param),
                            .provenance = provenance,
                        } });
                    }
                }
                if (try self.executableTypeKeyContainsErasedFn(target_key.exec_ret_ty)) {
                    for (demand.provenance) |provenance| {
                        _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                            .payload_root = self.valueRoot(roots.ret),
                            .provenance = provenance,
                        } });
                    }
                }
            },
            .root,
            .direct_call,
            .proc_value,
            .recursive_group_member,
            => {},
        }
    }

    fn executableTypeKeyContainsErasedFn(
        self: *BodySolver,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!bool {
        const ref = self.representation_store.session_executable_type_payloads.refForKey(key) orelse {
            lambdaInvariant("lambda-solved finite erased adapter member target key has no executable payload");
        };
        var visited = std.AutoHashMap(repr.SessionExecutableTypePayloadId, void).init(self.allocator);
        defer visited.deinit();
        return try self.executablePayloadContainsErasedFn(ref.payload, &visited);
    }

    fn executablePayloadContainsErasedFn(
        self: *BodySolver,
        payload_id: repr.SessionExecutableTypePayloadId,
        visited: *std.AutoHashMap(repr.SessionExecutableTypePayloadId, void),
    ) Allocator.Error!bool {
        if (visited.contains(payload_id)) return false;
        try visited.put(payload_id, {});
        const payload = self.representation_store.session_executable_type_payloads.get(payload_id);
        switch (payload) {
            .erased_fn => return true,
            .record => |record| {
                for (record.fields) |field| {
                    if (try self.executablePayloadContainsErasedFn(field.ty.payload, visited)) return true;
                }
                return false;
            },
            .tuple => |items| {
                for (items) |item| {
                    if (try self.executablePayloadContainsErasedFn(item.ty.payload, visited)) return true;
                }
                return false;
            },
            .tag_union => |tag_union| {
                for (tag_union.variants) |variant| {
                    for (variant.payloads) |tag_payload| {
                        if (try self.executablePayloadContainsErasedFn(tag_payload.ty.payload, visited)) return true;
                    }
                }
                return false;
            },
            .list => |list| return try self.executablePayloadContainsErasedFn(list.ty.payload, visited),
            .box => |box| return try self.executablePayloadContainsErasedFn(box.ty.payload, visited),
            .nominal => |nominal| return try self.executablePayloadContainsErasedFn(nominal.backing.payload, visited),
            .callable_set => |callable_set| {
                for (callable_set.members) |member| {
                    const member_payload = member.payload_ty orelse continue;
                    if (try self.executablePayloadContainsErasedFn(member_payload.payload, visited)) return true;
                }
                return false;
            },
            .recursive_ref => |recursive| return try self.executablePayloadContainsErasedFn(recursive, visited),
            .pending => lambdaInvariant("lambda-solved finite erased adapter member target referenced pending executable payload"),
            .primitive,
            .vacant_callable_slot,
            => return false,
        }
    }

    fn executableSyntheticTypeKeyContainsErasedFn(
        self: *BodySolver,
        synthetic: ids.ExecutableSyntheticProc,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!bool {
        const ref = synthetic.executable_type_payloads.refForKey(.{ .bytes = synthetic.artifact.bytes }, key) orelse {
            lambdaInvariant("lambda-solved executable synthetic erased requirement referenced missing executable payload key");
        };
        return try self.executableSyntheticPayloadContainsErasedFn(synthetic, ref);
    }

    fn executableArtifactTypeKeyContainsErasedFn(
        self: *BodySolver,
        payloads: *const checked_artifact.ExecutableTypePayloadStore,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!bool {
        const ref = payloads.refForKey(.{ .bytes = artifact.bytes }, key) orelse {
            lambdaInvariant("lambda-solved sealed adapter member erased requirement referenced missing executable payload key");
        };
        if (!std.meta.eql(ref.artifact.bytes, artifact.bytes)) {
            lambdaInvariant("lambda-solved sealed adapter member executable payload ref points at another artifact");
        }
        var visited = std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, void).init(self.allocator);
        defer visited.deinit();
        return try self.executableArtifactPayloadContainsErasedFn(payloads, ref.payload, &visited);
    }

    fn executableSyntheticPayloadContainsErasedFn(
        self: *BodySolver,
        synthetic: ids.ExecutableSyntheticProc,
        ref: checked_artifact.ExecutableTypePayloadRef,
    ) Allocator.Error!bool {
        if (!std.mem.eql(u8, &ref.artifact.bytes, &synthetic.artifact.bytes)) {
            lambdaInvariant("lambda-solved executable synthetic erased requirement payload ref points at another artifact");
        }
        var visited = std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, void).init(self.allocator);
        defer visited.deinit();
        return try self.executableArtifactPayloadContainsErasedFn(synthetic.executable_type_payloads, ref.payload, &visited);
    }

    fn executableArtifactPayloadContainsErasedFn(
        self: *BodySolver,
        payloads: *const checked_artifact.ExecutableTypePayloadStore,
        payload_id: checked_artifact.ExecutableTypePayloadId,
        visited: *std.AutoHashMap(checked_artifact.ExecutableTypePayloadId, void),
    ) Allocator.Error!bool {
        if (visited.contains(payload_id)) return false;
        try visited.put(payload_id, {});
        const payload = payloads.get(payload_id);
        return switch (payload) {
            .erased_fn => true,
            .record => |record| {
                for (record) |field| {
                    if (try self.executableArtifactPayloadContainsErasedFn(payloads, field.ty.payload, visited)) return true;
                }
                return false;
            },
            .tuple => |items| {
                for (items) |item| {
                    if (try self.executableArtifactPayloadContainsErasedFn(payloads, item.ty.payload, visited)) return true;
                }
                return false;
            },
            .tag_union => |tag_union| {
                for (tag_union) |variant| {
                    for (variant.payloads) |tag_payload| {
                        if (try self.executableArtifactPayloadContainsErasedFn(payloads, tag_payload.ty.payload, visited)) return true;
                    }
                }
                return false;
            },
            .list => |list| return try self.executableArtifactPayloadContainsErasedFn(payloads, list.ty.payload, visited),
            .box => |box| return try self.executableArtifactPayloadContainsErasedFn(payloads, box.ty.payload, visited),
            .nominal => |nominal| return try self.executableArtifactPayloadContainsErasedFn(payloads, nominal.backing.payload, visited),
            .callable_set => |callable_set| {
                for (callable_set.members) |member| {
                    const member_payload = member.payload_ty orelse continue;
                    if (try self.executableArtifactPayloadContainsErasedFn(payloads, member_payload.payload, visited)) return true;
                }
                return false;
            },
            .recursive_ref => |recursive| return try self.executableArtifactPayloadContainsErasedFn(payloads, recursive, visited),
            .pending => lambdaInvariant("lambda-solved executable synthetic erased requirement reached pending executable payload"),
            .primitive,
            .vacant_callable_slot,
            => false,
        };
    }

    fn expectedReturnFromExpr(
        self: *BodySolver,
        expr_id: Lifted.Ast.ExprId,
    ) Allocator.Error!ExpectedReturn {
        const expr = self.input.getExpr(expr_id);
        return .{
            .ty = try self.type_importer.importType(expr.ty),
            .source_ty = expr.source_ty,
        };
    }

    fn wrapImplicitReturn(
        self: *BodySolver,
        body: Ast.ExprId,
        expected_ret: ExpectedReturn,
        public_ret: repr.ValueInfoId,
    ) Allocator.Error!Ast.ExprId {
        const return_info = try self.addReturnInfo(self.exprValue(body));
        return try self.output.addExpr(
            expected_ret.ty,
            expected_ret.source_ty,
            public_ret,
            .{ .return_ = .{
                .expr = body,
                .return_info = return_info,
            } },
        );
    }

    fn addReturnInfo(
        self: *BodySolver,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.ReturnInfoId {
        const public_ret = self.active_return_value orelse {
            lambdaInvariant("lambda-solved return lowering had no active procedure return root");
        };
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = self.valueRoot(value) },
            .to = .{ .local = self.valueRoot(public_ret) },
            .kind = .value_move,
        });
        try self.propagatePendingComptimeDependencyOrigin(value, public_ret);
        return try self.value_store.addReturn(.{
            .value = value,
        });
    }

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
            try self.publishProcedureParamRootKind(value, @intCast(i));
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

    fn lowerParamSpanWithExistingValues(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.TypedSymbol),
        existing_values: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!LoweredParams {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        const values = self.value_store.sliceValueSpan(existing_values);
        if (input_items.len != values.len) {
            lambdaInvariant("lambda-solved materialized procedure param arity differs from descriptor roots");
        }
        if (input_items.len == 0) return .{
            .symbols = Ast.Span(Ast.TypedSymbol).empty(),
            .values = existing_values,
        };
        const symbols = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(symbols);
        for (input_items, values, 0..) |param, value, i| {
            const ty = try self.type_importer.importType(param.ty);
            try self.publishProcedureParamRootKind(value, @intCast(i));
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
        }
        return .{
            .symbols = try self.output.addTypedSymbolSpan(symbols),
            .values = existing_values,
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
            try self.publishProcedureCaptureRootKind(value, @intCast(i));
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

    fn lowerCaptureSlotRootsWithExistingValues(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.CaptureSlot),
        existing_values: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!repr.Span(repr.ValueInfoId) {
        const input_items = self.input.sliceCaptureSlotSpan(span);
        const values = self.value_store.sliceValueSpan(existing_values);
        if (input_items.len != values.len) {
            lambdaInvariant("lambda-solved materialized procedure capture arity differs from descriptor roots");
        }
        for (input_items, values, 0..) |slot, value, i| {
            try self.publishProcedureCaptureRootKind(value, @intCast(i));
            const binding = try self.value_store.addBinding(.{
                .symbol = slot.source_symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(slot.source_symbol, binding);
        }
        return existing_values;
    }

    fn exprCanUseExprMap(expr: Lifted.Ast.Expr) bool {
        return switch (expr.data) {
            .capture_ref,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .bool_lit,
            .str_lit,
            .const_instance,
            .const_ref,
            .pending_callable_instance,
            .pending_local_root,
            .unit,
            => true,

            else => false,
        };
    }

    fn lowerExpr(self: *BodySolver, expr_id: Lifted.Ast.ExprId) Allocator.Error!Ast.ExprId {
        const expr = self.input.getExpr(expr_id);
        if (expr.data == .low_level) {
            return try self.lowerLowLevelSubgraph(expr_id);
        }
        const can_use_expr_map = exprCanUseExprMap(expr);
        if (can_use_expr_map) {
            if (self.expr_map.get(expr_id)) |existing| return existing;
        }

        const ty = try self.type_importer.importType(expr.ty);
        switch (expr.data) {
            .var_ => |symbol| {
                const binding_info = self.env.get(symbol) orelse {
                    const entry = self.symbols.get(symbol);
                    const record = self.registry.procRecord(self.instance);
                    lambdaInvariantFmt(
                        "lambda-solved variable occurrence has no published binding info for symbol {d} ({s}) in instance {d} owner {s} proc_base={d} callable={s}",
                        .{
                            @intFromEnum(symbol),
                            @tagName(entry.origin),
                            @intFromEnum(self.instance),
                            @tagName(record.owner),
                            @intFromEnum(record.proc.proc.proc_base),
                            @tagName(record.proc.callable.template),
                        },
                    );
                };
                const binding = self.value_store.bindings.items[@intFromEnum(binding_info)];
                const value = try self.newValue(ty, expr.source_ty);
                try self.publishValueAlias(binding.value, value);
                self.value_store.values.items[@intFromEnum(value)].value_alias_needs_executable_transform = true;
                const lowered = try self.output.addExpr(ty, expr.source_ty, value, .{ .var_ = .{
                    .symbol = symbol,
                    .binding_info = binding_info,
                } });
                if (can_use_expr_map) {
                    try self.expr_map.put(expr_id, lowered);
                }
                return lowered;
            },
            .capture_ref => |slot| {
                const captures_span = self.active_captures orelse lambdaInvariant("lambda-solved capture_ref reached a procedure without capture roots");
                const captures = self.value_store.sliceValueSpan(captures_span);
                const capture_index: usize = @intCast(slot);
                if (capture_index >= captures.len) lambdaInvariant("lambda-solved capture_ref slot does not exist in procedure capture roots");
                const source = captures[capture_index];
                const value = try self.newValue(ty, expr.source_ty);
                try self.publishValueAlias(source, value);
                const lowered = try self.output.addExpr(ty, expr.source_ty, value, .{ .capture_ref = slot });
                if (can_use_expr_map) {
                    try self.expr_map.put(expr_id, lowered);
                }
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
                if (can_use_expr_map) {
                    try self.expr_map.put(expr_id, lowered);
                }
                return lowered;
            },
            .block => |block| {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = try self.lowerExpr(block.final_expr);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(final_expr), .{ .block = .{
                    .stmts = stmts,
                    .final_expr = final_expr,
                } });
                if (can_use_expr_map) {
                    try self.expr_map.put(expr_id, lowered);
                }
                return lowered;
            },
            else => {},
        }
        switch (expr.data) {
            .access => |access| {
                const record = try self.lowerExpr(access.record);
                const source = self.exprValue(record);
                if (self.aggregateForProjection(source)) |aggregate| {
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
                    if (can_use_expr_map) {
                        try self.expr_map.put(expr_id, lowered);
                    }
                    return lowered;
                }
                if (try self.publishConstBackedProjectionValue(source, ty, expr.source_ty, .{ .record_field = access.field })) |projected| {
                    const lowered = try self.output.addExpr(ty, expr.source_ty, projected.value, .{ .access = .{
                        .record = record,
                        .field = access.field,
                        .projection_info = projected.projection,
                    } });
                    if (can_use_expr_map) {
                        try self.expr_map.put(expr_id, lowered);
                    }
                    return lowered;
                }
            },
            .tuple_access => |access| {
                const tuple = try self.lowerExpr(access.tuple);
                const source = self.exprValue(tuple);
                if (self.aggregateForProjection(source)) |aggregate| {
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
                    if (can_use_expr_map) {
                        try self.expr_map.put(expr_id, lowered);
                    }
                    return lowered;
                }
                if (try self.publishConstBackedProjectionValue(source, ty, expr.source_ty, .{ .tuple_elem = access.elem_index })) |projected| {
                    const lowered = try self.output.addExpr(ty, expr.source_ty, projected.value, .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                        .projection_info = projected.projection,
                    } });
                    if (can_use_expr_map) {
                        try self.expr_map.put(expr_id, lowered);
                    }
                    return lowered;
                }
            },
            .tag_payload => |payload| {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const source = self.exprValue(tag_union);
                if (self.aggregateForProjection(source)) |aggregate| {
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
                    if (can_use_expr_map) {
                        try self.expr_map.put(expr_id, lowered);
                    }
                    return lowered;
                }
                if (try self.publishConstBackedProjectionValue(source, ty, expr.source_ty, .{ .tag_payload = payload.payload })) |projected| {
                    const lowered = try self.output.addExpr(ty, expr.source_ty, projected.value, .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                        .projection_info = projected.projection,
                    } });
                    if (can_use_expr_map) {
                        try self.expr_map.put(expr_id, lowered);
                    }
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
            .const_instance => |const_instance| blk: {
                try self.publishConstBackedValueMetadata(value, self.constBackedRoot(const_instance));
                break :blk .{ .const_instance = const_instance };
            },
            .const_ref => |key| blk: {
                self.value_store.values.items[@intFromEnum(value)].pending_comptime_dependency_origin = true;
                break :blk .{ .const_ref = key };
            },
            .pending_callable_instance => |key| blk: {
                self.value_store.values.items[@intFromEnum(value)].pending_comptime_dependency_origin = true;
                break :blk .{ .pending_callable_instance = key };
            },
            .pending_local_root => |root| blk: {
                self.value_store.values.items[@intFromEnum(value)].pending_comptime_dependency_origin = true;
                break :blk .{ .pending_local_root = root };
            },
            .tag => |tag| blk: {
                const eval_order = try self.lowerTagPayloadEvalSpan(tag.eval_order);
                const assembly_order = try self.lowerTagPayloadAssemblySpan(tag.assembly_order);
                try self.publishTagAggregate(value, tag.union_shape, tag.tag, eval_order, assembly_order);
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
                try self.publishRecordAggregate(value, record.shape, eval_order, assembly_order);
                break :blk .{ .record = .{
                    .shape = record.shape,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                } };
            },
            .nominal_reinterpret => |backing| blk: {
                const lowered_backing = try self.lowerExpr(backing);
                try self.publishNominalBackingValue(value, self.exprValue(lowered_backing), ty);
                break :blk .{ .nominal_reinterpret = lowered_backing };
            },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                const projection = try self.publishProjectionInfo(self.exprValue(record), value, .{ .record_field = access.field });
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
                const requested_fn_root = self.representation_store.reserveRoot();
                const call_site = try self.value_store.addCallSite(.{
                    .callee = callee_value,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = requested_fn_root,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .dispatch = null,
                    .source_match_branch = self.active_source_match_branch,
                });
                if (self.value_store.values.items[@intFromEnum(callee_value)].pending_comptime_dependency_origin) {
                    self.value_store.call_sites.items[@intFromEnum(call_site)].dispatch = .pending_comptime_dependency_call;
                    self.value_store.values.items[@intFromEnum(value)].pending_comptime_dependency_origin = true;
                } else {
                    try self.publishCallValueRequestedFunctionEdges(
                        call_site,
                        callee_value,
                        lowered_args.values,
                        value,
                        requested_fn_root,
                        requested_fn_ty,
                        call.requested_source_fn_ty,
                    );
                }
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
                const requested_fn_root = self.representation_store.reserveRoot();
                const call_site = try self.value_store.addCallSite(.{
                    .callee = null,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = requested_fn_root,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .dispatch = null,
                    .source_match_branch = self.active_source_match_branch,
                });
                const target_instance = try self.registry.reserveDirectCall(self.instance, call_site, call.proc);
                self.refreshValueStore();
                self.value_store.call_sites.items[@intFromEnum(call_site)].dispatch = .{ .call_proc = target_instance };
                try self.publishCallProcRequestedFunctionEdges(
                    call_site,
                    lowered_args.values,
                    value,
                    requested_fn_root,
                    requested_fn_ty,
                    call.requested_source_fn_ty,
                );
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
                const fn_ty = try self.type_importer.importType(proc_value.fn_ty);
                const whole_function_root = self.representation_store.reserveRoot();
                const target_instance = try self.registry.reserveProcValue(self.instance, value, proc_value.proc, proc_value.forced_target);
                self.refreshValueStore();
                try self.representation_store.publishRootKind(whole_function_root, .{ .proc_value_fn = .{
                    .instance = self.instance,
                    .value = value,
                } });
                try self.publishFunctionRootShape(whole_function_root, fn_ty, proc_value.proc.callable.source_fn_ty);
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(value) },
                    .to = .{ .local = whole_function_root },
                    .kind = .value_alias,
                });
                const callable = try self.representation_store.addSingletonProcValueCallable(
                    value,
                    whole_function_root,
                    proc_value.proc,
                    proc_value.published_proc,
                    target_instance,
                    self.value_store.sliceValueSpan(captures.values),
                    self.sourcePayloadForKey(proc_value.proc.callable.source_fn_ty) orelse {
                        lambdaInvariant("lambda-solved proc-value source function type has no concrete payload");
                    },
                );
                self.value_store.values.items[@intFromEnum(value)].callable = callable;
                break :blk .{ .proc_value = .{
                    .proc = proc_value.proc,
                    .published_proc = proc_value.published_proc,
                    .captures = captures.args,
                    .fn_ty = fn_ty,
                    .forced_target = try ids.cloneProcValueExecutableTargetOptional(self.allocator, proc_value.forced_target),
                } };
            },
            .low_level => unreachable,
            .block => unreachable,
            .tuple => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishTupleAggregate(value, lowered_items.values);
                break :blk .{ .tuple = lowered_items.exprs };
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const projection = try self.publishProjectionInfo(self.exprValue(tag_union), value, .{ .tag_payload = payload.payload });
                break :blk .{ .tag_payload = .{
                    .tag_union = tag_union,
                    .payload = payload.payload,
                    .projection_info = projection,
                } };
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                const projection = try self.publishProjectionInfo(self.exprValue(tuple), value, .{ .tuple_elem = access.elem_index });
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
                const return_info = try self.addReturnInfo(self.exprValue(lowered_child));
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
                const lowered_branches = try self.lowerSourceMatchBranchSpan(match_id, self.exprValue(cond), match_.branches);
                const branch_inputs = try self.joinInputsForBranches(match_id, lowered_branches);
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = branch_inputs,
                    .root = self.valueRoot(value),
                    .kind = .match_expr,
                });
                try self.publishJoinResult(value, join_info);
                try self.publishJoinRepresentationEdges(value, branch_inputs);
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
                if (self.exprCanCompleteNormally(then_body)) {
                    try inputs.append(self.allocator, .{
                        .source = .{ .if_branch = .{
                            .if_expr = if_expr_id,
                            .branch = .then_,
                        } },
                        .value = self.exprValue(then_body),
                    });
                }
                if (self.exprCanCompleteNormally(else_body)) {
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
                try self.publishJoinResult(value, join_info);
                try self.publishJoinRepresentationEdges(value, self.value_store.joins.items[@intFromEnum(join_info)].inputs);
                break :blk .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                    .join_info = join_info,
                } };
            },
            .for_ => |for_| try self.lowerForExpr(value, for_),
        });
        if (can_use_expr_map) {
            try self.expr_map.put(expr_id, lowered);
        }
        return lowered;
    }

    const SavedBinding = struct {
        symbol: Ast.Symbol,
        previous: ?repr.BindingInfoId,
    };

    const LoweredFor = struct {
        patt: Ast.PatId,
        iterable: Ast.ExprId,
        body: Ast.ExprId,
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

    fn lowerSourceMatchBranch(
        self: *BodySolver,
        scrutinee: repr.ValueInfoId,
        branch_id: Lifted.Ast.BranchId,
        branch_ref: repr.SourceMatchBranchRef,
    ) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
        _ = try self.value_store.addSourceMatchBranchReachability(branch_ref);
        const previous_source_match_branch = self.active_source_match_branch;
        self.active_source_match_branch = branch_ref;
        defer self.active_source_match_branch = previous_source_match_branch;

        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const pat = try self.lowerPatScoped(branch.pat, &saved);
        try self.publishSourceMatchPatternRepresentationEdges(scrutinee, pat);
        defer self.restoreBindings(&saved, 0);
        const guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null;
        const body = try self.lowerExpr(branch.body);
        return try self.output.addBranch(.{
            .pat = pat,
            .guard = guard,
            .body = body,
            .degenerate = branch.degenerate,
            .source_match_branch = branch_ref,
        });
    }

    fn lowerForExpr(self: *BodySolver, _: repr.ValueInfoId, for_: anytype) Allocator.Error!Ast.Expr.Data {
        const lowered = try self.lowerForParts(for_);
        return .{ .for_ = .{
            .patt = lowered.patt,
            .iterable = lowered.iterable,
            .body = lowered.body,
        } };
    }

    fn lowerForParts(self: *BodySolver, for_: anytype) Allocator.Error!LoweredFor {
        const iterable = try self.lowerExpr(for_.iterable);
        const iterable_value = self.exprValue(iterable);
        const iterable_info = self.value_store.values.items[@intFromEnum(iterable_value)];
        const iterable_parent_root = self.patternProjectionParentRoot(
            self.valueRoot(iterable_value),
            iterable_info.logical_ty,
        );
        const elem_root = self.structuralChildRoot(iterable_parent_root, .list_elem);

        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        try self.publishPatternRepresentationEdges(elem_root, patt);
        defer self.restoreBindings(&saved, 0);

        return .{
            .patt = patt,
            .iterable = iterable,
            .body = try self.lowerExpr(for_.body),
        };
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
                const return_info = try self.addReturnInfo(self.exprValue(lowered_child));
                break :blk .{ .return_ = .{
                    .expr = lowered_child,
                    .return_info = return_info,
                } };
            },
            .break_ => .break_,
            .for_ => |for_| blk: {
                const lowered = try self.lowerForParts(for_);
                break :blk .{ .for_ = .{
                    .patt = lowered.patt,
                    .iterable = lowered.iterable,
                    .body = lowered.body,
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

    const LowLevelLowerFrame = struct {
        expr: Lifted.Ast.ExprId,
        expanded: bool,
    };

    fn publishCallValueRequestedFunctionEdges(
        self: *BodySolver,
        call_site: repr.CallSiteInfoId,
        callee_value: repr.ValueInfoId,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        requested_fn_root: repr.RepRootId,
        requested_fn_ty: Type.TypeVarId,
        requested_source_fn_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!void {
        try self.representation_store.publishRootKind(requested_fn_root, .{ .call_value_requested_fn = .{
            .instance = self.instance,
            .call_site = call_site,
        } });
        try self.publishFunctionRootShape(requested_fn_root, requested_fn_ty, requested_source_fn_ty);
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = self.valueRoot(callee_value) },
            .to = .{ .local = requested_fn_root },
            .kind = .value_alias,
        });
        try self.publishRequestedFunctionArgAndReturnEdges(args, result, requested_fn_root);
    }

    fn publishCallProcRequestedFunctionEdges(
        self: *BodySolver,
        call_site: repr.CallSiteInfoId,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        requested_fn_root: repr.RepRootId,
        requested_fn_ty: Type.TypeVarId,
        requested_source_fn_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!void {
        try self.representation_store.publishRootKind(requested_fn_root, .{ .call_proc_requested_fn = .{
            .instance = self.instance,
            .call_site = call_site,
        } });
        try self.publishFunctionRootShape(requested_fn_root, requested_fn_ty, requested_source_fn_ty);
        try self.publishRequestedFunctionArgAndReturnEdges(args, result, requested_fn_root);
    }

    fn publishRequestedFunctionArgAndReturnEdges(
        self: *BodySolver,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        requested_fn_root: repr.RepRootId,
    ) Allocator.Error!void {
        for (self.value_store.sliceValueSpan(args), 0..) |arg, i| {
            const arg_root = self.structuralChildRoot(requested_fn_root, .{ .function_arg = @intCast(i) });
            _ = try self.representation_store.appendRepresentationEdge(.{
                .from = .{ .local = self.valueRoot(arg) },
                .to = .{ .local = arg_root },
                .kind = .{ .call_arg = @intCast(i) },
            });
        }
        const return_root = self.structuralChildRoot(requested_fn_root, .function_return);
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = return_root },
            .to = .{ .local = self.valueRoot(result) },
            .kind = .call_result,
        });
    }

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

    fn lowerLowLevelSubgraph(self: *BodySolver, root_expr: Lifted.Ast.ExprId) Allocator.Error!Ast.ExprId {
        var lowered = std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId).init(self.allocator);
        defer lowered.deinit();

        var stack = std.ArrayList(LowLevelLowerFrame).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .expr = root_expr, .expanded = false });

        while (stack.pop()) |frame| {
            if (lowered.contains(frame.expr)) continue;
            const input_expr = self.input.getExpr(frame.expr);
            switch (input_expr.data) {
                .low_level => |low_level| {
                    if (!frame.expanded) {
                        try stack.append(self.allocator, .{ .expr = frame.expr, .expanded = true });
                        const args = self.input.sliceExprSpan(low_level.args);
                        var i = args.len;
                        while (i > 0) {
                            i -= 1;
                            const arg_expr = args[i];
                            if (self.input.getExpr(arg_expr).data == .low_level and !lowered.contains(arg_expr)) {
                                try stack.append(self.allocator, .{ .expr = arg_expr, .expanded = false });
                            }
                        }
                        continue;
                    }

                    const ty = try self.type_importer.importType(input_expr.ty);
                    const value = try self.newValue(ty, input_expr.source_ty);
                    const lowered_args = try self.lowerLowLevelArgSpanWithValues(low_level.args, &lowered);
                    const data = try self.lowerLowLevelWithArgs(value, input_expr.source_ty, low_level, lowered_args);
                    const lowered_expr = try self.output.addExpr(ty, input_expr.source_ty, value, data);
                    try lowered.put(frame.expr, lowered_expr);
                },
                else => {
                    const lowered_expr = try self.lowerExpr(frame.expr);
                    try lowered.put(frame.expr, lowered_expr);
                },
            }
        }

        return lowered.get(root_expr) orelse lambdaInvariant("lambda-solved iterative low-level lowering did not publish root expression");
    }

    fn lowerLowLevelArgSpanWithValues(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.ExprId),
        lowered_low_levels: *const std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId),
    ) Allocator.Error!LoweredExprSpan {
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
            if (self.input.getExpr(expr).data == .low_level) {
                exprs[i] = lowered_low_levels.get(expr) orelse lambdaInvariant("lambda-solved iterative low-level lowering reached unlowered low-level argument");
            } else {
                exprs[i] = try self.lowerExpr(expr);
            }
            values[i] = self.exprValue(exprs[i]);
        }
        return .{
            .exprs = try self.output.addExprSpan(exprs),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerLowLevelWithArgs(
        self: *BodySolver,
        result_value: repr.ValueInfoId,
        result_source_ty: canonical.CanonicalTypeKey,
        low_level: anytype,
        lowered_args: LoweredExprSpan,
    ) Allocator.Error!Ast.Expr.Data {
        const source_constraint_ty = try self.type_importer.importType(low_level.source_constraint_ty);
        const arg_values = self.value_store.sliceValueSpan(lowered_args.values);
        var value_flow_edges = std.ArrayList(repr.LowLevelValueFlowEdge).empty;
        defer value_flow_edges.deinit(self.allocator);
        var box_boundary: ?repr.BoxBoundaryId = null;
        switch (low_level.op) {
            .box_box => {
                if (arg_values.len != 1) lambdaInvariant("lambda-solved Box.box reached non-unary low-level expression");
                const payload_value = arg_values[0];
                const payload_info = self.value_store.values.items[@intFromEnum(payload_value)];
                const result_root = self.valueRoot(result_value);
                const payload_root = self.valueRoot(payload_value);
                const boundary = try self.representation_store.appendBoxBoundary(self.allocator, .{
                    .box_ty = result_source_ty,
                    .box_ty_payload = self.sourcePayloadForKey(result_source_ty),
                    .payload_source_ty = payload_info.source_ty,
                    .payload_source_ty_payload = payload_info.source_ty_payload,
                    .payload_boundary_ty = payload_info.source_ty,
                    .payload_boundary_ty_payload = payload_info.source_ty_payload,
                    .direction = .box,
                    .source_root = payload_root,
                    .boundary_root = result_root,
                    .payload_plan = .unchanged,
                });
                box_boundary = boundary;
                _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                    .payload_root = payload_root,
                    .provenance = .{ .local_box_boundary = boundary },
                } });
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = result_root },
                    .to = .{ .local = payload_root },
                    .kind = .{ .child = .box_payload },
                });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = 0,
                    .arg_projection = .whole_value,
                    .result_projection = .box_payload,
                } });
                self.value_store.values.items[@intFromEnum(result_value)].boxed = .{
                    .box_root = result_root,
                    .payload_root = payload_root,
                    .payload_value = payload_value,
                    .boundary = boundary,
                };
            },
            .box_unbox => {
                if (arg_values.len != 1) lambdaInvariant("lambda-solved Box.unbox reached non-unary low-level expression");
                const boxed_value = arg_values[0];
                const boxed_info = self.value_store.values.items[@intFromEnum(boxed_value)];
                const boxed_source_ty = boxed_info.source_ty;
                const boundary = try self.representation_store.appendBoxBoundary(self.allocator, .{
                    .box_ty = boxed_source_ty,
                    .box_ty_payload = boxed_info.source_ty_payload,
                    .payload_source_ty = result_source_ty,
                    .payload_source_ty_payload = self.sourcePayloadForKey(result_source_ty),
                    .payload_boundary_ty = result_source_ty,
                    .payload_boundary_ty_payload = self.sourcePayloadForKey(result_source_ty),
                    .direction = .unbox,
                    .source_root = self.valueRoot(boxed_value),
                    .boundary_root = self.valueRoot(result_value),
                    .payload_plan = .unchanged,
                });
                box_boundary = boundary;
                _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                    .payload_root = self.valueRoot(result_value),
                    .provenance = .{ .local_box_boundary = boundary },
                } });
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(boxed_value) },
                    .to = .{ .local = self.valueRoot(result_value) },
                    .kind = .{ .child = .box_payload },
                });
                if (self.value_store.values.items[@intFromEnum(boxed_value)].boxed == null) {
                    self.value_store.values.items[@intFromEnum(boxed_value)].boxed = .{
                        .box_root = self.valueRoot(boxed_value),
                        .payload_root = self.valueRoot(result_value),
                        .payload_value = result_value,
                        .boundary = boundary,
                    };
                }
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = 0,
                    .arg_projection = .box_payload,
                    .result_projection = .whole_value,
                } });
            },
            .list_get_unsafe => {
                if (arg_values.len < 1) lambdaInvariant("lambda-solved list element low-level reached without a list argument");
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(arg_values[0]) },
                    .to = .{ .local = self.valueRoot(result_value) },
                    .kind = .{ .child = .list_elem },
                });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result = .{
                    .arg = 0,
                    .projection = .list_elem,
                } });
            },
            .list_first,
            .list_last,
            => {
                if (arg_values.len < 1) lambdaInvariant("lambda-solved list optional element low-level reached without a list argument");
                const ok_payload = try self.singlePayloadForTagLabel(result_value, "Ok");
                const ok_payload_root = self.structuralChildRoot(self.valueRoot(result_value), .{ .tag_payload = ok_payload });
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(arg_values[0]) },
                    .to = .{ .local = ok_payload_root },
                    .kind = .{ .child = .list_elem },
                });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = 0,
                    .arg_projection = .list_elem,
                    .result_projection = .{ .tag_payload = ok_payload },
                } });
            },
            .list_append_unsafe,
            .list_prepend,
            .list_set,
            => {
                if (arg_values.len < 2) lambdaInvariant("lambda-solved list update low-level reached without enough arguments");
                try self.publishValueAlias(arg_values[0], result_value);
                const elem_arg_index: usize = if (low_level.op == .list_set) 2 else 1;
                if (elem_arg_index >= arg_values.len) lambdaInvariant("lambda-solved list update low-level missing element argument");
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(result_value) },
                    .to = .{ .local = self.valueRoot(arg_values[elem_arg_index]) },
                    .kind = .{ .child = .list_elem },
                });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = 0,
                    .arg_projection = .list_elem,
                    .result_projection = .list_elem,
                } });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = @intCast(elem_arg_index),
                    .arg_projection = .whole_value,
                    .result_projection = .list_elem,
                } });
            },
            .list_concat => {
                if (arg_values.len != 2) lambdaInvariant("lambda-solved List.concat low-level reached without two list arguments");
                try self.publishValueAlias(arg_values[0], result_value);
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(arg_values[1]) },
                    .to = .{ .local = self.valueRoot(result_value) },
                    .kind = .value_alias,
                });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = 0,
                    .arg_projection = .list_elem,
                    .result_projection = .list_elem,
                } });
                try value_flow_edges.append(self.allocator, .{ .arg_to_result_projection = .{
                    .arg = 1,
                    .arg_projection = .list_elem,
                    .result_projection = .list_elem,
                } });
            },
            .list_sublist,
            .list_drop_at,
            .list_drop_first,
            .list_drop_last,
            .list_take_first,
            .list_take_last,
            .list_reverse,
            .list_reserve,
            .list_release_excess_capacity,
            => {
                if (arg_values.len < 1) lambdaInvariant("lambda-solved list-result low-level reached without a list argument");
                try self.publishValueAlias(arg_values[0], result_value);
                try self.appendLowLevelProducedFromArgs(&value_flow_edges, &.{0}, .list_elem);
            },
            .list_split_first,
            .list_split_last,
            => {
                if (arg_values.len < 1) lambdaInvariant("lambda-solved list split low-level reached without a list argument");
                try self.appendLowLevelProducedFromArgs(&value_flow_edges, &.{0}, .whole_value);
            },
            .list_with_capacity => {
                try value_flow_edges.append(self.allocator, .{ .fresh_result = .list_elem });
            },
            .str_concat,
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_drop_prefix,
            .str_drop_suffix,
            .str_reserve,
            .str_release_excess_capacity,
            .str_to_utf8,
            .str_from_utf8,
            .str_from_utf8_lossy,
            .str_split_on,
            .str_join_with,
            .str_repeat,
            .str_inspect,
            => {
                try self.appendLowLevelProducedFromArgs(&value_flow_edges, &.{0}, .whole_value);
            },
            .str_with_capacity,
            .u8_to_str,
            .i8_to_str,
            .u16_to_str,
            .i16_to_str,
            .u32_to_str,
            .i32_to_str,
            .u64_to_str,
            .i64_to_str,
            .u128_to_str,
            .i128_to_str,
            .dec_to_str,
            .f32_to_str,
            .f64_to_str,
            .num_to_str,
            => {
                try value_flow_edges.append(self.allocator, .{ .fresh_result = .whole_value });
            },
            else => {},
        }
        const value_flow = try self.publishLowLevelValueFlowSignature(
            low_level.op,
            lowered_args.values,
            result_value,
            value_flow_edges.items,
            box_boundary,
        );
        return .{ .low_level = .{
            .op = low_level.op,
            .rc_effect = low_level.rc_effect,
            .value_flow = value_flow,
            .args = lowered_args.exprs,
            .source_constraint_ty = source_constraint_ty,
        } };
    }

    fn publishLowLevelValueFlowSignature(
        self: *BodySolver,
        op: base.LowLevel,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        edges: []const repr.LowLevelValueFlowEdge,
        box_boundary: ?repr.BoxBoundaryId,
    ) Allocator.Error!repr.LowLevelValueFlowSignatureId {
        if (edges.len == 0 and box_boundary == null) {
            return try self.value_store.addLowLevelValueFlowSignature(.{ .no_value_flow = .{
                .op = op,
                .args = args,
                .result = result,
            } });
        }
        return try self.value_store.addLowLevelValueFlowSignature(.{ .flows = .{
            .op = op,
            .args = args,
            .result = result,
            .edges = try self.value_store.addLowLevelValueFlowEdgeSpan(edges),
            .box_boundary = box_boundary,
        } });
    }

    fn appendLowLevelProducedFromArgs(
        self: *BodySolver,
        edges: *std.ArrayList(repr.LowLevelValueFlowEdge),
        args: []const u32,
        result_projection: repr.LowLevelProjectionPath,
    ) Allocator.Error!void {
        try edges.append(self.allocator, .{ .produced_from_args = .{
            .args = try self.value_store.addLowLevelValueFlowArgIndexSpan(args),
            .result_projection = result_projection,
        } });
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

    fn lowerSourceMatchBranchSpan(
        self: *BodySolver,
        match_id: repr.SourceMatchId,
        scrutinee: repr.ValueInfoId,
        span: Lifted.Ast.Span(Lifted.Ast.BranchId),
    ) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            const branch_ref: repr.SourceMatchBranchRef = .{
                .match = match_id,
                .branch = @enumFromInt(@as(u32, @intCast(i))),
                .alternative = @enumFromInt(@as(u32, @intCast(i))),
            };
            output_items[i] = try self.lowerSourceMatchBranch(scrutinee, branch, branch_ref);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn publishSourceMatchPatternRepresentationEdges(
        self: *BodySolver,
        scrutinee: repr.ValueInfoId,
        pat_id: Ast.PatId,
    ) Allocator.Error!void {
        const scrutinee_root = self.valueRoot(scrutinee);
        const scrutinee_info = self.value_store.values.items[@intFromEnum(scrutinee)];
        const scrutinee_const_backing = self.constBackedForProjection(scrutinee);
        try self.publishPatternRepresentationEdges(scrutinee_root, pat_id);
        if (scrutinee_info.pending_comptime_dependency_origin) {
            self.markPatternPendingComptimeDependencyOrigin(pat_id);
        }
        if (scrutinee_const_backing) |backing| {
            try self.publishConstBackedPatternValues(backing, pat_id);
        }
    }

    fn publishPatternRepresentationEdges(
        self: *BodySolver,
        source_root: repr.RepRootId,
        pat_id: Ast.PatId,
    ) Allocator.Error!void {
        const pat = self.output.pats.items[@intFromEnum(pat_id)];
        const pat_root = self.valueRoot(pat.value_info);
        try self.publishRootAlias(source_root, pat_root);

        switch (pat.data) {
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .wildcard,
            .var_,
            => {},
            .as => |as| try self.publishPatternRepresentationEdges(source_root, as.pattern),
            .nominal => |child| {
                const backing_root = self.patternProjectionParentRoot(pat_root, pat.ty);
                try self.publishPatternRepresentationEdges(backing_root, child);
            },
            .tuple => |items| {
                const parent_root = self.patternProjectionParentRoot(pat_root, pat.ty);
                const child_ids = self.output.pat_ids.items[items.start..][0..items.len];
                for (child_ids, 0..) |child_id, i| {
                    const child_root = self.structuralChildRoot(parent_root, .{ .tuple_elem = @intCast(i) });
                    try self.publishPatternRepresentationEdges(child_root, child_id);
                }
            },
            .record => |record| {
                const parent_root = self.patternProjectionParentRoot(pat_root, pat.ty);
                const fields = self.output.record_field_patterns.items[record.fields.start..][0..record.fields.len];
                for (fields) |field| {
                    const child_root = self.structuralChildRoot(parent_root, .{ .record_field = field.field });
                    try self.publishPatternRepresentationEdges(child_root, field.pattern);
                }
                if (record.rest) |rest| {
                    try self.publishRecordRestPatternRepresentationEdges(parent_root, record.shape, rest);
                }
            },
            .tag => |tag| {
                const parent_root = self.patternProjectionParentRoot(pat_root, pat.ty);
                const payloads = self.output.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                for (payloads) |payload| {
                    const child_root = self.structuralChildRoot(parent_root, .{ .tag_payload = payload.payload });
                    try self.publishPatternRepresentationEdges(child_root, payload.pattern);
                }
            },
            .list => |list| {
                const parent_root = self.patternProjectionParentRoot(pat_root, pat.ty);
                const elem_root = self.structuralChildRoot(parent_root, .list_elem);
                const items = self.output.pat_ids.items[list.items.start..][0..list.items.len];
                for (items) |item| {
                    try self.publishPatternRepresentationEdges(elem_root, item);
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pat| {
                        try self.publishPatternRepresentationEdges(parent_root, rest_pat);
                    }
                }
            },
        }
    }

    fn markPatternPendingComptimeDependencyOrigin(
        self: *BodySolver,
        pat_id: Ast.PatId,
    ) void {
        const pat = self.output.pats.items[@intFromEnum(pat_id)];
        self.value_store.values.items[@intFromEnum(pat.value_info)].pending_comptime_dependency_origin = true;
        switch (pat.data) {
            .as => |as| self.markPatternPendingComptimeDependencyOrigin(as.pattern),
            .nominal => |child| self.markPatternPendingComptimeDependencyOrigin(child),
            .tuple => |items| {
                const child_ids = self.output.pat_ids.items[items.start..][0..items.len];
                for (child_ids) |child_id| self.markPatternPendingComptimeDependencyOrigin(child_id);
            },
            .record => |record| {
                const fields = self.output.record_field_patterns.items[record.fields.start..][0..record.fields.len];
                for (fields) |field| self.markPatternPendingComptimeDependencyOrigin(field.pattern);
                if (record.rest) |rest| self.markPatternPendingComptimeDependencyOrigin(rest);
            },
            .tag => |tag| {
                const payloads = self.output.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                for (payloads) |payload| self.markPatternPendingComptimeDependencyOrigin(payload.pattern);
            },
            .list => |list| {
                const items = self.output.pat_ids.items[list.items.start..][0..list.items.len];
                for (items) |item| self.markPatternPendingComptimeDependencyOrigin(item);
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pat| self.markPatternPendingComptimeDependencyOrigin(rest_pat);
                }
            },
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .wildcard,
            .var_,
            => {},
        }
    }

    fn publishConstBackedPatternValues(
        self: *BodySolver,
        backing: repr.ConstBackedValueInfo,
        pat_id: Ast.PatId,
    ) Allocator.Error!void {
        const pat = self.output.pats.items[@intFromEnum(pat_id)];
        switch (pat.data) {
            .tag => |tag| {
                if (!self.constBackedTagMatches(backing, tag.tag)) return;
            },
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            => {},
            .wildcard,
            .var_,
            .as,
            .nominal,
            .tuple,
            .record,
            .list,
            => {},
        }

        try self.publishConstBackedValueMetadata(pat.value_info, backing);

        switch (pat.data) {
            .as => |as| try self.publishConstBackedPatternValues(backing, as.pattern),
            .nominal => |child| try self.publishConstBackedPatternValues(try self.constBackedNominalBacking(backing), child),
            .tuple => |items| {
                const child_ids = self.output.pat_ids.items[items.start..][0..items.len];
                for (child_ids, 0..) |child_id, i| {
                    try self.publishConstBackedPatternValues(
                        self.constBackedTupleElem(
                            self.unwrapConstBackedValue(self.resolveConstBackedValue(backing)),
                            @intCast(i),
                            backing.const_instance,
                        ),
                        child_id,
                    );
                }
            },
            .record => |record| {
                const fields = self.output.record_field_patterns.items[record.fields.start..][0..record.fields.len];
                for (fields) |field| {
                    try self.publishConstBackedPatternValues(
                        try self.constBackedRecordField(
                            self.unwrapConstBackedValue(self.resolveConstBackedValue(backing)),
                            field.field,
                            backing.const_instance,
                        ),
                        field.pattern,
                    );
                }
                if (record.rest) |rest| {
                    try self.publishConstBackedPatternValues(backing, rest);
                }
            },
            .tag => |tag| {
                const payloads = self.output.tag_payload_patterns.items[tag.payloads.start..][0..tag.payloads.len];
                for (payloads) |payload| {
                    try self.publishConstBackedPatternValues(
                        self.constBackedTagPayload(
                            self.unwrapConstBackedValue(self.resolveConstBackedValue(backing)),
                            payload.payload,
                            backing.const_instance,
                        ),
                        payload.pattern,
                    );
                }
            },
            .list => |list| {
                const items = self.output.pat_ids.items[list.items.start..][0..list.items.len];
                for (items, 0..) |item, i| {
                    try self.publishConstBackedPatternValues(
                        try self.constBackedListElemAt(backing, @intCast(i)),
                        item,
                    );
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pat| {
                        try self.publishConstBackedPatternValues(backing, rest_pat);
                    }
                }
            },
            .bool_lit,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .str_lit,
            .wildcard,
            .var_,
            => {},
        }
    }

    fn publishRecordRestPatternRepresentationEdges(
        self: *BodySolver,
        source_record_root: repr.RepRootId,
        source_shape: MonoRow.RecordShapeId,
        rest_pat_id: Ast.PatId,
    ) Allocator.Error!void {
        const rest_pat = self.output.pats.items[@intFromEnum(rest_pat_id)];
        const rest_root = self.valueRoot(rest_pat.value_info);
        const rest_parent_root = self.patternProjectionParentRoot(rest_root, rest_pat.ty);
        const rest_fields = try self.logicalRecordFields(rest_pat.ty);
        const rest_shape = try self.recordShapeForTypeFields(rest_fields);
        for (rest_fields) |field| {
            const source_field = self.recordShapeFieldByLabel(source_shape, field.name);
            const source_field_root = self.structuralChildRoot(source_record_root, .{ .record_field = source_field });
            const rest_field = self.recordShapeFieldByLabel(rest_shape, field.name);
            const rest_field_root = self.structuralChildRoot(rest_parent_root, .{ .record_field = rest_field });
            try self.publishRootAlias(source_field_root, rest_field_root);
        }
        try self.publishPatternRepresentationEdges(rest_root, rest_pat_id);
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
            if (!self.exprCanCompleteNormally(body)) continue;
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

    fn publishJoinRepresentationEdges(
        self: *BodySolver,
        result: repr.ValueInfoId,
        inputs: repr.Span(repr.JoinInputInfo),
    ) Allocator.Error!void {
        const result_root = self.valueRoot(result);
        for (self.value_store.sliceJoinInputSpan(inputs)) |input| {
            _ = try self.representation_store.appendRepresentationEdge(.{
                .from = .{ .local = self.valueRoot(input.value) },
                .to = .{ .local = result_root },
                .kind = .branch_join,
            });
        }
    }

    fn publishJoinResult(
        self: *BodySolver,
        result: repr.ValueInfoId,
        join_info: repr.JoinInfoId,
    ) Allocator.Error!void {
        const value_info = &self.value_store.values.items[@intFromEnum(result)];
        if (value_info.join_info) |existing| {
            if (existing != join_info) lambdaInvariant("lambda-solved join result already points at a different join");
        } else {
            value_info.join_info = join_info;
        }
        const join = self.value_store.joins.items[@intFromEnum(join_info)];
        for (self.value_store.sliceJoinInputSpan(join.inputs)) |input| {
            try self.propagatePendingComptimeDependencyOrigin(input.value, result);
        }
    }

    fn exprCanCompleteNormally(self: *const BodySolver, expr_id: Ast.ExprId) bool {
        return switch (self.output.exprs.items[@intFromEnum(expr_id)].data) {
            .return_,
            .crash,
            .runtime_error,
            => false,
            .block => |block| self.blockCanCompleteNormally(block.stmts, block.final_expr),
            .if_ => |if_| self.exprCanCompleteNormally(if_.then_body) or
                self.exprCanCompleteNormally(if_.else_body),
            .match_ => |match_| self.anyBranchCanCompleteNormally(match_.branches),
            else => true,
        };
    }

    fn blockCanCompleteNormally(
        self: *const BodySolver,
        stmts: Ast.Span(Ast.StmtId),
        final_expr: Ast.ExprId,
    ) bool {
        const stmt_ids = self.output.stmt_ids.items[stmts.start..][0..stmts.len];
        for (stmt_ids) |stmt_id| {
            if (!self.stmtCanCompleteNormally(stmt_id)) return false;
        }
        return self.exprCanCompleteNormally(final_expr);
    }

    fn stmtCanCompleteNormally(self: *const BodySolver, stmt_id: Ast.StmtId) bool {
        return switch (self.output.stmts.items[@intFromEnum(stmt_id)]) {
            .decl => |decl| self.exprCanCompleteNormally(decl.body),
            .var_decl => |decl| self.exprCanCompleteNormally(decl.body),
            .reassign => |reassign| self.exprCanCompleteNormally(reassign.body),
            .expr => |expr| self.exprCanCompleteNormally(expr),
            .debug => |expr| self.exprCanCompleteNormally(expr),
            .expect => |expr| self.exprCanCompleteNormally(expr),
            .crash,
            .return_,
            .break_,
            => false,
            .for_,
            .while_,
            => true,
        };
    }

    fn anyBranchCanCompleteNormally(
        self: *const BodySolver,
        branches: Ast.Span(Ast.BranchId),
    ) bool {
        const branch_ids = self.output.branch_ids.items[branches.start..][0..branches.len];
        for (branch_ids) |branch_id| {
            if (self.exprCanCompleteNormally(self.output.branches.items[@intFromEnum(branch_id)].body)) return true;
        }
        return false;
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
                .eval_index = field.eval_index,
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
                .eval_index = payload.eval_index,
            };
        }
        return try self.output.addTagPayloadAssemblySpan(output_items);
    }

    fn recordAggregateFieldValue(_: *const BodySolver, record: anytype, field: MonoRow.RecordFieldId) repr.ValueInfoId {
        for (record.fields) |field_info| {
            if (field_info.field == field) return field_info.value;
        }
        lambdaInvariant("lambda-solved record aggregate projection referenced a missing field");
    }

    fn tupleAggregateElemValue(_: *const BodySolver, tuple: []const repr.ElemValueInfo, elem_index: u32) repr.ValueInfoId {
        for (tuple) |elem| {
            if (elem.index == elem_index) return elem.value;
        }
        lambdaInvariant("lambda-solved tuple aggregate projection referenced a missing element");
    }

    fn tagAggregatePayloadValue(_: *const BodySolver, tag: anytype, payload: MonoRow.TagPayloadId) repr.ValueInfoId {
        for (tag.payloads) |payload_info| {
            if (payload_info.payload == payload) return payload_info.value;
        }
        lambdaInvariant("lambda-solved tag aggregate projection referenced a missing payload");
    }

    fn aggregateForProjection(
        self: *const BodySolver,
        value: repr.ValueInfoId,
    ) ?repr.AggregateValueInfo {
        var current = value;
        var remaining = self.value_store.values.items.len;
        while (remaining != 0) : (remaining -= 1) {
            const info = self.value_store.values.items[@intFromEnum(current)];
            if (info.aggregate) |aggregate| return aggregate;
            current = info.value_alias_source orelse return null;
        }
        lambdaInvariant("lambda-solved aggregate projection alias chain is cyclic");
    }

    fn constBackedForProjection(
        self: *const BodySolver,
        value: repr.ValueInfoId,
    ) ?repr.ConstBackedValueInfo {
        var current = value;
        var remaining = self.value_store.values.items.len;
        while (remaining != 0) : (remaining -= 1) {
            const info = self.value_store.values.items[@intFromEnum(current)];
            if (info.const_backing) |backing| return backing;
            current = info.value_alias_source orelse return null;
        }
        lambdaInvariant("lambda-solved const-backed projection alias chain is cyclic");
    }

    fn publishConstBackedProjectionValue(
        self: *BodySolver,
        source: repr.ValueInfoId,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
        kind: repr.ProjectionKind,
    ) Allocator.Error!?ConstBackedProjectionResult {
        const source_backing = self.constBackedForProjection(source) orelse return null;
        const child_backing = try self.constBackedProjectionChild(source_backing, kind);
        const result = try self.newValue(ty, source_ty);
        const projection = try self.publishProjectionInfo(source, result, kind);
        try self.publishConstBackedValueMetadata(result, child_backing);
        return .{
            .value = result,
            .projection = projection,
        };
    }

    fn publishConstBackedValueMetadata(
        self: *BodySolver,
        value: repr.ValueInfoId,
        backing: repr.ConstBackedValueInfo,
    ) Allocator.Error!void {
        const info = &self.value_store.values.items[@intFromEnum(value)];
        if (info.const_backing) |existing| {
            if (!constBackedValueInfoEql(existing, backing)) {
                lambdaInvariant("lambda-solved value received conflicting const-backed metadata");
            }
        } else {
            info.const_backing = backing;
        }

        const resolved = self.unwrapConstBackedValue(self.resolveConstBackedValue(backing));
        const schema = self.constSchema(resolved.materialization, resolved.schema);
        const value_data = self.constValue(resolved.materialization, resolved.value);
        switch (schema) {
            .callable => {
                const leaf = switch (value_data) {
                    .callable => |leaf| leaf,
                    else => lambdaInvariant("lambda-solved const-backed callable schema had non-callable value"),
                };
                try self.publishConstBackedCallableLeaf(value, backing.const_instance.owner, leaf);
            },
            .record => try self.publishConstBackedRecordAggregate(value, backing, resolved),
            .tuple => try self.publishConstBackedTupleAggregate(value, backing, resolved),
            .list => try self.publishConstBackedListAggregate(value, backing, resolved),
            .box => try self.publishConstBackedBoxedValue(value, backing, resolved),
            .pending => lambdaInvariant("lambda-solved const-backed value reached pending schema"),
            .zst,
            .int,
            .frac,
            .str,
            .tag_union,
            .alias,
            .nominal,
            => {},
        }
    }

    fn publishConstBackedCallableLeaf(
        self: *BodySolver,
        value: repr.ValueInfoId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        leaf: checked_artifact.CallableLeafInstance,
    ) Allocator.Error!void {
        if (self.value_store.values.items[@intFromEnum(value)].callable != null) return;
        switch (leaf) {
            .finite => |finite| try self.publishConstBackedFiniteCallableLeaf(value, finite),
            .erased_boxed => |erased| try self.publishConstBackedErasedCallableLeaf(value, owner, erased),
        }
    }

    fn publishConstBackedErasedCallableLeaf(
        self: *BodySolver,
        value: repr.ValueInfoId,
        owner: checked_artifact.CheckedModuleArtifactKey,
        erased: checked_artifact.ErasedCallableLeafInstance,
    ) Allocator.Error!void {
        const sealed = erased.sealed;
        const boundary = sealed.boundary;
        if (boundary.provenance.len == 0) {
            lambdaInvariant("lambda-solved const-backed erased callable leaf had no Box(T) provenance");
        }
        for (boundary.provenance) |provenance| {
            switch (provenance) {
                .local_box_boundary => lambdaInvariant("lambda-solved const-backed erased callable leaf carried session-local BoxBoundaryId provenance"),
                .const_graph_box,
                .promoted_wrapper,
                => {},
            }
        }
        try self.importConstBackedErasedAbi(owner, boundary.sig_key.abi);
        const current_record = self.registry.procRecord(self.instance);
        try self.registry.reserveSealedErasedCallableValueDependencies(
            current_record.solve_session,
            owner,
            sealed,
        );
        self.refreshValueStore();
        const capture_plan: repr.AlreadyErasedCapturePlan = if (boundary.sig_key.capture_ty) |capture_key| blk: {
            _ = try self.importConstBackedExecutableEndpoint(owner, capture_key);
            switch (sealed.capture) {
                .none => lambdaInvariant("lambda-solved const-backed erased callable had hidden capture signature but no materialized capture"),
                else => {},
            }
            break :blk .{ .materialized_capture = .{
                .owner = owner,
                .capture = sealed.capture,
            } };
        } else .none;
        const plan = repr.AlreadyErasedCallablePlan{
            .sig_key = boundary.sig_key,
            .capture_shape_key = erasedCallableLeafCaptureShapeKey(erased),
            .result_ty = repr.erasedCallableExecValueTypeKey(boundary.sig_key),
            .capture = capture_plan,
            .provenance = boundary.provenance,
        };
        const emission = try self.representation_store.appendAlreadyErasedCallableEmissionPlan(plan);
        const value_info = &self.value_store.values.items[@intFromEnum(value)];
        if (!self.valueHasFunctionType(value_info.logical_ty)) {
            lambdaInvariant("lambda-solved const-backed erased callable leaf was attached to a non-function value");
        }
        value_info.callable = .{
            .whole_function_root = value_info.root,
            .callable_root = self.structuralChildRoot(value_info.root, .function_callable),
            .source = .{ .already_erased = .{
                .sig_key = boundary.sig_key,
                .capture_shape_key = plan.capture_shape_key,
                .result_ty = plan.result_ty,
                .capture = capture_plan,
                .provenance = &.{},
            } },
            .emission_plan = emission,
            .construction_plan = null,
        };
    }

    fn importConstBackedErasedAbi(
        self: *BodySolver,
        owner: checked_artifact.CheckedModuleArtifactKey,
        key: canonical.ErasedFnAbiKey,
    ) Allocator.Error!void {
        if (self.representation_store.erased_fn_abis.abiFor(key) != null) return;
        const source_abis = erasedFnAbisForArtifactViews(self.artifact_views, owner) orelse {
            lambdaInvariant("lambda-solved const-backed erased callable referenced unavailable erased ABI owner");
        };
        const source = source_abis.abiFor(key) orelse {
            lambdaInvariant("lambda-solved const-backed erased callable referenced missing erased ABI");
        };
        _ = try self.importConstBackedExecutableEndpoint(owner, source.ret_exec_key);
        for (source.arg_exec_keys) |arg_key| {
            _ = try self.importConstBackedExecutableEndpoint(owner, arg_key);
        }
        const published = try self.representation_store.erased_fn_abis.append(self.allocator, source.*);
        if (!canonical.erasedFnAbiKeyEql(published, key)) {
            lambdaInvariant("lambda-solved imported const-backed erased ABI under a different key");
        }
    }

    fn importConstBackedExecutableEndpoint(
        self: *BodySolver,
        owner: checked_artifact.CheckedModuleArtifactKey,
        key: canonical.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableTypeEndpoint {
        if (self.representation_store.session_executable_type_payloads.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }
        const payloads = executablePayloadsForArtifactViews(self.artifact_views, owner) orelse {
            lambdaInvariant("lambda-solved const-backed erased callable referenced unavailable executable payload owner");
        };
        const source_ref = payloads.refForKey(.{ .bytes = owner.bytes }, key) orelse {
            lambdaInvariant("lambda-solved const-backed erased callable capture key had no published executable payload");
        };
        const source_names = canonicalNamesForArtifactViews(self.artifact_views, owner);
        var importer = ArtifactExecutablePayloadImporter.init(
            self.allocator,
            source_names,
            &self.registry.program.canonical_names,
            self.row_shapes,
            owner,
            payloads,
            &self.representation_store.session_executable_type_payloads,
        );
        defer importer.deinit();
        return try importer.importRef(source_ref, key);
    }

    fn publishConstBackedFiniteCallableLeaf(
        self: *BodySolver,
        value: repr.ValueInfoId,
        finite: checked_artifact.FiniteCallableLeafInstance,
    ) Allocator.Error!void {
        const callable = try self.type_importer.name_resolver.procedureCallableRef(finite.proc_value);
        const proc = self.registry.procForCallable(callable) orelse {
            var same_template_count: usize = 0;
            for (self.registry.input.procs.items) |input_proc| {
                if (canonical.callableProcedureTemplateRefEql(input_proc.proc.callable.template, callable.template)) {
                    same_template_count += 1;
                }
            }
            lambdaInvariantFmt(
                "lambda-solved const-backed finite callable leaf referenced a procedure that was not published to lifted MIR (template={s}, input_procs={d}, synthetic_procs={d}, same_template_procs={d})",
                .{
                    @tagName(std.meta.activeTag(callable.template)),
                    self.registry.input.procs.items.len,
                    self.registry.input.executable_synthetic_procs.items.len,
                    same_template_count,
                },
            );
        };
        const whole_function_root = self.representation_store.reserveRoot();
        const source_fn_ty_payload = self.sourcePayloadForKey(proc.callable.source_fn_ty) orelse {
            lambdaInvariant("lambda-solved const-backed callable source function type has no concrete payload");
        };
        const fn_ty = try self.type_importer.importConcreteRef(source_fn_ty_payload);
        const target_instance = try self.registry.reserveProcValue(self.instance, value, proc, null);
        self.refreshValueStore();
        try self.representation_store.publishRootKind(whole_function_root, .{ .proc_value_fn = .{
            .instance = self.instance,
            .value = value,
        } });
        try self.publishFunctionRootShape(whole_function_root, fn_ty, proc.callable.source_fn_ty);
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = self.valueRoot(value) },
            .to = .{ .local = whole_function_root },
            .kind = .value_alias,
        });
        const callable_info = try self.representation_store.addSingletonProcValueCallable(
            value,
            whole_function_root,
            proc,
            null,
            target_instance,
            &.{},
            source_fn_ty_payload,
        );
        self.value_store.values.items[@intFromEnum(value)].callable = callable_info;
    }

    fn publishConstBackedRecordAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        backing: repr.ConstBackedValueInfo,
        resolved: ResolvedConstBackedValue,
    ) Allocator.Error!void {
        if (self.value_store.values.items[@intFromEnum(value)].aggregate != null) return;
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const logical_fields = try self.logicalRecordFields(value_info.logical_ty);
        const shape = try self.recordShapeForTypeFields(logical_fields);
        const shape_fields = self.row_shapes.recordShapeFields(shape);
        if (shape_fields.len != logical_fields.len) {
            lambdaInvariant("lambda-solved const-backed record aggregate shape arity mismatch");
        }
        const fields = try self.allocator.alloc(repr.FieldValueInfo, shape_fields.len);
        errdefer if (fields.len > 0) self.allocator.free(fields);

        const source_root = self.sourceRootForPayload(value_info.source_ty_payload);
        for (shape_fields, 0..) |field_id, i| {
            const field_ty = self.logicalRecordFieldType(logical_fields, field_id);
            const child_backing = try self.constBackedRecordField(resolved, field_id, backing.const_instance);
            const child_source_root = try self.sourceChildRoot(source_root, .{ .record_field = field_id });
            const child_value = try self.newValue(field_ty, if (child_source_root) |source| source.key else .{});
            try self.publishConstBackedValueMetadata(child_value, child_backing);
            fields[i] = .{
                .field = field_id,
                .value = child_value,
            };
        }
        try self.publishAggregate(value, .{ .record = .{
            .shape = shape,
            .fields = fields,
        } });
    }

    fn publishConstBackedTupleAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        backing: repr.ConstBackedValueInfo,
        resolved: ResolvedConstBackedValue,
    ) Allocator.Error!void {
        if (self.value_store.values.items[@intFromEnum(value)].aggregate != null) return;
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const logical_items = try self.logicalTupleItems(value_info.logical_ty);
        const schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .tuple => |items| items,
            else => lambdaInvariant("lambda-solved const-backed tuple aggregate reached non-tuple schema"),
        };
        const values = switch (self.constValue(resolved.materialization, resolved.value)) {
            .tuple => |items| items,
            else => lambdaInvariant("lambda-solved const-backed tuple aggregate reached non-tuple value"),
        };
        if (logical_items.len != schema.len or schema.len != values.len) {
            lambdaInvariant("lambda-solved const-backed tuple aggregate arity mismatch");
        }
        const child_values = try self.allocator.alloc(repr.ValueInfoId, logical_items.len);
        defer if (child_values.len > 0) self.allocator.free(child_values);
        const source_root = self.sourceRootForPayload(value_info.source_ty_payload);
        for (logical_items, 0..) |item_ty, i| {
            const child_source_root = try self.sourceChildRoot(source_root, .{ .tuple_elem = @intCast(i) });
            const child_value = try self.newValue(item_ty, if (child_source_root) |source| source.key else .{});
            try self.publishConstBackedValueMetadata(child_value, .{
                .const_instance = backing.const_instance,
                .schema = schema[i],
                .value = values[i],
            });
            child_values[i] = child_value;
        }
        const span = try self.value_store.addValueSpan(child_values);
        try self.publishTupleAggregate(value, span);
    }

    fn publishConstBackedListAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        backing: repr.ConstBackedValueInfo,
        resolved: ResolvedConstBackedValue,
    ) Allocator.Error!void {
        if (self.value_store.values.items[@intFromEnum(value)].aggregate != null) return;
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const elem_ty = try self.logicalListElemType(value_info.logical_ty);
        const elem_schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .list => |elem| elem,
            else => lambdaInvariant("lambda-solved const-backed list aggregate reached non-list schema"),
        };
        const values = switch (self.constValue(resolved.materialization, resolved.value)) {
            .list => |items| items,
            else => lambdaInvariant("lambda-solved const-backed list aggregate reached non-list value"),
        };
        const child_values = try self.allocator.alloc(repr.ValueInfoId, values.len);
        defer if (child_values.len > 0) self.allocator.free(child_values);
        const source_root = self.sourceRootForPayload(value_info.source_ty_payload);
        const elem_source_root = try self.sourceChildRoot(source_root, .list_elem);
        for (values, 0..) |item, i| {
            const child_value = try self.newValue(elem_ty, if (elem_source_root) |source| source.key else .{});
            try self.publishConstBackedValueMetadata(child_value, .{
                .const_instance = backing.const_instance,
                .schema = elem_schema,
                .value = item,
            });
            child_values[i] = child_value;
        }
        const span = try self.value_store.addValueSpan(child_values);
        try self.publishListAggregate(value, span);
    }

    fn publishConstBackedBoxedValue(
        self: *BodySolver,
        value: repr.ValueInfoId,
        backing: repr.ConstBackedValueInfo,
        resolved: ResolvedConstBackedValue,
    ) Allocator.Error!void {
        if (self.value_store.values.items[@intFromEnum(value)].boxed != null) return;
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const payload_ty = try self.logicalBoxPayloadType(value_info.logical_ty);
        const source_root = self.sourceRootForPayload(value_info.source_ty_payload);
        const payload_source_root = try self.sourceChildRoot(source_root, .box_payload);
        const payload_value = try self.newValue(payload_ty, if (payload_source_root) |source| source.key else .{});
        try self.publishConstBackedValueMetadata(payload_value, self.constBackedBoxPayload(resolved, backing.const_instance));
        try self.appendConstBackedBoxErasureRequirements(payload_value);

        const box_root = self.valueRoot(value);
        const payload_root = self.valueRoot(payload_value);
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = box_root },
            .to = .{ .local = payload_root },
            .kind = .{ .child = .box_payload },
        });
        self.value_store.values.items[@intFromEnum(value)].boxed = .{
            .box_root = box_root,
            .payload_root = payload_root,
            .payload_value = payload_value,
            .boundary = null,
        };
    }

    fn appendConstBackedBoxErasureRequirements(
        self: *BodySolver,
        payload_value: repr.ValueInfoId,
    ) Allocator.Error!void {
        const payload_info = self.value_store.values.items[@intFromEnum(payload_value)];
        const callable = payload_info.callable orelse return;
        const provenance = switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .already_erased => |erased| erased.provenance,
            .erase_proc_value => |erase| erase.provenance,
            .erase_finite_set => |erase| erase.provenance,
            .pending_proc_value,
            .finite,
            => return,
        };
        if (provenance.len == 0) {
            lambdaInvariant("lambda-solved const-backed boxed erased callable payload had no Box(T) provenance");
        }
        const payload_root = self.valueRoot(payload_value);
        for (provenance) |item| {
            _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = .{
                .payload_root = payload_root,
                .provenance = item,
            } });
        }
    }

    fn constBackedProjectionChild(
        self: *BodySolver,
        backing: repr.ConstBackedValueInfo,
        kind: repr.ProjectionKind,
    ) Allocator.Error!repr.ConstBackedValueInfo {
        const resolved = self.unwrapConstBackedValue(self.resolveConstBackedValue(backing));
        return switch (kind) {
            .record_field => |field| try self.constBackedRecordField(resolved, field, backing.const_instance),
            .tuple_elem => |index| self.constBackedTupleElem(resolved, index, backing.const_instance),
            .tag_payload => |payload| self.constBackedTagPayload(resolved, payload, backing.const_instance),
        };
    }

    fn constBackedBoxPayload(
        self: *BodySolver,
        resolved: ResolvedConstBackedValue,
        const_instance: checked_artifact.ConstInstanceRef,
    ) repr.ConstBackedValueInfo {
        const payload_schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .box => |payload| payload,
            else => lambdaInvariant("lambda-solved const-backed box payload reached non-box schema"),
        };
        const payload_value = switch (self.constValue(resolved.materialization, resolved.value)) {
            .box => |payload| payload,
            else => lambdaInvariant("lambda-solved const-backed box payload reached non-box value"),
        };
        return .{
            .const_instance = const_instance,
            .schema = payload_schema,
            .value = payload_value,
        };
    }

    fn constBackedRecordField(
        self: *BodySolver,
        resolved: ResolvedConstBackedValue,
        field: MonoRow.RecordFieldId,
        const_instance: checked_artifact.ConstInstanceRef,
    ) Allocator.Error!repr.ConstBackedValueInfo {
        const schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .record => |fields| fields,
            else => lambdaInvariant("lambda-solved const-backed record projection reached non-record schema"),
        };
        const values = switch (self.constValue(resolved.materialization, resolved.value)) {
            .record => |values| values,
            else => lambdaInvariant("lambda-solved const-backed record projection reached non-record value"),
        };
        if (schema.len != values.len) {
            lambdaInvariant("lambda-solved const-backed record projection reached schema/value arity mismatch");
        }
        const logical_label = self.row_shapes.recordField(field).label;
        for (schema, values) |field_schema, field_value| {
            if (!self.constRecordFieldMatches(resolved.materialization, field_schema.name, logical_label)) continue;
            return .{
                .const_instance = const_instance,
                .schema = field_schema.schema,
                .value = field_value,
            };
        }
        lambdaInvariant("lambda-solved const-backed record projection could not find requested field");
    }

    fn logicalRecordFieldType(
        self: *BodySolver,
        fields: []const Type.Field,
        field: MonoRow.RecordFieldId,
    ) Type.TypeVarId {
        const label = self.row_shapes.recordField(field).label;
        for (fields) |logical_field| {
            if (logical_field.name == label) return logical_field.ty;
        }
        lambdaInvariant("lambda-solved const-backed record aggregate field missing from logical type");
    }

    fn constBackedTupleElem(
        self: *BodySolver,
        resolved: ResolvedConstBackedValue,
        index: u32,
        const_instance: checked_artifact.ConstInstanceRef,
    ) repr.ConstBackedValueInfo {
        const schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .tuple => |items| items,
            else => lambdaInvariant("lambda-solved const-backed tuple projection reached non-tuple schema"),
        };
        const values = switch (self.constValue(resolved.materialization, resolved.value)) {
            .tuple => |values| values,
            else => lambdaInvariant("lambda-solved const-backed tuple projection reached non-tuple value"),
        };
        if (schema.len != values.len) {
            lambdaInvariant("lambda-solved const-backed tuple projection reached schema/value arity mismatch");
        }
        const raw_index: usize = @intCast(index);
        if (raw_index >= schema.len) {
            lambdaInvariant("lambda-solved const-backed tuple projection index out of range");
        }
        return .{
            .const_instance = const_instance,
            .schema = schema[raw_index],
            .value = values[raw_index],
        };
    }

    fn constBackedTagPayload(
        self: *BodySolver,
        resolved: ResolvedConstBackedValue,
        payload: MonoRow.TagPayloadId,
        const_instance: checked_artifact.ConstInstanceRef,
    ) repr.ConstBackedValueInfo {
        const schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .tag_union => |variants| variants,
            else => lambdaInvariant("lambda-solved const-backed tag payload projection reached non-tag schema"),
        };
        const tag_value = switch (self.constValue(resolved.materialization, resolved.value)) {
            .tag_union => |tag| tag,
            else => lambdaInvariant("lambda-solved const-backed tag payload projection reached non-tag value"),
        };
        const variant_index: usize = @intCast(tag_value.variant_index);
        if (variant_index >= schema.len) {
            lambdaInvariant("lambda-solved const-backed tag payload projection variant index out of range");
        }
        const variant = schema[variant_index];
        const payload_info = self.row_shapes.tagPayload(payload);
        const tag_info = self.row_shapes.tag(payload_info.tag);
        if (!self.constTagMatches(resolved.materialization, variant.name, tag_info.label)) {
            lambdaInvariant("lambda-solved const-backed tag payload projection selected the wrong tag");
        }
        if (variant.payloads.len != tag_value.payloads.len) {
            lambdaInvariant("lambda-solved const-backed tag payload projection schema/value arity mismatch");
        }
        const payload_index: usize = @intCast(payload_info.logical_index);
        if (payload_index >= variant.payloads.len) {
            lambdaInvariant("lambda-solved const-backed tag payload projection payload index out of range");
        }
        return .{
            .const_instance = const_instance,
            .schema = variant.payloads[payload_index],
            .value = tag_value.payloads[payload_index],
        };
    }

    fn constBackedNominalBacking(
        self: *BodySolver,
        backing: repr.ConstBackedValueInfo,
    ) Allocator.Error!repr.ConstBackedValueInfo {
        const resolved = self.unwrapConstBackedValue(self.resolveConstBackedValue(backing));
        return .{
            .const_instance = backing.const_instance,
            .schema = resolved.schema,
            .value = resolved.value,
        };
    }

    fn constBackedListElemAt(
        self: *BodySolver,
        backing: repr.ConstBackedValueInfo,
        index: u32,
    ) Allocator.Error!repr.ConstBackedValueInfo {
        const resolved = self.unwrapConstBackedValue(self.resolveConstBackedValue(backing));
        const elem_schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .list => |elem| elem,
            else => lambdaInvariant("lambda-solved const-backed list pattern reached non-list schema"),
        };
        const values = switch (self.constValue(resolved.materialization, resolved.value)) {
            .list => |items| items,
            else => lambdaInvariant("lambda-solved const-backed list pattern reached non-list value"),
        };
        const raw_index: usize = @intCast(index);
        if (raw_index >= values.len) {
            lambdaInvariant("lambda-solved const-backed list pattern item index out of range");
        }
        return .{
            .const_instance = backing.const_instance,
            .schema = elem_schema,
            .value = values[raw_index],
        };
    }

    fn constBackedTagMatches(
        self: *BodySolver,
        backing: repr.ConstBackedValueInfo,
        tag_id: MonoRow.TagId,
    ) bool {
        const resolved = self.unwrapConstBackedValue(self.resolveConstBackedValue(backing));
        const schema = switch (self.constSchema(resolved.materialization, resolved.schema)) {
            .tag_union => |variants| variants,
            else => return false,
        };
        const tag_value = switch (self.constValue(resolved.materialization, resolved.value)) {
            .tag_union => |tag| tag,
            else => lambdaInvariant("lambda-solved const-backed tag schema had non-tag value"),
        };
        const variant_index: usize = @intCast(tag_value.variant_index);
        if (variant_index >= schema.len) {
            lambdaInvariant("lambda-solved const-backed tag pattern variant index out of range");
        }
        const tag_info = self.row_shapes.tag(tag_id);
        return self.constTagMatches(resolved.materialization, schema[variant_index].name, tag_info.label);
    }

    fn constBackedRoot(
        self: *const BodySolver,
        ref: checked_artifact.ConstInstanceRef,
    ) repr.ConstBackedValueInfo {
        const resolved = self.resolveConstInstance(ref);
        return .{
            .const_instance = ref,
            .schema = resolved.instance.schema,
            .value = resolved.instance.value,
        };
    }

    fn resolveConstBackedValue(
        self: *const BodySolver,
        backing: repr.ConstBackedValueInfo,
    ) ResolvedConstBackedValue {
        return .{
            .materialization = self.constMaterializationForArtifact(backing.const_instance.owner),
            .schema = backing.schema,
            .value = backing.value,
        };
    }

    fn resolveConstInstance(
        self: *const BodySolver,
        ref: checked_artifact.ConstInstanceRef,
    ) ResolvedConstInstance {
        const materialization = self.constMaterializationForArtifact(ref.owner);
        const instances = self.constInstancesForArtifact(ref.owner);
        if (!artifactKeyEql(instances.owner, ref.owner)) {
            lambdaInvariant("lambda-solved constant instance view has wrong owning artifact");
        }
        const index: usize = @intFromEnum(ref.instance);
        if (index >= instances.instances.len) {
            lambdaInvariant("lambda-solved const-backed value referenced an out-of-range constant instance");
        }
        const record = instances.instances[index];
        if (!checked_artifact.constInstantiationKeyEql(record.key, ref.key)) {
            lambdaInvariant("lambda-solved const-backed value instance key does not match published row");
        }
        const instance = switch (record.state) {
            .evaluated => |instance| instance,
            .reserved,
            .evaluating,
            => lambdaInvariant("lambda-solved const-backed value consumed an unsealed constant instance"),
        };
        return .{
            .materialization = materialization,
            .instance = instance,
        };
    }

    fn constMaterializationForArtifact(
        self: *const BodySolver,
        owner: checked_artifact.CheckedModuleArtifactKey,
    ) ConstMaterializationView {
        if (artifactKeyEql(self.artifact_views.root.artifact.key, owner)) {
            return .{
                .owner = self.artifact_views.root.artifact.key,
                .names = &self.artifact_views.root.artifact.canonical_names,
                .values = &self.artifact_views.root.artifact.comptime_values,
            };
        }
        for (self.artifact_views.imports) |imported| {
            if (!artifactKeyEql(imported.key, owner)) continue;
            return .{
                .owner = imported.key,
                .names = imported.canonical_names,
                .values = imported.comptime_values,
            };
        }
        for (self.artifact_views.root.relation_artifacts) |related| {
            if (!artifactKeyEql(related.key, owner)) continue;
            return .{
                .owner = related.key,
                .names = related.canonical_names,
                .values = related.comptime_values,
            };
        }
        lambdaInvariant("lambda-solved const-backed value referenced an unpublished artifact");
    }

    fn constInstancesForArtifact(
        self: *const BodySolver,
        owner: checked_artifact.CheckedModuleArtifactKey,
    ) checked_artifact.ConstInstantiationStoreView {
        if (artifactKeyEql(self.artifact_views.root.artifact.key, owner)) {
            return self.artifact_views.root.artifact.const_instances.view();
        }
        for (self.artifact_views.imports) |imported| {
            if (artifactKeyEql(imported.key, owner)) return imported.const_instances;
        }
        for (self.artifact_views.root.relation_artifacts) |related| {
            if (artifactKeyEql(related.key, owner)) return related.const_instances;
        }
        lambdaInvariant("lambda-solved const-backed value referenced unpublished constant instances");
    }

    fn unwrapConstBackedValue(
        self: *const BodySolver,
        resolved: ResolvedConstBackedValue,
    ) ResolvedConstBackedValue {
        var current = resolved;
        var remaining = current.materialization.values.schemas.items.len + current.materialization.values.values.items.len;
        while (remaining != 0) : (remaining -= 1) {
            const schema = self.constSchema(current.materialization, current.schema);
            switch (schema) {
                .alias => |alias| {
                    current.schema = alias.backing;
                    current.value = switch (self.constValue(current.materialization, current.value)) {
                        .alias => |backing| backing,
                        else => lambdaInvariant("lambda-solved const-backed alias schema had non-alias value"),
                    };
                },
                .nominal => |nominal| {
                    current.schema = nominal.backing;
                    current.value = switch (self.constValue(current.materialization, current.value)) {
                        .nominal => |backing| backing,
                        else => lambdaInvariant("lambda-solved const-backed nominal schema had non-nominal value"),
                    };
                },
                else => return current,
            }
        }
        lambdaInvariant("lambda-solved const-backed schema/value wrapper chain is cyclic");
    }

    fn constSchema(
        _: *const BodySolver,
        materialization: ConstMaterializationView,
        id: checked_artifact.ComptimeSchemaId,
    ) checked_artifact.ComptimeSchema {
        const index: usize = @intFromEnum(id);
        if (index >= materialization.values.schemas.items.len) {
            lambdaInvariant("lambda-solved const-backed schema id out of range");
        }
        return materialization.values.schemas.items[index];
    }

    fn constValue(
        _: *const BodySolver,
        materialization: ConstMaterializationView,
        id: checked_artifact.ComptimeValueId,
    ) checked_artifact.ComptimeValue {
        const index: usize = @intFromEnum(id);
        if (index >= materialization.values.values.items.len) {
            lambdaInvariant("lambda-solved const-backed value id out of range");
        }
        return materialization.values.values.items[index];
    }

    fn constRecordFieldMatches(
        self: *const BodySolver,
        materialization: ConstMaterializationView,
        source_field: canonical.RecordFieldLabelId,
        logical_field: canonical.RecordFieldLabelId,
    ) bool {
        return checked_artifact.recordFieldLabelsMatch(
            materialization.names,
            source_field,
            self.canonical_names,
            logical_field,
        );
    }

    fn constTagMatches(
        self: *const BodySolver,
        materialization: ConstMaterializationView,
        source_tag: canonical.TagLabelId,
        logical_tag: canonical.TagLabelId,
    ) bool {
        return checked_artifact.tagLabelsMatch(
            materialization.names,
            source_tag,
            self.canonical_names,
            logical_tag,
        );
    }

    fn recordAssemblyEval(
        evals: []const Ast.RecordFieldEval,
        assembly: Ast.RecordFieldAssembly,
    ) Ast.RecordFieldEval {
        if (assembly.eval_index >= evals.len) {
            lambdaInvariant("lambda-solved record assembly referenced eval index outside eval order");
        }
        const evaluated = evals[assembly.eval_index];
        if (evaluated.field != assembly.field) {
            lambdaInvariant("lambda-solved record assembly field disagreed with eval-order field");
        }
        return evaluated;
    }

    fn tagAssemblyEval(
        evals: []const Ast.TagPayloadEval,
        assembly: Ast.TagPayloadAssembly,
    ) Ast.TagPayloadEval {
        if (assembly.eval_index >= evals.len) {
            lambdaInvariant("lambda-solved tag assembly referenced eval index outside eval order");
        }
        const evaluated = evals[assembly.eval_index];
        if (evaluated.payload != assembly.payload) {
            lambdaInvariant("lambda-solved tag assembly payload disagreed with eval-order payload");
        }
        return evaluated;
    }

    fn publishRecordAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        shape: MonoRow.RecordShapeId,
        eval_order: Ast.Span(Ast.RecordFieldEval),
        assembly_order: Ast.Span(Ast.RecordFieldAssembly),
    ) Allocator.Error!void {
        const evals = self.output.record_field_evals.items[eval_order.start..][0..eval_order.len];
        const assemblies = self.output.record_field_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const fields = try self.allocator.alloc(repr.FieldValueInfo, assemblies.len);
        errdefer if (fields.len > 0) self.allocator.free(fields);

        for (assemblies, 0..) |field, i| {
            const evaluated = recordAssemblyEval(evals, field);
            fields[i] = .{
                .field = field.field,
                .value = self.exprValue(evaluated.value),
            };
        }
        try self.publishAggregate(value, .{ .record = .{
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
        try self.publishAggregate(value, .{ .tuple = infos });
    }

    fn publishTagAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        union_shape: MonoRow.TagUnionShapeId,
        tag_id: MonoRow.TagId,
        eval_order: Ast.Span(Ast.TagPayloadEval),
        assembly_order: Ast.Span(Ast.TagPayloadAssembly),
    ) Allocator.Error!void {
        const evals = self.output.tag_payload_evals.items[eval_order.start..][0..eval_order.len];
        const assemblies = self.output.tag_payload_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const payloads = try self.allocator.alloc(repr.TagPayloadValueInfo, assemblies.len);
        errdefer if (payloads.len > 0) self.allocator.free(payloads);

        for (assemblies, 0..) |payload, i| {
            const evaluated = tagAssemblyEval(evals, payload);
            payloads[i] = .{
                .payload = payload.payload,
                .value = self.exprValue(evaluated.value),
            };
        }
        var payload_root_count: usize = 0;
        for (self.row_shapes.tagUnionTags(union_shape)) |shape_tag| {
            payload_root_count += self.row_shapes.tagPayloads(shape_tag).len;
        }
        const payload_roots = try self.allocator.alloc(repr.TagPayloadRootInfo, payload_root_count);
        errdefer if (payload_roots.len > 0) self.allocator.free(payload_roots);
        var next_payload_root: usize = 0;
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const source_root = self.sourceRootForPayload(value_info.source_ty_payload);
        const source_tags = try self.logicalTagUnionTags(value_info.logical_ty);
        for (self.row_shapes.tagUnionTags(union_shape)) |shape_tag| {
            const shape_tag_info = self.row_shapes.tag(shape_tag);
            const source_tag = self.logicalTagByLabel(source_tags, shape_tag_info.label);
            const tag_args = self.type_importer.output.sliceTypeVarSpan(source_tag.args);
            for (self.row_shapes.tagPayloads(shape_tag)) |shape_payload| {
                const payload_info = self.row_shapes.tagPayload(shape_payload);
                const payload_index: usize = @intCast(payload_info.logical_index);
                if (payload_index >= tag_args.len) lambdaInvariant("lambda-solved tag aggregate structural root payload index exceeded logical args");
                const payload_source_root = try self.sourceChildRoot(source_root, .{ .tag_payload = shape_payload });
                const payload_source_ty = if (payload_source_root) |source| source.key else canonical.CanonicalTypeKey{};
                payload_roots[next_payload_root] = .{
                    .payload = shape_payload,
                    .root = self.representation_store.reserveRoot(),
                };
                try self.publishStructuralRootEdges(payload_roots[next_payload_root].root, tag_args[payload_index], payload_source_ty, payload_source_root);
                next_payload_root += 1;
            }
        }
        try self.publishAggregate(value, .{ .tag = .{
            .union_shape = union_shape,
            .tag = tag_id,
            .payloads = payloads,
            .payload_roots = payload_roots,
        } });
    }

    fn publishListAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const owned_elems = try self.allocator.alloc(repr.ElemValueInfo, elem_values.len);
        errdefer if (owned_elems.len > 0) self.allocator.free(owned_elems);
        for (elem_values, 0..) |elem, i| {
            owned_elems[i] = .{
                .index = @intCast(i),
                .value = elem,
            };
        }
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const source_root = self.sourceRootForPayload(value_info.source_ty_payload);
        const elem_source_root = try self.sourceChildRoot(source_root, .list_elem);
        const elem_source_ty = if (elem_source_root) |source| source.key else canonical.CanonicalTypeKey{};
        const elem_root = self.representation_store.reserveRoot();
        try self.publishStructuralRootEdges(
            elem_root,
            try self.logicalListElemType(value_info.logical_ty),
            elem_source_ty,
            elem_source_root,
        );

        try self.publishAggregate(value, .{ .list = .{
            .elem_root = elem_root,
            .elems = owned_elems,
        } });
    }

    fn publishAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        aggregate: repr.AggregateValueInfo,
    ) Allocator.Error!void {
        const value_info = &self.value_store.values.items[@intFromEnum(value)];
        if (value_info.aggregate != null) lambdaInvariant("lambda-solved value published aggregate metadata twice");
        value_info.aggregate = aggregate;
        try self.publishAggregateRepresentationEdges(value, aggregate);
    }

    fn publishAggregateRepresentationEdges(
        self: *BodySolver,
        value: repr.ValueInfoId,
        aggregate: repr.AggregateValueInfo,
    ) Allocator.Error!void {
        const value_info = self.value_store.values.items[@intFromEnum(value)];
        const target = self.aggregateRepresentationTarget(value_info.root, value_info.logical_ty);
        const root = target.root;
        switch (aggregate) {
            .record => |record| {
                for (record.fields) |field| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(field.value) },
                        .kind = .{ .child = .{ .record_field = field.field } },
                    });
                }
            },
            .tuple => |tuple| {
                for (tuple) |elem| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(elem.value) },
                        .kind = .{ .child = .{ .tuple_elem = elem.index } },
                    });
                }
            },
            .tag => |tag| {
                for (tag.payload_roots) |payload| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = payload.root },
                        .kind = .{ .child = .{ .tag_payload = payload.payload } },
                    });
                }
                for (tag.payloads) |payload| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(payload.value) },
                        .kind = .{ .child = .{ .tag_payload = payload.payload } },
                    });
                }
            },
            .list => |list| {
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = root },
                    .to = .{ .local = list.elem_root },
                    .kind = .{ .child = .list_elem },
                });
                for (list.elems) |elem| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(elem.value) },
                        .kind = .{ .child = .list_elem },
                    });
                }
            },
        }
    }

    const AggregateRepresentationTarget = struct {
        root: repr.RepRootId,
        logical_ty: Type.TypeVarId,
    };

    fn aggregateRepresentationTarget(
        self: *BodySolver,
        root: repr.RepRootId,
        logical_ty: Type.TypeVarId,
    ) AggregateRepresentationTarget {
        var current_root = root;
        var current_ty = logical_ty;
        while (true) {
            const unlinked = self.type_importer.output.unlinkConst(current_ty);
            switch (self.type_importer.output.getNode(unlinked)) {
                .nominal => |nominal| {
                    current_root = self.structuralNominalBackingRoot(current_root, nominal.nominal);
                    current_ty = nominal.backing;
                },
                else => return .{
                    .root = current_root,
                    .logical_ty = current_ty,
                },
            }
        }
    }

    fn structuralNominalBackingRoot(
        self: *BodySolver,
        parent: repr.RepRootId,
        nominal: canonical.NominalTypeKey,
    ) repr.RepRootId {
        for (self.representation_store.representation_edges.items) |edge| {
            const from = switch (edge.from) {
                .local => |local| local,
                .procedure_public,
                .procedure_function_root,
                => continue,
            };
            if (from != parent) continue;
            switch (edge.kind) {
                .child => |child| switch (child) {
                    .nominal_backing => |backing| {
                        if (backing.module_name != nominal.module_name or backing.type_name != nominal.type_name) continue;
                    },
                    else => continue,
                },
                else => continue,
            }
            return switch (edge.to) {
                .local => |local| local,
                .procedure_public,
                .procedure_function_root,
                => lambdaInvariant("lambda-solved nominal backing edge targets procedure-public root"),
            };
        }
        lambdaInvariant("lambda-solved aggregate nominal value has no published nominal backing root");
    }

    fn patternProjectionParentRoot(
        self: *BodySolver,
        parent: repr.RepRootId,
        logical_ty: Type.TypeVarId,
    ) repr.RepRootId {
        const root_ty = self.type_importer.output.unlinkConst(logical_ty);
        return switch (self.type_importer.output.getNode(root_ty)) {
            .nominal => |nominal| self.structuralChildRoot(parent, .{ .nominal_backing = nominal.nominal }),
            else => parent,
        };
    }

    fn structuralChildRoot(
        self: *BodySolver,
        parent: repr.RepRootId,
        kind: repr.RepresentationChildKind,
    ) repr.RepRootId {
        for (self.representation_store.representation_edges.items) |edge| {
            const from = switch (edge.from) {
                .local => |local| local,
                .procedure_public,
                .procedure_function_root,
                => continue,
            };
            if (from != parent) continue;
            const child_kind = switch (edge.kind) {
                .child => |child| child,
                else => continue,
            };
            if (!representationChildKindMatches(child_kind, kind)) continue;
            return switch (edge.to) {
                .local => |local| local,
                .procedure_public,
                .procedure_function_root,
                => lambdaInvariant("lambda-solved structural child edge targets procedure-public root"),
            };
        }
        lambdaInvariant("lambda-solved structural child root is missing");
    }

    fn publishRootAlias(
        self: *BodySolver,
        source_root: repr.RepRootId,
        target_root: repr.RepRootId,
    ) Allocator.Error!void {
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = source_root },
            .to = .{ .local = target_root },
            .kind = .value_alias,
        });
    }

    fn publishValueAlias(
        self: *BodySolver,
        source: repr.ValueInfoId,
        result: repr.ValueInfoId,
    ) Allocator.Error!void {
        const result_info = &self.value_store.values.items[@intFromEnum(result)];
        if (result_info.value_alias_source) |existing| {
            if (existing != source) lambdaInvariant("lambda-solved value alias result already points at a different source value");
        } else {
            result_info.value_alias_source = source;
        }
        try self.propagatePendingComptimeDependencyOrigin(source, result);
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = self.valueRoot(source) },
            .to = .{ .local = self.valueRoot(result) },
            .kind = .value_alias,
        });
    }

    fn propagatePendingComptimeDependencyOrigin(
        self: *BodySolver,
        source: repr.ValueInfoId,
        result: repr.ValueInfoId,
    ) Allocator.Error!void {
        if (!self.value_store.values.items[@intFromEnum(source)].pending_comptime_dependency_origin) return;
        self.value_store.values.items[@intFromEnum(result)].pending_comptime_dependency_origin = true;
    }

    fn publishProjectionRepresentationEdge(
        self: *BodySolver,
        source: repr.ValueInfoId,
        result: repr.ValueInfoId,
        kind: repr.ProjectionKind,
    ) Allocator.Error!void {
        const edge_kind: repr.RepresentationChildKind = switch (kind) {
            .record_field => |field| .{ .record_field = field },
            .tuple_elem => |index| .{ .tuple_elem = index },
            .tag_payload => |payload| .{ .tag_payload = payload },
        };
        const source_root = self.projectionSourceRoot(source, kind);
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = source_root },
            .to = .{ .local = self.valueRoot(result) },
            .kind = .{ .child = edge_kind },
        });
    }

    fn projectionSourceRoot(
        self: *BodySolver,
        source: repr.ValueInfoId,
        _: repr.ProjectionKind,
    ) repr.RepRootId {
        const source_info = self.value_store.values.items[@intFromEnum(source)];
        const source_root = self.valueRoot(source);
        const root_ty = self.type_importer.output.unlinkConst(source_info.logical_ty);
        return switch (self.type_importer.output.getNode(root_ty)) {
            .nominal => |nominal| self.structuralNominalBackingRoot(source_root, nominal.nominal),
            else => source_root,
        };
    }

    fn publishNominalBackingValue(
        self: *BodySolver,
        nominal_value: repr.ValueInfoId,
        backing_value: repr.ValueInfoId,
        nominal_ty: Type.TypeVarId,
    ) Allocator.Error!void {
        const root_ty = self.type_importer.output.unlinkConst(nominal_ty);
        const nominal = switch (self.type_importer.output.getNode(root_ty)) {
            .nominal => |nominal| nominal.nominal,
            .content => |content| switch (content) {
                .primitive,
                .list,
                .box,
                .tag_union,
                => return,
                .func,
                .tuple,
                .record,
                => lambdaInvariant("lambda-solved nominal reinterpret result had unsupported non-nominal type"),
            },
            else => lambdaInvariant("lambda-solved nominal reinterpret result had unresolved non-nominal type"),
        };
        const nominal_info = &self.value_store.values.items[@intFromEnum(nominal_value)];
        if (nominal_info.nominal_backing_value) |existing| {
            if (existing != backing_value) lambdaInvariant("lambda-solved nominal value already points at a different backing value");
        } else {
            nominal_info.nominal_backing_value = backing_value;
        }
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = self.valueRoot(nominal_value) },
            .to = .{ .local = self.valueRoot(backing_value) },
            .kind = .{ .child = .{ .nominal_backing = nominal } },
        });
    }

    fn publishProjectionInfo(
        self: *BodySolver,
        source: repr.ValueInfoId,
        result: repr.ValueInfoId,
        kind: repr.ProjectionKind,
    ) Allocator.Error!repr.ProjectionInfoId {
        const projection = try self.value_store.addProjection(.{
            .source = source,
            .result = result,
            .root = self.valueRoot(result),
            .kind = kind,
        });
        const result_info = &self.value_store.values.items[@intFromEnum(result)];
        if (result_info.projection_info) |existing| {
            if (existing != projection) lambdaInvariant("lambda-solved projection result already points at a different projection");
        } else {
            result_info.projection_info = projection;
        }
        try self.propagatePendingComptimeDependencyOrigin(source, result);
        try self.publishProjectionRepresentationEdge(source, result, kind);
        return projection;
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
        const source_ty_payload = self.sourcePayloadForKey(source_ty);
        const value = try self.value_store.addValue(.{
            .logical_ty = ty,
            .source_ty = source_ty,
            .source_ty_payload = source_ty_payload,
            .root = root,
            .source_match_branch = self.active_source_match_branch,
        });
        try self.publishRootTypeInfo(root, ty, source_ty, self.sourceRootForPayload(source_ty_payload));
        try self.representation_store.publishRootKind(root, .{ .local_value = .{
            .instance = self.instance,
            .value = value,
        } });
        try self.publishStructuralRootEdges(root, ty, source_ty, self.sourceRootForPayload(source_ty_payload));
        return value;
    }

    fn publishRootTypeInfo(
        self: *BodySolver,
        root: repr.RepRootId,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
        source_root: ?ConcreteSourceType.ConcreteSourceTypeRoot,
    ) Allocator.Error!void {
        try self.representation_store.publishRootTypeInfo(root, .{
            .logical_ty = ty,
            .source_ty = source_ty,
            .source_root = source_root,
        });
    }

    fn publishFunctionRootShape(
        self: *BodySolver,
        root: repr.RepRootId,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!void {
        const source_ty_payload = self.sourcePayloadForKey(source_ty);
        try self.publishStructuralRootEdges(root, ty, source_ty, self.sourceRootForPayload(source_ty_payload));
    }

    fn publishStructuralRootEdges(
        self: *BodySolver,
        root: repr.RepRootId,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
        source_root: ?ConcreteSourceType.ConcreteSourceTypeRoot,
    ) Allocator.Error!void {
        var active = std.AutoHashMap(Type.TypeVarId, repr.RepRootId).init(self.allocator);
        defer active.deinit();
        try self.publishStructuralRootEdgesRec(root, ty, source_ty, source_root, &active);
    }

    fn publishStructuralRootEdgesRec(
        self: *BodySolver,
        rep_root: repr.RepRootId,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
        source_root: ?ConcreteSourceType.ConcreteSourceTypeRoot,
        active: *std.AutoHashMap(Type.TypeVarId, repr.RepRootId),
    ) Allocator.Error!void {
        try self.publishRootTypeInfo(rep_root, ty, source_ty, source_root);
        const root_ty = self.type_importer.output.unlinkConst(ty);
        if (active.get(root_ty) != null) return;
        try active.put(root_ty, rep_root);
        defer _ = active.remove(root_ty);

        switch (self.type_importer.output.getNode(root_ty)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => lambdaInvariant("lambda-solved structural root publication reached unresolved type"),
            .nominal => |nominal| {
                _ = try self.publishStructuralChildRoot(
                    rep_root,
                    .{ .nominal_backing = nominal.nominal },
                    nominal.backing,
                    try self.sourceChildRoot(source_root, .{ .nominal_backing = nominal.nominal }),
                    active,
                );
            },
            .content => |content| switch (content) {
                .primitive => {},
                .func => |func| {
                    const args = self.type_importer.output.sliceTypeVarSpan(func.args);
                    for (args, 0..) |arg, i| {
                        _ = try self.publishStructuralChildRoot(
                            rep_root,
                            .{ .function_arg = @intCast(i) },
                            arg,
                            try self.sourceChildRoot(source_root, .{ .function_arg = @intCast(i) }),
                            active,
                        );
                    }
                    _ = try self.publishStructuralChildRoot(
                        rep_root,
                        .function_return,
                        func.ret,
                        try self.sourceChildRoot(source_root, .function_return),
                        active,
                    );
                    _ = try self.publishFunctionCallableChildRoot(rep_root);
                },
                .list => |elem| {
                    _ = try self.publishStructuralChildRoot(rep_root, .list_elem, elem, try self.sourceChildRoot(source_root, .list_elem), active);
                },
                .box => |payload| {
                    _ = try self.publishStructuralChildRoot(rep_root, .box_payload, payload, try self.sourceChildRoot(source_root, .box_payload), active);
                },
                .tuple => |span| {
                    const elems = self.type_importer.output.sliceTypeVarSpan(span);
                    for (elems, 0..) |elem, i| {
                        const child_source = try self.sourceChildRoot(source_root, .{ .tuple_elem = @intCast(i) });
                        _ = try self.publishStructuralChildRoot(
                            rep_root,
                            .{ .tuple_elem = @intCast(i) },
                            elem,
                            child_source,
                            active,
                        );
                    }
                },
                .record => |record| {
                    const fields = self.type_importer.output.sliceFields(record.fields);
                    const shape = try self.recordShapeForTypeFields(fields);
                    const shape_fields = self.row_shapes.recordShapeFields(shape);
                    if (shape_fields.len != fields.len) {
                        lambdaInvariant("lambda-solved structural record root shape arity mismatch");
                    }
                    for (fields, shape_fields) |field, field_id| {
                        const child_source = try self.sourceChildRoot(source_root, .{ .record_field = field_id });
                        _ = try self.publishStructuralChildRoot(
                            rep_root,
                            .{ .record_field = field_id },
                            field.ty,
                            child_source,
                            active,
                        );
                    }
                },
                .tag_union => |tag_union| {
                    const tags = self.type_importer.output.sliceTags(tag_union.tags);
                    const shape = try self.tagUnionShapeForTypeTags(tags);
                    const shape_tags = self.row_shapes.tagUnionTags(shape);
                    if (shape_tags.len != tags.len) {
                        lambdaInvariant("lambda-solved structural tag root shape arity mismatch");
                    }
                    for (tags, shape_tags) |tag, tag_id| {
                        const args = self.type_importer.output.sliceTypeVarSpan(tag.args);
                        const payloads = self.row_shapes.tagPayloads(tag_id);
                        if (payloads.len != args.len) {
                            lambdaInvariant("lambda-solved structural tag root payload arity mismatch");
                        }
                        for (args, payloads) |arg, payload_id| {
                            const child_source = try self.sourceChildRoot(source_root, .{ .tag_payload = payload_id });
                            _ = try self.publishStructuralChildRoot(
                                rep_root,
                                .{ .tag_payload = payload_id },
                                arg,
                                child_source,
                                active,
                            );
                        }
                    }
                },
            },
        }
    }

    fn publishFunctionCallableChildRoot(
        self: *BodySolver,
        parent: repr.RepRootId,
    ) Allocator.Error!repr.RepRootId {
        const child_root = self.representation_store.reserveRoot();
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = parent },
            .to = .{ .local = child_root },
            .kind = .{ .child = .function_callable },
        });
        return child_root;
    }

    fn publishStructuralChildRoot(
        self: *BodySolver,
        parent: repr.RepRootId,
        kind: repr.RepresentationChildKind,
        child_ty: Type.TypeVarId,
        child_source_root: ?ConcreteSourceType.ConcreteSourceTypeRoot,
        active: *std.AutoHashMap(Type.TypeVarId, repr.RepRootId),
    ) Allocator.Error!repr.RepRootId {
        const child_root_ty = self.type_importer.output.unlinkConst(child_ty);
        const child_root = if (active.get(child_root_ty)) |existing|
            existing
        else
            self.representation_store.reserveRoot();

        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = parent },
            .to = .{ .local = child_root },
            .kind = .{ .child = kind },
        });

        if (active.get(child_root_ty) == null) {
            const child_source_ty = if (child_source_root) |source| source.key else canonical.CanonicalTypeKey{};
            try self.publishStructuralRootEdgesRec(child_root, child_ty, child_source_ty, child_source_root, active);
        }
        return child_root;
    }

    fn recordShapeForTypeFields(
        self: *BodySolver,
        fields: []const Type.Field,
    ) Allocator.Error!MonoRow.RecordShapeId {
        if (fields.len == 0) return try self.row_shapes.internRecordShapeFromLabels(&.{});
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, fields.len);
        defer self.allocator.free(labels);
        for (fields, 0..) |field, i| labels[i] = field.name;
        return try self.row_shapes.internRecordShapeFromLabels(labels);
    }

    fn recordShapeFieldByLabel(
        self: *BodySolver,
        shape: MonoRow.RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) MonoRow.RecordFieldId {
        for (self.row_shapes.recordShapeFields(shape)) |field| {
            if (self.row_shapes.recordField(field).label == label) return field;
        }
        lambdaInvariant("lambda-solved record shape does not contain requested field label");
    }

    fn tagUnionShapeForTypeTags(
        self: *BodySolver,
        tags: []const Type.Tag,
    ) Allocator.Error!MonoRow.TagUnionShapeId {
        if (tags.len == 0) return try self.row_shapes.internTagUnionShapeFromDescriptors(&.{});
        const descriptors = try self.allocator.alloc(MonoRow.Store.TagShapeDescriptor, tags.len);
        defer self.allocator.free(descriptors);
        for (tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = @intCast(self.type_importer.output.sliceTypeVarSpan(tag.args).len),
            };
        }
        return try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
    }

    fn logicalListElemType(self: *BodySolver, logical_ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const root = self.type_importer.output.unlinkConst(logical_ty);
        return switch (self.type_importer.output.getNode(root)) {
            .nominal => |nominal| try self.logicalListElemType(nominal.backing),
            .content => |content| switch (content) {
                .list => |elem| elem,
                else => lambdaInvariant("lambda-solved list structural root attached to non-list type"),
            },
            else => lambdaInvariant("lambda-solved list structural root attached to unresolved type"),
        };
    }

    fn singlePayloadForTagLabel(
        self: *BodySolver,
        value: repr.ValueInfoId,
        expected_label_text: []const u8,
    ) Allocator.Error!MonoRow.TagPayloadId {
        const info = self.value_store.values.items[@intFromEnum(value)];
        const tags = try self.logicalTagUnionTags(info.logical_ty);
        const shape = try self.tagUnionShapeForTypeTags(tags);
        const shape_tags = self.row_shapes.tagUnionTags(shape);
        if (shape_tags.len != tags.len) {
            lambdaInvariant("lambda-solved tag-union shape arity disagreed with logical type");
        }
        for (tags, shape_tags) |tag, tag_id| {
            if (!std.mem.eql(u8, self.canonical_names.tagLabelText(tag.name), expected_label_text)) continue;
            const payloads = self.row_shapes.tagPayloads(tag_id);
            if (payloads.len != 1) {
                lambdaInvariant("lambda-solved low-level result tag had unexpected payload arity");
            }
            return payloads[0];
        }
        lambdaInvariant("lambda-solved low-level result type did not contain the expected tag");
    }

    fn logicalBoxPayloadType(self: *BodySolver, logical_ty: Type.TypeVarId) Allocator.Error!Type.TypeVarId {
        const root = self.type_importer.output.unlinkConst(logical_ty);
        return switch (self.type_importer.output.getNode(root)) {
            .nominal => |nominal| try self.logicalBoxPayloadType(nominal.backing),
            .content => |content| switch (content) {
                .box => |payload| payload,
                else => lambdaInvariant("lambda-solved const-backed box value attached to non-box type"),
            },
            else => lambdaInvariant("lambda-solved const-backed box value attached to unresolved type"),
        };
    }

    fn logicalTupleItems(self: *BodySolver, logical_ty: Type.TypeVarId) Allocator.Error![]const Type.TypeVarId {
        const root = self.type_importer.output.unlinkConst(logical_ty);
        return switch (self.type_importer.output.getNode(root)) {
            .nominal => |nominal| try self.logicalTupleItems(nominal.backing),
            .content => |content| switch (content) {
                .tuple => |items| self.type_importer.output.sliceTypeVarSpan(items),
                else => lambdaInvariant("lambda-solved tuple structural root attached to non-tuple type"),
            },
            else => lambdaInvariant("lambda-solved tuple structural root attached to unresolved type"),
        };
    }

    fn logicalRecordFields(self: *BodySolver, logical_ty: Type.TypeVarId) Allocator.Error![]const Type.Field {
        const root = self.type_importer.output.unlinkConst(logical_ty);
        return switch (self.type_importer.output.getNode(root)) {
            .nominal => |nominal| try self.logicalRecordFields(nominal.backing),
            .content => |content| switch (content) {
                .record => |record| self.type_importer.output.sliceFields(record.fields),
                else => lambdaInvariant("lambda-solved record pattern root attached to non-record type"),
            },
            else => lambdaInvariant("lambda-solved record pattern root attached to unresolved type"),
        };
    }

    fn logicalTagUnionTags(self: *BodySolver, logical_ty: Type.TypeVarId) Allocator.Error![]const Type.Tag {
        const root = self.type_importer.output.unlinkConst(logical_ty);
        return switch (self.type_importer.output.getNode(root)) {
            .nominal => |nominal| try self.logicalTagUnionTags(nominal.backing),
            .content => |content| switch (content) {
                .tag_union => |tag_union| self.type_importer.output.sliceTags(tag_union.tags),
                else => lambdaInvariant("lambda-solved tag structural root attached to non-tag-union type"),
            },
            else => lambdaInvariant("lambda-solved tag structural root attached to unresolved type"),
        };
    }

    fn logicalTagByLabel(
        _: *BodySolver,
        tags: []const Type.Tag,
        label: canonical.TagLabelId,
    ) Type.Tag {
        for (tags) |tag| {
            if (tag.name == label) return tag;
        }
        lambdaInvariant("lambda-solved tag structural root tag label missing from logical type");
    }

    fn sourcePayloadForKey(
        self: *const BodySolver,
        source_ty: canonical.CanonicalTypeKey,
    ) ?ConcreteSourceType.ConcreteSourceTypeRef {
        if (isEmptyCanonicalTypeKey(source_ty)) return null;
        return self.concrete_source_types.refForKey(source_ty) orelse
            lambdaInvariant("lambda-solved value source type key has no concrete source type payload");
    }

    fn sourceRootForPayload(
        self: *const BodySolver,
        source_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef,
    ) ?ConcreteSourceType.ConcreteSourceTypeRoot {
        const ref = source_ty_payload orelse return null;
        return self.concrete_source_types.root(ref);
    }

    fn sourceChildRoot(
        self: *const BodySolver,
        parent: ?ConcreteSourceType.ConcreteSourceTypeRoot,
        kind: repr.RepresentationChildKind,
    ) Allocator.Error!?ConcreteSourceType.ConcreteSourceTypeRoot {
        const parent_root = parent orelse return null;
        const source = self.sourceViewForRoot(parent_root);
        const child = switch (kind) {
            .nominal_backing => self.sourceNominalBacking(source) orelse return null,
            .list_elem => self.sourceListElem(source) orelse return null,
            .box_payload => self.sourceBoxPayload(source) orelse return null,
            .tuple_elem => |index| self.sourceTupleElem(source, index) orelse return null,
            .record_field => |field| self.sourceRecordField(source, self.row_shapes.recordField(field).label) orelse return null,
            .tag_payload => |payload_id| blk: {
                const payload = self.row_shapes.tagPayload(payload_id);
                const tag = self.row_shapes.tag(payload.tag);
                break :blk self.sourceTagPayload(source, tag.label, payload.logical_index) orelse return null;
            },
            .function_arg => |index| self.sourceFunctionArg(source, index) orelse return null,
            .function_return => self.sourceFunctionReturn(source) orelse return null,
            .function_callable,
            => return null,
        };
        return .{
            .key = checkedTypeRootKey(source.view, child),
            .source = switch (parent_root.source) {
                .artifact => |artifact| .{ .artifact = .{
                    .artifact = artifact.artifact,
                    .ty = child,
                } },
                .local => .{ .local = child },
            },
        };
    }

    fn sourceViewForRoot(
        self: *const BodySolver,
        root: ConcreteSourceType.ConcreteSourceTypeRoot,
    ) ConcreteSourceTypeView {
        return switch (root.source) {
            .local => |local| .{
                .names = self.canonical_names,
                .view = self.concrete_source_types.localView(),
                .root = local,
            },
            .artifact => |artifact| artifactCheckedTypeSourceForArtifactViews(self.artifact_views, artifact.artifact, artifact.ty),
        };
    }

    fn resolvedSourcePayload(
        _: *const BodySolver,
        source: ConcreteSourceTypeView,
    ) ?ConcreteSourceTypeView {
        var current = source;
        while (true) {
            switch (checkedTypePayload(current.view, current.root)) {
                .alias => |alias| current.root = alias.backing,
                else => return current,
            }
        }
    }

    fn sourceNominalBacking(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
    ) ?checked_artifact.CheckedTypeId {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        return switch (checkedTypePayload(resolved.view, resolved.root)) {
            .nominal => |nominal| nominal.backing,
            else => null,
        };
    }

    fn sourceListElem(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
    ) ?checked_artifact.CheckedTypeId {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        return switch (checkedTypePayload(resolved.view, resolved.root)) {
            .nominal => |nominal| blk: {
                if (nominal.builtin != .list or nominal.args.len != 1) return null;
                break :blk nominal.args[0];
            },
            else => null,
        };
    }

    fn sourceBoxPayload(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
    ) ?checked_artifact.CheckedTypeId {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        return switch (checkedTypePayload(resolved.view, resolved.root)) {
            .nominal => |nominal| blk: {
                if (nominal.builtin != .box or nominal.args.len != 1) return null;
                break :blk nominal.args[0];
            },
            else => null,
        };
    }

    fn sourceTupleElem(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
        index: u32,
    ) ?checked_artifact.CheckedTypeId {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        const raw_index: usize = @intCast(index);
        return switch (checkedTypePayload(resolved.view, resolved.root)) {
            .tuple => |items| if (raw_index < items.len) items[raw_index] else null,
            else => null,
        };
    }

    fn sourceRecordField(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
        logical_field: canonical.RecordFieldLabelId,
    ) ?checked_artifact.CheckedTypeId {
        return checked_artifact.checkedTypeRecordFieldChild(
            .{ .names = source.names, .view = source.view },
            source.root,
            self.canonical_names,
            logical_field,
        );
    }

    fn sourceTagPayload(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
        logical_tag: canonical.TagLabelId,
        payload_index: u32,
    ) ?checked_artifact.CheckedTypeId {
        return checked_artifact.checkedTypeTagPayloadChild(
            .{ .names = source.names, .view = source.view },
            source.root,
            self.canonical_names,
            logical_tag,
            payload_index,
        );
    }

    fn sourceFunctionArg(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
        index: u32,
    ) ?checked_artifact.CheckedTypeId {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        const raw_index: usize = @intCast(index);
        return switch (checkedTypePayload(resolved.view, resolved.root)) {
            .function => |function| if (raw_index < function.args.len) function.args[raw_index] else null,
            else => null,
        };
    }

    fn sourceFunctionReturn(
        self: *const BodySolver,
        source: ConcreteSourceTypeView,
    ) ?checked_artifact.CheckedTypeId {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        return switch (checkedTypePayload(resolved.view, resolved.root)) {
            .function => |function| function.ret,
            else => null,
        };
    }

    fn exprValue(self: *const BodySolver, expr: Ast.ExprId) repr.ValueInfoId {
        return self.output.exprs.items[@intFromEnum(expr)].value_info;
    }

    fn valueRoot(self: *const BodySolver, value: repr.ValueInfoId) repr.RepRootId {
        return self.value_store.values.items[@intFromEnum(value)].root;
    }

    fn publishProcedureParamRootKind(
        self: *BodySolver,
        value: repr.ValueInfoId,
        index: u32,
    ) Allocator.Error!void {
        try self.representation_store.replaceRootKind(self.valueRoot(value), .{ .procedure_param = .{
            .instance = self.instance,
            .index = index,
        } });
    }

    fn publishProcedureReturnRootKind(
        self: *BodySolver,
        value: repr.ValueInfoId,
    ) Allocator.Error!void {
        try self.representation_store.replaceRootKind(self.valueRoot(value), .{ .procedure_return = self.instance });
    }

    fn publishProcedureCaptureRootKind(
        self: *BodySolver,
        value: repr.ValueInfoId,
        slot: u32,
    ) Allocator.Error!void {
        try self.representation_store.replaceRootKind(self.valueRoot(value), .{ .procedure_capture = .{
            .instance = self.instance,
            .slot = slot,
        } });
    }

    fn refreshValueStore(self: *BodySolver) void {
        const record = self.registry.procRecord(self.instance);
        self.value_store = &self.registry.program.value_stores.items[@intFromEnum(record.value_store)];
    }
};

fn isEmptyCanonicalTypeKey(key: canonical.CanonicalTypeKey) bool {
    return std.mem.allEqual(u8, key.bytes[0..], 0);
}

fn lambdaInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

fn lambdaInvariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(fmt, args);
    unreachable;
}

test "lambda-solved program owns representation tables" {
    std.testing.refAllDecls(@This());
}
