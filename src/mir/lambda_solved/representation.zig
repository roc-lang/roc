//! Lambda-solved MIR representation/value-flow records.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const row = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");
const type_mod = @import("type.zig");

const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

pub const CallableVarId = type_mod.CallableVarId;
pub const RepRootId = enum(u32) { _ };
pub const ValueInfoId = enum(u32) { _ };
pub const BindingInfoId = enum(u32) { _ };
pub const ProjectionInfoId = enum(u32) { _ };
pub const CallSiteInfoId = enum(u32) { _ };
pub const JoinInfoId = enum(u32) { _ };
pub const ReturnInfoId = enum(u32) { _ };
pub const BoxBoundaryId = canonical.BoxBoundaryId;
pub const CallableValueEmissionPlanId = enum(u32) { _ };
pub const CallableSetConstructionPlanId = enum(u32) { _ };
pub const CanonicalCallableSetDescriptorId = enum(u32) { _ };
pub const ValueTransformBoundaryId = enum(u32) { _ };
pub const SessionExecutableValueTransformId = checked_artifact.SessionExecutableValueTransformId;
pub const SourceMatchId = enum(u32) { _ };
pub const SourceMatchBranchId = enum(u32) { _ };
pub const SourceMatchAlternativeId = enum(u32) { _ };
pub const IfExprId = enum(u32) { _ };
pub const ProcedureBoundaryId = ReturnInfoId;
pub const CaptureBoundaryId = enum(u32) { _ };
pub const MutableJoinId = enum(u32) { _ };
pub const LoopPhiId = enum(u32) { _ };
pub const AggregateBoundaryId = enum(u32) { _ };
pub const RepresentationClassId = enum(u32) { _ };
pub const ProcRepresentationInstanceId = enum(u32) { _ };
pub const RepresentationSolveSessionId = enum(u32) { _ };
pub const ValueInfoStoreId = enum(u32) { _ };
pub const CallableSetMemberId = canonical.CallableSetMemberId;
pub const ErasedAdapterId = enum(u32) { _ };
pub const Symbol = symbol_mod.Symbol;
pub const TypeVarId = type_mod.TypeVarId;

pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }

        pub fn isEmpty(self: @This()) bool {
            return self.len == 0;
        }
    };
}

pub const CanonicalCallableSetKey = canonical.CanonicalCallableSetKey;
pub const CaptureShapeKey = canonical.CaptureShapeKey;
pub const CanonicalExecValueTypeKey = canonical.CanonicalExecValueTypeKey;
pub const ErasedFnAbiKey = canonical.ErasedFnAbiKey;
pub const ErasedFnAbi = canonical.ErasedFnAbi;
pub const ErasedFnAbiStore = canonical.ErasedFnAbiStore;
pub const ErasedFnSigKey = canonical.ErasedFnSigKey;
pub const CallableSetMemberRef = canonical.CallableSetMemberRef;
pub const CallableSetCaptureSlot = canonical.CallableSetCaptureSlot;
pub const CanonicalCallableSetMember = canonical.CanonicalCallableSetMember;
pub const CanonicalCallableSetDescriptor = canonical.CanonicalCallableSetDescriptor;
pub const ExecutablePrimitive = checked_artifact.ExecutablePrimitive;

pub const SessionExecutableTypePayloadId = enum(u32) { _ };

pub const SessionExecutableTypePayloadRef = struct {
    payload: SessionExecutableTypePayloadId,
};

pub const SessionExecutableTypeEndpoint = struct {
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

pub const SessionExecutableTypePayloadChild = struct {
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

pub const SessionExecutableRecordFieldPayload = struct {
    field: row.RecordFieldId,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

pub const SessionExecutableRecordPayload = struct {
    shape: row.RecordShapeId,
    fields: []const SessionExecutableRecordFieldPayload = &.{},
};

pub const SessionExecutableTupleElemPayload = struct {
    index: u32,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

pub const SessionExecutableTagPayload = struct {
    payload: row.TagPayloadId,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

pub const SessionExecutableTagVariantPayload = struct {
    tag: row.TagId,
    payloads: []const SessionExecutableTagPayload = &.{},
};

pub const SessionExecutableTagUnionPayload = struct {
    shape: row.TagUnionShapeId,
    variants: []const SessionExecutableTagVariantPayload = &.{},
};

pub const SessionExecutableNominalPayload = struct {
    nominal: canonical.NominalTypeKey,
    backing: SessionExecutableTypePayloadRef,
    backing_key: CanonicalExecValueTypeKey,
};

pub const SessionExecutableCallableSetMemberPayload = struct {
    member: CallableSetMemberId,
    payload_ty: ?SessionExecutableTypePayloadRef = null,
    payload_ty_key: ?CanonicalExecValueTypeKey = null,
};

pub const SessionExecutableCallableSetPayload = struct {
    key: CanonicalCallableSetKey,
    members: []const SessionExecutableCallableSetMemberPayload = &.{},
};

pub const SessionExecutableErasedFnPayload = struct {
    sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    capture_ty: ?SessionExecutableTypePayloadRef = null,
    capture_ty_key: ?CanonicalExecValueTypeKey = null,
};

pub const SessionExecutableTypePayload = union(enum) {
    pending,
    primitive: ExecutablePrimitive,
    record: SessionExecutableRecordPayload,
    tuple: []const SessionExecutableTupleElemPayload,
    tag_union: SessionExecutableTagUnionPayload,
    list: SessionExecutableTypePayloadChild,
    box: SessionExecutableTypePayloadChild,
    nominal: SessionExecutableNominalPayload,
    callable_set: SessionExecutableCallableSetPayload,
    erased_fn: SessionExecutableErasedFnPayload,
    recursive_ref: SessionExecutableTypePayloadId,
};

pub const SessionExecutableTypePayloadEntry = struct {
    key: CanonicalExecValueTypeKey,
    payload: SessionExecutableTypePayload,
};

pub const SessionExecutableTypePayloadStore = struct {
    entries: []SessionExecutableTypePayloadEntry = &.{},
    by_key: std.AutoHashMap(CanonicalExecValueTypeKey, SessionExecutableTypePayloadId),

    pub fn init(allocator: std.mem.Allocator) SessionExecutableTypePayloadStore {
        return .{
            .by_key = std.AutoHashMap(CanonicalExecValueTypeKey, SessionExecutableTypePayloadId).init(allocator),
        };
    }

    pub fn reserve(
        self: *SessionExecutableTypePayloadStore,
        allocator: std.mem.Allocator,
        key: CanonicalExecValueTypeKey,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadId {
        if (self.by_key.get(key) != null) representationInvariant("session executable type payload reserve saw duplicate key");
        return try self.appendNew(allocator, key, .pending);
    }

    pub fn append(
        self: *SessionExecutableTypePayloadStore,
        allocator: std.mem.Allocator,
        key: CanonicalExecValueTypeKey,
        payload: SessionExecutableTypePayload,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadId {
        if (self.by_key.get(key)) |existing| {
            var duplicate = payload;
            deinitSessionExecutableTypePayload(allocator, &duplicate);
            return existing;
        }
        return try self.appendNew(allocator, key, payload);
    }

    fn appendNew(
        self: *SessionExecutableTypePayloadStore,
        allocator: std.mem.Allocator,
        key: CanonicalExecValueTypeKey,
        payload: SessionExecutableTypePayload,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadId {
        const id: SessionExecutableTypePayloadId = @enumFromInt(@as(u32, @intCast(self.entries.len)));
        const old = self.entries;
        const next = try allocator.alloc(SessionExecutableTypePayloadEntry, old.len + 1);
        errdefer allocator.free(next);
        @memcpy(next[0..old.len], old);
        next[old.len] = .{
            .key = key,
            .payload = payload,
        };
        try self.by_key.put(key, id);
        allocator.free(old);
        self.entries = next;
        return id;
    }

    pub fn fill(
        self: *SessionExecutableTypePayloadStore,
        allocator: std.mem.Allocator,
        id: SessionExecutableTypePayloadId,
        payload: SessionExecutableTypePayload,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) representationInvariant("session executable type payload id out of range");
        deinitSessionExecutableTypePayload(allocator, &self.entries[index].payload);
        self.entries[index].payload = payload;
    }

    pub fn get(self: *const SessionExecutableTypePayloadStore, id: SessionExecutableTypePayloadId) SessionExecutableTypePayload {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) representationInvariant("session executable type payload id out of range");
        return self.entries[index].payload;
    }

    pub fn keyFor(self: *const SessionExecutableTypePayloadStore, id: SessionExecutableTypePayloadId) CanonicalExecValueTypeKey {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) representationInvariant("session executable type payload id out of range");
        return self.entries[index].key;
    }

    pub fn refForKey(self: *const SessionExecutableTypePayloadStore, key: CanonicalExecValueTypeKey) ?SessionExecutableTypePayloadRef {
        const id = self.by_key.get(key) orelse return null;
        return .{ .payload = id };
    }

    pub fn deinit(self: *SessionExecutableTypePayloadStore, allocator: std.mem.Allocator) void {
        for (self.entries) |*entry| deinitSessionExecutableTypePayload(allocator, &entry.payload);
        allocator.free(self.entries);
        self.by_key.deinit();
        self.entries = &.{};
    }
};

fn deinitSessionExecutableTypePayload(
    allocator: std.mem.Allocator,
    payload: *SessionExecutableTypePayload,
) void {
    switch (payload.*) {
        .pending,
        .primitive,
        .list,
        .box,
        .nominal,
        .erased_fn,
        .recursive_ref,
        => {},
        .record => |record| if (record.fields.len > 0) allocator.free(record.fields),
        .tuple => |items| if (items.len > 0) allocator.free(items),
        .tag_union => |tag_union| {
            for (tag_union.variants) |variant| {
                if (variant.payloads.len > 0) allocator.free(variant.payloads);
            }
            if (tag_union.variants.len > 0) allocator.free(tag_union.variants);
        },
        .callable_set => |callable_set| {
            if (callable_set.members.len > 0) allocator.free(callable_set.members);
        },
    }
    payload.* = undefined;
}

pub const CallableMemberInstanceId = struct {
    proc_base: canonical.ProcBaseKeyRef,
    mono_specialization: canonical.MonoSpecializationKey,
    lambda_solved_instance: ProcRepresentationInstanceId,
};

pub const CallableRepresentation = canonical.CallableRepresentation;

pub const CallableReprMode = canonical.CallableReprMode;
pub const ExecutableSpecializationKey = canonical.ExecutableSpecializationKey;

pub const RepresentationShape = union(enum) {
    primitive,
    function: struct {
        fixed_arity: u32,
        args: []const RepRootId,
        ret: RepRootId,
        callable: CallableRepresentation,
    },
    record: struct {
        shape: row.RecordShapeId,
        fields: []const RepRootId,
    },
    tag_union: struct {
        shape: row.TagUnionShapeId,
        payloads: []const RepRootId,
    },
    tuple: []const RepRootId,
    list: RepRootId,
    box: BoxBoundaryId,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: RepRootId,
    },
};

pub const BoxBoundaryDirection = enum {
    box,
    unbox,
};

pub const BoxPayloadRepresentationPlan = union(enum) {
    unchanged,
    function_erased: struct {
        source_fn_ty: canonical.CanonicalTypeKey,
        sig_key: ErasedFnSigKey,
    },
    record: []const BoxPayloadFieldPlan,
    tag_union: []const BoxPayloadTagPlan,
    tuple: []const BoxBoundaryId,
    list: BoxBoundaryId,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        child: BoxBoundaryId,
    },
};

pub const BoxPayloadFieldPlan = struct {
    field: row.RecordFieldId,
    boundary: BoxBoundaryId,
};

pub const BoxPayloadTagPlan = struct {
    tag: row.TagId,
    payloads: []const BoxBoundaryId,
};

pub const BoxBoundary = struct {
    box_ty: canonical.CanonicalTypeKey,
    payload_source_ty: canonical.CanonicalTypeKey,
    payload_boundary_ty: canonical.CanonicalTypeKey,
    direction: BoxBoundaryDirection,
    source_root: RepRootId,
    boundary_root: RepRootId,
    payload_plan: BoxPayloadRepresentationPlan,
};

pub const ProcValueErasePlan = struct {
    source: ValueInfoId,
    proc_value: canonical.ProcedureCallableRef,
    erased_fn_sig_key: ErasedFnSigKey,
    executable_specialization_key: ExecutableSpecializationKey,
    capture_shape_key: CaptureShapeKey,
    capture_slots: []const CallableSetCaptureSlot = &.{},
    provenance: []const BoxBoundaryId,
};

pub const AlreadyErasedCapturePlan = union(enum) {
    none,
    zero_sized_ty: TypeVarId,
    value: ValueInfoId,
};

pub const AlreadyErasedCallablePlan = struct {
    sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    code: canonical.ErasedCallableCodeRef,
    result_ty: CanonicalExecValueTypeKey,
    capture: AlreadyErasedCapturePlan = .none,
    provenance: []const BoxBoundaryId = &.{},
};

pub const ErasedAdapterKey = canonical.ErasedAdapterKey;

pub const FiniteSetErasePlan = struct {
    adapter: ErasedAdapterKey,
    result_ty: CanonicalExecValueTypeKey,
    provenance: []const BoxBoundaryId = &.{},
};

pub const CallableValueEmissionPlan = union(enum) {
    finite: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erase_proc_value: ProcValueErasePlan,
    erase_finite_set: FiniteSetErasePlan,
};

pub const CallableValueSource = union(enum) {
    proc_value: struct {
        proc: canonical.MirProcedureRef,
        captures: []const ValueInfoId,
        fn_ty: canonical.CanonicalTypeKey,
    },
    finite_set: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erased_adapter: ErasedAdapterKey,
};

pub const CallableSetConstructionPlan = struct {
    result: ValueInfoId,
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    selected_member: CallableSetMemberId,
    capture_values: []const ValueInfoId,
};

pub const CallableValueInfo = struct {
    whole_function_root: RepRootId,
    callable_root: RepRootId,
    source: CallableValueSource,
    emission_plan: CallableValueEmissionPlanId,
    construction_plan: ?CallableSetConstructionPlanId = null,
};

pub const BoxedValueInfo = struct {
    box_root: RepRootId,
    payload_root: RepRootId,
    boundary: ?BoxBoundaryId = null,
};

pub const AggregateValueInfo = union(enum) {
    record: struct {
        shape: row.RecordShapeId,
        fields: []const FieldValueInfo,
    },
    tuple: []const ElemValueInfo,
    tag: struct {
        union_shape: row.TagUnionShapeId,
        tag: row.TagId,
        payloads: []const TagPayloadValueInfo,
    },
    list: struct {
        elem_root: RepRootId,
        elems: []const ValueInfoId,
    },
};

pub const SessionExecutableValueEndpointOwner = union(enum) {
    local_value: ValueInfoId,
    procedure_param: struct {
        instance: ProcRepresentationInstanceId,
        index: u32,
    },
    procedure_return: ProcRepresentationInstanceId,
    call_raw_arg: struct {
        call: CallSiteInfoId,
        index: u32,
    },
    call_raw_result: CallSiteInfoId,
    callable_match_branch_raw_result: struct {
        call: CallSiteInfoId,
        member: CallableSetMemberRef,
    },
};

pub const SessionExecutableValueEndpoint = struct {
    owner: SessionExecutableValueEndpointOwner,
    logical_ty: TypeVarId,
    exec_ty: SessionExecutableTypeEndpoint,
};

pub const SessionValueTransformRecordField = struct {
    field: row.RecordFieldId,
    transform: checked_artifact.ExecutableValueTransformRef,
};

pub const SessionValueTransformTupleElem = struct {
    index: u32,
    transform: checked_artifact.ExecutableValueTransformRef,
};

pub const SessionValueTransformTagPayloadEdge = struct {
    source_payload_index: u32,
    target_payload_index: u32,
    transform: checked_artifact.ExecutableValueTransformRef,
};

pub const SessionValueTransformTagCase = struct {
    source_tag: row.TagId,
    target_tag: row.TagId,
    payloads: []const SessionValueTransformTagPayloadEdge = &.{},
};

pub const SessionBoxPayloadTransformPlan = struct {
    boundary: BoxBoundaryId,
    kind: checked_artifact.BoxPayloadTransformKind,
    payload: checked_artifact.ExecutableValueTransformRef,
};

pub const SessionExecutableStructuralBridgePlan = union(enum) {
    direct,
    zst,
    list_reinterpret,
    nominal_reinterpret,
    box_unbox: checked_artifact.ExecutableValueTransformRef,
    box_box: checked_artifact.ExecutableValueTransformRef,
    singleton_to_tag_union: struct {
        source_tag: row.TagId,
        target_tag: row.TagId,
        value_transform: ?checked_artifact.ExecutableValueTransformRef = null,
    },
    tag_union_to_singleton: struct {
        source_tag: row.TagId,
        target_tag: row.TagId,
        value_transform: ?checked_artifact.ExecutableValueTransformRef = null,
    },
};

pub const SessionCallableToErasedTransformPlan = union(enum) {
    finite_value: FiniteSetErasePlan,
    proc_value: ProcValueErasePlan,
};

pub const SessionExecutableValueTransformOp = union(enum) {
    identity,
    structural_bridge: SessionExecutableStructuralBridgePlan,
    record: []const SessionValueTransformRecordField,
    tuple: []const SessionValueTransformTupleElem,
    tag_union: []const SessionValueTransformTagCase,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        backing: checked_artifact.ExecutableValueTransformRef,
    },
    list: struct {
        elem: checked_artifact.ExecutableValueTransformRef,
    },
    box_payload: SessionBoxPayloadTransformPlan,
    callable_to_erased: SessionCallableToErasedTransformPlan,
    already_erased_callable: AlreadyErasedCallablePlan,
};

pub const SessionExecutableValueTransformPlan = struct {
    from: SessionExecutableValueEndpoint,
    to: SessionExecutableValueEndpoint,
    provenance: checked_artifact.ValueTransformProvenance = .none,
    op: SessionExecutableValueTransformOp,
};

pub const SessionExecutableValueTransformStore = struct {
    plans: std.ArrayList(SessionExecutableValueTransformPlan) = .empty,

    pub fn append(
        self: *SessionExecutableValueTransformStore,
        allocator: std.mem.Allocator,
        plan: SessionExecutableValueTransformPlan,
    ) std.mem.Allocator.Error!SessionExecutableValueTransformId {
        const id: SessionExecutableValueTransformId = @enumFromInt(@as(u32, @intCast(self.plans.items.len)));
        var cloned = try cloneSessionExecutableValueTransformPlan(allocator, plan);
        errdefer deinitSessionExecutableValueTransformPlan(allocator, &cloned);
        try self.plans.append(allocator, cloned);
        return id;
    }

    pub fn get(self: *const SessionExecutableValueTransformStore, id: SessionExecutableValueTransformId) SessionExecutableValueTransformPlan {
        const index = @intFromEnum(id);
        if (index >= self.plans.items.len) {
            debug.invariant(false, "lambda-solved invariant violated: session executable value transform id out of range");
            unreachable;
        }
        return self.plans.items[index];
    }

    pub fn deinit(self: *SessionExecutableValueTransformStore, allocator: std.mem.Allocator) void {
        for (self.plans.items) |*plan| deinitSessionExecutableValueTransformPlan(allocator, plan);
        self.plans.deinit(allocator);
        self.* = .{};
    }
};

fn cloneSessionExecutableValueTransformPlan(
    allocator: std.mem.Allocator,
    plan: SessionExecutableValueTransformPlan,
) std.mem.Allocator.Error!SessionExecutableValueTransformPlan {
    var cloned: SessionExecutableValueTransformPlan = .{
        .from = plan.from,
        .to = plan.to,
        .provenance = try cloneValueTransformProvenance(allocator, plan.provenance),
        .op = .identity,
    };
    errdefer deinitSessionExecutableValueTransformPlan(allocator, &cloned);

    cloned.op = try cloneSessionExecutableValueTransformOp(allocator, plan.op);
    return cloned;
}

fn cloneValueTransformProvenance(
    allocator: std.mem.Allocator,
    provenance: checked_artifact.ValueTransformProvenance,
) std.mem.Allocator.Error!checked_artifact.ValueTransformProvenance {
    return switch (provenance) {
        .none => .none,
        .box_erasure => |boundaries| .{
            .box_erasure = if (boundaries.len == 0)
                &.{}
            else
                try allocator.dupe(canonical.BoxBoundaryId, boundaries),
        },
    };
}

fn cloneSessionExecutableValueTransformOp(
    allocator: std.mem.Allocator,
    op: SessionExecutableValueTransformOp,
) std.mem.Allocator.Error!SessionExecutableValueTransformOp {
    return switch (op) {
        .identity => .identity,
        .structural_bridge => |bridge| .{ .structural_bridge = bridge },
        .record => |fields| .{
            .record = if (fields.len == 0)
                &.{}
            else
                try allocator.dupe(SessionValueTransformRecordField, fields),
        },
        .tuple => |items| .{
            .tuple = if (items.len == 0)
                &.{}
            else
                try allocator.dupe(SessionValueTransformTupleElem, items),
        },
        .tag_union => |cases| .{ .tag_union = try cloneSessionValueTransformTagCases(allocator, cases) },
        .nominal => |nominal| .{ .nominal = nominal },
        .list => |list| .{ .list = list },
        .box_payload => |box| .{ .box_payload = box },
        .callable_to_erased => |callable| .{
            .callable_to_erased = try cloneSessionCallableToErasedTransformPlan(allocator, callable),
        },
        .already_erased_callable => |erased| .{
            .already_erased_callable = try cloneAlreadyErasedCallablePlan(allocator, erased),
        },
    };
}

fn cloneSessionValueTransformTagCases(
    allocator: std.mem.Allocator,
    cases: []const SessionValueTransformTagCase,
) std.mem.Allocator.Error![]const SessionValueTransformTagCase {
    if (cases.len == 0) return &.{};
    const cloned = try allocator.alloc(SessionValueTransformTagCase, cases.len);
    @memset(cloned, .{
        .source_tag = @enumFromInt(0),
        .target_tag = @enumFromInt(0),
        .payloads = &.{},
    });
    errdefer {
        for (cloned) |case| {
            if (case.payloads.len > 0) allocator.free(case.payloads);
        }
        allocator.free(cloned);
    }

    for (cases, 0..) |case, i| {
        cloned[i] = .{
            .source_tag = case.source_tag,
            .target_tag = case.target_tag,
            .payloads = if (case.payloads.len == 0)
                &.{}
            else
                try allocator.dupe(SessionValueTransformTagPayloadEdge, case.payloads),
        };
    }
    return cloned;
}

fn cloneSessionCallableToErasedTransformPlan(
    allocator: std.mem.Allocator,
    plan: SessionCallableToErasedTransformPlan,
) std.mem.Allocator.Error!SessionCallableToErasedTransformPlan {
    return switch (plan) {
        .finite_value => |finite| .{ .finite_value = .{
            .adapter = finite.adapter,
            .result_ty = finite.result_ty,
            .provenance = if (finite.provenance.len == 0)
                &.{}
            else
                try allocator.dupe(BoxBoundaryId, finite.provenance),
        } },
        .proc_value => |proc| blk: {
            var key = try cloneExecutableSpecializationKey(allocator, proc.executable_specialization_key);
            errdefer deinitExecutableSpecializationKey(allocator, &key);

            const capture_slots = if (proc.capture_slots.len == 0)
                &.{}
            else
                try allocator.dupe(CallableSetCaptureSlot, proc.capture_slots);
            errdefer if (capture_slots.len > 0) allocator.free(capture_slots);

            const provenance = if (proc.provenance.len == 0)
                &.{}
            else
                try allocator.dupe(BoxBoundaryId, proc.provenance);
            errdefer if (provenance.len > 0) allocator.free(provenance);

            break :blk .{ .proc_value = .{
                .source = proc.source,
                .proc_value = proc.proc_value,
                .erased_fn_sig_key = proc.erased_fn_sig_key,
                .executable_specialization_key = key,
                .capture_shape_key = proc.capture_shape_key,
                .capture_slots = capture_slots,
                .provenance = provenance,
            } };
        },
    };
}

fn cloneAlreadyErasedCallablePlan(
    allocator: std.mem.Allocator,
    erased: AlreadyErasedCallablePlan,
) std.mem.Allocator.Error!AlreadyErasedCallablePlan {
    return .{
        .sig_key = erased.sig_key,
        .capture_shape_key = erased.capture_shape_key,
        .code = erased.code,
        .result_ty = erased.result_ty,
        .capture = erased.capture,
        .provenance = if (erased.provenance.len == 0)
            &.{}
        else
            try allocator.dupe(BoxBoundaryId, erased.provenance),
    };
}

fn deinitSessionExecutableValueTransformPlan(
    allocator: std.mem.Allocator,
    plan: *SessionExecutableValueTransformPlan,
) void {
    switch (plan.provenance) {
        .none => {},
        .box_erasure => |boundaries| if (boundaries.len > 0) allocator.free(boundaries),
    }
    switch (plan.op) {
        .identity,
        .structural_bridge,
        .box_payload,
        => {},
        .record => |fields| if (fields.len > 0) allocator.free(fields),
        .tuple => |items| if (items.len > 0) allocator.free(items),
        .tag_union => |cases| {
            for (cases) |case| {
                if (case.payloads.len > 0) allocator.free(case.payloads);
            }
            if (cases.len > 0) allocator.free(cases);
        },
        .nominal,
        .list,
        => {},
        .callable_to_erased => |callable| {
            var owned = callable;
            deinitSessionCallableToErasedTransformPlan(allocator, &owned);
        },
        .already_erased_callable => |erased| {
            if (erased.provenance.len > 0) allocator.free(erased.provenance);
        },
    }
    plan.* = undefined;
}

fn deinitSessionCallableToErasedTransformPlan(
    allocator: std.mem.Allocator,
    plan: *SessionCallableToErasedTransformPlan,
) void {
    switch (plan.*) {
        .finite_value => |finite| {
            if (finite.provenance.len > 0) allocator.free(finite.provenance);
        },
        .proc_value => |proc| {
            var key = proc.executable_specialization_key;
            deinitExecutableSpecializationKey(allocator, &key);
            if (proc.capture_slots.len > 0) allocator.free(proc.capture_slots);
            if (proc.provenance.len > 0) allocator.free(proc.provenance);
        },
    }
    plan.* = undefined;
}

pub const FieldValueInfo = struct {
    field: row.RecordFieldId,
    value: ValueInfoId,
};

pub const ElemValueInfo = struct {
    index: u32,
    value: ValueInfoId,
};

pub const TagPayloadValueInfo = struct {
    payload: row.TagPayloadId,
    value: ValueInfoId,
};

pub const ValueInfo = struct {
    logical_ty: TypeVarId,
    source_ty: canonical.CanonicalTypeKey,
    root: RepRootId,
    solved_class: ?RepresentationClassId = null,
    callable: ?CallableValueInfo = null,
    boxed: ?BoxedValueInfo = null,
    aggregate: ?AggregateValueInfo = null,
};

pub const BindingInfo = struct {
    symbol: Symbol,
    value: ValueInfoId,
    root: RepRootId,
};

pub const ProjectionKind = union(enum) {
    record_field: row.RecordFieldId,
    tuple_elem: u32,
    tag_payload: row.TagPayloadId,
};

pub const ProjectionInfo = struct {
    source: ValueInfoId,
    result: ValueInfoId,
    root: RepRootId,
    kind: ProjectionKind,
};

pub const CallSiteDispatch = union(enum) {
    call_proc: ProcRepresentationInstanceId,
    call_value_finite: CanonicalCallableSetKey,
    call_value_erased: ErasedFnSigKey,
};

pub const CallSiteInfo = struct {
    callee: ?ValueInfoId,
    args: Span(ValueInfoId),
    result: ValueInfoId,
    requested_fn_root: RepRootId,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    dispatch: CallSiteDispatch,
    arg_transforms: Span(ValueTransformBoundaryId) = Span(ValueTransformBoundaryId).empty(),
    branch_result_transforms: Span(ValueTransformBoundaryId) = Span(ValueTransformBoundaryId).empty(),
    result_transform: ?ValueTransformBoundaryId = null,
};

pub const JoinKind = enum {
    if_expr,
    match_expr,
    loop_expr,
};

pub const JoinInputSource = union(enum) {
    if_branch: struct {
        if_expr: IfExprId,
        branch: IfBranch,
    },
    source_match_branch: struct {
        match: SourceMatchId,
        branch: SourceMatchBranchId,
        alternative: SourceMatchAlternativeId,
    },
    loop_phi: LoopPhiId,
};

pub const JoinInputInfo = struct {
    source: JoinInputSource,
    value: ValueInfoId,
};

pub const JoinInfo = struct {
    result: ValueInfoId,
    inputs: Span(JoinInputInfo),
    root: RepRootId,
    kind: JoinKind,
    input_transforms: Span(ValueTransformBoundaryId) = Span(ValueTransformBoundaryId).empty(),
};

pub const ReturnInfo = struct {
    value: ValueInfoId,
    transform: ?ValueTransformBoundaryId = null,
};

pub const ProcPublicValueRoots = struct {
    params: Span(ValueInfoId),
    ret: ValueInfoId,
    captures: Span(ValueInfoId),
    function_root: RepRootId,
};

pub const IfBranch = enum {
    then_,
    else_,
};

pub const ValueTransformBoundaryKind = union(enum) {
    call_arg: struct {
        call: CallSiteInfoId,
        arg_index: u32,
    },
    call_result: CallSiteInfoId,
    callable_match_branch_result: struct {
        call: CallSiteInfoId,
        member: CallableSetMemberRef,
    },
    source_match_branch_result: struct {
        match: SourceMatchId,
        branch: SourceMatchBranchId,
        alternative: SourceMatchAlternativeId,
    },
    if_branch_result: struct {
        if_expr: IfExprId,
        branch: IfBranch,
    },
    return_value: ProcedureBoundaryId,
    capture_value: CaptureBoundaryId,
    mutable_join: MutableJoinId,
    loop_phi: LoopPhiId,
    aggregate_existing_value: AggregateBoundaryId,
};

pub const ValueTransformBoundary = struct {
    kind: ValueTransformBoundaryKind,
    from_value: ValueInfoId,
    to_value: ValueInfoId,
    from_endpoint: SessionExecutableValueEndpoint,
    to_endpoint: SessionExecutableValueEndpoint,
    transform: checked_artifact.ExecutableValueTransformRef,
};

pub const RepresentationSolveState = enum {
    reserved,
    building,
    solving,
    sealed,
};

pub const RepresentationStore = struct {
    allocator: std.mem.Allocator,
    roots_len: u32 = 0,
    classes_len: u32 = 0,
    callable_emission_plans: []const CallableValueEmissionPlan = &.{},
    callable_construction_plans: []const CallableSetConstructionPlan = &.{},
    callable_set_descriptors: []const CanonicalCallableSetDescriptor = &.{},
    session_executable_type_payloads: SessionExecutableTypePayloadStore,
    erased_fn_abis: ErasedFnAbiStore = .{},
    box_boundaries: []const BoxBoundary = &.{},
    value_transform_boundaries: []const ValueTransformBoundary = &.{},
    session_value_transforms: SessionExecutableValueTransformStore = .{},

    pub fn init(allocator: std.mem.Allocator) RepresentationStore {
        return .{
            .allocator = allocator,
            .session_executable_type_payloads = SessionExecutableTypePayloadStore.init(allocator),
        };
    }

    pub fn deinit(self: *RepresentationStore) void {
        self.session_value_transforms.deinit(self.allocator);
        self.session_executable_type_payloads.deinit(self.allocator);
        self.erased_fn_abis.deinit(self.allocator);
        for (self.callable_emission_plans) |plan| {
            switch (plan) {
                .already_erased => |erased| {
                    if (erased.provenance.len > 0) self.allocator.free(erased.provenance);
                },
                .erase_proc_value => |erase| {
                    var key = erase.executable_specialization_key;
                    deinitExecutableSpecializationKey(self.allocator, &key);
                    if (erase.capture_slots.len > 0) self.allocator.free(erase.capture_slots);
                    if (erase.provenance.len > 0) self.allocator.free(erase.provenance);
                },
                .erase_finite_set => |erase| {
                    if (erase.provenance.len > 0) self.allocator.free(erase.provenance);
                },
                .finite => {},
            }
        }
        if (self.callable_emission_plans.len > 0) self.allocator.free(self.callable_emission_plans);
        for (self.callable_construction_plans) |plan| {
            if (plan.capture_values.len > 0) self.allocator.free(plan.capture_values);
        }
        if (self.callable_construction_plans.len > 0) self.allocator.free(self.callable_construction_plans);
        for (self.callable_set_descriptors) |descriptor| {
            for (descriptor.members) |member| {
                if (member.capture_slots.len > 0) self.allocator.free(member.capture_slots);
            }
            if (descriptor.members.len > 0) self.allocator.free(descriptor.members);
        }
        if (self.callable_set_descriptors.len > 0) self.allocator.free(self.callable_set_descriptors);
        if (self.box_boundaries.len > 0) self.allocator.free(self.box_boundaries);
        if (self.value_transform_boundaries.len > 0) self.allocator.free(self.value_transform_boundaries);
        self.* = RepresentationStore.init(self.allocator);
    }

    pub fn reserveRoot(self: *RepresentationStore) RepRootId {
        const id: RepRootId = @enumFromInt(self.roots_len);
        self.roots_len += 1;
        return id;
    }

    pub fn reserveClass(self: *RepresentationStore) RepresentationClassId {
        const id: RepresentationClassId = @enumFromInt(self.classes_len);
        self.classes_len += 1;
        return id;
    }

    pub fn appendBoxBoundary(
        self: *RepresentationStore,
        allocator: std.mem.Allocator,
        boundary: BoxBoundary,
    ) std.mem.Allocator.Error!BoxBoundaryId {
        const id: BoxBoundaryId = @enumFromInt(@as(u32, @intCast(self.box_boundaries.len)));
        const old = self.box_boundaries;
        const next = try allocator.alloc(BoxBoundary, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = boundary;
        allocator.free(old);
        self.box_boundaries = next;
        return id;
    }

    pub fn appendValueTransformBoundary(
        self: *RepresentationStore,
        boundary: ValueTransformBoundary,
    ) std.mem.Allocator.Error!ValueTransformBoundaryId {
        const id: ValueTransformBoundaryId = @enumFromInt(@as(u32, @intCast(self.value_transform_boundaries.len)));
        const old = self.value_transform_boundaries;
        const next = try self.allocator.alloc(ValueTransformBoundary, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = boundary;
        if (old.len > 0) self.allocator.free(old);
        self.value_transform_boundaries = next;
        return id;
    }

    pub fn appendSessionExecutableValueTransform(
        self: *RepresentationStore,
        plan: SessionExecutableValueTransformPlan,
    ) std.mem.Allocator.Error!SessionExecutableValueTransformId {
        return try self.session_value_transforms.append(self.allocator, plan);
    }

    pub fn sessionExecutableValueTransform(
        self: *const RepresentationStore,
        id: SessionExecutableValueTransformId,
    ) SessionExecutableValueTransformPlan {
        return self.session_value_transforms.get(id);
    }

    pub fn callableEmissionPlan(
        self: *const RepresentationStore,
        id: CallableValueEmissionPlanId,
    ) CallableValueEmissionPlan {
        return self.callable_emission_plans[@intFromEnum(id)];
    }

    pub fn callableConstructionPlan(
        self: *const RepresentationStore,
        id: CallableSetConstructionPlanId,
    ) CallableSetConstructionPlan {
        return self.callable_construction_plans[@intFromEnum(id)];
    }

    pub fn valueTransformBoundary(
        self: *const RepresentationStore,
        id: ValueTransformBoundaryId,
    ) ValueTransformBoundary {
        return self.value_transform_boundaries[@intFromEnum(id)];
    }

    pub fn verifySealed(self: *const RepresentationStore) void {
        if (@import("builtin").mode != .Debug) return;
        self.erased_fn_abis.verifyPublished();
        for (self.erased_fn_abis.abis) |abi| {
            if (self.session_executable_type_payloads.refForKey(abi.ret_exec_key) == null) {
                debug.invariant(false, "lambda-solved invariant violated: erased ABI result key has no session executable type payload");
            }
            for (abi.arg_exec_keys) |arg_key| {
                if (self.session_executable_type_payloads.refForKey(arg_key) == null) {
                    debug.invariant(false, "lambda-solved invariant violated: erased ABI argument key has no session executable type payload");
                }
            }
        }
    }

    pub fn callableSetDescriptor(
        self: *const RepresentationStore,
        key: CanonicalCallableSetKey,
    ) ?*const CanonicalCallableSetDescriptor {
        for (self.callable_set_descriptors) |*descriptor| {
            if (callableSetKeyEql(descriptor.key, key)) return descriptor;
        }
        return null;
    }

    pub fn callableSetMember(
        self: *const RepresentationStore,
        key: CanonicalCallableSetKey,
        member_id: CallableSetMemberId,
    ) ?*const CanonicalCallableSetMember {
        const descriptor = self.callableSetDescriptor(key) orelse return null;
        for (descriptor.members) |*member| {
            if (member.member == member_id) return member;
        }
        return null;
    }

    pub fn addSingletonProcValueCallable(
        self: *RepresentationStore,
        names: *const canonical.CanonicalNameStore,
        types: *const type_mod.Store,
        value_store: *const ValueInfoStore,
        result: ValueInfoId,
        whole_function_root: RepRootId,
        proc: canonical.MirProcedureRef,
        capture_values: []const ValueInfoId,
    ) std.mem.Allocator.Error!CallableValueInfo {
        const source_fn_ty = proc.callable.source_fn_ty;
        const capture_shape_key = try captureShapeKeyForValueSlice(
            self.allocator,
            names,
            types,
            self,
            value_store,
            capture_values,
        );
        const capture_slots = try self.captureSlotsForValues(
            names,
            types,
            value_store,
            capture_values,
        );
        var descriptor_owned = false;
        errdefer if (!descriptor_owned and capture_slots.len > 0) self.allocator.free(capture_slots);

        const proc_callable = proc.callable;
        const callable_set_key = singletonCallableSetKey(proc_callable, capture_shape_key, capture_slots);
        const member_id: CallableSetMemberId = @enumFromInt(0);
        if (self.callableSetDescriptor(callable_set_key) == null) {
            const members = try self.allocator.dupe(CanonicalCallableSetMember, &.{.{
                .member = member_id,
                .proc_value = proc_callable,
                .source_proc = proc,
                .capture_slots = capture_slots,
                .capture_shape_key = capture_shape_key,
            }});
            errdefer if (!descriptor_owned) self.allocator.free(members);
            try self.appendCallableSetDescriptor(.{
                .key = callable_set_key,
                .members = members,
            });
            descriptor_owned = true;
        } else {
            if (capture_slots.len > 0) self.allocator.free(capture_slots);
            descriptor_owned = true;
        }
        const emission_plan = try self.appendCallableEmissionPlan(.{ .finite = callable_set_key });
        const construction_plan = try self.appendCallableConstructionPlan(.{
            .result = result,
            .source_fn_ty = source_fn_ty,
            .callable_set_key = callable_set_key,
            .selected_member = member_id,
            .capture_values = capture_values,
        });
        const construction = self.callableConstructionPlan(construction_plan);
        return .{
            .whole_function_root = whole_function_root,
            .callable_root = self.reserveRoot(),
            .source = .{ .proc_value = .{
                .proc = proc,
                .captures = construction.capture_values,
                .fn_ty = source_fn_ty,
            } },
            .emission_plan = emission_plan,
            .construction_plan = construction_plan,
        };
    }

    fn appendCallableEmissionPlan(
        self: *RepresentationStore,
        plan: CallableValueEmissionPlan,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        const old = self.callable_emission_plans;
        const next = try self.allocator.alloc(CallableValueEmissionPlan, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) self.allocator.free(old);
        const id: CallableValueEmissionPlanId = @enumFromInt(@as(u32, @intCast(old.len)));
        next[old.len] = plan;
        self.callable_emission_plans = next;
        return id;
    }

    fn appendCallableConstructionPlan(
        self: *RepresentationStore,
        plan: CallableSetConstructionPlan,
    ) std.mem.Allocator.Error!CallableSetConstructionPlanId {
        const old = self.callable_construction_plans;
        const next = try self.allocator.alloc(CallableSetConstructionPlan, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) self.allocator.free(old);
        const id: CallableSetConstructionPlanId = @enumFromInt(@as(u32, @intCast(old.len)));
        next[old.len] = .{
            .result = plan.result,
            .source_fn_ty = plan.source_fn_ty,
            .callable_set_key = plan.callable_set_key,
            .selected_member = plan.selected_member,
            .capture_values = if (plan.capture_values.len == 0)
                &.{}
            else
                try self.allocator.dupe(ValueInfoId, plan.capture_values),
        };
        self.callable_construction_plans = next;
        return id;
    }

    fn appendCallableSetDescriptor(
        self: *RepresentationStore,
        descriptor: CanonicalCallableSetDescriptor,
    ) std.mem.Allocator.Error!void {
        const old = self.callable_set_descriptors;
        const next = try self.allocator.alloc(CanonicalCallableSetDescriptor, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) self.allocator.free(old);
        next[old.len] = descriptor;
        self.callable_set_descriptors = next;
    }

    fn captureSlotsForValues(
        self: *RepresentationStore,
        names: *const canonical.CanonicalNameStore,
        types: *const type_mod.Store,
        value_store: *const ValueInfoStore,
        values: []const ValueInfoId,
    ) std.mem.Allocator.Error![]const CallableSetCaptureSlot {
        if (values.len == 0) return &.{};
        const slots = try self.allocator.alloc(CallableSetCaptureSlot, values.len);
        errdefer self.allocator.free(slots);
        for (values, 0..) |value, i| {
            const value_info = value_store.values.items[@intFromEnum(value)];
            if (isEmptyCanonicalTypeKey(value_info.source_ty)) {
                representationInvariant("lambda-solved capture slot reached callable-set construction without an explicit source type key");
            }
            const exec_key = try execValueTypeKeyForValue(self.allocator, names, types, self, value_store, value);
            slots[i] = .{
                .slot = @intCast(i),
                .source_ty = value_info.source_ty,
                .exec_value_ty = exec_key,
            };
        }
        return slots;
    }

    pub fn verifyCallableConstructionPlan(
        self: *const RepresentationStore,
        value_id: ValueInfoId,
        value_info: ValueInfo,
    ) void {
        const callable = value_info.callable orelse {
            debug.invariant(false, "lambda-solved invariant violated: callable construction value has no callable metadata");
            return;
        };
        const construction_id = callable.construction_plan orelse {
            debug.invariant(false, "lambda-solved invariant violated: finite callable construction has no construction plan");
            return;
        };
        const construction = self.callableConstructionPlan(construction_id);
        debug.invariant(construction.result == value_id, "lambda-solved invariant violated: callable construction plan is attached to the wrong value");
        const emission_plan = self.callableEmissionPlan(callable.emission_plan);
        const emission_key = switch (emission_plan) {
            .finite => |key| key,
            else => {
                debug.invariant(false, "lambda-solved invariant violated: callable construction does not have finite emission");
                return;
            },
        };
        debug.invariant(
            callableSetKeyEql(emission_key, construction.callable_set_key),
            "lambda-solved invariant violated: callable construction key differs from finite emission key",
        );
        const member = self.callableSetMember(construction.callable_set_key, construction.selected_member) orelse {
            debug.invariant(false, "lambda-solved invariant violated: callable construction selects missing member");
            return;
        };
        debug.invariant(
            canonicalTypeKeyEql(member.proc_value.source_fn_ty, construction.source_fn_ty),
            "lambda-solved invariant violated: callable construction source function type differs from descriptor member",
        );
        debug.invariant(
            member.capture_slots.len == construction.capture_values.len,
            "lambda-solved invariant violated: callable construction capture count differs from descriptor member",
        );
        for (member.capture_slots, 0..) |slot, i| {
            debug.invariant(
                slot.slot == @as(u32, @intCast(i)),
                "lambda-solved invariant violated: callable capture slots are not canonical",
            );
        }
    }
};

pub const ValueInfoStore = struct {
    allocator: std.mem.Allocator,
    values: std.ArrayList(ValueInfo),
    bindings: std.ArrayList(BindingInfo),
    projections: std.ArrayList(ProjectionInfo),
    call_sites: std.ArrayList(CallSiteInfo),
    joins: std.ArrayList(JoinInfo),
    returns: std.ArrayList(ReturnInfo),
    join_inputs: std.ArrayList(JoinInputInfo),
    value_ids: std.ArrayList(ValueInfoId),
    value_transform_boundary_ids: std.ArrayList(ValueTransformBoundaryId),

    pub fn init(allocator: std.mem.Allocator) ValueInfoStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .bindings = .empty,
            .projections = .empty,
            .call_sites = .empty,
            .joins = .empty,
            .returns = .empty,
            .join_inputs = .empty,
            .value_ids = .empty,
            .value_transform_boundary_ids = .empty,
        };
    }

    pub fn deinit(self: *ValueInfoStore) void {
        for (self.values.items) |value| {
            if (value.aggregate) |aggregate| {
                switch (aggregate) {
                    .record => |record| if (record.fields.len > 0) self.allocator.free(record.fields),
                    .tuple => |elems| if (elems.len > 0) self.allocator.free(elems),
                    .tag => |tag| if (tag.payloads.len > 0) self.allocator.free(tag.payloads),
                    .list => |list| if (list.elems.len > 0) self.allocator.free(list.elems),
                }
            }
        }
        self.value_ids.deinit(self.allocator);
        self.value_transform_boundary_ids.deinit(self.allocator);
        self.join_inputs.deinit(self.allocator);
        self.returns.deinit(self.allocator);
        self.joins.deinit(self.allocator);
        self.call_sites.deinit(self.allocator);
        self.projections.deinit(self.allocator);
        self.bindings.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.* = ValueInfoStore.init(self.allocator);
    }

    pub fn addValue(self: *ValueInfoStore, value: ValueInfo) std.mem.Allocator.Error!ValueInfoId {
        const id: ValueInfoId = @enumFromInt(@as(u32, @intCast(self.values.items.len)));
        try self.values.append(self.allocator, value);
        return id;
    }

    pub fn addBinding(self: *ValueInfoStore, binding: BindingInfo) std.mem.Allocator.Error!BindingInfoId {
        const id: BindingInfoId = @enumFromInt(@as(u32, @intCast(self.bindings.items.len)));
        try self.bindings.append(self.allocator, binding);
        return id;
    }

    pub fn addProjection(self: *ValueInfoStore, projection: ProjectionInfo) std.mem.Allocator.Error!ProjectionInfoId {
        const id: ProjectionInfoId = @enumFromInt(@as(u32, @intCast(self.projections.items.len)));
        try self.projections.append(self.allocator, projection);
        return id;
    }

    pub fn addCallSite(self: *ValueInfoStore, call_site: CallSiteInfo) std.mem.Allocator.Error!CallSiteInfoId {
        const id: CallSiteInfoId = @enumFromInt(@as(u32, @intCast(self.call_sites.items.len)));
        try self.call_sites.append(self.allocator, call_site);
        return id;
    }

    pub fn addJoin(self: *ValueInfoStore, join: JoinInfo) std.mem.Allocator.Error!JoinInfoId {
        const id: JoinInfoId = @enumFromInt(@as(u32, @intCast(self.joins.items.len)));
        try self.joins.append(self.allocator, join);
        return id;
    }

    pub fn addReturn(self: *ValueInfoStore, ret: ReturnInfo) std.mem.Allocator.Error!ReturnInfoId {
        const id: ReturnInfoId = @enumFromInt(@as(u32, @intCast(self.returns.items.len)));
        try self.returns.append(self.allocator, ret);
        return id;
    }

    pub fn addJoinInputSpan(self: *ValueInfoStore, inputs: []const JoinInputInfo) std.mem.Allocator.Error!Span(JoinInputInfo) {
        if (inputs.len == 0) return Span(JoinInputInfo).empty();
        const start: u32 = @intCast(self.join_inputs.items.len);
        try self.join_inputs.appendSlice(self.allocator, inputs);
        return .{ .start = start, .len = @intCast(inputs.len) };
    }

    pub fn sliceJoinInputSpan(self: *const ValueInfoStore, span: Span(JoinInputInfo)) []const JoinInputInfo {
        if (span.len == 0) return &.{};
        return self.join_inputs.items[span.start..][0..span.len];
    }

    pub fn addValueSpan(self: *ValueInfoStore, values: []const ValueInfoId) std.mem.Allocator.Error!Span(ValueInfoId) {
        if (values.len == 0) return Span(ValueInfoId).empty();
        const start: u32 = @intCast(self.value_ids.items.len);
        try self.value_ids.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceValueSpan(self: *const ValueInfoStore, span: Span(ValueInfoId)) []const ValueInfoId {
        if (span.len == 0) return &.{};
        return self.value_ids.items[span.start..][0..span.len];
    }

    pub fn addValueTransformBoundarySpan(
        self: *ValueInfoStore,
        boundaries: []const ValueTransformBoundaryId,
    ) std.mem.Allocator.Error!Span(ValueTransformBoundaryId) {
        if (boundaries.len == 0) return Span(ValueTransformBoundaryId).empty();
        const start: u32 = @intCast(self.value_transform_boundary_ids.items.len);
        try self.value_transform_boundary_ids.appendSlice(self.allocator, boundaries);
        return .{ .start = start, .len = @intCast(boundaries.len) };
    }

    pub fn sliceValueTransformBoundarySpan(
        self: *const ValueInfoStore,
        span: Span(ValueTransformBoundaryId),
    ) []const ValueTransformBoundaryId {
        if (span.len == 0) return &.{};
        return self.value_transform_boundary_ids.items[span.start..][0..span.len];
    }
};

pub fn canonicalTypeKeyEql(a: canonical.CanonicalTypeKey, b: canonical.CanonicalTypeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

fn isEmptyCanonicalTypeKey(key: canonical.CanonicalTypeKey) bool {
    for (key.bytes) |byte| {
        if (byte != 0) return false;
    }
    return true;
}

pub fn callableSetKeyEql(a: CanonicalCallableSetKey, b: CanonicalCallableSetKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

pub fn canonicalExecValueTypeKeyEql(a: CanonicalExecValueTypeKey, b: CanonicalExecValueTypeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

pub fn captureShapeKeyEql(a: CaptureShapeKey, b: CaptureShapeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

pub fn erasedFnSigKeyEql(a: ErasedFnSigKey, b: ErasedFnSigKey) bool {
    if (!canonicalTypeKeyEql(a.source_fn_ty, b.source_fn_ty)) return false;
    if (!std.mem.eql(u8, a.abi.bytes[0..], b.abi.bytes[0..])) return false;
    if (a.capture_ty) |a_capture| {
        const b_capture = b.capture_ty orelse return false;
        return canonicalExecValueTypeKeyEql(a_capture, b_capture);
    }
    return b.capture_ty == null;
}

pub const RepresentationSolveSession = struct {
    members: []const ProcRepresentationInstanceId,
    representation_store: RepresentationStore,
    state: RepresentationSolveState,

    pub fn deinit(self: *RepresentationSolveSession) void {
        self.representation_store.deinit();
    }
};

pub const ProcRepresentationInstance = struct {
    proc: canonical.MirProcedureRef,
    executable_specialization_key: ExecutableSpecializationKey,
    solve_session: RepresentationSolveSessionId,
    value_store: ValueInfoStoreId,
    public_roots: ProcPublicValueRoots,
};

pub fn executableSpecializationKeyForProc(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    proc: canonical.MirProcedureRef,
    roots: ProcPublicValueRoots,
) std.mem.Allocator.Error!ExecutableSpecializationKey {
    const params = value_store.sliceValueSpan(roots.params);
    const arg_keys = if (params.len == 0)
        &.{}
    else
        try allocator.alloc(CanonicalExecValueTypeKey, params.len);
    errdefer if (arg_keys.len > 0) allocator.free(arg_keys);
    for (params, 0..) |param, i| {
        arg_keys[i] = try execValueTypeKeyForValue(allocator, names, types, representation_store, value_store, param);
    }

    return .{
        .base = proc.proc.proc_base,
        .requested_fn_ty = proc.callable.source_fn_ty,
        .exec_arg_tys = arg_keys,
        .exec_ret_ty = try execValueTypeKeyForValue(allocator, names, types, representation_store, value_store, roots.ret),
        .callable_repr_mode = .direct,
        .capture_shape_key = try captureShapeKeyForValues(allocator, names, types, representation_store, value_store, roots.captures),
    };
}

pub fn deinitExecutableSpecializationKey(
    allocator: std.mem.Allocator,
    key: *ExecutableSpecializationKey,
) void {
    if (key.exec_arg_tys.len > 0) allocator.free(key.exec_arg_tys);
    key.exec_arg_tys = &.{};
}

pub fn cloneExecutableSpecializationKey(
    allocator: std.mem.Allocator,
    key: ExecutableSpecializationKey,
) std.mem.Allocator.Error!ExecutableSpecializationKey {
    return .{
        .base = key.base,
        .requested_fn_ty = key.requested_fn_ty,
        .exec_arg_tys = if (key.exec_arg_tys.len == 0)
            &.{}
        else
            try allocator.dupe(CanonicalExecValueTypeKey, key.exec_arg_tys),
        .exec_ret_ty = key.exec_ret_ty,
        .callable_repr_mode = key.callable_repr_mode,
        .capture_shape_key = key.capture_shape_key,
    };
}

pub fn executableSpecializationKeyEql(a: ExecutableSpecializationKey, b: ExecutableSpecializationKey) bool {
    if (a.base != b.base) return false;
    if (!canonicalTypeKeyEql(a.requested_fn_ty, b.requested_fn_ty)) return false;
    if (a.exec_arg_tys.len != b.exec_arg_tys.len) return false;
    for (a.exec_arg_tys, b.exec_arg_tys) |a_arg, b_arg| {
        if (!canonicalExecValueTypeKeyEql(a_arg, b_arg)) return false;
    }
    if (!canonicalExecValueTypeKeyEql(a.exec_ret_ty, b.exec_ret_ty)) return false;
    if (a.callable_repr_mode != b.callable_repr_mode) return false;
    return captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
}

pub fn deinitProcRepresentationInstance(
    allocator: std.mem.Allocator,
    instance: *ProcRepresentationInstance,
) void {
    deinitExecutableSpecializationKey(allocator, &instance.executable_specialization_key);
}

pub fn execValueTypeKeyForValue(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    value: ValueInfoId,
) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
    var builder = ExecValueTypeKeyBuilder.initForValues(allocator, names, types, representation_store, value_store);
    defer builder.deinit();
    return try builder.keyForValue(value);
}

pub fn execValueTypeKey(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    root: type_mod.TypeVarId,
) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
    var builder = ExecValueTypeKeyBuilder.init(allocator, names, types);
    defer builder.deinit();
    return try builder.key(root);
}

pub fn captureShapeKeyForValues(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    values: Span(ValueInfoId),
) std.mem.Allocator.Error!CaptureShapeKey {
    return try captureShapeKeyForValueSlice(
        allocator,
        names,
        types,
        representation_store,
        value_store,
        value_store.sliceValueSpan(values),
    );
}

pub fn captureShapeKeyForValueSlice(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    values: []const ValueInfoId,
) std.mem.Allocator.Error!CaptureShapeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "capture_shape");
    writeHashU32(&hasher, @intCast(values.len));
    for (values, 0..) |capture, i| {
        writeHashU32(&hasher, @intCast(i));
        const key = try execValueTypeKeyForValue(allocator, names, types, representation_store, value_store, capture);
        hasher.update(&key.bytes);
    }
    return .{ .bytes = hasher.finalResult() };
}

pub fn sessionExecutableTypeEndpointForValue(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *RepresentationStore,
    value_store: *const ValueInfoStore,
    value: ValueInfoId,
) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
    var builder = SessionExecutableTypePayloadBuilder.init(allocator, names, row_shapes, types, representation_store, value_store);
    defer builder.deinit();
    return try builder.endpointForValue(value);
}

pub fn sessionExecutableTypeEndpointForValueIntoStore(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    source_representation_store: *const RepresentationStore,
    owner_payload_store: *SessionExecutableTypePayloadStore,
    value_store: *const ValueInfoStore,
    value: ValueInfoId,
) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
    var builder = SessionExecutableTypePayloadBuilder.initWithPayloadStore(
        allocator,
        names,
        row_shapes,
        types,
        source_representation_store,
        owner_payload_store,
        value_store,
    );
    defer builder.deinit();
    return try builder.endpointForValue(value);
}

pub fn sessionExecutableTypeEndpointForType(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *RepresentationStore,
    ty: type_mod.TypeVarId,
) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
    var builder = SessionExecutableTypePayloadBuilder.init(allocator, names, row_shapes, types, representation_store, null);
    defer builder.deinit();
    return try builder.endpointForType(ty);
}

const SessionExecutableTypePayloadBuilder = struct {
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    payload_store: *SessionExecutableTypePayloadStore,
    value_store: ?*const ValueInfoStore,
    active_types: std.AutoHashMap(type_mod.TypeVarId, SessionExecutableTypePayloadId),
    active_values: std.AutoHashMap(ValueInfoId, SessionExecutableTypePayloadId),

    fn init(
        allocator: std.mem.Allocator,
        names: *const canonical.CanonicalNameStore,
        row_shapes: *row.Store,
        types: *const type_mod.Store,
        representation_store: *RepresentationStore,
        value_store: ?*const ValueInfoStore,
    ) SessionExecutableTypePayloadBuilder {
        return initWithPayloadStore(
            allocator,
            names,
            row_shapes,
            types,
            representation_store,
            &representation_store.session_executable_type_payloads,
            value_store,
        );
    }

    fn initWithPayloadStore(
        allocator: std.mem.Allocator,
        names: *const canonical.CanonicalNameStore,
        row_shapes: *row.Store,
        types: *const type_mod.Store,
        representation_store: *const RepresentationStore,
        payload_store: *SessionExecutableTypePayloadStore,
        value_store: ?*const ValueInfoStore,
    ) SessionExecutableTypePayloadBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .row_shapes = row_shapes,
            .types = types,
            .representation_store = representation_store,
            .payload_store = payload_store,
            .value_store = value_store,
            .active_types = std.AutoHashMap(type_mod.TypeVarId, SessionExecutableTypePayloadId).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, SessionExecutableTypePayloadId).init(allocator),
        };
    }

    fn deinit(self: *SessionExecutableTypePayloadBuilder) void {
        self.active_values.deinit();
        self.active_types.deinit();
    }

    fn refFor(id: SessionExecutableTypePayloadId) SessionExecutableTypePayloadRef {
        return .{ .payload = id };
    }

    fn endpointForValue(self: *SessionExecutableTypePayloadBuilder, value: ValueInfoId) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const values = self.value_store orelse representationInvariant("session executable type endpoint for value has no value store");
        const key = try execValueTypeKeyForValue(
            self.allocator,
            self.names,
            self.types,
            self.representation_store,
            values,
            value,
        );
        if (self.active_values.get(value)) |active| {
            return .{ .ty = refFor(active), .key = key };
        }
        if (self.payload_store.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }

        const id = try self.payload_store.reserve(self.allocator, key);
        try self.active_values.put(value, id);
        errdefer _ = self.active_values.remove(value);

        const info = values.values.items[@intFromEnum(value)];
        const payload = if (info.callable) |callable|
            try self.callablePayload(callable)
        else if (info.aggregate) |aggregate|
            try self.aggregatePayload(info.logical_ty, aggregate)
        else
            try self.typePayload(info.logical_ty);

        self.payload_store.fill(self.allocator, id, payload);
        _ = self.active_values.remove(value);
        return .{ .ty = refFor(id), .key = key };
    }

    fn endpointForType(self: *SessionExecutableTypePayloadBuilder, ty: type_mod.TypeVarId) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const key = try execValueTypeKey(self.allocator, self.names, self.types, ty);
        const root = self.types.unlinkConst(ty);
        if (self.active_types.get(root)) |active| {
            return .{ .ty = refFor(active), .key = key };
        }
        if (self.payload_store.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }

        const id = try self.payload_store.reserve(self.allocator, key);
        try self.active_types.put(root, id);
        errdefer _ = self.active_types.remove(root);

        const payload = try self.typePayload(root);
        self.payload_store.fill(self.allocator, id, payload);
        _ = self.active_types.remove(root);
        return .{ .ty = refFor(id), .key = key };
    }

    fn childForType(self: *SessionExecutableTypePayloadBuilder, ty: type_mod.TypeVarId) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const child = try self.endpointForType(ty);
        return .{ .ty = child.ty, .key = child.key };
    }

    fn childForValue(self: *SessionExecutableTypePayloadBuilder, value: ValueInfoId) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const child = try self.endpointForValue(value);
        return .{ .ty = child.ty, .key = child.key };
    }

    fn typePayload(self: *SessionExecutableTypePayloadBuilder, ty: type_mod.TypeVarId) std.mem.Allocator.Error!SessionExecutableTypePayload {
        const root = self.types.unlinkConst(ty);
        return switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("session executable type payload reached unresolved lambda-solved type"),
            .nominal => |nominal| blk: {
                const backing = try self.endpointForType(nominal.backing);
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .backing = backing.ty,
                    .backing_key = backing.key,
                } };
            },
            .content => |content| try self.contentPayload(content),
        };
    }

    fn contentPayload(self: *SessionExecutableTypePayloadBuilder, content: type_mod.Content) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (content) {
            .primitive => |prim| .{ .primitive = executablePrimitive(prim) },
            .list => |elem| .{ .list = try self.childForType(elem) },
            .box => |elem| .{ .box = try self.childForType(elem) },
            .tuple => |span| .{ .tuple = try self.tuplePayloadForTypeSpan(span) },
            .record => |record| .{ .record = try self.recordPayloadForTypeSpan(record.fields) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagUnionPayloadForTypeSpan(tag_union.tags) },
            .func => representationInvariant("session executable type payload requires callable value metadata for function type"),
        };
    }

    fn tuplePayloadForTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        span: type_mod.Span(type_mod.TypeVarId),
    ) std.mem.Allocator.Error![]const SessionExecutableTupleElemPayload {
        const items = self.types.sliceTypeVarSpan(span);
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(SessionExecutableTupleElemPayload, items.len);
        errdefer self.allocator.free(out);
        for (items, 0..) |item, i| {
            const child = try self.endpointForType(item);
            out[i] = .{
                .index = @intCast(i),
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn recordPayloadForTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        span: type_mod.Span(type_mod.Field),
    ) std.mem.Allocator.Error!SessionExecutableRecordPayload {
        const fields = self.types.sliceFields(span);
        if (fields.len == 0) {
            const shape = try self.row_shapes.internRecordShapeFromLabels(&.{});
            return .{ .shape = shape, .fields = &.{} };
        }

        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, fields.len);
        defer self.allocator.free(labels);
        for (fields, 0..) |field, i| labels[i] = field.name;

        const shape = try self.row_shapes.internRecordShapeFromLabels(labels);
        const shape_fields = self.row_shapes.recordShapeFields(shape);
        if (shape_fields.len != fields.len) representationInvariant("session executable record payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableRecordFieldPayload, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            const child = try self.endpointForType(field.ty);
            out[i] = .{
                .field = shape_fields[i],
                .ty = child.ty,
                .key = child.key,
            };
        }
        return .{ .shape = shape, .fields = out };
    }

    fn tagUnionPayloadForTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        span: type_mod.Span(type_mod.Tag),
    ) std.mem.Allocator.Error!SessionExecutableTagUnionPayload {
        const tags = self.types.sliceTags(span);
        if (tags.len == 0) {
            const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(&.{});
            return .{ .shape = shape, .variants = &.{} };
        }

        const descriptors = try self.allocator.alloc(row.Store.TagShapeDescriptor, tags.len);
        defer self.allocator.free(descriptors);
        for (tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = @intCast(self.types.sliceTypeVarSpan(tag.args).len),
            };
        }

        const shape = try self.row_shapes.internTagUnionShapeFromDescriptors(descriptors);
        const shape_tags = self.row_shapes.tagUnionTags(shape);
        if (shape_tags.len != tags.len) representationInvariant("session executable tag payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagVariantPayload, tags.len);
        for (out) |*variant| variant.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }

        for (tags, 0..) |tag, i| {
            out[i] = .{
                .tag = shape_tags[i],
                .payloads = try self.tagPayloadsForTypeSpan(shape_tags[i], tag.args),
            };
        }
        return .{ .shape = shape, .variants = out };
    }

    fn tagPayloadsForTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        tag: row.TagId,
        span: type_mod.Span(type_mod.TypeVarId),
    ) std.mem.Allocator.Error![]const SessionExecutableTagPayload {
        const args = self.types.sliceTypeVarSpan(span);
        if (args.len == 0) return &.{};
        const shape_payloads = self.row_shapes.tagPayloads(tag);
        if (shape_payloads.len != args.len) representationInvariant("session executable tag payload arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagPayload, args.len);
        errdefer self.allocator.free(out);
        for (args, 0..) |arg, i| {
            const child = try self.endpointForType(arg);
            out[i] = .{
                .payload = shape_payloads[i],
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn callablePayload(self: *SessionExecutableTypePayloadBuilder, callable: CallableValueInfo) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .finite => |key| .{ .callable_set = try self.callableSetPayload(key) },
            .already_erased => |erased| .{ .erased_fn = try self.erasedFnPayloadForAlreadyErased(erased) },
            .erase_proc_value => |erase| .{ .erased_fn = try self.erasedFnPayloadForProcValue(callable, erase) },
            .erase_finite_set => |erase| .{ .erased_fn = try self.erasedFnPayloadForFiniteSetAdapter(erase) },
        };
    }

    fn callableSetPayload(self: *SessionExecutableTypePayloadBuilder, key: CanonicalCallableSetKey) std.mem.Allocator.Error!SessionExecutableCallableSetPayload {
        const descriptor = self.representation_store.callableSetDescriptor(key) orelse {
            representationInvariant("session callable-set executable payload has no descriptor");
        };
        if (descriptor.members.len == 0) representationInvariant("session callable-set executable payload descriptor is empty");
        const members = try self.allocator.alloc(SessionExecutableCallableSetMemberPayload, descriptor.members.len);
        errdefer self.allocator.free(members);
        for (descriptor.members, 0..) |member, i| {
            members[i] = .{ .member = member.member };
        }
        return .{
            .key = key,
            .members = members,
        };
    }

    fn erasedFnPayloadForAlreadyErased(self: *SessionExecutableTypePayloadBuilder, erased: AlreadyErasedCallablePlan) std.mem.Allocator.Error!SessionExecutableErasedFnPayload {
        const capture = try self.hiddenCapturePayloadForAlreadyErased(erased);
        return .{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .capture_ty = if (capture) |item| item.ty else null,
            .capture_ty_key = if (capture) |item| item.key else null,
        };
    }

    fn erasedFnPayloadForProcValue(
        self: *SessionExecutableTypePayloadBuilder,
        callable: CallableValueInfo,
        erase: ProcValueErasePlan,
    ) std.mem.Allocator.Error!SessionExecutableErasedFnPayload {
        const capture = try self.hiddenCapturePayloadForProcValue(callable, erase.erased_fn_sig_key);
        return .{
            .sig_key = erase.erased_fn_sig_key,
            .capture_shape_key = erase.capture_shape_key,
            .capture_ty = if (capture) |item| item.ty else null,
            .capture_ty_key = if (capture) |item| item.key else null,
        };
    }

    fn erasedFnPayloadForFiniteSetAdapter(
        self: *SessionExecutableTypePayloadBuilder,
        erase: FiniteSetErasePlan,
    ) std.mem.Allocator.Error!SessionExecutableErasedFnPayload {
        const capture = if (erase.adapter.erased_fn_sig_key.capture_ty == null)
            null
        else
            try self.payloadForCallableSetType(erase.adapter.callable_set_key, erase.adapter.erased_fn_sig_key.capture_ty.?);
        return .{
            .sig_key = erase.adapter.erased_fn_sig_key,
            .capture_shape_key = erase.adapter.capture_shape_key,
            .capture_ty = if (capture) |item| item.ty else null,
            .capture_ty_key = if (capture) |item| item.key else null,
        };
    }

    fn hiddenCapturePayloadForAlreadyErased(
        self: *SessionExecutableTypePayloadBuilder,
        erased: AlreadyErasedCallablePlan,
    ) std.mem.Allocator.Error!?SessionExecutableTypeEndpoint {
        return switch (erased.capture) {
            .none => blk: {
                if (erased.sig_key.capture_ty != null) representationInvariant("already-erased session payload has no capture but signature has capture type");
                break :blk null;
            },
            .zero_sized_ty => |ty| blk: {
                const capture = try self.endpointForType(ty);
                const expected = erased.sig_key.capture_ty orelse representationInvariant("already-erased session payload zero-sized capture has no signature capture type");
                if (!canonicalExecValueTypeKeyEql(capture.key, expected)) {
                    representationInvariant("already-erased session payload zero-sized capture key differs from signature");
                }
                break :blk capture;
            },
            .value => |value| blk: {
                const capture = try self.endpointForValue(value);
                const expected = erased.sig_key.capture_ty orelse representationInvariant("already-erased session payload capture value has no signature capture type");
                if (!canonicalExecValueTypeKeyEql(capture.key, expected)) {
                    representationInvariant("already-erased session payload capture key differs from signature");
                }
                break :blk capture;
            },
        };
    }

    fn hiddenCapturePayloadForProcValue(
        self: *SessionExecutableTypePayloadBuilder,
        callable: CallableValueInfo,
        sig_key: ErasedFnSigKey,
    ) std.mem.Allocator.Error!?SessionExecutableTypeEndpoint {
        if (sig_key.capture_ty == null) return null;
        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => representationInvariant("session proc-value erased payload attached to non-proc callable source"),
        };
        return try self.tuplePayloadForCaptureValues(source.captures, sig_key.capture_ty);
    }

    fn payloadForCallableSetType(
        self: *SessionExecutableTypePayloadBuilder,
        key: CanonicalCallableSetKey,
        expected_key: CanonicalExecValueTypeKey,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const id = try self.payload_store.append(self.allocator, expected_key, .{
            .callable_set = try self.callableSetPayload(key),
        });
        return .{
            .ty = refFor(id),
            .key = expected_key,
        };
    }

    fn tuplePayloadForCaptureValues(
        self: *SessionExecutableTypePayloadBuilder,
        captures: []const ValueInfoId,
        expected_key: ?CanonicalExecValueTypeKey,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        if (captures.len == 0) {
            const key = expected_key orelse blk: {
                var key_hasher = std.crypto.hash.sha2.Sha256.init(.{});
                key_hasher.update("capture_tuple");
                break :blk CanonicalExecValueTypeKey{ .bytes = key_hasher.finalResult() };
            };
            const id = try self.payload_store.append(self.allocator, key, .{ .tuple = &.{} });
            return .{
                .ty = refFor(id),
                .key = key,
            };
        }

        const items = try self.allocator.alloc(SessionExecutableTupleElemPayload, captures.len);
        errdefer self.allocator.free(items);
        var key_hasher = std.crypto.hash.sha2.Sha256.init(.{});
        key_hasher.update("capture_tuple");
        for (captures, 0..) |capture, i| {
            const child = try self.endpointForValue(capture);
            key_hasher.update(&child.key.bytes);
            items[i] = .{
                .index = @intCast(i),
                .ty = child.ty,
                .key = child.key,
            };
        }
        const key = expected_key orelse .{ .bytes = key_hasher.finalResult() };
        const id = try self.payload_store.append(self.allocator, key, .{ .tuple = items });
        return .{
            .ty = refFor(id),
            .key = key,
        };
    }

    fn aggregatePayload(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
        aggregate: AggregateValueInfo,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (aggregate) {
            .record => |record| .{ .record = try self.recordPayloadForValue(record) },
            .tuple => |tuple| .{ .tuple = try self.tuplePayloadForValue(tuple) },
            .tag => |tag| .{ .tag_union = try self.tagUnionPayloadForValue(logical_ty, tag) },
            .list => |list| .{ .list = try self.listPayloadForValue(logical_ty, list) },
        };
    }

    fn recordPayloadForValue(
        self: *SessionExecutableTypePayloadBuilder,
        record: anytype,
    ) std.mem.Allocator.Error!SessionExecutableRecordPayload {
        if (record.fields.len == 0) return .{ .shape = record.shape, .fields = &.{} };
        const out = try self.allocator.alloc(SessionExecutableRecordFieldPayload, record.fields.len);
        errdefer self.allocator.free(out);
        for (record.fields, 0..) |field, i| {
            const child = try self.childForValue(field.value);
            out[i] = .{
                .field = field.field,
                .ty = child.ty,
                .key = child.key,
            };
        }
        return .{ .shape = record.shape, .fields = out };
    }

    fn tuplePayloadForValue(
        self: *SessionExecutableTypePayloadBuilder,
        tuple: []const ElemValueInfo,
    ) std.mem.Allocator.Error![]const SessionExecutableTupleElemPayload {
        if (tuple.len == 0) return &.{};
        const out = try self.allocator.alloc(SessionExecutableTupleElemPayload, tuple.len);
        errdefer self.allocator.free(out);
        const seen = try self.allocator.alloc(bool, tuple.len);
        defer self.allocator.free(seen);
        @memset(seen, false);
        for (tuple) |elem| {
            const index: usize = @intCast(elem.index);
            if (index >= tuple.len) representationInvariant("session executable tuple payload index exceeded arity");
            if (seen[index]) representationInvariant("session executable tuple payload had duplicate index");
            const child = try self.childForValue(elem.value);
            out[index] = .{
                .index = elem.index,
                .ty = child.ty,
                .key = child.key,
            };
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) representationInvariant("session executable tuple payload was not dense");
        }
        return out;
    }

    fn tagUnionPayloadForValue(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
        tag_value: anytype,
    ) std.mem.Allocator.Error!SessionExecutableTagUnionPayload {
        const source_tags = try self.logicalTagUnionTags(logical_ty);
        const shape_tags = self.row_shapes.tagUnionTags(tag_value.union_shape);
        if (shape_tags.len != source_tags.len) representationInvariant("session executable tag payload shape/logical arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagVariantPayload, shape_tags.len);
        for (out) |*variant| variant.* = .{ .tag = @enumFromInt(0), .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }

        const seen_tags = try self.allocator.alloc(bool, shape_tags.len);
        defer self.allocator.free(seen_tags);
        @memset(seen_tags, false);

        for (shape_tags) |shape_tag| {
            const shape_tag_info = self.row_shapes.tag(shape_tag);
            const tag_index: usize = @intCast(shape_tag_info.logical_index);
            if (tag_index >= source_tags.len) representationInvariant("session executable tag payload logical index exceeded arity");
            if (seen_tags[tag_index]) representationInvariant("session executable tag payload saw duplicate logical index");
            out[tag_index] = .{
                .tag = shape_tag,
                .payloads = if (shape_tag == tag_value.tag)
                    try self.selectedTagPayloadsForValue(tag_value)
                else
                    try self.tagPayloadsForTypeSpan(shape_tag, source_tags[tag_index].args),
            };
            seen_tags[tag_index] = true;
        }
        for (seen_tags) |was_seen| {
            if (!was_seen) representationInvariant("session executable tag payload omitted a logical tag");
        }
        return .{ .shape = tag_value.union_shape, .variants = out };
    }

    fn selectedTagPayloadsForValue(
        self: *SessionExecutableTypePayloadBuilder,
        tag_value: anytype,
    ) std.mem.Allocator.Error![]const SessionExecutableTagPayload {
        const shape_payloads = self.row_shapes.tagPayloads(tag_value.tag);
        if (shape_payloads.len == 0) return &.{};
        const out = try self.allocator.alloc(SessionExecutableTagPayload, shape_payloads.len);
        errdefer self.allocator.free(out);
        const seen = try self.allocator.alloc(bool, shape_payloads.len);
        defer self.allocator.free(seen);
        @memset(seen, false);

        for (tag_value.payloads) |payload| {
            const payload_info = self.row_shapes.tagPayload(payload.payload);
            if (payload_info.tag != tag_value.tag) representationInvariant("session executable selected payload belongs to another tag");
            const index: usize = @intCast(payload_info.logical_index);
            if (index >= shape_payloads.len) representationInvariant("session executable selected payload index exceeded tag arity");
            if (seen[index]) representationInvariant("session executable selected payload was duplicated");
            if (payload.payload != shape_payloads[index]) representationInvariant("session executable selected payload id differs from row shape slot");
            const child = try self.childForValue(payload.value);
            out[index] = .{
                .payload = shape_payloads[index],
                .ty = child.ty,
                .key = child.key,
            };
            seen[index] = true;
        }
        for (seen) |was_seen| {
            if (!was_seen) representationInvariant("session executable selected tag omitted a payload");
        }
        return out;
    }

    fn listPayloadForValue(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
        list: anytype,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        if (list.elems.len == 0) {
            const elem_ty = try self.logicalListElemType(logical_ty);
            return try self.childForType(elem_ty);
        }
        const first = try self.childForValue(list.elems[0]);
        for (list.elems[1..]) |elem| {
            const child = try self.childForValue(elem);
            if (!canonicalExecValueTypeKeyEql(first.key, child.key)) {
                representationInvariant("session executable list payload elements have different executable representations");
            }
        }
        return first;
    }

    fn logicalListElemType(self: *SessionExecutableTypePayloadBuilder, logical_ty: type_mod.TypeVarId) std.mem.Allocator.Error!type_mod.TypeVarId {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalListElemType(nominal.backing),
            .content => |content| switch (content) {
                .list => |elem| elem,
                else => representationInvariant("session executable list payload attached to non-list type"),
            },
            else => representationInvariant("session executable list payload attached to unresolved type"),
        };
    }

    fn logicalTagUnionTags(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error![]const type_mod.Tag {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalTagUnionTags(nominal.backing),
            .content => |content| switch (content) {
                .tag_union => |tag_union| self.types.sliceTags(tag_union.tags),
                else => representationInvariant("session executable tag payload attached to non-tag-union type"),
            },
            else => representationInvariant("session executable tag payload attached to unresolved type"),
        };
    }
};

pub fn singletonCallableSetKey(
    proc_callable: canonical.ProcedureCallableRef,
    capture_shape_key: CaptureShapeKey,
    capture_slots: []const CallableSetCaptureSlot,
) CanonicalCallableSetKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "singleton_callable_set");
    writeProcedureCallableRef(&hasher, proc_callable);
    hasher.update(&capture_shape_key.bytes);
    writeHashU32(&hasher, @intCast(capture_slots.len));
    for (capture_slots) |slot| {
        writeHashU32(&hasher, slot.slot);
        hasher.update(&slot.source_ty.bytes);
        hasher.update(&slot.exec_value_ty.bytes);
    }
    return .{ .bytes = hasher.finalResult() };
}

const ExecValueTypeKeyBuilder = struct {
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    representation_store: ?*const RepresentationStore = null,
    value_store: ?*const ValueInfoStore = null,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.AutoHashMap(type_mod.TypeVarId, u32),
    active_values: std.AutoHashMap(ValueInfoId, u32),

    fn init(
        allocator: std.mem.Allocator,
        names: *const canonical.CanonicalNameStore,
        types: *const type_mod.Store,
    ) ExecValueTypeKeyBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .types = types,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(type_mod.TypeVarId, u32).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, u32).init(allocator),
        };
    }

    fn initForValues(
        allocator: std.mem.Allocator,
        names: *const canonical.CanonicalNameStore,
        types: *const type_mod.Store,
        representation_store: *const RepresentationStore,
        value_store: *const ValueInfoStore,
    ) ExecValueTypeKeyBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .types = types,
            .representation_store = representation_store,
            .value_store = value_store,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(type_mod.TypeVarId, u32).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, u32).init(allocator),
        };
    }

    fn deinit(self: *ExecValueTypeKeyBuilder) void {
        self.active_values.deinit();
        self.active.deinit();
    }

    fn key(self: *ExecValueTypeKeyBuilder, root: type_mod.TypeVarId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        try self.writeType(root);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn keyForValue(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        try self.writeValue(value);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn writeValue(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!void {
        const values = self.value_store orelse representationInvariant("executable value type key for value has no value store");
        if (self.active_values.get(value)) |slot| {
            self.writeTag("value_cycle");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.active_values.count());
        try self.active_values.put(value, slot);
        const info = values.values.items[@intFromEnum(value)];
        if (info.callable) |callable| {
            try self.writeCallableValue(callable);
        } else if (info.aggregate) |aggregate| {
            try self.writeAggregateValue(info.logical_ty, aggregate);
        } else {
            try self.writeType(info.logical_ty);
        }
        _ = self.active_values.remove(value);
    }

    fn writeCallableValue(self: *ExecValueTypeKeyBuilder, callable: CallableValueInfo) std.mem.Allocator.Error!void {
        const representations = self.representation_store orelse representationInvariant("executable value type key for callable has no representation store");
        switch (representations.callableEmissionPlan(callable.emission_plan)) {
            .finite => |key| {
                self.writeTag("callable_set");
                self.writeCanonicalCallableSetKey(key);
            },
            .already_erased => |erased| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKey(erased.sig_key);
            },
            .erase_proc_value => |erase| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKey(erase.erased_fn_sig_key);
            },
            .erase_finite_set => |erase| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKey(erase.adapter.erased_fn_sig_key);
            },
        }
    }

    fn writeAggregateValue(
        self: *ExecValueTypeKeyBuilder,
        logical_ty: TypeVarId,
        aggregate: AggregateValueInfo,
    ) std.mem.Allocator.Error!void {
        switch (aggregate) {
            .record => |record| {
                self.writeTag("record_value");
                self.writeU32(@intFromEnum(record.shape));
                self.writeU32(@intCast(record.fields.len));
                for (record.fields) |field| {
                    self.writeU32(@intFromEnum(field.field));
                    try self.writeValue(field.value);
                }
            },
            .tuple => |tuple| {
                self.writeTag("tuple_value");
                self.writeU32(@intCast(tuple.len));
                const seen = try self.allocator.alloc(bool, tuple.len);
                defer self.allocator.free(seen);
                @memset(seen, false);
                const ordered = try self.allocator.alloc(ValueInfoId, tuple.len);
                defer self.allocator.free(ordered);
                for (tuple) |elem| {
                    const index: usize = @intCast(elem.index);
                    if (index >= tuple.len) representationInvariant("executable value type key tuple element index exceeded tuple arity");
                    if (seen[index]) representationInvariant("executable value type key tuple had duplicate element index");
                    ordered[index] = elem.value;
                    seen[index] = true;
                }
                for (seen) |was_seen| {
                    if (!was_seen) representationInvariant("executable value type key tuple did not provide every element");
                }
                for (ordered) |elem| try self.writeValue(elem);
            },
            .tag => |tag| {
                self.writeTag("tag_value");
                self.writeU32(@intFromEnum(tag.union_shape));
                self.writeU32(@intFromEnum(tag.tag));
                self.writeU32(@intCast(tag.payloads.len));
                for (tag.payloads) |payload| {
                    self.writeU32(@intFromEnum(payload.payload));
                    try self.writeValue(payload.value);
                }
            },
            .list => |list| {
                self.writeTag("list_value");
                if (list.elems.len == 0) {
                    self.writeTag("empty");
                    try self.writeType(logical_ty);
                    return;
                }
                const first_key = try self.valueKeySnapshot(list.elems[0]);
                self.writeCanonicalExecValueTypeKey(first_key);
                for (list.elems[1..]) |elem| {
                    const elem_key = try self.valueKeySnapshot(elem);
                    if (!canonicalExecValueTypeKeyEql(first_key, elem_key)) {
                        representationInvariant("executable value type key list elements have different executable representations");
                    }
                }
            },
        }
    }

    fn valueKeySnapshot(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        const representations = self.representation_store orelse representationInvariant("executable value type key snapshot has no representation store");
        const values = self.value_store orelse representationInvariant("executable value type key snapshot has no value store");
        return try execValueTypeKeyForValue(self.allocator, self.names, self.types, representations, values, value);
    }

    fn writeType(self: *ExecValueTypeKeyBuilder, id: type_mod.TypeVarId) std.mem.Allocator.Error!void {
        const root = self.types.unlinkConst(id);
        if (self.active.get(root)) |slot| {
            self.writeTag("cycle");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.active.count());
        try self.active.put(root, slot);
        switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("executable value type key reached unresolved lambda-solved type"),
            .nominal => |nominal| {
                self.writeTag("nominal");
                self.writeBytes(self.names.moduleNameText(nominal.nominal.module_name));
                self.writeBytes(self.names.typeNameText(nominal.nominal.type_name));
                self.writeBool(nominal.is_opaque);
                try self.writeTypeSpan(nominal.args);
                try self.writeType(nominal.backing);
            },
            .content => |content| try self.writeContent(content),
        }
        _ = self.active.remove(root);
    }

    fn writeContent(self: *ExecValueTypeKeyBuilder, content: type_mod.Content) std.mem.Allocator.Error!void {
        switch (content) {
            .primitive => |prim| {
                self.writeTag("primitive");
                self.writeU32(@as(u32, @intCast(@intFromEnum(prim))));
            },
            .list => |elem| {
                self.writeTag("list");
                try self.writeType(elem);
            },
            .box => |elem| {
                self.writeTag("box");
                try self.writeType(elem);
            },
            .tuple => |items| {
                self.writeTag("tuple");
                try self.writeTypeSpan(items);
            },
            .record => |record| {
                self.writeTag("record");
                const fields = self.types.sliceFields(record.fields);
                self.writeU32(@intCast(fields.len));
                for (fields) |field| {
                    self.writeBytes(self.names.recordFieldLabelText(field.name));
                    try self.writeType(field.ty);
                }
            },
            .tag_union => |tag_union| {
                self.writeTag("tag_union");
                const tags = self.types.sliceTags(tag_union.tags);
                self.writeU32(@intCast(tags.len));
                for (tags) |tag| {
                    self.writeBytes(self.names.tagLabelText(tag.name));
                    try self.writeTypeSpan(tag.args);
                }
            },
            .func => representationInvariant("executable value type key reached a function type before callable representation was solved"),
        }
    }

    fn writeTypeSpan(self: *ExecValueTypeKeyBuilder, span: type_mod.Span(type_mod.TypeVarId)) std.mem.Allocator.Error!void {
        const items = self.types.sliceTypeVarSpan(span);
        self.writeU32(@intCast(items.len));
        for (items) |item| try self.writeType(item);
    }

    fn writeTag(self: *ExecValueTypeKeyBuilder, tag: []const u8) void {
        writeHashTag(&self.hasher, tag);
    }

    fn writeBytes(self: *ExecValueTypeKeyBuilder, bytes: []const u8) void {
        writeHashBytes(&self.hasher, bytes);
    }

    fn writeBool(self: *ExecValueTypeKeyBuilder, value: bool) void {
        self.hasher.update(&[_]u8{if (value) 1 else 0});
    }

    fn writeU32(self: *ExecValueTypeKeyBuilder, value: u32) void {
        writeHashU32(&self.hasher, value);
    }

    fn writeCanonicalCallableSetKey(self: *ExecValueTypeKeyBuilder, key: CanonicalCallableSetKey) void {
        self.hasher.update(&key.bytes);
    }

    fn writeCanonicalExecValueTypeKey(self: *ExecValueTypeKeyBuilder, key: CanonicalExecValueTypeKey) void {
        self.hasher.update(&key.bytes);
    }

    fn writeErasedFnSigKey(self: *ExecValueTypeKeyBuilder, key: ErasedFnSigKey) void {
        self.hasher.update(&key.source_fn_ty.bytes);
        self.hasher.update(&key.abi.bytes);
        if (key.capture_ty) |capture_ty| {
            self.writeBool(true);
            self.writeCanonicalExecValueTypeKey(capture_ty);
        } else {
            self.writeBool(false);
        }
    }
};

fn writeHashTag(hasher: *std.crypto.hash.sha2.Sha256, tag: []const u8) void {
    writeHashBytes(hasher, tag);
}

fn writeProcedureCallableRef(
    hasher: *std.crypto.hash.sha2.Sha256,
    ref: canonical.ProcedureCallableRef,
) void {
    writeCallableProcedureTemplateRef(hasher, ref.template);
    hasher.update(&ref.source_fn_ty.bytes);
}

fn writeCallableProcedureTemplateRef(
    hasher: *std.crypto.hash.sha2.Sha256,
    ref: canonical.CallableProcedureTemplateRef,
) void {
    switch (ref) {
        .checked => |checked| {
            hasher.update(&[_]u8{0});
            writeProcedureTemplateRef(hasher, checked);
        },
        .lifted => |lifted| {
            hasher.update(&[_]u8{1});
            writeMonoSpecializationKey(hasher, lifted.owner_mono_specialization);
            writeHashU32(hasher, @as(u32, @intCast(@intFromEnum(lifted.site))));
        },
        .synthetic => |synthetic| {
            hasher.update(&[_]u8{2});
            writeProcedureTemplateRef(hasher, synthetic.template);
        },
    }
}

fn executablePrimitive(prim: type_mod.Prim) ExecutablePrimitive {
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

fn writeMonoSpecializationKey(
    hasher: *std.crypto.hash.sha2.Sha256,
    key: canonical.MonoSpecializationKey,
) void {
    writeProcedureTemplateRef(hasher, key.template);
    hasher.update(&key.requested_mono_fn_ty.bytes);
}

fn writeProcedureTemplateRef(
    hasher: *std.crypto.hash.sha2.Sha256,
    ref: canonical.ProcedureTemplateRef,
) void {
    hasher.update(&ref.artifact.bytes);
    writeHashU32(hasher, @as(u32, @intCast(@intFromEnum(ref.proc_base))));
    writeHashU32(hasher, @as(u32, @intCast(@intFromEnum(ref.template))));
}

fn writeHashBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeHashU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeHashU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

fn representationInvariant(comptime message: []const u8) noreturn {
    debug.invariant(false, message);
    unreachable;
}

test "lambda-solved representation records are explicit" {
    std.testing.refAllDecls(@This());
}
