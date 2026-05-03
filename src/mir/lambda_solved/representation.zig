//! Lambda-solved MIR representation/value-flow records.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const ConcreteSourceType = @import("../concrete_source_type.zig");
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
pub const BoxPayloadRepresentationPlanId = enum(u32) { _ };
pub const CallableValueEmissionPlanId = enum(u32) { _ };
pub const CallableSetConstructionPlanId = enum(u32) { _ };
pub const CanonicalCallableSetDescriptorId = enum(u32) { _ };
pub const ValueTransformBoundaryId = enum(u32) { _ };
pub const SessionExecutableValueTransformId = checked_artifact.SessionExecutableValueTransformId;
pub const RepresentationEdgeId = enum(u32) { _ };
pub const RepresentationRequirementId = enum(u32) { _ };
pub const SourceMatchId = enum(u32) { _ };
pub const SourceMatchBranchId = enum(u32) { _ };
pub const SourceMatchAlternativeId = enum(u32) { _ };
pub const IfExprId = enum(u32) { _ };
pub const ProcedureBoundaryId = ReturnInfoId;
pub const CaptureBoundaryId = enum(u32) { _ };
pub const MutableJoinId = enum(u32) { _ };
pub const LoopPhiId = enum(u32) { _ };
pub const AggregateBoundaryId = enum(u32) { _ };
pub const TransformEndpointScopeId = enum(u32) { _ };
pub const TransformEndpointPathId = enum(u32) { _ };
pub const RepresentationClassId = enum(u32) { _ };
pub const ProcRepresentationInstanceId = enum(u32) { _ };
pub const RepresentationSolveSessionId = enum(u32) { _ };
pub const ValueInfoStoreId = enum(u32) { _ };
pub const CallableSetMemberId = canonical.CallableSetMemberId;
pub const ErasedAdapterId = enum(u32) { _ };
pub const Symbol = symbol_mod.Symbol;
pub const TypeVarId = type_mod.TypeVarId;

pub const RepresentationRootKind = union(enum) {
    unclassified,
    local_value: struct {
        instance: ProcRepresentationInstanceId,
        value: ValueInfoId,
    },
    binding: struct {
        instance: ProcRepresentationInstanceId,
        binding: BindingInfoId,
    },
    pattern_binder: struct {
        instance: ProcRepresentationInstanceId,
        binding: BindingInfoId,
    },
    procedure_param: struct {
        instance: ProcRepresentationInstanceId,
        index: u32,
    },
    procedure_return: ProcRepresentationInstanceId,
    procedure_capture: struct {
        instance: ProcRepresentationInstanceId,
        slot: u32,
    },
    call_value_requested_fn: struct {
        instance: ProcRepresentationInstanceId,
        call_site: CallSiteInfoId,
    },
    call_proc_requested_fn: struct {
        instance: ProcRepresentationInstanceId,
        call_site: CallSiteInfoId,
    },
    proc_value_fn: struct {
        instance: ProcRepresentationInstanceId,
        value: ValueInfoId,
    },
    mutable_var_version: struct {
        instance: ProcRepresentationInstanceId,
        symbol: Symbol,
        version: u32,
    },
    loop_phi: struct {
        instance: ProcRepresentationInstanceId,
        phi: LoopPhiId,
    },
};

pub const RepresentationEdgeKind = union(enum) {
    value_alias,
    value_move,
    function_arg: u32,
    function_return,
    function_callable,
    record_field: row.RecordFieldId,
    tuple_elem: u32,
    tag_payload: row.TagPayloadId,
    list_elem,
    box_payload,
    nominal_backing: canonical.NominalTypeKey,
    branch_join,
    loop_phi,
    mutable_version,
};

pub const ProcPublicRootRef = struct {
    instance: ProcRepresentationInstanceId,
    value: ValueInfoId,
    rep_root: RepRootId,
};

pub const RepresentationEndpoint = union(enum) {
    local: RepRootId,
    procedure_public: ProcPublicRootRef,
};

pub const RepresentationEdge = struct {
    from: RepresentationEndpoint,
    to: RepresentationEndpoint,
    kind: RepresentationEdgeKind,
};

pub const RepresentationRequirement = union(enum) {
    require_box_erased: BoxBoundaryId,
};

pub const TransformEndpointSide = enum {
    from,
    to,
};

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
    source_ty: canonical.CanonicalTypeKey,
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

pub const BoxPayloadCapabilityRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    capability: checked_artifact.BoxPayloadCapabilityId,
};

pub const OpaqueAtomicProofRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    proof: checked_artifact.OpaqueAtomicProofId,
};

pub const HostedRepresentationCapabilityRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    capability: checked_artifact.HostedRepresentationCapabilityId,
};

pub const PlatformRepresentationCapabilityRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    capability: checked_artifact.PlatformRepresentationCapabilityId,
};

pub const NominalPayloadRepresentation = union(enum) {
    transparent_backing: struct {
        nominal: canonical.NominalTypeKey,
        source_ty: canonical.CanonicalTypeKey,
        backing_plan: BoxPayloadRepresentationPlanId,
    },
    imported_capability: struct {
        nominal: canonical.NominalTypeKey,
        source_ty: canonical.CanonicalTypeKey,
        capability: BoxPayloadCapabilityRef,
        backing_plan: BoxPayloadRepresentationPlanId,
    },
    opaque_atomic: struct {
        nominal: canonical.NominalTypeKey,
        source_ty: canonical.CanonicalTypeKey,
        proof: OpaqueAtomicProofRef,
    },
    hosted_abi: HostedRepresentationCapabilityRef,
    platform_abi: PlatformRepresentationCapabilityRef,
};

pub const BoxPayloadRepresentationPlan = union(enum) {
    unchanged,
    function_erased: struct {
        source_fn_ty: canonical.CanonicalTypeKey,
        sig_key: ErasedFnSigKey,
    },
    record: []const BoxPayloadFieldPlan,
    tag_union: []const BoxPayloadTagPlan,
    tuple: []const BoxPayloadTupleElemPlan,
    list: BoxPayloadRepresentationPlanId,
    nested_box: BoxPayloadRepresentationPlanId,
    nominal: NominalPayloadRepresentation,
};

pub const BoxPayloadFieldPlan = struct {
    field: row.RecordFieldId,
    plan: BoxPayloadRepresentationPlanId,
};

pub const BoxPayloadTupleElemPlan = struct {
    index: u32,
    plan: BoxPayloadRepresentationPlanId,
};

pub const BoxPayloadTagPlan = struct {
    tag: row.TagId,
    payloads: []const BoxPayloadTagPayloadPlan,
};

pub const BoxPayloadTagPayloadPlan = struct {
    payload: row.TagPayloadId,
    plan: BoxPayloadRepresentationPlanId,
};

fn deinitBoxPayloadRepresentationPlan(
    allocator: std.mem.Allocator,
    plan: BoxPayloadRepresentationPlan,
) void {
    switch (plan) {
        .unchanged,
        .function_erased,
        .list,
        .nested_box,
        .nominal,
        => {},
        .record => |fields| if (fields.len > 0) allocator.free(fields),
        .tuple => |items| if (items.len > 0) allocator.free(items),
        .tag_union => |tags| {
            for (tags) |tag| {
                if (tag.payloads.len > 0) allocator.free(tag.payloads);
            }
            if (tags.len > 0) allocator.free(tags);
        },
    }
}

pub const BoxBoundary = struct {
    box_ty: canonical.CanonicalTypeKey,
    box_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    payload_source_ty: canonical.CanonicalTypeKey,
    payload_source_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    payload_boundary_ty: canonical.CanonicalTypeKey,
    payload_boundary_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    direction: BoxBoundaryDirection,
    source_root: RepRootId,
    boundary_root: RepRootId,
    payload_plan: BoxPayloadRepresentationPlan,
};

pub const ProcValueErasePlan = struct {
    source_value: ValueInfoId,
    proc_value: canonical.ProcedureCallableRef,
    target_instance: ProcRepresentationInstanceId,
    erased_fn_sig_key: ErasedFnSigKey,
    executable_specialization_key: ExecutableSpecializationKey,
    capture_shape_key: CaptureShapeKey,
    capture_slots: []const CallableSetCaptureSlot = &.{},
    capture_transforms: []const ValueTransformBoundaryId = &.{},
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
    capture_transforms: []const ValueTransformBoundaryId = &.{},
};

pub const CaptureBoundaryOwner = union(enum) {
    callable_set_construction: struct {
        construction: CallableSetConstructionPlanId,
        selected_member: CallableSetMemberRef,
    },
    proc_value_erase: struct {
        emission_plan: CallableValueEmissionPlanId,
        source_value: ValueInfoId,
        proc_value: canonical.ProcedureCallableRef,
        erased_fn_sig_key: ErasedFnSigKey,
    },
};

pub const CaptureBoundaryInfo = struct {
    owner: CaptureBoundaryOwner,
    target_instance: ProcRepresentationInstanceId,
    slot: u32,
    source_capture_value: ValueInfoId,
    target_capture_value: ValueInfoId,
    boundary: ValueTransformBoundaryId,
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
    payload_value: ?ValueInfoId = null,
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

pub const TransformEndpointPathStep = union(enum) {
    record_field: row.RecordFieldId,
    tuple_elem: u32,
    tag_payload: struct {
        tag: row.TagId,
        payload_index: u32,
    },
    list_elem,
    box_payload,
    nominal_backing: canonical.NominalTypeKey,
    callable_leaf,
};

pub const TransformChildEndpoint = struct {
    scope: TransformEndpointScopeId,
    side: TransformEndpointSide,
    path: TransformEndpointPathId,
};

pub const SessionExecutableValueEndpointOwner = union(enum) {
    local_value: ValueInfoId,
    procedure_param: struct {
        instance: ProcRepresentationInstanceId,
        index: u32,
    },
    procedure_return: ProcRepresentationInstanceId,
    procedure_capture: struct {
        instance: ProcRepresentationInstanceId,
        slot: u32,
    },
    call_raw_arg: struct {
        call: CallSiteInfoId,
        index: u32,
    },
    call_raw_result: CallSiteInfoId,
    callable_match_branch_raw_result: struct {
        call: CallSiteInfoId,
        member: CallableSetMemberRef,
    },
    transform_child: TransformChildEndpoint,
};

pub const SessionExecutableValueEndpoint = struct {
    owner: SessionExecutableValueEndpointOwner,
    logical_ty: TypeVarId,
    exec_ty: SessionExecutableTypeEndpoint,
};

pub const TransformEndpointScope = struct {
    root_kind: ValueTransformBoundaryKind,
    root_from: SessionExecutableValueEndpoint,
    root_to: SessionExecutableValueEndpoint,
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
    already_erased_callable: checked_artifact.AlreadyErasedCallableTransformPlan,
};

pub const SessionExecutableValueTransformPlan = struct {
    scope: ?TransformEndpointScopeId = null,
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
        .scope = plan.scope,
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
        .already_erased_callable => |erased| .{ .already_erased_callable = erased },
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

            const capture_transforms = if (proc.capture_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(ValueTransformBoundaryId, proc.capture_transforms);
            errdefer if (capture_transforms.len > 0) allocator.free(capture_transforms);

            const provenance = if (proc.provenance.len == 0)
                &.{}
            else
                try allocator.dupe(BoxBoundaryId, proc.provenance);
            errdefer if (provenance.len > 0) allocator.free(provenance);

            break :blk .{ .proc_value = .{
                .source_value = proc.source_value,
                .proc_value = proc.proc_value,
                .target_instance = proc.target_instance,
                .erased_fn_sig_key = proc.erased_fn_sig_key,
                .executable_specialization_key = key,
                .capture_shape_key = proc.capture_shape_key,
                .capture_slots = capture_slots,
                .capture_transforms = capture_transforms,
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
        .already_erased_callable => {},
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
            if (proc.capture_transforms.len > 0) allocator.free(proc.capture_transforms);
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
    source_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    root: RepRootId,
    solved_class: ?RepresentationClassId = null,
    exec_ty: ?SessionExecutableTypeEndpoint = null,
    value_alias_source: ?ValueInfoId = null,
    projection_info: ?ProjectionInfoId = null,
    join_info: ?JoinInfoId = null,
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
    dispatch: ?CallSiteDispatch = null,
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
    root_classes: []RepresentationClassId = &.{},
    callable_class_emissions: []?CallableValueEmissionPlanId = &.{},
    root_kinds: std.AutoHashMap(RepRootId, RepresentationRootKind),
    representation_edges: std.ArrayList(RepresentationEdge) = .empty,
    representation_requirements: std.ArrayList(RepresentationRequirement) = .empty,
    callable_emission_plans: []CallableValueEmissionPlan = &.{},
    callable_construction_plans: []CallableSetConstructionPlan = &.{},
    callable_set_descriptors: []const CanonicalCallableSetDescriptor = &.{},
    session_executable_type_payloads: SessionExecutableTypePayloadStore,
    erased_fn_abis: ErasedFnAbiStore = .{},
    box_payload_plans: []const BoxPayloadRepresentationPlan = &.{},
    box_boundaries: []BoxBoundary = &.{},
    capture_boundaries: []CaptureBoundaryInfo = &.{},
    value_transform_boundaries: []const ValueTransformBoundary = &.{},
    transform_endpoint_scopes: std.ArrayList(TransformEndpointScope) = .empty,
    transform_endpoint_paths: std.ArrayList(Span(TransformEndpointPathStep)) = .empty,
    transform_endpoint_path_steps: std.ArrayList(TransformEndpointPathStep) = .empty,
    session_value_transforms: SessionExecutableValueTransformStore = .{},

    pub fn init(allocator: std.mem.Allocator) RepresentationStore {
        return .{
            .allocator = allocator,
            .root_kinds = std.AutoHashMap(RepRootId, RepresentationRootKind).init(allocator),
            .session_executable_type_payloads = SessionExecutableTypePayloadStore.init(allocator),
        };
    }

    pub fn deinit(self: *RepresentationStore) void {
        self.session_value_transforms.deinit(self.allocator);
        self.transform_endpoint_path_steps.deinit(self.allocator);
        self.transform_endpoint_paths.deinit(self.allocator);
        self.transform_endpoint_scopes.deinit(self.allocator);
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
                    if (erase.capture_transforms.len > 0) self.allocator.free(erase.capture_transforms);
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
            if (plan.capture_transforms.len > 0) self.allocator.free(plan.capture_transforms);
        }
        if (self.callable_construction_plans.len > 0) self.allocator.free(self.callable_construction_plans);
        for (self.callable_set_descriptors) |descriptor| {
            for (descriptor.members) |member| {
                if (member.capture_slots.len > 0) self.allocator.free(member.capture_slots);
            }
            if (descriptor.members.len > 0) self.allocator.free(descriptor.members);
        }
        if (self.callable_set_descriptors.len > 0) self.allocator.free(self.callable_set_descriptors);
        for (self.box_payload_plans) |plan| deinitBoxPayloadRepresentationPlan(self.allocator, plan);
        if (self.box_payload_plans.len > 0) self.allocator.free(self.box_payload_plans);
        for (self.box_boundaries) |*boundary| deinitBoxPayloadRepresentationPlan(self.allocator, boundary.payload_plan);
        if (self.box_boundaries.len > 0) self.allocator.free(self.box_boundaries);
        if (self.capture_boundaries.len > 0) self.allocator.free(self.capture_boundaries);
        if (self.value_transform_boundaries.len > 0) self.allocator.free(self.value_transform_boundaries);
        if (self.root_classes.len > 0) self.allocator.free(self.root_classes);
        if (self.callable_class_emissions.len > 0) self.allocator.free(self.callable_class_emissions);
        self.representation_requirements.deinit(self.allocator);
        self.representation_edges.deinit(self.allocator);
        self.root_kinds.deinit();
        self.* = RepresentationStore.init(self.allocator);
    }

    pub fn reserveRoot(self: *RepresentationStore) RepRootId {
        const id: RepRootId = @enumFromInt(self.roots_len);
        self.roots_len += 1;
        return id;
    }

    pub fn publishRootKind(
        self: *RepresentationStore,
        root: RepRootId,
        kind: RepresentationRootKind,
    ) std.mem.Allocator.Error!void {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: representation root kind published for an unreserved root");
            unreachable;
        }
        if (self.root_kinds.contains(root)) {
            debug.invariant(false, "lambda-solved invariant violated: representation root kind was published twice");
            unreachable;
        }
        try self.root_kinds.put(root, kind);
    }

    pub fn rootKind(self: *const RepresentationStore, root: RepRootId) RepresentationRootKind {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: representation root lookup referenced an unreserved root");
            unreachable;
        }
        return self.root_kinds.get(root) orelse .unclassified;
    }

    pub fn appendRepresentationEdge(
        self: *RepresentationStore,
        edge: RepresentationEdge,
    ) std.mem.Allocator.Error!RepresentationEdgeId {
        self.verifyRepresentationEndpoint(edge.from, "representation edge source");
        self.verifyRepresentationEndpoint(edge.to, "representation edge target");
        const id: RepresentationEdgeId = @enumFromInt(@as(u32, @intCast(self.representation_edges.items.len)));
        try self.representation_edges.append(self.allocator, edge);
        return id;
    }

    pub fn appendRepresentationRequirement(
        self: *RepresentationStore,
        requirement: RepresentationRequirement,
    ) std.mem.Allocator.Error!RepresentationRequirementId {
        const id: RepresentationRequirementId = @enumFromInt(@as(u32, @intCast(self.representation_requirements.items.len)));
        try self.representation_requirements.append(self.allocator, requirement);
        return id;
    }

    fn verifyReservedRoot(self: *const RepresentationStore, root: RepRootId, comptime label: []const u8) void {
        if (@intFromEnum(root) >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: " ++ label ++ " referenced an unreserved root");
            unreachable;
        }
    }

    fn verifyRepresentationEndpoint(
        self: *const RepresentationStore,
        endpoint: RepresentationEndpoint,
        comptime label: []const u8,
    ) void {
        switch (endpoint) {
            .local => |root| self.verifyReservedRoot(root, label),
            .procedure_public => {},
        }
    }

    pub fn reserveClass(self: *RepresentationStore) RepresentationClassId {
        const id: RepresentationClassId = @enumFromInt(self.classes_len);
        self.classes_len += 1;
        return id;
    }

    pub fn resetSolvedClasses(self: *RepresentationStore) void {
        self.classes_len = 0;
        if (self.root_classes.len > 0) self.allocator.free(self.root_classes);
        self.root_classes = &.{};
        if (self.callable_class_emissions.len > 0) self.allocator.free(self.callable_class_emissions);
        self.callable_class_emissions = &.{};
    }

    pub fn publishRootClasses(
        self: *RepresentationStore,
        root_classes: []RepresentationClassId,
    ) void {
        if (root_classes.len != @as(usize, @intCast(self.roots_len))) {
            debug.invariant(false, "lambda-solved invariant violated: solved root class table length differs from root count");
            unreachable;
        }
        if (self.root_classes.len > 0) {
            debug.invariant(false, "lambda-solved invariant violated: solved root classes were published twice");
            unreachable;
        }
        self.root_classes = root_classes;
    }

    pub fn classForRoot(self: *const RepresentationStore, root: RepRootId) RepresentationClassId {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: solved class lookup referenced an unreserved root");
            unreachable;
        }
        if (self.root_classes.len != @as(usize, @intCast(self.roots_len))) {
            debug.invariant(false, "lambda-solved invariant violated: solved root classes are not published");
            unreachable;
        }
        return self.root_classes[root_index];
    }

    pub fn publishCallableClassEmission(
        self: *RepresentationStore,
        class: RepresentationClassId,
        emission_plan: CallableValueEmissionPlanId,
    ) std.mem.Allocator.Error!void {
        const class_index: usize = @intFromEnum(class);
        if (class_index >= @as(usize, @intCast(self.classes_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable class emission referenced an unreserved class");
            unreachable;
        }
        if (self.callable_class_emissions.len == 0) {
            self.callable_class_emissions = try self.allocator.alloc(?CallableValueEmissionPlanId, @intCast(self.classes_len));
            @memset(self.callable_class_emissions, null);
        }
        if (self.callable_class_emissions.len != @as(usize, @intCast(self.classes_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable class emission table length differs from class count");
            unreachable;
        }
        if (self.callable_class_emissions[class_index]) |existing| {
            if (existing != emission_plan) {
                debug.invariant(false, "lambda-solved invariant violated: callable class emission was published twice with different plans");
                unreachable;
            }
            return;
        }
        self.callable_class_emissions[class_index] = emission_plan;
    }

    pub fn callableClassEmission(
        self: *const RepresentationStore,
        class: RepresentationClassId,
    ) ?CallableValueEmissionPlanId {
        const class_index: usize = @intFromEnum(class);
        if (class_index >= @as(usize, @intCast(self.classes_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable class emission lookup referenced an unreserved class");
            unreachable;
        }
        if (self.callable_class_emissions.len == 0) return null;
        if (self.callable_class_emissions.len != @as(usize, @intCast(self.classes_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable class emission table length differs from class count");
            unreachable;
        }
        return self.callable_class_emissions[class_index];
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

    pub fn appendBoxPayloadPlan(
        self: *RepresentationStore,
        plan: BoxPayloadRepresentationPlan,
    ) std.mem.Allocator.Error!BoxPayloadRepresentationPlanId {
        const old = self.box_payload_plans;
        const next = try self.allocator.alloc(BoxPayloadRepresentationPlan, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) self.allocator.free(old);
        const id: BoxPayloadRepresentationPlanId = @enumFromInt(@as(u32, @intCast(old.len)));
        next[old.len] = plan;
        self.box_payload_plans = next;
        return id;
    }

    pub fn setBoxBoundaryPayloadPlan(
        self: *RepresentationStore,
        boundary_id: BoxBoundaryId,
        plan: BoxPayloadRepresentationPlan,
    ) void {
        const index: usize = @intFromEnum(boundary_id);
        if (index >= self.box_boundaries.len) {
            debug.invariant(false, "lambda-solved invariant violated: boxed payload plan referenced missing BoxBoundary");
            unreachable;
        }
        switch (self.box_boundaries[index].payload_plan) {
            .unchanged => {},
            else => {
                debug.invariant(false, "lambda-solved invariant violated: BoxBoundary payload plan was finalized twice");
                unreachable;
            },
        }
        self.box_boundaries[index].payload_plan = plan;
    }

    pub fn appendTransformEndpointScope(
        self: *RepresentationStore,
        scope: TransformEndpointScope,
    ) std.mem.Allocator.Error!TransformEndpointScopeId {
        const id: TransformEndpointScopeId = @enumFromInt(@as(u32, @intCast(self.transform_endpoint_scopes.items.len)));
        try self.transform_endpoint_scopes.append(self.allocator, scope);
        return id;
    }

    pub fn appendTransformEndpointPath(
        self: *RepresentationStore,
        steps: []const TransformEndpointPathStep,
    ) std.mem.Allocator.Error!TransformEndpointPathId {
        if (steps.len == 0) {
            debug.invariant(false, "lambda-solved invariant violated: transform child endpoint path cannot be empty");
            unreachable;
        }

        const id: TransformEndpointPathId = @enumFromInt(@as(u32, @intCast(self.transform_endpoint_paths.items.len)));
        try self.transform_endpoint_paths.ensureUnusedCapacity(self.allocator, 1);
        try self.transform_endpoint_path_steps.ensureUnusedCapacity(self.allocator, steps.len);

        const start: u32 = @intCast(self.transform_endpoint_path_steps.items.len);
        self.transform_endpoint_path_steps.appendSliceAssumeCapacity(steps);
        self.transform_endpoint_paths.appendAssumeCapacity(.{
            .start = start,
            .len = @intCast(steps.len),
        });
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

    pub fn reserveCaptureBoundary(
        self: *RepresentationStore,
        info: CaptureBoundaryInfo,
    ) std.mem.Allocator.Error!CaptureBoundaryId {
        const id: CaptureBoundaryId = @enumFromInt(@as(u32, @intCast(self.capture_boundaries.len)));
        const old = self.capture_boundaries;
        const next = try self.allocator.alloc(CaptureBoundaryInfo, old.len + 1);
        @memcpy(next[0..old.len], old);
        next[old.len] = info;
        if (old.len > 0) self.allocator.free(old);
        self.capture_boundaries = next;
        return id;
    }

    pub fn fillCaptureBoundary(
        self: *RepresentationStore,
        id: CaptureBoundaryId,
        boundary: ValueTransformBoundaryId,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.capture_boundaries.len) {
            debug.invariant(false, "lambda-solved invariant violated: capture boundary id out of range");
            unreachable;
        }
        self.capture_boundaries[index].boundary = boundary;
    }

    pub fn captureBoundary(
        self: *const RepresentationStore,
        id: CaptureBoundaryId,
    ) CaptureBoundaryInfo {
        return self.capture_boundaries[@intFromEnum(id)];
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

    pub fn transformEndpointScope(
        self: *const RepresentationStore,
        id: TransformEndpointScopeId,
    ) TransformEndpointScope {
        const index = @intFromEnum(id);
        if (index >= self.transform_endpoint_scopes.items.len) {
            debug.invariant(false, "lambda-solved invariant violated: transform endpoint scope id out of range");
            unreachable;
        }
        return self.transform_endpoint_scopes.items[index];
    }

    pub fn transformEndpointPath(
        self: *const RepresentationStore,
        id: TransformEndpointPathId,
    ) []const TransformEndpointPathStep {
        const index = @intFromEnum(id);
        if (index >= self.transform_endpoint_paths.items.len) {
            debug.invariant(false, "lambda-solved invariant violated: transform endpoint path id out of range");
            unreachable;
        }
        const span = self.transform_endpoint_paths.items[index];
        return self.transform_endpoint_path_steps.items[span.start..][0..span.len];
    }

    pub fn callableEmissionPlan(
        self: *const RepresentationStore,
        id: CallableValueEmissionPlanId,
    ) CallableValueEmissionPlan {
        return self.callable_emission_plans[@intFromEnum(id)];
    }

    pub fn callableEmissionPlanPtr(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
    ) *CallableValueEmissionPlan {
        return &self.callable_emission_plans[@intFromEnum(id)];
    }

    pub fn setProcValueEraseCaptureTransforms(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
        transforms: []const ValueTransformBoundaryId,
    ) std.mem.Allocator.Error!void {
        const plan = self.callableEmissionPlanPtr(id);
        switch (plan.*) {
            .erase_proc_value => |*erase| {
                if (erase.capture_transforms.len > 0) {
                    debug.invariant(false, "lambda-solved invariant violated: proc-value erase capture transforms were already finalized");
                    unreachable;
                }
                erase.capture_transforms = if (transforms.len == 0)
                    &.{}
                else
                    try self.allocator.dupe(ValueTransformBoundaryId, transforms);
            },
            else => {
                debug.invariant(false, "lambda-solved invariant violated: proc-value erase capture transforms attached to non-proc-value erase plan");
                unreachable;
            },
        }
    }

    pub fn callableConstructionPlan(
        self: *const RepresentationStore,
        id: CallableSetConstructionPlanId,
    ) CallableSetConstructionPlan {
        return self.callable_construction_plans[@intFromEnum(id)];
    }

    pub fn callableConstructionPlanPtr(
        self: *RepresentationStore,
        id: CallableSetConstructionPlanId,
    ) *CallableSetConstructionPlan {
        return &self.callable_construction_plans[@intFromEnum(id)];
    }

    pub fn setCallableConstructionCaptureTransforms(
        self: *RepresentationStore,
        id: CallableSetConstructionPlanId,
        transforms: []const ValueTransformBoundaryId,
    ) std.mem.Allocator.Error!void {
        const plan = self.callableConstructionPlanPtr(id);
        if (plan.capture_transforms.len > 0) {
            debug.invariant(false, "lambda-solved invariant violated: callable construction capture transforms were already finalized");
            unreachable;
        }
        plan.capture_transforms = if (transforms.len == 0)
            &.{}
        else
            try self.allocator.dupe(ValueTransformBoundaryId, transforms);
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
        for (self.callable_construction_plans) |construction| {
            if (construction.capture_values.len != construction.capture_transforms.len) {
                debug.invariant(false, "lambda-solved invariant violated: callable construction capture transform count differs from capture count");
            }
        }
        for (self.callable_emission_plans) |plan| {
            switch (plan) {
                .erase_proc_value => |erase| {
                    if (erase.capture_slots.len != erase.capture_transforms.len) {
                        debug.invariant(false, "lambda-solved invariant violated: proc-value erase capture transform count differs from capture slot count");
                    }
                },
                else => {},
            }
        }
        for (self.capture_boundaries, 0..) |capture, raw_id| {
            const boundary_index = @intFromEnum(capture.boundary);
            if (boundary_index >= self.value_transform_boundaries.len) {
                debug.invariant(false, "lambda-solved invariant violated: capture boundary did not point at a value transform boundary");
            }
            const boundary = self.value_transform_boundaries[boundary_index];
            const capture_id: CaptureBoundaryId = @enumFromInt(@as(u32, @intCast(raw_id)));
            switch (boundary.kind) {
                .capture_value => |id| {
                    if (id != capture_id) {
                        debug.invariant(false, "lambda-solved invariant violated: capture boundary kind pointed at a different capture boundary");
                    }
                },
                else => debug.invariant(false, "lambda-solved invariant violated: capture boundary transform has non-capture kind"),
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
        const callable_root = self.reserveRoot();
        _ = try self.appendRepresentationEdge(.{
            .from = .{ .local = whole_function_root },
            .to = .{ .local = callable_root },
            .kind = .function_callable,
        });
        return .{
            .whole_function_root = whole_function_root,
            .callable_root = callable_root,
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

    pub fn appendFiniteSetEraseEmissionPlan(
        self: *RepresentationStore,
        erase: FiniteSetErasePlan,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        const provenance = if (erase.provenance.len == 0)
            &.{}
        else
            try self.allocator.dupe(BoxBoundaryId, erase.provenance);
        errdefer if (provenance.len > 0) self.allocator.free(provenance);

        return try self.appendCallableEmissionPlan(.{ .erase_finite_set = .{
            .adapter = erase.adapter,
            .result_ty = erase.result_ty,
            .provenance = provenance,
        } });
    }

    pub fn appendFiniteCallableEmissionPlan(
        self: *RepresentationStore,
        key: CanonicalCallableSetKey,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        return try self.appendCallableEmissionPlan(.{ .finite = key });
    }

    pub fn appendProcValueEraseEmissionPlan(
        self: *RepresentationStore,
        erase: ProcValueErasePlan,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        var key = try cloneExecutableSpecializationKey(self.allocator, erase.executable_specialization_key);
        errdefer deinitExecutableSpecializationKey(self.allocator, &key);

        const capture_slots = if (erase.capture_slots.len == 0)
            &.{}
        else
            try self.allocator.dupe(CallableSetCaptureSlot, erase.capture_slots);
        errdefer if (capture_slots.len > 0) self.allocator.free(capture_slots);

        const capture_transforms = if (erase.capture_transforms.len == 0)
            &.{}
        else
            try self.allocator.dupe(ValueTransformBoundaryId, erase.capture_transforms);
        errdefer if (capture_transforms.len > 0) self.allocator.free(capture_transforms);

        const provenance = if (erase.provenance.len == 0)
            &.{}
        else
            try self.allocator.dupe(BoxBoundaryId, erase.provenance);
        errdefer if (provenance.len > 0) self.allocator.free(provenance);

        return try self.appendCallableEmissionPlan(.{ .erase_proc_value = .{
            .source_value = erase.source_value,
            .proc_value = erase.proc_value,
            .target_instance = erase.target_instance,
            .erased_fn_sig_key = erase.erased_fn_sig_key,
            .executable_specialization_key = key,
            .capture_shape_key = erase.capture_shape_key,
            .capture_slots = capture_slots,
            .capture_transforms = capture_transforms,
            .provenance = provenance,
        } });
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
            .capture_transforms = if (plan.capture_transforms.len == 0)
                &.{}
            else
                try self.allocator.dupe(ValueTransformBoundaryId, plan.capture_transforms),
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

    pub fn internCallableSetDescriptor(
        self: *RepresentationStore,
        members: []const CanonicalCallableSetMember,
    ) std.mem.Allocator.Error!CanonicalCallableSetKey {
        if (members.len == 0) representationInvariant("lambda-solved attempted to intern an empty callable-set descriptor");
        const key = callableSetKeyForMembers(members);
        if (self.callableSetDescriptor(key) != null) return key;

        const owned_members = try self.allocator.alloc(CanonicalCallableSetMember, members.len);
        for (owned_members) |*member| member.* = .{
            .member = @enumFromInt(0),
            .proc_value = members[0].proc_value,
            .source_proc = members[0].source_proc,
            .capture_slots = &.{},
            .capture_shape_key = .{},
        };
        errdefer self.allocator.free(owned_members);
        errdefer {
            for (owned_members) |owned| {
                if (owned.capture_slots.len > 0) self.allocator.free(owned.capture_slots);
            }
        }
        for (members, 0..) |member, i| {
            owned_members[i] = .{
                .member = @enumFromInt(@as(u32, @intCast(i))),
                .proc_value = member.proc_value,
                .source_proc = member.source_proc,
                .capture_slots = if (member.capture_slots.len == 0)
                    &.{}
                else
                    try self.allocator.dupe(CallableSetCaptureSlot, member.capture_slots),
                .capture_shape_key = member.capture_shape_key,
            };
        }

        try self.appendCallableSetDescriptor(.{
            .key = key,
            .members = owned_members,
        });
        return key;
    }

    pub fn captureSlotsForValues(
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
            .erase_finite_set => |erase| erase.adapter.callable_set_key,
            else => {
                debug.invariant(false, "lambda-solved invariant violated: callable construction does not have finite callable-set emission");
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
        debug.invariant(
            member.capture_slots.len == construction.capture_transforms.len,
            "lambda-solved invariant violated: callable construction capture transform count differs from descriptor member",
        );
        for (member.capture_slots, 0..) |slot, i| {
            debug.invariant(
                slot.slot == @as(u32, @intCast(i)),
                "lambda-solved invariant violated: callable capture slots are not canonical",
            );
            const boundary = self.valueTransformBoundary(construction.capture_transforms[i]);
            switch (boundary.kind) {
                .capture_value => |capture_boundary| {
                    const info = self.captureBoundary(capture_boundary);
                    switch (info.owner) {
                        .callable_set_construction => |owner| {
                            debug.invariant(owner.construction == construction_id, "lambda-solved invariant violated: capture boundary points at a different construction plan");
                            debug.invariant(
                                callableSetKeyEql(owner.selected_member.callable_set_key, construction.callable_set_key) and
                                    owner.selected_member.member_index == construction.selected_member,
                                "lambda-solved invariant violated: capture boundary selected member differs from construction plan",
                            );
                        },
                        else => debug.invariant(false, "lambda-solved invariant violated: callable construction capture boundary has wrong owner"),
                    }
                    debug.invariant(info.slot == slot.slot, "lambda-solved invariant violated: capture boundary slot differs from descriptor member");
                    debug.invariant(info.source_capture_value == construction.capture_values[i], "lambda-solved invariant violated: capture boundary source differs from construction capture value");
                },
                else => debug.invariant(false, "lambda-solved invariant violated: callable construction capture transform has non-capture kind"),
            }
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

    pub fn deinit(self: *RepresentationSolveSession, allocator: std.mem.Allocator) void {
        if (self.members.len > 0) allocator.free(self.members);
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

pub fn captureShapeKeyForExecKeys(
    keys: []const CanonicalExecValueTypeKey,
) CaptureShapeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "capture_shape");
    writeHashU32(&hasher, @intCast(keys.len));
    for (keys, 0..) |key, i| {
        writeHashU32(&hasher, @intCast(i));
        hasher.update(&key.bytes);
    }
    return .{ .bytes = hasher.finalResult() };
}

pub fn captureTupleExecKeyForSlots(
    slots: []const CallableSetCaptureSlot,
) CanonicalExecValueTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("capture_tuple");
    for (slots, 0..) |slot, i| {
        if (slot.slot != @as(u32, @intCast(i))) {
            representationInvariant("lambda-solved capture tuple key requested for non-canonical capture slots");
        }
        hasher.update(&slot.exec_value_ty.bytes);
    }
    return .{ .bytes = hasher.finalResult() };
}

pub fn finiteCallableSetExecValueTypeKey(key: CanonicalCallableSetKey) CanonicalExecValueTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "callable_set");
    hasher.update(&key.bytes);
    return .{ .bytes = hasher.finalResult() };
}

pub fn erasedCallableExecValueTypeKey(sig_key: ErasedFnSigKey) CanonicalExecValueTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "erased_fn");
    writeErasedFnSigKey(&hasher, sig_key);
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

        const info = values.values.items[@intFromEnum(value)];
        if (info.callable == null and info.boxed == null and info.aggregate == null) {
            if (info.value_alias_source) |source| {
                const source_endpoint = try self.endpointForValue(source);
                if (!canonicalExecValueTypeKeyEql(source_endpoint.key, key)) {
                    representationInvariant("session executable type payload alias source key disagrees with alias value key");
                }
                return source_endpoint;
            }
            if (info.join_info) |join_id| {
                const join_endpoint = try self.endpointForJoin(value, join_id);
                if (!canonicalExecValueTypeKeyEql(join_endpoint.key, key)) {
                    representationInvariant("session executable type payload join input key disagrees with join value key");
                }
                return join_endpoint;
            }
        }

        const id = try self.payload_store.reserve(self.allocator, key);
        try self.active_values.put(value, id);
        errdefer _ = self.active_values.remove(value);

        const payload = if (info.callable) |callable|
            try self.callablePayload(callable)
        else if (info.boxed) |boxed|
            try self.boxedPayload(info.logical_ty, boxed)
        else if (info.aggregate) |aggregate|
            try self.aggregatePayload(info.logical_ty, aggregate)
        else if (self.valueHasFunctionType(info.logical_ty))
            return try self.endpointForCallableClass(info.solved_class orelse representationInvariant("session executable function value has no solved class"))
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

    fn endpointForJoin(
        self: *SessionExecutableTypePayloadBuilder,
        value: ValueInfoId,
        join_id: JoinInfoId,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const values = self.value_store orelse representationInvariant("session executable type payload join has no value store");
        const index = @intFromEnum(join_id);
        if (index >= values.joins.items.len) {
            representationInvariant("session executable type payload join id is out of range");
        }
        const join = values.joins.items[index];
        if (join.result != value) {
            representationInvariant("session executable type payload join is attached to a different result value");
        }
        const inputs = values.sliceJoinInputSpan(join.inputs);
        if (inputs.len == 0) {
            representationInvariant("session executable type payload join has no returning inputs");
        }
        const first = try self.endpointForValue(inputs[0].value);
        for (inputs[1..]) |input| {
            const endpoint = try self.endpointForValue(input.value);
            if (!canonicalExecValueTypeKeyEql(first.key, endpoint.key)) {
                representationInvariant("session executable type payload join inputs have different executable representations");
            }
        }
        return first;
    }

    fn childForType(self: *SessionExecutableTypePayloadBuilder, ty: type_mod.TypeVarId) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const child = try self.endpointForType(ty);
        return .{ .ty = child.ty, .key = child.key };
    }

    fn childForRootType(
        self: *SessionExecutableTypePayloadBuilder,
        root: RepRootId,
        ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const child = try self.endpointForRootType(root, ty);
        return .{ .ty = child.ty, .key = child.key };
    }

    fn childForValue(self: *SessionExecutableTypePayloadBuilder, value: ValueInfoId) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const child = try self.endpointForValue(value);
        return .{ .ty = child.ty, .key = child.key };
    }

    fn endpointForRootType(
        self: *SessionExecutableTypePayloadBuilder,
        root: RepRootId,
        ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        if (self.valueHasFunctionType(ty)) {
            return try self.endpointForCallableClass(self.representation_store.classForRoot(root));
        }
        return try self.endpointForType(ty);
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
                    .source_ty = nominal.source_ty,
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
        return try self.callablePayloadForEmissionPlan(callable.emission_plan);
    }

    fn endpointForCallableClass(
        self: *SessionExecutableTypePayloadBuilder,
        class: RepresentationClassId,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const emission_plan = self.representation_store.callableClassEmission(class) orelse {
            representationInvariant("session executable function value class has no callable emission plan");
        };
        const key = self.callableEmissionPlanKey(emission_plan);
        if (self.payload_store.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }
        const id = try self.payload_store.append(self.allocator, key, try self.callablePayloadForEmissionPlan(emission_plan));
        return .{ .ty = refFor(id), .key = key };
    }

    fn callablePayloadForEmissionPlan(
        self: *SessionExecutableTypePayloadBuilder,
        emission_plan: CallableValueEmissionPlanId,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (self.representation_store.callableEmissionPlan(emission_plan)) {
            .finite => |key| .{ .callable_set = try self.callableSetPayload(key) },
            .already_erased => |erased| .{ .erased_fn = try self.erasedFnPayloadForAlreadyErased(erased) },
            .erase_proc_value => |erase| .{ .erased_fn = try self.erasedFnPayloadForProcValue(erase) },
            .erase_finite_set => |erase| .{ .erased_fn = try self.erasedFnPayloadForFiniteSetAdapter(erase) },
        };
    }

    fn callableEmissionPlanKey(
        self: *SessionExecutableTypePayloadBuilder,
        emission_plan: CallableValueEmissionPlanId,
    ) CanonicalExecValueTypeKey {
        return switch (self.representation_store.callableEmissionPlan(emission_plan)) {
            .finite => |key| finiteCallableSetExecValueTypeKey(key),
            .already_erased => |erased| erasedCallableExecValueTypeKey(erased.sig_key),
            .erase_proc_value => |erase| erasedCallableExecValueTypeKey(erase.erased_fn_sig_key),
            .erase_finite_set => |erase| erasedCallableExecValueTypeKey(erase.adapter.erased_fn_sig_key),
        };
    }

    fn valueHasFunctionType(
        self: *SessionExecutableTypePayloadBuilder,
        ty: type_mod.TypeVarId,
    ) bool {
        const root = self.types.unlinkConst(ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| self.valueHasFunctionType(nominal.backing),
            .content => |content| switch (content) {
                .func => true,
                else => false,
            },
            else => false,
        };
    }

    fn boxedPayload(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
        boxed: BoxedValueInfo,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        const payload_value = boxed.payload_value orelse representationInvariant("session executable boxed value payload has no published payload value");
        const child = try self.childForValue(payload_value);
        const root = self.types.unlinkConst(logical_ty);
        const content = switch (self.types.getNode(root)) {
            .content => |content| content,
            else => representationInvariant("session executable boxed value has non-box logical type"),
        };
        switch (content) {
            .box => {},
            else => representationInvariant("session executable boxed value has non-box content"),
        }
        return .{ .box = child };
    }

    fn callableSetPayload(self: *SessionExecutableTypePayloadBuilder, key: CanonicalCallableSetKey) std.mem.Allocator.Error!SessionExecutableCallableSetPayload {
        const descriptor = self.representation_store.callableSetDescriptor(key) orelse {
            representationInvariant("session callable-set executable payload has no descriptor");
        };
        if (descriptor.members.len == 0) representationInvariant("session callable-set executable payload descriptor is empty");
        const members = try self.allocator.alloc(SessionExecutableCallableSetMemberPayload, descriptor.members.len);
        errdefer self.allocator.free(members);
        for (descriptor.members, 0..) |member, i| {
            members[i] = try self.callableSetMemberPayload(member);
        }
        return .{
            .key = key,
            .members = members,
        };
    }

    fn callableSetMemberPayload(
        self: *SessionExecutableTypePayloadBuilder,
        member: CanonicalCallableSetMember,
    ) std.mem.Allocator.Error!SessionExecutableCallableSetMemberPayload {
        if (member.capture_slots.len == 0) {
            return .{
                .member = member.member,
                .payload_ty = null,
                .payload_ty_key = null,
            };
        }
        const payload = try self.tuplePayloadForCaptureSlots(
            member.capture_slots,
            captureTupleExecKeyForSlots(member.capture_slots),
        );
        return .{
            .member = member.member,
            .payload_ty = payload.ty,
            .payload_ty_key = payload.key,
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
        erase: ProcValueErasePlan,
    ) std.mem.Allocator.Error!SessionExecutableErasedFnPayload {
        const capture = try self.hiddenCapturePayloadForProcValue(erase);
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
        erase: ProcValueErasePlan,
    ) std.mem.Allocator.Error!?SessionExecutableTypeEndpoint {
        if (erase.erased_fn_sig_key.capture_ty == null) {
            if (erase.capture_slots.len != 0) representationInvariant("session proc-value erased payload has captures but no signature capture type");
            return null;
        }
        return try self.tuplePayloadForCaptureSlots(erase.capture_slots, erase.erased_fn_sig_key.capture_ty);
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

    fn tuplePayloadForCaptureSlots(
        self: *SessionExecutableTypePayloadBuilder,
        slots: []const CallableSetCaptureSlot,
        expected_key: ?CanonicalExecValueTypeKey,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        if (slots.len == 0) {
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

        const items = try self.allocator.alloc(SessionExecutableTupleElemPayload, slots.len);
        errdefer self.allocator.free(items);
        for (slots, 0..) |slot, i| {
            if (slot.slot != @as(u32, @intCast(i))) {
                representationInvariant("session proc-value erased payload capture slots are not canonical");
            }
            const child = self.payload_store.refForKey(slot.exec_value_ty) orelse {
                representationInvariant("session proc-value erased payload target capture slot has no executable payload");
            };
            items[i] = .{
                .index = @intCast(i),
                .ty = child,
                .key = slot.exec_value_ty,
            };
        }
        const key = expected_key orelse captureTupleExecKeyForSlots(slots);
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
        const elem_ty = try self.logicalListElemType(logical_ty);
        const elem_endpoint = try self.childForRootType(list.elem_root, elem_ty);
        for (list.elems) |elem| {
            const child = try self.childForValue(elem);
            if (!canonicalExecValueTypeKeyEql(elem_endpoint.key, child.key)) {
                representationInvariant("session executable list payload elements have different executable representations");
            }
        }
        return elem_endpoint;
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

pub fn callableSetKeyForMembers(
    members: []const CanonicalCallableSetMember,
) CanonicalCallableSetKey {
    if (members.len == 1) {
        return singletonCallableSetKey(
            members[0].proc_value,
            members[0].capture_shape_key,
            members[0].capture_slots,
        );
    }

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "finite_callable_set");
    writeHashU32(&hasher, @intCast(members.len));
    for (members) |member| {
        writeHashU32(&hasher, @intFromEnum(member.member));
        writeMirProcedureRef(&hasher, member.source_proc);
        writeProcedureCallableRef(&hasher, member.proc_value);
        hasher.update(&member.capture_shape_key.bytes);
        writeHashU32(&hasher, @intCast(member.capture_slots.len));
        for (member.capture_slots) |slot| {
            writeHashU32(&hasher, slot.slot);
            hasher.update(&slot.source_ty.bytes);
            hasher.update(&slot.exec_value_ty.bytes);
        }
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
        if (try self.redirectedValueKey(value)) |key| return key;
        try self.writeValue(value);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn redirectedValueKey(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!?CanonicalExecValueTypeKey {
        const values = self.value_store orelse return null;
        const info = values.values.items[@intFromEnum(value)];
        if (info.callable != null or info.boxed != null or info.aggregate != null) return null;
        if (info.value_alias_source) |source| return try self.valueKeySnapshot(source);
        if (info.join_info) |join_id| return try self.joinValueKey(value, join_id);
        return null;
    }

    fn joinValueKey(
        self: *ExecValueTypeKeyBuilder,
        value: ValueInfoId,
        join_id: JoinInfoId,
    ) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        const values = self.value_store orelse representationInvariant("executable value type key for join value has no value store");
        const index = @intFromEnum(join_id);
        if (index >= values.joins.items.len) {
            representationInvariant("executable value type key join id is out of range");
        }
        const join = values.joins.items[index];
        if (join.result != value) {
            representationInvariant("executable value type key join is attached to a different result value");
        }
        const inputs = values.sliceJoinInputSpan(join.inputs);
        if (inputs.len == 0) {
            representationInvariant("executable value type key join has no returning inputs");
        }
        const first = try self.valueKeySnapshot(inputs[0].value);
        for (inputs[1..]) |input| {
            const key = try self.valueKeySnapshot(input.value);
            if (!canonicalExecValueTypeKeyEql(first, key)) {
                representationInvariant("executable value type key join inputs have different executable representations");
            }
        }
        return first;
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
        } else if (info.boxed) |boxed| {
            try self.writeBoxedValue(info.logical_ty, boxed);
        } else if (info.aggregate) |aggregate| {
            try self.writeAggregateValue(info.logical_ty, aggregate);
        } else if (info.value_alias_source) |source| {
            try self.writeValue(source);
        } else if (info.join_info) |join_id| {
            const key = try self.joinValueKey(value, join_id);
            self.writeCanonicalExecValueTypeKey(key);
        } else {
            try self.writeType(info.logical_ty);
        }
        _ = self.active_values.remove(value);
    }

    fn writeBoxedValue(
        self: *ExecValueTypeKeyBuilder,
        logical_ty: TypeVarId,
        boxed: BoxedValueInfo,
    ) std.mem.Allocator.Error!void {
        self.writeTag("box_value");
        if (boxed.payload_value) |payload| {
            const payload_key = try self.valueKeySnapshot(payload);
            self.writeCanonicalExecValueTypeKey(payload_key);
            return;
        }
        try self.writeType(logical_ty);
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
                    const field_key = try self.valueKeySnapshot(field.value);
                    self.writeCanonicalExecValueTypeKey(field_key);
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
                for (ordered) |elem| {
                    const elem_key = try self.valueKeySnapshot(elem);
                    self.writeCanonicalExecValueTypeKey(elem_key);
                }
            },
            .tag => |tag| {
                self.writeTag("tag_value");
                self.writeU32(@intFromEnum(tag.union_shape));
                self.writeU32(@intFromEnum(tag.tag));
                self.writeU32(@intCast(tag.payloads.len));
                for (tag.payloads) |payload| {
                    self.writeU32(@intFromEnum(payload.payload));
                    const payload_key = try self.valueKeySnapshot(payload.value);
                    self.writeCanonicalExecValueTypeKey(payload_key);
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
                self.hasher.update(&nominal.source_ty.bytes);
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
        writeErasedFnSigKey(&self.hasher, key);
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

fn writeMirProcedureRef(
    hasher: *std.crypto.hash.sha2.Sha256,
    ref: canonical.MirProcedureRef,
) void {
    hasher.update(&ref.proc.artifact.bytes);
    writeHashU32(hasher, @intFromEnum(ref.proc.proc_base));
    writeProcedureCallableRef(hasher, ref.callable);
}

fn writeErasedFnSigKey(
    hasher: *std.crypto.hash.sha2.Sha256,
    key: ErasedFnSigKey,
) void {
    hasher.update(&key.source_fn_ty.bytes);
    hasher.update(&key.abi.bytes);
    if (key.capture_ty) |capture_ty| {
        hasher.update(&[_]u8{1});
        hasher.update(&capture_ty.bytes);
    } else {
        hasher.update(&[_]u8{0});
    }
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
