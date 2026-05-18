//! Lambda-solved MIR representation/value-flow records.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const symbol_mod = @import("symbol");
const ConcreteSourceType = @import("../concrete_source_type.zig");
const row = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");
const type_mod = @import("type.zig");

const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

pub const CallableVarId = type_mod.CallableVarId;
/// Public `RepRootId` declaration.
pub const RepRootId = enum(u32) { _ };
/// Public `ValueInfoId` declaration.
pub const ValueInfoId = enum(u32) { _ };
/// Public `BindingInfoId` declaration.
pub const BindingInfoId = enum(u32) { _ };
/// Public `ProjectionInfoId` declaration.
pub const ProjectionInfoId = enum(u32) { _ };
/// Public `CallSiteInfoId` declaration.
pub const CallSiteInfoId = enum(u32) { _ };
/// Public `CallValueFiniteDispatchPlanId` declaration.
pub const CallValueFiniteDispatchPlanId = enum(u32) { _ };
/// Public `LowLevelValueFlowSignatureId` declaration.
pub const LowLevelValueFlowSignatureId = enum(u32) { _ };
/// Public `JoinInfoId` declaration.
pub const JoinInfoId = enum(u32) { _ };
/// Public `ReturnInfoId` declaration.
pub const ReturnInfoId = enum(u32) { _ };
pub const BoxBoundaryId = canonical.BoxBoundaryId;
pub const BoxErasureProvenance = checked_artifact.BoxErasureProvenance;
/// Public `BoxPayloadRepresentationPlanId` declaration.
pub const BoxPayloadRepresentationPlanId = enum(u32) { _ };
/// Public `CallableValueEmissionPlanId` declaration.
pub const CallableValueEmissionPlanId = enum(u32) { _ };
/// Public `CallableSetConstructionPlanId` declaration.
pub const CallableSetConstructionPlanId = enum(u32) { _ };
/// Public `CanonicalCallableSetDescriptorId` declaration.
pub const CanonicalCallableSetDescriptorId = enum(u32) { _ };
/// Public `ValueTransformBoundaryId` declaration.
pub const ValueTransformBoundaryId = enum(u32) { _ };
/// Public `ConsumerUsePlanId` declaration.
pub const ConsumerUsePlanId = enum(u32) { _ };
pub const SessionExecutableValueTransformId = checked_artifact.SessionExecutableValueTransformId;
/// Public `RepresentationEdgeId` declaration.
pub const RepresentationEdgeId = enum(u32) { _ };
/// Public `RepresentationRequirementId` declaration.
pub const RepresentationRequirementId = enum(u32) { _ };
/// Public `SourceMatchId` declaration.
pub const SourceMatchId = enum(u32) { _ };
/// Public `SourceMatchBranchId` declaration.
pub const SourceMatchBranchId = enum(u32) { _ };
/// Public `SourceMatchAlternativeId` declaration.
pub const SourceMatchAlternativeId = enum(u32) { _ };
/// Public `SourceMatchBranchReachabilityId` declaration.
pub const SourceMatchBranchReachabilityId = enum(u32) { _ };
/// Public `IfExprId` declaration.
pub const IfExprId = enum(u32) { _ };
/// Public `ProcedureBoundaryId` declaration.
pub const ProcedureBoundaryId = ReturnInfoId;
/// Public `CaptureBoundaryId` declaration.
pub const CaptureBoundaryId = enum(u32) { _ };
/// Public `MutableJoinId` declaration.
pub const MutableJoinId = enum(u32) { _ };
/// Public `LoopPhiId` declaration.
pub const LoopPhiId = enum(u32) { _ };
/// Public `AggregateBoundaryId` declaration.
pub const AggregateBoundaryId = enum(u32) { _ };
/// Public `TransformEndpointScopeId` declaration.
pub const TransformEndpointScopeId = enum(u32) { _ };
/// Public `TransformEndpointPathId` declaration.
pub const TransformEndpointPathId = enum(u32) { _ };
/// Public `RepresentationGroupId` declaration.
pub const RepresentationGroupId = enum(u32) { _ };
/// Public `ProcRepresentationInstanceId` declaration.
pub const ProcRepresentationInstanceId = enum(u32) { _ };
/// Public `RepresentationSolveSessionId` declaration.
pub const RepresentationSolveSessionId = enum(u32) { _ };
/// Public `ValueInfoStoreId` declaration.
pub const ValueInfoStoreId = enum(u32) { _ };
pub const CallableSetMemberId = canonical.CallableSetMemberId;
/// Public `ErasedAdapterId` declaration.
pub const ErasedAdapterId = enum(u32) { _ };
pub const Symbol = symbol_mod.Symbol;
pub const TypeVarId = type_mod.TypeVarId;

/// Public `RepresentationRootKind` declaration.
pub const RepresentationRootKind = union(enum) {
    unassigned,
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

/// Public `RepresentationRootTypeInfo` declaration.
pub const RepresentationRootTypeInfo = struct {
    logical_ty: TypeVarId,
    source_ty: canonical.CanonicalTypeKey,
    source_root: ?ConcreteSourceType.ConcreteSourceTypeRoot = null,
};

/// Public `RepresentationEdgeKind` declaration.
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

/// Public `ProcPublicRootRef` declaration.
pub const ProcPublicRootRef = struct {
    instance: ProcRepresentationInstanceId,
    value: ValueInfoId,
    rep_root: RepRootId,
};

/// Public `ProcPublicFunctionRootRef` declaration.
pub const ProcPublicFunctionRootRef = struct {
    instance: ProcRepresentationInstanceId,
    rep_root: RepRootId,
};

/// Public `RepresentationEndpoint` declaration.
pub const RepresentationEndpoint = union(enum) {
    local: RepRootId,
    procedure_public: ProcPublicRootRef,
    procedure_function_root: ProcPublicFunctionRootRef,
};

/// Public `RepresentationEdge` declaration.
pub const RepresentationEdge = struct {
    from: RepresentationEndpoint,
    to: RepresentationEndpoint,
    kind: RepresentationEdgeKind,
};

const StructuralChildKind = struct {
    tag: enum {
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

const SolvedStructuralChildKey = struct {
    parent_group: RepresentationGroupId,
    kind: StructuralChildKind,
};

/// Public `RepresentationRequirement` declaration.
pub const RepresentationRequirement = union(enum) {
    require_box_erased: BoxErasureRequirement,
};

/// Public `BoxErasureRequirement` declaration.
pub const BoxErasureRequirement = struct {
    payload_root: RepRootId,
    provenance: BoxErasureProvenance,
};

/// Public `TransformEndpointSide` declaration.
pub const TransformEndpointSide = enum {
    from,
    to,
};

/// Public `Span` function.
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

/// Public `LowLevelProjectionPath` declaration.
pub const LowLevelProjectionPath = union(enum) {
    whole_value,
    list_elem,
    box_payload,
    record_field: row.RecordFieldId,
    tuple_elem: u32,
    tag_payload: row.TagPayloadId,
};

/// Public `LowLevelValueFlowEdge` declaration.
pub const LowLevelValueFlowEdge = union(enum) {
    arg_to_result: struct {
        arg: u32,
        projection: LowLevelProjectionPath,
    },
    arg_to_result_projection: struct {
        arg: u32,
        arg_projection: LowLevelProjectionPath,
        result_projection: LowLevelProjectionPath,
    },
    produced_from_args: struct {
        args: Span(u32),
        result_projection: LowLevelProjectionPath,
    },
    fresh_result: LowLevelProjectionPath,
};

/// Public `LowLevelValueFlowSignature` declaration.
pub const LowLevelValueFlowSignature = union(enum) {
    no_value_flow: struct {
        op: base.LowLevel,
        args: Span(ValueInfoId),
        result: ValueInfoId,
    },
    flows: struct {
        op: base.LowLevel,
        args: Span(ValueInfoId),
        result: ValueInfoId,
        edges: Span(LowLevelValueFlowEdge),
        box_boundary: ?BoxBoundaryId = null,
    },
};

pub const CanonicalCallableSetKey = canonical.CanonicalCallableSetKey;
pub const CaptureShapeKey = canonical.CaptureShapeKey;
pub const CanonicalExecValueTypeKey = canonical.CanonicalExecValueTypeKey;
pub const ErasedFnAbiKey = canonical.ErasedFnAbiKey;
pub const ErasedFnAbi = canonical.ErasedFnAbi;
pub const ErasedFnAbiStore = canonical.ErasedFnAbiStore;
pub const ErasedFnSigKey = canonical.ErasedFnSigKey;
pub const CallableSetMemberRef = canonical.CallableSetMemberRef;
pub const CallableSetCaptureSlot = canonical.CallableSetCaptureSlot;
pub const ExecutablePrimitive = checked_artifact.ExecutablePrimitive;

/// Public `CanonicalCallableSetMember` declaration.
pub const CanonicalCallableSetMember = struct {
    member: CallableSetMemberId,
    proc_value: canonical.ProcedureCallableRef,
    source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    source_proc: canonical.MirProcedureRef,
    published_proc_value: ?canonical.ProcedureCallableRef = null,
    published_source_proc: ?canonical.MirProcedureRef = null,
    lifted_owner_source_fn_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    target_instance: ProcRepresentationInstanceId,
    capture_slots: []const CallableSetCaptureSlot,
    capture_shape_key: CaptureShapeKey,
};

/// Public `CanonicalCallableSetDescriptor` declaration.
pub const CanonicalCallableSetDescriptor = struct {
    key: CanonicalCallableSetKey,
    members: []const CanonicalCallableSetMember,
};

/// Public `SessionExecutableTypePayloadId` declaration.
pub const SessionExecutableTypePayloadId = enum(u32) { _ };

const RootTypeKey = struct {
    group: RepresentationGroupId = undefined,
    layer: enum {
        primitive,
        nominal,
        tag_union,
        record,
        tuple,
        list,
        box,
    },
    primitive: u32 = 0,
    nominal_module_name: u32 = 0,
    nominal_type_name: u32 = 0,
    is_opaque: bool = false,
    shape: u32 = 0,
    arity: u32 = 0,
};

/// Public `SessionExecutableTypePayloadRef` declaration.
pub const SessionExecutableTypePayloadRef = struct {
    payload: SessionExecutableTypePayloadId,
};

/// Public `SessionExecutableTypeEndpoint` declaration.
pub const SessionExecutableTypeEndpoint = struct {
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

/// Public `SessionExecutableTypePayloadChild` declaration.
pub const SessionExecutableTypePayloadChild = struct {
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

/// Public `SessionExecutableRecordFieldPayload` declaration.
pub const SessionExecutableRecordFieldPayload = struct {
    field: row.RecordFieldId,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

/// Public `SessionExecutableRecordPayload` declaration.
pub const SessionExecutableRecordPayload = struct {
    shape: row.RecordShapeId,
    fields: []const SessionExecutableRecordFieldPayload = &.{},
};

/// Public `SessionExecutableTupleElemPayload` declaration.
pub const SessionExecutableTupleElemPayload = struct {
    index: u32,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

/// Public `SessionExecutableTagPayload` declaration.
pub const SessionExecutableTagPayload = struct {
    payload: row.TagPayloadId,
    ty: SessionExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

/// Public `SessionExecutableTagVariantPayload` declaration.
pub const SessionExecutableTagVariantPayload = struct {
    tag: row.TagId,
    payloads: []const SessionExecutableTagPayload = &.{},
};

/// Public `SessionExecutableTagUnionPayload` declaration.
pub const SessionExecutableTagUnionPayload = struct {
    shape: row.TagUnionShapeId,
    variants: []const SessionExecutableTagVariantPayload = &.{},
};

/// Public `SessionExecutableNominalPayload` declaration.
pub const SessionExecutableNominalPayload = struct {
    nominal: canonical.NominalTypeKey,
    source_ty: canonical.CanonicalTypeKey,
    is_opaque: bool,
    backing: SessionExecutableTypePayloadRef,
    backing_key: CanonicalExecValueTypeKey,
};

/// Public `SessionExecutableCallableSetMemberPayload` declaration.
pub const SessionExecutableCallableSetMemberPayload = struct {
    member: CallableSetMemberId,
    payload_ty: ?SessionExecutableTypePayloadRef = null,
    payload_ty_key: ?CanonicalExecValueTypeKey = null,
};

/// Public `SessionExecutableCallableSetPayload` declaration.
pub const SessionExecutableCallableSetPayload = struct {
    key: CanonicalCallableSetKey,
    members: []const SessionExecutableCallableSetMemberPayload = &.{},
};

/// Public `SessionExecutableErasedFnPayload` declaration.
pub const SessionExecutableErasedFnPayload = struct {
    sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    capture_ty: ?SessionExecutableTypePayloadRef = null,
    capture_ty_key: ?CanonicalExecValueTypeKey = null,
};

/// Public `SessionExecutableTypePayload` declaration.
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
    vacant_callable_slot,
    recursive_ref: SessionExecutableTypePayloadId,
};

/// Public `SessionExecutableTypePayloadEntry` declaration.
pub const SessionExecutableTypePayloadEntry = struct {
    key: CanonicalExecValueTypeKey,
    payload: SessionExecutableTypePayload,
};

/// Public `SessionExecutableTypePayloadStore` declaration.
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
            const index = @intFromEnum(existing);
            if (index >= self.entries.len) representationInvariant("session executable type payload duplicate id out of range");
            switch (self.entries[index].payload) {
                .pending => self.fill(allocator, existing, payload),
                else => {
                    var duplicate = payload;
                    deinitSessionExecutableTypePayload(allocator, &duplicate);
                },
            }
            return existing;
        }
        return try self.appendNew(allocator, key, payload);
    }

    pub fn replaceDerived(
        self: *SessionExecutableTypePayloadStore,
        allocator: std.mem.Allocator,
        key: CanonicalExecValueTypeKey,
        payload: SessionExecutableTypePayload,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadId {
        if (self.by_key.get(key)) |existing| {
            self.fill(allocator, existing, payload);
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

    pub fn isPending(self: *const SessionExecutableTypePayloadStore, id: SessionExecutableTypePayloadId) bool {
        const index = @intFromEnum(id);
        if (index >= self.entries.len) representationInvariant("session executable type payload id out of range");
        return switch (self.entries[index].payload) {
            .pending => true,
            else => false,
        };
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
        .vacant_callable_slot,
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

fn structuralChildKindFromEdgeKind(kind: RepresentationEdgeKind) ?StructuralChildKind {
    return switch (kind) {
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
        .value_alias,
        .value_move,
        .function_arg,
        .function_return,
        .function_callable,
        .branch_join,
        .loop_phi,
        .mutable_version,
        => null,
    };
}

/// Public `CallableMemberInstanceId` declaration.
pub const CallableMemberInstanceId = struct {
    proc_base: canonical.ProcBaseKeyRef,
    mono_specialization: canonical.MonoSpecializationKey,
    lambda_solved_instance: ProcRepresentationInstanceId,
};

pub const CallableRepresentation = canonical.CallableRepresentation;

pub const CallableReprMode = canonical.CallableReprMode;
pub const ExecutableSpecializationKey = canonical.ExecutableSpecializationKey;

/// Public `RepresentationShape` declaration.
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

/// Public `BoxBoundaryDirection` declaration.
pub const BoxBoundaryDirection = enum {
    box,
    unbox,
};

/// Public `BoxPayloadCapabilityRef` declaration.
pub const BoxPayloadCapabilityRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    capability: checked_artifact.BoxPayloadCapabilityId,
};

/// Public `OpaqueAtomicProofRef` declaration.
pub const OpaqueAtomicProofRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    proof: checked_artifact.OpaqueAtomicProofId,
};

/// Public `HostedRepresentationCapabilityRef` declaration.
pub const HostedRepresentationCapabilityRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    capability: checked_artifact.HostedRepresentationCapabilityId,
};

/// Public `PlatformRepresentationCapabilityRef` declaration.
pub const PlatformRepresentationCapabilityRef = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    capability: checked_artifact.PlatformRepresentationCapabilityId,
};

/// Public `NominalPayloadRepresentation` declaration.
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

/// Public `BoxPayloadRepresentationPlan` declaration.
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
    recursive_ref: BoxPayloadRepresentationPlanId,
};

/// Public `BoxPayloadFieldPlan` declaration.
pub const BoxPayloadFieldPlan = struct {
    field: row.RecordFieldId,
    plan: BoxPayloadRepresentationPlanId,
};

/// Public `BoxPayloadTupleElemPlan` declaration.
pub const BoxPayloadTupleElemPlan = struct {
    index: u32,
    plan: BoxPayloadRepresentationPlanId,
};

/// Public `BoxPayloadTagPlan` declaration.
pub const BoxPayloadTagPlan = struct {
    tag: row.TagId,
    payloads: []const BoxPayloadTagPayloadPlan,
};

/// Public `BoxPayloadTagPayloadPlan` declaration.
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
        .recursive_ref,
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

/// Public `BoxBoundary` declaration.
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

/// Public `ProcValueErasePlan` declaration.
pub const ProcValueErasePlan = struct {
    source_value: ValueInfoId,
    proc_value: canonical.ProcedureCallableRef,
    target_instance: ProcRepresentationInstanceId,
    erased_fn_sig_key: ErasedFnSigKey,
    executable_specialization_key: ExecutableSpecializationKey,
    capture_shape_key: CaptureShapeKey,
    adapter_arg_transforms: []const ValueTransformBoundaryId = &.{},
    capture_slots: []const CallableSetCaptureSlot = &.{},
    capture_transforms: []const ValueTransformBoundaryId = &.{},
    provenance: []const BoxErasureProvenance,
};

/// Public `AlreadyErasedCapturePlan` declaration.
pub const AlreadyErasedCapturePlan = union(enum) {
    none,
    zero_sized_ty: TypeVarId,
    executable_key: CanonicalExecValueTypeKey,
    materialized_capture: checked_artifact.ArtifactErasedCaptureMaterializationRef,
    value: ValueInfoId,
};

/// Public `AlreadyErasedCallablePlan` declaration.
pub const AlreadyErasedCallablePlan = struct {
    sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    result_ty: CanonicalExecValueTypeKey,
    capture: AlreadyErasedCapturePlan = .none,
    provenance: []const BoxErasureProvenance = &.{},
};

pub const ErasedAdapterKey = canonical.ErasedAdapterKey;

/// Public `FiniteSetEraseAdapterBranchPlan` declaration.
pub const FiniteSetEraseAdapterBranchPlan = struct {
    member: CallableSetMemberRef,
    target_instance: ProcRepresentationInstanceId,
    arg_transforms: []const ValueTransformBoundaryId = &.{},
    capture_transforms: []const ValueTransformBoundaryId = &.{},
    result_transform: ?ValueTransformBoundaryId = null,
};

/// Public `FiniteSetErasePlan` declaration.
pub const FiniteSetErasePlan = struct {
    adapter: ErasedAdapterKey,
    result_ty: CanonicalExecValueTypeKey,
    member_targets: []const ExecutableSpecializationKey = &.{},
    branches: []const FiniteSetEraseAdapterBranchPlan = &.{},
    provenance: []const BoxErasureProvenance = &.{},
};

/// Public `FiniteErasedAdapterDemandId` declaration.
pub const FiniteErasedAdapterDemandId = enum(u32) { _ };

/// Public `FiniteErasedAdapterDemand` declaration.
pub const FiniteErasedAdapterDemand = struct {
    adapter: ErasedAdapterKey,
    result_ty: CanonicalExecValueTypeKey,
    member_targets: []const ExecutableSpecializationKey = &.{},
    provenance: []const BoxErasureProvenance = &.{},
};

/// Public `CallableValueEmissionPlan` declaration.
pub const CallableValueEmissionPlan = union(enum) {
    pending_proc_value: CallableSetConstructionPlanId,
    finite: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erase_proc_value: ProcValueErasePlan,
    erase_finite_set: FiniteSetErasePlan,
};

/// Public `CallableValueSource` declaration.
pub const CallableValueSource = union(enum) {
    proc_value: struct {
        proc: canonical.MirProcedureRef,
        published_proc: ?canonical.MirProcedureRef = null,
        target_instance: ProcRepresentationInstanceId,
        captures: []const ValueInfoId,
        fn_ty: canonical.CanonicalTypeKey,
        source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    },
    finite_set: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erased_adapter: ErasedAdapterKey,
};

/// Public `CallableSetConstructionPlan` declaration.
pub const CallableSetConstructionPlan = struct {
    result: ValueInfoId,
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    selected_member: CallableSetMemberId,
    target_instance: ProcRepresentationInstanceId,
    capture_values: []const ValueInfoId,
    capture_transforms: []const ValueTransformBoundaryId = &.{},
};

/// Public `CaptureBoundaryOwner` declaration.
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

/// Public `CaptureBoundaryInfo` declaration.
pub const CaptureBoundaryInfo = struct {
    owner: CaptureBoundaryOwner,
    target_instance: ProcRepresentationInstanceId,
    slot: u32,
    source_capture_value: ValueInfoId,
    target_capture_value: ValueInfoId,
    boundary: ValueTransformBoundaryId,
};

/// Public `CallableValueInfo` declaration.
pub const CallableValueInfo = struct {
    whole_function_root: RepRootId,
    callable_root: RepRootId,
    source: CallableValueSource,
    emission_plan: CallableValueEmissionPlanId,
    construction_plan: ?CallableSetConstructionPlanId = null,
};

/// Public `BoxedValueInfo` declaration.
pub const BoxedValueInfo = struct {
    box_root: RepRootId,
    payload_root: RepRootId,
    payload_value: ?ValueInfoId = null,
    boundary: ?BoxBoundaryId = null,
};

/// Public `AggregateValueInfo` declaration.
pub const AggregateValueInfo = union(enum) {
    record: struct {
        shape: row.RecordShapeId,
        fields: []FieldValueInfo,
    },
    tuple: []ElemValueInfo,
    tag: struct {
        union_shape: row.TagUnionShapeId,
        tag: row.TagId,
        payloads: []TagPayloadValueInfo,
        payload_roots: []const TagPayloadRootInfo = &.{},
    },
    list: struct {
        elem_root: RepRootId,
        elems: []ElemValueInfo,
    },
};

/// Public `TransformEndpointPathStep` declaration.
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

/// Public `TransformChildEndpoint` declaration.
pub const TransformChildEndpoint = struct {
    scope: TransformEndpointScopeId,
    side: TransformEndpointSide,
    path: TransformEndpointPathId,
};

/// Public `ConsumerUseOwner` declaration.
pub const ConsumerUseOwner = union(enum) {
    return_value: ReturnInfoId,
    call_arg: struct {
        call: CallSiteInfoId,
        arg_index: u32,
    },
    record_field: struct {
        parent: ValueInfoId,
        field: row.RecordFieldId,
    },
    tuple_elem: struct {
        parent: ValueInfoId,
        index: u32,
    },
    tag_payload: struct {
        parent: ValueInfoId,
        tag: row.TagId,
        payload: row.TagPayloadId,
    },
    list_elem: struct {
        parent: ValueInfoId,
        index: u32,
    },
    nominal_backing: struct {
        parent: ValueInfoId,
        nominal: canonical.NominalTypeKey,
    },
    if_branch_result: struct {
        parent: ValueInfoId,
        join: JoinInfoId,
        branch: IfBranch,
    },
    source_match_branch_result: struct {
        parent: ValueInfoId,
        join: JoinInfoId,
        branch_index: u32,
    },
};

/// Public `ConsumerUseLowering` declaration.
pub const ConsumerUseLowering = union(enum) {
    construct_directly,
    lower_control_flow_contextually,
    existing_value: ValueTransformBoundaryId,
};

/// Public `ConsumerUsePlan` declaration.
pub const ConsumerUsePlan = struct {
    owner: ConsumerUseOwner,
    child_value: ValueInfoId,
    expected_endpoint: SessionExecutableValueEndpoint,
    lowering: ConsumerUseLowering,
};

/// Public `SessionExecutableValueEndpointOwner` declaration.
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
    erased_proc_value_adapter_arg: struct {
        emission_plan: CallableValueEmissionPlanId,
        source_value: ValueInfoId,
        proc_value: canonical.ProcedureCallableRef,
        erased_fn_sig_key: ErasedFnSigKey,
        index: u32,
    },
    erased_finite_adapter_arg: struct {
        adapter: ErasedAdapterKey,
        member: CallableSetMemberRef,
        index: u32,
    },
    erased_finite_adapter_capture: struct {
        adapter: ErasedAdapterKey,
        member: CallableSetMemberRef,
        slot: u32,
    },
    erased_finite_adapter_result: struct {
        adapter: ErasedAdapterKey,
        member: CallableSetMemberRef,
    },
    call_raw_result: CallSiteInfoId,
    projection_slot: ProjectionInfoId,
    consumer_use: ConsumerUseOwner,
    transform_child: TransformChildEndpoint,
};

/// Public `SessionExecutableValueEndpoint` declaration.
pub const SessionExecutableValueEndpoint = struct {
    owner: SessionExecutableValueEndpointOwner,
    logical_ty: TypeVarId,
    exec_ty: SessionExecutableTypeEndpoint,
};

/// Public `TransformEndpointScope` declaration.
pub const TransformEndpointScope = struct {
    root_kind: ValueTransformBoundaryKind,
    root_from: SessionExecutableValueEndpoint,
    root_to: SessionExecutableValueEndpoint,
};

/// Public `SessionValueTransformRecordField` declaration.
pub const SessionValueTransformRecordField = struct {
    field: row.RecordFieldId,
    transform: checked_artifact.ExecutableValueTransformRef,
};

/// Public `SessionValueTransformTupleElem` declaration.
pub const SessionValueTransformTupleElem = struct {
    index: u32,
    transform: checked_artifact.ExecutableValueTransformRef,
};

/// Public `SessionValueTransformTagPayloadEdge` declaration.
pub const SessionValueTransformTagPayloadEdge = struct {
    source_payload_index: u32,
    target_payload_index: u32,
    transform: checked_artifact.ExecutableValueTransformRef,
};

/// Public `SessionValueTransformTagCase` declaration.
pub const SessionValueTransformTagCase = struct {
    source_tag: row.TagId,
    target_tag: row.TagId,
    payloads: []const SessionValueTransformTagPayloadEdge = &.{},
};

/// Public `SessionBoxPayloadTransformPlan` declaration.
pub const SessionBoxPayloadTransformPlan = struct {
    boundary: ?BoxBoundaryId,
    kind: checked_artifact.BoxPayloadTransformKind,
    payload: checked_artifact.ExecutableValueTransformRef,
};

/// Public `SessionExecutableStructuralBridgePlan` declaration.
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

/// Public `SessionCallableToErasedTransformPlan` declaration.
pub const SessionCallableToErasedTransformPlan = union(enum) {
    finite_value: FiniteSetErasePlan,
    proc_value: ProcValueErasePlan,
};

/// Public `SessionExecutableValueTransformOp` declaration.
pub const SessionExecutableValueTransformOp = union(enum) {
    identity,
    structural_bridge: SessionExecutableStructuralBridgePlan,
    record: []const SessionValueTransformRecordField,
    tuple: []const SessionValueTransformTupleElem,
    tag_union: []const SessionValueTransformTagCase,
    nominal: struct {
        nominal: canonical.NominalTypeKey,
        source_ty: canonical.CanonicalTypeKey,
        backing: checked_artifact.ExecutableValueTransformRef,
    },
    list: struct {
        elem: checked_artifact.ExecutableValueTransformRef,
    },
    box_payload: SessionBoxPayloadTransformPlan,
    callable_to_erased: SessionCallableToErasedTransformPlan,
    already_erased_callable: checked_artifact.AlreadyErasedCallableTransformPlan,
};

/// Public `SessionExecutableValueTransformPlan` declaration.
pub const SessionExecutableValueTransformPlan = struct {
    scope: ?TransformEndpointScopeId = null,
    from: SessionExecutableValueEndpoint,
    to: SessionExecutableValueEndpoint,
    provenance: checked_artifact.ValueTransformProvenance = .none,
    op: SessionExecutableValueTransformOp,
};

/// Public `SessionExecutableValueTransformStore` declaration.
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
                try allocator.dupe(checked_artifact.BoxErasureProvenance, boundaries),
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
        .source_tag = undefined,
        .target_tag = undefined,
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
        .finite_value => |finite| blk: {
            const member_targets = try cloneExecutableSpecializationKeySlice(allocator, finite.member_targets);
            errdefer {
                for (member_targets) |target| {
                    var key = target;
                    deinitExecutableSpecializationKey(allocator, &key);
                }
                if (member_targets.len > 0) allocator.free(member_targets);
            }
            const provenance = if (finite.provenance.len == 0)
                &.{}
            else
                try allocator.dupe(BoxErasureProvenance, finite.provenance);
            errdefer if (provenance.len > 0) allocator.free(provenance);
            const branches = try cloneFiniteSetEraseAdapterBranches(allocator, finite.branches);
            errdefer deinitFiniteSetEraseAdapterBranches(allocator, branches);
            break :blk .{ .finite_value = .{
                .adapter = finite.adapter,
                .result_ty = finite.result_ty,
                .member_targets = member_targets,
                .branches = branches,
                .provenance = provenance,
            } };
        },
        .proc_value => |proc| blk: {
            var key = try cloneExecutableSpecializationKey(allocator, proc.executable_specialization_key);
            errdefer deinitExecutableSpecializationKey(allocator, &key);

            const adapter_arg_transforms = if (proc.adapter_arg_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(ValueTransformBoundaryId, proc.adapter_arg_transforms);
            errdefer if (adapter_arg_transforms.len > 0) allocator.free(adapter_arg_transforms);

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
                try allocator.dupe(BoxErasureProvenance, proc.provenance);
            errdefer if (provenance.len > 0) allocator.free(provenance);

            break :blk .{ .proc_value = .{
                .source_value = proc.source_value,
                .proc_value = proc.proc_value,
                .target_instance = proc.target_instance,
                .erased_fn_sig_key = proc.erased_fn_sig_key,
                .executable_specialization_key = key,
                .capture_shape_key = proc.capture_shape_key,
                .adapter_arg_transforms = adapter_arg_transforms,
                .capture_slots = capture_slots,
                .capture_transforms = capture_transforms,
                .provenance = provenance,
            } };
        },
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
            for (finite.member_targets) |target| {
                var key = target;
                deinitExecutableSpecializationKey(allocator, &key);
            }
            if (finite.member_targets.len > 0) allocator.free(finite.member_targets);
            deinitFiniteSetEraseAdapterBranches(allocator, finite.branches);
            if (finite.provenance.len > 0) allocator.free(finite.provenance);
        },
        .proc_value => |proc| {
            var key = proc.executable_specialization_key;
            deinitExecutableSpecializationKey(allocator, &key);
            if (proc.adapter_arg_transforms.len > 0) allocator.free(proc.adapter_arg_transforms);
            if (proc.capture_slots.len > 0) allocator.free(proc.capture_slots);
            if (proc.capture_transforms.len > 0) allocator.free(proc.capture_transforms);
            if (proc.provenance.len > 0) allocator.free(proc.provenance);
        },
    }
    plan.* = undefined;
}

fn deinitFiniteSetErasePlan(
    allocator: std.mem.Allocator,
    plan: FiniteSetErasePlan,
) void {
    for (plan.member_targets) |target| {
        var key = target;
        deinitExecutableSpecializationKey(allocator, &key);
    }
    if (plan.member_targets.len > 0) allocator.free(plan.member_targets);
    deinitFiniteSetEraseAdapterBranches(allocator, plan.branches);
    if (plan.provenance.len > 0) allocator.free(plan.provenance);
}

fn deinitCallableEmissionPlan(
    allocator: std.mem.Allocator,
    plan: CallableValueEmissionPlan,
) void {
    switch (plan) {
        .already_erased => |erased| {
            if (erased.provenance.len > 0) allocator.free(erased.provenance);
        },
        .erase_proc_value => |erase| {
            var key = erase.executable_specialization_key;
            deinitExecutableSpecializationKey(allocator, &key);
            if (erase.adapter_arg_transforms.len > 0) allocator.free(erase.adapter_arg_transforms);
            if (erase.capture_slots.len > 0) allocator.free(erase.capture_slots);
            if (erase.capture_transforms.len > 0) allocator.free(erase.capture_transforms);
            if (erase.provenance.len > 0) allocator.free(erase.provenance);
        },
        .erase_finite_set => |erase| deinitFiniteSetErasePlan(allocator, erase),
        .pending_proc_value,
        .finite,
        => {},
    }
}

/// Clone finite-set erased adapter branch plans, including owned arg-transform spans.
pub fn cloneFiniteSetEraseAdapterBranches(
    allocator: std.mem.Allocator,
    branches: []const FiniteSetEraseAdapterBranchPlan,
) std.mem.Allocator.Error![]const FiniteSetEraseAdapterBranchPlan {
    if (branches.len == 0) return &.{};
    const cloned = try allocator.alloc(FiniteSetEraseAdapterBranchPlan, branches.len);
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
    errdefer deinitFiniteSetEraseAdapterBranches(allocator, cloned);
    for (branches, 0..) |branch, i| {
        cloned[i] = .{
            .member = branch.member,
            .target_instance = branch.target_instance,
            .arg_transforms = if (branch.arg_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(ValueTransformBoundaryId, branch.arg_transforms),
            .capture_transforms = if (branch.capture_transforms.len == 0)
                &.{}
            else
                try allocator.dupe(ValueTransformBoundaryId, branch.capture_transforms),
            .result_transform = branch.result_transform,
        };
    }
    return cloned;
}

/// Release finite-set erased adapter branch plans cloned with `cloneFiniteSetEraseAdapterBranches`.
pub fn deinitFiniteSetEraseAdapterBranches(
    allocator: std.mem.Allocator,
    branches: []const FiniteSetEraseAdapterBranchPlan,
) void {
    for (branches) |branch| {
        if (branch.arg_transforms.len > 0) allocator.free(branch.arg_transforms);
        if (branch.capture_transforms.len > 0) allocator.free(branch.capture_transforms);
    }
    if (branches.len > 0) allocator.free(branches);
}

/// Public `FieldValueInfo` declaration.
pub const FieldValueInfo = struct {
    field: row.RecordFieldId,
    value: ValueInfoId,
    consumer_use: ?ConsumerUsePlanId = null,
};

/// Public `ElemValueInfo` declaration.
pub const ElemValueInfo = struct {
    index: u32,
    value: ValueInfoId,
    consumer_use: ?ConsumerUsePlanId = null,
};

/// Public `TagPayloadValueInfo` declaration.
pub const TagPayloadValueInfo = struct {
    payload: row.TagPayloadId,
    value: ValueInfoId,
    consumer_use: ?ConsumerUsePlanId = null,
};

/// Public `TagPayloadRootInfo` declaration.
pub const TagPayloadRootInfo = struct {
    payload: row.TagPayloadId,
    root: RepRootId,
};

/// Public `SourceMatchBranchRef` declaration.
pub const SourceMatchBranchRef = struct {
    match: SourceMatchId,
    branch: SourceMatchBranchId,
    alternative: SourceMatchAlternativeId,
};

/// Public `SourceMatchBranchReachability` declaration.
pub const SourceMatchBranchReachability = struct {
    ref: SourceMatchBranchRef,
    reachable: bool = true,
};

/// Public `ConstBackedValueInfo` declaration.
pub const ConstBackedValueInfo = struct {
    const_instance: checked_artifact.ConstInstanceRef,
    schema: checked_artifact.ComptimeSchemaId,
    value: checked_artifact.ComptimeValueId,
};

/// Public `ValueInfo` declaration.
pub const ValueInfo = struct {
    logical_ty: TypeVarId,
    source_ty: canonical.CanonicalTypeKey,
    source_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef = null,
    root: RepRootId,
    solved_group: ?RepresentationGroupId = null,
    exec_ty: ?SessionExecutableTypeEndpoint = null,
    value_alias_source: ?ValueInfoId = null,
    value_alias_needs_executable_transform: bool = false,
    value_alias_transform: ?ValueTransformBoundaryId = null,
    nominal_backing_value: ?ValueInfoId = null,
    projection_info: ?ProjectionInfoId = null,
    join_info: ?JoinInfoId = null,
    callable: ?CallableValueInfo = null,
    boxed: ?BoxedValueInfo = null,
    aggregate: ?AggregateValueInfo = null,
    const_backing: ?ConstBackedValueInfo = null,
    nominal_backing_consumer_use: ?ConsumerUsePlanId = null,
    source_match_branch: ?SourceMatchBranchRef = null,
    pending_comptime_dependency_origin: bool = false,
};

/// Public `BindingInfo` declaration.
pub const BindingInfo = struct {
    symbol: Symbol,
    value: ValueInfoId,
    root: RepRootId,
};

/// Public `ProjectionKind` declaration.
pub const ProjectionKind = union(enum) {
    record_field: row.RecordFieldId,
    tuple_elem: u32,
    tag_payload: row.TagPayloadId,
};

/// Public `ProjectionInfo` declaration.
pub const ProjectionInfo = struct {
    source: ValueInfoId,
    result: ValueInfoId,
    root: RepRootId,
    kind: ProjectionKind,
    endpoint_kind: ?ProjectionKind = null,
    result_transform: ?ValueTransformBoundaryId = null,
};

/// Public `CallSiteDispatch` declaration.
pub const CallSiteDispatch = union(enum) {
    call_proc: ProcRepresentationInstanceId,
    call_value_finite: CallValueFiniteDispatchPlanId,
    call_value_erased: ErasedFnSigKey,
    pending_comptime_dependency_call,
};

/// Public `CallValueFiniteDispatchBranch` declaration.
pub const CallValueFiniteDispatchBranch = struct {
    member: CallableSetMemberRef,
    target_instance: ProcRepresentationInstanceId,
    arg_transforms: Span(ValueTransformBoundaryId),
    result_transform: ValueTransformBoundaryId,
};

/// Public `CallValueFiniteDispatchPlan` declaration.
pub const CallValueFiniteDispatchPlan = struct {
    callable_set_key: CanonicalCallableSetKey,
    branches: Span(CallValueFiniteDispatchBranch),
};

/// Public `CallSiteInfo` declaration.
pub const CallSiteInfo = struct {
    callee: ?ValueInfoId,
    args: Span(ValueInfoId),
    result: ValueInfoId,
    requested_fn_root: RepRootId,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    dispatch: ?CallSiteDispatch = null,
    source_match_branch: ?SourceMatchBranchRef = null,
    representation_edges_resolved: bool = false,
    arg_transforms: Span(ValueTransformBoundaryId) = Span(ValueTransformBoundaryId).empty(),
    arg_consumer_uses: Span(ConsumerUsePlanId) = Span(ConsumerUsePlanId).empty(),
    result_transform: ?ValueTransformBoundaryId = null,
};

/// Public `JoinKind` declaration.
pub const JoinKind = enum {
    if_expr,
    match_expr,
    loop_expr,
};

/// Public `JoinInputSource` declaration.
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

/// Public `JoinInputInfo` declaration.
pub const JoinInputInfo = struct {
    source: JoinInputSource,
    value: ValueInfoId,
};

/// Public `JoinInfo` declaration.
pub const JoinInfo = struct {
    result: ValueInfoId,
    inputs: Span(JoinInputInfo),
    root: RepRootId,
    kind: JoinKind,
    input_transforms: Span(ValueTransformBoundaryId) = Span(ValueTransformBoundaryId).empty(),
    contextual_consumer_uses: Span(ConsumerUsePlanId) = Span(ConsumerUsePlanId).empty(),
};

/// Public `ReturnInfo` declaration.
pub const ReturnInfo = struct {
    value: ValueInfoId,
    consumer_use: ?ConsumerUsePlanId = null,
    transform: ?ValueTransformBoundaryId = null,
};

/// Public `ProcPublicValueRoots` declaration.
pub const ProcPublicValueRoots = struct {
    params: Span(ValueInfoId),
    ret: ValueInfoId,
    captures: Span(ValueInfoId),
    function_root: RepRootId,
};

/// Public `IfBranch` declaration.
pub const IfBranch = enum {
    then_,
    else_,
};

/// Public `ValueTransformBoundaryKind` declaration.
pub const ValueTransformBoundaryKind = union(enum) {
    value_alias: struct {
        source: ValueInfoId,
        result: ValueInfoId,
    },
    call_arg: struct {
        call: CallSiteInfoId,
        arg_index: u32,
    },
    call_result: CallSiteInfoId,
    callable_match_branch_arg: struct {
        call: CallSiteInfoId,
        member: CallableSetMemberRef,
        arg_index: u32,
    },
    erased_proc_value_adapter_arg: struct {
        emission_plan: CallableValueEmissionPlanId,
        source_value: ValueInfoId,
        proc_value: canonical.ProcedureCallableRef,
        erased_fn_sig_key: ErasedFnSigKey,
        index: u32,
    },
    erased_finite_adapter_arg: struct {
        adapter: ErasedAdapterKey,
        member: CallableSetMemberRef,
        index: u32,
    },
    erased_finite_adapter_capture: struct {
        adapter: ErasedAdapterKey,
        member: CallableSetMemberRef,
        slot: u32,
    },
    erased_finite_adapter_result: struct {
        adapter: ErasedAdapterKey,
        member: CallableSetMemberRef,
    },
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
    projection_result: ProjectionInfoId,
    consumer_use: ConsumerUsePlanId,
};

/// Public `ValueTransformBoundary` declaration.
pub const ValueTransformBoundary = struct {
    kind: ValueTransformBoundaryKind,
    from_value: ValueInfoId,
    to_value: ValueInfoId,
    from_endpoint: SessionExecutableValueEndpoint,
    to_endpoint: SessionExecutableValueEndpoint,
    transform: checked_artifact.ExecutableValueTransformRef,
};

/// Public `RepresentationSolveState` declaration.
pub const RepresentationSolveState = enum {
    reserved,
    building,
    solving,
    sealed,
};

/// Public `RepresentationStore` declaration.
pub const RepresentationStore = struct {
    allocator: std.mem.Allocator,
    roots_len: u32 = 0,
    groups_len: u32 = 0,
    root_groups: []RepresentationGroupId = &.{},
    callable_group_emissions: []?CallableValueEmissionPlanId = &.{},
    group_erasure_provenance: [][]const BoxErasureProvenance = &.{},
    root_kinds: std.AutoHashMap(RepRootId, RepresentationRootKind),
    root_type_infos: std.AutoHashMap(RepRootId, RepresentationRootTypeInfo),
    solved_structural_child_roots: std.AutoHashMap(SolvedStructuralChildKey, RepRootId),
    solved_structural_child_roots_published: bool = false,
    representation_edges: std.ArrayList(RepresentationEdge) = .empty,
    representation_requirements: std.ArrayList(RepresentationRequirement) = .empty,
    callable_emission_plans: []CallableValueEmissionPlan = &.{},
    finite_erased_adapter_demands: []FiniteErasedAdapterDemand = &.{},
    callable_construction_plans: []CallableSetConstructionPlan = &.{},
    callable_set_descriptors: []const CanonicalCallableSetDescriptor = &.{},
    session_executable_type_payloads: SessionExecutableTypePayloadStore,
    erased_fn_abis: ErasedFnAbiStore = .{},
    box_payload_plans: []BoxPayloadRepresentationPlan = &.{},
    box_boundaries: []BoxBoundary = &.{},
    capture_boundaries: []CaptureBoundaryInfo = &.{},
    value_transform_boundaries: []const ValueTransformBoundary = &.{},
    consumer_use_plans: std.ArrayList(ConsumerUsePlan) = .empty,
    transform_endpoint_scopes: std.ArrayList(TransformEndpointScope) = .empty,
    transform_endpoint_paths: std.ArrayList(Span(TransformEndpointPathStep)) = .empty,
    transform_endpoint_path_steps: std.ArrayList(TransformEndpointPathStep) = .empty,
    session_value_transforms: SessionExecutableValueTransformStore = .{},

    pub fn init(allocator: std.mem.Allocator) RepresentationStore {
        return .{
            .allocator = allocator,
            .root_kinds = std.AutoHashMap(RepRootId, RepresentationRootKind).init(allocator),
            .root_type_infos = std.AutoHashMap(RepRootId, RepresentationRootTypeInfo).init(allocator),
            .solved_structural_child_roots = std.AutoHashMap(SolvedStructuralChildKey, RepRootId).init(allocator),
            .session_executable_type_payloads = SessionExecutableTypePayloadStore.init(allocator),
        };
    }

    pub fn deinit(self: *RepresentationStore) void {
        self.session_value_transforms.deinit(self.allocator);
        self.transform_endpoint_path_steps.deinit(self.allocator);
        self.transform_endpoint_paths.deinit(self.allocator);
        self.transform_endpoint_scopes.deinit(self.allocator);
        self.consumer_use_plans.deinit(self.allocator);
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
                    if (erase.adapter_arg_transforms.len > 0) self.allocator.free(erase.adapter_arg_transforms);
                    if (erase.capture_slots.len > 0) self.allocator.free(erase.capture_slots);
                    if (erase.capture_transforms.len > 0) self.allocator.free(erase.capture_transforms);
                    if (erase.provenance.len > 0) self.allocator.free(erase.provenance);
                },
                .erase_finite_set => |erase| {
                    for (erase.member_targets) |target| {
                        var key = target;
                        deinitExecutableSpecializationKey(self.allocator, &key);
                    }
                    if (erase.member_targets.len > 0) self.allocator.free(erase.member_targets);
                    deinitFiniteSetEraseAdapterBranches(self.allocator, erase.branches);
                    if (erase.provenance.len > 0) self.allocator.free(erase.provenance);
                },
                .pending_proc_value,
                .finite,
                => {},
            }
        }
        if (self.callable_emission_plans.len > 0) self.allocator.free(self.callable_emission_plans);
        for (self.finite_erased_adapter_demands) |demand| {
            for (demand.member_targets) |target| {
                var key = target;
                deinitExecutableSpecializationKey(self.allocator, &key);
            }
            if (demand.member_targets.len > 0) self.allocator.free(demand.member_targets);
            if (demand.provenance.len > 0) self.allocator.free(demand.provenance);
        }
        if (self.finite_erased_adapter_demands.len > 0) self.allocator.free(self.finite_erased_adapter_demands);
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
        if (self.root_groups.len > 0) self.allocator.free(self.root_groups);
        if (self.callable_group_emissions.len > 0) self.allocator.free(self.callable_group_emissions);
        for (self.group_erasure_provenance) |provenance| {
            if (provenance.len > 0) self.allocator.free(provenance);
        }
        if (self.group_erasure_provenance.len > 0) self.allocator.free(self.group_erasure_provenance);
        self.representation_requirements.deinit(self.allocator);
        self.representation_edges.deinit(self.allocator);
        self.solved_structural_child_roots.deinit();
        self.root_type_infos.deinit();
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

    /// Public `replaceRootKind` function.
    pub fn replaceRootKind(
        self: *RepresentationStore,
        root: RepRootId,
        kind: RepresentationRootKind,
    ) std.mem.Allocator.Error!void {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: representation root kind replaced for an unreserved root");
            unreachable;
        }
        if (!self.root_kinds.contains(root)) {
            debug.invariant(false, "lambda-solved invariant violated: representation root kind replaced before publication");
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
        return self.root_kinds.get(root) orelse .unassigned;
    }

    pub fn publishRootTypeInfo(
        self: *RepresentationStore,
        root: RepRootId,
        info: RepresentationRootTypeInfo,
    ) std.mem.Allocator.Error!void {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: representation root type info published for an unreserved root");
            unreachable;
        }
        if (self.root_type_infos.get(root)) |existing| {
            if (existing.logical_ty != info.logical_ty or
                !canonicalTypeKeyEql(existing.source_ty, info.source_ty))
            {
                debug.invariant(false, "lambda-solved invariant violated: representation root type info was published twice with different types");
                unreachable;
            }
            return;
        }
        try self.root_type_infos.put(root, info);
    }

    pub fn rootTypeInfo(
        self: *const RepresentationStore,
        root: RepRootId,
    ) ?RepresentationRootTypeInfo {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: representation root type info lookup referenced an unreserved root");
            unreachable;
        }
        return self.root_type_infos.get(root);
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
            .procedure_function_root => {},
        }
    }

    pub fn reserveGroup(self: *RepresentationStore) RepresentationGroupId {
        const id: RepresentationGroupId = @enumFromInt(self.groups_len);
        self.groups_len += 1;
        return id;
    }

    pub fn resetSolvedGroups(self: *RepresentationStore) void {
        self.groups_len = 0;
        if (self.root_groups.len > 0) self.allocator.free(self.root_groups);
        self.root_groups = &.{};
        self.solved_structural_child_roots.clearRetainingCapacity();
        self.solved_structural_child_roots_published = false;
        if (self.callable_group_emissions.len > 0) self.allocator.free(self.callable_group_emissions);
        self.callable_group_emissions = &.{};
        for (self.group_erasure_provenance) |provenance| {
            if (provenance.len > 0) self.allocator.free(provenance);
        }
        if (self.group_erasure_provenance.len > 0) self.allocator.free(self.group_erasure_provenance);
        self.group_erasure_provenance = &.{};
    }

    pub fn publishRootGroups(
        self: *RepresentationStore,
        root_groups: []RepresentationGroupId,
    ) std.mem.Allocator.Error!void {
        if (root_groups.len != @as(usize, @intCast(self.roots_len))) {
            debug.invariant(false, "lambda-solved invariant violated: solved root group table length differs from root count");
            unreachable;
        }
        if (self.root_groups.len > 0) {
            debug.invariant(false, "lambda-solved invariant violated: solved root groups were published twice");
            unreachable;
        }
        if (self.solved_structural_child_roots_published) {
            debug.invariant(false, "lambda-solved invariant violated: solved structural child roots were published twice");
            unreachable;
        }
        self.root_groups = root_groups;
        errdefer {
            self.root_groups = &.{};
            self.solved_structural_child_roots.clearRetainingCapacity();
            self.solved_structural_child_roots_published = false;
        }
        try self.publishSolvedStructuralChildRoots();
        self.solved_structural_child_roots_published = true;
    }

    pub fn groupForRoot(self: *const RepresentationStore, root: RepRootId) RepresentationGroupId {
        const root_index = @intFromEnum(root);
        if (root_index >= self.roots_len) {
            debug.invariant(false, "lambda-solved invariant violated: solved group lookup referenced an unreserved root");
            unreachable;
        }
        if (self.root_groups.len != @as(usize, @intCast(self.roots_len))) {
            debug.invariant(false, "lambda-solved invariant violated: solved root groups are not published");
            unreachable;
        }
        return self.root_groups[root_index];
    }

    pub fn solvedStructuralChildRoot(
        self: *const RepresentationStore,
        parent: RepRootId,
        kind: RepresentationEdgeKind,
    ) ?RepRootId {
        if (!self.solved_structural_child_roots_published) {
            debug.invariant(false, "lambda-solved invariant violated: solved structural child roots are not published");
            unreachable;
        }
        const child_kind = structuralChildKindFromEdgeKind(kind) orelse {
            debug.invariant(false, "lambda-solved invariant violated: solved structural child root lookup used a non-structural edge kind");
            unreachable;
        };
        const parent_group = self.groupForRoot(parent);
        return self.solved_structural_child_roots.get(.{
            .parent_group = parent_group,
            .kind = child_kind,
        });
    }

    fn publishSolvedStructuralChildRoots(self: *RepresentationStore) std.mem.Allocator.Error!void {
        self.solved_structural_child_roots.clearRetainingCapacity();
        for (self.representation_edges.items) |edge| {
            const kind = structuralChildKindFromEdgeKind(edge.kind) orelse continue;
            const from = switch (edge.from) {
                .local => |root| root,
                .procedure_public,
                .procedure_function_root,
                => continue,
            };
            const child = switch (edge.to) {
                .local => |root| root,
                .procedure_public,
                .procedure_function_root,
                => {
                    debug.invariant(false, "lambda-solved invariant violated: structural projection edge targets procedure-public root");
                    unreachable;
                },
            };
            const key: SolvedStructuralChildKey = .{
                .parent_group = self.groupForRoot(from),
                .kind = kind,
            };
            const child_group = self.groupForRoot(child);
            const entry = try self.solved_structural_child_roots.getOrPut(key);
            if (entry.found_existing) {
                const existing = entry.value_ptr.*;
                const existing_group = self.groupForRoot(existing);
                if (existing_group != child_group) {
                    debug.invariant(false, "lambda-solved invariant violated: solved structural projection group is ambiguous");
                    unreachable;
                }
                if (@intFromEnum(child) < @intFromEnum(existing)) {
                    entry.value_ptr.* = child;
                }
            } else {
                entry.value_ptr.* = child;
            }
        }
    }

    pub fn publishCallableGroupEmission(
        self: *RepresentationStore,
        group: RepresentationGroupId,
        emission_plan: CallableValueEmissionPlanId,
    ) std.mem.Allocator.Error!void {
        const group_index: usize = @intFromEnum(group);
        if (group_index >= @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable group emission referenced an unreserved group");
            unreachable;
        }
        if (self.callable_group_emissions.len == 0) {
            self.callable_group_emissions = try self.allocator.alloc(?CallableValueEmissionPlanId, @intCast(self.groups_len));
            @memset(self.callable_group_emissions, null);
        }
        if (self.callable_group_emissions.len != @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable group emission table length differs from group count");
            unreachable;
        }
        if (self.callable_group_emissions[group_index]) |existing| {
            if (existing != emission_plan) {
                debug.invariant(false, "lambda-solved invariant violated: callable group emission was published twice with different plans");
                unreachable;
            }
            return;
        }
        self.callable_group_emissions[group_index] = emission_plan;
    }

    pub fn callableGroupEmission(
        self: *const RepresentationStore,
        group: RepresentationGroupId,
    ) ?CallableValueEmissionPlanId {
        const group_index: usize = @intFromEnum(group);
        if (group_index >= @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable group emission lookup referenced an unreserved group");
            unreachable;
        }
        if (self.callable_group_emissions.len == 0) return null;
        if (self.callable_group_emissions.len != @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: callable group emission table length differs from group count");
            unreachable;
        }
        return self.callable_group_emissions[group_index];
    }

    /// Public `publishGroupErasureProvenance` function.
    pub fn publishGroupErasureProvenance(
        self: *RepresentationStore,
        group: RepresentationGroupId,
        provenance: []const BoxErasureProvenance,
    ) std.mem.Allocator.Error!void {
        if (provenance.len == 0) {
            debug.invariant(false, "lambda-solved invariant violated: group erasure provenance publication was empty");
            unreachable;
        }
        const group_index: usize = @intFromEnum(group);
        if (group_index >= @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: group erasure provenance referenced an unreserved group");
            unreachable;
        }
        if (self.group_erasure_provenance.len == 0) {
            self.group_erasure_provenance = try self.allocator.alloc([]const BoxErasureProvenance, @intCast(self.groups_len));
            @memset(self.group_erasure_provenance, &.{});
        }
        if (self.group_erasure_provenance.len != @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: group erasure provenance table length differs from group count");
            unreachable;
        }
        if (self.group_erasure_provenance[group_index].len > 0) {
            self.group_erasure_provenance[group_index] = try mergeOwnedBoxErasureProvenanceSet(
                self.allocator,
                self.group_erasure_provenance[group_index],
                provenance,
            );
            return;
        }
        self.group_erasure_provenance[group_index] = try self.allocator.dupe(BoxErasureProvenance, provenance);
    }

    /// Public `groupErasureProvenance` function.
    pub fn groupErasureProvenance(
        self: *const RepresentationStore,
        group: RepresentationGroupId,
    ) []const BoxErasureProvenance {
        const group_index: usize = @intFromEnum(group);
        if (group_index >= @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: group erasure provenance lookup referenced an unreserved group");
            unreachable;
        }
        if (self.group_erasure_provenance.len == 0) return &.{};
        if (self.group_erasure_provenance.len != @as(usize, @intCast(self.groups_len))) {
            debug.invariant(false, "lambda-solved invariant violated: group erasure provenance table length differs from group count");
            unreachable;
        }
        return self.group_erasure_provenance[group_index];
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

    pub fn setBoxPayloadPlan(
        self: *RepresentationStore,
        id: BoxPayloadRepresentationPlanId,
        plan: BoxPayloadRepresentationPlan,
    ) void {
        const index: usize = @intFromEnum(id);
        if (index >= self.box_payload_plans.len) {
            debug.invariant(false, "lambda-solved invariant violated: boxed payload plan id was out of range");
            unreachable;
        }
        self.box_payload_plans[index] = plan;
    }

    pub fn truncateBoxPayloadPlans(self: *RepresentationStore, len: usize) std.mem.Allocator.Error!void {
        if (len > self.box_payload_plans.len) {
            debug.invariant(false, "lambda-solved invariant violated: boxed payload plan truncation length was out of range");
            unreachable;
        }
        if (len == self.box_payload_plans.len) return;
        var i = len;
        while (i < self.box_payload_plans.len) : (i += 1) {
            deinitBoxPayloadRepresentationPlan(self.allocator, self.box_payload_plans[i]);
        }
        const old = self.box_payload_plans;
        if (len == 0) {
            if (old.len > 0) self.allocator.free(old);
            self.box_payload_plans = &.{};
            return;
        }
        const next = try self.allocator.alloc(BoxPayloadRepresentationPlan, len);
        @memcpy(next[0..len], old[0..len]);
        self.allocator.free(old);
        self.box_payload_plans = next;
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

    /// Public `mergeCallableEmissionPlanProvenance` function.
    pub fn mergeCallableEmissionPlanProvenance(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
        provenance: []const BoxErasureProvenance,
    ) std.mem.Allocator.Error!void {
        if (provenance.len == 0) return;
        const plan = self.callableEmissionPlanPtr(id);
        switch (plan.*) {
            .already_erased => |*erased| {
                erased.provenance = try mergeOwnedBoxErasureProvenanceSet(self.allocator, erased.provenance, provenance);
            },
            .erase_proc_value => |*erase| {
                erase.provenance = try mergeOwnedBoxErasureProvenanceSet(self.allocator, erase.provenance, provenance);
            },
            .erase_finite_set => |*erase| {
                erase.provenance = try mergeOwnedBoxErasureProvenanceSet(self.allocator, erase.provenance, provenance);
            },
            .pending_proc_value,
            .finite,
            => representationInvariant("lambda-solved attempted to merge Box(T) provenance into a non-erased callable emission plan"),
        }
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

    pub fn setProcValueEraseAdapterArgTransforms(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
        transforms: []const ValueTransformBoundaryId,
    ) std.mem.Allocator.Error!void {
        const plan = self.callableEmissionPlanPtr(id);
        switch (plan.*) {
            .erase_proc_value => |*erase| {
                if (erase.adapter_arg_transforms.len > 0) {
                    debug.invariant(false, "lambda-solved invariant violated: proc-value erase adapter arg transforms were already finalized");
                    unreachable;
                }
                erase.adapter_arg_transforms = if (transforms.len == 0)
                    &.{}
                else
                    try self.allocator.dupe(ValueTransformBoundaryId, transforms);
            },
            else => {
                debug.invariant(false, "lambda-solved invariant violated: proc-value erase adapter arg transforms attached to non-proc-value erase plan");
                unreachable;
            },
        }
    }

    pub fn setFiniteSetEraseAdapterBranches(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
        branches: []const FiniteSetEraseAdapterBranchPlan,
    ) std.mem.Allocator.Error!void {
        const plan = self.callableEmissionPlanPtr(id);
        switch (plan.*) {
            .erase_finite_set => |*erase| {
                if (erase.branches.len > 0) {
                    debug.invariant(false, "lambda-solved invariant violated: finite-set erase adapter branches were already finalized");
                    unreachable;
                }
                erase.branches = try cloneFiniteSetEraseAdapterBranches(self.allocator, branches);
            },
            else => {
                debug.invariant(false, "lambda-solved invariant violated: finite-set erase adapter branches attached to non-finite-set erase plan");
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

    pub fn appendConsumerUsePlan(
        self: *RepresentationStore,
        plan: ConsumerUsePlan,
    ) std.mem.Allocator.Error!ConsumerUsePlanId {
        const id: ConsumerUsePlanId = @enumFromInt(@as(u32, @intCast(self.consumer_use_plans.items.len)));
        try self.consumer_use_plans.append(self.allocator, plan);
        return id;
    }

    pub fn setConsumerUsePlanLowering(
        self: *RepresentationStore,
        id: ConsumerUsePlanId,
        lowering: ConsumerUseLowering,
    ) void {
        self.consumer_use_plans.items[@intFromEnum(id)].lowering = lowering;
    }

    pub fn consumerUsePlan(
        self: *const RepresentationStore,
        id: ConsumerUsePlanId,
    ) ConsumerUsePlan {
        return self.consumer_use_plans.items[@intFromEnum(id)];
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
        for (self.callable_emission_plans) |plan| {
            switch (plan) {
                .erase_finite_set => |erase| {
                    const capture_key = erase.adapter.erased_fn_sig_key.capture_ty orelse continue;
                    const maybe_ref = self.session_executable_type_payloads.refForKey(capture_key);
                    debug.invariant(maybe_ref != null, "lambda-solved invariant violated: finite adapter hidden capture key has no session executable type payload");
                    const ref = maybe_ref orelse continue;
                    const payload = self.session_executable_type_payloads.get(ref.payload);
                    switch (payload) {
                        .callable_set => |callable_set| {
                            if (!callableSetKeyEql(callable_set.key, erase.adapter.callable_set_key)) {
                                debug.invariant(false, "lambda-solved invariant violated: finite adapter hidden capture payload key differs from adapter callable set");
                            }
                        },
                        else => debug.invariant(false, "lambda-solved invariant violated: finite adapter hidden capture payload is not a callable set"),
                    }
                },
                else => {},
            }
        }
        for (self.finite_erased_adapter_demands) |demand| {
            if (demand.provenance.len == 0) {
                debug.invariant(false, "lambda-solved invariant violated: finite-erased adapter demand has no Box(T) provenance");
            }
            const descriptor = self.callableSetDescriptor(demand.adapter.callable_set_key) orelse {
                debug.invariant(false, "lambda-solved invariant violated: finite-erased adapter demand referenced missing callable-set descriptor");
                continue;
            };
            if (descriptor.members.len != demand.member_targets.len) {
                debug.invariant(false, "lambda-solved invariant violated: finite-erased adapter demand target count differs from descriptor");
            }
            const abi = self.erased_fn_abis.abiFor(demand.adapter.erased_fn_sig_key.abi) orelse {
                debug.invariant(false, "lambda-solved invariant violated: finite-erased adapter demand referenced missing erased ABI");
                continue;
            };
            for (demand.member_targets) |target| {
                if (target.exec_arg_tys.len != abi.arg_exec_keys.len) {
                    debug.invariant(false, "lambda-solved invariant violated: finite-erased adapter demand target arity differs from erased ABI");
                }
            }
        }
        for (self.callable_construction_plans) |construction| {
            if (construction.capture_values.len != construction.capture_transforms.len) {
                debug.invariant(false, "lambda-solved invariant violated: callable construction capture transform count differs from capture count");
            }
        }
        for (self.session_executable_type_payloads.entries, 0..) |entry, raw_payload| {
            switch (entry.payload) {
                .pending => std.debug.panic("lambda-solved invariant violated: pending executable type payload {d} reached sealed representation store", .{raw_payload}),
                else => {},
            }
        }
        for (self.callable_emission_plans) |plan| {
            switch (plan) {
                .pending_proc_value => {
                    debug.invariant(false, "lambda-solved invariant violated: pending proc-value callable emission reached sealed representation store");
                },
                .erase_proc_value => |erase| {
                    if (erase.adapter_arg_transforms.len != erase.executable_specialization_key.exec_arg_tys.len) {
                        debug.invariant(false, "lambda-solved invariant violated: proc-value erase adapter arg transform count differs from target arg count");
                    }
                    if (erase.capture_slots.len != erase.capture_transforms.len) {
                        debug.invariant(false, "lambda-solved invariant violated: proc-value erase capture transform count differs from capture slot count");
                    }
                },
                .erase_finite_set => |erase| {
                    if (erase.branches.len != erase.member_targets.len) {
                        debug.invariant(false, "lambda-solved invariant violated: finite-set erase branch count differs from member target count");
                    }
                    const abi = self.erased_fn_abis.abiFor(erase.adapter.erased_fn_sig_key.abi) orelse {
                        debug.invariant(false, "lambda-solved invariant violated: finite-set erase branch plan referenced missing ABI");
                        continue;
                    };
                    for (erase.branches) |branch| {
                        if (branch.arg_transforms.len != abi.arg_exec_keys.len) {
                            debug.invariant(false, "lambda-solved invariant violated: finite-set erase branch argument transforms were not finalized");
                        }
                        const member = self.callableSetMember(erase.adapter.callable_set_key, branch.member.member_index) orelse {
                            debug.invariant(false, "lambda-solved invariant violated: finite-set erase branch referenced missing callable member");
                            continue;
                        };
                        if (branch.capture_transforms.len != member.capture_slots.len) {
                            debug.invariant(false, "lambda-solved invariant violated: finite-set erase branch capture transforms were not finalized");
                        }
                        if (branch.result_transform == null) {
                            debug.invariant(false, "lambda-solved invariant violated: finite-set erase branch result transform was not finalized");
                        }
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
        result: ValueInfoId,
        whole_function_root: RepRootId,
        proc: canonical.MirProcedureRef,
        published_proc: ?canonical.MirProcedureRef,
        target_instance: ProcRepresentationInstanceId,
        capture_values: []const ValueInfoId,
        source_fn_ty_payload: ConcreteSourceType.ConcreteSourceTypeRef,
    ) std.mem.Allocator.Error!CallableValueInfo {
        const source_fn_ty = proc.callable.source_fn_ty;
        const construction_plan = try self.appendCallableConstructionPlan(.{
            .result = result,
            .source_fn_ty = source_fn_ty,
            .callable_set_key = .{},
            .selected_member = canonical.onlyCallableSetMemberId(),
            .target_instance = target_instance,
            .capture_values = capture_values,
        });
        const emission_plan = try self.appendCallableEmissionPlan(.{ .pending_proc_value = construction_plan });
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
                .published_proc = published_proc,
                .target_instance = target_instance,
                .captures = construction.capture_values,
                .fn_ty = source_fn_ty,
                .source_fn_ty_payload = source_fn_ty_payload,
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
        const owned = try self.cloneFiniteSetEraseEmissionPayload(erase);

        return try self.appendCallableEmissionPlan(.{ .erase_finite_set = .{
            .adapter = owned.adapter,
            .result_ty = owned.result_ty,
            .member_targets = owned.member_targets,
            .branches = owned.branches,
            .provenance = owned.provenance,
        } });
    }

    pub fn replaceCallableEmissionPlanWithFinite(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
        key: CanonicalCallableSetKey,
    ) void {
        const plan = self.callableEmissionPlanPtr(id);
        deinitCallableEmissionPlan(self.allocator, plan.*);
        plan.* = .{ .finite = key };
    }

    pub fn replaceCallableEmissionPlanWithFiniteSetErase(
        self: *RepresentationStore,
        id: CallableValueEmissionPlanId,
        erase: FiniteSetErasePlan,
    ) std.mem.Allocator.Error!void {
        const owned = try self.cloneFiniteSetEraseEmissionPayload(erase);
        errdefer deinitFiniteSetErasePlan(self.allocator, owned);
        const plan = self.callableEmissionPlanPtr(id);
        deinitCallableEmissionPlan(self.allocator, plan.*);
        plan.* = .{ .erase_finite_set = owned };
    }

    fn cloneFiniteSetEraseEmissionPayload(
        self: *RepresentationStore,
        erase: FiniteSetErasePlan,
    ) std.mem.Allocator.Error!FiniteSetErasePlan {
        const provenance = if (erase.provenance.len == 0)
            &.{}
        else
            try self.allocator.dupe(BoxErasureProvenance, erase.provenance);
        errdefer if (provenance.len > 0) self.allocator.free(provenance);
        const member_targets = try cloneExecutableSpecializationKeySlice(self.allocator, erase.member_targets);
        errdefer {
            for (member_targets) |target| {
                var key = target;
                deinitExecutableSpecializationKey(self.allocator, &key);
            }
            if (member_targets.len > 0) self.allocator.free(member_targets);
        }
        const branches = try cloneFiniteSetEraseAdapterBranches(self.allocator, erase.branches);
        errdefer deinitFiniteSetEraseAdapterBranches(self.allocator, branches);
        return .{
            .adapter = erase.adapter,
            .result_ty = erase.result_ty,
            .member_targets = member_targets,
            .branches = branches,
            .provenance = provenance,
        };
    }

    pub fn ensureFiniteErasedAdapterDemand(
        self: *RepresentationStore,
        demand: FiniteErasedAdapterDemand,
    ) std.mem.Allocator.Error!FiniteErasedAdapterDemandId {
        if (demand.provenance.len == 0) {
            representationInvariant("lambda-solved finite erased adapter demand has no Box(T) provenance");
        }
        for (self.finite_erased_adapter_demands, 0..) |existing, raw| {
            if (!erasedAdapterKeyEql(existing.adapter, demand.adapter)) continue;
            if (!canonicalExecValueTypeKeyEql(existing.result_ty, demand.result_ty)) continue;
            if (!executableSpecializationKeySliceEql(existing.member_targets, demand.member_targets)) continue;
            if (!boxErasureProvenanceSliceEql(existing.provenance, demand.provenance)) continue;
            return @enumFromInt(@as(u32, @intCast(raw)));
        }

        const member_targets = try cloneExecutableSpecializationKeySlice(self.allocator, demand.member_targets);
        errdefer {
            for (member_targets) |target| {
                var key = target;
                deinitExecutableSpecializationKey(self.allocator, &key);
            }
            if (member_targets.len > 0) self.allocator.free(member_targets);
        }
        const provenance = try self.allocator.dupe(BoxErasureProvenance, demand.provenance);
        errdefer self.allocator.free(provenance);

        const old = self.finite_erased_adapter_demands;
        const next = try self.allocator.alloc(FiniteErasedAdapterDemand, old.len + 1);
        @memcpy(next[0..old.len], old);
        if (old.len > 0) self.allocator.free(old);
        const id: FiniteErasedAdapterDemandId = @enumFromInt(@as(u32, @intCast(old.len)));
        next[old.len] = .{
            .adapter = demand.adapter,
            .result_ty = demand.result_ty,
            .member_targets = member_targets,
            .provenance = provenance,
        };
        self.finite_erased_adapter_demands = next;
        return id;
    }

    pub fn finiteErasedAdapterDemand(
        self: *const RepresentationStore,
        id: FiniteErasedAdapterDemandId,
    ) FiniteErasedAdapterDemand {
        return self.finite_erased_adapter_demands[@intFromEnum(id)];
    }

    pub fn appendFiniteCallableEmissionPlan(
        self: *RepresentationStore,
        key: CanonicalCallableSetKey,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        return try self.appendCallableEmissionPlan(.{ .finite = key });
    }

    pub fn appendAlreadyErasedCallableEmissionPlan(
        self: *RepresentationStore,
        erased: AlreadyErasedCallablePlan,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        const provenance = if (erased.provenance.len == 0)
            &.{}
        else
            try self.allocator.dupe(BoxErasureProvenance, erased.provenance);
        errdefer if (provenance.len > 0) self.allocator.free(provenance);

        return try self.appendCallableEmissionPlan(.{ .already_erased = .{
            .sig_key = erased.sig_key,
            .capture_shape_key = erased.capture_shape_key,
            .result_ty = erased.result_ty,
            .capture = erased.capture,
            .provenance = provenance,
        } });
    }

    pub fn appendProcValueEraseEmissionPlan(
        self: *RepresentationStore,
        erase: ProcValueErasePlan,
    ) std.mem.Allocator.Error!CallableValueEmissionPlanId {
        var key = try cloneExecutableSpecializationKey(self.allocator, erase.executable_specialization_key);
        errdefer deinitExecutableSpecializationKey(self.allocator, &key);

        const adapter_arg_transforms = if (erase.adapter_arg_transforms.len == 0)
            &.{}
        else
            try self.allocator.dupe(ValueTransformBoundaryId, erase.adapter_arg_transforms);
        errdefer if (adapter_arg_transforms.len > 0) self.allocator.free(adapter_arg_transforms);

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
            try self.allocator.dupe(BoxErasureProvenance, erase.provenance);
        errdefer if (provenance.len > 0) self.allocator.free(provenance);

        return try self.appendCallableEmissionPlan(.{ .erase_proc_value = .{
            .source_value = erase.source_value,
            .proc_value = erase.proc_value,
            .target_instance = erase.target_instance,
            .erased_fn_sig_key = erase.erased_fn_sig_key,
            .executable_specialization_key = key,
            .capture_shape_key = erase.capture_shape_key,
            .adapter_arg_transforms = adapter_arg_transforms,
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
            .target_instance = plan.target_instance,
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
            .member = undefined,
            .proc_value = members[0].proc_value,
            .source_fn_ty_payload = members[0].source_fn_ty_payload,
            .source_proc = members[0].source_proc,
            .published_proc_value = members[0].published_proc_value,
            .published_source_proc = members[0].published_source_proc,
            .lifted_owner_source_fn_ty_payload = members[0].lifted_owner_source_fn_ty_payload,
            .target_instance = members[0].target_instance,
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
                .source_fn_ty_payload = member.source_fn_ty_payload,
                .source_proc = member.source_proc,
                .published_proc_value = member.published_proc_value,
                .published_source_proc = member.published_source_proc,
                .lifted_owner_source_fn_ty_payload = member.lifted_owner_source_fn_ty_payload,
                .target_instance = member.target_instance,
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
        row_shapes: *row.Store,
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
            const exec_key = try execValueTypeKeyForValue(self.allocator, names, row_shapes, types, self, value_store, value);
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

/// Public `ValueInfoStore` declaration.
pub const ValueInfoStore = struct {
    allocator: std.mem.Allocator,
    values: std.ArrayList(ValueInfo),
    bindings: std.ArrayList(BindingInfo),
    projections: std.ArrayList(ProjectionInfo),
    call_sites: std.ArrayList(CallSiteInfo),
    call_value_finite_dispatches: std.ArrayList(CallValueFiniteDispatchPlan),
    call_value_finite_dispatch_branches: std.ArrayList(CallValueFiniteDispatchBranch),
    low_level_value_flows: std.ArrayList(LowLevelValueFlowSignature),
    low_level_value_flow_edges: std.ArrayList(LowLevelValueFlowEdge),
    low_level_value_flow_arg_indices: std.ArrayList(u32),
    joins: std.ArrayList(JoinInfo),
    returns: std.ArrayList(ReturnInfo),
    join_inputs: std.ArrayList(JoinInputInfo),
    source_match_branch_reachabilities: std.ArrayList(SourceMatchBranchReachability),
    source_match_branch_index: std.AutoHashMap(SourceMatchBranchRef, SourceMatchBranchReachabilityId),
    value_ids: std.ArrayList(ValueInfoId),
    value_transform_boundary_ids: std.ArrayList(ValueTransformBoundaryId),
    consumer_use_plan_ids: std.ArrayList(ConsumerUsePlanId),

    pub fn init(allocator: std.mem.Allocator) ValueInfoStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .bindings = .empty,
            .projections = .empty,
            .call_sites = .empty,
            .call_value_finite_dispatches = .empty,
            .call_value_finite_dispatch_branches = .empty,
            .low_level_value_flows = .empty,
            .low_level_value_flow_edges = .empty,
            .low_level_value_flow_arg_indices = .empty,
            .joins = .empty,
            .returns = .empty,
            .join_inputs = .empty,
            .source_match_branch_reachabilities = .empty,
            .source_match_branch_index = std.AutoHashMap(SourceMatchBranchRef, SourceMatchBranchReachabilityId).init(allocator),
            .value_ids = .empty,
            .value_transform_boundary_ids = .empty,
            .consumer_use_plan_ids = .empty,
        };
    }

    pub fn deinit(self: *ValueInfoStore) void {
        for (self.values.items) |value| {
            if (value.aggregate) |aggregate| {
                switch (aggregate) {
                    .record => |record| if (record.fields.len > 0) self.allocator.free(record.fields),
                    .tuple => |elems| if (elems.len > 0) self.allocator.free(elems),
                    .tag => |tag| {
                        if (tag.payloads.len > 0) self.allocator.free(tag.payloads);
                        if (tag.payload_roots.len > 0) self.allocator.free(tag.payload_roots);
                    },
                    .list => |list| if (list.elems.len > 0) self.allocator.free(list.elems),
                }
            }
        }
        self.value_ids.deinit(self.allocator);
        self.value_transform_boundary_ids.deinit(self.allocator);
        self.consumer_use_plan_ids.deinit(self.allocator);
        self.source_match_branch_index.deinit();
        self.source_match_branch_reachabilities.deinit(self.allocator);
        self.join_inputs.deinit(self.allocator);
        self.returns.deinit(self.allocator);
        self.joins.deinit(self.allocator);
        self.low_level_value_flow_arg_indices.deinit(self.allocator);
        self.low_level_value_flow_edges.deinit(self.allocator);
        self.low_level_value_flows.deinit(self.allocator);
        self.call_value_finite_dispatch_branches.deinit(self.allocator);
        self.call_value_finite_dispatches.deinit(self.allocator);
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

    pub fn addCallValueFiniteDispatchPlan(
        self: *ValueInfoStore,
        plan: CallValueFiniteDispatchPlan,
    ) std.mem.Allocator.Error!CallValueFiniteDispatchPlanId {
        const id: CallValueFiniteDispatchPlanId = @enumFromInt(@as(u32, @intCast(self.call_value_finite_dispatches.items.len)));
        try self.call_value_finite_dispatches.append(self.allocator, plan);
        return id;
    }

    pub fn addCallValueFiniteDispatchBranchSpan(
        self: *ValueInfoStore,
        branches: []const CallValueFiniteDispatchBranch,
    ) std.mem.Allocator.Error!Span(CallValueFiniteDispatchBranch) {
        if (branches.len == 0) return Span(CallValueFiniteDispatchBranch).empty();
        const start: u32 = @intCast(self.call_value_finite_dispatch_branches.items.len);
        try self.call_value_finite_dispatch_branches.appendSlice(self.allocator, branches);
        return .{ .start = start, .len = @intCast(branches.len) };
    }

    pub fn callValueFiniteDispatchPlan(
        self: *const ValueInfoStore,
        id: CallValueFiniteDispatchPlanId,
    ) CallValueFiniteDispatchPlan {
        return self.call_value_finite_dispatches.items[@intFromEnum(id)];
    }

    pub fn sliceCallValueFiniteDispatchBranches(
        self: *const ValueInfoStore,
        span: Span(CallValueFiniteDispatchBranch),
    ) []const CallValueFiniteDispatchBranch {
        if (span.len == 0) return &.{};
        return self.call_value_finite_dispatch_branches.items[span.start..][0..span.len];
    }

    pub fn addLowLevelValueFlowSignature(
        self: *ValueInfoStore,
        signature: LowLevelValueFlowSignature,
    ) std.mem.Allocator.Error!LowLevelValueFlowSignatureId {
        const id: LowLevelValueFlowSignatureId = @enumFromInt(@as(u32, @intCast(self.low_level_value_flows.items.len)));
        try self.low_level_value_flows.append(self.allocator, signature);
        return id;
    }

    pub fn addLowLevelValueFlowEdgeSpan(
        self: *ValueInfoStore,
        edges: []const LowLevelValueFlowEdge,
    ) std.mem.Allocator.Error!Span(LowLevelValueFlowEdge) {
        if (edges.len == 0) return Span(LowLevelValueFlowEdge).empty();
        const start: u32 = @intCast(self.low_level_value_flow_edges.items.len);
        try self.low_level_value_flow_edges.appendSlice(self.allocator, edges);
        return .{ .start = start, .len = @intCast(edges.len) };
    }

    pub fn sliceLowLevelValueFlowEdgeSpan(
        self: *const ValueInfoStore,
        span: Span(LowLevelValueFlowEdge),
    ) []const LowLevelValueFlowEdge {
        if (span.len == 0) return &.{};
        return self.low_level_value_flow_edges.items[span.start..][0..span.len];
    }

    pub fn addLowLevelValueFlowArgIndexSpan(
        self: *ValueInfoStore,
        args: []const u32,
    ) std.mem.Allocator.Error!Span(u32) {
        if (args.len == 0) return Span(u32).empty();
        const start: u32 = @intCast(self.low_level_value_flow_arg_indices.items.len);
        try self.low_level_value_flow_arg_indices.appendSlice(self.allocator, args);
        return .{ .start = start, .len = @intCast(args.len) };
    }

    pub fn sliceLowLevelValueFlowArgIndexSpan(
        self: *const ValueInfoStore,
        span: Span(u32),
    ) []const u32 {
        if (span.len == 0) return &.{};
        return self.low_level_value_flow_arg_indices.items[span.start..][0..span.len];
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

    pub fn addSourceMatchBranchReachability(
        self: *ValueInfoStore,
        ref: SourceMatchBranchRef,
    ) std.mem.Allocator.Error!SourceMatchBranchReachabilityId {
        if (self.source_match_branch_index.contains(ref)) {
            debug.invariant(false, "lambda-solved invariant violated: source-match branch reachability was published twice");
            unreachable;
        }
        const id: SourceMatchBranchReachabilityId = @enumFromInt(@as(u32, @intCast(self.source_match_branch_reachabilities.items.len)));
        try self.source_match_branch_reachabilities.append(self.allocator, .{ .ref = ref });
        try self.source_match_branch_index.put(ref, id);
        return id;
    }

    pub fn sourceMatchBranchReachabilityId(
        self: *const ValueInfoStore,
        ref: SourceMatchBranchRef,
    ) ?SourceMatchBranchReachabilityId {
        return self.source_match_branch_index.get(ref);
    }

    pub fn sourceMatchBranchReachable(
        self: *const ValueInfoStore,
        ref: SourceMatchBranchRef,
    ) bool {
        const id = self.sourceMatchBranchReachabilityId(ref) orelse {
            debug.invariant(false, "lambda-solved invariant violated: source-match branch reachability was not published");
            unreachable;
        };
        return self.source_match_branch_reachabilities.items[@intFromEnum(id)].reachable;
    }

    pub fn setSourceMatchBranchReachable(
        self: *ValueInfoStore,
        ref: SourceMatchBranchRef,
        reachable: bool,
    ) void {
        const id = self.sourceMatchBranchReachabilityId(ref) orelse {
            debug.invariant(false, "lambda-solved invariant violated: source-match branch reachability was not published before finalization");
            unreachable;
        };
        self.source_match_branch_reachabilities.items[@intFromEnum(id)].reachable = reachable;
    }

    pub fn valueSourceMatchBranchReachable(
        self: *const ValueInfoStore,
        value: ValueInfo,
    ) bool {
        const ref = value.source_match_branch orelse return true;
        return self.sourceMatchBranchReachable(ref);
    }

    pub fn callSiteSourceMatchBranchReachable(
        self: *const ValueInfoStore,
        call_site: CallSiteInfo,
    ) bool {
        const ref = call_site.source_match_branch orelse return true;
        return self.sourceMatchBranchReachable(ref);
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

    pub fn addConsumerUsePlanSpan(
        self: *ValueInfoStore,
        ids: []const ConsumerUsePlanId,
    ) std.mem.Allocator.Error!Span(ConsumerUsePlanId) {
        if (ids.len == 0) return Span(ConsumerUsePlanId).empty();
        const start: u32 = @intCast(self.consumer_use_plan_ids.items.len);
        try self.consumer_use_plan_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceConsumerUsePlanSpan(
        self: *const ValueInfoStore,
        span: Span(ConsumerUsePlanId),
    ) []const ConsumerUsePlanId {
        if (span.len == 0) return &.{};
        return self.consumer_use_plan_ids.items[span.start..][0..span.len];
    }
};

/// Public `canonicalTypeKeyEql` function.
pub fn canonicalTypeKeyEql(a: canonical.CanonicalTypeKey, b: canonical.CanonicalTypeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

fn isEmptyCanonicalTypeKey(key: canonical.CanonicalTypeKey) bool {
    for (key.bytes) |byte| {
        if (byte != 0) return false;
    }
    return true;
}

/// Public `callableSetKeyEql` function.
pub fn callableSetKeyEql(a: CanonicalCallableSetKey, b: CanonicalCallableSetKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

/// Public `canonicalExecValueTypeKeyEql` function.
pub fn canonicalExecValueTypeKeyEql(a: CanonicalExecValueTypeKey, b: CanonicalExecValueTypeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

/// Public `captureShapeKeyEql` function.
pub fn captureShapeKeyEql(a: CaptureShapeKey, b: CaptureShapeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
}

/// Public `erasedFnSigKeyEql` function.
pub fn erasedFnSigKeyEql(a: ErasedFnSigKey, b: ErasedFnSigKey) bool {
    if (!canonicalTypeKeyEql(a.source_fn_ty, b.source_fn_ty)) return false;
    if (!std.mem.eql(u8, a.abi.bytes[0..], b.abi.bytes[0..])) return false;
    return true;
}

/// Public `erasedAdapterKeyEql` function.
pub fn erasedAdapterKeyEql(a: ErasedAdapterKey, b: ErasedAdapterKey) bool {
    return canonicalTypeKeyEql(a.source_fn_ty, b.source_fn_ty) and
        callableSetKeyEql(a.callable_set_key, b.callable_set_key) and
        erasedFnSigKeyEql(a.erased_fn_sig_key, b.erased_fn_sig_key) and
        captureShapeKeyEql(a.capture_shape_key, b.capture_shape_key);
}

/// Public `RepresentationSolveSession` declaration.
pub const RepresentationSolveSession = struct {
    members: []const ProcRepresentationInstanceId,
    representation_store: RepresentationStore,
    state: RepresentationSolveState,

    pub fn deinit(self: *RepresentationSolveSession, allocator: std.mem.Allocator) void {
        if (self.members.len > 0) allocator.free(self.members);
        self.representation_store.deinit();
    }
};

/// Published artifact payload store used for adapter-owned public procedure boundaries.
pub const ProcBoundaryExecutablePayloads = struct {
    artifact: checked_artifact.CheckedModuleArtifactKey,
    payloads: *const checked_artifact.ExecutableTypePayloadStore,
    promoted_wrapper: ?canonical.MirProcedureRef = null,
};

/// Public `ProcRepresentationInstance` declaration.
pub const ProcRepresentationInstance = struct {
    proc: canonical.MirProcedureRef,
    executable_specialization_key: ExecutableSpecializationKey,
    solve_session: RepresentationSolveSessionId,
    value_store: ValueInfoStoreId,
    public_roots: ProcPublicValueRoots,
    boundary_payloads: ?ProcBoundaryExecutablePayloads = null,
    boundary_provenance: []const BoxErasureProvenance = &.{},
    materialized: bool,
};

/// Public `executableSpecializationKeyForProc` function.
pub fn executableSpecializationKeyForProc(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    proc: canonical.MirProcedureRef,
    roots: ProcPublicValueRoots,
) std.mem.Allocator.Error!ExecutableSpecializationKey {
    const params = value_store.sliceValueSpan(roots.params);
    const arg_keys: []CanonicalExecValueTypeKey = if (params.len == 0)
        &.{}
    else
        try allocator.alloc(CanonicalExecValueTypeKey, params.len);
    errdefer if (arg_keys.len > 0) allocator.free(arg_keys);
    for (params, 0..) |param, i| {
        arg_keys[i] = try execValueTypeKeyForValue(allocator, names, row_shapes, types, representation_store, value_store, param);
    }

    return .{
        .base = proc.proc.proc_base,
        .requested_fn_ty = proc.callable.source_fn_ty,
        .exec_arg_tys = arg_keys,
        .exec_ret_ty = try execValueTypeKeyForValue(allocator, names, row_shapes, types, representation_store, value_store, roots.ret),
        .callable_repr_mode = .direct,
        .capture_shape_key = try captureShapeKeyForValues(allocator, names, row_shapes, types, representation_store, value_store, roots.captures),
    };
}

pub fn deinitExecutableSpecializationKey(
    allocator: std.mem.Allocator,
    key: *ExecutableSpecializationKey,
) void {
    if (key.exec_arg_tys.len > 0) allocator.free(key.exec_arg_tys);
    key.exec_arg_tys = &.{};
}

/// Public `cloneExecutableSpecializationKey` function.
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

fn cloneExecutableSpecializationKeySlice(
    allocator: std.mem.Allocator,
    keys: []const ExecutableSpecializationKey,
) std.mem.Allocator.Error![]const ExecutableSpecializationKey {
    if (keys.len == 0) return &.{};
    const out = try allocator.alloc(ExecutableSpecializationKey, keys.len);
    var initialized: usize = 0;
    errdefer {
        for (out[0..initialized]) |*key| deinitExecutableSpecializationKey(allocator, key);
        allocator.free(out);
    }
    for (keys, 0..) |key, i| {
        out[i] = try cloneExecutableSpecializationKey(allocator, key);
        initialized += 1;
    }
    return out;
}

/// Public `executableSpecializationKeyEql` function.
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

fn executableSpecializationKeySliceEql(
    a: []const ExecutableSpecializationKey,
    b: []const ExecutableSpecializationKey,
) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (!executableSpecializationKeyEql(left, right)) return false;
    }
    return true;
}

fn boxErasureProvenanceEql(a: BoxErasureProvenance, b: BoxErasureProvenance) bool {
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
    a: []const BoxErasureProvenance,
    b: []const BoxErasureProvenance,
) bool {
    if (a.len != b.len) return false;
    for (a, b) |left, right| {
        if (!boxErasureProvenanceEql(left, right)) return false;
    }
    return true;
}

fn boxErasureProvenanceSliceContains(
    items: []const BoxErasureProvenance,
    needle: BoxErasureProvenance,
) bool {
    for (items) |item| {
        if (boxErasureProvenanceEql(item, needle)) return true;
    }
    return false;
}

/// Public `mergeBoxErasureProvenanceSets` function.
pub fn mergeBoxErasureProvenanceSets(
    allocator: std.mem.Allocator,
    existing: []const BoxErasureProvenance,
    added: []const BoxErasureProvenance,
) std.mem.Allocator.Error![]const BoxErasureProvenance {
    if (existing.len == 0) {
        if (added.len == 0) return &.{};
        return try allocator.dupe(BoxErasureProvenance, added);
    }
    var unique_added = std.ArrayList(BoxErasureProvenance).empty;
    defer unique_added.deinit(allocator);
    for (added) |item| {
        if (boxErasureProvenanceSliceContains(existing, item)) continue;
        if (boxErasureProvenanceSliceContains(unique_added.items, item)) continue;
        try unique_added.append(allocator, item);
    }
    if (unique_added.items.len == 0) return try allocator.dupe(BoxErasureProvenance, existing);

    const merged = try allocator.alloc(BoxErasureProvenance, existing.len + unique_added.items.len);
    @memcpy(merged[0..existing.len], existing);
    @memcpy(merged[existing.len..], unique_added.items);
    return merged;
}

fn mergeOwnedBoxErasureProvenanceSet(
    allocator: std.mem.Allocator,
    existing: []const BoxErasureProvenance,
    added: []const BoxErasureProvenance,
) std.mem.Allocator.Error![]const BoxErasureProvenance {
    if (added.len == 0) return existing;
    var unique_added = std.ArrayList(BoxErasureProvenance).empty;
    defer unique_added.deinit(allocator);
    for (added) |item| {
        if (boxErasureProvenanceSliceContains(existing, item)) continue;
        if (boxErasureProvenanceSliceContains(unique_added.items, item)) continue;
        try unique_added.append(allocator, item);
    }
    if (unique_added.items.len == 0) return existing;

    const merged = try allocator.alloc(BoxErasureProvenance, existing.len + unique_added.items.len);
    @memcpy(merged[0..existing.len], existing);
    @memcpy(merged[existing.len..], unique_added.items);
    if (existing.len > 0) allocator.free(existing);
    return merged;
}

pub fn deinitProcRepresentationInstance(
    allocator: std.mem.Allocator,
    instance: *ProcRepresentationInstance,
) void {
    if (instance.boundary_provenance.len > 0) allocator.free(instance.boundary_provenance);
    deinitExecutableSpecializationKey(allocator, &instance.executable_specialization_key);
}

/// Public `execValueTypeKeyForValue` function.
pub fn execValueTypeKeyForValue(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    value: ValueInfoId,
) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
    var builder = ExecValueTypeKeyBuilder.initForValues(allocator, names, row_shapes, types, representation_store, value_store);
    defer builder.deinit();
    return try builder.keyForValue(value);
}

fn execValueTypeKeyForAggregateRootValue(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    rep_root: RepRootId,
    logical_ty: type_mod.TypeVarId,
    aggregate: AggregateValueInfo,
) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
    var builder = ExecValueTypeKeyBuilder.initForValues(allocator, names, row_shapes, types, representation_store, value_store);
    defer builder.deinit();
    return try builder.keyForAggregateRootValue(rep_root, logical_ty, aggregate);
}

/// Public `execValueTypeKey` function.
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

/// Public `execValueTypeKeyForRootType` function.
pub fn execValueTypeKeyForRootType(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    rep_root: RepRootId,
    ty: type_mod.TypeVarId,
) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
    var builder = ExecValueTypeKeyBuilder.initForRootTypes(allocator, names, row_shapes, types, representation_store);
    defer builder.deinit();
    return try builder.keyForRootType(rep_root, ty);
}

/// Public `captureShapeKeyForValues` function.
pub fn captureShapeKeyForValues(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    value_store: *const ValueInfoStore,
    values: Span(ValueInfoId),
) std.mem.Allocator.Error!CaptureShapeKey {
    return try captureShapeKeyForValueSlice(
        allocator,
        names,
        row_shapes,
        types,
        representation_store,
        value_store,
        value_store.sliceValueSpan(values),
    );
}

/// Public `captureShapeKeyForValueSlice` function.
pub fn captureShapeKeyForValueSlice(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
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
        const key = try execValueTypeKeyForValue(allocator, names, row_shapes, types, representation_store, value_store, capture);
        hasher.update(&key.bytes);
    }
    return .{ .bytes = hasher.finalResult() };
}

/// Public `captureShapeKeyForExecKeys` function.
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

/// Public `captureTupleExecKeyForSlots` function.
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

/// Public `finiteCallableSetExecValueTypeKey` function.
pub fn finiteCallableSetExecValueTypeKey(key: CanonicalCallableSetKey) CanonicalExecValueTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "callable_set");
    hasher.update(&key.bytes);
    return .{ .bytes = hasher.finalResult() };
}

/// Public `erasedCallableExecValueTypeKey` function.
pub fn erasedCallableExecValueTypeKey(sig_key: ErasedFnSigKey) CanonicalExecValueTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "erased_fn");
    writeErasedFnSigKey(&hasher, sig_key);
    return .{ .bytes = hasher.finalResult() };
}

/// Public `sessionExecutableTypeEndpointForValue` function.
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

/// Public `sessionExecutableTypeEndpointForValueIntoStore` function.
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

/// Public `sessionExecutableTypeEndpointForErasedBoundaryTypeIntoStore` function.
pub fn sessionExecutableTypeEndpointForErasedBoundaryTypeIntoStore(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *RepresentationStore,
    owner_payload_store: *SessionExecutableTypePayloadStore,
    logical_ty: type_mod.TypeVarId,
    source_ty_hint: canonical.CanonicalTypeKey,
    source_ty_names: ?*const canonical.CanonicalNameStore,
    source_ty_view: ?checked_artifact.CheckedTypeStoreView,
    source_ty_root: ?checked_artifact.CheckedTypeId,
) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
    var builder = SessionExecutableTypePayloadBuilder.initWithPayloadStore(
        allocator,
        names,
        row_shapes,
        types,
        representation_store,
        owner_payload_store,
        null,
    );
    builder.erased_abi_sink = &representation_store.erased_fn_abis;
    builder.erased_source_types = source_ty_view;
    builder.erased_source_names = source_ty_names;
    defer builder.deinit();
    return try builder.endpointForErasedBoundaryType(logical_ty, .{
        .root = source_ty_root,
        .key_hint = source_ty_hint,
    });
}

const ErasedBoundarySourceCursor = struct {
    root: ?checked_artifact.CheckedTypeId = null,
    key_hint: canonical.CanonicalTypeKey = .{},
};

const ResolvedSourcePayload = struct {
    cursor: ErasedBoundarySourceCursor,
    payload: checked_artifact.CheckedTypePayload,
};

/// Public `sessionExecutableTypeEndpointForCallableSetIntoStore` function.
pub fn sessionExecutableTypeEndpointForCallableSetIntoStore(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    owner_payload_store: *SessionExecutableTypePayloadStore,
    callable_set_key: CanonicalCallableSetKey,
    expected_key: CanonicalExecValueTypeKey,
) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
    var builder = SessionExecutableTypePayloadBuilder.initWithPayloadStore(
        allocator,
        names,
        row_shapes,
        types,
        representation_store,
        owner_payload_store,
        null,
    );
    defer builder.deinit();
    return try builder.payloadForCallableSetType(callable_set_key, expected_key);
}

const SessionExecutableTypePayloadBuilder = struct {
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    row_shapes: *row.Store,
    types: *const type_mod.Store,
    representation_store: *const RepresentationStore,
    erased_abi_sink: ?*canonical.ErasedFnAbiStore = null,
    erased_source_types: ?checked_artifact.CheckedTypeStoreView = null,
    erased_source_names: ?*const canonical.CanonicalNameStore = null,
    payload_store: *SessionExecutableTypePayloadStore,
    value_store: ?*const ValueInfoStore,
    active_types: std.AutoHashMap(type_mod.TypeVarId, SessionExecutableTypePayloadId),
    active_root_types: std.AutoHashMap(CanonicalExecValueTypeKey, SessionExecutableTypePayloadId),
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
            .erased_abi_sink = null,
            .erased_source_names = null,
            .payload_store = payload_store,
            .value_store = value_store,
            .active_types = std.AutoHashMap(type_mod.TypeVarId, SessionExecutableTypePayloadId).init(allocator),
            .active_root_types = std.AutoHashMap(CanonicalExecValueTypeKey, SessionExecutableTypePayloadId).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, SessionExecutableTypePayloadId).init(allocator),
        };
    }

    fn deinit(self: *SessionExecutableTypePayloadBuilder) void {
        self.active_values.deinit();
        self.active_root_types.deinit();
        self.active_types.deinit();
    }

    fn refFor(id: SessionExecutableTypePayloadId) SessionExecutableTypePayloadRef {
        return .{ .payload = id };
    }

    fn endpointForErasedBoundaryType(
        self: *SessionExecutableTypePayloadBuilder,
        ty: type_mod.TypeVarId,
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const root = self.types.unlinkConst(ty);
        const payload = try self.erasedBoundaryTypePayload(root, source);
        const key = try self.keyForErasedBoundaryPayload(payload);
        if (self.payload_store.refForKey(key)) |existing| {
            self.deinitTransientPayload(payload);
            return .{ .ty = existing, .key = key };
        }
        const id = try self.payload_store.append(self.allocator, key, payload);
        return .{ .ty = refFor(id), .key = key };
    }

    fn erasedBoundaryTypePayload(
        self: *SessionExecutableTypePayloadBuilder,
        root: type_mod.TypeVarId,
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("session erased-boundary payload reached unresolved lambda-solved type"),
            .nominal => |nominal| blk: {
                const backing_source = self.sourceNominalBacking(source) orelse ErasedBoundarySourceCursor{ .key_hint = nominal.source_ty };
                const backing = try self.endpointForErasedBoundaryType(nominal.backing, backing_source);
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .source_ty = nominal.source_ty,
                    .is_opaque = nominal.is_opaque,
                    .backing = backing.ty,
                    .backing_key = backing.key,
                } };
            },
            .content => |content| try self.erasedBoundaryContentPayload(root, content, source),
        };
    }

    fn erasedBoundaryContentPayload(
        self: *SessionExecutableTypePayloadBuilder,
        root: type_mod.TypeVarId,
        content: type_mod.Content,
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (content) {
            .primitive => |prim| .{ .primitive = executablePrimitive(prim) },
            .list => |elem| .{ .list = try self.childForErasedBoundaryType(elem, self.sourceListElem(source)) },
            .box => |elem| .{ .box = try self.childForErasedBoundaryType(elem, self.sourceBoxPayload(source)) },
            .tuple => |span| .{ .tuple = try self.tuplePayloadForErasedBoundaryTypeSpan(span, source) },
            .record => |record| .{ .record = try self.recordPayloadForErasedBoundaryTypeSpan(record.fields, source) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagUnionPayloadForErasedBoundaryTypeSpan(tag_union.tags, source) },
            .func => |func| .{ .erased_fn = try self.erasedFunctionSlotPayload(root, func, source) },
        };
    }

    fn childForErasedBoundaryType(
        self: *SessionExecutableTypePayloadBuilder,
        ty: type_mod.TypeVarId,
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const child = try self.endpointForErasedBoundaryType(ty, source);
        return .{ .ty = child.ty, .key = child.key };
    }

    fn sourceCursorForCheckedRoot(
        self: *const SessionExecutableTypePayloadBuilder,
        root: checked_artifact.CheckedTypeId,
    ) ErasedBoundarySourceCursor {
        const view = self.erased_source_types orelse representationInvariant("erased boundary source child requested without a checked source type view");
        const index = @intFromEnum(root);
        if (index >= view.roots.len or index >= view.payloads.len) {
            representationInvariant("erased boundary checked source child is outside the checked type store");
        }
        return .{
            .root = root,
            .key_hint = view.roots[index].key,
        };
    }

    fn sourceCursorKey(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) canonical.CanonicalTypeKey {
        if (source.root) |root| {
            const view = self.erased_source_types orelse representationInvariant("erased boundary source root has no checked source type view");
            const index = @intFromEnum(root);
            if (index >= view.roots.len) {
                representationInvariant("erased boundary checked source root is outside the checked type root store");
            }
            return view.roots[index].key;
        }
        return source.key_hint;
    }

    fn sourcePayload(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ?checked_artifact.CheckedTypePayload {
        const root = source.root orelse return null;
        const view = self.erased_source_types orelse representationInvariant("erased boundary source root has no checked source type view");
        const index = @intFromEnum(root);
        if (index >= view.payloads.len) {
            representationInvariant("erased boundary checked source root is outside the checked type payload store");
        }
        return view.payloads[index];
    }

    fn sourceRecordFieldMatches(
        self: *const SessionExecutableTypePayloadBuilder,
        source_field: canonical.RecordFieldLabelId,
        logical_field: canonical.RecordFieldLabelId,
    ) bool {
        const source_names = self.erased_source_names orelse self.names;
        return std.mem.eql(
            u8,
            source_names.recordFieldLabelText(source_field),
            self.names.recordFieldLabelText(logical_field),
        );
    }

    fn sourceTagMatches(
        self: *const SessionExecutableTypePayloadBuilder,
        source_tag: canonical.TagLabelId,
        logical_tag: canonical.TagLabelId,
    ) bool {
        const source_names = self.erased_source_names orelse self.names;
        return std.mem.eql(
            u8,
            source_names.tagLabelText(source_tag),
            self.names.tagLabelText(logical_tag),
        );
    }

    fn resolvedSourcePayload(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ?ResolvedSourcePayload {
        var current = source;
        while (true) {
            const payload = self.sourcePayload(current) orelse return null;
            switch (payload) {
                .alias => |alias| current = self.sourceCursorForCheckedRoot(alias.backing),
                else => return .{ .cursor = current, .payload = payload },
            }
        }
    }

    fn sourceNominalBacking(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ?ErasedBoundarySourceCursor {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        return switch (resolved.payload) {
            .nominal => |nominal| self.sourceCursorForCheckedRoot(nominal.backing),
            else => null,
        };
    }

    fn sourceListElem(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ErasedBoundarySourceCursor {
        const resolved = self.resolvedSourcePayload(source) orelse return .{};
        return switch (resolved.payload) {
            .nominal => |nominal| blk: {
                if (nominal.builtin != .list or nominal.args.len != 1) {
                    representationInvariant("erased boundary list source payload is not builtin List(T)");
                }
                break :blk self.sourceCursorForCheckedRoot(nominal.args[0]);
            },
            else => representationInvariant("erased boundary list source payload is not builtin List(T)"),
        };
    }

    fn sourceBoxPayload(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ErasedBoundarySourceCursor {
        const resolved = self.resolvedSourcePayload(source) orelse return .{};
        return switch (resolved.payload) {
            .nominal => |nominal| blk: {
                if (nominal.builtin != .box or nominal.args.len != 1) {
                    representationInvariant("erased boundary Box source payload is not builtin Box(T)");
                }
                break :blk self.sourceCursorForCheckedRoot(nominal.args[0]);
            },
            else => representationInvariant("erased boundary Box source payload is not builtin Box(T)"),
        };
    }

    fn sourceTupleElem(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
        index: usize,
    ) ErasedBoundarySourceCursor {
        const resolved = self.resolvedSourcePayload(source) orelse return .{};
        return switch (resolved.payload) {
            .tuple => |items| blk: {
                if (index >= items.len) representationInvariant("erased boundary tuple source payload arity mismatch");
                break :blk self.sourceCursorForCheckedRoot(items[index]);
            },
            else => representationInvariant("erased boundary tuple source payload is not a tuple"),
        };
    }

    fn sourceRecordField(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
        field: canonical.RecordFieldLabelId,
    ) ErasedBoundarySourceCursor {
        var current = source;
        var resolved = self.resolvedSourcePayload(current) orelse return .{};
        while (true) {
            switch (resolved.payload) {
                .record => |record| {
                    for (record.fields) |candidate| {
                        if (self.sourceRecordFieldMatches(candidate.name, field)) return self.sourceCursorForCheckedRoot(candidate.ty);
                    }
                    current = self.sourceCursorForCheckedRoot(record.ext);
                    resolved = self.resolvedSourcePayload(current) orelse return .{};
                },
                .record_unbound => |fields| {
                    for (fields) |candidate| {
                        if (self.sourceRecordFieldMatches(candidate.name, field)) return self.sourceCursorForCheckedRoot(candidate.ty);
                    }
                    representationInvariant("erased boundary record source payload is missing a field");
                },
                .empty_record => representationInvariant("erased boundary record source payload is missing a field"),
                else => representationInvariant("erased boundary record source payload is not a record"),
            }
        }
    }

    fn sourceTagPayload(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
        tag: canonical.TagLabelId,
        index: usize,
    ) ErasedBoundarySourceCursor {
        var current = source;
        var resolved = self.resolvedSourcePayload(current) orelse return .{};
        while (true) {
            switch (resolved.payload) {
                .tag_union => |tag_union| {
                    for (tag_union.tags) |candidate| {
                        if (!self.sourceTagMatches(candidate.name, tag)) continue;
                        if (index >= candidate.args.len) representationInvariant("erased boundary tag source payload arity mismatch");
                        return self.sourceCursorForCheckedRoot(candidate.args[index]);
                    }
                    current = self.sourceCursorForCheckedRoot(tag_union.ext);
                    resolved = self.resolvedSourcePayload(current) orelse return .{};
                },
                .empty_tag_union => representationInvariant("erased boundary tag-union source payload is missing a tag"),
                else => representationInvariant("erased boundary tag-union source payload is not a tag union"),
            }
        }
    }

    fn resolvedFunctionSourcePayload(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ?checked_artifact.CheckedFunctionType {
        const resolved = self.resolvedSourcePayload(source) orelse return null;
        return switch (resolved.payload) {
            .function => |function| function,
            else => representationInvariant("erased boundary function source payload is not a function"),
        };
    }

    fn sourceFunctionArg(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
        index: usize,
    ) ErasedBoundarySourceCursor {
        const function = self.resolvedFunctionSourcePayload(source) orelse return .{};
        if (index >= function.args.len) representationInvariant("erased boundary function source payload arity mismatch");
        return self.sourceCursorForCheckedRoot(function.args[index]);
    }

    fn sourceFunctionReturn(
        self: *const SessionExecutableTypePayloadBuilder,
        source: ErasedBoundarySourceCursor,
    ) ErasedBoundarySourceCursor {
        const function = self.resolvedFunctionSourcePayload(source) orelse return .{};
        return self.sourceCursorForCheckedRoot(function.ret);
    }

    fn erasedFunctionSlotPayload(
        self: *SessionExecutableTypePayloadBuilder,
        root: type_mod.TypeVarId,
        func: type_mod.LambdaSolvedFnType,
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error!SessionExecutableErasedFnPayload {
        const args = self.types.sliceTypeVarSpan(func.args);
        var arg_keys: []CanonicalExecValueTypeKey = if (args.len == 0)
            &.{}
        else
            try self.allocator.alloc(CanonicalExecValueTypeKey, args.len);
        defer if (arg_keys.len > 0) self.allocator.free(arg_keys);
        var arg_abis: []canonical.ErasedValueAbi = if (args.len == 0)
            &.{}
        else
            try self.allocator.alloc(canonical.ErasedValueAbi, args.len);
        defer if (arg_abis.len > 0) self.allocator.free(arg_abis);

        for (args, 0..) |arg, i| {
            const endpoint = try self.endpointForErasedBoundaryType(arg, self.sourceFunctionArg(source, i));
            arg_keys[i] = endpoint.key;
            arg_abis[i] = .ordinary_roc_value;
        }
        const ret_endpoint = try self.endpointForErasedBoundaryType(func.ret, self.sourceFunctionReturn(source));
        const abi_sink = self.erased_abi_sink orelse representationInvariant("erased boundary function slot has no erased ABI sink");
        const abi_key = try abi_sink.append(self.allocator, .{
            .fixed_arity = func.fixed_arity,
            .arg_exec_keys = arg_keys,
            .ret_exec_key = ret_endpoint.key,
            .arg_abis = arg_abis,
            .capture_arg = .ordinary_roc_value,
        });
        const source_fn_ty = if (isEmptyCanonicalTypeKey(self.sourceCursorKey(source)))
            try self.sourceTypeKeyForType(root)
        else
            self.sourceCursorKey(source);
        return .{
            .sig_key = .{
                .source_fn_ty = source_fn_ty,
                .abi = abi_key,
                .capture_ty = null,
            },
            .capture_shape_key = captureShapeKeyForExecKeys(&.{}),
        };
    }

    fn tuplePayloadForErasedBoundaryTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        span: type_mod.Span(type_mod.TypeVarId),
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error![]const SessionExecutableTupleElemPayload {
        const items = self.types.sliceTypeVarSpan(span);
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(SessionExecutableTupleElemPayload, items.len);
        errdefer self.allocator.free(out);
        for (items, 0..) |item, i| {
            const child = try self.endpointForErasedBoundaryType(item, self.sourceTupleElem(source, i));
            out[i] = .{
                .index = @intCast(i),
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn recordPayloadForErasedBoundaryTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        span: type_mod.Span(type_mod.Field),
        source: ErasedBoundarySourceCursor,
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
        if (self.row_shapes.recordShapeFields(shape).len != fields.len) representationInvariant("erased boundary record payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableRecordFieldPayload, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            const child = try self.endpointForErasedBoundaryType(field.ty, self.sourceRecordField(source, field.name));
            out[i] = .{
                .field = self.recordFieldInShape(shape, field.name),
                .ty = child.ty,
                .key = child.key,
            };
        }
        return .{ .shape = shape, .fields = out };
    }

    fn tagUnionPayloadForErasedBoundaryTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        span: type_mod.Span(type_mod.Tag),
        source: ErasedBoundarySourceCursor,
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
        if (self.row_shapes.tagUnionTags(shape).len != tags.len) representationInvariant("erased boundary tag payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagVariantPayload, tags.len);
        for (out) |*variant| variant.* = .{ .tag = undefined, .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }

        for (tags, 0..) |tag, i| {
            const shape_tag = self.tagInShape(shape, tag.name);
            out[i] = .{
                .tag = shape_tag,
                .payloads = try self.tagPayloadsForErasedBoundaryTypeSpan(shape_tag, tag.name, tag.args, source),
            };
        }
        return .{ .shape = shape, .variants = out };
    }

    fn recordFieldInShape(
        self: *SessionExecutableTypePayloadBuilder,
        shape: row.RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) row.RecordFieldId {
        for (self.row_shapes.recordShapeFields(shape)) |field_id| {
            if (self.row_shapes.recordField(field_id).label == label) return field_id;
        }
        representationInvariant("erased boundary record payload label missing from interned shape");
    }

    fn tagInShape(
        self: *SessionExecutableTypePayloadBuilder,
        shape: row.TagUnionShapeId,
        label: canonical.TagLabelId,
    ) row.TagId {
        for (self.row_shapes.tagUnionTags(shape)) |tag_id| {
            if (self.row_shapes.tag(tag_id).label == label) return tag_id;
        }
        representationInvariant("erased boundary tag payload label missing from interned shape");
    }

    fn tagPayloadsForErasedBoundaryTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        tag: row.TagId,
        source_tag: canonical.TagLabelId,
        span: type_mod.Span(type_mod.TypeVarId),
        source: ErasedBoundarySourceCursor,
    ) std.mem.Allocator.Error![]const SessionExecutableTagPayload {
        const args = self.types.sliceTypeVarSpan(span);
        if (args.len == 0) return &.{};
        const shape_payloads = self.row_shapes.tagPayloads(tag);
        if (shape_payloads.len != args.len) representationInvariant("erased boundary tag payload arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagPayload, args.len);
        errdefer self.allocator.free(out);
        for (args, shape_payloads, 0..) |arg, payload, i| {
            const source_child = self.sourceTagPayload(source, source_tag, i);
            const child = try self.endpointForErasedBoundaryType(arg, source_child);
            out[i] = .{
                .payload = payload,
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn keyForErasedBoundaryPayload(
        self: *SessionExecutableTypePayloadBuilder,
        payload: SessionExecutableTypePayload,
    ) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        var hasher = std.crypto.hash.sha2.Sha256.init(.{});
        try self.hashErasedBoundaryPayload(&hasher, payload);
        return .{ .bytes = hasher.finalResult() };
    }

    fn hashErasedBoundaryPayload(
        self: *SessionExecutableTypePayloadBuilder,
        hasher: *std.crypto.hash.sha2.Sha256,
        payload: SessionExecutableTypePayload,
    ) std.mem.Allocator.Error!void {
        switch (payload) {
            .pending,
            .recursive_ref,
            => representationInvariant("erased boundary key reached incomplete payload"),
            .primitive => |prim| {
                writeHashTag(hasher, "primitive");
                writeHashU32(hasher, @intCast(@intFromEnum(prim)));
            },
            .callable_set => |set| {
                writeHashTag(hasher, "callable_set");
                hasher.update(&set.key.bytes);
            },
            .erased_fn => |erased| {
                writeHashTag(hasher, "erased_fn");
                writeErasedFnSigKey(hasher, erased.sig_key);
            },
            .vacant_callable_slot => writeHashTag(hasher, "vacant_callable_slot"),
            .list => |child| {
                writeHashTag(hasher, "list");
                hasher.update(&child.key.bytes);
            },
            .box => |child| {
                writeHashTag(hasher, "box");
                hasher.update(&child.key.bytes);
            },
            .tuple => |items| {
                writeHashTag(hasher, "tuple");
                writeHashU32(hasher, @intCast(items.len));
                for (items) |item| {
                    writeHashU32(hasher, item.index);
                    hasher.update(&item.key.bytes);
                }
            },
            .record => |record| {
                writeHashTag(hasher, "record");
                writeHashU32(hasher, @intCast(record.fields.len));
                for (record.fields) |field| {
                    const label = self.row_shapes.recordField(field.field).label;
                    writeHashBytes(hasher, self.names.recordFieldLabelText(label));
                    hasher.update(&field.key.bytes);
                }
            },
            .tag_union => |tag_union| {
                writeHashTag(hasher, "tag_union");
                writeHashU32(hasher, @intCast(tag_union.variants.len));
                for (tag_union.variants) |variant| {
                    const tag = self.row_shapes.tag(variant.tag);
                    writeHashBytes(hasher, self.names.tagLabelText(tag.label));
                    writeHashU32(hasher, @intCast(variant.payloads.len));
                    for (variant.payloads) |payload_item| {
                        hasher.update(&payload_item.key.bytes);
                    }
                }
            },
            .nominal => |nominal| {
                writeHashTag(hasher, "nominal");
                writeHashBytes(hasher, self.names.moduleNameText(nominal.nominal.module_name));
                writeHashBytes(hasher, self.names.typeNameText(nominal.nominal.type_name));
                writeHashU32(hasher, @intFromBool(nominal.is_opaque));
                hasher.update(&nominal.backing_key.bytes);
            },
        }
    }

    fn sourceTypeKeyForType(
        self: *SessionExecutableTypePayloadBuilder,
        ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!canonical.CanonicalTypeKey {
        var key_builder = ExecValueTypeKeyBuilder.init(self.allocator, self.names, self.types);
        defer key_builder.deinit();
        key_builder.writeTag("lambda_solved_source_type");
        try key_builder.writeSourceTypeAllowFunctions(ty);
        return .{ .bytes = key_builder.hasher.finalResult() };
    }

    fn deinitTransientPayload(
        self: *SessionExecutableTypePayloadBuilder,
        payload: SessionExecutableTypePayload,
    ) void {
        switch (payload) {
            .record => |record| if (record.fields.len > 0) self.allocator.free(record.fields),
            .tuple => |items| if (items.len > 0) self.allocator.free(items),
            .tag_union => |tag_union| {
                for (tag_union.variants) |variant| {
                    if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
                }
                if (tag_union.variants.len > 0) self.allocator.free(tag_union.variants);
            },
            .callable_set => |callable_set| if (callable_set.members.len > 0) self.allocator.free(callable_set.members),
            else => {},
        }
    }

    fn endpointForValue(self: *SessionExecutableTypePayloadBuilder, value: ValueInfoId) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const values = self.value_store orelse representationInvariant("session executable type endpoint for value has no value store");
        const key = try execValueTypeKeyForValue(
            self.allocator,
            self.names,
            self.row_shapes,
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
        if ((info.boxed != null or info.aggregate != null) and !(try self.valueRequiresSpecificEndpoint(value))) {
            return try self.endpointForRootType(info.root, info.logical_ty);
        }
        if (info.callable == null and info.boxed == null and info.aggregate == null) {
            if (info.value_alias_source) |source| {
                const source_endpoint = try self.publishedAliasSourceEndpoint(source) orelse try self.endpointForValue(source);
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

        if (info.callable == null and info.boxed == null and info.aggregate == null and self.valueHasFunctionType(info.logical_ty)) {
            return try self.endpointForCallableGroup(info.solved_group orelse representationInvariant("session executable function value has no solved group"));
        }

        const id = try self.payload_store.reserve(self.allocator, key);
        try self.active_values.put(value, id);
        errdefer _ = self.active_values.remove(value);

        const payload = if (info.callable) |callable|
            try self.callablePayload(callable)
        else if (info.boxed) |boxed|
            try self.boxedPayload(info.logical_ty, boxed)
        else if (info.aggregate) |aggregate|
            try self.aggregatePayload(info.root, info.logical_ty, aggregate)
        else
            try self.rootTypePayload(info.root, info.logical_ty);

        self.payload_store.fill(self.allocator, id, payload);
        _ = self.active_values.remove(value);
        return .{ .ty = refFor(id), .key = key };
    }

    fn publishedAliasSourceEndpoint(
        self: *SessionExecutableTypePayloadBuilder,
        source: ValueInfoId,
    ) std.mem.Allocator.Error!?SessionExecutableTypeEndpoint {
        const values = self.value_store orelse representationInvariant("session executable type alias source endpoint has no value store");
        const source_info = values.values.items[@intFromEnum(source)];
        const source_exec_ty = source_info.exec_ty orelse return null;
        const published = self.payload_store.refForKey(source_exec_ty.key) orelse return null;
        return .{
            .ty = published,
            .key = source_exec_ty.key,
        };
    }

    fn endpointForType(self: *SessionExecutableTypePayloadBuilder, ty: type_mod.TypeVarId) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const key = try execValueTypeKey(self.allocator, self.names, self.types, ty);
        const root = self.types.unlinkConst(ty);
        if (self.active_types.get(root)) |active| {
            return .{ .ty = refFor(active), .key = key };
        }
        if (self.payload_store.refForKey(key)) |existing| {
            if (self.payload_store.isPending(existing.payload)) {
                try self.active_types.put(root, existing.payload);
                errdefer _ = self.active_types.remove(root);
                const payload = try self.typePayload(root);
                self.payload_store.fill(self.allocator, existing.payload, payload);
                _ = self.active_types.remove(root);
            }
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
        return try self.endpointForRootType(join.root, values.values.items[@intFromEnum(value)].logical_ty);
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
        const key = try execValueTypeKeyForRootType(
            self.allocator,
            self.names,
            self.row_shapes,
            self.types,
            self.representation_store,
            root,
            ty,
        );
        if (self.valueHasFunctionType(ty)) {
            const group = self.representation_store.groupForRoot(root);
            if (self.representation_store.callableGroupEmission(group)) |emission_plan| {
                return try self.endpointForCallableEmissionPlan(emission_plan);
            }
            return try self.endpointForVacantCallableSlot(key);
        }

        const root_ty = self.types.unlinkConst(ty);
        if (self.active_root_types.get(key)) |active| {
            return .{ .ty = refFor(active), .key = key };
        }
        if (self.payload_store.refForKey(key)) |existing| {
            if (self.payload_store.isPending(existing.payload)) {
                try self.active_root_types.put(key, existing.payload);
                errdefer _ = self.active_root_types.remove(key);
                const payload = try self.rootTypePayload(root, root_ty);
                self.payload_store.fill(self.allocator, existing.payload, payload);
                _ = self.active_root_types.remove(key);
            }
            return .{ .ty = existing, .key = key };
        }

        const id = try self.payload_store.reserve(self.allocator, key);
        try self.active_root_types.put(key, id);
        errdefer _ = self.active_root_types.remove(key);

        const payload = try self.rootTypePayload(root, root_ty);
        self.payload_store.fill(self.allocator, id, payload);
        _ = self.active_root_types.remove(key);
        return .{ .ty = refFor(id), .key = key };
    }

    fn endpointForAggregateRootValue(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
        logical_ty: type_mod.TypeVarId,
        aggregate: AggregateValueInfo,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const values = self.value_store orelse representationInvariant("session aggregate executable endpoint has no value store");
        const key = try execValueTypeKeyForAggregateRootValue(
            self.allocator,
            self.names,
            self.row_shapes,
            self.types,
            self.representation_store,
            values,
            rep_root,
            logical_ty,
            aggregate,
        );

        if (self.active_root_types.get(key)) |active| {
            return .{ .ty = refFor(active), .key = key };
        }
        if (self.payload_store.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }

        const id = try self.payload_store.reserve(self.allocator, key);
        try self.active_root_types.put(key, id);
        errdefer _ = self.active_root_types.remove(key);

        const payload = try self.aggregatePayload(rep_root, logical_ty, aggregate);
        self.payload_store.fill(self.allocator, id, payload);
        _ = self.active_root_types.remove(key);
        return .{ .ty = refFor(id), .key = key };
    }

    fn rootTypePayload(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
        ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        const root = self.types.unlinkConst(ty);
        return switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("session executable root type payload reached unresolved lambda-solved type"),
            .nominal => |nominal| blk: {
                const backing_root = self.structuralChildRoot(rep_root, .{ .nominal_backing = nominal.nominal });
                const backing = try self.endpointForRootType(backing_root, nominal.backing);
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .source_ty = nominal.source_ty,
                    .is_opaque = nominal.is_opaque,
                    .backing = backing.ty,
                    .backing_key = backing.key,
                } };
            },
            .content => |content| try self.rootContentPayload(rep_root, content),
        };
    }

    fn rootContentPayload(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
        content: type_mod.Content,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (content) {
            .primitive => |prim| .{ .primitive = executablePrimitive(prim) },
            .list => |elem| .{ .list = try self.childForRootType(self.structuralChildRoot(rep_root, .list_elem), elem) },
            .box => |elem| .{ .box = try self.childForRootType(self.structuralChildRoot(rep_root, .box_payload), elem) },
            .tuple => |span| .{ .tuple = try self.tuplePayloadForRootTypeSpan(rep_root, span) },
            .record => |record| .{ .record = try self.recordPayloadForRootTypeSpan(rep_root, record.fields) },
            .tag_union => |tag_union| .{ .tag_union = try self.tagUnionPayloadForRootTypeSpan(rep_root, tag_union.tags) },
            .func => representationInvariant("session executable root type payload requires callable representation for function type"),
        };
    }

    fn tuplePayloadForRootTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
        span: type_mod.Span(type_mod.TypeVarId),
    ) std.mem.Allocator.Error![]const SessionExecutableTupleElemPayload {
        const items = self.types.sliceTypeVarSpan(span);
        if (items.len == 0) return &.{};
        const out = try self.allocator.alloc(SessionExecutableTupleElemPayload, items.len);
        errdefer self.allocator.free(out);
        for (items, 0..) |item, i| {
            const child_root = self.structuralChildRoot(rep_root, .{ .tuple_elem = @intCast(i) });
            const child = try self.endpointForRootType(child_root, item);
            out[i] = .{
                .index = @intCast(i),
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn recordPayloadForRootTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
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
        if (self.row_shapes.recordShapeFields(shape).len != fields.len) representationInvariant("session executable root record payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableRecordFieldPayload, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            const field_id = self.recordFieldInShape(shape, field.name);
            const child_root = self.structuralChildRoot(rep_root, .{ .record_field = field_id });
            const child = try self.endpointForRootType(child_root, field.ty);
            out[i] = .{
                .field = field_id,
                .ty = child.ty,
                .key = child.key,
            };
        }
        return .{ .shape = shape, .fields = out };
    }

    fn tagUnionPayloadForRootTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
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
        if (self.row_shapes.tagUnionTags(shape).len != tags.len) representationInvariant("session executable root tag payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagVariantPayload, tags.len);
        for (out) |*variant| variant.* = .{ .tag = undefined, .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }

        for (tags, 0..) |tag, i| {
            const shape_tag = self.tagInShape(shape, tag.name);
            out[i] = .{
                .tag = shape_tag,
                .payloads = try self.tagPayloadsForRootTypeSpan(rep_root, shape_tag, tag.args),
            };
        }
        return .{ .shape = shape, .variants = out };
    }

    fn tagPayloadsForRootTypeSpan(
        self: *SessionExecutableTypePayloadBuilder,
        rep_root: RepRootId,
        tag: row.TagId,
        span: type_mod.Span(type_mod.TypeVarId),
    ) std.mem.Allocator.Error![]const SessionExecutableTagPayload {
        const args = self.types.sliceTypeVarSpan(span);
        if (args.len == 0) return &.{};
        const shape_payloads = self.row_shapes.tagPayloads(tag);
        if (shape_payloads.len != args.len) representationInvariant("session executable root tag payload arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagPayload, args.len);
        errdefer self.allocator.free(out);
        for (shape_payloads, args, 0..) |shape_payload, arg, i| {
            const child_root = self.structuralChildRoot(rep_root, .{ .tag_payload = shape_payload });
            const child = try self.endpointForRootType(child_root, arg);
            out[i] = .{
                .payload = shape_payload,
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn structuralChildRoot(
        self: *SessionExecutableTypePayloadBuilder,
        parent: RepRootId,
        kind: RepresentationEdgeKind,
    ) RepRootId {
        if (self.representation_store.solvedStructuralChildRoot(parent, kind)) |child| return child;
        representationInvariant("session executable root payload has no published structural child root");
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
                    .is_opaque = nominal.is_opaque,
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
        if (self.row_shapes.recordShapeFields(shape).len != fields.len) representationInvariant("session executable record payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableRecordFieldPayload, fields.len);
        errdefer self.allocator.free(out);
        for (fields, 0..) |field, i| {
            const child = try self.endpointForType(field.ty);
            out[i] = .{
                .field = self.recordFieldInShape(shape, field.name),
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
        if (self.row_shapes.tagUnionTags(shape).len != tags.len) representationInvariant("session executable tag payload shape arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagVariantPayload, tags.len);
        for (out) |*variant| variant.* = .{ .tag = undefined, .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }

        for (tags, 0..) |tag, i| {
            const shape_tag = self.tagInShape(shape, tag.name);
            out[i] = .{
                .tag = shape_tag,
                .payloads = try self.tagPayloadsForTypeSpan(shape_tag, tag.args),
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

    fn endpointForCallableGroup(
        self: *SessionExecutableTypePayloadBuilder,
        group: RepresentationGroupId,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const emission_plan = self.representation_store.callableGroupEmission(group) orelse {
            representationInvariant("session executable function value group has no callable emission plan");
        };
        return try self.endpointForCallableEmissionPlan(emission_plan);
    }

    fn endpointForCallableEmissionPlan(
        self: *SessionExecutableTypePayloadBuilder,
        emission_plan: CallableValueEmissionPlanId,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        const key = self.callableEmissionPlanKey(emission_plan);
        switch (self.representation_store.callableEmissionPlan(emission_plan)) {
            .finite => |callable_set_key| return try self.payloadForCallableSetType(callable_set_key, key),
            else => {},
        }
        if (self.payload_store.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }
        const id = try self.payload_store.append(self.allocator, key, try self.callablePayloadForEmissionPlan(emission_plan));
        return .{ .ty = refFor(id), .key = key };
    }

    fn endpointForVacantCallableSlot(
        self: *SessionExecutableTypePayloadBuilder,
        key: CanonicalExecValueTypeKey,
    ) std.mem.Allocator.Error!SessionExecutableTypeEndpoint {
        if (self.payload_store.refForKey(key)) |existing| {
            return .{ .ty = existing, .key = key };
        }
        const id = try self.payload_store.append(self.allocator, key, .vacant_callable_slot);
        return .{ .ty = refFor(id), .key = key };
    }

    fn callablePayloadForEmissionPlan(
        self: *SessionExecutableTypePayloadBuilder,
        emission_plan: CallableValueEmissionPlanId,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        return switch (self.representation_store.callableEmissionPlan(emission_plan)) {
            .pending_proc_value => representationInvariant("session executable callable payload reached pending proc-value emission"),
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
            .pending_proc_value => representationInvariant("session executable callable key reached pending proc-value emission"),
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
            .executable_key => |key| blk: {
                const expected = erased.sig_key.capture_ty orelse representationInvariant("already-erased session payload executable capture has no signature capture type");
                if (!canonicalExecValueTypeKeyEql(key, expected)) {
                    representationInvariant("already-erased session payload executable capture key differs from signature");
                }
                const capture = self.payload_store.refForKey(key) orelse {
                    representationInvariant("already-erased session payload executable capture key was not published");
                };
                break :blk .{ .ty = capture, .key = key };
            },
            .materialized_capture => blk: {
                const key = erased.sig_key.capture_ty orelse representationInvariant("already-erased session payload materialized capture has no signature capture type");
                const capture = self.payload_store.refForKey(key) orelse {
                    representationInvariant("already-erased session payload materialized capture key was not published");
                };
                break :blk .{ .ty = capture, .key = key };
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
        const id = try self.payload_store.replaceDerived(self.allocator, expected_key, .{
            .callable_set = try self.callableSetPayload(key),
        });
        return .{
            .ty = refFor(id),
            .key = expected_key,
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
        value_root: RepRootId,
        logical_ty: type_mod.TypeVarId,
        aggregate: AggregateValueInfo,
    ) std.mem.Allocator.Error!SessionExecutableTypePayload {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("session executable aggregate payload reached unresolved lambda-solved type"),
            .nominal => |nominal| blk: {
                const backing_root = self.structuralChildRoot(value_root, .{ .nominal_backing = nominal.nominal });
                const backing = try self.endpointForAggregateRootValue(backing_root, nominal.backing, aggregate);
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .source_ty = nominal.source_ty,
                    .is_opaque = nominal.is_opaque,
                    .backing = backing.ty,
                    .backing_key = backing.key,
                } };
            },
            .content => switch (aggregate) {
                .record => |record| .{ .record = try self.recordPayloadForValue(value_root, logical_ty, record) },
                .tuple => |tuple| .{ .tuple = try self.tuplePayloadForValue(value_root, logical_ty, tuple) },
                .tag => |tag| .{ .tag_union = try self.tagUnionPayloadForValue(logical_ty, tag) },
                .list => |list| .{ .list = try self.listPayloadForValue(logical_ty, list) },
            },
        };
    }

    fn recordPayloadForValue(
        self: *SessionExecutableTypePayloadBuilder,
        value_root: RepRootId,
        logical_ty: type_mod.TypeVarId,
        record: anytype,
    ) std.mem.Allocator.Error!SessionExecutableRecordPayload {
        const source_fields = try self.logicalRecordFields(logical_ty);
        if (record.fields.len == 0) return .{ .shape = record.shape, .fields = &.{} };
        const out = try self.allocator.alloc(SessionExecutableRecordFieldPayload, record.fields.len);
        errdefer self.allocator.free(out);
        for (record.fields, 0..) |field, i| {
            const field_ty = try self.logicalRecordFieldType(source_fields, field.field);
            const child_root = self.structuralChildRoot(value_root, .{ .record_field = field.field });
            const child = try self.childForRootType(child_root, field_ty);
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
        value_root: RepRootId,
        logical_ty: type_mod.TypeVarId,
        tuple: []const ElemValueInfo,
    ) std.mem.Allocator.Error![]const SessionExecutableTupleElemPayload {
        const source_items = try self.logicalTupleItems(logical_ty);
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
            if (index >= source_items.len) representationInvariant("session executable tuple payload index exceeded logical type arity");
            const child_root = self.structuralChildRoot(value_root, .{ .tuple_elem = elem.index });
            const child = try self.childForRootType(child_root, source_items[index]);
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

        const out = try self.allocator.alloc(SessionExecutableTagVariantPayload, source_tags.len);
        for (out) |*variant| variant.* = .{ .tag = undefined, .payloads = &.{} };
        errdefer {
            for (out) |variant| {
                if (variant.payloads.len > 0) self.allocator.free(variant.payloads);
            }
            self.allocator.free(out);
        }

        const seen_tags = try self.allocator.alloc(bool, source_tags.len);
        defer self.allocator.free(seen_tags);
        @memset(seen_tags, false);

        for (source_tags, 0..) |source_tag, i| {
            const shape_tag = self.tagInShape(tag_value.union_shape, source_tag.name);
            if (seen_tags[i]) representationInvariant("session executable tag payload saw duplicate source tag");
            out[i] = .{
                .tag = shape_tag,
                .payloads = try self.tagPayloadsForValueRoots(tag_value, shape_tag, source_tag.args),
            };
            seen_tags[i] = true;
        }
        for (seen_tags) |was_seen| {
            if (!was_seen) representationInvariant("session executable tag payload omitted a logical tag");
        }
        return .{ .shape = tag_value.union_shape, .variants = out };
    }

    fn tagPayloadsForValueRoots(
        self: *SessionExecutableTypePayloadBuilder,
        tag_value: anytype,
        tag: row.TagId,
        span: type_mod.Span(type_mod.TypeVarId),
    ) std.mem.Allocator.Error![]const SessionExecutableTagPayload {
        const args = self.types.sliceTypeVarSpan(span);
        if (args.len == 0) return &.{};
        const shape_payloads = self.row_shapes.tagPayloads(tag);
        if (shape_payloads.len != args.len) representationInvariant("session executable tag root payload arity mismatch");

        const out = try self.allocator.alloc(SessionExecutableTagPayload, args.len);
        errdefer self.allocator.free(out);
        for (shape_payloads, args, 0..) |shape_payload, arg, i| {
            if (tag == tag_value.tag and self.selectedTagPayloadValue(tag_value, shape_payload) == null) {
                representationInvariant("session executable selected tag omitted a payload");
            }
            const child = try self.childForRootType(self.tagPayloadRoot(tag_value, shape_payload), arg);
            out[i] = .{
                .payload = shape_payload,
                .ty = child.ty,
                .key = child.key,
            };
        }
        return out;
    }

    fn tagPayloadRoot(
        _: *SessionExecutableTypePayloadBuilder,
        tag_value: anytype,
        payload: row.TagPayloadId,
    ) RepRootId {
        for (tag_value.payload_roots) |payload_root| {
            if (payload_root.payload == payload) return payload_root.root;
        }
        representationInvariant("session executable tag payload root metadata omitted a payload");
    }

    fn selectedTagPayloadValue(
        _: *SessionExecutableTypePayloadBuilder,
        tag_value: anytype,
        payload: row.TagPayloadId,
    ) ?ValueInfoId {
        for (tag_value.payloads) |selected| {
            if (selected.payload == payload) return selected.value;
        }
        return null;
    }

    fn valueRequiresSpecificEndpoint(
        self: *SessionExecutableTypePayloadBuilder,
        value: ValueInfoId,
    ) std.mem.Allocator.Error!bool {
        var visited = std.AutoHashMap(ValueInfoId, void).init(self.allocator);
        defer visited.deinit();
        return try self.valueRequiresSpecificEndpointInner(value, &visited);
    }

    fn valueRequiresSpecificEndpointInner(
        self: *SessionExecutableTypePayloadBuilder,
        value: ValueInfoId,
        visited: *std.AutoHashMap(ValueInfoId, void),
    ) std.mem.Allocator.Error!bool {
        if (visited.contains(value)) return false;
        try visited.put(value, {});

        const values = self.value_store orelse representationInvariant("session executable value-specific predicate has no value store");
        const info = values.values.items[@intFromEnum(value)];
        if (info.callable != null) return true;
        if (self.valueHasFunctionType(info.logical_ty)) return true;
        if (info.value_alias_source) |source| return try self.valueRequiresSpecificEndpointInner(source, visited);
        if (info.join_info != null) return true;
        if (info.boxed) |boxed| {
            if (boxed.boundary != null) return true;
            if (boxed.payload_value) |payload| return try self.valueRequiresSpecificEndpointInner(payload, visited);
            return false;
        }
        if (info.aggregate) |aggregate| {
            return switch (aggregate) {
                .record => |record| for (record.fields) |field| {
                    if (try self.valueRequiresSpecificEndpointInner(field.value, visited)) break true;
                } else false,
                .tuple => |tuple| for (tuple) |elem| {
                    if (try self.valueRequiresSpecificEndpointInner(elem.value, visited)) break true;
                } else false,
                .tag => |tag| for (tag.payloads) |payload| {
                    if (try self.valueRequiresSpecificEndpointInner(payload.value, visited)) break true;
                } else false,
                .list => |list| for (list.elems) |elem| {
                    if (try self.valueRequiresSpecificEndpointInner(elem.value, visited)) break true;
                } else false,
            };
        }
        return false;
    }

    fn listPayloadForValue(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
        list: anytype,
    ) std.mem.Allocator.Error!SessionExecutableTypePayloadChild {
        const elem_ty = try self.logicalListElemType(logical_ty);
        return try self.childForRootType(list.elem_root, elem_ty);
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

    fn logicalTupleItems(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error![]const type_mod.TypeVarId {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalTupleItems(nominal.backing),
            .content => |content| switch (content) {
                .tuple => |items| self.types.sliceTypeVarSpan(items),
                else => representationInvariant("session executable tuple payload attached to non-tuple type"),
            },
            else => representationInvariant("session executable tuple payload attached to unresolved type"),
        };
    }

    fn logicalRecordFields(
        self: *SessionExecutableTypePayloadBuilder,
        logical_ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error![]const type_mod.Field {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalRecordFields(nominal.backing),
            .content => |content| switch (content) {
                .record => |record| self.types.sliceFields(record.fields),
                else => representationInvariant("session executable record payload attached to non-record type"),
            },
            else => representationInvariant("session executable record payload attached to unresolved type"),
        };
    }

    fn logicalRecordFieldType(
        self: *SessionExecutableTypePayloadBuilder,
        fields: []const type_mod.Field,
        field_id: row.RecordFieldId,
    ) std.mem.Allocator.Error!type_mod.TypeVarId {
        const expected_label = self.row_shapes.recordField(field_id).label;
        for (fields) |field| {
            if (field.name == expected_label) return field.ty;
        }
        representationInvariant("session executable record value field was absent from logical type");
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

/// Public `singletonCallableSetKey` function.
pub fn singletonCallableSetKey(
    source_proc: canonical.MirProcedureRef,
    proc_callable: canonical.ProcedureCallableRef,
    capture_shape_key: CaptureShapeKey,
    capture_slots: []const CallableSetCaptureSlot,
) CanonicalCallableSetKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "singleton_callable_set");
    writeMirProcedureRef(&hasher, source_proc);
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

/// Public `callableSetKeyForMembers` function.
pub fn callableSetKeyForMembers(
    members: []const CanonicalCallableSetMember,
) CanonicalCallableSetKey {
    if (members.len == 1) {
        return singletonCallableSetKey(
            members[0].source_proc,
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
        writeHashU32(&hasher, @intFromEnum(member.target_instance));
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
    row_shapes: ?*row.Store = null,
    types: *const type_mod.Store,
    representation_store: ?*const RepresentationStore = null,
    value_store: ?*const ValueInfoStore = null,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.AutoHashMap(type_mod.TypeVarId, u32),
    active_root_types: std.AutoHashMap(RootTypeKey, u32),
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
            .active_root_types = std.AutoHashMap(RootTypeKey, u32).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, u32).init(allocator),
        };
    }

    fn initForRootTypes(
        allocator: std.mem.Allocator,
        names: *const canonical.CanonicalNameStore,
        row_shapes: *row.Store,
        types: *const type_mod.Store,
        representation_store: *const RepresentationStore,
    ) ExecValueTypeKeyBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .row_shapes = row_shapes,
            .types = types,
            .representation_store = representation_store,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(type_mod.TypeVarId, u32).init(allocator),
            .active_root_types = std.AutoHashMap(RootTypeKey, u32).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, u32).init(allocator),
        };
    }

    fn initForValues(
        allocator: std.mem.Allocator,
        names: *const canonical.CanonicalNameStore,
        row_shapes: *row.Store,
        types: *const type_mod.Store,
        representation_store: *const RepresentationStore,
        value_store: *const ValueInfoStore,
    ) ExecValueTypeKeyBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .row_shapes = row_shapes,
            .types = types,
            .representation_store = representation_store,
            .value_store = value_store,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(type_mod.TypeVarId, u32).init(allocator),
            .active_root_types = std.AutoHashMap(RootTypeKey, u32).init(allocator),
            .active_values = std.AutoHashMap(ValueInfoId, u32).init(allocator),
        };
    }

    fn deinit(self: *ExecValueTypeKeyBuilder) void {
        self.active_values.deinit();
        self.active_root_types.deinit();
        self.active.deinit();
    }

    fn key(self: *ExecValueTypeKeyBuilder, root: type_mod.TypeVarId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        try self.writeType(root);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn keyForValue(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        if (try self.redirectedValueKey(value)) |redirected_key| return redirected_key;
        try self.writeValue(value);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn keyForRootType(
        self: *ExecValueTypeKeyBuilder,
        rep_root: RepRootId,
        ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        try self.writeRootType(rep_root, ty);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn keyForAggregateRootValue(
        self: *ExecValueTypeKeyBuilder,
        rep_root: RepRootId,
        logical_ty: type_mod.TypeVarId,
        aggregate: AggregateValueInfo,
    ) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        try self.writeAggregateValue(rep_root, logical_ty, aggregate);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn redirectedValueKey(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!?CanonicalExecValueTypeKey {
        const values = self.value_store orelse return null;
        const info = values.values.items[@intFromEnum(value)];
        if ((info.boxed != null or info.aggregate != null) and !(try self.valueRequiresSpecificKey(value))) {
            return try execValueTypeKeyForRootType(
                self.allocator,
                self.names,
                self.rowShapes(),
                self.types,
                self.representationStore(),
                info.root,
                info.logical_ty,
            );
        }
        if (info.callable != null or info.boxed != null or info.aggregate != null) return null;
        if (info.value_alias_source) |source| {
            const source_info = values.values.items[@intFromEnum(source)];
            if (source_info.exec_ty) |exec_ty| return exec_ty.key;
            return try self.valueKeySnapshot(source);
        }
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
        return try execValueTypeKeyForRootType(
            self.allocator,
            self.names,
            self.rowShapes(),
            self.types,
            self.representationStore(),
            join.root,
            values.values.items[@intFromEnum(value)].logical_ty,
        );
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
            try self.writeAggregateValue(info.root, info.logical_ty, aggregate);
        } else if (info.value_alias_source) |source| {
            try self.writeValue(source);
        } else if (info.join_info) |join_id| {
            const join_key = try self.joinValueKey(value, join_id);
            self.writeCanonicalExecValueTypeKey(join_key);
        } else {
            try self.writeRootType(info.root, info.logical_ty);
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
        const payload_ty = try self.logicalBoxPayloadType(logical_ty);
        try self.writeRootType(boxed.payload_root, payload_ty);
    }

    fn writeCallableValue(self: *ExecValueTypeKeyBuilder, callable: CallableValueInfo) std.mem.Allocator.Error!void {
        const representations = self.representation_store orelse representationInvariant("executable value type key for callable has no representation store");
        try self.writeCallableEmissionPlan(callable.emission_plan, representations);
    }

    fn writeAggregateValue(
        self: *ExecValueTypeKeyBuilder,
        value_root: RepRootId,
        logical_ty: TypeVarId,
        aggregate: AggregateValueInfo,
    ) std.mem.Allocator.Error!void {
        const root = self.types.unlinkConst(logical_ty);
        const active_key = try self.activeRootTypeKey(value_root, root);
        if (self.active_root_types.get(active_key)) |slot| {
            self.writeTag("aggregate_root_cycle");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.active_root_types.count());
        try self.active_root_types.put(active_key, slot);
        defer _ = self.active_root_types.remove(active_key);

        switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("executable aggregate value type key reached unresolved lambda-solved type"),
            .nominal => |nominal| {
                self.writeTag("nominal");
                self.writeBytes(self.names.moduleNameText(nominal.nominal.module_name));
                self.writeBytes(self.names.typeNameText(nominal.nominal.type_name));
                self.writeBool(nominal.is_opaque);
                try self.writeAggregateValue(
                    self.structuralChildRoot(value_root, .{ .nominal_backing = nominal.nominal }),
                    nominal.backing,
                    aggregate,
                );
            },
            .content => switch (aggregate) {
                .record => |record| {
                    const source_fields = try self.logicalRecordFields(logical_ty);
                    self.writeTag("record_value");
                    if (record.fields.len != source_fields.len) {
                        representationInvariant("executable value type key record value shape/logical arity mismatch");
                    }
                    self.writeU32(@intCast(source_fields.len));
                    for (source_fields) |field| {
                        self.writeBytes(self.names.recordFieldLabelText(field.name));
                        const field_id = self.recordFieldInShape(record.shape, field.name);
                        if (self.recordValueFieldByLabel(record.fields, field.name) == null) {
                            representationInvariant("executable value type key record value omitted a logical field");
                        }
                        const field_key = try execValueTypeKeyForRootType(
                            self.allocator,
                            self.names,
                            self.rowShapes(),
                            self.types,
                            self.representationStore(),
                            self.structuralChildRoot(value_root, .{ .record_field = field_id }),
                            field.ty,
                        );
                        self.writeCanonicalExecValueTypeKey(field_key);
                    }
                },
                .tuple => |tuple| {
                    const source_items = try self.logicalTupleItems(logical_ty);
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
                    for (ordered, 0..) |_, index| {
                        if (index >= source_items.len) representationInvariant("executable value type key tuple value index exceeded logical type arity");
                        const elem_key = try execValueTypeKeyForRootType(
                            self.allocator,
                            self.names,
                            self.rowShapes(),
                            self.types,
                            self.representationStore(),
                            self.structuralChildRoot(value_root, .{ .tuple_elem = @intCast(index) }),
                            source_items[index],
                        );
                        self.writeCanonicalExecValueTypeKey(elem_key);
                    }
                },
                .tag => |tag| {
                    self.writeTag("tag_value");
                    try self.writeTagUnionRootPayloadKeys(value_root, logical_ty, tag);
                },
                .list => |list| {
                    self.writeTag("list_value");
                    const elem_ty = try self.logicalListElemType(logical_ty);
                    try self.writeRootType(list.elem_root, elem_ty);
                },
            },
        }
    }

    fn valueKeySnapshot(self: *ExecValueTypeKeyBuilder, value: ValueInfoId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        const representations = self.representation_store orelse representationInvariant("executable value type key snapshot has no representation store");
        const values = self.value_store orelse representationInvariant("executable value type key snapshot has no value store");
        const row_shapes = self.row_shapes orelse representationInvariant("executable value type key snapshot has no row-shape store");
        return try execValueTypeKeyForValue(self.allocator, self.names, row_shapes, self.types, representations, values, value);
    }

    fn writeRootType(
        self: *ExecValueTypeKeyBuilder,
        rep_root: RepRootId,
        id: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!void {
        const root = self.types.unlinkConst(id);
        if (self.valueHasFunctionType(root)) {
            const representations = self.representationStore();
            const group = representations.groupForRoot(rep_root);
            if (representations.callableGroupEmission(group)) |emission| {
                try self.writeCallableEmissionPlan(emission, representations);
            } else {
                try self.writeVacantCallableSlotType(root);
            }
            return;
        }

        const active_key = try self.activeRootTypeKey(rep_root, root);
        if (self.active_root_types.get(active_key)) |slot| {
            self.writeTag("root_cycle");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.active_root_types.count());
        try self.active_root_types.put(active_key, slot);
        switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("executable value type key reached unresolved lambda-solved root type"),
            .nominal => |nominal| {
                self.writeTag("nominal");
                self.writeBytes(self.names.moduleNameText(nominal.nominal.module_name));
                self.writeBytes(self.names.typeNameText(nominal.nominal.type_name));
                self.writeBool(nominal.is_opaque);
                try self.writeRootType(
                    self.structuralChildRoot(rep_root, .{ .nominal_backing = nominal.nominal }),
                    nominal.backing,
                );
            },
            .content => |content| try self.writeRootContent(rep_root, content),
        }
        _ = self.active_root_types.remove(active_key);
    }

    fn writeRootContent(
        self: *ExecValueTypeKeyBuilder,
        rep_root: RepRootId,
        content: type_mod.Content,
    ) std.mem.Allocator.Error!void {
        switch (content) {
            .primitive => |prim| {
                self.writeTag("primitive");
                self.writeU32(@as(u32, @intCast(@intFromEnum(prim))));
            },
            .list => |elem| {
                self.writeTag("list");
                try self.writeRootType(self.structuralChildRoot(rep_root, .list_elem), elem);
            },
            .box => |elem| {
                self.writeTag("box");
                try self.writeRootType(self.structuralChildRoot(rep_root, .box_payload), elem);
            },
            .tuple => |items| {
                self.writeTag("tuple");
                const elems = self.types.sliceTypeVarSpan(items);
                self.writeU32(@intCast(elems.len));
                for (elems, 0..) |elem, i| {
                    try self.writeRootType(self.structuralChildRoot(rep_root, .{ .tuple_elem = @intCast(i) }), elem);
                }
            },
            .record => |record| {
                self.writeTag("record");
                const fields = self.types.sliceFields(record.fields);
                self.writeU32(@intCast(fields.len));
                const shape = try self.recordShapeForTypeFields(fields);
                if (self.rowShapes().recordShapeFields(shape).len != fields.len) {
                    representationInvariant("executable value type key record shape arity mismatch");
                }
                for (fields) |field| {
                    self.writeBytes(self.names.recordFieldLabelText(field.name));
                    const field_id = self.recordFieldInShape(shape, field.name);
                    try self.writeRootType(self.structuralChildRoot(rep_root, .{ .record_field = field_id }), field.ty);
                }
            },
            .tag_union => |tag_union| {
                self.writeTag("tag_union");
                const tags = self.types.sliceTags(tag_union.tags);
                self.writeU32(@intCast(tags.len));
                const shape = try self.tagUnionShapeForTypeTags(tags);
                if (self.rowShapes().tagUnionTags(shape).len != tags.len) {
                    representationInvariant("executable value type key tag shape arity mismatch");
                }
                for (tags) |tag| {
                    self.writeBytes(self.names.tagLabelText(tag.name));
                    const args = self.types.sliceTypeVarSpan(tag.args);
                    self.writeU32(@intCast(args.len));
                    const tag_id = self.tagInShape(shape, tag.name);
                    const shape_payloads = self.rowShapes().tagPayloads(tag_id);
                    if (shape_payloads.len != args.len) representationInvariant("executable value type key tag payload arity mismatch");
                    for (args, shape_payloads) |arg, payload_id| {
                        try self.writeRootType(self.structuralChildRoot(rep_root, .{ .tag_payload = payload_id }), arg);
                    }
                }
            },
            .func => representationInvariant("executable value type key requires callable representation for function root"),
        }
    }

    fn writeCallableEmissionPlan(
        self: *ExecValueTypeKeyBuilder,
        emission: CallableValueEmissionPlanId,
        representations: *const RepresentationStore,
    ) std.mem.Allocator.Error!void {
        switch (representations.callableEmissionPlan(emission)) {
            .pending_proc_value => representationInvariant("executable value type key reached pending proc-value emission"),
            .finite => |callable_set_key| {
                self.writeTag("callable_set");
                self.writeCanonicalCallableSetKey(callable_set_key);
            },
            .already_erased => |erased| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKeyRef(erased.sig_key);
            },
            .erase_proc_value => |erase| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKeyRef(erase.erased_fn_sig_key);
            },
            .erase_finite_set => |erase| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKeyRef(erase.adapter.erased_fn_sig_key);
            },
        }
    }

    fn writeVacantCallableSlotType(
        self: *ExecValueTypeKeyBuilder,
        root: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!void {
        self.writeTag("vacant_callable_slot");
        try self.writeSourceTypeAllowFunctions(root);
    }

    fn writeTagUnionRootPayloadKeys(
        self: *ExecValueTypeKeyBuilder,
        _: RepRootId,
        logical_ty: TypeVarId,
        tag_value: anytype,
    ) std.mem.Allocator.Error!void {
        const source_tags = try self.logicalTagUnionTags(logical_ty);
        const shape_tags = self.rowShapes().tagUnionTags(tag_value.union_shape);
        if (shape_tags.len != source_tags.len) representationInvariant("executable value type key tag shape/logical arity mismatch");
        self.writeU32(@intCast(source_tags.len));
        for (source_tags) |source_tag| {
            self.writeBytes(self.names.tagLabelText(source_tag.name));
            const shape_tag = self.tagInShape(tag_value.union_shape, source_tag.name);
            const args = self.types.sliceTypeVarSpan(source_tag.args);
            const payloads = self.rowShapes().tagPayloads(shape_tag);
            if (payloads.len != args.len) representationInvariant("executable value type key tag payload arity mismatch");
            self.writeU32(@intCast(payloads.len));
            for (payloads, args) |payload_id, arg| {
                self.writeU32(self.rowShapes().tagPayload(payload_id).logical_index);
                if (shape_tag == tag_value.tag and self.selectedTagPayloadValue(tag_value, payload_id) == null) {
                    representationInvariant("executable value type key selected tag omitted a payload");
                }
                try self.writeRootType(self.tagPayloadRoot(tag_value, payload_id), arg);
            }
        }
    }

    fn recordFieldInShape(
        self: *ExecValueTypeKeyBuilder,
        shape: row.RecordShapeId,
        label: canonical.RecordFieldLabelId,
    ) row.RecordFieldId {
        for (self.rowShapes().recordShapeFields(shape)) |field_id| {
            if (self.rowShapes().recordField(field_id).label == label) return field_id;
        }
        representationInvariant("executable value type key record field label missing from shape");
    }

    fn tagInShape(
        self: *ExecValueTypeKeyBuilder,
        shape: row.TagUnionShapeId,
        label: canonical.TagLabelId,
    ) row.TagId {
        for (self.rowShapes().tagUnionTags(shape)) |tag_id| {
            if (self.rowShapes().tag(tag_id).label == label) return tag_id;
        }
        representationInvariant("executable value type key tag label missing from shape");
    }

    fn recordValueFieldByLabel(
        self: *ExecValueTypeKeyBuilder,
        fields: []const FieldValueInfo,
        label: canonical.RecordFieldLabelId,
    ) ?ValueInfoId {
        for (fields) |field| {
            if (self.rowShapes().recordField(field.field).label == label) return field.value;
        }
        return null;
    }

    fn valueRequiresSpecificKey(
        self: *ExecValueTypeKeyBuilder,
        value: ValueInfoId,
    ) std.mem.Allocator.Error!bool {
        var visited = std.AutoHashMap(ValueInfoId, void).init(self.allocator);
        defer visited.deinit();
        return try self.valueRequiresSpecificKeyInner(value, &visited);
    }

    fn valueRequiresSpecificKeyInner(
        self: *ExecValueTypeKeyBuilder,
        value: ValueInfoId,
        visited: *std.AutoHashMap(ValueInfoId, void),
    ) std.mem.Allocator.Error!bool {
        if (visited.contains(value)) return false;
        try visited.put(value, {});

        const values = self.value_store orelse representationInvariant("executable value type key specific predicate has no value store");
        const info = values.values.items[@intFromEnum(value)];
        if (info.callable != null) return true;
        if (self.valueHasFunctionType(info.logical_ty)) return true;
        if (info.value_alias_source) |source| return try self.valueRequiresSpecificKeyInner(source, visited);
        if (info.join_info != null) return true;
        if (info.boxed) |boxed| {
            if (boxed.boundary != null) return true;
            if (boxed.payload_value) |payload| return try self.valueRequiresSpecificKeyInner(payload, visited);
            return false;
        }
        if (info.aggregate) |aggregate| {
            return switch (aggregate) {
                .record => |record| for (record.fields) |field| {
                    if (try self.valueRequiresSpecificKeyInner(field.value, visited)) break true;
                } else false,
                .tuple => |tuple| for (tuple) |elem| {
                    if (try self.valueRequiresSpecificKeyInner(elem.value, visited)) break true;
                } else false,
                .tag => |tag| for (tag.payloads) |payload| {
                    if (try self.valueRequiresSpecificKeyInner(payload.value, visited)) break true;
                } else false,
                .list => |list| for (list.elems) |elem| {
                    if (try self.valueRequiresSpecificKeyInner(elem.value, visited)) break true;
                } else false,
            };
        }
        return false;
    }

    fn logicalListElemType(self: *ExecValueTypeKeyBuilder, logical_ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalListElemType(nominal.backing),
            .content => |content| switch (content) {
                .list => |elem| elem,
                else => representationInvariant("executable value type key list payload attached to non-list type"),
            },
            else => representationInvariant("executable value type key list payload attached to unresolved type"),
        };
    }

    fn logicalBoxPayloadType(self: *ExecValueTypeKeyBuilder, logical_ty: TypeVarId) std.mem.Allocator.Error!TypeVarId {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalBoxPayloadType(nominal.backing),
            .content => |content| switch (content) {
                .box => |payload| payload,
                else => representationInvariant("executable value type key boxed payload attached to non-box type"),
            },
            else => representationInvariant("executable value type key boxed payload attached to unresolved type"),
        };
    }

    fn logicalTupleItems(self: *ExecValueTypeKeyBuilder, logical_ty: TypeVarId) std.mem.Allocator.Error![]const type_mod.TypeVarId {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalTupleItems(nominal.backing),
            .content => |content| switch (content) {
                .tuple => |items| self.types.sliceTypeVarSpan(items),
                else => representationInvariant("executable value type key tuple payload attached to non-tuple type"),
            },
            else => representationInvariant("executable value type key tuple payload attached to unresolved type"),
        };
    }

    fn logicalRecordFields(self: *ExecValueTypeKeyBuilder, logical_ty: TypeVarId) std.mem.Allocator.Error![]const type_mod.Field {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalRecordFields(nominal.backing),
            .content => |content| switch (content) {
                .record => |record| self.types.sliceFields(record.fields),
                else => representationInvariant("executable value type key record payload attached to non-record type"),
            },
            else => representationInvariant("executable value type key record payload attached to unresolved type"),
        };
    }

    fn logicalRecordFieldType(
        self: *ExecValueTypeKeyBuilder,
        fields: []const type_mod.Field,
        field_id: row.RecordFieldId,
    ) std.mem.Allocator.Error!TypeVarId {
        const expected_label = self.rowShapes().recordField(field_id).label;
        for (fields) |field| {
            if (field.name == expected_label) return field.ty;
        }
        representationInvariant("executable value type key record field was absent from logical type");
    }

    fn logicalTagUnionTags(self: *ExecValueTypeKeyBuilder, logical_ty: TypeVarId) std.mem.Allocator.Error![]const type_mod.Tag {
        const root = self.types.unlinkConst(logical_ty);
        return switch (self.types.getNode(root)) {
            .nominal => |nominal| try self.logicalTagUnionTags(nominal.backing),
            .content => |content| switch (content) {
                .tag_union => |tag_union| self.types.sliceTags(tag_union.tags),
                else => representationInvariant("executable value type key tag payload attached to non-tag-union type"),
            },
            else => representationInvariant("executable value type key tag payload attached to unresolved type"),
        };
    }

    fn tagPayloadRoot(_: *ExecValueTypeKeyBuilder, tag_value: anytype, payload: row.TagPayloadId) RepRootId {
        for (tag_value.payload_roots) |payload_root| {
            if (payload_root.payload == payload) return payload_root.root;
        }
        representationInvariant("executable value type key tag payload root metadata omitted a payload");
    }

    fn selectedTagPayloadValue(_: *ExecValueTypeKeyBuilder, tag_value: anytype, payload: row.TagPayloadId) ?ValueInfoId {
        for (tag_value.payloads) |selected| {
            if (selected.payload == payload) return selected.value;
        }
        return null;
    }

    fn structuralChildRoot(
        self: *ExecValueTypeKeyBuilder,
        parent: RepRootId,
        kind: RepresentationEdgeKind,
    ) RepRootId {
        if (self.representationStore().solvedStructuralChildRoot(parent, kind)) |child| return child;
        representationInvariant("executable value type key root has no published structural child root");
    }

    fn activeRootTypeKey(
        self: *ExecValueTypeKeyBuilder,
        rep_root: RepRootId,
        root_ty: type_mod.TypeVarId,
    ) std.mem.Allocator.Error!RootTypeKey {
        const root = self.types.unlinkConst(root_ty);
        const representations = self.representationStore();
        const group = representations.groupForRoot(rep_root);
        return switch (self.types.getNode(root)) {
            .link => unreachable,
            .unbd,
            .for_a,
            .flex_for_a,
            => representationInvariant("executable value type key active root reached unresolved lambda-solved type"),
            .nominal => |nominal| .{
                .group = group,
                .layer = .nominal,
                .nominal_module_name = @intFromEnum(nominal.nominal.module_name),
                .nominal_type_name = @intFromEnum(nominal.nominal.type_name),
                .is_opaque = nominal.is_opaque,
            },
            .content => |content| try self.activeRootContentKey(group, content),
        };
    }

    fn activeRootContentKey(
        self: *ExecValueTypeKeyBuilder,
        group: RepresentationGroupId,
        content: type_mod.Content,
    ) std.mem.Allocator.Error!RootTypeKey {
        return switch (content) {
            .primitive => |prim| .{
                .group = group,
                .layer = .primitive,
                .primitive = @intFromEnum(executablePrimitive(prim)),
            },
            .list => .{
                .group = group,
                .layer = .list,
            },
            .box => .{
                .group = group,
                .layer = .box,
            },
            .tuple => |span| .{
                .group = group,
                .layer = .tuple,
                .arity = @intCast(self.types.sliceTypeVarSpan(span).len),
            },
            .record => |record| .{
                .group = group,
                .layer = .record,
                .shape = @intFromEnum(try self.recordShapeForTypeFields(self.types.sliceFields(record.fields))),
            },
            .tag_union => |tag_union| .{
                .group = group,
                .layer = .tag_union,
                .shape = @intFromEnum(try self.tagUnionShapeForTypeTags(self.types.sliceTags(tag_union.tags))),
            },
            .func => representationInvariant("executable value type key active root requires callable representation for function type"),
        };
    }

    fn recordShapeForTypeFields(
        self: *ExecValueTypeKeyBuilder,
        fields: []const type_mod.Field,
    ) std.mem.Allocator.Error!row.RecordShapeId {
        if (fields.len == 0) return try self.rowShapes().internRecordShapeFromLabels(&.{});
        const labels = try self.allocator.alloc(canonical.RecordFieldLabelId, fields.len);
        defer self.allocator.free(labels);
        for (fields, 0..) |field, i| labels[i] = field.name;
        return try self.rowShapes().internRecordShapeFromLabels(labels);
    }

    fn tagUnionShapeForTypeTags(
        self: *ExecValueTypeKeyBuilder,
        tags: []const type_mod.Tag,
    ) std.mem.Allocator.Error!row.TagUnionShapeId {
        if (tags.len == 0) return try self.rowShapes().internTagUnionShapeFromDescriptors(&.{});
        const descriptors = try self.allocator.alloc(row.Store.TagShapeDescriptor, tags.len);
        defer self.allocator.free(descriptors);
        for (tags, 0..) |tag, i| {
            descriptors[i] = .{
                .name = tag.name,
                .payload_arity = @intCast(self.types.sliceTypeVarSpan(tag.args).len),
            };
        }
        return try self.rowShapes().internTagUnionShapeFromDescriptors(descriptors);
    }

    fn rowShapes(self: *ExecValueTypeKeyBuilder) *row.Store {
        return self.row_shapes orelse representationInvariant("executable value type key root traversal has no row-shape store");
    }

    fn representationStore(self: *ExecValueTypeKeyBuilder) *const RepresentationStore {
        return self.representation_store orelse representationInvariant("executable value type key root traversal has no representation store");
    }

    fn valueHasFunctionType(
        self: *ExecValueTypeKeyBuilder,
        ty: type_mod.TypeVarId,
    ) bool {
        const root = self.types.unlinkConst(ty);
        return switch (self.types.getNode(root)) {
            .content => |content| switch (content) {
                .func => true,
                else => false,
            },
            else => false,
        };
    }

    fn writeSourceTypeAllowFunctions(self: *ExecValueTypeKeyBuilder, id: type_mod.TypeVarId) std.mem.Allocator.Error!void {
        const root = self.types.unlinkConst(id);
        if (self.active.get(root)) |slot| {
            self.writeTag("source_cycle");
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
            => representationInvariant("vacant callable slot key reached unresolved lambda-solved type"),
            .nominal => |nominal| {
                self.writeTag("nominal");
                self.writeBytes(self.names.moduleNameText(nominal.nominal.module_name));
                self.writeBytes(self.names.typeNameText(nominal.nominal.type_name));
                self.hasher.update(&nominal.source_ty.bytes);
                self.writeBool(nominal.is_opaque);
                try self.writeSourceTypeSpanAllowFunctions(nominal.args);
                try self.writeSourceTypeAllowFunctions(nominal.backing);
            },
            .content => |content| try self.writeSourceContentAllowFunctions(content),
        }
        _ = self.active.remove(root);
    }

    fn writeSourceContentAllowFunctions(
        self: *ExecValueTypeKeyBuilder,
        content: type_mod.Content,
    ) std.mem.Allocator.Error!void {
        switch (content) {
            .primitive => |prim| {
                self.writeTag("primitive");
                self.writeU32(@as(u32, @intCast(@intFromEnum(prim))));
            },
            .list => |elem| {
                self.writeTag("list");
                try self.writeSourceTypeAllowFunctions(elem);
            },
            .box => |elem| {
                self.writeTag("box");
                try self.writeSourceTypeAllowFunctions(elem);
            },
            .tuple => |items| {
                self.writeTag("tuple");
                try self.writeSourceTypeSpanAllowFunctions(items);
            },
            .record => |record| {
                self.writeTag("record");
                const fields = self.types.sliceFields(record.fields);
                self.writeU32(@intCast(fields.len));
                for (fields) |field| {
                    self.writeBytes(self.names.recordFieldLabelText(field.name));
                    try self.writeSourceTypeAllowFunctions(field.ty);
                }
            },
            .tag_union => |tag_union| {
                self.writeTag("tag_union");
                const tags = self.types.sliceTags(tag_union.tags);
                self.writeU32(@intCast(tags.len));
                for (tags) |tag| {
                    self.writeBytes(self.names.tagLabelText(tag.name));
                    try self.writeSourceTypeSpanAllowFunctions(tag.args);
                }
            },
            .func => |func| {
                self.writeTag("func");
                self.writeU32(func.fixed_arity);
                try self.writeSourceTypeSpanAllowFunctions(func.args);
                try self.writeSourceTypeAllowFunctions(func.ret);
            },
        }
    }

    fn writeSourceTypeSpanAllowFunctions(
        self: *ExecValueTypeKeyBuilder,
        span: type_mod.Span(type_mod.TypeVarId),
    ) std.mem.Allocator.Error!void {
        const items = self.types.sliceTypeVarSpan(span);
        self.writeU32(@intCast(items.len));
        for (items) |item| try self.writeSourceTypeAllowFunctions(item);
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

    fn writeCanonicalCallableSetKey(self: *ExecValueTypeKeyBuilder, callable_set_key: CanonicalCallableSetKey) void {
        self.hasher.update(&callable_set_key.bytes);
    }

    fn writeCanonicalExecValueTypeKey(self: *ExecValueTypeKeyBuilder, exec_value_key: CanonicalExecValueTypeKey) void {
        self.hasher.update(&exec_value_key.bytes);
    }

    fn writeErasedFnSigKeyRef(self: *ExecValueTypeKeyBuilder, sig_key: ErasedFnSigKey) void {
        writeErasedFnSigKey(&self.hasher, sig_key);
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
