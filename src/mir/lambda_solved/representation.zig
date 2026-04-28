//! Lambda-solved MIR representation/value-flow records.

const std = @import("std");
const check = @import("check");
const row = @import("../mono_row/mod.zig");

const canonical = check.CanonicalNames;

pub const CallableVarId = enum(u32) { _ };
pub const RepRootId = enum(u32) { _ };
pub const ValueInfoId = enum(u32) { _ };
pub const BindingInfoId = enum(u32) { _ };
pub const ProjectionInfoId = enum(u32) { _ };
pub const CallSiteInfoId = enum(u32) { _ };
pub const BoxBoundaryId = enum(u32) { _ };
pub const RepresentationClassId = enum(u32) { _ };
pub const ProcRepresentationInstanceId = enum(u32) { _ };
pub const RepresentationSolveSessionId = enum(u32) { _ };
pub const ValueInfoStoreId = enum(u32) { _ };
pub const CallableSetMemberId = enum(u32) { _ };
pub const ErasedAdapterId = enum(u32) { _ };

pub const CanonicalCallableSetKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const CaptureShapeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const CanonicalExecValueTypeKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const ErasedFnAbiKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const ErasedFnSigKey = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    abi: ErasedFnAbiKey,
};

pub const CallableSetMemberRef = struct {
    callable_set_key: CanonicalCallableSetKey,
    member_index: CallableSetMemberId,
};

pub const CallableMemberInstanceId = struct {
    proc_base: canonical.ProcBaseKeyRef,
    mono_specialization: canonical.MonoSpecializationKey,
    lambda_solved_instance: ProcRepresentationInstanceId,
};

pub const CallableRepresentation = union(enum) {
    finite: CanonicalCallableSetKey,
    erased: ErasedFnSigKey,
};

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
    executable_specialization_key: canonical.CanonicalTypeKey,
    capture_shape_key: CaptureShapeKey,
    provenance: []const BoxBoundaryId,
};

pub const ErasedAdapterKey = struct {
    source_fn_ty: canonical.CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    erased_fn_sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
};

pub const CallableValueEmissionPlan = union(enum) {
    finite: CanonicalCallableSetKey,
    already_erased: ErasedFnSigKey,
    erase_proc_value: ProcValueErasePlan,
    erase_finite_set: ErasedAdapterKey,
};

pub const ValueInfo = struct {
    logical_ty: canonical.CanonicalTypeKey,
    root: RepRootId,
    solved_class: ?RepresentationClassId = null,
    callable: ?CallableValueEmissionPlan = null,
};

pub const CallSiteDispatch = union(enum) {
    finite: CanonicalCallableSetKey,
    erased: ErasedFnSigKey,
};

pub const CallSiteInfo = struct {
    callee: ValueInfoId,
    args: []const ValueInfoId,
    result: ValueInfoId,
    requested_fn_root: RepRootId,
    dispatch: CallSiteDispatch,
};

pub const ProcPublicValueRoots = struct {
    params: []const ValueInfoId,
    ret: ValueInfoId,
    captures: []const ValueInfoId,
    function_root: RepRootId,
};

pub const RepresentationSolveState = enum {
    reserved,
    building,
    solving,
    sealed,
};

pub const RepresentationStore = struct {
    roots_len: u32 = 0,
    classes_len: u32 = 0,
    box_boundaries: []const BoxBoundary = &.{},
};

pub const RepresentationSolveSession = struct {
    members: []const ProcRepresentationInstanceId,
    representation_store: RepresentationStore,
    state: RepresentationSolveState,
};

pub const ProcRepresentationInstance = struct {
    proc: canonical.ProcedureValueRef,
    executable_specialization_key: canonical.CanonicalTypeKey,
    solve_session: RepresentationSolveSessionId,
    value_store: ValueInfoStoreId,
    public_roots: ProcPublicValueRoots,
};

test "lambda-solved representation records are explicit" {
    std.testing.refAllDecls(@This());
}
