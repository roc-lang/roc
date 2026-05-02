//! Lambda-solved MIR representation/value-flow records.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const row = @import("../mono_row/mod.zig");
const debug = @import("../debug_verify.zig");
const type_mod = @import("type.zig");

const canonical = check.CanonicalNames;

pub const CallableVarId = type_mod.CallableVarId;
pub const RepRootId = enum(u32) { _ };
pub const ValueInfoId = enum(u32) { _ };
pub const BindingInfoId = enum(u32) { _ };
pub const ProjectionInfoId = enum(u32) { _ };
pub const CallSiteInfoId = enum(u32) { _ };
pub const JoinInfoId = enum(u32) { _ };
pub const BoxBoundaryId = canonical.BoxBoundaryId;
pub const CallableValueEmissionPlanId = enum(u32) { _ };
pub const CallableSetConstructionPlanId = enum(u32) { _ };
pub const CanonicalCallableSetDescriptorId = enum(u32) { _ };
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
pub const ErasedFnSigKey = canonical.ErasedFnSigKey;
pub const CallableSetMemberRef = canonical.CallableSetMemberRef;
pub const CallableSetCaptureSlot = canonical.CallableSetCaptureSlot;
pub const CanonicalCallableSetMember = canonical.CanonicalCallableSetMember;
pub const CanonicalCallableSetDescriptor = canonical.CanonicalCallableSetDescriptor;

pub const CallableMemberInstanceId = struct {
    proc_base: canonical.ProcBaseKeyRef,
    mono_specialization: canonical.MonoSpecializationKey,
    lambda_solved_instance: ProcRepresentationInstanceId,
};

pub const CallableRepresentation = canonical.CallableRepresentation;

pub const CallableReprMode = enum {
    direct,
    finite_callable_set,
    erased_callable,
    erased_adapter,
    intrinsic_wrapper,
};

pub const ExecutableSpecializationKey = struct {
    base: canonical.ProcBaseKeyRef,
    requested_fn_ty: canonical.CanonicalTypeKey,
    exec_arg_tys: []const CanonicalExecValueTypeKey,
    exec_ret_ty: CanonicalExecValueTypeKey,
    callable_repr_mode: CallableReprMode,
    capture_shape_key: CaptureShapeKey,
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
    capture: AlreadyErasedCapturePlan = .none,
    provenance: []const BoxBoundaryId = &.{},
};

pub const ErasedAdapterKey = canonical.ErasedAdapterKey;

pub const CallableValueEmissionPlan = union(enum) {
    finite: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erase_proc_value: ProcValueErasePlan,
    erase_finite_set: ErasedAdapterKey,
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
    finite: CanonicalCallableSetKey,
    erased: ErasedFnSigKey,
};

pub const CallSiteInfo = struct {
    callee: ?ValueInfoId,
    args: Span(ValueInfoId),
    result: ValueInfoId,
    requested_fn_root: RepRootId,
    requested_source_fn_ty: canonical.CanonicalTypeKey,
    dispatch: ?CallSiteDispatch = null,
};

pub const JoinKind = enum {
    if_expr,
    match_expr,
    loop_expr,
};

pub const JoinInfo = struct {
    result: ValueInfoId,
    inputs: Span(ValueInfoId),
    root: RepRootId,
    kind: JoinKind,
};

pub const ProcPublicValueRoots = struct {
    params: Span(ValueInfoId),
    ret: ValueInfoId,
    captures: Span(ValueInfoId),
    function_root: RepRootId,
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
    box_boundaries: []const BoxBoundary = &.{},

    pub fn init(allocator: std.mem.Allocator) RepresentationStore {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RepresentationStore) void {
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
                .finite,
                .erase_finite_set,
                => {},
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
            const exec_key = try execValueTypeKeyForValue(self.allocator, names, types, self, value_store, value);
            slots[i] = .{
                .slot = @intCast(i),
                .source_ty = .{ .bytes = exec_key.bytes },
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
    value_ids: std.ArrayList(ValueInfoId),

    pub fn init(allocator: std.mem.Allocator) ValueInfoStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .bindings = .empty,
            .projections = .empty,
            .call_sites = .empty,
            .joins = .empty,
            .value_ids = .empty,
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
};

pub fn canonicalTypeKeyEql(a: canonical.CanonicalTypeKey, b: canonical.CanonicalTypeKey) bool {
    return std.mem.eql(u8, a.bytes[0..], b.bytes[0..]);
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
            .erase_finite_set => |adapter| {
                self.writeTag("erased_fn");
                self.writeErasedFnSigKey(adapter.erased_fn_sig_key);
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
