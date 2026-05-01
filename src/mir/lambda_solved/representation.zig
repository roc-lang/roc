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
pub const BoxBoundaryId = enum(u32) { _ };
pub const CallableValueEmissionPlanId = enum(u32) { _ };
pub const CallableSetConstructionPlanId = enum(u32) { _ };
pub const CanonicalCallableSetDescriptorId = enum(u32) { _ };
pub const RepresentationClassId = enum(u32) { _ };
pub const ProcRepresentationInstanceId = enum(u32) { _ };
pub const RepresentationSolveSessionId = enum(u32) { _ };
pub const ValueInfoStoreId = enum(u32) { _ };
pub const CallableSetMemberId = enum(u32) { _ };
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

pub const CallableSetCaptureSlot = struct {
    slot: u32,
    source_ty: canonical.CanonicalTypeKey,
    exec_value_ty: CanonicalExecValueTypeKey,
};

pub const CanonicalCallableSetMember = struct {
    member: CallableSetMemberId,
    proc_value: canonical.ProcedureCallableRef,
    capture_slots: []const CallableSetCaptureSlot,
    capture_shape_key: CaptureShapeKey,
};

pub const CanonicalCallableSetDescriptor = struct {
    key: CanonicalCallableSetKey,
    members: []const CanonicalCallableSetMember,
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
    executable_specialization_key: ExecutableSpecializationKey,
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

pub const CallableValueSource = union(enum) {
    proc_value: struct {
        proc: canonical.MonoSpecializedProcRef,
        captures: []const ValueInfoId,
        fn_ty: canonical.CanonicalTypeKey,
    },
    finite_set: CanonicalCallableSetKey,
    already_erased: ErasedFnSigKey,
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
    record: []const FieldValueInfo,
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

pub const RepresentationSolveSession = struct {
    members: []const ProcRepresentationInstanceId,
    representation_store: RepresentationStore,
    state: RepresentationSolveState,

    pub fn deinit(self: *RepresentationSolveSession) void {
        self.representation_store.deinit();
    }
};

pub const ProcRepresentationInstance = struct {
    proc: canonical.MonoSpecializedProcRef,
    executable_specialization_key: ExecutableSpecializationKey,
    solve_session: RepresentationSolveSessionId,
    value_store: ValueInfoStoreId,
    public_roots: ProcPublicValueRoots,
};

pub fn executableSpecializationKeyForProc(
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    value_store: *const ValueInfoStore,
    proc: canonical.MonoSpecializedProcRef,
    roots: ProcPublicValueRoots,
) std.mem.Allocator.Error!ExecutableSpecializationKey {
    const params = value_store.sliceValueSpan(roots.params);
    const arg_keys = if (params.len == 0)
        &.{}
    else
        try allocator.alloc(CanonicalExecValueTypeKey, params.len);
    errdefer if (arg_keys.len > 0) allocator.free(arg_keys);
    for (params, 0..) |param, i| {
        arg_keys[i] = try execValueTypeKeyForValue(allocator, names, types, value_store, param);
    }

    return .{
        .base = proc.proc.proc_base,
        .requested_fn_ty = proc.specialization.requested_mono_fn_ty,
        .exec_arg_tys = arg_keys,
        .exec_ret_ty = try execValueTypeKeyForValue(allocator, names, types, value_store, roots.ret),
        .callable_repr_mode = .direct,
        .capture_shape_key = try captureShapeKeyForValues(allocator, names, types, value_store, roots.captures),
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
    value_store: *const ValueInfoStore,
    value: ValueInfoId,
) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
    const info = value_store.values.items[@intFromEnum(value)];
    return try execValueTypeKey(allocator, names, types, info.logical_ty);
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
    value_store: *const ValueInfoStore,
    values: Span(ValueInfoId),
) std.mem.Allocator.Error!CaptureShapeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeHashTag(&hasher, "capture_shape");
    const captures = value_store.sliceValueSpan(values);
    writeHashU32(&hasher, @intCast(captures.len));
    for (captures, 0..) |capture, i| {
        writeHashU32(&hasher, @intCast(i));
        const key = try execValueTypeKeyForValue(allocator, names, types, value_store, capture);
        hasher.update(&key.bytes);
    }
    return .{ .bytes = hasher.finalResult() };
}

const ExecValueTypeKeyBuilder = struct {
    allocator: std.mem.Allocator,
    names: *const canonical.CanonicalNameStore,
    types: *const type_mod.Store,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.AutoHashMap(type_mod.TypeVarId, u32),

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
        };
    }

    fn deinit(self: *ExecValueTypeKeyBuilder) void {
        self.active.deinit();
    }

    fn key(self: *ExecValueTypeKeyBuilder, root: type_mod.TypeVarId) std.mem.Allocator.Error!CanonicalExecValueTypeKey {
        try self.writeType(root);
        return .{ .bytes = self.hasher.finalResult() };
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
};

fn writeHashTag(hasher: *std.crypto.hash.sha2.Sha256, tag: []const u8) void {
    writeHashBytes(hasher, tag);
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
