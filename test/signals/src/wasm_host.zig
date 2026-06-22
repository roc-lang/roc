//! Browser-oriented Signals platform host symbols for wasm32 builds.
//!
//! This host links Signals Roc apps as wasm reactors and owns the browser-facing
//! host boundary: Roc descriptor ingestion, event dispatch, and render command
//! serialization for JavaScript to apply to the real DOM.

const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const render = @import("render_commands.zig");
const signal_graph = @import("signal_graph.zig");
const scope_tree = @import("scope_tree.zig");
const identity_table = @import("identity_table.zig");
const keyed_rows = @import("keyed_rows.zig");
const host_value_registry = @import("host_value_registry.zig");
const erased_calls = @import("erased_calls.zig");
const hv = @import("host_values.zig");
const engine = @import("engine.zig");

const HostValue = u64;
const WasmHostValueTypeTag = *u64;
const HostValueTypeTag = WasmHostValueTypeTag;
const HostValueRegistry = host_value_registry.Registry(HostValueTypeTag, false);
const HostValueList = abi.RocListWith(HostValue, false);
const RocStr = abi.RocStr;
const ElemBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const RenderTextField = render.TextField;
const RenderBoolField = render.BoolField;
const RenderEventKind = render.EventKind;
const SharedEngine = engine.Engine(WasmCtx);
const HostSignalCacheSlot = engine.HostSignalCacheSlot;
const HostSignalBinding = engine.HostSignalBinding;
const HostSignalRecord = engine.HostSignalRecord;
const HostSignalRecordPayload = engine.HostSignalRecordPayload;
const HostSignalToken = engine.HostSignalToken;
const HostNodeDescriptorStream = engine.HostNodeDescriptorStream;
const HostSignalEvalResult = engine.HostSignalEvalResult;

const WasmCtx = struct {
    pub const HostValueTypeTag = WasmHostValueTypeTag;
    pub const host_value_type_tags_enabled = false;
    pub const RegistryOps = hv.RegistryOps;
    pub const Metrics = engine.NoMetrics;

    pub fn zeroMetrics() Metrics {
        return .{};
    }

    pub fn allocator(_: anytype) std.mem.Allocator {
        return std.heap.wasm_allocator;
    }

    pub fn cloneHostValue(_: anytype, value: HostValue) HostValue {
        return host_values.clone(std.heap.wasm_allocator, value, registryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    pub fn stateValueByNodeId(_: anytype, node_id: u64) HostValue {
        const state = stateByNodeId(node_id);
        return host_values.clone(std.heap.wasm_allocator, state.value, registryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    pub fn stateEqCallable(_: anytype, node_id: u64) abi.RocErasedCallable {
        return stateByNodeId(node_id).eq;
    }

    pub fn stateDropCallable(_: anytype, node_id: u64) abi.RocErasedCallable {
        return stateByNodeId(node_id).drop;
    }

    pub fn sink(ctx: anytype) WasmSink {
        return .{ .emit = ctx.emit };
    }
};

const WasmRenderCtx = struct {
    emit: bool,
};

const WasmSink = struct {
    emit: bool,

    pub fn reset(self: WasmSink) void {
        if (!self.emit) return;
        appendCommand(.reset_dom, 0, 0, 0, 0, 0);
    }

    pub fn appendNode(self: WasmSink, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
        if (!self.emit) return;
        if (std.mem.eql(u8, tag, "text")) {
            appendStringCommand(.create_text, toU32(elem_id), "");
        } else {
            appendStringCommand(.create_element, toU32(elem_id), tag);
        }
        appendCommand(.append_child, toU32(parent_elem_id), toU32(elem_id), 0, 0, 0);
    }

    pub fn ensureNode(self: WasmSink, elem_id: u64, tag: []const u8) void {
        if (!self.emit) return;
        if (std.mem.eql(u8, tag, "text")) {
            appendStringCommand(.create_text, toU32(elem_id), "");
        } else {
            appendStringCommand(.create_element, toU32(elem_id), tag);
        }
    }

    pub fn removeNode(self: WasmSink, elem_id: u64) void {
        if (!self.emit) return;
        appendCommand(.remove_node, toU32(elem_id), 0, 0, 0, 0);
    }

    pub fn replaceChildren(self: WasmSink, _: u64, _: []const u64) void {
        if (self.emit) failHost();
    }

    pub fn replaceChildrenForMoves(self: WasmSink, _: u64, _: []const u64) void {
        if (self.emit) failHost();
    }

    pub fn applyTextField(self: WasmSink, elem_id: u64, field: RenderTextField, value: []const u8) void {
        if (!self.emit) return;
        appendStringCommand(field.setOp(), toU32(elem_id), value);
    }

    pub fn applyBoolField(self: WasmSink, elem_id: u64, field: RenderBoolField, value: bool) void {
        if (!self.emit) return;
        appendBoolFieldCommand(field, toU32(elem_id), value);
    }

    pub fn clearTextField(self: WasmSink, elem_id: u64, field: RenderTextField) void {
        if (!self.emit) return;
        appendStringCommand(field.setOp(), toU32(elem_id), "");
    }

    pub fn clearBoolField(self: WasmSink, elem_id: u64, field: RenderBoolField) void {
        if (!self.emit) return;
        appendBoolFieldCommand(field, toU32(elem_id), false);
    }

    pub fn bindEventKind(self: WasmSink, elem_id: u64, kind: RenderEventKind, event_id: u64) void {
        if (!self.emit) return;
        appendCommand(kind.bindOp(), toU32(elem_id), toU32(event_id), 0, 0, 0);
    }

    pub fn clearEvent(self: WasmSink, _: u64, _: RenderEventKind) void {
        if (self.emit) failHost();
    }

    pub fn debugAssertNode(_: WasmSink, _: u64, _: bool, _: ?[]const u8, _: ?u64, _: []const u64, _: ?u64, _: ?u64, _: ?u64) void {}
};

comptime {
    const BuildRecord = struct { id: u64 };
    const BuildRow = struct { site_ordinal: u64 };
    _ = signal_graph.Node(BuildRecord);
    _ = scope_tree.Scope(BuildRow);
    _ = scope_tree.Branch.false_branch.opposite();
    _ = identity_table.NodeIdentity;
    _ = identity_table.DomIdentity;
    _ = keyed_rows.RowPlan.create;
    _ = SharedEngine;
}

var shared_engine: SharedEngine = .init();
var host_values: HostValueRegistry = .{};
var command_buffer: render.Buffer = .{};
var string_buffer: std.ArrayListUnmanaged(u8) = .empty;
var roc_host_env: u8 = 0;
var roc_host = abi.RocHost{
    .env = @ptrCast(&roc_host_env),
    .roc_alloc = &rocAllocForAbi,
    .roc_dealloc = &rocDeallocForAbi,
    .roc_realloc = &rocReallocForAbi,
    .roc_dbg = &rocDbgForAbi,
    .roc_expect_failed = &rocExpectFailedForAbi,
    .roc_crashed = &rocCrashedForAbi,
};
var root_elem: ?abi.Elem = null;
var states: std.ArrayListUnmanaged(HostState) = .empty;
var active_events: std.ArrayListUnmanaged(ActiveEvent) = .empty;
var next_dom_id: u32 = 1;
var next_node_id: u64 = 1;

const EventPayloadKind = enum(u64) {
    unit = 1,
    str = 2,
    bool = 3,
};

const HostBinderToken = *u64;

const HostBinderBinding = struct {
    token: HostBinderToken,
    node_id: u64,
};

const HostState = struct {
    node_id: u64,
    value: HostValue,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,

    fn deinit(self: *HostState) void {
        callErasedHostValueToUnit(&roc_host, self.drop, self.value);
        self.* = undefined;
    }
};

const ActiveEvent = struct {
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_tag: HostValueTypeTag,
    payload_drop: abi.RocErasedCallable,
    transform: abi.RocErasedCallable,
};

const ErasedUnitArgs = erased_calls.ErasedUnitArgs;
const ErasedHostValueUnaryArgs = erased_calls.ErasedHostValueUnaryArgs;
const ErasedHostValueBinaryArgs = erased_calls.ErasedHostValueBinaryArgs;
const ErasedHostValueListUnaryArgs = erased_calls.ErasedHostValueListUnaryArgs;

fn failHost() noreturn {
    @trap();
}

fn allocator() std.mem.Allocator {
    return std.heap.wasm_allocator;
}

fn toU32(value: anytype) u32 {
    return std.math.cast(u32, value) orelse failHost();
}

fn checkedAdd(left: usize, right: usize) usize {
    return std.math.add(usize, left, right) catch failHost();
}

fn alignmentFromBytes(alignment: usize) std.mem.Alignment {
    if (alignment == 0 or !std.math.isPowerOfTwo(alignment)) failHost();
    return @enumFromInt(std.math.log2_int(usize, alignment));
}

fn allocWithHeader(length: usize, alignment_arg: usize) ?*anyopaque {
    const alignment = @max(alignment_arg, @sizeOf(usize));
    const align_log2 = alignmentFromBytes(alignment);
    const header_size = alignment;
    const total_size = checkedAdd(length, header_size);

    const base = std.heap.wasm_allocator.rawAlloc(total_size, align_log2, @returnAddress()) orelse return null;
    const size_ptr: *usize = @ptrCast(@alignCast(base));
    size_ptr.* = total_size;
    return @ptrCast(base + header_size);
}

fn appendCommand(op: render.Op, a: u32, b: u32, c: u32, d: u32, e: u32) void {
    command_buffer.append(allocator(), op, a, b, c, d, e) catch failHost();
}

fn clearCommandBuffers() void {
    command_buffer.clearRetainingCapacity();
    string_buffer.clearRetainingCapacity();
}

fn storeBytes(bytes: []const u8) u32 {
    const offset = toU32(string_buffer.items.len);
    string_buffer.appendSlice(allocator(), bytes) catch failHost();
    return offset;
}

fn appendStringCommand(op: render.Op, elem_id: u32, bytes: []const u8) void {
    appendCommand(op, elem_id, storeBytes(bytes), toU32(bytes.len), 0, 0);
}

fn appendBoolFieldCommand(field: RenderBoolField, elem_id: u32, value: bool) void {
    appendCommand(field.setOp(), elem_id, @intFromBool(value), 0, 0, 0);
}

fn nextDomId() u32 {
    const id = next_dom_id;
    if (id == 0 or id == std.math.maxInt(u32)) failHost();
    next_dom_id += 1;
    return id;
}

fn nextNodeId() u64 {
    const id = next_node_id;
    if (id == 0 or id == std.math.maxInt(u64)) failHost();
    next_node_id += 1;
    return id;
}

fn renderTextFieldFromAbi(field: u64) RenderTextField {
    return switch (field) {
        @intFromEnum(RenderTextField.text) => .text,
        @intFromEnum(RenderTextField.role) => .role,
        @intFromEnum(RenderTextField.label) => .label,
        @intFromEnum(RenderTextField.test_id) => .test_id,
        @intFromEnum(RenderTextField.value) => .value,
        else => failHost(),
    };
}

fn renderBoolFieldFromAbi(field: u64) RenderBoolField {
    return switch (field) {
        @intFromEnum(RenderBoolField.checked) => .checked,
        @intFromEnum(RenderBoolField.disabled) => .disabled,
        else => failHost(),
    };
}

fn renderEventKindFromAbi(kind: u64) RenderEventKind {
    return switch (kind) {
        @intFromEnum(RenderEventKind.click) => .click,
        @intFromEnum(RenderEventKind.input) => .input,
        @intFromEnum(RenderEventKind.check) => .check,
        else => failHost(),
    };
}

fn eventPayloadKindFromAbi(payload_kind: u64) EventPayloadKind {
    return switch (payload_kind) {
        @intFromEnum(EventPayloadKind.unit) => .unit,
        @intFromEnum(EventPayloadKind.str) => .str,
        @intFromEnum(EventPayloadKind.bool) => .bool,
        else => failHost(),
    };
}

fn resolveNodeBinderRef(binder_stack: []const HostBinderBinding, token: HostBinderToken) u64 {
    var index = binder_stack.len;
    while (index > 0) {
        index -= 1;
        const binding = binder_stack[index];
        if (binding.token == token) return binding.node_id;
    }
    failHost();
}

fn stateByNodeId(node_id: u64) *HostState {
    for (states.items) |*state| {
        if (state.node_id == node_id) return state;
    }
    failHost();
}

fn cloneHostValue(value: HostValue) HostValue {
    return host_values.clone(allocator(), value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

fn setHostValueTypeTag(value: HostValue, tag: HostValueTypeTag) void {
    host_values.setTag(value, tag, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

// `ctx` surface consumed by the shared `host_values` box constructors. The
// browser host has no test-kind bookkeeping, so `recordKind` is a no-op.
const HostValueOpsCtx = struct {
    pub fn store(_: HostValueOpsCtx, box: abi.RocBox) HostValue {
        return host_values.storeOwnedTag(allocator(), box, null, registryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    pub fn recordKind(_: HostValueOpsCtx, _: HostValue, _: hv.ValueKind) void {}
};

fn hostValueUnit() HostValue {
    return hv.makeUnit(HostValueOpsCtx{}, &roc_host);
}

fn hostValueStr(bytes: []const u8) HostValue {
    return hv.makeStr(HostValueOpsCtx{}, &roc_host, bytes);
}

fn hostValueBool(value: bool) HostValue {
    return hv.makeBool(HostValueOpsCtx{}, &roc_host, value);
}

fn updateStateValue(state: *HostState, value: HostValue) bool {
    if (callErasedHostValueHostValueToBool(&roc_host, state.eq, state.value, value)) {
        callErasedHostValueToUnit(&roc_host, state.drop, value);
        return false;
    }

    callErasedHostValueToUnit(&roc_host, state.drop, state.value);
    state.value = value;
    return true;
}

const erasedCallablePayload = erased_calls.erasedCallablePayload;
const callValueInitThunk = erased_calls.callValueInitThunk;
const callErasedHostValueToHostValue = erased_calls.callErasedHostValueToHostValue;
const callErasedHostValueHostValueToHostValue = erased_calls.callErasedHostValueHostValueToHostValue;
const callErasedHostValueListToHostValue = erased_calls.callErasedHostValueListToHostValue;
const callErasedHostValueHostValueToBool = erased_calls.callErasedHostValueHostValueToBool;
const callErasedHostValueToUnit = erased_calls.callErasedHostValueToUnit;
const callErasedHostValueToStr = erased_calls.callErasedHostValueToStr;
const callErasedHostValueToBool = erased_calls.callErasedHostValueToBool;

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

fn validateExistingSignalRecord(record: *HostSignalRecord, expected_tag: std.meta.Tag(HostSignalRecordPayload)) void {
    if (std.meta.activeTag(record.payload) != expected_tag) failHost();
}

fn bindNodeSignalExpr(stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) *HostSignalRecord {
    return switch (expr.tag) {
        .Ref => blk: {
            const node_id = resolveNodeBinderRef(binder_stack, expr.payload_ref());
            break :blk HostSignalRecord.init(allocator(), .{ .ref = node_id });
        },
        .ConstValue => blk: {
            const payload = expr.payload_const_value();
            const token: HostSignalToken = payload._0;
            if (stream.signalRecordByToken(token)) |record| {
                validateExistingSignalRecord(record, .const_value);
                break :blk record.retain();
            }

            const record = HostSignalRecord.init(allocator(), .{ .const_value = .{
                .token = engine.retainHostSignalToken(token),
                .init = engine.retainHostCallable(payload._1, &shared_engine.pending_roc_metrics),
                .eq = engine.retainHostCallable(payload._2, &shared_engine.pending_roc_metrics),
                .drop = engine.retainHostCallable(payload._3, &shared_engine.pending_roc_metrics),
            } });
            stream.rememberSignalRecord(allocator(), record);
            break :blk record;
        },
        .Map => blk: {
            const payload = expr.payload_map();
            const token: HostSignalToken = payload._0;
            if (stream.signalRecordByToken(token)) |record| {
                validateExistingSignalRecord(record, .map);
                break :blk record.retain();
            }

            const input = bindNodeSignalExpr(stream, payload._1.*, binder_stack);
            const record = HostSignalRecord.init(allocator(), .{ .map = .{
                .token = engine.retainHostSignalToken(token),
                .input = input,
                .transform = engine.retainHostCallable(payload._2, &shared_engine.pending_roc_metrics),
                .eq = engine.retainHostCallable(payload._3, &shared_engine.pending_roc_metrics),
                .drop = engine.retainHostCallable(payload._4, &shared_engine.pending_roc_metrics),
            } });
            stream.rememberSignalRecord(allocator(), record);
            break :blk record;
        },
        .Map2 => blk: {
            const payload = expr.payload_map2();
            const token: HostSignalToken = payload._0;
            if (stream.signalRecordByToken(token)) |record| {
                validateExistingSignalRecord(record, .map2);
                break :blk record.retain();
            }

            const left = bindNodeSignalExpr(stream, payload._1.*, binder_stack);
            const right = bindNodeSignalExpr(stream, payload._2.*, binder_stack);
            const record = HostSignalRecord.init(allocator(), .{ .map2 = .{
                .token = engine.retainHostSignalToken(token),
                .left = left,
                .right = right,
                .transform = engine.retainHostCallable(payload._3, &shared_engine.pending_roc_metrics),
                .eq = engine.retainHostCallable(payload._4, &shared_engine.pending_roc_metrics),
                .drop = engine.retainHostCallable(payload._5, &shared_engine.pending_roc_metrics),
            } });
            stream.rememberSignalRecord(allocator(), record);
            break :blk record;
        },
        .Combine => blk: {
            const payload = expr.payload_combine();
            const token: HostSignalToken = payload._0;
            if (stream.signalRecordByToken(token)) |record| {
                validateExistingSignalRecord(record, .combine);
                break :blk record.retain();
            }

            const source_children = payload._1.items();
            const children = allocator().alloc(*HostSignalRecord, source_children.len) catch failHost();
            for (source_children, children) |child, *dest| {
                dest.* = bindNodeSignalExpr(stream, child, binder_stack);
            }
            const record = HostSignalRecord.init(allocator(), .{ .combine = .{
                .token = engine.retainHostSignalToken(token),
                .children = children,
                .transform = engine.retainHostCallable(payload._2, &shared_engine.pending_roc_metrics),
                .eq = engine.retainHostCallable(payload._3, &shared_engine.pending_roc_metrics),
                .drop = engine.retainHostCallable(payload._4, &shared_engine.pending_roc_metrics),
            } });
            stream.rememberSignalRecord(allocator(), record);
            break :blk record;
        },
        .TaskSource => blk: {
            const payload = expr.payload_task_source();
            const token: HostSignalToken = payload.token;
            if (stream.signalRecordByToken(token)) |record| {
                validateExistingSignalRecord(record, .task_source);
                break :blk record.retain();
            }

            abi.increfBox(@ptrCast(payload.payload_tag), 1);
            const record = HostSignalRecord.init(allocator(), .{ .task_source = .{
                .token = engine.retainHostSignalToken(token),
                .name = allocator().dupe(u8, payload.name.asSlice()) catch failHost(),
                .payload_tag = @ptrCast(payload.payload_tag),
                .payload_drop = engine.retainHostCallable(payload.payload_drop, &shared_engine.pending_roc_metrics),
                .initial = engine.retainHostCallable(payload.initial, &shared_engine.pending_roc_metrics),
                .done = engine.retainHostCallable(payload.done, &shared_engine.pending_roc_metrics),
                .failed = engine.retainHostCallable(payload.failed, &shared_engine.pending_roc_metrics),
                .eq = engine.retainHostCallable(payload.eq, &shared_engine.pending_roc_metrics),
                .drop = engine.retainHostCallable(payload.drop, &shared_engine.pending_roc_metrics),
            } });
            stream.rememberSignalRecord(allocator(), record);
            break :blk record;
        },
        .IntervalSource => blk: {
            const payload = expr.payload_interval_source();
            const token: HostSignalToken = payload.token;
            if (stream.signalRecordByToken(token)) |record| {
                validateExistingSignalRecord(record, .interval_source);
                break :blk record.retain();
            }

            const record = HostSignalRecord.init(allocator(), .{ .interval_source = .{
                .token = engine.retainHostSignalToken(token),
                .period_ms = payload.period_ms,
                .initial = engine.retainHostCallable(payload.initial, &shared_engine.pending_roc_metrics),
                .tick = engine.retainHostCallable(payload.tick, &shared_engine.pending_roc_metrics),
                .eq = engine.retainHostCallable(payload.eq, &shared_engine.pending_roc_metrics),
                .drop = engine.retainHostCallable(payload.drop, &shared_engine.pending_roc_metrics),
            } });
            stream.rememberSignalRecord(allocator(), record);
            break :blk record;
        },
    };
}

fn appendSignalRecordSourceNodeIds(source_node_ids: *std.ArrayListUnmanaged(u64), record: *HostSignalRecord) void {
    switch (record.payload) {
        .ref => |node_id| {
            if (!u64SliceContains(source_node_ids.items, node_id)) {
                source_node_ids.append(allocator(), node_id) catch failHost();
            }
        },
        .const_value => {},
        .map => |payload| appendSignalRecordSourceNodeIds(source_node_ids, payload.input),
        .map2 => |payload| {
            appendSignalRecordSourceNodeIds(source_node_ids, payload.left);
            appendSignalRecordSourceNodeIds(source_node_ids, payload.right);
        },
        .combine => |payload| {
            for (payload.children) |child| {
                appendSignalRecordSourceNodeIds(source_node_ids, child);
            }
        },
        .task_source, .interval_source => {},
    }
}

fn bindNodeSignal(stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) HostSignalBinding {
    const record = bindNodeSignalExpr(stream, expr, binder_stack);
    var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
    appendSignalRecordSourceNodeIds(&source_node_ids, record);
    return .{
        .record = record,
        .source_node_ids = source_node_ids.toOwnedSlice(allocator()) catch failHost(),
    };
}

fn recordDerivedCall() void {
    shared_engine.recordDerivedCall();
}

fn cloneCachedSignalValue(cache_slot: *const HostSignalCacheSlot) HostValue {
    return shared_engine.cloneCachedSignalValue(WasmRenderCtx{ .emit = false }, cache_slot);
}

fn replaceSignalExprCacheAndClone(cache_slot: *HostSignalCacheSlot, value: HostValue, eq: abi.RocErasedCallable, drop: abi.RocErasedCallable) HostValue {
    cache_slot.replace(&roc_host, &shared_engine.pending_roc_metrics, value, eq, drop);
    return cloneCachedSignalValue(cache_slot);
}

fn hostSignalRecordDropCallable(record: *const HostSignalRecord) abi.RocErasedCallable {
    return shared_engine.hostSignalRecordDropCallable(WasmRenderCtx{ .emit = false }, record);
}

fn dropHostSignalRecordValue(record: *const HostSignalRecord, value: HostValue) void {
    shared_engine.dropHostSignalRecordValue(WasmRenderCtx{ .emit = false }, &roc_host, record, value);
}

fn evalHostSignalRecord(record: *HostSignalRecord) HostValue {
    switch (record.payload) {
        .ref => |node_id| return cloneHostValue(stateByNodeId(node_id).value),
        .const_value => |*payload| {
            const value = callValueInitThunk(&roc_host, payload.init);
            return replaceSignalExprCacheAndClone(&payload.cached_value, value, payload.eq, payload.drop);
        },
        .map => |*payload| {
            const input = evalHostSignalRecord(payload.input);
            defer dropHostSignalRecordValue(payload.input, input);
            recordDerivedCall();
            const value = callErasedHostValueToHostValue(&roc_host, payload.transform, input);
            return replaceSignalExprCacheAndClone(&payload.cached_value, value, payload.eq, payload.drop);
        },
        .map2 => |*payload| {
            const left = evalHostSignalRecord(payload.left);
            defer dropHostSignalRecordValue(payload.left, left);
            const right = evalHostSignalRecord(payload.right);
            defer dropHostSignalRecordValue(payload.right, right);
            recordDerivedCall();
            const value = callErasedHostValueHostValueToHostValue(&roc_host, payload.transform, left, right);
            return replaceSignalExprCacheAndClone(&payload.cached_value, value, payload.eq, payload.drop);
        },
        .combine => |*payload| {
            var values: std.ArrayListUnmanaged(HostValue) = .empty;
            errdefer {
                for (payload.children[0..values.items.len], values.items) |child, value| {
                    dropHostSignalRecordValue(child, value);
                }
                values.deinit(allocator());
            }

            for (payload.children) |child| {
                values.append(allocator(), evalHostSignalRecord(child)) catch failHost();
            }

            const list = HostValueList.fromSlice(values.items, &roc_host);
            defer list.decref(&roc_host);
            recordDerivedCall();
            const value = callErasedHostValueListToHostValue(&roc_host, payload.transform, list);
            for (payload.children, values.items) |child, child_value| {
                dropHostSignalRecordValue(child, child_value);
            }
            values.deinit(allocator());
            return replaceSignalExprCacheAndClone(&payload.cached_value, value, payload.eq, payload.drop);
        },
        .task_source => |*payload| {
            switch (payload.cached_value) {
                .present => return cloneCachedSignalValue(&payload.cached_value),
                .absent => {
                    const value = callValueInitThunk(&roc_host, payload.initial);
                    return replaceSignalExprCacheAndClone(&payload.cached_value, value, payload.eq, payload.drop);
                },
            }
        },
        .interval_source => |*payload| {
            switch (payload.cached_value) {
                .present => return cloneCachedSignalValue(&payload.cached_value),
                .absent => {
                    const value = callValueInitThunk(&roc_host, payload.initial);
                    return replaceSignalExprCacheAndClone(&payload.cached_value, value, payload.eq, payload.drop);
                },
            }
        },
    }
}

fn evalHostSignalBinding(signal: *HostSignalBinding) HostValue {
    return evalHostSignalRecord(signal.record);
}

fn evalDirtyHostSignalBinding(signal: *HostSignalBinding, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
    return shared_engine.evalDirtyHostSignalBinding(WasmRenderCtx{ .emit = true }, &roc_host, signal, dirty_source_node_ids, dirty_generation);
}

fn hostSignalBindingEqCallable(signal: *const HostSignalBinding) abi.RocErasedCallable {
    return shared_engine.hostSignalBindingEqCallable(WasmRenderCtx{ .emit = false }, signal);
}

fn hostSignalBindingDropCallable(signal: *const HostSignalBinding) abi.RocErasedCallable {
    return shared_engine.hostSignalBindingDropCallable(WasmRenderCtx{ .emit = false }, signal);
}

fn updateDirtySignalCache(cache_slot: *HostSignalCacheSlot, value: HostValue) bool {
    return shared_engine.updateDirtySignalCache(&roc_host, cache_slot, value);
}

fn appendState(node_id: u64, state: abi.__AnonStruct77) void {
    states.append(allocator(), .{
        .node_id = node_id,
        .value = callValueInitThunk(&roc_host, state.initial),
        .eq = state.eq,
        .drop = state.drop,
    }) catch failHost();
}

fn emitInitialSignalTextAttr(desc: *engine.HostNodeSignalTextAttrDesc) void {
    const value = evalHostSignalBinding(&desc.signal);
    const text = callErasedHostValueToStr(&roc_host, desc.read, value);
    defer text.decref(&roc_host);
    _ = shared_engine.applyRenderTextField(WasmRenderCtx{ .emit = false }, desc.elem_id, desc.field, text.asSlice());
    appendStringCommand(desc.field.setOp(), toU32(desc.elem_id), text.asSlice());
    desc.cached_value.replace(&roc_host, &shared_engine.pending_roc_metrics, value, hostSignalBindingEqCallable(&desc.signal), hostSignalBindingDropCallable(&desc.signal));
}

fn emitInitialSignalBoolAttr(desc: *engine.HostNodeSignalBoolAttrDesc) void {
    const value = evalHostSignalBinding(&desc.signal);
    const bool_value = callErasedHostValueToBool(&roc_host, desc.read, value);
    _ = shared_engine.applyRenderBoolField(WasmRenderCtx{ .emit = false }, desc.elem_id, desc.field, bool_value);
    appendBoolFieldCommand(desc.field, toU32(desc.elem_id), bool_value);
    desc.cached_value.replace(&roc_host, &shared_engine.pending_roc_metrics, value, hostSignalBindingEqCallable(&desc.signal), hostSignalBindingDropCallable(&desc.signal));
}

fn emitInitialSignalTextNode(desc: *engine.HostNodeSignalTextNodeDesc) void {
    const value = evalHostSignalBinding(&desc.signal);
    const text = callErasedHostValueToStr(&roc_host, desc.read, value);
    defer text.decref(&roc_host);
    _ = shared_engine.applyRenderTextField(WasmRenderCtx{ .emit = false }, desc.elem_id, .text, text.asSlice());
    appendStringCommand(.create_text, toU32(desc.elem_id), text.asSlice());
    appendCommand(.append_child, toU32(desc.parent_elem_id), toU32(desc.elem_id), 0, 0, 0);
    desc.cached_value.replace(&roc_host, &shared_engine.pending_roc_metrics, value, hostSignalBindingEqCallable(&desc.signal), hostSignalBindingDropCallable(&desc.signal));
}

fn collectNodeAttr(stream: *HostNodeDescriptorStream, elem_id: u32, attr: abi.NodeAttr, binder_stack: []const HostBinderBinding) void {
    switch (attr.tag) {
        .StaticText => {
            const payload = attr.payload_static_text();
            const field = renderTextFieldFromAbi(payload.field);
            stream.appendStaticTextAttr(allocator(), elem_id, field, payload.value.asSlice());
            _ = shared_engine.applyRenderTextField(WasmRenderCtx{ .emit = false }, elem_id, field, payload.value.asSlice());
            appendStringCommand(field.setOp(), elem_id, payload.value.asSlice());
        },
        .SignalText => {
            const payload = attr.payload_signal_text();
            const field = renderTextFieldFromAbi(payload.field);
            const signal = bindNodeSignal(stream, payload.signal.*, binder_stack);
            stream.appendSignalTextAttr(allocator(), &roc_host, &shared_engine.pending_roc_metrics, elem_id, field, signal, payload.read);
            emitInitialSignalTextAttr(&stream.signal_text_attrs.items[stream.signal_text_attrs.items.len - 1]);
        },
        .StaticBool => {
            const payload = attr.payload_static_bool();
            const field = renderBoolFieldFromAbi(payload.field);
            stream.appendStaticBoolAttr(allocator(), elem_id, field, payload.value);
            _ = shared_engine.applyRenderBoolField(WasmRenderCtx{ .emit = false }, elem_id, field, payload.value);
            appendBoolFieldCommand(field, elem_id, payload.value);
        },
        .SignalBool => {
            const payload = attr.payload_signal_bool();
            const field = renderBoolFieldFromAbi(payload.field);
            const signal = bindNodeSignal(stream, payload.signal.*, binder_stack);
            stream.appendSignalBoolAttr(allocator(), &roc_host, &shared_engine.pending_roc_metrics, elem_id, field, signal, payload.read);
            emitInitialSignalBoolAttr(&stream.signal_bool_attrs.items[stream.signal_bool_attrs.items.len - 1]);
        },
        .OnEvent => {
            const payload = attr.payload_on_event();
            const msg = payload.msg;
            const payload_kind = eventPayloadKindFromAbi(msg.payload_kind);
            const active = ActiveEvent{
                .target_node_id = resolveNodeBinderRef(binder_stack, msg.binder),
                .payload_kind = payload_kind,
                .payload_tag = switch (payload_kind) {
                    .unit => @ptrCast(msg.payload_unit_tag),
                    .str => @ptrCast(msg.payload_str_tag),
                    .bool => @ptrCast(msg.payload_bool_tag),
                },
                .payload_drop = msg.payload_drop,
                .transform = msg.transform,
            };
            active_events.append(allocator(), active) catch failHost();
            const event_id = toU32(active_events.items.len);
            appendCommand(renderEventKindFromAbi(payload.kind).bindOp(), elem_id, event_id, 0, 0, 0);
        },
    }
}

fn updateSignalTextField(elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: abi.RocErasedCallable, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
    const result = evalDirtyHostSignalBinding(signal, dirty_source_node_ids, dirty_generation);
    if (!result.changed) {
        callErasedHostValueToUnit(&roc_host, hostSignalBindingDropCallable(signal), result.value);
        return;
    }
    if (!updateDirtySignalCache(cache_slot, result.value)) return;
    const text = callErasedHostValueToStr(&roc_host, read, result.value);
    defer text.decref(&roc_host);
    _ = shared_engine.applyRenderTextField(WasmRenderCtx{ .emit = true }, elem_id, field, text.asSlice());
}

fn updateSignalBoolField(elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: abi.RocErasedCallable, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
    const result = evalDirtyHostSignalBinding(signal, dirty_source_node_ids, dirty_generation);
    if (!result.changed) {
        callErasedHostValueToUnit(&roc_host, hostSignalBindingDropCallable(signal), result.value);
        return;
    }
    if (!updateDirtySignalCache(cache_slot, result.value)) return;
    _ = shared_engine.applyRenderBoolField(WasmRenderCtx{ .emit = true }, elem_id, field, callErasedHostValueToBool(&roc_host, read, result.value));
}

fn updateSignalSinks(dirty_source_node_ids: []const u64, dirty_generation: u64) void {
    for (shared_engine.active_stream.signal_text_nodes.items) |*desc| {
        updateSignalTextField(desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation);
    }
    for (shared_engine.active_stream.signal_text_attrs.items) |*desc| {
        updateSignalTextField(desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation);
    }
    for (shared_engine.active_stream.signal_bool_attrs.items) |*desc| {
        updateSignalBoolField(desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation);
    }
}

fn collectElem(stream: *HostNodeDescriptorStream, elem: abi.Elem, parent_elem_id: u32, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding)) void {
    switch (elem.tag) {
        .Element => {
            const payload = elem.payload_element();
            const elem_id = nextDomId();
            _ = stream.appendElement(allocator(), elem_id, parent_elem_id, 0, payload.tag.asSlice());
            shared_engine.appendRenderNode(WasmRenderCtx{ .emit = false }, elem_id, parent_elem_id, payload.tag.asSlice());
            appendStringCommand(.create_element, elem_id, payload.tag.asSlice());
            for (payload.attrs.items()) |attr| {
                collectNodeAttr(stream, elem_id, attr, binder_stack.items);
            }
            appendCommand(.append_child, parent_elem_id, elem_id, 0, 0, 0);
            for (payload.children.items()) |child| {
                collectElem(stream, child, elem_id, binder_stack);
            }
        },
        .Text => {
            const elem_id = nextDomId();
            const text = elem.payload_text().asSlice();
            stream.appendTextNode(allocator(), elem_id, parent_elem_id, 0, text);
            shared_engine.appendRenderNode(WasmRenderCtx{ .emit = false }, elem_id, parent_elem_id, "text");
            _ = shared_engine.applyRenderTextField(WasmRenderCtx{ .emit = false }, elem_id, .text, text);
            appendStringCommand(.create_text, elem_id, text);
            appendCommand(.append_child, parent_elem_id, elem_id, 0, 0, 0);
        },
        .TextSignal => {
            const payload = elem.payload_text_signal();
            const elem_id = nextDomId();
            const signal = bindNodeSignal(stream, payload.signal.*, binder_stack.items);
            stream.appendSignalTextNode(allocator(), &roc_host, &shared_engine.pending_roc_metrics, elem_id, parent_elem_id, 0, signal, payload.read);
            shared_engine.appendRenderNode(WasmRenderCtx{ .emit = false }, elem_id, parent_elem_id, "text");
            emitInitialSignalTextNode(&stream.signal_text_nodes.items[stream.signal_text_nodes.items.len - 1]);
        },
        .State => {
            const payload = elem.payload_state();
            const node_id = nextNodeId();
            appendState(node_id, payload);
            binder_stack.append(allocator(), .{ .token = payload.binder, .node_id = node_id }) catch failHost();
            collectElem(stream, payload.child.*, parent_elem_id, binder_stack);
            _ = binder_stack.pop() orelse failHost();
        },
        .Cleanup, .Component, .Each, .OnChange, .When => failHost(),
    }
}

fn clearActiveRuntime() void {
    shared_engine.active_stream.deinit(allocator(), &roc_host, &shared_engine.pending_roc_metrics);
    shared_engine.deinitRenderCache(WasmRenderCtx{ .emit = false });
    active_events.clearRetainingCapacity();

    for (states.items) |*state| state.deinit();
    states.clearRetainingCapacity();

    if (root_elem) |root| {
        abi.decrefElem(root, &roc_host);
        root_elem = null;
    }

    if (host_values.hasLiveValues()) failHost();
    host_values.deinit(allocator());
    host_values = .{};
}

fn eventById(event_id: u32) ActiveEvent {
    if (event_id == 0 or event_id > active_events.items.len) failHost();
    return active_events.items[event_id - 1];
}

fn dispatchEvent(event_id: u32, payload_kind: EventPayloadKind, payload: HostValue) void {
    const event = eventById(event_id);
    if (event.payload_kind != payload_kind) failHost();
    setHostValueTypeTag(payload, event.payload_tag);
    defer callErasedHostValueToUnit(&roc_host, event.payload_drop, payload);

    const state = stateByNodeId(event.target_node_id);
    const current = cloneHostValue(state.value);
    defer callErasedHostValueToUnit(&roc_host, state.drop, current);
    const next = callErasedHostValueHostValueToHostValue(&roc_host, event.transform, current, payload);
    if (updateStateValue(state, next)) {
        const dirty_generation = shared_engine.nextDirtySignalGeneration();
        updateSignalSinks(&.{event.target_node_id}, dirty_generation);
    }
}

fn dropMovedElemPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

fn deallocWithHeader(ptr: *anyopaque, alignment_arg: usize) void {
    const alignment = @max(alignment_arg, @sizeOf(usize));
    const align_log2 = alignmentFromBytes(alignment);
    const header_size = alignment;
    const user_ptr: [*]u8 = @ptrCast(ptr);
    const base = user_ptr - header_size;
    const size_ptr: *const usize = @ptrCast(@alignCast(base));
    const total_size = size_ptr.*;
    std.heap.wasm_allocator.rawFree(base[0..total_size], align_log2, @returnAddress());
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocWithHeader(length, alignment);
}

export fn roc_dealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    deallocWithHeader(ptr, alignment);
}

export fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment_arg: usize) callconv(.c) ?*anyopaque {
    const alignment = @max(alignment_arg, @sizeOf(usize));
    const header_size = alignment;
    const user_ptr: [*]u8 = @ptrCast(ptr);
    const old_base = user_ptr - header_size;
    const old_size_ptr: *const usize = @ptrCast(@alignCast(old_base));
    const old_total_size = old_size_ptr.*;
    const old_user_size = old_total_size - header_size;

    const new_ptr = allocWithHeader(new_length, alignment) orelse return null;
    const new_user_ptr: [*]u8 = @ptrCast(new_ptr);
    @memcpy(new_user_ptr[0..@min(old_user_size, new_length)], user_ptr[0..@min(old_user_size, new_length)]);
    deallocWithHeader(ptr, alignment);
    return new_ptr;
}

export fn roc_ui_command_buffer_ptr() callconv(.c) usize {
    return command_buffer.ptrAddress();
}

export fn roc_ui_command_buffer_len() callconv(.c) usize {
    return command_buffer.len();
}

export fn roc_ui_string_buffer_ptr() callconv(.c) usize {
    if (string_buffer.items.len == 0) return 0;
    return @intFromPtr(string_buffer.items.ptr);
}

export fn roc_ui_string_buffer_len() callconv(.c) usize {
    return string_buffer.items.len;
}

export fn roc_ui_command_record_words() callconv(.c) usize {
    return render.Record.word_count;
}

export fn roc_ui_command_buffer_clear() callconv(.c) void {
    clearCommandBuffers();
}

/// Number of retained host values currently live in the registry.
///
/// The browser leak guard asserts this returns to zero after `roc_ui_unmount`,
/// proving the host drops every retained closure/value it stored while mounted.
export fn roc_ui_live_host_values() callconv(.c) usize {
    return host_values.liveCount();
}

export fn roc_ui_mount() callconv(.c) void {
    clearActiveRuntime();
    clearCommandBuffers();
    next_dom_id = 1;
    next_node_id = 1;

    const root_box: ElemBox = abi.roc_ui_init();
    root_elem = root_box.*;
    abi.decrefBoxWith(@ptrCast(root_box), @alignOf(abi.Elem), &dropMovedElemPayload, &roc_host);

    shared_engine.resetRenderTree(WasmRenderCtx{ .emit = false });
    appendCommand(.reset_dom, 0, 0, 0, 0, 0);
    var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
    defer binder_stack.deinit(allocator());
    collectElem(&shared_engine.active_stream, root_elem.?, 0, &binder_stack);
}

export fn roc_ui_event(event_id: u32, payload_kind_id: u32, payload_ptr: usize, payload_len: usize, bool_value: u32) callconv(.c) void {
    clearCommandBuffers();
    const payload_kind: EventPayloadKind = switch (payload_kind_id) {
        @intFromEnum(EventPayloadKind.unit) => .unit,
        @intFromEnum(EventPayloadKind.str) => .str,
        @intFromEnum(EventPayloadKind.bool) => .bool,
        else => failHost(),
    };
    const payload = switch (payload_kind) {
        .unit => hostValueUnit(),
        .str => hostValueStr((@as([*]const u8, @ptrFromInt(payload_ptr)))[0..payload_len]),
        .bool => hostValueBool(bool_value != 0),
    };
    dispatchEvent(event_id, payload_kind, payload);
}

export fn roc_ui_unmount() callconv(.c) void {
    clearActiveRuntime();
    clearCommandBuffers();
}

export fn roc_dbg(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_expect_failed(_: [*]const u8, _: usize) callconv(.c) void {}

export fn roc_crashed(_: [*]const u8, _: usize) callconv(.c) void {
    failHost();
}

fn rocAllocForAbi(_: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_alloc(length, alignment);
}

fn rocDeallocForAbi(_: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    roc_dealloc(ptr, alignment);
}

fn rocReallocForAbi(_: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return roc_realloc(ptr, new_length, alignment);
}

fn rocDbgForAbi(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_dbg(bytes, len);
}

fn rocExpectFailedForAbi(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_expect_failed(bytes, len);
}

fn rocCrashedForAbi(_: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    roc_crashed(bytes, len);
}

fn tagFromBox(box: abi.RocBox) ?HostValueTypeTag {
    const ptr = box orelse return null;
    return @ptrCast(@alignCast(ptr));
}

fn failHostValueRegistryError(_: host_value_registry.Error) noreturn {
    failHost();
}

fn registryOps() hv.RegistryOps {
    return .{ .roc_host = &roc_host };
}

export fn roc_host_value_clone(value: HostValue) callconv(.c) HostValue {
    return host_values.clone(std.heap.wasm_allocator, value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_get(value: HostValue) callconv(.c) abi.RocBox {
    return host_values.get(value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_get_tagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    defer registryOps().releaseTag(tag);
    return host_values.getTagged(value, tag, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_store(box: abi.RocBox) callconv(.c) HostValue {
    return host_values.storeOwnedTag(std.heap.wasm_allocator, box, null, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_store_tagged(box: abi.RocBox, tag_box: abi.RocBox) callconv(.c) HostValue {
    const tag = tagFromBox(tag_box);
    return host_values.storeOwnedTag(std.heap.wasm_allocator, box, tag, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_take(value: HostValue) callconv(.c) abi.RocBox {
    return host_values.take(value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_take_tagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    defer registryOps().releaseTag(tag);
    return host_values.takeTagged(value, tag, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}
