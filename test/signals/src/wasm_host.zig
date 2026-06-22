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

const HostValue = u64;
const HostValueTypeTag = *u64;
const HostValueRegistry = host_value_registry.Registry(HostValueTypeTag, false);
const HostValueList = abi.RocListWith(HostValue, false);
const RocStr = abi.RocStr;
const ElemBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const RenderTextField = render.TextField;
const RenderBoolField = render.BoolField;
const RenderEventKind = render.EventKind;

comptime {
    const BuildRecord = struct { id: u64 };
    const BuildRow = struct { site_ordinal: u64 };
    _ = signal_graph.Node(BuildRecord);
    _ = scope_tree.Scope(BuildRow);
    _ = scope_tree.Branch.false_branch.opposite();
    _ = identity_table.NodeIdentity;
    _ = identity_table.DomIdentity;
    _ = keyed_rows.RowPlan.create;
}

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
var active_text_sinks: std.ArrayListUnmanaged(TextSink) = .empty;
var active_text_attr_sinks: std.ArrayListUnmanaged(TextAttrSink) = .empty;
var active_bool_attr_sinks: std.ArrayListUnmanaged(BoolAttrSink) = .empty;
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

const BoundSignalConst = struct {
    init: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
};

const BoundSignalMap = struct {
    input: *BoundSignal,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
};

const BoundSignalMap2 = struct {
    left: *BoundSignal,
    right: *BoundSignal,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
};

const BoundSignalCombine = struct {
    children: []*BoundSignal,
    transform: abi.RocErasedCallable,
    eq: abi.RocErasedCallable,
    drop: abi.RocErasedCallable,
};

const BoundSignal = union(enum) {
    ref: u64,
    const_value: BoundSignalConst,
    map: BoundSignalMap,
    map2: BoundSignalMap2,
    combine: BoundSignalCombine,
};

const TextSink = struct {
    elem_id: u32,
    signal: *BoundSignal,
    read: abi.RocErasedCallable,

    fn deinit(self: *TextSink) void {
        deinitBoundSignal(self.signal);
        self.* = undefined;
    }
};

const TextAttrSink = struct {
    elem_id: u32,
    field: RenderTextField,
    signal: *BoundSignal,
    read: abi.RocErasedCallable,

    fn deinit(self: *TextAttrSink) void {
        deinitBoundSignal(self.signal);
        self.* = undefined;
    }
};

const BoolAttrSink = struct {
    elem_id: u32,
    field: RenderBoolField,
    signal: *BoundSignal,
    read: abi.RocErasedCallable,

    fn deinit(self: *BoolAttrSink) void {
        deinitBoundSignal(self.signal);
        self.* = undefined;
    }
};

const ErasedUnitArgs = extern struct {};

const ErasedHostValueUnaryArgs = extern struct {
    arg0: HostValue,
};

const ErasedHostValueBinaryArgs = extern struct {
    arg0: HostValue,
    arg1: HostValue,
};

const ErasedHostValueListUnaryArgs = extern struct {
    arg0: HostValueList,
};

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

fn hostValueUnit() HostValue {
    const payload = abi.allocateBox(0, @alignOf(u8), false, &roc_host);
    return host_values.storeOwnedTag(allocator(), @ptrCast(payload), null, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

fn hostValueStr(bytes: []const u8) HostValue {
    const payload: *RocStr = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(RocStr), @alignOf(RocStr), true, &roc_host)));
    payload.* = RocStr.fromSlice(bytes, &roc_host);
    return host_values.storeOwnedTag(allocator(), @ptrCast(payload), null, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

fn hostValueBool(value: bool) HostValue {
    const payload: *bool = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(bool), @alignOf(bool), false, &roc_host)));
    payload.* = value;
    return host_values.storeOwnedTag(allocator(), @ptrCast(payload), null, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
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

fn erasedCallablePayload(callable: abi.RocErasedCallable) *abi.RocErasedCallablePayload {
    if (callable == null) failHost();
    return abi.rocErasedCallablePayloadPtr(callable);
}

fn callValueInitThunk(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable) HostValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedUnitArgs{};
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueToHostValue(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) HostValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueHostValueToHostValue(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue, arg1: HostValue) HostValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueListToHostValue(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValueList) HostValue {
    const payload = erasedCallablePayload(callable);
    arg0.incref(1);
    var call_args = ErasedHostValueListUnaryArgs{ .arg0 = arg0 };
    var result: HostValue = undefined;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueHostValueToBool(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue, arg1: HostValue) bool {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: usize = 0;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return (result & 0xff) != 0;
}

fn callErasedHostValueToUnit(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) void {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: usize = 0;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
}

fn callErasedHostValueToStr(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) RocStr {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: RocStr = undefined;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedHostValueToBool(roc_host_ptr: *abi.RocHost, callable: abi.RocErasedCallable, arg0: HostValue) bool {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedHostValueUnaryArgs{ .arg0 = arg0 };
    var result: usize = 0;
    payload.callable_fn_ptr(
        roc_host_ptr,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return (result & 0xff) != 0;
}

fn bindNodeSignal(expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) *BoundSignal {
    const signal = allocator().create(BoundSignal) catch failHost();
    signal.* = switch (expr.tag) {
        .Ref => .{ .ref = resolveNodeBinderRef(binder_stack, expr.payload_ref()) },
        .ConstValue => .{ .const_value = .{
            .init = expr.payload_const_value()._1,
            .eq = expr.payload_const_value()._2,
            .drop = expr.payload_const_value()._3,
        } },
        .Map => map: {
            const payload = expr.payload_map();
            const input = bindNodeSignal(payload._1.*, binder_stack);
            break :map .{ .map = .{
                .input = input,
                .transform = payload._2,
                .eq = payload._3,
                .drop = payload._4,
            } };
        },
        .Map2 => map2: {
            const payload = expr.payload_map2();
            const left = bindNodeSignal(payload._1.*, binder_stack);
            const right = bindNodeSignal(payload._2.*, binder_stack);
            break :map2 .{ .map2 = .{
                .left = left,
                .right = right,
                .transform = payload._3,
                .eq = payload._4,
                .drop = payload._5,
            } };
        },
        .Combine => combine: {
            const payload = expr.payload_combine();
            const source_children = payload._1.items();
            const children = allocator().alloc(*BoundSignal, source_children.len) catch failHost();
            for (source_children, children) |child, *dest| {
                dest.* = bindNodeSignal(child, binder_stack);
            }
            break :combine .{ .combine = .{
                .children = children,
                .transform = payload._2,
                .eq = payload._3,
                .drop = payload._4,
            } };
        },
        .IntervalSource, .TaskSource => failHost(),
    };
    return signal;
}

fn deinitBoundSignal(signal: *BoundSignal) void {
    switch (signal.*) {
        .ref, .const_value => {},
        .map => |payload| deinitBoundSignal(payload.input),
        .map2 => |payload| {
            deinitBoundSignal(payload.left);
            deinitBoundSignal(payload.right);
        },
        .combine => |payload| {
            for (payload.children) |child| {
                deinitBoundSignal(child);
            }
            allocator().free(payload.children);
        },
    }
    allocator().destroy(signal);
}

fn boundSignalDropCallable(signal: *const BoundSignal) abi.RocErasedCallable {
    return switch (signal.*) {
        .ref => |node_id| stateByNodeId(node_id).drop,
        .const_value => |payload| payload.drop,
        .map => |payload| payload.drop,
        .map2 => |payload| payload.drop,
        .combine => |payload| payload.drop,
    };
}

fn dropBoundSignalValue(signal: *const BoundSignal, value: HostValue) void {
    callErasedHostValueToUnit(&roc_host, boundSignalDropCallable(signal), value);
}

fn evalBoundSignal(signal: *BoundSignal) HostValue {
    return switch (signal.*) {
        .ref => |node_id| cloneHostValue(stateByNodeId(node_id).value),
        .const_value => |payload| callValueInitThunk(&roc_host, payload.init),
        .map => |payload| blk: {
            const input = evalBoundSignal(payload.input);
            defer dropBoundSignalValue(payload.input, input);
            break :blk callErasedHostValueToHostValue(&roc_host, payload.transform, input);
        },
        .map2 => |payload| blk: {
            const left = evalBoundSignal(payload.left);
            defer dropBoundSignalValue(payload.left, left);
            const right = evalBoundSignal(payload.right);
            defer dropBoundSignalValue(payload.right, right);
            break :blk callErasedHostValueHostValueToHostValue(&roc_host, payload.transform, left, right);
        },
        .combine => |payload| blk: {
            var values: std.ArrayListUnmanaged(HostValue) = .empty;
            errdefer {
                for (payload.children[0..values.items.len], values.items) |child, value| {
                    dropBoundSignalValue(child, value);
                }
                values.deinit(allocator());
            }

            for (payload.children) |child| {
                values.append(allocator(), evalBoundSignal(child)) catch failHost();
            }

            const list = HostValueList.fromSlice(values.items, &roc_host);
            defer list.decref(&roc_host);
            const value = callErasedHostValueListToHostValue(&roc_host, payload.transform, list);
            for (payload.children, values.items) |child, child_value| {
                dropBoundSignalValue(child, child_value);
            }
            values.deinit(allocator());
            break :blk value;
        },
    };
}

fn appendState(node_id: u64, state: abi.__AnonStruct77) void {
    states.append(allocator(), .{
        .node_id = node_id,
        .value = callValueInitThunk(&roc_host, state.initial),
        .eq = state.eq,
        .drop = state.drop,
    }) catch failHost();
}

fn collectNodeAttr(elem_id: u32, attr: abi.NodeAttr, binder_stack: []const HostBinderBinding) void {
    switch (attr.tag) {
        .StaticText => {
            const payload = attr.payload_static_text();
            appendStringCommand(renderTextFieldFromAbi(payload.field).setOp(), elem_id, payload.value.asSlice());
        },
        .SignalText => {
            const payload = attr.payload_signal_text();
            const signal = bindNodeSignal(payload.signal.*, binder_stack);
            const field = renderTextFieldFromAbi(payload.field);
            emitTextAttrSink(elem_id, field, signal, payload.read);
        },
        .StaticBool => {
            const payload = attr.payload_static_bool();
            appendBoolFieldCommand(renderBoolFieldFromAbi(payload.field), elem_id, payload.value);
        },
        .SignalBool => {
            const payload = attr.payload_signal_bool();
            const signal = bindNodeSignal(payload.signal.*, binder_stack);
            const field = renderBoolFieldFromAbi(payload.field);
            emitBoolAttrSink(elem_id, field, signal, payload.read);
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

fn emitTextAttrSink(elem_id: u32, field: RenderTextField, signal: *BoundSignal, read: abi.RocErasedCallable) void {
    const value = evalBoundSignal(signal);
    defer dropBoundSignalValue(signal, value);
    const text = callErasedHostValueToStr(&roc_host, read, value);
    defer text.decref(&roc_host);
    appendStringCommand(field.setOp(), elem_id, text.asSlice());
    active_text_attr_sinks.append(allocator(), .{
        .elem_id = elem_id,
        .field = field,
        .signal = signal,
        .read = read,
    }) catch failHost();
}

fn emitBoolAttrSink(elem_id: u32, field: RenderBoolField, signal: *BoundSignal, read: abi.RocErasedCallable) void {
    const value = evalBoundSignal(signal);
    defer dropBoundSignalValue(signal, value);
    appendBoolFieldCommand(field, elem_id, callErasedHostValueToBool(&roc_host, read, value));
    active_bool_attr_sinks.append(allocator(), .{
        .elem_id = elem_id,
        .field = field,
        .signal = signal,
        .read = read,
    }) catch failHost();
}

fn emitTextSink(parent_elem_id: u32, signal: *BoundSignal, read: abi.RocErasedCallable) void {
    const elem_id = nextDomId();
    const value = evalBoundSignal(signal);
    defer dropBoundSignalValue(signal, value);
    const text = callErasedHostValueToStr(&roc_host, read, value);
    defer text.decref(&roc_host);
    appendStringCommand(.create_text, elem_id, text.asSlice());
    appendCommand(.append_child, parent_elem_id, elem_id, 0, 0, 0);
    active_text_sinks.append(allocator(), .{
        .elem_id = elem_id,
        .signal = signal,
        .read = read,
    }) catch failHost();
}

fn updateTextSink(sink: *TextSink) void {
    const value = evalBoundSignal(sink.signal);
    defer dropBoundSignalValue(sink.signal, value);
    const text = callErasedHostValueToStr(&roc_host, sink.read, value);
    defer text.decref(&roc_host);
    appendStringCommand(.set_text, sink.elem_id, text.asSlice());
}

fn updateTextAttrSink(sink: *TextAttrSink) void {
    const value = evalBoundSignal(sink.signal);
    defer dropBoundSignalValue(sink.signal, value);
    const text = callErasedHostValueToStr(&roc_host, sink.read, value);
    defer text.decref(&roc_host);
    appendStringCommand(sink.field.setOp(), sink.elem_id, text.asSlice());
}

fn updateBoolAttrSink(sink: *BoolAttrSink) void {
    const value = evalBoundSignal(sink.signal);
    defer dropBoundSignalValue(sink.signal, value);
    appendBoolFieldCommand(sink.field, sink.elem_id, callErasedHostValueToBool(&roc_host, sink.read, value));
}

fn updateSignalSinks() void {
    for (active_text_sinks.items) |*sink| updateTextSink(sink);
    for (active_text_attr_sinks.items) |*sink| updateTextAttrSink(sink);
    for (active_bool_attr_sinks.items) |*sink| updateBoolAttrSink(sink);
}

fn collectElem(elem: abi.Elem, parent_elem_id: u32, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding)) void {
    switch (elem.tag) {
        .Element => {
            const payload = elem.payload_element();
            const elem_id = nextDomId();
            appendStringCommand(.create_element, elem_id, payload.tag.asSlice());
            for (payload.attrs.items()) |attr| {
                collectNodeAttr(elem_id, attr, binder_stack.items);
            }
            appendCommand(.append_child, parent_elem_id, elem_id, 0, 0, 0);
            for (payload.children.items()) |child| {
                collectElem(child, elem_id, binder_stack);
            }
        },
        .Text => {
            const elem_id = nextDomId();
            appendStringCommand(.create_text, elem_id, elem.payload_text().asSlice());
            appendCommand(.append_child, parent_elem_id, elem_id, 0, 0, 0);
        },
        .TextSignal => {
            const payload = elem.payload_text_signal();
            emitTextSink(parent_elem_id, bindNodeSignal(payload.signal.*, binder_stack.items), payload.read);
        },
        .State => {
            const payload = elem.payload_state();
            const node_id = nextNodeId();
            appendState(node_id, payload);
            binder_stack.append(allocator(), .{ .token = payload.binder, .node_id = node_id }) catch failHost();
            collectElem(payload.child.*, parent_elem_id, binder_stack);
            _ = binder_stack.pop() orelse failHost();
        },
        .Cleanup, .Component, .Each, .OnChange, .When => failHost(),
    }
}

fn clearActiveRuntime() void {
    for (active_text_sinks.items) |*sink| sink.deinit();
    active_text_sinks.clearRetainingCapacity();
    for (active_text_attr_sinks.items) |*sink| sink.deinit();
    active_text_attr_sinks.clearRetainingCapacity();
    for (active_bool_attr_sinks.items) |*sink| sink.deinit();
    active_bool_attr_sinks.clearRetainingCapacity();
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
        updateSignalSinks();
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

export fn roc_ui_mount() callconv(.c) void {
    clearActiveRuntime();
    clearCommandBuffers();
    next_dom_id = 1;
    next_node_id = 1;

    const root_box: ElemBox = abi.roc_ui_init();
    root_elem = root_box.*;
    abi.decrefBoxWith(@ptrCast(root_box), @alignOf(abi.Elem), &dropMovedElemPayload, &roc_host);

    appendCommand(.reset_dom, 0, 0, 0, 0, 0);
    var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
    defer binder_stack.deinit(allocator());
    collectElem(root_elem.?, 0, &binder_stack);
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

const HostValueRegistryOps = struct {
    pub fn retainBox(_: @This(), box: abi.RocBox) void {
        abi.increfBox(box, 1);
    }

    pub fn releaseBox(_: @This(), box: abi.RocBox) void {
        abi.decrefBox(box, &roc_host);
    }

    pub fn retainTag(_: @This(), tag: HostValueTypeTag) void {
        abi.increfBox(@ptrCast(tag), 1);
    }

    pub fn releaseTag(_: @This(), tag: HostValueTypeTag) void {
        abi.decrefBox(@ptrCast(tag), &roc_host);
    }

    pub fn tagId(_: @This(), tag: HostValueTypeTag) u64 {
        return tag.*;
    }
};

fn failHostValueRegistryError(_: host_value_registry.Error) noreturn {
    failHost();
}

fn registryOps() HostValueRegistryOps {
    return .{};
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
