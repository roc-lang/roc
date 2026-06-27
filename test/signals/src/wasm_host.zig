//! Browser-oriented Signals platform host symbols for wasm32 builds.
//!
//! This host links Signals Roc apps as wasm reactors and owns the browser-facing
//! boundary only: `roc_alloc` marshalling, the command-buffer sink serialized
//! into linear memory, and the integer event/payload codec JavaScript drives.
//!
//! All reactive and structural behaviour lives in the shared `engine.zig`. Like
//! the native host, this file is a thin shell: it provides a `Ctx` (`WasmCtx`)
//! plus a render `sink()` and drives the engine's collect/apply/dispatch path.
//! It deliberately holds no reactive or structural logic of its own.

const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const render = @import("render_commands.zig");
const host_value_registry = @import("host_value_registry.zig");
const erased_calls = @import("erased_calls.zig");
const hv = @import("host_values.zig");
const engine = @import("engine.zig");

const HostValue = u64;
const HostValueCapability = hv.HostValueCapabilityHandle;
const HostValueList = abi.RocListWith(HostValue, false);
const ElemBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const RenderTextField = render.TextField;
const RenderBoolField = render.BoolField;
const RenderEventKind = render.EventKind;
const SharedEngine = engine.Engine(WasmCtx);
const HostNodeDescriptorStream = engine.HostNodeDescriptorStream;
const EventPayloadKind = engine.EventPayloadKind;
const HostActiveEventDesc = SharedEngine.ActiveEventDesc;

const WasmCtx = struct {
    pub const Handle = WasmCtx;
    pub const RegistryOps = hv.RegistryOps();
    pub const Metrics = engine.NoMetrics;
    pub const Sink = WasmSink;

    pub fn zeroMetrics() Metrics {
        return .{};
    }

    pub fn allocator(_: Handle) std.mem.Allocator {
        return std.heap.wasm_allocator;
    }

    pub fn cloneHostValue(_: Handle, value: HostValue) HostValue {
        return shared_engine.host_values.clone(std.heap.wasm_allocator, value, registryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    pub fn stateValueByNodeId(_: Handle, node_id: u64) HostValue {
        return currentStateValue(node_id);
    }

    pub fn stateCapability(_: Handle, node_id: u64) HostValueCapability {
        return shared_engine.stateCapability(node_id) catch failHost();
    }

    pub fn sink(_: Handle) Sink {
        return .{};
    }

    pub fn debugPhase(_: Handle, phase: u32) void {
        roc_allocation_phase = phase;
    }

    pub fn pushHostValueCapabilities(_: Handle, caps: []const HostValueCapability) void {
        active_capabilities.push(caps);
    }

    pub fn popHostValueCapabilities(_: Handle) void {
        active_capabilities.pop();
    }
};

// The engine threads a zero-sized `WasmCtx{}` value through every
// collect/apply/dispatch call and reaches host state through its methods plus the
// `shared_engine` global. The command-buffer sink always emits — there is no
// silent "build cache only" phase in the browser host (teardown never touches
// the sink), so the sink carries no state.
const WasmSink = struct {
    pub fn reset(_: WasmSink) void {
        appendCommand(.reset_dom, 0, 0, 0, 0, 0);
    }

    pub fn appendNode(_: WasmSink, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
        if (std.mem.eql(u8, tag, "text")) {
            appendStringCommand(.create_text, toU32(elem_id), "");
        } else {
            appendStringCommand(.create_element, toU32(elem_id), tag);
        }
        appendCommand(.append_child, toU32(parent_elem_id), toU32(elem_id), 0, 0, 0);
    }

    pub fn ensureNode(_: WasmSink, elem_id: u64, tag: []const u8) void {
        if (std.mem.eql(u8, tag, "text")) {
            appendStringCommand(.create_text, toU32(elem_id), "");
        } else {
            appendStringCommand(.create_element, toU32(elem_id), tag);
        }
    }

    pub fn removeNode(_: WasmSink, elem_id: u64) void {
        appendCommand(.remove_node, toU32(elem_id), 0, 0, 0, 0);
    }

    // The engine hands the sink the final child order; achieving it in the real
    // DOM is a sequence of `appendChild`s, which the JS executor treats as a move
    // for already-attached nodes and a parent-link for freshly created ones. The
    // engine still computes the minimal-move count for its telemetry; this thin
    // executor just realises the order it was given.
    pub fn replaceChildren(_: WasmSink, parent_elem_id: u64, next_child_ids: []const u64) void {
        emitAppendChildren(parent_elem_id, next_child_ids);
    }

    pub fn replaceChildrenForMoves(_: WasmSink, parent_elem_id: u64, next_child_ids: []const u64) void {
        emitAppendChildren(parent_elem_id, next_child_ids);
    }

    pub fn applyTextField(_: WasmSink, elem_id: u64, field: RenderTextField, value: []const u8) void {
        appendStringCommand(field.setOp(), toU32(elem_id), value);
    }

    pub fn applyBoolField(_: WasmSink, elem_id: u64, field: RenderBoolField, value: bool) void {
        appendBoolFieldCommand(field, toU32(elem_id), value);
    }

    pub fn clearTextField(_: WasmSink, elem_id: u64, field: RenderTextField) void {
        appendStringCommand(field.setOp(), toU32(elem_id), "");
    }

    pub fn clearBoolField(_: WasmSink, elem_id: u64, field: RenderBoolField) void {
        appendBoolFieldCommand(field, toU32(elem_id), false);
    }

    pub fn bindEventKind(_: WasmSink, elem_id: u64, kind: RenderEventKind, event_id: u64, payload_accessor: engine.EventPayloadAccessor) void {
        appendCommand(kind.bindOp(), toU32(elem_id), toU32(event_id), toU32(@intFromEnum(payload_accessor)), 0, 0);
    }

    pub fn clearEvent(_: WasmSink, elem_id: u64, kind: RenderEventKind) void {
        appendCommand(.clear_event, toU32(elem_id), toU32(@intFromEnum(kind)), 0, 0, 0);
    }

    pub fn startInterval(_: WasmSink, token: u64, period_ms: u64) void {
        appendCommand(.start_interval, toU32(token), toU32(period_ms), 0, 0, 0);
    }

    pub fn cancelInterval(_: WasmSink, token: u64) void {
        appendCommand(.cancel_interval, toU32(token), 0, 0, 0, 0);
    }

    pub fn startTask(_: WasmSink, request_id: u64, task_name: []const u8, request: []const u8) void {
        const name_offset = storeBytes(task_name);
        const request_offset = storeBytes(request);
        appendCommand(.start_task, toU32(request_id), name_offset, toU32(task_name.len), request_offset, toU32(request.len));
    }

    pub fn cancelTask(_: WasmSink, request_id: u64) void {
        appendCommand(.cancel_task, toU32(request_id), 0, 0, 0, 0);
    }

    pub fn debugAssertNode(_: WasmSink, _: u64, _: bool, _: ?[]const u8, _: ?u64, _: []const u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64) void {}
};

fn emitAppendChildren(parent_elem_id: u64, next_child_ids: []const u64) void {
    for (next_child_ids) |child_id| {
        appendCommand(.append_child, toU32(parent_elem_id), toU32(child_id), 0, 0, 0);
    }
}

var shared_engine: SharedEngine = .init();
var command_buffer: render.Buffer = .{};
var string_buffer: std.ArrayListUnmanaged(u8) = .empty;

const RocAllocation = struct {
    user_ptr: [*]u8,
    requested_size: usize,
    allocated_size: usize,
    alignment: std.mem.Alignment,
};

const FreedRocAllocation = struct {
    user_ptr_addr: usize,
    requested_size: usize,
    allocated_size: usize,
    alignment: std.mem.Alignment,
    phase: u32,
};

const NearestRocAllocation = struct {
    user_ptr_addr: usize,
    allocated_size: usize,
    distance: usize,
};

const recent_freed_roc_allocation_capacity = 4096;

var roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty;
var recent_freed_roc_allocations: [recent_freed_roc_allocation_capacity]FreedRocAllocation = undefined;
var recent_freed_roc_allocation_len: usize = 0;
var recent_freed_roc_allocation_next: usize = 0;
var roc_allocation_phase: u32 = 0;
var active_capabilities: hv.ActiveCapabilityStack = .{};
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

var last_host_error: []const u8 = "";
var last_host_error_buf: [512]u8 = undefined;

fn clearHostError() void {
    last_host_error = "";
}

fn failHostWith(message: []const u8) noreturn {
    last_host_error = message;
    @trap();
}

fn failHostWithFmt(comptime fmt: []const u8, args: anytype) noreturn {
    last_host_error = std.fmt.bufPrint(&last_host_error_buf, fmt, args) catch "Signals wasm host invariant failed while formatting diagnostic";
    @trap();
}

fn failHost() noreturn {
    failHostWith("Signals wasm host invariant failed");
}

fn allocator() std.mem.Allocator {
    return std.heap.wasm_allocator;
}

fn toU32(value: anytype) u32 {
    return std.math.cast(u32, value) orelse failHost();
}

fn alignmentFromBytes(alignment: usize) std.mem.Alignment {
    if (alignment == 0 or !std.math.isPowerOfTwo(alignment)) failHost();
    return @enumFromInt(std.math.log2_int(usize, alignment));
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

const callErasedHostValueToHostValue = erased_calls.callErasedHostValueToHostValue;
const callErasedHostValueHostValueToHostValue = erased_calls.callErasedHostValueHostValueToHostValue;
const callErasedHostValueToUnit = erased_calls.callErasedHostValueToUnit;

fn callHostValueToUnitWithCapability(cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) void {
    const caps = [_]HostValueCapability{cap};
    active_capabilities.push(&caps);
    defer active_capabilities.pop();
    callErasedHostValueToUnit(&roc_host, callable, value);
}

fn callHostValueToHostValueWithCapability(cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) HostValue {
    const caps = [_]HostValueCapability{cap};
    active_capabilities.push(&caps);
    defer active_capabilities.pop();
    return callErasedHostValueToHostValue(&roc_host, callable, value);
}

fn callHostValueHostValueToHostValueWithCapabilities(left_cap: HostValueCapability, right_cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) HostValue {
    const caps = [_]HostValueCapability{ left_cap, right_cap };
    active_capabilities.push(&caps);
    defer active_capabilities.pop();
    return callErasedHostValueHostValueToHostValue(&roc_host, callable, left, right);
}

// --- Host value registry glue (all routed through the engine's registry) ---

fn registryOps() hv.RegistryOps() {
    return .{
        .roc_host = &roc_host,
        .active_capabilities = &active_capabilities,
        .debug_phase = &roc_allocation_phase,
    };
}

fn failHostValueRegistryError(err: host_value_registry.Error) noreturn {
    switch (err) {
        error.InvalidHandle => failHostWith("HostValue handle referenced an unknown value"),
        error.ReleasedHandle => failHostWith("HostValue handle referenced a released value"),
        error.UnconsumedHandle => failHostWith("HostValue consuming callback returned without taking the transferred value"),
        error.MissingCapability => failHostWith("HostValue operation crossed erasure boundary without an owning capability"),
        error.CapabilityMismatch => failHostWith("HostValue operation used a capability that does not own the retained value"),
        error.ConflictingCapability => failHostWith("HostValue was assigned a conflicting capability"),
        error.InactiveCapability => failHostWith("HostValue split operation ran without an active owning capability"),
        error.CloneCapabilityMismatch => failHostWith("HostValue capability clone returned a value owned by a different capability"),
        error.CloneReturnedSource => failHostWith("HostValue capability clone returned the source handle"),
        error.OutOfMemory => failHostWith("HostValue registry allocation failed"),
    }
}

fn cloneHostValue(value: HostValue) HostValue {
    return shared_engine.host_values.clone(allocator(), value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

fn setHostValueCapability(value: HostValue, cap: HostValueCapability) void {
    shared_engine.host_values.setCapability(value, cap, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

fn assertHostValueCapability(value: HostValue, cap: HostValueCapability) void {
    shared_engine.host_values.assertCapability(value, cap, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

fn hostValueTakeEpoch() u64 {
    return shared_engine.host_values.takeEpoch();
}

fn assertHostValueTakenAfter(value: HostValue, epoch: u64) void {
    shared_engine.host_values.assertTakenAfter(value, epoch) catch |err| {
        failHostValueRegistryError(err);
    };
}

// `ctx` surface consumed by the shared `host_values` box constructors. The
// browser host has no test-kind bookkeeping, so `recordKind` is a no-op.
const HostValueOpsCtx = struct {
    pub fn store(_: HostValueOpsCtx, box: abi.RocBox) HostValue {
        return shared_engine.host_values.storeOwnedCapability(allocator(), box, null, registryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    pub fn storeWithCapability(_: HostValueOpsCtx, box: abi.RocBox, cap: HostValueCapability) HostValue {
        return shared_engine.host_values.storeRetainedCapability(allocator(), box, cap, registryOps()) catch |err| {
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

// --- State access (routed through the engine's state table) ---

fn currentStateValue(node_id: u64) HostValue {
    const state_index = shared_engine.stateIndexByNodeId(node_id) orelse failHost();
    return cloneHostValue(shared_engine.states.items[state_index].cell.value);
}

fn updateStateValue(node_id: u64, value: HostValue) bool {
    const state_index = shared_engine.stateIndexByNodeId(node_id) orelse failHost();
    const state = &shared_engine.states.items[state_index];
    const ctx = WasmCtx{};
    if (state.cell.valueEquals(ctx, &roc_host, value)) {
        state.cell.dropIncoming(ctx, &roc_host, value);
        return false;
    }
    state.cell.replaceValue(ctx, &roc_host, value);
    state.version += 1;
    return true;
}

// --- Engine-driven init / dispatch / teardown ---

fn dropMovedElemPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

/// Collect the root `Elem` into a fresh descriptor stream, apply it against the
/// active stream (first render creates everything; later renders diff), rebuild
/// the active event table, and swap the new stream in. Mirrors the native host's
/// `renderActiveRootMeasured`.
fn renderActiveRoot(dirty_source_node_ids: []const u64) void {
    const ctx = WasmCtx{};
    const root = shared_engine.root_elem orelse failHost();

    var next_stream: HostNodeDescriptorStream = .{};
    shared_engine.collectActiveElemRootDescriptors(ctx, &roc_host, &next_stream, root, dirty_source_node_ids);

    if (!shared_engine.hasRenderRoot()) {
        _ = shared_engine.applyNodeDescriptorStream(ctx, &roc_host, &next_stream);
    } else {
        _ = shared_engine.applyStructuralNodeDescriptorStream(ctx, &roc_host, &next_stream);
    }

    shared_engine.rebuildActiveEventsFromStream(ctx, &next_stream);
    shared_engine.active_stream.deinit(allocator(), ctx, &roc_host, &shared_engine.pending_roc_metrics);
    shared_engine.active_stream = next_stream;
    const mount_counts = shared_engine.runActiveMountCommands(ctx, &roc_host);
    shared_engine.render_metrics.addCommandCounts(mount_counts);
}

fn hostEventById(event_id: u32) HostActiveEventDesc {
    if (event_id == 0 or event_id > shared_engine.active_events.items.len) failHost();
    return shared_engine.active_events.items[event_id - 1];
}

/// Route a DOM event into its source node's retained reducer thunk, then
/// propagate in rank order and apply both scalar render sinks and any structural
/// splice the change triggers. Mirrors the native host's `dispatchRocEventMeasured`.
fn dispatchEvent(event_id: u32, payload_kind: EventPayloadKind, payload: HostValue) void {
    const ctx = WasmCtx{};
    const desc = hostEventById(event_id);
    if (desc.payload_kind != payload_kind) failHost();
    const payload_cap = desc.payload_reducer.capability;
    setHostValueCapability(payload, payload_cap);
    defer callHostValueToUnitWithCapability(payload_cap, hv.hostValueCapabilityDrop(payload_cap), payload);

    shared_engine.recordDispatch();

    const current = currentStateValue(desc.target_node_id);
    const state_cap = shared_engine.stateCapability(desc.target_node_id) catch failHost();
    defer callHostValueToUnitWithCapability(state_cap, hv.hostValueCapabilityDrop(state_cap), current);
    const next = callHostValueHostValueToHostValueWithCapabilities(state_cap, payload_cap, desc.payload_reducer.transform, current, payload);
    if (!updateStateValue(desc.target_node_id, next)) return;

    const dirty_source_node_ids = [_]u64{desc.target_node_id};
    const dirty_generation = shared_engine.nextDirtySignalGeneration();

    const changed_record_ids = shared_engine.propagateDirtyActiveSignals(ctx, &roc_host, allocator(), &dirty_source_node_ids, dirty_generation);
    defer allocator().free(changed_record_ids);

    const dirty_structural_signals = shared_engine.collectDirtyStructuralSignals(ctx, &roc_host, allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer allocator().free(dirty_structural_signals);

    _ = shared_engine.applyDirtyRenderSinks(ctx, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
    if (dirty_structural_signals.len != 0) {
        _ = shared_engine.applyDirtyStructuralSignalsLocally(ctx, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);
    }
}

fn resolveTask(request_id: u64, payload_text: []const u8, failed: bool) void {
    const previous_phase = roc_allocation_phase;
    defer roc_allocation_phase = previous_phase;
    const ctx = WasmCtx{};
    const pending_index = shared_engine.pendingTaskIndexByRequestId(request_id) orelse failHostWith("task result had no matching pending request");
    var pending = shared_engine.removePendingTaskAt(pending_index);
    defer shared_engine.deinitPendingTask(ctx, &pending);

    const record = shared_engine.activeTaskRecordByToken(pending.task_token) orelse failHostWith("task result matched no active task source");
    const task_payload = switch (record.payload) {
        .task_source => |payload| payload,
        .ref, .const_value, .map, .map2, .combine, .interval_source => unreachable,
    };
    if (task_payload.token != pending.task_token) failHostWith("task result matched a pending request for a different task source");

    roc_allocation_phase = 10;
    const payload = hostValueStr(payload_text);
    setHostValueCapability(payload, task_payload.payload_cap);
    const payload_take_epoch = hostValueTakeEpoch();

    roc_allocation_phase = 20;
    const next = if (failed)
        callHostValueToHostValueWithCapability(task_payload.payload_cap, task_payload.failed, payload)
    else
        callHostValueToHostValueWithCapability(task_payload.payload_cap, task_payload.done, payload);
    assertHostValueTakenAfter(payload, payload_take_epoch);
    roc_allocation_phase = 30;
    _ = shared_engine.dispatchEffectSourceValue(ctx, &roc_host, record, next);
}

fn tickInterval(token: u64) void {
    const ctx = WasmCtx{};
    _ = shared_engine.tickIntervalSourceByRuntimeToken(ctx, &roc_host, token);
}

/// Tear the engine's reactive runtime back down to a re-mountable empty state.
/// Runs before each mount and on unmount; the order mirrors the engine portion of
/// the native host's `HostEnv.deinit`.
fn clearActiveRuntime() void {
    const a = allocator();
    const ctx = WasmCtx{};
    shared_engine.roc_host = &roc_host;

    shared_engine.clearActiveSignalRoutes(ctx);
    shared_engine.active_source_signal_routes.deinit(a);
    shared_engine.active_text_signal_routes.deinit(a);
    shared_engine.active_bool_signal_routes.deinit(a);
    shared_engine.active_change_signal_routes.deinit(a);
    shared_engine.active_structural_signal_routes.deinit(a);
    shared_engine.active_source_signal_routes = .empty;
    shared_engine.active_text_signal_routes = .empty;
    shared_engine.active_bool_signal_routes = .empty;
    shared_engine.active_change_signal_routes = .empty;
    shared_engine.active_structural_signal_routes = .empty;

    shared_engine.clearActiveSignalGraph(ctx);
    shared_engine.active_signal_graph.deinit(a);
    shared_engine.active_signal_graph = .empty;

    shared_engine.active_stream.deinit(a, ctx, &roc_host, &shared_engine.pending_roc_metrics);
    shared_engine.active_stream = .{};

    shared_engine.clearActiveEvents() catch failHost();
    shared_engine.active_events.deinit(a);
    shared_engine.active_events = .empty;

    shared_engine.clearPendingTasks(ctx);
    shared_engine.pending_tasks.deinit(a);
    shared_engine.pending_tasks = .empty;

    shared_engine.clearActiveIntervals(ctx);
    shared_engine.active_intervals.deinit(a);
    shared_engine.active_intervals = .empty;

    for (shared_engine.cleanup_events.items) |name| {
        a.free(name);
    }
    shared_engine.cleanup_events.deinit(a);
    shared_engine.cleanup_events = .empty;

    if (shared_engine.root_elem) |root| {
        abi.decrefElem(root, &roc_host);
        shared_engine.root_elem = null;
    }

    shared_engine.clearStates(WasmCtx{}) catch failHost();
    shared_engine.states.deinit(a);
    shared_engine.states = .empty;
    shared_engine.state_indexes_by_node_id.deinit(a);
    shared_engine.state_indexes_by_node_id = .empty;

    shared_engine.clearScopes(WasmCtx{}) catch failHost();
    shared_engine.scopes.deinit(a);
    shared_engine.scopes = .empty;

    shared_engine.node_identities.deinit(a);
    shared_engine.node_identities = .empty;
    shared_engine.dom_identities.deinit(a);
    shared_engine.dom_identities = .empty;

    shared_engine.deinitRenderCache(ctx);

    if (shared_engine.host_values.hasLiveValues()) failHost();
    shared_engine.host_values.deinit(a);
    shared_engine.host_values = .{};
}

// --- Compiler-rt shim ---

// The Roc app's `key.hash` path (`Ui.each`) emits a 128-bit integer multiply.
// ReleaseSmall leaves it as an undefined `__multi3` symbol instead of bundling
// compiler-rt, so the app object imports `env.__multi3`. The host is linked into
// every app wasm, so defining it here resolves that reference at link time and
// keeps the final module self-contained (no `env` imports) — the JS runtime can
// keep instantiating with no import object.
//
// ABI matches compiler-rt's sret form `void __multi3(i128 *ret, i128 a, i128 b)`,
// which wasm32 lowers to `(i32, i64, i64, i64, i64) -> ()` with each i128 split
// little-endian into (low, high).
//
// The body computes the low 128 bits of a*b using only 64-bit limb arithmetic. A
// `u128 *% u128` here would lower straight back to a `__multi3` call and recurse
// forever, so the 64x64->128 product of the low words is done with the classic
// 32-bit-limb schoolbook multiply, and the two cross terms (a_low*b_high,
// a_high*b_low) contribute only their low 64 bits — the a_high*b_high term lands
// entirely above bit 128 and drops.
export fn __multi3(result: *align(8) u128, a_low: u64, a_high: u64, b_low: u64, b_high: u64) callconv(.c) void {
    const mask: u64 = 0xffff_ffff;
    const al = a_low & mask;
    const ah = a_low >> 32;
    const bl = b_low & mask;
    const bh = b_low >> 32;

    var t: u64 = al *% bl;
    const w0 = t & mask;
    var k: u64 = t >> 32;

    t = ah *% bl +% k;
    const w1 = t & mask;
    const w2 = t >> 32;

    t = al *% bh +% w1;
    k = t >> 32;

    const lo = (t << 32) +% w0;
    const hi = ah *% bh +% w2 +% k +% (a_low *% b_high) +% (a_high *% b_low);

    result.* = (@as(u128, hi) << 64) | @as(u128, lo);
}

// --- Allocation marshalling (roc_alloc and friends) ---

fn allocatedSizeForRocRequest(length: usize) usize {
    return if (length == 0) 1 else length;
}

fn findRocAllocationIndex(ptr: *anyopaque) ?usize {
    const ptr_addr = @intFromPtr(ptr);
    for (roc_allocations.items, 0..) |alloc, index| {
        const user_addr = @intFromPtr(alloc.user_ptr);
        const end_addr = user_addr + alloc.allocated_size;
        if (ptr_addr >= user_addr and ptr_addr < end_addr) return index;
    }
    return null;
}

fn findExactRocAllocationIndex(ptr: *anyopaque) ?usize {
    const ptr_addr = @intFromPtr(ptr);
    for (roc_allocations.items, 0..) |alloc, index| {
        if (ptr_addr == @intFromPtr(alloc.user_ptr)) return index;
    }
    return null;
}

fn removeRocAllocationAt(index: usize) RocAllocation {
    if (index >= roc_allocations.items.len) failHostWith("Roc allocation ledger index is out of bounds");
    return roc_allocations.swapRemove(index);
}

fn recordFreedRocAllocation(alloc: RocAllocation) void {
    recent_freed_roc_allocations[recent_freed_roc_allocation_next] = .{
        .user_ptr_addr = @intFromPtr(alloc.user_ptr),
        .requested_size = alloc.requested_size,
        .allocated_size = alloc.allocated_size,
        .alignment = alloc.alignment,
        .phase = roc_allocation_phase,
    };
    recent_freed_roc_allocation_next = (recent_freed_roc_allocation_next + 1) % recent_freed_roc_allocation_capacity;
    recent_freed_roc_allocation_len = @min(recent_freed_roc_allocation_len + 1, recent_freed_roc_allocation_capacity);
}

fn findRecentlyFreedRocAllocation(ptr: *anyopaque) ?FreedRocAllocation {
    const ptr_addr = @intFromPtr(ptr);
    for (recent_freed_roc_allocations[0..recent_freed_roc_allocation_len]) |alloc| {
        if (alloc.user_ptr_addr == ptr_addr) return alloc;
    }
    return null;
}

fn distanceBetweenAddresses(left: usize, right: usize) usize {
    return if (left >= right) left - right else right - left;
}

fn nearestLiveRocAllocation(ptr: *anyopaque) ?NearestRocAllocation {
    const ptr_addr = @intFromPtr(ptr);
    var nearest: ?NearestRocAllocation = null;
    for (roc_allocations.items) |alloc| {
        const user_ptr_addr = @intFromPtr(alloc.user_ptr);
        const distance = distanceBetweenAddresses(ptr_addr, user_ptr_addr);
        if (nearest == null or distance < nearest.?.distance) {
            nearest = .{
                .user_ptr_addr = user_ptr_addr,
                .allocated_size = alloc.allocated_size,
                .distance = distance,
            };
        }
    }
    return nearest;
}

fn nearestRecentlyFreedRocAllocation(ptr: *anyopaque) ?NearestRocAllocation {
    const ptr_addr = @intFromPtr(ptr);
    var nearest: ?NearestRocAllocation = null;
    for (recent_freed_roc_allocations[0..recent_freed_roc_allocation_len]) |alloc| {
        const distance = distanceBetweenAddresses(ptr_addr, alloc.user_ptr_addr);
        if (nearest == null or distance < nearest.?.distance) {
            nearest = .{
                .user_ptr_addr = alloc.user_ptr_addr,
                .allocated_size = alloc.allocated_size,
                .distance = distance,
            };
        }
    }
    return nearest;
}

fn failUnknownRocAllocation(comptime op: []const u8, ptr: *anyopaque, alignment_arg: usize) noreturn {
    const nearest_live = nearestLiveRocAllocation(ptr);
    const nearest_freed = nearestRecentlyFreedRocAllocation(ptr);
    if (nearest_live) |live| {
        if (nearest_freed) |freed| {
            failHostWithFmt(
                "{s} unknown ptr=0x{x} align={} live={} nearest_live=0x{x}/{} dist={} recent_freed={} nearest_freed=0x{x}/{} dist={}",
                .{ op, @intFromPtr(ptr), alignment_arg, roc_allocations.items.len, live.user_ptr_addr, live.allocated_size, live.distance, recent_freed_roc_allocation_len, freed.user_ptr_addr, freed.allocated_size, freed.distance },
            );
        }
        failHostWithFmt(
            "{s} unknown ptr=0x{x} align={} live={} nearest_live=0x{x}/{} dist={} recent_freed=0",
            .{ op, @intFromPtr(ptr), alignment_arg, roc_allocations.items.len, live.user_ptr_addr, live.allocated_size, live.distance },
        );
    }
    if (nearest_freed) |freed| {
        failHostWithFmt(
            "{s} unknown ptr=0x{x} align={} live=0 recent_freed={} nearest_freed=0x{x}/{} dist={}",
            .{ op, @intFromPtr(ptr), alignment_arg, recent_freed_roc_allocation_len, freed.user_ptr_addr, freed.allocated_size, freed.distance },
        );
    }
    failHostWithFmt(
        "{s} unknown ptr=0x{x} align={} live=0 recent_freed=0",
        .{ op, @intFromPtr(ptr), alignment_arg },
    );
}

fn recordRocAllocation(user_ptr: [*]u8, requested_size: usize, allocated_size: usize, alignment: std.mem.Alignment) bool {
    roc_allocations.append(allocator(), .{
        .user_ptr = user_ptr,
        .requested_size = requested_size,
        .allocated_size = allocated_size,
        .alignment = alignment,
    }) catch return false;
    return true;
}

fn allocRocMemory(length: usize, alignment_arg: usize) ?*anyopaque {
    const min_alignment = @max(alignment_arg, @sizeOf(usize));
    const alignment = alignmentFromBytes(min_alignment);
    const allocated_size = allocatedSizeForRocRequest(length);
    const user_ptr = std.heap.wasm_allocator.rawAlloc(allocated_size, alignment, @returnAddress()) orelse return null;
    if (!recordRocAllocation(user_ptr, length, allocated_size, alignment)) {
        std.heap.wasm_allocator.rawFree(user_ptr[0..allocated_size], alignment, @returnAddress());
        return null;
    }
    return @ptrCast(user_ptr);
}

fn freeRocAllocation(ptr: *anyopaque, alignment_arg: usize) RocAllocation {
    const min_alignment = @max(alignment_arg, @sizeOf(usize));
    const alignment = alignmentFromBytes(min_alignment);
    const index = findExactRocAllocationIndex(ptr) orelse {
        if (findRocAllocationIndex(ptr)) |interior_index| {
            const alloc = roc_allocations.items[interior_index];
            const base = @intFromPtr(alloc.user_ptr);
            const ptr_addr = @intFromPtr(ptr);
            failHostWithFmt("roc_dealloc received an interior pointer ptr=0x{x} align={} base=0x{x} offset={} requested_size={} allocated_size={} tracked_align={} current_phase={}", .{ ptr_addr, alignment_arg, base, ptr_addr - base, alloc.requested_size, alloc.allocated_size, alloc.alignment.toByteUnits(), roc_allocation_phase });
        }
        if (findRecentlyFreedRocAllocation(ptr)) |freed| {
            if (freed.alignment != alignment) {
                failHostWithFmt("roc_dealloc received an already freed pointer ptr=0x{x} align={} with tracked_align={} requested_size={} allocated_size={} freed_phase={} current_phase={}", .{ @intFromPtr(ptr), alignment_arg, freed.alignment.toByteUnits(), freed.requested_size, freed.allocated_size, freed.phase, roc_allocation_phase });
            }
            failHostWithFmt("roc_dealloc received a pointer that was already freed ptr=0x{x} align={} requested_size={} allocated_size={} freed_phase={} current_phase={}", .{ @intFromPtr(ptr), alignment_arg, freed.requested_size, freed.allocated_size, freed.phase, roc_allocation_phase });
        }
        failUnknownRocAllocation("roc_dealloc", ptr, alignment_arg);
    };
    if (roc_allocations.items[index].alignment != alignment) {
        failHostWithFmt("roc_dealloc alignment did not match the tracked allocation ptr=0x{x} align={} tracked_align={}", .{ @intFromPtr(ptr), alignment_arg, roc_allocations.items[index].alignment.toByteUnits() });
    }
    const alloc = removeRocAllocationAt(index);
    recordFreedRocAllocation(alloc);
    std.heap.wasm_allocator.rawFree(alloc.user_ptr[0..alloc.allocated_size], alloc.alignment, @returnAddress());
    return alloc;
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return allocRocMemory(length, alignment);
}

export fn roc_dealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    _ = freeRocAllocation(ptr, alignment);
}

export fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment_arg: usize) callconv(.c) ?*anyopaque {
    const old_index = findExactRocAllocationIndex(ptr) orelse {
        if (findRocAllocationIndex(ptr)) |interior_index| {
            const alloc = roc_allocations.items[interior_index];
            const base = @intFromPtr(alloc.user_ptr);
            const ptr_addr = @intFromPtr(ptr);
            failHostWithFmt("roc_realloc received an interior pointer ptr=0x{x} align={} base=0x{x} offset={} requested_size={} allocated_size={} tracked_align={} current_phase={}", .{ ptr_addr, alignment_arg, base, ptr_addr - base, alloc.requested_size, alloc.allocated_size, alloc.alignment.toByteUnits(), roc_allocation_phase });
        }
        if (findRecentlyFreedRocAllocation(ptr)) |freed| {
            const min_alignment = @max(alignment_arg, @sizeOf(usize));
            const alignment = alignmentFromBytes(min_alignment);
            if (freed.alignment != alignment) {
                failHostWithFmt("roc_realloc received an already freed pointer ptr=0x{x} align={} with tracked_align={}", .{ @intFromPtr(ptr), alignment_arg, freed.alignment.toByteUnits() });
            }
            failHostWithFmt("roc_realloc received a pointer that was already freed ptr=0x{x} align={}", .{ @intFromPtr(ptr), alignment_arg });
        }
        failUnknownRocAllocation("roc_realloc", ptr, alignment_arg);
    };
    const old_alloc = roc_allocations.items[old_index];
    const min_alignment = @max(alignment_arg, @sizeOf(usize));
    const alignment = alignmentFromBytes(min_alignment);
    if (old_alloc.alignment != alignment) {
        failHostWithFmt("roc_realloc alignment did not match the tracked allocation ptr=0x{x} align={} tracked_align={}", .{ @intFromPtr(ptr), alignment_arg, old_alloc.alignment.toByteUnits() });
    }

    const new_allocation_ptr = allocRocMemory(new_length, alignment_arg) orelse return null;
    const new_allocation_user_ptr: [*]u8 = @ptrCast(new_allocation_ptr);
    const old_user_ptr: [*]const u8 = @ptrCast(ptr);
    const copy_size = @min(old_alloc.requested_size, new_length);
    @memcpy(new_allocation_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);
    const freed = removeRocAllocationAt(old_index);
    recordFreedRocAllocation(freed);
    std.heap.wasm_allocator.rawFree(freed.user_ptr[0..freed.allocated_size], freed.alignment, @returnAddress());
    return @ptrCast(new_allocation_user_ptr);
}

// --- Command-buffer wire surface (drained by the JS executor) ---

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

export fn roc_ui_last_error_ptr() callconv(.c) usize {
    return @intFromPtr(last_host_error.ptr);
}

export fn roc_ui_last_error_len() callconv(.c) usize {
    return last_host_error.len;
}

/// Number of retained host values currently live in the registry.
///
/// The browser leak guard asserts this returns to zero after `roc_ui_unmount`,
/// proving the host drops every retained closure/value it stored while mounted.
export fn roc_ui_live_host_values() callconv(.c) usize {
    return shared_engine.host_values.liveCount();
}

export fn roc_ui_mount() callconv(.c) void {
    clearHostError();
    clearActiveRuntime();
    clearCommandBuffers();

    const root_box: ElemBox = abi.roc_ui_init();
    shared_engine.root_elem = root_box.*;
    abi.decrefBoxWith(@ptrCast(root_box), @alignOf(abi.Elem), &dropMovedElemPayload, &roc_host);

    renderActiveRoot(&.{});
}

export fn roc_ui_event(event_id: u32, payload_kind_id: u32, payload_ptr: usize, payload_len: usize, bool_value: u32) callconv(.c) void {
    clearHostError();
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

export fn roc_ui_timer(token: u32) callconv(.c) void {
    clearHostError();
    clearCommandBuffers();
    tickInterval(token);
}

export fn roc_ui_resolve(request_id: u32, payload_ptr: usize, payload_len: usize, failed: u32) callconv(.c) void {
    clearHostError();
    clearCommandBuffers();
    resolveTask(
        request_id,
        (@as([*]const u8, @ptrFromInt(payload_ptr)))[0..payload_len],
        failed != 0,
    );
}

export fn roc_ui_unmount() callconv(.c) void {
    clearHostError();
    clearCommandBuffers();
    clearActiveRuntime();
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

export fn roc_host_value_clone(value: HostValue) callconv(.c) HostValue {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 101;
    defer roc_allocation_phase = previous_phase;
    return shared_engine.host_values.clone(std.heap.wasm_allocator, value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_get_with_capability(value: HostValue, cap: HostValueCapability) callconv(.c) abi.RocBox {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 108;
    defer roc_allocation_phase = previous_phase;
    defer hv.releaseHostValueCapability(cap, &roc_host);
    return shared_engine.host_values.getWithCapability(std.heap.wasm_allocator, value, cap, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_get_with_split(value: HostValue, split: abi.RocErasedCallable) callconv(.c) abi.RocBox {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 103;
    defer roc_allocation_phase = previous_phase;
    defer abi.decrefErasedCallable(split, &roc_host);
    return shared_engine.host_values.getWithSplit(value, split, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_store_with_capability(box: abi.RocBox, cap: HostValueCapability) callconv(.c) HostValue {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 109;
    defer roc_allocation_phase = previous_phase;
    return shared_engine.host_values.storeOwnedCapability(std.heap.wasm_allocator, box, cap, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_store_with_existing_capability(box: abi.RocBox, source_value: HostValue) callconv(.c) HostValue {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 109;
    defer roc_allocation_phase = previous_phase;
    return shared_engine.host_values.storeRetainedExistingCapability(std.heap.wasm_allocator, box, source_value, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_take_with_capability(value: HostValue, cap: HostValueCapability) callconv(.c) abi.RocBox {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 110;
    defer roc_allocation_phase = previous_phase;
    defer hv.releaseHostValueCapability(cap, &roc_host);
    return shared_engine.host_values.takeWithCapability(value, cap, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}

export fn roc_host_value_take_with_split(value: HostValue, split: abi.RocErasedCallable) callconv(.c) abi.RocBox {
    const previous_phase = roc_allocation_phase;
    roc_allocation_phase = 107;
    defer roc_allocation_phase = previous_phase;
    defer abi.decrefErasedCallable(split, &roc_host);
    return shared_engine.host_values.takeWithSplit(value, split, registryOps()) catch |err| {
        failHostValueRegistryError(err);
    };
}
