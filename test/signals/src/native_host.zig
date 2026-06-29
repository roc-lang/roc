//! Platform host for testing signal-based reactive UI applications.
//!
//! The native host is the spec/telemetry/debug boundary: it owns the simulated
//! DOM, Roc allocation diagnostics, benchmark metrics, and browser-style event
//! replay. Reactive and structural behavior lives in `engine.zig`; this host
//! supplies `NativeCtx` plus a render sink and drives the shared engine path.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const signals = @import("signals");
const abi = signals.abi;
const render = signals.render;
const render_sink = signals.render_sink;
const scope_tree = signals.scope_tree;
const erased_calls = signals.erased_calls;
const hv = signals.host_values;
const engine = signals.engine;
const spec_parser = @import("spec/spec_parser.zig");
const spec_runner = @import("spec/spec_runner.zig");
const benchmark = @import("bench/benchmark.zig");
const sim_dom = @import("sim_dom.zig");
const roc_alloc_ledger = @import("roc_alloc_ledger.zig");
const crash_handlers = @import("crash_handlers.zig");

const enable_runtime_metrics = builtin.is_test or build_options.metrics;

const ElemBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const RocStr = abi.RocStr;
const HostValue = u64;
const HostValueCapability = hv.HostValueCapabilityHandle;
const HostTextRead = engine.HostTextRead;
const HostBoolRead = engine.HostBoolRead;
const HostEachOps = engine.HostEachOps;
const HostValueList = abi.RocListWith(HostValue, false);
const I64List = abi.RocListWith(i64, false);
const U8List = hv.U8List;
const RenderTextField = render.TextField;
const RenderBoolField = render.BoolField;
const RenderEventKind = render.EventKind;
const CommandCounts = render.Counts;
const HostScopeBranch = scope_tree.Branch;

const NativeCtx = struct {
    pub const Handle = *HostEnv;
    pub const RegistryOps = hv.RegistryOps();
    pub const Metrics = RuntimeMetrics;
    pub const Sink = render_sink.DomSink(HostEnv);

    pub fn zeroMetrics() Metrics {
        return zeroRuntimeMetrics();
    }

    pub fn allocator(ctx: Handle) std.mem.Allocator {
        return ctx.hostAllocator();
    }

    pub fn cloneHostValue(ctx: Handle, value: HostValue) HostValue {
        return ctx.cloneHostValue(value);
    }

    pub fn debugPhase(ctx: Handle, phase: u32) void {
        ctx.debug_phase = phase;
    }

    pub fn pushHostValueCapabilities(ctx: Handle, caps: []const HostValueCapability) void {
        ctx.active_capabilities.push(caps);
    }

    pub fn popHostValueCapabilities(ctx: Handle) void {
        ctx.active_capabilities.pop();
    }

    pub fn stateValueByNodeId(ctx: Handle, node_id: u64) HostValue {
        return ctx.stateValueByNodeId(node_id);
    }

    pub fn stateCapability(ctx: Handle, node_id: u64) HostValueCapability {
        return ctx.stateCapability(node_id);
    }

    pub fn sink(ctx: Handle) Sink {
        return ctx.sink();
    }
};

const HostEngine = engine.Engine(NativeCtx);
const RuntimeMetrics = engine.RuntimeMetrics;
const NoMetrics = engine.NoMetrics;
const addRuntimeMetrics = engine.addRuntimeMetrics;

comptime {
    std.debug.assert(@sizeOf(NoMetrics) == 0);
}

test "NoMetrics is a zero-size no-op metrics sink" {
    try std.testing.expectEqual(@as(usize, 0), @sizeOf(NoMetrics));
    var metrics: NoMetrics = .{};
    metrics.bump(.closure_retains, 2);
    metrics.bump(.nodes_recomputed, 1);
}

const EventPayloadKind = engine.EventPayloadKind;
const EventPayloadAccessor = engine.EventPayloadAccessor;
const SignalKind = engine.SignalKind;
const HostActiveEventDesc = engine.HostActiveEventDesc;
const HostPendingTask = engine.HostPendingTask;

const HostSignalCacheSlot = engine.HostSignalCacheSlot;

const HostNodeScopeSiteKind = engine.HostNodeScopeSiteKind;
const HostBinderToken = engine.HostBinderToken;
const HostSignalToken = engine.HostSignalToken;
const HostBinderBinding = engine.HostBinderBinding;

const HostSignalRecordPayload = engine.HostSignalRecordPayload;
const NodeFieldCustom = engine.NodeFieldCustom;

const HostSignalRecord = engine.HostSignalRecord;

const HostSignalBinding = engine.HostSignalBinding;

const HostNodeDescriptorStream = engine.HostNodeDescriptorStream;

const HostActiveStructuralSignalKind = engine.HostActiveStructuralSignalKind;
const HostDirtyStructuralSignal = engine.HostDirtyStructuralSignal;
const HostKeyedRowDiffResult = engine.HostKeyedRowDiffResult;

pub const std_options = crash_handlers.std_options;
pub const panic = crash_handlers.panic;

fn writeStderr(bytes: []const u8) void {
    crash_handlers.writeStderr(bytes);
}

const DomElement = sim_dom.Element;
const DomNamedEvent = sim_dom.NamedEvent;

const SpecCommandType = spec_parser.SpecCommandType;
const Locator = spec_parser.Locator;
const SpecCommand = spec_parser.SpecCommand;
const ParseError = spec_parser.ParseError;
const parseTestSpecFile = spec_parser.parseTestSpecFile;
const freeSpecCommands = spec_parser.freeSpecCommands;
const BenchmarkStats = benchmark.Stats;

const TestState = struct {
    verbose: bool,
    commands: []SpecCommand,

    fn init() TestState {
        return .{
            .verbose = false,
            .commands = &.{},
        };
    }
};

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

fn failScopeTreeError(err: anyerror, unknown_scope_message: []const u8) noreturn {
    switch (err) {
        error.OutOfMemory => std.process.exit(1),
        error.UnknownScope => failHost(unknown_scope_message),
        error.InactiveScope => failHost("scope id references a disposed host scope"),
        error.InvalidRoot => failHost("host root scope descriptor is not indexed by scope id"),
        else => failHost("scope tree operation failed"),
    }
}

fn failIdentityTableError(err: anyerror) noreturn {
    switch (err) {
        error.OutOfMemory => std.process.exit(1),
        else => failHost("identity table operation failed"),
    }
}

fn failScopeOrIdentityTableError(err: anyerror, unknown_scope_message: []const u8) noreturn {
    switch (err) {
        error.UnknownScope, error.InactiveScope, error.InvalidRoot => failScopeTreeError(err, unknown_scope_message),
        error.OutOfMemory => std.process.exit(1),
        else => failIdentityTableError(err),
    }
}

fn failSignalLookupError(err: HostEngine.SignalLookupError) noreturn {
    switch (err) {
        error.MissingSignalRoute => failHost("state id has no host signal route descriptor"),
        error.SignalRouteIndexMismatch => failHost("host signal route table is not indexed by state id"),
        error.MissingSignalDependentRoute => failHost("signal id has no host dependent route descriptor"),
        error.SignalDependentRouteIndexMismatch => failHost("host signal dependent route table is not indexed by signal id"),
        error.MissingSignalDescriptor => failHost("signal id has no host signal descriptor"),
        error.SignalDescriptorIndexMismatch => failHost("host signal descriptor table is not indexed by signal id"),
    }
}

fn failRocHostRequiredError(err: HostEngine.RocHostRequiredError, message: []const u8) noreturn {
    switch (err) {
        error.MissingRocHost => failHost(message),
    }
}

fn failHostValueRegistryError(err: anyerror) noreturn {
    switch (err) {
        error.OutOfMemory => std.process.exit(1),
        error.InvalidHandle => failHost("HostValue handle referenced an unknown value"),
        error.ReleasedHandle => failHost("HostValue handle referenced a released value"),
        error.UnconsumedHandle => failHost("HostValue consuming callback returned without taking the transferred value"),
        error.MissingCapability => failHost("HostValue operation crossed erasure boundary without an owning capability"),
        error.CapabilityMismatch => failHost("HostValue operation used a capability that does not own the retained value"),
        error.ConflictingCapability => failHost("HostValue was assigned a conflicting capability"),
        error.InactiveCapability => failHost("HostValue split operation ran without an active owning capability"),
        error.CloneCapabilityMismatch => failHost("HostValue capability clone returned a value owned by a different capability"),
        error.CloneReturnedSource => failHost("HostValue capability clone returned the source handle"),
        else => failHost("HostValue registry operation failed"),
    }
}

const TestHostValueKind = enum {
    unit,
    i64,
    bool,
    str,
    i64_list,
    u8_list,
};

const HostAllocator = struct {
    const vtable: std.mem.Allocator.VTable = .{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };

    fn host(ptr: *anyopaque) *HostEnv {
        return @ptrCast(@alignCast(ptr));
    }

    fn alloc(ptr: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const env = host(ptr);
        const memory = env.backingAllocator().rawAlloc(len, alignment, ret_addr) orelse return null;
        env.recordHostAlloc(len);
        return memory;
    }

    fn resize(ptr: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const env = host(ptr);
        if (!env.backingAllocator().rawResize(memory, alignment, new_len, ret_addr)) return false;
        env.recordHostResize(memory.len, new_len);
        return true;
    }

    fn remap(ptr: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const env = host(ptr);
        const result = env.backingAllocator().rawRemap(memory, alignment, new_len, ret_addr) orelse return null;
        env.recordHostResize(memory.len, new_len);
        return result;
    }

    fn free(ptr: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const env = host(ptr);
        env.recordHostFree(memory.len);
        env.backingAllocator().rawFree(memory, alignment, ret_addr);
    }
};

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    engine: HostEngine = .{},
    test_state: TestState,
    roc_allocations: roc_alloc_ledger.Ledger = .{},
    test_host_value_kinds: std.ArrayListUnmanaged(?TestHostValueKind) = .empty,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    host_alloc_count: u64 = 0,
    host_dealloc_count: u64 = 0,
    host_alloc_bytes: u64 = 0,
    host_dealloc_bytes: u64 = 0,
    debug_phase: u32 = 0,
    active_capabilities: hv.ActiveCapabilityStack = .{},
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,

    fn init() HostEnv {
        return .{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
    }

    fn backingAllocator(self: *HostEnv) std.mem.Allocator {
        return self.gpa.allocator();
    }

    inline fn hostAllocator(self: *HostEnv) std.mem.Allocator {
        if (comptime !enable_runtime_metrics) return self.backingAllocator();
        return .{ .ptr = self, .vtable = &HostAllocator.vtable };
    }

    inline fn hostMetricBytes(bytes: usize) u64 {
        return std.math.cast(u64, bytes) orelse failHost("host allocation byte count exceeded metric range");
    }

    inline fn recordHostAlloc(self: *HostEnv, bytes: usize) void {
        if (comptime !enable_runtime_metrics) return;
        const metric_bytes = HostEnv.hostMetricBytes(bytes);
        self.host_alloc_count += 1;
        self.host_alloc_bytes += metric_bytes;
        self.engine.pending_roc_metrics.bump(.host_allocs_this_event, 1);
        self.engine.pending_roc_metrics.bump(.host_alloc_bytes_this_event, metric_bytes);
    }

    inline fn recordHostFree(self: *HostEnv, bytes: usize) void {
        if (comptime !enable_runtime_metrics) return;
        const metric_bytes = HostEnv.hostMetricBytes(bytes);
        self.host_dealloc_count += 1;
        self.host_dealloc_bytes += metric_bytes;
        self.engine.pending_roc_metrics.bump(.host_deallocs_this_event, 1);
        self.engine.pending_roc_metrics.bump(.host_dealloc_bytes_this_event, metric_bytes);
    }

    inline fn recordHostResize(self: *HostEnv, old_len: usize, new_len: usize) void {
        if (comptime !enable_runtime_metrics) return;
        if (new_len > old_len) {
            const delta = HostEnv.hostMetricBytes(new_len - old_len);
            self.host_alloc_bytes += delta;
            self.engine.pending_roc_metrics.bump(.host_alloc_bytes_this_event, delta);
        } else if (old_len > new_len) {
            const delta = HostEnv.hostMetricBytes(old_len - new_len);
            self.host_dealloc_bytes += delta;
            self.engine.pending_roc_metrics.bump(.host_dealloc_bytes_this_event, delta);
        }
    }

    inline fn recordRocAllocMetric(self: *HostEnv) void {
        if (comptime !enable_runtime_metrics) return;
        self.alloc_count += 1;
        self.engine.pending_roc_metrics.bump(.allocs_this_event, 1);
    }

    inline fn recordRocFreeMetric(self: *HostEnv) void {
        if (comptime !enable_runtime_metrics) return;
        self.dealloc_count += 1;
        self.engine.pending_roc_metrics.bump(.deallocs_this_event, 1);
    }

    pub fn sink(self: *HostEnv) render_sink.DomSink(HostEnv) {
        return .{ .host = self };
    }

    pub fn sinkReset(self: *HostEnv) void {
        resetSimulatedDom(self);
    }

    pub fn sinkAppendNode(self: *HostEnv, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
        appendDomNode(self, elem_id, parent_elem_id, tag);
    }

    pub fn sinkEnsureNode(self: *HostEnv, elem_id: u64, tag: []const u8) void {
        ensureDomNode(self, elem_id, tag);
    }

    pub fn sinkRemoveNode(self: *HostEnv, elem_id: u64) void {
        removeDomNode(self, elem_id);
    }

    pub fn sinkReplaceChildren(self: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64) void {
        replaceDomChildrenForStructuralParent(self, parent_elem_id, next_child_ids);
    }

    pub fn sinkReplaceChildrenForMoves(self: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64) void {
        replaceDomChildrenForStructuralParentMoves(self, parent_elem_id, next_child_ids);
    }

    pub fn sinkApplyTextField(self: *HostEnv, elem_id: u64, field: RenderTextField, value: []const u8) void {
        setRenderTextField(self, elem_id, field, value);
    }

    pub fn sinkApplyTextAttr(self: *HostEnv, elem_id: u64, name: []const u8, value: []const u8) void {
        setRenderTextAttr(self, elem_id, name, value);
    }

    pub fn sinkApplyBoolField(self: *HostEnv, elem_id: u64, field: RenderBoolField, value: bool) void {
        setRenderBoolField(self, elem_id, field, value);
    }

    pub fn sinkClearTextField(self: *HostEnv, elem_id: u64, field: RenderTextField) void {
        clearRenderTextField(self, elem_id, field);
    }

    pub fn sinkClearTextAttr(self: *HostEnv, elem_id: u64, name: []const u8) void {
        clearRenderTextAttr(self, elem_id, name);
    }

    pub fn sinkClearBoolField(self: *HostEnv, elem_id: u64, field: RenderBoolField) void {
        clearRenderBoolField(self, elem_id, field);
    }

    pub fn sinkBindEventKind(self: *HostEnv, elem_id: u64, kind: RenderEventKind, event_id: u64, _: EventPayloadAccessor) void {
        bindNodeEventKind(self, elem_id, kind, event_id);
    }

    pub fn sinkClearEvent(self: *HostEnv, elem_id: u64, kind: RenderEventKind) void {
        clearNodeEventKind(self, elem_id, kind);
    }

    pub fn sinkBindEventName(self: *HostEnv, elem_id: u64, name: []const u8, event_id: u64, options: u32, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor) void {
        bindNodeEventName(self, elem_id, name, event_id, options, payload_kind, payload_accessor);
    }

    pub fn sinkClearEventName(self: *HostEnv, elem_id: u64, name: []const u8) void {
        clearNodeEventName(self, elem_id, name);
    }

    pub fn sinkStartInterval(_: *HostEnv, _: u64, _: u64) void {}

    pub fn sinkCancelInterval(_: *HostEnv, _: u64) void {}

    pub fn sinkStartTask(_: *HostEnv, _: u64, _: []const u8, _: []const u8) void {}

    pub fn sinkCancelTask(_: *HostEnv, _: u64) void {}

    pub fn sinkDebugAssertNode(self: *HostEnv, elem_id: u64, active: bool, tag: ?[]const u8, parent_id: ?u64, children: []const u64, click_event: ?u64, input_event: ?u64, check_event: ?u64, pointer_down_event: ?u64, pointer_up_event: ?u64, pointer_enter_event: ?u64, pointer_leave_event: ?u64) void {
        if (elem_id >= self.dom_elements.items.len) {
            if (!active) return;
            failHost("render cache active node was missing from simulated DOM");
        }

        const elem = &self.dom_elements.items[@intCast(elem_id)];
        if (elem.active != active) failHost("render cache active flag disagreed with simulated DOM");
        if (!active) return;

        const expected_tag = tag orelse failHost("active render cache node had no tag");
        if (!std.mem.eql(u8, elem.tag, expected_tag)) failHost("render cache tag disagreed with simulated DOM");
        if (elem.parent_id != parent_id) failHost("render cache parent disagreed with simulated DOM");
        if (!std.mem.eql(u64, elem.children.items, children)) failHost("render cache child order disagreed with simulated DOM");
        if (elem.bound_click_event != click_event) failHost("render cache click binding disagreed with simulated DOM");
        if (elem.bound_input_event != input_event) failHost("render cache input binding disagreed with simulated DOM");
        if (elem.bound_check_event != check_event) failHost("render cache check binding disagreed with simulated DOM");
        if (elem.bound_pointer_down_event != pointer_down_event) failHost("render cache pointer-down binding disagreed with simulated DOM");
        if (elem.bound_pointer_up_event != pointer_up_event) failHost("render cache pointer-up binding disagreed with simulated DOM");
        if (elem.bound_pointer_enter_event != pointer_enter_event) failHost("render cache pointer-enter binding disagreed with simulated DOM");
        if (elem.bound_pointer_leave_event != pointer_leave_event) failHost("render cache pointer-leave binding disagreed with simulated DOM");
    }

    fn activeRocHost(self: *HostEnv) *abi.RocHost {
        return self.engine.roc_host orelse failHost("HostValue capability release requires an active Roc host");
    }

    fn releaseOwnedHostValueCapability(self: *HostEnv, cap: HostValueCapability) void {
        hv.releaseHostValueCapability(cap, self.activeRocHost());
    }

    fn hostValueRegistryOps(self: *HostEnv) hv.RegistryOps() {
        return .{
            .roc_host = self.activeRocHost(),
            .active_capabilities = &self.active_capabilities,
            .debug_phase = &self.debug_phase,
        };
    }

    pub fn pushHostValueCapabilities(self: *HostEnv, caps: []const HostValueCapability) void {
        self.active_capabilities.push(caps);
    }

    pub fn popHostValueCapabilities(self: *HostEnv) void {
        self.active_capabilities.pop();
    }

    // `ctx` surface consumed by the shared `host_values` box constructors
    // (`pub` so the `host_values` module can call them through `anytype`).
    pub fn store(self: *HostEnv, box: abi.RocBox) HostValue {
        return self.storeHostValue(box);
    }

    pub fn storeWithCapability(self: *HostEnv, box: abi.RocBox, cap: HostValueCapability) HostValue {
        return self.storeHostValueWithRetainedCapability(box, cap);
    }

    pub fn recordKind(self: *HostEnv, value: HostValue, kind: hv.ValueKind) void {
        if (builtin.is_test) self.setTestHostValueKind(value, switch (kind) {
            .unit => .unit,
            .str => .str,
            .bool => .bool,
            .i64 => .i64,
            .u8_list => .u8_list,
        });
    }

    fn resetTestHostValueKind(self: *HostEnv, value: HostValue) void {
        if (builtin.is_test) {
            const allocator = self.hostAllocator();
            const index = value - 1;
            if (index >= self.test_host_value_kinds.items.len) {
                self.test_host_value_kinds.append(allocator, null) catch std.process.exit(1);
            } else {
                self.test_host_value_kinds.items[@intCast(index)] = null;
            }
        }
    }

    fn storeHostValueWithOwnedCapability(self: *HostEnv, box: abi.RocBox, owned_cap: HostValueCapability) HostValue {
        const allocator = self.hostAllocator();
        const previous_phase = self.debug_phase;
        self.debug_phase = 109;
        defer self.debug_phase = previous_phase;
        const value = self.engine.host_values.storeOwnedCapability(allocator, box, owned_cap, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(value);
        return value;
    }

    fn storeHostValueWithRetainedCapability(self: *HostEnv, box: abi.RocBox, borrowed_cap: HostValueCapability) HostValue {
        const allocator = self.hostAllocator();
        const value = self.engine.host_values.storeRetainedCapability(allocator, box, borrowed_cap, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(value);
        return value;
    }

    fn storeHostValueWithExistingCapability(self: *HostEnv, box: abi.RocBox, source_value: HostValue) HostValue {
        const allocator = self.hostAllocator();
        const previous_phase = self.debug_phase;
        self.debug_phase = 109;
        defer self.debug_phase = previous_phase;
        const value = self.engine.host_values.storeRetainedExistingCapability(allocator, box, source_value, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(value);
        if (builtin.is_test) {
            self.setTestHostValueKind(value, self.testHostValueKind(source_value));
        }
        return value;
    }

    fn storeHostValue(self: *HostEnv, box: abi.RocBox) HostValue {
        const allocator = self.hostAllocator();
        const value = self.engine.host_values.storeOwnedCapability(allocator, box, null, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(value);
        return value;
    }

    fn setTestHostValueKind(self: *HostEnv, value: HostValue, kind: TestHostValueKind) void {
        if (!builtin.is_test) @compileError("setTestHostValueKind is test-only");
        const index = value - 1;
        if (index >= self.test_host_value_kinds.items.len) failHost("test HostValue kind referenced an unknown value");
        self.test_host_value_kinds.items[@intCast(index)] = kind;
    }

    fn testHostValueKind(self: *HostEnv, value: HostValue) TestHostValueKind {
        if (!builtin.is_test) @compileError("testHostValueKind is test-only");
        const index = value - 1;
        if (index >= self.test_host_value_kinds.items.len) failHost("test HostValue kind referenced an unknown value");
        return self.test_host_value_kinds.items[@intCast(index)] orelse @panic("test HostValue kind was not recorded");
    }

    fn setHostValueCapability(self: *HostEnv, value: HostValue, borrowed_cap: HostValueCapability) void {
        self.engine.host_values.setCapability(value, borrowed_cap, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn getHostValue(self: *HostEnv, value: HostValue) abi.RocBox {
        const previous_phase = self.debug_phase;
        self.debug_phase = 108;
        defer self.debug_phase = previous_phase;
        return self.engine.host_values.get(self.hostAllocator(), value, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn getHostValueWithCapability(self: *HostEnv, value: HostValue, owned_cap: HostValueCapability) abi.RocBox {
        const previous_phase = self.debug_phase;
        self.debug_phase = 108;
        defer self.debug_phase = previous_phase;
        defer self.releaseOwnedHostValueCapability(owned_cap);
        return self.engine.host_values.getWithCapability(self.hostAllocator(), value, owned_cap, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn getHostValueWithSplit(self: *HostEnv, value: HostValue, owned_split: abi.RocErasedCallable) abi.RocBox {
        const previous_phase = self.debug_phase;
        self.debug_phase = 103;
        defer self.debug_phase = previous_phase;
        defer abi.decrefErasedCallable(owned_split, self.activeRocHost());
        return self.engine.host_values.getWithSplit(value, owned_split, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn takeHostValue(self: *HostEnv, value: HostValue) abi.RocBox {
        const box = self.engine.host_values.take(value, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        if (builtin.is_test) self.test_host_value_kinds.items[@intCast(value - 1)] = null;
        return box;
    }

    fn takeHostValueWithCapability(self: *HostEnv, value: HostValue, owned_cap: HostValueCapability) abi.RocBox {
        const previous_phase = self.debug_phase;
        self.debug_phase = 110;
        defer self.debug_phase = previous_phase;
        defer self.releaseOwnedHostValueCapability(owned_cap);
        return self.engine.host_values.takeWithCapability(value, owned_cap, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn takeHostValueWithSplit(self: *HostEnv, value: HostValue, owned_split: abi.RocErasedCallable) abi.RocBox {
        const previous_phase = self.debug_phase;
        self.debug_phase = 107;
        defer self.debug_phase = previous_phase;
        defer abi.decrefErasedCallable(owned_split, self.activeRocHost());
        const box = self.engine.host_values.takeWithSplit(value, owned_split, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        if (builtin.is_test) self.test_host_value_kinds.items[@intCast(value - 1)] = null;
        return box;
    }

    fn hostValueTakeEpoch(self: *const HostEnv) u64 {
        return self.engine.host_values.takeEpoch();
    }

    fn assertHostValueTakenAfter(self: *HostEnv, value: HostValue, epoch: u64) void {
        self.engine.host_values.assertTakenAfter(value, epoch) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    // `pub` so the shared `engine.HostValueCell.cloneRetained` can clone a value
    // through the host's registry (the engine treats `self` as its clone ctx).
    pub fn cloneHostValue(self: *HostEnv, value: HostValue) HostValue {
        const allocator = self.hostAllocator();
        const previous_phase = self.debug_phase;
        self.debug_phase = 101;
        defer self.debug_phase = previous_phase;
        const cloned = self.engine.host_values.clone(allocator, value, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(cloned);
        if (builtin.is_test) {
            self.setTestHostValueKind(cloned, self.testHostValueKind(value));
        }
        return cloned;
    }

    fn nextDirtySignalGeneration(self: *HostEnv) u64 {
        return self.engine.nextDirtySignalGeneration();
    }

    fn clearEventDescriptors(self: *HostEnv) void {
        self.engine.clearEventDescriptors();
    }

    fn clearActiveEvents(self: *HostEnv) void {
        self.engine.clearActiveEvents() catch |err| {
            failRocHostRequiredError(err, "active event table cannot release retained payloads without a Roc host");
        };
    }

    fn rebuildActiveEventsFromStream(self: *HostEnv, stream: *HostNodeDescriptorStream) void {
        return self.engine.rebuildActiveEventsFromStream(self, stream);
    }

    fn clearSignalEventRoutes(self: *HostEnv) void {
        const allocator = self.hostAllocator();
        for (self.engine.signal_event_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.engine.signal_event_routes.items.len = 0;
    }

    fn rebuildSignalEventRoutesFromSignals(self: *HostEnv) void {
        const allocator = self.hostAllocator();
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), self.engine.event_descriptors.items.len) catch std.process.exit(1);
        defer allocator.free(route_lists);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.engine.signal_descriptors.items) |signal| {
            if (signal.kind != .source) continue;
            for (signal.source_event_ids) |event_id| {
                if (event_id == 0 or event_id > self.engine.event_descriptors.items.len) {
                    failHost("host source signal registry contains an unknown event id");
                }
                route_lists[@intCast(event_id - 1)].append(allocator, signal.signal_id) catch std.process.exit(1);
            }
        }

        self.clearSignalEventRoutes();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.engine.signal_event_routes.append(allocator, .{
                .event_id = @intCast(index + 1),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn clearSignalDescriptors(self: *HostEnv) void {
        const allocator = self.hostAllocator();
        for (self.engine.signal_descriptors.items) |descriptor| {
            allocator.free(descriptor.source_state_ids);
            allocator.free(descriptor.source_event_ids);
            allocator.free(descriptor.input_signal_ids);
        }
        self.engine.signal_descriptors.items.len = 0;
    }

    fn clearSignalRoutes(self: *HostEnv) void {
        const allocator = self.hostAllocator();
        for (self.engine.signal_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.engine.signal_routes.items.len = 0;
    }

    fn clearSignalDependents(self: *HostEnv) void {
        const allocator = self.hostAllocator();
        for (self.engine.signal_dependents.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.engine.signal_dependents.items.len = 0;
    }

    fn clearSignalCache(self: *HostEnv) void {
        self.engine.clearSignalCache(self) catch |err| {
            failRocHostRequiredError(err, "signal cache cannot release values without a Roc host");
        };
    }

    fn clearActiveSignalGraph(self: *HostEnv) void {
        return self.engine.clearActiveSignalGraph(self);
    }

    fn clearActiveSignalRoutes(self: *HostEnv) void {
        return self.engine.clearActiveSignalRoutes(self);
    }

    fn requireActiveSignalRecordId(self: *HostEnv, record: *const HostSignalRecord) u64 {
        return self.engine.requireActiveSignalRecordId(record);
    }

    fn rebuildSignalTopologyFromSignals(self: *HostEnv) void {
        const allocator = self.hostAllocator();
        const signal_count = self.engine.signal_descriptors.items.len;
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), signal_count) catch std.process.exit(1);
        defer allocator.free(route_lists);
        const ranks = allocator.alloc(u64, signal_count) catch std.process.exit(1);
        defer allocator.free(ranks);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        @memset(ranks, 0);

        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.engine.signal_descriptors.items, 0..) |*signal, index| {
            if (signal.signal_id != index) {
                failHost("host signal registry is not indexed by signal id");
            }

            for (signal.input_signal_ids) |input_signal_id| {
                if (input_signal_id >= index) {
                    failHost("host signal topology must reference prior signal ids");
                }

                const input_index: usize = @intCast(input_signal_id);
                const next_rank = ranks[input_index] + 1;
                if (next_rank > ranks[index]) {
                    ranks[index] = next_rank;
                }
                if (!u64SliceContains(route_lists[input_index].items, signal.signal_id)) {
                    route_lists[input_index].append(allocator, signal.signal_id) catch std.process.exit(1);
                }
            }

            signal.rank = ranks[index];
        }

        self.clearSignalDependents();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.engine.signal_dependents.append(allocator, .{
                .signal_id = @intCast(index),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn signalRank(self: *HostEnv, signal_id: u64) u64 {
        return self.engine.signalRank(signal_id) catch |err| {
            failSignalLookupError(err);
        };
    }

    fn dirtySignalIdsForEvent(self: *HostEnv, allocator: std.mem.Allocator, event_id: u64) []u64 {
        return self.engine.dirtySignalIdsForEvent(allocator, event_id);
    }

    fn activeSignalRank(self: *HostEnv, record_id: u64) u64 {
        return self.engine.activeSignalRank(record_id);
    }

    fn dependentActiveSignalRecordIds(self: *HostEnv, record_id: u64) []const u64 {
        return self.engine.dependentActiveSignalRecordIds(record_id);
    }

    fn recordDispatch(self: *HostEnv) void {
        self.engine.recordDispatch();
    }

    fn deinitPendingTask(self: *HostEnv, task: *HostPendingTask) void {
        return self.engine.deinitPendingTask(self, task);
    }

    fn clearPendingTasks(self: *HostEnv) void {
        self.engine.clearPendingTasks(self);
    }

    fn clearStates(self: *HostEnv) void {
        self.engine.clearStates(self) catch |err| {
            failRocHostRequiredError(err, "state table cannot release values without a Roc host");
        };
    }

    pub fn stateValueByNodeId(self: *HostEnv, node_id: u64) HostValue {
        const state_index = self.engine.stateIndexByNodeId(node_id) orelse failHost("signal referenced an unknown active state node");
        return self.cloneHostValue(self.engine.states.items[state_index].cell.value);
    }

    fn updateStateValue(self: *HostEnv, roc_host: *abi.RocHost, node_id: u64, value: HostValue) bool {
        const state_index = self.engine.stateIndexByNodeId(node_id) orelse failHost("event referenced an unknown active state node");
        const state = &self.engine.states.items[state_index];
        if (state.cell.valueEquals(self, roc_host, value)) {
            state.cell.dropIncoming(self, roc_host, value);
            return false;
        }

        state.cell.replaceValue(self, roc_host, value);
        state.version += 1;
        return true;
    }

    pub fn stateCapability(self: *HostEnv, node_id: u64) HostValueCapability {
        return self.engine.stateCapability(node_id) catch |err| switch (err) {
            error.MissingActiveState => failHost("active state has no capability"),
        };
    }

    fn validateScopeId(self: *HostEnv, scope_id: u64) void {
        self.engine.validateScopeId(scope_id) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn internRootScope(self: *HostEnv) u64 {
        const result = self.engine.internRootScope(self.hostAllocator()) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
        return result.scope_id;
    }

    fn internComponentScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64) u64 {
        const result = self.engine.internComponentScope(self.hostAllocator(), parent_scope_id, site_ordinal) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
        return result.scope_id;
    }

    fn internWhenBranchScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) u64 {
        const result = self.engine.internWhenBranchScope(self.hostAllocator(), parent_scope_id, site_ordinal, branch) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
        return result.scope_id;
    }

    fn createEachRowScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, key_hash: u64, key: HostValue, item: HostValue, key_cap: HostValueCapability, item_cap: HostValueCapability) u64 {
        return self.engine.createEachRowScope(self, parent_scope_id, site_ordinal, key_hash, key, item, key_cap, item_cap);
    }

    fn internNodeIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        return self.engine.internNodeIdentity(self.hostAllocator(), scope_id, ordinal) catch |err| {
            failScopeOrIdentityTableError(err, "scope id has no host scope descriptor");
        };
    }

    fn internDomIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        return self.engine.internDomIdentity(self.hostAllocator(), scope_id, ordinal) catch |err| {
            failScopeOrIdentityTableError(err, "scope id has no host scope descriptor");
        };
    }

    fn disposeScopeSubtree(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64) void {
        self.engine.disposeScopeSubtree(self, roc_host, scope_id);
    }

    fn eachRowScopeValues(self: *HostEnv, scope_id: u64) engine.EachRowValues {
        return self.engine.eachRowScopeValues(scope_id);
    }

    fn syncEachRowScopes(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, ops: HostEachOps) HostKeyedRowDiffResult {
        return self.engine.syncEachRowScopes(self, roc_host, parent_scope_id, site_ordinal, keys, items, ops);
    }

    fn bindNodeSignal(self: *HostEnv, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) HostSignalBinding {
        return self.engine.bindNodeSignal(allocator, stream, expr, binder_stack);
    }

    fn activeWhenBranchScopeId(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) ?u64 {
        return self.engine.activeWhenBranchScopeId(parent_scope_id, site_ordinal, branch) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn collectActiveElemRootDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem, dirty_source_node_ids: []const u64) void {
        return self.engine.collectActiveElemRootDescriptors(self, roc_host, stream, root, dirty_source_node_ids);
    }

    fn clearScopes(self: *HostEnv) void {
        self.engine.clearScopes(self) catch |err| {
            failRocHostRequiredError(err, "host scope table cannot release keys without a Roc host");
        };
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.hostAllocator();

        self.clearActiveSignalRoutes();
        self.engine.active_source_signal_routes.deinit(allocator);
        self.engine.active_text_signal_routes.deinit(allocator);
        self.engine.active_bool_signal_routes.deinit(allocator);
        self.engine.active_change_signal_routes.deinit(allocator);
        self.engine.active_structural_signal_routes.deinit(allocator);
        self.engine.clearActiveIntervals(self);
        self.engine.active_intervals.deinit(allocator);
        self.clearActiveSignalGraph();
        self.engine.active_signal_graph.deinit(allocator);
        self.engine.active_stream.deinit(allocator, self, self.engine.roc_host.?, &self.engine.pending_roc_metrics);
        self.clearActiveEvents();
        self.engine.active_events.deinit(allocator);

        self.clearPendingTasks();
        self.engine.pending_tasks.deinit(allocator);

        engine.deinitCleanupEvents(allocator, &self.engine.cleanup_events);

        if (self.engine.root_elem) |root| {
            abi.decrefElem(root, self.engine.roc_host.?);
            self.engine.root_elem = null;
        }

        for (self.dom_elements.items) |*elem| {
            elem.deinit(allocator);
        }
        self.dom_elements.deinit(allocator);
        self.clearEventDescriptors();
        self.engine.event_descriptors.deinit(allocator);
        self.clearSignalEventRoutes();
        self.engine.signal_event_routes.deinit(allocator);
        self.clearSignalDescriptors();
        self.engine.signal_descriptors.deinit(allocator);
        self.clearSignalRoutes();
        self.engine.signal_routes.deinit(allocator);
        self.clearSignalDependents();
        self.engine.signal_dependents.deinit(allocator);
        self.clearSignalCache();
        self.engine.signal_cache.deinit(allocator);
        self.clearStates();
        self.engine.states.deinit(allocator);
        self.engine.state_indexes_by_node_id.deinit(allocator);
        self.clearScopes();
        self.engine.scopes.deinit(allocator);
        self.engine.node_identities.deinit(allocator);
        self.engine.dom_identities.deinit(allocator);
        self.engine.deinitRenderCache(self);
        self.engine.deinitScratch(self);

        freeSpecCommands(allocator, self.test_state.commands);

        if (self.engine.host_values.hasLiveValues()) failHost("host value registry still owned a typed cell at shutdown");
        self.engine.host_values.deinit(allocator);
        self.test_host_value_kinds.deinit(allocator);

        const roc_allocator = self.backingAllocator();
        for (self.roc_allocations.allocations.items) |alloc| {
            self.recordHostFree(alloc.allocated_size);
            roc_allocator.rawFree(alloc.user_ptr[0..alloc.allocated_size], alloc.alignment, @returnAddress());
        }
        self.roc_allocations.deinit(allocator);
    }

    fn findElementByLocator(self: *HostEnv, locator: Locator, line_num: usize) ?*DomElement {
        var found: ?*DomElement = null;
        var match_count: usize = 0;
        for (self.dom_elements.items) |*elem| {
            if (!elem.active) continue;
            if (!sim_dom.matchesLocator(elem, locator)) continue;
            match_count += 1;
            if (found == null) found = elem;
        }

        if (match_count > 1) {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: locator matched {d} elements\n", .{ line_num, match_count }) catch "TEST FAILED: ambiguous locator\n";
            writeStderr(msg);
            if (self.test_state.verbose) self.dumpDom();
            return null;
        }
        if (found == null and self.test_state.verbose) self.dumpDom();
        return found;
    }

    fn countElementsByLocator(self: *HostEnv, locator: Locator) usize {
        var match_count: usize = 0;
        for (self.dom_elements.items) |*elem| {
            if (!elem.active) continue;
            if (!sim_dom.matchesLocator(elem, locator)) continue;
            match_count += 1;
        }
        return match_count;
    }

    fn dumpDom(self: *HostEnv) void {
        for (self.dom_elements.items, 0..) |elem, idx| {
            var dbg_buf: [512]u8 = undefined;
            const dbg_msg = std.fmt.bufPrint(&dbg_buf, "[DEBUG] elem[{d}] tag=\"{s}\" text=\"{s}\" value=\"{s}\" parent={?d} children={d} active={} updates={d}\n", .{
                idx,
                elem.tag,
                elem.text orelse "(null)",
                elem.value orelse "(null)",
                elem.parent_id,
                elem.children.items.len,
                elem.active,
                elem.text_update_count + elem.value_update_count + elem.checked_update_count + elem.disabled_update_count,
            }) catch "";
            writeStderr(dbg_msg);
        }
    }
};

fn zeroRuntimeMetrics() RuntimeMetrics {
    return engine.zeroRuntimeMetrics();
}

var current_host: ?*HostEnv = null;
var current_roc_host: ?*abi.RocHost = null;

fn hostFromRocHost(roc_host: *abi.RocHost) *HostEnv {
    return @ptrCast(@alignCast(roc_host.env));
}

fn currentRocHost() *abi.RocHost {
    return current_roc_host orelse @panic("signals RocHost is not initialized");
}

fn findExactRocAllocationIndex(host: *HostEnv, ptr: *anyopaque) ?usize {
    return host.roc_allocations.findExactIndex(ptr);
}

fn findRecentlyFreedRocAllocation(host: *HostEnv, ptr: *anyopaque) ?roc_alloc_ledger.FreedAllocation {
    return host.roc_allocations.findRecentlyFreed(ptr);
}

fn failRocDeallocError(err: roc_alloc_ledger.DeallocError) noreturn {
    failHost(roc_alloc_ledger.deallocErrorMessage(err));
}

fn failRocReallocError(err: roc_alloc_ledger.ReallocError) noreturn {
    failHost(roc_alloc_ledger.reallocErrorMessage(err));
}

fn rocAllocFn(roc_host: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const result = host.roc_allocations.allocate(host.hostAllocator(), host.backingAllocator(), length, alignment, @returnAddress()) orelse return null;
    host.recordHostAlloc(result.allocated_size);
    host.recordRocAllocMetric();
    return result.ptr;
}

fn rocDeallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host = hostFromRocHost(roc_host);
    const alloc = host.roc_allocations.deallocate(host.hostAllocator(), host.backingAllocator(), ptr, alignment, @returnAddress()) catch |err| failRocDeallocError(err);
    host.recordRocFreeMetric();
    host.recordHostFree(alloc.allocated_size);
}

fn rocReallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment_arg: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const result = host.roc_allocations.reallocate(host.hostAllocator(), host.backingAllocator(), ptr, new_length, alignment_arg, @returnAddress()) catch |err| failRocReallocError(err);
    host.recordHostAlloc(result.allocated_size);
    host.recordRocAllocMetric();
    host.recordRocFreeMetric();
    host.recordHostFree(result.freed.allocated_size);
    return result.ptr;
}

fn rocDbgFn(roc_host: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = roc_host;
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

fn rocExpectFailedFn(roc_host: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = roc_host;
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

fn rocCrashedFn(roc_host: *abi.RocHost, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = roc_host;
    const message = bytes[0..len];
    writeStderr("\n\x1b[31mRoc crashed:\x1b[0m ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocAllocFn(currentRocHost(), length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    rocDeallocFn(currentRocHost(), ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocReallocFn(currentRocHost(), ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocDbgFn(currentRocHost(), bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocExpectFailedFn(currentRocHost(), bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocCrashedFn(currentRocHost(), bytes, len);
}

fn currentHost() *HostEnv {
    return current_host orelse @panic("signals HostEnv is not initialized");
}

fn hostValueClone(value: HostValue) callconv(.c) HostValue {
    return currentHost().cloneHostValue(value);
}

fn hostValueGetWithCapability(value: HostValue, cap: HostValueCapability) callconv(.c) abi.RocBox {
    return currentHost().getHostValueWithCapability(value, cap);
}

fn hostValueGetWithSplit(value: HostValue, split: abi.RocErasedCallable) callconv(.c) abi.RocBox {
    return currentHost().getHostValueWithSplit(value, split);
}

fn hostValueStoreWithCapability(box: abi.RocBox, cap: HostValueCapability) callconv(.c) HostValue {
    return currentHost().storeHostValueWithOwnedCapability(box, cap);
}

fn hostValueStoreWithExistingCapability(box: abi.RocBox, source_value: HostValue) callconv(.c) HostValue {
    return currentHost().storeHostValueWithExistingCapability(box, source_value);
}

fn hostValueTakeWithCapability(value: HostValue, cap: HostValueCapability) callconv(.c) abi.RocBox {
    return currentHost().takeHostValueWithCapability(value, cap);
}

fn hostValueTakeWithSplit(value: HostValue, split: abi.RocErasedCallable) callconv(.c) abi.RocBox {
    return currentHost().takeHostValueWithSplit(value, split);
}

fn failHost(message: []const u8) noreturn {
    writeStderr("HOST ERROR: ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn u64MetricAsI64(value: u64) i64 {
    return std.math.cast(i64, value) orelse failHost("runtime metric exceeded signed assertion range");
}

fn setElementText(host: *HostEnv, elem: *DomElement, text: []const u8) void {
    sim_dom.setText(host.hostAllocator(), elem, text);
}

fn setElementValueIfChanged(host: *HostEnv, elem: *DomElement, value: []const u8) bool {
    return sim_dom.setValueIfChanged(host.hostAllocator(), elem, value);
}

fn setElementValue(host: *HostEnv, elem: *DomElement, value: []const u8) void {
    sim_dom.setValue(host.hostAllocator(), elem, value);
}

fn clearElementText(host: *HostEnv, elem: *DomElement) void {
    sim_dom.clearText(host.hostAllocator(), elem);
}

fn clearElementValue(host: *HostEnv, elem: *DomElement) void {
    sim_dom.clearValue(host.hostAllocator(), elem);
}

fn setElementTextAttr(host: *HostEnv, elem: *DomElement, name: []const u8, value: []const u8) void {
    sim_dom.setTextAttr(host.hostAllocator(), elem, name, value);
}

fn clearElementTextAttr(host: *HostEnv, elem: *DomElement, name: []const u8) void {
    sim_dom.clearTextAttr(host.hostAllocator(), elem, name);
}

fn elementTextAttr(elem: *const DomElement, name: []const u8) ?[]const u8 {
    return sim_dom.textAttr(elem, name);
}

fn setElementCheckedIfChanged(elem: *DomElement, checked: bool) bool {
    return sim_dom.setCheckedIfChanged(elem, checked);
}

fn setElementChecked(elem: *DomElement, checked: bool) void {
    sim_dom.setChecked(elem, checked);
}

fn setElementDisabled(elem: *DomElement, disabled: bool) void {
    sim_dom.setDisabled(elem, disabled);
}

fn resetSimulatedDom(host: *HostEnv) void {
    sim_dom.reset(host.hostAllocator(), &host.dom_elements);
    host.engine.next_elem_id = 1;
}

fn domElementById(host: *HostEnv, id: u64) *DomElement {
    if (id >= host.dom_elements.items.len) failHost("DOM command referenced missing element");
    const elem = &host.dom_elements.items[@intCast(id)];
    if (!elem.active) {
        var message: [96]u8 = undefined;
        const rendered = std.fmt.bufPrint(&message, "DOM command referenced inactive element {d}", .{id}) catch "DOM command referenced inactive element";
        failHost(rendered);
    }
    return elem;
}

fn setRenderTextField(host: *HostEnv, elem_id: u64, field: RenderTextField, value: []const u8) void {
    const elem = domElementById(host, elem_id);
    switch (field) {
        .text => setElementText(host, elem, value),
        .role => sim_dom.setOwnedString(host.hostAllocator(), &elem.role, value),
        .label => sim_dom.setOwnedString(host.hostAllocator(), &elem.label, value),
        .test_id => sim_dom.setOwnedString(host.hostAllocator(), &elem.test_id, value),
        .value => setElementValue(host, elem, value),
        .class => sim_dom.setOwnedString(host.hostAllocator(), &elem.class, value),
    }
}

fn setRenderTextAttr(host: *HostEnv, elem_id: u64, name: []const u8, value: []const u8) void {
    setElementTextAttr(host, domElementById(host, elem_id), name, value);
}

fn setRenderBoolField(host: *HostEnv, elem_id: u64, field: RenderBoolField, value: bool) void {
    const elem = domElementById(host, elem_id);
    switch (field) {
        .checked => setElementChecked(elem, value),
        .disabled => setElementDisabled(elem, value),
    }
}

fn clearRenderTextField(host: *HostEnv, elem_id: u64, field: RenderTextField) void {
    const elem = domElementById(host, elem_id);
    switch (field) {
        .text => clearElementText(host, elem),
        .role => sim_dom.clearOwnedString(host.hostAllocator(), &elem.role),
        .label => sim_dom.clearOwnedString(host.hostAllocator(), &elem.label),
        .test_id => sim_dom.clearOwnedString(host.hostAllocator(), &elem.test_id),
        .value => clearElementValue(host, elem),
        .class => sim_dom.clearOwnedString(host.hostAllocator(), &elem.class),
    }
}

fn clearRenderTextAttr(host: *HostEnv, elem_id: u64, name: []const u8) void {
    clearElementTextAttr(host, domElementById(host, elem_id), name);
}

fn clearRenderBoolField(host: *HostEnv, elem_id: u64, field: RenderBoolField) void {
    const elem = domElementById(host, elem_id);
    switch (field) {
        .checked => setElementChecked(elem, false),
        .disabled => setElementDisabled(elem, false),
    }
}

fn appendDetachedDomNode(host: *HostEnv, elem_id: u64, tag: []const u8) void {
    if (elem_id != host.dom_elements.items.len) failHost("descriptor stream elements must be dense and ordered by elem id");
    sim_dom.appendDetached(host.hostAllocator(), &host.dom_elements, elem_id, tag);
    host.engine.next_elem_id = elem_id + 1;
}

fn appendDomNode(host: *HostEnv, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
    appendDetachedDomNode(host, elem_id, tag);
    const parent = domElementById(host, parent_elem_id);
    const child = domElementById(host, elem_id);
    sim_dom.appendChild(host.hostAllocator(), parent, child);
}

fn findDomChildIndex(elem: *const DomElement, child_id: u64) ?usize {
    return sim_dom.childIndex(elem, child_id);
}

fn ensureDomNode(host: *HostEnv, elem_id: u64, tag: []const u8) void {
    if (elem_id == 0) failHost("render descriptor cannot claim the host DOM root id");
    appendDetachedDomNode(host, elem_id, tag);
}

fn propagateDirtyActiveSignals(host: *HostEnv, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
    return host.engine.propagateDirtyActiveSignals(host, roc_host, allocator, dirty_source_node_ids, dirty_generation);
}

fn hostSignalRecordCapability(host: *HostEnv, record: *const HostSignalRecord) HostValueCapability {
    return host.engine.hostSignalRecordCapability(host, record);
}

fn hostSignalBindingCapability(host: *HostEnv, signal: *const HostSignalBinding) HostValueCapability {
    return hostSignalRecordCapability(host, signal.record);
}

fn updateDirtySignalCache(host: *HostEnv, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue) bool {
    const cap = testHostValueCapability(roc_host);
    defer abi.decrefHostValueCapabilityHandle(cap, roc_host);
    return host.engine.updateDirtySignalCache(host, roc_host, cache_slot, value, cap);
}

fn resolvePendingTask(host: *HostEnv, roc_host: *abi.RocHost, name: []const u8, payload_text: []const u8, failed: bool) CommandCounts {
    const pending_index = host.engine.pendingTaskIndexByName(name) orelse failHost("fake task result had no matching pending request");
    var pending = host.engine.removePendingTaskAt(pending_index);
    defer host.engine.deinitPendingTask(host, &pending);

    const record = host.engine.activeTaskRecordByName(name) orelse failHost("fake task result matched no active task source");
    const task_payload = switch (record.payload) {
        .task_source => |payload| payload,
        .ref, .const_value, .map, .map2, .combine, .interval_source => unreachable,
    };
    if (task_payload.token != pending.task_token) {
        failHost("fake task result matched a pending request for a different task source");
    }

    const payload_value = hostValueStrWithCapability(host, roc_host, payload_text, task_payload.payload_cap);
    const payload_take_epoch = host.hostValueTakeEpoch();
    const next = if (failed)
        callHostValueToHostValueWithCapability(host, roc_host, task_payload.payload_cap, task_payload.failed, payload_value)
    else
        callHostValueToHostValueWithCapability(host, roc_host, task_payload.payload_cap, task_payload.done, payload_value);
    host.assertHostValueTakenAfter(payload_value, payload_take_epoch);
    return host.engine.dispatchEffectSourceValue(host, roc_host, record, next);
}

fn tickIntervalSource(host: *HostEnv, roc_host: *abi.RocHost, period_ms: u64) CommandCounts {
    return host.engine.tickIntervalSource(host, roc_host, period_ms);
}

fn collectDirtyStructuralSignals(host: *HostEnv, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) []HostDirtyStructuralSignal {
    return host.engine.collectDirtyStructuralSignals(host, roc_host, allocator, dirty_source_node_ids, changed_record_ids, dirty_generation);
}

fn applyDirtyRenderSinks(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) CommandCounts {
    return host.engine.applyDirtyRenderSinks(host, roc_host, dirty_source_node_ids, changed_record_ids, dirty_generation);
}

fn bindNodeEventKind(host: *HostEnv, elem_id: u64, kind: RenderEventKind, event_id: u64) void {
    sim_dom.bindEventKind(domElementById(host, elem_id), kind, event_id);
}

fn clearNodeEventKind(host: *HostEnv, elem_id: u64, kind: RenderEventKind) void {
    sim_dom.clearEventKind(domElementById(host, elem_id), kind);
}

fn bindNodeEventName(host: *HostEnv, elem_id: u64, name: []const u8, event_id: u64, options: u32, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor) void {
    sim_dom.bindEventName(host.hostAllocator(), domElementById(host, elem_id), name, event_id, options, payload_kind, payload_accessor);
}

fn clearNodeEventName(host: *HostEnv, elem_id: u64, name: []const u8) void {
    sim_dom.clearEventName(host.hostAllocator(), domElementById(host, elem_id), name);
}

fn nodeEventName(elem: *const DomElement, name: []const u8) ?DomNamedEvent {
    return sim_dom.namedEvent(elem, name);
}

fn replaceDomChildrenForStructuralParentMoves(host: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64) void {
    replaceDomChildrenForStructuralParent(host, parent_elem_id, next_child_ids);
}

fn removeDomNode(host: *HostEnv, elem_id: u64) void {
    const allocator = host.hostAllocator();
    if (elem_id == 0) failHost("structural patch attempted to remove host DOM root");
    if (elem_id >= host.dom_elements.items.len) failHost("structural patch removed an element missing from DOM state");

    const elem = &host.dom_elements.items[@intCast(elem_id)];
    if (!elem.active) {
        var message: [128]u8 = undefined;
        const rendered = std.fmt.bufPrint(&message, "structural patch removed inactive DOM node {d}", .{elem_id}) catch "structural patch removed an inactive DOM node";
        failHost(rendered);
    }
    if (elem.parent_id) |parent_id| {
        if (parent_id >= host.dom_elements.items.len) failHost("structural patch removed an element with missing parent");
        const parent = &host.dom_elements.items[@intCast(parent_id)];
        if (parent.active) {
            if (findDomChildIndex(parent, elem_id)) |child_index| {
                sim_dom.removeChildAt(parent, child_index);
            }
        }
    }
    sim_dom.deactivateRemovedNode(allocator, elem);
}

fn replaceDomChildrenForStructuralParent(host: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64) void {
    const allocator = host.hostAllocator();
    if (parent_elem_id >= host.dom_elements.items.len) failHost("structural child replacement referenced missing parent");
    const parent = &host.dom_elements.items[@intCast(parent_elem_id)];
    if (!parent.active) {
        var message: [128]u8 = undefined;
        const rendered = std.fmt.bufPrint(&message, "structural child replacement referenced inactive parent {d}", .{parent_elem_id}) catch "structural child replacement referenced inactive parent";
        failHost(rendered);
    }

    for (next_child_ids) |child_id| {
        if (child_id >= host.dom_elements.items.len) failHost("structural child replacement referenced missing child");
        const child = &host.dom_elements.items[@intCast(child_id)];
        if (!child.active) {
            var message: [128]u8 = undefined;
            const rendered = std.fmt.bufPrint(&message, "structural child replacement referenced inactive child {d} under parent {d}", .{ child_id, parent_elem_id }) catch "structural child replacement referenced inactive child";
            failHost(rendered);
        }
    }

    sim_dom.replaceChildren(allocator, host.dom_elements.items, parent, next_child_ids);
}

fn applyNodeDescriptorStream(host: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) CommandCounts {
    return host.engine.applyNodeDescriptorStream(host, roc_host, stream);
}

fn applyStructuralNodeDescriptorStream(host: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) CommandCounts {
    return host.engine.applyStructuralNodeDescriptorStream(host, roc_host, stream);
}

fn dropMovedElemPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

fn hostValueUnit(host: *HostEnv, roc_host: *abi.RocHost) HostValue {
    return hv.makeUnit(host, roc_host);
}

fn hostValueStr(host: *HostEnv, roc_host: *abi.RocHost, value: []const u8) HostValue {
    return hv.makeStr(host, roc_host, value);
}

fn hostValueStrWithCapability(host: *HostEnv, roc_host: *abi.RocHost, value: []const u8, cap: HostValueCapability) HostValue {
    return hv.makeStrWithCapability(host, roc_host, value, cap);
}

fn hostValueBool(host: *HostEnv, roc_host: *abi.RocHost, value: bool) HostValue {
    return hv.makeBool(host, roc_host, value);
}

fn hostValueI64(host: *HostEnv, roc_host: *abi.RocHost, value: i64) HostValue {
    return hv.makeI64(host, roc_host, value);
}

fn hostValueU8List(host: *HostEnv, roc_host: *abi.RocHost, bytes: []const u8) HostValue {
    return hv.makeU8List(host, roc_host, bytes);
}

const ErasedHostValueUnaryArgs = erased_calls.ErasedHostValueUnaryArgs;
const ErasedHostValueBinaryArgs = erased_calls.ErasedHostValueBinaryArgs;
const ErasedRocBoxUnaryArgs = erased_calls.ErasedRocBoxUnaryArgs;

const callErasedHostValueToHostValue = erased_calls.callErasedHostValueToHostValue;

const callErasedHostValueHostValueToHostValue = erased_calls.callErasedHostValueHostValueToHostValue;

const callErasedHostValueHostValueToElem = erased_calls.callErasedHostValueHostValueToElem;

const callErasedHostValueHostValueToBool = erased_calls.callErasedHostValueHostValueToBool;

const callErasedHostValueToUnit = erased_calls.callErasedHostValueToUnit;

fn callHostValueToUnitWithCapability(host: *HostEnv, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) void {
    const caps = [_]HostValueCapability{cap};
    host.pushHostValueCapabilities(&caps);
    defer host.popHostValueCapabilities();
    callErasedHostValueToUnit(roc_host, callable, value);
}

fn callHostValueToHostValueWithCapability(host: *HostEnv, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) HostValue {
    const caps = [_]HostValueCapability{cap};
    host.pushHostValueCapabilities(&caps);
    defer host.popHostValueCapabilities();
    return callErasedHostValueToHostValue(roc_host, callable, value);
}

fn callHostValueHostValueToHostValueWithCapabilities(host: *HostEnv, roc_host: *abi.RocHost, left_cap: HostValueCapability, right_cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) HostValue {
    const caps = [_]HostValueCapability{ left_cap, right_cap };
    host.pushHostValueCapabilities(&caps);
    defer host.popHostValueCapabilities();
    return callErasedHostValueHostValueToHostValue(roc_host, callable, left, right);
}

fn hostEventById(host: *HostEnv, event_id: u64) HostActiveEventDesc {
    if (event_id == 0 or event_id > host.engine.active_events.items.len) {
        failHost("DOM event referenced an unknown active event");
    }
    return host.engine.active_events.items[@intCast(event_id - 1)];
}

fn validateEventPayloadKind(desc: HostActiveEventDesc, actual_payload_kind: EventPayloadKind) void {
    if (desc.payload_kind != actual_payload_kind) {
        failHost("DOM event payload kind does not match Roc event descriptor");
    }
}

fn finishHostMetrics(host: *HostEnv) void {
    if (comptime !enable_runtime_metrics) return;
    var metrics = addRuntimeMetrics(host.engine.last_runtime_metrics, host.engine.pending_roc_metrics);
    metrics.patches_emitted = host.engine.render_metrics.patches_emitted;
    metrics.reset_dom = host.engine.render_metrics.reset_dom;
    metrics.create_element = host.engine.render_metrics.create_element;
    metrics.append_child = host.engine.render_metrics.append_child;
    metrics.remove_node = host.engine.render_metrics.remove_node;
    metrics.move_before = host.engine.render_metrics.move_before;
    metrics.set_text = host.engine.render_metrics.set_text;
    metrics.set_value = host.engine.render_metrics.set_value;
    metrics.set_checked = host.engine.render_metrics.set_checked;
    metrics.set_disabled = host.engine.render_metrics.set_disabled;
    metrics.set_metadata = host.engine.render_metrics.set_metadata;
    metrics.bind_event = host.engine.render_metrics.bind_event;
    metrics.retained_alloc_delta = @as(i64, @intCast(host.alloc_count)) - @as(i64, @intCast(host.dealloc_count));
    metrics.host_retained_alloc_delta = u64MetricAsI64(host.host_alloc_count) - u64MetricAsI64(host.host_dealloc_count);
    metrics.host_retained_bytes_delta = u64MetricAsI64(host.host_alloc_bytes) - u64MetricAsI64(host.host_dealloc_bytes);
    metrics.events_processed = host.engine.dispatch_metrics.events_processed;
    metrics.recompute_batches = host.engine.dispatch_metrics.recompute_batches;
    host.engine.last_runtime_metrics = metrics;
    host.engine.pending_roc_metrics = zeroRuntimeMetrics();
}

fn applyDirtyStructuralSignalsLocally(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, dirty_generation: u64, changes: []const HostDirtyStructuralSignal) CommandCounts {
    return host.engine.applyDirtyStructuralSignalsLocally(host, roc_host, dirty_source_node_ids, dirty_generation, changes);
}

fn applyDirtyWhenStructuralSignals(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, dirty_generation: u64, changes: []const HostDirtyStructuralSignal) CommandCounts {
    return host.engine.applyDirtyWhenStructuralSignals(host, roc_host, dirty_source_node_ids, dirty_generation, changes);
}

fn renderActiveRootWithStats(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    const root = host.engine.root_elem orelse failHost("host render requested before Roc root Elem was initialized");

    var next_stream: HostNodeDescriptorStream = .{};
    errdefer next_stream.deinit(host.hostAllocator(), host, roc_host, &host.engine.pending_roc_metrics);
    host.collectActiveElemRootDescriptors(roc_host, &next_stream, root, dirty_source_node_ids);

    const start_ns = benchmark.nowNs();
    const counts = if (!host.engine.hasRenderRoot())
        applyNodeDescriptorStream(host, roc_host, &next_stream)
    else
        applyStructuralNodeDescriptorStream(host, roc_host, &next_stream);
    const elapsed = benchmark.nowNs() - start_ns;
    if (apply_ns) |ns| ns.* += elapsed;
    if (command_counts) |total| total.addAll(counts);

    host.rebuildActiveEventsFromStream(&next_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), host, roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = next_stream;
    const mount_counts = host.engine.runActiveMountCommands(host, roc_host);
    if (comptime enable_runtime_metrics) host.engine.render_metrics.addCommandCounts(mount_counts);
    if (command_counts) |total| total.addAll(mount_counts);
    finishHostMetrics(host);
}

fn acceptInitElemWithStats(host: *HostEnv, roc_host: *abi.RocHost, root_box: ElemBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    if (host.engine.root_elem != null) failHost("Roc root Elem initialized more than once");
    const root = root_box.*;
    host.engine.root_elem = root;
    abi.decrefBoxWith(@ptrCast(root_box), @alignOf(abi.Elem), true, &dropMovedElemPayload, roc_host);
    renderActiveRootWithStats(host, roc_host, &.{}, apply_ns, command_counts);
}

fn acceptInitElem(host: *HostEnv, roc_host: *abi.RocHost, root_box: ElemBox) void {
    acceptInitElemWithStats(host, roc_host, root_box, null, null);
}

fn dispatchRocEventWithStats(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue, stats: ?*BenchmarkStats) void {
    const desc = hostEventById(host, event_id);
    validateEventPayloadKind(desc, payload_kind);
    const payload_cap = desc.payload_reducer.capability;
    host.setHostValueCapability(payload, payload_cap);
    defer callHostValueToUnitWithCapability(host, roc_host, payload_cap, hv.hostValueCapabilityDrop(payload_cap), payload);

    host.recordDispatch();

    var metrics = host.engine.pending_roc_metrics;
    metrics.bump(.nodes_recomputed, 1);
    metrics.bump(.derived_calls_into_roc, 1);
    host.engine.pending_roc_metrics = metrics;

    const start_ns = benchmark.nowNs();
    const current = host.stateValueByNodeId(desc.target_node_id);
    const state_cap = host.stateCapability(desc.target_node_id);
    defer callHostValueToUnitWithCapability(host, roc_host, state_cap, hv.hostValueCapabilityDrop(state_cap), current);
    const next = callHostValueHostValueToHostValueWithCapabilities(host, roc_host, state_cap, payload_cap, desc.payload_reducer.transform, current, payload);
    if (stats) |s| s.dispatch_roc_ns += benchmark.nowNs() - start_ns;

    const changed = host.updateStateValue(roc_host, desc.target_node_id, next);
    if (!changed) {
        var prune_metrics = host.engine.pending_roc_metrics;
        prune_metrics.bump(.propagation_prunes, 1);
        host.engine.pending_roc_metrics = prune_metrics;
        finishHostMetrics(host);
        if (stats) |s| s.actions += 1;
        return;
    }

    if (stats) |s| {
        const dirty_source_node_ids = [_]u64{desc.target_node_id};
        const dirty_generation = host.nextDirtySignalGeneration();
        const changed_record_ids = propagateDirtyActiveSignals(host, roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
        defer host.hostAllocator().free(changed_record_ids);
        const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
        defer host.hostAllocator().free(dirty_structural_signals);
        if (dirty_structural_signals.len != 0) {
            const apply_start_ns = benchmark.nowNs();
            const sink_counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            const counts = applyDirtyStructuralSignalsLocally(host, roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);
            s.dispatch_apply_ns += benchmark.nowNs() - apply_start_ns;
            s.commands.addAll(sink_counts);
            s.commands.addAll(counts);
            finishHostMetrics(host);
        } else {
            const apply_start_ns = benchmark.nowNs();
            const counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            s.dispatch_apply_ns += benchmark.nowNs() - apply_start_ns;
            s.commands.addAll(counts);
            finishHostMetrics(host);
        }
        s.actions += 1;
    } else {
        const dirty_source_node_ids = [_]u64{desc.target_node_id};
        const dirty_generation = host.nextDirtySignalGeneration();
        const changed_record_ids = propagateDirtyActiveSignals(host, roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
        defer host.hostAllocator().free(changed_record_ids);
        const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
        defer host.hostAllocator().free(dirty_structural_signals);
        if (dirty_structural_signals.len != 0) {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            _ = applyDirtyStructuralSignalsLocally(host, roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);
            finishHostMetrics(host);
        } else {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            finishHostMetrics(host);
        }
    }
}

fn dispatchRocEvent(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue) void {
    dispatchRocEventWithStats(host, roc_host, event_id, payload_kind, payload, null);
}

fn encodeKeyShiftPayload(allocator: std.mem.Allocator, key: []const u8, shift_key: bool) []u8 {
    const bytes = allocator.alloc(u8, @sizeOf(u32) + key.len + 1) catch std.process.exit(1);
    std.mem.writeInt(u32, bytes[0..@sizeOf(u32)], @intCast(key.len), .little);
    @memcpy(bytes[@sizeOf(u32)..][0..key.len], key);
    bytes[@sizeOf(u32) + key.len] = if (shift_key) 1 else 0;
    return bytes;
}

fn requireNamedEvent(elem: *const DomElement, name: []const u8, message: []const u8) DomNamedEvent {
    return nodeEventName(elem, name) orelse failHost(message);
}

fn dispatchKeyDownWithStats(host: *HostEnv, roc_host: *abi.RocHost, elem: *const DomElement, key: []const u8, shift_key: bool, stats: ?*BenchmarkStats) void {
    const event = requireNamedEvent(elem, "keydown", "keydown target has no named keydown binding");
    if (event.payload_kind != .bytes or event.payload_accessor != .record_key_shift) {
        failHost("keydown binding does not request the key/shift payload descriptor");
    }
    const payload_bytes = encodeKeyShiftPayload(host.hostAllocator(), key, shift_key);
    defer host.hostAllocator().free(payload_bytes);
    dispatchRocEventWithStats(host, roc_host, event.event_id, .bytes, hostValueU8List(host, roc_host, payload_bytes), stats);
}

fn dispatchSubmitWithStats(host: *HostEnv, roc_host: *abi.RocHost, elem: *const DomElement, stats: ?*BenchmarkStats) void {
    const event = requireNamedEvent(elem, "submit", "submit target has no named submit binding");
    if (event.payload_kind != .unit or event.payload_accessor != .none) {
        failHost("submit binding does not use a unit payload descriptor");
    }
    if ((event.options & render.listener_option_prevent_default) == 0) {
        failHost("submit binding is missing the static prevent-default listener policy");
    }
    dispatchRocEventWithStats(host, roc_host, event.event_id, .unit, hostValueUnit(host, roc_host), stats);
}

fn makeSignalsRocHost(host: *HostEnv) abi.RocHost {
    if (builtin.is_test) current_host = host;
    return .{
        .env = @ptrCast(host),
        .roc_alloc = &rocAllocFn,
        .roc_dealloc = &rocDeallocFn,
        .roc_realloc = &rocReallocFn,
        .roc_dbg = &rocDbgFn,
        .roc_expect_failed = &rocExpectFailedFn,
        .roc_crashed = &rocCrashedFn,
    };
}

fn pointerEventIdForCommand(elem: *const DomElement, cmd_type: SpecCommandType) ?u64 {
    return switch (cmd_type) {
        .pointer_down => elem.bound_pointer_down_event,
        .pointer_up => elem.bound_pointer_up_event,
        .pointer_enter => elem.bound_pointer_enter_event,
        .pointer_leave => elem.bound_pointer_leave_event,
        else => null,
    };
}

const BenchmarkDomElement = DomElement;

fn hostValueUnitForBenchmark(host: *HostEnv, roc_host: *abi.RocHost) HostValue {
    return hostValueUnit(host, roc_host);
}

fn hostValueStrForBenchmark(host: *HostEnv, roc_host: *abi.RocHost, value: []const u8) HostValue {
    return hostValueStr(host, roc_host, value);
}

fn hostValueBoolForBenchmark(host: *HostEnv, roc_host: *abi.RocHost, value: bool) HostValue {
    return hostValueBool(host, roc_host, value);
}

fn setElementValueForBenchmark(host: *HostEnv, elem: *DomElement, value: []const u8) bool {
    return setElementValueIfChanged(host, elem, value);
}

fn setElementCheckedForBenchmark(elem: *DomElement, checked: bool) bool {
    return setElementCheckedIfChanged(elem, checked);
}

fn resolvePendingTaskForBenchmark(host: *HostEnv, roc_host: *abi.RocHost, name: []const u8, payload_text: []const u8, failed: bool) CommandCounts {
    return resolvePendingTask(host, roc_host, name, payload_text, failed);
}

fn tickIntervalSourceForBenchmark(host: *HostEnv, roc_host: *abi.RocHost, period_ms: u64) CommandCounts {
    return tickIntervalSource(host, roc_host, period_ms);
}

fn finishHostMetricsForBenchmark(host: *HostEnv) void {
    finishHostMetrics(host);
}

fn addRuntimeMetricsForBenchmark(left: RuntimeMetrics, right: RuntimeMetrics) RuntimeMetrics {
    return addRuntimeMetrics(left, right);
}

const BenchmarkCtx = struct {
    pub const Host = HostEnv;
    pub const RocHost = abi.RocHost;
    pub const DomElement = BenchmarkDomElement;

    pub fn fail(message: []const u8) noreturn {
        failHost(message);
    }

    pub fn initHost() Host {
        return Host.init();
    }

    pub fn deinitHost(host: *Host) void {
        host.deinit();
    }

    pub fn setVerbose(host: *Host, verbose: bool) void {
        host.test_state.verbose = verbose;
    }

    pub fn makeRocHost(host: *Host) RocHost {
        return makeSignalsRocHost(host);
    }

    pub fn attachRocHost(host: *Host, roc_host: *RocHost) void {
        host.engine.roc_host = roc_host;
    }

    pub fn enterCurrent(host: *Host, roc_host: *RocHost) void {
        current_host = host;
        current_roc_host = roc_host;
    }

    pub fn leaveCurrent() void {
        current_host = null;
        current_roc_host = null;
    }

    pub fn initRocUi() ElemBox {
        return abi.roc_ui_init();
    }

    pub fn acceptInitElemMeasured(host: *Host, roc_host: *RocHost, root_box: ElemBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
        acceptInitElemWithStats(host, roc_host, root_box, apply_ns, command_counts);
    }

    pub fn findElementByLocator(host: *Host, locator: Locator, line_num: usize) ?*BenchmarkDomElement {
        return host.findElementByLocator(locator, line_num);
    }

    pub fn elementDisabled(elem: *const BenchmarkDomElement) bool {
        return elem.disabled;
    }

    pub fn clickEventId(elem: *const BenchmarkDomElement) ?u64 {
        return elem.bound_click_event;
    }

    pub fn pointerEventId(elem: *const BenchmarkDomElement, cmd_type: SpecCommandType) ?u64 {
        return pointerEventIdForCommand(elem, cmd_type);
    }

    pub fn inputEventId(elem: *const BenchmarkDomElement) ?u64 {
        return elem.bound_input_event;
    }

    pub fn checkEventId(elem: *const BenchmarkDomElement) ?u64 {
        return elem.bound_check_event;
    }

    pub fn dispatchRocEventMeasured(host: *Host, roc_host: *RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue, stats: ?*BenchmarkStats) void {
        dispatchRocEventWithStats(host, roc_host, event_id, payload_kind, payload, stats);
    }

    pub fn hostValueUnit(host: *Host, roc_host: *RocHost) HostValue {
        return hostValueUnitForBenchmark(host, roc_host);
    }

    pub fn hostValueStr(host: *Host, roc_host: *RocHost, value: []const u8) HostValue {
        return hostValueStrForBenchmark(host, roc_host, value);
    }

    pub fn hostValueBool(host: *Host, roc_host: *RocHost, value: bool) HostValue {
        return hostValueBoolForBenchmark(host, roc_host, value);
    }

    pub fn dispatchKeyDownMeasured(host: *Host, roc_host: *RocHost, elem: *const BenchmarkDomElement, key: []const u8, shift_key: bool, stats: ?*BenchmarkStats) void {
        dispatchKeyDownWithStats(host, roc_host, elem, key, shift_key, stats);
    }

    pub fn dispatchSubmitMeasured(host: *Host, roc_host: *RocHost, elem: *const BenchmarkDomElement, stats: ?*BenchmarkStats) void {
        dispatchSubmitWithStats(host, roc_host, elem, stats);
    }

    pub fn setElementValueIfChanged(host: *Host, elem: *BenchmarkDomElement, value: []const u8) bool {
        return setElementValueForBenchmark(host, elem, value);
    }

    pub fn setElementCheckedIfChanged(elem: *BenchmarkDomElement, checked: bool) bool {
        return setElementCheckedForBenchmark(elem, checked);
    }

    pub fn resolvePendingTask(host: *Host, roc_host: *RocHost, name: []const u8, payload_text: []const u8, failed: bool) CommandCounts {
        return resolvePendingTaskForBenchmark(host, roc_host, name, payload_text, failed);
    }

    pub fn tickIntervalSource(host: *Host, roc_host: *RocHost, period_ms: u64) CommandCounts {
        return tickIntervalSourceForBenchmark(host, roc_host, period_ms);
    }

    pub fn finishHostMetrics(host: *Host) void {
        finishHostMetricsForBenchmark(host);
    }

    pub fn allocCount(host: *const Host) usize {
        return host.alloc_count;
    }

    pub fn deallocCount(host: *const Host) usize {
        return host.dealloc_count;
    }

    pub fn hostAllocCount(host: *const Host) u64 {
        return host.host_alloc_count;
    }

    pub fn hostDeallocCount(host: *const Host) u64 {
        return host.host_dealloc_count;
    }

    pub fn hostAllocBytes(host: *const Host) u64 {
        return host.host_alloc_bytes;
    }

    pub fn hostDeallocBytes(host: *const Host) u64 {
        return host.host_dealloc_bytes;
    }

    pub fn lastRuntimeMetrics(host: *const Host) RuntimeMetrics {
        return host.engine.last_runtime_metrics;
    }

    pub fn addRuntimeMetrics(left: RuntimeMetrics, right: RuntimeMetrics) RuntimeMetrics {
        return addRuntimeMetricsForBenchmark(left, right);
    }
};

const BenchmarkRunner = benchmark.Runner(BenchmarkCtx);
const runAppBenchmarks = BenchmarkRunner.runAppBenchmarks;

const SpecRunnerCtx = struct {
    pub const Host = HostEnv;
    pub const RocHost = abi.RocHost;

    pub fn fail(message: []const u8) noreturn {
        failHost(message);
    }

    pub fn writeStderr(bytes: []const u8) void {
        crash_handlers.writeStderr(bytes);
    }

    pub fn allocator(host: *Host) std.mem.Allocator {
        return host.hostAllocator();
    }

    pub fn findElementByLocator(host: *Host, locator: Locator, line_num: usize) ?*DomElement {
        return host.findElementByLocator(locator, line_num);
    }

    pub fn countElementsByLocator(host: *Host, locator: Locator) usize {
        return host.countElementsByLocator(locator);
    }

    pub fn namedEvent(elem: *const DomElement, name: []const u8) ?DomNamedEvent {
        return nodeEventName(elem, name);
    }

    pub fn dispatchRocEvent(host: *Host, roc_host: *RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue) void {
        dispatchRocEventWithStats(host, roc_host, event_id, payload_kind, payload, null);
    }

    pub fn hostValueUnit(host: *Host, roc_host: *RocHost) HostValue {
        return hostValueUnitForBenchmark(host, roc_host);
    }

    pub fn hostValueStr(host: *Host, roc_host: *RocHost, value: []const u8) HostValue {
        return hostValueStrForBenchmark(host, roc_host, value);
    }

    pub fn hostValueBool(host: *Host, roc_host: *RocHost, value: bool) HostValue {
        return hostValueBoolForBenchmark(host, roc_host, value);
    }

    pub fn hostValueU8List(host: *Host, roc_host: *RocHost, bytes: []const u8) HostValue {
        return hv.makeU8List(host, roc_host, bytes);
    }

    pub fn setElementValueIfChanged(host: *Host, elem: *DomElement, value: []const u8) bool {
        return sim_dom.setValueIfChanged(host.hostAllocator(), elem, value);
    }

    pub fn setElementCheckedIfChanged(elem: *DomElement, checked: bool) bool {
        return sim_dom.setCheckedIfChanged(elem, checked);
    }

    pub fn elementTextAttr(elem: *const DomElement, name: []const u8) ?[]const u8 {
        return sim_dom.textAttr(elem, name);
    }

    pub fn resolvePendingTask(host: *Host, roc_host: *RocHost, name: []const u8, payload_text: []const u8, failed: bool) CommandCounts {
        return resolvePendingTaskForBenchmark(host, roc_host, name, payload_text, failed);
    }

    pub fn tickIntervalSource(host: *Host, roc_host: *RocHost, period_ms: u64) CommandCounts {
        return tickIntervalSourceForBenchmark(host, roc_host, period_ms);
    }

    pub fn finishHostMetrics(host: *Host) void {
        finishHostMetricsForBenchmark(host);
    }

    pub fn cleanupEventCount(host: *const Host, name: []const u8) u64 {
        return host.engine.cleanupEventCount(name);
    }

    pub fn pendingTaskCountByName(host: *const Host, name: []const u8) u64 {
        return host.engine.pendingTaskCountByName(name);
    }

    pub fn activeIntervalRecordCountByPeriod(host: *const Host, period_ms: u64) u64 {
        return host.engine.activeIntervalRecordCountByPeriod(period_ms);
    }

    pub fn lastRuntimeMetrics(host: *const Host) RuntimeMetrics {
        return host.engine.last_runtime_metrics;
    }
};

const SpecRunner = spec_runner.Runner(SpecRunnerCtx);

comptime {
    if (!builtin.is_test) {
        @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
        @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
        @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
        @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
        @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
        @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });
        @export(&hostValueClone, .{ .name = "roc_host_value_clone", .visibility = .hidden });
        @export(&hostValueGetWithCapability, .{ .name = "roc_host_value_get_with_capability", .visibility = .hidden });
        @export(&hostValueGetWithSplit, .{ .name = "roc_host_value_get_with_split", .visibility = .hidden });
        @export(&hostValueStoreWithCapability, .{ .name = "roc_host_value_store_with_capability", .visibility = .hidden });
        @export(&hostValueStoreWithExistingCapability, .{ .name = "roc_host_value_store_with_existing_capability", .visibility = .hidden });
        @export(&hostValueTakeWithCapability, .{ .name = "roc_host_value_take_with_capability", .visibility = .hidden });
        @export(&hostValueTakeWithSplit, .{ .name = "roc_host_value_take_with_split", .visibility = .hidden });

        @export(&main, .{ .name = "main" });
        if (@import("builtin").os.tag == .windows) {
            @export(&__main, .{ .name = "__main" });
        }
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    var verbose = false;
    var bench_app = false;
    var bench_name: []const u8 = "app_dispatch";
    var bench_iterations: usize = 100;
    var bench_samples: usize = 3;
    var spec_file: ?[]const u8 = null;

    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        } else if (std.mem.eql(u8, arg, "--bench-app")) {
            bench_app = true;
        } else if (std.mem.eql(u8, arg, "--bench-name")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
                return 1;
            }
            bench_name = std.mem.span(argv[i]);
        } else if (std.mem.eql(u8, arg, "--bench-iterations")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
                return 1;
            }
            bench_iterations = std.fmt.parseInt(usize, std.mem.span(argv[i]), 10) catch {
                writeStderr("Error: Invalid --bench-iterations value\n");
                return 1;
            };
            if (bench_iterations == 0) {
                writeStderr("Error: --bench-iterations must be greater than zero\n");
                return 1;
            }
        } else if (std.mem.eql(u8, arg, "--bench-samples")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
                return 1;
            }
            bench_samples = std.fmt.parseInt(usize, std.mem.span(argv[i]), 10) catch {
                writeStderr("Error: Invalid --bench-samples value\n");
                return 1;
            };
            if (bench_samples == 0) {
                writeStderr("Error: --bench-samples must be greater than zero\n");
                return 1;
            }
        } else if (arg.len > 0 and arg[0] != '-') {
            spec_file = arg;
        } else {
            writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
            return 1;
        }
    }

    if (spec_file == null) {
        writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench-app [--bench-name NAME] [--bench-iterations N] [--bench-samples N] <test_spec.txt>\n");
        return 1;
    }

    if (bench_app) {
        return runAppBenchmarks(spec_file.?, bench_name, bench_iterations, bench_samples, verbose) catch |err| {
            writeStderr("HOST ERROR: ");
            writeStderr(@errorName(err));
            writeStderr("\n");
            return 1;
        };
    }

    return platform_main(spec_file.?, verbose) catch |err| {
        writeStderr("HOST ERROR: ");
        writeStderr(@errorName(err));
        writeStderr("\n");
        return 1;
    };
}

fn platform_main(spec_file: []const u8, verbose: bool) error{}!c_int {
    _ = crash_handlers.installForCurrentThread();

    var host_env = HostEnv.init();
    const allocator = host_env.hostAllocator();

    host_env.test_state.commands = parseTestSpecFile(allocator, spec_file) catch |err| {
        switch (err) {
            ParseError.FileNotFound => writeStderr("Error: Test spec file not found\n"),
            ParseError.InvalidFormat => writeStderr("Error: Invalid test spec format\n"),
            else => writeStderr("Error: Failed to parse test spec\n"),
        }
        return 1;
    };
    host_env.test_state.verbose = verbose;

    var roc_host = makeSignalsRocHost(&host_env);
    host_env.engine.roc_host = &roc_host;
    current_host = &host_env;
    current_roc_host = &roc_host;
    defer current_host = null;
    defer current_roc_host = null;
    defer host_env.deinit();

    acceptInitElem(&host_env, &roc_host, abi.roc_ui_init());

    if (verbose) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} recomputed nodes\n", .{
            host_env.dom_elements.items.len,
            host_env.engine.last_runtime_metrics.nodes_recomputed,
        }) catch "";
        writeStderr(msg);
        host_env.dumpDom();
    }

    return SpecRunner.run(&host_env, &roc_host, host_env.test_state.commands, verbose);
}

fn appendTestSignalDescriptor(host: *HostEnv, signal_id: u64, kind: SignalKind, source_event_ids: []const u64, input_signal_ids: []const u64) std.mem.Allocator.Error!void {
    if (!builtin.is_test) @compileError("appendTestSignalDescriptor is test-only");

    const allocator = host.hostAllocator();
    const owned_source_states = try allocator.dupe(u64, &.{});
    errdefer allocator.free(owned_source_states);
    const owned_source_events = try allocator.dupe(u64, source_event_ids);
    errdefer allocator.free(owned_source_events);
    const owned_inputs = try allocator.dupe(u64, input_signal_ids);
    errdefer allocator.free(owned_inputs);

    try host.engine.signal_descriptors.append(allocator, .{
        .signal_id = signal_id,
        .kind = kind,
        .source_state_ids = owned_source_states,
        .source_event_ids = owned_source_events,
        .input_signal_ids = owned_inputs,
        .rank = 0,
    });
}

fn deinitTestHostGraph(host: *HostEnv) void {
    if (!builtin.is_test) @compileError("deinitTestHostGraph is test-only");

    const allocator = host.hostAllocator();
    host.clearActiveSignalRoutes();
    host.engine.active_source_signal_routes.deinit(allocator);
    host.engine.active_text_signal_routes.deinit(allocator);
    host.engine.active_bool_signal_routes.deinit(allocator);
    host.engine.active_change_signal_routes.deinit(allocator);
    host.engine.active_structural_signal_routes.deinit(allocator);
    host.engine.clearActiveIntervals(host);
    host.engine.active_intervals.deinit(allocator);
    host.clearActiveSignalGraph();
    host.engine.active_signal_graph.deinit(allocator);
    host.clearActiveEvents();
    host.engine.active_events.deinit(allocator);
    host.clearEventDescriptors();
    host.engine.event_descriptors.deinit(allocator);
    host.clearSignalEventRoutes();
    host.engine.signal_event_routes.deinit(allocator);
    host.clearSignalDescriptors();
    host.engine.signal_descriptors.deinit(allocator);
    host.clearSignalRoutes();
    host.engine.signal_routes.deinit(allocator);
    host.clearSignalDependents();
    host.engine.signal_dependents.deinit(allocator);
    host.clearSignalCache();
    host.engine.signal_cache.deinit(allocator);
}

fn deinitTestHostIdentity(host: *HostEnv) void {
    if (!builtin.is_test) @compileError("deinitTestHostIdentity is test-only");

    const allocator = host.hostAllocator();
    host.clearScopes();
    host.engine.scopes.deinit(allocator);
    host.engine.node_identities.deinit(allocator);
    host.engine.dom_identities.deinit(allocator);
    host.engine.deinitScratch(host);
    if (host.engine.host_values.hasLiveValues()) failHost("test host value registry still owned a typed cell at shutdown");
    host.engine.host_values.deinit(allocator);
    host.test_host_value_kinds.deinit(allocator);
    host.roc_allocations.deinit(allocator);
}

test "signals host dirty plan deduplicates diamond join by rank" {
    var host = HostEnv.init();
    defer {
        deinitTestHostGraph(&host);
        _ = host.gpa.deinit();
    }
    const allocator = host.hostAllocator();

    try host.engine.event_descriptors.append(allocator, .{
        .event_id = 1,
        .payload_kind = .unit,
        .payload_accessor = .none,
    });
    try appendTestSignalDescriptor(&host, 0, .source, &.{1}, &.{});
    try appendTestSignalDescriptor(&host, 1, .map, &.{}, &.{0});
    try appendTestSignalDescriptor(&host, 2, .map, &.{}, &.{0});
    try appendTestSignalDescriptor(&host, 3, .map2, &.{}, &.{ 1, 2 });

    host.rebuildSignalTopologyFromSignals();
    host.rebuildSignalEventRoutesFromSignals();

    const dirty_signal_ids = host.dirtySignalIdsForEvent(allocator, 1);
    defer allocator.free(dirty_signal_ids);

    try std.testing.expectEqualSlices(u64, &.{ 0, 1, 2, 3 }, dirty_signal_ids);
    try std.testing.expectEqual(@as(u64, 0), host.signalRank(0));
    try std.testing.expectEqual(@as(u64, 1), host.signalRank(1));
    try std.testing.expectEqual(@as(u64, 1), host.signalRank(2));
    try std.testing.expectEqual(@as(u64, 2), host.signalRank(3));
}

test "signals metrics accumulate propagation pruning counters" {
    var left = zeroRuntimeMetrics();
    left.active_graph_records_rebuilt = 7;
    left.allocs_this_event = 9;
    left.deallocs_this_event = 6;
    left.events_processed = 2;
    left.host_allocs_this_event = 3;
    left.host_deallocs_this_event = 2;
    left.host_alloc_bytes_this_event = 128;
    left.host_dealloc_bytes_this_event = 64;
    left.host_retained_alloc_delta = 1;
    left.host_retained_bytes_delta = 64;
    left.nodes_recomputed = 5;
    left.propagation_prunes = 3;
    left.derived_calls_into_roc = 4;
    left.each_key_compares = 6;
    left.each_key_hashes = 2;
    left.each_key_reuse_compares = 3;
    left.each_key_duplicate_compares = 1;
    left.each_item_compares = 4;
    left.each_syncs = 5;
    left.each_sync_keys = 6;
    left.each_sync_existing_rows = 7;
    left.recompute_batches = 2;
    left.patches_emitted = 7;
    left.create_element = 2;
    left.append_child = 3;
    left.remove_node = 4;
    left.move_before = 5;
    left.set_text = 1;
    left.bind_event = 1;
    left.stream_nodes_scanned = 12;
    left.stream_nodes_scanned_apply = 1;
    left.stream_nodes_scanned_children = 2;
    left.stream_nodes_scanned_dirty_scope = 3;
    left.stream_nodes_scanned_events = 4;
    left.stream_nodes_scanned_mounts = 5;
    left.stream_nodes_scanned_remove_target = 6;
    left.stream_nodes_scanned_render_scope = 7;
    left.stream_nodes_scanned_splice = 8;
    left.signal_record_table_rebuilt = 9;
    left.active_intervals_synced = 10;
    left.render_indexes_refreshed = 11;

    var right = zeroRuntimeMetrics();
    right.active_graph_records_rebuilt = 2;
    right.allocs_this_event = 4;
    right.deallocs_this_event = 5;
    right.events_processed = 1;
    right.host_allocs_this_event = 5;
    right.host_deallocs_this_event = 1;
    right.host_alloc_bytes_this_event = 512;
    right.host_dealloc_bytes_this_event = 128;
    right.host_retained_alloc_delta = 4;
    right.host_retained_bytes_delta = 384;
    right.nodes_recomputed = 8;
    right.propagation_prunes = 11;
    right.derived_calls_into_roc = 6;
    right.each_key_compares = 7;
    right.each_key_hashes = 5;
    right.each_key_reuse_compares = 7;
    right.each_key_duplicate_compares = 11;
    right.each_item_compares = 13;
    right.each_syncs = 17;
    right.each_sync_keys = 19;
    right.each_sync_existing_rows = 23;
    right.recompute_batches = 1;
    right.patches_emitted = 13;
    right.create_element = 5;
    right.append_child = 8;
    right.remove_node = 10;
    right.move_before = 12;
    right.set_text = 2;
    right.bind_event = 4;
    right.stream_nodes_scanned = 5;
    right.stream_nodes_scanned_apply = 11;
    right.stream_nodes_scanned_children = 13;
    right.stream_nodes_scanned_dirty_scope = 17;
    right.stream_nodes_scanned_events = 19;
    right.stream_nodes_scanned_mounts = 23;
    right.stream_nodes_scanned_remove_target = 29;
    right.stream_nodes_scanned_render_scope = 31;
    right.stream_nodes_scanned_splice = 37;
    right.signal_record_table_rebuilt = 41;
    right.active_intervals_synced = 43;
    right.render_indexes_refreshed = 47;
    right.retained_alloc_delta = -2;

    const total = addRuntimeMetrics(left, right);
    try std.testing.expectEqual(@as(u64, 9), total.active_graph_records_rebuilt);
    try std.testing.expectEqual(@as(u64, 13), total.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 11), total.deallocs_this_event);
    try std.testing.expectEqual(@as(u64, 3), total.events_processed);
    try std.testing.expectEqual(@as(u64, 8), total.host_allocs_this_event);
    try std.testing.expectEqual(@as(u64, 3), total.host_deallocs_this_event);
    try std.testing.expectEqual(@as(u64, 640), total.host_alloc_bytes_this_event);
    try std.testing.expectEqual(@as(u64, 192), total.host_dealloc_bytes_this_event);
    try std.testing.expectEqual(@as(i64, 5), total.host_retained_alloc_delta);
    try std.testing.expectEqual(@as(i64, 448), total.host_retained_bytes_delta);
    try std.testing.expectEqual(@as(u64, 13), total.nodes_recomputed);
    try std.testing.expectEqual(@as(u64, 14), total.propagation_prunes);
    try std.testing.expectEqual(@as(u64, 10), total.derived_calls_into_roc);
    try std.testing.expectEqual(@as(u64, 13), total.each_key_compares);
    try std.testing.expectEqual(@as(u64, 7), total.each_key_hashes);
    try std.testing.expectEqual(@as(u64, 10), total.each_key_reuse_compares);
    try std.testing.expectEqual(@as(u64, 12), total.each_key_duplicate_compares);
    try std.testing.expectEqual(@as(u64, 17), total.each_item_compares);
    try std.testing.expectEqual(@as(u64, 22), total.each_syncs);
    try std.testing.expectEqual(@as(u64, 25), total.each_sync_keys);
    try std.testing.expectEqual(@as(u64, 30), total.each_sync_existing_rows);
    try std.testing.expectEqual(@as(u64, 3), total.recompute_batches);
    try std.testing.expectEqual(@as(u64, 20), total.patches_emitted);
    try std.testing.expectEqual(@as(u64, 7), total.create_element);
    try std.testing.expectEqual(@as(u64, 11), total.append_child);
    try std.testing.expectEqual(@as(u64, 14), total.remove_node);
    try std.testing.expectEqual(@as(u64, 17), total.move_before);
    try std.testing.expectEqual(@as(u64, 3), total.set_text);
    try std.testing.expectEqual(@as(u64, 5), total.bind_event);
    try std.testing.expectEqual(@as(u64, 17), total.stream_nodes_scanned);
    try std.testing.expectEqual(@as(u64, 12), total.stream_nodes_scanned_apply);
    try std.testing.expectEqual(@as(u64, 15), total.stream_nodes_scanned_children);
    try std.testing.expectEqual(@as(u64, 20), total.stream_nodes_scanned_dirty_scope);
    try std.testing.expectEqual(@as(u64, 23), total.stream_nodes_scanned_events);
    try std.testing.expectEqual(@as(u64, 28), total.stream_nodes_scanned_mounts);
    try std.testing.expectEqual(@as(u64, 35), total.stream_nodes_scanned_remove_target);
    try std.testing.expectEqual(@as(u64, 38), total.stream_nodes_scanned_render_scope);
    try std.testing.expectEqual(@as(u64, 45), total.stream_nodes_scanned_splice);
    try std.testing.expectEqual(@as(u64, 50), total.signal_record_table_rebuilt);
    try std.testing.expectEqual(@as(u64, 53), total.active_intervals_synced);
    try std.testing.expectEqual(@as(u64, 58), total.render_indexes_refreshed);
    try std.testing.expectEqual(@as(i64, -2), total.retained_alloc_delta);
}

test "signals host assigns explicit active graph record ids" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const label = testNodeStableStrMapExpr(&roc_host, testNodeRefExpr(state_token));
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testNodeTextSignal(&roc_host, label),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expectEqual(@as(usize, 2), host.engine.active_signal_graph.items.len);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.active_graph_records_rebuilt);

    const first_record = host.engine.active_signal_graph.items[0].record;
    const second_record = host.engine.active_signal_graph.items[1].record;
    try std.testing.expectEqual(@as(?u64, 0), first_record.active_graph_id);
    try std.testing.expectEqual(@as(?u64, 1), second_record.active_graph_id);
    try std.testing.expectEqual(@as(u64, 0), host.requireActiveSignalRecordId(first_record));
    try std.testing.expectEqual(@as(u64, 1), host.requireActiveSignalRecordId(second_record));

    host.clearActiveSignalGraph();

    try std.testing.expectEqual(@as(usize, 0), host.engine.active_signal_graph.items.len);
    try std.testing.expectEqual(@as(?u64, null), first_record.active_graph_id);
    try std.testing.expectEqual(@as(?u64, null), second_record.active_graph_id);
}

test "signals host allocation ledger tracks exact returned pointers" {
    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const first = rocAllocFn(&roc_host, 8, 8) orelse return error.OutOfMemory;
    const middle = rocAllocFn(&roc_host, 16, 8) orelse return error.OutOfMemory;
    const last = rocAllocFn(&roc_host, 24, 8) orelse return error.OutOfMemory;

    try std.testing.expectEqual(@as(usize, 3), host.roc_allocations.allocations.items.len);
    try std.testing.expect(findExactRocAllocationIndex(&host, first) != null);
    try std.testing.expect(findExactRocAllocationIndex(&host, middle) != null);
    try std.testing.expect(findExactRocAllocationIndex(&host, last) != null);

    rocDeallocFn(&roc_host, middle, 8);

    try std.testing.expectEqual(@as(usize, 2), host.roc_allocations.allocations.items.len);
    try std.testing.expect(findExactRocAllocationIndex(&host, first) != null);
    try std.testing.expectEqual(@as(?usize, null), findExactRocAllocationIndex(&host, middle));
    try std.testing.expect(findExactRocAllocationIndex(&host, last) != null);
    try std.testing.expect(findRecentlyFreedRocAllocation(&host, middle) != null);
    try std.testing.expectEqual(@as(u64, 3), host.engine.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 1), host.engine.pending_roc_metrics.deallocs_this_event);

    const grown = rocReallocFn(&roc_host, first, 32, 8) orelse return error.OutOfMemory;

    try std.testing.expectEqual(@as(usize, 2), host.roc_allocations.allocations.items.len);
    try std.testing.expectEqual(@as(?usize, null), findExactRocAllocationIndex(&host, first));
    try std.testing.expect(findExactRocAllocationIndex(&host, grown) != null);
    try std.testing.expect(findExactRocAllocationIndex(&host, last) != null);
    try std.testing.expect(findRecentlyFreedRocAllocation(&host, first) != null);
    try std.testing.expectEqual(@as(u64, 4), host.alloc_count);
    try std.testing.expectEqual(@as(u64, 2), host.dealloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.engine.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.deallocs_this_event);

    rocDeallocFn(&roc_host, last, 8);
    rocDeallocFn(&roc_host, grown, 8);

    try std.testing.expectEqual(@as(usize, 0), host.roc_allocations.allocations.items.len);
    try std.testing.expectEqual(@as(u64, 4), host.alloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.dealloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.engine.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 4), host.engine.pending_roc_metrics.deallocs_this_event);
}

const TestErasedI64Capture = extern struct {
    amount: i64,
};

const TestErasedBinderCapture = extern struct {
    condition_binder: HostBinderToken,
    condition_cap: HostValueCapability,
};

const TestErasedHostValueCapture = extern struct {
    value: HostValue,
};

const TestCapabilityCloneCapture = extern struct {
    split: abi.RocErasedCallable,
};

const TestTaskPayloadCapture = extern struct {
    payload_cap: HostValueCapability,
};

var test_erased_callable_drop_count: u64 = 0;
var test_row_elem_call_count: u64 = 0;

fn testCapturePtrAs(comptime T: type, capture_ptr: ?[*]u8) *T {
    return @ptrCast(@alignCast(capture_ptr orelse unreachable));
}

fn testErasedArgsAs(comptime T: type, args: ?[*]const u8) *align(1) const T {
    return @as(*align(1) const T, @ptrCast(args orelse unreachable));
}

fn writeTestErasedResult(comptime T: type, ret: ?[*]u8, value: T) void {
    @as(*align(1) T, @ptrCast(ret orelse unreachable)).* = value;
}

fn writeTestErasedCallable(
    comptime Capture: type,
    roc_host: *abi.RocHost,
    callable_fn_ptr: abi.RocErasedCallableFn,
    on_drop: ?abi.RocErasedCallableOnDrop,
    capture: Capture,
) abi.RocErasedCallable {
    comptime {
        if (@alignOf(Capture) > abi.roc_erased_callable_capture_alignment) {
            @compileError("signals test erased-callable capture alignment exceeds Roc ABI alignment");
        }
    }
    const callable = abi.rocErasedCallableAllocate(roc_host, callable_fn_ptr, on_drop, @sizeOf(Capture));
    testCapturePtrAs(Capture, abi.rocErasedCallableCapturePtr(callable)).* = capture;
    return callable;
}

fn testCurrentRocHost() *abi.RocHost {
    return currentHost().engine.roc_host orelse failHost("test HostValue helper requires an active Roc host");
}

fn capabilityTestHostValue(host: *HostEnv, roc_host: *abi.RocHost, value: HostValue) HostValue {
    const cap = testHostValueCapability(roc_host);
    host.setHostValueCapability(value, cap);
    host.releaseOwnedHostValueCapability(cap);
    return value;
}

fn testHostValueUnit() HostValue {
    const host = currentHost();
    const roc_host = testCurrentRocHost();
    return hostValueUnit(host, roc_host);
}

fn testHostValueStr(roc_host: *abi.RocHost, value: []const u8) HostValue {
    const host = hostFromRocHost(roc_host);
    return capabilityTestHostValue(host, roc_host, hostValueStr(host, roc_host, value));
}

fn testHostValueBool(value: bool) HostValue {
    const host = currentHost();
    const roc_host = testCurrentRocHost();
    return capabilityTestHostValue(host, roc_host, hostValueBool(host, roc_host, value));
}

fn testHostValueI64(value: i64) HostValue {
    const host = currentHost();
    const roc_host = testCurrentRocHost();
    return capabilityTestHostValue(host, roc_host, hostValueI64(host, roc_host, value));
}

fn testReadHostValueI64(roc_host: *abi.RocHost, value: HostValue) i64 {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .i64) @panic("test HostValue expected I64");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const i64 = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueBool(roc_host: *abi.RocHost, value: HostValue) bool {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .bool) @panic("test HostValue expected Bool");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const bool = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueStr(roc_host: *abi.RocHost, value: HostValue) RocStr {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .str) @panic("test HostValue expected Str");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const RocStr = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueI64List(roc_host: *abi.RocHost, value: HostValue) I64List {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .i64_list) @panic("test HostValue expected List(I64)");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const I64List = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testReadHostValueU8List(roc_host: *abi.RocHost, value: HostValue) U8List {
    const host = hostFromRocHost(roc_host);
    if (host.testHostValueKind(value) != .u8_list) @panic("test HostValue expected List(U8)");
    const box = host.getHostValue(value);
    defer abi.decrefBox(box, roc_host);
    const payload: *const U8List = @ptrCast(@alignCast(box orelse unreachable));
    return payload.*;
}

fn testDropRocStrBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *RocStr = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropI64ListBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *I64List = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropU8ListBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *U8List = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropHostValue(roc_host: *abi.RocHost, value: HostValue) void {
    const host = hostFromRocHost(roc_host);
    const kind = host.testHostValueKind(value);
    const box = host.takeHostValue(value);
    switch (kind) {
        .unit, .i64, .bool => abi.decrefBox(box, roc_host),
        .str => abi.decrefBoxWith(box, @alignOf(RocStr), true, &testDropRocStrBoxPayload, roc_host),
        .i64_list => abi.decrefBoxWith(box, @alignOf(I64List), true, &testDropI64ListBoxPayload, roc_host),
        .u8_list => abi.decrefBoxWith(box, @alignOf(U8List), true, &testDropU8ListBoxPayload, roc_host),
    }
}

fn testHostValueDropCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testDropHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testHostValueCloneCallable(roc_host: *abi.RocHost, split: abi.RocErasedCallable) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestCapabilityCloneCapture,
        roc_host,
        &testCloneHostValueWithSplitCallable,
        &testCapabilityCloneOnDrop,
        .{ .split = split },
    );
}

fn testHostValueCapabilityWithEq(roc_host: *abi.RocHost, eq_fn: abi.RocErasedCallableFn) HostValueCapability {
    const split = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testSplitHostValueBoxCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .clone = testHostValueCloneCallable(roc_host, split),
        .eq = writeTestErasedCallable(
            TestErasedI64Capture,
            roc_host,
            eq_fn,
            &testErasedCallableOnDrop,
            .{ .amount = 0 },
        ),
        .drop = testHostValueDropCallable(roc_host),
    };
}

fn testHostValueCapability(roc_host: *abi.RocHost) HostValueCapability {
    return testHostValueCapabilityWithEq(roc_host, &testHostValueEqErasedCallable);
}

fn testHostValueKeyTextCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueKeyTextErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testHashI64KeyText(value: i64) u64 {
    var buf: [32]u8 = undefined;
    const text = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
    return std.hash.Wyhash.hash(0, text);
}

fn testHashHostValueKeyText(roc_host: *abi.RocHost, key: HostValue) u64 {
    return testHashI64KeyText(testReadHostValueI64(roc_host, key));
}

fn testReadStrCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testReadStrHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testReadBoolCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testReadBoolHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testItemsToValuesCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testI64ListToHostValuesCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testUnaryHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const input = testReadHostValueI64(roc_host, call_args.arg0);
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueI64(host, roc_host, input + capture.amount)));
}

fn testHostValueKeyTextErasedCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const value = testReadHostValueI64(roc_host, call_args.arg0);
    var buf: [32]u8 = undefined;
    const text = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
    writeTestErasedResult(RocStr, ret, RocStr.fromSlice(text, roc_host));
}

fn testUnaryIdentityHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    writeTestErasedResult(HostValue, ret, hostFromRocHost(roc_host).cloneHostValue(call_args.arg0));
}

fn testBinaryHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const left = testReadHostValueI64(roc_host, call_args.arg0);
    const right = testReadHostValueI64(roc_host, call_args.arg1);
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueI64(host, roc_host, left + right + capture.amount)));
}

fn testUnitIncrementHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const current = testReadHostValueI64(roc_host, call_args.arg0);
    if (hostFromRocHost(roc_host).testHostValueKind(call_args.arg1) != .unit) @panic("test unit event callable expected unit payload");
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueI64(host, roc_host, current + 1)));
}

fn testInitialHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    const capture = testCapturePtrAs(TestErasedHostValueCapture, capture_ptr);
    writeTestErasedResult(HostValue, ret, hostFromRocHost(roc_host).cloneHostValue(capture.value));
}

fn testBinaryElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const left = testReadHostValueI64(roc_host, call_args.arg0);
    const right = testReadHostValueI64(roc_host, call_args.arg1);
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}", .{left + right + capture.amount}) catch @panic("test row Elem callable could not format text");
    writeTestErasedResult(abi.Elem, ret, testNodeText(roc_host, text));
}

fn testStatefulRowElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const key = testReadHostValueI64(roc_host, call_args.arg0);
    const item = testReadHostValueI64(roc_host, call_args.arg1);
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}-{d}", .{ key, item + capture.amount }) catch @panic("test stateful row Elem callable could not format text");
    writeTestErasedResult(abi.Elem, ret, testNodeState(roc_host, testNodeText(roc_host, text)));
}

fn testStatefulRowButtonElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const key = testReadHostValueI64(roc_host, call_args.arg0);
    const item = testReadHostValueI64(roc_host, call_args.arg1);
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-action-{d}-{d}", .{ key, item + capture.amount }) catch @panic("test stateful row button Elem callable could not format text");
    const token = newTestBinderToken(roc_host);
    const attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(roc_host, .text, text),
        testNodeEventAttr(roc_host, .click, token, .unit),
    };
    const button = testElementWith(roc_host, "button", &attrs, &.{});
    writeTestErasedResult(abi.Elem, ret, testNodeStateWithToken(roc_host, token, button));
}

fn testNestedWhenRowElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    test_row_elem_call_count += 1;
    const capture = testCapturePtrAs(TestErasedBinderCapture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const key = testReadHostValueI64(roc_host, call_args.arg0);
    const item = testReadHostValueI64(roc_host, call_args.arg1);

    var true_text_buffer: [64]u8 = undefined;
    var false_text_buffer: [64]u8 = undefined;
    const true_text = std.fmt.bufPrint(&true_text_buffer, "row-{d}-{d}-true", .{ key, item }) catch @panic("test nested row true text format failed");
    const false_text = std.fmt.bufPrint(&false_text_buffer, "row-{d}-{d}-false", .{ key, item }) catch @panic("test nested row false text format failed");
    const row = abi.Elem{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeRefExpr(capture.condition_binder)),
                .read = testBoolReadHandle(roc_host, capture.condition_cap),
                .when_false = boxTestElem(roc_host, testNodeText(roc_host, false_text)),
                .when_true = boxTestElem(roc_host, testNodeText(roc_host, true_text)),
            },
        },
        .tag = .When,
    };
    writeTestErasedResult(abi.Elem, ret, row);
}

fn testHostValueEqErasedCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const host = hostFromRocHost(roc_host);
    const left_kind = host.testHostValueKind(call_args.arg0);
    const right_kind = host.testHostValueKind(call_args.arg1);
    const is_equal = if (left_kind != right_kind) false else switch (left_kind) {
        .unit => true,
        .i64 => testReadHostValueI64(roc_host, call_args.arg0) == testReadHostValueI64(roc_host, call_args.arg1),
        .bool => testReadHostValueBool(roc_host, call_args.arg0) == testReadHostValueBool(roc_host, call_args.arg1),
        .str => blk: {
            const left = testReadHostValueStr(roc_host, call_args.arg0);
            const right = testReadHostValueStr(roc_host, call_args.arg1);
            break :blk std.mem.eql(u8, left.asSlice(), right.asSlice());
        },
        .i64_list => blk: {
            const left = testReadHostValueI64List(roc_host, call_args.arg0);
            const right = testReadHostValueI64List(roc_host, call_args.arg1);
            break :blk std.mem.eql(i64, left.items(), right.items());
        },
        .u8_list => blk: {
            const left = testReadHostValueU8List(roc_host, call_args.arg0);
            const right = testReadHostValueU8List(roc_host, call_args.arg1);
            break :blk std.mem.eql(u8, left.items(), right.items());
        },
    };
    writeTestErasedResult(bool, ret, is_equal);
}

fn testStableStrHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueStr(host, roc_host, "stable")));
}

fn testStableI64HostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueI64(host, roc_host, capture.amount)));
}

fn testStableBoolHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueBool(host, roc_host, true)));
}

fn testBoolIdentityHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const input = testReadHostValueBool(roc_host, call_args.arg0);
    const host = hostFromRocHost(roc_host);
    writeTestErasedResult(HostValue, ret, capabilityTestHostValue(host, roc_host, hostValueBool(host, roc_host, input)));
}

fn testAlwaysEqualHostValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(bool, ret, true);
}

fn testNeverEqualHostValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(bool, ret, false);
}

fn testSplitHostValueBoxCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedRocBoxUnaryArgs, args);
    abi.increfBox(call_args.arg0, 1);
    writeTestErasedResult(erased_calls.RocBoxPair, ret, .{
        .keep = call_args.arg0,
        .out = call_args.arg0,
    });
}

fn testCloneHostValueWithSplitCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestCapabilityCloneCapture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const host = hostFromRocHost(roc_host);

    abi.increfErasedCallable(capture.split, 1);
    const box = host.getHostValueWithSplit(call_args.arg0, capture.split);
    const cloned = host.storeHostValueWithExistingCapability(box, call_args.arg0);
    writeTestErasedResult(HostValue, ret, cloned);
}

fn testErasedCallableOnDrop(_: ?[*]u8, _: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
}

fn testCapabilityCloneOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestCapabilityCloneCapture, capture_ptr);
    abi.decrefErasedCallable(capture.split, roc_host);
}

fn testBinderCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedBinderCapture, capture_ptr);
    hv.releaseU64Box(capture.condition_binder, roc_host);
    hv.releaseHostValueCapability(capture.condition_cap, roc_host);
}

fn testDropHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = ret;
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    testDropHostValue(roc_host, call_args.arg0);
}

fn testConsumeTaskPayloadStrCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const host = hostFromRocHost(roc_host);
    const capture = testCapturePtrAs(TestTaskPayloadCapture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const box = host.takeHostValueWithCapability(call_args.arg0, hv.retainHostValueCapability(capture.payload_cap));
    const value = host.storeHostValueWithRetainedCapability(box, capture.payload_cap);
    if (builtin.is_test) host.setTestHostValueKind(value, .str);
    writeTestErasedResult(HostValue, ret, value);
}

fn testReadStrHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    var value = testReadHostValueStr(roc_host, call_args.arg0);
    value.incref(1);
    writeTestErasedResult(RocStr, ret, value);
}

fn testReadBoolHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    writeTestErasedResult(bool, ret, testReadHostValueBool(roc_host, call_args.arg0));
}

fn testI64ListToHostValuesCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const host = hostFromRocHost(roc_host);
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const source = testReadHostValueI64List(roc_host, call_args.arg0);
    const source_items = source.items();
    const result = HostValueList.allocate(source_items.len, roc_host);
    if (source_items.len > 0) {
        const dest = result.elements_ptr orelse unreachable;
        for (source_items, 0..) |item, index| {
            dest[index] = capabilityTestHostValue(host, roc_host, hostValueI64(host, roc_host, item));
        }
    }
    writeTestErasedResult(HostValueList, ret, result);
}

fn testHostValueCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedHostValueCapture, capture_ptr);
    testDropHostValue(roc_host, capture.value);
}

fn expectHostValueI64(value: HostValue, expected: i64) error{TestExpectedEqual}!void {
    const roc_host = testCurrentRocHost();
    try std.testing.expectEqual(expected, testReadHostValueI64(roc_host, value));
    testDropHostValue(roc_host, value);
}

fn expectCachedTaskSourceText(roc_host: *abi.RocHost, record: *HostSignalRecord, expected: []const u8) error{TestExpectedEqual}!void {
    const task_payload = switch (record.payload) {
        .task_source => |payload| payload,
        else => unreachable,
    };
    const cached = switch (task_payload.cached_value) {
        .present => |cell| cell,
        .absent => return error.TestExpectedEqual,
    };
    const text = testReadHostValueStr(roc_host, cached.value);
    try std.testing.expectEqualStrings(expected, text.asSlice());
}

fn makeTestConsumingTaskSourceRecord(host: *HostEnv, roc_host: *abi.RocHost, name: []const u8) *HostSignalRecord {
    const allocator = host.hostAllocator();
    const payload_cap = testHostValueCapability(roc_host);
    const capture = TestTaskPayloadCapture{ .payload_cap = payload_cap };
    return HostSignalRecord.init(allocator, .{ .task_source = .{
        .token = newTestSignalToken(roc_host),
        .name = allocator.dupe(u8, name) catch @panic("out of memory"),
        .payload_cap = payload_cap,
        .initial = writeTestErasedCallable(
            TestErasedI64Capture,
            roc_host,
            &testStableStrHostValueCallable,
            null,
            .{ .amount = 0 },
        ),
        .done = writeTestErasedCallable(
            TestTaskPayloadCapture,
            roc_host,
            &testConsumeTaskPayloadStrCallable,
            &testErasedCallableOnDrop,
            capture,
        ),
        .failed = writeTestErasedCallable(
            TestTaskPayloadCapture,
            roc_host,
            &testConsumeTaskPayloadStrCallable,
            &testErasedCallableOnDrop,
            capture,
        ),
        .cap = testHostValueCapability(roc_host),
        .reset_on_start = false,
    } });
}

test "signals host invokes erased HostValue thunks with ABI argument layouts" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    {
        const unary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testUnaryHostValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 5 },
        );
        defer abi.decrefErasedCallable(unary, &roc_host);

        const input = testHostValueI64(37);
        defer testDropHostValue(&roc_host, input);
        const result = callErasedHostValueToHostValue(&roc_host, unary, input);
        try expectHostValueI64(result, 42);
    }

    {
        const binary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryHostValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(binary, &roc_host);

        const left = testHostValueI64(10);
        defer testDropHostValue(&roc_host, left);
        const right = testHostValueI64(29);
        defer testDropHostValue(&roc_host, right);
        const result = callErasedHostValueHostValueToHostValue(&roc_host, binary, left, right);
        try expectHostValueI64(result, 42);
    }

    {
        const row = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryElemCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(row, &roc_host);

        const left = testHostValueI64(10);
        defer testDropHostValue(&roc_host, left);
        const right = testHostValueI64(29);
        defer testDropHostValue(&roc_host, right);
        const result = callErasedHostValueHostValueToElem(&roc_host, row, left, right);
        defer abi.decrefElem(result, &roc_host);

        try std.testing.expectEqual(abi.ElemTag.Text, result.tag);
        try std.testing.expectEqualStrings("row-42", result.payload.text.asSlice());
    }

    {
        const eq = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testHostValueEqErasedCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 0 },
        );
        defer abi.decrefErasedCallable(eq, &roc_host);

        const equal_left = testHostValueI64(42);
        defer testDropHostValue(&roc_host, equal_left);
        const equal_right = testHostValueI64(42);
        defer testDropHostValue(&roc_host, equal_right);
        try std.testing.expect(callErasedHostValueHostValueToBool(&roc_host, eq, equal_left, equal_right));

        const unequal_left = testHostValueI64(41);
        defer testDropHostValue(&roc_host, unequal_left);
        const unequal_right = testHostValueI64(42);
        defer testDropHostValue(&roc_host, unequal_right);
        try std.testing.expect(!callErasedHostValueHostValueToBool(&roc_host, eq, unequal_left, unequal_right));
    }

    try std.testing.expectEqual(@as(u64, 48), test_erased_callable_drop_count);
}

test "signals host task result callbacks consume heap string payloads" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;

    const record = makeTestConsumingTaskSourceRecord(&host, &roc_host, "lookup");
    host.engine.retainActiveSignalRecord(&host, record);
    defer {
        host.engine.clearActiveSignalGraph(&host);
        record.release(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
        host.deinit();
        _ = host.gpa.deinit();
    }

    const success_payload = "successful task payload that is intentionally longer than the Roc small string limit";
    _ = host.engine.appendPendingTask(&host, 0, switch (record.payload) {
        .task_source => |payload| payload.token,
        else => unreachable,
    }, "lookup", "/api/test");
    _ = resolvePendingTask(&host, &roc_host, "lookup", success_payload, false);
    try expectCachedTaskSourceText(&roc_host, record, success_payload);
    try std.testing.expectEqual(@as(usize, 0), host.engine.pending_tasks.items.len);

    const failed_payload = "failed task payload that is intentionally longer than the Roc small string limit";
    _ = host.engine.appendPendingTask(&host, 0, switch (record.payload) {
        .task_source => |payload| payload.token,
        else => unreachable,
    }, "lookup", "/api/test");
    _ = resolvePendingTask(&host, &roc_host, "lookup", failed_payload, true);
    try expectCachedTaskSourceText(&roc_host, record, failed_payload);
    try std.testing.expectEqual(@as(usize, 0), host.engine.pending_tasks.items.len);
}

test "signals host interns scopes and node identities from explicit paths" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const root = host.internRootScope();
    try std.testing.expectEqual(@as(u64, 0), root);
    try std.testing.expectEqual(root, host.internRootScope());

    const true_branch = host.internWhenBranchScope(root, 2, .true_branch);
    try std.testing.expectEqual(true_branch, host.internWhenBranchScope(root, 2, .true_branch));

    const false_branch = host.internWhenBranchScope(root, 2, .false_branch);
    try std.testing.expect(false_branch != true_branch);

    const nested_true_branch = host.internWhenBranchScope(true_branch, 2, .true_branch);
    try std.testing.expect(nested_true_branch != true_branch);

    const key_cap = testHostValueCapability(&roc_host);
    defer abi.decrefHostValueCapabilityHandle(key_cap, &roc_host);

    const initial_keys = [_]HostValue{ testHostValueI64(10), testHostValueI64(11) };
    const initial_rows = syncTestEachRowScopes(&host, &roc_host, root, 7, &initial_keys, &initial_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, initial_rows);
    const row_a = initial_rows.scope_ids[0];
    const row_b = initial_rows.scope_ids[1];
    try std.testing.expect(row_b != row_a);

    const same_keys = [_]HostValue{testHostValueI64(10)};
    const same_rows = syncTestEachRowScopes(&host, &roc_host, root, 7, &same_keys, &same_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, same_rows);
    try std.testing.expectEqual(row_a, same_rows.scope_ids[0]);

    const other_site_keys = [_]HostValue{testHostValueI64(10)};
    const other_site_rows = syncTestEachRowScopes(&host, &roc_host, root, 8, &other_site_keys, &other_site_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, other_site_rows);
    const same_key_other_site = other_site_rows.scope_ids[0];
    try std.testing.expect(same_key_other_site != row_a);

    const root_state = host.internNodeIdentity(root, 0);
    try std.testing.expectEqual(root_state, host.internNodeIdentity(root, 0));

    const row_state = host.internNodeIdentity(row_a, 0);
    try std.testing.expect(row_state != root_state);

    const row_next_state = host.internNodeIdentity(row_a, 1);
    try std.testing.expect(row_next_state != row_state);
}

test "signals host disposal retires scope subtree identities" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_cap = testHostValueCapability(&roc_host);
    defer abi.decrefHostValueCapabilityHandle(key_cap, &roc_host);

    const root = host.internRootScope();
    const row = createTestEachRowScope(&host, &roc_host, root, 3, testHostValueI64(10), testHostValueI64(10), key_cap, key_cap);
    const branch = host.internWhenBranchScope(row, 4, .true_branch);
    const row_state = host.internNodeIdentity(row, 0);
    const branch_state = host.internNodeIdentity(branch, 0);

    host.disposeScopeSubtree(&roc_host, row);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(!host.engine.scopes.items[@intCast(row)].active);
    try std.testing.expect(!host.engine.scopes.items[@intCast(branch)].active);
    try std.testing.expect(!host.engine.node_identities.items[@intCast(row_state)].active);
    try std.testing.expect(!host.engine.node_identities.items[@intCast(branch_state)].active);

    const recreated_row = createTestEachRowScope(&host, &roc_host, root, 3, testHostValueI64(10), testHostValueI64(10), key_cap, key_cap);
    try std.testing.expect(recreated_row != row);

    const recreated_state = host.internNodeIdentity(recreated_row, 0);
    try std.testing.expect(recreated_state != row_state);
}

test "signals host patches dirty leaf sinks without descriptor rebuild" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const root = testNodeStateWithTokenAndInitialCapability(
        &roc_host,
        state_token,
        testHostValueStr(&roc_host, "first"),
        testNodeTextSignalWithCapability(&roc_host, testNodeRefExpr(state_token), state_cap),
        state_cap,
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), initial_counts.set_text);
    try std.testing.expectEqual(@as(usize, 2), host.dom_elements.items.len);
    try std.testing.expectEqualStrings("first", host.dom_elements.items[1].text.?);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueStr(&roc_host, "second");
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 0), dirty_structural_signals.len);

    const patch_start = host.engine.render_metrics.patches_emitted;
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.total);
    try std.testing.expectEqual(patch_start + 1, host.engine.render_metrics.patches_emitted);
    try std.testing.expectEqual(@as(usize, 2), host.dom_elements.items.len);
    try std.testing.expectEqualStrings("second", host.dom_elements.items[1].text.?);

    const unchanged_generation = host.nextDirtySignalGeneration();
    const unchanged_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, unchanged_generation);
    defer host.hostAllocator().free(unchanged_record_ids);
    const unchanged_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, unchanged_record_ids, unchanged_generation);
    try std.testing.expectEqual(@as(u64, 0), unchanged_counts.total);
}

test "signals host prunes dirty leaf sink when retained map equality is unchanged" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const stable_label = testNodeStableStrMapExpr(&roc_host, testNodeRefExpr(state_token));
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testNodeTextSignal(&roc_host, stable_label),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.set_text);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64(2);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.engine.pending_roc_metrics.propagation_prunes;
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.engine.pending_roc_metrics.propagation_prunes);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);
}

test "signals host evaluates shared dirty record once per batch" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const shared_label = testNodeStableStrMapExpr(&roc_host, testNodeRefExpr(state_token));
    abi.increfNodeSignalExpr(shared_label, 1);
    const children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, shared_label),
        testNodeTextSignal(&roc_host, shared_label),
    };
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testElement(&roc_host, &children),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 2), initial_counts.set_text);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[2].text.?);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[3].text.?);
    try std.testing.expect(host.engine.active_stream.signal_text_nodes.items[0].signal.record == host.engine.active_stream.signal_text_nodes.items[1].signal.record);
    try std.testing.expectEqual(@as(usize, 2), host.engine.active_signal_graph.items.len);

    const shared_record = host.engine.active_stream.signal_text_nodes.items[0].signal.record;
    const shared_record_id = host.requireActiveSignalRecordId(shared_record);
    const source_record = switch (shared_record.payload) {
        .map => |payload| payload.input,
        else => failHost("shared label test expected a map signal record"),
    };
    const source_record_id = host.requireActiveSignalRecordId(source_record);
    try std.testing.expectEqual(@as(u64, 0), host.activeSignalRank(source_record_id));
    try std.testing.expectEqual(@as(u64, 1), host.activeSignalRank(shared_record_id));
    const expected_dependents = [_]u64{shared_record_id};
    try std.testing.expectEqualSlices(u64, &expected_dependents, host.dependentActiveSignalRecordIds(source_record_id));

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    try std.testing.expectEqual(@as(usize, @intCast(state_id + 1)), host.engine.active_source_signal_routes.items.len);
    const expected_source_routes = [_]u64{source_record_id};
    try std.testing.expectEqualSlices(u64, &expected_source_routes, host.engine.active_source_signal_routes.items[@intCast(state_id)].items);
    try std.testing.expectEqual(@as(usize, 2), host.engine.active_text_signal_routes.items[@intCast(shared_record_id)].items.len);

    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64(2);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.engine.pending_roc_metrics.propagation_prunes;
    const derived_start = host.engine.pending_roc_metrics.derived_calls_into_roc;
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.engine.pending_roc_metrics.propagation_prunes);
    try std.testing.expectEqual(derived_start + 1, host.engine.pending_roc_metrics.derived_calls_into_roc);
}

test "signals host skips parent transform when dirty child output is unchanged" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const stable_count = testNodeStableI64MapExpr(&roc_host, testNodeRefExpr(state_token), 42);
    const parent_label = testNodeStableStrMapExpr(&roc_host, stable_count);
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueI64(1),
        testNodeTextSignal(&roc_host, parent_label),
    );
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.set_text);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64(2);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.engine.pending_roc_metrics.propagation_prunes;
    const derived_start = host.engine.pending_roc_metrics.derived_calls_into_roc;
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const patch_counts = applyDirtyRenderSinks(&host, &roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.total);
    try std.testing.expectEqual(prune_start + 1, host.engine.pending_roc_metrics.propagation_prunes);
    try std.testing.expectEqual(derived_start + 1, host.engine.pending_roc_metrics.derived_calls_into_roc);
    try std.testing.expectEqualStrings("stable", host.dom_elements.items[1].text.?);
}

test "signals host prunes dirty combine output through cache-owned equality" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2) };
    const combine = testNodeCombineExpr(&roc_host, &.{});
    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    var binding = host.bindNodeSignal(host.hostAllocator(), &stream, combine, &.{});
    defer binding.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    var cache: HostSignalCacheSlot = .absent;
    cache.replace(&host, &roc_host, &host.engine.pending_roc_metrics, testHostValueI64List(&roc_host, &initial_items), hostSignalBindingCapability(&host, &binding));
    defer cache.deinit(&host, &roc_host, &host.engine.pending_roc_metrics);
    abi.decrefNodeSignalExpr(combine, &roc_host);

    const dirty_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(4) };
    const prune_start = host.engine.pending_roc_metrics.propagation_prunes;
    try std.testing.expect(!updateDirtySignalCache(&host, &roc_host, &cache, testHostValueI64List(&roc_host, &dirty_items)));
    try std.testing.expectEqual(prune_start + 1, host.engine.pending_roc_metrics.propagation_prunes);
}

test "signals host marks dirty structural sources for structural patching" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, testNodeRefExpr(state_token)),
                .read = testBoolReadHandle(&roc_host, state_cap),
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitialCapability(&roc_host, state_token, testHostValueBool(true), when_elem, state_cap);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueBool(false);
    host.engine.states.items[state_index].version += 1;
    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);
    try std.testing.expectEqual(HostScopeBranch.false_branch, dirty_structural_signals[0].branch.?);

    const patch_counts = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expect(activeTextElementId(&host, "true branch") == null);
    try std.testing.expect(activeTextElementId(&host, "false branch") != null);
}

test "signals host reuses active signal records while collecting dirty when branch" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const ready = testNodeBoolIdentityMapExpr(&roc_host, testNodeRefExpr(state_token));
    abi.increfNodeSignalExpr(ready, 1);
    const label = testNodeStableStrMapExpr(&roc_host, ready);
    const ready_cap = testNodeSignalExprCapabilityOrPanic(ready);
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, ready),
                .read = testBoolReadHandle(&roc_host, ready_cap),
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "loading")),
                .when_true = boxTestElem(&roc_host, testNodeTextSignal(&roc_host, label)),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueBool(false), when_elem);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expect(activeTextElementId(&host, "loading") != null);
    try std.testing.expect(activeTextElementId(&host, "stable") == null);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueBool(true);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);

    _ = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);

    try std.testing.expect(activeTextElementId(&host, "loading") == null);
    try std.testing.expect(activeTextElementId(&host, "stable") != null);
}

test "signals host prunes structural render when retained condition equality is unchanged" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const condition = testNodeStableBoolMapExpr(&roc_host, testNodeRefExpr(state_token));
    const condition_cap = testNodeSignalExprCapabilityOrPanic(condition);
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, condition),
                .read = testBoolReadHandle(&roc_host, condition_cap),
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(1), when_elem);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.engine.active_stream = stream;

    try std.testing.expect(activeTextElementId(&host, "true branch") != null);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64(2);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const prune_start = host.engine.pending_roc_metrics.propagation_prunes;

    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 0), dirty_structural_signals.len);
    try std.testing.expectEqual(prune_start + 1, host.engine.pending_roc_metrics.propagation_prunes);
    try std.testing.expect(activeTextElementId(&host, "true branch") != null);
    try std.testing.expect(activeTextElementId(&host, "false branch") == null);
}

test "signals host structural patch reorders keyed row DOM without recreating survivors" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial_children = [_]abi.Elem{
        testNodeEachWithItems(&roc_host, &initial_items),
    };
    const initial_root = testElementWith(&roc_host, "section", &.{}, &initial_children);
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 1), initial_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 4), initial_counts.create_element);
    try std.testing.expectEqual(@as(usize, 5), host.dom_elements.items.len);

    const section_id = host.engine.active_stream.elements.items[0].elem_id;
    const row_1_id = activeTextElementId(&host, "row-1-1") orelse unreachable;
    const row_2_id = activeTextElementId(&host, "row-2-2") orelse unreachable;
    const row_3_id = activeTextElementId(&host, "row-3-3") orelse unreachable;
    try std.testing.expectEqualSlices(u64, &.{ row_1_id, row_2_id, row_3_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const reordered_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered_children = [_]abi.Elem{
        testNodeEachWithItems(&roc_host, &reordered_items),
    };
    const reordered_root = testElementWith(&roc_host, "section", &.{}, &reordered_children);
    defer abi.decrefElem(reordered_root, &roc_host);

    var reordered_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &reordered_stream, reordered_root, &.{});
    const patch_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &reordered_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = reordered_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.remove_node);
    try std.testing.expect(patch_counts.move_before >= 2);
    try std.testing.expectEqual(@as(usize, 5), host.dom_elements.items.len);
    try std.testing.expectEqual(row_1_id, activeTextElementId(&host, "row-1-1") orelse unreachable);
    try std.testing.expectEqual(row_2_id, activeTextElementId(&host, "row-2-2") orelse unreachable);
    try std.testing.expectEqual(row_3_id, activeTextElementId(&host, "row-3-3") orelse unreachable);
    try std.testing.expectEqualSlices(u64, &.{ row_3_id, row_1_id, row_2_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const changed_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(4) };
    const changed_children = [_]abi.Elem{
        testNodeEachWithItems(&roc_host, &changed_items),
    };
    const changed_root = testElementWith(&roc_host, "section", &.{}, &changed_children);
    defer abi.decrefElem(changed_root, &roc_host);

    var changed_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &changed_stream, changed_root, &.{});
    const changed_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &changed_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = changed_stream;

    try std.testing.expectEqual(@as(u64, 4), test_row_elem_call_count);
    const row_4_id = activeTextElementId(&host, "row-4-4") orelse unreachable;
    try std.testing.expectEqual(@as(u64, 0), changed_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), changed_counts.create_element);
    try std.testing.expectEqual(row_2_id, activeTextElementId(&host, "row-2-2") orelse unreachable);
    try std.testing.expect(activeTextElementId(&host, "row-1-1") == null);
    try std.testing.expect(activeTextElementId(&host, "row-3-3") == null);
    try std.testing.expectEqualSlices(u64, &.{ row_2_id, row_4_id }, host.dom_elements.items[@intCast(section_id)].children.items);
}

test "signals host dirty each append patches only changed row" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const row_count = 24;
    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const each = testNodeEachWithSignalCapabilityAndRow(&roc_host, testNodeRefExpr(state_token), state_cap, &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    var initial_items: [row_count]HostValue = undefined;
    for (&initial_items, 0..) |*item, index| {
        item.* = testHostValueI64(@intCast(index + 1));
    }
    const root = testNodeStateWithTokenAndInitialCapability(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section, state_cap);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, row_count), test_row_elem_call_count);
    try std.testing.expect(activeTextElementId(&host, "row-24-24") != null);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;

    var next_items: [row_count + 1]HostValue = undefined;
    for (&next_items, 0..) |*item, index| {
        item.* = testHostValueI64(@intCast(index + 1));
    }
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64List(&roc_host, &next_items);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.engine.pending_roc_metrics.rows_reused;
    const rows_created_start = host.engine.pending_roc_metrics.rows_created;
    const rows_removed_start = host.engine.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.engine.render_metrics.patches_emitted;
    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, row_count), host.engine.pending_roc_metrics.rows_reused - rows_reused_start);
    try std.testing.expectEqual(@as(u64, 1), host.engine.pending_roc_metrics.rows_created - rows_created_start);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.rows_removed - rows_removed_start);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(row_call_start + 1, test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 3), patch_counts.total);
    try std.testing.expectEqual(patch_start + 3, host.engine.render_metrics.patches_emitted);
    try std.testing.expect(activeTextElementId(&host, "row-25-25") != null);
}

test "signals host dirty each reorder moves rows without recollecting bodies" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const each = testNodeEachWithSignalCapabilityAndRow(&roc_host, testNodeRefExpr(state_token), state_cap, &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const root = testNodeStateWithTokenAndInitialCapability(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section, state_cap);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    const section_id = host.engine.active_stream.elements.items[0].elem_id;
    const row_1_id = activeTextElementId(&host, "row-1-1") orelse unreachable;
    const row_2_id = activeTextElementId(&host, "row-2-2") orelse unreachable;
    const row_3_id = activeTextElementId(&host, "row-3-3") orelse unreachable;
    try std.testing.expectEqualSlices(u64, &.{ row_1_id, row_2_id, row_3_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;

    const reordered_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64List(&roc_host, &reordered_items);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.engine.pending_roc_metrics.rows_reused;
    const rows_created_start = host.engine.pending_roc_metrics.rows_created;
    const rows_removed_start = host.engine.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.engine.render_metrics.patches_emitted;
    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 3), host.engine.pending_roc_metrics.rows_reused - rows_reused_start);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.rows_created - rows_created_start);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.rows_removed - rows_removed_start);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(row_call_start, test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.remove_node);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.move_before);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.total);
    try std.testing.expectEqual(patch_start + 1, host.engine.render_metrics.patches_emitted);
    try std.testing.expectEqualSlices(u64, &.{ row_3_id, row_1_id, row_2_id }, host.dom_elements.items[@intCast(section_id)].children.items);
}

test "signals host dirty each mixed churn splices changed rows and moves survivors" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const each = testNodeEachWithSignalCapabilityAndRow(&roc_host, testNodeRefExpr(state_token), state_cap, &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const root = testNodeStateWithTokenAndInitialCapability(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section, state_cap);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 3), test_row_elem_call_count);
    const section_id = host.engine.active_stream.elements.items[0].elem_id;
    const row_1_id = activeTextElementId(&host, "row-1-1") orelse unreachable;
    const row_2_id = activeTextElementId(&host, "row-2-2") orelse unreachable;
    const row_3_id = activeTextElementId(&host, "row-3-3") orelse unreachable;
    try std.testing.expectEqualSlices(u64, &.{ row_1_id, row_2_id, row_3_id }, host.dom_elements.items[@intCast(section_id)].children.items);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;

    const mixed_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(4) };
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueI64List(&roc_host, &mixed_items);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.engine.pending_roc_metrics.rows_reused;
    const rows_created_start = host.engine.pending_roc_metrics.rows_created;
    const rows_removed_start = host.engine.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.engine.render_metrics.patches_emitted;
    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.rows_reused - rows_reused_start);
    try std.testing.expectEqual(@as(u64, 1), host.engine.pending_roc_metrics.rows_created - rows_created_start);
    try std.testing.expectEqual(@as(u64, 1), host.engine.pending_roc_metrics.rows_removed - rows_removed_start);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(row_call_start + 1, test_row_elem_call_count);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.remove_node);
    try std.testing.expectEqual(@as(u64, 2), patch_counts.move_before);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_text);
    try std.testing.expectEqual(@as(u64, 6), patch_counts.total);
    try std.testing.expectEqual(patch_start + 6, host.engine.render_metrics.patches_emitted);
    const row_4_id = activeTextElementId(&host, "row-4-4") orelse unreachable;
    try std.testing.expectEqual(row_1_id, activeTextElementId(&host, "row-1-1") orelse unreachable);
    try std.testing.expectEqual(row_3_id, activeTextElementId(&host, "row-3-3") orelse unreachable);
    try std.testing.expect(activeTextElementId(&host, "row-2-2") == null);
    try std.testing.expectEqualSlices(u64, &.{ row_3_id, row_1_id, row_4_id }, host.dom_elements.items[@intCast(section_id)].children.items);
}

test "signals host updates nested when without rebuilding unchanged row" {
    test_erased_callable_drop_count = 0;
    test_row_elem_call_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const items = [_]HostValue{testHostValueI64(1)};
    const children = [_]abi.Elem{
        testNodeEachWithNestedWhenRows(&roc_host, &items, state_token, state_cap),
    };
    const section = testElementWith(&roc_host, "section", &.{}, &children);
    const root = testNodeStateWithTokenAndInitialCapability(&roc_host, state_token, testHostValueBool(true), section, state_cap);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 1), test_row_elem_call_count);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-true") != null);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-false") == null);

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    const state_index = host.engine.stateIndexByNodeId(state_id) orelse unreachable;
    testDropHostValue(&roc_host, host.engine.states.items[state_index].cell.value);
    host.engine.states.items[state_index].cell.value = testHostValueBool(false);
    host.engine.states.items[state_index].version += 1;

    const dirty_source_node_ids = [_]u64{state_id};
    const dirty_generation = host.nextDirtySignalGeneration();
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, dirty_generation);
    defer host.hostAllocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.hostAllocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.hostAllocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);

    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;
    _ = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_generation, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.active_graph_records_rebuilt - graph_rebuild_start);
    try std.testing.expectEqual(@as(u64, 1), test_row_elem_call_count);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-true") == null);
    try std.testing.expect(activeTextElementId(&host, "row-1-1-false") != null);
}

test "signals host structural patch clears fields absent from reused DOM node" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .label, "Initial label"),
        testNodeStaticCustomTextAttr(&roc_host, "data-mode", "initial"),
        testNodeStaticBoolAttr(.disabled, true),
    };
    const initial_root = testElementWith(&roc_host, "section", &initial_attrs, &.{});
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    const section_id = host.engine.active_stream.elements.items[0].elem_id;
    try std.testing.expectEqualStrings("Initial label", host.dom_elements.items[@intCast(section_id)].label.?);
    try std.testing.expectEqualStrings("initial", elementTextAttr(&host.dom_elements.items[@intCast(section_id)], "data-mode").?);
    try std.testing.expect(host.dom_elements.items[@intCast(section_id)].disabled);

    const next_root = testElementWith(&roc_host, "section", &.{}, &.{});
    defer abi.decrefElem(next_root, &roc_host);

    var next_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &next_stream, next_root, &.{});
    const patch_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &next_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = next_stream;

    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 2), patch_counts.set_metadata);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_disabled);
    try std.testing.expect(host.dom_elements.items[@intCast(section_id)].label == null);
    try std.testing.expect(elementTextAttr(&host.dom_elements.items[@intCast(section_id)], "data-mode") == null);
    try std.testing.expect(!host.dom_elements.items[@intCast(section_id)].disabled);
}

test "signals host structural patch binds only changed event slots" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const initial_button_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .text, "Submit"),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const initial_children = [_]abi.Elem{
        testElementWith(&roc_host, "button", &initial_button_attrs, &.{}),
    };
    const initial_section = testElementWith(&roc_host, "section", &.{}, &initial_children);
    const initial_root = testNodeStateWithToken(&roc_host, state_token, initial_section);
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 1), initial_counts.bind_event);
    const button_id = host.engine.active_stream.elements.items[1].elem_id;
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(button_id)].bound_click_event);

    const same_button_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .text, "Submit"),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const same_children = [_]abi.Elem{
        testElementWith(&roc_host, "button", &same_button_attrs, &.{}),
    };
    const same_section = testElementWith(&roc_host, "section", &.{}, &same_children);
    const same_root = testNodeStateWithToken(&roc_host, cloneTestBinderToken(state_token), same_section);
    defer abi.decrefElem(same_root, &roc_host);

    var same_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &same_stream, same_root, &.{});
    const same_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &same_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = same_stream;

    try std.testing.expectEqual(@as(u64, 0), same_counts.bind_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(button_id)].bound_click_event);

    const removed_button_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .text, "Submit"),
    };
    const removed_children = [_]abi.Elem{
        testElementWith(&roc_host, "button", &removed_button_attrs, &.{}),
    };
    const removed_section = testElementWith(&roc_host, "section", &.{}, &removed_children);
    const removed_root = testNodeStateWithToken(&roc_host, cloneTestBinderToken(state_token), removed_section);
    defer abi.decrefElem(removed_root, &roc_host);

    var removed_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &removed_stream, removed_root, &.{});
    const removed_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &removed_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = removed_stream;

    try std.testing.expectEqual(@as(u64, 1), removed_counts.bind_event);
    try std.testing.expect(host.dom_elements.items[@intCast(button_id)].bound_click_event == null);
}

test "signals host structural patch shifts moved row event ids only" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2) };
    const initial_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &initial_items, &testStatefulRowButtonElemCallable),
    };
    const initial_root = testElementWith(&roc_host, "section", &.{}, &initial_children);
    defer abi.decrefElem(initial_root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &initial_stream, initial_root, &.{});
    const initial_counts = applyNodeDescriptorStream(&host, &roc_host, &initial_stream);
    host.engine.active_stream = initial_stream;

    try std.testing.expectEqual(@as(u64, 2), initial_counts.bind_event);
    const row_1_button_id = activeTextElementId(&host, "row-action-1-1") orelse unreachable;
    const row_2_button_id = activeTextElementId(&host, "row-action-2-2") orelse unreachable;
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);

    const reordered_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(1) };
    const reordered_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &reordered_items, &testStatefulRowButtonElemCallable),
    };
    const reordered_root = testElementWith(&roc_host, "section", &.{}, &reordered_children);
    defer abi.decrefElem(reordered_root, &roc_host);

    var reordered_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &reordered_stream, reordered_root, &.{});
    const reordered_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &reordered_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = reordered_stream;

    try std.testing.expectEqual(@as(u64, 0), reordered_counts.create_element);
    try std.testing.expectEqual(@as(u64, 2), reordered_counts.bind_event);
    try std.testing.expectEqual(row_1_button_id, activeTextElementId(&host, "row-action-1-1") orelse unreachable);
    try std.testing.expectEqual(row_2_button_id, activeTextElementId(&host, "row-action-2-2") orelse unreachable);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);

    const same_reordered_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(1) };
    const same_reordered_children = [_]abi.Elem{
        testNodeEachWithItemsAndRow(&roc_host, &same_reordered_items, &testStatefulRowButtonElemCallable),
    };
    const same_reordered_root = testElementWith(&roc_host, "section", &.{}, &same_reordered_children);
    defer abi.decrefElem(same_reordered_root, &roc_host);

    var same_reordered_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &same_reordered_stream, same_reordered_root, &.{});
    const same_reordered_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &same_reordered_stream);
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = same_reordered_stream;

    try std.testing.expectEqual(@as(u64, 0), same_reordered_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), same_reordered_counts.bind_event);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);
}

fn freeKeyedRowDiff(host: *HostEnv, diff: HostKeyedRowDiffResult) void {
    diff.deinit(host.hostAllocator());
}

fn syncTestEachRowScopes(host: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, key_cap: HostValueCapability, item_cap: HostValueCapability) HostKeyedRowDiffResult {
    const allocator = host.hostAllocator();
    const key_values = allocator.alloc(HostValue, keys.len) catch std.process.exit(1);
    defer allocator.free(key_values);
    const item_values = allocator.alloc(HostValue, items.len) catch std.process.exit(1);
    defer allocator.free(item_values);

    for (keys, key_values) |key, *dest| {
        dest.* = host.cloneHostValue(key);
    }
    for (items, item_values) |item, *dest| {
        dest.* = host.cloneHostValue(item);
    }
    for (keys) |key| {
        testDropHostValue(roc_host, key);
    }
    if (keys.ptr != items.ptr) {
        for (items) |item| {
            testDropHostValue(roc_host, item);
        }
    }

    const key_text = testHostValueKeyTextCallable(roc_host);
    defer abi.decrefErasedCallable(key_text, roc_host);
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_of, roc_host);
    const items_to_values = testItemsToValuesCallable(roc_host);
    defer abi.decrefErasedCallable(items_to_values, roc_host);
    const row = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testBinaryElemCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(row, roc_host);
    const ops: HostEachOps = .{
        .items_capability = item_cap,
        .item_capability = item_cap,
        .key_capability = key_cap,
        .items_to_values = items_to_values,
        .key_text = key_text,
        .key_of = key_of,
        .row = row,
    };
    return host.syncEachRowScopes(roc_host, parent_scope_id, site_ordinal, key_values, item_values, ops);
}

fn createTestEachRowScope(host: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, key: HostValue, item: HostValue, key_cap: HostValueCapability, item_cap: HostValueCapability) u64 {
    return host.createEachRowScope(parent_scope_id, site_ordinal, testHashHostValueKeyText(roc_host, key), key, item, key_cap, item_cap);
}

fn boxTestElem(roc_host: *abi.RocHost, elem: abi.Elem) *abi.Elem {
    const raw = abi.allocateBox(@sizeOf(abi.Elem), @alignOf(abi.Elem), true, roc_host);
    const boxed: *abi.Elem = @ptrCast(@alignCast(raw));
    boxed.* = elem;
    return boxed;
}

fn boxTestNodeSignalExpr(roc_host: *abi.RocHost, expr: abi.NodeSignalExpr) *abi.NodeSignalExpr {
    const raw = abi.allocateBox(@sizeOf(abi.NodeSignalExpr), @alignOf(abi.NodeSignalExpr), true, roc_host);
    const boxed: *abi.NodeSignalExpr = @ptrCast(@alignCast(raw));
    boxed.* = expr;
    return boxed;
}

fn newTestBinderToken(roc_host: *abi.RocHost) HostBinderToken {
    const token: *u64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, roc_host)));
    token.* = 0;
    return token;
}

fn cloneTestBinderToken(token: HostBinderToken) HostBinderToken {
    abi.increfBox(@ptrCast(token), 1);
    return token;
}

fn newTestSignalToken(roc_host: *abi.RocHost) HostSignalToken {
    const token: *u64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, roc_host)));
    token.* = 0;
    return token;
}

fn testNodeConstExpr(roc_host: *abi.RocHost, value: HostValue) abi.NodeSignalExpr {
    const cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{ .const_value = .{
            ._0 = newTestSignalToken(roc_host),
            ._1 = testHostValueInitialThunk(roc_host, value),
            ._2 = cap,
        } },
        .tag = .ConstValue,
    };
}

fn testNodeSignalExprCapability(signal: abi.NodeSignalExpr) ?HostValueCapability {
    return switch (signal.tag) {
        .ConstValue => signal.payload_const_value()._2,
        .Map => signal.payload_map()._3,
        .Map2 => signal.payload_map2()._4,
        .Combine => signal.payload_combine()._3,
        .TaskSource => signal.payload_task_source().cap,
        .IntervalSource => signal.payload_interval_source().cap,
        .Ref => null,
    };
}

fn testNodeSignalExprCapabilityOrPanic(signal: abi.NodeSignalExpr) HostValueCapability {
    return testNodeSignalExprCapability(signal) orelse @panic("test signal helper requires an explicit capability for Ref signals");
}

fn testTextReadHandle(roc_host: *abi.RocHost, cap: HostValueCapability) HostTextRead {
    return .{
        .capability = hv.retainHostValueCapability(cap),
        .read = testReadStrCallable(roc_host),
    };
}

fn testBoolReadHandle(roc_host: *abi.RocHost, cap: HostValueCapability) HostBoolRead {
    return .{
        .capability = hv.retainHostValueCapability(cap),
        .read = testReadBoolCallable(roc_host),
    };
}

fn testNodeRefExpr(binder_token: HostBinderToken) abi.NodeSignalExpr {
    return .{
        .payload = .{ .ref = cloneTestBinderToken(binder_token) },
        .tag = .Ref,
    };
}

fn testNodeMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 1 },
    );
    const cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = cap,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableStrMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableStrHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = cap,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableI64MapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr, value: i64) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableI64HostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = value },
    );
    const cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = cap,
            },
        },
        .tag = .Map,
    };
}

fn testNodeStableBoolMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStableBoolHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = cap,
            },
        },
        .tag = .Map,
    };
}

fn testNodeBoolIdentityMapExpr(roc_host: *abi.RocHost, input: abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testBoolIdentityHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = cap,
            },
        },
        .tag = .Map,
    };
}

fn testNodeCombineExpr(roc_host: *abi.RocHost, children: []const abi.NodeSignalExpr) abi.NodeSignalExpr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryIdentityHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const cap = testHostValueCapabilityWithEq(roc_host, &testAlwaysEqualHostValueCallable);
    return .{
        .payload = .{
            .combine = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = abi.RocList(abi.NodeSignalExpr).fromSlice(children, roc_host),
                ._2 = transform,
                ._3 = cap,
            },
        },
        .tag = .Combine,
    };
}

fn testNodeText(roc_host: *abi.RocHost, text: []const u8) abi.Elem {
    return .{
        .payload = .{ .text = RocStr.fromSlice(text, roc_host) },
        .tag = .Text,
    };
}

fn testNodeTextSignal(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr) abi.Elem {
    const cap = testNodeSignalExprCapabilityOrPanic(signal);
    return testNodeTextSignalWithCapability(roc_host, signal, cap);
}

fn testNodeTextSignalWithCapability(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr, cap: HostValueCapability) abi.Elem {
    return .{
        .payload = .{ .text_signal = .{
            .read = testTextReadHandle(roc_host, cap),
            .signal = boxTestNodeSignalExpr(roc_host, signal),
        } },
        .tag = .TextSignal,
    };
}

fn testNodeStaticTextAttr(roc_host: *abi.RocHost, field: RenderTextField, value: []const u8) abi.NodeAttr {
    return .{
        .payload = .{
            .static_text = .{
                .field = @intFromEnum(field),
                .name = RocStr.fromSlice("", roc_host),
                .value = RocStr.fromSlice(value, roc_host),
            },
        },
        .tag = .StaticText,
    };
}

fn testNodeStaticCustomTextAttr(roc_host: *abi.RocHost, name: []const u8, value: []const u8) abi.NodeAttr {
    return .{
        .payload = .{
            .static_text = .{
                .field = NodeFieldCustom,
                .name = RocStr.fromSlice(name, roc_host),
                .value = RocStr.fromSlice(value, roc_host),
            },
        },
        .tag = .StaticText,
    };
}

fn testNodeSignalTextAttr(roc_host: *abi.RocHost, field: RenderTextField, signal: abi.NodeSignalExpr) abi.NodeAttr {
    const cap = testNodeSignalExprCapabilityOrPanic(signal);
    return testNodeSignalTextAttrWithCapability(roc_host, field, signal, cap);
}

fn testNodeSignalTextAttrWithCapability(roc_host: *abi.RocHost, field: RenderTextField, signal: abi.NodeSignalExpr, cap: HostValueCapability) abi.NodeAttr {
    return .{
        .payload = .{
            .signal_text = .{
                .field = @intFromEnum(field),
                .name = RocStr.fromSlice("", roc_host),
                .read = testTextReadHandle(roc_host, cap),
                .signal = boxTestNodeSignalExpr(roc_host, signal),
            },
        },
        .tag = .SignalText,
    };
}

fn testNodeStaticBoolAttr(field: RenderBoolField, value: bool) abi.NodeAttr {
    return .{
        .payload = .{
            .static_bool = .{
                .field = @intFromEnum(field),
                .value = value,
            },
        },
        .tag = .StaticBool,
    };
}

fn testNodeSignalBoolAttr(roc_host: *abi.RocHost, field: RenderBoolField, signal: abi.NodeSignalExpr) abi.NodeAttr {
    const cap = testNodeSignalExprCapabilityOrPanic(signal);
    return testNodeSignalBoolAttrWithCapability(roc_host, field, signal, cap);
}

fn testNodeSignalBoolAttrWithCapability(roc_host: *abi.RocHost, field: RenderBoolField, signal: abi.NodeSignalExpr, cap: HostValueCapability) abi.NodeAttr {
    return .{
        .payload = .{
            .signal_bool = .{
                .field = @intFromEnum(field),
                .read = testBoolReadHandle(roc_host, cap),
                .signal = boxTestNodeSignalExpr(roc_host, signal),
            },
        },
        .tag = .SignalBool,
    };
}

fn testPayloadAccessorForKind(payload_kind: EventPayloadKind) EventPayloadAccessor {
    return switch (payload_kind) {
        .unit => .none,
        .str => .target_value,
        .bool => .target_checked,
        .bytes => .record_key_shift,
    };
}

fn testNodeEventAttr(roc_host: *abi.RocHost, kind: RenderEventKind, binder_token: HostBinderToken, payload_kind: EventPayloadKind) abi.NodeAttr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testBinaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const payload_cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_accessor = @intFromEnum(testPayloadAccessorForKind(payload_kind)),
                    .payload_kind = @intFromEnum(payload_kind),
                    .payload_reducer = .{
                        .capability = payload_cap,
                        .transform = transform,
                    },
                },
            },
        },
        .tag = .OnEvent,
    };
}

fn testNodeUnitIncrementEventAttr(roc_host: *abi.RocHost, kind: RenderEventKind, binder_token: HostBinderToken) abi.NodeAttr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnitIncrementHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const payload_cap = testHostValueCapability(roc_host);
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_accessor = @intFromEnum(EventPayloadAccessor.none),
                    .payload_kind = @intFromEnum(EventPayloadKind.unit),
                    .payload_reducer = .{
                        .capability = payload_cap,
                        .transform = transform,
                    },
                },
            },
        },
        .tag = .OnEvent,
    };
}

fn testElementWith(roc_host: *abi.RocHost, tag: []const u8, attrs: []const abi.NodeAttr, children: []const abi.Elem) abi.Elem {
    return .{
        .payload = .{
            .element = .{
                .attrs = abi.RocList(abi.NodeAttr).fromSlice(attrs, roc_host),
                .children = abi.RocList(abi.Elem).fromSlice(children, roc_host),
                .tag = RocStr.fromSlice(tag, roc_host),
            },
        },
        .tag = .Element,
    };
}

fn testElement(roc_host: *abi.RocHost, children: []const abi.Elem) abi.Elem {
    return testElementWith(roc_host, "div", &.{}, children);
}

fn testHostValueInitialThunk(roc_host: *abi.RocHost, initial: HostValue) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedHostValueCapture,
        roc_host,
        &testInitialHostValueCallable,
        &testHostValueCaptureOnDrop,
        .{ .value = initial },
    );
}

fn testNodeStateWithTokenAndInitial(roc_host: *abi.RocHost, binder_token: HostBinderToken, initial: HostValue, child: abi.Elem) abi.Elem {
    const cap = testHostValueCapability(roc_host);
    return testNodeStateWithTokenAndInitialCapability(roc_host, binder_token, initial, child, cap);
}

fn testNodeStateWithTokenAndInitialCapability(roc_host: *abi.RocHost, binder_token: HostBinderToken, initial: HostValue, child: abi.Elem, cap: HostValueCapability) abi.Elem {
    const initial_thunk = testHostValueInitialThunk(roc_host, initial);
    return .{
        .payload = .{
            .state = .{
                .binder = binder_token,
                .child = boxTestElem(roc_host, child),
                .cap = cap,
                .initial = initial_thunk,
            },
        },
        .tag = .State,
    };
}

fn testNodeStateWithToken(roc_host: *abi.RocHost, binder_token: HostBinderToken, child: abi.Elem) abi.Elem {
    return testNodeStateWithTokenAndInitial(roc_host, binder_token, testHostValueI64(0), child);
}

fn testNodeState(roc_host: *abi.RocHost, child: abi.Elem) abi.Elem {
    return testNodeStateWithToken(roc_host, newTestBinderToken(roc_host), child);
}

fn testNodeWhen(roc_host: *abi.RocHost, when_true: abi.Elem, when_false: abi.Elem) abi.Elem {
    const condition = testNodeConstExpr(roc_host, testHostValueBool(true));
    const condition_cap = testNodeSignalExprCapabilityOrPanic(condition);
    return .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, condition),
                .read = testBoolReadHandle(roc_host, condition_cap),
                .when_false = boxTestElem(roc_host, when_false),
                .when_true = boxTestElem(roc_host, when_true),
            },
        },
        .tag = .When,
    };
}

fn testHostValueI64List(roc_host: *abi.RocHost, items: []const HostValue) HostValue {
    const host = hostFromRocHost(roc_host);
    const values = I64List.allocate(items.len, roc_host);
    if (items.len > 0) {
        const dest = values.elements_ptr orelse unreachable;
        for (items, 0..) |item, index| {
            dest[index] = testReadHostValueI64(roc_host, item);
            testDropHostValue(roc_host, item);
        }
    }
    const payload: *I64List = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(I64List), @alignOf(I64List), true, roc_host)));
    payload.* = values;
    const host_value = host.storeHostValue(@ptrCast(payload));
    host.setTestHostValueKind(host_value, .i64_list);
    return capabilityTestHostValue(host, roc_host, host_value);
}

fn testNodeEachWithSignalAndRow(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr, row_fn: abi.RocErasedCallableFn) abi.Elem {
    const items_cap = testNodeSignalExprCapabilityOrPanic(signal);
    return testNodeEachWithSignalCapabilityAndRow(roc_host, signal, items_cap, row_fn);
}

fn testNodeEachWithSignalCapabilityAndRow(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr, items_cap: HostValueCapability, row_fn: abi.RocErasedCallableFn) abi.Elem {
    const key_cap = testHostValueCapability(roc_host);
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_text = testHostValueKeyTextCallable(roc_host);
    const item_cap = testHostValueCapability(roc_host);
    const items_to_values = testItemsToValuesCallable(roc_host);
    const row = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        row_fn,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .each = .{
                .items = boxTestNodeSignalExpr(roc_host, signal),
                .ops = .{
                    .items_capability = hv.retainHostValueCapability(items_cap),
                    .item_capability = item_cap,
                    .key_capability = key_cap,
                    .items_to_values = items_to_values,
                    .key_text = key_text,
                    .key_of = key_of,
                    .row = row,
                },
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithItemsAndRow(roc_host: *abi.RocHost, items: []const HostValue, row_fn: abi.RocErasedCallableFn) abi.Elem {
    return testNodeEachWithSignalAndRow(roc_host, testNodeConstExpr(roc_host, testHostValueI64List(roc_host, items)), row_fn);
}

fn testNodeEachWithNestedWhenRows(roc_host: *abi.RocHost, items: []const HostValue, condition_binder: HostBinderToken, condition_cap: HostValueCapability) abi.Elem {
    const key_cap = testHostValueCapability(roc_host);
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_text = testHostValueKeyTextCallable(roc_host);
    const item_cap = testHostValueCapability(roc_host);
    const items_to_values = testItemsToValuesCallable(roc_host);
    const row = writeTestErasedCallable(
        TestErasedBinderCapture,
        roc_host,
        &testNestedWhenRowElemCallable,
        &testBinderCaptureOnDrop,
        .{
            .condition_binder = cloneTestBinderToken(condition_binder),
            .condition_cap = hv.retainHostValueCapability(condition_cap),
        },
    );
    const items_signal = testNodeConstExpr(roc_host, testHostValueI64List(roc_host, items));
    const items_cap = testNodeSignalExprCapabilityOrPanic(items_signal);
    return .{
        .payload = .{
            .each = .{
                .items = boxTestNodeSignalExpr(roc_host, items_signal),
                .ops = .{
                    .items_capability = hv.retainHostValueCapability(items_cap),
                    .item_capability = item_cap,
                    .key_capability = key_cap,
                    .items_to_values = items_to_values,
                    .key_text = key_text,
                    .key_of = key_of,
                    .row = row,
                },
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithItems(roc_host: *abi.RocHost, items: []const HostValue) abi.Elem {
    return testNodeEachWithItemsAndRow(roc_host, items, &testStatefulRowElemCallable);
}

fn testNodeEach(roc_host: *abi.RocHost) abi.Elem {
    return testNodeEachWithItems(roc_host, &.{});
}

fn activeTextElementId(host: *const HostEnv, text: []const u8) ?u64 {
    for (host.dom_elements.items) |elem| {
        if (!elem.active) continue;
        const elem_text = elem.text orelse continue;
        if (std.mem.eql(u8, elem_text, text)) return elem.id;
    }
    return null;
}

test "signals host keyed row diff reuses creates and removes by typed key" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_cap = testHostValueCapability(&roc_host);
    defer abi.decrefHostValueCapabilityHandle(key_cap, &roc_host);

    const root = host.internRootScope();

    const initial_keys = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_updated);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    const reordered_keys = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered = syncTestEachRowScopes(&host, &roc_host, root, 5, &reordered_keys, &reordered_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, reordered);
    try std.testing.expectEqual(@as(u64, 3), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);
    try std.testing.expectEqual(@as(u64, 3), reordered.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), reordered.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[2], reordered.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[0], reordered.scope_ids[1]);
    try std.testing.expectEqual(initial.scope_ids[1], reordered.scope_ids[2]);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(reordered.scope_ids[2], 0));

    const changed_keys = [_]HostValue{ testHostValueI64(2), testHostValueI64(4) };
    const changed_items = [_]HostValue{ testHostValueI64(22), testHostValueI64(4) };
    const changed = syncTestEachRowScopes(&host, &roc_host, root, 5, &changed_keys, &changed_items, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, changed);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), changed.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 1), changed.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);

    const reappeared_keys = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(4) };
    const reappeared = syncTestEachRowScopes(&host, &roc_host, root, 5, &reappeared_keys, &reappeared_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, reappeared);
    try std.testing.expectEqual(@as(u64, 2), reappeared.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), reappeared.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reappeared.rows_removed);
    try std.testing.expectEqual(@as(u64, 1), reappeared.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 1), reappeared.row_items_updated);
    try std.testing.expect(reappeared.scope_ids[0] != initial.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[1], reappeared.scope_ids[1]);
    try std.testing.expectEqual(changed.scope_ids[1], reappeared.scope_ids[2]);

    try std.testing.expectEqual(@as(u64, 6), host.engine.pending_roc_metrics.rows_reused);
    try std.testing.expectEqual(@as(u64, 5), host.engine.pending_roc_metrics.rows_created);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.rows_removed);
}

test "signals host keyed row diff hash probes scale linearly" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_cap = testHostValueCapability(&roc_host);
    defer abi.decrefHostValueCapabilityHandle(key_cap, &roc_host);

    const root = host.internRootScope();
    const row_count = 64;

    var initial_keys: [row_count]HostValue = undefined;
    for (&initial_keys, 0..) |*key, index| {
        key.* = testHostValueI64(@intCast(index + 1));
    }
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, row_count), initial.rows_created);

    const metrics_start = host.engine.pending_roc_metrics;

    var reordered_keys: [row_count]HostValue = undefined;
    for (&reordered_keys, 0..) |*key, index| {
        key.* = testHostValueI64(@intCast(row_count - index));
    }
    const reordered = syncTestEachRowScopes(&host, &roc_host, root, 5, &reordered_keys, &reordered_keys, key_cap, key_cap);
    defer freeKeyedRowDiff(&host, reordered);
    try std.testing.expectEqual(@as(u64, row_count), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);

    const metrics_end = host.engine.pending_roc_metrics;
    const compare_delta = metrics_end.each_key_compares - metrics_start.each_key_compares;
    const hash_delta = metrics_end.each_key_hashes - metrics_start.each_key_hashes;
    const reuse_delta = metrics_end.each_key_reuse_compares - metrics_start.each_key_reuse_compares;
    const duplicate_delta = metrics_end.each_key_duplicate_compares - metrics_start.each_key_duplicate_compares;
    const item_delta = metrics_end.each_item_compares - metrics_start.each_item_compares;
    try std.testing.expectEqual(@as(u64, row_count * 2), compare_delta);
    try std.testing.expectEqual(@as(u64, row_count), hash_delta);
    try std.testing.expectEqual(@as(u64, row_count), reuse_delta);
    try std.testing.expectEqual(@as(u64, 0), duplicate_delta);
    try std.testing.expectEqual(@as(u64, row_count), item_delta);
}

test "signals host row scopes retain key and item capabilities" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_cap = testHostValueCapability(&roc_host);
    const item_cap = testHostValueCapability(&roc_host);

    const root = host.internRootScope();
    const initial_keys = [_]HostValue{testHostValueI64(1)};
    const initial_items = [_]HostValue{testHostValueI64(10)};
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_items, key_cap, item_cap);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 1), initial.rows_created);
    const row_scope_id = initial.scope_ids[0];

    test_erased_callable_drop_count = 0;
    abi.decrefHostValueCapabilityHandle(key_cap, &roc_host);
    abi.decrefHostValueCapabilityHandle(item_cap, &roc_host);
    try std.testing.expectEqual(@as(u64, 0), test_erased_callable_drop_count);

    const incoming_key_cap = testHostValueCapabilityWithEq(&roc_host, &testNeverEqualHostValueCallable);
    defer abi.decrefHostValueCapabilityHandle(incoming_key_cap, &roc_host);
    const incoming_item_cap = testHostValueCapabilityWithEq(&roc_host, &testNeverEqualHostValueCallable);
    defer abi.decrefHostValueCapabilityHandle(incoming_item_cap, &roc_host);

    const next_keys = [_]HostValue{testHostValueI64(1)};
    const next_items = [_]HostValue{testHostValueI64(10)};
    const next = syncTestEachRowScopes(&host, &roc_host, root, 5, &next_keys, &next_items, incoming_key_cap, incoming_item_cap);
    defer freeKeyedRowDiff(&host, next);
    try std.testing.expectEqual(@as(u64, 1), next.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), next.rows_created);
    try std.testing.expectEqual(@as(u64, 1), next.row_items_unchanged);
    try std.testing.expectEqual(row_scope_id, next.scope_ids[0]);
}

test "signals host collects Elem descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);

    const root_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .role, "region"),
        testNodeStaticTextAttr(&roc_host, .label, "Dashboard"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeConstExpr(&roc_host, testHostValueStr(&roc_host, "search"))),
        testNodeStaticBoolAttr(.disabled, true),
        testNodeSignalBoolAttr(&roc_host, .checked, testNodeConstExpr(&roc_host, testHostValueBool(false))),
    };
    const state_token = newTestBinderToken(&roc_host);
    const state_cap = testHostValueCapability(&roc_host);
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .test_id, "state-child"),
        testNodeSignalTextAttrWithCapability(&roc_host, .value, testNodeRefExpr(state_token), state_cap),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child_children = [_]abi.Elem{
        testNodeText(&roc_host, "state child"),
        testNodeTextSignalWithCapability(&roc_host, testNodeRefExpr(state_token), state_cap),
    };
    const state_child = testElementWith(&roc_host, "span", &state_child_attrs, &state_child_children);
    const state = testNodeStateWithTokenAndInitialCapability(&roc_host, state_token, testHostValueI64(0), state_child, state_cap);
    const when_elem = testNodeWhen(&roc_host, testNodeText(&roc_host, "true branch"), testNodeText(&roc_host, "false branch"));
    const each_elem = testNodeEach(&roc_host);
    const root_children = [_]abi.Elem{
        testNodeText(&roc_host, "intro"),
        testNodeTextSignal(&roc_host, testNodeConstExpr(&roc_host, testHostValueStr(&roc_host, "dynamic text"))),
        state,
        when_elem,
        each_elem,
    };
    const root = testElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});

    try std.testing.expectEqual(@as(usize, 2), stream.elements.items.len);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 0), stream.elements.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(u64, 0), stream.elements.items[0].scope_id);
    try std.testing.expectEqualStrings("section", stream.elements.items[0].tag);
    try std.testing.expectEqual(@as(u64, 4), stream.elements.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[1].parent_elem_id);
    try std.testing.expectEqualStrings("span", stream.elements.items[1].tag);

    try std.testing.expectEqual(@as(usize, 3), stream.text_nodes.items.len);
    try std.testing.expectEqual(@as(u64, 2), stream.text_nodes.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.text_nodes.items[0].parent_elem_id);
    try std.testing.expectEqualStrings("intro", stream.text_nodes.items[0].value);
    try std.testing.expectEqual(@as(u64, 5), stream.text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqualStrings("state child", stream.text_nodes.items[1].value);
    try std.testing.expectEqual(@as(u64, 7), stream.text_nodes.items[2].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.text_nodes.items[2].parent_elem_id);
    try std.testing.expectEqualStrings("true branch", stream.text_nodes.items[2].value);

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_nodes.items.len);
    try std.testing.expectEqual(@as(u64, 3), stream.signal_text_nodes.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.signal_text_nodes.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_text_nodes.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 6), stream.signal_text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .ref), std.meta.activeTag(stream.signal_text_nodes.items[1].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[1].signal.source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_nodes.items[1].signal.source_node_ids[0]);
    try std.testing.expectEqual(@as(u64, 8), stream.next_elem_id);

    try std.testing.expectEqual(@as(usize, 3), stream.static_text_attrs.items.len);
    try std.testing.expectEqual(RenderTextField.role, stream.static_text_attrs.items[0].field);
    try std.testing.expectEqualStrings("region", stream.static_text_attrs.items[0].value);
    try std.testing.expectEqual(RenderTextField.label, stream.static_text_attrs.items[1].field);
    try std.testing.expectEqualStrings("Dashboard", stream.static_text_attrs.items[1].value);
    try std.testing.expectEqual(RenderTextField.test_id, stream.static_text_attrs.items[2].field);
    try std.testing.expectEqualStrings("state-child", stream.static_text_attrs.items[2].value);

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_attrs.items.len);
    try std.testing.expectEqual(@as(u64, 1), stream.signal_text_attrs.items[0].elem_id);
    try std.testing.expectEqual(RenderTextField.value, stream.signal_text_attrs.items[0].field);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_text_attrs.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_attrs.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_attrs.items[1].elem_id);
    try std.testing.expectEqual(RenderTextField.value, stream.signal_text_attrs.items[1].field);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .ref), std.meta.activeTag(stream.signal_text_attrs.items[1].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_attrs.items[1].signal.source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_attrs.items[1].signal.source_node_ids[0]);

    try std.testing.expectEqual(@as(usize, 1), stream.static_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.disabled, stream.static_bool_attrs.items[0].field);
    try std.testing.expect(stream.static_bool_attrs.items[0].value);

    try std.testing.expectEqual(@as(usize, 1), stream.signal_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.checked, stream.signal_bool_attrs.items[0].field);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_bool_attrs.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_bool_attrs.items[0].signal.source_node_ids.len);

    try std.testing.expectEqual(@as(usize, 1), stream.events.items.len);
    try std.testing.expectEqual(RenderEventKind.click, stream.events.items[0].kind);
    try std.testing.expect(stream.events.items[0].binder_token == state_token);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.events.items[0].target_node_id);
    try std.testing.expectEqual(EventPayloadKind.unit, stream.events.items[0].payload_kind);

    try std.testing.expectEqual(@as(usize, 3), stream.scope_sites.items.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.state, stream.scope_sites.items[0].kind);
    try std.testing.expectEqual(@as(u64, 0), stream.scope_sites.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 1), stream.scope_sites.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[0].binder_bindings.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.when, stream.scope_sites.items[1].kind);
    try std.testing.expectEqual(@as(u64, 1), stream.scope_sites.items[1].ordinal);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[1].binder_bindings.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.each, stream.scope_sites.items[2].kind);
    try std.testing.expectEqual(@as(u64, 2), stream.scope_sites.items[2].ordinal);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[2].binder_bindings.len);

    try std.testing.expectEqual(@as(usize, 1), stream.states.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.states.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 1), stream.whens.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[1].node_id, stream.whens.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.whens.items[0].condition.source_node_ids.len);
    try std.testing.expectEqual(@as(usize, 1), stream.eaches.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[2].node_id, stream.eaches.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.eaches.items[0].items.source_node_ids.len);

    try std.testing.expectEqual(@as(usize, 3), host.engine.node_identities.items.len);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 1), host.engine.node_identities.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 2), host.engine.node_identities.items[2].ordinal);
}

test "signals host tracks descriptor stream closure lifecycle metrics" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};

    const state_token = newTestBinderToken(&roc_host);
    const root_attrs = [_]abi.NodeAttr{
        testNodeSignalTextAttr(&roc_host, .value, testNodeMapExpr(&roc_host, testNodeConstExpr(&roc_host, testHostValueI64(41)))),
    };
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child = testElementWith(&roc_host, "button", &state_child_attrs, &.{});
    const state = testNodeStateWithToken(&roc_host, state_token, state_child);
    const items = [_]HostValue{testHostValueI64(1)};
    const each = testNodeEachWithItems(&roc_host, &items);
    const root_children = [_]abi.Elem{ state, each };
    const root = testElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});

    try std.testing.expectEqual(@as(u64, 59), host.engine.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.closure_releases);

    stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);

    try std.testing.expectEqual(@as(u64, 59), host.engine.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 47), host.engine.pending_roc_metrics.closure_releases);
}

test "signals host descriptors carry capability-owned extension records" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);

    const root_attrs = [_]abi.NodeAttr{
        testNodeSignalTextAttr(&roc_host, .value, testNodeConstExpr(&roc_host, testHostValueStr(&roc_host, "search"))),
        testNodeSignalBoolAttr(&roc_host, .checked, testNodeConstExpr(&roc_host, testHostValueBool(true))),
    };
    const state_token = newTestBinderToken(&roc_host);
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child = testElementWith(&roc_host, "button", &state_child_attrs, &.{});
    const root_children = [_]abi.Elem{
        testNodeStateWithToken(&roc_host, state_token, state_child),
        testNodeWhen(&roc_host, testNodeText(&roc_host, "ready"), testNodeText(&roc_host, "waiting")),
        testNodeEach(&roc_host),
    };
    const root = testElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});

    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_attrs.items.len);
    const text_attr = &stream.signal_text_attrs.items[0];
    try std.testing.expect(hv.hostValueCapabilitiesMatch(text_attr.read.capability, hostSignalBindingCapability(&host, &text_attr.signal)));

    try std.testing.expectEqual(@as(usize, 1), stream.signal_bool_attrs.items.len);
    const bool_attr = &stream.signal_bool_attrs.items[0];
    try std.testing.expect(hv.hostValueCapabilitiesMatch(bool_attr.read.capability, hostSignalBindingCapability(&host, &bool_attr.signal)));

    try std.testing.expectEqual(@as(usize, 1), stream.whens.items.len);
    const when = &stream.whens.items[0];
    try std.testing.expect(hv.hostValueCapabilitiesMatch(when.read.capability, hostSignalBindingCapability(&host, &when.condition)));

    try std.testing.expectEqual(@as(usize, 1), stream.eaches.items.len);
    const each = &stream.eaches.items[0];
    try std.testing.expect(hv.hostValueCapabilitiesMatch(each.ops.items_capability, hostSignalBindingCapability(&host, &each.items)));

    try std.testing.expectEqual(@as(usize, 1), stream.events.items.len);
    const event_reducer = stream.events.items[0].payload_reducer;
    try std.testing.expect(stream.events.items[0].owns_payload_reducer);
    host.rebuildActiveEventsFromStream(&stream);
    try std.testing.expect(!stream.events.items[0].owns_payload_reducer);
    try std.testing.expectEqual(@as(usize, 1), host.engine.active_events.items.len);
    try std.testing.expect(hv.hostValueCapabilitiesMatch(host.engine.active_events.items[0].payload_reducer.capability, event_reducer.capability));
    try std.testing.expectEqual(host.engine.active_events.items[0].payload_reducer.transform, event_reducer.transform);
}

test "signals host preserves explicit signal tokens across cloned descriptors" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);

    const signal = testNodeMapExpr(&roc_host, testNodeConstExpr(&roc_host, testHostValueI64(41)));
    abi.increfNodeSignalExpr(signal, 1);
    const root_children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, signal),
        testNodeTextSignal(&roc_host, signal),
    };
    const root = testElement(&roc_host, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_nodes.items.len);
    const first = stream.signal_text_nodes.items[0].signal.record;
    const second = stream.signal_text_nodes.items[1].signal.record;
    try std.testing.expect(first == second);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .map), std.meta.activeTag(first.payload));
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .map), std.meta.activeTag(second.payload));
    try std.testing.expect(first.payload.map.token == second.payload.map.token);
}

test "signals host retains state equality outside descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(0), testNodeText(&roc_host, "state"));
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    host.engine.active_stream = stream;

    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;
    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = .{};

    try std.testing.expect(!host.updateStateValue(&roc_host, state_id, testHostValueI64(0)));
    try std.testing.expect(host.updateStateValue(&roc_host, state_id, testHostValueI64(1)));
}

test "signals host dispatches through active event records outside descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const attrs = [_]abi.NodeAttr{
        testNodeUnitIncrementEventAttr(&roc_host, .click, state_token),
    };
    const button = testElementWith(&roc_host, "button", &attrs, &.{});
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(0), button);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.rebuildActiveEventsFromStream(&stream);
    host.engine.active_stream = stream;

    const button_id = host.engine.active_stream.elements.items[0].elem_id;
    const event_id = host.dom_elements.items[@intCast(button_id)].bound_click_event orelse unreachable;
    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;

    host.engine.active_stream.deinit(host.hostAllocator(), &host, &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = .{};

    dispatchRocEvent(&host, &roc_host, event_id, .unit, testHostValueUnit());
    try expectHostValueI64(host.stateValueByNodeId(state_id), 1);
}

test "signals host keeps live allocations flat across repeated events" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        host.deinit();
        _ = host.gpa.deinit();
    }

    const state_token = newTestBinderToken(&roc_host);
    const attrs = [_]abi.NodeAttr{
        testNodeUnitIncrementEventAttr(&roc_host, .click, state_token),
    };
    const button = testElementWith(&roc_host, "button", &attrs, &.{});
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64(0), button);
    defer abi.decrefElem(root, &roc_host);

    var stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &stream, root, &.{});
    _ = applyNodeDescriptorStream(&host, &roc_host, &stream);
    host.rebuildActiveEventsFromStream(&stream);
    host.engine.active_stream = stream;

    const button_id = host.engine.active_stream.elements.items[0].elem_id;
    const event_id = host.dom_elements.items[@intCast(button_id)].bound_click_event orelse unreachable;
    const state_id = host.engine.active_stream.scope_sites.items[0].node_id;

    var live_after_warmup: ?i64 = null;
    var iteration: usize = 0;
    while (iteration < 100) : (iteration += 1) {
        dispatchRocEvent(&host, &roc_host, event_id, .unit, hostValueUnit(&host, &roc_host));
        const live = @as(i64, @intCast(host.alloc_count)) - @as(i64, @intCast(host.dealloc_count));
        if (iteration == 9) {
            live_after_warmup = live;
        } else if (iteration > 9) {
            try std.testing.expectEqual(live_after_warmup.?, live);
        }
    }

    try expectHostValueI64(host.stateValueByNodeId(state_id), 100);
}
