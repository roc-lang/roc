//! Shared reactive-engine internals for the Signals hosts.
//!
//! This module is the home for the host-agnostic reactive engine that
//! `native_host.zig` and `wasm_host.zig` will both drive (see the G-B0 plan).
//! It starts with the retained "HostValue / thunk / scope" adapter: the value
//! cell that owns a boxed Roc value plus its equality/drop thunks, and the
//! each-row scope step that carries those cells through the scope forest.
//!
//! Host-specific concerns stay out: methods take the active `*abi.RocHost`
//! explicitly, the metrics sink is a duck-typed `anytype` (so a host can pass a
//! real `RuntimeMetrics` or a zero-size `NoMetrics`), and cloning a HostValue is
//! delegated to a `ctx` the host supplies (`ctx.cloneHostValue`).

const std = @import("std");
const builtin = @import("builtin");
const abi = @import("roc_platform_abi.zig");
const scope_tree = @import("scope_tree.zig");
const erased_calls = @import("erased_calls.zig");
const render = @import("render_commands.zig");
const signal_graph = @import("signal_graph.zig");
const identity_table = @import("identity_table.zig");
const host_value_registry = @import("host_value_registry.zig");
const keyed_rows = @import("keyed_rows.zig");
const hv = @import("host_values.zig");

pub const RenderTextField = render.TextField;
pub const RenderBoolField = render.BoolField;
pub const RenderEventKind = render.EventKind;
pub const EventPayloadAccessor = render.EventPayloadAccessor;

pub const HostValue = u64;
pub const HostValueList = abi.RocListWith(HostValue, false);

const render_event_kinds = [_]RenderEventKind{ .click, .input, .check, .pointer_down, .pointer_up, .pointer_enter, .pointer_leave };

const RenderScalarNodeCache = struct {
    active: bool = false,
    tag: ?[]const u8 = null,
    parent_id: ?u64 = null,
    children: std.ArrayListUnmanaged(u64) = .empty,
    bound_click_event: ?u64 = null,
    bound_click_accessor: ?EventPayloadAccessor = null,
    bound_input_event: ?u64 = null,
    bound_input_accessor: ?EventPayloadAccessor = null,
    bound_check_event: ?u64 = null,
    bound_check_accessor: ?EventPayloadAccessor = null,
    bound_pointer_down_event: ?u64 = null,
    bound_pointer_down_accessor: ?EventPayloadAccessor = null,
    bound_pointer_up_event: ?u64 = null,
    bound_pointer_up_accessor: ?EventPayloadAccessor = null,
    bound_pointer_enter_event: ?u64 = null,
    bound_pointer_enter_accessor: ?EventPayloadAccessor = null,
    bound_pointer_leave_event: ?u64 = null,
    bound_pointer_leave_accessor: ?EventPayloadAccessor = null,
    text: ?[]const u8 = null,
    role: ?[]const u8 = null,
    label: ?[]const u8 = null,
    test_id: ?[]const u8 = null,
    value: ?[]const u8 = null,
    class: ?[]const u8 = null,
    checked: ?bool = null,
    disabled: ?bool = null,

    fn deinit(self: *RenderScalarNodeCache, allocator: std.mem.Allocator) void {
        if (self.tag) |tag| allocator.free(tag);
        if (self.text) |text| allocator.free(text);
        if (self.role) |role| allocator.free(role);
        if (self.label) |label| allocator.free(label);
        if (self.test_id) |test_id| allocator.free(test_id);
        if (self.value) |value| allocator.free(value);
        if (self.class) |class| allocator.free(class);
        self.children.deinit(allocator);
        self.* = .{};
    }

    fn initActive(allocator: std.mem.Allocator, tag: []const u8) RenderScalarNodeCache {
        return .{
            .active = true,
            .tag = allocator.dupe(u8, tag) catch @panic("out of memory"),
        };
    }

    fn textSlot(self: *RenderScalarNodeCache, field: RenderTextField) *?[]const u8 {
        return switch (field) {
            .text => &self.text,
            .role => &self.role,
            .label => &self.label,
            .test_id => &self.test_id,
            .value => &self.value,
            .class => &self.class,
        };
    }

    fn boolSlot(self: *RenderScalarNodeCache, field: RenderBoolField) *?bool {
        return switch (field) {
            .checked => &self.checked,
            .disabled => &self.disabled,
        };
    }

    fn eventSlot(self: *RenderScalarNodeCache, kind: RenderEventKind) *?u64 {
        return switch (kind) {
            .click => &self.bound_click_event,
            .input => &self.bound_input_event,
            .check => &self.bound_check_event,
            .pointer_down => &self.bound_pointer_down_event,
            .pointer_up => &self.bound_pointer_up_event,
            .pointer_enter => &self.bound_pointer_enter_event,
            .pointer_leave => &self.bound_pointer_leave_event,
        };
    }

    fn eventAccessorSlot(self: *RenderScalarNodeCache, kind: RenderEventKind) *?EventPayloadAccessor {
        return switch (kind) {
            .click => &self.bound_click_accessor,
            .input => &self.bound_input_accessor,
            .check => &self.bound_check_accessor,
            .pointer_down => &self.bound_pointer_down_accessor,
            .pointer_up => &self.bound_pointer_up_accessor,
            .pointer_enter => &self.bound_pointer_enter_accessor,
            .pointer_leave => &self.bound_pointer_leave_accessor,
        };
    }
};

pub const RuntimeMetrics = struct {
    active_graph_records_rebuilt: u64,
    append_child: u64,
    allocs_this_event: u64,
    bind_event: u64,
    closure_releases: u64,
    closure_retains: u64,
    create_element: u64,
    deallocs_this_event: u64,
    derived_calls_into_roc: u64,
    each_key_compares: u64,
    events_processed: u64,
    move_before: u64,
    nodes_recomputed: u64,
    patches_emitted: u64,
    propagation_prunes: u64,
    recompute_batches: u64,
    remove_node: u64,
    retained_alloc_delta: i64,
    reset_dom: u64,
    rows_created: u64,
    rows_removed: u64,
    rows_reused: u64,
    scopes_created: u64,
    scopes_disposed: u64,
    set_checked: u64,
    set_disabled: u64,
    set_metadata: u64,
    set_text: u64,
    set_value: u64,
    stream_nodes_scanned: u64,

    pub const Field = std.meta.FieldEnum(@This());

    pub inline fn bump(self: *RuntimeMetrics, comptime field: Field, n: u64) void {
        @field(self, @tagName(field)) += n;
    }
};

pub fn zeroRuntimeMetrics() RuntimeMetrics {
    return .{
        .active_graph_records_rebuilt = 0,
        .append_child = 0,
        .allocs_this_event = 0,
        .bind_event = 0,
        .closure_releases = 0,
        .closure_retains = 0,
        .create_element = 0,
        .deallocs_this_event = 0,
        .derived_calls_into_roc = 0,
        .each_key_compares = 0,
        .events_processed = 0,
        .move_before = 0,
        .nodes_recomputed = 0,
        .patches_emitted = 0,
        .propagation_prunes = 0,
        .recompute_batches = 0,
        .remove_node = 0,
        .retained_alloc_delta = 0,
        .reset_dom = 0,
        .rows_created = 0,
        .rows_removed = 0,
        .rows_reused = 0,
        .scopes_created = 0,
        .scopes_disposed = 0,
        .set_checked = 0,
        .set_disabled = 0,
        .set_metadata = 0,
        .set_text = 0,
        .set_value = 0,
        .stream_nodes_scanned = 0,
    };
}

/// Zero-size stand-in for `RuntimeMetrics`: every `bump` is a comptime no-op.
pub const NoMetrics = struct {
    pub const Field = RuntimeMetrics.Field;

    pub inline fn bump(_: *NoMetrics, comptime _: Field, _: u64) void {}
};

/// Dispatch counters, accumulated per host event and folded into the finalized
/// runtime metrics. Engine-owned so both hosts share one dispatch path.
pub const DispatchMetrics = struct {
    events_processed: u64 = 0,
    recompute_batches: u64 = 0,
};

fn verifyDeclFn(comptime owner_name: []const u8, comptime Owner: type, comptime decl_name: []const u8, comptime params: anytype, comptime return_type: type) void {
    if (!@hasDecl(Owner, decl_name)) {
        @compileError(owner_name ++ " is missing " ++ decl_name);
    }

    const fn_type = @TypeOf(@field(Owner, decl_name));
    const type_info = @typeInfo(fn_type);
    if (type_info != .@"fn") {
        @compileError(owner_name ++ "." ++ decl_name ++ " must be a function");
    }

    const fn_info = type_info.@"fn";
    if (fn_info.params.len != params.len) {
        @compileError(owner_name ++ "." ++ decl_name ++ " has the wrong parameter count");
    }
    inline for (params, 0..) |expected, index| {
        const actual = fn_info.params[index].type orelse {
            @compileError(owner_name ++ "." ++ decl_name ++ " must not use anytype parameters");
        };
        if (actual != expected) {
            @compileError(owner_name ++ "." ++ decl_name ++ " has an incompatible parameter type");
        }
    }
    const actual_return = fn_info.return_type orelse void;
    if (actual_return != return_type) {
        @compileError(owner_name ++ "." ++ decl_name ++ " has an incompatible return type");
    }
}

fn verifyTypeDecl(comptime owner_name: []const u8, comptime Owner: type, comptime decl_name: []const u8) void {
    if (!@hasDecl(Owner, decl_name)) {
        @compileError(owner_name ++ " is missing " ++ decl_name);
    }
    if (@TypeOf(@field(Owner, decl_name)) != type) {
        @compileError(owner_name ++ "." ++ decl_name ++ " must be a type");
    }
}

pub fn verifyRegistryOps(comptime Ops: type) void {
    verifyDeclFn("engine RegistryOps", Ops, "retainCapability", .{ Ops, HostValueCapability }, void);
    verifyDeclFn("engine RegistryOps", Ops, "releaseCapability", .{ Ops, HostValueCapability }, void);
    verifyDeclFn("engine RegistryOps", Ops, "capabilitiesMatch", .{ Ops, HostValueCapability, HostValueCapability }, bool);
    verifyDeclFn("engine RegistryOps", Ops, "capabilityIsActive", .{ Ops, HostValueCapability }, bool);
    verifyDeclFn("engine RegistryOps", Ops, "cloneValueWithCapability", .{ Ops, HostValue, HostValueCapability }, HostValue);
    verifyDeclFn("engine RegistryOps", Ops, "callHostValueToHostValueWithCapability", .{ Ops, HostValueCapability, abi.RocErasedCallable, HostValue }, HostValue);
    verifyDeclFn("engine RegistryOps", Ops, "splitBoxWithSplit", .{ Ops, abi.RocBox, abi.RocErasedCallable }, erased_calls.RocBoxPair);
}

pub fn verifySink(comptime Sink: type) void {
    verifyDeclFn("engine Sink", Sink, "reset", .{Sink}, void);
    verifyDeclFn("engine Sink", Sink, "appendNode", .{ Sink, u64, u64, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "ensureNode", .{ Sink, u64, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "removeNode", .{ Sink, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "replaceChildren", .{ Sink, u64, []const u64 }, void);
    verifyDeclFn("engine Sink", Sink, "replaceChildrenForMoves", .{ Sink, u64, []const u64 }, void);
    verifyDeclFn("engine Sink", Sink, "applyTextField", .{ Sink, u64, RenderTextField, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "applyBoolField", .{ Sink, u64, RenderBoolField, bool }, void);
    verifyDeclFn("engine Sink", Sink, "clearTextField", .{ Sink, u64, RenderTextField }, void);
    verifyDeclFn("engine Sink", Sink, "clearBoolField", .{ Sink, u64, RenderBoolField }, void);
    verifyDeclFn("engine Sink", Sink, "bindEventKind", .{ Sink, u64, RenderEventKind, u64, EventPayloadAccessor }, void);
    verifyDeclFn("engine Sink", Sink, "clearEvent", .{ Sink, u64, RenderEventKind }, void);
    verifyDeclFn("engine Sink", Sink, "startInterval", .{ Sink, u64, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "cancelInterval", .{ Sink, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "startTask", .{ Sink, u64, []const u8, []const u8 }, void);
    verifyDeclFn("engine Sink", Sink, "cancelTask", .{ Sink, u64 }, void);
    verifyDeclFn("engine Sink", Sink, "debugAssertNode", .{ Sink, u64, bool, ?[]const u8, ?u64, []const u64, ?u64, ?u64, ?u64, ?u64, ?u64, ?u64, ?u64 }, void);
}

pub fn verifyMetrics(comptime Metrics: type) void {
    verifyDeclFn("engine Metrics", Metrics, "bump", .{ *Metrics, RuntimeMetrics.Field, u64 }, void);
}

pub fn verifyCtx(comptime Ctx: type) void {
    verifyTypeDecl("engine Ctx", Ctx, "Handle");
    verifyTypeDecl("engine Ctx", Ctx, "RegistryOps");
    verifyTypeDecl("engine Ctx", Ctx, "Metrics");
    verifyTypeDecl("engine Ctx", Ctx, "Sink");

    verifyDeclFn("engine Ctx", Ctx, "zeroMetrics", .{}, Ctx.Metrics);
    verifyDeclFn("engine Ctx", Ctx, "allocator", .{Ctx.Handle}, std.mem.Allocator);
    verifyDeclFn("engine Ctx", Ctx, "cloneHostValue", .{ Ctx.Handle, HostValue }, HostValue);
    verifyDeclFn("engine Ctx", Ctx, "pushHostValueCapabilities", .{ Ctx.Handle, []const HostValueCapability }, void);
    verifyDeclFn("engine Ctx", Ctx, "popHostValueCapabilities", .{Ctx.Handle}, void);
    verifyDeclFn("engine Ctx", Ctx, "stateValueByNodeId", .{ Ctx.Handle, u64 }, HostValue);
    verifyDeclFn("engine Ctx", Ctx, "stateCapability", .{ Ctx.Handle, u64 }, HostValueCapability);
    verifyDeclFn("engine Ctx", Ctx, "sink", .{Ctx.Handle}, Ctx.Sink);
    verifyRegistryOps(Ctx.RegistryOps);
    verifyMetrics(Ctx.Metrics);
    verifySink(Ctx.Sink);
}

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

pub fn appendUniqueU64(allocator: std.mem.Allocator, values: *std.ArrayListUnmanaged(u64), value: u64) void {
    if (u64SliceContains(values.items, value)) return;
    values.append(allocator, value) catch @panic("out of memory");
}

fn u64SliceIndex(items: []const u64, target: u64) ?usize {
    for (items, 0..) |item, index| {
        if (item == target) return index;
    }
    return null;
}

fn stableSubsequenceLength(indexes: []const usize, scratch: []usize) usize {
    var len: usize = 0;
    for (indexes) |index| {
        var low: usize = 0;
        var high = len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (scratch[mid] < index) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        scratch[low] = index;
        if (low == len) len += 1;
    }
    return len;
}

/// A retained Roc value plus the capability that owns its equality/drop
/// operations. Holds exactly one refcount on the capability while live.
pub const HostValueCell = struct {
    value: HostValue,
    cap: HostValueCapability,

    pub fn initRetained(value: HostValue, cap: HostValueCapability, metrics: anytype) HostValueCell {
        _ = retainHostValueCapability(cap, metrics);
        return .{ .value = value, .cap = cap };
    }

    /// Clone the cell, retaining the capability and cloning the boxed value through
    /// `ctx.cloneHostValue` (which the host implements with its registry).
    pub fn cloneRetained(self: HostValueCell, ctx: anytype, metrics: anytype) HostValueCell {
        const value = ctx.cloneHostValue(self.value);
        _ = retainHostValueCapability(self.cap, metrics);
        return .{ .value = value, .cap = self.cap };
    }

    pub fn deinit(self: *HostValueCell, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(self.cap), self.value);
        releaseHostValueCapability(self.cap, roc_host, metrics);
        self.* = undefined;
    }

    pub fn valueEquals(self: *const HostValueCell, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) bool {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        return erased_calls.callErasedHostValueHostValueToBool(roc_host, hv.hostValueCapabilityEq(self.cap), self.value, value);
    }

    pub fn dropIncoming(self: *const HostValueCell, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) void {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(self.cap), value);
    }

    pub fn replaceValue(self: *HostValueCell, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) void {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(self.cap), self.value);
        self.value = value;
    }
};

/// Per-row payload carried in an `Ui.each` scope: the row's key and item cells,
/// keyed by the construction-site ordinal.
pub const HostEachRowScopeStep = struct {
    site_ordinal: u64,
    key: HostValueCell,
    item: HostValueCell,
};

pub const HostScopeStep = scope_tree.Step(HostEachRowScopeStep);
pub const HostScope = scope_tree.Scope(HostEachRowScopeStep);

/// Drop the retained cells owned by an each-row scope step (no-op for the
/// structural scope kinds, which carry no Roc values).
pub fn deinitHostScopeStep(step: *HostScopeStep, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
    switch (step.*) {
        .each_row => |*row| {
            row.key.deinit(ctx, roc_host, metrics);
            row.item.deinit(ctx, roc_host, metrics);
        },
        .root, .component, .when_branch => {},
    }
}

/// Retain one refcount on a Roc thunk the host is about to store.
pub fn retainHostCallable(callable: abi.RocErasedCallable, metrics: anytype) abi.RocErasedCallable {
    abi.increfErasedCallable(callable, 1);
    metrics.bump(.closure_retains, 1);
    return callable;
}

pub const HostValueCapability = hv.HostValueCapabilityHandle;
pub const HostTextRead = abi.HostValueTextReadHandle;
pub const HostBoolRead = abi.HostValueBoolReadHandle;
pub const HostEventReducer = abi.HostValueEventReducerHandle;
pub const HostTaskRequestRead = abi.HostValueTaskRequestReadHandle;
pub const HostEachOps = abi.__AnonStruct58;

/// Identity token interned per signal expression node.
pub const HostSignalToken = *u64;

pub fn retainHostSignalToken(token: HostSignalToken) HostSignalToken {
    abi.increfBox(@ptrCast(token), 1);
    return token;
}

fn releaseHostSignalToken(token: HostSignalToken, roc_host: *abi.RocHost) void {
    hv.releaseU64Box(token, roc_host);
}

fn retainHostValueCapability(capability: HostValueCapability, metrics: anytype) HostValueCapability {
    metrics.bump(.closure_retains, 3);
    return hv.retainHostValueCapability(capability);
}

fn releaseHostValueCapability(capability: HostValueCapability, roc_host: *abi.RocHost, metrics: anytype) void {
    metrics.bump(.closure_releases, 3);
    hv.releaseHostValueCapability(capability, roc_host);
}

fn assertHostValueCapabilitiesMatch(actual: HostValueCapability, expected: HostValueCapability, message: []const u8) void {
    if (!hv.hostValueCapabilitiesMatch(actual, expected)) @panic(message);
}

fn retainHostTextRead(read: HostTextRead, metrics: anytype) HostTextRead {
    _ = retainHostValueCapability(read.capability, metrics);
    abi.increfErasedCallable(read.read, 1);
    metrics.bump(.closure_retains, 1);
    return read;
}

fn releaseHostTextRead(read: HostTextRead, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(read.capability, roc_host, metrics);
    abi.decrefErasedCallable(read.read, roc_host);
    metrics.bump(.closure_releases, 1);
}

fn retainHostBoolRead(read: HostBoolRead, metrics: anytype) HostBoolRead {
    _ = retainHostValueCapability(read.capability, metrics);
    abi.increfErasedCallable(read.read, 1);
    metrics.bump(.closure_retains, 1);
    return read;
}

fn releaseHostBoolRead(read: HostBoolRead, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(read.capability, roc_host, metrics);
    abi.decrefErasedCallable(read.read, roc_host);
    metrics.bump(.closure_releases, 1);
}

fn retainHostEventReducer(reducer: HostEventReducer, metrics: anytype) HostEventReducer {
    _ = retainHostValueCapability(reducer.capability, metrics);
    abi.increfErasedCallable(reducer.transform, 1);
    metrics.bump(.closure_retains, 1);
    return reducer;
}

fn releaseHostEventReducer(reducer: HostEventReducer, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(reducer.capability, roc_host, metrics);
    abi.decrefErasedCallable(reducer.transform, roc_host);
    metrics.bump(.closure_releases, 1);
}

fn retainHostEachOps(ops: HostEachOps, metrics: anytype) HostEachOps {
    _ = retainHostValueCapability(ops.items_capability, metrics);
    _ = retainHostValueCapability(ops.item_capability, metrics);
    _ = retainHostValueCapability(ops.key_capability, metrics);
    abi.increfErasedCallable(ops.items_to_values, 1);
    abi.increfErasedCallable(ops.key_hash, 1);
    abi.increfErasedCallable(ops.key_of, 1);
    abi.increfErasedCallable(ops.row, 1);
    metrics.bump(.closure_retains, 4);
    return ops;
}

fn releaseHostEachOps(ops: HostEachOps, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(ops.items_capability, roc_host, metrics);
    releaseHostValueCapability(ops.item_capability, roc_host, metrics);
    releaseHostValueCapability(ops.key_capability, roc_host, metrics);
    abi.decrefErasedCallable(ops.items_to_values, roc_host);
    abi.decrefErasedCallable(ops.key_hash, roc_host);
    abi.decrefErasedCallable(ops.key_of, roc_host);
    abi.decrefErasedCallable(ops.row, roc_host);
    metrics.bump(.closure_releases, 4);
}

fn releaseHostBinderToken(token: HostBinderToken, roc_host: *abi.RocHost) void {
    hv.releaseU64Box(token, roc_host);
}

/// Memoized output of a signal record: absent until first evaluated, then a
/// retained cell holding the last computed value plus its capability.
pub const HostSignalCacheSlot = union(enum) {
    absent,
    present: HostValueCell,

    pub fn deinit(self: *HostSignalCacheSlot, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        switch (self.*) {
            .absent => {},
            .present => |*cached| cached.deinit(ctx, roc_host, metrics),
        }
        self.* = .absent;
    }

    pub fn replace(self: *HostSignalCacheSlot, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, value: HostValue, cap: HostValueCapability) void {
        self.deinit(ctx, roc_host, metrics);
        self.* = .{ .present = HostValueCell.initRetained(value, cap, metrics) };
    }

    pub fn replaceValue(self: *HostSignalCacheSlot, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) void {
        switch (self.*) {
            .absent => @panic("dirty signal expression was evaluated before its initial value was cached"),
            .present => |*cached| cached.replaceValue(ctx, roc_host, value),
        }
    }

    pub fn cloneRetained(self: HostSignalCacheSlot, ctx: anytype, metrics: anytype) HostSignalCacheSlot {
        return switch (self) {
            .absent => .absent,
            .present => |cached| .{ .present = cached.cloneRetained(ctx, metrics) },
        };
    }
};

pub const HostSignalEvalResult = struct {
    value: HostValue,
    changed: bool,
};

pub const HostSignalConstRecord = struct {
    token: HostSignalToken,
    init: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalMapRecord = struct {
    token: HostSignalToken,
    input: *HostSignalRecord,
    transform: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalMap2Record = struct {
    token: HostSignalToken,
    left: *HostSignalRecord,
    right: *HostSignalRecord,
    transform: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalCombineRecord = struct {
    token: HostSignalToken,
    children: []*HostSignalRecord,
    transform: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalTaskSourceRecord = struct {
    token: HostSignalToken,
    name: []const u8,
    payload_cap: HostValueCapability,
    initial: abi.RocErasedCallable,
    done: abi.RocErasedCallable,
    failed: abi.RocErasedCallable,
    cap: HostValueCapability,
    reset_on_start: bool,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalIntervalSourceRecord = struct {
    token: HostSignalToken,
    period_ms: u64,
    initial: abi.RocErasedCallable,
    tick: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalRecordPayload = union(enum) {
    ref: u64,
    const_value: HostSignalConstRecord,
    map: HostSignalMapRecord,
    map2: HostSignalMap2Record,
    combine: HostSignalCombineRecord,
    task_source: HostSignalTaskSourceRecord,
    interval_source: HostSignalIntervalSourceRecord,
};

/// A refcounted, shareable node in the signal graph. Owns its transform/eq/drop
/// thunks plus a memoized cached value; the active graph holds one reference
/// while a record is mounted.
pub const HostSignalRecord = struct {
    ref_count: usize,
    payload: HostSignalRecordPayload,
    active_graph_id: ?u64 = null,
    active_use_count: usize = 0,
    last_dirty_generation: u64 = 0,
    last_dirty_changed: bool = false,

    pub fn init(allocator: std.mem.Allocator, payload: HostSignalRecordPayload) *HostSignalRecord {
        const record = allocator.create(HostSignalRecord) catch @panic("out of memory");
        record.* = .{
            .ref_count = 1,
            .payload = payload,
        };
        return record;
    }

    pub fn token(self: *const HostSignalRecord) ?HostSignalToken {
        return switch (self.payload) {
            .ref => null,
            .const_value => |payload| payload.token,
            .map => |payload| payload.token,
            .map2 => |payload| payload.token,
            .combine => |payload| payload.token,
            .task_source => |payload| payload.token,
            .interval_source => |payload| payload.token,
        };
    }

    pub fn retain(self: *HostSignalRecord) *HostSignalRecord {
        self.ref_count += 1;
        return self;
    }

    pub fn release(self: *HostSignalRecord, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        if (self.ref_count == 0) @panic("host signal record release underflow");
        if (self.ref_count == 1 and self.active_graph_id != null) @panic("active signal graph held the last signal record reference");
        self.ref_count -= 1;
        if (self.ref_count != 0) return;

        switch (self.payload) {
            .ref => {},
            .const_value => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.init, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .map => |payload| {
                payload.input.release(allocator, ctx, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .map2 => |payload| {
                payload.left.release(allocator, ctx, roc_host, metrics);
                payload.right.release(allocator, ctx, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    child.release(allocator, ctx, roc_host, metrics);
                }
                allocator.free(payload.children);
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .task_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                allocator.free(payload.name);
                releaseHostValueCapability(payload.payload_cap, roc_host, metrics);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.done, roc_host);
                abi.decrefErasedCallable(payload.failed, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 3);
            },
            .interval_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.tick, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 2);
            },
        }

        allocator.destroy(self);
    }
};

/// A reference to a shared signal record plus the source state-node ids that
/// feed it; the unit a descriptor edge owns.
pub const HostSignalBinding = struct {
    record: *HostSignalRecord,
    source_node_ids: []u64,

    pub fn cloneRetained(self: HostSignalBinding, allocator: std.mem.Allocator, metrics: anytype) HostSignalBinding {
        _ = metrics;
        return .{
            .record = self.record.retain(),
            .source_node_ids = allocator.dupe(u64, self.source_node_ids) catch @panic("out of memory"),
        };
    }

    pub fn deinit(self: *HostSignalBinding, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        self.record.release(allocator, ctx, roc_host, metrics);
        allocator.free(self.source_node_ids);
    }
};

// Descriptor layer
//
// The host's ingested view of the Roc `Elem` tree: one descriptor per element,
// text node, attribute, event binding, scope site, state, when, and each.
// Markup carries no identity of its own; dynamic descriptors reference shared
// signal records and event reducers by retained thunk.

/// Typed payload a reducer reads off a real DOM event.
pub const EventPayloadKind = enum(u64) {
    unit = 1,
    str = 2,
    bool = 3,
};

pub const SignalKind = enum(u64) {
    source = 1,
    map = 2,
    map2 = 3,
};

pub const HostEventDescriptor = struct {
    event_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
};

pub const HostActiveEventDesc = struct {
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
};

pub const HostPendingTask = struct {
    request_id: u64,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
    active: bool,
};

pub const HostActiveInterval = struct {
    token: u64,
    source_token: HostSignalToken,
    period_ms: u64,
    active: bool,
};

pub const HostSignalEventRoute = struct {
    event_id: u64,
    signal_ids: []u64,
};

pub const HostState = struct {
    state_id: u64,
    cell: HostValueCell,
    version: u64,
    active: bool,
};

pub const HostSignalDescriptor = struct {
    signal_id: u64,
    kind: SignalKind,
    source_state_ids: []u64,
    source_event_ids: []u64,
    input_signal_ids: []u64,
    rank: u64,
};

pub const HostSignalRoute = struct {
    state_id: u64,
    signal_ids: []u64,
};

pub const HostSignalDependentsRoute = struct {
    signal_id: u64,
    signal_ids: []u64,
};

pub const HostActiveSignalGraphNode = signal_graph.Node(HostSignalRecord);
pub const HostNodeIdentity = identity_table.NodeIdentity;
pub const HostDomIdentity = identity_table.DomIdentity;
pub const HostScopeBranch = scope_tree.Branch;

pub const HostActiveTextSignalSinkKind = enum {
    text_node,
    text_attr,
};

pub const HostActiveTextSignalSink = struct {
    kind: HostActiveTextSignalSinkKind,
    index: usize,
};

pub const HostActiveBoolSignalSink = struct {
    index: usize,
};

pub const HostActiveChangeSignalSink = struct {
    index: usize,
};

pub const HostActiveStructuralSignalKind = enum {
    when,
    each,
};

pub const HostActiveStructuralSignal = struct {
    kind: HostActiveStructuralSignalKind,
    index: usize,
};

pub const HostDirtyStructuralSignal = struct {
    kind: HostActiveStructuralSignalKind,
    node_id: u64,
    branch: ?HostScopeBranch = null,
};

pub const HostEachSite = struct {
    parent_scope_id: u64,
    site_ordinal: u64,
};

pub const HostStructuralReplacementTarget = union(enum) {
    scope: u64,
    each_site: HostEachSite,
};

pub const HostStructuralPatchTargets = struct {
    removed: HostStructuralReplacementTarget,
    replacement: HostStructuralReplacementTarget,
};

pub const HostStructuralSplice = struct {
    removed_elem_ids: []u64,
    touched_parent_ids: []u64,
    replacement_elem_ids: []u64,
    replacement_on_change_indices: []usize,
    replacement_mount_indices: []usize,

    pub fn deinit(self: HostStructuralSplice, allocator: std.mem.Allocator) void {
        allocator.free(self.removed_elem_ids);
        allocator.free(self.touched_parent_ids);
        allocator.free(self.replacement_elem_ids);
        allocator.free(self.replacement_on_change_indices);
        allocator.free(self.replacement_mount_indices);
    }
};

pub const HostStructuralSpliceAndTargets = struct {
    splice: HostStructuralSplice,
    targets: HostStructuralPatchTargets,
};

pub const RecomputeApplyOutcome = struct {
    structural_render_required: bool,
};

pub const HostKeyedRowDiffResult = struct {
    scope_ids: []u64,
    row_items_changed: []bool,
    scope_created: []bool,
    removed_scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
    row_items_unchanged: u64,
    row_items_updated: u64,

    pub fn deinit(self: HostKeyedRowDiffResult, allocator: std.mem.Allocator) void {
        allocator.free(self.scope_ids);
        allocator.free(self.row_items_changed);
        allocator.free(self.scope_created);
        allocator.free(self.removed_scope_ids);
    }
};

/// The retained key/item pair read back out of an `Ui.each` row scope. Named so
/// the native forwarder and the engine method share one type instead of each
/// declaring a distinct anonymous struct.
pub const EachRowValues = struct {
    key: HostValue,
    item: HostValue,
};

pub const HostEachRowRenderSegment = struct {
    scope_id: u64,
    start: usize,
    len: usize,
};

pub const HostEachRowRenderMove = struct {
    old_start: usize,
    new_start: usize,
    len: usize,
};

pub const HostRequiredEventBinding = struct {
    event_id: u64,
    payload_accessor: EventPayloadAccessor,
};

pub const HostRequiredEventBindings = struct {
    click: ?HostRequiredEventBinding = null,
    input: ?HostRequiredEventBinding = null,
    check: ?HostRequiredEventBinding = null,
    pointer_down: ?HostRequiredEventBinding = null,
    pointer_up: ?HostRequiredEventBinding = null,
    pointer_enter: ?HostRequiredEventBinding = null,
    pointer_leave: ?HostRequiredEventBinding = null,
};

pub fn requiredEventBindingSlot(bindings: *HostRequiredEventBindings, kind: RenderEventKind) *?HostRequiredEventBinding {
    return switch (kind) {
        .click => &bindings.click,
        .input => &bindings.input,
        .check => &bindings.check,
        .pointer_down => &bindings.pointer_down,
        .pointer_up => &bindings.pointer_up,
        .pointer_enter => &bindings.pointer_enter,
        .pointer_leave => &bindings.pointer_leave,
    };
}

/// Identity token interned per `Ui.state` binder.
pub const HostBinderToken = *u64;

/// Binds a state binder token to the node id it resolves to within a scope.
pub const HostBinderBinding = struct {
    token: HostBinderToken,
    node_id: u64,
};

pub const HostNodeScopeSiteKind = enum {
    component,
    state,
    when,
    each,
};

pub const HostRenderNodeKind = enum {
    element,
    text,
    signal_text,
};

/// A node that occupies a slot in the rendered child order.
pub const HostRenderNode = struct {
    elem_id: u64,
    kind: HostRenderNodeKind,
};

pub const HostNodeScopeSiteDesc = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    parent_elem_id: u64,
    render_insert_index: usize,
    kind: HostNodeScopeSiteKind,
    binder_bindings: []HostBinderBinding,
};

pub const HostElementDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    tag: []const u8,
};

pub const HostNodeTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    value: []const u8,
};

pub const HostNodeSignalTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeStaticTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    value: []const u8,
};

pub const HostNodeSignalTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    signal: HostSignalBinding,
    read: HostTextRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeStaticBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    value: bool,
};

pub const HostNodeSignalBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    signal: HostSignalBinding,
    read: HostBoolRead,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeOnChangeDesc = struct {
    scope_id: u64,
    signal: HostSignalBinding,
    to_cmd: abi.RocErasedCallable,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeMountDesc = struct {
    scope_id: u64,
    to_cmd: abi.RocErasedCallable,
    run_on_mount: bool,
};

pub const HostNodeCleanupDesc = struct {
    scope_id: u64,
    name: []const u8,
};

pub const HostNodeEventDesc = struct {
    elem_id: u64,
    kind: RenderEventKind,
    binder_token: HostBinderToken,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    payload_accessor: EventPayloadAccessor,
    payload_reducer: HostEventReducer,
    owns_payload_reducer: bool = true,
};

pub const HostNodeStateDesc = struct {
    node_id: u64,
    initial: abi.RocErasedCallable,
    cap: HostValueCapability,
};

pub const HostNodeWhenDesc = struct {
    node_id: u64,
    condition: HostSignalBinding,
    read: HostBoolRead,
    when_false: abi.Elem,
    when_true: abi.Elem,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostNodeEachDesc = struct {
    node_id: u64,
    items: HostSignalBinding,
    ops: HostEachOps,
    cached_value: HostSignalCacheSlot = .absent,
};

pub const HostSignalRecordTokenEntry = struct {
    token: HostSignalToken,
    record: *HostSignalRecord,
};

// Descriptor stream
//
// The retained stream of node descriptors plus the per-elem index that makes
// descriptor lookup O(1). Its append* methods ingest the Roc Elem tree; the
// host drives ingestion and consumes the stream to render.

pub const HostTextFieldDescriptorIndexes = struct {
    text: ?usize = null,
    role: ?usize = null,
    label: ?usize = null,
    test_id: ?usize = null,
    value: ?usize = null,
    class: ?usize = null,

    pub fn get(self: HostTextFieldDescriptorIndexes, field: RenderTextField) ?usize {
        return switch (field) {
            .text => self.text,
            .role => self.role,
            .label => self.label,
            .test_id => self.test_id,
            .value => self.value,
            .class => self.class,
        };
    }

    pub fn slot(self: *HostTextFieldDescriptorIndexes, field: RenderTextField) *?usize {
        return switch (field) {
            .text => &self.text,
            .role => &self.role,
            .label => &self.label,
            .test_id => &self.test_id,
            .value => &self.value,
            .class => &self.class,
        };
    }
};

pub const HostBoolFieldDescriptorIndexes = struct {
    checked: ?usize = null,
    disabled: ?usize = null,

    pub fn get(self: HostBoolFieldDescriptorIndexes, field: RenderBoolField) ?usize {
        return switch (field) {
            .checked => self.checked,
            .disabled => self.disabled,
        };
    }

    pub fn slot(self: *HostBoolFieldDescriptorIndexes, field: RenderBoolField) *?usize {
        return switch (field) {
            .checked => &self.checked,
            .disabled => &self.disabled,
        };
    }
};

pub const HostEventDescriptorIndexes = struct {
    click: ?usize = null,
    input: ?usize = null,
    check: ?usize = null,
    pointer_down: ?usize = null,
    pointer_up: ?usize = null,
    pointer_enter: ?usize = null,
    pointer_leave: ?usize = null,

    pub fn get(self: HostEventDescriptorIndexes, kind: RenderEventKind) ?usize {
        return switch (kind) {
            .click => self.click,
            .input => self.input,
            .check => self.check,
            .pointer_down => self.pointer_down,
            .pointer_up => self.pointer_up,
            .pointer_enter => self.pointer_enter,
            .pointer_leave => self.pointer_leave,
        };
    }

    pub fn slot(self: *HostEventDescriptorIndexes, kind: RenderEventKind) *?usize {
        return switch (kind) {
            .click => &self.click,
            .input => &self.input,
            .check => &self.check,
            .pointer_down => &self.pointer_down,
            .pointer_up => &self.pointer_up,
            .pointer_enter => &self.pointer_enter,
            .pointer_leave => &self.pointer_leave,
        };
    }
};

pub const HostElemDescriptorIndex = struct {
    element: ?usize = null,
    text_node: ?usize = null,
    signal_text_node: ?usize = null,
    static_text_attrs: HostTextFieldDescriptorIndexes = .{},
    signal_text_attrs: HostTextFieldDescriptorIndexes = .{},
    static_bool_attrs: HostBoolFieldDescriptorIndexes = .{},
    signal_bool_attrs: HostBoolFieldDescriptorIndexes = .{},
    events: HostEventDescriptorIndexes = .{},
};

pub const HostScopeSiteDescriptorIndexes = struct {
    component: ?usize = null,
    state: ?usize = null,
    when: ?usize = null,
    each: ?usize = null,

    pub fn get(self: HostScopeSiteDescriptorIndexes, kind: HostNodeScopeSiteKind) ?usize {
        return switch (kind) {
            .component => self.component,
            .state => self.state,
            .when => self.when,
            .each => self.each,
        };
    }

    pub fn slot(self: *HostScopeSiteDescriptorIndexes, kind: HostNodeScopeSiteKind) *?usize {
        return switch (kind) {
            .component => &self.component,
            .state => &self.state,
            .when => &self.when,
            .each => &self.each,
        };
    }
};

pub const HostNodeDescriptorIndex = struct {
    scope_sites: HostScopeSiteDescriptorIndexes = .{},
    state: ?usize = null,
    when: ?usize = null,
    each: ?usize = null,
};

pub const HostNodeDescriptorStream = struct {
    render_nodes: std.ArrayListUnmanaged(HostRenderNode) = .empty,
    elements: std.ArrayListUnmanaged(HostElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(HostNodeTextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(HostNodeSignalTextNodeDesc) = .empty,
    static_text_attrs: std.ArrayListUnmanaged(HostNodeStaticTextAttrDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(HostNodeSignalTextAttrDesc) = .empty,
    static_bool_attrs: std.ArrayListUnmanaged(HostNodeStaticBoolAttrDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(HostNodeSignalBoolAttrDesc) = .empty,
    on_changes: std.ArrayListUnmanaged(HostNodeOnChangeDesc) = .empty,
    mounts: std.ArrayListUnmanaged(HostNodeMountDesc) = .empty,
    cleanups: std.ArrayListUnmanaged(HostNodeCleanupDesc) = .empty,
    events: std.ArrayListUnmanaged(HostNodeEventDesc) = .empty,
    scope_sites: std.ArrayListUnmanaged(HostNodeScopeSiteDesc) = .empty,
    states: std.ArrayListUnmanaged(HostNodeStateDesc) = .empty,
    whens: std.ArrayListUnmanaged(HostNodeWhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(HostNodeEachDesc) = .empty,
    signal_records_by_token: std.ArrayListUnmanaged(HostSignalRecordTokenEntry) = .empty,
    descriptor_indexes_by_elem_id: std.ArrayListUnmanaged(HostElemDescriptorIndex) = .empty,
    descriptor_indexes_by_node_id: std.ArrayListUnmanaged(HostNodeDescriptorIndex) = .empty,
    next_elem_id: u64 = 1,

    pub fn ensureElemDescriptorIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64) *HostElemDescriptorIndex {
        const index: usize = @intCast(elem_id);
        while (self.descriptor_indexes_by_elem_id.items.len <= index) {
            self.descriptor_indexes_by_elem_id.append(allocator, .{}) catch @panic("out of memory");
        }
        return &self.descriptor_indexes_by_elem_id.items[index];
    }

    pub fn elemDescriptorIndex(self: *const HostNodeDescriptorStream, elem_id: u64) ?HostElemDescriptorIndex {
        if (elem_id >= self.descriptor_indexes_by_elem_id.items.len) return null;
        return self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)];
    }

    pub fn ensureNodeDescriptorIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64) *HostNodeDescriptorIndex {
        const index: usize = @intCast(node_id);
        while (self.descriptor_indexes_by_node_id.items.len <= index) {
            self.descriptor_indexes_by_node_id.append(allocator, .{}) catch @panic("out of memory");
        }
        return &self.descriptor_indexes_by_node_id.items[index];
    }

    pub fn nodeDescriptorIndex(self: *const HostNodeDescriptorStream, node_id: u64) ?HostNodeDescriptorIndex {
        if (node_id >= self.descriptor_indexes_by_node_id.items.len) return null;
        return self.descriptor_indexes_by_node_id.items[@intCast(node_id)];
    }

    pub fn setFreshIndex(slot: *?usize, value: usize) void {
        if (slot.* != null) @panic("descriptor stream recorded duplicate descriptor index");
        slot.* = value;
    }

    pub fn updateIndex(slot: *?usize, value: usize) void {
        if (slot.* == null) @panic("descriptor stream updated a missing descriptor index");
        slot.* = value;
    }

    pub fn clearIndex(slot: *?usize, expected: usize) void {
        const existing = slot.* orelse @panic("descriptor stream cleared a missing descriptor index");
        if (existing != expected) @panic("descriptor stream cleared the wrong descriptor index");
        slot.* = null;
    }

    pub fn recordElementIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).element, index);
    }

    pub fn updateElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, index);
    }

    pub fn clearElementIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].element, expected);
    }

    pub fn recordTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).text_node, index);
    }

    pub fn updateTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, index);
    }

    pub fn clearTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].text_node, expected);
    }

    pub fn recordSignalTextNodeIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_node, index);
    }

    pub fn updateSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, index);
    }

    pub fn clearSignalTextNodeIndex(self: *HostNodeDescriptorStream, elem_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_node, expected);
    }

    pub fn recordStaticTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_text_attrs.slot(field), index);
    }

    pub fn updateStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), index);
    }

    pub fn clearStaticTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_text_attrs.slot(field), expected);
    }

    pub fn recordSignalTextAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_text_attrs.slot(field), index);
    }

    pub fn updateSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), index);
    }

    pub fn clearSignalTextAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderTextField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_text_attrs.slot(field), expected);
    }

    pub fn recordStaticBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).static_bool_attrs.slot(field), index);
    }

    pub fn updateStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), index);
    }

    pub fn clearStaticBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].static_bool_attrs.slot(field), expected);
    }

    pub fn recordSignalBoolAttrIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).signal_bool_attrs.slot(field), index);
    }

    pub fn updateSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), index);
    }

    pub fn clearSignalBoolAttrIndex(self: *HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].signal_bool_attrs.slot(field), expected);
    }

    pub fn recordEventIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, kind: RenderEventKind, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureElemDescriptorIndex(allocator, elem_id).events.slot(kind), index);
    }

    pub fn updateEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), index);
    }

    pub fn clearEventIndex(self: *HostNodeDescriptorStream, elem_id: u64, kind: RenderEventKind, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_elem_id.items[@intCast(elem_id)].events.slot(kind), expected);
    }

    pub fn recordScopeSiteIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, kind: HostNodeScopeSiteKind, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(self.ensureNodeDescriptorIndex(allocator, node_id).scope_sites.slot(kind), index);
    }

    pub fn updateScopeSiteIndex(self: *HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, index: usize) void {
        HostNodeDescriptorStream.updateIndex(self.descriptor_indexes_by_node_id.items[@intCast(node_id)].scope_sites.slot(kind), index);
    }

    pub fn clearScopeSiteIndex(self: *HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(self.descriptor_indexes_by_node_id.items[@intCast(node_id)].scope_sites.slot(kind), expected);
    }

    pub fn recordStateIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureNodeDescriptorIndex(allocator, node_id).state, index);
    }

    pub fn updateStateIndex(self: *HostNodeDescriptorStream, node_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].state, index);
    }

    pub fn clearStateIndex(self: *HostNodeDescriptorStream, node_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].state, expected);
    }

    pub fn recordWhenIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureNodeDescriptorIndex(allocator, node_id).when, index);
    }

    pub fn updateWhenIndex(self: *HostNodeDescriptorStream, node_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].when, index);
    }

    pub fn clearWhenIndex(self: *HostNodeDescriptorStream, node_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].when, expected);
    }

    pub fn recordEachIndex(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, index: usize) void {
        HostNodeDescriptorStream.setFreshIndex(&self.ensureNodeDescriptorIndex(allocator, node_id).each, index);
    }

    pub fn updateEachIndex(self: *HostNodeDescriptorStream, node_id: u64, index: usize) void {
        HostNodeDescriptorStream.updateIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].each, index);
    }

    pub fn clearEachIndex(self: *HostNodeDescriptorStream, node_id: u64, expected: usize) void {
        HostNodeDescriptorStream.clearIndex(&self.descriptor_indexes_by_node_id.items[@intCast(node_id)].each, expected);
    }

    pub fn deinit(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        self.render_nodes.deinit(allocator);

        for (self.elements.items) |desc| {
            allocator.free(desc.tag);
        }
        self.elements.deinit(allocator);

        for (self.text_nodes.items) |desc| {
            allocator.free(desc.value);
        }
        self.text_nodes.deinit(allocator);

        for (self.signal_text_nodes.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(desc.read, roc_host, metrics);
        }
        self.signal_text_nodes.deinit(allocator);

        for (self.static_text_attrs.items) |desc| {
            allocator.free(desc.value);
        }
        self.static_text_attrs.deinit(allocator);

        for (self.signal_text_attrs.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(desc.read, roc_host, metrics);
        }
        self.signal_text_attrs.deinit(allocator);

        self.static_bool_attrs.deinit(allocator);

        for (self.signal_bool_attrs.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(desc.read, roc_host, metrics);
        }
        self.signal_bool_attrs.deinit(allocator);

        for (self.on_changes.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.signal.deinit(allocator, ctx, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }
        self.on_changes.deinit(allocator);

        for (self.mounts.items) |desc| {
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }
        self.mounts.deinit(allocator);

        for (self.cleanups.items) |desc| {
            allocator.free(desc.name);
        }
        self.cleanups.deinit(allocator);

        for (self.events.items) |desc| {
            if (desc.owns_payload_reducer) releaseHostEventReducer(desc.payload_reducer, roc_host, metrics);
        }
        self.events.deinit(allocator);

        for (self.scope_sites.items) |desc| {
            allocator.free(desc.binder_bindings);
        }
        self.scope_sites.deinit(allocator);

        for (self.states.items) |desc| {
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.initial, roc_host);
            releaseHostValueCapability(desc.cap, roc_host, metrics);
        }
        self.states.deinit(allocator);

        for (self.whens.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.condition.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(desc.read, roc_host, metrics);
            abi.decrefElem(desc.when_false, roc_host);
            abi.decrefElem(desc.when_true, roc_host);
        }
        self.whens.deinit(allocator);

        for (self.eaches.items) |*desc| {
            desc.cached_value.deinit(ctx, roc_host, metrics);
            desc.items.deinit(allocator, ctx, roc_host, metrics);
            releaseHostEachOps(desc.ops, roc_host, metrics);
        }
        self.eaches.deinit(allocator);

        self.signal_records_by_token.deinit(allocator);
        self.descriptor_indexes_by_elem_id.deinit(allocator);
        self.descriptor_indexes_by_node_id.deinit(allocator);

        self.* = .{};
    }

    pub fn signalRecordByToken(self: *HostNodeDescriptorStream, token: HostSignalToken) ?*HostSignalRecord {
        for (self.signal_records_by_token.items) |entry| {
            if (entry.token == token) return entry.record;
        }
        return null;
    }

    pub fn rememberSignalRecord(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        const token = record.token() orelse return;
        if (self.signalRecordByToken(token)) |existing| {
            if (existing != record) @panic("signal token was bound to multiple host records");
            return;
        }
        self.signal_records_by_token.append(allocator, .{
            .token = token,
            .record = record,
        }) catch @panic("out of memory");
    }

    pub fn rememberSignalRecordTree(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, record: *HostSignalRecord) void {
        self.rememberSignalRecord(allocator, record);
        switch (record.payload) {
            .ref, .const_value => {},
            .map => |payload| self.rememberSignalRecordTree(allocator, payload.input),
            .map2 => |payload| {
                self.rememberSignalRecordTree(allocator, payload.left);
                self.rememberSignalRecordTree(allocator, payload.right);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    self.rememberSignalRecordTree(allocator, child);
                }
            },
            .task_source, .interval_source => {},
        }
    }

    pub fn appendElement(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        self.next_elem_id += 1;

        const tag_copy = allocator.dupe(u8, tag) catch @panic("out of memory");
        const element_index = self.elements.items.len;
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .element }) catch {
            allocator.free(tag_copy);
            @panic("out of memory");
        };
        self.elements.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .tag = tag_copy,
        }) catch {
            allocator.free(tag_copy);
            @panic("out of memory");
        };
        self.recordElementIndex(allocator, elem_id, element_index);
        return elem_id;
    }

    pub fn appendTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        self.next_elem_id += 1;

        const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
        const text_node_index = self.text_nodes.items.len;
        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .text }) catch {
            allocator.free(value_copy);
            @panic("out of memory");
        };
        self.text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            @panic("out of memory");
        };
        self.recordTextNodeIndex(allocator, elem_id, text_node_index);
    }

    pub fn appendSignalTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, parent_elem_id: u64, scope_id: u64, signal: HostSignalBinding, read: HostTextRead) void {
        self.next_elem_id += 1;
        self.rememberSignalRecordTree(allocator, signal.record);
        const retained_read = retainHostTextRead(read, metrics);
        const signal_text_node_index = self.signal_text_nodes.items.len;

        self.render_nodes.append(allocator, .{ .elem_id = elem_id, .kind = .signal_text }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.signal_text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .signal = signal,
            .read = retained_read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordSignalTextNodeIndex(allocator, elem_id, signal_text_node_index);
    }

    pub fn appendStaticTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, value: []const u8) void {
        const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
        const attr_index = self.static_text_attrs.items.len;
        self.static_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            @panic("out of memory");
        };
        self.recordStaticTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendSignalTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: RenderTextField, signal: HostSignalBinding, read: HostTextRead) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        const retained_read = retainHostTextRead(read, metrics);
        const attr_index = self.signal_text_attrs.items.len;
        self.signal_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = retained_read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostTextRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordSignalTextAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendStaticBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, value: bool) void {
        const attr_index = self.static_bool_attrs.items.len;
        self.static_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value,
        }) catch @panic("out of memory");
        self.recordStaticBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendSignalBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, field: RenderBoolField, signal: HostSignalBinding, read: HostBoolRead) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        const retained_read = retainHostBoolRead(read, metrics);
        const attr_index = self.signal_bool_attrs.items.len;
        self.signal_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .read = retained_read,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(retained_read, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordSignalBoolAttrIndex(allocator, elem_id, field, attr_index);
    }

    pub fn appendOnChange(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, signal: HostSignalBinding, to_cmd: abi.RocErasedCallable) void {
        self.rememberSignalRecordTree(allocator, signal.record);
        abi.increfErasedCallable(to_cmd, 1);
        metrics.bump(.closure_retains, 1);
        self.on_changes.append(allocator, .{
            .scope_id = scope_id,
            .signal = signal,
            .to_cmd = to_cmd,
        }) catch {
            var owned_signal = signal;
            owned_signal.deinit(allocator, ctx, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(to_cmd, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendMount(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, scope_id: u64, to_cmd: abi.RocErasedCallable, run_on_mount: bool) void {
        abi.increfErasedCallable(to_cmd, 1);
        metrics.bump(.closure_retains, 1);
        self.mounts.append(allocator, .{
            .scope_id = scope_id,
            .to_cmd = to_cmd,
            .run_on_mount = run_on_mount,
        }) catch {
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(to_cmd, roc_host);
            @panic("out of memory");
        };
    }

    pub fn appendCleanup(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, scope_id: u64, name: []const u8) void {
        const name_copy = allocator.dupe(u8, name) catch @panic("out of memory");
        self.cleanups.append(allocator, .{
            .scope_id = scope_id,
            .name = name_copy,
        }) catch {
            allocator.free(name_copy);
            @panic("out of memory");
        };
    }

    pub fn appendEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, elem_id: u64, kind: RenderEventKind, binder_token: HostBinderToken, target_node_id: u64, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor, payload_reducer: HostEventReducer) void {
        const retained_reducer = retainHostEventReducer(payload_reducer, metrics);
        const event_index = self.events.items.len;
        self.events.append(allocator, .{
            .elem_id = elem_id,
            .kind = kind,
            .binder_token = binder_token,
            .target_node_id = target_node_id,
            .payload_kind = payload_kind,
            .payload_accessor = payload_accessor,
            .payload_reducer = retained_reducer,
        }) catch {
            releaseHostEventReducer(retained_reducer, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordEventIndex(allocator, elem_id, kind, event_index);
    }

    pub fn appendScopeSite(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        self.appendScopeSiteAt(allocator, node_id, scope_id, ordinal, parent_elem_id, self.render_nodes.items.len, kind, binder_bindings);
    }

    pub fn appendScopeSiteAt(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, render_insert_index: usize, kind: HostNodeScopeSiteKind, binder_bindings: []const HostBinderBinding) void {
        const binder_copy = allocator.dupe(HostBinderBinding, binder_bindings) catch @panic("out of memory");
        const scope_site_index = self.scope_sites.items.len;
        self.scope_sites.append(allocator, .{
            .node_id = node_id,
            .scope_id = scope_id,
            .ordinal = ordinal,
            .parent_elem_id = parent_elem_id,
            .render_insert_index = render_insert_index,
            .kind = kind,
            .binder_bindings = binder_copy,
        }) catch {
            allocator.free(binder_copy);
            @panic("out of memory");
        };
        self.recordScopeSiteIndex(allocator, node_id, kind, scope_site_index);
    }

    pub fn appendState(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, initial: abi.RocErasedCallable, cap: HostValueCapability) void {
        _ = retainHostValueCapability(cap, metrics);
        abi.increfErasedCallable(initial, 1);
        metrics.bump(.closure_retains, 1);
        const state_index = self.states.items.len;
        self.states.append(allocator, .{
            .node_id = node_id,
            .initial = initial,
            .cap = cap,
        }) catch {
            releaseHostValueCapability(cap, roc_host, metrics);
            metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(initial, roc_host);
            @panic("out of memory");
        };
        self.recordStateIndex(allocator, node_id, state_index);
    }

    pub fn appendWhen(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, condition: HostSignalBinding, read: HostBoolRead, when_false: abi.Elem, when_true: abi.Elem) void {
        self.rememberSignalRecordTree(allocator, condition.record);
        const retained_read = retainHostBoolRead(read, metrics);
        abi.increfElem(when_false, 1);
        abi.increfElem(when_true, 1);
        const when_index = self.whens.items.len;
        self.whens.append(allocator, .{
            .node_id = node_id,
            .condition = condition,
            .read = retained_read,
            .when_false = when_false,
            .when_true = when_true,
        }) catch {
            var owned_condition = condition;
            owned_condition.deinit(allocator, ctx, roc_host, metrics);
            releaseHostBoolRead(retained_read, roc_host, metrics);
            abi.decrefElem(when_false, roc_host);
            abi.decrefElem(when_true, roc_host);
            @panic("out of memory");
        };
        self.recordWhenIndex(allocator, node_id, when_index);
    }

    pub fn appendEach(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, node_id: u64, items: HostSignalBinding, ops: HostEachOps) void {
        self.rememberSignalRecordTree(allocator, items.record);
        const retained_ops = retainHostEachOps(ops, metrics);
        const each_index = self.eaches.items.len;
        self.eaches.append(allocator, .{
            .node_id = node_id,
            .items = items,
            .ops = retained_ops,
        }) catch {
            var owned_items = items;
            owned_items.deinit(allocator, ctx, roc_host, metrics);
            releaseHostEachOps(retained_ops, roc_host, metrics);
            @panic("out of memory");
        };
        self.recordEachIndex(allocator, node_id, each_index);
    }
};

// Host-agnostic readers over a descriptor stream. These operate purely on the
// stream's descriptor tables and panic on internal invariant violations, so they
// are shared by both hosts and by the engine's structural apply path.

pub fn findElementDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostElementDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.element orelse return null;
    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
    const desc = stream.elements.items[index];
    if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.text_node orelse return null;
    if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
    const desc = stream.text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findSignalTextNodeDesc(stream: *const HostNodeDescriptorStream, elem_id: u64) ?HostNodeSignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
    const desc = stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn findSignalTextNodeDescMutable(stream: *HostNodeDescriptorStream, elem_id: u64) ?*HostNodeSignalTextNodeDesc {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    const index = descriptor_index.signal_text_node orelse return null;
    if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
    const desc = &stream.signal_text_nodes.items[index];
    if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
    return desc;
}

pub fn streamHasTextField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderTextField) bool {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return false;
    if (field == .text and descriptor_index.text_node != null) return true;
    if (field == .text and descriptor_index.signal_text_node != null) return true;

    if (descriptor_index.static_text_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.static_text_attrs.items.len) @panic("static text attr descriptor index exceeded descriptor table");
        const desc = stream.static_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("static text attr descriptor index pointed at the wrong field");
        return true;
    }
    if (descriptor_index.signal_text_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.signal_text_attrs.items.len) @panic("signal text attr descriptor index exceeded descriptor table");
        const desc = stream.signal_text_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("signal text attr descriptor index pointed at the wrong field");
        return true;
    }
    return false;
}

pub fn streamHasBoolField(stream: *const HostNodeDescriptorStream, elem_id: u64, field: RenderBoolField) bool {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return false;
    if (descriptor_index.static_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.static_bool_attrs.items.len) @panic("static bool attr descriptor index exceeded descriptor table");
        const desc = stream.static_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("static bool attr descriptor index pointed at the wrong field");
        return true;
    }
    if (descriptor_index.signal_bool_attrs.get(field)) |attr_index| {
        if (attr_index >= stream.signal_bool_attrs.items.len) @panic("signal bool attr descriptor index exceeded descriptor table");
        const desc = stream.signal_bool_attrs.items[attr_index];
        if (desc.elem_id != elem_id or desc.field != field) @panic("signal bool attr descriptor index pointed at the wrong field");
        return true;
    }
    return false;
}

pub fn maxRenderElemId(stream: *const HostNodeDescriptorStream) u64 {
    var max_elem_id: u64 = 0;
    for (stream.render_nodes.items) |node| {
        max_elem_id = @max(max_elem_id, node.elem_id);
    }
    return max_elem_id;
}

pub fn renderNodeTag(stream: *const HostNodeDescriptorStream, node: HostRenderNode) []const u8 {
    return switch (node.kind) {
        .element => (findElementDesc(stream, node.elem_id) orelse @panic("renderNodeTag: render node has no matching descriptor")).tag,
        .text, .signal_text => "text",
    };
}

pub fn streamElemTag(stream: *const HostNodeDescriptorStream, elem_id: u64) []const u8 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse @panic("elem id had no descriptor index");
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
        return desc.tag;
    }
    if (descriptor_index.text_node != null or descriptor_index.signal_text_node != null) return "text";
    @panic("elem id had no render descriptor");
}

pub fn renderNodeParentElemId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
        .text => (findTextNodeDesc(stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
        .signal_text => (findSignalTextNodeDesc(stream, node.elem_id) orelse @panic("renderNodeParentElemId: render node has no matching descriptor")).parent_elem_id,
    };
}

pub fn streamElemParentElemId(stream: *const HostNodeDescriptorStream, elem_id: u64) u64 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse @panic("elem id had no descriptor index");
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    if (descriptor_index.text_node) |index| {
        if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
        const desc = stream.text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    if (descriptor_index.signal_text_node) |index| {
        if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
        const desc = stream.signal_text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
        return desc.parent_elem_id;
    }
    @panic("elem id had no render descriptor");
}

pub fn streamDirectChildren(allocator: std.mem.Allocator, stream: *const HostNodeDescriptorStream, parent_elem_id: u64) []u64 {
    var children: std.ArrayListUnmanaged(u64) = .empty;
    errdefer children.deinit(allocator);

    for (stream.render_nodes.items) |node| {
        if (renderNodeParentElemId(stream, node) == parent_elem_id) {
            children.append(allocator, node.elem_id) catch @panic("out of memory");
        }
    }

    return children.toOwnedSlice(allocator) catch @panic("out of memory");
}

pub fn renderNodeScopeId(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
    return switch (node.kind) {
        .element => (findElementDesc(stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
        .text => (findTextNodeDesc(stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
        .signal_text => (findSignalTextNodeDesc(stream, node.elem_id) orelse @panic("renderNodeScopeId: render node has no matching descriptor")).scope_id,
    };
}

pub fn elemScopeId(stream: *const HostNodeDescriptorStream, elem_id: u64) ?u64 {
    const descriptor_index = stream.elemDescriptorIndex(elem_id) orelse return null;
    if (descriptor_index.element) |index| {
        if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
        const desc = stream.elements.items[index];
        if (desc.elem_id != elem_id) @panic("element descriptor index pointed at the wrong elem id");
        return desc.scope_id;
    }
    if (descriptor_index.text_node) |index| {
        if (index >= stream.text_nodes.items.len) @panic("text node descriptor index exceeded descriptor table");
        const desc = stream.text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("text node descriptor index pointed at the wrong elem id");
        return desc.scope_id;
    }
    if (descriptor_index.signal_text_node) |index| {
        if (index >= stream.signal_text_nodes.items.len) @panic("signal text node descriptor index exceeded descriptor table");
        const desc = stream.signal_text_nodes.items[index];
        if (desc.elem_id != elem_id) @panic("signal text node descriptor index pointed at the wrong elem id");
        return desc.scope_id;
    }
    return null;
}

pub fn adjustedRenderInsertIndex(old_index: usize, replace_index: usize, removed_count: usize, replacement_count: usize) usize {
    if (removed_count == 0) {
        if (old_index < replace_index) return old_index;
        return old_index + replacement_count;
    }
    if (old_index <= replace_index) return old_index;
    if (old_index < replace_index + removed_count) @panic("scope site inside replaced scope was not removed");
    return old_index - removed_count + replacement_count;
}

fn sourceNodeIdsIntersect(left: []const u64, right: []const u64) bool {
    for (left) |left_id| {
        for (right) |right_id| {
            if (left_id == right_id) return true;
        }
    }
    return false;
}

// Host-agnostic signal-record construction helpers (shared by both hosts).

pub fn resolveNodeBinderRef(binder_stack: []const HostBinderBinding, token: HostBinderToken) u64 {
    var index = binder_stack.len;
    while (index > 0) {
        index -= 1;
        const binding = binder_stack[index];
        if (binding.token == token) return binding.node_id;
    }
    @panic("Node.BinderRef referenced a state binder outside the active scope");
}

pub fn renderTextFieldFromAbi(field: u64) RenderTextField {
    return switch (field) {
        @intFromEnum(RenderTextField.text) => .text,
        @intFromEnum(RenderTextField.role) => .role,
        @intFromEnum(RenderTextField.label) => .label,
        @intFromEnum(RenderTextField.test_id) => .test_id,
        @intFromEnum(RenderTextField.value) => .value,
        @intFromEnum(RenderTextField.class) => .class,
        else => @panic("Roc render text descriptor used an unknown field"),
    };
}

pub fn renderBoolFieldFromAbi(field: u64) RenderBoolField {
    return switch (field) {
        @intFromEnum(RenderBoolField.checked) => .checked,
        @intFromEnum(RenderBoolField.disabled) => .disabled,
        else => @panic("Roc render bool descriptor used an unknown field"),
    };
}

pub fn renderEventKindFromAbi(kind: u64) RenderEventKind {
    return switch (kind) {
        @intFromEnum(RenderEventKind.click) => .click,
        @intFromEnum(RenderEventKind.input) => .input,
        @intFromEnum(RenderEventKind.check) => .check,
        @intFromEnum(RenderEventKind.pointer_down) => .pointer_down,
        @intFromEnum(RenderEventKind.pointer_up) => .pointer_up,
        @intFromEnum(RenderEventKind.pointer_enter) => .pointer_enter,
        @intFromEnum(RenderEventKind.pointer_leave) => .pointer_leave,
        else => @panic("Roc render event descriptor used an unknown event kind"),
    };
}

pub fn eventPayloadKindFromAbi(payload_kind: u64) EventPayloadKind {
    return switch (payload_kind) {
        @intFromEnum(EventPayloadKind.unit) => .unit,
        @intFromEnum(EventPayloadKind.str) => .str,
        @intFromEnum(EventPayloadKind.bool) => .bool,
        else => @panic("Roc event descriptor used an unknown payload kind"),
    };
}

pub fn eventPayloadAccessorFromAbi(payload_accessor: u64) EventPayloadAccessor {
    return switch (payload_accessor) {
        @intFromEnum(EventPayloadAccessor.none) => .none,
        @intFromEnum(EventPayloadAccessor.target_value) => .target_value,
        @intFromEnum(EventPayloadAccessor.target_checked) => .target_checked,
        else => @panic("Roc event descriptor used an unknown payload accessor"),
    };
}

pub fn validateExistingSignalRecord(record: *HostSignalRecord, expected_tag: std.meta.Tag(HostSignalRecordPayload)) void {
    if (std.meta.activeTag(record.payload) != expected_tag) {
        @panic("signal token was reused for a different signal expression kind");
    }
}

pub fn appendSignalRecordSourceNodeIds(allocator: std.mem.Allocator, source_node_ids: *std.ArrayListUnmanaged(u64), record: *HostSignalRecord) void {
    switch (record.payload) {
        .ref => |node_id| {
            if (!u64SliceContains(source_node_ids.items, node_id)) {
                source_node_ids.append(allocator, node_id) catch @panic("out of memory");
            }
        },
        .const_value => {},
        .map => |payload| appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.input),
        .map2 => |payload| {
            appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.left);
            appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.right);
        },
        .combine => |payload| {
            for (payload.children) |child| {
                appendSignalRecordSourceNodeIds(allocator, source_node_ids, child);
            }
        },
        .task_source, .interval_source => {},
    }
}

pub fn Engine(comptime Ctx: type) type {
    verifyCtx(Ctx);

    return struct {
        const Self = @This();

        pub const Context = Ctx;
        pub const Metrics = Ctx.Metrics;
        pub const RegistryOps = Ctx.RegistryOps;
        pub const HostValueRegistry = host_value_registry.Registry(HostValueCapability);
        pub const ActiveEventDesc = HostActiveEventDesc;
        pub const IdentityInternError = scope_tree.Error || identity_table.Error;
        pub const EventLookupError = error{
            EventIdZero,
            MissingSignalEventRoute,
            SignalEventRouteIndexMismatch,
            MissingEventDescriptor,
            EventDescriptorIndexMismatch,
        };
        pub const SignalLookupError = error{
            MissingSignalRoute,
            SignalRouteIndexMismatch,
            MissingSignalDependentRoute,
            SignalDependentRouteIndexMismatch,
            MissingSignalDescriptor,
            SignalDescriptorIndexMismatch,
        };
        pub const ActiveEventLookupError = error{
            MissingActiveEvent,
        };
        pub const StateLookupError = error{
            MissingActiveState,
        };
        pub const RocHostRequiredError = error{
            MissingRocHost,
        };

        host_values: HostValueRegistry = .{},
        active_events: std.ArrayListUnmanaged(ActiveEventDesc) = .empty,
        event_descriptors: std.ArrayListUnmanaged(HostEventDescriptor) = .empty,
        signal_event_routes: std.ArrayListUnmanaged(HostSignalEventRoute) = .empty,
        signal_descriptors: std.ArrayListUnmanaged(HostSignalDescriptor) = .empty,
        signal_routes: std.ArrayListUnmanaged(HostSignalRoute) = .empty,
        signal_dependents: std.ArrayListUnmanaged(HostSignalDependentsRoute) = .empty,
        signal_cache: std.ArrayListUnmanaged(HostSignalCacheSlot) = .empty,
        states: std.ArrayListUnmanaged(HostState) = .empty,
        state_indexes_by_node_id: std.ArrayListUnmanaged(?usize) = .empty,
        scopes: std.ArrayListUnmanaged(HostScope) = .empty,
        node_identities: std.ArrayListUnmanaged(HostNodeIdentity) = .empty,
        dom_identities: std.ArrayListUnmanaged(HostDomIdentity) = .empty,
        active_stream: HostNodeDescriptorStream = .{},
        active_signal_graph: std.ArrayListUnmanaged(HostActiveSignalGraphNode) = .empty,
        active_source_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(u64)) = .empty,
        active_text_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveTextSignalSink)) = .empty,
        active_bool_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveBoolSignalSink)) = .empty,
        active_change_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveChangeSignalSink)) = .empty,
        active_structural_signal_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostActiveStructuralSignal)) = .empty,
        render_scalar_nodes: std.ArrayListUnmanaged(RenderScalarNodeCache) = .empty,
        pending_tasks: std.ArrayListUnmanaged(HostPendingTask) = .empty,
        active_intervals: std.ArrayListUnmanaged(HostActiveInterval) = .empty,
        cleanup_events: std.ArrayListUnmanaged([]const u8) = .empty,
        next_task_request_id: u64 = 1,
        next_interval_token: u64 = 1,
        next_elem_id: u64 = 0,
        roc_host: ?*abi.RocHost = null,
        root_elem: ?abi.Elem = null,
        last_runtime_metrics: Metrics = Ctx.zeroMetrics(),
        pending_roc_metrics: Metrics = Ctx.zeroMetrics(),
        // Render-command accumulator (patches/create/append/remove/...) folded into
        // last_runtime_metrics at finish. Engine-owned so both hosts share it.
        render_metrics: render.Metrics = .{},
        // Dispatch counters (events processed / recompute batches) folded into
        // last_runtime_metrics at finish. Engine-owned so both hosts share it.
        dispatch_metrics: DispatchMetrics = .{},
        dirty_signal_generation: u64 = 0,

        pub fn init() Self {
            return .{};
        }

        fn debugPhase(ctx: Ctx.Handle, phase: u32) void {
            if (comptime @hasDecl(Ctx, "debugPhase")) {
                Ctx.debugPhase(ctx, phase);
            }
        }

        fn pushCapabilities(ctx: Ctx.Handle, caps: []const HostValueCapability) void {
            Ctx.pushHostValueCapabilities(ctx, caps);
        }

        fn popCapabilities(ctx: Ctx.Handle) void {
            Ctx.popHostValueCapabilities(ctx);
        }

        fn callHostValueToUnitWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) void {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            erased_calls.callErasedHostValueToUnit(roc_host, callable, value);
        }

        fn callHostValueToHostValueWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) HostValue {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToHostValue(roc_host, callable, value);
        }

        fn callHostValueToStartTaskCmdWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) erased_calls.StartTaskCmd {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToStartTaskCmd(roc_host, callable, value);
        }

        fn callHostValueToStrWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) abi.RocStr {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToStr(roc_host, callable, value);
        }

        fn callHostValueToBoolWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) bool {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToBool(roc_host, callable, value);
        }

        fn callHostValueToU64WithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) u64 {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToU64(roc_host, callable, value);
        }

        fn callHostValueToHostValueListWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValue) HostValueList {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueToHostValueList(roc_host, callable, value);
        }

        fn callHostValueListToHostValueWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, value: HostValueList) HostValue {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueListToHostValue(roc_host, callable, value);
        }

        fn callHostValueHostValueToBoolWithCapability(ctx: Ctx.Handle, roc_host: *abi.RocHost, cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) bool {
            const caps = [_]HostValueCapability{cap};
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueHostValueToBool(roc_host, callable, left, right);
        }

        fn callHostValueHostValueToHostValueWithCapabilities(ctx: Ctx.Handle, roc_host: *abi.RocHost, left_cap: HostValueCapability, right_cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) HostValue {
            const caps = [_]HostValueCapability{ left_cap, right_cap };
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueHostValueToHostValue(roc_host, callable, left, right);
        }

        fn callHostValueHostValueToElemWithCapabilities(ctx: Ctx.Handle, roc_host: *abi.RocHost, left_cap: HostValueCapability, right_cap: HostValueCapability, callable: abi.RocErasedCallable, left: HostValue, right: HostValue) abi.Elem {
            const caps = [_]HostValueCapability{ left_cap, right_cap };
            pushCapabilities(ctx, &caps);
            defer popCapabilities(ctx);
            return erased_calls.callErasedHostValueHostValueToElem(roc_host, callable, left, right);
        }

        pub fn recordDispatch(self: *Self) void {
            self.dispatch_metrics.events_processed += 1;
            self.dispatch_metrics.recompute_batches += 1;
        }

        pub fn recordStreamNodesScanned(self: *Self, count: usize) void {
            self.pending_roc_metrics.bump(.stream_nodes_scanned, @intCast(count));
        }

        pub fn recordScopeCreated(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.scopes_created, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordEachKeyCompare(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.each_key_compares, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn deinitRenderCache(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            for (self.render_scalar_nodes.items) |*node| {
                node.deinit(allocator);
            }
            self.render_scalar_nodes.deinit(allocator);
            self.render_scalar_nodes = .empty;
        }

        pub fn hasRenderRoot(self: *const Self) bool {
            return self.render_scalar_nodes.items.len != 0 and self.render_scalar_nodes.items[0].active;
        }

        pub fn resetRenderTree(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            for (self.render_scalar_nodes.items) |*node| {
                node.deinit(allocator);
            }
            self.render_scalar_nodes.items.len = 0;
            self.render_scalar_nodes.append(allocator, RenderScalarNodeCache.initActive(allocator, "root")) catch @panic("out of memory");
            Ctx.sink(ctx).reset();
        }

        fn ensureRenderCacheNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, tag: []const u8) bool {
            const allocator = Ctx.allocator(ctx);
            const index: usize = @intCast(elem_id);
            if (index > self.render_scalar_nodes.items.len) {
                @panic("render cache node ids must be dense and ordered by elem id");
            }
            if (index == self.render_scalar_nodes.items.len) {
                self.render_scalar_nodes.append(allocator, RenderScalarNodeCache.initActive(allocator, tag)) catch @panic("out of memory");
                return true;
            }
            const node = &self.render_scalar_nodes.items[index];
            if (!node.active) {
                @panic("render descriptor referenced an inactive render cache identity");
            }
            if (node.tag == null or !std.mem.eql(u8, node.tag.?, tag)) {
                @panic("render descriptor changed the tag for an existing render cache identity");
            }
            return false;
        }

        pub fn appendRenderNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
            const created = self.ensureRenderCacheNode(ctx, elem_id, tag);
            if (!created) @panic("initial render append reused an existing render cache identity");
            const parent = self.activeRenderScalarNode(parent_elem_id);
            const child = self.activeRenderScalarNode(elem_id);
            child.parent_id = parent_elem_id;
            parent.children.append(Ctx.allocator(ctx), elem_id) catch @panic("out of memory");
            Ctx.sink(ctx).appendNode(elem_id, parent_elem_id, tag);
        }

        pub fn ensureRenderNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, tag: []const u8, counts: *render.Counts) void {
            if (!self.ensureRenderCacheNode(ctx, elem_id, tag)) return;
            Ctx.sink(ctx).ensureNode(elem_id, tag);
            counts.addCreateElement();
        }

        pub fn removeRenderNode(self: *Self, ctx: Ctx.Handle, elem_id: u64, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const index: usize = @intCast(elem_id);
            if (index >= self.render_scalar_nodes.items.len or !self.render_scalar_nodes.items[index].active) {
                @panic("render cache removed a missing element");
            }
            if (elem_id == 0) @panic("render cache attempted to remove the host DOM root");

            if (self.render_scalar_nodes.items[index].parent_id) |parent_id| {
                const parent_index: usize = @intCast(parent_id);
                if (parent_index < self.render_scalar_nodes.items.len and self.render_scalar_nodes.items[parent_index].active) {
                    const parent = &self.render_scalar_nodes.items[parent_index];
                    if (u64SliceIndex(parent.children.items, elem_id)) |child_index| {
                        _ = parent.children.orderedRemove(child_index);
                    }
                }
            }
            self.render_scalar_nodes.items[index].deinit(allocator);
            Ctx.sink(ctx).removeNode(elem_id);
            counts.addRemoveNode();
        }

        fn activeRenderScalarNode(self: *Self, elem_id: u64) *RenderScalarNodeCache {
            const index: usize = @intCast(elem_id);
            if (index >= self.render_scalar_nodes.items.len or !self.render_scalar_nodes.items[index].active) {
                @panic("render command referenced missing element cache");
            }
            return &self.render_scalar_nodes.items[index];
        }

        pub fn replaceRenderChildren(self: *Self, ctx: Ctx.Handle, parent_elem_id: u64, next_child_ids: []const u64, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const parent = self.activeRenderScalarNode(parent_elem_id);

            for (next_child_ids, 0..) |child_id, new_index| {
                const child = self.activeRenderScalarNode(child_id);
                const old_parent_id = child.parent_id;
                const old_child_index = if (old_parent_id) |id| u64SliceIndex(self.activeRenderScalarNode(id).children.items, child_id) else null;

                if (old_parent_id == null or old_parent_id.? != parent_elem_id or old_child_index == null) {
                    counts.addAppendChild();
                } else if (old_child_index.? != new_index) {
                    counts.addMoveBefore();
                }
                child.parent_id = parent_elem_id;
            }

            parent.children.deinit(allocator);
            parent.children = .empty;
            parent.children.appendSlice(allocator, next_child_ids) catch @panic("out of memory");
            Ctx.sink(ctx).replaceChildren(parent_elem_id, next_child_ids);
        }

        pub fn replaceRenderChildrenForMoves(self: *Self, ctx: Ctx.Handle, parent_elem_id: u64, next_child_ids: []const u64, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            const parent = self.activeRenderScalarNode(parent_elem_id);
            if (parent.children.items.len != next_child_ids.len) @panic("pure structural move changed child count");

            var old_child_indexes: std.AutoHashMapUnmanaged(u64, usize) = .{};
            defer old_child_indexes.deinit(allocator);
            for (parent.children.items, 0..) |child_id, index| {
                const entry = old_child_indexes.getOrPut(allocator, child_id) catch @panic("out of memory");
                if (entry.found_existing) @panic("parent child list contained duplicate element ids");
                entry.value_ptr.* = index;
            }

            const old_indexes_in_next_order = allocator.alloc(usize, next_child_ids.len) catch @panic("out of memory");
            defer allocator.free(old_indexes_in_next_order);
            for (next_child_ids, 0..) |child_id, index| {
                const child = self.activeRenderScalarNode(child_id);
                if (child.parent_id == null or child.parent_id.? != parent_elem_id) @panic("pure structural move crossed parent boundary");
                old_indexes_in_next_order[index] = old_child_indexes.get(child_id) orelse @panic("pure structural move inserted a child");
            }

            const stable_scratch = allocator.alloc(usize, next_child_ids.len) catch @panic("out of memory");
            defer allocator.free(stable_scratch);
            const stable_len = stableSubsequenceLength(old_indexes_in_next_order, stable_scratch);
            const displaced_count = next_child_ids.len - stable_len;
            var displaced_index: usize = 0;
            while (displaced_index < displaced_count) : (displaced_index += 1) {
                counts.addMoveBefore();
            }

            for (next_child_ids) |child_id| {
                self.activeRenderScalarNode(child_id).parent_id = parent_elem_id;
            }
            parent.children.deinit(allocator);
            parent.children = .empty;
            parent.children.appendSlice(allocator, next_child_ids) catch @panic("out of memory");
            Ctx.sink(ctx).replaceChildrenForMoves(parent_elem_id, next_child_ids);
        }

        pub fn applyRenderEventBinding(self: *Self, ctx: Ctx.Handle, elem_id: u64, kind: RenderEventKind, binding: ?HostRequiredEventBinding, counts: *render.Counts) void {
            const node = self.activeRenderScalarNode(elem_id);
            const event_slot = node.eventSlot(kind);
            const accessor_slot = node.eventAccessorSlot(kind);
            const event_id = if (binding) |payload| payload.event_id else null;
            const payload_accessor = if (binding) |payload| payload.payload_accessor else null;
            if (event_slot.* == event_id and accessor_slot.* == payload_accessor) return;

            event_slot.* = event_id;
            accessor_slot.* = payload_accessor;
            if (event_id) |id| {
                Ctx.sink(ctx).bindEventKind(elem_id, kind, id, payload_accessor orelse @panic("event binding was missing payload accessor"));
            } else {
                Ctx.sink(ctx).clearEvent(elem_id, kind);
            }
            counts.addEventBinding();
        }

        fn descriptorStreamNodeTag(stream: *const HostNodeDescriptorStream, node: HostRenderNode) []const u8 {
            const descriptor_index = stream.elemDescriptorIndex(node.elem_id) orelse @panic("render node had no descriptor index");
            return switch (node.kind) {
                .element => blk: {
                    const index = descriptor_index.element orelse @panic("element render node had no element descriptor");
                    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
                    const desc = stream.elements.items[index];
                    if (desc.elem_id != node.elem_id) @panic("element descriptor index pointed at the wrong elem id");
                    break :blk desc.tag;
                },
                .text, .signal_text => "text",
            };
        }

        fn descriptorStreamNodeParent(stream: *const HostNodeDescriptorStream, node: HostRenderNode) u64 {
            const descriptor_index = stream.elemDescriptorIndex(node.elem_id) orelse @panic("render node had no descriptor index");
            return switch (node.kind) {
                .element => blk: {
                    const index = descriptor_index.element orelse @panic("element render node had no element descriptor");
                    if (index >= stream.elements.items.len) @panic("element descriptor index exceeded descriptor table");
                    const desc = stream.elements.items[index];
                    if (desc.elem_id != node.elem_id) @panic("element descriptor index pointed at the wrong elem id");
                    break :blk desc.parent_elem_id;
                },
                .text => blk: {
                    const index = descriptor_index.text_node orelse @panic("text render node had no text descriptor");
                    if (index >= stream.text_nodes.items.len) @panic("text descriptor index exceeded descriptor table");
                    const desc = stream.text_nodes.items[index];
                    if (desc.elem_id != node.elem_id) @panic("text descriptor index pointed at the wrong elem id");
                    break :blk desc.parent_elem_id;
                },
                .signal_text => blk: {
                    const index = descriptor_index.signal_text_node orelse @panic("signal text render node had no signal text descriptor");
                    if (index >= stream.signal_text_nodes.items.len) @panic("signal text descriptor index exceeded descriptor table");
                    const desc = stream.signal_text_nodes.items[index];
                    if (desc.elem_id != node.elem_id) @panic("signal text descriptor index pointed at the wrong elem id");
                    break :blk desc.parent_elem_id;
                },
            };
        }

        pub fn debugAssertRenderCacheMatchesStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream) void {
            if (comptime builtin.mode != .Debug) return;

            const allocator = Ctx.allocator(ctx);
            var seen = allocator.alloc(bool, self.render_scalar_nodes.items.len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);
            if (seen.len != 0) seen[0] = true;

            for (stream.render_nodes.items) |node| {
                const index: usize = @intCast(node.elem_id);
                if (index >= self.render_scalar_nodes.items.len) @panic("descriptor stream referenced render cache node outside table");
                const cached = &self.render_scalar_nodes.items[index];
                if (!cached.active) @panic("descriptor stream referenced inactive render cache node");
                if (cached.tag == null or !std.mem.eql(u8, cached.tag.?, descriptorStreamNodeTag(stream, node))) {
                    @panic("descriptor stream tag disagreed with render cache");
                }
                const parent_id = descriptorStreamNodeParent(stream, node);
                if (cached.parent_id == null or cached.parent_id.? != parent_id) {
                    @panic("descriptor stream parent disagreed with render cache");
                }
                seen[index] = true;
            }

            for (self.render_scalar_nodes.items, 0..) |cached, index| {
                if (index == 0 or !cached.active) continue;
                if (index >= seen.len or !seen[index]) @panic("active render cache node was absent from descriptor stream");
            }

            for (self.render_scalar_nodes.items, 0..) |cached, parent_id| {
                if (!cached.active) continue;
                var expected_children: std.ArrayListUnmanaged(u64) = .empty;
                defer expected_children.deinit(allocator);
                for (stream.render_nodes.items) |node| {
                    if (descriptorStreamNodeParent(stream, node) == parent_id) {
                        expected_children.append(allocator, node.elem_id) catch @panic("out of memory");
                    }
                }
                if (!std.mem.eql(u64, cached.children.items, expected_children.items)) {
                    @panic("descriptor stream child order disagreed with render cache");
                }
            }
        }

        pub fn debugAssertRenderCacheMatchesSink(self: *Self, ctx: Ctx.Handle) void {
            if (comptime builtin.mode != .Debug) return;

            for (self.render_scalar_nodes.items, 0..) |cached, index| {
                Ctx.sink(ctx).debugAssertNode(
                    @intCast(index),
                    cached.active,
                    cached.tag,
                    cached.parent_id,
                    cached.children.items,
                    cached.bound_click_event,
                    cached.bound_input_event,
                    cached.bound_check_event,
                    cached.bound_pointer_down_event,
                    cached.bound_pointer_up_event,
                    cached.bound_pointer_enter_event,
                    cached.bound_pointer_leave_event,
                );
            }
        }

        pub fn applyRenderTextField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderTextField, value: []const u8) bool {
            const allocator = Ctx.allocator(ctx);
            const slot = self.activeRenderScalarNode(elem_id).textSlot(field);
            if (slot.*) |existing| {
                if (std.mem.eql(u8, existing, value)) return false;
            }

            const value_copy = allocator.dupe(u8, value) catch @panic("out of memory");
            if (slot.*) |existing| allocator.free(existing);
            slot.* = value_copy;
            Ctx.sink(ctx).applyTextField(elem_id, field, value);
            return true;
        }

        pub fn applyRenderBoolField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderBoolField, value: bool) bool {
            const slot = self.activeRenderScalarNode(elem_id).boolSlot(field);
            if (slot.*) |existing| {
                if (existing == value) return false;
            }

            slot.* = value;
            Ctx.sink(ctx).applyBoolField(elem_id, field, value);
            return true;
        }

        pub fn clearRenderTextField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderTextField) bool {
            const allocator = Ctx.allocator(ctx);
            const slot = self.activeRenderScalarNode(elem_id).textSlot(field);
            const existing = slot.* orelse return false;
            allocator.free(existing);
            slot.* = null;
            Ctx.sink(ctx).clearTextField(elem_id, field);
            return true;
        }

        pub fn clearRenderBoolField(self: *Self, ctx: Ctx.Handle, elem_id: u64, field: RenderBoolField) bool {
            const slot = self.activeRenderScalarNode(elem_id).boolSlot(field);
            const existing = slot.* orelse return false;
            slot.* = null;
            if (!existing) return false;
            Ctx.sink(ctx).clearBoolField(elem_id, field);
            return true;
        }

        pub fn clearEventDescriptors(self: *Self) void {
            self.event_descriptors.items.len = 0;
        }

        pub fn deinitActiveEventDesc(self: *Self, roc_host: *abi.RocHost, desc: ActiveEventDesc) void {
            releaseHostEventReducer(desc.payload_reducer, roc_host, &self.pending_roc_metrics);
        }

        pub fn clearActiveEvents(self: *Self) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                if (self.active_events.items.len != 0) return RocHostRequiredError.MissingRocHost;
                return;
            };
            for (self.active_events.items) |desc| {
                self.deinitActiveEventDesc(roc_host, desc);
            }
            self.active_events.items.len = 0;
        }

        pub fn clearSignalCache(self: *Self, ctx: Ctx.Handle) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                if (self.signal_cache.items.len != 0) return RocHostRequiredError.MissingRocHost;
                return;
            };
            for (self.signal_cache.items) |*slot| {
                slot.deinit(ctx, roc_host, &self.pending_roc_metrics);
            }
            self.signal_cache.items.len = 0;
        }

        pub fn clearStates(self: *Self, ctx: Ctx.Handle) RocHostRequiredError!void {
            const roc_host = self.roc_host orelse {
                for (self.states.items) |state| {
                    if (state.active) return RocHostRequiredError.MissingRocHost;
                }
                self.states.items.len = 0;
                self.state_indexes_by_node_id.items.len = 0;
                return;
            };
            for (self.states.items) |*state| {
                if (!state.active) continue;
                state.cell.deinit(ctx, roc_host, &self.pending_roc_metrics);
                state.active = false;
            }
            self.states.items.len = 0;
            self.state_indexes_by_node_id.items.len = 0;
        }

        fn ensureStateIndexSlot(self: *Self, ctx: Ctx.Handle, node_id: u64) *?usize {
            const allocator = Ctx.allocator(ctx);
            const index: usize = @intCast(node_id);
            while (self.state_indexes_by_node_id.items.len <= index) {
                self.state_indexes_by_node_id.append(allocator, null) catch @panic("out of memory");
            }
            return &self.state_indexes_by_node_id.items[index];
        }

        fn recordStateCellIndex(self: *Self, ctx: Ctx.Handle, node_id: u64, index: usize) void {
            const slot = self.ensureStateIndexSlot(ctx, node_id);
            if (slot.* != null) @panic("state cell index already existed for node id");
            slot.* = index;
        }

        fn clearStateCellIndex(self: *Self, node_id: u64, expected: usize) void {
            if (node_id >= self.state_indexes_by_node_id.items.len) @panic("state cell index clear referenced an unknown node id");
            const slot = &self.state_indexes_by_node_id.items[@intCast(node_id)];
            const existing = slot.* orelse @panic("state cell index clear missed its node id");
            if (existing != expected) @panic("state cell index clear referenced the wrong state index");
            slot.* = null;
        }

        pub fn deactivateState(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, node_id: u64) void {
            const state_index = self.stateIndexByNodeId(node_id) orelse return;
            const state = &self.states.items[state_index];
            state.cell.deinit(ctx, roc_host, &self.pending_roc_metrics);
            state.active = false;
            self.clearStateCellIndex(node_id, state_index);
        }

        pub fn clearScopes(self: *Self, ctx: Ctx.Handle) RocHostRequiredError!void {
            if (self.roc_host) |roc_host| {
                for (self.scopes.items) |*scope| {
                    if (!scope.active) continue;
                    deinitHostScopeStep(&scope.step, ctx, roc_host, &self.pending_roc_metrics);
                }
            } else if (self.scopes.items.len != 0) {
                return RocHostRequiredError.MissingRocHost;
            }
            self.scopes.items.len = 0;
        }

        pub fn cleanupEventCount(self: *const Self, name: []const u8) u64 {
            var count: u64 = 0;
            for (self.cleanup_events.items) |event_name| {
                if (std.mem.eql(u8, event_name, name)) count += 1;
            }
            return count;
        }

        pub fn activeTaskRecordByToken(self: *Self, token: HostSignalToken) ?*HostSignalRecord {
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .task_source => |payload| {
                        if (payload.token == token) return node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .interval_source => {},
                }
            }
            return null;
        }

        pub fn activeIntervalRecordCountByPeriod(self: *const Self, period_ms: u64) u64 {
            var count: u64 = 0;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| {
                        if (payload.period_ms == period_ms) count += 1;
                    },
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }
            return count;
        }

        pub fn activeIntervalRecordByToken(self: *Self, source_token: HostSignalToken) ?*HostSignalRecord {
            var found: ?*HostSignalRecord = null;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| {
                        if (payload.token != source_token) continue;
                        if (found != null) @panic("interval token matched more than one active interval source");
                        found = node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }
            return found;
        }

        pub fn activeIntervalSourceTokenByRuntimeToken(self: *Self, token: u64) ?HostSignalToken {
            var found: ?HostSignalToken = null;
            for (self.active_intervals.items) |interval| {
                if (!interval.active or interval.token != token) continue;
                if (found != null) @panic("runtime interval token matched more than one active interval");
                found = interval.source_token;
            }
            return found;
        }

        pub fn pendingTaskCountByName(self: *const Self, name: []const u8) u64 {
            var count: u64 = 0;
            for (self.pending_tasks.items) |task| {
                if (task.active and std.mem.eql(u8, task.task_name, name)) count += 1;
            }
            return count;
        }

        pub fn pendingTaskIndexByRequestId(self: *Self, request_id: u64) ?usize {
            var found: ?usize = null;
            for (self.pending_tasks.items, 0..) |task, index| {
                if (!task.active or task.request_id != request_id) continue;
                if (found != null) @panic("task request id matched more than one pending request");
                found = index;
            }
            return found;
        }

        pub fn sourceSignalIdsForEvent(self: *Self, event_id: u64) EventLookupError![]const u64 {
            if (event_id == 0) return EventLookupError.EventIdZero;

            const route_index = event_id - 1;
            if (route_index >= self.signal_event_routes.items.len) return EventLookupError.MissingSignalEventRoute;

            const route = self.signal_event_routes.items[@intCast(route_index)];
            if (route.event_id != event_id) return EventLookupError.SignalEventRouteIndexMismatch;
            return route.signal_ids;
        }

        pub fn eventPayloadKind(self: *Self, event_id: u64) EventLookupError!EventPayloadKind {
            if (event_id == 0) return EventLookupError.EventIdZero;

            const event_index = event_id - 1;
            if (event_index >= self.event_descriptors.items.len) return EventLookupError.MissingEventDescriptor;

            const descriptor = self.event_descriptors.items[@intCast(event_index)];
            if (descriptor.event_id != event_id) return EventLookupError.EventDescriptorIndexMismatch;
            return descriptor.payload_kind;
        }

        pub fn signalIdsForState(self: *Self, state_id: u64) SignalLookupError![]const u64 {
            if (state_id >= self.signal_routes.items.len) return SignalLookupError.MissingSignalRoute;

            const route = self.signal_routes.items[@intCast(state_id)];
            if (route.state_id != state_id) return SignalLookupError.SignalRouteIndexMismatch;
            return route.signal_ids;
        }

        pub fn dependentSignalIdsForSignal(self: *Self, signal_id: u64) SignalLookupError![]const u64 {
            if (signal_id >= self.signal_dependents.items.len) return SignalLookupError.MissingSignalDependentRoute;

            const route = self.signal_dependents.items[@intCast(signal_id)];
            if (route.signal_id != signal_id) return SignalLookupError.SignalDependentRouteIndexMismatch;
            return route.signal_ids;
        }

        pub fn signalRank(self: *Self, signal_id: u64) SignalLookupError!u64 {
            if (signal_id >= self.signal_descriptors.items.len) return SignalLookupError.MissingSignalDescriptor;

            const descriptor = self.signal_descriptors.items[@intCast(signal_id)];
            if (descriptor.signal_id != signal_id) return SignalLookupError.SignalDescriptorIndexMismatch;
            return descriptor.rank;
        }

        pub fn nextDirtySignalGeneration(self: *Self) u64 {
            if (self.dirty_signal_generation == std.math.maxInt(u64)) {
                @panic("host dirty signal generation overflowed");
            }
            self.dirty_signal_generation += 1;
            return self.dirty_signal_generation;
        }

        pub fn appendSignalAndDependents(self: *Self, allocator: std.mem.Allocator, signal_ids: *std.ArrayListUnmanaged(u64), signal_id: u64) void {
            if (!u64SliceContains(signal_ids.items, signal_id)) {
                signal_ids.append(allocator, signal_id) catch @panic("out of memory");
            }

            var index: usize = 0;
            while (index < signal_ids.items.len) : (index += 1) {
                const current_signal_id = signal_ids.items[index];
                const dependents = self.dependentSignalIdsForSignal(current_signal_id) catch @panic("host signal dependent route table is invalid");
                for (dependents) |dependent_signal_id| {
                    if (!u64SliceContains(signal_ids.items, dependent_signal_id)) {
                        signal_ids.append(allocator, dependent_signal_id) catch @panic("out of memory");
                    }
                }
            }
        }

        pub fn sortSignalIdsByRank(self: *Self, signal_ids: []u64) void {
            var index: usize = 1;
            while (index < signal_ids.len) : (index += 1) {
                const value = signal_ids[index];
                const value_rank = self.signalRank(value) catch @panic("host signal rank table is invalid");
                var insert_index = index;
                while (insert_index > 0) {
                    const previous = signal_ids[insert_index - 1];
                    const previous_rank = self.signalRank(previous) catch @panic("host signal rank table is invalid");
                    if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
                    signal_ids[insert_index] = previous;
                    insert_index -= 1;
                }
                signal_ids[insert_index] = value;
            }
        }

        pub fn dirtySignalIdsForEvent(self: *Self, allocator: std.mem.Allocator, event_id: u64) []u64 {
            var dirty_signal_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer dirty_signal_ids.deinit(allocator);

            const source_signal_ids = self.sourceSignalIdsForEvent(event_id) catch @panic("event id has no host source signal route descriptor");
            for (source_signal_ids) |signal_id| {
                self.appendSignalAndDependents(allocator, &dirty_signal_ids, signal_id);
            }

            const signal_ids = dirty_signal_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            self.sortSignalIdsByRank(signal_ids);
            return signal_ids;
        }

        pub fn activeSignalRank(self: *Self, record_id: u64) u64 {
            return signal_graph.rank(HostSignalRecord, self.active_signal_graph.items, record_id) catch @panic("active signal record id has no graph node");
        }

        pub fn dependentActiveSignalRecordIds(self: *Self, record_id: u64) []const u64 {
            return signal_graph.dependentIds(HostSignalRecord, self.active_signal_graph.items, record_id) catch @panic("active signal record id has no dependent table");
        }

        pub fn appendActiveSignalAndDependents(self: *Self, allocator: std.mem.Allocator, record_ids: *std.ArrayListUnmanaged(u64), record_id: u64) void {
            signal_graph.appendReachableDependents(HostSignalRecord, allocator, self.active_signal_graph.items, record_ids, record_id) catch |err| switch (err) {
                error.OutOfMemory => @panic("out of memory"),
                else => @panic("active signal dependent traversal referenced an unknown record"),
            };
        }

        pub fn sortActiveSignalRecordIdsByRank(self: *Self, record_ids: []u64) void {
            signal_graph.sortIdsByRank(HostSignalRecord, self.active_signal_graph.items, record_ids) catch {
                @panic("active signal rank sort referenced an unknown record");
            };
        }

        pub fn dirtyActiveSignalRecordIdsForSources(self: *Self, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64) []u64 {
            var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer dirty_record_ids.deinit(allocator);

            for (dirty_source_node_ids) |source_node_id| {
                const route_index: usize = @intCast(source_node_id);
                if (route_index >= self.active_source_signal_routes.items.len) continue;

                for (self.active_source_signal_routes.items[route_index].items) |record_id| {
                    self.appendActiveSignalAndDependents(allocator, &dirty_record_ids, record_id);
                }
            }

            const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            self.sortActiveSignalRecordIdsByRank(record_ids);
            return record_ids;
        }

        pub fn dirtyActiveSignalRecordIdsForRoots(self: *Self, allocator: std.mem.Allocator, root_record_ids: []const u64) []u64 {
            var dirty_record_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer dirty_record_ids.deinit(allocator);

            for (root_record_ids) |record_id| {
                if (record_id >= self.active_signal_graph.items.len) @panic("dirty active signal root referenced an unknown record");
                self.appendActiveSignalAndDependents(allocator, &dirty_record_ids, record_id);
            }

            const record_ids = dirty_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
            self.sortActiveSignalRecordIdsByRank(record_ids);
            return record_ids;
        }

        pub fn recordDerivedCall(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.derived_calls_into_roc, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn recordSignalPrune(self: *Self) void {
            var metrics = self.pending_roc_metrics;
            metrics.bump(.propagation_prunes, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn cloneCachedSignalValue(self: *Self, ctx: Ctx.Handle, cache_slot: *const HostSignalCacheSlot) HostValue {
            _ = self;
            debugPhase(ctx, 409);
            return switch (cache_slot.*) {
                .absent => @panic("cached signal expression value was requested before initialization"),
                .present => |cached| Ctx.cloneHostValue(ctx, cached.value),
            };
        }

        pub fn updateDirtySignalExprCache(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, cap: HostValueCapability) HostSignalEvalResult {
            switch (cache_slot.*) {
                .absent => {
                    debugPhase(ctx, 450);
                    return .{
                        .value = self.replaceSignalExprCacheAndClone(ctx, cache_slot, roc_host, value, cap),
                        .changed = true,
                    };
                },
                .present => |*cached| {
                    debugPhase(ctx, 451);
                    const values_equal = cached.valueEquals(ctx, roc_host, value);
                    if (values_equal) {
                        debugPhase(ctx, 452);
                        cached.dropIncoming(ctx, roc_host, value);
                        self.recordSignalPrune();
                        debugPhase(ctx, 453);
                        return .{ .value = Ctx.cloneHostValue(ctx, cached.value), .changed = false };
                    }

                    debugPhase(ctx, 454);
                    cached.replaceValue(ctx, roc_host, value);
                    debugPhase(ctx, 455);
                    return .{ .value = Ctx.cloneHostValue(ctx, cached.value), .changed = true };
                },
            }
        }

        pub fn updateDirtySignalCache(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, cap: HostValueCapability) bool {
            switch (cache_slot.*) {
                .absent => {
                    cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, cap);
                    return true;
                },
                .present => |*cached| {
                    const values_equal = cached.valueEquals(ctx, roc_host, value);
                    if (values_equal) {
                        cached.dropIncoming(ctx, roc_host, value);
                        self.recordSignalPrune();
                        return false;
                    }

                    cache_slot.replaceValue(ctx, roc_host, value);
                    return true;
                },
            }
        }

        pub fn cloneMemoizedDirtySignalResult(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord, dirty_generation: u64) ?HostSignalEvalResult {
            if (record.last_dirty_generation != dirty_generation) return null;

            return switch (record.payload) {
                .ref => null,
                .const_value => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .map => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .map2 => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .combine => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .task_source => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
                .interval_source => |*payload| .{
                    .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                    .changed = record.last_dirty_changed,
                },
            };
        }

        pub fn rememberDirtySignalResult(_: *Self, record: *HostSignalRecord, dirty_generation: u64, result: HostSignalEvalResult) HostSignalEvalResult {
            record.last_dirty_generation = dirty_generation;
            record.last_dirty_changed = result.changed;
            return result;
        }

        pub fn hostSignalRecordCapability(_: *Self, ctx: Ctx.Handle, record: *const HostSignalRecord) HostValueCapability {
            return switch (record.payload) {
                .ref => |node_id| Ctx.stateCapability(ctx, node_id),
                .const_value => |payload| payload.cap,
                .map => |payload| payload.cap,
                .map2 => |payload| payload.cap,
                .combine => |payload| payload.cap,
                .task_source => |payload| payload.cap,
                .interval_source => |payload| payload.cap,
            };
        }

        pub fn hostSignalBindingCapability(self: *Self, ctx: Ctx.Handle, signal: *const HostSignalBinding) HostValueCapability {
            return self.hostSignalRecordCapability(ctx, signal.record);
        }

        pub fn dropHostSignalRecordValue(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *const HostSignalRecord, value: HostValue) void {
            const cap = self.hostSignalRecordCapability(ctx, record);
            callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), value);
        }

        pub fn ensureStateFromDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: HostNodeStateDesc) void {
            if (self.stateIndexByNodeId(desc.node_id) != null) return;

            const initial = erased_calls.callValueInitThunk(roc_host, desc.initial);
            var cell = HostValueCell.initRetained(initial, desc.cap, &self.pending_roc_metrics);
            const state_index = self.states.items.len;
            self.states.append(Ctx.allocator(ctx), .{
                .state_id = desc.node_id,
                .cell = cell,
                .version = 0,
                .active = true,
            }) catch {
                cell.deinit(ctx, roc_host, &self.pending_roc_metrics);
                @panic("out of memory");
            };
            self.recordStateCellIndex(ctx, desc.node_id, state_index);
        }

        fn signalRecordByTokenForStream(self: *Self, stream: *HostNodeDescriptorStream, token: HostSignalToken) ?*HostSignalRecord {
            if (stream.signalRecordByToken(token)) |record| return record;
            if (stream != &self.active_stream) return self.active_stream.signalRecordByToken(token);
            return null;
        }

        fn retainExistingSignalRecordForStream(self: *Self, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, token: HostSignalToken, expected_tag: std.meta.Tag(HostSignalRecordPayload)) ?*HostSignalRecord {
            const record = self.signalRecordByTokenForStream(stream, token) orelse return null;
            validateExistingSignalRecord(record, expected_tag);
            stream.rememberSignalRecord(allocator, record);
            return record.retain();
        }

        pub fn bindNodeSignalExpr(self: *Self, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) *HostSignalRecord {
            return switch (expr.tag) {
                .Ref => blk: {
                    const node_id = resolveNodeBinderRef(binder_stack, expr.payload_ref());
                    break :blk HostSignalRecord.init(allocator, .{ .ref = node_id });
                },
                .ConstValue => blk: {
                    const payload = expr.payload_const_value();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .const_value)) |record| {
                        break :blk record;
                    }

                    const record = HostSignalRecord.init(allocator, .{ .const_value = .{
                        .token = retainHostSignalToken(token),
                        .init = retainHostCallable(payload._1, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._2, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .Map => blk: {
                    const payload = expr.payload_map();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .map)) |record| {
                        break :blk record;
                    }

                    const input = self.bindNodeSignalExpr(allocator, stream, payload._1.*, binder_stack);
                    const record = HostSignalRecord.init(allocator, .{ .map = .{
                        .token = retainHostSignalToken(token),
                        .input = input,
                        .transform = retainHostCallable(payload._2, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._3, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .Map2 => blk: {
                    const payload = expr.payload_map2();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .map2)) |record| {
                        break :blk record;
                    }

                    const left = self.bindNodeSignalExpr(allocator, stream, payload._1.*, binder_stack);
                    const right = self.bindNodeSignalExpr(allocator, stream, payload._2.*, binder_stack);
                    const record = HostSignalRecord.init(allocator, .{ .map2 = .{
                        .token = retainHostSignalToken(token),
                        .left = left,
                        .right = right,
                        .transform = retainHostCallable(payload._3, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._4, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .Combine => blk: {
                    const payload = expr.payload_combine();
                    const token = payload._0;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .combine)) |record| {
                        break :blk record;
                    }

                    const source_children = payload._1.items();
                    const children = allocator.alloc(*HostSignalRecord, source_children.len) catch @panic("out of memory");
                    for (source_children, children) |child, *dest| {
                        dest.* = self.bindNodeSignalExpr(allocator, stream, child, binder_stack);
                    }
                    const record = HostSignalRecord.init(allocator, .{ .combine = .{
                        .token = retainHostSignalToken(token),
                        .children = children,
                        .transform = retainHostCallable(payload._2, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload._3, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .TaskSource => blk: {
                    const payload = expr.payload_task_source();
                    const token = payload.token;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .task_source)) |record| {
                        break :blk record;
                    }

                    const record = HostSignalRecord.init(allocator, .{ .task_source = .{
                        .token = retainHostSignalToken(token),
                        .name = allocator.dupe(u8, payload.name.asSlice()) catch @panic("out of memory"),
                        .payload_cap = retainHostValueCapability(payload.payload_cap, &self.pending_roc_metrics),
                        .initial = retainHostCallable(payload.initial, &self.pending_roc_metrics),
                        .done = retainHostCallable(payload.done, &self.pending_roc_metrics),
                        .failed = retainHostCallable(payload.failed, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload.cap, &self.pending_roc_metrics),
                        .reset_on_start = payload.reset_on_start,
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
                .IntervalSource => blk: {
                    const payload = expr.payload_interval_source();
                    const token = payload.token;
                    if (self.retainExistingSignalRecordForStream(allocator, stream, token, .interval_source)) |record| {
                        break :blk record;
                    }

                    const record = HostSignalRecord.init(allocator, .{ .interval_source = .{
                        .token = retainHostSignalToken(token),
                        .period_ms = payload.period_ms,
                        .initial = retainHostCallable(payload.initial, &self.pending_roc_metrics),
                        .tick = retainHostCallable(payload.tick, &self.pending_roc_metrics),
                        .cap = retainHostValueCapability(payload.cap, &self.pending_roc_metrics),
                    } });
                    stream.rememberSignalRecord(allocator, record);
                    break :blk record;
                },
            };
        }

        pub fn bindNodeSignal(self: *Self, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) HostSignalBinding {
            const record = self.bindNodeSignalExpr(allocator, stream, expr, binder_stack);
            var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
            appendSignalRecordSourceNodeIds(allocator, &source_node_ids, record);
            return .{
                .record = record,
                .source_node_ids = source_node_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
            };
        }

        pub fn streamNodeIdInScopeSubtree(self: *Self, previous: *const HostNodeDescriptorStream, node_id: u64, root_scope_id: u64) bool {
            self.recordStreamNodesScanned(previous.scope_sites.items.len);
            for (previous.scope_sites.items) |site| {
                if (site.node_id == node_id and self.scopeIsDescendantOrSelf(site.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope")) return true;
            }
            return false;
        }

        pub fn renderNodeInScopeSubtree(self: *Self, stream: *const HostNodeDescriptorStream, node: HostRenderNode, root_scope_id: u64) bool {
            return self.scopeIsDescendantOrSelf(renderNodeScopeId(stream, node), root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope");
        }

        pub fn firstRenderIndexInScopeSubtree(self: *Self, stream: *const HostNodeDescriptorStream, root_scope_id: u64) ?usize {
            self.recordStreamNodesScanned(stream.render_nodes.items.len);
            for (stream.render_nodes.items, 0..) |node, index| {
                if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) return index;
            }
            return null;
        }

        pub fn lastRenderEndIndexInScopeSubtree(self: *Self, stream: *const HostNodeDescriptorStream, root_scope_id: u64) ?usize {
            var end_index: ?usize = null;
            self.recordStreamNodesScanned(stream.render_nodes.items.len);
            for (stream.render_nodes.items, 0..) |node, index| {
                if (self.renderNodeInScopeSubtree(stream, node, root_scope_id)) {
                    end_index = index + 1;
                }
            }
            return end_index;
        }

        pub fn scopeSubtreeHasDirtyStructuralSource(self: *Self, previous: *const HostNodeDescriptorStream, root_scope_id: u64, dirty_source_node_ids: []const u64) bool {
            if (dirty_source_node_ids.len == 0) return false;

            self.recordStreamNodesScanned(previous.whens.items.len);
            for (previous.whens.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                if (sourceNodeIdsIntersect(desc.condition.source_node_ids, dirty_source_node_ids)) return true;
            }
            self.recordStreamNodesScanned(previous.eaches.items.len);
            for (previous.eaches.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                if (sourceNodeIdsIntersect(desc.items.source_node_ids, dirty_source_node_ids)) return true;
            }
            return false;
        }

        pub fn cloneHostSignalCacheSlot(self: *Self, ctx: Ctx.Handle, slot: HostSignalCacheSlot, metrics: anytype) HostSignalCacheSlot {
            _ = self;
            return slot.cloneRetained(ctx, metrics);
        }

        pub fn copyActiveScopeSubtreeDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root_scope_id: u64) void {
            const allocator = Ctx.allocator(ctx);
            const previous = &self.active_stream;
            const previous_render_base = self.firstRenderIndexInScopeSubtree(previous, root_scope_id);
            const next_render_base = stream.render_nodes.items.len;
            var copied_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer copied_elem_ids.deinit(allocator);

            for (previous.render_nodes.items) |node| {
                const node_scope_id = renderNodeScopeId(previous, node);
                if (!(self.scopeIsDescendantOrSelf(node_scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;

                copied_elem_ids.append(allocator, node.elem_id) catch @panic("out of memory");
                switch (node.kind) {
                    .element => {
                        const desc = findElementDesc(previous, node.elem_id) orelse @panic("copyActiveScopeSubtreeDescriptors: render node has no matching descriptor");
                        _ = stream.appendElement(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.tag);
                    },
                    .text => {
                        const desc = findTextNodeDesc(previous, node.elem_id) orelse @panic("copyActiveScopeSubtreeDescriptors: render node has no matching descriptor");
                        stream.appendTextNode(allocator, desc.elem_id, desc.parent_elem_id, desc.scope_id, desc.value);
                    },
                    .signal_text => {
                        const desc = findSignalTextNodeDesc(previous, node.elem_id) orelse @panic("copyActiveScopeSubtreeDescriptors: render node has no matching descriptor");
                        const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                        stream.appendSignalTextNode(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.parent_elem_id, desc.scope_id, signal, desc.read);
                        stream.signal_text_nodes.items[stream.signal_text_nodes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
                    },
                }
            }

            for (previous.static_text_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                stream.appendStaticTextAttr(allocator, desc.elem_id, desc.field, desc.value);
            }
            for (previous.signal_text_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendSignalTextAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
                stream.signal_text_attrs.items[stream.signal_text_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.static_bool_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                stream.appendStaticBoolAttr(allocator, desc.elem_id, desc.field, desc.value);
            }
            for (previous.signal_bool_attrs.items) |desc| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendSignalBoolAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.field, signal, desc.read);
                stream.signal_bool_attrs.items[stream.signal_bool_attrs.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.on_changes.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                const signal = desc.signal.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendOnChange(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.scope_id, signal, desc.to_cmd);
                stream.on_changes.items[stream.on_changes.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.mounts.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                stream.appendMount(allocator, roc_host, &self.pending_roc_metrics, desc.scope_id, desc.to_cmd, false);
            }
            for (previous.cleanups.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                stream.appendCleanup(allocator, desc.scope_id, desc.name);
            }
            for (previous.events.items, 0..) |desc, event_index| {
                if (!u64SliceContains(copied_elem_ids.items, desc.elem_id)) continue;
                const payload_reducer = if (desc.owns_payload_reducer) desc.payload_reducer else self.activeEventReducerByIndex(event_index) catch @panic("active event table is missing a retained payload reducer");
                stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, desc.elem_id, desc.kind, desc.binder_token, desc.target_node_id, desc.payload_kind, desc.payload_accessor, payload_reducer);
            }

            for (previous.scope_sites.items) |desc| {
                if (!(self.scopeIsDescendantOrSelf(desc.scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"))) continue;
                const render_insert_index = if (previous_render_base) |render_base| blk: {
                    if (desc.render_insert_index < render_base) @panic("copied scope site insertion point precedes its scope subtree");
                    break :blk next_render_base + (desc.render_insert_index - render_base);
                } else next_render_base;
                stream.appendScopeSiteAt(allocator, desc.node_id, desc.scope_id, desc.ordinal, desc.parent_elem_id, render_insert_index, desc.kind, desc.binder_bindings);
            }
            for (previous.states.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                stream.appendState(allocator, roc_host, &self.pending_roc_metrics, desc.node_id, desc.initial, desc.cap);
            }
            for (previous.whens.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                const condition = desc.condition.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendWhen(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.node_id, condition, desc.read, desc.when_false, desc.when_true);
                stream.whens.items[stream.whens.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
            for (previous.eaches.items) |desc| {
                if (!self.streamNodeIdInScopeSubtree(previous, desc.node_id, root_scope_id)) continue;
                const items = desc.items.cloneRetained(allocator, &self.pending_roc_metrics);
                stream.appendEach(allocator, ctx, roc_host, &self.pending_roc_metrics, desc.node_id, items, desc.ops);
                stream.eaches.items[stream.eaches.items.len - 1].cached_value = self.cloneHostSignalCacheSlot(ctx, desc.cached_value, &self.pending_roc_metrics);
            }
        }

        pub fn deinitPendingTask(self: *Self, ctx: Ctx.Handle, task: *HostPendingTask) void {
            const allocator = Ctx.allocator(ctx);
            releaseHostSignalToken(task.task_token, self.roc_host.?);
            allocator.free(task.task_name);
            allocator.free(task.request);
            task.* = undefined;
        }

        pub fn cancelPendingTask(self: *Self, ctx: Ctx.Handle, task: *HostPendingTask) void {
            if (task.active) {
                Ctx.sink(ctx).cancelTask(task.request_id);
            }
            self.deinitPendingTask(ctx, task);
        }

        pub fn clearPendingTasks(self: *Self, ctx: Ctx.Handle) void {
            for (self.pending_tasks.items) |*task| {
                self.cancelPendingTask(ctx, task);
            }
            self.pending_tasks.items.len = 0;
        }

        pub fn cancelPendingTasksByTaskToken(self: *Self, ctx: Ctx.Handle, task_token: HostSignalToken) void {
            var index: usize = 0;
            while (index < self.pending_tasks.items.len) {
                if (!self.pending_tasks.items[index].active or self.pending_tasks.items[index].task_token != task_token) {
                    index += 1;
                    continue;
                }

                var task = self.removePendingTaskAt(index);
                self.cancelPendingTask(ctx, &task);
            }
        }

        pub fn cancelPendingTasksInScopeSubtree(self: *Self, ctx: Ctx.Handle, scope_id: u64) void {
            var write_index: usize = 0;
            for (self.pending_tasks.items) |*task| {
                if (self.scopeIsDescendantOrSelf(task.owner_scope_id, scope_id) catch @panic("scope descriptor referenced an unknown parent scope")) {
                    self.cancelPendingTask(ctx, task);
                    continue;
                }
                self.pending_tasks.items[write_index] = task.*;
                write_index += 1;
            }
            self.pending_tasks.items.len = write_index;
        }

        pub fn appendCleanupEvent(self: *Self, ctx: Ctx.Handle, name: []const u8) void {
            const allocator = Ctx.allocator(ctx);
            const copy = allocator.dupe(u8, name) catch @panic("out of memory");
            self.cleanup_events.append(allocator, copy) catch {
                allocator.free(copy);
                @panic("out of memory");
            };
        }

        pub fn disposeScopeSubtree(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");

            var child_index: usize = 0;
            while (child_index < self.scopes.items.len) : (child_index += 1) {
                const child = self.scopes.items[child_index];
                if (!child.active) continue;
                if (child.parent_scope_id == scope_id) {
                    self.disposeScopeSubtree(ctx, roc_host, child.scope_id);
                }
            }

            for (self.node_identities.items) |*identity| {
                if (identity.active and identity.scope_id == scope_id) {
                    self.deactivateState(ctx, roc_host, identity.node_id);
                    identity.active = false;
                }
            }

            for (self.active_stream.cleanups.items) |cleanup| {
                if (cleanup.scope_id == scope_id) {
                    self.appendCleanupEvent(ctx, cleanup.name);
                }
            }

            self.cancelPendingTasksInScopeSubtree(ctx, scope_id);

            for (self.dom_identities.items) |*identity| {
                if (identity.active and identity.scope_id == scope_id) {
                    identity.active = false;
                }
            }

            const scope = &self.scopes.items[@intCast(scope_id)];
            deinitHostScopeStep(&scope.step, ctx, roc_host, &self.pending_roc_metrics);
            scope.active = false;
            var metrics = self.pending_roc_metrics;
            metrics.bump(.scopes_disposed, 1);
            self.pending_roc_metrics = metrics;
        }

        pub fn createEachRowScope(self: *Self, ctx: Ctx.Handle, parent_scope_id: u64, site_ordinal: u64, key: HostValue, item: HostValue, key_cap: HostValueCapability, item_cap: HostValueCapability) u64 {
            self.validateScopeId(parent_scope_id) catch @panic("scope id has no host scope descriptor");

            const key_cell = HostValueCell.initRetained(key, key_cap, &self.pending_roc_metrics);
            const item_cell = HostValueCell.initRetained(item, item_cap, &self.pending_roc_metrics);
            const result = scope_tree.appendEachRow(HostEachRowScopeStep, Ctx.allocator(ctx), &self.scopes, parent_scope_id, .{
                .site_ordinal = site_ordinal,
                .key = key_cell,
                .item = item_cell,
            }) catch @panic("scope id has no host scope descriptor");
            self.recordScopeCreated();
            return result.scope_id;
        }

        pub fn eachRowScopeItemEquals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, item: HostValue) bool {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            return switch (scope.step) {
                .each_row => |*row| row.item.valueEquals(ctx, roc_host, item),
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn replaceEachRowScopeItem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, item: HostValue) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            switch (scope.step) {
                .each_row => {},
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            }

            scope.step.each_row.item.replaceValue(ctx, roc_host, item);
        }

        pub fn eachRowScopeValues(self: *Self, scope_id: u64) EachRowValues {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            return switch (scope.step) {
                .each_row => |row| .{ .key = row.key.value, .item = row.item.value },
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn syncEachRowScopes(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, ops: HostEachOps) HostKeyedRowDiffResult {
            self.validateScopeId(parent_scope_id) catch @panic("scope id has no host scope descriptor");
            if (keys.len != items.len) @panic("Ui.each keyed scope received mismatched key and item lists");

            const allocator = Ctx.allocator(ctx);
            const existing_scope_ids = self.activeEachRowScopes(allocator, parent_scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
            defer allocator.free(existing_scope_ids);

            const key_cap = ops.key_capability;
            const item_cap = ops.item_capability;
            const match_plan = self.buildEachRowMatchPlan(allocator, ctx, roc_host, existing_scope_ids, keys, ops) catch @panic("keyed row diff operation failed");
            defer allocator.free(match_plan.rows);
            errdefer allocator.free(match_plan.removed_scope_ids);

            var next_scope_ids = allocator.alloc(u64, keys.len) catch @panic("out of memory");
            errdefer allocator.free(next_scope_ids);
            var row_items_changed = allocator.alloc(bool, keys.len) catch @panic("out of memory");
            errdefer allocator.free(row_items_changed);
            var scope_created = allocator.alloc(bool, keys.len) catch @panic("out of memory");
            errdefer allocator.free(scope_created);

            var row_items_unchanged: u64 = 0;
            var row_items_updated: u64 = 0;

            for (match_plan.rows, keys, items, 0..) |row_plan, key, item, key_index| {
                switch (row_plan) {
                    .reuse => |reuse| {
                        const scope_id = reuse.scope_id;
                        next_scope_ids[key_index] = scope_id;
                        scope_created[key_index] = false;
                        callHostValueToUnitWithCapability(ctx, roc_host, key_cap, hv.hostValueCapabilityDrop(key_cap), key);
                        if (self.eachRowScopeItemEquals(ctx, roc_host, scope_id, item)) {
                            callHostValueToUnitWithCapability(ctx, roc_host, item_cap, hv.hostValueCapabilityDrop(item_cap), item);
                            row_items_changed[key_index] = false;
                            row_items_unchanged += 1;
                        } else {
                            self.replaceEachRowScopeItem(ctx, roc_host, scope_id, item);
                            row_items_changed[key_index] = true;
                            row_items_updated += 1;
                        }
                    },
                    .create => {
                        next_scope_ids[key_index] = self.createEachRowScope(ctx, parent_scope_id, site_ordinal, key, item, key_cap, item_cap);
                        row_items_changed[key_index] = true;
                        scope_created[key_index] = true;
                    },
                }
            }

            for (match_plan.removed_scope_ids) |scope_id| {
                self.disposeScopeSubtree(ctx, roc_host, scope_id);
            }

            var metrics = self.pending_roc_metrics;
            metrics.bump(.rows_reused, match_plan.rows_reused);
            metrics.bump(.rows_created, match_plan.rows_created);
            metrics.bump(.rows_removed, match_plan.rows_removed);
            self.pending_roc_metrics = metrics;

            return .{
                .scope_ids = next_scope_ids,
                .row_items_changed = row_items_changed,
                .scope_created = scope_created,
                .removed_scope_ids = match_plan.removed_scope_ids,
                .rows_reused = match_plan.rows_reused,
                .rows_created = match_plan.rows_created,
                .rows_removed = match_plan.rows_removed,
                .row_items_unchanged = row_items_unchanged,
                .row_items_updated = row_items_updated,
            };
        }

        pub fn syncActiveEachRowScopes(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc) HostKeyedRowDiffResult {
            if (site.kind != .each) {
                @panic("active row sync requires an each scope site");
            }
            if (site.node_id != each.node_id) {
                @panic("active row sync received mismatched each descriptors");
            }

            const allocator = Ctx.allocator(ctx);
            const items_value = self.cloneCachedSignalValue(ctx, &each.cached_value);
            const items_cap = self.hostSignalBindingCapability(ctx, &each.items);
            assertHostValueCapabilitiesMatch(each.ops.items_capability, items_cap, "each items extension capability did not match its signal value");
            defer callHostValueToUnitWithCapability(ctx, roc_host, items_cap, hv.hostValueCapabilityDrop(items_cap), items_value);

            const items = callHostValueToHostValueListWithCapability(ctx, roc_host, each.ops.items_capability, each.ops.items_to_values, items_value);
            defer items.decref(roc_host);
            const item_values = items.items();

            const keys = allocator.alloc(HostValue, item_values.len) catch @panic("out of memory");
            defer allocator.free(keys);

            for (item_values, 0..) |item, index| {
                keys[index] = callHostValueToHostValueWithCapability(ctx, roc_host, each.ops.item_capability, each.ops.key_of, item);
            }

            return self.syncEachRowScopes(ctx, roc_host, site.scope_id, site.ordinal, keys, item_values, each.ops);
        }

        pub fn collectNodeAttrDescriptor(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem_id: u64, attr: abi.NodeAttr, binder_stack: []const HostBinderBinding) void {
            const allocator = Ctx.allocator(ctx);
            switch (attr.tag) {
                .StaticText => {
                    const payload = attr.payload_static_text();
                    const field = renderTextFieldFromAbi(payload.field);
                    stream.appendStaticTextAttr(allocator, elem_id, field, payload.value.asSlice());
                },
                .SignalText => {
                    const payload = attr.payload_signal_text();
                    const field = renderTextFieldFromAbi(payload.field);
                    const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                    stream.appendSignalTextAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, field, signal, payload.read);
                },
                .StaticBool => {
                    const payload = attr.payload_static_bool();
                    const field = renderBoolFieldFromAbi(payload.field);
                    stream.appendStaticBoolAttr(allocator, elem_id, field, payload.value);
                },
                .SignalBool => {
                    const payload = attr.payload_signal_bool();
                    const field = renderBoolFieldFromAbi(payload.field);
                    const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack);
                    stream.appendSignalBoolAttr(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, field, signal, payload.read);
                },
                .OnEvent => {
                    const payload = attr.payload_on_event();
                    const msg = payload.msg;
                    const kind = renderEventKindFromAbi(payload.kind);
                    const payload_kind = eventPayloadKindFromAbi(msg.payload_kind);
                    const payload_accessor = eventPayloadAccessorFromAbi(msg.payload_accessor);
                    const target_node_id = resolveNodeBinderRef(binder_stack, msg.binder);
                    stream.appendEvent(allocator, roc_host, &self.pending_roc_metrics, elem_id, kind, msg.binder, target_node_id, payload_kind, payload_accessor, msg.payload_reducer);
                },
            }
        }

        pub fn collectActiveWhenBranchDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, when: HostNodeWhenDesc, active_branch: HostScopeBranch, dirty_source_node_ids: []const u64) u64 {
            if (site.kind != .when) {
                @panic("active branch collection requires a when scope site");
            }
            if (site.node_id != when.node_id) {
                @panic("active branch collection received mismatched when descriptors");
            }

            if (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite()) catch @panic("scope id has no host scope descriptor")) |inactive_scope_id| {
                self.disposeScopeSubtree(ctx, roc_host, inactive_scope_id);
            }

            const branch_scope = self.internWhenBranchScope(Ctx.allocator(ctx), site.scope_id, site.ordinal, active_branch) catch @panic("scope id has no host scope descriptor");
            const branch_scope_id = branch_scope.scope_id;
            const allocator = Ctx.allocator(ctx);
            var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
            defer binder_stack.deinit(allocator);
            binder_stack.appendSlice(allocator, site.binder_bindings) catch @panic("out of memory");

            const branch_elem = switch (active_branch) {
                .true_branch => when.when_true,
                .false_branch => when.when_false,
            };
            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(ctx, roc_host, stream, branch_elem, branch_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack, branch_scope.created, dirty_source_node_ids);
            return branch_scope_id;
        }

        pub fn collectActiveEachRowDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, dirty_source_node_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            const diff = self.syncActiveEachRowScopes(ctx, roc_host, site, each);
            defer diff.deinit(allocator);
            self.collectActiveEachRowDescriptorsFromDiff(ctx, roc_host, stream, site, each, diff, dirty_source_node_ids);
        }

        pub fn collectActiveEachRowDescriptorsFromDiff(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
            defer binder_stack.deinit(allocator);
            binder_stack.appendSlice(allocator, site.binder_bindings) catch @panic("out of memory");

            for (diff.scope_ids, diff.row_items_changed, diff.scope_created) |row_scope_id, row_item_changed, row_created| {
                if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                    self.copyActiveScopeSubtreeDescriptors(ctx, roc_host, stream, row_scope_id);
                    continue;
                }

                const row_values = self.eachRowScopeValues(row_scope_id);
                const row_elem = callHostValueHostValueToElemWithCapabilities(ctx, roc_host, each.ops.key_capability, each.ops.item_capability, each.ops.row, row_values.key, row_values.item);
                defer abi.decrefElem(row_elem, roc_host);

                var ordinal: u64 = 0;
                var dom_ordinal: u64 = 0;
                self.collectActiveElemDescriptors(ctx, roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack, row_created, dirty_source_node_ids);
            }
        }

        pub fn collectActiveEachSingleRowDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, row_scope_id: u64, row_created: bool, dirty_source_node_ids: []const u64) void {
            const allocator = Ctx.allocator(ctx);
            var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
            defer binder_stack.deinit(allocator);
            binder_stack.appendSlice(allocator, site.binder_bindings) catch @panic("out of memory");

            const row_values = self.eachRowScopeValues(row_scope_id);
            const row_elem = callHostValueHostValueToElemWithCapabilities(ctx, roc_host, each.ops.key_capability, each.ops.item_capability, each.ops.row, row_values.key, row_values.item);
            defer abi.decrefElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(ctx, roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack, row_created, dirty_source_node_ids);
        }

        pub fn collectActiveElemDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem: abi.Elem, scope_id: u64, parent_elem_id: u64, ordinal: *u64, dom_ordinal: *u64, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding), scope_created: bool, dirty_source_node_ids: []const u64) void {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");

            const allocator = Ctx.allocator(ctx);
            switch (elem.tag) {
                .Element => {
                    const payload = elem.payload_element();
                    const elem_id = self.internDomIdentity(Ctx.allocator(ctx), scope_id, dom_ordinal.*) catch @panic("scope id has no host scope descriptor");
                    dom_ordinal.* += 1;
                    _ = stream.appendElement(allocator, elem_id, parent_elem_id, scope_id, payload.tag.asSlice());
                    for (payload.attrs.items()) |attr| {
                        self.collectNodeAttrDescriptor(ctx, roc_host, stream, elem_id, attr, binder_stack.items);
                    }
                    for (payload.children.items()) |child| {
                        self.collectActiveElemDescriptors(ctx, roc_host, stream, child, scope_id, elem_id, ordinal, dom_ordinal, binder_stack, scope_created, dirty_source_node_ids);
                    }
                },
                .Text => {
                    const elem_id = self.internDomIdentity(Ctx.allocator(ctx), scope_id, dom_ordinal.*) catch @panic("scope id has no host scope descriptor");
                    dom_ordinal.* += 1;
                    stream.appendTextNode(allocator, elem_id, parent_elem_id, scope_id, elem.payload_text().asSlice());
                },
                .TextSignal => {
                    const elem_id = self.internDomIdentity(Ctx.allocator(ctx), scope_id, dom_ordinal.*) catch @panic("scope id has no host scope descriptor");
                    dom_ordinal.* += 1;
                    const text_signal = elem.payload_text_signal();
                    const signal = self.bindNodeSignal(allocator, stream, text_signal.signal.*, binder_stack.items);
                    stream.appendSignalTextNode(allocator, ctx, roc_host, &self.pending_roc_metrics, elem_id, parent_elem_id, scope_id, signal, text_signal.read);
                },
                .Cleanup => {
                    stream.appendCleanup(allocator, scope_id, elem.payload_cleanup().cleanup.asSlice());
                },
                .OnChange => {
                    const payload = elem.payload_on_change();
                    const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack.items);
                    stream.appendOnChange(allocator, ctx, roc_host, &self.pending_roc_metrics, scope_id, signal, payload.to_cmd);
                },
                .OnMount => {
                    const payload = elem.payload_on_mount();
                    stream.appendMount(allocator, roc_host, &self.pending_roc_metrics, scope_id, payload.to_cmd, scope_created);
                },
                .State => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                    const state = elem.payload_state();
                    stream.appendState(allocator, roc_host, &self.pending_roc_metrics, node_id, state.initial, state.cap);
                    self.ensureStateFromDesc(ctx, roc_host, stream.states.items[stream.states.items.len - 1]);
                    binder_stack.append(allocator, .{ .token = state.binder, .node_id = node_id }) catch @panic("out of memory");
                    self.collectActiveElemDescriptors(ctx, roc_host, stream, state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack, scope_created, dirty_source_node_ids);
                    _ = binder_stack.pop() orelse unreachable;
                },
                .Component => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .component, binder_stack.items);
                    const component_scope = self.internComponentScope(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    const component_scope_id = component_scope.scope_id;
                    var component_ordinal: u64 = 0;
                    var component_dom_ordinal: u64 = 0;
                    self.collectActiveElemDescriptors(ctx, roc_host, stream, elem.payload_component().child.*, component_scope_id, parent_elem_id, &component_ordinal, &component_dom_ordinal, binder_stack, component_scope.created, dirty_source_node_ids);
                },
                .When => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                    const when_payload = elem.payload_when();
                    const condition_binding = self.bindNodeSignal(allocator, stream, when_payload.condition.*, binder_stack.items);
                    stream.appendWhen(allocator, ctx, roc_host, &self.pending_roc_metrics, node_id, condition_binding, when_payload.read, when_payload.when_false.*, when_payload.when_true.*);

                    const when_index = stream.whens.items.len - 1;
                    const when_desc = &stream.whens.items[when_index];
                    const condition = self.evalHostSignalBinding(ctx, roc_host, &when_desc.condition);
                    const condition_cap = self.hostSignalBindingCapability(ctx, &when_desc.condition);
                    assertHostValueCapabilitiesMatch(when_desc.read.capability, condition_cap, "when read extension capability did not match its signal value");
                    const active_branch: HostScopeBranch = if (callHostValueToBoolWithCapability(ctx, roc_host, when_desc.read.capability, when_desc.read.read, condition)) .true_branch else .false_branch;
                    when_desc.cached_value.replace(ctx, roc_host, &self.pending_roc_metrics, condition, condition_cap);
                    if (self.activeWhenBranchScopeId(scope_id, site_ordinal, active_branch.opposite()) catch @panic("scope id has no host scope descriptor")) |inactive_scope_id| {
                        self.disposeScopeSubtree(ctx, roc_host, inactive_scope_id);
                    }
                    const branch_scope = self.internWhenBranchScope(Ctx.allocator(ctx), scope_id, site_ordinal, active_branch) catch @panic("scope id has no host scope descriptor");
                    const branch_scope_id = branch_scope.scope_id;
                    var branch_ordinal: u64 = 0;
                    const branch_elem = switch (active_branch) {
                        .true_branch => when_payload.when_true.*,
                        .false_branch => when_payload.when_false.*,
                    };
                    var branch_dom_ordinal: u64 = 0;
                    self.collectActiveElemDescriptors(ctx, roc_host, stream, branch_elem, branch_scope_id, parent_elem_id, &branch_ordinal, &branch_dom_ordinal, binder_stack, branch_scope.created, dirty_source_node_ids);
                },
                .Each => {
                    const site_ordinal = ordinal.*;
                    const node_id = self.internNodeIdentity(Ctx.allocator(ctx), scope_id, site_ordinal) catch @panic("scope id has no host scope descriptor");
                    ordinal.* += 1;
                    stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                    const each_payload = elem.payload_each();
                    const items_binding = self.bindNodeSignal(allocator, stream, each_payload.items.*, binder_stack.items);
                    stream.appendEach(allocator, ctx, roc_host, &self.pending_roc_metrics, node_id, items_binding, each_payload.ops);
                    const each_index = stream.eaches.items.len - 1;
                    const each_desc = stream.eaches.items[stream.eaches.items.len - 1];

                    const items_value = self.evalHostSignalBinding(ctx, roc_host, &stream.eaches.items[each_index].items);
                    const each_items_cap = self.hostSignalBindingCapability(ctx, &stream.eaches.items[each_index].items);
                    assertHostValueCapabilitiesMatch(each_desc.ops.items_capability, each_items_cap, "each items extension capability did not match its signal value");
                    const items = callHostValueToHostValueListWithCapability(ctx, roc_host, each_desc.ops.items_capability, each_desc.ops.items_to_values, items_value);
                    defer items.decref(roc_host);
                    stream.eaches.items[each_index].cached_value.replace(ctx, roc_host, &self.pending_roc_metrics, items_value, each_items_cap);
                    const item_values = items.items();

                    const keys = allocator.alloc(HostValue, item_values.len) catch @panic("out of memory");
                    defer allocator.free(keys);

                    for (item_values, 0..) |item, index| {
                        keys[index] = callHostValueToHostValueWithCapability(ctx, roc_host, each_desc.ops.item_capability, each_desc.ops.key_of, item);
                    }

                    const diff = self.syncEachRowScopes(ctx, roc_host, scope_id, site_ordinal, keys, item_values, each_desc.ops);
                    defer diff.deinit(allocator);

                    for (diff.scope_ids, diff.row_items_changed, diff.scope_created) |row_scope_id, row_item_changed, row_created| {
                        if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                            self.copyActiveScopeSubtreeDescriptors(ctx, roc_host, stream, row_scope_id);
                            continue;
                        }

                        const row_values = self.eachRowScopeValues(row_scope_id);
                        const row_elem = callHostValueHostValueToElemWithCapabilities(ctx, roc_host, each_desc.ops.key_capability, each_desc.ops.item_capability, each_desc.ops.row, row_values.key, row_values.item);
                        defer abi.decrefElem(row_elem, roc_host);

                        var row_ordinal: u64 = 0;
                        var row_dom_ordinal: u64 = 0;
                        self.collectActiveElemDescriptors(ctx, roc_host, stream, row_elem, row_scope_id, parent_elem_id, &row_ordinal, &row_dom_ordinal, binder_stack, row_created, dirty_source_node_ids);
                    }
                },
            }
        }

        pub fn collectActiveElemRootDescriptors(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem, dirty_source_node_ids: []const u64) void {
            const root_scope = self.internRootScope(Ctx.allocator(ctx)) catch @panic("scope id has no host scope descriptor");
            const root_scope_id = root_scope.scope_id;
            const allocator = Ctx.allocator(ctx);
            var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
            defer binder_stack.deinit(allocator);
            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectActiveElemDescriptors(ctx, roc_host, stream, root, root_scope_id, 0, &ordinal, &dom_ordinal, &binder_stack, root_scope.created, dirty_source_node_ids);
        }

        pub fn clearActiveSignalGraph(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            const roc_host = self.roc_host orelse {
                if (self.active_signal_graph.items.len != 0) @panic("active signal graph cannot release records without a Roc host");
                self.active_signal_graph.items.len = 0;
                return;
            };
            for (self.active_signal_graph.items, 0..) |node, index| {
                allocator.free(node.dependents);
                const active_graph_id = node.record.active_graph_id orelse @panic("active signal graph record was missing its dense id");
                if (active_graph_id != index) @panic("active signal graph record dense id did not match its slot");
                node.record.active_graph_id = null;
                node.record.active_use_count = 0;
                node.record.release(allocator, ctx, roc_host, &self.pending_roc_metrics);
            }
            self.active_signal_graph.items.len = 0;
        }

        pub fn clearActiveIntervals(self: *Self, ctx: Ctx.Handle) void {
            const roc_host = self.roc_host orelse {
                if (self.active_intervals.items.len != 0) @panic("active intervals cannot release tokens without a Roc host");
                self.active_intervals.items.len = 0;
                return;
            };
            for (self.active_intervals.items) |interval| {
                if (interval.active) {
                    Ctx.sink(ctx).cancelInterval(interval.token);
                }
                releaseHostSignalToken(interval.source_token, roc_host);
            }
            self.active_intervals.clearRetainingCapacity();
        }

        fn markActiveIntervalsInactive(self: *Self) void {
            for (self.active_intervals.items) |*interval| {
                interval.active = false;
            }
        }

        fn activeIntervalBySourceToken(self: *Self, source_token: HostSignalToken) ?*HostActiveInterval {
            var found: ?*HostActiveInterval = null;
            for (self.active_intervals.items) |*interval| {
                if (interval.source_token != source_token) continue;
                if (found != null) @panic("interval source token matched more than one runtime interval");
                found = interval;
            }
            return found;
        }

        fn ensureActiveInterval(self: *Self, ctx: Ctx.Handle, source_token: HostSignalToken, period_ms: u64) void {
            if (self.activeIntervalBySourceToken(source_token)) |interval| {
                if (interval.period_ms != period_ms) @panic("interval source token changed period");
                interval.active = true;
                return;
            }

            if (self.next_interval_token == std.math.maxInt(u64)) @panic("host interval token overflowed");
            const token = self.next_interval_token;
            self.next_interval_token += 1;
            abi.increfBox(@ptrCast(source_token), 1);
            self.active_intervals.append(Ctx.allocator(ctx), .{
                .token = token,
                .source_token = source_token,
                .period_ms = period_ms,
                .active = true,
            }) catch {
                releaseHostSignalToken(source_token, self.roc_host.?);
                @panic("out of memory");
            };
            Ctx.sink(ctx).startInterval(token, period_ms);
        }

        fn syncActiveIntervalsFromGraph(self: *Self, ctx: Ctx.Handle) void {
            self.markActiveIntervalsInactive();

            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| self.ensureActiveInterval(ctx, payload.token, payload.period_ms),
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }

            const roc_host = self.roc_host orelse {
                for (self.active_intervals.items) |interval| {
                    if (!interval.active) @panic("inactive interval cannot release token without a Roc host");
                }
                return;
            };

            var write_index: usize = 0;
            for (self.active_intervals.items) |interval| {
                if (!interval.active) {
                    Ctx.sink(ctx).cancelInterval(interval.token);
                    releaseHostSignalToken(interval.source_token, roc_host);
                    continue;
                }
                self.active_intervals.items[write_index] = interval;
                write_index += 1;
            }
            self.active_intervals.items.len = write_index;
        }

        pub fn clearActiveSignalRoutes(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            for (self.active_source_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_source_signal_routes.items.len = 0;

            for (self.active_text_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_text_signal_routes.items.len = 0;

            for (self.active_bool_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_bool_signal_routes.items.len = 0;

            for (self.active_change_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_change_signal_routes.items.len = 0;

            for (self.active_structural_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_structural_signal_routes.items.len = 0;
        }

        pub fn clearActiveSinkSignalRoutes(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            for (self.active_text_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_text_signal_routes.items.len = 0;

            for (self.active_bool_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_bool_signal_routes.items.len = 0;

            for (self.active_change_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_change_signal_routes.items.len = 0;

            for (self.active_structural_signal_routes.items) |*route| {
                route.deinit(allocator);
            }
            self.active_structural_signal_routes.items.len = 0;
        }

        pub fn activeSignalRecordId(self: *Self, record: *const HostSignalRecord) ?u64 {
            const record_id = record.active_graph_id orelse return null;
            if (record_id >= self.active_signal_graph.items.len) @panic("active signal record dense id exceeded the graph table");
            if (self.active_signal_graph.items[@intCast(record_id)].record != record) {
                @panic("active signal record dense id pointed at a different record");
            }
            return record_id;
        }

        pub fn requireActiveSignalRecordId(self: *Self, record: *const HostSignalRecord) u64 {
            return self.activeSignalRecordId(record) orelse @panic("active signal graph referenced a record that was not registered");
        }

        pub fn appendActiveSignalGraphNode(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord, rank: u64) u64 {
            const allocator = Ctx.allocator(ctx);
            const record_id: u64 = @intCast(self.active_signal_graph.items.len);
            self.active_signal_graph.append(allocator, .{
                .record = record.retain(),
                .rank = rank,
            }) catch @panic("out of memory");
            record.active_graph_id = record_id;
            self.pending_roc_metrics.bump(.active_graph_records_rebuilt, 1);
            return record_id;
        }

        pub fn appendActiveSignalDependentId(self: *Self, ctx: Ctx.Handle, input_record_id: u64, dependent_record_id: u64) void {
            signal_graph.appendDependent(HostSignalRecord, Ctx.allocator(ctx), self.active_signal_graph.items, input_record_id, dependent_record_id) catch |err| switch (err) {
                error.OutOfMemory => @panic("out of memory"),
                error.UnknownNode => @panic("active signal dependent referenced an unknown input record"),
                else => @panic("active signal dependent insertion missed its edge"),
            };
        }

        pub fn appendActiveSourceSignalRoute(self: *Self, ctx: Ctx.Handle, source_node_id: u64, record_id: u64) void {
            const route = self.ensureActiveSourceSignalRoute(ctx, source_node_id);
            if (!u64SliceContains(route.items, record_id)) {
                route.append(Ctx.allocator(ctx), record_id) catch @panic("out of memory");
            }
        }

        pub fn retainActiveSignalRecord(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord) void {
            if (record.active_use_count != 0) {
                record.active_use_count += 1;
                return;
            }

            record.active_use_count = 1;
            var rank: u64 = 0;

            switch (record.payload) {
                .ref, .const_value, .task_source, .interval_source => {},
                .map => |payload| {
                    self.retainActiveSignalRecord(ctx, payload.input);
                    const input_id = self.requireActiveSignalRecordId(payload.input);
                    rank = self.active_signal_graph.items[@intCast(input_id)].rank + 1;
                },
                .map2 => |payload| {
                    self.retainActiveSignalRecord(ctx, payload.left);
                    if (payload.right != payload.left) {
                        self.retainActiveSignalRecord(ctx, payload.right);
                    }
                    const left_id = self.requireActiveSignalRecordId(payload.left);
                    const right_id = self.requireActiveSignalRecordId(payload.right);
                    rank = @max(
                        self.active_signal_graph.items[@intCast(left_id)].rank,
                        self.active_signal_graph.items[@intCast(right_id)].rank,
                    ) + 1;
                },
                .combine => |payload| {
                    for (payload.children, 0..) |child, index| {
                        if (recordSliceContains(payload.children[0..index], child)) continue;
                        self.retainActiveSignalRecord(ctx, child);
                        const child_id = self.requireActiveSignalRecordId(child);
                        rank = @max(rank, self.active_signal_graph.items[@intCast(child_id)].rank + 1);
                    }
                },
            }

            const record_id = self.appendActiveSignalGraphNode(ctx, record, rank);

            switch (record.payload) {
                .ref => |source_node_id| self.appendActiveSourceSignalRoute(ctx, source_node_id, record_id),
                .const_value, .task_source, .interval_source => {},
                .map => |payload| self.appendActiveSignalDependentId(ctx, self.requireActiveSignalRecordId(payload.input), record_id),
                .map2 => |payload| {
                    self.appendActiveSignalDependentId(ctx, self.requireActiveSignalRecordId(payload.left), record_id);
                    if (payload.right != payload.left) {
                        self.appendActiveSignalDependentId(ctx, self.requireActiveSignalRecordId(payload.right), record_id);
                    }
                },
                .combine => |payload| {
                    for (payload.children, 0..) |child, index| {
                        if (recordSliceContains(payload.children[0..index], child)) continue;
                        self.appendActiveSignalDependentId(ctx, self.requireActiveSignalRecordId(child), record_id);
                    }
                },
            }
        }

        pub fn ensureActiveSourceSignalRoute(self: *Self, ctx: Ctx.Handle, source_node_id: u64) *std.ArrayListUnmanaged(u64) {
            if (source_node_id >= self.node_identities.items.len) @panic("active source signal route referenced an unknown source node");
            const allocator = Ctx.allocator(ctx);
            const route_index: usize = @intCast(source_node_id);
            while (self.active_source_signal_routes.items.len <= route_index) {
                self.active_source_signal_routes.append(allocator, .empty) catch @panic("out of memory");
            }
            return &self.active_source_signal_routes.items[route_index];
        }

        pub fn ensureActiveTextSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveTextSignalSink) {
            if (record_id >= self.active_signal_graph.items.len) @panic("active text signal route referenced an unknown signal record");
            const allocator = Ctx.allocator(ctx);
            const route_index: usize = @intCast(record_id);
            while (self.active_text_signal_routes.items.len <= route_index) {
                self.active_text_signal_routes.append(allocator, .empty) catch @panic("out of memory");
            }
            return &self.active_text_signal_routes.items[route_index];
        }

        pub fn ensureActiveBoolSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveBoolSignalSink) {
            if (record_id >= self.active_signal_graph.items.len) @panic("active bool signal route referenced an unknown signal record");
            const allocator = Ctx.allocator(ctx);
            const route_index: usize = @intCast(record_id);
            while (self.active_bool_signal_routes.items.len <= route_index) {
                self.active_bool_signal_routes.append(allocator, .empty) catch @panic("out of memory");
            }
            return &self.active_bool_signal_routes.items[route_index];
        }

        pub fn ensureActiveChangeSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveChangeSignalSink) {
            if (record_id >= self.active_signal_graph.items.len) @panic("active change signal route referenced an unknown signal record");
            const allocator = Ctx.allocator(ctx);
            const route_index: usize = @intCast(record_id);
            while (self.active_change_signal_routes.items.len <= route_index) {
                self.active_change_signal_routes.append(allocator, .empty) catch @panic("out of memory");
            }
            return &self.active_change_signal_routes.items[route_index];
        }

        pub fn ensureActiveStructuralSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64) *std.ArrayListUnmanaged(HostActiveStructuralSignal) {
            if (record_id >= self.active_signal_graph.items.len) @panic("active structural signal route referenced an unknown signal record");
            const allocator = Ctx.allocator(ctx);
            const route_index: usize = @intCast(record_id);
            while (self.active_structural_signal_routes.items.len <= route_index) {
                self.active_structural_signal_routes.append(allocator, .empty) catch @panic("out of memory");
            }
            return &self.active_structural_signal_routes.items[route_index];
        }

        fn removeActiveRouteTableRecordId(
            comptime Route: type,
            allocator: std.mem.Allocator,
            routes: *std.ArrayListUnmanaged(std.ArrayListUnmanaged(Route)),
            record_index: usize,
            last_index: usize,
            comptime live_route_message: []const u8,
        ) void {
            if (routes.items.len > last_index + 1) @panic("active sink route table exceeded active signal graph length");
            if (record_index >= routes.items.len) return;

            if (routes.items[record_index].items.len != 0) @panic(live_route_message);
            routes.items[record_index].deinit(allocator);

            if (record_index != last_index and last_index < routes.items.len) {
                routes.items[record_index] = routes.items[last_index];
                routes.items[last_index] = .empty;
            } else {
                routes.items[record_index] = .empty;
            }

            if (routes.items.len == last_index + 1) {
                routes.items.len = last_index;
            }
        }

        fn removeActiveSinkRoutesForRecordId(self: *Self, ctx: Ctx.Handle, record_index: usize, last_index: usize) void {
            const allocator = Ctx.allocator(ctx);
            removeActiveRouteTableRecordId(
                HostActiveTextSignalSink,
                allocator,
                &self.active_text_signal_routes,
                record_index,
                last_index,
                "active signal graph removed a record with live text sinks",
            );
            removeActiveRouteTableRecordId(
                HostActiveBoolSignalSink,
                allocator,
                &self.active_bool_signal_routes,
                record_index,
                last_index,
                "active signal graph removed a record with live bool sinks",
            );
            removeActiveRouteTableRecordId(
                HostActiveChangeSignalSink,
                allocator,
                &self.active_change_signal_routes,
                record_index,
                last_index,
                "active signal graph removed a record with live change sinks",
            );
            removeActiveRouteTableRecordId(
                HostActiveStructuralSignal,
                allocator,
                &self.active_structural_signal_routes,
                record_index,
                last_index,
                "active signal graph removed a record with live structural sinks",
            );
        }

        fn appendActiveTextSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveTextSignalSink) void {
            self.ensureActiveTextSignalRoute(ctx, record_id).append(Ctx.allocator(ctx), route) catch @panic("out of memory");
        }

        fn removeActiveTextSignalRoute(self: *Self, record_id: u64, kind: HostActiveTextSignalSinkKind, index: usize) void {
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_text_signal_routes.items.len) @panic("active text signal route removal referenced an unknown signal record");
            var route = &self.active_text_signal_routes.items[route_index];
            for (route.items, 0..) |sink, sink_index| {
                if (sink.kind == kind and sink.index == index) {
                    _ = route.swapRemove(sink_index);
                    return;
                }
            }
            @panic("active text signal route removal missed its sink");
        }

        fn updateActiveTextSignalRouteIndex(self: *Self, record_id: u64, kind: HostActiveTextSignalSinkKind, old_index: usize, new_index: usize) void {
            if (old_index == new_index) return;
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_text_signal_routes.items.len) @panic("active text signal route update referenced an unknown signal record");
            for (self.active_text_signal_routes.items[route_index].items) |*sink| {
                if (sink.kind == kind and sink.index == old_index) {
                    sink.index = new_index;
                    return;
                }
            }
            @panic("active text signal route update missed its sink");
        }

        fn appendActiveBoolSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveBoolSignalSink) void {
            self.ensureActiveBoolSignalRoute(ctx, record_id).append(Ctx.allocator(ctx), route) catch @panic("out of memory");
        }

        fn removeActiveBoolSignalRoute(self: *Self, record_id: u64, index: usize) void {
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_bool_signal_routes.items.len) @panic("active bool signal route removal referenced an unknown signal record");
            var route = &self.active_bool_signal_routes.items[route_index];
            for (route.items, 0..) |sink, sink_index| {
                if (sink.index == index) {
                    _ = route.swapRemove(sink_index);
                    return;
                }
            }
            @panic("active bool signal route removal missed its sink");
        }

        fn updateActiveBoolSignalRouteIndex(self: *Self, record_id: u64, old_index: usize, new_index: usize) void {
            if (old_index == new_index) return;
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_bool_signal_routes.items.len) @panic("active bool signal route update referenced an unknown signal record");
            for (self.active_bool_signal_routes.items[route_index].items) |*sink| {
                if (sink.index == old_index) {
                    sink.index = new_index;
                    return;
                }
            }
            @panic("active bool signal route update missed its sink");
        }

        fn appendActiveChangeSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveChangeSignalSink) void {
            self.ensureActiveChangeSignalRoute(ctx, record_id).append(Ctx.allocator(ctx), route) catch @panic("out of memory");
        }

        fn removeActiveChangeSignalRoute(self: *Self, record_id: u64, index: usize) void {
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_change_signal_routes.items.len) @panic("active change signal route removal referenced an unknown signal record");
            var route = &self.active_change_signal_routes.items[route_index];
            for (route.items, 0..) |sink, sink_index| {
                if (sink.index == index) {
                    _ = route.swapRemove(sink_index);
                    return;
                }
            }
            @panic("active change signal route removal missed its sink");
        }

        fn updateActiveChangeSignalRouteIndex(self: *Self, record_id: u64, old_index: usize, new_index: usize) void {
            if (old_index == new_index) return;
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_change_signal_routes.items.len) @panic("active change signal route update referenced an unknown signal record");
            for (self.active_change_signal_routes.items[route_index].items) |*sink| {
                if (sink.index == old_index) {
                    sink.index = new_index;
                    return;
                }
            }
            @panic("active change signal route update missed its sink");
        }

        fn appendActiveStructuralSignalRoute(self: *Self, ctx: Ctx.Handle, record_id: u64, route: HostActiveStructuralSignal) void {
            self.ensureActiveStructuralSignalRoute(ctx, record_id).append(Ctx.allocator(ctx), route) catch @panic("out of memory");
        }

        fn removeActiveStructuralSignalRoute(self: *Self, record_id: u64, kind: HostActiveStructuralSignalKind, index: usize) void {
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_structural_signal_routes.items.len) @panic("active structural signal route removal referenced an unknown signal record");
            var route = &self.active_structural_signal_routes.items[route_index];
            for (route.items, 0..) |sink, sink_index| {
                if (sink.kind == kind and sink.index == index) {
                    _ = route.swapRemove(sink_index);
                    return;
                }
            }
            @panic("active structural signal route removal missed its sink");
        }

        fn updateActiveStructuralSignalRouteIndex(self: *Self, record_id: u64, kind: HostActiveStructuralSignalKind, old_index: usize, new_index: usize) void {
            if (old_index == new_index) return;
            const route_index: usize = @intCast(record_id);
            if (route_index >= self.active_structural_signal_routes.items.len) @panic("active structural signal route update referenced an unknown signal record");
            for (self.active_structural_signal_routes.items[route_index].items) |*sink| {
                if (sink.kind == kind and sink.index == old_index) {
                    sink.index = new_index;
                    return;
                }
            }
            @panic("active structural signal route update missed its sink");
        }

        pub fn rebuildActiveSinkSignalRoutesFromStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream) void {
            self.clearActiveSinkSignalRoutes(ctx);

            for (stream.signal_text_nodes.items, 0..) |desc, index| {
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .text_node,
                    .index = index,
                });
            }

            for (stream.signal_text_attrs.items, 0..) |desc, index| {
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .text_attr,
                    .index = index,
                });
            }

            for (stream.signal_bool_attrs.items, 0..) |desc, index| {
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveBoolSignalRoute(ctx, record_id, .{
                    .index = index,
                });
            }

            for (stream.on_changes.items, 0..) |desc, index| {
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveChangeSignalRoute(ctx, record_id, .{
                    .index = index,
                });
            }

            for (stream.whens.items, 0..) |desc, index| {
                const record_id = self.requireActiveSignalRecordId(desc.condition.record);
                self.appendActiveStructuralSignalRoute(ctx, record_id, .{
                    .kind = .when,
                    .index = index,
                });
            }

            for (stream.eaches.items, 0..) |desc, index| {
                const record_id = self.requireActiveSignalRecordId(desc.items.record);
                self.appendActiveStructuralSignalRoute(ctx, record_id, .{
                    .kind = .each,
                    .index = index,
                });
            }
        }

        pub fn rebuildActiveSignalGraphFromStream(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream) void {
            self.clearActiveSignalRoutes(ctx);
            self.clearActiveSignalGraph(ctx);

            for (stream.signal_text_nodes.items) |*desc| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
            }
            for (stream.signal_text_attrs.items) |*desc| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
            }
            for (stream.signal_bool_attrs.items) |*desc| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
            }
            for (stream.on_changes.items) |*desc| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
            }
            for (stream.whens.items) |*desc| {
                self.retainActiveSignalRecord(ctx, desc.condition.record);
            }
            for (stream.eaches.items) |*desc| {
                self.retainActiveSignalRecord(ctx, desc.items.record);
            }

            self.rebuildActiveSinkSignalRoutesFromStream(ctx, stream);
            self.syncActiveIntervalsFromGraph(ctx);
        }

        fn removeActiveSignalDependentId(self: *Self, ctx: Ctx.Handle, input_record_id: u64, dependent_record_id: u64) void {
            signal_graph.removeDependent(HostSignalRecord, Ctx.allocator(ctx), self.active_signal_graph.items, input_record_id, dependent_record_id) catch |err| switch (err) {
                error.OutOfMemory => @panic("out of memory"),
                error.UnknownNode => @panic("active signal dependent removal referenced an unknown input record"),
                else => @panic("active signal dependent removal missed its edge"),
            };
        }

        fn replaceActiveSignalDependentId(self: *Self, input_record_id: u64, old_dependent_id: u64, new_dependent_id: u64) void {
            signal_graph.replaceDependent(HostSignalRecord, self.active_signal_graph.items, input_record_id, old_dependent_id, new_dependent_id) catch |err| switch (err) {
                error.UnknownNode => @panic("active signal dependent rewrite referenced an unknown input record"),
                else => @panic("active signal dependent rewrite missed its edge"),
            };
        }

        fn removeActiveSourceSignalRoute(self: *Self, source_node_id: u64, record_id: u64) void {
            if (source_node_id >= self.active_source_signal_routes.items.len) @panic("active source signal route removal referenced an unknown source node");
            var route = &self.active_source_signal_routes.items[@intCast(source_node_id)];
            for (route.items, 0..) |existing_id, index| {
                if (existing_id != record_id) continue;
                _ = route.swapRemove(index);
                return;
            }
            @panic("active source signal route removal missed its record");
        }

        fn replaceActiveSourceSignalRouteId(self: *Self, source_node_id: u64, old_record_id: u64, new_record_id: u64) void {
            if (source_node_id >= self.active_source_signal_routes.items.len) @panic("active source signal route rewrite referenced an unknown source node");
            const route = self.active_source_signal_routes.items[@intCast(source_node_id)].items;
            for (route) |*existing_id| {
                if (existing_id.* != old_record_id) continue;
                existing_id.* = new_record_id;
                return;
            }
            @panic("active source signal route rewrite missed its record");
        }

        fn appendUniqueActiveInputRecord(ctx: Ctx.Handle, records: *std.ArrayListUnmanaged(*HostSignalRecord), record: *HostSignalRecord) void {
            if (!recordSliceContains(records.items, record)) {
                records.append(Ctx.allocator(ctx), record) catch @panic("out of memory");
            }
        }

        fn appendActiveSignalInputRecords(ctx: Ctx.Handle, records: *std.ArrayListUnmanaged(*HostSignalRecord), record: *HostSignalRecord) void {
            switch (record.payload) {
                .ref, .const_value, .task_source, .interval_source => {},
                .map => |payload| appendUniqueActiveInputRecord(ctx, records, payload.input),
                .map2 => |payload| {
                    appendUniqueActiveInputRecord(ctx, records, payload.left);
                    appendUniqueActiveInputRecord(ctx, records, payload.right);
                },
                .combine => |payload| {
                    for (payload.children) |child| {
                        appendUniqueActiveInputRecord(ctx, records, child);
                    }
                },
            }
        }

        fn updateMovedActiveSignalRecordEdges(self: *Self, moved_record: *HostSignalRecord, old_record_id: u64, new_record_id: u64) void {
            switch (moved_record.payload) {
                .ref => |source_node_id| self.replaceActiveSourceSignalRouteId(source_node_id, old_record_id, new_record_id),
                .const_value, .task_source, .interval_source => {},
                .map => |payload| self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(payload.input), old_record_id, new_record_id),
                .map2 => |payload| {
                    self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(payload.left), old_record_id, new_record_id);
                    if (payload.right != payload.left) {
                        self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(payload.right), old_record_id, new_record_id);
                    }
                },
                .combine => |payload| {
                    for (payload.children, 0..) |child, index| {
                        if (recordSliceContains(payload.children[0..index], child)) continue;
                        self.replaceActiveSignalDependentId(self.requireActiveSignalRecordId(child), old_record_id, new_record_id);
                    }
                },
            }
        }

        fn removeActiveSignalGraphNode(self: *Self, ctx: Ctx.Handle, record_id: u64, record: *HostSignalRecord) void {
            const allocator = Ctx.allocator(ctx);
            const record_index: usize = @intCast(record_id);
            if (record_index >= self.active_signal_graph.items.len) @panic("active signal graph removal referenced an unknown record");
            if (self.active_signal_graph.items[record_index].record != record) @panic("active signal graph removal referenced the wrong record");
            if (self.active_signal_graph.items[record_index].dependents.len != 0) @panic("active signal graph removed a record with live dependents");

            allocator.free(self.active_signal_graph.items[record_index].dependents);
            const last_index = self.active_signal_graph.items.len - 1;
            self.removeActiveSinkRoutesForRecordId(ctx, record_index, last_index);
            _ = self.active_signal_graph.swapRemove(record_index);
            record.active_graph_id = null;
            record.release(allocator, ctx, self.roc_host.?, &self.pending_roc_metrics);

            if (record_index != last_index) {
                const moved_id: u64 = @intCast(record_index);
                const old_moved_id: u64 = @intCast(last_index);
                const moved_record = self.active_signal_graph.items[record_index].record;
                moved_record.active_graph_id = moved_id;
                self.updateMovedActiveSignalRecordEdges(moved_record, old_moved_id, moved_id);
            }
        }

        pub fn releaseActiveSignalRecord(self: *Self, ctx: Ctx.Handle, record: *HostSignalRecord) void {
            if (record.active_use_count == 0) @panic("active signal graph record use count underflow");
            record.active_use_count -= 1;
            if (record.active_use_count != 0) return;

            const allocator = Ctx.allocator(ctx);
            const record_id = self.requireActiveSignalRecordId(record);
            var input_records: std.ArrayListUnmanaged(*HostSignalRecord) = .empty;
            defer input_records.deinit(allocator);
            appendActiveSignalInputRecords(ctx, &input_records, record);

            switch (record.payload) {
                .ref => |source_node_id| self.removeActiveSourceSignalRoute(source_node_id, record_id),
                .const_value, .task_source, .interval_source => {},
                .map, .map2, .combine => {},
            }

            for (input_records.items) |input_record| {
                self.removeActiveSignalDependentId(ctx, self.requireActiveSignalRecordId(input_record), record_id);
            }

            self.removeActiveSignalGraphNode(ctx, record_id, record);

            for (input_records.items) |input_record| {
                self.releaseActiveSignalRecord(ctx, input_record);
            }
        }

        fn deinitActiveSignalTextNodeDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalTextNodeDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            releaseHostTextRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveSignalTextAttrDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalTextAttrDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            releaseHostTextRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveSignalBoolAttrDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeSignalBoolAttrDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            releaseHostBoolRead(desc.read, roc_host, &self.pending_roc_metrics);
        }

        fn deinitActiveOnChangeDesc(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc) void {
            desc.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
            desc.signal.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
            self.pending_roc_metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }

        fn deinitActiveMountDesc(self: *Self, roc_host: *abi.RocHost, desc: *HostNodeMountDesc) void {
            self.pending_roc_metrics.bump(.closure_releases, 1);
            abi.decrefErasedCallable(desc.to_cmd, roc_host);
        }

        pub fn scopeIsInReplacementTarget(self: *Self, scope_id: u64, target: HostStructuralReplacementTarget) bool {
            return switch (target) {
                .scope => |root_scope_id| self.scopeIsDescendantOrSelf(scope_id, root_scope_id) catch @panic("scope descriptor referenced an unknown parent scope"),
                .each_site => |site| self.scopeIsEachSiteRowDescendantOrSelf(scope_id, site) catch @panic("scope descriptor referenced an unknown parent scope"),
            };
        }

        pub fn renderNodeInReplacementTarget(self: *Self, stream: *const HostNodeDescriptorStream, node: HostRenderNode, target: HostStructuralReplacementTarget) bool {
            return self.scopeIsInReplacementTarget(renderNodeScopeId(stream, node), target);
        }

        pub fn elemIdInReplacementTarget(self: *Self, stream: *const HostNodeDescriptorStream, elem_id: u64, target: HostStructuralReplacementTarget) bool {
            const scope_id = elemScopeId(stream, elem_id) orelse @panic("descriptor referenced an element outside the render stream");
            return self.scopeIsInReplacementTarget(scope_id, target);
        }

        pub fn streamNodeIdInReplacementTarget(self: *Self, previous: *const HostNodeDescriptorStream, node_id: u64, kind: HostNodeScopeSiteKind, target: HostStructuralReplacementTarget) bool {
            const descriptor_index = previous.nodeDescriptorIndex(node_id) orelse return false;
            const site_index = descriptor_index.scope_sites.get(kind) orelse return false;
            if (site_index >= previous.scope_sites.items.len) @panic("scope site descriptor index exceeded descriptor table");
            const site = previous.scope_sites.items[site_index];
            if (site.node_id != node_id or site.kind != kind) @panic("scope site descriptor index pointed at the wrong node");
            return self.scopeIsInReplacementTarget(site.scope_id, target);
        }

        fn removeActiveElementDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target: HostStructuralReplacementTarget) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.elements.items.len);
            for (self.active_stream.elements.items, 0..) |desc, read_index| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    self.active_stream.clearElementIndex(desc.elem_id, read_index);
                    allocator.free(desc.tag);
                    continue;
                }
                self.active_stream.elements.items[write_index] = desc;
                self.active_stream.updateElementIndex(desc.elem_id, write_index);
                write_index += 1;
            }
            self.active_stream.elements.items.len = write_index;
        }

        fn removeActiveTextNodeDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target: HostStructuralReplacementTarget) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.text_nodes.items.len);
            for (self.active_stream.text_nodes.items, 0..) |desc, read_index| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    self.active_stream.clearTextNodeIndex(desc.elem_id, read_index);
                    allocator.free(desc.value);
                    continue;
                }
                self.active_stream.text_nodes.items[write_index] = desc;
                self.active_stream.updateTextNodeIndex(desc.elem_id, write_index);
                write_index += 1;
            }
            self.active_stream.text_nodes.items.len = write_index;
        }

        fn removeActiveSignalTextNodeDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.signal_text_nodes.items.len);
            for (self.active_stream.signal_text_nodes.items, 0..) |desc, read_index| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.signal.record);
                    self.removeActiveTextSignalRoute(record_id, .text_node, read_index);
                    self.active_stream.clearSignalTextNodeIndex(removed.elem_id, read_index);
                    self.releaseActiveSignalRecord(ctx, removed.signal.record);
                    self.deinitActiveSignalTextNodeDesc(ctx, roc_host, &removed);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.updateActiveTextSignalRouteIndex(record_id, .text_node, read_index, write_index);
                self.active_stream.signal_text_nodes.items[write_index] = desc;
                self.active_stream.updateSignalTextNodeIndex(desc.elem_id, write_index);
                write_index += 1;
            }
            self.active_stream.signal_text_nodes.items.len = write_index;
        }

        fn removeActiveStaticTextAttrDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target: HostStructuralReplacementTarget) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.static_text_attrs.items.len);
            for (self.active_stream.static_text_attrs.items, 0..) |desc, read_index| {
                if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                    self.active_stream.clearStaticTextAttrIndex(desc.elem_id, desc.field, read_index);
                    allocator.free(desc.value);
                    continue;
                }
                self.active_stream.static_text_attrs.items[write_index] = desc;
                self.active_stream.updateStaticTextAttrIndex(desc.elem_id, desc.field, write_index);
                write_index += 1;
            }
            self.active_stream.static_text_attrs.items.len = write_index;
        }

        fn removeActiveSignalTextAttrDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.signal_text_attrs.items.len);
            for (self.active_stream.signal_text_attrs.items, 0..) |desc, read_index| {
                if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.signal.record);
                    self.removeActiveTextSignalRoute(record_id, .text_attr, read_index);
                    self.active_stream.clearSignalTextAttrIndex(removed.elem_id, removed.field, read_index);
                    self.releaseActiveSignalRecord(ctx, removed.signal.record);
                    self.deinitActiveSignalTextAttrDesc(ctx, roc_host, &removed);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.updateActiveTextSignalRouteIndex(record_id, .text_attr, read_index, write_index);
                self.active_stream.signal_text_attrs.items[write_index] = desc;
                self.active_stream.updateSignalTextAttrIndex(desc.elem_id, desc.field, write_index);
                write_index += 1;
            }
            self.active_stream.signal_text_attrs.items.len = write_index;
        }

        fn removeActiveStaticBoolAttrDescriptorsInTarget(self: *Self, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.static_bool_attrs.items.len);
            for (self.active_stream.static_bool_attrs.items, 0..) |desc, read_index| {
                if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                    self.active_stream.clearStaticBoolAttrIndex(desc.elem_id, desc.field, read_index);
                    continue;
                }
                self.active_stream.static_bool_attrs.items[write_index] = desc;
                self.active_stream.updateStaticBoolAttrIndex(desc.elem_id, desc.field, write_index);
                write_index += 1;
            }
            self.active_stream.static_bool_attrs.items.len = write_index;
        }

        fn removeActiveSignalBoolAttrDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.signal_bool_attrs.items.len);
            for (self.active_stream.signal_bool_attrs.items, 0..) |desc, read_index| {
                if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.signal.record);
                    self.removeActiveBoolSignalRoute(record_id, read_index);
                    self.active_stream.clearSignalBoolAttrIndex(removed.elem_id, removed.field, read_index);
                    self.releaseActiveSignalRecord(ctx, removed.signal.record);
                    self.deinitActiveSignalBoolAttrDesc(ctx, roc_host, &removed);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.updateActiveBoolSignalRouteIndex(record_id, read_index, write_index);
                self.active_stream.signal_bool_attrs.items[write_index] = desc;
                self.active_stream.updateSignalBoolAttrIndex(desc.elem_id, desc.field, write_index);
                write_index += 1;
            }
            self.active_stream.signal_bool_attrs.items.len = write_index;
        }

        fn removeActiveOnChangeDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.on_changes.items.len);
            for (self.active_stream.on_changes.items, 0..) |desc, read_index| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.signal.record);
                    self.removeActiveChangeSignalRoute(record_id, read_index);
                    self.releaseActiveSignalRecord(ctx, removed.signal.record);
                    self.deinitActiveOnChangeDesc(ctx, roc_host, &removed);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.updateActiveChangeSignalRouteIndex(record_id, read_index, write_index);
                self.active_stream.on_changes.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.on_changes.items.len = write_index;
        }

        fn removeActiveMountDescriptorsInTarget(self: *Self, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.mounts.items.len);
            for (self.active_stream.mounts.items) |desc| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    var removed = desc;
                    self.deinitActiveMountDesc(roc_host, &removed);
                    continue;
                }
                self.active_stream.mounts.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.mounts.items.len = write_index;
        }

        fn removeActiveCleanupDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target: HostStructuralReplacementTarget) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.cleanups.items.len);
            for (self.active_stream.cleanups.items) |desc| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    allocator.free(desc.name);
                    continue;
                }
                self.active_stream.cleanups.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.cleanups.items.len = write_index;
        }

        fn removeActiveEventDescriptorsInTarget(self: *Self, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            if (self.active_events.items.len != self.active_stream.events.items.len) {
                if (self.active_stream.events.items.len != 0) @panic("active event descriptor table is out of sync with active events");
            }

            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.events.items.len);
            for (self.active_stream.events.items, 0..) |desc, event_index| {
                if (self.elemIdInReplacementTarget(&self.active_stream, desc.elem_id, target)) {
                    if (desc.owns_payload_reducer) {
                        @panic("active event descriptor retained ownership outside the active event table");
                    }
                    self.active_stream.clearEventIndex(desc.elem_id, desc.kind, event_index);
                    if (self.active_events.items.len != 0) {
                        self.deinitActiveEventDesc(roc_host, self.active_events.items[event_index]);
                    }
                    continue;
                }

                self.active_stream.events.items[write_index] = desc;
                self.active_stream.updateEventIndex(desc.elem_id, desc.kind, write_index);
                if (self.active_events.items.len != 0) {
                    self.active_events.items[write_index] = self.active_events.items[event_index];
                }
                write_index += 1;
            }
            self.active_stream.events.items.len = write_index;
            if (self.active_events.items.len != 0) self.active_events.items.len = write_index;
        }

        fn removeActiveScopeSiteDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, target: HostStructuralReplacementTarget) void {
            const allocator = Ctx.allocator(ctx);
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.scope_sites.items.len);
            for (self.active_stream.scope_sites.items, 0..) |desc, read_index| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, target)) {
                    self.active_stream.clearScopeSiteIndex(desc.node_id, desc.kind, read_index);
                    allocator.free(desc.binder_bindings);
                    continue;
                }
                self.active_stream.updateScopeSiteIndex(desc.node_id, desc.kind, write_index);
                self.active_stream.scope_sites.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.scope_sites.items.len = write_index;
        }

        fn removeActiveStateDescriptorsInTarget(self: *Self, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.states.items.len);
            for (self.active_stream.states.items, 0..) |desc, read_index| {
                if (self.streamNodeIdInReplacementTarget(&self.active_stream, desc.node_id, .state, target)) {
                    self.active_stream.clearStateIndex(desc.node_id, read_index);
                    self.pending_roc_metrics.bump(.closure_releases, 1);
                    abi.decrefErasedCallable(desc.initial, roc_host);
                    releaseHostValueCapability(desc.cap, roc_host, &self.pending_roc_metrics);
                    continue;
                }
                self.active_stream.updateStateIndex(desc.node_id, write_index);
                self.active_stream.states.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.states.items.len = write_index;
        }

        fn removeActiveWhenDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.whens.items.len);
            for (self.active_stream.whens.items, 0..) |desc, read_index| {
                if (self.streamNodeIdInReplacementTarget(&self.active_stream, desc.node_id, .when, target)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.condition.record);
                    self.removeActiveStructuralSignalRoute(record_id, .when, read_index);
                    self.active_stream.clearWhenIndex(removed.node_id, read_index);
                    self.releaseActiveSignalRecord(ctx, removed.condition.record);
                    removed.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
                    removed.condition.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
                    releaseHostBoolRead(removed.read, roc_host, &self.pending_roc_metrics);
                    abi.decrefElem(removed.when_false, roc_host);
                    abi.decrefElem(removed.when_true, roc_host);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.condition.record);
                self.updateActiveStructuralSignalRouteIndex(record_id, .when, read_index, write_index);
                self.active_stream.updateWhenIndex(desc.node_id, write_index);
                self.active_stream.whens.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.whens.items.len = write_index;
        }

        fn removeActiveEachDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            var write_index: usize = 0;
            self.recordStreamNodesScanned(self.active_stream.eaches.items.len);
            for (self.active_stream.eaches.items, 0..) |desc, read_index| {
                if (self.streamNodeIdInReplacementTarget(&self.active_stream, desc.node_id, .each, target)) {
                    var removed = desc;
                    const record_id = self.requireActiveSignalRecordId(removed.items.record);
                    self.removeActiveStructuralSignalRoute(record_id, .each, read_index);
                    self.active_stream.clearEachIndex(removed.node_id, read_index);
                    self.releaseActiveSignalRecord(ctx, removed.items.record);
                    removed.cached_value.deinit(ctx, roc_host, &self.pending_roc_metrics);
                    removed.items.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
                    releaseHostEachOps(removed.ops, roc_host, &self.pending_roc_metrics);
                    continue;
                }
                const record_id = self.requireActiveSignalRecordId(desc.items.record);
                self.updateActiveStructuralSignalRouteIndex(record_id, .each, read_index, write_index);
                self.active_stream.updateEachIndex(desc.node_id, write_index);
                self.active_stream.eaches.items[write_index] = desc;
                write_index += 1;
            }
            self.active_stream.eaches.items.len = write_index;
        }

        fn removeActiveNonRenderDescriptorsInTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget) void {
            self.removeActiveStaticTextAttrDescriptorsInTarget(ctx, target);
            self.removeActiveSignalTextAttrDescriptorsInTarget(ctx, roc_host, target);
            self.removeActiveStaticBoolAttrDescriptorsInTarget(target);
            self.removeActiveSignalBoolAttrDescriptorsInTarget(ctx, roc_host, target);
            self.removeActiveOnChangeDescriptorsInTarget(ctx, roc_host, target);
            self.removeActiveMountDescriptorsInTarget(roc_host, target);
            self.removeActiveCleanupDescriptorsInTarget(ctx, target);
            self.removeActiveEventDescriptorsInTarget(roc_host, target);
            self.removeActiveStateDescriptorsInTarget(roc_host, target);
            self.removeActiveWhenDescriptorsInTarget(ctx, roc_host, target);
            self.removeActiveEachDescriptorsInTarget(ctx, roc_host, target);
            self.removeActiveScopeSiteDescriptorsInTarget(ctx, target);
            self.removeActiveElementDescriptorsInTarget(ctx, target);
            self.removeActiveTextNodeDescriptorsInTarget(ctx, target);
            self.removeActiveSignalTextNodeDescriptorsInTarget(ctx, roc_host, target);
        }

        fn adjustActiveScopeSiteRenderInsertIndices(self: *Self, replace_index: usize, removed_render_count: usize, replacement_render_count: usize) void {
            for (self.active_stream.scope_sites.items) |*desc| {
                desc.render_insert_index = adjustedRenderInsertIndex(desc.render_insert_index, replace_index, removed_render_count, replacement_render_count);
            }
        }

        fn appendReplacementEventsMoved(self: *Self, ctx: Ctx.Handle, replacement: *HostNodeDescriptorStream) void {
            const allocator = Ctx.allocator(ctx);
            if (self.active_events.items.len != self.active_stream.events.items.len) {
                if (self.active_stream.events.items.len != 0) @panic("active event descriptor table is out of sync before replacement event splice");
            }

            const event_base = self.active_stream.events.items.len;
            for (replacement.events.items, 0..) |*desc, offset| {
                if (!desc.owns_payload_reducer) {
                    @panic("replacement event descriptor did not own its retained payload");
                }
                self.active_stream.recordEventIndex(allocator, desc.elem_id, desc.kind, event_base + offset);
                self.active_events.append(allocator, .{
                    .target_node_id = desc.target_node_id,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                    .payload_reducer = desc.payload_reducer,
                }) catch @panic("out of memory");
                desc.owns_payload_reducer = false;
            }

            self.active_stream.events.appendSlice(allocator, replacement.events.items) catch @panic("out of memory");
            replacement.events.items.len = 0;
        }

        fn appendReplacementNonRenderDescriptorsMoved(self: *Self, ctx: Ctx.Handle, replacement: *HostNodeDescriptorStream, render_insert_offset: usize) void {
            const allocator = Ctx.allocator(ctx);

            const element_base = self.active_stream.elements.items.len;
            for (replacement.elements.items, 0..) |desc, offset| {
                self.active_stream.recordElementIndex(allocator, desc.elem_id, element_base + offset);
            }
            self.active_stream.elements.appendSlice(allocator, replacement.elements.items) catch @panic("out of memory");
            replacement.elements.items.len = 0;

            const text_node_base = self.active_stream.text_nodes.items.len;
            for (replacement.text_nodes.items, 0..) |desc, offset| {
                self.active_stream.recordTextNodeIndex(allocator, desc.elem_id, text_node_base + offset);
            }
            self.active_stream.text_nodes.appendSlice(allocator, replacement.text_nodes.items) catch @panic("out of memory");
            replacement.text_nodes.items.len = 0;

            const signal_text_node_base = self.active_stream.signal_text_nodes.items.len;
            for (replacement.signal_text_nodes.items, 0..) |desc, offset| {
                self.active_stream.recordSignalTextNodeIndex(allocator, desc.elem_id, signal_text_node_base + offset);
            }
            for (replacement.signal_text_nodes.items, 0..) |desc, offset| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .text_node,
                    .index = signal_text_node_base + offset,
                });
            }
            self.active_stream.signal_text_nodes.appendSlice(allocator, replacement.signal_text_nodes.items) catch @panic("out of memory");
            replacement.signal_text_nodes.items.len = 0;

            const static_text_attr_base = self.active_stream.static_text_attrs.items.len;
            for (replacement.static_text_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordStaticTextAttrIndex(allocator, desc.elem_id, desc.field, static_text_attr_base + offset);
            }
            self.active_stream.static_text_attrs.appendSlice(allocator, replacement.static_text_attrs.items) catch @panic("out of memory");
            replacement.static_text_attrs.items.len = 0;

            const signal_text_attr_base = self.active_stream.signal_text_attrs.items.len;
            for (replacement.signal_text_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordSignalTextAttrIndex(allocator, desc.elem_id, desc.field, signal_text_attr_base + offset);
            }
            for (replacement.signal_text_attrs.items, 0..) |desc, offset| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveTextSignalRoute(ctx, record_id, .{
                    .kind = .text_attr,
                    .index = signal_text_attr_base + offset,
                });
            }
            self.active_stream.signal_text_attrs.appendSlice(allocator, replacement.signal_text_attrs.items) catch @panic("out of memory");
            replacement.signal_text_attrs.items.len = 0;

            const static_bool_attr_base = self.active_stream.static_bool_attrs.items.len;
            for (replacement.static_bool_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordStaticBoolAttrIndex(allocator, desc.elem_id, desc.field, static_bool_attr_base + offset);
            }
            self.active_stream.static_bool_attrs.appendSlice(allocator, replacement.static_bool_attrs.items) catch @panic("out of memory");
            replacement.static_bool_attrs.items.len = 0;

            const signal_bool_attr_base = self.active_stream.signal_bool_attrs.items.len;
            for (replacement.signal_bool_attrs.items, 0..) |desc, offset| {
                self.active_stream.recordSignalBoolAttrIndex(allocator, desc.elem_id, desc.field, signal_bool_attr_base + offset);
            }
            for (replacement.signal_bool_attrs.items, 0..) |desc, offset| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveBoolSignalRoute(ctx, record_id, .{
                    .index = signal_bool_attr_base + offset,
                });
            }
            self.active_stream.signal_bool_attrs.appendSlice(allocator, replacement.signal_bool_attrs.items) catch @panic("out of memory");
            replacement.signal_bool_attrs.items.len = 0;

            const on_change_base = self.active_stream.on_changes.items.len;
            for (replacement.on_changes.items, 0..) |desc, offset| {
                self.retainActiveSignalRecord(ctx, desc.signal.record);
                const record_id = self.requireActiveSignalRecordId(desc.signal.record);
                self.appendActiveChangeSignalRoute(ctx, record_id, .{
                    .index = on_change_base + offset,
                });
            }
            self.active_stream.on_changes.appendSlice(allocator, replacement.on_changes.items) catch @panic("out of memory");
            replacement.on_changes.items.len = 0;

            self.active_stream.mounts.appendSlice(allocator, replacement.mounts.items) catch @panic("out of memory");
            replacement.mounts.items.len = 0;

            self.active_stream.cleanups.appendSlice(allocator, replacement.cleanups.items) catch @panic("out of memory");
            replacement.cleanups.items.len = 0;

            self.appendReplacementEventsMoved(ctx, replacement);

            const scope_site_base = self.active_stream.scope_sites.items.len;
            for (replacement.scope_sites.items) |*desc| {
                desc.render_insert_index += render_insert_offset;
            }
            for (replacement.scope_sites.items, 0..) |desc, offset| {
                self.active_stream.recordScopeSiteIndex(allocator, desc.node_id, desc.kind, scope_site_base + offset);
            }
            self.active_stream.scope_sites.appendSlice(allocator, replacement.scope_sites.items) catch @panic("out of memory");
            replacement.scope_sites.items.len = 0;

            const state_base = self.active_stream.states.items.len;
            for (replacement.states.items, 0..) |desc, offset| {
                self.active_stream.recordStateIndex(allocator, desc.node_id, state_base + offset);
            }
            self.active_stream.states.appendSlice(allocator, replacement.states.items) catch @panic("out of memory");
            replacement.states.items.len = 0;

            const when_base = self.active_stream.whens.items.len;
            for (replacement.whens.items, 0..) |desc, offset| {
                self.active_stream.recordWhenIndex(allocator, desc.node_id, when_base + offset);
                self.retainActiveSignalRecord(ctx, desc.condition.record);
                const record_id = self.requireActiveSignalRecordId(desc.condition.record);
                self.appendActiveStructuralSignalRoute(ctx, record_id, .{
                    .kind = .when,
                    .index = when_base + offset,
                });
            }
            self.active_stream.whens.appendSlice(allocator, replacement.whens.items) catch @panic("out of memory");
            replacement.whens.items.len = 0;

            const each_base = self.active_stream.eaches.items.len;
            for (replacement.eaches.items, 0..) |desc, offset| {
                self.active_stream.recordEachIndex(allocator, desc.node_id, each_base + offset);
                self.retainActiveSignalRecord(ctx, desc.items.record);
                const record_id = self.requireActiveSignalRecordId(desc.items.record);
                self.appendActiveStructuralSignalRoute(ctx, record_id, .{
                    .kind = .each,
                    .index = each_base + offset,
                });
            }
            self.active_stream.eaches.appendSlice(allocator, replacement.eaches.items) catch @panic("out of memory");
            replacement.eaches.items.len = 0;
        }

        fn rebuildActiveStreamSignalRecordTable(self: *Self, ctx: Ctx.Handle) void {
            const allocator = Ctx.allocator(ctx);
            self.active_stream.signal_records_by_token.items.len = 0;

            for (self.active_stream.signal_text_nodes.items) |desc| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
            }
            for (self.active_stream.signal_text_attrs.items) |desc| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
            }
            for (self.active_stream.signal_bool_attrs.items) |desc| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
            }
            for (self.active_stream.on_changes.items) |desc| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.signal.record);
            }
            for (self.active_stream.whens.items) |desc| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.condition.record);
            }
            for (self.active_stream.eaches.items) |desc| {
                self.active_stream.rememberSignalRecordTree(allocator, desc.items.record);
            }
        }

        fn validateActiveRenderDescriptorIntegrity(self: *Self) void {
            for (self.active_stream.render_nodes.items) |node| {
                const found = switch (node.kind) {
                    .element => findElementDesc(&self.active_stream, node.elem_id) != null,
                    .text => findTextNodeDesc(&self.active_stream, node.elem_id) != null,
                    .signal_text => findSignalTextNodeDesc(&self.active_stream, node.elem_id) != null,
                };
                if (!found) {
                    var message: [128]u8 = undefined;
                    const rendered = std.fmt.bufPrint(
                        &message,
                        "active render node {d} with kind {s} has no matching descriptor",
                        .{ node.elem_id, @tagName(node.kind) },
                    ) catch "active render node has no matching descriptor";
                    @panic(rendered);
                }
            }
        }

        pub fn spliceActiveStreamReplacingTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, target: HostStructuralReplacementTarget, render_insert_index: usize, replacement: *HostNodeDescriptorStream) HostStructuralSplice {
            const allocator = Ctx.allocator(ctx);
            if (render_insert_index > self.active_stream.render_nodes.items.len) @panic("structural replacement render insertion point is outside the active stream");

            var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer removed_elem_ids.deinit(allocator);
            var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer touched_parent_ids.deinit(allocator);

            var removed_render_start: ?usize = null;
            var removed_render_count: usize = 0;
            var target_range_closed = false;

            self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items, 0..) |node, index| {
                if (self.renderNodeInReplacementTarget(&self.active_stream, node, target)) {
                    if (target_range_closed) @panic("structural replacement render target is not contiguous");
                    if (removed_render_start == null) removed_render_start = index;
                    removed_render_count += 1;
                    removed_elem_ids.append(allocator, node.elem_id) catch @panic("out of memory");
                } else if (removed_render_start != null) {
                    target_range_closed = true;
                }
            }

            self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items) |node| {
                if (!self.renderNodeInReplacementTarget(&self.active_stream, node, target)) continue;
                const parent_elem_id = renderNodeParentElemId(&self.active_stream, node);
                if (u64SliceContains(removed_elem_ids.items, parent_elem_id)) continue;
                appendUniqueU64(allocator, &touched_parent_ids, parent_elem_id);
            }

            const render_start = removed_render_start orelse render_insert_index;
            if (removed_render_count != 0 and render_start != render_insert_index) {
                @panic("structural replacement render target did not start at its explicit insertion point");
            }

            const replacement_render_count = replacement.render_nodes.items.len;
            const on_change_count = replacement.on_changes.items.len;
            const mount_count = replacement.mounts.items.len;
            const replacement_elem_ids = allocator.alloc(u64, replacement_render_count) catch @panic("out of memory");
            errdefer allocator.free(replacement_elem_ids);
            for (replacement.render_nodes.items, 0..) |node, index| {
                replacement_elem_ids[index] = node.elem_id;
            }

            self.removeActiveNonRenderDescriptorsInTarget(ctx, roc_host, target);
            self.adjustActiveScopeSiteRenderInsertIndices(render_insert_index, removed_render_count, replacement_render_count);
            const on_change_start = self.active_stream.on_changes.items.len;
            const replacement_on_change_indices = allocator.alloc(usize, on_change_count) catch @panic("out of memory");
            errdefer allocator.free(replacement_on_change_indices);
            for (replacement_on_change_indices, 0..) |*index, offset| {
                index.* = on_change_start + offset;
            }
            const mount_start = self.active_stream.mounts.items.len;
            const replacement_mount_indices = allocator.alloc(usize, mount_count) catch @panic("out of memory");
            errdefer allocator.free(replacement_mount_indices);
            for (replacement_mount_indices, 0..) |*index, offset| {
                index.* = mount_start + offset;
            }
            self.appendReplacementNonRenderDescriptorsMoved(ctx, replacement, render_insert_index);

            self.active_stream.render_nodes.replaceRange(allocator, render_start, removed_render_count, replacement.render_nodes.items) catch @panic("out of memory");
            replacement.render_nodes.items.len = 0;
            self.validateActiveRenderDescriptorIntegrity();
            self.rebuildActiveStreamSignalRecordTable(ctx);
            self.syncActiveIntervalsFromGraph(ctx);

            return .{
                .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_elem_ids = replacement_elem_ids,
                .replacement_on_change_indices = replacement_on_change_indices,
                .replacement_mount_indices = replacement_mount_indices,
            };
        }

        pub fn spliceActiveStreamReplacingScope(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, replaced_scope_id: u64, render_insert_index: usize, replacement: *HostNodeDescriptorStream) HostStructuralSplice {
            return self.spliceActiveStreamReplacingTarget(ctx, roc_host, .{ .scope = replaced_scope_id }, render_insert_index, replacement);
        }

        pub fn replaceSignalExprCacheAndClone(self: *Self, ctx: Ctx.Handle, cache_slot: *HostSignalCacheSlot, roc_host: *abi.RocHost, value: HostValue, cap: HostValueCapability) HostValue {
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, cap);
            return self.cloneCachedSignalValue(ctx, cache_slot);
        }

        pub fn evalHostSignalRecord(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord) HostValue {
            switch (record.payload) {
                .ref => |node_id| return Ctx.stateValueByNodeId(ctx, node_id),
                .const_value => |*payload| {
                    const value = erased_calls.callValueInitThunk(roc_host, payload.init);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .map => |*payload| {
                    const input = self.evalHostSignalRecord(ctx, roc_host, payload.input);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.input, input);
                    self.recordDerivedCall();
                    const input_cap = self.hostSignalRecordCapability(ctx, payload.input);
                    const value = callHostValueToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, input);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .map2 => |*payload| {
                    const left = self.evalHostSignalRecord(ctx, roc_host, payload.left);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.left, left);
                    const right = self.evalHostSignalRecord(ctx, roc_host, payload.right);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.right, right);
                    self.recordDerivedCall();
                    const left_cap = self.hostSignalRecordCapability(ctx, payload.left);
                    const right_cap = self.hostSignalRecordCapability(ctx, payload.right);
                    const value = callHostValueHostValueToHostValueWithCapabilities(ctx, roc_host, left_cap, right_cap, payload.transform, left, right);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .combine => |*payload| {
                    const allocator = Ctx.allocator(ctx);
                    var values: std.ArrayListUnmanaged(HostValue) = .empty;
                    errdefer {
                        for (payload.children, values.items) |child, value| {
                            self.dropHostSignalRecordValue(ctx, roc_host, child, value);
                        }
                        values.deinit(allocator);
                    }
                    for (payload.children) |child| {
                        values.append(allocator, self.evalHostSignalRecord(ctx, roc_host, child)) catch @panic("out of memory");
                    }
                    const list = HostValueList.fromSlice(values.items, roc_host);
                    defer list.decref(roc_host);
                    self.recordDerivedCall();
                    const input_cap = if (payload.children.len == 0) payload.cap else self.hostSignalRecordCapability(ctx, payload.children[0]);
                    const value = callHostValueListToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, list);
                    for (payload.children, values.items) |child, child_value| {
                        self.dropHostSignalRecordValue(ctx, roc_host, child, child_value);
                    }
                    values.deinit(allocator);
                    return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                },
                .task_source => |*payload| {
                    switch (payload.cached_value) {
                        .present => return self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .absent => {
                            const value = erased_calls.callValueInitThunk(roc_host, payload.initial);
                            return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                        },
                    }
                },
                .interval_source => |*payload| {
                    switch (payload.cached_value) {
                        .present => return self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .absent => {
                            const value = erased_calls.callValueInitThunk(roc_host, payload.initial);
                            return self.replaceSignalExprCacheAndClone(ctx, &payload.cached_value, roc_host, value, payload.cap);
                        },
                    }
                },
            }
        }

        pub fn evalHostSignalBinding(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, signal: *HostSignalBinding) HostValue {
            return self.evalHostSignalRecord(ctx, roc_host, signal.record);
        }

        pub fn evalSignalTextField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot) bool {
            const value = self.evalHostSignalBinding(ctx, roc_host, signal);
            const signal_cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, signal_cap, "text read extension capability did not match its signal value");
            const text = callHostValueToStrWithCapability(ctx, roc_host, read.capability, read.read, value);
            defer text.decref(roc_host);
            const changed = self.applyRenderTextField(ctx, elem_id, field, text.asSlice());
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, signal_cap);
            return changed;
        }

        pub fn evalSignalBoolField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: HostBoolRead, cache_slot: *HostSignalCacheSlot) bool {
            const value = self.evalHostSignalBinding(ctx, roc_host, signal);
            const signal_cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, signal_cap, "bool read extension capability did not match its signal value");
            const bool_value = callHostValueToBoolWithCapability(ctx, roc_host, read.capability, read.read, value);
            const changed = self.applyRenderBoolField(ctx, elem_id, field, bool_value);
            cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, signal_cap);
            return changed;
        }

        pub fn evalDirtySignalTextField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, cap, "dirty text read extension capability did not match its signal value");
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return false;
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, cache_slot, result.value, cap)) return false;
            const text = callHostValueToStrWithCapability(ctx, roc_host, read.capability, read.read, result.value);
            defer text.decref(roc_host);
            return self.applyRenderTextField(ctx, elem_id, field, text.asSlice());
        }

        pub fn evalDirtySignalBoolField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: HostBoolRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, signal);
            assertHostValueCapabilitiesMatch(read.capability, cap, "dirty bool read extension capability did not match its signal value");
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return false;
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, cache_slot, result.value, cap)) return false;
            return self.applyRenderBoolField(ctx, elem_id, field, callHostValueToBoolWithCapability(ctx, roc_host, read.capability, read.read, result.value));
        }

        pub fn evalStructuralSignalTextField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: *HostSignalBinding, read: HostTextRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            if (dirty_generation != 0 and sourceNodeIdsIntersect(signal.source_node_ids, dirty_source_node_ids)) {
                return self.evalDirtySignalTextField(ctx, roc_host, elem_id, field, signal, read, cache_slot, dirty_source_node_ids, dirty_generation);
            }
            return self.evalSignalTextField(ctx, roc_host, elem_id, field, signal, read, cache_slot);
        }

        pub fn evalStructuralSignalBoolField(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: *HostSignalBinding, read: HostBoolRead, cache_slot: *HostSignalCacheSlot, dirty_source_node_ids: []const u64, dirty_generation: u64) bool {
            if (dirty_generation != 0 and sourceNodeIdsIntersect(signal.source_node_ids, dirty_source_node_ids)) {
                return self.evalDirtySignalBoolField(ctx, roc_host, elem_id, field, signal, read, cache_slot, dirty_source_node_ids, dirty_generation);
            }
            return self.evalSignalBoolField(ctx, roc_host, elem_id, field, signal, read, cache_slot);
        }

        pub fn evalDirtyHostSignalRecord(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
            if (dirty_generation == 0) @panic("dirty signal evaluation used generation 0");
            debugPhase(ctx, 400);
            if (self.cloneMemoizedDirtySignalResult(ctx, record, dirty_generation)) |result| return result;

            switch (record.payload) {
                .ref => |node_id| {
                    debugPhase(ctx, 401);
                    return .{
                        .value = Ctx.stateValueByNodeId(ctx, node_id),
                        .changed = u64SliceContains(dirty_source_node_ids, node_id),
                    };
                },
                .const_value => |*payload| {
                    if (payload.cached_value == .absent) {
                        debugPhase(ctx, 402);
                        const value = erased_calls.callValueInitThunk(roc_host, payload.init);
                        return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                    }
                    debugPhase(ctx, 403);
                    return self.rememberDirtySignalResult(record, dirty_generation, .{
                        .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .changed = false,
                    });
                },
                .map => |*payload| {
                    const cache_was_absent = payload.cached_value == .absent;
                    debugPhase(ctx, 420);
                    const input = self.evalDirtyHostSignalRecord(ctx, roc_host, payload.input, dirty_source_node_ids, dirty_generation);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.input, input.value);
                    if (!input.changed and !cache_was_absent) {
                        debugPhase(ctx, 423);
                        return self.rememberDirtySignalResult(record, dirty_generation, .{ .value = self.cloneCachedSignalValue(ctx, &payload.cached_value), .changed = false });
                    }

                    self.recordDerivedCall();
                    debugPhase(ctx, 421);
                    const input_cap = self.hostSignalRecordCapability(ctx, payload.input);
                    const value = callHostValueToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, input.value);
                    debugPhase(ctx, 422);
                    return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                },
                .map2 => |*payload| {
                    const cache_was_absent = payload.cached_value == .absent;
                    debugPhase(ctx, 430);
                    const left = self.evalDirtyHostSignalRecord(ctx, roc_host, payload.left, dirty_source_node_ids, dirty_generation);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.left, left.value);
                    debugPhase(ctx, 431);
                    const right = self.evalDirtyHostSignalRecord(ctx, roc_host, payload.right, dirty_source_node_ids, dirty_generation);
                    defer self.dropHostSignalRecordValue(ctx, roc_host, payload.right, right.value);
                    if (!left.changed and !right.changed and !cache_was_absent) {
                        debugPhase(ctx, 434);
                        return self.rememberDirtySignalResult(record, dirty_generation, .{ .value = self.cloneCachedSignalValue(ctx, &payload.cached_value), .changed = false });
                    }

                    self.recordDerivedCall();
                    debugPhase(ctx, 432);
                    const left_cap = self.hostSignalRecordCapability(ctx, payload.left);
                    const right_cap = self.hostSignalRecordCapability(ctx, payload.right);
                    const value = callHostValueHostValueToHostValueWithCapabilities(ctx, roc_host, left_cap, right_cap, payload.transform, left.value, right.value);
                    debugPhase(ctx, 433);
                    return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                },
                .combine => |*payload| {
                    const cache_was_absent = payload.cached_value == .absent;
                    const allocator = Ctx.allocator(ctx);
                    var values: std.ArrayListUnmanaged(HostValue) = .empty;
                    errdefer {
                        for (payload.children[0..values.items.len], values.items) |child, value| {
                            self.dropHostSignalRecordValue(ctx, roc_host, child, value);
                        }
                        values.deinit(allocator);
                    }

                    var any_changed = false;
                    for (payload.children) |child| {
                        debugPhase(ctx, 440);
                        const child_result = self.evalDirtyHostSignalRecord(ctx, roc_host, child, dirty_source_node_ids, dirty_generation);
                        any_changed = any_changed or child_result.changed;
                        values.append(allocator, child_result.value) catch @panic("out of memory");
                    }

                    if (!any_changed and !cache_was_absent) {
                        debugPhase(ctx, 443);
                        for (payload.children, values.items) |child, value| {
                            self.dropHostSignalRecordValue(ctx, roc_host, child, value);
                        }
                        values.deinit(allocator);
                        return self.rememberDirtySignalResult(record, dirty_generation, .{ .value = self.cloneCachedSignalValue(ctx, &payload.cached_value), .changed = false });
                    }

                    const list = HostValueList.fromSlice(values.items, roc_host);
                    defer list.decref(roc_host);
                    self.recordDerivedCall();
                    debugPhase(ctx, 441);
                    const input_cap = if (payload.children.len == 0) payload.cap else self.hostSignalRecordCapability(ctx, payload.children[0]);
                    const value = callHostValueListToHostValueWithCapability(ctx, roc_host, input_cap, payload.transform, list);
                    debugPhase(ctx, 442);
                    for (payload.children, values.items) |child, child_value| {
                        self.dropHostSignalRecordValue(ctx, roc_host, child, child_value);
                    }
                    values.deinit(allocator);
                    return self.rememberDirtySignalResult(record, dirty_generation, self.updateDirtySignalExprCache(ctx, roc_host, &payload.cached_value, value, payload.cap));
                },
                .task_source => |*payload| {
                    debugPhase(ctx, 410);
                    return self.rememberDirtySignalResult(record, dirty_generation, .{
                        .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .changed = record.last_dirty_generation == dirty_generation and record.last_dirty_changed,
                    });
                },
                .interval_source => |*payload| {
                    debugPhase(ctx, 411);
                    return self.rememberDirtySignalResult(record, dirty_generation, .{
                        .value = self.cloneCachedSignalValue(ctx, &payload.cached_value),
                        .changed = record.last_dirty_generation == dirty_generation and record.last_dirty_changed,
                    });
                },
            }
        }

        pub fn evalDirtyHostSignalBinding(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, signal: *HostSignalBinding, dirty_source_node_ids: []const u64, dirty_generation: u64) HostSignalEvalResult {
            return self.evalDirtyHostSignalRecord(ctx, roc_host, signal.record, dirty_source_node_ids, dirty_generation);
        }

        pub fn propagateDirtyActiveSignals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
            const dirty_record_ids = self.dirtyActiveSignalRecordIdsForSources(allocator, dirty_source_node_ids);
            defer allocator.free(dirty_record_ids);
            return self.propagateDirtyActiveSignalRecordIds(ctx, roc_host, allocator, dirty_record_ids, dirty_source_node_ids, dirty_generation);
        }

        pub fn propagateDirtyActiveSignalRecordIds(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_record_ids: []const u64, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
            var changed_record_ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer changed_record_ids.deinit(allocator);

            for (dirty_record_ids) |record_id| {
                const record = self.active_signal_graph.items[@intCast(record_id)].record;
                debugPhase(ctx, 331);
                const result = self.evalDirtyHostSignalRecord(ctx, roc_host, record, dirty_source_node_ids, dirty_generation);
                if (result.changed) {
                    changed_record_ids.append(allocator, record_id) catch @panic("out of memory");
                }
                debugPhase(ctx, 332);
                self.dropHostSignalRecordValue(ctx, roc_host, record, result.value);
            }

            return changed_record_ids.toOwnedSlice(allocator) catch @panic("out of memory");
        }

        pub fn collectDirtyStructuralSignals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) []HostDirtyStructuralSignal {
            var dirty_structural_signals: std.ArrayListUnmanaged(HostDirtyStructuralSignal) = .empty;
            errdefer dirty_structural_signals.deinit(allocator);

            for (changed_record_ids) |record_id| {
                const route_index: usize = @intCast(record_id);
                if (route_index >= self.active_structural_signal_routes.items.len) continue;

                for (self.active_structural_signal_routes.items[route_index].items) |route| {
                    switch (route.kind) {
                        .when => {
                            const desc = &self.active_stream.whens.items[route.index];
                            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, &desc.condition, dirty_source_node_ids, dirty_generation);
                            const cap = self.hostSignalBindingCapability(ctx, &desc.condition);
                            assertHostValueCapabilitiesMatch(desc.read.capability, cap, "dirty when read extension capability did not match its signal value");
                            if (!result.changed) {
                                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                                continue;
                            }
                            const active_branch: HostScopeBranch = if (callHostValueToBoolWithCapability(ctx, roc_host, desc.read.capability, desc.read.read, result.value)) .true_branch else .false_branch;
                            if (self.updateDirtySignalCache(ctx, roc_host, &desc.cached_value, result.value, cap)) {
                                dirty_structural_signals.append(allocator, .{
                                    .kind = .when,
                                    .node_id = desc.node_id,
                                    .branch = active_branch,
                                }) catch @panic("out of memory");
                            }
                        },
                        .each => {
                            const desc = &self.active_stream.eaches.items[route.index];
                            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, &desc.items, dirty_source_node_ids, dirty_generation);
                            const cap = self.hostSignalBindingCapability(ctx, &desc.items);
                            if (!result.changed) {
                                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                                continue;
                            }
                            if (self.updateDirtySignalCache(ctx, roc_host, &desc.cached_value, result.value, cap)) {
                                dirty_structural_signals.append(allocator, .{
                                    .kind = .each,
                                    .node_id = desc.node_id,
                                }) catch @panic("out of memory");
                            }
                        },
                    }
                }
            }

            return dirty_structural_signals.toOwnedSlice(allocator) catch @panic("out of memory");
        }

        pub fn stateIndexByNodeId(self: *Self, node_id: u64) ?usize {
            if (node_id >= self.state_indexes_by_node_id.items.len) return null;
            const state_index = self.state_indexes_by_node_id.items[@intCast(node_id)] orelse return null;
            if (state_index >= self.states.items.len) @panic("state cell index exceeded state table");
            const state = self.states.items[state_index];
            if (!state.active or state.state_id != node_id) @panic("state cell index pointed at the wrong state");
            return state_index;
        }

        pub fn stateCapability(self: *Self, node_id: u64) StateLookupError!HostValueCapability {
            const state_index = self.stateIndexByNodeId(node_id) orelse return StateLookupError.MissingActiveState;
            return self.states.items[state_index].cell.cap;
        }

        pub fn activeEventReducerByIndex(self: *Self, event_index: usize) ActiveEventLookupError!HostEventReducer {
            if (event_index >= self.active_events.items.len) return ActiveEventLookupError.MissingActiveEvent;
            return self.active_events.items[event_index].payload_reducer;
        }

        pub fn activeScopeSiteByNodeId(self: *Self, node_id: u64, kind: HostNodeScopeSiteKind) ?HostNodeScopeSiteDesc {
            const descriptor_index = self.active_stream.nodeDescriptorIndex(node_id) orelse return null;
            const scope_site_index = descriptor_index.scope_sites.get(kind) orelse return null;
            if (scope_site_index >= self.active_stream.scope_sites.items.len) @panic("active scope site index exceeded descriptor table");
            const site = self.active_stream.scope_sites.items[scope_site_index];
            if (site.node_id != node_id or site.kind != kind) @panic("active scope site index pointed at the wrong node");
            return site;
        }

        pub fn activeWhenIndexByNodeId(self: *Self, node_id: u64) ?usize {
            const descriptor_index = self.active_stream.nodeDescriptorIndex(node_id) orelse return null;
            const when_index = descriptor_index.when orelse return null;
            if (when_index >= self.active_stream.whens.items.len) @panic("active when index exceeded descriptor table");
            if (self.active_stream.whens.items[when_index].node_id != node_id) @panic("active when index pointed at the wrong node");
            return when_index;
        }

        pub fn activeEachIndexByNodeId(self: *Self, node_id: u64) ?usize {
            const descriptor_index = self.active_stream.nodeDescriptorIndex(node_id) orelse return null;
            const each_index = descriptor_index.each orelse return null;
            if (each_index >= self.active_stream.eaches.items.len) @panic("active each index exceeded descriptor table");
            if (self.active_stream.eaches.items[each_index].node_id != node_id) @panic("active each index pointed at the wrong node");
            return each_index;
        }

        pub fn recordSliceContains(records: []const *HostSignalRecord, record: *HostSignalRecord) bool {
            for (records) |existing| {
                if (existing == record) return true;
            }
            return false;
        }

        pub fn activeWhenBranchScopeId(self: *Self, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) scope_tree.Error!?u64 {
            return scope_tree.activeWhenBranch(HostEachRowScopeStep, self.scopes.items, parent_scope_id, site_ordinal, branch);
        }

        pub fn validateScopeId(self: *Self, scope_id: u64) scope_tree.Error!void {
            return scope_tree.validate(HostEachRowScopeStep, self.scopes.items, scope_id);
        }

        pub fn internRootScope(self: *Self, allocator: std.mem.Allocator) scope_tree.Error!scope_tree.InternResult {
            const result = try scope_tree.internRoot(HostEachRowScopeStep, allocator, &self.scopes);
            if (result.created) self.recordScopeCreated();
            return result;
        }

        pub fn internComponentScope(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) scope_tree.Error!scope_tree.InternResult {
            const result = try scope_tree.internComponent(HostEachRowScopeStep, allocator, &self.scopes, parent_scope_id, site_ordinal);
            if (result.created) self.recordScopeCreated();
            return result;
        }

        pub fn internWhenBranchScope(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) scope_tree.Error!scope_tree.InternResult {
            const result = try scope_tree.internWhenBranch(HostEachRowScopeStep, allocator, &self.scopes, parent_scope_id, site_ordinal, branch);
            if (result.created) self.recordScopeCreated();
            return result;
        }

        pub fn internNodeIdentity(self: *Self, allocator: std.mem.Allocator, scope_id: u64, ordinal: u64) IdentityInternError!u64 {
            try self.validateScopeId(scope_id);
            return identity_table.internNode(allocator, &self.node_identities, scope_id, ordinal);
        }

        pub fn internDomIdentity(self: *Self, allocator: std.mem.Allocator, scope_id: u64, ordinal: u64) IdentityInternError!u64 {
            try self.validateScopeId(scope_id);
            return identity_table.internDom(allocator, &self.dom_identities, scope_id, ordinal);
        }

        pub fn activeEachRowScopes(self: *Self, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) scope_tree.Error![]u64 {
            return scope_tree.activeEachRows(HostEachRowScopeStep, allocator, self.scopes.items, parent_scope_id, site_ordinal);
        }

        pub fn eachRowScopeKeyEquals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, scope_id: u64, key: HostValue) bool {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            self.recordEachKeyCompare();
            return switch (scope.step) {
                .each_row => |*row| row.key.valueEquals(ctx, roc_host, key),
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn eachRowScopeKeyValue(self: *Self, scope_id: u64) HostValue {
            self.validateScopeId(scope_id) catch @panic("scope id has no host scope descriptor");
            const scope = &self.scopes.items[@intCast(scope_id)];
            return switch (scope.step) {
                .each_row => |row| row.key.value,
                .root, .component, .when_branch => @panic("scope id does not reference an each-row scope"),
            };
        }

        pub fn hashEachKeyValue(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, key_hash: abi.RocErasedCallable, key_cap: HostValueCapability, key: HostValue) u64 {
            self.recordEachKeyCompare();
            return callHostValueToU64WithCapability(ctx, roc_host, key_cap, key_hash, key);
        }

        pub fn eachKeysEqual(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, ops: HostEachOps, left: HostValue, right: HostValue) bool {
            self.recordEachKeyCompare();
            const key_cap = ops.key_capability;
            return callHostValueHostValueToBoolWithCapability(ctx, roc_host, key_cap, hv.hostValueCapabilityEq(key_cap), left, right);
        }

        pub fn buildEachRowMatchPlan(self: *Self, allocator: std.mem.Allocator, ctx: Ctx.Handle, roc_host: *abi.RocHost, existing_scope_ids: []const u64, keys: []const HostValue, ops: HostEachOps) keyed_rows.Error!keyed_rows.Plan {
            const key_hashes = allocator.alloc(u64, keys.len) catch return error.OutOfMemory;
            defer allocator.free(key_hashes);
            for (keys, 0..) |key, key_index| {
                key_hashes[key_index] = self.hashEachKeyValue(ctx, roc_host, ops.key_hash, ops.key_capability, key);
            }

            const existing_key_hashes = allocator.alloc(u64, existing_scope_ids.len) catch return error.OutOfMemory;
            defer allocator.free(existing_key_hashes);
            for (existing_scope_ids, 0..) |scope_id, existing_index| {
                const existing_key = self.eachRowScopeKeyValue(scope_id);
                existing_key_hashes[existing_index] = self.hashEachKeyValue(ctx, roc_host, ops.key_hash, ops.key_capability, existing_key);
            }

            const MatchContext = struct {
                engine: *Self,
                ctx: Ctx.Handle,
                roc_host: *abi.RocHost,
                existing_scope_ids: []const u64,
                keys: []const HostValue,
                ops: HostEachOps,

                pub fn nextKeysEqual(context: *@This(), left_index: usize, right_index: usize) bool {
                    return context.engine.eachKeysEqual(context.ctx, context.roc_host, context.ops, context.keys[left_index], context.keys[right_index]);
                }

                pub fn existingKeyEquals(context: *@This(), existing_index: usize, key_index: usize) bool {
                    const scope_id = context.existing_scope_ids[existing_index];
                    return context.engine.eachRowScopeKeyEquals(context.ctx, context.roc_host, scope_id, context.keys[key_index]);
                }
            };

            var match_context = MatchContext{
                .engine = self,
                .ctx = ctx,
                .roc_host = roc_host,
                .existing_scope_ids = existing_scope_ids,
                .keys = keys,
                .ops = ops,
            };
            return keyed_rows.buildPlan(allocator, existing_scope_ids, existing_key_hashes, key_hashes, &match_context);
        }

        pub fn eachSiteRowAncestorScopeId(self: *Self, scope_id: u64, site: HostEachSite) scope_tree.Error!?u64 {
            return scope_tree.eachSiteRowAncestor(HostEachRowScopeStep, self.scopes.items, scope_id, site.parent_scope_id, site.site_ordinal);
        }

        pub fn scopeIsDescendantOrSelf(self: *Self, scope_id: u64, root_scope_id: u64) scope_tree.Error!bool {
            return scope_tree.descendantOrSelf(HostEachRowScopeStep, self.scopes.items, scope_id, root_scope_id);
        }

        pub fn scopeIsEachSiteRowDescendantOrSelf(self: *Self, scope_id: u64, site: HostEachSite) scope_tree.Error!bool {
            return scope_tree.eachSiteRowDescendantOrSelf(HostEachRowScopeStep, self.scopes.items, scope_id, site.parent_scope_id, site.site_ordinal);
        }

        pub fn eachDiffPreservesSurvivorRenderOrder(old_render_rows: []const u64, next_scope_ids: []const u64) bool {
            var old_index: usize = 0;
            for (next_scope_ids) |next_scope_id| {
                if (!u64SliceContains(old_render_rows, next_scope_id)) continue;
                while (old_index < old_render_rows.len and !u64SliceContains(next_scope_ids, old_render_rows[old_index])) {
                    old_index += 1;
                }
                if (old_index >= old_render_rows.len) return false;
                if (old_render_rows[old_index] != next_scope_id) return false;
                old_index += 1;
            }
            return true;
        }

        pub fn activeEachRowScopesInRenderOrder(self: *Self, allocator: std.mem.Allocator, site: HostEachSite) []u64 {
            var ids: std.ArrayListUnmanaged(u64) = .empty;
            errdefer ids.deinit(allocator);

            self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items) |node| {
                const scope_id = renderNodeScopeId(&self.active_stream, node);
                const row_scope_id = (self.eachSiteRowAncestorScopeId(scope_id, site) catch @panic("scope descriptor referenced an unknown parent scope")) orelse continue;
                appendUniqueU64(allocator, &ids, row_scope_id);
            }

            return ids.toOwnedSlice(allocator) catch @panic("out of memory");
        }

        pub fn eachDiffIsPurePermutation(self: *Self, old_render_rows: []const u64, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64) bool {
            if (diff.rows_created != 0 or diff.rows_removed != 0) return false;
            if (diff.scope_ids.len != old_render_rows.len) return false;
            for (diff.scope_ids, diff.row_items_changed) |scope_id, row_item_changed| {
                if (row_item_changed) return false;
                if (!u64SliceContains(old_render_rows, scope_id)) return false;
                if (self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, scope_id, dirty_source_node_ids)) return false;
            }
            return true;
        }

        pub fn applyDirtyEachPermutationMoves(self: *Self, ctx: Ctx.Handle, site: HostNodeScopeSiteDesc, next_scope_ids: []const u64) render.Counts {
            if (site.kind != .each) @panic("dirty each permutation move received a non-each site");

            const allocator = Ctx.allocator(ctx);
            const each_site = HostEachSite{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal };
            var segments: std.ArrayListUnmanaged(HostEachRowRenderSegment) = .empty;
            defer segments.deinit(allocator);
            var segment_indexes_by_scope: std.AutoHashMapUnmanaged(u64, usize) = .{};
            defer segment_indexes_by_scope.deinit(allocator);

            var render_index: usize = 0;
            while (render_index < self.active_stream.render_nodes.items.len) {
                const node = self.active_stream.render_nodes.items[render_index];
                const scope_id = renderNodeScopeId(&self.active_stream, node);
                const row_scope_id = (self.eachSiteRowAncestorScopeId(scope_id, each_site) catch @panic("scope descriptor referenced an unknown parent scope")) orelse {
                    render_index += 1;
                    continue;
                };
                const start = render_index;
                render_index += 1;
                while (render_index < self.active_stream.render_nodes.items.len) : (render_index += 1) {
                    const next_node = self.active_stream.render_nodes.items[render_index];
                    const next_scope_id = renderNodeScopeId(&self.active_stream, next_node);
                    const next_row_scope_id = self.eachSiteRowAncestorScopeId(next_scope_id, each_site) catch @panic("scope descriptor referenced an unknown parent scope");
                    if (next_row_scope_id == null or next_row_scope_id.? != row_scope_id) break;
                }

                const segment_index = segments.items.len;
                segments.append(allocator, .{
                    .scope_id = row_scope_id,
                    .start = start,
                    .len = render_index - start,
                }) catch @panic("out of memory");
                const entry = segment_indexes_by_scope.getOrPut(allocator, row_scope_id) catch @panic("out of memory");
                if (entry.found_existing) @panic("each row render nodes were split across multiple segments");
                entry.value_ptr.* = segment_index;
            }

            if (segments.items.len != next_scope_ids.len) @panic("pure each permutation did not cover every rendered row");
            if (segments.items.len == 0) return .{};

            const region_start = segments.items[0].start;
            var expected_start = region_start;
            var total_len: usize = 0;
            for (segments.items) |segment| {
                if (segment.start != expected_start) @panic("each row render segments were not contiguous");
                expected_start += segment.len;
                total_len += segment.len;
            }

            var moves_by_scope: std.AutoHashMapUnmanaged(u64, HostEachRowRenderMove) = .{};
            defer moves_by_scope.deinit(allocator);
            const reordered_nodes = allocator.alloc(HostRenderNode, total_len) catch @panic("out of memory");
            defer allocator.free(reordered_nodes);

            var write_index: usize = 0;
            for (next_scope_ids) |scope_id| {
                const segment_index = segment_indexes_by_scope.get(scope_id) orelse @panic("pure each permutation referenced a row without render nodes");
                const segment = segments.items[segment_index];
                const next_start = region_start + write_index;
                @memcpy(reordered_nodes[write_index..][0..segment.len], self.active_stream.render_nodes.items[segment.start..][0..segment.len]);
                moves_by_scope.put(allocator, scope_id, .{
                    .old_start = segment.start,
                    .new_start = next_start,
                    .len = segment.len,
                }) catch @panic("out of memory");
                write_index += segment.len;
            }

            if (write_index != total_len) @panic("pure each permutation wrote the wrong render-node count");
            @memcpy(self.active_stream.render_nodes.items[region_start..][0..total_len], reordered_nodes);

            for (self.active_stream.scope_sites.items) |*scope_site| {
                const row_scope_id = (self.eachSiteRowAncestorScopeId(scope_site.scope_id, each_site) catch @panic("scope descriptor referenced an unknown parent scope")) orelse continue;
                const move = moves_by_scope.get(row_scope_id) orelse @panic("scope site referenced a row missing from pure each permutation");
                if (scope_site.render_insert_index < move.old_start) @panic("scope site insertion point preceded its row render segment");
                const offset = scope_site.render_insert_index - move.old_start;
                if (offset > move.len) @panic("scope site insertion point exceeded its row render segment");
                scope_site.render_insert_index = move.new_start + offset;
            }

            var counts: render.Counts = .{};
            const children = streamDirectChildren(allocator, &self.active_stream, site.parent_elem_id);
            defer allocator.free(children);
            self.replaceRenderChildrenForMoves(ctx, site.parent_elem_id, children, &counts);
            self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn renderInsertIndexForEachRow(self: *Self, site: HostNodeScopeSiteDesc, next_scope_ids: []const u64, row_index: usize) usize {
            if (row_index >= next_scope_ids.len) @panic("each row insertion index was requested outside the next row order");

            if (self.firstRenderIndexInScopeSubtree(&self.active_stream, next_scope_ids[row_index])) |existing_index| {
                return existing_index;
            }

            var next_index = row_index + 1;
            while (next_index < next_scope_ids.len) : (next_index += 1) {
                if (self.firstRenderIndexInScopeSubtree(&self.active_stream, next_scope_ids[next_index])) |insert_before| {
                    return insert_before;
                }
            }

            var previous_index = row_index;
            while (previous_index > 0) {
                previous_index -= 1;
                if (self.lastRenderEndIndexInScopeSubtree(&self.active_stream, next_scope_ids[previous_index])) |insert_after| {
                    return insert_after;
                }
            }

            return site.render_insert_index;
        }

        pub fn applyDirtyEachRowScopeSplices(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            const allocator = Ctx.allocator(ctx);
            var removed_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer removed_elem_ids.deinit(allocator);
            var touched_parent_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer touched_parent_ids.deinit(allocator);
            var replacement_elem_ids: std.ArrayListUnmanaged(u64) = .empty;
            defer replacement_elem_ids.deinit(allocator);
            var replacement_on_change_indices: std.ArrayListUnmanaged(usize) = .empty;
            defer replacement_on_change_indices.deinit(allocator);
            var replacement_mount_indices: std.ArrayListUnmanaged(usize) = .empty;
            defer replacement_mount_indices.deinit(allocator);
            var spliced_any = false;

            for (diff.removed_scope_ids) |removed_scope_id| {
                var empty_stream: HostNodeDescriptorStream = .{};
                const render_insert_index = self.firstRenderIndexInScopeSubtree(&self.active_stream, removed_scope_id) orelse site.render_insert_index;
                const splice = self.spliceActiveStreamReplacingScope(ctx, roc_host, removed_scope_id, render_insert_index, &empty_stream);
                defer splice.deinit(allocator);

                removed_elem_ids.appendSlice(allocator, splice.removed_elem_ids) catch @panic("out of memory");
                for (splice.touched_parent_ids) |parent_id| {
                    appendUniqueU64(allocator, &touched_parent_ids, parent_id);
                }
                replacement_elem_ids.appendSlice(allocator, splice.replacement_elem_ids) catch @panic("out of memory");
                replacement_on_change_indices.appendSlice(allocator, splice.replacement_on_change_indices) catch @panic("out of memory");
                replacement_mount_indices.appendSlice(allocator, splice.replacement_mount_indices) catch @panic("out of memory");
                spliced_any = true;
            }

            for (diff.scope_ids, diff.row_items_changed, 0..) |row_scope_id, row_item_changed, row_index| {
                if (!row_item_changed and !self.scopeSubtreeHasDirtyStructuralSource(&self.active_stream, row_scope_id, dirty_source_node_ids)) {
                    continue;
                }

                var row_stream: HostNodeDescriptorStream = .{};
                defer row_stream.deinit(allocator, ctx, roc_host, &self.pending_roc_metrics);
                self.collectActiveEachSingleRowDescriptors(ctx, roc_host, &row_stream, site, each, row_scope_id, diff.scope_created[row_index], dirty_source_node_ids);

                const render_insert_index = self.renderInsertIndexForEachRow(site, diff.scope_ids, row_index);
                const splice = self.spliceActiveStreamReplacingScope(ctx, roc_host, row_scope_id, render_insert_index, &row_stream);
                defer splice.deinit(allocator);

                removed_elem_ids.appendSlice(allocator, splice.removed_elem_ids) catch @panic("out of memory");
                for (splice.touched_parent_ids) |parent_id| {
                    appendUniqueU64(allocator, &touched_parent_ids, parent_id);
                }
                replacement_elem_ids.appendSlice(allocator, splice.replacement_elem_ids) catch @panic("out of memory");
                replacement_on_change_indices.appendSlice(allocator, splice.replacement_on_change_indices) catch @panic("out of memory");
                replacement_mount_indices.appendSlice(allocator, splice.replacement_mount_indices) catch @panic("out of memory");
                spliced_any = true;
            }

            if (!spliced_any) return .{};

            const merged_splice = HostStructuralSplice{
                .removed_elem_ids = removed_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .touched_parent_ids = touched_parent_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_elem_ids = replacement_elem_ids.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_on_change_indices = replacement_on_change_indices.toOwnedSlice(allocator) catch @panic("out of memory"),
                .replacement_mount_indices = replacement_mount_indices.toOwnedSlice(allocator) catch @panic("out of memory"),
            };
            defer merged_splice.deinit(allocator);
            const target = HostStructuralReplacementTarget{ .each_site = .{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal } };
            return self.applySplicedStructuralNodeDescriptorTarget(ctx, roc_host, merged_splice, .{
                .removed = target,
                .replacement = target,
            }, dirty_source_node_ids, dirty_generation);
        }

        pub fn applyDirtyEachMixedRowSplicesAndMoves(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, diff: HostKeyedRowDiffResult, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            var counts = self.applyDirtyEachRowScopeSplices(ctx, roc_host, site, each, diff, dirty_source_node_ids, dirty_generation);
            counts.addAll(self.applyDirtyEachPermutationMoves(ctx, site, diff.scope_ids));
            return counts;
        }

        pub fn evalOnChangeInitial(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc) void {
            const value = self.evalHostSignalBinding(ctx, roc_host, &desc.signal);
            desc.cached_value.replace(ctx, roc_host, &self.pending_roc_metrics, value, self.hostSignalBindingCapability(ctx, &desc.signal));
        }

        pub fn evalMountCommand(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeMountDesc) render.Counts {
            if (!desc.run_on_mount) return .{};
            desc.run_on_mount = false;

            const cmd = erased_calls.callUnitToStartTaskCmd(roc_host, desc.to_cmd);
            defer abi.decref__AnonStruct86(cmd, roc_host);
            return self.startTaskCommand(ctx, roc_host, desc.scope_id, cmd);
        }

        pub fn runActiveMountCommandIndices(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, indices: []const usize) render.Counts {
            var counts: render.Counts = .{};
            self.recordStreamNodesScanned(indices.len);
            for (indices) |mount_index| {
                if (mount_index >= self.active_stream.mounts.items.len) @panic("mount descriptor index exceeded active descriptor stream");
                counts.addAll(self.evalMountCommand(ctx, roc_host, &self.active_stream.mounts.items[mount_index]));
            }
            return counts;
        }

        pub fn runActiveMountCommands(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost) render.Counts {
            var counts: render.Counts = .{};
            self.recordStreamNodesScanned(self.active_stream.mounts.items.len);
            for (self.active_stream.mounts.items) |*desc| {
                counts.addAll(self.evalMountCommand(ctx, roc_host, desc));
            }
            return counts;
        }

        pub fn applyStructuralEventBindings(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream, seen: []const bool, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            var required = allocator.alloc(HostRequiredEventBindings, seen.len) catch @panic("out of memory");
            defer allocator.free(required);
            @memset(required, .{});

            self.recordStreamNodesScanned(stream.events.items.len);
            for (stream.events.items, 0..) |desc, index| {
                if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) {
                    @panic("event descriptor referenced an element outside the structural render stream");
                }
                const event_id: u64 = @intCast(index + 1);
                const slot = requiredEventBindingSlot(&required[@intCast(desc.elem_id)], desc.kind);
                if (slot.* != null) @panic("element has duplicate event descriptors for one event kind");
                slot.* = .{ .event_id = event_id, .payload_accessor = desc.payload_accessor };
            }

            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;

                for (render_event_kinds) |kind| {
                    const next_binding = requiredEventBindingSlot(&required[index], kind).*;
                    self.applyRenderEventBinding(ctx, @intCast(index), kind, next_binding, counts);
                }
            }
        }

        pub fn applyStructuralEventBindingsForSeen(self: *Self, ctx: Ctx.Handle, stream: *const HostNodeDescriptorStream, seen: []const bool, counts: *render.Counts) void {
            const allocator = Ctx.allocator(ctx);
            var required = allocator.alloc(HostRequiredEventBindings, seen.len) catch @panic("out of memory");
            defer allocator.free(required);
            @memset(required, .{});

            self.recordStreamNodesScanned(stream.events.items.len);
            for (stream.events.items, 0..) |desc, index| {
                if (desc.elem_id >= seen.len or !seen[@intCast(desc.elem_id)]) continue;
                const event_id: u64 = @intCast(index + 1);
                const slot = requiredEventBindingSlot(&required[@intCast(desc.elem_id)], desc.kind);
                if (slot.* != null) @panic("element has duplicate event descriptors for one event kind");
                slot.* = .{ .event_id = event_id, .payload_accessor = desc.payload_accessor };
            }

            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;

                for (render_event_kinds) |kind| {
                    const next_binding = requiredEventBindingSlot(&required[index], kind).*;
                    self.applyRenderEventBinding(ctx, @intCast(index), kind, next_binding, counts);
                }
            }
        }

        pub fn activeEventBindingForElemKind(self: *Self, elem_id: u64, kind: RenderEventKind) ?HostRequiredEventBinding {
            const descriptor_index = self.active_stream.elemDescriptorIndex(elem_id) orelse return null;
            const event_index = descriptor_index.events.get(kind) orelse return null;
            if (event_index >= self.active_stream.events.items.len) @panic("active event descriptor index exceeded descriptor table");
            const desc = self.active_stream.events.items[event_index];
            if (desc.elem_id != elem_id or desc.kind != kind) @panic("active event descriptor index pointed at the wrong event");
            return .{ .event_id = @intCast(event_index + 1), .payload_accessor = desc.payload_accessor };
        }

        pub fn applyStructuralEventBindingsForElem(self: *Self, ctx: Ctx.Handle, elem_id: u64, counts: *render.Counts) void {
            for (render_event_kinds) |kind| {
                const next_binding = self.activeEventBindingForElemKind(elem_id, kind);
                self.applyRenderEventBinding(ctx, elem_id, kind, next_binding, counts);
            }
        }

        pub fn applyActiveStreamEventBindings(self: *Self, ctx: Ctx.Handle, counts: *render.Counts) void {
            self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items) |node| {
                if (node.kind != .element) continue;
                self.applyStructuralEventBindingsForElem(ctx, node.elem_id, counts);
            }
        }

        pub fn applyActiveStreamTextAttrForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, descriptor_index: HostElemDescriptorIndex, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            if (descriptor_index.static_text_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.static_text_attrs.items.len) @panic("active static text attr index exceeded descriptor table");
                const desc = self.active_stream.static_text_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active static text attr index pointed at the wrong field");
                if (self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }

            if (descriptor_index.signal_text_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.signal_text_attrs.items.len) @panic("active signal text attr index exceeded descriptor table");
                const desc = &self.active_stream.signal_text_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active signal text attr index pointed at the wrong field");
                if (self.evalStructuralSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addTextField(desc.field);
                }
            }
        }

        pub fn applyActiveStreamBoolAttrForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, descriptor_index: HostElemDescriptorIndex, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            if (descriptor_index.static_bool_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.static_bool_attrs.items.len) @panic("active static bool attr index exceeded descriptor table");
                const desc = self.active_stream.static_bool_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active static bool attr index pointed at the wrong field");
                if (self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }

            if (descriptor_index.signal_bool_attrs.get(field)) |attr_index| {
                if (attr_index >= self.active_stream.signal_bool_attrs.items.len) @panic("active signal bool attr index exceeded descriptor table");
                const desc = &self.active_stream.signal_bool_attrs.items[attr_index];
                if (desc.elem_id != elem_id or desc.field != field) @panic("active signal bool attr index pointed at the wrong field");
                if (self.evalStructuralSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addBoolField(desc.field);
                }
            }
        }

        pub fn applyActiveStreamFieldsForElem(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, elem_id: u64, counts: *render.Counts, dirty_source_node_ids: []const u64, dirty_generation: u64) void {
            const descriptor_index = self.active_stream.elemDescriptorIndex(elem_id) orelse @panic("active render node had no descriptor index");
            const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value, .class };
            const bool_fields = [_]RenderBoolField{ .checked, .disabled };

            for (text_fields) |field| {
                if (!streamHasTextField(&self.active_stream, elem_id, field) and self.clearRenderTextField(ctx, elem_id, field)) {
                    counts.addTextField(field);
                }
            }
            for (bool_fields) |field| {
                if (!streamHasBoolField(&self.active_stream, elem_id, field) and self.clearRenderBoolField(ctx, elem_id, field)) {
                    counts.addBoolField(field);
                }
            }

            if (descriptor_index.text_node) |text_index| {
                if (text_index >= self.active_stream.text_nodes.items.len) @panic("active text node index exceeded descriptor table");
                const desc = self.active_stream.text_nodes.items[text_index];
                if (desc.elem_id != elem_id) @panic("active text node index pointed at the wrong elem id");
                if (self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            }

            if (descriptor_index.signal_text_node) |signal_text_index| {
                if (signal_text_index >= self.active_stream.signal_text_nodes.items.len) @panic("active signal text node index exceeded descriptor table");
                const desc = &self.active_stream.signal_text_nodes.items[signal_text_index];
                if (desc.elem_id != elem_id) @panic("active signal text node index pointed at the wrong elem id");
                if (self.evalStructuralSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                    counts.addTextField(.text);
                }
            }

            for (text_fields) |field| {
                self.applyActiveStreamTextAttrForElem(ctx, roc_host, elem_id, field, descriptor_index, counts, dirty_source_node_ids, dirty_generation);
            }
            for (bool_fields) |field| {
                self.applyActiveStreamBoolAttrForElem(ctx, roc_host, elem_id, field, descriptor_index, counts, dirty_source_node_ids, dirty_generation);
            }
        }

        pub fn applyStructuralNodeDescriptorTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, targets: HostStructuralPatchTargets) render.Counts {
            if (!self.hasRenderRoot()) @panic("structural DOM patch requested before initial DOM root creation");

            const allocator = Ctx.allocator(ctx);
            const max_elem_id = @max(maxRenderElemId(&self.active_stream), maxRenderElemId(stream));
            const required_child_table_len: usize = @intCast(max_elem_id + 1);
            const child_table_len = required_child_table_len;

            var seen = allocator.alloc(bool, child_table_len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);

            var touched_parents: std.ArrayListUnmanaged(u64) = .empty;
            defer touched_parents.deinit(allocator);

            var counts: render.Counts = .{};

            self.recordStreamNodesScanned(stream.render_nodes.items.len);
            for (stream.render_nodes.items) |node| {
                if (!self.renderNodeInReplacementTarget(stream, node, targets.replacement)) continue;
                if (node.elem_id >= child_table_len) @panic("render node exceeded structural DOM patch table");

                const parent_elem_id = renderNodeParentElemId(stream, node);
                if (parent_elem_id >= child_table_len) @panic("render node referenced parent outside structural DOM patch table");

                self.ensureRenderNode(ctx, node.elem_id, renderNodeTag(stream, node), &counts);
                seen[@intCast(node.elem_id)] = true;
                appendUniqueU64(allocator, &touched_parents, parent_elem_id);
            }

            self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
            for (self.active_stream.render_nodes.items) |node| {
                if (!self.renderNodeInReplacementTarget(&self.active_stream, node, targets.removed)) continue;
                if (node.elem_id < seen.len and seen[@intCast(node.elem_id)]) continue;
                self.removeRenderNode(ctx, node.elem_id, &counts);
            }

            for (touched_parents.items) |parent_elem_id| {
                self.recordStreamNodesScanned(stream.render_nodes.items.len);
                const children = streamDirectChildren(allocator, stream, parent_elem_id);
                defer allocator.free(children);
                self.replaceRenderChildren(ctx, parent_elem_id, children, &counts);
            }

            const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value, .class };
            const bool_fields = [_]RenderBoolField{ .checked, .disabled };
            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;
                const elem_id: u64 = @intCast(index);

                for (text_fields) |field| {
                    if (!streamHasTextField(stream, elem_id, field) and self.clearRenderTextField(ctx, elem_id, field)) {
                        counts.addTextField(field);
                    }
                }
                for (bool_fields) |field| {
                    if (!streamHasBoolField(stream, elem_id, field) and self.clearRenderBoolField(ctx, elem_id, field)) {
                        counts.addBoolField(field);
                    }
                }
            }

            self.recordStreamNodesScanned(stream.text_nodes.items.len);
            for (stream.text_nodes.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            }
            self.recordStreamNodesScanned(stream.signal_text_nodes.items.len);
            for (stream.signal_text_nodes.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(.text);
                }
            }
            self.recordStreamNodesScanned(stream.static_text_attrs.items.len);
            for (stream.static_text_attrs.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }
            self.recordStreamNodesScanned(stream.signal_text_attrs.items.len);
            for (stream.signal_text_attrs.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(desc.field);
                }
            }
            self.recordStreamNodesScanned(stream.static_bool_attrs.items.len);
            for (stream.static_bool_attrs.items) |desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }
            self.recordStreamNodesScanned(stream.signal_bool_attrs.items.len);
            for (stream.signal_bool_attrs.items) |*desc| {
                if (desc.elem_id < seen.len and seen[@intCast(desc.elem_id)] and self.evalSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addBoolField(desc.field);
                }
            }
            self.recordStreamNodesScanned(stream.on_changes.items.len);
            for (stream.on_changes.items) |*desc| {
                if (self.scopeIsInReplacementTarget(desc.scope_id, targets.replacement)) {
                    self.evalOnChangeInitial(ctx, roc_host, desc);
                }
            }

            self.applyStructuralEventBindingsForSeen(ctx, stream, seen, &counts);
            self.debugAssertRenderCacheMatchesStream(ctx, stream);
            self.debugAssertRenderCacheMatchesSink(ctx);

            self.rebuildActiveSignalGraphFromStream(ctx, stream);
            self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applyNodeDescriptorStream(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) render.Counts {
            var counts: render.Counts = .{};
            counts.addHostReset();
            self.resetRenderTree(ctx);

            for (stream.render_nodes.items) |node| {
                switch (node.kind) {
                    .element => {
                        const desc = findElementDesc(stream, node.elem_id) orelse @panic("render node referenced missing element descriptor");
                        self.appendRenderNode(ctx, desc.elem_id, desc.parent_elem_id, desc.tag);
                        counts.addCreateElement();
                        counts.addAppendChild();
                    },
                    .text => {
                        const desc = findTextNodeDesc(stream, node.elem_id) orelse @panic("render node referenced missing text descriptor");
                        self.appendRenderNode(ctx, desc.elem_id, desc.parent_elem_id, "text");
                        counts.addCreateElement();
                        counts.addAppendChild();
                        if (self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                            counts.addTextField(.text);
                        }
                    },
                    .signal_text => {
                        const desc = findSignalTextNodeDescMutable(stream, node.elem_id) orelse @panic("render node referenced missing signal text descriptor");
                        self.appendRenderNode(ctx, desc.elem_id, desc.parent_elem_id, "text");
                        counts.addCreateElement();
                        counts.addAppendChild();
                        if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                            counts.addTextField(.text);
                        }
                    },
                }
            }

            for (stream.static_text_attrs.items) |desc| {
                if (self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.signal_text_attrs.items) |*desc| {
                if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.static_bool_attrs.items) |desc| {
                if (self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.signal_bool_attrs.items) |*desc| {
                if (self.evalSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.on_changes.items) |*desc| {
                self.evalOnChangeInitial(ctx, roc_host, desc);
            }
            for (stream.events.items, 0..) |desc, index| {
                self.applyRenderEventBinding(ctx, desc.elem_id, desc.kind, .{ .event_id = @intCast(index + 1), .payload_accessor = desc.payload_accessor }, &counts);
            }

            self.debugAssertRenderCacheMatchesStream(ctx, stream);
            self.debugAssertRenderCacheMatchesSink(ctx);
            self.rebuildActiveSignalGraphFromStream(ctx, stream);
            self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applySplicedStructuralNodeDescriptorTarget(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, splice: HostStructuralSplice, targets: HostStructuralPatchTargets, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            _ = targets;
            if (!self.hasRenderRoot()) @panic("structural DOM patch requested before initial DOM root creation");

            const allocator = Ctx.allocator(ctx);
            var max_elem_id: u64 = 0;
            for (splice.removed_elem_ids) |elem_id| {
                max_elem_id = @max(max_elem_id, elem_id);
            }
            for (splice.replacement_elem_ids) |elem_id| {
                max_elem_id = @max(max_elem_id, elem_id);
            }
            const required_child_table_len: usize = @intCast(max_elem_id + 1);
            const child_table_len = required_child_table_len;

            var seen = allocator.alloc(bool, child_table_len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);

            var touched_parents: std.ArrayListUnmanaged(u64) = .empty;
            defer touched_parents.deinit(allocator);
            for (splice.touched_parent_ids) |parent_id| {
                appendUniqueU64(allocator, &touched_parents, parent_id);
            }

            var counts: render.Counts = .{};

            self.recordStreamNodesScanned(splice.replacement_elem_ids.len);
            for (splice.replacement_elem_ids) |elem_id| {
                if (elem_id >= child_table_len) @panic("render node exceeded structural DOM patch table");

                const parent_elem_id = streamElemParentElemId(&self.active_stream, elem_id);
                if (parent_elem_id >= child_table_len) @panic("render node referenced parent outside structural DOM patch table");

                self.ensureRenderNode(ctx, elem_id, streamElemTag(&self.active_stream, elem_id), &counts);
                seen[@intCast(elem_id)] = true;
                appendUniqueU64(allocator, &touched_parents, parent_elem_id);
            }

            for (splice.removed_elem_ids) |elem_id| {
                if (elem_id < seen.len and seen[@intCast(elem_id)]) continue;
                self.removeRenderNode(ctx, elem_id, &counts);
            }

            for (touched_parents.items) |parent_elem_id| {
                self.recordStreamNodesScanned(self.active_stream.render_nodes.items.len);
                const children = streamDirectChildren(allocator, &self.active_stream, parent_elem_id);
                defer allocator.free(children);
                self.replaceRenderChildren(ctx, parent_elem_id, children, &counts);
            }

            for (splice.replacement_elem_ids) |elem_id| {
                self.applyActiveStreamFieldsForElem(ctx, roc_host, elem_id, &counts, dirty_source_node_ids, dirty_generation);
            }
            self.applyActiveStreamEventBindings(ctx, &counts);

            self.recordStreamNodesScanned(splice.replacement_on_change_indices.len);
            for (splice.replacement_on_change_indices) |on_change_index| {
                if (on_change_index >= self.active_stream.on_changes.items.len) @panic("structural splice on_change index exceeded active descriptor stream");
                const desc = &self.active_stream.on_changes.items[on_change_index];
                self.evalOnChangeInitial(ctx, roc_host, desc);
            }

            self.debugAssertRenderCacheMatchesStream(ctx, &self.active_stream);
            self.debugAssertRenderCacheMatchesSink(ctx);
            counts.addAll(self.runActiveMountCommandIndices(ctx, roc_host, splice.replacement_mount_indices));
            self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applyStructuralNodeDescriptorStream(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream) render.Counts {
            if (!self.hasRenderRoot()) @panic("structural DOM patch requested before initial DOM root creation");

            const allocator = Ctx.allocator(ctx);
            const max_elem_id = @max(maxRenderElemId(&self.active_stream), maxRenderElemId(stream));
            const required_child_table_len: usize = @intCast(max_elem_id + 1);
            const child_table_len = required_child_table_len;

            var seen = allocator.alloc(bool, child_table_len) catch @panic("out of memory");
            defer allocator.free(seen);
            @memset(seen, false);

            var next_children = allocator.alloc(std.ArrayListUnmanaged(u64), child_table_len) catch @panic("out of memory");
            defer {
                for (next_children) |*children| {
                    children.deinit(allocator);
                }
                allocator.free(next_children);
            }
            for (next_children) |*children| {
                children.* = .empty;
            }

            var counts: render.Counts = .{};

            for (stream.render_nodes.items) |node| {
                if (node.elem_id >= child_table_len) @panic("render node exceeded structural DOM patch table");

                const parent_elem_id = renderNodeParentElemId(stream, node);
                if (parent_elem_id >= child_table_len) @panic("render node referenced parent outside structural DOM patch table");

                self.ensureRenderNode(ctx, node.elem_id, renderNodeTag(stream, node), &counts);
                seen[@intCast(node.elem_id)] = true;

                next_children[@intCast(parent_elem_id)].append(allocator, node.elem_id) catch @panic("out of memory");
            }

            for (self.active_stream.render_nodes.items) |node| {
                const still_rendered = node.elem_id < seen.len and seen[@intCast(node.elem_id)];
                if (!still_rendered) self.removeRenderNode(ctx, node.elem_id, &counts);
            }

            for (next_children, 0..) |*children, index| {
                const accepts_children = index == 0 or (index < seen.len and seen[index]);
                if (!accepts_children) continue;
                self.replaceRenderChildren(ctx, @intCast(index), children.items, &counts);
            }

            const text_fields = [_]RenderTextField{ .text, .role, .label, .test_id, .value, .class };
            const bool_fields = [_]RenderBoolField{ .checked, .disabled };
            for (seen, 0..) |is_seen, index| {
                if (index == 0 or !is_seen) continue;
                const elem_id: u64 = @intCast(index);

                for (text_fields) |field| {
                    if (!streamHasTextField(stream, elem_id, field) and self.clearRenderTextField(ctx, elem_id, field)) {
                        counts.addTextField(field);
                    }
                }
                for (bool_fields) |field| {
                    if (!streamHasBoolField(stream, elem_id, field) and self.clearRenderBoolField(ctx, elem_id, field)) {
                        counts.addBoolField(field);
                    }
                }
            }

            for (stream.text_nodes.items) |desc| {
                if (self.applyRenderTextField(ctx, desc.elem_id, .text, desc.value)) {
                    counts.addTextField(.text);
                }
            }
            for (stream.signal_text_nodes.items) |*desc| {
                if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(.text);
                }
            }
            for (stream.static_text_attrs.items) |desc| {
                if (self.applyRenderTextField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.signal_text_attrs.items) |*desc| {
                if (self.evalSignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addTextField(desc.field);
                }
            }
            for (stream.static_bool_attrs.items) |desc| {
                if (self.applyRenderBoolField(ctx, desc.elem_id, desc.field, desc.value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.signal_bool_attrs.items) |*desc| {
                if (self.evalSignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value)) {
                    counts.addBoolField(desc.field);
                }
            }
            for (stream.on_changes.items) |*desc| {
                self.evalOnChangeInitial(ctx, roc_host, desc);
            }

            self.applyStructuralEventBindings(ctx, stream, seen, &counts);
            self.debugAssertRenderCacheMatchesStream(ctx, stream);
            self.debugAssertRenderCacheMatchesSink(ctx);

            self.rebuildActiveSignalGraphFromStream(ctx, stream);
            self.render_metrics.addCommandCounts(counts);
            return counts;
        }

        pub fn applyDirtyStructuralSignalsLocally(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, dirty_generation: u64, changes: []const HostDirtyStructuralSignal) render.Counts {
            var total_counts: render.Counts = .{};
            var applied_any = false;

            for (changes) |change| {
                var replacement_stream: HostNodeDescriptorStream = .{};
                defer replacement_stream.deinit(Ctx.allocator(ctx), ctx, roc_host, &self.pending_roc_metrics);
                const splice_and_targets: HostStructuralSpliceAndTargets = switch (change.kind) {
                    .when => when_target: {
                        const site = self.activeScopeSiteByNodeId(change.node_id, .when) orelse {
                            if (applied_any) continue;
                            @panic("dirty when structural site is not active");
                        };
                        const when_index = self.activeWhenIndexByNodeId(change.node_id) orelse {
                            if (applied_any) continue;
                            @panic("dirty when descriptor is not active");
                        };
                        const active_branch = change.branch orelse @panic("dirty when structural signal did not record its active branch");
                        const replaced_scope_id = (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite()) catch @panic("scope id has no host scope descriptor")) orelse @panic("dirty when structural update had no active opposite branch scope");
                        const when_desc = self.active_stream.whens.items[when_index];

                        const replacement_scope_id = self.collectActiveWhenBranchDescriptors(ctx, roc_host, &replacement_stream, site, when_desc, active_branch, dirty_source_node_ids);
                        const splice = self.spliceActiveStreamReplacingScope(ctx, roc_host, replaced_scope_id, site.render_insert_index, &replacement_stream);
                        break :when_target .{
                            .splice = splice,
                            .targets = HostStructuralPatchTargets{
                                .removed = .{ .scope = replaced_scope_id },
                                .replacement = .{ .scope = replacement_scope_id },
                            },
                        };
                    },
                    .each => each_target: {
                        const site = self.activeScopeSiteByNodeId(change.node_id, .each) orelse {
                            if (applied_any) continue;
                            @panic("dirty each structural site is not active");
                        };
                        const each_index = self.activeEachIndexByNodeId(change.node_id) orelse {
                            if (applied_any) continue;
                            @panic("dirty each descriptor is not active");
                        };
                        const each_desc = self.active_stream.eaches.items[each_index];
                        const each_site = HostEachSite{ .parent_scope_id = site.scope_id, .site_ordinal = site.ordinal };
                        const target = HostStructuralReplacementTarget{ .each_site = each_site };
                        const allocator = Ctx.allocator(ctx);
                        const old_active_rows = self.activeEachRowScopes(allocator, site.scope_id, site.ordinal) catch @panic("scope id has no host scope descriptor");
                        defer allocator.free(old_active_rows);
                        const old_render_rows = self.activeEachRowScopesInRenderOrder(allocator, each_site);
                        defer allocator.free(old_render_rows);
                        const diff = self.syncActiveEachRowScopes(ctx, roc_host, site, each_desc);
                        defer diff.deinit(allocator);

                        if (old_active_rows.len == old_render_rows.len and Self.eachDiffPreservesSurvivorRenderOrder(old_render_rows, diff.scope_ids)) {
                            const counts = self.applyDirtyEachRowScopeSplices(ctx, roc_host, site, each_desc, diff, dirty_source_node_ids, dirty_generation);
                            total_counts.addAll(counts);
                            applied_any = true;
                            continue;
                        }

                        if (old_active_rows.len == old_render_rows.len and self.eachDiffIsPurePermutation(old_render_rows, diff, dirty_source_node_ids)) {
                            const counts = self.applyDirtyEachPermutationMoves(ctx, site, diff.scope_ids);
                            total_counts.addAll(counts);
                            applied_any = true;
                            continue;
                        }

                        if (old_active_rows.len == old_render_rows.len) {
                            const counts = self.applyDirtyEachMixedRowSplicesAndMoves(ctx, roc_host, site, each_desc, diff, dirty_source_node_ids, dirty_generation);
                            total_counts.addAll(counts);
                            applied_any = true;
                            continue;
                        }

                        self.collectActiveEachRowDescriptorsFromDiff(ctx, roc_host, &replacement_stream, site, each_desc, diff, dirty_source_node_ids);
                        const splice = self.spliceActiveStreamReplacingTarget(
                            ctx,
                            roc_host,
                            target,
                            site.render_insert_index,
                            &replacement_stream,
                        );
                        break :each_target .{
                            .splice = splice,
                            .targets = HostStructuralPatchTargets{
                                .removed = target,
                                .replacement = target,
                            },
                        };
                    },
                };
                defer splice_and_targets.splice.deinit(Ctx.allocator(ctx));

                const counts = self.applySplicedStructuralNodeDescriptorTarget(ctx, roc_host, splice_and_targets.splice, splice_and_targets.targets, dirty_source_node_ids, dirty_generation);
                total_counts.addAll(counts);
                applied_any = true;
            }

            return total_counts;
        }

        pub fn applyDirtyWhenStructuralSignals(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, dirty_generation: u64, changes: []const HostDirtyStructuralSignal) render.Counts {
            for (changes) |change| {
                if (change.kind != .when) @panic("non-when structural change reached when-only test helper");
            }
            return self.applyDirtyStructuralSignalsLocally(ctx, roc_host, dirty_source_node_ids, dirty_generation, changes);
        }

        pub fn appendPendingTask(self: *Self, ctx: Ctx.Handle, owner_scope_id: u64, task_token: HostSignalToken, task_name: []const u8, request: []const u8) u64 {
            const allocator = Ctx.allocator(ctx);
            if (self.next_task_request_id == std.math.maxInt(u64)) @panic("host task request id overflowed");
            const request_id = self.next_task_request_id;
            self.next_task_request_id += 1;

            abi.increfBox(@ptrCast(task_token), 1);
            const task_name_copy = allocator.dupe(u8, task_name) catch @panic("out of memory");
            const request_copy = allocator.dupe(u8, request) catch {
                allocator.free(task_name_copy);
                @panic("out of memory");
            };
            self.pending_tasks.append(allocator, .{
                .request_id = request_id,
                .owner_scope_id = owner_scope_id,
                .task_token = task_token,
                .task_name = task_name_copy,
                .request = request_copy,
                .active = true,
            }) catch {
                releaseHostSignalToken(task_token, self.roc_host.?);
                allocator.free(task_name_copy);
                allocator.free(request_copy);
                @panic("out of memory");
            };
            return request_id;
        }

        pub fn pendingTaskIndexByName(self: *Self, name: []const u8) ?usize {
            var found: ?usize = null;
            for (self.pending_tasks.items, 0..) |task, index| {
                if (!task.active) continue;
                if (!std.mem.eql(u8, task.task_name, name)) continue;
                if (found != null) @panic("fake task result matched more than one pending request");
                found = index;
            }
            return found;
        }

        pub fn removePendingTaskAt(self: *Self, index: usize) HostPendingTask {
            if (index >= self.pending_tasks.items.len) @panic("pending task index is out of bounds");
            const task = self.pending_tasks.items[index];
            const last_index = self.pending_tasks.items.len - 1;
            if (index != last_index) {
                self.pending_tasks.items[index] = self.pending_tasks.items[last_index];
            }
            self.pending_tasks.items.len = last_index;
            return task;
        }

        pub fn activeTaskRecordByName(self: *Self, name: []const u8) ?*HostSignalRecord {
            var found: ?*HostSignalRecord = null;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .task_source => |payload| {
                        if (!std.mem.eql(u8, payload.name, name)) continue;
                        if (found != null) @panic("fake task result matched more than one active task source");
                        found = node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .interval_source => {},
                }
            }
            return found;
        }

        pub fn activeIntervalRecordByPeriod(self: *Self, period_ms: u64) ?*HostSignalRecord {
            var found: ?*HostSignalRecord = null;
            for (self.active_signal_graph.items) |node| {
                switch (node.record.payload) {
                    .interval_source => |payload| {
                        if (payload.period_ms != period_ms) continue;
                        if (found != null) @panic("tick_interval matched more than one active interval source");
                        found = node.record;
                    },
                    .ref, .const_value, .map, .map2, .combine, .task_source => {},
                }
            }
            return found;
        }

        pub fn rebuildActiveEventsFromStream(self: *Self, ctx: Ctx.Handle, stream: *HostNodeDescriptorStream) void {
            const allocator = Ctx.allocator(ctx);
            self.clearActiveEvents() catch @panic("active event table cannot release retained payloads without a Roc host");

            for (stream.events.items) |*desc| {
                if (!desc.owns_payload_reducer) @panic("event descriptor payload reducer ownership was already transferred");
                self.active_events.append(allocator, .{
                    .target_node_id = desc.target_node_id,
                    .payload_kind = desc.payload_kind,
                    .payload_accessor = desc.payload_accessor,
                    .payload_reducer = desc.payload_reducer,
                }) catch @panic("out of memory");
                desc.owns_payload_reducer = false;
            }
        }

        pub fn updateEffectSourceCacheSlot(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue, cap: HostValueCapability) bool {
            switch (cache_slot.*) {
                .absent => {
                    debugPhase(ctx, 301);
                    cache_slot.replace(ctx, roc_host, &self.pending_roc_metrics, value, cap);
                    return true;
                },
                .present => |*cached| {
                    debugPhase(ctx, 310);
                    if (cached.valueEquals(ctx, roc_host, value)) {
                        debugPhase(ctx, 311);
                        cached.dropIncoming(ctx, roc_host, value);
                        self.recordSignalPrune();
                        return false;
                    }
                    debugPhase(ctx, 312);
                    cached.replaceValue(ctx, roc_host, value);
                    return true;
                },
            }
        }

        pub fn updateEffectSourceCache(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord, value: HostValue) bool {
            return switch (record.payload) {
                .task_source => |*payload| self.updateEffectSourceCacheSlot(ctx, roc_host, &payload.cached_value, value, payload.cap),
                .interval_source => |*payload| self.updateEffectSourceCacheSlot(ctx, roc_host, &payload.cached_value, value, payload.cap),
                .ref, .const_value, .map, .map2, .combine => @panic("effect source update targeted a non-source signal record"),
            };
        }

        pub fn applyDirtySignalBatch(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) render.Counts {
            debugPhase(ctx, 350);
            const dirty_structural_signals = self.collectDirtyStructuralSignals(ctx, roc_host, Ctx.allocator(ctx), dirty_source_node_ids, changed_record_ids, dirty_generation);
            defer Ctx.allocator(ctx).free(dirty_structural_signals);

            debugPhase(ctx, 360);
            var counts = self.applyDirtyRenderSinks(ctx, roc_host, dirty_source_node_ids, changed_record_ids, dirty_generation);
            if (dirty_structural_signals.len != 0) {
                debugPhase(ctx, 370);
                counts.addAll(self.applyDirtyStructuralSignalsLocally(ctx, roc_host, dirty_source_node_ids, dirty_generation, dirty_structural_signals));
            }
            return counts;
        }

        pub fn dispatchEffectSourceValue(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, record: *HostSignalRecord, value: HostValue) render.Counts {
            debugPhase(ctx, 300);
            if (!self.updateEffectSourceCache(ctx, roc_host, record, value)) return .{};

            self.recordDispatch();
            var metrics = self.pending_roc_metrics;
            metrics.bump(.nodes_recomputed, 1);
            self.pending_roc_metrics = metrics;
            const dirty_generation = self.nextDirtySignalGeneration();
            record.last_dirty_generation = dirty_generation;
            record.last_dirty_changed = true;

            const record_id = self.requireActiveSignalRecordId(record);
            const roots = [_]u64{record_id};
            const dirty_record_ids = self.dirtyActiveSignalRecordIdsForRoots(Ctx.allocator(ctx), &roots);
            defer Ctx.allocator(ctx).free(dirty_record_ids);

            debugPhase(ctx, 330);
            const changed_record_ids = self.propagateDirtyActiveSignalRecordIds(ctx, roc_host, Ctx.allocator(ctx), dirty_record_ids, &.{}, dirty_generation);
            defer Ctx.allocator(ctx).free(changed_record_ids);
            debugPhase(ctx, 340);
            return self.applyDirtySignalBatch(ctx, roc_host, &.{}, changed_record_ids, dirty_generation);
        }

        pub fn startTaskCommand(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, owner_scope_id: u64, cmd: erased_calls.StartTaskCmd) render.Counts {
            const record = self.activeTaskRecordByToken(cmd.task_token) orelse @panic("StartTask referenced a task source that is not active");
            const task_payload = switch (record.payload) {
                .task_source => |payload| payload,
                .ref, .const_value, .map, .map2, .combine, .interval_source => unreachable,
            };
            if (!std.mem.eql(u8, task_payload.name, cmd.task_name.asSlice())) {
                @panic("StartTask task name does not match the referenced task source");
            }

            const request_value = erased_calls.callValueInitThunk(roc_host, cmd.request_init);
            defer callHostValueToUnitWithCapability(ctx, roc_host, cmd.request_read.capability, hv.hostValueCapabilityDrop(cmd.request_read.capability), request_value);
            const request = callHostValueToStrWithCapability(ctx, roc_host, cmd.request_read.capability, cmd.request_read.read, request_value);
            defer request.decref(roc_host);

            self.cancelPendingTasksByTaskToken(ctx, cmd.task_token);
            const request_id = self.appendPendingTask(ctx, owner_scope_id, cmd.task_token, cmd.task_name.asSlice(), request.asSlice());
            Ctx.sink(ctx).startTask(request_id, cmd.task_name.asSlice(), request.asSlice());

            if (task_payload.reset_on_start) {
                const loading = erased_calls.callValueInitThunk(roc_host, task_payload.initial);
                return self.dispatchEffectSourceValue(ctx, roc_host, record, loading);
            }

            return .{};
        }

        pub fn tickIntervalSource(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, period_ms: u64) render.Counts {
            const record = self.activeIntervalRecordByPeriod(period_ms) orelse @panic("tick_interval matched no active interval source");
            const interval_payload = switch (record.payload) {
                .interval_source => |payload| payload,
                .ref, .const_value, .map, .map2, .combine, .task_source => unreachable,
            };

            const current = self.evalHostSignalRecord(ctx, roc_host, record);
            defer self.dropHostSignalRecordValue(ctx, roc_host, record, current);
            const next = callHostValueToHostValueWithCapability(ctx, roc_host, interval_payload.cap, interval_payload.tick, current);
            return self.dispatchEffectSourceValue(ctx, roc_host, record, next);
        }

        pub fn tickIntervalSourceByRuntimeToken(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, token: u64) render.Counts {
            const source_token = self.activeIntervalSourceTokenByRuntimeToken(token) orelse @panic("timer tick referenced an inactive interval token");
            const record = self.activeIntervalRecordByToken(source_token) orelse @panic("timer tick matched no active interval source");
            const interval_payload = switch (record.payload) {
                .interval_source => |payload| payload,
                .ref, .const_value, .map, .map2, .combine, .task_source => unreachable,
            };

            const current = self.evalHostSignalRecord(ctx, roc_host, record);
            defer self.dropHostSignalRecordValue(ctx, roc_host, record, current);
            const next = callHostValueToHostValueWithCapability(ctx, roc_host, interval_payload.cap, interval_payload.tick, current);
            return self.dispatchEffectSourceValue(ctx, roc_host, record, next);
        }

        pub fn evalDirtyOnChange(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, desc: *HostNodeOnChangeDesc, dirty_source_node_ids: []const u64, dirty_generation: u64) render.Counts {
            const result = self.evalDirtyHostSignalBinding(ctx, roc_host, &desc.signal, dirty_source_node_ids, dirty_generation);
            const cap = self.hostSignalBindingCapability(ctx, &desc.signal);
            if (!result.changed) {
                callHostValueToUnitWithCapability(ctx, roc_host, cap, hv.hostValueCapabilityDrop(cap), result.value);
                return .{};
            }
            if (!self.updateDirtySignalCache(ctx, roc_host, &desc.cached_value, result.value, cap)) return .{};

            const cmd = callHostValueToStartTaskCmdWithCapability(ctx, roc_host, cap, desc.to_cmd, result.value);
            defer abi.decref__AnonStruct86(cmd, roc_host);
            return self.startTaskCommand(ctx, roc_host, desc.scope_id, cmd);
        }

        pub fn applyDirtyRenderSinks(self: *Self, ctx: Ctx.Handle, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changed_record_ids: []const u64, dirty_generation: u64) render.Counts {
            var counts: render.Counts = .{};

            for (changed_record_ids) |record_id| {
                const route_index: usize = @intCast(record_id);
                if (route_index < self.active_text_signal_routes.items.len) {
                    for (self.active_text_signal_routes.items[route_index].items) |route| {
                        switch (route.kind) {
                            .text_node => {
                                const desc = &self.active_stream.signal_text_nodes.items[route.index];
                                if (self.evalDirtySignalTextField(ctx, roc_host, desc.elem_id, .text, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                                    counts.addTextField(.text);
                                }
                            },
                            .text_attr => {
                                const desc = &self.active_stream.signal_text_attrs.items[route.index];
                                if (self.evalDirtySignalTextField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                                    counts.addTextField(desc.field);
                                }
                            },
                        }
                    }
                }

                if (route_index < self.active_bool_signal_routes.items.len) {
                    for (self.active_bool_signal_routes.items[route_index].items) |route| {
                        const desc = &self.active_stream.signal_bool_attrs.items[route.index];
                        if (self.evalDirtySignalBoolField(ctx, roc_host, desc.elem_id, desc.field, &desc.signal, desc.read, &desc.cached_value, dirty_source_node_ids, dirty_generation)) {
                            counts.addBoolField(desc.field);
                        }
                    }
                }

                if (route_index < self.active_change_signal_routes.items.len) {
                    for (self.active_change_signal_routes.items[route_index].items) |route| {
                        const desc = &self.active_stream.on_changes.items[route.index];
                        counts.addAll(self.evalDirtyOnChange(ctx, roc_host, desc, dirty_source_node_ids, dirty_generation));
                    }
                }
            }

            self.render_metrics.addCommandCounts(counts);
            return counts;
        }
    };
}

const VerifyCtxHost = struct {};

const VerifySink = struct {
    pub fn reset(_: VerifySink) void {}
    pub fn appendNode(_: VerifySink, _: u64, _: u64, _: []const u8) void {}
    pub fn ensureNode(_: VerifySink, _: u64, _: []const u8) void {}
    pub fn removeNode(_: VerifySink, _: u64) void {}
    pub fn replaceChildren(_: VerifySink, _: u64, _: []const u64) void {}
    pub fn replaceChildrenForMoves(_: VerifySink, _: u64, _: []const u64) void {}
    pub fn applyTextField(_: VerifySink, _: u64, _: RenderTextField, _: []const u8) void {}
    pub fn applyBoolField(_: VerifySink, _: u64, _: RenderBoolField, _: bool) void {}
    pub fn clearTextField(_: VerifySink, _: u64, _: RenderTextField) void {}
    pub fn clearBoolField(_: VerifySink, _: u64, _: RenderBoolField) void {}
    pub fn bindEventKind(_: VerifySink, _: u64, _: RenderEventKind, _: u64, _: EventPayloadAccessor) void {}
    pub fn clearEvent(_: VerifySink, _: u64, _: RenderEventKind) void {}
    pub fn startInterval(_: VerifySink, _: u64, _: u64) void {}
    pub fn cancelInterval(_: VerifySink, _: u64) void {}
    pub fn startTask(_: VerifySink, _: u64, _: []const u8, _: []const u8) void {}
    pub fn cancelTask(_: VerifySink, _: u64) void {}
    pub fn debugAssertNode(_: VerifySink, _: u64, _: bool, _: ?[]const u8, _: ?u64, _: []const u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64) void {}
};

const VerifyCtx = struct {
    pub const Handle = *VerifyCtxHost;
    pub const RegistryOps = hv.RegistryOps();
    pub const Metrics = RuntimeMetrics;
    pub const Sink = VerifySink;

    pub fn zeroMetrics() Metrics {
        return zeroRuntimeMetrics();
    }

    pub fn allocator(_: Handle) std.mem.Allocator {
        return std.heap.page_allocator;
    }

    pub fn cloneHostValue(_: Handle, value: HostValue) HostValue {
        return value;
    }

    pub fn pushHostValueCapabilities(_: Handle, _: []const HostValueCapability) void {}

    pub fn popHostValueCapabilities(_: Handle) void {}

    pub fn stateValueByNodeId(_: Handle, _: u64) HostValue {
        return 0;
    }

    pub fn stateCapability(_: Handle, _: u64) HostValueCapability {
        return undefined;
    }

    pub fn sink(_: Handle) Sink {
        return .{};
    }
};

comptime {
    verifyCtx(VerifyCtx);
    std.debug.assert(@sizeOf(NoMetrics) == 0);
    _ = Engine(VerifyCtx);
}
