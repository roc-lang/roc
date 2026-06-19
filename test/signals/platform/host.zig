//! Platform host for testing signal-based reactive UI applications.
//!
//! The host owns simulated DOM state, event routing, signal topology/cache, and
//! render batching. Roc still runs retained app code and emits explicit render
//! descriptors through the `roc_ui_*` entrypoints during the migration.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const abi = @import("roc_platform_abi.zig");

const DispatchResultBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const DispatchResult = std.meta.Child(DispatchResultBox);
const RecomputeResultBox = @typeInfo(@TypeOf(abi.roc_ui_recompute)).@"fn".return_type.?;
const RecomputeResult = std.meta.Child(RecomputeResultBox);
const RuntimeBox = @typeInfo(@TypeOf(abi.roc_ui_drop)).@"fn".params[0].type.?;
const RuntimeMetrics = @FieldType(DispatchResult, "metrics");
const EventDescriptorList = @FieldType(DispatchResult, "event_descriptors");
const SignalValueList = @FieldType(DispatchResult, "signal_changes");
const SignalDescriptorList = @FieldType(DispatchResult, "signal_descriptors");
const StateDescriptorList = @FieldType(DispatchResult, "state_descriptors");
const StateChangeList = @FieldType(DispatchResult, "state_changes");
const SignalValueDesc = @typeInfo(@TypeOf(@as(SignalValueList, undefined).items())).pointer.child;
const StateValueDesc = @typeInfo(@TypeOf(@as(StateChangeList, undefined).items())).pointer.child;
const RenderElementDescList = @FieldType(DispatchResult, "render_elements");
const RenderTextDescList = @FieldType(DispatchResult, "render_texts");
const RenderSignalTextDescList = @FieldType(DispatchResult, "render_signal_texts");
const RenderBoolDescList = @FieldType(DispatchResult, "render_bools");
const RenderSignalBoolDescList = @FieldType(DispatchResult, "render_signal_bools");
const RenderEventDescList = @FieldType(DispatchResult, "render_events");
const RenderStructuralDescList = @FieldType(DispatchResult, "render_structures");
const RenderElementDesc = @typeInfo(@TypeOf(@as(RenderElementDescList, undefined).items())).pointer.child;
const RenderTextDesc = @typeInfo(@TypeOf(@as(RenderTextDescList, undefined).items())).pointer.child;
const RenderSignalTextDesc = @typeInfo(@TypeOf(@as(RenderSignalTextDescList, undefined).items())).pointer.child;
const RenderBoolDesc = @typeInfo(@TypeOf(@as(RenderBoolDescList, undefined).items())).pointer.child;
const RenderSignalBoolDesc = @typeInfo(@TypeOf(@as(RenderSignalBoolDescList, undefined).items())).pointer.child;
const RenderEventDesc = @typeInfo(@TypeOf(@as(RenderEventDescList, undefined).items())).pointer.child;
const RenderStructuralDesc = @typeInfo(@TypeOf(@as(RenderStructuralDescList, undefined).items())).pointer.child;
const RecomputeInputBox = @typeInfo(@TypeOf(abi.roc_ui_recompute)).@"fn".params[1].type.?;
const RecomputeInput = std.meta.Child(RecomputeInputBox);
const ActiveEvent = @FieldType(RecomputeInput, "active_event");
const RenderInputBox = @typeInfo(@TypeOf(abi.roc_ui_render)).@"fn".params[1].type.?;
const RenderInput = std.meta.Child(RenderInputBox);
const RocStr = abi.RocStr;

const EventPayloadKind = enum(u64) {
    unit = 1,
    str = 2,
    bool = 3,
};

const SignalKind = enum(u64) {
    source = 1,
    map = 2,
    map2 = 3,
};

const RenderTextField = enum(u64) {
    text = 1,
    role = 2,
    label = 3,
    test_id = 4,
    value = 5,
};

const RenderBoolField = enum(u64) {
    checked = 1,
    disabled = 2,
};

const RenderEventKind = enum(u64) {
    click = 1,
    input = 2,
    check = 3,
};

const HostEventDescriptor = struct {
    event_id: u64,
    payload_kind: EventPayloadKind,
};

const HostSignalEventRoute = struct {
    event_id: u64,
    signal_ids: []u64,
};

const HostState = struct {
    state_id: u64,
    value: abi.NodeValue,
    version: u64,
};

const HostSignalDescriptor = struct {
    signal_id: u64,
    kind: SignalKind,
    source_state_ids: []u64,
    source_event_ids: []u64,
    input_signal_ids: []u64,
    rank: u64,
};

const HostSignalRoute = struct {
    state_id: u64,
    signal_ids: []u64,
};

const HostSignalDependentsRoute = struct {
    signal_id: u64,
    signal_ids: []u64,
};

const HostSignalCacheSlot = union(enum) {
    absent,
    present: abi.NodeValue,
};

const SignalEventPayload = struct {
    dirty_signal_ids: abi.RocListWith(u64, false),
    cached_signals: SignalValueList,
    cached_states: StateChangeList,
};

const HostScopeBranch = enum(u8) {
    false_branch,
    true_branch,

    fn opposite(self: HostScopeBranch) HostScopeBranch {
        return switch (self) {
            .false_branch => .true_branch,
            .true_branch => .false_branch,
        };
    }
};

const HostWhenBranchScopeStep = struct {
    site_ordinal: u64,
    branch: HostScopeBranch,
};

const HostEachRowScopeStep = struct {
    site_ordinal: u64,
    key: abi.NodeValue,
};

const HostScopeStep = union(enum) {
    root,
    when_branch: HostWhenBranchScopeStep,
    each_row: HostEachRowScopeStep,

    fn deinit(self: *HostScopeStep, roc_host: *abi.RocHost) void {
        switch (self.*) {
            .each_row => |row| abi.decrefNodeValue(row.key, roc_host),
            .root, .when_branch => {},
        }
    }
};

const HostScope = struct {
    scope_id: u64,
    parent_scope_id: ?u64,
    step: HostScopeStep,
    active: bool,
};

const HostNodeIdentity = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    active: bool,
};

const HostNodeScopeSiteKind = enum {
    state,
    when,
    each,
};

const HostNodeScopeSiteDesc = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    parent_elem_id: u64,
    kind: HostNodeScopeSiteKind,
    binder_node_ids: []u64,
};

const HostNodeElementDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    tag: []const u8,
};

const HostNodeTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    value: []const u8,
};

const HostNodeSignalTextNodeDesc = struct {
    elem_id: u64,
    parent_elem_id: u64,
    scope_id: u64,
    signal: abi.NodeSignalExpr,
    source_node_ids: []u64,
};

const HostNodeStaticTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    value: []const u8,
};

const HostNodeSignalTextAttrDesc = struct {
    elem_id: u64,
    field: RenderTextField,
    signal: abi.NodeSignalExpr,
    source_node_ids: []u64,
};

const HostNodeStaticBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    value: bool,
};

const HostNodeSignalBoolAttrDesc = struct {
    elem_id: u64,
    field: RenderBoolField,
    signal: abi.NodeSignalExpr,
    source_node_ids: []u64,
};

const HostNodeEventDesc = struct {
    elem_id: u64,
    kind: RenderEventKind,
    binder_ref: u64,
    target_node_id: u64,
    payload_kind: EventPayloadKind,
    transform: abi.RocErasedCallable,
};

const HostNodeStateDesc = struct {
    node_id: u64,
    initial: abi.NodeValue,
    eq: abi.RocErasedCallable,
};

const HostNodeWhenDesc = struct {
    node_id: u64,
    condition: abi.NodeSignalExpr,
    source_node_ids: []u64,
};

const HostNodeEachDesc = struct {
    node_id: u64,
    items: abi.NodeSignalExpr,
    source_node_ids: []u64,
    key_of: abi.RocErasedCallable,
    key_eq: abi.RocErasedCallable,
    row: abi.RocErasedCallable,
};

const HostNodeDescriptorStream = struct {
    elements: std.ArrayListUnmanaged(HostNodeElementDesc) = .empty,
    text_nodes: std.ArrayListUnmanaged(HostNodeTextNodeDesc) = .empty,
    signal_text_nodes: std.ArrayListUnmanaged(HostNodeSignalTextNodeDesc) = .empty,
    static_text_attrs: std.ArrayListUnmanaged(HostNodeStaticTextAttrDesc) = .empty,
    signal_text_attrs: std.ArrayListUnmanaged(HostNodeSignalTextAttrDesc) = .empty,
    static_bool_attrs: std.ArrayListUnmanaged(HostNodeStaticBoolAttrDesc) = .empty,
    signal_bool_attrs: std.ArrayListUnmanaged(HostNodeSignalBoolAttrDesc) = .empty,
    events: std.ArrayListUnmanaged(HostNodeEventDesc) = .empty,
    scope_sites: std.ArrayListUnmanaged(HostNodeScopeSiteDesc) = .empty,
    states: std.ArrayListUnmanaged(HostNodeStateDesc) = .empty,
    whens: std.ArrayListUnmanaged(HostNodeWhenDesc) = .empty,
    eaches: std.ArrayListUnmanaged(HostNodeEachDesc) = .empty,
    next_elem_id: u64 = 1,

    fn deinit(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost) void {
        for (self.elements.items) |desc| {
            allocator.free(desc.tag);
        }
        self.elements.deinit(allocator);

        for (self.text_nodes.items) |desc| {
            allocator.free(desc.value);
        }
        self.text_nodes.deinit(allocator);

        for (self.signal_text_nodes.items) |desc| {
            abi.decrefNodeSignalExpr(desc.signal, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.signal_text_nodes.deinit(allocator);

        for (self.static_text_attrs.items) |desc| {
            allocator.free(desc.value);
        }
        self.static_text_attrs.deinit(allocator);

        for (self.signal_text_attrs.items) |desc| {
            abi.decrefNodeSignalExpr(desc.signal, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.signal_text_attrs.deinit(allocator);

        self.static_bool_attrs.deinit(allocator);

        for (self.signal_bool_attrs.items) |desc| {
            abi.decrefNodeSignalExpr(desc.signal, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.signal_bool_attrs.deinit(allocator);

        for (self.events.items) |desc| {
            abi.decrefErasedCallable(desc.transform, roc_host);
        }
        self.events.deinit(allocator);

        for (self.scope_sites.items) |desc| {
            allocator.free(desc.binder_node_ids);
        }
        self.scope_sites.deinit(allocator);

        for (self.states.items) |desc| {
            abi.decrefNodeValue(desc.initial, roc_host);
            abi.decrefErasedCallable(desc.eq, roc_host);
        }
        self.states.deinit(allocator);

        for (self.whens.items) |desc| {
            abi.decrefNodeSignalExpr(desc.condition, roc_host);
            allocator.free(desc.source_node_ids);
        }
        self.whens.deinit(allocator);

        for (self.eaches.items) |desc| {
            abi.decrefNodeSignalExpr(desc.items, roc_host);
            allocator.free(desc.source_node_ids);
            abi.decrefErasedCallable(desc.key_of, roc_host);
            abi.decrefErasedCallable(desc.key_eq, roc_host);
            abi.decrefErasedCallable(desc.row, roc_host);
        }
        self.eaches.deinit(allocator);

        self.* = .{};
    }

    fn appendElement(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64, scope_id: u64, tag: []const u8) u64 {
        const elem_id = self.next_elem_id;
        self.next_elem_id += 1;

        const tag_copy = allocator.dupe(u8, tag) catch std.process.exit(1);
        self.elements.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .tag = tag_copy,
        }) catch {
            allocator.free(tag_copy);
            std.process.exit(1);
        };
        return elem_id;
    }

    fn appendTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, parent_elem_id: u64, scope_id: u64, value: []const u8) void {
        const elem_id = self.next_elem_id;
        self.next_elem_id += 1;

        const value_copy = allocator.dupe(u8, value) catch std.process.exit(1);
        self.text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            std.process.exit(1);
        };
    }

    fn appendSignalTextNode(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, parent_elem_id: u64, scope_id: u64, signal: abi.NodeSignalExpr, source_node_ids: []u64) void {
        const elem_id = self.next_elem_id;
        self.next_elem_id += 1;

        abi.increfNodeSignalExpr(signal, 1);
        self.signal_text_nodes.append(allocator, .{
            .elem_id = elem_id,
            .parent_elem_id = parent_elem_id,
            .scope_id = scope_id,
            .signal = signal,
            .source_node_ids = source_node_ids,
        }) catch {
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendStaticTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderTextField, value: []const u8) void {
        const value_copy = allocator.dupe(u8, value) catch std.process.exit(1);
        self.static_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value_copy,
        }) catch {
            allocator.free(value_copy);
            std.process.exit(1);
        };
    }

    fn appendSignalTextAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, elem_id: u64, field: RenderTextField, signal: abi.NodeSignalExpr, source_node_ids: []u64) void {
        abi.increfNodeSignalExpr(signal, 1);
        self.signal_text_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .source_node_ids = source_node_ids,
        }) catch {
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendStaticBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, elem_id: u64, field: RenderBoolField, value: bool) void {
        self.static_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .value = value,
        }) catch std.process.exit(1);
    }

    fn appendSignalBoolAttr(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, elem_id: u64, field: RenderBoolField, signal: abi.NodeSignalExpr, source_node_ids: []u64) void {
        abi.increfNodeSignalExpr(signal, 1);
        self.signal_bool_attrs.append(allocator, .{
            .elem_id = elem_id,
            .field = field,
            .signal = signal,
            .source_node_ids = source_node_ids,
        }) catch {
            abi.decrefNodeSignalExpr(signal, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendEvent(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, elem_id: u64, kind: RenderEventKind, binder_ref: u64, target_node_id: u64, payload_kind: EventPayloadKind, transform: abi.RocErasedCallable) void {
        abi.increfErasedCallable(transform, 1);
        self.events.append(allocator, .{
            .elem_id = elem_id,
            .kind = kind,
            .binder_ref = binder_ref,
            .target_node_id = target_node_id,
            .payload_kind = payload_kind,
            .transform = transform,
        }) catch {
            abi.decrefErasedCallable(transform, roc_host);
            std.process.exit(1);
        };
    }

    fn appendScopeSite(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, node_id: u64, scope_id: u64, ordinal: u64, parent_elem_id: u64, kind: HostNodeScopeSiteKind, binder_node_ids: []const u64) void {
        const binder_copy = allocator.dupe(u64, binder_node_ids) catch std.process.exit(1);
        self.scope_sites.append(allocator, .{
            .node_id = node_id,
            .scope_id = scope_id,
            .ordinal = ordinal,
            .parent_elem_id = parent_elem_id,
            .kind = kind,
            .binder_node_ids = binder_copy,
        }) catch {
            allocator.free(binder_copy);
            std.process.exit(1);
        };
    }

    fn appendState(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, node_id: u64, initial: abi.NodeValue, eq: abi.RocErasedCallable) void {
        abi.increfNodeValue(initial, 1);
        abi.increfErasedCallable(eq, 1);
        self.states.append(allocator, .{
            .node_id = node_id,
            .initial = initial,
            .eq = eq,
        }) catch {
            abi.decrefNodeValue(initial, roc_host);
            abi.decrefErasedCallable(eq, roc_host);
            std.process.exit(1);
        };
    }

    fn appendWhen(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, node_id: u64, condition: abi.NodeSignalExpr, source_node_ids: []u64) void {
        abi.increfNodeSignalExpr(condition, 1);
        self.whens.append(allocator, .{
            .node_id = node_id,
            .condition = condition,
            .source_node_ids = source_node_ids,
        }) catch {
            abi.decrefNodeSignalExpr(condition, roc_host);
            allocator.free(source_node_ids);
            std.process.exit(1);
        };
    }

    fn appendEach(self: *HostNodeDescriptorStream, allocator: std.mem.Allocator, roc_host: *abi.RocHost, node_id: u64, items: abi.NodeSignalExpr, source_node_ids: []u64, key_of: abi.RocErasedCallable, key_eq: abi.RocErasedCallable, row: abi.RocErasedCallable) void {
        abi.increfNodeSignalExpr(items, 1);
        abi.increfErasedCallable(key_of, 1);
        abi.increfErasedCallable(key_eq, 1);
        abi.increfErasedCallable(row, 1);
        self.eaches.append(allocator, .{
            .node_id = node_id,
            .items = items,
            .source_node_ids = source_node_ids,
            .key_of = key_of,
            .key_eq = key_eq,
            .row = row,
        }) catch {
            abi.decrefNodeSignalExpr(items, roc_host);
            allocator.free(source_node_ids);
            abi.decrefErasedCallable(key_of, roc_host);
            abi.decrefErasedCallable(key_eq, roc_host);
            abi.decrefErasedCallable(row, roc_host);
            std.process.exit(1);
        };
    }
};

const HostRenderTextSink = struct {
    elem_id: u64,
    field: RenderTextField,
};

const HostRenderBoolSink = struct {
    elem_id: u64,
    field: RenderBoolField,
};

const RecomputeApplyOutcome = struct {
    structural_render_required: bool,
};

const HostKeyedRowDiffResult = struct {
    scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,
};

const HostRenderMetrics = struct {
    patches_emitted: u64 = 0,
};

const HostDispatchMetrics = struct {
    events_processed: u64 = 0,
    recompute_batches: u64 = 0,
};

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    .allow_stack_tracing = false,
};

pub const panic = std.debug.FullPanic(panicImpl);

fn writeStderr(bytes: []const u8) void {
    std.Io.File.stderr().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn printStderr(comptime fmt: []const u8, args: anytype) void {
    var buf: [2048]u8 = undefined;
    const out = std.fmt.bufPrint(&buf, fmt, args) catch return;
    writeStderr(out);
}

fn writeStdout(bytes: []const u8) void {
    std.Io.File.stdout().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn printStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [2048]u8 = undefined;
    const out = std.fmt.bufPrint(&buf, fmt, args) catch return;
    writeStdout(out);
}

fn benchmarkNowNs() u64 {
    const ns = std.Io.Clock.awake.now(std.Io.Threaded.global_single_threaded.io()).nanoseconds;
    return @intCast(@max(ns, 0));
}

fn traceEnabled() bool {
    if (current_host) |host| {
        return host.test_state.verbose;
    }
    return false;
}

fn traceStderr(bytes: []const u8) void {
    if (traceEnabled()) {
        writeStderr(bytes);
    }
}

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    writeStderr("\n=== PANIC (no stack trace) ===\n");
    writeStderr(msg);
    if (addr) |a| {
        var buf: [32]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, " at address 0x{x}\n", .{a}) catch "";
        writeStderr(hex);
    } else {
        writeStderr("\n");
    }
    std.process.abort();
}

const STACK_OVERFLOW_MESSAGE = "\nThis Roc application overflowed its stack memory and crashed.\n\n";

fn handleRocStackOverflow() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, STACK_OVERFLOW_MESSAGE.ptr, STACK_OVERFLOW_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(134);
    } else if (comptime builtin.os.tag != .wasi) {
        writeStderr(STACK_OVERFLOW_MESSAGE);
        std.process.exit(134);
    } else {
        std.process.exit(134);
    }
}

fn handleRocAccessViolation(fault_addr: usize, _: base.signal_handler.AccessViolationContext) noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        var addr_buf: [18]u8 = undefined;
        const addr_str = base.signal_handler.formatHex(fault_addr, &addr_buf);
        const msg1 = "\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ";
        const msg2 = "\n\n";
        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, msg1.ptr, msg1.len, &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, addr_str.ptr, @intCast(addr_str.len), &bytes_written, null);
        _ = kernel32.WriteFile(stderr_handle, msg2.ptr, msg2.len, &bytes_written, null);
        kernel32.ExitProcess(139);
    } else {
        writeStderr("\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ");
        var addr_buf: [18]u8 = undefined;
        writeStderr(base.signal_handler.formatHex(fault_addr, &addr_buf));
        writeStderr("\n\n");
        std.process.exit(139);
    }
}

const DIVISION_BY_ZERO_MESSAGE = "\nThis Roc application divided by zero and crashed.\n\n";

fn handleRocArithmeticError() noreturn {
    if (comptime builtin.os.tag == .windows) {
        const DWORD = u32;
        const HANDLE = ?*anyopaque;
        const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

        const kernel32 = struct {
            extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;
            extern "kernel32" fn WriteFile(hFile: HANDLE, lpBuffer: [*]const u8, nNumberOfBytesToWrite: DWORD, lpNumberOfBytesWritten: ?*DWORD, lpOverlapped: ?*anyopaque) callconv(.winapi) i32;
            extern "kernel32" fn ExitProcess(uExitCode: c_uint) callconv(.winapi) noreturn;
        };

        const stderr_handle = kernel32.GetStdHandle(STD_ERROR_HANDLE);
        var bytes_written: DWORD = 0;
        _ = kernel32.WriteFile(stderr_handle, DIVISION_BY_ZERO_MESSAGE.ptr, DIVISION_BY_ZERO_MESSAGE.len, &bytes_written, null);
        kernel32.ExitProcess(136);
    } else if (comptime builtin.os.tag != .wasi) {
        writeStderr(DIVISION_BY_ZERO_MESSAGE);
        std.process.exit(136);
    } else {
        std.process.exit(136);
    }
}

const DomElement = struct {
    id: u64,
    tag: []const u8,
    role: ?[]const u8,
    label: ?[]const u8,
    test_id: ?[]const u8,
    text: ?[]const u8,
    value: ?[]const u8,
    checked: bool,
    disabled: bool,
    parent_id: ?u64,
    children: std.ArrayListUnmanaged(u64),
    bound_click_event: ?u64,
    bound_input_event: ?u64,
    bound_check_event: ?u64,
    active: bool,
    text_update_count: u64,
    value_update_count: u64,
    checked_update_count: u64,
    disabled_update_count: u64,

    fn init(id: u64, tag: []const u8) DomElement {
        return .{
            .id = id,
            .tag = tag,
            .role = null,
            .label = null,
            .test_id = null,
            .text = null,
            .value = null,
            .checked = false,
            .disabled = false,
            .parent_id = null,
            .children = .empty,
            .bound_click_event = null,
            .bound_input_event = null,
            .bound_check_event = null,
            .active = true,
            .text_update_count = 0,
            .value_update_count = 0,
            .checked_update_count = 0,
            .disabled_update_count = 0,
        };
    }

    fn deinit(self: *DomElement, allocator: std.mem.Allocator) void {
        allocator.free(self.tag);
        if (self.role) |role| allocator.free(role);
        if (self.label) |label| allocator.free(label);
        if (self.test_id) |test_id| allocator.free(test_id);
        if (self.text) |text| allocator.free(text);
        if (self.value) |value| allocator.free(value);
        self.children.deinit(allocator);
    }
};

const SpecCommandType = enum {
    click,
    fill,
    check,
    uncheck,
    expect_text,
    expect_visible,
    expect_value,
    expect_checked,
    expect_disabled,
    expect_updates,
    mark_metrics,
    expect_metric_delta,
};

const LocatorKind = enum {
    none,
    role_name,
    label,
    text,
    test_id,
};

const Locator = struct {
    kind: LocatorKind,
    role: ?[]const u8 = null,
    name: ?[]const u8 = null,
    label: ?[]const u8 = null,
    text: ?[]const u8 = null,
    test_id: ?[]const u8 = null,

    fn deinit(self: Locator, allocator: std.mem.Allocator) void {
        if (self.role) |value| allocator.free(value);
        if (self.name) |value| allocator.free(value);
        if (self.label) |value| allocator.free(value);
        if (self.text) |value| allocator.free(value);
        if (self.test_id) |value| allocator.free(value);
    }
};

fn emptyLocator() Locator {
    return .{ .kind = .none };
}

const SpecCommand = struct {
    cmd_type: SpecCommandType,
    locator: Locator,
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_bool: ?bool,
    line_num: usize,
};

fn freeSpecCommands(allocator: std.mem.Allocator, commands: []SpecCommand) void {
    for (commands) |cmd| {
        cmd.locator.deinit(allocator);
        if (cmd.expected_text) |text| allocator.free(text);
    }
    if (commands.len > 0) {
        allocator.free(commands);
    }
}

const TestState = struct {
    verbose: bool,
    commands: []SpecCommand,
    metrics_mark: ?RuntimeMetrics,

    fn init() TestState {
        return .{
            .verbose = false,
            .commands = &.{},
            .metrics_mark = null,
        };
    }
};

const ParseError = error{
    InvalidFormat,
    OutOfMemory,
    FileNotFound,
    IoError,
};

fn parseTestSpecFile(allocator: std.mem.Allocator, file_path: []const u8) ParseError![]SpecCommand {
    const io = std.Io.Threaded.global_single_threaded.io();
    const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(1024 * 1024)) catch |err| switch (err) {
        error.FileNotFound => return ParseError.FileNotFound,
        else => return ParseError.IoError,
    };
    defer allocator.free(content);

    return parseTestSpec(allocator, content);
}

const SplitTrailingQuoted = struct {
    head: []const u8,
    quoted: []const u8,
};

fn splitTrailingQuoted(input: []const u8) ParseError!SplitTrailingQuoted {
    const end_quote = std.mem.lastIndexOfScalar(u8, input, '"') orelse return ParseError.InvalidFormat;
    if (end_quote == 0) return ParseError.InvalidFormat;
    const before_end = input[0..end_quote];
    const start_quote = std.mem.lastIndexOfScalar(u8, before_end, '"') orelse return ParseError.InvalidFormat;
    const tail = std.mem.trim(u8, input[end_quote + 1 ..], " \t");
    if (tail.len != 0) return ParseError.InvalidFormat;
    return .{
        .head = std.mem.trim(u8, input[0..start_quote], " \t"),
        .quoted = input[start_quote + 1 .. end_quote],
    };
}

fn splitTrailingToken(input: []const u8) ParseError!struct { head: []const u8, token: []const u8 } {
    const trimmed = std.mem.trim(u8, input, " \t");
    const space_idx = std.mem.lastIndexOfAny(u8, trimmed, " \t") orelse return ParseError.InvalidFormat;
    return .{
        .head = std.mem.trim(u8, trimmed[0..space_idx], " \t"),
        .token = std.mem.trim(u8, trimmed[space_idx + 1 ..], " \t"),
    };
}

fn parseQuotedValue(allocator: std.mem.Allocator, prefix: []const u8, input: []const u8) ParseError!?[]const u8 {
    if (!std.mem.startsWith(u8, input, prefix)) return null;
    const rest = std.mem.trim(u8, input[prefix.len..], " \t");
    if (rest.len < 2 or rest[0] != '"' or rest[rest.len - 1] != '"') return ParseError.InvalidFormat;
    return allocator.dupe(u8, rest[1 .. rest.len - 1]) catch ParseError.OutOfMemory;
}

fn parseLocator(allocator: std.mem.Allocator, input: []const u8) ParseError!Locator {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len == 0) return ParseError.InvalidFormat;

    if (std.mem.startsWith(u8, trimmed, "role:")) {
        const rest = trimmed["role:".len..];
        const space_idx = std.mem.indexOfAny(u8, rest, " \t") orelse return ParseError.InvalidFormat;
        const role = rest[0..space_idx];
        const name_part = std.mem.trim(u8, rest[space_idx + 1 ..], " \t");
        const name = (try parseQuotedValue(allocator, "name:", name_part)) orelse return ParseError.InvalidFormat;
        const role_copy = allocator.dupe(u8, role) catch return ParseError.OutOfMemory;
        return .{
            .kind = .role_name,
            .role = role_copy,
            .name = name,
        };
    }

    if ((try parseQuotedValue(allocator, "label:", trimmed))) |value| return .{ .kind = .label, .label = value };
    if ((try parseQuotedValue(allocator, "text:", trimmed))) |value| return .{ .kind = .text, .text = value };
    if ((try parseQuotedValue(allocator, "test_id:", trimmed))) |value| return .{ .kind = .test_id, .test_id = value };

    return ParseError.InvalidFormat;
}

fn appendSpecCommand(
    commands: *std.ArrayListUnmanaged(SpecCommand),
    allocator: std.mem.Allocator,
    cmd_type: SpecCommandType,
    locator: Locator,
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_bool: ?bool,
    line_num: usize,
) ParseError!void {
    commands.append(allocator, .{
        .cmd_type = cmd_type,
        .locator = locator,
        .expected_text = expected_text,
        .expected_count = expected_count,
        .expected_bool = expected_bool,
        .line_num = line_num,
    }) catch return ParseError.OutOfMemory;
}

fn parseBoolToken(token: []const u8) ParseError!bool {
    if (std.mem.eql(u8, token, "true")) return true;
    if (std.mem.eql(u8, token, "false")) return false;
    return ParseError.InvalidFormat;
}

fn parseTestSpec(allocator: std.mem.Allocator, content: []const u8) ParseError![]SpecCommand {
    var commands: std.ArrayListUnmanaged(SpecCommand) = .empty;
    errdefer commands.deinit(allocator);

    var line_num: usize = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        line_num += 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        if (std.mem.startsWith(u8, trimmed, "click ")) {
            try appendSpecCommand(&commands, allocator, .click, try parseLocator(allocator, trimmed[6..]), null, null, null, line_num);
        } else if (std.mem.eql(u8, trimmed, "mark_metrics")) {
            try appendSpecCommand(&commands, allocator, .mark_metrics, emptyLocator(), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "fill ")) {
            const split = try splitTrailingQuoted(trimmed[5..]);
            const value_copy = allocator.dupe(u8, split.quoted) catch return ParseError.OutOfMemory;
            try appendSpecCommand(&commands, allocator, .fill, try parseLocator(allocator, split.head), value_copy, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "check ")) {
            try appendSpecCommand(&commands, allocator, .check, try parseLocator(allocator, trimmed[6..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "uncheck ")) {
            try appendSpecCommand(&commands, allocator, .uncheck, try parseLocator(allocator, trimmed[8..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_text ")) {
            const split = try splitTrailingQuoted(trimmed[12..]);
            const text_copy = allocator.dupe(u8, split.quoted) catch return ParseError.OutOfMemory;
            try appendSpecCommand(&commands, allocator, .expect_text, try parseLocator(allocator, split.head), text_copy, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_visible ")) {
            try appendSpecCommand(&commands, allocator, .expect_visible, try parseLocator(allocator, trimmed[15..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_value ")) {
            const split = try splitTrailingQuoted(trimmed[13..]);
            const value_copy = allocator.dupe(u8, split.quoted) catch return ParseError.OutOfMemory;
            try appendSpecCommand(&commands, allocator, .expect_value, try parseLocator(allocator, split.head), value_copy, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_checked ")) {
            const split = try splitTrailingToken(trimmed[15..]);
            try appendSpecCommand(&commands, allocator, .expect_checked, try parseLocator(allocator, split.head), null, null, try parseBoolToken(split.token), line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_disabled ")) {
            const split = try splitTrailingToken(trimmed[16..]);
            try appendSpecCommand(&commands, allocator, .expect_disabled, try parseLocator(allocator, split.head), null, null, try parseBoolToken(split.token), line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_updates ")) {
            const split = try splitTrailingToken(trimmed[15..]);
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_updates, try parseLocator(allocator, split.head), null, expected_count, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "expect_metric_delta ")) {
            const split = try splitTrailingToken(trimmed[20..]);
            const metric_name = allocator.dupe(u8, split.head) catch return ParseError.OutOfMemory;
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_metric_delta, emptyLocator(), metric_name, expected_count, null, line_num);
        } else {
            return ParseError.InvalidFormat;
        }
    }

    return commands.toOwnedSlice(allocator) catch ParseError.OutOfMemory;
}

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

const RocAllocation = struct {
    ptr: [*]u8,
    total_size: usize,
    alignment: std.mem.Alignment,
};

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    test_state: TestState,
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,
    event_descriptors: std.ArrayListUnmanaged(HostEventDescriptor) = .empty,
    signal_event_routes: std.ArrayListUnmanaged(HostSignalEventRoute) = .empty,
    signal_descriptors: std.ArrayListUnmanaged(HostSignalDescriptor) = .empty,
    signal_routes: std.ArrayListUnmanaged(HostSignalRoute) = .empty,
    signal_dependents: std.ArrayListUnmanaged(HostSignalDependentsRoute) = .empty,
    signal_cache: std.ArrayListUnmanaged(HostSignalCacheSlot) = .empty,
    states: std.ArrayListUnmanaged(HostState) = .empty,
    scopes: std.ArrayListUnmanaged(HostScope) = .empty,
    node_identities: std.ArrayListUnmanaged(HostNodeIdentity) = .empty,
    render_text_sink_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostRenderTextSink)) = .empty,
    render_bool_sink_routes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(HostRenderBoolSink)) = .empty,
    render_structural_signals: std.ArrayListUnmanaged(bool) = .empty,
    render_metrics: HostRenderMetrics = .{},
    dispatch_metrics: HostDispatchMetrics = .{},
    next_elem_id: u64 = 0,
    roc_host: ?*abi.RocHost = null,
    runtime_box: ?RuntimeBox = null,
    last_runtime_metrics: RuntimeMetrics = zeroRuntimeMetrics(),
    pending_roc_metrics: RuntimeMetrics = zeroRuntimeMetrics(),

    fn init() HostEnv {
        return .{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
    }

    fn eventPayloadKindFromAbi(payload_kind: u64) EventPayloadKind {
        return switch (payload_kind) {
            @intFromEnum(EventPayloadKind.unit) => .unit,
            @intFromEnum(EventPayloadKind.str) => .str,
            @intFromEnum(EventPayloadKind.bool) => .bool,
            else => failHost("Roc event descriptor used an unknown payload kind"),
        };
    }

    fn clearEventDescriptors(self: *HostEnv) void {
        self.event_descriptors.items.len = 0;
    }

    fn setEventDescriptors(self: *HostEnv, descriptors: EventDescriptorList) void {
        if (descriptors.len() < self.event_descriptors.items.len) {
            failHost("Roc event descriptors must not shrink");
        }

        const allocator = self.gpa.allocator();
        for (descriptors.items(), 0..) |desc, index| {
            const expected_event_id: u64 = @intCast(index + 1);
            if (desc.event_id != expected_event_id) {
                failHost("Roc event descriptors must be dense and ordered by event id");
            }
            const payload_kind = HostEnv.eventPayloadKindFromAbi(desc.payload_kind);

            if (index < self.event_descriptors.items.len) {
                const existing = self.event_descriptors.items[index];
                if (existing.event_id != desc.event_id or existing.payload_kind != payload_kind) {
                    failHost("Roc event descriptor changed after registration");
                }
                continue;
            }

            self.event_descriptors.append(allocator, .{
                .event_id = desc.event_id,
                .payload_kind = payload_kind,
            }) catch std.process.exit(1);
        }
    }

    fn clearSignalEventRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_event_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.signal_event_routes.items.len = 0;
    }

    fn rebuildSignalEventRoutesFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), self.event_descriptors.items.len) catch std.process.exit(1);
        defer allocator.free(route_lists);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.signal_descriptors.items) |signal| {
            if (signal.kind != .source) continue;
            for (signal.source_event_ids) |event_id| {
                if (event_id == 0 or event_id > self.event_descriptors.items.len) {
                    failHost("host source signal registry contains an unknown event id");
                }
                route_lists[@intCast(event_id - 1)].append(allocator, signal.signal_id) catch std.process.exit(1);
            }
        }

        self.clearSignalEventRoutes();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.signal_event_routes.append(allocator, .{
                .event_id = @intCast(index + 1),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn sourceSignalIdsForEvent(self: *HostEnv, event_id: u64) []const u64 {
        if (event_id == 0) failHost("event id 0 is not registered");

        const route_index = event_id - 1;
        if (route_index >= self.signal_event_routes.items.len) failHost("event id has no host source signal route descriptor");

        const route = self.signal_event_routes.items[@intCast(route_index)];
        if (route.event_id != event_id) failHost("host signal event route table is not indexed by event id");
        return route.signal_ids;
    }

    fn eventPayloadKind(self: *HostEnv, event_id: u64) EventPayloadKind {
        if (event_id == 0) failHost("event id 0 is not registered");

        const event_index = event_id - 1;
        if (event_index >= self.event_descriptors.items.len) failHost("event id has no host event descriptor");

        const descriptor = self.event_descriptors.items[@intCast(event_index)];
        if (descriptor.event_id != event_id) failHost("host event descriptor table is not indexed by event id");
        return descriptor.payload_kind;
    }

    fn validateEventPayload(self: *HostEnv, event_id: u64, expected_payload_kind: EventPayloadKind) void {
        _ = self.sourceSignalIdsForEvent(event_id);
        if (self.eventPayloadKind(event_id) != expected_payload_kind) {
            failHost("DOM binding payload kind does not match Roc event descriptor");
        }
    }

    fn signalKindFromAbi(kind: u64) SignalKind {
        return switch (kind) {
            @intFromEnum(SignalKind.source) => .source,
            @intFromEnum(SignalKind.map) => .map,
            @intFromEnum(SignalKind.map2) => .map2,
            else => failHost("Roc signal descriptor used an unknown signal kind"),
        };
    }

    fn renderTextFieldFromAbi(field: u64) RenderTextField {
        return switch (field) {
            @intFromEnum(RenderTextField.text) => .text,
            @intFromEnum(RenderTextField.role) => .role,
            @intFromEnum(RenderTextField.label) => .label,
            @intFromEnum(RenderTextField.test_id) => .test_id,
            @intFromEnum(RenderTextField.value) => .value,
            else => failHost("Roc render text descriptor used an unknown field"),
        };
    }

    fn renderBoolFieldFromAbi(field: u64) RenderBoolField {
        return switch (field) {
            @intFromEnum(RenderBoolField.checked) => .checked,
            @intFromEnum(RenderBoolField.disabled) => .disabled,
            else => failHost("Roc render bool descriptor used an unknown field"),
        };
    }

    fn renderEventKindFromAbi(kind: u64) RenderEventKind {
        return switch (kind) {
            @intFromEnum(RenderEventKind.click) => .click,
            @intFromEnum(RenderEventKind.input) => .input,
            @intFromEnum(RenderEventKind.check) => .check,
            else => failHost("Roc render event descriptor used an unknown event kind"),
        };
    }

    fn validateSignalSourceStateIds(self: *HostEnv, kind: SignalKind, source_state_ids: []const u64) void {
        switch (kind) {
            .source => {
                if (source_state_ids.len != 1) {
                    failHost("Roc source signal descriptor must have exactly one source state id");
                }
            },
            .map, .map2 => {
                if (source_state_ids.len != 0) {
                    failHost("Roc derived signal descriptor must not have source state ids");
                }
            },
        }

        for (source_state_ids, 0..) |state_id, index| {
            if (state_id >= self.states.items.len) {
                failHost("Roc signal descriptor referenced an unknown source state id");
            }

            for (source_state_ids[0..index]) |previous| {
                if (previous == state_id) {
                    failHost("Roc signal descriptor must not contain duplicate source state ids");
                }
            }
        }
    }

    fn validateSignalInputIds(self: *HostEnv, signal_id: u64, kind: SignalKind, input_signal_ids: []const u64) void {
        _ = self;
        switch (kind) {
            .source => {
                if (input_signal_ids.len != 0) {
                    failHost("Roc source signal descriptor must not have input signal ids");
                }
            },
            .map => {
                if (input_signal_ids.len != 1) {
                    failHost("Roc map signal descriptor must have exactly one input signal id");
                }
            },
            .map2 => {
                if (input_signal_ids.len != 2) {
                    failHost("Roc map2 signal descriptor must have exactly two input signal ids");
                }
            },
        }

        for (input_signal_ids) |input_signal_id| {
            if (input_signal_id >= signal_id) {
                failHost("Roc signal descriptor input ids must reference prior signal ids");
            }
        }
    }

    fn clearSignalDescriptors(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_descriptors.items) |descriptor| {
            allocator.free(descriptor.source_state_ids);
            allocator.free(descriptor.source_event_ids);
            allocator.free(descriptor.input_signal_ids);
        }
        self.signal_descriptors.items.len = 0;
    }

    fn clearSignalRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.signal_routes.items.len = 0;
    }

    fn clearSignalDependents(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.signal_dependents.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.signal_dependents.items.len = 0;
    }

    fn clearSignalCache(self: *HostEnv) void {
        for (self.signal_cache.items) |slot| {
            switch (slot) {
                .absent => {},
                .present => |value| abi.decrefNodeValue(value, self.roc_host.?),
            }
        }
        self.signal_cache.items.len = 0;
    }

    fn clearRenderSinkRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.render_text_sink_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.render_text_sink_routes.items.len = 0;

        for (self.render_bool_sink_routes.items) |*route| {
            route.deinit(allocator);
        }
        self.render_bool_sink_routes.items.len = 0;

        self.render_structural_signals.items.len = 0;
    }

    fn resetRenderSinkRoutes(self: *HostEnv) void {
        self.clearRenderSinkRoutes();

        const allocator = self.gpa.allocator();
        const signal_count = self.signal_descriptors.items.len;
        self.render_text_sink_routes.ensureTotalCapacity(allocator, signal_count) catch std.process.exit(1);
        self.render_bool_sink_routes.ensureTotalCapacity(allocator, signal_count) catch std.process.exit(1);
        self.render_structural_signals.ensureTotalCapacity(allocator, signal_count) catch std.process.exit(1);

        var index: usize = 0;
        while (index < signal_count) : (index += 1) {
            self.render_text_sink_routes.appendAssumeCapacity(.empty);
            self.render_bool_sink_routes.appendAssumeCapacity(.empty);
            self.render_structural_signals.appendAssumeCapacity(false);
        }
    }

    fn validateRenderSinkSignalId(self: *HostEnv, signal_id: u64) usize {
        if (signal_id >= self.signal_descriptors.items.len) {
            failHost("Roc render sink descriptor referenced an unknown signal id");
        }
        const signal_index: usize = @intCast(signal_id);
        const signal = self.signal_descriptors.items[signal_index];
        if (signal.signal_id != signal_id) {
            failHost("host signal registry is not indexed by signal id");
        }
        return signal_index;
    }

    fn appendRenderTextSink(self: *HostEnv, signal_id: u64, elem_id: u64, field: RenderTextField) void {
        const signal_index = self.validateRenderSinkSignalId(signal_id);
        if (signal_index >= self.render_text_sink_routes.items.len) {
            failHost("host render text sink routes are not indexed by signal id");
        }
        self.render_text_sink_routes.items[signal_index].append(self.gpa.allocator(), .{
            .elem_id = elem_id,
            .field = field,
        }) catch std.process.exit(1);
    }

    fn appendRenderBoolSink(self: *HostEnv, signal_id: u64, elem_id: u64, field: RenderBoolField) void {
        const signal_index = self.validateRenderSinkSignalId(signal_id);
        if (signal_index >= self.render_bool_sink_routes.items.len) {
            failHost("host render bool sink routes are not indexed by signal id");
        }
        self.render_bool_sink_routes.items[signal_index].append(self.gpa.allocator(), .{
            .elem_id = elem_id,
            .field = field,
        }) catch std.process.exit(1);
    }

    fn markRenderStructuralSignal(self: *HostEnv, signal_id: u64) void {
        const signal_index = self.validateRenderSinkSignalId(signal_id);
        if (signal_index >= self.render_structural_signals.items.len) {
            failHost("host render structural signal table is not indexed by signal id");
        }
        self.render_structural_signals.items[signal_index] = true;
    }

    fn signalChangesRequireStructuralRender(self: *HostEnv, changes: SignalValueList) bool {
        for (changes.items()) |change| {
            if (change.signal_id >= self.render_structural_signals.items.len) continue;
            if (self.render_structural_signals.items[@intCast(change.signal_id)]) return true;
        }
        return false;
    }

    fn rebuildSignalRoutesFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        var route_lists = allocator.alloc(std.ArrayListUnmanaged(u64), self.states.items.len) catch std.process.exit(1);
        defer allocator.free(route_lists);

        for (route_lists) |*list| {
            list.* = .empty;
        }
        errdefer {
            for (route_lists) |*list| {
                list.deinit(allocator);
            }
        }

        for (self.signal_descriptors.items) |signal| {
            if (signal.kind != .source) continue;
            for (signal.source_state_ids) |state_id| {
                if (state_id >= self.states.items.len) {
                    failHost("host signal registry contains an unknown state id");
                }
                route_lists[@intCast(state_id)].append(allocator, signal.signal_id) catch std.process.exit(1);
            }
        }

        self.clearSignalRoutes();

        for (route_lists, 0..) |*route_list, index| {
            const signal_ids = route_list.toOwnedSlice(allocator) catch std.process.exit(1);
            self.signal_routes.append(allocator, .{
                .state_id = @intCast(index),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn validateSignalSourceEventIds(self: *HostEnv, kind: SignalKind, source_event_ids: []const u64) void {
        switch (kind) {
            .source => {
                if (source_event_ids.len == 0) {
                    failHost("Roc source signal descriptor must have at least one source event id");
                }
            },
            .map, .map2 => {
                if (source_event_ids.len != 0) {
                    failHost("Roc derived signal descriptor must not have source event ids");
                }
            },
        }

        for (source_event_ids, 0..) |event_id, index| {
            if (event_id == 0 or event_id > self.event_descriptors.items.len) {
                failHost("Roc signal descriptor referenced an unknown source event id");
            }

            for (source_event_ids[0..index]) |previous| {
                if (previous == event_id) {
                    failHost("Roc signal descriptor must not contain duplicate source event ids");
                }
            }
        }
    }

    fn rebuildSignalTopologyFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        const signal_count = self.signal_descriptors.items.len;
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

        for (self.signal_descriptors.items, 0..) |*signal, index| {
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
            self.signal_dependents.append(allocator, .{
                .signal_id = @intCast(index),
                .signal_ids = signal_ids,
            }) catch {
                allocator.free(signal_ids);
                std.process.exit(1);
            };
        }
    }

    fn setSignalDescriptors(self: *HostEnv, descriptors: SignalDescriptorList) void {
        if (descriptors.len() < self.signal_descriptors.items.len) {
            failHost("Roc signal descriptors must not shrink");
        }

        const allocator = self.gpa.allocator();
        for (descriptors.items(), 0..) |desc, index| {
            const expected_signal_id: u64 = @intCast(index);
            if (desc.signal_id != expected_signal_id) {
                failHost("Roc signal descriptors must be dense and ordered by signal id");
            }
            const kind = HostEnv.signalKindFromAbi(desc.kind);
            self.validateSignalSourceStateIds(kind, desc.source_state_ids.items());
            self.validateSignalSourceEventIds(kind, desc.source_event_ids.items());
            self.validateSignalInputIds(desc.signal_id, kind, desc.input_signal_ids.items());

            if (index < self.signal_descriptors.items.len) {
                if (index >= self.signal_cache.items.len) {
                    failHost("host signal cache is not indexed by signal id");
                }
                const existing = self.signal_descriptors.items[index];
                if (existing.signal_id != desc.signal_id or existing.kind != kind) {
                    failHost("Roc signal descriptor changed after registration");
                }
                if (existing.source_state_ids.len != desc.source_state_ids.len()) {
                    failHost("Roc signal descriptor source state ids changed after registration");
                }
                if (existing.source_event_ids.len != desc.source_event_ids.len()) {
                    failHost("Roc signal descriptor source event ids changed after registration");
                }
                if (existing.input_signal_ids.len != desc.input_signal_ids.len()) {
                    failHost("Roc signal descriptor input signal ids changed after registration");
                }
                for (desc.source_state_ids.items(), existing.source_state_ids) |next_state_id, existing_state_id| {
                    if (next_state_id != existing_state_id) {
                        failHost("Roc signal descriptor source state ids changed after registration");
                    }
                }
                for (desc.source_event_ids.items(), existing.source_event_ids) |next_event_id, existing_event_id| {
                    if (next_event_id != existing_event_id) {
                        failHost("Roc signal descriptor source event ids changed after registration");
                    }
                }
                for (desc.input_signal_ids.items(), existing.input_signal_ids) |next_input_signal_id, existing_input_signal_id| {
                    if (next_input_signal_id != existing_input_signal_id) {
                        failHost("Roc signal descriptor input signal ids changed after registration");
                    }
                }
                continue;
            }

            const source_state_ids = allocator.dupe(u64, desc.source_state_ids.items()) catch std.process.exit(1);
            const source_event_ids = allocator.dupe(u64, desc.source_event_ids.items()) catch {
                allocator.free(source_state_ids);
                std.process.exit(1);
            };
            const input_signal_ids = allocator.dupe(u64, desc.input_signal_ids.items()) catch {
                allocator.free(source_state_ids);
                allocator.free(source_event_ids);
                std.process.exit(1);
            };
            self.signal_descriptors.append(allocator, .{
                .signal_id = desc.signal_id,
                .kind = kind,
                .source_state_ids = source_state_ids,
                .source_event_ids = source_event_ids,
                .input_signal_ids = input_signal_ids,
                .rank = 0,
            }) catch {
                allocator.free(source_state_ids);
                allocator.free(source_event_ids);
                allocator.free(input_signal_ids);
                std.process.exit(1);
            };
            self.signal_cache.append(allocator, .absent) catch std.process.exit(1);
        }

        self.rebuildSignalRoutesFromSignals();
        self.rebuildSignalTopologyFromSignals();
        self.rebuildSignalEventRoutesFromSignals();
    }

    fn signalIdsForState(self: *HostEnv, state_id: u64) []const u64 {
        if (state_id >= self.signal_routes.items.len) failHost("state id has no host signal route descriptor");

        const route = self.signal_routes.items[@intCast(state_id)];
        if (route.state_id != state_id) failHost("host signal route table is not indexed by state id");
        return route.signal_ids;
    }

    fn dependentSignalIdsForSignal(self: *HostEnv, signal_id: u64) []const u64 {
        if (signal_id >= self.signal_dependents.items.len) failHost("signal id has no host dependent route descriptor");

        const route = self.signal_dependents.items[@intCast(signal_id)];
        if (route.signal_id != signal_id) failHost("host signal dependent route table is not indexed by signal id");
        return route.signal_ids;
    }

    fn appendSignalAndDependents(self: *HostEnv, allocator: std.mem.Allocator, signal_ids: *std.ArrayListUnmanaged(u64), signal_id: u64) void {
        if (!u64SliceContains(signal_ids.items, signal_id)) {
            signal_ids.append(allocator, signal_id) catch std.process.exit(1);
        }

        var index: usize = 0;
        while (index < signal_ids.items.len) : (index += 1) {
            const current_signal_id = signal_ids.items[index];
            for (self.dependentSignalIdsForSignal(current_signal_id)) |dependent_signal_id| {
                if (!u64SliceContains(signal_ids.items, dependent_signal_id)) {
                    signal_ids.append(allocator, dependent_signal_id) catch std.process.exit(1);
                }
            }
        }
    }

    fn signalRank(self: *HostEnv, signal_id: u64) u64 {
        if (signal_id >= self.signal_descriptors.items.len) failHost("signal id has no host signal descriptor");

        const descriptor = self.signal_descriptors.items[@intCast(signal_id)];
        if (descriptor.signal_id != signal_id) failHost("host signal descriptor table is not indexed by signal id");
        return descriptor.rank;
    }

    fn sortSignalIdsByRank(self: *HostEnv, signal_ids: []u64) void {
        var index: usize = 1;
        while (index < signal_ids.len) : (index += 1) {
            const value = signal_ids[index];
            const value_rank = self.signalRank(value);
            var insert_index = index;
            while (insert_index > 0) {
                const previous = signal_ids[insert_index - 1];
                const previous_rank = self.signalRank(previous);
                if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
                signal_ids[insert_index] = previous;
                insert_index -= 1;
            }
            signal_ids[insert_index] = value;
        }
    }

    fn dirtySignalIdsForEvent(self: *HostEnv, allocator: std.mem.Allocator, event_id: u64) []u64 {
        var dirty_signal_ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer dirty_signal_ids.deinit(allocator);

        for (self.sourceSignalIdsForEvent(event_id)) |signal_id| {
            self.appendSignalAndDependents(allocator, &dirty_signal_ids, signal_id);
        }

        const signal_ids = dirty_signal_ids.toOwnedSlice(allocator) catch std.process.exit(1);
        self.sortSignalIdsByRank(signal_ids);
        return signal_ids;
    }

    fn recordDispatch(self: *HostEnv) void {
        self.dispatch_metrics.events_processed += 1;
        self.dispatch_metrics.recompute_batches += 1;
    }

    fn clearStates(self: *HostEnv) void {
        for (self.states.items) |state| {
            abi.decrefNodeValue(state.value, self.roc_host.?);
        }
        self.states.items.len = 0;
    }

    fn setStateDescriptors(self: *HostEnv, descriptors: StateDescriptorList) void {
        if (descriptors.len() < self.states.items.len) {
            failHost("Roc state descriptors must not shrink");
        }

        const allocator = self.gpa.allocator();
        for (descriptors.items(), 0..) |desc, index| {
            const expected_state_id: u64 = @intCast(index);
            if (desc.state_id != expected_state_id) {
                failHost("Roc state descriptors must be dense and ordered by state id");
            }

            if (index < self.states.items.len) {
                const existing = self.states.items[index];
                if (existing.state_id != desc.state_id) {
                    failHost("host state registry is not indexed by state id");
                }
                continue;
            }

            abi.increfNodeValue(desc.value, 1);
            self.states.append(allocator, .{
                .state_id = desc.state_id,
                .value = desc.value,
                .version = 0,
            }) catch {
                abi.decrefNodeValue(desc.value, self.roc_host.?);
                std.process.exit(1);
            };
        }
    }

    fn applyStateChanges(self: *HostEnv, changes: StateChangeList) void {
        if (changes.len() == 0) return;

        const allocator = self.gpa.allocator();
        const seen = allocator.alloc(bool, self.states.items.len) catch std.process.exit(1);
        defer allocator.free(seen);
        @memset(seen, false);

        for (changes.items()) |change| {
            if (change.state_id >= self.states.items.len) {
                failHost("Roc state change descriptor referenced an unknown state id");
            }

            const state_index: usize = @intCast(change.state_id);
            if (seen[state_index]) {
                failHost("Roc state change descriptors must not contain duplicate state ids");
            }
            seen[state_index] = true;

            const state = &self.states.items[state_index];
            if (state.state_id != change.state_id) {
                failHost("host state registry is not indexed by state id");
            }

            abi.increfNodeValue(change.value, 1);
            abi.decrefNodeValue(state.value, self.roc_host.?);
            state.value = change.value;
            state.version += 1;
        }
    }

    fn applySignalChanges(self: *HostEnv, changes: SignalValueList) void {
        if (changes.len() == 0) return;

        const allocator = self.gpa.allocator();
        const seen = allocator.alloc(bool, self.signal_cache.items.len) catch std.process.exit(1);
        defer allocator.free(seen);
        @memset(seen, false);

        for (changes.items()) |change| {
            if (change.signal_id >= self.signal_cache.items.len) {
                failHost("Roc signal change descriptor referenced an unknown signal id");
            }

            const signal_index: usize = @intCast(change.signal_id);
            if (seen[signal_index]) {
                failHost("Roc signal change descriptors must not contain duplicate signal ids");
            }
            seen[signal_index] = true;

            if (self.signal_descriptors.items[signal_index].signal_id != change.signal_id) {
                failHost("host signal registry is not indexed by signal id");
            }

            abi.increfNodeValue(change.value, 1);
            switch (self.signal_cache.items[signal_index]) {
                .absent => {},
                .present => |previous| abi.decrefNodeValue(previous, self.roc_host.?),
            }
            self.signal_cache.items[signal_index] = .{ .present = change.value };
        }
    }

    fn validateScopeId(self: *HostEnv, scope_id: u64) void {
        if (scope_id >= self.scopes.items.len) {
            failHost("scope id has no host scope descriptor");
        }
        const scope = self.scopes.items[@intCast(scope_id)];
        if (scope.scope_id != scope_id) {
            failHost("host scope table is not indexed by scope id");
        }
        if (!scope.active) {
            failHost("scope id references a disposed host scope");
        }
    }

    fn internRootScope(self: *HostEnv) u64 {
        const allocator = self.gpa.allocator();
        if (self.scopes.items.len == 0) {
            self.scopes.append(allocator, .{
                .scope_id = 0,
                .parent_scope_id = null,
                .step = .root,
                .active = true,
            }) catch std.process.exit(1);
            return 0;
        }

        const root = self.scopes.items[0];
        if (root.scope_id != 0 or root.parent_scope_id != null or root.step != .root or !root.active) {
            failHost("host root scope descriptor is not indexed by scope id");
        }
        return 0;
    }

    fn internWhenBranchScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) u64 {
        self.validateScopeId(parent_scope_id);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .when_branch => |step| {
                    if (step.site_ordinal == site_ordinal and step.branch == branch) {
                        return scope.scope_id;
                    }
                },
                .root, .each_row => {},
            }
        }

        const scope_id: u64 = @intCast(self.scopes.items.len);
        self.scopes.append(self.gpa.allocator(), .{
            .scope_id = scope_id,
            .parent_scope_id = parent_scope_id,
            .step = .{ .when_branch = .{ .site_ordinal = site_ordinal, .branch = branch } },
            .active = true,
        }) catch std.process.exit(1);
        return scope_id;
    }

    fn internEachRowScope(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, key: abi.NodeValue, key_eq: abi.RocErasedCallable) u64 {
        self.validateScopeId(parent_scope_id);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .each_row => |step| {
                    if (step.site_ordinal == site_ordinal and callErasedNodeValueNodeValueToBool(roc_host, key_eq, step.key, key)) {
                        return scope.scope_id;
                    }
                },
                .root, .when_branch => {},
            }
        }

        abi.increfNodeValue(key, 1);
        errdefer abi.decrefNodeValue(key, roc_host);

        const scope_id: u64 = @intCast(self.scopes.items.len);
        self.scopes.append(self.gpa.allocator(), .{
            .scope_id = scope_id,
            .parent_scope_id = parent_scope_id,
            .step = .{ .each_row = .{ .site_ordinal = site_ordinal, .key = key } },
            .active = true,
        }) catch std.process.exit(1);
        return scope_id;
    }

    fn internNodeIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        self.validateScopeId(scope_id);

        for (self.node_identities.items) |identity| {
            if (!identity.active) continue;
            if (identity.scope_id == scope_id and identity.ordinal == ordinal) {
                return identity.node_id;
            }
        }

        const node_id: u64 = @intCast(self.node_identities.items.len);
        self.node_identities.append(self.gpa.allocator(), .{
            .node_id = node_id,
            .scope_id = scope_id,
            .ordinal = ordinal,
            .active = true,
        }) catch std.process.exit(1);
        return node_id;
    }

    fn disposeScopeSubtree(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64) void {
        self.validateScopeId(scope_id);

        var child_index: usize = 0;
        while (child_index < self.scopes.items.len) : (child_index += 1) {
            const child = self.scopes.items[child_index];
            if (!child.active) continue;
            if (child.parent_scope_id == scope_id) {
                self.disposeScopeSubtree(roc_host, child.scope_id);
            }
        }

        for (self.node_identities.items) |*identity| {
            if (identity.active and identity.scope_id == scope_id) {
                identity.active = false;
            }
        }

        const scope = &self.scopes.items[@intCast(scope_id)];
        scope.step.deinit(roc_host);
        scope.active = false;
        var metrics = self.pending_roc_metrics;
        metrics.scopes_disposed += 1;
        self.pending_roc_metrics = metrics;
    }

    fn activeEachRowScopes(self: *HostEnv, allocator: std.mem.Allocator, parent_scope_id: u64, site_ordinal: u64) []u64 {
        var ids: std.ArrayListUnmanaged(u64) = .empty;
        errdefer ids.deinit(allocator);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .each_row => |row| {
                    if (row.site_ordinal == site_ordinal) {
                        ids.append(allocator, scope.scope_id) catch std.process.exit(1);
                    }
                },
                .root, .when_branch => {},
            }
        }

        return ids.toOwnedSlice(allocator) catch std.process.exit(1);
    }

    fn eachRowScopeKey(self: *HostEnv, scope_id: u64) abi.NodeValue {
        self.validateScopeId(scope_id);
        const scope = self.scopes.items[@intCast(scope_id)];
        return switch (scope.step) {
            .each_row => |row| row.key,
            .root, .when_branch => failHost("scope id does not reference an each-row scope"),
        };
    }

    fn nextKeyIsDuplicate(roc_host: *abi.RocHost, key_eq: abi.RocErasedCallable, keys: []const abi.NodeValue, key_index: usize) bool {
        const key = keys[key_index];
        for (keys[0..key_index]) |previous| {
            if (callErasedNodeValueNodeValueToBool(roc_host, key_eq, previous, key)) {
                return true;
            }
        }
        return false;
    }

    fn syncEachRowScopes(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const abi.NodeValue, key_eq: abi.RocErasedCallable) HostKeyedRowDiffResult {
        self.validateScopeId(parent_scope_id);

        const allocator = self.gpa.allocator();
        const existing_scope_ids = self.activeEachRowScopes(allocator, parent_scope_id, site_ordinal);
        defer allocator.free(existing_scope_ids);

        const matched_existing = allocator.alloc(bool, existing_scope_ids.len) catch std.process.exit(1);
        defer allocator.free(matched_existing);
        @memset(matched_existing, false);

        var next_scope_ids = allocator.alloc(u64, keys.len) catch std.process.exit(1);
        errdefer allocator.free(next_scope_ids);

        var rows_reused: u64 = 0;
        var rows_created: u64 = 0;

        for (keys, 0..) |key, key_index| {
            if (HostEnv.nextKeyIsDuplicate(roc_host, key_eq, keys, key_index)) {
                failHost("Ui.each keyed scope received duplicate keys");
            }

            var matched_scope_id: ?u64 = null;
            for (existing_scope_ids, 0..) |scope_id, existing_index| {
                if (matched_existing[existing_index]) continue;
                const existing_key = self.eachRowScopeKey(scope_id);
                if (callErasedNodeValueNodeValueToBool(roc_host, key_eq, existing_key, key)) {
                    matched_existing[existing_index] = true;
                    matched_scope_id = scope_id;
                    break;
                }
            }

            if (matched_scope_id) |scope_id| {
                next_scope_ids[key_index] = scope_id;
                rows_reused += 1;
            } else {
                next_scope_ids[key_index] = self.internEachRowScope(roc_host, parent_scope_id, site_ordinal, key, key_eq);
                rows_created += 1;
            }
        }

        var rows_removed: u64 = 0;
        for (existing_scope_ids, 0..) |scope_id, existing_index| {
            if (matched_existing[existing_index]) continue;
            self.disposeScopeSubtree(roc_host, scope_id);
            rows_removed += 1;
        }

        var metrics = self.pending_roc_metrics;
        metrics.rows_reused += rows_reused;
        metrics.rows_created += rows_created;
        metrics.rows_removed += rows_removed;
        self.pending_roc_metrics = metrics;

        return .{
            .scope_ids = next_scope_ids,
            .rows_reused = rows_reused,
            .rows_created = rows_created,
            .rows_removed = rows_removed,
        };
    }

    fn walkNodeElemIdentitySites(self: *HostEnv, elem: abi.NodeElem, scope_id: u64, ordinal: *u64) void {
        self.validateScopeId(scope_id);

        switch (elem.tag) {
            .Element => {
                for (elem.payload.element.children.items()) |child| {
                    self.walkNodeElemIdentitySites(child, scope_id, ordinal);
                }
            },
            .State => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
                self.walkNodeElemIdentitySites(elem.payload.state.child.*, scope_id, ordinal);
            },
            .When => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Each => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Text, .TextSignal => {},
        }
    }

    fn walkNodeElemRootIdentitySites(self: *HostEnv, root: abi.NodeElem) void {
        const root_scope_id = self.internRootScope();
        var ordinal: u64 = 0;
        self.walkNodeElemIdentitySites(root, root_scope_id, &ordinal);
    }

    fn walkNodeElemWhenBranchIdentitySites(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch, elem: abi.NodeElem) u64 {
        const branch_scope_id = self.internWhenBranchScope(parent_scope_id, site_ordinal, branch);
        var ordinal: u64 = 0;
        self.walkNodeElemIdentitySites(elem, branch_scope_id, &ordinal);
        return branch_scope_id;
    }

    fn resolveNodeBinderRef(binder_stack: []const u64, binder_ref: u64) u64 {
        if (binder_ref >= @as(u64, @intCast(binder_stack.len))) {
            failHost("Node.BinderRef referenced a state binder outside the active scope");
        }
        return binder_stack[binder_stack.len - 1 - @as(usize, @intCast(binder_ref))];
    }

    fn collectNodeSignalExprSources(self: *HostEnv, allocator: std.mem.Allocator, expr: abi.NodeSignalExpr, binder_stack: []const u64, source_node_ids: *std.ArrayListUnmanaged(u64)) void {
        switch (expr.tag) {
            .Ref => {
                source_node_ids.append(allocator, HostEnv.resolveNodeBinderRef(binder_stack, expr.payload.ref)) catch std.process.exit(1);
            },
            .ConstValue => {},
            .Map => {
                self.collectNodeSignalExprSources(allocator, expr.payload.map.input.*, binder_stack, source_node_ids);
            },
            .Map2 => {
                self.collectNodeSignalExprSources(allocator, expr.payload.map2.left.*, binder_stack, source_node_ids);
                self.collectNodeSignalExprSources(allocator, expr.payload.map2.right.*, binder_stack, source_node_ids);
            },
            .Combine => {
                for (expr.payload.combine.items()) |child| {
                    self.collectNodeSignalExprSources(allocator, child, binder_stack, source_node_ids);
                }
            },
        }
    }

    fn nodeSignalExprSourceNodeIds(self: *HostEnv, allocator: std.mem.Allocator, expr: abi.NodeSignalExpr, binder_stack: []const u64) []u64 {
        var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
        self.collectNodeSignalExprSources(allocator, expr, binder_stack, &source_node_ids);
        return source_node_ids.toOwnedSlice(allocator) catch std.process.exit(1);
    }

    fn collectNodeAttrDescriptor(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem_id: u64, attr: abi.NodeAttr, binder_stack: []const u64) void {
        const allocator = self.gpa.allocator();
        switch (attr.tag) {
            .StaticText => {
                const payload = attr.payload.static_text;
                const field = HostEnv.renderTextFieldFromAbi(payload.field);
                stream.appendStaticTextAttr(allocator, elem_id, field, payload.value.asSlice());
            },
            .SignalText => {
                const payload = attr.payload.signal_text;
                const field = HostEnv.renderTextFieldFromAbi(payload.field);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, payload.signal.*, binder_stack);
                stream.appendSignalTextAttr(allocator, roc_host, elem_id, field, payload.signal.*, source_node_ids);
            },
            .StaticBool => {
                const payload = attr.payload.static_bool;
                const field = HostEnv.renderBoolFieldFromAbi(payload.field);
                stream.appendStaticBoolAttr(allocator, elem_id, field, payload.value);
            },
            .SignalBool => {
                const payload = attr.payload.signal_bool;
                const field = HostEnv.renderBoolFieldFromAbi(payload.field);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, payload.signal.*, binder_stack);
                stream.appendSignalBoolAttr(allocator, roc_host, elem_id, field, payload.signal.*, source_node_ids);
            },
            .OnEvent => {
                const payload = attr.payload.on_event;
                const msg = payload.msg;
                const kind = HostEnv.renderEventKindFromAbi(payload.kind);
                const payload_kind = HostEnv.eventPayloadKindFromAbi(msg.payload_kind);
                const target_node_id = HostEnv.resolveNodeBinderRef(binder_stack, msg.binder);
                stream.appendEvent(allocator, roc_host, elem_id, kind, msg.binder, target_node_id, payload_kind, msg.transform);
            },
        }
    }

    fn collectNodeElemDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem: abi.NodeElem, scope_id: u64, parent_elem_id: u64, ordinal: *u64, binder_stack: *std.ArrayListUnmanaged(u64)) void {
        self.validateScopeId(scope_id);

        const allocator = self.gpa.allocator();
        switch (elem.tag) {
            .Element => {
                const payload = elem.payload.element;
                const elem_id = stream.appendElement(allocator, parent_elem_id, scope_id, payload.tag.asSlice());
                for (payload.attrs.items()) |attr| {
                    self.collectNodeAttrDescriptor(roc_host, stream, elem_id, attr, binder_stack.items);
                }
                for (payload.children.items()) |child| {
                    self.collectNodeElemDescriptors(roc_host, stream, child, scope_id, elem_id, ordinal, binder_stack);
                }
            },
            .Text => {
                stream.appendTextNode(allocator, parent_elem_id, scope_id, elem.payload.text.asSlice());
            },
            .TextSignal => {
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.text_signal.*, binder_stack.items);
                stream.appendSignalTextNode(allocator, roc_host, parent_elem_id, scope_id, elem.payload.text_signal.*, source_node_ids);
            },
            .State => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                stream.appendState(allocator, roc_host, node_id, elem.payload.state.initial, elem.payload.state.eq);
                binder_stack.append(allocator, node_id) catch std.process.exit(1);
                self.collectNodeElemDescriptors(roc_host, stream, elem.payload.state.child.*, scope_id, parent_elem_id, ordinal, binder_stack);
                _ = binder_stack.pop() orelse unreachable;
            },
            .When => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.when.condition.*, binder_stack.items);
                stream.appendWhen(allocator, roc_host, node_id, elem.payload.when.condition.*, source_node_ids);
            },
            .Each => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                const source_node_ids = self.nodeSignalExprSourceNodeIds(allocator, elem.payload.each.items.*, binder_stack.items);
                stream.appendEach(
                    allocator,
                    roc_host,
                    node_id,
                    elem.payload.each.items.*,
                    source_node_ids,
                    elem.payload.each.key_of,
                    elem.payload.each.key_eq,
                    elem.payload.each.row,
                );
            },
        }
    }

    fn collectNodeElemRootDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.NodeElem) void {
        const root_scope_id = self.internRootScope();
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(u64) = .empty;
        defer binder_stack.deinit(allocator);
        var ordinal: u64 = 0;
        self.collectNodeElemDescriptors(roc_host, stream, root, root_scope_id, 0, &ordinal, &binder_stack);
    }

    fn collectNodeElemWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, branch: HostScopeBranch, elem: abi.NodeElem) u64 {
        if (site.kind != .when) {
            failHost("NodeElem branch collection requires a when scope site");
        }
        const branch_scope_id = self.internWhenBranchScope(site.scope_id, site.ordinal, branch);
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(u64) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_node_ids) catch std.process.exit(1);
        var ordinal: u64 = 0;
        self.collectNodeElemDescriptors(roc_host, stream, elem, branch_scope_id, site.parent_elem_id, &ordinal, &binder_stack);
        return branch_scope_id;
    }

    fn activeWhenBranchScopeId(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) ?u64 {
        self.validateScopeId(parent_scope_id);

        for (self.scopes.items) |scope| {
            if (!scope.active) continue;
            if (scope.parent_scope_id != parent_scope_id) continue;
            switch (scope.step) {
                .when_branch => |step| {
                    if (step.site_ordinal == site_ordinal and step.branch == branch) {
                        return scope.scope_id;
                    }
                },
                .root, .each_row => {},
            }
        }

        return null;
    }

    fn collectNodeElemActiveWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, when: HostNodeWhenDesc, active_branch: HostScopeBranch, elem: abi.NodeElem) u64 {
        if (site.kind != .when) {
            failHost("NodeElem active branch collection requires a when scope site");
        }
        if (site.node_id != when.node_id) {
            failHost("NodeElem active branch collection received mismatched when descriptors");
        }

        if (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite())) |inactive_scope_id| {
            self.disposeScopeSubtree(roc_host, inactive_scope_id);
        }

        return self.collectNodeElemWhenBranchDescriptors(roc_host, stream, site, active_branch, elem);
    }

    fn collectNodeElemEachRowDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, items: []const abi.NodeValue) HostKeyedRowDiffResult {
        if (site.kind != .each) {
            failHost("NodeElem row collection requires an each scope site");
        }
        if (site.node_id != each.node_id) {
            failHost("NodeElem row collection received mismatched each descriptors");
        }

        const allocator = self.gpa.allocator();
        const keys = allocator.alloc(abi.NodeValue, items.len) catch std.process.exit(1);
        var initialized_keys: usize = 0;
        defer {
            for (keys[0..initialized_keys]) |key| {
                abi.decrefNodeValue(key, roc_host);
            }
            allocator.free(keys);
        }

        for (items, 0..) |item, index| {
            keys[index] = callErasedNodeValueToNodeValue(roc_host, each.key_of, item);
            initialized_keys += 1;
        }

        const diff = self.syncEachRowScopes(roc_host, site.scope_id, site.ordinal, keys, each.key_eq);

        var binder_stack: std.ArrayListUnmanaged(u64) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_node_ids) catch std.process.exit(1);

        for (items, keys, diff.scope_ids) |item, key, row_scope_id| {
            const row_elem = callErasedNodeValueNodeValueToNodeElem(roc_host, each.row, key, item);
            defer abi.decrefNodeElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            self.collectNodeElemDescriptors(roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &binder_stack);
        }

        return diff;
    }

    fn clearScopes(self: *HostEnv) void {
        if (self.roc_host) |roc_host| {
            for (self.scopes.items) |*scope| {
                if (!scope.active) continue;
                scope.step.deinit(roc_host);
            }
        } else if (self.scopes.items.len != 0) {
            failHost("host scope table cannot release keys without a Roc host");
        }
        self.scopes.items.len = 0;
    }

    fn metricsWithHostRender(self: *HostEnv, roc_metrics: RuntimeMetrics) RuntimeMetrics {
        var metrics = roc_metrics;
        metrics.patches_emitted += self.render_metrics.patches_emitted;
        metrics.events_processed += self.dispatch_metrics.events_processed;
        metrics.recompute_batches += self.dispatch_metrics.recompute_batches;
        return metrics;
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.gpa.allocator();

        if (self.runtime_box) |runtime| {
            abi.roc_ui_drop(runtime);
            self.runtime_box = null;
        }

        for (self.dom_elements.items) |*elem| {
            elem.deinit(allocator);
        }
        self.dom_elements.deinit(allocator);
        self.clearEventDescriptors();
        self.event_descriptors.deinit(allocator);
        self.clearSignalEventRoutes();
        self.signal_event_routes.deinit(allocator);
        self.clearSignalDescriptors();
        self.signal_descriptors.deinit(allocator);
        self.clearSignalRoutes();
        self.signal_routes.deinit(allocator);
        self.clearSignalDependents();
        self.signal_dependents.deinit(allocator);
        self.clearSignalCache();
        self.signal_cache.deinit(allocator);
        self.clearStates();
        self.states.deinit(allocator);
        self.clearScopes();
        self.scopes.deinit(allocator);
        self.node_identities.deinit(allocator);
        self.clearRenderSinkRoutes();
        self.render_text_sink_routes.deinit(allocator);
        self.render_bool_sink_routes.deinit(allocator);
        self.render_structural_signals.deinit(allocator);

        freeSpecCommands(allocator, self.test_state.commands);

        for (self.roc_allocations.items) |alloc| {
            allocator.rawFree(alloc.ptr[0..alloc.total_size], alloc.alignment, @returnAddress());
        }
        self.roc_allocations.deinit(allocator);
    }

    fn implicitRole(elem: *const DomElement) ?[]const u8 {
        if (elem.role) |role| return role;
        if (std.mem.eql(u8, elem.tag, "button")) return "button";
        if (std.mem.eql(u8, elem.tag, "h1") or
            std.mem.eql(u8, elem.tag, "h2") or
            std.mem.eql(u8, elem.tag, "h3") or
            std.mem.eql(u8, elem.tag, "h4") or
            std.mem.eql(u8, elem.tag, "h5") or
            std.mem.eql(u8, elem.tag, "h6")) return "heading";
        if (std.mem.eql(u8, elem.tag, "section")) return "region";
        return null;
    }

    fn accessibleName(elem: *const DomElement) []const u8 {
        if (elem.label) |label| return label;
        if (elem.text) |text| return text;
        if (elem.value) |value| return value;
        return "";
    }

    fn locatorMatches(self: *HostEnv, elem: *const DomElement, locator: Locator) bool {
        _ = self;
        return switch (locator.kind) {
            .none => false,
            .role_name => blk: {
                const role = HostEnv.implicitRole(elem) orelse break :blk false;
                const expected_role = locator.role orelse break :blk false;
                const expected_name = locator.name orelse break :blk false;
                break :blk std.mem.eql(u8, role, expected_role) and std.mem.eql(u8, HostEnv.accessibleName(elem), expected_name);
            },
            .label => blk: {
                const expected = locator.label orelse break :blk false;
                const label = elem.label orelse break :blk false;
                break :blk std.mem.eql(u8, label, expected);
            },
            .text => blk: {
                const expected = locator.text orelse break :blk false;
                const text = elem.text orelse break :blk false;
                break :blk std.mem.eql(u8, text, expected);
            },
            .test_id => blk: {
                const expected = locator.test_id orelse break :blk false;
                const test_id = elem.test_id orelse break :blk false;
                break :blk std.mem.eql(u8, test_id, expected);
            },
        };
    }

    fn findElementByLocator(self: *HostEnv, locator: Locator, line_num: usize) ?*DomElement {
        var found: ?*DomElement = null;
        var match_count: usize = 0;
        for (self.dom_elements.items) |*elem| {
            if (!elem.active) continue;
            if (!self.locatorMatches(elem, locator)) continue;
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
    return .{
        .closure_releases = 0,
        .closure_retains = 0,
        .derived_calls_into_roc = 0,
        .events_processed = 0,
        .nodes_recomputed = 0,
        .patches_emitted = 0,
        .propagation_prunes = 0,
        .recompute_batches = 0,
        .retained_alloc_delta = 0,
        .rows_created = 0,
        .rows_removed = 0,
        .rows_reused = 0,
        .scopes_created = 0,
        .scopes_disposed = 0,
    };
}

var current_host: ?*HostEnv = null;
var current_roc_host: ?*abi.RocHost = null;

fn hostFromRocHost(roc_host: *abi.RocHost) *HostEnv {
    return @ptrCast(@alignCast(roc_host.env));
}

fn currentRocHost() *abi.RocHost {
    return current_roc_host orelse @panic("signals RocHost is not initialized");
}

fn rocAllocFn(roc_host: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const total_size = length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);
    host.alloc_count += 1;

    return @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn rocDeallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.dealloc_count += 1;

    allocator.rawFree(base_ptr[0..total_size], align_enum, @returnAddress());
}

fn rocReallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);
    const old_user_data_size = old_total_size - size_storage_bytes;
    const new_total_size = new_length + size_storage_bytes;

    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const new_user_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
    const old_user_ptr: [*]const u8 = @ptrCast(ptr);
    const copy_size = @min(old_user_data_size, new_length);
    @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);

    for (host.roc_allocations.items, 0..) |alloc, i| {
        if (alloc.ptr == old_base_ptr) {
            _ = host.roc_allocations.swapRemove(i);
            break;
        }
    }
    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = new_ptr,
        .total_size = new_total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);

    allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    return new_user_ptr;
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

fn failHost(message: []const u8) noreturn {
    writeStderr("HOST ERROR: ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn replaceOwnedString(allocator: std.mem.Allocator, field: *?[]const u8, value: []const u8) bool {
    if (field.*) |existing| {
        if (std.mem.eql(u8, existing, value)) return false;
        allocator.free(existing);
    }
    field.* = allocator.dupe(u8, value) catch std.process.exit(1);
    return true;
}

const CommandCounts = struct {
    total: u64 = 0,
    reset_dom: u64 = 0,
    create_element: u64 = 0,
    append_child: u64 = 0,
    set_text: u64 = 0,
    set_value: u64 = 0,
    set_checked: u64 = 0,
    set_disabled: u64 = 0,
    set_metadata: u64 = 0,
    bind_event: u64 = 0,

    fn addHostReset(self: *CommandCounts) void {
        self.total += 1;
        self.reset_dom += 1;
    }

    fn addCreateElement(self: *CommandCounts) void {
        self.total += 1;
        self.create_element += 1;
    }

    fn addAppendChild(self: *CommandCounts) void {
        self.total += 1;
        self.append_child += 1;
    }

    fn addTextField(self: *CommandCounts, field: RenderTextField) void {
        self.total += 1;
        switch (field) {
            .text => self.set_text += 1,
            .value => self.set_value += 1,
            .role, .label, .test_id => self.set_metadata += 1,
        }
    }

    fn addBoolField(self: *CommandCounts, field: RenderBoolField) void {
        self.total += 1;
        switch (field) {
            .checked => self.set_checked += 1,
            .disabled => self.set_disabled += 1,
        }
    }

    fn addEventBinding(self: *CommandCounts) void {
        self.total += 1;
        self.bind_event += 1;
    }

    fn addAll(self: *CommandCounts, other: CommandCounts) void {
        self.total += other.total;
        self.reset_dom += other.reset_dom;
        self.create_element += other.create_element;
        self.append_child += other.append_child;
        self.set_text += other.set_text;
        self.set_value += other.set_value;
        self.set_checked += other.set_checked;
        self.set_disabled += other.set_disabled;
        self.set_metadata += other.set_metadata;
        self.bind_event += other.bind_event;
    }
};

const BenchmarkStats = struct {
    init_roc_ns: u64 = 0,
    init_apply_ns: u64 = 0,
    dispatch_roc_ns: u64 = 0,
    dispatch_apply_ns: u64 = 0,
    actions: u64 = 0,
    allocs: u64 = 0,
    deallocs: u64 = 0,
    retained_alloc_delta: i64 = 0,
    commands: CommandCounts = .{},
    metrics: RuntimeMetrics = zeroRuntimeMetrics(),
};

fn addRuntimeMetrics(left: RuntimeMetrics, right: RuntimeMetrics) RuntimeMetrics {
    return .{
        .closure_releases = left.closure_releases + right.closure_releases,
        .closure_retains = left.closure_retains + right.closure_retains,
        .derived_calls_into_roc = left.derived_calls_into_roc + right.derived_calls_into_roc,
        .events_processed = left.events_processed + right.events_processed,
        .nodes_recomputed = left.nodes_recomputed + right.nodes_recomputed,
        .patches_emitted = left.patches_emitted + right.patches_emitted,
        .propagation_prunes = left.propagation_prunes + right.propagation_prunes,
        .recompute_batches = left.recompute_batches + right.recompute_batches,
        .retained_alloc_delta = left.retained_alloc_delta + right.retained_alloc_delta,
        .rows_created = left.rows_created + right.rows_created,
        .rows_removed = left.rows_removed + right.rows_removed,
        .rows_reused = left.rows_reused + right.rows_reused,
        .scopes_created = left.scopes_created + right.scopes_created,
        .scopes_disposed = left.scopes_disposed + right.scopes_disposed,
    };
}

fn u64MetricAsI64(value: u64) i64 {
    return std.math.cast(i64, value) orelse failHost("runtime metric exceeded signed assertion range");
}

fn runtimeMetricValue(metrics: RuntimeMetrics, name: []const u8) ?i64 {
    if (std.mem.eql(u8, name, "events_processed")) return u64MetricAsI64(metrics.events_processed);
    if (std.mem.eql(u8, name, "nodes_recomputed")) return u64MetricAsI64(metrics.nodes_recomputed);
    if (std.mem.eql(u8, name, "propagation_prunes")) return u64MetricAsI64(metrics.propagation_prunes);
    if (std.mem.eql(u8, name, "derived_calls_into_roc")) return u64MetricAsI64(metrics.derived_calls_into_roc);
    if (std.mem.eql(u8, name, "recompute_batches")) return u64MetricAsI64(metrics.recompute_batches);
    if (std.mem.eql(u8, name, "patches_emitted")) return u64MetricAsI64(metrics.patches_emitted);
    if (std.mem.eql(u8, name, "scopes_created")) return u64MetricAsI64(metrics.scopes_created);
    if (std.mem.eql(u8, name, "scopes_disposed")) return u64MetricAsI64(metrics.scopes_disposed);
    if (std.mem.eql(u8, name, "rows_reused")) return u64MetricAsI64(metrics.rows_reused);
    if (std.mem.eql(u8, name, "rows_created")) return u64MetricAsI64(metrics.rows_created);
    if (std.mem.eql(u8, name, "rows_removed")) return u64MetricAsI64(metrics.rows_removed);
    if (std.mem.eql(u8, name, "closure_retains")) return u64MetricAsI64(metrics.closure_retains);
    if (std.mem.eql(u8, name, "closure_releases")) return u64MetricAsI64(metrics.closure_releases);
    if (std.mem.eql(u8, name, "retained_alloc_delta")) return metrics.retained_alloc_delta;
    return null;
}

fn setElementTextIfChanged(host: *HostEnv, elem: *DomElement, text: []const u8) bool {
    if (replaceOwnedString(host.gpa.allocator(), &elem.text, text)) {
        elem.text_update_count += 1;
        return true;
    }
    return false;
}

fn setElementValueIfChanged(host: *HostEnv, elem: *DomElement, value: []const u8) bool {
    if (replaceOwnedString(host.gpa.allocator(), &elem.value, value)) {
        elem.value_update_count += 1;
        return true;
    }
    return false;
}

fn setElementCheckedIfChanged(elem: *DomElement, checked: bool) bool {
    if (elem.checked != checked) {
        elem.checked = checked;
        elem.checked_update_count += 1;
        return true;
    }
    return false;
}

fn setElementDisabledIfChanged(elem: *DomElement, disabled: bool) bool {
    if (elem.disabled != disabled) {
        elem.disabled = disabled;
        elem.disabled_update_count += 1;
        return true;
    }
    return false;
}

fn resetSimulatedDom(host: *HostEnv) void {
    const allocator = host.gpa.allocator();
    for (host.dom_elements.items) |*elem| {
        elem.deinit(allocator);
    }
    host.dom_elements.items.len = 0;
    host.next_elem_id = 1;

    const root_tag = allocator.dupe(u8, "root") catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(0, root_tag)) catch {
        allocator.free(root_tag);
        std.process.exit(1);
    };
}

fn domElementById(host: *HostEnv, id: u64) *DomElement {
    if (id >= host.dom_elements.items.len) failHost("DOM command referenced missing element");
    const elem = &host.dom_elements.items[@intCast(id)];
    if (!elem.active) failHost("DOM command referenced inactive element");
    return elem;
}

fn applyRenderElementDesc(host: *HostEnv, desc: RenderElementDesc) void {
    if (desc.elem_id != host.dom_elements.items.len) failHost("Roc render element descriptors must be dense and ordered by element id");

    const allocator = host.gpa.allocator();
    const tag_copy = allocator.dupe(u8, desc.tag.asSlice()) catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(desc.elem_id, tag_copy)) catch {
        allocator.free(tag_copy);
        std.process.exit(1);
    };
    host.next_elem_id = desc.elem_id + 1;

    const parent = domElementById(host, desc.parent_id);
    const child = domElementById(host, desc.elem_id);
    child.parent_id = parent.id;
    parent.children.append(allocator, child.id) catch std.process.exit(1);
}

fn applyRenderTextField(host: *HostEnv, elem_id: u64, field: RenderTextField, value: []const u8) bool {
    const elem = domElementById(host, elem_id);
    return switch (field) {
        .text => setElementTextIfChanged(host, elem, value),
        .role => replaceOwnedString(host.gpa.allocator(), &elem.role, value),
        .label => replaceOwnedString(host.gpa.allocator(), &elem.label, value),
        .test_id => replaceOwnedString(host.gpa.allocator(), &elem.test_id, value),
        .value => setElementValueIfChanged(host, elem, value),
    };
}

fn applyRenderBoolField(host: *HostEnv, elem_id: u64, field: RenderBoolField, value: bool) bool {
    const elem = domElementById(host, elem_id);
    return switch (field) {
        .checked => setElementCheckedIfChanged(elem, value),
        .disabled => setElementDisabledIfChanged(elem, value),
    };
}

fn validateRenderSinkSignal(host: *HostEnv, signal_id: u64) void {
    _ = host.validateRenderSinkSignalId(signal_id);
}

fn applyRenderEventDesc(host: *HostEnv, desc: RenderEventDesc) void {
    const elem = domElementById(host, desc.elem_id);
    const kind = HostEnv.renderEventKindFromAbi(desc.event_kind);
    switch (kind) {
        .click => {
            host.validateEventPayload(desc.event_id, .unit);
            elem.bound_click_event = desc.event_id;
        },
        .input => {
            host.validateEventPayload(desc.event_id, .str);
            elem.bound_input_event = desc.event_id;
        },
        .check => {
            host.validateEventPayload(desc.event_id, .bool);
            elem.bound_check_event = desc.event_id;
        },
    }
}

fn applyRenderStructuralDesc(host: *HostEnv, desc: RenderStructuralDesc) void {
    _ = domElementById(host, desc.elem_id);
    host.markRenderStructuralSignal(desc.signal_id);
}

fn nodeValueText(value: abi.NodeValue) []const u8 {
    return switch (value.tag) {
        .NvStr => value.payload.nv_str.asSlice(),
        else => failHost("render text sink expected a Str signal value"),
    };
}

fn nodeValueBoolValue(value: abi.NodeValue) bool {
    return switch (value.tag) {
        .NvBool => value.payload.nv_bool,
        else => failHost("render bool sink expected a Bool signal value"),
    };
}

fn applySignalRenderPatches(host: *HostEnv, changes: SignalValueList) CommandCounts {
    var counts: CommandCounts = .{};

    for (changes.items()) |change| {
        if (change.signal_id >= host.signal_descriptors.items.len) {
            failHost("Roc signal change descriptor referenced an unknown signal id");
        }
        const signal_index: usize = @intCast(change.signal_id);

        if (signal_index < host.render_text_sink_routes.items.len and host.render_text_sink_routes.items[signal_index].items.len > 0) {
            const value = nodeValueText(change.value);
            for (host.render_text_sink_routes.items[signal_index].items) |sink| {
                if (applyRenderTextField(host, sink.elem_id, sink.field, value)) {
                    counts.addTextField(sink.field);
                }
            }
        }

        if (signal_index < host.render_bool_sink_routes.items.len and host.render_bool_sink_routes.items[signal_index].items.len > 0) {
            const value = nodeValueBoolValue(change.value);
            for (host.render_bool_sink_routes.items[signal_index].items) |sink| {
                if (applyRenderBoolField(host, sink.elem_id, sink.field, value)) {
                    counts.addBoolField(sink.field);
                }
            }
        }
    }

    host.render_metrics.patches_emitted += counts.total;

    return counts;
}

fn traceHostDomReset() void {
    if (!traceEnabled()) return;
    writeStderr("[HOST CMD] ResetDom\n");
}

fn applyFullRenderDescriptors(host: *HostEnv, result: DispatchResult) CommandCounts {
    var counts: CommandCounts = .{};
    counts.addHostReset();
    traceHostDomReset();
    resetSimulatedDom(host);
    host.resetRenderSinkRoutes();

    for (result.render_elements.items()) |desc| {
        applyRenderElementDesc(host, desc);
        counts.addCreateElement();
        counts.addAppendChild();
    }

    for (result.render_texts.items()) |desc| {
        const field = HostEnv.renderTextFieldFromAbi(desc.field);
        if (applyRenderTextField(host, desc.elem_id, field, desc.value.asSlice())) {
            counts.addTextField(field);
        }
    }

    for (result.render_signal_texts.items()) |desc| {
        validateRenderSinkSignal(host, desc.signal_id);
        const field = HostEnv.renderTextFieldFromAbi(desc.field);
        host.appendRenderTextSink(desc.signal_id, desc.elem_id, field);
        if (applyRenderTextField(host, desc.elem_id, field, desc.value.asSlice())) {
            counts.addTextField(field);
        }
    }

    for (result.render_bools.items()) |desc| {
        const field = HostEnv.renderBoolFieldFromAbi(desc.field);
        if (applyRenderBoolField(host, desc.elem_id, field, desc.value)) {
            counts.addBoolField(field);
        }
    }

    for (result.render_signal_bools.items()) |desc| {
        validateRenderSinkSignal(host, desc.signal_id);
        const field = HostEnv.renderBoolFieldFromAbi(desc.field);
        host.appendRenderBoolSink(desc.signal_id, desc.elem_id, field);
        if (applyRenderBoolField(host, desc.elem_id, field, desc.value)) {
            counts.addBoolField(field);
        }
    }

    for (result.render_events.items()) |desc| {
        applyRenderEventDesc(host, desc);
        counts.addEventBinding();
    }

    for (result.render_structures.items()) |desc| {
        applyRenderStructuralDesc(host, desc);
    }

    host.render_metrics.patches_emitted += counts.total;

    return counts;
}

fn releaseRenderElementList(elements: RenderElementDescList, roc_host: *abi.RocHost) void {
    if (elements.isUnique()) {
        for (elements.items()) |desc| {
            desc.tag.decref(roc_host);
        }
    }
    elements.decref(roc_host);
}

fn releaseRenderTextList(texts: RenderTextDescList, roc_host: *abi.RocHost) void {
    if (texts.isUnique()) {
        for (texts.items()) |desc| {
            desc.value.decref(roc_host);
        }
    }
    texts.decref(roc_host);
}

fn releaseRenderSignalTextList(texts: RenderSignalTextDescList, roc_host: *abi.RocHost) void {
    if (texts.isUnique()) {
        for (texts.items()) |desc| {
            desc.value.decref(roc_host);
        }
    }
    texts.decref(roc_host);
}

fn releaseRenderBoolList(bools: RenderBoolDescList, roc_host: *abi.RocHost) void {
    bools.decref(roc_host);
}

fn releaseRenderSignalBoolList(bools: RenderSignalBoolDescList, roc_host: *abi.RocHost) void {
    bools.decref(roc_host);
}

fn releaseRenderEventList(events: RenderEventDescList, roc_host: *abi.RocHost) void {
    events.decref(roc_host);
}

fn releaseRenderStructuralList(structures: RenderStructuralDescList, roc_host: *abi.RocHost) void {
    structures.decref(roc_host);
}

fn releaseDispatchRenderDescriptors(result: DispatchResult, roc_host: *abi.RocHost) void {
    releaseRenderElementList(result.render_elements, roc_host);
    releaseRenderTextList(result.render_texts, roc_host);
    releaseRenderSignalTextList(result.render_signal_texts, roc_host);
    releaseRenderBoolList(result.render_bools, roc_host);
    releaseRenderSignalBoolList(result.render_signal_bools, roc_host);
    releaseRenderEventList(result.render_events, roc_host);
    releaseRenderStructuralList(result.render_structures, roc_host);
}

fn releaseEventDescriptorList(descriptors: EventDescriptorList, roc_host: *abi.RocHost) void {
    descriptors.decref(roc_host);
}

fn releaseSignalDescriptorList(descriptors: SignalDescriptorList, roc_host: *abi.RocHost) void {
    if (descriptors.isUnique()) {
        for (descriptors.items()) |descriptor| {
            descriptor.input_signal_ids.decref(roc_host);
            descriptor.source_event_ids.decref(roc_host);
            descriptor.source_state_ids.decref(roc_host);
        }
    }
    descriptors.decref(roc_host);
}

fn releaseSignalValueList(values: SignalValueList, roc_host: *abi.RocHost) void {
    if (values.isUnique()) {
        for (values.items()) |value| {
            abi.decrefNodeValue(value.value, roc_host);
        }
    }
    values.decref(roc_host);
}

fn releaseStateDescriptorList(descriptors: StateDescriptorList, roc_host: *abi.RocHost) void {
    if (descriptors.isUnique()) {
        for (descriptors.items()) |descriptor| {
            abi.decrefNodeValue(descriptor.value, roc_host);
        }
    }
    descriptors.decref(roc_host);
}

fn releaseStateChangeList(changes: StateChangeList, roc_host: *abi.RocHost) void {
    if (changes.isUnique()) {
        for (changes.items()) |change| {
            abi.decrefNodeValue(change.value, roc_host);
        }
    }
    changes.decref(roc_host);
}

fn dropMovedDispatchResultPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

fn dropMovedRecomputeResultPayload(_: ?*anyopaque, _: *abi.RocHost) callconv(.c) void {}

fn acceptRecomputeResultMeasured(host: *HostEnv, roc_host: *abi.RocHost, result_box: RecomputeResultBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) RecomputeApplyOutcome {
    const result = result_box.*;
    if (host.runtime_box != null) failHost("Roc runtime box was overwritten before being consumed");
    host.runtime_box = result.runtime;
    host.pending_roc_metrics = addRuntimeMetrics(host.pending_roc_metrics, result.metrics);
    const start_ns = benchmarkNowNs();
    host.setEventDescriptors(result.event_descriptors);
    host.setStateDescriptors(result.state_descriptors);
    host.setSignalDescriptors(result.signal_descriptors);
    const structural_render_required = host.signalChangesRequireStructuralRender(result.signal_changes);
    host.applyStateChanges(result.state_changes);
    host.applySignalChanges(result.signal_changes);
    if (!structural_render_required) {
        const counts = applySignalRenderPatches(host, result.signal_changes);
        if (command_counts) |total| total.addAll(counts);
        host.last_runtime_metrics = host.metricsWithHostRender(host.pending_roc_metrics);
        host.pending_roc_metrics = zeroRuntimeMetrics();
    }
    const elapsed = benchmarkNowNs() - start_ns;
    if (apply_ns) |ns| ns.* += elapsed;
    releaseEventDescriptorList(result.event_descriptors, roc_host);
    releaseSignalValueList(result.signal_changes, roc_host);
    releaseSignalDescriptorList(result.signal_descriptors, roc_host);
    releaseStateDescriptorList(result.state_descriptors, roc_host);
    releaseStateChangeList(result.state_changes, roc_host);
    abi.decrefBoxWith(@ptrCast(result_box), @alignOf(RecomputeResult), &dropMovedRecomputeResultPayload, roc_host);
    return .{ .structural_render_required = structural_render_required };
}

fn acceptRecomputeResult(host: *HostEnv, roc_host: *abi.RocHost, result_box: RecomputeResultBox) RecomputeApplyOutcome {
    return acceptRecomputeResultMeasured(host, roc_host, result_box, null, null);
}

fn acceptRenderResultMeasured(host: *HostEnv, roc_host: *abi.RocHost, result_box: DispatchResultBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    const result = result_box.*;
    if (host.runtime_box != null) failHost("Roc runtime box was overwritten before being consumed");
    host.runtime_box = result.runtime;
    const roc_metrics = addRuntimeMetrics(host.pending_roc_metrics, result.metrics);
    host.pending_roc_metrics = zeroRuntimeMetrics();
    host.setEventDescriptors(result.event_descriptors);
    host.setStateDescriptors(result.state_descriptors);
    host.setSignalDescriptors(result.signal_descriptors);
    host.applyStateChanges(result.state_changes);
    host.applySignalChanges(result.signal_changes);
    const start_ns = benchmarkNowNs();
    const counts = applyFullRenderDescriptors(host, result);
    const elapsed = benchmarkNowNs() - start_ns;
    host.last_runtime_metrics = host.metricsWithHostRender(roc_metrics);
    if (apply_ns) |ns| ns.* += elapsed;
    if (command_counts) |total| total.addAll(counts);
    releaseDispatchRenderDescriptors(result, roc_host);
    releaseEventDescriptorList(result.event_descriptors, roc_host);
    releaseSignalValueList(result.signal_changes, roc_host);
    releaseSignalDescriptorList(result.signal_descriptors, roc_host);
    releaseStateDescriptorList(result.state_descriptors, roc_host);
    releaseStateChangeList(result.state_changes, roc_host);
    abi.decrefBoxWith(@ptrCast(result_box), @alignOf(DispatchResult), &dropMovedDispatchResultPayload, roc_host);
}

fn acceptRenderResult(host: *HostEnv, roc_host: *abi.RocHost, result_box: DispatchResultBox) void {
    acceptRenderResultMeasured(host, roc_host, result_box, null, null);
}

fn acceptInitResultMeasured(host: *HostEnv, roc_host: *abi.RocHost, result_box: DispatchResultBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    acceptRenderResultMeasured(host, roc_host, result_box, apply_ns, command_counts);
}

fn acceptInitResult(host: *HostEnv, roc_host: *abi.RocHost, result_box: DispatchResultBox) void {
    acceptRenderResult(host, roc_host, result_box);
}

fn boxRenderInput(roc_host: *abi.RocHost, input: RenderInput) RenderInputBox {
    const raw = abi.allocateBox(@sizeOf(RenderInput), @alignOf(RenderInput), true, roc_host);
    const input_box: RenderInputBox = @ptrCast(@alignCast(raw));
    input_box.* = input;
    return input_box;
}

fn boxRecomputeInput(roc_host: *abi.RocHost, input: RecomputeInput) RecomputeInputBox {
    const raw = abi.allocateBox(@sizeOf(RecomputeInput), @alignOf(RecomputeInput), true, roc_host);
    const input_box: RecomputeInputBox = @ptrCast(@alignCast(raw));
    input_box.* = input;
    return input_box;
}

fn cachedSignalValueListExcluding(host: *HostEnv, roc_host: *abi.RocHost, dirty_signal_ids: []const u64) SignalValueList {
    const allocator = host.gpa.allocator();
    var cached_signals: std.ArrayListUnmanaged(SignalValueDesc) = .empty;
    defer cached_signals.deinit(allocator);

    for (host.signal_cache.items, 0..) |slot, signal_index| {
        if (u64SliceContains(dirty_signal_ids, @intCast(signal_index))) continue;
        switch (slot) {
            .absent => {},
            .present => |value| {
                cached_signals.append(allocator, .{
                    .signal_id = @intCast(signal_index),
                    .value = value,
                }) catch std.process.exit(1);
            },
        }
    }

    const list = SignalValueList.fromSlice(cached_signals.items, roc_host);
    for (cached_signals.items) |entry| {
        abi.increfNodeValue(entry.value, 1);
    }
    return list;
}

fn cachedStateValueList(host: *HostEnv, roc_host: *abi.RocHost) StateChangeList {
    const allocator = host.gpa.allocator();
    var cached_states: std.ArrayListUnmanaged(StateValueDesc) = .empty;
    defer cached_states.deinit(allocator);

    for (host.states.items, 0..) |state, state_index| {
        if (state.state_id != state_index) {
            failHost("host state registry is not indexed by state id");
        }
        cached_states.append(allocator, .{
            .state_id = @intCast(state_index),
            .value = state.value,
        }) catch std.process.exit(1);
    }

    const list = StateChangeList.fromSlice(cached_states.items, roc_host);
    for (cached_states.items) |entry| {
        abi.increfNodeValue(entry.value, 1);
    }
    return list;
}

fn renderInputForHost(host: *HostEnv, roc_host: *abi.RocHost) RenderInput {
    return .{
        .cached_signals = cachedSignalValueListExcluding(host, roc_host, &.{}),
        .cached_states = cachedStateValueList(host, roc_host),
    };
}

fn signalEventPayloadForEvent(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64) SignalEventPayload {
    const allocator = host.gpa.allocator();
    const dirty_signal_ids = host.dirtySignalIdsForEvent(allocator, event_id);
    defer allocator.free(dirty_signal_ids);
    return .{
        .dirty_signal_ids = abi.RocListWith(u64, false).fromSlice(dirty_signal_ids, roc_host),
        .cached_signals = cachedSignalValueListExcluding(host, roc_host, &.{}),
        .cached_states = cachedStateValueList(host, roc_host),
    };
}

fn nodeValueUnit() abi.NodeValue {
    return .{
        .payload = .{ .nv_unit = .{} },
        .tag = .NvUnit,
    };
}

fn nodeValueStr(roc_host: *abi.RocHost, value: []const u8) abi.NodeValue {
    return .{
        .payload = .{ .nv_str = RocStr.fromSlice(value, roc_host) },
        .tag = .NvStr,
    };
}

fn nodeValueBool(value: bool) abi.NodeValue {
    return .{
        .payload = .{ .nv_bool = value },
        .tag = .NvBool,
    };
}

fn nodeValueI64(value: i64) abi.NodeValue {
    return .{
        .payload = .{ .nv_i64 = value },
        .tag = .NvI64,
    };
}

const ErasedNodeValueUnaryArgs = extern struct {
    arg0: abi.NodeValue,
};

const ErasedNodeValueBinaryArgs = extern struct {
    arg0: abi.NodeValue,
    arg1: abi.NodeValue,
};

fn erasedCallablePayload(callable: abi.RocErasedCallable) *abi.RocErasedCallablePayload {
    if (callable == null) failHost("host attempted to call a null Roc erased callable");
    return abi.rocErasedCallablePayloadPtr(callable);
}

fn callErasedNodeValueToNodeValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue) abi.NodeValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedNodeValueUnaryArgs{ .arg0 = arg0 };
    var result: abi.NodeValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedNodeValueNodeValueToNodeValue(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue, arg1: abi.NodeValue) abi.NodeValue {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedNodeValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: abi.NodeValue = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedNodeValueNodeValueToNodeElem(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue, arg1: abi.NodeValue) abi.NodeElem {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedNodeValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: abi.NodeElem = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn callErasedNodeValueNodeValueToBool(roc_host: *abi.RocHost, callable: abi.RocErasedCallable, arg0: abi.NodeValue, arg1: abi.NodeValue) bool {
    const payload = erasedCallablePayload(callable);
    var call_args = ErasedNodeValueBinaryArgs{ .arg0 = arg0, .arg1 = arg1 };
    var result: bool = undefined;
    payload.callable_fn_ptr(
        roc_host,
        @ptrCast(&result),
        @ptrCast(&call_args),
        abi.rocErasedCallableCapturePtr(callable),
    );
    return result;
}

fn activeOccurrence(event_id: u64, value: abi.NodeValue) ActiveEvent {
    return .{
        .payload = .{ .occurrence = .{ .id = event_id, .value = value } },
        .tag = .Occurrence,
    };
}

fn recomputeInputForEvent(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, value: abi.NodeValue) RecomputeInput {
    const signal_payload = signalEventPayloadForEvent(host, roc_host, event_id);
    return .{
        .active_event = activeOccurrence(event_id, value),
        .cached_signals = signal_payload.cached_signals,
        .cached_states = signal_payload.cached_states,
        .dirty_signal_ids = signal_payload.dirty_signal_ids,
    };
}

fn clickRecomputeInput(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64) RecomputeInput {
    return recomputeInputForEvent(host, roc_host, event_id, nodeValueUnit());
}

fn inputRecomputeInput(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, value: []const u8) RecomputeInput {
    return recomputeInputForEvent(host, roc_host, event_id, nodeValueStr(roc_host, value));
}

fn checkRecomputeInput(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, checked: bool) RecomputeInput {
    return recomputeInputForEvent(host, roc_host, event_id, nodeValueBool(checked));
}

fn dispatchRocEvent(host: *HostEnv, roc_host: *abi.RocHost, input: RecomputeInput) void {
    const runtime = host.runtime_box orelse failHost("Roc runtime not initialized");
    host.runtime_box = null;
    host.recordDispatch();
    const outcome = acceptRecomputeResult(host, roc_host, abi.roc_ui_recompute(runtime, boxRecomputeInput(roc_host, input)));
    if (!outcome.structural_render_required) return;

    const render_runtime = host.runtime_box orelse failHost("Roc runtime not initialized after recompute");
    host.runtime_box = null;
    acceptRenderResult(host, roc_host, abi.roc_ui_render(render_runtime, boxRenderInput(roc_host, renderInputForHost(host, roc_host))));
}

fn dispatchRocEventMeasured(host: *HostEnv, roc_host: *abi.RocHost, input: RecomputeInput, stats: *BenchmarkStats) void {
    const runtime = host.runtime_box orelse failHost("Roc runtime not initialized");
    host.runtime_box = null;
    const input_box = boxRecomputeInput(roc_host, input);
    host.recordDispatch();
    const start_ns = benchmarkNowNs();
    const result_box = abi.roc_ui_recompute(runtime, input_box);
    stats.dispatch_roc_ns += benchmarkNowNs() - start_ns;
    const outcome = acceptRecomputeResultMeasured(host, roc_host, result_box, &stats.dispatch_apply_ns, &stats.commands);
    if (!outcome.structural_render_required) {
        stats.actions += 1;
        return;
    }

    const render_runtime = host.runtime_box orelse failHost("Roc runtime not initialized after recompute");
    host.runtime_box = null;
    const render_input_box = boxRenderInput(roc_host, renderInputForHost(host, roc_host));
    const render_start_ns = benchmarkNowNs();
    const render_result_box = abi.roc_ui_render(render_runtime, render_input_box);
    stats.dispatch_roc_ns += benchmarkNowNs() - render_start_ns;
    acceptRenderResultMeasured(host, roc_host, render_result_box, &stats.dispatch_apply_ns, &stats.commands);
    stats.actions += 1;
}

fn makeSignalsRocHost(host: *HostEnv) abi.RocHost {
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

fn commandIsAction(cmd: SpecCommand) bool {
    return switch (cmd.cmd_type) {
        .click, .fill, .check, .uncheck => true,
        else => false,
    };
}

fn runActionCommandMeasured(host: *HostEnv, roc_host: *abi.RocHost, cmd: SpecCommand, stats: *BenchmarkStats) void {
    switch (cmd.cmd_type) {
        .click => {
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark click locator did not resolve");
            if (elem.disabled) failHost("benchmark click target is disabled");
            const event_id = elem.bound_click_event orelse failHost("benchmark click target has no binding");
            dispatchRocEventMeasured(host, roc_host, clickRecomputeInput(host, roc_host, event_id), stats);
        },

        .fill => {
            const value = cmd.expected_text orelse "";
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark fill locator did not resolve");
            if (elem.disabled) failHost("benchmark fill target is disabled");
            if (elem.bound_input_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, inputRecomputeInput(host, roc_host, event_id, value), stats);
            } else {
                _ = setElementValueIfChanged(host, elem, value);
            }
        },

        .check, .uncheck => {
            const checked = cmd.cmd_type == .check;
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark check locator did not resolve");
            if (elem.disabled) failHost("benchmark check target is disabled");
            if (elem.bound_check_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, checkRecomputeInput(host, roc_host, event_id, checked), stats);
            } else {
                _ = setElementCheckedIfChanged(elem, checked);
            }
        },

        else => {},
    }
}

fn runBenchmarkIteration(commands: []const SpecCommand, verbose: bool, stats: *BenchmarkStats) void {
    var host_env = HostEnv.init();
    host_env.test_state.verbose = verbose;

    var roc_host = makeSignalsRocHost(&host_env);
    host_env.roc_host = &roc_host;
    current_host = &host_env;
    current_roc_host = &roc_host;

    const init_start_ns = benchmarkNowNs();
    const init_result = abi.roc_ui_init();
    stats.init_roc_ns += benchmarkNowNs() - init_start_ns;
    acceptInitResultMeasured(&host_env, &roc_host, init_result, &stats.init_apply_ns, &stats.commands);

    for (commands) |cmd| {
        if (commandIsAction(cmd)) {
            runActionCommandMeasured(&host_env, &roc_host, cmd, stats);
        }
    }

    const retained_delta = @as(i64, @intCast(host_env.alloc_count)) - @as(i64, @intCast(host_env.dealloc_count));
    var iteration_metrics = host_env.last_runtime_metrics;
    iteration_metrics.retained_alloc_delta = retained_delta;
    stats.metrics = addRuntimeMetrics(stats.metrics, iteration_metrics);
    stats.allocs += @intCast(host_env.alloc_count);
    stats.deallocs += @intCast(host_env.dealloc_count);
    stats.retained_alloc_delta += retained_delta;

    host_env.deinit();
    current_host = null;
    current_roc_host = null;
}

fn printBenchmarkHeader() void {
    writeStdout("case,sample,iterations,actions,init_roc_ns,init_apply_ns,dispatch_roc_ns,dispatch_apply_ns,total_ns,allocs,deallocs,retained_alloc_delta,commands,reset_dom,create_element,append_child,set_text,set_value,set_checked,set_disabled,set_metadata,bind_event,events_processed,nodes_recomputed,propagation_prunes,derived_calls_into_roc,recompute_batches,patches_emitted,scopes_created,scopes_disposed,rows_reused,rows_created,rows_removed,closure_retains,closure_releases,metrics_retained_alloc_delta\n");
}

fn printBenchmarkRow(case_name: []const u8, sample: usize, iterations: usize, stats: BenchmarkStats) void {
    const total_ns = stats.init_roc_ns + stats.init_apply_ns + stats.dispatch_roc_ns + stats.dispatch_apply_ns;
    printStdout(
        "{s},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            case_name,
            sample,
            iterations,
            stats.actions,
            stats.init_roc_ns,
            stats.init_apply_ns,
            stats.dispatch_roc_ns,
            stats.dispatch_apply_ns,
            total_ns,
            stats.allocs,
            stats.deallocs,
            stats.retained_alloc_delta,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            stats.commands.total,
            stats.commands.reset_dom,
            stats.commands.create_element,
            stats.commands.append_child,
            stats.commands.set_text,
            stats.commands.set_value,
            stats.commands.set_checked,
            stats.commands.set_disabled,
            stats.commands.set_metadata,
            stats.commands.bind_event,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            stats.metrics.events_processed,
            stats.metrics.nodes_recomputed,
            stats.metrics.propagation_prunes,
            stats.metrics.derived_calls_into_roc,
            stats.metrics.recompute_batches,
            stats.metrics.patches_emitted,
            stats.metrics.scopes_created,
            stats.metrics.scopes_disposed,
            stats.metrics.rows_reused,
            stats.metrics.rows_created,
            stats.metrics.rows_removed,
            stats.metrics.closure_retains,
            stats.metrics.closure_releases,
            stats.metrics.retained_alloc_delta,
        },
    );
}

fn runAppBenchmarks(spec_file: []const u8, case_name: []const u8, iterations: usize, samples: usize, verbose: bool) !c_int {
    var bench_gpa = std.heap.DebugAllocator(.{ .safety = true }){};
    defer _ = bench_gpa.deinit();
    const allocator = bench_gpa.allocator();
    const commands = parseTestSpecFile(allocator, spec_file) catch |err| {
        switch (err) {
            ParseError.FileNotFound => writeStderr("Error: Test spec file not found\n"),
            ParseError.InvalidFormat => writeStderr("Error: Invalid test spec format\n"),
            else => writeStderr("Error: Failed to parse test spec\n"),
        }
        return 1;
    };
    defer freeSpecCommands(allocator, commands);

    printBenchmarkHeader();
    for (0..samples) |sample| {
        var stats: BenchmarkStats = .{};
        for (0..iterations) |_| {
            runBenchmarkIteration(commands, verbose, &stats);
        }
        printBenchmarkRow(case_name, sample, iterations, stats);
    }

    return 0;
}

comptime {
    if (!builtin.is_test) {
        @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
        @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
        @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
        @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
        @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
        @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });

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

fn platform_main(spec_file: []const u8, verbose: bool) !c_int {
    _ = base.signal_handler.installForCurrentThread(.{
        .stack_overflow = handleRocStackOverflow,
        .access_violation = handleRocAccessViolation,
        .arithmetic_error = handleRocArithmeticError,
    });

    var host_env = HostEnv.init();
    const allocator = host_env.gpa.allocator();

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
    host_env.roc_host = &roc_host;
    current_host = &host_env;
    current_roc_host = &roc_host;
    defer current_host = null;
    defer current_roc_host = null;
    defer host_env.deinit();

    traceStderr("[HOST] calling roc_ui_init\n");
    acceptInitResult(&host_env, &roc_host, abi.roc_ui_init());
    traceStderr("[HOST] roc_ui_init returned\n");

    if (verbose) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} recomputed nodes\n", .{
            host_env.dom_elements.items.len,
            host_env.last_runtime_metrics.nodes_recomputed,
        }) catch "";
        writeStderr(msg);
        host_env.dumpDom();
    }

    for (host_env.test_state.commands) |cmd| {
        switch (cmd.cmd_type) {
            .mark_metrics => {
                host_env.test_state.metrics_mark = host_env.last_runtime_metrics;
            },

            .click => {
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled) {
                    writeLocatorFailure(cmd.line_num, "target is disabled");
                    return 1;
                }
                const event_id = elem.bound_click_event orelse {
                    writeLocatorFailure(cmd.line_num, "target has no click binding");
                    return 1;
                };
                dispatchRocEvent(&host_env, &roc_host, clickRecomputeInput(&host_env, &roc_host, event_id));
            },

            .fill => {
                const value = cmd.expected_text orelse "";
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled) {
                    writeLocatorFailure(cmd.line_num, "target is disabled");
                    return 1;
                }
                if (elem.bound_input_event) |event_id| {
                    dispatchRocEvent(&host_env, &roc_host, inputRecomputeInput(&host_env, &roc_host, event_id, value));
                } else {
                    _ = setElementValueIfChanged(&host_env, elem, value);
                }
            },

            .check, .uncheck => {
                const checked = cmd.cmd_type == .check;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled) {
                    writeLocatorFailure(cmd.line_num, "target is disabled");
                    return 1;
                }
                if (elem.bound_check_event) |event_id| {
                    dispatchRocEvent(&host_env, &roc_host, checkRecomputeInput(&host_env, &roc_host, event_id, checked));
                } else {
                    _ = setElementCheckedIfChanged(elem, checked);
                }
            },

            .expect_visible => {
                _ = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one visible element");
                    return 1;
                };
            },

            .expect_text => {
                const expected = cmd.expected_text orelse "";
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                const actual = elem.text orelse "";
                if (!std.mem.eql(u8, actual, expected)) {
                    writeStringMismatch(cmd.line_num, "text", expected, actual);
                    return 1;
                }
            },

            .expect_value => {
                const expected = cmd.expected_text orelse "";
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                const actual = elem.value orelse "";
                if (!std.mem.eql(u8, actual, expected)) {
                    writeStringMismatch(cmd.line_num, "value", expected, actual);
                    return 1;
                }
            },

            .expect_checked => {
                const expected = cmd.expected_bool orelse false;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.checked != expected) {
                    writeBoolMismatch(cmd.line_num, "checked", expected, elem.checked);
                    return 1;
                }
            },

            .expect_disabled => {
                const expected = cmd.expected_bool orelse false;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                if (elem.disabled != expected) {
                    writeBoolMismatch(cmd.line_num, "disabled", expected, elem.disabled);
                    return 1;
                }
            },

            .expect_updates => {
                const expected = cmd.expected_count orelse 0;
                const elem = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                    return 1;
                };
                const actual = elem.text_update_count + elem.value_update_count + elem.checked_update_count + elem.disabled_update_count;
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected updates: {d}\n  Got updates:      {d}\n", .{ cmd.line_num, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_metric_delta => {
                const metric_name = cmd.expected_text orelse "";
                const expected = u64MetricAsI64(cmd.expected_count orelse 0);
                const marked = host_env.test_state.metrics_mark orelse {
                    writeMetricFailure(cmd.line_num, "mark_metrics must run before expect_metric_delta");
                    return 1;
                };
                const start = runtimeMetricValue(marked, metric_name) orelse {
                    writeUnknownMetric(cmd.line_num, metric_name);
                    return 1;
                };
                const current = runtimeMetricValue(host_env.last_runtime_metrics, metric_name) orelse {
                    writeUnknownMetric(cmd.line_num, metric_name);
                    return 1;
                };
                const actual = current - start;
                if (actual != expected) {
                    writeMetricDeltaMismatch(cmd.line_num, metric_name, expected, actual);
                    return 1;
                }
            },
        }
    }

    if (verbose) {
        writeStderr("[PASS] All tests passed\n");
    }

    return 0;
}

fn writeLocatorFailure(line_num: usize, message: []const u8) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: {s}\n", .{ line_num, message }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeStringMismatch(line_num: usize, field: []const u8, expected: []const u8, actual: []const u8) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s}: \"{s}\"\n  Got {s}:      \"{s}\"\n", .{ line_num, field, expected, field, actual }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeBoolMismatch(line_num: usize, field: []const u8, expected: bool, actual: bool) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s}: {}\n  Got {s}:      {}\n", .{ line_num, field, expected, field, actual }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeMetricFailure(line_num: usize, message: []const u8) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: {s}\n", .{ line_num, message }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeUnknownMetric(line_num: usize, metric_name: []const u8) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: unknown metric \"{s}\"\n", .{ line_num, metric_name }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn writeMetricDeltaMismatch(line_num: usize, metric_name: []const u8, expected: i64, actual: i64) void {
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s} delta: {d}\n  Got {s} delta:      {d}\n", .{ line_num, metric_name, expected, metric_name, actual }) catch "TEST FAILED\n";
    writeStderr(msg);
}

fn appendTestSignalDescriptor(host: *HostEnv, signal_id: u64, kind: SignalKind, source_event_ids: []const u64, input_signal_ids: []const u64) !void {
    if (!builtin.is_test) @compileError("appendTestSignalDescriptor is test-only");

    const allocator = host.gpa.allocator();
    const owned_source_states = try allocator.dupe(u64, &.{});
    errdefer allocator.free(owned_source_states);
    const owned_source_events = try allocator.dupe(u64, source_event_ids);
    errdefer allocator.free(owned_source_events);
    const owned_inputs = try allocator.dupe(u64, input_signal_ids);
    errdefer allocator.free(owned_inputs);

    try host.signal_descriptors.append(allocator, .{
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

    const allocator = host.gpa.allocator();
    host.clearEventDescriptors();
    host.event_descriptors.deinit(allocator);
    host.clearSignalEventRoutes();
    host.signal_event_routes.deinit(allocator);
    host.clearSignalDescriptors();
    host.signal_descriptors.deinit(allocator);
    host.clearSignalRoutes();
    host.signal_routes.deinit(allocator);
    host.clearSignalDependents();
    host.signal_dependents.deinit(allocator);
    host.signal_cache.deinit(allocator);
}

fn deinitTestHostIdentity(host: *HostEnv) void {
    if (!builtin.is_test) @compileError("deinitTestHostIdentity is test-only");

    const allocator = host.gpa.allocator();
    host.clearScopes();
    host.scopes.deinit(allocator);
    host.node_identities.deinit(allocator);
    host.roc_allocations.deinit(allocator);
}

test "signals host dirty plan deduplicates diamond join by rank" {
    var host = HostEnv.init();
    defer {
        deinitTestHostGraph(&host);
        _ = host.gpa.deinit();
    }
    const allocator = host.gpa.allocator();

    try host.event_descriptors.append(allocator, .{
        .event_id = 1,
        .payload_kind = .unit,
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
    left.events_processed = 2;
    left.nodes_recomputed = 5;
    left.propagation_prunes = 3;
    left.derived_calls_into_roc = 4;
    left.recompute_batches = 2;
    left.patches_emitted = 7;

    var right = zeroRuntimeMetrics();
    right.events_processed = 1;
    right.nodes_recomputed = 8;
    right.propagation_prunes = 11;
    right.derived_calls_into_roc = 6;
    right.recompute_batches = 1;
    right.patches_emitted = 13;
    right.retained_alloc_delta = -2;

    const total = addRuntimeMetrics(left, right);
    try std.testing.expectEqual(@as(u64, 3), total.events_processed);
    try std.testing.expectEqual(@as(u64, 13), total.nodes_recomputed);
    try std.testing.expectEqual(@as(u64, 14), total.propagation_prunes);
    try std.testing.expectEqual(@as(u64, 10), total.derived_calls_into_roc);
    try std.testing.expectEqual(@as(u64, 3), total.recompute_batches);
    try std.testing.expectEqual(@as(u64, 20), total.patches_emitted);
    try std.testing.expectEqual(@as(i64, -2), total.retained_alloc_delta);
}

const TestErasedI64Capture = extern struct {
    amount: i64,
};

var test_erased_callable_drop_count: u64 = 0;

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

fn testUnaryNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueUnaryArgs, args);
    const input = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test unary NodeValue callable expected NvI64"),
    };
    writeTestErasedResult(abi.NodeValue, ret, nodeValueI64(input + capture.amount));
}

fn testBinaryNodeValueCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const left = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test binary NodeValue callable expected left NvI64"),
    };
    const right = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test binary NodeValue callable expected right NvI64"),
    };
    writeTestErasedResult(abi.NodeValue, ret, nodeValueI64(left + right + capture.amount));
}

fn testBinaryNodeElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const left = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test row NodeElem callable expected left NvI64"),
    };
    const right = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test row NodeElem callable expected right NvI64"),
    };
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}", .{left + right + capture.amount}) catch @panic("test row NodeElem callable could not format text");
    writeTestErasedResult(abi.NodeElem, ret, testNodeText(roc_host, text));
}

fn testStatefulRowNodeElemCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const key = switch (call_args.arg0.tag) {
        .NvI64 => call_args.arg0.payload.nv_i64,
        else => @panic("test stateful row NodeElem callable expected key NvI64"),
    };
    const item = switch (call_args.arg1.tag) {
        .NvI64 => call_args.arg1.payload.nv_i64,
        else => @panic("test stateful row NodeElem callable expected item NvI64"),
    };
    var text_buffer: [64]u8 = undefined;
    const text = std.fmt.bufPrint(&text_buffer, "row-{d}-{d}", .{ key, item + capture.amount }) catch @panic("test stateful row NodeElem callable could not format text");
    writeTestErasedResult(abi.NodeElem, ret, testNodeState(roc_host, testNodeText(roc_host, text)));
}

fn testNodeValueEqCallable(_: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedNodeValueBinaryArgs, args);
    const is_equal =
        call_args.arg0.tag == .NvI64 and
        call_args.arg1.tag == .NvI64 and
        call_args.arg0.payload.nv_i64 == call_args.arg1.payload.nv_i64;
    writeTestErasedResult(bool, ret, is_equal);
}

fn testErasedCallableOnDrop(_: ?[*]u8, _: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
}

fn expectNodeValueI64(value: abi.NodeValue, expected: i64) !void {
    try std.testing.expectEqual(abi.NodeValueTag.NvI64, value.tag);
    try std.testing.expectEqual(expected, value.payload.nv_i64);
}

test "signals host invokes erased NodeValue thunks with ABI argument layouts" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        host.roc_allocations.deinit(host.gpa.allocator());
        _ = host.gpa.deinit();
    }

    {
        const unary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testUnaryNodeValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 5 },
        );
        defer abi.decrefErasedCallable(unary, &roc_host);

        const result = callErasedNodeValueToNodeValue(&roc_host, unary, nodeValueI64(37));
        try expectNodeValueI64(result, 42);
    }

    {
        const binary = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryNodeValueCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(binary, &roc_host);

        const result = callErasedNodeValueNodeValueToNodeValue(&roc_host, binary, nodeValueI64(10), nodeValueI64(29));
        try expectNodeValueI64(result, 42);
    }

    {
        const row = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testBinaryNodeElemCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 3 },
        );
        defer abi.decrefErasedCallable(row, &roc_host);

        const result = callErasedNodeValueNodeValueToNodeElem(&roc_host, row, nodeValueI64(10), nodeValueI64(29));
        defer abi.decrefNodeElem(result, &roc_host);

        try std.testing.expectEqual(abi.NodeElemTag.Text, result.tag);
        try std.testing.expectEqualStrings("row-42", result.payload.text.asSlice());
    }

    {
        const eq = writeTestErasedCallable(
            TestErasedI64Capture,
            &roc_host,
            &testNodeValueEqCallable,
            &testErasedCallableOnDrop,
            .{ .amount = 0 },
        );
        defer abi.decrefErasedCallable(eq, &roc_host);

        try std.testing.expect(callErasedNodeValueNodeValueToBool(&roc_host, eq, nodeValueI64(42), nodeValueI64(42)));
        try std.testing.expect(!callErasedNodeValueNodeValueToBool(&roc_host, eq, nodeValueI64(41), nodeValueI64(42)));
    }

    try std.testing.expectEqual(@as(u64, 4), test_erased_callable_drop_count);
}

test "signals host interns scopes and node identities from explicit paths" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
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

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const row_a = host.internEachRowScope(&roc_host, root, 7, nodeValueI64(10), key_eq);
    try std.testing.expectEqual(row_a, host.internEachRowScope(&roc_host, root, 7, nodeValueI64(10), key_eq));

    const row_b = host.internEachRowScope(&roc_host, root, 7, nodeValueI64(11), key_eq);
    try std.testing.expect(row_b != row_a);

    const same_key_other_site = host.internEachRowScope(&roc_host, root, 8, nodeValueI64(10), key_eq);
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
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();
    const row = host.internEachRowScope(&roc_host, root, 3, nodeValueI64(10), key_eq);
    const branch = host.internWhenBranchScope(row, 4, .true_branch);
    const row_state = host.internNodeIdentity(row, 0);
    const branch_state = host.internNodeIdentity(branch, 0);

    host.disposeScopeSubtree(&roc_host, row);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(!host.scopes.items[@intCast(row)].active);
    try std.testing.expect(!host.scopes.items[@intCast(branch)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(row_state)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(branch_state)].active);

    const recreated_row = host.internEachRowScope(&roc_host, root, 3, nodeValueI64(10), key_eq);
    try std.testing.expect(recreated_row != row);

    const recreated_state = host.internNodeIdentity(recreated_row, 0);
    try std.testing.expect(recreated_state != row_state);
}

fn freeKeyedRowDiff(host: *HostEnv, diff: HostKeyedRowDiffResult) void {
    host.gpa.allocator().free(diff.scope_ids);
}

fn boxTestNodeElem(roc_host: *abi.RocHost, elem: abi.NodeElem) *abi.NodeElem {
    const raw = abi.allocateBox(@sizeOf(abi.NodeElem), @alignOf(abi.NodeElem), true, roc_host);
    const boxed: *abi.NodeElem = @ptrCast(@alignCast(raw));
    boxed.* = elem;
    return boxed;
}

fn boxTestNodeSignalExpr(roc_host: *abi.RocHost, expr: abi.NodeSignalExpr) *abi.NodeSignalExpr {
    const raw = abi.allocateBox(@sizeOf(abi.NodeSignalExpr), @alignOf(abi.NodeSignalExpr), true, roc_host);
    const boxed: *abi.NodeSignalExpr = @ptrCast(@alignCast(raw));
    boxed.* = expr;
    return boxed;
}

fn testNodeConstExpr(value: abi.NodeValue) abi.NodeSignalExpr {
    return .{
        .payload = .{ .const_value = value },
        .tag = .ConstValue,
    };
}

fn testNodeRefExpr(binder_ref: u64) abi.NodeSignalExpr {
    return .{
        .payload = .{ .ref = binder_ref },
        .tag = .Ref,
    };
}

fn testNodeText(roc_host: *abi.RocHost, text: []const u8) abi.NodeElem {
    return .{
        .payload = .{ .text = RocStr.fromSlice(text, roc_host) },
        .tag = .Text,
    };
}

fn testNodeTextSignal(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr) abi.NodeElem {
    return .{
        .payload = .{ .text_signal = boxTestNodeSignalExpr(roc_host, signal) },
        .tag = .TextSignal,
    };
}

fn testNodeStaticTextAttr(roc_host: *abi.RocHost, field: RenderTextField, value: []const u8) abi.NodeAttr {
    return .{
        .payload = .{
            .static_text = .{
                .field = @intFromEnum(field),
                .value = RocStr.fromSlice(value, roc_host),
            },
        },
        .tag = .StaticText,
    };
}

fn testNodeSignalTextAttr(roc_host: *abi.RocHost, field: RenderTextField, signal: abi.NodeSignalExpr) abi.NodeAttr {
    return .{
        .payload = .{
            .signal_text = .{
                .field = @intFromEnum(field),
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
    return .{
        .payload = .{
            .signal_bool = .{
                .field = @intFromEnum(field),
                .signal = boxTestNodeSignalExpr(roc_host, signal),
            },
        },
        .tag = .SignalBool,
    };
}

fn testNodeEventAttr(roc_host: *abi.RocHost, kind: RenderEventKind, binder_ref: u64, payload_kind: EventPayloadKind) abi.NodeAttr {
    const transform = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testBinaryNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = binder_ref,
                    .payload_kind = @intFromEnum(payload_kind),
                    .transform = transform,
                },
            },
        },
        .tag = .OnEvent,
    };
}

fn testNodeElementWith(roc_host: *abi.RocHost, tag: []const u8, attrs: []const abi.NodeAttr, children: []const abi.NodeElem) abi.NodeElem {
    return .{
        .payload = .{
            .element = .{
                .attrs = abi.RocList(abi.NodeAttr).fromSlice(attrs, roc_host),
                .children = abi.RocList(abi.NodeElem).fromSlice(children, roc_host),
                .tag = RocStr.fromSlice(tag, roc_host),
            },
        },
        .tag = .Element,
    };
}

fn testNodeElement(roc_host: *abi.RocHost, children: []const abi.NodeElem) abi.NodeElem {
    return testNodeElementWith(roc_host, "div", &.{}, children);
}

fn testNodeState(roc_host: *abi.RocHost, child: abi.NodeElem) abi.NodeElem {
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .state = .{
                .child = boxTestNodeElem(roc_host, child),
                .eq = eq,
                .initial = nodeValueI64(0),
            },
        },
        .tag = .State,
    };
}

fn testNodeWhen(roc_host: *abi.RocHost, when_true: abi.NodeElem, when_false: abi.NodeElem) abi.NodeElem {
    return .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(nodeValueBool(true))),
                .when_false = boxTestNodeElem(roc_host, when_false),
                .when_true = boxTestNodeElem(roc_host, when_true),
            },
        },
        .tag = .When,
    };
}

fn testNodeEach(roc_host: *abi.RocHost) abi.NodeElem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryNodeValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const row = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testStatefulRowNodeElemCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    return .{
        .payload = .{
            .each = .{
                .items = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(.{
                    .payload = .{ .nv_list = abi.RocList(abi.NodeValue).empty() },
                    .tag = .NvList,
                })),
                .key_eq = key_eq,
                .key_of = key_of,
                .row = row,
            },
        },
        .tag = .Each,
    };
}

test "signals host keyed row diff reuses creates and removes by typed key" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNodeValueEqCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();

    const initial_keys = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(3) };
    const initial = host.syncEachRowScopes(&roc_host, root, 5, &initial_keys, key_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    const reordered_keys = [_]abi.NodeValue{ nodeValueI64(3), nodeValueI64(1), nodeValueI64(2) };
    const reordered = host.syncEachRowScopes(&roc_host, root, 5, &reordered_keys, key_eq);
    defer freeKeyedRowDiff(&host, reordered);
    try std.testing.expectEqual(@as(u64, 3), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);
    try std.testing.expectEqual(initial.scope_ids[2], reordered.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[0], reordered.scope_ids[1]);
    try std.testing.expectEqual(initial.scope_ids[1], reordered.scope_ids[2]);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(reordered.scope_ids[2], 0));

    const changed_keys = [_]abi.NodeValue{ nodeValueI64(2), nodeValueI64(4) };
    const changed = host.syncEachRowScopes(&roc_host, root, 5, &changed_keys, key_eq);
    defer freeKeyedRowDiff(&host, changed);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);

    const reappeared_keys = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(4) };
    const reappeared = host.syncEachRowScopes(&roc_host, root, 5, &reappeared_keys, key_eq);
    defer freeKeyedRowDiff(&host, reappeared);
    try std.testing.expectEqual(@as(u64, 2), reappeared.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), reappeared.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reappeared.rows_removed);
    try std.testing.expect(reappeared.scope_ids[0] != initial.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[1], reappeared.scope_ids[1]);
    try std.testing.expectEqual(changed.scope_ids[1], reappeared.scope_ids[2]);

    try std.testing.expectEqual(@as(u64, 6), host.pending_roc_metrics.rows_reused);
    try std.testing.expectEqual(@as(u64, 5), host.pending_roc_metrics.rows_created);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.rows_removed);
}

test "signals host walks NodeElem identity-bearing sites only" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const nested_state = testNodeState(&roc_host, testNodeText(&roc_host, "nested"));
    const branch_true = testNodeState(&roc_host, testNodeText(&roc_host, "true"));
    const branch_false = testNodeState(&roc_host, testNodeText(&roc_host, "false"));
    const when_elem = testNodeWhen(&roc_host, branch_true, branch_false);
    const each_elem = testNodeEach(&roc_host);
    const nested_children = [_]abi.NodeElem{
        testNodeText(&roc_host, "ordinary text"),
        when_elem,
        each_elem,
    };
    const nested_element = testNodeElement(&roc_host, &nested_children);
    const root_children = [_]abi.NodeElem{
        testNodeText(&roc_host, "root text"),
        nested_state,
        nested_element,
    };
    const root = testNodeElement(&roc_host, &root_children);
    defer abi.decrefNodeElem(root, &roc_host);

    host.walkNodeElemRootIdentitySites(root);
    try std.testing.expectEqual(@as(usize, 3), host.node_identities.items.len);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[0].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[1].scope_id);
    try std.testing.expectEqual(@as(u64, 1), host.node_identities.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[2].scope_id);
    try std.testing.expectEqual(@as(u64, 2), host.node_identities.items[2].ordinal);

    host.walkNodeElemRootIdentitySites(root);
    try std.testing.expectEqual(@as(usize, 3), host.node_identities.items.len);

    const true_scope = host.walkNodeElemWhenBranchIdentitySites(0, 1, .true_branch, when_elem.payload.when.when_true.*);
    try std.testing.expectEqual(@as(usize, 4), host.node_identities.items.len);
    try std.testing.expectEqual(true_scope, host.node_identities.items[3].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[3].ordinal);

    const true_scope_again = host.walkNodeElemWhenBranchIdentitySites(0, 1, .true_branch, when_elem.payload.when.when_true.*);
    try std.testing.expectEqual(true_scope, true_scope_again);
    try std.testing.expectEqual(@as(usize, 4), host.node_identities.items.len);

    const false_scope = host.walkNodeElemWhenBranchIdentitySites(0, 1, .false_branch, when_elem.payload.when.when_false.*);
    try std.testing.expect(false_scope != true_scope);
    try std.testing.expectEqual(@as(usize, 5), host.node_identities.items.len);
    try std.testing.expectEqual(false_scope, host.node_identities.items[4].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[4].ordinal);
}

test "signals host collects NodeElem descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host);

    const root_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .role, "region"),
        testNodeStaticTextAttr(&roc_host, .label, "Dashboard"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeConstExpr(nodeValueStr(&roc_host, "search"))),
        testNodeStaticBoolAttr(.disabled, true),
        testNodeSignalBoolAttr(&roc_host, .checked, testNodeConstExpr(nodeValueBool(false))),
    };
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .test_id, "state-child"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeRefExpr(0)),
        testNodeEventAttr(&roc_host, .click, 0, .unit),
    };
    const state_child_children = [_]abi.NodeElem{
        testNodeText(&roc_host, "state child"),
        testNodeTextSignal(&roc_host, testNodeRefExpr(0)),
    };
    const state_child = testNodeElementWith(&roc_host, "span", &state_child_attrs, &state_child_children);
    const state = testNodeState(&roc_host, state_child);
    const when_elem = testNodeWhen(&roc_host, testNodeText(&roc_host, "true branch"), testNodeText(&roc_host, "false branch"));
    const each_elem = testNodeEach(&roc_host);
    const root_children = [_]abi.NodeElem{
        testNodeText(&roc_host, "intro"),
        testNodeTextSignal(&roc_host, testNodeConstExpr(nodeValueStr(&roc_host, "dynamic text"))),
        state,
        when_elem,
        each_elem,
    };
    const root = testNodeElementWith(&roc_host, "section", &root_attrs, &root_children);
    defer abi.decrefNodeElem(root, &roc_host);

    host.collectNodeElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(usize, 2), stream.elements.items.len);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 0), stream.elements.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(u64, 0), stream.elements.items[0].scope_id);
    try std.testing.expectEqualStrings("section", stream.elements.items[0].tag);
    try std.testing.expectEqual(@as(u64, 4), stream.elements.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[1].parent_elem_id);
    try std.testing.expectEqualStrings("span", stream.elements.items[1].tag);

    try std.testing.expectEqual(@as(usize, 2), stream.text_nodes.items.len);
    try std.testing.expectEqual(@as(u64, 2), stream.text_nodes.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.text_nodes.items[0].parent_elem_id);
    try std.testing.expectEqualStrings("intro", stream.text_nodes.items[0].value);
    try std.testing.expectEqual(@as(u64, 5), stream.text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqualStrings("state child", stream.text_nodes.items[1].value);

    try std.testing.expectEqual(@as(usize, 2), stream.signal_text_nodes.items.len);
    try std.testing.expectEqual(@as(u64, 3), stream.signal_text_nodes.items[0].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.signal_text_nodes.items[0].parent_elem_id);
    try std.testing.expectEqual(abi.NodeSignalExprTag.ConstValue, stream.signal_text_nodes.items[0].signal.tag);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items[0].source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 6), stream.signal_text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqual(abi.NodeSignalExprTag.Ref, stream.signal_text_nodes.items[1].signal.tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[1].source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_nodes.items[1].source_node_ids[0]);
    try std.testing.expectEqual(@as(u64, 7), stream.next_elem_id);

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
    try std.testing.expectEqual(abi.NodeSignalExprTag.ConstValue, stream.signal_text_attrs.items[0].signal.tag);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_attrs.items[0].source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_attrs.items[1].elem_id);
    try std.testing.expectEqual(RenderTextField.value, stream.signal_text_attrs.items[1].field);
    try std.testing.expectEqual(abi.NodeSignalExprTag.Ref, stream.signal_text_attrs.items[1].signal.tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_attrs.items[1].source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_attrs.items[1].source_node_ids[0]);

    try std.testing.expectEqual(@as(usize, 1), stream.static_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.disabled, stream.static_bool_attrs.items[0].field);
    try std.testing.expect(stream.static_bool_attrs.items[0].value);

    try std.testing.expectEqual(@as(usize, 1), stream.signal_bool_attrs.items.len);
    try std.testing.expectEqual(RenderBoolField.checked, stream.signal_bool_attrs.items[0].field);
    try std.testing.expectEqual(abi.NodeSignalExprTag.ConstValue, stream.signal_bool_attrs.items[0].signal.tag);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_bool_attrs.items[0].source_node_ids.len);

    try std.testing.expectEqual(@as(usize, 1), stream.events.items.len);
    try std.testing.expectEqual(RenderEventKind.click, stream.events.items[0].kind);
    try std.testing.expectEqual(@as(u64, 0), stream.events.items[0].binder_ref);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.events.items[0].target_node_id);
    try std.testing.expectEqual(EventPayloadKind.unit, stream.events.items[0].payload_kind);

    try std.testing.expectEqual(@as(usize, 3), stream.scope_sites.items.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.state, stream.scope_sites.items[0].kind);
    try std.testing.expectEqual(@as(u64, 0), stream.scope_sites.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 1), stream.scope_sites.items[0].parent_elem_id);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[0].binder_node_ids.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.when, stream.scope_sites.items[1].kind);
    try std.testing.expectEqual(@as(u64, 1), stream.scope_sites.items[1].ordinal);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[1].binder_node_ids.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.each, stream.scope_sites.items[2].kind);
    try std.testing.expectEqual(@as(u64, 2), stream.scope_sites.items[2].ordinal);
    try std.testing.expectEqual(@as(usize, 0), stream.scope_sites.items[2].binder_node_ids.len);

    try std.testing.expectEqual(@as(usize, 1), stream.states.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.states.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 1), stream.whens.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[1].node_id, stream.whens.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.whens.items[0].source_node_ids.len);
    try std.testing.expectEqual(@as(usize, 1), stream.eaches.items.len);
    try std.testing.expectEqual(stream.scope_sites.items[2].node_id, stream.eaches.items[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.eaches.items[0].source_node_ids.len);

    try std.testing.expectEqual(@as(usize, 3), host.node_identities.items.len);
    try std.testing.expectEqual(@as(u64, 0), host.node_identities.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 1), host.node_identities.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 2), host.node_identities.items[2].ordinal);
}

test "signals host carries binder context into NodeElem when branch collection" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host);

    const branch_children = [_]abi.NodeElem{
        testNodeTextSignal(&roc_host, testNodeRefExpr(0)),
    };
    const branch_true = testNodeElementWith(&roc_host, "span", &.{}, &branch_children);
    const when_elem = testNodeWhen(&roc_host, branch_true, testNodeText(&roc_host, "hidden"));
    const state_child_children = [_]abi.NodeElem{
        when_elem,
    };
    const state_child = testNodeElementWith(&roc_host, "div", &.{}, &state_child_children);
    const root = testNodeState(&roc_host, state_child);
    defer abi.decrefNodeElem(root, &roc_host);

    host.collectNodeElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(usize, 2), stream.scope_sites.items.len);
    const state_site = stream.scope_sites.items[0];
    const when_site = stream.scope_sites.items[1];
    try std.testing.expectEqual(HostNodeScopeSiteKind.state, state_site.kind);
    try std.testing.expectEqual(HostNodeScopeSiteKind.when, when_site.kind);
    try std.testing.expectEqual(@as(usize, 1), when_site.binder_node_ids.len);
    try std.testing.expectEqual(state_site.node_id, when_site.binder_node_ids[0]);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items.len);

    const branch_scope_id = host.collectNodeElemWhenBranchDescriptors(&roc_host, &stream, when_site, .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expect(branch_scope_id != 0);
    try std.testing.expectEqual(@as(usize, 2), stream.elements.items.len);
    try std.testing.expectEqual(@as(u64, 2), stream.elements.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[1].parent_elem_id);
    try std.testing.expectEqual(branch_scope_id, stream.elements.items[1].scope_id);
    try std.testing.expectEqualStrings("span", stream.elements.items[1].tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items.len);
    try std.testing.expectEqual(branch_scope_id, stream.signal_text_nodes.items[0].scope_id);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[0].source_node_ids.len);
    try std.testing.expectEqual(state_site.node_id, stream.signal_text_nodes.items[0].source_node_ids[0]);
}

test "signals host disposes inactive NodeElem when branch scope" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const branch_true = testNodeState(&roc_host, testNodeText(&roc_host, "true branch"));
    const branch_false = testNodeState(&roc_host, testNodeText(&roc_host, "false branch"));
    const when_elem = testNodeWhen(&roc_host, branch_true, branch_false);
    const root_children = [_]abi.NodeElem{when_elem};
    const root = testNodeElementWith(&roc_host, "section", &.{}, &root_children);
    defer abi.decrefNodeElem(root, &roc_host);

    var true_stream: HostNodeDescriptorStream = .{};
    defer true_stream.deinit(host.gpa.allocator(), &roc_host);
    host.collectNodeElemRootDescriptors(&roc_host, &true_stream, root);
    const true_scope = host.collectNodeElemActiveWhenBranchDescriptors(&roc_host, &true_stream, true_stream.scope_sites.items[0], true_stream.whens.items[0], .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expectEqual(@as(usize, 2), true_stream.scope_sites.items.len);
    const true_state_id = true_stream.scope_sites.items[1].node_id;
    try std.testing.expectEqual(true_scope, true_stream.scope_sites.items[1].scope_id);
    try std.testing.expect(host.scopes.items[@intCast(true_scope)].active);
    try std.testing.expect(host.node_identities.items[@intCast(true_state_id)].active);

    var false_stream: HostNodeDescriptorStream = .{};
    defer false_stream.deinit(host.gpa.allocator(), &roc_host);
    host.collectNodeElemRootDescriptors(&roc_host, &false_stream, root);
    const false_scope = host.collectNodeElemActiveWhenBranchDescriptors(&roc_host, &false_stream, false_stream.scope_sites.items[0], false_stream.whens.items[0], .false_branch, when_elem.payload.when.when_false.*);

    try std.testing.expect(false_scope != true_scope);
    try std.testing.expect(!host.scopes.items[@intCast(true_scope)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(true_state_id)].active);
    try std.testing.expect(host.scopes.items[@intCast(false_scope)].active);
    try std.testing.expectEqual(@as(u64, 1), host.pending_roc_metrics.scopes_disposed);
    const false_state_id = false_stream.scope_sites.items[1].node_id;

    var true_again_stream: HostNodeDescriptorStream = .{};
    defer true_again_stream.deinit(host.gpa.allocator(), &roc_host);
    host.collectNodeElemRootDescriptors(&roc_host, &true_again_stream, root);
    const true_again_scope = host.collectNodeElemActiveWhenBranchDescriptors(&roc_host, &true_again_stream, true_again_stream.scope_sites.items[0], true_again_stream.whens.items[0], .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expect(true_again_scope != true_scope);
    try std.testing.expect(true_again_scope != false_scope);
    try std.testing.expect(!host.scopes.items[@intCast(false_scope)].active);
    try std.testing.expect(!host.node_identities.items[@intCast(false_state_id)].active);
    try std.testing.expect(host.scopes.items[@intCast(true_again_scope)].active);
    try std.testing.expectEqual(@as(u64, 2), host.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(true_again_stream.scope_sites.items[1].node_id != true_state_id);
}

test "signals host collects NodeElem each row bodies by keyed scope" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const root_children = [_]abi.NodeElem{
        testNodeEach(&roc_host),
    };
    const root = testNodeElementWith(&roc_host, "section", &.{}, &root_children);
    defer abi.decrefNodeElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    defer initial_stream.deinit(host.gpa.allocator(), &roc_host);
    host.collectNodeElemRootDescriptors(&roc_host, &initial_stream, root);

    try std.testing.expectEqual(@as(usize, 1), initial_stream.scope_sites.items.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.each, initial_stream.scope_sites.items[0].kind);
    try std.testing.expectEqual(@as(usize, 1), initial_stream.eaches.items.len);

    const initial_items = [_]abi.NodeValue{ nodeValueI64(1), nodeValueI64(2), nodeValueI64(3) };
    const initial = host.collectNodeElemEachRowDescriptors(&roc_host, &initial_stream, initial_stream.scope_sites.items[0], initial_stream.eaches.items[0], &initial_items);
    defer freeKeyedRowDiff(&host, initial);

    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(usize, 4), initial_stream.scope_sites.items.len);
    try std.testing.expectEqual(@as(usize, 3), initial_stream.states.items.len);
    try std.testing.expectEqual(@as(usize, 3), initial_stream.text_nodes.items.len);

    try std.testing.expectEqual(initial.scope_ids[0], initial_stream.scope_sites.items[1].scope_id);
    try std.testing.expectEqual(initial.scope_ids[1], initial_stream.scope_sites.items[2].scope_id);
    try std.testing.expectEqual(initial.scope_ids[2], initial_stream.scope_sites.items[3].scope_id);
    try std.testing.expectEqual(@as(u64, 0), initial_stream.scope_sites.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 0), initial_stream.scope_sites.items[2].ordinal);
    try std.testing.expectEqual(@as(u64, 0), initial_stream.scope_sites.items[3].ordinal);
    try std.testing.expectEqualStrings("row-1-1", initial_stream.text_nodes.items[0].value);
    try std.testing.expectEqualStrings("row-2-2", initial_stream.text_nodes.items[1].value);
    try std.testing.expectEqualStrings("row-3-3", initial_stream.text_nodes.items[2].value);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    var reordered_stream: HostNodeDescriptorStream = .{};
    defer reordered_stream.deinit(host.gpa.allocator(), &roc_host);
    host.collectNodeElemRootDescriptors(&roc_host, &reordered_stream, root);

    const reordered_items = [_]abi.NodeValue{ nodeValueI64(3), nodeValueI64(1), nodeValueI64(2) };
    const reordered = host.collectNodeElemEachRowDescriptors(&roc_host, &reordered_stream, reordered_stream.scope_sites.items[0], reordered_stream.eaches.items[0], &reordered_items);
    defer freeKeyedRowDiff(&host, reordered);

    try std.testing.expectEqual(@as(u64, 3), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);
    try std.testing.expectEqual(initial.scope_ids[2], reordered.scope_ids[0]);
    try std.testing.expectEqual(initial.scope_ids[0], reordered.scope_ids[1]);
    try std.testing.expectEqual(initial.scope_ids[1], reordered.scope_ids[2]);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(reordered.scope_ids[2], 0));
    try std.testing.expectEqual(state_for_key_2, reordered_stream.scope_sites.items[3].node_id);

    var changed_stream: HostNodeDescriptorStream = .{};
    defer changed_stream.deinit(host.gpa.allocator(), &roc_host);
    host.collectNodeElemRootDescriptors(&roc_host, &changed_stream, root);

    const changed_items = [_]abi.NodeValue{ nodeValueI64(2), nodeValueI64(4) };
    const changed = host.collectNodeElemEachRowDescriptors(&roc_host, &changed_stream, changed_stream.scope_sites.items[0], changed_stream.eaches.items[0], &changed_items);
    defer freeKeyedRowDiff(&host, changed);

    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);
    try std.testing.expect(!host.scopes.items[@intCast(initial.scope_ids[0])].active);
    try std.testing.expect(!host.scopes.items[@intCast(initial.scope_ids[2])].active);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(changed.scope_ids[0], 0));
}
