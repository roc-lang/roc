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
