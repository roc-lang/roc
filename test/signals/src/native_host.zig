//! Platform host for testing signal-based reactive UI applications.
//!
//! The host owns simulated DOM state, event routing, signal evaluation,
//! scope/key lifecycle, and render batching. Roc returns a retained immutable
//! descriptor tree from `roc_ui_init`; the host drives all later events.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const abi = @import("roc_platform_abi.zig");
const render = @import("render_commands.zig");
const render_sink = @import("render_sink.zig");
const scope_tree = @import("scope_tree.zig");
const erased_calls = @import("erased_calls.zig");
const hv = @import("host_values.zig");
const engine = @import("engine.zig");

const ElemBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const RocStr = abi.RocStr;
const HostValue = u64;
const HostValueTypeTag = *anyopaque;
const HostValueList = abi.RocListWith(HostValue, false);
const I64List = abi.RocListWith(i64, false);
const RenderTextField = render.TextField;
const RenderBoolField = render.BoolField;
const RenderEventKind = render.EventKind;
const CommandCounts = render.Counts;
const HostScopeBranch = scope_tree.Branch;

const native_host_value_type_tags_enabled = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

const NativeCtx = struct {
    pub const Handle = *HostEnv;
    pub const HostValueTypeTag = *anyopaque;
    pub const host_value_type_tags_enabled = native_host_value_type_tags_enabled;
    pub const RegistryOps = hv.RegistryOps(@This().HostValueTypeTag);
    pub const Metrics = RuntimeMetrics;
    pub const Sink = render_sink.DomSink(HostEnv);

    pub fn zeroMetrics() Metrics {
        return zeroRuntimeMetrics();
    }

    pub fn allocator(ctx: Handle) std.mem.Allocator {
        return ctx.gpa.allocator();
    }

    pub fn cloneHostValue(ctx: Handle, value: HostValue) HostValue {
        return ctx.cloneHostValue(value);
    }

    pub fn stateValueByNodeId(ctx: Handle, node_id: u64) HostValue {
        return ctx.stateValueByNodeId(node_id);
    }

    pub fn stateEqCallable(ctx: Handle, node_id: u64) abi.RocErasedCallable {
        return ctx.stateEqCallable(node_id);
    }

    pub fn stateDropCallable(ctx: Handle, node_id: u64) abi.RocErasedCallable {
        return ctx.stateDropCallable(node_id);
    }

    pub fn sink(ctx: Handle) Sink {
        return ctx.sink();
    }
};

const HostEngine = engine.Engine(NativeCtx);
const RuntimeMetrics = engine.RuntimeMetrics;
const NoMetrics = engine.NoMetrics;

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
const HostActiveEventDesc = engine.HostActiveEventDesc(HostValueTypeTag);
const HostPendingTask = engine.HostPendingTask;

const HostSignalCacheSlot = engine.HostSignalCacheSlot;

const HostNodeScopeSiteKind = engine.HostNodeScopeSiteKind;
const HostBinderToken = engine.HostBinderToken;
const HostSignalToken = engine.HostSignalToken;
const HostBinderBinding = engine.HostBinderBinding;

const HostSignalRecordPayload = engine.HostSignalRecordPayload;

const HostSignalRecord = engine.HostSignalRecord;

const HostSignalBinding = engine.HostSignalBinding;

const HostNodeScopeSiteDesc = engine.HostNodeScopeSiteDesc;
const HostNodeWhenDesc = engine.HostNodeWhenDesc;
const HostNodeEachDesc = engine.HostNodeEachDesc;

const HostNodeDescriptorStream = engine.HostNodeDescriptorStream;

const HostActiveStructuralSignalKind = engine.HostActiveStructuralSignalKind;
const HostDirtyStructuralSignal = engine.HostDirtyStructuralSignal;
const HostKeyedRowDiffResult = engine.HostKeyedRowDiffResult;

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    .allow_stack_tracing = false,
};

pub const panic = std.debug.FullPanic(panicImpl);

fn writeStderr(bytes: []const u8) void {
    std.Io.File.stderr().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
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
    expect_absent,
    expect_value,
    expect_checked,
    expect_disabled,
    expect_updates,
    resolve_task,
    reject_task,
    tick_interval,
    expect_cleanup,
    expect_pending_task,
    expect_interval,
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
    task_name: ?[]const u8 = null,
    interval_ms: ?u64 = null,
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_metric_delta: ?i64 = null,
    expected_bool: ?bool,
    line_num: usize,
};

fn freeSpecCommands(allocator: std.mem.Allocator, commands: []SpecCommand) void {
    for (commands) |cmd| {
        cmd.locator.deinit(allocator);
        if (cmd.task_name) |name| allocator.free(name);
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
    const end_quote = std.mem.findScalarLast(u8, input, '"') orelse return ParseError.InvalidFormat;
    if (end_quote == 0) return ParseError.InvalidFormat;
    const before_end = input[0..end_quote];
    const start_quote = std.mem.findScalarLast(u8, before_end, '"') orelse return ParseError.InvalidFormat;
    const tail = std.mem.trim(u8, input[end_quote + 1 ..], " \t");
    if (tail.len != 0) return ParseError.InvalidFormat;
    return .{
        .head = std.mem.trim(u8, input[0..start_quote], " \t"),
        .quoted = input[start_quote + 1 .. end_quote],
    };
}

fn splitTrailingToken(input: []const u8) ParseError!struct { head: []const u8, token: []const u8 } {
    const trimmed = std.mem.trim(u8, input, " \t");
    const space_idx = std.mem.findLastAny(u8, trimmed, " \t") orelse return ParseError.InvalidFormat;
    return .{
        .head = std.mem.trim(u8, trimmed[0..space_idx], " \t"),
        .token = std.mem.trim(u8, trimmed[space_idx + 1 ..], " \t"),
    };
}

fn parseSingleQuoted(input: []const u8) ParseError![]const u8 {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len < 2 or trimmed[0] != '"' or trimmed[trimmed.len - 1] != '"') return ParseError.InvalidFormat;
    return trimmed[1 .. trimmed.len - 1];
}

fn splitTwoQuoted(input: []const u8) ParseError!struct { first: []const u8, second: []const u8 } {
    const trimmed = std.mem.trim(u8, input, " \t");
    if (trimmed.len < 5 or trimmed[0] != '"') return ParseError.InvalidFormat;
    const first_end = std.mem.findScalarPos(u8, trimmed, 1, '"') orelse return ParseError.InvalidFormat;
    const rest = std.mem.trim(u8, trimmed[first_end + 1 ..], " \t");
    if (rest.len < 2 or rest[0] != '"' or rest[rest.len - 1] != '"') return ParseError.InvalidFormat;
    return .{
        .first = trimmed[1..first_end],
        .second = rest[1 .. rest.len - 1],
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
        const space_idx = std.mem.findAny(u8, rest, " \t") orelse return ParseError.InvalidFormat;
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
        .expected_metric_delta = null,
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
        } else if (std.mem.startsWith(u8, trimmed, "expect_absent ")) {
            try appendSpecCommand(&commands, allocator, .expect_absent, try parseLocator(allocator, trimmed[14..]), null, null, null, line_num);
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
        } else if (std.mem.startsWith(u8, trimmed, "resolve_task ")) {
            const split = try splitTwoQuoted(trimmed["resolve_task ".len..]);
            const task_name = allocator.dupe(u8, split.first) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const payload = allocator.dupe(u8, split.second) catch return ParseError.OutOfMemory;
            errdefer allocator.free(payload);
            try appendSpecCommand(&commands, allocator, .resolve_task, emptyLocator(), payload, null, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "reject_task ")) {
            const split = try splitTwoQuoted(trimmed["reject_task ".len..]);
            const task_name = allocator.dupe(u8, split.first) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const payload = allocator.dupe(u8, split.second) catch return ParseError.OutOfMemory;
            errdefer allocator.free(payload);
            try appendSpecCommand(&commands, allocator, .reject_task, emptyLocator(), payload, null, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "tick_interval ")) {
            const period_text = std.mem.trim(u8, trimmed["tick_interval ".len..], " \t");
            const period_ms = std.fmt.parseInt(u64, period_text, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .tick_interval, emptyLocator(), null, null, null, line_num);
            commands.items[commands.items.len - 1].interval_ms = period_ms;
        } else if (std.mem.startsWith(u8, trimmed, "expect_cleanup ")) {
            const split = try splitTrailingToken(trimmed["expect_cleanup ".len..]);
            const name_value = try parseSingleQuoted(split.head);
            const task_name = allocator.dupe(u8, name_value) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_cleanup, emptyLocator(), null, expected_count, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "expect_pending_task ")) {
            const split = try splitTrailingToken(trimmed["expect_pending_task ".len..]);
            const name_value = try parseSingleQuoted(split.head);
            const task_name = allocator.dupe(u8, name_value) catch return ParseError.OutOfMemory;
            errdefer allocator.free(task_name);
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_pending_task, emptyLocator(), null, expected_count, null, line_num);
            commands.items[commands.items.len - 1].task_name = task_name;
        } else if (std.mem.startsWith(u8, trimmed, "expect_interval ")) {
            const split = try splitTrailingToken(trimmed["expect_interval ".len..]);
            const period_ms = std.fmt.parseInt(u64, split.head, 10) catch return ParseError.InvalidFormat;
            const expected_count = std.fmt.parseInt(u64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_interval, emptyLocator(), null, expected_count, null, line_num);
            commands.items[commands.items.len - 1].interval_ms = period_ms;
        } else if (std.mem.startsWith(u8, trimmed, "expect_metric_delta ")) {
            const split = try splitTrailingToken(trimmed[20..]);
            const metric_name = allocator.dupe(u8, split.head) catch return ParseError.OutOfMemory;
            const expected_delta = std.fmt.parseInt(i64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_metric_delta, emptyLocator(), metric_name, null, null, line_num);
            commands.items[commands.items.len - 1].expected_metric_delta = expected_delta;
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
        error.MissingTag => failHost("HostValue read crossed erasure boundary without a type tag"),
        error.TagMismatch => failHost("HostValue read crossed erasure boundary with the wrong type tag"),
        error.ConflictingTag => failHost("HostValue was tagged with a conflicting type tag"),
        else => failHost("HostValue registry operation failed"),
    }
}

const RocAllocation = struct {
    ptr: [*]u8,
    total_size: usize,
    alignment: std.mem.Alignment,
};

const RocAllocationHeader = extern struct {
    total_size: usize,
    ledger_index: usize,
};

const TestHostValueKind = enum {
    unit,
    i64,
    bool,
    str,
    i64_list,
};

const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    engine: HostEngine = .{},
    test_state: TestState,
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty,
    test_host_value_kinds: std.ArrayListUnmanaged(?TestHostValueKind) = .empty,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,

    fn init() HostEnv {
        return .{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
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

    pub fn sinkApplyBoolField(self: *HostEnv, elem_id: u64, field: RenderBoolField, value: bool) void {
        setRenderBoolField(self, elem_id, field, value);
    }

    pub fn sinkClearTextField(self: *HostEnv, elem_id: u64, field: RenderTextField) void {
        clearRenderTextField(self, elem_id, field);
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

    pub fn sinkStartInterval(_: *HostEnv, _: u64, _: u64) void {}

    pub fn sinkCancelInterval(_: *HostEnv, _: u64) void {}

    pub fn sinkStartTask(_: *HostEnv, _: u64, _: []const u8, _: []const u8) void {}

    pub fn sinkCancelTask(_: *HostEnv, _: u64) void {}

    pub fn sinkDebugAssertNode(self: *HostEnv, elem_id: u64, active: bool, tag: ?[]const u8, parent_id: ?u64, children: []const u64, click_event: ?u64, input_event: ?u64, check_event: ?u64) void {
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
    }

    fn activeRocHost(self: *HostEnv) *abi.RocHost {
        return self.engine.roc_host orelse failHost("HostValue type tag release requires an active Roc host");
    }

    fn releaseOwnedHostValueTypeTag(self: *HostEnv, tag: HostValueTypeTag) void {
        abi.decrefBox(@ptrCast(tag), self.activeRocHost());
    }

    fn hostValueRegistryOps(self: *HostEnv) hv.RegistryOps(HostValueTypeTag) {
        return .{ .roc_host = self.activeRocHost() };
    }

    // `ctx` surface consumed by the shared `host_values` box constructors
    // (`pub` so the `host_values` module can call them through `anytype`).
    pub fn store(self: *HostEnv, box: abi.RocBox) HostValue {
        return self.storeHostValue(box);
    }

    pub fn recordKind(self: *HostEnv, value: HostValue, kind: hv.ValueKind) void {
        if (builtin.is_test) self.setTestHostValueKind(value, switch (kind) {
            .unit => .unit,
            .str => .str,
            .bool => .bool,
            .i64 => .i64,
        });
    }

    fn hostValueTypeTagId(tag: HostValueTypeTag) u64 {
        const payload: *const u64 = @ptrCast(@alignCast(tag));
        return payload.*;
    }

    fn hostValueTypeTagsMatch(actual_tag: HostValueTypeTag, expected_tag: HostValueTypeTag) bool {
        if (actual_tag == expected_tag) return true;
        const actual_id = HostEnv.hostValueTypeTagId(actual_tag);
        return actual_id != 0 and actual_id == HostEnv.hostValueTypeTagId(expected_tag);
    }

    fn resetTestHostValueKind(self: *HostEnv, value: HostValue) void {
        if (builtin.is_test) {
            const allocator = self.gpa.allocator();
            const index = value - 1;
            if (index >= self.test_host_value_kinds.items.len) {
                self.test_host_value_kinds.append(allocator, null) catch std.process.exit(1);
            } else {
                self.test_host_value_kinds.items[@intCast(index)] = null;
            }
        }
    }

    fn storeHostValueWithOwnedTag(self: *HostEnv, box: abi.RocBox, owned_tag: ?HostValueTypeTag) HostValue {
        const allocator = self.gpa.allocator();
        const value = self.engine.host_values.storeOwnedTag(allocator, box, owned_tag, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(value);
        return value;
    }

    fn storeHostValueWithRetainedTag(self: *HostEnv, box: abi.RocBox, borrowed_tag: ?HostValueTypeTag) HostValue {
        const allocator = self.gpa.allocator();
        const value = self.engine.host_values.storeRetainedTag(allocator, box, borrowed_tag, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        self.resetTestHostValueKind(value);
        return value;
    }

    fn storeHostValue(self: *HostEnv, box: abi.RocBox) HostValue {
        return self.storeHostValueWithOwnedTag(box, null);
    }

    fn storeTaggedHostValue(self: *HostEnv, box: abi.RocBox, owned_tag: HostValueTypeTag) HostValue {
        return self.storeHostValueWithOwnedTag(box, owned_tag);
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

    fn getHostValue(self: *HostEnv, value: HostValue) abi.RocBox {
        return self.engine.host_values.get(value, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn hostValueTypeTag(self: *HostEnv, value: HostValue) ?HostValueTypeTag {
        return self.engine.host_values.tag(value) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn assertHostValueTypeTag(self: *HostEnv, value: HostValue, expected_tag: HostValueTypeTag) void {
        if (comptime native_host_value_type_tags_enabled) {
            const actual_tag = self.hostValueTypeTag(value) orelse failHost("HostValue read crossed erasure boundary without a type tag");
            if (!HostEnv.hostValueTypeTagsMatch(actual_tag, expected_tag)) {
                var buf: [192]u8 = undefined;
                const message = std.fmt.bufPrint(
                    &buf,
                    "HostValue read crossed erasure boundary with the wrong type tag: value={} actual=0x{x} expected=0x{x}",
                    .{ value, @intFromPtr(actual_tag), @intFromPtr(expected_tag) },
                ) catch "HostValue read crossed erasure boundary with the wrong type tag";
                failHost(message);
            }
        }
    }

    fn setHostValueTypeTag(self: *HostEnv, value: HostValue, borrowed_tag: HostValueTypeTag) void {
        self.engine.host_values.setTag(value, borrowed_tag, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
    }

    fn getTaggedHostValue(self: *HostEnv, value: HostValue, tag: HostValueTypeTag) abi.RocBox {
        self.assertHostValueTypeTag(value, tag);
        return self.getHostValue(value);
    }

    fn takeHostValue(self: *HostEnv, value: HostValue) abi.RocBox {
        const box = self.engine.host_values.take(value, self.hostValueRegistryOps()) catch |err| {
            failHostValueRegistryError(err);
        };
        if (builtin.is_test) self.test_host_value_kinds.items[@intCast(value - 1)] = null;
        return box;
    }

    fn takeTaggedHostValue(self: *HostEnv, value: HostValue, tag: HostValueTypeTag) abi.RocBox {
        self.assertHostValueTypeTag(value, tag);
        return self.takeHostValue(value);
    }

    // `pub` so the shared `engine.HostValueCell.cloneRetained` can clone a value
    // through the host's registry (the engine treats `self` as its clone ctx).
    pub fn cloneHostValue(self: *HostEnv, value: HostValue) HostValue {
        const allocator = self.gpa.allocator();
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
        const allocator = self.gpa.allocator();
        for (self.engine.signal_event_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.engine.signal_event_routes.items.len = 0;
    }

    fn rebuildSignalEventRoutesFromSignals(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
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
        const allocator = self.gpa.allocator();
        for (self.engine.signal_descriptors.items) |descriptor| {
            allocator.free(descriptor.source_state_ids);
            allocator.free(descriptor.source_event_ids);
            allocator.free(descriptor.input_signal_ids);
        }
        self.engine.signal_descriptors.items.len = 0;
    }

    fn clearSignalRoutes(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.engine.signal_routes.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.engine.signal_routes.items.len = 0;
    }

    fn clearSignalDependents(self: *HostEnv) void {
        const allocator = self.gpa.allocator();
        for (self.engine.signal_dependents.items) |route| {
            allocator.free(route.signal_ids);
        }
        self.engine.signal_dependents.items.len = 0;
    }

    fn clearSignalCache(self: *HostEnv) void {
        self.engine.clearSignalCache() catch |err| {
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
        const allocator = self.gpa.allocator();
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
        self.engine.clearStates() catch |err| {
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
        if (state.cell.valueEquals(roc_host, value)) {
            state.cell.dropIncoming(roc_host, value);
            return false;
        }

        state.cell.replaceValue(roc_host, value);
        state.version += 1;
        return true;
    }

    pub fn stateEqCallable(self: *HostEnv, node_id: u64) abi.RocErasedCallable {
        return self.engine.stateEqCallable(node_id) catch |err| switch (err) {
            error.MissingActiveState => failHost("active state has no equality callable"),
        };
    }

    pub fn stateDropCallable(self: *HostEnv, node_id: u64) abi.RocErasedCallable {
        return self.engine.stateDropCallable(node_id) catch |err| switch (err) {
            error.MissingActiveState => failHost("active state has no drop callable"),
        };
    }

    fn validateScopeId(self: *HostEnv, scope_id: u64) void {
        self.engine.validateScopeId(scope_id) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn internRootScope(self: *HostEnv) u64 {
        return self.engine.internRootScope(self.gpa.allocator()) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn internComponentScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64) u64 {
        return self.engine.internComponentScope(self.gpa.allocator(), parent_scope_id, site_ordinal) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn internWhenBranchScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) u64 {
        return self.engine.internWhenBranchScope(self.gpa.allocator(), parent_scope_id, site_ordinal, branch) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn createEachRowScope(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, key: HostValue, item: HostValue, key_eq: abi.RocErasedCallable, key_drop: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, item_drop: abi.RocErasedCallable) u64 {
        return self.engine.createEachRowScope(self, parent_scope_id, site_ordinal, key, item, key_eq, key_drop, item_eq, item_drop);
    }

    fn internNodeIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        return self.engine.internNodeIdentity(self.gpa.allocator(), scope_id, ordinal) catch |err| {
            failScopeOrIdentityTableError(err, "scope id has no host scope descriptor");
        };
    }

    fn internDomIdentity(self: *HostEnv, scope_id: u64, ordinal: u64) u64 {
        return self.engine.internDomIdentity(self.gpa.allocator(), scope_id, ordinal) catch |err| {
            failScopeOrIdentityTableError(err, "scope id has no host scope descriptor");
        };
    }

    fn disposeScopeSubtree(self: *HostEnv, roc_host: *abi.RocHost, scope_id: u64) void {
        self.engine.disposeScopeSubtree(self, roc_host, scope_id);
    }

    fn eachRowScopeValues(self: *HostEnv, scope_id: u64) engine.EachRowValues {
        return self.engine.eachRowScopeValues(scope_id);
    }

    fn syncEachRowScopes(self: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, key_hash: abi.RocErasedCallable, key_eq: abi.RocErasedCallable, key_drop: abi.RocErasedCallable, item_eq: abi.RocErasedCallable, item_drop: abi.RocErasedCallable) HostKeyedRowDiffResult {
        return self.engine.syncEachRowScopes(self, roc_host, parent_scope_id, site_ordinal, keys, items, key_hash, key_eq, key_drop, item_eq, item_drop);
    }

    fn walkElemIdentitySites(self: *HostEnv, elem: abi.Elem, scope_id: u64, ordinal: *u64) void {
        self.validateScopeId(scope_id);

        switch (elem.tag) {
            .Element => {
                for (elem.payload.element.children.items()) |child| {
                    self.walkElemIdentitySites(child, scope_id, ordinal);
                }
            },
            .State => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
                self.walkElemIdentitySites(elem.payload.state.child.*, scope_id, ordinal);
            },
            .Component => {
                const site_ordinal = ordinal.*;
                _ = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                const component_scope_id = self.internComponentScope(scope_id, site_ordinal);
                var component_ordinal: u64 = 0;
                self.walkElemIdentitySites(elem.payload.component.child.*, component_scope_id, &component_ordinal);
            },
            .When => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Each => {
                _ = self.internNodeIdentity(scope_id, ordinal.*);
                ordinal.* += 1;
            },
            .Cleanup, .OnChange, .Text, .TextSignal => {},
        }
    }

    fn walkElemRootIdentitySites(self: *HostEnv, root: abi.Elem) void {
        const root_scope_id = self.internRootScope();
        var ordinal: u64 = 0;
        self.walkElemIdentitySites(root, root_scope_id, &ordinal);
    }

    fn walkElemWhenBranchIdentitySites(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch, elem: abi.Elem) u64 {
        const branch_scope_id = self.internWhenBranchScope(parent_scope_id, site_ordinal, branch);
        var ordinal: u64 = 0;
        self.walkElemIdentitySites(elem, branch_scope_id, &ordinal);
        return branch_scope_id;
    }

    fn bindNodeSignal(self: *HostEnv, allocator: std.mem.Allocator, stream: *HostNodeDescriptorStream, expr: abi.NodeSignalExpr, binder_stack: []const HostBinderBinding) HostSignalBinding {
        return self.engine.bindNodeSignal(allocator, stream, expr, binder_stack);
    }

    fn collectNodeAttrDescriptor(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem_id: u64, attr: abi.NodeAttr, binder_stack: []const HostBinderBinding) void {
        return self.engine.collectNodeAttrDescriptor(self, roc_host, stream, elem_id, attr, binder_stack);
    }

    fn collectElemDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, elem: abi.Elem, scope_id: u64, parent_elem_id: u64, ordinal: *u64, dom_ordinal: *u64, binder_stack: *std.ArrayListUnmanaged(HostBinderBinding)) void {
        self.validateScopeId(scope_id);

        const allocator = self.gpa.allocator();
        switch (elem.tag) {
            .Element => {
                const payload = elem.payload.element;
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                _ = stream.appendElement(allocator, elem_id, parent_elem_id, scope_id, payload.tag.asSlice());
                for (payload.attrs.items()) |attr| {
                    self.collectNodeAttrDescriptor(roc_host, stream, elem_id, attr, binder_stack.items);
                }
                for (payload.children.items()) |child| {
                    self.collectElemDescriptors(roc_host, stream, child, scope_id, elem_id, ordinal, dom_ordinal, binder_stack);
                }
            },
            .Text => {
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                stream.appendTextNode(allocator, elem_id, parent_elem_id, scope_id, elem.payload.text.asSlice());
            },
            .TextSignal => {
                const elem_id = self.internDomIdentity(scope_id, dom_ordinal.*);
                dom_ordinal.* += 1;
                const text_signal = elem.payload.text_signal;
                const signal = self.bindNodeSignal(allocator, stream, text_signal.signal.*, binder_stack.items);
                stream.appendSignalTextNode(allocator, roc_host, &self.engine.pending_roc_metrics, elem_id, parent_elem_id, scope_id, signal, text_signal.read);
            },
            .Cleanup => {
                stream.appendCleanup(allocator, scope_id, elem.payload.cleanup.cleanup.asSlice());
            },
            .OnChange => {
                const payload = elem.payload.on_change;
                const signal = self.bindNodeSignal(allocator, stream, payload.signal.*, binder_stack.items);
                stream.appendOnChange(allocator, roc_host, &self.engine.pending_roc_metrics, scope_id, signal, payload.to_cmd);
            },
            .State => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .state, binder_stack.items);
                stream.appendState(allocator, roc_host, &self.engine.pending_roc_metrics, node_id, elem.payload.state.initial, elem.payload.state.eq, elem.payload.state.drop);
                binder_stack.append(allocator, .{ .token = elem.payload.state.binder, .node_id = node_id }) catch std.process.exit(1);
                self.collectElemDescriptors(roc_host, stream, elem.payload.state.child.*, scope_id, parent_elem_id, ordinal, dom_ordinal, binder_stack);
                _ = binder_stack.pop() orelse unreachable;
            },
            .Component => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .component, binder_stack.items);
                const component_scope_id = self.internComponentScope(scope_id, site_ordinal);
                var component_ordinal: u64 = 0;
                var component_dom_ordinal: u64 = 0;
                self.collectElemDescriptors(roc_host, stream, elem.payload.component.child.*, component_scope_id, parent_elem_id, &component_ordinal, &component_dom_ordinal, binder_stack);
            },
            .When => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .when, binder_stack.items);
                const condition = self.bindNodeSignal(allocator, stream, elem.payload.when.condition.*, binder_stack.items);
                stream.appendWhen(allocator, roc_host, &self.engine.pending_roc_metrics, node_id, condition, elem.payload.when.read, elem.payload.when.when_false.*, elem.payload.when.when_true.*);
            },
            .Each => {
                const site_ordinal = ordinal.*;
                const node_id = self.internNodeIdentity(scope_id, site_ordinal);
                ordinal.* += 1;
                stream.appendScopeSite(allocator, node_id, scope_id, site_ordinal, parent_elem_id, .each, binder_stack.items);
                const items = self.bindNodeSignal(allocator, stream, elem.payload.each.items.*, binder_stack.items);
                stream.appendEach(
                    allocator,
                    roc_host,
                    &self.engine.pending_roc_metrics,
                    node_id,
                    items,
                    elem.payload.each.items_to_values,
                    elem.payload.each.key_hash,
                    elem.payload.each.key_of,
                    elem.payload.each.key_eq,
                    elem.payload.each.key_drop,
                    elem.payload.each.item_eq,
                    elem.payload.each.item_drop,
                    elem.payload.each.row,
                );
            },
        }
    }

    fn collectElemRootDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem) void {
        const root_scope_id = self.internRootScope();
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectElemDescriptors(roc_host, stream, root, root_scope_id, 0, &ordinal, &dom_ordinal, &binder_stack);
    }

    fn collectElemWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, branch: HostScopeBranch, elem: abi.Elem) u64 {
        if (site.kind != .when) {
            failHost("Elem branch collection requires a when scope site");
        }
        const branch_scope_id = self.internWhenBranchScope(site.scope_id, site.ordinal, branch);
        const allocator = self.gpa.allocator();
        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);
        var ordinal: u64 = 0;
        var dom_ordinal: u64 = 0;
        self.collectElemDescriptors(roc_host, stream, elem, branch_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack);
        return branch_scope_id;
    }

    fn activeWhenBranchScopeId(self: *HostEnv, parent_scope_id: u64, site_ordinal: u64, branch: HostScopeBranch) ?u64 {
        return self.engine.activeWhenBranchScopeId(parent_scope_id, site_ordinal, branch) catch |err| {
            failScopeTreeError(err, "scope id has no host scope descriptor");
        };
    }

    fn collectElemActiveWhenBranchDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, when: HostNodeWhenDesc, active_branch: HostScopeBranch, elem: abi.Elem) u64 {
        if (site.kind != .when) {
            failHost("Elem active branch collection requires a when scope site");
        }
        if (site.node_id != when.node_id) {
            failHost("Elem active branch collection received mismatched when descriptors");
        }

        if (self.activeWhenBranchScopeId(site.scope_id, site.ordinal, active_branch.opposite())) |inactive_scope_id| {
            self.disposeScopeSubtree(roc_host, inactive_scope_id);
        }

        return self.collectElemWhenBranchDescriptors(roc_host, stream, site, active_branch, elem);
    }

    fn collectElemEachRowDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, site: HostNodeScopeSiteDesc, each: HostNodeEachDesc, items: []const HostValue) HostKeyedRowDiffResult {
        if (site.kind != .each) {
            failHost("Elem row collection requires an each scope site");
        }
        if (site.node_id != each.node_id) {
            failHost("Elem row collection received mismatched each descriptors");
        }

        const allocator = self.gpa.allocator();
        const keys = allocator.alloc(HostValue, items.len) catch std.process.exit(1);
        defer allocator.free(keys);

        for (items, 0..) |item, index| {
            keys[index] = callErasedHostValueToHostValue(roc_host, each.key_of, item);
        }

        const diff = self.syncEachRowScopes(roc_host, site.scope_id, site.ordinal, keys, items, each.key_hash, each.key_eq, each.key_drop, each.item_eq, each.item_drop);

        var binder_stack: std.ArrayListUnmanaged(HostBinderBinding) = .empty;
        defer binder_stack.deinit(allocator);
        binder_stack.appendSlice(allocator, site.binder_bindings) catch std.process.exit(1);

        for (diff.scope_ids) |row_scope_id| {
            const row_values = self.eachRowScopeValues(row_scope_id);
            const row_elem = callErasedHostValueHostValueToElem(roc_host, each.row, row_values.key, row_values.item);
            defer abi.decrefElem(row_elem, roc_host);

            var ordinal: u64 = 0;
            var dom_ordinal: u64 = 0;
            self.collectElemDescriptors(roc_host, stream, row_elem, row_scope_id, site.parent_elem_id, &ordinal, &dom_ordinal, &binder_stack);
        }

        return diff;
    }

    fn collectActiveElemRootDescriptors(self: *HostEnv, roc_host: *abi.RocHost, stream: *HostNodeDescriptorStream, root: abi.Elem, dirty_source_node_ids: []const u64) void {
        return self.engine.collectActiveElemRootDescriptors(self, roc_host, stream, root, dirty_source_node_ids);
    }

    fn clearScopes(self: *HostEnv) void {
        self.engine.clearScopes() catch |err| {
            failRocHostRequiredError(err, "host scope table cannot release keys without a Roc host");
        };
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.gpa.allocator();

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
        self.engine.active_stream.deinit(allocator, self.engine.roc_host.?, &self.engine.pending_roc_metrics);
        self.clearActiveEvents();
        self.engine.active_events.deinit(allocator);

        self.clearPendingTasks();
        self.engine.pending_tasks.deinit(allocator);

        for (self.engine.cleanup_events.items) |name| {
            allocator.free(name);
        }
        self.engine.cleanup_events.deinit(allocator);

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
        self.clearScopes();
        self.engine.scopes.deinit(allocator);
        self.engine.node_identities.deinit(allocator);
        self.engine.dom_identities.deinit(allocator);
        self.engine.deinitRenderCache(self);

        freeSpecCommands(allocator, self.test_state.commands);

        if (self.engine.host_values.hasLiveValues()) failHost("host value registry still owned a typed cell at shutdown");
        self.engine.host_values.deinit(allocator);
        self.test_host_value_kinds.deinit(allocator);

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

    fn countElementsByLocator(self: *HostEnv, locator: Locator) usize {
        var match_count: usize = 0;
        for (self.dom_elements.items) |*elem| {
            if (!elem.active) continue;
            if (!self.locatorMatches(elem, locator)) continue;
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

var current_host: ?*HostEnv = null;
var current_roc_host: ?*abi.RocHost = null;

fn hostFromRocHost(roc_host: *abi.RocHost) *HostEnv {
    return @ptrCast(@alignCast(roc_host.env));
}

fn currentRocHost() *abi.RocHost {
    return current_roc_host orelse @panic("signals RocHost is not initialized");
}

fn rocAllocationHeaderBytes(byte_alignment: usize) usize {
    return std.mem.alignForward(usize, @sizeOf(RocAllocationHeader), byte_alignment);
}

fn rocAllocationHeaderFromUserPtr(ptr: *anyopaque) *RocAllocationHeader {
    return @ptrFromInt(@intFromPtr(ptr) - @sizeOf(RocAllocationHeader));
}

fn rocAllocationUserPtr(alloc: RocAllocation) *anyopaque {
    return @ptrFromInt(@intFromPtr(alloc.ptr) + rocAllocationHeaderBytes(alloc.alignment.toByteUnits()));
}

fn removeRocAllocationAt(host: *HostEnv, ledger_index: usize, base_ptr: [*]u8) void {
    if (ledger_index >= host.roc_allocations.items.len) {
        failHost("Roc allocation header referenced an unknown ledger index");
    }
    const recorded = host.roc_allocations.items[ledger_index];
    if (recorded.ptr != base_ptr) {
        failHost("Roc allocation header did not match its ledger slot");
    }

    const last_index = host.roc_allocations.items.len - 1;
    _ = host.roc_allocations.swapRemove(ledger_index);
    if (ledger_index != last_index) {
        const moved = host.roc_allocations.items[ledger_index];
        rocAllocationHeaderFromUserPtr(rocAllocationUserPtr(moved)).ledger_index = ledger_index;
    }
}

fn rocAllocFn(roc_host: *abi.RocHost, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const header_bytes = rocAllocationHeaderBytes(min_alignment);
    const total_size = length + header_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const user_ptr: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + header_bytes);
    const ledger_index = host.roc_allocations.items.len;
    rocAllocationHeaderFromUserPtr(user_ptr).* = .{
        .total_size = total_size,
        .ledger_index = ledger_index,
    };

    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);
    host.alloc_count += 1;
    host.engine.pending_roc_metrics.bump(.allocs_this_event, 1);

    return user_ptr;
}

fn rocDeallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const header_bytes = rocAllocationHeaderBytes(min_alignment);
    const header = rocAllocationHeaderFromUserPtr(ptr);
    const total_size = header.total_size;
    const ledger_index = header.ledger_index;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - header_bytes);

    removeRocAllocationAt(host, ledger_index, base_ptr);
    host.dealloc_count += 1;
    host.engine.pending_roc_metrics.bump(.deallocs_this_event, 1);

    allocator.rawFree(base_ptr[0..total_size], align_enum, @returnAddress());
}

fn rocReallocFn(roc_host: *abi.RocHost, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromRocHost(roc_host);
    const allocator = host.gpa.allocator();
    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const header_bytes = rocAllocationHeaderBytes(min_alignment);
    const old_header = rocAllocationHeaderFromUserPtr(ptr);
    const old_total_size = old_header.total_size;
    const old_ledger_index = old_header.ledger_index;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - header_bytes);
    const old_user_data_size = old_total_size - header_bytes;
    const new_total_size = new_length + header_bytes;

    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse std.process.exit(1);
    const new_user_ptr: [*]u8 = @ptrFromInt(@intFromPtr(new_ptr) + header_bytes);
    const old_user_ptr: [*]const u8 = @ptrCast(ptr);
    const copy_size = @min(old_user_data_size, new_length);
    @memcpy(new_user_ptr[0..copy_size], old_user_ptr[0..copy_size]);

    const new_ledger_index = host.roc_allocations.items.len;
    rocAllocationHeaderFromUserPtr(new_user_ptr).* = .{
        .total_size = new_total_size,
        .ledger_index = new_ledger_index,
    };
    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = new_ptr,
        .total_size = new_total_size,
        .alignment = align_enum,
    }) catch std.process.exit(1);

    removeRocAllocationAt(host, old_ledger_index, old_base_ptr);
    host.alloc_count += 1;
    host.dealloc_count += 1;
    host.engine.pending_roc_metrics.bump(.allocs_this_event, 1);
    host.engine.pending_roc_metrics.bump(.deallocs_this_event, 1);

    allocator.rawFree(old_base_ptr[0..old_total_size], align_enum, @returnAddress());

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

fn currentHost() *HostEnv {
    return current_host orelse @panic("signals HostEnv is not initialized");
}

fn hostValueClone(value: HostValue) callconv(.c) HostValue {
    return currentHost().cloneHostValue(value);
}

fn hostValueGet(value: HostValue) callconv(.c) abi.RocBox {
    return currentHost().getHostValue(value);
}

fn hostValueGetTagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    const host = currentHost();
    defer host.releaseOwnedHostValueTypeTag(tag);
    return host.getTaggedHostValue(value, tag);
}

fn hostValueStore(box: abi.RocBox) callconv(.c) HostValue {
    return currentHost().storeHostValue(box);
}

fn hostValueStoreTagged(box: abi.RocBox, tag: HostValueTypeTag) callconv(.c) HostValue {
    return currentHost().storeTaggedHostValue(box, tag);
}

fn hostValueTake(value: HostValue) callconv(.c) abi.RocBox {
    return currentHost().takeHostValue(value);
}

fn hostValueTakeTagged(value: HostValue, tag: HostValueTypeTag) callconv(.c) abi.RocBox {
    const host = currentHost();
    defer host.releaseOwnedHostValueTypeTag(tag);
    return host.takeTaggedHostValue(value, tag);
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

fn setOwnedString(allocator: std.mem.Allocator, field: *?[]const u8, value: []const u8) void {
    if (field.*) |existing| {
        allocator.free(existing);
    }
    field.* = allocator.dupe(u8, value) catch std.process.exit(1);
}

fn clearOwnedString(allocator: std.mem.Allocator, field: *?[]const u8) void {
    if (field.*) |existing| {
        allocator.free(existing);
    }
    field.* = null;
}

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
        .active_graph_records_rebuilt = left.active_graph_records_rebuilt + right.active_graph_records_rebuilt,
        .append_child = left.append_child + right.append_child,
        .allocs_this_event = left.allocs_this_event + right.allocs_this_event,
        .bind_event = left.bind_event + right.bind_event,
        .closure_releases = left.closure_releases + right.closure_releases,
        .closure_retains = left.closure_retains + right.closure_retains,
        .create_element = left.create_element + right.create_element,
        .deallocs_this_event = left.deallocs_this_event + right.deallocs_this_event,
        .derived_calls_into_roc = left.derived_calls_into_roc + right.derived_calls_into_roc,
        .each_key_compares = left.each_key_compares + right.each_key_compares,
        .events_processed = left.events_processed + right.events_processed,
        .move_before = left.move_before + right.move_before,
        .nodes_recomputed = left.nodes_recomputed + right.nodes_recomputed,
        .patches_emitted = left.patches_emitted + right.patches_emitted,
        .propagation_prunes = left.propagation_prunes + right.propagation_prunes,
        .recompute_batches = left.recompute_batches + right.recompute_batches,
        .remove_node = left.remove_node + right.remove_node,
        .retained_alloc_delta = left.retained_alloc_delta + right.retained_alloc_delta,
        .reset_dom = left.reset_dom + right.reset_dom,
        .rows_created = left.rows_created + right.rows_created,
        .rows_removed = left.rows_removed + right.rows_removed,
        .rows_reused = left.rows_reused + right.rows_reused,
        .scopes_created = left.scopes_created + right.scopes_created,
        .scopes_disposed = left.scopes_disposed + right.scopes_disposed,
        .set_checked = left.set_checked + right.set_checked,
        .set_disabled = left.set_disabled + right.set_disabled,
        .set_metadata = left.set_metadata + right.set_metadata,
        .set_text = left.set_text + right.set_text,
        .set_value = left.set_value + right.set_value,
        .stream_nodes_scanned = left.stream_nodes_scanned + right.stream_nodes_scanned,
    };
}

fn u64MetricAsI64(value: u64) i64 {
    return std.math.cast(i64, value) orelse failHost("runtime metric exceeded signed assertion range");
}

fn runtimeMetricValue(metrics: RuntimeMetrics, name: []const u8) ?i64 {
    if (std.mem.eql(u8, name, "active_graph_records_rebuilt")) return u64MetricAsI64(metrics.active_graph_records_rebuilt);
    if (std.mem.eql(u8, name, "reset_dom")) return u64MetricAsI64(metrics.reset_dom);
    if (std.mem.eql(u8, name, "create_element")) return u64MetricAsI64(metrics.create_element);
    if (std.mem.eql(u8, name, "append_child")) return u64MetricAsI64(metrics.append_child);
    if (std.mem.eql(u8, name, "remove_node")) return u64MetricAsI64(metrics.remove_node);
    if (std.mem.eql(u8, name, "move_before")) return u64MetricAsI64(metrics.move_before);
    if (std.mem.eql(u8, name, "set_text")) return u64MetricAsI64(metrics.set_text);
    if (std.mem.eql(u8, name, "set_value")) return u64MetricAsI64(metrics.set_value);
    if (std.mem.eql(u8, name, "set_checked")) return u64MetricAsI64(metrics.set_checked);
    if (std.mem.eql(u8, name, "set_disabled")) return u64MetricAsI64(metrics.set_disabled);
    if (std.mem.eql(u8, name, "set_metadata")) return u64MetricAsI64(metrics.set_metadata);
    if (std.mem.eql(u8, name, "bind_event")) return u64MetricAsI64(metrics.bind_event);
    if (std.mem.eql(u8, name, "allocs_this_event")) return u64MetricAsI64(metrics.allocs_this_event);
    if (std.mem.eql(u8, name, "deallocs_this_event")) return u64MetricAsI64(metrics.deallocs_this_event);
    if (std.mem.eql(u8, name, "events_processed")) return u64MetricAsI64(metrics.events_processed);
    if (std.mem.eql(u8, name, "nodes_recomputed")) return u64MetricAsI64(metrics.nodes_recomputed);
    if (std.mem.eql(u8, name, "propagation_prunes")) return u64MetricAsI64(metrics.propagation_prunes);
    if (std.mem.eql(u8, name, "derived_calls_into_roc")) return u64MetricAsI64(metrics.derived_calls_into_roc);
    if (std.mem.eql(u8, name, "each_key_compares")) return u64MetricAsI64(metrics.each_key_compares);
    if (std.mem.eql(u8, name, "recompute_batches")) return u64MetricAsI64(metrics.recompute_batches);
    if (std.mem.eql(u8, name, "patches_emitted")) return u64MetricAsI64(metrics.patches_emitted);
    if (std.mem.eql(u8, name, "scopes_created")) return u64MetricAsI64(metrics.scopes_created);
    if (std.mem.eql(u8, name, "scopes_disposed")) return u64MetricAsI64(metrics.scopes_disposed);
    if (std.mem.eql(u8, name, "rows_reused")) return u64MetricAsI64(metrics.rows_reused);
    if (std.mem.eql(u8, name, "rows_created")) return u64MetricAsI64(metrics.rows_created);
    if (std.mem.eql(u8, name, "rows_removed")) return u64MetricAsI64(metrics.rows_removed);
    if (std.mem.eql(u8, name, "closure_retains")) return u64MetricAsI64(metrics.closure_retains);
    if (std.mem.eql(u8, name, "closure_releases")) return u64MetricAsI64(metrics.closure_releases);
    if (std.mem.eql(u8, name, "stream_nodes_scanned")) return u64MetricAsI64(metrics.stream_nodes_scanned);
    if (std.mem.eql(u8, name, "retained_alloc_delta")) return metrics.retained_alloc_delta;
    return null;
}

fn setElementText(host: *HostEnv, elem: *DomElement, text: []const u8) void {
    setOwnedString(host.gpa.allocator(), &elem.text, text);
    elem.text_update_count += 1;
}

fn setElementValueIfChanged(host: *HostEnv, elem: *DomElement, value: []const u8) bool {
    if (replaceOwnedString(host.gpa.allocator(), &elem.value, value)) {
        elem.value_update_count += 1;
        return true;
    }
    return false;
}

fn setElementValue(host: *HostEnv, elem: *DomElement, value: []const u8) void {
    setOwnedString(host.gpa.allocator(), &elem.value, value);
    elem.value_update_count += 1;
}

fn clearElementText(host: *HostEnv, elem: *DomElement) void {
    clearOwnedString(host.gpa.allocator(), &elem.text);
    elem.text_update_count += 1;
}

fn clearElementValue(host: *HostEnv, elem: *DomElement) void {
    clearOwnedString(host.gpa.allocator(), &elem.value);
    elem.value_update_count += 1;
}

fn setElementCheckedIfChanged(elem: *DomElement, checked: bool) bool {
    if (elem.checked != checked) {
        elem.checked = checked;
        elem.checked_update_count += 1;
        return true;
    }
    return false;
}

fn setElementChecked(elem: *DomElement, checked: bool) void {
    elem.checked = checked;
    elem.checked_update_count += 1;
}

fn setElementDisabled(elem: *DomElement, disabled: bool) void {
    elem.disabled = disabled;
    elem.disabled_update_count += 1;
}

fn resetSimulatedDom(host: *HostEnv) void {
    const allocator = host.gpa.allocator();
    for (host.dom_elements.items) |*elem| {
        elem.deinit(allocator);
    }
    host.dom_elements.items.len = 0;
    host.engine.next_elem_id = 1;

    const root_tag = allocator.dupe(u8, "root") catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(0, root_tag)) catch {
        allocator.free(root_tag);
        std.process.exit(1);
    };
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
        .role => setOwnedString(host.gpa.allocator(), &elem.role, value),
        .label => setOwnedString(host.gpa.allocator(), &elem.label, value),
        .test_id => setOwnedString(host.gpa.allocator(), &elem.test_id, value),
        .value => setElementValue(host, elem, value),
    }
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
        .role => clearOwnedString(host.gpa.allocator(), &elem.role),
        .label => clearOwnedString(host.gpa.allocator(), &elem.label),
        .test_id => clearOwnedString(host.gpa.allocator(), &elem.test_id),
        .value => clearElementValue(host, elem),
    }
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

    const allocator = host.gpa.allocator();
    const tag_copy = allocator.dupe(u8, tag) catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(elem_id, tag_copy)) catch {
        allocator.free(tag_copy);
        std.process.exit(1);
    };
    host.engine.next_elem_id = elem_id + 1;
}

fn appendDomNode(host: *HostEnv, elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
    appendDetachedDomNode(host, elem_id, tag);
    const allocator = host.gpa.allocator();
    const parent = domElementById(host, parent_elem_id);
    const child = domElementById(host, elem_id);
    child.parent_id = parent.id;
    parent.children.append(allocator, child.id) catch std.process.exit(1);
}

fn findDomChildIndex(elem: *const DomElement, child_id: u64) ?usize {
    for (elem.children.items, 0..) |id, index| {
        if (id == child_id) return index;
    }
    return null;
}

fn ensureDomNode(host: *HostEnv, elem_id: u64, tag: []const u8) void {
    if (elem_id == 0) failHost("render descriptor cannot claim the host DOM root id");
    appendDetachedDomNode(host, elem_id, tag);
}

fn propagateDirtyActiveSignals(host: *HostEnv, roc_host: *abi.RocHost, allocator: std.mem.Allocator, dirty_source_node_ids: []const u64, dirty_generation: u64) []u64 {
    return host.engine.propagateDirtyActiveSignals(host, roc_host, allocator, dirty_source_node_ids, dirty_generation);
}

fn hostSignalRecordEqCallable(host: *HostEnv, record: *const HostSignalRecord) abi.RocErasedCallable {
    return host.engine.hostSignalRecordEqCallable(host, record);
}

fn hostSignalBindingEqCallable(host: *HostEnv, signal: *const HostSignalBinding) abi.RocErasedCallable {
    return hostSignalRecordEqCallable(host, signal.record);
}

fn hostSignalBindingDropCallable(host: *HostEnv, signal: *const HostSignalBinding) abi.RocErasedCallable {
    return host.engine.hostSignalBindingDropCallable(host, signal);
}

fn updateDirtySignalCache(host: *HostEnv, roc_host: *abi.RocHost, cache_slot: *HostSignalCacheSlot, value: HostValue) bool {
    return host.engine.updateDirtySignalCache(roc_host, cache_slot, value);
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

    const payload_value = hostValueStrTagged(host, roc_host, payload_text, task_payload.payload_tag);
    defer callErasedHostValueToUnit(roc_host, task_payload.payload_drop, payload_value);
    const next = if (failed)
        callErasedHostValueToHostValue(roc_host, task_payload.failed, payload_value)
    else
        callErasedHostValueToHostValue(roc_host, task_payload.done, payload_value);
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
    const elem = domElementById(host, elem_id);
    switch (kind) {
        .click => elem.bound_click_event = event_id,
        .input => elem.bound_input_event = event_id,
        .check => elem.bound_check_event = event_id,
    }
}

fn clearNodeEventKind(host: *HostEnv, elem_id: u64, kind: RenderEventKind) void {
    const elem = domElementById(host, elem_id);
    switch (kind) {
        .click => elem.bound_click_event = null,
        .input => elem.bound_input_event = null,
        .check => elem.bound_check_event = null,
    }
}

fn replaceDomChildrenForStructuralParentMoves(host: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64) void {
    replaceDomChildrenForStructuralParent(host, parent_elem_id, next_child_ids);
}

fn removeDomNode(host: *HostEnv, elem_id: u64) void {
    const allocator = host.gpa.allocator();
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
                _ = parent.children.orderedRemove(child_index);
            }
        }
    }
    elem.active = false;
    elem.parent_id = null;
    elem.bound_click_event = null;
    elem.bound_input_event = null;
    elem.bound_check_event = null;
    elem.children.deinit(allocator);
    elem.children = .empty;
}

fn replaceDomChildrenForStructuralParent(host: *HostEnv, parent_elem_id: u64, next_child_ids: []const u64) void {
    const allocator = host.gpa.allocator();
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
        child.parent_id = parent_elem_id;
    }

    parent.children.deinit(allocator);
    parent.children = .empty;
    parent.children.appendSlice(allocator, next_child_ids) catch std.process.exit(1);
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

fn hostValueStrTagged(host: *HostEnv, roc_host: *abi.RocHost, value: []const u8, tag: HostValueTypeTag) HostValue {
    const payload: *RocStr = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(RocStr), @alignOf(RocStr), true, roc_host)));
    payload.* = RocStr.fromSlice(value, roc_host);
    const host_value = host.storeHostValueWithRetainedTag(@ptrCast(payload), tag);
    if (builtin.is_test) host.setTestHostValueKind(host_value, .str);
    return host_value;
}

fn hostValueBool(host: *HostEnv, roc_host: *abi.RocHost, value: bool) HostValue {
    return hv.makeBool(host, roc_host, value);
}

fn hostValueI64(host: *HostEnv, roc_host: *abi.RocHost, value: i64) HostValue {
    return hv.makeI64(host, roc_host, value);
}

const ErasedHostValueUnaryArgs = erased_calls.ErasedHostValueUnaryArgs;
const ErasedHostValueBinaryArgs = erased_calls.ErasedHostValueBinaryArgs;

const callErasedHostValueToHostValue = erased_calls.callErasedHostValueToHostValue;

const callErasedHostValueHostValueToHostValue = erased_calls.callErasedHostValueHostValueToHostValue;

const callErasedHostValueHostValueToElem = erased_calls.callErasedHostValueHostValueToElem;

const callErasedHostValueHostValueToBool = erased_calls.callErasedHostValueHostValueToBool;

const callErasedHostValueToUnit = erased_calls.callErasedHostValueToUnit;

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
    metrics.events_processed = host.engine.dispatch_metrics.events_processed;
    metrics.recompute_batches = host.engine.dispatch_metrics.recompute_batches;
    host.engine.last_runtime_metrics = metrics;
    host.engine.pending_roc_metrics = zeroRuntimeMetrics();
}

fn applyDirtyStructuralSignalsLocally(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changes: []const HostDirtyStructuralSignal) CommandCounts {
    return host.engine.applyDirtyStructuralSignalsLocally(host, roc_host, dirty_source_node_ids, changes);
}

fn applyDirtyWhenStructuralSignals(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, changes: []const HostDirtyStructuralSignal) CommandCounts {
    return host.engine.applyDirtyWhenStructuralSignals(host, roc_host, dirty_source_node_ids, changes);
}

fn renderActiveRootMeasured(host: *HostEnv, roc_host: *abi.RocHost, dirty_source_node_ids: []const u64, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    const root = host.engine.root_elem orelse failHost("host render requested before Roc root Elem was initialized");

    var next_stream: HostNodeDescriptorStream = .{};
    errdefer next_stream.deinit(host.gpa.allocator(), roc_host, &host.engine.pending_roc_metrics);
    host.collectActiveElemRootDescriptors(roc_host, &next_stream, root, dirty_source_node_ids);

    const start_ns = benchmarkNowNs();
    const counts = if (!host.engine.hasRenderRoot())
        applyNodeDescriptorStream(host, roc_host, &next_stream)
    else
        applyStructuralNodeDescriptorStream(host, roc_host, &next_stream);
    const elapsed = benchmarkNowNs() - start_ns;
    if (apply_ns) |ns| ns.* += elapsed;
    if (command_counts) |total| total.addAll(counts);

    host.rebuildActiveEventsFromStream(&next_stream);
    host.engine.active_stream.deinit(host.gpa.allocator(), roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = next_stream;
    finishHostMetrics(host);
}

fn acceptInitElemMeasured(host: *HostEnv, roc_host: *abi.RocHost, root_box: ElemBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    if (host.engine.root_elem != null) failHost("Roc root Elem initialized more than once");
    const root = root_box.*;
    host.engine.root_elem = root;
    abi.decrefBoxWith(@ptrCast(root_box), @alignOf(abi.Elem), &dropMovedElemPayload, roc_host);
    renderActiveRootMeasured(host, roc_host, &.{}, apply_ns, command_counts);
}

fn acceptInitElem(host: *HostEnv, roc_host: *abi.RocHost, root_box: ElemBox) void {
    acceptInitElemMeasured(host, roc_host, root_box, null, null);
}

fn dispatchRocEventMeasured(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue, stats: ?*BenchmarkStats) void {
    const desc = hostEventById(host, event_id);
    validateEventPayloadKind(desc, payload_kind);
    host.setHostValueTypeTag(payload, desc.payload_tag);
    defer callErasedHostValueToUnit(roc_host, desc.payload_drop, payload);

    host.recordDispatch();

    var metrics = host.engine.pending_roc_metrics;
    metrics.bump(.nodes_recomputed, 1);
    metrics.bump(.derived_calls_into_roc, 1);
    host.engine.pending_roc_metrics = metrics;

    const start_ns = benchmarkNowNs();
    const current = host.stateValueByNodeId(desc.target_node_id);
    defer callErasedHostValueToUnit(roc_host, host.stateDropCallable(desc.target_node_id), current);
    const next = callErasedHostValueHostValueToHostValue(roc_host, desc.transform, current, payload);
    if (stats) |s| s.dispatch_roc_ns += benchmarkNowNs() - start_ns;

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
        const changed_record_ids = propagateDirtyActiveSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
        defer host.gpa.allocator().free(changed_record_ids);
        const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
        defer host.gpa.allocator().free(dirty_structural_signals);
        if (dirty_structural_signals.len != 0) {
            const apply_start_ns = benchmarkNowNs();
            const sink_counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            const counts = applyDirtyStructuralSignalsLocally(host, roc_host, &dirty_source_node_ids, dirty_structural_signals);
            s.dispatch_apply_ns += benchmarkNowNs() - apply_start_ns;
            s.commands.addAll(sink_counts);
            s.commands.addAll(counts);
            finishHostMetrics(host);
        } else {
            const apply_start_ns = benchmarkNowNs();
            const counts = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            s.dispatch_apply_ns += benchmarkNowNs() - apply_start_ns;
            s.commands.addAll(counts);
            finishHostMetrics(host);
        }
        s.actions += 1;
    } else {
        const dirty_source_node_ids = [_]u64{desc.target_node_id};
        const dirty_generation = host.nextDirtySignalGeneration();
        const changed_record_ids = propagateDirtyActiveSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
        defer host.gpa.allocator().free(changed_record_ids);
        const dirty_structural_signals = collectDirtyStructuralSignals(host, roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
        defer host.gpa.allocator().free(dirty_structural_signals);
        if (dirty_structural_signals.len != 0) {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            _ = applyDirtyStructuralSignalsLocally(host, roc_host, &dirty_source_node_ids, dirty_structural_signals);
            finishHostMetrics(host);
        } else {
            _ = applyDirtyRenderSinks(host, roc_host, &dirty_source_node_ids, changed_record_ids, dirty_generation);
            finishHostMetrics(host);
        }
    }
}

fn dispatchRocEvent(host: *HostEnv, roc_host: *abi.RocHost, event_id: u64, payload_kind: EventPayloadKind, payload: HostValue) void {
    dispatchRocEventMeasured(host, roc_host, event_id, payload_kind, payload, null);
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

fn commandIsAction(cmd: SpecCommand) bool {
    return switch (cmd.cmd_type) {
        .click, .fill, .check, .uncheck, .resolve_task, .reject_task, .tick_interval => true,
        else => false,
    };
}

fn runActionCommandMeasured(host: *HostEnv, roc_host: *abi.RocHost, cmd: SpecCommand, stats: *BenchmarkStats) void {
    switch (cmd.cmd_type) {
        .click => {
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark click locator did not resolve");
            if (elem.disabled) failHost("benchmark click target is disabled");
            const event_id = elem.bound_click_event orelse failHost("benchmark click target has no binding");
            dispatchRocEventMeasured(host, roc_host, event_id, .unit, hostValueUnit(host, roc_host), stats);
        },

        .fill => {
            const value = cmd.expected_text orelse "";
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark fill locator did not resolve");
            if (elem.disabled) failHost("benchmark fill target is disabled");
            if (elem.bound_input_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, event_id, .str, hostValueStr(host, roc_host, value), stats);
            } else {
                _ = setElementValueIfChanged(host, elem, value);
            }
        },

        .check, .uncheck => {
            const checked = cmd.cmd_type == .check;
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark check locator did not resolve");
            if (elem.disabled) failHost("benchmark check target is disabled");
            if (elem.bound_check_event) |event_id| {
                dispatchRocEventMeasured(host, roc_host, event_id, .bool, hostValueBool(host, roc_host, checked), stats);
            } else {
                _ = setElementCheckedIfChanged(elem, checked);
            }
        },

        .resolve_task, .reject_task => {
            const task_name = cmd.task_name orelse failHost("benchmark task command had no task name");
            const payload = cmd.expected_text orelse "";
            const start_ns = benchmarkNowNs();
            const counts = resolvePendingTask(host, roc_host, task_name, payload, cmd.cmd_type == .reject_task);
            stats.dispatch_apply_ns += benchmarkNowNs() - start_ns;
            stats.commands.addAll(counts);
            finishHostMetrics(host);
            stats.actions += 1;
        },

        .tick_interval => {
            const period_ms = cmd.interval_ms orelse failHost("benchmark interval command had no period");
            const start_ns = benchmarkNowNs();
            const counts = tickIntervalSource(host, roc_host, period_ms);
            stats.dispatch_apply_ns += benchmarkNowNs() - start_ns;
            stats.commands.addAll(counts);
            finishHostMetrics(host);
            stats.actions += 1;
        },

        else => {},
    }
}

fn runBenchmarkIteration(commands: []const SpecCommand, verbose: bool, stats: *BenchmarkStats) void {
    var host_env = HostEnv.init();
    host_env.test_state.verbose = verbose;

    var roc_host = makeSignalsRocHost(&host_env);
    host_env.engine.roc_host = &roc_host;
    current_host = &host_env;
    current_roc_host = &roc_host;

    const init_start_ns = benchmarkNowNs();
    const init_result = abi.roc_ui_init();
    stats.init_roc_ns += benchmarkNowNs() - init_start_ns;
    acceptInitElemMeasured(&host_env, &roc_host, init_result, &stats.init_apply_ns, &stats.commands);

    for (commands) |cmd| {
        if (commandIsAction(cmd)) {
            runActionCommandMeasured(&host_env, &roc_host, cmd, stats);
        }
    }

    const retained_delta = @as(i64, @intCast(host_env.alloc_count)) - @as(i64, @intCast(host_env.dealloc_count));
    var iteration_metrics = host_env.engine.last_runtime_metrics;
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
    writeStdout("case,sample,iterations,actions,init_roc_ns,init_apply_ns,dispatch_roc_ns,dispatch_apply_ns,total_ns,allocs,deallocs,retained_alloc_delta,commands,reset_dom,create_element,append_child,remove_node,move_before,set_text,set_value,set_checked,set_disabled,set_metadata,bind_event,active_graph_records_rebuilt,stream_nodes_scanned,each_key_compares,allocs_this_event,deallocs_this_event,events_processed,nodes_recomputed,propagation_prunes,derived_calls_into_roc,recompute_batches,patches_emitted,scopes_created,scopes_disposed,rows_reused,rows_created,rows_removed,closure_retains,closure_releases,metrics_retained_alloc_delta\n");
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
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            stats.commands.total,
            stats.commands.reset_dom,
            stats.commands.create_element,
            stats.commands.append_child,
            stats.commands.remove_node,
            stats.commands.move_before,
            stats.commands.set_text,
            stats.commands.set_value,
            stats.commands.set_checked,
            stats.commands.set_disabled,
            stats.commands.set_metadata,
            stats.commands.bind_event,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            stats.metrics.active_graph_records_rebuilt,
            stats.metrics.stream_nodes_scanned,
            stats.metrics.each_key_compares,
            stats.metrics.allocs_this_event,
            stats.metrics.deallocs_this_event,
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

fn runAppBenchmarks(spec_file: []const u8, case_name: []const u8, iterations: usize, samples: usize, verbose: bool) error{}!c_int {
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
        @export(&hostValueClone, .{ .name = "roc_host_value_clone", .visibility = .hidden });
        @export(&hostValueGet, .{ .name = "roc_host_value_get", .visibility = .hidden });
        @export(&hostValueGetTagged, .{ .name = "roc_host_value_get_tagged", .visibility = .hidden });
        @export(&hostValueStore, .{ .name = "roc_host_value_store", .visibility = .hidden });
        @export(&hostValueStoreTagged, .{ .name = "roc_host_value_store_tagged", .visibility = .hidden });
        @export(&hostValueTake, .{ .name = "roc_host_value_take", .visibility = .hidden });
        @export(&hostValueTakeTagged, .{ .name = "roc_host_value_take_tagged", .visibility = .hidden });

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

    for (host_env.test_state.commands) |cmd| {
        switch (cmd.cmd_type) {
            .mark_metrics => {
                host_env.test_state.metrics_mark = host_env.engine.last_runtime_metrics;
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
                dispatchRocEvent(&host_env, &roc_host, event_id, .unit, hostValueUnit(&host_env, &roc_host));
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
                    dispatchRocEvent(&host_env, &roc_host, event_id, .str, hostValueStr(&host_env, &roc_host, value));
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
                    dispatchRocEvent(&host_env, &roc_host, event_id, .bool, hostValueBool(&host_env, &roc_host, checked));
                } else {
                    _ = setElementCheckedIfChanged(elem, checked);
                }
            },

            .resolve_task, .reject_task => {
                const task_name = cmd.task_name orelse {
                    writeLocatorFailure(cmd.line_num, "task command had no task name");
                    return 1;
                };
                const payload = cmd.expected_text orelse "";
                _ = resolvePendingTask(&host_env, &roc_host, task_name, payload, cmd.cmd_type == .reject_task);
                finishHostMetrics(&host_env);
            },

            .tick_interval => {
                const period_ms = cmd.interval_ms orelse {
                    writeLocatorFailure(cmd.line_num, "interval command had no period");
                    return 1;
                };
                _ = tickIntervalSource(&host_env, &roc_host, period_ms);
                finishHostMetrics(&host_env);
            },

            .expect_visible => {
                _ = host_env.findElementByLocator(cmd.locator, cmd.line_num) orelse {
                    writeLocatorFailure(cmd.line_num, "locator did not resolve to one visible element");
                    return 1;
                };
            },

            .expect_absent => {
                const match_count = host_env.countElementsByLocator(cmd.locator);
                if (match_count != 0) {
                    writeAbsentFailure(cmd.line_num, match_count);
                    return 1;
                }
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

            .expect_cleanup => {
                const name = cmd.task_name orelse "";
                const expected = cmd.expected_count orelse 0;
                const actual = host_env.engine.cleanupEventCount(name);
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected cleanup \"{s}\": {d}\n  Got cleanup count:       {d}\n", .{ cmd.line_num, name, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_pending_task => {
                const name = cmd.task_name orelse "";
                const expected = cmd.expected_count orelse 0;
                const actual = host_env.engine.pendingTaskCountByName(name);
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected pending task \"{s}\": {d}\n  Got pending task count:       {d}\n", .{ cmd.line_num, name, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_interval => {
                const period_ms = cmd.interval_ms orelse 0;
                const expected = cmd.expected_count orelse 0;
                const actual = host_env.engine.activeIntervalRecordCountByPeriod(period_ms);
                if (actual != expected) {
                    var buf: [512]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected active interval {d}ms: {d}\n  Got active interval count:   {d}\n", .{ cmd.line_num, period_ms, expected, actual }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_metric_delta => {
                const metric_name = cmd.expected_text orelse "";
                const expected = cmd.expected_metric_delta orelse 0;
                const marked = host_env.test_state.metrics_mark orelse {
                    writeMetricFailure(cmd.line_num, "mark_metrics must run before expect_metric_delta");
                    return 1;
                };
                const start = runtimeMetricValue(marked, metric_name) orelse {
                    writeUnknownMetric(cmd.line_num, metric_name);
                    return 1;
                };
                const current = runtimeMetricValue(host_env.engine.last_runtime_metrics, metric_name) orelse {
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

fn writeAbsentFailure(line_num: usize, match_count: usize) void {
    var buf: [256]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: expected no matching elements, found {d}\n", .{ line_num, match_count }) catch "TEST FAILED\n";
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

fn appendTestSignalDescriptor(host: *HostEnv, signal_id: u64, kind: SignalKind, source_event_ids: []const u64, input_signal_ids: []const u64) std.mem.Allocator.Error!void {
    if (!builtin.is_test) @compileError("appendTestSignalDescriptor is test-only");

    const allocator = host.gpa.allocator();
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

    const allocator = host.gpa.allocator();
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

    const allocator = host.gpa.allocator();
    host.clearScopes();
    host.engine.scopes.deinit(allocator);
    host.engine.node_identities.deinit(allocator);
    host.engine.dom_identities.deinit(allocator);
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
    const allocator = host.gpa.allocator();

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
    left.nodes_recomputed = 5;
    left.propagation_prunes = 3;
    left.derived_calls_into_roc = 4;
    left.each_key_compares = 6;
    left.recompute_batches = 2;
    left.patches_emitted = 7;
    left.create_element = 2;
    left.append_child = 3;
    left.remove_node = 4;
    left.move_before = 5;
    left.set_text = 1;
    left.bind_event = 1;
    left.stream_nodes_scanned = 12;

    var right = zeroRuntimeMetrics();
    right.active_graph_records_rebuilt = 2;
    right.allocs_this_event = 4;
    right.deallocs_this_event = 5;
    right.events_processed = 1;
    right.nodes_recomputed = 8;
    right.propagation_prunes = 11;
    right.derived_calls_into_roc = 6;
    right.each_key_compares = 7;
    right.recompute_batches = 1;
    right.patches_emitted = 13;
    right.create_element = 5;
    right.append_child = 8;
    right.remove_node = 10;
    right.move_before = 12;
    right.set_text = 2;
    right.bind_event = 4;
    right.stream_nodes_scanned = 5;
    right.retained_alloc_delta = -2;

    const total = addRuntimeMetrics(left, right);
    try std.testing.expectEqual(@as(u64, 9), total.active_graph_records_rebuilt);
    try std.testing.expectEqual(@as(u64, 13), total.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 11), total.deallocs_this_event);
    try std.testing.expectEqual(@as(u64, 3), total.events_processed);
    try std.testing.expectEqual(@as(u64, 13), total.nodes_recomputed);
    try std.testing.expectEqual(@as(u64, 14), total.propagation_prunes);
    try std.testing.expectEqual(@as(u64, 10), total.derived_calls_into_roc);
    try std.testing.expectEqual(@as(u64, 13), total.each_key_compares);
    try std.testing.expectEqual(@as(u64, 3), total.recompute_batches);
    try std.testing.expectEqual(@as(u64, 20), total.patches_emitted);
    try std.testing.expectEqual(@as(u64, 7), total.create_element);
    try std.testing.expectEqual(@as(u64, 11), total.append_child);
    try std.testing.expectEqual(@as(u64, 14), total.remove_node);
    try std.testing.expectEqual(@as(u64, 17), total.move_before);
    try std.testing.expectEqual(@as(u64, 3), total.set_text);
    try std.testing.expectEqual(@as(u64, 5), total.bind_event);
    try std.testing.expectEqual(@as(u64, 17), total.stream_nodes_scanned);
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

test "signals host allocation ledger updates moved header indexes" {
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

    try std.testing.expectEqual(@as(usize, 3), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(usize, 2), rocAllocationHeaderFromUserPtr(last).ledger_index);

    rocDeallocFn(&roc_host, middle, 8);

    try std.testing.expectEqual(@as(usize, 2), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(usize, 1), rocAllocationHeaderFromUserPtr(last).ledger_index);
    try std.testing.expectEqual(@as(u64, 3), host.engine.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 1), host.engine.pending_roc_metrics.deallocs_this_event);

    const grown = rocReallocFn(&roc_host, first, 32, 8) orelse return error.OutOfMemory;

    try std.testing.expectEqual(@as(usize, 2), host.roc_allocations.items.len);
    try std.testing.expectEqual(@as(u64, 4), host.alloc_count);
    try std.testing.expectEqual(@as(u64, 2), host.dealloc_count);
    try std.testing.expectEqual(@as(u64, 4), host.engine.pending_roc_metrics.allocs_this_event);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.deallocs_this_event);

    rocDeallocFn(&roc_host, last, 8);
    rocDeallocFn(&roc_host, grown, 8);

    try std.testing.expectEqual(@as(usize, 0), host.roc_allocations.items.len);
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
};

const TestErasedHostValueCapture = extern struct {
    value: HostValue,
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

fn testHostValueUnit() HostValue {
    const host = currentHost();
    return hostValueUnit(host, testCurrentRocHost());
}

fn testHostValueStr(roc_host: *abi.RocHost, value: []const u8) HostValue {
    return hostValueStr(hostFromRocHost(roc_host), roc_host, value);
}

fn testHostValueBool(value: bool) HostValue {
    const host = currentHost();
    return hostValueBool(host, testCurrentRocHost(), value);
}

fn testHostValueI64(value: i64) HostValue {
    const host = currentHost();
    return hostValueI64(host, testCurrentRocHost(), value);
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

fn testDropRocStrBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *RocStr = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropI64ListBoxPayload(data_ptr: ?*anyopaque, roc_host: *abi.RocHost) callconv(.c) void {
    const payload: *I64List = @ptrCast(@alignCast(data_ptr orelse return));
    payload.*.decref(roc_host);
}

fn testDropHostValue(roc_host: *abi.RocHost, value: HostValue) void {
    const host = hostFromRocHost(roc_host);
    const kind = host.testHostValueKind(value);
    const box = host.takeHostValue(value);
    switch (kind) {
        .unit, .i64, .bool => abi.decrefBox(box, roc_host),
        .str => abi.decrefBoxWith(box, @alignOf(RocStr), &testDropRocStrBoxPayload, roc_host),
        .i64_list => abi.decrefBoxWith(box, @alignOf(I64List), &testDropI64ListBoxPayload, roc_host),
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

fn testHostValueEqCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
}

fn testHostValueHashCallable(roc_host: *abi.RocHost) abi.RocErasedCallable {
    return writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueHashErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
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
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, input + capture.amount));
}

fn testHostValueHashErasedCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    const value = testReadHostValueI64(roc_host, call_args.arg0);
    writeTestErasedResult(u64, ret, @intCast(value));
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
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, left + right + capture.amount));
}

fn testUnitIncrementHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueBinaryArgs, args);
    const current = testReadHostValueI64(roc_host, call_args.arg0);
    if (hostFromRocHost(roc_host).testHostValueKind(call_args.arg1) != .unit) @panic("test unit event callable expected unit payload");
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, current + 1));
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
                .read = testReadBoolCallable(roc_host),
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
    };
    writeTestErasedResult(bool, ret, is_equal);
}

fn testStableStrHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(HostValue, ret, hostValueStr(hostFromRocHost(roc_host), roc_host, "stable"));
}

fn testStableI64HostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    const capture = testCapturePtrAs(TestErasedI64Capture, capture_ptr);
    writeTestErasedResult(HostValue, ret, hostValueI64(hostFromRocHost(roc_host), roc_host, capture.amount));
}

fn testStableBoolHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = args;
    _ = capture_ptr;
    writeTestErasedResult(HostValue, ret, hostValueBool(hostFromRocHost(roc_host), roc_host, true));
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

fn testErasedCallableOnDrop(_: ?[*]u8, _: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
}

fn testBinderCaptureOnDrop(capture_ptr: ?[*]u8, roc_host: *abi.RocHost) callconv(.c) void {
    test_erased_callable_drop_count += 1;
    const capture = testCapturePtrAs(TestErasedBinderCapture, capture_ptr);
    abi.decrefBox(@ptrCast(capture.condition_binder), roc_host);
}

fn testDropHostValueCallable(roc_host: *abi.RocHost, ret: ?[*]u8, args: ?[*]const u8, capture_ptr: ?[*]u8) callconv(.c) void {
    _ = ret;
    _ = capture_ptr;
    const call_args = testErasedArgsAs(ErasedHostValueUnaryArgs, args);
    testDropHostValue(roc_host, call_args.arg0);
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
            dest[index] = hostValueI64(host, roc_host, item);
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

    try std.testing.expectEqual(@as(u64, 4), test_erased_callable_drop_count);
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

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const initial_keys = [_]HostValue{ testHostValueI64(10), testHostValueI64(11) };
    const initial_rows = syncTestEachRowScopes(&host, &roc_host, root, 7, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial_rows);
    const row_a = initial_rows.scope_ids[0];
    const row_b = initial_rows.scope_ids[1];
    try std.testing.expect(row_b != row_a);

    const same_keys = [_]HostValue{testHostValueI64(10)};
    const same_rows = syncTestEachRowScopes(&host, &roc_host, root, 7, &same_keys, &same_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, same_rows);
    try std.testing.expectEqual(row_a, same_rows.scope_ids[0]);

    const other_site_keys = [_]HostValue{testHostValueI64(10)};
    const other_site_rows = syncTestEachRowScopes(&host, &roc_host, root, 8, &other_site_keys, &other_site_keys, key_eq, key_eq);
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

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();
    const row = createTestEachRowScope(&host, &roc_host, root, 3, testHostValueI64(10), testHostValueI64(10), key_eq, key_eq);
    const branch = host.internWhenBranchScope(row, 4, .true_branch);
    const row_state = host.internNodeIdentity(row, 0);
    const branch_state = host.internNodeIdentity(branch, 0);

    host.disposeScopeSubtree(&roc_host, row);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(!host.engine.scopes.items[@intCast(row)].active);
    try std.testing.expect(!host.engine.scopes.items[@intCast(branch)].active);
    try std.testing.expect(!host.engine.node_identities.items[@intCast(row_state)].active);
    try std.testing.expect(!host.engine.node_identities.items[@intCast(branch_state)].active);

    const recreated_row = createTestEachRowScope(&host, &roc_host, root, 3, testHostValueI64(10), testHostValueI64(10), key_eq, key_eq);
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
    const root = testNodeStateWithTokenAndInitial(
        &roc_host,
        state_token,
        testHostValueStr(&roc_host, "first"),
        testNodeTextSignal(&roc_host, testNodeRefExpr(state_token)),
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
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
    const unchanged_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, unchanged_generation);
    defer host.gpa.allocator().free(unchanged_record_ids);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
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
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    var binding = host.bindNodeSignal(host.gpa.allocator(), &stream, combine, &.{});
    defer binding.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    var cache: HostSignalCacheSlot = .absent;
    cache.replace(&roc_host, &host.engine.pending_roc_metrics, testHostValueI64List(&roc_host, &initial_items), hostSignalBindingEqCallable(&host, &binding), hostSignalBindingDropCallable(&host, &binding));
    defer cache.deinit(&roc_host, &host.engine.pending_roc_metrics);
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
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, testNodeRefExpr(state_token)),
                .read = testReadBoolCallable(&roc_host),
                .when_false = boxTestElem(&roc_host, testNodeText(&roc_host, "false branch")),
                .when_true = boxTestElem(&roc_host, testNodeText(&roc_host, "true branch")),
            },
        },
        .tag = .When,
    };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueBool(true), when_elem);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);
    try std.testing.expectEqual(HostScopeBranch.false_branch, dirty_structural_signals[0].branch.?);

    const patch_counts = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.append_child);
    try std.testing.expect(activeTextElementId(&host, "true branch") == null);
    try std.testing.expect(activeTextElementId(&host, "false branch") != null);
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
    const when_elem: abi.Elem = .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(&roc_host, condition),
                .read = testReadBoolCallable(&roc_host),
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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
    const each = testNodeEachWithSignalAndRow(&roc_host, testNodeRefExpr(state_token), &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    var initial_items: [row_count]HostValue = undefined;
    for (&initial_items, 0..) |*item, index| {
        item.* = testHostValueI64(@intCast(index + 1));
    }
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.engine.pending_roc_metrics.rows_reused;
    const rows_created_start = host.engine.pending_roc_metrics.rows_created;
    const rows_removed_start = host.engine.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.engine.render_metrics.patches_emitted;
    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

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
    const each = testNodeEachWithSignalAndRow(&roc_host, testNodeRefExpr(state_token), &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.engine.pending_roc_metrics.rows_reused;
    const rows_created_start = host.engine.pending_roc_metrics.rows_created;
    const rows_removed_start = host.engine.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.engine.render_metrics.patches_emitted;
    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

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
    const each = testNodeEachWithSignalAndRow(&roc_host, testNodeRefExpr(state_token), &testStatefulRowElemCallable);
    const children = [_]abi.Elem{each};
    const section = testElementWith(&roc_host, "section", &.{}, &children);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueI64List(&roc_host, &initial_items), section);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);

    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.each, dirty_structural_signals[0].kind);

    const rows_reused_start = host.engine.pending_roc_metrics.rows_reused;
    const rows_created_start = host.engine.pending_roc_metrics.rows_created;
    const rows_removed_start = host.engine.pending_roc_metrics.rows_removed;
    const row_call_start = test_row_elem_call_count;
    const patch_start = host.engine.render_metrics.patches_emitted;
    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;

    const patch_counts = applyDirtyStructuralSignalsLocally(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

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
    const items = [_]HostValue{testHostValueI64(1)};
    const children = [_]abi.Elem{
        testNodeEachWithNestedWhenRows(&roc_host, &items, state_token),
    };
    const section = testElementWith(&roc_host, "section", &.{}, &children);
    const root = testNodeStateWithTokenAndInitial(&roc_host, state_token, testHostValueBool(true), section);
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
    const changed_record_ids = propagateDirtyActiveSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, dirty_generation);
    defer host.gpa.allocator().free(changed_record_ids);
    const dirty_structural_signals = collectDirtyStructuralSignals(&host, &roc_host, host.gpa.allocator(), &dirty_source_node_ids, changed_record_ids, dirty_generation);
    defer host.gpa.allocator().free(dirty_structural_signals);
    try std.testing.expectEqual(@as(usize, 1), dirty_structural_signals.len);
    try std.testing.expectEqual(HostActiveStructuralSignalKind.when, dirty_structural_signals[0].kind);

    const graph_rebuild_start = host.engine.pending_roc_metrics.active_graph_records_rebuilt;
    _ = applyDirtyWhenStructuralSignals(&host, &roc_host, &dirty_source_node_ids, dirty_structural_signals);

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
    try std.testing.expect(host.dom_elements.items[@intCast(section_id)].disabled);

    const next_root = testElementWith(&roc_host, "section", &.{}, &.{});
    defer abi.decrefElem(next_root, &roc_host);

    var next_stream: HostNodeDescriptorStream = .{};
    host.collectActiveElemRootDescriptors(&roc_host, &next_stream, next_root, &.{});
    const patch_counts = applyStructuralNodeDescriptorStream(&host, &roc_host, &next_stream);
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = next_stream;

    try std.testing.expectEqual(@as(u64, 0), patch_counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 0), patch_counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_metadata);
    try std.testing.expectEqual(@as(u64, 1), patch_counts.set_disabled);
    try std.testing.expect(host.dom_elements.items[@intCast(section_id)].label == null);
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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.engine.active_stream = same_reordered_stream;

    try std.testing.expectEqual(@as(u64, 0), same_reordered_counts.create_element);
    try std.testing.expectEqual(@as(u64, 0), same_reordered_counts.bind_event);
    try std.testing.expectEqual(@as(?u64, 2), host.dom_elements.items[@intCast(row_1_button_id)].bound_click_event);
    try std.testing.expectEqual(@as(?u64, 1), host.dom_elements.items[@intCast(row_2_button_id)].bound_click_event);
}

fn freeKeyedRowDiff(host: *HostEnv, diff: HostKeyedRowDiffResult) void {
    diff.deinit(host.gpa.allocator());
}

fn syncTestEachRowScopes(host: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, keys: []const HostValue, items: []const HostValue, key_eq: abi.RocErasedCallable, item_eq: abi.RocErasedCallable) HostKeyedRowDiffResult {
    const allocator = host.gpa.allocator();
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

    const key_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(key_drop, roc_host);
    const key_hash = testHostValueHashCallable(roc_host);
    defer abi.decrefErasedCallable(key_hash, roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(item_drop, roc_host);
    return host.syncEachRowScopes(roc_host, parent_scope_id, site_ordinal, key_values, item_values, key_hash, key_eq, key_drop, item_eq, item_drop);
}

fn createTestEachRowScope(host: *HostEnv, roc_host: *abi.RocHost, parent_scope_id: u64, site_ordinal: u64, key: HostValue, item: HostValue, key_eq: abi.RocErasedCallable, item_eq: abi.RocErasedCallable) u64 {
    const key_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(key_drop, roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    defer abi.decrefErasedCallable(item_drop, roc_host);
    return host.createEachRowScope(parent_scope_id, site_ordinal, key, item, key_eq, key_drop, item_eq, item_drop);
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

fn newTestHostValueTypeTag(roc_host: *abi.RocHost) *u64 {
    const tag: *u64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(u64), @alignOf(u64), false, roc_host)));
    tag.* = 0;
    return tag;
}

fn testNodeConstExpr(roc_host: *abi.RocHost, value: HostValue) abi.NodeSignalExpr {
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{ .const_value = .{
            ._0 = newTestSignalToken(roc_host),
            ._1 = testHostValueInitialThunk(roc_host, value),
            ._2 = eq,
            ._3 = drop,
        } },
        .tag = .ConstValue,
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
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
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
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
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
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
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
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .map = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = boxTestNodeSignalExpr(roc_host, input),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
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
    const eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testAlwaysEqualHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .combine = .{
                ._0 = newTestSignalToken(roc_host),
                ._1 = abi.RocList(abi.NodeSignalExpr).fromSlice(children, roc_host),
                ._2 = transform,
                ._3 = eq,
                ._4 = drop,
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
    return .{
        .payload = .{ .text_signal = .{
            .read = testReadStrCallable(roc_host),
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
                .read = testReadStrCallable(roc_host),
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
                .read = testReadBoolCallable(roc_host),
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
    const payload_drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_bool_tag = newTestHostValueTypeTag(roc_host),
                    .payload_accessor = @intFromEnum(testPayloadAccessorForKind(payload_kind)),
                    .payload_drop = payload_drop,
                    .payload_kind = @intFromEnum(payload_kind),
                    .payload_str_tag = newTestHostValueTypeTag(roc_host),
                    .payload_unit_tag = newTestHostValueTypeTag(roc_host),
                    .transform = transform,
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
    const payload_drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .on_event = .{
                .kind = @intFromEnum(kind),
                .msg = .{
                    .binder = cloneTestBinderToken(binder_token),
                    .payload_bool_tag = newTestHostValueTypeTag(roc_host),
                    .payload_accessor = @intFromEnum(EventPayloadAccessor.none),
                    .payload_drop = payload_drop,
                    .payload_kind = @intFromEnum(EventPayloadKind.unit),
                    .payload_str_tag = newTestHostValueTypeTag(roc_host),
                    .payload_unit_tag = newTestHostValueTypeTag(roc_host),
                    .transform = transform,
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
    const initial_thunk = testHostValueInitialThunk(roc_host, initial);
    const eq = testHostValueEqCallable(roc_host);
    const drop = testHostValueDropCallable(roc_host);
    return .{
        .payload = .{
            .state = .{
                .binder = binder_token,
                .child = boxTestElem(roc_host, child),
                .drop = drop,
                .eq = eq,
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
    return .{
        .payload = .{
            .when = .{
                .condition = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, testHostValueBool(true))),
                .read = testReadBoolCallable(roc_host),
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
    return host_value;
}

fn testNodeEachWithSignalAndRow(roc_host: *abi.RocHost, signal: abi.NodeSignalExpr, row_fn: abi.RocErasedCallableFn) abi.Elem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_hash = testHostValueHashCallable(roc_host);
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_drop = testHostValueDropCallable(roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
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
                .items_to_values = items_to_values,
                .key_hash = key_hash,
                .key_drop = key_drop,
                .key_eq = key_eq,
                .key_of = key_of,
                .item_drop = item_drop,
                .item_eq = item_eq,
                .row = row,
            },
        },
        .tag = .Each,
    };
}

fn testNodeEachWithItemsAndRow(roc_host: *abi.RocHost, items: []const HostValue, row_fn: abi.RocErasedCallableFn) abi.Elem {
    return testNodeEachWithSignalAndRow(roc_host, testNodeConstExpr(roc_host, testHostValueI64List(roc_host, items)), row_fn);
}

fn testNodeEachWithNestedWhenRows(roc_host: *abi.RocHost, items: []const HostValue, condition_binder: HostBinderToken) abi.Elem {
    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_of = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testUnaryHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_hash = testHostValueHashCallable(roc_host);
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const key_drop = testHostValueDropCallable(roc_host);
    const item_drop = testHostValueDropCallable(roc_host);
    const items_to_values = testItemsToValuesCallable(roc_host);
    const row = writeTestErasedCallable(
        TestErasedBinderCapture,
        roc_host,
        &testNestedWhenRowElemCallable,
        &testBinderCaptureOnDrop,
        .{ .condition_binder = cloneTestBinderToken(condition_binder) },
    );
    return .{
        .payload = .{
            .each = .{
                .items = boxTestNodeSignalExpr(roc_host, testNodeConstExpr(roc_host, testHostValueI64List(roc_host, items))),
                .items_to_values = items_to_values,
                .key_hash = key_hash,
                .key_drop = key_drop,
                .key_eq = key_eq,
                .key_of = key_of,
                .item_drop = item_drop,
                .item_eq = item_eq,
                .row = row,
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

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();

    const initial_keys = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_updated);

    const state_for_key_2 = host.internNodeIdentity(initial.scope_ids[1], 0);

    const reordered_keys = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered = syncTestEachRowScopes(&host, &roc_host, root, 5, &reordered_keys, &reordered_keys, key_eq, key_eq);
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
    const changed = syncTestEachRowScopes(&host, &roc_host, root, 5, &changed_keys, &changed_items, key_eq, key_eq);
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
    const reappeared = syncTestEachRowScopes(&host, &roc_host, root, 5, &reappeared_keys, &reappeared_keys, key_eq, key_eq);
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

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(key_eq, &roc_host);

    const root = host.internRootScope();
    const row_count = 64;

    var initial_keys: [row_count]HostValue = undefined;
    for (&initial_keys, 0..) |*key, index| {
        key.* = testHostValueI64(@intCast(index + 1));
    }
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, row_count), initial.rows_created);

    const compare_start = host.engine.pending_roc_metrics.each_key_compares;

    var reordered_keys: [row_count]HostValue = undefined;
    for (&reordered_keys, 0..) |*key, index| {
        key.* = testHostValueI64(@intCast(row_count - index));
    }
    const reordered = syncTestEachRowScopes(&host, &roc_host, root, 5, &reordered_keys, &reordered_keys, key_eq, key_eq);
    defer freeKeyedRowDiff(&host, reordered);
    try std.testing.expectEqual(@as(u64, row_count), reordered.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_created);
    try std.testing.expectEqual(@as(u64, 0), reordered.rows_removed);

    const compare_delta = host.engine.pending_roc_metrics.each_key_compares - compare_start;
    try std.testing.expectEqual(@as(u64, row_count * 3), compare_delta);
}

test "signals host row scopes retain key and item equality thunks" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    const item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testHostValueEqErasedCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );

    const root = host.internRootScope();
    const initial_keys = [_]HostValue{testHostValueI64(1)};
    const initial_items = [_]HostValue{testHostValueI64(10)};
    const initial = syncTestEachRowScopes(&host, &roc_host, root, 5, &initial_keys, &initial_items, key_eq, item_eq);
    defer freeKeyedRowDiff(&host, initial);
    try std.testing.expectEqual(@as(u64, 1), initial.rows_created);
    const row_scope_id = initial.scope_ids[0];

    test_erased_callable_drop_count = 0;
    abi.decrefErasedCallable(key_eq, &roc_host);
    abi.decrefErasedCallable(item_eq, &roc_host);
    try std.testing.expectEqual(@as(u64, 0), test_erased_callable_drop_count);

    const incoming_key_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNeverEqualHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(incoming_key_eq, &roc_host);
    const incoming_item_eq = writeTestErasedCallable(
        TestErasedI64Capture,
        &roc_host,
        &testNeverEqualHostValueCallable,
        &testErasedCallableOnDrop,
        .{ .amount = 0 },
    );
    defer abi.decrefErasedCallable(incoming_item_eq, &roc_host);

    const next_keys = [_]HostValue{testHostValueI64(1)};
    const next_items = [_]HostValue{testHostValueI64(10)};
    const next = syncTestEachRowScopes(&host, &roc_host, root, 5, &next_keys, &next_items, incoming_key_eq, incoming_item_eq);
    defer freeKeyedRowDiff(&host, next);
    try std.testing.expectEqual(@as(u64, 1), next.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), next.rows_created);
    try std.testing.expectEqual(@as(u64, 1), next.row_items_unchanged);
    try std.testing.expectEqual(row_scope_id, next.scope_ids[0]);
}

test "signals host walks Elem identity-bearing sites only" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const nested_state = testNodeState(&roc_host, testNodeText(&roc_host, "nested"));
    const branch_true = testNodeState(&roc_host, testNodeText(&roc_host, "true"));
    const branch_false = testNodeState(&roc_host, testNodeText(&roc_host, "false"));
    const when_elem = testNodeWhen(&roc_host, branch_true, branch_false);
    const each_elem = testNodeEach(&roc_host);
    const nested_children = [_]abi.Elem{
        testNodeText(&roc_host, "ordinary text"),
        when_elem,
        each_elem,
    };
    const nested_element = testElement(&roc_host, &nested_children);
    const root_children = [_]abi.Elem{
        testNodeText(&roc_host, "root text"),
        nested_state,
        nested_element,
    };
    const root = testElement(&roc_host, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.walkElemRootIdentitySites(root);
    try std.testing.expectEqual(@as(usize, 3), host.engine.node_identities.items.len);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[0].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[0].ordinal);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[1].scope_id);
    try std.testing.expectEqual(@as(u64, 1), host.engine.node_identities.items[1].ordinal);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[2].scope_id);
    try std.testing.expectEqual(@as(u64, 2), host.engine.node_identities.items[2].ordinal);

    host.walkElemRootIdentitySites(root);
    try std.testing.expectEqual(@as(usize, 3), host.engine.node_identities.items.len);

    const true_scope = host.walkElemWhenBranchIdentitySites(0, 1, .true_branch, when_elem.payload.when.when_true.*);
    try std.testing.expectEqual(@as(usize, 4), host.engine.node_identities.items.len);
    try std.testing.expectEqual(true_scope, host.engine.node_identities.items[3].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[3].ordinal);

    const true_scope_again = host.walkElemWhenBranchIdentitySites(0, 1, .true_branch, when_elem.payload.when.when_true.*);
    try std.testing.expectEqual(true_scope, true_scope_again);
    try std.testing.expectEqual(@as(usize, 4), host.engine.node_identities.items.len);

    const false_scope = host.walkElemWhenBranchIdentitySites(0, 1, .false_branch, when_elem.payload.when.when_false.*);
    try std.testing.expect(false_scope != true_scope);
    try std.testing.expectEqual(@as(usize, 5), host.engine.node_identities.items.len);
    try std.testing.expectEqual(false_scope, host.engine.node_identities.items[4].scope_id);
    try std.testing.expectEqual(@as(u64, 0), host.engine.node_identities.items[4].ordinal);
}

test "signals host collects Elem descriptor stream" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);

    const root_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .role, "region"),
        testNodeStaticTextAttr(&roc_host, .label, "Dashboard"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeConstExpr(&roc_host, testHostValueStr(&roc_host, "search"))),
        testNodeStaticBoolAttr(.disabled, true),
        testNodeSignalBoolAttr(&roc_host, .checked, testNodeConstExpr(&roc_host, testHostValueBool(false))),
    };
    const state_token = newTestBinderToken(&roc_host);
    const state_child_attrs = [_]abi.NodeAttr{
        testNodeStaticTextAttr(&roc_host, .test_id, "state-child"),
        testNodeSignalTextAttr(&roc_host, .value, testNodeRefExpr(state_token)),
        testNodeEventAttr(&roc_host, .click, state_token, .unit),
    };
    const state_child_children = [_]abi.Elem{
        testNodeText(&roc_host, "state child"),
        testNodeTextSignal(&roc_host, testNodeRefExpr(state_token)),
    };
    const state_child = testElementWith(&roc_host, "span", &state_child_attrs, &state_child_children);
    const state = testNodeStateWithToken(&roc_host, state_token, state_child);
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

    host.collectElemRootDescriptors(&roc_host, &stream, root);

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
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .const_value), std.meta.activeTag(stream.signal_text_nodes.items[0].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(@as(u64, 6), stream.signal_text_nodes.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 4), stream.signal_text_nodes.items[1].parent_elem_id);
    try std.testing.expectEqual(@as(std.meta.Tag(HostSignalRecordPayload), .ref), std.meta.activeTag(stream.signal_text_nodes.items[1].signal.record.payload));
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[1].signal.source_node_ids.len);
    try std.testing.expectEqual(stream.scope_sites.items[0].node_id, stream.signal_text_nodes.items[1].signal.source_node_ids[0]);
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
        deinitTestHostIdentity(&host);
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

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(u64, 23), host.engine.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 0), host.engine.pending_roc_metrics.closure_releases);

    stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);

    try std.testing.expectEqual(@as(u64, 23), host.engine.pending_roc_metrics.closure_retains);
    try std.testing.expectEqual(@as(u64, 23), host.engine.pending_roc_metrics.closure_releases);
}

test "signals host preserves explicit signal tokens across cloned descriptors" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);

    const signal = testNodeMapExpr(&roc_host, testNodeConstExpr(&roc_host, testHostValueI64(41)));
    abi.increfNodeSignalExpr(signal, 1);
    const root_children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, signal),
        testNodeTextSignal(&roc_host, signal),
    };
    const root = testElement(&roc_host, &root_children);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

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
    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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

    host.engine.active_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
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

test "signals host carries binder context into Elem when branch collection" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    var stream: HostNodeDescriptorStream = .{};
    defer stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);

    const state_token = newTestBinderToken(&roc_host);
    const branch_children = [_]abi.Elem{
        testNodeTextSignal(&roc_host, testNodeRefExpr(state_token)),
    };
    const branch_true = testElementWith(&roc_host, "span", &.{}, &branch_children);
    const when_elem = testNodeWhen(&roc_host, branch_true, testNodeText(&roc_host, "hidden"));
    const state_child_children = [_]abi.Elem{
        when_elem,
    };
    const state_child = testElementWith(&roc_host, "div", &.{}, &state_child_children);
    const root = testNodeStateWithToken(&roc_host, state_token, state_child);
    defer abi.decrefElem(root, &roc_host);

    host.collectElemRootDescriptors(&roc_host, &stream, root);

    try std.testing.expectEqual(@as(usize, 2), stream.scope_sites.items.len);
    const state_site = stream.scope_sites.items[0];
    const when_site = stream.scope_sites.items[1];
    try std.testing.expectEqual(HostNodeScopeSiteKind.state, state_site.kind);
    try std.testing.expectEqual(HostNodeScopeSiteKind.when, when_site.kind);
    try std.testing.expectEqual(@as(usize, 1), when_site.binder_bindings.len);
    try std.testing.expect(when_site.binder_bindings[0].token == state_token);
    try std.testing.expectEqual(state_site.node_id, when_site.binder_bindings[0].node_id);
    try std.testing.expectEqual(@as(usize, 0), stream.signal_text_nodes.items.len);

    const branch_scope_id = host.collectElemWhenBranchDescriptors(&roc_host, &stream, when_site, .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expect(branch_scope_id != 0);
    try std.testing.expectEqual(@as(usize, 2), stream.elements.items.len);
    try std.testing.expectEqual(@as(u64, 2), stream.elements.items[1].elem_id);
    try std.testing.expectEqual(@as(u64, 1), stream.elements.items[1].parent_elem_id);
    try std.testing.expectEqual(branch_scope_id, stream.elements.items[1].scope_id);
    try std.testing.expectEqualStrings("span", stream.elements.items[1].tag);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items.len);
    try std.testing.expectEqual(branch_scope_id, stream.signal_text_nodes.items[0].scope_id);
    try std.testing.expectEqual(@as(usize, 1), stream.signal_text_nodes.items[0].signal.source_node_ids.len);
    try std.testing.expectEqual(state_site.node_id, stream.signal_text_nodes.items[0].signal.source_node_ids[0]);
}

test "signals host disposes inactive Elem when branch scope" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const branch_true = testNodeState(&roc_host, testNodeText(&roc_host, "true branch"));
    const branch_false = testNodeState(&roc_host, testNodeText(&roc_host, "false branch"));
    const when_elem = testNodeWhen(&roc_host, branch_true, branch_false);
    const root_children = [_]abi.Elem{when_elem};
    const root = testElementWith(&roc_host, "section", &.{}, &root_children);
    defer abi.decrefElem(root, &roc_host);

    var true_stream: HostNodeDescriptorStream = .{};
    defer true_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &true_stream, root);
    const true_scope = host.collectElemActiveWhenBranchDescriptors(&roc_host, &true_stream, true_stream.scope_sites.items[0], true_stream.whens.items[0], .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expectEqual(@as(usize, 2), true_stream.scope_sites.items.len);
    const true_state_id = true_stream.scope_sites.items[1].node_id;
    try std.testing.expectEqual(true_scope, true_stream.scope_sites.items[1].scope_id);
    try std.testing.expect(host.engine.scopes.items[@intCast(true_scope)].active);
    try std.testing.expect(host.engine.node_identities.items[@intCast(true_state_id)].active);

    var false_stream: HostNodeDescriptorStream = .{};
    defer false_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &false_stream, root);
    const false_scope = host.collectElemActiveWhenBranchDescriptors(&roc_host, &false_stream, false_stream.scope_sites.items[0], false_stream.whens.items[0], .false_branch, when_elem.payload.when.when_false.*);

    try std.testing.expect(false_scope != true_scope);
    try std.testing.expect(!host.engine.scopes.items[@intCast(true_scope)].active);
    try std.testing.expect(!host.engine.node_identities.items[@intCast(true_state_id)].active);
    try std.testing.expect(host.engine.scopes.items[@intCast(false_scope)].active);
    try std.testing.expectEqual(@as(u64, 1), host.engine.pending_roc_metrics.scopes_disposed);
    const false_state_id = false_stream.scope_sites.items[1].node_id;

    var true_again_stream: HostNodeDescriptorStream = .{};
    defer true_again_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &true_again_stream, root);
    const true_again_scope = host.collectElemActiveWhenBranchDescriptors(&roc_host, &true_again_stream, true_again_stream.scope_sites.items[0], true_again_stream.whens.items[0], .true_branch, when_elem.payload.when.when_true.*);

    try std.testing.expect(true_again_scope != true_scope);
    try std.testing.expect(true_again_scope != false_scope);
    try std.testing.expect(!host.engine.scopes.items[@intCast(false_scope)].active);
    try std.testing.expect(!host.engine.node_identities.items[@intCast(false_state_id)].active);
    try std.testing.expect(host.engine.scopes.items[@intCast(true_again_scope)].active);
    try std.testing.expectEqual(@as(u64, 2), host.engine.pending_roc_metrics.scopes_disposed);
    try std.testing.expect(true_again_stream.scope_sites.items[1].node_id != true_state_id);
}

test "signals host collects Elem each row bodies by keyed scope" {
    test_erased_callable_drop_count = 0;

    var host = HostEnv.init();
    var roc_host = makeSignalsRocHost(&host);
    host.engine.roc_host = &roc_host;
    defer {
        deinitTestHostIdentity(&host);
        _ = host.gpa.deinit();
    }

    const root_children = [_]abi.Elem{
        testNodeEach(&roc_host),
    };
    const root = testElementWith(&roc_host, "section", &.{}, &root_children);
    defer abi.decrefElem(root, &roc_host);

    var initial_stream: HostNodeDescriptorStream = .{};
    defer initial_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &initial_stream, root);

    try std.testing.expectEqual(@as(usize, 1), initial_stream.scope_sites.items.len);
    try std.testing.expectEqual(HostNodeScopeSiteKind.each, initial_stream.scope_sites.items[0].kind);
    try std.testing.expectEqual(@as(usize, 1), initial_stream.eaches.items.len);

    const initial_items = [_]HostValue{ testHostValueI64(1), testHostValueI64(2), testHostValueI64(3) };
    const initial = host.collectElemEachRowDescriptors(&roc_host, &initial_stream, initial_stream.scope_sites.items[0], initial_stream.eaches.items[0], &initial_items);
    defer freeKeyedRowDiff(&host, initial);

    try std.testing.expectEqual(@as(u64, 0), initial.rows_reused);
    try std.testing.expectEqual(@as(u64, 3), initial.rows_created);
    try std.testing.expectEqual(@as(u64, 0), initial.rows_removed);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), initial.row_items_updated);
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
    defer reordered_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &reordered_stream, root);

    const reordered_items = [_]HostValue{ testHostValueI64(3), testHostValueI64(1), testHostValueI64(2) };
    const reordered = host.collectElemEachRowDescriptors(&roc_host, &reordered_stream, reordered_stream.scope_sites.items[0], reordered_stream.eaches.items[0], &reordered_items);
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
    try std.testing.expectEqual(state_for_key_2, reordered_stream.scope_sites.items[3].node_id);

    var changed_stream: HostNodeDescriptorStream = .{};
    defer changed_stream.deinit(host.gpa.allocator(), &roc_host, &host.engine.pending_roc_metrics);
    host.collectElemRootDescriptors(&roc_host, &changed_stream, root);

    const changed_items = [_]HostValue{ testHostValueI64(2), testHostValueI64(4) };
    const changed = host.collectElemEachRowDescriptors(&roc_host, &changed_stream, changed_stream.scope_sites.items[0], changed_stream.eaches.items[0], &changed_items);
    defer freeKeyedRowDiff(&host, changed);

    try std.testing.expectEqual(@as(u64, 1), changed.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), changed.rows_created);
    try std.testing.expectEqual(@as(u64, 2), changed.rows_removed);
    try std.testing.expectEqual(@as(u64, 1), changed.row_items_unchanged);
    try std.testing.expectEqual(@as(u64, 0), changed.row_items_updated);
    try std.testing.expectEqual(initial.scope_ids[1], changed.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[0]);
    try std.testing.expect(changed.scope_ids[1] != initial.scope_ids[2]);
    try std.testing.expect(!host.engine.scopes.items[@intCast(initial.scope_ids[0])].active);
    try std.testing.expect(!host.engine.scopes.items[@intCast(initial.scope_ids[2])].active);
    try std.testing.expectEqual(state_for_key_2, host.internNodeIdentity(changed.scope_ids[0], 0));
}
