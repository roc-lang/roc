//! Platform host for testing signal-based reactive UI applications.
//!
//! Roc owns the signal graph and renders command batches through the `roc_ui_*`
//! entrypoints. The host owns only the simulated DOM, test runner, allocation
//! hooks, and boxed runtime lifecycle.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const abi = @import("roc_platform_abi.zig");

const DispatchResultBox = @typeInfo(@TypeOf(abi.roc_ui_init)).@"fn".return_type.?;
const DispatchResult = std.meta.Child(DispatchResultBox);
const RuntimeBox = @typeInfo(@TypeOf(abi.roc_ui_drop)).@"fn".params[0].type.?;
const RuntimeMetrics = @FieldType(DispatchResult, "metrics");
const HostEventBox = @typeInfo(@TypeOf(abi.roc_ui_dispatch)).@"fn".params[1].type.?;
const HostEvent = std.meta.Child(HostEventBox);
const CommandPayload = @FieldType(abi.UiRuntimeCommand, "payload");
const AppendChildPayload = @FieldType(CommandPayload, "append_child");
const CreateElementPayload = @FieldType(CommandPayload, "create_element");
const StringCommandPayload = @FieldType(CommandPayload, "set_text");
const RocStr = abi.RocStr;

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

fn handleRocAccessViolation(fault_addr: usize) noreturn {
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
};

const LocatorKind = enum {
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

    fn init() TestState {
        return .{
            .verbose = false,
            .commands = &.{},
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
        } else {
            return ParseError.InvalidFormat;
        }
    }

    return commands.toOwnedSlice(allocator) catch ParseError.OutOfMemory;
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
    next_elem_id: u64 = 0,
    roc_ops: ?*abi.RocOps = null,
    runtime_box: ?RuntimeBox = null,
    last_runtime_metrics: RuntimeMetrics = zeroRuntimeMetrics(),

    fn init() HostEnv {
        return .{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
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
        .callbacks = 0,
        .dynamic_renders = 0,
        .evaluated_nodes = 0,
        .events_processed = 0,
        .graph_nodes = 0,
        .keyed_creates = 0,
        .keyed_removes = 0,
        .keyed_reuses = 0,
        .node_value_equality_checks = 0,
        .signal_changes = 0,
        .signal_suppressed = 0,
        .signal_writes = 0,
        .commands_emitted = 0,
        .event_lookups = 0,
        .full_render_batches = 0,
        .incremental_batches = 0,
        .state_lookups = 0,
        .structural_resets = 0,
    };
}

var current_host: ?*HostEnv = null;
var current_roc_ops: ?*abi.RocOps = null;

fn hostFromOps(ops: *abi.RocOps) *HostEnv {
    return @ptrCast(@alignCast(ops.env));
}

fn currentRocOps() *abi.RocOps {
    return current_roc_ops orelse @panic("signals RocOps is not initialized");
}

fn rocAllocFn(ops: *abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromOps(ops);
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

fn rocDeallocFn(ops: *abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host = hostFromOps(ops);
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

fn rocReallocFn(ops: *abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromOps(ops);
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

fn rocDbgFn(ops: *abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

fn rocExpectFailedFn(ops: *abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

fn rocCrashedFn(ops: *abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    writeStderr("\n\x1b[31mRoc crashed:\x1b[0m ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn hostAlloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocAllocFn(currentRocOps(), length, alignment);
}

fn hostDealloc(ptr: *anyopaque, alignment: usize) callconv(.c) void {
    rocDeallocFn(currentRocOps(), ptr, alignment);
}

fn hostRealloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    return rocReallocFn(currentRocOps(), ptr, new_length, alignment);
}

fn hostDbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocDbgFn(currentRocOps(), bytes, len);
}

fn hostExpectFailed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocExpectFailedFn(currentRocOps(), bytes, len);
}

fn hostCrashed(bytes: [*]const u8, len: usize) callconv(.c) void {
    rocCrashedFn(currentRocOps(), bytes, len);
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

const StringField = enum { role, label, test_id };

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

    fn add(self: *CommandCounts, command: abi.UiRuntimeCommand) void {
        self.total += 1;
        switch (command.tag) {
            .ResetDom => self.reset_dom += 1,
            .CreateElement => self.create_element += 1,
            .AppendChild => self.append_child += 1,
            .SetText => self.set_text += 1,
            .SetValue => self.set_value += 1,
            .SetChecked => self.set_checked += 1,
            .SetDisabled => self.set_disabled += 1,
            .SetRole, .SetLabel, .SetTestId => self.set_metadata += 1,
            .BindClick, .BindInput, .BindCheck => self.bind_event += 1,
        }
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
        .callbacks = left.callbacks + right.callbacks,
        .commands_emitted = left.commands_emitted + right.commands_emitted,
        .dynamic_renders = left.dynamic_renders + right.dynamic_renders,
        .evaluated_nodes = left.evaluated_nodes + right.evaluated_nodes,
        .event_lookups = left.event_lookups + right.event_lookups,
        .events_processed = left.events_processed + right.events_processed,
        .full_render_batches = left.full_render_batches + right.full_render_batches,
        .graph_nodes = left.graph_nodes + right.graph_nodes,
        .incremental_batches = left.incremental_batches + right.incremental_batches,
        .keyed_creates = left.keyed_creates + right.keyed_creates,
        .keyed_removes = left.keyed_removes + right.keyed_removes,
        .keyed_reuses = left.keyed_reuses + right.keyed_reuses,
        .node_value_equality_checks = left.node_value_equality_checks + right.node_value_equality_checks,
        .signal_changes = left.signal_changes + right.signal_changes,
        .signal_suppressed = left.signal_suppressed + right.signal_suppressed,
        .signal_writes = left.signal_writes + right.signal_writes,
        .state_lookups = left.state_lookups + right.state_lookups,
        .structural_resets = left.structural_resets + right.structural_resets,
    };
}

fn setElementTextIfChanged(host: *HostEnv, elem: *DomElement, text: []const u8) void {
    if (replaceOwnedString(host.gpa.allocator(), &elem.text, text)) {
        elem.text_update_count += 1;
    }
}

fn setElementValueIfChanged(host: *HostEnv, elem: *DomElement, value: []const u8) void {
    if (replaceOwnedString(host.gpa.allocator(), &elem.value, value)) {
        elem.value_update_count += 1;
    }
}

fn setElementCheckedIfChanged(elem: *DomElement, checked: bool) void {
    if (elem.checked != checked) {
        elem.checked = checked;
        elem.checked_update_count += 1;
    }
}

fn setElementDisabledIfChanged(elem: *DomElement, disabled: bool) void {
    if (elem.disabled != disabled) {
        elem.disabled = disabled;
        elem.disabled_update_count += 1;
    }
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

fn applyCreateElementCommand(host: *HostEnv, payload: CreateElementPayload) void {
    const id = payload.id;
    if (id != host.dom_elements.items.len) failHost("DOM CreateElement command ids must be sequential");

    const allocator = host.gpa.allocator();
    const tag_copy = allocator.dupe(u8, payload.tag.asSlice()) catch std.process.exit(1);
    host.dom_elements.append(allocator, DomElement.init(id, tag_copy)) catch {
        allocator.free(tag_copy);
        std.process.exit(1);
    };
    host.next_elem_id = id + 1;
}

fn applyAppendChildCommand(host: *HostEnv, payload: AppendChildPayload) void {
    const parent = domElementById(host, payload.parent);
    const child = domElementById(host, payload.child);
    child.parent_id = parent.id;
    parent.children.append(host.gpa.allocator(), child.id) catch std.process.exit(1);
}

fn applyStringCommand(host: *HostEnv, payload: StringCommandPayload, field: StringField) void {
    const elem = domElementById(host, payload.id);
    const value = payload.value.asSlice();
    switch (field) {
        .role => _ = replaceOwnedString(host.gpa.allocator(), &elem.role, value),
        .label => _ = replaceOwnedString(host.gpa.allocator(), &elem.label, value),
        .test_id => _ = replaceOwnedString(host.gpa.allocator(), &elem.test_id, value),
    }
}

fn applyCommand(host: *HostEnv, command: abi.UiRuntimeCommand) void {
    switch (command.tag) {
        .ResetDom => resetSimulatedDom(host),
        .CreateElement => applyCreateElementCommand(host, command.payload.create_element),
        .AppendChild => applyAppendChildCommand(host, command.payload.append_child),
        .SetText => {
            const payload = command.payload.set_text;
            setElementTextIfChanged(host, domElementById(host, payload.id), payload.value.asSlice());
        },
        .SetRole => applyStringCommand(host, command.payload.set_role, .role),
        .SetLabel => applyStringCommand(host, command.payload.set_label, .label),
        .SetTestId => applyStringCommand(host, command.payload.set_test_id, .test_id),
        .SetValue => {
            const payload = command.payload.set_value;
            setElementValueIfChanged(host, domElementById(host, payload.id), payload.value.asSlice());
        },
        .SetChecked => {
            const payload = command.payload.set_checked;
            setElementCheckedIfChanged(domElementById(host, payload.id), payload.value);
        },
        .SetDisabled => {
            const payload = command.payload.set_disabled;
            setElementDisabledIfChanged(domElementById(host, payload.id), payload.value);
        },
        .BindClick => {
            const payload = command.payload.bind_click;
            domElementById(host, payload.id).bound_click_event = payload.event;
        },
        .BindInput => {
            const payload = command.payload.bind_input;
            domElementById(host, payload.id).bound_input_event = payload.event;
        },
        .BindCheck => {
            const payload = command.payload.bind_check;
            domElementById(host, payload.id).bound_check_event = payload.event;
        },
    }
}

fn traceCommand(command: abi.UiRuntimeCommand) void {
    if (!traceEnabled()) return;
    switch (command.tag) {
        .ResetDom => writeStderr("[HOST CMD] ResetDom\n"),
        .CreateElement => {
            const payload = command.payload.create_element;
            printStderr("[HOST CMD] CreateElement id={d} tag=\"{s}\"\n", .{ payload.id, payload.tag.asSlice() });
        },
        .AppendChild => {
            const payload = command.payload.append_child;
            printStderr("[HOST CMD] AppendChild parent={d} child={d}\n", .{ payload.parent, payload.child });
        },
        .SetText => {
            const payload = command.payload.set_text;
            printStderr("[HOST CMD] SetText id={d} value=\"{s}\"\n", .{ payload.id, payload.value.asSlice() });
        },
        .SetRole => {
            const payload = command.payload.set_role;
            printStderr("[HOST CMD] SetRole id={d} value=\"{s}\"\n", .{ payload.id, payload.value.asSlice() });
        },
        .SetLabel => {
            const payload = command.payload.set_label;
            printStderr("[HOST CMD] SetLabel id={d} value=\"{s}\"\n", .{ payload.id, payload.value.asSlice() });
        },
        .SetTestId => {
            const payload = command.payload.set_test_id;
            printStderr("[HOST CMD] SetTestId id={d} value=\"{s}\"\n", .{ payload.id, payload.value.asSlice() });
        },
        .SetValue => {
            const payload = command.payload.set_value;
            printStderr("[HOST CMD] SetValue id={d} value=\"{s}\"\n", .{ payload.id, payload.value.asSlice() });
        },
        .SetChecked => {
            const payload = command.payload.set_checked;
            printStderr("[HOST CMD] SetChecked id={d} value={}\n", .{ payload.id, payload.value });
        },
        .SetDisabled => {
            const payload = command.payload.set_disabled;
            printStderr("[HOST CMD] SetDisabled id={d} value={}\n", .{ payload.id, payload.value });
        },
        .BindClick => {
            const payload = command.payload.bind_click;
            printStderr("[HOST CMD] BindClick id={d} event={d}\n", .{ payload.id, payload.event });
        },
        .BindInput => {
            const payload = command.payload.bind_input;
            printStderr("[HOST CMD] BindInput id={d} event={d}\n", .{ payload.id, payload.event });
        },
        .BindCheck => {
            const payload = command.payload.bind_check;
            printStderr("[HOST CMD] BindCheck id={d} event={d}\n", .{ payload.id, payload.event });
        },
    }
}

fn applyCommandList(host: *HostEnv, commands: abi.RocList(abi.UiRuntimeCommand)) CommandCounts {
    var counts: CommandCounts = .{};
    for (commands.items()) |command| {
        counts.add(command);
        traceCommand(command);
        applyCommand(host, command);
    }
    return counts;
}

fn releaseCommandList(commands: abi.RocList(abi.UiRuntimeCommand), ops: *abi.RocOps) void {
    if (commands.isUnique()) {
        for (commands.items()) |command| {
            abi.decrefUiRuntimeCommand(command, ops);
        }
    }
    commands.decref(ops);
}

fn dropMovedDispatchResultPayload(_: ?*anyopaque, _: *abi.RocOps) callconv(.c) void {}

fn acceptDispatchResultMeasured(host: *HostEnv, ops: *abi.RocOps, result_box: DispatchResultBox, apply_ns: ?*u64, command_counts: ?*CommandCounts) void {
    const result = result_box.*;
    if (host.runtime_box != null) failHost("Roc runtime box was overwritten before being consumed");
    host.runtime_box = result.runtime;
    host.last_runtime_metrics = result.metrics;
    const start_ns = benchmarkNowNs();
    const counts = applyCommandList(host, result.commands);
    const elapsed = benchmarkNowNs() - start_ns;
    if (apply_ns) |ns| ns.* += elapsed;
    if (command_counts) |total| total.addAll(counts);
    releaseCommandList(result.commands, ops);
    abi.decrefBoxWith(@ptrCast(result_box), @alignOf(DispatchResult), &dropMovedDispatchResultPayload, ops);
}

fn acceptDispatchResult(host: *HostEnv, ops: *abi.RocOps, result_box: DispatchResultBox) void {
    acceptDispatchResultMeasured(host, ops, result_box, null, null);
}

fn boxHostEvent(ops: *abi.RocOps, event: HostEvent) HostEventBox {
    const raw = abi.allocateBox(@sizeOf(HostEvent), @alignOf(HostEvent), true, ops);
    const event_box: HostEventBox = @ptrCast(@alignCast(raw));
    event_box.* = event;
    return event_box;
}

fn dispatchRocEvent(host: *HostEnv, ops: *abi.RocOps, event: HostEvent) void {
    const runtime = host.runtime_box orelse failHost("Roc runtime not initialized");
    host.runtime_box = null;
    acceptDispatchResult(host, ops, abi.roc_ui_dispatch(runtime, boxHostEvent(ops, event)));
}

fn dispatchRocEventMeasured(host: *HostEnv, ops: *abi.RocOps, event: HostEvent, stats: *BenchmarkStats) void {
    const runtime = host.runtime_box orelse failHost("Roc runtime not initialized");
    host.runtime_box = null;
    const event_box = boxHostEvent(ops, event);
    const start_ns = benchmarkNowNs();
    const result_box = abi.roc_ui_dispatch(runtime, event_box);
    stats.dispatch_roc_ns += benchmarkNowNs() - start_ns;
    acceptDispatchResultMeasured(host, ops, result_box, &stats.dispatch_apply_ns, &stats.commands);
    stats.actions += 1;
}

fn signalsHostedFunctions() abi.HostedFunctions {
    return abi.hostedFunctions(.{});
}

fn makeSignalsRocOps(host: *HostEnv) abi.RocOps {
    return .{
        .env = @ptrCast(host),
        .roc_alloc = &rocAllocFn,
        .roc_dealloc = &rocDeallocFn,
        .roc_realloc = &rocReallocFn,
        .roc_dbg = &rocDbgFn,
        .roc_expect_failed = &rocExpectFailedFn,
        .roc_crashed = &rocCrashedFn,
        .hosted_fns = signalsHostedFunctions(),
    };
}

fn commandIsAction(cmd: SpecCommand) bool {
    return switch (cmd.cmd_type) {
        .click, .fill, .check, .uncheck => true,
        else => false,
    };
}

fn runActionCommandMeasured(host: *HostEnv, ops: *abi.RocOps, cmd: SpecCommand, stats: *BenchmarkStats) void {
    switch (cmd.cmd_type) {
        .click => {
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark click locator did not resolve");
            if (elem.disabled) failHost("benchmark click target is disabled");
            const event_id = elem.bound_click_event orelse failHost("benchmark click target has no binding");
            dispatchRocEventMeasured(host, ops, .{
                .payload = .{ .click = event_id },
                .tag = .Click,
            }, stats);
        },

        .fill => {
            const value = cmd.expected_text orelse "";
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark fill locator did not resolve");
            if (elem.disabled) failHost("benchmark fill target is disabled");
            if (elem.bound_input_event) |event_id| {
                dispatchRocEventMeasured(host, ops, .{
                    .payload = .{
                        .input = .{
                            .event = event_id,
                            .value = RocStr.fromSlice(value, ops),
                        },
                    },
                    .tag = .Input,
                }, stats);
            } else {
                setElementValueIfChanged(host, elem, value);
            }
        },

        .check, .uncheck => {
            const checked = cmd.cmd_type == .check;
            const elem = host.findElementByLocator(cmd.locator, cmd.line_num) orelse failHost("benchmark check locator did not resolve");
            if (elem.disabled) failHost("benchmark check target is disabled");
            if (elem.bound_check_event) |event_id| {
                dispatchRocEventMeasured(host, ops, .{
                    .payload = .{
                        .check = .{
                            .event = event_id,
                            .checked = checked,
                        },
                    },
                    .tag = .Check,
                }, stats);
            } else {
                setElementCheckedIfChanged(elem, checked);
            }
        },

        else => {},
    }
}

fn runBenchmarkIteration(commands: []const SpecCommand, verbose: bool, stats: *BenchmarkStats) void {
    var host_env = HostEnv.init();
    host_env.test_state.verbose = verbose;

    var roc_ops = makeSignalsRocOps(&host_env);
    host_env.roc_ops = &roc_ops;
    current_host = &host_env;
    current_roc_ops = &roc_ops;

    const init_start_ns = benchmarkNowNs();
    const init_result = abi.roc_ui_init();
    stats.init_roc_ns += benchmarkNowNs() - init_start_ns;
    acceptDispatchResultMeasured(&host_env, &roc_ops, init_result, &stats.init_apply_ns, &stats.commands);

    for (commands) |cmd| {
        if (commandIsAction(cmd)) {
            runActionCommandMeasured(&host_env, &roc_ops, cmd, stats);
        }
    }

    stats.metrics = addRuntimeMetrics(stats.metrics, host_env.last_runtime_metrics);
    stats.allocs += @intCast(host_env.alloc_count);
    stats.deallocs += @intCast(host_env.dealloc_count);
    stats.retained_alloc_delta += @as(i64, @intCast(host_env.alloc_count)) - @as(i64, @intCast(host_env.dealloc_count));

    host_env.deinit();
    current_host = null;
    current_roc_ops = null;
}

fn printBenchmarkHeader() void {
    writeStdout("case,sample,iterations,actions,init_roc_ns,init_apply_ns,dispatch_roc_ns,dispatch_apply_ns,total_ns,allocs,deallocs,retained_alloc_delta,commands,reset_dom,create_element,append_child,set_text,set_value,set_checked,set_disabled,set_metadata,bind_event,events_processed,evaluated_nodes,callbacks,dynamic_renders,graph_nodes,commands_emitted,full_render_batches,incremental_batches,structural_resets,state_lookups,event_lookups,signal_writes,signal_changes,signal_suppressed,node_value_equality_checks\n");
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
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            stats.metrics.events_processed,
            stats.metrics.evaluated_nodes,
            stats.metrics.callbacks,
            stats.metrics.dynamic_renders,
            stats.metrics.graph_nodes,
            stats.metrics.commands_emitted,
            stats.metrics.full_render_batches,
            stats.metrics.incremental_batches,
            stats.metrics.structural_resets,
            stats.metrics.state_lookups,
            stats.metrics.event_lookups,
            stats.metrics.signal_writes,
            stats.metrics.signal_changes,
            stats.metrics.signal_suppressed,
            stats.metrics.node_value_equality_checks,
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

    var roc_ops = makeSignalsRocOps(&host_env);
    host_env.roc_ops = &roc_ops;
    current_host = &host_env;
    current_roc_ops = &roc_ops;
    defer current_host = null;
    defer current_roc_ops = null;
    defer host_env.deinit();

    traceStderr("[HOST] calling roc_ui_init\n");
    acceptDispatchResult(&host_env, &roc_ops, abi.roc_ui_init());
    traceStderr("[HOST] roc_ui_init returned\n");

    if (verbose) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} graph nodes\n", .{
            host_env.dom_elements.items.len,
            host_env.last_runtime_metrics.graph_nodes,
        }) catch "";
        writeStderr(msg);
        host_env.dumpDom();
    }

    for (host_env.test_state.commands) |cmd| {
        switch (cmd.cmd_type) {
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
                dispatchRocEvent(&host_env, &roc_ops, .{
                    .payload = .{ .click = event_id },
                    .tag = .Click,
                });
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
                    dispatchRocEvent(&host_env, &roc_ops, .{
                        .payload = .{
                            .input = .{
                                .event = event_id,
                                .value = RocStr.fromSlice(value, &roc_ops),
                            },
                        },
                        .tag = .Input,
                    });
                } else {
                    setElementValueIfChanged(&host_env, elem, value);
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
                    dispatchRocEvent(&host_env, &roc_ops, .{
                        .payload = .{
                            .check = .{
                                .event = event_id,
                                .checked = checked,
                            },
                        },
                        .tag = .Check,
                    });
                } else {
                    setElementCheckedIfChanged(elem, checked);
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
