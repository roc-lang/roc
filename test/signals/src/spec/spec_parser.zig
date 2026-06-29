const std = @import("std");

pub const SpecCommandType = enum {
    click,
    pointer_down,
    pointer_up,
    pointer_enter,
    pointer_leave,
    key_down,
    submit,
    fill,
    check,
    uncheck,
    expect_text,
    expect_visible,
    expect_absent,
    expect_value,
    expect_attr,
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
    expect_metric_delta_at_most,
};

pub const LocatorKind = enum {
    none,
    role_name,
    label,
    text,
    test_id,
};

pub const Locator = struct {
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

pub const SpecCommand = struct {
    cmd_type: SpecCommandType,
    locator: Locator,
    task_name: ?[]const u8 = null,
    expected_attr: ?[]const u8 = null,
    interval_ms: ?u64 = null,
    expected_text: ?[]const u8,
    expected_count: ?u64,
    expected_metric_delta: ?i64 = null,
    expected_bool: ?bool,
    line_num: usize,
};

pub fn freeSpecCommands(allocator: std.mem.Allocator, commands: []SpecCommand) void {
    for (commands) |cmd| {
        cmd.locator.deinit(allocator);
        if (cmd.task_name) |name| allocator.free(name);
        if (cmd.expected_attr) |attr| allocator.free(attr);
        if (cmd.expected_text) |text| allocator.free(text);
    }
    if (commands.len > 0) {
        allocator.free(commands);
    }
}

pub const ParseError = error{
    InvalidFormat,
    OutOfMemory,
    FileNotFound,
    IoError,
};

pub fn parseTestSpecFile(allocator: std.mem.Allocator, file_path: []const u8) ParseError![]SpecCommand {
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

pub fn parseTestSpec(allocator: std.mem.Allocator, content: []const u8) ParseError![]SpecCommand {
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
        } else if (std.mem.startsWith(u8, trimmed, "pointer_down ")) {
            try appendSpecCommand(&commands, allocator, .pointer_down, try parseLocator(allocator, trimmed["pointer_down ".len..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "pointer_up ")) {
            try appendSpecCommand(&commands, allocator, .pointer_up, try parseLocator(allocator, trimmed["pointer_up ".len..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "pointer_enter ")) {
            try appendSpecCommand(&commands, allocator, .pointer_enter, try parseLocator(allocator, trimmed["pointer_enter ".len..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "pointer_leave ")) {
            try appendSpecCommand(&commands, allocator, .pointer_leave, try parseLocator(allocator, trimmed["pointer_leave ".len..]), null, null, null, line_num);
        } else if (std.mem.startsWith(u8, trimmed, "key_down ")) {
            const shift_split = try splitTrailingToken(trimmed["key_down ".len..]);
            const key_split = try splitTrailingQuoted(shift_split.head);
            const key_copy = allocator.dupe(u8, key_split.quoted) catch return ParseError.OutOfMemory;
            errdefer allocator.free(key_copy);
            try appendSpecCommand(&commands, allocator, .key_down, try parseLocator(allocator, key_split.head), key_copy, null, try parseBoolToken(shift_split.token), line_num);
        } else if (std.mem.startsWith(u8, trimmed, "submit ")) {
            try appendSpecCommand(&commands, allocator, .submit, try parseLocator(allocator, trimmed["submit ".len..]), null, null, null, line_num);
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
        } else if (std.mem.startsWith(u8, trimmed, "expect_attr ")) {
            const value_split = try splitTrailingQuoted(trimmed["expect_attr ".len..]);
            const name_split = try splitTrailingToken(value_split.head);
            const attr_name = allocator.dupe(u8, name_split.token) catch return ParseError.OutOfMemory;
            errdefer allocator.free(attr_name);
            const value_copy = allocator.dupe(u8, value_split.quoted) catch return ParseError.OutOfMemory;
            errdefer allocator.free(value_copy);
            try appendSpecCommand(&commands, allocator, .expect_attr, try parseLocator(allocator, name_split.head), value_copy, null, null, line_num);
            commands.items[commands.items.len - 1].expected_attr = attr_name;
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
        } else if (std.mem.startsWith(u8, trimmed, "expect_metric_delta_at_most ")) {
            const split = try splitTrailingToken(trimmed["expect_metric_delta_at_most ".len..]);
            const metric_name = allocator.dupe(u8, split.head) catch return ParseError.OutOfMemory;
            const expected_delta = std.fmt.parseInt(i64, split.token, 10) catch return ParseError.InvalidFormat;
            try appendSpecCommand(&commands, allocator, .expect_metric_delta_at_most, emptyLocator(), metric_name, null, null, line_num);
            commands.items[commands.items.len - 1].expected_metric_delta = expected_delta;
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

test "spec parser parses actions and assertions" {
    const content =
        \\click role:button name:"Save"
        \\fill label:"Email" "a@example.com"
        \\expect_attr test_id:"status" data-state "ready"
        \\tick_interval 250
        \\expect_interval 250 1
    ;
    const commands = try parseTestSpec(std.testing.allocator, content);
    defer freeSpecCommands(std.testing.allocator, commands);

    try std.testing.expectEqual(@as(usize, 5), commands.len);
    try std.testing.expectEqual(SpecCommandType.click, commands[0].cmd_type);
    try std.testing.expectEqual(LocatorKind.role_name, commands[0].locator.kind);
    try std.testing.expectEqualStrings("button", commands[0].locator.role.?);
    try std.testing.expectEqualStrings("Save", commands[0].locator.name.?);
    try std.testing.expectEqualStrings("a@example.com", commands[1].expected_text.?);
    try std.testing.expectEqualStrings("data-state", commands[2].expected_attr.?);
    try std.testing.expectEqualStrings("ready", commands[2].expected_text.?);
    try std.testing.expectEqual(@as(?u64, 250), commands[3].interval_ms);
    try std.testing.expectEqual(@as(?u64, 1), commands[4].expected_count);
}

test "spec parser rejects malformed commands" {
    try std.testing.expectError(ParseError.InvalidFormat, parseTestSpec(std.testing.allocator, "click missing_locator"));
}
