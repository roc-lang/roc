//! Platform host for testing signal-based reactive UI applications.
//!
//! This host simulates a retained-mode UI with a reactive graph, without requiring
//! actual DOM or GUI infrastructure.
//!
//! ## Usage
//!
//! Run with a test spec file:
//! ```
//! ./app test_spec.txt
//! ```
//!
//! Or with verbose output:
//! ```
//! ./app --verbose test_spec.txt
//! ```
//!
//! ## Test Spec Format (line-separated)
//!
//! ```
//! # Comment (ignored)
//! expect_text button:0 "-"
//! expect_text span:0 "0"
//! click button:1
//! expect_text span:0 "1"
//! ```
//!
//! Commands:
//! - `click <tag>:<index>` - Simulate click on element
//! - `expect_text <tag>:<index> "<text>"` - Verify element text content
//!
//! Exit codes:
//! - 0: All expectations matched
//! - 1: Test failed (mismatch or error)

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const abi = @import("roc_platform_abi.zig");

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
    // Zig 0.16's std.debug.SelfInfo (Windows) references ntdll.LdrRegisterDllNotification,
    // which isn't available when the host static archive is linked into a roc-compiled
    // program. Disabling stack tracing avoids pulling that code in.
    .allow_stack_tracing = false,
};

/// Override the default panic handler to avoid secondary crashes in stack trace generation
pub const panic = std.debug.FullPanic(panicImpl);

fn writeStderr(bytes: []const u8) void {
    std.Io.File.stderr().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn writeStdout(bytes: []const u8) void {
    std.Io.File.stdout().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn printStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [1024]u8 = undefined;
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

/// Error message to display on stack overflow in a Roc program
const STACK_OVERFLOW_MESSAGE = "\nThis Roc application overflowed its stack memory and crashed.\n\n";

/// Callback for stack overflow in a Roc program
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

/// Callback for access violation in a Roc program
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
        const msg = "\nSegmentation fault (SIGSEGV) in this Roc program.\nFault address: ";
        writeStderr(msg);

        var addr_buf: [18]u8 = undefined;
        const addr_str = base.signal_handler.formatHex(fault_addr, &addr_buf);
        writeStderr(addr_str);
        writeStderr("\n\n");
        std.process.exit(139);
    }
}

/// Error message to display on division by zero in a Roc program
const DIVISION_BY_ZERO_MESSAGE = "\nThis Roc application divided by zero and crashed.\n\n";

/// Callback for arithmetic errors (division by zero) in a Roc program
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

const RocStr = abi.RocStr;
const RocBox = abi.RocErasedCallable;
const NodeValue = abi.NodeValue;

fn nodeValueUnit() NodeValue {
    return .{
        .payload = .{ .nv_unit = .{} },
        .tag = .NvUnit,
    };
}

// Simulated DOM

const DomElement = struct {
    id: u64,
    tag: []const u8,
    text: ?[]const u8,
    parent_id: ?u64,
    children: std.ArrayListUnmanaged(u64),
    bound_text_signal: ?u64, // NodeId of signal bound to text
    bound_click_event: ?u64, // NodeId of event bound to click
    scope_id: u64,
    active: bool,
    text_update_count: u64,

    fn init(id: u64, tag: []const u8, scope_id: u64) DomElement {
        return .{
            .id = id,
            .tag = tag,
            .text = null,
            .parent_id = null,
            .children = .empty,
            .bound_text_signal = null,
            .bound_click_event = null,
            .scope_id = scope_id,
            .active = true,
            .text_update_count = 0,
        };
    }

    fn deinit(self: *DomElement, allocator: std.mem.Allocator) void {
        allocator.free(self.tag);
        if (self.text) |text| {
            allocator.free(text);
        }
        self.children.deinit(allocator);
    }
};

// Reactive Graph

const KeyedScope = struct {
    key: []u8,
    scope_id: u64,
};

const PendingEvent = struct {
    node_id: u64,
    value: NodeValue,
};

const StoredValue = union(enum) {
    node_value: NodeValue,
    i64: i64,
    bool: bool,
    str: RocStr,
    unit,
};

const NodeKind = union(enum) {
    event_source,
    event_map: struct { source: u64, transform: RocBox },
    event_map_unit_i64_const: struct { source: u64, value: i64 },
    event_filter: struct { source: u64, predicate: RocBox },
    event_merge: struct { left: u64, right: u64 },
    event_with_latest: struct { event: u64, signal: u64, combine: RocBox },
    signal_const,
    signal_state: struct { update_event: ?u64 },
    signal_map: struct { source: u64, transform: RocBox },
    signal_map_i64_i64: struct { source: u64, transform: RocBox },
    signal_map_i64_str: struct { source: u64, transform: RocBox },
    signal_map2: struct { left: u64, right: u64, transform: RocBox },
    signal_map2_i64_i64: struct { left: u64, right: u64, transform: RocBox },
    signal_map2_i64_i64_str: struct { left: u64, right: u64, transform: RocBox },
    signal_hold: struct { event: u64 },
    signal_fold: struct { event: u64, step: RocBox },
    signal_fold_i64: struct { event: u64, step: RocBox },
    signal_fold_bool_toggle: struct { event: u64 },
    signal_zip_with: struct { source: u64, event: u64, combine: RocBox },
    dynamic: struct { parent: u64, signal: u64, render: RocBox, child_scope: ?u64 },
    each: struct { parent: u64, signal: u64, key: RocBox, render: RocBox, entries: std.ArrayListUnmanaged(KeyedScope) },
};

const GraphNode = struct {
    id: u64,
    kind: NodeKind,
    current_value: ?StoredValue,
    event_values: std.ArrayListUnmanaged(StoredValue),
    dependents: std.ArrayListUnmanaged(u64),
    scope_id: u64,
    active: bool,
    released: bool,

    fn init(id: u64, scope_id: u64, kind: NodeKind) GraphNode {
        return .{
            .id = id,
            .kind = kind,
            .current_value = null,
            .event_values = .empty,
            .dependents = .empty,
            .scope_id = scope_id,
            .active = true,
            .released = false,
        };
    }

    fn releaseResources(self: *GraphNode, allocator: std.mem.Allocator, ops: *abi.RocOps) void {
        if (self.released) return;
        self.released = true;

        if (self.current_value) |value| {
            decrefStoredValue(value, ops);
            self.current_value = null;
        }
        for (self.event_values.items) |value| {
            decrefStoredValue(value, ops);
        }
        self.event_values.clearRetainingCapacity();

        switch (self.kind) {
            .event_map => |*data| decrefErasedCallable(data.transform, ops),
            .event_filter => |*data| decrefErasedCallable(data.predicate, ops),
            .event_with_latest => |*data| decrefErasedCallable(data.combine, ops),
            .signal_map => |*data| decrefErasedCallable(data.transform, ops),
            .signal_map_i64_i64 => |*data| decrefErasedCallable(data.transform, ops),
            .signal_map_i64_str => |*data| decrefErasedCallable(data.transform, ops),
            .signal_map2 => |*data| decrefErasedCallable(data.transform, ops),
            .signal_map2_i64_i64 => |*data| decrefErasedCallable(data.transform, ops),
            .signal_map2_i64_i64_str => |*data| decrefErasedCallable(data.transform, ops),
            .signal_fold => |*data| decrefErasedCallable(data.step, ops),
            .signal_fold_i64 => |*data| decrefErasedCallable(data.step, ops),
            .signal_zip_with => |*data| decrefErasedCallable(data.combine, ops),
            .dynamic => |*data| decrefErasedCallable(data.render, ops),
            .each => |*data| {
                decrefErasedCallable(data.key, ops);
                decrefErasedCallable(data.render, ops);
                for (data.entries.items) |entry| {
                    allocator.free(entry.key);
                }
                data.entries.deinit(allocator);
                data.entries = .empty;
            },
            else => {},
        }
    }

    fn deinit(self: *GraphNode, allocator: std.mem.Allocator, ops: ?*abi.RocOps) void {
        if (ops) |roc_ops| {
            self.releaseResources(allocator, roc_ops);
        }
        self.event_values.deinit(allocator);
        self.dependents.deinit(allocator);
    }
};

const Scope = struct {
    id: u64,
    parent: ?u64,
    active: bool,
    on_mount_events: std.ArrayListUnmanaged(u64),
    on_unmount_events: std.ArrayListUnmanaged(u64),

    fn init(id: u64, parent: ?u64) Scope {
        return .{
            .id = id,
            .parent = parent,
            .active = true,
            .on_mount_events = .empty,
            .on_unmount_events = .empty,
        };
    }

    fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.on_mount_events.deinit(allocator);
        self.on_unmount_events.deinit(allocator);
    }
};

// Test Spec Types

const SpecCommandType = enum {
    click,
    expect_text,
    expect_updates,
};

const SpecCommand = struct {
    cmd_type: SpecCommandType,
    tag: []const u8,
    index: usize,
    expected_text: ?[]const u8, // For expect_text
    expected_count: ?u64,
    line_num: usize, // For error reporting
};

const TestState = struct {
    enabled: bool,
    verbose: bool,
    commands: []SpecCommand,
    failed: bool,
    failure_message: ?[]const u8,
    failure_line: usize,

    fn init() TestState {
        return .{
            .enabled = false,
            .verbose = false,
            .commands = &.{},
            .failed = false,
            .failure_message = null,
            .failure_line = 0,
        };
    }
};

const BenchCounters = struct {
    boundary_encode_count: u64 = 0,
    boundary_decode_count: u64 = 0,
    node_value_incref_count: u64 = 0,
    node_value_decref_count: u64 = 0,
    node_value_equality_count: u64 = 0,
    callback_transform_count: u64 = 0,
    callback_step_count: u64 = 0,
    callback_predicate_count: u64 = 0,
    callback_key_count: u64 = 0,
    callback_render_count: u64 = 0,
    event_count: u64 = 0,
    evaluated_node_count: u64 = 0,
    signal_write_count: u64 = 0,
    signal_changed_count: u64 = 0,
    signal_unchanged_count: u64 = 0,

    fn callbackCount(self: BenchCounters) u64 {
        return self.callback_transform_count +
            self.callback_step_count +
            self.callback_predicate_count +
            self.callback_key_count +
            self.callback_render_count;
    }
};

const ParseError = error{
    InvalidFormat,
    OutOfMemory,
    FileNotFound,
    IoError,
};

/// Parse test spec from file
fn parseTestSpecFile(allocator: std.mem.Allocator, file_path: []const u8) ParseError![]SpecCommand {
    const io = std.Io.Threaded.global_single_threaded.io();
    const content = std.Io.Dir.cwd().readFileAlloc(io, file_path, allocator, .limited(1024 * 1024)) catch |err| switch (err) {
        error.FileNotFound => return ParseError.FileNotFound,
        else => return ParseError.IoError,
    };
    defer allocator.free(content);

    return parseTestSpec(allocator, content);
}

/// Parse test spec content
fn parseTestSpec(allocator: std.mem.Allocator, content: []const u8) ParseError![]SpecCommand {
    var commands: std.ArrayListUnmanaged(SpecCommand) = .empty;
    errdefer commands.deinit(allocator);

    var line_num: usize = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        line_num += 1;

        // Trim whitespace
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Skip empty lines and comments
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        // Parse command
        if (std.mem.startsWith(u8, trimmed, "click ")) {
            const rest = trimmed[6..];
            const elem_id = parseElemId(rest) catch return ParseError.InvalidFormat;
            const tag_copy = allocator.dupe(u8, elem_id.tag) catch return ParseError.OutOfMemory;
            commands.append(allocator, .{
                .cmd_type = .click,
                .tag = tag_copy,
                .index = elem_id.index,
                .expected_text = null,
                .expected_count = null,
                .line_num = line_num,
            }) catch return ParseError.OutOfMemory;
        } else if (std.mem.startsWith(u8, trimmed, "expect_text ")) {
            const rest = trimmed[12..];
            // Parse: tag:index "text"
            const space_idx = std.mem.indexOfScalar(u8, rest, ' ') orelse return ParseError.InvalidFormat;
            const elem_part = rest[0..space_idx];
            const text_part = std.mem.trim(u8, rest[space_idx + 1 ..], " \t");

            // Text must be quoted
            if (text_part.len < 2 or text_part[0] != '"' or text_part[text_part.len - 1] != '"') {
                return ParseError.InvalidFormat;
            }
            const text = text_part[1 .. text_part.len - 1];

            const elem_id = parseElemId(elem_part) catch return ParseError.InvalidFormat;

            // Duplicate strings so they outlive the content buffer
            const tag_copy = allocator.dupe(u8, elem_id.tag) catch return ParseError.OutOfMemory;
            const text_copy = allocator.dupe(u8, text) catch return ParseError.OutOfMemory;

            commands.append(allocator, .{
                .cmd_type = .expect_text,
                .tag = tag_copy,
                .index = elem_id.index,
                .expected_text = text_copy,
                .expected_count = null,
                .line_num = line_num,
            }) catch return ParseError.OutOfMemory;
        } else if (std.mem.startsWith(u8, trimmed, "expect_updates ")) {
            const rest = trimmed[15..];
            const space_idx = std.mem.indexOfScalar(u8, rest, ' ') orelse return ParseError.InvalidFormat;
            const elem_part = rest[0..space_idx];
            const count_part = std.mem.trim(u8, rest[space_idx + 1 ..], " \t");

            const elem_id = parseElemId(elem_part) catch return ParseError.InvalidFormat;
            const expected_count = std.fmt.parseInt(u64, count_part, 10) catch return ParseError.InvalidFormat;
            const tag_copy = allocator.dupe(u8, elem_id.tag) catch return ParseError.OutOfMemory;

            commands.append(allocator, .{
                .cmd_type = .expect_updates,
                .tag = tag_copy,
                .index = elem_id.index,
                .expected_text = null,
                .expected_count = expected_count,
                .line_num = line_num,
            }) catch return ParseError.OutOfMemory;
        } else {
            return ParseError.InvalidFormat;
        }
    }

    return commands.toOwnedSlice(allocator) catch ParseError.OutOfMemory;
}

const ElemId = struct { tag: []const u8, index: usize };

fn parseElemId(s: []const u8) !ElemId {
    const colon_idx = std.mem.indexOfScalar(u8, s, ':') orelse return error.InvalidFormat;
    const tag = s[0..colon_idx];
    const index_str = s[colon_idx + 1 ..];
    const index = std.fmt.parseInt(usize, index_str, 10) catch return error.InvalidFormat;
    // Note: tag is a slice into the caller's buffer.
    // The caller (parseTestSpec) must dupe it if the buffer is transient.
    return .{ .tag = tag, .index = index };
}

// Host Environment

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
    bench_counters: BenchCounters = .{},

    // Simulated DOM
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,
    next_elem_id: u64 = 0,
    tag_counts: std.StringHashMapUnmanaged(usize) = .empty, // Track count per tag for indexing

    // Reactive graph
    graph_nodes: std.ArrayListUnmanaged(GraphNode) = .empty,
    next_node_id: u64 = 0,

    // Mount scopes
    scopes: std.ArrayListUnmanaged(Scope) = .empty,
    current_scope_id: u64 = 0,

    // Propagation state
    pending_events: std.ArrayListUnmanaged(PendingEvent) = .empty,
    propagation_depth: usize = 0,

    // RocOps pointer for closure evaluation
    roc_ops: ?*abi.RocOps = null,

    fn init() HostEnv {
        return HostEnv{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
    }

    fn resetRunMetrics(self: *HostEnv) void {
        self.alloc_count = 0;
        self.dealloc_count = 0;
        self.bench_counters = .{};
    }

    fn textUpdateCount(self: *const HostEnv) u64 {
        var count: u64 = 0;
        for (self.dom_elements.items) |elem| {
            count += elem.text_update_count;
        }
        return count;
    }

    fn activeGraphNodeCount(self: *const HostEnv) u64 {
        var count: u64 = 0;
        for (self.graph_nodes.items) |node| {
            if (node.active) count += 1;
        }
        return count;
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.gpa.allocator();

        // Free DOM elements
        for (self.dom_elements.items) |*elem| {
            elem.deinit(allocator);
        }
        self.dom_elements.deinit(allocator);
        self.tag_counts.deinit(allocator);

        for (self.graph_nodes.items) |*node| {
            node.deinit(allocator, self.roc_ops);
        }
        self.graph_nodes.deinit(allocator);

        for (self.scopes.items) |*scope| {
            scope.deinit(allocator);
        }
        self.scopes.deinit(allocator);

        if (self.roc_ops) |ops| {
            for (self.pending_events.items) |pending| {
                decrefNodeValue(pending.value, ops);
            }
        }
        self.pending_events.deinit(allocator);

        // Free test command strings
        for (self.test_state.commands) |cmd| {
            allocator.free(cmd.tag);
            if (cmd.expected_text) |text| {
                allocator.free(text);
            }
        }
        if (self.test_state.commands.len > 0) {
            allocator.free(self.test_state.commands);
        }

        // Free remaining Roc allocations
        for (self.roc_allocations.items) |alloc| {
            const slice = alloc.ptr[0..alloc.total_size];
            allocator.rawFree(slice, alloc.alignment, @returnAddress());
        }
        self.roc_allocations.deinit(allocator);
    }

    fn findElementByTagIndex(self: *HostEnv, tag: []const u8, index: usize) ?*DomElement {
        var count: usize = 0;
        if (self.dom_elements.items.len == 0) return null;
        return self.findElementByTagIndexFrom(0, tag, index, &count);
    }

    fn findElementByTagIndexFrom(self: *HostEnv, elem_id: u64, tag: []const u8, target_index: usize, count: *usize) ?*DomElement {
        if (elem_id >= self.dom_elements.items.len) return null;
        const elem = &self.dom_elements.items[elem_id];
        if (!elem.active) return null;

        if (std.mem.eql(u8, elem.tag, tag)) {
            if (count.* == target_index) {
                return elem;
            }
            count.* += 1;
        }

        for (elem.children.items) |child_id| {
            if (self.findElementByTagIndexFrom(child_id, tag, target_index, count)) |found| {
                return found;
            }
        }
        return null;
    }

    fn addDependency(self: *HostEnv, source_id: u64, dependent_id: u64) void {
        const allocator = self.gpa.allocator();
        if (source_id < self.graph_nodes.items.len) {
            self.graph_nodes.items[source_id].dependents.append(allocator, dependent_id) catch std.process.exit(1);
        }
    }

    fn createScope(self: *HostEnv, parent: ?u64) u64 {
        const scope_id = @as(u64, @intCast(self.scopes.items.len));
        self.scopes.append(self.gpa.allocator(), Scope.init(scope_id, parent)) catch std.process.exit(1);
        return scope_id;
    }

    fn pushScope(self: *HostEnv, scope_id: u64) u64 {
        const previous = self.current_scope_id;
        self.current_scope_id = scope_id;
        return previous;
    }

    fn popScope(self: *HostEnv, previous: u64) void {
        self.current_scope_id = previous;
    }
};

// Current Host Access

var current_host: ?*HostEnv = null;
var current_roc_ops: ?*abi.RocOps = null;

fn hostFromOps(ops: *abi.RocOps) *HostEnv {
    return @ptrCast(@alignCast(ops.env));
}

fn currentHost() *HostEnv {
    return current_host orelse @panic("signals host is not initialized");
}

fn currentRocOps() *abi.RocOps {
    return current_roc_ops orelse @panic("signals RocOps is not initialized");
}

fn decrefErasedCallable(callable: RocBox, ops: *abi.RocOps) void {
    abi.decrefErasedCallable(callable, ops);
}

// Memory Management

fn rocAllocFn(ops: *abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host = hostFromOps(ops);
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = min_alignment;
    const total_size = length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        std.process.exit(1);
    };

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

    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    allocator.rawFree(slice, align_enum, @returnAddress());
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

    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        std.process.exit(1);
    };

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

    allocator.rawFree(old_slice, align_enum, @returnAddress());

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

// Hosted Effects

/// Host.append_child!
fn hostedAppendChild(parent_id: u64, child_id: u64) callconv(.c) void {
    const host = currentHost();
    {
        var buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[HOST] append_child! parent={d} child={d}\n", .{ parent_id, child_id }) catch "[HOST] append_child!\n";
        traceStderr(msg);
    }

    if (parent_id < host.dom_elements.items.len and child_id < host.dom_elements.items.len and
        host.dom_elements.items[parent_id].active and host.dom_elements.items[child_id].active)
    {
        host.dom_elements.items[parent_id].children.append(host.gpa.allocator(), child_id) catch std.process.exit(1);
        host.dom_elements.items[child_id].parent_id = parent_id;
    }
}

/// Host.bind_click!
fn hostedBindClick(elem_id: u64, event_node_id: u64) callconv(.c) void {
    const host = currentHost();

    if (elem_id < host.dom_elements.items.len and host.dom_elements.items[elem_id].active) {
        host.dom_elements.items[elem_id].bound_click_event = event_node_id;
    }
}

/// Host.bind_text!
fn hostedBindText(elem_id: u64, signal_node_id: u64) callconv(.c) void {
    const host = currentHost();

    if (elem_id < host.dom_elements.items.len and host.dom_elements.items[elem_id].active) {
        host.dom_elements.items[elem_id].bound_text_signal = signal_node_id;
        updateElementText(host, elem_id);
    }
}

/// Host.bind_signal_update!
fn hostedBindSignalUpdate(state_node_id: u64, event_node_id: u64) callconv(.c) void {
    const host = currentHost();

    if (state_node_id < host.graph_nodes.items.len) {
        const node = &host.graph_nodes.items[state_node_id];
        switch (node.kind) {
            .signal_state => {
                node.kind = .{ .signal_state = .{
                    .update_event = event_node_id,
                } };
                host.addDependency(event_node_id, state_node_id);
            },
            else => {},
        }
    }
}

/// Elem.register_lifecycle!
fn hostedRegisterLifecycle(on_mount_event_id: u64, on_unmount_event_id: u64) callconv(.c) void {
    const host = currentHost();
    if (host.current_scope_id >= host.scopes.items.len) return;
    const allocator = host.gpa.allocator();
    const scope = &host.scopes.items[host.current_scope_id];
    scope.on_mount_events.append(allocator, on_mount_event_id) catch std.process.exit(1);
    scope.on_unmount_events.append(allocator, on_unmount_event_id) catch std.process.exit(1);
    fireEvent(host, on_mount_event_id, nodeValueUnit());
}

/// Host.send_event!
fn hostedSendEvent(ops: *abi.RocOps, event_node_id: u64, value: NodeValue) callconv(.c) void {
    const host = hostFromOps(ops);
    fireEvent(host, event_node_id, value);
}

/// Elem.create_dynamic!
fn hostedCreateDynamic(parent_id: u64, signal_id: u64, render: RocBox) callconv(.c) void {
    const host = currentHost();
    traceStderr("[HOST] create_dynamic!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var child_scope: ?u64 = null;
    if (signal_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[signal_id].current_value) |value| {
            child_scope = mountRenderScope(host, parent_id, host.current_scope_id, render, value);
        }
    }

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .dynamic = .{
        .parent = parent_id,
        .signal = signal_id,
        .render = render,
        .child_scope = child_scope,
    } });

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(signal_id, node_id);
}

/// Elem.create_each!
fn hostedCreateEach(parent_id: u64, signal_id: u64, keyer: RocBox, render: RocBox) callconv(.c) void {
    const host = currentHost();
    traceStderr("[HOST] create_each!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .each = .{
        .parent = parent_id,
        .signal = signal_id,
        .key = keyer,
        .render = render,
        .entries = .empty,
    } });

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(signal_id, node_id);
    renderEachNode(host, node_id);
}

/// Host.create_element!
fn hostedCreateElement(ops: *abi.RocOps, tag: RocStr) callconv(.c) u64 {
    const host = hostFromOps(ops);

    const tag_slice = tag.asSlice();
    if (traceEnabled()) {
        traceStderr("[HOST] create_element! tag=\"");
        traceStderr(tag_slice);
        traceStderr("\"\n");
    }
    const allocator = host.gpa.allocator();

    // Duplicate tag string for storage
    const tag_copy = allocator.dupe(u8, tag_slice) catch {
        tag.decref(ops);
        std.process.exit(1);
    };
    tag.decref(ops);

    const elem_id = host.next_elem_id;
    host.next_elem_id += 1;

    const elem = DomElement.init(elem_id, tag_copy, host.current_scope_id);
    host.dom_elements.append(allocator, elem) catch {
        allocator.free(tag_copy);
        std.process.exit(1);
    };

    return elem_id;
}

/// Host.create_event_filter!
fn hostedCreateEventFilter(ops: *abi.RocOps, source_id: u64, predicate: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_event_filter!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .event_filter = .{
        .source = source_id,
        .predicate = predicate,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_event_map!
fn hostedCreateEventMap(ops: *abi.RocOps, source_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_event_map!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .event_map = .{
        .source = source_id,
        .transform = transform,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_event_map_unit_i64_const!
fn hostedCreateEventMapUnitI64Const(source_id: u64, value: i64) callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_event_map_unit_i64_const!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .event_map_unit_i64_const = .{
        .source = source_id,
        .value = value,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);

    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_event_merge!
fn hostedCreateEventMerge(left_id: u64, right_id: u64) callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_event_merge!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .event_merge = .{
        .left = left_id,
        .right = right_id,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(left_id, node_id);
    host.addDependency(right_id, node_id);
    return node_id;
}

/// Host.create_event_with_latest!
fn hostedCreateEventWithLatest(ops: *abi.RocOps, event_id: u64, signal_id: u64, combine: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_event_with_latest!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .{ .event_with_latest = .{
        .event = event_id,
        .signal = signal_id,
        .combine = combine,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.create_event_source!
fn hostedCreateEventSource() callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_event_source!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, host.current_scope_id, .event_source);
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    return node_id;
}

/// Host.create_root!
fn hostedCreateRoot() callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_root!\n");

    const elem_id = host.next_elem_id;
    host.next_elem_id += 1;

    const allocator = host.gpa.allocator();
    const root_tag = allocator.dupe(u8, "root") catch {
        std.process.exit(1);
    };

    const elem = DomElement.init(elem_id, root_tag, host.current_scope_id);
    host.dom_elements.append(allocator, elem) catch {
        allocator.free(root_tag);
        std.process.exit(1);
    };

    return elem_id;
}

/// Host.create_signal_const!
fn hostedCreateSignalConst(ops: *abi.RocOps, value: NodeValue) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_const!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .signal_const);
    node.current_value = .{ .node_value = value };
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    return node_id;
}

/// Host.create_signal_const_i64!
fn hostedCreateSignalConstI64(value: i64) callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_signal_const_i64!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .signal_const);
    node.current_value = .{ .i64 = value };
    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);

    return node_id;
}

/// Host.create_signal_const_bool!
fn hostedCreateSignalConstBool(value: bool) callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_signal_const_bool!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .signal_const);
    node.current_value = .{ .bool = value };
    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);

    return node_id;
}

/// Host.create_signal_const_str!
fn hostedCreateSignalConstStr(ops: *abi.RocOps, value: RocStr) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_const_str!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .signal_const);
    node.current_value = .{ .str = value };
    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);

    return node_id;
}

/// Host.create_signal_state!
fn hostedCreateSignalState(ops: *abi.RocOps, initial: NodeValue) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_state!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_state = .{
        .update_event = null,
    } });
    node.current_value = .{ .node_value = initial };
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    return node_id;
}

/// Host.create_signal_fold!
fn hostedCreateSignalFold(ops: *abi.RocOps, initial: NodeValue, event_id: u64, step: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_fold!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_fold = .{
        .event = event_id,
        .step = step,
    } });
    node.current_value = .{ .node_value = initial };
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.create_signal_fold_i64!
fn hostedCreateSignalFoldI64(ops: *abi.RocOps, initial: i64, event_id: u64, step: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_fold_i64!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_fold_i64 = .{
        .event = event_id,
        .step = step,
    } });
    node.current_value = .{ .i64 = initial };
    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);

    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.create_signal_fold_bool_toggle!
fn hostedCreateSignalFoldBoolToggle(initial: bool, event_id: u64) callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_signal_fold_bool_toggle!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_fold_bool_toggle = .{
        .event = event_id,
    } });
    node.current_value = .{ .bool = initial };
    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);

    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.create_signal_hold!
fn hostedCreateSignalHold(ops: *abi.RocOps, initial: NodeValue, event_id: u64) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_hold!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_hold = .{
        .event = event_id,
    } });
    node.current_value = .{ .node_value = initial };
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.create_signal_map!
fn hostedCreateSignalMap(ops: *abi.RocOps, source_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_map!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_map = .{
        .source = source_id,
        .transform = transform,
    } });

    // Compute initial value from source
    if (source_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[source_id].current_value) |source_val| {
            traceStderr("[HOST] call initial signal map transform\n");
            const result_val = callRocTransform(host, transform, source_val);
            traceStderr("[HOST] initial signal map transform returned\n");
            node.current_value = result_val;
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_signal_map_i64_i64!
fn hostedCreateSignalMapI64I64(ops: *abi.RocOps, source_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_map_i64_i64!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_map_i64_i64 = .{
        .source = source_id,
        .transform = transform,
    } });

    if (source_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[source_id].current_value) |source_val| {
            node.current_value = .{ .i64 = callRocI64ToI64(host, transform, storedValueAsI64(source_val)) };
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_signal_map_i64_str!
fn hostedCreateSignalMapI64Str(ops: *abi.RocOps, source_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_map_i64_str!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_map_i64_str = .{
        .source = source_id,
        .transform = transform,
    } });

    if (source_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[source_id].current_value) |source_val| {
            node.current_value = .{ .str = callRocI64ToStr(host, transform, storedValueAsI64(source_val)) };
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_signal_map2!
fn hostedCreateSignalMap2(ops: *abi.RocOps, left_id: u64, right_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_map2!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_map2 = .{
        .left = left_id,
        .right = right_id,
        .transform = transform,
    } });

    if (left_id < host.graph_nodes.items.len and right_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[left_id].current_value) |left_value| {
            if (host.graph_nodes.items[right_id].current_value) |right_value| {
                node.current_value = callRocStep(host, transform, left_value, right_value);
            }
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(left_id, node_id);
    host.addDependency(right_id, node_id);
    return node_id;
}

/// Host.create_signal_map2_i64_i64!
fn hostedCreateSignalMap2I64I64(ops: *abi.RocOps, left_id: u64, right_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_map2_i64_i64!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_map2_i64_i64 = .{
        .left = left_id,
        .right = right_id,
        .transform = transform,
    } });

    if (left_id < host.graph_nodes.items.len and right_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[left_id].current_value) |left_value| {
            if (host.graph_nodes.items[right_id].current_value) |right_value| {
                node.current_value = .{ .i64 = callRocI64I64ToI64(host, transform, storedValueAsI64(left_value), storedValueAsI64(right_value)) };
            }
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(left_id, node_id);
    host.addDependency(right_id, node_id);
    return node_id;
}

/// Host.create_signal_map2_i64_i64_str!
fn hostedCreateSignalMap2I64I64Str(ops: *abi.RocOps, left_id: u64, right_id: u64, transform: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_map2_i64_i64_str!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_map2_i64_i64_str = .{
        .left = left_id,
        .right = right_id,
        .transform = transform,
    } });

    if (left_id < host.graph_nodes.items.len and right_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[left_id].current_value) |left_value| {
            if (host.graph_nodes.items[right_id].current_value) |right_value| {
                node.current_value = .{ .str = callRocI64I64ToStr(host, transform, storedValueAsI64(left_value), storedValueAsI64(right_value)) };
            }
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch std.process.exit(1);
    host.addDependency(left_id, node_id);
    host.addDependency(right_id, node_id);
    return node_id;
}

/// Host.create_signal_zip_with!
fn hostedCreateSignalZipWith(ops: *abi.RocOps, source_id: u64, event_id: u64, combine: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_zip_with!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, host.current_scope_id, .{ .signal_zip_with = .{
        .source = source_id,
        .event = event_id,
        .combine = combine,
    } });

    // Use source signal's initial value
    if (source_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[source_id].current_value) |value| {
            increfStoredValue(value);
            node.current_value = value;
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(source_id, node_id);
    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.set_text!
fn hostedSetText(ops: *abi.RocOps, elem_id: u64, text: RocStr) callconv(.c) void {
    const host = hostFromOps(ops);
    {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[HOST] set_text! elem={d} text=\"{s}\"\n", .{ elem_id, text.asSlice() }) catch "[HOST] set_text!\n";
        traceStderr(msg);
    }

    if (elem_id < host.dom_elements.items.len and host.dom_elements.items[elem_id].active) {
        const allocator = host.gpa.allocator();
        const text_slice = text.asSlice();
        const elem = &host.dom_elements.items[elem_id];

        if (elem.text) |old_text| {
            if (std.mem.eql(u8, old_text, text_slice)) {
                text.decref(ops);
                return;
            }
        }

        const text_copy = allocator.dupe(u8, text_slice) catch {
            text.decref(ops);
            std.process.exit(1);
        };
        text.decref(ops);

        if (elem.text) |old_text| {
            allocator.free(old_text);
        }

        elem.text = text_copy;
        elem.text_update_count += 1;
    } else {
        text.decref(ops);
    }
}

fn hostCreateElement(tag: RocStr) callconv(.c) u64 {
    return hostedCreateElement(currentRocOps(), tag);
}

fn hostCreateEventFilter(source_id: u64, predicate: RocBox) callconv(.c) u64 {
    return hostedCreateEventFilter(currentRocOps(), source_id, predicate);
}

fn hostCreateEventMap(source_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateEventMap(currentRocOps(), source_id, transform);
}

fn hostCreateEventMapUnitI64Const(source_id: u64, value: i64) callconv(.c) u64 {
    return hostedCreateEventMapUnitI64Const(source_id, value);
}

fn hostCreateEventWithLatest(event_id: u64, signal_id: u64, combine: RocBox) callconv(.c) u64 {
    return hostedCreateEventWithLatest(currentRocOps(), event_id, signal_id, combine);
}

fn hostCreateSignalConst(value: NodeValue) callconv(.c) u64 {
    return hostedCreateSignalConst(currentRocOps(), value);
}

fn hostCreateSignalConstI64(value: i64) callconv(.c) u64 {
    return hostedCreateSignalConstI64(value);
}

fn hostCreateSignalConstBool(value: bool) callconv(.c) u64 {
    return hostedCreateSignalConstBool(value);
}

fn hostCreateSignalConstStr(value: RocStr) callconv(.c) u64 {
    return hostedCreateSignalConstStr(currentRocOps(), value);
}

fn hostCreateSignalState(initial: NodeValue) callconv(.c) u64 {
    return hostedCreateSignalState(currentRocOps(), initial);
}

fn hostCreateSignalFold(initial: NodeValue, event_id: u64, step: RocBox) callconv(.c) u64 {
    return hostedCreateSignalFold(currentRocOps(), initial, event_id, step);
}

fn hostCreateSignalFoldI64(initial: i64, event_id: u64, step: RocBox) callconv(.c) u64 {
    return hostedCreateSignalFoldI64(currentRocOps(), initial, event_id, step);
}

fn hostCreateSignalFoldBoolToggle(initial: bool, event_id: u64) callconv(.c) u64 {
    return hostedCreateSignalFoldBoolToggle(initial, event_id);
}

fn hostCreateSignalHold(initial: NodeValue, event_id: u64) callconv(.c) u64 {
    return hostedCreateSignalHold(currentRocOps(), initial, event_id);
}

fn hostCreateSignalMap(source_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMap(currentRocOps(), source_id, transform);
}

fn hostCreateSignalMapI64I64(source_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMapI64I64(currentRocOps(), source_id, transform);
}

fn hostCreateSignalMapI64Str(source_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMapI64Str(currentRocOps(), source_id, transform);
}

fn hostCreateSignalMap2(left_id: u64, right_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMap2(currentRocOps(), left_id, right_id, transform);
}

fn hostCreateSignalMap2I64I64(left_id: u64, right_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMap2I64I64(currentRocOps(), left_id, right_id, transform);
}

fn hostCreateSignalMap2I64I64Str(left_id: u64, right_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMap2I64I64Str(currentRocOps(), left_id, right_id, transform);
}

fn hostCreateSignalZipWith(source_id: u64, event_id: u64, combine: RocBox) callconv(.c) u64 {
    return hostedCreateSignalZipWith(currentRocOps(), source_id, event_id, combine);
}

fn hostSetText(elem_id: u64, text: RocStr) callconv(.c) void {
    hostedSetText(currentRocOps(), elem_id, text);
}

// Reactive Graph Propagation

fn increfNodeValue(value: NodeValue) void {
    if (current_host) |host| {
        host.bench_counters.node_value_incref_count += 1;
    }
    abi.increfNodeValue(value, 1);
}

fn decrefNodeValue(value: NodeValue, ops: *abi.RocOps) void {
    hostFromOps(ops).bench_counters.node_value_decref_count += 1;
    abi.decrefNodeValue(value, ops);
}

fn increfStoredValue(value: StoredValue) void {
    switch (value) {
        .node_value => |node_value| increfNodeValue(node_value),
        .str => |str| str.incref(1),
        else => {},
    }
}

fn decrefStoredValue(value: StoredValue, ops: *abi.RocOps) void {
    switch (value) {
        .node_value => |node_value| decrefNodeValue(node_value, ops),
        .str => |str| str.decref(ops),
        else => {},
    }
}

fn storedValueAsNodeValue(value: StoredValue) NodeValue {
    return switch (value) {
        .node_value => |node_value| node_value,
        .i64 => |i| .{ .payload = .{ .nv_i64 = i }, .tag = .NvI64 },
        .bool => |b| .{ .payload = .{ .nv_bool = b }, .tag = .NvBool },
        .str => |str| .{ .payload = .{ .nv_str = str }, .tag = .NvStr },
        .unit => nodeValueUnit(),
    };
}

fn storedValueAsI64(value: StoredValue) i64 {
    return switch (value) {
        .i64 => |i| i,
        .node_value => |node_value| if (node_value.tag == .NvI64) node_value.payload.nv_i64 else failHost("scalar I64 node received non-I64 NodeValue"),
        else => failHost("scalar I64 node received non-I64 value"),
    };
}

fn storedValueAsBool(value: StoredValue) bool {
    return switch (value) {
        .bool => |b| b,
        .node_value => |node_value| if (node_value.tag == .NvBool) node_value.payload.nv_bool else failHost("scalar Bool node received non-Bool NodeValue"),
        else => failHost("scalar Bool node received non-Bool value"),
    };
}

fn storedValueAsStr(value: StoredValue) RocStr {
    return switch (value) {
        .str => |str| str,
        .node_value => |node_value| if (node_value.tag == .NvStr) node_value.payload.nv_str else failHost("scalar Str node received non-Str NodeValue"),
        else => failHost("scalar Str node received non-Str value"),
    };
}

fn nodeValueEqual(left: NodeValue, right: NodeValue) bool {
    if (current_host) |host| {
        host.bench_counters.node_value_equality_count += 1;
    }
    if (left.tag != right.tag) return false;
    return switch (left.tag) {
        .NvBool => left.payload.nv_bool == right.payload.nv_bool,
        .NvF64 => left.payload.nv_f64 == right.payload.nv_f64,
        .NvI64 => left.payload.nv_i64 == right.payload.nv_i64,
        .NvUnit => true,
        .NvStr => std.mem.eql(u8, left.payload.nv_str.asSlice(), right.payload.nv_str.asSlice()),
        .NvList => blk: {
            const left_items = left.payload.nv_list.items();
            const right_items = right.payload.nv_list.items();
            if (left_items.len != right_items.len) break :blk false;
            for (left_items, right_items) |left_item, right_item| {
                if (!nodeValueEqual(left_item, right_item)) break :blk false;
            }
            break :blk true;
        },
    };
}

fn storedValueEqual(left: StoredValue, right: StoredValue) bool {
    return switch (left) {
        .node_value => |left_node| switch (right) {
            .node_value => |right_node| nodeValueEqual(left_node, right_node),
            .i64 => |right_i64| left_node.tag == .NvI64 and left_node.payload.nv_i64 == right_i64,
            .bool => |right_bool| left_node.tag == .NvBool and left_node.payload.nv_bool == right_bool,
            .str => |right_str| left_node.tag == .NvStr and std.mem.eql(u8, left_node.payload.nv_str.asSlice(), right_str.asSlice()),
            .unit => left_node.tag == .NvUnit,
        },
        .i64 => |left_i64| switch (right) {
            .node_value => |right_node| right_node.tag == .NvI64 and right_node.payload.nv_i64 == left_i64,
            .i64 => |right_i64| left_i64 == right_i64,
            else => false,
        },
        .bool => |left_bool| switch (right) {
            .node_value => |right_node| right_node.tag == .NvBool and right_node.payload.nv_bool == left_bool,
            .bool => |right_bool| left_bool == right_bool,
            else => false,
        },
        .str => |left_str| switch (right) {
            .node_value => |right_node| right_node.tag == .NvStr and std.mem.eql(u8, left_str.asSlice(), right_node.payload.nv_str.asSlice()),
            .str => |right_str| std.mem.eql(u8, left_str.asSlice(), right_str.asSlice()),
            else => false,
        },
        .unit => switch (right) {
            .node_value => |right_node| right_node.tag == .NvUnit,
            .unit => true,
            else => false,
        },
    };
}

fn setSignalValue(host: *HostEnv, node_id: u64, value: StoredValue, changed: ?*std.ArrayListUnmanaged(u64)) bool {
    if (node_id >= host.graph_nodes.items.len) return false;
    if (!host.graph_nodes.items[node_id].active) return false;
    const ops = host.roc_ops orelse @panic("RocOps unavailable while updating signals");
    const node = &host.graph_nodes.items[node_id];
    host.bench_counters.signal_write_count += 1;

    if (node.current_value) |old_value| {
        if (storedValueEqual(old_value, value)) {
            host.bench_counters.signal_unchanged_count += 1;
            decrefStoredValue(value, ops);
            return false;
        }
        decrefStoredValue(old_value, ops);
    }
    node.current_value = value;
    host.bench_counters.signal_changed_count += 1;
    if (changed) |list| {
        list.append(host.gpa.allocator(), node_id) catch std.process.exit(1);
    }
    return true;
}

fn clearEventOccurrences(host: *HostEnv, node_id: u64) void {
    if (node_id >= host.graph_nodes.items.len) return;
    const ops = host.roc_ops orelse @panic("RocOps unavailable while clearing events");
    const node = &host.graph_nodes.items[node_id];
    for (node.event_values.items) |value| {
        decrefStoredValue(value, ops);
    }
    node.event_values.clearRetainingCapacity();
}

fn appendEventOccurrence(host: *HostEnv, node_id: u64, value: StoredValue) void {
    if (node_id >= host.graph_nodes.items.len) {
        const ops = host.roc_ops orelse @panic("RocOps unavailable while dropping event");
        decrefStoredValue(value, ops);
        return;
    }
    if (!host.graph_nodes.items[node_id].active) {
        const ops = host.roc_ops orelse @panic("RocOps unavailable while dropping inactive event");
        decrefStoredValue(value, ops);
        return;
    }
    host.graph_nodes.items[node_id].event_values.append(host.gpa.allocator(), value) catch std.process.exit(1);
}

fn callRocTransform(host: *HostEnv, transform: RocBox, input: StoredValue) StoredValue {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating transform");
    host.bench_counters.callback_transform_count += 1;
    var result: NodeValue = undefined;
    const input_node_value = storedValueAsNodeValue(input);
    increfNodeValue(input_node_value);
    var input_arg = input_node_value;
    const payload = abi.rocErasedCallablePayloadPtr(transform);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(transform));
    return .{ .node_value = result };
}

fn callRocStep(host: *HostEnv, step: RocBox, acc: StoredValue, event_val: StoredValue) StoredValue {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating step");
    host.bench_counters.callback_step_count += 1;
    const acc_node_value = storedValueAsNodeValue(acc);
    const event_node_value = storedValueAsNodeValue(event_val);
    increfNodeValue(acc_node_value);
    increfNodeValue(event_node_value);
    var args = extern struct {
        _0: NodeValue,
        _1: NodeValue,
    }{ ._0 = acc_node_value, ._1 = event_node_value };
    var result: NodeValue = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(step);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&args), abi.rocErasedCallableCapturePtr(step));
    return .{ .node_value = result };
}

fn callRocPredicate(host: *HostEnv, predicate: RocBox, input: StoredValue) bool {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating predicate");
    host.bench_counters.callback_predicate_count += 1;
    const input_node_value = storedValueAsNodeValue(input);
    increfNodeValue(input_node_value);
    var input_arg = input_node_value;
    var result: bool = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(predicate);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(predicate));
    return result;
}

fn callRocKey(host: *HostEnv, keyer: RocBox, input: StoredValue) RocStr {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating key");
    host.bench_counters.callback_key_count += 1;
    const input_node_value = storedValueAsNodeValue(input);
    increfNodeValue(input_node_value);
    var input_arg = input_node_value;
    var result: RocStr = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(keyer);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(keyer));
    return result;
}

fn callRocMountDynamic(host: *HostEnv, render: RocBox, value: StoredValue, parent_id: u64) void {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while mounting dynamic subtree");
    host.bench_counters.callback_render_count += 1;
    const node_value = storedValueAsNodeValue(value);
    increfNodeValue(node_value);
    var args = extern struct {
        _0: NodeValue,
        _1: u64,
    }{ ._0 = node_value, ._1 = parent_id };
    var ret: [0]u8 = .{};
    const payload = abi.rocErasedCallablePayloadPtr(render);
    payload.callable_fn_ptr(ops, @ptrCast(&ret), @ptrCast(&args), abi.rocErasedCallableCapturePtr(render));
}

fn callRocI64ToI64(host: *HostEnv, transform: RocBox, input: i64) i64 {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating I64 transform");
    host.bench_counters.callback_transform_count += 1;
    var input_arg = input;
    var result: i64 = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(transform);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(transform));
    return result;
}

fn callRocI64ToStr(host: *HostEnv, transform: RocBox, input: i64) RocStr {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating I64 to Str transform");
    host.bench_counters.callback_transform_count += 1;
    var input_arg = input;
    var result: RocStr = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(transform);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(transform));
    return result;
}

fn callRocI64I64ToI64(host: *HostEnv, step: RocBox, left: i64, right: i64) i64 {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating I64 step");
    host.bench_counters.callback_step_count += 1;
    var args = extern struct {
        _0: i64,
        _1: i64,
    }{ ._0 = left, ._1 = right };
    var result: i64 = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(step);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&args), abi.rocErasedCallableCapturePtr(step));
    return result;
}

fn callRocI64I64ToStr(host: *HostEnv, transform: RocBox, left: i64, right: i64) RocStr {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating I64 pair to Str transform");
    host.bench_counters.callback_step_count += 1;
    var args = extern struct {
        _0: i64,
        _1: i64,
    }{ ._0 = left, ._1 = right };
    var result: RocStr = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(transform);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&args), abi.rocErasedCallableCapturePtr(transform));
    return result;
}

fn mountRenderScope(host: *HostEnv, parent_elem_id: u64, parent_scope_id: u64, render: RocBox, value: StoredValue) u64 {
    const scope_id = host.createScope(parent_scope_id);
    const previous_scope = host.pushScope(scope_id);
    callRocMountDynamic(host, render, value, parent_elem_id);
    host.popScope(previous_scope);
    return scope_id;
}

fn clearNodeValues(host: *HostEnv, node_id: u64) void {
    if (node_id >= host.graph_nodes.items.len) return;
    const ops = host.roc_ops orelse @panic("RocOps unavailable while clearing node");
    const node = &host.graph_nodes.items[node_id];
    if (node.released) return;
    if (node.current_value) |value| {
        decrefStoredValue(value, ops);
        node.current_value = null;
    }
    for (node.event_values.items) |value| {
        decrefStoredValue(value, ops);
    }
    node.event_values.clearRetainingCapacity();
}

fn unmountScope(host: *HostEnv, scope_id: u64) void {
    if (scope_id >= host.scopes.items.len) return;
    if (!host.scopes.items[scope_id].active) return;
    const ops = host.roc_ops orelse @panic("RocOps unavailable while unmounting scope");

    const unmount_events = host.scopes.items[scope_id].on_unmount_events.items;
    for (unmount_events) |event_id| {
        fireEvent(host, event_id, nodeValueUnit());
    }

    for (host.scopes.items, 0..) |scope, child_scope_id| {
        if (scope.active and scope.parent == scope_id) {
            unmountScope(host, @intCast(child_scope_id));
        }
    }

    for (host.dom_elements.items) |*elem| {
        if (elem.scope_id == scope_id) {
            elem.active = false;
            elem.bound_click_event = null;
            elem.bound_text_signal = null;
        }
    }

    for (host.graph_nodes.items, 0..) |*node, node_id| {
        if (node.scope_id == scope_id) {
            _ = node_id;
            node.active = false;
            node.releaseResources(host.gpa.allocator(), ops);
        }
    }

    host.scopes.items[scope_id].active = false;
}

fn keyedScopeIndex(entries: []const KeyedScope, key: []const u8) ?usize {
    for (entries, 0..) |entry, index| {
        if (std.mem.eql(u8, entry.key, key)) return index;
    }
    return null;
}

fn failHost(message: []const u8) noreturn {
    writeStderr("HOST ERROR: ");
    writeStderr(message);
    writeStderr("\n");
    std.process.exit(1);
}

fn reorderChildrenForKeyedScopes(host: *HostEnv, parent_id: u64, entries: []const KeyedScope) void {
    if (parent_id >= host.dom_elements.items.len) return;
    const allocator = host.gpa.allocator();
    const old_children = host.dom_elements.items[parent_id].children.items;
    var reordered: std.ArrayListUnmanaged(u64) = .empty;
    defer reordered.deinit(allocator);

    for (entries) |entry| {
        for (old_children) |child_id| {
            if (child_id < host.dom_elements.items.len) {
                const child = &host.dom_elements.items[child_id];
                if (child.active and child.scope_id == entry.scope_id) {
                    reordered.append(allocator, child_id) catch std.process.exit(1);
                }
            }
        }
    }

    host.dom_elements.items[parent_id].children.clearRetainingCapacity();
    host.dom_elements.items[parent_id].children.appendSlice(allocator, reordered.items) catch std.process.exit(1);
}

fn renderDynamicNode(host: *HostEnv, node_id: u64) void {
    if (node_id >= host.graph_nodes.items.len) return;
    if (!host.graph_nodes.items[node_id].active) return;

    const data = switch (host.graph_nodes.items[node_id].kind) {
        .dynamic => |data| data,
        else => return,
    };

    if (data.child_scope) |scope_id| {
        unmountScope(host, scope_id);
    }

    var new_scope: ?u64 = null;
    if (data.signal < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[data.signal].current_value) |value| {
            new_scope = mountRenderScope(host, data.parent, host.graph_nodes.items[node_id].scope_id, data.render, value);
        }
    }

    if (node_id < host.graph_nodes.items.len and host.graph_nodes.items[node_id].active) {
        switch (host.graph_nodes.items[node_id].kind) {
            .dynamic => |*dynamic| dynamic.child_scope = new_scope,
            else => {},
        }
    }
}

fn renderEachNode(host: *HostEnv, node_id: u64) void {
    if (node_id >= host.graph_nodes.items.len) return;
    if (!host.graph_nodes.items[node_id].active) return;

    const copied = switch (host.graph_nodes.items[node_id].kind) {
        .each => |*data| blk: {
            const entries = data.entries;
            data.entries = .empty;
            break :blk .{
                .parent = data.parent,
                .signal = data.signal,
                .key = data.key,
                .render = data.render,
                .old_entries = entries,
                .scope_id = host.graph_nodes.items[node_id].scope_id,
            };
        },
        else => return,
    };

    const allocator = host.gpa.allocator();
    var old_entries = copied.old_entries;
    var used_old = allocator.alloc(bool, old_entries.items.len) catch std.process.exit(1);
    defer allocator.free(used_old);
    @memset(used_old, false);

    var new_entries: std.ArrayListUnmanaged(KeyedScope) = .empty;
    errdefer {
        for (new_entries.items) |entry| allocator.free(entry.key);
        new_entries.deinit(allocator);
    }

    if (copied.signal < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[copied.signal].current_value) |stored_list_value| {
            const list_value = switch (stored_list_value) {
                .node_value => |node_value| node_value,
                else => failHost("Elem.each expected an NvList signal value"),
            };
            if (list_value.tag != .NvList) failHost("Elem.each expected an NvList signal value");
            const items = list_value.payload.nv_list.items();
            for (items) |item| {
                const key = callRocKey(host, copied.key, .{ .node_value = item });
                const key_slice = key.asSlice();
                if (keyedScopeIndex(new_entries.items, key_slice) != null) {
                    key.decref(host.roc_ops orelse @panic("RocOps unavailable while handling key"));
                    failHost("Elem.each received duplicate key");
                }

                const key_copy = allocator.dupe(u8, key_slice) catch std.process.exit(1);
                key.decref(host.roc_ops orelse @panic("RocOps unavailable while handling key"));

                const scope_id = if (keyedScopeIndex(old_entries.items, key_copy)) |old_index| blk: {
                    used_old[old_index] = true;
                    break :blk old_entries.items[old_index].scope_id;
                } else mountRenderScope(host, copied.parent, copied.scope_id, copied.render, .{ .node_value = item });

                new_entries.append(allocator, .{ .key = key_copy, .scope_id = scope_id }) catch std.process.exit(1);
            }
        }
    }

    for (old_entries.items, 0..) |entry, index| {
        if (!used_old[index]) {
            unmountScope(host, entry.scope_id);
        }
        allocator.free(entry.key);
    }
    old_entries.deinit(allocator);

    reorderChildrenForKeyedScopes(host, copied.parent, new_entries.items);

    if (node_id < host.graph_nodes.items.len and host.graph_nodes.items[node_id].active) {
        switch (host.graph_nodes.items[node_id].kind) {
            .each => |*data| data.entries = new_entries,
            else => {
                for (new_entries.items) |entry| allocator.free(entry.key);
                new_entries.deinit(allocator);
            },
        }
    } else {
        for (new_entries.items) |entry| allocator.free(entry.key);
        new_entries.deinit(allocator);
    }
}

fn fireEvent(host: *HostEnv, node_id: u64, value: NodeValue) void {
    if (host.propagation_depth > 0) {
        host.pending_events.append(host.gpa.allocator(), .{ .node_id = node_id, .value = value }) catch std.process.exit(1);
        return;
    }

    processEvent(host, node_id, value);
    while (host.pending_events.items.len > 0) {
        const pending = host.pending_events.orderedRemove(0);
        processEvent(host, pending.node_id, pending.value);
    }
}

fn processEvent(host: *HostEnv, node_id: u64, value: NodeValue) void {
    host.bench_counters.event_count += 1;
    host.propagation_depth += 1;
    defer host.propagation_depth -= 1;

    if (node_id >= host.graph_nodes.items.len) return;
    if (!host.graph_nodes.items[node_id].active) {
        const ops = host.roc_ops orelse @panic("RocOps unavailable while dropping inactive event");
        decrefNodeValue(value, ops);
        return;
    }

    const allocator = host.gpa.allocator();

    for (host.graph_nodes.items, 0..) |n, i| {
        if (isEventNode(n.kind)) clearEventOccurrences(host, @intCast(i));
    }

    appendEventOccurrence(host, node_id, .{ .node_value = value });

    const affected = allocator.alloc(bool, host.graph_nodes.items.len) catch std.process.exit(1);
    defer allocator.free(affected);
    @memset(affected, false);

    collectAffected(host, node_id, affected);

    var scheduled = allocator.alloc(bool, host.graph_nodes.items.len) catch std.process.exit(1);
    defer allocator.free(scheduled);
    @memset(scheduled, false);

    var changed_signals: std.ArrayListUnmanaged(u64) = .empty;
    defer changed_signals.deinit(allocator);

    var remaining = countAffected(affected);
    while (remaining > 0) {
        var progressed = false;
        for (affected, 0..) |is_affected, dep_id| {
            if (!is_affected or scheduled[dep_id]) continue;
            if (!inputsReady(host, @intCast(dep_id), affected, scheduled)) continue;

            evaluateNode(host, @intCast(dep_id), &changed_signals);
            scheduled[dep_id] = true;
            remaining -= 1;
            progressed = true;
        }
        if (!progressed) failHost("cycle detected in signals graph");
    }

    updateDirtyTextBindings(host, changed_signals.items);
}

fn isEventNode(kind: NodeKind) bool {
    return switch (kind) {
        .event_source, .event_map, .event_map_unit_i64_const, .event_filter, .event_merge, .event_with_latest => true,
        else => false,
    };
}

fn countAffected(affected: []const bool) usize {
    var count: usize = 0;
    for (affected) |item| {
        if (item) count += 1;
    }
    return count;
}

fn collectAffected(host: *HostEnv, node_id: u64, affected: []bool) void {
    if (node_id >= host.graph_nodes.items.len) return;

    const node = &host.graph_nodes.items[node_id];
    for (node.dependents.items) |dep_id| {
        if (dep_id < host.graph_nodes.items.len and host.graph_nodes.items[dep_id].active and !affected[dep_id]) {
            affected[dep_id] = true;
            collectAffected(host, dep_id, affected);
        }
    }
}

fn signalChanged(changed_signals: []const u64, node_id: u64) bool {
    for (changed_signals) |changed_id| {
        if (changed_id == node_id) return true;
    }
    return false;
}

fn inputsReady(host: *HostEnv, node_id: u64, affected: []const bool, scheduled: []const bool) bool {
    if (node_id >= host.graph_nodes.items.len) return true;
    const node = &host.graph_nodes.items[node_id];
    return switch (node.kind) {
        .event_source, .signal_const => true,
        .event_map => |data| inputReady(data.source, affected, scheduled),
        .event_map_unit_i64_const => |data| inputReady(data.source, affected, scheduled),
        .event_filter => |data| inputReady(data.source, affected, scheduled),
        .event_merge => |data| inputReady(data.left, affected, scheduled) and inputReady(data.right, affected, scheduled),
        .event_with_latest => |data| inputReady(data.event, affected, scheduled),
        .signal_state => |data| if (data.update_event) |event_id| inputReady(event_id, affected, scheduled) else true,
        .signal_map => |data| inputReady(data.source, affected, scheduled),
        .signal_map_i64_i64 => |data| inputReady(data.source, affected, scheduled),
        .signal_map_i64_str => |data| inputReady(data.source, affected, scheduled),
        .signal_map2 => |data| inputReady(data.left, affected, scheduled) and inputReady(data.right, affected, scheduled),
        .signal_map2_i64_i64 => |data| inputReady(data.left, affected, scheduled) and inputReady(data.right, affected, scheduled),
        .signal_map2_i64_i64_str => |data| inputReady(data.left, affected, scheduled) and inputReady(data.right, affected, scheduled),
        .signal_hold => |data| inputReady(data.event, affected, scheduled),
        .signal_fold => |data| inputReady(data.event, affected, scheduled),
        .signal_fold_i64 => |data| inputReady(data.event, affected, scheduled),
        .signal_fold_bool_toggle => |data| inputReady(data.event, affected, scheduled),
        .signal_zip_with => |data| inputReady(data.event, affected, scheduled) and inputReady(data.source, affected, scheduled),
        .dynamic => |data| inputReady(data.signal, affected, scheduled),
        .each => |data| inputReady(data.signal, affected, scheduled),
    };
}

fn inputReady(input_id: u64, affected: []const bool, scheduled: []const bool) bool {
    if (input_id >= affected.len) return true;
    return !affected[input_id] or scheduled[input_id];
}

fn evaluateNode(host: *HostEnv, node_id: u64, changed_signals: *std.ArrayListUnmanaged(u64)) void {
    if (node_id >= host.graph_nodes.items.len) return;
    if (!host.graph_nodes.items[node_id].active) return;

    host.bench_counters.evaluated_node_count += 1;
    const node = &host.graph_nodes.items[node_id];
    switch (node.kind) {
        .event_source => {}, // Value already set by fireEvent
        .signal_const => {}, // Constant, never changes
        .signal_state => |data| {
            if (data.update_event) |event_id| {
                if (event_id < host.graph_nodes.items.len) {
                    for (host.graph_nodes.items[event_id].event_values.items) |val| {
                        increfStoredValue(val);
                        _ = setSignalValue(host, node_id, val, changed_signals);
                    }
                }
            }
        },

        .event_map => |data| {
            if (data.source < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.source].event_values.items) |input| {
                    appendEventOccurrence(host, node_id, callRocTransform(host, data.transform, input));
                }
            }
        },

        .event_map_unit_i64_const => |data| {
            if (data.source < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.source].event_values.items) |_| {
                    appendEventOccurrence(host, node_id, .{ .i64 = data.value });
                }
            }
        },

        .event_merge => |data| {
            if (data.left < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.left].event_values.items) |val| {
                    increfStoredValue(val);
                    appendEventOccurrence(host, node_id, val);
                }
            }
            if (data.right < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.right].event_values.items) |val| {
                    increfStoredValue(val);
                    appendEventOccurrence(host, node_id, val);
                }
            }
        },

        .event_filter => |data| {
            if (data.source < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.source].event_values.items) |input| {
                    if (callRocPredicate(host, data.predicate, input)) {
                        increfStoredValue(input);
                        appendEventOccurrence(host, node_id, input);
                    }
                }
            }
        },

        .event_with_latest => |data| {
            if (data.event < host.graph_nodes.items.len and data.signal < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.event].event_values.items) |event_val| {
                    if (host.graph_nodes.items[data.signal].current_value) |signal_val| {
                        appendEventOccurrence(host, node_id, callRocStep(host, data.combine, event_val, signal_val));
                    }
                }
            }
        },

        .signal_map => |data| {
            if (data.source < host.graph_nodes.items.len and signalChanged(changed_signals.items, data.source)) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    _ = setSignalValue(host, node_id, callRocTransform(host, data.transform, input), changed_signals);
                }
            }
        },

        .signal_map_i64_i64 => |data| {
            if (data.source < host.graph_nodes.items.len and signalChanged(changed_signals.items, data.source)) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    _ = setSignalValue(host, node_id, .{ .i64 = callRocI64ToI64(host, data.transform, storedValueAsI64(input)) }, changed_signals);
                }
            }
        },

        .signal_map_i64_str => |data| {
            if (data.source < host.graph_nodes.items.len and signalChanged(changed_signals.items, data.source)) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    _ = setSignalValue(host, node_id, .{ .str = callRocI64ToStr(host, data.transform, storedValueAsI64(input)) }, changed_signals);
                }
            }
        },

        .signal_map2 => |data| {
            if ((signalChanged(changed_signals.items, data.left) or signalChanged(changed_signals.items, data.right)) and
                data.left < host.graph_nodes.items.len and data.right < host.graph_nodes.items.len)
            {
                if (host.graph_nodes.items[data.left].current_value) |left_value| {
                    if (host.graph_nodes.items[data.right].current_value) |right_value| {
                        _ = setSignalValue(host, node_id, callRocStep(host, data.transform, left_value, right_value), changed_signals);
                    }
                }
            }
        },

        .signal_map2_i64_i64 => |data| {
            if ((signalChanged(changed_signals.items, data.left) or signalChanged(changed_signals.items, data.right)) and
                data.left < host.graph_nodes.items.len and data.right < host.graph_nodes.items.len)
            {
                if (host.graph_nodes.items[data.left].current_value) |left_value| {
                    if (host.graph_nodes.items[data.right].current_value) |right_value| {
                        _ = setSignalValue(host, node_id, .{ .i64 = callRocI64I64ToI64(host, data.transform, storedValueAsI64(left_value), storedValueAsI64(right_value)) }, changed_signals);
                    }
                }
            }
        },

        .signal_map2_i64_i64_str => |data| {
            if ((signalChanged(changed_signals.items, data.left) or signalChanged(changed_signals.items, data.right)) and
                data.left < host.graph_nodes.items.len and data.right < host.graph_nodes.items.len)
            {
                if (host.graph_nodes.items[data.left].current_value) |left_value| {
                    if (host.graph_nodes.items[data.right].current_value) |right_value| {
                        _ = setSignalValue(host, node_id, .{ .str = callRocI64I64ToStr(host, data.transform, storedValueAsI64(left_value), storedValueAsI64(right_value)) }, changed_signals);
                    }
                }
            }
        },

        .signal_hold => |data| {
            if (data.event < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.event].event_values.items) |val| {
                    increfStoredValue(val);
                    _ = setSignalValue(host, node_id, val, changed_signals);
                }
            }
        },

        .signal_fold => |data| {
            if (data.event < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.event].event_values.items) |event_val| {
                    if (node.current_value) |acc| {
                        _ = setSignalValue(host, node_id, callRocStep(host, data.step, acc, event_val), changed_signals);
                    }
                }
            }
        },

        .signal_fold_i64 => |data| {
            if (data.event < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.event].event_values.items) |event_val| {
                    if (node.current_value) |acc| {
                        _ = setSignalValue(host, node_id, .{ .i64 = callRocI64I64ToI64(host, data.step, storedValueAsI64(acc), storedValueAsI64(event_val)) }, changed_signals);
                    }
                }
            }
        },

        .signal_fold_bool_toggle => |data| {
            if (data.event < host.graph_nodes.items.len) {
                for (host.graph_nodes.items[data.event].event_values.items) |_| {
                    if (node.current_value) |current| {
                        _ = setSignalValue(host, node_id, .{ .bool = !storedValueAsBool(current) }, changed_signals);
                    }
                }
            }
        },

        .signal_zip_with => |data| {
            if (data.source < host.graph_nodes.items.len and data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |signal_val| {
                    for (host.graph_nodes.items[data.event].event_values.items) |event_val| {
                        _ = setSignalValue(host, node_id, callRocStep(host, data.combine, signal_val, event_val), changed_signals);
                    }
                }
            }
        },

        .dynamic => {
            renderDynamicNode(host, node_id);
        },

        .each => {
            renderEachNode(host, node_id);
        },
    }
}

fn updateElementText(host: *HostEnv, elem_id: u64) void {
    if (elem_id >= host.dom_elements.items.len) return;

    const elem = &host.dom_elements.items[elem_id];
    if (!elem.active) return;
    if (elem.bound_text_signal) |signal_id| {
        if (signal_id < host.graph_nodes.items.len) {
            if (!host.graph_nodes.items[signal_id].active) return;
            if (host.graph_nodes.items[signal_id].current_value) |stored_value| {
                const allocator = host.gpa.allocator();
                const val = storedValueAsNodeValue(stored_value);

                switch (val.tag) {
                    .NvStr => {
                        const text = val.payload.nv_str.asSlice();
                        if (elem.text) |old_text| {
                            if (std.mem.eql(u8, old_text, text)) return;
                        }
                        const text_copy = allocator.dupe(u8, text) catch std.process.exit(1);
                        if (elem.text) |old_text| {
                            allocator.free(old_text);
                        }
                        elem.text = text_copy;
                        elem.text_update_count += 1;
                    },
                    .NvI64 => {
                        var buf: [32]u8 = undefined;
                        const text = std.fmt.bufPrint(&buf, "{d}", .{val.payload.nv_i64}) catch std.process.exit(1);
                        if (elem.text) |old_text| {
                            if (std.mem.eql(u8, old_text, text)) return;
                        }
                        const text_copy = allocator.dupe(u8, text) catch std.process.exit(1);
                        if (elem.text) |old_text| {
                            allocator.free(old_text);
                        }
                        elem.text = text_copy;
                        elem.text_update_count += 1;
                    },
                    else => {},
                }
            }
        }
    }
}

fn updateAllTextBindings(host: *HostEnv) void {
    for (host.dom_elements.items, 0..) |_, i| {
        updateElementText(host, @intCast(i));
    }
}

fn updateDirtyTextBindings(host: *HostEnv, changed_signals: []const u64) void {
    for (host.dom_elements.items, 0..) |elem, i| {
        if (!elem.active) continue;
        if (elem.bound_text_signal) |signal_id| {
            if (signalChanged(changed_signals, signal_id)) {
                updateElementText(host, @intCast(i));
            }
        }
    }
}

// Boundary Benchmarking

const Prototype = enum {
    baseline_node_value,
    scalar_fast_paths,
    host_value_handles,
    generated_boundary_shims,
};

const prototype_list = [_]Prototype{
    .baseline_node_value,
    .scalar_fast_paths,
    .host_value_handles,
    .generated_boundary_shims,
};

fn prototypeName(prototype: Prototype) []const u8 {
    return switch (prototype) {
        .baseline_node_value => "A.baseline_node_value",
        .scalar_fast_paths => "B.scalar_fast_paths",
        .host_value_handles => "C.host_value_handles",
        .generated_boundary_shims => "D.generated_boundary_shims",
    };
}

fn parsePrototypeFilter(arg: []const u8) ?Prototype {
    if (std.mem.eql(u8, arg, "A") or std.mem.eql(u8, arg, "baseline") or std.mem.eql(u8, arg, "baseline_node_value")) return .baseline_node_value;
    if (std.mem.eql(u8, arg, "B") or std.mem.eql(u8, arg, "scalar") or std.mem.eql(u8, arg, "scalar_fast_paths")) return .scalar_fast_paths;
    if (std.mem.eql(u8, arg, "C") or std.mem.eql(u8, arg, "handles") or std.mem.eql(u8, arg, "host_value_handles")) return .host_value_handles;
    if (std.mem.eql(u8, arg, "D") or std.mem.eql(u8, arg, "generated") or std.mem.eql(u8, arg, "generated_boundary_shims")) return .generated_boundary_shims;
    if (std.mem.eql(u8, arg, "all")) return null;
    failHost("unknown benchmark prototype");
}

const BenchmarkResult = struct {
    prototype: []const u8,
    case_name: []const u8,
    sample: usize,
    iterations: usize,
    elapsed_ns: u64,
    operations: u64,
    allocs: usize,
    deallocs: usize,
    retained_alloc_delta: isize,
    counters: BenchCounters,
    text_updates: u64,
    active_graph_nodes: u64,
};

fn printBenchmarkHeader() void {
    writeStdout("prototype,case,sample,iterations,elapsed_ns,operations,allocs,deallocs,retained_alloc_delta,node_value_increfs,node_value_decrefs,node_value_equality_checks,boundary_encodes,boundary_decodes,callbacks,events,evaluated_nodes,signal_writes,signal_changes,signal_suppressed,text_updates,active_graph_nodes\n");
}

fn printBenchmarkResult(result: BenchmarkResult) void {
    printStdout(
        "{s},{s},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            result.prototype,
            result.case_name,
            result.sample,
            result.iterations,
            result.elapsed_ns,
            result.operations,
            result.allocs,
            result.deallocs,
            result.retained_alloc_delta,
            result.counters.node_value_incref_count,
            result.counters.node_value_decref_count,
            result.counters.node_value_equality_count,
            result.counters.boundary_encode_count,
            result.counters.boundary_decode_count,
            result.counters.callbackCount(),
            result.counters.event_count,
            result.counters.evaluated_node_count,
            result.counters.signal_write_count,
            result.counters.signal_changed_count,
            result.counters.signal_unchanged_count,
            result.text_updates,
            result.active_graph_nodes,
        },
    );
}

fn nvI64(value: i64) NodeValue {
    return .{ .payload = .{ .nv_i64 = value }, .tag = .NvI64 };
}

fn encodeI64NodeValue(host: *HostEnv, value: i64) NodeValue {
    host.bench_counters.boundary_encode_count += 1;
    return nvI64(value);
}

fn decodeI64NodeValue(host: *HostEnv, value: NodeValue) i64 {
    host.bench_counters.boundary_decode_count += 1;
    if (value.tag != .NvI64) failHost("benchmark expected NvI64");
    return value.payload.nv_i64;
}

fn encodeI64ListNodeValue(host: *HostEnv, ops: *abi.RocOps, values: []const NodeValue) NodeValue {
    host.bench_counters.boundary_encode_count += 1;
    return .{
        .payload = .{ .nv_list = abi.RocList(NodeValue).fromSlice(values, ops) },
        .tag = .NvList,
    };
}

fn decodeI64ListSum(host: *HostEnv, value: NodeValue) i64 {
    host.bench_counters.boundary_decode_count += 1;
    if (value.tag != .NvList) failHost("benchmark expected NvList");
    var sum: i64 = 0;
    for (value.payload.nv_list.items()) |item| {
        sum += decodeI64NodeValue(host, item);
    }
    return sum;
}

const CounterShim = extern struct {
    count: i64,
};

const AppShim = extern struct {
    left: CounterShim,
    right: CounterShim,
};

const OpaqueShimBox = *anyopaque;

fn callShimPayloadInit(ops: *abi.RocOps) OpaqueShimBox {
    _ = ops;
    return abi.roc_shim_payload_init();
}

fn callShimPayloadTotal(ops: *abi.RocOps, payload: OpaqueShimBox) i64 {
    _ = ops;
    return abi.roc_shim_payload_total(payload);
}

fn callShimPayloadItemKey(ops: *abi.RocOps, payload: OpaqueShimBox) RocStr {
    _ = ops;
    return abi.roc_shim_payload_item_key(payload);
}

fn callShimPayloadItemLabel(ops: *abi.RocOps, payload: OpaqueShimBox) RocStr {
    _ = ops;
    return abi.roc_shim_payload_item_label(payload);
}

fn callShimPayloadItemsLen(ops: *abi.RocOps, payload: OpaqueShimBox) u64 {
    _ = ops;
    return abi.roc_shim_payload_items_len(payload);
}

const HandleStore = struct {
    scalars: []i64,
    lists: [][]i64,

    fn init(allocator: std.mem.Allocator) !HandleStore {
        const scalars = try allocator.alloc(i64, 8);
        const lists = try allocator.alloc([]i64, 4);
        for (lists) |*items| {
            items.* = try allocator.alloc(i64, 16);
        }
        return .{ .scalars = scalars, .lists = lists };
    }

    fn deinit(self: *HandleStore, allocator: std.mem.Allocator) void {
        for (self.lists) |items| allocator.free(items);
        allocator.free(self.lists);
        allocator.free(self.scalars);
    }

    fn putScalar(self: *HandleStore, slot: usize, value: i64) u32 {
        self.scalars[slot] = value;
        return @intCast(slot);
    }

    fn getScalar(self: *const HandleStore, handle: u32) i64 {
        return self.scalars[handle];
    }

    fn putList(self: *HandleStore, slot: usize, values: []const i64) u32 {
        @memcpy(self.lists[slot][0..values.len], values);
        return @intCast(slot);
    }

    fn list(self: *const HandleStore, handle: u32) []const i64 {
        return self.lists[handle][0..16];
    }
};

fn benchMicroScalar(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    _ = ops;
    var checksum: i64 = 0;
    switch (prototype) {
        .baseline_node_value => {
            var previous = encodeI64NodeValue(host, 0);
            for (0..iterations) |i| {
                const input = encodeI64NodeValue(host, @as(i64, @intCast(i)));
                const decoded = decodeI64NodeValue(host, input);
                const output = encodeI64NodeValue(host, decoded + 1);
                std.mem.doNotOptimizeAway(output);
                if (!nodeValueEqual(previous, output)) {
                    checksum += decodeI64NodeValue(host, output);
                }
                previous = output;
            }
        },
        .scalar_fast_paths => {
            var previous: i64 = 0;
            for (0..iterations) |i| {
                const output = @as(i64, @intCast(i)) + 1;
                std.mem.doNotOptimizeAway(output);
                if (previous != output) checksum += output;
                previous = output;
            }
        },
        .host_value_handles => {
            var store = HandleStore.init(host.gpa.allocator()) catch std.process.exit(1);
            defer store.deinit(host.gpa.allocator());
            var previous_value: i64 = 0;
            for (0..iterations) |i| {
                const output = @as(i64, @intCast(i)) + 1;
                const next = store.putScalar(1, output);
                const next_value = store.getScalar(next);
                std.mem.doNotOptimizeAway(next_value);
                if (previous_value != next_value) checksum += next_value;
                previous_value = next_value;
            }
        },
        .generated_boundary_shims => {
            var previous = CounterShim{ .count = 0 };
            for (0..iterations) |i| {
                host.bench_counters.boundary_decode_count += 1;
                const input = CounterShim{ .count = @as(i64, @intCast(i)) };
                const output = CounterShim{ .count = input.count + 1 };
                host.bench_counters.boundary_encode_count += 1;
                std.mem.doNotOptimizeAway(output);
                if (previous.count != output.count) checksum += output.count;
                previous = output;
            }
        },
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations);
}

fn benchMicroListRecord(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    var checksum: i64 = 0;
    switch (prototype) {
        .baseline_node_value, .scalar_fast_paths => {
            for (0..iterations) |i| {
                const base_value = @as(i64, @intCast(i));
                var items = [_]NodeValue{
                    encodeI64NodeValue(host, base_value),
                    encodeI64NodeValue(host, base_value + 1),
                    encodeI64NodeValue(host, base_value + 2),
                    encodeI64NodeValue(host, base_value + 3),
                };
                const list = encodeI64ListNodeValue(host, ops, &items);
                const sum = decodeI64ListSum(host, list);
                std.mem.doNotOptimizeAway(sum);
                checksum += sum;
                decrefNodeValue(list, ops);
            }
        },
        .host_value_handles => {
            var store = HandleStore.init(host.gpa.allocator()) catch std.process.exit(1);
            defer store.deinit(host.gpa.allocator());
            var values: [16]i64 = undefined;
            for (0..iterations) |i| {
                const base_value = @as(i64, @intCast(i));
                values[0] = base_value;
                values[1] = base_value + 1;
                values[2] = base_value + 2;
                values[3] = base_value + 3;
                const handle = store.putList(0, &values);
                const borrowed = store.list(handle);
                const sum = borrowed[0] + borrowed[1] + borrowed[2] + borrowed[3];
                std.mem.doNotOptimizeAway(sum);
                checksum += sum;
            }
        },
        .generated_boundary_shims => {
            for (0..iterations) |i| {
                host.bench_counters.boundary_decode_count += 1;
                const base_value = @as(i64, @intCast(i));
                const app = AppShim{
                    .left = .{ .count = base_value },
                    .right = .{ .count = base_value + 1 },
                };
                host.bench_counters.boundary_encode_count += 1;
                const sum = app.left.count + app.right.count;
                std.mem.doNotOptimizeAway(sum);
                checksum += sum;
            }
        },
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations * 4);
}

fn benchMicroGeneratedAppShim(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    if (prototype != .generated_boundary_shims) failHost("generated app shim benchmark requires prototype D");

    var checksum: i64 = 0;
    for (0..iterations) |_| {
        host.bench_counters.callback_transform_count += 8;
        host.bench_counters.boundary_encode_count += 4;
        host.bench_counters.boundary_decode_count += 4;

        const total_payload = callShimPayloadInit(ops);
        checksum += callShimPayloadTotal(ops, total_payload);
        std.mem.doNotOptimizeAway(checksum);

        const key_payload = callShimPayloadInit(ops);
        const key = callShimPayloadItemKey(ops, key_payload);
        checksum += @intCast(key.asSlice().len);
        std.mem.doNotOptimizeAway(checksum);
        key.decref(ops);

        const label_payload = callShimPayloadInit(ops);
        const label = callShimPayloadItemLabel(ops, label_payload);
        checksum += @intCast(label.asSlice().len);
        std.mem.doNotOptimizeAway(checksum);
        label.decref(ops);

        const items_payload = callShimPayloadInit(ops);
        checksum += @intCast(callShimPayloadItemsLen(ops, items_payload));
        std.mem.doNotOptimizeAway(checksum);
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations * 8);
}

fn benchMicroEquality(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    var checksum: u64 = 0;
    switch (prototype) {
        .baseline_node_value, .scalar_fast_paths => {
            var left_items: [16]NodeValue = undefined;
            var right_items: [16]NodeValue = undefined;
            for (&left_items, 0..) |*item, i| item.* = encodeI64NodeValue(host, @intCast(i));
            for (&right_items, 0..) |*item, i| item.* = encodeI64NodeValue(host, @intCast(i));
            const left = encodeI64ListNodeValue(host, ops, &left_items);
            const right = encodeI64ListNodeValue(host, ops, &right_items);
            defer decrefNodeValue(left, ops);
            defer decrefNodeValue(right, ops);

            for (0..iterations) |_| {
                const equal = nodeValueEqual(left, right);
                std.mem.doNotOptimizeAway(equal);
                if (equal) checksum += 1;
            }
        },
        .host_value_handles => {
            var store = HandleStore.init(host.gpa.allocator()) catch std.process.exit(1);
            defer store.deinit(host.gpa.allocator());
            var values: [16]i64 = undefined;
            for (&values, 0..) |*item, i| item.* = @intCast(i);
            const left = store.putList(0, &values);
            const right = store.putList(1, &values);
            for (0..iterations) |_| {
                const equal = std.mem.eql(i64, store.list(left), store.list(right));
                std.mem.doNotOptimizeAway(equal);
                if (equal) checksum += 1;
            }
        },
        .generated_boundary_shims => {
            var left: [16]i64 = undefined;
            var right: [16]i64 = undefined;
            for (&left, 0..) |*item, i| item.* = @intCast(i);
            for (&right, 0..) |*item, i| item.* = @intCast(i);
            for (0..iterations) |_| {
                const equal = std.mem.eql(i64, left[0..], right[0..]);
                std.mem.doNotOptimizeAway(equal);
                if (equal) checksum += 1;
            }
        },
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations);
}

fn syntheticNodeValueCallback(host: *HostEnv, input: NodeValue) NodeValue {
    host.bench_counters.callback_transform_count += 1;
    const decoded = decodeI64NodeValue(host, input);
    return encodeI64NodeValue(host, decoded + 1);
}

fn syntheticScalarCallback(value: i64) i64 {
    return value + 1;
}

fn syntheticHandleCallback(store: *HandleStore, handle: u32) u32 {
    const value = store.getScalar(handle);
    return store.putScalar(1, value + 1);
}

fn syntheticGeneratedCallback(host: *HostEnv, input: CounterShim) CounterShim {
    host.bench_counters.callback_transform_count += 1;
    host.bench_counters.boundary_decode_count += 1;
    const result = CounterShim{ .count = input.count + 1 };
    host.bench_counters.boundary_encode_count += 1;
    return result;
}

fn benchMicroCallback(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    _ = ops;
    var checksum: i64 = 0;
    switch (prototype) {
        .baseline_node_value => {
            for (0..iterations) |i| {
                const input = encodeI64NodeValue(host, @intCast(i));
                const output = syntheticNodeValueCallback(host, input);
                std.mem.doNotOptimizeAway(output);
                checksum += decodeI64NodeValue(host, output);
            }
        },
        .scalar_fast_paths => {
            for (0..iterations) |i| {
                const output = syntheticScalarCallback(@intCast(i));
                std.mem.doNotOptimizeAway(output);
                checksum += output;
            }
        },
        .host_value_handles => {
            var store = HandleStore.init(host.gpa.allocator()) catch std.process.exit(1);
            defer store.deinit(host.gpa.allocator());
            for (0..iterations) |i| {
                const input = store.putScalar(0, @intCast(i));
                const output = syntheticHandleCallback(&store, input);
                const output_value = store.getScalar(output);
                std.mem.doNotOptimizeAway(output_value);
                checksum += output_value;
            }
        },
        .generated_boundary_shims => {
            for (0..iterations) |i| {
                const output = syntheticGeneratedCallback(host, .{ .count = @intCast(i) });
                std.mem.doNotOptimizeAway(output);
                checksum += output.count;
            }
        },
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations);
}

fn benchSyntheticDeepMap(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    _ = ops;
    const depth = 32;
    var checksum: i64 = 0;
    switch (prototype) {
        .baseline_node_value => {
            var previous = encodeI64NodeValue(host, -1);
            for (0..iterations) |i| {
                var value = encodeI64NodeValue(host, @intCast(i));
                for (0..depth) |_| {
                    host.bench_counters.evaluated_node_count += 1;
                    value = syntheticNodeValueCallback(host, value);
                    std.mem.doNotOptimizeAway(value);
                }
                host.bench_counters.signal_write_count += 1;
                if (!nodeValueEqual(previous, value)) {
                    host.bench_counters.signal_changed_count += 1;
                    checksum += decodeI64NodeValue(host, value);
                } else {
                    host.bench_counters.signal_unchanged_count += 1;
                }
                previous = value;
            }
        },
        .scalar_fast_paths => {
            var previous: i64 = -1;
            for (0..iterations) |i| {
                var value: i64 = @intCast(i);
                for (0..depth) |_| {
                    host.bench_counters.evaluated_node_count += 1;
                    value += 1;
                    std.mem.doNotOptimizeAway(value);
                }
                host.bench_counters.signal_write_count += 1;
                if (previous != value) {
                    host.bench_counters.signal_changed_count += 1;
                    checksum += value;
                } else {
                    host.bench_counters.signal_unchanged_count += 1;
                }
                previous = value;
            }
        },
        .host_value_handles => {
            var store = HandleStore.init(host.gpa.allocator()) catch std.process.exit(1);
            defer store.deinit(host.gpa.allocator());
            var previous_value: i64 = -1;
            for (0..iterations) |i| {
                var value = store.putScalar(0, @intCast(i));
                for (0..depth) |_| {
                    host.bench_counters.evaluated_node_count += 1;
                    value = syntheticHandleCallback(&store, value);
                    std.mem.doNotOptimizeAway(value);
                }
                host.bench_counters.signal_write_count += 1;
                const next_value = store.getScalar(value);
                if (previous_value != next_value) {
                    host.bench_counters.signal_changed_count += 1;
                    checksum += next_value;
                } else {
                    host.bench_counters.signal_unchanged_count += 1;
                }
                previous_value = next_value;
            }
        },
        .generated_boundary_shims => {
            var previous = CounterShim{ .count = -1 };
            for (0..iterations) |i| {
                var value = CounterShim{ .count = @intCast(i) };
                for (0..depth) |_| {
                    host.bench_counters.evaluated_node_count += 1;
                    value = syntheticGeneratedCallback(host, value);
                    std.mem.doNotOptimizeAway(value);
                }
                host.bench_counters.signal_write_count += 1;
                if (previous.count != value.count) {
                    host.bench_counters.signal_changed_count += 1;
                    checksum += value.count;
                } else {
                    host.bench_counters.signal_unchanged_count += 1;
                }
                previous = value;
            }
        },
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations * depth);
}

fn benchSyntheticWideFanout(host: *HostEnv, ops: *abi.RocOps, prototype: Prototype, iterations: usize) u64 {
    _ = ops;
    const width = 128;
    var checksum: i64 = 0;
    switch (prototype) {
        .baseline_node_value => {
            for (0..iterations) |i| {
                const source = encodeI64NodeValue(host, @intCast(i));
                for (0..width) |label_index| {
                    host.bench_counters.evaluated_node_count += 1;
                    const decoded = decodeI64NodeValue(host, source);
                    const label_value = encodeI64NodeValue(host, decoded + @as(i64, @intCast(label_index)));
                    std.mem.doNotOptimizeAway(label_value);
                    if (nodeValueEqual(label_value, source)) {
                        host.bench_counters.signal_unchanged_count += 1;
                    } else {
                        host.bench_counters.signal_changed_count += 1;
                    }
                    host.bench_counters.signal_write_count += 1;
                    checksum += decodeI64NodeValue(host, label_value);
                }
            }
        },
        .scalar_fast_paths => {
            for (0..iterations) |i| {
                const source: i64 = @intCast(i);
                for (0..width) |label_index| {
                    host.bench_counters.evaluated_node_count += 1;
                    const label_value = source + @as(i64, @intCast(label_index));
                    std.mem.doNotOptimizeAway(label_value);
                    host.bench_counters.signal_write_count += 1;
                    host.bench_counters.signal_changed_count += 1;
                    checksum += label_value;
                }
            }
        },
        .host_value_handles => {
            var store = HandleStore.init(host.gpa.allocator()) catch std.process.exit(1);
            defer store.deinit(host.gpa.allocator());
            for (0..iterations) |i| {
                const source = store.putScalar(0, @intCast(i));
                for (0..width) |label_index| {
                    host.bench_counters.evaluated_node_count += 1;
                    const label_value = store.getScalar(source) + @as(i64, @intCast(label_index));
                    _ = store.putScalar(1, label_value);
                    std.mem.doNotOptimizeAway(label_value);
                    host.bench_counters.signal_write_count += 1;
                    host.bench_counters.signal_changed_count += 1;
                    checksum += label_value;
                }
            }
        },
        .generated_boundary_shims => {
            for (0..iterations) |i| {
                const source = CounterShim{ .count = @intCast(i) };
                for (0..width) |label_index| {
                    host.bench_counters.evaluated_node_count += 1;
                    const label_value = CounterShim{ .count = source.count + @as(i64, @intCast(label_index)) };
                    std.mem.doNotOptimizeAway(label_value);
                    host.bench_counters.signal_write_count += 1;
                    host.bench_counters.signal_changed_count += 1;
                    checksum += label_value.count;
                }
            }
        },
    }
    std.mem.doNotOptimizeAway(checksum);
    return @intCast(iterations * width);
}

fn runSyntheticBenchmarkCase(
    host: *HostEnv,
    ops: *abi.RocOps,
    prototype: Prototype,
    case_name: []const u8,
    sample: usize,
    iterations: usize,
    benchFn: *const fn (*HostEnv, *abi.RocOps, Prototype, usize) u64,
) void {
    host.resetRunMetrics();
    const retained_before = host.roc_allocations.items.len;
    const start_ns = benchmarkNowNs();
    const operations = benchFn(host, ops, prototype, iterations);
    const elapsed = benchmarkNowNs() - start_ns;
    const retained_after = host.roc_allocations.items.len;
    printBenchmarkResult(.{
        .prototype = prototypeName(prototype),
        .case_name = case_name,
        .sample = sample,
        .iterations = iterations,
        .elapsed_ns = elapsed,
        .operations = operations,
        .allocs = host.alloc_count,
        .deallocs = host.dealloc_count,
        .retained_alloc_delta = @as(isize, @intCast(retained_after)) - @as(isize, @intCast(retained_before)),
        .counters = host.bench_counters,
        .text_updates = host.textUpdateCount(),
        .active_graph_nodes = host.activeGraphNodeCount(),
    });
}

fn runSyntheticBenchmarks(host: *HostEnv, ops: *abi.RocOps, iterations: usize, samples: usize, prototype_filter: ?Prototype) void {
    for (0..samples) |sample_index| {
        const sample = sample_index + 1;
        for (prototype_list) |prototype| {
            if (prototype_filter) |filter| {
                if (prototype != filter) continue;
            }
            runSyntheticBenchmarkCase(host, ops, prototype, "micro.scalar_boundary", sample, iterations, benchMicroScalar);
            runSyntheticBenchmarkCase(host, ops, prototype, "micro.list_record_boundary", sample, iterations, benchMicroListRecord);
            if (prototype == .generated_boundary_shims) {
                runSyntheticBenchmarkCase(host, ops, prototype, "micro.generated_app_shim", sample, iterations, benchMicroGeneratedAppShim);
            }
            runSyntheticBenchmarkCase(host, ops, prototype, "micro.equality", sample, iterations, benchMicroEquality);
            runSyntheticBenchmarkCase(host, ops, prototype, "micro.callback_shape", sample, iterations, benchMicroCallback);
            runSyntheticBenchmarkCase(host, ops, prototype, "scenario.deep_map_chain.synthetic", sample, iterations, benchSyntheticDeepMap);
            runSyntheticBenchmarkCase(host, ops, prototype, "scenario.wide_fanout.synthetic", sample, iterations, benchSyntheticWideFanout);
        }
    }
}

const ScenarioClick = struct {
    tag: []const u8,
    index: usize,
};

const Scenario = struct {
    name: []const u8,
    clicks: []const ScenarioClick,
};

const scenario_counter_updates = [_]ScenarioClick{.{ .tag = "button", .index = 1 }};
const scenario_diamond_updates = [_]ScenarioClick{.{ .tag = "button", .index = 5 }};
const scenario_event_merge = [_]ScenarioClick{.{ .tag = "button", .index = 8 }};
const scenario_dynamic_toggle = [_]ScenarioClick{.{ .tag = "button", .index = 9 }};
const scenario_keyed_churn = [_]ScenarioClick{
    .{ .tag = "button", .index = 10 },
    .{ .tag = "button", .index = 11 },
};

const current_app_scenarios = [_]Scenario{
    .{ .name = "scenario.counter_updates.current_app", .clicks = &scenario_counter_updates },
    .{ .name = "scenario.diamond_updates.current_app", .clicks = &scenario_diamond_updates },
    .{ .name = "scenario.event_merge_fanout.current_app", .clicks = &scenario_event_merge },
    .{ .name = "scenario.dynamic_toggle_churn.current_app", .clicks = &scenario_dynamic_toggle },
    .{ .name = "scenario.keyed_reorder_remove.current_app", .clicks = &scenario_keyed_churn },
};

fn signalsHostedFunctions() abi.HostedFunctions {
    return abi.hostedFunctions(.{
        .elem_create_dynamic = @ptrCast(&hostedCreateDynamic),
        .elem_create_each = @ptrCast(&hostedCreateEach),
        .elem_register_lifecycle = &hostedRegisterLifecycle,
        .host_append_child = &hostedAppendChild,
        .host_bind_click = &hostedBindClick,
        .host_bind_signal_update = &hostedBindSignalUpdate,
        .host_bind_text = &hostedBindText,
        .host_create_element = &hostedCreateElement,
        .host_create_event_filter = &hostedCreateEventFilter,
        .host_create_event_map = &hostedCreateEventMap,
        .host_create_event_map_unit_i64_const = &hostedCreateEventMapUnitI64Const,
        .host_create_event_merge = &hostedCreateEventMerge,
        .host_create_event_source = &hostedCreateEventSource,
        .host_create_event_with_latest = &hostedCreateEventWithLatest,
        .host_create_root = &hostedCreateRoot,
        .host_create_signal_const_bool = &hostedCreateSignalConstBool,
        .host_create_signal_const = &hostedCreateSignalConst,
        .host_create_signal_const_i64 = &hostedCreateSignalConstI64,
        .host_create_signal_const_str = &hostedCreateSignalConstStr,
        .host_create_signal_fold_bool_toggle = &hostedCreateSignalFoldBoolToggle,
        .host_create_signal_fold = &hostedCreateSignalFold,
        .host_create_signal_fold_i64 = &hostedCreateSignalFoldI64,
        .host_create_signal_hold = &hostedCreateSignalHold,
        .host_create_signal_map = &hostedCreateSignalMap,
        .host_create_signal_map_i64_i64 = &hostedCreateSignalMapI64I64,
        .host_create_signal_map_i64_str = &hostedCreateSignalMapI64Str,
        .host_create_signal_map2 = &hostedCreateSignalMap2,
        .host_create_signal_map2_i64_i64 = &hostedCreateSignalMap2I64I64,
        .host_create_signal_map2_i64_i64_str = &hostedCreateSignalMap2I64I64Str,
        .host_create_signal_state = &hostedCreateSignalState,
        .host_create_signal_zip_with = &hostedCreateSignalZipWith,
        .host_send_event = &hostedSendEvent,
        .host_set_text = &hostedSetText,
    });
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

fn clickBoundElement(host: *HostEnv, tag: []const u8, index: usize) void {
    if (host.findElementByTagIndex(tag, index)) |elem| {
        if (elem.bound_click_event) |event_id| {
            fireEvent(host, event_id, nodeValueUnit());
            return;
        }
        failHost("benchmark click target has no click binding");
    }
    failHost("benchmark click target not found");
}

fn runCurrentAppScenarioBenchmark(scenario: Scenario, sample: usize, iterations: usize, verbose: bool) !void {
    var host_env = HostEnv.init();

    host_env.test_state.verbose = verbose;
    _ = host_env.createScope(null);
    host_env.current_scope_id = 0;

    var roc_ops = makeSignalsRocOps(&host_env);
    host_env.roc_ops = &roc_ops;
    current_host = &host_env;
    current_roc_ops = &roc_ops;
    defer current_host = null;
    defer current_roc_ops = null;
    defer host_env.deinit();

    abi.roc_main();

    host_env.resetRunMetrics();
    const text_updates_before = host_env.textUpdateCount();
    const retained_before = host_env.roc_allocations.items.len;
    const start_ns = benchmarkNowNs();

    for (0..iterations) |_| {
        for (scenario.clicks) |click| {
            clickBoundElement(&host_env, click.tag, click.index);
        }
    }

    const elapsed = benchmarkNowNs() - start_ns;
    const retained_after = host_env.roc_allocations.items.len;
    printBenchmarkResult(.{
        .prototype = prototypeName(.baseline_node_value),
        .case_name = scenario.name,
        .sample = sample,
        .iterations = iterations,
        .elapsed_ns = elapsed,
        .operations = @intCast(iterations * scenario.clicks.len),
        .allocs = host_env.alloc_count,
        .deallocs = host_env.dealloc_count,
        .retained_alloc_delta = @as(isize, @intCast(retained_after)) - @as(isize, @intCast(retained_before)),
        .counters = host_env.bench_counters,
        .text_updates = host_env.textUpdateCount() - text_updates_before,
        .active_graph_nodes = host_env.activeGraphNodeCount(),
    });
}

fn runCurrentAppScenarioBenchmarks(iterations: usize, samples: usize, verbose: bool) !void {
    for (0..samples) |sample_index| {
        const sample = sample_index + 1;
        for (current_app_scenarios) |scenario| {
            try runCurrentAppScenarioBenchmark(scenario, sample, iterations, verbose);
        }
    }
}

fn runSyntheticCorrectnessChecks(host: *HostEnv, ops: *abi.RocOps) void {
    host.resetRunMetrics();
    const node_id = hostedCreateSignalState(ops, encodeI64NodeValue(host, 7));
    var changed: std.ArrayListUnmanaged(u64) = .empty;
    defer changed.deinit(host.gpa.allocator());
    const changed_same_value = setSignalValue(host, node_id, .{ .node_value = encodeI64NodeValue(host, 7) }, &changed);
    if (changed_same_value) failHost("unchanged-value suppression correctness check failed");
    if (changed.items.len != 0) failHost("unchanged-value suppression recorded a changed signal");
    clearNodeValues(host, node_id);
    host.graph_nodes.items[@intCast(node_id)].active = false;
    host.resetRunMetrics();
}

fn runBoundaryBenchmarks(iterations: usize, samples: usize, prototype_filter: ?Prototype, verbose: bool) !c_int {
    const acceptance = try platform_main("test/signals/test_counter.txt", false);
    if (acceptance != 0) return acceptance;

    var host_env = HostEnv.init();

    host_env.test_state.verbose = verbose;
    _ = host_env.createScope(null);
    host_env.current_scope_id = 0;

    var roc_ops = makeSignalsRocOps(&host_env);
    host_env.roc_ops = &roc_ops;
    current_host = &host_env;
    current_roc_ops = &roc_ops;
    defer current_host = null;
    defer current_roc_ops = null;
    defer host_env.deinit();

    runSyntheticCorrectnessChecks(&host_env, &roc_ops);

    printBenchmarkHeader();
    runSyntheticBenchmarks(&host_env, &roc_ops, iterations, samples, prototype_filter);

    const should_run_current_app = if (prototype_filter) |filter| filter == .baseline_node_value else true;
    if (should_run_current_app) {
        try runCurrentAppScenarioBenchmarks(iterations, samples, verbose);
    }

    return 0;
}

// Main Entry Point

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });

    @export(&hostedCreateDynamic, .{ .name = "roc_host_create_dynamic", .visibility = .hidden });
    @export(&hostedCreateEach, .{ .name = "roc_host_create_each", .visibility = .hidden });
    @export(&hostedRegisterLifecycle, .{ .name = "roc_host_register_lifecycle", .visibility = .hidden });
    @export(&hostedAppendChild, .{ .name = "roc_host_append_child", .visibility = .hidden });
    @export(&hostedBindClick, .{ .name = "roc_host_bind_click", .visibility = .hidden });
    @export(&hostedBindSignalUpdate, .{ .name = "roc_host_bind_signal_update", .visibility = .hidden });
    @export(&hostedBindText, .{ .name = "roc_host_bind_text", .visibility = .hidden });
    @export(&hostCreateElement, .{ .name = "roc_host_create_element", .visibility = .hidden });
    @export(&hostCreateEventFilter, .{ .name = "roc_host_create_event_filter", .visibility = .hidden });
    @export(&hostCreateEventMap, .{ .name = "roc_host_create_event_map", .visibility = .hidden });
    @export(&hostCreateEventMapUnitI64Const, .{ .name = "roc_host_create_event_map_unit_i64_const", .visibility = .hidden });
    @export(&hostedCreateEventMerge, .{ .name = "roc_host_create_event_merge", .visibility = .hidden });
    @export(&hostedCreateEventSource, .{ .name = "roc_host_create_event_source", .visibility = .hidden });
    @export(&hostCreateEventWithLatest, .{ .name = "roc_host_create_event_with_latest", .visibility = .hidden });
    @export(&hostedCreateRoot, .{ .name = "roc_host_create_root", .visibility = .hidden });
    @export(&hostCreateSignalConstBool, .{ .name = "roc_host_create_signal_const_bool", .visibility = .hidden });
    @export(&hostCreateSignalConst, .{ .name = "roc_host_create_signal_const", .visibility = .hidden });
    @export(&hostCreateSignalConstI64, .{ .name = "roc_host_create_signal_const_i64", .visibility = .hidden });
    @export(&hostCreateSignalConstStr, .{ .name = "roc_host_create_signal_const_str", .visibility = .hidden });
    @export(&hostCreateSignalFoldBoolToggle, .{ .name = "roc_host_create_signal_fold_bool_toggle", .visibility = .hidden });
    @export(&hostCreateSignalFold, .{ .name = "roc_host_create_signal_fold", .visibility = .hidden });
    @export(&hostCreateSignalFoldI64, .{ .name = "roc_host_create_signal_fold_i64", .visibility = .hidden });
    @export(&hostCreateSignalHold, .{ .name = "roc_host_create_signal_hold", .visibility = .hidden });
    @export(&hostCreateSignalMap, .{ .name = "roc_host_create_signal_map", .visibility = .hidden });
    @export(&hostCreateSignalMapI64I64, .{ .name = "roc_host_create_signal_map_i64_i64", .visibility = .hidden });
    @export(&hostCreateSignalMapI64Str, .{ .name = "roc_host_create_signal_map_i64_str", .visibility = .hidden });
    @export(&hostCreateSignalMap2, .{ .name = "roc_host_create_signal_map2", .visibility = .hidden });
    @export(&hostCreateSignalMap2I64I64, .{ .name = "roc_host_create_signal_map2_i64_i64", .visibility = .hidden });
    @export(&hostCreateSignalMap2I64I64Str, .{ .name = "roc_host_create_signal_map2_i64_i64_str", .visibility = .hidden });
    @export(&hostCreateSignalState, .{ .name = "roc_host_create_signal_state", .visibility = .hidden });
    @export(&hostCreateSignalZipWith, .{ .name = "roc_host_create_signal_zip_with", .visibility = .hidden });
    @export(&hostedSendEvent, .{ .name = "roc_host_send_event", .visibility = .hidden });
    @export(&hostSetText, .{ .name = "roc_host_set_text", .visibility = .hidden });

    @export(&main, .{ .name = "main" });
    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    var verbose: bool = false;
    var bench: bool = false;
    var bench_iterations: usize = 1000;
    var bench_samples: usize = 1;
    var prototype_filter: ?Prototype = null;
    var spec_file: ?[]const u8 = null;

    // Parse args
    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        } else if (std.mem.eql(u8, arg, "--bench")) {
            bench = true;
        } else if (std.mem.eql(u8, arg, "--bench-iterations")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app --bench [--bench-iterations N] [--bench-samples N] [--bench-prototype A|B|C|D|all]\n");
                return 1;
            }
            bench_iterations = std.fmt.parseInt(usize, std.mem.span(argv[i]), 10) catch {
                writeStderr("Error: Invalid --bench-iterations value\n");
                return 1;
            };
        } else if (std.mem.eql(u8, arg, "--bench-samples")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app --bench [--bench-iterations N] [--bench-samples N] [--bench-prototype A|B|C|D|all]\n");
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
        } else if (std.mem.eql(u8, arg, "--bench-prototype")) {
            i += 1;
            if (i >= arg_count) {
                writeStderr("Usage: ./app --bench [--bench-iterations N] [--bench-samples N] [--bench-prototype A|B|C|D|all]\n");
                return 1;
            }
            prototype_filter = parsePrototypeFilter(std.mem.span(argv[i]));
        } else if (arg.len > 0 and arg[0] != '-') {
            spec_file = arg;
        }
    }

    if (bench) {
        const exit_code = runBoundaryBenchmarks(bench_iterations, bench_samples, prototype_filter, verbose) catch |err| {
            writeStderr("HOST ERROR: ");
            writeStderr(@errorName(err));
            writeStderr("\n");
            return 1;
        };
        return exit_code;
    }

    if (spec_file == null) {
        writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n       ./app --bench [--bench-iterations N] [--bench-samples N] [--bench-prototype A|B|C|D|all]\n");
        return 1;
    }

    const exit_code = platform_main(spec_file.?, verbose) catch |err| {
        writeStderr("HOST ERROR: ");
        writeStderr(@errorName(err));
        writeStderr("\n");
        return 1;
    };
    return exit_code;
}

fn platform_main(spec_file: []const u8, verbose: bool) !c_int {
    // Install signal handlers
    _ = base.signal_handler.installForCurrentThread(.{
        .stack_overflow = handleRocStackOverflow,
        .access_violation = handleRocAccessViolation,
        .arithmetic_error = handleRocArithmeticError,
    });

    var host_env = HostEnv.init();

    const allocator = host_env.gpa.allocator();

    // Parse test spec
    host_env.test_state.commands = parseTestSpecFile(allocator, spec_file) catch |err| {
        switch (err) {
            ParseError.FileNotFound => writeStderr("Error: Test spec file not found\n"),
            ParseError.InvalidFormat => writeStderr("Error: Invalid test spec format\n"),
            else => writeStderr("Error: Failed to parse test spec\n"),
        }
        return 1;
    };
    host_env.test_state.enabled = true;
    host_env.test_state.verbose = verbose;
    _ = host_env.createScope(null);
    host_env.current_scope_id = 0;

    var roc_ops = makeSignalsRocOps(&host_env);
    host_env.roc_ops = &roc_ops;
    current_host = &host_env;
    current_roc_ops = &roc_ops;
    defer current_host = null;
    defer current_roc_ops = null;
    defer host_env.deinit();

    // Call Roc main to build the UI
    traceStderr("[HOST] calling roc_main\n");
    abi.roc_main();
    traceStderr("[HOST] roc_main returned\n");

    if (verbose) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} graph nodes\n", .{ host_env.dom_elements.items.len, host_env.graph_nodes.items.len }) catch "";
        writeStderr(msg);

        // Debug: dump all DOM elements
        for (host_env.dom_elements.items, 0..) |elem, idx| {
            var dbg_buf: [512]u8 = undefined;
            const dbg_msg = std.fmt.bufPrint(&dbg_buf, "[DEBUG] elem[{d}] tag=\"{s}\" text=\"{s}\" parent={?d} children={d} scope={d} active={} updates={d}\n", .{
                idx,
                elem.tag,
                elem.text orelse "(null)",
                elem.parent_id,
                elem.children.items.len,
                elem.scope_id,
                elem.active,
                elem.text_update_count,
            }) catch "";
            writeStderr(dbg_msg);
        }
    }

    // Execute test commands
    for (host_env.test_state.commands) |cmd| {
        switch (cmd.cmd_type) {
            .click => {
                if (verbose) {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "[CMD] click {s}:{d}\n", .{ cmd.tag, cmd.index }) catch "";
                    writeStderr(msg);
                }

                if (host_env.findElementByTagIndex(cmd.tag, cmd.index)) |elem| {
                    if (elem.bound_click_event) |event_id| {
                        fireEvent(&host_env, event_id, nodeValueUnit());
                    } else {
                        var buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} has no click binding\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
                        writeStderr(msg);
                        return 1;
                    }
                } else {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} not found\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_text => {
                const expected = cmd.expected_text orelse "";

                if (host_env.findElementByTagIndex(cmd.tag, cmd.index)) |elem| {
                    const actual = elem.text orelse "";

                    if (std.mem.eql(u8, actual, expected)) {
                        if (verbose) {
                            var buf: [256]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "[OK] expect_text {s}:{d} = \"{s}\"\n", .{ cmd.tag, cmd.index, expected }) catch "";
                            writeStderr(msg);
                        }
                    } else {
                        var buf: [512]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected: {s}:{d} = \"{s}\"\n  Got:      {s}:{d} = \"{s}\"\n", .{ cmd.line_num, cmd.tag, cmd.index, expected, cmd.tag, cmd.index, actual }) catch "TEST FAILED\n";
                        writeStderr(msg);
                        return 1;
                    }
                } else {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} not found\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
                    writeStderr(msg);
                    return 1;
                }
            },

            .expect_updates => {
                const expected = cmd.expected_count orelse 0;

                if (host_env.findElementByTagIndex(cmd.tag, cmd.index)) |elem| {
                    const actual = elem.text_update_count;
                    if (actual == expected) {
                        if (verbose) {
                            var buf: [256]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "[OK] expect_updates {s}:{d} = {d}\n", .{ cmd.tag, cmd.index, expected }) catch "";
                            writeStderr(msg);
                        }
                    } else {
                        var buf: [512]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected updates: {s}:{d} = {d}\n  Got updates:      {s}:{d} = {d}\n", .{ cmd.line_num, cmd.tag, cmd.index, expected, cmd.tag, cmd.index, actual }) catch "TEST FAILED\n";
                        writeStderr(msg);
                        return 1;
                    }
                } else {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} not found\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
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
