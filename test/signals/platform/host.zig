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
const builtins = @import("builtins");
const posix = if (builtin.os.tag != .windows and builtin.os.tag != .wasi) std.posix else undefined;

pub const std_options: std.Options = .{
    .logFn = std.log.defaultLog,
    .log_level = .warn,
};

/// Override the default panic handler to avoid secondary crashes in stack trace generation
pub const panic = std.debug.FullPanic(panicImpl);

fn panicImpl(msg: []const u8, addr: ?usize) noreturn {
    const stderr: std.fs.File = .stderr();
    stderr.writeAll("\n=== PANIC (no stack trace) ===\n") catch {};
    stderr.writeAll(msg) catch {};
    if (addr) |a| {
        var buf: [32]u8 = undefined;
        const hex = std.fmt.bufPrint(&buf, " at address 0x{x}\n", .{a}) catch "";
        stderr.writeAll(hex) catch {};
    } else {
        stderr.writeAll("\n") catch {};
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
        _ = posix.write(posix.STDERR_FILENO, STACK_OVERFLOW_MESSAGE) catch {};
        posix.exit(134);
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
        const addr_str = builtins.handlers.formatHex(fault_addr, &addr_buf);

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
        _ = posix.write(posix.STDERR_FILENO, msg) catch {};

        var addr_buf: [18]u8 = undefined;
        const addr_str = builtins.handlers.formatHex(fault_addr, &addr_buf);
        _ = posix.write(posix.STDERR_FILENO, addr_str) catch {};
        _ = posix.write(posix.STDERR_FILENO, "\n\n") catch {};
        posix.exit(139);
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
        _ = posix.write(posix.STDERR_FILENO, DIVISION_BY_ZERO_MESSAGE) catch {};
        posix.exit(136);
    } else {
        std.process.exit(136);
    }
}

// NodeValue - Universal value type matching Roc's NodeValue

// Tag values must match Roc's alphabetical ordering of NodeValue variants:
// NvBool, NvF64, NvI64, NvList, NvStr, NvUnit
const NodeValueTag = enum(u8) {
    bool_val = 0, // NvBool
    f64_val = 1, // NvF64
    i64_val = 2, // NvI64
    list_val = 3, // NvList
    str_val = 4, // NvStr
    unit_val = 5, // NvUnit
};

const NodeValue = extern struct {
    tag: u8,
    _padding: [7]u8 = undefined,
    payload: extern union {
        i64_val: i64,
        str_val: RocStr,
        bool_val: bool,
        f64_val: f64,
    },

    fn unit() NodeValue {
        return .{
            .tag = @intFromEnum(NodeValueTag.unit_val),
            .payload = .{ .i64_val = 0 },
        };
    }

    fn toI64(self: NodeValue) i64 {
        return self.payload.i64_val;
    }

    fn toStr(self: NodeValue) []const u8 {
        return self.payload.str_val.asSlice();
    }
};

// Simulated DOM

const DomElement = struct {
    id: u64,
    tag: []const u8,
    text: ?[]const u8,
    parent_id: ?u64,
    children: std.ArrayListUnmanaged(u64),
    bound_text_signal: ?u64, // NodeId of signal bound to text
    bound_click_event: ?u64, // NodeId of event bound to click

    fn init(id: u64, tag: []const u8) DomElement {
        return .{
            .id = id,
            .tag = tag,
            .text = null,
            .parent_id = null,
            .children = .{},
            .bound_text_signal = null,
            .bound_click_event = null,
        };
    }

    fn deinit(self: *DomElement, allocator: std.mem.Allocator) void {
        self.children.deinit(allocator);
    }
};

// Reactive Graph

const RocBox = *anyopaque;

const NodeKind = union(enum) {
    event_source,
    event_map: struct { source: u64, transform: RocBox },
    event_filter: struct { source: u64, predicate: RocBox },
    event_merge: struct { left: u64, right: u64 },
    signal_const,
    signal_map: struct { source: u64, transform: RocBox },
    signal_hold: struct { event: u64 },
    signal_fold: struct { event: u64, step: RocBox },
    signal_zip_with: struct { source: u64, event: u64, combine: RocBox },
};

const GraphNode = struct {
    id: u64,
    kind: NodeKind,
    current_value: ?NodeValue,
    dependents: std.ArrayListUnmanaged(u64),

    fn init(id: u64, kind: NodeKind) GraphNode {
        return .{
            .id = id,
            .kind = kind,
            .current_value = null,
            .dependents = .{},
        };
    }

    fn deinit(self: *GraphNode, allocator: std.mem.Allocator) void {
        self.dependents.deinit(allocator);
    }
};

// Test Spec Types

const SpecCommandType = enum {
    click,
    expect_text,
};

const SpecCommand = struct {
    cmd_type: SpecCommandType,
    tag: []const u8,
    index: usize,
    expected_text: ?[]const u8, // For expect_text
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

const ParseError = error{
    InvalidFormat,
    OutOfMemory,
    FileNotFound,
    IoError,
};

/// Parse test spec from file
fn parseTestSpecFile(allocator: std.mem.Allocator, file_path: []const u8) ParseError![]SpecCommand {
    const file = std.fs.cwd().openFile(file_path, .{}) catch return ParseError.FileNotFound;
    defer file.close();

    const content = file.readToEndAlloc(allocator, 1024 * 1024) catch return ParseError.IoError;
    defer allocator.free(content);

    return parseTestSpec(allocator, content);
}

/// Parse test spec content
fn parseTestSpec(allocator: std.mem.Allocator, content: []const u8) ParseError![]SpecCommand {
    var commands = std.ArrayListUnmanaged(SpecCommand){};
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
    gpa: std.heap.GeneralPurposeAllocator(.{ .safety = true }),
    test_state: TestState,
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .{},
    alloc_count: usize = 0,
    dealloc_count: usize = 0,

    // Simulated DOM
    dom_elements: std.ArrayListUnmanaged(DomElement) = .{},
    next_elem_id: u64 = 0,
    tag_counts: std.StringHashMapUnmanaged(usize) = .{}, // Track count per tag for indexing

    // Reactive graph
    graph_nodes: std.ArrayListUnmanaged(GraphNode) = .{},
    next_node_id: u64 = 0,

    // RocOps pointer for closure evaluation
    roc_ops: ?*builtins.host_abi.RocOps = null,

    fn init() HostEnv {
        return HostEnv{
            .gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){},
            .test_state = TestState.init(),
        };
    }

    fn deinit(self: *HostEnv) void {
        const allocator = self.gpa.allocator();

        // Free DOM elements
        for (self.dom_elements.items) |*elem| {
            elem.deinit(allocator);
        }
        self.dom_elements.deinit(allocator);
        self.tag_counts.deinit(allocator);

        // Free graph nodes
        for (self.graph_nodes.items) |*node| {
            node.deinit(allocator);
        }
        self.graph_nodes.deinit(allocator);

        // Free test command strings
        for (self.test_state.commands) |cmd| {
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
        for (self.dom_elements.items) |*elem| {
            if (std.mem.eql(u8, elem.tag, tag)) {
                if (count == index) {
                    return elem;
                }
                count += 1;
            }
        }
        return null;
    }

    fn addDependency(self: *HostEnv, source_id: u64, dependent_id: u64) void {
        const allocator = self.gpa.allocator();
        if (source_id < self.graph_nodes.items.len) {
            self.graph_nodes.items[source_id].dependents.append(allocator, dependent_id) catch {};
        }
    }
};

// Memory Management (same as fx platform)

fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_alloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = @max(roc_alloc.alignment, @alignOf(usize));
    const total_size = roc_alloc.length + size_storage_bytes;

    const base_ptr = allocator.rawAlloc(total_size, align_enum, @returnAddress()) orelse {
        std.process.exit(1);
    };

    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    host.roc_allocations.append(host.gpa.allocator(), .{
        .ptr = base_ptr,
        .total_size = total_size,
        .alignment = align_enum,
    }) catch {};
    host.alloc_count += 1;
}

fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_dealloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = @max(roc_dealloc.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - size_storage_bytes);

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

fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const min_alignment: usize = @max(roc_realloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);
    const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);
    const new_total_size = roc_realloc.new_length + size_storage_bytes;

    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        std.process.exit(1);
    };

    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_slice[0..copy_size]);

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
    }) catch {};

    allocator.rawFree(old_slice, align_enum, @returnAddress());

    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    roc_realloc.answer = @ptrFromInt(@intFromPtr(new_ptr) + size_storage_bytes);
}

fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    const stderr: std.fs.File = .stderr();
    stderr.writeAll("\n\x1b[31mRoc crashed:\x1b[0m ") catch {};
    stderr.writeAll(message) catch {};
    stderr.writeAll("\n") catch {};
    std.process.exit(1);
}

// Roc Entrypoints

extern fn roc__main(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;
extern fn roc__call_transform(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: *anyopaque) callconv(.c) void;
extern fn roc__call_step(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: *anyopaque) callconv(.c) void;

const RocStr = builtins.str.RocStr;

// Hosted Effects

/// Host.append_child! (alphabetically first)
fn hostedAppendChild(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ret_ptr;
    const Args = extern struct { parent_id: u64, child_id: u64 };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    {
        const stderr_dbg: std.fs.File = .stderr();
        var buf: [128]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[HOST] append_child! parent={d} child={d}\n", .{ args.parent_id, args.child_id }) catch "[HOST] append_child!\n";
        stderr_dbg.writeAll(msg) catch {};
    }

    if (args.parent_id < host.dom_elements.items.len and args.child_id < host.dom_elements.items.len) {
        host.dom_elements.items[args.parent_id].children.append(host.gpa.allocator(), args.child_id) catch {};
        host.dom_elements.items[args.child_id].parent_id = args.parent_id;
    }
}

/// Host.bind_click!
fn hostedBindClick(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ret_ptr;
    const Args = extern struct { elem_id: u64, event_node_id: u64 };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    if (args.elem_id < host.dom_elements.items.len) {
        host.dom_elements.items[args.elem_id].bound_click_event = args.event_node_id;
    }
}

/// Host.bind_text!
fn hostedBindText(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ret_ptr;
    const Args = extern struct { elem_id: u64, signal_node_id: u64 };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    if (args.elem_id < host.dom_elements.items.len) {
        host.dom_elements.items[args.elem_id].bound_text_signal = args.signal_node_id;
        // Update text from signal's current value
        updateElementText(host, args.elem_id);
    }
}

/// Host.create_element!
fn hostedCreateElement(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { tag: RocStr };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const tag_slice = args.tag.asSlice();
    {
        const stderr_dbg: std.fs.File = .stderr();
        stderr_dbg.writeAll("[HOST] create_element! tag=\"") catch {};
        stderr_dbg.writeAll(tag_slice) catch {};
        stderr_dbg.writeAll("\"\n") catch {};
    }
    const allocator = host.gpa.allocator();

    // Duplicate tag string for storage
    const tag_copy = allocator.dupe(u8, tag_slice) catch {
        result.* = 0;
        return;
    };

    const elem_id = host.next_elem_id;
    host.next_elem_id += 1;

    const elem = DomElement.init(elem_id, tag_copy);
    host.dom_elements.append(allocator, elem) catch {
        allocator.free(tag_copy);
        result.* = 0;
        return;
    };

    result.* = elem_id;
}

/// Host.create_event_filter!
fn hostedCreateEventFilter(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { source_id: u64, predicate: RocBox };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, .{ .event_filter = .{
        .source = args.source_id,
        .predicate = args.predicate,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.source_id, node_id);
    result.* = node_id;
}

/// Host.create_event_map!
fn hostedCreateEventMap(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { source_id: u64, transform: RocBox };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init( node_id, .{ .event_map = .{
        .source = args.source_id,
        .transform = args.transform,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.source_id, node_id);
    result.* = node_id;
}

/// Host.create_event_merge!
fn hostedCreateEventMerge(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { left_id: u64, right_id: u64 };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init( node_id, .{ .event_merge = .{
        .left = args.left_id,
        .right = args.right_id,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.left_id, node_id);
    host.addDependency(args.right_id, node_id);
    result.* = node_id;
}

/// Host.create_event_source!
fn hostedCreateEventSource(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = args_ptr;
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init( node_id, .event_source);
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    result.* = node_id;
}

/// Host.create_root!
fn hostedCreateRoot(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = args_ptr;
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));
    const stderr_dbg: std.fs.File = .stderr();
    stderr_dbg.writeAll("[HOST] create_root!\n") catch {};

    const elem_id = host.next_elem_id;
    host.next_elem_id += 1;

    const allocator = host.gpa.allocator();
    const root_tag = allocator.dupe(u8, "root") catch {
        result.* = 0;
        return;
    };

    const elem = DomElement.init(elem_id, root_tag);
    host.dom_elements.append(allocator, elem) catch {
        allocator.free(root_tag);
        result.* = 0;
        return;
    };

    result.* = elem_id;
}

/// Host.create_signal_const!
fn hostedCreateSignalConst(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { value: NodeValue };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init( node_id, .signal_const);
    node.current_value = args.value;
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    result.* = node_id;
}

/// Host.create_signal_fold!
fn hostedCreateSignalFold(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { initial: NodeValue, event_id: u64, step: RocBox };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init( node_id, .{ .signal_fold = .{
        .event = args.event_id,
        .step = args.step,
    } });
    node.current_value = args.initial;
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.event_id, node_id);
    result.* = node_id;
}

/// Host.create_signal_hold!
fn hostedCreateSignalHold(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { initial: NodeValue, event_id: u64 };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init( node_id, .{ .signal_hold = .{
        .event = args.event_id,
    } });
    node.current_value = args.initial;
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.event_id, node_id);
    result.* = node_id;
}

/// Host.create_signal_map!
fn hostedCreateSignalMap(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { source_id: u64, transform: RocBox };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init( node_id, .{ .signal_map = .{
        .source = args.source_id,
        .transform = args.transform,
    } });

    // Compute initial value from source
    if (args.source_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[args.source_id].current_value) |source_val| {
            const result_val = callRocTransform(host, args.transform, source_val);
            node.current_value = result_val;
        }
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.source_id, node_id);
    result.* = node_id;
}

/// Host.create_signal_zip_with!
fn hostedCreateSignalZipWith(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    const Args = extern struct { source_id: u64, event_id: u64, combine: RocBox };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *u64 = @ptrCast(@alignCast(ret_ptr));

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init( node_id, .{ .signal_zip_with = .{
        .source = args.source_id,
        .event = args.event_id,
        .combine = args.combine,
    } });

    // Use source signal's initial value
    if (args.source_id < host.graph_nodes.items.len) {
        node.current_value = host.graph_nodes.items[args.source_id].current_value;
    }

    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        result.* = 0;
        return;
    };

    host.addDependency(args.source_id, node_id);
    host.addDependency(args.event_id, node_id);
    result.* = node_id;
}

/// Host.set_text!
fn hostedSetText(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ret_ptr;
    const Args = extern struct { elem_id: u64, text: RocStr };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    {
        const stderr_dbg: std.fs.File = .stderr();
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[HOST] set_text! elem={d} text=\"{s}\"\n", .{ args.elem_id, args.text.asSlice() }) catch "[HOST] set_text!\n";
        stderr_dbg.writeAll(msg) catch {};
    }

    if (args.elem_id < host.dom_elements.items.len) {
        const allocator = host.gpa.allocator();
        const text_slice = args.text.asSlice();

        // Free old text if any
        if (host.dom_elements.items[args.elem_id].text) |old_text| {
            allocator.free(old_text);
        }

        // Duplicate new text
        const text_copy = allocator.dupe(u8, text_slice) catch return;
        host.dom_elements.items[args.elem_id].text = text_copy;
    }
}

// Array of hosted function pointers (MUST be alphabetically sorted by fully-qualified name)
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{
    hostedAppendChild, // Host.append_child!
    hostedBindClick, // Host.bind_click!
    hostedBindText, // Host.bind_text!
    hostedCreateElement, // Host.create_element!
    hostedCreateEventFilter, // Host.create_event_filter!
    hostedCreateEventMap, // Host.create_event_map!
    hostedCreateEventMerge, // Host.create_event_merge!
    hostedCreateEventSource, // Host.create_event_source!
    hostedCreateRoot, // Host.create_root!
    hostedCreateSignalConst, // Host.create_signal_const!
    hostedCreateSignalFold, // Host.create_signal_fold!
    hostedCreateSignalHold, // Host.create_signal_hold!
    hostedCreateSignalMap, // Host.create_signal_map!
    hostedCreateSignalZipWith, // Host.create_signal_zip_with!
    hostedSetText, // Host.set_text!
};

// Reactive Graph Propagation

fn callRocTransform(host: *HostEnv, transform: RocBox, input: NodeValue) NodeValue {
    if (host.roc_ops) |ops| {
        const Args = extern struct { boxed_fn: RocBox, input: NodeValue };
        var args = Args{ .boxed_fn = transform, .input = input };
        var result: NodeValue = undefined;
        roc__call_transform(ops, @ptrCast(&result), @ptrCast(&args));
        return result;
    }
    return NodeValue.unit();
}

fn callRocStep(host: *HostEnv, step: RocBox, acc: NodeValue, event_val: NodeValue) NodeValue {
    if (host.roc_ops) |ops| {
        const Args = extern struct { boxed_fn: RocBox, acc: NodeValue, event: NodeValue };
        var args = Args{ .boxed_fn = step, .acc = acc, .event = event_val };
        var result: NodeValue = undefined;
        roc__call_step(ops, @ptrCast(&result), @ptrCast(&args));
        return result;
    }
    return acc;
}

fn fireEvent(host: *HostEnv, node_id: u64, value: NodeValue) void {
    if (node_id >= host.graph_nodes.items.len) return;

    const allocator = host.gpa.allocator();

    // Clear all event node values to prevent stale values from previous firings
    // being picked up by event_merge nodes
    for (host.graph_nodes.items) |*n| {
        switch (n.kind) {
            .event_source, .event_map, .event_filter, .event_merge => {
                n.current_value = null;
            },
            else => {},
        }
    }

    // Set the event source value
    host.graph_nodes.items[node_id].current_value = value;

    // Collect nodes to evaluate in dependency order
    var to_evaluate = std.ArrayListUnmanaged(u64){};
    defer to_evaluate.deinit(allocator);

    collectDependents(host, node_id, &to_evaluate, allocator);

    // Evaluate each dependent node
    for (to_evaluate.items) |dep_id| {
        evaluateNode(host, dep_id);
    }

    // Update all DOM text bindings
    updateAllTextBindings(host);
}

fn collectDependents(host: *HostEnv, node_id: u64, out: *std.ArrayListUnmanaged(u64), allocator: std.mem.Allocator) void {
    if (node_id >= host.graph_nodes.items.len) return;

    const node = &host.graph_nodes.items[node_id];
    for (node.dependents.items) |dep_id| {
        // Avoid duplicates (simple check)
        var found = false;
        for (out.items) |existing| {
            if (existing == dep_id) {
                found = true;
                break;
            }
        }
        if (!found) {
            out.append(allocator, dep_id) catch {};
            collectDependents(host, dep_id, out, allocator);
        }
    }
}

fn evaluateNode(host: *HostEnv, node_id: u64) void {
    if (node_id >= host.graph_nodes.items.len) return;

    const node = &host.graph_nodes.items[node_id];
    switch (node.kind) {
        .event_source => {}, // Value already set by fireEvent
        .signal_const => {}, // Constant, never changes

        .event_map => |data| {
            if (data.source < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    node.current_value = callRocTransform(host, data.transform, input);
                }
            }
        },

        .event_merge => |data| {
            // Pass through whichever input fired
            // For simplicity, check left first
            if (data.left < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.left].current_value) |val| {
                    node.current_value = val;
                }
            }
            if (data.right < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.right].current_value) |val| {
                    node.current_value = val;
                }
            }
        },

        .event_filter => |data| {
            // TODO: implement predicate call
            _ = data;
        },

        .signal_map => |data| {
            if (data.source < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    node.current_value = callRocTransform(host, data.transform, input);
                }
            }
        },

        .signal_hold => |data| {
            if (data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.event].current_value) |val| {
                    node.current_value = val;
                }
            }
        },

        .signal_fold => |data| {
            if (data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.event].current_value) |event_val| {
                    if (node.current_value) |acc| {
                        node.current_value = callRocStep(host, data.step, acc, event_val);
                    }
                }
            }
        },

        .signal_zip_with => |data| {
            if (data.source < host.graph_nodes.items.len and data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |signal_val| {
                    if (host.graph_nodes.items[data.event].current_value) |event_val| {
                        node.current_value = callRocStep(host, data.combine, signal_val, event_val);
                    }
                }
            }
        },
    }
}

fn updateElementText(host: *HostEnv, elem_id: u64) void {
    if (elem_id >= host.dom_elements.items.len) return;

    const elem = &host.dom_elements.items[elem_id];
    if (elem.bound_text_signal) |signal_id| {
        if (signal_id < host.graph_nodes.items.len) {
            if (host.graph_nodes.items[signal_id].current_value) |val| {
                const allocator = host.gpa.allocator();

                // Free old text
                if (elem.text) |old_text| {
                    allocator.free(old_text);
                }

                // Get string representation
                if (val.tag == @intFromEnum(NodeValueTag.str_val)) {
                    const text = val.toStr();
                    elem.text = allocator.dupe(u8, text) catch null;
                } else if (val.tag == @intFromEnum(NodeValueTag.i64_val)) {
                    var buf: [32]u8 = undefined;
                    const text = std.fmt.bufPrint(&buf, "{d}", .{val.toI64()}) catch "?";
                    elem.text = allocator.dupe(u8, text) catch null;
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

// Main Entry Point

comptime {
    @export(&main, .{ .name = "main" });
    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

fn __main() callconv(.c) void {}

fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    var verbose: bool = false;
    var spec_file: ?[]const u8 = null;

    // Parse args
    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--verbose") or std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        } else if (arg.len > 0 and arg[0] != '-') {
            spec_file = arg;
        }
    }

    const stderr_file: std.fs.File = .stderr();

    if (spec_file == null) {
        stderr_file.writeAll("Usage: ./app [--verbose] <test_spec.txt>\n") catch {};
        return 1;
    }

    const exit_code = platform_main(spec_file.?, verbose) catch |err| {
        stderr_file.writeAll("HOST ERROR: ") catch {};
        stderr_file.writeAll(@errorName(err)) catch {};
        stderr_file.writeAll("\n") catch {};
        return 1;
    };
    return exit_code;
}

fn platform_main(spec_file: []const u8, verbose: bool) !c_int {
    // Install signal handlers
    _ = builtins.handlers.install(handleRocStackOverflow, handleRocAccessViolation, handleRocArithmeticError);

    var host_env = HostEnv.init();
    defer host_env.deinit();

    const allocator = host_env.gpa.allocator();

    // Parse test spec
    host_env.test_state.commands = parseTestSpecFile(allocator, spec_file) catch |err| {
        const stderr_file: std.fs.File = .stderr();
        switch (err) {
            ParseError.FileNotFound => stderr_file.writeAll("Error: Test spec file not found\n") catch {},
            ParseError.InvalidFormat => stderr_file.writeAll("Error: Invalid test spec format\n") catch {},
            else => stderr_file.writeAll("Error: Failed to parse test spec\n") catch {},
        }
        return 1;
    };
    host_env.test_state.enabled = true;
    host_env.test_state.verbose = verbose;

    // Create RocOps
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = rocAllocFn,
        .roc_dealloc = rocDeallocFn,
        .roc_realloc = rocReallocFn,
        .roc_dbg = rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{
            .count = hosted_function_ptrs.len,
            .fns = @constCast(&hosted_function_ptrs),
        },
    };
    host_env.roc_ops = &roc_ops;

    // Call Roc main to build the UI
    const stderr_dbg: std.fs.File = .stderr();
    stderr_dbg.writeAll("[HOST] calling roc__main\n") catch {};
    var ret: [0]u8 = undefined;
    var args: [0]u8 = undefined;
    roc__main(&roc_ops, @as(*anyopaque, @ptrCast(&ret)), @as(*anyopaque, @ptrCast(&args)));
    stderr_dbg.writeAll("[HOST] roc__main returned\n") catch {};

    if (verbose) {
        const stderr_file: std.fs.File = .stderr();
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} graph nodes\n", .{ host_env.dom_elements.items.len, host_env.graph_nodes.items.len }) catch "";
        stderr_file.writeAll(msg) catch {};

        // Debug: dump all DOM elements
        for (host_env.dom_elements.items, 0..) |elem, idx| {
            var dbg_buf: [512]u8 = undefined;
            const dbg_msg = std.fmt.bufPrint(&dbg_buf, "[DEBUG] elem[{d}] tag=\"{s}\" text=\"{s}\" parent={?d} children={d}\n", .{
                idx,
                elem.tag,
                elem.text orelse "(null)",
                elem.parent_id,
                elem.children.items.len,
            }) catch "";
            stderr_file.writeAll(dbg_msg) catch {};
        }
    }

    // Execute test commands
    const stderr_file: std.fs.File = .stderr();
    for (host_env.test_state.commands) |cmd| {
        switch (cmd.cmd_type) {
            .click => {
                if (verbose) {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "[CMD] click {s}:{d}\n", .{ cmd.tag, cmd.index }) catch "";
                    stderr_file.writeAll(msg) catch {};
                }

                if (host_env.findElementByTagIndex(cmd.tag, cmd.index)) |elem| {
                    if (elem.bound_click_event) |event_id| {
                        fireEvent(&host_env, event_id, NodeValue.unit());
                    } else {
                        var buf: [256]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} has no click binding\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
                        stderr_file.writeAll(msg) catch {};
                        return 1;
                    }
                } else {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} not found\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
                    stderr_file.writeAll(msg) catch {};
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
                            stderr_file.writeAll(msg) catch {};
                        }
                    } else {
                        var buf: [512]u8 = undefined;
                        const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected: {s}:{d} = \"{s}\"\n  Got:      {s}:{d} = \"{s}\"\n", .{ cmd.line_num, cmd.tag, cmd.index, expected, cmd.tag, cmd.index, actual }) catch "TEST FAILED\n";
                        stderr_file.writeAll(msg) catch {};
                        return 1;
                    }
                } else {
                    var buf: [256]u8 = undefined;
                    const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: Element {s}:{d} not found\n", .{ cmd.line_num, cmd.tag, cmd.index }) catch "TEST FAILED\n";
                    stderr_file.writeAll(msg) catch {};
                    return 1;
                }
            },
        }
    }

    if (verbose) {
        stderr_file.writeAll("[PASS] All tests passed\n") catch {};
    }

    return 0;
}
