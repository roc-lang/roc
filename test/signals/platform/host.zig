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

    fn init(id: u64, tag: []const u8) DomElement {
        return .{
            .id = id,
            .tag = tag,
            .text = null,
            .parent_id = null,
            .children = .empty,
            .bound_text_signal = null,
            .bound_click_event = null,
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

const NodeKind = union(enum) {
    event_source,
    event_map: struct { source: u64, transform: RocBox },
    event_filter: struct { source: u64, predicate: RocBox },
    event_merge: struct { left: u64, right: u64 },
    event_with_latest: struct { event: u64, signal: u64, combine: RocBox },
    signal_const,
    signal_state: struct { update_event: ?u64 },
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
            .dependents = .empty,
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
    gpa: std.heap.DebugAllocator(.{ .safety = true }),
    test_state: TestState,
    roc_allocations: std.ArrayListUnmanaged(RocAllocation) = .empty,
    alloc_count: usize = 0,
    dealloc_count: usize = 0,

    // Simulated DOM
    dom_elements: std.ArrayListUnmanaged(DomElement) = .empty,
    next_elem_id: u64 = 0,
    tag_counts: std.StringHashMapUnmanaged(usize) = .empty, // Track count per tag for indexing

    // Reactive graph
    graph_nodes: std.ArrayListUnmanaged(GraphNode) = .empty,
    next_node_id: u64 = 0,

    // RocOps pointer for closure evaluation
    roc_ops: ?*abi.RocOps = null,

    fn init() HostEnv {
        return HostEnv{
            .gpa = std.heap.DebugAllocator(.{ .safety = true }){},
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
        if (self.roc_ops) |ops| {
            for (self.graph_nodes.items) |node| {
                if (node.current_value) |value| {
                    decrefNodeValue(value, ops);
                }
            }
        }
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
            self.graph_nodes.items[source_id].dependents.append(allocator, dependent_id) catch std.process.exit(1);
        }
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

    if (parent_id < host.dom_elements.items.len and child_id < host.dom_elements.items.len) {
        host.dom_elements.items[parent_id].children.append(host.gpa.allocator(), child_id) catch std.process.exit(1);
        host.dom_elements.items[child_id].parent_id = parent_id;
    }
}

/// Host.bind_click!
fn hostedBindClick(elem_id: u64, event_node_id: u64) callconv(.c) void {
    const host = currentHost();

    if (elem_id < host.dom_elements.items.len) {
        host.dom_elements.items[elem_id].bound_click_event = event_node_id;
    }
}

/// Host.bind_text!
fn hostedBindText(elem_id: u64, signal_node_id: u64) callconv(.c) void {
    const host = currentHost();

    if (elem_id < host.dom_elements.items.len) {
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

    const elem = DomElement.init(elem_id, tag_copy);
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

    const node = GraphNode.init(node_id, .{ .event_filter = .{
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

    const node = GraphNode.init(node_id, .{ .event_map = .{
        .source = source_id,
        .transform = transform,
    } });
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(source_id, node_id);
    return node_id;
}

/// Host.create_event_merge!
fn hostedCreateEventMerge(left_id: u64, right_id: u64) callconv(.c) u64 {
    const host = currentHost();
    traceStderr("[HOST] create_event_merge!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    const node = GraphNode.init(node_id, .{ .event_merge = .{
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

    const node = GraphNode.init(node_id, .{ .event_with_latest = .{
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

    const node = GraphNode.init(node_id, .event_source);
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

    const elem = DomElement.init(elem_id, root_tag);
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

    var node = GraphNode.init(node_id, .signal_const);
    node.current_value = value;
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    return node_id;
}

/// Host.create_signal_state!
fn hostedCreateSignalState(ops: *abi.RocOps, initial: NodeValue) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_state!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, .{ .signal_state = .{
        .update_event = null,
    } });
    node.current_value = initial;
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

    var node = GraphNode.init(node_id, .{ .signal_fold = .{
        .event = event_id,
        .step = step,
    } });
    node.current_value = initial;
    host.graph_nodes.append(host.gpa.allocator(), node) catch {
        std.process.exit(1);
    };

    host.addDependency(event_id, node_id);
    return node_id;
}

/// Host.create_signal_hold!
fn hostedCreateSignalHold(ops: *abi.RocOps, initial: NodeValue, event_id: u64) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_hold!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, .{ .signal_hold = .{
        .event = event_id,
    } });
    node.current_value = initial;
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

    var node = GraphNode.init(node_id, .{ .signal_map = .{
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

/// Host.create_signal_zip_with!
fn hostedCreateSignalZipWith(ops: *abi.RocOps, source_id: u64, event_id: u64, combine: RocBox) callconv(.c) u64 {
    const host = hostFromOps(ops);
    traceStderr("[HOST] create_signal_zip_with!\n");

    const node_id = host.next_node_id;
    host.next_node_id += 1;

    var node = GraphNode.init(node_id, .{ .signal_zip_with = .{
        .source = source_id,
        .event = event_id,
        .combine = combine,
    } });

    // Use source signal's initial value
    if (source_id < host.graph_nodes.items.len) {
        if (host.graph_nodes.items[source_id].current_value) |value| {
            increfNodeValue(value);
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

    if (elem_id < host.dom_elements.items.len) {
        const allocator = host.gpa.allocator();
        const text_slice = text.asSlice();
        const text_copy = allocator.dupe(u8, text_slice) catch {
            text.decref(ops);
            std.process.exit(1);
        };
        text.decref(ops);

        // Free old text if any
        if (host.dom_elements.items[elem_id].text) |old_text| {
            allocator.free(old_text);
        }

        host.dom_elements.items[elem_id].text = text_copy;
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

fn hostCreateEventWithLatest(event_id: u64, signal_id: u64, combine: RocBox) callconv(.c) u64 {
    return hostedCreateEventWithLatest(currentRocOps(), event_id, signal_id, combine);
}

fn hostCreateSignalConst(value: NodeValue) callconv(.c) u64 {
    return hostedCreateSignalConst(currentRocOps(), value);
}

fn hostCreateSignalState(initial: NodeValue) callconv(.c) u64 {
    return hostedCreateSignalState(currentRocOps(), initial);
}

fn hostCreateSignalFold(initial: NodeValue, event_id: u64, step: RocBox) callconv(.c) u64 {
    return hostedCreateSignalFold(currentRocOps(), initial, event_id, step);
}

fn hostCreateSignalHold(initial: NodeValue, event_id: u64) callconv(.c) u64 {
    return hostedCreateSignalHold(currentRocOps(), initial, event_id);
}

fn hostCreateSignalMap(source_id: u64, transform: RocBox) callconv(.c) u64 {
    return hostedCreateSignalMap(currentRocOps(), source_id, transform);
}

fn hostCreateSignalZipWith(source_id: u64, event_id: u64, combine: RocBox) callconv(.c) u64 {
    return hostedCreateSignalZipWith(currentRocOps(), source_id, event_id, combine);
}

fn hostSetText(elem_id: u64, text: RocStr) callconv(.c) void {
    hostedSetText(currentRocOps(), elem_id, text);
}

// Reactive Graph Propagation

fn increfNodeValue(value: NodeValue) void {
    switch (value.tag) {
        .NvStr => value.payload.nv_str.incref(1),
        .NvList => value.payload.nv_list.incref(1),
        else => {},
    }
}

fn decrefNodeValue(value: NodeValue, ops: *abi.RocOps) void {
    switch (value.tag) {
        .NvStr => value.payload.nv_str.decref(ops),
        .NvList => value.payload.nv_list.decref(ops),
        else => {},
    }
}

fn setCurrentValue(host: *HostEnv, node_id: u64, value: ?NodeValue) void {
    if (node_id >= host.graph_nodes.items.len) return;
    const ops = host.roc_ops orelse @panic("RocOps unavailable while updating signals");
    if (host.graph_nodes.items[node_id].current_value) |old_value| {
        decrefNodeValue(old_value, ops);
    }
    host.graph_nodes.items[node_id].current_value = value;
}

fn callRocTransform(host: *HostEnv, transform: RocBox, input: NodeValue) NodeValue {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating transform");
    var result: NodeValue = undefined;
    increfNodeValue(input);
    var input_arg = input;
    const payload = abi.rocErasedCallablePayloadPtr(transform);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(transform));
    return result;
}

fn callRocStep(host: *HostEnv, step: RocBox, acc: NodeValue, event_val: NodeValue) NodeValue {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating step");
    increfNodeValue(acc);
    increfNodeValue(event_val);
    var args = extern struct {
        _0: NodeValue,
        _1: NodeValue,
    }{ ._0 = acc, ._1 = event_val };
    var result: NodeValue = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(step);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&args), abi.rocErasedCallableCapturePtr(step));
    return result;
}

fn callRocPredicate(host: *HostEnv, predicate: RocBox, input: NodeValue) bool {
    const ops = host.roc_ops orelse @panic("RocOps unavailable while evaluating predicate");
    increfNodeValue(input);
    var input_arg = input;
    var result: bool = undefined;
    const payload = abi.rocErasedCallablePayloadPtr(predicate);
    payload.callable_fn_ptr(ops, @ptrCast(&result), @ptrCast(&input_arg), abi.rocErasedCallableCapturePtr(predicate));
    return result;
}

fn fireEvent(host: *HostEnv, node_id: u64, value: NodeValue) void {
    if (node_id >= host.graph_nodes.items.len) return;

    const allocator = host.gpa.allocator();

    // Clear all event node values to prevent stale values from previous firings
    // being picked up by event_merge nodes
    for (host.graph_nodes.items, 0..) |*n, i| {
        switch (n.kind) {
            .event_source, .event_map, .event_filter, .event_merge, .event_with_latest => {
                setCurrentValue(host, @intCast(i), null);
            },
            else => {},
        }
    }

    // Set the event source value
    setCurrentValue(host, node_id, value);

    // Collect nodes to evaluate in dependency order
    var to_evaluate: std.ArrayListUnmanaged(u64) = .empty;
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
            out.append(allocator, dep_id) catch std.process.exit(1);
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
        .signal_state => |data| {
            if (data.update_event) |event_id| {
                if (event_id < host.graph_nodes.items.len) {
                    if (host.graph_nodes.items[event_id].current_value) |val| {
                        increfNodeValue(val);
                        setCurrentValue(host, node_id, val);
                    }
                }
            }
        },

        .event_map => |data| {
            if (data.source < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    setCurrentValue(host, node_id, callRocTransform(host, data.transform, input));
                }
            }
        },

        .event_merge => |data| {
            if (data.left < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.left].current_value) |val| {
                    increfNodeValue(val);
                    setCurrentValue(host, node_id, val);
                }
            }
            if (data.right < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.right].current_value) |val| {
                    increfNodeValue(val);
                    setCurrentValue(host, node_id, val);
                }
            }
        },

        .event_filter => |data| {
            if (data.source < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    if (callRocPredicate(host, data.predicate, input)) {
                        increfNodeValue(input);
                        setCurrentValue(host, node_id, input);
                    } else {
                        setCurrentValue(host, node_id, null);
                    }
                }
            }
        },

        .event_with_latest => |data| {
            if (data.event < host.graph_nodes.items.len and data.signal < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.event].current_value) |event_val| {
                    if (host.graph_nodes.items[data.signal].current_value) |signal_val| {
                        setCurrentValue(host, node_id, callRocStep(host, data.combine, event_val, signal_val));
                    }
                }
            }
        },

        .signal_map => |data| {
            if (data.source < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |input| {
                    setCurrentValue(host, node_id, callRocTransform(host, data.transform, input));
                }
            }
        },

        .signal_hold => |data| {
            if (data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.event].current_value) |val| {
                    increfNodeValue(val);
                    setCurrentValue(host, node_id, val);
                }
            }
        },

        .signal_fold => |data| {
            if (data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.event].current_value) |event_val| {
                    if (node.current_value) |acc| {
                        setCurrentValue(host, node_id, callRocStep(host, data.step, acc, event_val));
                    }
                }
            }
        },

        .signal_zip_with => |data| {
            if (data.source < host.graph_nodes.items.len and data.event < host.graph_nodes.items.len) {
                if (host.graph_nodes.items[data.source].current_value) |signal_val| {
                    if (host.graph_nodes.items[data.event].current_value) |event_val| {
                        setCurrentValue(host, node_id, callRocStep(host, data.combine, signal_val, event_val));
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

                switch (val.tag) {
                    .NvStr => {
                        const text = val.payload.nv_str.asSlice();
                        const text_copy = allocator.dupe(u8, text) catch std.process.exit(1);
                        if (elem.text) |old_text| {
                            allocator.free(old_text);
                        }
                        elem.text = text_copy;
                    },
                    .NvI64 => {
                        var buf: [32]u8 = undefined;
                        const text = std.fmt.bufPrint(&buf, "{d}", .{val.payload.nv_i64}) catch std.process.exit(1);
                        const text_copy = allocator.dupe(u8, text) catch std.process.exit(1);
                        if (elem.text) |old_text| {
                            allocator.free(old_text);
                        }
                        elem.text = text_copy;
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

// Main Entry Point

comptime {
    @export(&hostAlloc, .{ .name = "roc_alloc", .visibility = .hidden });
    @export(&hostDealloc, .{ .name = "roc_dealloc", .visibility = .hidden });
    @export(&hostRealloc, .{ .name = "roc_realloc", .visibility = .hidden });
    @export(&hostDbg, .{ .name = "roc_dbg", .visibility = .hidden });
    @export(&hostExpectFailed, .{ .name = "roc_expect_failed", .visibility = .hidden });
    @export(&hostCrashed, .{ .name = "roc_crashed", .visibility = .hidden });

    @export(&hostedAppendChild, .{ .name = "roc_host_append_child", .visibility = .hidden });
    @export(&hostedBindClick, .{ .name = "roc_host_bind_click", .visibility = .hidden });
    @export(&hostedBindSignalUpdate, .{ .name = "roc_host_bind_signal_update", .visibility = .hidden });
    @export(&hostedBindText, .{ .name = "roc_host_bind_text", .visibility = .hidden });
    @export(&hostCreateElement, .{ .name = "roc_host_create_element", .visibility = .hidden });
    @export(&hostCreateEventFilter, .{ .name = "roc_host_create_event_filter", .visibility = .hidden });
    @export(&hostCreateEventMap, .{ .name = "roc_host_create_event_map", .visibility = .hidden });
    @export(&hostedCreateEventMerge, .{ .name = "roc_host_create_event_merge", .visibility = .hidden });
    @export(&hostedCreateEventSource, .{ .name = "roc_host_create_event_source", .visibility = .hidden });
    @export(&hostCreateEventWithLatest, .{ .name = "roc_host_create_event_with_latest", .visibility = .hidden });
    @export(&hostedCreateRoot, .{ .name = "roc_host_create_root", .visibility = .hidden });
    @export(&hostCreateSignalConst, .{ .name = "roc_host_create_signal_const", .visibility = .hidden });
    @export(&hostCreateSignalFold, .{ .name = "roc_host_create_signal_fold", .visibility = .hidden });
    @export(&hostCreateSignalHold, .{ .name = "roc_host_create_signal_hold", .visibility = .hidden });
    @export(&hostCreateSignalMap, .{ .name = "roc_host_create_signal_map", .visibility = .hidden });
    @export(&hostCreateSignalState, .{ .name = "roc_host_create_signal_state", .visibility = .hidden });
    @export(&hostCreateSignalZipWith, .{ .name = "roc_host_create_signal_zip_with", .visibility = .hidden });
    @export(&hostSetText, .{ .name = "roc_host_set_text", .visibility = .hidden });

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

    if (spec_file == null) {
        writeStderr("Usage: ./app [--verbose] <test_spec.txt>\n");
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
    defer host_env.deinit();

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

    const hosted_fns = abi.hostedFunctions(.{
        .host_append_child = &hostedAppendChild,
        .host_bind_click = &hostedBindClick,
        .host_bind_signal_update = &hostedBindSignalUpdate,
        .host_bind_text = &hostedBindText,
        .host_create_element = &hostedCreateElement,
        .host_create_event_filter = &hostedCreateEventFilter,
        .host_create_event_map = &hostedCreateEventMap,
        .host_create_event_merge = &hostedCreateEventMerge,
        .host_create_event_source = &hostedCreateEventSource,
        .host_create_event_with_latest = &hostedCreateEventWithLatest,
        .host_create_root = &hostedCreateRoot,
        .host_create_signal_const = &hostedCreateSignalConst,
        .host_create_signal_fold = &hostedCreateSignalFold,
        .host_create_signal_hold = &hostedCreateSignalHold,
        .host_create_signal_map = &hostedCreateSignalMap,
        .host_create_signal_state = &hostedCreateSignalState,
        .host_create_signal_zip_with = &hostedCreateSignalZipWith,
        .host_set_text = &hostedSetText,
    });

    var roc_ops = abi.RocOps{
        .env = @ptrCast(&host_env),
        .roc_alloc = &rocAllocFn,
        .roc_dealloc = &rocDeallocFn,
        .roc_realloc = &rocReallocFn,
        .roc_dbg = &rocDbgFn,
        .roc_expect_failed = &rocExpectFailedFn,
        .roc_crashed = &rocCrashedFn,
        .hosted_fns = hosted_fns,
    };
    host_env.roc_ops = &roc_ops;
    current_host = &host_env;
    current_roc_ops = &roc_ops;
    defer current_host = null;
    defer current_roc_ops = null;

    // Call Roc main to build the UI
    traceStderr("[HOST] calling roc_main\n");
    var ret: [0]u8 = .{};
    abi.roc_main(&roc_ops, @ptrCast(&ret), null);
    traceStderr("[HOST] roc_main returned\n");

    if (verbose) {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "[INFO] UI built: {d} DOM elements, {d} graph nodes\n", .{ host_env.dom_elements.items.len, host_env.graph_nodes.items.len }) catch "";
        writeStderr(msg);

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
        }
    }

    if (verbose) {
        writeStderr("[PASS] All tests passed\n");
    }

    return 0;
}
