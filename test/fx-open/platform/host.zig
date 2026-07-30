///! Platform host that tests effectful functions with open union error types.
const std = @import("std");
const Allocator = std.mem.Allocator;
const shim_io = @import("shim_io");
const builtins = @import("builtins");
const build_options = @import("build_options");
const host_alloc = @import("host_alloc");

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_no_stack_tracing` for why stack tracing is disabled.
pub const std_options = shim_io.std_options_no_stack_tracing;

/// Host environment - contains DebugAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .thread_safe = false, .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }),
    std_io: std.Io,

    pub fn rocAllocator(self: *HostEnv) Allocator {
        return self.gpa.allocator();
    }
};

const callbacks = host_alloc.Callbacks(HostEnv);

/// Roc expect failed function
fn rocExpectFailedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const source_bytes = bytes[0..len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    var buf: [512]u8 = undefined;
    const msg = std.fmt.bufPrint(&buf, "\n\x1b[31mRoc crashed:\x1b[0m {s}\n", .{message}) catch "\n\x1b[31mRoc crashed\x1b[0m\n";
    std.debug.print("{s}", .{msg});
    std.process.exit(1);
}

// The app's entrypoint, exported under its provides symbol with its natural
// C ABI: main_for_host! takes List(Str) and returns I32.
extern fn roc_main(args: RocList) callconv(.c) i32;

// OS-specific entry point handling
comptime {
    // Export main for all platforms
    @export(&main, .{ .name = "main" });

    // Windows MinGW/MSVCRT compatibility: export __main stub
    if (@import("builtin").os.tag == .windows) {
        @export(&__main, .{ .name = "__main" });
    }
}

// Windows MinGW/MSVCRT compatibility stub
// The C runtime on Windows calls __main from main for constructor initialization
fn __main() callconv(.c) void {}

// C compatible main for runtime
fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int {
    const exit_code = platform_main(argc, argv) catch |err| {
        std.debug.print("{s}", .{"HOST ERROR: "});
        std.debug.print("{s}", .{@errorName(err)});
        std.debug.print("{s}", .{"\n"});
        return 1;
    };
    return exit_code;
}

// Use the actual RocStr and RocList from builtins
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;

// The host's private RocOps. Hosted functions have natural C ABIs with no ops
// parameter, so they reach the host's allocator and std.Io through this
// global, set by platform_main before any Roc code runs.
var g_roc_ops: ?*RocOps = null;

/// Hosted function: Stderr.line!
/// Returns {} and takes Str as argument; ownership of the Str transfers here.
fn hostedStderrLine(str: RocStr) callconv(.c) void {
    const ops = g_roc_ops.?;
    var owned = str;
    defer owned.decref(ops);
    const message = owned.asSlice();
    std.debug.print("{s}", .{message});
    std.debug.print("{s}", .{"\n"});
}

/// Hosted function: Stdin.line!
/// Returns Str and takes no arguments.
fn hostedStdinLine() callconv(.c) RocStr {
    const ops = g_roc_ops.?;
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    // Read a line from stdin
    var buffer: [4096]u8 = undefined;
    const bytes_read = std.Io.File.stdin().readStreaming(host.std_io, &.{&buffer}) catch {
        // Return empty string on error
        return RocStr.empty();
    };

    // Handle EOF (no bytes read)
    if (bytes_read == 0) {
        return RocStr.empty();
    }

    // Find newline and trim it (handle both \n and \r\n)
    const line_with_newline = buffer[0..bytes_read];
    var line = if (std.mem.findScalar(u8, line_with_newline, '\n')) |newline_idx|
        line_with_newline[0..newline_idx]
    else
        line_with_newline;

    // Also trim trailing \r for Windows line endings
    if (line.len > 0 and line[line.len - 1] == '\r') {
        line = line[0 .. line.len - 1];
    }

    // Create RocStr from the read line and return it
    // RocStr.fromSlice handles allocation internally (either inline for small strings
    // or via roc_alloc for big strings with proper refcount tracking)
    return RocStr.fromSlice(line, ops);
}

/// Hosted function: Stdout.line!
/// Returns {} and takes Str as argument; ownership of the Str transfers here.
fn hostedStdoutLine(str: RocStr) callconv(.c) void {
    const ops = g_roc_ops.?;
    var owned = str;
    defer owned.decref(ops);
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const message = owned.asSlice();
    std.Io.File.stdout().writeStreamingAll(host.std_io, message) catch {};
    std.Io.File.stdout().writeStreamingAll(host.std_io, "\n") catch {};
}

// Matches the Roc type `Try(Str, [HostErr(Str)])` for FallibleHost.str_ok!.
const FallibleStrResultTag = enum(u8) {
    err = 0,
    ok = 1,
};

const FallibleStrResult = extern struct {
    payload: extern union {
        err: RocStr,
        ok: RocStr,
    },
    tag: FallibleStrResultTag,
};

/// Hosted function: FallibleHost.str_ok!
/// Always returns Ok("ok").
fn hostedFallibleStrOk() callconv(.c) FallibleStrResult {
    const ops = g_roc_ops.?;
    return .{
        .payload = .{ .ok = RocStr.fromSlice("ok", ops) },
        .tag = .ok,
    };
}

// --- Symbol-ABI runtime exports
// The fixed runtime symbols every symbol-ABI host defines, plus this
// platform's hosted function symbols. All hidden: they are link-time plumbing
// between the app and the host, not part of the host binary's public API.

fn getOps() *RocOps {
    return g_roc_ops.?;
}

comptime {
    @export(&hostedFallibleStrOk, .{ .name = "roc_fallible_str_ok", .visibility = .hidden });
    @export(&hostedStderrLine, .{ .name = "roc_stderr_line", .visibility = .hidden });
    @export(&hostedStdinLine, .{ .name = "roc_stdin_line", .visibility = .hidden });
    @export(&hostedStdoutLine, .{ .name = "roc_stdout_line", .visibility = .hidden });

    host_alloc.exportRuntimeSymbols(getOps, .{});
}

/// Build a RocList of RocStr from argc/argv
fn buildArgsList(ops: *builtins.host_abi.RocOps, argc: c_int, argv: [*][*:0]u8) RocList {
    const count: usize = @intCast(argc);
    if (count == 0) {
        return RocList.empty();
    }

    // Allocate the list (RocStr is refcounted)
    const list = RocList.list_allocate(@alignOf(RocStr), count, @sizeOf(RocStr), true, ops);

    // Get pointer to list data
    const list_ptr: [*]RocStr = @ptrCast(@alignCast(list.bytes));

    // Fill in each string
    for (0..count) |i| {
        const arg = argv[i];
        const len = std.mem.len(arg);
        list_ptr[i] = RocStr.fromSlice(arg[0..len], ops);
    }

    return list;
}

/// Platform host entrypoint
fn platform_main(argc: c_int, argv: [*][*:0]u8) Allocator.Error!c_int {
    var host_env = HostEnv{
        .gpa = std.heap.DebugAllocator(.{ .thread_safe = false, .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }){},
        .std_io = shim_io.io(),
    };
    defer {
        const leaked = host_env.gpa.deinit();
        if (leaked == .leak) {
            std.log.err("\x1b[33mMemory leak detected!\x1b[0m", .{});
            std.debug.print("{s}", .{build_options.debug_gpa_leak_hint});
        }
    }

    // The host's private RocOps for using builtins helpers (RocStr/RocList
    // allocation, decref). Not part of the ABI.
    var roc_ops = builtins.host_abi.RocOps{
        .env = @as(*anyopaque, @ptrCast(&host_env)),
        .roc_alloc = callbacks.rocAllocFn,
        .roc_dealloc = callbacks.rocDeallocFn,
        .roc_realloc = callbacks.rocReallocFn,
        .roc_dbg = callbacks.rocDbgFn,
        .roc_expect_failed = rocExpectFailedFn,
        .roc_crashed = rocCrashedFn,
        .hosted_fns = .{ .count = 0, .fns = undefined },
    };
    g_roc_ops = &roc_ops;

    // Build the args list; ownership transfers to the entrypoint.
    const args = buildArgsList(&roc_ops, argc, argv);

    // Call the app's main_for_host! entrypoint with its natural C ABI.
    const exit_code: i32 = roc_main(args);

    return exit_code;
}
