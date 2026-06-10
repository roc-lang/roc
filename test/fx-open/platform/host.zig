///! Platform host that tests effectful functions with open union error types.
const std = @import("std");
const Allocator = std.mem.Allocator;
const shim_io = @import("shim_io");
const builtins = @import("builtins");
const build_options = @import("build_options");

const trace_refcount = build_options.trace_refcount;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
pub const std_options_debug_io = shim_io.io();
pub const std_options_debug_threaded_io = null;
// See `shim_io.std_options_no_stack_tracing` for why stack tracing is disabled.
pub const std_options = shim_io.std_options_no_stack_tracing;

/// Host environment - contains DebugAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.DebugAllocator(.{ .thread_safe = false }),
    std_io: std.Io,
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(ops: *builtins.host_abi.RocOps, length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alignment)));

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const total_size = length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "\x1b[31mHost error:\x1b[0m allocation failed for size={d} align={d}\n", .{
            total_size,
            alignment,
        }) catch "\x1b[31mHost error:\x1b[0m allocation failed, out of memory\n";
        std.debug.print("{s}", .{msg});
        std.process.exit(1);
    };

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    const answer: *anyopaque = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ @intFromPtr(answer), length, alignment });
    }

    return answer;
}

/// Roc deallocation function with size-tracking metadata
fn rocDeallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    if (trace_refcount) {
        std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{
            @intFromPtr(ptr),
            alignment,
            total_size,
            size_storage_bytes,
        });
    }

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    // Use same alignment calculation as alloc
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alignment)));

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    allocator.rawFree(slice, align_enum, @returnAddress());
}

/// Roc reallocation function with size-tracking metadata
fn rocReallocFn(ops: *builtins.host_abi.RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const allocator = host.gpa.allocator();

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(ptr) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = new_length + size_storage_bytes;

    // Perform reallocation
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = allocator.realloc(old_slice, new_total_size) catch {
        std.debug.print("{s}", .{"\x1b[31mHost error:\x1b[0m reallocation failed, out of memory\n"});
        std.process.exit(1);
    };

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    const answer: *anyopaque = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(old_base_ptr) + size_storage_bytes, @intFromPtr(answer), new_length });
    }

    return answer;
}

/// Roc debug function
fn rocDbgFn(ops: *builtins.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    _ = ops;
    const message = bytes[0..len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

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

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
// main_for_host! takes List(Str) and returns I32
extern fn roc_main(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

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

/// Hosted function: Stderr.line! (index 0 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStderrLine(ops: *RocOps, str: RocStr) callconv(.c) void {
    _ = ops;
    const message = str.asSlice();
    std.debug.print("{s}", .{message});
    std.debug.print("{s}", .{"\n"});
}

/// Hosted function: Stdin.line! (index 1 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns Str and takes {} as argument
fn hostedStdinLine(ops: *RocOps) callconv(.c) RocStr {
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

/// Hosted function: Stdout.line! (index 2 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStdoutLine(ops: *RocOps, str: RocStr) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const message = str.asSlice();
    std.Io.File.stdout().writeStreamingAll(host.std_io, message) catch {};
    std.Io.File.stdout().writeStreamingAll(host.std_io, "\n") catch {};
}

/// Array of hosted function pointers, sorted alphabetically by fully-qualified name
/// These correspond to the hosted functions defined in Stderr, Stdin, and Stdout Type Modules
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{
    builtins.host_abi.hostedFn(&hostedStderrLine), // Stderr.line! (index 0)
    builtins.host_abi.hostedFn(&hostedStdinLine), // Stdin.line! (index 1)
    builtins.host_abi.hostedFn(&hostedStdoutLine), // Stdout.line! (index 2)
};

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
        .gpa = std.heap.DebugAllocator(.{ .thread_safe = false }){},
        .std_io = shim_io.io(),
    };
    defer {
        const leaked = host_env.gpa.deinit();
        if (leaked == .leak) {
            std.log.err("\x1b[33mMemory leak detected!\x1b[0m", .{});
        }
    }

    // Create the RocOps struct
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

    // Build the args list
    var args = buildArgsList(&roc_ops, argc, argv);

    // Call the app's main_for_host! entrypoint which returns I32
    var exit_code: i32 = 0;
    roc_main(&roc_ops, @as(*anyopaque, @ptrCast(&exit_code)), @as(*anyopaque, @ptrCast(&args)));

    // Note: We don't explicitly free the args list here because:
    // 1. The process is about to exit anyway
    // 2. Properly freeing would require a Dec function for RocStr elements
    // The GPA leak detection is fine with this since OS will reclaim memory

    return exit_code;
}
