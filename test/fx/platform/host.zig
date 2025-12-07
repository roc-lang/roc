///! Platform host that tests effectful functions writing to stdout and stderr.
const std = @import("std");
const builtins = @import("builtins");
const build_options = @import("build_options");

const trace_refcount = build_options.trace_refcount;

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

/// Host environment - contains GeneralPurposeAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.GeneralPurposeAllocator(.{}),
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
    std.debug.print("[rocAllocFn ENTRY] roc_alloc=0x{x} env=0x{x}\n", .{ @intFromPtr(roc_alloc), @intFromPtr(env) });

    // Debug: print entry and check roc_alloc pointer alignment
    const roc_alloc_addr = @intFromPtr(roc_alloc);
    if (roc_alloc_addr % @alignOf(builtins.host_abi.RocAlloc) != 0) {
        std.debug.panic("[rocAllocFn] roc_alloc ptr not aligned! addr=0x{x} required={}", .{ roc_alloc_addr, @alignOf(builtins.host_abi.RocAlloc) });
    }

    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("rocAllocFn: env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }

    std.debug.print("[rocAllocFn] about to @alignCast env\n", .{});
    const host: *HostEnv = @ptrCast(@alignCast(env));
    std.debug.print("[rocAllocFn] @alignCast env done, getting allocator\n", .{});
    const allocator = host.gpa.allocator();

    // The allocation must be at least 8-byte aligned because:
    // 1. The refcount (isize/usize) is stored before the data and needs proper alignment
    // 2. The builtins code casts data pointers to [*]isize for refcount access
    const min_alignment: usize = @max(roc_alloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate additional bytes needed to store the size
    const size_storage_bytes = @max(roc_alloc.alignment, @alignOf(usize));
    const total_size = roc_alloc.length + size_storage_bytes;

    // Allocate memory including space for size metadata
    const result = allocator.rawAlloc(total_size, align_enum, @returnAddress());

    const base_ptr = result orelse {
        const stderr: std.fs.File = .stderr();
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "\x1b[31mHost error:\x1b[0m allocation failed for size={d} align={d}\n", .{
            total_size,
            roc_alloc.alignment,
        }) catch "\x1b[31mHost error:\x1b[0m allocation failed, out of memory\n";
        stderr.writeAll(msg) catch {};
        std.process.exit(1);
    };

    // Debug check: verify the allocator returned properly aligned memory
    const base_addr = @intFromPtr(base_ptr);
    if (base_addr % min_alignment != 0) {
        @panic("Host allocator returned misaligned memory in rocAllocFn");
    }

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    // Debug check: verify the returned pointer is also properly aligned
    const answer_addr = @intFromPtr(roc_alloc.answer);
    if (answer_addr % roc_alloc.alignment != 0) {
        @panic("Host allocator returned misaligned answer in rocAllocFn");
    }

    if (trace_refcount) {
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ @intFromPtr(roc_alloc.answer), roc_alloc.length, roc_alloc.alignment });
    }
}

/// Roc deallocation function with size-tracking metadata
fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocDeallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    // Use same minimum alignment as alloc
    const min_alignment: usize = @max(roc_dealloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate where the size metadata is stored
    const size_storage_bytes = @max(roc_dealloc.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;

    if (trace_refcount) {
        std.debug.print("[DEALLOC] ptr=0x{x} align={d} total_size={d} size_storage={d}\n", .{
            @intFromPtr(roc_dealloc.ptr),
            roc_dealloc.alignment,
            total_size,
            size_storage_bytes,
        });
    }

    // Calculate the base pointer (start of actual allocation)
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_dealloc.ptr) - size_storage_bytes);

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    allocator.rawFree(slice, align_enum, @returnAddress());
}

/// Roc reallocation function with size-tracking metadata
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
    std.debug.print("[HOST REALLOC] answer=0x{x} alignment={} new_length={}\n", .{
        @intFromPtr(roc_realloc.answer),
        roc_realloc.alignment,
        roc_realloc.new_length,
    });
    // Debug check: verify env is properly aligned for HostEnv
    const env_addr = @intFromPtr(env);
    if (env_addr % @alignOf(HostEnv) != 0) {
        std.debug.panic("[rocReallocFn] env=0x{x} not aligned to {} bytes", .{ env_addr, @alignOf(HostEnv) });
    }
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    // Use same minimum alignment as alloc
    const min_alignment: usize = @max(roc_realloc.alignment, @alignOf(usize));
    const align_enum = std.mem.Alignment.fromByteUnits(min_alignment);

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = roc_realloc.new_length + size_storage_bytes;

    // Free old memory and allocate new with proper alignment
    // This is necessary because Zig's realloc infers alignment from slice type ([]u8 = alignment 1)
    // which could cause the new allocation to be misaligned
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];

    // Allocate new memory with proper alignment
    const new_ptr = allocator.rawAlloc(new_total_size, align_enum, @returnAddress()) orelse {
        const stderr: std.fs.File = .stderr();
        stderr.writeAll("\x1b[31mHost error:\x1b[0m reallocation failed, out of memory\n") catch {};
        std.process.exit(1);
    };

    // Copy old data to new location
    const copy_size = @min(old_total_size, new_total_size);
    @memcpy(new_ptr[0..copy_size], old_slice[0..copy_size]);

    // Free old memory
    allocator.rawFree(old_slice, align_enum, @returnAddress());

    const new_slice = new_ptr[0..new_total_size];

    // Store the new total size in the metadata
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;

    // Return pointer to the user data (after the size metadata)
    roc_realloc.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[REALLOC] old=0x{x} new=0x{x} new_size={d}\n", .{ @intFromPtr(old_base_ptr) + size_storage_bytes, @intFromPtr(roc_realloc.answer), roc_realloc.new_length });
    }
}

/// Roc debug function
fn rocDbgFn(roc_dbg: *const builtins.host_abi.RocDbg, env: *anyopaque) callconv(.c) void {
    _ = env;
    const message = roc_dbg.utf8_bytes[0..roc_dbg.len];
    std.debug.print("ROC DBG: {s}\n", .{message});
}

/// Roc expect failed function
fn rocExpectFailedFn(roc_expect: *const builtins.host_abi.RocExpectFailed, env: *anyopaque) callconv(.c) void {
    _ = env;
    const source_bytes = roc_expect.utf8_bytes[0..roc_expect.len];
    const trimmed = std.mem.trim(u8, source_bytes, " \t\n\r");
    std.debug.print("Expect failed: {s}\n", .{trimmed});
}

/// Roc crashed function
fn rocCrashedFn(roc_crashed: *const builtins.host_abi.RocCrashed, env: *anyopaque) callconv(.c) noreturn {
    _ = env;
    const message = roc_crashed.utf8_bytes[0..roc_crashed.len];
    const stderr: std.fs.File = .stderr();
    var buf: [256]u8 = undefined;
    var w = stderr.writer(&buf);
    w.interface.print("\n\x1b[31mRoc crashed:\x1b[0m {s}\n", .{message}) catch {};
    w.interface.flush() catch {};
    std.process.exit(1);
}

// External symbols provided by the Roc runtime object file
// Follows RocCall ABI: ops, ret_ptr, then argument pointers
extern fn roc__main(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, arg_ptr: ?*anyopaque) callconv(.c) void;

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
    _ = argc;
    _ = argv;
    platform_main() catch |err| {
        const stderr: std.fs.File = .stderr();
        stderr.writeAll("HOST ERROR: ") catch {};
        stderr.writeAll(@errorName(err)) catch {};
        stderr.writeAll("\n") catch {};
        return 1;
    };
    return 0;
}

// Use the actual RocStr from builtins instead of defining our own
const RocStr = builtins.str.RocStr;

/// Hosted function: Stderr.line! (index 0 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStderrLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ops;
    _ = ret_ptr; // Return value is {} which is zero-sized

    // Arguments struct for single Str parameter
    const Args = extern struct { str: RocStr };
    // Debug check: verify args_ptr is properly aligned for Args
    const args_addr = @intFromPtr(args_ptr);
    if (args_addr % @alignOf(Args) != 0) {
        std.debug.panic("[hostedStderrLine] args_ptr=0x{x} not aligned to {} bytes", .{ args_addr, @alignOf(Args) });
    }
    const args: *Args = @ptrCast(@alignCast(args_ptr));

    const message = args.str.asSlice();
    const stderr: std.fs.File = .stderr();
    stderr.writeAll(message) catch {};
    stderr.writeAll("\n") catch {};
}

/// Hosted function: Stdin.line! (index 1 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns Str and takes {} as argument
fn hostedStdinLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = args_ptr; // Argument is {} which is zero-sized

    // Read a line from stdin
    var buffer: [4096]u8 = undefined;
    const stdin_file: std.fs.File = .stdin();
    const bytes_read = stdin_file.read(&buffer) catch {
        // Return empty string on error
        const result: *RocStr = @ptrCast(@alignCast(ret_ptr));
        result.* = RocStr.empty();
        return;
    };

    // Handle EOF (no bytes read)
    if (bytes_read == 0) {
        const result: *RocStr = @ptrCast(@alignCast(ret_ptr));
        result.* = RocStr.empty();
        return;
    }

    // Find newline and trim it (handle both \n and \r\n)
    const line_with_newline = buffer[0..bytes_read];
    var line = if (std.mem.indexOfScalar(u8, line_with_newline, '\n')) |newline_idx|
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
    const result: *RocStr = @ptrCast(@alignCast(ret_ptr));
    result.* = RocStr.fromSlice(line, ops);
}

/// Hosted function: Stdout.line! (index 2 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStdoutLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ops;
    _ = ret_ptr; // Return value is {} which is zero-sized

    // Arguments struct for single Str parameter
    const Args = extern struct { str: RocStr };
    // Debug check: verify args_ptr is properly aligned for Args
    const args_addr = @intFromPtr(args_ptr);
    if (args_addr % @alignOf(Args) != 0) {
        std.debug.panic("[hostedStdoutLine] args_ptr=0x{x} not aligned to {} bytes", .{ args_addr, @alignOf(Args) });
    }
    const args: *Args = @ptrCast(@alignCast(args_ptr));

    const message = args.str.asSlice();
    const stdout: std.fs.File = .stdout();
    stdout.writeAll(message) catch {};
    stdout.writeAll("\n") catch {};
}

/// Array of hosted function pointers, sorted alphabetically by fully-qualified name
/// These correspond to the hosted functions defined in Stderr, Stdin, and Stdout Type Modules
const hosted_function_ptrs = [_]builtins.host_abi.HostedFn{
    hostedStderrLine, // Stderr.line! (index 0)
    hostedStdinLine, // Stdin.line! (index 1)
    hostedStdoutLine, // Stdout.line! (index 2)
};

/// Platform host entrypoint
fn platform_main() !void {
    std.debug.print("[HOST platform_main] ENTRY\n", .{});
    var host_env = HostEnv{
        .gpa = std.heap.GeneralPurposeAllocator(.{}){},
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

    // Call the app's main! entrypoint
    var ret: [0]u8 = undefined; // Result is {} which is zero-sized
    var args: [0]u8 = undefined;
    // Note: although this is a function with no args and a zero-sized return value,
    // we can't currently pass null pointers for either of these because Roc will
    // currently dereference both of these eagerly even though it won't use either,
    // causing a segfault if you pass null. This should be changed! Dereferencing
    // garbage memory is obviously pointless, and there's no reason we should do it.
    std.debug.print("[HOST platform_main] About to call roc__main\n", .{});
    std.debug.print("[HOST platform_main] roc_ops.env=0x{x} align check: {}\n", .{
        @intFromPtr(roc_ops.env),
        @intFromPtr(roc_ops.env) % @alignOf(HostEnv) == 0,
    });
    roc__main(&roc_ops, @as(*anyopaque, @ptrCast(&ret)), @as(*anyopaque, @ptrCast(&args)));
    std.debug.print("[HOST platform_main] roc__main returned\n", .{});
}
