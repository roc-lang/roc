///! Platform host that tests effectful functions writing to stdout and stderr.
const std = @import("std");
const builtins = @import("builtins");
const build_options = @import("build_options");

const trace_refcount = build_options.trace_refcount;

/// Type of IO operation in test spec
const EffectType = enum(u8) {
    stdin_input, // 0<
    stdout_expect, // 1>
    stderr_expect, // 2>
};

/// A single entry in the test spec
const SpecEntry = struct {
    effect_type: EffectType,
    value: []const u8,
    spec_line: usize, // For error reporting
};

/// Test state for simulated IO mode
const TestState = struct {
    enabled: bool,
    verbose: bool,
    entries: []const SpecEntry,
    current_index: usize,
    failed: bool,
    failure_info: ?FailureInfo,

    const FailureInfo = struct {
        expected_type: EffectType,
        expected_value: []const u8,
        actual_type: EffectType,
        spec_line: usize,
    };

    fn init() TestState {
        return .{
            .enabled = false,
            .verbose = false,
            .entries = &.{},
            .current_index = 0,
            .failed = false,
            .failure_info = null,
        };
    }
};

/// Parse test spec string into array of SpecEntry
/// Format: "0<input|1>output|2>error" (pipe-separated)
fn parseTestSpec(allocator: std.mem.Allocator, spec: []const u8) ![]SpecEntry {
    var entries = try std.ArrayList(SpecEntry).initCapacity(allocator, 8);
    errdefer entries.deinit(allocator);

    var line_num: usize = 1;
    // Split on pipe character
    var iter = std.mem.splitScalar(u8, spec, '|');

    while (iter.next()) |line| {
        defer line_num += 1;

        if (line.len < 2) continue; // Skip empty/short lines

        const effect_type: EffectType = blk: {
            if (line[0] == '0' and line[1] == '<') break :blk .stdin_input;
            if (line[0] == '1' and line[1] == '>') break :blk .stdout_expect;
            if (line[0] == '2' and line[1] == '>') break :blk .stderr_expect;
            continue; // Skip invalid lines
        };

        try entries.append(allocator, .{
            .effect_type = effect_type,
            .value = line[2..],
            .spec_line = line_num,
        });
    }

    return try entries.toOwnedSlice(allocator);
}

/// Host environment - contains GeneralPurposeAllocator for leak detection
const HostEnv = struct {
    gpa: std.heap.GeneralPurposeAllocator(.{}),
    test_state: TestState,
};

/// Roc allocation function with size-tracking metadata
fn rocAllocFn(roc_alloc: *builtins.host_abi.RocAlloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(roc_alloc.alignment)));

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

    // Store the total size (including metadata) right before the user data
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;

    // Return pointer to the user data (after the size metadata)
    roc_alloc.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);

    if (trace_refcount) {
        std.debug.print("[ALLOC] ptr=0x{x} size={d} align={d}\n", .{ @intFromPtr(roc_alloc.answer), roc_alloc.length, roc_alloc.alignment });
    }
}

/// Roc deallocation function with size-tracking metadata
fn rocDeallocFn(roc_dealloc: *builtins.host_abi.RocDealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

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

    // Use same alignment calculation as alloc
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(roc_dealloc.alignment)));

    // Free the memory (including the size metadata)
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    allocator.rawFree(slice, align_enum, @returnAddress());
}

/// Roc reallocation function with size-tracking metadata
fn rocReallocFn(roc_realloc: *builtins.host_abi.RocRealloc, env: *anyopaque) callconv(.c) void {
    const host: *HostEnv = @ptrCast(@alignCast(env));
    const allocator = host.gpa.allocator();

    // Calculate where the size metadata is stored for the old allocation
    const size_storage_bytes = @max(roc_realloc.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(roc_realloc.answer) - @sizeOf(usize));

    // Read the old total size from metadata
    const old_total_size = old_size_ptr.*;

    // Calculate the old base pointer (start of actual allocation)
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(roc_realloc.answer) - size_storage_bytes);

    // Calculate new total size needed
    const new_total_size = roc_realloc.new_length + size_storage_bytes;

    // Perform reallocation
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = allocator.realloc(old_slice, new_total_size) catch {
        const stderr: std.fs.File = .stderr();
        stderr.writeAll("\x1b[31mHost error:\x1b[0m reallocation failed, out of memory\n") catch {};
        std.process.exit(1);
    };

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
    // Parse --test or --test-verbose argument
    var test_spec: ?[]const u8 = null;
    var test_verbose: bool = false;
    var i: usize = 1;
    const arg_count: usize = @intCast(argc);
    const stderr_file: std.fs.File = .stderr();
    while (i < arg_count) : (i += 1) {
        const arg = std.mem.span(argv[i]);
        if (std.mem.eql(u8, arg, "--test-verbose")) {
            if (i + 1 < arg_count) {
                i += 1;
                test_spec = std.mem.span(argv[i]);
                test_verbose = true;
            } else {
                stderr_file.writeAll("Error: --test-verbose requires a spec argument\n") catch {};
                return 1;
            }
        } else if (std.mem.eql(u8, arg, "--test")) {
            if (i + 1 < arg_count) {
                i += 1;
                test_spec = std.mem.span(argv[i]);
            } else {
                stderr_file.writeAll("Error: --test requires a spec argument\n") catch {};
                return 1;
            }
        } else if (arg.len >= 2 and arg[0] == '-' and arg[1] == '-') {
            stderr_file.writeAll("Error: unknown flag '") catch {};
            stderr_file.writeAll(arg) catch {};
            stderr_file.writeAll("'\n") catch {};
            stderr_file.writeAll("Usage: <app> [--test <spec>] [--test-verbose <spec>]\n") catch {};
            return 1;
        }
    }

    const exit_code = platform_main(test_spec, test_verbose) catch |err| {
        stderr_file.writeAll("HOST ERROR: ") catch {};
        stderr_file.writeAll(@errorName(err)) catch {};
        stderr_file.writeAll("\n") catch {};
        return 1;
    };
    return exit_code;
}

// Use the actual RocStr from builtins instead of defining our own
const RocStr = builtins.str.RocStr;

/// Hosted function: Stderr.line! (index 0 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStderrLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ret_ptr; // Return value is {} which is zero-sized

    // Arguments struct for single Str parameter
    const Args = extern struct { str: RocStr };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const message = args.str.asSlice();

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: verify output matches expected
    if (host.test_state.enabled) {
        const stderr_file: std.fs.File = .stderr();
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stderr_expect and std.mem.eql(u8, entry.value, message)) {
                host.test_state.current_index += 1;
                if (host.test_state.verbose) {
                    stderr_file.writeAll("[OK] stderr: \"") catch {};
                    stderr_file.writeAll(message) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                }
                return; // Match!
            }
            // Mismatch
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = entry.effect_type,
                .expected_value = entry.value,
                .actual_type = .stderr_expect,
                .spec_line = entry.spec_line,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stderr: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (expected ") catch {};
                stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                stderr_file.writeAll(": \"") catch {};
                stderr_file.writeAll(entry.value) catch {};
                stderr_file.writeAll("\")\n") catch {};
            }
        } else {
            // Extra output not in spec
            host.test_state.failed = true;
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stderr: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (unexpected - no more expected operations)\n") catch {};
            }
        }
        return;
    }

    // Normal mode: write to stderr
    const stderr: std.fs.File = .stderr();
    stderr.writeAll(message) catch {};
    stderr.writeAll("\n") catch {};
}

/// Hosted function: Stdin.line! (index 1 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns Str and takes {} as argument
fn hostedStdinLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = args_ptr; // Argument is {} which is zero-sized

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));
    const result: *RocStr = @ptrCast(@alignCast(ret_ptr));

    // Test mode: consume next stdin_input entry from spec
    if (host.test_state.enabled) {
        const stderr_file: std.fs.File = .stderr();
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stdin_input) {
                host.test_state.current_index += 1;
                result.* = RocStr.fromSlice(entry.value, ops);
                if (host.test_state.verbose) {
                    stderr_file.writeAll("[OK] stdin: \"") catch {};
                    stderr_file.writeAll(entry.value) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                }
                return;
            }
            // Wrong type - expected stdin but spec has output
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = entry.effect_type,
                .expected_value = entry.value,
                .actual_type = .stdin_input,
                .spec_line = entry.spec_line,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdin read (expected ") catch {};
                stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                stderr_file.writeAll(": \"") catch {};
                stderr_file.writeAll(entry.value) catch {};
                stderr_file.writeAll("\")\n") catch {};
            }
        } else {
            // Ran out of entries - app tried to read more stdin than provided
            host.test_state.failed = true;
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdin read (unexpected - no more expected operations)\n") catch {};
            }
        }
        result.* = RocStr.empty();
        return;
    }

    // Normal mode: Read a line from stdin
    var buffer: [4096]u8 = undefined;
    const stdin_file: std.fs.File = .stdin();
    const bytes_read = stdin_file.read(&buffer) catch {
        // Return empty string on error
        result.* = RocStr.empty();
        return;
    };

    // Handle EOF (no bytes read)
    if (bytes_read == 0) {
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
    result.* = RocStr.fromSlice(line, ops);
}

/// Hosted function: Stdout.line! (index 2 - sorted alphabetically)
/// Follows RocCall ABI: (ops, ret_ptr, args_ptr)
/// Returns {} and takes Str as argument
fn hostedStdoutLine(ops: *builtins.host_abi.RocOps, ret_ptr: *anyopaque, args_ptr: *anyopaque) callconv(.c) void {
    _ = ret_ptr; // Return value is {} which is zero-sized

    // Arguments struct for single Str parameter
    const Args = extern struct { str: RocStr };
    const args: *Args = @ptrCast(@alignCast(args_ptr));
    const message = args.str.asSlice();

    const host: *HostEnv = @ptrCast(@alignCast(ops.env));

    // Test mode: verify output matches expected
    if (host.test_state.enabled) {
        const stderr_file: std.fs.File = .stderr();
        if (host.test_state.current_index < host.test_state.entries.len) {
            const entry = host.test_state.entries[host.test_state.current_index];
            if (entry.effect_type == .stdout_expect and std.mem.eql(u8, entry.value, message)) {
                host.test_state.current_index += 1;
                if (host.test_state.verbose) {
                    stderr_file.writeAll("[OK] stdout: \"") catch {};
                    stderr_file.writeAll(message) catch {};
                    stderr_file.writeAll("\"\n") catch {};
                }
                return; // Match!
            }
            // Mismatch
            host.test_state.failed = true;
            host.test_state.failure_info = .{
                .expected_type = entry.effect_type,
                .expected_value = entry.value,
                .actual_type = .stdout_expect,
                .spec_line = entry.spec_line,
            };
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdout: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (expected ") catch {};
                stderr_file.writeAll(effectTypeName(entry.effect_type)) catch {};
                stderr_file.writeAll(": \"") catch {};
                stderr_file.writeAll(entry.value) catch {};
                stderr_file.writeAll("\")\n") catch {};
            }
        } else {
            // Extra output not in spec
            host.test_state.failed = true;
            if (host.test_state.verbose) {
                stderr_file.writeAll("[FAIL] stdout: \"") catch {};
                stderr_file.writeAll(message) catch {};
                stderr_file.writeAll("\" (unexpected - no more expected operations)\n") catch {};
            }
        }
        return;
    }

    // Normal mode: write to stdout
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
fn platform_main(test_spec: ?[]const u8, test_verbose: bool) !c_int {
    var host_env = HostEnv{
        .gpa = std.heap.GeneralPurposeAllocator(.{}){},
        .test_state = TestState.init(),
    };

    // Parse test spec if provided
    if (test_spec) |spec| {
        host_env.test_state.entries = try parseTestSpec(host_env.gpa.allocator(), spec);
        host_env.test_state.enabled = true;
        host_env.test_state.verbose = test_verbose;
    }

    defer {
        // Free test entries if allocated
        if (host_env.test_state.entries.len > 0) {
            host_env.gpa.allocator().free(host_env.test_state.entries);
        }

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
    roc__main(&roc_ops, @as(*anyopaque, @ptrCast(&ret)), @as(*anyopaque, @ptrCast(&args)));

    // Check test results if in test mode
    if (host_env.test_state.enabled) {
        // Check if test failed or not all entries were consumed
        if (host_env.test_state.failed or host_env.test_state.current_index != host_env.test_state.entries.len) {
            const stderr_file: std.fs.File = .stderr();

            // Print failure info
            if (host_env.test_state.failure_info) |info| {
                var buf: [512]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "TEST FAILED at spec line {d}:\n  Expected: {s} \"{s}\"\n  Got:      {s}\n", .{
                    info.spec_line,
                    effectTypeName(info.expected_type),
                    info.expected_value,
                    effectTypeName(info.actual_type),
                }) catch "TEST FAILED\n";
                stderr_file.writeAll(msg) catch {};
            } else if (host_env.test_state.current_index < host_env.test_state.entries.len) {
                // Not all entries were consumed
                const remaining = host_env.test_state.entries.len - host_env.test_state.current_index;
                var buf: [256]u8 = undefined;
                const msg = std.fmt.bufPrint(&buf, "TEST FAILED: {d} expected IO operation(s) not performed\n", .{remaining}) catch "TEST FAILED: expected IO operations not performed\n";
                stderr_file.writeAll(msg) catch {};
            } else {
                stderr_file.writeAll("TEST FAILED\n") catch {};
            }

            return 1;
        }
    }

    return 0;
}

fn effectTypeName(effect_type: EffectType) []const u8 {
    return switch (effect_type) {
        .stdin_input => "stdin",
        .stdout_expect => "stdout",
        .stderr_expect => "stderr",
    };
}
