//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dllvm -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const fmt = @import("fmt.zig");
const base = @import("base.zig");
const collections = @import("collections.zig");
const reporting = @import("reporting.zig");
const build_options = @import("build_options");
// const coordinate = @import("coordinate.zig");
const coordinate_simple = @import("coordinate_simple.zig");

const tracy = @import("tracy.zig");
const Filesystem = @import("fs/Filesystem.zig");
const cli_args = @import("cli_args.zig");
const cache_mod = @import("cache/mod.zig");
const CacheManager = cache_mod.CacheManager;
const CacheConfig = cache_mod.CacheConfig;
const tokenize = @import("check/parse/tokenize.zig");
const parse = @import("check/parse.zig");
const bench = @import("bench.zig");
const linker = @import("linker.zig");

const builtin = @import("builtin");
const shared_memory_source = @embedFile("shared_memory.zig");
const c = std.c;

// External C functions for POSIX shared memory
extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: c.mode_t) c_int;
extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: c.off_t) ?*anyopaque;
extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;

const benchTokenizer = bench.benchTokenizer;
const benchParse = bench.benchParse;

const Allocator = std.mem.Allocator;
const ColorPalette = reporting.ColorPalette;

const legalDetailsFileContent = @embedFile("legal_details");

/// The CLI entrypoint for the Roc compiler.
pub fn main() !void {
    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa = std.heap.c_allocator;

    if (tracy.enable_allocation) {
        gpa_tracy = tracy.tracyAllocator(gpa);
        gpa = gpa_tracy.allocator();
    }

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try std.process.argsAlloc(arena);

    const result = mainArgs(gpa, arena, args);
    if (tracy.enable) {
        try tracy.waitForShutdown();
    }
    return result;
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const parsed_args = try cli_args.parse(gpa, args[1..]);
    defer parsed_args.deinit(gpa);

    try switch (parsed_args) {
        .run => |run_args| rocRun(gpa, run_args),
        .check => |check_args| rocCheck(gpa, check_args),
        .build => |build_args| rocBuild(gpa, build_args),
        .format => |format_args| rocFormat(gpa, arena, format_args),
        .test_cmd => |test_args| rocTest(gpa, test_args),
        .link => |link_args| rocLink(gpa, link_args),
        .repl => rocRepl(gpa),
        .version => stdout.print("Roc compiler version {s}\n", .{build_options.compiler_version}),
        .docs => |docs_args| rocDocs(gpa, docs_args),
        .help => |help_message| stdout.writeAll(help_message),
        .licenses => stdout.writeAll(legalDetailsFileContent),
        .problem => |problem| {
            try switch (problem) {
                .missing_flag_value => |details| stderr.print("Error: no value was supplied for {s}\n", .{details.flag}),
                .unexpected_argument => |details| stderr.print("Error: roc {s} received an unexpected argument: `{s}`\n", .{ details.cmd, details.arg }),
                .invalid_flag_value => |details| stderr.print("Error: `{s}` is not a valid value for {s}. The valid options are {s}\n", .{ details.value, details.flag, details.valid_options }),
            };
            std.process.exit(1);
        },
    };
}

fn rocRun(gpa: Allocator, args: cli_args.RunArgs) void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // Initialize cache
    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(gpa, cache_config, Filesystem.default());

    // Create cache directory for executables
    const cache_dir = cache_manager.config.getCacheEntriesDir(gpa) catch |err| {
        stderr.print("Failed to get cache directory: {}\n", .{err}) catch {};
        std.process.exit(1);
    };
    defer gpa.free(cache_dir);
    const exe_cache_dir = std.fs.path.join(gpa, &.{ cache_dir, "executables" }) catch |err| {
        stderr.print("Failed to create executable cache path: {}\n", .{err}) catch {};
        std.process.exit(1);
    };
    defer gpa.free(exe_cache_dir);

    std.fs.cwd().makePath(exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            stderr.print("Failed to create cache directory: {}\n", .{err}) catch {};
            std.process.exit(1);
        },
    };

    // Generate executable name based on the roc file path
    const exe_name = std.fmt.allocPrint(gpa, "roc_run_{}", .{std.hash.crc.Crc32.hash(args.path)}) catch |err| {
        stderr.print("Failed to generate executable name: {}\n", .{err}) catch {};
        std.process.exit(1);
    };
    defer gpa.free(exe_name);

    const exe_path = std.fs.path.join(gpa, &.{ exe_cache_dir, exe_name }) catch |err| {
        stderr.print("Failed to create executable path: {}\n", .{err}) catch {};
        std.process.exit(1);
    };
    defer gpa.free(exe_path);

    // Check if executable already exists in cache
    const exe_exists = blk: {
        std.fs.accessAbsolute(exe_path, .{}) catch {
            break :blk false;
        };
        break :blk true;
    };

    if (!exe_exists) {
        // Use pre-built host.a from the install directory
        const installed_host_lib = std.fs.cwd().realpathAlloc(gpa, "zig-out/lib/libhost.a") catch |err| {
            stderr.print("Failed to get absolute path for host library: {}\n", .{err}) catch {};
            std.process.exit(1);
        };
        defer gpa.free(installed_host_lib);

        const host_lib_path = std.fs.path.join(gpa, &.{ exe_cache_dir, "host.a" }) catch |err| {
            stderr.print("Failed to create host library path: {}\n", .{err}) catch {};
            std.process.exit(1);
        };
        defer gpa.free(host_lib_path);

        // Copy host.a to cache directory
        std.fs.copyFileAbsolute(installed_host_lib, host_lib_path, .{}) catch |err| {
            stderr.print("Failed to copy host library from {s} to {s}: {}\n", .{ installed_host_lib, host_lib_path, err }) catch {};
            std.process.exit(1);
        };

        // Extract embedded object file to cache
        const obj_file_path = std.fs.path.join(gpa, &.{ exe_cache_dir, "shared_memory.o" }) catch |err| {
            stderr.print("Failed to create object file path: {}\n", .{err}) catch {};
            std.process.exit(1);
        };
        defer gpa.free(obj_file_path);

        createSharedMemoryObject(gpa, obj_file_path) catch |err| {
            stderr.print("Failed to create shared memory object: {}\n", .{err}) catch {};
            std.process.exit(1);
        };

        // Link the host.a with our object file to create the executable
        const link_config = linker.LinkConfig{
            .output_path = exe_path,
            .object_files = &.{ host_lib_path, obj_file_path },
        };

        linker.link(gpa, link_config) catch |err| {
            stderr.print("Failed to link executable: {}\n", .{err}) catch {};
            std.process.exit(1);
        };
    }

    // Set up shared memory with the placeholder string
    const test_string = "Testing Roc shared memory communication!";
    const shm_handle = writeToSharedMemory(test_string) catch |err| {
        stderr.print("Failed to write to shared memory: {}\n", .{err}) catch {};
        std.process.exit(1);
    };
    // Ensure we clean up shared memory resources on all exit paths
    defer {
        _ = munmap(shm_handle.ptr, shm_handle.size);
        _ = c.close(shm_handle.fd);
        cleanupSharedMemory();
    }

    // Execute the created executable using spawn for better control
    var child = std.process.Child.init(&.{exe_path}, gpa);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch |err| {
        stderr.print("Failed to spawn {s}: {}\n", .{ exe_path, err }) catch {};
        cleanupSharedMemory();
        std.process.exit(1);
    };

    // Read output while child is running
    var stdout_buf = std.ArrayList(u8).init(gpa);
    defer stdout_buf.deinit();
    var stderr_buf = std.ArrayList(u8).init(gpa);
    defer stderr_buf.deinit();

    if (child.stdout) |child_stdout| {
        child_stdout.reader().readAllArrayList(&stdout_buf, 1024 * 1024) catch {};
    }
    if (child.stderr) |child_stderr| {
        child_stderr.reader().readAllArrayList(&stderr_buf, 1024 * 1024) catch {};
    }

    // Wait for child to complete
    _ = child.wait() catch |err| {
        stderr.print("Failed waiting for child process: {}\n", .{err}) catch {};
        std.process.exit(1);
    };

    // Print the output
    stdout.print("{s}", .{stdout_buf.items}) catch {};
    if (stderr_buf.items.len > 0) {
        stderr.print("{s}", .{stderr_buf.items}) catch {};
    }
}

const SharedMemoryHandle = struct {
    fd: c_int,
    ptr: *anyopaque,
    size: usize,
};

fn writeToSharedMemory(data: []const u8) !SharedMemoryHandle {
    const shm_name = "/ROC_FILE_TO_INTERPRET";

    // Calculate total size needed: length + data
    const total_size = @sizeOf(usize) + data.len;

    // Unlink any existing shared memory object first
    _ = shm_unlink(shm_name);

    // Create shared memory object
    const shm_fd = shm_open(shm_name, 0x0002 | 0x0200, 0o666); // O_RDWR | O_CREAT
    if (shm_fd < 0) {
        std.debug.print("Failed to create shared memory\n", .{});
        return error.SharedMemoryCreateFailed;
    }

    // Set the size of the shared memory object
    if (c.ftruncate(shm_fd, @intCast(total_size)) != 0) {
        _ = c.close(shm_fd);
        std.debug.print("Failed to set shared memory size\n", .{});
        return error.SharedMemoryTruncateFailed;
    }

    // Map the shared memory
    const mapped_ptr = mmap(
        null,
        total_size,
        0x01 | 0x02, // PROT_READ | PROT_WRITE
        0x0001, // MAP_SHARED
        shm_fd,
        0,
    ) orelse {
        _ = c.close(shm_fd);
        std.debug.print("Failed to map shared memory\n", .{});
        return error.SharedMemoryMapFailed;
    };
    const mapped_memory = @as([*]u8, @ptrCast(mapped_ptr))[0..total_size];

    // Write length at the beginning
    const length_ptr: *align(1) usize = @ptrCast(mapped_memory.ptr);
    length_ptr.* = data.len;

    // Write data after the length
    const data_ptr = mapped_memory.ptr + @sizeOf(usize);
    @memcpy(data_ptr[0..data.len], data);

    return SharedMemoryHandle{
        .fd = shm_fd,
        .ptr = mapped_ptr,
        .size = total_size,
    };
}

fn cleanupSharedMemory() void {
    const shm_name = "/ROC_FILE_TO_INTERPRET";
    if (shm_unlink(shm_name) != 0) {
        std.debug.print("Failed to unlink shared memory\n", .{});
    }
}

fn createSharedMemoryObject(gpa: Allocator, output_path: []const u8) !void {
    // Create temporary Zig source file
    const zig_source_path = try std.fmt.allocPrint(gpa, "{s}.zig", .{output_path});
    defer gpa.free(zig_source_path);

    const zig_file = try std.fs.cwd().createFile(zig_source_path, .{});
    defer zig_file.close();

    // Write the shared memory Zig source
    try zig_file.writeAll(shared_memory_source);

    // Compile to object file using zig build-obj with explicit output path
    const emit_bin_arg = try std.fmt.allocPrint(gpa, "-femit-bin={s}", .{output_path});
    defer gpa.free(emit_bin_arg);

    const compile_result = std.process.Child.run(.{
        .allocator = gpa,
        .argv = &.{ "zig", "build-obj", zig_source_path, "-fno-emit-asm", "-fno-emit-llvm-ir", emit_bin_arg, "-lc" },
    }) catch |err| {
        std.debug.print("Failed to compile object file: {}\n", .{err});
        return err;
    };

    if (compile_result.term.Exited != 0) {
        std.debug.print("Object compilation failed: {s}\n", .{compile_result.stderr});
        return error.CompilationFailed;
    }

    // Clean up temporary files
    std.fs.cwd().deleteFile(zig_source_path) catch {};
}

fn rocBuild(gpa: Allocator, args: cli_args.BuildArgs) !void {
    // Handle the --z-bench-tokenize flag
    if (args.z_bench_tokenize) |file_path| {
        try benchTokenizer(gpa, file_path);
        return;
    }

    // Handle the --z-bench-parse flag
    if (args.z_bench_parse) |directory_path| {
        try benchParse(gpa, directory_path);
        return;
    }

    fatal("build not implemented", .{});
}

fn rocTest(gpa: Allocator, args: cli_args.TestArgs) !void {
    _ = gpa;
    _ = args;
    fatal("test not implemented", .{});
}

fn rocLink(gpa: Allocator, args: cli_args.LinkArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var timer = try std.time.Timer.start();

    const config = linker.LinkConfig{
        .output_path = args.output,
        .object_files = args.object_files,
        .extra_args = args.extra_args,
    };

    linker.link(gpa, config) catch |err| {
        switch (err) {
            linker.LinkError.LinkFailed => {
                stderr.print("Error: Failed to link object files\n", .{}) catch {};
                std.process.exit(1);
            },
            linker.LinkError.OutOfMemory => {
                stderr.print("Error: Out of memory during linking\n", .{}) catch {};
                std.process.exit(1);
            },
            linker.LinkError.InvalidArguments => {
                stderr.print("Error: Invalid arguments provided to linker\n", .{}) catch {};
                std.process.exit(1);
            },
            linker.LinkError.LLVMNotAvailable => {
                stderr.print("Error: Linking requires LLVM support. Please rebuild with -Dllvm=true\n", .{}) catch {};
                std.process.exit(1);
            },
        }
    };

    const elapsed = timer.read();
    stdout.print("Successfully linked {d} object files to {s} in ", .{ args.object_files.len, args.output }) catch {};
    formatElapsedTime(stdout, elapsed) catch {};
    stdout.print("\n", .{}) catch {};
}

fn rocRepl(gpa: Allocator) !void {
    _ = gpa;
    fatal("repl not implemented", .{});
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(gpa: Allocator, arena: Allocator, args: cli_args.FormatArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut();
    if (args.stdin) {
        fmt.formatStdin(gpa) catch std.process.exit(1);
        return;
    }

    var timer = try std.time.Timer.start();
    var elapsed: u64 = undefined;
    var failure_count: usize = 0;
    var exit_code: u8 = 0;

    if (args.check) {
        var unformatted_files = std.ArrayList([]const u8).init(gpa);
        defer unformatted_files.deinit();

        for (args.paths) |path| {
            var result = try fmt.formatPath(gpa, arena, std.fs.cwd(), path, true);
            defer result.deinit();
            if (result.unformatted_files) |files| {
                try unformatted_files.appendSlice(files.items);
            }
            failure_count += result.failure;
        }

        elapsed = timer.read();
        if (unformatted_files.items.len > 0) {
            try stdout.writer().print("The following file(s) failed `roc format --check`:\n", .{});
            for (unformatted_files.items) |file_name| {
                try stdout.writer().print("    {s}\n", .{file_name});
            }
            try stdout.writer().print("You can fix this with `roc format FILENAME.roc`.\n", .{});
            exit_code = 1;
        } else {
            try stdout.writer().print("All formatting valid\n", .{});
        }
        if (failure_count > 0) {
            try stdout.writer().print("Failed to check {} files.\n", .{failure_count});
            exit_code = 1;
        }
    } else {
        var success_count: usize = 0;
        for (args.paths) |path| {
            const result = try fmt.formatPath(gpa, arena, std.fs.cwd(), path, false);
            success_count += result.success;
            failure_count += result.failure;
        }
        elapsed = timer.read();
        try stdout.writer().print("Successfully formatted {} files\n", .{success_count});
        if (failure_count > 0) {
            try stdout.writer().print("Failed to format {} files.\n", .{failure_count});
            exit_code = 1;
        }
    }

    try stdout.writer().print("Took ", .{});
    try formatElapsedTime(stdout.writer(), elapsed);
    try stdout.writer().print(".\n", .{});

    std.process.exit(exit_code);
}

/// Helper function to format elapsed time, showing decimal milliseconds
fn formatElapsedTime(writer: anytype, elapsed_ns: u64) !void {
    const elapsed_ms_float = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    try writer.print("{d:.1} ms", .{elapsed_ms_float});
}

fn handleProcessFileError(err: anytype, stderr: anytype, path: []const u8) noreturn {
    stderr.print("Failed to check {s}: ", .{path}) catch {};
    switch (err) {
        error.FileNotFound => stderr.print("File not found\n", .{}) catch {},
        error.AccessDenied => stderr.print("Access denied\n", .{}) catch {},
        error.FileReadError => stderr.print("Could not read file\n", .{}) catch {},
        else => stderr.print("{}\n", .{err}) catch {},
    }
    std.process.exit(1);
}

fn rocCheck(gpa: Allocator, args: cli_args.CheckArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const stderr_writer = stderr.any();

    var timer = try std.time.Timer.start();

    // Initialize cache if enabled
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    var cache_manager = if (cache_config.enabled) blk: {
        const manager = CacheManager.init(gpa, cache_config, Filesystem.default());
        break :blk manager;
    } else null;

    // Process the file and get Reports
    var process_result = coordinate_simple.processFile(gpa, Filesystem.default(), args.path, if (cache_manager) |*cm| cm else null, args.time) catch |err| handleProcessFileError(err, stderr, args.path);

    defer process_result.deinit(gpa);

    const elapsed = timer.read();

    // Print cache statistics if verbose
    if (cache_manager) |*cm| {
        if (args.verbose) {
            cm.printStats(gpa);
        }
    }

    // Handle cached results vs fresh compilation results differently
    if (process_result.was_cached) {
        // For cached results, use the stored diagnostic counts
        const total_errors = process_result.error_count;
        const total_warnings = process_result.warning_count;

        if (total_errors > 0 or total_warnings > 0) {
            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                total_errors,
                total_warnings,
            }) catch {};
            formatElapsedTime(stderr, elapsed) catch {};
            stderr.print(" for {s} (note module loaded from cache, use --no-cache to display Errors and Warnings.).\n", .{args.path}) catch {};
            std.process.exit(1);
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s} (loaded from cache)\n", .{args.path}) catch {};
        }
    } else {
        // For fresh compilation, process and display reports normally
        if (process_result.reports.len > 0) {
            var fatal_errors: usize = 0;
            var runtime_errors: usize = 0;
            var warnings: usize = 0;

            // Render each report
            for (process_result.reports) |*report| {

                // Render the diagnostic report to stderr
                reporting.renderReportToTerminal(report, stderr_writer, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
                    stderr.print("Error rendering diagnostic report: {}\n", .{render_err}) catch {};
                    // Fallback to just printing the title
                    stderr.print("  {s}\n", .{report.title}) catch {};
                };

                switch (report.severity) {
                    .info => {}, // Informational messages don't affect error/warning counts
                    .runtime_error => {
                        runtime_errors += 1;
                    },
                    .fatal => {
                        fatal_errors += 1;
                    },
                    .warning => {
                        warnings += 1;
                    },
                }
            }
            stderr.writeAll("\n") catch {};

            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                (fatal_errors + runtime_errors),
                warnings,
            }) catch {};
            formatElapsedTime(stderr, elapsed) catch {};
            stderr.print(" for {s}.\n", .{args.path}) catch {};

            std.process.exit(1);
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s}\n", .{args.path}) catch {};
        }
    }

    printTimingBreakdown(stderr, process_result.timing);
}

fn printTimingBreakdown(writer: anytype, timing: ?coordinate_simple.TimingInfo) void {
    if (timing) |t| {
        writer.print("\nTiming breakdown:\n", .{}) catch {};
        writer.print("  tokenize + parse:             ", .{}) catch {};
        formatElapsedTime(writer, t.tokenize_parse_ns) catch {};
        writer.print("  ({} ns)\n", .{t.tokenize_parse_ns}) catch {};
        writer.print("  canonicalize:                 ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_ns) catch {};
        writer.print("  ({} ns)\n", .{t.canonicalize_ns}) catch {};
        writer.print("  can diagnostics:              ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_diagnostics_ns) catch {};
        writer.print("  ({} ns)\n", .{t.canonicalize_diagnostics_ns}) catch {};
        writer.print("  type checking:                ", .{}) catch {};
        formatElapsedTime(writer, t.type_checking_ns) catch {};
        writer.print("  ({} ns)\n", .{t.type_checking_ns}) catch {};
        writer.print("  type checking diagnostics:    ", .{}) catch {};
        formatElapsedTime(writer, t.check_diagnostics_ns) catch {};
        writer.print("  ({} ns)\n", .{t.check_diagnostics_ns}) catch {};
    }
}

fn rocDocs(gpa: Allocator, args: cli_args.DocsArgs) !void {
    _ = gpa;
    _ = args;
    fatal("docs not implemented", .{});
}

/// Log a fatal error and exit the process with a non-zero code.
pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.io.getStdErr().writer().print(format, args) catch unreachable;
    if (tracy.enable) {
        tracy.waitForShutdown() catch unreachable;
    }
    std.process.exit(1);
}
