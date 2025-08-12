//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dllvm -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const reporting = @import("reporting");
const parse = @import("parse");
const tracy = @import("tracy");

const SharedMemoryAllocator = @import("./SharedMemoryAllocator.zig");
const platform = @import("ipc/platform.zig");
const fmt = @import("fmt.zig");
const coordinate_simple = @import("coordinate_simple.zig");
const Filesystem = @import("fs/Filesystem.zig");
const cli_args = @import("cli_args.zig");
const cache_mod = @import("cache/mod.zig");
const bench = @import("bench.zig");
const linker = @import("linker.zig");
const compile = @import("compile");
const can = @import("can");
const check = @import("check");
const bundle = @import("bundle/bundle.zig");

const ModuleEnv = compile.ModuleEnv;

const CacheManager = cache_mod.CacheManager;
const CacheConfig = cache_mod.CacheConfig;
const tokenize = parse.tokenize;

const roc_shim_lib = if (builtin.is_test) &[_]u8{} else if (builtin.target.os.tag == .windows) @embedFile("roc_shim.lib") else @embedFile("libroc_shim.a");

// Workaround for Zig standard library compilation issue on macOS ARM64.
//
// The Problem:
// When importing std.c directly, Zig attempts to compile ALL C function declarations,
// including mremap which has this signature in std/c.zig:9562:
//   pub extern "c" fn mremap(addr: ?*align(page_size) const anyopaque, old_len: usize,
//                            new_len: usize, flags: MREMAP, ...) *anyopaque;
//
// The variadic arguments (...) at the end trigger this compiler error on macOS ARM64:
//   "parameter of type 'void' not allowed in function with calling convention 'aarch64_aapcs_darwin'"
//
// This is because:
// 1. mremap is a Linux-specific syscall that doesn't exist on macOS
// 2. The variadic declaration is incompatible with ARM64 macOS calling conventions
// 3. Even though we never call mremap, just importing std.c triggers its compilation
//
// Related issues:
// - https://github.com/ziglang/zig/issues/6321 - Discussion about mremap platform support
// - mremap is only available on Linux/FreeBSD, not macOS/Darwin
//
// Solution:
// Instead of importing all of std.c, we create a minimal wrapper that only exposes
// the specific types and functions we actually need. This avoids triggering the
// compilation of the broken mremap declaration.
//
// TODO: This workaround can be removed once the upstream Zig issue is fixed.
/// Minimal wrapper around std.c types and functions to avoid mremap compilation issues.
/// Contains only the C types and functions we actually need.
pub const c = struct {
    pub const mode_t = std.c.mode_t;
    pub const off_t = std.c.off_t;

    pub const close = std.c.close;
    pub const link = std.c.link;
    pub const ftruncate = std.c.ftruncate;
    pub const _errno = std.c._errno;
};

// Platform-specific shared memory implementation
const is_windows = builtin.target.os.tag == .windows;

// POSIX shared memory functions
const posix = if (!is_windows) struct {
    extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
    extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: std.c.off_t) ?*anyopaque;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
    extern "c" fn fcntl(fd: c_int, cmd: c_int, arg: c_int) c_int;

    // fcntl constants
    const F_GETFD = 1;
    const F_SETFD = 2;
    const FD_CLOEXEC = 1;
} else struct {};

// Windows shared memory functions
const windows = if (is_windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPVOID = ?*anyopaque;
    const LPCWSTR = [*:0]const u16;
    const SIZE_T = usize;
    const STARTUPINFOW = extern struct {
        cb: DWORD,
        lpReserved: ?LPCWSTR,
        lpDesktop: ?LPCWSTR,
        lpTitle: ?LPCWSTR,
        dwX: DWORD,
        dwY: DWORD,
        dwXSize: DWORD,
        dwYSize: DWORD,
        dwXCountChars: DWORD,
        dwYCountChars: DWORD,
        dwFillAttribute: DWORD,
        dwFlags: DWORD,
        wShowWindow: u16,
        cbReserved2: u16,
        lpReserved2: ?*u8,
        hStdInput: ?HANDLE,
        hStdOutput: ?HANDLE,
        hStdError: ?HANDLE,
    };
    const PROCESS_INFORMATION = extern struct {
        hProcess: HANDLE,
        hThread: HANDLE,
        dwProcessId: DWORD,
        dwThreadId: DWORD,
    };

    extern "kernel32" fn SetHandleInformation(hObject: HANDLE, dwMask: DWORD, dwFlags: DWORD) BOOL;
    extern "kernel32" fn CreateProcessW(
        lpApplicationName: ?LPCWSTR,
        lpCommandLine: ?[*:0]u16,
        lpProcessAttributes: ?*anyopaque,
        lpThreadAttributes: ?*anyopaque,
        bInheritHandles: BOOL,
        dwCreationFlags: DWORD,
        lpEnvironment: ?*anyopaque,
        lpCurrentDirectory: ?LPCWSTR,
        lpStartupInfo: *STARTUPINFOW,
        lpProcessInformation: *PROCESS_INFORMATION,
    ) BOOL;
    extern "kernel32" fn WaitForSingleObject(hHandle: HANDLE, dwMilliseconds: DWORD) DWORD;

    const HANDLE_FLAG_INHERIT = 0x00000001;
    const INFINITE = 0xFFFFFFFF;
} else struct {};

const benchTokenizer = bench.benchTokenizer;
const benchParse = bench.benchParse;

const Allocator = std.mem.Allocator;
const ColorPalette = reporting.ColorPalette;

const legalDetailsFileContent = @embedFile("legal_details");

/// Size for shared memory allocator (just virtual address space to reserve)
///
/// We pick a large number because we can't resize this without messing up the
/// child process. It's just virtual address space though, not physical memory.
/// On 32-bit targets, we use 512MB since 2TB won't fit in the address space.
const SHARED_MEMORY_SIZE: usize = if (@sizeOf(usize) >= 8)
    512 * 1024 * 1024 // 512MB for 64-bit targets (reduced from 2TB for Windows compatibility)
else
    256 * 1024 * 1024; // 256MB for 32-bit targets

/// Cross-platform hardlink creation
fn createHardlink(allocator: Allocator, source: []const u8, dest: []const u8) !void {
    if (comptime builtin.target.os.tag == .windows) {
        // On Windows, use CreateHardLinkW
        const source_w = try std.unicode.utf8ToUtf16LeAllocZ(allocator, source);
        defer allocator.free(source_w);
        const dest_w = try std.unicode.utf8ToUtf16LeAllocZ(allocator, dest);
        defer allocator.free(dest_w);

        // Declare CreateHardLinkW since it's not in all versions of std
        const kernel32 = struct {
            extern "kernel32" fn CreateHardLinkW(
                lpFileName: [*:0]const u16,
                lpExistingFileName: [*:0]const u16,
                lpSecurityAttributes: ?*anyopaque,
            ) callconv(std.os.windows.WINAPI) std.os.windows.BOOL;
        };

        if (kernel32.CreateHardLinkW(dest_w, source_w, null) == 0) {
            const err = std.os.windows.kernel32.GetLastError();
            switch (err) {
                .ALREADY_EXISTS => return error.PathAlreadyExists,
                else => return error.Unexpected,
            }
        }
    } else {
        // On POSIX systems, use the link system call
        const source_c = try allocator.dupeZ(u8, source);
        defer allocator.free(source_c);
        const dest_c = try allocator.dupeZ(u8, dest);
        defer allocator.free(dest_c);

        const result = c.link(source_c, dest_c);
        if (result != 0) {
            const errno = c._errno().*;
            switch (errno) {
                17 => return error.PathAlreadyExists, // EEXIST
                else => return error.Unexpected,
            }
        }
    }
}

/// Generate a cryptographically secure random ASCII string for directory names
fn generateRandomSuffix(allocator: Allocator) ![]u8 {
    // TODO: Consider switching to a library like https://github.com/abhinav/temp.zig
    // for more robust temporary file/directory handling
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    const suffix = try allocator.alloc(u8, 32);

    // Fill with cryptographically secure random bytes
    std.crypto.random.bytes(suffix);

    // Convert to ASCII characters from our charset
    for (suffix) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }

    return suffix;
}

/// Create the temporary directory structure for fd communication.
/// Returns the path to the executable in the temp directory (caller must free).
/// If a cache directory is provided, it will be used for temporary files; otherwise
/// falls back to the system temp directory.
pub fn createTempDirStructure(allocator: Allocator, exe_path: []const u8, shm_handle: SharedMemoryHandle, cache_dir: ?[]const u8) ![]const u8 {
    // Use provided cache dir or fall back to system temp directory
    const temp_dir = if (cache_dir) |dir|
        try allocator.dupe(u8, dir)
    else if (comptime is_windows)
        std.process.getEnvVarOwned(allocator, "TEMP") catch
            std.process.getEnvVarOwned(allocator, "TMP") catch try allocator.dupe(u8, "C:\\Windows\\Temp")
    else
        std.process.getEnvVarOwned(allocator, "TMPDIR") catch try allocator.dupe(u8, "/tmp");
    defer allocator.free(temp_dir);

    // Try up to 10 times to create a unique directory
    var attempt: u8 = 0;
    while (attempt < 10) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(allocator);
        errdefer allocator.free(random_suffix);

        // Create the full path with .txt suffix first
        const normalized_temp_dir = if (comptime is_windows)
            std.mem.trimRight(u8, temp_dir, "/\\")
        else
            std.mem.trimRight(u8, temp_dir, "/");
        const dir_name_with_txt = if (comptime is_windows)
            try std.fmt.allocPrint(allocator, "{s}\\roc-tmp-{s}.txt", .{ normalized_temp_dir, random_suffix })
        else
            try std.fmt.allocPrint(allocator, "{s}/roc-tmp-{s}.txt", .{ normalized_temp_dir, random_suffix });
        errdefer allocator.free(dir_name_with_txt);

        // Get the directory path by slicing off the .txt suffix
        const dir_path_len = dir_name_with_txt.len - 4; // Remove ".txt"
        const temp_dir_path = dir_name_with_txt[0..dir_path_len];

        // Try to create the directory
        std.fs.cwd().makeDir(temp_dir_path) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // Directory already exists, try again with a new random suffix
                allocator.free(random_suffix);
                allocator.free(dir_name_with_txt);
                continue;
            },
            else => {
                allocator.free(random_suffix);
                allocator.free(dir_name_with_txt);
                return err;
            },
        };

        // Try to create the fd file
        const fd_file = std.fs.cwd().createFile(dir_name_with_txt, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // File already exists, remove the directory and try again
                std.fs.cwd().deleteDir(temp_dir_path) catch {};
                allocator.free(random_suffix);
                allocator.free(dir_name_with_txt);
                continue;
            },
            else => {
                // Clean up directory on other errors
                std.fs.cwd().deleteDir(temp_dir_path) catch {};
                allocator.free(random_suffix);
                allocator.free(dir_name_with_txt);
                return err;
            },
        };
        // Note: We'll close this explicitly later, before spawning the child

        // Write shared memory info to file (POSIX only - Windows uses command line args)
        const fd_str = try std.fmt.allocPrint(allocator, "{}\n{}", .{ shm_handle.fd, shm_handle.size });
        defer allocator.free(fd_str);

        try fd_file.writeAll(fd_str);

        // IMPORTANT: Flush and close the file explicitly before spawning child process
        // On Windows, having the file open can prevent child process access
        try fd_file.sync(); // Ensure data is written to disk
        fd_file.close();

        // Create hardlink to executable in temp directory
        const exe_basename = std.fs.path.basename(exe_path);
        const temp_exe_path = try std.fs.path.join(allocator, &.{ temp_dir_path, exe_basename });
        defer allocator.free(temp_exe_path);

        // Try to create a hardlink first (more efficient than copying)
        createHardlink(allocator, exe_path, temp_exe_path) catch {
            // If hardlinking fails for any reason, fall back to copying
            // Common reasons: cross-device link, permissions, file already exists
            try std.fs.cwd().copyFile(exe_path, std.fs.cwd(), temp_exe_path, .{});
        };

        // Allocate and return just the executable path
        const final_exe_path = try allocator.dupe(u8, temp_exe_path);

        // Free all temporary allocations
        allocator.free(dir_name_with_txt);
        allocator.free(random_suffix);

        return final_exe_path;
    }

    // Failed after 10 attempts
    return error.FailedToCreateUniqueTempDir;
}

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
        .bundle => |bundle_args| rocBundle(gpa, bundle_args),
        .unbundle => |unbundle_args| rocUnbundle(gpa, unbundle_args),
        .format => |format_args| rocFormat(gpa, arena, format_args),
        .test_cmd => |test_args| rocTest(gpa, test_args),
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

    // Initialize cache - used to store our shim, and linked interpreter executables in cache
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(gpa, cache_config, Filesystem.default());

    // Create cache directory for linked interpreter executables
    const cache_dir = cache_manager.config.getCacheEntriesDir(gpa) catch |err| {
        std.log.err("Failed to get cache directory: {}\n", .{err});
        std.process.exit(1);
    };
    defer gpa.free(cache_dir);
    const exe_cache_dir = std.fs.path.join(gpa, &.{ cache_dir, "executables" }) catch |err| {
        std.log.err("Failed to create executable cache path: {}\n", .{err});
        std.process.exit(1);
    };
    defer gpa.free(exe_cache_dir);

    std.fs.cwd().makePath(exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.log.err("Failed to create cache directory: {}\n", .{err});
            std.process.exit(1);
        },
    };

    // Generate executable name based on the roc file path
    // TODO use something more interesting like a hash from the platform.main or platform/host.a etc
    const exe_name = std.fmt.allocPrint(gpa, "roc_run_{}", .{std.hash.crc.Crc32.hash(args.path)}) catch |err| {
        std.log.err("Failed to generate executable name: {}\n", .{err});
        std.process.exit(1);
    };
    defer gpa.free(exe_name);

    const exe_path = std.fs.path.join(gpa, &.{ exe_cache_dir, exe_name }) catch |err| {
        std.log.err("Failed to create executable path: {}\n", .{err});
        std.process.exit(1);
    };
    defer gpa.free(exe_path);

    // Check if the interpreter executable already exists (cached)
    const exe_exists = if (args.no_cache) false else blk: {
        std.fs.accessAbsolute(exe_path, .{}) catch {
            break :blk false;
        };
        break :blk true;
    };

    if (!exe_exists) {

        // Resolve platform from app header
        const host_path = resolvePlatformHost(gpa, args.path) catch |err| {
            std.log.err("Failed to resolve platform: {}\n", .{err});
            std.process.exit(1);
        };
        defer gpa.free(host_path);

        // Check for cached shim library, extract if not present
        const shim_filename = if (builtin.target.os.tag == .windows) "roc_shim.lib" else "libroc_shim.a";
        const shim_path = std.fs.path.join(gpa, &.{ exe_cache_dir, shim_filename }) catch |err| {
            std.log.err("Failed to create shim library path: {}\n", .{err});
            std.process.exit(1);
        };
        defer gpa.free(shim_path);

        // Only extract if the shim doesn't already exist in cache
        std.fs.cwd().access(shim_path, .{}) catch {
            // Shim not found in cache, extract it
            extractReadRocFilePathShimLibrary(gpa, shim_path) catch |err| {
                std.log.err("Failed to extract read roc file path shim library: {}\n", .{err});
                std.process.exit(1);
            };
        };

        // Link the host.a with our shim to create the interpreter executable using our linker
        // Try LLD first, fallback to clang if LLVM is not available
        var extra_args = std.ArrayList([]const u8).init(gpa);
        defer extra_args.deinit();

        // Add system libraries for macOS
        if (builtin.target.os.tag == .macos) {
            extra_args.append("-lSystem") catch {
                std.log.err("Failed to allocate memory for linker args\n", .{});
                std.process.exit(1);
            };
        }

        const link_config = linker.LinkConfig{
            .output_path = exe_path,
            .object_files = &.{ host_path, shim_path },
            .extra_args = extra_args.items,
            .can_exit_early = false,
            .disable_output = false,
        };

        linker.link(gpa, link_config) catch |err| switch (err) {
            linker.LinkError.LLVMNotAvailable => {
                // Fallback to clang when LLVM is not available
                const link_result = std.process.Child.run(.{
                    .allocator = gpa,
                    .argv = &.{ "clang", "-o", exe_path, host_path, shim_path },
                }) catch |clang_err| {
                    std.log.err("Failed to link executable with both LLD and clang: LLD unavailable, clang error: {}\n", .{clang_err});
                    std.process.exit(1);
                };
                defer gpa.free(link_result.stdout);
                defer gpa.free(link_result.stderr);
                if (link_result.term.Exited != 0) {
                    std.log.err("Linker failed with exit code: {}\n", .{link_result.term.Exited});
                    if (link_result.stderr.len > 0) {
                        std.log.err("Linker stderr: {s}\n", .{link_result.stderr});
                    }
                    if (link_result.stdout.len > 0) {
                        std.log.err("Linker stdout: {s}\n", .{link_result.stdout});
                    }
                    std.process.exit(1);
                }
            },
            linker.LinkError.LinkFailed => {
                std.log.err("LLD linker failed to create executable\n", .{});
                std.process.exit(1);
            },
            else => {
                std.log.err("Failed to link executable: {}\n", .{err});
                std.process.exit(1);
            },
        };
    }

    // Set up shared memory with ModuleEnv
    const shm_handle = setupSharedMemoryWithModuleEnv(gpa, args.path) catch |err| {
        std.log.err("Failed to set up shared memory with ModuleEnv: {}\n", .{err});
        std.process.exit(1);
    };

    // Ensure we clean up shared memory resources on all exit paths
    defer {
        if (comptime is_windows) {
            _ = platform.windows.UnmapViewOfFile(shm_handle.ptr);
            _ = platform.windows.CloseHandle(@ptrCast(shm_handle.fd));
        } else {
            _ = posix.munmap(shm_handle.ptr, shm_handle.size);
            _ = c.close(shm_handle.fd);
        }
    }

    if (comptime is_windows) {
        // Windows: Use handle inheritance approach
        runWithWindowsHandleInheritance(gpa, exe_path, shm_handle) catch |err| {
            std.log.err("Failed to run with Windows handle inheritance: {}\n", .{err});
            std.process.exit(1);
        };
    } else {
        // POSIX: Use existing file descriptor inheritance approach
        runWithPosixFdInheritance(gpa, exe_path, shm_handle, &cache_manager) catch |err| {
            std.log.err("Failed to run with POSIX fd inheritance: {}\n", .{err});
            std.process.exit(1);
        };
    }
}

/// Run child process using Windows handle inheritance (idiomatic Windows approach)
fn runWithWindowsHandleInheritance(gpa: Allocator, exe_path: []const u8, shm_handle: SharedMemoryHandle) !void {
    // Make the shared memory handle inheritable
    if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
        std.log.err("Failed to set handle as inheritable\n", .{});
        return error.HandleInheritanceFailed;
    }

    // Convert paths to Windows wide strings
    const exe_path_w = try std.unicode.utf8ToUtf16LeAllocZ(gpa, exe_path);
    defer gpa.free(exe_path_w);

    const cwd = try std.fs.cwd().realpathAlloc(gpa, ".");
    defer gpa.free(cwd);
    const cwd_w = try std.unicode.utf8ToUtf16LeAllocZ(gpa, cwd);
    defer gpa.free(cwd_w);

    // Create command line with handle and size as arguments
    const handle_uint = @intFromPtr(shm_handle.fd);
    const cmd_line = try std.fmt.allocPrintZ(gpa, "\"{s}\" {} {}", .{ exe_path, handle_uint, shm_handle.size });
    defer gpa.free(cmd_line);
    const cmd_line_w = try std.unicode.utf8ToUtf16LeAllocZ(gpa, cmd_line);
    defer gpa.free(cmd_line_w);

    // Set up process creation structures
    var startup_info = std.mem.zeroes(windows.STARTUPINFOW);
    startup_info.cb = @sizeOf(windows.STARTUPINFOW);

    var process_info = std.mem.zeroes(windows.PROCESS_INFORMATION);

    // Create the child process with handle inheritance

    // Create the child process with handle inheritance enabled
    const success = windows.CreateProcessW(
        exe_path_w.ptr, // Application name
        cmd_line_w.ptr, // Command line (mutable)
        null, // Process attributes
        null, // Thread attributes
        1, // bInheritHandles = TRUE
        0, // Creation flags
        null, // Environment
        cwd_w.ptr, // Current directory
        &startup_info, // Startup info
        &process_info, // Process info
    );

    if (success == 0) {
        std.log.err("CreateProcessW failed\n", .{});
        return error.ProcessCreationFailed;
    }

    // Child process spawned successfully

    // Wait for the child process to complete
    const wait_result = windows.WaitForSingleObject(process_info.hProcess, windows.INFINITE);
    if (wait_result != 0) { // WAIT_OBJECT_0 = 0
        std.log.err("WaitForSingleObject failed or timed out\n", .{});
    }

    // Clean up process handles
    _ = platform.windows.CloseHandle(process_info.hProcess);
    _ = platform.windows.CloseHandle(process_info.hThread);
}

/// Run child process using POSIX file descriptor inheritance (existing approach for Unix)
fn runWithPosixFdInheritance(gpa: Allocator, exe_path: []const u8, shm_handle: SharedMemoryHandle, cache_manager: *CacheManager) !void {
    // Get cache directory for temporary files
    const temp_cache_dir = cache_manager.config.getTempDir(gpa) catch |err| {
        std.log.err("Failed to get temp cache directory: {}\n", .{err});
        return err;
    };
    defer gpa.free(temp_cache_dir);

    // Ensure temp cache directory exists
    std.fs.cwd().makePath(temp_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.log.err("Failed to create temp cache directory: {}\n", .{err});
            return err;
        },
    };

    // Create temporary directory structure for fd communication
    const temp_exe_path = createTempDirStructure(gpa, exe_path, shm_handle, temp_cache_dir) catch |err| {
        std.log.err("Failed to create temp dir structure: {}\n", .{err});
        return err;
    };
    defer gpa.free(temp_exe_path);

    // Configure fd inheritance
    var flags = posix.fcntl(shm_handle.fd, posix.F_GETFD, 0);
    if (flags < 0) {
        std.log.err("Failed to get fd flags: {}\n", .{c._errno().*});
        return error.FdConfigFailed;
    }

    flags &= ~@as(c_int, posix.FD_CLOEXEC);

    if (posix.fcntl(shm_handle.fd, posix.F_SETFD, flags) < 0) {
        std.log.err("Failed to set fd flags: {}\n", .{c._errno().*});
        return error.FdConfigFailed;
    }

    // Run the interpreter as a child process from the temp directory
    var child = std.process.Child.init(&.{temp_exe_path}, gpa);
    child.cwd = std.fs.cwd().realpathAlloc(gpa, ".") catch |err| {
        std.log.err("Failed to get current directory: {}\n", .{err});
        return err;
    };
    defer gpa.free(child.cwd.?);

    // Forward stdout and stderr
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    // Spawn the child process
    child.spawn() catch |err| {
        std.log.err("Failed to spawn {s}: {}\n", .{ exe_path, err });
        return err;
    };
    // Child process spawned successfully

    // Wait for child to complete
    _ = child.wait() catch |err| {
        std.log.err("Failed waiting for child process: {}\n", .{err});
        return err;
    };
}

/// Handle for cross-platform shared memory operations.
/// Contains the file descriptor/handle, memory pointer, and size.
pub const SharedMemoryHandle = struct {
    fd: if (is_windows) *anyopaque else c_int,
    ptr: *anyopaque,
    size: usize,
};

/// Write data to shared memory for inter-process communication.
/// Creates a shared memory region and writes the data with a length prefix.
/// Returns a handle that can be used to access the shared memory.
pub fn writeToSharedMemory(data: []const u8) !SharedMemoryHandle {
    // Calculate total size needed: length + data
    const total_size = @sizeOf(usize) + data.len;

    if (comptime is_windows) {
        return writeToWindowsSharedMemory(data, total_size);
    } else {
        return writeToPosixSharedMemory(data, total_size);
    }
}

fn writeToWindowsSharedMemory(data: []const u8, total_size: usize) !SharedMemoryHandle {
    // Create anonymous shared memory object (no name for handle inheritance)
    const shm_handle = platform.windows.CreateFileMappingW(
        platform.windows.INVALID_HANDLE_VALUE,
        null,
        platform.windows.PAGE_READWRITE,
        0,
        @intCast(total_size),
        null, // Anonymous - no name needed for handle inheritance
    ) orelse {
        std.log.err("Failed to create shared memory mapping\n", .{});
        return error.SharedMemoryCreateFailed;
    };

    // Map the shared memory at a fixed address to avoid ASLR issues
    const mapped_ptr = platform.windows.MapViewOfFileEx(
        shm_handle,
        platform.windows.FILE_MAP_ALL_ACCESS,
        0,
        0,
        0,
        platform.SHARED_MEMORY_BASE_ADDR,
    ) orelse {
        _ = platform.windows.CloseHandle(shm_handle);
        return error.SharedMemoryMapFailed;
    };

    // Write length and data
    const length_ptr: *usize = @ptrCast(@alignCast(mapped_ptr));
    length_ptr.* = data.len;

    const data_ptr = @as([*]u8, @ptrCast(mapped_ptr)) + @sizeOf(usize);
    @memcpy(data_ptr[0..data.len], data);

    return SharedMemoryHandle{
        .fd = shm_handle,
        .ptr = mapped_ptr,
        .size = total_size,
    };
}

/// Set up shared memory with a compiled ModuleEnv from a Roc file.
/// This parses, canonicalizes, and type-checks the Roc file, with the resulting ModuleEnv
/// ending up in shared memory because all allocations were done into shared memory.
pub fn setupSharedMemoryWithModuleEnv(_: std.mem.Allocator, roc_file_path: []const u8) !SharedMemoryHandle {
    // Create shared memory with SharedMemoryAllocator
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(SHARED_MEMORY_SIZE, page_size);
    // Don't defer deinit here - we need to keep the shared memory alive

    const shm_allocator = shm.allocator();

    // Allocate space for the offset value at the beginning
    const offset_ptr = try shm_allocator.alloc(u64, 1);
    // Also store the canonicalized expression index for the child to evaluate
    const expr_idx_ptr = try shm_allocator.alloc(u32, 1);

    // Store the base address of the shared memory mapping (for ASLR-safe relocation)
    // The child will calculate the offset from its own base address
    const shm_base_addr = @intFromPtr(shm.base_ptr);
    offset_ptr[0] = shm_base_addr;

    // Allocate and store a pointer to the ModuleEnv
    const env_ptr = try shm_allocator.create(ModuleEnv);

    // Read the actual Roc file
    const roc_file = std.fs.cwd().openFile(roc_file_path, .{}) catch |err| {
        std.log.err("Failed to open Roc file '{s}': {}\n", .{ roc_file_path, err });
        return error.FileNotFound;
    };
    defer roc_file.close();

    // Read the entire file into shared memory
    const file_size = try roc_file.getEndPos();
    const source = try shm_allocator.alloc(u8, @intCast(file_size));
    _ = try roc_file.read(source);

    // Extract module name from the file path
    const basename = std.fs.path.basename(roc_file_path);
    const module_name = try shm_allocator.dupe(u8, basename);

    var env = try ModuleEnv.init(shm_allocator, source);
    env.source = source;
    env.module_name = module_name;
    try env.calcLineStarts();

    // Parse the source code as a full module
    var parse_ast = try parse.parse(&env);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(shm_allocator, module_name);

    // Create canonicalizer
    var canonicalizer = try can.init(&env, &parse_ast, null);

    // Canonicalize the entire module
    try canonicalizer.canonicalizeFile();

    // Find the "main" definition in the module
    // Look through all definitions to find one named "main"
    var main_expr_idx: ?u32 = null;
    const defs = env.store.sliceDefs(env.all_defs);
    for (defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const pattern = env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_idx = pattern.assign.ident;
            const ident_text = env.idents.getText(ident_idx);
            if (std.mem.eql(u8, ident_text, "main")) {
                main_expr_idx = @intFromEnum(def.expr);
                break;
            }
        }
    }

    // Store the main expression index for the child
    expr_idx_ptr[0] = main_expr_idx orelse {
        std.log.err("No 'main' definition found in module\n", .{});
        return error.NoMainFunction;
    };

    // Type check the module
    var checker = try check.init(shm_allocator, &env.types, &env, &.{}, &env.store.regions);
    try checker.checkDefs();

    // Copy the ModuleEnv to the allocated space
    env_ptr.* = env;

    // Clean up the canonicalizer (but keep parse_ast data since it's in shared memory)
    canonicalizer.deinit();

    // Don't deinit parse_ast since its data is in shared memory
    // Don't deinit checker since its data is in shared memory

    // Update the header with used size
    shm.updateHeader();

    // Return the shared memory handle from SharedMemoryAllocator
    // This ensures we use the SAME shared memory region for both processes
    return SharedMemoryHandle{
        .fd = shm.handle,
        .ptr = shm.base_ptr,
        .size = shm.getUsedSize(),
    };
}

fn writeToPosixSharedMemory(data: []const u8, total_size: usize) !SharedMemoryHandle {
    const shm_name = "/ROC_FILE_TO_INTERPRET";

    // Unlink any existing shared memory object first
    _ = posix.shm_unlink(shm_name);

    // Create shared memory object
    const shm_fd = posix.shm_open(shm_name, 0x0002 | 0x0200, 0o666); // O_RDWR | O_CREAT
    if (shm_fd < 0) {
        return error.SharedMemoryCreateFailed;
    }

    // Set the size of the shared memory object
    if (c.ftruncate(shm_fd, @intCast(total_size)) != 0) {
        _ = c.close(shm_fd);
        return error.SharedMemoryTruncateFailed;
    }

    // Map the shared memory
    const mapped_ptr = posix.mmap(
        null,
        total_size,
        0x01 | 0x02, // PROT_READ | PROT_WRITE
        0x0001, // MAP_SHARED
        shm_fd,
        0,
    ) orelse {
        _ = c.close(shm_fd);
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

/// Resolve platform specification from a Roc file to find the host library
pub fn resolvePlatformHost(gpa: std.mem.Allocator, roc_file_path: []const u8) (std.mem.Allocator.Error || error{ NoPlatformFound, PlatformNotSupported })![]u8 {
    // Read the Roc file to parse the app header
    const roc_file = std.fs.cwd().openFile(roc_file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.NoPlatformFound,
        else => return error.NoPlatformFound, // Treat all file errors as no platform found
    };
    defer roc_file.close();

    const file_size = roc_file.getEndPos() catch return error.NoPlatformFound;
    const source = gpa.alloc(u8, @intCast(file_size)) catch return error.OutOfMemory;
    defer gpa.free(source);
    _ = roc_file.read(source) catch return error.NoPlatformFound;

    // Parse the source to find the app header
    // Look for "app" followed by platform specification
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        // Check if this is an app header line
        if (std.mem.startsWith(u8, trimmed, "app")) {
            // Look for platform specification after "platform"
            if (std.mem.indexOf(u8, trimmed, "platform")) |platform_start| {
                const after_platform = trimmed[platform_start + "platform".len ..];

                // Find the platform name/URL in quotes
                if (std.mem.indexOf(u8, after_platform, "\"")) |quote_start| {
                    const after_quote = after_platform[quote_start + 1 ..];
                    if (std.mem.indexOf(u8, after_quote, "\"")) |quote_end| {
                        const platform_spec = after_quote[0..quote_end];

                        // If it's a relative path, resolve it relative to the app directory
                        if (std.mem.startsWith(u8, platform_spec, "./") or std.mem.startsWith(u8, platform_spec, "../")) {
                            const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";
                            const platform_path = try std.fs.path.join(gpa, &.{ app_dir, platform_spec });
                            defer gpa.free(platform_path);

                            // Look for host library near the platform file
                            const platform_dir = std.fs.path.dirname(platform_path) orelse ".";
                            const host_filename = if (comptime builtin.target.os.tag == .windows) "host.lib" else "libhost.a";
                            const host_path = try std.fs.path.join(gpa, &.{ platform_dir, host_filename });
                            defer gpa.free(host_path);

                            std.fs.cwd().access(host_path, .{}) catch {
                                return error.PlatformNotSupported;
                            };

                            return try gpa.dupe(u8, host_path);
                        }

                        // Try to resolve platform to a local host library
                        return resolvePlatformSpecToHostLib(gpa, platform_spec);
                    }
                }
            }
        }
    }

    return error.NoPlatformFound;
}

/// Resolve a platform specification to a local host library path
fn resolvePlatformSpecToHostLib(gpa: std.mem.Allocator, platform_spec: []const u8) (std.mem.Allocator.Error || error{PlatformNotSupported})![]u8 {

    // Check for common platform names and map them to host libraries
    if (std.mem.eql(u8, platform_spec, "cli")) {
        // Try to find CLI platform host library
        const cli_paths = if (comptime builtin.target.os.tag == .windows)
            [_][]const u8{
                "zig-out/lib/platform_host_cli.lib",
                "platform/cli/host.lib",
                "platforms/cli/host.lib",
            }
        else
            [_][]const u8{
                "zig-out/lib/libplatform_host_cli.a",
                "platform/cli/host.a",
                "platforms/cli/host.a",
            };

        for (cli_paths) |path| {
            std.fs.cwd().access(path, .{}) catch continue;
            return try gpa.dupe(u8, path);
        }
    } else if (std.mem.eql(u8, platform_spec, "basic-cli")) {
        // Try to find basic-cli platform host library
        const basic_cli_paths = if (comptime builtin.target.os.tag == .windows)
            [_][]const u8{
                "zig-out/lib/platform_host_basic_cli.lib",
                "platform/basic-cli/host.lib",
                "platforms/basic-cli/host.lib",
            }
        else
            [_][]const u8{
                "zig-out/lib/libplatform_host_basic_cli.a",
                "platform/basic-cli/host.a",
                "platforms/basic-cli/host.a",
            };

        for (basic_cli_paths) |path| {
            std.fs.cwd().access(path, .{}) catch continue;
            return try gpa.dupe(u8, path);
        }
    } else if (std.mem.startsWith(u8, platform_spec, "http")) {
        // This is a URL - for production, would download and resolve
        // For now, return not supported
        return error.PlatformNotSupported;
    }

    // Try to interpret as a direct file path
    std.fs.cwd().access(platform_spec, .{}) catch {
        return error.PlatformNotSupported;
    };

    return try gpa.dupe(u8, platform_spec);
}

/// Extract the embedded roc_shim library to the specified path
/// This library contains the shim code that runs in child processes to read ModuleEnv from shared memory
pub fn extractReadRocFilePathShimLibrary(gpa: Allocator, output_path: []const u8) !void {
    _ = gpa; // unused but kept for consistency

    if (builtin.is_test) {
        // In test mode, create an empty file to avoid embedding issues
        const shim_file = try std.fs.cwd().createFile(output_path, .{});
        defer shim_file.close();
        return;
    }

    // Write the embedded shim library to the output path
    const shim_file = try std.fs.cwd().createFile(output_path, .{});
    defer shim_file.close();

    try shim_file.writeAll(roc_shim_lib);
}

/// Format a path validation reason into a user-friendly error message
fn formatPathValidationReason(reason: bundle.PathValidationReason) []const u8 {
    return switch (reason) {
        .empty_path => "Path cannot be empty",
        .path_too_long => "Path exceeds maximum length of 255 characters",
        .windows_reserved_char => |char| switch (char) {
            0 => "Path contains NUL byte (\\0)",
            ':' => "Path contains colon (:) which is reserved on Windows",
            '*' => "Path contains asterisk (*) which is a wildcard on Windows",
            '?' => "Path contains question mark (?) which is a wildcard on Windows",
            '"' => "Path contains quote (\") which is reserved on Windows",
            '<' => "Path contains less-than (<) which is reserved on Windows",
            '>' => "Path contains greater-than (>) which is reserved on Windows",
            '|' => "Path contains pipe (|) which is reserved on Windows",
            '\\' => "Path contains backslash (\\). Use forward slashes (/) for all paths",
            else => "Path contains reserved character",
        },
        .absolute_path => "Absolute paths are not allowed",
        .path_traversal => "Path traversal (..) is not allowed",
        .current_directory_reference => "Current directory reference (.) is not allowed",
        .contained_backslash_on_unix => "Path contains a backslash, which is a directory separator on Windows.",
        .windows_reserved_name => "Path contains Windows reserved device name (CON, PRN, AUX, NUL, COM1-9, LPT1-9)",
        .component_ends_with_space => "Path components cannot end with space",
        .component_ends_with_period => "Path components cannot end with period",
    };
}

fn rocBundle(gpa: Allocator, args: cli_args.BundleArgs) !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // Start timing
    const start_time = std.time.nanoTimestamp();

    // Get current working directory
    const cwd = std.fs.cwd();

    // Determine output directory
    var output_dir = if (args.output_dir) |dir|
        try cwd.openDir(dir, .{})
    else
        cwd;
    defer if (args.output_dir != null) output_dir.close();

    // Create a temporary directory for the output file
    var tmp_dir = try std.fs.cwd().makeOpenPath(".roc_bundle_tmp", .{});
    defer {
        tmp_dir.close();
        std.fs.cwd().deleteTree(".roc_bundle_tmp") catch {};
    }

    // Collect all files to bundle
    var file_paths = std.ArrayList([]const u8).init(gpa);
    defer file_paths.deinit();

    var uncompressed_size: u64 = 0;

    // Check that all files exist and collect their sizes
    for (args.paths) |path| {
        const file = cwd.openFile(path, .{}) catch |err| {
            try stderr.print("Error: Could not open file '{s}': {}\n", .{ path, err });
            return err;
        };
        defer file.close();

        const stat = try file.stat();
        uncompressed_size += stat.size;

        try file_paths.append(path);
    }

    // Create temporary output file
    const temp_filename = "temp_bundle.tar.zst";
    const temp_file = try tmp_dir.createFile(temp_filename, .{});
    defer temp_file.close();

    // Create file path iterator
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var iter = FilePathIterator{ .paths = file_paths.items };

    // Bundle the files
    var allocator_copy = gpa;
    var error_ctx: bundle.ErrorContext = undefined;
    const final_filename = bundle.bundle(
        &iter,
        @intCast(args.compression_level),
        &allocator_copy,
        temp_file.writer(),
        cwd,
        null, // path_prefix parameter - null means no stripping
        &error_ctx,
    ) catch |err| {
        if (err == error.InvalidPath) {
            try stderr.print("Error: Invalid file path - {s}\n", .{formatPathValidationReason(error_ctx.reason)});
            try stderr.print("Path: {s}\n", .{error_ctx.path});
        }
        return err;
    };
    defer gpa.free(final_filename);

    // Get the compressed file size
    const compressed_stat = try temp_file.stat();
    const compressed_size = compressed_stat.size;

    // Move the temp file to the final location
    try std.fs.rename(tmp_dir, temp_filename, output_dir, final_filename);

    // Calculate elapsed time
    const end_time = std.time.nanoTimestamp();
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = elapsed_ns / 1_000_000;

    // Calculate relative path for display
    const display_path = if (args.output_dir == null)
        final_filename
    else
        try std.fs.path.join(gpa, &.{ args.output_dir.?, final_filename });
    defer if (args.output_dir != null) gpa.free(display_path);

    // Print results
    try stdout.print("Created: {s}\n", .{display_path});
    try stdout.print("Compressed size: {} bytes\n", .{compressed_size});
    try stdout.print("Uncompressed size: {} bytes\n", .{uncompressed_size});
    try stdout.print("Compression ratio: {d:.2}:1\n", .{@as(f64, @floatFromInt(uncompressed_size)) / @as(f64, @floatFromInt(compressed_size))});
    try stdout.print("Time: {} ms\n", .{elapsed_ms});
}

fn rocUnbundle(gpa: Allocator, args: cli_args.UnbundleArgs) !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const cwd = std.fs.cwd();

    var had_errors = false;

    for (args.paths) |archive_path| {
        // Extract directory name from archive filename
        const basename = std.fs.path.basename(archive_path);
        var dir_name: []const u8 = undefined;

        if (std.mem.endsWith(u8, basename, ".tar.zst")) {
            dir_name = basename[0 .. basename.len - 8];
        } else {
            try stderr.print("Error: {s} is not a .tar.zst file\n", .{archive_path});
            had_errors = true;
            continue;
        }

        // Check if directory already exists
        cwd.access(dir_name, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                // Good, directory doesn't exist
            },
            else => return err,
        };

        if (cwd.openDir(dir_name, .{})) |_| {
            try stderr.print("Error: Directory {s} already exists\n", .{dir_name});
            had_errors = true;
            continue;
        } else |_| {
            // Directory doesn't exist, proceed
        }

        // Create the output directory
        var output_dir = try cwd.makeOpenPath(dir_name, .{});
        defer output_dir.close();

        // Open the archive file
        const archive_file = cwd.openFile(archive_path, .{}) catch |err| {
            try stderr.print("Error opening {s}: {s}\n", .{ archive_path, @errorName(err) });
            had_errors = true;
            continue;
        };
        defer archive_file.close();

        // Unbundle the archive
        var allocator_copy2 = gpa;
        var error_ctx: bundle.ErrorContext = undefined;
        bundle.unbundle(
            archive_file.reader(),
            output_dir,
            &allocator_copy2,
            basename,
            &error_ctx,
        ) catch |err| {
            switch (err) {
                error.HashMismatch => {
                    try stderr.print("Error: Hash mismatch for {s} - file may be corrupted\n", .{archive_path});
                    had_errors = true;
                },
                error.InvalidFilename => {
                    try stderr.print("Error: Invalid filename format for {s}\n", .{archive_path});
                    had_errors = true;
                },
                error.InvalidPath => {
                    try stderr.print("Error: Invalid path in archive - {s}\n", .{formatPathValidationReason(error_ctx.reason)});
                    try stderr.print("Path: {s}\n", .{error_ctx.path});
                    try stderr.print("Archive: {s}\n", .{archive_path});
                    had_errors = true;
                },
                else => {
                    try stderr.print("Error unbundling {s}: {s}\n", .{ archive_path, @errorName(err) });
                    had_errors = true;
                },
            }
            continue; // Skip success message on error
        };

        try stdout.print("Extracted: {s}\n", .{dir_name});
    }

    if (had_errors) {
        std.process.exit(1);
    }
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
    var process_result = coordinate_simple.processFile(
        gpa,
        Filesystem.default(),
        args.path,
        if (cache_manager) |*cm| cm else null,
        args.time,
    ) catch |err| {
        handleProcessFileError(err, stderr, args.path);
    };

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

    printTimingBreakdown(stdout, process_result.timing);
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
