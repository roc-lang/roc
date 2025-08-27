//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");
const build_options = @import("build_options");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const reporting = @import("reporting");
const parse = @import("parse");
const tracy = @import("tracy");
const fs_mod = @import("fs");
const compile = @import("compile");
const can = @import("can");
const check = @import("check");
const bundle = @import("bundle");
const unbundle = @import("unbundle");
const ipc = @import("ipc");
const fmt = @import("fmt");
const eval = @import("eval");
const layout = @import("layout");
const builtins = @import("builtins");

const cli_args = @import("cli_args.zig");
const bench = @import("bench.zig");
const linker = @import("linker.zig");
const platform_host_shim = @import("platform_host_shim.zig");
const builder = @import("builder.zig");

/// Check if LLVM is available
const llvm_available = builder.isLLVMAvailable();

const Can = can.Can;
const Check = check.Check;
const SharedMemoryAllocator = ipc.SharedMemoryAllocator;
const Filesystem = fs_mod.Filesystem;
const ModuleEnv = can.ModuleEnv;
const BuildEnv = compile.BuildEnv;
const TimingInfo = compile.package.TimingInfo;
const CacheManager = compile.CacheManager;
const CacheConfig = compile.CacheConfig;
const tokenize = parse.tokenize;
const Interpreter = eval.Interpreter;
const LayoutStore = layout.Store;
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;

const roc_interpreter_shim_lib = if (builtin.is_test) &[_]u8{} else if (builtin.target.os.tag == .windows) @embedFile("roc_interpreter_shim.lib") else @embedFile("libroc_interpreter_shim.a");

test "main cli tests" {
    _ = @import("test_bundle_logic.zig");
    _ = @import("libc_finder.zig");
    _ = @import("test_shared_memory_system.zig");
}

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
    extern "kernel32" fn GetExitCodeProcess(hProcess: HANDLE, lpExitCode: *DWORD) BOOL;

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
        .version => stdout.print("Roc compiler version {s}", .{build_options.compiler_version}),
        .docs => |docs_args| rocDocs(gpa, docs_args),
        .help => |help_message| stdout.writeAll(help_message),
        .licenses => stdout.writeAll(legalDetailsFileContent),
        .problem => |problem| {
            try switch (problem) {
                .missing_flag_value => |details| stderr.print("Error: no value was supplied for {s}", .{details.flag}),
                .unexpected_argument => |details| stderr.print("Error: roc {s} received an unexpected argument: `{s}`", .{ details.cmd, details.arg }),
                .invalid_flag_value => |details| stderr.print("Error: `{s}` is not a valid value for {s}. The valid options are {s}", .{ details.value, details.flag, details.valid_options }),
            };
            std.process.exit(1);
        },
    };
}

fn generatePlatformHostShim(gpa: Allocator, cache_dir: []const u8, entrypoint_names: []const []const u8) !?[]const u8 {
    // Check if LLVM is available (this is a compile-time check)
    if (!llvm_available) {
        std.log.debug("LLVM not available, skipping platform host shim generation", .{});
        return null;
    }

    const std_zig_llvm = @import("std").zig.llvm;
    const Builder = std_zig_llvm.Builder;

    // Create LLVM Builder
    var llvm_builder = Builder.init(.{
        .allocator = gpa,
        .name = "roc_platform_shim",
    }) catch |err| {
        std.log.err("Failed to initialize LLVM Builder: {}", .{err});
        return err;
    };
    defer llvm_builder.deinit();

    // Create entrypoints array from the provided names
    var entrypoints = std.ArrayList(platform_host_shim.EntryPoint).init(gpa);
    defer entrypoints.deinit();

    for (entrypoint_names, 0..) |name, idx| {
        try entrypoints.append(.{ .name = name, .idx = @intCast(idx) });
    }

    // Create the complete platform shim
    platform_host_shim.createInterpreterShim(&llvm_builder, entrypoints.items) catch |err| {
        std.log.err("Failed to create interpreter shim: {}", .{err});
        return err;
    };

    // Generate paths for temporary files
    const bitcode_path = std.fs.path.join(gpa, &.{ cache_dir, "platform_shim.bc" }) catch |err| {
        std.log.err("Failed to create bitcode path: {}", .{err});
        return err;
    };
    defer gpa.free(bitcode_path);

    const object_path = std.fs.path.join(gpa, &.{ cache_dir, "platform_shim.o" }) catch |err| {
        std.log.err("Failed to create object path: {}", .{err});
        return err;
    };
    // Don't defer free object_path since we return it

    // Generate bitcode first
    const producer = Builder.Producer{
        .name = "Roc Platform Host Shim Generator",
        .version = .{ .major = 1, .minor = 0, .patch = 0 },
    };

    const bitcode = llvm_builder.toBitcode(gpa, producer) catch |err| {
        std.log.err("Failed to generate bitcode: {}", .{err});
        gpa.free(object_path);
        return err;
    };
    defer gpa.free(bitcode);

    // Write bitcode to file
    const bc_file = std.fs.cwd().createFile(bitcode_path, .{}) catch |err| {
        std.log.err("Failed to create bitcode file: {}", .{err});
        gpa.free(object_path);
        return err;
    };
    defer bc_file.close();

    // Convert u32 array to bytes for writing
    const bytes = std.mem.sliceAsBytes(bitcode);
    bc_file.writeAll(bytes) catch |err| {
        std.log.err("Failed to write bitcode: {}", .{err});
        gpa.free(object_path);
        return err;
    };

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .speed,
        .target = builder.RocTarget.detectNative(),
    };

    if (builder.compileBitcodeToObject(gpa, compile_config)) |success| {
        if (!success) {
            std.log.warn("LLVM compilation not ready, falling back to clang", .{});
            return error.LLVMCompilationFailed;
        }
    } else |err| {
        std.log.warn("Failed to compile with embedded LLVM: {}, falling back to clang", .{err});
        return error.LLVMCompilationFailed;
    }

    std.log.debug("Generated platform host shim: {s}", .{object_path});

    return object_path;
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
        std.log.err("Failed to get cache directory: {}", .{err});
        std.process.exit(1);
    };
    defer gpa.free(cache_dir);
    const exe_cache_dir = std.fs.path.join(gpa, &.{ cache_dir, "executables" }) catch |err| {
        std.log.err("Failed to create executable cache path: {}", .{err});
        std.process.exit(1);
    };
    defer gpa.free(exe_cache_dir);

    std.fs.cwd().makePath(exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.log.err("Failed to create cache directory: {}", .{err});
            std.process.exit(1);
        },
    };

    // Generate executable name based on the roc file path
    // TODO use something more interesting like a hash from the platform.main or platform/host.a etc
    const exe_name = std.fmt.allocPrint(gpa, "roc_run_{}", .{std.hash.crc.Crc32.hash(args.path)}) catch |err| {
        std.log.err("Failed to generate executable name: {}", .{err});
        std.process.exit(1);
    };
    defer gpa.free(exe_name);

    const exe_path = std.fs.path.join(gpa, &.{ exe_cache_dir, exe_name }) catch |err| {
        std.log.err("Failed to create executable path: {}", .{err});
        std.process.exit(1);
    };
    defer gpa.free(exe_path);

    // First, parse the app file to get the platform reference
    const platform_spec = extractPlatformSpecFromApp(gpa, args.path) catch |err| {
        std.log.err("Failed to extract platform spec from app file: {}", .{err});
        std.process.exit(1);
    };
    defer gpa.free(platform_spec);

    // Resolve platform paths from the platform spec (relative to app file directory)
    const app_dir = std.fs.path.dirname(args.path) orelse ".";
    const platform_paths = resolvePlatformSpecToPaths(gpa, platform_spec, app_dir) catch |err| {
        std.log.err("Failed to resolve platform spec '{s}': {}", .{ platform_spec, err });
        std.process.exit(1);
    };
    defer platform_paths.deinit(gpa);

    // Extract entrypoints from platform source file
    var entrypoints = std.ArrayList([]const u8).init(gpa);
    defer {
        for (entrypoints.items) |entrypoint| {
            gpa.free(entrypoint);
        }
        entrypoints.deinit();
    }

    if (platform_paths.platform_source_path) |platform_source| {
        extractEntrypointsFromPlatform(gpa, platform_source, &entrypoints) catch |err| {
            std.log.err("Failed to extract entrypoints from platform header: {}", .{err});
            std.process.exit(1);
        };
    } else {
        std.log.err("No platform source file found for entrypoint extraction", .{});
        std.process.exit(1);
    }

    // Check if the interpreter executable already exists (cached)
    const exe_exists = if (args.no_cache) false else blk: {
        std.fs.accessAbsolute(exe_path, .{}) catch {
            break :blk false;
        };
        break :blk true;
    };

    if (!exe_exists) {

        // Check for cached shim library, extract if not present
        const shim_filename = if (builtin.target.os.tag == .windows) "roc_shim.lib" else "libroc_shim.a";
        const shim_path = std.fs.path.join(gpa, &.{ exe_cache_dir, shim_filename }) catch |err| {
            std.log.err("Failed to create shim library path: {}", .{err});
            std.process.exit(1);
        };
        defer gpa.free(shim_path);

        // Extract shim if not cached or if --no-cache is used
        const shim_exists = if (args.no_cache) false else blk: {
            std.fs.cwd().access(shim_path, .{}) catch {
                break :blk false;
            };
            break :blk true;
        };

        if (!shim_exists) {
            // Shim not found in cache or cache disabled, extract it
            extractReadRocFilePathShimLibrary(gpa, shim_path) catch |err| {
                std.log.err("Failed to extract read roc file path shim library: {}", .{err});
                std.process.exit(1);
            };
        }

        // Generate platform host shim using the detected entrypoints

        const platform_shim_path = generatePlatformHostShim(gpa, exe_cache_dir, entrypoints.items) catch |err| {
            std.log.err("Failed to generate platform host shim: {}", .{err});
            std.process.exit(1);
        };
        defer if (platform_shim_path) |path| gpa.free(path);

        // Link the host.a with our shim to create the interpreter executable using our linker
        // Try LLD first, fallback to clang if LLVM is not available
        var extra_args = std.ArrayList([]const u8).init(gpa);
        defer extra_args.deinit();

        // Add system libraries for macOS
        if (builtin.target.os.tag == .macos) {
            extra_args.append("-lSystem") catch {
                std.log.err("Failed to allocate memory for linker args", .{});
                std.process.exit(1);
            };
        }

        // Create object files list - include platform shim if available
        var object_files = std.ArrayList([]const u8).init(gpa);
        defer object_files.deinit();
        object_files.append(platform_paths.host_lib_path) catch {
            std.log.err("Failed to add host path to object files", .{});
            std.process.exit(1);
        };
        if (platform_shim_path) |path| {
            object_files.append(path) catch {
                std.log.err("Failed to add platform shim path to object files", .{});
                std.process.exit(1);
            };
        }
        object_files.append(shim_path) catch {
            std.log.err("Failed to add shim path to object files", .{});
            std.process.exit(1);
        };

        // Determine platform-specific dependencies based on platform spec
        var platform_files_pre = std.ArrayList([]const u8).init(gpa);
        defer platform_files_pre.deinit();
        var platform_files_post = std.ArrayList([]const u8).init(gpa);
        defer platform_files_post.deinit();
        var target_abi: ?linker.TargetAbi = null;

        // Determine platform type from host library path to configure dependencies
        std.log.debug("Platform host library path: {s}", .{platform_paths.host_lib_path});
        if (std.mem.indexOf(u8, platform_paths.host_lib_path, "/int/") != null and std.mem.indexOf(u8, platform_paths.host_lib_path, "platform") != null) {
            std.log.debug("Detected int platform - using musl static linking", .{});
            // Int platform: use musl static linking
            target_abi = .musl;
            if (builtin.target.os.tag == .linux) {
                platform_files_pre.append("test/int/platform/vendored/musl/crt1.o") catch {
                    std.log.err("Failed to add musl crt1.o", .{});
                    std.process.exit(1);
                };
                platform_files_post.append("test/int/platform/vendored/musl/libc.a") catch {
                    std.log.err("Failed to add musl libc.a", .{});
                    std.process.exit(1);
                };
            }
        } else if (std.mem.indexOf(u8, platform_paths.host_lib_path, "/str/") != null and std.mem.indexOf(u8, platform_paths.host_lib_path, "platform") != null) {
            std.log.debug("Detected str platform - using gnu dynamic linking", .{});
            // Str platform: use gnu dynamic linking
            target_abi = .gnu;
            if (builtin.target.os.tag == .linux) {
                // TEMPORARY SOLUTION: Auto-detecting CRT files and library paths
                //
                // According to the platform design (see Platform modules and linking design.md),
                // platform authors should explicitly provide all required CRT files (Scrt1.o, crti.o,
                // crtn.o) in their platform's targets/ directory. For example:
                //   targets/x64glibc/crti.o
                //   targets/x64glibc/crtn.o
                //   targets/x64musl/crti.o
                //   targets/x64musl/crtn.o
                //
                // The platform module header would then specify these in the targets section:
                //   shared_lib: {
                //       x64glibc: ["crti.o", "host.o", app, "crtn.o"],
                //       x64musl: ["crti.o", "host.o", app, "crtn.o"],
                //   }
                //
                // Current implementation: We auto-detect system libc and use system CRT files as a
                // convenience during platform development. This allows platform authors to develop
                // and test without immediately vendoring architecture-specific CRT files.
                //
                // This auto-detection will be removed once platforms properly vendor their CRT files
                // as specified in the design document.

                const libc_finder = @import("libc_finder.zig");
                if (libc_finder.findLibc(gpa)) |libc_info| {
                    defer {
                        var info = libc_info;
                        info.deinit();
                    }

                    // Use system CRT files from the detected lib directory
                    // TODO: Remove this once platforms provide their own CRT files
                    const scrt1_path = std.fmt.allocPrint(gpa, "{s}/Scrt1.o", .{libc_info.lib_dir}) catch {
                        std.log.err("Failed to allocate Scrt1.o path", .{});
                        std.process.exit(1);
                    };
                    const crti_path = std.fmt.allocPrint(gpa, "{s}/crti.o", .{libc_info.lib_dir}) catch {
                        std.log.err("Failed to allocate crti.o path", .{});
                        std.process.exit(1);
                    };
                    const crtn_path = std.fmt.allocPrint(gpa, "{s}/crtn.o", .{libc_info.lib_dir}) catch {
                        std.log.err("Failed to allocate crtn.o path", .{});
                        std.process.exit(1);
                    };

                    // Check if system CRT files exist, fall back to vendored ones if not (for x86_64 only)
                    if (std.fs.openFileAbsolute(scrt1_path, .{})) |file| {
                        file.close();
                        platform_files_pre.append(scrt1_path) catch {
                            std.log.err("Failed to add system Scrt1.o", .{});
                            std.process.exit(1);
                        };
                    } else |_| {
                        if (builtin.target.cpu.arch == .x86_64) {
                            platform_files_pre.append("test/str/platform/vendored/gnu/Scrt1.o") catch {
                                std.log.err("Failed to add vendored Scrt1.o", .{});
                                std.process.exit(1);
                            };
                        } else {
                            std.log.err("CRT file Scrt1.o not found at {s} and no vendored version for this architecture", .{scrt1_path});
                            std.process.exit(1);
                        }
                    }

                    if (std.fs.openFileAbsolute(crti_path, .{})) |file| {
                        file.close();
                        platform_files_pre.append(crti_path) catch {
                            std.log.err("Failed to add system crti.o", .{});
                            std.process.exit(1);
                        };
                    } else |_| {
                        if (builtin.target.cpu.arch == .x86_64) {
                            platform_files_pre.append("test/str/platform/vendored/gnu/crti.o") catch {
                                std.log.err("Failed to add vendored crti.o", .{});
                                std.process.exit(1);
                            };
                        } else {
                            std.log.err("CRT file crti.o not found at {s} and no vendored version for this architecture", .{crti_path});
                            std.process.exit(1);
                        }
                    }

                    if (std.fs.openFileAbsolute(crtn_path, .{})) |file| {
                        file.close();
                        platform_files_post.append(crtn_path) catch {
                            std.log.err("Failed to add system crtn.o", .{});
                            std.process.exit(1);
                        };
                    } else |_| {
                        if (builtin.target.cpu.arch == .x86_64) {
                            platform_files_post.append("test/str/platform/vendored/gnu/crtn.o") catch {
                                std.log.err("Failed to add vendored crtn.o", .{});
                                std.process.exit(1);
                            };
                        } else {
                            std.log.err("CRT file crtn.o not found at {s} and no vendored version for this architecture", .{crtn_path});
                            std.process.exit(1);
                        }
                    }

                    const lib_path = std.fmt.allocPrint(gpa, "-L{s}", .{libc_info.lib_dir}) catch {
                        std.log.err("Failed to allocate library path", .{});
                        std.process.exit(1);
                    };
                    extra_args.append(lib_path) catch {
                        std.log.err("Failed to add library path", .{});
                        std.process.exit(1);
                    };
                } else |_| {
                    // Fallback to vendored files for x86_64 or error for other architectures
                    if (builtin.target.cpu.arch == .x86_64) {
                        platform_files_pre.append("test/str/platform/vendored/gnu/Scrt1.o") catch {
                            std.log.err("Failed to add gnu Scrt1.o", .{});
                            std.process.exit(1);
                        };
                        platform_files_pre.append("test/str/platform/vendored/gnu/crti.o") catch {
                            std.log.err("Failed to add gnu crti.o", .{});
                            std.process.exit(1);
                        };
                        platform_files_post.append("test/str/platform/vendored/gnu/crtn.o") catch {
                            std.log.err("Failed to add gnu crtn.o", .{});
                            std.process.exit(1);
                        };
                        extra_args.append("-L/usr/lib/x86_64-linux-gnu") catch {
                            std.log.err("Failed to add library path", .{});
                            std.process.exit(1);
                        };
                    } else {
                        std.log.err("Cannot detect system libc for architecture {} and no vendored CRT files available", .{builtin.target.cpu.arch});
                        std.process.exit(1);
                    }
                }
                extra_args.append("-lc") catch {
                    std.log.err("Failed to add libc", .{});
                    std.process.exit(1);
                };
            }
        } else {
            std.log.debug("No platform-specific configuration found, using defaults", .{});
        }

        std.log.debug("Final target_abi: {?}", .{target_abi});

        const link_config = linker.LinkConfig{
            .target_abi = target_abi,
            .output_path = exe_path,
            .object_files = object_files.items,
            .platform_files_pre = platform_files_pre.items,
            .platform_files_post = platform_files_post.items,
            .extra_args = extra_args.items,
            .can_exit_early = false,
            .disable_output = false,
        };

        linker.link(gpa, link_config) catch |err| switch (err) {
            linker.LinkError.LLVMNotAvailable => {
                std.log.err("LLD linker not available -- this is likely a test executable that was built without LLVM", .{});
                std.process.exit(1);
            },
            linker.LinkError.LinkFailed => {
                std.log.err("LLD linker failed to create executable", .{});
                std.process.exit(1);
            },
            else => {
                std.log.err("Failed to link executable: {}", .{err});
                std.process.exit(1);
            },
        };
    }

    // Set up shared memory with ModuleEnv
    std.log.debug("Setting up shared memory for Roc file: {s}", .{args.path});
    const shm_handle = setupSharedMemoryWithModuleEnv(gpa, args.path) catch |err| {
        std.log.err("Failed to set up shared memory with ModuleEnv: {}", .{err});
        std.process.exit(1);
    };
    std.log.debug("Shared memory setup complete, size: {} bytes", .{shm_handle.size});

    // Ensure we clean up shared memory resources on all exit paths
    defer {
        if (comptime is_windows) {
            _ = ipc.platform.windows.UnmapViewOfFile(shm_handle.ptr);
            _ = ipc.platform.windows.CloseHandle(@ptrCast(shm_handle.fd));
        } else {
            _ = posix.munmap(shm_handle.ptr, shm_handle.size);
            _ = c.close(shm_handle.fd);
        }
    }

    std.log.debug("Launching interpreter executable: {s}", .{exe_path});
    if (comptime is_windows) {
        // Windows: Use handle inheritance approach
        std.log.debug("Using Windows handle inheritance approach", .{});
        runWithWindowsHandleInheritance(gpa, exe_path, shm_handle) catch |err| {
            std.log.err("Failed to run with Windows handle inheritance: {}", .{err});
            std.process.exit(1);
        };
    } else {
        // POSIX: Use existing file descriptor inheritance approach
        std.log.debug("Using POSIX file descriptor inheritance approach", .{});
        runWithPosixFdInheritance(gpa, exe_path, shm_handle, &cache_manager) catch |err| {
            std.log.err("Failed to run with POSIX fd inheritance: {}", .{err});
            std.process.exit(1);
        };
    }
    std.log.debug("Interpreter execution completed", .{});
}

/// Run child process using Windows handle inheritance (idiomatic Windows approach)
fn runWithWindowsHandleInheritance(gpa: Allocator, exe_path: []const u8, shm_handle: SharedMemoryHandle) !void {
    // Make the shared memory handle inheritable
    if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
        std.log.err("Failed to set handle as inheritable", .{});
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
        std.log.err("CreateProcessW failed", .{});
        return error.ProcessCreationFailed;
    }

    // Child process spawned successfully

    // Wait for the child process to complete
    std.log.debug("Waiting for child process to complete: {s}", .{exe_path});
    const wait_result = windows.WaitForSingleObject(process_info.hProcess, windows.INFINITE);
    if (wait_result != 0) { // WAIT_OBJECT_0 = 0
        std.log.err("WaitForSingleObject failed or timed out (result: {})", .{wait_result});
        // Clean up handles before returning
        _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
        _ = ipc.platform.windows.CloseHandle(process_info.hThread);
        return error.ProcessWaitFailed;
    }

    // Get the exit code
    var exit_code: windows.DWORD = undefined;
    if (windows.GetExitCodeProcess(process_info.hProcess, &exit_code) == 0) {
        std.log.err("Failed to get exit code for child process", .{});
        // Clean up handles before returning
        _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
        _ = ipc.platform.windows.CloseHandle(process_info.hThread);
        return error.ProcessExitCodeFailed;
    }

    // Clean up process handles
    _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
    _ = ipc.platform.windows.CloseHandle(process_info.hThread);

    // Check exit code
    if (exit_code != 0) {
        std.log.err("Child process {s} exited with code: {}", .{ exe_path, exit_code });
        if (exit_code == 0xC0000005) { // STATUS_ACCESS_VIOLATION
            std.log.err("Child process crashed with access violation (segfault)", .{});
        } else if (exit_code >= 0xC0000000) { // NT status codes for exceptions
            std.log.err("Child process crashed with exception code: 0x{X}", .{exit_code});
        }
        return error.ProcessExitedWithError;
    }

    std.log.debug("Child process completed successfully", .{});
}

/// Run child process using POSIX file descriptor inheritance (existing approach for Unix)
fn runWithPosixFdInheritance(gpa: Allocator, exe_path: []const u8, shm_handle: SharedMemoryHandle, cache_manager: *CacheManager) !void {
    // Get cache directory for temporary files
    const temp_cache_dir = cache_manager.config.getTempDir(gpa) catch |err| {
        std.log.err("Failed to get temp cache directory: {}", .{err});
        return err;
    };
    defer gpa.free(temp_cache_dir);

    // Ensure temp cache directory exists
    std.fs.cwd().makePath(temp_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.log.err("Failed to create temp cache directory: {}", .{err});
            return err;
        },
    };

    // Create temporary directory structure for fd communication
    std.log.debug("Creating temporary directory structure for fd communication", .{});
    const temp_exe_path = createTempDirStructure(gpa, exe_path, shm_handle, temp_cache_dir) catch |err| {
        std.log.err("Failed to create temp dir structure: {}", .{err});
        return err;
    };
    defer gpa.free(temp_exe_path);
    std.log.debug("Temporary executable created at: {s}", .{temp_exe_path});

    // Configure fd inheritance
    var flags = posix.fcntl(shm_handle.fd, posix.F_GETFD, 0);
    if (flags < 0) {
        std.log.err("Failed to get fd flags: {}", .{c._errno().*});
        return error.FdConfigFailed;
    }

    flags &= ~@as(c_int, posix.FD_CLOEXEC);

    if (posix.fcntl(shm_handle.fd, posix.F_SETFD, flags) < 0) {
        std.log.err("Failed to set fd flags: {}", .{c._errno().*});
        return error.FdConfigFailed;
    }

    // Run the interpreter as a child process from the temp directory
    var child = std.process.Child.init(&.{temp_exe_path}, gpa);
    child.cwd = std.fs.cwd().realpathAlloc(gpa, ".") catch |err| {
        std.log.err("Failed to get current directory: {}", .{err});
        return err;
    };
    defer gpa.free(child.cwd.?);

    // Forward stdout and stderr
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    // Spawn the child process
    std.log.debug("Spawning child process: {s}", .{temp_exe_path});
    std.log.debug("Child process working directory: {s}", .{child.cwd.?});
    child.spawn() catch |err| {
        std.log.err("Failed to spawn {s}: {}", .{ temp_exe_path, err });
        return err;
    };
    std.log.debug("Child process spawned successfully (PID: {})", .{child.id});

    // Wait for child to complete
    const term = child.wait() catch |err| {
        std.log.err("Failed waiting for child process: {}", .{err});
        return err;
    };

    // Check the termination status
    switch (term) {
        .Exited => |exit_code| {
            if (exit_code == 0) {
                std.log.debug("Child process completed successfully", .{});
            } else {
                std.log.err("Child process {s} exited with code: {}", .{ temp_exe_path, exit_code });
                return error.ProcessExitedWithError;
            }
        },
        .Signal => |signal| {
            std.log.err("Child process {s} killed by signal: {}", .{ temp_exe_path, signal });
            if (signal == 11) { // SIGSEGV
                std.log.err("Child process crashed with segmentation fault (SIGSEGV)", .{});
            } else if (signal == 6) { // SIGABRT
                std.log.err("Child process aborted (SIGABRT)", .{});
            } else if (signal == 9) { // SIGKILL
                std.log.err("Child process was killed (SIGKILL)", .{});
            }
            return error.ProcessKilledBySignal;
        },
        .Stopped => |signal| {
            std.log.err("Child process {s} stopped by signal: {}", .{ temp_exe_path, signal });
            return error.ProcessStopped;
        },
        .Unknown => |status| {
            std.log.err("Child process {s} terminated with unknown status: {}", .{ temp_exe_path, status });
            return error.ProcessUnknownTermination;
        },
    }
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
    const shm_handle = ipc.platform.windows.CreateFileMappingW(
        ipc.platform.windows.INVALID_HANDLE_VALUE,
        null,
        ipc.platform.windows.PAGE_READWRITE,
        0,
        @intCast(total_size),
        null, // Anonymous - no name needed for handle inheritance
    ) orelse {
        std.log.err("Failed to create shared memory mapping", .{});
        return error.SharedMemoryCreateFailed;
    };

    // Map the shared memory at a fixed address to avoid ASLR issues
    const mapped_ptr = ipc.platform.windows.MapViewOfFileEx(
        shm_handle,
        ipc.platform.windows.FILE_MAP_ALL_ACCESS,
        0,
        0,
        0,
        ipc.platform.SHARED_MEMORY_BASE_ADDR,
    ) orelse {
        _ = ipc.platform.windows.CloseHandle(shm_handle);
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
/// Stores all exported definitions for multi-entrypoint evaluation.
pub fn setupSharedMemoryWithModuleEnv(gpa: std.mem.Allocator, roc_file_path: []const u8) !SharedMemoryHandle {
    // Create shared memory with SharedMemoryAllocator
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(SHARED_MEMORY_SIZE, page_size);
    // Don't defer deinit here - we need to keep the shared memory alive

    const shm_allocator = shm.allocator();

    // Create a properly aligned header structure
    const Header = struct {
        parent_base_addr: u64,
        entry_count: u32,
        _padding: u32, // Ensure 8-byte alignment
        def_indices_offset: u64,
        module_env_offset: u64,
    };

    const header_ptr = try shm_allocator.create(Header);

    // Store the base address of the shared memory mapping (for ASLR-safe relocation)
    // The child will calculate the offset from its own base address
    const shm_base_addr = @intFromPtr(shm.base_ptr);
    header_ptr.parent_base_addr = shm_base_addr;

    // Allocate the ModuleEnv right after the header for predictable layout
    const env_ptr = try shm_allocator.create(ModuleEnv);
    const module_env_offset = @intFromPtr(env_ptr) - @intFromPtr(shm.base_ptr);
    header_ptr.module_env_offset = module_env_offset;

    // Read the actual Roc file
    const roc_file = std.fs.cwd().openFile(roc_file_path, .{}) catch |err| {
        std.log.err("Failed to open Roc file '{s}': {}", .{ roc_file_path, err });
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
    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(shm_allocator);

    // Parse the source code as a full module
    var parse_ast = try parse.parse(&env.common, gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(shm_allocator, module_name);

    // Create canonicalizer
    var canonicalizer = try Can.init(&env, &parse_ast, null);

    // Canonicalize the entire module
    try canonicalizer.canonicalizeFile();

    // Validation check - ensure exports were populated during canonicalization
    if (env.exports.span.len == 0) {
        std.log.err("No exported definitions found after canonicalization", .{});
        return error.NoMainFunction;
    }

    // Get the exported definitions from the canonicalization process
    const exports_slice = env.store.sliceDefs(env.exports);

    // Store entry count based on exports
    header_ptr.entry_count = @intCast(exports_slice.len);

    // Allocate space for exported def indices array
    const def_indices_ptr = try shm_allocator.alloc(u32, exports_slice.len);

    // Store the def_indices location in the header
    const def_indices_location = @intFromPtr(def_indices_ptr.ptr) - @intFromPtr(shm.base_ptr);
    header_ptr.def_indices_offset = def_indices_location;

    // Store definition index for each exported function
    for (exports_slice, 0..) |def_idx, i| {
        def_indices_ptr[i] = @intFromEnum(def_idx);
    }

    // Type check the module
    var checker = try Check.init(shm_allocator, &env.types, &env, &.{}, &env.store.regions);
    try checker.checkDefs();

    // Copy the ModuleEnv to the allocated space
    env_ptr.* = env;

    // Clean up the canonicalizer and parsing structures
    canonicalizer.deinit();

    // Clean up parse_ast since it was allocated with gpa, not shared memory
    parse_ast.deinit(gpa);

    // Clean up checker since it was allocated with shared memory, but we need to clean up its gpa allocations
    checker.deinit();

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

/// Platform resolution result containing both host library and platform source paths
pub const PlatformPaths = struct {
    host_lib_path: []const u8,
    platform_source_path: ?[]const u8, // Optional - may not exist for some platforms

    pub fn deinit(self: *const PlatformPaths, gpa: std.mem.Allocator) void {
        gpa.free(self.host_lib_path);
        if (self.platform_source_path) |path| {
            gpa.free(path);
        }
    }
};

/// Resolve platform specification from a Roc file to find both host library and platform source
pub fn resolvePlatformPaths(gpa: std.mem.Allocator, roc_file_path: []const u8) (std.mem.Allocator.Error || error{ NoPlatformFound, PlatformNotSupported })!PlatformPaths {
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

                            // Try to find platform source file (commonly main.roc but could be anything)
                            const platform_source_path = blk: {
                                // First try the exact path if it's a .roc file
                                if (std.mem.endsWith(u8, platform_path, ".roc")) {
                                    std.fs.cwd().access(platform_path, .{}) catch break :blk null;
                                    break :blk try gpa.dupe(u8, platform_path);
                                }

                                // Try common platform source names in the platform directory
                                const common_names = [_][]const u8{ "main.roc", "platform.roc", "Platform.roc" };
                                for (common_names) |name| {
                                    const source_path = try std.fs.path.join(gpa, &.{ platform_dir, name });
                                    defer gpa.free(source_path);
                                    std.fs.cwd().access(source_path, .{}) catch continue;
                                    break :blk try gpa.dupe(u8, source_path);
                                }
                                break :blk null;
                            };

                            return PlatformPaths{
                                .host_lib_path = try gpa.dupe(u8, host_path),
                                .platform_source_path = platform_source_path,
                            };
                        }

                        // Try to resolve platform to a local host library and source
                        const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";
                        return resolvePlatformSpecToPaths(gpa, platform_spec, app_dir);
                    }
                }
            }
        }
    }

    return error.NoPlatformFound;
}

/// Extract platform specification from app file header using simple string parsing
fn extractPlatformSpecFromApp(gpa: std.mem.Allocator, app_file_path: []const u8) ![]const u8 {
    // Read the app file
    const source = std.fs.cwd().readFileAlloc(gpa, app_file_path, std.math.maxInt(usize)) catch return error.FileNotFound;
    defer gpa.free(source);

    // Simple string parsing to find platform specification
    // Look for pattern: platform "..." or platform ".../..."
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (std.mem.startsWith(u8, trimmed, "app ")) {
            // Look for pf: platform "..." pattern
            if (std.mem.indexOf(u8, trimmed, "pf: platform \"")) |start_idx| {
                const after_quote = start_idx + 14; // length of "pf: platform \""
                if (std.mem.indexOfScalarPos(u8, trimmed, after_quote, '"')) |end_idx| {
                    const platform_path = trimmed[after_quote..end_idx];
                    return try gpa.dupe(u8, platform_path);
                }
            }
            // Also try alternative format: platform "..."
            if (std.mem.indexOf(u8, trimmed, "platform \"")) |start_idx| {
                const quote_start = start_idx + 10; // length of "platform \""
                if (std.mem.indexOfScalarPos(u8, trimmed, quote_start, '"')) |end_idx| {
                    const platform_path = trimmed[quote_start..end_idx];
                    return try gpa.dupe(u8, platform_path);
                }
            }
        }
    }

    return error.NotAppFile;
}

/// Resolve a platform specification to both host library and platform source paths
fn resolvePlatformSpecToPaths(gpa: std.mem.Allocator, platform_spec: []const u8, base_dir: []const u8) (std.mem.Allocator.Error || error{PlatformNotSupported})!PlatformPaths {

    // Check for common platform names and map them to host libraries
    if (std.mem.eql(u8, platform_spec, "cli")) {
        // Try to find CLI platform host library
        const cli_host_paths = if (comptime builtin.target.os.tag == .windows)
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

        const cli_source_paths = [_][]const u8{
            "platform/cli/platform.roc",
            "platforms/cli/platform.roc",
        };

        for (cli_host_paths) |host_path| {
            std.fs.cwd().access(host_path, .{}) catch continue;

            // Found host library, now try to find platform source
            var platform_source_path: ?[]const u8 = null;
            for (cli_source_paths) |source_path| {
                std.fs.cwd().access(source_path, .{}) catch continue;
                platform_source_path = try gpa.dupe(u8, source_path);
                break;
            }

            return PlatformPaths{
                .host_lib_path = try gpa.dupe(u8, host_path),
                .platform_source_path = platform_source_path,
            };
        }
    } else if (std.mem.eql(u8, platform_spec, "basic-cli")) {
        // Try to find basic-cli platform host library
        const basic_cli_host_paths = if (comptime builtin.target.os.tag == .windows)
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

        const basic_cli_source_paths = [_][]const u8{
            "platform/basic-cli/platform.roc",
            "platforms/basic-cli/platform.roc",
        };

        for (basic_cli_host_paths) |host_path| {
            std.fs.cwd().access(host_path, .{}) catch continue;

            // Found host library, now try to find platform source
            var platform_source_path: ?[]const u8 = null;
            for (basic_cli_source_paths) |source_path| {
                std.fs.cwd().access(source_path, .{}) catch continue;
                platform_source_path = try gpa.dupe(u8, source_path);
                break;
            }

            return PlatformPaths{
                .host_lib_path = try gpa.dupe(u8, host_path),
                .platform_source_path = platform_source_path,
            };
        }
    } else if (std.mem.startsWith(u8, platform_spec, "http")) {
        // This is a URL - for production, would download and resolve
        // For now, return not supported
        return error.PlatformNotSupported;
    }

    // Try to interpret as a file path (resolve relative to base_dir)
    const resolved_path = if (std.fs.path.isAbsolute(platform_spec))
        try gpa.dupe(u8, platform_spec)
    else
        try std.fs.path.join(gpa, &.{ base_dir, platform_spec });
    defer if (!std.fs.path.isAbsolute(platform_spec)) gpa.free(resolved_path);

    std.fs.cwd().access(resolved_path, .{}) catch {
        return error.PlatformNotSupported;
    };

    // For file paths, we need to determine if it's a host library or platform source
    // Host libraries typically have .a/.lib extensions, platform sources have .roc extension
    if (std.mem.endsWith(u8, resolved_path, ".roc")) {
        // This is a platform source file - look for host library near it
        const platform_dir = std.fs.path.dirname(resolved_path) orelse ".";
        const host_filename = if (comptime builtin.target.os.tag == .windows) "host.lib" else "libhost.a";
        const host_path = try std.fs.path.join(gpa, &.{ platform_dir, host_filename });
        defer gpa.free(host_path);

        std.fs.cwd().access(host_path, .{}) catch {
            return error.PlatformNotSupported;
        };

        return PlatformPaths{
            .host_lib_path = try gpa.dupe(u8, host_path),
            .platform_source_path = try gpa.dupe(u8, resolved_path),
        };
    } else {
        // Assume it's a host library file
        return PlatformPaths{
            .host_lib_path = try gpa.dupe(u8, resolved_path),
            .platform_source_path = null,
        };
    }
}

/// Extract all entrypoint names from platform header provides record into ArrayList
/// TODO: Replace this with proper BuildEnv solution in the future
fn extractEntrypointsFromPlatform(gpa: std.mem.Allocator, roc_file_path: []const u8, entrypoints: *std.ArrayList([]const u8)) !void {
    // Read the Roc file
    const source = std.fs.cwd().readFileAlloc(gpa, roc_file_path, std.math.maxInt(usize)) catch return error.NoPlatformFound;
    defer gpa.free(source);

    // Extract module name from the file path
    const basename = std.fs.path.basename(roc_file_path);
    const module_name = try gpa.dupe(u8, basename);
    defer gpa.free(module_name);

    // Create ModuleEnv
    var env = ModuleEnv.init(gpa, source) catch return error.ParseFailed;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(gpa);

    // Parse the source code as a full module
    var parse_ast = parse.parse(&env.common, gpa) catch return error.ParseFailed;
    defer parse_ast.deinit(gpa);

    // Look for platform header in the AST
    const file_node = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file_node.header);

    // Check if this is a platform file with a platform header
    switch (header) {
        .platform => |platform_header| {
            // Get the provides collection and its record fields
            const provides_coll = parse_ast.store.getCollection(platform_header.provides);
            const provides_fields = parse_ast.store.recordFieldSlice(.{ .span = provides_coll.span });

            // Extract all field names as entrypoints
            for (provides_fields) |field_idx| {
                const field = parse_ast.store.getRecordField(field_idx);
                const field_name = parse_ast.resolve(field.name);
                try entrypoints.append(try gpa.dupe(u8, field_name));
            }

            if (provides_fields.len == 0) {
                return error.NoEntrypointFound;
            }
        },
        else => {
            return error.NotPlatformFile;
        },
    }
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

    try shim_file.writeAll(roc_interpreter_shim_lib);
}

/// Format a bundle path validation reason into a user-friendly error message
fn formatBundlePathValidationReason(reason: bundle.PathValidationReason) []const u8 {
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

/// Format an unbundle path validation reason into a user-friendly error message
fn formatUnbundlePathValidationReason(reason: unbundle.PathValidationReason) []const u8 {
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

/// Bundles a roc package and its dependencies into a compressed tar archive
pub fn rocBundle(gpa: Allocator, args: cli_args.BundleArgs) !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // Use arena allocator for all bundle operations
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

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
    var file_paths = std.ArrayList([]const u8).init(arena_allocator);
    defer file_paths.deinit();

    var uncompressed_size: u64 = 0;

    // If no paths provided, default to "main.roc"
    const paths_to_use = if (args.paths.len == 0) &[_][]const u8{"main.roc"} else args.paths;

    // Remember the first path from CLI args (before sorting)
    const first_cli_path = paths_to_use[0];

    // Check that all files exist and collect their sizes
    for (paths_to_use) |path| {
        const file = cwd.openFile(path, .{}) catch |err| {
            try stderr.print("Error: Could not open file '{s}': {}", .{ path, err });
            return err;
        };
        defer file.close();

        const stat = try file.stat();
        uncompressed_size += stat.size;

        try file_paths.append(path);
    }

    // Sort and deduplicate paths
    std.mem.sort([]const u8, file_paths.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    // Remove duplicates by keeping only unique consecutive elements
    var unique_count: usize = 0;
    for (file_paths.items, 0..) |path, i| {
        if (i == 0 or !std.mem.eql(u8, path, file_paths.items[i - 1])) {
            file_paths.items[unique_count] = path;
            unique_count += 1;
        }
    }
    file_paths.items.len = unique_count;

    // If we have more than one file, ensure the first CLI arg stays first
    if (file_paths.items.len > 1) {
        // Find the first CLI path in the sorted array
        var found_index: ?usize = null;
        for (file_paths.items, 0..) |path, i| {
            if (std.mem.eql(u8, path, first_cli_path)) {
                found_index = i;
                break;
            }
        }

        // Swap the found item with the first position if needed
        if (found_index) |idx| {
            if (idx != 0) {
                const temp = file_paths.items[0];
                file_paths.items[0] = file_paths.items[idx];
                file_paths.items[idx] = temp;
            }
        }
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
    var allocator_copy = arena_allocator;
    var error_ctx: bundle.ErrorContext = undefined;
    const final_filename = bundle.bundleFiles(
        &iter,
        @intCast(args.compression_level),
        &allocator_copy,
        temp_file.writer(),
        cwd,
        null, // path_prefix parameter - null means no stripping
        &error_ctx,
    ) catch |err| {
        if (err == error.InvalidPath) {
            try stderr.print("Error: Invalid file path - {s}", .{formatBundlePathValidationReason(error_ctx.reason)});
            try stderr.print("Path: {s}", .{error_ctx.path});
        }
        return err;
    };
    // No need to free when using arena allocator

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
        try std.fs.path.join(arena_allocator, &.{ args.output_dir.?, final_filename });
    // No need to free when using arena allocator

    // Print results
    try stdout.print("Created: {s}", .{display_path});
    try stdout.print("Compressed size: {} bytes", .{compressed_size});
    try stdout.print("Uncompressed size: {} bytes", .{uncompressed_size});
    try stdout.print("Compression ratio: {d:.2}:1", .{@as(f64, @floatFromInt(uncompressed_size)) / @as(f64, @floatFromInt(compressed_size))});
    try stdout.print("Time: {} ms", .{elapsed_ms});
}

fn rocUnbundle(allocator: Allocator, args: cli_args.UnbundleArgs) !void {
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
            try stderr.print("Error: {s} is not a .tar.zst file", .{archive_path});
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
            try stderr.print("Error: Directory {s} already exists", .{dir_name});
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
            try stderr.print("Error opening {s}: {s}", .{ archive_path, @errorName(err) });
            had_errors = true;
            continue;
        };
        defer archive_file.close();

        // Unbundle the archive
        var error_ctx: unbundle.ErrorContext = undefined;
        unbundle.unbundleFiles(
            allocator,
            archive_file.reader(),
            output_dir,
            basename,
            &error_ctx,
        ) catch |err| {
            switch (err) {
                error.HashMismatch => {
                    try stderr.print("Error: Hash mismatch for {s} - file may be corrupted", .{archive_path});
                    had_errors = true;
                },
                error.InvalidFilename => {
                    try stderr.print("Error: Invalid filename format for {s}", .{archive_path});
                    had_errors = true;
                },
                error.InvalidPath => {
                    try stderr.print("Error: Invalid path in archive - {s}", .{formatUnbundlePathValidationReason(error_ctx.reason)});
                    try stderr.print("Path: {s}", .{error_ctx.path});
                    try stderr.print("Archive: {s}", .{archive_path});
                    had_errors = true;
                },
                else => {
                    try stderr.print("Error unbundling {s}: {s}", .{ archive_path, @errorName(err) });
                    had_errors = true;
                },
            }
            continue; // Skip success message on error
        };

        try stdout.print("Extracted: {s}", .{dir_name});
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

    // Import needed modules
    const target_mod = @import("target.zig");
    const app_stub = @import("app_stub.zig");

    std.log.info("Building {s} for cross-compilation", .{args.path});

    // Parse target if provided, otherwise use native with musl preference
    const target = if (args.target) |target_str| blk: {
        break :blk target_mod.RocTarget.fromString(target_str) orelse {
            std.log.err("Invalid target: {s}", .{target_str});
            std.log.err("Valid targets: x64musl, x64glibc, arm64musl, arm64glibc, etc.", .{});
            std.process.exit(1);
        };
    } else target_mod.RocTarget.detectNative();

    std.log.info("Target: {} ({s})", .{ target, target.toTriple() });

    // Only support test platforms for now (int and str)
    const platform_type = if (std.mem.indexOf(u8, args.path, "/int/") != null)
        "int"
    else if (std.mem.indexOf(u8, args.path, "/str/") != null)
        "str"
    else {
        std.log.err("roc build currently only supports test platforms (int and str)", .{});
        std.log.err("Your app path: {s}", .{args.path});
        std.process.exit(1);
    };

    std.log.info("Detected platform type: {s}", .{platform_type});

    // Get platform directory path
    const platform_dir = if (std.mem.eql(u8, platform_type, "int"))
        try std.fs.path.join(gpa, &.{ "test", "int", "platform" })
    else
        try std.fs.path.join(gpa, &.{ "test", "str", "platform" });
    defer gpa.free(platform_dir);

    // Check that platform exists
    std.fs.cwd().access(platform_dir, .{}) catch |err| {
        std.log.err("Platform directory not found: {s} ({})", .{ platform_dir, err });
        std.process.exit(1);
    };

    // Get host library path
    const host_lib_filename = if (builtin.target.os.tag == .windows) "host.lib" else "libhost.a";
    const host_lib_path = try std.fs.path.join(gpa, &.{ platform_dir, host_lib_filename });
    defer gpa.free(host_lib_path);

    std.fs.cwd().access(host_lib_path, .{}) catch |err| {
        std.log.err("Host library not found: {s} ({})", .{ host_lib_path, err });
        std.process.exit(1);
    };

    // Get expected entrypoints for this platform
    const entrypoints = try app_stub.getTestPlatformEntrypoints(gpa, platform_type);
    defer gpa.free(entrypoints);

    std.log.info("Expected entrypoints: {}", .{entrypoints.len});
    for (entrypoints, 0..) |ep, i| {
        std.log.info("  {}: roc__{s}", .{ i, ep.name });
    }

    // Create temp directory for build artifacts
    const temp_dir = try std.fs.path.join(gpa, &.{ "zig-cache", "roc_build" });
    defer gpa.free(temp_dir);

    std.fs.cwd().makePath(temp_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Generate app stub object file
    const app_stub_obj = try app_stub.generateAppStubObject(gpa, temp_dir, entrypoints, target);
    defer gpa.free(app_stub_obj);

    // Get CRT files for the target
    const crt_files = try target_mod.getVendoredCRTFiles(gpa, target, platform_dir);

    // Create object files list for linking
    var object_files = std.ArrayList([]const u8).init(gpa);
    defer object_files.deinit();

    // Add CRT files in correct order
    if (crt_files.crt1_o) |crt1| try object_files.append(crt1);
    if (crt_files.crti_o) |crti| try object_files.append(crti);

    // Add our app stub and host library
    try object_files.append(app_stub_obj);
    try object_files.append(host_lib_path);

    // Add libc.a for static linking
    if (crt_files.libc_a) |libc| try object_files.append(libc);
    if (crt_files.crtn_o) |crtn| try object_files.append(crtn);

    // Determine output path
    const output_path = if (args.output) |output|
        try gpa.dupe(u8, output)
    else blk: {
        const basename = std.fs.path.basename(args.path);
        const name_without_ext = if (std.mem.endsWith(u8, basename, ".roc"))
            basename[0 .. basename.len - 4]
        else
            basename;
        break :blk try gpa.dupe(u8, name_without_ext);
    };
    defer gpa.free(output_path);

    // Use LLD for linking
    const linker_mod = @import("linker.zig");
    const target_abi = if (target.isStatic()) linker_mod.TargetAbi.musl else linker_mod.TargetAbi.gnu;
    const link_config = linker_mod.LinkConfig{
        .object_files = object_files.items,
        .output_path = output_path,
        .target_abi = target_abi,
    };

    try linker_mod.link(gpa, link_config);

    std.log.info("Successfully built executable: {s}", .{output_path});
}

/// Information about a test (expect statement) to be evaluated
const ExpectTest = struct {
    expr_idx: can.CIR.Expr.Idx,
    region: base.Region,
};

/// Simple test environment for evaluating expects
const TestOpsEnv = struct {
    allocator: Allocator,
    interpreter: ?*Interpreter,
    roc_ops: ?RocOps,

    fn init(allocator: Allocator) TestOpsEnv {
        return TestOpsEnv{
            .allocator = allocator,
            .interpreter = null,
            .roc_ops = null,
        };
    }

    fn setInterpreter(self: *TestOpsEnv, interp: *Interpreter) void {
        self.interpreter = interp;
    }

    fn get_ops(self: *TestOpsEnv) *RocOps {
        if (self.roc_ops == null) {
            self.roc_ops = RocOps{
                .env = @ptrCast(self),
                .roc_alloc = testRocAlloc,
                .roc_dealloc = testRocDealloc,
                .roc_realloc = testRocRealloc,
                .roc_dbg = testRocDbg,
                .roc_expect_failed = testRocExpectFailed,
                .roc_crashed = testRocCrashed,
                .host_fns = undefined, // Not used in tests
            };
        }
        return &(self.roc_ops.?);
    }

    fn deinit(self: *TestOpsEnv) void {
        if (self.interpreter) |interp| {
            if (interp.crash_message) |msg| {
                // Only free if we allocated it (not a string literal)
                if (!std.mem.eql(u8, msg, "Failed to store crash message")) {
                    self.allocator.free(msg);
                }
            }
        }
    }
};

fn testRocAlloc(alloc_args: *RocAlloc, env: *anyopaque) callconv(.C) void {
    const test_env: *TestOpsEnv = @ptrCast(@alignCast(env));
    const align_enum = std.mem.Alignment.fromByteUnits(@as(usize, @intCast(alloc_args.alignment)));
    const size_storage_bytes = @max(alloc_args.alignment, @alignOf(usize));
    const total_size = alloc_args.length + size_storage_bytes;
    const result = test_env.allocator.rawAlloc(total_size, align_enum, @returnAddress());
    const base_ptr = result orelse {
        std.debug.panic("Out of memory during testRocAlloc", .{});
    };
    const size_ptr: *usize = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes - @sizeOf(usize));
    size_ptr.* = total_size;
    alloc_args.answer = @ptrFromInt(@intFromPtr(base_ptr) + size_storage_bytes);
}

fn testRocDealloc(dealloc_args: *RocDealloc, env: *anyopaque) callconv(.C) void {
    const test_env: *TestOpsEnv = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(dealloc_args.alignment, @alignOf(usize));
    const size_ptr: *const usize = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - @sizeOf(usize));
    const total_size = size_ptr.*;
    const base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(dealloc_args.ptr) - size_storage_bytes);
    const log2_align = std.math.log2_int(u32, @intCast(dealloc_args.alignment));
    const align_enum: std.mem.Alignment = @enumFromInt(log2_align);
    const slice = @as([*]u8, @ptrCast(base_ptr))[0..total_size];
    test_env.allocator.rawFree(slice, align_enum, @returnAddress());
}

fn testRocRealloc(realloc_args: *RocRealloc, env: *anyopaque) callconv(.C) void {
    const test_env: *TestOpsEnv = @ptrCast(@alignCast(env));
    const size_storage_bytes = @max(realloc_args.alignment, @alignOf(usize));
    const old_size_ptr: *const usize = @ptrFromInt(@intFromPtr(realloc_args.answer) - @sizeOf(usize));
    const old_total_size = old_size_ptr.*;
    const old_base_ptr: [*]u8 = @ptrFromInt(@intFromPtr(realloc_args.answer) - size_storage_bytes);
    const new_total_size = realloc_args.new_length + size_storage_bytes;
    const old_slice = @as([*]u8, @ptrCast(old_base_ptr))[0..old_total_size];
    const new_slice = test_env.allocator.realloc(old_slice, new_total_size) catch {
        std.debug.panic("Out of memory during testRocRealloc", .{});
    };
    const new_size_ptr: *usize = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes - @sizeOf(usize));
    new_size_ptr.* = new_total_size;
    realloc_args.answer = @ptrFromInt(@intFromPtr(new_slice.ptr) + size_storage_bytes);
}

fn testRocDbg(dbg_args: *const RocDbg, env: *anyopaque) callconv(.C) void {
    _ = dbg_args;
    _ = env;
    @panic("testRocDbg not implemented yet");
}

fn testRocExpectFailed(expect_args: *const RocExpectFailed, env: *anyopaque) callconv(.C) void {
    _ = expect_args;
    _ = env;
    @panic("testRocExpectFailed not implemented yet");
}

fn testRocCrashed(crashed_args: *const RocCrashed, env: *anyopaque) callconv(.C) void {
    const test_env: *TestOpsEnv = @ptrCast(@alignCast(env));
    const msg_slice = crashed_args.utf8_bytes[0..crashed_args.len];
    if (test_env.interpreter) |interp| {
        interp.has_crashed = true;
        const owned_msg = test_env.allocator.dupe(u8, msg_slice) catch |err| {
            std.log.err("Failed to allocate crash message: {}", .{err});
            interp.crash_message = "Failed to store crash message";
            return;
        };
        interp.crash_message = owned_msg;
    }
}

fn rocTest(gpa: Allocator, args: cli_args.TestArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Start timing
    const start_time = std.time.nanoTimestamp();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    // Read the Roc file
    const source = std.fs.cwd().readFileAlloc(gpa, args.path, std.math.maxInt(usize)) catch |err| {
        try stderr.print("Failed to read file '{s}': {}", .{ args.path, err });
        std.process.exit(1);
    };
    defer gpa.free(source);

    // Extract module name from the file path
    const basename = std.fs.path.basename(args.path);
    const module_name = try gpa.dupe(u8, basename);
    defer gpa.free(module_name);

    // Create ModuleEnv
    var env = ModuleEnv.init(gpa, source) catch |err| {
        try stderr.print("Failed to initialize module environment: {}", .{err});
        std.process.exit(1);
    };
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(gpa);

    // Parse the source code as a full module
    var parse_ast = parse.parse(&env.common, gpa) catch |err| {
        try stderr.print("Failed to parse file: {}", .{err});
        std.process.exit(1);
    };
    defer parse_ast.deinit(gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(gpa, module_name);

    // Create canonicalizer
    var canonicalizer = Can.init(&env, &parse_ast, null) catch |err| {
        try stderr.print("Failed to initialize canonicalizer: {}", .{err});
        std.process.exit(1);
    };
    defer canonicalizer.deinit();

    // Canonicalize the entire module
    canonicalizer.canonicalizeFile() catch |err| {
        try stderr.print("Failed to canonicalize file: {}", .{err});
        std.process.exit(1);
    };

    // Type check the module
    var checker = Check.init(gpa, &env.types, &env, &.{}, &env.store.regions) catch |err| {
        try stderr.print("Failed to initialize type checker: {}", .{err});
        std.process.exit(1);
    };
    defer checker.deinit();

    checker.checkDefs() catch |err| {
        try stderr.print("Type checking failed: {}", .{err});
        std.process.exit(1);
    };

    // Find all expect statements
    const statements = env.store.sliceStatements(env.all_statements);
    var expects = std.ArrayList(ExpectTest).init(gpa);
    defer expects.deinit();

    for (statements) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        if (stmt == .s_expect) {
            const region = env.store.getStatementRegion(stmt_idx);
            try expects.append(.{
                .expr_idx = stmt.s_expect.body,
                .region = region,
            });
        }
    }

    if (expects.items.len == 0) {
        try stdout.print("No tests found in {s}", .{args.path});
        return;
    }

    // Create interpreter infrastructure for test evaluation
    var stack_memory = eval.Stack.initCapacity(gpa, 1024) catch |err| {
        try stderr.print("Failed to create stack memory: {}", .{err});
        std.process.exit(1);
    };
    defer stack_memory.deinit();

    var layout_cache = LayoutStore.init(&env, &env.types) catch |err| {
        try stderr.print("Failed to create layout cache: {}", .{err});
        std.process.exit(1);
    };
    defer layout_cache.deinit();

    var test_env = TestOpsEnv.init(gpa);
    defer test_env.deinit();

    var interpreter = Interpreter.init(gpa, &env, &stack_memory, &layout_cache, &env.types) catch |err| {
        try stderr.print("Failed to create interpreter: {}", .{err});
        std.process.exit(1);
    };
    defer interpreter.deinit(test_env.get_ops());
    test_env.setInterpreter(&interpreter);

    // Track test results for verbose output
    const TestResult = struct {
        line_number: u32,
        passed: bool,
        error_msg: ?[]const u8 = null,
    };

    var test_results = std.ArrayList(TestResult).init(gpa);
    defer test_results.deinit();

    // Evaluate each expect statement
    var passed: u32 = 0;
    var failed: u32 = 0;

    for (expects.items) |expect_test| {
        const region_info = env.calcRegionInfo(expect_test.region);
        const line_number = region_info.start_line_idx + 1;

        // Evaluate the expect expression
        const result = interpreter.eval(expect_test.expr_idx, test_env.get_ops()) catch |err| {
            const error_msg = try std.fmt.allocPrint(gpa, "Test evaluation failed: {}", .{err});
            try test_results.append(.{ .line_number = line_number, .passed = false, .error_msg = error_msg });
            failed += 1;
            continue;
        };

        // Check if the result is a boolean true
        if (result.layout.tag == .scalar and result.layout.data.scalar.tag == .bool) {
            const is_true = result.asBool();
            if (is_true) {
                try test_results.append(.{ .line_number = line_number, .passed = true });
                passed += 1;
            } else {
                try test_results.append(.{ .line_number = line_number, .passed = false });
                failed += 1;
            }
        } else {
            const error_msg = try gpa.dupe(u8, "Test did not evaluate to a boolean");
            try test_results.append(.{ .line_number = line_number, .passed = false, .error_msg = error_msg });
            failed += 1;
        }
    }

    // Calculate elapsed time
    const end_time = std.time.nanoTimestamp();
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    // Free allocated error messages
    defer for (test_results.items) |test_result| {
        if (test_result.error_msg) |msg| {
            gpa.free(msg);
        }
    };

    // Report results
    if (failed == 0) {
        // Success case: only print if verbose, exit with 0
        if (args.verbose) {
            try stdout.print("Ran {} test(s): {} passed, 0 failed in {d:.1}ms", .{ passed, passed, elapsed_ms });
            for (test_results.items) |test_result| {
                try stdout.print("PASS: line {}", .{test_result.line_number});
            }
        }
        // Otherwise print nothing at all
        return; // Exit with 0
    } else {
        // Failure case: always print summary with timing
        try stderr.print("Ran {} test(s): {} passed, {} failed in {d:.1}ms", .{ passed + failed, passed, failed, elapsed_ms });

        if (args.verbose) {
            for (test_results.items) |test_result| {
                if (test_result.passed) {
                    try stderr.print("PASS: line {}", .{test_result.line_number});
                } else {
                    if (test_result.error_msg) |msg| {
                        try stderr.print("FAIL: line {} - {s}", .{ test_result.line_number, msg });
                    } else {
                        try stderr.print("FAIL: line {}", .{test_result.line_number});
                    }
                }
            }
        }

        std.process.exit(1);
    }
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
            try stdout.writer().print("The following file(s) failed `roc format --check`:", .{});
            for (unformatted_files.items) |file_name| {
                try stdout.writer().print("    {s}", .{file_name});
            }
            try stdout.writer().print("You can fix this with `roc format FILENAME.roc`.", .{});
            exit_code = 1;
        } else {
            try stdout.writer().print("All formatting valid", .{});
        }
        if (failure_count > 0) {
            try stdout.writer().print("Failed to check {} files.", .{failure_count});
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
        try stdout.writer().print("Successfully formatted {} files", .{success_count});
        if (failure_count > 0) {
            try stdout.writer().print("Failed to format {} files.", .{failure_count});
            exit_code = 1;
        }
    }

    try stdout.writer().print("Took ", .{});
    try formatElapsedTime(stdout.writer(), elapsed);
    try stdout.writer().print(".", .{});

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
        // Custom BuildEnv errors - these need special messages
        error.ExpectedAppHeader => stderr.print("Expected app header but found different header type", .{}) catch {},
        error.ExpectedPlatformString => stderr.print("Expected platform string in header", .{}) catch {},
        error.PathOutsideWorkspace => stderr.print("Dependency path outside workspace not allowed", .{}) catch {},
        error.UnsupportedHeader => stderr.print("Unsupported header type", .{}) catch {},
        error.ExpectedString => stderr.print("Expected string in header", .{}) catch {},
        error.Internal => stderr.print("Internal compiler error", .{}) catch {},
        error.InvalidDependency => stderr.print("Invalid dependency relationship", .{}) catch {},
        error.TooNested => stderr.print("Too deeply nested", .{}) catch {},
        error.InvalidPackageName => stderr.print("Invalid package name", .{}) catch {},

        // Catch-all for any other errors
        else => stderr.print("{s}", .{@errorName(err)}) catch {},
    }
    std.process.exit(1);
}

/// Result from checking a file using BuildEnv
const CheckResult = struct {
    reports: []DrainedReport,
    timing: CheckTimingInfo = if (builtin.target.cpu.arch == .wasm32) .{} else .{
        .tokenize_parse_ns = 0,
        .canonicalize_ns = 0,
        .canonicalize_diagnostics_ns = 0,
        .type_checking_ns = 0,
        .check_diagnostics_ns = 0,
    },
    was_cached: bool = false,
    error_count: u32 = 0,
    warning_count: u32 = 0,

    /// Free allocated memory
    pub fn deinit(self: *CheckResult, gpa: Allocator) void {
        for (self.reports) |*report| {
            report.deinit(gpa);
        }
        gpa.free(self.reports);
    }
};

/// Drained report with module info and file path
const DrainedReport = struct {
    file_path: []const u8,
    reports: []reporting.Report,

    pub fn deinit(self: *DrainedReport, gpa: Allocator) void {
        gpa.free(self.file_path);
        for (self.reports) |*report| {
            report.deinit();
        }
        gpa.free(self.reports);
    }
};

/// Timing information for check phases
const CheckTimingInfo = if (builtin.target.cpu.arch == .wasm32) struct {} else TimingInfo;

/// Error set for BuildEnv.build operations
const BuildAppError = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.ReadError || std.fs.File.WriteError || std.Thread.SpawnError || error{
    // Custom BuildEnv errors
    ExpectedAppHeader,
    ExpectedPlatformString,
    PathOutsideWorkspace,
    UnsupportedHeader,
    ExpectedString,
    Internal,
    InvalidDependency,
    TooNested,
    InvalidPackageName,
    InvalidNullByteInPath,
    // Additional errors from std library that might be missing
    Unseekable,
    CurrentWorkingDirectoryUnlinked,
};

/// Check a Roc file using the BuildEnv system
fn checkFileWithBuildEnv(
    gpa: Allocator,
    filepath: []const u8,
    collect_timing: bool,
    cache_config: CacheConfig,
) BuildAppError!CheckResult {
    _ = collect_timing; // Timing is always collected by BuildEnv
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize BuildEnv in single-threaded mode for checking
    var build_env = BuildEnv.init(gpa, .single_threaded, 1);
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(gpa, cache_config, Filesystem.default());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager
    }

    // Build the file (works for both app and module files)
    try build_env.build(filepath);

    // Drain all reports
    const drained = try build_env.drainReports();

    // Count errors and warnings
    var error_count: u32 = 0;
    var warning_count: u32 = 0;

    for (drained) |mod| {
        for (mod.reports) |report| {
            switch (report.severity) {
                .info => {},
                .runtime_error, .fatal => error_count += 1,
                .warning => warning_count += 1,
            }
        }
    }

    // Convert BuildEnv drained reports to our format
    var reports = try gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try gpa.dupe(u8, mod.abs_path),
            .reports = try gpa.dupe(reporting.Report, mod.reports),
        };
    }

    // Free the original drained reports
    // Note: abs_path is owned by BuildEnv, reports are moved to our array
    gpa.free(drained);

    // Get timing information from BuildEnv
    const timing = if (builtin.target.cpu.arch == .wasm32)
        CheckTimingInfo{}
    else
        build_env.getTimingInfo();

    return CheckResult{
        .reports = reports,
        .timing = timing,
        .was_cached = false, // BuildEnv doesn't currently expose cache info
        .error_count = error_count,
        .warning_count = warning_count,
    };
}

fn rocCheck(gpa: Allocator, args: cli_args.CheckArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();
    const stderr_writer = stderr.any();

    var timer = try std.time.Timer.start();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    // Use BuildEnv to check the file
    var check_result = checkFileWithBuildEnv(
        gpa,
        args.path,
        args.time,
        cache_config,
    ) catch |err| {
        handleProcessFileError(err, stderr, args.path);
    };

    defer check_result.deinit(gpa);

    const elapsed = timer.read();

    // Handle cached results vs fresh compilation results differently
    if (check_result.was_cached) {
        // For cached results, use the stored diagnostic counts
        const total_errors = check_result.error_count;
        const total_warnings = check_result.warning_count;

        if (total_errors > 0 or total_warnings > 0) {
            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                total_errors,
                total_warnings,
            }) catch {};
            formatElapsedTime(stderr, elapsed) catch {};
            stderr.print(" for {s} (note module loaded from cache, use --no-cache to display Errors and Warnings.).", .{args.path}) catch {};
            std.process.exit(1);
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s} (loaded from cache)", .{args.path}) catch {};
        }
    } else {
        // For fresh compilation, process and display reports normally
        var has_errors = false;

        // Render reports grouped by module
        for (check_result.reports) |module| {
            for (module.reports) |*report| {

                // Render the diagnostic report to stderr
                reporting.renderReportToTerminal(report, stderr_writer, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
                    stderr.print("Error rendering diagnostic report: {}", .{render_err}) catch {};
                    // Fallback to just printing the title
                    stderr.print("  {s}", .{report.title}) catch {};
                };

                if (report.severity == .fatal or report.severity == .runtime_error) {
                    has_errors = true;
                }
            }
        }

        if (check_result.error_count > 0 or check_result.warning_count > 0) {
            stderr.writeAll("\n") catch {};
            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                check_result.error_count,
                check_result.warning_count,
            }) catch {};
            formatElapsedTime(stderr, elapsed) catch {};
            stderr.print(" for {s}.", .{args.path}) catch {};

            std.process.exit(1);
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s}", .{args.path}) catch {};
        }
    }

    // Print timing breakdown if requested
    if (args.time) {
        printTimingBreakdown(stdout, if (builtin.target.cpu.arch == .wasm32) null else check_result.timing);
    }
}

fn printTimingBreakdown(writer: anytype, timing: ?CheckTimingInfo) void {
    if (timing) |t| {
        writer.print("\nTiming breakdown:", .{}) catch {};
        writer.print("  tokenize + parse:             ", .{}) catch {};
        formatElapsedTime(writer, t.tokenize_parse_ns) catch {};
        writer.print("  ({} ns)", .{t.tokenize_parse_ns}) catch {};
        writer.print("  canonicalize:                 ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_ns) catch {};
        writer.print("  ({} ns)", .{t.canonicalize_ns}) catch {};
        writer.print("  can diagnostics:              ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_diagnostics_ns) catch {};
        writer.print("  ({} ns)", .{t.canonicalize_diagnostics_ns}) catch {};
        writer.print("  type checking:                ", .{}) catch {};
        formatElapsedTime(writer, t.type_checking_ns) catch {};
        writer.print("  ({} ns)", .{t.type_checking_ns}) catch {};
        writer.print("  type checking diagnostics:    ", .{}) catch {};
        formatElapsedTime(writer, t.check_diagnostics_ns) catch {};
        writer.print("  ({} ns)", .{t.check_diagnostics_ns}) catch {};
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
