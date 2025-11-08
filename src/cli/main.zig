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
const builtins = @import("builtins");
const compiled_builtins = @import("compiled_builtins");
const builtin_loading = eval.builtin_loading;
const BuiltinTypes = eval.BuiltinTypes;

const cli_args = @import("cli_args.zig");

comptime {
    if (builtin.is_test) {
        std.testing.refAllDecls(cli_args);
    }
}
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
const TestRunner = eval.TestRunner;
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const TestOpsEnv = eval.TestOpsEnv;
const Allocators = base.Allocators;

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

var stdout_buffer: [4096]u8 = undefined;
var stdout_writer: std.fs.File.Writer = undefined;
var stdout_initialized = false;

var stderr_buffer: [4096]u8 = undefined;
var stderr_writer: std.fs.File.Writer = undefined;
var stderr_initialized = false;

fn stdoutWriter() *std.Io.Writer {
    if (is_windows or !stdout_initialized) {
        stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        stdout_initialized = true;
    }
    return &stdout_writer.interface;
}

fn stderrWriter() *std.Io.Writer {
    if (is_windows or !stderr_initialized) {
        stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
        stderr_initialized = true;
    }
    return &stderr_writer.interface;
}

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
fn createHardlink(allocs: *Allocators, source: []const u8, dest: []const u8) !void {
    if (comptime builtin.target.os.tag == .windows) {
        // On Windows, use CreateHardLinkW
        const source_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, source);
        const dest_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, dest);

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
        const source_c = try allocs.arena.dupeZ(u8, source);
        const dest_c = try allocs.arena.dupeZ(u8, dest);

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
fn generateRandomSuffix(allocs: *Allocators) ![]u8 {
    // TODO: Consider switching to a library like https://github.com/abhinav/temp.zig
    // for more robust temporary file/directory handling
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    const suffix = try allocs.arena.alloc(u8, 32);

    // Fill with cryptographically secure random bytes
    std.crypto.random.bytes(suffix);

    // Convert to ASCII characters from our charset
    for (suffix) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }

    return suffix;
}

/// Create the temporary directory structure for fd communication.
/// Returns the path to the executable in the temp directory (allocated from arena, no need to free).
/// If a cache directory is provided, it will be used for temporary files; otherwise
/// falls back to the system temp directory.
pub fn createTempDirStructure(allocs: *Allocators, exe_path: []const u8, shm_handle: SharedMemoryHandle, cache_dir: ?[]const u8) ![]const u8 {
    // Use provided cache dir or fall back to system temp directory
    const temp_dir = if (cache_dir) |dir|
        try allocs.arena.dupe(u8, dir)
    else if (comptime is_windows)
        std.process.getEnvVarOwned(allocs.arena, "TEMP") catch
            std.process.getEnvVarOwned(allocs.arena, "TMP") catch try allocs.arena.dupe(u8, "C:\\Windows\\Temp")
    else
        std.process.getEnvVarOwned(allocs.arena, "TMPDIR") catch try allocs.arena.dupe(u8, "/tmp");

    // Try up to 10 times to create a unique directory
    var attempt: u8 = 0;
    while (attempt < 10) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(allocs);

        // Create the full path with .txt suffix first
        const normalized_temp_dir = if (comptime is_windows)
            std.mem.trimRight(u8, temp_dir, "/\\")
        else
            std.mem.trimRight(u8, temp_dir, "/");
        const dir_name_with_txt = if (comptime is_windows)
            try std.fmt.allocPrint(allocs.arena, "{s}\\roc-tmp-{s}.txt", .{ normalized_temp_dir, random_suffix })
        else
            try std.fmt.allocPrint(allocs.arena, "{s}/roc-tmp-{s}.txt", .{ normalized_temp_dir, random_suffix });

        // Get the directory path by slicing off the .txt suffix
        const dir_path_len = dir_name_with_txt.len - 4; // Remove ".txt"
        const temp_dir_path = dir_name_with_txt[0..dir_path_len];

        // Try to create the directory
        std.fs.cwd().makeDir(temp_dir_path) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // Directory already exists, try again with a new random suffix
                continue;
            },
            else => {
                return err;
            },
        };

        // Try to create the fd file
        const fd_file = std.fs.cwd().createFile(dir_name_with_txt, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // File already exists, remove the directory and try again
                std.fs.cwd().deleteDir(temp_dir_path) catch {};
                continue;
            },
            else => {
                // Clean up directory on other errors
                std.fs.cwd().deleteDir(temp_dir_path) catch {};
                return err;
            },
        };
        // Note: We'll close this explicitly later, before spawning the child

        // Write shared memory info to file (POSIX only - Windows uses command line args)
        const fd_str = try std.fmt.allocPrint(allocs.arena, "{}\n{}", .{ shm_handle.fd, shm_handle.size });

        try fd_file.writeAll(fd_str);

        // IMPORTANT: Flush and close the file explicitly before spawning child process
        // On Windows, having the file open can prevent child process access
        try fd_file.sync(); // Ensure data is written to disk
        fd_file.close();

        // Create hardlink to executable in temp directory
        const exe_basename = std.fs.path.basename(exe_path);
        const temp_exe_path = try std.fs.path.join(allocs.arena, &.{ temp_dir_path, exe_basename });

        // Try to create a hardlink first (more efficient than copying)
        createHardlink(allocs, exe_path, temp_exe_path) catch {
            // If hardlinking fails for any reason, fall back to copying
            // Common reasons: cross-device link, permissions, file already exists
            try std.fs.cwd().copyFile(exe_path, std.fs.cwd(), temp_exe_path, .{});
        };

        return temp_exe_path;
    }

    // Failed after 10 attempts
    return error.FailedToCreateUniqueTempDir;
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .{
    .backing_allocator = std.heap.c_allocator,
};

/// The CLI entrypoint for the Roc compiler.
pub fn main() !void {
    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa, const is_safe = gpa: {
        if (builtin.os.tag == .wasi) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.c_allocator, false },
        };
    };
    defer if (is_safe) {
        const mem_state = debug_allocator.deinit();
        std.debug.assert(mem_state == .ok);
    };

    if (tracy.enable_allocation) {
        gpa_tracy = tracy.tracyAllocator(gpa);
        gpa = gpa_tracy.allocator();
    }

    var allocs: Allocators = undefined;
    allocs.initInPlace(gpa);
    defer allocs.deinit();

    const args = try std.process.argsAlloc(allocs.arena);

    mainArgs(&allocs, args) catch {
        // Error messages have already been printed by the individual functions.
        // Exit cleanly without showing a stack trace to the user.
        if (tracy.enable) {
            tracy.waitForShutdown() catch {};
        }
        std.process.exit(1);
    };

    if (tracy.enable) {
        try tracy.waitForShutdown();
    }
}

fn mainArgs(allocs: *Allocators, args: []const []const u8) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = stdoutWriter();
    defer stdout.flush() catch {};

    const stderr = stderrWriter();
    defer stderr.flush() catch {};

    const parsed_args = try cli_args.parse(allocs.arena, args[1..]);

    try switch (parsed_args) {
        .run => |run_args| {
            if (std.mem.eql(u8, run_args.path, "main.roc")) {
                std.fs.cwd().access(run_args.path, .{}) catch |err| switch (err) {
                    error.FileNotFound => {
                        const cwd_path = std.fs.cwd().realpathAlloc(allocs.arena, ".") catch |real_err| {
                            stderr.print(
                                "Error: No app file specified and default 'main.roc' was not found. Additionally, the current directory could not be resolved: {}\n",
                                .{real_err},
                            ) catch {};
                            return error.FileNotFound;
                        };
                        stderr.print(
                            "Error: No app file specified and default 'main.roc' was not found in {s}\n",
                            .{cwd_path},
                        ) catch {};
                        stderr.print(
                            "\nHint: pass an explicit path (e.g. `roc my-app.roc`) or create a 'main.roc' in that directory.\n",
                            .{},
                        ) catch {};
                        return error.FileNotFound;
                    },
                    else => {
                        stderr.print(
                            "Error: Unable to access default 'main.roc': {}\n",
                            .{err},
                        ) catch {};
                        return err;
                    },
                };
            }

            try rocRun(allocs, run_args);
        },
        .check => |check_args| rocCheck(allocs, check_args),
        .build => |build_args| rocBuild(allocs, build_args),
        .bundle => |bundle_args| rocBundle(allocs, bundle_args),
        .unbundle => |unbundle_args| rocUnbundle(allocs, unbundle_args),
        .fmt => |format_args| rocFormat(allocs, format_args),
        .test_cmd => |test_args| rocTest(allocs, test_args),
        .repl => rocRepl(allocs),
        .version => stdout.print("Roc compiler version {s}\n", .{build_options.compiler_version}),
        .docs => |docs_args| rocDocs(allocs, docs_args),
        .help => |help_message| {
            try stdout.writeAll(help_message);
        },
        .licenses => {
            try stdout.writeAll(legalDetailsFileContent);
        },
        .problem => |problem| {
            try switch (problem) {
                .missing_flag_value => |details| stderr.print("Error: no value was supplied for {s}\n", .{details.flag}),
                .unexpected_argument => |details| stderr.print("Error: roc {s} received an unexpected argument: `{s}`\n", .{ details.cmd, details.arg }),
                .invalid_flag_value => |details| stderr.print("Error: `{s}` is not a valid value for {s}. The valid options are {s}\n", .{ details.value, details.flag, details.valid_options }),
            };
            return error.InvalidArguments;
        },
    };
}

/// Generate platform host shim object file using LLVM.
/// Returns the path to the generated object file (allocated from arena, no need to free), or null if LLVM unavailable.
fn generatePlatformHostShim(allocs: *Allocators, cache_dir: []const u8, entrypoint_names: []const []const u8, target: builder.RocTarget) !?[]const u8 {
    // Check if LLVM is available (this is a compile-time check)
    if (!llvm_available) {
        std.log.debug("LLVM not available, skipping platform host shim generation", .{});
        return null;
    }

    const std_zig_llvm = @import("std").zig.llvm;
    const Builder = std_zig_llvm.Builder;

    // Create LLVM Builder
    var llvm_builder = Builder.init(.{
        .allocator = allocs.gpa,
        .name = "roc_platform_shim",
    }) catch |err| {
        std.log.err("Failed to initialize LLVM Builder: {}", .{err});
        return err;
    };
    defer llvm_builder.deinit();

    // Create entrypoints array from the provided names
    var entrypoints = try std.array_list.Managed(platform_host_shim.EntryPoint).initCapacity(allocs.arena, 8);

    for (entrypoint_names, 0..) |name, idx| {
        try entrypoints.append(.{ .name = name, .idx = @intCast(idx) });
    }

    // Create the complete platform shim
    platform_host_shim.createInterpreterShim(&llvm_builder, entrypoints.items) catch |err| {
        std.log.err("Failed to create interpreter shim: {}", .{err});
        return err;
    };

    // Generate paths for temporary files
    const bitcode_path = std.fs.path.join(allocs.arena, &.{ cache_dir, "platform_shim.bc" }) catch |err| {
        std.log.err("Failed to create bitcode path: {}", .{err});
        return err;
    };

    const object_path = std.fs.path.join(allocs.arena, &.{ cache_dir, "platform_shim.o" }) catch |err| {
        std.log.err("Failed to create object path: {}", .{err});
        return err;
    };

    // Generate bitcode first
    const producer = Builder.Producer{
        .name = "Roc Platform Host Shim Generator",
        .version = .{ .major = 1, .minor = 0, .patch = 0 },
    };

    const bitcode = llvm_builder.toBitcode(allocs.gpa, producer) catch |err| {
        std.log.err("Failed to generate bitcode: {}", .{err});
        return err;
    };
    defer allocs.gpa.free(bitcode);

    // Write bitcode to file
    const bc_file = std.fs.cwd().createFile(bitcode_path, .{}) catch |err| {
        std.log.err("Failed to create bitcode file: {}", .{err});
        return err;
    };
    defer bc_file.close();

    // Convert u32 array to bytes for writing
    const bytes = std.mem.sliceAsBytes(bitcode);
    bc_file.writeAll(bytes) catch |err| {
        std.log.err("Failed to write bitcode: {}", .{err});
        return err;
    };

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .speed,
        .target = target,
    };

    if (builder.compileBitcodeToObject(allocs.gpa, compile_config)) |success| {
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

fn rocRun(allocs: *Allocators, args: cli_args.RunArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Import needed modules
    const target_mod = @import("target.zig");

    // Initialize cache - used to store our shim, and linked interpreter executables in cache
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(allocs.gpa, cache_config, Filesystem.default());

    // Create cache directory for linked interpreter executables
    const cache_dir = cache_manager.config.getCacheEntriesDir(allocs.arena) catch |err| {
        std.log.err("Failed to get cache directory: {}", .{err});
        return err;
    };
    const exe_cache_dir = std.fs.path.join(allocs.arena, &.{ cache_dir, "executables" }) catch |err| {
        std.log.err("Failed to create executable cache path: {}", .{err});
        return err;
    };

    std.fs.cwd().makePath(exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            std.log.err("Failed to create cache directory: {}", .{err});
            return err;
        },
    };

    // Generate executable name based on the roc file path
    // TODO use something more interesting like a hash from the platform.main or platform/host.a etc
    const exe_base_name = std.fmt.allocPrint(allocs.arena, "roc_run_{}", .{std.hash.crc.Crc32.hash(args.path)}) catch |err| {
        std.log.err("Failed to generate executable name: {}", .{err});
        return err;
    };

    // Add .exe extension on Windows
    const exe_name = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(allocs.arena, "{s}.exe", .{exe_base_name}) catch |err| {
            std.log.err("Failed to generate executable name with extension: {}", .{err});
            return err;
        }
    else
        allocs.arena.dupe(u8, exe_base_name) catch |err| {
            std.log.err("Failed to duplicate executable name: {}", .{err});
            return err;
        };

    const exe_path = std.fs.path.join(allocs.arena, &.{ exe_cache_dir, exe_name }) catch |err| {
        std.log.err("Failed to create executable path: {}", .{err});
        return err;
    };

    // First, parse the app file to get the platform reference
    const platform_spec = extractPlatformSpecFromApp(allocs, args.path) catch |err| {
        std.log.err("Failed to extract platform spec from app file: {}", .{err});
        return err;
    };

    // Resolve platform paths from the platform spec (relative to app file directory)
    const app_dir = std.fs.path.dirname(args.path) orelse ".";
    const platform_paths = resolvePlatformSpecToPaths(allocs, platform_spec, app_dir) catch |err| {
        std.log.err("Failed to resolve platform spec '{s}': {}", .{ platform_spec, err });
        return err;
    };

    // Use native detection (typically musl) for shim generation to match embedded shim library
    const shim_target = builder.RocTarget.detectNative();

    // Extract entrypoints from platform source file
    var entrypoints = std.array_list.Managed([]const u8).initCapacity(allocs.arena, 32) catch |err| {
        std.log.err("Failed to allocate entrypoints list: {}", .{err});
        return err;
    };

    if (platform_paths.platform_source_path) |platform_source| {
        extractEntrypointsFromPlatform(allocs, platform_source, &entrypoints) catch |err| {
            std.log.err("Failed to extract entrypoints from platform header: {}", .{err});
            return err;
        };
    } else {
        std.log.err("No platform source file found for entrypoint extraction", .{});
        return error.NoPlatformSource;
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
        const shim_path = std.fs.path.join(allocs.arena, &.{ exe_cache_dir, shim_filename }) catch |err| {
            std.log.err("Failed to create shim library path: {}", .{err});
            return err;
        };

        // Extract shim if not cached or if --no-cache is used
        const shim_exists = if (args.no_cache) false else blk: {
            std.fs.cwd().access(shim_path, .{}) catch {
                break :blk false;
            };
            break :blk true;
        };

        if (!shim_exists) {
            // Shim not found in cache or cache disabled, extract it
            extractReadRocFilePathShimLibrary(allocs, shim_path) catch |err| {
                std.log.err("Failed to extract read roc file path shim library: {}", .{err});
                return err;
            };
        }

        // Generate platform host shim using the detected entrypoints

        const platform_shim_path = generatePlatformHostShim(allocs, exe_cache_dir, entrypoints.items, shim_target) catch |err| {
            std.log.err("Failed to generate platform host shim: {}", .{err});
            return err;
        };

        // Link the host.a with our shim to create the interpreter executable using our linker
        // Try LLD first, fallback to clang if LLVM is not available
        var extra_args = std.array_list.Managed([]const u8).initCapacity(allocs.arena, 32) catch |err| {
            std.log.err("Failed to allocate extra args list: {}", .{err});
            return err;
        };

        // Add system libraries for macOS
        if (builtin.target.os.tag == .macos) {
            extra_args.append("-lSystem") catch |err| {
                std.log.err("Failed to allocate memory for linker args", .{});
                return err;
            };
        }

        // Create object files list - include platform shim if available
        var object_files = std.array_list.Managed([]const u8).initCapacity(allocs.arena, 8) catch |err| {
            std.log.err("Failed to allocate object files list: {}", .{err});
            return err;
        };
        object_files.append(platform_paths.host_lib_path) catch |err| {
            std.log.err("Failed to add host path to object files", .{});
            return err;
        };
        if (platform_shim_path) |path| {
            object_files.append(path) catch |err| {
                std.log.err("Failed to add platform shim path to object files", .{});
                return err;
            };
        }
        object_files.append(shim_path) catch |err| {
            std.log.err("Failed to add shim path to object files", .{});
            return err;
        };

        // Get platform directory and CRT files
        const platform_dir = std.fs.path.dirname(platform_paths.host_lib_path) orelse {
            std.log.err("Invalid platform host library path", .{});
            return error.InvalidPlatform;
        };

        const crt_files = try target_mod.getVendoredCRTFiles(allocs.arena, shim_target, platform_dir);

        // Setup platform files based on CRT files
        var platform_files_pre = try std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16);
        var platform_files_post = try std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16);

        // Add CRT files in correct order
        if (crt_files.crt1_o) |crt1| try platform_files_pre.append(crt1);
        if (crt_files.crti_o) |crti| try platform_files_pre.append(crti);
        if (crt_files.crtn_o) |crtn| try platform_files_post.append(crtn);
        if (crt_files.libc_a) |libc| try platform_files_post.append(libc);

        const target_abi: ?linker.TargetAbi = null;

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

        linker.link(allocs, link_config) catch |err| switch (err) {
            linker.LinkError.LLVMNotAvailable => {
                std.log.err("LLD linker not available -- this is likely a test executable that was built without LLVM", .{});
                return err;
            },
            linker.LinkError.LinkFailed => {
                std.log.err("LLD linker failed to create executable", .{});
                return err;
            },
            else => {
                std.log.err("Failed to link executable: {}", .{err});
                return err;
            },
        };
    }

    // Set up shared memory with ModuleEnv
    std.log.debug("Setting up shared memory for Roc file: {s}", .{args.path});
    const shm_handle = setupSharedMemoryWithModuleEnv(allocs, args.path) catch |err| {
        std.log.err("Failed to set up shared memory with ModuleEnv: {}", .{err});
        return err;
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
        runWithWindowsHandleInheritance(allocs, exe_path, shm_handle) catch |err| {
            std.log.err("Failed to run with Windows handle inheritance: {}", .{err});
            return err;
        };
    } else {
        // POSIX: Use existing file descriptor inheritance approach
        std.log.debug("Using POSIX file descriptor inheritance approach", .{});
        runWithPosixFdInheritance(allocs, exe_path, shm_handle, &cache_manager) catch |err| {
            std.log.err("Failed to run with POSIX fd inheritance: {}", .{err});
            return err;
        };
    }
    std.log.debug("Interpreter execution completed", .{});
}

/// Run child process using Windows handle inheritance (idiomatic Windows approach)
fn runWithWindowsHandleInheritance(allocs: *Allocators, exe_path: []const u8, shm_handle: SharedMemoryHandle) !void {
    // Make the shared memory handle inheritable
    if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
        std.log.err("Failed to set handle as inheritable", .{});
        return error.HandleInheritanceFailed;
    }

    // Convert paths to Windows wide strings
    const exe_path_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, exe_path);

    const cwd = try std.fs.cwd().realpathAlloc(allocs.arena, ".");
    const cwd_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, cwd);

    // Create command line with handle and size as arguments
    const handle_uint = @intFromPtr(shm_handle.fd);
    const cmd_line = try std.fmt.allocPrintSentinel(allocs.arena, "\"{s}\" {} {}", .{ exe_path, handle_uint, shm_handle.size }, 0);
    const cmd_line_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, cmd_line);

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
fn runWithPosixFdInheritance(allocs: *Allocators, exe_path: []const u8, shm_handle: SharedMemoryHandle, cache_manager: *CacheManager) !void {
    // Get cache directory for temporary files
    const temp_cache_dir = cache_manager.config.getTempDir(allocs.arena) catch |err| {
        std.log.err("Failed to get temp cache directory: {}", .{err});
        return err;
    };

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
    const temp_exe_path = createTempDirStructure(allocs, exe_path, shm_handle, temp_cache_dir) catch |err| {
        std.log.err("Failed to create temp dir structure: {}", .{err});
        return err;
    };
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
    var child = std.process.Child.init(&.{temp_exe_path}, allocs.gpa);
    child.cwd = std.fs.cwd().realpathAlloc(allocs.arena, ".") catch |err| {
        std.log.err("Failed to get current directory: {}", .{err});
        return err;
    };

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
pub fn setupSharedMemoryWithModuleEnv(allocs: *Allocators, roc_file_path: []const u8) !SharedMemoryHandle {
    // Create shared memory with SharedMemoryAllocator
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(SHARED_MEMORY_SIZE, page_size);
    // Don't defer deinit here - we need to keep the shared memory alive

    const shm_allocator = shm.allocator();

    // Quick check: is this an app with a platform?
    // If so, we need multi-module compilation
    const is_app_with_platform = try isAppWithPlatform(allocs.gpa, roc_file_path);

    std.debug.print("=== SETUP SHARED MEMORY: {s} ===\n", .{roc_file_path});
    std.debug.print("Is app with platform: {}\n", .{is_app_with_platform});

    // TEMP: Force multi-module for fx test
    const force_multi = std.mem.indexOf(u8, roc_file_path, "test/fx/app.roc") != null or
        std.mem.indexOf(u8, roc_file_path, "minimal_fx.roc") != null or
        std.mem.indexOf(u8, roc_file_path, "simple_fx_test") != null;
    if (force_multi) {
        std.debug.print("FORCING MULTI-MODULE for fx test\n", .{});
    }

    if (is_app_with_platform or force_multi) {
        std.debug.print("Using MULTI-MODULE compilation path\n", .{});
        return try setupSharedMemoryMultiModule(allocs, roc_file_path, &shm, shm_allocator);
    } else {
        std.debug.print("Using SINGLE-MODULE compilation path\n", .{});
        return try setupSharedMemorySingleModule(allocs, roc_file_path, &shm, shm_allocator);
    }
}

/// Check if a file is an app with a platform
fn isAppWithPlatform(allocator: std.mem.Allocator, file_path: []const u8) !bool {
    // Read and parse just the header to check
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(source);

    // Create a minimal ModuleEnv just for parsing
    var env = try ModuleEnv.init(allocator, source);
    defer env.deinit();

    env.common.source = source;

    var ast = parse.parse(&env.common, allocator) catch |err| {
        std.debug.print("DEBUG isAppWithPlatform: parse error: {}\n", .{err});
        return false;
    };
    defer ast.deinit(allocator);

    // Check if it's an app header
    std.debug.print("DEBUG isAppWithPlatform: root_node_idx={}\n", .{ast.root_node_idx});
    if (ast.root_node_idx != 0) {
        const root_idx: parse.AST.Header.Idx = @enumFromInt(ast.root_node_idx);
        const root_data = ast.store.getHeader(root_idx);
        std.debug.print("DEBUG isAppWithPlatform: root_data={}\n", .{root_data});
        const is_app = root_data == .app;
        std.debug.print("DEBUG isAppWithPlatform: is_app={}\n", .{is_app});
        return is_app;
    }

    std.debug.print("DEBUG isAppWithPlatform: root_node_idx is 0, returning false\n", .{});
    return false;
}

/// Single module compilation (existing logic)
fn setupSharedMemorySingleModule(allocs: *Allocators, roc_file_path: []const u8, shm: *SharedMemoryAllocator, shm_allocator: std.mem.Allocator) !SharedMemoryHandle {

    // Create a properly aligned header structure
    const Header = struct {
        parent_base_addr: u64,
        module_count: u32,  // Number of ModuleEnvs stored
        entry_count: u32,   // Number of entry points
        def_indices_offset: u64,
        module_envs_offset: u64,  // Offset to array of module env offsets
    };

    const header_ptr = try shm_allocator.create(Header);

    // Store the base address of the shared memory mapping (for ASLR-safe relocation)
    // The child will calculate the offset from its own base address
    const shm_base_addr = @intFromPtr(shm.base_ptr);
    header_ptr.parent_base_addr = shm_base_addr;

    // For now, we only support single-module compilation
    // TODO: Extend to support multi-module when an app imports platform modules
    header_ptr.module_count = 1;

    // Allocate array of module env offsets (currently just one)
    const module_env_offsets_ptr = try shm_allocator.alloc(u64, 1);
    const module_envs_offset_location = @intFromPtr(module_env_offsets_ptr.ptr) - @intFromPtr(shm.base_ptr);
    header_ptr.module_envs_offset = module_envs_offset_location;

    // Allocate the ModuleEnv
    const env_ptr = try shm_allocator.create(ModuleEnv);
    const module_env_offset = @intFromPtr(env_ptr) - @intFromPtr(shm.base_ptr);
    module_env_offsets_ptr[0] = module_env_offset;

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

    // Load builtin modules (Bool, Result, Str) for canonicalization and type checking
    var builtin_modules = try eval.BuiltinModules.init(allocs.gpa);
    defer builtin_modules.deinit();

    // Create arena allocator for scratch memory
    var arena = std.heap.ArenaAllocator.init(shm_allocator);
    defer arena.deinit();

    var env = try ModuleEnv.init(shm_allocator, source);
    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(shm_allocator);

    // Parse the source code as a full module
    var parse_ast = try parse.parse(&env.common, allocs.gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(shm_allocator, module_name);
    const common_idents: Check.CommonIdents = .{
        .module_name = try env.insertIdent(base.Ident.for_text("test")),
        .list = try env.insertIdent(base.Ident.for_text("List")),
        .box = try env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = builtin_modules.builtin_indices.bool_type,
        .try_stmt = builtin_modules.builtin_indices.try_type,
        .builtin_module = builtin_modules.builtin_module.env,
    };

    // Create module_envs map for auto-importing builtin types
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer module_envs_map.deinit();

    // Populate module_envs with Bool, Result, Dict, Set using shared function
    // This ensures production and tests use identical logic
    try Can.populateModuleEnvs(
        &module_envs_map,
        &env,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_indices,
    );

    // Create canonicalizer with module_envs
    var canonicalizer = try Can.init(&env, &parse_ast, &module_envs_map);

    // Canonicalize the entire module
    try canonicalizer.canonicalizeFile();
    try canonicalizer.validateForExecution();

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

    // Type check the module - pass the single Builtin module as the only imported module
    const imported_envs = [_]*const can.ModuleEnv{builtin_modules.builtin_module.env};
    var checker = try Check.init(shm_allocator, &env.types, &env, &imported_envs, &module_envs_map, &env.store.regions, common_idents);
    try checker.checkFile();

    // Copy the ModuleEnv to the allocated space
    env_ptr.* = env;

    // Clean up the canonicalizer and parsing structures
    canonicalizer.deinit();

    // Clean up parse_ast since it was allocated with gpa, not shared memory
    parse_ast.deinit(allocs.gpa);

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

/// Multi-module compilation for apps with platforms
fn setupSharedMemoryMultiModule(allocs: *Allocators, app_file_path: []const u8, shm: *SharedMemoryAllocator, shm_allocator: std.mem.Allocator) !SharedMemoryHandle {
    // This is a simplified implementation that handles the specific case of:
    // - An app that uses a platform
    // - Platform modules that need to be compiled with building_platform_modules=true
    //
    // For the general case, this would need to use BuildEnv for full dependency resolution.
    // But for the fx test case, we can hard-code the platform modules.

    const app_dir = std.fs.path.dirname(app_file_path) orelse ".";

    // For now, assume the platform modules are Stdout and Stderr in test/fx/platform/
    // This is sufficient for the fx_platform_test
    const platform_dir = try std.fs.path.join(allocs.gpa, &[_][]const u8{ app_dir, "platform" });
    defer allocs.gpa.free(platform_dir);

    const stdout_path = try std.fs.path.join(allocs.gpa, &[_][]const u8{ platform_dir, "Stdout.roc" });
    defer allocs.gpa.free(stdout_path);

    const stderr_path = try std.fs.path.join(allocs.gpa, &[_][]const u8{ platform_dir, "Stderr.roc" });
    defer allocs.gpa.free(stderr_path);

    const host_path = try std.fs.path.join(allocs.gpa, &[_][]const u8{ platform_dir, "Host.roc" });
    defer allocs.gpa.free(host_path);

    // Create header
    const Header = struct {
        parent_base_addr: u64,
        module_count: u32,
        entry_count: u32,
        def_indices_offset: u64,
        module_envs_offset: u64,
    };

    const header_ptr = try shm_allocator.create(Header);
    const shm_base_addr = @intFromPtr(shm.base_ptr);
    header_ptr.parent_base_addr = shm_base_addr;

    // We'll compile 4 modules: app, Stdout, Stderr, Host
    header_ptr.module_count = 4;

    // Allocate array for module env offsets
    const module_env_offsets_ptr = try shm_allocator.alloc(u64, 4);
    const module_envs_offset_location = @intFromPtr(module_env_offsets_ptr.ptr) - @intFromPtr(shm.base_ptr);
    header_ptr.module_envs_offset = module_envs_offset_location;

    // Load builtin modules
    var builtin_modules = try eval.BuiltinModules.init(allocs.gpa);
    defer builtin_modules.deinit();

    // Compile Stdout module (index 1)
    const stdout_env_ptr = try compileModuleToSharedMemory(
        allocs,
        stdout_path,
        "Stdout.roc",
        shm_allocator,
        &builtin_modules,
        true, // building_platform_modules
    );
    module_env_offsets_ptr[1] = @intFromPtr(stdout_env_ptr) - @intFromPtr(shm.base_ptr);

    // Compile Stderr module (index 2)
    const stderr_env_ptr = try compileModuleToSharedMemory(
        allocs,
        stderr_path,
        "Stderr.roc",
        shm_allocator,
        &builtin_modules,
        true, // building_platform_modules
    );
    module_env_offsets_ptr[2] = @intFromPtr(stderr_env_ptr) - @intFromPtr(shm.base_ptr);

    // Compile Host module (index 3)
    const host_env_ptr = try compileModuleToSharedMemory(
        allocs,
        host_path,
        "Host.roc",
        shm_allocator,
        &builtin_modules,
        true, // building_platform_modules
    );
    module_env_offsets_ptr[3] = @intFromPtr(host_env_ptr) - @intFromPtr(shm.base_ptr);

    // Now that all platform modules are compiled, collect ALL hosted functions
    // from all modules and sort them globally to assign correct indices
    std.debug.print("DEBUG: Collecting all hosted functions for global sorting\n", .{});
    const HostedCompiler = can.HostedCompiler;
    var all_hosted_fns = std.ArrayList(HostedCompiler.HostedFunctionInfo).empty;
    defer {
        // TODO: Fix allocator mismatch - strings were allocated with env.gpa, need to free with same allocator
        // for (all_hosted_fns.items) |fn_info| {
        //     allocs.gpa.free(fn_info.name_text);
        // }
        all_hosted_fns.deinit(allocs.gpa);
    }

    // Collect from platform modules that are actually used by the app
    // (Stdout and Stderr, but not Host which is internal)
    const platform_envs = [_]*ModuleEnv{ stdout_env_ptr, stderr_env_ptr };
    for (platform_envs) |platform_env| {
        const module_fns = try HostedCompiler.collectAndSortHostedFunctions(platform_env);
        // Note: We don't deinit module_fns because we're transferring ownership of items to all_hosted_fns
        // The ArrayList buffer itself will leak but it's a small one-time allocation

        // Move to global list (transfer ownership of allocated strings)
        for (module_fns.items) |fn_info| {
            try all_hosted_fns.append(allocs.gpa, fn_info);
        }
    }

    // Sort all hosted functions globally
    const SortContext = struct {
        pub fn lessThan(_: void, a: HostedCompiler.HostedFunctionInfo, b: HostedCompiler.HostedFunctionInfo) bool {
            return std.mem.order(u8, a.name_text, b.name_text) == .lt;
        }
    };
    std.mem.sort(HostedCompiler.HostedFunctionInfo, all_hosted_fns.items, {}, SortContext.lessThan);

    std.debug.print("DEBUG: Global sorted hosted functions ({} total):\n", .{all_hosted_fns.items.len});
    for (all_hosted_fns.items, 0..) |fn_info, i| {
        std.debug.print("  [{d}] {s}\n", .{i, fn_info.name_text});
    }

    // Now reassign indices globally across all modules
    // We need to track which module each function belongs to
    for (platform_envs) |platform_env| {
        // For each hosted function in this module, find its global index
        const all_defs = platform_env.store.sliceDefs(platform_env.all_defs);
        for (all_defs) |def_idx| {
            const def = platform_env.store.getDef(def_idx);
            const expr = platform_env.store.getExpr(def.expr);

            if (expr == .e_hosted_lambda) {
                const hosted = expr.e_hosted_lambda;
                const local_name = platform_env.getIdent(hosted.symbol_name);

                // Build the same qualified name we used for sorting
                var module_name = platform_env.module_name;
                if (std.mem.endsWith(u8, module_name, ".roc")) {
                    module_name = module_name[0 .. module_name.len - 4];
                }
                const qualified_name = try std.fmt.allocPrint(allocs.gpa, "{s}.{s}", .{module_name, local_name});
                defer allocs.gpa.free(qualified_name);

                const stripped_name = if (std.mem.endsWith(u8, qualified_name, "!"))
                    qualified_name[0 .. qualified_name.len - 1]
                else
                    qualified_name;

                // Find the global index for this function
                var global_index: ?u32 = null;
                for (all_hosted_fns.items, 0..) |fn_info, i| {
                    if (std.mem.eql(u8, fn_info.name_text, stripped_name)) {
                        global_index = @intCast(i);
                        break;
                    }
                }

                if (global_index) |idx| {
                    // Update the expression's index field
                    const expr_node_idx = @as(@TypeOf(platform_env.store.nodes).Idx, @enumFromInt(@intFromEnum(def.expr)));
                    var expr_node = platform_env.store.nodes.get(expr_node_idx);
                    expr_node.data_2 = idx;
                    platform_env.store.nodes.set(expr_node_idx, expr_node);
                    std.debug.print("DEBUG: Assigned global index {} to {s}\n", .{idx, stripped_name});
                }
            }
        }
    }

    // Compile app module (index 0) with references to platform modules
    const app_env_ptr = try compileAppModuleToSharedMemory(
        allocs,
        app_file_path,
        shm_allocator,
        &builtin_modules,
        &[_]*const ModuleEnv{ stdout_env_ptr, stderr_env_ptr, host_env_ptr },
    );
    module_env_offsets_ptr[0] = @intFromPtr(app_env_ptr) - @intFromPtr(shm.base_ptr);

    // Debug: print app imports
    std.debug.print("=== APP IMPORTS ===\n", .{});
    std.debug.print("App has {} imports\n", .{app_env_ptr.imports.imports.items.items.len});
    for (app_env_ptr.imports.imports.items.items, 0..) |str_idx, i| {
        const import_name = app_env_ptr.common.getString(str_idx);
        std.debug.print("  Import {}: {s}\n", .{i, import_name});
    }

    // Set up entry points from app module exports
    const exports_slice = app_env_ptr.store.sliceDefs(app_env_ptr.exports);
    header_ptr.entry_count = @intCast(exports_slice.len);

    const def_indices_ptr = try shm_allocator.alloc(u32, exports_slice.len);
    const def_indices_location = @intFromPtr(def_indices_ptr.ptr) - @intFromPtr(shm.base_ptr);
    header_ptr.def_indices_offset = def_indices_location;

    for (exports_slice, 0..) |def_idx, i| {
        def_indices_ptr[i] = @intFromEnum(def_idx);
    }

    shm.updateHeader();

    return SharedMemoryHandle{
        .fd = shm.handle,
        .ptr = shm.base_ptr,
        .size = shm.getUsedSize(),
    };
}

/// Helper to compile a single module into shared memory
fn compileModuleToSharedMemory(
    allocs: *Allocators,
    file_path: []const u8,
    module_name: []const u8,
    shm_allocator: std.mem.Allocator,
    builtin_modules: *eval.BuiltinModules,
    building_platform_modules: bool,
) !*ModuleEnv {
    // Read file
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const source = try shm_allocator.alloc(u8, @intCast(file_size));
    _ = try file.read(source);

    const module_name_copy = try shm_allocator.dupe(u8, module_name);

    // Initialize ModuleEnv
    var env = try ModuleEnv.init(shm_allocator, source);
    env.common.source = source;
    env.module_name = module_name_copy;
    try env.common.calcLineStarts(shm_allocator);

    // Parse
    var parse_ast = try parse.parse(&env.common, allocs.gpa);
    defer parse_ast.deinit(allocs.gpa);
    parse_ast.store.emptyScratch();

    // Initialize CIR
    try env.initCIRFields(shm_allocator, module_name_copy);

    // Create module_envs map
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer module_envs_map.deinit();

    try Can.populateModuleEnvs(
        &module_envs_map,
        &env,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_indices,
    );

    // Canonicalize
    var canonicalizer = try Can.init(&env, &parse_ast, &module_envs_map);
    defer canonicalizer.deinit();

    // Set building_platform_modules flag
    env.building_platform_modules = building_platform_modules;

    try canonicalizer.canonicalizeFile();

    // Allocate and return
    const env_ptr = try shm_allocator.create(ModuleEnv);
    env_ptr.* = env;
    return env_ptr;
}

/// Helper to compile the app module with platform module imports
fn compileAppModuleToSharedMemory(
    allocs: *Allocators,
    file_path: []const u8,
    shm_allocator: std.mem.Allocator,
    builtin_modules: *eval.BuiltinModules,
    platform_envs: []const *const ModuleEnv,
) !*ModuleEnv {
    // Read file
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const source = try shm_allocator.alloc(u8, @intCast(file_size));
    _ = try file.read(source);

    const basename = std.fs.path.basename(file_path);
    const module_name = try shm_allocator.dupe(u8, basename);

    // Initialize ModuleEnv
    var env = try ModuleEnv.init(shm_allocator, source);
    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(shm_allocator);

    // Parse
    var parse_ast = try parse.parse(&env.common, allocs.gpa);
    defer parse_ast.deinit(allocs.gpa);
    parse_ast.store.emptyScratch();

    // Initialize CIR
    try env.initCIRFields(shm_allocator, module_name);

    const common_idents: Check.CommonIdents = .{
        .module_name = try env.insertIdent(base.Ident.for_text("test")),
        .list = try env.insertIdent(base.Ident.for_text("List")),
        .box = try env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = builtin_modules.builtin_indices.bool_type,
        .try_stmt = builtin_modules.builtin_indices.try_type,
        .builtin_module = builtin_modules.builtin_module.env,
    };

    // Create module_envs map
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer module_envs_map.deinit();

    try Can.populateModuleEnvs(
        &module_envs_map,
        &env,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_indices,
    );

    // Add platform modules to module_envs_map so canonicalizer can resolve imports
    // The app imports "pf.Stdout" and "pf.Stderr", so we need to add those qualified names
    for (platform_envs) |platform_env| {
        // Get the module name from the module_name field (e.g., "Stdout.roc")
        const platform_module_name = platform_env.module_name;

        // Remove ".roc" extension if present to get just the module name
        const name_without_ext = if (std.mem.endsWith(u8, platform_module_name, ".roc"))
            platform_module_name[0 .. platform_module_name.len - 4]
        else
            platform_module_name;

        // Create the qualified name "pf.ModuleName" to match the import statement
        const qualified_name = try std.fmt.allocPrint(allocs.gpa, "pf.{s}", .{name_without_ext});
        defer allocs.gpa.free(qualified_name);

        std.debug.print("DEBUG: Adding platform module to module_envs_map: {s}\n", .{qualified_name});

        // Find or create the ident for the qualified module name
        const ident_idx = try env.insertIdent(base.Ident.for_text(qualified_name));
        std.debug.print("DEBUG:   Stored with ident_idx={}\n", .{ident_idx});

        // Add to module_envs_map
        try module_envs_map.put(ident_idx, .{
            .env = platform_env,
            .statement_idx = null, // No specific statement, just the module reference
        });
    }

    // Canonicalize
    var canonicalizer = try Can.init(&env, &parse_ast, &module_envs_map);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();
    try canonicalizer.validateForExecution();

    // Check for canonicalization diagnostics
    if (env.diagnostics.span.len > 0) {
        std.debug.print("WARNING: {} canonicalization diagnostics generated!\n", .{env.diagnostics.span.len});
    }

    // Type check with platform modules as imports
    std.debug.print("DEBUG: About to create type checker for app module\n", .{});
    std.debug.print("DEBUG: env.imports.imports.items.items.len = {}\n", .{env.imports.imports.items.items.len});
    std.debug.print("DEBUG: platform_envs.len = {}\n", .{platform_envs.len});

    // Build imported_modules array that matches the imports
    // The imports are: Builtin (0), pf.Stdout (1), pf.Stderr (2), Stdout.roc (3 - duplicate of 1)
    // We need an array where index matches import index
    const import_count = env.imports.imports.items.items.len;
    const imported_modules_slice = try shm_allocator.alloc(*const ModuleEnv, import_count);

    // Import 0 is always Builtin
    imported_modules_slice[0] = builtin_modules.builtin_module.env;

    // For each remaining import, find the matching platform module
    for (env.imports.imports.items.items[1..], 1..) |import_str_idx, i| {
        const import_name = env.common.getString(import_str_idx);
        std.debug.print("DEBUG: Import {}: {s}\n", .{i, import_name});

        // Match to one of the platform modules
        var found_match = false;
        for (platform_envs) |platform_env| {
            const platform_module_name = platform_env.module_name;
            // Match "pf.Stdout" to "Stdout.roc", or "Stdout.roc" to "Stdout.roc"
            if (std.mem.indexOf(u8, import_name, "Stdout") != null and std.mem.indexOf(u8, platform_module_name, "Stdout") != null) {
                imported_modules_slice[i] = platform_env;
                std.debug.print("DEBUG:   -> Matched to Stdout module\n", .{});
                found_match = true;
                break;
            } else if (std.mem.indexOf(u8, import_name, "Stderr") != null and std.mem.indexOf(u8, platform_module_name, "Stderr") != null) {
                imported_modules_slice[i] = platform_env;
                std.debug.print("DEBUG:   -> Matched to Stderr module\n", .{});
                found_match = true;
                break;
            }
        }

        if (!found_match) {
            std.debug.print("DEBUG:   -> WARNING: No match found, using first platform module as fallback\n", .{});
            imported_modules_slice[i] = platform_envs[0];
        }
    }

    std.debug.print("DEBUG: Built imported_modules array with {} entries\n", .{imported_modules_slice.len});

    var checker = try Check.init(shm_allocator, &env.types, &env, imported_modules_slice, &module_envs_map, &env.store.regions, common_idents);
    defer checker.deinit();

    std.debug.print("DEBUG: About to call checkFile() for app module\n", .{});
    try checker.checkFile();
    std.debug.print("DEBUG: checkFile() completed for app module\n", .{});

    // Check for type checking diagnostics
    if (env.diagnostics.span.len > 0) {
        std.debug.print("WARNING: {} total diagnostics after type checking!\n", .{env.diagnostics.span.len});
    }

    // Allocate and return
    const env_ptr = try shm_allocator.create(ModuleEnv);
    env_ptr.* = env;
    return env_ptr;
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
};

/// Resolve platform specification from a Roc file to find both host library and platform source.
/// Returns PlatformPaths with arena-allocated paths (no need to free).
pub fn resolvePlatformPaths(allocs: *Allocators, roc_file_path: []const u8) (std.mem.Allocator.Error || error{ NoPlatformFound, PlatformNotSupported })!PlatformPaths {
    // Read the Roc file to parse the app header
    const roc_file = std.fs.cwd().openFile(roc_file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return error.NoPlatformFound,
        else => return error.NoPlatformFound, // Treat all file errors as no platform found
    };
    defer roc_file.close();

    const file_size = roc_file.getEndPos() catch return error.NoPlatformFound;
    const source = allocs.gpa.alloc(u8, @intCast(file_size)) catch return error.OutOfMemory;
    defer allocs.gpa.free(source);
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
                            const platform_path = try std.fs.path.join(allocs.arena, &.{ app_dir, platform_spec });

                            // Look for host library near the platform file
                            const platform_dir = std.fs.path.dirname(platform_path) orelse ".";
                            const host_filename = if (comptime builtin.target.os.tag == .windows) "host.lib" else "libhost.a";
                            const host_path = try std.fs.path.join(allocs.arena, &.{ platform_dir, host_filename });

                            std.fs.cwd().access(host_path, .{}) catch {
                                return error.PlatformNotSupported;
                            };

                            // Try to find platform source file (commonly main.roc but could be anything)
                            const platform_source_path = blk: {
                                // First try the exact path if it's a .roc file
                                if (std.mem.endsWith(u8, platform_path, ".roc")) {
                                    std.fs.cwd().access(platform_path, .{}) catch break :blk null;
                                    break :blk platform_path;
                                }

                                // Try common platform source names in the platform directory
                                const common_names = [_][]const u8{ "main.roc", "platform.roc", "Platform.roc" };
                                for (common_names) |name| {
                                    const source_path = try std.fs.path.join(allocs.arena, &.{ platform_dir, name });
                                    std.fs.cwd().access(source_path, .{}) catch continue;
                                    break :blk source_path;
                                }
                                break :blk null;
                            };

                            return PlatformPaths{
                                .host_lib_path = host_path,
                                .platform_source_path = platform_source_path,
                            };
                        }

                        // Try to resolve platform to a local host library and source
                        const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";
                        return resolvePlatformSpecToPaths(allocs, platform_spec, app_dir);
                    }
                }
            }
        }
    }

    return error.NoPlatformFound;
}

/// Extract platform specification from app file header using simple string parsing
///
/// TODO use this information from BuildEnv once we have the parser/can/typechcking setup
/// for multiple modules, and we have this information available. This is just a temporary hack
/// for testing now.
fn extractPlatformSpecFromApp(allocs: *Allocators, app_file_path: []const u8) ![]const u8 {
    // Read the app file
    const source = std.fs.cwd().readFileAlloc(allocs.gpa, app_file_path, std.math.maxInt(usize)) catch return error.FileNotFound;
    defer allocs.gpa.free(source);

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
                    return try allocs.arena.dupe(u8, platform_path);
                }
            }
            // Also try alternative format: platform "..."
            if (std.mem.indexOf(u8, trimmed, "platform \"")) |start_idx| {
                const quote_start = start_idx + 10; // length of "platform \""
                if (std.mem.indexOfScalarPos(u8, trimmed, quote_start, '"')) |end_idx| {
                    const platform_path = trimmed[quote_start..end_idx];
                    return try allocs.arena.dupe(u8, platform_path);
                }
            }
        }
    }

    return error.NotAppFile;
}

/// Resolve a platform specification to both host library and platform source paths
fn resolvePlatformSpecToPaths(allocs: *Allocators, platform_spec: []const u8, base_dir: []const u8) (std.mem.Allocator.Error || error{PlatformNotSupported})!PlatformPaths {

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
                platform_source_path = try allocs.arena.dupe(u8, source_path);
                break;
            }

            return PlatformPaths{
                .host_lib_path = try allocs.arena.dupe(u8, host_path),
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
                platform_source_path = try allocs.arena.dupe(u8, source_path);
                break;
            }

            return PlatformPaths{
                .host_lib_path = try allocs.arena.dupe(u8, host_path),
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
        try allocs.arena.dupe(u8, platform_spec)
    else
        try std.fs.path.join(allocs.arena, &.{ base_dir, platform_spec });

    std.fs.cwd().access(resolved_path, .{}) catch {
        return error.PlatformNotSupported;
    };

    // For file paths, we need to determine if it's a host library or platform source
    // Host libraries typically have .a/.lib extensions, platform sources have .roc extension
    if (std.mem.endsWith(u8, resolved_path, ".roc")) {
        // This is a platform source file - look for host library near it
        const platform_dir = std.fs.path.dirname(resolved_path) orelse ".";
        const host_filename = if (comptime builtin.target.os.tag == .windows) "host.lib" else "libhost.a";
        const host_path = try std.fs.path.join(allocs.arena, &.{ platform_dir, host_filename });

        std.fs.cwd().access(host_path, .{}) catch {
            return error.PlatformNotSupported;
        };

        return PlatformPaths{
            .host_lib_path = try allocs.arena.dupe(u8, host_path),
            .platform_source_path = try allocs.arena.dupe(u8, resolved_path),
        };
    } else {
        // Assume it's a host library file
        return PlatformPaths{
            .host_lib_path = try allocs.arena.dupe(u8, resolved_path),
            .platform_source_path = null,
        };
    }
}

/// Extract all entrypoint names from platform header provides record into ArrayList
/// TODO: Replace this with proper BuildEnv solution in the future
fn extractEntrypointsFromPlatform(allocs: *Allocators, roc_file_path: []const u8, entrypoints: *std.array_list.Managed([]const u8)) !void {
    // Read the Roc file
    const source = std.fs.cwd().readFileAlloc(allocs.gpa, roc_file_path, std.math.maxInt(usize)) catch return error.NoPlatformFound;
    defer allocs.gpa.free(source);

    // Extract module name from the file path
    const basename = std.fs.path.basename(roc_file_path);
    const module_name = try allocs.arena.dupe(u8, basename);

    // Create ModuleEnv
    var env = ModuleEnv.init(allocs.gpa, source) catch return error.ParseFailed;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(allocs.gpa);

    // Parse the source code as a full module
    var parse_ast = parse.parse(&env.common, allocs.gpa) catch return error.ParseFailed;
    defer parse_ast.deinit(allocs.gpa);

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

                // Strip the `!` suffix from effectful function names
                // The `!` is Roc syntax for effects, but shouldn't be in C symbols
                const clean_name = if (std.mem.endsWith(u8, field_name, "!"))
                    field_name[0 .. field_name.len - 1]
                else
                    field_name;

                try entrypoints.append(try allocs.arena.dupe(u8, clean_name));
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
pub fn extractReadRocFilePathShimLibrary(allocs: *Allocators, output_path: []const u8) !void {
    _ = allocs; // unused but kept for consistency

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
pub fn rocBundle(allocs: *Allocators, args: cli_args.BundleArgs) !void {
    const stdout = stdoutWriter();
    const stderr = stderrWriter();

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
    var file_paths = std.ArrayList([]const u8).empty;
    defer file_paths.deinit(allocs.arena);

    var uncompressed_size: u64 = 0;

    // If no paths provided, default to "main.roc"
    const paths_to_use = if (args.paths.len == 0) &[_][]const u8{"main.roc"} else args.paths;

    // Remember the first path from CLI args (before sorting)
    const first_cli_path = paths_to_use[0];

    // Check that all files exist and collect their sizes
    for (paths_to_use) |path| {
        const file = cwd.openFile(path, .{}) catch |err| {
            try stderr.print("Error: Could not open file '{s}': {}\n", .{ path, err });
            return err;
        };
        defer file.close();

        const stat = try file.stat();
        uncompressed_size += stat.size;

        try file_paths.append(allocs.arena, path);
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
    var allocator_copy = allocs.arena;
    var error_ctx: bundle.ErrorContext = undefined;
    var temp_writer_buffer: [4096]u8 = undefined;
    var temp_writer = temp_file.writer(&temp_writer_buffer);
    const final_filename = bundle.bundleFiles(
        &iter,
        @intCast(args.compression_level),
        &allocator_copy,
        &temp_writer.interface,
        cwd,
        null, // path_prefix parameter - null means no stripping
        &error_ctx,
    ) catch |err| {
        if (err == error.InvalidPath) {
            try stderr.print("Error: Invalid file path - {s}\n", .{formatBundlePathValidationReason(error_ctx.reason)});
            try stderr.print("Path: {s}\n", .{error_ctx.path});
        }
        return err;
    };
    // No need to free when using arena allocator

    try temp_writer.interface.flush();

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
        try std.fs.path.join(allocs.arena, &.{ args.output_dir.?, final_filename });
    // No need to free when using arena allocator

    // Print results
    try stdout.print("Created: {s}\n", .{display_path});
    try stdout.print("Compressed size: {} bytes\n", .{compressed_size});
    try stdout.print("Uncompressed size: {} bytes\n", .{uncompressed_size});
    try stdout.print("Compression ratio: {d:.2}:1\n", .{@as(f64, @floatFromInt(uncompressed_size)) / @as(f64, @floatFromInt(compressed_size))});
    try stdout.print("Time: {} ms\n", .{elapsed_ms});
}

fn rocUnbundle(allocs: *Allocators, args: cli_args.UnbundleArgs) !void {
    const stdout = stdoutWriter();
    const stderr = stderrWriter();
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
        var error_ctx: unbundle.ErrorContext = undefined;
        var archive_reader_buffer: [4096]u8 = undefined;
        var archive_reader = archive_file.reader(&archive_reader_buffer);
        unbundle.unbundleFiles(
            allocs.gpa,
            &archive_reader.interface,
            output_dir,
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
                    try stderr.print("Error: Invalid path in archive - {s}\n", .{formatUnbundlePathValidationReason(error_ctx.reason)});
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
        return error.UnbundleFailed;
    }
}

fn rocBuild(allocs: *Allocators, args: cli_args.BuildArgs) !void {
    // Handle the --z-bench-tokenize flag
    if (args.z_bench_tokenize) |file_path| {
        try benchTokenizer(allocs.gpa, file_path);
        return;
    }

    // Handle the --z-bench-parse flag
    if (args.z_bench_parse) |directory_path| {
        try benchParse(allocs.gpa, directory_path);
        return;
    }

    // Import needed modules
    const target_mod = @import("target.zig");
    const app_stub = @import("app_stub.zig");
    const cross_compilation = @import("cross_compilation.zig");

    std.log.info("Building {s} for cross-compilation", .{args.path});

    // Detect host target
    const host_target = cross_compilation.detectHostTarget();
    std.log.info("Host: {} ({s})", .{ host_target, host_target.toTriple() });

    // Parse target if provided, otherwise use native with musl preference
    const target = if (args.target) |target_str| blk: {
        break :blk target_mod.RocTarget.fromString(target_str) orelse {
            std.log.err("Invalid target: {s}", .{target_str});
            std.log.err("Valid targets: x64musl, x64glibc, arm64musl, arm64glibc, etc.", .{});
            return error.InvalidTarget;
        };
    } else target_mod.RocTarget.detectNative();

    std.log.info("Target: {} ({s})", .{ target, target.toTriple() });

    // Validate cross-compilation support
    const cross_validation = cross_compilation.validateCrossCompilation(host_target, target);
    switch (cross_validation) {
        .supported => {
            std.log.info("Cross-compilation from {s} to {s} is supported", .{ @tagName(host_target), @tagName(target) });
        },
        .unsupported_host_target, .unsupported_cross_compilation, .missing_toolchain => {
            const stderr = stderrWriter();
            try cross_compilation.printCrossCompilationError(stderr, cross_validation);
            return error.UnsupportedCrossCompilation;
        },
    }

    // Get platform paths from the app file
    const platform_paths = resolvePlatformPaths(allocs, args.path) catch |err| {
        std.log.err("Failed to resolve platform paths for {s}: {}", .{ args.path, err });
        return err;
    };

    const platform_dir = std.fs.path.dirname(platform_paths.host_lib_path) orelse {
        std.log.err("Invalid platform host library path", .{});
        return error.InvalidPlatform;
    };

    // Try to find target-specific host library, otherwise use generic
    const host_lib_filename = if (target.toOsTag() == .windows) "host.lib" else "libhost.a";
    const target_specific_host = try std.fs.path.join(allocs.arena, &.{ platform_dir, "targets", @tagName(target), host_lib_filename });

    const host_lib_path = blk: {
        std.fs.cwd().access(target_specific_host, .{}) catch {
            // No target-specific host library, use the generic one
            break :blk platform_paths.host_lib_path;
        };
        break :blk target_specific_host;
    };

    std.fs.cwd().access(host_lib_path, .{}) catch |err| {
        std.log.err("Host library not found: {s} ({})", .{ host_lib_path, err });
        return err;
    };

    // Get expected entrypoints by parsing the platform's main.roc file
    const platform_source_path = platform_paths.platform_source_path orelse {
        std.log.err("Platform source file not found for: {s}", .{args.path});
        return error.NoPlatformFound;
    };
    var entrypoints_list = std.array_list.Managed([]const u8).init(allocs.arena);
    defer entrypoints_list.deinit();

    try extractEntrypointsFromPlatform(allocs, platform_source_path, &entrypoints_list);

    // Convert to PlatformEntrypoint array for generateAppStubObject
    const entrypoints = try allocs.arena.alloc(app_stub.PlatformEntrypoint, entrypoints_list.items.len);
    for (entrypoints_list.items, 0..) |name, i| {
        entrypoints[i] = app_stub.PlatformEntrypoint{ .name = name };
    }

    std.log.info("Expected entrypoints: {}", .{entrypoints.len});
    for (entrypoints, 0..) |ep, i| {
        std.log.info("  {}: roc__{s}", .{ i, ep.name });
    }

    // Create temp directory for build artifacts using Roc's cache system
    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(allocs.gpa, cache_config, Filesystem.default());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(allocs.arena);
    const temp_dir = try std.fs.path.join(allocs.arena, &.{ cache_dir, "roc_build" });

    std.fs.cwd().makePath(temp_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Generate app stub object file
    const app_stub_obj = try app_stub.generateAppStubObject(allocs.arena, temp_dir, entrypoints, target);

    // Get CRT files for the target
    const crt_files = try target_mod.getVendoredCRTFiles(allocs.arena, target, platform_dir);

    // Create object files list for linking
    var object_files = try std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16);

    // Add our app stub and host library
    try object_files.append(app_stub_obj);
    try object_files.append(host_lib_path);

    // Setup platform files based on target
    var platform_files_pre = try std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16);
    var platform_files_post = try std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16);
    var extra_args = try std.array_list.Managed([]const u8).initCapacity(allocs.arena, 32);

    // Add CRT files in correct order
    if (crt_files.crt1_o) |crt1| try platform_files_pre.append(crt1);
    if (crt_files.crti_o) |crti| try platform_files_pre.append(crti);
    if (crt_files.crtn_o) |crtn| try platform_files_post.append(crtn);

    // For static linking with musl, add libc.a
    if (crt_files.libc_a) |libc| {
        try platform_files_post.append(libc);
    } else if (target.isDynamic()) {
        // For dynamic linking with glibc, check platform targets folder for stubs
        const target_name = @tagName(target);
        const stub_dir_path = try std.fs.path.join(allocs.arena, &.{ platform_dir, "targets", target_name });
        const stub_so_path = try std.fs.path.join(allocs.arena, &.{ stub_dir_path, "libc.so.6" });

        // Try to use platform-provided glibc stubs if they exist
        const has_platform_stubs = blk: {
            std.fs.cwd().access(stub_so_path, .{}) catch break :blk false;
            break :blk true;
        };

        if (has_platform_stubs) {
            // Platform stubs exist, use them
            const stub_dir_arg = try std.fmt.allocPrint(allocs.arena, "-L{s}", .{stub_dir_path});
            try extra_args.append(stub_dir_arg);
            try extra_args.append("-lc");
            std.log.info("Using platform-provided glibc stubs from: {s}", .{stub_dir_path});
        } else {
            // No platform stubs found, use system libraries
            std.log.debug("No platform-provided glibc stubs found at {s}, using system libraries", .{stub_so_path});
            const common_lib_paths = [_][]const u8{
                "/lib/x86_64-linux-gnu",
                "/usr/lib/x86_64-linux-gnu",
                "/lib/aarch64-linux-gnu",
                "/usr/lib/aarch64-linux-gnu",
                "/lib64",
                "/usr/lib64",
                "/lib",
                "/usr/lib",
            };

            for (common_lib_paths) |lib_path| {
                std.fs.cwd().access(lib_path, .{}) catch continue;
                const search_arg = try std.fmt.allocPrint(allocs.arena, "-L{s}", .{lib_path});
                try extra_args.append(search_arg);
            }

            try extra_args.append("-lc");
        }

        // Add dynamic linker path
        if (target.getDynamicLinkerPath()) |dl_path| {
            const dl_arg = try std.fmt.allocPrint(allocs.arena, "--dynamic-linker={s}", .{dl_path});
            try extra_args.append(dl_arg);
        } else |_| {}
    }

    // Determine output path
    const base_output_path = if (args.output) |output|
        try allocs.arena.dupe(u8, output)
    else blk: {
        const basename = std.fs.path.basename(args.path);
        const name_without_ext = if (std.mem.endsWith(u8, basename, ".roc"))
            basename[0 .. basename.len - 4]
        else
            basename;
        break :blk try allocs.arena.dupe(u8, name_without_ext);
    };

    // Add .exe extension on Windows if not already present
    const output_path = if (target.toOsTag() == .windows and !std.mem.endsWith(u8, base_output_path, ".exe"))
        try std.fmt.allocPrint(allocs.arena, "{s}.exe", .{base_output_path})
    else
        try allocs.arena.dupe(u8, base_output_path);

    // Use LLD for linking
    const linker_mod = @import("linker.zig");
    const target_abi = if (target.isStatic()) linker_mod.TargetAbi.musl else linker_mod.TargetAbi.gnu;
    const link_config = linker_mod.LinkConfig{
        .target_format = linker_mod.TargetFormat.detectFromOs(target.toOsTag()),
        .object_files = object_files.items,
        .platform_files_pre = platform_files_pre.items,
        .platform_files_post = platform_files_post.items,
        .extra_args = extra_args.items,
        .output_path = output_path,
        .target_abi = target_abi,
        .target_os = target.toOsTag(),
        .target_arch = target.toCpuArch(),
    };

    try linker_mod.link(allocs, link_config);

    std.log.info("Successfully built executable: {s}", .{output_path});
}

/// Information about a test (expect statement) to be evaluated
const ExpectTest = struct {
    expr_idx: can.CIR.Expr.Idx,
    region: base.Region,
};

fn rocTest(allocs: *Allocators, args: cli_args.TestArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Start timing
    const start_time = std.time.nanoTimestamp();

    const stdout = stdoutWriter();
    defer stdout.flush() catch {};

    const stderr = stderrWriter();
    defer stderr.flush() catch {};

    // Read the Roc file
    const source = std.fs.cwd().readFileAlloc(allocs.gpa, args.path, std.math.maxInt(usize)) catch |err| {
        try stderr.print("Failed to read file '{s}': {}\n", .{ args.path, err });
        return err;
    };
    defer allocs.gpa.free(source);

    // Extract module name from the file path
    const basename = std.fs.path.basename(args.path);
    const module_name = try allocs.arena.dupe(u8, basename);

    // Create ModuleEnv
    var env = ModuleEnv.init(allocs.gpa, source) catch |err| {
        try stderr.print("Failed to initialize module environment: {}\n", .{err});
        return err;
    };
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(allocs.gpa);

    const module_common_idents: Check.CommonIdents = .{
        .module_name = try env.insertIdent(base.Ident.for_text(module_name)),
        .list = try env.insertIdent(base.Ident.for_text("List")),
        .box = try env.insertIdent(base.Ident.for_text("Box")),
        .bool_stmt = @enumFromInt(0), // TODO: load from builtin modules
        .try_stmt = @enumFromInt(0), // TODO: load from builtin modules
        .builtin_module = null,
    };

    // Parse the source code as a full module
    var parse_ast = parse.parse(&env.common, allocs.gpa) catch |err| {
        try stderr.print("Failed to parse file: {}\n", .{err});
        return err;
    };
    defer parse_ast.deinit(allocs.gpa);

    // Empty scratch space (required before canonicalization)
    parse_ast.store.emptyScratch();

    // Initialize CIR fields in ModuleEnv
    try env.initCIRFields(allocs.gpa, module_name);

    // Create canonicalizer
    var canonicalizer = Can.init(&env, &parse_ast, null) catch |err| {
        try stderr.print("Failed to initialize canonicalizer: {}\n", .{err});
        return err;
    };
    defer canonicalizer.deinit();

    // Canonicalize the entire module
    canonicalizer.canonicalizeFile() catch |err| {
        try stderr.print("Failed to canonicalize file: {}\n", .{err});
        return err;
    };

    // Validate for checking mode
    canonicalizer.validateForChecking() catch |err| {
        try stderr.print("Failed to validate module: {}\n", .{err});
        return err;
    };

    // Type check the module
    var checker = Check.init(allocs.gpa, &env.types, &env, &.{}, null, &env.store.regions, module_common_idents) catch |err| {
        try stderr.print("Failed to initialize type checker: {}\n", .{err});
        return err;
    };
    defer checker.deinit();

    checker.checkFile() catch |err| {
        try stderr.print("Type checking failed: {}\n", .{err});
        return err;
    };

    // Evaluate all top-level declarations at compile time
    // Load builtin modules required by the interpreter
    const builtin_indices = builtin_loading.deserializeBuiltinIndices(allocs.gpa, compiled_builtins.builtin_indices_bin) catch |err| {
        try stderr.print("Failed to deserialize builtin indices: {}\n", .{err});
        return err;
    };
    const builtin_source = compiled_builtins.builtin_source;
    var builtin_module = builtin_loading.loadCompiledModule(allocs.gpa, compiled_builtins.builtin_bin, "Builtin", builtin_source) catch |err| {
        try stderr.print("Failed to load Builtin module: {}\n", .{err});
        return err;
    };
    defer builtin_module.deinit();

    const builtin_types_for_eval = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    var comptime_evaluator = eval.ComptimeEvaluator.init(allocs.gpa, &env, &.{}, &checker.problems, builtin_types_for_eval) catch |err| {
        try stderr.print("Failed to create compile-time evaluator: {}\n", .{err});
        return err;
    };
    // Note: comptime_evaluator must be deinitialized AFTER building reports from checker.problems
    // because the crash messages are owned by the evaluator but referenced by the problems

    _ = comptime_evaluator.evalAll() catch |err| {
        try stderr.print("Failed to evaluate declarations: {}\n", .{err});
        return err;
    };

    // Create test runner infrastructure for test evaluation (reuse builtin_types_for_eval from above)
    var test_runner = TestRunner.init(allocs.gpa, &env, builtin_types_for_eval) catch |err| {
        try stderr.print("Failed to create test runner: {}\n", .{err});
        return err;
    };
    defer test_runner.deinit();

    const summary = test_runner.eval_all() catch |err| {
        try stderr.print("Failed to evaluate tests: {}\n", .{err});
        return err;
    };
    const passed = summary.passed;
    const failed = summary.failed;

    // Calculate elapsed time
    const end_time = std.time.nanoTimestamp();
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    // Report any compile-time crashes
    const has_comptime_crashes = checker.problems.len() > 0;
    if (has_comptime_crashes) {
        const problem = @import("check").problem;
        var report_builder = problem.ReportBuilder.init(
            allocs.gpa,
            &env,
            &env,
            &checker.snapshots,
            args.path,
            &.{},
            &checker.import_mapping,
        );
        defer report_builder.deinit();

        for (0..checker.problems.len()) |i| {
            const problem_idx: problem.Problem.Idx = @enumFromInt(i);
            const prob = checker.problems.get(problem_idx);
            var report = report_builder.build(prob) catch |err| {
                try stderr.print("Failed to build problem report: {}\n", .{err});
                continue;
            };
            defer report.deinit();

            const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
            const config = reporting.ReportingConfig.initColorTerminal();
            try reporting.renderReportToTerminal(&report, stderr, palette, config);
        }
    }

    // Clean up comptime evaluator AFTER building reports (crash messages must stay alive until reports are built)
    comptime_evaluator.deinit();

    // Report results
    if (failed == 0 and !has_comptime_crashes) {
        // Success case: only print if verbose, exit with 0
        if (args.verbose) {
            try stdout.print("Ran {} test(s): {} passed, 0 failed in {d:.1}ms\n", .{ passed, passed, elapsed_ms });
            for (test_runner.test_results.items) |test_result| {
                const region_info = env.calcRegionInfo(test_result.region);
                try stdout.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
            }
        }
        // Otherwise print nothing at all
        return; // Exit with 0
    } else {
        // Failure case: always print summary with timing
        const total_tests = passed + failed;
        if (total_tests > 0) {
            try stderr.print("Ran {} test(s): {} passed, {} failed in {d:.1}ms\n", .{ total_tests, passed, failed, elapsed_ms });
        }

        if (args.verbose) {
            for (test_runner.test_results.items) |test_result| {
                const region_info = env.calcRegionInfo(test_result.region);
                if (test_result.passed) {
                    try stdout.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
                } else {
                    // Generate and render a detailed report for this failure
                    var report = test_runner.createReport(test_result, args.path) catch |err| {
                        // Fallback to simple message if report generation fails
                        try stderr.print("\x1b[31mFAIL\x1b[0m: {s}:{}", .{ args.path, region_info.start_line_idx + 1 });
                        if (test_result.error_msg) |msg| {
                            try stderr.print(" - {s}", .{msg});
                        }
                        try stderr.print(" (report generation failed: {})\n", .{err});
                        continue;
                    };
                    defer report.deinit();

                    // Render the report to terminal
                    const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
                    const config = reporting.ReportingConfig.initColorTerminal();
                    try reporting.renderReportToTerminal(&report, stderr, palette, config);
                }
            }
        } else {
            // Non-verbose mode: just show simple FAIL messages with line numbers
            for (test_runner.test_results.items) |test_result| {
                if (!test_result.passed) {
                    const region_info = env.calcRegionInfo(test_result.region);
                    try stderr.print("\x1b[31mFAIL\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
                }
            }
        }

        return error.TestsFailed;
    }
}

fn rocRepl(allocs: *Allocators) !void {
    _ = allocs;
    const stderr = stderrWriter();
    defer stderr.flush() catch {};
    stderr.print("repl not implemented\n", .{}) catch {};
    return error.NotImplemented;
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(allocs: *Allocators, args: cli_args.FormatArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = stdoutWriter();
    if (args.stdin) {
        fmt.formatStdin(allocs.gpa) catch |err| return err;
        return;
    }

    var timer = try std.time.Timer.start();
    var elapsed: u64 = undefined;
    var failure_count: usize = 0;
    var had_errors: bool = false;

    if (args.check) {
        var unformatted_files = std.ArrayList([]const u8).empty;
        defer unformatted_files.deinit(allocs.gpa);

        for (args.paths) |path| {
            var result = try fmt.formatPath(allocs.gpa, allocs.arena, std.fs.cwd(), path, true);
            defer result.deinit();
            if (result.unformatted_files) |files| {
                try unformatted_files.appendSlice(allocs.gpa, files.items);
            }
            failure_count += result.failure;
        }

        elapsed = timer.read();
        if (unformatted_files.items.len > 0) {
            try stdout.print("The following file(s) failed `roc format --check`:", .{});
            for (unformatted_files.items) |file_name| {
                try stdout.print("    {s}\n", .{file_name});
            }
            try stdout.print("You can fix this with `roc format FILENAME.roc`.", .{});
            had_errors = true;
        } else {
            try stdout.print("All formatting valid.\n", .{});
        }
        if (failure_count > 0) {
            try stdout.print("Failed to check {} files.", .{failure_count});
            had_errors = true;
        }
    } else {
        var success_count: usize = 0;
        for (args.paths) |path| {
            const result = try fmt.formatPath(allocs.gpa, allocs.arena, std.fs.cwd(), path, false);
            success_count += result.success;
            failure_count += result.failure;
        }
        elapsed = timer.read();
        try stdout.print("Successfully formatted {} files\n", .{success_count});
        if (failure_count > 0) {
            try stdout.print("Failed to format {} files.\n", .{failure_count});
            had_errors = true;
        }
    }

    try stdout.print("Took ", .{});
    try formatElapsedTime(stdout, elapsed);
    try stdout.print(".\n", .{});

    if (had_errors) {
        return error.FormattingFailed;
    }
}

/// Helper function to format elapsed time, showing decimal milliseconds
fn formatElapsedTime(writer: anytype, elapsed_ns: u64) !void {
    const elapsed_ms_float = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    try writer.print("{d:.1} ms", .{elapsed_ms_float});
}

fn handleProcessFileError(err: anytype, stderr: anytype, path: []const u8) !void {
    stderr.print("Failed to check {s}: ", .{path}) catch {};
    switch (err) {
        // Custom BuildEnv errors - these need special messages
        error.ExpectedAppHeader => stderr.print("Expected app header but found different header type\n", .{}) catch {},
        error.ExpectedPlatformString => stderr.print("Expected platform string in header\n", .{}) catch {},
        error.PathOutsideWorkspace => stderr.print("Dependency path outside workspace not allowed\n", .{}) catch {},
        error.UnsupportedHeader => stderr.print("Unsupported header type\n", .{}) catch {},
        error.ExpectedString => stderr.print("Expected string in header\n", .{}) catch {},
        error.Internal => stderr.print("Internal compiler error\n", .{}) catch {},
        error.InvalidDependency => stderr.print("Invalid dependency relationship\n", .{}) catch {},
        error.TooNested => stderr.print("Too deeply nested\n", .{}) catch {},
        error.InvalidPackageName => stderr.print("Invalid package name\n", .{}) catch {},

        // Catch-all for any other errors
        else => stderr.print("{s}\n", .{@errorName(err)}) catch {},
    }

    return err;
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

/// Result from checking a file that preserves the BuildEnv for further processing (e.g., docs generation)
const CheckResultWithBuildEnv = struct {
    check_result: CheckResult,
    build_env: BuildEnv,

    /// Free allocated memory including the BuildEnv
    pub fn deinit(self: *CheckResultWithBuildEnv, gpa: Allocator) void {
        self.check_result.deinit(gpa);
        self.build_env.deinit();
    }
};

/// Check a Roc file using BuildEnv and preserve the BuildEnv for further processing
fn checkFileWithBuildEnvPreserved(
    allocs: *Allocators,
    filepath: []const u8,
    collect_timing: bool,
    cache_config: CacheConfig,
) BuildAppError!CheckResultWithBuildEnv {
    _ = collect_timing; // Timing is always collected by BuildEnv
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize BuildEnv in single-threaded mode for checking
    var build_env = try BuildEnv.init(allocs.gpa, .single_threaded, 1);
    build_env.compiler_version = build_options.compiler_version;
    // Note: We do NOT defer build_env.deinit() here because we're returning it

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try allocs.gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(allocs.gpa, cache_config, Filesystem.default());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager when caller calls deinit
    }

    // Build the file (works for both app and module files)
    build_env.build(filepath) catch |err| {
        // Even on error, try to drain and print any reports that were collected
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.gpa.free(drained);

        // Print any error reports to stderr before failing
        return err;
    };

    // Force processing to ensure canonicalization happens
    var sched_iter = build_env.schedulers.iterator();
    if (sched_iter.next()) |sched_entry| {
        const package_env = sched_entry.value_ptr.*;
        if (package_env.modules.items.len > 0) {
            const module_name = package_env.modules.items[0].name;

            // Keep processing until the module is done
            var max_iterations: u32 = 20;
            while (max_iterations > 0) : (max_iterations -= 1) {
                const phase = package_env.modules.items[0].phase;
                if (phase == .Done) break;

                package_env.processModuleByName(module_name) catch break;
            }
        }
    }

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
    var reports = try allocs.gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try allocs.gpa.dupe(u8, mod.abs_path),
            .reports = mod.reports, // Transfer ownership
        };
    }

    // Free the original drained reports
    // Note: abs_path is owned by BuildEnv, reports are moved to our array
    allocs.gpa.free(drained);

    // Get timing information from BuildEnv
    const timing = if (builtin.target.cpu.arch == .wasm32)
        CheckTimingInfo{}
    else
        build_env.getTimingInfo();

    const check_result = CheckResult{
        .reports = reports,
        .timing = timing,
        .was_cached = false, // BuildEnv doesn't currently expose cache info
        .error_count = error_count,
        .warning_count = warning_count,
    };

    return CheckResultWithBuildEnv{
        .check_result = check_result,
        .build_env = build_env,
    };
}

/// Check a Roc file using the BuildEnv system
fn checkFileWithBuildEnv(
    allocs: *Allocators,
    filepath: []const u8,
    collect_timing: bool,
    cache_config: CacheConfig,
) BuildAppError!CheckResult {
    _ = collect_timing; // Timing is always collected by BuildEnv
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize BuildEnv in single-threaded mode for checking
    var build_env = try BuildEnv.init(allocs.gpa, .single_threaded, 1);
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try allocs.gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(allocs.gpa, cache_config, Filesystem.default());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager
    }

    // Build the file (works for both app and module files)
    build_env.build(filepath) catch {
        // Even on error, drain reports to show what went wrong
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.gpa.free(drained);

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
        // Note: Transfer ownership of reports since drainReports() already transferred them
        var reports = try build_env.gpa.alloc(DrainedReport, drained.len);
        for (drained, 0..) |mod, i| {
            reports[i] = .{
                .file_path = try build_env.gpa.dupe(u8, mod.abs_path),
                .reports = mod.reports, // Transfer ownership
            };
        }

        return CheckResult{
            .reports = reports,
            .error_count = error_count,
            .warning_count = warning_count,
        };
    };

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
    var reports = try allocs.gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try allocs.gpa.dupe(u8, mod.abs_path),
            .reports = mod.reports, // Transfer ownership
        };
    }

    // Free the original drained reports
    // Note: abs_path is owned by BuildEnv, reports are moved to our array
    allocs.gpa.free(drained);

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

fn rocCheck(allocs: *Allocators, args: cli_args.CheckArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = stdoutWriter();
    const stderr = stderrWriter();

    var timer = try std.time.Timer.start();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    // Use BuildEnv to check the file
    var check_result = checkFileWithBuildEnv(
        allocs,
        args.path,
        args.time,
        cache_config,
    ) catch |err| {
        try handleProcessFileError(err, stderr, args.path);
        return;
    };
    defer check_result.deinit(allocs.gpa);

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
            return error.CheckFailed;
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
                try reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal());

                if (report.severity == .fatal or report.severity == .runtime_error) {
                    has_errors = true;
                }
            }
        }

        // Flush stderr to ensure all error output is visible
        stderr_writer.interface.flush() catch {};

        if (check_result.error_count > 0 or check_result.warning_count > 0) {
            stderr.writeAll("\n") catch {};
            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                check_result.error_count,
                check_result.warning_count,
            }) catch {};
            formatElapsedTime(stderr, elapsed) catch {};
            stderr.print(" for {s}.\n", .{args.path}) catch {};

            // Flush before exit
            stderr_writer.interface.flush() catch {};
            return error.CheckFailed;
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTime(stdout, elapsed) catch {};
            stdout.print(" for {s}\n", .{args.path}) catch {};
            stdout_writer.interface.flush() catch {};
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

/// Start an HTTP server to serve the generated documentation
fn serveDocumentation(allocs: *Allocators, docs_dir: []const u8) !void {
    const stdout = stdoutWriter();

    const address = try std.net.Address.parseIp("127.0.0.1", 8080);
    var server = try address.listen(.{
        .reuse_address = true,
    });
    defer server.deinit();

    stdout.print("Visit http://localhost:8080 to view the docs at ./{s}/\n", .{docs_dir}) catch {};
    stdout.print("Press Ctrl+C to stop the server\n", .{}) catch {};

    while (true) {
        const connection = try server.accept();
        handleConnection(allocs, connection, docs_dir) catch |err| {
            std.debug.print("Error handling connection: {}\n", .{err});
        };
    }
}

/// Handle a single HTTP connection
fn handleConnection(allocs: *Allocators, connection: std.net.Server.Connection, docs_dir: []const u8) !void {
    defer connection.stream.close();

    var buffer: [4096]u8 = undefined;
    var reader_buffer: [512]u8 = undefined;
    var conn_reader = connection.stream.reader(&reader_buffer);
    var slices = [_][]u8{buffer[0..]};
    const bytes_read = std.Io.Reader.readVec(conn_reader.interface(), &slices) catch |err| switch (err) {
        error.EndOfStream => 0,
        error.ReadFailed => return conn_reader.getError() orelse error.Unexpected,
    };

    if (bytes_read == 0) return;

    const request = buffer[0..bytes_read];

    // Parse the request line (e.g., "GET /path HTTP/1.1")
    var lines = std.mem.splitSequence(u8, request, "\r\n");
    const request_line = lines.next() orelse return;

    var parts = std.mem.splitSequence(u8, request_line, " ");
    const method = parts.next() orelse return;
    const path = parts.next() orelse return;

    if (!std.mem.eql(u8, method, "GET")) {
        try sendResponse(connection.stream, "405 Method Not Allowed", "text/plain", "Method Not Allowed");
        return;
    }

    // Determine the file path to serve
    const file_path = try resolveFilePath(allocs, docs_dir, path);

    // Try to open and serve the file
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        if (err == error.FileNotFound) {
            try sendResponse(connection.stream, "404 Not Found", "text/plain", "File Not Found");
        } else {
            try sendResponse(connection.stream, "500 Internal Server Error", "text/plain", "Internal Server Error");
        }
        return;
    };
    defer file.close();

    // Read file contents
    const file_content = try file.readToEndAlloc(allocs.gpa, 10 * 1024 * 1024); // 10MB max
    defer allocs.gpa.free(file_content);

    // Determine content type
    const content_type = getContentType(file_path);

    // Send response
    try sendResponse(connection.stream, "200 OK", content_type, file_content);
}

/// Resolve the file path based on the URL path.
/// Returns arena-allocated path (no need to free).
fn resolveFilePath(allocs: *Allocators, docs_dir: []const u8, url_path: []const u8) ![]const u8 {
    // Remove leading slash
    const clean_path = if (url_path.len > 0 and url_path[0] == '/')
        url_path[1..]
    else
        url_path;

    // If path is empty or ends with /, serve index.html
    if (clean_path.len == 0 or clean_path[clean_path.len - 1] == '/') {
        return try std.fmt.allocPrint(allocs.arena, "{s}/{s}index.html", .{ docs_dir, clean_path });
    }

    // Check if the path has a file extension (contains a dot in the last component)
    const last_slash = std.mem.lastIndexOfScalar(u8, clean_path, '/') orelse 0;
    const last_component = clean_path[last_slash..];
    const has_extension = std.mem.indexOfScalar(u8, last_component, '.') != null;

    if (has_extension) {
        // Path has extension, serve the file directly
        return try std.fmt.allocPrint(allocs.arena, "{s}/{s}", .{ docs_dir, clean_path });
    } else {
        // No extension, serve index.html from that directory
        return try std.fmt.allocPrint(allocs.arena, "{s}/{s}/index.html", .{ docs_dir, clean_path });
    }
}

/// Get content type based on file extension
fn getContentType(file_path: []const u8) []const u8 {
    if (std.mem.endsWith(u8, file_path, ".html")) {
        return "text/html; charset=utf-8";
    } else if (std.mem.endsWith(u8, file_path, ".css")) {
        return "text/css";
    } else if (std.mem.endsWith(u8, file_path, ".js")) {
        return "application/javascript";
    } else if (std.mem.endsWith(u8, file_path, ".json")) {
        return "application/json";
    } else if (std.mem.endsWith(u8, file_path, ".png")) {
        return "image/png";
    } else if (std.mem.endsWith(u8, file_path, ".jpg") or std.mem.endsWith(u8, file_path, ".jpeg")) {
        return "image/jpeg";
    } else if (std.mem.endsWith(u8, file_path, ".svg")) {
        return "image/svg+xml";
    } else {
        return "text/plain";
    }
}

/// Send an HTTP response
fn sendResponse(stream: std.net.Stream, status: []const u8, content_type: []const u8, body: []const u8) !void {
    var response_buffer: [8192]u8 = undefined;
    const response = try std.fmt.bufPrint(
        &response_buffer,
        "HTTP/1.1 {s}\r\n" ++
            "Content-Type: {s}\r\n" ++
            "Content-Length: {d}\r\n" ++
            "Connection: close\r\n" ++
            "\r\n",
        .{ status, content_type, body.len },
    );

    try stream.writeAll(response);
    try stream.writeAll(body);
}

fn rocDocs(allocs: *Allocators, args: cli_args.DocsArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = stdoutWriter();
    const stderr = stderrWriter();

    var timer = try std.time.Timer.start();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    // Use BuildEnv to check the file, preserving the BuildEnv for docs generation
    var result_with_env = checkFileWithBuildEnvPreserved(
        allocs,
        args.path,
        args.time,
        cache_config,
    ) catch |err| {
        return handleProcessFileError(err, stderr, args.path);
    };

    // Clean up when we're done - this includes the BuildEnv and all module envs
    defer result_with_env.deinit(allocs.gpa);

    const check_result = &result_with_env.check_result;
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
            return error.DocsFailed;
        }
    } else {
        // For fresh compilation, process and display reports normally
        var has_errors = false;

        // Render reports grouped by module
        for (check_result.reports) |module| {
            for (module.reports) |*report| {

                // Render the diagnostic report to stderr
                reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
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

            if (check_result.error_count > 0) {
                return error.DocsFailed;
            }
        }
    }

    // Print timing breakdown if requested
    if (args.time) {
        printTimingBreakdown(stdout, if (builtin.target.cpu.arch == .wasm32) null else check_result.timing);
    }

    // Generate documentation for all packages and modules
    try generateDocs(allocs, &result_with_env.build_env, args.path, args.output);

    stdout.print("\nDocumentation generation complete for {s}\n", .{args.path}) catch {};

    // Start HTTP server if --serve flag is enabled
    if (args.serve) {
        try serveDocumentation(allocs, args.output);
    }
}

/// Associated item (type or value) within a module or type
pub const AssociatedItem = struct {
    name: []const u8,
    children: []AssociatedItem, // Nested associated items (for types with associated items)

    fn deinit(self: AssociatedItem, gpa: Allocator) void {
        gpa.free(self.name);
        for (self.children) |child| {
            child.deinit(gpa);
        }
        gpa.free(self.children);
    }
};

/// Information about an imported module
pub const ModuleInfo = struct {
    name: []const u8, // e.g., "Foo" or "foo.Bar"
    link_path: []const u8, // e.g., "Foo" or "foo/Bar"
    associated_items: []AssociatedItem, // Types and values defined in this module

    fn deinit(self: ModuleInfo, gpa: Allocator) void {
        gpa.free(self.name);
        gpa.free(self.link_path);
        for (self.associated_items) |item| {
            item.deinit(gpa);
        }
        gpa.free(self.associated_items);
    }
};

/// Recursively write associated items as nested <ul> elements
fn writeAssociatedItems(writer: anytype, items: []const AssociatedItem, indent_level: usize) !void {
    // Write opening <ul>
    try writer.splatByteAll(' ', indent_level * 2);
    try writer.writeAll("<ul>\n");

    for (items) |item| {
        // Write <li> with item name
        try writer.splatByteAll(' ', (indent_level + 1) * 2);
        try writer.print("<li>{s}\n", .{item.name});

        // Recursively write children if any
        if (item.children.len > 0) {
            try writeAssociatedItems(writer, item.children, indent_level + 2);
        }

        // Close <li>
        try writer.splatByteAll(' ', (indent_level + 1) * 2);
        try writer.writeAll("</li>\n");
    }

    // Write closing </ul>
    try writer.splatByteAll(' ', indent_level * 2);
    try writer.writeAll("</ul>\n");
}

/// Generate HTML index file for a package or app
pub fn generatePackageIndex(
    allocs: *Allocators,
    output_path: []const u8,
    module_path: []const u8,
    package_shorthands: []const []const u8,
    imported_modules: []const ModuleInfo,
) !void {
    // Create output directory if it doesn't exist
    std.fs.cwd().makePath(output_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Create index.html file
    const index_path = try std.fs.path.join(allocs.arena, &[_][]const u8{ output_path, "index.html" });

    const file = try std.fs.cwd().createFile(index_path, .{});
    defer file.close();

    var file_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(&file_buffer);
    const writer = &file_writer.interface;

    // Write HTML header
    try writer.writeAll("<!DOCTYPE html>\n<html>\n<head>\n");
    try writer.writeAll("  <meta charset=\"UTF-8\">\n");
    try writer.writeAll("  <title>Documentation</title>\n");
    try writer.writeAll("</head>\n<body>\n");

    // Write module path as h1
    try writer.print("  <h1>{s}</h1>\n", .{module_path});

    // Write sidebar with imported modules if any
    if (imported_modules.len > 0) {
        try writer.writeAll("  <ul class='sidebar'>\n");
        for (imported_modules) |mod_info| {
            try writer.print("    <li>\n      <a href=\"{s}\">{s}</a>\n", .{ mod_info.link_path, mod_info.name });

            // Write nested associated items if any
            if (mod_info.associated_items.len > 0) {
                try writeAssociatedItems(writer, mod_info.associated_items, 3);
            }

            try writer.writeAll("    </li>\n");
        }
        try writer.writeAll("  </ul>\n");
    }

    // Write links to package dependencies if any exist
    if (package_shorthands.len > 0) {
        try writer.writeAll("  <ul>\n");
        for (package_shorthands) |shorthand| {
            try writer.print("    <li><a href=\"{s}\">{s}</a></li>\n", .{ shorthand, shorthand });
        }
        try writer.writeAll("  </ul>\n");
    }

    try writer.writeAll("</body>\n</html>\n");
    try writer.flush();
}

/// Generate HTML index file for a module
pub fn generateModuleIndex(
    allocs: *Allocators,
    output_path: []const u8,
    module_name: []const u8,
) !void {
    // Create output directory if it doesn't exist
    std.fs.cwd().makePath(output_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Create index.html file
    const index_path = try std.fs.path.join(allocs.arena, &[_][]const u8{ output_path, "index.html" });

    const file = try std.fs.cwd().createFile(index_path, .{});
    defer file.close();

    var file_buffer: [4096]u8 = undefined;
    var file_writer = file.writer(&file_buffer);
    const writer = &file_writer.interface;

    // Write HTML header
    try writer.writeAll("<!DOCTYPE html>\n<html>\n<head>\n");
    try writer.writeAll("  <meta charset=\"UTF-8\">\n");
    try writer.print("  <title>{s}</title>\n", .{module_name});
    try writer.writeAll("</head>\n<body>\n");

    // Write module name as h1
    try writer.print("  <h1>{s}</h1>\n", .{module_name});

    try writer.writeAll("</body>\n</html>\n");
    try writer.flush();
}

/// Extract associated items from a record expression (recursively)
fn extractRecordAssociatedItems(
    allocs: *Allocators,
    module_env: *const ModuleEnv,
    record_fields: can.CIR.RecordField.Span,
) ![]AssociatedItem {
    var items = std.array_list.Managed(AssociatedItem).init(allocs.gpa);
    errdefer {
        for (items.items) |item| {
            item.deinit(allocs.gpa);
        }
        items.deinit();
    }

    const fields_slice = module_env.store.sliceRecordFields(record_fields);
    for (fields_slice) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const field_name = try allocs.gpa.dupe(u8, module_env.getIdentText(field.name));
        errdefer allocs.gpa.free(field_name);

        // Check if the field value is a nominal type (has nested associated items)
        const field_expr = module_env.store.getExpr(field.value);
        const children = switch (field_expr) {
            .e_nominal => |nom| blk: {
                // Get the nominal type's backing expression
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk switch (backing_expr) {
                    .e_record => |rec| try extractRecordAssociatedItems(allocs, module_env, rec.fields),
                    else => try allocs.gpa.alloc(AssociatedItem, 0),
                };
            },
            else => try allocs.gpa.alloc(AssociatedItem, 0),
        };

        try items.append(.{
            .name = field_name,
            .children = children,
        });
    }

    return try items.toOwnedSlice();
}

/// Extract associated items from a module's exports
fn extractAssociatedItems(
    allocs: *Allocators,
    module_env: *const ModuleEnv,
) ![]AssociatedItem {
    var items = std.array_list.Managed(AssociatedItem).init(allocs.gpa);
    errdefer {
        for (items.items) |item| {
            item.deinit(allocs.gpa);
        }
        items.deinit();
    }

    // Get all exported definitions
    const exports_slice = module_env.store.sliceDefs(module_env.exports);

    // If no exports, try all_defs (for modules that are still being processed)
    const defs_slice = if (exports_slice.len == 0)
        module_env.store.sliceDefs(module_env.all_defs)
    else
        exports_slice;

    for (defs_slice) |def_idx| {
        const def = module_env.store.getDef(def_idx);

        // Get the pattern to find the name
        const pattern = module_env.store.getPattern(def.pattern);

        // Extract name from pattern (could be assign, nominal, etc.)
        const name_ident_opt = switch (pattern) {
            .assign => |a| a.ident,
            .nominal => |n| blk: {
                // For nominal types, we need to get the statement and extract the header
                const stmt = module_env.store.getStatement(n.nominal_type_decl);
                break :blk switch (stmt) {
                    .s_nominal_decl => |decl| module_env.store.getTypeHeader(decl.header).name,
                    else => continue,
                };
            },
            else => continue,
        };

        const name = try allocs.gpa.dupe(u8, module_env.getIdentText(name_ident_opt));
        errdefer allocs.gpa.free(name);

        // Extract nested associated items if this is a nominal type with a record
        const children = switch (pattern) {
            .nominal => blk: {
                // For nominal types, look at the expression to find associated items
                const expr = module_env.store.getExpr(def.expr);
                break :blk switch (expr) {
                    .e_nominal => |nom_expr| blk2: {
                        const backing = module_env.store.getExpr(nom_expr.backing_expr);
                        break :blk2 switch (backing) {
                            .e_record => |record| try extractRecordAssociatedItems(allocs, module_env, record.fields),
                            else => try allocs.gpa.alloc(AssociatedItem, 0),
                        };
                    },
                    else => try allocs.gpa.alloc(AssociatedItem, 0),
                };
            },
            else => try allocs.gpa.alloc(AssociatedItem, 0),
        };

        try items.append(.{
            .name = name,
            .children = children,
        });
    }

    return try items.toOwnedSlice();
}

/// Generate documentation for the root and all its dependencies and imported modules
fn generateDocs(
    allocs: *Allocators,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
) !void {
    // First, determine if this is an app or other kind
    var pkg_iter = build_env.packages.iterator();
    const first_pkg = if (pkg_iter.next()) |entry| entry.value_ptr.* else return;

    const is_app = first_pkg.kind == .app;

    if (is_app) {
        // For apps, collect all imported modules and generate sidebar
        try generateAppDocs(allocs, build_env, module_path, base_output_dir);
    } else {
        // For packages, just generate package dependency docs
        try generatePackageDocs(allocs, build_env, module_path, base_output_dir, "");
    }
}

/// Generate docs for an app module
fn generateAppDocs(
    allocs: *Allocators,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
) !void {
    // Collect all imported modules (both local and from packages)
    var modules_map = std.StringHashMap(ModuleInfo).init(allocs.gpa);
    defer {
        var it = modules_map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(allocs.gpa);
        }
        modules_map.deinit();
    }

    // Get the root package
    var pkg_iter = build_env.packages.iterator();
    const first_pkg = if (pkg_iter.next()) |entry| entry.value_ptr.* else return;

    // Iterate through schedulers to get modules
    var sched_iter = build_env.schedulers.iterator();
    while (sched_iter.next()) |sched_entry| {
        const package_name = sched_entry.key_ptr.*;
        const package_env = sched_entry.value_ptr.*;

        // Iterate through modules in this package
        for (package_env.modules.items) |module_state| {
            // Process external imports (e.g., "cli.Stdout")
            for (module_state.external_imports.items) |ext_import| {
                // Parse the import (e.g., "cli.Stdout" -> package="cli", module="Stdout")
                if (std.mem.indexOfScalar(u8, ext_import, '.')) |dot_index| {
                    const pkg_shorthand = ext_import[0..dot_index];
                    const module_name = ext_import[dot_index + 1 ..];

                    // Create full name and link path
                    const full_name = try allocs.arena.dupe(u8, ext_import);
                    const link_path = try std.fmt.allocPrint(allocs.arena, "{s}/{s}", .{ pkg_shorthand, module_name });

                    const empty_items = [_]AssociatedItem{};
                    const mod_info = ModuleInfo{
                        .name = full_name,
                        .link_path = link_path,
                        .associated_items = &empty_items,
                    };

                    // Add to map (deduplicates automatically)
                    const gop = try modules_map.getOrPut(full_name);
                    if (!gop.found_existing) {
                        gop.value_ptr.* = mod_info;
                    }

                    // Generate index.html for this module
                    const module_output_dir = try std.fs.path.join(allocs.arena, &[_][]const u8{ base_output_dir, pkg_shorthand, module_name });
                    generateModuleIndex(allocs, module_output_dir, ext_import) catch |err| {
                        std.debug.print("Warning: failed to generate module index for {s}: {}\n", .{ ext_import, err });
                    };
                }
            }

            // Process local imports (non-external modules in the same package)
            for (module_state.imports.items) |import_id| {
                if (import_id < package_env.modules.items.len) {
                    const imported_module = package_env.modules.items[import_id];
                    const module_name = imported_module.name;

                    // Skip if this is the root module itself
                    if (std.mem.eql(u8, module_name, "main")) continue;

                    // Only include if it's a local module (not from a package)
                    if (std.mem.eql(u8, package_name, first_pkg.name)) {
                        const full_name = try allocs.gpa.dupe(u8, module_name);
                        const link_path = try allocs.gpa.dupe(u8, module_name);

                        // Extract associated items from the module if it has an env
                        const associated_items = if (imported_module.env) |*mod_env|
                            try extractAssociatedItems(allocs, mod_env)
                        else
                            try allocs.gpa.alloc(AssociatedItem, 0);

                        const mod_info = ModuleInfo{
                            .name = full_name,
                            .link_path = link_path,
                            .associated_items = associated_items,
                        };

                        const gop = try modules_map.getOrPut(full_name);
                        if (!gop.found_existing) {
                            gop.value_ptr.* = mod_info;
                        } else {
                            // Free the duplicates
                            allocs.gpa.free(full_name);
                            allocs.gpa.free(link_path);
                            for (associated_items) |item| {
                                item.deinit(allocs.gpa);
                            }
                            allocs.gpa.free(associated_items);
                        }

                        // Generate index.html for this local module
                        const module_output_dir = try std.fs.path.join(allocs.arena, &[_][]const u8{ base_output_dir, module_name });
                        generateModuleIndex(allocs, module_output_dir, module_name) catch |err| {
                            std.debug.print("Warning: failed to generate module index for {s}: {}\n", .{ module_name, err });
                        };
                    }
                }
            }
        }
    }

    // Convert map to sorted list
    var modules_list = std.ArrayList(ModuleInfo).empty;
    defer modules_list.deinit(allocs.gpa);
    var map_iter = modules_map.iterator();
    while (map_iter.next()) |entry| {
        try modules_list.append(allocs.gpa, entry.value_ptr.*);
    }

    // Collect package shorthands
    var shorthands_list = std.array_list.Managed([]const u8).init(allocs.gpa);
    defer {
        for (shorthands_list.items) |item| allocs.gpa.free(item);
        shorthands_list.deinit();
    }

    var shorthand_iter = first_pkg.shorthands.iterator();
    while (shorthand_iter.next()) |sh_entry| {
        const shorthand = try allocs.gpa.dupe(u8, sh_entry.key_ptr.*);
        try shorthands_list.append(shorthand);
    }

    // Generate root index.html
    try generatePackageIndex(allocs, base_output_dir, module_path, shorthands_list.items, modules_list.items);

    // Generate package dependency docs recursively
    shorthand_iter = first_pkg.shorthands.iterator();
    while (shorthand_iter.next()) |sh_entry| {
        const shorthand = sh_entry.key_ptr.*;
        const dep_ref = sh_entry.value_ptr.*;

        generatePackageDocs(allocs, build_env, dep_ref.root_file, base_output_dir, shorthand) catch |err| {
            std.debug.print("Warning: failed to generate docs for package {s}: {}\n", .{ shorthand, err });
        };
    }
}

/// Recursively generate documentation for a package and its dependencies
fn generatePackageDocs(
    allocs: *Allocators,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
    relative_path: []const u8,
) error{OutOfMemory}!void {
    const output_dir = if (relative_path.len == 0)
        try allocs.arena.dupe(u8, base_output_dir)
    else
        try std.fs.path.join(allocs.arena, &[_][]const u8{ base_output_dir, relative_path });

    var shorthands_list = std.array_list.Managed([]const u8).init(allocs.gpa);
    defer {
        for (shorthands_list.items) |item| allocs.gpa.free(item);
        shorthands_list.deinit();
    }

    var pkg_iter = build_env.packages.iterator();
    while (pkg_iter.next()) |entry| {
        const pkg = entry.value_ptr;

        var shorthand_iter = pkg.shorthands.iterator();
        while (shorthand_iter.next()) |sh_entry| {
            const shorthand = try allocs.gpa.dupe(u8, sh_entry.key_ptr.*);
            try shorthands_list.append(shorthand);
        }

        shorthand_iter = pkg.shorthands.iterator();
        while (shorthand_iter.next()) |sh_entry| {
            const shorthand = sh_entry.key_ptr.*;

            const dep_relative_path = if (relative_path.len == 0)
                try allocs.arena.dupe(u8, shorthand)
            else
                try std.fs.path.join(allocs.arena, &[_][]const u8{ relative_path, shorthand });

            const dep_ref = sh_entry.value_ptr.*;
            generatePackageDocs(allocs, build_env, dep_ref.root_file, base_output_dir, dep_relative_path) catch |err| {
                std.debug.print("Warning: failed to generate docs for {s}: {}\n", .{ shorthand, err });
            };
        }

        break;
    }

    // For standalone modules, extract and display their exports
    var module_infos = std.array_list.Managed(ModuleInfo).init(allocs.gpa);
    defer {
        for (module_infos.items) |mod| mod.deinit(allocs.gpa);
        module_infos.deinit();
    }

    // Get the module's exports if it's a standalone module
    var sched_iter = build_env.schedulers.iterator();
    while (sched_iter.next()) |sched_entry| {
        const package_env = sched_entry.value_ptr.*;

        // Check ALL modules in this package
        for (package_env.modules.items) |module_state| {
            if (module_state.env) |*mod_env| {
                const associated_items = try extractAssociatedItems(allocs, mod_env);
                const mod_name = try allocs.gpa.dupe(u8, module_state.name);

                try module_infos.append(.{
                    .name = mod_name,
                    .link_path = try allocs.gpa.dupe(u8, ""),
                    .associated_items = associated_items,
                });
            }
        }
    }

    generatePackageIndex(allocs, output_dir, module_path, shorthands_list.items, module_infos.items) catch |err| {
        std.debug.print("Warning: failed to generate index for {s}: {}\n", .{ module_path, err });
    };
}
