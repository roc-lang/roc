//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`

const std = @import("std");

/// Configure std library logging to suppress debug messages in production.
/// This prevents debug logs from polluting stderr which should only contain
/// actual program output (like Stderr.line! calls).
pub const std_options: std.Options = .{
    .log_level = .warn,
};
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
const lsp = @import("lsp");
const compiled_builtins = @import("compiled_builtins");
const builtin_loading = eval.builtin_loading;
const BuiltinTypes = eval.BuiltinTypes;

const cli_args = @import("cli_args.zig");
const roc_target = @import("target.zig");

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

var windows_console_configured = false;
var windows_console_previous_code_page: ?std.os.windows.UINT = null;

fn ensureWindowsConsoleSupportsAnsiAndUtf8() void {
    if (!is_windows) return;
    if (windows_console_configured) return;
    windows_console_configured = true;

    // Ensure the legacy console interprets escape sequences and UTF-8 output.
    const kernel32 = std.os.windows.kernel32;
    const current_code_page = kernel32.GetConsoleOutputCP();
    if (current_code_page != 0 and current_code_page != 65001) {
        windows_console_previous_code_page = current_code_page;
        _ = kernel32.SetConsoleOutputCP(65001);
    }
    _ = std.fs.File.stdout().getOrEnableAnsiEscapeSupport();
    _ = std.fs.File.stderr().getOrEnableAnsiEscapeSupport();
}

fn restoreWindowsConsoleCodePage() void {
    if (!is_windows) return;
    if (windows_console_previous_code_page) |code_page| {
        windows_console_previous_code_page = null;
        _ = std.os.windows.kernel32.SetConsoleOutputCP(code_page);
    }
}

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
const ReportBuilder = check.ReportBuilder;

const legalDetailsFileContent = @embedFile("legal_details");

/// Render type checking problems as diagnostic reports to stderr.
/// Returns the count of errors (fatal/runtime_error severity).
/// This is shared between rocCheck and rocRun to ensure consistent error reporting.
fn renderTypeProblems(
    gpa: Allocator,
    checker: *Check,
    module_env: *ModuleEnv,
    filename: []const u8,
) usize {
    const stderr = stderrWriter();

    var rb = ReportBuilder.init(
        gpa,
        module_env,
        module_env,
        &checker.snapshots,
        filename,
        &.{},
        &checker.import_mapping,
    );
    defer rb.deinit();

    var error_count: usize = 0;
    var warning_count: usize = 0;

    // Render canonicalization diagnostics (unused variables, etc.)
    // Note: getDiagnostics allocates with module_env.gpa, so we must free with that allocator
    const diags = module_env.getDiagnostics() catch &.{};
    defer module_env.gpa.free(diags);
    for (diags) |d| {
        var report = module_env.diagnosticToReport(d, module_env.gpa, filename) catch continue;
        defer report.deinit();

        reporting.renderReportToTerminal(&report, stderr, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch continue;

        if (report.severity == .fatal or report.severity == .runtime_error) {
            error_count += 1;
        } else if (report.severity == .warning) {
            warning_count += 1;
        }
    }

    // Render type checking problems
    for (checker.problems.problems.items) |prob| {
        var report = rb.build(prob) catch continue;
        defer report.deinit();

        // Render the diagnostic report to stderr
        reporting.renderReportToTerminal(&report, stderr, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch continue;

        if (report.severity == .fatal or report.severity == .runtime_error) {
            error_count += 1;
        } else if (report.severity == .warning) {
            warning_count += 1;
        }
    }

    // Print summary if there were any problems
    if (error_count > 0 or warning_count > 0) {
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) for {s}.\n", .{
            error_count,
            warning_count,
            filename,
        }) catch {};
    }

    // Flush stderr to ensure all error output is visible
    stderr_writer.interface.flush() catch {};

    return error_count;
}

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
            ) callconv(.winapi) std.os.windows.BOOL;
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

/// Create a unique temporary directory with PID-based naming.
/// Returns the path to the directory (allocated from arena, no need to free).
/// Uses system temp directory to avoid race conditions when cache is cleared.
pub fn createUniqueTempDir(allocs: *Allocators) ![]const u8 {
    // Use system temp directory (not roc cache) to avoid race conditions
    const temp_dir = if (comptime is_windows)
        std.process.getEnvVarOwned(allocs.arena, "TEMP") catch
            std.process.getEnvVarOwned(allocs.arena, "TMP") catch try allocs.arena.dupe(u8, "C:\\Windows\\Temp")
    else
        std.process.getEnvVarOwned(allocs.arena, "TMPDIR") catch try allocs.arena.dupe(u8, "/tmp");

    const normalized_temp_dir = if (comptime is_windows)
        std.mem.trimRight(u8, temp_dir, "/\\")
    else
        std.mem.trimRight(u8, temp_dir, "/");

    // Get the current process ID for uniqueness
    const pid = if (comptime is_windows)
        std.os.windows.GetCurrentProcessId()
    else
        std.c.getpid();

    // Try PID-based name first, then fall back to random suffix up to 5 times
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const dir_path = if (attempt == 0) blk: {
            // First attempt: use PID only
            break :blk if (comptime is_windows)
                try std.fmt.allocPrint(allocs.arena, "{s}\\roc-{d}", .{ normalized_temp_dir, pid })
            else
                try std.fmt.allocPrint(allocs.arena, "{s}/roc-{d}", .{ normalized_temp_dir, pid });
        } else blk: {
            // Subsequent attempts: use PID + random 8-char suffix
            const random_suffix = try generateRandomSuffix(allocs);
            break :blk if (comptime is_windows)
                try std.fmt.allocPrint(allocs.arena, "{s}\\roc-{d}-{s}", .{ normalized_temp_dir, pid, random_suffix })
            else
                try std.fmt.allocPrint(allocs.arena, "{s}/roc-{d}-{s}", .{ normalized_temp_dir, pid, random_suffix });
        };

        // Try to create the directory
        std.fs.cwd().makeDir(dir_path) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // Directory already exists, try again with a new random suffix
                continue;
            },
            else => {
                return err;
            },
        };

        return dir_path;
    }

    // Failed after 6 attempts (1 with PID only, 5 with PID + random suffix)
    return error.FailedToCreateUniqueTempDir;
}

/// Write shared memory coordination file (.txt) next to the executable.
/// This is the file that the child process reads to find the shared memory fd.
pub fn writeFdCoordinationFile(allocs: *Allocators, temp_exe_path: []const u8, shm_handle: SharedMemoryHandle) !void {
    // The coordination file is at {temp_dir}.txt where temp_dir is the directory containing the exe
    const temp_dir = std.fs.path.dirname(temp_exe_path) orelse return error.InvalidPath;

    // Ensure we have no trailing slashes
    var dir_path = temp_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = try std.fmt.allocPrint(allocs.arena, "{s}.txt", .{dir_path});

    // Create the file (exclusive - fail if exists to detect collisions)
    const fd_file = std.fs.cwd().createFile(fd_file_path, .{ .exclusive = true }) catch |err| switch (err) {
        error.PathAlreadyExists => {
            // File already exists - this is unexpected since we have unique temp dirs
            std.log.err("Coordination file already exists at '{s}'", .{fd_file_path});
            return err;
        },
        else => return err,
    };
    defer fd_file.close();

    // Write shared memory info to file
    const fd_str = try std.fmt.allocPrint(allocs.arena, "{}\n{}", .{ shm_handle.fd, shm_handle.size });
    try fd_file.writeAll(fd_str);
    try fd_file.sync();
}

/// Create the temporary directory structure for fd communication.
/// Returns the path to the executable in the temp directory (allocated from arena, no need to free).
/// If a cache directory is provided, it will be used for temporary files; otherwise
/// falls back to the system temp directory.
/// The exe_display_name is the name that will appear in `ps` output (e.g., "app.roc").
pub fn createTempDirStructure(allocs: *Allocators, exe_path: []const u8, exe_display_name: []const u8, shm_handle: SharedMemoryHandle, cache_dir: ?[]const u8) ![]const u8 {
    // Use provided cache dir or fall back to system temp directory
    const temp_dir = if (cache_dir) |dir|
        try allocs.arena.dupe(u8, dir)
    else if (comptime is_windows)
        std.process.getEnvVarOwned(allocs.arena, "TEMP") catch
            std.process.getEnvVarOwned(allocs.arena, "TMP") catch try allocs.arena.dupe(u8, "C:\\Windows\\Temp")
    else
        std.process.getEnvVarOwned(allocs.arena, "TMPDIR") catch try allocs.arena.dupe(u8, "/tmp");

    const normalized_temp_dir = if (comptime is_windows)
        std.mem.trimRight(u8, temp_dir, "/\\")
    else
        std.mem.trimRight(u8, temp_dir, "/");

    // Get the current process ID for uniqueness
    const pid = if (comptime is_windows)
        std.os.windows.GetCurrentProcessId()
    else
        std.c.getpid();

    // Try PID-based name first, then fall back to random suffix up to 5 times
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const dir_name_with_txt = if (attempt == 0) blk: {
            // First attempt: use PID only
            break :blk if (comptime is_windows)
                try std.fmt.allocPrint(allocs.arena, "{s}\\roc-{d}.txt", .{ normalized_temp_dir, pid })
            else
                try std.fmt.allocPrint(allocs.arena, "{s}/roc-{d}.txt", .{ normalized_temp_dir, pid });
        } else blk: {
            // Subsequent attempts: use PID + random 8-char suffix
            const random_suffix = try generateRandomSuffix(allocs);
            break :blk if (comptime is_windows)
                try std.fmt.allocPrint(allocs.arena, "{s}\\roc-{d}-{s}.txt", .{ normalized_temp_dir, pid, random_suffix })
            else
                try std.fmt.allocPrint(allocs.arena, "{s}/roc-{d}-{s}.txt", .{ normalized_temp_dir, pid, random_suffix });
        };

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

        // Create hardlink to executable in temp directory with display name
        const temp_exe_path = try std.fs.path.join(allocs.arena, &.{ temp_dir_path, exe_display_name });

        // Try to create a hardlink first (more efficient than copying)
        createHardlink(allocs, exe_path, temp_exe_path) catch {
            // If hardlinking fails for any reason, fall back to copying
            // Common reasons: cross-device link, permissions, file already exists
            try std.fs.cwd().copyFile(exe_path, std.fs.cwd(), temp_exe_path, .{});
        };

        return temp_exe_path;
    }

    // Failed after 6 attempts (1 with PID only, 5 with PID + random suffix)
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
    defer restoreWindowsConsoleCodePage();
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
        restoreWindowsConsoleCodePage();
        std.process.exit(1);
    };

    if (tracy.enable) {
        try tracy.waitForShutdown();
    }
}

fn mainArgs(allocs: *Allocators, args: []const []const u8) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    ensureWindowsConsoleSupportsAnsiAndUtf8();

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
        .experimental_lsp => |lsp_args| try lsp.runWithStdIo(allocs.gpa, .{
            .transport = lsp_args.debug_io,
            .build = lsp_args.debug_build,
            .syntax = lsp_args.debug_syntax,
            .server = lsp_args.debug_server,
        }),
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
    // Note: Symbol names include platform-specific prefixes (underscore for macOS)
    platform_host_shim.createInterpreterShim(&llvm_builder, entrypoints.items, target) catch |err| {
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

    // The final executable name seen in `ps` is the roc filename (e.g., "app.roc")
    const exe_display_name = std.fs.path.basename(args.path);

    // Display name for temp directory (what shows in ps)
    const exe_display_name_with_ext = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(allocs.arena, "{s}.exe", .{exe_display_name}) catch |err| {
            std.log.err("Failed to generate display name with extension: {}", .{err});
            return err;
        }
    else
        allocs.arena.dupe(u8, exe_display_name) catch |err| {
            std.log.err("Failed to duplicate display name: {}", .{err});
            return err;
        };

    // Cache executable name uses hash of path (no PID - collision is fine since same content)
    const exe_cache_name = std.fmt.allocPrint(allocs.arena, "roc_{x}", .{std.hash.crc.Crc32.hash(args.path)}) catch |err| {
        std.log.err("Failed to generate cache executable name: {}", .{err});
        return err;
    };

    const exe_cache_name_with_ext = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(allocs.arena, "{s}.exe", .{exe_cache_name}) catch |err| {
            std.log.err("Failed to generate cache name with extension: {}", .{err});
            return err;
        }
    else
        allocs.arena.dupe(u8, exe_cache_name) catch |err| {
            std.log.err("Failed to duplicate cache name: {}", .{err});
            return err;
        };

    const exe_cache_path = std.fs.path.join(allocs.arena, &.{ exe_cache_dir, exe_cache_name_with_ext }) catch |err| {
        std.log.err("Failed to create cache executable path: {}", .{err});
        return err;
    };

    // Create unique temp directory for this build (uses PID for uniqueness)
    const temp_dir_path = createUniqueTempDir(allocs) catch |err| {
        std.log.err("Failed to create temp directory: {}", .{err});
        return err;
    };

    // The executable is built directly in the temp dir with the display name
    const exe_path = std.fs.path.join(allocs.arena, &.{ temp_dir_path, exe_display_name_with_ext }) catch |err| {
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

    // Check if the interpreter executable already exists in cache
    const cache_exists = if (args.no_cache) false else blk: {
        std.fs.accessAbsolute(exe_cache_path, .{}) catch {
            break :blk false;
        };
        break :blk true;
    };

    if (cache_exists) {
        // Cached executable exists - hardlink from cache to temp dir
        std.log.debug("Using cached executable: {s}", .{exe_cache_path});
        createHardlink(allocs, exe_cache_path, exe_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink from cache failed, copying: {}", .{err});
            std.fs.cwd().copyFile(exe_cache_path, std.fs.cwd(), exe_path, .{}) catch |copy_err| {
                std.log.err("Failed to copy cached executable: {}", .{copy_err});
                return copy_err;
            };
        };
    } else {

        // Extract shim library to temp dir to avoid race conditions
        const shim_filename = if (builtin.target.os.tag == .windows) "roc_shim.lib" else "libroc_shim.a";
        const shim_path = std.fs.path.join(allocs.arena, &.{ temp_dir_path, shim_filename }) catch |err| {
            std.log.err("Failed to create shim library path: {}", .{err});
            return err;
        };

        // Always extract to temp dir (unique per process, no race condition)
        extractReadRocFilePathShimLibrary(allocs, shim_path) catch |err| {
            std.log.err("Failed to extract read roc file path shim library: {}", .{err});
            return err;
        };

        // Generate platform host shim using the detected entrypoints
        // Use temp dir to avoid race conditions when multiple processes run in parallel

        const platform_shim_path = generatePlatformHostShim(allocs, temp_dir_path, entrypoints.items, shim_target) catch |err| {
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

        // Determine platform-specific dependencies based on platform spec
        var platform_files_pre = std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16) catch |err| {
            std.log.err("Failed to allocate platform files pre list: {}", .{err});
            return err;
        };
        var platform_files_post = std.array_list.Managed([]const u8).initCapacity(allocs.arena, 16) catch |err| {
            std.log.err("Failed to allocate platform files post list: {}", .{err});
            return err;
        };
        var target_abi: ?linker.TargetAbi = null;

        // Determine platform type from host library path to configure dependencies
        std.log.debug("Platform host library path: {s}", .{platform_paths.host_lib_path});

        // Get the directory containing the host library - this is where sibling files
        // (crt1.o, libc.a, etc.) provided by the platform author should be located.
        // Works for both local platforms (test/int/platform/targets/x64musl/)
        // and cached URL platforms (~/.cache/roc/packages/HASH/targets/x64musl/)
        const host_lib_dir = std.fs.path.dirname(platform_paths.host_lib_path) orelse ".";

        // Detect if this is a musl platform by parsing the directory name as a RocTarget
        // This handles exact matches like "x64musl", "arm64musl" from target.zig
        const host_lib_dirname = std.fs.path.basename(host_lib_dir);
        const parsed_target = roc_target.RocTarget.fromString(host_lib_dirname);
        const is_musl_platform = if (parsed_target) |t| t.isStatic() else false;

        if (is_musl_platform) {
            std.log.debug("Detected musl platform target: {s}", .{host_lib_dirname});
            target_abi = .musl;

            if (builtin.target.os.tag == .linux) {
                // Look for CRT files in the same directory as libhost.a
                // These are provided by the platform author in their bundle
                const crt1_path = std.fs.path.join(allocs.arena, &.{ host_lib_dir, "crt1.o" }) catch |err| {
                    std.log.err("Failed to allocate crt1 path", .{});
                    return err;
                };
                const libc_path = std.fs.path.join(allocs.arena, &.{ host_lib_dir, "libc.a" }) catch |err| {
                    std.log.err("Failed to allocate libc path", .{});
                    return err;
                };

                // Only add files if they exist (platform may not need all of them)
                if (std.fs.cwd().access(crt1_path, .{})) |_| {
                    platform_files_pre.append(crt1_path) catch |err| {
                        std.log.err("Failed to add musl crt1.o", .{});
                        return err;
                    };
                } else |_| {
                    std.log.debug("crt1.o not found in {s}, skipping", .{host_lib_dir});
                }

                if (std.fs.cwd().access(libc_path, .{})) |_| {
                    platform_files_post.append(libc_path) catch |err| {
                        std.log.err("Failed to add musl libc.a", .{});
                        return err;
                    };
                } else |_| {
                    std.log.debug("libc.a not found in {s}, skipping", .{host_lib_dir});
                }
            }
        } else {
            // Non-musl platform - use system defaults
            std.log.debug("No musl platform detected, using defaults", .{});
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

        // After building, hardlink to cache for future runs
        // Force-hardlink (delete existing first) since hash collision means identical content
        std.log.debug("Caching executable to: {s}", .{exe_cache_path});
        std.fs.cwd().deleteFile(exe_cache_path) catch |err| switch (err) {
            error.FileNotFound => {}, // OK, doesn't exist
            else => std.log.debug("Could not delete existing cache file: {}", .{err}),
        };
        createHardlink(allocs, exe_path, exe_cache_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink to cache failed, copying: {}", .{err});
            std.fs.cwd().copyFile(exe_path, std.fs.cwd(), exe_cache_path, .{}) catch |copy_err| {
                // Non-fatal - just means future runs won't be cached
                std.log.debug("Failed to copy to cache: {}", .{copy_err});
            };
        };
    }

    // Set up shared memory with ModuleEnv
    std.log.debug("Setting up shared memory for Roc file: {s}", .{args.path});
    const shm_result = setupSharedMemoryWithModuleEnv(allocs, args.path) catch |err| {
        std.log.err("Failed to set up shared memory with ModuleEnv: {}", .{err});
        return err;
    };
    std.log.debug("Shared memory setup complete, size: {} bytes", .{shm_result.handle.size});

    // Check for errors - abort unless --allow-errors flag is set
    if (shm_result.error_count > 0 and !args.allow_errors) {
        return error.TypeCheckingFailed;
    }

    const shm_handle = shm_result.handle;

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
        runWithWindowsHandleInheritance(allocs, exe_path, shm_handle, args.app_args) catch |err| {
            return err;
        };
    } else {
        // POSIX: Use existing file descriptor inheritance approach
        std.log.debug("Using POSIX file descriptor inheritance approach", .{});
        runWithPosixFdInheritance(allocs, exe_path, shm_handle, args.app_args) catch |err| {
            return err;
        };
    }
    std.log.debug("Interpreter execution completed", .{});
}

/// Append an argument to a command line buffer with proper Windows quoting.
/// Windows command line parsing rules:
/// - Arguments containing spaces, tabs, or quotes must be quoted
/// - Embedded quotes must be escaped with backslash: " -> \"
/// - Backslashes before quotes must be doubled: \" -> \\"
fn appendWindowsQuotedArg(cmd_builder: *std.array_list.Managed(u8), arg: []const u8) !void {
    const needs_quoting = arg.len == 0 or std.mem.indexOfAny(u8, arg, " \t\"") != null;

    if (!needs_quoting) {
        try cmd_builder.appendSlice(arg);
        return;
    }

    try cmd_builder.append('"');
    var backslash_count: usize = 0;
    for (arg) |char| {
        if (char == '\\') {
            backslash_count += 1;
        } else if (char == '"') {
            // Double all backslashes before quote, then escape the quote
            // N backslashes + " -> 2N backslashes + \"
            for (0..backslash_count * 2) |_| try cmd_builder.append('\\');
            backslash_count = 0;
            try cmd_builder.appendSlice("\\\"");
        } else {
            // Emit accumulated backslashes as-is (not before a quote)
            for (0..backslash_count) |_| try cmd_builder.append('\\');
            backslash_count = 0;
            try cmd_builder.append(char);
        }
    }
    // Double any trailing backslashes before closing quote
    for (0..backslash_count * 2) |_| try cmd_builder.append('\\');
    try cmd_builder.append('"');
}

/// Run child process using Windows handle inheritance (idiomatic Windows approach)
fn runWithWindowsHandleInheritance(allocs: *Allocators, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) !void {
    // Make the shared memory handle inheritable
    if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
        std.log.err("Failed to set handle as inheritable", .{});
        return error.HandleInheritanceFailed;
    }

    // Convert paths to Windows wide strings
    const exe_path_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, exe_path);

    const cwd = try std.fs.cwd().realpathAlloc(allocs.arena, ".");
    const cwd_w = try std.unicode.utf8ToUtf16LeAllocZ(allocs.arena, cwd);

    // Create command line with handle and size as arguments, plus any app arguments
    const handle_uint = @intFromPtr(shm_handle.fd);

    // Build command line string with proper quoting for Windows
    var cmd_builder = std.array_list.Managed(u8).initCapacity(allocs.gpa, 256) catch |err| {
        std.log.err("Failed to allocate command line builder: {}", .{err});
        return err;
    };
    defer cmd_builder.deinit();
    try cmd_builder.writer().print("\"{s}\" {} {}", .{ exe_path, handle_uint, shm_handle.size });
    for (app_args) |arg| {
        try cmd_builder.append(' ');
        try appendWindowsQuotedArg(&cmd_builder, arg);
    }
    try cmd_builder.append(0); // null terminator for sentinel

    const cmd_line = cmd_builder.items[0 .. cmd_builder.items.len - 1 :0];
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

    // Check exit code and propagate to parent
    if (exit_code != 0) {
        std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
        if (exit_code == 0xC0000005) { // STATUS_ACCESS_VIOLATION
            std.log.err("Child process crashed with access violation (segfault)", .{});
        } else if (exit_code >= 0xC0000000) { // NT status codes for exceptions
            std.log.err("Child process crashed with exception code: 0x{X}", .{exit_code});
        }
        // Propagate the exit code (truncated to u8 for compatibility)
        std.process.exit(@truncate(exit_code));
    }

    std.log.debug("Child process completed successfully", .{});
}

/// Run child process using POSIX file descriptor inheritance (existing approach for Unix)
/// The exe_path should already be in a unique temp directory created by createUniqueTempDir.
fn runWithPosixFdInheritance(allocs: *Allocators, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) !void {
    // Write the coordination file (.txt) next to the executable
    // The executable is already in a unique temp directory
    std.log.debug("Writing fd coordination file for: {s}", .{exe_path});
    writeFdCoordinationFile(allocs, exe_path, shm_handle) catch |err| {
        std.log.err("Failed to write fd coordination file: {}", .{err});
        return err;
    };
    std.log.debug("Coordination file written successfully", .{});

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

    // Build argv slice using arena allocator (memory lives until arena is freed)
    const argv = allocs.arena.alloc([]const u8, 1 + app_args.len) catch |err| {
        std.log.err("Failed to allocate argv: {}", .{err});
        return err;
    };
    argv[0] = exe_path;
    for (app_args, 0..) |arg, i| {
        argv[1 + i] = arg;
    }

    // Run the interpreter as a child process from the temp directory
    var child = std.process.Child.init(argv, allocs.gpa);
    child.cwd = std.fs.cwd().realpathAlloc(allocs.arena, ".") catch |err| {
        std.log.err("Failed to get current directory: {}", .{err});
        return err;
    };

    // Forward stdout and stderr
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    // Spawn the child process
    std.log.debug("Spawning child process: {s} with {} app args", .{ exe_path, app_args.len });
    std.log.debug("Child process working directory: {s}", .{child.cwd.?});
    child.spawn() catch |err| {
        std.log.err("Failed to spawn {s}: {}", .{ exe_path, err });
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
                // Propagate the exit code from the child process to our parent
                std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
                std.process.exit(exit_code);
            }
        },
        .Signal => |signal| {
            std.log.err("Child process {s} killed by signal: {}", .{ exe_path, signal });
            if (signal == 11) { // SIGSEGV
                std.log.err("Child process crashed with segmentation fault (SIGSEGV)", .{});
            } else if (signal == 6) { // SIGABRT
                std.log.err("Child process aborted (SIGABRT)", .{});
            } else if (signal == 9) { // SIGKILL
                std.log.err("Child process was killed (SIGKILL)", .{});
            }
            // Standard POSIX convention: exit with 128 + signal number
            std.process.exit(128 +| @as(u8, @truncate(signal)));
        },
        .Stopped => |signal| {
            std.log.err("Child process {s} stopped by signal: {}", .{ exe_path, signal });
            return error.ProcessStopped;
        },
        .Unknown => |status| {
            std.log.err("Child process {s} terminated with unknown status: {}", .{ exe_path, status });
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

/// Result of setting up shared memory with type checking information.
/// Contains both the shared memory handle for the compiled modules and
/// a count of type errors encountered during compilation.
pub const SharedMemoryResult = struct {
    handle: SharedMemoryHandle,
    error_count: usize,
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

/// Set up shared memory with compiled ModuleEnvs from a Roc file and its platform modules.
/// This parses, canonicalizes, and type-checks all modules, with the resulting ModuleEnvs
/// ending up in shared memory because all allocations were done into shared memory.
/// Platform type modules have their e_anno_only expressions converted to e_hosted_lambda.
pub fn setupSharedMemoryWithModuleEnv(allocs: *Allocators, roc_file_path: []const u8) !SharedMemoryResult {
    // Create shared memory with SharedMemoryAllocator
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(SHARED_MEMORY_SIZE, page_size);
    // Don't defer deinit here - we need to keep the shared memory alive

    const shm_allocator = shm.allocator();

    // Load builtin modules
    var builtin_modules = try eval.BuiltinModules.init(allocs.gpa);
    defer builtin_modules.deinit();

    // If the roc file path has no directory component (e.g., "app.roc"), use current directory
    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";

    const platform_spec = try extractPlatformSpecFromApp(allocs, roc_file_path);

    // Resolve platform path based on type:
    // - Relative paths (./...) -> join with app directory
    // - URL paths (http/https) -> resolve to cached package main.roc
    // - Other paths -> null (not supported)
    const platform_main_path: ?[]const u8 = if (std.mem.startsWith(u8, platform_spec, "./") or std.mem.startsWith(u8, platform_spec, "../"))
        try std.fs.path.join(allocs.gpa, &[_][]const u8{ app_dir, platform_spec })
    else if (std.mem.startsWith(u8, platform_spec, "http://") or std.mem.startsWith(u8, platform_spec, "https://")) blk: {
        // URL platform - resolve to cached package path
        const platform_paths = resolveUrlPlatform(allocs, platform_spec) catch {
            break :blk null;
        };
        break :blk platform_paths.platform_source_path;
    } else null;
    defer if (platform_main_path) |p| {
        // Only free if it was allocated by join (not arena-allocated from resolveUrlPlatform)
        if (std.mem.startsWith(u8, platform_spec, "./") or std.mem.startsWith(u8, platform_spec, "../")) {
            allocs.gpa.free(p);
        }
    };

    // Get the platform directory from the resolved path
    const platform_dir: ?[]const u8 = if (platform_main_path) |p|
        std.fs.path.dirname(p) orelse return error.InvalidPlatformPath
    else
        null;

    // Extract exposed modules from the platform header (if platform exists)
    var exposed_modules = std.ArrayList([]const u8).empty;
    defer exposed_modules.deinit(allocs.gpa);

    var has_platform = false;
    if (platform_main_path) |pmp| {
        has_platform = true;
        extractExposedModulesFromPlatform(allocs, pmp, &exposed_modules) catch {
            // Platform file not found or couldn't be parsed - continue without platform modules
            has_platform = false;
        };
    }

    // IMPORTANT: Create header FIRST before any module compilation.
    // The interpreter_shim expects the Header to be at FIRST_ALLOC_OFFSET (504).
    // If we compile modules first, they would occupy that offset and break
    // shared memory layout assumptions.
    const Header = struct {
        parent_base_addr: u64,
        module_count: u32,
        entry_count: u32,
        def_indices_offset: u64,
        module_envs_offset: u64,
        /// Offset to platform's main.roc env (0 if no platform, entry points are in app)
        platform_main_env_offset: u64,
        /// Offset to app env (always present, used for e_lookup_required resolution)
        app_env_offset: u64,
    };

    const header_ptr = try shm_allocator.create(Header);
    const shm_base_addr = @intFromPtr(shm.base_ptr);
    header_ptr.parent_base_addr = shm_base_addr;

    // Module count = 1 (app) + number of platform modules
    const total_module_count: u32 = 1 + @as(u32, @intCast(exposed_modules.items.len));
    header_ptr.module_count = total_module_count;

    // Allocate array for module env offsets
    const module_env_offsets_ptr = try shm_allocator.alloc(u64, total_module_count);
    const module_envs_offset_location = @intFromPtr(module_env_offsets_ptr.ptr) - @intFromPtr(shm.base_ptr);
    header_ptr.module_envs_offset = module_envs_offset_location;

    // Compile platform sibling modules FIRST (Stdout, Stderr, Stdin, etc.)
    // This must happen before platform main.roc so that when main.roc is canonicalized,
    // we can pass the sibling modules to module_envs and validate imports correctly.
    var platform_env_ptrs = try allocs.gpa.alloc(*ModuleEnv, exposed_modules.items.len);
    defer allocs.gpa.free(platform_env_ptrs);

    for (exposed_modules.items, 0..) |module_name, i| {
        // platform_dir is guaranteed to be non-null if exposed_modules is non-empty
        // because we only populate exposed_modules when platform_main_path is non-null
        const plat_dir = platform_dir orelse unreachable;
        const module_filename = try std.fmt.allocPrint(allocs.gpa, "{s}.roc", .{module_name});
        defer allocs.gpa.free(module_filename);

        const module_path = try std.fs.path.join(allocs.gpa, &[_][]const u8{ plat_dir, module_filename });
        defer allocs.gpa.free(module_path);

        const module_env_ptr = try compileModuleToSharedMemory(
            allocs,
            module_path,
            module_name, // Use just "Stdout" (not "Stdout.roc") so type-module detection works
            shm_allocator,
            &builtin_modules,
            &.{},
        );

        // Store platform modules at indices 0..N-2, app will be at N-1
        module_env_offsets_ptr[i] = @intFromPtr(module_env_ptr) - @intFromPtr(shm.base_ptr);
        platform_env_ptrs[i] = module_env_ptr;
    }

    // NOW compile platform main.roc AFTER sibling modules so we can pass them to module_envs.
    // This allows the canonicalizer to validate that imports of Stdout, Stderr, etc. are valid.
    var platform_main_env: ?*ModuleEnv = null;
    if (has_platform) {
        // Cast []*ModuleEnv to []const *ModuleEnv for the function parameter
        const const_platform_env_ptrs: []const *ModuleEnv = platform_env_ptrs;
        // platform_main_path is guaranteed non-null when has_platform is true
        platform_main_env = compileModuleToSharedMemory(
            allocs,
            platform_main_path.?,
            "main.roc",
            shm_allocator,
            &builtin_modules,
            const_platform_env_ptrs,
        ) catch null;
    }

    // Collect and sort all hosted functions globally, then assign indices
    if (platform_env_ptrs.len > 0) {
        const HostedCompiler = can.HostedCompiler;
        var all_hosted_fns = std.ArrayList(HostedCompiler.HostedFunctionInfo).empty;
        defer all_hosted_fns.deinit(allocs.gpa);

        // Collect from all platform modules
        for (platform_env_ptrs) |platform_env| {
            var module_fns = try HostedCompiler.collectAndSortHostedFunctions(platform_env);
            defer module_fns.deinit(platform_env.gpa);

            for (module_fns.items) |fn_info| {
                try all_hosted_fns.append(allocs.gpa, fn_info);
            }
        }

        // Sort globally
        const SortContext = struct {
            pub fn lessThan(_: void, a: HostedCompiler.HostedFunctionInfo, b: HostedCompiler.HostedFunctionInfo) bool {
                return std.mem.order(u8, a.name_text, b.name_text) == .lt;
            }
        };
        std.mem.sort(HostedCompiler.HostedFunctionInfo, all_hosted_fns.items, {}, SortContext.lessThan);

        // Deduplicate
        var write_idx: usize = 0;
        for (all_hosted_fns.items, 0..) |fn_info, read_idx| {
            if (write_idx == 0 or !std.mem.eql(u8, all_hosted_fns.items[write_idx - 1].name_text, fn_info.name_text)) {
                if (write_idx != read_idx) {
                    all_hosted_fns.items[write_idx] = fn_info;
                }
                write_idx += 1;
            } else {
                allocs.gpa.free(fn_info.name_text);
            }
        }
        all_hosted_fns.shrinkRetainingCapacity(write_idx);

        // Reassign global indices
        for (platform_env_ptrs) |platform_env| {
            const all_defs = platform_env.store.sliceDefs(platform_env.all_defs);
            for (all_defs) |def_idx| {
                const def = platform_env.store.getDef(def_idx);
                const expr = platform_env.store.getExpr(def.expr);

                if (expr == .e_hosted_lambda) {
                    const hosted = expr.e_hosted_lambda;
                    const local_name = platform_env.getIdent(hosted.symbol_name);

                    var plat_module_name = platform_env.module_name;
                    if (std.mem.endsWith(u8, plat_module_name, ".roc")) {
                        plat_module_name = plat_module_name[0 .. plat_module_name.len - 4];
                    }
                    const qualified_name = try std.fmt.allocPrint(allocs.gpa, "{s}.{s}", .{ plat_module_name, local_name });
                    defer allocs.gpa.free(qualified_name);

                    const stripped_name = if (std.mem.endsWith(u8, qualified_name, "!"))
                        qualified_name[0 .. qualified_name.len - 1]
                    else
                        qualified_name;

                    for (all_hosted_fns.items, 0..) |fn_info, idx| {
                        if (std.mem.eql(u8, fn_info.name_text, stripped_name)) {
                            const expr_node_idx = @as(@TypeOf(platform_env.store.nodes).Idx, @enumFromInt(@intFromEnum(def.expr)));
                            var expr_node = platform_env.store.nodes.get(expr_node_idx);
                            expr_node.data_2 = @intCast(idx);
                            platform_env.store.nodes.set(expr_node_idx, expr_node);
                            break;
                        }
                    }
                }
            }
        }
    }

    // Now compile the app module
    const app_env_ptr = try shm_allocator.create(ModuleEnv);

    const app_file = std.fs.cwd().openFile(roc_file_path, .{}) catch |err| {
        std.log.err("Failed to open Roc file '{s}': {}", .{ roc_file_path, err });
        return error.FileNotFound;
    };
    defer app_file.close();

    const app_file_size = try app_file.getEndPos();
    const app_source = try shm_allocator.alloc(u8, @intCast(app_file_size));
    _ = try app_file.read(app_source);

    const app_basename = std.fs.path.basename(roc_file_path);
    const app_module_name = try shm_allocator.dupe(u8, app_basename);

    var app_env = try ModuleEnv.init(shm_allocator, app_source);
    app_env.common.source = app_source;
    app_env.module_name = app_module_name;
    try app_env.common.calcLineStarts(shm_allocator);

    var app_parse_ast = try parse.parse(&app_env.common, allocs.gpa);
    defer app_parse_ast.deinit(allocs.gpa);
    app_parse_ast.store.emptyScratch();

    try app_env.initCIRFields(app_module_name);

    var app_module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer app_module_envs_map.deinit();

    try Can.populateModuleEnvs(
        &app_module_envs_map,
        &app_env,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_indices,
    );

    for (platform_env_ptrs) |mod_env| {
        const name = try app_env.insertIdent(base.Ident.for_text(mod_env.module_name));
        // For user/platform modules, the qualified name is just the module name itself
        const qualified_ident = try app_env.insertIdent(base.Ident.for_text(mod_env.module_name));
        try app_module_envs_map.put(name, .{ .env = mod_env, .qualified_type_ident = qualified_ident });
    }

    // Add platform modules to the module envs map for canonicalization
    // Two keys are needed for each platform module:
    // 1. "pf.Stdout" - used during import validation (import pf.Stdout)
    // 2. "Stdout" - used during expression canonicalization (Stdout.line!)
    // Also set statement_idx to the actual type node index, which is needed for
    // creating e_nominal_external and e_lookup_external expressions.
    for (exposed_modules.items, 0..) |module_name, i| {
        const platform_env = platform_env_ptrs[i];
        // For platform modules (type modules), the qualified type name is just the type name.
        // Type modules like Stdout.roc store associated items as "Stdout.line!" (not "Stdout.roc.Stdout.line!")
        // because processTypeDeclFirstPass uses parent_name=null for top-level types.
        // Insert into app_env (calling module) since Ident.Idx values are not transferable between stores.
        const type_qualified_ident = try app_env.insertIdent(base.Ident.for_text(module_name));

        // Look up the type in the platform module's exposed_items to get the actual node index
        const type_ident_in_platform = platform_env.common.findIdent(module_name) orelse {
            std.log.err("Platform module '{s}' does not expose a type named '{s}'", .{ module_name, module_name });
            return error.MissingTypeInPlatformModule;
        };
        const type_node_idx = platform_env.getExposedNodeIndexById(type_ident_in_platform) orelse {
            std.log.err("Platform module type '{s}' has no node index in exposed_items", .{module_name});
            return error.MissingNodeIndexForPlatformType;
        };

        const auto_type = Can.AutoImportedType{
            .env = platform_env,
            .statement_idx = @enumFromInt(type_node_idx), // actual type node index for e_lookup_external
            .qualified_type_ident = type_qualified_ident,
        };

        // Add with qualified name key (for import validation: "pf.Stdout")
        const qualified_name = try std.fmt.allocPrint(allocs.gpa, "pf.{s}", .{module_name});
        defer allocs.gpa.free(qualified_name);
        const qualified_ident = try app_env.insertIdent(base.Ident.for_text(qualified_name));
        try app_module_envs_map.put(qualified_ident, auto_type);

        // Add with unaliased name key (for expression canonicalization: "Stdout")
        const module_ident = try app_env.insertIdent(base.Ident.for_text(module_name));
        try app_module_envs_map.put(module_ident, auto_type);

        // Add with resolved module name key (for after alias resolution: "Stdout.roc")
        // The import system resolves "pf.Stdout" to "Stdout.roc", so scopeLookupModule
        // returns "Stdout.roc" which is then used to look up in module_envs
        const module_name_with_roc = try std.fmt.allocPrint(allocs.gpa, "{s}.roc", .{module_name});
        defer allocs.gpa.free(module_name_with_roc);
        const resolved_ident = try app_env.insertIdent(base.Ident.for_text(module_name_with_roc));
        try app_module_envs_map.put(resolved_ident, auto_type);
    }

    var app_canonicalizer = try Can.init(&app_env, &app_parse_ast, &app_module_envs_map);
    defer app_canonicalizer.deinit();

    try app_canonicalizer.canonicalizeFile();
    try app_canonicalizer.validateForExecution();

    if (app_env.exports.span.len == 0) {
        std.log.err("No exported definitions found after canonicalization", .{});
        return error.NoMainFunction;
    }

    // Store app env at the last index (N-1, after platform modules at 0..N-2)
    module_env_offsets_ptr[total_module_count - 1] = @intFromPtr(app_env_ptr) - @intFromPtr(shm.base_ptr);

    // Store app env offset for e_lookup_required resolution
    header_ptr.app_env_offset = @intFromPtr(app_env_ptr) - @intFromPtr(shm.base_ptr);

    // Entry points are defined in the platform's `provides` section.
    // The platform wraps app-provided functions (from `requires`) and exports them for the host.
    // For example: `provides { main_for_host!: "main" }` where `main_for_host! = main!`
    const platform_env = platform_main_env orelse {
        std.log.err("No platform found. Every Roc app requires a platform.", .{});
        return error.NoPlatformFound;
    };
    const exports_slice = platform_env.store.sliceDefs(platform_env.exports);
    if (exports_slice.len == 0) {
        std.log.err("Platform has no exports in `provides` clause.", .{});
        return error.NoEntrypointFound;
    }

    // Store platform env offset for entry point lookups
    header_ptr.platform_main_env_offset = @intFromPtr(platform_env) - @intFromPtr(shm.base_ptr);
    header_ptr.entry_count = @intCast(exports_slice.len);

    const def_indices_ptr = try shm_allocator.alloc(u32, exports_slice.len);
    header_ptr.def_indices_offset = @intFromPtr(def_indices_ptr.ptr) - @intFromPtr(shm.base_ptr);

    for (exports_slice, 0..) |def_idx, i| {
        def_indices_ptr[i] = @intFromEnum(def_idx);
    }

    // Type check with all imported modules
    // Use the env's module_name_idx so that nominal types' origin_module matches
    // the env's identity for method resolution at runtime
    const app_builtin_ctx: Check.BuiltinContext = .{
        .module_name = app_env.module_name_idx,
        .bool_stmt = builtin_modules.builtin_indices.bool_type,
        .try_stmt = builtin_modules.builtin_indices.try_type,
        .str_stmt = builtin_modules.builtin_indices.str_type,
        .builtin_module = builtin_modules.builtin_module.env,
        .builtin_indices = builtin_modules.builtin_indices,
    };

    var app_imported_envs = std.ArrayList(*const ModuleEnv).empty;
    defer app_imported_envs.deinit(allocs.gpa);
    try app_imported_envs.append(allocs.gpa, builtin_modules.builtin_module.env);
    for (platform_env_ptrs) |penv| {
        try app_imported_envs.append(allocs.gpa, penv);
    }

    // Resolve imports - map each import to its index in app_imported_envs
    app_env.imports.resolveImports(&app_env, app_imported_envs.items);

    var app_checker = try Check.init(shm_allocator, &app_env.types, &app_env, app_imported_envs.items, &app_module_envs_map, &app_env.store.regions, app_builtin_ctx);
    defer app_checker.deinit();

    try app_checker.checkFile();

    // Check that app exports match platform requirements (if platform exists)
    if (platform_main_env) |penv| {
        // Build the platform-to-app ident translation map
        var platform_to_app_idents = std.AutoHashMap(base.Ident.Idx, base.Ident.Idx).init(allocs.gpa);
        defer platform_to_app_idents.deinit();

        for (penv.requires_types.items.items) |required_type| {
            const platform_ident_text = penv.getIdent(required_type.ident);
            if (app_env.common.findIdent(platform_ident_text)) |app_ident| {
                try platform_to_app_idents.put(required_type.ident, app_ident);
            }
        }

        try app_checker.checkPlatformRequirements(penv, &platform_to_app_idents);
    }

    // Render all type problems (errors and warnings) exactly as roc check would
    // Count errors so the caller can decide whether to proceed with execution
    // Skip rendering in test mode to avoid polluting test output
    const error_count = if (!builtin.is_test)
        renderTypeProblems(allocs.gpa, &app_checker, &app_env, roc_file_path)
    else
        0;

    app_env_ptr.* = app_env;

    shm.updateHeader();

    return SharedMemoryResult{
        .handle = SharedMemoryHandle{
            .fd = shm.handle,
            .ptr = shm.base_ptr,
            .size = shm.getUsedSize(),
        },
        .error_count = error_count,
    };
}

/// Extract exposed modules from a platform's main.roc file
fn extractExposedModulesFromPlatform(allocs: *Allocators, roc_file_path: []const u8, exposed_modules: *std.ArrayList([]const u8)) !void {
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
            // Get the exposes collection
            const exposes_coll = parse_ast.store.getCollection(platform_header.exposes);
            const exposes_items = parse_ast.store.exposedItemSlice(.{ .span = exposes_coll.span });

            // Extract all exposed module names
            for (exposes_items) |item_idx| {
                const item = parse_ast.store.getExposedItem(item_idx);
                const token_idx = switch (item) {
                    .upper_ident => |ui| ui.ident,
                    .upper_ident_star => |uis| uis.ident,
                    .lower_ident => |li| li.ident,
                    .malformed => continue, // Skip malformed items
                };
                const item_name = parse_ast.resolve(token_idx);
                try exposed_modules.append(allocs.gpa, try allocs.arena.dupe(u8, item_name));
            }
        },
        else => {
            return error.NotPlatformFile;
        },
    }
}

/// Compile a single module to shared memory (for platform modules)
fn compileModuleToSharedMemory(
    allocs: *Allocators,
    file_path: []const u8,
    module_name_arg: []const u8,
    shm_allocator: std.mem.Allocator,
    builtin_modules: *eval.BuiltinModules,
    additional_modules: []const *ModuleEnv,
) !*ModuleEnv {
    // Read file
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const source = try shm_allocator.alloc(u8, @intCast(file_size));
    _ = try file.read(source);

    const module_name_copy = try shm_allocator.dupe(u8, module_name_arg);

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
    try env.initCIRFields(module_name_copy);

    // Create module_envs map
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer module_envs_map.deinit();

    try Can.populateModuleEnvs(
        &module_envs_map,
        &env,
        builtin_modules.builtin_module.env,
        builtin_modules.builtin_indices,
    );

    for (additional_modules) |mod_env| {
        // Add with full module name (e.g., "Stdout.roc")
        const name = try env.insertIdent(base.Ident.for_text(mod_env.module_name));
        // For user modules, the qualified name is just the module name itself
        const qualified_ident = try mod_env.common.insertIdent(mod_env.gpa, base.Ident.for_text(mod_env.module_name));
        try module_envs_map.put(name, .{ .env = mod_env, .qualified_type_ident = qualified_ident });

        // Also add without .roc suffix (e.g., "Stdout") for import validation
        // The import statement `import Stdout` uses the name without .roc
        if (std.mem.endsWith(u8, mod_env.module_name, ".roc")) {
            const name_without_roc = mod_env.module_name[0 .. mod_env.module_name.len - 4];
            const short_name = try env.insertIdent(base.Ident.for_text(name_without_roc));
            const short_qualified_ident = try mod_env.common.insertIdent(mod_env.gpa, base.Ident.for_text(name_without_roc));
            try module_envs_map.put(short_name, .{ .env = mod_env, .qualified_type_ident = short_qualified_ident });
        }
    }

    // Canonicalize (without root_is_platform - we'll run HostedCompiler separately)
    var canonicalizer = try Can.init(&env, &parse_ast, &module_envs_map);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Run HostedCompiler to convert e_anno_only to e_hosted_lambda
    // This is the key step for platform type modules
    const HostedCompiler = can.HostedCompiler;
    _ = try HostedCompiler.replaceAnnoOnlyWithHosted(&env);

    // Type check
    var check_module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer check_module_envs_map.deinit();

    const builtin_ctx: Check.BuiltinContext = .{
        .module_name = try env.insertIdent(base.Ident.for_text(module_name_arg)),
        .bool_stmt = builtin_modules.builtin_indices.bool_type,
        .try_stmt = builtin_modules.builtin_indices.try_type,
        .str_stmt = builtin_modules.builtin_indices.str_type,
        .builtin_module = builtin_modules.builtin_module.env,
        .builtin_indices = builtin_modules.builtin_indices,
    };

    const imported_envs = [_]*const ModuleEnv{builtin_modules.builtin_module.env};

    // Resolve imports - map each import to its index in imported_envs
    env.imports.resolveImports(&env, &imported_envs);

    var checker = try Check.init(shm_allocator, &env.types, &env, &imported_envs, &check_module_envs_map, &env.store.regions, builtin_ctx);
    defer checker.deinit();

    try checker.checkFile();

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
    // Use the parser to extract the platform spec
    const platform_spec = extractPlatformSpecFromApp(allocs, roc_file_path) catch return error.NoPlatformFound;
    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";
    return resolvePlatformSpecToPaths(allocs, platform_spec, app_dir);
}

/// Extract platform specification from app file header by parsing it properly.
fn extractPlatformSpecFromApp(allocs: *Allocators, app_file_path: []const u8) ![]const u8 {
    // Read the app file
    const source = std.fs.cwd().readFileAlloc(allocs.gpa, app_file_path, std.math.maxInt(usize)) catch return error.FileNotFound;
    defer allocs.gpa.free(source);

    // Extract module name from file path
    const basename = std.fs.path.basename(app_file_path);
    const module_name = try allocs.arena.dupe(u8, basename);

    // Create ModuleEnv for parsing
    var env = ModuleEnv.init(allocs.gpa, source) catch return error.ParseFailed;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    env.common.calcLineStarts(allocs.gpa) catch return error.ParseFailed;

    // Parse the source
    var ast = parse.parse(&env.common, allocs.gpa) catch return error.ParseFailed;
    defer ast.deinit(allocs.gpa);

    // Get the file header
    const file = ast.store.getFile();
    const header = ast.store.getHeader(file.header);

    // Check if this is an app file
    switch (header) {
        .app => |a| {
            // Get the platform field
            const pf = ast.store.getRecordField(a.platform_idx);
            const value_expr = pf.value orelse return error.NotAppFile;

            // Extract the string value from the expression
            const platform_spec = stringFromExpr(&ast, value_expr) catch return error.NotAppFile;
            return try allocs.arena.dupe(u8, platform_spec);
        },
        else => return error.NotAppFile,
    }
}

/// Extract a string value from an expression (for platform/package paths).
fn stringFromExpr(ast: *parse.AST, expr_idx: parse.AST.Expr.Idx) ![]const u8 {
    const e = ast.store.getExpr(expr_idx);
    return switch (e) {
        .string => |s| {
            // For simple strings, iterate through the parts
            for (ast.store.exprSlice(s.parts)) |part_idx| {
                const part = ast.store.getExpr(part_idx);
                if (part == .string_part) {
                    // Return the first string part (platform specs are simple strings)
                    return ast.resolve(part.string_part.token);
                }
            }
            return error.ExpectedString;
        },
        else => error.ExpectedString,
    };
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
        return resolveUrlPlatform(allocs, platform_spec);
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

        // Use findHostLibrary to check targets directory first for proper musl detection,
        // then fall back to root-level libhost.a
        const host_path = findHostLibrary(allocs.arena, platform_dir) catch {
            return error.PlatformNotSupported;
        } orelse {
            return error.PlatformNotSupported;
        };

        return PlatformPaths{
            .host_lib_path = host_path,
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

/// Get the roc cache directory for downloaded packages, creating it if needed.
/// Standard cache locations by platform:
/// - Linux/macOS: ~/.cache/roc/packages/ (respects XDG_CACHE_HOME if set)
/// - Windows: %LOCALAPPDATA%\roc\packages\
fn getRocCacheDir(allocator: std.mem.Allocator) ![]const u8 {
    // Check XDG_CACHE_HOME first (Linux/macOS)
    if (getEnvVar(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
        defer allocator.free(xdg_cache);
        return std.fs.path.join(allocator, &.{ xdg_cache, "roc", "packages" });
    }

    // Fall back to %LOCALAPPDATA%\roc\packages (Windows)
    if (comptime builtin.os.tag == .windows) {
        if (getEnvVar(allocator, "LOCALAPPDATA")) |local_app_data| {
            defer allocator.free(local_app_data);
            return std.fs.path.join(allocator, &.{ local_app_data, "roc", "packages" });
        }
    }

    // Fall back to ~/.cache/roc/packages (Unix)
    if (getEnvVar(allocator, "HOME")) |home| {
        defer allocator.free(home);
        return std.fs.path.join(allocator, &.{ home, ".cache", "roc", "packages" });
    }

    return error.NoCacheDir;
}

/// Cross-platform helper to get environment variable.
/// Returns null if the variable is not set. Caller must free the returned slice.
fn getEnvVar(allocator: std.mem.Allocator, key: []const u8) ?[]const u8 {
    return std.process.getEnvVarOwned(allocator, key) catch null;
}

/// Get list of target directory names to try, in preference order.
/// For Linux, tries musl first (produces static executables), then glibc as fallback.
fn getNativeTargetDirs() []const []const u8 {
    return switch (builtin.os.tag) {
        .linux => switch (builtin.cpu.arch) {
            .aarch64 => &.{ "arm64musl", "arm64glibc" },
            .x86_64 => &.{ "x64musl", "x64glibc" },
            .arm => &.{"arm32musl"},
            else => &.{},
        },
        .macos => switch (builtin.cpu.arch) {
            .aarch64 => &.{"arm64mac"},
            .x86_64 => &.{"x64mac"},
            else => &.{},
        },
        .windows => switch (builtin.cpu.arch) {
            .aarch64 => &.{"arm64win"},
            .x86_64 => &.{"x64win"},
            else => &.{},
        },
        .wasi => &.{"wasm32"},
        else => &.{},
    };
}

/// Find host library in package directory, checking multiple locations.
/// Search order:
/// 1. {package_dir}/targets/{target}/libhost.a for each target in preference order
///    (e.g., on Linux x64: tries x64musl first, then x64glibc as fallback)
///    This is checked first because target-specific libraries include CRT/libc dependencies
///    that are needed for proper linking and musl detection.
/// 2. {package_dir}/libhost.a (simple platforms without target-specific builds)
fn findHostLibrary(allocator: std.mem.Allocator, package_dir_path: []const u8) !?[]const u8 {
    const host_filename = if (comptime builtin.os.tag == .windows) "host.lib" else "libhost.a";

    // 1. Check targets directory first (preferred for proper linking)
    for (getNativeTargetDirs()) |target| {
        const target_path = try std.fs.path.join(allocator, &.{
            package_dir_path, "targets", target, host_filename,
        });
        if (std.fs.cwd().access(target_path, .{})) |_| {
            return target_path;
        } else |_| {}
    }

    // 2. Fall back to root directory (simple platforms)
    const root_path = try std.fs.path.join(allocator, &.{ package_dir_path, host_filename });
    if (std.fs.cwd().access(root_path, .{})) |_| {
        return root_path;
    } else |_| {}

    return null;
}

/// Resolve a URL platform specification by downloading and caching the bundle.
/// The URL must point to a .tar.zst bundle with a base58-encoded BLAKE3 hash filename.
fn resolveUrlPlatform(allocs: *Allocators, url: []const u8) (std.mem.Allocator.Error || error{PlatformNotSupported})!PlatformPaths {
    const download = unbundle.download;

    // 1. Validate URL and extract hash
    const base58_hash = download.validateUrl(url) catch {
        std.log.debug("Invalid platform URL: {s}", .{url});
        return error.PlatformNotSupported;
    };

    // 2. Get cache directory
    const cache_dir_path = getRocCacheDir(allocs.arena) catch {
        std.log.err("Could not determine cache directory", .{});
        return error.PlatformNotSupported;
    };
    const package_dir_path = try std.fs.path.join(allocs.arena, &.{ cache_dir_path, base58_hash });

    // 3. Check if already cached
    var package_dir = std.fs.cwd().openDir(package_dir_path, .{}) catch |err| switch (err) {
        error.FileNotFound => blk: {
            // Not cached - need to download
            std.log.info("Downloading platform from {s}...", .{url});

            // Create cache directory structure
            std.fs.cwd().makePath(cache_dir_path) catch |make_err| {
                std.log.err("Failed to create cache directory: {}", .{make_err});
                return error.PlatformNotSupported;
            };

            // Create package directory
            std.fs.cwd().makeDir(package_dir_path) catch |make_err| switch (make_err) {
                error.PathAlreadyExists => {}, // Race condition, another process created it
                else => {
                    std.log.err("Failed to create package directory: {}", .{make_err});
                    return error.PlatformNotSupported;
                },
            };

            var new_package_dir = std.fs.cwd().openDir(package_dir_path, .{}) catch |open_err| {
                std.log.err("Failed to open package directory: {}", .{open_err});
                return error.PlatformNotSupported;
            };

            // Download and extract
            var gpa_copy = allocs.gpa;
            download.downloadAndExtract(&gpa_copy, url, new_package_dir) catch |download_err| {
                // Clean up failed download
                new_package_dir.close();
                std.fs.cwd().deleteTree(package_dir_path) catch {};
                std.log.err("Failed to download platform: {}", .{download_err});
                return error.PlatformNotSupported;
            };

            std.log.info("Platform cached at {s}", .{package_dir_path});
            break :blk new_package_dir;
        },
        else => {
            std.log.err("Failed to access package directory: {}", .{err});
            return error.PlatformNotSupported;
        },
    };
    defer package_dir.close();

    // 4. Find host library (check multiple locations)
    const host_path = findHostLibrary(allocs.arena, package_dir_path) catch {
        std.log.err("Failed to search for host library", .{});
        return error.PlatformNotSupported;
    } orelse {
        std.log.err("No host library found in platform bundle", .{});
        return error.PlatformNotSupported;
    };

    // 5. Platforms must have a main.roc entry point
    const platform_source_path = try std.fs.path.join(allocs.arena, &.{ package_dir_path, "main.roc" });
    std.fs.cwd().access(platform_source_path, .{}) catch {
        std.log.err("No main.roc found in platform bundle at {s}", .{package_dir_path});
        return error.PlatformNotSupported;
    };

    return PlatformPaths{
        .host_lib_path = host_path,
        .platform_source_path = platform_source_path,
    };
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

            // Extract FFI symbol names from provides clause
            // Format: `provides { roc_identifier: "ffi_symbol_name" }`
            // The string value specifies the symbol name exported to the host (becomes roc__<symbol>)
            for (provides_fields) |field_idx| {
                const field = parse_ast.store.getRecordField(field_idx);

                // Require explicit string value for symbol name
                const symbol_name = if (field.value) |value_idx| blk: {
                    const value_expr = parse_ast.store.getExpr(value_idx);
                    switch (value_expr) {
                        .string => |str_like| {
                            const parts = parse_ast.store.exprSlice(str_like.parts);
                            if (parts.len > 0) {
                                const first_part = parse_ast.store.getExpr(parts[0]);
                                switch (first_part) {
                                    .string_part => |sp| break :blk parse_ast.resolve(sp.token),
                                    else => {},
                                }
                            }
                            std.log.err("Invalid provides entry: string value is empty", .{});
                            return error.InvalidProvidesEntry;
                        },
                        .string_part => |str_part| break :blk parse_ast.resolve(str_part.token),
                        else => {
                            std.log.err("Invalid provides entry: expected string value for symbol name", .{});
                            return error.InvalidProvidesEntry;
                        },
                    }
                } else {
                    const field_name = parse_ast.resolve(field.name);
                    std.log.err("Provides entry '{s}' missing symbol name. Use format: {{ {s}: \"symbol_name\" }}", .{ field_name, field_name });
                    return error.InvalidProvidesEntry;
                };
                try entrypoints.append(try allocs.arena.dupe(u8, symbol_name));
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
    const temp_file = try tmp_dir.createFile(temp_filename, .{
        // Allow querying metadata (stat) on the handle, necessary for windows
        .read = true,
        .truncate = true,
    });
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

    // Only support int test platform for cross-compilation
    // Check if path contains "int" directory using cross-platform path handling
    const path_contains_int = blk: {
        var iter = std.fs.path.componentIterator(args.path) catch break :blk false;
        while (iter.next()) |component| {
            if (std.mem.eql(u8, component.name, "int")) {
                break :blk true;
            }
        }
        break :blk false;
    };

    const platform_type = if (path_contains_int)
        "int"
    else {
        std.log.err("roc build cross-compilation currently only supports the int test platform", .{});
        std.log.err("Your app path: {s}", .{args.path});
        std.log.err("For str platform and other platforms, please use regular 'roc' command", .{});
        return error.UnsupportedPlatform;
    };

    std.log.info("Detected platform type: {s}", .{platform_type});

    // Get platform directory path
    const platform_dir = if (std.mem.eql(u8, platform_type, "int"))
        try std.fs.path.join(allocs.arena, &.{ "test", "int", "platform" })
    else
        try std.fs.path.join(allocs.arena, &.{ "test", "str", "platform" });

    // Check that platform exists
    std.fs.cwd().access(platform_dir, .{}) catch |err| {
        std.log.err("Platform directory not found: {s} ({})", .{ platform_dir, err });
        return err;
    };

    // Get target-specific host library path
    // Use target OS to determine library filename, not host OS
    const host_lib_filename = if (target.toOsTag() == .windows) "host.lib" else "libhost.a";
    const host_lib_path = blk: {
        // Try target-specific host library first
        const target_specific_path = try std.fs.path.join(allocs.arena, &.{ platform_dir, "targets", @tagName(target), host_lib_filename });
        std.fs.cwd().access(target_specific_path, .{}) catch {
            // Fallback to generic host library
            std.log.warn("Target-specific host library not found, falling back to generic: {s}", .{target_specific_path});
            break :blk try std.fs.path.join(allocs.arena, &.{ platform_dir, host_lib_filename });
        };
        break :blk target_specific_path;
    };

    std.fs.cwd().access(host_lib_path, .{}) catch |err| {
        std.log.err("Host library not found: {s} ({})", .{ host_lib_path, err });
        return err;
    };

    // Get expected entrypoints for this platform
    const entrypoints = try app_stub.getTestPlatformEntrypoints(allocs.gpa, platform_type);
    defer allocs.gpa.free(entrypoints);

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
        // For dynamic linking with glibc, generate stub library for cross-compilation
        // Check if we're doing actual cross-compilation
        const is_cross_compiling = host_target != target;

        if (is_cross_compiling) {
            // For cross-compilation, use pre-built vendored stubs from the platform targets folder
            const target_name = switch (target) {
                .x64glibc => "x64glibc",
                .arm64glibc => "arm64glibc",
                else => {
                    std.log.err("Cross-compilation target {} not supported for glibc", .{target});
                    return error.UnsupportedTarget;
                },
            };

            // Check if vendored stubs exist in the platform targets folder
            const stub_dir = try std.fmt.allocPrint(allocs.arena, "test/int/platform/targets/{s}", .{target_name});

            const stub_so_path = try std.fmt.allocPrint(allocs.arena, "{s}/libc.so.6", .{stub_dir});

            // Verify the vendored stub exists
            std.fs.cwd().access(stub_so_path, .{}) catch |err| {
                std.log.err("Pre-built glibc stub not found: {s}", .{stub_so_path});
                std.log.err("Error: {}", .{err});
                std.log.err("This suggests the build system didn't generate the required stubs.", .{});
                std.log.err("Try running 'zig build' first to generate platform target files.", .{});
                return err;
            };

            // Use the vendored stub library
            const stub_dir_arg = try std.fmt.allocPrint(allocs.arena, "-L{s}", .{stub_dir});
            try extra_args.append(stub_dir_arg);
            try extra_args.append("-lc");
            std.log.info("Using pre-built glibc stub from platform targets: {s}", .{stub_dir});
        } else {
            // For native compilation, use system libraries
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
                // Check if the directory exists before adding it
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

    // Load builtin modules required by the type checker and interpreter
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

    // Populate module_envs with Bool, Try, Dict, Set from builtin module
    var module_envs = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(allocs.gpa);
    defer module_envs.deinit();

    const module_builtin_ctx: Check.BuiltinContext = .{
        .module_name = try env.insertIdent(base.Ident.for_text(module_name)),
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module.env,
        .builtin_indices = builtin_indices,
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
    try env.initCIRFields(module_name);

    // Populate module_envs with Bool, Try, Dict, Set using shared function
    try Can.populateModuleEnvs(
        &module_envs,
        &env,
        builtin_module.env,
        builtin_indices,
    );

    // Create canonicalizer
    var canonicalizer = Can.init(&env, &parse_ast, &module_envs) catch |err| {
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

    // Build imported_envs array with builtin module
    const imported_envs: []const *const ModuleEnv = &.{builtin_module.env};

    // Resolve imports - map each import to its index in imported_envs
    env.imports.resolveImports(&env, imported_envs);

    // Type check the module
    var checker = Check.init(allocs.gpa, &env.types, &env, imported_envs, &module_envs, &env.store.regions, module_builtin_ctx) catch |err| {
        try stderr.print("Failed to initialize type checker: {}\n", .{err});
        return err;
    };
    defer checker.deinit();

    checker.checkFile() catch |err| {
        try stderr.print("Type checking failed: {}\n", .{err});
        return err;
    };

    // Evaluate all top-level declarations at compile time
    const builtin_types_for_eval = BuiltinTypes.init(builtin_indices, builtin_module.env, builtin_module.env, builtin_module.env);
    var comptime_evaluator = eval.ComptimeEvaluator.init(allocs.gpa, &env, imported_envs, &checker.problems, builtin_types_for_eval, builtin_module.env, &checker.import_mapping) catch |err| {
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
    var test_runner = TestRunner.init(allocs.gpa, &env, builtin_types_for_eval, imported_envs, builtin_module.env, &checker.import_mapping) catch |err| {
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

fn rocRepl(_: *Allocators) !void {
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
    // URL package resolution errors
    FileError,
    InvalidUrl,
    NoCacheDir,
    DownloadFailed,
    NoPackageSource,
    // Interpreter errors (propagate from eval during build)
    Crash,
    DivisionByZero,
    EarlyReturn,
    IntegerOverflow,
    InvalidImportIndex,
    InvalidMethodReceiver,
    InvalidNumExt,
    InvalidTagExt,
    ListIndexOutOfBounds,
    MethodLookupFailed,
    MethodNotFound,
    NotImplemented,
    NotNumeric,
    NullStackPointer,
    RecordIndexOutOfBounds,
    StackOverflow,
    StringOrderingNotSupported,
    TupleIndexOutOfBounds,
    TypeMismatch,
    UnresolvedImport,
    ZeroSizedType,
    // Layout errors
    TypeContainedMismatch,
    InvalidRecordExtension,
    InvalidNumberExtension,
    BugUnboxedFlexVar,
    BugUnboxedRigidVar,
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

test "appendWindowsQuotedArg" {
    const testing = std.testing;

    // Helper to test the quoting function
    const testQuote = struct {
        fn run(input: []const u8, expected: []const u8) !void {
            var cmd = std.array_list.Managed(u8).initCapacity(testing.allocator, 64) catch unreachable;
            defer cmd.deinit();
            try appendWindowsQuotedArg(&cmd, input);
            try testing.expectEqualStrings(expected, cmd.items);
        }
    }.run;

    // Simple arg without spaces - no quoting needed
    try testQuote("simple", "simple");

    // Arg with spaces - needs quoting
    try testQuote("hello world", "\"hello world\"");

    // Arg with tab - needs quoting
    try testQuote("hello\tworld", "\"hello\tworld\"");

    // Empty arg - needs quoting
    try testQuote("", "\"\"");

    // Arg with embedded quote - needs escaping
    try testQuote("say \"hello\"", "\"say \\\"hello\\\"\"");

    // Arg with backslash not before quote - unchanged
    try testQuote("path\\to\\file", "path\\to\\file");

    // Arg with backslash before quote - backslash doubled
    try testQuote("path\\\"quote", "\"path\\\\\\\"quote\"");

    // Arg with trailing backslash - doubled when quoted
    try testQuote("path with spaces\\", "\"path with spaces\\\\\"");

    // Arg with multiple trailing backslashes (needs space to trigger quoting)
    try testQuote("has spaces\\\\", "\"has spaces\\\\\\\\\"");
}
