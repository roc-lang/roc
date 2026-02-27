//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`
//!
//! ## Module Data Modes
//!
//! The CLI supports two modes for passing compiled Roc modules to the interpreter:
//!
//! ### IPC Mode (`roc path/to/app.roc`)
//! - Compiles Roc source to ModuleEnv in shared memory
//! - Spawns interpreter host as child process that maps the shared memory
//! - Fast startup, same-architecture only
//! - See: `setupSharedMemoryWithCoordinator`, `rocRun`
//!
//! ### Embedded Mode (`roc build path/to/app.roc`)
//! - Serializes ModuleEnv to portable binary format
//! - Embeds serialized data directly into output binary
//! - Cross-architecture support, standalone executables
//! - See: `serialize_modules.zig`, `rocBuild`
//!
//! For detailed documentation, see `src/interpreter_shim/README.md`.

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
const echo_platform = @import("echo_platform");
const lsp = @import("lsp");
const cli_repl = @import("repl.zig");

const cli_args = @import("cli_args.zig");
const roc_target = @import("target.zig");
pub const targets_validator = @import("targets_validator.zig");
const platform_validation = @import("platform_validation.zig");
const cli_context = @import("CliContext.zig");
const cli_problem = @import("CliProblem.zig");

const CliContext = cli_context.CliContext;
const Io = cli_context.Io;
const Command = cli_context.Command;
const CliError = cli_context.CliError;
const renderProblem = cli_context.renderProblem;

comptime {
    if (builtin.is_test) {
        std.testing.refAllDecls(cli_args);
        std.testing.refAllDecls(targets_validator);
        std.testing.refAllDecls(platform_validation);
        std.testing.refAllDecls(cli_context);
        std.testing.refAllDecls(cli_problem);
        std.testing.refAllDecls(@import("stack_probe.zig"));
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
const Coordinator = compile.coordinator.Coordinator;
const Mode = compile.package.Mode;
const TimingInfo = compile.package.TimingInfo;
const CacheManager = compile.CacheManager;
const CacheConfig = compile.CacheConfig;
const serialize_modules = compile.serialize_modules;
const TestRunner = eval.TestRunner;
const backend = @import("backend");
const mono = @import("mono");
const layout = @import("layout");
const Allocators = base.Allocators;
const RocTarget = @import("target.zig").RocTarget;

/// Embedded interpreter shim libraries for different targets.
/// The native shim is used for roc run and native builds.
/// Cross-compilation shims are used for roc build --target=<target>.
const ShimLibraries = struct {
    /// Native shim (for host platform builds and roc run)
    const native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.target.os.tag == .windows)
        @embedFile("roc_interpreter_shim.lib")
    else
        @embedFile("libroc_interpreter_shim.a");

    /// Cross-compilation target shims (Linux musl targets)
    const x64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64musl/libroc_interpreter_shim.a");
    const arm64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64musl/libroc_interpreter_shim.a");

    /// Cross-compilation target shims (Linux glibc targets)
    const x64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64glibc/libroc_interpreter_shim.a");
    const arm64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64glibc/libroc_interpreter_shim.a");

    /// WebAssembly target shim (wasm32-freestanding)
    const wasm32 = if (builtin.is_test) &[_]u8{} else @embedFile("targets/wasm32/libroc_interpreter_shim.a");

    /// Cross-compilation target shims (Windows targets)
    const x64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64win/roc_interpreter_shim.lib");
    const arm64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64win/roc_interpreter_shim.lib");

    /// Get the appropriate shim library bytes for the given target
    pub fn forTarget(target: RocTarget) []const u8 {
        return switch (target) {
            .x64musl => x64musl,
            .arm64musl => arm64musl,
            .x64glibc => x64glibc,
            .arm64glibc => arm64glibc,
            .wasm32 => wasm32,
            .x64win => x64win,
            .arm64win => arm64win,
            // Native/host targets use the native shim
            .x64mac, .arm64mac => native,
            // Fallback for other targets (will use native, may not work for cross-compilation)
            else => native,
        };
    }
};

/// Embedded pre-compiled builtins object files for each target.
/// These contain the wrapper functions needed by the dev backend for string/list operations.
/// Used by `roc build --backend=dev` to link the app object with builtins.
/// Now using static libraries instead of object files to include compiler_rt
/// (needed for 128-bit integer operations used by Dec type).
const BuiltinsObjects = struct {
    /// Native builtins (for host platform builds)
    const native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.os.tag == .windows)
        @embedFile("roc_builtins.lib")
    else
        @embedFile("libroc_builtins.a");

    /// Cross-compilation target builtins (Linux musl targets)
    const x64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64musl/libroc_builtins.a");
    const arm64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64musl/libroc_builtins.a");

    /// Cross-compilation target builtins (Linux glibc targets)
    const x64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64glibc/libroc_builtins.a");
    const arm64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64glibc/libroc_builtins.a");

    /// WebAssembly target builtins (wasm32-freestanding) - not used by dev backend
    const wasm32 = if (builtin.is_test) &[_]u8{} else @embedFile("targets/wasm32/libroc_builtins.a");

    /// Cross-compilation target builtins (Windows targets)
    const x64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64win/roc_builtins.lib");
    const arm64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64win/roc_builtins.lib");

    /// Cross-compilation target builtins (macOS targets)
    const x64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64mac/libroc_builtins.a");
    const arm64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64mac/libroc_builtins.a");

    /// Get the appropriate builtins library bytes for the given target
    pub fn forTarget(target: RocTarget) []const u8 {
        return switch (target) {
            .x64musl => x64musl,
            .arm64musl => arm64musl,
            .x64glibc => x64glibc,
            .arm64glibc => arm64glibc,
            .wasm32 => wasm32,
            .x64win => x64win,
            .arm64win => arm64win,
            .x64mac => x64mac,
            .arm64mac => arm64mac,
            // Fallback for other targets (will use native, may not work for cross-compilation)
            else => native,
        };
    }

    /// Get the filename for builtins library on given target
    pub fn filename(target: RocTarget) []const u8 {
        return switch (target.toOsTag()) {
            .windows => "roc_builtins.lib",
            else => "libroc_builtins.a",
        };
    }
};

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
    // Note: ANSI escape support is enabled in Io.init()
}

fn restoreWindowsConsoleCodePage() void {
    if (!is_windows) return;
    if (windows_console_previous_code_page) |code_page| {
        windows_console_previous_code_page = null;
        _ = std.os.windows.kernel32.SetConsoleOutputCP(code_page);
    }
}

// POSIX shared memory functions
const posix = if (!is_windows) struct {
    extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
    extern "c" fn mmap(addr: ?*anyopaque, len: usize, prot: c_int, flags: c_int, fd: c_int, offset: std.c.off_t) *anyopaque;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;

    // MAP_FAILED is (void*)-1, not NULL
    const MAP_FAILED: *anyopaque = @ptrFromInt(@as(usize, @bitCast(@as(isize, -1))));
} else struct {};

// Windows shared memory functions
const windows = if (is_windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPCWSTR = [*:0]const u16;
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

/// Preferred size for shared memory allocator: 2TB on 64-bit, 256MB on 32-bit.
///
/// We need a large size because SharedMemoryAllocator is a bump allocator that
/// cannot free memory. During type checking, the types Store grows significantly
/// and every array growth allocates new memory without freeing old, causing
/// memory fragmentation. With a 25KB source file, type checking can use ~2GB
/// of shared memory due to this fragmentation.
///
/// On 64-bit Linux/Windows, we reserve 2TB of virtual address space. This is possible
/// without consuming physical memory:
/// - On Linux: memfd_create with lazy page allocation means untouched pages cost nothing.
/// - On Windows: SEC_RESERVE reserves virtual address space without page file backing,
///   and VirtualAlloc(MEM_COMMIT) commits pages on-demand as they're accessed.
///
/// On macOS, shm_open + ftruncate creates a Mach VM object with higher per-object
/// kernel overhead than Linux's memfd_create. Using 2TB causes kernel resource pressure
/// that accumulates across rapid sequential process invocations (e.g., running tests
/// in a loop), leading to SIGKILL from the jetsam memory pressure system.
/// We use 8GB on macOS which provides ample headroom while keeping kernel overhead low.
///
/// On 32-bit targets, we use 256MB since larger sizes won't fit in the address space.
const SHARED_MEMORY_SIZE: usize = if (@sizeOf(usize) < 8)
    256 * 1024 * 1024 // 256MB for 32-bit targets
else if (builtin.os.tag == .macos)
    8 * 1024 * 1024 * 1024 // 8GB for macOS (shm_open has higher kernel overhead)
else
    2 * 1024 * 1024 * 1024 * 1024; // 2TB for 64-bit Linux/Windows

/// Fallback size for systems with overcommit disabled or limited resources.
/// On Linux with vm.overcommit_memory=2, the kernel rejects large ftruncate calls even
/// though the memory wouldn't actually be used. We fall back to 4GB which
/// should work on most systems while still being large enough for typical use.
const SHARED_MEMORY_FALLBACK_SIZE: usize = if (@sizeOf(usize) < 8)
    256 * 1024 * 1024 // 256MB for 32-bit targets (same as primary)
else
    4 * 1024 * 1024 * 1024; // 4GB for 64-bit targets

/// Try to create shared memory, falling back to a smaller size if the system
/// has overcommit disabled and rejects the initial allocation.
fn createSharedMemoryWithFallback(page_size: usize) !SharedMemoryAllocator {
    // Try the preferred size first
    if (SharedMemoryAllocator.create(SHARED_MEMORY_SIZE, page_size)) |shm| {
        return shm;
    } else |_| {}

    // Fall back to smaller size for systems with overcommit disabled
    return SharedMemoryAllocator.create(SHARED_MEMORY_FALLBACK_SIZE, page_size);
}

/// Cross-platform hardlink creation
fn createHardlink(ctx: *CliContext, source: []const u8, dest: []const u8) !void {
    if (comptime builtin.target.os.tag == .windows) {
        // On Windows, use CreateHardLinkW
        const source_w = try std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, source);
        const dest_w = try std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, dest);

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
        const source_c = try ctx.arena.dupeZ(u8, source);
        const dest_c = try ctx.arena.dupeZ(u8, dest);

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
fn generateRandomSuffix(ctx: *CliContext) ![]u8 {
    // TODO: Consider switching to a library like https://github.com/abhinav/temp.zig
    // for more robust temporary file/directory handling
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    const suffix = try ctx.arena.alloc(u8, 32);

    // Fill with cryptographically secure random bytes
    std.crypto.random.bytes(suffix);

    // Convert to ASCII characters from our charset
    for (suffix) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }

    return suffix;
}

/// Create a unique temporary directory under roc/{version}/{random}/.
/// Returns the path to the directory (allocated from arena, no need to free).
/// Uses system temp directory to avoid race conditions when cache is cleared.
pub fn createUniqueTempDir(ctx: *CliContext) ![]const u8 {
    // Get the version-specific temp directory: {temp}/roc/{version}
    const version_temp_dir = try CacheConfig.getVersionTempDir(ctx.arena);

    // Ensure the roc/{version} directory exists
    // makePath automatically handles PathAlreadyExists internally
    try std.fs.cwd().makePath(version_temp_dir);

    // Try to create a unique subdirectory with random suffix
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(ctx);
        const dir_path = try std.fs.path.join(ctx.arena, &.{ version_temp_dir, random_suffix });

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

    // Failed after 6 attempts
    return error.FailedToCreateUniqueTempDir;
}

/// Write shared memory coordination file (.txt) next to the executable.
/// This is the file that the child process reads to find the shared memory fd.
pub fn writeFdCoordinationFile(ctx: *CliContext, temp_exe_path: []const u8, shm_handle: SharedMemoryHandle) !void {
    // The coordination file is at {temp_dir}.txt where temp_dir is the directory containing the exe
    const temp_dir = std.fs.path.dirname(temp_exe_path) orelse return error.InvalidPath;

    // Ensure we have no trailing slashes
    var dir_path = temp_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = try std.fmt.allocPrint(ctx.arena, "{s}.txt", .{dir_path});

    // Create the file (exclusive - fail if exists to detect collisions)
    const fd_file = std.fs.cwd().createFile(fd_file_path, .{ .exclusive = true }) catch |err| {
        // Error is handled by caller with ctx.fail()
        return err;
    };
    defer fd_file.close();

    // Write shared memory info to file
    const fd_str = try std.fmt.allocPrint(ctx.arena, "{}\n{}", .{ shm_handle.fd, shm_handle.size });
    try fd_file.writeAll(fd_str);
    try fd_file.sync();
}

/// Create the temporary directory structure for fd communication.
/// Returns the path to the executable in the temp directory (allocated from arena, no need to free).
/// Uses the standard roc/{version}/{random}/ structure in the system temp directory.
/// The exe_display_name is the name that will appear in `ps` output (e.g., "app.roc").
pub fn createTempDirStructure(allocs: *Allocators, exe_path: []const u8, exe_display_name: []const u8, shm_handle: SharedMemoryHandle, _: ?[]const u8) ![]const u8 {
    // Get the version-specific temp directory: {temp}/roc/{version}
    const version_temp_dir = try CacheConfig.getVersionTempDir(allocs.arena);

    // Ensure the roc/{version} directory exists
    // makePath automatically handles PathAlreadyExists internally
    try std.fs.cwd().makePath(version_temp_dir);

    // Try to create a unique subdirectory with random suffix
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(allocs);
        const temp_dir_path = try std.fs.path.join(allocs.arena, &.{ version_temp_dir, random_suffix });

        // The coordination file path is the directory path with .txt appended
        const dir_name_with_txt = try std.fmt.allocPrint(allocs.arena, "{s}.txt", .{temp_dir_path});

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

    // Failed after 6 attempts
    return error.FailedToCreateUniqueTempDir;
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .{
    .backing_allocator = std.heap.c_allocator,
};

/// The CLI entrypoint for the Roc compiler.
pub fn main() !void {
    // Install stack overflow handler early, before any significant work.
    // This gives us a helpful error message instead of a generic segfault
    // if the compiler blows the stack (e.g., due to infinite recursion in type translation).
    _ = base.stack_overflow.install();

    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa, const is_safe = gpa: {
        if (builtin.os.tag == .freestanding) break :gpa .{ std.heap.wasm_allocator, false };
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

    mainArgs(&allocs, args) catch |err| {
        // Handle OutOfMemory specially - it may not have been printed
        switch (err) {
            error.OutOfMemory => {
                // Use std.debug.print to stderr since we don't have access to ctx.io here
                // TODO: if virtual address allocation fails at 4gb, fall back on doing `roc build` followed by manually running the executable
                std.debug.print("The Roc compiler ran out of memory trying to preallocate virtual address space for compiling and running this program. Try using `roc build` to build the executable separately, then run it manually.\n", .{});
            },
            else => {}, // Other errors should already have printed messages
        }
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

    // Start background cache cleanup on a separate thread.
    // This is a fire-and-forget thread that:
    // - Cleans up stale temp directories (>5 min old)
    // - Cleans up old persistent cache files (>30 days old)
    // - Exits automatically when done
    //
    // We intentionally don't join the thread. If the main process exits before
    // cleanup completes, the OS will automatically terminate the cleanup thread.
    // This ensures cleanup never delays compilation or execution.
    //
    // Uses page_allocator instead of GPA to avoid leak detection false positives
    // (the thread may still be running when the main thread's leak check fires).
    if (compile.CacheCleanup.startBackgroundCleanup(std.heap.page_allocator)) |_| {
        // Thread started successfully, will run in background
    } else |_| {
        // Non-fatal: cleanup failure shouldn't prevent compilation
        std.log.debug("Failed to start background cleanup thread", .{});
    }

    // Create I/O interface - this is passed to all command handlers via ctx
    var io = Io.init();

    const parsed_args = try cli_args.parse(allocs.arena, args[1..]);

    // Determine command for context
    const command: Command = switch (parsed_args) {
        .run => .run,
        .build => .build,
        .check => .check,
        .test_cmd => .test_cmd,
        .fmt => .fmt,
        .bundle => .bundle,
        .unbundle => .unbundle,
        else => .unknown,
    };

    // Create CLI context at the top level - this is passed to all command handlers
    var ctx = CliContext.init(allocs.gpa, allocs.arena, &io, command);
    ctx.initIo(); // Must be called after ctx is at its final stack location
    defer ctx.deinit(); // deinit flushes I/O

    try switch (parsed_args) {
        .run => |run_args| {
            if (std.mem.eql(u8, run_args.path, "main.roc")) {
                std.fs.cwd().access(run_args.path, .{}) catch |err| switch (err) {
                    error.FileNotFound => {
                        const cwd_path = std.fs.cwd().realpathAlloc(allocs.arena, ".") catch |real_err| {
                            ctx.io.stderr().print(
                                "Error: No app file specified and default 'main.roc' was not found. Additionally, the current directory could not be resolved: {}\n",
                                .{real_err},
                            ) catch {};
                            return error.FileNotFound;
                        };
                        ctx.io.stderr().print(
                            "Error: No app file specified and default 'main.roc' was not found in {s}\n",
                            .{cwd_path},
                        ) catch {};
                        ctx.io.stderr().print(
                            "\nHint: pass an explicit path (e.g. `roc my-app.roc`) or create a 'main.roc' in that directory.\n",
                            .{},
                        ) catch {};
                        return error.FileNotFound;
                    },
                    else => {
                        ctx.io.stderr().print(
                            "Error: Unable to access default 'main.roc': {}\n",
                            .{err},
                        ) catch {};
                        return err;
                    },
                };
            }

            rocRun(&ctx, run_args) catch |err| switch (err) {
                error.CliError => {
                    // Problems already recorded in context, render them below
                },
                else => return err,
            };
        },
        .check => |check_args| rocCheck(&ctx, check_args),
        .build => |build_args| rocBuild(&ctx, build_args) catch |err| switch (err) {
            error.CliError => {
                // Problems already recorded in context, render them below
            },
            else => return err,
        },
        .bundle => |bundle_args| rocBundle(&ctx, bundle_args),
        .unbundle => |unbundle_args| rocUnbundle(&ctx, unbundle_args),
        .fmt => |format_args| rocFormat(&ctx, format_args),
        .test_cmd => |test_args| try rocTest(&ctx, test_args),
        .repl => |repl_args| rocRepl(&ctx, repl_args),
        .glue => |glue_args| try rocGlue(&ctx, glue_args),
        .version => ctx.io.stdout().print("Roc compiler version {s}\n", .{build_options.compiler_version}),
        .docs => |docs_args| rocDocs(&ctx, docs_args),
        .experimental_lsp => |lsp_args| try lsp.runWithStdIo(allocs.gpa, .{
            .transport = lsp_args.debug_io,
            .build = lsp_args.debug_build,
            .syntax = lsp_args.debug_syntax,
            .server = lsp_args.debug_server,
        }),
        .help => |help_message| {
            try ctx.io.stdout().writeAll(help_message);
        },
        .licenses => {
            try ctx.io.stdout().writeAll(legalDetailsFileContent);
        },
        .problem => |problem| {
            try switch (problem) {
                .missing_flag_value => |details| ctx.io.stderr().print("Error: no value was supplied for {s}\n", .{details.flag}),
                .unexpected_argument => |details| ctx.io.stderr().print("Error: roc {s} received an unexpected argument: `{s}`\n", .{ details.cmd, details.arg }),
                .invalid_flag_value => |details| ctx.io.stderr().print("Error: `{s}` is not a valid value for {s}. The valid options are {s}\n", .{ details.value, details.flag, details.valid_options }),
            };
            return error.InvalidArguments;
        },
    };

    // Render any problems accumulated during command execution
    if (ctx.hasProblems()) {
        try ctx.renderProblemsTo(ctx.io.stderr());
        if (ctx.hasErrors()) {
            return error.CliError;
        }
    }
}

/// Generate platform host shim object file using LLVM.
/// Returns the path to the generated object file (allocated from arena, no need to free), or null if LLVM unavailable.
/// If serialized_module is provided, it will be embedded in the binary (for roc build).
/// If serialized_module is null, the binary will use IPC to get module data (for roc run).
/// If debug is true, include debug information in the generated object file.
fn generatePlatformHostShim(ctx: *CliContext, cache_dir: []const u8, entrypoint_names: []const []const u8, target: builder.RocTarget, serialized_module: ?[]const u8, debug: bool) !?[]const u8 {
    // Check if LLVM is available (this is a compile-time check)
    if (!llvm_available) {
        std.log.debug("LLVM not available, skipping platform host shim generation", .{});
        return null;
    }

    const std_zig_llvm = @import("std").zig.llvm;
    const Builder = std_zig_llvm.Builder;

    // Create std.Target for the target RocTarget
    // This is needed so the LLVM Builder generates correct pointer sizes
    const query = std.Target.Query{
        .cpu_arch = target.toCpuArch(),
        .os_tag = target.toOsTag(),
    };
    const std_target = std.zig.system.resolveTargetQuery(query) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    // Create LLVM Builder with the correct target
    var llvm_builder = Builder.init(.{
        .allocator = ctx.gpa,
        .name = "roc_platform_shim",
        .target = &std_target,
        .triple = target.toTriple(),
    }) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };
    defer llvm_builder.deinit();

    // Create entrypoints array from the provided names
    var entrypoints = try std.array_list.Managed(platform_host_shim.EntryPoint).initCapacity(ctx.arena, 8);

    for (entrypoint_names, 0..) |name, idx| {
        try entrypoints.append(.{ .name = name, .idx = @intCast(idx) });
    }

    // Create the complete platform shim
    // Note: Symbol names include platform-specific prefixes (underscore for macOS)
    // serialized_module is null for roc run (IPC mode) or contains data for roc build (embedded mode)
    platform_host_shim.createInterpreterShim(&llvm_builder, entrypoints.items, target, serialized_module) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    // Generate paths for temporary files
    // Use a hash of the serialized module content to avoid race conditions when multiple
    // builds run in parallel. Each unique module content gets its own shim files.
    const content_hash = if (serialized_module) |module_bytes|
        std.hash.Crc32.hash(module_bytes)
    else
        0; // For IPC mode (roc run), use a fixed name since there's no embedded data

    const bitcode_filename = std.fmt.allocPrint(ctx.arena, "platform_shim_{x}.bc", .{content_hash}) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };
    const object_filename = std.fmt.allocPrint(ctx.arena, "platform_shim_{x}.o", .{content_hash}) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    const bitcode_path = std.fs.path.join(ctx.arena, &.{ cache_dir, bitcode_filename }) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    const object_path = std.fs.path.join(ctx.arena, &.{ cache_dir, object_filename }) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    // Generate bitcode first
    const producer = Builder.Producer{
        .name = "Roc Platform Host Shim Generator",
        .version = .{ .major = 1, .minor = 0, .patch = 0 },
    };

    const bitcode = llvm_builder.toBitcode(ctx.gpa, producer) catch |err| {
        return ctx.fail(.{ .object_compilation_failed = .{ .path = bitcode_path, .err = err } });
    };
    defer ctx.gpa.free(bitcode);

    // Write bitcode to file
    const bc_file = std.fs.cwd().createFile(bitcode_path, .{}) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = bitcode_path, .err = err } });
    };
    defer bc_file.close();

    // Convert u32 array to bytes for writing
    const bytes = std.mem.sliceAsBytes(bitcode);
    bc_file.writeAll(bytes) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = bitcode_path, .err = err } });
    };

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .speed,
        .target = target,
        .debug = debug, // Use the debug flag passed from caller
    };

    if (builder.compileBitcodeToObject(ctx.gpa, compile_config)) |success| {
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

fn rocRun(ctx: *CliContext, args: cli_args.RunArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize cache - used to store our shim, and linked interpreter executables in cache
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());

    // Create cache directory for linked interpreter executables
    const exe_cache_dir = cache_manager.config.getExeCacheDir(ctx.arena) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    std.fs.cwd().makePath(exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            return ctx.fail(.{ .directory_create_failed = .{ .path = exe_cache_dir, .err = err } });
        },
    };

    // The final executable name seen in `ps` is the roc filename (e.g., "app.roc")
    const exe_display_name = std.fs.path.basename(args.path);

    // Display name for temp directory (what shows in ps)
    const exe_display_name_with_ext = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(ctx.arena, "{s}.exe", .{exe_display_name}) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        }
    else
        ctx.arena.dupe(u8, exe_display_name) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        };

    // Cache executable name uses hash of path (no PID - collision is fine since same content)
    const exe_cache_name = std.fmt.allocPrint(ctx.arena, "roc_{x}", .{std.hash.crc.Crc32.hash(args.path)}) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    const exe_cache_name_with_ext = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(ctx.arena, "{s}.exe", .{exe_cache_name}) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        }
    else
        ctx.arena.dupe(u8, exe_cache_name) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        };

    const exe_cache_path = std.fs.path.join(ctx.arena, &.{ exe_cache_dir, exe_cache_name_with_ext }) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    // Create unique temp directory for this build (uses PID for uniqueness)
    const temp_dir_path = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };

    // The executable is built directly in the temp dir with the display name
    const exe_path = std.fs.path.join(ctx.arena, &.{ temp_dir_path, exe_display_name_with_ext }) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    // Check if this is a default_app (headerless file with main!)
    if (readDefaultAppSource(ctx, args.path)) |source| {
        return rocRunDefaultApp(ctx, args, source);
    }

    // First, parse the app file to get the platform reference
    const platform_spec = try extractPlatformSpecFromApp(ctx, args.path);

    // Resolve platform paths from the platform spec (relative to app file directory)
    const app_dir = std.fs.path.dirname(args.path) orelse ".";
    const platform_paths = try resolvePlatformSpecToPaths(ctx, platform_spec, app_dir);

    // Validate platform header and get link spec
    var link_spec: ?roc_target.TargetLinkSpec = null;
    var targets_config: ?roc_target.TargetsConfig = null;
    if (platform_paths.platform_source_path) |platform_source| {
        if (platform_validation.validatePlatformHeader(ctx.arena, platform_source)) |validation| {
            targets_config = validation.config;

            // Check if this is a static_lib-only platform (no exe targets)
            if (validation.config.exe.len == 0 and validation.config.static_lib.len > 0) {
                ctx.io.stderr().print("Error: This platform only produces static libraries.\n\n", .{}) catch {};
                ctx.io.stderr().print("Static library platforms produce .a/.lib/.wasm files that must be\n", .{}) catch {};
                ctx.io.stderr().print("linked by a host application. Use 'roc build' instead to produce\n", .{}) catch {};
                ctx.io.stderr().print("the library artifact.\n", .{}) catch {};
                return error.UnsupportedTarget;
            }

            // Select target: if --target is provided, use that; otherwise try native then fallback
            if (args.target) |target_str| {
                // User explicitly specified a target
                const parsed_target = RocTarget.fromString(target_str) orelse {
                    const result = platform_validation.targets_validator.ValidationResult{
                        .invalid_target = .{ .target_str = target_str },
                    };
                    _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.InvalidTarget;
                };

                if (validation.config.getLinkSpec(parsed_target, .exe)) |spec| {
                    link_spec = spec;
                } else {
                    const result = platform_validation.createUnsupportedTargetResult(
                        platform_source,
                        parsed_target,
                        .exe,
                        validation.config,
                    );
                    _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.UnsupportedTarget;
                }
            } else {
                // No --target provided: use the first compatible exe target from the platform
                if (validation.config.getDefaultTarget(.exe)) |compatible_target| {
                    link_spec = validation.config.getLinkSpec(compatible_target, .exe);
                } else {
                    // No compatible exe target found
                    const native_target = builder.RocTarget.detectNative();
                    const result = platform_validation.createUnsupportedTargetResult(
                        platform_source,
                        native_target,
                        .exe,
                        validation.config,
                    );
                    _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.UnsupportedTarget;
                }
            }
        } else |err| {
            switch (err) {
                error.MissingTargetsSection => {
                    ctx.io.stderr().print("Error: Platform is missing a targets section.\n\n", .{}) catch {};
                    ctx.io.stderr().print("All platforms must have a 'targets:' section in their header\n", .{}) catch {};
                    ctx.io.stderr().print("that specifies which targets are supported and what files to link.\n", .{}) catch {};
                    return error.PlatformNotSupported;
                },
                else => {
                    std.log.debug("Could not validate platform header: {}", .{err});
                },
            }
        }
    }

    // All platforms must have a targets section with a link spec for a compatible target
    const validated_link_spec = link_spec orelse {
        ctx.io.stderr().print("Error: Platform does not support any target compatible with this system.\n\n", .{}) catch {};
        ctx.io.stderr().print("The platform's targets section must specify files to link for\n", .{}) catch {};
        ctx.io.stderr().print("the current system. Check the platform header for supported targets.\n", .{}) catch {};
        return error.PlatformNotSupported;
    };

    // Extract entrypoints from platform source file
    var entrypoints = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 32) catch {
        return error.OutOfMemory;
    };

    if (platform_paths.platform_source_path) |platform_source| {
        extractEntrypointsFromPlatform(ctx, platform_source, &entrypoints) catch |err| {
            return ctx.fail(.{ .entrypoint_extraction_failed = .{
                .path = platform_source,
                .reason = @errorName(err),
            } });
        };
    } else {
        return ctx.fail(.{ .entrypoint_extraction_failed = .{
            .path = platform_paths.platform_source_path orelse "<unknown>",
            .reason = "No platform source file found for entrypoint extraction",
        } });
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
        createHardlink(ctx, exe_cache_path, exe_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink from cache failed, copying: {}", .{err});
            std.fs.cwd().copyFile(exe_cache_path, std.fs.cwd(), exe_path, .{}) catch |copy_err| {
                return ctx.fail(.{ .file_write_failed = .{
                    .path = exe_path,
                    .err = copy_err,
                } });
            };
        };
    } else {

        // Extract shim library to temp dir to avoid race conditions
        const shim_filename = if (builtin.target.os.tag == .windows) "roc_shim.lib" else "libroc_shim.a";
        const shim_path = std.fs.path.join(ctx.arena, &.{ temp_dir_path, shim_filename }) catch {
            return error.OutOfMemory;
        };

        // Always extract to temp dir (unique per process, no race condition)
        // Use the selected target's shim (which may differ from native if falling back to a compatible target)
        const selected_target = validated_link_spec.target;
        extractReadRocFilePathShimLibrary(ctx, shim_path, selected_target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };

        // Generate platform host shim using the detected entrypoints
        // Use temp dir to avoid race conditions when multiple processes run in parallel
        // Pass null for serialized_module since roc run uses IPC mode
        // Auto-enable debug when roc is built in debug mode (no explicit --debug flag for roc run)
        const platform_shim_path = try generatePlatformHostShim(ctx, temp_dir_path, entrypoints.items, selected_target, null, builtin.mode == .Debug);

        // Link the host.a with our shim to create the interpreter executable using our linker
        // Try LLD first, fallback to clang if LLVM is not available
        var extra_args = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 32) catch {
            return error.OutOfMemory;
        };

        // Add system libraries for macOS
        if (builtin.target.os.tag == .macos) {
            extra_args.append("-lSystem") catch {
                return error.OutOfMemory;
            };
        }

        // Build object files list from the link spec items
        // Items are linked in the order specified in the targets section
        var object_files = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 16) catch {
            return error.OutOfMemory;
        };

        // Get the platform directory for resolving relative paths
        const platform_dir = if (platform_paths.platform_source_path) |p|
            std.fs.path.dirname(p) orelse "."
        else
            ".";

        // Get files_dir and target name for path resolution
        const files_dir = if (targets_config) |cfg| cfg.files_dir orelse "targets" else "targets";
        const target_name = @tagName(validated_link_spec.target);

        std.log.debug("Platform dir: {s}, files_dir: {s}, target: {s}", .{ platform_dir, files_dir, target_name });

        // Process each link item in order
        for (validated_link_spec.items) |item| {
            switch (item) {
                .file_path => |file_name| {
                    // Resolve path: platform_dir / files_dir / target_name / file_name
                    const full_path = std.fs.path.join(ctx.arena, &.{
                        platform_dir, files_dir, target_name, file_name,
                    }) catch {
                        return error.OutOfMemory;
                    };
                    std.log.debug("Adding link item: {s}", .{full_path});
                    object_files.append(full_path) catch {
                        return error.OutOfMemory;
                    };
                },
                .app => {
                    // Add the compiled Roc application (shim)
                    std.log.debug("Adding app (shim): {s}", .{shim_path});
                    object_files.append(shim_path) catch {
                        return error.OutOfMemory;
                    };
                    // Also add platform shim if available
                    if (platform_shim_path) |path| {
                        object_files.append(path) catch {
                            return error.OutOfMemory;
                        };
                    }
                },
                .win_gui => {
                    // Windows GUI flag - handled separately in linker config
                    std.log.debug("win_gui flag detected", .{});
                },
            }
        }

        // Determine ABI from target (for musl detection)
        const target_abi: linker.TargetAbi = if (validated_link_spec.target.isStatic()) .musl else .gnu;
        std.log.debug("Target ABI: {?}", .{target_abi});

        // No pre/post files needed - everything comes from link spec in order
        const empty_files: []const []const u8 = &.{};

        // Build full path to platform files directory for sysroot lookup
        const platform_files_dir = std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir }) catch {
            return error.OutOfMemory;
        };

        const link_config = linker.LinkConfig{
            .target_abi = target_abi,
            .output_path = exe_path,
            .object_files = object_files.items,
            .platform_files_pre = empty_files,
            .platform_files_post = empty_files,
            .extra_args = extra_args.items,
            .can_exit_early = false,
            .disable_output = false,
            .platform_files_dir = platform_files_dir,
        };

        linker.link(ctx, link_config) catch |err| {
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = @tagName(validated_link_spec.target),
            } });
        };

        // After building, hardlink to cache for future runs
        // Force-hardlink (delete existing first) since hash collision means identical content
        std.log.debug("Caching executable to: {s}", .{exe_cache_path});
        std.fs.cwd().deleteFile(exe_cache_path) catch |err| switch (err) {
            error.FileNotFound => {}, // OK, doesn't exist
            else => std.log.debug("Could not delete existing cache file: {}", .{err}),
        };
        createHardlink(ctx, exe_path, exe_cache_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink to cache failed, copying: {}", .{err});
            std.fs.cwd().copyFile(exe_path, std.fs.cwd(), exe_cache_path, .{}) catch |copy_err| {
                // Non-fatal - just means future runs won't be cached
                std.log.debug("Failed to copy to cache: {}", .{copy_err});
            };
        };
    }

    // Set up shared memory with ModuleEnv using the Coordinator
    const shm_result = try setupSharedMemoryWithCoordinator(ctx, args.path, args.allow_errors);

    // Check for errors - abort unless --allow-errors flag is set
    if (shm_result.error_count > 0 and !args.allow_errors) {
        return error.TypeCheckingFailed;
    }

    const shm_handle = shm_result.handle;

    // Ensure we clean up shared memory resources on all exit paths.
    // Use mapped_size (the full mmap'd region) rather than size (the used portion)
    // to properly unmap the entire shared memory region and release kernel resources.
    defer {
        if (comptime is_windows) {
            _ = ipc.platform.windows.UnmapViewOfFile(shm_handle.ptr);
            _ = ipc.platform.windows.CloseHandle(@ptrCast(shm_handle.fd));
        } else {
            _ = posix.munmap(shm_handle.ptr, shm_handle.mapped_size);
            _ = c.close(shm_handle.fd);
        }
    }

    std.log.debug("Launching interpreter executable: {s}", .{exe_path});
    if (comptime is_windows) {
        // Windows: Use handle inheritance approach
        std.log.debug("Using Windows handle inheritance approach", .{});
        try runWithWindowsHandleInheritance(ctx, exe_path, shm_handle, args.app_args);
    } else {
        // POSIX: Use existing file descriptor inheritance approach
        std.log.debug("Using POSIX file descriptor inheritance approach", .{});
        try runWithPosixFdInheritance(ctx, exe_path, shm_handle, args.app_args);
    }
    std.log.debug("Interpreter execution completed", .{});

    // Exit with code 2 if there were warnings (but no errors)
    if (shm_result.warning_count > 0) {
        ctx.io.flush();
        std.process.exit(2);
    }
}

/// Check if a file is a default_app (headerless file with a main! function).
/// On success, returns the file source (caller owns the allocation).
/// Returns null if the file is not a default_app.
fn readDefaultAppSource(ctx: *CliContext, file_path: []const u8) ?[]const u8 {
    const max_source_size = 256 * 1024 * 1024; // 256 MB
    const source = std.fs.cwd().readFileAlloc(ctx.gpa, file_path, max_source_size) catch return null;

    const module_name = base.module_path.getModuleNameAlloc(ctx.arena, file_path) catch {
        ctx.gpa.free(source);
        return null;
    };

    var env = ModuleEnv.init(ctx.gpa, source) catch {
        ctx.gpa.free(source);
        return null;
    };
    defer env.deinit();
    env.common.source = source;
    env.module_name = module_name;

    var allocators: Allocators = undefined;
    allocators.initInPlace(ctx.gpa);
    defer allocators.deinit();

    const ast = parse.parse(&allocators, &env.common) catch {
        ctx.gpa.free(source);
        return null;
    };
    defer ast.deinit();

    const file = ast.store.getFile();
    const header = ast.store.getHeader(file.header);

    // Only headerless files (type_module) can be default apps
    if (header != .type_module) {
        ctx.gpa.free(source);
        return null;
    }

    if (!ast.hasMainBangDecl()) {
        ctx.gpa.free(source);
        return null;
    }

    return source;
}

/// Virtual file provider for the echo platform.
/// Intercepts reads for the synthetic app source and embedded platform files,
/// delegating all other reads to the real filesystem.
const EchoFileProvider = struct {
    app_abs_path: []const u8,
    synthetic_app_source: []const u8,
    platform_main_path: []const u8,
    echo_module_path: []const u8,

    const FileProvider = compile.package.FileProvider;

    fn read(ctx_ptr: ?*anyopaque, path: []const u8, gpa: std.mem.Allocator) std.mem.Allocator.Error!?[]u8 {
        const self: *@This() = @ptrCast(@alignCast(ctx_ptr.?));
        if (std.mem.eql(u8, path, self.app_abs_path))
            return try gpa.dupe(u8, self.synthetic_app_source);
        if (std.mem.eql(u8, path, self.platform_main_path))
            return try gpa.dupe(u8, echo_platform.platform_main_source);
        if (std.mem.eql(u8, path, self.echo_module_path))
            return try gpa.dupe(u8, echo_platform.echo_module_source);
        return FileProvider.filesystem.read(null, path, gpa);
    }

    fn provider(self: *@This()) FileProvider {
        return .{ .ctx = @ptrCast(self), .read = &@This().read };
    }
};

/// Run a default_app (headerless file with main! and echo platform).
/// This compiles the app with real platform .roc files through the standard
/// multi-module pipeline, JIT-compiles main_for_host!, and executes it.
fn rocRunDefaultApp(ctx: *CliContext, args: cli_args.RunArgs, original_source: []const u8) !void {
    const HostedFn = echo_platform.host_abi.HostedFn;
    const target = RocTarget.detectNative();
    defer ctx.gpa.free(original_source);

    const cwd_tmp = std.process.getCwdAlloc(ctx.gpa) catch return error.OutOfMemory;
    defer ctx.gpa.free(cwd_tmp);
    const app_abs = std.fs.path.resolve(ctx.gpa, &.{ cwd_tmp, args.path }) catch return error.OutOfMemory;
    defer ctx.gpa.free(app_abs);

    // Virtual paths for the echo platform — intercepted by EchoFileProvider,
    // never actually read from disk. Derived from the app's directory so they
    // are valid absolute paths on any OS.
    const app_dir = std.fs.path.dirname(app_abs) orelse ".";
    const platform_main_path = std.fs.path.join(ctx.gpa, &.{ app_dir, ".roc_echo_platform", "main.roc" }) catch return error.OutOfMemory;
    defer ctx.gpa.free(platform_main_path);
    const echo_module_path = std.fs.path.join(ctx.gpa, &.{ app_dir, ".roc_echo_platform", "Echo.roc" }) catch return error.OutOfMemory;
    defer ctx.gpa.free(echo_module_path);

    const header = std.fmt.allocPrint(
        ctx.gpa,
        "app [main!] {{ pf: platform \"{s}\" }}\n\nimport pf.Echo\n\necho! = |msg| Echo.line!(msg)\n\n",
        .{platform_main_path},
    ) catch return error.OutOfMemory;
    defer ctx.gpa.free(header);

    const synthetic_source = std.mem.concat(ctx.gpa, u8, &.{ header, original_source }) catch return error.OutOfMemory;
    defer ctx.gpa.free(synthetic_source);

    // Phase 2: Compile through standard pipeline
    var build_env = try BuildEnv.init(ctx.gpa, .single_threaded, 1, target);
    defer build_env.deinit();

    var echo_fp = EchoFileProvider{
        .app_abs_path = app_abs,
        .synthetic_app_source = synthetic_source,
        .platform_main_path = platform_main_path,
        .echo_module_path = echo_module_path,
    };
    build_env.setFileProvider(echo_fp.provider());

    build_env.discoverDependencies(args.path) catch |err| {
        _ = build_env.renderDiagnostics(ctx.io.stderr());
        return err;
    };

    build_env.compileDiscovered() catch |err| {
        _ = build_env.renderDiagnostics(ctx.io.stderr());
        return err;
    };

    const diag = build_env.renderDiagnostics(ctx.io.stderr());
    if (diag.errors > 0) return error.CompilationFailed;

    // Phase 3: Prepare for execution
    var resolved = try build_env.getResolvedModuleEnvs(ctx.arena);
    try resolved.processHostedFunctions(ctx.gpa, null);
    const entry = try resolved.findEntrypoint();

    // Phase 4: Execute via interpreter
    var hosted_fn_array = [_]HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var roc_ops = echo_platform.makeDefaultRocOps(&hosted_fn_array);
    var cli_args_list = echo_platform.buildCliArgs(args.app_args, &roc_ops);
    var result_buf: [16]u8 align(16) = undefined;

    compile.runner.runViaInterpreter(
        ctx.gpa,
        entry.platform_env,
        build_env.builtin_modules,
        resolved.all_module_envs,
        entry.app_module_env,
        entry.entrypoint_expr,
        &roc_ops,
        @ptrCast(&cli_args_list),
        @ptrCast(&result_buf),
        target,
    ) catch |err| {
        std.debug.print("Execution error: {}\n", .{err});
        std.process.exit(1);
    };

    // Platform returns I8; bit-identical to u8 for std.process.exit
    const exit_code = result_buf[0];
    if (exit_code != 0) {
        std.process.exit(exit_code);
    }
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
fn runWithWindowsHandleInheritance(ctx: *CliContext, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) (CliError || error{OutOfMemory})!void {
    // Make the shared memory handle inheritable
    if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "set handle inheritable",
            .err = error.HandleInheritanceFailed,
        } });
    }

    // Convert paths to Windows wide strings
    const exe_path_w = std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, exe_path) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidUtf8 => return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } }),
    };

    const cwd = std.fs.cwd().realpathAlloc(ctx.arena, ".") catch {
        return ctx.fail(.{ .directory_not_found = .{
            .path = ".",
        } });
    };
    const cwd_w = std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, cwd) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidUtf8 => return ctx.fail(.{ .directory_not_found = .{
            .path = cwd,
        } }),
    };

    // Create command line with handle and size as arguments, plus any app arguments
    const handle_uint = @intFromPtr(shm_handle.fd);

    // Build command line string with proper quoting for Windows
    var cmd_builder = std.array_list.Managed(u8).initCapacity(ctx.gpa, 256) catch {
        return error.OutOfMemory;
    };
    defer cmd_builder.deinit();
    try cmd_builder.writer().print("\"{s}\" {} {}", .{ exe_path, handle_uint, shm_handle.size });
    for (app_args) |arg| {
        try cmd_builder.append(' ');
        try appendWindowsQuotedArg(&cmd_builder, arg);
    }
    try cmd_builder.append(0); // null terminator for sentinel

    const cmd_line = cmd_builder.items[0 .. cmd_builder.items.len - 1 :0];
    const cmd_line_w = std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, cmd_line) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidUtf8 => return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } }),
    };

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
        const last_error = std.os.windows.kernel32.GetLastError();
        std.log.err("CreateProcessW failed with Windows error code: {}", .{last_error});
        std.log.err("exe_path: {s}", .{exe_path});
        std.log.err("cmd_line: {s}", .{cmd_builder.items[0 .. cmd_builder.items.len - 1]});
        std.log.err("cwd: {s}", .{cwd});
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = error.ProcessCreationFailed,
        } });
    }

    // Child process spawned successfully

    // Wait for the child process to complete
    std.log.debug("Waiting for child process to complete: {s}", .{exe_path});
    const wait_result = windows.WaitForSingleObject(process_info.hProcess, windows.INFINITE);
    if (wait_result != 0) { // WAIT_OBJECT_0 = 0
        // Clean up handles before returning
        _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
        _ = ipc.platform.windows.CloseHandle(process_info.hThread);
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = error.ProcessWaitFailed,
        } });
    }

    // Get the exit code
    var exit_code: windows.DWORD = undefined;
    if (windows.GetExitCodeProcess(process_info.hProcess, &exit_code) == 0) {
        // Clean up handles before returning
        _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
        _ = ipc.platform.windows.CloseHandle(process_info.hThread);
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = error.ProcessExitCodeFailed,
        } });
    }

    // Clean up process handles
    _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
    _ = ipc.platform.windows.CloseHandle(process_info.hThread);

    // On Windows, clean up temp files after the child process exits.
    // (Unlike Unix, Windows locks files while they're being executed)
    if (std.fs.path.dirname(exe_path)) |temp_dir_path| {
        compile.CacheCleanup.deleteTempDir(ctx.arena, temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

    // Check exit code and propagate to parent
    if (exit_code != 0) {
        std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
        if (exit_code == 0xC0000005) { // STATUS_ACCESS_VIOLATION
            const result = platform_validation.targets_validator.ValidationResult{
                .process_crashed = .{ .exit_code = exit_code, .is_access_violation = true },
            };
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
        } else if (exit_code >= 0xC0000000) { // NT status codes for exceptions
            const result = platform_validation.targets_validator.ValidationResult{
                .process_crashed = .{ .exit_code = exit_code, .is_access_violation = false },
            };
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
        }
        // Propagate the exit code (truncated to u8 for compatibility)
        std.process.exit(@truncate(exit_code));
    }

    std.log.debug("Child process completed successfully", .{});
}

/// Run child process using POSIX file descriptor inheritance (existing approach for Unix)
/// The exe_path should already be in a unique temp directory created by createUniqueTempDir.
fn runWithPosixFdInheritance(ctx: *CliContext, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) (CliError || error{OutOfMemory})!void {
    // Write the coordination file (.txt) next to the executable
    // The executable is already in a unique temp directory
    std.log.debug("Writing fd coordination file for: {s}", .{exe_path});
    writeFdCoordinationFile(ctx, exe_path, shm_handle) catch |err| {
        // Get the actual .txt file path for error reporting
        const temp_dir = std.fs.path.dirname(exe_path) orelse exe_path;
        const fd_file_path = std.fmt.allocPrint(ctx.arena, "{s}.txt", .{temp_dir}) catch exe_path;
        return ctx.fail(.{ .file_write_failed = .{
            .path = fd_file_path,
            .err = err,
        } });
    };
    std.log.debug("Coordination file written successfully", .{});

    // Configure fd inheritance - clear FD_CLOEXEC so child process inherits the fd
    // Use std.posix.fcntl which properly handles the variadic C function.
    const current_flags = std.posix.fcntl(shm_handle.fd, std.posix.F.GETFD, 0) catch |err| {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "get fd flags",
            .err = err,
        } });
    };

    // Clear FD_CLOEXEC - the flag value is 1
    const new_flags = current_flags & ~@as(usize, 1);
    _ = std.posix.fcntl(shm_handle.fd, std.posix.F.SETFD, new_flags) catch |err| {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "set fd flags",
            .err = err,
        } });
    };

    // Debug-only verification that fd flags were actually cleared
    if (comptime builtin.mode == .Debug) {
        const verify_flags = std.posix.fcntl(shm_handle.fd, std.posix.F.GETFD, 0) catch |err| {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "verify fd flags",
                .err = err,
            } });
        };
        if ((verify_flags & 1) != 0) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "clear FD_CLOEXEC",
                .err = error.FdConfigFailed,
            } });
        }
        std.log.debug("fd={} FD_CLOEXEC cleared successfully", .{shm_handle.fd});
    }

    // Build argv slice using arena allocator (memory lives until arena is freed)
    const argv = ctx.arena.alloc([]const u8, 1 + app_args.len) catch {
        return error.OutOfMemory;
    };
    argv[0] = exe_path;
    for (app_args, 0..) |arg, i| {
        argv[1 + i] = arg;
    }

    // Run the interpreter as a child process from the temp directory
    var child = std.process.Child.init(argv, ctx.gpa);
    child.cwd = std.fs.cwd().realpathAlloc(ctx.arena, ".") catch {
        return ctx.fail(.{ .directory_not_found = .{
            .path = ".",
        } });
    };

    // Forward stdout and stderr
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    // Spawn the child process
    std.log.debug("Spawning child process: {s} with {} app args", .{ exe_path, app_args.len });
    std.log.debug("Child process working directory: {s}", .{child.cwd.?});
    child.spawn() catch |err| {
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };
    std.log.debug("Child process spawned successfully (PID: {})", .{child.id});

    // Wait for child to complete
    const term = child.wait() catch |err| {
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };

    // Clean up temp files after child has exited.
    // We wait until after child exits because the child needs to read the coordination
    // file to find the shared memory before it can run.
    // The background cleanup thread will also clean up old temp directories.
    if (std.fs.path.dirname(exe_path)) |temp_dir_path| {
        compile.CacheCleanup.deleteTempDir(ctx.arena, temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

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
            std.log.debug("Child process {s} killed by signal: {}", .{ exe_path, signal });
            const result = platform_validation.targets_validator.ValidationResult{
                .process_signaled = .{ .signal = signal },
            };
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
            // Standard POSIX convention: exit with 128 + signal number
            std.process.exit(128 +| @as(u8, @truncate(signal)));
        },
        .Stopped => |signal| {
            return ctx.fail(.{ .child_process_signaled = .{
                .command = exe_path,
                .signal = signal,
            } });
        },
        .Unknown => |status| {
            return ctx.fail(.{ .child_process_failed = .{
                .command = exe_path,
                .exit_code = status,
            } });
        },
    }
}

/// Handle for cross-platform shared memory operations.
/// Contains the file descriptor/handle, memory pointer, and size.
pub const SharedMemoryHandle = struct {
    fd: if (is_windows) *anyopaque else c_int,
    ptr: *anyopaque,
    /// The used size of the shared memory (for coordination with child process).
    size: usize,
    /// The total mapped size of the shared memory region (for munmap cleanup).
    /// This may be much larger than `size` since the bump allocator reserves
    /// a large virtual address region upfront.
    mapped_size: usize,
};

/// Result of setting up shared memory with type checking information.
/// Contains the shared memory handle for the compiled modules and
/// counts of errors and warnings encountered during compilation.
pub const SharedMemoryResult = struct {
    handle: SharedMemoryHandle,
    error_count: usize,
    warning_count: usize,
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
        .mapped_size = total_size,
    };
}

/// Set up shared memory with compiled ModuleEnvs from a Roc file and its platform modules.
/// This parses, canonicalizes, and type-checks all modules using the Coordinator actor model,
/// with the resulting ModuleEnvs ending up in shared memory.
///
/// Features:
/// - Uses the Coordinator for compilation (same infrastructure as `roc check` and `roc build`)
/// - Supports multi-threaded compilation (SharedMemoryAllocator is thread-safe)
/// - Platform type modules have their e_anno_only expressions converted to e_hosted_lambda
pub fn setupSharedMemoryWithCoordinator(ctx: *CliContext, roc_file_path: []const u8, allow_errors: bool) !SharedMemoryResult {
    // Create shared memory with SharedMemoryAllocator, trying progressively smaller
    // sizes if larger ones fail (e.g., due to valgrind or overcommit-disabled Linux)
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try createSharedMemoryWithFallback(page_size);
    // Don't defer deinit here - we need to keep the shared memory alive

    const shm_allocator = shm.allocator();

    // Load builtin modules using gpa (not shared memory - builtins are shared read-only)
    var builtin_modules = try eval.BuiltinModules.init(ctx.gpa);
    defer builtin_modules.deinit();

    // If the roc file path has no directory component (e.g., "app.roc"), use current directory
    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";

    const platform_spec = try extractPlatformSpecFromApp(ctx, roc_file_path);

    // Check for absolute paths and reject them early
    try validatePlatformSpec(ctx, platform_spec);

    // Resolve platform path based on type
    const platform_main_path: ?[]const u8 = if (std.mem.startsWith(u8, platform_spec, "./") or std.mem.startsWith(u8, platform_spec, "../"))
        try std.fs.path.join(ctx.arena, &[_][]const u8{ app_dir, platform_spec })
    else if (base.url.isSafeUrl(platform_spec)) blk: {
        const platform_paths = resolveUrlPlatform(ctx, platform_spec) catch |err| switch (err) {
            error.CliError => break :blk null,
            error.OutOfMemory => return error.OutOfMemory,
        };
        break :blk platform_paths.platform_source_path;
    } else null;

    // Get the platform directory from the resolved path
    const platform_dir: ?[]const u8 = if (platform_main_path) |p|
        std.fs.path.dirname(p) orelse return error.InvalidPlatformPath
    else
        null;

    // Extract exposed modules from the platform header (if platform exists)
    var exposed_modules = std.ArrayList([]const u8).empty;
    defer exposed_modules.deinit(ctx.gpa);

    var has_platform = false;
    if (platform_main_path) |pmp| {
        has_platform = true;
        extractExposedModulesFromPlatform(ctx, pmp, &exposed_modules) catch {
            has_platform = false;
        };
    }

    // IMPORTANT: Create header FIRST before any module compilation.
    // The interpreter_shim expects the Header to be at FIRST_ALLOC_OFFSET (504).
    const Header = struct {
        parent_base_addr: u64,
        module_count: u32,
        entry_count: u32,
        def_indices_offset: u64,
        module_envs_offset: u64,
        platform_main_env_offset: u64,
        app_env_offset: u64,
    };

    const header_ptr = try shm_allocator.create(Header);
    const shm_base_addr = @intFromPtr(shm.base_ptr);
    header_ptr.parent_base_addr = shm_base_addr;

    // Allocate module env offsets array (over-allocated, actual count set later)
    const platform_module_count: u32 = @intCast(exposed_modules.items.len);
    const max_sibling_modules: u32 = 64;
    const max_package_modules: u32 = 64;
    const max_module_count: u32 = 1 + platform_module_count + max_sibling_modules + max_package_modules;

    const module_env_offsets_ptr = try shm_allocator.alloc(u64, max_module_count);
    header_ptr.module_envs_offset = @intFromPtr(module_env_offsets_ptr.ptr) - shm_base_addr;

    // Initialize Coordinator
    var coord = try Coordinator.init(
        ctx.gpa, // Use regular allocator for Coordinator internals
        .single_threaded,
        1,
        RocTarget.detectNative(), // IPC runs on host
        &builtin_modules,
        build_options.compiler_version,
        null, // no cache for IPC
    );
    defer coord.deinit();

    // Inject shared memory allocator for module data (ModuleEnv, source)
    coord.setModuleAllocator(shm_allocator);
    coord.owns_module_data = false; // Don't free - shared memory will be unmapped
    coord.enable_hosted_transform = true; // Enable hosted lambda conversion for platform modules

    // Start worker threads
    try coord.start();

    // Set up app package
    const app_pkg = try coord.ensurePackage("app", app_dir);
    const app_module_name = base.module_path.getModuleName(roc_file_path);
    const app_module_id = try app_pkg.ensureModule(ctx.gpa, app_module_name, roc_file_path);
    app_pkg.root_module_id = app_module_id;
    app_pkg.modules.items[app_module_id].depth = 0;
    app_pkg.remaining_modules += 1;
    coord.total_remaining += 1;

    // Extract the platform qualifier from the app header (e.g., "fx" from { fx: platform "..." })
    const platform_qualifier = try extractPlatformQualifier(ctx, roc_file_path);

    // Set up platform package and shorthands
    if (platform_dir) |pf_dir| {
        const pf_pkg = try coord.ensurePackage("pf", pf_dir);

        // Add platform shorthand to app package
        if (platform_qualifier) |qual| {
            try app_pkg.shorthands.put(
                try ctx.gpa.dupe(u8, qual),
                try ctx.gpa.dupe(u8, "pf"),
            );
        }

        // Queue platform main module only
        // Don't pre-queue exposed modules - let the coordinator discover them
        // through import resolution (like roc check does)
        if (platform_main_path) |pmp| {
            const pf_module_id = try pf_pkg.ensureModule(ctx.gpa, "main", pmp);
            pf_pkg.root_module_id = pf_module_id;
            pf_pkg.modules.items[pf_module_id].depth = 1;
            pf_pkg.remaining_modules += 1;
            coord.total_remaining += 1;
            try coord.enqueueParseTask("pf", pf_module_id);
        }
    }

    // Set up non-platform packages (e.g., { hlp: "./helper_pkg/main.roc" })
    var non_platform_packages = try extractNonPlatformPackages(ctx, roc_file_path, platform_qualifier);
    defer {
        var iter = non_platform_packages.iterator();
        while (iter.next()) |entry| {
            ctx.gpa.free(entry.key_ptr.*);
            ctx.gpa.free(entry.value_ptr.*);
        }
        non_platform_packages.deinit();
    }

    var pkg_iter = non_platform_packages.iterator();
    while (pkg_iter.next()) |entry| {
        const shorthand = entry.key_ptr.*;
        const pkg_main_path = entry.value_ptr.*;

        // Get the package directory from the main file path
        const pkg_dir = std.fs.path.dirname(pkg_main_path) orelse ".";

        // Create an internal package name (use shorthand as the package name)
        const pkg_name = try ctx.gpa.dupe(u8, shorthand);
        defer ctx.gpa.free(pkg_name);

        _ = try coord.ensurePackage(pkg_name, pkg_dir);

        // Add shorthand mapping to app package
        // The coordinator will automatically discover and queue modules from this package
        // when the app imports them via scheduleExternalImport
        try app_pkg.shorthands.put(
            try ctx.gpa.dupe(u8, shorthand),
            try ctx.gpa.dupe(u8, pkg_name),
        );
    }

    // Queue app module
    try coord.enqueueParseTask("app", app_module_id);

    // Run coordinator loop
    try coord.coordinatorLoop();

    // Check that app exports match platform requirements
    // This must happen after all modules are type-checked
    try checkPlatformRequirementsFromCoordinator(&coord, ctx, &builtin_modules);

    // Process hosted functions and assign global indices
    // Note: The hosted lambda conversion is done automatically by the Coordinator
    // when enable_hosted_transform is true (done in handleCanonicalized)
    try processHostedFunctionsFromCoordinator(&coord, ctx);

    // Populate header with module offsets from coordinator
    var module_idx: u32 = 0;
    var app_env_offset: u64 = 0;
    var platform_main_env_offset: u64 = 0;

    // Collect platform modules first (excluding platform main, which goes in platform_main_env_offset)
    // The interpreter expects module_env_offsets to contain only exposed platform modules,
    // not the platform main module which is accessed separately.
    if (coord.getPackage("pf")) |pf_pkg| {
        for (pf_pkg.modules.items) |*mod| {
            if (mod.env) |env| {
                const env_offset = @intFromPtr(env) - shm_base_addr;

                // Platform main goes in platform_main_env_offset, NOT in the array
                if (std.mem.eql(u8, mod.name, "main") or std.mem.eql(u8, mod.name, "main.roc")) {
                    platform_main_env_offset = env_offset;
                } else {
                    // Exposed platform modules go in the array
                    module_env_offsets_ptr[module_idx] = env_offset;
                    module_idx += 1;
                }
            }
        }
    }

    // Collect modules from non-platform packages (e.g., hlp)
    var all_pkg_iter = coord.packages.iterator();
    while (all_pkg_iter.next()) |entry| {
        const pkg_name = entry.key_ptr.*;
        // Skip platform and app packages (already handled above/below)
        if (std.mem.eql(u8, pkg_name, "pf") or std.mem.eql(u8, pkg_name, "app")) {
            continue;
        }
        const pkg = entry.value_ptr.*;
        for (pkg.modules.items) |*mod| {
            if (mod.env) |env| {
                const env_offset = @intFromPtr(env) - shm_base_addr;
                module_env_offsets_ptr[module_idx] = env_offset;
                module_idx += 1;
            }
        }
    }

    // Collect app package modules (sibling modules first, then the root app at the end)
    // The interpreter expects the app module at the last index (module_count - 1)
    if (coord.getPackage("app")) |app_pkg_result| {
        const root_id = app_pkg_result.root_module_id;

        // First pass: add sibling modules (non-root modules)
        for (app_pkg_result.modules.items, 0..) |*mod, mod_idx| {
            // Skip root app module - it goes at the end
            if (root_id != null and mod_idx == root_id.?) {
                continue;
            }
            if (mod.env) |env| {
                const env_offset = @intFromPtr(env) - shm_base_addr;
                module_env_offsets_ptr[module_idx] = env_offset;
                module_idx += 1;
            }
        }

        // Second pass: add root app module at the end
        if (root_id) |rid| {
            const root_mod = &app_pkg_result.modules.items[rid];
            if (root_mod.env) |env| {
                const env_offset = @intFromPtr(env) - shm_base_addr;
                module_env_offsets_ptr[module_idx] = env_offset;
                app_env_offset = env_offset;
                module_idx += 1;
            }
        }
    }

    header_ptr.module_count = module_idx;
    header_ptr.app_env_offset = app_env_offset;
    header_ptr.platform_main_env_offset = platform_main_env_offset;

    // Set up entry points from platform exports
    var entry_count: u32 = 0;
    var def_indices_offset: u64 = 0;
    if (platform_main_env_offset != 0) {
        const platform_env: *ModuleEnv = @ptrFromInt(@as(usize, @intCast(platform_main_env_offset + shm_base_addr)));
        const exports_slice = platform_env.store.sliceDefs(platform_env.exports);
        entry_count = @intCast(exports_slice.len);

        if (entry_count > 0) {
            const def_indices_ptr = try shm_allocator.alloc(u32, exports_slice.len);
            def_indices_offset = @intFromPtr(def_indices_ptr.ptr) - shm_base_addr;
            for (exports_slice, 0..) |def_idx, i| {
                def_indices_ptr[i] = @intFromEnum(def_idx);
            }
        }
    }

    header_ptr.entry_count = entry_count;
    header_ptr.def_indices_offset = def_indices_offset;

    // Count errors from all modules
    var error_count: usize = 0;
    var warning_count: usize = 0;

    var pkg_it = coord.packages.iterator();
    while (pkg_it.next()) |entry| {
        const pkg = entry.value_ptr.*;
        for (pkg.modules.items) |*mod| {
            for (mod.reports.items) |*rep| {
                if (rep.severity == .fatal or rep.severity == .runtime_error) {
                    error_count += 1;
                    // Render error to stderr
                    if (!builtin.is_test) {
                        reporting.renderReportToTerminal(rep, ctx.io.stderr(), ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch {};
                    }
                } else if (rep.severity == .warning) {
                    warning_count += 1;
                    // Render warning to stderr
                    if (!builtin.is_test) {
                        reporting.renderReportToTerminal(rep, ctx.io.stderr(), ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch {};
                    }
                }
            }
        }
    }

    // Print summary if there were any problems
    if (error_count > 0 or warning_count > 0) {
        const stderr = ctx.io.stderr();
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) for {s}.\n", .{
            error_count,
            warning_count,
            roc_file_path,
        }) catch {};
    }

    // Flush stderr buffer to ensure errors are visible before execution
    ctx.io.flush();

    // Abort if errors and not allowed
    if (error_count > 0 and !allow_errors) {
        return SharedMemoryResult{
            .handle = SharedMemoryHandle{
                .fd = shm.handle,
                .ptr = shm.base_ptr,
                .size = shm.getUsedSize(),
                .mapped_size = shm.total_size,
            },
            .error_count = error_count,
            .warning_count = warning_count,
        };
    }

    shm.updateHeader();

    return SharedMemoryResult{
        .handle = SharedMemoryHandle{
            .fd = shm.handle,
            .ptr = shm.base_ptr,
            .size = shm.getUsedSize(),
            .mapped_size = shm.total_size,
        },
        .error_count = error_count,
        .warning_count = warning_count,
    };
}

/// Extract the platform qualifier from an app header (e.g., "rr" from { rr: platform "..." })
fn extractPlatformQualifier(ctx: *CliContext, roc_file_path: []const u8) !?[]const u8 {
    var source = std.fs.cwd().readFileAlloc(ctx.gpa, roc_file_path, std.math.maxInt(usize)) catch return null;
    source = base.source_utils.normalizeLineEndingsRealloc(ctx.gpa, source) catch |err| {
        ctx.gpa.free(source);
        return err;
    };
    defer ctx.gpa.free(source);

    var env = ModuleEnv.init(ctx.gpa, source) catch return null;
    defer env.deinit();
    env.common.source = source;

    var allocators: Allocators = undefined;
    allocators.initInPlace(ctx.gpa);
    defer allocators.deinit();

    const parse_ast = parse.parse(&allocators, &env.common) catch return null;
    defer parse_ast.deinit();

    const file_node = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file_node.header);

    if (header == .app) {
        const platform_field = parse_ast.store.getRecordField(header.app.platform_idx);
        const key_region = parse_ast.tokens.resolve(platform_field.name);
        const qualifier = source[key_region.start.offset..key_region.end.offset];
        return try ctx.arena.dupe(u8, qualifier);
    }

    return null;
}

/// Extract non-platform package shorthands from app header.
/// Returns a map of shorthand name -> absolute package path.
/// e.g., for `{ fx: platform "./platform/main.roc", hlp: "./helper_pkg/main.roc" }`,
/// this would return { "hlp" -> "/absolute/path/to/helper_pkg/main.roc" }.
fn extractNonPlatformPackages(
    ctx: *CliContext,
    roc_file_path: []const u8,
    platform_qualifier: ?[]const u8,
) !std.StringHashMap([]const u8) {
    var packages = std.StringHashMap([]const u8).init(ctx.gpa);
    errdefer {
        var iter = packages.iterator();
        while (iter.next()) |entry| {
            ctx.gpa.free(entry.key_ptr.*);
            ctx.gpa.free(entry.value_ptr.*);
        }
        packages.deinit();
    }

    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";

    var source = std.fs.cwd().readFileAlloc(ctx.gpa, roc_file_path, std.math.maxInt(usize)) catch return packages;
    source = base.source_utils.normalizeLineEndingsRealloc(ctx.gpa, source) catch |err| {
        ctx.gpa.free(source);
        return err;
    };
    defer ctx.gpa.free(source);

    var env = ModuleEnv.init(ctx.gpa, source) catch return packages;
    defer env.deinit();
    env.common.source = source;

    var allocators: Allocators = undefined;
    allocators.initInPlace(ctx.gpa);
    defer allocators.deinit();

    const parse_ast = parse.parse(&allocators, &env.common) catch return packages;
    defer parse_ast.deinit();

    const file_node = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file_node.header);

    if (header == .app) {
        const packages_coll = parse_ast.store.getCollection(header.app.packages);
        const packages_fields = parse_ast.store.recordFieldSlice(.{ .span = packages_coll.span });
        for (packages_fields) |field_idx| {
            const field = parse_ast.store.getRecordField(field_idx);
            const key_region = parse_ast.tokens.resolve(field.name);
            const shorthand = source[key_region.start.offset..key_region.end.offset];

            // Skip if this is the platform field
            if (platform_qualifier) |qual| {
                if (std.mem.eql(u8, shorthand, qual)) continue;
            }

            // Get the package path from the field value
            if (field.value) |value_idx| {
                const value_node = parse_ast.store.getExpr(value_idx);
                switch (value_node) {
                    .string => |str| {
                        // Use the region to get the full string
                        const str_region = parse_ast.tokenizedRegionToRegion(str.region);
                        const raw_path = source[str_region.start.offset..str_region.end.offset];
                        if (raw_path.len >= 2 and raw_path[0] == '"' and raw_path[raw_path.len - 1] == '"') {
                            const pkg_rel_path = raw_path[1 .. raw_path.len - 1];
                            // Make absolute path relative to app directory
                            const pkg_abs_path = try std.fs.path.join(ctx.gpa, &.{ app_dir, pkg_rel_path });
                            try packages.put(try ctx.gpa.dupe(u8, shorthand), pkg_abs_path);
                        }
                    },
                    else => {},
                }
            }
        }
    }

    return packages;
}

/// Process hosted functions from coordinator modules and assign global indices.
fn processHostedFunctionsFromCoordinator(coord: *Coordinator, ctx: *CliContext) !void {
    const HostedCompiler = can.HostedCompiler;
    var all_hosted_fns = std.ArrayList(HostedCompiler.HostedFunctionInfo).empty;
    defer all_hosted_fns.deinit(ctx.gpa);

    // Collect from all platform modules
    const pf_pkg = coord.getPackage("pf") orelse return;

    for (pf_pkg.modules.items) |*mod| {
        if (mod.env) |platform_env| {
            var module_fns = try HostedCompiler.collectAndSortHostedFunctions(platform_env);
            defer module_fns.deinit(platform_env.gpa);

            for (module_fns.items) |fn_info| {
                try all_hosted_fns.append(ctx.gpa, fn_info);
            }
        }
    }

    if (all_hosted_fns.items.len == 0) return;

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
            ctx.gpa.free(fn_info.name_text);
        }
    }
    all_hosted_fns.shrinkRetainingCapacity(write_idx);

    // Reassign global indices
    for (pf_pkg.modules.items) |*mod| {
        if (mod.env) |platform_env| {
            const all_defs = platform_env.store.sliceDefs(platform_env.all_defs);
            for (all_defs) |def_idx| {
                const def = platform_env.store.getDef(def_idx);
                const expr = platform_env.store.getExpr(def.expr);

                if (expr == .e_hosted_lambda) {
                    const hosted = expr.e_hosted_lambda;
                    const local_name = platform_env.getIdent(hosted.symbol_name);

                    const plat_module_name = base.module_path.getModuleName(platform_env.module_name);
                    const qualified_name = try std.fmt.allocPrint(ctx.gpa, "{s}.{s}", .{ plat_module_name, local_name });
                    defer ctx.gpa.free(qualified_name);

                    const stripped_name = if (std.mem.endsWith(u8, qualified_name, "!"))
                        qualified_name[0 .. qualified_name.len - 1]
                    else
                        qualified_name;

                    for (all_hosted_fns.items, 0..) |fn_info, idx| {
                        if (std.mem.eql(u8, fn_info.name_text, stripped_name)) {
                            const expr_node_idx = @as(@TypeOf(platform_env.store.nodes).Idx, @enumFromInt(@intFromEnum(def.expr)));
                            var expr_node = platform_env.store.nodes.get(expr_node_idx);
                            var payload = expr_node.getPayload().expr_hosted_lambda;
                            payload.index = @intCast(idx);
                            expr_node.setPayload(.{ .expr_hosted_lambda = payload });
                            platform_env.store.nodes.set(expr_node_idx, expr_node);
                            break;
                        }
                    }
                }
            }
        }
    }
}

/// Check that app exports match platform requirements.
/// This is called after all modules are compiled and type-checked.
/// This mirrors the logic in compile_build.zig's BuildEnv.checkPlatformRequirements.
fn checkPlatformRequirementsFromCoordinator(
    coord: *Coordinator,
    ctx: *CliContext,
    builtin_modules: *eval.BuiltinModules,
) !void {
    // Find app and platform packages
    const app_pkg = coord.getPackage("app") orelse return;
    const pf_pkg = coord.getPackage("pf") orelse return;

    // Get the app's root module env
    const app_root_id = app_pkg.root_module_id orelse return;
    const app_root_env: *ModuleEnv = app_pkg.modules.items[app_root_id].env orelse return;

    // Get the platform's root module env (the "main" module containing the requires clause)
    var platform_root_env: ?*ModuleEnv = null;
    for (pf_pkg.modules.items) |*mod| {
        if (std.mem.eql(u8, mod.name, "main") or std.mem.eql(u8, mod.name, "main.roc")) {
            if (mod.env) |env| {
                platform_root_env = env;
                break;
            }
        }
    }
    const pf_root_env = platform_root_env orelse return;

    // If the platform has no requires_types, nothing to check
    if (pf_root_env.requires_types.items.items.len == 0) {
        return;
    }

    // Get builtin indices and module
    const builtin_indices = builtin_modules.builtin_indices;
    const builtin_module_env = builtin_modules.builtin_module.env;

    // Build module_envs_map for type resolution
    var module_envs_map = std.AutoHashMap(base.Ident.Idx, Can.AutoImportedType).init(ctx.gpa);
    defer module_envs_map.deinit();

    // Use the shared populateModuleEnvs function to set up auto-imported types
    try Can.populateModuleEnvs(&module_envs_map, app_root_env, builtin_module_env, builtin_indices);

    // Build builtin context for the type checker
    const builtin_ctx = Check.BuiltinContext{
        .module_name = app_root_env.qualified_module_ident,
        .bool_stmt = builtin_indices.bool_type,
        .try_stmt = builtin_indices.try_type,
        .str_stmt = builtin_indices.str_type,
        .builtin_module = builtin_module_env,
        .builtin_indices = builtin_indices,
    };

    // Create type checker for the app module
    var checker = try Check.init(
        ctx.gpa,
        &app_root_env.types,
        app_root_env,
        &.{}, // No imported modules needed for checking exports
        &module_envs_map,
        &app_root_env.store.regions,
        builtin_ctx,
    );
    defer checker.deinit();

    // Build the platform-to-app ident translation map
    // This translates platform requirement idents to app idents by name
    var platform_to_app_idents = std.AutoHashMap(base.Ident.Idx, base.Ident.Idx).init(ctx.gpa);
    defer platform_to_app_idents.deinit();

    for (pf_root_env.requires_types.items.items) |required_type| {
        const platform_ident_text = pf_root_env.getIdent(required_type.ident);
        if (app_root_env.common.findIdent(platform_ident_text)) |app_ident| {
            try platform_to_app_idents.put(required_type.ident, app_ident);
        }

        // Also add for-clause type alias names (Model, model) to the translation map
        const all_aliases = pf_root_env.for_clause_aliases.items.items;
        const type_aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
        for (type_aliases_slice) |alias| {
            // Add alias name (e.g., "Model")
            const alias_name_text = pf_root_env.getIdentText(alias.alias_name);
            const alias_app_ident = try app_root_env.common.insertIdent(ctx.gpa, base.Ident.for_text(alias_name_text));
            try platform_to_app_idents.put(alias.alias_name, alias_app_ident);

            // Add rigid name (e.g., "model")
            const rigid_name_text = pf_root_env.getIdentText(alias.rigid_name);
            const rigid_app_ident = try app_root_env.common.insertIdent(ctx.gpa, base.Ident.for_text(rigid_name_text));
            try platform_to_app_idents.put(alias.rigid_name, rigid_app_ident);
        }
    }

    // Check platform requirements against app exports
    try checker.checkPlatformRequirements(pf_root_env, &platform_to_app_idents);

    // Now finalize numeric defaults for the app module. This must happen AFTER
    // checkPlatformRequirements so that numeric literals can be constrained by
    // platform types (e.g., I64) before defaulting to Dec.
    try checker.finalizeNumericDefaults();

    // If there are type problems, convert them to reports and add to app module
    if (checker.problems.problems.items.len > 0) {
        const app_module = &app_pkg.modules.items[app_root_id];
        const app_path = app_module.path;

        var rb = try check.ReportBuilder.init(
            ctx.gpa,
            app_root_env,
            app_root_env,
            &checker.snapshots,
            &checker.problems,
            app_path,
            &.{},
            &checker.import_mapping,
            &checker.regions,
        );
        defer rb.deinit();

        for (checker.problems.problems.items) |prob| {
            const rep = rb.build(prob) catch continue;
            try app_module.reports.append(ctx.gpa, rep);
        }
    }
}

/// Extract exposed modules from a platform's main.roc file
fn extractExposedModulesFromPlatform(ctx: *CliContext, roc_file_path: []const u8, exposed_modules: *std.ArrayList([]const u8)) !void {
    // Read the Roc file
    var source = std.fs.cwd().readFileAlloc(ctx.gpa, roc_file_path, std.math.maxInt(usize)) catch return error.NoPlatformFound;
    source = base.source_utils.normalizeLineEndingsRealloc(ctx.gpa, source) catch |err| {
        ctx.gpa.free(source);
        return err;
    };
    defer ctx.gpa.free(source);

    // Extract module name from the file path (strip .roc extension)
    const module_name = try base.module_path.getModuleNameAlloc(ctx.arena, roc_file_path);

    // Create ModuleEnv
    var env = ModuleEnv.init(ctx.gpa, source) catch return error.ParseFailed;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(ctx.gpa);

    // Parse the source code as a full module
    var allocators: Allocators = undefined;
    allocators.initInPlace(ctx.gpa);
    defer allocators.deinit();

    const parse_ast = parse.parse(&allocators, &env.common) catch return error.ParseFailed;
    defer parse_ast.deinit();

    // Look for platform header in the AST
    const file_node = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file_node.header);

    // Check if this is a platform file with a platform header
    switch (header) {
        .platform => |platform_header| {
            // Validate platform header has targets section (non-blocking warning)
            // This helps platform authors know they need to add targets
            _ = validatePlatformHeader(ctx, parse_ast, roc_file_path);

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
                try exposed_modules.append(ctx.gpa, try ctx.arena.dupe(u8, item_name));
            }
        },
        else => {
            return error.NotPlatformFile;
        },
    }
}

/// Validate a platform header and report any errors/warnings
/// Returns true if valid, false if there are validation issues
/// This currently only warns about missing targets sections - it doesn't block compilation
fn validatePlatformHeader(ctx: *CliContext, parse_ast: *const parse.AST, platform_path: []const u8) bool {
    const validation_result = targets_validator.validatePlatformHasTargets(parse_ast.*, platform_path);

    switch (validation_result) {
        .valid => return true,
        else => {
            // Create and render the validation report
            var report = targets_validator.createValidationReport(ctx.gpa, validation_result) catch {
                std.log.warn("Platform at {s} is missing targets section", .{platform_path});
                return false;
            };
            defer report.deinit();

            // Render to stderr
            if (!builtin.is_test) {
                reporting.renderReportToTerminal(&report, ctx.io.stderr(), .ANSI, reporting.ReportingConfig.initColorTerminal()) catch {};
            }
            return false;
        },
    }
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
    );
    // mmap returns MAP_FAILED ((void*)-1) on error, not NULL
    if (mapped_ptr == posix.MAP_FAILED) {
        _ = c.close(shm_fd);
        return error.SharedMemoryMapFailed;
    }
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
        .mapped_size = total_size,
    };
}

/// Platform resolution result containing the platform source path
pub const PlatformPaths = struct {
    platform_source_path: ?[]const u8, // Optional - may not exist for some platforms
};

/// Resolve platform specification from a Roc file to find both host library and platform source.
/// Returns PlatformPaths with arena-allocated paths (no need to free).
pub fn resolvePlatformPaths(ctx: *CliContext, roc_file_path: []const u8) CliError!PlatformPaths {
    // Use the parser to extract the platform spec
    const platform_spec = extractPlatformSpecFromApp(ctx, roc_file_path) catch {
        return ctx.fail(.{ .file_not_found = .{
            .path = roc_file_path,
            .context = .source_file,
        } });
    };
    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";
    return resolvePlatformSpecToPaths(ctx, platform_spec, app_dir);
}

/// Extract platform specification from app file header by parsing it properly.
/// Takes a CliContext which provides allocators and error reporting.
fn extractPlatformSpecFromApp(ctx: *CliContext, app_file_path: []const u8) ![]const u8 {
    // Read the app file
    var source = std.fs.cwd().readFileAlloc(ctx.gpa, app_file_path, std.math.maxInt(usize)) catch |err| {
        return ctx.fail(switch (err) {
            error.FileNotFound => .{ .file_not_found = .{
                .path = app_file_path,
                .context = .source_file,
            } },
            else => .{ .file_read_failed = .{
                .path = app_file_path,
                .err = err,
            } },
        });
    };
    source = base.source_utils.normalizeLineEndingsRealloc(ctx.gpa, source) catch |err| {
        ctx.gpa.free(source);
        return err;
    };
    defer ctx.gpa.free(source);

    // Extract module name from file path (strips .roc extension)
    const module_name = try base.module_path.getModuleNameAlloc(ctx.arena, app_file_path);

    // Create ModuleEnv for parsing
    var env = ModuleEnv.init(ctx.gpa, source) catch {
        return ctx.fail(.{ .module_init_failed = .{
            .path = app_file_path,
            .err = error.OutOfMemory,
        } });
    };
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    env.common.calcLineStarts(ctx.gpa) catch {
        return ctx.fail(.{ .module_init_failed = .{
            .path = app_file_path,
            .err = error.OutOfMemory,
        } });
    };

    // Parse the source
    var allocators: Allocators = undefined;
    allocators.initInPlace(ctx.gpa);
    defer allocators.deinit();

    const ast = parse.parse(&allocators, &env.common) catch {
        return ctx.fail(.{ .module_init_failed = .{
            .path = app_file_path,
            .err = error.OutOfMemory,
        } });
    };
    defer ast.deinit();

    // Get the file header
    const file = ast.store.getFile();
    const header = ast.store.getHeader(file.header);

    // Check if this is an app file
    switch (header) {
        .app => |a| {
            // Get the platform field
            const pf = ast.store.getRecordField(a.platform_idx);
            const value_expr = pf.value orelse {
                return ctx.fail(.{ .expected_platform_string = .{ .path = app_file_path } });
            };

            // Extract the string value from the expression
            const platform_spec = stringFromExpr(ast, value_expr) catch {
                return ctx.fail(.{ .expected_platform_string = .{ .path = app_file_path } });
            };
            return try ctx.arena.dupe(u8, platform_spec);
        },
        else => {
            return ctx.fail(.{ .expected_app_header = .{
                .path = app_file_path,
                .found = @tagName(header),
            } });
        },
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

/// Check if platform spec is an absolute path and reject it.
/// Uses CliContext for error reporting.
fn validatePlatformSpec(ctx: *CliContext, platform_spec: []const u8) CliError!void {
    if (std.fs.path.isAbsolute(platform_spec)) {
        return ctx.fail(.{ .absolute_platform_path = .{ .platform_spec = platform_spec } });
    }
}

/// Resolve a platform specification to a platform source path.
/// Uses CliContext for error reporting.
fn resolvePlatformSpecToPaths(ctx: *CliContext, platform_spec: []const u8, base_dir: []const u8) CliError!PlatformPaths {
    // Handle URL-based platforms
    if (std.mem.startsWith(u8, platform_spec, "http")) {
        return resolveUrlPlatform(ctx, platform_spec) catch |err| switch (err) {
            error.CliError => return error.CliError,
            error.OutOfMemory => return ctx.fail(.{ .cache_dir_unavailable = .{
                .reason = "Out of memory while resolving URL platform",
            } }),
        };
    }

    // Check for absolute paths and reject them
    try validatePlatformSpec(ctx, platform_spec);

    // Try to interpret as a file path (must be relative, resolve relative to base_dir)
    const resolved_path = std.fs.path.join(ctx.arena, &.{ base_dir, platform_spec }) catch {
        return ctx.fail(.{ .file_read_failed = .{
            .path = platform_spec,
            .err = error.OutOfMemory,
        } });
    };

    std.fs.cwd().access(resolved_path, .{}) catch {
        return ctx.fail(.{ .platform_not_found = .{
            .app_path = base_dir,
            .platform_path = resolved_path,
        } });
    };

    // Platform spec should point to a .roc file
    if (std.mem.endsWith(u8, resolved_path, ".roc")) {
        return PlatformPaths{
            .platform_source_path = ctx.arena.dupe(u8, resolved_path) catch {
                return ctx.fail(.{ .file_read_failed = .{
                    .path = resolved_path,
                    .err = error.OutOfMemory,
                } });
            },
        };
    } else {
        // Non-.roc file path - not supported
        return ctx.fail(.{ .platform_validation_failed = .{
            .message = "Platform path must end with .roc",
        } });
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

/// Resolve a URL platform specification by downloading and caching the bundle.
/// The URL must point to a .tar.zst bundle with a base58-encoded BLAKE3 hash filename.
fn resolveUrlPlatform(ctx: *CliContext, url: []const u8) (CliError || error{OutOfMemory})!PlatformPaths {
    const download = unbundle.download;

    // 1. Validate URL and extract hash
    const base58_hash = download.validateUrl(url) catch {
        return ctx.fail(.{ .invalid_url = .{
            .url = url,
            .reason = "Invalid platform URL format or missing hash",
        } });
    };

    // 2. Get cache directory
    const cache_dir_path = getRocCacheDir(ctx.arena) catch {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = "Could not determine cache directory" } });
    };
    const package_dir_path = try std.fs.path.join(ctx.arena, &.{ cache_dir_path, base58_hash });

    // 3. Check if already cached
    var package_dir = std.fs.cwd().openDir(package_dir_path, .{}) catch |err| switch (err) {
        error.FileNotFound => blk: {
            // Not cached - need to download
            std.log.info("Downloading platform from {s}...", .{url});

            // Create cache directory structure
            std.fs.cwd().makePath(cache_dir_path) catch |make_err| {
                return ctx.fail(.{ .directory_create_failed = .{
                    .path = cache_dir_path,
                    .err = make_err,
                } });
            };

            // Create package directory
            std.fs.cwd().makeDir(package_dir_path) catch |make_err| switch (make_err) {
                error.PathAlreadyExists => {}, // Race condition, another process created it
                else => {
                    return ctx.fail(.{ .directory_create_failed = .{
                        .path = package_dir_path,
                        .err = make_err,
                    } });
                },
            };

            var new_package_dir = std.fs.cwd().openDir(package_dir_path, .{}) catch {
                return ctx.fail(.{ .directory_not_found = .{
                    .path = package_dir_path,
                } });
            };

            // Download and extract
            var gpa_copy = ctx.gpa;
            download.downloadAndExtract(&gpa_copy, url, new_package_dir) catch |download_err| {
                // Clean up failed download
                new_package_dir.close();
                std.fs.cwd().deleteTree(package_dir_path) catch {};
                return ctx.fail(.{ .download_failed = .{
                    .url = url,
                    .err = download_err,
                } });
            };

            std.log.info("Platform cached at {s}", .{package_dir_path});
            break :blk new_package_dir;
        },
        else => {
            return ctx.fail(.{ .directory_not_found = .{
                .path = package_dir_path,
            } });
        },
    };
    defer package_dir.close();

    // Platforms must have a main.roc entry point
    const platform_source_path = try std.fs.path.join(ctx.arena, &.{ package_dir_path, "main.roc" });
    std.fs.cwd().access(platform_source_path, .{}) catch {
        return ctx.fail(.{ .platform_source_not_found = .{
            .platform_path = package_dir_path,
            .searched_paths = &.{platform_source_path},
        } });
    };

    return PlatformPaths{
        .platform_source_path = platform_source_path,
    };
}

/// Extract all entrypoint names from platform header provides record into ArrayList
/// TODO: Replace this with proper BuildEnv solution in the future
fn extractEntrypointsFromPlatform(ctx: *CliContext, roc_file_path: []const u8, entrypoints: *std.array_list.Managed([]const u8)) !void {
    // Read the Roc file
    var source = std.fs.cwd().readFileAlloc(ctx.gpa, roc_file_path, std.math.maxInt(usize)) catch return error.NoPlatformFound;
    source = base.source_utils.normalizeLineEndingsRealloc(ctx.gpa, source) catch |err| {
        ctx.gpa.free(source);
        return err;
    };
    defer ctx.gpa.free(source);

    // Extract module name from the file path (strip .roc extension)
    const module_name = try base.module_path.getModuleNameAlloc(ctx.arena, roc_file_path);

    // Create ModuleEnv
    var env = ModuleEnv.init(ctx.gpa, source) catch return error.ParseFailed;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    try env.common.calcLineStarts(ctx.gpa);

    // Parse the source code as a full module
    var allocators2: Allocators = undefined;
    allocators2.initInPlace(ctx.gpa);
    defer allocators2.deinit();

    const parse_ast = parse.parse(&allocators2, &env.common) catch return error.ParseFailed;
    defer parse_ast.deinit();

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
                            return error.InvalidProvidesEntry;
                        },
                        .string_part => |str_part| break :blk parse_ast.resolve(str_part.token),
                        else => {
                            return error.InvalidProvidesEntry;
                        },
                    }
                } else {
                    return error.InvalidProvidesEntry;
                };
                try entrypoints.append(try ctx.arena.dupe(u8, symbol_name));
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

/// Extract the embedded roc_shim library to the specified path for the given target.
/// This library contains the shim code that runs in child processes to read ModuleEnv from shared memory.
/// For native builds and roc run, use the native shim (pass null or native target).
/// For cross-compilation, pass the target to get the appropriate shim.
pub fn extractReadRocFilePathShimLibrary(ctx: *CliContext, output_path: []const u8, target: ?RocTarget) !void {
    _ = ctx; // unused but kept for consistency

    if (builtin.is_test) {
        // In test mode, create an empty file to avoid embedding issues
        const shim_file = try std.fs.cwd().createFile(output_path, .{});
        defer shim_file.close();
        return;
    }

    // Get the appropriate shim for the target (or native if not specified)
    const shim_data = if (target) |t|
        ShimLibraries.forTarget(t)
    else
        ShimLibraries.native;

    // Write the embedded shim library to the output path
    const shim_file = try std.fs.cwd().createFile(output_path, .{});
    defer shim_file.close();

    try shim_file.writeAll(shim_data);
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

/// Validate a bundle by using the Coordinator to discover all transitive module
/// dependencies, then checking that every discovered .roc file is present in the
/// bundle file list. Also validates platform target binaries if a platform is found.
fn validateBundleWithCoordinator(
    ctx: *CliContext,
    first_roc_file: []const u8,
    bundled_file_paths: []const []const u8,
    stderr: anytype,
) !void {
    // Resolve the entry point to an absolute path
    const abs_entry = std.fs.cwd().realpathAlloc(ctx.gpa, first_roc_file) catch |err| {
        try stderr.print("Error: Could not resolve path '{s}': {}\n", .{ first_roc_file, err });
        return err;
    };
    defer ctx.gpa.free(abs_entry);

    // Create a BuildEnv to parse headers and discover modules via the Coordinator
    var build_env = try BuildEnv.init(ctx.gpa, .single_threaded, 1, RocTarget.detectNative());
    defer build_env.deinit();

    // Run the build — the Coordinator discovers all transitive module dependencies
    build_env.build(abs_entry) catch {
        // Drain and display any errors from the build
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.freeDrainedReportsPathsOnly(drained);

        for (drained) |mod| {
            for (mod.reports) |report| {
                switch (report.severity) {
                    .runtime_error, .fatal => {
                        try stderr.print("{s}: error in module\n", .{mod.abs_path});
                    },
                    .warning => {
                        try stderr.print("{s}: warning in module\n", .{mod.abs_path});
                    },
                    .info => {},
                }
            }
        }
        // Build errors are not fatal for bundling — continue to check what we can
    };

    // Detect platform from BuildEnv packages using the accessor
    const platform_root_file = build_env.getPlatformRootFile();

    // Collect all discovered module paths from the Coordinator
    var required_paths = std.ArrayList([]const u8).empty;
    defer required_paths.deinit(ctx.gpa);

    if (build_env.coordinator) |coord| {
        var coord_pkg_it = coord.packages.iterator();
        while (coord_pkg_it.next()) |pkg_entry| {
            for (pkg_entry.value_ptr.*.modules.items) |mod_state| {
                try required_paths.append(ctx.gpa, mod_state.path);
            }
        }
    }

    // Build a set of absolute paths from the bundle file list for quick lookup
    var bundled_set = std.StringHashMap(void).init(ctx.gpa);
    defer bundled_set.deinit();

    for (bundled_file_paths) |rel_path| {
        const abs_path = std.fs.cwd().realpathAlloc(ctx.gpa, rel_path) catch continue;
        defer ctx.gpa.free(abs_path);
        try bundled_set.put(try ctx.arena.dupe(u8, abs_path), {});
    }

    // Compare discovered module paths against bundled file list
    var missing_count: u32 = 0;
    for (required_paths.items) |req_path| {
        if (!bundled_set.contains(req_path)) {
            // Try to make the path relative for a nicer error message
            const display_path = std.fs.path.relative(ctx.arena, ".", req_path) catch req_path;
            try stderr.print("Error: Required module file is missing from bundle: {s}\n", .{display_path});
            missing_count += 1;
        }
    }

    if (missing_count > 0) {
        try stderr.print("Error: {} required module file(s) missing from bundle\n", .{missing_count});
        return error.MissingBundleFiles;
    }

    // If a platform was detected, validate target binaries exist
    // Use TargetsConfig from BuildEnv (already extracted during header parsing)
    if (platform_root_file) |pf| {
        if (build_env.getPlatformTargetsConfig()) |tc| {
            const pf_dir = std.fs.path.dirname(pf) orelse ".";
            if (platform_validation.validateAllTargetFilesExist(ctx.arena, tc, pf_dir)) |result| {
                _ = platform_validation.renderValidationError(ctx.gpa, result, stderr);
                return switch (result) {
                    .missing_target_file => error.MissingTargetFile,
                    .missing_files_directory => error.MissingFilesDirectory,
                    else => error.MissingTargetFile,
                };
            }
        }
    }
}

/// Bundles a roc package and its dependencies into a compressed tar archive
pub fn rocBundle(ctx: *CliContext, args: cli_args.BundleArgs) !void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

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
    defer file_paths.deinit(ctx.arena);

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

        try file_paths.append(ctx.arena, path);
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

    // Find the first .roc file to use as entry point for Coordinator-based validation
    const first_roc_file: ?[]const u8 = for (file_paths.items) |path| {
        if (std.mem.endsWith(u8, path, ".roc")) break path;
    } else null;

    // Use the Coordinator to discover all transitive module dependencies and validate
    if (first_roc_file) |roc_file| {
        try validateBundleWithCoordinator(ctx, roc_file, file_paths.items, stderr);
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
    var allocator_copy = ctx.arena;
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
        switch (err) {
            error.InvalidPath => {
                try stderr.print("Error: Invalid file path - {s}\n", .{formatBundlePathValidationReason(error_ctx.reason)});
                try stderr.print("Path: {s}\n", .{error_ctx.path});
            },
            else => {},
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
        try std.fs.path.join(ctx.arena, &.{ args.output_dir.?, final_filename });
    // No need to free when using arena allocator

    // Print results
    try stdout.print("Created: {s}\n", .{display_path});
    try stdout.print("Compressed size: {} bytes\n", .{compressed_size});
    try stdout.print("Uncompressed size: {} bytes\n", .{uncompressed_size});
    try stdout.print("Compression ratio: {d:.2}:1\n", .{@as(f64, @floatFromInt(uncompressed_size)) / @as(f64, @floatFromInt(compressed_size))});
    try stdout.print("Time: {} ms\n", .{elapsed_ms});
}

fn rocUnbundle(ctx: *CliContext, args: cli_args.UnbundleArgs) !void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();
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
            ctx.gpa,
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

fn rocBuild(ctx: *CliContext, args: cli_args.BuildArgs) !void {
    // Handle the --z-bench-tokenize flag
    if (args.z_bench_tokenize) |file_path| {
        try benchTokenizer(ctx.gpa, file_path);
        return;
    }

    // Handle the --z-bench-parse flag
    if (args.z_bench_parse) |directory_path| {
        try benchParse(ctx.gpa, directory_path);
        return;
    }

    // Select build path based on backend
    switch (args.backend) {
        .dev => {
            // Use native code generation backend
            try rocBuildNative(ctx, args);
        },
        .interpreter => {
            // Use embedded interpreter build approach
            // This compiles the Roc app, serializes the ModuleEnv, and embeds it in the binary
            try rocBuildEmbedded(ctx, args);
        },
    }
}

/// Build using the dev backend to generate native machine code.
/// This produces truly compiled executables without an interpreter.
fn rocBuildNative(ctx: *CliContext, args: cli_args.BuildArgs) !void {
    const target_mod = @import("target.zig");

    var timer = try std.time.Timer.start();

    std.log.info("Building {s} with native dev backend", .{args.path});

    // Determine output path
    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else blk: {
        break :blk try base.module_path.getModuleNameAlloc(ctx.arena, args.path);
    };

    // Set up cache directory for build artifacts
    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(ctx.arena);
    const build_cache_dir = try std.fs.path.join(ctx.arena, &.{ cache_dir, "roc_build" });

    std.fs.cwd().makePath(build_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Phase 1: Create BuildEnv with native target for discovery
    const thread_count: usize = if (args.max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: compile.package.Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative());
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager for compilation caching
    if (!args.no_cache) {
        const build_cache_manager = try ctx.gpa.create(CacheManager);
        build_cache_manager.* = CacheManager.init(ctx.gpa, .{
            .enabled = true,
            .verbose = args.verbose,
        }, Filesystem.default());
        build_env.setCacheManager(build_cache_manager);
    }

    // Phase 2: Discover dependencies (parses headers once, extracts TargetsConfig)
    build_env.discoverDependencies(args.path) catch |err| {
        _ = build_env.renderDiagnostics(ctx.io.stderr());
        return err;
    };

    // Phase 3: Get TargetsConfig from the discovered platform package
    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };

    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |ps| (std.fs.path.dirname(ps) orelse ".") else ".";

    // Phase 4: Select target and link type
    const target: target_mod.RocTarget, const link_type: target_mod.LinkType = if (args.target) |target_str| blk: {
        const parsed_target = target_mod.RocTarget.fromString(target_str) orelse {
            const result = platform_validation.targets_validator.ValidationResult{
                .invalid_target = .{ .target_str = target_str },
            };
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.InvalidTarget;
        };

        const lt: target_mod.LinkType = if (targets_config.supportsTarget(parsed_target, .exe))
            .exe
        else if (targets_config.supportsTarget(parsed_target, .static_lib))
            .static_lib
        else if (targets_config.supportsTarget(parsed_target, .shared_lib))
            .shared_lib
        else {
            const result = platform_validation.createUnsupportedTargetResult(
                platform_source orelse "<unknown>",
                parsed_target,
                .exe,
                targets_config,
            );
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.UnsupportedTarget;
        };

        break :blk .{ parsed_target, lt };
    } else blk: {
        const compatible = targets_config.getFirstCompatibleTarget() orelse {
            renderProblem(ctx.gpa, ctx.io.stderr(), .{
                .platform_validation_failed = .{
                    .message = "No compatible target found. The platform does not support any target compatible with this system.",
                },
            });
            return error.UnsupportedTarget;
        };
        break :blk .{ compatible.target, compatible.link_type };
    };

    std.log.debug("Target: {s}, Link type: {s}", .{ @tagName(target), @tagName(link_type) });

    // Check if dev backend supports this target architecture
    const target_arch = target.toCpuArch();
    const target_os = target.toOsTag();
    switch (target_arch) {
        .x86_64, .aarch64 => {}, // Supported
        else => {
            const stdout = ctx.io.stdout();
            try stdout.print("Note: Dev backend does not support {s} architecture.\n", .{@tagName(target_arch)});
            try stdout.print("Falling back to interpreter mode.\n\n", .{});
            return rocBuildEmbedded(ctx, args);
        },
    }

    // Add appropriate file extension based on target and link type
    const final_output_path = if (args.output != null)
        output_path
    else blk: {
        const ext = switch (link_type) {
            .exe => switch (target_os) {
                .windows => ".exe",
                .freestanding => ".wasm",
                else => "",
            },
            .static_lib => switch (target_os) {
                .windows => ".lib",
                else => ".a",
            },
            .shared_lib => switch (target_os) {
                .windows => ".dll",
                .macos => ".dylib",
                else => ".so",
            },
        };
        break :blk try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{ output_path, ext });
    };

    // Phase 5: Set target and compile
    build_env.setTarget(target);

    build_env.compileDiscovered() catch |err| {
        _ = build_env.renderDiagnostics(ctx.io.stderr());
        return err;
    };

    const diag = build_env.renderDiagnostics(ctx.io.stderr());
    if (diag.errors > 0 and !args.allow_errors) {
        return error.CompilationFailed;
    }

    // Get resolved module envs (Builtin at [0], imports re-resolved)
    var resolved = build_env.getResolvedModuleEnvs(ctx.arena) catch |err| {
        std.log.err("Failed to get compiled modules: {}", .{err});
        return err;
    };
    const modules = resolved.compiled_modules;
    const all_module_envs = resolved.all_module_envs;

    std.log.debug("Found {} modules", .{modules.len});

    // Find platform module and validate provides entries
    const plat = resolved.getPlatformModule() catch {
        return ctx.fail(.{ .entrypoint_extraction_failed = .{
            .path = platform_source.?,
            .reason = "NoEntrypointFound",
        } });
    };
    const platform_module = plat.module;
    const platform_idx = plat.platform_idx;
    const provides_entries = plat.provides_entries;
    std.log.debug("Found {} provides entries", .{provides_entries.len});

    // Compiled modules (excluding Builtin at index 0) for pipelines that shouldn't process Builtin
    const compiled_module_envs = resolved.compiledModuleEnvs();

    // Run closure pipeline on modules (lambda lifting, inference, transformation)
    std.log.debug("Running closure pipeline...", .{});
    for (compiled_module_envs) |module| {
        if (!module.is_lambda_lifted) {
            var top_level_patterns = std.AutoHashMap(can.CIR.Pattern.Idx, void).init(ctx.gpa);
            defer top_level_patterns.deinit();

            const stmts = module.store.sliceStatements(module.all_statements);
            for (stmts) |stmt_idx| {
                const stmt = module.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl| {
                        top_level_patterns.put(decl.pattern, {}) catch {};
                    },
                    else => {},
                }
            }

            var lifter = can.LambdaLifter.init(ctx.gpa, module, &top_level_patterns);
            defer lifter.deinit();
            module.is_lambda_lifted = true;
        }
    }

    // Run lambda set inference
    var lambda_inference = can.LambdaSetInference.init(ctx.gpa);
    defer lambda_inference.deinit();

    // Convert to mutable slice for inferAll (compiled modules only)
    var mutable_envs = try ctx.arena.alloc(*ModuleEnv, compiled_module_envs.len);
    for (compiled_module_envs, 0..) |env, i| {
        mutable_envs[i] = env;
    }
    lambda_inference.inferAll(mutable_envs) catch {
        std.log.err("Lambda set inference failed", .{});
        return error.LambdaInferenceFailed;
    };

    // Run closure transformer (defunctionalization)
    for (mutable_envs) |module| {
        if (!module.is_defunctionalized) {
            var transformer = can.ClosureTransformer.initWithInference(ctx.gpa, module, &lambda_inference);
            defer transformer.deinit();
            module.is_defunctionalized = true;
        }
    }

    // Process hosted functions - assign global indices and build lookup map for lowering
    var hosted_functions = BuildEnv.ResolvedModules.HostedFunctionMap.init(ctx.gpa);
    defer hosted_functions.deinit();
    try resolved.processHostedFunctions(ctx.gpa, &hosted_functions);

    // Create layout store
    std.log.debug("Creating layout store...", .{});
    const builtin_str = if (all_module_envs.len > 0)
        all_module_envs[0].idents.builtin_str
    else
        null;

    var layout_store = layout.Store.init(all_module_envs, builtin_str, ctx.gpa, base.target.TargetUsize.native) catch {
        std.log.err("Failed to create layout store", .{});
        return error.LayoutStoreFailed;
    };
    defer layout_store.deinit();

    const app_module_idx = plat.app_module_idx;
    std.log.debug("App module index: {?}", .{app_module_idx});

    // Create Mono IR store
    std.log.debug("Creating Mono IR store and lowering expressions...", .{});
    var mono_store = mono.MonoExprStore.init(ctx.gpa);
    defer mono_store.deinit();

    // Create lowerer with hosted function map
    var lowerer = mono.Lower.init(ctx.gpa, &mono_store, all_module_envs, &lambda_inference, &layout_store, app_module_idx, &hosted_functions);
    defer lowerer.deinit();

    // Find and lower entrypoint expressions from platform module
    // The platform's provides clause maps Roc identifiers to FFI symbols
    // e.g., provides { main_for_host!: "main" } means we look for main_for_host! in platform
    var entrypoints = try std.ArrayList(backend.Entrypoint).initCapacity(ctx.gpa, provides_entries.len);
    defer entrypoints.deinit(ctx.gpa);

    const platform_defs = platform_module.env.store.sliceDefs(platform_module.env.all_defs);

    for (provides_entries) |entry| {
        // Find declaration matching the Roc identifier in the platform module's defs
        var found_expr: ?can.CIR.Expr.Idx = null;
        for (platform_defs) |def_idx| {
            const def = platform_module.env.store.getDef(def_idx);
            const pattern = platform_module.env.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => |assign| {
                    const ident_name = platform_module.env.getIdent(assign.ident);
                    if (std.mem.eql(u8, ident_name, entry.roc_ident)) {
                        found_expr = def.expr;
                        break;
                    }
                },
                else => {},
            }
        }

        if (found_expr) |expr_idx| {
            // Lower the expression from the platform module
            const mono_expr_id = lowerer.lowerExpr(@intCast(platform_idx + 1), expr_idx) catch |err| {
                std.log.err("Failed to lower expression for entrypoint {s} ({s}): {}", .{ entry.roc_ident, entry.ffi_symbol, err });
                continue;
            };

            // Get layout for the expression
            const type_var = can.ModuleEnv.varFrom(expr_idx);
            var type_scope = @import("types").TypeScope.init(ctx.gpa);
            defer type_scope.deinit();
            const ret_layout = layout_store.fromTypeVar(@intCast(platform_idx + 1), type_var, &type_scope, null) catch {
                std.log.err("Failed to get layout for entrypoint {s}", .{entry.roc_ident});
                continue;
            };

            try entrypoints.append(ctx.gpa, .{
                .symbol_name = try std.fmt.allocPrint(ctx.arena, "roc__{s}", .{entry.ffi_symbol}),
                .body_expr = mono_expr_id,
                .arg_layouts = &[_]layout.Idx{}, // Top-level constants have no args
                .ret_layout = ret_layout,
            });

            std.log.debug("Found entrypoint: {s} -> roc__{s}", .{ entry.roc_ident, entry.ffi_symbol });
        } else {
            std.log.warn("Entrypoint '{s}' not found in platform module", .{entry.roc_ident});
        }
    }

    if (entrypoints.items.len == 0) {
        std.log.err("No entrypoints could be lowered", .{});
        return error.NoEntrypointsLowered;
    }

    // Run RC insertion pass
    var rc_pass = try mono.RcInsert.RcInsertPass.init(ctx.gpa, &mono_store, &layout_store);
    defer rc_pass.deinit();

    for (entrypoints.items) |*ep| {
        ep.body_expr = rc_pass.insertRcOps(ep.body_expr) catch ep.body_expr;
    }

    // Get procedures from the mono store
    const procs = mono_store.getProcs();

    // Compile to object file
    std.log.debug("Generating native code...", .{});
    var object_compiler = backend.ObjectFileCompiler.init(ctx.gpa);

    const obj_filename = try std.fmt.allocPrint(ctx.arena, "roc_app_{s}.o", .{@tagName(target)});
    const obj_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, obj_filename });

    object_compiler.compileToObjectFileAndWrite(
        &mono_store,
        &layout_store,
        entrypoints.items,
        procs,
        target,
        obj_path,
    ) catch |err| {
        std.log.err("Native compilation failed: {}", .{err});
        return error.NativeCompilationFailed;
    };

    std.log.debug("Object file generated: {s}", .{obj_path});

    // If --no-link, we're done
    if (args.no_link) {
        const stdout = ctx.io.stdout();
        try stdout.print("Object file generated: {s}\n", .{obj_path});
        return;
    }

    // Get link spec and build file lists
    const target_name = @tagName(target);
    const link_spec = targets_config.getLinkSpec(target, link_type) orelse {
        return ctx.fail(.{ .linker_failed = .{
            .err = error.UnsupportedTarget,
            .target = target_name,
        } });
    };

    const files_dir = targets_config.files_dir orelse "targets";
    var platform_files_pre = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    var platform_files_post = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    var hit_app = false;

    for (link_spec.items) |item| {
        switch (item) {
            .file_path => |path| {
                const full_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir, target_name, path });

                std.fs.cwd().access(full_path, .{}) catch {
                    const result = platform_validation.targets_validator.ValidationResult{
                        .missing_target_file = .{
                            .target = target,
                            .link_type = link_type,
                            .file_path = path,
                            .expected_full_path = full_path,
                        },
                    };
                    _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.MissingTargetFile;
                };

                if (!hit_app) {
                    try platform_files_pre.append(full_path);
                } else {
                    try platform_files_post.append(full_path);
                }
            },
            .app => {
                hit_app = true;
            },
            .win_gui => {},
        }
    }

    // Extract builtins object file for the target and add to link inputs
    const builtins_bytes = BuiltinsObjects.forTarget(target);
    const builtins_filename = BuiltinsObjects.filename(target);
    const builtins_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, builtins_filename });

    // Write builtins object to cache if not already there
    std.fs.cwd().writeFile(.{
        .sub_path = builtins_path,
        .data = builtins_bytes,
    }) catch |err| {
        std.log.err("Failed to write builtins object file: {}", .{err});
        return error.BuiltinsExtractionFailed;
    };
    std.log.debug("Builtins object file: {s}", .{builtins_path});

    // Link the object file with platform files
    var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);
    try object_files.append(obj_path);
    try object_files.append(builtins_path);

    std.log.debug("Linking: {} pre-files, {} object files, {} post-files", .{
        platform_files_pre.items.len,
        object_files.items.len,
        platform_files_post.items.len,
    });

    linker.link(ctx, .{
        .target_format = linker.TargetFormat.detectFromOs(target_os),
        .target_abi = linker.TargetAbi.fromRocTarget(target),
        .target_os = target_os,
        .target_arch = target_arch,
        .output_path = final_output_path,
        .object_files = object_files.items,
        .platform_files_pre = platform_files_pre.items,
        .platform_files_post = platform_files_post.items,
        .extra_args = &.{},
    }) catch |err| {
        return ctx.fail(.{ .linker_failed = .{
            .err = err,
            .target = target_name,
        } });
    };

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    // Get cache statistics for verbose output
    const cache_stats = build_env.getCacheStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    const stdout = ctx.io.stdout();
    try stdout.print("Built {s} in {d:.1}ms", .{ final_output_path, elapsed_ms });
    if (cache_stats.modules_total > 0 and cache_stats.cache_hits > 0) {
        try stdout.print(" with {}% cache hit", .{cache_percent});
    }
    try stdout.writeAll(" (dev backend)\n");

    // Print verbose stats if requested
    if (args.verbose) {
        try stdout.print("\n    Modules: {} total, {} cached, {} built\n", .{
            cache_stats.modules_total,
            cache_stats.cache_hits,
            cache_stats.modules_compiled,
        });
        try stdout.print("    Cache Hit: {}%\n", .{cache_percent});
    }

    if (diag.warnings > 0) {
        try stdout.print("  {} warning(s)\n", .{diag.warnings});
    }
}

/// Build a standalone binary with the interpreter and embedded module data.
/// This is the primary build path that creates executables or libraries without requiring IPC.
fn rocBuildEmbedded(ctx: *CliContext, args: cli_args.BuildArgs) !void {
    const target_mod = @import("target.zig");

    var timer = try std.time.Timer.start();

    std.log.info("Building {s} with embedded interpreter", .{args.path});

    // Determine output path
    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else blk: {
        break :blk try base.module_path.getModuleNameAlloc(ctx.arena, args.path);
    };

    // Set up cache directory for build artifacts
    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(ctx.arena);
    const build_cache_dir = try std.fs.path.join(ctx.arena, &.{ cache_dir, "roc_build" });

    std.fs.cwd().makePath(build_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Phase 1: Create BuildEnv with native target for discovery
    const thread_count: usize = if (args.max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: compile.package.Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative());
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager for compilation caching
    if (!args.no_cache) {
        const build_cache_manager = try ctx.gpa.create(CacheManager);
        build_cache_manager.* = CacheManager.init(ctx.gpa, .{
            .enabled = true,
            .verbose = args.verbose,
        }, Filesystem.default());
        build_env.setCacheManager(build_cache_manager);
    }

    // Phase 2: Discover dependencies (parses headers once, extracts TargetsConfig)
    build_env.discoverDependencies(args.path) catch |err| {
        _ = build_env.renderDiagnostics(ctx.io.stderr());
        return err;
    };

    // Phase 3: Get TargetsConfig from the discovered platform package
    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };

    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |ps| (std.fs.path.dirname(ps) orelse ".") else ".";

    // Phase 4: Select target and link type
    // If --target is provided, use that; otherwise find the first compatible target
    const target: target_mod.RocTarget, const link_type: target_mod.LinkType = if (args.target) |target_str| blk: {
        const parsed_target = target_mod.RocTarget.fromString(target_str) orelse {
            const result = platform_validation.targets_validator.ValidationResult{
                .invalid_target = .{ .target_str = target_str },
            };
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.InvalidTarget;
        };

        // Find which link type supports this target (prefer exe > static_lib > shared_lib)
        const lt: target_mod.LinkType = if (targets_config.supportsTarget(parsed_target, .exe))
            .exe
        else if (targets_config.supportsTarget(parsed_target, .static_lib))
            .static_lib
        else if (targets_config.supportsTarget(parsed_target, .shared_lib))
            .shared_lib
        else {
            const result = platform_validation.createUnsupportedTargetResult(
                platform_source orelse "<unknown>",
                parsed_target,
                .exe, // Show exe as the expected type for error message
                targets_config,
            );
            _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.UnsupportedTarget;
        };

        break :blk .{ parsed_target, lt };
    } else blk: {
        // No --target provided: find the first compatible target across all link types
        const compatible = targets_config.getFirstCompatibleTarget() orelse {
            renderProblem(ctx.gpa, ctx.io.stderr(), .{
                .platform_validation_failed = .{
                    .message = "No compatible target found. The platform does not support any target compatible with this system.",
                },
            });
            return error.UnsupportedTarget;
        };
        break :blk .{ compatible.target, compatible.link_type };
    };

    std.log.debug("Target: {s}, Link type: {s}", .{ @tagName(target), @tagName(link_type) });

    // Add appropriate file extension based on target and link type
    const final_output_path = if (args.output != null)
        output_path // User specified output, use as-is
    else blk: {
        // Auto-determine extension based on target
        const ext = if (target == .wasm32)
            ".wasm"
        else if (target.isWindows())
            if (link_type == .exe) ".exe" else if (link_type == .shared_lib) ".dll" else ".lib"
        else if (target.isMacOS())
            if (link_type == .shared_lib) ".dylib" else if (link_type == .static_lib) ".a" else ""
        else if (link_type == .shared_lib) ".so" else if (link_type == .static_lib) ".a" else "";

        if (ext.len > 0) {
            break :blk try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{ output_path, ext });
        } else {
            break :blk output_path;
        }
    };

    // Check for unsupported cross-compilation scenarios
    const host_os = builtin.target.os.tag;
    const host_ptr_width = @bitSizeOf(usize);

    // Always use portable serialization for roc build (embedded mode)
    // The IPC format relies on shared memory alignment guarantees that don't apply
    // when data is embedded in a binary at arbitrary addresses
    const target_ptr_width = target.ptrBitWidth();

    // Phase 5: Set target and compile
    std.log.debug("Compiling Roc file: {s}", .{args.path});
    std.log.debug("Using portable serialization ({d}-bit host -> {d}-bit target)", .{ host_ptr_width, target_ptr_width });

    build_env.setTarget(target);

    build_env.compileDiscovered() catch |err| {
        _ = build_env.renderDiagnostics(ctx.io.stderr());
        return err;
    };

    const embedded_diag = build_env.renderDiagnostics(ctx.io.stderr());
    if (embedded_diag.errors > 0 and !args.allow_errors) {
        return error.CompilationFailed;
    }

    // Get compiled modules in serialization order
    const modules = build_env.getModulesInSerializationOrder(ctx.arena) catch |err| {
        std.log.err("Failed to get compiled modules: {}", .{err});
        return err;
    };

    if (modules.len == 0) {
        std.log.err("No modules were compiled", .{});
        return error.NoModulesCompiled;
    }

    // Find primary and app module indices
    const primary_idx = BuildEnv.findPrimaryModuleIndex(modules) orelse 0;
    const app_idx = BuildEnv.findAppModuleIndex(modules) orelse modules.len - 1;

    // Serialize modules
    const compile_result = serialize_modules.serializeModules(
        ctx.arena,
        modules,
        primary_idx,
        app_idx,
    ) catch |err| {
        std.log.err("Failed to serialize modules: {}", .{err});
        return err;
    };

    std.log.debug("Portable serialization complete, {} bytes, {} modules", .{ compile_result.bytes.len, modules.len });

    const serialized_module = compile_result.bytes;

    // glibc targets require a full libc for linking, which is only available on Linux hosts
    if (target.isDynamic() and host_os != .linux) {
        const result = platform_validation.targets_validator.ValidationResult{
            .unsupported_glibc_cross = .{
                .target = target,
                .host_os = @tagName(host_os),
            },
        };
        _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
        return error.UnsupportedCrossCompilation;
    }

    // Get the link spec for this target - tells us exactly what files to link
    const link_spec = targets_config.getLinkSpec(target, link_type) orelse {
        return ctx.fail(.{ .linker_failed = .{
            .err = error.UnsupportedTarget,
            .target = @tagName(target),
        } });
    };

    // Build link file lists from the link spec
    // Files before 'app' go in pre, files after 'app' go in post
    const target_name = @tagName(target);
    const files_dir = targets_config.files_dir orelse "targets";
    var platform_files_pre = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    var platform_files_post = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    var hit_app = false;

    for (link_spec.items) |item| {
        switch (item) {
            .file_path => |path| {
                // Build full path: platform_dir/files_dir/target_name/path
                const full_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir, target_name, path });

                // Validate the file exists
                std.fs.cwd().access(full_path, .{}) catch {
                    const result = platform_validation.targets_validator.ValidationResult{
                        .missing_target_file = .{
                            .target = target,
                            .link_type = link_type,
                            .file_path = path,
                            .expected_full_path = full_path,
                        },
                    };
                    _ = platform_validation.renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.MissingTargetFile;
                };

                if (!hit_app) {
                    try platform_files_pre.append(full_path);
                } else {
                    try platform_files_post.append(full_path);
                }
            },
            .app => {
                hit_app = true;
            },
            .win_gui => {
                // Windows subsystem flag - will be handled by linker
            },
        }
    }

    std.log.debug("Link spec: {} files before app, {} files after app", .{ platform_files_pre.items.len, platform_files_post.items.len });

    // Extract entrypoints from the platform source file
    std.log.debug("Extracting entrypoints from platform...", .{});
    var entrypoints = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 16) catch {
        return error.OutOfMemory;
    };

    extractEntrypointsFromPlatform(ctx, platform_source.?, &entrypoints) catch |err| {
        return ctx.fail(.{ .entrypoint_extraction_failed = .{
            .path = platform_source.?,
            .reason = @errorName(err),
        } });
    };
    std.log.debug("Found {} entrypoints", .{entrypoints.items.len});

    // Link everything together
    // object_files = the Roc application files
    // platform_files_pre/post = files declared in link spec before/after 'app'
    var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);

    // Extract shim library (interpreter shim) - now works for both native and wasm32 targets
    // Include target name in filename to support different targets in the same cache
    const shim_filename = try std.fmt.allocPrint(ctx.arena, "libroc_shim_{s}.a", .{target_name});
    const shim_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, shim_filename });

    std.fs.cwd().access(shim_path, .{}) catch {
        // Shim not found, extract it
        // For roc build, use the target-specific shim for cross-compilation support
        std.log.debug("Extracting shim library for target {s} to {s}...", .{ target_name, shim_path });
        extractReadRocFilePathShimLibrary(ctx, shim_path, target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };
    };

    // Generate platform host shim with embedded module data
    // The shim provides roc__<entrypoint> functions and embeds serialized bytecode
    const enable_debug = args.debug or (builtin.mode == .Debug);
    std.log.debug("Generating platform host shim with {} bytes of embedded data (debug={})...", .{ serialized_module.len, enable_debug });
    const platform_shim_path = try generatePlatformHostShim(ctx, build_cache_dir, entrypoints.items, target, serialized_module, enable_debug);
    std.log.debug("Platform shim generated: {?s}", .{platform_shim_path});

    try object_files.append(shim_path);
    if (platform_shim_path) |psp| {
        try object_files.append(psp);
    }

    // Extra linker args for system libraries (not platform-provided)
    var extra_args = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    if (target.isMacOS()) {
        // macOS requires linking with system libraries
        try extra_args.append("-lSystem");
    }

    const linker_mod = @import("linker.zig");
    const target_abi = if (target.isStatic()) linker_mod.TargetAbi.musl else linker_mod.TargetAbi.gnu;

    // Build full path to platform files directory for sysroot lookup
    const platform_files_dir = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir });

    const link_config = linker_mod.LinkConfig{
        .target_format = linker_mod.TargetFormat.detectFromOs(target.toOsTag()),
        .object_files = object_files.items,
        .platform_files_pre = platform_files_pre.items,
        .platform_files_post = platform_files_post.items,
        .extra_args = extra_args.items,
        .output_path = final_output_path,
        .target_abi = target_abi,
        .target_os = target.toOsTag(),
        .target_arch = target.toCpuArch(),
        .wasm_initial_memory = args.wasm_memory orelse linker_mod.DEFAULT_WASM_INITIAL_MEMORY,
        .wasm_stack_size = args.wasm_stack_size orelse linker_mod.DEFAULT_WASM_STACK_SIZE,
        .platform_files_dir = platform_files_dir,
    };

    // Dump linker inputs to temp directory if requested
    if (args.z_dump_linker) {
        try dumpLinkerInputs(ctx, link_config);
    }

    try linker_mod.link(ctx, link_config);

    // Print success message to stdout
    // Add "./" prefix for relative paths to make it clear it's in the current directory
    const display_path = if (std.fs.path.isAbsolute(final_output_path) or
        std.mem.startsWith(u8, final_output_path, "./") or
        std.mem.startsWith(u8, final_output_path, "../"))
        final_output_path
    else
        try std.fmt.allocPrint(ctx.arena, "./{s}", .{final_output_path});

    // Get cache stats for summary
    const cache_stats = build_env.getCacheStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    const elapsed = timer.read();
    const stdout = ctx.io.stdout();

    // Print success with timing and cache info
    try stdout.print("Successfully built {s} in ", .{display_path});
    try formatElapsedTimeMs(stdout, elapsed);
    if (cache_stats.modules_total > 0 and cache_stats.cache_hits > 0) {
        try stdout.print(" with {}% cache hit\n", .{cache_percent});
    } else {
        try stdout.writeAll("\n");
    }

    // Print verbose stats if requested
    if (args.verbose) {
        try stdout.print("\n    Modules: {} total, {} cached, {} built\n", .{
            cache_stats.modules_total,
            cache_stats.cache_hits,
            cache_stats.modules_compiled,
        });
        try stdout.print("    Cache Hit: {}%\n", .{cache_percent});
    }

    // Exit with code 2 if there were warnings (but no errors)
    if (embedded_diag.warnings > 0) {
        ctx.io.flush();
        std.process.exit(2);
    }
}

/// Dump linker inputs to a temp directory for debugging linking issues.
/// Creates a directory with all input files copied and a README with the linker command.
fn dumpLinkerInputs(ctx: *CliContext, link_config: linker.LinkConfig) !void {
    const stderr = ctx.io.stderr();

    // Create temp directory with unique name based on timestamp
    const timestamp = std.time.timestamp();
    const dir_name = try std.fmt.allocPrint(ctx.arena, "roc-linker-debug-{d}", .{timestamp});
    const dump_dir = try std.fs.path.join(ctx.arena, &.{ "/tmp", dir_name });

    std.fs.cwd().makePath(dump_dir) catch |err| {
        try stderr.print("Failed to create debug dump directory '{s}': {}\n", .{ dump_dir, err });
        return err;
    };

    // Track copied files for the README
    var copied_files = try std.array_list.Managed(CopiedFile).initCapacity(ctx.arena, 16);

    // Copy platform_files_pre
    for (link_config.platform_files_pre, 0..) |src, i| {
        const basename = std.fs.path.basename(src);
        const dest_name = try std.fmt.allocPrint(ctx.arena, "pre_{d}_{s}", .{ i, basename });
        const dest_path = try std.fs.path.join(ctx.arena, &.{ dump_dir, dest_name });
        std.fs.cwd().copyFile(src, std.fs.cwd(), dest_path, .{}) catch |err| {
            try stderr.print("Warning: Failed to copy '{s}': {}\n", .{ src, err });
            continue;
        };
        try copied_files.append(.{ .name = dest_name, .original = src, .category = "platform (pre-link)" });
    }

    // Copy object_files
    for (link_config.object_files, 0..) |src, i| {
        const basename = std.fs.path.basename(src);
        const dest_name = try std.fmt.allocPrint(ctx.arena, "obj_{d}_{s}", .{ i, basename });
        const dest_path = try std.fs.path.join(ctx.arena, &.{ dump_dir, dest_name });
        std.fs.cwd().copyFile(src, std.fs.cwd(), dest_path, .{}) catch |err| {
            try stderr.print("Warning: Failed to copy '{s}': {}\n", .{ src, err });
            continue;
        };
        try copied_files.append(.{ .name = dest_name, .original = src, .category = "object file" });
    }

    // Copy platform_files_post
    for (link_config.platform_files_post, 0..) |src, i| {
        const basename = std.fs.path.basename(src);
        const dest_name = try std.fmt.allocPrint(ctx.arena, "post_{d}_{s}", .{ i, basename });
        const dest_path = try std.fs.path.join(ctx.arena, &.{ dump_dir, dest_name });
        std.fs.cwd().copyFile(src, std.fs.cwd(), dest_path, .{}) catch |err| {
            try stderr.print("Warning: Failed to copy '{s}': {}\n", .{ src, err });
            continue;
        };
        try copied_files.append(.{ .name = dest_name, .original = src, .category = "platform (post-link)" });
    }

    // Generate the linker command string
    const link_cmd = linker.formatLinkCommand(ctx, link_config) catch |err| {
        try stderr.print("Warning: Failed to format linker command: {}\n", .{err});
        return;
    };

    // Build the file list for README
    var file_list = std.array_list.Managed(u8).init(ctx.arena);
    for (copied_files.items) |file| {
        try file_list.writer().print("  {s}\n    <- {s} ({s})\n", .{ file.name, file.original, file.category });
    }

    // Write README.txt with instructions
    const readme_content = try std.fmt.allocPrint(ctx.arena,
        \\Roc Linker Debug Dump
        \\=====================
        \\
        \\Target format: {s}
        \\Target OS: {s}
        \\Target arch: {s}
        \\Output: {s}
        \\
        \\Files ({d} copied):
        \\{s}
        \\
        \\To manually reproduce the link step:
        \\
        \\  {s}
        \\
        \\Note: The command above uses original file paths. The copied files
        \\in this directory preserve original filenames for inspection.
        \\
    , .{
        @tagName(link_config.target_format),
        if (link_config.target_os) |os| @tagName(os) else "native",
        if (link_config.target_arch) |arch| @tagName(arch) else "native",
        link_config.output_path,
        copied_files.items.len,
        file_list.items,
        link_cmd,
    });

    const readme_path = try std.fs.path.join(ctx.arena, &.{ dump_dir, "README.txt" });
    const readme_file = std.fs.cwd().createFile(readme_path, .{}) catch |err| {
        try stderr.print("Warning: Failed to create README.txt: {}\n", .{err});
        return;
    };
    defer readme_file.close();
    readme_file.writeAll(readme_content) catch |err| {
        try stderr.print("Warning: Failed to write README.txt: {}\n", .{err});
    };

    // Print summary to stderr
    try stderr.print(
        \\
        \\=== Linker debug dump ===
        \\Directory: {s}
        \\Files: {d} copied
        \\
        \\To reproduce:
        \\  {s}
        \\
        \\See {s}/README.txt for details
        \\=========================
        \\
    , .{ dump_dir, copied_files.items.len, link_cmd, dump_dir });
}

const CopiedFile = struct {
    name: []const u8,
    original: []const u8,
    category: []const u8,
};

// Test cache blob format
// Binary format for caching test results.

const TestCacheHeader = extern struct {
    magic: u32 = 0x524F4354, // "ROCT"
    version: u32 = 1,
    outcome: u32, // 0=all_passed, 1=some_failed, 2=compilation_error
    passed_count: u32,
    failed_count: u32,
    num_results: u32,
    comptime_report_len: u32,
    _reserved: u32 = 0,

    comptime {
        std.debug.assert(@sizeOf(TestCacheHeader) == 32);
    }
};

const TestCacheResultEntry = extern struct {
    passed: u8,
    _pad: [3]u8 = .{ 0, 0, 0 },
    region_start: u32,
    region_end: u32,
    report_len: u32,

    comptime {
        std.debug.assert(@sizeOf(TestCacheResultEntry) == 16);
    }
};

const TestCacheOutcome = enum(u32) {
    all_passed = 0,
    some_failed = 1,
    compilation_error = 2,
};

fn parseTestCacheHeader(data: []const u8) ?*const TestCacheHeader {
    if (data.len < @sizeOf(TestCacheHeader)) return null;
    const header: *const TestCacheHeader = @ptrCast(@alignCast(data.ptr));
    if (header.magic != 0x524F4354) return null;
    if (header.version != 1) return null;
    return header;
}

fn buildTestCacheBlob(
    allocator: std.mem.Allocator,
    outcome: TestCacheOutcome,
    passed_count: u32,
    failed_count: u32,
    results: []const TestCacheResultEntry,
    failure_reports: []const []const u8,
    comptime_report: []const u8,
) ![]u8 {
    // Calculate total size
    var total_size: usize = @sizeOf(TestCacheHeader);

    if (outcome == .compilation_error) {
        total_size += comptime_report.len;
    } else {
        total_size += results.len * @sizeOf(TestCacheResultEntry);
        for (failure_reports) |report| {
            total_size += report.len;
        }
        total_size += comptime_report.len;
    }

    var buf = try allocator.alloc(u8, total_size);
    var offset: usize = 0;

    // Write header
    const header = TestCacheHeader{
        .outcome = @intFromEnum(outcome),
        .passed_count = passed_count,
        .failed_count = failed_count,
        .num_results = @intCast(results.len),
        .comptime_report_len = @intCast(comptime_report.len),
    };
    const header_bytes = std.mem.asBytes(&header);
    @memcpy(buf[offset..][0..header_bytes.len], header_bytes);
    offset += header_bytes.len;

    if (outcome == .compilation_error) {
        @memcpy(buf[offset..][0..comptime_report.len], comptime_report);
        offset += comptime_report.len;
    } else {
        // Write result entries
        for (results) |entry| {
            const entry_bytes = std.mem.asBytes(&entry);
            @memcpy(buf[offset..][0..entry_bytes.len], entry_bytes);
            offset += entry_bytes.len;
        }
        // Write failure report data
        for (failure_reports) |report| {
            @memcpy(buf[offset..][0..report.len], report);
            offset += report.len;
        }
        // Write comptime report
        @memcpy(buf[offset..][0..comptime_report.len], comptime_report);
        offset += comptime_report.len;
    }

    std.debug.assert(offset == total_size);
    return buf;
}

fn replayTestCache(
    gpa: std.mem.Allocator,
    data: []const u8,
    args: cli_args.TestArgs,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    source: []const u8,
    start_time: i128,
) !void {
    const header = parseTestCacheHeader(data) orelse return error.InvalidCacheData;
    const outcome: TestCacheOutcome = @enumFromInt(header.outcome);

    // Calculate elapsed time
    const end_time = std.time.nanoTimestamp();
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    if (outcome == .compilation_error) {
        // Print cached error message
        const error_start = @sizeOf(TestCacheHeader);
        if (data.len < error_start + header.comptime_report_len) return error.InvalidCacheData;
        const error_msg = data[error_start..][0..header.comptime_report_len];
        try stderr.writeAll(error_msg);
        return error.TestsFailed;
    }

    // Parse result entries
    const entries_start = @sizeOf(TestCacheHeader);
    const entries_size = @as(usize, header.num_results) * @sizeOf(TestCacheResultEntry);
    if (data.len < entries_start + entries_size + header.comptime_report_len) return error.InvalidCacheData;

    const entries_bytes = data[entries_start..][0..entries_size];
    const entries: []const TestCacheResultEntry = @as([*]const TestCacheResultEntry, @ptrCast(@alignCast(entries_bytes.ptr)))[0..header.num_results];

    // Calculate where failure reports start
    const failure_data_start = entries_start + entries_size;

    // Print compile-time crash reports from cache
    const comptime_report_start = data.len - header.comptime_report_len;
    if (header.comptime_report_len > 0) {
        const comptime_report = data[comptime_report_start..][0..header.comptime_report_len];
        try stderr.writeAll(comptime_report);
    }

    const has_comptime_crashes = header.comptime_report_len > 0;
    const passed = header.passed_count;
    const failed = header.failed_count;

    // Create minimal ModuleEnv just for line number computation
    var env = can.ModuleEnv.init(gpa, source) catch return error.InvalidCacheData;
    defer env.deinit();
    env.common.source = source;
    env.common.calcLineStarts(gpa) catch return error.InvalidCacheData;

    if (failed == 0 and !has_comptime_crashes) {
        try stdout.print("All ({}) tests passed in {d:.1} ms. (cached)\n", .{ passed, elapsed_ms });
        if (args.verbose) {
            for (entries) |entry| {
                const region = base.Region.from_raw_offsets(entry.region_start, entry.region_end);
                const region_info = env.calcRegionInfo(region);
                try stdout.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
            }
        }
        return; // Exit with 0
    } else {
        const total_tests = passed + failed;
        if (total_tests > 0) {
            try stderr.print("Ran {} test(s): {} passed, {} failed in {d:.1}ms (cached)\n", .{ total_tests, passed, failed, elapsed_ms });
        }

        if (args.verbose) {
            var report_offset = failure_data_start;
            for (entries) |entry| {
                const region = base.Region.from_raw_offsets(entry.region_start, entry.region_end);
                const region_info = env.calcRegionInfo(region);
                if (entry.passed != 0) {
                    try stdout.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
                } else {
                    // Print cached failure report
                    if (entry.report_len > 0 and report_offset + entry.report_len <= comptime_report_start) {
                        const report_text = data[report_offset..][0..entry.report_len];
                        try stderr.writeAll(report_text);
                    } else {
                        try stderr.print("\x1b[31mFAIL\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
                    }
                }
                if (entry.passed == 0) {
                    report_offset += entry.report_len;
                }
            }
        } else {
            for (entries) |entry| {
                if (entry.passed == 0) {
                    const region = base.Region.from_raw_offsets(entry.region_start, entry.region_end);
                    const region_info = env.calcRegionInfo(region);
                    try stderr.print("\x1b[31mFAIL\x1b[0m: {s}:{}\n", .{ args.path, region_info.start_line_idx + 1 });
                }
            }
        }

        return error.TestsFailed;
    }
}

fn rocTest(ctx: *CliContext, args: cli_args.TestArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Start timing
    const start_time = std.time.nanoTimestamp();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    // --- Test cache check (before any compilation) ---
    // Read source to compute cache key for test result caching
    const source: ?[]const u8 = if (!args.no_cache)
        (std.fs.cwd().readFileAlloc(ctx.gpa, args.path, std.math.maxInt(usize)) catch null)
    else
        null;
    defer if (source) |s| ctx.gpa.free(s);

    if (source) |src| {
        {
            const cache_key = CacheManager.generateCacheKey(src, build_options.compiler_version);
            const test_cache_dir = cache_config.getTestCacheDir(ctx.gpa) catch null;
            if (test_cache_dir) |dir| {
                defer ctx.gpa.free(dir);
                var test_cache_manager = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
                if (test_cache_manager.loadRawBytes(cache_key, dir)) |cached_data| {
                    defer ctx.gpa.free(cached_data);
                    replayTestCache(ctx.gpa, cached_data, args, stdout, stderr, src, start_time) catch |err| switch (err) {
                        error.TestsFailed => return err,
                        else => {}, // On invalid cache data, fall through to normal path
                    };
                    return;
                }
            }
        }
    }

    // --- Normal compilation path (cache miss) ---

    // Determine threading mode and thread count
    const thread_count: usize = if (args.max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    // Initialize BuildEnv for compilation
    var build_env = BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative()) catch |err| {
        try stderr.print("Failed to initialize build environment: {}\n", .{err});
        return err;
    };
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = ctx.gpa.create(CacheManager) catch |err| {
            try stderr.print("Failed to create cache manager: {}\n", .{err});
            return err;
        };
        cache_manager.* = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
        build_env.setCacheManager(cache_manager);
    }

    // Build the file using the Coordinator (handles all module types)
    build_env.build(args.path) catch |err| {
        // On build error, drain and display any reports
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.freeDrainedReports(drained);
        for (drained) |mod| {
            for (mod.reports) |*report| {
                const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
                const config = reporting.ReportingConfig.initColorTerminal();
                reporting.renderReportToTerminal(report, stderr, palette, config) catch {};
            }
        }
        try stderr.print("Build failed: {}\n", .{err});
        return err;
    };

    // Determine package name - could be "app" or "module" depending on header type
    // After build, the scheduler contains the compiled modules (coordinator's envs are transferred)
    const pkg_name = if (build_env.schedulers.get("app") != null) "app" else "module";
    const root_scheduler = build_env.schedulers.get(pkg_name) orelse {
        try stderr.print("Internal error: Scheduler '{s}' not found after build\n", .{pkg_name});
        return error.InternalError;
    };

    // Get root module from the scheduler (where envs live after transfer)
    const root_mod = root_scheduler.getRootModule() orelse {
        try stderr.print("Internal error: No root module in scheduler\n", .{});
        return error.InternalError;
    };
    // Note: In PackageEnv, the env is stored inline (not as a pointer), so we take a pointer to it
    const root_env: *const ModuleEnv = if (root_mod.env) |*env| env else {
        try stderr.print("Internal error: Root module has no environment\n", .{});
        return error.InternalError;
    };

    // Drain any compilation reports first
    const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
    defer build_env.freeDrainedReports(drained);

    var has_compilation_errors = false;
    for (drained) |mod| {
        for (mod.reports) |*report| {
            const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
            const config = reporting.ReportingConfig.initColorTerminal();
            reporting.renderReportToTerminal(report, stderr, palette, config) catch {};
            if (report.severity == .fatal or report.severity == .runtime_error) {
                has_compilation_errors = true;
            }
        }
    }

    // Collect all module environments for the interpreter
    // This includes all modules from all packages (imports from other modules)
    var other_modules_list = std.array_list.Managed(*const ModuleEnv).init(ctx.gpa);
    defer other_modules_list.deinit();

    // Add builtin module first
    try other_modules_list.append(build_env.builtin_modules.builtin_module.env);

    // Iterate through all schedulers and collect module envs
    var sched_iter = build_env.schedulers.iterator();
    while (sched_iter.next()) |sched_entry| {
        const scheduler = sched_entry.value_ptr.*;
        for (scheduler.modules.items) |*mod| {
            if (mod.env) |*env| {
                // Don't add the root module to other_modules (it's handled separately)
                if (env != root_env) {
                    try other_modules_list.append(env);
                }
            }
        }
    }

    const other_modules = other_modules_list.items;

    // Get builtin types from BuildEnv's builtin modules
    const builtin_types = build_env.builtin_modules.asBuiltinTypes();
    const builtin_module_env = build_env.builtin_modules.builtin_module.env;
    const builtin_indices = build_env.builtin_modules.builtin_indices;

    // Create import mapping for the root module
    // This combines builtin mappings with user import mappings from canonicalization
    var import_mapping = Check.createImportMapping(
        ctx.gpa,
        @constCast(root_env).getIdentStore(),
        root_env,
        builtin_module_env,
        builtin_indices,
        null,
    ) catch |err| {
        try stderr.print("Failed to create import mapping: {}\n", .{err});
        return err;
    };
    defer import_mapping.deinit();

    // Create a problem store for comptime evaluation
    var problems = check.problem.Store.init(ctx.gpa) catch |err| {
        try stderr.print("Failed to create problem store: {}\n", .{err});
        return err;
    };
    defer problems.deinit(ctx.gpa);

    // Evaluate all top-level declarations at compile time
    var comptime_evaluator = eval.ComptimeEvaluator.init(
        ctx.gpa,
        @constCast(root_env),
        other_modules,
        &problems,
        builtin_types,
        builtin_module_env,
        &import_mapping,
        RocTarget.detectNative(),
    ) catch |err| {
        try stderr.print("Failed to create compile-time evaluator: {}\n", .{err});
        return err;
    };

    // Only run evalAll if evaluation_order is set (not cached modules)
    // Cached modules have evaluation_order = null since it's not serialized
    if (root_env.evaluation_order != null) {
        _ = comptime_evaluator.evalAll() catch |err| {
            try stderr.print("Failed to evaluate declarations: {}\n", .{err});
            comptime_evaluator.deinit();
            return err;
        };
    }

    // Track test results across all modules
    var total_passed: u32 = 0;
    var total_failed: u32 = 0;

    // Structure to track test results per module for reporting
    const TestResultItem = struct {
        passed: bool,
        region: base.Region,
        error_msg: ?[]const u8,
    };
    const ModuleTestResult = struct {
        env: *const ModuleEnv,
        path: []const u8,
        results: []const TestResultItem,
    };

    var module_results = std.array_list.Managed(ModuleTestResult).init(ctx.gpa);
    defer {
        for (module_results.items) |mr| {
            ctx.gpa.free(mr.results);
        }
        module_results.deinit();
    }

    // Cache data: built inside the test runner scope while createReport is available
    var cache_entries = std.ArrayList(TestCacheResultEntry).empty;
    defer cache_entries.deinit(ctx.gpa);
    var cache_failure_reports = std.ArrayList([]const u8).empty;
    defer {
        for (cache_failure_reports.items) |r| if (r.len > 0) ctx.gpa.free(@constCast(r));
        cache_failure_reports.deinit(ctx.gpa);
    }

    // Run tests in the root module
    {
        var test_runner = TestRunner.init(
            ctx.gpa,
            @constCast(root_env),
            builtin_types,
            other_modules,
            builtin_module_env,
            &import_mapping,
        ) catch |err| {
            try stderr.print("Failed to create test runner for root module: {}\n", .{err});
            comptime_evaluator.deinit();
            return err;
        };
        defer test_runner.deinit();

        const summary = test_runner.eval_all() catch |err| {
            try stderr.print("Failed to evaluate tests in root module: {}\n", .{err});
            comptime_evaluator.deinit();
            return err;
        };

        total_passed += summary.passed;
        total_failed += summary.failed;

        // Copy test results for reporting
        var results = try ctx.gpa.alloc(TestResultItem, test_runner.test_results.items.len);
        for (test_runner.test_results.items, 0..) |tr, i| {
            results[i] = .{
                .passed = tr.passed,
                .region = tr.region,
                .error_msg = if (tr.error_msg) |msg| try ctx.gpa.dupe(u8, msg) else null,
            };
        }

        try module_results.append(.{
            .env = root_env,
            .path = args.path,
            .results = results,
        });

        // Build cache entries while test_runner is still alive (for createReport)
        // Always render verbose failure reports for caching (even in non-verbose mode)
        for (test_runner.test_results.items) |test_result| {
            var report_text: []const u8 = "";
            if (!test_result.passed) {
                var report_writer = std.Io.Writer.Allocating.init(ctx.gpa);
                errdefer report_writer.deinit();

                var report = test_runner.createReport(test_result, args.path) catch null;
                if (report != null) {
                    defer report.?.deinit();
                    const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
                    const config = reporting.ReportingConfig.initColorTerminal();
                    reporting.renderReportToTerminal(&report.?, &report_writer.writer, palette, config) catch {};
                }

                report_text = report_writer.toOwnedSlice() catch "";
            }
            try cache_failure_reports.append(ctx.gpa, report_text);

            try cache_entries.append(ctx.gpa, .{
                .passed = if (test_result.passed) 1 else 0,
                .region_start = test_result.region.start.offset,
                .region_end = test_result.region.end.offset,
                .report_len = @intCast(report_text.len),
            });
        }
    }

    // Run tests in all imported modules (recursive test execution)
    for (other_modules) |mod_env| {
        // Skip builtin module - no user tests there
        if (mod_env == builtin_module_env) continue;

        // Create import mapping for this module
        var mod_import_mapping = Check.createImportMapping(
            ctx.gpa,
            @constCast(mod_env).getIdentStore(),
            mod_env,
            builtin_module_env,
            builtin_indices,
            null,
        ) catch continue;
        defer mod_import_mapping.deinit();

        var test_runner = TestRunner.init(
            ctx.gpa,
            @constCast(mod_env),
            builtin_types,
            other_modules,
            builtin_module_env,
            &mod_import_mapping,
        ) catch continue;
        defer test_runner.deinit();

        const summary = test_runner.eval_all() catch continue;

        total_passed += summary.passed;
        total_failed += summary.failed;

        // Copy test results for reporting
        if (test_runner.test_results.items.len > 0) {
            var results = ctx.gpa.alloc(TestResultItem, test_runner.test_results.items.len) catch continue;
            for (test_runner.test_results.items, 0..) |tr, i| {
                results[i] = .{
                    .passed = tr.passed,
                    .region = tr.region,
                    .error_msg = if (tr.error_msg) |msg| ctx.gpa.dupe(u8, msg) catch null else null,
                };
            }

            // Find the module path from schedulers
            var mod_path: []const u8 = "<unknown>";
            var sched_iter2 = build_env.schedulers.iterator();
            outer: while (sched_iter2.next()) |sched_entry| {
                const scheduler2 = sched_entry.value_ptr.*;
                for (scheduler2.modules.items) |*m| {
                    if (m.env) |*env| {
                        if (env == mod_env) {
                            mod_path = m.path;
                            break :outer;
                        }
                    }
                }
            }

            module_results.append(.{
                .env = mod_env,
                .path = mod_path,
                .results = results,
            }) catch {
                ctx.gpa.free(results);
            };
        }
    }

    // Clean up comptime evaluator
    comptime_evaluator.deinit();

    // Calculate elapsed time
    const end_time = std.time.nanoTimestamp();
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;

    // --- Store test cache blob ---
    const cache_outcome: TestCacheOutcome = if (total_failed == 0 and !has_compilation_errors) .all_passed else .some_failed;

    if (!args.no_cache) {
        if (source) |src| {
            if (buildTestCacheBlob(
                ctx.gpa,
                cache_outcome,
                total_passed,
                total_failed,
                cache_entries.items,
                cache_failure_reports.items,
                "", // No comptime report in new BuildEnv architecture
            )) |blob| {
                defer ctx.gpa.free(blob);
                if (cache_config.getTestCacheDir(ctx.gpa)) |dir| {
                    defer ctx.gpa.free(dir);
                    const cache_key = CacheManager.generateCacheKey(src, build_options.compiler_version);
                    var store_cache_manager = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
                    store_cache_manager.storeRawBytes(cache_key, blob, dir);
                } else |_| {}
            } else |_| {}
        }
    }

    // Report results
    if (total_failed == 0 and !has_compilation_errors) {
        // Success case: print summary
        try stdout.print("All ({}) tests passed in {d:.1} ms.\n", .{ total_passed, elapsed_ms });
        if (args.verbose) {
            // Generate and render a detailed report if verbose is true
            for (module_results.items) |mr| {
                for (mr.results) |result| {
                    if (result.passed) {
                        const region_info = mr.env.calcRegionInfo(result.region);
                        try stdout.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ mr.path, region_info.start_line_idx + 1 });
                    }
                }
            }
        }
        return; // Exit with 0
    } else {
        // Failure case: always print summary with timing
        const total_tests = total_passed + total_failed;
        if (total_tests > 0) {
            try stderr.print("Ran {} test(s): {} passed, {} failed in {d:.1}ms\n", .{ total_tests, total_passed, total_failed, elapsed_ms });
        }

        if (args.verbose) {
            for (module_results.items) |mr| {
                for (mr.results) |result| {
                    const region_info = mr.env.calcRegionInfo(result.region);
                    if (result.passed) {
                        try stdout.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ mr.path, region_info.start_line_idx + 1 });
                    } else {
                        try stderr.print("\x1b[31mFAIL\x1b[0m: {s}:{}", .{ mr.path, region_info.start_line_idx + 1 });
                        if (result.error_msg) |msg| {
                            try stderr.print(" - {s}", .{msg});
                        }
                        try stderr.print("\n", .{});
                    }
                }
            }
        } else {
            // Non-verbose mode: just show simple FAIL messages with line numbers
            for (module_results.items) |mr| {
                for (mr.results) |result| {
                    if (!result.passed) {
                        const region_info = mr.env.calcRegionInfo(result.region);
                        try stderr.print("\x1b[31mFAIL\x1b[0m: {s}:{}\n", .{ mr.path, region_info.start_line_idx + 1 });
                    }
                }
            }
        }

        return error.TestsFailed;
    }
}

fn rocRepl(ctx: *CliContext, repl_args: cli_args.ReplArgs) !void {
    return cli_repl.run(ctx, repl_args.backend);
}

/// Error types for glue generation operations.
const GlueError = error{
    GlueSpecNotFound,
    NotPlatformFile,
    FileNotFound,
    ParseFailed,
    PlatformPathResolution,
    TempDirCreation,
    SyntheticAppWrite,
    BuildEnvInit,
    CompilationFailed,
    ModuleRetrieval,
    JsonSerialization,
    ExePathResolution,
    ProcessSpawn,
    ProcessFailed,
    OutOfMemory,
};

/// Print platform glue information for a platform's main.roc file using full compilation path.
/// This provides resolved types via TypeWriter and discovers hosted functions via e_hosted_lambda detection.
fn rocGlue(ctx: *CliContext, args: cli_args.GlueArgs) GlueError!void {
    rocGlueInner(ctx, args) catch |err| {
        const stderr = ctx.io.stderr();
        (switch (err) {
            error.GlueSpecNotFound => stderr.print("Error: Glue spec file not found: '{s}'\n", .{args.glue_spec}),
            error.NotPlatformFile => blk: {
                stderr.print("Error: '{s}' is not a platform file.\n", .{args.platform_path}) catch {};
                break :blk stderr.print("The glue command only works with platform files.\n", .{});
            },
            error.FileNotFound => stderr.print("Error: File not found: '{s}'\n", .{args.platform_path}),
            error.ParseFailed => stderr.print("Error: Failed to parse '{s}'\n", .{args.platform_path}),
            error.PlatformPathResolution => stderr.print("Error: Could not resolve platform path\n", .{}),
            error.TempDirCreation => stderr.print("Error: Could not create temp directory\n", .{}),
            error.SyntheticAppWrite => stderr.print("Error: Could not write synthetic app\n", .{}),
            error.BuildEnvInit => stderr.print("Error: Failed to initialize build environment\n", .{}),
            error.CompilationFailed => stderr.print("Error: Compilation failed\n", .{}),
            error.ModuleRetrieval => stderr.print("Error: Failed to get compiled modules\n", .{}),
            error.JsonSerialization => stderr.print("Error: Failed to serialize types to JSON\n", .{}),
            error.ExePathResolution => stderr.print("Error: Could not determine roc executable path\n", .{}),
            error.ProcessSpawn => stderr.print("Error: Could not spawn process\n", .{}),
            error.ProcessFailed => stderr.print("Error: Process failed\n", .{}),
            error.OutOfMemory => stderr.print("Error: Out of memory\n", .{}),
        }) catch {};
        return err;
    };
}

fn rocGlueInner(ctx: *CliContext, args: cli_args.GlueArgs) GlueError!void {
    const stderr = ctx.io.stderr();

    // 0. Validate glue spec file exists
    std.fs.cwd().access(args.glue_spec, .{}) catch {
        return error.GlueSpecNotFound;
    };

    // 1. Parse platform header to get requires entries and verify it's a platform file
    const platform_info = parsePlatformHeader(ctx, args.platform_path) catch |err| {
        return switch (err) {
            error.NotPlatformFile => error.NotPlatformFile,
            error.FileNotFound => error.FileNotFound,
            error.ParseFailed => error.ParseFailed,
            else => error.ParseFailed,
        };
    };
    defer platform_info.deinit(ctx.gpa);

    // 2. Compile platform using BuildEnv by creating a synthetic app
    // BuildEnv expects an app file, so we create a minimal app that imports the platform
    const platform_abs_path = std.fs.cwd().realpathAlloc(ctx.gpa, args.platform_path) catch {
        return error.PlatformPathResolution;
    };
    defer ctx.gpa.free(platform_abs_path);

    // Create temp directory for synthetic app and glue spec executable
    const temp_dir = createUniqueTempDir(ctx) catch {
        return error.TempDirCreation;
    };
    defer std.fs.cwd().deleteTree(temp_dir) catch {};

    // Generate synthetic app source that imports the platform
    var app_source = std.ArrayList(u8).empty;
    defer app_source.deinit(ctx.gpa);
    const w = app_source.writer(ctx.gpa);

    // Build requires clause: app [Alias1, Alias2, entry1, entry2, ...] { pf: platform "path" }
    try w.print("app [", .{});

    // Add type aliases first
    for (platform_info.type_aliases, 0..) |alias, i| {
        if (i > 0) try w.print(", ", .{});
        try w.print("{s}", .{alias});
    }

    // Add requires entries
    for (platform_info.requires_entries, 0..) |entry, i| {
        if (platform_info.type_aliases.len > 0 or i > 0) {
            try w.print(", ", .{});
        }
        try w.print("{s}", .{entry.name});
    }

    try w.print("] {{ pf: platform \"", .{});
    // Escape backslashes for the Roc string literal (Windows paths contain backslashes)
    for (platform_abs_path) |ch| {
        if (ch == '\\') {
            try w.print("\\\\", .{});
        } else {
            try w.print("{c}", .{ch});
        }
    }
    try w.print("\" }}\n\n", .{});

    // Generate type alias definitions: Model : {}
    for (platform_info.type_aliases) |alias| {
        try w.print("{s} : {{}}\n", .{alias});
    }
    if (platform_info.type_aliases.len > 0) {
        try w.print("\n", .{});
    }

    // Generate stub implementations for each requires entry
    for (platform_info.requires_entries) |entry| {
        try w.print("{s} = {s}\n", .{ entry.name, entry.stub_expr });
    }

    // Write synthetic app to temp file
    const synthetic_app_path = std.fs.path.join(ctx.gpa, &.{ temp_dir, "synthetic_app.roc" }) catch {
        return error.OutOfMemory;
    };
    defer ctx.gpa.free(synthetic_app_path);

    std.fs.cwd().writeFile(.{
        .sub_path = synthetic_app_path,
        .data = app_source.items,
    }) catch {
        return error.SyntheticAppWrite;
    };

    // Compile using BuildEnv
    const thread_count: usize = 1;
    const mode: Mode = .single_threaded;

    var build_env = BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative()) catch {
        return error.BuildEnvInit;
    };
    defer build_env.deinit();

    // Build the synthetic app (which compiles the platform as a dependency)
    build_env.build(synthetic_app_path) catch {
        // Drain and display error reports
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.gpa.free(drained);
        for (drained) |mod| {
            for (mod.reports) |*report| {
                const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
                const config = reporting.ReportingConfig.initColorTerminal();
                reporting.renderReportToTerminal(report, stderr, palette, config) catch {};
            }
        }
        return error.CompilationFailed;
    };

    // Drain any reports (warnings, etc.)
    {
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.gpa.free(drained);
        for (drained) |mod| {
            for (mod.reports) |*report| {
                const palette = reporting.ColorUtils.getPaletteForConfig(reporting.ReportingConfig.initColorTerminal());
                const config = reporting.ReportingConfig.initColorTerminal();
                reporting.renderReportToTerminal(report, stderr, palette, config) catch {};
            }
        }
    }

    // Get compiled modules in dependency order
    const modules = build_env.getModulesInSerializationOrder(ctx.gpa) catch {
        return error.ModuleRetrieval;
    };
    defer ctx.gpa.free(modules);

    // 3. Collect hosted functions from compiled platform modules
    const HostedCompiler = can.HostedCompiler;
    var all_hosted_fns = std.ArrayList(HostedCompiler.HostedFunctionInfo).empty;
    defer {
        for (all_hosted_fns.items) |fn_info| {
            ctx.gpa.free(fn_info.name_text);
        }
        all_hosted_fns.deinit(ctx.gpa);
    }

    for (modules) |mod| {
        if (mod.is_platform_sibling or mod.is_platform_main) {
            var module_fns = HostedCompiler.collectAndSortHostedFunctions(mod.env) catch continue;
            defer {
                for (module_fns.items) |fn_info| {
                    mod.env.gpa.free(fn_info.name_text);
                }
                module_fns.deinit(mod.env.gpa);
            }

            for (module_fns.items) |fn_info| {
                const name_copy = ctx.gpa.dupe(u8, fn_info.name_text) catch continue;
                all_hosted_fns.append(ctx.gpa, .{
                    .symbol_name = fn_info.symbol_name,
                    .expr_idx = fn_info.expr_idx,
                    .name_text = name_copy,
                }) catch {
                    ctx.gpa.free(name_copy);
                    continue;
                };
            }
        }
    }

    // Sort hosted functions globally
    const SortContext = struct {
        pub fn lessThan(_: void, a: HostedCompiler.HostedFunctionInfo, b: HostedCompiler.HostedFunctionInfo) bool {
            return std.mem.order(u8, a.name_text, b.name_text) == .lt;
        }
    };
    std.mem.sort(HostedCompiler.HostedFunctionInfo, all_hosted_fns.items, {}, SortContext.lessThan);

    // 4. Collect module type info for JSON serialization
    var collected_modules = std.ArrayList(CollectedModuleTypeInfo).empty;
    defer {
        for (collected_modules.items) |*mod_info| {
            mod_info.deinit(ctx.gpa);
        }
        collected_modules.deinit(ctx.gpa);
    }

    for (modules) |mod| {
        if (mod.is_platform_sibling or mod.is_platform_main) {
            if (collectModuleTypeInfo(ctx, &mod, mod.name, &all_hosted_fns)) |mod_info| {
                collected_modules.append(ctx.gpa, mod_info) catch {};
            }
        }
    }

    // Serialize collected module type infos to JSON
    const types_json = serializeModuleTypeInfosToJson(ctx.gpa, collected_modules.items) catch {
        return error.JsonSerialization;
    };
    defer ctx.gpa.free(types_json);

    // 5. Build and run the glue spec
    // Get path to current roc executable
    const roc_exe_path = std.fs.selfExePathAlloc(ctx.gpa) catch {
        return error.ExePathResolution;
    };
    defer ctx.gpa.free(roc_exe_path);

    // Use the same temp directory for glue spec executable
    const glue_exe_path = std.fs.path.join(ctx.gpa, &.{ temp_dir, "glue_spec" }) catch {
        return error.OutOfMemory;
    };
    defer ctx.gpa.free(glue_exe_path);

    // Build the glue spec using roc build
    {
        var build_argv = std.ArrayList([]const u8).empty;
        defer build_argv.deinit(ctx.gpa);

        try build_argv.append(ctx.gpa, roc_exe_path);
        try build_argv.append(ctx.gpa, "build");
        try build_argv.append(ctx.gpa, args.glue_spec);
        // Use --output=path format (CLI expects = not space)
        const output_arg = std.fmt.allocPrint(ctx.gpa, "--output={s}", .{glue_exe_path}) catch {
            return error.OutOfMemory;
        };
        defer ctx.gpa.free(output_arg);
        try build_argv.append(ctx.gpa, output_arg);

        var build_child = std.process.Child.init(build_argv.items, ctx.gpa);
        build_child.stdout_behavior = .Inherit;
        build_child.stderr_behavior = .Inherit;

        build_child.spawn() catch {
            return error.ProcessSpawn;
        };

        const term = build_child.wait() catch {
            return error.ProcessFailed;
        };

        switch (term) {
            .Exited => |exit_code| {
                if (exit_code != 0) {
                    return error.ProcessFailed;
                }
            },
            else => {
                return error.ProcessFailed;
            },
        }
    }

    // Run the glue spec with platform source path as argument
    // The glue platform will compile the modules and extract types directly
    {
        var run_argv = std.ArrayList([]const u8).empty;
        defer run_argv.deinit(ctx.gpa);

        try run_argv.append(ctx.gpa, glue_exe_path);

        // Pass platform source file path - the glue platform will do its own compilation
        try run_argv.append(ctx.gpa, args.platform_path);

        // Pass types JSON as argument
        const types_json_arg = std.fmt.allocPrint(ctx.gpa, "--types-json={s}", .{types_json}) catch {
            return error.OutOfMemory;
        };
        defer ctx.gpa.free(types_json_arg);
        try run_argv.append(ctx.gpa, types_json_arg);

        // Pass output directory as argument
        const output_dir_arg = std.fmt.allocPrint(ctx.gpa, "--output-dir={s}", .{args.output_dir}) catch {
            return error.OutOfMemory;
        };
        defer ctx.gpa.free(output_dir_arg);
        try run_argv.append(ctx.gpa, output_dir_arg);

        // Pass entry point names as additional arguments
        for (platform_info.requires_entries) |entry| {
            try run_argv.append(ctx.gpa, entry.name);
        }

        var run_child = std.process.Child.init(run_argv.items, ctx.gpa);
        run_child.stdout_behavior = .Inherit;
        run_child.stderr_behavior = .Inherit;

        run_child.spawn() catch {
            return error.ProcessSpawn;
        };

        const term = run_child.wait() catch {
            return error.ProcessFailed;
        };

        switch (term) {
            .Exited => |exit_code| {
                if (exit_code != 0) {
                    stderr.print("Glue spec exited with code {}\n", .{exit_code}) catch {};
                    return error.ProcessFailed;
                }
            },
            else => {
                stderr.print("Glue spec terminated abnormally\n", .{}) catch {};
                return error.ProcessFailed;
            },
        }
    }
}

/// Information extracted from a platform header for glue generation.
const PlatformHeaderInfo = struct {
    requires_entries: []RequiresEntry,
    type_aliases: [][]const u8,

    const RequiresEntry = struct {
        name: []const u8,
        type_str: []const u8,
        stub_expr: []const u8,
    };

    fn deinit(self: *const PlatformHeaderInfo, gpa: std.mem.Allocator) void {
        for (self.requires_entries) |entry| {
            gpa.free(entry.name);
            gpa.free(entry.type_str);
            gpa.free(entry.stub_expr);
        }
        gpa.free(self.requires_entries);
        for (self.type_aliases) |alias| {
            gpa.free(alias);
        }
        gpa.free(self.type_aliases);
    }
};

/// Parse a platform header to extract requires entries and validate it's a platform file.
fn parsePlatformHeader(ctx: *CliContext, platform_path: []const u8) !PlatformHeaderInfo {
    // Read source file
    var source = std.fs.cwd().readFileAlloc(ctx.gpa, platform_path, std.math.maxInt(usize)) catch |err| switch (err) {
        error.FileNotFound => return error.FileNotFound,
        else => return error.ParseFailed,
    };
    source = base.source_utils.normalizeLineEndingsRealloc(ctx.gpa, source) catch {
        ctx.gpa.free(source);
        return error.OutOfMemory;
    };
    defer ctx.gpa.free(source);

    // Get module name from path
    const module_name = std.fs.path.stem(platform_path);

    // Create ModuleEnv
    var env = ModuleEnv.init(ctx.gpa, source) catch return error.OutOfMemory;
    defer env.deinit();

    env.common.source = source;
    env.module_name = module_name;
    env.common.calcLineStarts(ctx.gpa) catch return error.OutOfMemory;

    var allocators: base.Allocators = undefined;
    allocators.initInPlace(ctx.gpa);
    defer allocators.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&allocators, &env.common) catch return error.ParseFailed;
    defer parse_ast.deinit();

    // Get the file header
    const file_node = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file_node.header);

    // Check if this is a platform file
    switch (header) {
        .platform => |platform_header| {
            // Extract requires entries
            const requires_entries_ast = parse_ast.store.requiresEntrySlice(platform_header.requires_entries);
            var requires_entries = std.ArrayList(PlatformHeaderInfo.RequiresEntry).empty;
            errdefer {
                for (requires_entries.items) |entry| {
                    ctx.gpa.free(entry.name);
                    ctx.gpa.free(entry.type_str);
                    ctx.gpa.free(entry.stub_expr);
                }
                requires_entries.deinit(ctx.gpa);
            }

            // Use a hash set to deduplicate type aliases across requires entries
            var type_alias_set = std.StringHashMap(void).init(ctx.gpa);
            defer type_alias_set.deinit();

            for (requires_entries_ast) |entry_idx| {
                const entry = parse_ast.store.getRequiresEntry(entry_idx);

                // Extract type aliases from for-clause
                const type_aliases_ast = parse_ast.store.forClauseTypeAliasSlice(entry.type_aliases);
                for (type_aliases_ast) |alias_idx| {
                    const alias = parse_ast.store.getForClauseTypeAlias(alias_idx);
                    if (parse_ast.tokens.resolveIdentifier(alias.alias_name)) |ident_idx| {
                        const alias_name = env.common.getIdent(ident_idx);
                        if (!type_alias_set.contains(alias_name)) {
                            try type_alias_set.put(try ctx.gpa.dupe(u8, alias_name), {});
                        }
                    }
                }

                if (parse_ast.tokens.resolveIdentifier(entry.entrypoint_name)) |ident_idx| {
                    const name = env.common.getIdent(ident_idx);

                    // Format type annotation to string
                    var type_buf = std.ArrayList(u8).empty;
                    defer type_buf.deinit(ctx.gpa);

                    printTypeAnnoToBuf(ctx.gpa, &env, parse_ast, entry.type_anno, &type_buf);

                    // Generate stub expression from type annotation
                    var stub_buf = std.ArrayList(u8).empty;
                    defer stub_buf.deinit(ctx.gpa);

                    generateStubExprFromTypeAnno(ctx.gpa, &env, parse_ast, entry.type_anno, &stub_buf);

                    try requires_entries.append(ctx.gpa, .{
                        .name = try ctx.gpa.dupe(u8, name),
                        .type_str = try type_buf.toOwnedSlice(ctx.gpa),
                        .stub_expr = try stub_buf.toOwnedSlice(ctx.gpa),
                    });
                }
            }

            // Convert type alias set to owned slice
            var type_aliases = std.ArrayList([]const u8).empty;
            errdefer {
                for (type_aliases.items) |alias| {
                    ctx.gpa.free(alias);
                }
                type_aliases.deinit(ctx.gpa);
            }
            var alias_iter = type_alias_set.keyIterator();
            while (alias_iter.next()) |key| {
                try type_aliases.append(ctx.gpa, key.*);
            }

            return PlatformHeaderInfo{
                .requires_entries = try requires_entries.toOwnedSlice(ctx.gpa),
                .type_aliases = try type_aliases.toOwnedSlice(ctx.gpa),
            };
        },
        else => return error.NotPlatformFile,
    }
}

/// Collected module type information for JSON serialization
const CollectedModuleTypeInfo = struct {
    name: []const u8,
    main_type: []const u8,
    functions: std.ArrayList(CollectedFunctionInfo),
    hosted_functions: std.ArrayList(CollectedHostedFunctionInfo),

    const CollectedFunctionInfo = struct {
        name: []const u8,
        type_str: []const u8,
    };

    const CollectedHostedFunctionInfo = struct {
        index: usize,
        name: []const u8,
        type_str: []const u8,
    };

    /// JSON-serializable view of CollectedModuleTypeInfo (uses slices instead of ArrayLists)
    const JsonView = struct {
        name: []const u8,
        main_type: []const u8,
        functions: []const CollectedFunctionInfo,
        hosted_functions: []const CollectedHostedFunctionInfo,
    };

    fn deinit(self: *CollectedModuleTypeInfo, gpa: std.mem.Allocator) void {
        gpa.free(self.name);
        gpa.free(self.main_type);
        for (self.functions.items) |f| {
            gpa.free(f.name);
            gpa.free(f.type_str);
        }
        self.functions.deinit(gpa);
        for (self.hosted_functions.items) |h| {
            gpa.free(h.name);
            gpa.free(h.type_str);
        }
        self.hosted_functions.deinit(gpa);
    }
};

/// Collect type information from a compiled module (same logic as printCompiledModuleTypes).
fn collectModuleTypeInfo(
    ctx: *CliContext,
    compiled_module: *const BuildEnv.CompiledModuleInfo,
    module_name: []const u8,
    all_hosted_fns: *const std.ArrayList(can.HostedCompiler.HostedFunctionInfo),
) ?CollectedModuleTypeInfo {
    const env = compiled_module.env;

    // Find main type
    var main_type_str: []const u8 = "";
    const all_stmts = env.store.sliceStatements(env.all_statements);

    for (all_stmts) |stmt_idx| {
        const stmt = env.store.getStatement(stmt_idx);
        if (stmt == .s_nominal_decl) {
            const nominal = stmt.s_nominal_decl;
            const type_header = env.store.getTypeHeader(nominal.header);
            const type_name = env.getIdent(type_header.relative_name);

            if (std.mem.eql(u8, type_name, module_name)) {
                var type_writer = env.initTypeWriter() catch continue;
                defer type_writer.deinit();

                const anno_node_idx: @TypeOf(env.store.nodes).Idx = @enumFromInt(@intFromEnum(nominal.anno));
                const type_var = ModuleEnv.varFrom(anno_node_idx);

                type_writer.write(type_var, .one_line) catch continue;
                const type_str = type_writer.get();

                main_type_str = ctx.gpa.dupe(u8, type_str) catch "";
                break;
            }
        }
    }

    // Collect functions
    const all_defs = env.store.sliceDefs(env.all_defs);
    var functions = std.ArrayList(CollectedModuleTypeInfo.CollectedFunctionInfo).empty;
    var hosted_functions = std.ArrayList(CollectedModuleTypeInfo.CollectedHostedFunctionInfo).empty;

    const module_prefix = std.fmt.allocPrint(ctx.gpa, "{s}.", .{module_name}) catch return null;
    defer ctx.gpa.free(module_prefix);

    for (all_defs) |def_idx| {
        const def = env.store.getDef(def_idx);
        const expr = env.store.getExpr(def.expr);

        const pattern = env.store.getPattern(def.pattern);
        if (pattern != .assign) continue;

        const def_name = env.getIdent(pattern.assign.ident);

        if (std.mem.eql(u8, def_name, module_name)) continue;

        const local_name = if (std.mem.startsWith(u8, def_name, module_prefix))
            def_name[module_prefix.len..]
        else
            continue;

        if (expr == .e_hosted_lambda) {
            const qualified_name = if (std.mem.endsWith(u8, def_name, "!"))
                def_name[0 .. def_name.len - 1]
            else
                def_name;

            for (all_hosted_fns.items, 0..) |fn_info, global_idx| {
                if (std.mem.eql(u8, fn_info.name_text, qualified_name)) {
                    var type_writer = env.initTypeWriter() catch continue;
                    defer type_writer.deinit();

                    const def_node_idx: @TypeOf(env.store.nodes).Idx = @enumFromInt(@intFromEnum(def_idx));
                    const type_var = ModuleEnv.varFrom(def_node_idx);

                    type_writer.write(type_var, .one_line) catch continue;
                    const type_str = type_writer.get();

                    hosted_functions.append(ctx.gpa, .{
                        .index = global_idx,
                        .name = ctx.gpa.dupe(u8, local_name) catch continue,
                        .type_str = ctx.gpa.dupe(u8, type_str) catch continue,
                    }) catch continue;
                    break;
                }
            }
        } else if (expr == .e_lambda or def.annotation != null) {
            var type_writer = env.initTypeWriter() catch continue;
            defer type_writer.deinit();

            const def_node_idx: @TypeOf(env.store.nodes).Idx = @enumFromInt(@intFromEnum(def_idx));
            const type_var = ModuleEnv.varFrom(def_node_idx);

            type_writer.write(type_var, .one_line) catch continue;
            const type_str = type_writer.get();

            functions.append(ctx.gpa, .{
                .name = ctx.gpa.dupe(u8, local_name) catch continue,
                .type_str = ctx.gpa.dupe(u8, type_str) catch continue,
            }) catch continue;
        }
    }

    return CollectedModuleTypeInfo{
        .name = ctx.gpa.dupe(u8, module_name) catch return null,
        .main_type = main_type_str,
        .functions = functions,
        .hosted_functions = hosted_functions,
    };
}

/// Serialize collected module type infos to JSON.
fn serializeModuleTypeInfosToJson(
    gpa: std.mem.Allocator,
    module_infos: []const CollectedModuleTypeInfo,
) ![]const u8 {
    // Create JSON-serializable views (slices instead of ArrayLists)
    var json_views = std.ArrayList(CollectedModuleTypeInfo.JsonView).empty;
    defer json_views.deinit(gpa);

    for (module_infos) |mod| {
        json_views.append(gpa, .{
            .name = mod.name,
            .main_type = mod.main_type,
            .functions = mod.functions.items,
            .hosted_functions = mod.hosted_functions.items,
        }) catch return error.OutOfMemory;
    }

    // Serialize using std.json
    var writer: std.io.Writer.Allocating = .init(gpa);
    defer writer.deinit();
    std.json.Stringify.value(json_views.items, .{}, &writer.writer) catch return error.OutOfMemory;
    return writer.toOwnedSlice();
}

/// Print a type annotation to a buffer (for requires entries which use AST types)
fn printTypeAnnoToBuf(gpa: std.mem.Allocator, env: *ModuleEnv, ast: *const parse.AST, type_anno_idx: parse.AST.TypeAnno.Idx, buf: *std.ArrayList(u8)) void {
    const type_anno = ast.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .@"fn" => |f| {
            const arrow = if (f.effectful) "=>" else "->";
            const args = ast.store.typeAnnoSlice(f.args);
            if (args.len == 0) {
                buf.appendSlice(gpa, "()") catch {};
            } else {
                for (args, 0..) |arg_idx, i| {
                    if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                    printTypeAnnoToBuf(gpa, env, ast, arg_idx, buf);
                }
            }
            buf.appendSlice(gpa, " ") catch {};
            buf.appendSlice(gpa, arrow) catch {};
            buf.appendSlice(gpa, " ") catch {};
            printTypeAnnoToBuf(gpa, env, ast, f.ret, buf);
        },
        .ty => |t| {
            // Print qualified type name
            const qualifiers = ast.store.tokenSlice(t.qualifiers);
            for (qualifiers) |qual_tok_idx| {
                const qual_tok: parse.tokenize.Token.Idx = @intCast(qual_tok_idx);
                if (ast.tokens.resolveIdentifier(qual_tok)) |ident_idx| {
                    buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
                    buf.append(gpa, '.') catch {};
                }
            }
            if (ast.tokens.resolveIdentifier(t.token)) |ident_idx| {
                buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
            }
        },
        .ty_var => |tv| {
            if (ast.tokens.resolveIdentifier(tv.tok)) |ident_idx| {
                buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
            }
        },
        .record => |r| {
            buf.appendSlice(gpa, "{ ") catch {};
            const fields = ast.store.annoRecordFieldSlice(r.fields);
            for (fields, 0..) |field_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                const field = ast.store.getAnnoRecordField(field_idx) catch continue;
                if (ast.tokens.resolveIdentifier(field.name)) |ident_idx| {
                    buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
                    buf.appendSlice(gpa, " : ") catch {};
                }
                printTypeAnnoToBuf(gpa, env, ast, field.ty, buf);
            }
            buf.appendSlice(gpa, " }") catch {};
            if (r.ext) |ext_idx| {
                printTypeAnnoToBuf(gpa, env, ast, ext_idx, buf);
            }
        },
        .tag_union => |tu| {
            buf.append(gpa, '[') catch {};
            const tags = ast.store.typeAnnoSlice(tu.tags);
            for (tags, 0..) |tag_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                printTypeAnnoToBuf(gpa, env, ast, tag_idx, buf);
            }
            switch (tu.ext) {
                .closed => {},
                .open => buf.appendSlice(gpa, ", ..") catch {},
                .named => |ext_idx| {
                    buf.appendSlice(gpa, ", ..") catch {};
                    printTypeAnnoToBuf(gpa, env, ast, ext_idx, buf);
                },
            }
            buf.append(gpa, ']') catch {};
        },
        .tuple => |t| {
            buf.append(gpa, '(') catch {};
            const annos = ast.store.typeAnnoSlice(t.annos);
            for (annos, 0..) |anno_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                printTypeAnnoToBuf(gpa, env, ast, anno_idx, buf);
            }
            buf.append(gpa, ')') catch {};
        },
        .apply => |a| {
            const args = ast.store.typeAnnoSlice(a.args);
            if (args.len > 0) {
                printTypeAnnoToBuf(gpa, env, ast, args[0], buf);
                if (args.len > 1) {
                    buf.append(gpa, ' ') catch {};
                    for (args[1..], 0..) |arg_idx, i| {
                        if (i > 0) buf.append(gpa, ' ') catch {};
                        printTypeAnnoToBuf(gpa, env, ast, arg_idx, buf);
                    }
                }
            }
        },
        .parens => |p| {
            buf.append(gpa, '(') catch {};
            printTypeAnnoToBuf(gpa, env, ast, p.anno, buf);
            buf.append(gpa, ')') catch {};
        },
        .underscore => {
            buf.append(gpa, '_') catch {};
        },
        .underscore_type_var => {
            buf.append(gpa, '_') catch {};
        },
        .malformed => {
            buf.appendSlice(gpa, "<malformed>") catch {};
        },
    }
}

/// Generate a stub expression from a type annotation.
/// This produces valid Roc expressions that will crash at runtime rather than compile-time.
/// Uses `...` inside lambdas to defer the crash to runtime.
fn generateStubExprFromTypeAnno(gpa: std.mem.Allocator, env: *ModuleEnv, ast: *const parse.AST, type_anno_idx: parse.AST.TypeAnno.Idx, buf: *std.ArrayList(u8)) void {
    const type_anno = ast.store.getTypeAnno(type_anno_idx);

    switch (type_anno) {
        .@"fn" => |f| {
            // Generate lambda stub
            const args = ast.store.typeAnnoSlice(f.args);
            if (args.len == 0) {
                // No args: || body
                buf.appendSlice(gpa, "|| ") catch {};
            } else {
                // Has args: |_, _, ...| body
                buf.append(gpa, '|') catch {};
                for (0..args.len) |i| {
                    if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                    buf.append(gpa, '_') catch {};
                }
                buf.appendSlice(gpa, "| ") catch {};
            }

            // Check if return type is unit {}
            const ret_anno = ast.store.getTypeAnno(f.ret);
            if (ret_anno == .record) {
                const record = ret_anno.record;
                const fields = ast.store.annoRecordFieldSlice(record.fields);
                if (fields.len == 0 and record.ext == null) {
                    // Return type is {} (unit) - return empty record
                    buf.appendSlice(gpa, "{}") catch {};
                    return;
                }
            }

            // Non-unit return type - use { ... } to crash at runtime (not compile-time)
            // The block syntax is required for single-line lambdas
            buf.appendSlice(gpa, "{ ... }") catch {};
        },
        .record => |r| {
            buf.appendSlice(gpa, "{ ") catch {};
            const fields = ast.store.annoRecordFieldSlice(r.fields);
            for (fields, 0..) |field_idx, i| {
                if (i > 0) buf.appendSlice(gpa, ", ") catch {};
                const field = ast.store.getAnnoRecordField(field_idx) catch continue;
                if (ast.tokens.resolveIdentifier(field.name)) |ident_idx| {
                    buf.appendSlice(gpa, env.common.getIdent(ident_idx)) catch {};
                    buf.appendSlice(gpa, ": ") catch {};
                }
                generateStubExprFromTypeAnno(gpa, env, ast, field.ty, buf);
            }
            buf.appendSlice(gpa, " }") catch {};
        },
        else => {
            // For all other types, use { ... } to crash at runtime
            buf.appendSlice(gpa, "{ ... }") catch {};
        },
    }
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(ctx: *CliContext, args: cli_args.FormatArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    if (args.stdin) {
        fmt.formatStdin(ctx.gpa) catch |err| return err;
        return;
    }

    var timer = try std.time.Timer.start();
    var elapsed: u64 = undefined;
    var failure_count: usize = 0;
    var had_errors: bool = false;

    if (args.check) {
        var unformatted_files = std.ArrayList([]const u8).empty;
        defer unformatted_files.deinit(ctx.gpa);

        for (args.paths) |path| {
            var result = try fmt.formatPath(ctx.gpa, ctx.arena, std.fs.cwd(), path, true);
            defer result.deinit();
            if (result.unformatted_files) |files| {
                try unformatted_files.appendSlice(ctx.gpa, files.items);
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
            const result = try fmt.formatPath(ctx.gpa, ctx.arena, std.fs.cwd(), path, false);
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

/// Helper function to format elapsed time as rounded integer milliseconds (no decimals)
fn formatElapsedTimeMs(writer: anytype, elapsed_ns: u64) !void {
    const elapsed_ms: u64 = (elapsed_ns + 500_000) / 1_000_000; // Round to nearest ms
    try writer.print("{}ms", .{elapsed_ms});
}

/// Compute cache hit percentage as an integer (0-100), rounded to nearest
fn cacheHitPercent(cache_hits: u32, cache_misses: u32) u32 {
    const total = cache_hits + cache_misses;
    if (total == 0) return 0;
    return @intCast((@as(u64, cache_hits) * 100 + total / 2) / total);
}

/// Compute average module time in nanoseconds
fn moduleTimeAvgNs(sum_ns: u64, count: u32) u64 {
    if (count == 0) return 0;
    return sum_ns / count;
}

/// Convert nanoseconds to rounded milliseconds
fn nsToMs(ns: u64) u32 {
    return @intCast((ns + 500_000) / 1_000_000);
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
    /// Build statistics
    modules_total: u32 = 0,
    cache_hits: u32 = 0,
    cache_misses: u32 = 0,
    modules_compiled: u32 = 0,
    /// Module compile time tracking (in nanoseconds)
    module_time_min_ns: u64 = 0,
    module_time_max_ns: u64 = 0,
    module_time_sum_ns: u64 = 0,

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
    ctx: *CliContext,
    filepath: []const u8,
    collect_timing: bool,
    cache_config: CacheConfig,
    max_threads: ?usize,
) BuildAppError!CheckResultWithBuildEnv {
    _ = collect_timing; // Timing is always collected by BuildEnv
    const trace = tracy.trace(@src());
    defer trace.end();

    // Determine threading mode and thread count
    // Default to multi-threaded with auto-detected CPU count; use -j1 for single-threaded
    const thread_count: usize = if (max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: compile.package.Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative());
    build_env.compiler_version = build_options.compiler_version;
    // Note: We do NOT defer build_env.deinit() here because we're returning it

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try ctx.gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager when caller calls deinit
    }

    // Build the file (works for both app and module files)
    build_env.build(filepath) catch |err| {
        // Even on error, try to drain and print any reports that were collected
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.freeDrainedReports(drained);

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
    var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
            .reports = mod.reports, // Transfer ownership
        };
    }

    // Free the original drained reports (abs_path strings and outer slice only)
    // Note: reports ownership was transferred above, abs_path was duped
    build_env.freeDrainedReportsPathsOnly(drained);

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
    ctx: *CliContext,
    filepath: []const u8,
    collect_timing: bool,
    cache_config: CacheConfig,
    max_threads: ?usize,
) BuildAppError!CheckResult {
    _ = collect_timing; // Timing is always collected by BuildEnv
    const trace = tracy.trace(@src());
    defer trace.end();

    // Determine threading mode and thread count
    // Default to multi-threaded with auto-detected CPU count; use -j1 for single-threaded
    const thread_count: usize = if (max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: compile.package.Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative());
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try ctx.gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(ctx.gpa, cache_config, Filesystem.default());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager
    }

    if (comptime build_options.trace_build) {
        std.debug.print("[CLI] Starting build for {s}\n", .{filepath});
    }

    // Build the file (works for both app and module files)
    build_env.build(filepath) catch {
        // Even on error, drain reports to show what went wrong
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.freeDrainedReportsPathsOnly(drained);

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

        // Get cache stats even on error
        const cache_stats = build_env.getCacheStats();

        return CheckResult{
            .reports = reports,
            .error_count = error_count,
            .warning_count = warning_count,
            .modules_total = cache_stats.modules_total,
            .cache_hits = cache_stats.cache_hits,
            .cache_misses = cache_stats.cache_misses,
            .modules_compiled = cache_stats.modules_compiled,
            .module_time_min_ns = cache_stats.module_time_min_ns,
            .module_time_max_ns = cache_stats.module_time_max_ns,
            .module_time_sum_ns = cache_stats.module_time_sum_ns,
        };
    };

    if (comptime build_options.trace_build) {
        std.debug.print("[CLI] Build complete, draining reports...\n", .{});
    }

    // Drain all reports
    const drained = try build_env.drainReports();

    if (comptime build_options.trace_build) {
        std.debug.print("[CLI] Reports drained: {} modules\n", .{drained.len});
    }

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
    var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
            .reports = mod.reports, // Transfer ownership
        };
    }

    // Free the original drained reports (abs_path strings and outer slice only)
    // Note: reports ownership was transferred above, abs_path was duped
    build_env.freeDrainedReportsPathsOnly(drained);

    // Get timing information from BuildEnv
    const timing = if (builtin.target.cpu.arch == .wasm32)
        CheckTimingInfo{}
    else
        build_env.getTimingInfo();

    // Get cache stats from coordinator
    const cache_stats = build_env.getCacheStats();

    if (comptime build_options.trace_build) {
        std.debug.print("[CLI] checkFileWithBuildEnv returning (defer deinit will run)\n", .{});
    }

    return CheckResult{
        .reports = reports,
        .timing = timing,
        .was_cached = false, // TODO: Set based on cache stats
        .error_count = error_count,
        .warning_count = warning_count,
        .modules_total = cache_stats.modules_total,
        .cache_hits = cache_stats.cache_hits,
        .cache_misses = cache_stats.cache_misses,
        .modules_compiled = cache_stats.modules_compiled,
        .module_time_min_ns = cache_stats.module_time_min_ns,
        .module_time_max_ns = cache_stats.module_time_max_ns,
        .module_time_sum_ns = cache_stats.module_time_sum_ns,
    };
}

fn rocCheck(ctx: *CliContext, args: cli_args.CheckArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    var timer = try std.time.Timer.start();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    // Use BuildEnv to check the file
    var check_result = checkFileWithBuildEnv(
        ctx,
        args.path,
        args.time,
        cache_config,
        args.max_threads,
    ) catch |err| {
        try handleProcessFileError(err, stderr, args.path);
        return;
    };
    defer check_result.deinit(ctx.gpa);

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
            formatElapsedTimeMs(stderr, elapsed) catch {};
            stderr.print(" with 100% cache hit for {s}\n", .{args.path}) catch {};
            stderr.print("(note: module loaded from cache, use --no-cache to display errors and warnings)\n", .{}) catch {};
            return error.CheckFailed;
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTimeMs(stdout, elapsed) catch {};
            stdout.print(" with 100% cache hit for {s}\n", .{args.path}) catch {};
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
        ctx.io.flush();

        // Compute cache hit percentage
        const cache_percent = cacheHitPercent(check_result.cache_hits, check_result.cache_misses);

        if (check_result.error_count > 0 or check_result.warning_count > 0) {
            stderr.writeAll("\n") catch {};
            stderr.print("Found {} error(s) and {} warning(s) in ", .{
                check_result.error_count,
                check_result.warning_count,
            }) catch {};
            formatElapsedTimeMs(stderr, elapsed) catch {};
            // Include inline cache stats summary
            if (check_result.modules_total > 0 and check_result.cache_hits > 0) {
                stderr.print(" with {}% cache hit", .{cache_percent}) catch {};
            }
            stderr.print(" for {s}.\n", .{args.path}) catch {};

            // Print verbose stats if requested
            if (args.verbose) {
                printVerboseStats(stderr, &check_result);
            }

            // Flush before exit
            ctx.io.flush();

            // Exit with code 1 for errors, code 2 for warnings only
            if (check_result.error_count > 0) {
                return error.CheckFailed;
            } else {
                std.process.exit(2);
            }
        } else {
            stdout.print("No errors found in ", .{}) catch {};
            formatElapsedTimeMs(stdout, elapsed) catch {};
            // Include inline cache stats summary
            if (check_result.modules_total > 0 and check_result.cache_hits > 0) {
                stdout.print(" with {}% cache hit", .{cache_percent}) catch {};
            }
            stdout.print(" for {s}\n", .{args.path}) catch {};

            // Print verbose stats if requested
            if (args.verbose) {
                printVerboseStats(stdout, &check_result);
            }

            ctx.io.flush();
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

/// Print verbose build statistics when --verbose flag is passed
/// Format:
///     Modules: 6 total, 4 cached, 2 built
///     Cache Hit: 67%
///     Build: 8ms / 14ms / 25ms (min / avg / max)
fn printVerboseStats(writer: anytype, result: *const CheckResult) void {
    const total = result.modules_total;
    if (total == 0) return;

    const cache_percent = cacheHitPercent(result.cache_hits, result.cache_misses);

    // Print modules breakdown
    writer.print("\n    Modules: {} total, {} cached, {} built\n", .{
        total,
        result.cache_hits,
        result.modules_compiled,
    }) catch {};

    // Print cache hit percentage
    writer.print("    Cache Hit: {}%\n", .{cache_percent}) catch {};

    // Print build time breakdown (only if we have compiled modules)
    if (result.modules_compiled > 0) {
        const min_ms = nsToMs(result.module_time_min_ns);
        const avg_ms = nsToMs(moduleTimeAvgNs(result.module_time_sum_ns, result.modules_compiled));
        const max_ms = nsToMs(result.module_time_max_ns);
        writer.print("    Build: {}ms / {}ms / {}ms (min / avg / max)\n", .{
            min_ms,
            avg_ms,
            max_ms,
        }) catch {};
    }
}

/// Start an HTTP server to serve the generated documentation
fn serveDocumentation(ctx: *CliContext, docs_dir: []const u8) !void {
    const stdout = ctx.io.stdout();

    const address = try std.net.Address.parseIp("127.0.0.1", 8080);
    var server = try address.listen(.{
        .reuse_address = true,
    });
    defer server.deinit();

    stdout.print("Visit http://localhost:8080 to view the docs at ./{s}/\n", .{docs_dir}) catch {};
    stdout.print("Press Ctrl+C to stop the server\n", .{}) catch {};

    while (true) {
        const connection = try server.accept();
        handleConnection(ctx, connection, docs_dir) catch |err| {
            std.debug.print("Error handling connection: {}\n", .{err});
        };
    }
}

/// Handle a single HTTP connection
fn handleConnection(ctx: *CliContext, connection: std.net.Server.Connection, docs_dir: []const u8) !void {
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
    const file_path = try resolveFilePath(ctx, docs_dir, path);

    // Try to open and serve the file
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        switch (err) {
            error.FileNotFound => try sendResponse(connection.stream, "404 Not Found", "text/plain", "File Not Found"),
            else => try sendResponse(connection.stream, "500 Internal Server Error", "text/plain", "Internal Server Error"),
        }
        return;
    };
    defer file.close();

    // Read file contents
    const file_content = try file.readToEndAlloc(ctx.gpa, 10 * 1024 * 1024); // 10MB max
    defer ctx.gpa.free(file_content);

    // Determine content type
    const content_type = getContentType(file_path);

    // Send response
    try sendResponse(connection.stream, "200 OK", content_type, file_content);
}

/// Resolve the file path based on the URL path.
/// Returns arena-allocated path (no need to free).
fn resolveFilePath(ctx: *CliContext, docs_dir: []const u8, url_path: []const u8) ![]const u8 {
    // Remove leading slash
    const clean_path = if (url_path.len > 0 and url_path[0] == '/')
        url_path[1..]
    else
        url_path;

    // If path is empty or ends with /, serve index.html
    if (clean_path.len == 0 or clean_path[clean_path.len - 1] == '/') {
        return try std.fmt.allocPrint(ctx.arena, "{s}/{s}index.html", .{ docs_dir, clean_path });
    }

    // Check if the path has a file extension (contains a dot in the last component)
    const last_slash = std.mem.lastIndexOfScalar(u8, clean_path, '/') orelse 0;
    const last_component = clean_path[last_slash..];
    const has_extension = std.mem.indexOfScalar(u8, last_component, '.') != null;

    if (has_extension) {
        // Path has extension, serve the file directly
        return try std.fmt.allocPrint(ctx.arena, "{s}/{s}", .{ docs_dir, clean_path });
    } else {
        // No extension, serve index.html from that directory
        return try std.fmt.allocPrint(ctx.arena, "{s}/{s}/index.html", .{ docs_dir, clean_path });
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

fn rocDocs(ctx: *CliContext, args: cli_args.DocsArgs) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    var timer = try std.time.Timer.start();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
    };

    // Use BuildEnv to check the file, preserving the BuildEnv for docs generation
    var result_with_env = checkFileWithBuildEnvPreserved(
        ctx,
        args.path,
        args.time,
        cache_config,
        null, // max_threads: use default (single-threaded for now)
    ) catch |err| {
        return handleProcessFileError(err, stderr, args.path);
    };

    // Clean up when we're done - this includes the BuildEnv and all module envs
    defer result_with_env.deinit(ctx.gpa);

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
    try generateDocs(ctx, &result_with_env.build_env, args.path, args.output);

    stdout.print("\nDocumentation generation complete for {s}\n", .{args.path}) catch {};

    // Start HTTP server if --serve flag is enabled
    if (args.serve) {
        try serveDocumentation(ctx, args.output);
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
    ctx: *CliContext,
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
    const index_path = try std.fs.path.join(ctx.arena, &[_][]const u8{ output_path, "index.html" });

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
    ctx: *CliContext,
    output_path: []const u8,
    module_name: []const u8,
) !void {
    // Create output directory if it doesn't exist
    std.fs.cwd().makePath(output_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Create index.html file
    const index_path = try std.fs.path.join(ctx.arena, &[_][]const u8{ output_path, "index.html" });

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
    ctx: *CliContext,
    module_env: *const ModuleEnv,
    record_fields: can.CIR.RecordField.Span,
) ![]AssociatedItem {
    var items = std.array_list.Managed(AssociatedItem).init(ctx.gpa);
    errdefer {
        for (items.items) |item| {
            item.deinit(ctx.gpa);
        }
        items.deinit();
    }

    const fields_slice = module_env.store.sliceRecordFields(record_fields);
    for (fields_slice) |field_idx| {
        const field = module_env.store.getRecordField(field_idx);
        const field_name = try ctx.gpa.dupe(u8, module_env.getIdentText(field.name));
        errdefer ctx.gpa.free(field_name);

        // Check if the field value is a nominal type (has nested associated items)
        const field_expr = module_env.store.getExpr(field.value);
        const children = switch (field_expr) {
            .e_nominal => |nom| blk: {
                // Get the nominal type's backing expression
                const backing_expr = module_env.store.getExpr(nom.backing_expr);
                break :blk switch (backing_expr) {
                    .e_record => |rec| try extractRecordAssociatedItems(ctx, module_env, rec.fields),
                    else => try ctx.gpa.alloc(AssociatedItem, 0),
                };
            },
            else => try ctx.gpa.alloc(AssociatedItem, 0),
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
    ctx: *CliContext,
    module_env: *const ModuleEnv,
) ![]AssociatedItem {
    var items = std.array_list.Managed(AssociatedItem).init(ctx.gpa);
    errdefer {
        for (items.items) |item| {
            item.deinit(ctx.gpa);
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

        const name = try ctx.gpa.dupe(u8, module_env.getIdentText(name_ident_opt));
        errdefer ctx.gpa.free(name);

        // Extract nested associated items if this is a nominal type with a record
        const children = switch (pattern) {
            .nominal => blk: {
                // For nominal types, look at the expression to find associated items
                const expr = module_env.store.getExpr(def.expr);
                break :blk switch (expr) {
                    .e_nominal => |nom_expr| blk2: {
                        const backing = module_env.store.getExpr(nom_expr.backing_expr);
                        break :blk2 switch (backing) {
                            .e_record => |record| try extractRecordAssociatedItems(ctx, module_env, record.fields),
                            else => try ctx.gpa.alloc(AssociatedItem, 0),
                        };
                    },
                    else => try ctx.gpa.alloc(AssociatedItem, 0),
                };
            },
            else => try ctx.gpa.alloc(AssociatedItem, 0),
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
    ctx: *CliContext,
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
        try generateAppDocs(ctx, build_env, module_path, base_output_dir);
    } else {
        // For packages, just generate package dependency docs
        try generatePackageDocs(ctx, build_env, module_path, base_output_dir, "");
    }
}

/// Generate docs for an app module
fn generateAppDocs(
    ctx: *CliContext,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
) !void {
    // Collect all imported modules (both local and from packages)
    var modules_map = std.StringHashMap(ModuleInfo).init(ctx.gpa);
    defer {
        var it = modules_map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(ctx.gpa);
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
                // Parse the import (e.g., "cli.Stdout" -> { .qualifier = "cli", .module = "Stdout" })
                if (base.module_path.parseQualifiedImport(ext_import)) |qualified| {
                    // Create full name and link path
                    const full_name = try ctx.arena.dupe(u8, ext_import);
                    const link_path = try std.fmt.allocPrint(ctx.arena, "{s}/{s}", .{ qualified.qualifier, qualified.module });

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
                    const module_output_dir = try std.fs.path.join(ctx.arena, &[_][]const u8{ base_output_dir, qualified.qualifier, qualified.module });
                    generateModuleIndex(ctx, module_output_dir, ext_import) catch |err| {
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
                        const full_name = try ctx.gpa.dupe(u8, module_name);
                        const link_path = try ctx.gpa.dupe(u8, module_name);

                        // Extract associated items from the module if it has an env
                        const associated_items = if (imported_module.env) |*mod_env|
                            try extractAssociatedItems(ctx, mod_env)
                        else
                            try ctx.gpa.alloc(AssociatedItem, 0);

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
                            ctx.gpa.free(full_name);
                            ctx.gpa.free(link_path);
                            for (associated_items) |item| {
                                item.deinit(ctx.gpa);
                            }
                            ctx.gpa.free(associated_items);
                        }

                        // Generate index.html for this local module
                        const module_output_dir = try std.fs.path.join(ctx.arena, &[_][]const u8{ base_output_dir, module_name });
                        generateModuleIndex(ctx, module_output_dir, module_name) catch |err| {
                            std.debug.print("Warning: failed to generate module index for {s}: {}\n", .{ module_name, err });
                        };
                    }
                }
            }
        }
    }

    // Convert map to sorted list
    var modules_list = std.ArrayList(ModuleInfo).empty;
    defer modules_list.deinit(ctx.gpa);
    var map_iter = modules_map.iterator();
    while (map_iter.next()) |entry| {
        try modules_list.append(ctx.gpa, entry.value_ptr.*);
    }

    // Collect package shorthands
    var shorthands_list = std.array_list.Managed([]const u8).init(ctx.gpa);
    defer {
        for (shorthands_list.items) |item| ctx.gpa.free(item);
        shorthands_list.deinit();
    }

    var shorthand_iter = first_pkg.shorthands.iterator();
    while (shorthand_iter.next()) |sh_entry| {
        const shorthand = try ctx.gpa.dupe(u8, sh_entry.key_ptr.*);
        try shorthands_list.append(shorthand);
    }

    // Generate root index.html
    try generatePackageIndex(ctx, base_output_dir, module_path, shorthands_list.items, modules_list.items);

    // Generate package dependency docs recursively
    shorthand_iter = first_pkg.shorthands.iterator();
    while (shorthand_iter.next()) |sh_entry| {
        const shorthand = sh_entry.key_ptr.*;
        const dep_ref = sh_entry.value_ptr.*;

        generatePackageDocs(ctx, build_env, dep_ref.root_file, base_output_dir, shorthand) catch |err| {
            std.debug.print("Warning: failed to generate docs for package {s}: {}\n", .{ shorthand, err });
        };
    }
}

/// Recursively generate documentation for a package and its dependencies
fn generatePackageDocs(
    ctx: *CliContext,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
    relative_path: []const u8,
) error{OutOfMemory}!void {
    const output_dir = if (relative_path.len == 0)
        try ctx.arena.dupe(u8, base_output_dir)
    else
        try std.fs.path.join(ctx.arena, &[_][]const u8{ base_output_dir, relative_path });

    var shorthands_list = std.array_list.Managed([]const u8).init(ctx.gpa);
    defer {
        for (shorthands_list.items) |item| ctx.gpa.free(item);
        shorthands_list.deinit();
    }

    var pkg_iter = build_env.packages.iterator();
    while (pkg_iter.next()) |entry| {
        const pkg = entry.value_ptr;

        var shorthand_iter = pkg.shorthands.iterator();
        while (shorthand_iter.next()) |sh_entry| {
            const shorthand = try ctx.gpa.dupe(u8, sh_entry.key_ptr.*);
            try shorthands_list.append(shorthand);
        }

        shorthand_iter = pkg.shorthands.iterator();
        while (shorthand_iter.next()) |sh_entry| {
            const shorthand = sh_entry.key_ptr.*;

            const dep_relative_path = if (relative_path.len == 0)
                try ctx.arena.dupe(u8, shorthand)
            else
                try std.fs.path.join(ctx.arena, &[_][]const u8{ relative_path, shorthand });

            const dep_ref = sh_entry.value_ptr.*;
            generatePackageDocs(ctx, build_env, dep_ref.root_file, base_output_dir, dep_relative_path) catch |err| {
                std.debug.print("Warning: failed to generate docs for {s}: {}\n", .{ shorthand, err });
            };
        }

        break;
    }

    // For standalone modules, extract and display their exports
    var module_infos = std.array_list.Managed(ModuleInfo).init(ctx.gpa);
    defer {
        for (module_infos.items) |mod| mod.deinit(ctx.gpa);
        module_infos.deinit();
    }

    // Get the module's exports if it's a standalone module
    var sched_iter = build_env.schedulers.iterator();
    while (sched_iter.next()) |sched_entry| {
        const package_env = sched_entry.value_ptr.*;

        // Check ALL modules in this package
        for (package_env.modules.items) |module_state| {
            if (module_state.env) |*mod_env| {
                const associated_items = try extractAssociatedItems(ctx, mod_env);
                const mod_name = try ctx.gpa.dupe(u8, module_state.name);

                try module_infos.append(.{
                    .name = mod_name,
                    .link_path = try ctx.gpa.dupe(u8, ""),
                    .associated_items = associated_items,
                });
            }
        }
    }

    generatePackageIndex(ctx, output_dir, module_path, shorthands_list.items, module_infos.items) catch |err| {
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
