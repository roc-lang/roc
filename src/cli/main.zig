//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`
//!
//! ## Module Data Modes
//!
//! The CLI supports two modes for passing compiled Roc programs to the interpreter:
//!
//! ### IPC Mode (`roc path/to/app.roc`)
//! - Compiles Roc source through ARC-inserted LIR and publishes a viewable LIR image in shared memory
//! - Spawns interpreter host as child process that maps the shared memory
//! - Fast startup, same-architecture only
//! - See: `buildLirImageWithCoordinator`, `rocRun`
//!
//! ### Embedded Interpreter Mode (`roc build --opt=interpreter path/to/app.roc`)
//! - Compiles Roc source through the same checked-artifact to LIR path as IPC mode
//! - Embeds the viewable LIR image directly in the output binary
//! - The interpreter shim receives only the LIR image pointer and length
//!
//! For detailed documentation, see `src/interpreter_shim/README.md`.

const std = @import("std");
const builtin = @import("builtin");
/// Configure std library logging to suppress debug messages in production.
/// This prevents debug logs from polluting stderr which should only contain
/// actual program output (like Stderr.line! calls).
pub const std_options: std.Options = .{
    .log_level = .warn,
    // On Windows, Zig's default segfault handler installs a vectored exception
    // handler that runs before SetUnhandledExceptionFilter and short-circuits
    // our handler in src/base/stack_overflow.zig. Disable it on Windows so our
    // signal-safe handler runs and we get stable exit codes (134/136/139).
    .enable_segfault_handler = builtin.os.tag != .windows and std.debug.default_enable_segfault_handler,
};
var debug_threaded_io_instance: std.Io.Threaded = .init_single_threaded;
/// Override the default debug IO so that `std.Options.debug_io` uses a properly
/// initialized Threaded instance with a real allocator. Without this, the default
/// `global_single_threaded` has `.allocator = .failing` and process spawning fails.
pub const std_options_debug_threaded_io: *std.Io.Threaded = &debug_threaded_io_instance;

const build_options = @import("build_options");
const base = @import("base");
const reporting = @import("reporting");
const parse = @import("parse");
const tracy = @import("tracy");
const ctx_mod = @import("ctx");
const compile = @import("compile");
const can = @import("can");
const check = @import("check");
const bundle = @import("bundle");
const unbundle = @import("unbundle");

comptime {
    if (builtin.is_test) {
        _ = @import("libc_finder.zig");
        _ = @import("test_shared_memory_system.zig");
    }
}
const ipc = @import("ipc");
const fmt = @import("fmt");
const eval = @import("eval");
const lir = @import("lir");
const echo_platform = @import("echo_platform");
const lsp = @import("lsp");
const ansi_term = @import("ansi_term.zig");

const cli_args = @import("cli_args.zig");
const roc_target = @import("target.zig");
pub const targets_validator = @import("targets_validator.zig");
const platform_validation = @import("platform_validation.zig");
const cli_context = @import("CliCtx.zig");
const cli_problem = @import("CliProblem.zig");
const ReplLine = @import("ReplLine.zig");
const ReplSession = @import("ReplSession.zig");

const CliCtx = cli_context.CliCtx;
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
        std.testing.refAllDecls(@import("ReplLine.zig"));
    }
}
const bench = @import("bench.zig");
const linker = @import("linker.zig");
const platform_host_shim = @import("platform_host_shim.zig");
const builder = @import("builder.zig");

/// Check if LLVM is available
const llvm_available = builder.isLLVMAvailable();

const SharedMemoryAllocator = ipc.SharedMemoryAllocator;
const CoreCtx = ctx_mod.CoreCtx;
const ModuleEnv = can.ModuleEnv;
const BuildEnv = compile.BuildEnv;
const Coordinator = compile.coordinator.Coordinator;
const Mode = compile.package.Mode;
const TimingInfo = compile.package.TimingInfo;
const CacheManager = compile.CacheManager;
const CacheConfig = compile.CacheConfig;
const cache_config_mod = compile.config;
const backend = @import("backend");
const layout = @import("layout");
const docs = @import("docs");
const RocTarget = @import("target.zig").RocTarget;

/// Embedded interpreter shim library for the native host target.
/// Used by `roc run` after the parent process has lowered checked artifacts to
/// an ARC-inserted LIR image in shared memory.
const ShimLibraries = struct {
    const native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.target.os.tag == .windows)
        @embedFile("roc_interpreter_shim.lib")
    else
        @embedFile("libroc_interpreter_shim.a");

    pub fn forTarget(_: RocTarget) []const u8 {
        return native;
    }
};

fn interpreterShimBytes(target: ?RocTarget) []const u8 {
    return if (target) |t| ShimLibraries.forTarget(t) else ShimLibraries.native;
}

fn interpreterShimDigest(target: ?RocTarget) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc-interpreter-shim-cache-v1");
    hasher.update(interpreterShimBytes(target));
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

fn interpreterShimCacheFilename(ctx: *CliCtx, target: RocTarget) Allocator.Error![]const u8 {
    const digest = interpreterShimDigest(target);
    const digest_hex = std.fmt.bytesToHex(digest, .lower);
    return if (target.isWindows())
        std.fmt.allocPrint(ctx.arena, "roc_interpreter_shim_{s}_{s}.lib", .{ @tagName(target), digest_hex[0..] })
    else
        std.fmt.allocPrint(ctx.arena, "libroc_interpreter_shim_{s}_{s}.a", .{ @tagName(target), digest_hex[0..] });
}

/// Embedded pre-compiled builtins object files for each target.
/// These contain the wrapper functions needed by the dev backend for string/list operations.
/// Used by `roc build --opt=dev` to link the app object with builtins.
/// Now using static libraries instead of object files to include compiler_rt
/// (needed for 128-bit integer operations used by Dec type).
const BuiltinsObjects = struct {
    /// Native builtins (for host platform builds)
    const native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.os.tag == .windows)
        @embedFile("roc_builtins.obj")
    else
        @embedFile("roc_builtins.o");

    /// Cross-compilation target builtins (Linux musl targets)
    const x64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64musl/roc_builtins.o");
    const arm64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64musl/roc_builtins.o");

    /// Cross-compilation target builtins (Linux glibc targets)
    const x64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64glibc/roc_builtins.o");
    const arm64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64glibc/roc_builtins.o");

    /// WebAssembly target builtins (wasm32-freestanding) - not used by dev backend
    const wasm32 = if (builtin.is_test) &[_]u8{} else @embedFile("targets/wasm32/roc_builtins.o");

    /// Cross-compilation target builtins (Windows targets)
    const x64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64win/roc_builtins.obj");
    const arm64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64win/roc_builtins.obj");

    /// Cross-compilation target builtins (macOS targets)
    const x64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64mac/roc_builtins.o");
    const arm64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64mac/roc_builtins.o");

    /// Get the appropriate builtins object bytes for the given target
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

    /// Get the filename for builtins object on given target
    pub fn filename(target: RocTarget) []const u8 {
        return switch (target.toOsTag()) {
            .windows => "roc_builtins.obj",
            else => "roc_builtins.o",
        };
    }
};

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
    // GetConsoleOutputCP / SetConsoleOutputCP were removed from std.os.windows.kernel32
    // in Zig 0.16; declare them locally.
    const console = struct {
        extern "kernel32" fn GetConsoleOutputCP() callconv(.winapi) u32;
        extern "kernel32" fn SetConsoleOutputCP(wCodePageID: u32) callconv(.winapi) std.os.windows.BOOL;
    };
    const current_code_page = console.GetConsoleOutputCP();
    if (current_code_page != 0 and current_code_page != 65001) {
        windows_console_previous_code_page = current_code_page;
        _ = console.SetConsoleOutputCP(65001);
    }
    // Note: ANSI escape support is enabled in Io.init()
}

fn restoreWindowsConsoleCodePage() void {
    if (!is_windows) return;
    const console = struct {
        extern "kernel32" fn SetConsoleOutputCP(wCodePageID: u32) callconv(.winapi) std.os.windows.BOOL;
    };
    if (windows_console_previous_code_page) |code_page| {
        windows_console_previous_code_page = null;
        _ = console.SetConsoleOutputCP(code_page);
    }
}

// POSIX shared memory functions
const posix = if (!is_windows) struct {
    extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
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

const legalDetailsFileContent = @embedFile("legal_details");

/// Preferred size for shared memory allocator: 2TB on 64-bit, 256MB on 32-bit.
///
/// We need a large size because SharedMemoryAllocator is a bump allocator that
/// cannot free memory. During type checking, the types Store grows significantly
/// and every array growth allocates new memory without freeing old, causing
/// memory fragmentation. With a 25KB source file, type checking can use ~2GB
/// of shared memory due to this fragmentation.
///
/// On 64-bit Linux, we reserve 2TB of virtual address space. This is possible
/// without consuming physical memory because memfd_create with lazy page
/// allocation means untouched pages cost nothing.
///
/// On macOS, shm_open + ftruncate creates a Mach VM object with higher per-object
/// kernel overhead than Linux's memfd_create. Using 2TB causes kernel resource pressure
/// that accumulates across rapid sequential process invocations (e.g., running tests
/// in a loop), leading to SIGKILL from the jetsam memory pressure system.
/// We use 8GB on macOS which provides ample headroom while keeping kernel overhead low.
///
/// On Windows, SEC_RESERVE on CreateFileMapping reserves address space without
/// page file backing, but MapViewOfFile still appears to charge against the
/// system commit limit. Under parallel test load (`zig build test` with several
/// workers each spawning `roc.exe`), four concurrent 2 TB reservations trip
/// ERROR_COMMITMENT_LIMIT on CI runners (7 GB RAM + limited page file). 8 GB
/// matches the macOS bound and leaves plenty of headroom for real programs.
///
/// On 32-bit targets, we use 256MB since larger sizes won't fit in the address space.
///
/// Test builds may provide an explicit size with `-Dshared-memory-size`.
/// This keeps production Linux at 2TB while allowing Valgrind CI to use a
/// smaller arena that Memcheck can map.
///
/// If the OS rejects the preferred reservation (e.g. aarch64 Linux kernels
/// built with CONFIG_ARM64_VA_BITS=39 cap user VA at ~256 GiB and refuse a
/// 2 TiB mmap with ENOMEM), `createSharedMemory` halves the request down to
/// `SHARED_MEMORY_MIN_SIZE` before giving up. See `createWithMinSize`.
const SHARED_MEMORY_SIZE: usize = if (build_options.has_shared_memory_size)
    configuredSharedMemorySize()
else if (@sizeOf(usize) < 8)
    256 * 1024 * 1024 // 256MB for 32-bit targets
else if (builtin.os.tag == .macos)
    8 * 1024 * 1024 * 1024 // 8GB for macOS (shm_open has higher kernel overhead)
else if (builtin.os.tag == .windows)
    8 * 1024 * 1024 * 1024 // 8GB for Windows (MapViewOfFile commit accounting)
else
    2 * 1024 * 1024 * 1024 * 1024; // 2TB for 64-bit Linux

fn configuredSharedMemorySize() usize {
    if (comptime build_options.shared_memory_size > std.math.maxInt(usize)) {
        @compileError("-Dshared-memory-size does not fit in usize for this target");
    }

    return @intCast(build_options.shared_memory_size);
}

/// Floor for the retry loop in `createSharedMemory`. Set to the
/// macOS/Windows reservation — documented as "ample headroom for real
/// programs" — so a smaller reservation still produces a usable arena. On
/// 32-bit targets the preferred size is already smaller than 8 GiB and an
/// 8 GiB literal doesn't fit in `usize`, so the floor is the preferred size
/// itself (single attempt, no retry); `-Dshared-memory-size` builds are
/// likewise handled by the allocator clamping `min_size` to the preferred.
const SHARED_MEMORY_MIN_SIZE: usize = if (@sizeOf(usize) < 8)
    SHARED_MEMORY_SIZE
else
    8 * 1024 * 1024 * 1024;

/// Create the shared-memory arena used for the parent-produced LIR runtime
/// image. Tries the preferred size first and halves down to
/// `SHARED_MEMORY_MIN_SIZE` if the OS rejects the reservation; see
/// `SharedMemoryAllocator.createWithMinSize` for details.
fn createSharedMemory(io: std.Io, page_size: usize) error{ CreateFileMappingFailed, FtruncateFailed, InvalidHandle, MapViewOfFileFailed, MemfdCreateFailed, MmapFailed, OpenFileMappingFailed, OutOfMemory, ShmOpenFailed, ShmUnlinkFailed, UnsupportedPlatform }!SharedMemoryAllocator {
    return SharedMemoryAllocator.createWithMinSize(io, SHARED_MEMORY_SIZE, SHARED_MEMORY_MIN_SIZE, page_size);
}

/// Cross-platform hardlink creation
fn createHardlink(ctx: *CliCtx, source: []const u8, dest: []const u8) (Allocator.Error || error{ InvalidUtf8, PathAlreadyExists, Unexpected })!void {
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

        if (kernel32.CreateHardLinkW(dest_w, source_w, null) == .FALSE) {
            const err = std.os.windows.GetLastError();
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
fn generateRandomSuffix(ctx: *CliCtx) Allocator.Error![]u8 {
    // TODO: Consider switching to a library like https://github.com/abhinav/temp.zig
    // for more robust temporary file/directory handling
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    const suffix = try ctx.arena.alloc(u8, 32);

    // Fill with cryptographically secure random bytes
    ctx.io.std_io.random(suffix);

    // Convert to ASCII characters from our charset
    for (suffix) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }

    return suffix;
}

/// Create a unique temporary directory under roc/{version}/{random}/.
/// Returns the path to the directory (allocated from arena, no need to free).
/// Uses system temp directory to avoid race conditions when cache is cleared.
pub fn createUniqueTempDir(ctx: *CliCtx) anyerror![]const u8 {
    // Get the version-specific temp directory: {temp}/roc/{version}
    const version_temp_dir = try cache_config_mod.getVersionTempDir(ctx.coreCtx(), ctx.arena);

    // Ensure the roc/{version} directory exists
    // makePath automatically handles PathAlreadyExists internally
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, version_temp_dir);

    // Try to create a unique subdirectory with random suffix
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(ctx);
        const dir_path = try std.fs.path.join(ctx.arena, &.{ version_temp_dir, random_suffix });

        // Try to create the directory
        std.Io.Dir.cwd().createDir(ctx.io.std_io, dir_path, .default_dir) catch |err| switch (err) {
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
pub fn writeFdCoordinationFile(ctx: *CliCtx, temp_exe_path: []const u8, shm_handle: SharedMemoryHandle) anyerror!void {
    // The coordination file is at {temp_dir}.txt where temp_dir is the directory containing the exe
    const temp_dir = std.fs.path.dirname(temp_exe_path) orelse return error.InvalidPath;

    // Ensure we have no trailing slashes
    var dir_path = temp_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = try std.fmt.allocPrint(ctx.arena, "{s}.txt", .{dir_path});

    // Create the file (exclusive - fail if exists to detect collisions)
    const fd_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, fd_file_path, .{ .exclusive = true }) catch |err| {
        // Error is handled by caller with ctx.fail()
        return err;
    };
    defer fd_file.close(ctx.io.std_io);

    // Write shared memory info to file
    const fd_str = try std.fmt.allocPrint(ctx.arena, "{}\n{}", .{ shm_handle.fd, shm_handle.size });
    try fd_file.writeStreamingAll(ctx.io.std_io, fd_str);
    try fd_file.sync(ctx.io.std_io);
}

/// Create the temporary directory structure for fd communication.
/// Returns the path to the executable in the temp directory (allocated from arena, no need to free).
/// Uses the standard roc/{version}/{random}/ structure in the system temp directory.
/// The exe_display_name is the name that will appear in `ps` output (e.g., "app.roc").
pub fn createTempDirStructure(ctx: *CliCtx, exe_path: []const u8, exe_display_name: []const u8, shm_handle: SharedMemoryHandle, _: ?[]const u8) Allocator.Error![]const u8 {
    // Get the version-specific temp directory: {temp}/roc/{version}
    const version_temp_dir = try cache_config_mod.getVersionTempDir(ctx.coreCtx(), ctx.arena);

    // Ensure the roc/{version} directory exists
    // makePath automatically handles PathAlreadyExists internally
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, version_temp_dir);

    // Try to create a unique subdirectory with random suffix
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(ctx);
        const temp_dir_path = try std.fs.path.join(ctx.arena, &.{ version_temp_dir, random_suffix });

        // The coordination file path is the directory path with .txt appended
        const dir_name_with_txt = try std.fmt.allocPrint(ctx.arena, "{s}.txt", .{temp_dir_path});

        // Try to create the directory
        std.Io.Dir.cwd().createDir(ctx.io.std_io, temp_dir_path, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // Directory already exists, try again with a new random suffix
                continue;
            },
            else => {
                return err;
            },
        };

        // Try to create the fd file
        const fd_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, dir_name_with_txt, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // File already exists, remove the directory and try again
                std.Io.Dir.cwd().deleteDir(ctx.io.std_io, temp_dir_path) catch {};
                continue;
            },
            else => {
                // Clean up directory on other errors
                std.Io.Dir.cwd().deleteDir(ctx.io.std_io, temp_dir_path) catch {};
                return err;
            },
        };
        // Note: We'll close this explicitly later, before spawning the child

        // Write shared memory info to file (POSIX only - Windows uses command line args)
        const fd_str = try std.fmt.allocPrint(ctx.arena, "{}\n{}", .{ shm_handle.fd, shm_handle.size });

        try fd_file.writeStreamingAll(ctx.io.std_io, fd_str);

        // IMPORTANT: Flush and close the file explicitly before spawning child process
        // On Windows, having the file open can prevent child process access
        try fd_file.sync(ctx.io.std_io); // Ensure data is written to disk
        fd_file.close(ctx.io.std_io);

        // Create hardlink to executable in temp directory with display name
        const temp_exe_path = try std.fs.path.join(ctx.arena, &.{ temp_dir_path, exe_display_name });

        // Try to create a hardlink first (more efficient than copying)
        createHardlink(ctx, exe_path, temp_exe_path) catch {
            // If hardlinking fails for any reason, fall back to copying
            // Common reasons: cross-device link, permissions, file already exists
            try std.Io.Dir.cwd().copyFile(exe_path, std.Io.Dir.cwd(), temp_exe_path, ctx.io.std_io, .{});
        };

        return temp_exe_path;
    }

    // Failed after 6 attempts
    return error.FailedToCreateUniqueTempDir;
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .{
    .backing_allocator = std.heap.page_allocator,
};

fn renderValidationError(
    allocator: std.mem.Allocator,
    result: platform_validation.ValidationResult,
    stderr: anytype,
) void {
    const rendered = platform_validation.renderValidationError(allocator, result, stderr);
    if (rendered) {} else {}
}

fn renderDiagnostics(build_env: *BuildEnv, stderr: anytype) Allocator.Error!void {
    const diag = try build_env.renderDiagnostics(stderr);
    if (diag.errors > 0) {} else {}
}

/// The CLI entrypoint for the Roc compiler.
pub fn main(init: std.process.Init) Allocator.Error!void {
    // Initialize the debug IO with a real allocator so std.Options.debug_io
    // can spawn processes, create directories, etc.
    debug_threaded_io_instance = .init(init.gpa, .{
        .argv0 = .init(init.minimal.args),
        .environ = init.minimal.environ,
    });
    defer debug_threaded_io_instance.deinit();

    // Install stack overflow handler early, before any significant work.
    // This gives us a helpful error message instead of a generic segfault
    // if the compiler blows the stack (e.g., due to infinite recursion in type translation).
    const stack_overflow_installed = base.stack_overflow.installForCurrentThread();
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(stack_overflow_installed);
    } else if (!stack_overflow_installed) {
        unreachable;
    }

    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa, const is_safe = gpa: {
        if (builtin.os.tag == .freestanding) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
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

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var args_list: std.ArrayList([]const u8) = .empty;
    defer args_list.deinit(arena);
    var args_iter = try init.minimal.args.iterateAllocator(arena);
    while (args_iter.next()) |arg| {
        try args_list.append(arena, arg);
    }
    const args = args_list.items;

    mainArgs(gpa, arena, args, init.io) catch |err| {
        // Handle OutOfMemory specially - it may not have been printed
        switch (err) {
            error.OutOfMemory => {
                // Use std.debug.print to stderr since we don't have access to ctx.io here
                // TODO: if virtual address allocation fails at 4gb, fall back on doing `roc build` followed by manually running the executable
                std.debug.print("The Roc compiler ran out of memory trying to preallocate virtual address space for compiling and running this program. Try using `roc build` to build the executable separately, then run it manually.\n", .{});
            },
            else => {
                // All other errors: problems were already recorded/rendered by the
                // command handlers; exit cleanly below without a stack trace.
            },
        }
        // Exit cleanly without showing a stack trace to the user.
        if (tracy.enable) {
            tracy.waitForShutdown(init.io);
        }
        restoreWindowsConsoleCodePage();
        std.process.exit(1);
    };

    if (tracy.enable) {
        tracy.waitForShutdown(init.io);
    }
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8, std_io: std.Io) anyerror!void {
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
    if (compile.CacheCleanup.startBackgroundCleanup(std.heap.page_allocator, CoreCtx.default(std.heap.page_allocator, std.heap.page_allocator, std_io))) |_| {
        // Thread started successfully, will run in background
    } else |_| {
        // Non-fatal: cleanup failure shouldn't prevent compilation
        std.log.debug("Failed to start background cleanup thread", .{});
    }

    // Create I/O interface - this is passed to all command handlers via ctx
    var io = Io.create(std_io);

    const parsed_args = try cli_args.parse(arena, std_io, args[1..]);

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
    var ctx = CliCtx.init(gpa, arena, &io, command);
    ctx.initIo(); // Must be called after ctx is at its final stack location
    defer ctx.deinit(); // deinit flushes I/O

    try switch (parsed_args) {
        .run => |run_args| {
            if (std.mem.eql(u8, run_args.path, "main.roc")) {
                std.Io.Dir.cwd().access(ctx.io.std_io, run_args.path, .{}) catch |err| switch (err) {
                    error.FileNotFound => {
                        const cwd_path = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", arena) catch |real_err| {
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
        .experimental_lsp => |lsp_args| try lsp.runWithStdIo(gpa, std_io, .{
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
/// If `lir_image` is present, embed the already-lowered LIR image
/// and call the interpreter shim entrypoint that views the image directly.
/// If debug is true, include debug information in the generated object file.
fn generatePlatformHostShim(
    ctx: *CliCtx,
    cache_dir: []const u8,
    entrypoint_names: []const []const u8,
    target: RocTarget,
    lir_image: ?[]const u8,
    debug: bool,
) (Allocator.Error || error{ CliError, LLVMCompilationFailed })!?[]const u8 {
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
    const std_target = std.zig.system.resolveTargetQuery(ctx.io.std_io, query) catch |err| {
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

    // Create the complete platform shim.
    // Note: Symbol names include platform-specific prefixes (underscore for macOS).
    if (lir_image) |image| {
        platform_host_shim.createEmbeddedInterpreterShim(&llvm_builder, entrypoints.items, target, image) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };
    } else {
        platform_host_shim.createInterpreterShim(&llvm_builder, entrypoints.items, target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };
    }

    // Generate paths for temporary files
    var hash = std.hash.Crc32.init();
    if (lir_image) |image| hash.update(image);
    for (entrypoint_names) |name| {
        hash.update(name);
        hash.update(&[_]u8{0});
    }
    hash.update(target.toTriple());
    hash.update(if (debug) "debug" else "nodebug");
    const content_hash = hash.final();

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
    const bc_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, bitcode_path, .{}) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = bitcode_path, .err = err } });
    };
    defer bc_file.close(ctx.io.std_io);

    // Convert u32 array to bytes for writing
    const bytes = std.mem.sliceAsBytes(bitcode);
    bc_file.writeStreamingAll(ctx.io.std_io, bytes) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = bitcode_path, .err = err } });
    };

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .speed,
        .target = target,
        .debug = debug, // Use the debug flag passed from caller
    };

    if (builder.compileBitcodeToObject(ctx.gpa, ctx.io.std_io, compile_config)) |success| {
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

fn ensureCompilerCacheDirExists(std_io: std.Io, path: []const u8) anyerror!void {
    // This helper is only for compiler-owned internal cache directories.
    // User-facing output paths should still fail normally if the parent directory is missing.
    std.Io.Dir.cwd().createDirPath(std_io, path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

fn interpreterExeCacheName(
    ctx: *CliCtx,
    app_path: []const u8,
    target: RocTarget,
    entrypoint_names: []const []const u8,
) (Allocator.Error || error{CliError})![]const u8 {
    var hash = std.hash.Crc32.init();
    hash.update("roc-run-lir-shared-memory-v1");
    hash.update(build_options.compiler_version);
    const shim_digest = interpreterShimDigest(target);
    hash.update(&shim_digest);
    hash.update(app_path);
    hash.update(@tagName(target));
    for (entrypoint_names) |name| {
        hash.update(&[_]u8{0});
        hash.update(name);
    }
    return std.fmt.allocPrint(ctx.arena, "roc_{x}", .{hash.final()}) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };
}

fn rejectRunTargetNotExecutable(ctx: *CliCtx, target: RocTarget) error{ WriteFailed, UnsupportedTarget }!void {
    const native_target = RocTarget.detectNative();
    try ctx.io.stderr().print(
        "Error: unsupported target for roc run: {s} cannot be executed on this host ({s}).\n\nUse `roc build --target={s}` to produce an artifact for that target.\n",
        .{ @tagName(target), @tagName(native_target), @tagName(target) },
    );
    return error.UnsupportedTarget;
}

fn rocRun(ctx: *CliCtx, args: cli_args.RunArgs) anyerror!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check if this is a default_app (headerless file with main!) before
    // linking the platform host shim.
    if (try readDefaultAppSource(ctx, args.path)) |source| {
        return rocRunDefaultApp(ctx, args, source);
    }

    // Initialize cache - used to store our shim, and linked interpreter executables in cache
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());

    // Create cache directory for linked interpreter executables
    const exe_cache_dir = cache_manager.config.getExeCacheDir(ctx.arena) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    ensureCompilerCacheDirExists(ctx.io.std_io, exe_cache_dir) catch |err| switch (err) {
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

    // Create unique temp directory for this build (uses PID for uniqueness)
    const temp_dir_path = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };

    // The executable is built directly in the temp dir with the display name
    const exe_path = std.fs.path.join(ctx.arena, &.{ temp_dir_path, exe_display_name_with_ext }) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    // First, parse the app file to get the platform reference
    const platform_spec = try extractPlatformSpecFromApp(ctx, args.path);

    // Resolve platform paths from the platform spec (relative to app file directory)
    const app_dir = std.fs.path.dirname(args.path) orelse ".";
    const platform_paths = try resolvePlatformSpecToPaths(ctx, platform_spec, app_dir);

    // Validate platform header and get link spec
    var link_spec: ?roc_target.TargetLinkSpec = null;
    var targets_config: ?roc_target.TargetsConfig = null;
    if (platform_paths.platform_source_path) |platform_source| {
        if (platform_validation.validatePlatformHeader(ctx.arena, ctx.io.std_io, platform_source)) |validation| {
            targets_config = validation.config;

            // Check if this is a static_lib-only platform (no exe targets)
            if (validation.config.exe.len == 0 and validation.config.static_lib.len > 0) {
                ctx.io.stderr().print("Error: This platform only produces static libraries.\n\n", .{}) catch {};
                ctx.io.stderr().print("Static library platforms produce .a/.lib/.wasm files that must be\n", .{}) catch {};
                ctx.io.stderr().print("linked by a host application. Use 'roc build' instead to produce\n", .{}) catch {};
                ctx.io.stderr().print("the library artifact.\n", .{}) catch {};
                return error.UnsupportedTarget;
            }

            // Select target: if --target is provided, use that; otherwise try native then the next supported linker path.
            if (args.target) |target_str| {
                // User explicitly specified a target
                const parsed_target = RocTarget.fromString(target_str) orelse {
                    const result = platform_validation.targets_validator.ValidationResult{
                        .invalid_target = .{ .target_str = target_str },
                    };
                    renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.InvalidTarget;
                };

                if (validation.config.getLinkSpec(parsed_target, .exe)) |spec| {
                    if (!parsed_target.isExecutableOnHost()) {
                        return rejectRunTargetNotExecutable(ctx, parsed_target);
                    }
                    link_spec = spec;
                } else {
                    const result = platform_validation.createUnsupportedTargetResult(
                        platform_source,
                        parsed_target,
                        .exe,
                        validation.config,
                    );
                    renderValidationError(ctx.gpa, result, ctx.io.stderr());
                    return error.UnsupportedTarget;
                }
            } else {
                // No --target provided: use the first exe target that can run on this host.
                if (validation.config.getDefaultHostExecutableTarget(.exe)) |compatible_target| {
                    link_spec = validation.config.getLinkSpec(compatible_target, .exe);
                } else {
                    // No compatible exe target found
                    const native_target = RocTarget.detectNative();
                    const result = platform_validation.createUnsupportedTargetResult(
                        platform_source,
                        native_target,
                        .exe,
                        validation.config,
                    );
                    renderValidationError(ctx.gpa, result, ctx.io.stderr());
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

    // Build the viewable LIR image in shared memory before linking the
    // host executable. The same lowered root metadata supplies the platform
    // entrypoint names used by the shim, so `roc run` does not rediscover roots
    // from platform source syntax after checking.
    const shm_result = try buildLirImageWithCoordinator(ctx, args.path, null, args.max_threads);
    const shm_handle = shm_result.handle;
    defer closeSharedMemoryHandle(shm_handle);

    if (shm_result.error_count > 0) {
        if (args.allow_errors) return;
        return error.TypeCheckingFailed;
    }

    const entrypoint_names = shm_result.entrypoint_names;
    if (entrypoint_names.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("roc run invariant violated: no platform entrypoints in checked LIR root metadata", .{});
        }
        unreachable;
    }

    const selected_target = validated_link_spec.target;
    const exe_cache_name = try interpreterExeCacheName(ctx, args.path, selected_target, entrypoint_names);
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

    // Check if the interpreter executable already exists in cache
    const cache_exists = if (args.no_cache) false else blk: {
        std.Io.Dir.cwd().access(ctx.io.std_io, exe_cache_path, .{}) catch {
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
            std.Io.Dir.cwd().copyFile(exe_cache_path, std.Io.Dir.cwd(), exe_path, ctx.io.std_io, .{}) catch |copy_err| {
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
        extractReadRocFilePathShimLibrary(ctx, shim_path, selected_target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };

        // Generate platform host shim using the published checked-artifact entrypoints
        // Use temp dir to avoid race conditions when multiple processes run in parallel
        // Auto-enable debug when roc is built in debug mode (no explicit --debug flag for roc run)
        const platform_shim_path = try generatePlatformHostShim(ctx, temp_dir_path, entrypoint_names, selected_target, null, builtin.mode == .Debug);

        // Link the host.a with our shim to create the interpreter executable using our linker
        // Try LLD first, then clang if LLVM is not available.
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
            .scratch_dir = temp_dir_path,
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
        std.Io.Dir.cwd().deleteFile(ctx.io.std_io, exe_cache_path) catch |err| switch (err) {
            error.FileNotFound => {}, // OK, doesn't exist
            else => std.log.debug("Could not delete existing cache file: {}", .{err}),
        };
        createHardlink(ctx, exe_path, exe_cache_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink to cache failed, copying: {}", .{err});
            std.Io.Dir.cwd().copyFile(exe_path, std.Io.Dir.cwd(), exe_cache_path, ctx.io.std_io, .{}) catch |copy_err| {
                // Non-fatal - just means future runs won't be cached
                std.log.debug("Failed to copy to cache: {}", .{copy_err});
            };
        };
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

const NativeRunTermination = union(enum) {
    success,
    exit_code: u8,
    signal: std.posix.SIG,
    stopped: std.posix.SIG,
    unknown: u32,
};

fn classifyNativeRunTermination(term: std.process.Child.Term, warning_count: usize) NativeRunTermination {
    return switch (term) {
        .exited => |code| if (code != 0)
            .{ .exit_code = code }
        else if (warning_count > 0)
            .{ .exit_code = 2 }
        else
            .success,
        .signal => |signal| .{ .signal = signal },
        .stopped => |signal| .{ .stopped = signal },
        .unknown => |status| .{ .unknown = status },
    };
}

/// Check if a file is a default_app (headerless file with a main! function).
/// On success, returns the file source (caller owns the allocation).
/// Returns null if the file is not a default_app.
fn readDefaultAppSource(ctx: *CliCtx, file_path: []const u8) std.mem.Allocator.Error!?[]const u8 {
    const max_source_size = 256 * 1024 * 1024; // 256 MB
    const source = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, file_path, ctx.gpa, .limited(max_source_size)) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        // Any other read failure (e.g. file not found) means this isn't a
        // default app to handle here; fall through to the normal path.
        else => return null,
    };

    const module_name = base.module_path.getModuleNameAlloc(ctx.arena, file_path) catch |err| switch (err) {
        error.OutOfMemory => {
            ctx.gpa.free(source);
            return error.OutOfMemory;
        },
    };

    var env = ModuleEnv.init(ctx.gpa, source) catch |err| switch (err) {
        error.OutOfMemory => {
            ctx.gpa.free(source);
            return error.OutOfMemory;
        },
    };
    defer env.deinit();
    env.common.source = source;
    env.module_name = module_name;

    const ast = parse.parse(ctx.gpa, &env.common) catch |err| switch (err) {
        error.OutOfMemory => {
            ctx.gpa.free(source);
            return error.OutOfMemory;
        },
        error.TooNested => {
            ctx.gpa.free(source);
            return null;
        },
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

/// State for the CLI echo platform's virtual I/O context.
/// Intercepts reads for the synthetic app source and embedded platform files.
/// Run a default_app (headerless file with main! and echo platform).
/// This compiles the app through checked artifacts and executes the resulting
/// LIR image with the echo platform host function.
fn rocRunDefaultApp(ctx: *CliCtx, args: cli_args.RunArgs, original_source: []const u8) anyerror!void {
    defer ctx.gpa.free(original_source);

    // Write synthetic app + echo platform files into a unique temp directory.
    // The coordinator reads platform/module files from disk, so the synthetic
    // sources must exist on the filesystem (a `cliEchoReadFile` filesystem
    // override only helps build_env paths, not the coordinator spawned by
    // `buildLirRuntimeImageWithCoordinator`).
    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        ctx.io.stderr().print("error: failed to create temp dir: {}\n", .{err}) catch {};
        return err;
    };
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, temp_dir) catch {};

    const platform_dir = std.fs.path.join(ctx.arena, &.{ temp_dir, ".roc_echo_platform" }) catch return error.OutOfMemory;
    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, platform_dir) catch |err| {
        ctx.io.stderr().print("error: failed to create platform dir {s}: {}\n", .{ platform_dir, err }) catch {};
        return err;
    };

    const app_path = std.fs.path.join(ctx.arena, &.{ temp_dir, "main.roc" }) catch return error.OutOfMemory;
    const platform_main_path = std.fs.path.join(ctx.arena, &.{ platform_dir, "main.roc" }) catch return error.OutOfMemory;
    const echo_module_path = std.fs.path.join(ctx.arena, &.{ platform_dir, "Echo.roc" }) catch return error.OutOfMemory;

    const header =
        "app [main!] { pf: platform \"./.roc_echo_platform/main.roc\" }\n\n" ++
        "import pf.Echo\n\n" ++
        "echo! = |msg| Echo.line!(msg)\n\n";
    const synthetic_source = std.mem.concat(ctx.gpa, u8, &.{ header, original_source }) catch return error.OutOfMemory;
    defer ctx.gpa.free(synthetic_source);

    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = app_path, .data = synthetic_source }) catch |err| {
        ctx.io.stderr().print("error: failed to write {s}: {}\n", .{ app_path, err }) catch {};
        return err;
    };
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = platform_main_path, .data = echo_platform.platform_main_source }) catch |err| {
        ctx.io.stderr().print("error: failed to write {s}: {}\n", .{ platform_main_path, err }) catch {};
        return err;
    };
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = echo_module_path, .data = echo_platform.echo_module_source }) catch |err| {
        ctx.io.stderr().print("error: failed to write {s}: {}\n", .{ echo_module_path, err }) catch {};
        return err;
    };

    const original_source_dir = std.fs.path.dirname(args.path) orelse ".";
    const shm_result = try buildLirImageWithCoordinator(ctx, app_path, original_source_dir, args.max_threads);
    defer closeSharedMemoryHandle(shm_result.handle);

    if (shm_result.error_count > 0) {
        if (args.allow_errors) return;
        return error.TypeCheckingFailed;
    }

    const view = try viewLirImageFromHandle(shm_result.handle);

    var hosted_fn_array = [_]echo_platform.host_abi.HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var echo_env = echo_platform.EchoEnv{ .std_io = ctx.io.std_io };
    var roc_ops = echo_platform.makeDefaultRocOps(&echo_env, &hosted_fn_array);
    var cli_args_list = try echo_platform.buildCliArgs(args.app_args, &roc_ops);

    var result_buf: [16]u8 align(16) = undefined;
    try evaluateLirImageEntrypoint(
        ctx.gpa,
        &view,
        0,
        &roc_ops,
        @ptrCast(&result_buf),
        @ptrCast(&cli_args_list),
    );

    const exit_code = result_buf[0];
    if (exit_code != 0) std.process.exit(exit_code);
    if (echo_env.inline_expect_failed) std.process.exit(1);
}

/// Append an argument to a command line buffer with proper Windows quoting.
/// Windows command line parsing rules:
/// - Arguments containing spaces, tabs, or quotes must be quoted
/// - Embedded quotes must be escaped with backslash: " -> \"
/// - Backslashes before quotes must be doubled: \" -> \\"
fn appendWindowsQuotedArg(cmd_builder: *std.array_list.Managed(u8), arg: []const u8) Allocator.Error!void {
    const needs_quoting = arg.len == 0 or std.mem.findAny(u8, arg, " \t\"") != null;

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
fn runWithWindowsHandleInheritance(ctx: *CliCtx, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) (CliError || error{OutOfMemory})!void {
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

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.arena) catch {
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
    try cmd_builder.print("\"{s}\" {} {}", .{ exe_path, handle_uint, shm_handle.size });
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
        const last_error = std.os.windows.GetLastError();
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
        compile.CacheCleanup.deleteTempDir(ctx.arena, ctx.coreCtx(), temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

    // Check exit code and propagate to parent
    if (exit_code != 0) {
        std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
        if (exit_code == 0xC0000005) { // STATUS_ACCESS_VIOLATION
            const result = platform_validation.targets_validator.ValidationResult{
                .process_crashed = .{ .exit_code = exit_code, .is_access_violation = true },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
        } else if (exit_code >= 0xC0000000) { // NT status codes for exceptions
            const result = platform_validation.targets_validator.ValidationResult{
                .process_crashed = .{ .exit_code = exit_code, .is_access_violation = false },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
        }
        // Propagate the exit code (truncated to u8 for compatibility)
        std.process.exit(@truncate(exit_code));
    }

    std.log.debug("Child process completed successfully", .{});
}

/// Run child process using POSIX file descriptor inheritance (existing approach for Unix)
/// The exe_path should already be in a unique temp directory created by createUniqueTempDir.
fn runWithPosixFdInheritance(ctx: *CliCtx, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) (CliError || error{OutOfMemory})!void {
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
    const current_flags = std.c.fcntl(shm_handle.fd, std.c.F.GETFD);
    if (current_flags == -1) {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "get fd flags",
            .err = error.FdConfigFailed,
        } });
    }

    // Clear FD_CLOEXEC - the flag value is 1
    const new_flags = current_flags & ~@as(c_int, 1);
    if (std.c.fcntl(shm_handle.fd, std.c.F.SETFD, new_flags) == -1) {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "set fd flags",
            .err = error.FdConfigFailed,
        } });
    }

    // Debug-only verification that fd flags were actually cleared
    if (comptime builtin.mode == .Debug) {
        const verify_flags = std.c.fcntl(shm_handle.fd, std.c.F.GETFD);
        if (verify_flags == -1) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "verify fd flags",
                .err = error.FdConfigFailed,
            } });
        }
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

    std.log.debug("Spawning child process: {s} with {} app args", .{ exe_path, app_args.len });
    std.log.debug("Child process inherits current working directory", .{});
    var child = std.process.spawn(ctx.io.std_io, .{
        .argv = argv,
        .cwd = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    }) catch |err| {
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };
    std.log.debug("Child process spawned successfully (PID: {})", .{child.id});

    // Wait for child to complete
    const term = child.wait(ctx.io.std_io) catch |err| {
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
        compile.CacheCleanup.deleteTempDir(ctx.arena, ctx.coreCtx(), temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

    // Check the termination status
    switch (term) {
        .exited => |exit_code| {
            if (exit_code == 0) {
                std.log.debug("Child process completed successfully", .{});
            } else {
                // Propagate the exit code from the child process to our parent
                std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
                std.process.exit(exit_code);
            }
        },
        .signal => |signal| {
            const sig_num = @intFromEnum(signal);
            std.log.debug("Child process {s} killed by signal: {}", .{ exe_path, sig_num });
            const result = platform_validation.targets_validator.ValidationResult{
                .process_signaled = .{ .signal = sig_num },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            // Standard POSIX convention: exit with 128 + signal number
            std.process.exit(128 +| @as(u8, @truncate(sig_num)));
        },
        .stopped => |signal| {
            return ctx.fail(.{ .child_process_signaled = .{
                .command = exe_path,
                .signal = @intFromEnum(signal),
            } });
        },
        .unknown => |status| {
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
    entrypoint_names: []const []const u8,
    error_count: usize,
    warning_count: usize,
};

const CoordinatorReportCounts = struct {
    errors: usize,
    warnings: usize,
};

fn renderCoordinatorReports(ctx: *CliCtx, coord: *Coordinator, roc_file_path: []const u8) CoordinatorReportCounts {
    var counts = CoordinatorReportCounts{ .errors = 0, .warnings = 0 };

    var it = coord.iterReports();
    while (it.next()) |entry| {
        const rep = entry.report;
        if (rep.severity == .fatal or rep.severity == .runtime_error) {
            counts.errors += 1;
            if (!builtin.is_test) {
                reporting.renderReportToTerminal(rep, ctx.io.stderr(), ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch {};
            }
        } else if (rep.severity == .warning) {
            counts.warnings += 1;
            if (!builtin.is_test) {
                reporting.renderReportToTerminal(rep, ctx.io.stderr(), ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch {};
            }
        }
    }

    if (counts.errors > 0 or counts.warnings > 0) {
        const stderr = ctx.io.stderr();
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) for {s}.\n", .{
            counts.errors,
            counts.warnings,
            roc_file_path,
        }) catch {};
    }

    ctx.io.flush();
    return counts;
}

fn sharedMemoryResult(
    shm: *SharedMemoryAllocator,
    counts: CoordinatorReportCounts,
    entrypoint_names: []const []const u8,
) SharedMemoryResult {
    return .{
        .handle = .{
            .fd = shm.handle,
            .ptr = shm.base_ptr,
            .size = shm.getUsedSize(),
            .mapped_size = shm.total_size,
        },
        .entrypoint_names = entrypoint_names,
        .error_count = counts.errors,
        .warning_count = counts.warnings,
    };
}

fn closeSharedMemoryHandle(handle: SharedMemoryHandle) void {
    if (comptime is_windows) {
        _ = ipc.platform.windows.UnmapViewOfFile(handle.ptr);
        _ = ipc.platform.windows.CloseHandle(@ptrCast(handle.fd));
    } else {
        _ = posix.munmap(handle.ptr, handle.mapped_size);
        if (c.close(handle.fd) != 0) {}
    }
}

fn viewLirImageFromHandle(handle: SharedMemoryHandle) lir.LirImage.ImageError!lir.LirImage.ProgramView {
    const base_ptr: [*]align(1) u8 = @ptrCast(@alignCast(handle.ptr));
    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(base_ptr + @sizeOf(SharedMemoryAllocator.Header)));
    return lir.LirImage.viewMappedImage(header, base_ptr, handle.size);
}

fn argLayoutsForProc(
    allocator: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) Allocator.Error![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout.Idx, arg_ids.len);
    errdefer allocator.free(arg_layouts);

    for (arg_ids, 0..) |local_id, i| {
        arg_layouts[i] = store.locals.items[@intFromEnum(local_id)].layout_idx;
    }

    return arg_layouts;
}

fn reportCliInterpreterError(ops: *echo_platform.host_abi.RocOps, interpreter: *const eval.LirInterpreter, err: eval.LirInterpreter.Error) void {
    const message = switch (err) {
        error.OutOfMemory => "Roc interpreter ran out of memory",
        error.RuntimeError => interpreter.getRuntimeErrorMessage() orelse "Roc runtime error",
        error.DivisionByZero => interpreter.getRuntimeErrorMessage() orelse "Division by zero",
        error.Crash => return,
    };
    ops.crash(message);
}

fn evaluateLirImageEntrypoint(
    allocator: Allocator,
    view: *const lir.LirImage.ProgramView,
    ordinal: u32,
    ops: *echo_platform.host_abi.RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) Allocator.Error!void {
    var interpreter = try eval.LirInterpreter.init(allocator, &view.store, &view.layouts, ops);
    defer interpreter.deinit();

    _ = interpreter.runEntrypoint(view, ordinal, arg_ptr, ret_ptr) catch |err| switch (err) {
        error.EntrypointNotFound => {
            if (builtin.mode == .Debug) {
                std.debug.panic("CLI LIR image invariant violated: missing platform entrypoint ordinal {d}", .{ordinal});
            }
            unreachable;
        },
        else => |e| {
            reportCliInterpreterError(ops, &interpreter, e);
            return;
        },
    };
}

/// Build shared memory containing a viewable ARC-inserted LIR image.
///
/// The parent process owns parse, canonicalization, checking, checked module
/// publication, post-check lowering, LIR lowering, and ARC insertion. The child
/// process maps only the LIR image and never sees `ModuleEnv`, CIR, checked
/// modules, or post-check IRs.
pub fn buildLirImageWithCoordinator(
    ctx: *CliCtx,
    roc_file_path: []const u8,
    source_dir_override: ?[]const u8,
    max_threads: ?usize,
) anyerror!SharedMemoryResult {
    // Create shared memory with SharedMemoryAllocator, trying progressively smaller
    // sizes if larger ones fail (e.g., due to valgrind or overcommit-disabled Linux)
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try createSharedMemory(ctx.io.std_io, page_size);
    // Don't defer deinit here - we need to keep the shared memory alive

    const shm_allocator = shm.allocator();
    const image_header = try shm_allocator.create(lir.LirImage.Header);

    var builtin_modules = try eval.BuiltinModules.init(ctx.gpa);
    defer builtin_modules.deinit();

    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";

    // Parse the app header
    const header_info = compile.app_header.parseAppHeader(
        ctx.coreCtx(),
        ctx.gpa,
        ctx.arena,
        roc_file_path,
    ) catch |err| switch (err) {
        error.NotAnAppHeader => return ctx.fail(.{ .expected_app_header = .{
            .path = roc_file_path,
            .found = "non-app header",
        } }),
        error.FileNotFound => return ctx.fail(.{ .file_not_found = .{
            .path = roc_file_path,
            .context = .source_file,
        } }),
        else => |e| return e,
    };
    try validatePlatformSpec(ctx, header_info.platform_spec);

    // Resolve the platform spec to a local main.roc path (handles URL fetch).
    const platform_main_path: ?[]const u8 = if (std.mem.startsWith(u8, header_info.platform_spec, "./") or std.mem.startsWith(u8, header_info.platform_spec, "../"))
        try std.fs.path.join(ctx.arena, &[_][]const u8{ app_dir, header_info.platform_spec })
    else if (base.url.isSafeUrl(header_info.platform_spec)) blk: {
        const platform_paths = resolveUrlPlatform(ctx, header_info.platform_spec) catch |err| switch (err) {
            error.CliError => break :blk null,
            error.OutOfMemory => return error.OutOfMemory,
        };
        break :blk platform_paths.platform_source_path;
    } else null;

    const platform_dir: ?[]const u8 = if (platform_main_path) |p|
        std.fs.path.dirname(p) orelse return error.InvalidPlatformPath
    else
        null;

    const thread_count: usize = max_threads orelse (std.Thread.getCpuCount() catch 1);
    const mode: Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    var coord = try Coordinator.init(
        ctx.gpa,
        mode,
        thread_count,
        RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null, // no cache for IPC
        ctx.coreCtx(),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;

    try coord.start();

    const app_pkg = try coord.ensurePackage("app", app_dir);
    const app_module_name = base.module_path.getModuleName(roc_file_path);
    const app_module_id = try app_pkg.ensureModule(ctx.gpa, app_module_name, roc_file_path);
    if (source_dir_override) |source_dir| {
        app_pkg.modules.items[app_module_id].source_dir_override = try ctx.gpa.dupe(u8, source_dir);
    }
    app_pkg.root_module_id = app_module_id;
    app_pkg.modules.items[app_module_id].depth = 0;
    app_pkg.remaining_modules += 1;
    coord.total_remaining += 1;

    if (platform_dir) |pf_dir| {
        if (platform_main_path) |pmp| {
            try coord.registerPlatformPackage(app_pkg, pf_dir, pmp, header_info.platform_qualifier);
        } else if (header_info.platform_qualifier) |qual| {
            // URL platform that failed to resolve — keep the shorthand wired so
            // downstream code reports a clean error rather than a missing-import.
            try app_pkg.shorthands.put(
                try ctx.gpa.dupe(u8, qual),
                try ctx.gpa.dupe(u8, "pf"),
            );
            _ = try coord.ensurePackage("pf", pf_dir);
        }
    }

    // Resolve and register non-platform packages (URL-aware path).
    for (header_info.non_platform_packages) |entry| {
        const pkg_abs_path = if (base.url.isSafeUrl(entry.spec)) blk: {
            const cached = resolveUrlBundle(ctx, entry.spec) catch |err| switch (err) {
                error.CliError => return error.CliError,
                error.OutOfMemory => return error.OutOfMemory,
            };
            break :blk try ctx.arena.dupe(u8, cached);
        } else try std.fs.path.join(ctx.arena, &.{ app_dir, entry.spec });
        const pkg_dir = std.fs.path.dirname(pkg_abs_path) orelse ".";
        _ = try coord.registerInlinePackage(entry.shorthand, pkg_dir, app_pkg, entry.shorthand);
    }

    try coord.enqueueParseTask("app", app_module_id);
    coord.coordinatorLoop() catch |err| {
        _ = renderCoordinatorReports(ctx, &coord, roc_file_path);
        return err;
    };

    const counts = renderCoordinatorReports(ctx, &coord, roc_file_path);
    if (counts.errors > 0) {
        shm.updateHeader();
        return sharedMemoryResult(&shm, counts, &.{});
    }

    try coord.finalizeExecutableArtifacts();
    const finalized_counts = renderCoordinatorReports(ctx, &coord, roc_file_path);
    if (finalized_counts.errors > 0) {
        shm.updateHeader();
        return sharedMemoryResult(&shm, finalized_counts, &.{});
    }

    const root_artifact = coord.executableRootCheckedArtifact();
    const imported_artifacts = try coord.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try coord.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    const lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        shm_allocator,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{ .requests = root_artifact.root_requests.runtime_requests },
        .{
            .target_usize = base.target.TargetUsize.native,
        },
    );

    const platform_entrypoints = try lowered.platformEntrypoints(shm_allocator);
    const entrypoint_names = try lowered.platformEntrypointNames(ctx.arena, root_artifact);

    try lir.LirImage.fillHeaderInSharedMemory(
        image_header,
        shm.base_ptr,
        shm.getUsedSize(),
        &lowered.lir_result,
        lowered.target_usize,
        platform_entrypoints,
    );

    shm.updateHeader();
    return sharedMemoryResult(&shm, finalized_counts, entrypoint_names);
}

/// Wrapper around buildLirImageWithCoordinator for callers that pass allow_errors.
/// The allow_errors flag is handled by the caller; this function ignores it.
pub fn setupSharedMemoryWithCoordinator(ctx: *CliCtx, roc_file_path: []const u8, _: bool) anyerror!SharedMemoryResult {
    return buildLirImageWithCoordinator(ctx, roc_file_path, null, null);
}

/// Platform resolution result containing the platform source path
pub const PlatformPaths = struct {
    platform_source_path: ?[]const u8, // Optional - may not exist for some platforms
};

/// Resolve platform specification from a Roc file to find both host library and platform source.
/// Returns PlatformPaths with arena-allocated paths (no need to free).
pub fn resolvePlatformPaths(ctx: *CliCtx, roc_file_path: []const u8) CliError!PlatformPaths {
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
/// Takes a CliCtx which provides allocators and error reporting.
fn extractPlatformSpecFromApp(ctx: *CliCtx, app_file_path: []const u8) (Allocator.Error || error{CliError})![]const u8 {
    // Read the app file
    var source = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, app_file_path, ctx.gpa, .unlimited) catch |err| {
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
    const ast = parse.parse(ctx.gpa, &env.common) catch {
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
fn stringFromExpr(ast: *parse.AST, expr_idx: parse.AST.Expr.Idx) error{ExpectedString}![]const u8 {
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
/// Uses CliCtx for error reporting.
fn validatePlatformSpec(ctx: *CliCtx, platform_spec: []const u8) CliError!void {
    if (std.fs.path.isAbsolute(platform_spec)) {
        return ctx.fail(.{ .absolute_platform_path = .{ .platform_spec = platform_spec } });
    }
}

/// Resolve a platform specification to a platform source path.
/// Uses CliCtx for error reporting.
fn resolvePlatformSpecToPaths(ctx: *CliCtx, platform_spec: []const u8, base_dir: []const u8) CliError!PlatformPaths {
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

    std.Io.Dir.cwd().access(ctx.io.std_io, resolved_path, .{}) catch {
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
fn getRocCacheDir(allocator: std.mem.Allocator) (Allocator.Error || error{NoCacheDir})![]const u8 {
    // Check XDG_CACHE_HOME first (Linux/macOS)
    if (try getEnvVar(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
        defer allocator.free(xdg_cache);
        return std.fs.path.join(allocator, &.{ xdg_cache, "roc", "packages" });
    }

    // Fall back to %LOCALAPPDATA%\roc\packages (Windows)
    if (comptime builtin.os.tag == .windows) {
        if (try getEnvVar(allocator, "LOCALAPPDATA")) |local_app_data| {
            defer allocator.free(local_app_data);
            return std.fs.path.join(allocator, &.{ local_app_data, "roc", "packages" });
        }
    }

    // Fall back to ~/.cache/roc/packages (Unix)
    if (try getEnvVar(allocator, "HOME")) |home| {
        defer allocator.free(home);
        return std.fs.path.join(allocator, &.{ home, ".cache", "roc", "packages" });
    }

    return error.NoCacheDir;
}

/// Cross-platform helper to get environment variable.
/// Returns null if the variable is not set. Caller must free the returned slice.
fn getEnvVar(allocator: std.mem.Allocator, key: []const u8) std.mem.Allocator.Error!?[]const u8 {
    const key_z = try allocator.dupeZ(u8, key);
    defer allocator.free(key_z);
    const value = std.c.getenv(key_z) orelse return null;
    const len = std.mem.len(value);
    return try allocator.dupe(u8, value[0..len]);
}

/// Resolve a URL bundle (platform or package) by downloading and caching it.
/// The URL must point to a .tar.zst bundle with a base58-encoded BLAKE3 hash filename.
/// Returns the path to `main.roc` inside the cache directory.
fn resolveUrlBundle(ctx: *CliCtx, url: []const u8) (CliError || error{OutOfMemory})![]const u8 {
    const download = unbundle.download;

    // 1. Validate URL and extract hash
    const base58_hash = download.validateUrl(url) catch {
        return ctx.fail(.{ .invalid_url = .{
            .url = url,
            .reason = "Invalid URL format or missing hash. URLs must end with a base58-encoded BLAKE3 hash filename (e.g., '<hash>.tar.zst').",
        } });
    };

    // 2. Get cache directory
    const cache_dir_path = getRocCacheDir(ctx.arena) catch {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = "Could not determine cache directory" } });
    };
    const package_dir_path = try std.fs.path.join(ctx.arena, &.{ cache_dir_path, base58_hash });

    // 3. Check if already cached
    const already_cached = blk: {
        var d = std.Io.Dir.cwd().openDir(ctx.io.std_io, package_dir_path, .{}) catch |err| switch (err) {
            error.FileNotFound => break :blk false,
            else => return ctx.fail(.{ .directory_not_found = .{ .path = package_dir_path } }),
        };
        d.close(ctx.io.std_io);
        break :blk true;
    };

    if (!already_cached) {
        // Not cached - need to download
        std.log.info("Downloading bundle from {s}...", .{url});

        // Create cache directory structure
        ensureCompilerCacheDirExists(ctx.io.std_io, cache_dir_path) catch |make_err| {
            return ctx.fail(.{ .directory_create_failed = .{
                .path = cache_dir_path,
                .err = make_err,
            } });
        };

        // Create package directory
        std.Io.Dir.cwd().createDir(ctx.io.std_io, package_dir_path, .default_dir) catch |make_err| switch (make_err) {
            error.PathAlreadyExists => {}, // Race condition, another process created it
            else => {
                return ctx.fail(.{ .directory_create_failed = .{
                    .path = package_dir_path,
                    .err = make_err,
                } });
            },
        };

        // Download and extract (path-based, no Dir handle needed)
        var gpa_copy = ctx.gpa;
        download.downloadAndExtract(&gpa_copy, ctx.io.std_io, url, package_dir_path) catch |download_err| {
            std.Io.Dir.cwd().deleteTree(ctx.io.std_io, package_dir_path) catch {};
            return ctx.fail(.{ .download_failed = .{
                .url = url,
                .err = download_err,
            } });
        };

        std.log.info("Bundle cached at {s}", .{package_dir_path});
    }

    // Platforms must have a main.roc entry point
    const platform_source_path = try std.fs.path.join(ctx.arena, &.{ package_dir_path, "main.roc" });
    std.Io.Dir.cwd().access(ctx.io.std_io, platform_source_path, .{}) catch {
        return ctx.fail(.{ .platform_source_not_found = .{
            .platform_path = package_dir_path,
            .searched_paths = &.{platform_source_path},
        } });
    };

    return platform_source_path;
}

/// Resolve a URL platform specification by downloading and caching the bundle.
/// The URL must point to a .tar.zst bundle with a base58-encoded BLAKE3 hash filename.
fn resolveUrlPlatform(ctx: *CliCtx, url: []const u8) (CliError || error{OutOfMemory})!PlatformPaths {
    const source_path = try resolveUrlBundle(ctx, url);
    return PlatformPaths{ .platform_source_path = source_path };
}

/// Extract the embedded roc_shim library to the specified path for the given target.
/// This library contains the shim code that runs in child processes to read the LIR image.
/// For native builds and roc run, use the native shim (pass null or native target).
/// For cross-compilation, pass the target to get the appropriate shim.
pub fn extractReadRocFilePathShimLibrary(ctx: *CliCtx, output_path: []const u8, target: ?RocTarget) anyerror!void {
    if (builtin.is_test) {
        // In test mode, create an empty file to avoid embedding issues
        const shim_file = try std.Io.Dir.cwd().createFile(ctx.io.std_io, output_path, .{});
        defer shim_file.close(ctx.io.std_io);
        return;
    }

    // Write the embedded shim library to the output path
    const shim_file = try std.Io.Dir.cwd().createFile(ctx.io.std_io, output_path, .{});
    defer shim_file.close(ctx.io.std_io);

    try shim_file.writeStreamingAll(ctx.io.std_io, interpreterShimBytes(target));
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

/// Use the Coordinator to discover every transitive module the entry point
/// imports (directly, via re-exports, or via a `package [...]` header) and
/// append any not already in `file_paths` so the bundle includes them
/// automatically. `uncompressed_size` is updated to reflect the newly added
/// files. Also validates platform target binaries if a platform is found.
fn discoverAndAddBundleModules(
    ctx: *CliCtx,
    first_roc_file: []const u8,
    file_paths: *std.ArrayList([]const u8),
    uncompressed_size: *u64,
    stderr: anytype,
) anyerror!void {
    // Resolve the entry point to an absolute path
    const abs_entry = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, first_roc_file, ctx.gpa) catch |err| {
        try stderr.print("Error: Could not resolve path '{s}': {}\n", .{ first_roc_file, err });
        return err;
    };
    defer ctx.gpa.free(abs_entry);

    // Create a BuildEnv to parse headers and discover modules via the Coordinator
    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.gpa);
    defer ctx.gpa.free(cwd);
    var build_env = try BuildEnv.init(ctx.gpa, .single_threaded, 1, RocTarget.detectNative(), cwd, ctx.io.std_io);
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

    // Build a set of absolute paths already in the bundle list for dedup.
    var bundled_set = std.StringHashMap(void).init(ctx.gpa);
    defer bundled_set.deinit();

    for (file_paths.items) |rel_path| {
        const abs_path = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, rel_path, ctx.gpa) catch continue;
        defer ctx.gpa.free(abs_path);
        try bundled_set.put(try ctx.arena.dupe(u8, abs_path), {});
    }

    // Append any discovered module that is not already in the bundle list.
    // The Coordinator yields absolute paths; convert each to a path relative
    // to cwd so it round-trips through `cwd.openFile` and survives the
    // bundle's path-validation step (which rejects absolute paths).
    if (build_env.coordinator) |coord| {
        var coord_pkg_it = coord.packages.iterator();
        while (coord_pkg_it.next()) |pkg_entry| {
            for (pkg_entry.value_ptr.*.modules.items) |mod_state| {
                const abs_path = mod_state.path;
                if (bundled_set.contains(abs_path)) continue;

                const rel_path = std.fs.path.relative(ctx.arena, cwd, null, cwd, abs_path) catch {
                    try stderr.print("Error: Discovered module path is outside the current directory and cannot be bundled: {s}\n", .{abs_path});
                    return error.MissingBundleFiles;
                };

                // Confirm the file is actually readable from cwd before adding it.
                const file = std.Io.Dir.cwd().openFile(ctx.io.std_io, rel_path, .{}) catch |err| {
                    try stderr.print("Error: Could not open discovered module '{s}': {}\n", .{ rel_path, err });
                    return err;
                };
                const stat = file.stat(ctx.io.std_io) catch |err| {
                    file.close(ctx.io.std_io);
                    return err;
                };
                file.close(ctx.io.std_io);

                try file_paths.append(ctx.arena, rel_path);
                try bundled_set.put(try ctx.arena.dupe(u8, abs_path), {});
                uncompressed_size.* += stat.size;
            }
        }
    }

    // If a platform was detected, validate target binaries exist
    // Use TargetsConfig from BuildEnv (already extracted during header parsing)
    if (platform_root_file) |pf| {
        if (build_env.getPlatformTargetsConfig()) |tc| {
            const pf_dir = std.fs.path.dirname(pf) orelse ".";
            if (platform_validation.validateAllTargetFilesExist(ctx.arena, ctx.io.std_io, tc, pf_dir)) |result| {
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

/// Find the longest directory path that is an ancestor of every input in `abs_paths`.
/// All inputs must be absolute paths (they may be paths to files or directories).
/// Returns the common parent directory with no trailing path separator (except for
/// a filesystem root such as "/"). Returns an empty slice if the inputs share no
/// directory ancestor (e.g. two Windows paths on different drives).
fn longestCommonParentDir(allocator: std.mem.Allocator, abs_paths: []const []const u8) Allocator.Error![]u8 {
    std.debug.assert(abs_paths.len > 0);

    const isSep = struct {
        fn f(byte: u8) bool {
            return byte == '/' or byte == '\\';
        }
    }.f;

    // Start with the dirname of the first path.
    var common = std.ArrayList(u8).empty;
    errdefer common.deinit(allocator);
    const first_dir = std.fs.path.dirname(abs_paths[0]) orelse abs_paths[0];
    try common.appendSlice(allocator, first_dir);

    for (abs_paths[1..]) |path| {
        const dir = std.fs.path.dirname(path) orelse path;

        // Find longest byte prefix shared between `common` and `dir`.
        var i: usize = 0;
        const max = @min(common.items.len, dir.len);
        while (i < max and common.items[i] == dir[i]) : (i += 1) {}

        // i is on a directory boundary if it is at the end of either path
        // OR the next byte of either path is a separator.
        const at_boundary = (i == common.items.len and (i == dir.len or isSep(dir[i]))) or
            (i == dir.len and (i == common.items.len or isSep(common.items[i])));

        if (!at_boundary) {
            // Back up to the last separator within [0..i). Drop everything after it.
            var j: usize = i;
            while (j > 0) {
                j -= 1;
                if (isSep(common.items[j])) {
                    // Keep the separator only when it's the root sep at index 0.
                    i = if (j == 0) 1 else j;
                    break;
                }
            } else {
                i = 0;
            }
        }

        common.items.len = @min(common.items.len, i);
    }

    // Strip trailing separators (preserve a single root separator).
    while (common.items.len > 1 and isSep(common.items[common.items.len - 1])) {
        common.items.len -= 1;
    }

    return common.toOwnedSlice(allocator);
}

/// Bundles a roc package and its dependencies into a compressed tar archive
pub fn rocBundle(ctx: *CliCtx, args: cli_args.BundleArgs) anyerror!void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    // Start timing
    const start_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    // Get current working directory
    const cwd = std.Io.Dir.cwd();

    // Determine output directory
    var output_dir = if (args.output_dir) |dir|
        try cwd.openDir(ctx.io.std_io, dir, .{})
    else
        cwd;
    defer if (args.output_dir != null) output_dir.close(ctx.io.std_io);

    // Create a temporary directory for the output file
    var tmp_dir = try std.Io.Dir.cwd().createDirPathOpen(ctx.io.std_io, ".roc_bundle_tmp", .{});
    defer {
        tmp_dir.close(ctx.io.std_io);
        std.Io.Dir.cwd().deleteTree(ctx.io.std_io, ".roc_bundle_tmp") catch {};
    }

    // Collect all files to bundle
    var file_paths = std.ArrayList([]const u8).empty;
    defer file_paths.deinit(ctx.arena);

    var uncompressed_size: u64 = 0;

    // If no paths provided, default to "main.roc"
    const paths_to_use = if (args.paths.len == 0) &[_][]const u8{"main.roc"} else args.paths;

    // Remember the first path from CLI args (before sorting)
    const first_cli_path = paths_to_use[0];

    // Detect whether any input path is absolute. Absolute paths are not allowed
    // inside the archive (the unbundle side rejects them, and a relative path
    // is what the user actually wants extracted). If any input is absolute we
    // rebase all paths against their longest common parent directory and pass
    // that directory to the bundle library — so the archive itself only ever
    // contains relative paths.
    var any_absolute = false;
    for (paths_to_use) |path| {
        if (std.fs.path.isAbsolute(path)) {
            any_absolute = true;
            break;
        }
    }

    // Check that all files exist and collect their sizes
    for (paths_to_use) |path| {
        const file = cwd.openFile(ctx.io.std_io, path, .{}) catch |err| {
            try stderr.print("Error: Could not open file '{s}': {}\n", .{ path, err });
            return err;
        };
        defer file.close(ctx.io.std_io);

        const stat = try file.stat(ctx.io.std_io);
        uncompressed_size += stat.size;

        try file_paths.append(ctx.arena, path);
    }

    // Find the first .roc file to use as entry point for module discovery
    const first_roc_file: ?[]const u8 = for (paths_to_use) |path| {
        if (std.mem.endsWith(u8, path, ".roc")) break path;
    } else null;

    // Use the Coordinator to discover all transitive module dependencies
    // (explicit imports plus modules exposed by a `package [...]` header)
    // and append any not already in the file list.
    if (first_roc_file) |roc_file| {
        try discoverAndAddBundleModules(ctx, roc_file, &file_paths, &uncompressed_size, stderr);
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

    // If any input was absolute, rebase all paths against their longest common
    // parent directory so the archive only contains relative paths. The opened
    // directory becomes the base_dir passed to the bundle library.
    //
    // Discovery (above) added transitively imported modules as cwd-relative
    // paths; `realpathAlloc` resolves both forms so they share the common
    // parent uniformly here.
    var rebased_base_dir: ?std.Io.Dir = null;
    defer if (rebased_base_dir) |d| d.close(ctx.io.std_io);
    var archive_paths: []const []const u8 = file_paths.items;
    if (any_absolute) {
        const resolved = try ctx.arena.alloc([]u8, file_paths.items.len);
        for (file_paths.items, 0..) |p, i| {
            resolved[i] = cwd.realPathFileAlloc(ctx.io.std_io, p, ctx.arena) catch |err| {
                try stderr.print("Error: Could not resolve path '{s}': {}\n", .{ p, err });
                return err;
            };
        }

        const common = try longestCommonParentDir(ctx.arena, resolved);
        if (common.len == 0) {
            try stderr.print("Error: Input file paths have no common parent directory.\n", .{});
            return error.InvalidPath;
        }

        const opened_dir = std.Io.Dir.openDirAbsolute(ctx.io.std_io, common, .{}) catch |err| {
            try stderr.print("Error: Could not open common parent directory '{s}': {}\n", .{ common, err });
            return err;
        };
        rebased_base_dir = opened_dir;

        // Build relative-to-common paths for the archive.
        const rel_paths = try ctx.arena.alloc([]const u8, resolved.len);
        for (resolved, 0..) |abs, i| {
            // abs must start with `common`; everything after is the relative path.
            // common has no trailing separator, so skip the leading separator byte
            // that follows it within abs.
            if (abs.len <= common.len or
                !std.mem.eql(u8, abs[0..common.len], common) or
                !(abs[common.len] == '/' or abs[common.len] == '\\'))
            {
                try stderr.print("Error: Path '{s}' is not under the common parent '{s}'.\n", .{ abs, common });
                return error.InvalidPath;
            }
            rel_paths[i] = abs[common.len + 1 ..];
        }
        archive_paths = rel_paths;
    }

    // Create temporary output file
    const temp_filename = "temp_bundle.tar.zst";
    const temp_file = try tmp_dir.createFile(ctx.io.std_io, temp_filename, .{
        // Allow querying metadata (stat) on the handle, necessary for windows
        .read = true,
        .truncate = true,
    });
    defer temp_file.close(ctx.io.std_io);

    // Create file path iterator
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) Allocator.Error!?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var iter = FilePathIterator{ .paths = archive_paths };

    // Bundle the files
    var allocator_copy = ctx.arena;
    var error_ctx: bundle.ErrorContext = undefined;
    var temp_writer_buffer: [4096]u8 = undefined;
    var temp_writer = temp_file.writerStreaming(ctx.io.std_io, &temp_writer_buffer);
    const bundle_base_dir = rebased_base_dir orelse cwd;
    const final_filename = bundle.bundleFiles(
        &iter,
        @intCast(args.compression_level),
        &allocator_copy,
        ctx.io.std_io,
        &temp_writer.interface,
        bundle_base_dir,
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
    const compressed_stat = try temp_file.stat(ctx.io.std_io);
    const compressed_size = compressed_stat.size;

    // Move the temp file to the final location
    try tmp_dir.rename(temp_filename, output_dir, final_filename, ctx.io.std_io);

    // Calculate elapsed time
    const end_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;
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

fn rocUnbundle(ctx: *CliCtx, args: cli_args.UnbundleArgs) anyerror!void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();
    const cwd = std.Io.Dir.cwd();

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
        cwd.access(ctx.io.std_io, dir_name, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                // Good, directory doesn't exist
            },
            else => return err,
        };

        if (cwd.openDir(ctx.io.std_io, dir_name, .{})) |_| {
            try stderr.print("Error: Directory {s} already exists\n", .{dir_name});
            had_errors = true;
            continue;
        } else |_| {
            // Directory doesn't exist, proceed
        }

        // Create the output directory
        var output_dir = try cwd.createDirPathOpen(ctx.io.std_io, dir_name, .{});
        defer output_dir.close(ctx.io.std_io);

        // Open the archive file
        const archive_file = cwd.openFile(ctx.io.std_io, archive_path, .{}) catch |err| {
            try stderr.print("Error opening {s}: {s}\n", .{ archive_path, @errorName(err) });
            had_errors = true;
            continue;
        };
        defer archive_file.close(ctx.io.std_io);

        // Unbundle the archive
        var error_ctx: unbundle.ErrorContext = undefined;
        var archive_reader_buffer: [4096]u8 = undefined;
        var archive_reader = archive_file.reader(ctx.io.std_io, &archive_reader_buffer);
        unbundle.unbundleFiles(
            ctx.gpa,
            &archive_reader.interface,
            output_dir,
            ctx.io.std_io,
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

fn rocBuild(ctx: *CliCtx, args: cli_args.BuildArgs) anyerror!void {
    // Handle the --z-bench-tokenize flag
    if (args.z_bench_tokenize) |file_path| {
        try benchTokenizer(ctx.gpa, ctx.io.std_io, file_path);
        return;
    }

    // Handle the --z-bench-parse flag
    if (args.z_bench_parse) |directory_path| {
        try benchParse(ctx.gpa, ctx.io.std_io, directory_path);
        return;
    }

    // Headerless apps use a simple builtin platform and cannot be compiled
    if (try readDefaultAppSource(ctx, args.path)) |source| {
        ctx.gpa.free(source);
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .build_not_supported_for_headerless = .{ .app_path = args.path },
        });
        return error.UnsupportedTarget;
    }

    // Select build path based on optimization level
    switch (args.opt.toBackend()) {
        .dev, .llvm => {
            // Use native code generation backend
            try rocBuildNative(ctx, args);
        },
        .interpreter, .wasm => {
            // Use embedded interpreter build approach
            // This compiles the Roc app and embeds a viewable LIR image in the binary.
            try rocBuildEmbedded(ctx, args);
        },
    }
}

/// Build using the dev backend to generate native machine code.
/// This produces truly compiled executables without an interpreter.
fn nativeBuildEntrypoints(
    ctx: *CliCtx,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) Allocator.Error![]backend.Entrypoint {
    const root_procs = lowered.lir_result.root_procs.items;
    const root_metadata = lowered.lir_result.root_metadata.items;
    if (root_procs.len != root_metadata.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "native build invariant violated: root metadata mismatch roots={d} metadata={d}",
                .{ root_procs.len, root_metadata.len },
            );
        }
        unreachable;
    }

    var entrypoints = std.ArrayList(backend.Entrypoint).empty;
    errdefer entrypoints.deinit(ctx.gpa);

    for (root_procs, root_metadata) |root_proc, metadata| {
        if (metadata.abi != .platform or metadata.exposure != .exported) continue;
        const root = root_artifact.lookupRootRequestByOrder(metadata.order) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("native build invariant violated: missing root request order {d}", .{metadata.order});
            }
            unreachable;
        };
        if (root.kind != .provided_export) continue;

        const proc_spec = lowered.lir_result.store.getProcSpec(root_proc);
        const arg_locals = lowered.lir_result.store.getLocalSpan(proc_spec.args);
        const arg_layouts = try ctx.arena.alloc(layout.Idx, arg_locals.len);
        for (arg_locals, 0..) |local_id, i| {
            arg_layouts[i] = lowered.lir_result.store.getLocal(local_id).layout_idx;
        }

        try entrypoints.append(ctx.gpa, .{
            .symbol_name = try nativeEntrypointSymbolName(ctx, root_artifact, root),
            .proc = root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = proc_spec.ret_layout,
        });
    }

    return try entrypoints.toOwnedSlice(ctx.gpa);
}

fn nativeEntrypointSymbolName(
    ctx: *CliCtx,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    root: check.CheckedArtifact.RootRequest,
) Allocator.Error![]const u8 {
    const entrypoint_name = root_artifact.providedEntrypointName(root) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "platform entrypoint invariant violated: exported platform root has no published FFI symbol",
                .{},
            );
        }
        unreachable;
    };
    return try std.fmt.allocPrint(ctx.arena, "roc__{s}", .{entrypoint_name});
}

const PlatformLinkInputs = struct {
    target_name: []const u8,
    platform_files_dir: []const u8,
    platform_files_pre: []const []const u8,
    platform_files_post: []const []const u8,
};

fn collectPlatformLinkInputs(
    ctx: *CliCtx,
    platform_dir: []const u8,
    targets_config: roc_target.TargetsConfig,
    target: RocTarget,
    link_type: roc_target.LinkType,
) (Allocator.Error || error{ CliError, MissingTargetFile })!PlatformLinkInputs {
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
                std.Io.Dir.cwd().access(ctx.io.std_io, full_path, .{}) catch {
                    renderValidationError(ctx.gpa, .{ .missing_target_file = .{
                        .target = target,
                        .link_type = link_type,
                        .file_path = path,
                        .expected_full_path = full_path,
                    } }, ctx.io.stderr());
                    return error.MissingTargetFile;
                };
                if (!hit_app) {
                    try platform_files_pre.append(full_path);
                } else {
                    try platform_files_post.append(full_path);
                }
            },
            .app => hit_app = true,
            .win_gui => {},
        }
    }

    return .{
        .target_name = target_name,
        .platform_files_dir = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir }),
        .platform_files_pre = platform_files_pre.items,
        .platform_files_post = platform_files_post.items,
    };
}

fn appendOwnedWasmInput(ctx: *CliCtx, owned_inputs: *std.ArrayList([]u8), path: []const u8) anyerror![]const u8 {
    const bytes = try std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, path, ctx.gpa, .unlimited);
    errdefer ctx.gpa.free(bytes);
    try owned_inputs.append(ctx.gpa, bytes);
    return bytes;
}

fn freeOwnedWasmInputs(ctx: *CliCtx, owned_inputs: *std.ArrayList([]u8)) void {
    for (owned_inputs.items) |bytes| {
        ctx.gpa.free(bytes);
    }
    owned_inputs.deinit(ctx.gpa);
}

fn preloadWasmInput(ctx: *CliCtx, owned_inputs: *std.ArrayList([]u8), path: []const u8) anyerror!backend.wasm.WasmModule {
    const bytes = try appendOwnedWasmInput(ctx, owned_inputs, path);
    return backend.wasm.WasmModule.preload(ctx.gpa, bytes, true) catch |err| {
        std.log.err("Failed to preload wasm input {s}: {}", .{ path, err });
        return err;
    };
}

fn mergeWasmInput(
    ctx: *CliCtx,
    module: *backend.wasm.WasmModule,
    owned_inputs: *std.ArrayList([]u8),
    path: []const u8,
) anyerror!void {
    var next_module = try preloadWasmInput(ctx, owned_inputs, path);
    defer next_module.deinit();

    var merge_result = try module.mergeModule(&next_module);
    merge_result.deinit();
}

fn rocBuildWasmSurgical(
    ctx: *CliCtx,
    args: cli_args.BuildArgs,
    target: RocTarget,
    link_type: roc_target.LinkType,
    final_output_path: []const u8,
    platform_dir: []const u8,
    targets_config: roc_target.TargetsConfig,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    entrypoints: []const backend.Entrypoint,
) anyerror!void {
    if (args.no_link) {
        try ctx.io.stderr().writeAll("Error: --no-link is not supported for wasm32 surgical builds.\n");
        return error.UnsupportedTarget;
    }

    if (entrypoints.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("wasm build invariant violated: no exported platform entrypoints", .{});
        }
        unreachable;
    }

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, targets_config, target, link_type);
    if (link_inputs.platform_files_pre.len + link_inputs.platform_files_post.len == 0) {
        try ctx.io.stderr().writeAll("Error: wasm32 builds require a relocatable wasm platform file.\n");
        return error.UnsupportedTarget;
    }

    var owned_inputs: std.ArrayList([]u8) = .empty;
    defer freeOwnedWasmInputs(ctx, &owned_inputs);

    var loaded_module = false;
    var wasm_module: backend.wasm.WasmModule = undefined;
    errdefer if (loaded_module) wasm_module.deinit();

    for (link_inputs.platform_files_pre) |path| {
        if (!loaded_module) {
            wasm_module = try preloadWasmInput(ctx, &owned_inputs, path);
            loaded_module = true;
        } else {
            try mergeWasmInput(ctx, &wasm_module, &owned_inputs, path);
        }
    }
    for (link_inputs.platform_files_post) |path| {
        if (!loaded_module) {
            wasm_module = try preloadWasmInput(ctx, &owned_inputs, path);
            loaded_module = true;
        } else {
            try mergeWasmInput(ctx, &wasm_module, &owned_inputs, path);
        }
    }

    try wasm_module.exportGlobalSymbols();
    wasm_module.removeMemoryAndTableImports();

    const builtins_bytes = BuiltinsObjects.forTarget(.wasm32);
    if (builtins_bytes.len > 0) {
        var builtins_module = backend.wasm.WasmModule.preload(ctx.gpa, builtins_bytes, true) catch |err| {
            std.log.err("Failed to preload wasm builtins: {}", .{err});
            return err;
        };
        defer builtins_module.deinit();

        var merge_result = try wasm_module.mergeModule(&builtins_module);
        merge_result.deinit();
    }

    var codegen = backend.wasm.WasmCodeGen.initWithModule(
        ctx.gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        wasm_module,
    );
    defer codegen.deinit();
    loaded_module = false;

    try codegen.registerIndirectCallTypes();
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());

    var host_to_app_map: std.ArrayList(backend.wasm.WasmModule.HostToAppEntry) = .empty;
    defer host_to_app_map.deinit(ctx.gpa);
    try host_to_app_map.ensureTotalCapacity(ctx.gpa, entrypoints.len);

    for (entrypoints) |entry| {
        const fn_index = try codegen.generateEntrypointWrapper(
            entry.symbol_name,
            entry.proc,
            entry.arg_layouts,
            entry.ret_layout,
        );
        host_to_app_map.appendAssumeCapacity(.{
            .name = entry.symbol_name,
            .fn_index = fn_index,
        });
    }

    try codegen.flushPendingBodies();
    try codegen.module.linkHostToAppCalls(host_to_app_map.items);

    const stack_bytes = args.wasm_stack_size orelse linker.DEFAULT_WASM_STACK_SIZE;
    try codegen.module.finalizeMemoryAndTable(@intCast(stack_bytes));
    codegen.module.ensureMemoryMinBytes(args.wasm_memory orelse linker.DEFAULT_WASM_INITIAL_MEMORY);
    try codegen.module.resolveRelocations();

    const called_fns = try ctx.gpa.alloc(bool, codegen.module.liveFunctionCount());
    defer ctx.gpa.free(called_fns);
    @memset(called_fns, false);
    try codegen.module.eliminateDeadCode(called_fns);

    try codegen.module.verifyNoBuiltinImports();
    try codegen.module.materializeFuncBodies();

    const wasm_bytes = try codegen.module.encode(ctx.gpa);
    defer ctx.gpa.free(wasm_bytes);
    backend.writeFileWindowsAvSafe(ctx.io.std_io, final_output_path, wasm_bytes) catch |err| {
        std.log.err("Failed to write wasm output: {}", .{err});
        return error.WasmOutputWriteFailed;
    };
}

fn rocBuildNative(ctx: *CliCtx, args: cli_args.BuildArgs) anyerror!void {
    const target_mod = @import("target.zig");

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else
        try base.module_path.getModuleNameAlloc(ctx.arena, args.path);

    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(ctx.arena);
    const build_cache_dir = try std.fs.path.join(ctx.arena, &.{ cache_dir, "roc_build" });
    ensureCompilerCacheDirExists(ctx.io.std_io, build_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const thread_count: usize = args.max_threads orelse (std.Thread.getCpuCount() catch 1);
    const mode: Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.gpa);
    defer ctx.gpa.free(cwd);
    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative(), cwd, ctx.io.std_io);
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    if (!args.no_cache) {
        const build_cache_manager = try ctx.gpa.create(CacheManager);
        build_cache_manager.* = CacheManager.init(ctx.gpa, .{
            .enabled = true,
            .verbose = args.verbose,
            .roc_ctx = ctx.coreCtx(),
        }, ctx.coreCtx());
        build_env.setCacheManager(build_cache_manager);
    }

    build_env.discoverDependencies(args.path) catch |err| {
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };

    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };
    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |path| std.fs.path.dirname(path) orelse "." else ".";

    const target: target_mod.RocTarget, const link_type: target_mod.LinkType = if (args.target) |target_str| blk: {
        const parsed_target = target_mod.RocTarget.fromString(target_str) orelse {
            renderValidationError(ctx.gpa, .{ .invalid_target = .{ .target_str = target_str } }, ctx.io.stderr());
            return error.InvalidTarget;
        };

        if (targets_config.supportsTarget(parsed_target, .exe)) {
            break :blk .{ parsed_target, .exe };
        }
        if (targets_config.supportsTarget(parsed_target, .static_lib)) {
            break :blk .{ parsed_target, .static_lib };
        }
        if (targets_config.supportsTarget(parsed_target, .shared_lib)) {
            break :blk .{ parsed_target, .shared_lib };
        }

        const result = platform_validation.createUnsupportedTargetResult(
            platform_source orelse "<unknown>",
            parsed_target,
            .exe,
            targets_config,
        );
        renderValidationError(ctx.gpa, result, ctx.io.stderr());
        return error.UnsupportedTarget;
    } else blk: {
        const compatible = targets_config.getFirstCompatibleTarget() orelse {
            try renderProblem(ctx.gpa, ctx.io.stderr(), .{
                .platform_validation_failed = .{
                    .message = "No compatible target found. The platform does not support any target compatible with this system.",
                },
            });
            return error.UnsupportedTarget;
        };
        break :blk .{ compatible.target, compatible.link_type };
    };

    if (args.require_executable_output and link_type != .exe) {
        const stderr = ctx.io.stderr();
        switch (link_type) {
            .static_lib => {
                try stderr.print("Error: The selected target only produces static libraries.\n\n", .{});
                try stderr.print("Static library platforms produce .a/.lib/.wasm files that must be\n", .{});
                try stderr.print("linked by a host application. Use 'roc build' instead to produce\n", .{});
                try stderr.print("the library artifact.\n", .{});
            },
            .shared_lib => {
                try stderr.print("Error: The selected target only produces shared libraries.\n\n", .{});
                try stderr.print("Shared library platforms produce .so/.dylib/.dll files that must be\n", .{});
                try stderr.print("loaded by a host application. Use 'roc build' instead to produce\n", .{});
                try stderr.print("the library artifact.\n", .{});
            },
            .exe => unreachable,
        }
        return error.UnsupportedTarget;
    }

    const target_arch = target.toCpuArch();
    const target_os = target.toOsTag();
    if (target.isDynamic() and builtin.target.os.tag != .linux) {
        renderValidationError(ctx.gpa, .{
            .unsupported_glibc_cross = .{
                .target = target,
                .host_os = @tagName(builtin.target.os.tag),
            },
        }, ctx.io.stderr());
        return error.UnsupportedCrossCompilation;
    }

    switch (target_arch) {
        .x86_64, .aarch64, .wasm32 => {},
        else => {
            try ctx.io.stderr().print(
                "Error: The native object backend does not support the '{s}' architecture.\n",
                .{@tagName(target_arch)},
            );
            return error.UnsupportedTarget;
        },
    }

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

    build_env.setTarget(target);
    build_env.compileDiscovered() catch |err| {
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };

    const diag = try build_env.renderDiagnostics(ctx.io.stderr());
    const total_warning_count = diag.warnings;
    if (diag.errors > 0) {
        if (args.allow_errors) return;
        return error.CompilationFailed;
    }

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    const target_usize: base.target.TargetUsize = switch (target.ptrBitWidth()) {
        32 => .u32,
        64 => .u64,
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("native build invariant violated: unsupported target pointer width {d}", .{target.ptrBitWidth()});
            }
            unreachable;
        },
    };

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        ctx.gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{ .requests = root_artifact.root_requests.runtime_requests },
        .{
            .target_usize = target_usize,
        },
    );
    defer lowered.deinit();

    const entrypoints = try nativeBuildEntrypoints(ctx, root_artifact, &lowered);
    defer ctx.gpa.free(entrypoints);

    if (target_arch == .wasm32) {
        try rocBuildWasmSurgical(
            ctx,
            args,
            target,
            link_type,
            final_output_path,
            platform_dir,
            targets_config,
            &lowered,
            entrypoints,
        );

        const elapsed_ns = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
        const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
        const cache_stats = build_env.getBuildStats();
        const cache_percent = if (cache_stats.modules_total > 0)
            @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
        else
            0;

        if (!args.suppress_build_status) {
            const stdout = ctx.io.stdout();
            try stdout.print("Built {s} in {d:.1}ms", .{ final_output_path, elapsed_ms });
            try stdout.writeAll(" (checked-artifact wasm backend)\n");

            if (args.verbose) {
                try stdout.print("\n    Modules: {} total, {} cached, {} built\n", .{
                    cache_stats.modules_total,
                    cache_stats.cache_hits,
                    cache_stats.modules_compiled,
                });
                try stdout.print("    Cache Hit: {}%\n", .{cache_percent});
            }

            if (total_warning_count > 0) {
                try stdout.print("  {} warning(s)\n", .{total_warning_count});
            }
        }
        return;
    }

    const static_data_exports = try compile.static_data_exports.buildProvidedDataExports(
        ctx.gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        &lowered,
        target,
    );
    defer compile.static_data_exports.deinitProvidedDataExports(ctx.gpa, static_data_exports);

    if (entrypoints.len == 0 and static_data_exports.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("native build invariant violated: no exported platform entrypoints or data symbols", .{});
        }
        unreachable;
    }

    var object_compiler = backend.ObjectFileCompiler.init(ctx.gpa);

    ensureCompilerCacheDirExists(ctx.io.std_io, build_cache_dir) catch |err| {
        std.log.err("Failed to create compiler build cache dir {s}: {}", .{ build_cache_dir, err });
        return err;
    };

    const obj_filename = try std.fmt.allocPrint(ctx.arena, "roc_app_{s}.o", .{@tagName(target)});
    const obj_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, obj_filename });
    object_compiler.compileToObjectFileAndWrite(
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        entrypoints,
        static_data_exports,
        lowered.lir_result.store.getProcSpecs(),
        target,
        obj_path,
        ctx.coreCtx(),
    ) catch |err| {
        std.log.err("Native compilation failed: {}", .{err});
        return error.NativeCompilationFailed;
    };

    if (args.no_link) {
        if (!args.suppress_build_status) {
            try ctx.io.stdout().print("Object file generated: {s}\n", .{obj_path});
        }
        return;
    }

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, targets_config, target, link_type);

    const builtins_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, BuiltinsObjects.filename(target) });
    backend.writeFileWindowsAvSafe(ctx.io.std_io, builtins_path, BuiltinsObjects.forTarget(target)) catch {
        return error.BuiltinsExtractionFailed;
    };

    var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);
    try object_files.append(obj_path);
    try object_files.append(builtins_path);

    const link_config = linker.LinkConfig{
        .target_format = linker.TargetFormat.detectFromOs(target_os),
        .target_abi = linker.TargetAbi.fromRocTarget(target),
        .target_os = target_os,
        .target_arch = target_arch,
        .output_path = final_output_path,
        .object_files = object_files.items,
        .platform_files_pre = link_inputs.platform_files_pre,
        .platform_files_post = link_inputs.platform_files_post,
        .extra_args = &.{},
        .can_exit_early = false,
        .disable_output = false,
        .platform_files_dir = link_inputs.platform_files_dir,
        .scratch_dir = build_cache_dir,
    };

    if (args.z_dump_linker) {
        try dumpLinkerInputs(ctx, link_config);
    }

    linker.link(ctx, link_config) catch |err| {
        return ctx.fail(.{ .linker_failed = .{
            .err = err,
            .target = link_inputs.target_name,
        } });
    };

    const elapsed_ns = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    const cache_stats = build_env.getBuildStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    if (!args.suppress_build_status) {
        const stdout = ctx.io.stdout();
        try stdout.print("Built {s} in {d:.1}ms", .{ final_output_path, elapsed_ms });
        try stdout.writeAll(" (checked-artifact native backend)\n");

        if (args.verbose) {
            try stdout.print("\n    Modules: {} total, {} cached, {} built\n", .{
                cache_stats.modules_total,
                cache_stats.cache_hits,
                cache_stats.modules_compiled,
            });
            try stdout.print("    Cache Hit: {}%\n", .{cache_percent});
        }

        if (total_warning_count > 0) {
            try stdout.print("  {} warning(s)\n", .{total_warning_count});
        }
    }

    if (args.warning_count_out) |warning_count_out| {
        warning_count_out.* = total_warning_count;
    }

    if (args.exit_on_warnings and total_warning_count > 0) {
        ctx.io.flush();
        std.process.exit(2);
    }
}

/// Build a standalone binary with the interpreter and an embedded LIR image.
/// This is the primary build path that creates executables or libraries without requiring IPC.
fn rocBuildEmbedded(ctx: *CliCtx, args: cli_args.BuildArgs) anyerror!void {
    const target_mod = @import("target.zig");

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else
        try base.module_path.getModuleNameAlloc(ctx.arena, args.path);

    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(ctx.arena);
    const build_cache_dir = try std.fs.path.join(ctx.arena, &.{ cache_dir, "roc_build" });
    ensureCompilerCacheDirExists(ctx.io.std_io, build_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    const thread_count: usize = args.max_threads orelse (std.Thread.getCpuCount() catch 1);
    const mode: Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.gpa);
    defer ctx.gpa.free(cwd);
    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative(), cwd, ctx.io.std_io);
    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    if (!args.no_cache) {
        const build_cache_manager = try ctx.gpa.create(CacheManager);
        build_cache_manager.* = CacheManager.init(ctx.gpa, .{
            .enabled = true,
            .verbose = args.verbose,
            .roc_ctx = ctx.coreCtx(),
        }, ctx.coreCtx());
        build_env.setCacheManager(build_cache_manager);
    }

    build_env.discoverDependencies(args.path) catch |err| {
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };

    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };
    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |path| std.fs.path.dirname(path) orelse "." else ".";

    const target: target_mod.RocTarget, const link_type: target_mod.LinkType = if (args.target) |target_str| blk: {
        const parsed_target = target_mod.RocTarget.fromString(target_str) orelse {
            renderValidationError(ctx.gpa, .{ .invalid_target = .{ .target_str = target_str } }, ctx.io.stderr());
            return error.InvalidTarget;
        };

        if (targets_config.supportsTarget(parsed_target, .exe)) {
            break :blk .{ parsed_target, .exe };
        }
        if (targets_config.supportsTarget(parsed_target, .static_lib)) {
            break :blk .{ parsed_target, .static_lib };
        }
        if (targets_config.supportsTarget(parsed_target, .shared_lib)) {
            break :blk .{ parsed_target, .shared_lib };
        }

        const result = platform_validation.createUnsupportedTargetResult(
            platform_source orelse "<unknown>",
            parsed_target,
            .exe,
            targets_config,
        );
        renderValidationError(ctx.gpa, result, ctx.io.stderr());
        return error.UnsupportedTarget;
    } else blk: {
        const compatible = targets_config.getFirstCompatibleTarget() orelse {
            try renderProblem(ctx.gpa, ctx.io.stderr(), .{
                .platform_validation_failed = .{
                    .message = "No compatible target found. The platform does not support any target compatible with this system.",
                },
            });
            return error.UnsupportedTarget;
        };
        break :blk .{ compatible.target, compatible.link_type };
    };

    const native_target = RocTarget.detectNative();
    if (target != native_target) {
        const stderr = ctx.io.stderr();
        try stderr.print("Error: The interpreter backend only supports building for the native target ({s}).\n\n", .{@tagName(native_target)});
        try stderr.print("To cross-compile for {s}, use the dev backend:\n\n", .{@tagName(target)});
        try stderr.print("    roc build --opt=dev --target={s} {s}\n\n", .{ @tagName(target), args.path });
        return error.UnsupportedCrossCompilation;
    }

    if (args.require_executable_output and link_type != .exe) {
        const stderr = ctx.io.stderr();
        switch (link_type) {
            .static_lib => {
                try stderr.print("Error: The selected target only produces static libraries.\n\n", .{});
                try stderr.print("Static library platforms produce .a/.lib/.wasm files that must be\n", .{});
                try stderr.print("linked by a host application. Use 'roc build' instead to produce\n", .{});
                try stderr.print("the library artifact.\n", .{});
            },
            .shared_lib => {
                try stderr.print("Error: The selected target only produces shared libraries.\n\n", .{});
                try stderr.print("Shared library platforms produce .so/.dylib/.dll files that must be\n", .{});
                try stderr.print("loaded by a host application. Use 'roc build' instead to produce\n", .{});
                try stderr.print("the library artifact.\n", .{});
            },
            .exe => unreachable,
        }
        return error.UnsupportedTarget;
    }

    const target_arch = target.toCpuArch();
    const target_os = target.toOsTag();
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

    build_env.setTarget(target);
    build_env.compileDiscovered() catch |err| {
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };

    const diag = try build_env.renderDiagnostics(ctx.io.stderr());
    const total_warning_count = diag.warnings;
    if (diag.errors > 0) {
        if (args.allow_errors) return;
        return error.CompilationFailed;
    }

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try createSharedMemory(ctx.io.std_io, page_size);
    defer shm.deinit(ctx.gpa);

    const shm_allocator = shm.allocator();
    const image_header = try shm_allocator.create(lir.LirImage.Header);

    const lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        shm_allocator,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{ .requests = root_artifact.root_requests.runtime_requests },
        .{
            .target_usize = base.target.TargetUsize.native,
        },
    );

    const platform_entrypoints = try lowered.platformEntrypoints(shm_allocator);
    try lir.LirImage.fillHeaderInSharedMemory(
        image_header,
        shm.base_ptr,
        shm.getUsedSize(),
        &lowered.lir_result,
        lowered.target_usize,
        platform_entrypoints,
    );
    shm.updateHeader();

    const lir_image = try ctx.arena.dupe(u8, shm.base_ptr[0..shm.getUsedSize()]);
    const entrypoint_names = try lowered.platformEntrypointNames(ctx.arena, root_artifact);
    if (entrypoint_names.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("embedded build invariant violated: no platform entrypoints", .{});
        }
        unreachable;
    }

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, targets_config, target, link_type);

    const shim_filename = try interpreterShimCacheFilename(ctx, target);
    const shim_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, shim_filename });
    std.Io.Dir.cwd().access(ctx.io.std_io, shim_path, .{}) catch {
        extractReadRocFilePathShimLibrary(ctx, shim_path, target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };
    };

    const enable_debug = args.debug or (builtin.mode == .Debug);
    const platform_shim_path = try generatePlatformHostShim(
        ctx,
        build_cache_dir,
        entrypoint_names,
        target,
        lir_image,
        enable_debug,
    );

    var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);
    try object_files.append(shim_path);
    if (platform_shim_path) |path| {
        try object_files.append(path);
    }

    var extra_args = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    if (target.isMacOS()) {
        try extra_args.append("-lSystem");
    }

    const link_config = linker.LinkConfig{
        .target_format = linker.TargetFormat.detectFromOs(target_os),
        .target_abi = linker.TargetAbi.fromRocTarget(target),
        .target_os = target_os,
        .target_arch = target_arch,
        .output_path = final_output_path,
        .object_files = object_files.items,
        .platform_files_pre = link_inputs.platform_files_pre,
        .platform_files_post = link_inputs.platform_files_post,
        .extra_args = extra_args.items,
        .can_exit_early = false,
        .disable_output = false,
        .wasm_initial_memory = args.wasm_memory orelse linker.DEFAULT_WASM_INITIAL_MEMORY,
        .wasm_stack_size = args.wasm_stack_size orelse linker.DEFAULT_WASM_STACK_SIZE,
        .platform_files_dir = link_inputs.platform_files_dir,
        .scratch_dir = build_cache_dir,
    };

    if (args.z_dump_linker) {
        try dumpLinkerInputs(ctx, link_config);
    }

    linker.link(ctx, link_config) catch |err| {
        return ctx.fail(.{ .linker_failed = .{
            .err = err,
            .target = link_inputs.target_name,
        } });
    };

    const elapsed_ns_embed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns_embed)) / 1_000_000.0;
    const cache_stats = build_env.getBuildStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    if (!args.suppress_build_status) {
        const stdout = ctx.io.stdout();
        try stdout.print("Built {s} in {d:.1}ms", .{ final_output_path, elapsed_ms });
        try stdout.writeAll(" (checked-artifact embedded interpreter)\n");
        if (args.verbose) {
            try stdout.print("\n    Modules: {} total, {} cached, {} built\n", .{
                cache_stats.modules_total,
                cache_stats.cache_hits,
                cache_stats.modules_compiled,
            });
            try stdout.print("    Cache Hit: {}%\n", .{cache_percent});
        }

        if (total_warning_count > 0) {
            try stdout.print("  {} warning(s)\n", .{total_warning_count});
        }
    }

    if (args.warning_count_out) |warning_count_out| {
        warning_count_out.* = total_warning_count;
    }

    if (args.exit_on_warnings and total_warning_count > 0) {
        ctx.io.flush();
        std.process.exit(2);
    }
}

/// Dump linker inputs to a temp directory for debugging linking issues.
/// Creates a directory with all input files copied and a README with the linker command.
fn dumpLinkerInputs(ctx: *CliCtx, link_config: linker.LinkConfig) anyerror!void {
    const stderr = ctx.io.stderr();

    // Create temp directory with unique name based on timestamp
    const timestamp = @divTrunc(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds, 1_000_000_000);
    const dir_name = try std.fmt.allocPrint(ctx.arena, "roc-linker-debug-{d}", .{timestamp});
    const dump_dir = try std.fs.path.join(ctx.arena, &.{ "/tmp", dir_name });

    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, dump_dir) catch |err| {
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
        std.Io.Dir.cwd().copyFile(src, std.Io.Dir.cwd(), dest_path, ctx.io.std_io, .{}) catch |err| {
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
        std.Io.Dir.cwd().copyFile(src, std.Io.Dir.cwd(), dest_path, ctx.io.std_io, .{}) catch |err| {
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
        std.Io.Dir.cwd().copyFile(src, std.Io.Dir.cwd(), dest_path, ctx.io.std_io, .{}) catch |err| {
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
        try file_list.print("  {s}\n    <- {s} ({s})\n", .{ file.name, file.original, file.category });
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
    const readme_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, readme_path, .{}) catch |err| {
        try stderr.print("Warning: Failed to create README.txt: {}\n", .{err});
        return;
    };
    defer readme_file.close(ctx.io.std_io);
    readme_file.writeStreamingAll(ctx.io.std_io, readme_content) catch |err| {
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

const CliTestResult = enum { passed, failed };

const CliTestFailureDetailVisibility = enum(u8) {
    always = 0,
    verbose_only = 1,
};

const CliTestResultItem = struct {
    result: CliTestResult,
    order: u32,
    region: base.Region,
    failure_detail: ?[]const u8,
    failure_detail_visibility: CliTestFailureDetailVisibility = .always,
};

const CliModuleTestResult = struct {
    env: *const ModuleEnv,
    path: []const u8,
    results: []const CliTestResultItem,
    cached: bool,
};

const CliTestRunSummary = struct {
    passed: u32 = 0,
    failed: u32 = 0,
    modules_with_tests: u32 = 0,
    cached_modules: u32 = 0,
};

const cli_test_cache_magic = "ROC_TEST_RESULTS_V3";

fn appendU32(bytes: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u32) Allocator.Error!void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, value, .little);
    try bytes.appendSlice(allocator, &buf);
}

fn readU32(bytes: []const u8, offset: *usize) ?u32 {
    if (offset.* + 4 > bytes.len) return null;
    const value = std.mem.readInt(u32, bytes[offset.*..][0..4], .little);
    offset.* += 4;
    return value;
}

fn cliTestCacheKey(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    opt: cli_args.OptLevel,
) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(cli_test_cache_magic);
    hasher.update(build_options.compiler_version);
    hasher.update(@tagName(opt));
    hasher.update(&artifact.key.bytes);
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

fn summarizeTestResults(results: []const CliTestResultItem) CliTestRunSummary {
    var summary = CliTestRunSummary{ .modules_with_tests = 1 };
    for (results) |result| {
        switch (result.result) {
            .passed => summary.passed += 1,
            .failed => summary.failed += 1,
        }
    }
    return summary;
}

fn storeCliTestResultsInCache(
    ctx: *CliCtx,
    cache_manager: ?*CacheManager,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    opt: cli_args.OptLevel,
    results: []const CliTestResultItem,
) (Allocator.Error || error{NoHomeDirectory})!void {
    const manager = cache_manager orelse return;

    var bytes = std.ArrayList(u8).empty;
    defer bytes.deinit(ctx.gpa);
    try bytes.appendSlice(ctx.gpa, cli_test_cache_magic);
    try appendU32(&bytes, ctx.gpa, @intCast(results.len));
    for (results) |result| {
        try appendU32(&bytes, ctx.gpa, result.order);
        try bytes.append(ctx.gpa, switch (result.result) {
            .passed => 0,
            .failed => 1,
        });
        if (result.failure_detail) |message| {
            try bytes.append(ctx.gpa, 1);
            try bytes.append(ctx.gpa, @intFromEnum(result.failure_detail_visibility));
            try appendU32(&bytes, ctx.gpa, @intCast(message.len));
            try bytes.appendSlice(ctx.gpa, message);
        } else {
            try bytes.append(ctx.gpa, 0);
        }
    }

    const entries_dir = try manager.config.getTestCacheDir(ctx.gpa);
    defer ctx.gpa.free(entries_dir);
    manager.storeRawBytes(cliTestCacheKey(artifact, opt), bytes.items, entries_dir);
}

fn appendCachedCliTestResults(
    ctx: *CliCtx,
    cache_manager: ?*CacheManager,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    module: BuildEnv.CompiledModuleInfo,
    opt: cli_args.OptLevel,
    test_roots: []const check.CheckedArtifact.RootRequest,
    module_results: *std.ArrayList(CliModuleTestResult),
) (Allocator.Error || error{NoHomeDirectory})!?CliTestRunSummary {
    const manager = cache_manager orelse return null;

    const entries_dir = try manager.config.getTestCacheDir(ctx.gpa);
    defer ctx.gpa.free(entries_dir);
    const data = manager.loadRawBytes(cliTestCacheKey(artifact, opt), entries_dir) orelse return null;
    defer ctx.gpa.free(data);

    var offset: usize = 0;
    if (data.len < cli_test_cache_magic.len) return null;
    if (!std.mem.eql(u8, data[0..cli_test_cache_magic.len], cli_test_cache_magic)) return null;
    offset += cli_test_cache_magic.len;

    const count = readU32(data, &offset) orelse return null;
    if (count != test_roots.len) return null;

    var results = std.ArrayList(CliTestResultItem).empty;
    var results_owned_by_module = false;
    defer {
        if (!results_owned_by_module) {
            for (results.items) |result| {
                if (result.failure_detail) |message| ctx.gpa.free(message);
            }
            results.deinit(ctx.gpa);
        }
    }

    for (0..@as(usize, @intCast(count))) |_| {
        const order = readU32(data, &offset) orelse return null;
        if (offset + 2 > data.len) return null;
        const result_tag = data[offset];
        offset += 1;
        const has_message = data[offset];
        offset += 1;
        const result: CliTestResult = switch (result_tag) {
            0 => .passed,
            1 => .failed,
            else => return null,
        };

        const root = testRootByOrder(test_roots, order);
        const region = testRootRegion(module.semantic.env, root);

        var visibility: CliTestFailureDetailVisibility = .always;
        const message = if (has_message == 0) null else blk: {
            if (offset >= data.len) return null;
            visibility = switch (data[offset]) {
                0 => .always,
                1 => .verbose_only,
                else => return null,
            };
            offset += 1;
            const message_len = readU32(data, &offset) orelse return null;
            const message_len_usize: usize = @intCast(message_len);
            if (offset + message_len_usize > data.len) return null;
            const message = try ctx.gpa.dupe(u8, data[offset..][0..message_len_usize]);
            offset += message_len_usize;
            break :blk message;
        };

        try results.append(ctx.gpa, .{
            .result = result,
            .order = order,
            .region = region,
            .failure_detail = message,
            .failure_detail_visibility = visibility,
        });
    }
    if (offset != data.len) return null;

    var summary = summarizeTestResults(results.items);
    summary.cached_modules = 1;
    const owned_results = try results.toOwnedSlice(ctx.gpa);
    errdefer {
        for (owned_results) |result| {
            if (result.failure_detail) |message| ctx.gpa.free(message);
        }
        ctx.gpa.free(owned_results);
    }
    try module_results.append(ctx.gpa, .{
        .env = module.semantic.env,
        .path = module.path,
        .results = owned_results,
        .cached = true,
    });
    results_owned_by_module = true;
    return summary;
}

fn collectTestRootRequests(
    allocator: std.mem.Allocator,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) Allocator.Error![]check.CheckedArtifact.RootRequest {
    var roots = std.ArrayList(check.CheckedArtifact.RootRequest).empty;
    errdefer roots.deinit(allocator);

    for (artifact.root_requests.requests) |root| {
        if (root.kind != .test_expect) continue;
        try roots.append(allocator, root);
    }

    return try roots.toOwnedSlice(allocator);
}

fn testRootRegion(
    env: *const ModuleEnv,
    root: check.CheckedArtifact.RootRequest,
) base.Region {
    return switch (root.source) {
        .statement => |statement| env.store.getStatementRegion(statement),
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("CLI test invariant violated: test root was not published from an expect statement", .{});
            }
            unreachable;
        },
    };
}

fn testRootByOrder(
    roots: []const check.CheckedArtifact.RootRequest,
    order: u32,
) check.CheckedArtifact.RootRequest {
    for (roots) |root| {
        if (root.order == order) return root;
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("CLI test invariant violated: lowered test root order {d} was not in the explicit test root set", .{order});
    }
    unreachable;
}

fn interpreterTestFailureMessage(
    allocator: std.mem.Allocator,
    interpreter: *const eval.LirInterpreter,
    err: eval.LirInterpreter.Error,
) std.mem.Allocator.Error![]const u8 {
    const message = switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.RuntimeError => interpreter.getRuntimeErrorMessage() orelse "Roc runtime error",
        error.DivisionByZero => interpreter.getRuntimeErrorMessage() orelse "Division by zero",
        error.Crash => interpreter.getCrashMessage() orelse "Test crashed",
    };
    return try allocator.dupe(u8, message);
}

fn runCheckedArtifactTests(
    ctx: *CliCtx,
    build_env: *BuildEnv,
    module: BuildEnv.CompiledModuleInfo,
    opt: cli_args.OptLevel,
    cache_manager: ?*CacheManager,
    module_results: *std.ArrayList(CliModuleTestResult),
) (Allocator.Error || error{NoHomeDirectory})!CliTestRunSummary {
    const artifact = module.semantic.checked_artifact orelse return .{};
    const test_roots = try collectTestRootRequests(ctx.gpa, artifact);
    defer ctx.gpa.free(test_roots);
    if (test_roots.len == 0) return .{};

    if (try appendCachedCliTestResults(
        ctx,
        cache_manager,
        artifact,
        module,
        opt,
        test_roots,
        module_results,
    )) |cached_summary| {
        return cached_summary;
    }

    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, artifact);
    defer ctx.gpa.free(relation_artifacts);

    var lowered = try lir.CheckedPipeline.lowerCheckedModulesToLir(
        ctx.gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{ .requests = test_roots },
        .{
            .target_usize = base.target.TargetUsize.native,
        },
    );
    defer lowered.deinit();

    var hosted_fn_array = [_]echo_platform.host_abi.HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var echo_env_test = echo_platform.EchoEnv{ .std_io = ctx.io.std_io };
    var roc_ops = echo_platform.makeDefaultRocOps(&echo_env_test, &hosted_fn_array);
    var interpreter = try eval.LirInterpreter.init(
        ctx.gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &roc_ops,
    );
    defer interpreter.deinit();

    var results = std.ArrayList(CliTestResultItem).empty;
    errdefer {
        for (results.items) |result| {
            if (result.failure_detail) |message| ctx.gpa.free(message);
        }
        results.deinit(ctx.gpa);
    }

    var summary = CliTestRunSummary{};
    const root_procs = lowered.lir_result.root_procs.items;
    const root_metadata = lowered.lir_result.root_metadata.items;
    for (root_procs, root_metadata) |root_proc, metadata| {
        if (metadata.kind != .test_expect) continue;
        const root = testRootByOrder(test_roots, metadata.order);
        const region = testRootRegion(module.semantic.env, root);
        const proc = lowered.lir_result.store.getProcSpec(root_proc);
        const arg_layouts = try argLayoutsForProc(ctx.gpa, &lowered.lir_result.store, root_proc);
        defer ctx.gpa.free(arg_layouts);

        const eval_result = interpreter.eval(.{
            .proc_id = root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = proc.ret_layout,
        }) catch |err| {
            summary.failed += 1;
            try results.append(ctx.gpa, .{
                .result = .failed,
                .order = root.order,
                .region = region,
                .failure_detail = try interpreterTestFailureMessage(ctx.gpa, &interpreter, err),
                .failure_detail_visibility = .always,
            });
            continue;
        };

        const passed = switch (eval_result) {
            .value => |value| blk: {
                const ok = value.read(u8) != 0;
                interpreter.dropValue(value, proc.ret_layout);
                break :blk ok;
            },
        };

        if (passed) {
            summary.passed += 1;
            try results.append(ctx.gpa, .{ .result = .passed, .order = root.order, .region = region, .failure_detail = null });
        } else {
            summary.failed += 1;
            try results.append(ctx.gpa, .{
                .result = .failed,
                .order = root.order,
                .region = region,
                .failure_detail = try ctx.gpa.dupe(u8, "TEST FAILURE: expect failed"),
                .failure_detail_visibility = .verbose_only,
            });
        }
    }
    summary.modules_with_tests = 1;

    try storeCliTestResultsInCache(ctx, cache_manager, artifact, opt, results.items);

    try module_results.append(ctx.gpa, .{
        .env = module.semantic.env,
        .path = module.path,
        .results = try ctx.gpa.dupe(CliTestResultItem, results.items),
        .cached = false,
    });
    results.deinit(ctx.gpa);

    return summary;
}

fn rocTest(ctx: *CliCtx, args: cli_args.TestArgs) anyerror!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Start timing
    const start_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
        .roc_ctx = ctx.coreCtx(),
    };

    // --- Normal compilation path ---

    // Determine threading mode and thread count
    const thread_count: usize = args.max_threads orelse (std.Thread.getCpuCount() catch 1);
    const mode: Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    // Initialize BuildEnv for compilation
    const cwd = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.gpa) catch |err| {
        try stderr.print("Failed to get current working directory: {}\n", .{err});
        return err;
    };
    defer ctx.gpa.free(cwd);

    var build_env = BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative(), cwd, ctx.io.std_io) catch |err| {
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
        cache_manager.* = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
        build_env.setCacheManager(cache_manager);
    }

    build_env.discoverDependencies(args.path) catch |err| {
        _ = try build_env.renderDiagnostics(stderr);
        return err;
    };
    build_env.compileDiscovered() catch |err| {
        _ = try build_env.renderDiagnostics(stderr);
        return err;
    };

    const diag = try build_env.renderDiagnostics(stderr);
    if (diag.errors > 0) return error.CompilationFailed;

    const modules = try build_env.getCompiledModules(ctx.gpa);
    defer ctx.gpa.free(modules);

    var module_results = std.ArrayList(CliModuleTestResult).empty;
    defer {
        for (module_results.items) |module_result| {
            for (module_result.results) |result| {
                if (result.failure_detail) |message| ctx.gpa.free(message);
            }
            ctx.gpa.free(module_result.results);
        }
        module_results.deinit(ctx.gpa);
    }

    var total = CliTestRunSummary{};
    for (modules) |module| {
        const summary = try runCheckedArtifactTests(
            ctx,
            &build_env,
            module,
            args.opt,
            if (args.no_cache) null else build_env.cache_manager,
            &module_results,
        );
        total.passed += summary.passed;
        total.failed += summary.failed;
        total.modules_with_tests += summary.modules_with_tests;
        total.cached_modules += summary.cached_modules;
    }

    // Calculate elapsed time
    const end_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    const cached_suffix = if (total.modules_with_tests > 0 and total.cached_modules == total.modules_with_tests)
        " (cached)"
    else
        "";

    // Render the per-module bodies once into in-memory buffers so we can
    // print them after the summary line.
    var stdout_body = std.Io.Writer.Allocating.init(ctx.gpa);
    defer stdout_body.deinit();
    var stderr_body = std.Io.Writer.Allocating.init(ctx.gpa);
    defer stderr_body.deinit();

    try renderTestResultBodies(
        &stdout_body.writer,
        &stderr_body.writer,
        module_results.items,
        args.verbose,
    );

    // Report results
    if (total.failed == 0) {
        try stdout.print("All ({}) tests passed in {d:.1} ms.{s}\n", .{ total.passed, elapsed_ms, cached_suffix });
        try stdout.writeAll(stdout_body.written());
        return;
    }

    const total_tests = total.passed + total.failed;
    try stderr.print("Ran {} tests{s} in {d:.1}ms:\n    " ++ ansi_term.green ++ "{}" ++ ansi_term.reset ++ " passed\n    " ++ ansi_term.red ++ "{}" ++ ansi_term.reset ++ " failed\n", .{ total_tests, cached_suffix, elapsed_ms, total.passed, total.failed });
    try stdout.writeAll(stdout_body.written());
    try stderr.writeAll(stderr_body.written());

    return error.TestsFailed;
}

/// Walk the per-module test results and write the per-test body output to
/// the supplied writers. Verbose PASS lines go to `stdout_body`; FAIL
/// blocks (and verbose PASS lines for partially-failing runs) go to
/// `stderr_body`.
fn renderTestResultBodies(
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    module_results: []const CliModuleTestResult,
    verbose: bool,
) error{WriteFailed}!void {
    // Verbose PASS lines go to stdout in every case; FAIL blocks go to
    // stderr. This matches the pre-refactor layout.
    for (module_results) |module_result| {
        for (module_result.results) |result| {
            const region_info = module_result.env.calcRegionInfo(result.region);
            switch (result.result) {
                .passed => {
                    if (!verbose) continue;
                    try stdout_body.print("\x1b[32mPASS\x1b[0m: {s}:{}\n", .{ module_result.path, region_info.start_line_idx + 1 });
                },
                .failed => {
                    try printTestFailure(
                        stderr_body,
                        module_result.path,
                        module_result.env,
                        result.region,
                        region_info,
                        result.failure_detail,
                        result.failure_detail_visibility,
                        verbose,
                    );
                },
            }
        }
    }
}

/// Prints a formatted test failure to stderr, including the source snippet,
/// an optional doc comment from the preceding line, and an optional error message.
fn printTestFailure(
    stderr: *std.Io.Writer,
    path: []const u8,
    env: *const ModuleEnv,
    region: base.Region,
    region_info: base.RegionInfo,
    failure_detail: ?[]const u8,
    failure_detail_visibility: CliTestFailureDetailVisibility,
    verbose: bool,
) error{WriteFailed}!void {
    const src = env.getSourceAll();
    const error_src = src[region.start.offset..region.end.offset];

    // Check if the previous line is a doc comment
    const doc_comment: ?[]const u8 = blk: {
        const line_starts = env.getLineStarts();
        const curr_line_start_idx = region_info.start_line_idx;
        const curr_line_start = line_starts[curr_line_start_idx];
        const prev_line_start = if (curr_line_start_idx > 0) line_starts[curr_line_start_idx - 1] else break :blk null;
        const prev_line = std.mem.trimStart(u8, src[prev_line_start..curr_line_start], " ");
        if (std.mem.startsWith(u8, prev_line, "##")) {
            break :blk std.mem.trimEnd(u8, prev_line, " \r\n");
        }
        break :blk null;
    };

    try stderr.print("\n\x1b[31mFAIL\x1b[0m: {s}", .{path});

    try stderr.print("\x1b[31m", .{});

    // Calculate the width needed to right-align line numbers
    const last_line_num: usize = region_info.end_line_idx + 1;
    const num_width: usize = blk: {
        var digits: usize = 0;
        var n = last_line_num;
        while (n > 0) : (n /= 10) digits += 1;
        break :blk if (digits == 0) 1 else digits;
    };

    var line_num: usize = region_info.start_line_idx + 1;

    if (doc_comment) |comment| {
        line_num -= 1;
        try stderr.print("\n{d:[num_width]} \u{2502} ", .{ .d = line_num, .num_width = num_width });
        try stderr.print("{s}", .{comment});
        line_num += 1;
    }

    var lines = std.mem.splitScalar(u8, error_src, '\n');
    while (lines.next()) |line| {
        try stderr.print("\n{d:[num_width]} \u{2502} ", .{ .d = line_num, .num_width = num_width });
        try stderr.print("{s}", .{line});
        line_num += 1;
    }

    const should_print_detail = switch (failure_detail_visibility) {
        .always => true,
        .verbose_only => verbose,
    };
    if (should_print_detail) {
        if (failure_detail) |msg| {
            try stderr.print("\x1b[31m - {s}", .{msg});
        }
    }

    try stderr.print("\x1b[0m\n", .{});
}

const ReplMode = enum {
    interactive,
    batch,
};

fn rocRepl(ctx: *CliCtx, repl_args: cli_args.ReplArgs) anyerror!void {
    const stdout = ctx.io.stdout();
    const backend_kind = repl_args.opt.toBackend();
    const stdin = std.Io.File.stdin();
    const stdin_is_tty = stdin.isTty(ctx.io.std_io) catch false;
    const stdout_is_tty = std.Io.File.stdout().isTty(ctx.io.std_io) catch false;
    const mode: ReplMode = if (stdin_is_tty and stdout_is_tty) .interactive else .batch;
    const report_config = try replReportingConfig(ctx, repl_args, mode);

    if (mode == .interactive) {
        try stdout.writeAll("Roc REPL\nType :help for commands.\n");
        ctx.io.flush();
    }

    var reader = ReplLine.init(ctx.gpa);
    defer reader.deinit();

    var session = try ReplSession.init(ctx.gpa, ctx.io.std_io, backend_kind);
    defer session.deinit();

    var pending = std.ArrayList(u8).empty;
    defer pending.deinit(ctx.gpa);

    var should_exit = false;
    var had_diagnostics = false;
    while (!should_exit) {
        const prompt: []const u8 = if (mode == .interactive)
            if (pending.items.len == 0) "> " else "| "
        else
            "";

        const read_result = try reader.readLine(ctx.gpa, ctx.io.std_io, prompt, stdin);
        switch (read_result) {
            .eof => {
                if (pending.items.len > 0) {
                    should_exit = try processReplInput(ctx, &session, pending.items, report_config, mode, &had_diagnostics);
                    pending.clearRetainingCapacity();
                } else if (mode == .interactive) {
                    try stdout.writeAll("Goodbye!\n");
                }
                break;
            },
            .line => |raw_line| {
                defer ctx.gpa.free(raw_line);

                if (pending.items.len == 0 and std.mem.trim(u8, raw_line, " \t\r\n").len == 0) {
                    continue;
                }

                if (pending.items.len == 0 and std.mem.findAny(u8, raw_line, "\n\r") != null) {
                    should_exit = try processReplInput(ctx, &session, raw_line, report_config, mode, &had_diagnostics);
                    continue;
                }

                if (pending.items.len > 0) try pending.append(ctx.gpa, '\n');
                try pending.appendSlice(ctx.gpa, raw_line);

                switch (try session.inputStatus(pending.items)) {
                    .incomplete => {},
                    .complete, .invalid => {
                        should_exit = try processReplInput(ctx, &session, pending.items, report_config, mode, &had_diagnostics);
                        pending.clearRetainingCapacity();
                    },
                }
            },
        }
        ctx.io.flush();
    }

    if (mode == .batch and had_diagnostics) {
        return error.CliError;
    }
}

fn processReplInput(
    ctx: *CliCtx,
    session: *ReplSession,
    input: []const u8,
    report_config: reporting.ReportingConfig,
    mode: ReplMode,
    had_diagnostics: *bool,
) anyerror!bool {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    const statements = try session.splitInputIntoStatements(input);
    defer session.freeStatementSlices(statements);

    for (statements) |statement| {
        const result = try session.stepWithConfig(statement, report_config);
        defer result.deinit(ctx.gpa);

        switch (result) {
            .output => |output| {
                if (output.len > 0) {
                    try stdout.print("{s}\n", .{output});
                }
            },
            .diagnostic => |diagnostic| {
                had_diagnostics.* = true;
                if (diagnostic.len > 0) {
                    try stderr.print("{s}\n", .{diagnostic});
                }
            },
            .none => {},
            .exit => {
                if (mode == .interactive) {
                    try stdout.writeAll("Goodbye!\n");
                }
                return true;
            },
        }
    }

    return false;
}

fn replReportingConfig(ctx: *CliCtx, repl_args: cli_args.ReplArgs, mode: ReplMode) Allocator.Error!reporting.ReportingConfig {
    const stderr_is_tty = std.Io.File.stderr().isTty(ctx.io.std_io) catch false;
    const no_color_env = try envVarNonEmpty(ctx.gpa, "NO_COLOR");
    const force_color = try envVarNonEmpty(ctx.gpa, "FORCE_COLOR");
    const high_contrast = try envVarEquals(ctx.gpa, "ROC_HIGH_CONTRAST", "1");
    const color_disabled = repl_args.no_color or no_color_env;
    const should_color = !color_disabled and (force_color or (mode == .interactive and stderr_is_tty));

    var config = if (should_color)
        if (high_contrast) reporting.ReportingConfig.initHighContrast() else reporting.ReportingConfig.initColorTerminal()
    else
        reporting.ReportingConfig.initMarkdown();

    config.is_tty = stderr_is_tty;
    return config;
}

fn envVarNonEmpty(allocator: Allocator, name: []const u8) Allocator.Error!bool {
    const value = try getEnvVar(allocator, name) orelse return false;
    defer allocator.free(value);
    return value.len > 0;
}

fn envVarEquals(allocator: Allocator, name: []const u8, expected: []const u8) Allocator.Error!bool {
    const value = try getEnvVar(allocator, name) orelse return false;
    defer allocator.free(value);
    return std.mem.eql(u8, value, expected);
}

const glue = @import("glue");

fn rocGlue(ctx: *CliCtx, args: cli_args.GlueArgs) glue.GlueError!void {
    const temp_dir = createUniqueTempDir(ctx) catch {
        return error.TempDirCreation;
    };
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, temp_dir) catch {};
    return glue.rocGlue(ctx.gpa, ctx.io.stderr(), ctx.io.stdout(), .{
        .glue_spec = args.glue_spec,
        .output_dir = args.output_dir,
        .platform_path = args.platform_path,
        .backend = args.opt.toBackend(),
    }, temp_dir, ctx.io.std_io);
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(ctx: *CliCtx, args: cli_args.FormatArgs) anyerror!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();
    if (args.stdin) {
        fmt.formatStdin(ctx.gpa, ctx.io.std_io, std.Io.File.stdin(), std.Io.File.stdout(), stderr) catch |err| return err;
        return;
    }

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;
    var elapsed: u64 = undefined;
    var failure_count: usize = 0;
    var had_errors: bool = false;

    if (args.check) {
        var unformatted_files = std.ArrayList([]const u8).empty;
        defer unformatted_files.deinit(ctx.gpa);

        for (args.paths) |path| {
            var result = try fmt.formatPath(ctx.gpa, ctx.arena, std.Io.Dir.cwd(), path, true, ctx.io.std_io, stderr);
            defer result.deinit();
            if (result.unformatted_files) |files| {
                try unformatted_files.appendSlice(ctx.gpa, files.items);
            }
            failure_count += result.failure;
        }

        elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
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
            const result = try fmt.formatPath(ctx.gpa, ctx.arena, std.Io.Dir.cwd(), path, false, ctx.io.std_io, stderr);
            success_count += result.success;
            failure_count += result.failure;
        }
        elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
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
fn formatElapsedTime(writer: anytype, elapsed_ns: u64) error{WriteFailed}!void {
    const elapsed_ms_float = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    try writer.print("{d:.1} ms", .{elapsed_ms_float});
}

/// Helper function to format elapsed time as rounded integer milliseconds (no decimals)
fn formatElapsedTimeMs(writer: anytype, elapsed_ns: u64) error{WriteFailed}!void {
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

fn handleProcessFileError(err: anytype, stderr: anytype, path: []const u8) anyerror!void {
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

fn isCompilerOwnedBuiltinSourcePath(gpa: Allocator, cwd: []const u8, filepath: []const u8) Allocator.Error!bool {
    const abs_path = if (std.fs.path.isAbsolute(filepath))
        try std.fs.path.resolve(gpa, &.{filepath})
    else
        try std.fs.path.resolve(gpa, &.{ cwd, filepath });
    defer gpa.free(abs_path);

    const compiler_builtin_path = try std.fs.path.resolve(gpa, &.{build_options.compiler_builtin_roc_path});
    defer gpa.free(compiler_builtin_path);

    return std.mem.eql(u8, abs_path, compiler_builtin_path);
}

/// Check a Roc file using BuildEnv and preserve the BuildEnv for further processing
fn checkFileWithBuildEnvPreserved(
    ctx: *CliCtx,
    filepath: []const u8,
    _: bool,
    cache_config: CacheConfig,
    max_threads: ?usize,
) anyerror!CheckResultWithBuildEnv {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Determine threading mode and thread count
    // Default to multi-threaded with auto-detected CPU count; use -j1 for single-threaded
    const thread_count: usize = if (max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: compile.package.Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.gpa);
    defer ctx.gpa.free(cwd);
    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative(), cwd, ctx.io.std_io);
    if (try isCompilerOwnedBuiltinSourcePath(ctx.gpa, cwd, filepath)) {
        build_env.setRootModuleRole(.builtin);
    }

    build_env.compiler_version = build_options.compiler_version;
    // Note: We do NOT defer build_env.deinit() here because we're returning it

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try ctx.gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager when caller calls deinit
    }

    // Build the file (works for both app and module files)
    build_env.build(filepath) catch |err| {
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }

        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};

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

        var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
        for (drained, 0..) |mod, i| {
            reports[i] = .{
                .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
                .reports = mod.reports,
            };
        }
        build_env.freeDrainedReportsPathsOnly(drained);

        const timing = if (builtin.target.cpu.arch == .wasm32)
            CheckTimingInfo{}
        else
            build_env.getTimingInfo();

        return CheckResultWithBuildEnv{
            .check_result = .{
                .reports = reports,
                .timing = timing,
                .error_count = error_count,
                .warning_count = warning_count,
            },
            .build_env = build_env,
        };
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

                package_env.processModuleByName(module_name) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => break,
                };
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
    ctx: *CliCtx,
    filepath: []const u8,
    _: bool,
    cache_config: CacheConfig,
    max_threads: ?usize,
) anyerror!CheckResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Determine threading mode and thread count
    // Default to multi-threaded with auto-detected CPU count; use -j1 for single-threaded
    const thread_count: usize = if (max_threads) |t| t else (std.Thread.getCpuCount() catch 1);
    const mode: compile.package.Mode = if (thread_count <= 1) .single_threaded else .multi_threaded;

    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.gpa);
    defer ctx.gpa.free(cwd);
    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative(), cwd, ctx.io.std_io);

    build_env.compiler_version = build_options.compiler_version;
    defer build_env.deinit();

    // Set up cache manager if caching is enabled
    if (cache_config.enabled) {
        const cache_manager = try ctx.gpa.create(CacheManager);
        cache_manager.* = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
        build_env.setCacheManager(cache_manager);
        // Note: BuildEnv.deinit() will clean up the cache manager
    }
    build_env.setPostCheckPublicationMode(.platform_relations);

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
        const cache_stats = build_env.getBuildStats();

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
    const cache_stats = build_env.getBuildStats();

    if (comptime build_options.trace_build) {
        std.debug.print("[CLI] checkFileWithBuildEnv returning (defer deinit will run)\n", .{});
    }

    return CheckResult{
        .reports = reports,
        .timing = timing,
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

fn rocCheck(ctx: *CliCtx, args: cli_args.CheckArgs) anyerror!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
        .roc_ctx = ctx.coreCtx(),
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

    const elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));

    // Render reports grouped by module
    for (check_result.reports) |module| {
        for (module.reports) |*report| {

            // Render the diagnostic report to stderr
            try reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal());
        }
    }

    // Flush stderr to ensure all error output is visible
    ctx.io.flush();

    if (check_result.error_count > 0 or check_result.warning_count > 0) {
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) in ", .{
            check_result.error_count,
            check_result.warning_count,
        }) catch {};
        formatElapsedTimeMs(stderr, elapsed) catch {};
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
        stdout.print(" for {s}\n", .{args.path}) catch {};

        // Print verbose stats if requested
        if (args.verbose) {
            printVerboseStats(stdout, &check_result);
        }

        ctx.io.flush();
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

/// Start an HTTP server to serve the generated documentation.
///
/// Single-threaded blocking accept loop on 127.0.0.1:8080. GET-only.
/// Files are streamed up to a 10 MB cap; anything larger returns 500.
fn serveDocumentation(ctx: *CliCtx, docs_dir: []const u8) anyerror!void {
    const stdout = ctx.io.stdout();
    const io = ctx.io.std_io;

    var address = try std.Io.net.IpAddress.parse("127.0.0.1", 8080);
    var server = try address.listen(io, .{ .reuse_address = true });
    defer server.deinit(io);

    stdout.print("Visit http://localhost:8080 to view the docs at ./{s}/\n", .{docs_dir}) catch {};
    stdout.print("Press Ctrl+C to stop the server\n", .{}) catch {};
    ctx.io.flush();

    while (true) {
        const stream = server.accept(io) catch |err| {
            ctx.io.stderr().print("Error accepting connection: {}\n", .{err}) catch {};
            ctx.io.flush();
            continue;
        };
        handleConnection(ctx, stream, docs_dir) catch |err| {
            ctx.io.stderr().print("Error handling connection: {}\n", .{err}) catch {};
            ctx.io.flush();
        };
    }
}

/// Handle a single HTTP connection. Closes the stream before returning.
fn handleConnection(ctx: *CliCtx, stream: std.Io.net.Stream, docs_dir: []const u8) anyerror!void {
    const io = ctx.io.std_io;
    defer stream.close(io);

    var read_buffer: [4096]u8 = undefined;
    var conn_reader = stream.reader(io, &read_buffer);

    // Read whatever the client has sent so far (we only care about the
    // request line; the body of a GET is empty).
    var request_buf: [4096]u8 = undefined;
    var slices = [_][]u8{request_buf[0..]};
    const bytes_read = std.Io.Reader.readVec(&conn_reader.interface, &slices) catch |err| switch (err) {
        error.EndOfStream => 0,
        error.ReadFailed => return conn_reader.err orelse error.Unexpected,
    };
    if (bytes_read == 0) return;

    const request = request_buf[0..bytes_read];

    // Parse the request line: "METHOD PATH HTTP/x.y\r\n..."
    var lines = std.mem.splitSequence(u8, request, "\r\n");
    const request_line = lines.next() orelse return;

    var parts = std.mem.splitSequence(u8, request_line, " ");
    const method = parts.next() orelse return;
    const path = parts.next() orelse return;

    if (!std.mem.eql(u8, method, "GET")) {
        try sendResponse(io, stream, "405 Method Not Allowed", "text/plain", "Method Not Allowed");
        return;
    }

    // Resolve the URL path to a filesystem path under docs_dir.
    const file_path = try resolveFilePath(ctx.gpa, docs_dir, path);
    defer ctx.gpa.free(file_path);

    // Read the file (10 MB cap per response).
    const file_content = std.Io.Dir.cwd().readFileAlloc(io, file_path, ctx.gpa, .limited(10 * 1024 * 1024)) catch |err| {
        switch (err) {
            error.FileNotFound => try sendResponse(io, stream, "404 Not Found", "text/plain", "File Not Found"),
            else => try sendResponse(io, stream, "500 Internal Server Error", "text/plain", "Internal Server Error"),
        }
        return;
    };
    defer ctx.gpa.free(file_content);

    const content_type = getContentType(file_path);
    try sendResponse(io, stream, "200 OK", content_type, file_content);
}

/// Resolve the URL path against `docs_dir`, expanding directory paths to
/// their `index.html`. Caller owns the returned slice.
fn resolveFilePath(gpa: Allocator, docs_dir: []const u8, url_path: []const u8) Allocator.Error![]const u8 {
    const clean_path = if (url_path.len > 0 and url_path[0] == '/')
        url_path[1..]
    else
        url_path;

    // Empty or trailing-slash paths serve the directory's index.html.
    if (clean_path.len == 0 or clean_path[clean_path.len - 1] == '/') {
        return try std.fmt.allocPrint(gpa, "{s}/{s}index.html", .{ docs_dir, clean_path });
    }

    // If the last path component has an extension, serve it directly;
    // otherwise treat the path as a directory and serve its index.html.
    const last_slash = std.mem.findScalarLast(u8, clean_path, '/') orelse 0;
    const last_component = clean_path[last_slash..];
    const has_extension = std.mem.findScalar(u8, last_component, '.') != null;

    if (has_extension) {
        return try std.fmt.allocPrint(gpa, "{s}/{s}", .{ docs_dir, clean_path });
    } else {
        return try std.fmt.allocPrint(gpa, "{s}/{s}/index.html", .{ docs_dir, clean_path });
    }
}

/// Map a file extension to its HTTP Content-Type.
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

/// Send a full HTTP/1.1 response (headers + body) over a stream.
fn sendResponse(
    io: std.Io,
    stream: std.Io.net.Stream,
    status: []const u8,
    content_type: []const u8,
    body: []const u8,
) (Allocator.Error || error{WriteFailed})!void {
    var write_buffer: [4096]u8 = undefined;
    var stream_writer = stream.writer(io, &write_buffer);
    const w = &stream_writer.interface;

    try w.print(
        "HTTP/1.1 {s}\r\n" ++
            "Content-Type: {s}\r\n" ++
            "Content-Length: {d}\r\n" ++
            "Connection: close\r\n" ++
            "\r\n",
        .{ status, content_type, body.len },
    );
    try w.writeAll(body);
    try w.flush();
}

fn rocDocs(ctx: *CliCtx, args: cli_args.DocsArgs) anyerror!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
        .roc_ctx = ctx.coreCtx(),
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
    const elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));

    // Render reports grouped by module
    for (check_result.reports) |module| {
        for (module.reports) |*report| {

            // Render the diagnostic report to stderr
            reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, reporting.ReportingConfig.initColorTerminal()) catch |render_err| {
                stderr.print("Error rendering diagnostic report: {}", .{render_err}) catch {};
                // Fallback to just printing the title
                stderr.print("  {s}", .{report.title}) catch {};
            };
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

    // Print timing breakdown if requested
    if (args.time) {
        printTimingBreakdown(stdout, if (builtin.target.cpu.arch == .wasm32) null else check_result.timing);
    }

    // Generate documentation for all packages and modules
    try generateDocs(ctx, &result_with_env.build_env, args.path, args.output);

    stdout.print("\nGenerated docs for {s}\n", .{args.path}) catch {};

    // Start HTTP server if --serve flag is enabled
    if (args.serve) {
        try serveDocumentation(ctx, args.output);
    }
}

// Documentation generation uses the docs module's extraction pipeline.
// See src/docs/ for DocModel, extract, and render_type modules.

/// Generate documentation for the root and all its dependencies and imported modules.
///
/// Builds a PackageDocs by extracting documentation from all compiled modules,
/// then generates an HTML documentation site in the output directory.
fn generateDocs(
    ctx: *CliCtx,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
) anyerror!void {
    const DocModel = docs.DocModel;
    const extract = docs.extract;

    // Determine if we're documenting a platform or something else by checking the module path
    // If the path contains "platform", we're documenting a platform directly
    const is_documenting_platform = std.mem.find(u8, module_path, "platform") != null;

    // Collect ModuleDocs from all compiled modules
    var module_docs_list = std.ArrayList(DocModel.ModuleDocs).empty;
    defer {
        for (module_docs_list.items) |*mod| mod.deinit(ctx.gpa);
        module_docs_list.deinit(ctx.gpa);
    }

    var is_package = false;

    var sched_iter = build_env.schedulers.iterator();
    while (sched_iter.next()) |sched_entry| {
        const sched_pkg_name = sched_entry.key_ptr.*;
        const package_env = sched_entry.value_ptr.*;

        for (package_env.modules.items) |*module_state| {
            if (module_state.moduleEnv()) |mod_env| {
                // Skip platform main.roc modules when documenting an app
                // Platform modules are still included when documenting a platform directly
                if (mod_env.module_kind == .platform and !is_documenting_platform) {
                    continue;
                }

                // Skip package definition files — they just declare which modules
                // are exposed and don't contain docs of their own.
                if (mod_env.module_kind == .package) {
                    is_package = true;
                    continue;
                }

                var mod_docs = extract.extractModuleDocs(ctx.gpa, mod_env, sched_pkg_name, module_state.path) catch |err| {
                    std.debug.print("Warning: failed to extract docs for module {s}: {}\n", .{ module_state.name, err });
                    continue;
                };
                module_docs_list.append(ctx.gpa, mod_docs) catch {
                    mod_docs.deinit(ctx.gpa);
                    continue;
                };
            }
        }
    }

    // Determine the package name for the docs header.
    // For packages, use the parent directory name (e.g., "my_parser" from "my_parser/main.roc")
    // since the entry file is just a package definition.
    // For apps/platforms, use the filename without extension (e.g., "app" from "app.roc").
    const pkg_name = if (is_package)
        try ctx.gpa.dupe(u8, std.fs.path.basename(std.fs.path.dirname(module_path) orelse "."))
    else blk: {
        const basename = std.fs.path.basename(module_path);
        break :blk if (std.mem.endsWith(u8, basename, ".roc"))
            try ctx.gpa.dupe(u8, basename[0 .. basename.len - 4])
        else
            try ctx.gpa.dupe(u8, basename);
    };

    const modules_slice = try module_docs_list.toOwnedSlice(ctx.gpa);

    var package_docs = DocModel.PackageDocs{
        .name = pkg_name,
        .modules = modules_slice,
    };
    defer package_docs.deinit(ctx.gpa);

    // Remove existing output directory to ensure a clean build
    try std.Io.Dir.cwd().deleteTree(ctx.io.std_io, base_output_dir);

    // Create output directory
    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, base_output_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Generate HTML documentation site
    // TODO: support --format md and --format json output formats
    const render_html = docs.render_html;
    var broken_links: std.ArrayListUnmanaged(render_html.BrokenLink) = .empty;
    defer {
        for (broken_links.items) |bl| {
            ctx.gpa.free(bl.label);
            ctx.gpa.free(bl.resolved_anchor);
        }
        broken_links.deinit(ctx.gpa);
    }
    render_html.renderPackageDocs(ctx.gpa, ctx.io.std_io, &package_docs, base_output_dir, &broken_links) catch |err| {
        return err;
    };

    if (broken_links.items.len > 0) {
        std.debug.print("Error: {d} doc reference(s) point at non-existent anchors:\n", .{broken_links.items.len});
        for (broken_links.items) |bl| {
            const path = if (bl.source_path.len > 0) bl.source_path else bl.source_module;
            if (bl.source_line > 0) {
                std.debug.print("  {s}:{d}: [{s}] -> #{s}\n", .{ path, bl.source_line, bl.label, bl.resolved_anchor });
            } else {
                std.debug.print("  {s}: [{s}] -> #{s}\n", .{ path, bl.label, bl.resolved_anchor });
            }
        }
        return error.BrokenDocLinks;
    }
}

test {
    _ = @import("linker.zig");
}

test "appendWindowsQuotedArg" {
    const testing = std.testing;

    // Helper to test the quoting function
    const testQuote = struct {
        fn run(input: []const u8, expected: []const u8) anyerror!void {
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

test "user project src/build/roc/Builtin.roc is not compiler-owned builtin" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const user_project_root = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", tmp.sub_path[0..] });
    defer allocator.free(user_project_root);

    const classified_as_compiler_builtin = try isCompilerOwnedBuiltinSourcePath(
        allocator,
        user_project_root,
        "src/build/roc/Builtin.roc",
    );

    try testing.expect(!classified_as_compiler_builtin);
}

test "classifyNativeRunTermination preserves warning exit code" {
    const testing = std.testing;

    const result = classifyNativeRunTermination(.{ .exited = 0 }, 1);

    try testing.expect(result == .exit_code);
    try testing.expectEqual(@as(u8, 2), result.exit_code);
}

test "classifyNativeRunTermination preserves signal termination" {
    const testing = std.testing;

    const result = classifyNativeRunTermination(.{ .signal = @enumFromInt(11) }, 0);

    try testing.expect(result == .signal);
    try testing.expectEqual(@as(std.posix.SIG, @enumFromInt(11)), result.signal);
}

test "longestCommonParentDir" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const cases = [_]struct {
        paths: []const []const u8,
        expected: []const u8,
    }{
        // Single file: parent directory of that file.
        .{ .paths = &.{"/tmp/pkg/main.roc"}, .expected = "/tmp/pkg" },
        // Two files sharing a parent.
        .{ .paths = &.{ "/tmp/pkg/main.roc", "/tmp/pkg/Mod.roc" }, .expected = "/tmp/pkg" },
        // Files in sibling subdirectories: common parent.
        .{ .paths = &.{ "/tmp/nested/a/main.roc", "/tmp/nested/b/Mod.roc" }, .expected = "/tmp/nested" },
        // Names share a byte prefix but no directory boundary — must back up.
        .{ .paths = &.{ "/tmp/abc/a.roc", "/tmp/abd/b.roc" }, .expected = "/tmp" },
        // Only root in common.
        .{ .paths = &.{ "/etc/foo.roc", "/var/bar.roc" }, .expected = "/" },
        // Three files with same parent.
        .{ .paths = &.{ "/a/b/c/x.roc", "/a/b/c/y.roc", "/a/b/c/z.roc" }, .expected = "/a/b/c" },
        // Three files where the third narrows the common parent.
        .{ .paths = &.{ "/a/b/c/x.roc", "/a/b/c/y.roc", "/a/b/d/z.roc" }, .expected = "/a/b" },
    };

    for (cases) |tc| {
        const got = try longestCommonParentDir(allocator, tc.paths);
        defer allocator.free(got);
        testing.expectEqualStrings(tc.expected, got) catch |err| {
            std.debug.print("Failed case: expected='{s}' got='{s}'\n", .{ tc.expected, got });
            return err;
        };
    }
}
