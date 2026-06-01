//! Zig wrapper for LLD (LLVM Linker) functionality.
//! Provides a high-level interface for linking object files into executables.
//! Supports ELF, COFF, MachO, and WebAssembly targets.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const libc_finder = @import("libc_finder.zig");
const stack_probe = @import("stack_probe.zig");
const embedded_lld = @import("embedded_lld");
const RocTarget = @import("roc_target").RocTarget;
const cli_ctx = @import("CliCtx.zig");
const CliCtx = cli_ctx.CliCtx;
const Io = cli_ctx.Io;

/// The embedded LLD entrypoints are only linked into LLVM-enabled CLI builds.
const llvm_available = if (@import("builtin").is_test) false else @import("config").llvm;

/// Supported target formats for linking
pub const TargetFormat = embedded_lld.Format;

/// Target ABI for runtime-configurable linking
pub const TargetAbi = enum {
    musl,
    gnu,

    /// Convert from RocTarget to TargetAbi
    pub fn fromRocTarget(roc_target: RocTarget) TargetAbi {
        return if (roc_target.isStatic()) .musl else .gnu;
    }
};

/// Default WASM initial memory: 64MB
pub const DEFAULT_WASM_INITIAL_MEMORY: usize = 64 * 1024 * 1024;

/// Default WASM stack size: 8MB
pub const DEFAULT_WASM_STACK_SIZE: usize = 8 * 1024 * 1024;

/// Configuration for the linker, specifying target format, ABI, paths, and linking options.
pub const LinkConfig = struct {
    /// Target format to use for linking
    target_format: TargetFormat = TargetFormat.detectFromSystem(),

    /// Target ABI - determines static vs dynamic linking strategy
    target_abi: ?TargetAbi = null, // null means detect from system

    /// Target OS tag - for cross-compilation support
    target_os: ?std.Target.Os.Tag = null, // null means detect from system

    /// Target CPU architecture - for cross-compilation support
    target_arch: ?std.Target.Cpu.Arch = null, // null means detect from system

    /// Output executable path
    output_path: []const u8,

    /// Input object files to link
    object_files: []const []const u8,

    /// Platform-provided files to link before object files (e.g., Scrt1.o, crti.o, host.o)
    platform_files_pre: []const []const u8 = &.{},

    /// Platform-provided files to link after object files (e.g., crtn.o)
    platform_files_post: []const []const u8 = &.{},

    /// Additional linker flags
    extra_args: []const []const u8 = &.{},

    /// Whether to allow LLD to exit early on errors
    can_exit_early: bool = false,

    /// Whether to disable linker output
    disable_output: bool = false,

    /// Initial memory size for WASM targets (bytes). This is the amount of linear memory
    /// available to the WASM module at runtime. Must be a multiple of 64KB (WASM page size).
    wasm_initial_memory: usize = DEFAULT_WASM_INITIAL_MEMORY,

    /// Stack size for WASM targets (bytes). This is the amount of memory reserved for the
    /// call stack within the WASM linear memory. Must be a multiple of 16 (stack alignment).
    wasm_stack_size: usize = DEFAULT_WASM_STACK_SIZE,

    /// Platform files directory (absolute path). Used to find platform-bundled sysroots.
    /// For example, if this is "/path/to/platform/targets", the linker will look for
    /// "/path/to/platform/targets/macos-sysroot" when linking for macOS.
    platform_files_dir: ?[]const u8 = null,

    /// Per-build scratch directory for linker-generated intermediate object files
    /// (e.g. the Windows stack_probe.obj). When multiple `roc build` processes
    /// run in parallel against the same exe_dir they would otherwise collide on
    /// a shared filename. When null, the linker falls back to the directory
    /// containing the running `roc` executable.
    scratch_dir: ?[]const u8 = null,
};

/// Errors that can occur during linking
pub const LinkError = error{
    LinkFailed,
    OutOfMemory,
    InvalidArguments,
    LLVMNotAvailable,
    WindowsSDKNotFound,
    DarwinSysrootNotFound,
} || std.zig.system.DetectError;

/// Resolve the path of the currently running executable, host-OS specific.
///
/// Zig 0.16 removed `std.fs.selfExePath` and the private std helpers live inside
/// `std.Io.Threaded` / `std.Io.Dispatch`. We need a cross-host implementation
/// because the linker runs on Linux/macOS/Windows but may target any OS.
fn selfExePath(std_io: std.Io, buf: []u8) ![]const u8 {
    switch (comptime builtin.os.tag) {
        .macos, .ios, .tvos, .watchos, .visionos => {
            var n: u32 = @intCast(buf.len);
            if (std.c._NSGetExecutablePath(buf.ptr, &n) != 0) return error.NameTooLong;
            return std.mem.sliceTo(buf, 0);
        },
        .linux => {
            const len = try std.Io.Dir.readLinkAbsolute(std_io, "/proc/self/exe", buf);
            return buf[0..len];
        },
        .windows => {
            // The PEB's ImagePathName contains the full path to the running exe.
            const image_path_name = std.os.windows.peb().ProcessParameters.ImagePathName;
            const wide = image_path_name.sliceZ();
            const written = std.unicode.wtf16LeToWtf8(buf, wide);
            return buf[0..written];
        },
        else => return error.UnsupportedOs,
    }
}

/// Get the directory containing the currently running executable.
fn getSelfExeDir(allocator: std.mem.Allocator, std_io: std.Io) ![]const u8 {
    var symlink_path_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const symlink_path = try selfExePath(std_io, &symlink_path_buf);
    var real_path_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const exe_path_len = std.Io.Dir.cwd().realPathFile(std_io, symlink_path, &real_path_buf) catch return error.OutOfMemory;
    const exe_path = real_path_buf[0..exe_path_len];
    return allocator.dupe(u8, std.fs.path.dirname(exe_path) orelse return error.OutOfMemory);
}

/// Find the Darwin sysroot directory at runtime.
/// First looks for a 'darwin' directory next to the executable (for distributed builds),
/// then falls back to the compile-time path (for local development builds).
fn findDarwinSysroot(allocator: std.mem.Allocator, std_io: std.Io) ![]const u8 {
    const exe_dir = getSelfExeDir(allocator, std_io) catch |err| {
        std.log.warn("Failed to resolve executable path: {}, falling back to compile-time path", .{err});
        return build_options.darwin_sysroot;
    };

    // Try to find 'darwin' directory next to executable (for distributed builds)
    const runtime_sysroot = std.fs.path.join(allocator, &.{ exe_dir, "darwin" }) catch {
        return build_options.darwin_sysroot;
    };

    // Check if the runtime path exists and contains the expected libSystem.tbd
    const tbd_path = std.fs.path.join(allocator, &.{ runtime_sysroot, "usr", "lib", "libSystem.tbd" }) catch {
        return build_options.darwin_sysroot;
    };

    std.Io.Dir.cwd().access(std_io, tbd_path, .{}) catch {
        // Runtime path doesn't exist, fall back to compile-time path (local dev builds)
        return build_options.darwin_sysroot;
    };

    return runtime_sysroot;
}

/// Find a platform-provided sysroot for macOS cross-compilation.
/// Looks for 'macos-sysroot' directory in the platform's files directory.
/// For example, if platform_files_dir is "/path/to/platform/targets",
/// this looks for "/path/to/platform/targets/macos-sysroot/".
fn findPlatformSysroot(allocator: std.mem.Allocator, std_io: std.Io, platform_files_dir: ?[]const u8) ?[]const u8 {
    const files_dir = platform_files_dir orelse return null;

    // Look for macos-sysroot in the platform files directory
    const sysroot_path = std.fs.path.join(allocator, &.{ files_dir, "macos-sysroot" }) catch return null;

    // Verify it exists and has the expected structure (usr/lib/libSystem.tbd)
    const lib_path = std.fs.path.join(allocator, &.{ sysroot_path, "usr", "lib", "libSystem.tbd" }) catch return null;
    std.Io.Dir.cwd().access(std_io, lib_path, .{}) catch return null;

    std.log.info("Using platform-provided macOS sysroot: {s}", .{sysroot_path});
    return sysroot_path;
}

/// Discover frameworks in a directory and add -framework flags for each.
/// This allows platforms to control their framework dependencies by choosing
/// which frameworks to bundle in their sysroot.
/// Only links frameworks that have a .tbd file (skips header-only frameworks).
fn discoverAndLinkFrameworks(allocator: std.mem.Allocator, std_io: std.Io, args: *std.array_list.Managed([]const u8), frameworks_dir: []const u8) LinkError!void {
    var dir = std.Io.Dir.cwd().openDir(std_io, frameworks_dir, .{ .iterate = true }) catch {
        // No frameworks directory - that's fine, just skip
        return;
    };
    defer dir.close(std_io);

    var iter = dir.iterate();
    while (iter.next(std_io) catch return) |entry| {
        if (entry.kind != .directory) continue;

        // Framework directories end with .framework
        if (std.mem.endsWith(u8, entry.name, ".framework")) {
            // Extract framework name (remove .framework suffix)
            const fw_name = entry.name[0 .. entry.name.len - ".framework".len];

            // Check if framework has a TBD file (skip header-only frameworks)
            // TBD can be at X.framework/X.tbd or X.framework/Versions/Current/X.tbd
            const tbd_name = std.fmt.allocPrint(allocator, "{s}.tbd", .{fw_name}) catch return LinkError.OutOfMemory;
            const tbd_path1 = std.fs.path.join(allocator, &.{ frameworks_dir, entry.name, tbd_name }) catch return LinkError.OutOfMemory;
            const tbd_path2 = std.fs.path.join(allocator, &.{ frameworks_dir, entry.name, "Versions", "Current", tbd_name }) catch return LinkError.OutOfMemory;

            const has_tbd = std.Io.Dir.cwd().access(std_io, tbd_path1, .{}) catch std.Io.Dir.cwd().access(std_io, tbd_path2, .{}) catch null;
            if (has_tbd == null) continue; // Skip frameworks without TBD files

            const fw_name_copy = allocator.dupe(u8, fw_name) catch return LinkError.OutOfMemory;
            try args.append("-framework");
            try args.append(fw_name_copy);
        }
    }
}

fn isStaticArchive(std_io: std.Io, path: []const u8) LinkError!bool {
    var file = std.Io.Dir.cwd().openFile(std_io, path, .{ .mode = .read_only }) catch return LinkError.InvalidArguments;
    defer file.close(std_io);

    var magic: [8]u8 = undefined;
    const n = file.readPositionalAll(std_io, &magic, 0) catch return LinkError.InvalidArguments;
    return n == magic.len and
        (std.mem.eql(u8, magic[0..], "!<arch>\n") or
            std.mem.eql(u8, magic[0..], "!<thin>\n"));
}

fn appendPlatformFile(
    ctx: *CliCtx,
    args: *std.array_list.Managed([]const u8),
    platform_file: []const u8,
    is_macos: bool,
    is_windows: bool,
) LinkError!void {
    if (is_windows) {
        const whole_arg = std.fmt.allocPrint(ctx.arena, "/wholearchive:{s}", .{platform_file}) catch return LinkError.OutOfMemory;
        try args.append(whole_arg);
    } else if (is_macos and try isStaticArchive(ctx.io.std_io, platform_file)) {
        try args.append("-force_load");
        try args.append(platform_file);
    } else {
        try args.append(platform_file);
    }
}

/// Build the linker command arguments for the given configuration.
/// Returns the args array that would be passed to LLD.
/// This is used both by link() and formatLinkCommand().
fn buildLinkArgs(ctx: *CliCtx, config: LinkConfig) LinkError!std.array_list.Managed([]const u8) {
    // Use arena allocator for all temporary allocations
    // Pre-allocate capacity to avoid reallocations (typical command has 20-40 args)
    var args = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 64) catch return LinkError.OutOfMemory;

    // Add platform-specific linker name and arguments
    // Use target OS if provided, otherwise fall back to host OS
    const target_os = config.target_os orelse builtin.target.os.tag;
    const target_arch = config.target_arch orelse builtin.target.cpu.arch;

    switch (target_os) {
        .macos => {
            // Add linker name for macOS
            try args.append("ld64.lld");

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Suppress LLD warnings
            try args.append("-w");

            // Add architecture flag
            try args.append("-arch");
            switch (target_arch) {
                .aarch64 => try args.append("arm64"),
                .x86_64 => try args.append("x86_64"),
                else => try args.append("arm64"), // default to arm64
            }

            // Add platform version - use a conservative minimum that works across macOS versions
            try args.append("-platform_version");
            try args.append("macos");
            try args.append("13.0"); // minimum deployment target
            try args.append("13.0"); // SDK version

            // Try to find a platform-provided sysroot first (for cross-compilation with bundled frameworks)
            // Falls back to Roc's bundled darwin sysroot (minimal, only has libSystem.tbd)
            try args.append("-syslibroot");
            if (findPlatformSysroot(ctx.arena, ctx.io.std_io, config.platform_files_dir)) |platform_sysroot| {
                try args.append(platform_sysroot);

                // Add framework search path to help linker resolve framework dependencies
                const fw_path = std.fs.path.join(ctx.arena, &.{ platform_sysroot, "System", "Library", "Frameworks" }) catch return LinkError.OutOfMemory;
                try args.append("-F");
                try args.append(fw_path);

                // Add library path for libobjc and other usr/lib dependencies
                const lib_path = std.fs.path.join(ctx.arena, &.{ platform_sysroot, "usr", "lib" }) catch return LinkError.OutOfMemory;
                try args.append("-L");
                try args.append(lib_path);

                // Auto-discover and link all frameworks bundled in the platform sysroot.
                // This keeps the compiler generic - platforms explicitly control their
                // dependencies by choosing which frameworks to bundle in their sysroot.
                try discoverAndLinkFrameworks(ctx.arena, ctx.io.std_io, &args, fw_path);
            } else {
                const darwin_sysroot = findDarwinSysroot(ctx.arena, ctx.io.std_io) catch return LinkError.DarwinSysrootNotFound;
                try args.append(darwin_sysroot);
            }

            // Link against system libraries on macOS
            try args.append("-lSystem");

            // Link C++ standard library if Tracy is enabled
            if (build_options.enable_tracy) {
                try args.append("-lc++");
            }
        },
        .linux => {
            // Add linker name for Linux
            try args.append("ld.lld");

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Prevent hidden linker behaviour -- only explicit platfor mdependencies
            try args.append("-nostdlib");
            // Remove unused sections to reduce binary size
            try args.append("--gc-sections");
            // TODO make the confirugable instead of using comments
            // Suppress linker warnings
            try args.append("-w");
            // Verbose linker for debugging (uncomment as needed)
            // try args.append("--verbose");
            // try args.append("--print-map");
            // try args.append("--error-limit=0");

            // Determine target ABI
            const target_abi = config.target_abi orelse if (builtin.target.abi == .musl) TargetAbi.musl else TargetAbi.gnu;

            switch (target_abi) {
                .musl => {
                    // Static musl linking
                    try args.append("-static");
                },
                .gnu => {
                    // Dynamic GNU linking - dynamic linker path is handled by caller
                    // for cross-compilation. Only detect locally for native builds
                    if (config.extra_args.len == 0) {
                        // Native build - try to detect dynamic linker
                        if (libc_finder.findLibc(ctx)) |libc_info| {
                            // We need to copy the path since args holds references
                            try args.append("-dynamic-linker");
                            try args.append(libc_info.dynamic_linker);
                        } else |err| {
                            // Fallback to hardcoded path based on architecture
                            std.log.warn("Failed to detect libc: {}, using fallback", .{err});
                            try args.append("-dynamic-linker");
                            const fallback_ld = switch (builtin.target.cpu.arch) {
                                .x86_64 => "/lib64/ld-linux-x86-64.so.2",
                                .aarch64 => "/lib/ld-linux-aarch64.so.1",
                                .x86 => "/lib/ld-linux.so.2",
                                else => "/lib/ld-linux.so.2",
                            };
                            try args.append(fallback_ld);
                        }
                    }
                    // Otherwise, dynamic linker is set via extra_args from caller
                },
            }

            // Link C++ standard library if Tracy is enabled
            if (build_options.enable_tracy) {
                try args.append("-lstdc++");
            }
        },
        .windows => {
            // Add linker name for Windows COFF
            try args.append("lld-link");

            const query = std.Target.Query{
                .cpu_arch = target_arch,
                .os_tag = .windows,
                .abi = .msvc,
                .ofmt = .coff,
            };

            const target = try std.zig.system.resolveTargetQuery(ctx.io.std_io, query);

            var environ_map = std.process.Environ.empty.createMap(ctx.arena) catch return error.WindowsSDKNotFound;
            defer environ_map.deinit();
            const native_libc = std.zig.LibCInstallation.findNative(ctx.arena, ctx.io.std_io, .{
                .target = &target,
                .environ_map = &environ_map,
            }) catch return error.WindowsSDKNotFound;

            if (native_libc.crt_dir) |lib_dir| {
                const lib_arg = try std.fmt.allocPrint(ctx.arena, "/libpath:{s}", .{lib_dir});
                try args.append(lib_arg);
            } else return error.WindowsSDKNotFound;

            if (native_libc.msvc_lib_dir) |lib_dir| {
                const lib_arg = try std.fmt.allocPrint(ctx.arena, "/libpath:{s}", .{lib_dir});
                try args.append(lib_arg);
            } else return error.WindowsSDKNotFound;

            if (native_libc.kernel32_lib_dir) |lib_dir| {
                const lib_arg = try std.fmt.allocPrint(ctx.arena, "/libpath:{s}", .{lib_dir});
                try args.append(lib_arg);
            } else return error.WindowsSDKNotFound;

            // Add output argument using Windows style
            const out_arg = try std.fmt.allocPrint(ctx.arena, "/out:{s}", .{config.output_path});
            try args.append(out_arg);

            // Add subsystem flag (console by default)
            try args.append("/subsystem:console");

            // Add machine type based on target architecture
            switch (target_arch) {
                .x86_64 => try args.append("/machine:x64"),
                .x86 => try args.append("/machine:x86"),
                .aarch64 => try args.append("/machine:arm64"),
                else => try args.append("/machine:x64"), // default to x64
            }

            // Set stack size to 64 MiB. Windows default is 1 MiB. Zig 0.16 codegen
            // produces bigger frames (e.g. std.fs PathSpace buffers ~64 KiB each) than
            // 0.15, and recursion-heavy Roc programs blow past 16 MiB before the
            // platform host's overflow handler can run. Match eval-test-runner.exe
            // and roc.exe.
            try args.append("/stack:67108864");

            // These are part of the core Windows OS and are available on all Windows systems
            try args.append("/defaultlib:kernel32");
            try args.append("/defaultlib:ntdll");
            try args.append("/defaultlib:msvcrt");

            // Suppress warnings using Windows style
            try args.append("/ignore:4217"); // Ignore locally defined symbol imported warnings
            try args.append("/ignore:4049"); // Ignore locally defined symbol imported warnings

            // Link C++ standard library if Tracy is enabled
            if (build_options.enable_tracy) {
                try args.append("/defaultlib:msvcprt");
            }

            // Generate and link stack probe object for ___chkstk_ms
            // This is needed when linking Zig-compiled code (like platform hosts) that uses
            // the MinGW ABI, which requires ___chkstk_ms for functions with large stack frames.
            if (target_arch == .x86_64) {
                const stack_probe_obj = stack_probe.generateStackProbeObject(ctx.arena) catch return LinkError.OutOfMemory;
                // Prefer a per-build scratch dir so parallel `roc build` invocations
                // don't race on a shared zig-out/bin/stack_probe.obj (truncated
                // writes from one linker can corrupt what another linker is reading).
                const probe_dir = config.scratch_dir orelse (std.process.executableDirPathAlloc(ctx.io.std_io, ctx.arena) catch return LinkError.OutOfMemory);
                const stack_probe_path = std.fs.path.join(ctx.arena, &.{
                    probe_dir,
                    "stack_probe.obj",
                }) catch return LinkError.OutOfMemory;
                @import("backend").writeFileWindowsAvSafe(ctx.io.std_io, stack_probe_path, stack_probe_obj) catch return LinkError.OutOfMemory;
                try args.append(stack_probe_path);
            }
        },
        .freestanding => {
            // WebAssembly linker (wasm-ld) for freestanding wasm32 target
            try args.append("wasm-ld");

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Don't look for _start or _main entry point - we export specific functions
            try args.append("--no-entry");

            // Export all symbols (the Roc app exports its entrypoints)
            try args.append("--export-all");

            // Disable garbage collection to preserve host-defined exports (init, handleEvent, update)
            // Without this, wasm-ld removes symbols that aren't referenced by the Roc app
            try args.append("--no-gc-sections");

            // Allow undefined symbols (imports from host environment)
            try args.append("--allow-undefined");

            // Set initial memory size (configurable, default 64MB)
            // Must be a multiple of 64KB (WASM page size)
            const initial_memory_str = std.fmt.allocPrint(ctx.arena, "--initial-memory={d}", .{config.wasm_initial_memory}) catch return LinkError.OutOfMemory;
            try args.append(initial_memory_str);

            // Set stack size (configurable, default 8MB)
            // Must be a multiple of 16 (stack alignment)
            const stack_size_str = std.fmt.allocPrint(ctx.arena, "stack-size={d}", .{config.wasm_stack_size}) catch return LinkError.OutOfMemory;
            try args.append("-z");
            try args.append(stack_size_str);
        },
        else => {
            // Generic ELF linker
            try args.append("ld.lld");

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Suppress LLD warnings
            try args.append("-w");
        },
    }

    // For WASM targets, wrap platform files in --whole-archive to include all symbols
    // This ensures host exports (init, handleEvent, update) aren't stripped even when
    // not referenced by other code
    const is_wasm = config.target_format == .wasm;
    const is_macos = target_os == .macos;
    const is_windows = target_os == .windows;
    if (is_wasm and config.platform_files_pre.len > 0) {
        try args.append("--whole-archive");
    }

    // Add platform-provided files that come before object files.
    // Static platform archives need all members included so host-exported
    // functions like init, handleEvent, and update are retained.
    if (config.platform_files_pre.len > 0) {
        if (!is_macos and !is_windows) {
            // ELF targets use --whole-archive
            try args.append("--whole-archive");
        }
        for (config.platform_files_pre) |platform_file| {
            try appendPlatformFile(ctx, &args, platform_file, is_macos, is_windows);
        }
        if (!is_macos and !is_windows) {
            try args.append("--no-whole-archive");
        }
    }

    // Add object files (Roc shim libraries - don't need --whole-archive)
    for (config.object_files) |obj_file| {
        try args.append(obj_file);
    }

    // Add platform-provided files that come after object files
    // Also use --whole-archive in case there are static libs here too
    if (config.platform_files_post.len > 0) {
        if (!is_macos and !is_windows) {
            try args.append("--whole-archive");
        }
        for (config.platform_files_post) |platform_file| {
            try appendPlatformFile(ctx, &args, platform_file, is_macos, is_windows);
        }
        if (!is_macos and !is_windows) {
            try args.append("--no-whole-archive");
        }
    }

    // Add any extra arguments
    for (config.extra_args) |extra_arg| {
        try args.append(extra_arg);
    }

    return args;
}

/// Link object files into an executable using LLD
pub fn link(ctx: *CliCtx, config: LinkConfig) LinkError!void {
    // Check if LLVM is available at compile time
    if (comptime !llvm_available) {
        return LinkError.LLVMNotAvailable;
    }

    const args = try buildLinkArgs(ctx, config);

    // Debug: Print the linker command
    std.log.debug("Linker command:", .{});
    for (args.items) |arg| {
        std.log.debug("  {s}", .{arg});
    }

    embedded_lld.link(ctx.arena, config.target_format, args.items, .{
        .can_exit_early = config.can_exit_early,
        .disable_output = config.disable_output,
    }) catch |err| switch (err) {
        error.OutOfMemory => return LinkError.OutOfMemory,
        error.LinkFailed => return LinkError.LinkFailed,
    };

    // On macOS, ld64.lld does not write LC_MAIN.stacksize from a `-stack_size`
    // arg (zig's own MachO linker does, but we link via the LLVM ld64.lld C
    // API). Patch it ourselves so the main thread gets 64 MiB instead of the
    // 8 MiB default. The host's embedded interpreter shim recurses one Zig
    // frame per Roc call (max_call_depth = 1024), and with Zig 0.16 frame
    // sizes 1024 frames overflows 8 MiB before the interpreter's depth check
    // can crash cleanly. Matches /stack:67108864 used for Windows above.
    if (config.target_format == .macho) {
        patchMachoStackSize(config.output_path, 64 * 1024 * 1024, ctx.io.std_io) catch |err| {
            std.log.warn("Failed to patch LC_MAIN stacksize for {s}: {}", .{ config.output_path, err });
        };
        // Patching invalidated the ad-hoc code signature ld64.lld wrote; on
        // macOS 14+ the kernel SIGKILLs (137) binaries with bad signatures,
        // so re-sign ad-hoc via /usr/bin/codesign.
        resignMachoAdHoc(ctx, config.output_path) catch |err| {
            std.log.warn("Failed to re-sign {s} after stacksize patch: {}", .{ config.output_path, err });
        };
    }
}

const macho = std.macho;

/// Patch a freshly-linked macOS executable's LC_MAIN stacksize field. See the
/// callsite in `link` for why this is needed.
fn patchMachoStackSize(path: []const u8, stacksize: u64, io: std.Io) !void {
    var file = try std.Io.Dir.cwd().openFile(io, path, .{ .mode = .read_write });
    defer file.close(io);

    var header: macho.mach_header_64 = undefined;
    const header_n = try file.readPositionalAll(io, std.mem.asBytes(&header), 0);
    if (header_n != @sizeOf(macho.mach_header_64)) return error.UnexpectedEof;
    if (header.magic != macho.MH_MAGIC_64) return error.NotMacho64;

    var offset: u64 = @sizeOf(macho.mach_header_64);
    var i: u32 = 0;
    while (i < header.ncmds) : (i += 1) {
        var lc: macho.load_command = undefined;
        const lc_n = try file.readPositionalAll(io, std.mem.asBytes(&lc), offset);
        if (lc_n != @sizeOf(macho.load_command)) return error.UnexpectedEof;
        if (lc.cmd == .MAIN) {
            // entry_point_command layout: cmd (u32) + cmdsize (u32) + entryoff (u64) + stacksize (u64).
            const stacksize_offset = offset + 16;
            try file.writePositionalAll(io, std.mem.asBytes(&stacksize), stacksize_offset);
            return;
        }
        offset += lc.cmdsize;
    }
    // No LC_MAIN — leave as-is (e.g. dylibs or unusual layouts).
}

fn resignMachoAdHoc(ctx: *CliCtx, path: []const u8) !void {
    const result = try std.process.run(ctx.arena, ctx.io.std_io, .{
        .argv = &.{ "/usr/bin/codesign", "--force", "--sign", "-", path },
    });
    defer ctx.arena.free(result.stdout);
    defer ctx.arena.free(result.stderr);
    switch (result.term) {
        .exited => |code| if (code != 0) return error.CodesignFailed,
        else => return error.CodesignFailed,
    }
}

fn findArg(args: []const []const u8, needle: []const u8) ?usize {
    for (args, 0..) |arg, i| {
        if (std.mem.eql(u8, arg, needle)) return i;
    }
    return null;
}

/// Format link configuration as a shell command string for manual reproduction.
/// Useful for debugging linking issues by allowing users to run the linker manually.
pub fn formatLinkCommand(ctx: *CliCtx, config: LinkConfig) LinkError![]const u8 {
    const args = try buildLinkArgs(ctx, config);

    // Join args with spaces, quoting paths that contain spaces or special chars
    var result = std.array_list.Managed(u8).init(ctx.arena);

    for (args.items, 0..) |arg, i| {
        if (i > 0) result.append(' ') catch return LinkError.OutOfMemory;

        // Quote if contains spaces or shell metacharacters
        const needs_quoting = std.mem.findAny(u8, arg, " \t'\"\\$`") != null;
        if (needs_quoting) {
            result.append('\'') catch return LinkError.OutOfMemory;
            // Escape single quotes within the string
            for (arg) |c| {
                if (c == '\'') {
                    result.appendSlice("'\\''") catch return LinkError.OutOfMemory;
                } else {
                    result.append(c) catch return LinkError.OutOfMemory;
                }
            }
            result.append('\'') catch return LinkError.OutOfMemory;
        } else {
            result.appendSlice(arg) catch return LinkError.OutOfMemory;
        }
    }

    return result.toOwnedSlice() catch return LinkError.OutOfMemory;
}

/// Convenience function to link two object files into an executable
pub fn linkTwoObjects(ctx: *CliCtx, obj1: []const u8, obj2: []const u8, output: []const u8) LinkError!void {
    if (comptime !llvm_available) {
        return LinkError.LLVMNotAvailable;
    }

    const config = LinkConfig{
        .output_path = output,
        .object_files = &.{ obj1, obj2 },
    };

    return link(ctx, config);
}

/// Convenience function to link multiple object files into an executable
pub fn linkObjects(ctx: *CliCtx, object_files: []const []const u8, output: []const u8) LinkError!void {
    if (comptime !llvm_available) {
        return LinkError.LLVMNotAvailable;
    }

    const config = LinkConfig{
        .output_path = output,
        .object_files = object_files,
    };

    return link(ctx, config);
}

test "link config creation" {
    const config = LinkConfig{
        .output_path = "test_output",
        .object_files = &.{ "file1.o", "file2.o" },
    };

    try std.testing.expect(config.target_format == TargetFormat.detectFromSystem());
    try std.testing.expectEqualStrings("test_output", config.output_path);
    try std.testing.expectEqual(@as(usize, 2), config.object_files.len);
    try std.testing.expectEqual(@as(usize, 0), config.platform_files_pre.len);
    try std.testing.expectEqual(@as(usize, 0), config.platform_files_post.len);
}

test "target format detection" {
    const detected = TargetFormat.detectFromSystem();

    // Should detect a valid format
    switch (detected) {
        .elf, .coff, .macho, .wasm => {},
    }
}

test "macOS platform archives use scoped force_load" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var archive_file = try tmp.dir.createFile(std.testing.io, "libhost.a", .{});
    defer archive_file.close(std.testing.io);
    try archive_file.writeStreamingAll(std.testing.io, "!<arch>\n");

    const archive_path = try tmp.dir.realPathFileAlloc(std.testing.io, "libhost.a", std.testing.allocator);
    defer std.testing.allocator.free(archive_path);

    var arena_instance = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_instance.deinit();

    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(std.testing.allocator, arena_instance.allocator(), &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    const config = LinkConfig{
        .target_format = .macho,
        .target_os = .macos,
        .target_arch = .x86_64,
        .output_path = "test_output",
        .object_files = &.{"libroc_interpreter_shim.a"},
        .platform_files_pre = &.{archive_path},
    };

    const args = try buildLinkArgs(&ctx, config);

    try std.testing.expectEqual(@as(?usize, null), findArg(args.items, "-all_load"));
    const force_idx = findArg(args.items, "-force_load") orelse return error.MissingForceLoad;
    try std.testing.expect(force_idx + 1 < args.items.len);
    try std.testing.expectEqualStrings(archive_path, args.items[force_idx + 1]);
}

test "macOS non-archive platform files are passed directly" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var object_file = try tmp.dir.createFile(std.testing.io, "host.o", .{});
    defer object_file.close(std.testing.io);
    try object_file.writeStreamingAll(std.testing.io, "mach-o!!");

    const object_path = try tmp.dir.realPathFileAlloc(std.testing.io, "host.o", std.testing.allocator);
    defer std.testing.allocator.free(object_path);

    var arena_instance = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_instance.deinit();

    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(std.testing.allocator, arena_instance.allocator(), &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    const config = LinkConfig{
        .target_format = .macho,
        .target_os = .macos,
        .target_arch = .x86_64,
        .output_path = "test_output",
        .object_files = &.{"libroc_interpreter_shim.a"},
        .platform_files_pre = &.{object_path},
    };

    const args = try buildLinkArgs(&ctx, config);

    try std.testing.expectEqual(@as(?usize, null), findArg(args.items, "-all_load"));
    try std.testing.expectEqual(@as(?usize, null), findArg(args.items, "-force_load"));
    _ = findArg(args.items, object_path) orelse return error.MissingObjectFile;
}

test "link error when LLVM not available" {
    if (comptime !llvm_available) {
        var io = Io.create(std.testing.io);
        var ctx = CliCtx.init(std.testing.allocator, std.testing.allocator, &io, .build);
        ctx.initIo();
        defer ctx.deinit();

        const config = LinkConfig{
            .output_path = "test_output",
            .object_files = &.{ "file1.o", "file2.o" },
        };

        const result = link(&ctx, config);
        try std.testing.expectError(LinkError.LLVMNotAvailable, result);
    }
}
