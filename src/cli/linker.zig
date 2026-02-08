//! Zig wrapper for LLD (LLVM Linker) functionality.
//! Provides a high-level interface for linking object files into executables.
//! Supports ELF, COFF, MachO, and WebAssembly targets.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const libc_finder = @import("libc_finder.zig");
const stack_probe = @import("stack_probe.zig");
const RocTarget = @import("roc_target").RocTarget;
const cli_ctx = @import("CliContext.zig");
const CliContext = cli_ctx.CliContext;
const Io = cli_ctx.Io;

/// External C functions from zig_llvm.cpp - only available when LLVM is enabled
const llvm_available = if (@import("builtin").is_test) false else @import("config").llvm;

// External C functions from zig_llvm.cpp - only available when LLVM is enabled
const llvm_externs = if (llvm_available) struct {
    extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
} else struct {};

/// Supported target formats for linking
pub const TargetFormat = enum {
    elf,
    coff,
    macho,
    wasm,

    /// Automatically detect target format based on the current system
    pub fn detectFromSystem() TargetFormat {
        return switch (builtin.target.os.tag) {
            .windows => .coff,
            .macos, .ios, .watchos, .tvos => .macho,
            .freestanding => .wasm,
            else => .elf,
        };
    }

    /// Detect target format from OS tag
    pub fn detectFromOs(os: std.Target.Os.Tag) TargetFormat {
        return switch (os) {
            .windows => .coff,
            .macos, .ios, .watchos, .tvos => .macho,
            .freestanding => .wasm,
            else => .elf,
        };
    }
};

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

/// Find the Darwin sysroot directory at runtime.
/// First looks for a 'darwin' directory next to the executable (for distributed builds),
/// then falls back to the compile-time path (for local development builds).
fn findDarwinSysroot(allocator: std.mem.Allocator) ![]const u8 {
    // Get the path to the currently running executable
    var exe_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const exe_path = std.fs.selfExePath(&exe_path_buf) catch |err| {
        std.log.warn("Failed to get executable path: {}, falling back to compile-time path", .{err});
        return build_options.darwin_sysroot;
    };

    // Get the directory containing the executable
    const exe_dir = std.fs.path.dirname(exe_path) orelse {
        std.log.warn("Failed to get executable directory, falling back to compile-time path", .{});
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

    std.fs.cwd().access(tbd_path, .{}) catch {
        // Runtime path doesn't exist, fall back to compile-time path (local dev builds)
        return build_options.darwin_sysroot;
    };

    return runtime_sysroot;
}

/// Find a platform-provided sysroot for macOS cross-compilation.
/// Looks for 'macos-sysroot' directory in the platform's files directory.
/// For example, if platform_files_dir is "/path/to/platform/targets",
/// this looks for "/path/to/platform/targets/macos-sysroot/".
fn findPlatformSysroot(allocator: std.mem.Allocator, platform_files_dir: ?[]const u8) ?[]const u8 {
    const files_dir = platform_files_dir orelse return null;

    // Look for macos-sysroot in the platform files directory
    const sysroot_path = std.fs.path.join(allocator, &.{ files_dir, "macos-sysroot" }) catch return null;

    // Verify it exists and has the expected structure (usr/lib/libSystem.tbd)
    const lib_path = std.fs.path.join(allocator, &.{ sysroot_path, "usr", "lib", "libSystem.tbd" }) catch return null;
    std.fs.cwd().access(lib_path, .{}) catch return null;

    std.log.info("Using platform-provided macOS sysroot: {s}", .{sysroot_path});
    return sysroot_path;
}

/// Discover frameworks in a directory and add -framework flags for each.
/// This allows platforms to control their framework dependencies by choosing
/// which frameworks to bundle in their sysroot.
/// Only links frameworks that have a .tbd file (skips header-only frameworks).
fn discoverAndLinkFrameworks(allocator: std.mem.Allocator, args: *std.array_list.Managed([]const u8), frameworks_dir: []const u8) LinkError!void {
    var dir = std.fs.cwd().openDir(frameworks_dir, .{ .iterate = true }) catch {
        // No frameworks directory - that's fine, just skip
        return;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (iter.next() catch return) |entry| {
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

            const has_tbd = std.fs.cwd().access(tbd_path1, .{}) catch std.fs.cwd().access(tbd_path2, .{}) catch null;
            if (has_tbd == null) continue; // Skip frameworks without TBD files

            const fw_name_copy = allocator.dupe(u8, fw_name) catch return LinkError.OutOfMemory;
            try args.append("-framework");
            try args.append(fw_name_copy);
        }
    }
}

/// Build the linker command arguments for the given configuration.
/// Returns the args array that would be passed to LLD.
/// This is used both by link() and formatLinkCommand().
fn buildLinkArgs(ctx: *CliContext, config: LinkConfig) LinkError!std.array_list.Managed([]const u8) {
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
            if (findPlatformSysroot(ctx.arena, config.platform_files_dir)) |platform_sysroot| {
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
                try discoverAndLinkFrameworks(ctx.arena, &args, fw_path);
            } else {
                const darwin_sysroot = findDarwinSysroot(ctx.arena) catch return LinkError.DarwinSysrootNotFound;
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

            const target = try std.zig.system.resolveTargetQuery(query);

            const native_libc = std.zig.LibCInstallation.findNative(.{
                .allocator = ctx.arena,
                .target = &target,
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
                // Write to a temp file and add to link line
                const stack_probe_path = std.fs.path.join(ctx.arena, &.{
                    std.fs.selfExeDirPathAlloc(ctx.arena) catch return LinkError.OutOfMemory,
                    "stack_probe.obj",
                }) catch return LinkError.OutOfMemory;
                std.fs.cwd().writeFile(.{
                    .sub_path = stack_probe_path,
                    .data = stack_probe_obj,
                }) catch return LinkError.OutOfMemory;
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

    // Add platform-provided files that come before object files
    // Use --whole-archive (or -all_load on macOS, /wholearchive on Windows) to include
    // all members from static libraries. This ensures host-exported functions like
    // init, handleEvent, update are included even though they're not referenced by
    // the Roc app's compiled code.
    if (config.platform_files_pre.len > 0) {
        if (is_macos) {
            // macOS uses -all_load to include all members from static libraries
            try args.append("-all_load");
        } else if (!is_windows) {
            // ELF targets use --whole-archive
            try args.append("--whole-archive");
        }
        for (config.platform_files_pre) |platform_file| {
            if (is_windows) {
                // Windows COFF uses /wholearchive:filename for each file
                const whole_arg = std.fmt.allocPrint(ctx.arena, "/wholearchive:{s}", .{platform_file}) catch return LinkError.OutOfMemory;
                try args.append(whole_arg);
            } else {
                try args.append(platform_file);
            }
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
        if (is_macos) {
            try args.append("-all_load");
        } else if (!is_windows) {
            try args.append("--whole-archive");
        }
        for (config.platform_files_post) |platform_file| {
            if (is_windows) {
                const whole_arg = std.fmt.allocPrint(ctx.arena, "/wholearchive:{s}", .{platform_file}) catch return LinkError.OutOfMemory;
                try args.append(whole_arg);
            } else {
                try args.append(platform_file);
            }
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
pub fn link(ctx: *CliContext, config: LinkConfig) LinkError!void {
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

    // Convert to null-terminated strings for C API
    // Arena allocator will clean up all these temporary allocations
    var c_args = ctx.arena.alloc([*:0]const u8, args.items.len) catch return LinkError.OutOfMemory;

    for (args.items, 0..) |arg, i| {
        c_args[i] = (ctx.arena.dupeZ(u8, arg) catch return LinkError.OutOfMemory).ptr;
    }

    // Call appropriate LLD function based on target format
    const success = switch (config.target_format) {
        .elf => llvm_externs.ZigLLDLinkELF(
            @intCast(c_args.len),
            c_args.ptr,
            config.can_exit_early,
            config.disable_output,
        ),
        .coff => llvm_externs.ZigLLDLinkCOFF(
            @intCast(c_args.len),
            c_args.ptr,
            config.can_exit_early,
            config.disable_output,
        ),
        .macho => llvm_externs.ZigLLDLinkMachO(
            @intCast(c_args.len),
            c_args.ptr,
            config.can_exit_early,
            config.disable_output,
        ),
        .wasm => llvm_externs.ZigLLDLinkWasm(
            @intCast(c_args.len),
            c_args.ptr,
            config.can_exit_early,
            config.disable_output,
        ),
    };

    if (!success) {
        return LinkError.LinkFailed;
    }
}

/// Format link configuration as a shell command string for manual reproduction.
/// Useful for debugging linking issues by allowing users to run the linker manually.
pub fn formatLinkCommand(ctx: *CliContext, config: LinkConfig) LinkError![]const u8 {
    const args = try buildLinkArgs(ctx, config);

    // Join args with spaces, quoting paths that contain spaces or special chars
    var result = std.array_list.Managed(u8).init(ctx.arena);

    for (args.items, 0..) |arg, i| {
        if (i > 0) result.append(' ') catch return LinkError.OutOfMemory;

        // Quote if contains spaces or shell metacharacters
        const needs_quoting = std.mem.indexOfAny(u8, arg, " \t'\"\\$`") != null;
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
pub fn linkTwoObjects(ctx: *CliContext, obj1: []const u8, obj2: []const u8, output: []const u8) LinkError!void {
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
pub fn linkObjects(ctx: *CliContext, object_files: []const []const u8, output: []const u8) LinkError!void {
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

test "link error when LLVM not available" {
    if (comptime !llvm_available) {
        var io = Io.init();
        var ctx = CliContext.init(std.testing.allocator, std.testing.allocator, &io, .build);
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
