//! Zig wrapper for LLD (LLVM Linker) functionality.
//! Provides a high-level interface for linking object files into executables.
//! Supports ELF, COFF, MachO, and WebAssembly targets.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const collections = @import("collections");
const build_options = @import("build_options");
const libc_finder = @import("libc_finder.zig");
const embedded_lld = @import("embedded_lld");
const stack_probe = embedded_lld.stack_probe;
const CodeSignature = @import("vendor_macho").CodeSignature;
const DwarfSplice = @import("macho/DwarfSplice.zig");
const roc_target = @import("roc_target");
const RocTarget = roc_target.RocTarget;
const cli_ctx = @import("CliCtx.zig");
const CliCtx = cli_ctx.CliCtx;
const Io = cli_ctx.Io;

/// Suppress linker warnings in release builds only. Debug builds of the
/// compiler surface them: lld reports flags it parsed but ignored (e.g. a
/// MachO `-r`) only as warnings, and suppressing those hides real bugs from
/// compiler developers.
const suppress_linker_warnings = builtin.mode != .Debug;

/// The embedded LLD entrypoints are only linked into LLVM-enabled CLI builds.
const llvm_available = if (@import("builtin").is_test) false else @import("config").llvm;

/// Supported target formats for linking
pub const TargetFormat = embedded_lld.Format;

/// Target ABI for runtime-configurable linking
pub const TargetAbi = enum {
    musl,
    gnu,

    /// Convert from RocTarget to TargetAbi
    pub fn fromRocTarget(target: RocTarget) TargetAbi {
        return if (target.isStatic()) .musl else .gnu;
    }
};

/// What kind of artifact the linker produces.
pub const OutputKind = enum {
    /// Executable binary.
    exe,
    /// Shared/dynamic library (.so, .dylib, .dll).
    shared_lib,
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

    /// Kind of artifact to produce (executable or shared library)
    output_kind: OutputKind = .exe,

    /// Link platform archives lazily (no whole-archive wrapping). Symbol-ABI
    /// links resolve host code by symbol reference, so only reachable archive
    /// members are included and section GC can strip unused host code. Links
    /// against vtable-ABI hosts keep whole-archive wrapping, since nothing
    /// references the host's exports by symbol.
    lazy_platform_archives: bool = false,

    /// Input object files to link
    object_files: []const []const u8,

    /// Platform-provided files to link before object files (e.g., Scrt1.o, crti.o, host.o)
    platform_files_pre: []const []const u8 = &.{},

    /// Platform-provided files to link after object files (e.g., crtn.o)
    platform_files_post: []const []const u8 = &.{},

    /// Additional linker flags
    extra_args: []const []const u8 = &.{},

    /// Symbols that must remain live even under section garbage collection.
    force_undefined_symbols: []const []const u8 = &.{},

    /// Host-declared symbols to force-include and place in the shared library's
    /// export table. Only consulted for shared-library output. Generalizes the
    /// wasm `--export` model to native formats so a Roc-built shared library
    /// exports the host's public API on every target rather than relying on the
    /// platform linker's implicit default-visibility auto-export (which COFF
    /// does not do).
    export_symbols: []const []const u8 = &.{},

    /// Whether to allow LLD to exit early on errors
    can_exit_early: bool = false,

    /// Whether to disable linker output
    disable_output: bool = false,

    /// Initial memory size for WASM targets (bytes). This is the amount of linear memory
    /// available to the WASM module at runtime. Must be a multiple of 64KB (WASM page size).
    wasm_initial_memory: usize = DEFAULT_WASM_INITIAL_MEMORY,

    /// Maximum memory size for WASM targets (bytes), when the runtime contract needs one.
    wasm_maximum_memory: ?usize = null,

    /// Stack size for WASM targets (bytes). This is the amount of memory reserved for the
    /// call stack within the WASM linear memory. Must be a multiple of 16 (stack alignment).
    wasm_stack_size: usize = DEFAULT_WASM_STACK_SIZE,

    /// Whether the final WASM module imports `env.memory` instead of defining memory.
    wasm_import_memory: bool = false,

    /// Optional data/global base for freestanding WASM links.
    wasm_global_base: ?u32 = null,

    /// Function exports derived from explicit platform host object exports.
    wasm_exports: []const []const u8 = &.{},

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

    /// Object file whose `__DWARF` sections get spliced into the linked
    /// macOS executable after linking, making it self-contained for
    /// debuggers. Also suppresses the stabs debug map and reserves load
    /// command space for the extra segment.
    macho_dwarf_object: ?[]const u8 = null,
};

fn appendForceUndefinedSymbol(
    ctx: *CliCtx,
    args: *std.array_list.Managed([]const u8),
    target_os: std.Target.Os.Tag,
    symbol: []const u8,
) LinkError!void {
    switch (target_os) {
        .macos => {
            try args.append("-u");
            const prefixed = std.fmt.allocPrint(ctx.arena, "_{s}", .{symbol}) catch return LinkError.OutOfMemory;
            try args.append(prefixed);
        },
        .windows => {
            const include_arg = std.fmt.allocPrint(ctx.arena, "/include:{s}", .{symbol}) catch return LinkError.OutOfMemory;
            try args.append(include_arg);
        },
        else => {
            const undefined_arg = std.fmt.allocPrint(ctx.arena, "--undefined={s}", .{symbol}) catch return LinkError.OutOfMemory;
            try args.append(undefined_arg);
        },
    }
}

/// Emit the flags that both force-include `symbol` and place it in the shared
/// library's export table, in each linker's spelling. Force-inclusion matters
/// because an outward export is typically unreferenced by the rest of the link
/// (the loader resolves it at runtime), so lazy archive members holding it
/// would otherwise be dropped.
fn appendExportSymbol(
    ctx: *CliCtx,
    args: *std.array_list.Managed([]const u8),
    target_os: std.Target.Os.Tag,
    symbol: []const u8,
) LinkError!void {
    switch (target_os) {
        .macos => {
            const prefixed = std.fmt.allocPrint(ctx.arena, "_{s}", .{symbol}) catch return LinkError.OutOfMemory;
            try args.append("-exported_symbol");
            try args.append(prefixed);
            try args.append("-u");
            try args.append(prefixed);
        },
        .windows => {
            // `/export:` adds the symbol to the export table and as an undefined,
            // which pulls its archive member and roots it against `/opt:ref`.
            const export_arg = std.fmt.allocPrint(ctx.arena, "/export:{s}", .{symbol}) catch return LinkError.OutOfMemory;
            try args.append(export_arg);
        },
        else => {
            const export_arg = std.fmt.allocPrint(ctx.arena, "--export-dynamic-symbol={s}", .{symbol}) catch return LinkError.OutOfMemory;
            const undefined_arg = std.fmt.allocPrint(ctx.arena, "--undefined={s}", .{symbol}) catch return LinkError.OutOfMemory;
            try args.append(export_arg);
            try args.append(undefined_arg);
        },
    }
}

fn appendSharedLibraryExports(
    ctx: *CliCtx,
    args: *std.array_list.Managed([]const u8),
    target_os: std.Target.Os.Tag,
    target_format: TargetFormat,
    output_kind: OutputKind,
    export_symbols: []const []const u8,
) LinkError!void {
    if (output_kind != .shared_lib or target_format == .wasm) return;

    for (export_symbols) |symbol| {
        try appendExportSymbol(ctx, args, target_os, symbol);
    }
}

/// Errors that can occur during linking
pub const LinkError = error{
    LinkFailed,
    OutOfMemory,
    InvalidArguments,
    LLVMNotAvailable,
    WindowsSDKNotFound,
    DarwinSysrootNotFound,
} || std.zig.system.DetectError;

const SelfExePathError = std.Io.Dir.ReadLinkError || error{
    NameTooLong,
    UnsupportedOs,
};

const SelfExeDirError = Allocator.Error || SelfExePathError;

const PatchMachoStackSizeError = std.Io.File.OpenError || std.Io.File.ReadPositionalError || std.Io.File.WritePositionalError || error{
    NotMacho64,
    UnexpectedEof,
};

const ResignMachoError = Allocator.Error || CodeSignature.WriteError || std.Io.File.OpenError || std.Io.File.ReadPositionalError || std.Io.File.WritePositionalError || error{
    CodeSignatureNotAtEnd,
    InvalidCodeSignatureSize,
    MissingLinkeditSegment,
    MissingTextSegment,
    NonResizable,
    NotMacho64,
    UnexpectedEof,
};

/// Resolve the path of the currently running executable, host-OS specific.
///
/// Zig 0.16 removed `std.fs.selfExePath` and the private std helpers live inside
/// `std.Io.Threaded` / `std.Io.Dispatch`. We need a cross-host implementation
/// because the linker runs on Linux/macOS/Windows but may target any OS.
fn selfExePath(std_io: std.Io, buf: []u8) SelfExePathError![]const u8 {
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
fn getSelfExeDir(allocator: std.mem.Allocator, std_io: std.Io) SelfExeDirError![]const u8 {
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
fn findDarwinSysroot(allocator: std.mem.Allocator, std_io: std.Io) Allocator.Error![]const u8 {
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
fn findPlatformSysroot(allocator: std.mem.Allocator, std_io: std.Io, platform_files_dir: ?[]const u8) std.mem.Allocator.Error!?[]const u8 {
    const files_dir = platform_files_dir orelse return null;

    // Look for macos-sysroot in the platform files directory
    const sysroot_path = try std.fs.path.join(allocator, &.{ files_dir, "macos-sysroot" });
    errdefer allocator.free(sysroot_path);

    // Verify it exists and has the expected structure (usr/lib/libSystem.tbd)
    const lib_path = try std.fs.path.join(allocator, &.{ sysroot_path, "usr", "lib", "libSystem.tbd" });
    defer allocator.free(lib_path);
    std.Io.Dir.cwd().access(std_io, lib_path, .{}) catch {
        allocator.free(sysroot_path);
        return null;
    };

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
fn buildLinkArgs(ctx: *CliCtx, config: LinkConfig) LinkError!std.array_list.Managed([]const u8) {
    // Use arena allocator for all temporary allocations
    // Pre-allocate capacity to avoid reallocations (typical command has 20-40 args)
    var args = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 64) catch return LinkError.OutOfMemory;

    // Add platform-specific linker name and arguments
    // Use target OS if provided, otherwise fall back to host OS
    const target_os = config.target_os orelse builtin.target.os.tag;
    const target_arch = config.target_arch orelse builtin.target.cpu.arch;
    const is_shared_lib = config.output_kind == .shared_lib;

    switch (target_os) {
        .macos => {
            // Add linker name for macOS
            try args.append("ld64.lld");

            if (is_shared_lib) {
                try args.append("-dylib");
            }

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Suppress LLD warnings
            if (suppress_linker_warnings) {
                try args.append("-w");
            }
            // For dylibs, dead-strip roots are the exported symbols instead of the entrypoint.
            try args.append("-dead_strip");

            // Add architecture flag
            try args.append("-arch");
            switch (target_arch) {
                .aarch64 => try args.append("arm64"),
                .x86_64 => try args.append("x86_64"),
                else => try args.append("arm64"), // default to arm64
            }

            // Roc rewrites the ad-hoc code signature after patching Mach-O
            // load commands. Suppress lld's content-derived LC_UUID so the
            // final executable bytes do not depend on lld's pre-patch view of
            // the output, which includes the output basename in the signature.
            try args.append("-no_uuid");

            // Add platform version metadata required by Mach-O links.
            try args.append("-platform_version");
            try args.append("macos");
            try args.append(roc_target.macos_deployment.linker_version);
            try args.append(roc_target.macos_deployment.linker_version);

            if (config.macho_dwarf_object != null) {
                // The post-link DWARF splice adds a __DWARF load command, so
                // reserve header space for it, and suppress the stabs debug
                // map since the spliced DWARF replaces it.
                try args.append("-headerpad");
                try args.append("0x2000");
                try args.append("-S");
            }

            // Try to find a platform-provided sysroot first (for cross-compilation with bundled frameworks)
            // Falls back to Roc's bundled darwin sysroot (minimal, only has libSystem.tbd)
            try args.append("-syslibroot");
            if (try findPlatformSysroot(ctx.arena, ctx.io.std_io, config.platform_files_dir)) |platform_sysroot| {
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
            // Stamp a build id so stripped copies of the binary can be
            // matched back to their debug info.
            try args.append("--build-id");
            // TODO make the confirugable instead of using comments
            // Suppress linker warnings
            if (suppress_linker_warnings) {
                try args.append("-w");
            }
            // Verbose linker for debugging (uncomment as needed)
            // try args.append("--verbose");
            // try args.append("--print-map");
            // try args.append("--error-limit=0");

            // Determine target ABI
            const target_abi = config.target_abi orelse if (builtin.target.abi == .musl) TargetAbi.musl else TargetAbi.gnu;

            switch (target_abi) {
                .musl => {
                    if (is_shared_lib) {
                        // -static and -shared are mutually exclusive; a shared library
                        // is inherently a dynamic artifact even on musl targets.
                        try args.append("-shared");
                    } else {
                        // Static musl linking
                        try args.append("-static");
                    }
                },
                .gnu => {
                    if (is_shared_lib) {
                        // Shared libraries have no program interpreter; the loading
                        // process's dynamic linker resolves them.
                        try args.append("-shared");
                    } else
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

            if (is_shared_lib) {
                try args.append("/dll");
            } else {
                // Add subsystem flag (console by default)
                try args.append("/subsystem:console");
            }
            try args.append("/opt:ref");
            // Roc objects carry DWARF (not CodeView); this keeps the .debug_*
            // sections in the PE for gdb/lldb instead of dropping them.
            try args.append("/debug:dwarf");

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
            // and roc.exe. (The loading process owns the stack for DLLs.)
            if (!is_shared_lib) {
                try args.append("/stack:67108864");
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

            // Remove unused sections. The compiler passes explicit host-object
            // exports below so they remain live roots.
            try args.append("--gc-sections");

            // Allow undefined symbols (imports from host environment)
            try args.append("--allow-undefined");

            if (config.wasm_import_memory) {
                try args.append("--import-memory");
            }

            // Set initial memory size (configurable, default 64MB)
            // Must be a multiple of 64KB (WASM page size)
            const initial_memory_str = std.fmt.allocPrint(ctx.arena, "--initial-memory={d}", .{config.wasm_initial_memory}) catch return LinkError.OutOfMemory;
            try args.append(initial_memory_str);

            if (config.wasm_maximum_memory) |maximum_memory| {
                const maximum_memory_str = std.fmt.allocPrint(ctx.arena, "--max-memory={d}", .{maximum_memory}) catch return LinkError.OutOfMemory;
                try args.append(maximum_memory_str);
            }

            if (config.wasm_global_base) |global_base| {
                const global_base_str = std.fmt.allocPrint(ctx.arena, "--global-base={d}", .{global_base}) catch return LinkError.OutOfMemory;
                try args.append(global_base_str);
            }

            // Set stack size (configurable, default 8MB)
            // Must be a multiple of 16 (stack alignment)
            const stack_size_str = std.fmt.allocPrint(ctx.arena, "stack-size={d}", .{config.wasm_stack_size}) catch return LinkError.OutOfMemory;
            try args.append("-z");
            try args.append(stack_size_str);

            for (config.wasm_exports) |export_name| {
                const export_arg = std.fmt.allocPrint(ctx.arena, "--export={s}", .{export_name}) catch return LinkError.OutOfMemory;
                try args.append(export_arg);
            }
        },
        else => {
            // Generic ELF linker
            try args.append("ld.lld");

            if (is_shared_lib) {
                try args.append("-shared");
            }

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Suppress LLD warnings
            if (suppress_linker_warnings) {
                try args.append("-w");
            }
            try args.append("--gc-sections");
        },
    }

    for (config.force_undefined_symbols) |symbol| {
        try appendForceUndefinedSymbol(ctx, &args, target_os, symbol);
    }

    // Force-include and export the host's declared exports from a shared
    // library. (wasm shared output uses --export via config.wasm_exports.)
    try appendSharedLibraryExports(ctx, &args, target_os, config.target_format, config.output_kind, config.export_symbols);

    // For WASM targets, wrap platform files in --whole-archive to include all symbols
    // This ensures host exports (init, handleEvent, update) aren't stripped even when
    // not referenced by other code
    const is_wasm = config.target_format == .wasm;
    const is_macos = target_os == .macos;
    const is_windows = target_os == .windows;
    if (is_wasm and config.platform_files_pre.len > 0 and !config.lazy_platform_archives) {
        try args.append("--whole-archive");
    }

    // Add platform-provided files that come before object files.
    // Static platform archives need all members included so host-exported
    // functions like init, handleEvent, and update are retained.
    if (config.platform_files_pre.len > 0) {
        if (!is_macos and !is_windows and !config.lazy_platform_archives) {
            // ELF targets use --whole-archive
            try args.append("--whole-archive");
        }
        for (config.platform_files_pre) |platform_file| {
            if (config.lazy_platform_archives) {
                try args.append(platform_file);
            } else {
                try appendPlatformFile(ctx, &args, platform_file, is_macos, is_windows);
            }
        }
        if (!is_macos and !is_windows and !config.lazy_platform_archives) {
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
        if (!is_macos and !is_windows and !config.lazy_platform_archives) {
            try args.append("--whole-archive");
        }
        for (config.platform_files_post) |platform_file| {
            if (config.lazy_platform_archives) {
                try args.append(platform_file);
            } else {
                try appendPlatformFile(ctx, &args, platform_file, is_macos, is_windows);
            }
        }
        if (!is_macos and !is_windows and !config.lazy_platform_archives) {
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
    if (config.target_format == .macho and config.output_kind == .exe) {
        patchMachoStackSize(config.output_path, 64 * 1024 * 1024, ctx.io.std_io) catch |err| {
            std.log.warn("Failed to patch LC_MAIN stacksize for {s}: {}", .{ config.output_path, err });
        };
        if (config.macho_dwarf_object) |dwarf_object| {
            DwarfSplice.spliceDwarf(ctx.gpa, ctx.io.std_io, config.output_path, dwarf_object) catch |err| {
                std.log.warn("Failed to splice DWARF into {s}: {}", .{ config.output_path, err });
            };
        }
        // Patching invalidated the ad-hoc code signature ld64.lld wrote; on
        // macOS 14+ the kernel SIGKILLs (137) binaries with bad signatures,
        // so rewrite the signature in place.
        resignMachoAdHoc(ctx, config.output_path) catch |err| {
            std.log.warn("Failed to re-sign {s} after stacksize patch: {}", .{ config.output_path, err });
        };
    }
}

const macho = std.macho;

const deterministic_macho_code_signature_identifier = "roc";

/// Patch a freshly-linked macOS executable's LC_MAIN stacksize field. See the
/// callsite in `link` for why this is needed.
fn patchMachoStackSize(path: []const u8, stacksize: u64, io: std.Io) PatchMachoStackSizeError!void {
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

/// Rewrite a Mach-O binary's ad-hoc code signature in place. The signature
/// blob is the last content in the file, recorded by LC_CODE_SIGNATURE; we
/// recompute the page hashes over everything before it and write a fresh
/// linker-style ad-hoc signature into that extent.
fn resignMachoAdHoc(ctx: *CliCtx, path: []const u8) ResignMachoError!void {
    const io = ctx.io.std_io;
    const gpa = ctx.gpa;

    var file = try std.Io.Dir.cwd().openFile(io, path, .{ .mode = .read_write });
    defer file.close(io);

    var header: macho.mach_header_64 = undefined;
    const header_n = try file.readPositionalAll(io, std.mem.asBytes(&header), 0);
    if (header_n != @sizeOf(macho.mach_header_64)) return error.UnexpectedEof;
    if (header.magic != macho.MH_MAGIC_64) return error.NotMacho64;

    const cmds_buf = try ctx.arena.alignedAlloc(u8, .of(macho.segment_command_64), header.sizeofcmds);
    const cmds_n = try file.readPositionalAll(io, cmds_buf, @sizeOf(macho.mach_header_64));
    if (cmds_n != header.sizeofcmds) return error.UnexpectedEof;

    var cs_cmd: ?*align(8) macho.linkedit_data_command = null;
    var text_seg: ?*align(8) macho.segment_command_64 = null;
    var linkedit_seg: ?*align(8) macho.segment_command_64 = null;

    var offset: usize = 0;
    var i: u32 = 0;
    while (i < header.ncmds) : (i += 1) {
        if (offset + @sizeOf(macho.load_command) > cmds_buf.len) return error.UnexpectedEof;
        const lc: *align(8) macho.load_command = @ptrCast(@alignCast(cmds_buf.ptr + offset));
        switch (lc.cmd) {
            .CODE_SIGNATURE => cs_cmd = @ptrCast(lc),
            .SEGMENT_64 => {
                const seg: *align(8) macho.segment_command_64 = @ptrCast(lc);
                if (std.mem.eql(u8, seg.segName(), "__TEXT")) {
                    text_seg = seg;
                } else if (std.mem.eql(u8, seg.segName(), "__LINKEDIT")) {
                    linkedit_seg = seg;
                }
            },
            else => {},
        }
        offset += lc.cmdsize;
    }

    // No LC_CODE_SIGNATURE means the linker did not sign this binary (ld64.lld
    // only ad-hoc signs arm64 by default), so patching invalidated nothing and
    // the kernel does not require a signature.
    const cs = cs_cmd orelse return;
    const text = text_seg orelse return error.MissingTextSegment;
    const linkedit = linkedit_seg orelse return error.MissingLinkeditSegment;

    const page_size: u16 = if (header.cputype == macho.CPU_TYPE_ARM64) 0x4000 else 0x1000;
    const ident = deterministic_macho_code_signature_identifier;

    // The signature hashes every page before LC_CODE_SIGNATURE's dataoff,
    // including page 0 with the load commands. Its exact size is known up
    // front (one CodeDirectory blob, no special slots), so any load command
    // size changes must be written back before hashing.
    const hash_size = std.crypto.hash.sha2.Sha256.digest_length;
    const total_pages = std.mem.alignForward(usize, cs.dataoff, page_size) / page_size;
    const exact_size = @sizeOf(macho.SuperBlob) + @sizeOf(macho.BlobIndex) +
        @sizeOf(macho.CodeDirectory) + ident.len + 1 + total_pages * hash_size;

    const old_datasize = cs.datasize;
    const old_sig_end: u64 = @as(u64, cs.dataoff) + @as(u64, old_datasize);
    if (try file.length(io) != old_sig_end) return error.CodeSignatureNotAtEnd;

    if (exact_size != old_datasize) {
        cs.datasize = @intCast(exact_size);
        if (exact_size > old_datasize) {
            const grow: u64 = @intCast(exact_size - old_datasize);
            linkedit.filesize += grow;
        } else {
            const shrink: u64 = @intCast(old_datasize - exact_size);
            if (linkedit.filesize < shrink) return error.InvalidCodeSignatureSize;
            linkedit.filesize -= shrink;
        }
        linkedit.vmsize = std.mem.alignForward(u64, linkedit.filesize, page_size);
        try file.writePositionalAll(io, cmds_buf, @sizeOf(macho.mach_header_64));
    }

    var code_sig = CodeSignature.init(page_size);
    defer code_sig.deinit(gpa);
    code_sig.code_directory.ident = ident;

    var sig_bytes: std.Io.Writer.Allocating = .init(gpa);
    defer sig_bytes.deinit();
    try code_sig.writeAdhocSignature(gpa, io, .{
        .file = file,
        .exec_seg_base = text.fileoff,
        .exec_seg_limit = text.filesize,
        .file_size = cs.dataoff,
        .dylib = header.filetype == macho.MH_DYLIB,
    }, &sig_bytes.writer);

    const sig = sig_bytes.written();
    std.debug.assert(sig.len == exact_size);
    try file.writePositionalAll(io, sig, cs.dataoff);
    const new_sig_end: u64 = @as(u64, cs.dataoff) + @as(u64, @intCast(sig.len));
    try file.setLength(io, new_sig_end);
}

fn findArg(args: []const []const u8, needle: []const u8) ?usize {
    for (args, 0..) |arg, i| {
        if (std.mem.eql(u8, arg, needle)) return i;
    }
    return null;
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

test "force undefined symbols use target linker spelling" {
    var arena_instance = collections.SingleThreadArena.init(std.testing.allocator);
    defer arena_instance.deinit();

    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(std.testing.allocator, arena_instance.allocator(), &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    const mac_config = LinkConfig{
        .target_format = .macho,
        .target_os = .macos,
        .target_arch = .x86_64,
        .output_path = "test_output",
        .object_files = &.{"app.o"},
        .force_undefined_symbols = &.{"roc__answer"},
    };
    const mac_args = try buildLinkArgs(&ctx, mac_config);
    const mac_u_idx = findArg(mac_args.items, "-u") orelse return error.MissingForceUndefined;
    try std.testing.expect(mac_u_idx + 1 < mac_args.items.len);
    try std.testing.expectEqualStrings("_roc__answer", mac_args.items[mac_u_idx + 1]);

    const linux_config = LinkConfig{
        .target_format = .elf,
        .target_abi = .musl,
        .target_os = .linux,
        .target_arch = .x86_64,
        .output_path = "test_output",
        .object_files = &.{"app.o"},
        .force_undefined_symbols = &.{"roc__answer"},
    };
    const linux_args = try buildLinkArgs(&ctx, linux_config);
    _ = findArg(linux_args.items, "--undefined=roc__answer") orelse return error.MissingForceUndefined;
}

test "shared library exports use target linker spelling" {
    var arena_instance = collections.SingleThreadArena.init(std.testing.allocator);
    defer arena_instance.deinit();

    var io = Io.create(std.testing.io);
    var ctx = CliCtx.init(std.testing.allocator, arena_instance.allocator(), &io, .build);
    ctx.initIo();
    defer ctx.deinit();

    // Windows: /export: both exports and force-includes.
    var win_args = std.array_list.Managed([]const u8).init(ctx.arena);
    try appendSharedLibraryExports(&ctx, &win_args, .windows, .coff, .shared_lib, &.{"roc_run_app"});
    _ = findArg(win_args.items, "/export:roc_run_app") orelse return error.MissingExport;

    // macOS: -exported_symbol + -u, both underscore-prefixed.
    var mac_args = std.array_list.Managed([]const u8).init(ctx.arena);
    try appendSharedLibraryExports(&ctx, &mac_args, .macos, .macho, .shared_lib, &.{"roc_run_app"});
    const mac_exp_idx = findArg(mac_args.items, "-exported_symbol") orelse return error.MissingExport;
    try std.testing.expect(mac_exp_idx + 1 < mac_args.items.len);
    try std.testing.expectEqualStrings("_roc_run_app", mac_args.items[mac_exp_idx + 1]);

    // ELF: --export-dynamic-symbol + --undefined to root and pull the member.
    var linux_args = std.array_list.Managed([]const u8).init(ctx.arena);
    try appendSharedLibraryExports(&ctx, &linux_args, .linux, .elf, .shared_lib, &.{"roc_run_app"});
    _ = findArg(linux_args.items, "--export-dynamic-symbol=roc_run_app") orelse return error.MissingExport;
    _ = findArg(linux_args.items, "--undefined=roc_run_app") orelse return error.MissingExport;

    // Exports must not leak into a non-shared (exe) link.
    var exe_args = std.array_list.Managed([]const u8).init(ctx.arena);
    try appendSharedLibraryExports(&ctx, &exe_args, .windows, .coff, .exe, &.{"roc_run_app"});
    try std.testing.expectEqual(@as(?usize, null), findArg(exe_args.items, "/export:roc_run_app"));
}

test "macOS platform archives use scoped force_load" {
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    var archive_file = try tmp.dir.createFile(std.testing.io, "libhost.a", .{});
    defer archive_file.close(std.testing.io);
    try archive_file.writeStreamingAll(std.testing.io, "!<arch>\n");

    const archive_path = try tmp.dir.realPathFileAlloc(std.testing.io, "libhost.a", std.testing.allocator);
    defer std.testing.allocator.free(archive_path);

    var arena_instance = collections.SingleThreadArena.init(std.testing.allocator);
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

    var arena_instance = collections.SingleThreadArena.init(std.testing.allocator);
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
