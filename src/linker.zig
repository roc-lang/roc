//! Zig wrapper for LLD (LLVM Linker) functionality.
//! Provides a high-level interface for linking object files into executables.
//! Supports ELF, COFF, MachO, and WebAssembly targets.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

/// External C functions from zig_llvm.cpp - only available when LLVM is enabled
const llvm_available = if (@import("builtin").is_test) false else @import("config").llvm;

// External C functions from zig_llvm.cpp - only available when LLVM is enabled
const llvm_externs = if (llvm_available) struct {
    extern fn ZigLLDLinkCOFF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkELF(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkMachO(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
    extern fn ZigLLDLinkWasm(argc: c_int, argv: [*]const [*:0]const u8, can_exit_early: bool, disable_output: bool) bool;
} else struct {};

/// Gets a reasonable minimum deployment target for macOS based on the current system
fn getMinimumDeploymentTarget(allocator: Allocator) ![]u8 {
    // Try to get the current macOS version
    var child = std.process.Child.init(&.{ "sw_vers", "-productVersion" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();
    const stdout = try child.stdout.?.reader().readAllAlloc(allocator, 1024);
    defer allocator.free(stdout);
    _ = try child.wait();

    // Parse the version string (e.g., "15.5.1" -> [15, 5, 1])
    const version_str = std.mem.trim(u8, stdout, " \n\r\t");
    var version_parts = std.mem.splitScalar(u8, version_str, '.');

    if (version_parts.next()) |major_str| {
        const major = std.fmt.parseInt(u8, major_str, 10) catch {
            // If we can't parse, fall back to a conservative default
            return allocator.dupe(u8, "13.0");
        };

        // Use the current major version as the deployment target
        // This matches the SDK version and avoids warnings
        const deployment_major: u8 = major;
        return std.fmt.allocPrint(allocator, "{d}.0", .{deployment_major});
    }

    // Fallback if version parsing fails
    return allocator.dupe(u8, "13.0");
}

/// Windows SDK paths for linking
const WindowsSDKPaths = struct {
    um_lib: []u8,
    ucrt_lib: []u8,
};

/// Find Windows SDK installation and return library paths
fn findWindowsSDK(allocator: Allocator) !WindowsSDKPaths {
    // Try to find Windows SDK via registry query
    var child = std.process.Child.init(&.{ "reg", "query", "HKLM\\SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots", "/v", "KitsRoot10", "/reg:64" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    child.spawn() catch {
        // Try 32-bit registry if 64-bit fails
        child = std.process.Child.init(&.{ "reg", "query", "HKLM\\SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots", "/v", "KitsRoot10", "/reg:32" }, allocator);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;
        try child.spawn();
    };

    const stdout = try child.stdout.?.reader().readAllAlloc(allocator, 4096);
    defer allocator.free(stdout);
    const term = try child.wait();

    if (term != .Exited or term.Exited != 0) {
        return error.WindowsSDKNotFound;
    }

    // Parse registry output to find SDK root
    var lines = std.mem.splitScalar(u8, stdout, '\n');
    var sdk_root: ?[]const u8 = null;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (std.mem.indexOf(u8, trimmed, "KitsRoot10")) |_| {
            if (std.mem.indexOf(u8, trimmed, "REG_SZ")) |reg_sz_pos| {
                const after_reg_sz = trimmed[reg_sz_pos + "REG_SZ".len ..];
                sdk_root = std.mem.trim(u8, after_reg_sz, " \t\r\n");
                break;
            }
        }
    }

    const root = sdk_root orelse return error.WindowsSDKNotFound;

    // Find the latest Windows 10/11 SDK version
    const lib_dir = try std.fmt.allocPrint(allocator, "{s}Lib", .{root});
    defer allocator.free(lib_dir);

    var lib_dir_handle = std.fs.cwd().openDir(lib_dir, .{ .iterate = true }) catch {
        return error.WindowsSDKNotFound;
    };
    defer lib_dir_handle.close();

    var latest_version: ?[]u8 = null;
    var iterator = lib_dir_handle.iterate();

    while (try iterator.next()) |entry| {
        if (entry.kind == .directory and std.mem.startsWith(u8, entry.name, "10.")) {
            if (latest_version == null or std.mem.order(u8, entry.name, latest_version.?) == .gt) {
                if (latest_version) |old| allocator.free(old);
                latest_version = try allocator.dupe(u8, entry.name);
            }
        }
    }

    const version = latest_version orelse return error.WindowsSDKNotFound;
    defer allocator.free(version);

    // Determine architecture suffix
    const arch_suffix = switch (builtin.target.cpu.arch) {
        .x86_64 => "x64",
        .x86 => "x86",
        .aarch64 => "arm64",
        else => "x64", // default to x64
    };

    // Build final library paths
    const um_lib = try std.fmt.allocPrint(allocator, "{s}Lib\\{s}\\um\\{s}", .{ root, version, arch_suffix });
    const ucrt_lib = try std.fmt.allocPrint(allocator, "{s}Lib\\{s}\\ucrt\\{s}", .{ root, version, arch_suffix });

    // Verify paths exist
    std.fs.cwd().access(um_lib, .{}) catch {
        allocator.free(um_lib);
        allocator.free(ucrt_lib);
        return error.WindowsSDKNotFound;
    };

    std.fs.cwd().access(ucrt_lib, .{}) catch {
        allocator.free(um_lib);
        allocator.free(ucrt_lib);
        return error.WindowsSDKNotFound;
    };

    return WindowsSDKPaths{
        .um_lib = um_lib,
        .ucrt_lib = ucrt_lib,
    };
}

/// Find Windows system libraries in common locations
fn findWindowsSystemLibs(allocator: Allocator) ![][]u8 {
    var lib_paths = std.ArrayList([]u8).init(allocator);
    errdefer {
        for (lib_paths.items) |path| {
            allocator.free(path);
        }
        lib_paths.deinit();
    }

    // Try the original Windows SDK approach first
    if (findWindowsSDK(allocator)) |sdk_paths| {
        defer allocator.free(sdk_paths.um_lib);
        defer allocator.free(sdk_paths.ucrt_lib);

        try lib_paths.append(try allocator.dupe(u8, sdk_paths.um_lib));
        try lib_paths.append(try allocator.dupe(u8, sdk_paths.ucrt_lib));

        return lib_paths.toOwnedSlice();
    } else |_| {}

    // Try common Visual Studio install locations
    const vs_paths = [_][]const u8{
        "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC",
        "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional\\VC\\Tools\\MSVC",
        "C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\VC\\Tools\\MSVC",
        "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC",
        "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional\\VC\\Tools\\MSVC",
        "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise\\VC\\Tools\\MSVC",
    };

    for (vs_paths) |vs_path| {
        const dir = std.fs.cwd().openDir(vs_path, .{ .iterate = true }) catch continue;
        defer dir.close();

        var iterator = dir.iterate();
        while (iterator.next() catch break) |entry| {
            if (entry.kind == .directory) {
                const arch_suffix = switch (builtin.target.cpu.arch) {
                    .x86_64 => "x64",
                    .x86 => "x86",
                    .aarch64 => "arm64",
                    else => "x64",
                };

                const lib_path = std.fmt.allocPrint(allocator, "{s}\\{s}\\lib\\{s}", .{ vs_path, entry.name, arch_suffix }) catch continue;

                // Check if this lib directory exists
                std.fs.cwd().access(lib_path, .{}) catch {
                    allocator.free(lib_path);
                    continue;
                };

                try lib_paths.append(lib_path);
                break; // Use the first version found
            }
        }

        if (lib_paths.items.len > 0) break;
    }

    // Try Windows Kit locations
    const kit_paths = [_][]const u8{
        "C:\\Program Files (x86)\\Windows Kits\\10\\Lib",
        "C:\\Program Files\\Windows Kits\\10\\Lib",
    };

    for (kit_paths) |kit_path| {
        const dir = std.fs.cwd().openDir(kit_path, .{ .iterate = true }) catch continue;
        defer dir.close();

        var latest_version: ?[]u8 = null;
        var iterator = dir.iterate();

        while (iterator.next() catch break) |entry| {
            if (entry.kind == .directory and std.mem.startsWith(u8, entry.name, "10.")) {
                if (latest_version == null or std.mem.order(u8, entry.name, latest_version.?) == .gt) {
                    if (latest_version) |old| allocator.free(old);
                    latest_version = allocator.dupe(u8, entry.name) catch continue;
                }
            }
        }

        if (latest_version) |version| {
            defer allocator.free(version);

            const arch_suffix = switch (builtin.target.cpu.arch) {
                .x86_64 => "x64",
                .x86 => "x86",
                .aarch64 => "arm64",
                else => "x64",
            };

            const um_lib = std.fmt.allocPrint(allocator, "{s}\\{s}\\um\\{s}", .{ kit_path, version, arch_suffix }) catch continue;
            const ucrt_lib = std.fmt.allocPrint(allocator, "{s}\\{s}\\ucrt\\{s}", .{ kit_path, version, arch_suffix }) catch {
                allocator.free(um_lib);
                continue;
            };

            // Check if both directories exist
            const um_exists = blk: {
                std.fs.cwd().access(um_lib, .{}) catch {
                    break :blk false;
                };
                break :blk true;
            };

            const ucrt_exists = blk: {
                std.fs.cwd().access(ucrt_lib, .{}) catch {
                    break :blk false;
                };
                break :blk true;
            };

            if (um_exists and ucrt_exists) {
                try lib_paths.append(um_lib);
                try lib_paths.append(ucrt_lib);
                break;
            } else {
                allocator.free(um_lib);
                allocator.free(ucrt_lib);
            }
        }
    }

    if (lib_paths.items.len == 0) {
        return error.WindowsSDKNotFound;
    }

    return lib_paths.toOwnedSlice();
}

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
            .wasi => .wasm,
            else => .elf,
        };
    }
};

/// Configuration for linking operation
pub const LinkConfig = struct {
    /// Target format to use for linking
    target_format: TargetFormat = TargetFormat.detectFromSystem(),

    /// Output executable path
    output_path: []const u8,

    /// Input object files to link
    object_files: []const []const u8,

    /// Additional linker flags
    extra_args: []const []const u8 = &.{},

    /// Whether to allow LLD to exit early on errors
    can_exit_early: bool = false,

    /// Whether to disable linker output
    disable_output: bool = false,
};

/// Errors that can occur during linking
pub const LinkError = error{
    LinkFailed,
    OutOfMemory,
    InvalidArguments,
    LLVMNotAvailable,
};

/// Link object files into an executable using LLD
pub fn link(allocator: Allocator, config: LinkConfig) LinkError!void {
    // Check if LLVM is available at compile time
    if (comptime !llvm_available) {
        return LinkError.LLVMNotAvailable;
    }

    var args = std.ArrayList([]const u8).init(allocator);
    defer args.deinit();

    // Add platform-specific linker name and arguments
    switch (builtin.target.os.tag) {
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
            switch (builtin.target.cpu.arch) {
                .aarch64 => try args.append("arm64"),
                .x86_64 => try args.append("x86_64"),
                else => try args.append("arm64"), // default to arm64
            }

            // Add platform version - use a conservative minimum that works across macOS versions
            try args.append("-platform_version");
            try args.append("macos");
            try args.append("13.0"); // minimum deployment target
            try args.append("13.0"); // SDK version

            // Add SDK path
            try args.append("-syslibroot");
            try args.append("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk");
        },
        .linux => {
            // Add linker name for Linux
            try args.append("ld.lld");

            // Add output argument
            try args.append("-o");
            try args.append(config.output_path);

            // Suppress LLD warnings
            try args.append("-w");

            // Use static linking to avoid dynamic linker dependency issues
            try args.append("-static");
        },
        .windows => {
            // Add linker name for Windows COFF
            try args.append("lld-link");

            // Add output argument using Windows style
            const out_arg = try std.fmt.allocPrint(allocator, "/out:{s}", .{config.output_path});
            try args.append(out_arg);

            // Add subsystem flag (console by default)
            try args.append("/subsystem:console");

            // Add machine type based on target architecture
            switch (builtin.target.cpu.arch) {
                .x86_64 => try args.append("/machine:x64"),
                .x86 => try args.append("/machine:x86"),
                .aarch64 => try args.append("/machine:arm64"),
                else => try args.append("/machine:x64"), // default to x64
            }

            // Let the CRT choose the proper startup (mainCRTStartup) implicitly.
            // Explicitly setting /entry can bypass initialization and cause unresolved symbols like __main.
            // Removed explicit /entry.

            // Use standard Windows system libraries that are always available
            // These are part of the core Windows OS and don't require a full SDK
            try args.append("/defaultlib:kernel32");
            try args.append("/defaultlib:ntdll");

            // Use dynamic MSVC CRTs available on Windows systems
            try args.append("/defaultlib:ucrt");
            try args.append("/defaultlib:vcruntime");
            try args.append("/defaultlib:msvcrt");
            try args.append("/defaultlib:legacy_stdio_definitions"); // For older stdio functions

            // Essential libraries for basic functionality
            try args.append("/defaultlib:user32");
            try args.append("/defaultlib:advapi32");

            // Additional libraries for more complete functionality
            try args.append("/defaultlib:shell32");
            try args.append("/defaultlib:ole32");
            try args.append("/defaultlib:oleaut32");
            try args.append("/defaultlib:uuid");
            try args.append("/defaultlib:winmm");

            // Suppress warnings using Windows style
            try args.append("/ignore:4217"); // Ignore locally defined symbol imported warnings
            try args.append("/ignore:4049"); // Ignore locally defined symbol imported warnings
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

    // Add object files
    for (config.object_files) |obj_file| {
        try args.append(obj_file);
    }

    // Add any extra arguments
    for (config.extra_args) |extra_arg| {
        try args.append(extra_arg);
    }

    // Convert to null-terminated strings for C API
    var c_args = allocator.alloc([*:0]const u8, args.items.len) catch return LinkError.OutOfMemory;
    defer allocator.free(c_args);

    for (args.items, 0..) |arg, i| {
        c_args[i] = (allocator.dupeZ(u8, arg) catch return LinkError.OutOfMemory).ptr;
    }
    defer {
        for (c_args) |c_arg| {
            allocator.free(std.mem.span(c_arg));
        }
    }

    // Call appropriate LLD function based on target format
    const success = if (comptime llvm_available) switch (config.target_format) {
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
    } else false;

    if (!success) {
        return LinkError.LinkFailed;
    }
}

/// Convenience function to link two object files into an executable
pub fn linkTwoObjects(allocator: Allocator, obj1: []const u8, obj2: []const u8, output: []const u8) LinkError!void {
    if (comptime !llvm_available) {
        return LinkError.LLVMNotAvailable;
    }

    const config = LinkConfig{
        .output_path = output,
        .object_files = &.{ obj1, obj2 },
    };

    return link(allocator, config);
}

/// Convenience function to link multiple object files into an executable
pub fn linkObjects(allocator: Allocator, object_files: []const []const u8, output: []const u8) LinkError!void {
    if (comptime !llvm_available) {
        return LinkError.LLVMNotAvailable;
    }

    const config = LinkConfig{
        .output_path = output,
        .object_files = object_files,
    };

    return link(allocator, config);
}

test "link config creation" {
    const config = LinkConfig{
        .output_path = "test_output",
        .object_files = &.{ "file1.o", "file2.o" },
    };

    try std.testing.expect(config.target_format == TargetFormat.detectFromSystem());
    try std.testing.expectEqualStrings("test_output", config.output_path);
    try std.testing.expectEqual(@as(usize, 2), config.object_files.len);
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
        const config = LinkConfig{
            .output_path = "test_output",
            .object_files = &.{ "file1.o", "file2.o" },
        };

        const result = link(std.testing.allocator, config);
        try std.testing.expectError(LinkError.LLVMNotAvailable, result);
    }
}
