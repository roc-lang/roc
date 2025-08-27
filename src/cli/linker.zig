//! Zig wrapper for LLD (LLVM Linker) functionality.
//! Provides a high-level interface for linking object files into executables.
//! Supports ELF, COFF, MachO, and WebAssembly targets.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const libc_finder = @import("libc_finder.zig");

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
            .wasi => .wasm,
            else => .elf,
        };
    }
};

/// Target ABI for runtime-configurable linking
pub const TargetAbi = enum {
    musl,
    gnu,

    /// Convert from RocTarget to TargetAbi
    pub fn fromRocTarget(roc_target: anytype) TargetAbi {
        // Use string matching to avoid circular imports
        const target_str = @tagName(roc_target);
        if (std.mem.endsWith(u8, target_str, "musl")) {
            return .musl;
        } else {
            return .gnu;
        }
    }
};

/// Configuration for linking operation
pub const LinkConfig = struct {
    /// Target format to use for linking
    target_format: TargetFormat = TargetFormat.detectFromSystem(),

    /// Target ABI - determines static vs dynamic linking strategy
    target_abi: ?TargetAbi = null, // null means detect from system

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

            // Link against system libraries on macOS
            try args.append("-lSystem");
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
                    // Dynamic GNU linking - find the dynamic linker
                    if (libc_finder.findLibc(allocator)) |libc_info| {
                        // We need to copy the path since args holds references
                        const dynamic_linker = try allocator.dupe(u8, libc_info.dynamic_linker);

                        // Clean up libc_info after copying what we need
                        var info = libc_info;
                        info.deinit();

                        try args.append("-dynamic-linker");
                        try args.append(dynamic_linker);
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
                },
            }
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

            // These are part of the core Windows OS and are available on all Windows systems
            try args.append("/defaultlib:kernel32");
            try args.append("/defaultlib:ntdll");
            try args.append("/defaultlib:msvcrt");

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

    // Add platform-provided files that come before object files
    for (config.platform_files_pre) |platform_file| {
        try args.append(platform_file);
    }

    // Add object files
    for (config.object_files) |obj_file| {
        try args.append(obj_file);
    }

    // Add platform-provided files that come after object files
    for (config.platform_files_post) |platform_file| {
        try args.append(platform_file);
    }

    // Add any extra arguments
    for (config.extra_args) |extra_arg| {
        try args.append(extra_arg);
    }

    // Debug: Print the linker command
    std.log.debug("Linker command:", .{});
    for (args.items) |arg| {
        std.log.debug("  {s}", .{arg});
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
        const config = LinkConfig{
            .output_path = "test_output",
            .object_files = &.{ "file1.o", "file2.o" },
        };

        const result = link(std.testing.allocator, config);
        try std.testing.expectError(LinkError.LLVMNotAvailable, result);
    }
}
