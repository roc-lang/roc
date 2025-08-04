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

    // Add linker name (required as first argument)
    try args.append("lld");

    // Add output argument
    try args.append("-o");
    try args.append(config.output_path);

    // Suppress LLD warnings
    try args.append("-w");

    // Add platform-specific flags
    switch (builtin.target.os.tag) {
        .macos => {
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

            // Allow undefined symbols for now - compiler-rt builtins will be resolved by system
            try args.append("-undefined");
            try args.append("dynamic_lookup");
        },
        .linux => {
            // Add dynamic linker
            try args.append("-dynamic-linker");
            try args.append("/lib64/ld-linux-x86-64.so.2");
        },
        .windows => {
            // Add subsystem for console applications
            try args.append("/subsystem:console");

            // Link against common Windows libraries
            try args.append("kernel32.lib");
            try args.append("msvcrt.lib");
        },
        else => {},
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
