//! Finds libc and dynamic linker paths on Linux systems
//! Supports multiple architectures and distributions

const std = @import("std");
const builtin = @import("builtin");
const fs = std.fs;
const process = std.process;

/// Information about the system's libc installation
pub const LibcInfo = struct {
    /// Path to the dynamic linker (e.g., /lib64/ld-linux-x86-64.so.2)
    dynamic_linker: []const u8,

    /// Path to libc library (e.g., /lib/x86_64-linux-gnu/libc.so.6)
    libc_path: []const u8,

    /// Directory containing libc and CRT files
    lib_dir: []const u8,

    /// System architecture (e.g., "x86_64", "aarch64")
    arch: []const u8,

    /// Allocator used for all allocations
    allocator: std.mem.Allocator,

    pub fn deinit(self: *LibcInfo) void {
        self.allocator.free(self.dynamic_linker);
        self.allocator.free(self.libc_path);
        self.allocator.free(self.lib_dir);
        self.allocator.free(self.arch);
    }
};

/// Main entry point - finds libc and dynamic linker
pub fn findLibc(allocator: std.mem.Allocator) !LibcInfo {
    // Try compiler-based detection first (most reliable)
    if (try findViaCompiler(allocator)) |info| {
        return info;
    }

    // Fall back to filesystem search
    return try findViaFilesystem(allocator);
}

/// Find libc using compiler queries (gcc/clang)
fn findViaCompiler(allocator: std.mem.Allocator) !?LibcInfo {
    const compilers = [_][]const u8{ "gcc", "clang", "cc" };

    for (compilers) |compiler| {
        // Try to get dynamic linker path from compiler
        const ld_result = process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{ compiler, "-print-file-name=ld-linux-x86-64.so.2" },
        }) catch continue;
        defer allocator.free(ld_result.stdout);
        defer allocator.free(ld_result.stderr);

        // Try to get libc path from compiler
        const libc_result = process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{ compiler, "-print-file-name=libc.so" },
        }) catch continue;
        defer allocator.free(libc_result.stdout);
        defer allocator.free(libc_result.stderr);

        const libc_path = std.mem.trimRight(u8, libc_result.stdout, "\n\r \t");
        if (libc_path.len == 0 or std.mem.eql(u8, libc_path, "libc.so")) continue;

        // Verify the file exists
        _ = fs.openFileAbsolute(libc_path, .{}) catch continue;

        const lib_dir = fs.path.dirname(libc_path) orelse continue;

        // Get architecture
        const arch = try getArchitecture(allocator);

        // Find dynamic linker
        const dynamic_linker = try findDynamicLinker(allocator, arch, lib_dir) orelse continue;

        return LibcInfo{
            .dynamic_linker = dynamic_linker,
            .libc_path = try allocator.dupe(u8, libc_path),
            .lib_dir = try allocator.dupe(u8, lib_dir),
            .arch = arch,
            .allocator = allocator,
        };
    }

    return null;
}

/// Find libc by searching the filesystem
fn findViaFilesystem(allocator: std.mem.Allocator) !LibcInfo {
    const arch = try getArchitecture(allocator);
    defer allocator.free(arch);

    const search_paths = try getSearchPaths(allocator, arch);
    defer {
        for (search_paths.items) |path| {
            allocator.free(path);
        }
        search_paths.deinit();
    }

    // Search for libc in standard paths
    for (search_paths.items) |lib_dir| {
        var dir = fs.openDirAbsolute(lib_dir, .{}) catch continue;
        defer dir.close();

        const libc_names = [_][]const u8{ "libc.so.6", "libc.so", "libc.a" };

        for (libc_names) |libc_name| {
            const libc_path = try fs.path.join(allocator, &[_][]const u8{ lib_dir, libc_name });
            defer allocator.free(libc_path);

            // Check if file exists
            _ = fs.openFileAbsolute(libc_path, .{}) catch continue;

            // Try to find dynamic linker
            const dynamic_linker = try findDynamicLinker(allocator, arch, lib_dir) orelse continue;

            return LibcInfo{
                .dynamic_linker = dynamic_linker,
                .libc_path = try allocator.dupe(u8, libc_path),
                .lib_dir = try allocator.dupe(u8, lib_dir),
                .arch = try allocator.dupe(u8, arch),
                .allocator = allocator,
            };
        }
    }

    return error.LibcNotFound;
}

/// Find the dynamic linker for the given architecture
fn findDynamicLinker(allocator: std.mem.Allocator, arch: []const u8, lib_dir: []const u8) !?[]const u8 {
    // Map architecture to dynamic linker name
    const ld_names = if (std.mem.eql(u8, arch, "x86_64"))
        &[_][]const u8{ "ld-linux-x86-64.so.2", "ld-linux.so.2" }
    else if (std.mem.eql(u8, arch, "aarch64"))
        &[_][]const u8{ "ld-linux-aarch64.so.1", "ld-linux.so.1" }
    else if (std.mem.startsWith(u8, arch, "arm"))
        &[_][]const u8{ "ld-linux-armhf.so.3", "ld-linux.so.3" }
    else if (std.mem.eql(u8, arch, "i686") or std.mem.eql(u8, arch, "i386"))
        &[_][]const u8{"ld-linux.so.2"}
    else
        &[_][]const u8{ "ld-linux.so.2", "ld.so.1" };

    // Search in the lib directory first
    for (ld_names) |ld_name| {
        const path = try fs.path.join(allocator, &[_][]const u8{ lib_dir, ld_name });
        defer allocator.free(path);

        if (fs.openFileAbsolute(path, .{})) |file| {
            file.close();
            return try allocator.dupe(u8, path);
        } else |_| {}
    }

    // Search in common locations
    const common_paths = if (std.mem.eql(u8, arch, "x86_64"))
        &[_][]const u8{ "/lib64", "/lib/x86_64-linux-gnu", "/lib" }
    else if (std.mem.eql(u8, arch, "aarch64"))
        &[_][]const u8{ "/lib", "/lib/aarch64-linux-gnu", "/lib64" }
    else if (std.mem.startsWith(u8, arch, "arm"))
        &[_][]const u8{ "/lib", "/lib/arm-linux-gnueabihf", "/lib32" }
    else
        &[_][]const u8{ "/lib", "/lib32" };

    for (common_paths) |search_dir| {
        for (ld_names) |ld_name| {
            const path = try fs.path.join(allocator, &[_][]const u8{ search_dir, ld_name });
            defer allocator.free(path);

            if (fs.openFileAbsolute(path, .{})) |file| {
                file.close();
                return try allocator.dupe(u8, path);
            } else |_| {}
        }
    }

    return null;
}

/// Get system architecture using uname
fn getArchitecture(allocator: std.mem.Allocator) ![]const u8 {
    const result = try process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "uname", "-m" },
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const arch = std.mem.trimRight(u8, result.stdout, "\n\r \t");
    return allocator.dupe(u8, arch);
}

/// Get library search paths for the given architecture
fn getSearchPaths(allocator: std.mem.Allocator, arch: []const u8) !std.ArrayList([]const u8) {
    var paths = std.ArrayList([]const u8).init(allocator);
    errdefer {
        for (paths.items) |path| {
            allocator.free(path);
        }
        paths.deinit();
    }

    // Get multiarch triplet if possible
    const triplet = try getMultiarchTriplet(allocator, arch);
    defer allocator.free(triplet);

    // Add multiarch paths
    try paths.append(try std.fmt.allocPrint(allocator, "/lib/{s}", .{triplet}));
    try paths.append(try std.fmt.allocPrint(allocator, "/usr/lib/{s}", .{triplet}));

    // Add architecture-specific paths
    if (std.mem.eql(u8, arch, "x86_64")) {
        try paths.append(try allocator.dupe(u8, "/lib64"));
        try paths.append(try allocator.dupe(u8, "/usr/lib64"));
        try paths.append(try allocator.dupe(u8, "/lib/x86_64-linux-gnu"));
        try paths.append(try allocator.dupe(u8, "/usr/lib/x86_64-linux-gnu"));
    } else if (std.mem.eql(u8, arch, "aarch64")) {
        try paths.append(try allocator.dupe(u8, "/lib64"));
        try paths.append(try allocator.dupe(u8, "/usr/lib64"));
        try paths.append(try allocator.dupe(u8, "/lib/aarch64-linux-gnu"));
        try paths.append(try allocator.dupe(u8, "/usr/lib/aarch64-linux-gnu"));
    } else if (std.mem.startsWith(u8, arch, "arm")) {
        try paths.append(try allocator.dupe(u8, "/lib32"));
        try paths.append(try allocator.dupe(u8, "/usr/lib32"));
        try paths.append(try allocator.dupe(u8, "/lib/arm-linux-gnueabihf"));
        try paths.append(try allocator.dupe(u8, "/usr/lib/arm-linux-gnueabihf"));
    }

    // Add generic paths
    try paths.append(try allocator.dupe(u8, "/lib"));
    try paths.append(try allocator.dupe(u8, "/usr/lib"));
    try paths.append(try allocator.dupe(u8, "/usr/local/lib"));

    return paths;
}

/// Get multiarch triplet (e.g., x86_64-linux-gnu)
fn getMultiarchTriplet(allocator: std.mem.Allocator, arch: []const u8) ![]const u8 {
    // Try to get from gcc first
    const result = process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "gcc", "-dumpmachine" },
    }) catch |err| switch (err) {
        error.FileNotFound => {
            // Fallback to common triplets
            if (std.mem.eql(u8, arch, "x86_64")) {
                return allocator.dupe(u8, "x86_64-linux-gnu");
            } else if (std.mem.eql(u8, arch, "aarch64")) {
                return allocator.dupe(u8, "aarch64-linux-gnu");
            } else if (std.mem.startsWith(u8, arch, "arm")) {
                return allocator.dupe(u8, "arm-linux-gnueabihf");
            } else {
                return allocator.dupe(u8, arch);
            }
        },
        else => return err,
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const triplet = std.mem.trimRight(u8, result.stdout, "\n\r \t");
    return allocator.dupe(u8, triplet);
}

/// Find a file in a directory
fn findFile(allocator: std.mem.Allocator, dir_path: []const u8, filename: []const u8) !?[]const u8 {
    const full_path = try fs.path.join(allocator, &[_][]const u8{ dir_path, filename });
    defer allocator.free(full_path);

    if (fs.openFileAbsolute(full_path, .{})) |file| {
        file.close();
        return try allocator.dupe(u8, full_path);
    } else |_| {
        return null;
    }
}

test "libc detection integration test" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;

    const allocator = std.testing.allocator;

    const libc_info = findLibc(allocator) catch |err| switch (err) {
        error.LibcNotFound => {
            std.log.warn("Libc not found on this system - this may be expected in some environments", .{});
            return error.SkipZigTest;
        },
        else => return err,
    };
    defer {
        var info = libc_info;
        info.deinit();
    }

    // Verify we got valid information
    try std.testing.expect(libc_info.arch.len > 0);
    try std.testing.expect(libc_info.dynamic_linker.len > 0);
    try std.testing.expect(libc_info.libc_path.len > 0);
    try std.testing.expect(libc_info.lib_dir.len > 0);

    // Verify the dynamic linker file exists and is accessible
    const ld_file = fs.openFileAbsolute(libc_info.dynamic_linker, .{}) catch |err| {
        std.log.err("Dynamic linker not accessible at {s}: {}", .{ libc_info.dynamic_linker, err });
        return err;
    };
    ld_file.close();

    // Verify the libc file exists and is accessible
    const libc_file = fs.openFileAbsolute(libc_info.libc_path, .{}) catch |err| {
        std.log.err("Libc not accessible at {s}: {}", .{ libc_info.libc_path, err });
        return err;
    };
    libc_file.close();
}
