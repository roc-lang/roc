//! Finds libc and dynamic linker paths on Linux systems
//!
//! Only used when building natively (not cross-compiling for another target)
//!
//! TODO can we improve this or make it more reliable? this implementation probably
//! needs some work but it will be hard to know until we have more users testing roc
//! on different systems.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const Allocators = base.Allocators;
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

/// Validate that a path is safe (absolute and no traversal)
fn validatePath(path: []const u8) bool {
    if (!fs.path.isAbsolute(path)) return false;
    if (std.mem.indexOf(u8, path, "../") != null) return false;
    return true;
}

/// Get the dynamic linker name for the given architecture
fn getDynamicLinkerName(arch: []const u8) []const u8 {
    if (std.mem.eql(u8, arch, "x86_64")) {
        return "ld-linux-x86-64.so.2";
    } else if (std.mem.eql(u8, arch, "aarch64")) {
        return "ld-linux-aarch64.so.1";
    } else if (std.mem.startsWith(u8, arch, "arm")) {
        return "ld-linux-armhf.so.3";
    } else if (std.mem.eql(u8, arch, "i686") or std.mem.eql(u8, arch, "i386")) {
        return "ld-linux.so.2";
    } else {
        return "ld-linux.so.2";
    }
}

/// finds libc and dynamic linker
/// Solely allocates into the arena
pub fn findLibc(allocs: *Allocators) !LibcInfo {
    // Try compiler-based detection first (most reliable)
    if (try findViaCompiler(allocs.arena)) |info|
        return info
    else
        // Fall back to filesystem search
        return try findViaFilesystem(allocs.arena);
}

/// Find libc using compiler queries (gcc/clang)
fn findViaCompiler(arena: std.mem.Allocator) !?LibcInfo {
    const compilers = [_][]const u8{ "gcc", "clang", "cc" };

    // Get architecture first
    const arch = try getArchitecture(arena);

    // Get the expected dynamic linker name for this architecture
    const ld_name = getDynamicLinkerName(arch);

    for (compilers) |compiler| {
        // Try to get dynamic linker path from compiler
        const ld_cmd = try std.fmt.allocPrint(arena, "-print-file-name={s}", .{ld_name});

        // TODO: Do we need to do something with this process' stdout,
        // or is this only here to continue to the next iteration?
        // Could be that it was forgotten before I refactored it and now to intent is lost.
        _ = process.Child.run(.{
            .allocator = arena,
            .argv = &[_][]const u8{ compiler, ld_cmd },
        }) catch continue;

        // Try to get libc path from compiler
        const libc_result = process.Child.run(.{
            .allocator = arena,
            .argv = &[_][]const u8{ compiler, "-print-file-name=libc.so" },
        }) catch continue;

        const libc_path = std.mem.trimRight(u8, libc_result.stdout, "\n\r \t");
        if (libc_path.len == 0 or std.mem.eql(u8, libc_path, "libc.so")) continue;

        // Validate path for security
        if (!validatePath(libc_path)) continue;

        // Verify the file exists and close it properly
        const libc_file = fs.openFileAbsolute(libc_path, .{}) catch continue;
        libc_file.close();

        const lib_dir = fs.path.dirname(libc_path) orelse continue;

        // Find dynamic linker
        const dynamic_linker = try findDynamicLinker(arena, arch, lib_dir) orelse continue;

        // Validate dynamic linker path
        if (!validatePath(dynamic_linker)) continue;

        return LibcInfo{
            .dynamic_linker = dynamic_linker,
            .libc_path = libc_path,
            .lib_dir = lib_dir,
            .arch = arch,
            .allocator = arena,
        };
    }

    return null;
}

/// Find libc by searching the filesystem
fn findViaFilesystem(arena: std.mem.Allocator) !LibcInfo {
    const arch = try getArchitecture(arena);
    const search_paths = try getSearchPaths(arena, arch);

    // Search for libc in standard paths
    for (search_paths) |lib_dir| {
        var dir = fs.openDirAbsolute(lib_dir, .{}) catch continue;
        defer dir.close();

        // Support both glibc and musl
        const libc_names = [_][]const u8{
            "libc.so.6", // glibc
            "libc.musl-x86_64.so.1", // musl x86_64
            "libc.musl-aarch64.so.1", // musl aarch64
            "libc.musl-arm.so.1", // musl arm
            "libc.so",
            "libc.a",
        };

        for (libc_names) |libc_name| {
            const libc_path = try fs.path.join(arena, &[_][]const u8{ lib_dir, libc_name });

            // Check if file exists and close it properly
            const libc_file = fs.openFileAbsolute(libc_path, .{}) catch continue;
            libc_file.close();

            // Try to find dynamic linker
            const dynamic_linker = try findDynamicLinker(arena, arch, lib_dir) orelse continue;

            // Validate paths for security
            if (!validatePath(libc_path) or !validatePath(dynamic_linker)) {
                continue;
            }

            return LibcInfo{
                .lib_dir = lib_dir,
                .dynamic_linker = dynamic_linker,
                .libc_path = libc_path,
                .arch = arch,
                .allocator = arena,
            };
        }
    }

    return error.LibcNotFound;
}

/// Find the dynamic linker for the given architecture
fn findDynamicLinker(arena: std.mem.Allocator, arch: []const u8, lib_dir: []const u8) !?[]const u8 {
    // Map architecture to dynamic linker names (including musl)
    const ld_names = if (std.mem.eql(u8, arch, "x86_64"))
        &[_][]const u8{ "ld-linux-x86-64.so.2", "ld-musl-x86_64.so.1", "ld-linux.so.2" }
    else if (std.mem.eql(u8, arch, "aarch64"))
        &[_][]const u8{ "ld-linux-aarch64.so.1", "ld-musl-aarch64.so.1", "ld-linux.so.1" }
    else if (std.mem.startsWith(u8, arch, "arm"))
        &[_][]const u8{ "ld-linux-armhf.so.3", "ld-musl-arm.so.1", "ld-linux.so.3" }
    else if (std.mem.eql(u8, arch, "i686") or std.mem.eql(u8, arch, "i386"))
        &[_][]const u8{ "ld-linux.so.2", "ld-musl-i386.so.1" }
    else
        &[_][]const u8{ "ld-linux.so.2", "ld.so.1" };

    // Search in the lib directory first
    for (ld_names) |ld_name| {
        const path = try fs.path.join(arena, &[_][]const u8{ lib_dir, ld_name });

        if (fs.openFileAbsolute(path, .{})) |file| {
            file.close();
            return path;
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
            const path = try fs.path.join(arena, &[_][]const u8{ search_dir, ld_name });

            if (fs.openFileAbsolute(path, .{})) |file| {
                file.close();
                return path;
            } else |_| {}
        }
    }

    return null;
}

/// Get system architecture using uname
fn getArchitecture(arena: std.mem.Allocator) ![]const u8 {
    const result = try process.Child.run(.{
        .allocator = arena,
        .argv = &[_][]const u8{ "uname", "-m" },
    });

    return std.mem.trimRight(u8, result.stdout, "\n\r \t");
}

/// Get library search paths for the given architecture
fn getSearchPaths(arena: std.mem.Allocator, arch: []const u8) ![]const []const u8 {
    const triplet = getMultiarchTriplet(arena, arch) catch |err| blk: {
        switch (err) {
            error.UnrecognisedArch => break :blk arch,
            else => |other_err| return other_err,
        }
    };

    // Dynamic string allocations for multiarch locations
    const path_lib_triplet = try std.fmt.allocPrint(arena, "/lib/{s}", .{triplet});
    const path_usr_lib_triplet = try std.fmt.allocPrint(arena, "/usr/lib/{s}", .{triplet});

    const arch_paths = if (std.mem.eql(u8, arch, "x86_64"))
        &[_][]const u8{
            "/lib64",
            "/usr/lib64",
            "/lib/x86_64-linux-gnu",
            "/usr/lib/x86_64-linux-gnu",
        }
    else if (std.mem.eql(u8, arch, "aarch64"))
        &[_][]const u8{
            "/lib64",
            "/usr/lib64",
            "/lib/aarch64-linux-gnu",
            "/usr/lib/aarch64-linux-gnu",
        }
    else if (std.mem.startsWith(u8, arch, "arm"))
        &[_][]const u8{
            "/lib32",
            "/usr/lib32",
            "/lib/arm-linux-gnueabihf",
            "/usr/lib/arm-linux-gnueabihf",
        }
    else
        &[_][]const u8{};

    // Always include these generic/musl paths
    const root_bases = [_][]const u8{
        path_lib_triplet,
        path_usr_lib_triplet,
        "/lib",
        "/usr/lib",
        "/usr/local/lib",
        "/lib/musl",
        "/usr/lib/musl",
    };

    const total_len = root_bases.len + arch_paths.len;
    const result = try arena.alloc([]const u8, total_len);
    @memcpy(result[0..root_bases.len], root_bases[0..]);
    @memcpy(result[root_bases.len..], arch_paths[0..]);

    return result;
}

/// Get multiarch triplet (e.g., x86_64-linux-gnu)
fn getMultiarchTriplet(arena: std.mem.Allocator, arch: []const u8) ![]const u8 {
    // Try to get from gcc first
    const result = process.Child.run(.{
        .allocator = arena,
        .argv = &[_][]const u8{ "gcc", "-dumpmachine" },
    }) catch |err| switch (err) {
        error.FileNotFound => {
            // Fallback to common triplets
            if (std.mem.eql(u8, arch, "x86_64")) {
                return "x86_64-linux-gnu";
            } else if (std.mem.eql(u8, arch, "aarch64")) {
                return "aarch64-linux-gnu";
            } else if (std.mem.startsWith(u8, arch, "arm")) {
                return "arm-linux-gnueabihf";
            } else {
                return error.UnrecognisedArch;
            }
        },
        else => return err,
    };

    return std.mem.trimRight(u8, result.stdout, "\n\r \t");
}

test "libc detection integration test" {
    if (builtin.os.tag != .linux) return error.SkipZigTest;

    var allocs: Allocators = undefined;
    allocs.initInPlace(std.testing.allocator);
    defer allocs.deinit();

    const libc_info = findLibc(&allocs) catch |err| switch (err) {
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

    // Verify paths are valid
    try std.testing.expect(validatePath(libc_info.dynamic_linker));
    try std.testing.expect(validatePath(libc_info.libc_path));

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
