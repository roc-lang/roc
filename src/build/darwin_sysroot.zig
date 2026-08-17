//! Resolution of the Darwin sysroot passed as `-syslibroot` to every embedded-lld
//! Mach-O link, shared by the CLI linker and the LLVM shared-library link.
//!
//! Distributed builds ship a `darwin` directory next to the `roc` executable and
//! must use it, because the sysroot path baked in at build time names a directory
//! on the machine that produced the release. Builds run from their own checkout
//! have no such directory and use the baked-in path.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

const Allocator = std.mem.Allocator;

const SelfExePathError = std.Io.Dir.ReadLinkError || error{
    NameTooLong,
    UnsupportedOs,
};

const SelfExeDirError = Allocator.Error || SelfExePathError || std.Io.Dir.RealPathFileError || error{
    NoExeDirectory,
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
        .freestanding,
        .other,
        .contiki,
        .fuchsia,
        .hermit,
        .managarm,
        .haiku,
        .hurd,
        .illumos,
        .plan9,
        .rtems,
        .serenity,
        .dragonfly,
        .driverkit,
        .maccatalyst,
        .uefi,
        .freebsd,
        .openbsd,
        .netbsd,
        .@"3ds",
        .ps3,
        .ps4,
        .ps5,
        .psp,
        .vita,
        .emscripten,
        .wasi,
        .amdhsa,
        .amdpal,
        .cuda,
        .mesa3d,
        .nvcl,
        .opencl,
        .opengl,
        .vulkan,
        => return error.UnsupportedOs,
    }
}

/// Get the directory containing the currently running executable.
fn getSelfExeDir(allocator: Allocator, std_io: std.Io) SelfExeDirError![]const u8 {
    var symlink_path_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const symlink_path = try selfExePath(std_io, &symlink_path_buf);
    var real_path_buf: [std.Io.Dir.max_path_bytes]u8 = undefined;
    const exe_path_len = try std.Io.Dir.cwd().realPathFile(std_io, symlink_path, &real_path_buf);
    const exe_path = real_path_buf[0..exe_path_len];
    const exe_dir = std.fs.path.dirname(exe_path) orelse return error.NoExeDirectory;
    return allocator.dupe(u8, exe_dir);
}

/// Find the Darwin sysroot to link against.
pub fn find(allocator: Allocator, std_io: std.Io) Allocator.Error![]const u8 {
    const exe_dir = getSelfExeDir(allocator, std_io) catch |err| {
        std.log.warn("Failed to resolve executable path: {}, falling back to compile-time path", .{err});
        return build_options.darwin_sysroot;
    };

    const runtime_sysroot = try std.fs.path.join(allocator, &.{ exe_dir, "darwin" });
    const tbd_path = try std.fs.path.join(allocator, &.{ runtime_sysroot, "usr", "lib", "libSystem.tbd" });

    std.Io.Dir.cwd().access(std_io, tbd_path, .{}) catch {
        return build_options.darwin_sysroot;
    };

    return runtime_sysroot;
}
