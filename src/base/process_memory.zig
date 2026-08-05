//! Current process memory footprint, for phase-attributed reporting.

const std = @import("std");
const builtin = @import("builtin");

/// Current process memory footprint in bytes, or null where unsupported.
/// On macOS this is the physical footprint (what Activity Monitor reports,
/// including compressed pages), so a phase's reading is not deflated by the
/// memory compressor evicting an earlier phase's idle pages.
pub fn currentBytes() ?u64 {
    switch (comptime builtin.os.tag) {
        .macos => {
            var info: DarwinRusageInfoV2 = undefined;
            if (proc_pid_rusage(std.c.getpid(), rusage_info_v2_flavor, &info) != 0) return null;
            return info.ri_phys_footprint;
        },
        .linux => {
            const linux = std.os.linux;
            const open_rc = linux.openat(linux.AT.FDCWD, "/proc/self/statm", .{}, 0);
            if (linux.errno(open_rc) != .SUCCESS) return null;
            const fd: i32 = @intCast(open_rc);
            defer _ = linux.close(fd);
            var buf: [128]u8 = undefined;
            const read_rc = linux.read(fd, &buf, buf.len);
            if (linux.errno(read_rc) != .SUCCESS) return null;
            var it = std.mem.tokenizeScalar(u8, buf[0..read_rc], ' ');
            _ = it.next() orelse return null;
            const resident_pages = it.next() orelse return null;
            const pages = std.fmt.parseInt(u64, resident_pages, 10) catch return null;
            return pages * std.heap.pageSize();
        },
        .windows => {
            var counters: WindowsProcessMemoryCounters = undefined;
            counters.cb = @sizeOf(WindowsProcessMemoryCounters);
            if (K32GetProcessMemoryInfo(std.os.windows.GetCurrentProcess(), &counters, counters.cb) == 0) return null;
            return counters.WorkingSetSize;
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
        .freebsd,
        .openbsd,
        .netbsd,
        .driverkit,
        .ios,
        .maccatalyst,
        .tvos,
        .visionos,
        .watchos,
        .uefi,
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
        => return null,
    }
}

/// `struct rusage_info_v2` from XNU's resource.h; the layout is kernel ABI.
const DarwinRusageInfoV2 = extern struct {
    ri_uuid: [16]u8,
    ri_user_time: u64,
    ri_system_time: u64,
    ri_pkg_idle_wkups: u64,
    ri_interrupt_wkups: u64,
    ri_pageins: u64,
    ri_wired_size: u64,
    ri_resident_size: u64,
    ri_phys_footprint: u64,
    ri_proc_start_abstime: u64,
    ri_proc_exit_abstime: u64,
    ri_child_user_time: u64,
    ri_child_system_time: u64,
    ri_child_pkg_idle_wkups: u64,
    ri_child_interrupt_wkups: u64,
    ri_child_pageins: u64,
    ri_child_elapsed_abstime: u64,
    ri_diskio_bytesread: u64,
    ri_diskio_byteswritten: u64,
};

const rusage_info_v2_flavor: c_int = 2;

extern "c" fn proc_pid_rusage(pid: std.c.pid_t, flavor: c_int, buffer: *DarwinRusageInfoV2) c_int;

const WindowsProcessMemoryCounters = extern struct {
    cb: u32,
    PageFaultCount: u32,
    PeakWorkingSetSize: usize,
    WorkingSetSize: usize,
    QuotaPeakPagedPoolUsage: usize,
    QuotaPagedPoolUsage: usize,
    QuotaPeakNonPagedPoolUsage: usize,
    QuotaNonPagedPoolUsage: usize,
    PagefileUsage: usize,
    PeakPagefileUsage: usize,
};

extern "kernel32" fn K32GetProcessMemoryInfo(
    process: std.os.windows.HANDLE,
    counters: *WindowsProcessMemoryCounters,
    cb: u32,
) callconv(.winapi) i32;
