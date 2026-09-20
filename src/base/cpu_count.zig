//! Worker thread count for parallel compilation.
//!
//! On hybrid Intel CPUs the kernel exposes the performance cores under
//! `/sys/devices/cpu_core/cpus`. Measured on a 12900KS (8 performance cores
//! with SMT, 8 efficiency cores), running the compiler with one worker per
//! logical CPU (24) is slower than one per performance-core thread (16):
//! the efficiency cores stretch the tail of every parallel wave and the extra
//! threads contend on the kernel's address-space lock while mapping memory.
//! Everywhere else the logical CPU count is used.
const std = @import("std");
const builtin = @import("builtin");

/// Number of worker threads to use when the caller did not choose one.
pub fn workerCount() usize {
    if (builtin.os.tag == .freestanding) return 1;
    if (builtin.os.tag == .linux) {
        if (performanceCoreCountLinux()) |count| return count;
    }
    return std.Thread.getCpuCount() catch 1;
}

fn performanceCoreCountLinux() ?usize {
    const linux = std.os.linux;
    const path = "/sys/devices/cpu_core/cpus";
    const fd_raw = linux.open(path, .{ .ACCMODE = .RDONLY, .CLOEXEC = true }, 0);
    if (linux.errno(fd_raw) != .SUCCESS) return null;
    const fd: linux.fd_t = @intCast(fd_raw);
    defer _ = linux.close(fd);
    var buf: [256]u8 = undefined;
    const read_raw = linux.read(fd, &buf, buf.len);
    if (linux.errno(read_raw) != .SUCCESS) return null;
    const count = parseCpuList(buf[0..read_raw]) orelse return null;
    return if (count == 0) null else count;
}

/// Parse a kernel cpulist such as `0-15` or `0-3,8-11` into a CPU count.
pub fn parseCpuList(text: []const u8) ?usize {
    var total: usize = 0;
    var it = std.mem.splitScalar(u8, std.mem.trim(u8, text, " \n\r\t"), ',');
    while (it.next()) |part| {
        if (part.len == 0) return null;
        if (std.mem.findScalar(u8, part, '-')) |dash| {
            const lo = std.fmt.parseInt(usize, part[0..dash], 10) catch return null;
            const hi = std.fmt.parseInt(usize, part[dash + 1 ..], 10) catch return null;
            if (hi < lo) return null;
            total += hi - lo + 1;
        } else {
            _ = std.fmt.parseInt(usize, part, 10) catch return null;
            total += 1;
        }
    }
    return total;
}

test "cpulist parsing" {
    try std.testing.expectEqual(@as(?usize, 16), parseCpuList("0-15\n"));
    try std.testing.expectEqual(@as(?usize, 8), parseCpuList("0-3,8-11"));
    try std.testing.expectEqual(@as(?usize, 3), parseCpuList("0,2,4"));
    try std.testing.expectEqual(@as(?usize, null), parseCpuList(""));
    try std.testing.expectEqual(@as(?usize, null), parseCpuList("5-2"));
}
