//! CPU topology for parallel compilation: how many worker threads to run and
//! where to put background helper threads.
//!
//! Hybrid CPUs mix fast cores with slower, more efficient ones. Measured on a
//! 12900KS (8 performance cores with SMT, 8 efficiency cores), running the
//! compiler with one worker per logical CPU (24) is slower than one per
//! performance-core thread (16): the efficiency cores stretch the tail of
//! every parallel wave and the extra threads contend on the kernel's
//! address-space lock while mapping memory. So the worker count is the number
//! of logical CPUs on the fastest core type, and background helpers such as
//! the allocator's page prefaulter are pinned to the slow cores, where they
//! never steal time from the workers.
//!
//! Each platform names the core types differently:
//! - Linux exposes them as `/sys/devices/cpu_core/cpus` (fast) and
//!   `/sys/devices/cpu_atom/cpus` (slow) on Intel hybrid parts.
//! - macOS reports performance levels through `sysctl`; level 0 is the fastest.
//!   There is no thread affinity API, so helpers are demoted to the
//!   background quality-of-service class, which the scheduler runs on the
//!   efficiency cores.
//! - Windows reports an efficiency class per core through
//!   `GetLogicalProcessorInformationEx`; a higher class is a faster core.
//!
//! A machine whose cores are all the same type uses every logical CPU.
const std = @import("std");
const builtin = @import("builtin");

/// Number of worker threads to use when the caller did not choose one.
pub fn workerCount() usize {
    if (builtin.os.tag == .freestanding) return 1;
    if (fastCoreLogicalCount()) |count| return count;
    return std.Thread.getCpuCount() catch 1;
}

/// Logical CPUs on the fastest core type, or null when the machine is not
/// hybrid or the platform does not say.
fn fastCoreLogicalCount() ?usize {
    return switch (builtin.os.tag) {
        .linux => linux_topology.fastCoreLogicalCount(),
        .macos => darwin_topology.fastCoreLogicalCount(),
        .windows => windows_topology.fastCoreLogicalCount(),
        else => null,
    };
}

/// Move the calling thread onto the slow cores of a hybrid CPU so that it
/// never competes with compiler workers for a fast core. Does nothing on a
/// machine whose cores are all the same type or on platforms without a way
/// to express the preference.
pub fn pinCurrentThreadToEfficiencyCores() void {
    switch (builtin.os.tag) {
        .linux => linux_topology.pinCurrentThreadToEfficiencyCores(),
        .macos => darwin_topology.pinCurrentThreadToEfficiencyCores(),
        .windows => windows_topology.pinCurrentThreadToEfficiencyCores(),
        else => {},
    }
}

/// Bit set over CPU numbers with the layout of Linux's `cpu_set_t`.
pub const CpuSet = [128 / @sizeOf(usize)]usize;
const cpu_set_bits = @bitSizeOf(CpuSet);

fn cpuSetInsert(set: *CpuSet, cpu: usize) bool {
    if (cpu >= cpu_set_bits) return false;
    const word_bits = @bitSizeOf(usize);
    set[cpu / word_bits] |= @as(usize, 1) << @intCast(cpu % word_bits);
    return true;
}

/// Parse a kernel cpulist such as `0-15` or `0-3,8-11`, setting each listed
/// CPU in `set`, and return the number of CPUs listed.
pub fn parseCpuList(text: []const u8, set: *CpuSet) ?usize {
    set.* = @splat(0);
    var total: usize = 0;
    var it = std.mem.splitScalar(u8, std.mem.trim(u8, text, " \n\r\t"), ',');
    while (it.next()) |part| {
        if (part.len == 0) return null;
        var lo: usize = undefined;
        var hi: usize = undefined;
        if (std.mem.findScalar(u8, part, '-')) |dash| {
            lo = std.fmt.parseInt(usize, part[0..dash], 10) catch return null;
            hi = std.fmt.parseInt(usize, part[dash + 1 ..], 10) catch return null;
            if (hi < lo) return null;
        } else {
            lo = std.fmt.parseInt(usize, part, 10) catch return null;
            hi = lo;
        }
        var cpu = lo;
        while (true) : (cpu += 1) {
            if (!cpuSetInsert(set, cpu)) return null;
            if (cpu == hi) break;
        }
        total += hi - lo + 1;
    }
    return total;
}

const linux_topology = struct {
    const linux = std.os.linux;

    fn fastCoreLogicalCount() ?usize {
        var set: CpuSet = undefined;
        const count = readCpuList("/sys/devices/cpu_core/cpus", &set) orelse return null;
        return if (count == 0) null else count;
    }

    fn pinCurrentThreadToEfficiencyCores() void {
        var set: CpuSet = undefined;
        const count = readCpuList("/sys/devices/cpu_atom/cpus", &set) orelse return;
        if (count == 0) return;
        linux.sched_setaffinity(0, &set) catch {};
    }

    fn readCpuList(path: [*:0]const u8, set: *CpuSet) ?usize {
        const fd_raw = linux.open(path, .{ .ACCMODE = .RDONLY, .CLOEXEC = true }, 0);
        if (linux.errno(fd_raw) != .SUCCESS) return null;
        const fd: linux.fd_t = @intCast(fd_raw);
        defer _ = linux.close(fd);
        var buf: [256]u8 = undefined;
        const read_raw = linux.read(fd, &buf, buf.len);
        if (linux.errno(read_raw) != .SUCCESS) return null;
        return parseCpuList(buf[0..read_raw], set);
    }
};

const darwin_topology = struct {
    fn fastCoreLogicalCount() ?usize {
        const levels = sysctlInt("hw.nperflevels") orelse return null;
        if (levels < 2) return null;
        const fast = sysctlInt("hw.perflevel0.logicalcpu") orelse return null;
        return if (fast == 0) null else fast;
    }

    fn pinCurrentThreadToEfficiencyCores() void {
        if (darwin_topology.fastCoreLogicalCount() == null) return;
        _ = std.c.pthread_set_qos_class_self_np(.BACKGROUND, 0);
    }

    fn sysctlInt(name: [*:0]const u8) ?usize {
        var value: c_int = 0;
        var len: usize = @sizeOf(c_int);
        if (std.c.sysctlbyname(name, &value, &len, null, 0) != 0) return null;
        if (len != @sizeOf(c_int) or value < 0) return null;
        return @intCast(value);
    }
};

const windows_topology = struct {
    const windows = std.os.windows;

    const GroupAffinity = extern struct {
        mask: windows.KAFFINITY,
        group: windows.WORD,
        reserved: [3]windows.WORD,
    };

    const ProcessorRelationship = extern struct {
        flags: windows.BYTE,
        efficiency_class: windows.BYTE,
        reserved: [20]windows.BYTE,
        group_count: windows.WORD,
        group_mask: [1]GroupAffinity,
    };

    const ProcessorInformation = extern struct {
        relationship: windows.DWORD,
        size: windows.DWORD,
        processor: ProcessorRelationship,
    };

    const relation_processor_core: windows.DWORD = 0;

    extern "kernel32" fn GetLogicalProcessorInformationEx(
        relationship_type: windows.DWORD,
        buffer: ?[*]align(8) u8,
        returned_length: *windows.DWORD,
    ) callconv(.winapi) windows.BOOL;

    extern "kernel32" fn GetCurrentThread() callconv(.winapi) windows.HANDLE;

    extern "kernel32" fn SetThreadGroupAffinity(
        thread: windows.HANDLE,
        group_affinity: *const GroupAffinity,
        previous_group_affinity: ?*GroupAffinity,
    ) callconv(.winapi) windows.BOOL;

    /// One entry per physical core in `GetLogicalProcessorInformationEx`'s
    /// output, with the group masks flattened to a single group: cores on a
    /// hybrid part all live in group 0 on any machine small enough to have
    /// only one processor group, and helper pinning only needs one group.
    const Core = struct {
        efficiency_class: u8,
        group: u16,
        mask: usize,
    };

    const CoreWalker = struct {
        buffer: []align(8) const u8,
        offset: usize = 0,

        fn next(self: *CoreWalker) ?Core {
            while (self.offset + @sizeOf(ProcessorInformation) <= self.buffer.len) {
                const info: *align(8) const ProcessorInformation = @ptrCast(@alignCast(self.buffer[self.offset..].ptr));
                const size: usize = info.size;
                if (size < @sizeOf(ProcessorInformation) or self.offset + size > self.buffer.len) return null;
                self.offset += size;
                if (info.relationship != relation_processor_core) continue;
                if (info.processor.group_count == 0) continue;
                return .{
                    .efficiency_class = info.processor.efficiency_class,
                    .group = info.processor.group_mask[0].group,
                    .mask = info.processor.group_mask[0].mask,
                };
            }
            return null;
        }
    };

    const max_buffer_len = 64 * 1024;

    /// Fill `buffer` with the core relationships; the returned slice is
    /// empty when the machine has more cores than fit.
    fn readCores(buffer: *align(8) [max_buffer_len]u8) []align(8) const u8 {
        var len: windows.DWORD = max_buffer_len;
        if (GetLogicalProcessorInformationEx(relation_processor_core, buffer, &len) == .FALSE) return buffer[0..0];
        return buffer[0..len];
    }

    const ClassRange = struct { lowest: u8, highest: u8 };

    fn classRange(bytes: []align(8) const u8) ?ClassRange {
        var walker = CoreWalker{ .buffer = bytes };
        var range: ?ClassRange = null;
        while (walker.next()) |core| {
            if (range) |*r| {
                r.lowest = @min(r.lowest, core.efficiency_class);
                r.highest = @max(r.highest, core.efficiency_class);
            } else {
                range = .{ .lowest = core.efficiency_class, .highest = core.efficiency_class };
            }
        }
        return range;
    }

    fn fastCoreLogicalCount() ?usize {
        var buffer: [max_buffer_len]u8 align(8) = undefined;
        const bytes = readCores(&buffer);
        const range = classRange(bytes) orelse return null;
        if (range.lowest == range.highest) return null;
        var walker = CoreWalker{ .buffer = bytes };
        var count: usize = 0;
        while (walker.next()) |core| {
            if (core.efficiency_class == range.highest) count += @popCount(core.mask);
        }
        return if (count == 0) null else count;
    }

    fn pinCurrentThreadToEfficiencyCores() void {
        var buffer: [max_buffer_len]u8 align(8) = undefined;
        const bytes = readCores(&buffer);
        const range = classRange(bytes) orelse return;
        if (range.lowest == range.highest) return;
        var walker = CoreWalker{ .buffer = bytes };
        var affinity: ?GroupAffinity = null;
        while (walker.next()) |core| {
            if (core.efficiency_class != range.lowest) continue;
            if (affinity) |*a| {
                if (a.group == core.group) a.mask |= core.mask;
            } else {
                affinity = .{ .mask = core.mask, .group = core.group, .reserved = @splat(0) };
            }
        }
        const chosen = affinity orelse return;
        _ = SetThreadGroupAffinity(GetCurrentThread(), &chosen, null);
    }
};

test "cpulist parsing" {
    var set: CpuSet = undefined;
    try std.testing.expectEqual(@as(?usize, 16), parseCpuList("0-15\n", &set));
    try std.testing.expectEqual(@as(usize, 0xFFFF), set[0]);
    try std.testing.expectEqual(@as(?usize, 8), parseCpuList("0-3,8-11", &set));
    try std.testing.expectEqual(@as(usize, 0x0F0F), set[0]);
    try std.testing.expectEqual(@as(?usize, 3), parseCpuList("0,2,4", &set));
    try std.testing.expectEqual(@as(usize, 0b10101), set[0]);
    try std.testing.expectEqual(@as(?usize, 2), parseCpuList("63-64", &set));
    try std.testing.expectEqual(@as(usize, 1) << 63, set[0]);
    try std.testing.expectEqual(@as(usize, 1), set[1]);
    try std.testing.expectEqual(@as(?usize, null), parseCpuList("", &set));
    try std.testing.expectEqual(@as(?usize, null), parseCpuList("5-2", &set));
    try std.testing.expectEqual(@as(?usize, null), parseCpuList("2000", &set));
}

test "worker count is at least one" {
    try std.testing.expect(workerCount() >= 1);
}
