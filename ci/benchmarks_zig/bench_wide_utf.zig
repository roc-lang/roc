//! Compare wide-UTF decoders against a prior revision with a libc allocation host.
//! Run through bench_wide_utf.sh; reports median nanoseconds over five alternating samples.

const std = @import("std");
const old = @import("baseline");
const new = @import("current");
extern "c" fn clock_gettime(std.c.clockid_t, *std.c.timespec) c_int;
fn now() u64 {
    var ts: std.c.timespec = undefined;
    if (clock_gettime(.MONOTONIC, &ts) != 0) @panic("clock_gettime failed");
    return @as(u64, @intCast(ts.sec)) * 1_000_000_000 + @as(u64, @intCast(ts.nsec));
}
fn Host(comptime B: type) type {
    return struct {
        const Self = @This();
        const Ops = B.host_abi.RocOps;
        allocations: usize = 0,
        reallocations: usize = 0,
        frees: usize = 0,
        requested: usize = 0,
        fn self(ops: *Ops) *Self {
            return @ptrCast(@alignCast(ops.env));
        }
        fn alloc(ops: *Ops, len: usize, alignment: usize) callconv(.c) ?*anyopaque {
            std.debug.assert(alignment <= 16);
            const h = self(ops);
            h.allocations += 1;
            h.requested += len;
            return std.c.malloc(len);
        }
        fn realloc(ops: *Ops, ptr: *anyopaque, len: usize, alignment: usize) callconv(.c) ?*anyopaque {
            std.debug.assert(alignment <= 16);
            const h = self(ops);
            h.reallocations += 1;
            h.requested += len;
            return std.c.realloc(ptr, len);
        }
        fn free(ops: *Ops, ptr: *anyopaque, _: usize) callconv(.c) void {
            self(ops).frees += 1;
            std.c.free(ptr);
        }
        fn message(_: *Ops, ptr: [*]const u8, len: usize) callconv(.c) void {
            std.debug.panic("{s}", .{ptr[0..len]});
        }
        fn makeOps(h: *Self) Ops {
            return .{ .env = h, .roc_alloc = alloc, .roc_realloc = realloc, .roc_dealloc = free, .roc_dbg = message, .roc_expect_failed = message, .roc_crashed = message, .hosted_fns = B.host_abi.emptyHostedFunctions() };
        }
    };
}
noinline fn decode(comptime B: type, comptime Unit: type, comptime lossy: bool, units: []const Unit, ops: *B.host_abi.RocOps) u64 {
    const list = B.list.RocList{ .bytes = @ptrCast(@constCast(units.ptr)), .length = units.len, .capacity_or_alloc_ptr = B.list.RocList.encodeCapacity(units.len) };
    const result = if (lossy) blk: {
        const str = if (Unit == u16) B.str.fromUtf16Lossy(list, ops) else B.str.fromUtf32Lossy(list, ops);
        break :blk B.str.FromWideUtfTry{ .string = str, .index = 0, .is_ok = true, .problem_code = 0 };
    } else if (Unit == u16) B.str.fromUtf16(list, ops) else B.str.fromUtf32(list, ops);
    defer result.string.decref(ops);
    const bytes = result.string.asSlice();
    std.mem.doNotOptimizeAway(bytes);
    return bytes.len + result.index + result.problem_code;
}
fn timed(comptime B: type, comptime Unit: type, comptime lossy: bool, units: []const Unit, count: usize) f64 {
    var host: Host(B) = .{};
    var ops = host.makeOps();
    var checksum: u64 = 0;
    const start = now();
    for (0..count) |_| {
        // Make the input address opaque on each call. A memory clobber alone
        // still lets LLVM hoist an allocation-free, immediately failing decode.
        const input = asm volatile (""
            : [output] "=r" (-> [*]const Unit),
            : [input] "0" (units.ptr),
            : .{ .memory = true });
        checksum +%= decode(B, Unit, lossy, input[0..units.len], &ops);
    }
    const elapsed = now() - start;
    std.mem.doNotOptimizeAway(checksum);
    if (host.allocations != host.frees) @panic("decoder leaked output storage");
    return @as(f64, @floatFromInt(elapsed)) / @as(f64, @floatFromInt(count));
}
fn bench(comptime Unit: type, comptime lossy: bool, name: []const u8, units: []const Unit) void {
    // Strict failures at the first unit take constant time, even for large
    // inputs. Keep their sample duration above the host clock's resolution.
    const count = if (!lossy and std.mem.eql(u8, name, "invalid_head")) 200_000 else @max(128, @min(200_000, 8 * 1024 * 1024 / units.len));
    _ = timed(old, Unit, lossy, units, 16);
    _ = timed(new, Unit, lossy, units, 16);
    var olds: [5]f64 = undefined;
    var news: [5]f64 = undefined;
    for (0..5) |i| {
        if (i % 2 == 0) {
            olds[i] = timed(old, Unit, lossy, units, count);
            news[i] = timed(new, Unit, lossy, units, count);
        } else {
            news[i] = timed(new, Unit, lossy, units, count);
            olds[i] = timed(old, Unit, lossy, units, count);
        }
    }
    std.mem.sort(f64, &olds, {}, std.sort.asc(f64));
    std.mem.sort(f64, &news, {}, std.sort.asc(f64));
    var host: Host(new) = .{};
    var ops = host.makeOps();
    _ = decode(new, Unit, lossy, units, &ops);
    std.debug.print("{s},{s},{s},{d},{d:.1},{d:.1},{d:.2},{d},{d},{d}\n", .{ @typeName(Unit), if (lossy) "lossy" else "strict", name, units.len, olds[2], news[2], olds[2] / news[2], host.allocations, host.reallocations, host.requested });
}
/// Run fixed decoding workloads and print CSV measurements to stderr.
pub fn main() error{OutOfMemory}!void {
    std.debug.print("unit,mode,case,units,old_ns,new_ns,speedup,allocs,reallocs,requested_bytes\n", .{});
    inline for (.{ u16, u32 }) |Unit| {
        for ([_]usize{ 8, 23, 4096, 1024 * 1024 }) |len| {
            const units = try std.heap.page_allocator.alloc(Unit, len);
            defer std.heap.page_allocator.free(units);
            inline for (.{ false, true }) |lossy| {
                @memset(units, 65);
                bench(Unit, lossy, "ascii", units);
                for (units, 0..) |*v, i| v.* = if (i % 64 == 63) 0xe9 else 65;
                bench(Unit, lossy, "mostly_ascii", units);
                @memset(units, 0x4e2d);
                bench(Unit, lossy, "bmp", units);
                for (units, 0..) |*v, i| v.* = if (Unit == u16) (if (i % 2 == 0) @as(Unit, 0xd83d) else @as(Unit, 0xdc26)) else 0x1f426;
                if (Unit == u16 and len % 2 != 0) units[len - 1] = 65;
                bench(Unit, lossy, "supplementary", units);
                @memset(units, 65);
                units[len - 1] = if (Unit == u16) 0xd800 else 0xffffffff;
                bench(Unit, lossy, "invalid_tail", units);
                @memset(units, 65);
                units[0] = if (Unit == u16) 0xd800 else 0xffffffff;
                bench(Unit, lossy, "invalid_head", units);
            }
        }
    }
}
