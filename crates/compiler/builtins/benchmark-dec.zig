const std = @import("std");
const time = std.time;
const Timer = time.Timer;

const RocStr = @import("./bitcode/src/str.zig").RocStr;
const RocDec = @import("./bitcode/src/dec.zig").RocDec;

fn roc_alloc(_: usize, _: u32) callconv(.c) ?*anyopaque {
    @panic("Not needed for dec benchmark");
}
fn roc_panic(_: *anyopaque, _: u32) callconv(.c) void {
    @panic("Not needed for dec benchmark");
}
fn roc_dbg(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {
    @panic("Not needed for dec benchmark");
}

comptime {
    @export(roc_alloc, .{ .name = "roc_alloc", .linkage = .strong });
    @export(roc_panic, .{ .name = "roc_panic", .linkage = .strong });
    @export(roc_dbg, .{ .name = "roc_dbg", .linkage = .strong });
}

var timer: Timer = undefined;

pub export fn main() u8 {
    run_tests() catch |err| {
        std.debug.print("Error: {}\n", .{err});
        return 1;
    };
    return 0;
}

fn run_tests() !void {
    const stdout = std.fs.File.stdout().deprecatedWriter();
    try stdout.print("Warning: Timer seems to step in units of 41ns\n\n", .{});
    timer = try Timer.start();

    const n = 1000;

    // Add/Sub are too fast and need a higher n.
    const add_sub_n = 10000;

    // This number are very close to 1 to avoid over and underflow.
    const f1 = 1.00123;
    const dec1 = RocDec.fromF64(f1) orelse {
        @panic("Failed to create RocDec from f64");
    };

    // `asin` and `acos` have a limited range, so they will use this value.
    const f2 = 0.00130000847;
    const dec2 = RocDec.fromF64(f2) orelse {
        @panic("Failed to create RocDec from f64");
    };

    try stdout.print("Dec:\n", .{});
    try stdout.print("{} additions took ", .{add_sub_n});
    const decAdd = try avg_runs(RocDec, add_sub_n, RocDec.add, dec1);

    try stdout.print("{} subtractions took ", .{add_sub_n});
    const decSub = try avg_runs(RocDec, add_sub_n, RocDec.sub, dec1);

    try stdout.print("{} multiplications took ", .{n});
    const decMul = try avg_runs(RocDec, n, RocDec.mul, dec1);

    try stdout.print("{} divisions took ", .{n});
    const decDiv = try avg_runs(RocDec, n, RocDec.div, dec1);

    try stdout.print("{} sin took ", .{n});
    const decSin = try avg_runs(RocDec, n, sinDec, dec1);

    try stdout.print("{} cos took ", .{n});
    const decCos = try avg_runs(RocDec, n, cosDec, dec1);

    try stdout.print("{} tan took ", .{n});
    const decTan = try avg_runs(RocDec, n, tanDec, dec1);

    try stdout.print("{} asin took ", .{n});
    const decAsin = try avg_runs(RocDec, n, asinDec, dec2);

    try stdout.print("{} acos took ", .{n});
    const decAcos = try avg_runs(RocDec, n, acosDec, dec2);

    try stdout.print("{} atan took ", .{n});
    const decAtan = try avg_runs(RocDec, n, atanDec, dec1);

    try stdout.print("\n\nF64:\n", .{});
    try stdout.print("{} additions took ", .{add_sub_n});
    const f64Add = try avg_runs(f64, add_sub_n, addF64, f1);

    try stdout.print("{} subtractions took ", .{add_sub_n});
    const f64Sub = try avg_runs(f64, add_sub_n, subF64, f1);

    try stdout.print("{} multiplications took ", .{n});
    const f64Mul = try avg_runs(f64, n, mulF64, f1);

    try stdout.print("{} divisions took ", .{n});
    const f64Div = try avg_runs(f64, n, divF64, f1);

    try stdout.print("{} sin took ", .{n});
    const f64Sin = try avg_runs(f64, n, sinF64, f1);

    try stdout.print("{} cos took ", .{n});
    const f64Cos = try avg_runs(f64, n, cosF64, f1);

    try stdout.print("{} tan took ", .{n});
    const f64Tan = try avg_runs(f64, n, tanF64, f1);

    try stdout.print("{} asin took ", .{n});
    const f64Asin = try avg_runs(f64, n, asinF64, f2);

    try stdout.print("{} acos took ", .{n});
    const f64Acos = try avg_runs(f64, n, acosF64, f2);

    try stdout.print("{} atan took ", .{n});
    const f64Atan = try avg_runs(f64, n, atanF64, f1);

    try stdout.print("\n\nDec/F64:\n", .{});
    try stdout.print("addition:       {d:0.2}\n", .{@as(f64, @floatFromInt(decAdd)) / @as(f64, @floatFromInt(f64Add))});
    try stdout.print("subtraction:    {d:0.2}\n", .{@as(f64, @floatFromInt(decSub)) / @as(f64, @floatFromInt(f64Sub))});
    try stdout.print("multiplication: {d:0.2}\n", .{@as(f64, @floatFromInt(decMul)) / @as(f64, @floatFromInt(f64Mul))});
    try stdout.print("division:       {d:0.2}\n", .{@as(f64, @floatFromInt(decDiv)) / @as(f64, @floatFromInt(f64Div))});
    try stdout.print("sin:            {d:0.2}\n", .{@as(f64, @floatFromInt(decSin)) / @as(f64, @floatFromInt(f64Sin))});
    try stdout.print("cos:            {d:0.2}\n", .{@as(f64, @floatFromInt(decCos)) / @as(f64, @floatFromInt(f64Cos))});
    try stdout.print("tan:            {d:0.2}\n", .{@as(f64, @floatFromInt(decTan)) / @as(f64, @floatFromInt(f64Tan))});
    try stdout.print("asin:           {d:0.2}\n", .{@as(f64, @floatFromInt(decAsin)) / @as(f64, @floatFromInt(f64Asin))});
    try stdout.print("acos:           {d:0.2}\n", .{@as(f64, @floatFromInt(decAcos)) / @as(f64, @floatFromInt(f64Acos))});
    try stdout.print("atan:           {d:0.2}\n", .{@as(f64, @floatFromInt(decAtan)) / @as(f64, @floatFromInt(f64Atan))});
}

fn avg_runs(comptime T: type, comptime n: usize, comptime op: fn (T, T) T, v: T) !u64 {
    const stdout = std.fs.File.stdout().deprecatedWriter();

    const warmups = 10000;
    const repeats = 10000;
    var runs = [_]u64{0} ** (warmups + repeats);

    var i: usize = 0;
    while (i < warmups + repeats) : (i += 1) {
        // Never inline run to ensure it doesn't optimize for the value of `v`.
        runs[i] = @call(.never_inline, run, .{ T, n, op, v });
    }

    const real_runs = runs[warmups..runs.len];
    std.sort.insertion(u64, real_runs, {}, comptime std.sort.asc(u64));

    const median = real_runs[real_runs.len / 2];
    const highest = real_runs[real_runs.len - 1];
    const lowest = real_runs[0];

    try stdout.print("{}ns (lowest: {}ns, highest: {}ns)\n", .{ median, lowest, highest });
    return median;
}

fn run(comptime T: type, comptime n: usize, comptime op: fn (T, T) T, v: T) u64 {
    var a = v;
    timer.reset();

    // Split into outer and inner loop to avoid breaking comptime.
    const max_inline = 100;
    const outer = n / max_inline;
    const inner = @min(n, max_inline);
    var i: usize = 0;
    while (i < outer) : (i += 1) {
        comptime var j = 0;
        inline while (j < inner) : (j += 1) {
            a = @call(.always_inline, op, .{ a, v });
        }
    }
    const rem = n % max_inline;
    const j = 0;
    inline while (j < rem) : (j += 1) {
        a = @call(.always_inline, op, .{ a, v });
    }

    // Clobber `a` to avoid removal as dead code.
    asm volatile (""
        :
        : [a] "r,m" (&a),
        : "memory"
    );
    return timer.read();
}

fn addF64(x: f64, y: f64) f64 {
    return x + y;
}
fn subF64(x: f64, y: f64) f64 {
    return x - y;
}
fn mulF64(x: f64, y: f64) f64 {
    return x * y;
}
fn divF64(x: f64, y: f64) f64 {
    return x / y;
}
fn sinF64(x: f64, _: f64) f64 {
    return std.math.sin(x);
}
fn cosF64(x: f64, _: f64) f64 {
    return std.math.cos(x);
}
fn tanF64(x: f64, _: f64) f64 {
    return std.math.tan(x);
}
fn asinF64(x: f64, _: f64) f64 {
    return std.math.asin(x);
}
const pi_over_2 = std.math.pi / 2.0;
fn acosF64(x: f64, _: f64) f64 {
    // acos is only stable if we subtract pi/2.
    // The perf should be essentially the same because subtraction is much faster than acos.
    return std.math.acos(x) - pi_over_2;
}
fn atanF64(x: f64, _: f64) f64 {
    return std.math.atan(x);
}

fn sinDec(x: RocDec, _: RocDec) RocDec {
    return x.sin();
}
fn cosDec(x: RocDec, _: RocDec) RocDec {
    return x.cos();
}
fn tanDec(x: RocDec, _: RocDec) RocDec {
    return x.tan();
}
fn asinDec(x: RocDec, _: RocDec) RocDec {
    return x.asin();
}
const pi_over_2_dec = RocDec.fromF64(pi_over_2).?;
fn acosDec(x: RocDec, _: RocDec) RocDec {
    // acos is only stable if we subtract pi/2.
    // The perf should be essentially the same because subtraction is much faster than acos.
    return x.acos().sub(pi_over_2_dec);
}
fn atanDec(x: RocDec, _: RocDec) RocDec {
    return x.atan();
}
