const std = @import("std");
const time = std.time;
const Timer = time.Timer;

const RocStr = @import("../src/str.zig").RocStr;
const RocDec = @import("../src/dec.zig").RocDec;

fn roc_alloc(_: usize, _: u32) callconv(.C) ?*anyopaque {
    @panic("Not needed for dec benchmark");
}
fn roc_panic(_: *anyopaque, _: u32) callconv(.C) void {
    @panic("Not needed for dec benchmark");
}

comptime {
    @export(roc_alloc, .{ .name = "roc_alloc", .linkage = .Strong });
    @export(roc_panic, .{ .name = "roc_panic", .linkage = .Strong });
}

var timer: Timer = undefined;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Warning: Timer seems to step in units of 41ns\n\n", .{});
    timer = try Timer.start();

    const n = 10000;

    // This number are very close to 1 to avoid over and underflow.
    const str1 = "1.00123";
    const f1 = 1.00123;
    const dec1 = RocDec.fromStr(RocStr.init(str1, 3)).?;

    try stdout.print("Dec:\n", .{});
    try stdout.print("{} additions took ", .{n});
    const decAdd = try avg_runs(RocDec, n, RocDec.add, dec1);

    try stdout.print("{} subtractions took ", .{n});
    const decSub = try avg_runs(RocDec, n, RocDec.sub, dec1);

    try stdout.print("{} multiplications took ", .{n});
    const decMul = try avg_runs(RocDec, n, RocDec.mul, dec1);

    try stdout.print("{} divisions took ", .{n});
    const decDiv = try avg_runs(RocDec, n, RocDec.div, dec1);

    try stdout.print("{} sin took ", .{n});
    const decSin = try avg_runs(RocDec, n, sinDec, dec1);

    try stdout.print("{} asin took ", .{n});
    const decAsin = try avg_runs(RocDec, n, asinDec, dec1);

    try stdout.print("\n\nF64:\n", .{});
    try stdout.print("{} additions took ", .{n});
    const f64Add = try avg_runs(f64, n, addF64, f1);

    try stdout.print("{} subtractions took ", .{n});
    const f64Sub = try avg_runs(f64, n, subF64, f1);

    try stdout.print("{} multiplications took ", .{n});
    const f64Mul = try avg_runs(f64, n, mulF64, f1);

    try stdout.print("{} divisions took ", .{n});
    const f64Div = try avg_runs(f64, n, divF64, f1);

    try stdout.print("{} sin took ", .{n});
    const f64Sin = try avg_runs(f64, n, sinF64, f1);

    try stdout.print("{} asin took ", .{n});
    const f64Asin = try avg_runs(f64, n, asinF64, f1);

    try stdout.print("\n\nDec/F64:\n", .{});
    try stdout.print("addition:       {d:0.2}\n", .{@intToFloat(f64, decAdd) / @intToFloat(f64, f64Add)});
    try stdout.print("subtraction:    {d:0.2}\n", .{@intToFloat(f64, decSub) / @intToFloat(f64, f64Sub)});
    try stdout.print("multiplication: {d:0.2}\n", .{@intToFloat(f64, decMul) / @intToFloat(f64, f64Mul)});
    try stdout.print("division:       {d:0.2}\n", .{@intToFloat(f64, decDiv) / @intToFloat(f64, f64Div)});
    try stdout.print("sin:            {d:0.2}\n", .{@intToFloat(f64, decSin) / @intToFloat(f64, f64Sin)});
    try stdout.print("asin:           {d:0.2}\n", .{@intToFloat(f64, decAsin) / @intToFloat(f64, f64Asin)});
}

fn avg_runs(comptime T: type, comptime n: usize, op: fn (T, T) T, v: T) !u64 {
    const stdout = std.io.getStdOut().writer();

    const repeats = 1000;
    var runs = [_]u64{0} ** repeats;

    var i: usize = 0;
    while (i < repeats) : (i += 1) {
        runs[i] = run(T, n, op, v);
    }

    std.sort.sort(u64, &runs, {}, comptime std.sort.asc(u64));

    const median = runs[runs.len / 2];
    const highest = runs[runs.len - 1];
    const lowest = runs[0];

    try stdout.print("{}ns (lowest: {}ns, highest: {}ns)\n", .{ median, lowest, highest });
    return median;
}

fn run(comptime T: type, comptime n: usize, op: fn (T, T) T, v: T) u64 {
    var a = v;
    timer.reset();

    // Split into outer and inner loop to avoid breaking comptime.
    comptime var outer = n / 500;
    comptime var inner = std.math.min(n, 500);
    var i: usize = 0;
    while (i < outer) : (i += 1) {
        comptime var j = 0;
        inline while (j < inner) : (j += 1) {
            a = op(a, v);

            // Clobber a to avoid optimizations and removal of dead code.
            asm volatile (""
                :
                : [a] "r,m" (&a),
                : "memory"
            );
        }
    }
    comptime var rem = n % 500;
    i = 0;
    inline while (i < rem) : (i += 1) {
        a = op(a, v);

        // Clobber a to avoid optimizations and removal of dead code.
        asm volatile (""
            :
            : [a] "r,m" (&a),
            : "memory"
        );
    }

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
fn asinF64(x: f64, _: f64) f64 {
    return std.math.asin(x);
}

fn sinDec(x: RocDec, _: RocDec) RocDec {
    return x.sin();
}
fn asinDec(x: RocDec, _: RocDec) RocDec {
    return x.asin();
}
