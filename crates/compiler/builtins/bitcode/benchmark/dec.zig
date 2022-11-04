const std = @import("std");
const time = std.time;
const Timer = time.Timer;

const RocStr = @import("../src/str.zig").RocStr;
const RocDec = @import("../src/dec.zig").RocDec;

var timer: Timer = undefined;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    timer = try Timer.start();

    try stdout.print("7 additions took ", .{});
    try avg_runs(add7);

    try stdout.print("7 subtractions took ", .{});
    try avg_runs(sub7);

    try stdout.print("7 multiplications took ", .{});
    try avg_runs(mul7);

    try stdout.print("7 divisions took ", .{});
    try avg_runs(div7);
}

fn avg_runs(func: fn() u64) !void {
    const stdout = std.io.getStdOut().writer();
    var first_run = func();
    var lowest = first_run;
    var highest = first_run;
    var sum = first_run;

    // 31 runs
    var runs = [_]u64{ first_run, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    var next_run: usize = 1; // we already did first_run

    while (next_run < runs.len) {
        const run = func();

        lowest = std.math.min(lowest, run);
        highest = std.math.max(highest, run);

        runs[next_run] = run;

        next_run += 1;
    }

    std.sort.sort(u64, &runs, {}, comptime std.sort.asc(u64));

    const median = runs[runs.len / 2];

    try stdout.print("{}ns (lowest: {}ns, highest: {}ns)\n", .{median, lowest, highest});
}

fn add7() u64 {
    var str1 = RocStr.init("1.2", 3);
    const dec1 = RocDec.fromStr(str1).?;

    var str2 = RocStr.init("3.4", 3);
    const dec2 = RocDec.fromStr(str2).?;

    timer.reset();

    var a = dec1.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);
    a = a.add(dec1);
    a = a.add(dec2);

    return timer.read();
}

fn sub7() u64 {
    var str1 = RocStr.init("1.2", 3);
    const dec1 = RocDec.fromStr(str1).?;

    var str2 = RocStr.init("3.4", 3);
    const dec2 = RocDec.fromStr(str2).?;

    timer.reset();

    var a = dec1.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);
    a = a.sub(dec1);
    a = a.sub(dec2);

    return timer.read();
}

fn mul7() u64 {
    var str1 = RocStr.init("1.2", 3);
    const dec1 = RocDec.fromStr(str1).?;

    var str2 = RocStr.init("3.4", 3);
    const dec2 = RocDec.fromStr(str2).?;

    timer.reset();

    var a = dec1.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);
    a = a.mul(dec1);
    a = a.mul(dec2);

    return timer.read();
}

fn div7() u64 {
    var str1 = RocStr.init("1.2", 3);
    const dec1 = RocDec.fromStr(str1).?;

    var str2 = RocStr.init("3.4", 3);
    const dec2 = RocDec.fromStr(str2).?;

    timer.reset();

    var a = dec1.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);
    a = a.div(dec1);
    a = a.div(dec2);

    return timer.read();
}
