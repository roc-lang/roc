const std = @import("std");
const time = std.time;
const Timer = time.Timer;

const RocStr = @import("../src/str.zig").RocStr;
const RocDec = @import("../src/dec.zig").RocDec;

var timer: Timer = undefined;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    timer = try Timer.start();

    const add7_ns = add7();
    try stdout.print("7 additions took {}ns\n", .{add7_ns});

    const sub7_ns = sub7();
    try stdout.print("7 subtractions took {}ns\n", .{sub7_ns});

    const mul7_ns = mul7();
    try stdout.print("7 multiplications took {}ns\n", .{mul7_ns});

    const div7_ns = div7();
    try stdout.print("7 divisions took {}ns\n", .{div7_ns});
}

fn add7() u64 {
    var str1 = RocStr.init("1.2", 3);
    const dec1 = RocDec.fromStr(str1).?;

    var str2 = RocStr.init("3.4", 3);
    const dec2 = RocDec.fromStr(str2).?;

    timer.reset();

    var a = dec1.add(dec2);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);
    a = a.add(dec1);

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
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);
    a = a.sub(dec1);

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
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);
    a = a.mul(dec1);

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
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);
    a = a.div(dec1);

    return timer.read();
}
