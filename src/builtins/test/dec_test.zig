//! TODO

const std = @import("std");
const builtins = @import("builtins");

const TestEnv = @import("../utils.zig").TestEnv;
const RocDec = builtins.dec.RocDec;
const RocStr = @import("../str.zig").RocStr;

test "fromU64" {
    const dec = RocDec.fromU64(25);

    try std.testing.expectEqual(RocDec{ .num = 25000000000000000000 }, dec);
}

test "fromF64" {
    const dec = RocDec.fromF64(25.5);
    try std.testing.expectEqual(RocDec{ .num = 25500000000000000000 }, dec.?);
}

test "fromF64 overflow" {
    const dec = RocDec.fromF64(1e308);
    try std.testing.expectEqual(dec, null);
}

test "fromStr: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("", 0, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(dec, null);
}

test "fromStr: 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0", 1, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.?);
}

test "fromStr: 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("1", 1, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec.one_point_zero, dec.?);
}

test "fromStr: 123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("123.45", 6, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = 123450000000000000000 }, dec.?);
}

test "fromStr: .45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init(".45", 3, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = 450000000000000000 }, dec.?);
}

test "fromStr: 0.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0.45", 4, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = 450000000000000000 }, dec.?);
}

test "fromStr: 123" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("123", 3, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = 123000000000000000000 }, dec.?);
}

test "fromStr: -.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-.45", 4, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = -450000000000000000 }, dec.?);
}

test "fromStr: -0.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-0.45", 5, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = -450000000000000000 }, dec.?);
}

test "fromStr: -123" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-123", 4, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = -123000000000000000000 }, dec.?);
}

test "fromStr: -123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-123.45", 7, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(RocDec{ .num = -123450000000000000000 }, dec.?);
}

test "fromStr: abc" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("abc", 3, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(dec, null);
}

test "fromStr: 123.abc" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("123.abc", 7, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(dec, null);
}

test "fromStr: abc.123" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("abc.123", 7, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(dec, null);
}

test "fromStr: .123.1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init(".123.1", 6, test_env.getOps());
    const dec = RocDec.fromStr(roc_str);

    try std.testing.expectEqual(dec, null);
}

test "to_str: 100.00" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 100000000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "100.0"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 123450000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "123.45"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: -123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = -123450000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "-123.45"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 123.0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 123000000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "123.0"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: -123.0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = -123000000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "-123.0"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 0.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 450000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "0.45"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: -0.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = -450000000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "-0.45"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 0.00045" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 450000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "0.00045"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: -0.00045" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = -450000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "-0.00045"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: -111.123456" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = -111123456000000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "-111.123456"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 123.1111111" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 123111111100000000000 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "123.1111111"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 123.1111111111111 (big str)" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 123111111111111000000 };
    var res_roc_str = dec.to_str(test_env.getOps());
    errdefer res_roc_str.decref(test_env.getOps());
    defer res_roc_str.decref(test_env.getOps());

    const res_slice: []const u8 = "123.111111111111"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 123.111111111111444444 (max number of decimal places)" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 123111111111111444444 };
    var res_roc_str = dec.to_str(test_env.getOps());
    errdefer res_roc_str.decref(test_env.getOps());
    defer res_roc_str.decref(test_env.getOps());

    const res_slice: []const u8 = "123.111111111111444444"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 12345678912345678912.111111111111111111 (max number of digits)" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 12345678912345678912111111111111111111 };
    var res_roc_str = dec.to_str(test_env.getOps());
    errdefer res_roc_str.decref(test_env.getOps());
    defer res_roc_str.decref(test_env.getOps());

    const res_slice: []const u8 = "12345678912345678912.111111111111111111"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: std.math.maxInt" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = std.math.maxInt(i128) };
    var res_roc_str = dec.to_str(test_env.getOps());
    errdefer res_roc_str.decref(test_env.getOps());
    defer res_roc_str.decref(test_env.getOps());

    const res_slice: []const u8 = "170141183460469231731.687303715884105727"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: std.math.minInt" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = std.math.minInt(i128) };
    var res_roc_str = dec.to_str(test_env.getOps());
    errdefer res_roc_str.decref(test_env.getOps());
    defer res_roc_str.decref(test_env.getOps());

    const res_slice: []const u8 = "-170141183460469231731.687303715884105728"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "to_str: 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 0 };
    var res_roc_str = dec.to_str(test_env.getOps());

    const res_slice: []const u8 = "0.0"[0..];
    try std.testing.expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "add: 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 0 };

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.add(.{ .num = 0 }, test_env.getOps()));
}

test "add: 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 0 };

    try std.testing.expectEqual(RocDec{ .num = 1 }, dec.add(.{ .num = 1 }, test_env.getOps()));
}

test "sub: 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 1 };

    try std.testing.expectEqual(RocDec{ .num = 1 }, dec.sub(.{ .num = 0 }, test_env.getOps()));
}

test "sub: 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = 1 };

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.sub(.{ .num = 1 }, test_env.getOps()));
}

test "mul: by 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = .{ .num = -2 };

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.mul(.{ .num = 0 }, test_env.getOps()));
}

test "mul: by 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = RocDec.fromU64(15);

    try std.testing.expectEqual(RocDec.fromU64(15), dec.mul(RocDec.fromU64(1), test_env.getOps()));
}

test "mul: by 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = RocDec.fromU64(15);

    try std.testing.expectEqual(RocDec.fromU64(30), dec.mul(RocDec.fromU64(2), test_env.getOps()));
}

test "div: 0 / 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = RocDec.fromU64(0);

    try std.testing.expectEqual(RocDec.fromU64(0), dec.div(RocDec.fromU64(2), test_env.getOps()));
}

test "div: 2 / 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = RocDec.fromU64(2);

    try std.testing.expectEqual(RocDec.fromU64(1), dec.div(RocDec.fromU64(2), test_env.getOps()));
}

test "div: 20 / 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = RocDec.fromU64(20);

    try std.testing.expectEqual(RocDec.fromU64(10), dec.div(RocDec.fromU64(2), test_env.getOps()));
}

test "div: 8 / 5" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var dec: RocDec = RocDec.fromU64(8);
    const res: RocDec = RocDec.fromStr(RocStr.init("1.6", 3, test_env.getOps())).?;
    try std.testing.expectEqual(res, dec.div(RocDec.fromU64(5), test_env.getOps()));
}

test "div: 10 / 3" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var numerator: RocDec = RocDec.fromU64(10);
    const denom: RocDec = RocDec.fromU64(3);

    var roc_str = RocStr.init("3.333333333333333333", 20, test_env.getOps());
    errdefer roc_str.decref(test_env.getOps());
    defer roc_str.decref(test_env.getOps());

    const res: RocDec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(res, numerator.div(denom, test_env.getOps()));
}

test "div: 341 / 341" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var number1: RocDec = RocDec.fromU64(341);
    const number2: RocDec = RocDec.fromU64(341);
    try std.testing.expectEqual(RocDec.fromU64(1), number1.div(number2, test_env.getOps()));
}

test "div: 342 / 343" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var number1: RocDec = RocDec.fromU64(342);
    const number2: RocDec = RocDec.fromU64(343);
    const roc_str = RocStr.init("0.997084548104956268", 20, test_env.getOps());
    try std.testing.expectEqual(RocDec.fromStr(roc_str), number1.div(number2, test_env.getOps()));
}

test "div: 680 / 340" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var number1: RocDec = RocDec.fromU64(680);
    const number2: RocDec = RocDec.fromU64(340);
    try std.testing.expectEqual(RocDec.fromU64(2), number1.div(number2, test_env.getOps()));
}

test "div: 500 / 1000" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const number1: RocDec = RocDec.fromU64(500);
    const number2: RocDec = RocDec.fromU64(1000);
    const roc_str = RocStr.init("0.5", 3, test_env.getOps());
    try std.testing.expectEqual(RocDec.fromStr(roc_str), number1.div(number2, test_env.getOps()));
}

test "log: 1" {
    try std.testing.expectEqual(RocDec.fromU64(0), RocDec.log(RocDec.fromU64(1)));
}

test "fract: 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0", 1, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.fract());
}

test "fract: 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("1", 1, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.fract());
}

test "fract: 123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("123.45", 6, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 450000000000000000 }, dec.fract());
}

test "fract: -123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-123.45", 7, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = -450000000000000000 }, dec.fract());
}

test "fract: .45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init(".45", 3, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 450000000000000000 }, dec.fract());
}

test "fract: -0.00045" {
    const dec: RocDec = .{ .num = -450000000000000 };
    const res = dec.fract();

    try std.testing.expectEqual(dec.num, res.num);
}

test "trunc: 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0", 1, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.trunc(test_env.getOps()));
}

test "trunc: 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("1", 1, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec.one_point_zero, dec.trunc(test_env.getOps()));
}

test "trunc: 123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("123.45", 6, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 123000000000000000000 }, dec.trunc(test_env.getOps()));
}

test "trunc: -123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-123.45", 7, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = -123000000000000000000 }, dec.trunc(test_env.getOps()));
}

test "trunc: .45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init(".45", 3, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 0 }, dec.trunc(test_env.getOps()));
}

test "trunc: -0.00045" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const dec: RocDec = .{ .num = -450000000000000 };
    const res = dec.trunc(test_env.getOps());

    try std.testing.expectEqual(RocDec{ .num = 0 }, res);
}

test "round: 123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("123.45", 6, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = 123000000000000000000 }, dec.round(test_env.getOps()));
}

test "round: -123.45" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-123.45", 7, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = -123000000000000000000 }, dec.round(test_env.getOps()));
}

test "round: 0.5" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0.5", 3, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec.one_point_zero, dec.round(test_env.getOps()));
}

test "round: -0.5" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("-0.5", 4, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec{ .num = -1000000000000000000 }, dec.round(test_env.getOps()));
}

test "powInt: 3.1 ^ 0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("3.1", 3, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(RocDec.one_point_zero, dec.powInt(0, test_env.getOps()));
}

test "powInt: 3.1 ^ 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("3.1", 3, test_env.getOps());
    var dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(dec, dec.powInt(1, test_env.getOps()));
}

test "powInt: 2 ^ 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("4", 1, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(dec, RocDec.two_point_zero.powInt(2, test_env.getOps()));
}

test "powInt: 0.5 ^ 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0.25", 4, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(dec, RocDec.zero_point_five.powInt(2, test_env.getOps()));
}

test "pow: 0.5 ^ 2.0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0.25", 4, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(dec, RocDec.zero_point_five.pow(RocDec.two_point_zero, test_env.getOps()));
}

test "sqrt: 1.0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("1.0", 3, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(dec, RocDec.sqrt(RocDec.one_point_zero, test_env.getOps()));
}

test "sqrt: 0.0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("0.0", 3, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    try std.testing.expectEqual(dec, dec.sqrt(test_env.getOps()));
}

test "sqrt: 9.0" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("9.0", 3, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    const roc_str_expected = RocStr.init("3.0", 3, test_env.getOps());
    const expected = RocDec.fromStr(roc_str_expected).?;

    try std.testing.expectEqual(expected, dec.sqrt(test_env.getOps()));
}

test "sqrt: 1.44" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const roc_str = RocStr.init("1.44", 4, test_env.getOps());
    const dec = RocDec.fromStr(roc_str).?;

    const roc_str_expected = RocStr.init("1.2", 3, test_env.getOps());
    const expected = RocDec.fromStr(roc_str_expected).?;

    try std.testing.expectEqual(expected, dec.sqrt(test_env.getOps()));
}
