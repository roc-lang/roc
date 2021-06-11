const std = @import("std");
const str = @import("str.zig");

const math = std.math;
const RocStr = str.RocStr;

pub const RocDec = struct {
    num: i128,

    pub const decimal_places: comptime u32 = 18;
    pub const whole_number_places: comptime u32 = 21;
    const max_digits: comptime u32 = decimal_places + whole_number_places;

    pub const min: comptime RocDec = .{ .num = math.minInt(i128) };
    pub const max: comptime RocDec = .{ .num = math.maxInt(i128) };

    pub const one_point_zero_i128: comptime i128 = comptime math.pow(i128, 10, RocDec.decimal_places);
    pub const one_point_zero: comptime RocDec = .{ .num = one_point_zero_i128 };

    pub fn fromU64(num: u64) RocDec {
        return .{ .num = num * one_point_zero_i128 };
    }

    pub fn fromStr(roc_str: RocStr) ?RocDec {
        if (roc_str.isEmpty()) {
            return null;
        }

        const length = roc_str.len();

        const roc_str_slice = roc_str.asSlice();

        var is_negative: bool = roc_str_slice[0] == '-';
        var initial_index: usize = if (is_negative) 1 else 0;

        var point_index: ?usize = null;
        var index: usize = initial_index;
        while (index < length) {
            var byte: u8 = roc_str_slice[index];
            if (byte == '.' and point_index == null) {
                point_index = index;
                index += 1;
                continue;
            }

            if (!isDigit(byte)) {
                return null;
            }
            index += 1;
        }

        var before_str_length = length;
        var after_val_i128: ?i128 = null;
        if (point_index) |pi| {
            before_str_length = pi;

            var after_str_len = (length - 1) - pi;
            if (after_str_len > decimal_places) {
                std.debug.panic("TODO runtime exception for too many decimal places!", .{});
            }
            var diff_decimal_places = decimal_places - after_str_len;

            var after_str = roc_str_slice[pi + 1 .. length];
            var after_u64 = std.fmt.parseUnsigned(u64, after_str, 10) catch null;
            after_val_i128 = if (after_u64) |f| @intCast(i128, f) * math.pow(i128, 10, diff_decimal_places) else null;
        }

        var before_str = roc_str_slice[initial_index..before_str_length];
        var before_val_not_adjusted = std.fmt.parseUnsigned(i128, before_str, 10) catch null;

        var before_val_i128: ?i128 = null;
        if (before_val_not_adjusted) |before| {
            var result: i128 = undefined;
            var overflowed = @mulWithOverflow(i128, before, one_point_zero_i128, &result);
            if (overflowed) {
                std.debug.panic("TODO runtime exception for overflow!", .{});
            }
            before_val_i128 = result;
        }

        var dec: ?RocDec = null;
        if (before_val_i128) |before| {
            if (after_val_i128) |after| {
                var result: i128 = undefined;
                var overflowed = @addWithOverflow(i128, before, after, &result);
                if (overflowed) {
                    std.debug.panic("TODO runtime exception for overflow!", .{});
                }
                dec = .{ .num = result };
            } else {
                dec = .{ .num = before };
            }
        } else if (after_val_i128) |after| {
            dec = .{ .num = after };
        }

        if (dec) |d| {
            if (is_negative) {
                dec = d.negate();
            }
        }

        return dec;
    }

    // TODO: Replace this with https://github.com/rtfeldman/roc/pull/1365/files#r643580738
    fn isDigit(c: u8) bool {
        return switch (c) {
            '0'...'9' => true,
            else => false,
        };
    }

    pub fn toStr(self: RocDec) ?RocStr {
        // Check if this Dec is negative, and if so convert to positive
        // We will handle adding the '-' later
        const is_negative = self.num < 0;
        const num = if (is_negative) std.math.negate(self.num) catch {
            std.debug.panic("TODO runtime exception failing to negate", .{});
        } else self.num;

        // Format the backing i128 into an array of digits (u8s)
        var digit_bytes: [max_digits + 1]u8 = undefined;
        var num_digits_formatted = std.fmt.formatIntBuf(digit_bytes[0..], num, 10, false, .{});

        // If self < 1, then pad digit_bytes with '0' to be at least 18 digits
        if (num_digits_formatted < decimal_places) {
            var diff = decimal_places - num_digits_formatted;
            var padded_digit_bytes: [max_digits + 1]u8 = undefined;
            var index: usize = 0;

            while (index < decimal_places) {
                if (index < diff) {
                    padded_digit_bytes[index] = '0';
                } else {
                    padded_digit_bytes[index] = digit_bytes[index - diff];
                }
                index += 1;
            }
            num_digits_formatted = num_digits_formatted + diff;
            digit_bytes = padded_digit_bytes;
        }

        // Get the slice of the part before the decimal
        // If this is empty, then hardcode a '0'
        var before_digits_num_raw: usize = undefined;
        var before_digits_slice: []const u8 = undefined;
        if (num_digits_formatted > decimal_places) {
            before_digits_num_raw = num_digits_formatted - decimal_places;
            before_digits_slice = digit_bytes[0..before_digits_num_raw];
        } else {
            before_digits_num_raw = 0;
            before_digits_slice = "0";
        }

        // Figure out the index where the trailing zeros start
        var index = decimal_places - 1;
        var trim_index: ?usize = null;
        var is_consecutive_zero = true;
        while (index != 0) {
            var digit = digit_bytes[before_digits_num_raw + index];
            // 48 => '0', 170 => ''
            if ((digit == 48 or digit == 170) and is_consecutive_zero) {
                trim_index = index;
            } else {
                is_consecutive_zero = false;
            }
            index -= 1;
        }

        // Get the slice of the part afterthe decimal
        var after_digits_slice: []const u8 = undefined;
        after_digits_slice = digit_bytes[before_digits_num_raw..(before_digits_num_raw + if (trim_index) |i| i else decimal_places)];

        // Make the RocStr

        var sign_len: usize = if (is_negative) 1 else 0;
        var dot_len: usize = 1;
        var str_len: usize = sign_len + before_digits_slice.len + dot_len + after_digits_slice.len;

        // TODO: Ideally we'd use [str_len]u8 here, but Zig gives an error if we do that.
        // [max_digits + 2]u8 here to account for '.' and '-', aka the max possible length of the string
        var str_bytes: [max_digits + 2]u8 = undefined;

        // Join the whole number slice & the decimal slice together
        // The format template arg in bufPrint is `comptime`, so we have to repeate the whole statement in each branch
        if (is_negative) {
            _ = std.fmt.bufPrint(str_bytes[0 .. str_len + 1], "-{s}.{s}", .{ before_digits_slice, after_digits_slice }) catch {
                std.debug.panic("TODO runtime exception failing to print slices", .{});
            };
        } else {
            _ = std.fmt.bufPrint(str_bytes[0 .. str_len + 1], "{s}.{s}", .{ before_digits_slice, after_digits_slice }) catch {
                std.debug.panic("TODO runtime exception failing to print slices", .{});
            };
        }

        return RocStr.init(&str_bytes, str_len);
    }

    pub fn negate(self: RocDec) ?RocDec {
        var negated = math.negate(self.num) catch null;
        return if (negated) |n| .{ .num = n } else null;
    }

    pub fn add(self: RocDec, other: RocDec) RocDec {
        var answer: i128 = undefined;
        const overflowed = @addWithOverflow(i128, self.num, other.num, &answer);

        if (!overflowed) {
            return RocDec{ .num = answer };
        } else {
            std.debug.panic("TODO runtime exception for overflow!", .{});
        }
    }

    pub fn mul(self: RocDec, other: RocDec) RocDec {
        const self_i128 = self.num;
        const other_i128 = other.num;
        // const answer = 0; //self_i256 * other_i256;

        const is_answer_negative = (self_i128 < 0) != (other_i128 < 0);

        const self_u128 = @intCast(u128, math.absInt(self_i128) catch {
            if (other_i128 == 0) {
                return .{ .num = 0 };
            } else if (other_i128 == RocDec.one_point_zero.num) {
                return self;
            } else {
                std.debug.panic("TODO runtime exception for overflow!", .{});
            }
        });

        const other_u128 = @intCast(u128, math.absInt(other_i128) catch {
            if (self_i128 == 0) {
                return .{ .num = 0 };
            } else if (self_i128 == RocDec.one_point_zero.num) {
                return other;
            } else {
                std.debug.panic("TODO runtime exception for overflow!", .{});
            }
        });

        const unsigned_answer: i128 = mul_and_decimalize(self_u128, other_u128);

        if (is_answer_negative) {
            return .{ .num = -unsigned_answer };
        } else {
            return .{ .num = unsigned_answer };
        }
    }
};

const U256 = struct {
    hi: u128,
    lo: u128,
};

fn mul_and_decimalize(a: u128, b: u128) i128 {
    const answer_u256 = mul_u128(a, b);

    var lhs_hi = answer_u256.hi;
    var lhs_lo = answer_u256.lo;

    // Divide - or just add 1, multiply by floor(2^315/10^18), then right shift 315 times.
    // floor(2^315/10^18) is 66749594872528440074844428317798503581334516323645399060845050244444366430645

    // Add 1.
    // This can't overflow because the initial numbers are only 127bit due to removing the sign bit.
    var overflowed = @addWithOverflow(u128, lhs_lo, 1, &lhs_lo);
    lhs_hi = blk: {
        if (overflowed) {
            break :blk lhs_hi + 1;
        } else {
            break :blk lhs_hi + 0;
        }
    };

    // This needs to do multiplication in a way that expands,
    // since we throw away 315 bits we care only about the higher end, not lower.
    // So like need to do high low mult with 2 U256's and then bitshift.
    // I bet this has a lot of room for multiplication optimization.
    const rhs_hi: u128 = 0x9392ee8e921d5d073aff322e62439fcf;
    const rhs_lo: u128 = 0x32d7f344649470f90cac0c573bf9e1b5;

    const ea = mul_u128(lhs_lo, rhs_lo);
    const gf = mul_u128(lhs_hi, rhs_lo);
    const jh = mul_u128(lhs_lo, rhs_hi);
    const lk = mul_u128(lhs_hi, rhs_hi);

    const e = ea.hi;
    const _a = ea.lo;

    const g = gf.hi;
    const f = gf.lo;

    const j = jh.hi;
    const h = jh.lo;

    const l = lk.hi;
    const k = lk.lo;

    // b = e + f + h
    var e_plus_f: u128 = undefined;
    overflowed = @addWithOverflow(u128, e, f, &e_plus_f);
    var b_carry1: u128 = undefined;
    if (overflowed) {
        b_carry1 = 1;
    } else {
        b_carry1 = 0;
    }

    var idk: u128 = undefined;
    overflowed = @addWithOverflow(u128, e_plus_f, h, &idk);
    var b_carry2: u128 = undefined;
    if (overflowed) {
        b_carry2 = 1;
    } else {
        b_carry2 = 0;
    }

    // c = carry + g + j + k // it doesn't say +k but I think it should be?
    var g_plus_j: u128 = undefined;
    overflowed = @addWithOverflow(u128, g, j, &g_plus_j);
    var c_carry1: u128 = undefined;
    if (overflowed) {
        c_carry1 = 1;
    } else {
        c_carry1 = 0;
    }

    var g_plus_j_plus_k: u128 = undefined;
    overflowed = @addWithOverflow(u128, g_plus_j, k, &g_plus_j_plus_k);
    var c_carry2: u128 = undefined;
    if (overflowed) {
        c_carry2 = 1;
    } else {
        c_carry2 = 0;
    }

    var c_without_bcarry2: u128 = undefined;
    overflowed = @addWithOverflow(u128, g_plus_j_plus_k, b_carry1, &c_without_bcarry2);
    var c_carry3: u128 = undefined;
    if (overflowed) {
        c_carry3 = 1;
    } else {
        c_carry3 = 0;
    }

    var c: u128 = undefined;
    overflowed = @addWithOverflow(u128, c_without_bcarry2, b_carry2, &c);
    var c_carry4: u128 = undefined;
    if (overflowed) {
        c_carry4 = 1;
    } else {
        c_carry4 = 0;
    }

    // d = carry + l
    var d: u128 = undefined;
    overflowed = @addWithOverflow(u128, l, c_carry1, &d);
    overflowed = overflowed or @addWithOverflow(u128, d, c_carry2, &d);
    overflowed = overflowed or @addWithOverflow(u128, d, c_carry3, &d);
    overflowed = overflowed or @addWithOverflow(u128, d, c_carry4, &d);

    if (overflowed) {
        std.debug.panic("TODO runtime exception for overflow!", .{});
    }

    // Final 512bit value is d, c, b, a
    // need to left shift 321 times
    // 315 - 256 is 59. So left shift d, c 59 times.
    return @intCast(i128, c >> 59 | (d << (128 - 59)));
}

fn mul_u128(a: u128, b: u128) U256 {
    var hi: u128 = undefined;
    var lo: u128 = undefined;

    const bits_in_dword_2: u32 = 64;
    const lower_mask: u128 = math.maxInt(u128) >> bits_in_dword_2;

    lo = (a & lower_mask) * (b & lower_mask);

    var t = lo >> bits_in_dword_2;

    lo &= lower_mask;

    t += (a >> bits_in_dword_2) * (b & lower_mask);

    lo += (t & lower_mask) << bits_in_dword_2;

    hi = t >> bits_in_dword_2;

    t = lo >> bits_in_dword_2;

    lo &= lower_mask;

    t += (b >> bits_in_dword_2) * (a & lower_mask);

    lo += (t & lower_mask) << bits_in_dword_2;

    hi += t >> bits_in_dword_2;

    hi += (a >> bits_in_dword_2) * (b >> bits_in_dword_2);

    return .{ .hi = hi, .lo = lo };
}

const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectEqualSlices = testing.expectEqualSlices;
const expect = testing.expect;

test "fromU64" {
    var dec = RocDec.fromU64(25);

    try expectEqual(RocDec{ .num = 25000000000000000000 }, dec);
}

test "fromStr: empty" {
    var roc_str = RocStr.init("", 0);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(dec, null);
}

test "fromStr: 0" {
    var roc_str = RocStr.init("0", 1);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = 0 }, dec.?);
}

test "fromStr: 1" {
    var roc_str = RocStr.init("1", 1);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec.one_point_zero, dec.?);
}

test "fromStr: 123.45" {
    var roc_str = RocStr.init("123.45", 6);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = 123450000000000000000 }, dec.?);
}

test "fromStr: .45" {
    var roc_str = RocStr.init(".45", 3);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = 450000000000000000 }, dec.?);
}

test "fromStr: 0.45" {
    var roc_str = RocStr.init("0.45", 4);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = 450000000000000000 }, dec.?);
}

test "fromStr: 123" {
    var roc_str = RocStr.init("123", 3);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = 123000000000000000000 }, dec.?);
}

test "fromStr: -.45" {
    var roc_str = RocStr.init("-.45", 4);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = -450000000000000000 }, dec.?);
}

test "fromStr: -0.45" {
    var roc_str = RocStr.init("-0.45", 5);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = -450000000000000000 }, dec.?);
}

test "fromStr: -123" {
    var roc_str = RocStr.init("-123", 4);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = -123000000000000000000 }, dec.?);
}

test "fromStr: -123.45" {
    var roc_str = RocStr.init("-123.45", 7);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(RocDec{ .num = -123450000000000000000 }, dec.?);
}

test "fromStr: abc" {
    var roc_str = RocStr.init("abc", 3);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(dec, null);
}

test "fromStr: 123.abc" {
    var roc_str = RocStr.init("123.abc", 7);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(dec, null);
}

test "fromStr: abc.123" {
    var roc_str = RocStr.init("abc.123", 7);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(dec, null);
}

test "fromStr: .123.1" {
    var roc_str = RocStr.init(".123.1", 6);
    var dec = RocDec.fromStr(roc_str);

    try expectEqual(dec, null);
}

test "toStr: 123.45" {
    var dec: RocDec = .{ .num = 123450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "123.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: -123.45" {
    var dec: RocDec = .{ .num = -123450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-123.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 123.0" {
    var dec: RocDec = .{ .num = 123000000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "123.0"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: -123.0" {
    var dec: RocDec = .{ .num = -123000000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-123.0"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 0.45" {
    var dec: RocDec = .{ .num = 450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "0.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: -0.45" {
    var dec: RocDec = .{ .num = -450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-0.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 0.00045" {
    var dec: RocDec = .{ .num = 000450000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "0.00045"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: -0.00045" {
    var dec: RocDec = .{ .num = -000450000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-0.00045"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: -111.123456789" {
    var dec: RocDec = .{ .num = -111123456789000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-111.123456789"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 123.1111111" {
    var dec: RocDec = .{ .num = 123111111100000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "123.1111111"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 123.1111111111111 (big str)" {
    var dec: RocDec = .{ .num = 123111111111111000000 };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.?.deinit();
    defer res_roc_str.?.deinit();

    const res_slice: []const u8 = "123.111111111111"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 123.111111111111444444 (max number of decimal places)" {
    var dec: RocDec = .{ .num = 123111111111111444444 };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.?.deinit();
    defer res_roc_str.?.deinit();

    const res_slice: []const u8 = "123.111111111111444444"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 12345678912345678912.111111111111111111 (max number of digits)" {
    var dec: RocDec = .{ .num = 12345678912345678912111111111111111111 };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.?.deinit();
    defer res_roc_str.?.deinit();

    const res_slice: []const u8 = "12345678912345678912.111111111111111111"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "toStr: 0" {
    var dec: RocDec = .{ .num = 0 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "0.0"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.?.asSlice());
}

test "add: 0" {
    var dec: RocDec = .{ .num = 0 };

    try expectEqual(RocDec{ .num = 0 }, dec.add(.{ .num = 0 }));
}

test "add: 1" {
    var dec: RocDec = .{ .num = 0 };

    try expectEqual(RocDec{ .num = 1 }, dec.add(.{ .num = 1 }));
}

test "mul: by 0" {
    var dec: RocDec = .{ .num = 0 };

    try expectEqual(RocDec{ .num = 0 }, dec.mul(.{ .num = 0 }));
}

test "mul: by 1" {
    var dec: RocDec = RocDec.fromU64(15);

    try expectEqual(RocDec.fromU64(15), dec.mul(RocDec.fromU64(1)));
}

test "mul: by 2" {
    var dec: RocDec = RocDec.fromU64(15);

    try expectEqual(RocDec.fromU64(30), dec.mul(RocDec.fromU64(2)));
}
