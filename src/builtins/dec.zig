//! Builtin decimal operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's Dec type, a fixed-point
//! decimal number with 18 decimal places. It includes arithmetic operations,
//! parsing, formatting, and conversions for precise decimal calculations
//! without floating-point precision issues.
const std = @import("std");

const U256 = @import("num.zig").U256;
const TestEnv = @import("utils.zig").TestEnv;
const WithOverflow = @import("utils.zig").WithOverflow;
const NumParseResult = @import("num.zig").NumParseResult;
const RocOps = @import("host_abi.zig").RocOps;
const RocStr = @import("str.zig").RocStr;
const mul_u128 = @import("num.zig").mul_u128;
const math = std.math;

/// TODO: Document the RocDec struct.
pub const RocDec = extern struct {
    num: i128,

    pub const decimal_places: u5 = 18;
    pub const whole_number_places: u5 = 21;
    const max_digits: u6 = 39;
    const max_str_length: u6 = max_digits + 2; // + 2 here to account for the sign & decimal dot

    pub const min: RocDec = .{ .num = math.minInt(i128) };
    pub const max: RocDec = .{ .num = math.maxInt(i128) };

    pub const one_point_zero_i128: i128 = math.pow(i128, 10, RocDec.decimal_places);
    pub const one_point_zero: RocDec = .{ .num = one_point_zero_i128 };

    pub const two_point_zero: RocDec = RocDec.add(
        RocDec.one_point_zero,
        RocDec.one_point_zero,
        undefined,
    );
    pub const zero_point_five: RocDec = RocDec.div(
        RocDec.one_point_zero,
        RocDec.two_point_zero,
        undefined,
    );

    pub fn fromU64(num: u64) RocDec {
        return .{ .num = num * one_point_zero_i128 };
    }

    pub fn fromF64(num: f64) ?RocDec {
        const result: f64 = num * comptime @as(f64, @floatFromInt(one_point_zero_i128));

        if (result > comptime @as(f64, @floatFromInt(math.maxInt(i128)))) {
            return null;
        }

        if (result < comptime @as(f64, @floatFromInt(math.minInt(i128)))) {
            return null;
        }

        const ret: RocDec = .{ .num = @as(i128, @intFromFloat(result)) };
        return ret;
    }

    pub fn toF64(dec: RocDec) f64 {
        return @as(f64, @floatFromInt(dec.num)) / comptime @as(f64, @floatFromInt(one_point_zero_i128));
    }

    // TODO: If Str.toDec eventually supports more error types, return errors here.
    // For now, just return null which will give the default error.
    pub fn fromStr(roc_str: RocStr) ?RocDec {
        if (roc_str.isEmpty()) {
            return null;
        }

        return @call(.always_inline, RocDec.fromNonemptySlice, .{roc_str.asSlice()});
    }

    // This a separate function because the compiler uses it.
    pub fn fromNonemptySlice(roc_str_slice: []const u8) ?RocDec {
        const length = roc_str_slice.len;
        const is_negative: bool = roc_str_slice[0] == '-';
        const initial_index: usize = @intFromBool(is_negative);

        var point_index: ?usize = null;
        var index: usize = initial_index;
        while (index < length) {
            const byte: u8 = roc_str_slice[index];
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

            const after_str_len = (length - 1) - pi;
            if (after_str_len > decimal_places) {
                // TODO: runtime exception for too many decimal places!
                return null;
            }
            const diff_decimal_places = decimal_places - after_str_len;

            const after_str = roc_str_slice[pi + 1 .. length];
            const after_u64 = std.fmt.parseUnsigned(u64, after_str, 10) catch null;
            after_val_i128 = if (after_u64) |f| @as(i128, @intCast(f)) * math.pow(i128, 10, diff_decimal_places) else null;
        }

        const before_str = roc_str_slice[initial_index..before_str_length];
        const before_val_not_adjusted = std.fmt.parseUnsigned(i128, before_str, 10) catch null;

        var before_val_i128: ?i128 = null;
        if (before_val_not_adjusted) |before| {
            const answer = @mulWithOverflow(before, one_point_zero_i128);
            const result = answer[0];
            const overflowed = answer[1];
            if (overflowed == 1) {
                // TODO: runtime exception for overflow!
                return null;
            }
            before_val_i128 = result;
        }

        const dec: RocDec = blk: {
            if (before_val_i128) |before| {
                if (after_val_i128) |after| {
                    const answer = @addWithOverflow(before, after);
                    const result = answer[0];
                    const overflowed = answer[1];
                    if (overflowed == 1) {
                        // TODO: runtime exception for overflow!
                        return null;
                    }
                    break :blk .{ .num = result };
                } else {
                    break :blk .{ .num = before };
                }
            } else if (after_val_i128) |after| {
                break :blk .{ .num = after };
            } else {
                return null;
            }
        };

        if (is_negative) {
            return dec.negate();
        } else {
            return dec;
        }
    }

    inline fn isDigit(c: u8) bool {
        return (c -% 48) <= 9;
    }

    pub fn to_str(self: RocDec, roc_ops: *RocOps) RocStr {

        // Special case
        if (self.num == 0) {
            return RocStr.init("0.0", 3, roc_ops);
        }

        const num = self.num;
        const is_negative = num < 0;

        // Format the backing i128 into an array of digit (ascii) characters (u8s)
        var digit_bytes_storage: [max_digits + 1]u8 = undefined;
        var num_digits = std.fmt.formatIntBuf(digit_bytes_storage[0..], num, 10, .lower, .{});
        var digit_bytes: [*]u8 = digit_bytes_storage[0..];

        // space where we assemble all the characters that make up the final string
        var str_bytes: [max_str_length]u8 = undefined;
        var position: usize = 0;

        // if negative, the first character is a negating minus
        if (is_negative) {
            str_bytes[position] = '-';
            position += 1;

            // but also, we have one fewer digit than we have characters
            num_digits -= 1;

            // and we drop the minus to make later arithmetic correct
            digit_bytes += 1;
        }

        // Get the slice for before the decimal point
        var before_digits_offset: usize = 0;
        if (num_digits > decimal_places) {
            // we have more digits than fit after the decimal point,
            // so we must have digits before the decimal point
            before_digits_offset = num_digits - decimal_places;

            for (digit_bytes[0..before_digits_offset]) |c| {
                str_bytes[position] = c;
                position += 1;
            }
        } else {
            // otherwise there are no actual digits before the decimal point
            // but we format it with a '0'
            str_bytes[position] = '0';
            position += 1;
        }

        // we've done everything before the decimal point, so now we can put the decimal point in
        str_bytes[position] = '.';
        position += 1;

        const trailing_zeros: u6 = count_trailing_zeros_base10(num);
        if (trailing_zeros >= decimal_places) {
            // add just a single zero if all decimal digits are zero
            str_bytes[position] = '0';
            position += 1;
        } else {
            // Figure out if we need to prepend any zeros to the after decimal point
            // For example, for the number 0.000123 we need to prepend 3 zeros after the decimal point
            const after_zeros_num = if (num_digits < decimal_places) decimal_places - num_digits else 0;

            var i: usize = 0;
            while (i < after_zeros_num) : (i += 1) {
                str_bytes[position] = '0';
                position += 1;
            }

            // otherwise append the decimal digits except the trailing zeros
            for (digit_bytes[before_digits_offset .. num_digits - trailing_zeros]) |c| {
                str_bytes[position] = c;
                position += 1;
            }
        }

        return RocStr.init(&str_bytes, position, roc_ops);
    }

    pub fn toI128(self: RocDec) i128 {
        return self.num;
    }

    pub fn fromI128(num: i128) RocDec {
        return .{ .num = num };
    }

    pub fn eq(self: RocDec, other: RocDec) bool {
        return self.num == other.num;
    }

    pub fn neq(self: RocDec, other: RocDec) bool {
        return self.num != other.num;
    }

    pub fn negate(self: RocDec) ?RocDec {
        const negated = math.negate(self.num) catch null;
        return if (negated) |n| .{ .num = n } else null;
    }

    pub fn abs(self: RocDec) !RocDec {
        const absolute = @abs(self.num);
        if (absolute <= @as(u128, @intCast(std.math.maxInt(i128)))) {
            return RocDec{ .num = @intCast(absolute) };
        }
        return error.OutOfRange;
    }

    pub fn addWithOverflow(self: RocDec, other: RocDec) WithOverflow(RocDec) {
        const answer = @addWithOverflow(self.num, other.num);

        return .{ .value = RocDec{ .num = answer[0] }, .has_overflowed = answer[1] == 1 };
    }

    pub fn add(
        self: RocDec,
        other: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        const answer = RocDec.addWithOverflow(self, other);

        if (answer.has_overflowed) {
            roc_ops.crash("Decimal addition overflowed!");
            unreachable; // The host should handle the crash
        } else {
            return answer.value;
        }
    }

    pub fn addSaturated(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.addWithOverflow(self, other);
        if (answer.has_overflowed) {
            // We can unambiguously tell which way it wrapped, because we have 129 bits including the overflow bit
            if (answer.value.num < 0) {
                return RocDec.max;
            } else {
                return RocDec.min;
            }
        } else {
            return answer.value;
        }
    }

    pub fn subWithOverflow(self: RocDec, other: RocDec) WithOverflow(RocDec) {
        const answer = @subWithOverflow(self.num, other.num);

        return .{ .value = RocDec{ .num = answer[0] }, .has_overflowed = answer[1] == 1 };
    }

    pub fn sub(
        self: RocDec,
        other: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        const answer = RocDec.subWithOverflow(self, other);

        if (answer.has_overflowed) {
            roc_ops.crash("Decimal subtraction overflowed!");
            unreachable; // The host should handle the crash
        } else {
            return answer.value;
        }
    }

    pub fn subSaturated(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.subWithOverflow(self, other);
        if (answer.has_overflowed) {
            if (answer.value.num < 0) {
                return RocDec.max;
            } else {
                return RocDec.min;
            }
        } else {
            return answer.value;
        }
    }

    pub fn mulWithOverflow(self: RocDec, other: RocDec) WithOverflow(RocDec) {
        const self_i128 = self.num;
        const other_i128 = other.num;
        // const answer = 0; //self_i256 * other_i256;

        const is_answer_negative = (self_i128 < 0) != (other_i128 < 0);

        const self_u128 = @abs(self_i128);
        if (self_u128 > @as(u128, @intCast(std.math.maxInt(i128)))) {
            if (other_i128 == 0) {
                return .{ .value = RocDec{ .num = 0 }, .has_overflowed = false };
            } else if (other_i128 == RocDec.one_point_zero.num) {
                return .{ .value = self, .has_overflowed = false };
            } else if (is_answer_negative) {
                return .{ .value = RocDec.min, .has_overflowed = true };
            } else {
                return .{ .value = RocDec.max, .has_overflowed = true };
            }
        }

        const other_u128 = @abs(other_i128);
        if (other_u128 > @as(u128, @intCast(std.math.maxInt(i128)))) {
            if (self_i128 == 0) {
                return .{ .value = RocDec{ .num = 0 }, .has_overflowed = false };
            } else if (self_i128 == RocDec.one_point_zero.num) {
                return .{ .value = other, .has_overflowed = false };
            } else if (is_answer_negative) {
                return .{ .value = RocDec.min, .has_overflowed = true };
            } else {
                return .{ .value = RocDec.max, .has_overflowed = true };
            }
        }

        const unsigned_answer = mul_and_decimalize(self_u128, other_u128);
        if (is_answer_negative) {
            return .{ .value = RocDec{ .num = -unsigned_answer.value }, .has_overflowed = unsigned_answer.has_overflowed };
        } else {
            return .{ .value = RocDec{ .num = unsigned_answer.value }, .has_overflowed = unsigned_answer.has_overflowed };
        }
    }

    pub fn trunc(
        self: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        return RocDec.sub(
            self,
            self.fract(),
            roc_ops,
        );
    }

    pub fn fract(self: RocDec) RocDec {
        const sign = std.math.sign(self.num);
        const digits = @mod(sign * self.num, RocDec.one_point_zero.num);

        return RocDec{ .num = sign * digits };
    }

    // Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
    pub fn round(
        arg1: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        // this rounds towards zero
        const tmp = arg1.trunc(roc_ops);

        const sign = std.math.sign(arg1.num);
        const abs_fract = sign * arg1.fract().num;

        if (abs_fract >= RocDec.zero_point_five.num) {
            return RocDec.add(
                tmp,
                RocDec{ .num = sign * RocDec.one_point_zero.num },
                roc_ops,
            );
        } else {
            return tmp;
        }
    }

    // Returns the largest integer less than or equal to itself
    fn floor(
        arg1: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        const tmp = arg1.trunc(roc_ops);

        if (arg1.num < 0 and arg1.fract().num != 0) {
            return RocDec.sub(tmp, RocDec.one_point_zero, roc_ops);
        } else {
            return tmp;
        }
    }

    // Returns the smallest integer greater than or equal to itself
    fn ceiling(
        arg1: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        const tmp = arg1.trunc(roc_ops);

        if (arg1.num > 0 and arg1.fract().num != 0) {
            return RocDec.add(
                tmp,
                RocDec.one_point_zero,
                roc_ops,
            );
        } else {
            return tmp;
        }
    }

    pub fn powInt(
        base: RocDec,
        exponent: i128,
        roc_ops: *RocOps,
    ) RocDec {
        if (exponent == 0) {
            return RocDec.one_point_zero;
        } else if (exponent > 0) {
            if (@mod(exponent, 2) == 0) {
                const half_power = RocDec.powInt(base, exponent >> 1, roc_ops); // `>> 1` == `/ 2`
                return RocDec.mul(half_power, half_power, roc_ops);
            } else {
                return RocDec.mul(base, RocDec.powInt(base, exponent - 1, roc_ops), roc_ops);
            }
        } else {
            return RocDec.div(RocDec.one_point_zero, RocDec.powInt(base, -exponent, roc_ops), roc_ops);
        }
    }

    pub fn pow(
        base: RocDec,
        exponent: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        if (exponent.trunc(roc_ops).num == exponent.num) {
            return base.powInt(
                @divTrunc(exponent.num, RocDec.one_point_zero_i128),
                roc_ops,
            );
        } else {
            return fromF64(std.math.pow(f64, base.toF64(), exponent.toF64())).?;
        }
    }

    pub fn sqrt(
        self: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        // sqrt(-n) is an error
        if (self.num < 0) {
            roc_ops.crash("Square root by 0!");
        }

        return fromF64(std.math.sqrt(self.toF64())).?;
    }

    pub fn mul(
        self: RocDec,
        other: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        const answer = RocDec.mulWithOverflow(self, other);

        if (answer.has_overflowed) {
            roc_ops.crash("Decimal multiplication overflowed!");
            unreachable; // The host should handle the crash
        } else {
            return answer.value;
        }
    }

    pub fn mulSaturated(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.mulWithOverflow(self, other);
        if (answer.has_overflowed) {
            if (answer.value.num < 0) {
                return RocDec.max;
            } else {
                return RocDec.min;
            }
        } else {
            return answer.value;
        }
    }

    pub fn div(
        self: RocDec,
        other: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        const numerator_i128 = self.num;
        const denominator_i128 = other.num;

        // (n / 0) is an error
        if (denominator_i128 == 0) {
            roc_ops.crash("Decimal division by 0!");
        }

        // (0 / n) is always 0
        if (numerator_i128 == 0) {
            return RocDec{ .num = 0 };
        }

        // If they're both negative, or if neither is negative, the final answer
        // is positive or zero. If one is negative and the denominator isn't, the
        // final answer is negative (or zero, in which case final sign won't matter).
        //
        // It's important that we do this in terms of negatives, because doing
        // it in terms of positives can cause bugs when one is zero.
        const is_answer_negative = (numerator_i128 < 0) != (denominator_i128 < 0);

        // Break the two i128s into two { hi: u64, lo: u64 } tuples, discarding
        // the sign for now.
        //
        // We'll multiply all 4 combinations of these (hi1 x lo1, hi2 x lo2,
        // hi1 x lo2, hi2 x lo1) and add them as appropriate, then apply the
        // appropriate sign at the very end.
        //
        // We do checked_abs because if we had -i128::MAX before, this will overflow.

        const numerator_u128 = @abs(numerator_i128);
        if (numerator_u128 > @as(u128, @intCast(std.math.maxInt(i128)))) {
            // Currently, if you try to do multiplication on i64::MIN, panic
            // unless you're specifically multiplying by 0 or 1.
            //
            // Maybe we could support more cases in the future
            if (denominator_i128 == one_point_zero_i128) {
                return self;
            } else {
                roc_ops.crash("Decimal division overflow in numerator!");
            }
        }

        const denominator_u128 = @abs(denominator_i128);
        if (denominator_u128 > @as(u128, @intCast(std.math.maxInt(i128)))) {
            // Currently, if you try to do multiplication on i64::MIN, panic
            // unless you're specifically multiplying by 0 or 1.
            //
            // Maybe we could support more cases in the future
            if (numerator_i128 == one_point_zero_i128) {
                return other;
            } else {
                roc_ops.crash("Decimal division overflow in denominator!");
            }
        }

        const numerator_u256: U256 = mul_u128(numerator_u128, math.pow(u128, 10, decimal_places));
        const answer = div_u256_by_u128(numerator_u256, denominator_u128);

        var unsigned_answer: i128 = undefined;
        if (answer.hi == 0 and answer.lo <= math.maxInt(i128)) {
            unsigned_answer = @as(i128, @intCast(answer.lo));
        } else {
            roc_ops.crash("Decimal division overflow!");
        }

        return RocDec{ .num = if (is_answer_negative) -unsigned_answer else unsigned_answer };
    }

    fn mod2pi(self: RocDec) RocDec {
        // This is made to be used before calling trig functions that work on the range 0 to 2*pi.
        // It should be reasonable fast (much faster than calling @mod) and much more accurate as well.
        // b is 2*pi as a dec. which is 6.2831853071795864769252867665590057684
        // as dec is times 10^18 so 6283185307179586476.9252867665590057684
        const b0: u64 = 6283185307179586476;
        // Fraction that represents 64 bits of precision past what dec normally supports.
        // 0.9252867665590057684 as binary to 64 places.
        const b1: u64 = 0b1110110011011111100101111111000111001010111000100101011111110111;

        // This is dec/(b0+1), but as a multiplication.
        // So dec * (1/(b0+1)). This is way faster.
        const dec = self.num;
        const tmp = @as(i128, @intCast(mul_u128(@abs(dec), 249757942369376157886101012127821356963).hi >> (190 - 128)));
        const q0 = if (dec < 0) -tmp else tmp;

        const upper = q0 * b0;
        const answer = @mulWithOverflow(q0, b1);
        const lower = answer[0];
        const overflowed = answer[1];
        // TODO: maybe write this out branchlessly.
        // Currently is is probably cmovs, but could be just math?
        const q0_sign: i128 =
            if (q0 > 0) 1 else -1;
        const overflowed_val: i128 = if (overflowed == 1) q0_sign << 64 else 0;
        const full = upper + @as(i128, @intCast(lower >> 64)) + overflowed_val;

        var out = dec - full;
        if (out < 0) {
            out += b0;
        }

        return RocDec{ .num = out };
    }

    pub fn log(self: RocDec) RocDec {
        return fromF64(@log(self.toF64())).?;
    }

    // I believe the output of the trig functions is always in range of Dec.
    // If not, we probably should just make it saturate the Dec.
    // I don't think this should crash or return errors.
    pub fn sin(self: RocDec) RocDec {
        return fromF64(math.sin(self.mod2pi().toF64())).?;
    }

    pub fn cos(self: RocDec) RocDec {
        return fromF64(math.cos(self.mod2pi().toF64())).?;
    }

    pub fn tan(self: RocDec) RocDec {
        return fromF64(math.tan(self.mod2pi().toF64())).?;
    }

    pub fn asin(self: RocDec) RocDec {
        return fromF64(math.asin(self.toF64())).?;
    }

    pub fn acos(self: RocDec) RocDec {
        return fromF64(math.acos(self.toF64())).?;
    }

    pub fn atan(self: RocDec) RocDec {
        return fromF64(math.atan(self.toF64())).?;
    }
};

// A number has `k` trailing zeros if `10^k` divides into it cleanly
inline fn count_trailing_zeros_base10(input: i128) u6 {
    if (input == 0) {
        // this should not happen in practice
        return 0;
    }

    var count: u6 = 0;
    var k: i128 = 1;

    while (true) {
        if (@mod(input, std.math.pow(i128, 10, k)) == 0) {
            count += 1;
            k += 1;
        } else {
            break;
        }
    }

    return count;
}

fn mul_and_decimalize(a: u128, b: u128) WithOverflow(i128) {
    const answer_u256 = mul_u128(a, b);

    var lhs_hi = answer_u256.hi;
    var lhs_lo = answer_u256.lo;

    // Divide - or just add 1, multiply by floor(2^315/10^18), then right shift 315 times.
    // floor(2^315/10^18) is 66749594872528440074844428317798503581334516323645399060845050244444366430645

    // Add 1.
    // This can't overflow because the initial numbers are only 127bit due to removing the sign bit.
    var answer = @addWithOverflow(lhs_lo, 1);
    lhs_lo = answer[0];
    var overflowed = answer[1];
    lhs_hi = blk: {
        if (overflowed == 1) {
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
    // const _a = ea.lo;

    const g = gf.hi;
    const f = gf.lo;

    const j = jh.hi;
    const h = jh.lo;

    const l = lk.hi;
    const k = lk.lo;

    // b = e + f + h
    answer = @addWithOverflow(e, f);
    const e_plus_f = answer[0];
    overflowed = answer[1];
    var b_carry1: u128 = undefined;
    if (overflowed == 1) {
        b_carry1 = 1;
    } else {
        b_carry1 = 0;
    }

    answer = @addWithOverflow(e_plus_f, h);
    overflowed = answer[1];
    var b_carry2: u128 = undefined;
    if (overflowed == 1) {
        b_carry2 = 1;
    } else {
        b_carry2 = 0;
    }

    // c = carry + g + j + k // it doesn't say +k but I think it should be?
    answer = @addWithOverflow(g, j);
    const g_plus_j = answer[0];
    overflowed = answer[1];
    var c_carry1: u128 = undefined;
    if (overflowed == 1) {
        c_carry1 = 1;
    } else {
        c_carry1 = 0;
    }

    answer = @addWithOverflow(g_plus_j, k);
    const g_plus_j_plus_k = answer[0];
    overflowed = answer[1];
    var c_carry2: u128 = undefined;
    if (overflowed == 1) {
        c_carry2 = 1;
    } else {
        c_carry2 = 0;
    }

    answer = @addWithOverflow(g_plus_j_plus_k, b_carry1);
    const c_without_bcarry2 = answer[0];
    overflowed = answer[1];
    var c_carry3: u128 = undefined;
    if (overflowed == 1) {
        c_carry3 = 1;
    } else {
        c_carry3 = 0;
    }

    answer = @addWithOverflow(c_without_bcarry2, b_carry2);
    const c = answer[0];
    overflowed = answer[1];
    var c_carry4: u128 = undefined;
    if (overflowed == 1) {
        c_carry4 = 1;
    } else {
        c_carry4 = 0;
    }

    // d = carry + l
    answer = @addWithOverflow(l, c_carry1);
    overflowed = answer[1];
    answer = @addWithOverflow(answer[0], c_carry2);
    overflowed = overflowed | answer[1];
    answer = @addWithOverflow(answer[0], c_carry3);
    overflowed = overflowed | answer[1];
    answer = @addWithOverflow(answer[0], c_carry4);
    overflowed = overflowed | answer[1];
    const d = answer[0];

    // Final 512bit value is d, c, b, a
    // need to right shift 315 times
    // 315 - 256 is 59. So shift c right 59 times.
    // d takes up the higher space above c, so shift it left by (128 - 59 = 69).

    // Since d is being shift left 69 times, all of those 69 bits (+1 for the sign bit)
    // must be zero. Otherwise, we have an overflow.
    const d_high_bits = d >> 58;
    return .{ .value = @as(i128, @intCast(c >> 59 | (d << (128 - 59)))), .has_overflowed = overflowed | d_high_bits != 0 };
}

// Multiply two 128-bit ints and divide the result by 10^DECIMAL_PLACES
//
// Adapted from https://github.com/nlordell/ethnum-rs/blob/c9ed57e131bffde7bcc8274f376e5becf62ef9ac/src/intrinsics/native/divmod.rs
// Copyright (c) 2020 Nicholas Rodrigues Lordello
// Licensed under the Apache License version 2.0
//
// When translating this to Zig, we often have to use math.shr/shl instead of >>/<<
// This is because casting to the right types for Zig can be kind of tricky.
// See https://github.com/ziglang/zig/issues/7605
fn div_u256_by_u128(numerator: U256, denominator: u128) U256 {
    const N_UDWORD_BITS: u8 = 128;
    const N_UTWORD_BITS: u9 = 256;

    var q: U256 = undefined;
    var r: U256 = undefined;
    var sr: u8 = undefined;

    // special case
    if (numerator.hi == 0) {
        // 0 X
        // ---
        // 0 X
        return .{
            .hi = 0,
            .lo = numerator.lo / denominator,
        };
    }

    // numerator.hi != 0
    if (denominator == 0) {
        // K X
        // ---
        // 0 0
        return .{
            .hi = 0,
            .lo = numerator.hi / denominator,
        };
    } else {
        // K X
        // ---
        // 0 K
        // NOTE: Modified from `if (d.low() & (d.low() - 1)) == 0`.
        if (math.isPowerOfTwo(denominator)) {
            // if d is a power of 2
            if (denominator == 1) {
                return numerator;
            }

            sr = @ctz(denominator);

            return .{
                .hi = math.shr(u128, numerator.hi, sr),
                .lo = math.shl(u128, numerator.hi, N_UDWORD_BITS - sr) | math.shr(u128, numerator.lo, sr),
            };
        }

        // K X
        // ---
        // 0 K
        const denominator_leading_zeros = @clz(denominator);
        const numerator_hi_leading_zeros = @clz(numerator.hi);
        sr = 1 + N_UDWORD_BITS + denominator_leading_zeros - numerator_hi_leading_zeros;
        // 2 <= sr <= N_UTWORD_BITS - 1
        // q.all = n.all << (N_UTWORD_BITS - sr);
        // r.all = n.all >> sr;
        // #[allow(clippy::comparison_chain)]
        if (sr == N_UDWORD_BITS) {
            q = .{
                .hi = numerator.lo,
                .lo = 0,
            };
            r = .{
                .hi = 0,
                .lo = numerator.hi,
            };
        } else if (sr < N_UDWORD_BITS) {
            // 2 <= sr <= N_UDWORD_BITS - 1
            q = .{
                .hi = math.shl(u128, numerator.lo, N_UDWORD_BITS - sr),
                .lo = 0,
            };
            r = .{
                .hi = math.shr(u128, numerator.hi, sr),
                .lo = math.shl(u128, numerator.hi, N_UDWORD_BITS - sr) | math.shr(u128, numerator.lo, sr),
            };
        } else {
            // N_UDWORD_BITS + 1 <= sr <= N_UTWORD_BITS - 1
            q = .{
                .hi = math.shl(u128, numerator.hi, N_UTWORD_BITS - sr) | math.shr(u128, numerator.lo, sr - N_UDWORD_BITS),
                .lo = math.shl(u128, numerator.lo, N_UTWORD_BITS - sr),
            };
            r = .{
                .hi = 0,
                .lo = math.shr(u128, numerator.hi, sr - N_UDWORD_BITS),
            };
        }
    }

    // Not a special case
    // q and r are initialized with:
    // q.all = n.all << (N_UTWORD_BITS - sr);
    // r.all = n.all >> sr;
    // 1 <= sr <= N_UTWORD_BITS - 1
    var carry: u128 = 0;

    while (sr > 0) {
        // r:q = ((r:q)  << 1) | carry
        r.hi = (r.hi << 1) | (r.lo >> (N_UDWORD_BITS - 1));
        r.lo = (r.lo << 1) | (q.hi >> (N_UDWORD_BITS - 1));
        q.hi = (q.hi << 1) | (q.lo >> (N_UDWORD_BITS - 1));
        q.lo = (q.lo << 1) | carry;

        // carry = 0;
        // if (r.all >= d.all)
        // {
        //     r.all -= d.all;
        //      carry = 1;
        // }
        // NOTE: Modified from `(d - r - 1) >> (N_UTWORD_BITS - 1)` to be an
        // **arithmetic** shift.

        var answer = @subWithOverflow(denominator, r.lo);
        var lo = answer[0];
        var lo_overflowed = answer[1];
        var hi = 0 -% @as(u128, @intCast(@as(u1, @bitCast(lo_overflowed)))) -% r.hi;

        answer = @subWithOverflow(lo, 1);
        lo = answer[0];
        lo_overflowed = answer[1];
        hi = hi -% @as(u128, @intCast(@as(u1, @bitCast(lo_overflowed))));

        // NOTE: this U256 was originally created by:
        //
        // ((hi as i128) >> 127).as_u256()
        //
        // As an implementation of `as_u256`, we wrap a negative value around to the maximum value of U256.

        const s_u128 = math.shr(u128, hi, 127);
        var s_hi: u128 = undefined;
        var s_lo: u128 = undefined;
        if (s_u128 == 1) {
            s_hi = math.maxInt(u128);
            s_lo = math.maxInt(u128);
        } else {
            s_hi = 0;
            s_lo = 0;
        }
        const s = .{
            .hi = s_hi,
            .lo = s_lo,
        };

        carry = s.lo & 1;

        // var (lo, carry) = r.lo.overflowing_sub(denom & s.lo);
        answer = @subWithOverflow(r.lo, (denominator & s.lo));
        lo = answer[0];
        lo_overflowed = answer[1];
        hi = r.hi -% @as(u128, @intCast(@as(u1, @bitCast(lo_overflowed))));

        r = .{ .hi = hi, .lo = lo };

        sr -= 1;
    }

    const hi = (q.hi << 1) | (q.lo >> (127));
    const lo = (q.lo << 1) | carry;

    return .{ .hi = hi, .lo = lo };
}

const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expectEqualSlices = std.testing.expectEqualSlices;
const expect = std.testing.expect;

// exports

/// TODO: Document fromStr.
pub fn fromStr(arg: RocStr) callconv(.c) NumParseResult(i128) {
    if (@call(.always_inline, RocDec.fromStr, .{arg})) |dec| {
        return .{ .errorcode = 0, .value = dec.num };
    } else {
        return .{ .errorcode = 1, .value = 0 };
    }
}

/// TODO: Document to_str.
pub fn to_str(
    arg: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    return @call(.always_inline, RocDec.to_str, .{ arg, roc_ops });
}

/// TODO: Document fromF64C.
pub fn fromF64C(
    arg: f64,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    if (@call(.always_inline, RocDec.fromF64, .{arg})) |dec| {
        return dec.num;
    } else {
        roc_ops.crash("Decimal conversion from f64 failed!");
        unreachable;
    }
}

/// TODO: Document fromF32C.
pub fn fromF32C(
    arg_f32: f32,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    const arg_f64 = arg_f32;
    if (@call(.always_inline, RocDec.fromF64, .{arg_f64})) |dec| {
        return dec.num;
    } else {
        roc_ops.crash("Decimal conversion from f32!");
        unreachable;
    }
}

/// TODO: Document toF64.
pub fn toF64(arg: RocDec) callconv(.c) f64 {
    return @call(.always_inline, RocDec.toF64, .{arg});
}

/// TODO: Document exportFromInt.
pub fn exportFromInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            self: T,
            roc_ops: *RocOps,
        ) callconv(.c) i128 {
            const this = @as(i128, @intCast(self));

            const answer = @mulWithOverflow(this, RocDec.one_point_zero_i128);
            if (answer[1] == 1) {
                roc_ops.crash("Decimal conversion from Integer failed!");
            } else {
                return answer[0];
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// TODO: Document fromU64C.
pub fn fromU64C(arg: u64) callconv(.c) i128 {
    return @call(.always_inline, RocDec.fromU64, .{arg}).toI128();
}

/// TODO: Document toI128.
pub fn toI128(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.toI128, .{arg});
}

/// TODO: Document fromI128.
pub fn fromI128(arg: i128) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.fromI128, .{arg});
}

/// TODO: Document eqC.
pub fn eqC(arg1: RocDec, arg2: RocDec) callconv(.c) bool {
    return @call(.always_inline, RocDec.eq, .{ arg1, arg2 });
}

/// TODO: Document neqC.
pub fn neqC(arg1: RocDec, arg2: RocDec) callconv(.c) bool {
    return @call(.always_inline, RocDec.neq, .{ arg1, arg2 });
}

/// TODO: Document negateC.
pub fn negateC(
    arg: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return if (@call(.always_inline, RocDec.negate, .{arg})) |dec| dec.num else {
        roc_ops.crash("Decimal negation overflow!");
        unreachable;
    };
}

/// TODO: Document absC.
pub fn absC(
    arg: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    const result = @call(.always_inline, RocDec.abs, .{arg}) catch {
        roc_ops.crash("Decimal absolute value overflow!");
        unreachable;
    };
    return result.num;
}

/// TODO: Document addC.
pub fn addC(arg1: RocDec, arg2: RocDec) callconv(.c) WithOverflow(RocDec) {
    return @call(.always_inline, RocDec.addWithOverflow, .{ arg1, arg2 });
}

/// TODO: Document subC.
pub fn subC(arg1: RocDec, arg2: RocDec) callconv(.c) WithOverflow(RocDec) {
    return @call(.always_inline, RocDec.subWithOverflow, .{ arg1, arg2 });
}

/// TODO: Document mulC.
pub fn mulC(arg1: RocDec, arg2: RocDec) callconv(.c) WithOverflow(RocDec) {
    return @call(.always_inline, RocDec.mulWithOverflow, .{ arg1, arg2 });
}

/// TODO: Document divC.
pub fn divC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return @call(.always_inline, RocDec.div, .{ arg1, arg2, roc_ops }).num;
}

/// TODO: Document logC.
pub fn logC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.log, .{arg}).num;
}

/// TODO: Document powC.
pub fn powC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return @call(.always_inline, RocDec.pow, .{ arg1, arg2, roc_ops }).num;
}

/// TODO: Document sinC.
pub fn sinC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.sin, .{arg}).num;
}

/// TODO: Document cosC.
pub fn cosC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.cos, .{arg}).num;
}

/// TODO: Document tanC.
pub fn tanC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.tan, .{arg}).num;
}

/// TODO: Document asinC.
pub fn asinC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.asin, .{arg}).num;
}

/// TODO: Document acosC.
pub fn acosC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.acos, .{arg}).num;
}

/// TODO: Document atanC.
pub fn atanC(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.atan, .{arg}).num;
}

/// TODO: Document addOrPanicC.
pub fn addOrPanicC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.add, .{ arg1, arg2, roc_ops });
}

/// TODO: Document addSaturatedC.
pub fn addSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.addSaturated, .{ arg1, arg2 });
}

/// TODO: Document subOrPanicC.
pub fn subOrPanicC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.sub, .{ arg1, arg2, roc_ops });
}

/// TODO: Document subSaturatedC.
pub fn subSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.subSaturated, .{ arg1, arg2 });
}

/// TODO: Document mulOrPanicC.
pub fn mulOrPanicC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.mul, .{ arg1, arg2, roc_ops });
}

/// TODO: Document mulSaturatedC.
pub fn mulSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.mulSaturated, .{ arg1, arg2 });
}

/// TODO: Document exportRound.
pub fn exportRound(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            input: RocDec,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return @as(T, @intCast(@divFloor(input.round(roc_ops).num, RocDec.one_point_zero_i128)));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// TODO: Document exportFloor.
pub fn exportFloor(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            input: RocDec,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return @as(T, @intCast(@divFloor(input.floor(roc_ops).num, RocDec.one_point_zero_i128)));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// TODO: Document exportCeiling.
pub fn exportCeiling(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            input: RocDec,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return @as(T, @intCast(@divFloor(input.ceiling(roc_ops).num, RocDec.one_point_zero_i128)));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

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
