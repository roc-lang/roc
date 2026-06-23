//! Builtin decimal operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's Dec type, a fixed-point
//! decimal number with 18 decimal places. It includes arithmetic operations,
//! parsing, formatting, and conversions for precise decimal calculations
//! without floating-point precision issues.
const std = @import("std");
const i128h = @import("compiler_rt_128.zig");

const U256 = @import("num.zig").U256;
const TestEnv = @import("utils.zig").TestEnv;
const WithOverflow = @import("utils.zig").WithOverflow;
const NumParseResult = @import("num.zig").NumParseResult;
const RocOps = @import("host_abi.zig").RocOps;
const RocStr = @import("str.zig").RocStr;
const mul_u128 = @import("num.zig").mul_u128;
const math = std.math;

/// Format an i128 as decimal into a buffer, returning the number of bytes written.
/// Uses i128h helpers to avoid compiler_rt 128-bit div/mod intrinsics.
fn printI128Decimal(buf: []u8, val: i128) usize {
    if (val == 0) {
        buf[0] = '0';
        return 1;
    }
    const is_negative = val < 0;
    var abs_val: u128 = if (is_negative) ~@as(u128, @bitCast(val)) +% 1 else @intCast(val);

    // Write digits in reverse into a temp buffer
    var tmp: [40]u8 = undefined;
    var len: usize = 0;
    while (abs_val != 0) {
        const digit: u8 = @truncate(i128h.rem_u128(abs_val, 10));
        tmp[len] = '0' + digit;
        len += 1;
        abs_val = i128h.divTrunc_u128(abs_val, 10);
    }

    // Write sign then reversed digits into output buffer
    var pos: usize = 0;
    if (is_negative) {
        buf[pos] = '-';
        pos += 1;
    }
    for (0..len) |i| {
        buf[pos] = tmp[len - 1 - i];
        pos += 1;
    }
    return pos;
}

/// Roc's fixed-point decimal runtime representation.
///
/// `num` stores the decimal value scaled by 10^18, so `1.0` is represented as
/// `1_000_000_000_000_000_000`. The extern layout is part of the C ABI used by
/// generated code and builtin exports.
pub const RocDec = extern struct {
    num: i128,

    pub const decimal_places: u5 = 18;
    pub const whole_number_places: u5 = 21;
    pub const max_digits: u6 = 39;
    pub const max_str_length: u6 = max_digits + 2; // + 2 here to account for the sign & decimal dot

    pub const min: RocDec = .{ .num = math.minInt(i128) };
    pub const max: RocDec = .{ .num = math.maxInt(i128) };

    pub const one_point_zero_i128: i128 = math.pow(i128, 10, RocDec.decimal_places);
    pub const one_point_zero: RocDec = .{ .num = one_point_zero_i128 };
    pub const neg_one_point_zero: RocDec = .{ .num = -one_point_zero_i128 };
    pub const e: RocDec = fromComptimeFloat(math.e);
    pub const pi: RocDec = fromComptimeFloat(math.pi);
    pub const tau: RocDec = fromComptimeFloat(math.tau);
    pub const half_pi: RocDec = fromComptimeFloat(math.pi / 2.0);
    pub const ln2: RocDec = fromComptimeFloat(math.ln2);

    pub const two_point_zero: RocDec = .{ .num = one_point_zero_i128 * 2 };
    pub const zero_point_five: RocDec = .{ .num = one_point_zero_i128 / 2 };

    pub fn fromU64(num: u64) RocDec {
        return .{ .num = i128h.mul_i128(@as(i128, num), one_point_zero_i128) };
    }

    fn fromComptimeFloat(comptime num: comptime_float) RocDec {
        const scale: comptime_float = @floatFromInt(one_point_zero_i128);
        return .{ .num = @intFromFloat(num * scale) };
    }

    /// Convert a fraction represented as numerator / 10^denominator_power to RocDec.
    /// For example, 314 with power 2 represents 3.14 (314 / 100).
    pub fn fromFraction(numerator: i128, denominator_power: u8) RocDec {
        // decimal_places is 18, so scale_power = 18 - denominator_power (clamped to 0)
        const scale_power: u7 = if (denominator_power >= decimal_places)
            0
        else
            decimal_places - @as(u5, @intCast(denominator_power));
        const scale = i128h.pow10_i128(@intCast(scale_power));
        return .{ .num = i128h.mul_i128(numerator, scale) };
    }

    pub fn fromF64(num: f64) ?RocDec {
        const result: f64 = num * comptime @as(f64, @floatFromInt(one_point_zero_i128));

        if (result > comptime @as(f64, @floatFromInt(math.maxInt(i128)))) {
            return null;
        }

        if (result < comptime @as(f64, @floatFromInt(math.minInt(i128)))) {
            return null;
        }

        const ret: RocDec = .{ .num = i128h.f64_to_i128(result) };
        return ret;
    }

    pub fn toF64(dec: RocDec) f64 {
        return i128h.i128_to_f64(dec.num) / comptime @as(f64, @floatFromInt(one_point_zero_i128));
    }

    // All parse failures currently map to null; the Roc wrapper reports that as
    // BadNumStr.
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

        // Sign is folded into the magnitude before multiplication and addition,
        // rather than negating at the end. This is required for the lowest Dec
        // value: its magnitude is 2^127, which overflows positive i128 even
        // though -2^127 is itself representable.
        var before_str_length = length;
        var after_val_i128: ?i128 = null;
        if (point_index) |point_idx| {
            before_str_length = point_idx;

            const after_str_len = (length - 1) - point_idx;
            if (after_str_len > decimal_places) {
                // More than 18 fractional digits cannot be represented exactly.
                return null;
            }
            const diff_decimal_places = decimal_places - after_str_len;

            const after_str = roc_str_slice[point_idx + 1 .. length];
            const after_u64 = @import("num.zig").parseUnsignedDecimal(u64, after_str);
            if (after_u64) |f| {
                const unsigned = i128h.mul_i128(@as(i128, @intCast(f)), i128h.pow10_i128(@intCast(diff_decimal_places)));
                after_val_i128 = if (is_negative) -unsigned else unsigned;
            }
        }

        const before_str = roc_str_slice[initial_index..before_str_length];
        var before_val_i128: ?i128 = null;
        if (before_str.len > 0) {
            const before = @import("num.zig").parseUnsignedDecimal(i128, before_str) orelse return null;
            const signed_before: i128 = if (is_negative) -before else before;
            const mul_ans = @import("num.zig").mulWithOverflow(i128, signed_before, one_point_zero_i128);
            if (mul_ans.has_overflowed) {
                // The whole-number part is outside Dec's scaled i128 range.
                return null;
            }
            before_val_i128 = mul_ans.value;
        }

        if (before_val_i128) |before| {
            if (after_val_i128) |after| {
                const answer = @addWithOverflow(before, after);
                if (answer[1] == 1) {
                    // Combining whole and fractional parts exceeded Dec's range.
                    return null;
                }
                return .{ .num = answer[0] };
            } else {
                return .{ .num = before };
            }
        } else if (after_val_i128) |after| {
            return .{ .num = after };
        } else {
            return null;
        }
    }

    inline fn isDigit(c: u8) bool {
        return (c -% 48) <= 9;
    }

    /// Format this Dec value into the provided buffer, returning the slice containing the result.
    /// The buffer must be at least `max_str_length` bytes.
    /// This is the allocation-free version; use `to_str` if you need a RocStr.
    pub fn format_to_buf(self: RocDec, buf: *[max_str_length]u8) []const u8 {
        // Special case
        if (self.num == 0) {
            buf[0] = '0';
            buf[1] = '.';
            buf[2] = '0';
            return buf[0..3];
        }

        const num = self.num;
        const is_negative = num < 0;

        // Format the backing i128 into an array of digit (ascii) characters (u8s)
        // Uses i128h helpers to avoid compiler_rt 128-bit div/mod intrinsics.
        var digit_bytes_storage: [max_digits + 1]u8 = undefined;
        var num_digits = printI128Decimal(&digit_bytes_storage, num);
        var digit_bytes: [*]u8 = digit_bytes_storage[0..];

        var position: usize = 0;

        // if negative, the first character is a negating minus
        if (is_negative) {
            buf[position] = '-';
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
                buf[position] = c;
                position += 1;
            }
        } else {
            // otherwise there are no actual digits before the decimal point
            // but we format it with a '0'
            buf[position] = '0';
            position += 1;
        }

        // we've done everything before the decimal point, so now we can put the decimal point in
        buf[position] = '.';
        position += 1;

        // Count trailing base-10 zeros directly from the digit characters.
        // This avoids i128 modulo arithmetic which produces incorrect results on wasm32.
        var trailing_zeros: u6 = 0;
        {
            var i = num_digits;
            while (i > 0) {
                i -= 1;
                if (digit_bytes[i] == '0') {
                    trailing_zeros += 1;
                } else {
                    break;
                }
            }
        }
        if (trailing_zeros >= decimal_places) {
            // add just a single zero if all decimal digits are zero
            buf[position] = '0';
            position += 1;
        } else {
            // Figure out if we need to prepend any zeros to the after decimal point
            // For example, for the number 0.000123 we need to prepend 3 zeros after the decimal point
            const after_zeros_num = if (num_digits < decimal_places) decimal_places - num_digits else 0;

            var i: usize = 0;
            while (i < after_zeros_num) : (i += 1) {
                buf[position] = '0';
                position += 1;
            }

            // otherwise append the decimal digits except the trailing zeros
            for (digit_bytes[before_digits_offset .. num_digits - trailing_zeros]) |c| {
                buf[position] = c;
                position += 1;
            }
        }

        return buf[0..position];
    }

    pub fn to_str(self: RocDec, roc_ops: *RocOps) RocStr {
        var buf: [max_str_length]u8 = undefined;
        const len = self.format_to_buf(&buf).len;
        return RocStr.init(&buf, len, roc_ops);
    }

    pub fn toI128(self: RocDec) i128 {
        return self.num;
    }

    pub fn fromI128(num: i128) RocDec {
        return .{ .num = num };
    }

    /// Extract the whole number part of a Dec via truncating division.
    /// Truncates toward zero: -1.5 → -1, 1.5 → 1
    pub fn toWholeInt(self: RocDec) i128 {
        return i128h.divTrunc_i128(self.num, one_point_zero_i128);
    }

    /// Convert a whole number to Dec representation.
    /// Returns null if the integer would overflow Dec's range.
    pub fn fromWholeInt(int: i128) ?RocDec {
        const result = @import("num.zig").mulWithOverflow(i128, int, one_point_zero_i128);
        return if (result.has_overflowed) null else RocDec{ .num = result.value };
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

    pub fn abs(self: RocDec) error{OutOfRange}!RocDec {
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
        const abs_num: u128 = @abs(self.num);
        const digits = i128h.rem_u128(abs_num, @as(u128, @intCast(RocDec.one_point_zero.num)));
        const digits_i128: i128 = @intCast(digits);

        return RocDec{ .num = if (self.num < 0) -digits_i128 else digits_i128 };
    }

    // Returns the nearest integer to self. If a value is half-way between two integers, round away from 0.0.
    pub fn round(
        arg1: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        // this rounds towards zero
        const tmp = arg1.trunc(roc_ops);

        const fract_num = arg1.fract().num;
        const abs_fract: i128 = if (fract_num < 0) -fract_num else fract_num;

        if (abs_fract >= RocDec.zero_point_five.num) {
            const one_signed: i128 = if (arg1.num < 0) -RocDec.one_point_zero.num else RocDec.one_point_zero.num;
            return RocDec.add(
                tmp,
                RocDec{ .num = one_signed },
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
            if (exponent & 1 == 0) {
                const half_power = RocDec.powInt(base, i128h.shr_i128(exponent, 1), roc_ops); // `>> 1` == `/ 2`
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
                i128h.divTrunc_i128(exponent.num, RocDec.one_point_zero_i128),
                roc_ops,
            );
        } else if (base.num <= 0) {
            roc_ops.crash("Decimal power is undefined for non-positive base and fractional exponent!");
            unreachable;
        } else {
            const log_base = decLnPositive(base, roc_ops);
            const scaled_exponent = RocDec.mul(log_base, exponent, roc_ops);
            return decExp(scaled_exponent, roc_ops);
        }
    }

    pub fn sqrt(
        self: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        // sqrt(-n) is an error
        if (self.num < 0) {
            roc_ops.crash("Decimal square root of a negative number!");
            unreachable;
        }

        return decSqrtNonNegative(self);
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

        const numerator_u256: U256 = mul_u128(numerator_u128, comptime math.pow(u128, 10, decimal_places));
        const answer = div_u256_by_u128(numerator_u256, denominator_u128);

        var unsigned_answer: i128 = undefined;
        if (answer.hi == 0 and answer.lo <= math.maxInt(i128)) {
            unsigned_answer = @as(i128, @intCast(answer.lo));
        } else {
            roc_ops.crash("Decimal division overflow!");
        }

        return RocDec{ .num = if (is_answer_negative) -unsigned_answer else unsigned_answer };
    }

    pub fn log(self: RocDec, roc_ops: *RocOps) RocDec {
        return decLnPositive(self, roc_ops);
    }

    pub fn sin(self: RocDec) RocDec {
        return decSinCos(self).sin;
    }

    pub fn cos(self: RocDec) RocDec {
        return decSinCos(self).cos;
    }

    pub fn tan(self: RocDec, roc_ops: *RocOps) RocDec {
        const pair = decSinCos(self);
        return RocDec.div(pair.sin, pair.cos, roc_ops);
    }

    pub fn asin(self: RocDec, roc_ops: *RocOps) RocDec {
        if (self.num > RocDec.one_point_zero.num or self.num < RocDec.neg_one_point_zero.num) {
            roc_ops.crash("Decimal asin input is outside [-1, 1]!");
            unreachable;
        }
        if (self.num == RocDec.one_point_zero.num) return RocDec.half_pi;
        if (self.num == RocDec.neg_one_point_zero.num) return RocDec{ .num = -RocDec.half_pi.num };

        const squared = RocDec.mul(self, self, roc_ops);
        const complement = RocDec.sub(RocDec.one_point_zero, squared, roc_ops);
        const denominator = RocDec.sqrt(complement, roc_ops);
        return RocDec.atan(RocDec.div(self, denominator, roc_ops), roc_ops);
    }

    pub fn acos(self: RocDec, roc_ops: *RocOps) RocDec {
        return RocDec.sub(RocDec.half_pi, RocDec.asin(self, roc_ops), roc_ops);
    }

    pub fn atan(self: RocDec, roc_ops: *RocOps) RocDec {
        if (self.num > RocDec.one_point_zero.num) {
            const reciprocal = RocDec.div(RocDec.one_point_zero, self, roc_ops);
            return RocDec.sub(RocDec.half_pi, decAtanReduced(reciprocal), roc_ops);
        }
        if (self.num < RocDec.neg_one_point_zero.num) {
            const reciprocal = RocDec.div(RocDec.one_point_zero, self, roc_ops);
            return RocDec.sub(RocDec{ .num = -RocDec.half_pi.num }, decAtanReduced(reciprocal), roc_ops);
        }
        return decAtanReduced(self);
    }

    pub fn rem(
        self: RocDec,
        other: RocDec,
        roc_ops: *RocOps,
    ) RocDec {
        // (n % 0) is an error
        if (other.num == 0) {
            roc_ops.crash("Decimal remainder by 0!");
        }

        // For Dec, remainder is straightforward since both operands have the same scaling factor
        return RocDec{ .num = i128h.rem_i128(self.num, other.num) };
    }
};

const dec_cordic_k = RocDec{ .num = 607252935008881256 };

const dec_cordic_atan = [_]i128{
    785398163397448309,
    463647609000806116,
    244978663126864154,
    124354994546761435,
    62418809995957348,
    31239833430268276,
    15623728620476830,
    7812341060101111,
    3906230131966971,
    1953122516478818,
    976562189559319,
    488281211194898,
    244140620149361,
    122070311893670,
    61035156174208,
    30517578115526,
    15258789061315,
    7629394531101,
    3814697265606,
    1907348632810,
    953674316405,
    476837158203,
    238418579101,
    119209289550,
    59604644775,
    29802322387,
    14901161193,
    7450580596,
    3725290298,
    1862645149,
    931322574,
    465661287,
    232830643,
    116415321,
    58207660,
    29103830,
    14551915,
    7275957,
    3637978,
    1818989,
    909494,
    454747,
    227373,
    113686,
    56843,
    28421,
    14210,
    7105,
    3552,
    1776,
    888,
    444,
    222,
    111,
    55,
    27,
    13,
    6,
    3,
    1,
    0,
    0,
    0,
    0,
};

const DecSinCos = struct {
    sin: RocDec,
    cos: RocDec,
};

fn decSqrtNonNegative(value: RocDec) RocDec {
    const raw: u128 = @intCast(value.num);
    const scaled = mul_u128(raw, @as(u128, @intCast(RocDec.one_point_zero_i128)));
    const root = intSqrtU256(scaled);
    return RocDec{ .num = @intCast(root) };
}

fn intSqrtU256(value: U256) u128 {
    var low: u128 = 0;
    var high: u128 = @intCast(math.maxInt(i128));
    var answer: u128 = 0;

    while (low <= high) {
        const mid = low + i128h.shr(high - low, 1);
        const square = mul_u128(mid, mid);

        if (u256Le(square, value)) {
            answer = mid;
            low = mid + 1;
        } else if (mid == 0) {
            break;
        } else {
            high = mid - 1;
        }
    }

    return answer;
}

fn u256Le(lhs: U256, rhs: U256) bool {
    return lhs.hi < rhs.hi or (lhs.hi == rhs.hi and lhs.lo <= rhs.lo);
}

fn decRemKnownNonZero(self: RocDec, other: RocDec) RocDec {
    std.debug.assert(other.num != 0);
    return RocDec{ .num = i128h.rem_i128(self.num, other.num) };
}

fn decLnPositive(value: RocDec, roc_ops: *RocOps) RocDec {
    if (value.num <= 0) {
        roc_ops.crash("Decimal log is undefined for non-positive input!");
        unreachable;
    }

    var reduced = value;
    var power_of_two: i128 = 0;
    while (reduced.num >= RocDec.two_point_zero.num) {
        reduced = RocDec.div(reduced, RocDec.two_point_zero, roc_ops);
        power_of_two += 1;
    }
    while (reduced.num < RocDec.one_point_zero.num) {
        reduced = RocDec.mul(reduced, RocDec.two_point_zero, roc_ops);
        power_of_two -= 1;
    }

    const numerator = RocDec.sub(reduced, RocDec.one_point_zero, roc_ops);
    const denominator = RocDec.add(reduced, RocDec.one_point_zero, roc_ops);
    const z = RocDec.div(numerator, denominator, roc_ops);
    const z_squared = RocDec.mul(z, z, roc_ops);

    var term = z;
    var sum = RocDec{ .num = 0 };
    var divisor: i128 = 1;
    while (term.num != 0) {
        const divisor_dec = RocDec.fromWholeInt(divisor).?;
        const next = RocDec.div(term, divisor_dec, roc_ops);
        if (next.num == 0) break;
        sum = RocDec.add(sum, next, roc_ops);
        term = RocDec.mul(term, z_squared, roc_ops);
        divisor += 2;
    }

    var result = RocDec.mul(RocDec.two_point_zero, sum, roc_ops);
    if (power_of_two != 0) {
        const power_dec = RocDec.fromWholeInt(power_of_two).?;
        const correction = RocDec.mul(power_dec, RocDec.ln2, roc_ops);
        result = RocDec.add(result, correction, roc_ops);
    }
    return result;
}

fn decExp(value: RocDec, roc_ops: *RocOps) RocDec {
    const quotient = RocDec.div(value, RocDec.ln2, roc_ops);
    const whole_quotient = RocDec.trunc(quotient, roc_ops);
    const exponent = whole_quotient.toWholeInt();
    const reduced = RocDec.sub(value, RocDec.mul(whole_quotient, RocDec.ln2, roc_ops), roc_ops);
    return decScaleByPowerOfTwo(decExpReduced(reduced, roc_ops), exponent, roc_ops);
}

fn decExpReduced(value: RocDec, roc_ops: *RocOps) RocDec {
    var sum = RocDec.one_point_zero;
    var term = RocDec.one_point_zero;
    var divisor: i128 = 1;

    while (true) {
        const divisor_dec = RocDec.fromWholeInt(divisor).?;
        const next = RocDec.div(RocDec.mul(term, value, roc_ops), divisor_dec, roc_ops);
        if (next.num == 0) break;
        sum = RocDec.add(sum, next, roc_ops);
        term = next;
        divisor += 1;
    }

    return sum;
}

fn decScaleByPowerOfTwo(value: RocDec, exponent: i128, roc_ops: *RocOps) RocDec {
    var result = value;
    var remaining = exponent;
    while (remaining > 0) {
        result = RocDec.mul(result, RocDec.two_point_zero, roc_ops);
        remaining -= 1;
    }
    while (remaining < 0 and result.num != 0) {
        result = RocDec.div(result, RocDec.two_point_zero, roc_ops);
        remaining += 1;
    }
    return result;
}

fn decSinCos(value: RocDec) DecSinCos {
    var angle = decRemKnownNonZero(value, RocDec.tau);
    if (angle.num > RocDec.pi.num) {
        angle = RocDec{ .num = angle.num - RocDec.tau.num };
    } else if (angle.num < -RocDec.pi.num) {
        angle = RocDec{ .num = angle.num + RocDec.tau.num };
    }

    var cos_sign: i128 = 1;
    if (angle.num > RocDec.half_pi.num) {
        angle = RocDec{ .num = RocDec.pi.num - angle.num };
        cos_sign = -1;
    } else if (angle.num < -RocDec.half_pi.num) {
        angle = RocDec{ .num = -RocDec.pi.num - angle.num };
        cos_sign = -1;
    }

    var pair = decCordicSinCos(angle);
    pair.cos.num *= cos_sign;
    return pair;
}

fn decCordicSinCos(angle: RocDec) DecSinCos {
    var x = dec_cordic_k.num;
    var y: i128 = 0;
    var z = angle.num;

    for (dec_cordic_atan, 0..) |atan_step, i| {
        const shift: u7 = @intCast(i);
        const x_shift = i128h.shr_i128(x, shift);
        const y_shift = i128h.shr_i128(y, shift);
        if (z >= 0) {
            x -= y_shift;
            y += x_shift;
            z -= atan_step;
        } else {
            x += y_shift;
            y -= x_shift;
            z += atan_step;
        }
    }

    return .{
        .sin = RocDec{ .num = y },
        .cos = RocDec{ .num = x },
    };
}

fn decAtanReduced(value: RocDec) RocDec {
    var x = RocDec.one_point_zero.num;
    var y = value.num;
    var z: i128 = 0;

    for (dec_cordic_atan, 0..) |atan_step, i| {
        if (y == 0) break;

        const shift: u7 = @intCast(i);
        const x_shift = i128h.shr_i128(x, shift);
        const y_shift = i128h.shr_i128(y, shift);
        if (y > 0) {
            x += y_shift;
            y -= x_shift;
            z += atan_step;
        } else {
            x -= y_shift;
            y += x_shift;
            z -= atan_step;
        }
    }

    return RocDec{ .num = z };
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
    const d_high_bits = i128h.shr(d, 58);
    const shifted = i128h.shr(c, 59) | i128h.shl(d, 69);
    const exceeds_i128 = shifted > @as(u128, @intCast(std.math.maxInt(i128)));
    const has_overflowed = overflowed != 0 or d_high_bits != 0 or exceeds_i128;
    return .{
        .value = if (has_overflowed) std.math.maxInt(i128) else @as(i128, @intCast(shifted)),
        .has_overflowed = has_overflowed,
    };
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
            .lo = i128h.divTrunc_u128(numerator.lo, denominator),
        };
    }

    // numerator.hi != 0
    if (denominator == 0) {
        // K X
        // ---
        // 0 0
        return .{
            .hi = 0,
            .lo = i128h.divTrunc_u128(numerator.hi, denominator),
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
                .hi = i128h.shr(numerator.hi, @intCast(sr)),
                .lo = i128h.shl(numerator.hi, @intCast(N_UDWORD_BITS - sr)) | i128h.shr(numerator.lo, @intCast(sr)),
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
                .hi = i128h.shl(numerator.lo, @intCast(N_UDWORD_BITS - sr)),
                .lo = 0,
            };
            r = .{
                .hi = i128h.shr(numerator.hi, @intCast(sr)),
                .lo = i128h.shl(numerator.hi, @intCast(N_UDWORD_BITS - sr)) | i128h.shr(numerator.lo, @intCast(sr)),
            };
        } else {
            // N_UDWORD_BITS + 1 <= sr <= N_UTWORD_BITS - 1
            q = .{
                .hi = i128h.shl(numerator.hi, @intCast(N_UTWORD_BITS - sr)) | i128h.shr(numerator.lo, @intCast(sr - N_UDWORD_BITS)),
                .lo = i128h.shl(numerator.lo, @intCast(N_UTWORD_BITS - sr)),
            };
            r = .{
                .hi = 0,
                .lo = i128h.shr(numerator.hi, @intCast(sr - N_UDWORD_BITS)),
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
        r.hi = i128h.shl(r.hi, 1) | i128h.shr(r.lo, 127);
        r.lo = i128h.shl(r.lo, 1) | i128h.shr(q.hi, 127);
        q.hi = i128h.shl(q.hi, 1) | i128h.shr(q.lo, 127);
        q.lo = i128h.shl(q.lo, 1) | carry;

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

        const s_u128 = i128h.shr(hi, 127);
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

    const result_hi = i128h.shl(q.hi, 1) | i128h.shr(q.lo, 127);
    const result_lo = i128h.shl(q.lo, 1) | carry;

    return .{ .hi = result_hi, .lo = result_lo };
}

const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

// exports

/// C ABI parse wrapper. Returns errorcode 0 with the scaled i128 on success, or
/// errorcode 1 with value 0 for any invalid or out-of-range decimal string.
pub fn fromStr(arg: RocStr) callconv(.c) NumParseResult(i128) {
    if (@call(.always_inline, RocDec.fromStr, .{arg})) |dec| {
        return .{ .errorcode = 0, .value = dec.num };
    } else {
        return .{ .errorcode = 1, .value = 0 };
    }
}

/// C ABI allocator-backed string wrapper for Dec formatting.
pub fn to_str(
    arg: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocStr {
    return @call(.always_inline, RocDec.to_str, .{ arg, roc_ops });
}

/// C ABI conversion wrapper from f64 to Dec. Crashes through RocOps if the
/// scaled value is outside Dec's i128 range.
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

/// C ABI conversion wrapper from f32 to Dec. Crashes through RocOps if the
/// scaled value is outside Dec's i128 range.
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

/// C ABI conversion wrapper from Dec to f64.
pub fn toF64(arg: RocDec) callconv(.c) f64 {
    return @call(.always_inline, RocDec.toF64, .{arg});
}

/// Convert Dec to F32 (lossy conversion)
pub fn toF32(arg: RocDec) callconv(.c) f32 {
    return @floatCast(arg.toF64());
}

/// Convert Dec to F32 with range check - returns null if out of range
pub fn toF32Try(arg: RocDec) ?f32 {
    const f64_val = arg.toF64();
    // Check if the value is within F32 range
    if (f64_val > math.floatMax(f32) or f64_val < -math.floatMax(f32)) {
        return null;
    }
    // Also check for infinity (which would indicate overflow)
    const f32_val: f32 = @floatCast(f64_val);
    if (math.isInf(f32_val) and !math.isInf(f64_val)) {
        return null;
    }
    return f32_val;
}

/// Convert Dec to integer by truncating the fractional part (wrapping on overflow)
pub fn toIntWrap(comptime T: type, arg: RocDec) T {
    // Divide by one_point_zero_i128 to get the integer part
    const whole_part = i128h.divTrunc_i128(arg.num, RocDec.one_point_zero_i128);
    // Truncate to the target type (wrapping)
    // First cast the i128 to u128, then truncate to the target size, then cast back to T if needed
    const as_u128: u128 = @bitCast(whole_part);
    const truncated = @as(std.meta.Int(.unsigned, @bitSizeOf(T)), @truncate(as_u128));
    return @bitCast(truncated);
}

/// Convert Dec to integer by truncating the fractional part (returns null if out of range)
pub fn toIntTry(comptime T: type, arg: RocDec) ?T {
    // Divide by one_point_zero_i128 to get the integer part
    const whole_part = i128h.divTrunc_i128(arg.num, RocDec.one_point_zero_i128);
    // Check if it fits in the target type
    if (whole_part < math.minInt(T) or whole_part > math.maxInt(T)) {
        return null;
    }
    return @intCast(whole_part);
}

/// Export an integer-to-Dec conversion wrapper for type T. The generated
/// function scales the integer by 10^18 and crashes through RocOps on overflow.
pub fn exportFromInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            self: T,
            roc_ops: *RocOps,
        ) callconv(.c) i128 {
            const this = @as(i128, @intCast(self));

            const answer = @import("num.zig").mulWithOverflow(i128, this, RocDec.one_point_zero_i128);
            if (answer.has_overflowed) {
                roc_ops.crash("Decimal conversion from Integer failed!");
            } else {
                return answer.value;
            }
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// C ABI conversion wrapper from u64 to Dec. Every u64 value fits in Dec.
pub fn fromU64C(arg: u64) callconv(.c) i128 {
    return @call(.always_inline, RocDec.fromU64, .{arg}).toI128();
}

/// C ABI wrapper that returns the raw scaled i128 backing a Dec.
pub fn toI128(arg: RocDec) callconv(.c) i128 {
    return @call(.always_inline, RocDec.toI128, .{arg});
}

/// C ABI wrapper that builds a Dec from an already scaled i128.
pub fn fromI128(arg: i128) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.fromI128, .{arg});
}

/// C ABI equality comparison wrapper.
pub fn eqC(arg1: RocDec, arg2: RocDec) callconv(.c) bool {
    return @call(.always_inline, RocDec.eq, .{ arg1, arg2 });
}

/// C ABI inequality comparison wrapper.
pub fn neqC(arg1: RocDec, arg2: RocDec) callconv(.c) bool {
    return @call(.always_inline, RocDec.neq, .{ arg1, arg2 });
}

/// C ABI negation wrapper. Crashes through RocOps when negating Dec.lowest
/// would overflow.
pub fn negateC(
    arg: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return if (@call(.always_inline, RocDec.negate, .{arg})) |dec| dec.num else {
        roc_ops.crash("Decimal negation overflow!");
        unreachable;
    };
}

/// C ABI absolute-value wrapper. Crashes through RocOps when abs(Dec.lowest)
/// would overflow.
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

/// C ABI checked-add wrapper. Returns a result value plus an overflow flag.
pub fn addC(arg1: RocDec, arg2: RocDec) callconv(.c) WithOverflow(RocDec) {
    return @call(.always_inline, RocDec.addWithOverflow, .{ arg1, arg2 });
}

/// C ABI checked-subtract wrapper. Returns a result value plus an overflow
/// flag.
pub fn subC(arg1: RocDec, arg2: RocDec) callconv(.c) WithOverflow(RocDec) {
    return @call(.always_inline, RocDec.subWithOverflow, .{ arg1, arg2 });
}

/// C ABI checked-multiply wrapper. Returns a result value plus an overflow
/// flag.
pub fn mulC(arg1: RocDec, arg2: RocDec) callconv(.c) WithOverflow(RocDec) {
    return @call(.always_inline, RocDec.mulWithOverflow, .{ arg1, arg2 });
}

/// C ABI division wrapper. Crashes through RocOps on division by zero or
/// overflow and otherwise returns the scaled i128 result.
pub fn divC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return @call(.always_inline, RocDec.div, .{ arg1, arg2, roc_ops }).num;
}

/// Truncating division: divide and truncate to the nearest integer toward zero.
pub fn divTruncC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    const quotient = @call(.always_inline, RocDec.div, .{ arg1, arg2, roc_ops });
    return @call(.always_inline, RocDec.trunc, .{ quotient, roc_ops }).num;
}

/// C ABI natural-log wrapper. The caller must provide a positive Dec; arithmetic
/// failures crash through RocOps.
pub fn logC(arg: RocDec, roc_ops: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.log, .{ arg, roc_ops }).num;
}

/// C ABI power wrapper. Domain errors, division by zero, and overflow crash
/// through RocOps.
pub fn powC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return @call(.always_inline, RocDec.pow, .{ arg1, arg2, roc_ops }).num;
}

/// C ABI square-root wrapper. Negative inputs and overflow crash through
/// RocOps.
pub fn sqrtC(
    arg: RocDec,
    roc_ops: *RocOps,
) callconv(.c) i128 {
    return @call(.always_inline, RocDec.sqrt, .{ arg, roc_ops }).num;
}

/// C ABI sine wrapper returning the scaled i128 result.
pub fn sinC(arg: RocDec, _: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.sin, .{arg}).num;
}

/// C ABI cosine wrapper returning the scaled i128 result.
pub fn cosC(arg: RocDec, _: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.cos, .{arg}).num;
}

/// C ABI tangent wrapper. Division by zero or overflow crashes through RocOps.
pub fn tanC(arg: RocDec, roc_ops: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.tan, .{ arg, roc_ops }).num;
}

/// C ABI arcsine wrapper. Inputs outside [-1, 1] and arithmetic failures crash
/// through RocOps.
pub fn asinC(arg: RocDec, roc_ops: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.asin, .{ arg, roc_ops }).num;
}

/// C ABI arccosine wrapper. Inputs outside [-1, 1] and arithmetic failures
/// crash through RocOps.
pub fn acosC(arg: RocDec, roc_ops: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.acos, .{ arg, roc_ops }).num;
}

/// C ABI arctangent wrapper returning the scaled i128 result. Arithmetic
/// failures crash through RocOps.
pub fn atanC(arg: RocDec, roc_ops: *RocOps) callconv(.c) i128 {
    return @call(.always_inline, RocDec.atan, .{ arg, roc_ops }).num;
}

/// C ABI addition wrapper that crashes through RocOps on overflow.
pub fn addOrPanicC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.add, .{ arg1, arg2, roc_ops });
}

/// C ABI saturated-add wrapper. Overflow clamps to RocDec.min or RocDec.max.
pub fn addSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.addSaturated, .{ arg1, arg2 });
}

/// C ABI subtraction wrapper that crashes through RocOps on overflow.
pub fn subOrPanicC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.sub, .{ arg1, arg2, roc_ops });
}

/// C ABI saturated-subtract wrapper. Overflow clamps to RocDec.min or RocDec.max.
pub fn subSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.subSaturated, .{ arg1, arg2 });
}

/// C ABI multiplication wrapper that crashes through RocOps on overflow.
pub fn mulOrPanicC(
    arg1: RocDec,
    arg2: RocDec,
    roc_ops: *RocOps,
) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.mul, .{ arg1, arg2, roc_ops });
}

/// C ABI saturated-multiply wrapper. Overflow clamps to RocDec.min or RocDec.max.
pub fn mulSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.c) RocDec {
    return @call(.always_inline, RocDec.mulSaturated, .{ arg1, arg2 });
}

/// Export a Dec rounding wrapper for integer return type T. The generated
/// function rounds away from zero on half values and returns the whole part.
pub fn exportRound(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            input: RocDec,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return @as(T, @intCast(i128h.divFloor_i128(input.round(roc_ops).num, RocDec.one_point_zero_i128)));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Export a Dec floor wrapper for integer return type T.
pub fn exportFloor(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            input: RocDec,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return @as(T, @intCast(i128h.divFloor_i128(input.floor(roc_ops).num, RocDec.one_point_zero_i128)));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

/// Export a Dec ceiling wrapper for integer return type T.
pub fn exportCeiling(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(
            input: RocDec,
            roc_ops: *RocOps,
        ) callconv(.c) T {
            return @as(T, @intCast(i128h.divFloor_i128(input.ceiling(roc_ops).num, RocDec.one_point_zero_i128)));
        }
    }.func;
    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn expectRocDecConstant(actual: RocDec, expected_text: []const u8) error{ InvalidExpectedDecimal, TestExpectedEqual }!void {
    const expected = RocDec.fromNonemptySlice(expected_text) orelse return error.InvalidExpectedDecimal;
    try std.testing.expectEqual(expected, actual);

    var buf: [RocDec.max_str_length]u8 = undefined;
    try std.testing.expectEqualStrings(expected_text, actual.format_to_buf(&buf));
}

test "math constants use fixed-point std.math values" {
    try expectRocDecConstant(RocDec.e, "2.718281828459045235");
    try expectRocDecConstant(RocDec.pi, "3.141592653589793238");
    try expectRocDecConstant(RocDec.tau, "6.283185307179586476");
    try expectRocDecConstant(RocDec.half_pi, "1.570796326794896619");
    try expectRocDecConstant(RocDec.ln2, "0.693147180559945309");
}

test "math constants preserve exact fixed-point relationships" {
    try std.testing.expectEqual(RocDec.pi.num * 2, RocDec.tau.num);
    try std.testing.expectEqual(@divTrunc(RocDec.pi.num, 2), RocDec.half_pi.num);
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
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    try std.testing.expectEqual(RocDec.fromU64(0), RocDec.log(RocDec.fromU64(1), test_env.getOps()));
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

fn expectApproxDec(expected: RocDec, actual: RocDec, tolerance: u128) error{TestUnexpectedResult}!void {
    const diff: u128 = @abs(expected.num - actual.num);
    try std.testing.expect(diff <= tolerance);
}

fn decFromText(text: []const u8) error{InvalidExpectedDecimal}!RocDec {
    return RocDec.fromNonemptySlice(text) orelse error.InvalidExpectedDecimal;
}

fn comptimeDecFromText(comptime text: []const u8) RocDec {
    @setEvalBranchQuota(100_000);
    return RocDec.fromNonemptySlice(text) orelse @compileError("invalid Dec fixture: " ++ text);
}

const DecExactUnaryCase = struct {
    input: RocDec,
    expected: RocDec,
};

const DecOracleUnaryOp = enum {
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
    log,
};

const DecOracleUnaryCase = struct {
    input: RocDec,
    expected: RocDec,
    max_error_raw: u128,
};

const DecOracleBinaryOp = enum {
    pow,
};

const DecOracleBinaryCase = struct {
    lhs: RocDec,
    rhs: RocDec,
    expected: RocDec,
    max_error_raw: u128,
};

fn comptimeDecToF128(comptime dec: RocDec) f128 {
    return @as(f128, @floatFromInt(dec.num)) / @as(f128, @floatFromInt(RocDec.one_point_zero_i128));
}

fn comptimeDecFromF128Trunc(comptime value: f128) RocDec {
    const scaled = value * @as(f128, @floatFromInt(RocDec.one_point_zero_i128));
    if (scaled > @as(f128, @floatFromInt(math.maxInt(i128)))) {
        @compileError("Dec oracle fixture overflowed above max Dec");
    }
    if (scaled < @as(f128, @floatFromInt(math.minInt(i128)))) {
        @compileError("Dec oracle fixture overflowed below min Dec");
    }
    return RocDec{ .num = @intFromFloat(scaled) };
}

fn comptimeSqrtExpected(comptime input: RocDec) RocDec {
    @setEvalBranchQuota(20_000);
    if (input.num < 0) @compileError("sqrt fixture input must be non-negative");

    const scaled = mul_u128(@as(u128, @intCast(input.num)), @as(u128, @intCast(RocDec.one_point_zero_i128)));
    var low: u128 = 0;
    var high: u128 = @intCast(math.maxInt(i128));
    var answer: u128 = 0;

    while (low <= high) {
        const mid = low + ((high - low) / 2);
        const square = mul_u128(mid, mid);
        if (u256Le(square, scaled)) {
            answer = mid;
            low = mid + 1;
        } else if (mid == 0) {
            break;
        } else {
            high = mid - 1;
        }
    }

    return RocDec{ .num = @intCast(answer) };
}

fn buildSqrtSpecCases(comptime input_texts: []const []const u8) [input_texts.len]DecExactUnaryCase {
    var cases: [input_texts.len]DecExactUnaryCase = undefined;
    inline for (input_texts, 0..) |text, i| {
        const input = comptimeDecFromText(text);
        cases[i] = .{
            .input = input,
            .expected = comptimeSqrtExpected(input),
        };
    }
    return cases;
}

fn decUnaryOracle(comptime op: DecOracleUnaryOp, comptime input: RocDec) RocDec {
    const x = comptimeDecToF128(input);
    const expected = switch (op) {
        .sin => @sin(x),
        .cos => @cos(x),
        .tan => @tan(x),
        .asin => math.asin(x),
        .acos => math.acos(x),
        .atan => math.atan(x),
        .log => @log(x),
    };
    return comptimeDecFromF128Trunc(expected);
}

fn buildUnaryOracleCases(
    comptime op: DecOracleUnaryOp,
    comptime input_texts: []const []const u8,
    comptime max_error_raw: u128,
) [input_texts.len]DecOracleUnaryCase {
    var cases: [input_texts.len]DecOracleUnaryCase = undefined;
    inline for (input_texts, 0..) |text, i| {
        const input = comptimeDecFromText(text);
        cases[i] = .{
            .input = input,
            .expected = decUnaryOracle(op, input),
            .max_error_raw = max_error_raw,
        };
    }
    return cases;
}

fn decBinaryOracle(comptime op: DecOracleBinaryOp, comptime lhs: RocDec, comptime rhs: RocDec) RocDec {
    const x = comptimeDecToF128(lhs);
    const y = comptimeDecToF128(rhs);
    const expected = switch (op) {
        .pow => @exp(@log(x) * y),
    };
    return comptimeDecFromF128Trunc(expected);
}

fn buildBinaryOracleCases(
    comptime op: DecOracleBinaryOp,
    comptime input_texts: []const [2][]const u8,
    comptime max_error_raw: u128,
) [input_texts.len]DecOracleBinaryCase {
    var cases: [input_texts.len]DecOracleBinaryCase = undefined;
    inline for (input_texts, 0..) |pair, i| {
        const lhs = comptimeDecFromText(pair[0]);
        const rhs = comptimeDecFromText(pair[1]);
        cases[i] = .{
            .lhs = lhs,
            .rhs = rhs,
            .expected = decBinaryOracle(op, lhs, rhs),
            .max_error_raw = max_error_raw,
        };
    }
    return cases;
}

fn expectDecWithin(expected: RocDec, actual: RocDec, max_error_raw: u128) error{TestUnexpectedResult}!void {
    const diff: u128 = @abs(expected.num - actual.num);
    if (diff > max_error_raw) {
        std.debug.print(
            "expected Dec raw {d}, actual raw {d}, max error {d}, actual error {d}\n",
            .{ expected.num, actual.num, max_error_raw, diff },
        );
        return error.TestUnexpectedResult;
    }
}

const dec_sqrt_spec_cases = buildSqrtSpecCases(&.{
    "0.0",
    "0.000000000000000001",
    "0.25",
    "1.0",
    "1.44",
    "2.0",
    "9.0",
    "12321.0",
});

const dec_sin_oracle_cases = buildUnaryOracleCases(.sin, &.{
    "-6.283185307179586476",
    "-3.141592653589793238",
    "-1.570796326794896619",
    "-0.785398163397448309",
    "-0.5",
    "0.0",
    "0.5",
    "0.785398163397448309",
    "1.570796326794896619",
    "3.141592653589793238",
    "6.283185307179586476",
}, 50_000);

const dec_cos_oracle_cases = buildUnaryOracleCases(.cos, &.{
    "-6.283185307179586476",
    "-3.141592653589793238",
    "-1.570796326794896619",
    "-0.785398163397448309",
    "-0.5",
    "0.0",
    "0.5",
    "0.785398163397448309",
    "1.570796326794896619",
    "3.141592653589793238",
    "6.283185307179586476",
}, 50_000);

const dec_tan_oracle_cases = buildUnaryOracleCases(.tan, &.{
    "-1.0",
    "-0.785398163397448309",
    "-0.5",
    "-0.1",
    "0.0",
    "0.1",
    "0.5",
    "0.785398163397448309",
    "1.0",
}, 150_000);

const dec_asin_oracle_cases = buildUnaryOracleCases(.asin, &.{
    "-1.0",
    "-0.5",
    "-0.1",
    "0.0",
    "0.1",
    "0.5",
    "1.0",
}, 75_000);

const dec_acos_oracle_cases = buildUnaryOracleCases(.acos, &.{
    "-1.0",
    "-0.5",
    "-0.1",
    "0.0",
    "0.1",
    "0.5",
    "1.0",
}, 75_000);

const dec_atan_oracle_cases = buildUnaryOracleCases(.atan, &.{
    "-10.0",
    "-1.0",
    "-0.5",
    "-0.1",
    "0.0",
    "0.1",
    "0.5",
    "1.0",
    "10.0",
}, 75_000);

const dec_log_oracle_cases = buildUnaryOracleCases(.log, &.{
    "0.1",
    "0.5",
    "1.0",
    "2.0",
    "10.0",
    "100.0",
}, 100_000);

const dec_pow_oracle_cases = buildBinaryOracleCases(.pow, &.{
    .{ "0.25", "0.5" },
    .{ "1.5", "2.25" },
    .{ "2.0", "0.5" },
    .{ "4.0", "0.5" },
    .{ "9.0", "0.5" },
    .{ "10.0", "0.5" },
}, 150_000);

test "sqrt: 2.0 truncates to fixed-point precision" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const two = RocDec.two_point_zero;
    const expected = RocDec{ .num = 1414213562373095048 };

    try std.testing.expectEqual(expected, two.sqrt(test_env.getOps()));
}

test "sqrt: small fixed-point values stay deterministic" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    try expectRocDecConstant((try decFromText("0.000000000000000001")).sqrt(test_env.getOps()), "0.000000001");
    try expectRocDecConstant((try decFromText("12321.0")).sqrt(test_env.getOps()), "111.0");
}

test "sqrt fixtures match exact fixed-point spec" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (dec_sqrt_spec_cases) |case| {
        try std.testing.expectEqual(case.expected, case.input.sqrt(test_env.getOps()));
    }
}

test "known-nonzero Dec remainder matches checked remainder" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for ([_]RocDec{
        RocDec{ .num = -RocDec.tau.num * 2 - 1 },
        RocDec{ .num = -RocDec.pi.num },
        RocDec{ .num = 0 },
        RocDec.pi,
        RocDec{ .num = RocDec.tau.num * 2 + 1 },
    }) |input| {
        try std.testing.expectEqual(
            RocDec.rem(input, RocDec.tau, test_env.getOps()),
            decRemKnownNonZero(input, RocDec.tau),
        );
    }
}

test "Dec trig matches f128 oracle envelope" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (dec_sin_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.sin(), case.max_error_raw);
    }
    inline for (dec_cos_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.cos(), case.max_error_raw);
    }
    inline for (dec_tan_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.tan(test_env.getOps()), case.max_error_raw);
    }
}

test "Dec inverse trig matches f128 oracle envelope" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (dec_asin_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.asin(test_env.getOps()), case.max_error_raw);
    }
    inline for (dec_acos_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.acos(test_env.getOps()), case.max_error_raw);
    }
    inline for (dec_atan_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.atan(test_env.getOps()), case.max_error_raw);
    }
}

test "Dec log and fractional pow match f128 oracle envelope" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    inline for (dec_log_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.input.log(test_env.getOps()), case.max_error_raw);
    }
    inline for (dec_pow_oracle_cases) |case| {
        try expectDecWithin(case.expected, case.lhs.pow(case.rhs, test_env.getOps()), case.max_error_raw);
    }
}

test "cordic trig identities" {
    const tolerance: u128 = 10_000;

    try expectApproxDec(RocDec{ .num = 0 }, (RocDec{ .num = 0 }).sin(), tolerance);
    try expectApproxDec(RocDec.one_point_zero, (RocDec{ .num = 0 }).cos(), tolerance);
    try expectApproxDec(RocDec.one_point_zero, RocDec.half_pi.sin(), tolerance);
    try expectApproxDec(RocDec{ .num = 0 }, RocDec.half_pi.cos(), tolerance);
}

test "cordic trig range reduction and quadrant signs" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const tolerance: u128 = 20_000;
    const quarter_pi = RocDec{ .num = @divTrunc(RocDec.pi.num, 4) };
    const neg_half_pi = RocDec{ .num = -RocDec.half_pi.num };

    try expectApproxDec(RocDec{ .num = 0 }, RocDec.pi.sin(), tolerance);
    try expectApproxDec(RocDec.neg_one_point_zero, RocDec.pi.cos(), tolerance);
    try expectApproxDec(RocDec{ .num = 0 }, RocDec.tau.sin(), tolerance);
    try expectApproxDec(RocDec.one_point_zero, RocDec.tau.cos(), tolerance);
    try expectApproxDec(RocDec.neg_one_point_zero, neg_half_pi.sin(), tolerance);
    try expectApproxDec(RocDec{ .num = 0 }, neg_half_pi.cos(), tolerance);
    try expectApproxDec(RocDec.one_point_zero, quarter_pi.tan(test_env.getOps()), tolerance);
}

test "cordic atan and inverse trig identities" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const tolerance: u128 = 10_000;

    try expectApproxDec(RocDec{ .num = 785398163397448309 }, RocDec.one_point_zero.atan(test_env.getOps()), tolerance);
    try expectApproxDec(RocDec.half_pi, RocDec.one_point_zero.asin(test_env.getOps()), tolerance);
    try expectApproxDec(RocDec{ .num = 0 }, RocDec.one_point_zero.acos(test_env.getOps()), tolerance);
}

test "cordic atan handles negative values and reciprocal reduction" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const tolerance: u128 = 20_000;
    const ten = RocDec.fromU64(10);
    const tenth = RocDec.div(RocDec.one_point_zero, ten, test_env.getOps());
    const atan_ten = ten.atan(test_env.getOps());
    const atan_tenth = tenth.atan(test_env.getOps());

    try expectApproxDec(RocDec{ .num = -785398163397448309 }, RocDec.neg_one_point_zero.atan(test_env.getOps()), tolerance);
    try expectApproxDec(RocDec.half_pi, RocDec.add(atan_ten, atan_tenth, test_env.getOps()), tolerance);
}

test "inverse trig maps back through trig functions" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const tolerance: u128 = 50_000;
    const half = RocDec.zero_point_five;

    try expectApproxDec(half, half.asin(test_env.getOps()).sin(), tolerance);
    try expectApproxDec(half, half.acos(test_env.getOps()).cos(), tolerance);
    try expectApproxDec(RocDec{ .num = -RocDec.half_pi.num }, RocDec.neg_one_point_zero.asin(test_env.getOps()), tolerance);
    try expectApproxDec(RocDec.pi, RocDec.neg_one_point_zero.acos(test_env.getOps()), tolerance);
}

test "fractional pow uses deterministic fixed-point exp ln path" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const four = RocDec.fromU64(4);
    const half = RocDec.zero_point_five;

    try expectApproxDec(RocDec.two_point_zero, four.pow(half, test_env.getOps()), 10_000);
}

test "pow covers integer negative and fractional exponents" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const three = RocDec.fromU64(3);
    const nine = RocDec.fromU64(9);
    const quarter = try decFromText("0.25");
    const neg_three = try decFromText("-3.0");

    try expectRocDecConstant(RocDec.two_point_zero.pow(try decFromText("10.0"), test_env.getOps()), "1024.0");
    try expectRocDecConstant(RocDec.two_point_zero.pow(neg_three, test_env.getOps()), "0.125");
    try expectApproxDec(three, nine.pow(RocDec.zero_point_five, test_env.getOps()), 10_000);
    try expectApproxDec(RocDec.zero_point_five, quarter.pow(RocDec.zero_point_five, test_env.getOps()), 10_000);
}
