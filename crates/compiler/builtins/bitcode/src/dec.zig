const std = @import("std");
const str = @import("str.zig");
const num_ = @import("num.zig");
const utils = @import("utils.zig");

const math = std.math;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const RocStr = str.RocStr;
const WithOverflow = utils.WithOverflow;
const roc_panic = @import("panic.zig").panic_help;
const U256 = num_.U256;
const mul_u128 = num_.mul_u128;

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

    pub fn fromU64(num: u64) RocDec {
        return .{ .num = num * one_point_zero_i128 };
    }

    pub fn fromF64(num: f64) ?RocDec {
        var result: f64 = num * comptime @intToFloat(f64, one_point_zero_i128);

        if (result > comptime @intToFloat(f64, math.maxInt(i128))) {
            return null;
        }

        if (result < comptime @intToFloat(f64, math.minInt(i128))) {
            return null;
        }

        var ret: RocDec = .{ .num = @floatToInt(i128, result) };
        return ret;
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
                @panic("TODO runtime exception for too many decimal places!");
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
                @panic("TODO runtime exception for overflow!");
            }
            before_val_i128 = result;
        }

        const dec: RocDec = blk: {
            if (before_val_i128) |before| {
                if (after_val_i128) |after| {
                    var result: i128 = undefined;
                    var overflowed = @addWithOverflow(i128, before, after, &result);
                    if (overflowed) {
                        @panic("TODO runtime exception for overflow!");
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

    pub fn toStr(self: RocDec) RocStr {
        // Special case
        if (self.num == 0) {
            return RocStr.init("0.0", 3);
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
        if (trailing_zeros == decimal_places) {
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

        return RocStr.init(&str_bytes, position);
    }

    pub fn eq(self: RocDec, other: RocDec) bool {
        return self.num == other.num;
    }

    pub fn neq(self: RocDec, other: RocDec) bool {
        return self.num != other.num;
    }

    pub fn negate(self: RocDec) ?RocDec {
        var negated = math.negate(self.num) catch null;
        return if (negated) |n| .{ .num = n } else null;
    }

    pub fn addWithOverflow(self: RocDec, other: RocDec) WithOverflow(RocDec) {
        var answer: i128 = undefined;
        const overflowed = @addWithOverflow(i128, self.num, other.num, &answer);

        return .{ .value = RocDec{ .num = answer }, .has_overflowed = overflowed };
    }

    pub fn add(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.addWithOverflow(self, other);

        if (answer.has_overflowed) {
            roc_panic("Decimal addition overflowed!", 0);
            unreachable;
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
        var answer: i128 = undefined;
        const overflowed = @subWithOverflow(i128, self.num, other.num, &answer);

        return .{ .value = RocDec{ .num = answer }, .has_overflowed = overflowed };
    }

    pub fn sub(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.subWithOverflow(self, other);

        if (answer.has_overflowed) {
            roc_panic("Decimal subtraction overflowed!", 0);
            unreachable;
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

        const self_u128 = @intCast(u128, math.absInt(self_i128) catch {
            if (other_i128 == 0) {
                return .{ .value = RocDec{ .num = 0 }, .has_overflowed = false };
            } else if (other_i128 == RocDec.one_point_zero.num) {
                return .{ .value = self, .has_overflowed = false };
            } else if (is_answer_negative) {
                return .{ .value = RocDec.min, .has_overflowed = true };
            } else {
                return .{ .value = RocDec.max, .has_overflowed = true };
            }
        });

        const other_u128 = @intCast(u128, math.absInt(other_i128) catch {
            if (self_i128 == 0) {
                return .{ .value = RocDec{ .num = 0 }, .has_overflowed = false };
            } else if (self_i128 == RocDec.one_point_zero.num) {
                return .{ .value = other, .has_overflowed = false };
            } else if (is_answer_negative) {
                return .{ .value = RocDec.min, .has_overflowed = true };
            } else {
                return .{ .value = RocDec.max, .has_overflowed = true };
            }
        });

        const unsigned_answer: i128 = mul_and_decimalize(self_u128, other_u128);

        if (is_answer_negative) {
            return .{ .value = RocDec{ .num = -unsigned_answer }, .has_overflowed = false };
        } else {
            return .{ .value = RocDec{ .num = unsigned_answer }, .has_overflowed = false };
        }
    }

    pub fn mul(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.mulWithOverflow(self, other);

        if (answer.has_overflowed) {
            roc_panic("Decimal multiplication overflowed!", 0);
            unreachable;
        } else {
            return answer.value;
        }
    }

    pub fn mulSaturated(self: RocDec, other: RocDec) RocDec {
        const answer = RocDec.mulWithOverflow(self, other);
        return answer.value;
    }

    pub fn div(self: RocDec, other: RocDec) RocDec {
        const numerator_i128 = self.num;
        const denominator_i128 = other.num;

        // (0 / n) is always 0
        if (numerator_i128 == 0) {
            return RocDec{ .num = 0 };
        }

        // (n / 0) is an error
        if (denominator_i128 == 0) {
            @panic("TODO runtime exception for dividing by 0!");
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

        const numerator_abs_i128 = math.absInt(numerator_i128) catch {
            // Currently, if you try to do multiplication on i64::MIN, panic
            // unless you're specifically multiplying by 0 or 1.
            //
            // Maybe we could support more cases in the future
            if (denominator_i128 == one_point_zero_i128) {
                return self;
            } else {
                @panic("TODO runtime exception for overflow when dividing!");
            }
        };
        const numerator_u128 = @intCast(u128, numerator_abs_i128);

        const denominator_abs_i128 = math.absInt(denominator_i128) catch {
            // Currently, if you try to do multiplication on i64::MIN, panic
            // unless you're specifically multiplying by 0 or 1.
            //
            // Maybe we could support more cases in the future
            if (numerator_i128 == one_point_zero_i128) {
                return other;
            } else {
                @panic("TODO runtime exception for overflow when dividing!");
            }
        };
        const denominator_u128 = @intCast(u128, denominator_abs_i128);

        const numerator_u256: U256 = mul_u128(numerator_u128, math.pow(u128, 10, decimal_places));
        const answer = div_u256_by_u128(numerator_u256, denominator_u128);

        var unsigned_answer: i128 = undefined;
        if (answer.hi == 0 and answer.lo <= math.maxInt(i128)) {
            unsigned_answer = @intCast(i128, answer.lo);
        } else {
            @panic("TODO runtime exception for overflow when dividing!");
        }

        return RocDec{ .num = if (is_answer_negative) -unsigned_answer else unsigned_answer };
    }
};

// A number has `k` trailling zeros if `10^k` divides into it cleanly
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
    // const _a = ea.lo;

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
        @panic("TODO runtime exception for overflow!");
    }

    // Final 512bit value is d, c, b, a
    // need to left shift 321 times
    // 315 - 256 is 59. So left shift d, c 59 times.
    return @intCast(i128, c >> 59 | (d << (128 - 59)));
}

// Multiply two 128-bit ints and divide the result by 10^DECIMAL_PLACES
//
// Adapted from https://github.com/nlordell/ethnum-rs
// Copyright (c) 2020 Nicholas Rodrigues Lordello
// Licensed under the Apache License version 2.0
//
// When translating this to Zig, we often have to use math.shr/shl instead of >>/<<
// This is because casting to the right types for Zig can be kind of tricky.
// See https://github.com/ziglang/zig/issues/7605
fn div_u256_by_u128(numer: U256, denom: u128) U256 {
    const N_UDWORD_BITS: u8 = 128;
    const N_UTWORD_BITS: u9 = 256;

    var q: U256 = undefined;
    var r: U256 = undefined;
    var sr: u8 = undefined;

    // special case
    if (numer.hi == 0) {
        // 0 X
        // ---
        // 0 X
        return .{
            .hi = 0,
            .lo = numer.lo / denom,
        };
    }

    // numer.hi != 0
    if (denom == 0) {
        // K X
        // ---
        // 0 0
        return .{
            .hi = 0,
            .lo = numer.hi / denom,
        };
    } else {
        // K X
        // ---
        // 0 K
        // NOTE: Modified from `if (d.low() & (d.low() - 1)) == 0`.
        if (math.isPowerOfTwo(denom)) {
            // if d is a power of 2
            if (denom == 1) {
                return numer;
            }

            sr = @ctz(u128, denom);

            return .{
                .hi = math.shr(u128, numer.hi, sr),
                .lo = math.shl(u128, numer.hi, N_UDWORD_BITS - sr) | math.shr(u128, numer.lo, sr),
            };
        }

        // K X
        // ---
        // 0 K
        var denom_leading_zeros = @clz(u128, denom);
        var numer_hi_leading_zeros = @clz(u128, numer.hi);
        sr = 1 + N_UDWORD_BITS + denom_leading_zeros - numer_hi_leading_zeros;
        // 2 <= sr <= N_UTWORD_BITS - 1
        // q.all = n.all << (N_UTWORD_BITS - sr);
        // r.all = n.all >> sr;
        // #[allow(clippy::comparison_chain)]
        if (sr == N_UDWORD_BITS) {
            q = .{
                .hi = numer.lo,
                .lo = 0,
            };
            r = .{
                .hi = 0,
                .lo = numer.hi,
            };
        } else if (sr < N_UDWORD_BITS) {
            // 2 <= sr <= N_UDWORD_BITS - 1
            q = .{
                .hi = math.shl(u128, numer.lo, N_UDWORD_BITS - sr),
                .lo = 0,
            };
            r = .{
                .hi = math.shr(u128, numer.hi, sr),
                .lo = math.shl(u128, numer.hi, N_UDWORD_BITS - sr) | math.shr(u128, numer.lo, sr),
            };
        } else {
            // N_UDWORD_BITS + 1 <= sr <= N_UTWORD_BITS - 1
            q = .{
                .hi = math.shl(u128, numer.hi, N_UTWORD_BITS - sr) | math.shr(u128, numer.lo, sr - N_UDWORD_BITS),
                .lo = math.shl(u128, numer.lo, N_UTWORD_BITS - sr),
            };
            r = .{
                .hi = 0,
                .lo = math.shr(u128, numer.hi, sr - N_UDWORD_BITS),
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

        var lo: u128 = undefined;
        var lo_overflowed: bool = undefined;
        var hi: u128 = undefined;

        lo_overflowed = @subWithOverflow(u128, denom, r.lo, &lo);
        hi = 0 -% @intCast(u128, @bitCast(u1, lo_overflowed)) -% r.hi;

        lo_overflowed = @subWithOverflow(u128, lo, 1, &lo);
        hi = hi -% @intCast(u128, @bitCast(u1, lo_overflowed));

        // TODO this U256 was originally created by:
        //
        // ((hi as i128) >> 127).as_u256()
        //
        // ...however, I can't figure out where that function is defined.
        // Maybe it's defined using a macro or something. Anyway, hopefully
        // this is what it would do in this scenario.
        var s = .{
            .hi = 0,
            .lo = math.shr(u128, hi, 127),
        };

        carry = s.lo & 1;

        // var (lo, carry) = r.lo.overflowing_sub(denom & s.lo);
        lo_overflowed = @subWithOverflow(u128, r.lo, (denom & s.lo), &lo);
        hi = r.hi -% @intCast(u128, @bitCast(u1, lo_overflowed));

        r = .{ .hi = hi, .lo = lo };

        sr -= 1;
    }

    var hi = (q.hi << 1) | (q.lo >> (127));
    var lo = (q.lo << 1) | carry;

    return .{ .hi = hi, .lo = lo };
}

const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expectEqualSlices = testing.expectEqualSlices;
const expect = testing.expect;

test "fromU64" {
    var dec = RocDec.fromU64(25);

    try expectEqual(RocDec{ .num = 25000000000000000000 }, dec);
}

test "fromF64" {
    var dec = RocDec.fromF64(25.5);
    try expectEqual(RocDec{ .num = 25500000000000000000 }, dec.?);
}

test "fromF64 overflow" {
    var dec = RocDec.fromF64(1e308);
    try expectEqual(dec, null);
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
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: -123.45" {
    var dec: RocDec = .{ .num = -123450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-123.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 123.0" {
    var dec: RocDec = .{ .num = 123000000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "123.0"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: -123.0" {
    var dec: RocDec = .{ .num = -123000000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-123.0"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 0.45" {
    var dec: RocDec = .{ .num = 450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "0.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: -0.45" {
    var dec: RocDec = .{ .num = -450000000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-0.45"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 0.00045" {
    var dec: RocDec = .{ .num = 000450000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "0.00045"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: -0.00045" {
    var dec: RocDec = .{ .num = -000450000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-0.00045"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: -111.123456" {
    var dec: RocDec = .{ .num = -111123456000000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "-111.123456"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 123.1111111" {
    var dec: RocDec = .{ .num = 123111111100000000000 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "123.1111111"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 123.1111111111111 (big str)" {
    var dec: RocDec = .{ .num = 123111111111111000000 };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.deinit();
    defer res_roc_str.deinit();

    const res_slice: []const u8 = "123.111111111111"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 123.111111111111444444 (max number of decimal places)" {
    var dec: RocDec = .{ .num = 123111111111111444444 };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.deinit();
    defer res_roc_str.deinit();

    const res_slice: []const u8 = "123.111111111111444444"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 12345678912345678912.111111111111111111 (max number of digits)" {
    var dec: RocDec = .{ .num = 12345678912345678912111111111111111111 };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.deinit();
    defer res_roc_str.deinit();

    const res_slice: []const u8 = "12345678912345678912.111111111111111111"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: std.math.maxInt" {
    var dec: RocDec = .{ .num = std.math.maxInt(i128) };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.deinit();
    defer res_roc_str.deinit();

    const res_slice: []const u8 = "170141183460469231731.687303715884105727"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: std.math.minInt" {
    var dec: RocDec = .{ .num = std.math.minInt(i128) };
    var res_roc_str = dec.toStr();
    errdefer res_roc_str.deinit();
    defer res_roc_str.deinit();

    const res_slice: []const u8 = "-170141183460469231731.687303715884105728"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "toStr: 0" {
    var dec: RocDec = .{ .num = 0 };
    var res_roc_str = dec.toStr();

    const res_slice: []const u8 = "0.0"[0..];
    try expectEqualSlices(u8, res_slice, res_roc_str.asSlice());
}

test "add: 0" {
    var dec: RocDec = .{ .num = 0 };

    try expectEqual(RocDec{ .num = 0 }, dec.add(.{ .num = 0 }));
}

test "add: 1" {
    var dec: RocDec = .{ .num = 0 };

    try expectEqual(RocDec{ .num = 1 }, dec.add(.{ .num = 1 }));
}

test "sub: 0" {
    var dec: RocDec = .{ .num = 1 };

    try expectEqual(RocDec{ .num = 1 }, dec.sub(.{ .num = 0 }));
}

test "sub: 1" {
    var dec: RocDec = .{ .num = 1 };

    try expectEqual(RocDec{ .num = 0 }, dec.sub(.{ .num = 1 }));
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

test "div: 0 / 2" {
    var dec: RocDec = RocDec.fromU64(0);

    try expectEqual(RocDec.fromU64(0), dec.div(RocDec.fromU64(2)));
}

test "div: 2 / 2" {
    var dec: RocDec = RocDec.fromU64(2);

    try expectEqual(RocDec.fromU64(1), dec.div(RocDec.fromU64(2)));
}

test "div: 20 / 2" {
    var dec: RocDec = RocDec.fromU64(20);

    try expectEqual(RocDec.fromU64(10), dec.div(RocDec.fromU64(2)));
}

test "div: 8 / 5" {
    var dec: RocDec = RocDec.fromU64(8);
    var res: RocDec = RocDec.fromStr(RocStr.init("1.6", 3)).?;
    try expectEqual(res, dec.div(RocDec.fromU64(5)));
}

test "div: 10 / 3" {
    var numer: RocDec = RocDec.fromU64(10);
    var denom: RocDec = RocDec.fromU64(3);

    var roc_str = RocStr.init("3.333333333333333333", 20);
    errdefer roc_str.deinit();
    defer roc_str.deinit();

    var res: RocDec = RocDec.fromStr(roc_str).?;

    try expectEqual(res, numer.div(denom));
}

// exports

pub fn fromStr(arg: RocStr) callconv(.C) num_.NumParseResult(i128) {
    if (@call(.{ .modifier = always_inline }, RocDec.fromStr, .{arg})) |dec| {
        return .{ .errorcode = 0, .value = dec.num };
    } else {
        return .{ .errorcode = 1, .value = 0 };
    }
}

pub fn toStr(arg: RocDec) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, RocDec.toStr, .{arg});
}

pub fn fromF64C(arg: f64) callconv(.C) i128 {
    return if (@call(.{ .modifier = always_inline }, RocDec.fromF64, .{arg})) |dec| dec.num else @panic("TODO runtime exception failing convert f64 to RocDec");
}

pub fn eqC(arg1: RocDec, arg2: RocDec) callconv(.C) bool {
    return @call(.{ .modifier = always_inline }, RocDec.eq, .{ arg1, arg2 });
}

pub fn neqC(arg1: RocDec, arg2: RocDec) callconv(.C) bool {
    return @call(.{ .modifier = always_inline }, RocDec.neq, .{ arg1, arg2 });
}

pub fn negateC(arg: RocDec) callconv(.C) i128 {
    return if (@call(.{ .modifier = always_inline }, RocDec.negate, .{arg})) |dec| dec.num else @panic("TODO overflow for negating RocDec");
}

pub fn addC(arg1: RocDec, arg2: RocDec) callconv(.C) WithOverflow(RocDec) {
    return @call(.{ .modifier = always_inline }, RocDec.addWithOverflow, .{ arg1, arg2 });
}

pub fn subC(arg1: RocDec, arg2: RocDec) callconv(.C) WithOverflow(RocDec) {
    return @call(.{ .modifier = always_inline }, RocDec.subWithOverflow, .{ arg1, arg2 });
}

pub fn mulC(arg1: RocDec, arg2: RocDec) callconv(.C) WithOverflow(RocDec) {
    return @call(.{ .modifier = always_inline }, RocDec.mulWithOverflow, .{ arg1, arg2 });
}

pub fn divC(arg1: RocDec, arg2: RocDec) callconv(.C) i128 {
    return @call(.{ .modifier = always_inline }, RocDec.div, .{ arg1, arg2 }).num;
}

pub fn addOrPanicC(arg1: RocDec, arg2: RocDec) callconv(.C) RocDec {
    return @call(.{ .modifier = always_inline }, RocDec.add, .{ arg1, arg2 });
}

pub fn addSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.C) RocDec {
    return @call(.{ .modifier = always_inline }, RocDec.addSaturated, .{ arg1, arg2 });
}

pub fn subOrPanicC(arg1: RocDec, arg2: RocDec) callconv(.C) RocDec {
    return @call(.{ .modifier = always_inline }, RocDec.sub, .{ arg1, arg2 });
}

pub fn subSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.C) RocDec {
    return @call(.{ .modifier = always_inline }, RocDec.subSaturated, .{ arg1, arg2 });
}

pub fn mulOrPanicC(arg1: RocDec, arg2: RocDec) callconv(.C) RocDec {
    return @call(.{ .modifier = always_inline }, RocDec.mul, .{ arg1, arg2 });
}

pub fn mulSaturatedC(arg1: RocDec, arg2: RocDec) callconv(.C) RocDec {
    return @call(.{ .modifier = always_inline }, RocDec.mulSaturated, .{ arg1, arg2 });
}
