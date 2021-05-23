const std = @import("std");

const math = std.math;

pub const RocDec = struct {
    num: i128,

    pub const decimal_places: u32 = 18;

    pub const min: RocDec = .{ .num = math.minInt(i128) };
    pub const max: RocDec = .{ .num = math.maxInt(i128) };

    pub const one_point_zero: RocDec = .{ .num = comptime math.pow(i128, 10, RocDec.decimal_places) };

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
    // This can't overflow because the intial numbers are only 127bit due to removing the sign bit.
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

const one_e20: i256 = 100000000000000000000;

const expectEqual = std.testing.expectEqual;

test "add" {
    const dec: RocDec = .{ .num = 0 };

    expectEqual(RocDec{ .num = 0 }, dec.add(.{ .num = 0 }));
}

test "mul" {
    var dec1: RocDec = .{ .num = 0 };

    expectEqual(RocDec{ .num = 0 }, dec1.mul(.{ .num = 0 }));

    var dec2: RocDec = .{ .num = 100000000000000000000 };

    expectEqual(RocDec{ .num = 100000000000000000000 }, dec2.mul(.{ .num = 100000000000000000000 }));
}
