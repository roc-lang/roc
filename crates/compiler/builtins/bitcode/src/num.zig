const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const math = std.math;
const RocList = @import("list.zig").RocList;
const RocStr = @import("str.zig").RocStr;
const WithOverflow = @import("utils.zig").WithOverflow;
const Ordering = @import("utils.zig").Ordering;
const roc_panic = @import("panic.zig").panic_help;

pub fn NumParseResult(comptime T: type) type {
    // on the roc side we sort by alignment; putting the errorcode last
    // always works out (no number with smaller alignment than 1)
    return extern struct {
        value: T,
        errorcode: u8, // 0 indicates success
    };
}

pub const U256 = struct {
    hi: u128,
    lo: u128,
};

pub fn mul_u128(a: u128, b: u128) U256 {
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

pub fn exportParseInt(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(buf: RocStr) callconv(.C) NumParseResult(T) {
            // a radix of 0 will make zig determine the radix from the frefix:
            //  * A prefix of "0b" implies radix=2,
            //  * A prefix of "0o" implies radix=8,
            //  * A prefix of "0x" implies radix=16,
            //  * Otherwise radix=10 is assumed.
            const radix = 0;
            if (std.fmt.parseInt(T, buf.asSlice(), radix)) |success| {
                return .{ .errorcode = 0, .value = success };
            } else |_| {
                return .{ .errorcode = 1, .value = 0 };
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportParseFloat(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(buf: RocStr) callconv(.C) NumParseResult(T) {
            if (std.fmt.parseFloat(T, buf.asSlice())) |success| {
                return .{ .errorcode = 0, .value = success };
            } else |_| {
                return .{ .errorcode = 1, .value = 0 };
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportPow(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(base: T, exp: T) callconv(.C) T {
            return std.math.pow(T, base, exp);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportIsNan(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) bool {
            return std.math.isNan(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportIsInfinite(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) bool {
            return std.math.isInf(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportIsFinite(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) bool {
            return std.math.isFinite(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAsin(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.asin(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAcos(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.acos(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAtan(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.atan(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportSin(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return math.sin(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportCos(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return math.cos(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportTan(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return math.tan(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportLog(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return math.ln(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportFAbs(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return math.absFloat(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportSqrt(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return math.sqrt(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportRound(comptime F: type, comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: F) callconv(.C) T {
            return @floatToInt(T, (math.round(input)));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportFloor(comptime F: type, comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: F) callconv(.C) T {
            return @floatToInt(T, (math.floor(input)));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportCeiling(comptime F: type, comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: F) callconv(.C) T {
            return @floatToInt(T, (math.ceil(input)));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportDivCeil(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(a: T, b: T) callconv(.C) T {
            return math.divCeil(T, a, b) catch @panic("TODO runtime exception for dividing by 0!");
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn ToIntCheckedResult(comptime T: type) type {
    // On the Roc side we sort by alignment; putting the errorcode last
    // always works out (no number with smaller alignment than 1).
    return extern struct {
        value: T,
        out_of_bounds: bool,
    };
}

pub fn exportToIntCheckingMax(comptime From: type, comptime To: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: From) callconv(.C) ToIntCheckedResult(To) {
            if (input > std.math.maxInt(To)) {
                return .{ .out_of_bounds = true, .value = 0 };
            }
            return .{ .out_of_bounds = false, .value = @intCast(To, input) };
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(From), .linkage = .Strong });
}

pub fn exportToIntCheckingMaxAndMin(comptime From: type, comptime To: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: From) callconv(.C) ToIntCheckedResult(To) {
            if (input > std.math.maxInt(To) or input < std.math.minInt(To)) {
                return .{ .out_of_bounds = true, .value = 0 };
            }
            return .{ .out_of_bounds = false, .value = @intCast(To, input) };
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(From), .linkage = .Strong });
}

pub fn bytesToU16C(arg: RocList, position: usize) callconv(.C) u16 {
    return @call(.{ .modifier = always_inline }, bytesToU16, .{ arg, position });
}

fn bytesToU16(arg: RocList, position: usize) u16 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u16, [_]u8{ bytes[position], bytes[position + 1] });
}

pub fn bytesToU32C(arg: RocList, position: usize) callconv(.C) u32 {
    return @call(.{ .modifier = always_inline }, bytesToU32, .{ arg, position });
}

fn bytesToU32(arg: RocList, position: usize) u32 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u32, [_]u8{ bytes[position], bytes[position + 1], bytes[position + 2], bytes[position + 3] });
}

pub fn bytesToU64C(arg: RocList, position: usize) callconv(.C) u64 {
    return @call(.{ .modifier = always_inline }, bytesToU64, .{ arg, position });
}

fn bytesToU64(arg: RocList, position: usize) u64 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u64, [_]u8{ bytes[position], bytes[position + 1], bytes[position + 2], bytes[position + 3], bytes[position + 4], bytes[position + 5], bytes[position + 6], bytes[position + 7] });
}

pub fn bytesToU128C(arg: RocList, position: usize) callconv(.C) u128 {
    return @call(.{ .modifier = always_inline }, bytesToU128, .{ arg, position });
}

fn bytesToU128(arg: RocList, position: usize) u128 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u128, [_]u8{ bytes[position], bytes[position + 1], bytes[position + 2], bytes[position + 3], bytes[position + 4], bytes[position + 5], bytes[position + 6], bytes[position + 7], bytes[position + 8], bytes[position + 9], bytes[position + 10], bytes[position + 11], bytes[position + 12], bytes[position + 13], bytes[position + 14], bytes[position + 15] });
}

fn isMultipleOf(comptime T: type, lhs: T, rhs: T) bool {
    if (rhs == 0 or rhs == -1) {
        // lhs is a multiple of rhs iff
        //
        // - rhs == -1
        // - both rhs and lhs are 0
        //
        // the -1 case is important for overflow reasons `isize::MIN % -1` crashes in rust
        return (rhs == -1) or (lhs == 0);
    } else {
        const rem = @mod(lhs, rhs);
        return rem == 0;
    }
}

pub fn exportIsMultipleOf(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) bool {
            return @call(.{ .modifier = always_inline }, isMultipleOf, .{ T, self, other });
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

fn addWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    switch (@typeInfo(T)) {
        .Int => {
            var answer: T = undefined;
            const overflowed = @addWithOverflow(T, self, other, &answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
        else => {
            const answer = self + other;
            const overflowed = !std.math.isFinite(answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
    }
}

pub fn exportAddWithOverflow(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) WithOverflow(T) {
            return @call(.{ .modifier = always_inline }, addWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAddSaturatedInt(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = addWithOverflow(T, self, other);
            if (result.has_overflowed) {
                // We can unambiguously tell which way it wrapped, because we have N+1 bits including the overflow bit
                if (result.value >= 0 and @typeInfo(T).Int.signedness == .signed) {
                    return std.math.minInt(T);
                } else {
                    return std.math.maxInt(T);
                }
            } else {
                return result.value;
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAddOrPanic(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = addWithOverflow(T, self, other);
            if (result.has_overflowed) {
                roc_panic("integer addition overflowed!", 0);
                unreachable;
            } else {
                return result.value;
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

fn subWithOverflow(comptime T: type, self: T, other: T) WithOverflow(T) {
    switch (@typeInfo(T)) {
        .Int => {
            var answer: T = undefined;
            const overflowed = @subWithOverflow(T, self, other, &answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
        else => {
            const answer = self - other;
            const overflowed = !std.math.isFinite(answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
    }
}

pub fn exportSubWithOverflow(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) WithOverflow(T) {
            return @call(.{ .modifier = always_inline }, subWithOverflow, .{ T, self, other });
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportSubSaturatedInt(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = subWithOverflow(T, self, other);
            if (result.has_overflowed) {
                if (@typeInfo(T).Int.signedness == .unsigned) {
                    return 0;
                } else if (self < 0) {
                    return std.math.minInt(T);
                } else {
                    return std.math.maxInt(T);
                }
            } else {
                return result.value;
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportSubOrPanic(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = subWithOverflow(T, self, other);
            if (result.has_overflowed) {
                roc_panic("integer subtraction overflowed!", 0);
                unreachable;
            } else {
                return result.value;
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

fn mulWithOverflow(comptime T: type, comptime W: type, self: T, other: T) WithOverflow(T) {
    switch (@typeInfo(T)) {
        .Int => {
            if (T == i128) {
                const is_answer_negative = (self < 0) != (other < 0);
                const max = std.math.maxInt(i128);
                const min = std.math.minInt(i128);

                const self_u128 = @intCast(u128, math.absInt(self) catch {
                    if (other == 0) {
                        return .{ .value = 0, .has_overflowed = false };
                    } else if (other == 1) {
                        return .{ .value = self, .has_overflowed = false };
                    } else if (is_answer_negative) {
                        return .{ .value = min, .has_overflowed = true };
                    } else {
                        return .{ .value = max, .has_overflowed = true };
                    }
                });

                const other_u128 = @intCast(u128, math.absInt(other) catch {
                    if (self == 0) {
                        return .{ .value = 0, .has_overflowed = false };
                    } else if (self == 1) {
                        return .{ .value = other, .has_overflowed = false };
                    } else if (is_answer_negative) {
                        return .{ .value = min, .has_overflowed = true };
                    } else {
                        return .{ .value = max, .has_overflowed = true };
                    }
                });

                const answer256: U256 = mul_u128(self_u128, other_u128);

                if (is_answer_negative) {
                    if (answer256.hi != 0 or answer256.lo > (1 << 127)) {
                        return .{ .value = min, .has_overflowed = true };
                    } else if (answer256.lo == (1 << 127)) {
                        return .{ .value = min, .has_overflowed = false };
                    } else {
                        return .{ .value = -@intCast(i128, answer256.lo), .has_overflowed = false };
                    }
                } else {
                    if (answer256.hi != 0 or answer256.lo > @intCast(u128, max)) {
                        return .{ .value = max, .has_overflowed = true };
                    } else {
                        return .{ .value = @intCast(i128, answer256.lo), .has_overflowed = false };
                    }
                }
            } else {
                const self_wide: W = self;
                const other_wide: W = other;
                const answer: W = self_wide * other_wide;

                const max: W = std.math.maxInt(T);
                const min: W = std.math.minInt(T);

                if (answer > max) {
                    return .{ .value = max, .has_overflowed = true };
                } else if (answer < min) {
                    return .{ .value = min, .has_overflowed = true };
                } else {
                    return .{ .value = @intCast(T, answer), .has_overflowed = false };
                }
            }
        },
        else => {
            const answer = self * other;
            const overflowed = !std.math.isFinite(answer);
            return .{ .value = answer, .has_overflowed = overflowed };
        },
    }
}

pub fn exportMulWithOverflow(comptime T: type, comptime W: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) WithOverflow(T) {
            return @call(.{ .modifier = always_inline }, mulWithOverflow, .{ T, W, self, other });
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportMulSaturatedInt(comptime T: type, comptime W: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = @call(.{ .modifier = always_inline }, mulWithOverflow, .{ T, W, self, other });
            return result.value;
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportMulWrappedInt(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            return self *% other;
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn shiftRightZeroFillI128(self: i128, other: u8) callconv(.C) i128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        return self >> @intCast(u7, other);
    }
}

pub fn shiftRightZeroFillU128(self: u128, other: u8) callconv(.C) u128 {
    if (other & 0b1000_0000 > 0) {
        return 0;
    } else {
        return self >> @intCast(u7, other);
    }
}

pub fn compareI128(self: i128, other: i128) callconv(.C) Ordering {
    if (self == other) {
        return Ordering.EQ;
    } else if (self < other) {
        return Ordering.LT;
    } else {
        return Ordering.GT;
    }
}

pub fn compareU128(self: u128, other: u128) callconv(.C) Ordering {
    if (self == other) {
        return Ordering.EQ;
    } else if (self < other) {
        return Ordering.LT;
    } else {
        return Ordering.GT;
    }
}

pub fn lessThanI128(self: i128, other: i128) callconv(.C) bool {
    return self < other;
}

pub fn lessThanOrEqualI128(self: i128, other: i128) callconv(.C) bool {
    return self <= other;
}

pub fn greaterThanI128(self: i128, other: i128) callconv(.C) bool {
    return self > other;
}

pub fn greaterThanOrEqualI128(self: i128, other: i128) callconv(.C) bool {
    return self >= other;
}

pub fn lessThanU128(self: u128, other: u128) callconv(.C) bool {
    return self < other;
}

pub fn lessThanOrEqualU128(self: u128, other: u128) callconv(.C) bool {
    return self <= other;
}

pub fn greaterThanU128(self: u128, other: u128) callconv(.C) bool {
    return self > other;
}

pub fn greaterThanOrEqualU128(self: u128, other: u128) callconv(.C) bool {
    return self >= other;
}

pub fn exportMulOrPanic(comptime T: type, comptime W: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T, other: T) callconv(.C) T {
            const result = @call(.{ .modifier = always_inline }, mulWithOverflow, .{ T, W, self, other });
            if (result.has_overflowed) {
                roc_panic("integer multiplication overflowed!", 0);
                unreachable;
            } else {
                return result.value;
            }
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportCountLeadingZeroBits(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T) callconv(.C) usize {
            return @as(usize, @clz(T, self));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportCountTrailingZeroBits(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T) callconv(.C) usize {
            return @as(usize, @ctz(T, self));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportCountOneBits(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(self: T) callconv(.C) usize {
            return @as(usize, @popCount(T, self));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}
