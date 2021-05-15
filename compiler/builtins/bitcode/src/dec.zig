const std = @import("std");

pub const RocDec = struct {
    num: i128,

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
        const self_i256 = @intCast(i256, self.num);
        const other_i256 = @intCast(i256, other.num);
        const answer = 0; //self_i256 * other_i256;

        if ((answer >> 192) < 0) {
            return RocDec{ .num = 0 };
            // return RocDec{ .num = @intCast(i128, @divTrunc(answer, one_e20)) };
        } else {
            return RocDec{ .num = 0 };
           // std.debug.panic("TODO runtime exception for overflow!", .{});
        }
    }
};

const one_e20: i256 = 100000000000000000000;

const expectEqual = std.testing.expectEqual;

test "add" {
    expectEqual(
        RocDec { .num = 0 },
        (RocDec{ .num = 0 }).add(RocDec{ .num = 0 })
    );
}

test "mul" {
    expectEqual(
        RocDec { .num = 0 },
        (RocDec{ .num = 0 }).mul(RocDec{ .num = 0 })
    );

    expectEqual(
        RocDec { .num = 100000000000000000000 },
        (RocDec{ .num = 100000000000000000000 }).mul(RocDec{ .num = 100000000000000000000 })
    );

    // expectEqual(
    //     (RocDec{ .num = 1 }).add(RocDec{ .num = 1 }),
    //     RocDec { .num = 0 }
    // );
}
