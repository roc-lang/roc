const std = @import("std");

pub const Target = struct {
    target_usize: TargetUsize,

    pub const native: Target = .{
        .target_usize = TargetUsize.native,
    };
};

/// How big is `usize` on a given target?
pub const TargetUsize = enum(u1) {
    /// Roc only supports compiling to 32-bit and 64-bit targets
    u32 = 0,
    u64 = 1,

    /// The size of the target usize, in bytes
    pub fn size(self: @This()) u32 {
        return @as(u32, @intCast(self.alignment().toByteUnits()));
    }

    /// The alignment of the target usize
    pub fn alignment(self: @This()) std.mem.Alignment {
        // u32 has alignment 4 (log2(4) = 2), u64 has alignment 8 (log2(8) = 3)
        const log2_value = @as(std.math.Log2Int(usize), @intFromEnum(self)) + 2;
        return @enumFromInt(log2_value);
    }

    /// The usize for the native target (that is, the currently-running machine)
    pub const native: TargetUsize = switch (@alignOf(usize)) {
        4 => .u32,
        8 => .u64,
        else => {
            // The Roc compiler can only be built for 32-bit and 64-bit targets.
            unreachable;
        },
    };

    /// Returns an array of all TargetUsize variants that Roc supports
    pub fn all() [2]TargetUsize {
        return .{ .u32, .u64 };
    }
};

test "TargetUsize conversion to usize" {
    try std.testing.expectEqual(TargetUsize.u32.size(), 4);
    try std.testing.expectEqual(TargetUsize.u64.size(), 8);
}
