
pub const InternId = union {
    small: [4]u8,
    big: IndexAndLen,

    pub fn is_inline(self: @This()) bool {
        // If the first byte is in the ASCII range (under 128), this is small and inline.
        return @as([4]u8, @bitCast(self))[0] <= @as(u8, @intCast(std.math.maxInt(i8)));
    }

    pub fn as_small(self: @This()) [4]u8 {
        std.debug.assert(self.is_inline());
        return self.small;
    }

    fn new_small(bytes: [4]u8) InternId {
        return InternId{ .small = bytes };
    }

    fn new_big(index_and_len: IndexAndLen) InternId {
        return InternId{ .big = index_and_len };
    }
};

const IndexAndLen = struct {
    is_small: bool,
    len_category: LenCategory,
    len: u29,
};

const LenCategory = enum {
    FiveToEight,
    NineToSixteen,
    OverSixteen,
};

const Bucket = struct {
    capacity: u32,
    len: u32,
    elements: [*]T,

    pub fn init(comptime T: type) Bucket {
        return .{
            .capacity = 0,
            .len = 0,
            .elements = undefined,
        };
    }
};
