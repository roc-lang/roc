const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;

pub const RocDict = extern struct {
    dict_bytes: ?[*]u8,
    dict_len: usize,

    pub fn init() RocDict {
        return RocDict{ .dict_len = 0, .dict_bytes = null };
    }

    pub fn contains(self: RocDict, key_size: usize, key_ptr: *const c_void, hash_code: u64) bool {
        return false;
    }
};

// Dict.empty
pub fn dictEmpty() callconv(.C) RocDict {
    return RocDict.init();
}

// Dict.len
pub fn dictLen(dict: RocDict) callconv(.C) usize {
    return dict.dict_len;
}

test "RocDict.init() contains nothing" {
    const dict = RocDict.init();

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));
}
