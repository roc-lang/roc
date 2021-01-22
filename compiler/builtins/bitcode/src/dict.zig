const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;

pub const RocDict = extern struct {
    len: usize,

    pub fn init() RocDict {
        return RocDict{
            .len = 0
        };
    }

    pub fn contains(self: RocDict, key_size: usize, key_ptr: *const c_void, hash_code: u64) bool {
        return false;
    }
};

// Dict.len
pub fn dictLen(dict: RocDict) callconv(.C) usize {
    return dict.len;
}

test "RocDict.init() contains nothing" {
    const dict = RocDict.init();

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));
}