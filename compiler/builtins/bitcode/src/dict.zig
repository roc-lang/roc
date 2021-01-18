const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;

pub const RocDict = extern struct {
    size: usize,

    pub fn init() RocDict {
        return RocDict{
            .len = 0
        };
    }

    pub fn contains(self: RocDict, key_size: usize, key_ptr: *const c_void, hash_code: u64) bool {
        return false;
    }
};

// Dict.size
pub fn dictSize(dict: RocDict) callconv(.C) usize {
    return dict.size;
}

test "RocDict.init() contains nothing" {
    const dict = RocDict.init();

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));
}