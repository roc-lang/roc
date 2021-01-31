const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const mem = std.mem;
const Allocator = mem.Allocator;

const level_size = 32;

const InPlace = packed enum(u8) {
    InPlace,
    Clone,
};

const Slot = packed enum(u8) {
    Empty,
    Filled,
    PreviouslyFilled,
};

pub const RocDict = extern struct {
    dict_bytes: ?[*]u8,
    dict_slot_len: usize,
    dict_entries_len: usize,

    pub fn init(allocator: *Allocator, bytes_ptr: [*]const u8, number_of_slots: usize, number_of_entries: usize, key_size: usize, value_size: usize) RocDict {
        var result = RocDict.allocate(
            allocator,
            InPlace.Clone,
            number_of_slots,
            number_of_entries,
            key_size,
            value_size,
        );

        @memcpy(result.asU8ptr(), bytes_ptr, number_of_slots);

        return result;
    }

    pub fn deinit(self: RocDict, allocator: *Allocator) void {
        const dict_bytes_ptr: [*]u8 = self.dict_bytes orelse unreachable;
        const dict_bytes: []u8 = dict_bytes_ptr[0..self.dict_slot_len];
        allocator.free(dict_bytes);
    }

    pub fn allocate(
        allocator: *Allocator,
        result_in_place: InPlace,
        number_of_slots: usize,
        number_of_entries: usize,
        key_size: usize,
        value_size: usize,
    ) RocDict {
        const slot_size = @sizeOf(Slot) + key_size + value_size;

        const length = @sizeOf(usize) + (number_of_slots * slot_size);

        var new_bytes: []usize = allocator.alloc(usize, length) catch unreachable;

        if (result_in_place == InPlace.InPlace) {
            new_bytes[0] = @intCast(usize, number_of_slots);
        } else {
            const v: isize = std.math.minInt(isize);
            new_bytes[0] = @bitCast(usize, v);
        }

        var first_slot = @ptrCast([*]align(@alignOf(usize)) u8, new_bytes);
        first_slot += @sizeOf(usize);

        return RocDict{
            .dict_bytes = first_slot,
            .dict_slot_len = number_of_slots,
            .dict_entries_len = number_of_entries,
        };
    }

    pub fn asU8ptr(self: RocDict) [*]u8 {
        return @ptrCast([*]u8, self.dict_bytes);
    }

    pub fn contains(self: RocDict, key_size: usize, key_ptr: *const c_void, hash_code: u64) bool {
        return false;
    }

    pub fn len(self: RocDict) usize {
        return self.dict_entries_len;
    }

    pub fn isEmpty(self: RocDict) bool {
        return self.len() == 0;
    }

    pub fn clone(self: RocDict, allocator: *Allocator, in_place: InPlace, key_size: usize, value_size: usize) RocDict {
        var new_dict = RocDict.init(allocator, self.dict_slot_len, self.dict_entries_len, key_size, value_size);

        var old_bytes: [*]u8 = @ptrCast([*]u8, self.dict_bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_dict.dict_bytes);

        @memcpy(new_bytes, old_bytes, self.dict_slot_len);

        return new_dict;
    }
    // pub fn insert(self: RocDict, key_size: usize, key_ptr: *const c_void, value_ptr: *const c_void)
};

// Dict.empty
pub fn dictEmpty(allocator: *Allocator, key_size: usize, value_size: usize) callconv(.C) RocDict {
    return RocDict.init(allocator, "", 0, 0, key_size, value_size);
}

// Dict.len
pub fn dictLen(dict: RocDict) callconv(.C) usize {
    return dict.dict_len;
}

test "RocDict.init() contains nothing" {
    const key_size = @sizeOf(usize);
    const value_size = @sizeOf(usize);

    const dict = dictEmpty(testing.allocator, key_size, value_size);

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));

    dict.deinit(testing.allocator);
}
