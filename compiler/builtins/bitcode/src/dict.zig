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

    pub fn empty() RocDict {
        return RocDict{
            .dict_entries_len = 0,
            .dict_slot_len = 0,
            .dict_bytes = null,
        };
    }

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

    pub fn deinit(self: RocDict, allocator: *Allocator, key_size: usize, value_size: usize) void {
        if (!self.isEmpty()) {
            const slot_size = slotSize(key_size, value_size);

            const dict_bytes_ptr: [*]u8 = self.dict_bytes orelse unreachable;

            const dict_bytes: []u8 = dict_bytes_ptr[0..(self.dict_slot_len)];
            allocator.free(dict_bytes);
        }
    }

    pub fn allocate(
        allocator: *Allocator,
        result_in_place: InPlace,
        number_of_slots: usize,
        number_of_entries: usize,
        key_size: usize,
        value_size: usize,
    ) RocDict {
        const slot_size = slotSize(key_size, value_size);

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
};

// Dict.empty
pub fn dictEmpty() callconv(.C) RocDict {
    return RocDict.empty();
}

pub fn slotSize(key_size: usize, value_size: usize) usize {
    return @sizeOf(Slot) + key_size + value_size;
}

// Dict.len
pub fn dictLen(dict: RocDict) callconv(.C) usize {
    return dict.dict_entries_len;
}

// Dict.insert : Dict k v, k, v -> Dict k v
const Opaque = ?[*]u8;
const HashFn = fn (u64, ?[*]u8) callconv(.C) u64;
const EqFn = fn (?[*]u8, ?[*]u8) callconv(.C) bool;
const Dec = fn (?[*]u8) callconv(.C) void;
pub fn dictInsert(dict: RocDict, alignment: usize, key: Opaque, key_width: usize, value: Opaque, value_width: usize, hash: HashFn, is_eq: EqFn, dec_value: Dec, result: *RocDict) callconv(.C) void {
    result.* = RocDict.empty();

    return;
}

test "RocDict.init() contains nothing" {
    const key_size = @sizeOf(usize);
    const value_size = @sizeOf(usize);

    const dict = dictEmpty();

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));
}
