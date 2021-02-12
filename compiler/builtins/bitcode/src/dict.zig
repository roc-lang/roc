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

const MaybeIndexTag = enum {
    index, not_found
};

const MaybeIndex = union(MaybeIndexTag) {
    index: usize, not_found: void
};

// aligmnent of elements. The number (16 or 8) indicates the maximum
// alignment of the key and value. The tag furthermore indicates
// which has the biggest aligmnent. If both are the same, we put
// the key first
const Alignment = packed enum(u8) {
    Align16KeyFirst,
    Align16ValueFirst,
    Align8KeyFirst,
    Align8ValueFirst,

    fn toUsize(self: Alignment) usize {
        switch (self) {
            .Align16KeyFirst => return 16,
            .Align16ValueFirst => return 16,
            .Align8KeyFirst => return 8,
            .Align8ValueFirst => return 8,
        }
    }

    fn keyFirst(self: Alignment) bool {
        switch (self) {
            .Align16KeyFirst => return true,
            .Align16ValueFirst => return false,
            .Align8KeyFirst => return true,
            .Align8ValueFirst => return false,
        }
    }
};

pub const RocDict = extern struct {
    dict_bytes: ?[*]u8,
    dict_entries_len: usize,
    dict_slot_len: usize,

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
            8,
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
        alignment: usize,
        key_size: usize,
        value_size: usize,
    ) RocDict {
        const first_slot = switch (alignment) {
            8 => blk: {
                const slot_size = slotSize(key_size, value_size);

                const length = @sizeOf(usize) + (number_of_slots * slot_size);

                var new_bytes: []align(8) u8 = allocator.alignedAlloc(u8, 8, length) catch unreachable;

                var as_usize_array = @ptrCast([*]usize, new_bytes);
                if (result_in_place == InPlace.InPlace) {
                    as_usize_array[0] = @intCast(usize, number_of_slots);
                } else {
                    const v: isize = std.math.minInt(isize);
                    as_usize_array[0] = @bitCast(usize, v);
                }

                var as_u8_array = @ptrCast([*]u8, new_bytes);
                const first_slot = as_u8_array + @sizeOf(usize);

                break :blk first_slot;
            },
            16 => blk: {
                const slot_size = slotSize(key_size, value_size);

                const length = 2 * @sizeOf(usize) + (number_of_slots * slot_size);

                var new_bytes: []align(16) u8 = allocator.alignedAlloc(u8, 16, length) catch unreachable;

                var as_usize_array = @ptrCast([*]usize, new_bytes);
                if (result_in_place == InPlace.InPlace) {
                    as_usize_array[0] = 0;
                    as_usize_array[1] = @intCast(usize, number_of_slots);
                } else {
                    const v: isize = std.math.minInt(isize);
                    as_usize_array[0] = 0;
                    as_usize_array[1] = @bitCast(usize, v);
                }

                var as_u8_array = @ptrCast([*]u8, new_bytes);
                const first_slot = as_u8_array + 2 * @sizeOf(usize);

                break :blk first_slot;
            },
            else => unreachable,
        };

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

    fn getSlot(self: *const RocDict, capacity: usize, index: usize, key_width: usize, value_width: usize) Slot {
        const offset = capacity * (key_width + value_width) + index * @sizeOf(Slot);

        if (self.dict_bytes) |u8_ptr| {
            return @intToEnum(Slot, u8_ptr[offset]);
        } else {
            unreachable;
        }
    }

    fn setSlot(self: *RocDict, capacity: usize, index: usize, key_width: usize, value_width: usize, slot: Slot) void {
        const offset = capacity * (key_width + value_width) + index * @sizeOf(Slot);

        if (self.dict_bytes) |u8_ptr| {
            u8_ptr[offset] = @enumToInt(slot);
        } else {
            unreachable;
        }
    }

    fn setKey(self: *RocDict, capacity: usize, index: usize, alignment: Alignment, key_width: usize, value_width: usize, data: Opaque) void {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (index * key_width);
            } else {
                break :blk (capacity * value_width) + (index * key_width);
            }
        };

        if (self.dict_bytes) |u8_ptr| {
            const source = data orelse unreachable;
            @memcpy(u8_ptr + offset, source, key_width);
        } else {
            unreachable;
        }
    }

    fn getKey(self: *const RocDict, capacity: usize, index: usize, alignment: Alignment, key_width: usize, value_width: usize) Opaque {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (index * key_width);
            } else {
                break :blk (capacity * value_width) + (index * key_width);
            }
        };

        if (self.dict_bytes) |u8_ptr| {
            return u8_ptr + offset;
        } else {
            unreachable;
        }
    }

    fn setValue(self: *RocDict, capacity: usize, index: usize, alignment: Alignment, key_width: usize, value_width: usize, data: Opaque) void {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (capacity * key_width) + (index * value_width);
            } else {
                break :blk (index * value_width);
            }
        };

        if (self.dict_bytes) |u8_ptr| {
            const source = data orelse unreachable;
            @memcpy(u8_ptr + offset, source, key_width);
        } else {
            unreachable;
        }
    }

    fn getValue(self: *const RocDict, capacity: usize, index: usize, alignment: Alignment, key_width: usize, value_width: usize) Opaque {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (capacity * key_width) + (index * value_width);
            } else {
                break :blk (index * value_width);
            }
        };

        if (self.dict_bytes) |u8_ptr| {
            return u8_ptr + offset;
        } else {
            unreachable;
        }
    }

    fn findIndex(self: *const RocDict, capacity: usize, seed: u64, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn) MaybeIndex {
        if (self.isEmpty()) {
            return MaybeIndex.not_found;
        }

        const n = capacity;
        // hash the key, and modulo by the maximum size
        // (so we get an in-bounds index)
        const hash = hash_fn(seed, key);
        const index = hash % n;

        switch (self.getSlot(n, index, key_width, value_width)) {
            Slot.Empty, Slot.PreviouslyFilled => {
                return MaybeIndex.not_found;
            },
            Slot.Filled => {
                // is this the same key, or a new key?
                const current_key = self.getKey(n, index, alignment, key_width, value_width);

                if (is_eq(key, current_key)) {
                    return MaybeIndex{ .index = index };
                } else {
                    unreachable;
                }
            },
        }
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

// commonly used type aliases
const Opaque = ?[*]u8;
const HashFn = fn (u64, ?[*]u8) callconv(.C) u64;
const EqFn = fn (?[*]u8, ?[*]u8) callconv(.C) bool;
const Dec = fn (?[*]u8) callconv(.C) void;

// Dict.insert : Dict k v, k, v -> Dict k v
pub fn dictInsert(dict: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value: Opaque, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Dec, dec_value: Dec, output: *RocDict) callconv(.C) void {
    const n: usize = 8;
    const seed: u64 = 0;

    var result = RocDict.allocate(
        std.heap.c_allocator,
        InPlace.Clone,
        n, // number_of_slots,
        0, // number_of_entries,
        alignment.toUsize(),
        key_width,
        value_width,
    );

    {
        var i: usize = 0;
        while (i < n) {
            result.setSlot(n, i, key_width, value_width, Slot.Empty);
            i += 1;
        }
    }

    // hash the key, and modulo by the maximum size
    // (so we get an in-bounds index)
    const hash = hash_fn(seed, key);
    const index = hash % n;

    switch (result.getSlot(n, index, key_width, value_width)) {
        Slot.Empty, Slot.PreviouslyFilled => {
            result.setSlot(n, index, key_width, value_width, Slot.Filled);
            result.setKey(n, index, alignment, key_width, value_width, key);
            result.setValue(n, index, alignment, key_width, value_width, value);

            result.dict_entries_len += 1;
        },
        Slot.Filled => {
            // is this the same key, or a new key?
            const current_key = result.getKey(n, index, alignment, key_width, value_width);

            if (is_eq(key, current_key)) {
                // we will override the old value, but first have to decrement its refcount
                const current_value = result.getValue(n, index, alignment, key_width, value_width);
                dec_value(current_value);

                // we must consume the key argument!
                dec_key(key);

                result.setValue(n, index, alignment, key_width, value_width, value);
            } else {
                // TODO rehash, possibly grow the allocation?
                unreachable;
            }
        },
    }

    // write result into pointer
    output.* = result;
}

// Dict.remove : Dict k v, k -> Dict k v
pub fn dictRemove(input: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Dec, dec_value: Dec, output: *RocDict) callconv(.C) void {
    const capacity: usize = input.dict_slot_len;
    const n = capacity;
    const seed: u64 = 0;

    switch (input.findIndex(capacity, seed, alignment, key, key_width, value_width, hash_fn, is_eq)) {
        MaybeIndex.not_found => {
            // the key was not found; we're done
            output.* = input;
            return;
        },
        MaybeIndex.index => |index| {
            // TODO make sure input is unique (or duplicate otherwise)
            var dict = input;

            dict.setSlot(n, index, key_width, value_width, Slot.PreviouslyFilled);
            const old_key = dict.getKey(n, index, alignment, key_width, value_width);
            const old_value = dict.getValue(n, index, alignment, key_width, value_width);

            dec_key(old_key);
            dec_value(old_value);

            dict.dict_entries_len -= 1;

            output.* = dict;
        },
    }
}

// Dict.contains : Dict k v, k -> Bool
pub fn dictContains(dict: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn) callconv(.C) bool {
    const capacity: usize = dict.dict_slot_len;
    const seed: u64 = 0;

    switch (dict.findIndex(capacity, seed, alignment, key, key_width, value_width, hash_fn, is_eq)) {
        MaybeIndex.not_found => {
            return false;
        },
        MaybeIndex.index => |_| {
            return true;
        },
    }
}

test "RocDict.init() contains nothing" {
    const key_size = @sizeOf(usize);
    const value_size = @sizeOf(usize);

    const dict = dictEmpty();

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));
}
