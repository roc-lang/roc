const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const mem = std.mem;
const Allocator = mem.Allocator;

const level_size = 32;

const INITIAL_SEED = 0xc70f6907;
const REFCOUNT_ONE_ISIZE: comptime isize = std.math.minInt(isize);
const REFCOUNT_ONE: usize = @bitCast(usize, REFCOUNT_ONE_ISIZE);

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

fn nextSeed(seed: u64) u64 {
    return seed + 1;
}

fn total_slots_at_level(input: usize) usize {
    if (input == 0) {
        return 0;
    }

    var n = input;
    var slots: usize = 8;

    while (n > 1) : (n -= 1) {
        slots = slots * 2 + slots;
    }

    return slots;
}

fn slots_at_level(input: usize) usize {
    if (input == 0) {
        return 0;
    }

    var n = input;
    var slots: usize = 8;

    while (n > 1) : (n -= 1) {
        slots = slots * 2;
    }

    return slots;
}

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
    number_of_levels: usize,

    pub fn empty() RocDict {
        return RocDict{
            .dict_entries_len = 0,
            .number_of_levels = 0,
            .dict_bytes = null,
        };
    }

    pub fn deinit(self: RocDict, allocator: *Allocator, key_size: usize, value_size: usize) void {
        if (!self.isEmpty()) {
            const slot_size = slotSize(key_size, value_size);

            const dict_bytes_ptr: [*]u8 = self.dict_bytes orelse unreachable;

            const dict_bytes: []u8 = dict_bytes_ptr[0..(self.number_of_levels)];
            allocator.free(dict_bytes);
        }
    }

    pub fn allocate(
        allocator: *Allocator,
        result_in_place: InPlace,
        number_of_levels: usize,
        number_of_entries: usize,
        alignment: usize,
        key_size: usize,
        value_size: usize,
    ) RocDict {
        const number_of_slots = total_slots_at_level(number_of_levels);
        const first_slot = switch (alignment) {
            8 => blk: {
                const slot_size = slotSize(key_size, value_size);

                const length = @sizeOf(usize) + (number_of_slots * slot_size);

                var new_bytes: []align(8) u8 = allocator.alignedAlloc(u8, 8, length) catch unreachable;

                var as_usize_array = @ptrCast([*]usize, new_bytes);
                if (result_in_place == InPlace.InPlace) {
                    as_usize_array[0] = @intCast(usize, number_of_slots);
                } else {
                    as_usize_array[0] = REFCOUNT_ONE;
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
                    as_usize_array[0] = 0;
                    as_usize_array[1] = REFCOUNT_ONE;
                }

                var as_u8_array = @ptrCast([*]u8, new_bytes);
                const first_slot = as_u8_array + 2 * @sizeOf(usize);

                break :blk first_slot;
            },
            else => unreachable,
        };

        return RocDict{
            .dict_bytes = first_slot,
            .number_of_levels = number_of_levels,
            .dict_entries_len = number_of_entries,
        };
    }

    pub fn reallocate(
        self: RocDict,
        allocator: *Allocator,
        alignment: usize,
        key_width: usize,
        value_width: usize,
    ) RocDict {
        const new_level = self.number_of_levels + 1;
        const slot_size = slotSize(key_width, value_width);

        const old_capacity = self.capacity();
        const new_capacity = total_slots_at_level(new_level);

        const first_slot = switch (alignment) {
            8 => blk: {
                const length = @sizeOf(usize) + (new_capacity * slot_size);

                var new_bytes: []align(8) u8 = allocator.alignedAlloc(u8, 8, length) catch unreachable;

                var as_usize_array = @ptrCast([*]usize, new_bytes);
                as_usize_array[0] = REFCOUNT_ONE;

                var as_u8_array = @ptrCast([*]u8, new_bytes);
                const first_slot = as_u8_array + @sizeOf(usize);

                break :blk first_slot;
            },
            else => unreachable,
        };

        // transfer the memory

        var source_ptr = self.dict_bytes orelse unreachable;
        var dest_ptr = first_slot;

        if (old_capacity > 0) {
            var source_offset: usize = 0;
            var dest_offset: usize = 0;
            @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * key_width);

            source_offset += old_capacity * key_width;
            dest_offset += old_capacity * key_width + (new_capacity * key_width);
            @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * value_width);

            source_offset += old_capacity * value_width;
            dest_offset += old_capacity * value_width + (new_capacity * value_width);
            @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * @sizeOf(Slot));
        }

        var i: usize = 0;
        const first_new_slot_value = dest_ptr + old_capacity * slot_size + new_capacity * (key_width + value_width);
        while (i < (new_capacity - old_capacity)) : (i += 1) {
            (first_new_slot_value)[i] = @enumToInt(Slot.Empty);
        }

        return RocDict{
            .dict_bytes = first_slot,
            .number_of_levels = self.number_of_levels + 1,
            .dict_entries_len = self.dict_entries_len,
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

    pub fn refcountIsOne(self: RocDict) bool {
        if (self.isEmpty()) {
            // empty dicts are not refcounted at all
            return false;
        }

        const ptr: [*]usize = @ptrCast([*]usize, @alignCast(8, self.dict_bytes));

        return (ptr - 1)[0] == REFCOUNT_ONE;
    }

    pub fn capacity(self: RocDict) usize {
        return total_slots_at_level(self.number_of_levels);
    }

    pub fn makeUnique(self: RocDict, allocator: *Allocator, in_place: InPlace, alignment: Alignment, key_width: usize, value_width: usize, inc_key: Inc, inc_value: Inc) RocDict {
        if (self.isEmpty()) {
            return self;
        }

        if (self.refcountIsOne()) {
            return self;
        }

        // unfortunately, we have to clone

        var new_dict = RocDict.allocate(allocator, in_place, 8, self.dict_entries_len, alignment.toUsize(), key_width, value_width);

        var old_bytes: [*]u8 = @ptrCast([*]u8, self.dict_bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_dict.dict_bytes);

        const number_of_bytes = 8 * (@sizeOf(Slot) + key_width + value_width);
        @memcpy(new_bytes, old_bytes, number_of_bytes);

        // we copied potentially-refcounted values; make sure to increment
        const size = new_dict.capacity();
        var i: usize = 0;

        i = 0;
        while (i < size) : (i += 1) {
            switch (new_dict.getSlot(i, key_width, value_width)) {
                Slot.Filled => {
                    inc_key(new_dict.getKey(i, alignment, key_width, value_width));
                    inc_value(new_dict.getValue(i, alignment, key_width, value_width));
                },
                else => {},
            }
        }

        return new_dict;
    }

    fn getSlot(self: *const RocDict, index: usize, key_width: usize, value_width: usize) Slot {
        const offset = self.capacity() * (key_width + value_width) + index * @sizeOf(Slot);

        if (self.dict_bytes) |u8_ptr| {
            return @intToEnum(Slot, u8_ptr[offset]);
        } else {
            unreachable;
        }
    }

    fn setSlot(self: *RocDict, index: usize, key_width: usize, value_width: usize, slot: Slot) void {
        const offset = self.capacity() * (key_width + value_width) + index * @sizeOf(Slot);

        if (self.dict_bytes) |u8_ptr| {
            u8_ptr[offset] = @enumToInt(slot);
        } else {
            unreachable;
        }
    }

    fn setKey(self: *RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize, data: Opaque) void {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (index * key_width);
            } else {
                break :blk (self.capacity() * value_width) + (index * key_width);
            }
        };

        if (self.dict_bytes) |u8_ptr| {
            const source = data orelse unreachable;
            @memcpy(u8_ptr + offset, source, key_width);
        } else {
            unreachable;
        }
    }

    fn getKey(self: *const RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize) Opaque {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (index * key_width);
            } else {
                break :blk (self.capacity() * value_width) + (index * key_width);
            }
        };

        if (self.dict_bytes) |u8_ptr| {
            return u8_ptr + offset;
        } else {
            unreachable;
        }
    }

    fn setValue(self: *RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize, data: Opaque) void {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (self.capacity() * key_width) + (index * value_width);
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

    fn getValue(self: *const RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize) Opaque {
        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (self.capacity() * key_width) + (index * value_width);
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

    fn findIndex(self: *const RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn) MaybeIndex {
        if (self.isEmpty()) {
            return MaybeIndex.not_found;
        }

        var seed: u64 = INITIAL_SEED;

        var current_level: usize = 1;
        var current_level_size: usize = 8;
        var next_level_size: usize = 2 * current_level_size;

        while (true) {
            if (current_level > self.number_of_levels) {
                return MaybeIndex.not_found;
            }

            // hash the key, and modulo by the maximum size
            // (so we get an in-bounds index)
            const hash = hash_fn(seed, key);
            const index = hash % current_level_size;

            switch (self.getSlot(index, key_width, value_width)) {
                Slot.Empty, Slot.PreviouslyFilled => {
                    return MaybeIndex.not_found;
                },
                Slot.Filled => {
                    // is this the same key, or a new key?
                    const current_key = self.getKey(index, alignment, key_width, value_width);

                    if (is_eq(key, current_key)) {
                        return MaybeIndex{ .index = index };
                    } else {
                        current_level += 1;
                        current_level_size *= 2;
                        next_level_size *= 2;

                        seed = nextSeed(seed);
                        continue;
                    }
                },
            }
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

const Inc = fn (?[*]u8) callconv(.C) void;
const Dec = fn (?[*]u8) callconv(.C) void;

// Dict.insert : Dict k v, k, v -> Dict k v
pub fn dictInsert(input: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value: Opaque, value_width: usize, hash_fn: HashFn, is_eq: EqFn, inc_key: Inc, dec_key: Dec, inc_value: Inc, dec_value: Dec, output: *RocDict) callconv(.C) void {
    var seed: u64 = INITIAL_SEED;

    var result: RocDict = blk: {
        if (input.isEmpty()) {
            break :blk input;
        } else {
            const in_place = InPlace.Clone;

            var temp = input.makeUnique(std.heap.c_allocator, in_place, alignment, key_width, value_width, inc_key, inc_value);

            break :blk temp;
        }
    };

    var current_level: usize = 1;
    var current_level_size: usize = 8;
    var next_level_size: usize = 2 * current_level_size;

    while (true) {
        if (current_level > result.number_of_levels) {
            result = result.reallocate(std.heap.c_allocator, alignment.toUsize(), key_width, value_width);
        }

        const hash = hash_fn(seed, key);
        const index = hash % current_level_size;

        switch (result.getSlot(index, key_width, value_width)) {
            Slot.Empty, Slot.PreviouslyFilled => {
                result.setSlot(index, key_width, value_width, Slot.Filled);
                result.setKey(index, alignment, key_width, value_width, key);
                result.setValue(index, alignment, key_width, value_width, value);

                result.dict_entries_len += 1;
                break;
            },
            Slot.Filled => {
                // is this the same key, or a new key?
                const current_key = result.getKey(index, alignment, key_width, value_width);

                if (is_eq(key, current_key)) {
                    // we will override the old value, but first have to decrement its refcount
                    const current_value = result.getValue(index, alignment, key_width, value_width);
                    dec_value(current_value);

                    // we must consume the key argument!
                    dec_key(key);

                    result.setValue(index, alignment, key_width, value_width, value);
                    break;
                } else {
                    seed = nextSeed(seed);

                    current_level += 1;
                    current_level_size *= 2;
                    next_level_size *= 2;

                    continue;
                }
            },
        }
    }

    // write result into pointer
    output.* = result;
}

// { ptr, length, level: u8 }
// [ key1 .. key8, value1, ...

// Dict.remove : Dict k v, k -> Dict k v
pub fn dictRemove(input: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, inc_key: Inc, dec_key: Dec, inc_value: Inc, dec_value: Dec, output: *RocDict) callconv(.C) void {
    switch (input.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
        MaybeIndex.not_found => {
            // the key was not found; we're done
            output.* = input;
            return;
        },
        MaybeIndex.index => |index| {
            // TODO make sure input is unique (or duplicate otherwise)
            var dict = input;

            dict.setSlot(index, key_width, value_width, Slot.PreviouslyFilled);
            const old_key = dict.getKey(index, alignment, key_width, value_width);
            const old_value = dict.getValue(index, alignment, key_width, value_width);

            dec_key(old_key);
            dec_value(old_value);

            dict.dict_entries_len -= 1;

            output.* = dict;
        },
    }
}

// Dict.contains : Dict k v, k -> Bool
pub fn dictContains(dict: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn) callconv(.C) bool {
    switch (dict.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
        MaybeIndex.not_found => {
            return false;
        },
        MaybeIndex.index => |_| {
            return true;
        },
    }
}

// Dict.get : Dict k v, k -> { flag: bool, value: Opaque }
pub fn dictGet(dict: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, inc_value: Inc) callconv(.C) extern struct { value: Opaque, flag: bool } {
    switch (dict.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
        MaybeIndex.not_found => {
            return .{ .flag = false, .value = null };
        },
        MaybeIndex.index => |index| {
            var value = dict.getValue(index, alignment, key_width, value_width);
            inc_value(value);
            return .{ .flag = true, .value = value };
        },
    }
}

// Dict.elementsRc
// increment or decrement all dict elements (but not the dict's allocation itself)
pub fn elementsRc(dict: RocDict, alignment: Alignment, key_width: usize, value_width: usize, modify_key: Inc, modify_value: Inc) callconv(.C) void {
    const size = dict.capacity();

    var i: usize = 0;
    while (i < size) : (i += 1) {
        switch (dict.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                modify_key(dict.getKey(i, alignment, key_width, value_width));
                modify_value(dict.getValue(i, alignment, key_width, value_width));
            },
            else => {},
        }
    }
}

test "RocDict.init() contains nothing" {
    const key_size = @sizeOf(usize);
    const value_size = @sizeOf(usize);

    const dict = dictEmpty();

    expectEqual(false, dict.contains(4, @ptrCast(*const c_void, &""), 9));
}
