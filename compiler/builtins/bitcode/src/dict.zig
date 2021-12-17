const std = @import("std");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const mem = std.mem;
const assert = std.debug.assert;

const utils = @import("utils.zig");
const RocList = @import("list.zig").RocList;

const INITIAL_SEED = 0xc70f6907;

const InPlace = enum(u8) {
    InPlace,
    Clone,
};

const Slot = enum(u8) {
    Empty,
    Filled,
    PreviouslyFilled,
};

const MaybeIndexTag = enum { index, not_found };

const MaybeIndex = union(MaybeIndexTag) { index: usize, not_found: void };

fn nextSeed(seed: u64) u64 {
    // TODO is this a valid way to get a new seed? are there better ways?
    return seed + 1;
}

fn totalCapacityAtLevel(input: usize) usize {
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

fn capacityOfLevel(input: usize) usize {
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
const Alignment = extern struct {
    bits: u8,

    const VALUE_BEFORE_KEY_FLAG: u8 = 0b1000_0000;

    fn toU32(self: Alignment) u32 {
        if (self.bits >= VALUE_BEFORE_KEY_FLAG) {
            return self.bits ^ Alignment.VALUE_BEFORE_KEY_FLAG;
        } else {
            return self.bits;
        }
    }

    fn keyFirst(self: Alignment) bool {
        if (self.bits & Alignment.VALUE_BEFORE_KEY_FLAG > 0) {
            return false;
        } else {
            return true;
        }
    }
};

pub fn decref(
    bytes_or_null: ?[*]u8,
    data_bytes: usize,
    alignment: Alignment,
) void {
    return utils.decref(bytes_or_null, data_bytes, alignment.toU32());
}

pub fn allocateWithRefcount(
    data_bytes: usize,
    alignment: Alignment,
) [*]u8 {
    return utils.allocateWithRefcount(data_bytes, alignment.toU32());
}

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

    pub fn allocate(
        number_of_levels: usize,
        number_of_entries: usize,
        alignment: Alignment,
        key_size: usize,
        value_size: usize,
    ) RocDict {
        const number_of_slots = totalCapacityAtLevel(number_of_levels);
        const slot_size = slotSize(key_size, value_size);
        const data_bytes = number_of_slots * slot_size;

        return RocDict{
            .dict_bytes = allocateWithRefcount(data_bytes, alignment),
            .number_of_levels = number_of_levels,
            .dict_entries_len = number_of_entries,
        };
    }

    pub fn reallocate(
        self: RocDict,
        alignment: Alignment,
        key_width: usize,
        value_width: usize,
    ) RocDict {
        const new_level = self.number_of_levels + 1;
        const slot_size = slotSize(key_width, value_width);

        const old_capacity = self.capacity();
        const new_capacity = totalCapacityAtLevel(new_level);
        const delta_capacity = new_capacity - old_capacity;

        const data_bytes = new_capacity * slot_size;
        const first_slot = allocateWithRefcount(data_bytes, alignment);

        // transfer the memory

        if (self.dict_bytes) |source_ptr| {
            const dest_ptr = first_slot;

            var source_offset: usize = 0;
            var dest_offset: usize = 0;

            if (alignment.keyFirst()) {
                // copy keys
                @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * key_width);

                // copy values
                source_offset = old_capacity * key_width;
                dest_offset = new_capacity * key_width;
                @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * value_width);
            } else {
                // copy values
                @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * value_width);

                // copy keys
                source_offset = old_capacity * value_width;
                dest_offset = new_capacity * value_width;
                @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * key_width);
            }

            // copy slots
            source_offset = old_capacity * (key_width + value_width);
            dest_offset = new_capacity * (key_width + value_width);
            @memcpy(dest_ptr + dest_offset, source_ptr + source_offset, old_capacity * @sizeOf(Slot));
        }

        var i: usize = 0;
        const first_new_slot_value = first_slot + old_capacity * slot_size + delta_capacity * (key_width + value_width);
        while (i < (new_capacity - old_capacity)) : (i += 1) {
            (first_new_slot_value)[i] = @enumToInt(Slot.Empty);
        }

        const result = RocDict{
            .dict_bytes = first_slot,
            .number_of_levels = self.number_of_levels + 1,
            .dict_entries_len = self.dict_entries_len,
        };

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        decref(self.dict_bytes, self.capacity() * slotSize(key_width, value_width), alignment);

        return result;
    }

    pub fn asU8ptr(self: RocDict) [*]u8 {
        return @ptrCast([*]u8, self.dict_bytes);
    }

    pub fn len(self: RocDict) usize {
        return self.dict_entries_len;
    }

    pub fn isEmpty(self: RocDict) bool {
        return self.len() == 0;
    }

    pub fn isUnique(self: RocDict) bool {
        // the empty dict is unique (in the sense that copying it will not leak memory)
        if (self.isEmpty()) {
            return true;
        }

        // otherwise, check if the refcount is one
        const ptr: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.dict_bytes));
        return (ptr - 1)[0] == utils.REFCOUNT_ONE;
    }

    pub fn capacity(self: RocDict) usize {
        return totalCapacityAtLevel(self.number_of_levels);
    }

    pub fn makeUnique(self: RocDict, alignment: Alignment, key_width: usize, value_width: usize) RocDict {
        if (self.isEmpty()) {
            return self;
        }

        if (self.isUnique()) {
            return self;
        }

        // unfortunately, we have to clone
        var new_dict = RocDict.allocate(self.number_of_levels, self.dict_entries_len, alignment, key_width, value_width);

        var old_bytes: [*]u8 = @ptrCast([*]u8, self.dict_bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_dict.dict_bytes);

        const number_of_bytes = self.capacity() * (@sizeOf(Slot) + key_width + value_width);
        @memcpy(new_bytes, old_bytes, number_of_bytes);

        // NOTE we fuse an increment of all keys/values with a decrement of the input dict
        const data_bytes = self.capacity() * slotSize(key_width, value_width);
        decref(self.dict_bytes, data_bytes, alignment);

        return new_dict;
    }

    fn getSlot(self: *const RocDict, index: usize, key_width: usize, value_width: usize) Slot {
        const offset = self.capacity() * (key_width + value_width) + index * @sizeOf(Slot);

        const ptr = self.dict_bytes orelse unreachable;
        return @intToEnum(Slot, ptr[offset]);
    }

    fn setSlot(self: *RocDict, index: usize, key_width: usize, value_width: usize, slot: Slot) void {
        const offset = self.capacity() * (key_width + value_width) + index * @sizeOf(Slot);

        const ptr = self.dict_bytes orelse unreachable;
        ptr[offset] = @enumToInt(slot);
    }

    fn setKey(self: *RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize, data: Opaque) void {
        if (key_width == 0) {
            return;
        }

        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (index * key_width);
            } else {
                break :blk (self.capacity() * value_width) + (index * key_width);
            }
        };

        const ptr = self.dict_bytes orelse unreachable;

        const source = data orelse unreachable;
        const dest = ptr + offset;
        @memcpy(dest, source, key_width);
    }

    fn getKey(self: *const RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize) Opaque {
        if (key_width == 0) {
            return null;
        }

        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (index * key_width);
            } else {
                break :blk (self.capacity() * value_width) + (index * key_width);
            }
        };

        const ptr = self.dict_bytes orelse unreachable;
        return ptr + offset;
    }

    fn setValue(self: *RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize, data: Opaque) void {
        if (value_width == 0) {
            return;
        }

        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (self.capacity() * key_width) + (index * value_width);
            } else {
                break :blk (index * value_width);
            }
        };

        const ptr = self.dict_bytes orelse unreachable;

        const source = data orelse unreachable;
        const dest = ptr + offset;
        @memcpy(dest, source, value_width);
    }

    fn getValue(self: *const RocDict, index: usize, alignment: Alignment, key_width: usize, value_width: usize) Opaque {
        if (value_width == 0) {
            return null;
        }

        const offset = blk: {
            if (alignment.keyFirst()) {
                break :blk (self.capacity() * key_width) + (index * value_width);
            } else {
                break :blk (index * value_width);
            }
        };

        const ptr = self.dict_bytes orelse unreachable;
        return ptr + offset;
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
            const index = capacityOfLevel(current_level - 1) + @intCast(usize, (hash % current_level_size));

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
pub fn dictEmpty(dict: *RocDict) callconv(.C) void {
    dict.* = RocDict.empty();
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
const IncN = fn (?[*]u8, usize) callconv(.C) void;
const Dec = fn (?[*]u8) callconv(.C) void;

const Caller3 = fn (?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8, ?[*]u8) callconv(.C) void;

// Dict.insert : Dict k v, k, v -> Dict k v
pub fn dictInsert(input: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value: Opaque, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Dec, dec_value: Dec, output: *RocDict) callconv(.C) void {
    var seed: u64 = INITIAL_SEED;

    var result = input.makeUnique(alignment, key_width, value_width);

    var current_level: usize = 1;
    var current_level_size: usize = 8;
    var next_level_size: usize = 2 * current_level_size;

    while (true) {
        if (current_level > result.number_of_levels) {
            result = result.reallocate(alignment, key_width, value_width);
        }

        const hash = hash_fn(seed, key);
        const index = capacityOfLevel(current_level - 1) + @intCast(usize, (hash % current_level_size));
        assert(index < result.capacity());

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

// Dict.remove : Dict k v, k -> Dict k v
pub fn dictRemove(input: RocDict, alignment: Alignment, key: Opaque, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Dec, dec_value: Dec, output: *RocDict) callconv(.C) void {
    switch (input.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
        MaybeIndex.not_found => {
            // the key was not found; we're done
            output.* = input;
            return;
        },
        MaybeIndex.index => |index| {
            var dict = input.makeUnique(alignment, key_width, value_width);

            assert(index < dict.capacity());

            dict.setSlot(index, key_width, value_width, Slot.PreviouslyFilled);
            const old_key = dict.getKey(index, alignment, key_width, value_width);
            const old_value = dict.getValue(index, alignment, key_width, value_width);

            dec_key(old_key);
            dec_value(old_value);
            dict.dict_entries_len -= 1;

            // if the dict is now completely empty, free its allocation
            if (dict.dict_entries_len == 0) {
                const data_bytes = dict.capacity() * slotSize(key_width, value_width);
                decref(dict.dict_bytes, data_bytes, alignment);
                output.* = RocDict.empty();
                return;
            }

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

pub fn dictKeys(dict: RocDict, alignment: Alignment, key_width: usize, value_width: usize, inc_key: Inc, output: *RocList) callconv(.C) void {
    const size = dict.capacity();

    var length: usize = 0;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        switch (dict.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                length += 1;
            },
            else => {},
        }
    }

    if (length == 0) {
        output.* = RocList.empty();
        return;
    }

    const data_bytes = length * key_width;
    var ptr = allocateWithRefcount(data_bytes, alignment);

    i = 0;
    var copied: usize = 0;
    while (i < size) : (i += 1) {
        switch (dict.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                const key = dict.getKey(i, alignment, key_width, value_width);
                inc_key(key);

                const key_cast = @ptrCast([*]const u8, key);
                @memcpy(ptr + (copied * key_width), key_cast, key_width);
                copied += 1;
            },
            else => {},
        }
    }

    output.* = RocList{ .bytes = ptr, .length = length };
}

pub fn dictValues(dict: RocDict, alignment: Alignment, key_width: usize, value_width: usize, inc_value: Inc, output: *RocList) callconv(.C) void {
    const size = dict.capacity();

    var length: usize = 0;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        switch (dict.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                length += 1;
            },
            else => {},
        }
    }

    if (length == 0) {
        output.* = RocList.empty();
        return;
    }

    const data_bytes = length * value_width;
    var ptr = allocateWithRefcount(data_bytes, alignment);

    i = 0;
    var copied: usize = 0;
    while (i < size) : (i += 1) {
        switch (dict.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                const value = dict.getValue(i, alignment, key_width, value_width);
                inc_value(value);

                const value_cast = @ptrCast([*]const u8, value);
                @memcpy(ptr + (copied * value_width), value_cast, value_width);
                copied += 1;
            },
            else => {},
        }
    }

    output.* = RocList{ .bytes = ptr, .length = length };
}

fn doNothing(_: Opaque) callconv(.C) void {
    return;
}

pub fn dictUnion(dict1: RocDict, dict2: RocDict, alignment: Alignment, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, inc_key: Inc, inc_value: Inc, output: *RocDict) callconv(.C) void {
    output.* = dict1.makeUnique(alignment, key_width, value_width);

    var i: usize = 0;
    while (i < dict2.capacity()) : (i += 1) {
        switch (dict2.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                const key = dict2.getKey(i, alignment, key_width, value_width);

                switch (output.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
                    MaybeIndex.not_found => {
                        const value = dict2.getValue(i, alignment, key_width, value_width);
                        inc_value(value);

                        // we need an extra RC token for the key
                        inc_key(key);
                        inc_value(value);

                        // we know the newly added key is not a duplicate, so the `dec`s are unreachable
                        const dec_key = doNothing;
                        const dec_value = doNothing;

                        dictInsert(output.*, alignment, key, key_width, value, value_width, hash_fn, is_eq, dec_key, dec_value, output);
                    },
                    MaybeIndex.index => |_| {
                        // the key is already in the output dict
                        continue;
                    },
                }
            },
            else => {},
        }
    }
}

pub fn dictIntersection(dict1: RocDict, dict2: RocDict, alignment: Alignment, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Inc, dec_value: Inc, output: *RocDict) callconv(.C) void {
    output.* = dict1.makeUnique(alignment, key_width, value_width);

    var i: usize = 0;
    const size = dict1.capacity();
    while (i < size) : (i += 1) {
        switch (output.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                const key = dict1.getKey(i, alignment, key_width, value_width);

                switch (dict2.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
                    MaybeIndex.not_found => {
                        dictRemove(output.*, alignment, key, key_width, value_width, hash_fn, is_eq, dec_key, dec_value, output);
                    },
                    MaybeIndex.index => |_| {
                        // keep this key/value
                        continue;
                    },
                }
            },
            else => {},
        }
    }
}

pub fn dictDifference(dict1: RocDict, dict2: RocDict, alignment: Alignment, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Dec, dec_value: Dec, output: *RocDict) callconv(.C) void {
    output.* = dict1.makeUnique(alignment, key_width, value_width);

    var i: usize = 0;
    const size = dict1.capacity();
    while (i < size) : (i += 1) {
        switch (output.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                const key = dict1.getKey(i, alignment, key_width, value_width);

                switch (dict2.findIndex(alignment, key, key_width, value_width, hash_fn, is_eq)) {
                    MaybeIndex.not_found => {
                        // keep this key/value
                        continue;
                    },
                    MaybeIndex.index => |_| {
                        dictRemove(output.*, alignment, key, key_width, value_width, hash_fn, is_eq, dec_key, dec_value, output);
                    },
                }
            },
            else => {},
        }
    }
}

pub fn setFromList(list: RocList, alignment: Alignment, key_width: usize, value_width: usize, hash_fn: HashFn, is_eq: EqFn, dec_key: Dec, output: *RocDict) callconv(.C) void {
    output.* = RocDict.empty();

    var ptr = @ptrCast([*]u8, list.bytes);

    const dec_value = doNothing;
    const value = null;

    const size = list.length;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        const key = ptr + i * key_width;
        dictInsert(output.*, alignment, key, key_width, value, value_width, hash_fn, is_eq, dec_key, dec_value, output);
    }

    // NOTE: decref checks for the empty case
    const data_bytes = size * key_width;
    decref(list.bytes, data_bytes, alignment);
}

pub fn dictWalk(
    dict: RocDict,
    caller: Caller3,
    data: Opaque,
    inc_n_data: IncN,
    data_is_owned: bool,
    accum: Opaque,
    alignment: Alignment,
    key_width: usize,
    value_width: usize,
    accum_width: usize,
    output: Opaque,
) callconv(.C) void {
    const alignment_u32 = alignment.toU32();
    // allocate space to write the result of the stepper into
    // experimentally aliasing the accum and output pointers is not a good idea
    const bytes_ptr: [*]u8 = utils.alloc(accum_width, alignment_u32);
    var b1 = output orelse unreachable;
    var b2 = bytes_ptr;

    if (data_is_owned) {
        inc_n_data(data, dict.len());
    }

    @memcpy(b2, accum orelse unreachable, accum_width);

    var i: usize = 0;
    const size = dict.capacity();
    while (i < size) : (i += 1) {
        switch (dict.getSlot(i, key_width, value_width)) {
            Slot.Filled => {
                const key = dict.getKey(i, alignment, key_width, value_width);
                const value = dict.getValue(i, alignment, key_width, value_width);

                caller(data, b2, key, value, b1);

                std.mem.swap([*]u8, &b1, &b2);
            },
            else => {},
        }
    }

    @memcpy(output orelse unreachable, b2, accum_width);
    utils.dealloc(bytes_ptr, alignment_u32);
}
