const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const assert = std.debug.assert;

const print = std.debug.print;

const hash = @import("hash.zig");

const size = 32;

pub fn RocDict(
    comptime Key: type,
    comptime Value: type,
) type {
    return struct {
        const Self = @This();

        len: u64,
        entries: [size]?Entry,

        pub const Entry = struct {
            key_ptr: *Key,
            value_ptr: *Value,
            overflow: ?Overflow,

            pub const Overflow = struct {
                entries: *[size]?*Entry,

                // This function takes an index, key, and value, because presumably overflow
                // is only created once an there is a collision between two entries, and 
                // there is therefore an extra entry that needs to be stored
                pub fn new(allocator: *Allocator, index: u64, key_ptr: *Key, value_ptr: *Value) Overflow {
                    var init_entries: [size]?*Entry = undefined;

                    var new_overflow = Overflow{ .entries = &init_entries };

                    new_overflow.setAtIndex(allocator, index, key_ptr, value_ptr);

                    return new_overflow;
                }

                pub fn setAtIndex(self: *Overflow, allocator: *Allocator, index: u64, key_ptr: *Key, value_ptr: *Value) void {
                    var overflow = self.*;
                    var new_entry = Entry.new(allocator, key_ptr, value_ptr);

                    overflow.entries.*[index] = &new_entry;
                }

                pub fn getAtIndex(
                    self: Overflow,
                    index: u64,
                ) ?*Entry {
                    return self.entries.*[index];
                }
            };

            pub fn new(allocator: *Allocator, key_ptr: *Key, value_ptr: *Value) Entry {
                return Entry{
                    .value_ptr = value_ptr,
                    .key_ptr = key_ptr,
                    .overflow = null,
                };
            }

            pub fn eq(self: Entry, other: Entry) bool {
                const same_keys = self.key_ptr.* == other.key_ptr.*;
                const same_value = self.value_ptr.* == other.value_ptr.*;
                return same_keys and same_value;
            }

            pub fn setValue(self: *Entry, value_ptr: *Value) void {
                self.*.value_ptr = value_ptr;
            }

            pub fn get(self: *Entry, key: Key, level: u64) ?*Value {
                const entry = self.*;
                const entry_key = entry.key_ptr.*;

                if (key == entry_key) {
                    return entry.value_ptr;
                } else {
                    const maybe_overflow = entry.overflow;

                    if (maybe_overflow == null) {
                        return null;
                    } else {
                        const overflow = maybe_overflow.?;
                        const index = keyToIndexAtLevel(key, level);
                        const maybe_next_entry_ptr = overflow.getAtIndex(index);

                        if (maybe_next_entry_ptr == null) {
                            return null;
                        } else {
                            var next_entry = maybe_next_entry_ptr.?.*;
                            const next_level = level + 1;

                            return next_entry.get(key, next_level);
                        }
                    }
                }
            }

            // The bool this function returns represents if an new entry was
            // added (true), or if an old one was updated (false);
            pub fn insertIntoOverflow(self: *Entry, allocator: *Allocator, key_ptr: *Key, value_ptr: *Value, level: u64) bool {
                const key = key_ptr.*;

                const index = keyToIndexAtLevel(key, level);

                var entry = self.*;

                // If there is no overflow, make a new OverFlow
                if (entry.overflow == null) {
                    var new_overflow: Overflow = Overflow.new(allocator, index, key_ptr, value_ptr);

                    entry.overflow = new_overflow;

                    return true;
                } else {
                    var overflow = entry.overflow.?;

                    const maybe_overflow_entry_ptr = overflow.getAtIndex(index);

                    // If there is overflow, check if this index is taken
                    // and if it isnt, than insert it
                    if (maybe_overflow_entry_ptr == null) {
                        overflow.setAtIndex(allocator, index, key_ptr, value_ptr);
                        return true;
                    } else {
                        var overflow_entry_ptr = maybe_overflow_entry_ptr.?;

                        var overflow_entry = overflow_entry_ptr.*;

                        // if this index in the overflow is taken
                        // and it has the same key, update the entry
                        // at that key, otherwise move down another
                        // level into that entry's overflow.
                        if (overflow_entry.key_ptr.* == key) {
                            overflow_entry.setValue(value_ptr);
                            return false;
                        } else {
                            return overflow_entry.insertIntoOverflow(allocator, key_ptr, value_ptr, level + 1);
                        }
                    }
                }
            }
        };

        pub const Query = struct {
            maybe_entry: ?Entry
        };

        fn query(self: Self, key: Key, level: u64) Query {
            const index = keyToIndexAtLevel(key, level);

            const maybe_entry = self.entries[index];

            if (maybe_entry == null) {
                return Query{ .maybe_entry = null };
            } else {
                var entry = maybe_entry.?;

                if (entry.key_ptr.* == key) {
                    return Query{ .maybe_entry = entry };
                } else {
                    return self.query(key, level + 1);
                }
            }
        }

        fn keyToIndexAtLevel(key: Key, level: u64) u64 {
            const index = hash.hash(std.mem.asBytes(&key), level) % size;

            return index;
        }

        pub fn init(allocator: *Allocator) Self {
            const roc_dict_size = @sizeOf(Self);

            var init_entries: [size]?Entry = undefined;

            for (init_entries) |*entry, i| {
                entry.* = null;
            }

            return Self{
                .len = 0,
                .entries = init_entries,
            };
        }

        pub fn get(self: Self, key: Key) ?*Value {
            const q = self.query(key, 0);

            if (q.maybe_entry == null) {
                return null;
            } else {
                var entry = q.maybe_entry.?;

                const entry_key = entry.key_ptr.*;

                if (entry_key == key) {
                    return entry.value_ptr;
                } else {
                    return entry.get(key, 0);
                }
            }
        }

        pub fn insert(self: *Self, allocator: *Allocator, key_ptr: *Key, value_ptr: *Value) void {
            const level = 0;

            const key = key_ptr.*;

            const q = self.query(key, level);

            const index = keyToIndexAtLevel(key, level);

            if (q.maybe_entry == null) {
                var new_entry = Entry.new(allocator, value_ptr, key_ptr);

                self.entries[index] = new_entry;

                self.*.len += 1;
            } else {
                var entry = q.maybe_entry.?;

                if (entry.key_ptr.* == key) {
                    var entry_ptr = &entry;
                    entry_ptr.setValue(value_ptr);
                    self.entries[index] = entry;
                } else {
                    const inserted_new_entry = entry.insertIntoOverflow(allocator, key_ptr, value_ptr, 0);

                    if (inserted_new_entry) {
                        self.len += 1;
                    }
                }
            }
        }

        pub fn getLen(self: Self) u64 {
            return self.len;
        }

        pub fn eq(self: Self, other: Self) bool {
            if (self.getLen() != other.getLen()) {
                return false;
            }

            var levels_count: u64 = self.entries.len;
            var are_same = true;

            var i: u64 = 0;
            while ((i < size) and are_same) {
                const maybe_entry = self.entries[i];
                const maybe_other_entry = other.entries[i];

                if (maybe_entry == null) {
                    if (maybe_other_entry != null) {
                        are_same = false;
                    } else {
                        i += 1;
                    }
                } else {
                    if (maybe_other_entry == null) {
                        are_same = false;
                    } else {
                        const entry = maybe_entry.?;
                        const other_entry = maybe_other_entry.?;
                        if (entry.eq(other_entry)) {
                            i += 1;
                        } else {
                            are_same = false;
                        }
                    }
                }
            }

            return are_same;
        }
    };
}

test "RocDict.insert with hash collisions" {
    var dict = RocDict(u64, u64).init(testing.allocator);

    var i: u64 = 0;

    while (i < (size * 2)) {
        dict.insert(testing.allocator, &i, &i);

        i += 1;
    }

    i = 0;
    while (i < (size * 2)) {
        const entry = dict.get(i);

        expectEqual(i, entry.?.*);

        i += 1;
    }
}

test "repeated RocDict.insert" {
    var dict = RocDict(u64, u64).init(testing.allocator);

    var index: u64 = 0;
    var fst_val: u64 = 17;
    var snd_val: u64 = 49;

    dict.insert(testing.allocator, &index, &fst_val);
    dict.insert(testing.allocator, &index, &snd_val);

    var value_ptr: ?*u64 = dict.get(index);

    if (value_ptr == null) {
        unreachable;
    } else {
        const value = value_ptr.?.*;
        expectEqual(snd_val, value);
    }
}

test "RocDict.eq" {
    var fst = RocDict(u64, u64).init(testing.allocator);
    var snd = RocDict(u64, u64).init(testing.allocator);

    var key: u64 = 0;
    var value: u64 = 30;

    fst.insert(testing.allocator, &key, &value);
    snd.insert(testing.allocator, &key, &value);
    assert(fst.eq(snd));

    var empty = RocDict(u64, u64).init(testing.allocator);
    assert(!fst.eq(empty));

    var trd = RocDict(u64, u64).init(testing.allocator);
    var new_value: u64 = value + 1;
    trd.insert(testing.allocator, &key, &new_value);

    assert(!fst.eq(trd));
}

test "RocDict.getLen" {
    var dict = RocDict(u64, u64).init(testing.allocator);

    var key: u64 = 0;

    var value: u64 = 16;
    dict.insert(testing.allocator, &key, &value);

    const expect_len: u64 = 1;
    expectEqual(dict.getLen(), expect_len);

    dict.insert(testing.allocator, &key, &value);

    expectEqual(dict.getLen(), expect_len);

    var next_key: u64 = key + 1;
    var next_value: u64 = 3;
    dict.insert(testing.allocator, &next_key, &next_value);

    var next_expected_len: u64 = 2;
    expectEqual(dict.getLen(), next_expected_len);
}

test "RocDict.insert" {
    var dict = RocDict(u64, u64).init(testing.allocator);

    var index: u64 = 0;
    var value: u64 = 30;

    dict.insert(testing.allocator, &index, &value);

    var result_value: ?*u64 = dict.get(index);
    expectEqual(result_value.?.*, value);

    var expect_len: u64 = 1;
    expectEqual(dict.getLen(), expect_len);
}

test "RocDict.get" {
    const empty = RocDict(u64, u64).init(testing.allocator);

    const expect: ?*u64 = null;
    expectEqual(empty.get(29), expect);
}

test "RocDict.init" {
    const empty = RocDict(u64, u64).init(testing.allocator);
    expectEqual(empty.getLen(), 0);

    const MadeUpType = struct {
        oneField: u64
    };

    const empty_made_up = RocDict(u64, MadeUpType).init(testing.allocator);

    const expect: u64 = 0;
    expectEqual(empty_made_up.getLen(), expect);
}
