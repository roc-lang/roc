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

        pub const Entry = struct {
            key: Key,
            value: Value,

            pub fn eq(self: Entry, other: Entry) bool {
                const same_keys = self.key == other.key;
                const same_value = self.value == other.value;
                return same_keys and same_value;
            }

            pub fn set_value(self: *Entry, value: Value) void {
                self.*.value = value;
            }
        };


        pub const Query = struct {
            maybe_entry: ?Entry
        };

        len: u64,
        entries: [size]?Entry,

        fn query(self: Self, key: Key, level: u64) Query {
            const index = key_to_index_at_level(key, level);

            const maybe_entry = self.entries[index];

            if (maybe_entry == null) {
                return Query { .maybe_entry = null };
            } else {
                var entry = maybe_entry.?;

                if (entry.key == key) {
                    return Query { .maybe_entry = entry };
                } else {
                    return self.query(key, level + 1);
                }
            }
        }

        fn key_to_index_at_level(key: Key, level: u64) u64 {
            const index = hash.hash(std.mem.asBytes(&key), level) % size;

            return index;
        }

        pub fn init(allocator: *Allocator) Self {
            const roc_dict_size = @sizeOf(Self);

            var init_entries: [size]?Entry = undefined;

            for (init_entries) |*entry, i| {
                entry.* = null;
            }

            return Self {
                .len = 0,
                .entries = init_entries,
            };
        }


        pub fn get(self: Self, key: Key) ?Value {

            const q = self.query(key, 0);

            if (q.maybe_entry == null) {
                return null;
            } else {
                const entry = q.maybe_entry.?;

                return entry.value;
            }
        }

        pub fn insert(self: *Self, key: Key, value: Value) void  {
            const level = 0;

            const q = self.query(key, level);

            const index = key_to_index_at_level(key, level);

            if (q.maybe_entry == null) {

                var new_entry = Entry {
                    .value = value,
                    .key = key
                };

                self.entries[index] = new_entry;
                
                self.len += 1;
            } else {
                var entry = q.maybe_entry.?;

                var entry_ptr = &entry;

                entry_ptr.set_value(value);

               self.entries[index] = entry;
            }
        }

        pub fn get_len(self: Self) u64 {
            return self.len;
        }

        pub fn eq(self: Self, other: Self) bool {
            if (self.get_len() != other.get_len()) {
                return false;
            }

            var levels_count : u64 = self.entries.len;
            var are_same = true;

            var i : u64 = 0;
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
   var dict = RocDict(u64,u64).init(testing.allocator);

   var i : u64 = 0;

   while (i < (size * 2)) {
       dict.insert(i, i);

       i += 1;
   }

   i = 0;
   while (i < (size * 2)) {
       const entry = dict.get(i);

       expectEqual(i, entry.?);

       i += 1;
   }
}

test "repeated RocDict.insert" {
   var dict = RocDict(u64,u64).init(testing.allocator);

   const index = 0;
   dict.insert(index, 17);
   dict.insert(index, 49);

   var value : ?u64 = dict.get(index);

   if (value == null) {
       unreachable;
   } else {
       const result : ?u64 = 49;
       expectEqual(result, value);
   }
}

test "RocDict.eq" {
   var fst = RocDict(u64,u64).init(testing.allocator);
   var snd = RocDict(u64,u64).init(testing.allocator);

   const key = 0;
   const value = 30;

   fst.insert(key, value);
   snd.insert(key, value);
   assert(fst.eq(snd));

   var empty = RocDict(u64,u64).init(testing.allocator);
   assert(!fst.eq(empty));

   var trd = RocDict(u64,u64).init(testing.allocator);
   trd.insert(key, value + 1);

   assert(!fst.eq(trd));
}



test "RocDict.get_len" {
   var dict = RocDict(u64,u64).init(testing.allocator);

   const index = 0;

   dict.insert(index, 16);

   const expect_len : u64 = 1;
   expectEqual(expect_len, dict.get_len());

   dict.insert(index, 16);

   expectEqual(expect_len, dict.get_len());

   dict.insert(index + 1, 3);

   expectEqual(expect_len + 1, dict.get_len());
}


test "RocDict.insert" {
    var dict = RocDict(u64,u64).init(testing.allocator);

    const index = 0;
    const value : u64 = 30;

    dict.insert(index, value);

    var result_value : ?u64 = dict.get(index);
    expectEqual(value, result_value.?);

    var expect_len : u64 = 1;
    expectEqual(expect_len, dict.get_len());
}


test "RocDict.get" {
    const empty = RocDict(u64, u64).init(testing.allocator);

    const expect : ?u64 = null;
    expectEqual(expect, empty.get(29));
}


test "RocDict.init" {
    const empty = RocDict(u64, u64).init(testing.allocator);
    expectEqual(empty.get_len(), 0);

    const MadeUpType = struct {
        oneField: u64
    };

    const empty_made_up = RocDict(u64, MadeUpType).init(testing.allocator);

    const expect : u64 = 0;
    expectEqual(expect, empty_made_up.get_len());
}