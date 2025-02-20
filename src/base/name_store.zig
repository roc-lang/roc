//! An interner for names of entities in a module, e.g. idents, tag names, etc.
const std = @import("std");
const collections = @import("../collections.zig");

const Region = @import("./Region.zig");
const Module = @import("./Module.zig");
const SmallStringInterner = @import("../collections/SmallStringInterner.zig");

const exitOnOom = collections.utils.exitOnOom;

pub fn NameStore(comptime Idx: type) type {
    return struct {
        interner: SmallStringInterner,
        /// An incrementing number that is used to generate unique names within
        /// this module's namespace.
        next_unique_name: u32,

        pub fn init(allocator: std.mem.Allocator) NameStore(Idx) {
            return NameStore(Idx){
                .interner = SmallStringInterner.init(allocator),
                .next_unique_name = 0,
            };
        }

        pub fn deinit(self: *NameStore(Idx)) void {
            self.interner.deinit();
        }

        pub fn insert(self: *NameStore(Idx), name: []u8, region: Region) Idx {
            const idx = self.interner.insert(name, region);

            return @enumFromInt(@intFromEnum(idx));
        }

        pub fn genUnique(self: *NameStore(Idx)) Idx {
            var id = self.next_unique_name;
            self.next_unique_name += 1;

            // Manually render the text into a buffer to avoid allocating
            // a string as the string interner will copy the text anyway.

            var digit_index: u8 = 9;
            // The max u32 value is 4294967295 which is 10 digits
            var str_buffer = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
            while (id > 0) {
                const digit = id % 10;
                str_buffer[digit_index] = @as(u8, @intCast(digit)) + '0';

                id = (id - digit) / 10;
                digit_index -= 1;
            }

            const name = str_buffer[digit_index..];

            const idx = self.interner.insert(name, Region.zero());

            return @enumFromInt(@intFromEnum(idx));
        }

        pub fn namesHaveSameText(
            self: *NameStore(Idx),
            first_idx: Idx,
            second_idx: Idx,
        ) bool {
            return self.interner.indicesHaveSameText(
                @enumFromInt(@intFromEnum(first_idx)),
                @enumFromInt(@intFromEnum(second_idx)),
            );
        }

        pub fn getText(self: *NameStore(Idx), idx: Idx) []u8 {
            return self.interner.getText(@enumFromInt(@intFromEnum(idx)));
        }

        pub fn getRegion(self: *NameStore(Idx), idx: Idx) Region {
            return self.interner.getRegion(@enumFromInt(@intFromEnum(idx)));
        }

        pub const Iterator = struct {
            names: []SmallStringInterner.Idx,

            pub fn next(self: *Iterator) ?Idx {
                if (self.names.len == 0) {
                    return null;
                }

                const idx = self.names[0];
                self.names = self.names[1..];

                return @enumFromInt(@intFromEnum(idx));
            }
        };

        /// Look up text in this store, returning an iterator that walks over all
        /// names in scope.
        pub fn lookup(self: *NameStore(Idx), string: []u8) Iterator {
            return Iterator{ .names = self.interner.lookup(string) };
        }
    };
}
