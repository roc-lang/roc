//! Lists that make it easier to avoid incorrect indexing.
const std = @import("std");
const testing = std.testing;
const exitOnOom = @import("utils.zig").exitOnOom;

/// Wraps a `std.ArrayList` to provide a list that's safer to access
/// with arbitrary indices.
///
/// Use this for values that aren't structs with more than one field.
/// Those values would likely be better stored in a SafeMultiList.
///
/// By default, lists and arrays in Zig are accessed with a `usize`
/// index, which allows for any index to be used with any list. This
/// requires devs to be careful about using indices on the right list
/// and to not look for out-of-bounds values.
///
/// Using a SafeList fixes this as it can only be accessed with a
/// SafeList(T).Idx, which is only created on appending to a SafeList
/// (barring manual usage of macros). An Idx can only be used for lists
/// that hold T's, giving type safety. Also, out-of-bounds errors are
/// less likely since indices are only created for valid list entries.
pub fn SafeList(comptime T: type) type {
    return struct {
        items: std.ArrayList(T),

        /// An index for an item in the list.
        pub const Idx = enum(u32) { _ };

        /// A type-safe slice of the list.
        pub const Slice = std.ArrayList(T).Slice;

        /// A type-safe slice which must have at least one element.
        pub const NonEmptySlice = struct {
            slice: std.ArrayList(T).Slice,

            pub fn makeUnchecked(items: []T) NonEmptySlice {
                return NonEmptySlice{ .slice = items };
            }

            pub fn first(slice: *NonEmptySlice) T {
                return slice.slice[0];
            }
        };

        pub fn init(allocator: std.mem.Allocator) SafeList(T) {
            return SafeList(T){ .items = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: *SafeList(T)) void {
            self.items.deinit();
        }

        pub fn len(self: *SafeList(T)) usize {
            return self.items.items.len;
        }

        pub fn append(self: *SafeList(T), item: T) Idx {
            const length = self.len();
            self.items.append(item) catch exitOnOom();

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        pub fn appendSlice(self: *SafeList(T), items: []const T) Slice {
            const start_length = self.len();
            self.items.appendSlice(items) catch exitOnOom();

            return self.items.items[start_length..];
        }

        pub fn extendFromIter(self: *SafeList(T), iter: anytype) Slice {
            const start_length = self.len();
            while (iter.next()) |item| {
                self.items.append(item) catch exitOnOom();
            }

            return self.items.items[start_length..];
        }

        pub fn get(self: *SafeList(T), id: Idx) T {
            return self.items.items[@as(usize, @intFromEnum(id))];
        }

        pub fn set(self: *SafeList(T), id: Idx, value: T) void {
            self.items.items[@as(usize, @intFromEnum(id))] = value;
        }

        pub const IndexIterator = struct {
            len: usize,
            current: usize,

            pub fn next(iter: *IndexIterator) ?Idx {
                if (iter.len == iter.current) {
                    return null;
                }

                const curr = iter.current;
                iter.current += 1;

                const idx: u32 = @truncate(curr);
                return @enumFromInt(idx);
            }
        };

        pub fn iterIndices(self: *SafeList(T)) IndexIterator {
            return IndexIterator{
                .len = self.len(),
                .current = 0,
            };
        }
    };
}

/// Wraps a `std.ArrayMultiList` to provide a list that's safer to access
/// with arbitrary indices.
///
/// Use this for lists comprising structs with differently-sized fields
/// to make the storage of those fields more compact, otherwise a
/// SafeList may be a simpler container.
///
/// By default, lists and arrays in Zig are accessed with a `usize`
/// index, which allows for any index to be used with any list. This
/// requires devs to be careful about using indices on the right list
/// and to not look for out-of-bounds values.
///
/// Using a SafeMultiList fixes this as it can only be accessed with a
/// SafeMultiList(T).Idx, which is only created on appending to a SafeMultiList
/// (barring manual usage of macros). An Idx can only be used for lists
/// that hold T's, giving type safety. Also, out-of-bounds errors are
/// less likely since indices are only created for valid list entries.
pub fn SafeMultiList(comptime T: type) type {
    return struct {
        items: std.MultiArrayList(T),
        allocator: std.mem.Allocator,

        /// Index of an item in the list.
        pub const Idx = enum(u32) { _ };

        /// A typesafe Slice of the list.
        pub const Slice = std.MultiArrayList(T).Slice;

        pub const Field = std.MultiArrayList(T).Field;

        pub fn init(allocator: std.mem.Allocator) SafeMultiList(T) {
            return SafeMultiList(T){
                .items = std.MultiArrayList(T){},
                .allocator = allocator,
            };
        }

        pub fn field(self: *SafeMultiList(T), comptime field_name: Field) []type {
            return self.items.items(field_name);
        }

        pub fn deinit(self: *SafeMultiList(T)) void {
            self.items.deinit(self.allocator);
        }

        pub fn len(self: *SafeMultiList(T)) usize {
            return self.items.len;
        }

        pub fn append(self: *SafeMultiList(T), item: T) Idx {
            const length = self.len();
            self.items.append(self.allocator, item) catch exitOnOom();

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        pub fn set(self: *SafeMultiList(T), idx: Idx, value: T) void {
            self.items.set(@intFromEnum(idx), value);
        }

        // TODO: consider removing this, or at least renaming to imply this is not a zero-cost operation
        pub fn get(self: *const SafeMultiList(T), idx: Idx) T {
            return self.items.get(@intFromEnum(idx));
        }

        pub fn ensureTotalCapacity(self: *SafeMultiList(T), capacity: usize) void {
            self.items.ensureTotalCapacity(self.allocator, capacity) catch exitOnOom();
        }

        pub const IndexIterator = struct {
            len: usize,
            current: usize,

            pub fn next(iter: *IndexIterator) ?Idx {
                if (iter.len == iter.current) {
                    return null;
                }

                const curr = iter.current;
                iter.current += 1;

                const idx: u32 = @truncate(curr);
                return @enumFromInt(idx);
            }
        };

        pub fn iterIndices(self: *SafeMultiList(T)) IndexIterator {
            return IndexIterator{
                .len = self.len(),
                .current = 0,
            };
        }
    };
}

test "safe list_u32 inserting and getting" {
    var list_u32 = SafeList(u32).init(testing.allocator);
    defer list_u32.deinit();

    try testing.expectEqual(list_u32.len(), 0);

    const id = list_u32.append(1);

    try testing.expectEqual(list_u32.len(), 1);

    const item = list_u32.get(id);

    try testing.expectEqual(item, 1);
}
