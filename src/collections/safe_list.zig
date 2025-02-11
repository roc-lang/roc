const std = @import("std");
const testing = std.testing;
const exit_on_oom = @import("exit_on_oom.zig").exit_on_oom;

/// Wraps a `std.ArrayList` to provide a type-safe interface.
pub fn SafeList(comptime T: type) type {
    return struct {
        items: std.ArrayList(T),

        /// Index of an item in the list.
        pub const Idx = struct { id: u32 };

        /// A typesafe Slice of the list.
        pub const Slice = std.ArrayList(T).Slice;

        /// A typesafe NonEmptySlice, must have at least one element.
        pub const NonEmptySlice = struct {
            slice: std.ArrayList(T).Slice,

            pub fn make_unchecked(items: []T) NonEmptySlice(T) {
                return NonEmptySlice(T){ .slice = items };
            }

            pub fn first(slice: *NonEmptySlice(T)) *T {
                return slice.slice[0];
            }
        };

        pub fn init(allocator: std.mem.Allocator) SafeList(T) {
            return SafeList(T){ .items = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: *SafeList(T)) void {
            self.items.deinit();
        }

        pub fn len(self: *const SafeList(T)) usize {
            return self.items.items.len;
        }

        pub fn append(self: *SafeList(T), item: T) Idx {
            const length = self.len();
            self.items.append(item) catch exit_on_oom();

            return Idx{ .id = @as(u32, @intCast(length)) };
        }

        pub fn appendSlice(self: *SafeList(T), items: []const T) Slice {
            const start_length = self.len();
            self.items.appendSlice(items) catch exit_on_oom();

            return self.items.items[start_length..];
        }

        pub fn get(self: *const SafeList(T), id: Idx) T {
            return self.items.items[@as(usize, id.id)];
        }
    };
}

/// Wraps a `std.MultiArrayList` to provide a type-safe interface.
pub fn SafeMultiList(comptime T: type) type {
    return struct {
        items: std.MultiArrayList(T),
        allocator: std.mem.Allocator,

        /// Index of an item in the list.
        pub const Idx = struct { id: u32 };

        /// A typesafe Slice of the list.
        pub const Slice = std.MultiArrayList(T).Slice;

        /// TODO -- yo what is this?
        pub const Field = std.MultiArrayList(T).Field;

        pub fn init(allocator: std.mem.Allocator) SafeMultiList(T) {
            return SafeMultiList(T){
                .items = std.MultiArrayList(T){},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *SafeMultiList(T)) void {
            self.items.deinit(self.allocator);
        }

        pub fn len(self: *const SafeMultiList(T)) usize {
            return self.items.len;
        }

        pub fn append(self: *SafeMultiList(T), item: T) Idx {
            const length = self.len();
            self.items.append(self.allocator, item) catch exit_on_oom();

            return Idx{ .id = @as(u32, @intCast(length)) };
        }

        pub fn get(self: *const SafeMultiList(T), idx: Idx) T {
            return self.items.get(idx.id);
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
