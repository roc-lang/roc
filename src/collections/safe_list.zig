const std = @import("std");
const testing = std.testing;
const cols = @import("../collections.zig");

// a thing that you have to give the right type... no more "trust me bro!"
pub fn SafeList(comptime T: type) type {
    return struct {
        items: std.ArrayList(T),

        pub const Id = struct { id: u32 };
        pub const Slice = std.ArrayList(T).Slice;
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
            return SafeList{ .items = std.ArrayList(T).init(allocator) };
        }

        pub fn deinit(self: *SafeList(T)) void {
            self.items.deinit();
        }

        pub fn len(self: *SafeList(T)) usize {
            return self.items.items.len;
        }

        pub fn append(self: *SafeList(T), item: T) Id {
            const length = self.len();
            self.items.append(item) catch cols.exit_on_oom;

            return Id{ .id = @as(u32, length) };
        }

        pub fn appendSlice(self: *SafeList(T), items: []const T) Slice {
            const start_length = self.len();
            self.items.appendSlice(items) catch cols.exit_on_oom;

            return self.items.items[start_length..];
        }

        pub fn get(self: *SafeList(T), id: Id) *T {
            return self.items.items[@as(usize, id.id)];
        }
    };
}

pub fn SafeMultiList(comptime T: type) type {
    return struct {
        items: std.MultiArrayList(T),
        allocator: std.mem.Allocator,

        pub const Id = struct { id: u32 };
        pub const Slice = std.MultiArrayList(T).Slice;
        pub const X = std.MultiArrayList(T).Field;

        pub fn init(allocator: std.mem.Allocator) SafeList(T) {
            return SafeList{
                .items = std.MultiArrayList(T){},
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *SafeMultiList(T)) void {
            self.items.deinit();
        }

        pub fn len(self: *SafeMultiList(T)) usize {
            return self.items.items.len;
        }

        pub fn append(self: *SafeMultiList(T), item: T) Id {
            const length = self.len();
            self.items.append(item) catch cols.exit_on_oom;

            return Id{ .id = @as(u32, length) };
        }
    };
}

test "safe list_u32 inserting and getting" {
    var list_u32 = SafeList(u32).init(testing.allocator);
    defer list_u32.deinit();

    try testing.expectEqual(list_u32.len(), 0);

    const id = list_u32.insert(1);

    try testing.expectEqual(list_u32.len(), 1);

    const item = list_u32.get(id);

    try testing.expectEqual(item.* == 1);
}
