//! Lists that make it easier to avoid incorrect indexing.

const std = @import("std");
const serialization = @import("serialization");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const SERIALIZATION_ALIGNMENT = serialization.SERIALIZATION_ALIGNMENT;

/// Represents a type safe range in a list; [start, end)
///
/// This is the conceptual equivalent of slice, but since this is based
/// on indexes in the list rather than pointers, it is reliable across
/// (de)serilaization and reallocation of the list.
///
/// This range is inclusive on the lower bound, exclusive on the upper bound.
pub fn SafeRange(comptime Idx: type) type {
    return struct {
        const Self = @This();

        start: Idx,
        count: u32,

        /// An empty range
        pub fn empty() Self {
            return .{ .start = @enumFromInt(0), .count = 0 };
        }

        // Drop first elem from the span, if possible
        pub fn dropFirstElem(self: *Self) void {
            if (self.count == 0) return;
            self.start = @enumFromInt(@intFromEnum(self.start) + 1);
            self.count -= 1;
        }

        /// Get the length of a range slice
        pub fn len(self: @This()) u32 {
            return self.count;
        }

        /// Get the last index in the range
        pub fn end(self: @This()) Idx {
            return @enumFromInt(@intFromEnum(self.start) + self.count);
        }

        /// Return whether the range is empty
        pub fn isEmpty(self: @This()) bool {
            return self.count == 0;
        }

        /// Get the length of a range slice
        pub fn iterIndices(self: @This()) IndexIterator {
            return IndexIterator{
                .end = @intFromEnum(self.start) + self.count,
                .current = @intFromEnum(self.start),
            };
        }

        /// An iterator over the indices of all elements in a list.
        pub const IndexIterator = struct {
            end: u32,
            current: u32,

            /// Get the next index from this iterator, or `null` if the iterator is finished.
            pub fn next(iter: *IndexIterator) ?Idx {
                if (iter.end == iter.current) {
                    return null;
                }

                const curr = iter.current;
                iter.current += 1;

                const idx: u32 = @truncate(curr);
                return @enumFromInt(idx);
            }
        };
    };
}

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
        items: std.ArrayListUnmanaged(T) = .{},

        /// An index for an item in the list.
        pub const Idx = enum(u32) { _ };

        /// A non-type-safe slice of the list.
        pub const Slice = std.ArrayListUnmanaged(T).Slice;

        /// A type-safe range of the list.
        pub const Range = SafeRange(Idx);

        /// A type-safe range which must have at least one element.
        pub const NonEmptyRange = struct {
            nonempty: Range,
        };

        /// Initialize the `SafeList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!SafeList(T) {
            return .{
                .items = try std.ArrayListUnmanaged(T).initCapacity(gpa, capacity),
            };
        }

        /// Deinitialize the memory of this `SafeList`.
        pub fn deinit(self: *SafeList(T), gpa: Allocator) void {
            self.items.deinit(gpa);
        }

        /// Get the length of this list.
        pub fn len(self: *const SafeList(T)) u32 {
            return @intCast(self.items.items.len);
        }

        /// Add an item to the end of this list.
        pub fn append(self: *SafeList(T), gpa: Allocator, item: T) std.mem.Allocator.Error!Idx {
            const length = self.len();
            try self.items.append(gpa, item);

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        /// Create a range from the provided idx to the end of the list
        pub fn rangeToEnd(self: *SafeList(T), start_int: u32) Range {
            const len_int = self.len();
            std.debug.assert(start_int <= len_int);
            return Range{ .start = @enumFromInt(start_int), .count = @intCast(len_int - start_int) };
        }

        /// Add all the items in a slice to the end of this list.
        pub fn appendSlice(self: *SafeList(T), gpa: Allocator, items: []const T) std.mem.Allocator.Error!Range {
            const start_length = self.len();
            try self.items.appendSlice(gpa, items);
            const end_length = self.len();
            return Range{ .start = @enumFromInt(start_length), .count = @intCast(end_length - start_length) };
        }

        /// Extend this list with all items generated by an iterator.
        pub fn extendFromIter(self: *SafeList(T), gpa: Allocator, iter_extend: anytype) std.mem.Allocator.Error!Range {
            const start_length = self.len();
            while (iter_extend.next()) |item| {
                try self.items.append(gpa, item);
            }
            const end_length = self.len();
            return Range{ .start = @enumFromInt(start_length), .count = @intCast(end_length - start_length) };
        }

        /// Convert a range to a slice
        pub fn sliceRange(self: *const SafeList(T), range: Range) Slice {
            const start: usize = @intFromEnum(range.start);
            const end: usize = start + range.count;

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.items.items.len);

            return self.items.items[start..end];
        }

        /// Get an item from this list without worrying about out-of-bounds errors.
        pub fn get(self: *const SafeList(T), id: Idx) *T {
            return &self.items.items[@as(usize, @intFromEnum(id))];
        }

        /// Set the value of an item in this list without worrying about out-of-bounds errors.
        pub fn set(self: *const SafeList(T), id: Idx, value: T) void {
            self.items.items[@as(usize, @intFromEnum(id))] = value;
        }

        /// Append this SafeList to an iovec writer for serialization
        pub fn appendToIovecs(self: *const SafeList(T), writer: anytype) !usize {
            // Create a mutable copy on the stack (properly aligned)
            var list_copy = self.*;

            // Create a buffer for the final serialized struct
            const list_copy_buffer = try writer.allocator.alloc(u8, @sizeOf(SafeList(T)));

            // Track this allocation so it gets freed when writer is deinitialized
            try writer.owned_buffers.append(list_copy_buffer);

            // Serialize items array
            const items_offset = if (self.items.items.len > 0) blk: {
                const bytes = std.mem.sliceAsBytes(self.items.items);
                const offset = try writer.appendBytes(T, bytes);
                break :blk offset;
            } else 0;

            // Update pointer in the copy to use offset
            list_copy.items.items.ptr = if (items_offset == 0)
                @ptrFromInt(serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL)
            else
                @ptrFromInt(items_offset);
            list_copy.items.items.len = self.items.items.len;

            // Copy the modified struct into the buffer
            @memcpy(list_copy_buffer, std.mem.asBytes(&list_copy));

            // Now that all pointers have been converted to offsets, add the copy to iovecs
            const struct_offset = try writer.appendBytes(SafeList(T), list_copy_buffer);

            return struct_offset;
        }

        /// Relocate all pointers in this SafeList by the given offset
        pub fn relocate(self: *SafeList(T), offset: isize) void {
            // Relocate items array
            if (self.items.items.len > 0) {
                const old_ptr_val = @intFromPtr(self.items.items.ptr);

                // Skip relocation if this is a sentinel value
                if (old_ptr_val != serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL) {
                    const new_ptr_val = @as(usize, @intCast(@as(isize, @intCast(old_ptr_val)) + offset));
                    self.items.items.ptr = @ptrFromInt(new_ptr_val);

                    // After relocation, the pointer should have the correct alignment for the type T.
                    // This catches alignment errors that might have been introduced during serialization.
                    std.debug.assert((new_ptr_val & (@alignOf(T) - 1)) == 0);
                }
            }
        }

        /// An iterator over all the indices in this list.
        pub const IndexIterator = struct {
            len: usize,
            current: usize,

            pub fn next(self: *IndexIterator) ?Idx {
                if (self.len == self.current) {
                    return null;
                }

                const curr = self.current;
                self.current += 1;

                const idx: u32 = @truncate(curr);
                return @enumFromInt(idx);
            }
        };

        /// Iterate over all the indices of the items in this list.
        pub fn iterIndices(self: *const SafeList(T)) IndexIterator {
            return IndexIterator{
                .len = @intCast(self.len()),
                .current = 0,
            };
        }

        /// An iterator over all the indices in this list.
        pub const Iterator = struct {
            array: *const SafeList(T),
            len: u32,
            current: Idx,

            pub fn next(self: *Iterator) ?T {
                const cur_idx = self.current;
                const cur_int = @intFromEnum(cur_idx);
                if (self.len == cur_int) {
                    return null;
                }
                self.current = @enumFromInt(cur_int + 1);
                return self.array.get(cur_idx).*;
            }

            pub fn count(self: *Iterator) u32 {
                return self.len - @intFromEnum(self.current);
            }

            pub fn shift(self: *Iterator) void {
                const cur_int = @intFromEnum(self.current);
                if (cur_int < self.len) {
                    self.current = @as(Idx, @enumFromInt(cur_int + 1));
                    self.len -= 1;
                }
            }
        };

        /// Iterate over the elements in a span
        pub fn iterRange(self: *const SafeList(T), range: Range) Iterator {
            return Iterator{
                .array = self,
                .len = @intFromEnum(range.start) + range.count,
                .current = range.start,
            };
        }

        /// Iterate over all items in this list.
        pub fn iter(self: *const SafeList(T)) Iterator {
            return Iterator{
                .array = self,
                .len = self.len(),
                .current = @enumFromInt(0),
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
        items: std.MultiArrayList(T) = .{},

        /// Index of an item in the list.
        pub const Idx = enum(u32) { zero = 0, _ };

        /// A non-type-safe slice of the list.
        pub const Slice = std.MultiArrayList(T).Slice;

        /// A type-safe slice of the list.
        pub const Range = SafeRange(Idx);

        /// One of the comptime fields in the list's wrapped type.
        pub const Field = std.MultiArrayList(T).Field;

        /// A slice of all values for a specific field of the wrapped type.
        pub fn field(self: *const SafeMultiList(T), comptime field_name: Field) []type {
            return self.items.items(field_name);
        }

        /// The value for a specific field at a specific index in the list.
        pub fn fieldItem(self: *const SafeMultiList(T), comptime field_name: Field, idx: Idx) type {
            return self.items.items(field_name)[@as(usize, @intFromEnum(idx))];
        }

        /// Initialize the `SafeMultiList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!SafeMultiList(T) {
            var items = std.MultiArrayList(T){};
            try items.ensureTotalCapacity(gpa, capacity);
            return .{
                .items = items,
            };
        }

        /// Deinitialize the memory of a `SafeMultiList`.
        pub fn deinit(self: *SafeMultiList(T), gpa: Allocator) void {
            self.items.deinit(gpa);
        }

        /// Get the length of this list.
        pub fn len(self: *const SafeMultiList(T)) u32 {
            return @intCast(self.items.len);
        }

        /// Create a range from the provided idx to the end of the list
        pub fn rangeToEnd(self: *SafeMultiList(T), start_int: u32) Range {
            const len_int = self.len();
            std.debug.assert(start_int <= len_int);
            return Range{ .start = @enumFromInt(start_int), .count = @intCast(len_int - start_int) };
        }

        /// Add a new item to the end of this list.
        pub fn append(self: *SafeMultiList(T), gpa: Allocator, item: T) std.mem.Allocator.Error!Idx {
            const length = self.len();
            try self.items.append(gpa, item);

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        pub fn appendSlice(self: *SafeMultiList(T), gpa: Allocator, elems: []const T) std.mem.Allocator.Error!Range {
            if (elems.len == 0) {
                return .{ .start = .zero, .count = 0 };
            }
            const start_length = self.len();
            try self.items.ensureUnusedCapacity(gpa, elems.len);
            for (elems) |elem| {
                self.items.appendAssumeCapacity(elem);
            }
            const end_length = self.len();
            return Range{ .start = @enumFromInt(start_length), .count = @intCast(end_length - start_length) };
        }

        /// Convert a range to a slice
        pub fn sliceRange(self: *const SafeMultiList(T), range: Range) Slice {
            const start: usize = @intFromEnum(range.start);
            const end: usize = start + range.count;

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.items.len);

            const base = self.items.slice();

            var new_ptrs: [base.ptrs.len][*]u8 = undefined;

            // This has to be inline, so `cur_field` can be known at comptime
            inline for (0..base.ptrs.len) |i| {
                const cur_field = @as(Field, @enumFromInt(i));
                const cur_items = base.items(cur_field);

                new_ptrs[i] = if (cur_items.len == 0)
                    @as([*]u8, @ptrFromInt(@alignOf(T)))
                else if (start >= cur_items.len)
                    @ptrCast(cur_items.ptr + cur_items.len)
                else
                    @ptrCast(&cur_items[start]);
            }

            return .{
                .ptrs = new_ptrs,
                .len = end - start,
                .capacity = base.capacity - start,
            };
        }

        /// Set the value of an element in this list.
        pub fn set(self: *SafeMultiList(T), idx: Idx, value: T) void {
            self.items.set(@intFromEnum(idx), value);
        }

        // TODO: consider removing this, or at least renaming to imply this is not a zero-cost operation
        pub fn get(self: *const SafeMultiList(T), idx: Idx) T {
            return self.items.get(@intFromEnum(idx));
        }

        /// Make sure that the backing array has at least capacity for the specified number of elements.
        pub fn ensureTotalCapacity(self: *SafeMultiList(T), gpa: Allocator, capacity: usize) std.mem.Allocator.Error!void {
            try self.items.ensureTotalCapacity(gpa, capacity);
        }

        /// An iterator over the indices of all elements in a list.
        pub const IndexIterator = struct {
            len: usize,
            current: usize,

            /// Get the next index from this iterator, or `null` if the iterator is finished.
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

        /// Iterator over all indices in this list.
        pub fn iterIndices(self: *const SafeMultiList(T)) IndexIterator {
            return IndexIterator{
                .len = self.len(),
                .current = 0,
            };
        }

        /// Append this SafeMultiList to an iovec writer for serialization
        pub fn appendToIovecs(self: *const SafeMultiList(T), writer: anytype) !usize {
            // Create a mutable copy on the stack (properly aligned)
            var list_items_copy = self.items;

            if (list_items_copy.len == 0) {
                list_items_copy.bytes = @ptrFromInt(serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL);
                list_items_copy.capacity = 0;
            } else {
                // MultiArrayList stores all data in a single bytes field
                // Calculate the actual bytes used for the current length
                const used_bytes_len = std.MultiArrayList(T).capacityInBytes(list_items_copy.len);
                const bytes_slice = list_items_copy.bytes[0..used_bytes_len];

                // Use T alignment to ensure proper alignment for the data
                const offset = try writer.appendBytes(T, bytes_slice);

                // Set the bytes pointer to the serialized offset
                list_items_copy.bytes = @ptrFromInt(offset);
                // Set capacity equal to length since we only serialize used data
                list_items_copy.capacity = list_items_copy.len;
            }

            // Create a buffer for the final serialized struct
            const list_copy_buffer = try writer.allocator.alloc(u8, @sizeOf(std.MultiArrayList(T)));

            // Track this allocation so it gets freed when writer is deinitialized
            try writer.owned_buffers.append(list_copy_buffer);

            // Copy the modified struct into the buffer
            @memcpy(list_copy_buffer, std.mem.asBytes(&list_items_copy));

            // Now that all pointers have been converted to offsets, add the copy to iovecs
            const struct_offset = try writer.appendBytes(std.MultiArrayList(T), list_copy_buffer);

            return struct_offset;
        }

        /// Relocate all pointers in this SafeMultiList by the given offset
        pub fn relocate(self: *SafeMultiList(T), offset: isize) void {
            // MultiArrayList stores all data in a single bytes field
            // The field is: bytes: [*]align(@alignOf(T)) u8
            if (self.items.capacity > 0) {
                const old_ptr_val = @intFromPtr(self.items.bytes);

                if (old_ptr_val != serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL) {
                    const new_ptr_val = @as(usize, @intCast(@as(isize, @intCast(old_ptr_val)) + offset));
                    self.items.bytes = @ptrFromInt(new_ptr_val);

                    // After relocation, the pointer should have the correct alignment for the type T.
                    std.debug.assert((new_ptr_val & (@alignOf(T) - 1)) == 0);
                }
            }
        }
    };
}

test "SafeList(32) inserting and getting" {
    const gpa = testing.allocator;

    var list_u32 = SafeList(u32){};
    defer list_u32.deinit(gpa);

    try testing.expectEqual(list_u32.len(), 0);

    const id = try list_u32.append(gpa, 1);

    try testing.expectEqual(list_u32.len(), 1);

    const item = list_u32.get(id);

    try testing.expectEqual(item.*, 1);
}

test "SafeList(u8) appendSlice" {
    const gpa = testing.allocator;

    var list = SafeList(u8){};
    defer list.deinit(gpa);

    const rangeA = try list.appendSlice(gpa, &[_]u8{ 'a', 'b', 'c', 'd' });
    try testing.expectEqual(0, @intFromEnum(rangeA.start));
    try testing.expectEqual(4, @intFromEnum(rangeA.end()));

    const rangeB = try list.appendSlice(gpa, &[_]u8{ 'd', 'e', 'f', 'g' });
    try testing.expectEqual(4, @intFromEnum(rangeB.start));
    try testing.expectEqual(8, @intFromEnum(rangeB.end()));
}

test "SafeList(u8) sliceRange" {
    const gpa = testing.allocator;

    var list = SafeList(u8){};
    defer list.deinit(gpa);

    const rangeA = try list.appendSlice(gpa, &[_]u8{ 'a', 'b', 'c', 'd' });
    const sliceA = list.sliceRange(rangeA);
    try testing.expectEqual('a', sliceA[0]);
    try testing.expectEqual('d', sliceA[3]);

    const rangeB = SafeList(u8).Range{ .start = @enumFromInt(2), .count = 2 };
    const sliceB = list.sliceRange(rangeB);
    try testing.expectEqual('c', sliceB[0]);
    try testing.expectEqual('d', sliceB[1]);
}

test "SafeMultiList(u8) appendSlice" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 3);
    defer multilist.deinit(gpa);

    const rangeA = try multilist.appendSlice(gpa, &[_]Struct{ .{ .num = 100, .char = 'a' }, .{ .num = 200, .char = 'b' }, .{ .num = 300, .char = 'd' } });
    try testing.expectEqual(0, @intFromEnum(rangeA.start));
    try testing.expectEqual(3, @intFromEnum(rangeA.end()));

    const rangeB = try multilist.appendSlice(gpa, &[_]Struct{ .{ .num = 400, .char = 'd' }, .{ .num = 500, .char = 'e' }, .{ .num = 600, .char = 'f' } });
    try testing.expectEqual(3, @intFromEnum(rangeB.start));
    try testing.expectEqual(6, @intFromEnum(rangeB.end()));
}

test "SafeMultiList(u8) sliceRange" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 3);
    defer multilist.deinit(gpa);

    const range_a = try multilist.appendSlice(gpa, &[_]Struct{ .{ .num = 100, .char = 'a' }, .{ .num = 200, .char = 'b' }, .{ .num = 300, .char = 'c' } });
    const slice_a = multilist.sliceRange(range_a);

    const num_slice_a = slice_a.items(.num);
    try testing.expectEqual(3, num_slice_a.len);
    try testing.expectEqual(100, num_slice_a[0]);
    try testing.expectEqual(200, num_slice_a[1]);
    try testing.expectEqual(300, num_slice_a[2]);

    const char_slice_a = slice_a.items(.char);
    try testing.expectEqual(3, char_slice_a.len);
    try testing.expectEqual('a', char_slice_a[0]);
    try testing.expectEqual('b', char_slice_a[1]);
    try testing.expectEqual('c', char_slice_a[2]);

    const range_b = StructMultiList.Range{ .start = @enumFromInt(1), .count = 1 };
    const slice_b = multilist.sliceRange(range_b);

    const num_slice_b = slice_b.items(.num);
    try testing.expectEqual(1, num_slice_b.len);
    try testing.expectEqual(200, num_slice_b[0]);

    const char_slice_b = slice_b.items(.char);
    try testing.expectEqual(1, char_slice_b.len);
    try testing.expectEqual('b', char_slice_b[0]);
}

test "SafeMultiList empty range at end" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 5);
    defer multilist.deinit(gpa);

    // Add 5 items to fill the list
    _ = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
        .{ .num = 500, .char = 'e' },
    });

    // Create an empty range at the end (start=5, end=5 for a list of length 5)
    const empty_range = StructMultiList.Range{ .start = @enumFromInt(5), .count = 0 };
    const empty_slice = multilist.sliceRange(empty_range);

    // The slice should be empty
    const num_slice = empty_slice.items(.num);
    try testing.expectEqual(0, num_slice.len);

    const char_slice = empty_slice.items(.char);
    try testing.expectEqual(0, char_slice.len);
}

test "SafeList comprehensive serialization framework test" {
    const gpa = testing.allocator;

    var list = try SafeList(u32).initCapacity(gpa, 8);
    defer list.deinit(gpa);

    // Add various test data including edge cases
    _ = try list.append(gpa, 0); // minimum value
    _ = try list.append(gpa, 42);
    _ = try list.append(gpa, 123);
    _ = try list.append(gpa, 0xFFFFFFFF); // maximum value
    _ = try list.append(gpa, 999);

    // Test serialization using the testing framework
    try serialization.testing.testSerialization(SafeList(u32), &list, gpa);
}

test "SafeList empty list serialization framework test" {
    const gpa = testing.allocator;

    var empty_list = SafeList(u32){};
    defer empty_list.deinit(gpa);

    try serialization.testing.testSerialization(SafeList(u32), &empty_list, gpa);
}

test "SafeMultiList comprehensive serialization framework test" {
    const gpa = testing.allocator;

    const TestStruct = struct {
        x: u32,
        y: u32,
    };

    var list = try SafeMultiList(TestStruct).initCapacity(gpa, 4);
    defer list.deinit(gpa);

    // Add various test data including edge cases
    _ = try list.append(gpa, TestStruct{ .x = 0, .y = 0 }); // minimum values
    _ = try list.append(gpa, TestStruct{ .x = 42, .y = 123 });
    _ = try list.append(gpa, TestStruct{ .x = 0xFFFFFFFF, .y = 0xFFFFFFFF }); // maximum values

    // Test serialization using the testing framework
    try serialization.testing.testSerialization(SafeMultiList(TestStruct), &list, gpa);
}

test "SafeMultiList empty list serialization framework test" {
    const gpa = testing.allocator;

    const TestStruct = struct {
        val: u32,
    };

    var empty_list = SafeMultiList(TestStruct){};
    defer empty_list.deinit(gpa);

    try serialization.testing.testSerialization(SafeMultiList(TestStruct), &empty_list, gpa);
}

test "SafeMultiList internal pointer verification after serialization" {
    const gpa = testing.allocator;

    const TestStruct = struct {
        a: u16,
        b: u32,
        c: u8,
    };

    var list = try SafeMultiList(TestStruct).initCapacity(gpa, 5);
    defer list.deinit(gpa);

    // Add test data with different values for each field
    _ = try list.append(gpa, TestStruct{ .a = 100, .b = 1000, .c = 10 });
    _ = try list.append(gpa, TestStruct{ .a = 200, .b = 2000, .c = 20 });
    _ = try list.append(gpa, TestStruct{ .a = 300, .b = 3000, .c = 30 });

    // Serialize and deserialize
    var writer = serialization.IovecWriter.init(gpa);
    defer writer.deinit();

    const list_offset = try list.appendToIovecs(&writer);
    const serialized_data = try writer.collectIntoBuffer();
    defer gpa.free(serialized_data);

    // Get the deserialized list
    const base_addr = @intFromPtr(serialized_data.ptr);

    // The buffer might be misaligned, so we can't just cast a pointer.
    // We must copy the bytes into an aligned stack variable.
    var restored: SafeMultiList(TestStruct) = undefined;
    const list_in_buffer = serialized_data[list_offset..];
    @memcpy(std.mem.asBytes(&restored), list_in_buffer[0..@sizeOf(SafeMultiList(TestStruct))]);

    // Relocate the pointers
    restored.relocate(@as(isize, @intCast(base_addr)));

    // Verify basic properties
    try testing.expectEqual(@as(u32, 3), restored.len());
    try testing.expectEqual(restored.len(), restored.items.capacity); // Capacity should equal length

    // Verify the internal pointers look reasonable
    const restored_slice = restored.items.slice();

    // The bytes pointer should point within our serialized buffer
    const bytes_addr = @intFromPtr(restored.items.bytes);
    const buffer_start = @intFromPtr(serialized_data.ptr);
    const buffer_end = buffer_start + serialized_data.len;
    try testing.expect(bytes_addr >= buffer_start);
    try testing.expect(bytes_addr < buffer_end);

    // Verify we can access the fields correctly through the slice
    const a_values = restored_slice.items(.a);
    const b_values = restored_slice.items(.b);
    const c_values = restored_slice.items(.c);

    try testing.expectEqual(@as(usize, 3), a_values.len);
    try testing.expectEqual(@as(usize, 3), b_values.len);
    try testing.expectEqual(@as(usize, 3), c_values.len);

    // Verify the data is correct
    try testing.expectEqual(@as(u16, 100), a_values[0]);
    try testing.expectEqual(@as(u32, 1000), b_values[0]);
    try testing.expectEqual(@as(u8, 10), c_values[0]);

    try testing.expectEqual(@as(u16, 200), a_values[1]);
    try testing.expectEqual(@as(u32, 2000), b_values[1]);
    try testing.expectEqual(@as(u8, 20), c_values[1]);

    try testing.expectEqual(@as(u16, 300), a_values[2]);
    try testing.expectEqual(@as(u32, 3000), b_values[2]);
    try testing.expectEqual(@as(u8, 30), c_values[2]);
}
