//! Lists that use 1-based indexing to allow 0 to be used as a sentinel value.
//!
//! This is similar to SafeList but specifically designed for cases where 0 needs
//! to be reserved as a special value (e.g., for layout stores that use 0 as "no layout").

const std = @import("std");
const utils = @import("utils.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const exitOnOom = utils.exitOnOom;

/// Represents a type safe range in a list; [start, end)
///
/// This is the conceptual equivalent of slice, but since this is based
/// on indexes in the list rather than pointers, it is reliable across
/// (de)serialization and reallocation of the list.
///
/// This range is inclusive on the lower bound, exclusive on the upper bound.
/// Note: These use 1-based indexing, so start/end of 0 means empty range.
pub fn SentinelRange(comptime Idx: type) type {
    return struct {
        const Self = @This();

        start: Idx,
        end: Idx,

        /// An empty range
        pub const empty: Self = .{ .start = @enumFromInt(0), .end = @enumFromInt(0) };

        /// Get the length of a range slice
        pub fn len(self: @This()) usize {
            return @intFromEnum(self.end) - @intFromEnum(self.start);
        }
    };
}

/// A range that's guaranteed to have at least one element
pub const NonEmptyRange = struct {
    start: u32,
    /// This count will never be zero
    count: u32,

    pub fn init(start: u32, count: u32) !NonEmptyRange {
        if (count == 0) {
            return error.EmptyRange;
        }
        return NonEmptyRange{ .start = start, .count = count };
    }

    /// Convert to a SentinelRange with the given index type
    pub fn toRange(self: NonEmptyRange, comptime Idx: type) SentinelRange(Idx) {
        return SentinelRange(Idx){
            .start = @enumFromInt(self.start),
            .end = @enumFromInt(self.start + self.count),
        };
    }
};

/// A list that uses 1-based indexing, allowing 0 to be used as a sentinel value.
///
/// This is useful when you need to distinguish between "no value" (0) and
/// "first value" (1). The implementation stores an offset pointer that points
/// to element -1, so that index 1 maps to the actual first element.
pub fn SentinelList(comptime T: type) type {
    return struct {
        /// Points to element -1 of the actual allocation
        /// This allows index 1 to map to the first real element
        items_offset: [*]T = undefined,
        /// Number of items in the list (not including the dummy element)
        length: usize = 0,
        /// Total capacity (not including the dummy element)
        capacity: usize = 0,
        /// Allocator used for this list
        allocator: Allocator = undefined,

        const Self = @This();

        /// An index for an item in the list.
        /// Value 0 is reserved as a sentinel (no value)
        /// Valid indices start from 1
        pub const Idx = enum(u32) { _ };

        /// A type-safe range of the list.
        pub const Range = SentinelRange(Idx);

        /// A type-safe range which must have at least one element.
        pub const NonEmptyRange = struct {
            range: Range,
        };

        /// Initialize the `SentinelList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) Self {
            if (capacity == 0) {
                return Self{
                    .allocator = gpa,
                };
            }

            // Allocate space for capacity + 1 elements (the +1 is for the dummy element at index 0)
            const ptr = gpa.alloc(T, capacity + 1) catch |err| exitOnOom(err);

            return Self{
                // Point to element -1 so that index 1 maps to the first real element
                .items_offset = @ptrCast(&ptr[1]),
                .len = 0,
                .capacity = capacity,
                .allocator = gpa,
            };
        }

        /// Deinitialize the memory of this `SentinelList`.
        pub fn deinit(self: *Self) void {
            if (self.capacity == 0) return;

            // Get back to the real allocation start
            const real_ptr: [*]T = @ptrFromInt(@intFromPtr(self.items_offset) - @sizeOf(T));
            const slice = real_ptr[0 .. self.capacity + 1];
            self.allocator.free(slice);

            self.* = .{};
        }

        /// Get the length of this list.
        pub fn len(self: *const Self) usize {
            return self.length;
        }

        /// Add an item to the end of this list.
        /// Returns a 1-based index.
        pub fn append(self: *Self, item: T) Idx {
            self.ensureUnusedCapacity(1);

            const new_idx = self.length + 1; // 1-based indexing
            self.items_offset[new_idx] = item;
            self.length += 1;

            return @enumFromInt(@as(u32, @intCast(new_idx)));
        }

        /// Add all the items in a slice to the end of this list.
        /// Returns a range with 1-based indices.
        pub fn appendSlice(self: *Self, items: []const T) Range {
            if (items.len == 0) {
                return Range.empty;
            }

            self.ensureUnusedCapacity(items.len);

            const start_idx = self.length + 1; // 1-based indexing
            for (items, 0..) |item, i| {
                self.items_offset[start_idx + i] = item;
            }
            self.length += items.len;
            const end_idx = self.length + 1; // exclusive end, 1-based

            return Range{ .start = @enumFromInt(@as(u32, @intCast(start_idx))), .end = @enumFromInt(@as(u32, @intCast(end_idx))) };
        }

        /// Ensure there's capacity for at least `count` more items
        fn ensureUnusedCapacity(self: *Self, count: usize) void {
            const needed_capacity = self.length + count;
            if (needed_capacity <= self.capacity) return;

            // Calculate new capacity (grow by 1.5x or to needed capacity, whichever is larger)
            var new_capacity = self.capacity;
            if (new_capacity == 0) {
                new_capacity = 16; // Initial capacity
            }
            while (new_capacity < needed_capacity) {
                new_capacity = (new_capacity * 3) / 2;
            }

            // Allocate new buffer (+1 for dummy element)
            const new_ptr = self.allocator.alloc(T, new_capacity + 1) catch |err| exitOnOom(err);

            // Copy existing items if any
            if (self.length > 0) {
                const old_real_ptr: [*]T = @ptrFromInt(@intFromPtr(self.items_offset) - @sizeOf(T));
                // Copy the dummy element and all real elements
                @memcpy(new_ptr[0 .. self.length + 1], old_real_ptr[0 .. self.length + 1]);

                // Free old allocation
                const old_slice = old_real_ptr[0 .. self.capacity + 1];
                self.allocator.free(old_slice);
            }

            // Update to new buffer
            self.items_offset = @ptrCast(&new_ptr[1]);
            self.capacity = new_capacity;
        }

        /// Convert a range to a slice
        /// Note: The returned slice uses 0-based indexing even though the range uses 1-based
        pub fn rangeToSlice(self: *const Self, range: Range) []T {
            const start: usize = @intFromEnum(range.start);
            const end: usize = @intFromEnum(range.end);

            std.debug.assert(start > 0 or (start == 0 and end == 0)); // Either valid range or empty
            std.debug.assert(start <= end);
            std.debug.assert(end <= self.length + 1); // end is exclusive and 1-based

            if (start == 0) {
                return &[_]T{}; // Empty slice
            }

            // Convert from 1-based range to 0-based slice indexing
            return self.items_offset[start..end];
        }

        /// Get an item from this list without worrying about out-of-bounds errors.
        /// Takes a 1-based index.
        pub fn get(self: *const Self, id: Idx) *T {
            const idx = @intFromEnum(id);
            std.debug.assert(idx > 0 and idx <= self.length);
            return &self.items_offset[idx];
        }

        /// Set the value of an item in this list without worrying about out-of-bounds errors.
        /// Takes a 1-based index.
        pub fn set(self: *const Self, id: Idx, value: T) void {
            const idx = @intFromEnum(id);
            std.debug.assert(idx > 0 and idx <= self.length);
            self.items_offset[idx] = value;
        }

        /// An iterator over all the indices in this list.
        pub const IndexIterator = struct {
            total: usize,
            current: usize,

            pub fn next(iter: *IndexIterator) ?Idx {
                if (iter.current >= iter.total) {
                    return null;
                }

                iter.current += 1;
                return @enumFromInt(@as(u32, @intCast(iter.current)));
            }
        };

        /// Iterate over all the indices of the items in this list.
        /// Returns 1-based indices.
        pub fn iterIndices(self: *const Self) IndexIterator {
            return IndexIterator{
                .total = self.length,
                .current = 0,
            };
        }
    };
}

/// A multi-list variant that uses 1-based indexing
pub fn SentinelMultiList(comptime T: type) type {
    return struct {
        /// The fields are stored in separate arrays pointed to by items_offset
        /// Each field array has its pointer offset by 1 element
        fields: std.MultiArrayList(T).Slice = .{
            .ptrs = undefined,
            .len = 0,
            .capacity = 0,
        },
        /// Allocator used for this list
        allocator: Allocator = undefined,

        const Self = @This();

        /// Index of an item in the list (1-based, 0 is sentinel)
        pub const Idx = enum(u32) { _ };

        /// A type-safe slice of the list.
        pub const Range = SentinelRange(Idx);

        /// One of the comptime fields in the list's wrapped type.
        pub const Field = std.meta.FieldEnum(T);

        /// Initialize the `SentinelMultiList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) Self {
            var result = Self{
                .allocator = gpa,
            };

            if (capacity > 0) {
                result.ensureTotalCapacity(capacity);
            }

            return result;
        }

        /// Deinitialize the memory of a `SentinelMultiList`.
        pub fn deinit(self: *Self) void {
            if (self.fields.capacity == 0) return;

            const field_info = @typeInfo(T).Struct.fields;
            inline for (field_info, 0..) |field_meta, i| {
                const field_ptr = self.fields.ptrs[i];
                if (@sizeOf(field_meta.type) > 0) {
                    // Get back to the real allocation start
                    const real_ptr: [*]field_meta.type = @ptrFromInt(@intFromPtr(field_ptr) - @sizeOf(field_meta.type));
                    const slice = real_ptr[0 .. self.fields.capacity + 1];
                    self.allocator.free(slice);
                }
            }

            self.* = .{ .allocator = self.allocator };
        }

        /// Get the length of this list.
        pub fn len(self: *const Self) usize {
            return self.fields.len;
        }

        /// Add a new item to the end of this list.
        /// Returns a 1-based index.
        pub fn append(self: *Self, item: T) Idx {
            self.ensureUnusedCapacity(1);

            const new_idx = self.fields.len + 1; // 1-based indexing
            self.setAtIndex(new_idx, item);
            self.fields.len += 1;

            return @enumFromInt(@as(u32, @intCast(new_idx)));
        }

        /// Add all items from a slice
        pub fn appendSlice(self: *Self, items: []const T) Range {
            if (items.len == 0) {
                return Range.empty;
            }

            self.ensureUnusedCapacity(items.len);

            const start_idx = self.fields.len + 1; // 1-based indexing
            for (items, 0..) |item, i| {
                self.setAtIndex(start_idx + i, item);
            }
            self.fields.len += items.len;
            const end_idx = self.fields.len + 1; // exclusive end, 1-based

            return Range{ .start = @enumFromInt(@as(u32, @intCast(start_idx))), .end = @enumFromInt(@as(u32, @intCast(end_idx))) };
        }

        /// Set item at raw index (used internally)
        fn setAtIndex(self: *Self, idx: usize, item: T) void {
            const field_info = @typeInfo(T).Struct.fields;
            inline for (field_info, 0..) |field_meta, i| {
                if (@sizeOf(field_meta.type) > 0) {
                    const field_ptr = self.fields.ptrs[i];
                    const typed_ptr: [*]field_meta.type = @ptrCast(@alignCast(field_ptr));
                    typed_ptr[idx] = @field(item, field_meta.name);
                }
            }
        }

        /// Get item at index
        pub fn get(self: *const Self, id: Idx) T {
            const idx = @intFromEnum(id);
            std.debug.assert(idx > 0 and idx <= self.fields.len);

            var result: T = undefined;
            const field_info = @typeInfo(T).Struct.fields;
            inline for (field_info, 0..) |field_meta, i| {
                if (@sizeOf(field_meta.type) > 0) {
                    const field_ptr = self.fields.ptrs[i];
                    const typed_ptr: [*]const field_meta.type = @ptrCast(@alignCast(field_ptr));
                    @field(result, field_meta.name) = typed_ptr[idx];
                }
            }
            return result;
        }

        /// Set the value of an element in this list.
        pub fn set(self: *Self, id: Idx, value: T) void {
            const idx = @intFromEnum(id);
            std.debug.assert(idx > 0 and idx <= self.fields.len);
            self.setAtIndex(idx, value);
        }

        /// Ensure capacity for at least this many total items
        pub fn ensureTotalCapacity(self: *Self, new_capacity: usize) void {
            if (new_capacity <= self.fields.capacity) return;
            self.growCapacity(new_capacity);
        }

        /// Ensure capacity for at least this many additional items
        fn ensureUnusedCapacity(self: *Self, count: usize) void {
            const needed = self.fields.len + count;
            if (needed <= self.fields.capacity) return;

            var new_capacity = self.fields.capacity;
            if (new_capacity == 0) {
                new_capacity = 16;
            }
            while (new_capacity < needed) {
                new_capacity = (new_capacity * 3) / 2;
            }
            self.growCapacity(new_capacity);
        }

        fn growCapacity(self: *Self, new_capacity: usize) void {
            const field_info = @typeInfo(T).Struct.fields;
            var new_ptrs: [field_info.len][*]u8 = undefined;

            // Allocate new arrays for each field
            inline for (field_info, 0..) |field_meta, i| {
                if (@sizeOf(field_meta.type) > 0) {
                    // Allocate with +1 for dummy element
                    const new_ptr = self.allocator.alloc(field_meta.type, new_capacity + 1) catch |err| exitOnOom(err);

                    // Copy existing data if any
                    if (self.fields.capacity > 0) {
                        const old_ptr = self.fields.ptrs[i];
                        const old_real_ptr: [*]field_meta.type = @ptrFromInt(@intFromPtr(old_ptr) - @sizeOf(field_meta.type));
                        @memcpy(new_ptr[0 .. self.fields.len + 1], old_real_ptr[0 .. self.fields.len + 1]);

                        // Free old allocation
                        const old_slice = old_real_ptr[0 .. self.fields.capacity + 1];
                        self.allocator.free(old_slice);
                    }

                    // Store offset pointer (pointing to element -1)
                    new_ptrs[i] = @ptrCast(&new_ptr[1]);
                } else {
                    new_ptrs[i] = undefined;
                }
            }

            self.fields.ptrs = new_ptrs;
            self.fields.capacity = new_capacity;
        }

        /// Convert a range to a slice
        pub fn rangeToSlice(self: *const Self, range: Range) std.MultiArrayList(T).Slice {
            const start: usize = @intFromEnum(range.start);
            const end: usize = @intFromEnum(range.end);

            std.debug.assert(start > 0 or (start == 0 and end == 0)); // Either valid range or empty
            std.debug.assert(start <= end);
            std.debug.assert(end <= self.fields.len + 1); // end is exclusive and 1-based

            if (start == 0) {
                // Return empty slice
                return .{
                    .ptrs = self.fields.ptrs,
                    .len = 0,
                    .capacity = 0,
                };
            }

            return .{
                .ptrs = self.fields.ptrs,
                .len = end - start,
                .capacity = self.fields.capacity - start + 1,
            };
        }

        /// A slice of all values for a specific field
        pub fn field(self: *const Self, comptime field_name: Field) []std.meta.fieldInfo(T, field_name).type {
            const field_index = @intFromEnum(field_name);
            const field_type = std.meta.fieldInfo(T, field_name).type;

            if (self.fields.len == 0) {
                return &[_]field_type{};
            }

            const field_ptr = self.fields.ptrs[field_index];
            const typed_ptr: [*]field_type = @ptrCast(@alignCast(field_ptr));
            // Return 1-based slice
            return typed_ptr[1 .. self.fields.len + 1];
        }

        /// The value for a specific field at a specific index
        pub fn fieldItem(self: *const Self, comptime field_name: Field, idx: Idx) std.meta.fieldInfo(T, field_name).type {
            const i = @intFromEnum(idx);
            std.debug.assert(i > 0 and i <= self.fields.len);

            const field_index = @intFromEnum(field_name);
            const field_type = std.meta.fieldInfo(T, field_name).type;
            const field_ptr = self.fields.ptrs[field_index];
            const typed_ptr: [*]field_type = @ptrCast(@alignCast(field_ptr));
            return typed_ptr[i];
        }

        /// An iterator over the indices of all elements in a list.
        pub const IndexIterator = struct {
            total: usize,
            current: usize,

            /// Get the next index from this iterator, or `null` if the iterator is finished.
            pub fn next(iter: *IndexIterator) ?Idx {
                if (iter.current >= iter.total) {
                    return null;
                }

                iter.current += 1;
                return @enumFromInt(@as(u32, @intCast(iter.current)));
            }
        };

        /// Iterator over all indices in this list.
        /// Returns 1-based indices.
        pub fn iterIndices(self: *const Self) IndexIterator {
            return IndexIterator{
                .total = self.fields.len,
                .current = 0,
            };
        }
    };
}

test "SentinelList basic operations" {
    const gpa = testing.allocator;

    var list = SentinelList(u32).initCapacity(gpa, 10);
    defer list.deinit();

    // Test that list starts empty
    try testing.expectEqual(@as(usize, 0), list.len());

    // Append returns 1-based index
    const idx1 = list.append(42);
    try testing.expectEqual(@as(u32, 1), @intFromEnum(idx1));
    try testing.expectEqual(@as(usize, 1), list.len());

    // Get with 1-based index
    const val1 = list.get(idx1);
    try testing.expectEqual(@as(u32, 42), val1.*);

    // Append more items
    const idx2 = list.append(100);
    const idx3 = list.append(200);
    try testing.expectEqual(@as(u32, 2), @intFromEnum(idx2));
    try testing.expectEqual(@as(u32, 3), @intFromEnum(idx3));

    // Set value
    list.set(idx2, 150);
    try testing.expectEqual(@as(u32, 150), list.get(idx2).*);
}

test "SentinelList appendSlice" {
    const gpa = testing.allocator;

    var list = SentinelList(u8).initCapacity(gpa, 0);
    defer list.deinit();

    // Empty slice
    const empty_range = list.appendSlice(&[_]u8{});
    try testing.expectEqual(@as(u32, 0), @intFromEnum(empty_range.start));
    try testing.expectEqual(@as(u32, 0), @intFromEnum(empty_range.end));

    // First slice - indices should be 1-based
    const range1 = list.appendSlice(&[_]u8{ 'a', 'b', 'c' });
    try testing.expectEqual(@as(u32, 1), @intFromEnum(range1.start));
    try testing.expectEqual(@as(u32, 4), @intFromEnum(range1.end)); // exclusive end

    // Second slice
    const range2 = list.appendSlice(&[_]u8{ 'd', 'e' });
    try testing.expectEqual(@as(u32, 4), @intFromEnum(range2.start));
    try testing.expectEqual(@as(u32, 6), @intFromEnum(range2.end));

    // Verify values
    try testing.expectEqual(@as(u8, 'a'), list.get(@enumFromInt(1)).*);
    try testing.expectEqual(@as(u8, 'c'), list.get(@enumFromInt(3)).*);
    try testing.expectEqual(@as(u8, 'e'), list.get(@enumFromInt(5)).*);
}

test "SentinelList rangeToSlice" {
    const gpa = testing.allocator;

    var list = SentinelList(u8).initCapacity(gpa, 0);
    defer list.deinit();

    _ = list.appendSlice(&[_]u8{ 'a', 'b', 'c', 'd', 'e' });

    // Get slice from 1-based range
    const range = SentinelList(u8).Range{
        .start = @enumFromInt(2), // 'b'
        .end = @enumFromInt(4), // up to but not including 'd'
    };
    const slice = list.rangeToSlice(range);

    try testing.expectEqual(@as(usize, 2), slice.len);
    try testing.expectEqual(@as(u8, 'b'), slice[0]);
    try testing.expectEqual(@as(u8, 'c'), slice[1]);
}

test "SentinelList iterator" {
    const gpa = testing.allocator;

    var list = SentinelList(u32).initCapacity(gpa, 0);
    defer list.deinit();

    _ = list.append(10);
    _ = list.append(20);
    _ = list.append(30);

    var iter = list.iterIndices();

    // Iterator should return 1-based indices
    const idx1 = iter.next().?;
    try testing.expectEqual(@as(u32, 1), @intFromEnum(idx1));

    const idx2 = iter.next().?;
    try testing.expectEqual(@as(u32, 2), @intFromEnum(idx2));

    const idx3 = iter.next().?;
    try testing.expectEqual(@as(u32, 3), @intFromEnum(idx3));

    try testing.expectEqual(@as(?SentinelList(u32).Idx, null), iter.next());
}

test "SentinelMultiList basic operations" {
    const gpa = testing.allocator;

    const Item = struct { value: u32, flag: bool };
    var list = SentinelMultiList(Item).initCapacity(gpa, 5);
    defer list.deinit();

    // Append items with 1-based indexing
    const idx1 = list.append(.{ .value = 100, .flag = true });
    const idx2 = list.append(.{ .value = 200, .flag = false });

    try testing.expectEqual(@as(u32, 1), @intFromEnum(idx1));
    try testing.expectEqual(@as(u32, 2), @intFromEnum(idx2));

    // Get items
    const item1 = list.get(idx1);
    try testing.expectEqual(@as(u32, 100), item1.value);
    try testing.expectEqual(true, item1.flag);

    // Field access
    try testing.expectEqual(@as(u32, 200), list.fieldItem(.value, idx2));
    try testing.expectEqual(false, list.fieldItem(.flag, idx2));

    // Set item
    list.set(idx1, .{ .value = 150, .flag = false });
    try testing.expectEqual(@as(u32, 150), list.fieldItem(.value, idx1));
}
