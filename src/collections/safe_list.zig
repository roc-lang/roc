//! Lists that make it easier to avoid incorrect indexing.

const std = @import("std");
const serialization = @import("../serialization/mod.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const SERIALIZATION_ALIGNMENT = serialization.SERIALIZATION_ALIGNMENT;

/// Represents a type safe span in a list; [start, end)
///
/// This is the conceptual equivalent of slice, but since this is based
/// on indexes in the list rather than pointers, it is reliable across
/// (de)serilaization and reallocation of the list.
///
/// This range is inclusive on the lower bound, exclusive on the upper bound.
pub fn SafeSpan(comptime Idx: type) type {
    return struct {
        const Self = @This();

        start: Idx,
        count: u32,

        /// An empty range
        pub fn empty() Self {
            return .{ .start = @enumFromInt(0), .count = 0 };
        }

        /// Return whether the range is empty
        pub fn isEmpty(self: @This()) bool {
            return self.count == 0;
        }

        // Drop first elem from the span, if possible
        pub fn dropFirstElem(self: *Self) void {
            if (self.count == 0) return;
            self.start = @enumFromInt(@intFromEnum(self.start) + 1);
            self.count -= 1;
        }

        /// Get the length of a range slice
        pub fn iterIndices(self: @This()) IndexIterator {
            return IndexIterator{
                .len = self.count,
                .current = @intFromEnum(self.start),
            };
        }

        /// An iterator over the indices of all elements in a list.
        pub const IndexIterator = struct {
            len: u32,
            current: u32,

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
    };
}

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
        end: Idx,

        /// An empty range
        pub const empty: Self = .{ .start = @enumFromInt(0), .end = @enumFromInt(0) };

        /// Get the length of a range slice
        pub fn len(self: @This()) u32 {
            return @intFromEnum(self.end) - @intFromEnum(self.start);
        }

        /// Return whether the range is empty
        pub fn isEmpty(self: @This()) bool {
            return self.start == self.end;
        }

        /// Get the length of a range slice
        pub fn iterIndices(self: @This()) IndexIterator {
            return IndexIterator{
                .len = @intFromEnum(self.end),
                .current = @intFromEnum(self.start),
            };
        }

        /// An iterator over the indices of all elements in a list.
        pub const IndexIterator = struct {
            len: u32,
            current: u32,

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
            range: Range,
        };

        /// A type-safe range of the list.
        pub const Span = SafeSpan(Idx);

        /// A type-safe span which must have at least one element.
        pub const NonEmptySpan = struct {
            nonempty: Span,
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
        pub fn len(self: *const SafeList(T)) usize {
            return self.items.items.len;
        }

        /// Add an item to the end of this list.
        pub fn append(self: *SafeList(T), gpa: Allocator, item: T) std.mem.Allocator.Error!Idx {
            const length = self.len();
            try self.items.append(gpa, item);

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        /// Add all the items in a slice to the end of this list.
        pub fn appendSlice(self: *SafeList(T), gpa: Allocator, items: []const T) std.mem.Allocator.Error!Range {
            const start_length = self.len();
            try self.items.appendSlice(gpa, items);
            const end_length = self.len();
            return Range{ .start = @enumFromInt(start_length), .end = @enumFromInt(end_length) };
        }

        /// Add all the items in a slice to the end of this list.
        pub fn appendSliceSpan(self: *SafeList(T), gpa: Allocator, items: []const T) std.mem.Allocator.Error!Span {
            const start_length = self.len();
            try self.items.appendSlice(gpa, items);
            const end_length = self.len();
            return Span{ .start = @enumFromInt(start_length), .count = @intCast(end_length - start_length) };
        }

        /// Extend this list with all items generated by an iterator.
        pub fn extendFromIter(self: *SafeList(T), gpa: Allocator, iter_extend: anytype) std.mem.Allocator.Error!Range {
            const start_length = self.len();
            while (iter_extend.next()) |item| {
                try self.items.append(gpa, item);
            }
            const end_length = self.len();
            return Range{ .start = @enumFromInt(start_length), .end = @enumFromInt(end_length) };
        }

        /// Convert a range to a slice
        pub fn rangeToSlice(self: *const SafeList(T), range: Range) Slice {
            const start: usize = @intFromEnum(range.start);
            const end: usize = @intFromEnum(range.end);

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.items.items.len);

            return self.items.items[start..end];
        }

        /// Convert a span to a slice
        pub fn sliceSpan(self: *const SafeList(T), span: Span) Slice {
            const start: usize = @intFromEnum(span.start);
            const end: usize = start + span.count;

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

        /// Returns the size needed to serialize this list
        pub fn serializedSize(self: *const SafeList(T)) usize {
            // Header: 4 bytes for count
            var total_size: usize = @sizeOf(u32);

            // Check if T has custom serialization
            if (comptime switch (@typeInfo(T)) {
                .@"struct", .@"union", .@"enum", .@"opaque" => @hasDecl(T, "serializedSize"),
                else => false,
            }) {
                // Use custom serialization for each item
                for (self.items.items) |*item| {
                    total_size += item.serializedSize();
                }
            } else {
                // Use fixed size for POD types
                total_size += self.items.items.len * @sizeOf(T);
            }

            // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
            return std.mem.alignForward(usize, total_size, SERIALIZATION_ALIGNMENT);
        }

        /// Serialize this list into the provided buffer
        /// Returns the slice of buffer that was written to
        pub fn serializeInto(self: *const SafeList(T), buffer: []align(SERIALIZATION_ALIGNMENT) u8) ![]align(SERIALIZATION_ALIGNMENT) const u8 {
            const size = self.serializedSize();
            if (buffer.len < size) return error.BufferTooSmall;

            // Write count
            const count_ptr = @as(*u32, @ptrCast(@alignCast(buffer.ptr)));
            count_ptr.* = @intCast(self.items.items.len);

            var offset: usize = @sizeOf(u32);

            // Check if T has custom serialization
            if (comptime switch (@typeInfo(T)) {
                .@"struct", .@"union", .@"enum", .@"opaque" => @hasDecl(T, "serializeInto"),
                else => false,
            }) {
                // Use custom serialization for each item
                for (self.items.items) |*item| {
                    const item_buffer = buffer[offset..];
                    const item_slice = try item.serializeInto(item_buffer);
                    offset += item_slice.len;
                }
            } else {
                // Use memcpy for POD types
                if (@typeInfo(T) == .@"struct" or @typeInfo(T) == .int or @typeInfo(T) == .float or @typeInfo(T) == .@"enum") {
                    const data_ptr = @as([*]T, @ptrCast(@alignCast(buffer.ptr + @sizeOf(u32))));
                    @memcpy(data_ptr[0..self.items.items.len], self.items.items);
                    offset += self.items.items.len * @sizeOf(T);
                } else {
                    @compileError("Cannot serialize non-POD type " ++ @typeName(T) ++ " without custom serialization methods");
                }
            }

            // Zero out any padding bytes
            if (offset < size) {
                @memset(buffer[offset..size], 0);
            }

            return buffer[0..size];
        }

        /// Deserialize from buffer, using provided allocator
        pub fn deserializeFrom(buffer: []align(@alignOf(T)) const u8, allocator: Allocator) !SafeList(T) {
            if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;

            // Read count
            const count = @as(*const u32, @ptrCast(@alignCast(buffer.ptr))).*;

            // Create list with exact capacity
            var list = try SafeList(T).initCapacity(allocator, count);
            errdefer list.deinit(allocator);

            var offset: usize = @sizeOf(u32);

            // Check if T has custom deserialization
            if (comptime switch (@typeInfo(T)) {
                .@"struct", .@"union", .@"enum", .@"opaque" => @hasDecl(T, "deserializeFrom"),
                else => false,
            }) {
                // Use custom deserialization for each item
                for (0..count) |_| {
                    const item_buffer = buffer[offset..];
                    const item = try T.deserializeFrom(item_buffer);
                    const item_idx = try list.items.append(allocator, item);
                    _ = item_idx;

                    // For custom deserialization, we need to advance offset by the actual serialized size
                    offset += item.serializedSize();
                }
            } else {
                // Use memcpy for POD types
                const expected_size = @sizeOf(u32) + (count * @sizeOf(T));
                if (buffer.len < expected_size) return error.BufferTooSmall;

                if (count > 0) {
                    const data_ptr = @as([*]const T, @ptrCast(@alignCast(buffer.ptr + @sizeOf(u32))));
                    list.items.appendSliceAssumeCapacity(data_ptr[0..count]);
                }
            }

            return list;
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
        pub fn iterSpan(self: *const SafeList(T), span: Span) Iterator {
            return Iterator{
                .array = self,
                .len = @intFromEnum(span.start) + span.count,
                .current = span.start,
            };
        }

        /// Iterate over all items in this list.
        pub fn iter(self: *const SafeList(T)) Iterator {
            return Iterator{
                .array = self,
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
        pub fn len(self: *const SafeMultiList(T)) usize {
            return self.items.len;
        }

        /// Add a new item to the end of this list.
        pub fn append(self: *SafeMultiList(T), gpa: Allocator, item: T) std.mem.Allocator.Error!Idx {
            const length = self.len();
            try self.items.append(gpa, item);

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        pub fn appendSlice(self: *SafeMultiList(T), gpa: Allocator, elems: []const T) std.mem.Allocator.Error!Range {
            if (elems.len == 0) {
                return .{ .start = .zero, .end = .zero };
            }
            const start_length = self.len();
            try self.items.ensureUnusedCapacity(gpa, elems.len);
            for (elems) |elem| {
                self.items.appendAssumeCapacity(elem);
            }
            const end_length = self.len();
            return Range{ .start = @enumFromInt(start_length), .end = @enumFromInt(end_length) };
        }

        /// Convert a range to a slice
        pub fn rangeToSlice(self: *const SafeMultiList(T), range: Range) Slice {
            const start: usize = @intFromEnum(range.start);
            const end: usize = @intFromEnum(range.end);

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

        pub fn serializedSize(self: *const SafeMultiList(T)) usize {
            // Header: 4 bytes for count
            // Data: items.len * @sizeOf(T)
            const raw_size = @sizeOf(u32) + (self.items.len * @sizeOf(T));
            // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
            return std.mem.alignForward(usize, raw_size, SERIALIZATION_ALIGNMENT);
        }

        /// Serialize this list into the provided buffer
        /// Returns the slice of buffer that was written to
        pub fn serializeInto(self: *const SafeMultiList(T), buffer: []align(SERIALIZATION_ALIGNMENT) u8) ![]align(SERIALIZATION_ALIGNMENT) const u8 {
            const size = self.serializedSize();
            if (buffer.len < size) return error.BufferTooSmall;

            // Write count
            const count_ptr = @as(*u32, @ptrCast(@alignCast(buffer.ptr)));
            count_ptr.* = @intCast(self.items.len);

            // If T is a POD type, serialize each item
            if (@typeInfo(T) == .@"struct" or @typeInfo(T) == .int or @typeInfo(T) == .float) {
                const data_ptr = @as([*]T, @ptrCast(@alignCast(buffer.ptr + @sizeOf(u32))));
                // Copy each item from the MultiArrayList to contiguous memory
                for (0..self.items.len) |i| {
                    data_ptr[i] = self.items.get(i);
                }
            } else {
                @compileError("Cannot serialize non-POD type " ++ @typeName(T));
            }

            // Zero out any padding bytes
            const actual_size = @sizeOf(u32) + (self.items.len * @sizeOf(T));
            if (actual_size < size) {
                @memset(buffer[actual_size..size], 0);
            }

            return buffer[0..size];
        }

        /// Deserialize from buffer, using provided allocator
        pub fn deserializeFrom(buffer: []align(@alignOf(T)) const u8, allocator: Allocator) !SafeMultiList(T) {
            if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;

            // Read count
            const count = @as(*const u32, @ptrCast(@alignCast(buffer.ptr))).*;

            const expected_size = @sizeOf(u32) + (count * @sizeOf(T));
            if (buffer.len < expected_size) return error.BufferTooSmall;

            // Create list with exact capacity
            var list = try SafeMultiList(T).initCapacity(allocator, count);

            // Copy data
            if (count > 0) {
                const data_ptr = @as([*]const T, @ptrCast(@alignCast(buffer.ptr + @sizeOf(u32))));
                // Add each item to the MultiArrayList
                for (0..count) |i| {
                    _ = try list.append(allocator, data_ptr[i]);
                }
            }

            return list;
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
    try testing.expectEqual(4, @intFromEnum(rangeA.end));

    const rangeB = try list.appendSlice(gpa, &[_]u8{ 'd', 'e', 'f', 'g' });
    try testing.expectEqual(4, @intFromEnum(rangeB.start));
    try testing.expectEqual(8, @intFromEnum(rangeB.end));
}

test "SafeList(u8) rangeToSlice" {
    const gpa = testing.allocator;

    var list = SafeList(u8){};
    defer list.deinit(gpa);

    const rangeA = try list.appendSlice(gpa, &[_]u8{ 'a', 'b', 'c', 'd' });
    const sliceA = list.rangeToSlice(rangeA);
    try testing.expectEqual('a', sliceA[0]);
    try testing.expectEqual('d', sliceA[3]);

    const rangeB = SafeList(u8).Range{ .start = @enumFromInt(2), .end = @enumFromInt(4) };
    const sliceB = list.rangeToSlice(rangeB);
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
    try testing.expectEqual(3, @intFromEnum(rangeA.end));

    const rangeB = try multilist.appendSlice(gpa, &[_]Struct{ .{ .num = 400, .char = 'd' }, .{ .num = 500, .char = 'e' }, .{ .num = 600, .char = 'f' } });
    try testing.expectEqual(3, @intFromEnum(rangeB.start));
    try testing.expectEqual(6, @intFromEnum(rangeB.end));
}

test "SafeMultiList(u8) rangeToSlice" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 3);
    defer multilist.deinit(gpa);

    const range_a = try multilist.appendSlice(gpa, &[_]Struct{ .{ .num = 100, .char = 'a' }, .{ .num = 200, .char = 'b' }, .{ .num = 300, .char = 'c' } });
    const slice_a = multilist.rangeToSlice(range_a);

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

    const range_b = StructMultiList.Range{ .start = @enumFromInt(1), .end = @enumFromInt(2) };
    const slice_b = multilist.rangeToSlice(range_b);

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
    const empty_range = StructMultiList.Range{ .start = @enumFromInt(5), .end = @enumFromInt(5) };
    const empty_slice = multilist.rangeToSlice(empty_range);

    // The slice should be empty
    const num_slice = empty_slice.items(.num);
    try testing.expectEqual(0, num_slice.len);

    const char_slice = empty_slice.items(.char);
    try testing.expectEqual(0, char_slice.len);
}

test "SafeList(u32) serialization empty list" {
    const gpa = testing.allocator;

    var list = SafeList(u32){};
    defer list.deinit(gpa);

    // Empty list should serialize to just a count of 0, aligned to SERIALIZATION_ALIGNMENT
    const expected_size = std.mem.alignForward(usize, @sizeOf(u32), SERIALIZATION_ALIGNMENT);
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [16]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Check that count is 0
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, 0), count);
}

test "SafeList(u32) serialization with data" {
    const gpa = testing.allocator;

    var list = SafeList(u32){};
    defer list.deinit(gpa);

    _ = try list.append(gpa, 42);
    _ = try list.append(gpa, 100);
    _ = try list.append(gpa, 255);

    const expected_size = @sizeOf(u32) + (3 * @sizeOf(u32));
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [256]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Check that count is 3
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, 3), count);

    // Check the data
    const data_start = @sizeOf(u32);
    const data_ptr = @as([*]const u32, @ptrCast(@alignCast(buffer[data_start..])));
    try testing.expectEqual(@as(u32, 42), data_ptr[0]);
    try testing.expectEqual(@as(u32, 100), data_ptr[1]);
    try testing.expectEqual(@as(u32, 255), data_ptr[2]);
}

test "SafeList(u8) serialization with data" {
    const gpa = testing.allocator;

    var list = SafeList(u8){};
    defer list.deinit(gpa);

    _ = try list.appendSlice(gpa, "hello");

    const expected_size = std.mem.alignForward(usize, @sizeOf(u32) + 5, SERIALIZATION_ALIGNMENT);
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [256]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Check that count is 5
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, 5), count);

    // Check the data
    try testing.expectEqualSlices(u8, "hello", buffer[@sizeOf(u32) .. @sizeOf(u32) + 5]);
}

test "SafeList(u32) deserialization empty list" {
    const gpa = testing.allocator;

    // Create buffer with count = 0
    var buffer: [@sizeOf(u32)]u8 align(@alignOf(u32)) = undefined;
    @as(*u32, @ptrCast(@alignCast(&buffer))).* = 0;

    var list = try SafeList(u32).deserializeFrom(&buffer, gpa);
    defer list.deinit(gpa);

    try testing.expectEqual(@as(usize, 0), list.len());
}

test "SafeList(u32) deserialization with data" {
    const gpa = testing.allocator;

    // Prepare buffer with count = 3 and data [42, 100, 255]
    const expected_data = [_]u32{ 42, 100, 255 };
    const buffer_size = @sizeOf(u32) + expected_data.len * @sizeOf(u32);
    var partial_buffer: [64]u8 align(SERIALIZATION_ALIGNMENT) = undefined;

    @as(*u32, @ptrCast(@alignCast(&partial_buffer))).* = expected_data.len;
    const data_ptr = @as([*]u32, @ptrCast(@alignCast(partial_buffer[@sizeOf(u32)..])));
    @memcpy(data_ptr[0..expected_data.len], &expected_data);

    var list = try SafeList(u32).deserializeFrom(partial_buffer[0..buffer_size], gpa);
    defer list.deinit(gpa);

    try testing.expectEqual(expected_data.len, list.len());
    for (expected_data, 0..) |expected, i| {
        const idx: SafeList(u32).Idx = @enumFromInt(i);
        try testing.expectEqual(expected, list.get(idx).*);
    }
}

test "SafeList(u8) deserialization with data" {
    const gpa = testing.allocator;

    // Prepare buffer with "world"
    const expected_data = "world";
    const buffer_size = @sizeOf(u32) + expected_data.len;
    var small_buffer: [64]u8 align(SERIALIZATION_ALIGNMENT) = undefined;

    @as(*u32, @ptrCast(@alignCast(&small_buffer))).* = @intCast(expected_data.len);
    @memcpy(small_buffer[@sizeOf(u32)..buffer_size], expected_data);

    var list = try SafeList(u8).deserializeFrom(small_buffer[0..buffer_size], gpa);
    defer list.deinit(gpa);

    try testing.expectEqual(expected_data.len, list.len());
    const slice = list.rangeToSlice(SafeList(u8).Range{ .start = @enumFromInt(0), .end = @enumFromInt(expected_data.len) });
    try testing.expectEqualSlices(u8, expected_data, slice);
}

test "SafeList(u32) round-trip serialization" {
    const gpa = testing.allocator;

    // Create original list
    var original = SafeList(u32){};
    defer original.deinit(gpa);

    const test_data = [_]u32{ 1, 2, 3, 42, 100, 255 };
    _ = try original.appendSlice(gpa, &test_data);

    // Serialize
    var buffer: [1024]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try original.serializeInto(&buffer);

    // Deserialize
    var deserialized = try SafeList(u32).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    // Compare
    try testing.expectEqual(original.len(), deserialized.len());
    for (test_data, 0..) |expected, i| {
        const idx: SafeList(u32).Idx = @enumFromInt(i);
        try testing.expectEqual(expected, deserialized.get(idx).*);
    }
}

test "SafeList serialization buffer too small error" {
    const gpa = testing.allocator;

    var list = SafeList(u32){};
    defer list.deinit(gpa);

    _ = try list.append(gpa, 42);
    _ = try list.append(gpa, 100);

    // Buffer too small for the data
    var small_buffer: [4]u8 align(SERIALIZATION_ALIGNMENT) = undefined; // Only room for count, not data
    try testing.expectError(error.BufferTooSmall, list.serializeInto(&small_buffer));
}

test "SafeList deserialization buffer too small error" {
    const gpa = testing.allocator;

    // Buffer too small to even contain count
    var small_buffer: [2]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    try testing.expectError(error.BufferTooSmall, SafeList(u32).deserializeFrom(&small_buffer, gpa));

    // Buffer with count but insufficient data
    var partial_buffer2: [6]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    @as(*u32, @ptrCast(@alignCast(&partial_buffer2))).* = 2; // Claims 2 items but only has 2 extra bytes
    try testing.expectError(error.BufferTooSmall, SafeList(u32).deserializeFrom(&partial_buffer2, gpa));
}

test "SafeList(struct) serialization" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };
    var list = SafeList(Point){};
    defer list.deinit(gpa);

    _ = try list.append(gpa, Point{ .x = 10, .y = 20 });
    _ = try list.append(gpa, Point{ .x = 30, .y = 40 });

    var buffer: [1024]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);

    var deserialized = try SafeList(Point).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    try testing.expectEqual(@as(usize, 2), deserialized.len());

    const p0 = deserialized.get(@enumFromInt(0));
    try testing.expectEqual(@as(i32, 10), p0.x);
    try testing.expectEqual(@as(i32, 20), p0.y);

    const p1 = deserialized.get(@enumFromInt(1));
    try testing.expectEqual(@as(i32, 30), p1.x);
    try testing.expectEqual(@as(i32, 40), p1.y);
}

test "SafeMultiList(struct) serialization empty list" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };
    var list = SafeMultiList(Point){};
    defer list.deinit(gpa);

    // Empty list should serialize to just a count of 0, aligned to SERIALIZATION_ALIGNMENT
    const expected_size = std.mem.alignForward(usize, @sizeOf(u32), SERIALIZATION_ALIGNMENT);
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [16]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Check that count is 0
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, 0), count);
}

test "SafeMultiList(struct) serialization with data" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };
    var list = SafeMultiList(Point){};
    defer list.deinit(gpa);

    _ = try list.append(gpa, Point{ .x = 10, .y = 20 });
    _ = try list.append(gpa, Point{ .x = 30, .y = 40 });
    _ = try list.append(gpa, Point{ .x = 50, .y = 60 });

    const expected_size = std.mem.alignForward(usize, @sizeOf(u32) + (3 * @sizeOf(Point)), SERIALIZATION_ALIGNMENT);
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [2048]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Check that count is 3
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, 3), count);

    // Check the data
    const data_start = @sizeOf(u32);
    const data_ptr = @as([*]const Point, @ptrCast(@alignCast(buffer[data_start..])));
    try testing.expectEqual(@as(i32, 10), data_ptr[0].x);
    try testing.expectEqual(@as(i32, 20), data_ptr[0].y);
    try testing.expectEqual(@as(i32, 30), data_ptr[1].x);
    try testing.expectEqual(@as(i32, 40), data_ptr[1].y);
    try testing.expectEqual(@as(i32, 50), data_ptr[2].x);
    try testing.expectEqual(@as(i32, 60), data_ptr[2].y);
}

test "SafeMultiList(struct) serialization with primitive data" {
    const gpa = testing.allocator;

    const Value = struct { val: u32 };
    var list = SafeMultiList(Value){};
    defer list.deinit(gpa);

    _ = try list.append(gpa, Value{ .val = 42 });
    _ = try list.append(gpa, Value{ .val = 100 });

    const expected_size = std.mem.alignForward(usize, @sizeOf(u32) + (2 * @sizeOf(u32)), SERIALIZATION_ALIGNMENT);
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [2048]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Check that count is 2
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, 2), count);

    // Check the data
    const data_start = @sizeOf(u32);
    const data_ptr = @as([*]const Value, @ptrCast(@alignCast(buffer[data_start..])));
    try testing.expectEqual(@as(u32, 42), data_ptr[0].val);
    try testing.expectEqual(@as(u32, 100), data_ptr[1].val);
}

test "SafeMultiList(struct) deserialization empty list" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };

    // Create buffer with count = 0
    var buffer: [@sizeOf(u32)]u8 align(@alignOf(Point)) = undefined;
    @as(*u32, @ptrCast(@alignCast(&buffer))).* = 0;

    var list = try SafeMultiList(Point).deserializeFrom(&buffer, gpa);
    defer list.deinit(gpa);

    try testing.expectEqual(@as(usize, 0), list.len());
}

test "SafeMultiList(struct) deserialization with data" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };
    const expected_data = [_]Point{
        Point{ .x = 10, .y = 20 },
        Point{ .x = 30, .y = 40 },
    };

    // Prepare buffer with count = 2 and data
    const buffer_size = @sizeOf(u32) + expected_data.len * @sizeOf(Point);
    var buffer: [2048]u8 align(SERIALIZATION_ALIGNMENT) = undefined;

    @as(*u32, @ptrCast(@alignCast(&buffer))).* = expected_data.len;
    const data_ptr = @as([*]Point, @ptrCast(@alignCast(buffer[@sizeOf(u32)..])));
    @memcpy(data_ptr[0..expected_data.len], &expected_data);

    var list = try SafeMultiList(Point).deserializeFrom(buffer[0..buffer_size], gpa);
    defer list.deinit(gpa);

    try testing.expectEqual(expected_data.len, list.len());
    for (expected_data, 0..) |expected, i| {
        const idx: SafeMultiList(Point).Idx = @enumFromInt(i); // SafeMultiList uses 0-based indexing
        const actual = list.get(idx);
        try testing.expectEqual(expected.x, actual.x);
        try testing.expectEqual(expected.y, actual.y);
    }
}

test "SafeMultiList(struct) deserialization with primitive data" {
    const gpa = testing.allocator;

    const Value = struct { val: u32 };
    const expected_data = [_]Value{ Value{ .val = 42 }, Value{ .val = 100 }, Value{ .val = 255 } };
    const buffer_size = @sizeOf(u32) + expected_data.len * @sizeOf(Value);
    var buffer: [2048]u8 align(SERIALIZATION_ALIGNMENT) = undefined;

    @as(*u32, @ptrCast(@alignCast(&buffer))).* = expected_data.len;
    const data_ptr = @as([*]Value, @ptrCast(@alignCast(buffer[@sizeOf(u32)..])));
    @memcpy(data_ptr[0..expected_data.len], &expected_data);

    var list = try SafeMultiList(Value).deserializeFrom(buffer[0..buffer_size], gpa);
    defer list.deinit(gpa);

    try testing.expectEqual(expected_data.len, list.len());
    for (expected_data, 0..) |expected, i| {
        const idx: SafeMultiList(Value).Idx = @enumFromInt(i); // SafeMultiList uses 0-based indexing
        try testing.expectEqual(expected.val, list.get(idx).val);
    }
}

test "SafeMultiList(struct) round-trip serialization" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };
    var original = SafeMultiList(Point){};
    defer original.deinit(gpa);

    const test_data = [_]Point{
        Point{ .x = 1, .y = 2 },
        Point{ .x = 42, .y = 100 },
        Point{ .x = 255, .y = 128 },
    };
    _ = try original.appendSlice(gpa, &test_data);

    // Serialize
    var buffer: [2048]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try original.serializeInto(&buffer);

    // Deserialize
    var deserialized = try SafeMultiList(Point).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    // Compare
    try testing.expectEqual(original.len(), deserialized.len());
    for (test_data, 0..) |expected, i| {
        const idx: SafeMultiList(Point).Idx = @enumFromInt(i); // SafeMultiList uses 0-based indexing
        const actual = deserialized.get(idx);
        try testing.expectEqual(expected.x, actual.x);
        try testing.expectEqual(expected.y, actual.y);
    }
}

test "SafeMultiList serialization buffer too small error" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };
    var list = SafeMultiList(Point){};
    defer list.deinit(gpa);

    _ = try list.append(gpa, Point{ .x = 10, .y = 20 });
    _ = try list.append(gpa, Point{ .x = 30, .y = 40 });

    // Buffer too small for the data
    var small_buffer: [4]u8 align(SERIALIZATION_ALIGNMENT) = undefined; // Only room for count, not data
    try testing.expectError(error.BufferTooSmall, list.serializeInto(&small_buffer));
}

test "SafeMultiList deserialization buffer too small error" {
    const gpa = testing.allocator;

    const Point = struct { x: i32, y: i32 };

    // Buffer too small to even contain count
    var tiny_buffer: [2]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    try testing.expectError(error.BufferTooSmall, SafeMultiList(Point).deserializeFrom(&tiny_buffer, gpa));

    // Buffer with count but insufficient data
    var partial_buffer: [6]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    @as(*u32, @ptrCast(@alignCast(&partial_buffer))).* = 1; // Claims 1 item but insufficient space for Point
    try testing.expectError(error.BufferTooSmall, SafeMultiList(Point).deserializeFrom(&partial_buffer, gpa));
}

test "SafeMultiList complex Node-like structure serialization" {
    const gpa = testing.allocator;

    // Complex structure similar to Node.zig with enums, multiple fields, and nested data
    const NodeTag = enum(u8) {
        statement_decl,
        statement_var,
        statement_expr,
        expr_var,
        expr_tuple,
        expr_list,
        expr_call,
        expr_int,
        expr_float,
        expr_string,
        pattern_identifier,
        pattern_list,
        ty_apply,
        ty_var,
        malformed,
        diag_not_implemented,
    };

    const Region = struct {
        start: u32,
        end: u32,
    };

    const ComplexNode = struct {
        data_1: u32,
        data_2: u32,
        data_3: u32,
        region: Region,
        tag: NodeTag,
        flags: u16,
        extra: u8,
    };

    var list = SafeMultiList(ComplexNode){};
    defer list.deinit(gpa);

    // Add various node types with different data
    const test_nodes = [_]ComplexNode{
        ComplexNode{
            .data_1 = 42,
            .data_2 = 100,
            .data_3 = 255,
            .region = Region{ .start = 0, .end = 10 },
            .tag = NodeTag.statement_decl,
            .flags = 0x1234,
            .extra = 0xAB,
        },
        ComplexNode{
            .data_1 = 0,
            .data_2 = 0xFFFFFFFF,
            .data_3 = 128,
            .region = Region{ .start = 15, .end = 45 },
            .tag = NodeTag.expr_call,
            .flags = 0x5678,
            .extra = 0xCD,
        },
        ComplexNode{
            .data_1 = 999,
            .data_2 = 1000,
            .data_3 = 1001,
            .region = Region{ .start = 50, .end = 75 },
            .tag = NodeTag.pattern_list,
            .flags = 0x9ABC,
            .extra = 0xEF,
        },
        ComplexNode{
            .data_1 = 0x12345678,
            .data_2 = 0x87654321,
            .data_3 = 0xDEADBEEF,
            .region = Region{ .start = 100, .end = 200 },
            .tag = NodeTag.malformed,
            .flags = 0xDEF0,
            .extra = 0x00,
        },
    };

    _ = try list.appendSlice(gpa, &test_nodes);

    // Test serialization
    const expected_size = std.mem.alignForward(usize, @sizeOf(u32) + (test_nodes.len * @sizeOf(ComplexNode)), SERIALIZATION_ALIGNMENT);
    try testing.expectEqual(expected_size, list.serializedSize());

    var buffer: [1024]u8 align(SERIALIZATION_ALIGNMENT) = undefined;
    const serialized = try list.serializeInto(&buffer);
    try testing.expectEqual(expected_size, serialized.len);

    // Verify count in serialized data
    const count = @as(*const u32, @ptrCast(@alignCast(buffer[0..@sizeOf(u32)]))).*;
    try testing.expectEqual(@as(u32, test_nodes.len), count);

    // Test deserialization
    var deserialized = try SafeMultiList(ComplexNode).deserializeFrom(serialized, gpa);
    defer deserialized.deinit(gpa);

    try testing.expectEqual(test_nodes.len, deserialized.len());

    // Verify all fields are correctly deserialized
    for (test_nodes, 0..) |expected, i| {
        const idx: SafeMultiList(ComplexNode).Idx = @enumFromInt(i);
        const actual = deserialized.get(idx);

        try testing.expectEqual(expected.data_1, actual.data_1);
        try testing.expectEqual(expected.data_2, actual.data_2);
        try testing.expectEqual(expected.data_3, actual.data_3);
        try testing.expectEqual(expected.region.start, actual.region.start);
        try testing.expectEqual(expected.region.end, actual.region.end);
        try testing.expectEqual(expected.tag, actual.tag);
        try testing.expectEqual(expected.flags, actual.flags);
        try testing.expectEqual(expected.extra, actual.extra);
    }
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
