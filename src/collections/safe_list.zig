//! Lists that make it easier to avoid incorrect indexing.

const std = @import("std");
const serialization = @import("serialization");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const SERIALIZATION_ALIGNMENT = serialization.SERIALIZATION_ALIGNMENT;
const CompactWriter = serialization.CompactWriter;

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

        /// Returns a SafeList that has had its pointer converted to an offset.
        /// It's only safe to serialize this return value; attempting to call
        /// methods on it or dereference its internal "pointers" (which are now
        /// offsets) is illegal behavior!
        pub fn serialize(
            self: *const SafeList(T),
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!*const SafeList(T) {
            const items = self.items.items;

            // First, write the backing elements. That will get us the byte offset in
            // the file of the first backing element.
            const offset = try writer.appendSlice(allocator, items.ptr, items.len);

            // Next, write the struct (using the offset as the ArrayList's pointer)
            const offset_self = try writer.appendAlloc(allocator, SafeList(T));

            offset_self.* = .{
                .items = .{
                    .items = @as([*]T, @ptrFromInt(offset))[0..items.len],
                    .capacity = items.len,
                },
            };

            // Finally, return the version of Self that's in the writer's buffer,
            // which has offsets instead of pointers.
            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *SafeList(T), offset: isize) void {
            if (self.items.items.len > 0) {
                const old_addr: isize = @intCast(@intFromPtr(self.items.items.ptr));
                self.items.items.ptr = @ptrFromInt(@as(usize, @intCast(old_addr + offset)));
            }
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

        /// Returns a SafeMultiList that has had its pointers converted to offsets,
        /// after appending pointers to the writer such that the result is only
        /// the actual filled elements of the MultiList being written (plus possibly
        /// some zeros for alignment padding), and none of its excess capacity
        /// being written.
        ///
        /// It's only safe to serialize this return value; attempting to call
        /// methods on it or dereference its internal "pointers" (which are now
        /// offsets) is illegal behavior!
        pub fn serialize(
            self: *const SafeMultiList(T),
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!*const SafeMultiList(T) {
            // Write only len elements, not capacity, to avoid storing garbage memory.
            const data_offset = if (self.items.len > 0) blk: {
                const slice = self.items.slice();
                const fields = std.meta.fields(T);

                // MultiArrayList lays out fields in order, with alignment padding as
                // necessary between the end of one field's elements and the beginning of
                // the next. So we need to append entries to the writer for all fields.
                const first_field_offset = writer.total_bytes;

                inline for (fields, 0..) |_, i| {
                    const field_ptr = slice.items(@as(Field, @enumFromInt(i))).ptr;

                    // Write the field data (only len elements' worth).
                    // appendSlice will take care of alignment padding.
                    _ = try writer.appendSlice(allocator, field_ptr, self.items.len);
                }

                break :blk first_field_offset;
            } else writer.total_bytes;

            // Write the SafeMultiList struct
            const offset_self = try writer.appendAlloc(allocator, SafeMultiList(T));

            // Initialize with offsets
            offset_self.* = .{
                .items = .{
                    .bytes = @ptrFromInt(data_offset),
                    .len = self.items.len,
                    .capacity = self.items.len, // capacity = len for compacted data
                },
            };

            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *SafeMultiList(T), offset: isize) void {
            if (self.items.capacity == 0) return;

            const old_addr: isize = @intCast(@intFromPtr(self.items.bytes));
            self.items.bytes = @ptrFromInt(@as(usize, @intCast(old_addr + offset)));
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
    const slice = list.sliceRange(SafeList(u8).Range{ .start = @enumFromInt(0), .count = expected_data.len });
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
        try testing.expectEqual(expected.flags, actual.flags);
    }
}

test "SafeList(u32) CompactWriter roundtrip with file" {
    const gpa = testing.allocator;

    // Create a SafeList with some test data
    var original = try SafeList(u32).initCapacity(gpa, 5);
    defer original.deinit(gpa);

    const test_data = [_]u32{ 42, 1337, 9999, 0, 12345 };
    for (test_data) |value| {
        _ = try original.append(gpa, value);
    }

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read the file back into an aligned buffer
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // The key insight: we can just cast the buffer to a SafeList pointer
    // The layout in memory is exactly what we need
    const deserialized = @as(*SafeList(u32), @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SafeList(u32)))));

    // Relocate the pointers - this adjusts the internal pointer from offset to actual memory address
    const base_addr = @intFromPtr(buffer.ptr);
    deserialized.relocate(@as(isize, @intCast(base_addr)));

    // Verify the data matches
    try testing.expectEqual(original.len(), deserialized.len());
    for (test_data, 0..) |expected, i| {
        const idx: SafeList(u32).Idx = @enumFromInt(i);
        try testing.expectEqual(expected, deserialized.get(idx).*);
    }
}

test "SafeList(struct) CompactWriter roundtrip with file" {
    const gpa = testing.allocator;

    const Point = struct {
        x: i32,
        y: i32,
    };

    // Create a SafeList with struct data
    var original = try SafeList(Point).initCapacity(gpa, 3);
    defer original.deinit(gpa);

    _ = try original.append(gpa, .{ .x = 10, .y = 20 });
    _ = try original.append(gpa, .{ .x = 30, .y = 40 });
    _ = try original.append(gpa, .{ .x = 50, .y = 60 });

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_struct.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back with proper alignment
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SafeList(Point), @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SafeList(Point)))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the data
    try testing.expectEqual(@as(usize, 3), deserialized.len());
    try testing.expectEqual(@as(i32, 10), deserialized.get(@enumFromInt(0)).x);
    try testing.expectEqual(@as(i32, 20), deserialized.get(@enumFromInt(0)).y);
    try testing.expectEqual(@as(i32, 30), deserialized.get(@enumFromInt(1)).x);
    try testing.expectEqual(@as(i32, 40), deserialized.get(@enumFromInt(1)).y);
    try testing.expectEqual(@as(i32, 50), deserialized.get(@enumFromInt(2)).x);
    try testing.expectEqual(@as(i32, 60), deserialized.get(@enumFromInt(2)).y);
}

test "SafeList empty list CompactWriter roundtrip" {
    const gpa = testing.allocator;

    // Create an empty SafeList
    var original = SafeList(u64){};
    defer original.deinit(gpa);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate - empty list should still work
    const deserialized = @as(*SafeList(u64), @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SafeList(u64)))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.len());
}

test "SafeList empty lists CompactWriter roundtrip multiple types" {
    const gpa = testing.allocator;

    // Test empty lists with different types to ensure alignment works correctly
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const test_types = .{
        u8, // 1-byte alignment
        u16, // 2-byte alignment
        u32, // 4-byte alignment
        u64, // 8-byte alignment
        struct { x: u32, y: u64 }, // Struct with 8-byte alignment
    };

    inline for (test_types) |T| {
        // Create file for this type
        const filename = try std.fmt.allocPrint(gpa, "empty_{s}.dat", .{@typeName(T)});
        defer gpa.free(filename);

        const file = try tmp_dir.dir.createFile(filename, .{ .read = true });
        defer file.close();

        // Create multiple empty lists
        var list1 = SafeList(T){};
        defer list1.deinit(gpa);

        var list2 = SafeList(T){};
        defer list2.deinit(gpa);

        // Add a non-empty u8 list between them to test alignment
        var list_u8 = SafeList(u8){};
        defer list_u8.deinit(gpa);
        _ = try list_u8.append(gpa, 123);

        // Serialize all three
        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
        };
        defer writer.iovecs.deinit(gpa);

        _ = try list1.serialize(gpa, &writer);
        const offset1 = writer.total_bytes - @sizeOf(SafeList(T));

        _ = try list_u8.serialize(gpa, &writer);
        const offset_u8 = writer.total_bytes - @sizeOf(SafeList(u8));

        _ = try list2.serialize(gpa, &writer);
        const offset2 = writer.total_bytes - @sizeOf(SafeList(T));

        // Write to file
        try writer.writeGather(gpa, file);

        // Read back
        try file.seekTo(0);
        const file_size = try file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, 16, file_size);
        defer gpa.free(buffer);

        _ = try file.read(buffer);

        const base = @intFromPtr(buffer.ptr);

        // Verify first empty list
        const d1 = @as(*SafeList(T), @ptrCast(@alignCast(buffer.ptr + offset1)));
        d1.relocate(@as(isize, @intCast(base)));
        try testing.expectEqual(@as(usize, 0), d1.len());

        // Verify non-empty u8 list
        const d_u8 = @as(*SafeList(u8), @ptrCast(@alignCast(buffer.ptr + offset_u8)));
        d_u8.relocate(@as(isize, @intCast(base)));
        try testing.expectEqual(@as(usize, 1), d_u8.len());
        try testing.expectEqual(@as(u8, 123), d_u8.get(@enumFromInt(0)).*);

        // Verify second empty list
        const d2 = @as(*SafeList(T), @ptrCast(@alignCast(buffer.ptr + offset2)));
        d2.relocate(@as(isize, @intCast(base)));
        try testing.expectEqual(@as(usize, 0), d2.len());
    }
}

test "SafeList CompactWriter verify offset calculation" {
    const gpa = testing.allocator;

    // This test verifies that the serialize function correctly stores offsets

    var list = try SafeList(u16).initCapacity(gpa, 4);
    defer list.deinit(gpa);

    _ = try list.append(gpa, 100);
    _ = try list.append(gpa, 200);
    _ = try list.append(gpa, 300);
    _ = try list.append(gpa, 400);

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    const serialized_ptr = try list.serialize(gpa, &writer);

    // The offset should be the aligned size of the data
    // 4 items * 2 bytes = 8 bytes, which is already aligned to 8
    const expected_offset = 8;
    try testing.expectEqual(@as(usize, expected_offset), @intFromPtr(serialized_ptr.items.items.ptr));
}

test "SafeList CompactWriter complete roundtrip example" {
    const gpa = testing.allocator;

    // Step 1: Create original data
    var original = try SafeList(u32).initCapacity(gpa, 4);
    defer original.deinit(gpa);

    _ = try original.append(gpa, 100);
    _ = try original.append(gpa, 200);
    _ = try original.append(gpa, 300);
    _ = try original.append(gpa, 400);

    // Step 2: Create temp file and CompactWriter
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("example.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    // Step 3: Serialize - this writes data first, then the SafeList struct
    const serialized_ptr = try original.serialize(gpa, &writer);

    // Verify the offset is correct (4 * 4 = 16 bytes, already aligned to 8)
    try testing.expectEqual(@as(usize, 16), @intFromPtr(serialized_ptr.items.items.ptr));

    // Step 4: Write to file using vectored I/O
    try writer.writeGather(gpa, file);

    // Step 5: Read file into 16-byte aligned buffer
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Step 6: Cast buffer to SafeList - the struct is at the end
    const list_offset = writer.total_bytes - @sizeOf(SafeList(u32));
    const deserialized = @as(*SafeList(u32), @ptrCast(@alignCast(buffer.ptr + list_offset)));

    // Step 7: Relocate - convert offset to pointer
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Step 8: Verify data is accessible and correct
    try testing.expectEqual(@as(usize, 4), deserialized.len());
    try testing.expectEqual(@as(u32, 100), deserialized.get(@enumFromInt(0)).*);
    try testing.expectEqual(@as(u32, 200), deserialized.get(@enumFromInt(1)).*);
    try testing.expectEqual(@as(u32, 300), deserialized.get(@enumFromInt(2)).*);
    try testing.expectEqual(@as(u32, 400), deserialized.get(@enumFromInt(3)).*);
}

test "SafeList CompactWriter multiple lists with different alignments" {
    const gpa = testing.allocator;

    // Create multiple SafeLists with different element types and alignments

    // 1. SafeList(u8) - 1 byte alignment
    var list_u8 = try SafeList(u8).initCapacity(gpa, 3);
    defer list_u8.deinit(gpa);
    _ = try list_u8.append(gpa, 10);
    _ = try list_u8.append(gpa, 20);
    _ = try list_u8.append(gpa, 30);

    // 2. SafeList(u16) - 2 byte alignment
    var list_u16 = try SafeList(u16).initCapacity(gpa, 2);
    defer list_u16.deinit(gpa);
    _ = try list_u16.append(gpa, 1000);
    _ = try list_u16.append(gpa, 2000);

    // 3. SafeList(u32) - 4 byte alignment
    var list_u32 = try SafeList(u32).initCapacity(gpa, 4);
    defer list_u32.deinit(gpa);
    _ = try list_u32.append(gpa, 100_000);
    _ = try list_u32.append(gpa, 200_000);
    _ = try list_u32.append(gpa, 300_000);
    _ = try list_u32.append(gpa, 400_000);

    // 4. SafeList(u64) - 8 byte alignment
    var list_u64 = try SafeList(u64).initCapacity(gpa, 2);
    defer list_u64.deinit(gpa);
    _ = try list_u64.append(gpa, 10_000_000_000);
    _ = try list_u64.append(gpa, 20_000_000_000);

    // 5. SafeList with a struct type
    const AlignedStruct = struct {
        x: u32,
        y: u64,
        z: u8,
    };
    var list_struct = try SafeList(AlignedStruct).initCapacity(gpa, 2);
    defer list_struct.deinit(gpa);
    _ = try list_struct.append(gpa, .{ .x = 42, .y = 1337, .z = 255 });
    _ = try list_struct.append(gpa, .{ .x = 99, .y = 9999, .z = 128 });

    // Create temp file and CompactWriter
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("multi_list.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    // Serialize all lists and track their positions
    const ptr_u8 = try list_u8.serialize(gpa, &writer);
    const offset_u8 = writer.total_bytes - @sizeOf(SafeList(u8));

    const ptr_u16 = try list_u16.serialize(gpa, &writer);
    const offset_u16 = writer.total_bytes - @sizeOf(SafeList(u16));

    const ptr_u32 = try list_u32.serialize(gpa, &writer);
    const offset_u32 = writer.total_bytes - @sizeOf(SafeList(u32));

    const ptr_u64 = try list_u64.serialize(gpa, &writer);
    const offset_u64 = writer.total_bytes - @sizeOf(SafeList(u64));

    const ptr_struct = try list_struct.serialize(gpa, &writer);
    const offset_struct = writer.total_bytes - @sizeOf(SafeList(AlignedStruct));

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back into aligned buffer
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Deserialize all lists
    const base_addr = @intFromPtr(buffer.ptr);

    // 1. Deserialize u8 list
    const deser_u8 = @as(*SafeList(u8), @ptrCast(@alignCast(buffer.ptr + offset_u8)));
    deser_u8.relocate(@as(isize, @intCast(base_addr)));
    try testing.expectEqual(@as(usize, 3), deser_u8.len());
    try testing.expectEqual(@as(u8, 10), deser_u8.get(@enumFromInt(0)).*);
    try testing.expectEqual(@as(u8, 20), deser_u8.get(@enumFromInt(1)).*);
    try testing.expectEqual(@as(u8, 30), deser_u8.get(@enumFromInt(2)).*);

    // 2. Deserialize u16 list
    const deser_u16 = @as(*SafeList(u16), @ptrCast(@alignCast(buffer.ptr + offset_u16)));
    deser_u16.relocate(@as(isize, @intCast(base_addr)));
    try testing.expectEqual(@as(usize, 2), deser_u16.len());
    try testing.expectEqual(@as(u16, 1000), deser_u16.get(@enumFromInt(0)).*);
    try testing.expectEqual(@as(u16, 2000), deser_u16.get(@enumFromInt(1)).*);

    // 3. Deserialize u32 list
    const deser_u32 = @as(*SafeList(u32), @ptrCast(@alignCast(buffer.ptr + offset_u32)));
    deser_u32.relocate(@as(isize, @intCast(base_addr)));
    try testing.expectEqual(@as(usize, 4), deser_u32.len());
    try testing.expectEqual(@as(u32, 100_000), deser_u32.get(@enumFromInt(0)).*);
    try testing.expectEqual(@as(u32, 200_000), deser_u32.get(@enumFromInt(1)).*);
    try testing.expectEqual(@as(u32, 300_000), deser_u32.get(@enumFromInt(2)).*);
    try testing.expectEqual(@as(u32, 400_000), deser_u32.get(@enumFromInt(3)).*);

    // 4. Deserialize u64 list
    const deser_u64 = @as(*SafeList(u64), @ptrCast(@alignCast(buffer.ptr + offset_u64)));
    deser_u64.relocate(@as(isize, @intCast(base_addr)));
    try testing.expectEqual(@as(usize, 2), deser_u64.len());
    try testing.expectEqual(@as(u64, 10_000_000_000), deser_u64.get(@enumFromInt(0)).*);
    try testing.expectEqual(@as(u64, 20_000_000_000), deser_u64.get(@enumFromInt(1)).*);

    // 5. Deserialize struct list
    const deser_struct = @as(*SafeList(AlignedStruct), @ptrCast(@alignCast(buffer.ptr + offset_struct)));
    deser_struct.relocate(@as(isize, @intCast(base_addr)));
    try testing.expectEqual(@as(usize, 2), deser_struct.len());

    const item0 = deser_struct.get(@enumFromInt(0));
    try testing.expectEqual(@as(u32, 42), item0.x);
    try testing.expectEqual(@as(u64, 1337), item0.y);
    try testing.expectEqual(@as(u8, 255), item0.z);

    const item1 = deser_struct.get(@enumFromInt(1));
    try testing.expectEqual(@as(u32, 99), item1.x);
    try testing.expectEqual(@as(u64, 9999), item1.y);
    try testing.expectEqual(@as(u8, 128), item1.z);

    // Verify the serialized pointers match what we deserialized
    _ = ptr_u8;
    _ = ptr_u16;
    _ = ptr_u32;
    _ = ptr_u64;
    _ = ptr_struct;
}

test "SafeList CompactWriter interleaved pattern with alignment tracking" {
    const gpa = testing.allocator;

    // This test demonstrates how alignment padding works when serializing
    // multiple lists in an interleaved pattern

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    // Track offsets as we go
    var offsets = std.ArrayList(usize).init(gpa);
    defer offsets.deinit();

    // Create temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const file = try tmp_dir.dir.createFile("interleaved.dat", .{ .read = true });
    defer file.close();

    // Pattern: u8 list, u64 list, u16 list, u32 list
    // This creates interesting alignment requirements

    // 1. u8 list (1-byte aligned, 3 elements = 3 bytes)
    var list1 = try SafeList(u8).initCapacity(gpa, 3);
    defer list1.deinit(gpa);
    _ = try list1.append(gpa, 1);
    _ = try list1.append(gpa, 2);
    _ = try list1.append(gpa, 3);

    const start1 = writer.total_bytes;
    _ = try list1.serialize(gpa, &writer);
    try offsets.append(writer.total_bytes - @sizeOf(SafeList(u8)));

    // 2. u64 list (8-byte aligned, forces significant padding)
    var list2 = try SafeList(u64).initCapacity(gpa, 2);
    defer list2.deinit(gpa);
    _ = try list2.append(gpa, 1_000_000);
    _ = try list2.append(gpa, 2_000_000);

    const start2 = writer.total_bytes;
    _ = try list2.serialize(gpa, &writer);
    try offsets.append(writer.total_bytes - @sizeOf(SafeList(u64)));

    // Verify padding was added before u64 data
    const padding_before_u64 = start2 - start1 - (3 + @sizeOf(SafeList(u8)));
    try testing.expect(padding_before_u64 > 0);

    // 3. u16 list (2-byte aligned)
    var list3 = try SafeList(u16).initCapacity(gpa, 4);
    defer list3.deinit(gpa);
    _ = try list3.append(gpa, 100);
    _ = try list3.append(gpa, 200);
    _ = try list3.append(gpa, 300);
    _ = try list3.append(gpa, 400);

    _ = try list3.serialize(gpa, &writer);
    try offsets.append(writer.total_bytes - @sizeOf(SafeList(u16)));

    // 4. u32 list (4-byte aligned)
    var list4 = try SafeList(u32).initCapacity(gpa, 1);
    defer list4.deinit(gpa);
    _ = try list4.append(gpa, 42);

    _ = try list4.serialize(gpa, &writer);
    try offsets.append(writer.total_bytes - @sizeOf(SafeList(u32)));

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back and verify
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);
    _ = try file.read(buffer);

    const base = @intFromPtr(buffer.ptr);

    // Deserialize and verify all lists
    const d1 = @as(*SafeList(u8), @ptrCast(@alignCast(buffer.ptr + offsets.items[0])));
    d1.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 3), d1.len());
    try testing.expectEqual(@as(u8, 1), d1.get(@enumFromInt(0)).*);

    const d2 = @as(*SafeList(u64), @ptrCast(@alignCast(buffer.ptr + offsets.items[1])));
    d2.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 2), d2.len());
    try testing.expectEqual(@as(u64, 1_000_000), d2.get(@enumFromInt(0)).*);

    const d3 = @as(*SafeList(u16), @ptrCast(@alignCast(buffer.ptr + offsets.items[2])));
    d3.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 4), d3.len());
    try testing.expectEqual(@as(u16, 100), d3.get(@enumFromInt(0)).*);

    const d4 = @as(*SafeList(u32), @ptrCast(@alignCast(buffer.ptr + offsets.items[3])));
    d4.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 1), d4.len());
    try testing.expectEqual(@as(u32, 42), d4.get(@enumFromInt(0)).*);
}

test "SafeList CompactWriter brute-force alignment verification" {
    const gpa = testing.allocator;

    // Test all combinations of slice lengths from 0 to 8 for different types
    // This ensures our alignment padding works correctly for all cases

    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Test different types with different alignments
    const test_types = .{
        u8, // 1-byte alignment
        u16, // 2-byte alignment
        u32, // 4-byte alignment
        u64, // 8-byte alignment
    };

    inline for (test_types) |T| {
        // Try all lengths from 0 to 8
        var length: usize = 0;
        while (length <= 8) : (length += 1) {
            // Create a file for this test case
            const filename = try std.fmt.allocPrint(gpa, "test_{s}_len_{}.dat", .{ @typeName(T), length });
            defer gpa.free(filename);

            const file = try tmp_dir.dir.createFile(filename, .{ .read = true });
            defer file.close();

            // Create lists with the specific length
            var list1 = SafeList(T){};
            defer list1.deinit(gpa);

            var i: usize = 0;
            while (i < length) : (i += 1) {
                _ = try list1.append(gpa, @as(T, @intCast(i + 1)));
            }

            // Also create a second list with different data
            var list2 = SafeList(T){};
            defer list2.deinit(gpa);

            i = 0;
            while (i < length) : (i += 1) {
                _ = try list2.append(gpa, @as(T, @intCast((i + 1) * 100)));
            }

            // Create a u8 list to add between them (to test alignment)
            var list_u8 = SafeList(u8){};
            defer list_u8.deinit(gpa);
            _ = try list_u8.append(gpa, 42);

            // Serialize everything
            var writer = CompactWriter{
                .iovecs = .{},
                .total_bytes = 0,
            };
            defer writer.iovecs.deinit(gpa);

            // Serialize in pattern: list1, u8 list, list2
            // This tests alignment padding between different types
            _ = try list1.serialize(gpa, &writer);
            const offset1 = writer.total_bytes - @sizeOf(SafeList(T));

            _ = try list_u8.serialize(gpa, &writer);
            const offset_u8 = writer.total_bytes - @sizeOf(SafeList(u8));

            _ = try list2.serialize(gpa, &writer);
            const offset2 = writer.total_bytes - @sizeOf(SafeList(T));

            // Write to file
            try writer.writeGather(gpa, file);

            // Read back
            try file.seekTo(0);
            const file_size = try file.getEndPos();
            const buffer = try gpa.alignedAlloc(u8, 16, file_size);
            defer gpa.free(buffer);

            _ = try file.read(buffer);

            // Deserialize and verify
            const base = @intFromPtr(buffer.ptr);

            // Verify first list
            const d1 = @as(*SafeList(T), @ptrCast(@alignCast(buffer.ptr + offset1)));
            d1.relocate(@as(isize, @intCast(base)));
            try testing.expectEqual(length, d1.len());

            i = 0;
            while (i < length) : (i += 1) {
                const expected = @as(T, @intCast(i + 1));
                const actual = d1.get(@enumFromInt(i)).*;
                try testing.expectEqual(expected, actual);
            }

            // Verify u8 list
            const d_u8 = @as(*SafeList(u8), @ptrCast(@alignCast(buffer.ptr + offset_u8)));
            d_u8.relocate(@as(isize, @intCast(base)));
            try testing.expectEqual(@as(usize, 1), d_u8.len());
            try testing.expectEqual(@as(u8, 42), d_u8.get(@enumFromInt(0)).*);

            // Verify second list
            const d2 = @as(*SafeList(T), @ptrCast(@alignCast(buffer.ptr + offset2)));
            d2.relocate(@as(isize, @intCast(base)));
            try testing.expectEqual(length, d2.len());

            i = 0;
            while (i < length) : (i += 1) {
                const expected = @as(T, @intCast((i + 1) * 100));
                const actual = d2.get(@enumFromInt(i)).*;
                try testing.expectEqual(expected, actual);
            }
        }
    }
}

test "SafeMultiList CompactWriter roundtrip with file" {
    const gpa = testing.allocator;

    // Create a SafeMultiList with test data
    const TestStruct = struct {
        id: u32,
        value: u64,
        flag: bool,
        data: u8,
    };

    var original = try SafeMultiList(TestStruct).initCapacity(gpa, 4);
    defer original.deinit(gpa);

    _ = try original.append(gpa, .{ .id = 100, .value = 1000, .flag = true, .data = 10 });
    _ = try original.append(gpa, .{ .id = 200, .value = 2000, .flag = false, .data = 20 });
    _ = try original.append(gpa, .{ .id = 300, .value = 3000, .flag = true, .data = 30 });
    _ = try original.append(gpa, .{ .id = 400, .value = 4000, .flag = false, .data = 40 });

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multi.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back into aligned buffer
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SafeMultiList(TestStruct), @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SafeMultiList(TestStruct)))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the data
    try testing.expectEqual(@as(usize, 4), deserialized.len());

    try testing.expectEqual(@as(u32, 100), deserialized.get(@enumFromInt(0)).id);
    try testing.expectEqual(@as(u64, 1000), deserialized.get(@enumFromInt(0)).value);
    try testing.expectEqual(true, deserialized.get(@enumFromInt(0)).flag);
    try testing.expectEqual(@as(u8, 10), deserialized.get(@enumFromInt(0)).data);

    try testing.expectEqual(@as(u32, 200), deserialized.get(@enumFromInt(1)).id);
    try testing.expectEqual(@as(u64, 2000), deserialized.get(@enumFromInt(1)).value);
    try testing.expectEqual(false, deserialized.get(@enumFromInt(1)).flag);
    try testing.expectEqual(@as(u8, 20), deserialized.get(@enumFromInt(1)).data);

    try testing.expectEqual(@as(u32, 300), deserialized.get(@enumFromInt(2)).id);
    try testing.expectEqual(@as(u64, 3000), deserialized.get(@enumFromInt(2)).value);
    try testing.expectEqual(true, deserialized.get(@enumFromInt(2)).flag);
    try testing.expectEqual(@as(u8, 30), deserialized.get(@enumFromInt(2)).data);

    try testing.expectEqual(@as(u32, 400), deserialized.get(@enumFromInt(3)).id);
    try testing.expectEqual(@as(u64, 4000), deserialized.get(@enumFromInt(3)).value);
    try testing.expectEqual(false, deserialized.get(@enumFromInt(3)).flag);
    try testing.expectEqual(@as(u8, 40), deserialized.get(@enumFromInt(3)).data);
}

test "SafeMultiList empty list CompactWriter roundtrip" {
    const gpa = testing.allocator;

    const TestStruct = struct {
        x: u32,
        y: u64,
    };

    // Create an empty SafeMultiList
    var original = SafeMultiList(TestStruct){};
    defer original.deinit(gpa);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_multi.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*SafeMultiList(TestStruct), @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SafeMultiList(TestStruct)))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.len());
}

test "SafeMultiList CompactWriter multiple lists different alignments" {
    const gpa = testing.allocator;

    // Create multiple SafeMultiLists with different field types
    const Type1 = struct {
        a: u8,
        b: u16,
    };

    const Type2 = struct {
        x: u32,
        y: u64,
    };

    const Type3 = struct {
        id: u64,
        data: u8,
        flag: bool,
    };

    var list1 = try SafeMultiList(Type1).initCapacity(gpa, 3);
    defer list1.deinit(gpa);
    _ = try list1.append(gpa, .{ .a = 10, .b = 100 });
    _ = try list1.append(gpa, .{ .a = 20, .b = 200 });
    _ = try list1.append(gpa, .{ .a = 30, .b = 300 });

    var list2 = try SafeMultiList(Type2).initCapacity(gpa, 2);
    defer list2.deinit(gpa);
    _ = try list2.append(gpa, .{ .x = 1000, .y = 10000 });
    _ = try list2.append(gpa, .{ .x = 2000, .y = 20000 });

    var list3 = try SafeMultiList(Type3).initCapacity(gpa, 2);
    defer list3.deinit(gpa);
    _ = try list3.append(gpa, .{ .id = 999, .data = 42, .flag = true });
    _ = try list3.append(gpa, .{ .id = 888, .data = 84, .flag = false });

    // Create temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("multi_types.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    // Serialize all lists
    _ = try list1.serialize(gpa, &writer);
    const offset1 = writer.total_bytes - @sizeOf(SafeMultiList(Type1));

    _ = try list2.serialize(gpa, &writer);
    const offset2 = writer.total_bytes - @sizeOf(SafeMultiList(Type2));

    _ = try list3.serialize(gpa, &writer);
    const offset3 = writer.total_bytes - @sizeOf(SafeMultiList(Type3));

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    const base = @intFromPtr(buffer.ptr);

    // Deserialize list1
    const d1 = @as(*SafeMultiList(Type1), @ptrCast(@alignCast(buffer.ptr + offset1)));
    d1.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 3), d1.len());
    try testing.expectEqual(@as(u8, 10), d1.get(@enumFromInt(0)).a);
    try testing.expectEqual(@as(u16, 100), d1.get(@enumFromInt(0)).b);
    try testing.expectEqual(@as(u8, 20), d1.get(@enumFromInt(1)).a);
    try testing.expectEqual(@as(u16, 200), d1.get(@enumFromInt(1)).b);

    // Deserialize list2
    const d2 = @as(*SafeMultiList(Type2), @ptrCast(@alignCast(buffer.ptr + offset2)));
    d2.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 2), d2.len());
    try testing.expectEqual(@as(u32, 1000), d2.get(@enumFromInt(0)).x);
    try testing.expectEqual(@as(u64, 10000), d2.get(@enumFromInt(0)).y);

    // Deserialize list3
    const d3 = @as(*SafeMultiList(Type3), @ptrCast(@alignCast(buffer.ptr + offset3)));
    d3.relocate(@as(isize, @intCast(base)));
    try testing.expectEqual(@as(usize, 2), d3.len());
    try testing.expectEqual(@as(u64, 999), d3.get(@enumFromInt(0)).id);
    try testing.expectEqual(@as(u8, 42), d3.get(@enumFromInt(0)).data);
    try testing.expectEqual(true, d3.get(@enumFromInt(0)).flag);
}

test "SafeMultiList CompactWriter field access after deserialization" {
    const gpa = testing.allocator;

    // Test that we can access individual fields after deserialization
    const TestStruct = struct {
        id: u32,
        value: u64,
        flag: bool,
        data: u8,
    };

    var original = try SafeMultiList(TestStruct).initCapacity(gpa, 3);
    defer original.deinit(gpa);

    _ = try original.append(gpa, .{ .id = 111, .value = 222, .flag = true, .data = 33 });
    _ = try original.append(gpa, .{ .id = 444, .value = 555, .flag = false, .data = 66 });
    _ = try original.append(gpa, .{ .id = 777, .value = 888, .flag = true, .data = 99 });

    // Create temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_field_access.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Deserialize
    const deserialized = @as(*SafeMultiList(TestStruct), @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(SafeMultiList(TestStruct)))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Test field access
    const ids = deserialized.field(.id);
    try testing.expectEqual(@as(u32, 111), ids[0]);
    try testing.expectEqual(@as(u32, 444), ids[1]);
    try testing.expectEqual(@as(u32, 777), ids[2]);

    const values = deserialized.field(.value);
    try testing.expectEqual(@as(u64, 222), values[0]);
    try testing.expectEqual(@as(u64, 555), values[1]);
    try testing.expectEqual(@as(u64, 888), values[2]);

    const flags = deserialized.field(.flag);
    try testing.expectEqual(true, flags[0]);
    try testing.expectEqual(false, flags[1]);
    try testing.expectEqual(true, flags[2]);

    const data_field = deserialized.field(.data);
    try testing.expectEqual(@as(u8, 33), data_field[0]);
    try testing.expectEqual(@as(u8, 66), data_field[1]);
    try testing.expectEqual(@as(u8, 99), data_field[2]);

    // Test fieldItem access
    try testing.expectEqual(@as(u32, 111), deserialized.fieldItem(.id, @enumFromInt(0)));
    try testing.expectEqual(@as(u64, 555), deserialized.fieldItem(.value, @enumFromInt(1)));
    try testing.expectEqual(true, deserialized.fieldItem(.flag, @enumFromInt(2)));
    try testing.expectEqual(@as(u8, 66), deserialized.fieldItem(.data, @enumFromInt(1)));
}

test "SafeMultiList CompactWriter brute-force alignment verification" {
    const gpa = testing.allocator;

    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Test with various struct configurations
    const TestType = struct {
        a: u8,
        b: u32,
        c: u64,
    };

    // Test all lengths from 0 to 8
    var length: usize = 0;
    while (length <= 8) : (length += 1) {
        const filename = try std.fmt.allocPrint(gpa, "multi_brute_{}.dat", .{length});
        defer gpa.free(filename);

        const file = try tmp_dir.dir.createFile(filename, .{ .read = true });
        defer file.close();

        // Create list with specific length but larger capacity to test compaction
        var list = try SafeMultiList(TestType).initCapacity(gpa, length + 5);
        defer list.deinit(gpa);

        var i: usize = 0;
        while (i < length) : (i += 1) {
            _ = try list.append(gpa, .{
                .a = @as(u8, @intCast(i)),
                .b = @as(u32, @intCast(i * 100)),
                .c = @as(u64, @intCast(i * 1000)),
            });
        }

        // Verify we have extra capacity that shouldn't be serialized
        try testing.expect(list.items.capacity >= length + 5);

        // Add another list to test alignment between lists
        var list2 = SafeMultiList(TestType){};
        defer list2.deinit(gpa);
        if (length > 0) {
            _ = try list2.append(gpa, .{ .a = 255, .b = 999999, .c = 888888888 });
        }

        // Serialize
        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
        };
        defer writer.iovecs.deinit(gpa);

        _ = try list.serialize(gpa, &writer);
        const offset1 = writer.total_bytes - @sizeOf(SafeMultiList(TestType));

        _ = try list2.serialize(gpa, &writer);
        const offset2 = writer.total_bytes - @sizeOf(SafeMultiList(TestType));

        // Write to file
        try writer.writeGather(gpa, file);

        // Read back
        try file.seekTo(0);
        const file_size = try file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, 16, file_size);
        defer gpa.free(buffer);

        _ = try file.read(buffer);

        const base = @intFromPtr(buffer.ptr);

        // Verify first list
        const d1 = @as(*SafeMultiList(TestType), @ptrCast(@alignCast(buffer.ptr + offset1)));
        d1.relocate(@as(isize, @intCast(base)));
        try testing.expectEqual(length, d1.len());

        i = 0;
        while (i < length) : (i += 1) {
            const item = d1.get(@enumFromInt(i));
            try testing.expectEqual(@as(u8, @intCast(i)), item.a);
            try testing.expectEqual(@as(u32, @intCast(i * 100)), item.b);
            try testing.expectEqual(@as(u64, @intCast(i * 1000)), item.c);
        }

        // Verify second list
        const d2 = @as(*SafeMultiList(TestType), @ptrCast(@alignCast(buffer.ptr + offset2)));
        d2.relocate(@as(isize, @intCast(base)));
        if (length > 0) {
            try testing.expectEqual(@as(usize, 1), d2.len());
            try testing.expectEqual(@as(u8, 255), d2.get(@enumFromInt(0)).a);
            try testing.expectEqual(@as(u32, 999999), d2.get(@enumFromInt(0)).b);
            try testing.expectEqual(@as(u64, 888888888), d2.get(@enumFromInt(0)).c);
        } else {
            try testing.expectEqual(@as(usize, 0), d2.len());
        }
    }
}

test "SafeMultiList CompactWriter various field alignments and sizes" {
    const gpa = testing.allocator;

    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    // Test different field alignment combinations
    const test_configs = .{
        // Type with increasing alignments
        struct { a: u8, b: u16, c: u32, d: u64 },
        // Type with decreasing alignments
        struct { a: u64, b: u32, c: u16, d: u8 },
        // Type with mixed alignments
        struct { a: u8, b: u64, c: u16, d: u32 },
        // Type with same alignment different sizes
        struct { a: u32, b: i32, c: f32 },
        // Type with bool and padding
        struct { flag: bool, value: u64, data: u8 },
    };

    inline for (test_configs) |TestType| {
        // Test lengths 0, 1, 3, 7 to cover various cases
        const test_lengths = [_]usize{ 0, 1, 3, 7 };

        for (test_lengths) |len| {
            var list = try SafeMultiList(TestType).initCapacity(gpa, len + 10);
            defer list.deinit(gpa);

            // Fill with test data
            var i: usize = 0;
            while (i < len) : (i += 1) {
                var item: TestType = undefined;
                inline for (std.meta.fields(TestType), 0..) |field, fi| {
                    const value = @as(field.type, @intCast(@min(i * (fi + 1) + 1, std.math.maxInt(field.type))));
                    @field(item, field.name) = value;
                }
                _ = try list.append(gpa, item);
            }

            // Serialize and deserialize
            const filename = try std.fmt.allocPrint(gpa, "align_test_{s}_{}.dat", .{ @typeName(TestType), len });
            defer gpa.free(filename);

            const file = try tmp_dir.dir.createFile(filename, .{ .read = true });
            defer file.close();

            var writer = CompactWriter{
                .iovecs = .{},
                .total_bytes = 0,
            };
            defer writer.iovecs.deinit(gpa);

            _ = try list.serialize(gpa, &writer);
            try writer.writeGather(gpa, file);

            // Read back
            try file.seekTo(0);
            const file_size = try file.getEndPos();
            const buffer = try gpa.alignedAlloc(u8, 16, file_size);
            defer gpa.free(buffer);

            _ = try file.read(buffer);

            // Deserialize
            const offset = writer.total_bytes - @sizeOf(SafeMultiList(TestType));
            const deserialized = @as(*SafeMultiList(TestType), @ptrCast(@alignCast(buffer.ptr + offset)));
            deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

            // Verify
            try testing.expectEqual(len, deserialized.len());

            i = 0;
            while (i < len) : (i += 1) {
                const original_item = list.get(@enumFromInt(i));
                const deser_item = deserialized.get(@enumFromInt(i));

                inline for (std.meta.fields(TestType)) |field| {
                    try testing.expectEqual(@field(original_item, field.name), @field(deser_item, field.name));
                }
            }
            try testing.expectEqual(@as(usize, 0), deserialized.len());
        }
    }
}

test "SafeMultiList CompactWriter verify exact memory layout" {
    const gpa = testing.allocator;

    // Test that our serialization produces the exact memory layout that MultiArrayList expects
    const TestStruct = struct {
        a: u8,
        b: u32,
        c: u16,
        d: u64,
    };

    // Test with various lengths to ensure layout is correct
    const test_lengths = [_]usize{ 1, 2, 3, 5, 8 };

    for (test_lengths) |len| {
        // Create a list with test data
        var original = try SafeMultiList(TestStruct).initCapacity(gpa, len + 10);
        defer original.deinit(gpa);

        var i: usize = 0;
        while (i < len) : (i += 1) {
            _ = try original.append(gpa, .{
                .a = @as(u8, @intCast(i + 10)),
                .b = @as(u32, @intCast(i + 100)),
                .c = @as(u16, @intCast(i + 1000)),
                .d = @as(u64, @intCast(i + 10000)),
            });
        }

        // Manually create the expected memory layout
        const expected_bytes = try gpa.alloc(u8, std.MultiArrayList(TestStruct).capacityInBytes(len));
        defer gpa.free(expected_bytes);

        // Calculate field offsets as MultiArrayList would
        var offset: usize = 0;
        const fields = std.meta.fields(TestStruct);

        // Write each field array at its proper offset
        inline for (fields, 0..) |field, field_idx| {
            const field_type = field.type;
            const field_align = @alignOf(field_type);
            const field_size = @sizeOf(field_type);

            // Align offset for this field
            offset = std.mem.alignForward(usize, offset, field_align);

            // Copy field data
            const field_enum = @as(SafeMultiList(TestStruct).Field, @enumFromInt(field_idx));
            const field_items = original.field(field_enum);
            @memcpy(expected_bytes[offset..][0 .. field_size * len], std.mem.sliceAsBytes(field_items[0..len]));

            offset += field_size * len;
        }

        // Now serialize using our implementation
        const tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();

        const file = try tmp_dir.dir.createFile("layout_test.dat", .{ .read = true });
        defer file.close();

        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
        };
        defer writer.iovecs.deinit(gpa);

        _ = try original.serialize(gpa, &writer);
        try writer.writeGather(gpa, file);

        // Read back
        try file.seekTo(0);
        const file_size = try file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, 16, file_size);
        defer gpa.free(buffer);

        _ = try file.read(buffer);

        // Extract the data portion (before the SafeMultiList struct)
        const data_size = std.MultiArrayList(TestStruct).capacityInBytes(len);
        const serialized_data = buffer[0..data_size];

        // Verify byte-for-byte equality
        try testing.expectEqualSlices(u8, expected_bytes, serialized_data);

        // Also verify it deserializes correctly
        const offset_struct = writer.total_bytes - @sizeOf(SafeMultiList(TestStruct));
        const deserialized = @as(*SafeMultiList(TestStruct), @ptrCast(@alignCast(buffer.ptr + offset_struct)));
        deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

        // Verify all data is accessible
        i = 0;
        while (i < len) : (i += 1) {
            const item = deserialized.get(@enumFromInt(i));
            try testing.expectEqual(@as(u8, @intCast(i + 10)), item.a);
            try testing.expectEqual(@as(u32, @intCast(i + 100)), item.b);
            try testing.expectEqual(@as(u16, @intCast(i + 1000)), item.c);
            try testing.expectEqual(@as(u64, @intCast(i + 10000)), item.d);
        }
    }
}

test "SafeMultiList CompactWriter stress test many field types" {
    const gpa = testing.allocator;

    // Test with a complex struct with many fields of different types and alignments
    const ComplexStruct = struct {
        flag1: bool,
        byte1: u8,
        short1: u16,
        int1: u32,
        long1: u64,
        byte2: u8,
        flag2: bool,
        short2: u16,
        float1: f32,
        long2: u64,
        int2: u32,
        double1: f64,
    };

    const test_lengths = [_]usize{ 0, 1, 4, 9 };

    for (test_lengths) |len| {
        var list = try SafeMultiList(ComplexStruct).initCapacity(gpa, len + 20);
        defer list.deinit(gpa);

        // Fill with data
        var i: usize = 0;
        while (i < len) : (i += 1) {
            _ = try list.append(gpa, .{
                .flag1 = (i % 2) == 0,
                .byte1 = @as(u8, @intCast(i * 2)),
                .short1 = @as(u16, @intCast(i * 10)),
                .int1 = @as(u32, @intCast(i * 100)),
                .long1 = @as(u64, @intCast(i * 1000)),
                .byte2 = @as(u8, @intCast(i * 3)),
                .flag2 = (i % 3) == 0,
                .short2 = @as(u16, @intCast(i * 20)),
                .float1 = @as(f32, @floatFromInt(i)) * 1.5,
                .long2 = @as(u64, @intCast(i * 2000)),
                .int2 = @as(u32, @intCast(i * 200)),
                .double1 = @as(f64, @floatFromInt(i)) * 2.5,
            });
        }

        const tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();

        const file = try tmp_dir.dir.createFile("complex_test.dat", .{ .read = true });
        defer file.close();

        var writer = CompactWriter{
            .iovecs = .{},
            .total_bytes = 0,
        };
        defer writer.iovecs.deinit(gpa);

        _ = try list.serialize(gpa, &writer);
        try writer.writeGather(gpa, file);

        // Read back
        try file.seekTo(0);
        const file_size = try file.getEndPos();
        const buffer = try gpa.alignedAlloc(u8, 16, file_size);
        defer gpa.free(buffer);

        _ = try file.read(buffer);

        // Deserialize
        const offset = writer.total_bytes - @sizeOf(SafeMultiList(ComplexStruct));
        const deserialized = @as(*SafeMultiList(ComplexStruct), @ptrCast(@alignCast(buffer.ptr + offset)));
        deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

        // Verify
        try testing.expectEqual(len, deserialized.len());

        i = 0;
        while (i < len) : (i += 1) {
            const original_item = list.get(@enumFromInt(i));
            const deser_item = deserialized.get(@enumFromInt(i));

            try testing.expectEqual(original_item.flag1, deser_item.flag1);
            try testing.expectEqual(original_item.byte1, deser_item.byte1);
            try testing.expectEqual(original_item.short1, deser_item.short1);
            try testing.expectEqual(original_item.int1, deser_item.int1);
            try testing.expectEqual(original_item.long1, deser_item.long1);
            try testing.expectEqual(original_item.byte2, deser_item.byte2);
            try testing.expectEqual(original_item.flag2, deser_item.flag2);
            try testing.expectEqual(original_item.short2, deser_item.short2);
            try testing.expectEqual(original_item.float1, deser_item.float1);
            try testing.expectEqual(original_item.long2, deser_item.long2);
            try testing.expectEqual(original_item.int2, deser_item.int2);
            try testing.expectEqual(original_item.double1, deser_item.double1);
        }
    }
}

test "SafeMultiList CompactWriter empty with capacity" {
    const gpa = testing.allocator;

    // Test that empty lists with capacity serialize correctly
    const TestStruct = struct {
        x: u32,
        y: u64,
        z: u8,
    };

    var list = try SafeMultiList(TestStruct).initCapacity(gpa, 50);
    defer list.deinit(gpa);

    // Verify it has capacity but no elements
    try testing.expect(list.items.capacity >= 50);
    try testing.expectEqual(@as(usize, 0), list.len());

    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("empty_capacity.dat", .{ .read = true });
    defer file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.iovecs.deinit(gpa);

    _ = try list.serialize(gpa, &writer);
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Deserialize
    const offset = writer.total_bytes - @sizeOf(SafeMultiList(TestStruct));
    const deserialized = @as(*SafeMultiList(TestStruct), @ptrCast(@alignCast(buffer.ptr + offset)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify it's still empty
    try testing.expectEqual(@as(usize, 0), deserialized.len());
    // Capacity should be 0 after compaction
    try testing.expectEqual(@as(usize, 0), deserialized.items.capacity);
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
