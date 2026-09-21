//! Lists that make it easier to avoid incorrect indexing.

const std = @import("std");

const testing = std.testing;
const Allocator = std.mem.Allocator;

const CompactWriter = @import("CompactWriter.zig");

/// L-10 bounds check: reject an `(offset)`+`span_bytes` extent that would reach
/// outside the `backing_len`-byte relocated buffer (truncated/corrupt blob), with
/// overflow-safe arithmetic. An empty span is always valid. The single primitive
/// behind every relocatable marker's `validateRelocations`—`SafeList.Serialized`/
/// `SafeMultiList.Serialized` call it directly, and `artifact_serialize`'s
/// element-count-based `validateOffsetLen` delegates here after computing the byte
/// extent (it lives in `collections`, the shared lower layer both can reach).
pub fn validateRelocatedSpan(elem_align: u64, offset: i64, span_bytes: u64, backing_len: u64) error{CorruptArtifact}!void {
    if (span_bytes == 0) return;
    if (offset < 0) return error.CorruptArtifact;
    const off: u64 = @intCast(offset);
    if (elem_align != 0 and off % elem_align != 0) return error.CorruptArtifact;
    const end = std.math.add(u64, off, span_bytes) catch return error.CorruptArtifact;
    if (end > backing_len) return error.CorruptArtifact;
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
        count: u32,

        /// An empty range
        pub fn empty() Self {
            return .{ .start = undefined, .count = 0 };
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
        items: std.ArrayList(T) = .empty,

        /// An index for an item in the list.
        pub const Idx = enum(u32) {
            /// The first valid index in the list.
            first = 0,
            _,

            /// Get the raw u32 value for storage
            pub fn toU32(self: Idx) u32 {
                return @intFromEnum(self);
            }

            /// Create from a raw u32 value
            pub fn fromU32(value: u32) Idx {
                return @enumFromInt(value);
            }
        };

        /// A non-type-safe slice of the list.
        pub const Slice = std.ArrayList(T).Slice;

        /// A type-safe range of the list.
        pub const Range = SafeRange(Idx);

        /// A type-safe range which must have at least one element.
        pub const NonEmptyRange = struct {
            nonempty: Range,
        };

        /// Serialized representation of a SafeList
        /// Uses extern struct to guarantee consistent field layout across optimization levels.
        pub const Serialized = extern struct {
            offset: i64,
            len: u64,
            capacity: u64,

            /// One relocatable base pointer (`offset`) is fixed up per `deserializeInto`/
            /// `relocate`, regardless of `len`. Counted by
            /// `artifact_serialize.relocatablePointerCount` so a composing store's total
            /// fixup count includes its `SafeList`-backed (e.g. interner) fields.
            pub const serialized_relocatable_pointers: usize = 1;

            /// The element type whose bytes are serialized, so a layout fingerprint can
            /// reflect a change to the element's field order/size.
            pub const SerializedElement = T;

            /// L-10: reject an `(offset, len)` whose `len` elements reach outside the
            /// `backing_len`-byte buffer before `deserializeInto` dereferences it.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                const span_bytes = std.math.mul(u64, self.len, @sizeOf(T)) catch return error.CorruptArtifact;
                try validateRelocatedSpan(@alignOf(T), self.offset, span_bytes, backing_len);
            }

            /// Serialize a SafeList into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                safe_list: *const SafeList(T),
                allocator: Allocator,
                writer: *CompactWriter,
            ) Allocator.Error!void {
                const items = safe_list.items.items;
                // An integer offset, so a store whose items legitimately begin at byte
                // zero records zero rather than an unrepresentable null pointer.
                const data_offset = try writer.appendSlicePodZeroedOffset(allocator, @as([]const T, items));

                self.offset = @intCast(data_offset);
                self.len = items.len;
                self.capacity = items.len;
            }

            /// Deserialize into a SafeList value (no in-place modification of cache buffer).
            /// The base parameter is the base address of the serialized buffer in memory.
            /// WARNING: The returned SafeList points into the cache buffer and CANNOT be grown.
            /// Use deserializeWithCopy() if the list needs to be mutable.
            pub fn deserializeInto(self: *const Serialized, base: usize) SafeList(T) {
                // Handle empty list case
                if (self.len == 0) {
                    return SafeList(T){ .items = .empty };
                }

                // Apply the base address to convert from serialized offset to actual pointer
                const items_ptr: [*]T = @ptrFromInt(base +% @as(usize, @intCast(self.offset)));

                return SafeList(T){
                    .items = .{
                        .items = items_ptr[0..@intCast(self.len)],
                        .capacity = @intCast(self.capacity),
                    },
                };
            }

            /// Deserialize into a SafeList value with fresh memory allocation.
            /// The returned SafeList owns its memory and can be safely grown/mutated.
            pub fn deserializeWithCopy(self: *const Serialized, base: usize, gpa: Allocator) Allocator.Error!SafeList(T) {
                // Handle empty list case
                if (self.len == 0) {
                    return SafeList(T){ .items = .empty };
                }

                // Get pointer to source data in cache buffer
                const src_ptr: [*]const T = @ptrFromInt(base +% @as(usize, @intCast(self.offset)));
                const item_len: usize = @intCast(self.len);
                const item_capacity: usize = @intCast(self.capacity);
                const src_slice = src_ptr[0..item_len];

                // Allocate fresh memory with full capacity
                const fresh_items = try gpa.alloc(T, item_capacity);
                @memcpy(fresh_items[0..item_len], src_slice);

                return SafeList(T){
                    .items = .{
                        .items = fresh_items[0..item_len],
                        .capacity = item_capacity,
                    },
                };
            }
        };

        /// Initialize the `SafeList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!SafeList(T) {
            return .{
                .items = try std.ArrayList(T).initCapacity(gpa, capacity),
            };
        }

        /// Deinitialize the memory of this `SafeList`.
        pub fn deinit(self: *SafeList(T), gpa: Allocator) void {
            self.items.deinit(gpa);
        }

        /// Clone this list into fresh owned memory.
        pub fn clone(self: *const SafeList(T), gpa: Allocator) Allocator.Error!SafeList(T) {
            var cloned = try SafeList(T).initCapacity(gpa, self.items.capacity);
            errdefer cloned.deinit(gpa);
            try cloned.items.appendSlice(gpa, self.items.items);
            return cloned;
        }

        /// Get the length of this list.
        pub fn len(self: *const SafeList(T)) u64 {
            return @intCast(self.items.items.len);
        }

        /// Add an item to the end of this list.
        pub fn append(self: *SafeList(T), gpa: Allocator, item: T) std.mem.Allocator.Error!Idx {
            const length = self.len();
            try self.items.append(gpa, item);

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        /// Add a new item to the end of this list assuming capacity is sufficient to hold an additional item.
        pub fn appendAssumeCapacity(self: *SafeList(T), item: T) Idx {
            const length = self.len();
            self.items.appendAssumeCapacity(item);

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
            // Empty ranges have undefined start, return empty slice directly
            if (range.count == 0) {
                return &.{};
            }

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

            const offset_self = try writer.appendAlloc(allocator, SafeList(T));
            const written = try writer.appendSlicePodZeroed(allocator, @as([]const T, items));

            offset_self.* = .{
                .items = .{
                    .items = @constCast(written),
                    .capacity = items.len,
                },
            };

            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *SafeList(T), offset: isize) void {
            if (self.items.capacity == 0) return;

            const old_addr: isize = @intCast(@intFromPtr(self.items.items.ptr));
            const new_addr = @as(usize, @intCast(old_addr + offset));
            self.items.items.ptr = @as([*]T, @ptrFromInt(new_addr));
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
                .current = .first,
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
        const Self = @This();

        items: std.MultiArrayList(T) = .empty,

        comptime {
            if (@typeInfo(T) != .@"struct") {
                @compileError("SafeMultiList element '" ++ @typeName(T) ++
                    "' must be a struct: the serialized column layout is derived from its fields");
            }
        }

        /// The order in which `std.MultiArrayList(T)` places its field columns in one
        /// allocation: descending field alignment, ties in declaration order. Each
        /// column is `@sizeOf(field) * capacity` bytes and they are packed with no gaps
        /// between them, because a field's size is a multiple of its alignment and the
        /// alignments only decrease.
        ///
        /// This mirrors the private `sizes` table inside `std.MultiArrayList`. It is not
        /// trusted on faith: `writeCompactedColumns` asserts every derived column offset
        /// against the pointers the real `slice()` hands back, so a change in std's
        /// layout fails loudly instead of writing a silently transposed blob.
        const column_order: [std.meta.fields(T).len]usize = blk: {
            const fields = std.meta.fields(T);
            var order: [fields.len]usize = undefined;
            for (&order, 0..) |*slot, i| slot.* = i;
            // Insertion sort: stable, so equal alignments keep declaration order.
            var i: usize = 1;
            while (i < order.len) : (i += 1) {
                var j = i;
                while (j > 0 and fieldAlignment(order[j - 1]) < fieldAlignment(order[j])) : (j -= 1) {
                    const tmp = order[j - 1];
                    order[j - 1] = order[j];
                    order[j] = tmp;
                }
            }
            break :blk order;
        };

        fn fieldAlignment(comptime field_index: usize) comptime_int {
            const info = std.meta.fields(T)[field_index];
            return info.alignment orelse @alignOf(info.type);
        }

        /// Append this list's live rows to `writer` as `std.MultiArrayList`'s own column
        /// layout at `capacity == len`, and return the byte offset the columns start at.
        ///
        /// Only live rows are written: spare capacity never reaches the output, so the
        /// serialized bytes and their length are a function of the list's contents alone
        /// rather than of the allocation history that produced its capacity.
        ///
        /// The input is not mutated. A column whose element bytes are already fully
        /// defined is gathered straight from the list with no copy; only a column that
        /// needs padding scrubbed is copied into writer-owned memory first. Which case
        /// applies is decided at compile time, and an element shape whose undefined bytes
        /// nothing could scrub is rejected there too.
        fn writeCompactedColumns(
            list: *const Self,
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!usize {
            comptime {
                // No padding is ever needed between columns, at any length: a column's
                // start is `len * columnBytesBefore`, and `columnBytesBefore` is a sum of
                // element sizes that are each a multiple of an alignment at least as
                // large as this column's, so it is a multiple of this column's alignment.
                // The block itself is padded to the largest field alignment below.
                var block_align: usize = 1;
                for (column_order) |field_index| {
                    if (@sizeOf(std.meta.fields(T)[field_index].type) == 0) continue;
                    const field_align = fieldAlignment(field_index);
                    if (columnBytesBefore(field_index) % field_align != 0) {
                        @compileError("SafeMultiList(" ++ @typeName(T) ++ ") column '" ++
                            std.meta.fields(T)[field_index].name ++
                            "' would need alignment padding in the compacted layout");
                    }
                    if (field_align > block_align) block_align = field_align;
                }
                if (block_align > @alignOf(T)) {
                    @compileError("SafeMultiList(" ++ @typeName(T) ++
                        ") has a field alignment larger than the element alignment the reader aligns to");
                }
            }
            try writer.padToAlignment(allocator, @alignOf(T));
            const data_offset = writer.total_bytes;
            const live_rows = list.items.len;
            if (live_rows == 0) return data_offset;

            const slice = list.items.slice();
            inline for (column_order) |field_index| {
                const FieldType = std.meta.fields(T)[field_index].type;
                if (@sizeOf(FieldType) > 0) {
                    // `column_order` must agree with the layout std actually produced:
                    // this column starts `capacity * columnBytesBefore` into the source
                    // allocation...
                    std.debug.assert(@intFromPtr(slice.ptrs[field_index]) - @intFromPtr(list.items.bytes) ==
                        list.items.capacity * columnBytesBefore(field_index));
                    const column: [*]const FieldType = @ptrCast(@alignCast(slice.ptrs[field_index]));
                    // An integer offset keeps a first column that starts at byte zero
                    // representable, and lets the assertions below name where the column
                    // actually landed.
                    const column_offset = try writer.appendSlicePodZeroedOffset(allocator, column[0..live_rows]);
                    // ...and `len * columnBytesBefore` into the compacted output, with no
                    // alignment padding inserted between columns.
                    std.debug.assert(column_offset == data_offset + live_rows * columnBytesBefore(field_index));
                    std.debug.assert(writer.total_bytes - column_offset == live_rows * @sizeOf(FieldType));
                }
            }
            return data_offset;
        }

        /// Sum of the element sizes of the columns laid out before `field_index`. The
        /// assertion above uses it to convert a capacity-strided source offset into the
        /// len-strided offset the compacted output uses.
        fn columnBytesBefore(comptime field_index: usize) usize {
            return comptime blk: {
                var total: usize = 0;
                for (column_order) |candidate| {
                    if (candidate == field_index) break :blk total;
                    total += @sizeOf(std.meta.fields(T)[candidate].type);
                }
                unreachable;
            };
        }

        /// Index of an item in the list.
        pub const Idx = enum(u32) { first = 0, _ };

        /// A non-type-safe range view. Resolve fields from the backing list on
        /// demand so this value never carries MultiArrayList's cached pointer
        /// array across a target ABI boundary.
        pub const Slice = struct {
            list: *const Self,
            start: usize,
            len: usize,

            pub inline fn items(self: Slice, comptime field_name: Field) []@FieldType(T, @tagName(field_name)) {
                const all_items = self.list.field(field_name);
                return all_items[self.start..][0..self.len];
            }

            pub inline fn get(self: Slice, index: usize) T {
                std.debug.assert(index < self.len);
                return @call(.always_inline, std.MultiArrayList(T).get, .{ self.list.items, self.start + index });
            }
        };

        /// A type-safe slice of the list.
        pub const Range = SafeRange(Idx);

        /// One of the comptime fields in the list's wrapped type.
        pub const Field = std.MultiArrayList(T).Field;

        /// A slice of all values for a specific field of the wrapped type.
        pub fn field(self: *const SafeMultiList(T), comptime field_name: Field) []@FieldType(T, @tagName(field_name)) {
            return self.items.items(field_name);
        }

        /// The value for a specific field at a specific index in the list.
        pub fn fieldItem(self: *const SafeMultiList(T), comptime field_name: Field, idx: Idx) @FieldType(T, @tagName(field_name)) {
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

        /// Clone this multilist into fresh owned memory.
        pub fn clone(self: *const SafeMultiList(T), gpa: Allocator) Allocator.Error!SafeMultiList(T) {
            if (self.items.len == 0) {
                return SafeMultiList(T){ .items = .{} };
            }

            const MultiArrayListType = std.MultiArrayList(T);
            const total_bytes = MultiArrayListType.capacityInBytes(self.items.capacity);
            const fresh_bytes = try gpa.alignedAlloc(u8, .of(T), total_bytes);
            @memcpy(fresh_bytes[0..total_bytes], self.items.bytes[0..total_bytes]);

            return SafeMultiList(T){
                .items = .{
                    .bytes = @ptrCast(fresh_bytes.ptr),
                    .len = self.items.len,
                    .capacity = self.items.capacity,
                },
            };
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

        /// Add a new item to the end of this list assuming capacity is sufficient to hold an additional item.
        pub fn appendAssumeCapacity(self: *SafeMultiList(T), item: T) Idx {
            const length = self.len();
            self.items.appendAssumeCapacity(item);

            return @enumFromInt(@as(u32, @intCast(length)));
        }

        pub fn appendSlice(self: *SafeMultiList(T), gpa: Allocator, elems: []const T) std.mem.Allocator.Error!Range {
            if (elems.len == 0) {
                return .{ .start = .first, .count = 0 };
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
        pub inline fn sliceRange(self: *const SafeMultiList(T), range: Range) Slice {
            if (range.count == 0) return .{ .list = self, .start = 0, .len = 0 };

            const start: usize = @intFromEnum(range.start);
            const end: usize = start + range.count;

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.items.len);
            return .{ .list = self, .start = start, .len = end - start };
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

        /// An iterator over all the indices in this list.
        pub const Iterator = struct {
            array: *const SafeMultiList(T),
            len: u32,
            current: Idx,

            pub fn next(self: *Iterator) ?T {
                const cur_idx = self.current;
                const cur_int = @intFromEnum(cur_idx);
                if (self.len == cur_int) {
                    return null;
                }
                self.current = @enumFromInt(cur_int + 1);
                return self.array.get(cur_idx);
            }

            pub fn count(self: *Iterator) u32 {
                return self.len - @intFromEnum(self.current);
            }
        };

        /// Iterate over the elements in a span
        pub fn iterRange(self: *const SafeMultiList(T), range: Range) Iterator {
            return Iterator{
                .array = self,
                .len = @intFromEnum(range.start) + range.count,
                .current = range.start,
            };
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

        /// Iterate over the indices of the elements in a span
        pub fn iterRangeIndices(self: *const SafeMultiList(T), range: Range) IndexIterator {
            std.debug.assert(@intFromEnum(range.start) + range.count <= self.len());
            return IndexIterator{
                .len = @intFromEnum(range.start) + range.count,
                .current = @intFromEnum(range.start),
            };
        }

        /// Iterator over all indices in this list.
        pub fn iterIndices(self: *const SafeMultiList(T)) IndexIterator {
            return IndexIterator{
                .len = self.len(),
                .current = 0,
            };
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
            const data_offset = try writeCompactedColumns(self, allocator, writer);

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

        /// Serialized representation of a SafeMultiList
        /// Uses extern struct to guarantee consistent field layout across optimization levels.
        pub const Serialized = extern struct {
            offset: i64,
            len: u64,
            capacity: u64,

            /// One relocatable base pointer (`offset`) fixed up per relocate, counted by
            /// `artifact_serialize.relocatablePointerCount`.
            pub const serialized_relocatable_pointers: usize = 1;

            /// The element type whose bytes are serialized (see `SafeList.Serialized`).
            pub const SerializedElement = T;

            /// L-10: reject an `(offset, capacity)` whose `capacityInBytes` extent (the
            /// region `serialize` writes) reaches outside the `backing_len`-byte buffer.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                if (self.len == 0) return;
                const span_bytes = std.MultiArrayList(T).capacityInBytes(@intCast(self.capacity));
                try validateRelocatedSpan(@alignOf(T), self.offset, span_bytes, backing_len);
            }

            /// Serialize a SafeMultiList into this Serialized struct, appending data to
            /// the writer. Shares `writeCompactedColumns` with the pointer-returning
            /// `SafeMultiList.serialize`, so both produce the same bytes for the same
            /// contents: live rows only, in MultiArrayList's own column order, at
            /// `capacity == len`.
            pub fn serialize(
                self: *Serialized,
                safe_multi_list: *const SafeMultiList(T),
                allocator: Allocator,
                writer: *CompactWriter,
            ) Allocator.Error!void {
                const data_offset = try writeCompactedColumns(safe_multi_list, allocator, writer);

                self.offset = @intCast(data_offset);
                self.len = safe_multi_list.items.len;
                self.capacity = safe_multi_list.items.len;
            }

            /// Deserialize into a SafeMultiList value (Option F: no in-place modification).
            /// The base parameter is the base address of the serialized buffer in memory.
            /// WARNING: The returned SafeMultiList points into the cache buffer and CANNOT be grown.
            pub fn deserializeInto(self: *const Serialized, base: usize) SafeMultiList(T) {
                // Handle empty list case
                if (self.len == 0) {
                    return SafeMultiList(T){ .items = .{} };
                }

                // We need to reconstruct the MultiArrayList from the serialized field arrays
                // MultiArrayList stores fields separately by type, and we serialized them in field order
                const current_ptr = @as([*]u8, @ptrFromInt(base +% @as(usize, @intCast(self.offset))));

                // Allocate aligned memory for the MultiArrayList bytes
                const bytes_ptr = @as([*]align(@alignOf(T)) u8, @ptrCast(@alignCast(current_ptr)));

                return SafeMultiList(T){
                    .items = .{
                        .bytes = bytes_ptr,
                        .len = @as(usize, @intCast(self.len)),
                        .capacity = @as(usize, @intCast(self.capacity)),
                    },
                };
            }

            /// Deserialize into a SafeMultiList value with fresh memory allocation.
            /// The returned SafeMultiList owns its memory and can be safely grown/mutated.
            pub fn deserializeWithCopy(self: *const Serialized, base: usize, gpa: Allocator) Allocator.Error!SafeMultiList(T) {
                // Handle empty list case
                if (self.len == 0) {
                    return SafeMultiList(T){ .items = .{} };
                }

                // Get source bytes from cache buffer
                const src_ptr = @as([*]const u8, @ptrFromInt(base +% @as(usize, @intCast(self.offset))));
                const item_capacity: usize = @intCast(self.capacity);
                const item_len: usize = @intCast(self.len);

                // Calculate total bytes using MultiArrayList's SoA layout
                // (sum of field sizes * capacity, not @sizeOf(T) * capacity)
                // IMPORTANT: We must copy the full capacity bytes because SoA layout
                // stores each field array contiguously. capacityInBytes(len) would
                // copy the wrong bytes due to the field array offsets.
                const MultiArrayListType = std.MultiArrayList(T);
                const total_bytes = MultiArrayListType.capacityInBytes(item_capacity);

                // Allocate fresh memory and copy full capacity to preserve SoA layout
                const fresh_bytes = try gpa.alignedAlloc(u8, .of(T), total_bytes);
                @memcpy(fresh_bytes[0..total_bytes], src_ptr[0..total_bytes]);

                return SafeMultiList(T){
                    .items = .{
                        .bytes = @ptrCast(fresh_bytes.ptr),
                        .len = item_len,
                        .capacity = item_capacity,
                    },
                };
            }
        };
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
    const added_range = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
        .{ .num = 500, .char = 'e' },
    });
    try testing.expectEqual(@as(usize, 5), added_range.count);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(added_range.start));

    // Create an empty range at the end (start=5, end=5 for a list of length 5)
    const empty_range = StructMultiList.Range{ .start = @enumFromInt(5), .count = 0 };
    const empty_slice = multilist.sliceRange(empty_range);

    // The slice should be empty
    const num_slice = empty_slice.items(.num);
    try testing.expectEqual(0, num_slice.len);

    const char_slice = empty_slice.items(.char);
    try testing.expectEqual(0, char_slice.len);
}

test "SafeList edge cases serialization" {
    const gpa = testing.allocator;

    // Test that empty SafeLists serialize and deserialize correctly
    {
        var empty_list = SafeList(u32){};
        defer empty_list.deinit(gpa);

        var writer = CompactWriter.init();
        defer writer.deinit(gpa);

        const serialized = try writer.appendAlloc(gpa, SafeList(u32).Serialized);
        try serialized.serialize(&empty_list, gpa, &writer);

        const buffer = try gpa.alloc(u8, writer.total_bytes);
        defer gpa.free(buffer);
        const written = try writer.writeToBuffer(buffer);
        try testing.expectEqual(buffer.len, written.len);

        const serialized_ptr = @as(*SafeList(u32).Serialized, @ptrCast(@alignCast(buffer.ptr)));
        const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

        try testing.expectEqual(@as(usize, 0), deserialized.len());
    }

    // Test mixed type serialization in a more realistic container struct
    {
        const Container = struct {
            list_u32: SafeList(u32),
            list_u8: SafeList(u8),

            const Self = @This();

            pub const Serialized = extern struct {
                list_u32: SafeList(u32).Serialized,
                list_u8: SafeList(u8).Serialized,

                pub fn serialize(self: *Serialized, container: *const Self, allocator: std.mem.Allocator, writer: *CompactWriter) Allocator.Error!void {
                    try self.list_u32.serialize(&container.list_u32, allocator, writer);
                    try self.list_u8.serialize(&container.list_u8, allocator, writer);
                }

                pub fn deserializeInto(self: *const Serialized, base: usize) Self {
                    return Self{
                        .list_u32 = self.list_u32.deserializeInto(base),
                        .list_u8 = self.list_u8.deserializeInto(base),
                    };
                }
            };
        };

        var container = Container{
            .list_u32 = SafeList(u32){},
            .list_u8 = SafeList(u8){},
        };
        defer container.list_u32.deinit(gpa);
        defer container.list_u8.deinit(gpa);

        const container_idx = try container.list_u8.append(gpa, 123);
        try testing.expectEqual(@as(usize, 0), @intFromEnum(container_idx));

        var writer = CompactWriter.init();
        defer writer.deinit(gpa);

        const serialized = try writer.appendAlloc(gpa, Container.Serialized);
        try serialized.serialize(&container, gpa, &writer);

        const buffer = try gpa.alloc(u8, writer.total_bytes);
        defer gpa.free(buffer);
        const written = try writer.writeToBuffer(buffer);
        try testing.expectEqual(buffer.len, written.len);

        const serialized_ptr: *const Container.Serialized = @ptrCast(@alignCast(buffer.ptr));
        const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

        try testing.expectEqual(@as(usize, 0), deserialized.list_u32.len());
        try testing.expectEqual(@as(usize, 1), deserialized.list_u8.len());
        try testing.expectEqual(@as(u8, 123), deserialized.list_u8.get(.first).*);
    }
}

test "SafeList CompactWriter complete roundtrip example" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    // Step 1: Create original data
    var original = try SafeList(u32).initCapacity(gpa, 4);
    defer original.deinit(gpa);

    const orig_idx0 = try original.append(gpa, 100);
    const orig_idx1 = try original.append(gpa, 200);
    const orig_idx2 = try original.append(gpa, 300);
    const orig_idx3 = try original.append(gpa, 400);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(orig_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(orig_idx1));
    try testing.expectEqual(@as(usize, 2), @intFromEnum(orig_idx2));
    try testing.expectEqual(@as(usize, 3), @intFromEnum(orig_idx3));

    // Step 2: Create temp file and CompactWriter
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "example.dat", .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    // Step 3: Serialize - this writes data first, then the SafeList.Serialized struct
    const serialized = try writer.appendAlloc(gpa, SafeList(u32).Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Verify the offset is correct - it should be the size of the Serialized struct
    try testing.expectEqual(@sizeOf(SafeList(u32).Serialized), serialized.offset);

    // Step 4: Write to file using vectored I/O
    try writer.writeGather(file, io);

    // Step 5: Read file into 16-byte aligned buffer
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(u32)), @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Step 6: Cast buffer to SafeList.Serialized - the struct is at the beginning
    const serialized_ptr = @as(*SafeList(u32).Serialized, @ptrCast(@alignCast(buffer.ptr)));

    // Step 7: Deserialize - convert offset to pointer
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Step 8: Verify data is accessible and correct
    const Idx = SafeList(u32).Idx;
    try testing.expectEqual(@as(usize, 4), deserialized.len());
    try testing.expectEqual(@as(u32, 100), deserialized.get(.first).*);
    try testing.expectEqual(@as(u32, 200), deserialized.get(@as(Idx, @enumFromInt(1))).*);
    try testing.expectEqual(@as(u32, 300), deserialized.get(@as(Idx, @enumFromInt(2))).*);
    try testing.expectEqual(@as(u32, 400), deserialized.get(@as(Idx, @enumFromInt(3))).*);
}

test "SafeList CompactWriter multiple lists with different alignments" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    // Create multiple SafeLists with different element types and alignments

    // 1. SafeList(u8) - 1 byte alignment
    var list_u8 = try SafeList(u8).initCapacity(gpa, 3);
    defer list_u8.deinit(gpa);
    const u8_idx0 = try list_u8.append(gpa, 10);
    const u8_idx1 = try list_u8.append(gpa, 20);
    const u8_idx2 = try list_u8.append(gpa, 30);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(u8_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(u8_idx1));
    try testing.expectEqual(@as(usize, 2), @intFromEnum(u8_idx2));

    // 2. SafeList(u16) - 2 byte alignment
    var list_u16 = try SafeList(u16).initCapacity(gpa, 2);
    defer list_u16.deinit(gpa);
    const u16_idx0 = try list_u16.append(gpa, 1000);
    const u16_idx1 = try list_u16.append(gpa, 2000);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(u16_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(u16_idx1));

    // 3. SafeList(u32) - 4 byte alignment
    var list_u32 = try SafeList(u32).initCapacity(gpa, 4);
    defer list_u32.deinit(gpa);
    const u32_idx0 = try list_u32.append(gpa, 100_000);
    const u32_idx1 = try list_u32.append(gpa, 200_000);
    const u32_idx2 = try list_u32.append(gpa, 300_000);
    const u32_idx3 = try list_u32.append(gpa, 400_000);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(u32_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(u32_idx1));
    try testing.expectEqual(@as(usize, 2), @intFromEnum(u32_idx2));
    try testing.expectEqual(@as(usize, 3), @intFromEnum(u32_idx3));

    // 4. SafeList(u64) - 8 byte alignment
    var list_u64 = try SafeList(u64).initCapacity(gpa, 2);
    defer list_u64.deinit(gpa);
    const u64_idx0 = try list_u64.append(gpa, 10_000_000_000);
    const u64_idx1 = try list_u64.append(gpa, 20_000_000_000);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(u64_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(u64_idx1));

    // 5. SafeList with a struct type
    const AlignedStruct = struct {
        x: u32,
        y: u64,
        z: u8,
    };
    var list_struct = try SafeList(AlignedStruct).initCapacity(gpa, 2);
    defer list_struct.deinit(gpa);
    const struct_idx0 = try list_struct.append(gpa, .{ .x = 42, .y = 1337, .z = 255 });
    const struct_idx1 = try list_struct.append(gpa, .{ .x = 99, .y = 9999, .z = 128 });
    try testing.expectEqual(@as(usize, 0), @intFromEnum(struct_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(struct_idx1));

    // Create temp file and CompactWriter
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "multi_list.dat", .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter{
        .iovecs = .empty,
        .total_bytes = 0,
        .allocated_memory = .empty,
    };
    defer writer.deinit(gpa);

    // Serialize all lists
    const serialized_u8 = try writer.appendAlloc(gpa, SafeList(u8).Serialized);
    try serialized_u8.serialize(&list_u8, gpa, &writer);

    const serialized_u16 = try writer.appendAlloc(gpa, SafeList(u16).Serialized);
    try serialized_u16.serialize(&list_u16, gpa, &writer);

    const serialized_u32 = try writer.appendAlloc(gpa, SafeList(u32).Serialized);
    try serialized_u32.serialize(&list_u32, gpa, &writer);

    const serialized_u64 = try writer.appendAlloc(gpa, SafeList(u64).Serialized);
    try serialized_u64.serialize(&list_u64, gpa, &writer);

    const serialized_struct = try writer.appendAlloc(gpa, SafeList(AlignedStruct).Serialized);
    try serialized_struct.serialize(&list_struct, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back into aligned buffer
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize all lists
    const base_addr = @intFromPtr(buffer.ptr);

    // Calculate offsets based on the serialization order and alignment requirements
    var offset: usize = 0;

    // 1. Deserialize u8 list
    // SafeList(u8).Serialized has 8-byte alignment due to u64 fields
    offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(u8).Serialized));
    const s_u8 = @as(*SafeList(u8).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
    const deser_u8 = s_u8.deserializeInto(base_addr);
    offset += @sizeOf(SafeList(u8).Serialized);
    // Skip the u8 data (3 bytes)
    offset = std.mem.alignForward(usize, offset, @alignOf(u8));
    offset += 3 * @sizeOf(u8);

    const U8Idx = SafeList(u8).Idx;
    try testing.expectEqual(@as(usize, 3), deser_u8.len());
    try testing.expectEqual(@as(u8, 10), deser_u8.get(.first).*);
    try testing.expectEqual(@as(u8, 20), deser_u8.get(@as(U8Idx, @enumFromInt(1))).*);
    try testing.expectEqual(@as(u8, 30), deser_u8.get(@as(U8Idx, @enumFromInt(2))).*);

    // 2. Deserialize u16 list
    offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(u16).Serialized));
    const s_u16 = @as(*SafeList(u16).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
    const deser_u16 = s_u16.deserializeInto(base_addr);
    offset += @sizeOf(SafeList(u16).Serialized);
    // Skip the u16 data (2 items)
    offset = std.mem.alignForward(usize, offset, @alignOf(u16));
    offset += 2 * @sizeOf(u16);

    const U16Idx = SafeList(u16).Idx;
    try testing.expectEqual(@as(usize, 2), deser_u16.len());
    try testing.expectEqual(@as(u16, 1000), deser_u16.get(.first).*);
    try testing.expectEqual(@as(u16, 2000), deser_u16.get(@as(U16Idx, @enumFromInt(1))).*);

    // 3. Deserialize u32 list
    offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(u32).Serialized));
    const s_u32 = @as(*SafeList(u32).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
    const deser_u32 = s_u32.deserializeInto(base_addr);
    offset += @sizeOf(SafeList(u32).Serialized);
    // Skip the u32 data (4 items)
    offset = std.mem.alignForward(usize, offset, @alignOf(u32));
    offset += 4 * @sizeOf(u32);

    const U32Idx = SafeList(u32).Idx;
    try testing.expectEqual(@as(usize, 4), deser_u32.len());
    try testing.expectEqual(@as(u32, 100_000), deser_u32.get(.first).*);
    try testing.expectEqual(@as(u32, 200_000), deser_u32.get(@as(U32Idx, @enumFromInt(1))).*);
    try testing.expectEqual(@as(u32, 300_000), deser_u32.get(@as(U32Idx, @enumFromInt(2))).*);
    try testing.expectEqual(@as(u32, 400_000), deser_u32.get(@as(U32Idx, @enumFromInt(3))).*);

    // 4. Deserialize u64 list
    offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(u64).Serialized));
    const s_u64 = @as(*SafeList(u64).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
    const deser_u64 = s_u64.deserializeInto(base_addr);
    offset += @sizeOf(SafeList(u64).Serialized);
    // Skip the u64 data (2 items)
    offset = std.mem.alignForward(usize, offset, @alignOf(u64));
    offset += 2 * @sizeOf(u64);

    const U64Idx = SafeList(u64).Idx;
    try testing.expectEqual(@as(usize, 2), deser_u64.len());
    try testing.expectEqual(@as(u64, 10_000_000_000), deser_u64.get(.first).*);
    try testing.expectEqual(@as(u64, 20_000_000_000), deser_u64.get(@as(U64Idx, @enumFromInt(1))).*);

    // 5. Deserialize struct list
    offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(AlignedStruct).Serialized));
    const s_struct = @as(*SafeList(AlignedStruct).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
    const deser_struct = s_struct.deserializeInto(base_addr);

    const StructIdx = SafeList(AlignedStruct).Idx;
    try testing.expectEqual(@as(usize, 2), deser_struct.len());
    const item0 = deser_struct.get(.first);
    try testing.expectEqual(@as(u32, 42), item0.x);
    try testing.expectEqual(@as(u64, 1337), item0.y);
    try testing.expectEqual(@as(u8, 255), item0.z);

    const item1 = deser_struct.get(@as(StructIdx, @enumFromInt(1)));
    try testing.expectEqual(@as(u32, 99), item1.x);
    try testing.expectEqual(@as(u64, 9999), item1.y);
    try testing.expectEqual(@as(u8, 128), item1.z);
}

test "SafeList CompactWriter brute-force alignment verification" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    // Test all combinations of slice lengths from 0 to 8 for different types
    // This ensures our alignment padding works correctly for all cases

    var tmp_dir = testing.tmpDir(.{});
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

            const file = try tmp_dir.dir.createFile(io, filename, .{ .read = true });
            defer file.close(io);

            // Create lists with the specific length
            var list1 = SafeList(T){};
            defer list1.deinit(gpa);

            var i: usize = 0;
            while (i < length) : (i += 1) {
                const idx = try list1.append(gpa, @as(T, @intCast(i + 1)));
                try testing.expectEqual(i, @intFromEnum(idx));
            }

            // Also create a second list with different data
            var list2 = SafeList(T){};
            defer list2.deinit(gpa);

            i = 0;
            while (i < length) : (i += 1) {
                // Use smaller values to avoid overflow for smaller integer types
                const multiplier: T = if (T == u8) 10 else if (T == u16) 1000 else 100000;
                const idx = try list2.append(gpa, @as(T, @intCast(i + 1)) * multiplier);
                try testing.expectEqual(i, @intFromEnum(idx));
            }

            // Create a u8 list to add between them (to test alignment)
            var list_u8 = SafeList(u8){};
            defer list_u8.deinit(gpa);
            const list_u8_idx = try list_u8.append(gpa, 42);
            try testing.expectEqual(@as(usize, 0), @intFromEnum(list_u8_idx));

            // Serialize everything
            var writer = CompactWriter{
                .iovecs = .empty,
                .total_bytes = 0,
                .allocated_memory = .empty,
            };
            defer writer.deinit(gpa);

            // Serialize in pattern: list1, u8 list, list2
            // This tests alignment padding between different types
            const serialized1 = try writer.appendAlloc(gpa, SafeList(T).Serialized);
            try serialized1.serialize(&list1, gpa, &writer);

            const serialized_u8 = try writer.appendAlloc(gpa, SafeList(u8).Serialized);
            try serialized_u8.serialize(&list_u8, gpa, &writer);

            const serialized2 = try writer.appendAlloc(gpa, SafeList(T).Serialized);
            try serialized2.serialize(&list2, gpa, &writer);

            // Write to file
            try writer.writeGather(file, io);

            // Read back
            const file_size = writer.total_bytes;
            const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
            defer gpa.free(buffer);

            _ = try file.readPositionalAll(io, buffer, 0);

            // Deserialize and verify
            const base = @intFromPtr(buffer.ptr);

            // Calculate offsets with proper alignment
            var offset: usize = 0;

            // First list
            offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(T).Serialized));
            const s1 = @as(*SafeList(T).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
            const d1 = s1.deserializeInto(base);
            offset += @sizeOf(SafeList(T).Serialized);
            offset = std.mem.alignForward(usize, offset, @alignOf(T));
            offset += length * @sizeOf(T);

            try testing.expectEqual(length, d1.len());
            i = 0;
            while (i < length) : (i += 1) {
                const expected = @as(T, @intCast(i + 1));
                const actual = d1.get(@enumFromInt(i)).*;
                try testing.expectEqual(expected, actual);
            }

            // u8 list
            offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(u8).Serialized));
            const s_u8 = @as(*SafeList(u8).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
            const d_u8 = s_u8.deserializeInto(base);
            offset += @sizeOf(SafeList(u8).Serialized);
            offset = std.mem.alignForward(usize, offset, @alignOf(u8));
            offset += 1; // 1 u8 element

            try testing.expectEqual(@as(usize, 1), d_u8.len());
            try testing.expectEqual(@as(u8, 42), d_u8.get(.first).*);

            // Second list
            offset = std.mem.alignForward(usize, offset, @alignOf(SafeList(T).Serialized));
            const s2 = @as(*SafeList(T).Serialized, @ptrCast(@alignCast(buffer.ptr + offset)));
            const d2 = s2.deserializeInto(base);

            try testing.expectEqual(length, d2.len());
            i = 0;
            while (i < length) : (i += 1) {
                const multiplier: T = if (T == u8) 10 else if (T == u16) 1000 else 100000;
                const expected = @as(T, @intCast(i + 1)) * multiplier;
                const actual = d2.get(@enumFromInt(i)).*;
                try testing.expectEqual(expected, actual);
            }
        }
    }
}

test "SafeMultiList CompactWriter roundtrip with file" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    // Create a SafeMultiList with test data
    const TestStruct = struct {
        id: u32,
        value: u64,
        flag: bool,
        data: u8,
    };

    var original = try SafeMultiList(TestStruct).initCapacity(gpa, 4);
    defer original.deinit(gpa);

    const orig_idx0 = try original.append(gpa, .{ .id = 100, .value = 1000, .flag = true, .data = 10 });
    const orig_idx1 = try original.append(gpa, .{ .id = 200, .value = 2000, .flag = false, .data = 20 });
    const orig_idx2 = try original.append(gpa, .{ .id = 300, .value = 3000, .flag = true, .data = 30 });
    const orig_idx3 = try original.append(gpa, .{ .id = 400, .value = 4000, .flag = false, .data = 40 });
    try testing.expectEqual(@as(usize, 0), @intFromEnum(orig_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(orig_idx1));
    try testing.expectEqual(@as(usize, 2), @intFromEnum(orig_idx2));
    try testing.expectEqual(@as(usize, 3), @intFromEnum(orig_idx3));

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "test_multi.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, SafeMultiList(TestStruct).Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back into aligned buffer
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // The memory layout from CompactWriter is:
    // 1. SafeMultiList.Serialized struct (appended first by appendAlloc)
    // 2. Field data (appended by serialize method)
    // So the Serialized struct is at the beginning
    const serialized_ptr = @as(*SafeMultiList(TestStruct).Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify the data
    const Idx = SafeMultiList(TestStruct).Idx;
    try testing.expectEqual(@as(usize, 4), deserialized.len());

    // Verify all the data
    try testing.expectEqual(@as(u32, 100), deserialized.get(.first).id);
    try testing.expectEqual(@as(u64, 1000), deserialized.get(.first).value);
    try testing.expectEqual(true, deserialized.get(.first).flag);
    try testing.expectEqual(@as(u8, 10), deserialized.get(.first).data);

    const second_idx: Idx = @enumFromInt(1);
    try testing.expectEqual(@as(u32, 200), deserialized.get(second_idx).id);
    try testing.expectEqual(@as(u64, 2000), deserialized.get(second_idx).value);
    try testing.expectEqual(false, deserialized.get(second_idx).flag);
    try testing.expectEqual(@as(u8, 20), deserialized.get(second_idx).data);

    const third_idx: Idx = @enumFromInt(2);
    try testing.expectEqual(@as(u32, 300), deserialized.get(third_idx).id);
    try testing.expectEqual(@as(u64, 3000), deserialized.get(third_idx).value);
    try testing.expectEqual(true, deserialized.get(third_idx).flag);
    try testing.expectEqual(@as(u8, 30), deserialized.get(third_idx).data);

    const fourth_idx: Idx = @enumFromInt(3);
    try testing.expectEqual(@as(u32, 400), deserialized.get(fourth_idx).id);
    try testing.expectEqual(@as(u64, 4000), deserialized.get(fourth_idx).value);
    try testing.expectEqual(false, deserialized.get(fourth_idx).flag);
    try testing.expectEqual(@as(u8, 40), deserialized.get(fourth_idx).data);
}

test "SafeMultiList CompactWriter brute-force alignment verification" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    var tmp_dir = testing.tmpDir(.{});
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

        const file = try tmp_dir.dir.createFile(io, filename, .{ .read = true });
        defer file.close(io);

        // Create list with specific length but larger capacity to test compaction
        var list = try SafeMultiList(TestType).initCapacity(gpa, length + 5);
        defer list.deinit(gpa);

        var i: usize = 0;
        while (i < length) : (i += 1) {
            const idx = try list.append(gpa, .{
                .a = @as(u8, @intCast(i)),
                .b = @as(u32, @intCast(i * 100)),
                .c = @as(u64, @intCast(i * 1000)),
            });
            try testing.expectEqual(i, @intFromEnum(idx));
        }

        // Verify we have extra capacity that shouldn't be serialized
        try testing.expect(list.items.capacity >= length + 5);

        // Add another list to test alignment between lists
        var list2 = SafeMultiList(TestType){};
        defer list2.deinit(gpa);
        if (length > 0) {
            const list2_idx = try list2.append(gpa, .{ .a = 255, .b = 999999, .c = 888888888 });
            try testing.expectEqual(@as(usize, 0), @intFromEnum(list2_idx));
        }

        // Serialize
        var writer = CompactWriter.init();
        defer writer.deinit(gpa);

        const serialized1 = try writer.appendAlloc(gpa, SafeMultiList(TestType).Serialized);
        const offset1 = writer.total_bytes - @sizeOf(SafeMultiList(TestType).Serialized);
        try serialized1.serialize(&list, gpa, &writer);

        const serialized2 = try writer.appendAlloc(gpa, SafeMultiList(TestType).Serialized);
        const offset2 = writer.total_bytes - @sizeOf(SafeMultiList(TestType).Serialized);
        try serialized2.serialize(&list2, gpa, &writer);

        // Write to file
        try writer.writeGather(file, io);

        // Read back
        const file_size = writer.total_bytes;
        const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
        defer gpa.free(buffer);

        _ = try file.readPositionalAll(io, buffer, 0);

        const base = @intFromPtr(buffer.ptr);

        // Verify first list
        const d1_serialized = @as(*SafeMultiList(TestType).Serialized, @ptrCast(@alignCast(buffer.ptr + offset1)));
        const d1 = d1_serialized.deserializeInto(base);
        try testing.expectEqual(length, d1.len());

        i = 0;
        while (i < length) : (i += 1) {
            const item = d1.get(@enumFromInt(i));
            try testing.expectEqual(@as(u8, @intCast(i)), item.a);
            try testing.expectEqual(@as(u32, @intCast(i * 100)), item.b);
            try testing.expectEqual(@as(u64, @intCast(i * 1000)), item.c);
        }

        // Verify second list
        const d2_serialized = @as(*SafeMultiList(TestType).Serialized, @ptrCast(@alignCast(buffer.ptr + offset2)));
        const d2 = d2_serialized.deserializeInto(base);
        if (length > 0) {
            const d2_first_idx: SafeMultiList(TestType).Idx = .first;
            try testing.expectEqual(@as(usize, 1), d2.len());
            try testing.expectEqual(@as(u8, 255), d2.get(d2_first_idx).a);
            try testing.expectEqual(@as(u32, 999999), d2.get(d2_first_idx).b);
            try testing.expectEqual(@as(u64, 888888888), d2.get(d2_first_idx).c);
        } else {
            try testing.expectEqual(@as(usize, 0), d2.len());
        }
    }
}

test "SafeMultiList CompactWriter various field alignments and sizes" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    var tmp_dir = testing.tmpDir(.{});
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
                    const field_type_info = @typeInfo(field.type);
                    const value = if (field_type_info == .int)
                        @as(field.type, @intCast(@min(i * (fi + 1) + 1, std.math.maxInt(field.type))))
                    else if (field_type_info == .float)
                        @as(field.type, @floatFromInt(i * (fi + 1) + 1))
                    else if (field_type_info == .bool)
                        @as(field.type, (i + fi) % 2 == 0)
                    else
                        @compileError("Unsupported field type in TestType: " ++ @typeName(field.type));
                    @field(item, field.name) = value;
                }
                const idx = try list.append(gpa, item);
                try testing.expectEqual(i, @intFromEnum(idx));
            }

            // Serialize and deserialize
            const filename = try std.fmt.allocPrint(gpa, "align_test_{s}_{}.dat", .{ @typeName(TestType), len });
            defer gpa.free(filename);

            const file = try tmp_dir.dir.createFile(io, filename, .{ .read = true });
            defer file.close(io);

            var writer = CompactWriter.init();
            defer writer.deinit(gpa);

            const serialized = try writer.appendAlloc(gpa, SafeMultiList(TestType).Serialized);
            try serialized.serialize(&list, gpa, &writer);
            try writer.writeGather(file, io);

            // Read back
            const file_size = writer.total_bytes;
            const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
            defer gpa.free(buffer);

            _ = try file.readPositionalAll(io, buffer, 0);

            // Deserialize
            const serialized_ptr = @as(*SafeMultiList(TestType).Serialized, @ptrCast(@alignCast(buffer.ptr)));
            const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

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
        }
    }
}

test "SafeMultiList CompactWriter verify exact memory layout" {
    const gpa = testing.allocator;
    const io = std.testing.io;

    // Test that our serialization produces the exact memory layout that MultiArrayList
    // expects when it reads the blob back: field columns in descending-alignment order,
    // packed with no gaps, each holding exactly `len` elements.
    const TestStruct = struct {
        a: u8,
        b: u32,
        c: u16,
        d: u64,
    };

    // Test with various lengths to ensure layout is correct
    const test_lengths = [_]usize{ 1, 2, 3, 5, 8 };

    for (test_lengths) |len| {
        // Reserve more than needed, so a layout that wrote capacity-sized columns
        // would disagree with the expectation below.
        var original = try SafeMultiList(TestStruct).initCapacity(gpa, len + 10);
        defer original.deinit(gpa);

        var i: usize = 0;
        while (i < len) : (i += 1) {
            const idx = try original.append(gpa, .{
                .a = @as(u8, @intCast(i + 10)),
                .b = @as(u32, @intCast(i + 100)),
                .c = @as(u16, @intCast(i + 1000)),
                .d = @as(u64, @intCast(i + 10000)),
            });
            try testing.expectEqual(i, @intFromEnum(idx));
        }

        // The oracle is std's own layout: a MultiArrayList holding the same rows with
        // capacity equal to length has exactly the byte image the blob must contain.
        // Deriving it this way rather than restating the column-ordering rule keeps the
        // expectation independent of the writer's computation of that rule—and correct
        // on hosts where the field alignments, and so the column order, differ.
        var oracle = try std.MultiArrayList(TestStruct).initCapacity(gpa, len);
        defer oracle.deinit(gpa);
        try testing.expectEqual(len, oracle.capacity);
        i = 0;
        while (i < len) : (i += 1) oracle.appendAssumeCapacity(original.get(@enumFromInt(@as(u32, @intCast(i)))));
        const expected_bytes = oracle.bytes[0..std.MultiArrayList(TestStruct).capacityInBytes(len)];

        // Now serialize using our implementation
        var tmp_dir = testing.tmpDir(.{});
        defer tmp_dir.cleanup();

        const file = try tmp_dir.dir.createFile(io, "layout_test.dat", .{ .read = true });
        defer file.close(io);

        var writer = CompactWriter.init();
        defer writer.deinit(gpa);

        const serialized = try writer.appendAlloc(gpa, SafeMultiList(TestStruct).Serialized);
        try serialized.serialize(&original, gpa, &writer);
        try writer.writeGather(file, io);

        // Read back
        const file_size = writer.total_bytes;
        const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
        defer gpa.free(buffer);

        _ = try file.readPositionalAll(io, buffer, 0);

        const serialized_ptr = @as(*SafeMultiList(TestStruct).Serialized, @ptrCast(@alignCast(buffer.ptr)));
        // Only the live rows are written, so the stored capacity is the length and the
        // blob ends right after the last column.
        try testing.expectEqual(@as(u64, len), serialized_ptr.len);
        try testing.expectEqual(@as(u64, len), serialized_ptr.capacity);
        const data_start: usize = @intCast(serialized_ptr.offset);
        try testing.expectEqual(file_size, data_start + expected_bytes.len);

        // Verify byte-for-byte equality
        try testing.expectEqualSlices(u8, expected_bytes, buffer[data_start..][0..expected_bytes.len]);

        // Also verify it deserializes correctly
        const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

        // Verify all data is accessible
        i = 0;
        while (i < len) : (i += 1) {
            const item = deserialized.get(@enumFromInt(@as(u32, @intCast(i))));
            try testing.expectEqual(@as(u8, @intCast(i + 10)), item.a);
            try testing.expectEqual(@as(u32, @intCast(i + 100)), item.b);
            try testing.expectEqual(@as(u16, @intCast(i + 1000)), item.c);
            try testing.expectEqual(@as(u64, @intCast(i + 10000)), item.d);
        }
    }
}

test "SafeMultiList CompactWriter empty with capacity" {
    const gpa = testing.allocator;
    const io = std.testing.io;

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

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(io, "empty_capacity.dat", .{ .read = true });
    defer file.close(io);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, SafeMultiList(TestStruct).Serialized);
    try serialized.serialize(&list, gpa, &writer);
    try writer.writeGather(file, io);

    // Read back
    const file_size = writer.total_bytes;
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const serialized_ptr = @as(*SafeMultiList(TestStruct).Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify it's still empty
    try testing.expectEqual(@as(usize, 0), deserialized.len());
    // Capacity should be 0 after compaction
    try testing.expectEqual(@as(usize, 0), deserialized.items.capacity);
}

test "SafeList deserialization with high address (issue 8728)" {
    // This test verifies that deserialization works correctly when the buffer
    // is located at a high memory address (above 0x8000_0000_0000_0000).
    // Previously, casting such addresses to i64 made them negative, causing
    // the @intCast to usize to fail with "integer does not fit in destination type".

    const gpa = testing.allocator;

    // Create a simple SafeList with some data
    var original = try SafeList(u64).initCapacity(gpa, 3);
    defer original.deinit(gpa);
    const orig_idx0 = try original.append(gpa, 100);
    const orig_idx1 = try original.append(gpa, 200);
    const orig_idx2 = try original.append(gpa, 300);
    try testing.expectEqual(@as(usize, 0), @intFromEnum(orig_idx0));
    try testing.expectEqual(@as(usize, 1), @intFromEnum(orig_idx1));
    try testing.expectEqual(@as(usize, 2), @intFromEnum(orig_idx2));

    // Serialize it
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, SafeList(u64).Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to a buffer
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer gpa.free(buffer);
    const written = try writer.writeToBuffer(buffer);
    try testing.expectEqual(buffer.len, written.len);

    // Get the serialized struct
    const serialized_ptr = @as(*SafeList(u64).Serialized, @ptrCast(@alignCast(buffer.ptr)));

    // Test with the actual buffer address (should work on any system)
    const actual_base = @intFromPtr(buffer.ptr);
    const deserialized1 = serialized_ptr.deserializeInto(actual_base);
    try testing.expectEqual(@as(usize, 3), deserialized1.len());

    // Now test the math that would occur with a high address.
    // If a buffer were at address 0x9000_0000_0000_0000, and the data offset within
    // the buffer is, say, 0x100, then the final address should be 0x9000_0000_0000_0100.
    //
    // With the old i64-based approach:
    // - base address 0x9000_0000_0000_0000 cast to i64 = negative number
    // - adding offset 0x100 still gives negative number
    // - casting negative to usize fails
    //
    // With the correct usize-based approach:
    // - base address 0x9000_0000_0000_0000 stays as usize
    // - adding u64 offset 0x100 gives 0x9000_0000_0000_0100
    // - this is a valid usize
    //
    // We can't actually allocate at such a high address in a test, but we can verify
    // the arithmetic works correctly by checking that high_base + offset produces
    // the expected result when both are treated as unsigned.
    // This test only runs on 64-bit platforms where usize can represent high addresses.
    if (@sizeOf(usize) == 8) {
        const high_base: usize = 0x9000_0000_0000_0000;
        const offset_in_buffer: u64 = 0x100;
        const expected_address: usize = high_base + offset_in_buffer;

        // This would have failed with the old i64 approach because:
        // @as(i64, @intCast(high_base)) would be negative
        // But with usize, this works correctly
        try testing.expectEqual(@as(usize, 0x9000_0000_0000_0100), expected_address);
    }
}

test "SafeMultiList(T) iterRange over middle span yields elements in order then null" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 6);
    defer multilist.deinit(gpa);

    _ = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
        .{ .num = 500, .char = 'e' },
        .{ .num = 600, .char = 'f' },
    });

    // Middle span: indices [2, 4) -> elements 'c' and 'd'.
    const range = StructMultiList.Range{ .start = @enumFromInt(2), .count = 2 };
    var iter = multilist.iterRange(range);

    const first = iter.next().?;
    try testing.expectEqual(@as(u32, 300), first.num);
    try testing.expectEqual(@as(u8, 'c'), first.char);

    const second = iter.next().?;
    try testing.expectEqual(@as(u32, 400), second.num);
    try testing.expectEqual(@as(u8, 'd'), second.char);

    // Exhausted, and stays null on repeated calls.
    try testing.expectEqual(@as(?Struct, null), iter.next());
    try testing.expectEqual(@as(?Struct, null), iter.next());
}

test "SafeMultiList(T) iterRange over empty range yields null and count 0" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 4);
    defer multilist.deinit(gpa);

    _ = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
    });

    // Empty range in the middle of the populated list.
    const range = StructMultiList.Range{ .start = @enumFromInt(2), .count = 0 };
    var iter = multilist.iterRange(range);

    try testing.expectEqual(@as(u32, 0), iter.count());
    try testing.expectEqual(@as(?Struct, null), iter.next());
    // Still null after exhaustion, and count stays 0.
    try testing.expectEqual(@as(?Struct, null), iter.next());
    try testing.expectEqual(@as(u32, 0), iter.count());
}

test "SafeMultiList(T) iterRange count decreases as next consumes" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 5);
    defer multilist.deinit(gpa);

    _ = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
        .{ .num = 500, .char = 'e' },
    });

    // Span [1, 4): 3 elements remaining up front.
    const range = StructMultiList.Range{ .start = @enumFromInt(1), .count = 3 };
    var iter = multilist.iterRange(range);

    try testing.expectEqual(@as(u32, 3), iter.count());
    _ = iter.next();
    try testing.expectEqual(@as(u32, 2), iter.count());
    _ = iter.next();
    try testing.expectEqual(@as(u32, 1), iter.count());
    _ = iter.next();
    try testing.expectEqual(@as(u32, 0), iter.count());
    // Draining past the end keeps count at 0.
    try testing.expectEqual(@as(?Struct, null), iter.next());
    try testing.expectEqual(@as(u32, 0), iter.count());
}

test "SafeMultiList(T) iterRangeIndices yields span indices matching get" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 6);
    defer multilist.deinit(gpa);

    _ = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
        .{ .num = 500, .char = 'e' },
        .{ .num = 600, .char = 'f' },
    });

    // Mid-list span [2, 5): indices 2, 3, 4.
    const range = StructMultiList.Range{ .start = @enumFromInt(2), .count = 3 };
    var idx_iter = multilist.iterRangeIndices(range);

    const idx_a = idx_iter.next().?;
    try testing.expectEqual(@as(u32, 2), @intFromEnum(idx_a));
    try testing.expectEqual(@as(u32, 300), multilist.get(idx_a).num);

    const idx_b = idx_iter.next().?;
    try testing.expectEqual(@as(u32, 3), @intFromEnum(idx_b));
    try testing.expectEqual(@as(u32, 400), multilist.get(idx_b).num);

    const idx_c = idx_iter.next().?;
    try testing.expectEqual(@as(u32, 4), @intFromEnum(idx_c));
    try testing.expectEqual(@as(u32, 500), multilist.get(idx_c).num);

    try testing.expectEqual(@as(?StructMultiList.Idx, null), idx_iter.next());
}

test "SafeMultiList(T) iterRange from index zero vs mid-list (len = start + count encoding)" {
    const gpa = testing.allocator;

    const Struct = struct { num: u32, char: u8 };
    const StructMultiList = SafeMultiList(Struct);

    var multilist = try StructMultiList.initCapacity(gpa, 5);
    defer multilist.deinit(gpa);

    const appended = try multilist.appendSlice(gpa, &[_]Struct{
        .{ .num = 100, .char = 'a' },
        .{ .num = 200, .char = 'b' },
        .{ .num = 300, .char = 'c' },
        .{ .num = 400, .char = 'd' },
        .{ .num = 500, .char = 'e' },
    });

    // Range starting at the appended slice's first index (0): len encodes as
    // start(0) + count(2) = 2.
    var iter_zero = multilist.iterRange(.{ .start = appended.start, .count = 2 });
    try testing.expectEqual(@as(u32, 2), iter_zero.count());
    try testing.expectEqual(@as(u32, 100), iter_zero.next().?.num);
    try testing.expectEqual(@as(u32, 200), iter_zero.next().?.num);
    try testing.expectEqual(@as(?Struct, null), iter_zero.next());

    // Range starting mid-list: len encodes as start(3) + count(2) = 5, so the
    // window is [3, 5) rather than [0, 2). This pins the `start + count`
    // encoding: the same count starting at a different offset must not read
    // from the front of the list.
    var iter_mid = multilist.iterRange(.{ .start = @enumFromInt(3), .count = 2 });
    try testing.expectEqual(@as(u32, 2), iter_mid.count());
    try testing.expectEqual(@as(u32, 400), iter_mid.next().?.num);
    try testing.expectEqual(@as(u32, 500), iter_mid.next().?.num);
    try testing.expectEqual(@as(?Struct, null), iter_mid.next());
}

/// Element whose declaration order (`small`, `big`, `mid`) deliberately differs from
/// MultiArrayList's column order (`big`, `mid`, `small`, by descending alignment), so
/// the serialization tests below fail if the columns are written in declaration order.
const MixedAlignElem = struct {
    small: u8,
    big: u64,
    mid: u16,
};

fn buildMixedAlign(gpa: Allocator, reserve: usize, count: u8) Allocator.Error!SafeMultiList(MixedAlignElem) {
    var list = SafeMultiList(MixedAlignElem){};
    errdefer list.deinit(gpa);
    if (reserve > 0) try list.ensureTotalCapacity(gpa, reserve);
    for (0..count) |i| {
        _ = try list.append(gpa, .{
            .small = @intCast(i + 1),
            .big = 0x1122334455660000 + @as(u64, i),
            .mid = @intCast(0x7700 + i),
        });
    }
    return list;
}

fn serializeMultiListToBuffer(
    gpa: Allocator,
    comptime T: type,
    list: *const SafeMultiList(T),
) (Allocator.Error || error{BufferTooSmall})![]align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8 {
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const serialized = try writer.appendAlloc(gpa, SafeMultiList(T).Serialized);
    try serialized.serialize(list, gpa, &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    errdefer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    return buffer;
}

test "SafeMultiList serialization depends on contents, not on allocation history" {
    // Spare capacity is a property of how a list was grown, not of what it holds. When
    // serialization wrote capacity-sized regions, two lists with identical contents but
    // different capacities produced different bytes AND different file sizes, so the
    // compiler's own baked output depended on its allocator's growth history.
    const gpa = testing.allocator;

    var grown = try buildMixedAlign(gpa, 0, 9); // amortized growth: capacity > len
    defer grown.deinit(gpa);
    var exact = try buildMixedAlign(gpa, 9, 9); // capacity == len
    defer exact.deinit(gpa);
    var oversized = try buildMixedAlign(gpa, 4096, 9); // capacity >> len
    defer oversized.deinit(gpa);

    try testing.expect(grown.items.capacity != oversized.items.capacity);

    const a = try serializeMultiListToBuffer(gpa, MixedAlignElem, &grown);
    defer gpa.free(a);
    const b = try serializeMultiListToBuffer(gpa, MixedAlignElem, &exact);
    defer gpa.free(b);
    const c = try serializeMultiListToBuffer(gpa, MixedAlignElem, &oversized);
    defer gpa.free(c);

    try testing.expectEqualSlices(u8, a, b);
    try testing.expectEqualSlices(u8, a, c);

    // And the written region is exactly the live rows: no spare capacity reaches the
    // output, so its size is a function of `len` alone.
    const header_bytes = @sizeOf(SafeMultiList(MixedAlignElem).Serialized);
    try testing.expectEqual(header_bytes + 9 * (8 + 2 + 1), a.len);
}

test "SafeMultiList serialization leaves a frozen source untouched" {
    // The serialize path may be handed a store it does not own (a cache-loaded module
    // re-serialized, or a shared read-only view). Scrubbing in place would corrupt a
    // shared store and fault on read-only memory, so the source must not be written.
    //
    // The element's padded field has to be a padded *column*: MultiArrayList splits a
    // struct into one array per field, so a struct of plain scalars has no padding
    // anywhere in the backing store and would only prove that spare capacity is left
    // alone. `Detail` is a nested padded struct, so the live rows of that column really
    // do contain padding bytes for an in-place scrub to touch.
    const gpa = testing.allocator;

    const Detail = struct { tag: u8, value: u64 }; // auto layout: inter-field gap
    const Choice = union(enum) { none: void, some: u64 }; // inactive/tail bytes
    const Row = struct { detail: Detail, choice: Choice, plain: u32 };
    comptime std.debug.assert(CompactWriter.needsPaddingZeroing(Detail));
    comptime std.debug.assert(CompactWriter.needsPaddingZeroing(Choice));

    var list = SafeMultiList(Row){};
    defer list.deinit(gpa);

    // Exactly as many rows as capacity, so there is no spare capacity at all: only a
    // scrub of live rows could change the backing bytes.
    const row_count = 6;
    try list.ensureTotalCapacity(gpa, row_count);
    const backing_bytes = std.MultiArrayList(Row).capacityInBytes(list.items.capacity);
    @memset(list.items.bytes[0..backing_bytes], 0xA7);
    for (0..row_count) |i| {
        _ = try list.append(gpa, .{
            .detail = .{ .tag = @intCast(i), .value = i * 1000 },
            .choice = if (i % 2 == 0) .{ .some = i } else .none,
            .plain = @intCast(i),
        });
    }

    // Appending whole rows can overwrite a row's padding with the temporary's own, so
    // put the stale bytes into the live `detail` column deliberately: poison each
    // element, then rewrite its fields one at a time, which leaves the inter-field gap
    // holding poison. That is the state a store built field by field really has.
    const detail_column = list.field(.detail);
    for (detail_column, 0..) |*detail, i| {
        @memset(std.mem.asBytes(detail), 0xA7);
        detail.tag = @intCast(i);
        detail.value = i * 1000;
    }

    // Prove the poison is actually there, so the test cannot pass by having nothing to
    // scrub in the first place.
    var poisoned_padding_bytes: usize = 0;
    for (detail_column) |*detail| {
        for (std.mem.asBytes(detail)) |byte| {
            if (byte == 0xA7) poisoned_padding_bytes += 1;
        }
    }
    try testing.expect(poisoned_padding_bytes > 0);

    const before = try gpa.alloc(u8, backing_bytes);
    defer gpa.free(before);
    @memcpy(before, list.items.bytes[0..backing_bytes]);

    const buffer = try serializeMultiListToBuffer(gpa, Row, &list);
    defer gpa.free(buffer);

    // Every byte of the source backing store is unchanged, live rows included.
    try testing.expectEqualSlices(u8, before, list.items.bytes[0..backing_bytes]);

    // ...while the serialized copy is scrubbed: no poison byte reaches the output.
    try testing.expect(std.mem.findScalar(u8, buffer, 0xA7) == null);

    // And the values survive the scrub.
    const serialized: *const SafeMultiList(Row).Serialized = @ptrCast(@alignCast(buffer.ptr));
    const loaded = serialized.deserializeInto(@intFromPtr(buffer.ptr));
    try testing.expectEqual(@as(u32, row_count), loaded.len());
    for (0..row_count) |i| {
        try testing.expectEqualDeep(
            list.get(@enumFromInt(@as(u32, @intCast(i)))),
            loaded.get(@enumFromInt(@as(u32, @intCast(i)))),
        );
    }
}

test "SafeMultiList round-trips when column order differs from declaration order" {
    const gpa = testing.allocator;

    var list = try buildMixedAlign(gpa, 0, 6);
    defer list.deinit(gpa);

    const buffer = try serializeMultiListToBuffer(gpa, MixedAlignElem, &list);
    defer gpa.free(buffer);

    const serialized: *const SafeMultiList(MixedAlignElem).Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized.validateRelocations(buffer.len);
    const loaded = serialized.deserializeInto(@intFromPtr(buffer.ptr));

    try testing.expectEqual(@as(u32, 6), loaded.len());
    try testing.expectEqual(@as(usize, 6), loaded.items.capacity);
    for (0..6) |i| {
        const expected = list.get(@enumFromInt(@as(u32, @intCast(i))));
        try testing.expectEqualDeep(expected, loaded.get(@enumFromInt(@as(u32, @intCast(i)))));
    }
}

test "SafeMultiList serialization: empty list writes no columns" {
    const gpa = testing.allocator;

    var list = SafeMultiList(MixedAlignElem){};
    defer list.deinit(gpa);
    // Reserved-but-unused capacity must still write nothing.
    try list.ensureTotalCapacity(gpa, 32);

    const buffer = try serializeMultiListToBuffer(gpa, MixedAlignElem, &list);
    defer gpa.free(buffer);

    try testing.expectEqual(@sizeOf(SafeMultiList(MixedAlignElem).Serialized), buffer.len);
    const serialized: *const SafeMultiList(MixedAlignElem).Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized.validateRelocations(buffer.len);
    try testing.expectEqual(@as(u32, 0), serialized.deserializeInto(@intFromPtr(buffer.ptr)).len());
}

test "SafeMultiList: both serialize entry points produce the same column bytes" {
    // There are two serialization entry points (the `Serialized` header form used by
    // composing stores, and the pointer-returning form). They must agree byte for byte,
    // or a store that mixes them writes a blob its own reader cannot interpret.
    const gpa = testing.allocator;

    var list = try buildMixedAlign(gpa, 0, 7);
    defer list.deinit(gpa);

    var header_writer = CompactWriter.init();
    defer header_writer.deinit(gpa);
    const serialized = try header_writer.appendAlloc(gpa, SafeMultiList(MixedAlignElem).Serialized);
    try serialized.serialize(&list, gpa, &header_writer);
    const header_buf = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, header_writer.total_bytes);
    defer gpa.free(header_buf);
    _ = try header_writer.writeToBuffer(header_buf);
    const header_columns_at: usize = @intCast(serialized.offset);

    var direct_writer = CompactWriter.init();
    defer direct_writer.deinit(gpa);
    const anchor = try direct_writer.appendAlloc(gpa, SafeMultiList(MixedAlignElem).Serialized);
    anchor.* = .{ .offset = 0, .len = 0, .capacity = 0 };
    const direct = try list.serialize(gpa, &direct_writer);
    const direct_buf = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, direct_writer.total_bytes);
    defer gpa.free(direct_buf);
    _ = try direct_writer.writeToBuffer(direct_buf);
    const direct_columns_at = @intFromPtr(direct.items.bytes);

    const column_bytes = 7 * (8 + 2 + 1);
    try testing.expectEqualSlices(
        u8,
        header_buf[header_columns_at..][0..column_bytes],
        direct_buf[direct_columns_at..][0..column_bytes],
    );
    try testing.expectEqual(@as(u64, 7), serialized.len);
    try testing.expectEqual(@as(u64, 7), serialized.capacity);
    try testing.expectEqual(@as(usize, 7), direct.items.capacity);
}

/// Serialize `list` into a stack-allocated `Serialized` header and a fresh writer, so
/// the data is the very first thing the writer gathers and therefore begins at byte
/// zero. Returns the header alongside the bytes; the caller frees the bytes.
fn serializeAtByteZero(
    gpa: Allocator,
    comptime Container: type,
    list: *const Container,
) (Allocator.Error || error{BufferTooSmall})!struct {
    header: Container.Serialized,
    bytes: []align(CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits()) u8,
} {
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    var header: Container.Serialized = undefined;
    try header.serialize(list, gpa, &writer);
    const bytes = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    errdefer gpa.free(bytes);
    _ = try writer.writeToBuffer(bytes);
    return .{ .header = header, .bytes = bytes };
}

test "SafeList serialization records a byte-zero offset as zero" {
    // A store normally writes its header struct into the writer first, which puts its
    // data past byte zero. Nothing requires that, though, and a serializer that carries
    // the offset in a pointer cannot express zero: it either panics on the null cast or
    // silently records the wrong place. Keep the offset an integer.
    const gpa = testing.allocator;

    {
        var list = SafeList(u32){};
        defer list.deinit(gpa);
        for (0..4) |i| _ = try list.append(gpa, @intCast(0x1000 + i));

        const written = try serializeAtByteZero(gpa, SafeList(u32), &list);
        defer gpa.free(written.bytes);

        try testing.expectEqual(@as(i64, 0), written.header.offset);
        try testing.expectEqual(@as(u64, 4), written.header.len);
        try testing.expectEqual(written.bytes.len, 4 * @sizeOf(u32));

        try written.header.validateRelocations(written.bytes.len);
        const loaded = written.header.deserializeInto(@intFromPtr(written.bytes.ptr));
        try testing.expectEqual(@as(u64, 4), loaded.len());
        for (0..4) |i| {
            try testing.expectEqual(@as(u32, @intCast(0x1000 + i)), loaded.get(@enumFromInt(@as(u32, @intCast(i)))).*);
        }
    }

    {
        // Empty at byte zero: no bytes written, and the header still reads back empty.
        var empty = SafeList(u32){};
        defer empty.deinit(gpa);

        const written = try serializeAtByteZero(gpa, SafeList(u32), &empty);
        defer gpa.free(written.bytes);

        try testing.expectEqual(@as(i64, 0), written.header.offset);
        try testing.expectEqual(@as(u64, 0), written.header.len);
        try testing.expectEqual(@as(usize, 0), written.bytes.len);
        try written.header.validateRelocations(written.bytes.len);
        try testing.expectEqual(@as(u64, 0), written.header.deserializeInto(@intFromPtr(written.bytes.ptr)).len());
    }
}

test "SafeMultiList serialization records a byte-zero offset as zero" {
    // Same property for the column writer, with a scrubbable column so the first column
    // at byte zero is one that takes the copy-and-canonicalize path rather than the
    // gather-verbatim one.
    const gpa = testing.allocator;

    const Detail = struct { tag: u8, value: u64 }; // auto layout: inter-field gap
    const Row = struct { detail: Detail, plain: u32 };
    comptime std.debug.assert(CompactWriter.needsPaddingZeroing(Detail));

    {
        var list = SafeMultiList(Row){};
        defer list.deinit(gpa);
        for (0..5) |i| {
            _ = try list.append(gpa, .{
                .detail = .{ .tag = @intCast(i), .value = 0x2000 + i },
                .plain = @intCast(0x30 + i),
            });
        }
        // The widest column is the scrubbable one, so it sorts first and lands at zero.
        const detail_column = list.field(.detail);
        for (detail_column, 0..) |*detail, i| {
            @memset(std.mem.asBytes(detail), 0xC3);
            detail.tag = @intCast(i);
            detail.value = 0x2000 + i;
        }

        const written = try serializeAtByteZero(gpa, SafeMultiList(Row), &list);
        defer gpa.free(written.bytes);

        try testing.expectEqual(@as(i64, 0), written.header.offset);
        try testing.expectEqual(@as(u64, 5), written.header.len);
        try testing.expectEqual(@as(u64, 5), written.header.capacity);
        try testing.expectEqual(written.bytes.len, 5 * (@sizeOf(Detail) + @sizeOf(u32)));

        // The scrubbed copy carries no poison even though the source column does.
        try testing.expect(std.mem.findScalar(u8, written.bytes, 0xC3) == null);

        try written.header.validateRelocations(written.bytes.len);
        const loaded = written.header.deserializeInto(@intFromPtr(written.bytes.ptr));
        try testing.expectEqual(@as(u32, 5), loaded.len());
        for (0..5) |i| {
            const row = loaded.get(@enumFromInt(@as(u32, @intCast(i))));
            try testing.expectEqual(@as(u8, @intCast(i)), row.detail.tag);
            try testing.expectEqual(@as(u64, 0x2000 + i), row.detail.value);
            try testing.expectEqual(@as(u32, @intCast(0x30 + i)), row.plain);
        }
    }

    {
        var empty = SafeMultiList(Row){};
        defer empty.deinit(gpa);

        const written = try serializeAtByteZero(gpa, SafeMultiList(Row), &empty);
        defer gpa.free(written.bytes);

        try testing.expectEqual(@as(i64, 0), written.header.offset);
        try testing.expectEqual(@as(u64, 0), written.header.len);
        try testing.expectEqual(@as(usize, 0), written.bytes.len);
        try written.header.validateRelocations(written.bytes.len);
        try testing.expectEqual(@as(u32, 0), written.header.deserializeInto(@intFromPtr(written.bytes.ptr)).len());
    }
}

test "SafeList: both serialize entry points produce the same item bytes" {
    // The `Serialized` header form and the pointer-returning form must stay on one
    // implementation, or a store that mixes them writes a blob its own reader cannot
    // interpret. The padded item type also proves both scrub.
    const gpa = testing.allocator;

    const Padded = struct { tag: u8, value: u64 };
    comptime std.debug.assert(CompactWriter.needsPaddingZeroing(Padded));

    var list = SafeList(Padded){};
    defer list.deinit(gpa);
    for (0..6) |i| _ = try list.append(gpa, .{ .tag = @intCast(i), .value = 0x4000 + i });
    for (list.items.items, 0..) |*item, i| {
        @memset(std.mem.asBytes(item), 0xD4);
        item.tag = @intCast(i);
        item.value = 0x4000 + i;
    }

    var header_writer = CompactWriter.init();
    defer header_writer.deinit(gpa);
    const header = try header_writer.appendAlloc(gpa, SafeList(Padded).Serialized);
    try header.serialize(&list, gpa, &header_writer);
    const header_buf = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, header_writer.total_bytes);
    defer gpa.free(header_buf);
    _ = try header_writer.writeToBuffer(header_buf);
    const header_items_at: usize = @intCast(header.offset);

    var direct_writer = CompactWriter.init();
    defer direct_writer.deinit(gpa);
    const anchor = try direct_writer.appendAlloc(gpa, SafeList(Padded).Serialized);
    anchor.* = .{ .offset = 0, .len = 0, .capacity = 0 };
    const direct = try list.serialize(gpa, &direct_writer);
    const direct_buf = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, direct_writer.total_bytes);
    defer gpa.free(direct_buf);
    _ = try direct_writer.writeToBuffer(direct_buf);
    const direct_items_at = @intFromPtr(direct.items.items.ptr);

    const item_bytes = 6 * @sizeOf(Padded);
    try testing.expectEqualSlices(
        u8,
        header_buf[header_items_at..][0..item_bytes],
        direct_buf[direct_items_at..][0..item_bytes],
    );
    try testing.expect(std.mem.findScalar(u8, header_buf[header_items_at..][0..item_bytes], 0xD4) == null);
    try testing.expectEqual(@as(u64, 6), header.len);
    try testing.expectEqual(@as(usize, 6), direct.items.capacity);
}
