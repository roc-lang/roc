//! Lists that make it easier to avoid incorrect indexing.

const std = @import("std");

const testing = std.testing;
const Allocator = std.mem.Allocator;

const CompactWriter = @import("CompactWriter.zig");

/// The most elements a safe list holds, so every index and range count is a
/// `u32`.
const max_len: usize = std.math.maxInt(u32);

/// Rejects a length past the 32-bit index range as allocation failure.
fn checkLen(new_len: usize) Allocator.Error!void {
    if (new_len > max_len) return error.OutOfMemory;
}

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

/// An iterator over consecutive indices of type `Idx` in `[current, end)`.
pub fn IndexIterator(comptime Idx: type) type {
    return struct {
        end: u32,
        current: u32,

        /// Get the next index from this iterator, or `null` if the iterator is finished.
        pub fn next(iter: *@This()) ?Idx {
            if (iter.end == iter.current) return null;
            const curr = iter.current;
            iter.current += 1;
            return @fromBackingInt(@intCast(curr));
        }
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
        count: u32,

        /// The `start` of every range built by `empty`. A list holds at most
        /// `maxInt(u32)` rows, so no row has this index, and a range that
        /// addresses no row still persists fully defined bytes (see design.md
        /// "Fully Defined Persisted Bytes").
        pub const empty_start: Idx = @fromBackingInt(std.math.maxInt(u32));

        /// An empty range
        pub fn empty() Self {
            return .{ .start = empty_start, .count = 0 };
        }

        // Drop first elem from the span, if possible
        pub fn dropFirstElem(self: *Self) void {
            if (self.count == 0) return;
            self.start = @fromBackingInt(@intCast(@backingInt(self.start) + 1));
            self.count -= 1;
        }

        /// Get the length of a range slice
        pub fn len(self: @This()) u32 {
            return self.count;
        }

        /// Get the last index in the range
        pub fn end(self: @This()) Idx {
            return @fromBackingInt(@intCast(@backingInt(self.start) + self.count));
        }

        /// Return whether the range is empty
        pub fn isEmpty(self: @This()) bool {
            return self.count == 0;
        }

        /// Iterate over the indices in this range.
        pub fn iterIndices(self: @This()) IndexIterator(Idx) {
            return .{
                .end = @backingInt(self.start) + self.count,
                .current = @backingInt(self.start),
            };
        }
    };
}

/// A growable list addressed by typed indices, stored as a pointer, a `u32`
/// length and a `u32` capacity.
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
///
/// A list owns its memory exactly when `capacity != 0`. A list with
/// `capacity == 0` and a nonzero length is borrowed: its items live in memory
/// the list does not own, such as a mapped cache entry. Reads and in-place
/// writes go straight to that memory. The first growth copies the items into
/// fresh owned memory and leaves the borrowed memory alone, and `deinit` frees
/// nothing. An empty list also has `capacity == 0` and owns nothing.
pub fn SafeList(comptime T: type) type {
    return struct {
        const Self = @This();

        ptr: [*]T = empty_ptr,
        length: u32 = 0,
        capacity: u32 = 0,

        const empty_ptr: [*]T = @ptrFromInt(@alignOf(T));

        /// Items allocated by the first growth of an empty list, so small lists
        /// start at about one cache line.
        const init_capacity: usize = @max(1, std.atomic.cache_line / @max(1, @sizeOf(T)));

        /// An index for an item in the list.
        pub const Idx = enum(u32) {
            /// The first valid index in the list.
            first = 0,
            _,

            /// Get the raw u32 value for storage
            pub fn toU32(self: Idx) u32 {
                return @backingInt(self);
            }

            /// Create from a raw u32 value
            pub fn fromU32(value: u32) Idx {
                return @fromBackingInt(@intCast(value));
            }
        };

        /// A non-type-safe slice of the list.
        pub const Slice = []T;

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

            /// One relocatable base pointer (`offset`) is fixed up per `deserializeInto`,
            /// regardless of `len`. Counted by
            /// `artifact_serialize.relocatablePointerCount` so a composing store's total
            /// fixup count includes its `SafeList`-backed (e.g. interner) fields.
            pub const serialized_relocatable_pointers: usize = 1;

            /// The element type whose bytes are serialized, so a layout fingerprint can
            /// reflect a change to the element's field order/size.
            pub const SerializedElement = T;

            /// L-10: reject an `(offset, len)` whose `len` elements reach outside the
            /// `backing_len`-byte buffer before `deserializeInto` dereferences it.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                if (self.len > max_len) return error.CorruptArtifact;
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
                const list_items = safe_list.items();
                // An integer offset, so a store whose items legitimately begin at byte
                // zero records zero rather than an unrepresentable null pointer.
                const data_offset = try writer.appendSlicePodZeroedOffset(allocator, @as([]const T, list_items));

                self.offset = @intCast(data_offset);
                self.len = list_items.len;
                self.capacity = list_items.len;
            }

            /// A borrowed list (`capacity == 0`) whose items are the serialized items
            /// in the buffer at `base`. The list reads and writes those items in
            /// place, copies them on its first growth, and never frees them.
            pub fn deserializeInto(self: *const Serialized, base: usize) SafeList(T) {
                if (self.len == 0) return .{};
                return .{
                    .ptr = @ptrFromInt(base +% @as(usize, @intCast(self.offset))),
                    .length = @intCast(self.len),
                    .capacity = 0,
                };
            }

            /// Deserialize into a SafeList value with fresh memory allocation.
            /// The returned SafeList owns its memory.
            pub fn deserializeWithCopy(self: *const Serialized, base: usize, gpa: Allocator) Allocator.Error!SafeList(T) {
                const borrowed = self.deserializeInto(base);
                return borrowed.clone(gpa);
            }
        };

        /// A list that owns a copy of `source`.
        pub fn fromSlice(gpa: Allocator, source: []const T) Allocator.Error!SafeList(T) {
            var list = try SafeList(T).initCapacity(gpa, source.len);
            _ = list.appendSliceAssumeCapacity(source);
            return list;
        }

        /// A list that takes ownership of `owned`, which `gpa` allocated with
        /// exactly `owned.len` items.
        pub fn fromOwnedSlice(owned: []T) SafeList(T) {
            std.debug.assert(owned.len <= max_len);
            if (owned.len == 0) return .{};
            return .{ .ptr = owned.ptr, .length = @intCast(owned.len), .capacity = @intCast(owned.len) };
        }

        /// A borrowed list over `borrowed`, which outlives the list. The list reads
        /// and writes `borrowed` in place, copies it on its first growth, and
        /// never frees it.
        pub fn fromBorrowedSlice(borrowed: []T) SafeList(T) {
            std.debug.assert(borrowed.len <= max_len);
            if (borrowed.len == 0) return .{};
            return .{ .ptr = borrowed.ptr, .length = @intCast(borrowed.len), .capacity = 0 };
        }

        /// Initialize the `SafeList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!SafeList(T) {
            var list = SafeList(T){};
            try list.ensureTotalCapacityPrecise(gpa, capacity);
            return list;
        }

        /// Free the memory this list owns. A borrowed or empty list owns none.
        pub fn deinit(self: *SafeList(T), gpa: Allocator) void {
            if (self.capacity != 0) gpa.free(self.ptr[0..self.capacity]);
            self.* = undefined;
        }

        /// Clone this list into fresh owned memory.
        pub fn clone(self: *const SafeList(T), gpa: Allocator) Allocator.Error!SafeList(T) {
            var cloned = try SafeList(T).initCapacity(gpa, @max(self.capacity, self.length));
            _ = cloned.appendSliceAssumeCapacity(self.items());
            return cloned;
        }

        /// Get the length of this list.
        pub fn len(self: *const SafeList(T)) u64 {
            return self.length;
        }

        /// All the items in this list, as a non-type-safe slice.
        pub fn items(self: *const SafeList(T)) []T {
            return self.ptr[0..self.length];
        }

        /// Ensure the list holds at least `new_capacity` items without further
        /// allocation, growing super-linearly so repeated appends stay amortized
        /// constant time. A request no larger than the length or the capacity
        /// changes nothing, so a borrowed list stays borrowed.
        pub fn ensureTotalCapacity(self: *SafeList(T), gpa: Allocator, new_capacity: usize) Allocator.Error!void {
            if (new_capacity <= self.length or new_capacity <= self.capacity) return;
            try checkLen(new_capacity);
            try self.ensureTotalCapacityPrecise(gpa, growCapacity(@max(self.capacity, self.length), new_capacity));
        }

        /// Ensure the list holds at least `new_capacity` items without further
        /// allocation, allocating exactly `new_capacity` when it must grow. A
        /// request no larger than the length or the capacity changes nothing.
        pub fn ensureTotalCapacityPrecise(self: *SafeList(T), gpa: Allocator, new_capacity: usize) Allocator.Error!void {
            if (new_capacity <= self.length or new_capacity <= self.capacity) return;
            try checkLen(new_capacity);
            if (self.capacity != 0) {
                const old_memory = self.ptr[0..self.capacity];
                if (gpa.remap(old_memory, new_capacity)) |new_memory| {
                    self.ptr = new_memory.ptr;
                    self.capacity = @intCast(new_capacity);
                    return;
                }
            }
            const new_memory = try gpa.alloc(T, new_capacity);
            @memcpy(new_memory[0..self.length], self.items());
            if (self.capacity != 0) gpa.free(self.ptr[0..self.capacity]);
            self.ptr = new_memory.ptr;
            self.capacity = @intCast(new_capacity);
        }

        /// Ensure the list holds at least `additional` more items without further
        /// allocation.
        pub fn ensureUnusedCapacity(self: *SafeList(T), gpa: Allocator, additional: usize) Allocator.Error!void {
            const new_len = std.math.add(usize, self.length, additional) catch return error.OutOfMemory;
            try self.ensureTotalCapacity(gpa, new_len);
        }

        /// Add an item to the end of this list.
        pub fn append(self: *SafeList(T), gpa: Allocator, item: T) Allocator.Error!Idx {
            try self.ensureUnusedCapacity(gpa, 1);
            return self.appendAssumeCapacity(item);
        }

        /// Add a new item to the end of this list assuming capacity is sufficient to hold an additional item.
        pub fn appendAssumeCapacity(self: *SafeList(T), item: T) Idx {
            std.debug.assert(self.length < self.capacity);
            const idx = self.length;
            self.ptr[idx] = item;
            self.length += 1;
            return @fromBackingInt(idx);
        }

        /// Add `count` copies of `item` to the end of this list.
        pub fn appendNTimes(self: *SafeList(T), gpa: Allocator, item: T, count: usize) Allocator.Error!void {
            try self.ensureUnusedCapacity(gpa, count);
            self.appendNTimesAssumeCapacity(item, count);
        }

        /// Add `count` copies of `item` to the end of this list assuming capacity is sufficient.
        pub fn appendNTimesAssumeCapacity(self: *SafeList(T), item: T, count: usize) void {
            std.debug.assert(self.length + count <= self.capacity);
            @memset(self.ptr[self.length..][0..count], item);
            self.length += @intCast(count);
        }

        /// Add one uninitialized item to the end of this list and return a pointer to it.
        pub fn addOne(self: *SafeList(T), gpa: Allocator) Allocator.Error!*T {
            try self.ensureUnusedCapacity(gpa, 1);
            const idx = self.length;
            self.length += 1;
            return &self.ptr[idx];
        }

        /// Create a range from the provided idx to the end of the list
        pub fn rangeToEnd(self: *SafeList(T), start_int: u32) Range {
            const len_int = self.len();
            std.debug.assert(start_int <= len_int);
            return Range{ .start = @fromBackingInt(@intCast(start_int)), .count = @intCast(len_int - start_int) };
        }

        /// Add all the items in a slice to the end of this list.
        pub fn appendSlice(self: *SafeList(T), gpa: Allocator, new_items: []const T) Allocator.Error!Range {
            try self.ensureUnusedCapacity(gpa, new_items.len);
            return self.appendSliceAssumeCapacity(new_items);
        }

        /// Add all the items in a slice to the end of this list assuming capacity is sufficient.
        pub fn appendSliceAssumeCapacity(self: *SafeList(T), new_items: []const T) Range {
            const start = self.length;
            std.debug.assert(@as(usize, start) + new_items.len <= self.capacity);
            @memcpy(self.ptr[start..][0..new_items.len], new_items);
            self.length += @intCast(new_items.len);
            return Range{ .start = @fromBackingInt(start), .count = @intCast(new_items.len) };
        }

        /// Insert `item` at index `i`, moving every later item up by one.
        pub fn insert(self: *SafeList(T), gpa: Allocator, i: usize, item: T) Allocator.Error!void {
            try self.insertSlice(gpa, i, &.{item});
        }

        /// Insert `new_items` at index `i`, moving every later item up.
        pub fn insertSlice(self: *SafeList(T), gpa: Allocator, i: usize, new_items: []const T) Allocator.Error!void {
            std.debug.assert(i <= self.length);
            try self.ensureUnusedCapacity(gpa, new_items.len);
            const old_len: usize = self.length;
            self.length += @intCast(new_items.len);
            const all = self.items();
            std.mem.copyBackwards(T, all[i + new_items.len ..], all[i..old_len]);
            @memcpy(all[i..][0..new_items.len], new_items);
        }

        /// Remove and return the item at index `i`, moving every later item down by one.
        pub fn orderedRemove(self: *SafeList(T), i: usize) T {
            const all = self.items();
            const removed = all[i];
            std.mem.copyForwards(T, all[i .. all.len - 1], all[i + 1 ..]);
            self.length -= 1;
            return removed;
        }

        /// Remove and return the last item, or `null` when the list is empty.
        pub fn pop(self: *SafeList(T)) ?T {
            if (self.length == 0) return null;
            self.length -= 1;
            return self.ptr[self.length];
        }

        /// Set the length to `new_len`, which must not exceed the length. The
        /// capacity, and so the list's ownership, is unchanged.
        pub fn shrinkRetainingCapacity(self: *SafeList(T), new_len: usize) void {
            std.debug.assert(new_len <= self.length);
            self.length = @intCast(new_len);
        }

        /// Set the length to `new_len`, which must not exceed the length, and
        /// release the owned capacity past it. A borrowed list only shortens:
        /// its memory is not the allocator's. When a smaller allocation cannot
        /// be made, the list keeps its larger one.
        pub fn shrinkAndFree(self: *SafeList(T), gpa: Allocator, new_len: usize) void {
            std.debug.assert(new_len <= self.length);
            self.length = @intCast(new_len);
            if (self.capacity == 0 or new_len == self.capacity) return;
            const old_memory = self.ptr[0..self.capacity];
            if (new_len == 0) {
                gpa.free(old_memory);
                self.* = .{};
                return;
            }
            if (gpa.remap(old_memory, new_len)) |new_memory| {
                self.ptr = new_memory.ptr;
                self.capacity = @intCast(new_len);
                return;
            }
            const new_memory = gpa.alloc(T, new_len) catch return;
            @memcpy(new_memory, self.items());
            gpa.free(old_memory);
            self.ptr = new_memory.ptr;
            self.capacity = @intCast(new_len);
        }

        /// Remove every item, keeping the capacity and so the list's ownership.
        pub fn clearRetainingCapacity(self: *SafeList(T)) void {
            self.length = 0;
        }

        /// Set the length to `new_len`, growing first when needed. Items past the
        /// old length are undefined.
        pub fn resize(self: *SafeList(T), gpa: Allocator, new_len: usize) Allocator.Error!void {
            try self.ensureTotalCapacity(gpa, new_len);
            self.length = @intCast(new_len);
        }

        /// Unsupported. A borrowed list has `capacity == 0` with a nonzero
        /// length, so "the capacity" is not a length the list could be extended
        /// to: this would truncate a borrowed list to nothing.
        pub fn expandToCapacity(_: *SafeList(T)) void {
            @compileError("SafeList does not support expandToCapacity: a borrowed list has capacity 0 below its length. Reserve with ensureUnusedCapacity and append instead.");
        }

        /// Unsupported. A borrowed list has `capacity == 0` with a nonzero
        /// length, so the items between the length and the capacity are not a
        /// well-formed slice for it.
        pub fn unusedCapacitySlice(_: *SafeList(T)) []T {
            @compileError("SafeList does not support unusedCapacitySlice: a borrowed list has capacity 0 below its length. Reserve with ensureUnusedCapacity and append instead.");
        }

        /// Unsupported. A bounded append writes into spare capacity and fails
        /// when there is none, which would report a borrowed list as full and
        /// could never grow it out of the memory it borrows.
        pub fn appendBounded(_: *SafeList(T), _: T) error{OutOfMemory}!void {
            @compileError("SafeList does not support bounded appends: a borrowed list has no spare capacity and must grow by copying. Use append, or ensureUnusedCapacity with appendAssumeCapacity.");
        }

        /// Unsupported, for the same reason as `appendBounded`.
        pub fn appendSliceBounded(_: *SafeList(T), _: []const T) error{OutOfMemory}!void {
            @compileError("SafeList does not support bounded appends: a borrowed list has no spare capacity and must grow by copying. Use appendSlice, or ensureUnusedCapacity with appendSliceAssumeCapacity.");
        }

        /// The items as a slice of exactly the length that the caller owns and
        /// frees with `gpa`. A borrowed list's items are copied. The list is left
        /// empty.
        pub fn toOwnedSlice(self: *SafeList(T), gpa: Allocator) Allocator.Error![]T {
            const length: usize = self.length;
            if (self.capacity == 0) {
                const owned = try gpa.dupe(T, self.items());
                self.* = .{};
                return owned;
            }
            const old_memory = self.ptr[0..self.capacity];
            if (gpa.remap(old_memory, length)) |new_memory| {
                self.* = .{};
                return new_memory;
            }
            const owned = try gpa.dupe(T, self.items());
            gpa.free(old_memory);
            self.* = .{};
            return owned;
        }

        /// Convert a range to a slice
        pub fn sliceRange(self: *const SafeList(T), range: Range) Slice {
            // An empty range addresses no row, so return an empty slice directly
            if (range.count == 0) {
                return &.{};
            }

            const start: usize = @backingInt(range.start);
            const end: usize = start + range.count;

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.length);

            return self.ptr[start..end];
        }

        /// Get an item from this list without worrying about out-of-bounds errors.
        pub fn get(self: *const SafeList(T), id: Idx) *T {
            const i: usize = @backingInt(id);
            std.debug.assert(i < self.length);
            return &self.ptr[i];
        }

        /// Set the value of an item in this list without worrying about out-of-bounds errors.
        pub fn set(self: *const SafeList(T), id: Idx, value: T) void {
            self.get(id).* = value;
        }

        /// Iterate over all the indices of the items in this list.
        pub fn iterIndices(self: *const SafeList(T)) IndexIterator(Idx) {
            return .{ .end = @intCast(self.len()), .current = 0 };
        }

        /// An iterator over all the indices in this list.
        pub const Iterator = struct {
            array: *const SafeList(T),
            len: u32,
            current: Idx,

            pub fn next(self: *Iterator) ?T {
                const cur_idx = self.current;
                const cur_int = @backingInt(cur_idx);
                if (self.len == cur_int) {
                    return null;
                }
                self.current = @fromBackingInt(@intCast(cur_int + 1));
                return self.array.get(cur_idx).*;
            }

            pub fn count(self: *Iterator) u32 {
                return self.len - @backingInt(self.current);
            }
        };

        /// Iterate over the elements in a span
        pub fn iterRange(self: *const SafeList(T), range: Range) Iterator {
            return Iterator{
                .array = self,
                .len = @backingInt(range.start) + range.count,
                .current = range.start,
            };
        }

        /// Iterate over all items in this list.
        pub fn iter(self: *const SafeList(T)) Iterator {
            return Iterator{
                .array = self,
                .len = @intCast(self.len()),
                .current = .first,
            };
        }

        /// The capacity to grow to from `current` so it holds at least `minimum`
        /// items: super-linear, and capped at the 32-bit length limit.
        fn growCapacity(current: usize, minimum: usize) usize {
            var new_capacity = current;
            while (true) {
                new_capacity +|= new_capacity / 2 + init_capacity;
                if (new_capacity >= minimum) return @min(new_capacity, max_len);
            }
        }
    };
}

/// A growable struct-of-arrays list addressed by typed indices, stored as a
/// pointer to its columns, a `u32` length and a `u32` capacity word.
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
///
/// The columns sit in one allocation in descending field alignment, each
/// `capacity` rows long, so their addresses depend on the real capacity. The
/// top bit of the capacity word marks a borrowed list, whose columns live in
/// memory the list does not own, such as a mapped cache entry. Reads and
/// in-place writes go straight to that memory; the first growth copies every
/// column into fresh owned memory and clears the mark, and `deinit` frees
/// nothing while the mark is set. A multi-list holds at most `2^31 - 1` rows.
pub fn SafeMultiList(comptime T: type) type {
    return struct {
        const Self = @This();

        bytes: [*]align(@alignOf(T)) u8 = empty_bytes,
        length: u32 = 0,
        /// The capacity in rows in the low 31 bits, and the borrowed mark in
        /// the top bit.
        capacity_and_mark: u32 = 0,

        comptime {
            if (@typeInfo(T) != .@"struct") {
                @compileError("SafeMultiList element '" ++ @typeName(T) ++
                    "' must be a struct: the serialized column layout is derived from its fields");
            }
        }

        const empty_bytes: [*]align(@alignOf(T)) u8 = @ptrFromInt(@alignOf(T));

        /// The top bit of `capacity_and_mark`: set when the columns live in memory
        /// the list does not own.
        const borrowed_mark: u32 = 1 << 31;

        /// The most rows a multi-list holds: the capacity shares its word with the
        /// borrowed mark.
        const max_rows: usize = borrowed_mark - 1;

        /// Rows allocated by the first growth of an empty list.
        const init_capacity: usize = @max(@as(usize, 1), @as(usize, std.atomic.cache_line) / @max(@as(usize, 1), row_bytes));

        const field_types = @typeInfo(T).@"struct".field_types;
        const field_names = @typeInfo(T).@"struct".field_names;

        /// Bytes one row occupies across all columns.
        const row_bytes: usize = blk: {
            var total: usize = 0;
            for (field_types) |FieldType| total += @sizeOf(FieldType);
            break :blk total;
        };

        /// The order in which the columns are laid out in one allocation:
        /// descending field alignment, ties in declaration order. Each column is
        /// `@sizeOf(field) * capacity` bytes and they are packed with no gaps
        /// between them, because a field's size is a multiple of its alignment and
        /// the alignments only decrease. The serialized form uses the same order
        /// at `capacity == len`.
        const column_order: [field_types.len]usize = blk: {
            var order: [field_types.len]usize = undefined;
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

        comptime {
            // No padding is ever needed between columns, at any capacity: a
            // column's start is `capacity * columnBytesBefore`, and
            // `columnBytesBefore` is a sum of element sizes that are each a multiple
            // of an alignment at least as large as this column's, so it is a
            // multiple of this column's alignment. The block itself is aligned to
            // the element alignment, which is at least every field alignment.
            for (column_order) |field_index| {
                if (@sizeOf(field_types[field_index]) == 0) continue;
                const field_align = fieldAlignment(field_index);
                if (columnBytesBefore(field_index) % field_align != 0) {
                    @compileError("SafeMultiList(" ++ @typeName(T) ++ ") column '" ++
                        field_names[field_index] ++ "' would need alignment padding");
                }
                if (field_align > @alignOf(T)) {
                    @compileError("SafeMultiList(" ++ @typeName(T) ++
                        ") has a field alignment larger than the element alignment");
                }
            }
        }

        fn fieldAlignment(comptime field_index: usize) comptime_int {
            const info = @typeInfo(T).@"struct";
            return info.field_attrs[field_index].@"align" orelse @alignOf(info.field_types[field_index]);
        }

        /// Sum of the element sizes of the columns laid out before `field_index`.
        fn columnBytesBefore(comptime field_index: usize) usize {
            return comptime blk: {
                var total: usize = 0;
                for (column_order) |candidate| {
                    if (candidate == field_index) break :blk total;
                    total += @sizeOf(field_types[candidate]);
                }
                @compileError("column_order must list every field index of " ++ @typeName(T));
            };
        }

        /// Index of an item in the list.
        pub const Idx = enum(u32) { first = 0, _ };

        /// A non-type-safe range view. Resolves fields from the backing list on
        /// demand, so the view stays valid across growth of the list.
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
                return self.list.getAt(self.start + index);
            }
        };

        /// A type-safe slice of the list.
        pub const Range = SafeRange(Idx);

        /// One of the comptime fields in the list's wrapped type.
        pub const Field = std.meta.FieldEnum(T);

        /// The number of rows the list holds without further allocation.
        pub fn capacity(self: *const Self) u32 {
            return self.capacity_and_mark & ~borrowed_mark;
        }

        /// Whether the columns live in memory the list does not own.
        pub fn isBorrowed(self: *const Self) bool {
            return self.capacity_and_mark & borrowed_mark != 0;
        }

        /// The column of `field_index` in an allocation of `cap` rows at `bytes`.
        inline fn columnPtr(bytes: [*]align(@alignOf(T)) u8, cap: usize, comptime field_index: usize) [*]field_types[field_index] {
            return @ptrCast(@alignCast(bytes + cap * columnBytesBefore(field_index)));
        }

        /// A slice of all values for a specific field of the wrapped type.
        pub fn field(self: *const Self, comptime field_name: Field) []@FieldType(T, @tagName(field_name)) {
            const field_index = comptime fieldIndex(field_name);
            return columnPtr(self.bytes, self.capacity(), field_index)[0..self.length];
        }

        fn fieldIndex(comptime field_name: Field) usize {
            return std.meta.fieldIndex(T, @tagName(field_name)).?;
        }

        /// The value for a specific field at a specific index in the list.
        pub fn fieldItem(self: *const Self, comptime field_name: Field, idx: Idx) @FieldType(T, @tagName(field_name)) {
            return self.field(field_name)[@as(usize, @backingInt(idx))];
        }

        /// Initialize the `SafeMultiList` with the specified capacity.
        pub fn initCapacity(gpa: Allocator, cap: usize) Allocator.Error!Self {
            var list = Self{};
            try list.setOwnedCapacity(gpa, cap);
            return list;
        }

        /// Free the memory this list owns. A borrowed or empty list owns none.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (!self.isBorrowed() and self.capacity() != 0) {
                gpa.free(self.bytes[0 .. @as(usize, self.capacity()) * row_bytes]);
            }
            self.* = undefined;
        }

        /// Clone this multilist into fresh owned memory.
        pub fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
            var cloned = Self{};
            if (self.length == 0) return cloned;
            try cloned.setOwnedCapacity(gpa, self.capacity());
            cloned.length = self.length;
            inline for (column_order) |field_index| {
                if (@sizeOf(field_types[field_index]) > 0) {
                    @memcpy(
                        columnPtr(cloned.bytes, cloned.capacity(), field_index)[0..self.length],
                        columnPtr(self.bytes, self.capacity(), field_index)[0..self.length],
                    );
                }
            }
            return cloned;
        }

        /// Get the length of this list.
        pub fn len(self: *const Self) u32 {
            return self.length;
        }

        /// Create a range from the provided idx to the end of the list
        pub fn rangeToEnd(self: *Self, start_int: u32) Range {
            const len_int = self.len();
            std.debug.assert(start_int <= len_int);
            return Range{ .start = @fromBackingInt(@intCast(start_int)), .count = @intCast(len_int - start_int) };
        }

        /// Add a new item to the end of this list.
        pub fn append(self: *Self, gpa: Allocator, item: T) Allocator.Error!Idx {
            try self.ensureUnusedCapacity(gpa, 1);
            return self.appendAssumeCapacity(item);
        }

        /// Add a new item to the end of this list assuming capacity is sufficient to hold an additional item.
        /// On a borrowed list after a shrink, the row is written in place.
        pub fn appendAssumeCapacity(self: *Self, item: T) Idx {
            std.debug.assert(self.length < self.capacity());
            const idx = self.length;
            self.length += 1;
            self.setAt(idx, item);
            return @fromBackingInt(idx);
        }

        /// Add all the items in a slice to the end of this list.
        pub fn appendSlice(self: *Self, gpa: Allocator, elems: []const T) Allocator.Error!Range {
            if (elems.len == 0) {
                return .{ .start = .first, .count = 0 };
            }
            const start_length = self.len();
            try self.ensureUnusedCapacity(gpa, elems.len);
            for (elems) |elem| _ = self.appendAssumeCapacity(elem);
            return Range{ .start = @fromBackingInt(start_length), .count = @intCast(elems.len) };
        }

        /// Convert a range to a slice
        pub inline fn sliceRange(self: *const Self, range: Range) Slice {
            if (range.count == 0) return .{ .list = self, .start = 0, .len = 0 };

            const start: usize = @backingInt(range.start);
            const end: usize = start + range.count;

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.length);
            return .{ .list = self, .start = start, .len = end - start };
        }

        fn setAt(self: *const Self, index: usize, value: T) void {
            std.debug.assert(index < self.length);
            const cap = self.capacity();
            inline for (field_names, 0..) |name, field_index| {
                columnPtr(self.bytes, cap, field_index)[index] = @field(value, name);
            }
        }

        fn getAt(self: *const Self, index: usize) T {
            std.debug.assert(index < self.length);
            const cap = self.capacity();
            var result: T = undefined;
            inline for (field_names, 0..) |name, field_index| {
                @field(result, name) = columnPtr(self.bytes, cap, field_index)[index];
            }
            return result;
        }

        /// Set the value of an element in this list. On a borrowed list the row
        /// is written in place.
        pub fn set(self: *Self, idx: Idx, value: T) void {
            self.setAt(@backingInt(idx), value);
        }

        /// The element at `idx`, gathered from every column.
        pub fn get(self: *const Self, idx: Idx) T {
            return self.getAt(@backingInt(idx));
        }

        /// Remove and return the last element, or `null` when the list is empty.
        pub fn pop(self: *Self) ?T {
            if (self.length == 0) return null;
            const value = self.getAt(self.length - 1);
            self.length -= 1;
            return value;
        }

        /// Set the length to `new_len`, which must not exceed the length. The
        /// capacity and the borrowed mark are unchanged.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            std.debug.assert(new_len <= self.length);
            self.length = @intCast(new_len);
        }

        /// Remove every element, keeping the capacity and the borrowed mark.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.length = 0;
        }

        /// Make sure the list holds at least `new_capacity` rows without further
        /// allocation, growing super-linearly. A request no larger than the
        /// length or the capacity changes nothing, so a borrowed list stays
        /// borrowed.
        pub fn ensureTotalCapacity(self: *Self, gpa: Allocator, new_capacity: usize) Allocator.Error!void {
            if (new_capacity <= self.length or new_capacity <= self.capacity()) return;
            if (new_capacity > max_rows) return error.OutOfMemory;
            var better: usize = self.capacity();
            while (true) {
                better +|= better / 2 + init_capacity;
                if (better >= new_capacity) break;
            }
            try self.setOwnedCapacity(gpa, @min(better, max_rows));
        }

        /// Make sure the list holds at least `additional` more rows without
        /// further allocation.
        pub fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional: usize) Allocator.Error!void {
            const needed = std.math.add(usize, self.length, additional) catch return error.OutOfMemory;
            try self.ensureTotalCapacity(gpa, needed);
        }

        /// Move the rows into a fresh owned allocation of `new_capacity` rows,
        /// which must hold them. Owned memory is freed; borrowed memory is left to
        /// its owner, and the list is owned afterwards.
        fn setOwnedCapacity(self: *Self, gpa: Allocator, new_capacity: usize) Allocator.Error!void {
            std.debug.assert(new_capacity >= self.length);
            if (new_capacity > max_rows) return error.OutOfMemory;
            const new_bytes = try gpa.alignedAlloc(u8, .of(T), new_capacity * row_bytes);
            const old_capacity = self.capacity();
            inline for (column_order) |field_index| {
                if (@sizeOf(field_types[field_index]) > 0) {
                    @memcpy(
                        columnPtr(new_bytes.ptr, new_capacity, field_index)[0..self.length],
                        columnPtr(self.bytes, old_capacity, field_index)[0..self.length],
                    );
                }
            }
            if (!self.isBorrowed() and old_capacity != 0) {
                gpa.free(self.bytes[0 .. @as(usize, old_capacity) * row_bytes]);
            }
            self.bytes = new_bytes.ptr;
            self.capacity_and_mark = @intCast(new_capacity);
        }

        /// Unsupported. A bounded append fails when there is no spare capacity,
        /// which would report a borrowed list as full instead of growing it out of
        /// the memory it borrows.
        pub fn appendBounded(_: *Self, _: T) error{OutOfMemory}!void {
            @compileError("SafeMultiList does not support bounded appends: a borrowed list must grow by copying. Use append, or ensureUnusedCapacity with appendAssumeCapacity.");
        }

        /// Unsupported, for the same reason as `appendBounded`.
        pub fn addOneBounded(_: *Self) error{OutOfMemory}!usize {
            @compileError("SafeMultiList does not support bounded appends: a borrowed list must grow by copying. Use append, or ensureUnusedCapacity with appendAssumeCapacity.");
        }

        /// Unsupported, for the same reason as `appendBounded`.
        pub fn insertBounded(_: *Self, _: usize, _: T) error{OutOfMemory}!void {
            @compileError("SafeMultiList does not support bounded inserts: a borrowed list must grow by copying.");
        }

        /// Unsupported. Freeing spare capacity would hand borrowed memory to the
        /// allocator.
        pub fn shrinkAndFree(_: *Self, _: Allocator, _: usize) void {
            @compileError("SafeMultiList does not support shrinkAndFree: a borrowed list's memory is not the allocator's. Use shrinkRetainingCapacity.");
        }

        /// Unsupported, for the same reason as `shrinkAndFree`.
        pub fn clearAndFree(_: *Self, _: Allocator) void {
            @compileError("SafeMultiList does not support clearAndFree: a borrowed list's memory is not the allocator's. Use clearRetainingCapacity, then deinit.");
        }

        /// Unsupported. Handing the columns to the caller as owned memory would
        /// pass borrowed memory to the allocator when the caller frees it.
        pub fn toOwnedSlice(_: *Self) void {
            @compileError("SafeMultiList does not support toOwnedSlice: a borrowed list's columns are not owned memory. Use clone.");
        }

        /// Unsupported. Setting an exact capacity below the borrowed length, or
        /// reallocating in place, is wrong for a borrowed list.
        pub fn setCapacity(_: *Self, _: Allocator, _: usize) void {
            @compileError("SafeMultiList does not support setCapacity: use ensureTotalCapacity, which copies a borrowed list on growth.");
        }

        /// An iterator over all the indices in this list.
        pub const Iterator = struct {
            array: *const Self,
            len: u32,
            current: Idx,

            pub fn next(self: *Iterator) ?T {
                const cur_idx = self.current;
                const cur_int = @backingInt(cur_idx);
                if (self.len == cur_int) {
                    return null;
                }
                self.current = @fromBackingInt(@intCast(cur_int + 1));
                return self.array.get(cur_idx);
            }

            pub fn count(self: *Iterator) u32 {
                return self.len - @backingInt(self.current);
            }
        };

        /// Iterate over the elements in a span
        pub fn iterRange(self: *const Self, range: Range) Iterator {
            return Iterator{
                .array = self,
                .len = @backingInt(range.start) + range.count,
                .current = range.start,
            };
        }

        /// Iterator over all indices in this list.
        pub fn iterIndices(self: *const Self) IndexIterator(Idx) {
            return .{ .end = @intCast(self.len()), .current = 0 };
        }

        /// Append this list's live rows to `writer` in the column layout at
        /// `capacity == len`, and return the byte offset the columns start at.
        ///
        /// Only live rows are written: spare capacity never reaches the output, so the
        /// serialized bytes and their length are a function of the list's contents alone
        /// rather than of the allocation history that produced its capacity.
        ///
        /// The input is not mutated. A column whose element bytes are already fully
        /// defined is gathered straight from the list; a column that needs padding
        /// scrubbed records its canonicalization operation for the write boundary.
        fn writeCompactedColumns(
            list: *const Self,
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!usize {
            try writer.padToAlignment(allocator, @alignOf(T));
            const data_offset = writer.total_bytes;
            const live_rows: usize = list.length;
            if (live_rows == 0) return data_offset;

            inline for (column_order) |field_index| {
                const FieldType = field_types[field_index];
                if (@sizeOf(FieldType) > 0) {
                    const column = columnPtr(list.bytes, list.capacity(), field_index);
                    // An integer offset keeps a first column that starts at byte zero
                    // representable, and lets the assertions below name where the column
                    // actually landed.
                    const column_offset = try writer.appendSlicePodZeroedOffset(allocator, @as([]const FieldType, column[0..live_rows]));
                    // The column lands `len * columnBytesBefore` into the compacted
                    // output, with no alignment padding inserted between columns.
                    std.debug.assert(column_offset == data_offset + live_rows * columnBytesBefore(field_index));
                    std.debug.assert(writer.total_bytes - column_offset == live_rows * @sizeOf(FieldType));
                }
            }
            return data_offset;
        }

        /// Serialized representation of a SafeMultiList
        /// Uses extern struct to guarantee consistent field layout across optimization levels.
        pub const Serialized = extern struct {
            offset: i64,
            len: u64,
            capacity: u64,

            /// One relocatable base pointer (`offset`) fixed up per `deserializeInto`, counted by
            /// `artifact_serialize.relocatablePointerCount`.
            pub const serialized_relocatable_pointers: usize = 1;

            /// The element type whose bytes are serialized (see `SafeList.Serialized`).
            pub const SerializedElement = T;

            /// L-10: reject an `(offset, len, capacity)` that a multi-list cannot
            /// hold or whose columns (`capacity` rows each, the region `serialize`
            /// writes) reach outside the `backing_len`-byte buffer.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                if (self.len == 0) return;
                if (self.capacity > max_rows or self.len > self.capacity) return error.CorruptArtifact;
                const span_bytes = std.math.mul(u64, self.capacity, row_bytes) catch return error.CorruptArtifact;
                try validateRelocatedSpan(@alignOf(T), self.offset, span_bytes, backing_len);
            }

            /// Serialize a SafeMultiList into this Serialized struct, appending data to
            /// the writer through `writeCompactedColumns`: live rows only, in
            /// column order, at `capacity == len`.
            pub fn serialize(
                self: *Serialized,
                safe_multi_list: *const Self,
                allocator: Allocator,
                writer: *CompactWriter,
            ) Allocator.Error!void {
                const data_offset = try writeCompactedColumns(safe_multi_list, allocator, writer);

                self.offset = @intCast(data_offset);
                self.len = safe_multi_list.length;
                self.capacity = safe_multi_list.length;
            }

            /// A borrowed list whose columns are the serialized columns in the
            /// buffer at `base`, laid out at the serialized capacity. The list reads
            /// and writes those columns in place, copies them on its first growth,
            /// and never frees them.
            pub fn deserializeInto(self: *const Serialized, base: usize) Self {
                if (self.len == 0) return .{};
                std.debug.assert(self.capacity <= max_rows and self.len <= self.capacity);
                return .{
                    .bytes = @ptrFromInt(base +% @as(usize, @intCast(self.offset))),
                    .length = @intCast(self.len),
                    .capacity_and_mark = @as(u32, @intCast(self.capacity)) | borrowed_mark,
                };
            }

            /// Deserialize into a SafeMultiList value with fresh memory allocation.
            /// The returned SafeMultiList owns its memory.
            pub fn deserializeWithCopy(self: *const Serialized, base: usize, gpa: Allocator) Allocator.Error!Self {
                const borrowed = self.deserializeInto(base);
                return borrowed.clone(gpa);
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

test "SafeList iterator counts and visits its bounded domain" {
    var list = SafeList(u32){};
    defer list.deinit(testing.allocator);
    var empty = list.iter();
    try testing.expectEqual(@as(u32, 0), empty.count());
    try testing.expectEqual(@as(?u32, null), empty.next());
    _ = try list.append(testing.allocator, 7);
    _ = try list.append(testing.allocator, 9);
    var items = list.iter();
    try testing.expectEqual(@as(u32, 2), items.count());
    try testing.expectEqual(@as(?u32, 7), items.next());
    try testing.expectEqual(@as(u32, 1), items.count());
    try testing.expectEqual(@as(?u32, 9), items.next());
    try testing.expectEqual(@as(?u32, null), items.next());
    try testing.expectEqual(@as(u32, 0), items.count());
}

test "SafeRange.empty has a defined start that addresses no row" {
    // An empty range reaches persisted columns, so its start is a fixed index
    // past every row a list can hold rather than an undefined placeholder.
    const Range = SafeList(u8).Range;
    try testing.expectEqual(@as(u32, std.math.maxInt(u32)), @backingInt(Range.empty().start));
    try testing.expectEqual(@as(u32, std.math.maxInt(u32)), @backingInt(SafeMultiList(struct { a: u8, b: u32 }).Range.empty().start));
    try testing.expectEqual(Range.empty(), Range.empty());
}

test "SafeList(u8) appendSlice" {
    const gpa = testing.allocator;

    var list = SafeList(u8){};
    defer list.deinit(gpa);

    const rangeA = try list.appendSlice(gpa, &[_]u8{ 'a', 'b', 'c', 'd' });
    try testing.expectEqual(0, @backingInt(rangeA.start));
    try testing.expectEqual(4, @backingInt(rangeA.end()));

    const rangeB = try list.appendSlice(gpa, &[_]u8{ 'd', 'e', 'f', 'g' });
    try testing.expectEqual(4, @backingInt(rangeB.start));
    try testing.expectEqual(8, @backingInt(rangeB.end()));
}

test "SafeList(u8) sliceRange" {
    const gpa = testing.allocator;

    var list = SafeList(u8){};
    defer list.deinit(gpa);

    const rangeA = try list.appendSlice(gpa, &[_]u8{ 'a', 'b', 'c', 'd' });
    const sliceA = list.sliceRange(rangeA);
    try testing.expectEqual('a', sliceA[0]);
    try testing.expectEqual('d', sliceA[3]);

    const rangeB = SafeList(u8).Range{ .start = @fromBackingInt(@intCast(2)), .count = 2 };
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
    try testing.expectEqual(0, @backingInt(rangeA.start));
    try testing.expectEqual(3, @backingInt(rangeA.end()));

    const rangeB = try multilist.appendSlice(gpa, &[_]Struct{ .{ .num = 400, .char = 'd' }, .{ .num = 500, .char = 'e' }, .{ .num = 600, .char = 'f' } });
    try testing.expectEqual(3, @backingInt(rangeB.start));
    try testing.expectEqual(6, @backingInt(rangeB.end()));
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

    const range_b = StructMultiList.Range{ .start = @fromBackingInt(@intCast(1)), .count = 1 };
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
    try testing.expectEqual(@as(usize, 0), @backingInt(added_range.start));

    // Create an empty range at the end (start=5, end=5 for a list of length 5)
    const empty_range = StructMultiList.Range{ .start = @fromBackingInt(@intCast(5)), .count = 0 };
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
        try testing.expectEqual(@as(usize, 0), @backingInt(container_idx));

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
    try testing.expectEqual(@as(usize, 0), @backingInt(orig_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(orig_idx1));
    try testing.expectEqual(@as(usize, 2), @backingInt(orig_idx2));
    try testing.expectEqual(@as(usize, 3), @backingInt(orig_idx3));

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
    try testing.expectEqual(@as(u32, 200), deserialized.get(@as(Idx, @fromBackingInt(@intCast(1)))).*);
    try testing.expectEqual(@as(u32, 300), deserialized.get(@as(Idx, @fromBackingInt(@intCast(2)))).*);
    try testing.expectEqual(@as(u32, 400), deserialized.get(@as(Idx, @fromBackingInt(@intCast(3)))).*);
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
    try testing.expectEqual(@as(usize, 0), @backingInt(u8_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(u8_idx1));
    try testing.expectEqual(@as(usize, 2), @backingInt(u8_idx2));

    // 2. SafeList(u16) - 2 byte alignment
    var list_u16 = try SafeList(u16).initCapacity(gpa, 2);
    defer list_u16.deinit(gpa);
    const u16_idx0 = try list_u16.append(gpa, 1000);
    const u16_idx1 = try list_u16.append(gpa, 2000);
    try testing.expectEqual(@as(usize, 0), @backingInt(u16_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(u16_idx1));

    // 3. SafeList(u32) - 4 byte alignment
    var list_u32 = try SafeList(u32).initCapacity(gpa, 4);
    defer list_u32.deinit(gpa);
    const u32_idx0 = try list_u32.append(gpa, 100_000);
    const u32_idx1 = try list_u32.append(gpa, 200_000);
    const u32_idx2 = try list_u32.append(gpa, 300_000);
    const u32_idx3 = try list_u32.append(gpa, 400_000);
    try testing.expectEqual(@as(usize, 0), @backingInt(u32_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(u32_idx1));
    try testing.expectEqual(@as(usize, 2), @backingInt(u32_idx2));
    try testing.expectEqual(@as(usize, 3), @backingInt(u32_idx3));

    // 4. SafeList(u64) - 8 byte alignment
    var list_u64 = try SafeList(u64).initCapacity(gpa, 2);
    defer list_u64.deinit(gpa);
    const u64_idx0 = try list_u64.append(gpa, 10_000_000_000);
    const u64_idx1 = try list_u64.append(gpa, 20_000_000_000);
    try testing.expectEqual(@as(usize, 0), @backingInt(u64_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(u64_idx1));

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
    try testing.expectEqual(@as(usize, 0), @backingInt(struct_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(struct_idx1));

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
    try testing.expectEqual(@as(u8, 20), deser_u8.get(@as(U8Idx, @fromBackingInt(@intCast(1)))).*);
    try testing.expectEqual(@as(u8, 30), deser_u8.get(@as(U8Idx, @fromBackingInt(@intCast(2)))).*);

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
    try testing.expectEqual(@as(u16, 2000), deser_u16.get(@as(U16Idx, @fromBackingInt(@intCast(1)))).*);

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
    try testing.expectEqual(@as(u32, 200_000), deser_u32.get(@as(U32Idx, @fromBackingInt(@intCast(1)))).*);
    try testing.expectEqual(@as(u32, 300_000), deser_u32.get(@as(U32Idx, @fromBackingInt(@intCast(2)))).*);
    try testing.expectEqual(@as(u32, 400_000), deser_u32.get(@as(U32Idx, @fromBackingInt(@intCast(3)))).*);

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
    try testing.expectEqual(@as(u64, 20_000_000_000), deser_u64.get(@as(U64Idx, @fromBackingInt(@intCast(1)))).*);

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

    const item1 = deser_struct.get(@as(StructIdx, @fromBackingInt(@intCast(1))));
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
                try testing.expectEqual(i, @backingInt(idx));
            }

            // Also create a second list with different data
            var list2 = SafeList(T){};
            defer list2.deinit(gpa);

            i = 0;
            while (i < length) : (i += 1) {
                // Use smaller values to avoid overflow for smaller integer types
                const multiplier: T = if (T == u8) 10 else if (T == u16) 1000 else 100000;
                const idx = try list2.append(gpa, @as(T, @intCast(i + 1)) * multiplier);
                try testing.expectEqual(i, @backingInt(idx));
            }

            // Create a u8 list to add between them (to test alignment)
            var list_u8 = SafeList(u8){};
            defer list_u8.deinit(gpa);
            const list_u8_idx = try list_u8.append(gpa, 42);
            try testing.expectEqual(@as(usize, 0), @backingInt(list_u8_idx));

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
                const actual = d1.get(@fromBackingInt(@intCast(i))).*;
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
                const actual = d2.get(@fromBackingInt(@intCast(i))).*;
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
    try testing.expectEqual(@as(usize, 0), @backingInt(orig_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(orig_idx1));
    try testing.expectEqual(@as(usize, 2), @backingInt(orig_idx2));
    try testing.expectEqual(@as(usize, 3), @backingInt(orig_idx3));

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

    const second_idx: Idx = @fromBackingInt(@intCast(1));
    try testing.expectEqual(@as(u32, 200), deserialized.get(second_idx).id);
    try testing.expectEqual(@as(u64, 2000), deserialized.get(second_idx).value);
    try testing.expectEqual(false, deserialized.get(second_idx).flag);
    try testing.expectEqual(@as(u8, 20), deserialized.get(second_idx).data);

    const third_idx: Idx = @fromBackingInt(@intCast(2));
    try testing.expectEqual(@as(u32, 300), deserialized.get(third_idx).id);
    try testing.expectEqual(@as(u64, 3000), deserialized.get(third_idx).value);
    try testing.expectEqual(true, deserialized.get(third_idx).flag);
    try testing.expectEqual(@as(u8, 30), deserialized.get(third_idx).data);

    const fourth_idx: Idx = @fromBackingInt(@intCast(3));
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
            try testing.expectEqual(i, @backingInt(idx));
        }

        // Verify we have extra capacity that shouldn't be serialized
        try testing.expect(list.capacity() >= length + 5);

        // Add another list to test alignment between lists
        var list2 = SafeMultiList(TestType){};
        defer list2.deinit(gpa);
        if (length > 0) {
            const list2_idx = try list2.append(gpa, .{ .a = 255, .b = 999999, .c = 888888888 });
            try testing.expectEqual(@as(usize, 0), @backingInt(list2_idx));
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
            const item = d1.get(@fromBackingInt(@intCast(i)));
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
                inline for (@typeInfo(TestType).@"struct".field_names, @typeInfo(TestType).@"struct".field_types, 0..) |field_name, field_type, fi| {
                    const field_type_info = @typeInfo(field_type);
                    const value = if (field_type_info == .int)
                        @as(field_type, @intCast(@min(i * (fi + 1) + 1, std.math.maxInt(field_type))))
                    else if (field_type_info == .float)
                        @as(field_type, @floatFromInt(i * (fi + 1) + 1))
                    else if (field_type_info == .bool)
                        @as(field_type, (i + fi) % 2 == 0)
                    else
                        @compileError("Unsupported field type in TestType: " ++ @typeName(field_type));
                    @field(item, field_name) = value;
                }
                const idx = try list.append(gpa, item);
                try testing.expectEqual(i, @backingInt(idx));
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
                const original_item = list.get(@fromBackingInt(@intCast(i)));
                const deser_item = deserialized.get(@fromBackingInt(@intCast(i)));

                inline for (@typeInfo(TestType).@"struct".field_names) |field_name| {
                    try testing.expectEqual(@field(original_item, field_name), @field(deser_item, field_name));
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
            try testing.expectEqual(i, @backingInt(idx));
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
        while (i < len) : (i += 1) oracle.appendAssumeCapacity(original.get(@fromBackingInt(@intCast(@as(u32, @intCast(i))))));
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
            const item = deserialized.get(@fromBackingInt(@intCast(@as(u32, @intCast(i)))));
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
    try testing.expect(list.capacity() >= 50);
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
    try testing.expectEqual(@as(usize, 0), deserialized.capacity());
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
    try testing.expectEqual(@as(usize, 0), @backingInt(orig_idx0));
    try testing.expectEqual(@as(usize, 1), @backingInt(orig_idx1));
    try testing.expectEqual(@as(usize, 2), @backingInt(orig_idx2));

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
    const range = StructMultiList.Range{ .start = @fromBackingInt(@intCast(2)), .count = 2 };
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
    const range = StructMultiList.Range{ .start = @fromBackingInt(@intCast(2)), .count = 0 };
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
    const range = StructMultiList.Range{ .start = @fromBackingInt(@intCast(1)), .count = 3 };
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
    var iter_mid = multilist.iterRange(.{ .start = @fromBackingInt(@intCast(3)), .count = 2 });
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

    try testing.expect(grown.capacity() != oversized.capacity());

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
    const backing_bytes = std.MultiArrayList(Row).capacityInBytes(list.capacity());
    @memset(list.bytes[0..backing_bytes], 0xA7);
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
    @memcpy(before, list.bytes[0..backing_bytes]);

    const buffer = try serializeMultiListToBuffer(gpa, Row, &list);
    defer gpa.free(buffer);

    // Every byte of the source backing store is unchanged, live rows included.
    try testing.expectEqualSlices(u8, before, list.bytes[0..backing_bytes]);

    // ...while the serialized copy is scrubbed: no poison byte reaches the output.
    try testing.expect(std.mem.findScalar(u8, buffer, 0xA7) == null);

    // And the values survive the scrub.
    const serialized: *const SafeMultiList(Row).Serialized = @ptrCast(@alignCast(buffer.ptr));
    const loaded = serialized.deserializeInto(@intFromPtr(buffer.ptr));
    try testing.expectEqual(@as(u32, row_count), loaded.len());
    for (0..row_count) |i| {
        try testing.expectEqualDeep(
            list.get(@fromBackingInt(@intCast(@as(u32, @intCast(i))))),
            loaded.get(@fromBackingInt(@intCast(@as(u32, @intCast(i))))),
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
    try testing.expectEqual(@as(usize, 6), loaded.capacity());
    for (0..6) |i| {
        const expected = list.get(@fromBackingInt(@intCast(@as(u32, @intCast(i)))));
        try testing.expectEqualDeep(expected, loaded.get(@fromBackingInt(@intCast(@as(u32, @intCast(i))))));
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
            try testing.expectEqual(@as(u32, @intCast(0x1000 + i)), loaded.get(@fromBackingInt(@intCast(@as(u32, @intCast(i))))).*);
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
            const row = loaded.get(@fromBackingInt(@intCast(@as(u32, @intCast(i)))));
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

test "SafeList is a pointer, a u32 length and a u32 capacity" {
    try testing.expectEqual(@as(usize, 16), @sizeOf(SafeList(u64)));
    try testing.expectEqual(@as(usize, 16), @sizeOf(SafeList(u8)));
}

test "SafeList borrowed: reads and in-place writes use the borrowed memory" {
    var backing = [_]u32{ 1, 2, 3 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    // A never-grown borrowed list frees nothing; the testing allocator would
    // report an invalid free of memory it does not own.
    defer list.deinit(testing.allocator);

    try testing.expectEqual(@as(u32, 0), list.capacity);
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3 }, list.items());
    list.set(@fromBackingInt(1), 20);
    try testing.expectEqual(@as(u32, 20), backing[1]);
    try testing.expectEqual(@as(*u32, &backing[2]), list.get(@fromBackingInt(2)));
}

test "SafeList borrowed: append copies into owned memory and leaves the borrowed bytes alone" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 1, 2, 3 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    defer list.deinit(gpa);

    const idx = try list.append(gpa, 4);
    try testing.expectEqual(@as(u32, 3), @backingInt(idx));
    try testing.expect(list.capacity >= 4);
    try testing.expect(list.ptr != @as([*]u32, &backing));
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3, 4 }, list.items());

    // Writes after the growth land in the owned copy.
    list.set(.first, 100);
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3 }, &backing);
}

test "SafeList borrowed: ensureTotalCapacity below, at and above the length" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 1, 2, 3, 4 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    defer list.deinit(gpa);

    try list.ensureTotalCapacity(gpa, 2);
    try testing.expectEqual(@as(u32, 0), list.capacity);
    try testing.expectEqual(@as([*]u32, &backing), list.ptr);

    try list.ensureTotalCapacity(gpa, 4);
    try testing.expectEqual(@as(u32, 0), list.capacity);
    try testing.expectEqual(@as([*]u32, &backing), list.ptr);

    try list.ensureTotalCapacityPrecise(gpa, 4);
    try testing.expectEqual(@as(u32, 0), list.capacity);

    try list.ensureTotalCapacity(gpa, 5);
    try testing.expect(list.capacity >= 5);
    try testing.expect(list.ptr != @as([*]u32, &backing));
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3, 4 }, list.items());
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3, 4 }, &backing);
}

test "SafeList borrowed: clear then append grows into owned memory" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 1, 2, 3 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    defer list.deinit(gpa);

    list.clearRetainingCapacity();
    try testing.expectEqual(@as(u64, 0), list.len());
    try testing.expectEqual(@as(u32, 0), list.capacity);

    _ = try list.append(gpa, 9);
    try testing.expect(list.capacity >= 1);
    try testing.expectEqualSlices(u32, &.{9}, list.items());
    try testing.expectEqualSlices(u32, &.{ 1, 2, 3 }, &backing);
}

test "SafeList borrowed: shrink keeps the list borrowed" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 1, 2, 3 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    defer list.deinit(gpa);

    list.shrinkRetainingCapacity(1);
    try testing.expectEqual(@as(u32, 0), list.capacity);
    try testing.expectEqual(@as([*]u32, &backing), list.ptr);
    try testing.expectEqual(@as(?u32, 1), list.pop());
    try testing.expectEqual(@as(?u32, null), list.pop());
}

test "SafeList borrowed: clone owns a copy" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 5, 6 };
    const list = SafeList(u32).fromBorrowedSlice(&backing);

    var cloned = try list.clone(gpa);
    defer cloned.deinit(gpa);
    try testing.expect(cloned.capacity >= 2);
    try testing.expect(cloned.ptr != @as([*]u32, &backing));
    cloned.set(.first, 50);
    try testing.expectEqualSlices(u32, &.{ 50, 6 }, cloned.items());
    try testing.expectEqualSlices(u32, &.{ 5, 6 }, &backing);
}

test "SafeList borrowed: toOwnedSlice copies" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 7, 8 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    const owned = try list.toOwnedSlice(gpa);
    defer gpa.free(owned);
    try testing.expect(owned.ptr != @as([*]u32, &backing));
    try testing.expectEqualSlices(u32, &.{ 7, 8 }, owned);
    try testing.expectEqual(@as(u64, 0), list.len());
}

test "SafeList borrowed: insert and orderedRemove" {
    const gpa = testing.allocator;
    var backing = [_]u32{ 1, 3 };
    var list = SafeList(u32).fromBorrowedSlice(&backing);
    defer list.deinit(gpa);

    // orderedRemove writes in place without growing.
    try testing.expectEqual(@as(u32, 1), list.orderedRemove(0));
    try testing.expectEqual(@as(u32, 0), list.capacity);
    try testing.expectEqualSlices(u32, &.{3}, list.items());

    try list.insert(gpa, 0, 2);
    try testing.expectEqualSlices(u32, &.{ 2, 3 }, list.items());
}

test "SafeList deserializeInto produces a borrowed list" {
    const gpa = testing.allocator;

    var original = SafeList(u32){};
    defer original.deinit(gpa);
    _ = try original.appendSlice(gpa, &.{ 10, 20, 30 });

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const serialized = try writer.appendAlloc(gpa, SafeList(u32).Serialized);
    try serialized.serialize(&original, gpa, &writer);

    const buffer = try gpa.alignedAlloc(u8, .of(SafeList(u32).Serialized), writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const serialized_ptr: *const SafeList(u32).Serialized = @ptrCast(@alignCast(buffer.ptr));
    var loaded = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));
    defer loaded.deinit(gpa);

    try testing.expectEqual(@as(u32, 0), loaded.capacity);
    try testing.expectEqualSlices(u32, &.{ 10, 20, 30 }, loaded.items());

    _ = try loaded.append(gpa, 40);
    try testing.expectEqualSlices(u32, &.{ 10, 20, 30, 40 }, loaded.items());
    const in_buffer: [*]const u32 = @ptrFromInt(@intFromPtr(buffer.ptr) + @as(usize, @intCast(serialized_ptr.offset)));
    try testing.expectEqualSlices(u32, &.{ 10, 20, 30 }, in_buffer[0..3]);
}

test "SafeList growth is amortized" {
    const gpa = testing.allocator;
    var list = SafeList(u32){};
    defer list.deinit(gpa);
    var reallocations: usize = 0;
    var last_ptr = list.ptr;
    for (0..10_000) |i| {
        _ = try list.append(gpa, @intCast(i));
        if (list.ptr != last_ptr) {
            reallocations += 1;
            last_ptr = list.ptr;
        }
    }
    try testing.expect(reallocations < 40);
    for (list.items(), 0..) |item, i| try testing.expectEqual(@as(u32, @intCast(i)), item);
}

const BorrowRow = struct { wide: u64, narrow: u8, mid: u32 };

/// Serialize `rows` as a SafeMultiList into a fresh aligned buffer and return
/// the buffer with the serialized header at its start.
fn serializedMultiListBuffer(gpa: Allocator, rows: []const BorrowRow) (Allocator.Error || error{BufferTooSmall})![]align(16) u8 {
    var original = SafeMultiList(BorrowRow){};
    defer original.deinit(gpa);
    _ = try original.appendSlice(gpa, rows);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const serialized = try writer.appendAlloc(gpa, SafeMultiList(BorrowRow).Serialized);
    try serialized.serialize(&original, gpa, &writer);

    const buffer = try gpa.alignedAlloc(u8, .@"16", writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);
    return buffer;
}

fn loadBorrowed(buffer: []align(16) u8) SafeMultiList(BorrowRow) {
    const serialized: *const SafeMultiList(BorrowRow).Serialized = @ptrCast(buffer.ptr);
    return serialized.deserializeInto(@intFromPtr(buffer.ptr));
}

const borrow_rows = [_]BorrowRow{
    .{ .wide = 10, .narrow = 1, .mid = 100 },
    .{ .wide = 20, .narrow = 2, .mid = 200 },
    .{ .wide = 30, .narrow = 3, .mid = 300 },
};

test "SafeMultiList is a pointer, a u32 length and a u32 capacity word" {
    try testing.expectEqual(@as(usize, 16), @sizeOf(SafeMultiList(BorrowRow)));
}

test "SafeMultiList borrowed: deserializeInto marks the list borrowed and reads in place" {
    const gpa = testing.allocator;
    const buffer = try serializedMultiListBuffer(gpa, &borrow_rows);
    defer gpa.free(buffer);

    var list = loadBorrowed(buffer);
    // A never-grown borrowed list frees nothing; the testing allocator would
    // report an invalid free of the buffer it does not own through the list.
    defer list.deinit(gpa);

    try testing.expect(list.isBorrowed());
    try testing.expectEqual(@as(u32, 3), list.capacity());
    try testing.expectEqualSlices(u64, &.{ 10, 20, 30 }, list.field(.wide));
    try testing.expectEqualSlices(u8, &.{ 1, 2, 3 }, list.field(.narrow));
    try testing.expectEqualSlices(u32, &.{ 100, 200, 300 }, list.field(.mid));
    try testing.expect(@intFromPtr(list.field(.wide).ptr) >= @intFromPtr(buffer.ptr));
    try testing.expect(@intFromPtr(list.field(.wide).ptr) < @intFromPtr(buffer.ptr) + buffer.len);

    // In-place writes land in the borrowed memory.
    list.set(@fromBackingInt(1), .{ .wide = 21, .narrow = 22, .mid = 23 });
    const reloaded = loadBorrowed(buffer);
    try testing.expectEqual(BorrowRow{ .wide = 21, .narrow = 22, .mid = 23 }, reloaded.get(@fromBackingInt(1)));
}

test "SafeMultiList borrowed: append copies every column and leaves the borrowed bytes alone" {
    const gpa = testing.allocator;
    const buffer = try serializedMultiListBuffer(gpa, &borrow_rows);
    defer gpa.free(buffer);
    const before = try gpa.dupe(u8, buffer);
    defer gpa.free(before);

    var list = loadBorrowed(buffer);
    defer list.deinit(gpa);

    const idx = try list.append(gpa, .{ .wide = 40, .narrow = 4, .mid = 400 });
    try testing.expectEqual(@as(u32, 3), @backingInt(idx));
    try testing.expect(!list.isBorrowed());
    try testing.expect(list.capacity() >= 4);
    try testing.expectEqualSlices(u64, &.{ 10, 20, 30, 40 }, list.field(.wide));
    try testing.expectEqualSlices(u8, &.{ 1, 2, 3, 4 }, list.field(.narrow));
    try testing.expectEqualSlices(u32, &.{ 100, 200, 300, 400 }, list.field(.mid));

    list.set(.first, .{ .wide = 0, .narrow = 0, .mid = 0 });
    try testing.expectEqualSlices(u8, before, buffer);
}

test "SafeMultiList borrowed: ensureTotalCapacity below, at and above the length" {
    const gpa = testing.allocator;
    const buffer = try serializedMultiListBuffer(gpa, &borrow_rows);
    defer gpa.free(buffer);

    var list = loadBorrowed(buffer);
    defer list.deinit(gpa);
    const borrowed_bytes = list.bytes;

    try list.ensureTotalCapacity(gpa, 2);
    try testing.expect(list.isBorrowed());
    try list.ensureTotalCapacity(gpa, 3);
    try testing.expect(list.isBorrowed());
    try testing.expectEqual(borrowed_bytes, list.bytes);

    try list.ensureTotalCapacity(gpa, 4);
    try testing.expect(!list.isBorrowed());
    try testing.expect(list.capacity() >= 4);
    try testing.expect(list.bytes != borrowed_bytes);
    try testing.expectEqualSlices(u32, &.{ 100, 200, 300 }, list.field(.mid));
}

test "SafeMultiList borrowed: shrink then append writes in place, clear then grow copies" {
    const gpa = testing.allocator;
    const buffer = try serializedMultiListBuffer(gpa, &borrow_rows);
    defer gpa.free(buffer);

    var list = loadBorrowed(buffer);
    defer list.deinit(gpa);
    const borrowed_bytes = list.bytes;

    list.shrinkRetainingCapacity(2);
    _ = try list.append(gpa, .{ .wide = 99, .narrow = 9, .mid = 999 });
    try testing.expect(list.isBorrowed());
    try testing.expectEqual(borrowed_bytes, list.bytes);
    try testing.expectEqualSlices(u64, &.{ 10, 20, 99 }, list.field(.wide));

    list.clearRetainingCapacity();
    try testing.expect(list.isBorrowed());
    for (0..4) |i| _ = try list.append(gpa, .{ .wide = i, .narrow = @intCast(i), .mid = @intCast(i) });
    try testing.expect(!list.isBorrowed());
    try testing.expectEqualSlices(u64, &.{ 0, 1, 2, 3 }, list.field(.wide));
}

test "SafeMultiList borrowed: clone owns a copy" {
    const gpa = testing.allocator;
    const buffer = try serializedMultiListBuffer(gpa, &borrow_rows);
    defer gpa.free(buffer);

    const list = loadBorrowed(buffer);
    var cloned = try list.clone(gpa);
    defer cloned.deinit(gpa);
    try testing.expect(!cloned.isBorrowed());
    cloned.set(.first, .{ .wide = 1, .narrow = 1, .mid = 1 });
    try testing.expectEqual(borrow_rows[0], list.get(.first));
    try testing.expectEqual(BorrowRow{ .wide = 1, .narrow = 1, .mid = 1 }, cloned.get(.first));
}

test "SafeMultiList serialized form rejects more rows than a multi-list holds" {
    const Serialized = SafeMultiList(BorrowRow).Serialized;
    const too_many: Serialized = .{ .offset = 0, .len = 1 << 31, .capacity = 1 << 31 };
    try testing.expectError(error.CorruptArtifact, too_many.validateRelocations(std.math.maxInt(u64)));
    const len_past_capacity: Serialized = .{ .offset = 0, .len = 4, .capacity = 3 };
    try testing.expectError(error.CorruptArtifact, len_past_capacity.validateRelocations(std.math.maxInt(u64)));
}

test "SafeMultiList growth past 2^31 - 1 rows is allocation failure" {
    var list = SafeMultiList(BorrowRow){};
    try testing.expectError(error.OutOfMemory, list.ensureTotalCapacity(testing.failing_allocator, 1 << 31));
}
