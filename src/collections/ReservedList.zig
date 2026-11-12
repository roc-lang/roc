//! Append-only columns of unknown final length whose growth never copies.
//!
//! A `ReservedList` whose estimated size reaches `reserve_threshold_bytes`
//! lives on one virtual reservation (`zeroed_pages.reserve`): appending bumps
//! the length, the pages ahead of it are committed as it reaches them, and
//! the column never moves while it fits its reservation. A column estimated
//! below the threshold grows through its allocator like an ordinary list and
//! moves onto a reservation once, when it crosses the threshold, which costs
//! the same one copy as a doubling. A column that outgrows its reservation
//! reserves a larger one and moves once. Those are size policies with no
//! failure path: every column holds the same entries whichever storage backs
//! it.
//!
//! No reader relies on a column's address across appends. `seal` returns the
//! unused tail of a reservation to the kernel and forbids further growth.
//! Serialization writes the logical length only, and `deserializeInto`
//! returns a column borrowing the serialized bytes: it reads and writes them
//! in place, copies them into storage of its own on its first growth, and
//! never frees them.
//!
//! `ReservedColumns(T)` keeps one `ReservedList` per field of the struct `T`,
//! so each column grows on its own reservation without copying, and offers
//! field-slice access for readers.

const std = @import("std");
const storage_work = @import("storage_work.zig");
const zeroed_pages = @import("zeroed_pages.zig");
const safe_list = @import("safe_list.zig");
const CompactWriter = @import("CompactWriter.zig");

const Allocator = std.mem.Allocator;
const Density = zeroed_pages.Density;

/// A column estimated at fewer bytes than this grows through its allocator
/// and moves onto a reservation only if it crosses this size. This initial
/// threshold must be validated on the compiler corpus before migration.
pub const reserve_threshold_bytes: usize = 256 * 1024;

/// A reservation made on crossing the threshold, or to replace an outgrown
/// one, holds this many times the bytes the column needs.
const reservation_growth_factor: usize = 4;

/// The most entries a column holds: indices and range counts are `u32`, and
/// `maxInt(u32)` is the empty-range start.
const max_len: usize = std.math.maxInt(u32);

const Storage = enum {
    /// Grows through the allocator passed to each growing call.
    allocated,
    /// Lives on `reservation`.
    reserved,
    /// Borrows serialized bytes, which it owns none of; the first growth
    /// copies the entries into storage of the column's own.
    borrowed,
};

/// An append-only list of `T` with `SafeList(T)`'s typed indices and ranges.
pub fn ReservedList(comptime T: type, comptime density: Density) type {
    if (@sizeOf(T) == 0) @compileError("ReservedList needs a sized element type");
    if (@alignOf(T) > std.heap.page_size_min) @compileError("ReservedList elements must fit the host page alignment");
    return struct {
        const Self = @This();

        /// The live entries.
        items: []T = &.{},
        /// Entries that fit before the next growth step. For a reservation,
        /// the committed watermark, so the append check is one comparison.
        capacity: usize = 0,
        storage: Storage = .allocated,
        reservation: zeroed_pages.Reservation = .{},
        /// Whether `seal` forbade further growth.
        sealed: bool = false,

        /// The typed index of an entry; the same type `SafeList(T)` uses.
        pub const Idx = safe_list.SafeList(T).Idx;
        /// A typed range of entries; the same type `SafeList(T)` uses.
        pub const Range = safe_list.SafeList(T).Range;
        /// A range with a mandatory first entry.
        pub const NonEmptyRange = safe_list.SafeList(T).NonEmptyRange;
        /// A non-type-safe slice of entries.
        pub const Slice = []T;

        /// An empty column sized for about `estimated_len` entries.
        pub fn init(gpa: Allocator, estimated_len: usize) Allocator.Error!Self {
            if (estimated_len > max_len) return error.OutOfMemory;
            const bytes = std.math.mul(usize, estimated_len, @sizeOf(T)) catch return error.OutOfMemory;
            if (bytes >= reserve_threshold_bytes) {
                var self: Self = .{ .storage = .reserved };
                self.reservation = try zeroed_pages.reserve(bytes, density);
                return self;
            }
            var self: Self = .{};
            if (estimated_len != 0) {
                const block = try gpa.alloc(T, estimated_len);
                self.items = block[0..0];
                self.capacity = block.len;
            }
            return self;
        }

        /// Initialize a column with immediately usable capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!Self {
            var result = try Self.init(gpa, capacity);
            errdefer result.deinit(gpa);
            try result.ensureTotalCapacity(gpa, capacity);
            return result;
        }

        /// Free the column.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            switch (self.storage) {
                .allocated => if (self.capacity != 0) gpa.free(self.items.ptr[0..self.capacity]),
                .reserved => self.reservation.release(),
                .borrowed => {},
            }
            self.* = .{};
        }

        /// The number of entries.
        pub inline fn len(self: *const Self) u64 {
            return self.items.len;
        }

        /// Make room for `additional` more entries.
        pub inline fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional: usize) Allocator.Error!void {
            std.debug.assert(!self.sealed);
            const needed = std.math.add(usize, self.items.len, additional) catch return error.OutOfMemory;
            if (needed <= self.capacity) return;
            return self.grow(gpa, needed);
        }

        /// Make room for `total` entries.
        pub inline fn ensureTotalCapacity(self: *Self, gpa: Allocator, total: usize) Allocator.Error!void {
            std.debug.assert(!self.sealed);
            if (total <= self.capacity) return;
            return self.grow(gpa, total);
        }

        noinline fn grow(self: *Self, gpa: Allocator, needed: usize) Allocator.Error!void {
            std.debug.assert(!self.sealed);
            if (needed > max_len) return error.OutOfMemory;
            storage_work.add(.growth_allocations, 1);
            const needed_bytes = std.math.mul(usize, needed, @sizeOf(T)) catch return error.OutOfMemory;
            switch (self.storage) {
                .borrowed => {
                    if (needed_bytes >= reserve_threshold_bytes) {
                        storage_work.add(.reservation_crossings, 1);
                        return self.moveToReservation(gpa, needed_bytes);
                    }
                    var new_capacity: usize = @max(self.items.len, 8);
                    while (new_capacity < needed) new_capacity *|= 2;
                    new_capacity = @min(new_capacity, max_len);
                    const fresh = try gpa.alloc(T, new_capacity);
                    @memcpy(fresh[0..self.items.len], self.items);
                    storage_work.add(.bytes_copied, self.items.len * @sizeOf(T));
                    self.items = fresh[0..self.items.len];
                    self.capacity = fresh.len;
                    self.storage = .allocated;
                },
                .allocated => {
                    if (needed_bytes >= reserve_threshold_bytes) {
                        storage_work.add(.reservation_crossings, 1);
                        return self.moveToReservation(gpa, needed_bytes);
                    }
                    var new_capacity = @max(self.capacity, 8);
                    while (new_capacity < needed) new_capacity *|= 2;
                    new_capacity = @min(new_capacity, max_len);
                    const old_len = self.items.len;
                    const old_block = self.items.ptr[0..self.capacity];
                    const block = if (gpa.remap(old_block, new_capacity)) |remapped| remapped else blk: {
                        const fresh = try gpa.alloc(T, new_capacity);
                        @memcpy(fresh[0..old_len], self.items);
                        storage_work.add(.bytes_copied, old_len * @sizeOf(T));
                        gpa.free(old_block);
                        break :blk fresh;
                    };
                    self.items = block[0..old_len];
                    self.capacity = block.len;
                },
                .reserved => {
                    if (needed_bytes > self.reservation.bytes.len and self.reservation.bytes.len != 0) {
                        if (comptime zeroed_pages.can_remap_reservations) {
                            try self.reservation.relocateLinux(try reservationBytes(needed_bytes), density);
                            // A following commit can fail after the mapping moved.
                            self.refreshFromReservation();
                        } else {
                            storage_work.add(.relocations, 1);
                            return self.moveToReservation(gpa, needed_bytes);
                        }
                    }
                    if (self.reservation.bytes.len == 0) {
                        self.reservation = try zeroed_pages.reserve(try reservationBytes(needed_bytes), density);
                    }
                    try self.reservation.commitTo(needed_bytes);
                    self.refreshFromReservation();
                },
            }
        }

        fn reservationBytes(needed_bytes: usize) Allocator.Error!usize {
            // Leave room for the primitive's alignment rounding and over-reservation.
            const ceiling = std.math.maxInt(usize) - 2 * zeroed_pages.huge_page_size;
            if (needed_bytes > ceiling) return error.OutOfMemory;
            return @min(needed_bytes *| reservation_growth_factor, ceiling);
        }

        fn moveToReservation(self: *Self, gpa: Allocator, needed_bytes: usize) Allocator.Error!void {
            const reserve_bytes = try reservationBytes(needed_bytes);
            var next = try zeroed_pages.reserve(reserve_bytes, density);
            errdefer next.release();
            try next.commitTo(needed_bytes);
            const live: []T = @as([*]T, @ptrCast(@alignCast(next.bytes.ptr)))[0..self.items.len];
            @memcpy(live, self.items);
            storage_work.add(.bytes_copied, self.items.len * @sizeOf(T));
            switch (self.storage) {
                .allocated => if (self.capacity != 0) gpa.free(self.items.ptr[0..self.capacity]),
                .reserved => self.reservation.release(),
                .borrowed => {},
            }
            self.storage = .reserved;
            self.reservation = next;
            self.refreshFromReservation();
        }

        fn refreshFromReservation(self: *Self) void {
            const base: [*]T = @ptrCast(@alignCast(self.reservation.bytes.ptr));
            self.items = base[0..self.items.len];
            self.capacity = @min(self.reservation.committed / @sizeOf(T), max_len);
        }

        /// Append `item`.
        pub inline fn append(self: *Self, gpa: Allocator, item: T) Allocator.Error!Idx {
            try self.ensureUnusedCapacity(gpa, 1);
            return self.appendAssumeCapacity(item);
        }

        /// Append `item` into capacity reserved earlier.
        pub inline fn appendAssumeCapacity(self: *Self, item: T) Idx {
            std.debug.assert(!self.sealed);
            std.debug.assert(self.items.len < self.capacity);
            const index = self.items.len;
            self.items.len += 1;
            self.items[index] = item;
            return @fromBackingInt(@intCast(index));
        }

        /// Append every item of `items`.
        pub fn appendSlice(self: *Self, gpa: Allocator, new_items: []const T) Allocator.Error!Range {
            try self.ensureUnusedCapacity(gpa, new_items.len);
            return self.appendSliceAssumeCapacity(new_items);
        }

        /// Append every item of `items` into capacity reserved earlier.
        pub fn appendSliceAssumeCapacity(self: *Self, new_items: []const T) Range {
            std.debug.assert(!self.sealed);
            std.debug.assert(new_items.len <= self.capacity - self.items.len);
            const start = self.items.len;
            self.items.len += new_items.len;
            @memcpy(self.items[start..], new_items);
            return .{ .start = @fromBackingInt(@intCast(start)), .count = @intCast(new_items.len) };
        }

        /// Drop the entries past `new_len`, keeping their capacity. Entries
        /// appended again are written in full.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            std.debug.assert(new_len <= self.items.len);
            self.items.len = new_len;
        }

        /// A copy of this column in fresh storage sized for its length.
        pub fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
            var cloned = try Self.init(gpa, self.items.len);
            errdefer cloned.deinit(gpa);
            _ = try cloned.appendSlice(gpa, self.items);
            return cloned;
        }

        /// Return a reservation's unused tail to the kernel and forbid growth.
        pub fn seal(self: *Self) void {
            if (self.storage == .reserved and self.reservation.bytes.len != 0) {
                self.reservation.trim(self.items.len * @sizeOf(T));
                self.refreshFromReservation();
            }
            self.sealed = true;
        }

        /// A range from `start_int` to the end of the column.
        pub fn rangeToEnd(self: *const Self, start_int: u32) Range {
            std.debug.assert(start_int <= self.items.len);
            return .{ .start = @fromBackingInt(start_int), .count = @intCast(self.items.len - start_int) };
        }

        /// The entries of `range`.
        pub fn sliceRange(self: *const Self, range: Range) []T {
            if (range.count == 0) return &.{};
            const start: usize = @backingInt(range.start);
            return self.items[start..][0..range.count];
        }

        /// A pointer to entry `id`.
        pub inline fn get(self: *const Self, id: Idx) *T {
            return &self.items[@backingInt(id)];
        }

        /// Overwrite entry `id`.
        pub inline fn set(self: *const Self, id: Idx, value: T) void {
            self.items[@backingInt(id)] = value;
        }

        /// Iterates values while retaining only the owner and index bounds.
        pub const Iterator = struct {
            array: *const Self,
            len: u32,
            current: Idx,

            /// The next value, or null at the end.
            pub fn next(self: *Iterator) ?T {
                const index = @backingInt(self.current);
                if (index == self.len) return null;
                self.current = @fromBackingInt(index + 1);
                return self.array.items[index];
            }

            /// The remaining number of values.
            pub fn count(self: *const Iterator) u32 {
                return self.len - @backingInt(self.current);
            }
        };

        /// Iterate the values of a typed range.
        pub fn iterRange(self: *const Self, range: Range) Iterator {
            if (range.count == 0) return .{ .array = self, .len = 0, .current = .first };
            return .{ .array = self, .len = @backingInt(range.start) + range.count, .current = range.start };
        }

        /// Iterate over every index.
        pub fn iterIndices(self: *const Self) safe_list.IndexIterator(Idx) {
            return .{ .end = @intCast(self.items.len), .current = 0 };
        }

        /// The serialized form: the same layout as `SafeList(T).Serialized`,
        /// holding the logical length only.
        pub const Serialized = extern struct {
            offset: i64,
            len: u64,
            capacity: u64,

            /// One relocatable base pointer per `deserializeInto`.
            pub const serialized_relocatable_pointers: usize = 1;
            /// The element type whose bytes are serialized.
            pub const SerializedElement = T;

            /// Reject an extent that reaches outside the serialized buffer.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                if (self.len > max_len or self.capacity != self.len) return error.CorruptArtifact;
                const span_bytes = std.math.mul(u64, self.len, @sizeOf(T)) catch return error.CorruptArtifact;
                try safe_list.validateRelocatedSpan(@alignOf(T), self.offset, span_bytes, backing_len);
            }

            /// Write the live entries of `list` to `writer`.
            pub fn serialize(self: *Serialized, list: *const Self, allocator: Allocator, writer: *CompactWriter) Allocator.Error!void {
                const data_offset = try writer.appendSlicePodZeroedOffset(allocator, @as([]const T, list.items));
                self.offset = @intCast(data_offset);
                self.len = list.items.len;
                self.capacity = list.items.len;
            }

            /// A column borrowing the serialized entries: it reads and writes
            /// them in place and copies them on its first growth.
            pub fn deserializeInto(self: *const Serialized, base: usize) Self {
                if (self.len == 0) return .{ .storage = .borrowed };
                const items_ptr: [*]T = @ptrFromInt(base +% @as(usize, @intCast(self.offset)));
                const length: usize = @intCast(self.len);
                return .{ .items = items_ptr[0..length], .capacity = length, .storage = .borrowed };
            }

            /// An owned, growable copy of the serialized entries.
            pub fn deserializeWithCopy(self: *const Serialized, base: usize, gpa: Allocator) Allocator.Error!Self {
                const view = self.deserializeInto(base);
                return view.clone(gpa);
            }
        };
    };
}

/// One `ReservedList` per field of the struct `T`, each on its own storage, so
/// every column grows without copying. Readers keep field-slice access.
pub fn ReservedColumns(comptime T: type, comptime density: Density) type {
    const info = @typeInfo(T).@"struct";
    const field_names = info.field_names;
    const field_types = info.field_types;
    const Columns = blk: {
        var column_types: [field_names.len]type = undefined;
        for (field_types, &column_types) |field_type, *column_type| column_type.* = ReservedList(field_type, density);
        break :blk @Struct(.auto, null, field_names, &column_types, &@splat(.{}));
    };
    const SerializedColumns = blk: {
        var column_types: [field_names.len]type = undefined;
        for (field_types, &column_types) |field_type, *column_type| column_type.* = ReservedList(field_type, density).Serialized;
        break :blk @Struct(.@"extern", null, field_names, &column_types, &@splat(.{}));
    };
    return struct {
        const Self = @This();

        capacity: usize = 0,

        columns: Columns = blk: {
            var columns: Columns = undefined;
            for (field_names) |name| @field(columns, name) = .{};
            break :blk columns;
        },

        /// The typed index of a row; the same type `SafeMultiList(T)` uses.
        pub const Idx = safe_list.SafeMultiList(T).Idx;
        /// A typed range of rows; the same type `SafeMultiList(T)` uses.
        pub const Range = safe_list.SafeMultiList(T).Range;
        /// A field of `T`.
        pub const Field = std.meta.FieldEnum(T);

        /// Empty columns sized for about `estimated_len` rows. A failure
        /// frees every column made before it.
        pub fn init(gpa: Allocator, estimated_len: usize) Allocator.Error!Self {
            var self: Self = .{};
            errdefer self.deinit(gpa);
            inline for (field_names) |name| @field(self.columns, name) = try .init(gpa, estimated_len);
            self.refreshCapacity();
            return self;
        }

        /// Initialize every column with immediately usable capacity.
        pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!Self {
            var result = try Self.init(gpa, capacity);
            errdefer result.deinit(gpa);
            try result.ensureTotalCapacity(gpa, capacity);
            return result;
        }

        /// A range view which resolves field slices from the owning columns.
        pub const Slice = struct {
            list: *const Self,
            start: usize,
            len: usize,

            /// The live entries of one field in the range.
            pub inline fn items(self: Slice, comptime field_name: Field) []@FieldType(T, @tagName(field_name)) {
                return self.list.items(field_name)[self.start..][0..self.len];
            }

            /// One row in the range.
            pub inline fn get(self: Slice, index: usize) T {
                std.debug.assert(index < self.len);
                return self.list.get(@fromBackingInt(@intCast(self.start + index)));
            }
        };

        /// View the rows of a typed range without copying them.
        pub inline fn sliceRange(self: *const Self, range: Range) Slice {
            if (range.count == 0) return .{ .list = self, .start = 0, .len = 0 };
            const start: usize = @backingInt(range.start);
            std.debug.assert(start <= self.len() and range.count <= self.len() - start);
            return .{ .list = self, .start = start, .len = range.count };
        }

        /// A field slice, using the safe-list reader interface.
        pub inline fn field(self: *const Self, comptime field_name: Field) []@FieldType(T, @tagName(field_name)) {
            return self.items(field_name);
        }

        /// Free every column.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            inline for (field_names) |name| @field(self.columns, name).deinit(gpa);
            self.capacity = 0;
        }

        /// The number of rows.
        pub inline fn len(self: *const Self) u32 {
            return @intCast(@field(self.columns, field_names[0]).items.len);
        }

        /// The live entries of one column.
        pub inline fn items(self: *const Self, comptime field_name: Field) []@FieldType(T, @tagName(field_name)) {
            return @field(self.columns, @tagName(field_name)).items;
        }

        /// One field of row `idx`.
        pub inline fn fieldItem(self: *const Self, comptime field_name: Field, idx: Idx) @FieldType(T, @tagName(field_name)) {
            return self.items(field_name)[@backingInt(idx)];
        }

        /// Row `idx`.
        pub inline fn get(self: *const Self, idx: Idx) T {
            var row: T = undefined;
            inline for (field_names) |name| @field(row, name) = @field(self.columns, name).items[@backingInt(idx)];
            return row;
        }

        /// Overwrite row `idx`.
        pub inline fn set(self: *Self, idx: Idx, row: T) void {
            inline for (field_names) |name| @field(self.columns, name).items[@backingInt(idx)] = @field(row, name);
        }

        /// Make room in every column for `additional` more rows.
        pub inline fn ensureUnusedCapacity(self: *Self, gpa: Allocator, additional: usize) Allocator.Error!void {
            const needed = std.math.add(usize, self.len(), additional) catch return error.OutOfMemory;
            try self.ensureTotalCapacity(gpa, needed);
        }

        /// Make room in every column for `total` rows.
        pub inline fn ensureTotalCapacity(self: *Self, gpa: Allocator, total: usize) Allocator.Error!void {
            if (total <= self.capacity) return;
            try self.grow(gpa, total);
        }

        noinline fn grow(self: *Self, gpa: Allocator, total: usize) Allocator.Error!void {
            inline for (field_names) |name| try @field(self.columns, name).ensureTotalCapacity(gpa, total);
            self.refreshCapacity();
        }

        fn refreshCapacity(self: *Self) void {
            var capacity: usize = max_len;
            inline for (field_names) |name| capacity = @min(capacity, @field(self.columns, name).capacity);
            self.capacity = capacity;
        }

        /// Append `row`. Every column makes room before any column grows its
        /// length, so a failure leaves all columns at the same length.
        pub inline fn append(self: *Self, gpa: Allocator, row: T) Allocator.Error!Idx {
            try self.ensureUnusedCapacity(gpa, 1);
            return self.appendAssumeCapacity(row);
        }

        /// Append `row` into capacity reserved earlier.
        pub inline fn appendAssumeCapacity(self: *Self, row: T) Idx {
            const index = self.len();
            inline for (field_names) |name| _ = @field(self.columns, name).appendAssumeCapacity(@field(row, name));
            return @fromBackingInt(index);
        }

        /// Append every row of `rows`. No rows yield the range that starts at
        /// the first row and holds nothing, so empty ranges compare equal.
        pub fn appendSlice(self: *Self, gpa: Allocator, rows: []const T) Allocator.Error!Range {
            if (rows.len == 0) return .{ .start = .first, .count = 0 };
            try self.ensureUnusedCapacity(gpa, rows.len);
            const start = self.len();
            for (rows) |row| _ = self.appendAssumeCapacity(row);
            return .{ .start = @fromBackingInt(start), .count = @intCast(rows.len) };
        }

        /// Drop the rows past `new_len`, keeping their capacity.
        pub fn shrinkRetainingCapacity(self: *Self, new_len: usize) void {
            inline for (field_names) |name| @field(self.columns, name).shrinkRetainingCapacity(new_len);
        }

        /// A copy of these columns in fresh storage.
        pub fn clone(self: *const Self, gpa: Allocator) Allocator.Error!Self {
            var cloned: Self = .{};
            errdefer cloned.deinit(gpa);
            inline for (field_names) |name| @field(cloned.columns, name) = try @field(self.columns, name).clone(gpa);
            cloned.refreshCapacity();
            return cloned;
        }

        /// Seal every column.
        pub fn seal(self: *Self) void {
            inline for (field_names) |name| @field(self.columns, name).seal();
            self.refreshCapacity();
        }

        /// A range from `start_int` to the last row.
        pub fn rangeToEnd(self: *const Self, start_int: u32) Range {
            std.debug.assert(start_int <= self.len());
            return .{ .start = @fromBackingInt(start_int), .count = self.len() - start_int };
        }

        /// Iterates values while retaining only the owner and index bounds.
        pub const Iterator = struct {
            array: *const Self,
            len: u32,
            current: Idx,

            /// The next value, or null at the end.
            pub fn next(self: *Iterator) ?T {
                const index = @backingInt(self.current);
                if (index == self.len) return null;
                self.current = @fromBackingInt(index + 1);
                return self.array.get(@fromBackingInt(index));
            }

            /// The remaining number of values.
            pub fn count(self: *const Iterator) u32 {
                return self.len - @backingInt(self.current);
            }
        };

        /// Iterate the values of a typed range.
        pub fn iterRange(self: *const Self, range: Range) Iterator {
            if (range.count == 0) return .{ .array = self, .len = 0, .current = .first };
            return .{ .array = self, .len = @backingInt(range.start) + range.count, .current = range.start };
        }

        /// Iterate over every row index.
        pub fn iterIndices(self: *const Self) safe_list.IndexIterator(Idx) {
            return .{ .end = self.len(), .current = 0 };
        }

        /// The serialized form: every column in declared field order, each
        /// holding the logical length only.
        pub const Serialized = extern struct {
            columns: SerializedColumns,

            /// Reject a column extent that reaches outside the buffer.
            pub fn validateRelocations(self: *const Serialized, backing_len: u64) error{CorruptArtifact}!void {
                const row_count = @field(self.columns, field_names[0]).len;
                inline for (field_names) |name| {
                    const column = &@field(self.columns, name);
                    if (column.len != row_count) return error.CorruptArtifact;
                    try column.validateRelocations(backing_len);
                }
            }

            /// Write every column of `list` to `writer`.
            pub fn serialize(self: *Serialized, list: *const Self, allocator: Allocator, writer: *CompactWriter) Allocator.Error!void {
                inline for (field_names) |name| try @field(self.columns, name).serialize(&@field(list.columns, name), allocator, writer);
            }

            /// Columns borrowing the serialized columns.
            pub fn deserializeInto(self: *const Serialized, base: usize) Self {
                var result: Self = .{};
                inline for (field_names) |name| @field(result.columns, name) = @field(self.columns, name).deserializeInto(base);
                result.refreshCapacity();
                return result;
            }

            /// Owned, growable copies of the serialized columns.
            pub fn deserializeWithCopy(self: *const Serialized, base: usize, gpa: Allocator) Allocator.Error!Self {
                return self.deserializeInto(base).clone(gpa);
            }
        };
    };
}

const testing = std.testing;

test "a small column grows through its allocator and keeps every entry" {
    var list = try ReservedList(u32, .dense).init(testing.allocator, 4);
    defer list.deinit(testing.allocator);
    try testing.expectEqual(Storage.allocated, list.storage);
    for (0..1000) |i| _ = try list.append(testing.allocator, @intCast(i));
    for (list.items, 0..) |item, i| try testing.expectEqual(@as(u32, @intCast(i)), item);
}

test "a column crosses onto a reservation and past a commit chunk" {
    var list = try ReservedList(u64, .dense).init(testing.allocator, 16);
    defer list.deinit(testing.allocator);
    const count = (zeroed_pages.commit_chunk_bytes / @sizeOf(u64)) * 2 + 3;
    for (0..count) |i| _ = try list.append(testing.allocator, i * 3);
    try testing.expectEqual(Storage.reserved, list.storage);
    try testing.expectEqual(@as(u64, count), list.len());
    for (list.items, 0..) |item, i| try testing.expectEqual(@as(u64, i * 3), item);
}

test "a column past its reservation moves once and keeps its entries" {
    var list = try ReservedList(u8, .sparse).init(testing.allocator, reserve_threshold_bytes);
    defer list.deinit(testing.allocator);
    try testing.expectEqual(Storage.reserved, list.storage);
    const reserved = list.reservation.bytes.len;
    const relocations = storage_work.get(.relocations);
    for (0..reserved + 10) |i| _ = try list.append(testing.allocator, @truncate(i));
    if (storage_work.enabled) try testing.expect(storage_work.get(.relocations) > relocations);
    try testing.expect(list.reservation.bytes.len > reserved);
    for (list.items, 0..) |item, i| try testing.expectEqual(@as(u8, @truncate(i)), item);
}

test "shrinking and regrowing writes every entry" {
    var list = try ReservedList(u32, .dense).init(testing.allocator, 1 << 17);
    defer list.deinit(testing.allocator);
    for (0..100) |i| _ = try list.append(testing.allocator, @intCast(i));
    list.shrinkRetainingCapacity(10);
    for (10..20) |i| _ = try list.append(testing.allocator, @intCast(i + 1000));
    try testing.expectEqual(@as(u64, 20), list.len());
    try testing.expectEqual(@as(u32, 9), list.items[9]);
    try testing.expectEqual(@as(u32, 1010), list.items[10]);
}

test "sealing trims the reservation, and serialization round-trips the logical length" {
    var list = try ReservedList(u32, .dense).init(testing.allocator, 1 << 18);
    defer list.deinit(testing.allocator);
    for (0..5000) |i| _ = try list.append(testing.allocator, @intCast(i * 7));
    list.seal();
    try testing.expect(list.reservation.bytes.len < (1 << 20));
    try testing.expectEqual(@as(u32, 4999 * 7), list.items[4999]);

    const List = ReservedList(u32, .dense);
    var writer = CompactWriter.init();
    defer writer.deinit(testing.allocator);
    var header: List.Serialized = undefined;
    try header.serialize(&list, testing.allocator, &writer);
    try testing.expectEqual(@as(u64, 5000), header.len);
    try testing.expectEqual(@as(u64, 5000), header.capacity);
    const bytes = try testing.allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer testing.allocator.free(bytes);
    _ = try writer.writeToBuffer(bytes);
    try testing.expectEqual(@as(usize, 5000 * @sizeOf(u32)), bytes.len);
    try header.validateRelocations(bytes.len);
    var view = header.deserializeInto(@intFromPtr(bytes.ptr));
    defer view.deinit(testing.allocator);
    try testing.expectEqual(Storage.borrowed, view.storage);
    try testing.expectEqualSlices(u32, list.items, view.items);
    // A borrowed column copies on its first growth and leaves the bytes alone.
    const before = try testing.allocator.dupe(u8, bytes);
    defer testing.allocator.free(before);
    _ = try view.append(testing.allocator, 1);
    try testing.expectEqual(Storage.allocated, view.storage);
    try testing.expectEqualSlices(u32, list.items, view.items[0..5000]);
    try testing.expectEqualSlices(u8, before, bytes);
    var copied = try header.deserializeWithCopy(@intFromPtr(bytes.ptr), testing.allocator);
    defer copied.deinit(testing.allocator);
    _ = try copied.append(testing.allocator, 1);
    try testing.expectEqual(@as(u64, 5001), copied.len());
}

test "a clone holds the same entries in its own storage" {
    var list = try ReservedList(u16, .dense).init(testing.allocator, 3);
    defer list.deinit(testing.allocator);
    _ = try list.appendSlice(testing.allocator, &.{ 5, 6, 7 });
    var cloned = try list.clone(testing.allocator);
    defer cloned.deinit(testing.allocator);
    list.items[0] = 9;
    try testing.expectEqualSlices(u16, &.{ 5, 6, 7 }, cloned.items);
}

const Row = struct { wide: u64, narrow: u8, mid: u32 };

test "appending no rows yields the empty range at the first row" {
    var columns = try ReservedColumns(Row, .dense).init(testing.allocator, 2);
    defer columns.deinit(testing.allocator);
    _ = try columns.append(testing.allocator, .{ .wide = 1, .narrow = 2, .mid = 3 });
    const empty = try columns.appendSlice(testing.allocator, &.{});
    try testing.expectEqual(@as(u32, 0), @backingInt(empty.start));
    try testing.expectEqual(@as(u32, 0), empty.count);
    try testing.expectEqual(@as(usize, 0), columns.sliceRange(empty).len);
    try testing.expectEqual(@as(u32, 1), columns.len());
}

test "columns keep rows and field slices" {
    var columns = try ReservedColumns(Row, .dense).init(testing.allocator, 2);
    defer columns.deinit(testing.allocator);
    _ = try columns.append(testing.allocator, .{ .wide = 1, .narrow = 2, .mid = 3 });
    const second = try columns.append(testing.allocator, .{ .wide = 4, .narrow = 5, .mid = 6 });
    try testing.expectEqual(@as(u32, 2), columns.len());
    try testing.expectEqualSlices(u8, &.{ 2, 5 }, columns.items(.narrow));
    try testing.expectEqual(@as(u32, 6), columns.fieldItem(.mid, second));
    columns.set(second, .{ .wide = 7, .narrow = 8, .mid = 9 });
    try testing.expectEqual(Row{ .wide = 7, .narrow = 8, .mid = 9 }, columns.get(second));
}

test "an allocation failure at any column of an append leaves every column at the same length" {
    var fail_index: usize = 0;
    while (true) : (fail_index += 1) {
        var failing = std.testing.FailingAllocator.init(testing.allocator, .{ .fail_index = fail_index });
        const gpa = failing.allocator();
        var columns = ReservedColumns(Row, .dense).init(gpa, 1) catch continue;
        var appended: usize = 0;
        var failed = false;
        while (appended < 20) : (appended += 1) {
            _ = columns.append(gpa, .{ .wide = appended, .narrow = 1, .mid = 2 }) catch {
                failed = true;
                break;
            };
        }
        try testing.expectEqual(columns.items(.wide).len, columns.items(.narrow).len);
        try testing.expectEqual(columns.items(.wide).len, columns.items(.mid).len);
        try testing.expectEqual(appended, columns.items(.mid).len);
        columns.deinit(gpa);
        if (!failed) break;
    }
}

test "serialized columns reject inconsistent extents and row counts" {
    const List = ReservedList(u32, .dense);
    var single: List.Serialized = .{ .offset = 0, .len = 2, .capacity = 3 };
    try testing.expectError(error.CorruptArtifact, single.validateRelocations(16));
    single = .{ .offset = 0, .len = @as(u64, std.math.maxInt(u32)) + 1, .capacity = @as(u64, std.math.maxInt(u32)) + 1 };
    try testing.expectError(error.CorruptArtifact, single.validateRelocations(std.math.maxInt(u64)));
    const Columns = ReservedColumns(Row, .dense);
    var columns: Columns.Serialized = .{ .columns = .{
        .wide = .{ .offset = 0, .len = 2, .capacity = 2 },
        .narrow = .{ .offset = 16, .len = 1, .capacity = 1 },
        .mid = .{ .offset = 20, .len = 2, .capacity = 2 },
    } };
    try testing.expectError(error.CorruptArtifact, columns.validateRelocations(28));
    columns.columns.narrow.len = 2;
    columns.columns.narrow.capacity = 2;
    try columns.validateRelocations(28);
}

test "reserved columns report unrepresentable element extents as allocation failure" {
    if (@sizeOf(usize) != 4) return error.SkipZigTest;
    try testing.expectError(error.OutOfMemory, ReservedList(u64, .dense).init(testing.allocator, std.math.maxInt(u32)));
}

test "parallel columns serialize live rows in declared field order" {
    const Columns = ReservedColumns(Row, .dense);
    var original = try Columns.init(testing.allocator, 8);
    defer original.deinit(testing.allocator);
    _ = try original.append(testing.allocator, .{ .wide = 19, .narrow = 2, .mid = 41 });
    _ = try original.append(testing.allocator, .{ .wide = 23, .narrow = 3, .mid = 43 });
    original.seal();
    var writer = CompactWriter.init();
    defer writer.deinit(testing.allocator);
    var header: Columns.Serialized = undefined;
    try header.serialize(&original, testing.allocator, &writer);
    const bytes = try testing.allocator.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer testing.allocator.free(bytes);
    _ = try writer.writeToBuffer(bytes);
    try header.validateRelocations(bytes.len);
    try testing.expect(header.columns.wide.offset < header.columns.narrow.offset);
    try testing.expect(header.columns.narrow.offset < header.columns.mid.offset);
    var view = header.deserializeInto(@intFromPtr(bytes.ptr));
    defer view.deinit(testing.allocator);
    var copied = try header.deserializeWithCopy(@intFromPtr(bytes.ptr), testing.allocator);
    defer copied.deinit(testing.allocator);
    try testing.expectEqual(original.get(@fromBackingInt(1)), view.get(@fromBackingInt(1)));
    _ = try copied.append(testing.allocator, .{ .wide = 29, .narrow = 4, .mid = 47 });
    try testing.expectEqual(@as(u32, 3), copied.len());
}
