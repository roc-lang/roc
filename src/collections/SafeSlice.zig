//! A slice type that is extern-compatible and supports serialization/relocation.
//!
//! SafeSlice provides a low-level building block for types that need to:
//! 1. Be extern-compatible (for consistent memory layout across optimization levels)
//! 2. Support serialization and deserialization
//! 3. Support pointer relocation for shared memory scenarios
//!
//! This is used as the underlying storage for SafeList and can be used directly
//! for slice fields that need extern compatibility (e.g., source code buffers).

const std = @import("std");
const Allocator = std.mem.Allocator;
const CompactWriter = @import("CompactWriter.zig");

/// An extern-compatible slice type that can be serialized and relocated.
///
/// Unlike Zig's native `[]T` slice type, SafeSlice uses separate pointer and
/// length fields that are extern-compatible, making it safe to use in extern
/// structs where consistent field layout is required.
pub fn SafeSlice(comptime T: type) type {
    return extern struct {
        const Self = @This();

        /// Pointer to the first element, or undefined if len == 0.
        /// Using [*]T instead of ?[*]T for extern compatibility.
        ptr: [*]T,
        /// Number of elements in the slice.
        len: u64,
        /// Allocated capacity (may be >= len).
        capacity: u64,

        /// Create an empty SafeSlice.
        pub fn empty() Self {
            return .{
                .ptr = undefined,
                .len = 0,
                .capacity = 0,
            };
        }

        /// Create a SafeSlice from a Zig slice.
        /// Note: This does NOT take ownership - the caller must ensure the
        /// backing memory outlives this SafeSlice.
        pub fn fromSlice(slice: []const T) Self {
            if (slice.len == 0) {
                return empty();
            }
            return .{
                .ptr = @constCast(slice.ptr),
                .len = slice.len,
                .capacity = slice.len,
            };
        }

        /// Create a SafeSlice from a mutable Zig slice.
        pub fn fromSliceMut(slice: []T) Self {
            if (slice.len == 0) {
                return empty();
            }
            return .{
                .ptr = slice.ptr,
                .len = slice.len,
                .capacity = slice.len,
            };
        }

        /// Convert to a Zig slice.
        pub fn toSlice(self: Self) []T {
            if (self.len == 0) {
                return &[_]T{};
            }
            return self.ptr[0..@intCast(self.len)];
        }

        /// Convert to a const Zig slice.
        pub fn toSliceConst(self: Self) []const T {
            if (self.len == 0) {
                return &[_]T{};
            }
            return self.ptr[0..@intCast(self.len)];
        }

        /// Initialize with the specified capacity.
        pub fn initCapacity(gpa: Allocator, cap: usize) Allocator.Error!Self {
            if (cap == 0) {
                return empty();
            }
            const slice = try gpa.alloc(T, cap);
            return .{
                .ptr = slice.ptr,
                .len = 0,
                .capacity = cap,
            };
        }

        /// Free the underlying memory.
        pub fn deinit(self: *Self, gpa: Allocator) void {
            if (self.capacity > 0) {
                gpa.free(self.ptr[0..@intCast(self.capacity)]);
            }
            self.* = empty();
        }

        /// Append an item, growing if necessary.
        pub fn append(self: *Self, gpa: Allocator, item: T) Allocator.Error!void {
            try self.ensureCapacity(gpa, self.len + 1);
            self.ptr[@intCast(self.len)] = item;
            self.len += 1;
        }

        /// Append an item assuming capacity is sufficient.
        pub fn appendAssumeCapacity(self: *Self, item: T) void {
            std.debug.assert(self.len < self.capacity);
            self.ptr[@intCast(self.len)] = item;
            self.len += 1;
        }

        /// Append a slice of items.
        pub fn appendSlice(self: *Self, gpa: Allocator, items: []const T) Allocator.Error!void {
            try self.ensureCapacity(gpa, self.len + items.len);
            const dest = self.ptr[@intCast(self.len)..][0..items.len];
            @memcpy(dest, items);
            self.len += items.len;
        }

        /// Ensure capacity for at least `min_capacity` items.
        pub fn ensureCapacity(self: *Self, gpa: Allocator, min_capacity: u64) Allocator.Error!void {
            if (self.capacity >= min_capacity) return;

            const new_capacity = @max(min_capacity, self.capacity * 2);
            if (self.capacity == 0) {
                const slice = try gpa.alloc(T, @intCast(new_capacity));
                self.ptr = slice.ptr;
                self.capacity = new_capacity;
            } else {
                const old_slice = self.ptr[0..@intCast(self.capacity)];
                const new_slice = try gpa.realloc(old_slice, @intCast(new_capacity));
                self.ptr = new_slice.ptr;
                self.capacity = new_capacity;
            }
        }

        /// Alias for ensureCapacity for API compatibility.
        pub const ensureTotalCapacity = ensureCapacity;

        /// Ensure capacity for at least `self.len + count` items.
        pub fn ensureUnusedCapacity(self: *Self, gpa: Allocator, count: u64) Allocator.Error!void {
            return self.ensureCapacity(gpa, self.len + count);
        }

        /// Ensure capacity is exactly `exact_capacity` items.
        /// Used for hash tables and other structures that need precise sizing.
        pub fn ensureTotalCapacityPrecise(self: *Self, gpa: Allocator, exact_capacity: u64) Allocator.Error!void {
            if (self.capacity >= exact_capacity) return;

            if (self.capacity == 0) {
                const slice = try gpa.alloc(T, @intCast(exact_capacity));
                self.ptr = slice.ptr;
                self.capacity = exact_capacity;
            } else {
                const old_slice = self.ptr[0..@intCast(self.capacity)];
                const new_slice = try gpa.realloc(old_slice, @intCast(exact_capacity));
                self.ptr = new_slice.ptr;
                self.capacity = exact_capacity;
            }
        }

        /// Set the length directly. Use with caution - caller must ensure
        /// the data in [0..new_len] is valid.
        pub fn setLen(self: *Self, new_len: u64) void {
            std.debug.assert(new_len <= self.capacity);
            self.len = new_len;
        }

        /// Clear all items while retaining the allocated capacity.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.len = 0;
        }

        /// Pop and return the last item.
        pub fn pop(self: *Self) T {
            std.debug.assert(self.len > 0);
            self.len -= 1;
            return self.ptr[@intCast(self.len)];
        }

        /// Get a slice of the full allocated capacity (not just len).
        /// Useful for initializing with @memset.
        pub fn sliceCapacity(self: Self) []T {
            if (self.capacity == 0) {
                return &[_]T{};
            }
            return self.ptr[0..@intCast(self.capacity)];
        }

        /// Get an element by index.
        pub fn get(self: Self, index: usize) *T {
            std.debug.assert(index < self.len);
            return &self.ptr[index];
        }

        /// Get an element by index (const).
        pub fn getConst(self: Self, index: usize) T {
            std.debug.assert(index < self.len);
            return self.ptr[index];
        }

        /// Serialize this SafeSlice, appending data to the writer.
        /// The serialized form stores an offset instead of a pointer.
        pub fn serialize(
            self: *Self,
            source: *const Self,
            allocator: Allocator,
            writer: *CompactWriter,
        ) Allocator.Error!void {
            // Pad to the alignment of the slice elements.
            try writer.padToAlignment(allocator, @alignOf(T));

            // Now that we are aligned, this is the correct offset for our data.
            const data_offset = writer.total_bytes;

            // Append the raw data without further padding.
            if (source.len > 0) {
                try writer.iovecs.append(allocator, .{
                    .iov_base = @ptrCast(source.ptr),
                    .iov_len = @intCast(source.len * @sizeOf(T)),
                });
                writer.total_bytes += @intCast(source.len * @sizeOf(T));
            }

            self.ptr = @ptrFromInt(data_offset);
            self.len = source.len;
            self.capacity = source.len;
        }

        /// Deserialize this SafeSlice in place.
        /// Converts the stored offset back to a pointer using the given base offset.
        pub fn deserialize(self: *Self, offset: i64) void {
            if (self.len == 0) {
                self.* = empty();
            } else {
                // The ptr field currently holds an offset, not a real pointer.
                // Add the base offset to get the actual address.
                const serialized_offset: i64 = @intCast(@intFromPtr(self.ptr));
                const new_addr: usize = @intCast(serialized_offset + offset);
                self.ptr = @ptrFromInt(new_addr);
            }
        }

        /// Add the given offset to the pointer address.
        /// Used for relocating shared memory mappings.
        pub fn relocate(self: *Self, offset: isize) void {
            if (self.capacity == 0) return;

            const old_addr: isize = @intCast(@intFromPtr(self.ptr));
            const new_addr: usize = @intCast(old_addr + offset);
            self.ptr = @ptrFromInt(new_addr);
        }
    };
}

test "SafeSlice basic operations" {
    const gpa = std.testing.allocator;

    var slice = try SafeSlice(u32).initCapacity(gpa, 4);
    defer slice.deinit(gpa);

    try slice.append(gpa, 1);
    try slice.append(gpa, 2);
    try slice.append(gpa, 3);

    try std.testing.expectEqual(@as(u64, 3), slice.len);
    try std.testing.expectEqual(@as(u32, 1), slice.getConst(0));
    try std.testing.expectEqual(@as(u32, 2), slice.getConst(1));
    try std.testing.expectEqual(@as(u32, 3), slice.getConst(2));
}

test "SafeSlice fromSlice" {
    const data = [_]u8{ 'h', 'e', 'l', 'l', 'o' };
    const slice = SafeSlice(u8).fromSlice(&data);

    try std.testing.expectEqual(@as(u64, 5), slice.len);
    try std.testing.expectEqualStrings("hello", slice.toSliceConst());
}
