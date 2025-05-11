const std = @import("std");
const math = std.math;

const Allocator = std.mem.Allocator;

/// A pointer that's intended to be serialized to/from disk.
///
/// It gets serialized to disk as a null pointer, but then rehydrated
/// into a useful pointer again after loading the cache file from disk.
///
/// For consistency on disk, it's always 64 bytes in size. It wraps a
/// pointer on 64-bit targets, and wraps a pointer plus 4 bytes of padding
/// on 32-bit targets. This is a no-op on 64-bit targets, but it means that
/// it doesn't matter whether a cached file is generated on a 32-bit or 64-bit
/// target, its bytes will be identical and it will work on the other target.
pub fn CachedPtr(comptime T: type) type {
    return struct {
        ptr: *T align(8),
        padding: [8 - @sizeOf(usize)]u8 align(8) = [_]u8{0} ** (8 - @sizeOf(usize)),

        pub const zero = init(null);

        comptime {
            std.debug.assert(@sizeOf(@This()) == 8);
            std.debug.assert(@alignOf(@This()) == 8);
        }

        pub fn init(ptr: *T) @This() {
            return .{
                .ptr = ptr,
            };
        }

        /// Example of how to use this:
        /// - You have a pointer followed by N bytes worth of data
        /// - You pass in that pointer and N
        /// - You get back a PaddedPtr whose address is the next (properly-aligned) address after those N bytes.
        pub fn after(comptime P: type, before_ptr: *P, bytes_after: usize) @This() {
            const unaligned = @intFromPtr(@as(*u8, @ptrCast(before_ptr + 1))) + bytes_after;
            const align_minus_1 = @alignOf(T) - 1;
            const offset = (unaligned + align_minus_1) & ~align_minus_1;

            return .{
                .ptr = @as(*T, @ptrFromInt(offset)),
            };
        }

        /// If the current capacity is less than `new_capacity`, this function will
        /// modify the array so that it can hold at least `new_capacity` items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureTotalCapacity(self: *CachedPtr(T), new_capacity: usize) Allocator.Error!void {
            if (@sizeOf(T) == 0) {
                self.capacity = math.maxInt(usize);
                return;
            }

            if (self.capacity >= new_capacity) return;

            const better_capacity = ArrayListAlignedUnmanaged(T, alignment).growCapacity(self.capacity, new_capacity);
            return self.ensureTotalCapacityPrecise(better_capacity);
        }
    };
}
