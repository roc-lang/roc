//! Builtin string operations and data structures for the Roc runtime.
//!
//! This module provides the core implementation of Roc's Str type, including
//! operations for string manipulation, Unicode handling, formatting, and
//! memory management. It defines the RocStr structure and associated functions
//! that are called from compiled Roc code to handle string operations efficiently.
const builtins = @import("builtins");
const std = @import("std");

const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;
const UpdateMode = builtins.utils.UpdateMode;
const utils = builtins.utils;
const ascii = std.ascii;
const mem = std.mem;
const unicode = std.unicode;
const testing = std.testing;
const rcNone = builtins.utils.rcNone;

const InPlace = enum(u8) {
    InPlace,
    Clone,
};

const MASK_ISIZE: isize = std.math.minInt(isize);
const MASK: usize = @as(usize, @bitCast(MASK_ISIZE));
const SEAMLESS_SLICE_BIT: usize = MASK;

/// TODO
pub const SMALL_STR_MAX_LENGTH = SMALL_STRING_SIZE - 1;

const SMALL_STRING_SIZE = @sizeOf(RocStr);

fn init_blank_small_string(comptime n: usize) [n]u8 {
    var prime_list: [n]u8 = undefined;

    var i = 0;
    while (i < n) : (i += 1) {
        prime_list[i] = 0;
    }

    return prime_list;
}

/// TODO: Document RocStr struct.
pub const RocStr = extern struct {
    bytes: ?[*]u8,
    length: usize,
    // For big strs, contains the capacity.
    // For seamless slices contains the pointer to the original allocation.
    // This pointer is to the first character of the original string.
    // Note we storing an allocation pointer, the pointer must be right shifted by one.
    capacity_or_alloc_ptr: usize,

    pub const alignment = @alignOf(usize);

    pub inline fn empty() RocStr {
        return RocStr{
            .length = 0,
            .bytes = null,
            .capacity_or_alloc_ptr = MASK,
        };
    }

    // This clones the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(
        bytes_ptr: [*]const u8,
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        var result = RocStr.allocate(length, roc_ops);
        @memcpy(result.asU8ptrMut()[0..length], bytes_ptr[0..length]);

        return result;
    }

    // This requires that the list is non-null.
    // It also requires that start and count define a slice that does not go outside the bounds of the list.
    pub fn fromSubListUnsafe(list: RocList, start: usize, count: usize, update_mode: UpdateMode) RocStr {
        const start_byte = @as([*]u8, @ptrCast(list.bytes)) + start;
        if (list.isSeamlessSlice()) {
            return RocStr{
                .bytes = start_byte,
                .length = count | SEAMLESS_SLICE_BIT,
                .capacity_or_alloc_ptr = list.capacity_or_alloc_ptr & (~SEAMLESS_SLICE_BIT),
            };
        } else if (start == 0 and (update_mode == .InPlace or list.isUnique())) {
            // Rare case, we can take over the original list.
            return RocStr{
                .bytes = start_byte,
                .length = count,
                .capacity_or_alloc_ptr = list.capacity_or_alloc_ptr, // This is guaranteed to be a proper capacity.
            };
        } else {
            // Create seamless slice pointing to the list.
            return RocStr{
                .bytes = start_byte,
                .length = count | SEAMLESS_SLICE_BIT,
                .capacity_or_alloc_ptr = @intFromPtr(list.bytes) >> 1,
            };
        }
    }

    pub fn isSeamlessSlice(self: RocStr) bool {
        return !self.isSmallStr() and @as(isize, @bitCast(self.length)) < 0;
    }

    pub fn fromSlice(
        slice: []const u8,
        roc_ops: *RocOps,
    ) RocStr {
        return RocStr.init(slice.ptr, slice.len, roc_ops);
    }

    /// Create a RocStr from raw bytes and length, using RocOps for allocation
    /// This is a convenience wrapper around the existing functionality
    pub fn fromBytes(
        bytes: [*]const u8,
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        return RocStr.init(bytes, length, roc_ops);
    }

    fn allocateBig(
        length: usize,
        capacity: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const first_element = builtins.utils.allocateWithRefcount(
            capacity,
            @sizeOf(usize),
            false,
            roc_ops,
        );

        return RocStr{
            .bytes = first_element,
            .length = length,
            .capacity_or_alloc_ptr = capacity,
        };
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // May have a larger capacity than the length.
    pub fn allocate(
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const element_width = 1;
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = utils.calculateCapacity(0, length, element_width);
            return RocStr.allocateBig(length, capacity, roc_ops);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;

            return string;
        }
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // Will have the exact same capacity as length if it is not a small string.
    pub fn allocateExact(
        length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            return RocStr.allocateBig(length, length, roc_ops);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;

            return string;
        }
    }

    // This returns all ones if the list is a seamless slice.
    // Otherwise, it returns all zeros.
    // This is done without branching for optimization purposes.
    pub fn seamlessSliceMask(self: RocStr) usize {
        return @as(usize, @bitCast(@as(isize, @bitCast(self.length)) >> (@bitSizeOf(isize) - 1)));
    }

    // returns a pointer to the original allocation.
    // This pointer points to the first element of the allocation.
    // The pointer is to just after the refcount.
    // For big strings, it just returns their bytes pointer.
    // For seamless slices, it returns the pointer stored in capacity_or_alloc_ptr.
    // This does not return a valid value if the input is a small string.
    pub fn getAllocationPtr(self: RocStr) ?[*]u8 {
        const str_alloc_ptr = @intFromPtr(self.bytes);
        const slice_alloc_ptr = self.capacity_or_alloc_ptr << 1;
        const slice_mask = self.seamlessSliceMask();
        const alloc_ptr = (str_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);
        return @as(?[*]u8, @ptrFromInt(alloc_ptr));
    }

    pub fn incref(self: RocStr, n: usize) void {
        if (!self.isSmallStr()) {
            const alloc_ptr = self.getAllocationPtr();
            if (alloc_ptr != null) {
                const isizes: [*]isize = @as([*]isize, @ptrCast(@alignCast(alloc_ptr)));
                builtins.utils.increfRcPtrC(@as(*isize, @ptrCast(isizes - 1)), @as(isize, @intCast(n)));
            }
        }
    }

    pub fn decref(
        self: RocStr,
        roc_ops: *RocOps,
    ) void {
        if (!self.isSmallStr()) {
            builtins.utils.decref(self.getAllocationPtr(), self.capacity_or_alloc_ptr, RocStr.alignment, false, roc_ops);
        }
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        // If they are byte-for-byte equal, they're definitely equal!
        if (self.bytes == other.bytes and self.length == other.length) {
            return true;
        }

        const self_len = self.len();
        const other_len = other.len();

        // If their lengths are different, they're definitely unequal.
        if (self_len != other_len) {
            return false;
        }

        // Now we have to look at the string contents
        const self_bytes = self.asU8ptr();
        const other_bytes = other.asU8ptr();
        // TODO: we can make an optimization like memcmp does in glibc.
        // We can check the min shared alignment 1, 2, 4, or 8.
        // Then do a copy at that alignment before falling back on one byte at a time.
        // Currently we have to be unaligned because slices can be at any alignment.
        var b: usize = 0;
        while (b < self_len) : (b += 1) {
            if (self_bytes[b] != other_bytes[b]) {
                return false;
            }
        }

        return true;
    }

    pub fn clone(
        str: RocStr,
        roc_ops: *RocOps,
    ) RocStr {
        if (str.isSmallStr()) {
            // just return the bytes
            return str;
        } else {
            const new_str = RocStr.allocateBig(str.length, str.length, roc_ops);

            var old_bytes: [*]u8 = @as([*]u8, @ptrCast(str.bytes));
            var new_bytes: [*]u8 = @as([*]u8, @ptrCast(new_str.bytes));

            @memcpy(new_bytes[0..str.length], old_bytes[0..str.length]);

            return new_str;
        }
    }

    pub fn reallocate(
        self: RocStr,
        new_length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const element_width = 1;
        const old_capacity = self.getCapacity();

        if (self.isSmallStr() or self.isSeamlessSlice() or !self.isUnique()) {
            return self.reallocateFresh(new_length, roc_ops);
        }

        if (self.bytes) |source_ptr| {
            if (old_capacity > new_length) {
                var output = self;
                output.setLen(new_length);
                return output;
            }
            const new_capacity = builtins.utils.calculateCapacity(old_capacity, new_length, element_width);
            const new_source = builtins.utils.unsafeReallocate(
                source_ptr,
                RocStr.alignment,
                old_capacity,
                new_capacity,
                element_width,
                false,
            );

            return RocStr{ .bytes = new_source, .length = new_length, .capacity_or_alloc_ptr = new_capacity };
        }
        return self.reallocateFresh(new_length, roc_ops);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocStr,
        new_length: usize,
        roc_ops: *RocOps,
    ) RocStr {
        const old_length = self.len();

        const element_width = 1;
        const result_is_big = new_length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = builtins.utils.calculateCapacity(0, new_length, element_width);
            var result = RocStr.allocateBig(new_length, capacity, roc_ops);

            // transfer the memory

            const source_ptr = self.asU8ptr();
            const dest_ptr = result.asU8ptrMut();

            @memcpy(dest_ptr[0..old_length], source_ptr[0..old_length]);
            @memset(dest_ptr[old_length..new_length], 0);

            self.decref(roc_ops);

            return result;
        } else {
            var string = RocStr.empty();

            // I believe taking this reference on the stack here is important for correctness.
            // Doing it via a method call seemed to cause issues
            const dest_ptr = @as([*]u8, @ptrCast(&string));
            dest_ptr[@sizeOf(RocStr) - 1] = @as(u8, @intCast(new_length)) | 0b1000_0000;

            const source_ptr = self.asU8ptr();

            @memcpy(dest_ptr[0..old_length], source_ptr[0..old_length]);
            @memset(dest_ptr[old_length..new_length], 0);

            self.decref(roc_ops);

            return string;
        }
    }

    pub fn isSmallStr(self: RocStr) bool {
        return @as(isize, @bitCast(self.capacity_or_alloc_ptr)) < 0;
    }

    fn asArray(self: RocStr) [@sizeOf(RocStr)]u8 {
        const as_ptr = @as([*]const u8, @ptrCast(&self));
        const slice = as_ptr[0..@sizeOf(RocStr)];

        return slice.*;
    }

    pub fn len(self: RocStr) usize {
        if (self.isSmallStr()) {
            return self.asArray()[@sizeOf(RocStr) - 1] ^ 0b1000_0000;
        } else {
            return self.length & (~SEAMLESS_SLICE_BIT);
        }
    }

    pub fn setLen(self: *RocStr, length: usize) void {
        if (self.isSmallStr()) {
            self.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;
        } else {
            self.length = length | (SEAMLESS_SLICE_BIT & self.length);
        }
    }

    pub fn getCapacity(self: RocStr) usize {
        if (self.isSmallStr()) {
            return SMALL_STR_MAX_LENGTH;
        } else if (self.isSeamlessSlice()) {
            return self.length & (~SEAMLESS_SLICE_BIT);
        } else {
            return self.capacity_or_alloc_ptr;
        }
    }

    // This does a small string check, but no bounds checking whatsoever!
    pub fn getUnchecked(self: RocStr, index: usize) u8 {
        if (self.isSmallStr()) {
            return self.asArray()[index];
        } else {
            const bytes = self.bytes orelse unreachable;

            return bytes[index];
        }
    }

    pub fn isEmpty(self: RocStr) bool {
        return self.len() == 0;
    }

    pub fn isUnique(self: RocStr) bool {
        // small strings can be copied
        if (self.isSmallStr()) {
            return true;
        }

        // otherwise, check if the refcount is one
        return @call(.always_inline, RocStr.isRefcountOne, .{self});
    }

    fn isRefcountOne(self: RocStr) bool {
        return builtins.utils.rcUnique(@bitCast(self.refcount()));
    }

    fn refcount(self: RocStr) usize {
        const is_seamless_slice = self.isSeamlessSlice();
        if ((self.getCapacity() == 0 and !is_seamless_slice) or self.isSmallStr()) {
            return 1;
        }

        const data_ptr = if (is_seamless_slice)
            self.getAllocationPtr()
        else
            self.bytes;

        const ptr: [*]usize = @as([*]usize, @ptrCast(@alignCast(data_ptr)));
        return (ptr - 1)[0];
    }

    pub fn asSlice(self: *const RocStr) []const u8 {
        return self.asU8ptr()[0..self.len()];
    }

    pub fn asSliceWithCapacity(self: *const RocStr) []const u8 {
        return self.asU8ptr()[0..self.getCapacity()];
    }

    pub fn asSliceWithCapacityMut(self: *RocStr) []u8 {
        return self.asU8ptrMut()[0..self.getCapacity()];
    }

    pub fn asU8ptr(self: *const RocStr) [*]const u8 {
        if (self.isSmallStr()) {
            return @as([*]const u8, @ptrCast(self));
        } else {
            return @as([*]const u8, @ptrCast(self.bytes));
        }
    }

    pub fn asU8ptrMut(self: *RocStr) [*]u8 {
        if (self.isSmallStr()) {
            return @as([*]u8, @ptrCast(self));
        } else {
            return @as([*]u8, @ptrCast(self.bytes));
        }
    }

    // Given a pointer to some bytes, write the first (len) bytes of this
    // RocStr's contents into it.
    //
    // One use for this function is writing into an `alloca` for a C string that
    // only needs to live long enough to be passed as an argument to
    // a C function - like the file path argument to `fopen`.
    pub fn memcpy(self: RocStr, dest: [*]u8) void {
        const src = self.asU8ptr();
        @memcpy(dest[0..self.len()], src[0..self.len()]);
    }
};

pub fn init(
    bytes_ptr: [*]const u8,
    length: usize,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    return @call(.always_inline, RocStr.init, .{ bytes_ptr, length, roc_ops });
}

// Str.equal
/// TODO: Document strEqual.
pub fn strEqual(self: RocStr, other: RocStr) callconv(.C) bool {
    return self.eq(other);
}

// Str.numberOfBytes
/// TODO: Document strNumberOfBytes.
pub fn strNumberOfBytes(string: RocStr) callconv(.C) usize {
    return string.len();
}

// Str.fromInt
/// TODO: Document exportFromInt.
pub fn exportFromInt(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            int: T,
            roc_ops: *RocOps,
        ) callconv(.C) RocStr {
            return @call(.always_inline, strFromIntHelp, .{ T, int, roc_ops });
        }
    }.func;

    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn strFromIntHelp(
    comptime T: type,
    int: T,
    roc_ops: *RocOps,
) RocStr {
    // determine maximum size for this T
    const size = comptime blk: {
        // the string representation of the minimum i128 value uses at most 40 characters
        var buf: [40]u8 = undefined;
        const resultMin = std.fmt.bufPrint(&buf, "{}", .{std.math.minInt(T)}) catch unreachable;
        const resultMax = std.fmt.bufPrint(&buf, "{}", .{std.math.maxInt(T)}) catch unreachable;
        const result = if (resultMin.len > resultMax.len) resultMin.len else resultMax.len;
        break :blk result;
    };

    var buf: [size]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{}", .{int}) catch unreachable;

    return RocStr.init(&buf, result.len, roc_ops);
}

// Str.fromFloat
/// TODO: Document exportFromFloat.
pub fn exportFromFloat(
    comptime T: type,
    comptime name: []const u8,
) void {
    const f = struct {
        fn func(
            float: T,
            roc_ops: *RocOps,
        ) callconv(.C) RocStr {
            return @call(.always_inline, strFromFloatHelp, .{ T, float, roc_ops });
        }
    }.func;

    @export(&f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn strFromFloatHelp(
    comptime T: type,
    float: T,
    roc_ops: *RocOps,
) RocStr {
    var buf: [400]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{d}", .{float}) catch unreachable;

    return RocStr.init(&buf, result.len, roc_ops);
}

// Str.splitOn
/// TODO: Document strSplitOn.
pub fn strSplitOn(
    string: RocStr,
    delimiter: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    const segment_count = countSegments(string, delimiter);
    const list = RocList.list_allocate(@alignOf(RocStr), segment_count, @sizeOf(RocStr), true, roc_ops);

    if (list.bytes) |bytes| {
        const strings = @as([*]RocStr, @ptrCast(@alignCast(bytes)));
        strSplitOnHelp(strings, string, delimiter, roc_ops);
    }

    return list;
}

fn initFromSmallStr(
    slice_bytes: [*]u8,
    len: usize,
    _: usize,
    // TODO we probable don't need this here
    roc_ops: *RocOps,
) RocStr {
    return RocStr.init(slice_bytes, len, roc_ops);
}

/// TODO
pub fn strSplitOnHelp(
    array: [*]RocStr,
    string: RocStr,
    delimiter: RocStr,
    roc_ops: *RocOps,
) void {
    if (delimiter.len() == 0) {
        string.incref(1);
        array[0] = string;
        return;
    }

    var it = std.mem.splitSequence(u8, string.asSlice(), delimiter.asSlice());

    var i: usize = 0;
    var offset: usize = 0;

    while (it.next()) |zig_slice| : (i += 1) {
        const slice_offset = @intFromPtr(zig_slice.ptr) - @intFromPtr(string.asSlice().ptr);
        const roc_slice = substringUnsafe(string, slice_offset, zig_slice.len, roc_ops);
        array[i] = roc_slice;
        offset = slice_offset + zig_slice.len + delimiter.len();
    }

    // Correct refcount for all of the splits made.
    string.incref(i); // i == array.len()
}

// This is used for `Str.splitOn : Str, Str -> List Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
/// TODO: Document countSegments.
pub fn countSegments(string: RocStr, delimiter: RocStr) callconv(.C) usize {
    if (delimiter.isEmpty()) {
        return 1;
    }

    var it = std.mem.splitSequence(u8, string.asSlice(), delimiter.asSlice());
    var count: usize = 0;

    while (it.next()) |_| : (count += 1) {}

    return count;
}

/// TODO: Document countUtf8Bytes.
pub fn countUtf8Bytes(string: RocStr) callconv(.C) u64 {
    return @intCast(string.len());
}

/// TODO: Document isEmpty.
pub fn isEmpty(string: RocStr) callconv(.C) bool {
    return string.isEmpty();
}

/// TODO: Document getCapacity.
pub fn getCapacity(string: RocStr) callconv(.C) usize {
    return string.getCapacity();
}

/// TODO: Document substringUnsafeC.
pub fn substringUnsafeC(
    string: RocStr,
    start_u64: u64,
    length_u64: u64,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    const start: usize = @intCast(start_u64);
    const length: usize = @intCast(length_u64);

    return substringUnsafe(string, start, length, roc_ops);
}

/// TODO
pub fn substringUnsafe(
    string: RocStr,
    start: usize,
    length: usize,
    roc_ops: *RocOps,
) RocStr {
    if (string.isSmallStr()) {
        if (start == 0) {
            var output = string;
            output.setLen(length);
            return output;
        }
        const slice = string.asSlice()[start .. start + length];
        return RocStr.fromSlice(slice, roc_ops);
    }
    if (string.bytes) |source_ptr| {
        if (start == 0 and string.isUnique()) {
            var output = string;
            output.setLen(length);
            return output;
        } else {
            // Shifting right by 1 is required to avoid the highest bit of capacity being set.
            // If it was set, the slice would get interpreted as a small string.
            const str_alloc_ptr = (@intFromPtr(source_ptr) >> 1);
            const slice_alloc_ptr = string.capacity_or_alloc_ptr;
            const slice_mask = string.seamlessSliceMask();
            const alloc_ptr = (str_alloc_ptr & ~slice_mask) | (slice_alloc_ptr & slice_mask);
            return RocStr{
                .bytes = source_ptr + start,
                .length = length | SEAMLESS_SLICE_BIT,
                .capacity_or_alloc_ptr = alloc_ptr,
            };
        }
    }
    return RocStr.empty();
}

/// TODO: Document getUnsafeC.
pub fn getUnsafeC(string: RocStr, index: u64) callconv(.C) u8 {
    return string.getUnchecked(@intCast(index));
}

// Str.startsWith
/// TODO: Document startsWith.
pub fn startsWith(string: RocStr, prefix: RocStr) callconv(.C) bool {
    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    const prefix_len = prefix.len();
    const prefix_ptr = prefix.asU8ptr();

    if (prefix_len > bytes_len) {
        return false;
    }

    // we won't exceed bytes_len due to the previous check
    var i: usize = 0;
    while (i < prefix_len) {
        if (bytes_ptr[i] != prefix_ptr[i]) {
            return false;
        }
        i += 1;
    }
    return true;
}

// Str.repeat
/// TODO: Document repeatC.
pub fn repeatC(
    string: RocStr,
    count_u64: u64,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    const count: usize = @intCast(count_u64);
    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    var ret_string = RocStr.allocate(count * bytes_len, roc_ops);
    var ret_string_ptr = ret_string.asU8ptrMut();

    var i: usize = 0;
    while (i < count) : (i += 1) {
        @memcpy(ret_string_ptr[0..bytes_len], bytes_ptr[0..bytes_len]);
        ret_string_ptr += bytes_len;
    }

    return ret_string;
}

/// Str.endsWith
pub fn endsWith(string: RocStr, suffix: RocStr) callconv(.C) bool {
    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    const suffix_len = suffix.len();
    const suffix_ptr = suffix.asU8ptr();

    if (suffix_len > bytes_len) {
        return false;
    }

    const offset: usize = bytes_len - suffix_len;
    var i: usize = 0;
    while (i < suffix_len) {
        if (bytes_ptr[i + offset] != suffix_ptr[i]) {
            return false;
        }
        i += 1;
    }
    return true;
}

/// Str.concat
pub fn strConcatC(
    arg1: RocStr,
    arg2: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    return @call(.always_inline, strConcat, .{ arg1, arg2, roc_ops });
}

/// TODO
pub fn strConcat(
    arg1: RocStr,
    arg2: RocStr,
    roc_ops: *RocOps,
) RocStr {
    // NOTE: we don't special-case the first argument being empty. That is because it is owned and
    // may have sufficient capacity to store the rest of the list.
    if (arg2.isEmpty()) {
        // the first argument is owned, so we can return it without cloning
        return arg1;
    } else {
        const combined_length = arg1.len() + arg2.len();

        var result = arg1.reallocate(combined_length, roc_ops);
        @memcpy(result.asU8ptrMut()[arg1.len()..combined_length], arg2.asU8ptr()[0..arg2.len()]);

        return result;
    }
}

/// TODO: Document RocListStr.
pub const RocListStr = extern struct {
    list_elements: ?[*]RocStr,
    list_length: usize,
    list_capacity_or_alloc_ptr: usize,
};

/// Str.joinWith
pub fn strJoinWithC(
    list: RocList,
    separator: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    const roc_list_str = RocListStr{
        .list_elements = @as(?[*]RocStr, @ptrCast(@alignCast(list.bytes))),
        .list_length = list.length,
        .list_capacity_or_alloc_ptr = list.capacity_or_alloc_ptr,
    };

    return @call(.always_inline, strJoinWith, .{ roc_list_str, separator, roc_ops });
}

/// TODO
pub fn strJoinWith(
    list: RocListStr,
    separator: RocStr,
    roc_ops: *RocOps,
) RocStr {
    const len = list.list_length;

    if (len == 0) {
        return RocStr.empty();
    } else {
        const ptr = @as([*]RocStr, @ptrCast(list.list_elements));
        const slice: []RocStr = ptr[0..len];

        // determine the size of the result
        var total_size: usize = 0;
        for (slice) |substr| {
            total_size += substr.len();
        }

        // include size of the separator
        total_size += separator.len() * (len - 1);

        var result = RocStr.allocate(total_size, roc_ops);
        const result_ptr = result.asU8ptrMut();

        var offset: usize = 0;
        for (slice[0 .. len - 1]) |substr| {
            substr.memcpy(result_ptr + offset);
            offset += substr.len();

            separator.memcpy(result_ptr + offset);
            offset += separator.len();
        }

        const substr = slice[len - 1];
        substr.memcpy(result_ptr + offset);

        return result;
    }
}

/// Str.toUtf8
pub fn strToUtf8C(
    arg: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocList {
    return strToBytes(arg, roc_ops);
}

inline fn strToBytes(
    arg: RocStr,
    roc_ops: *RocOps,
) RocList {
    const length = arg.len();
    if (length == 0) {
        return RocList.empty();
    } else if (arg.isSmallStr()) {
        const ptr = builtins.utils.allocateWithRefcount(length, RocStr.alignment, false, roc_ops);

        @memcpy(ptr[0..length], arg.asU8ptr()[0..length]);

        return RocList{ .length = length, .bytes = ptr, .capacity_or_alloc_ptr = length };
    } else {
        const is_seamless_slice = arg.length & SEAMLESS_SLICE_BIT;
        return RocList{ .length = length, .bytes = arg.bytes, .capacity_or_alloc_ptr = arg.capacity_or_alloc_ptr | is_seamless_slice };
    }
}

/// TODO
pub const FromUtf8Result = extern struct {
    byte_index: u64,
    string: RocStr,
    is_ok: bool,
    problem_code: Utf8ByteProblem,
};

/// TODO: Document fromUtf8C.
pub fn fromUtf8C(
    list: RocList,
    update_mode: UpdateMode,
    roc_ops: *RocOps,
) callconv(.C) FromUtf8Result {
    return fromUtf8(list, update_mode, roc_ops);
}

const UNICODE_REPLACEMENT: u21 = 0xfffd;

const Utf8Iterator = struct {
    bytes: []u8,
    i: usize,

    pub fn init(list: RocList) Utf8Iterator {
        const bytes = @as([*]u8, @ptrCast(list.bytes))[0..list.length];
        return Utf8Iterator{
            .bytes = bytes,
            .i = 0,
        };
    }

    pub fn nextLossy(it: *Utf8Iterator) ?u32 {
        if (it.bytes.len <= it.i) {
            return null;
        }

        const rest = it.bytes[it.i..];
        const n = unicode.utf8ByteSequenceLength(rest[0]) catch {
            // invalid start byte
            it.i += 1;
            return UNICODE_REPLACEMENT;
        };

        for (1..n) |i| {
            if (rest.len == i) {
                // unexpected end
                it.i += i;
                return UNICODE_REPLACEMENT;
            }
            if (rest[i] < 0x70) {
                // expected continuation byte (>= 0x70)
                it.i += i;
                return UNICODE_REPLACEMENT;
            }
        }

        it.i += n;
        return unicode.utf8Decode(rest[0..n]) catch {
            return UNICODE_REPLACEMENT;
        };
    }

    pub fn reset(it: *Utf8Iterator) void {
        it.i = 0;
    }
};

fn codepointSeqLengthLossy(c: u32) u3 {
    if (c < 0x110000) {
        if (unicode.utf8CodepointSequenceLength(@intCast(c))) |n| {
            return n;
        } else |_| {
            // fallthrough
        }
    }
    return unicode.utf8CodepointSequenceLength(UNICODE_REPLACEMENT) catch unreachable;
}

fn utf8EncodeLossy(c: u32, out: []u8) u3 {
    if (c < 0x110000) {
        if (unicode.utf8Encode(@intCast(c), out)) |n| {
            return n;
        } else |_| {
            // fallthrough
        }
    }
    return unicode.utf8Encode(UNICODE_REPLACEMENT, out) catch unreachable;
}

/// TODO: Document fromUtf8Lossy.
pub fn fromUtf8Lossy(
    list: RocList,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    if (list.len() == 0) {
        return RocStr.empty();
    }

    // PERF: we could try to reuse the input list if it's already valid utf-8, similar to fromUtf8

    var it = Utf8Iterator.init(list);

    var enc_len: usize = 0;
    while (it.nextLossy()) |c| {
        enc_len += codepointSeqLengthLossy(c);
    }

    var str = RocStr.allocate(enc_len, roc_ops);
    const ptr = str.asU8ptrMut()[0..enc_len];
    var end_index: usize = 0;
    it.reset();
    while (it.nextLossy()) |c| {
        end_index += utf8EncodeLossy(c, ptr[end_index..]);
    }
    str.setLen(end_index);
    return str;
}

/// TODO: Document fromUtf8.
pub fn fromUtf8(
    list: RocList,
    update_mode: UpdateMode,
    // TODO seems odd that we need this here
    // maybe we should pass in undefined or something to list.decref?
    roc_ops: *RocOps,
) FromUtf8Result {
    if (list.len() == 0) {
        list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, roc_ops);
        return FromUtf8Result{
            .is_ok = true,
            .string = RocStr.empty(),
            .byte_index = 0,
            .problem_code = Utf8ByteProblem.InvalidStartByte,
        };
    }
    const bytes = @as([*]const u8, @ptrCast(list.bytes))[0..list.len()];

    if (isValidUnicode(bytes)) {
        // Make a seamless slice of the input.
        const string = RocStr.fromSubListUnsafe(list, 0, list.len(), update_mode);
        return FromUtf8Result{
            .is_ok = true,
            .string = string,
            .byte_index = 0,
            .problem_code = Utf8ByteProblem.InvalidStartByte,
        };
    } else {
        const temp = errorToProblem(bytes);

        list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, roc_ops);

        return FromUtf8Result{
            .is_ok = false,
            .string = RocStr.empty(),
            .byte_index = @intCast(temp.index),
            .problem_code = temp.problem,
        };
    }
}

fn errorToProblem(bytes: []const u8) struct { index: usize, problem: Utf8ByteProblem } {
    const len = bytes.len;
    var index: usize = 0;

    while (index < len) {
        const nextNumBytes = numberOfNextCodepointBytes(bytes, index) catch |err| {
            switch (err) {
                error.UnexpectedEof => {
                    return .{ .index = index, .problem = Utf8ByteProblem.UnexpectedEndOfSequence };
                },
                error.Utf8InvalidStartByte => return .{ .index = index, .problem = Utf8ByteProblem.InvalidStartByte },
                error.Utf8ExpectedContinuation => return .{ .index = index, .problem = Utf8ByteProblem.ExpectedContinuation },
                error.Utf8OverlongEncoding => return .{ .index = index, .problem = Utf8ByteProblem.OverlongEncoding },
                error.Utf8EncodesSurrogateHalf => return .{ .index = index, .problem = Utf8ByteProblem.EncodesSurrogateHalf },
                error.Utf8CodepointTooLarge => return .{ .index = index, .problem = Utf8ByteProblem.CodepointTooLarge },
            }
        };
        index += nextNumBytes;
    }

    unreachable;
}

/// Returns true if the given buffer contains valid Unicode data.
pub fn isValidUnicode(buf: []const u8) bool {
    const size = @sizeOf(u64);
    // TODO: we should test changing the step on other platforms.
    // The general tradeoff is making extremely large strings potentially much faster
    // at the cost of small strings being slightly slower.
    const step = size;
    var i: usize = 0;
    while (i + step < buf.len) {
        var bytes: u64 = undefined;
        @memcpy(@as([*]u8, @ptrCast(&bytes))[0..size], buf[i..(i + size)]);
        const unicode_bytes = bytes & 0x8080_8080_8080_8080;
        if (unicode_bytes == 0) {
            i += step;
            continue;
        }

        while (buf[i] < 0b1000_0000) : (i += 1) {}

        while (buf[i] >= 0b1000_0000) {
            // This forces prefetching, otherwise the loop can run at about half speed.
            if (i + 4 >= buf.len) break;
            var small_buf: [4]u8 = undefined;
            @memcpy(small_buf[0..4], buf[i..(i + 4)]);
            // TODO: Should we always inline these function calls below?
            if (std.unicode.utf8ByteSequenceLength(small_buf[0])) |cp_len| {
                if (std.meta.isError(std.unicode.utf8Decode(small_buf[0..cp_len]))) {
                    return false;
                }
                i += cp_len;
            } else |_| {
                return false;
            }
        }
    }

    if (i == buf.len) return true;
    while (buf[i] < 0b1000_0000) {
        i += 1;
        if (i == buf.len) return true;
    }

    return @call(.always_inline, unicode.utf8ValidateSlice, .{buf[i..]});
}

/// TODO
pub const Utf8DecodeError = error{
    UnexpectedEof,
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8EncodesSurrogateHalf,
    Utf8CodepointTooLarge,
};

/// Returns the number of bytes in the next codepoint starting at the given index in the byte slice.
/// Returns an error if the bytes are not valid UTF-8.
///
/// Essentially unicode.utf8ValidateSlice -> https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L156
/// but only for the next codepoint from the index. Then we return the number of bytes of that codepoint.
///
/// TODO: we only ever use the values 0-4, so can we use smaller int than `usize`?
pub fn numberOfNextCodepointBytes(bytes: []const u8, index: usize) Utf8DecodeError!usize {
    const codepoint_len = try unicode.utf8ByteSequenceLength(bytes[index]);
    const codepoint_end_index = index + codepoint_len;
    if (codepoint_end_index > bytes.len) {
        return error.UnexpectedEof;
    }
    _ = try unicode.utf8Decode(bytes[index..codepoint_end_index]);
    return codepoint_end_index - index;
}

/// Return types for validateUtf8Bytes
/// Values must be in alphabetical order. That is, lowest values are the first alphabetically.
pub const Utf8ByteProblem = enum(u8) {
    CodepointTooLarge = 0,
    EncodesSurrogateHalf = 1,
    ExpectedContinuation = 2,
    InvalidStartByte = 3,
    OverlongEncoding = 4,
    UnexpectedEndOfSequence = 5,
};

/// TODO
pub fn validateUtf8Bytes(
    bytes: [*]u8,
    length: usize,
    roc_ops: *RocOps,
) FromUtf8Result {
    return fromUtf8(RocList{ .bytes = bytes, .length = length, .capacity_or_alloc_ptr = length }, .Immutable, roc_ops);
}

/// TODO
pub fn validateUtf8BytesX(
    str: RocList,
    roc_ops: *RocOps,
) FromUtf8Result {
    return fromUtf8(str, .Immutable, roc_ops);
}

/// TODO
pub fn sliceHelp(
    bytes: [*]const u8,
    length: usize,
    roc_ops: *RocOps,
) RocList {
    var list = RocList.list_allocate(RocStr.alignment, length, @sizeOf(u8), false, roc_ops);
    var list_bytes = list.bytes orelse unreachable;
    @memcpy(list_bytes[0..length], bytes[0..length]);
    list.length = length;

    return list;
}

/// TODO
pub fn toErrUtf8ByteResponse(index: usize, problem: Utf8ByteProblem) FromUtf8Result {
    return FromUtf8Result{ .is_ok = false, .string = RocStr.empty(), .byte_index = @as(u64, @intCast(index)), .problem_code = problem };
}

// NOTE on memory: the validate function consumes a RC token of the input. Since
// we freshly created it (in `sliceHelp`), it has only one RC token, and input list will be deallocated.
//
// If we tested with big strings, we'd have to deallocate the output string, but never the input list

/// TODO
pub fn isWhitespace(codepoint: u21) bool {
    // https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
    return switch (codepoint) {
        0x0009...0x000D => true, // control characters
        0x0020 => true, // space
        0x0085 => true, // control character
        0x00A0 => true, // no-break space
        0x1680 => true, // ogham space
        0x2000...0x200A => true, // en quad..hair space
        0x200E...0x200F => true, // left-to-right & right-to-left marks
        0x2028 => true, // line separator
        0x2029 => true, // paragraph separator
        0x202F => true, // narrow no-break space
        0x205F => true, // medium mathematical space
        0x3000 => true, // ideographic space

        else => false,
    };
}

/// TODO: Document strTrim.
pub fn strTrim(
    input_string: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const new_len = original_len - leading_bytes - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr() + leading_bytes, new_len, roc_ops);
    } else if (leading_bytes == 0 and string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.length = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .length = new_len | SEAMLESS_SLICE_BIT,
            .capacity_or_alloc_ptr = string.capacity_or_alloc_ptr,
        };
    } else {
        // Not unique or removing leading bytes, just make a slice.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .length = new_len | SEAMLESS_SLICE_BIT,
            .capacity_or_alloc_ptr = @intFromPtr(bytes_ptr) >> 1,
        };
    }
}

/// TODO: Document strTrimStart.
pub fn strTrimStart(
    input_string: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const new_len = original_len - leading_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr() + leading_bytes, new_len, roc_ops);
    } else if (leading_bytes == 0 and string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.length = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .length = new_len | SEAMLESS_SLICE_BIT,
            .capacity_or_alloc_ptr = string.capacity_or_alloc_ptr,
        };
    } else {
        // Not unique or removing leading bytes, just make a slice.
        return RocStr{
            .bytes = bytes_ptr + leading_bytes,
            .length = new_len | SEAMLESS_SLICE_BIT,
            .capacity_or_alloc_ptr = @intFromPtr(bytes_ptr) >> 1,
        };
    }
}

/// TODO: Document strTrimEnd.
pub fn strTrimEnd(
    input_string: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == trailing_bytes) {
        string.decref(roc_ops);
        return RocStr.empty();
    }

    const new_len = original_len - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr(), new_len, roc_ops);
    } else if (string.isUnique()) {
        // Big and unique with no leading bytes to remove.
        // Just take ownership and shrink the length.
        var new_string = string;
        new_string.length = new_len;

        return new_string;
    } else if (string.isSeamlessSlice()) {
        // Already a seamless slice, just update the range.
        return RocStr{
            .bytes = bytes_ptr,
            .length = new_len | SEAMLESS_SLICE_BIT,
            .capacity_or_alloc_ptr = string.capacity_or_alloc_ptr,
        };
    } else {
        // Not unique, just make a slice.
        return RocStr{
            .bytes = bytes_ptr,
            .length = new_len | SEAMLESS_SLICE_BIT,
            .capacity_or_alloc_ptr = @intFromPtr(bytes_ptr) >> 1,
        };
    }
}

fn countLeadingWhitespaceBytes(string: RocStr) usize {
    var byte_count: usize = 0;

    const bytes = string.asU8ptr()[0..string.len()];
    var iter = unicode.Utf8View.initUnchecked(bytes).iterator();
    while (iter.nextCodepoint()) |codepoint| {
        if (isWhitespace(codepoint)) {
            byte_count += unicode.utf8CodepointSequenceLength(codepoint) catch break;
        } else {
            break;
        }
    }

    return byte_count;
}

fn countTrailingWhitespaceBytes(string: RocStr) usize {
    var byte_count: usize = 0;

    const bytes = string.asU8ptr()[0..string.len()];
    var iter = ReverseUtf8View.initUnchecked(bytes).iterator();
    while (iter.nextCodepoint()) |codepoint| {
        if (isWhitespace(codepoint)) {
            byte_count += unicode.utf8CodepointSequenceLength(codepoint) catch break;
        } else {
            break;
        }
    }

    return byte_count;
}

/// Str.with_ascii_lowercased
pub fn strWithAsciiLowercased(
    string: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    var new_str = if (string.isUnique())
        string
    else blk: {
        string.decref(roc_ops);
        break :blk RocStr.fromSlice(string.asSlice(), roc_ops);
    };

    const new_str_bytes = new_str.asU8ptrMut()[0..string.len()];
    for (new_str_bytes) |*c| {
        c.* = ascii.toLower(c.*);
    }
    return new_str;
}

/// Str.with_ascii_uppercased
pub fn strWithAsciiUppercased(
    string: RocStr,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    var new_str = if (string.isUnique())
        string
    else blk: {
        string.decref(roc_ops);
        break :blk RocStr.fromSlice(string.asSlice(), roc_ops);
    };

    const new_str_bytes = new_str.asU8ptrMut()[0..string.len()];
    for (new_str_bytes) |*c| {
        c.* = ascii.toUpper(c.*);
    }
    return new_str;
}

/// TODO: Document strCaselessAsciiEquals.
pub fn strCaselessAsciiEquals(self: RocStr, other: RocStr) callconv(.C) bool {
    if (self.bytes == other.bytes and self.length == other.length) {
        return true;
    }

    return ascii.eqlIgnoreCase(self.asSlice(), other.asSlice());
}

fn decStr(ptr: ?[*]u8) callconv(.C) void {
    const str_ptr = @as(*RocStr, @ptrCast(@alignCast(ptr orelse unreachable)));
    str_ptr.decref();
}

/// A backwards version of Utf8View from std.unicode
pub const ReverseUtf8View = struct {
    bytes: []const u8,

    pub fn initUnchecked(s: []const u8) ReverseUtf8View {
        return ReverseUtf8View{ .bytes = s };
    }

    pub fn iterator(s: ReverseUtf8View) ReverseUtf8Iterator {
        return ReverseUtf8Iterator{
            .bytes = s.bytes,
            .i = if (s.bytes.len > 0) s.bytes.len - 1 else null,
        };
    }
};

/// A backwards version of Utf8Iterator from std.unicode
const ReverseUtf8Iterator = struct {
    bytes: []const u8,
    // NOTE null signifies complete/empty
    i: ?usize,

    pub fn nextCodepointSlice(it: *ReverseUtf8Iterator) ?[]const u8 {
        if (it.i) |index| {
            var i = index;

            // NOTE this relies on the string being valid utf8 to not run off the end
            while (!utf8BeginByte(it.bytes[i])) {
                i -= 1;
            }

            const cp_len = unicode.utf8ByteSequenceLength(it.bytes[i]) catch unreachable;
            const slice = it.bytes[i .. i + cp_len];

            it.i = if (i == 0) null else i - 1;

            return slice;
        } else {
            return null;
        }
    }

    pub fn nextCodepoint(it: *ReverseUtf8Iterator) ?u21 {
        const slice = it.nextCodepointSlice() orelse return null;

        return switch (slice.len) {
            1 => @as(u21, slice[0]),
            2 => unicode.utf8Decode2([2]u8{ slice[0], slice[1] }) catch unreachable,
            3 => unicode.utf8Decode3([3]u8{ slice[0], slice[1], slice[2] }) catch unreachable,
            4 => unicode.utf8Decode4([4]u8{ slice[0], slice[1], slice[2], slice[3] }) catch unreachable,
            else => unreachable,
        };
    }
};

fn utf8BeginByte(byte: u8) bool {
    return switch (byte) {
        0b1000_0000...0b1011_1111 => false,
        else => true,
    };
}

/// Ensures the RocStr has at least the specified spare capacity, reallocating if necessary.
pub fn reserveC(
    string: RocStr,
    spare_u64: u64,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    return reserve(string, @intCast(spare_u64), roc_ops);
}

/// TODO
pub fn reserve(
    string: RocStr,
    spare: usize,
    roc_ops: *RocOps,
) RocStr {
    const old_length = string.len();

    if (string.getCapacity() >= old_length + spare) {
        return string;
    } else {
        var output = string.reallocate(old_length + spare, roc_ops);
        output.setLen(old_length);
        return output;
    }
}

/// Creates a new RocStr with the specified capacity.
pub fn withCapacityC(
    capacity: u64,
    roc_ops: *RocOps,
) callconv(.C) RocStr {
    var str = RocStr.allocate(@intCast(capacity), roc_ops);
    str.setLen(0);
    return str;
}

/// Clones the contents of the given RocStr into the provided pointer, starting at the given offset and extra_offset.
pub fn strCloneTo(
    string: RocStr,
    ptr: [*]u8,
    offset: usize,
    extra_offset: usize,
) callconv(.C) usize {
    const WIDTH: usize = @sizeOf(RocStr);
    if (string.isSmallStr()) {
        const array: [@sizeOf(RocStr)]u8 = @as([@sizeOf(RocStr)]u8, @bitCast(string));

        var i: usize = 0;
        while (i < WIDTH) : (i += 1) {
            ptr[offset + i] = array[i];
        }

        return extra_offset;
    } else {
        const slice = string.asSlice();

        var relative = string;
        relative.bytes = @as(?[*]u8, @ptrFromInt(extra_offset)); // i.e. just after the string struct

        // write the string struct
        const array = relative.asArray();
        @memcpy(ptr[offset..(offset + WIDTH)], array[0..WIDTH]);

        // write the string bytes just after the struct
        @memcpy(ptr[extra_offset..(extra_offset + slice.len)], slice);

        return extra_offset + slice.len;
    }
}

/// Returns a pointer to the allocation backing the given RocStr
pub fn strAllocationPtr(
    string: RocStr,
) callconv(.C) ?[*]u8 {
    return string.getAllocationPtr();
}

/// Release excess capacity
pub fn strReleaseExcessCapacity(
    roc_ops: RocOps,
    string: RocStr,
) callconv(.C) RocStr {
    const old_length = string.len();
    // We use the direct list.capacity_or_alloc_ptr to make sure both that there is no extra capacity and that it isn't a seamless slice.
    if (string.isSmallStr()) {
        // SmallStr has no excess capacity.
        return string;
    } else if (string.isUnique() and !string.isSeamlessSlice() and string.getCapacity() == old_length) {
        return string;
    } else if (old_length == 0) {
        string.decref(roc_ops);
        return RocStr.empty();
    } else {
        var output = RocStr.allocateExact(old_length, roc_ops);
        const source_ptr = string.asU8ptr();
        const dest_ptr = output.asU8ptrMut();

        @memcpy(dest_ptr[0..old_length], source_ptr[0..old_length]);
        string.decref(roc_ops);

        return output;
    }
}
