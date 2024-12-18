const utils = @import("utils.zig");
const RocList = @import("list.zig").RocList;
const UpdateMode = utils.UpdateMode;
const std = @import("std");
const mem = std.mem;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expect = testing.expect;

const InPlace = enum(u8) {
    InPlace,
    Clone,
};

const MASK_ISIZE: isize = std.math.minInt(isize);
const MASK: usize = @as(usize, @bitCast(MASK_ISIZE));
const SEAMLESS_SLICE_BIT: usize = MASK;

const SMALL_STR_MAX_LENGTH = SMALL_STRING_SIZE - 1;
const SMALL_STRING_SIZE = @sizeOf(RocStr);

fn init_blank_small_string(comptime n: usize) [n]u8 {
    var prime_list: [n]u8 = undefined;

    var i = 0;
    while (i < n) : (i += 1) {
        prime_list[i] = 0;
    }

    return prime_list;
}

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
    pub fn init(bytes_ptr: [*]const u8, length: usize) RocStr {
        var result = RocStr.allocate(length);
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

    pub fn fromSlice(slice: []const u8) RocStr {
        return RocStr.init(slice.ptr, slice.len);
    }

    fn allocateBig(length: usize, capacity: usize) RocStr {
        const first_element = utils.allocateWithRefcount(capacity, @sizeOf(usize), false);

        return RocStr{
            .bytes = first_element,
            .length = length,
            .capacity_or_alloc_ptr = capacity,
        };
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // May have a larger capacity than the length.
    pub fn allocate(length: usize) RocStr {
        const element_width = 1;
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = utils.calculateCapacity(0, length, element_width);
            return RocStr.allocateBig(length, capacity);
        } else {
            var string = RocStr.empty();

            string.asU8ptrMut()[@sizeOf(RocStr) - 1] = @as(u8, @intCast(length)) | 0b1000_0000;

            return string;
        }
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet.
    // Will have the exact same capacity as length if it is not a small string.
    pub fn allocateExact(length: usize) RocStr {
        const result_is_big = length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            return RocStr.allocateBig(length, length);
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
                utils.increfRcPtrC(@as(*isize, @ptrCast(isizes - 1)), @as(isize, @intCast(n)));
            }
        }
    }

    pub fn decref(self: RocStr) void {
        if (!self.isSmallStr()) {
            utils.decref(self.getAllocationPtr(), self.capacity_or_alloc_ptr, RocStr.alignment, false);
        }
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        // If they are byte-for-byte equal, they're definitely equal!
        if (self.bytes == other.bytes and self.length == other.length and self.capacity_or_alloc_ptr == other.capacity_or_alloc_ptr) {
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

    pub fn clone(str: RocStr) RocStr {
        if (str.isSmallStr()) {
            // just return the bytes
            return str;
        } else {
            const new_str = RocStr.allocateBig(str.length, str.length);

            var old_bytes: [*]u8 = @as([*]u8, @ptrCast(str.bytes));
            var new_bytes: [*]u8 = @as([*]u8, @ptrCast(new_str.bytes));

            @memcpy(new_bytes[0..str.length], old_bytes[0..str.length]);

            return new_str;
        }
    }

    pub fn reallocate(
        self: RocStr,
        new_length: usize,
    ) RocStr {
        const element_width = 1;
        const old_capacity = self.getCapacity();

        if (self.isSmallStr() or self.isSeamlessSlice() or !self.isUnique()) {
            return self.reallocateFresh(new_length);
        }

        if (self.bytes) |source_ptr| {
            if (old_capacity > new_length) {
                var output = self;
                output.setLen(new_length);
                return output;
            }
            const new_capacity = utils.calculateCapacity(old_capacity, new_length, element_width);
            const new_source = utils.unsafeReallocate(
                source_ptr,
                RocStr.alignment,
                old_capacity,
                new_capacity,
                element_width,
                false,
            );

            return RocStr{ .bytes = new_source, .length = new_length, .capacity_or_alloc_ptr = new_capacity };
        }
        return self.reallocateFresh(new_length);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    fn reallocateFresh(
        self: RocStr,
        new_length: usize,
    ) RocStr {
        const old_length = self.len();

        const element_width = 1;
        const result_is_big = new_length >= SMALL_STRING_SIZE;

        if (result_is_big) {
            const capacity = utils.calculateCapacity(0, new_length, element_width);
            var result = RocStr.allocateBig(new_length, capacity);

            // transfer the memory

            const source_ptr = self.asU8ptr();
            const dest_ptr = result.asU8ptrMut();

            @memcpy(dest_ptr[0..old_length], source_ptr[0..old_length]);
            @memset(dest_ptr[old_length..new_length], 0);

            self.decref();

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

            self.decref();

            return string;
        }
    }

    pub fn isSmallStr(self: RocStr) bool {
        return @as(isize, @bitCast(self.capacity_or_alloc_ptr)) < 0;
    }

    test "isSmallStr: returns true for empty string" {
        try expect(isSmallStr(RocStr.empty()));
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
        return self.refcountMachine() == utils.REFCOUNT_ONE;
    }

    fn refcountMachine(self: RocStr) usize {
        if ((self.getCapacity() == 0 and !self.isSeamlessSlice()) or self.isSmallStr()) {
            return utils.REFCOUNT_ONE;
        }

        const ptr: [*]usize = @as([*]usize, @ptrCast(@alignCast(self.bytes)));
        return (ptr - 1)[0];
    }

    fn refcountHuman(self: RocStr) usize {
        return self.refcountMachine() - utils.REFCOUNT_ONE + 1;
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

    test "RocStr.eq: small, equal" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "abc".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        try expect(roc_str1.eq(roc_str2));

        roc_str1.decref();
        roc_str2.decref();
    }

    test "RocStr.eq: small, not equal, different length" {
        const str1_len = 4;
        var str1: [str1_len]u8 = "abcd".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: small, not equal, same length" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "acb".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, equal" {
        const content = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content, content.len);
        const roc_str2 = RocStr.init(content, content.len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, different lengths, unequal" {
        const content1 = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content1, content1.len);
        const content2 = "012345678901234567890";
        const roc_str2 = RocStr.init(content2, content2.len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, different content, unequal" {
        const content1 = "012345678901234567890123456789!!";
        const roc_str1 = RocStr.init(content1, content1.len);
        const content2 = "012345678901234567890123456789--";
        const roc_str2 = RocStr.init(content2, content2.len);

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, garbage after end, equal" {
        const content = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content, content.len);
        const roc_str2 = RocStr.init(content, content.len);
        try expect(roc_str1.bytes != roc_str2.bytes);

        // Insert garbage after the end of each string
        roc_str1.bytes.?[30] = '!';
        roc_str1.bytes.?[31] = '!';
        roc_str2.bytes.?[30] = '-';
        roc_str2.bytes.?[31] = '-';

        defer {
            roc_str1.decref();
            roc_str2.decref();
        }

        try expect(roc_str1.eq(roc_str2));
    }
};

pub fn init(bytes_ptr: [*]const u8, length: usize) callconv(.C) RocStr {
    return @call(.always_inline, RocStr.init, .{ bytes_ptr, length });
}

// Str.equal
pub fn strEqual(self: RocStr, other: RocStr) callconv(.C) bool {
    return self.eq(other);
}

// Str.numberOfBytes
pub fn strNumberOfBytes(string: RocStr) callconv(.C) usize {
    return string.len();
}

// Str.fromInt
pub fn exportFromInt(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(int: T) callconv(.C) RocStr {
            return @call(.always_inline, strFromIntHelp, .{ T, int });
        }
    }.func;

    @export(f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn strFromIntHelp(comptime T: type, int: T) RocStr {
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

    return RocStr.init(&buf, result.len);
}

// Str.fromFloat
pub fn exportFromFloat(comptime T: type, comptime name: []const u8) void {
    const f = struct {
        fn func(float: T) callconv(.C) RocStr {
            return @call(.always_inline, strFromFloatHelp, .{ T, float });
        }
    }.func;

    @export(f, .{ .name = name ++ @typeName(T), .linkage = .strong });
}

fn strFromFloatHelp(comptime T: type, float: T) RocStr {
    var buf: [400]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{d}", .{float}) catch unreachable;

    return RocStr.init(&buf, result.len);
}

// Str.splitOn
pub fn strSplitOn(string: RocStr, delimiter: RocStr) callconv(.C) RocList {
    const segment_count = countSegments(string, delimiter);
    const list = RocList.allocate(@alignOf(RocStr), segment_count, @sizeOf(RocStr), true);

    if (list.bytes) |bytes| {
        const strings = @as([*]RocStr, @ptrCast(@alignCast(bytes)));
        strSplitOnHelp(strings, string, delimiter);
    }

    return list;
}

fn initFromSmallStr(slice_bytes: [*]u8, len: usize, _: usize) RocStr {
    return RocStr.init(slice_bytes, len);
}

// The alloc_ptr must already be shifted to be ready for storing in a seamless slice.
fn initFromBigStr(slice_bytes: [*]u8, len: usize, alloc_ptr: usize) RocStr {
    // Here we can make seamless slices instead of copying to a new small str.
    return RocStr{
        .bytes = slice_bytes,
        .length = len | SEAMLESS_SLICE_BIT,
        .capacity_or_alloc_ptr = alloc_ptr,
    };
}

fn strSplitOnHelp(array: [*]RocStr, string: RocStr, delimiter: RocStr) void {
    if (delimiter.len() == 0) {
        string.incref(1);
        array[0] = string;
        return;
    }

    var it = std.mem.split(u8, string.asSlice(), delimiter.asSlice());

    var i: usize = 0;
    var offset: usize = 0;

    while (it.next()) |zig_slice| {
        const roc_slice = substringUnsafe(string, offset, zig_slice.len);
        array[i] = roc_slice;

        i += 1;
        offset += zig_slice.len + delimiter.len();
    }

    // Correct refcount for all of the splits made.
    string.incref(i); // i == array.len()
}

test "strSplitHelp: empty delimiter" {
    // Str.splitOn "abc" "" == ["abc"]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref();
        }

        for (expected) |roc_str| {
            roc_str.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitHelp: no delimiter" {
    // Str.splitOn "abc" "!" == ["abc"]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref();
        }

        for (expected) |roc_str| {
            roc_str.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitHelp: empty start" {
    const str_arr = "/a";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "/";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 2;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const one = RocStr.init("a", 1);

    const expected = [2]RocStr{
        RocStr.empty(), one,
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: empty end" {
    const str_arr = "1---- ---- ---- ---- ----2---- ---- ---- ---- ----";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "---- ---- ---- ---- ----";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const one = RocStr.init("1", 1);
    const two = RocStr.init("2", 1);

    const expected = [3]RocStr{
        one, two, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitHelp: string equals delimiter" {
    const str_delimiter_arr = "/";
    const str_delimiter = RocStr.init(str_delimiter_arr, str_delimiter_arr.len);

    const array_len: usize = 2;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str_delimiter, str_delimiter);

    const expected = [2]RocStr{ RocStr.empty(), RocStr.empty() };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str_delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: delimiter on sides" {
    const str_arr = "tttghittt";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "ttt";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;
    strSplitOnHelp(array_ptr, str, delimiter);

    const ghi_arr = "ghi";
    const ghi = RocStr.init(ghi_arr, ghi_arr.len);

    const expected = [3]RocStr{
        RocStr.empty(), ghi, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref();
        }

        for (expected) |rocStr| {
            rocStr.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitHelp: three pieces" {
    // Str.splitOn "a!b!c" "!" == ["a", "b", "c"]
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const a = RocStr.init("a", 1);
    const b = RocStr.init("b", 1);
    const c = RocStr.init("c", 1);

    const expected_array = [array_len]RocStr{
        a, b, c,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref();
        }

        for (expected_array) |roc_str| {
            roc_str.decref();
        }

        str.decref();
        delimiter.decref();
    }

    try expectEqual(expected_array.len, array.len);
    try expect(array[0].eq(expected_array[0]));
    try expect(array[1].eq(expected_array[1]));
    try expect(array[2].eq(expected_array[2]));
}

test "strSplitHelp: overlapping delimiter 1" {
    // Str.splitOn "aaa" "aa" == ["", "a"]
    const str_arr = "aaa";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "aa";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [2]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const expected = [2]RocStr{
        RocStr.empty(),
        RocStr.init("a", 1),
    };

    // strings are all small so we ignore freeing the memory

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: overlapping delimiter 2" {
    // Str.splitOn "aaa" "aa" == ["", "a"]
    const str_arr = "aaaa";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "aa";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [3]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitOnHelp(array_ptr, str, delimiter);

    const expected = [3]RocStr{
        RocStr.empty(),
        RocStr.empty(),
        RocStr.empty(),
    };

    // strings are all small so we ignore freeing the memory

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

// This is used for `Str.splitOn : Str, Str -> List Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
pub fn countSegments(string: RocStr, delimiter: RocStr) callconv(.C) usize {
    if (delimiter.isEmpty()) {
        return 1;
    }

    var it = std.mem.split(u8, string.asSlice(), delimiter.asSlice());
    var count: usize = 0;

    while (it.next()) |_| : (count += 1) {}

    return count;
}

test "countSegments: long delimiter" {
    // Str.splitOn "str" "delimiter" == ["str"]
    // 1 segment
    const str_arr = "str";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.decref();
        delimiter.decref();
    }

    const segments_count = countSegments(str, delimiter);
    try expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    // Str.splitOn "hello there" "hello" == ["", " there"]
    // 2 segments
    const str_arr = "hello there";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "hello";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.decref();
        delimiter.decref();
    }

    const segments_count = countSegments(str, delimiter);

    try expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    // Str.splitOn "a!b!c" "!" == ["a", "b", "c"]
    // 3 segments
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.decref();
        delimiter.decref();
    }

    const segments_count = countSegments(str, delimiter);

    try expectEqual(segments_count, 3);
}

test "countSegments: string equals delimiter" {
    // Str.splitOn "/" "/" == ["", ""]
    // 2 segments
    const str_delimiter_arr = "/";
    const str_delimiter = RocStr.init(str_delimiter_arr, str_delimiter_arr.len);

    defer {
        str_delimiter.decref();
    }

    const segments_count = countSegments(str_delimiter, str_delimiter);

    try expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 1" {
    // Str.splitOn "aaa" "aa" == ["", "a"]
    const segments_count = countSegments(RocStr.init("aaa", 3), RocStr.init("aa", 2));

    try expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 2" {
    // Str.splitOn "aaa" "aa" == ["", "a"]
    const segments_count = countSegments(RocStr.init("aaaa", 4), RocStr.init("aa", 2));

    try expectEqual(segments_count, 3);
}

pub fn countUtf8Bytes(string: RocStr) callconv(.C) u64 {
    return @intCast(string.len());
}

pub fn isEmpty(string: RocStr) callconv(.C) bool {
    return string.isEmpty();
}

pub fn getCapacity(string: RocStr) callconv(.C) usize {
    return string.getCapacity();
}

pub fn substringUnsafeC(string: RocStr, start_u64: u64, length_u64: u64) callconv(.C) RocStr {
    const start: usize = @intCast(start_u64);
    const length: usize = @intCast(length_u64);

    return substringUnsafe(string, start, length);
}

fn substringUnsafe(string: RocStr, start: usize, length: usize) RocStr {
    if (string.isSmallStr()) {
        if (start == 0) {
            var output = string;
            output.setLen(length);
            return output;
        }
        const slice = string.asSlice()[start .. start + length];
        return RocStr.fromSlice(slice);
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

pub fn getUnsafeC(string: RocStr, index: u64) callconv(.C) u8 {
    return string.getUnchecked(@intCast(index));
}

test "substringUnsafe: start" {
    const str = RocStr.fromSlice("abcdef");
    defer str.decref();

    const expected = RocStr.fromSlice("abc");
    defer expected.decref();

    const actual = substringUnsafe(str, 0, 3);

    try expect(RocStr.eq(actual, expected));
}

test "substringUnsafe: middle" {
    const str = RocStr.fromSlice("abcdef");
    defer str.decref();

    const expected = RocStr.fromSlice("bcd");
    defer expected.decref();

    const actual = substringUnsafe(str, 1, 3);

    try expect(RocStr.eq(actual, expected));
}

test "substringUnsafe: end" {
    const str = RocStr.fromSlice("a string so long it is heap-allocated");
    defer str.decref();

    const expected = RocStr.fromSlice("heap-allocated");
    defer expected.decref();

    const actual = substringUnsafe(str, 23, 37 - 23);

    try expect(RocStr.eq(actual, expected));
}

// Str.startsWith
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
pub fn repeatC(string: RocStr, count_u64: u64) callconv(.C) RocStr {
    const count: usize = @intCast(count_u64);
    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    var ret_string = RocStr.allocate(count * bytes_len);
    var ret_string_ptr = ret_string.asU8ptrMut();

    var i: usize = 0;
    while (i < count) : (i += 1) {
        @memcpy(ret_string_ptr[0..bytes_len], bytes_ptr[0..bytes_len]);
        ret_string_ptr += bytes_len;
    }

    return ret_string;
}

test "startsWith: foo starts with fo" {
    const foo = RocStr.fromSlice("foo");
    const fo = RocStr.fromSlice("fo");
    try expect(startsWith(foo, fo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str = RocStr.fromSlice("123456789123456789");
    defer str.decref();
    try expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.fromSlice("12345678912345678910");
    defer str.decref();
    const prefix = RocStr.fromSlice("123456789123456789");
    defer prefix.decref();

    try expect(startsWith(str, prefix));
}

// Str.endsWith
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

test "endsWith: foo ends with oo" {
    const foo = RocStr.init("foo", 3);
    const oo = RocStr.init("oo", 2);
    defer foo.decref();
    defer oo.decref();

    try expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    defer str.decref();
    try expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init("12345678912345678910", 20);
    const suffix = RocStr.init("345678912345678910", 18);
    defer str.decref();
    defer suffix.decref();

    try expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init("hello world", 11);
    const suffix = RocStr.init("world", 5);
    defer str.decref();
    defer suffix.decref();

    try expect(endsWith(str, suffix));
}

// Str.concat
pub fn strConcatC(arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return @call(.always_inline, strConcat, .{ arg1, arg2 });
}

fn strConcat(arg1: RocStr, arg2: RocStr) RocStr {
    // NOTE: we don't special-case the first argument being empty. That is because it is owned and
    // may have sufficient capacity to store the rest of the list.
    if (arg2.isEmpty()) {
        // the first argument is owned, so we can return it without cloning
        return arg1;
    } else {
        const combined_length = arg1.len() + arg2.len();

        var result = arg1.reallocate(combined_length);
        @memcpy(result.asU8ptrMut()[arg1.len()..combined_length], arg2.asU8ptr()[0..arg2.len()]);

        return result;
    }
}

test "RocStr.concat: small concat small" {
    const str1_len = 3;
    var str1: [str1_len]u8 = "foo".*;
    const str1_ptr: [*]u8 = &str1;
    var roc_str1 = RocStr.init(str1_ptr, str1_len);

    const str2_len = 3;
    var str2: [str2_len]u8 = "abc".*;
    const str2_ptr: [*]u8 = &str2;
    var roc_str2 = RocStr.init(str2_ptr, str2_len);

    const str3_len = 6;
    var str3: [str3_len]u8 = "fooabc".*;
    const str3_ptr: [*]u8 = &str3;
    var roc_str3 = RocStr.init(str3_ptr, str3_len);

    defer {
        roc_str1.decref();
        roc_str2.decref();
        roc_str3.decref();
    }

    const result = strConcat(roc_str1, roc_str2);

    defer result.decref();

    try expect(roc_str3.eq(result));
}

pub const RocListStr = extern struct {
    list_elements: ?[*]RocStr,
    list_length: usize,
    list_capacity_or_alloc_ptr: usize,
};

// Str.joinWith
pub fn strJoinWithC(list: RocList, separator: RocStr) callconv(.C) RocStr {
    const roc_list_str = RocListStr{
        .list_elements = @as(?[*]RocStr, @ptrCast(@alignCast(list.bytes))),
        .list_length = list.length,
        .list_capacity_or_alloc_ptr = list.capacity_or_alloc_ptr,
    };

    return @call(.always_inline, strJoinWith, .{ roc_list_str, separator });
}

fn strJoinWith(list: RocListStr, separator: RocStr) RocStr {
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

        var result = RocStr.allocate(total_size);
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

test "RocStr.joinWith: result is big" {
    const sep_len = 2;
    var sep: [sep_len]u8 = ", ".*;
    const sep_ptr: [*]u8 = &sep;
    var roc_sep = RocStr.init(sep_ptr, sep_len);

    const elem_len = 13;
    var elem: [elem_len]u8 = "foobarbazspam".*;
    const elem_ptr: [*]u8 = &elem;
    var roc_elem = RocStr.init(elem_ptr, elem_len);

    const result_len = 43;
    var xresult: [result_len]u8 = "foobarbazspam, foobarbazspam, foobarbazspam".*;
    const result_ptr: [*]u8 = &xresult;
    var roc_result = RocStr.init(result_ptr, result_len);

    var elements: [3]RocStr = .{ roc_elem, roc_elem, roc_elem };
    const list = RocListStr{
        .list_length = 3,
        .list_capacity_or_alloc_ptr = 3,
        .list_elements = @as([*]RocStr, @ptrCast(&elements)),
    };

    defer {
        roc_sep.decref();
        roc_elem.decref();
        roc_result.decref();
    }

    const result = strJoinWith(list, roc_sep);

    defer result.decref();

    try expect(roc_result.eq(result));
}

// Str.toUtf8
pub fn strToUtf8C(arg: RocStr) callconv(.C) RocList {
    return strToBytes(arg);
}

inline fn strToBytes(arg: RocStr) RocList {
    const length = arg.len();
    if (length == 0) {
        return RocList.empty();
    } else if (arg.isSmallStr()) {
        const ptr = utils.allocateWithRefcount(length, RocStr.alignment, false);

        @memcpy(ptr[0..length], arg.asU8ptr()[0..length]);

        return RocList{ .length = length, .bytes = ptr, .capacity_or_alloc_ptr = length };
    } else {
        const is_seamless_slice = arg.length & SEAMLESS_SLICE_BIT;
        return RocList{ .length = length, .bytes = arg.bytes, .capacity_or_alloc_ptr = arg.capacity_or_alloc_ptr | is_seamless_slice };
    }
}

const FromUtf8Result = extern struct {
    byte_index: u64,
    string: RocStr,
    is_ok: bool,
    problem_code: Utf8ByteProblem,
};

pub fn fromUtf8C(
    list: RocList,
    update_mode: UpdateMode,
) callconv(.C) FromUtf8Result {
    return fromUtf8(list, update_mode);
}

pub fn fromUtf8(
    list: RocList,
    update_mode: UpdateMode,
) FromUtf8Result {
    if (list.len() == 0) {
        list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone);
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

        list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone);

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

const Utf8DecodeError = error{
    UnexpectedEof,
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8EncodesSurrogateHalf,
    Utf8CodepointTooLarge,
};

// Essentially unicode.utf8ValidateSlice -> https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L156
// but only for the next codepoint from the index. Then we return the number of bytes of that codepoint.
// TODO: we only ever use the values 0-4, so can we use smaller int than `usize`?
pub fn numberOfNextCodepointBytes(bytes: []const u8, index: usize) Utf8DecodeError!usize {
    const codepoint_len = try unicode.utf8ByteSequenceLength(bytes[index]);
    const codepoint_end_index = index + codepoint_len;
    if (codepoint_end_index > bytes.len) {
        return error.UnexpectedEof;
    }
    _ = try unicode.utf8Decode(bytes[index..codepoint_end_index]);
    return codepoint_end_index - index;
}

// Return types for validateUtf8Bytes
// Values must be in alphabetical order. That is, lowest values are the first alphabetically.
pub const Utf8ByteProblem = enum(u8) {
    CodepointTooLarge = 0,
    EncodesSurrogateHalf = 1,
    ExpectedContinuation = 2,
    InvalidStartByte = 3,
    OverlongEncoding = 4,
    UnexpectedEndOfSequence = 5,
};

fn validateUtf8Bytes(bytes: [*]u8, length: usize) FromUtf8Result {
    return fromUtf8(RocList{ .bytes = bytes, .length = length, .capacity_or_alloc_ptr = length }, .Immutable);
}

fn validateUtf8BytesX(str: RocList) FromUtf8Result {
    return fromUtf8(str, .Immutable);
}

fn expectOk(result: FromUtf8Result) !void {
    try expectEqual(result.is_ok, true);
}

fn sliceHelp(bytes: [*]const u8, length: usize) RocList {
    var list = RocList.allocate(RocStr.alignment, length, @sizeOf(u8), false);
    var list_bytes = list.bytes orelse unreachable;
    @memcpy(list_bytes[0..length], bytes[0..length]);
    list.length = length;

    return list;
}

fn toErrUtf8ByteResponse(index: usize, problem: Utf8ByteProblem) FromUtf8Result {
    return FromUtf8Result{ .is_ok = false, .string = RocStr.empty(), .byte_index = @as(u64, @intCast(index)), .problem_code = problem };
}

// NOTE on memory: the validate function consumes a RC token of the input. Since
// we freshly created it (in `sliceHelp`), it has only one RC token, and input list will be deallocated.
//
// If we tested with big strings, we'd have to deallocate the output string, but never the input list

test "validateUtf8Bytes: ascii" {
    const raw = "abc";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode œ" {
    const raw = "œ";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode ∆" {
    const raw = "∆";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: emoji" {
    const raw = "💖";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode ∆ in middle of array" {
    const raw = "œb∆c¬";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    const str_result = validateUtf8BytesX(list);
    defer str_result.string.decref();
    try expectOk(str_result);
}

fn expectErr(list: RocList, index: usize, err: Utf8DecodeError, problem: Utf8ByteProblem) !void {
    const str_ptr = @as([*]u8, @ptrCast(list.bytes));
    const len = list.length;

    try expectError(err, numberOfNextCodepointBytes(str_ptr[0..len], index));
    try expectEqual(toErrUtf8ByteResponse(index, problem), validateUtf8Bytes(str_ptr, len));
}

test "validateUtf8Bytes: invalid start byte" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "ab\x80c";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 2, error.Utf8InvalidStartByte, Utf8ByteProblem.InvalidStartByte);
}

test "validateUtf8Bytes: unexpected eof for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\xa0\xc0";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x80\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: overlong" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L451
    const raw = "abc\xf0\x80\x80\x80";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8OverlongEncoding, Utf8ByteProblem.OverlongEncoding);
}

test "validateUtf8Bytes: codepoint out too large" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L465
    const raw = "abc\xf4\x90\x80\x80";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8CodepointTooLarge, Utf8ByteProblem.CodepointTooLarge);
}

test "validateUtf8Bytes: surrogate halves" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L468
    const raw = "abc\xed\xa0\x80";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8EncodesSurrogateHalf, Utf8ByteProblem.EncodesSurrogateHalf);
}

fn isWhitespace(codepoint: u21) bool {
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

test "isWhitespace" {
    try expect(isWhitespace(' '));
    try expect(isWhitespace('\u{00A0}'));
    try expect(!isWhitespace('x'));
}

pub fn strTrim(input_string: RocStr) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref();
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref();
        return RocStr.empty();
    }

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const new_len = original_len - leading_bytes - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr() + leading_bytes, new_len);
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

pub fn strTrimStart(input_string: RocStr) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref();
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const leading_bytes = countLeadingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == leading_bytes) {
        string.decref();
        return RocStr.empty();
    }

    const new_len = original_len - leading_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr() + leading_bytes, new_len);
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

pub fn strTrimEnd(input_string: RocStr) callconv(.C) RocStr {
    var string = input_string;

    if (string.isEmpty()) {
        string.decref();
        return RocStr.empty();
    }

    const bytes_ptr = string.asU8ptrMut();

    const trailing_bytes = countTrailingWhitespaceBytes(string);
    const original_len = string.len();

    if (original_len == trailing_bytes) {
        string.decref();
        return RocStr.empty();
    }

    const new_len = original_len - trailing_bytes;

    if (string.isSmallStr()) {
        // Just create another small string of the correct bytes.
        // No need to decref because it is a small string.
        return RocStr.init(string.asU8ptr(), new_len);
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

fn rcNone(_: ?[*]u8) callconv(.C) void {}

fn decStr(ptr: ?[*]u8) callconv(.C) void {
    const str_ptr = @as(*RocStr, @ptrCast(@alignCast(ptr orelse unreachable)));
    str_ptr.decref();
}

/// A backwards version of Utf8View from std.unicode
const ReverseUtf8View = struct {
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
            2 => unicode.utf8Decode2(slice) catch unreachable,
            3 => unicode.utf8Decode3(slice) catch unreachable,
            4 => unicode.utf8Decode4(slice) catch unreachable,
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

test "strTrim: empty" {
    const trimmedEmpty = strTrim(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrim: null byte" {
    const bytes = [_]u8{0};
    const original = RocStr.init(&bytes, 1);

    try expectEqual(@as(usize, 1), original.len());
    try expectEqual(@as(usize, SMALL_STR_MAX_LENGTH), original.getCapacity());

    const original_with_capacity = reserve(original, 40);
    defer original_with_capacity.decref();

    try expectEqual(@as(usize, 1), original_with_capacity.len());
    try expectEqual(@as(usize, 64), original_with_capacity.getCapacity());

    const trimmed = strTrim(original.clone());
    defer trimmed.decref();

    try expect(original.eq(trimmed));
}

test "strTrim: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    const trimmed = strTrim(original);
    defer trimmed.decref();

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrim: large to large" {
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(!expected.isSmallStr());

    const trimmed = strTrim(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
}

test "strTrim: large to small sized slice" {
    const original_bytes = "             hello         ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    try expect(original.isUnique());
    const trimmed = strTrim(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrim: small to small" {
    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrim(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimStart: empty" {
    const trimmedEmpty = strTrimStart(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimStart: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    const trimmed = strTrimStart(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimStart: large to large" {
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(!expected.isSmallStr());

    const trimmed = strTrimStart(original);

    try expect(trimmed.eq(expected));
}

test "strTrimStart: large to small" {
    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = "                    hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimStart(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrimStart: small to small" {
    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimStart(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimEnd: empty" {
    const trimmedEmpty = strTrimEnd(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimEnd: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    const trimmed = strTrimEnd(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimEnd: large to large" {
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(!original.isSmallStr());

    const expected_bytes = " hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(!expected.isSmallStr());

    const trimmed = strTrimEnd(original);

    try expect(trimmed.eq(expected));
}

test "strTrimEnd: large to small" {
    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = " hello                    ";
    const original = RocStr.init(original_bytes, original_bytes.len);

    try expect(!original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimEnd(original);
    defer trimmed.decref();

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrimEnd: small to small" {
    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.decref();

    try expect(original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.decref();

    try expect(expected.isSmallStr());

    const trimmed = strTrimEnd(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "ReverseUtf8View: hello world" {
    const original_bytes = "hello world";
    const expected_bytes = "dlrow olleh";

    var i: usize = 0;
    var iter = ReverseUtf8View.initUnchecked(original_bytes).iterator();
    while (iter.nextCodepoint()) |codepoint| {
        try expect(expected_bytes[i] == codepoint);
        i += 1;
    }
}

test "ReverseUtf8View: empty" {
    const original_bytes = "";

    var iter = ReverseUtf8View.initUnchecked(original_bytes).iterator();
    while (iter.nextCodepoint()) |_| {
        try expect(false);
    }
}

test "capacity: small string" {
    const data_bytes = "foobar";
    var data = RocStr.init(data_bytes, data_bytes.len);
    defer data.decref();

    try expectEqual(data.getCapacity(), SMALL_STR_MAX_LENGTH);
}

test "capacity: big string" {
    const data_bytes = "a string so large that it must be heap-allocated";
    var data = RocStr.init(data_bytes, data_bytes.len);
    defer data.decref();

    try expect(data.getCapacity() >= data_bytes.len);
}

pub fn reserveC(string: RocStr, spare_u64: u64) callconv(.C) RocStr {
    return reserve(string, @intCast(spare_u64));
}

fn reserve(string: RocStr, spare: usize) RocStr {
    const old_length = string.len();

    if (string.getCapacity() >= old_length + spare) {
        return string;
    } else {
        var output = string.reallocate(old_length + spare);
        output.setLen(old_length);
        return output;
    }
}

pub fn withCapacityC(capacity: u64) callconv(.C) RocStr {
    var str = RocStr.allocate(@intCast(capacity));
    str.setLen(0);
    return str;
}

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

pub fn strAllocationPtr(
    string: RocStr,
) callconv(.C) ?[*]u8 {
    return string.getAllocationPtr();
}

pub fn strReleaseExcessCapacity(
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
        string.decref();
        return RocStr.empty();
    } else {
        var output = RocStr.allocateExact(old_length);
        const source_ptr = string.asU8ptr();
        const dest_ptr = output.asU8ptrMut();

        @memcpy(dest_ptr[0..old_length], source_ptr[0..old_length]);
        string.decref();

        return output;
    }
}
