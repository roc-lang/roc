const utils = @import("utils.zig");
const RocList = @import("list.zig").RocList;
const UpdateMode = utils.UpdateMode;
const std = @import("std");
const mem = std.mem;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expect = testing.expect;

const InPlace = enum(u8) {
    InPlace,
    Clone,
};

const SMALL_STR_MAX_LENGTH = small_string_size - 1;
const small_string_size = @sizeOf(RocStr);
const blank_small_string: [@sizeOf(RocStr)]u8 = init_blank_small_string(small_string_size);

fn init_blank_small_string(comptime n: usize) [n]u8 {
    var prime_list: [n]u8 = undefined;

    var i = 0;
    while (i < n) : (i += 1) {
        prime_list[i] = 0;
    }

    return prime_list;
}

pub const RocStr = extern struct {
    str_bytes: ?[*]u8,
    str_len: usize,

    pub const alignment = @alignOf(usize);

    pub inline fn empty() RocStr {
        const small_str_flag: isize = std.math.minInt(isize);
        return RocStr{
            .str_len = @bitCast(usize, small_str_flag),
            .str_bytes = null,
        };
    }

    // This clones the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(bytes_ptr: [*]const u8, length: usize) RocStr {
        var result = RocStr.allocate(InPlace.Clone, length);
        @memcpy(result.asU8ptr(), bytes_ptr, length);

        return result;
    }

    pub fn initBig(_: InPlace, number_of_chars: usize) RocStr {
        const first_element = utils.allocateWithRefcount(number_of_chars, @sizeOf(usize));

        return RocStr{
            .str_bytes = first_element,
            .str_len = number_of_chars,
        };
    }

    // allocate space for a (big or small) RocStr, but put nothing in it yet
    pub fn allocate(result_in_place: InPlace, number_of_chars: usize) RocStr {
        const result_is_big = number_of_chars >= small_string_size;

        if (result_is_big) {
            return RocStr.initBig(result_in_place, number_of_chars);
        } else {
            var t = blank_small_string;

            const mask: u8 = 0b1000_0000;
            const final_byte = @truncate(u8, number_of_chars) | mask;

            t[small_string_size - 1] = final_byte;

            return @bitCast(RocStr, t);
        }
    }

    pub fn deinit(self: RocStr) void {
        if (!self.isSmallStr()) {
            utils.decref(self.str_bytes, self.str_len, RocStr.alignment);
        }
    }

    // This takes ownership of the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn withCapacity(length: usize) RocStr {
        const roc_str_size = @sizeOf(RocStr);

        if (length < roc_str_size) {
            return RocStr.empty();
        } else {
            var new_bytes = utils.alloc(length, RocStr.alignment) catch unreachable;

            var new_bytes_ptr: [*]u8 = @ptrCast([*]u8, &new_bytes);

            return RocStr{
                .str_bytes = new_bytes_ptr,
                .str_len = length,
            };
        }
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        // If they are byte-for-byte equal, they're definitely equal!
        if (self.str_bytes == other.str_bytes and self.str_len == other.str_len) {
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

        // It's faster to compare pointer-sized words rather than bytes, as far as possible
        // The bytes are always pointer-size aligned due to the refcount
        const self_words = @ptrCast([*]const usize, @alignCast(@alignOf(usize), self_bytes));
        const other_words = @ptrCast([*]const usize, @alignCast(@alignOf(usize), other_bytes));
        var w: usize = 0;
        while (w < self_len / @sizeOf(usize)) : (w += 1) {
            if (self_words[w] != other_words[w]) {
                return false;
            }
        }

        // Compare the leftover bytes
        var b = w * @sizeOf(usize);
        while (b < self_len) : (b += 1) {
            if (self_bytes[b] != other_bytes[b]) {
                return false;
            }
        }

        return true;
    }

    pub fn clone(in_place: InPlace, str: RocStr) RocStr {
        if (str.isSmallStr()) {
            // just return the bytes
            return str;
        } else {
            var new_str = RocStr.initBig(in_place, str.str_len);

            var old_bytes: [*]u8 = @ptrCast([*]u8, str.str_bytes);
            var new_bytes: [*]u8 = @ptrCast([*]u8, new_str.str_bytes);

            @memcpy(new_bytes, old_bytes, str.str_len);

            return new_str;
        }
    }

    pub fn reallocate(
        self: RocStr,
        new_length: usize,
    ) RocStr {
        const element_width = 1;

        if (self.str_bytes) |source_ptr| {
            if (self.isUnique()) {
                const new_source = utils.unsafeReallocate(source_ptr, RocStr.alignment, self.len(), new_length, element_width);

                return RocStr{ .str_bytes = new_source, .str_len = new_length };
            }
        }

        return self.reallocateFresh(new_length);
    }

    /// reallocate by explicitly making a new allocation and copying elements over
    pub fn reallocateFresh(
        self: RocStr,
        new_length: usize,
    ) RocStr {
        const old_length = self.len();
        const delta_length = new_length - old_length;

        const result = RocStr.allocate(InPlace.Clone, new_length);

        // transfer the memory

        const source_ptr = self.asU8ptr();
        const dest_ptr = result.asU8ptr();

        @memcpy(dest_ptr, source_ptr, old_length);
        @memset(dest_ptr + old_length, 0, delta_length);

        self.deinit();

        return result;
    }

    // NOTE: returns false for empty string!
    pub fn isSmallStr(self: RocStr) bool {
        return @bitCast(isize, self.str_len) < 0;
    }

    pub fn len(self: RocStr) usize {
        const bytes: [*]const u8 = @ptrCast([*]const u8, &self);
        const last_byte = bytes[@sizeOf(RocStr) - 1];
        const small_len = @as(usize, last_byte ^ 0b1000_0000);
        const big_len = self.str_len;

        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        return if (self.isSmallStr()) small_len else big_len;
    }

    pub fn isEmpty(self: RocStr) bool {
        comptime const empty_len = RocStr.empty().str_len;
        return self.str_len == empty_len;
    }

    // If a string happens to be null-terminated already, then we can pass its
    // bytes directly to functions (e.g. for opening files) that require
    // null-terminated strings. Otherwise, we need to allocate and copy a new
    // null-terminated string, which has a much higher performance cost!
    fn isNullTerminated(self: RocStr) bool {
        const length = self.len();
        const longest_small_str = @sizeOf(RocStr) - 1;

        // NOTE: We want to compare length here, *NOT* check for is_small_str!
        // This is because we explicitly want the empty string to be handled in
        // this branch, even though the empty string is not a small string.
        //
        // (The other branch dereferences the bytes pointer, which is not safe
        // to do for the empty string.)
        if (length <= longest_small_str) {
            // If we're a small string, then usually the next byte after the
            // end of the string will be zero. (Small strings set all their
            // unused bytes to 0, so that comparison for equality can be fast.)
            //
            // However, empty strings are *not* null terminated, so if this is
            // empty, it should return false.
            //
            // Also, if we are exactly a maximum-length small string,
            // then the next byte is off the end of the struct;
            // in that case, we are also not null-terminated!
            return length != 0 and length != longest_small_str;
        } else {
            // This is a big string, and it's not empty, so we can safely
            // dereference the pointer.
            const ptr: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.str_bytes));
            const capacity_or_refcount: isize = (ptr - 1)[0];

            // If capacity_or_refcount is positive, then it's a capacity value.
            //
            // If we have excess capacity, then we can safely read the next
            // byte after the end of the string. Maybe it happens to be zero!
            if (capacity_or_refcount > @intCast(isize, length)) {
                return self.str_bytes[length] == 0;
            } else {
                // This string was refcounted or immortal; we can't safely read
                // the next byte, so assume the string is not null-terminated.
                return false;
            }
        }
    }

    // Returns (@sizeOf(RocStr) - 1) for small strings and the empty string.
    // Returns 0 for refcounted strings and immortal strings.
    // Returns the stored capacity value for all other strings.
    pub fn capacity(self: RocStr) usize {
        const length = self.len();
        const longest_small_str = @sizeOf(RocStr) - 1;

        if (length <= longest_small_str) {
            // Note that although empty strings technically have the full
            // capacity of a small string available, they aren't marked as small
            // strings, so if you want to make use of that capacity, you need
            // to first change its flag to mark it as a small string!
            return longest_small_str;
        } else {
            const ptr: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.str_bytes));
            const capacity_or_refcount: isize = (ptr - 1)[0];

            if (capacity_or_refcount > 0) {
                // If capacity_or_refcount is positive, that means it's a
                // capacity value.
                return capacity_or_refcount;
            } else {
                // This is either a refcount or else this big string is stored
                // in a readonly section; either way, it has no capacity,
                // because we cannot mutate it in-place!
                return 0;
            }
        }
    }

    pub fn isUnique(self: RocStr) bool {
        // small strings can be copied
        if (self.isSmallStr()) {
            return true;
        }

        // otherwise, check if the refcount is one
        return @call(.{ .modifier = always_inline }, RocStr.isRefcountOne, .{self});
    }

    fn isRefcountOne(self: RocStr) bool {
        const ptr: [*]usize = @ptrCast([*]usize, @alignCast(@alignOf(usize), self.str_bytes));
        return (ptr - 1)[0] == utils.REFCOUNT_ONE;
    }

    pub fn asSlice(self: RocStr) []u8 {
        return self.asU8ptr()[0..self.len()];
    }

    pub fn asU8ptr(self: RocStr) [*]u8 {

        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        // return if (self.isSmallStr()) (&@bitCast([@sizeOf(RocStr)]u8, self)) else (@ptrCast([*]u8, self.str_bytes));
        if (self.isSmallStr()) {
            const as_int = @ptrToInt(&self);
            const as_ptr = @intToPtr([*]u8, as_int);
            return as_ptr;
        } else {
            return @ptrCast([*]u8, self.str_bytes);
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
        @memcpy(dest, src, self.len());
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

        roc_str1.deinit();
        roc_str2.deinit();
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
            roc_str1.deinit();
            roc_str2.deinit();
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
            roc_str1.deinit();
            roc_str2.deinit();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, equal" {
        const content = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content, content.len);
        const roc_str2 = RocStr.init(content, content.len);

        defer {
            roc_str1.deinit();
            roc_str2.deinit();
        }

        try expect(roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, different lengths, unequal" {
        const content1 = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content1, content1.len);
        const content2 = "012345678901234567890";
        const roc_str2 = RocStr.init(content2, content2.len);

        defer {
            roc_str1.deinit();
            roc_str2.deinit();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, different content, unequal" {
        const content1 = "012345678901234567890123456789!!";
        const roc_str1 = RocStr.init(content1, content1.len);
        const content2 = "012345678901234567890123456789--";
        const roc_str2 = RocStr.init(content2, content2.len);

        defer {
            roc_str1.deinit();
            roc_str2.deinit();
        }

        try expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: large, garbage after end, equal" {
        const content = "012345678901234567890123456789";
        const roc_str1 = RocStr.init(content, content.len);
        const roc_str2 = RocStr.init(content, content.len);
        try expect(roc_str1.str_bytes != roc_str2.str_bytes);

        // Insert garbage after the end of each string
        roc_str1.str_bytes.?[30] = '!';
        roc_str1.str_bytes.?[31] = '!';
        roc_str2.str_bytes.?[30] = '-';
        roc_str2.str_bytes.?[31] = '-';

        defer {
            roc_str1.deinit();
            roc_str2.deinit();
        }

        try expect(roc_str1.eq(roc_str2));
    }
};

pub fn init(bytes_ptr: [*]const u8, length: usize) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, RocStr.init, .{ bytes_ptr, length });
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
    comptime var f = struct {
        fn func(int: T) callconv(.C) RocStr {
            return @call(.{ .modifier = always_inline }, strFromIntHelp, .{ T, int });
        }
    }.func;

    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

fn strFromIntHelp(comptime T: type, int: T) RocStr {
    // determine maximum size for this T
    const size = comptime blk: {
        // the string representation of the minimum i128 value uses at most 40 characters
        var buf: [40]u8 = undefined;
        var result = std.fmt.bufPrint(&buf, "{}", .{std.math.minInt(T)}) catch unreachable;
        break :blk result.len;
    };

    var buf: [size]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{}", .{int}) catch unreachable;

    return RocStr.init(&buf, result.len);
}

// Str.fromFloat
pub fn strFromFloatC(float: f64) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, strFromFloatHelp, .{ f64, float });
}

fn strFromFloatHelp(comptime T: type, float: T) RocStr {
    var buf: [100]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{d}", .{float}) catch unreachable;

    return RocStr.init(&buf, result.len);
}

// Str.split
pub fn strSplitInPlaceC(array: [*]RocStr, string: RocStr, delimiter: RocStr) callconv(.C) void {
    return @call(.{ .modifier = always_inline }, strSplitInPlace, .{ array, string, delimiter });
}

fn strSplitInPlace(array: [*]RocStr, string: RocStr, delimiter: RocStr) void {
    var ret_array_index: usize = 0;
    var slice_start_index: usize = 0;
    var str_index: usize = 0;

    const str_bytes = string.asU8ptr();
    const str_len = string.len();

    const delimiter_bytes_ptrs = delimiter.asU8ptr();
    const delimiter_len = delimiter.len();

    if (str_len > delimiter_len and delimiter_len > 0) {
        const end_index: usize = str_len - delimiter_len + 1;
        while (str_index <= end_index) {
            var delimiter_index: usize = 0;
            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                var delimiterChar = delimiter_bytes_ptrs[delimiter_index];
                var strChar = str_bytes[str_index + delimiter_index];

                if (delimiterChar != strChar) {
                    matches_delimiter = false;
                    break;
                }

                delimiter_index += 1;
            }

            if (matches_delimiter) {
                const segment_len: usize = str_index - slice_start_index;

                array[ret_array_index] = RocStr.init(str_bytes + slice_start_index, segment_len);
                slice_start_index = str_index + delimiter_len;
                ret_array_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    array[ret_array_index] = RocStr.init(str_bytes + slice_start_index, str_len - slice_start_index);
}

test "strSplitInPlace: empty delimiter" {
    // Str.split "abc" "" == [ "abc" ]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(array_ptr, str, delimiter);

    var expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.deinit();
        }

        for (expected) |roc_str| {
            roc_str.deinit();
        }

        str.deinit();
        delimiter.deinit();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitInPlace: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(array_ptr, str, delimiter);

    var expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.deinit();
        }

        for (expected) |roc_str| {
            roc_str.deinit();
        }

        str.deinit();
        delimiter.deinit();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitInPlace: empty end" {
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

    strSplitInPlace(array_ptr, str, delimiter);

    const one = RocStr.init("1", 1);
    const two = RocStr.init("2", 1);

    var expected = [3]RocStr{
        one, two, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.deinit();
        }

        for (expected) |rocStr| {
            rocStr.deinit();
        }

        str.deinit();
        delimiter.deinit();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitInPlace: delimiter on sides" {
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
    strSplitInPlace(array_ptr, str, delimiter);

    const ghi_arr = "ghi";
    const ghi = RocStr.init(ghi_arr, ghi_arr.len);

    var expected = [3]RocStr{
        RocStr.empty(), ghi, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.deinit();
        }

        for (expected) |rocStr| {
            rocStr.deinit();
        }

        str.deinit();
        delimiter.deinit();
    }

    try expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitInPlace: three pieces" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(array_ptr, str, delimiter);

    const a = RocStr.init("a", 1);
    const b = RocStr.init("b", 1);
    const c = RocStr.init("c", 1);

    var expected_array = [array_len]RocStr{
        a, b, c,
    };

    defer {
        for (array) |roc_str| {
            roc_str.deinit();
        }

        for (expected_array) |roc_str| {
            roc_str.deinit();
        }

        str.deinit();
        delimiter.deinit();
    }

    try expectEqual(expected_array.len, array.len);
    try expect(array[0].eq(expected_array[0]));
    try expect(array[1].eq(expected_array[1]));
    try expect(array[2].eq(expected_array[2]));
}

// This is used for `Str.split : Str, Str -> Array Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
pub fn countSegments(string: RocStr, delimiter: RocStr) callconv(.C) usize {
    const str_bytes = string.asU8ptr();
    const str_len = string.len();

    const delimiter_bytes_ptrs = delimiter.asU8ptr();
    const delimiter_len = delimiter.len();

    var count: usize = 1;

    if (str_len > delimiter_len and delimiter_len > 0) {
        var str_index: usize = 0;
        const end_cond: usize = str_len - delimiter_len + 1;

        while (str_index < end_cond) {
            var delimiter_index: usize = 0;

            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                const delimiterChar = delimiter_bytes_ptrs[delimiter_index];
                const strChar = str_bytes[str_index + delimiter_index];

                if (delimiterChar != strChar) {
                    matches_delimiter = false;
                    break;
                }

                delimiter_index += 1;
            }

            if (matches_delimiter) {
                count += 1;
            }

            str_index += 1;
        }
    }

    return count;
}

test "countSegments: long delimiter" {
    // Str.split "str" "delimiter" == [ "str" ]
    // 1 segment
    const str_arr = "str";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit();
        delimiter.deinit();
    }

    const segments_count = countSegments(str, delimiter);
    try expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments
    const str_arr = "hello there";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "hello";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit();
        delimiter.deinit();
    }

    const segments_count = countSegments(str, delimiter);

    try expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit();
        delimiter.deinit();
    }

    const segments_count = countSegments(str, delimiter);

    try expectEqual(segments_count, 3);
}

// Str.countGraphemeClusters
const grapheme = @import("helpers/grapheme.zig");
pub fn countGraphemeClusters(string: RocStr) callconv(.C) usize {
    if (string.isEmpty()) {
        return 0;
    }

    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    var bytes = bytes_ptr[0..bytes_len];
    var iter = (unicode.Utf8View.init(bytes) catch unreachable).iterator();

    var count: usize = 0;
    var grapheme_break_state: ?grapheme.BoundClass = null;
    var grapheme_break_state_ptr = &grapheme_break_state;
    var opt_last_codepoint: ?u21 = null;
    while (iter.nextCodepoint()) |cur_codepoint| {
        if (opt_last_codepoint) |last_codepoint| {
            var did_break = grapheme.isGraphemeBreak(last_codepoint, cur_codepoint, grapheme_break_state_ptr);
            if (did_break) {
                count += 1;
                grapheme_break_state = null;
            }
        }
        opt_last_codepoint = cur_codepoint;
    }

    // If there are no breaks, but the str is not empty, then there
    // must be a single grapheme
    if (bytes_len != 0) {
        count += 1;
    }

    return count;
}

test "countGraphemeClusters: empty string" {
    const count = countGraphemeClusters(RocStr.empty());
    try expectEqual(count, 0);
}

test "countGraphemeClusters: ascii characters" {
    const bytes_arr = "abcd";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    try expectEqual(count, 4);
}

test "countGraphemeClusters: utf8 characters" {
    const bytes_arr = "ãxā";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    try expectEqual(count, 3);
}

test "countGraphemeClusters: emojis" {
    const bytes_arr = "🤔🤔🤔";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    try expectEqual(count, 3);
}

test "countGraphemeClusters: emojis and ut8 characters" {
    const bytes_arr = "🤔å🤔¥🤔ç";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    try expectEqual(count, 6);
}

test "countGraphemeClusters: emojis, ut8, and ascii characters" {
    const bytes_arr = "6🤔å🤔e¥🤔çpp";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    try expectEqual(count, 10);
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
pub fn repeat(string: RocStr, count: usize) callconv(.C) RocStr {
    const bytes_len = string.len();
    const bytes_ptr = string.asU8ptr();

    var ret_string = RocStr.allocate(.Clone, count * bytes_len);
    var ret_string_ptr = ret_string.asU8ptr();

    var i: usize = 0;
    while (i < count) : (i += 1) {
        @memcpy(ret_string_ptr + (i * bytes_len), bytes_ptr, bytes_len);
    }

    return ret_string;
}

// Str.startsWithCodePt
pub fn startsWithCodePt(string: RocStr, prefix: u32) callconv(.C) bool {
    const bytes_ptr = string.asU8ptr();

    var buffer: [4]u8 = undefined;

    var width = std.unicode.utf8Encode(@truncate(u21, prefix), &buffer) catch unreachable;

    var i: usize = 0;
    while (i < width) : (i += 1) {
        const a = buffer[i];
        const b = bytes_ptr[i];
        if (a != b) {
            return false;
        }
    }

    return true;
}

test "startsWithCodePt: ascii char" {
    const whole = RocStr.init("foobar", 6);
    const prefix = 'f';
    try expect(startsWithCodePt(whole, prefix));
}

test "startsWithCodePt: emoji" {
    const yes = RocStr.init("💖foobar", 10);
    const no = RocStr.init("foobar", 6);
    const prefix = '💖';
    try expect(startsWithCodePt(yes, prefix));
    try expect(!startsWithCodePt(no, prefix));
}

test "startsWith: foo starts with fo" {
    const foo = RocStr.init("foo", 3);
    const fo = RocStr.init("fo", 2);
    try expect(startsWith(foo, fo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    defer str.deinit();
    try expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.init("12345678912345678910", 20);
    defer str.deinit();
    const prefix = RocStr.init("123456789123456789", 18);
    defer prefix.deinit();

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
    defer foo.deinit();
    defer oo.deinit();

    try expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    defer str.deinit();
    try expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init("12345678912345678910", 20);
    const suffix = RocStr.init("345678912345678910", 18);
    defer str.deinit();
    defer suffix.deinit();

    try expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init("hello world", 11);
    const suffix = RocStr.init("world", 5);
    defer str.deinit();
    defer suffix.deinit();

    try expect(endsWith(str, suffix));
}

// Str.concat
pub fn strConcatC(arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, strConcat, .{ arg1, arg2 });
}

fn strConcat(arg1: RocStr, arg2: RocStr) RocStr {
    if (arg1.isEmpty()) {
        // the second argument is borrowed, so we must increment its refcount before returning
        const result_in_place = InPlace.Clone;
        return RocStr.clone(result_in_place, arg2);
    } else if (arg2.isEmpty()) {
        // the first argument is owned, so we can return it without cloning
        return arg1;
    } else {
        const combined_length = arg1.len() + arg2.len();

        const element_width = 1;

        if (!arg1.isSmallStr() and arg1.isUnique()) {
            if (arg1.str_bytes) |source_ptr| {
                const new_source = utils.unsafeReallocate(
                    source_ptr,
                    RocStr.alignment,
                    arg1.len(),
                    combined_length,
                    element_width,
                );

                @memcpy(new_source + arg1.len(), arg2.asU8ptr(), arg2.len());

                return RocStr{ .str_bytes = new_source, .str_len = combined_length };
            }
        }

        var result = arg1.reallocateFresh(combined_length);
        var result_ptr = result.asU8ptr();

        arg1.memcpy(result_ptr);
        arg2.memcpy(result_ptr + arg1.len());

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
        roc_str1.deinit();
        roc_str2.deinit();
        roc_str3.deinit();
    }

    const result = strConcat(roc_str1, roc_str2);

    defer result.deinit();

    try expect(roc_str3.eq(result));
}

pub const RocListStr = extern struct {
    list_elements: ?[*]RocStr,
    list_length: usize,
};

// Str.joinWith
pub fn strJoinWithC(list: RocListStr, separator: RocStr) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, strJoinWith, .{ list, separator });
}

fn strJoinWith(list: RocListStr, separator: RocStr) RocStr {
    const len = list.list_length;

    if (len == 0) {
        return RocStr.empty();
    } else {
        const ptr = @ptrCast([*]RocStr, list.list_elements);
        const slice: []RocStr = ptr[0..len];

        // determine the size of the result
        var total_size: usize = 0;
        for (slice) |substr| {
            total_size += substr.len();
        }

        // include size of the separator
        total_size += separator.len() * (len - 1);

        var result = RocStr.allocate(InPlace.Clone, total_size);
        var result_ptr = result.asU8ptr();

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
    const list = RocListStr{ .list_length = 3, .list_elements = @ptrCast([*]RocStr, &elements) };

    defer {
        roc_sep.deinit();
        roc_elem.deinit();
        roc_result.deinit();
    }

    const result = strJoinWith(list, roc_sep);

    defer result.deinit();

    try expect(roc_result.eq(result));
}

// Str.toUtf8
pub fn strToUtf8C(arg: RocStr) callconv(.C) RocList {
    return strToBytes(arg);
}

inline fn strToBytes(arg: RocStr) RocList {
    if (arg.isEmpty()) {
        return RocList.empty();
    } else if (arg.isSmallStr()) {
        const length = arg.len();
        const ptr = utils.allocateWithRefcount(length, RocStr.alignment);

        @memcpy(ptr, arg.asU8ptr(), length);

        return RocList{ .length = length, .bytes = ptr };
    } else {
        return RocList{ .length = arg.len(), .bytes = arg.str_bytes };
    }
}

const FromUtf8Result = extern struct {
    byte_index: usize,
    string: RocStr,
    is_ok: bool,
    problem_code: Utf8ByteProblem,
};

const CountAndStart = extern struct {
    count: usize,
    start: usize,
};

pub fn fromUtf8C(arg: RocList, update_mode: UpdateMode, output: *FromUtf8Result) callconv(.C) void {
    output.* = fromUtf8(arg, update_mode);
}

inline fn fromUtf8(arg: RocList, update_mode: UpdateMode) FromUtf8Result {
    const bytes = @ptrCast([*]const u8, arg.bytes)[0..arg.length];

    if (unicode.utf8ValidateSlice(bytes)) {
        // the output will be correct. Now we need to take ownership of the input
        if (arg.len() <= SMALL_STR_MAX_LENGTH) {
            // turn the bytes into a small string
            const string = RocStr.init(@ptrCast([*]u8, arg.bytes), arg.len());

            // then decrement the input list
            const data_bytes = arg.len();
            utils.decref(arg.bytes, data_bytes, RocStr.alignment);

            return FromUtf8Result{
                .is_ok = true,
                .string = string,
                .byte_index = 0,
                .problem_code = Utf8ByteProblem.InvalidStartByte,
            };
        } else {
            const byte_list = arg.makeUniqueExtra(RocStr.alignment, @sizeOf(u8), update_mode);

            const string = RocStr{ .str_bytes = byte_list.bytes, .str_len = byte_list.length };

            return FromUtf8Result{
                .is_ok = true,
                .string = string,
                .byte_index = 0,
                .problem_code = Utf8ByteProblem.InvalidStartByte,
            };
        }
    } else {
        const temp = errorToProblem(@ptrCast([*]u8, arg.bytes), arg.length);

        // consume the input list
        const data_bytes = arg.len();
        utils.decref(arg.bytes, data_bytes, RocStr.alignment);

        return FromUtf8Result{
            .is_ok = false,
            .string = RocStr.empty(),
            .byte_index = temp.index,
            .problem_code = temp.problem,
        };
    }
}

pub fn fromUtf8RangeC(arg: RocList, countAndStart: CountAndStart, output: *FromUtf8Result) callconv(.C) void {
    output.* = @call(.{ .modifier = always_inline }, fromUtf8Range, .{ arg, countAndStart });
}

fn fromUtf8Range(arg: RocList, countAndStart: CountAndStart) FromUtf8Result {
    const bytes = @ptrCast([*]const u8, arg.bytes)[countAndStart.start..countAndStart.count];

    if (unicode.utf8ValidateSlice(bytes)) {
        // the output will be correct. Now we need to clone the input
        const string = RocStr.init(@ptrCast([*]const u8, bytes), countAndStart.count);

        return FromUtf8Result{ .is_ok = true, .string = string, .byte_index = 0, .problem_code = Utf8ByteProblem.InvalidStartByte };
    } else {
        const temp = errorToProblem(@ptrCast([*]u8, arg.bytes), arg.length);
        return FromUtf8Result{ .is_ok = false, .string = RocStr.empty(), .byte_index = temp.index, .problem_code = temp.problem };
    }
}

fn errorToProblem(bytes: [*]u8, length: usize) struct { index: usize, problem: Utf8ByteProblem } {
    var index: usize = 0;

    while (index < length) {
        const nextNumBytes = numberOfNextCodepointBytes(bytes, length, index) catch |err| {
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

pub fn isValidUnicode(ptr: [*]u8, len: usize) callconv(.C) bool {
    const bytes: []u8 = ptr[0..len];
    return @call(.{ .modifier = always_inline }, unicode.utf8ValidateSlice, .{bytes});
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
pub fn numberOfNextCodepointBytes(ptr: [*]u8, len: usize, index: usize) Utf8DecodeError!usize {
    const codepoint_len = try unicode.utf8ByteSequenceLength(ptr[index]);
    const codepoint_end_index = index + codepoint_len;
    if (codepoint_end_index > len) {
        return error.UnexpectedEof;
    }
    _ = try unicode.utf8Decode(ptr[index..codepoint_end_index]);
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
    return fromUtf8(RocList{ .bytes = bytes, .length = length }, .Immutable);
}

fn validateUtf8BytesX(str: RocList) FromUtf8Result {
    return fromUtf8(str, .Immutable);
}

fn expectOk(result: FromUtf8Result) !void {
    try expectEqual(result.is_ok, true);
}

fn sliceHelp(bytes: [*]const u8, length: usize) RocList {
    var list = RocList.allocate(RocStr.alignment, length, @sizeOf(u8));
    @memcpy(list.bytes orelse unreachable, bytes, length);
    list.length = length;

    return list;
}

fn toErrUtf8ByteResponse(index: usize, problem: Utf8ByteProblem) FromUtf8Result {
    return FromUtf8Result{ .is_ok = false, .string = RocStr.empty(), .byte_index = index, .problem_code = problem };
}

// NOTE on memory: the validate function consumes a RC token of the input. Since
// we freshly created it (in `sliceHelp`), it has only one RC token, and input list will be deallocated.
//
// If we tested with big strings, we'd have to deallocate the output string, but never the input list

test "validateUtf8Bytes: ascii" {
    const raw = "abc";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: unicode œ" {
    const raw = "œ";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: unicode ∆" {
    const raw = "∆";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: emoji" {
    const raw = "💖";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: unicode ∆ in middle of array" {
    const raw = "œb∆c¬";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectOk(validateUtf8BytesX(list));
}

fn expectErr(list: RocList, index: usize, err: Utf8DecodeError, problem: Utf8ByteProblem) !void {
    const str_ptr = @ptrCast([*]u8, list.bytes);
    const str_len = list.length;

    try expectError(err, numberOfNextCodepointBytes(str_ptr, str_len, index));
    try expectEqual(toErrUtf8ByteResponse(index, problem), validateUtf8Bytes(str_ptr, str_len));
}

test "validateUtf8Bytes: invalid start byte" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "ab\x80c";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 2, error.Utf8InvalidStartByte, Utf8ByteProblem.InvalidStartByte);
}

test "validateUtf8Bytes: unexpected eof for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\xa0\xc0";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x80\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: overlong" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L451
    const raw = "abc\xf0\x80\x80\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8OverlongEncoding, Utf8ByteProblem.OverlongEncoding);
}

test "validateUtf8Bytes: codepoint out too large" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L465
    const raw = "abc\xf4\x90\x80\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    try expectErr(list, 3, error.Utf8CodepointTooLarge, Utf8ByteProblem.CodepointTooLarge);
}

test "validateUtf8Bytes: surrogate halves" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L468
    const raw = "abc\xed\xa0\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
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

pub fn strTrim(string: RocStr) callconv(.C) RocStr {
    if (string.str_bytes) |bytes_ptr| {
        const leading_bytes = countLeadingWhitespaceBytes(string);
        const original_len = string.len();

        if (original_len == leading_bytes) {
            string.deinit();
            return RocStr.empty();
        }

        const trailing_bytes = countTrailingWhitespaceBytes(string);
        const new_len = original_len - leading_bytes - trailing_bytes;

        const small_or_shared = new_len <= SMALL_STR_MAX_LENGTH or !string.isRefcountOne();
        if (small_or_shared) {
            return RocStr.init(string.asU8ptr() + leading_bytes, new_len);
        }

        // nonempty, large, and unique:

        if (leading_bytes > 0) {
            var i: usize = 0;
            while (i < new_len) : (i += 1) {
                const dest = bytes_ptr + i;
                const source = dest + leading_bytes;
                @memcpy(dest, source, 1);
            }
        }

        var new_string = string;
        new_string.str_len = new_len;

        return new_string;
    }

    return RocStr.empty();
}

pub fn strTrimLeft(string: RocStr) callconv(.C) RocStr {
    if (string.str_bytes) |bytes_ptr| {
        const leading_bytes = countLeadingWhitespaceBytes(string);
        const original_len = string.len();

        if (original_len == leading_bytes) {
            string.deinit();
            return RocStr.empty();
        }

        const new_len = original_len - leading_bytes;

        const small_or_shared = new_len <= SMALL_STR_MAX_LENGTH or !string.isRefcountOne();
        if (small_or_shared) {
            return RocStr.init(string.asU8ptr() + leading_bytes, new_len);
        }

        // nonempty, large, and unique:

        if (leading_bytes > 0) {
            var i: usize = 0;
            while (i < new_len) : (i += 1) {
                const dest = bytes_ptr + i;
                const source = dest + leading_bytes;
                @memcpy(dest, source, 1);
            }
        }

        var new_string = string;
        new_string.str_len = new_len;

        return new_string;
    }

    return RocStr.empty();
}

pub fn strTrimRight(string: RocStr) callconv(.C) RocStr {
    if (string.str_bytes) |bytes_ptr| {
        const trailing_bytes = countTrailingWhitespaceBytes(string);
        const original_len = string.len();

        if (original_len == trailing_bytes) {
            string.deinit();
            return RocStr.empty();
        }

        const new_len = original_len - trailing_bytes;

        const small_or_shared = new_len <= SMALL_STR_MAX_LENGTH or !string.isRefcountOne();
        if (small_or_shared) {
            return RocStr.init(string.asU8ptr(), new_len);
        }

        // nonempty, large, and unique:

        var i: usize = 0;
        while (i < new_len) : (i += 1) {
            const dest = bytes_ptr + i;
            const source = dest;
            @memcpy(dest, source, 1);
        }

        var new_string = string;
        new_string.str_len = new_len;

        return new_string;
    }

    return RocStr.empty();
}

fn countLeadingWhitespaceBytes(string: RocStr) usize {
    var byte_count: usize = 0;

    var bytes = string.asU8ptr()[0..string.len()];
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

    var bytes = string.asU8ptr()[0..string.len()];
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

test "strTrim: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    const trimmed = strTrim(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrim: large to large" {
    const original_bytes = " hello giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(!original.isSmallStr());

    const expected_bytes = "hello giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(!expected.isSmallStr());

    const trimmed = strTrim(original);

    try expect(trimmed.eq(expected));
}

test "strTrim: large to small" {
    const original_bytes = "             hello world         ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(!original.isSmallStr());

    const expected_bytes = "hello world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(expected.isSmallStr());

    const trimmed = strTrim(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrim: small to small" {
    const original_bytes = " hello world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(original.isSmallStr());

    const expected_bytes = "hello world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(expected.isSmallStr());

    const trimmed = strTrim(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimLeft: empty" {
    const trimmedEmpty = strTrimLeft(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimLeft: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimLeft: large to large" {
    const original_bytes = " hello giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(!original.isSmallStr());

    const expected_bytes = "hello giant world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(!expected.isSmallStr());

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(expected));
}

test "strTrimLeft: large to small" {
    const original_bytes = "                    hello world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(!original.isSmallStr());

    const expected_bytes = "hello world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(expected.isSmallStr());

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimLeft: small to small" {
    const original_bytes = " hello world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(original.isSmallStr());

    const expected_bytes = "hello world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(expected.isSmallStr());

    const trimmed = strTrimLeft(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimRight: empty" {
    const trimmedEmpty = strTrimRight(RocStr.empty());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimRight: blank" {
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    const trimmed = strTrimRight(original);

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimRight: large to large" {
    const original_bytes = " hello giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(!original.isSmallStr());

    const expected_bytes = " hello giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(!expected.isSmallStr());

    const trimmed = strTrimRight(original);

    try expect(trimmed.eq(expected));
}

test "strTrimRight: large to small" {
    const original_bytes = " hello world                    ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(!original.isSmallStr());

    const expected_bytes = " hello world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(expected.isSmallStr());

    const trimmed = strTrimRight(original);

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimRight: small to small" {
    const original_bytes = " hello world ";
    const original = RocStr.init(original_bytes, original_bytes.len);
    defer original.deinit();

    try expect(original.isSmallStr());

    const expected_bytes = " hello world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len);
    defer expected.deinit();

    try expect(expected.isSmallStr());

    const trimmed = strTrimRight(original);

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
    while (iter.nextCodepoint()) |codepoint| {
        try expect(false);
    }
}
