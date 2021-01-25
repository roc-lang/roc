const std = @import("std");
const mem = std.mem;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const Allocator = mem.Allocator;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

const InPlace = packed enum(u8) {
    InPlace,
    Clone,
};

pub const RocStr = extern struct {
    str_bytes: ?[*]u8,
    str_len: usize,

    pub inline fn empty() RocStr {
        return RocStr{
            .str_len = 0,
            .str_bytes = null,
        };
    }

    // This clones the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(allocator: *Allocator, bytes_ptr: [*]const u8, length: usize) RocStr {
        const roc_str_size = @sizeOf(RocStr);

        if (length < roc_str_size) {
            var ret_small_str = RocStr.empty();
            const target_ptr = @ptrToInt(&ret_small_str);
            var index: u8 = 0;

            // TODO isn't there a way to bulk-zero data in Zig?
            // Zero out the data, just to be safe
            while (index < roc_str_size) {
                var offset_ptr = @intToPtr(*u8, target_ptr + index);
                offset_ptr.* = 0;
                index += 1;
            }

            // TODO rewrite this into a for loop
            index = 0;
            while (index < length) {
                var offset_ptr = @intToPtr(*u8, target_ptr + index);
                offset_ptr.* = bytes_ptr[index];
                index += 1;
            }

            // set the final byte to be the length
            const final_byte_ptr = @intToPtr(*u8, target_ptr + roc_str_size - 1);
            final_byte_ptr.* = @truncate(u8, length) ^ 0b10000000;

            return ret_small_str;
        } else {
            var result = RocStr.initBig(allocator, u64, InPlace.Clone, length);

            @memcpy(@ptrCast([*]u8, result.str_bytes), bytes_ptr, length);

            return result;
        }
    }

    pub fn initBig(allocator: *Allocator, comptime T: type, in_place: InPlace, number_of_chars: u64) RocStr {
        const length = @sizeOf(T) + number_of_chars;
        var new_bytes: []T = allocator.alloc(T, length) catch unreachable;

        if (in_place == InPlace.InPlace) {
            new_bytes[0] = @intCast(T, number_of_chars);
        } else {
            new_bytes[0] = std.math.minInt(T);
        }

        var first_element = @ptrCast([*]align(@alignOf(T)) u8, new_bytes);
        first_element += @sizeOf(usize);

        return RocStr{
            .str_bytes = first_element,
            .str_len = number_of_chars,
        };
    }

    pub fn deinit(self: RocStr, allocator: *Allocator) void {
        if (!self.isSmallStr() and !self.isEmpty()) {
            const str_bytes_ptr: [*]u8 = self.str_bytes orelse unreachable;
            const str_bytes: []u8 = str_bytes_ptr[0..self.str_len];
            allocator.free(str_bytes);
        }
    }

    // This takes ownership of the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn withCapacity(length: usize) RocStr {
        const roc_str_size = @sizeOf(RocStr);

        if (length < roc_str_size) {
            return RocStr.empty();
        } else {
            var new_bytes: []T = allocator.alloc(u8, length) catch unreachable;

            var new_bytes_ptr: [*]u8 = @ptrCast([*]u8, &new_bytes);

            return RocStr{
                .str_bytes = new_bytes_ptr,
                .str_len = length,
            };
        }
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        const self_bytes_ptr: ?[*]const u8 = self.str_bytes;
        const other_bytes_ptr: ?[*]const u8 = other.str_bytes;

        // If they are byte-for-byte equal, they're definitely equal!
        if (self_bytes_ptr == other_bytes_ptr and self.str_len == other.str_len) {
            return true;
        }

        const self_len = self.len();
        const other_len = other.len();

        // If their lengths are different, they're definitely unequal.
        if (self_len != other_len) {
            return false;
        }

        const self_u8_ptr: [*]const u8 = @ptrCast([*]const u8, &self);
        const other_u8_ptr: [*]const u8 = @ptrCast([*]const u8, &other);
        const self_bytes: [*]const u8 = if (self.isSmallStr() or self.isEmpty()) self_u8_ptr else self_bytes_ptr orelse unreachable;
        const other_bytes: [*]const u8 = if (other.isSmallStr() or other.isEmpty()) other_u8_ptr else other_bytes_ptr orelse unreachable;

        var index: usize = 0;

        // TODO rewrite this into a for loop
        const length = self.len();
        while (index < length) {
            if (self_bytes[index] != other_bytes[index]) {
                return false;
            }

            index = index + 1;
        }

        return true;
    }

    pub fn clone(allocator: *Allocator, comptime T: type, in_place: InPlace, str: RocStr) RocStr {
        if (str.isSmallStr() or str.isEmpty()) {
            // just return the bytes
            return str;
        } else {
            var new_str = RocStr.initBig(allocator, T, in_place, str.str_len);

            var old_bytes: [*]u8 = @ptrCast([*]u8, str.str_bytes);
            var new_bytes: [*]u8 = @ptrCast([*]u8, new_str.str_bytes);

            @memcpy(new_bytes, old_bytes, str.str_len);

            return new_str;
        }
    }

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
        return self.len() == 0;
    }

    pub fn asSlice(self: RocStr) []u8 {
        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        return self.asU8ptr()[0..self.len()];
    }

    pub fn asU8ptr(self: RocStr) [*]u8 {
        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        return if (self.isSmallStr() or self.isEmpty()) (&@bitCast([16]u8, self)) else (@ptrCast([*]u8, self.str_bytes));
    }

    // Given a pointer to some bytes, write the first (len) bytes of this
    // RocStr's contents into it.
    //
    // One use for this function is writing into an `alloca` for a C string that
    // only needs to live long enough to be passed as an argument to
    // a C function - like the file path argument to `fopen`.
    pub fn memcpy(self: RocStr, dest: [*]u8, length: usize) void {
        const src = self.asU8ptr();
        @memcpy(dest, src, length);
    }

    test "RocStr.eq: equal" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "abc".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(testing.allocator, str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(testing.allocator, str2_ptr, str2_len);

        expect(roc_str1.eq(roc_str2));

        roc_str1.deinit(testing.allocator);
        roc_str2.deinit(testing.allocator);
    }

    test "RocStr.eq: not equal different length" {
        const str1_len = 4;
        var str1: [str1_len]u8 = "abcd".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(testing.allocator, str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(testing.allocator, str2_ptr, str2_len);

        defer {
            roc_str1.deinit(testing.allocator);
            roc_str2.deinit(testing.allocator);
        }

        expect(!roc_str1.eq(roc_str2));
    }

    test "RocStr.eq: not equal same length" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "acb".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(testing.allocator, str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(testing.allocator, str2_ptr, str2_len);

        defer {
            roc_str1.deinit(testing.allocator);
            roc_str2.deinit(testing.allocator);
        }

        expect(!roc_str1.eq(roc_str2));
    }
};

// Str.equal
pub fn strEqual(self: RocStr, other: RocStr) callconv(.C) bool {
    return self.eq(other);
}

// Str.numberOfBytes
pub fn strNumberOfBytes(string: RocStr) callconv(.C) usize {
    return string.len();
}

// Str.fromInt
// When we actually use this in Roc, libc will be linked so we have access to std.heap.c_allocator
pub fn strFromIntC(int: i64) callconv(.C) RocStr {
    return strFromInt(std.heap.c_allocator, int);
}

fn strFromInt(allocator: *Allocator, int: i64) RocStr {
    // prepare for having multiple integer types in the future
    return @call(.{ .modifier = always_inline }, strFromIntHelp, .{ allocator, i64, int });
}

fn strFromIntHelp(allocator: *Allocator, comptime T: type, int: T) RocStr {
    // determine maximum size for this T
    comptime const size = comptime blk: {
        // the string representation of the minimum i128 value uses at most 40 characters
        var buf: [40]u8 = undefined;
        var result = std.fmt.bufPrint(&buf, "{}", .{std.math.minInt(T)}) catch unreachable;
        break :blk result.len;
    };

    var buf: [size]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{}", .{int}) catch unreachable;

    return RocStr.init(allocator, &buf, result.len);
}

// Str.split
// When we actually use this in Roc, libc will be linked so we have access to std.heap.c_allocator
pub fn strSplitInPlaceC(array: [*]RocStr, string: RocStr, delimiter: RocStr) callconv(.C) void {
    return @call(.{ .modifier = always_inline }, strSplitInPlace, .{ std.heap.c_allocator, array, string, delimiter });
}

fn strSplitInPlace(allocator: *Allocator, array: [*]RocStr, string: RocStr, delimiter: RocStr) void {
    var ret_array_index: usize = 0;
    var slice_start_index: usize = 0;
    var str_index: usize = 0;

    const str_bytes = string.asU8ptr();
    const str_len = string.len();

    const delimiter_bytes_ptrs = delimiter.asU8ptr();
    const delimiter_len = delimiter.len();

    if (str_len > delimiter_len) {
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

                array[ret_array_index] = RocStr.init(allocator, str_bytes + slice_start_index, segment_len);
                slice_start_index = str_index + delimiter_len;
                ret_array_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    array[ret_array_index] = RocStr.init(allocator, str_bytes + slice_start_index, str_len - slice_start_index);
}

test "strSplitInPlace: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]
    const str_arr = "abc";
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, array_ptr, str, delimiter);

    var expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.deinit(testing.allocator);
        }

        for (expected) |roc_str| {
            roc_str.deinit(testing.allocator);
        }

        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
}

test "strSplitInPlace: empty end" {
    const str_arr = "1---- ---- ---- ---- ----2---- ---- ---- ---- ----";
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "---- ---- ---- ---- ----";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, array_ptr, str, delimiter);

    const one = RocStr.init(testing.allocator, "1", 1);
    const two = RocStr.init(testing.allocator, "2", 1);

    var expected = [3]RocStr{
        one, two, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.deinit(testing.allocator);
        }

        for (expected) |rocStr| {
            rocStr.deinit(testing.allocator);
        }

        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
    expect(array[1].eq(expected[1]));
    expect(array[2].eq(expected[2]));
}

test "strSplitInPlace: delimiter on sides" {
    const str_arr = "tttghittt";
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "ttt";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;
    strSplitInPlace(testing.allocator, array_ptr, str, delimiter);

    const ghi_arr = "ghi";
    const ghi = RocStr.init(testing.allocator, ghi_arr, ghi_arr.len);

    var expected = [3]RocStr{
        RocStr.empty(), ghi, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.deinit(testing.allocator);
        }

        for (expected) |rocStr| {
            rocStr.deinit(testing.allocator);
        }

        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
    expect(array[1].eq(expected[1]));
    expect(array[2].eq(expected[2]));
}

test "strSplitInPlace: three pieces" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    const str_arr = "a!b!c";
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, array_ptr, str, delimiter);

    const a = RocStr.init(testing.allocator, "a", 1);
    const b = RocStr.init(testing.allocator, "b", 1);
    const c = RocStr.init(testing.allocator, "c", 1);

    var expected_array = [array_len]RocStr{
        a, b, c,
    };

    defer {
        for (array) |roc_str| {
            roc_str.deinit(testing.allocator);
        }

        for (expected_array) |roc_str| {
            roc_str.deinit(testing.allocator);
        }

        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    expectEqual(expected_array.len, array.len);
    expect(array[0].eq(expected_array[0]));
    expect(array[1].eq(expected_array[1]));
    expect(array[2].eq(expected_array[2]));
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

    if (str_len > delimiter_len) {
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
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    const segments_count = countSegments(str, delimiter);
    expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments
    const str_arr = "hello there";
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "hello";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    const segments_count = countSegments(str, delimiter);

    expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments
    const str_arr = "a!b!c";
    const str = RocStr.init(testing.allocator, str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(testing.allocator, delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    const segments_count = countSegments(str, delimiter);

    expectEqual(segments_count, 3);
}

fn rocStrFromLiteral(bytes_arr: *const []u8) RocStr {}

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

test "startsWith: foo starts with fo" {
    const foo = RocStr.init(testing.allocator, "foo", 3);
    const fo = RocStr.init(testing.allocator, "fo", 2);
    expect(startsWith(foo, fo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str = RocStr.init(testing.allocator, "123456789123456789", 18);
    defer str.deinit(testing.allocator);
    expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.init(testing.allocator, "12345678912345678910", 20);
    defer str.deinit(testing.allocator);
    const prefix = RocStr.init(testing.allocator, "123456789123456789", 18);
    defer prefix.deinit(testing.allocator);

    expect(startsWith(str, prefix));
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
    const foo = RocStr.init(testing.allocator, "foo", 3);
    const oo = RocStr.init(testing.allocator, "oo", 2);
    defer foo.deinit(testing.allocator);
    defer oo.deinit(testing.allocator);

    expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init(testing.allocator, "123456789123456789", 18);
    defer str.deinit(testing.allocator);
    expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init(testing.allocator, "12345678912345678910", 20);
    const suffix = RocStr.init(testing.allocator, "345678912345678910", 18);
    defer str.deinit(testing.allocator);
    defer suffix.deinit(testing.allocator);

    expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init(testing.allocator, "hello world", 11);
    const suffix = RocStr.init(testing.allocator, "world", 5);
    defer str.deinit(testing.allocator);
    defer suffix.deinit(testing.allocator);

    expect(endsWith(str, suffix));
}

// Str.concat
// When we actually use this in Roc, libc will be linked so we have access to std.heap.c_allocator
pub fn strConcatC(ptr_size: u32, result_in_place: InPlace, arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, strConcat, .{ std.heap.c_allocator, ptr_size, result_in_place, arg1, arg2 });
}

fn strConcat(allocator: *Allocator, ptr_size: u32, result_in_place: InPlace, arg1: RocStr, arg2: RocStr) RocStr {
    return switch (ptr_size) {
        4 => strConcatHelp(allocator, i32, result_in_place, arg1, arg2),
        8 => strConcatHelp(allocator, i64, result_in_place, arg1, arg2),
        else => unreachable,
    };
}

fn strConcatHelp(allocator: *Allocator, comptime T: type, result_in_place: InPlace, arg1: RocStr, arg2: RocStr) RocStr {
    if (arg1.isEmpty()) {
        return RocStr.clone(allocator, T, result_in_place, arg2);
    } else if (arg2.isEmpty()) {
        return RocStr.clone(allocator, T, result_in_place, arg1);
    } else {
        const combined_length = arg1.len() + arg2.len();

        const small_str_bytes = 2 * @sizeOf(T);
        const result_is_big = combined_length >= small_str_bytes;

        if (result_is_big) {
            var result = RocStr.initBig(allocator, T, result_in_place, combined_length);

            {
                const old_bytes = arg1.asU8ptr();

                const new_bytes: [*]u8 = @ptrCast([*]u8, result.str_bytes);

                @memcpy(new_bytes, old_bytes, arg1.len());
            }

            {
                const old_bytes = arg2.asU8ptr();

                const new_bytes = @ptrCast([*]u8, result.str_bytes) + arg1.len();

                @memcpy(new_bytes, old_bytes, arg2.len());
            }

            return result;
        } else {
            var result = [16]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

            // if the result is small, then for sure arg1 and arg2 are also small

            {
                var old_bytes: [*]u8 = @ptrCast([*]u8, &@bitCast([16]u8, arg1));
                var new_bytes: [*]u8 = @ptrCast([*]u8, &result);

                @memcpy(new_bytes, old_bytes, arg1.len());
            }

            {
                var old_bytes: [*]u8 = @ptrCast([*]u8, &@bitCast([16]u8, arg2));
                var new_bytes = @ptrCast([*]u8, &result) + arg1.len();

                @memcpy(new_bytes, old_bytes, arg2.len());
            }

            const mask: u8 = 0b1000_0000;
            const final_byte = @truncate(u8, combined_length) | mask;

            result[small_str_bytes - 1] = final_byte;

            return @bitCast(RocStr, result);
        }

        return result;
    }
}

test "RocStr.concat: small concat small" {
    const str1_len = 3;
    var str1: [str1_len]u8 = "foo".*;
    const str1_ptr: [*]u8 = &str1;
    var roc_str1 = RocStr.init(testing.allocator, str1_ptr, str1_len);

    const str2_len = 3;
    var str2: [str2_len]u8 = "abc".*;
    const str2_ptr: [*]u8 = &str2;
    var roc_str2 = RocStr.init(testing.allocator, str2_ptr, str2_len);

    const str3_len = 6;
    var str3: [str3_len]u8 = "fooabc".*;
    const str3_ptr: [*]u8 = &str3;
    var roc_str3 = RocStr.init(testing.allocator, str3_ptr, str3_len);

    defer {
        roc_str1.deinit(testing.allocator);
        roc_str2.deinit(testing.allocator);
        roc_str3.deinit(testing.allocator);
    }

    const result = strConcat(testing.allocator, 8, InPlace.Clone, roc_str1, roc_str2);

    defer result.deinit(testing.allocator);

    expect(roc_str3.eq(result));
}
