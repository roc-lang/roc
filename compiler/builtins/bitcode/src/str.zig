const std = @import("std");
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

extern fn malloc(size: usize) ?*u8;
extern fn free([*]u8) void;

const RocStr = extern struct {
    str_bytes: ?[*]u8,
    str_len: usize,

    pub fn empty() RocStr {
        return RocStr{
            .str_len = 0,
            .str_bytes = null,
        };
    }

    // This takes ownership of the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(bytes: [*]const u8, length: usize) RocStr {
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
                offset_ptr.* = bytes[index];
                index += 1;
            }

            // set the final byte to be the length
            const final_byte_ptr = @intToPtr(*u8, target_ptr + roc_str_size - 1);
            final_byte_ptr.* = @truncate(u8, length) ^ 0b10000000;

            return ret_small_str;
        } else {
            var result = allocate_str(u64, InPlace.Clone, length);

            @memcpy(@ptrCast([*]u8, result.str_bytes), bytes, length);

            return result;
        }
    }

    // This takes ownership of the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn withCapacity(length: usize) RocStr {
        const roc_str_size = @sizeOf(RocStr);

        if (length < roc_str_size) {
            return RocStr.empty();
        } else {
            var new_bytes: [*]u8 = @ptrCast([*]u8, malloc(length));

            return RocStr{
                .str_bytes = new_bytes,
                .str_len = length,
            };
        }
    }

    pub fn deinit(self: RocStr) void {
        if (!self.isSmallStr()) {
            const str_bytes: [*]u8 = self.str_bytes orelse unreachable;

            free(str_bytes);
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
        const self_bytes: [*]const u8 = if (self.is_small_str() or self.is_empty()) self_u8_ptr else self_bytes_ptr orelse unreachable;
        const other_bytes: [*]const u8 = if (other.is_small_str() or other.is_empty()) other_u8_ptr else other_bytes_ptr orelse unreachable;

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

    pub fn is_small_str(self: RocStr) bool {
        return @bitCast(isize, self.str_len) < 0;
    }

    pub fn len(self: RocStr) usize {
        const bytes: [*]const u8 = @ptrCast([*]const u8, &self);
        const last_byte = bytes[@sizeOf(RocStr) - 1];
        const small_len = @as(usize, last_byte ^ 0b1000_0000);
        const big_len = self.str_len;

        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        return if (self.is_small_str()) small_len else big_len;
    }

    pub fn is_empty(self: RocStr) bool {
        return self.len() == 0;
    }

    pub fn as_u8_ptr(self: RocStr) [*]u8 {
        const if_small = &@bitCast([16]u8, self);
        const if_big = @ptrCast([*]u8, self.str_bytes);
        return if (self.is_small_str() or self.is_empty()) if_small else if_big;
    }

    // Given a pointer to some bytes, write the first (len) bytes of this
    // RocStr's contents into it.
    //
    // One use for this function is writing into an `alloca` for a C string that
    // only needs to live long enough to be passed as an argument to
    // a C function - like the file path argument to `fopen`.
    pub fn memcpy(self: RocStr, dest: [*]u8, len: usize) void {
        const small_src = @ptrCast(*u8, self);
        const big_src = self.str_bytes_ptr;

        // For a small string, copy the bytes directly from `self`.
        // For a large string, copy from the pointed-to bytes.

        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        const src: [*]u8 = if (self.is_small_str()) small_src else big_src;

        @memcpy(dest, src, len);
    }

    test "RocStr.eq: equal" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "abc".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        // TODO: fix those tests
        // expect(roc_str1.eq(roc_str2));

        roc_str1.deinit();
        roc_str2.deinit();
    }

    test "RocStr.eq: not equal different length" {
        const str1_len = 4;
        var str1: [str1_len]u8 = "abcd".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        expect(!roc_str1.eq(roc_str2));

        roc_str1.deinit();
        roc_str2.deinit();
    }

    test "RocStr.eq: not equal same length" {
        const str1_len = 3;
        var str1: [str1_len]u8 = "acb".*;
        const str1_ptr: [*]u8 = &str1;
        var roc_str1 = RocStr.init(str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(str2_ptr, str2_len);

        // TODO: fix those tests
        // expect(!roc_str1.eq(roc_str2));

        roc_str1.deinit();
        roc_str2.deinit();
    }
};

// Str.numberOfBytes

pub fn strNumberOfBytes(string: RocStr) callconv(.C) usize {
    return string.len();
}

// Str.fromInt

pub fn strFromInt(int: i64) callconv(.C) RocStr {
    // prepare for having multiple integer types in the future
    return strFromIntHelp(i64, int);
}

fn strFromIntHelp(comptime T: type, int: T) RocStr {
    // determine maximum size for this T
    comptime const size = comptime blk: {
        // the string representation of the minimum i128 value uses at most 40 characters
        var buf: [40]u8 = undefined;
        var result = std.fmt.bufPrint(&buf, "{}", .{std.math.minInt(T)}) catch unreachable;
        break :blk result.len;
    };

    var buf: [size]u8 = undefined;
    const result = std.fmt.bufPrint(&buf, "{}", .{int}) catch unreachable;

    return RocStr.init(&buf, result.len);
}

// Str.split

pub fn strSplitInPlace(array: [*]RocStr, array_len: usize, string: RocStr, delimiter: RocStr) callconv(.C) void {
    var ret_array_index: usize = 0;
    var sliceStart_index: usize = 0;
    var str_index: usize = 0;

    const str_bytes = string.as_u8_ptr();
    const str_len = string.len();

    const delimiter_bytes_ptrs = delimiter.as_u8_ptr();
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
                const segment_len: usize = str_index - sliceStart_index;

                array[ret_array_index] = RocStr.init(str_bytes + sliceStart_index, segment_len);
                sliceStart_index = str_index + delimiter_len;
                ret_array_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    array[ret_array_index] = RocStr.init(str_bytes + sliceStart_index, str_len - sliceStart_index);
}

test "strSplitInPlace: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(array_ptr, 1, str, delimiter);

    var expected = [1]RocStr{
        str,
    };

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));

    for (array) |roc_str| {
        roc_str.deinit();
    }

    for (expected) |roc_str| {
        roc_str.deinit();
    }
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

    strSplitInPlace(array_ptr, array_len, str, delimiter);

    const one = RocStr.init("1", 1);
    const two = RocStr.init("2", 1);

    var expected = [3]RocStr{
        one, two, RocStr.empty(),
    };

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
    expect(array[1].eq(expected[1]));
    expect(array[2].eq(expected[2]));
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
    strSplitInPlace(array_ptr, array_len, str, delimiter);

    const ghi_arr = "ghi";
    const ghi = RocStr.init(ghi_arr, ghi_arr.len);

    var expected = [3]RocStr{
        RocStr.empty(), ghi, RocStr.empty(),
    };

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
    expect(array[1].eq(expected[1]));
    expect(array[2].eq(expected[2]));
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

    strSplitInPlace(array_ptr, array_len, str, delimiter);

    const a = RocStr.init("a", 1);
    const b = RocStr.init("b", 1);
    const c = RocStr.init("c", 1);

    var expected_array = [array_len]RocStr{
        a, b, c,
    };

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
    const str_bytes = string.as_u8_ptr();
    const str_len = string.len();

    const delimiter_bytes_ptrs = delimiter.as_u8_ptr();
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
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const segments_count = countSegments(str, delimiter);

    expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments
    const str_arr = "hello there";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "hello";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const segments_count = countSegments(str, delimiter);

    expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    const segments_count = countSegments(str, delimiter);

    expectEqual(segments_count, 3);
}

// Str.countGraphemeClusters
const grapheme = @import("helpers/grapheme.zig");

pub fn countGraphemeClusters(string: RocStr) callconv(.C) usize {
    if (string.is_empty()) {
        return 0;
    }

    const bytes_len = string.len();
    const bytes_ptr = string.as_u8_ptr();

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

fn roc_str_from_literal(bytes_arr: *const []u8) RocStr {}

test "countGraphemeClusters: empty string" {
    const count = countGraphemeClusters(RocStr.empty());
    expectEqual(count, 0);
}

test "countGraphemeClusters: ascii characters" {
    const bytes_arr = "abcd";
    const bytes_len = bytes_arr.len;
    const count = countGraphemeClusters(RocStr.init(bytes_arr, bytes_len));
    expectEqual(count, 4);
}

test "countGraphemeClusters: utf8 characters" {
    const bytes_arr = "ãxā";
    const bytes_len = bytes_arr.len;
    const count = countGraphemeClusters(RocStr.init(bytes_arr, bytes_len));
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis" {
    const bytes_arr = "🤔🤔🤔";
    const bytes_len = bytes_arr.len;
    const count = countGraphemeClusters(RocStr.init(bytes_arr, bytes_len));
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis and ut8 characters" {
    const bytes_arr = "🤔å🤔¥🤔ç";
    const bytes_len = bytes_arr.len;
    const count = countGraphemeClusters(RocStr.init(bytes_arr, bytes_len));
    expectEqual(count, 6);
}

test "countGraphemeClusters: emojis, ut8, and ascii characters" {
    const bytes_arr = "6🤔å🤔e¥🤔çpp";
    const bytes_len = bytes_arr.len;
    const count = countGraphemeClusters(RocStr.init(bytes_arr, bytes_len));
    expectEqual(count, 10);
}

// Str.startsWith

pub fn startsWith(string: RocStr, prefix: RocStr) callconv(.C) bool {
    const bytes_len = string.len();
    const bytes_ptr = string.as_u8_ptr();

    const prefix_len = prefix.len();
    const prefix_ptr = prefix.as_u8_ptr();

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
    const foo = RocStr.init("foo", 3);
    const fo = RocStr.init("fo", 2);
    expect(startsWith(foo, fo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.init("12345678912345678910", 20);
    const prefix = RocStr.init("123456789123456789", 18);

    expect(startsWith(str, prefix));
}

// Str.endsWith

pub fn endsWith(string: RocStr, suffix: RocStr) callconv(.C) bool {
    const bytes_len = string.len();
    const bytes_ptr = string.as_u8_ptr();

    const suffix_len = suffix.len();
    const suffix_ptr = suffix.as_u8_ptr();

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
    expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init("12345678912345678910", 20);
    const suffix = RocStr.init("345678912345678910", 18);

    expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init("hello world", 11);
    const suffix = RocStr.init("world", 5);

    expect(endsWith(str, suffix));
}

// Str.concat

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

    const result = strConcat(8, InPlace.Clone, roc_str1, roc_str2);

    expect(roc_str3.eq(result));

    roc_str1.deinit();
    roc_str2.deinit();
    roc_str3.deinit();
    result.deinit();
}

pub fn strConcat(ptr_size: u32, result_in_place: InPlace, arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return switch (ptr_size) {
        4 => strConcatHelp(i32, result_in_place, arg1, arg2),
        8 => strConcatHelp(i64, result_in_place, arg1, arg2),
        else => unreachable,
    };
}

fn strConcatHelp(comptime T: type, result_in_place: InPlace, arg1: RocStr, arg2: RocStr) RocStr {
    if (arg1.is_empty()) {
        return cloneStr(T, result_in_place, arg2);
    } else if (arg2.is_empty()) {
        return cloneStr(T, result_in_place, arg1);
    } else {
        const combined_length = arg1.len() + arg2.len();

        const small_str_bytes = 2 * @sizeOf(T);
        const result_is_big = combined_length >= small_str_bytes;

        if (result_is_big) {
            var result = allocate_str(T, result_in_place, combined_length);

            {
                const old_if_small = &@bitCast([16]u8, arg1);
                const old_if_big = @ptrCast([*]u8, arg1.str_bytes);
                const old_bytes = if (arg1.is_small_str()) old_if_small else old_if_big;

                const new_bytes: [*]u8 = @ptrCast([*]u8, result.str_bytes);

                @memcpy(new_bytes, old_bytes, arg1.len());
            }

            {
                const old_if_small = &@bitCast([16]u8, arg2);
                const old_if_big = @ptrCast([*]u8, arg2.str_bytes);
                const old_bytes = if (arg2.is_small_str()) old_if_small else old_if_big;

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

const InPlace = packed enum(u8) {
    InPlace,
    Clone,
};

fn cloneStr(comptime T: type, in_place: InPlace, str: RocStr) RocStr {
    if (str.is_small_str() or str.is_empty()) {
        // just return the bytes
        return str;
    } else {
        var new_str = allocate_str(T, in_place, str.str_len);

        var old_bytes: [*]u8 = @ptrCast([*]u8, str.str_bytes);
        var new_bytes: [*]u8 = @ptrCast([*]u8, new_str.str_bytes);

        @memcpy(new_bytes, old_bytes, str.str_len);

        return new_str;
    }
}

fn allocate_str(comptime T: type, in_place: InPlace, number_of_chars: u64) RocStr {
    const length = @sizeOf(T) + number_of_chars;
    var new_bytes: [*]T = @ptrCast([*]T, @alignCast(@alignOf(T), malloc(length)));

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
