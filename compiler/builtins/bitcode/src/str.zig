const std = @import("std");
const Allocator = std.mem.Allocator;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

<<<<<<< HEAD
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
    pub fn init(allocator: *Allocator, bytes: [*]const u8, length: usize) RocStr {
        const rocStrSize = @sizeOf(RocStr);

        if (length < rocStrSize) {
            var ret_small_str = RocStr.empty();
            const target_ptr = @ptrToInt(&ret_small_str);
            var index: u8 = 0;

            // TODO isn't there a way to bulk-zero data in Zig?
            // Zero out the data, just to be safe
            while (index < rocStrSize) {
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
            const final_byte_ptr = @intToPtr(*u8, target_ptr + rocStrSize - 1);
            final_byte_ptr.* = @truncate(u8, length) ^ 0b10000000;

            return ret_small_str;
        } else {
            var new_bytes: []u8 = allocator.alloc(u8, length) catch unreachable;
            var new_bytes_ptr: [*]u8 = @ptrCast([*]u8, &new_bytes);
            @memcpy(new_bytes_ptr, bytes, length);
            return RocStr{
                .str_bytes = new_bytes_ptr,
                .str_len = length,
            };
        }
    }

    pub fn drop(self: RocStr, allocator: *Allocator) void {
        if (!self.isSmallStr()) {
            const str_bytes_ptr: [*]u8 = self.str_bytes orelse unreachable;
            const align_of_slice = @alignOf(*[]u8);
            const slice_ptr: *[]u8 = @ptrCast(*[]u8, @alignCast(align_of_slice, str_bytes_ptr));
            const slice_bytes: []u8 = slice_ptr.*;
            allocator.free(slice_bytes);
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

        const self_bytes_nonnull: [*]const u8 = self_bytes_ptr orelse unreachable;
        const other_bytes_nonnull: [*]const u8 = other_bytes_ptr orelse unreachable;
        const self_u8_ptr: [*]const u8 = @ptrCast([*]const u8, &self);
        const other_u8_ptr: [*]const u8 = @ptrCast([*]const u8, &other);
        const self_bytes: [*]const u8 = if (self.is_small_str()) self_u8_ptr else self_bytes_nonnull;
        const other_bytes: [*]const u8 = if (other.is_small_str()) other_u8_ptr else other_bytes_nonnull;

        var index: usize = 0;

        // TODO rewrite this into a for loop
        while (index < self.str_len) {
            if (self_bytes[index] != other_bytes[index]) {
                return false;
            }

            index = index + 1;
        }

        return true;
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

    pub fn is_empty(self: RocStr) bool {
        return self.len() == 0;
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
        var roc_str1 = RocStr.init(testing.allocator, str1_ptr, str1_len);

        const str2_len = 3;
        var str2: [str2_len]u8 = "abc".*;
        const str2_ptr: [*]u8 = &str2;
        var roc_str2 = RocStr.init(testing.allocator, str2_ptr, str2_len);

        // TODO: fix those tests
        // expect(roc_str1.eq(roc_str2));

        roc_str1.drop(testing.allocator);
        roc_str2.drop(testing.allocator);
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

        expect(!roc_str1.eq(roc_str2));

        roc_str1.drop(testing.allocator);
        roc_str2.drop(testing.allocator);
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

        // TODO: fix those tests
        // expect(!roc_str1.eq(roc_str2));

        roc_str1.drop(testing.allocator);
        roc_str2.drop(testing.allocator);
    }
};

// Str.split

pub fn strSplitInPlace(
    allocator: *Allocator,
    array: [*]RocStr,
    array_len: usize,
    str_bytes: [*]const u8,
    str_len: usize,
    delimiter_bytes_ptrs: [*]const u8,
    delimiter_len: usize
) void {
    var ret_array_index: usize = 0;
    var sliceStart_index: usize = 0;
    var str_index: usize = 0;

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

                array[ret_array_index] = RocStr.init(allocator, str_bytes + sliceStart_index, segment_len);
                sliceStart_index = str_index + delimiter_len;
                ret_array_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    array[ret_array_index] = RocStr.init(allocator, str_bytes + sliceStart_index, str_len - sliceStart_index);
}

// When we actually use this in Roc, libc will be linked so we have access to std.heap.c_allocator
pub fn strSplitInPlaceC(
    array: [*]RocStr,
    array_len: usize,
    str_bytes: [*]const u8,
    str_len: usize,
    delimiter_bytes_ptrs: [*]const u8,
    delimiter_len: usize
) callconv(.C) void {
    strSplitInPlace(
        std.heap.c_allocator,
        array,
        array_len,
        str_bytes,
        str_len,
        delimiter_bytes_ptrs,
        delimiter_len
    );
}

test "strSplitInPlace: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]

    const array_len: usize = 1;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(
        testing.allocator,
        array_ptr,
        array_len,
        "abc",
        3,
        "!",
        1
    );

    var expected = RocStr.init(testing.allocator, "abc", 3);

    expect(array[0].eq(expected));

    for (array) |roc_str| {
        roc_str.drop(testing.allocator);
    }
    expected.drop(testing.allocator);
}

test "strSplitInPlace: empty end" {
    const array_len: usize = 2;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(
        testing.allocator,
        array_ptr,
        array_len,
        "1---- ---- ---- ---- ----2---- ---- ---- ---- ----",
        50,
        "---- ---- ---- ---- ----",
        24
        );

    var expected = [2]RocStr{
        RocStr.init(testing.allocator, "1", 1),
        RocStr.init(testing.allocator, "2", 1),
    };

    expect(array[0].eq(expected[0]));
    expect(array[1].eq(expected[1]));

    for (array) |roc_str| {
        roc_str.drop(testing.allocator);
    }
    for (expected) |roc_str| {
        roc_str.drop(testing.allocator);
    }
}

test "strSplitInPlace: delimiter on sides" {
    // Str.split "tttghittt" "ttt" == [ "", "ghi", "" ]
    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, array_ptr, array_len, "tttghittt", 9, "ttt", 3);

    const expected_str_len: usize = 3;
    const expected_str_ptr: [*]const u8 = "ghi";
    var expected_roc_str = RocStr.init(testing.allocator, expected_str_ptr, expected_str_len);

    // TODO: fix empty str tests
    // expect(array[0].eq(empty_roc_str));
    expect(array[1].eq(expected_roc_str));
    // expect(array[2].eq(empty_roc_str));

    for (array) |roc_str| {
        roc_str.drop(testing.allocator);
    }
    expected_roc_str.drop(testing.allocator);
}

test "strSplitInPlace: three pieces" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    const str_len: usize = 5;
    var str_ptr: [*]const u8 = "a!b!c";

    const delimiter_len = 1;
    var delimiter_ptr: [*]const u8 = "!";

    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, array_ptr, array_len, str_ptr, str_len, delimiter_ptr, delimiter_len);

    var expected_array = [array_len]RocStr{
        RocStr{.str_bytes = "a", .str_len = 1},
        RocStr{.str_bytes = "b", .str_len = 1},
        RocStr{.str_bytes = "c", .str_len = 1},
    };

    // TODO: fix those tests
    // expectEqual(expected_array.len, array.len);
    // expect(array[0].eq(expected_array[0]));
    // expect(array[1].eq(expected_array[1]));
    // expect(array[2].eq(expected_array[2]));

    for (expected_array) |roc_str| {
        roc_str.drop(testing.allocator);
    }
}

// This is used for `Str.split : Str, Str -> Array Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
pub fn countSegments(str_bytes: [*]const u8, str_len: usize, delimiter_bytes_ptrs: [*]const u8, delimiter_len: usize) callconv(.C) usize {
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
    const str_len: usize = 3;
    var str_ptr: [*]const u8 = "str";

    const delimiter_len = 9;
    var delimiter_ptr: [*]const u8 = "delimiter";

    const segments_count = countSegments(str_ptr, str_len, delimiter_ptr, delimiter_len);

    expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments
    const str_len: usize = 11;
    var str: [str_len]u8 = "hello there".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 5;
    var delimiter: [delimiter_len]u8 = "hello".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const segments_count = countSegments(str_ptr, str_len, delimiter_ptr, delimiter_len);

    expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments
    const str_len: usize = 5;
    var str: [str_len]u8 = "a!b!c".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 1;
    var delimiter: [delimiter_len]u8 = "!".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const segments_count = countSegments(str_ptr, str_len, delimiter_ptr, delimiter_len);

    expectEqual(segments_count, 3);
}

// Str.countGraphemeClusters
const grapheme = @import("helpers/grapheme.zig");

pub fn countGraphemeClusters(bytes_ptr: [*]u8, bytes_len: usize) callconv(.C) usize {
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
    var bytes_arr = "".*;
    var bytes_len = bytes_arr.len;
    var bytes_ptr: [*]u8 = &bytes_arr;
    var count = countGraphemeClusters(bytes_ptr, bytes_len);
    expectEqual(count, 0);
}

test "countGraphemeClusters: ascii characters" {
    var bytes_arr = "abcd".*;
    var bytes_len = bytes_arr.len;
    var bytes_ptr: [*]u8 = &bytes_arr;
    var count = countGraphemeClusters(bytes_ptr, bytes_len);
    expectEqual(count, 4);
}

test "countGraphemeClusters: utf8 characters" {
    var bytes_arr = "ãxā".*;
    var bytes_len = bytes_arr.len;
    var bytes_ptr: [*]u8 = &bytes_arr;
    var count = countGraphemeClusters(bytes_ptr, bytes_len);
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis" {
    var bytes_arr = "🤔🤔🤔".*;
    var bytes_len = bytes_arr.len;
    var bytes_ptr: [*]u8 = &bytes_arr;
    var count = countGraphemeClusters(bytes_ptr, bytes_len);
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis and ut8 characters" {
    var bytes_arr = "🤔å🤔¥🤔ç".*;
    var bytes_len = bytes_arr.len;
    var bytes_ptr: [*]u8 = &bytes_arr;
    var count = countGraphemeClusters(bytes_ptr, bytes_len);
    expectEqual(count, 6);
}

test "countGraphemeClusters: emojis, ut8, and ascii characters" {
    var bytes_arr = "6🤔å🤔e¥🤔çpp".*;
    var bytes_len = bytes_arr.len;
    var bytes_ptr: [*]u8 = &bytes_arr;
    var count = countGraphemeClusters(bytes_ptr, bytes_len);
    expectEqual(count, 10);
}

// Str.startsWith

pub fn startsWith(bytes_ptr: [*]u8, bytes_len: usize, prefix_ptr: [*]u8, prefix_len: usize) callconv(.C) bool {
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

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str_len: usize = 18;
    var str: [str_len]u8 = "123456789123456789".*;
    const str_ptr: [*]u8 = &str;

    expect(startsWith(str_ptr, str_len, str_ptr, str_len));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str_len: usize = 20;
    var str: [str_len]u8 = "12345678912345678910".*;
    const str_ptr: [*]u8 = &str;

    const prefix_len: usize = 18;
    var prefix: [prefix_len]u8 = "123456789123456789".*;
    const prefix_ptr: [*]u8 = &str;

    expect(startsWith(str_ptr, str_len, prefix_ptr, prefix_len));
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

    roc_str1.drop();
    roc_str2.drop();
    roc_str3.drop();
    result.drop();
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
        return cloneNonemptyStr(T, result_in_place, arg2);
    } else if (arg2.is_empty()) {
        return cloneNonemptyStr(T, result_in_place, arg1);
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

fn cloneNonemptyStr(comptime T: type, in_place: InPlace, str: RocStr) RocStr {
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
    first_element += 8;

    return RocStr{
        .str_bytes = first_element,
        .str_len = number_of_chars,
    };
}
