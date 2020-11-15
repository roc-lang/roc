const std = @import("std");
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;


const RocStr = struct {
    str_bytes_ptrs: [*]u8,
    str_len: usize,

    pub fn get_small_str_ptr(self: *RocStr) *u8 {
        const small_str_ptr = @ptrCast(*u8, self);
        return small_str_ptr;
    }

    pub fn empty() RocStr {
        return RocStr {
            .str_len = 0,
            .str_bytes_ptrs = undefined
        };
    }

    pub fn init(bytes: [*]u8, len: usize) RocStr {
        const rocStrSize = @sizeOf(RocStr);

        if (len < rocStrSize) {
            var empty_roc_str = RocStr.empty();

            const target_ptr = @ptrToInt(empty_roc_str.get_small_str_ptr());

            var index : u8 = 0;

            while (index < len) {
                var offset_ptr = @intToPtr(*usize, target_ptr + index);
                offset_ptr.* = bytes[index];
                index += 1;
            }
            const final_byte_ptr = @intToPtr(*usize, target_ptr + index);
            final_byte_ptr.* = rocStrSize ^ 0b10000000;
            empty_roc_str.str_len = target_ptr;

            return empty_roc_str;
        } else {
            return RocStr {
                .str_bytes_ptrs = bytes,
                .str_len = len
            };
        }


    }

    pub fn eq(self: *RocStr, other: RocStr) bool {
        if (self.str_len != other.str_len) {
            return false;
        }

        var areEq: bool = true;
        var index: usize = 0;
        while (index < self.str_len and areEq) {
            areEq = areEq and self.str_bytes_ptrs[index] == other.str_bytes_ptrs[index];
            index = index + 1;
        }

        return areEq;
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

        expect(roc_str1.eq(roc_str2));
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

        expect(!roc_str1.eq(roc_str2));
    }
};

// Str.split

pub fn strSplitInPlace(
    bytes_array: [*]u128,
    array_len: usize,
    str_bytes_ptrs: [*]u8,
    str_len: usize,
    delimiter_bytes_ptrs: [*]u8,
    delimiter_len: usize
) callconv(.C) void {

    var array = @ptrCast([*]RocStr, bytes_array);

    var ret_array_index : usize = 0;

    var sliceStart_index : usize = 0;

    var str_index : usize = 0;

    if (str_len > delimiter_len) {
        const end_index : usize = str_len - delimiter_len;
        while (str_index <= end_index) {
            var delimiter_index : usize = 0;
            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                var delimiterChar = delimiter_bytes_ptrs[delimiter_index];
                var strChar = str_bytes_ptrs[str_index + delimiter_index];

                if (delimiterChar != strChar) {
                    matches_delimiter = false;
                    break;
                }

                delimiter_index += 1;
            }

            if (matches_delimiter) {
                const segment_len : usize = str_index - sliceStart_index;
                array[ret_array_index] = RocStr.init(str_bytes_ptrs + sliceStart_index, segment_len);
                sliceStart_index = str_index + delimiter_len;
                ret_array_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    array[ret_array_index] = RocStr.init(str_bytes_ptrs + sliceStart_index, str_len - sliceStart_index);
}

test "strSplitInPlace: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]

    var str: [3]u8 = "abc".*;
    const str_ptr: [*]u8 = &str;

    var delimiter: [1]u8 = "!".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(
        array_ptr,
        1,
        str_ptr,
        3,
        delimiter_ptr,
        1
    );

    var expected = [1]RocStr{
        RocStr.init(str_ptr, 3),
    };

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
}

test "strSplitInPlace: delimiter on sides" {
    // Str.split "tttghittt" "ttt" == [ "", "ghi", "" ]

    const str_len: usize = 9;
    var str: [str_len]u8 = "tttghittt".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 3;
    var delimiter: [delimiter_len]u8 = "ttt".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const array_len : usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined ,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(
        array_ptr,
        array_len,
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

    const expected_str_len: usize = 3;
    var expected_str: [expected_str_len]u8 = "ghi".*;
    const expected_str_ptr: [*]u8 = &expected_str;
    var expectedRocStr = RocStr.init(expected_str_ptr, expected_str_len);

    expectEqual(array.len, 3);
    expectEqual(array[0].str_len, 0);
    expect(array[1].eq(expectedRocStr));
    expectEqual(array[2].str_len, 0);
}

test "strSplitInPlace: three pieces" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]

    const str_len: usize = 5;
    var str: [str_len]u8 = "a!b!c".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 1;
    var delimiter: [delimiter_len]u8 = "!".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const array_len : usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    strSplitInPlace(
        array_ptr,
        array_len,
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

    var a: [1]u8 = "a".*;
    const a_ptr: [*]u8 = &a;

    var b: [1]u8 = "b".*;
    const b_ptr: [*]u8 = &b;

    var c: [1]u8 = "c".*;
    const c_ptr: [*]u8 = &c;

    var expected_array = [array_len]RocStr{
        RocStr{
            .str_bytes_ptrs = a_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes_ptrs = b_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes_ptrs = c_ptr,
            .str_len = 1,
        }
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
pub fn countSegments(
    str_bytes_ptrs: [*]u8,
    str_len: usize,
    delimiter_bytes_ptrs: [*]u8,
    delimiter_len: usize
) callconv(.C) usize {
    var count: usize = 1;

    if (str_len > delimiter_len) {
        var str_index: usize = 0;
        const end_cond: usize = str_len - delimiter_len;

        while (str_index < end_cond) {
            var delimiter_index: usize = 0;

            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                const delimiterChar = delimiter_bytes_ptrs[delimiter_index];
                const strChar = str_bytes_ptrs[str_index + delimiter_index];

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
    var str: [str_len]u8 = "str".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 9;
    var delimiter: [delimiter_len]u8 = "delimiter".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const segments_count = countSegments(
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

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

    const segments_count = countSegments(
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

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

    const segments_count = countSegments(
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

    expectEqual(segments_count, 3);
}

// Str.countGraphemeClusters
const grapheme = @import("helpers/grapheme.zig");

pub fn countGraphemeClusters(bytes_ptr: [*]u8, bytes_len: usize)  callconv(.C) usize {
    var bytes = bytes_ptr[0..bytes_len];
    var iter = (unicode.Utf8View.init(bytes) catch unreachable).iterator();

    var count: usize = 0;
    var grapheme_break_state: ?grapheme.BoundClass = null;
    var grapheme_break_state_ptr = &grapheme_break_state;
    var opt_last_codepoint: ?u21 = null;
    while (iter.nextCodepoint()) |cur_codepoint| {
        if (opt_last_codepoint) |last_codepoint| {
            var did_break = grapheme.isGraphemeBreak(
                last_codepoint,
                cur_codepoint,
                grapheme_break_state_ptr
            );
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
