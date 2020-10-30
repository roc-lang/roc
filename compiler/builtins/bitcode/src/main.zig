const std = @import("std");
const math = std.math;
const expect = std.testing.expect;

const roc_builtins_namespace = "roc_builtins";
const math_namespace = roc_builtins_namespace ++ ".math";
const str_namespace = roc_builtins_namespace ++ ".str";

comptime { @export(atan, .{ .name = math_namespace ++ ".atan", .linkage = .Strong  }); }
fn atan(num: f64) callconv(.C) f64 {
    return math.atan(num);
}

comptime { @export(isFinite, .{ .name = math_namespace ++ ".is_finite", .linkage = .Strong  }); }
fn isFinite(num: f64) callconv(.C) bool {
    return math.isFinite(num);
}

comptime { @export(powInt, .{ .name = math_namespace ++ ".pow_int", .linkage = .Strong  }); }
fn powInt(base: i64, exp: i64) callconv(.C) i64 {
    return math.pow(i64, base, exp);
}


// Str.split

const RocStr = struct {
    str_bytes_ptrs: [*]u8,
    str_len: usize,

    pub fn init(bytes: [*]u8, len: usize) RocStr {
        return RocStr {
            .str_bytes_ptrs = bytes,
            .str_len = len
        };
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
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

        expect(RocStr.eq(roc_str1, roc_str2));
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

        expect(!RocStr.eq(roc_str1, roc_str2));
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

        expect(!RocStr.eq(roc_str1, roc_str2));
    }
};

comptime { @export(strSplitInPlace, .{ .name = str_namespace ++ ".str_split_in_place", .linkage = .Strong  }); }
fn strSplitInPlace(
    array: [*]RocStr,
    array_len: usize,
    str_bytes_ptrs: [*]u8,
    str_len: usize,
    delimiter_bytes: [*]u8,
    delimiter_len: usize
) callconv(.C) void {
    var ret_array_index : usize = 0;

    var sliceStart_index : usize = 0;

    var str_index : usize = 0;

    if (str_len > delimiter_len) {
        const end_index : usize = str_len - delimiter_len;
        while (str_index <= end_index) {
            var delimiter_index : usize = 0;
            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                var delimiterChar = delimiter_bytes[delimiter_index];
                var strChar = str_bytes_ptrs[str_index + delimiter_index];

                if (delimiterChar != strChar) {
                    matches_delimiter = false;
                    break;
                }

                delimiter_index += 1;
            }

            if (matches_delimiter) {
                array[ret_array_index] = RocStr.init(str_bytes_ptrs + sliceStart_index, str_index - sliceStart_index);
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

    expect(array.len == expected.len);
    expect(RocStr.eq(array[0], expected[0]));
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

    expect(array.len == 3);
    expect(array[0].str_len == 0);
    expect(RocStr.eq(array[1], expectedRocStr));
    expect(array[2].str_len == 0);
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

    expect(expected_array.len == array.len);
    expect(RocStr.eq(array[0], expected_array[0]));
    expect(RocStr.eq(array[1], expected_array[1]));
    expect(RocStr.eq(array[2], expected_array[2]));
}

// This is used for `Str.split : Str, Str -> Array Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
comptime { @export(countSegments, .{ .name = str_namespace ++ ".count_segements", .linkage = .Strong  }); }
fn countSegments(
    str_bytes_ptrs: [*]u8,
    str_len: usize,
    delimiter_bytes: [*]u8,
    delimiter_len: usize
) callconv(.C) i64 {
    var count: i64 = 1;

    if (str_len > delimiter_len) {
        var str_index: usize = 0;
        const end_cond: usize = str_len - delimiter_len;

        while (str_index < end_cond) {
            var delimiter_index: usize = 0;

            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                const delimiterChar = delimiter_bytes[delimiter_index];
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

    expect(segments_count == 1);
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

    expect(segments_count == 2);
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

    expect(segments_count == 3);
}
