const std = @import("std");
const math = std.math;
const expect = std.testing.expect;

export fn atan_(num: f64) f64 {
    return math.atan(num);
}

export fn is_finite_(num: f64) bool {
    return math.isFinite(num);
}

export fn pow_int_(base: i64, exp: i64) i64 {
    return math.pow(i64, base, exp);
}

// Str.split

const RocStr = struct {
    str_bytes: [*]u8,
    str_len: usize,

    pub fn init(bytes: [*]u8, len: usize) RocStr {
        return RocStr {
            .str_bytes = bytes,
            .str_len = len
        };
    }
};

export fn str_split_in_place_(
    list: [*]RocStr,
    list_len: usize,
    str_bytes: [*]u8,
    str_len: usize,
    delimiter_bytes: [*]u8,
    delimiter_len: usize
) void {
    var ret_list_index : usize = 0;

    var slice_start_index : usize = 0;

    var str_index : usize = 0;

    if (str_len > delimiter_len) {
        const end_index : usize = str_len - delimiter_len;
        while (str_index <= end_index) {
            var delimiter_index : usize = 0;
            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                var delimiter_char = delimiter_bytes[delimiter_index];
                var str_char = str_bytes[str_index + delimiter_index];

                if (delimiter_char != str_char) {
                    matches_delimiter = false;
                    break;
                }

                delimiter_index += 1;
            }

            if (matches_delimiter) {
                list[ret_list_index] = RocStr.init(str_bytes + slice_start_index, str_index - slice_start_index);
                slice_start_index = str_index + delimiter_len;
                ret_list_index += 1;
                str_index += delimiter_len;
            } else {
                str_index += 1;
            }
        }
    }

    list[ret_list_index] = RocStr.init(str_bytes + slice_start_index, str_len - slice_start_index);
}

test "str_split_in_place_ no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]
    
    var str: [3]u8 = "abc".*;
    const str_ptr: [*]u8 = &str;

    var delimiter: [1]u8 = "!".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    var list: [1]RocStr = [_]RocStr{
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        }
    };

    const list_ptr: [*]RocStr = &list;

    str_split_in_place_(
        list_ptr,
        1,
        str_ptr,
        3,
        delimiter_ptr,
        1
    );

    var expected_array = [1]RocStr{
        RocStr{
            .str_bytes = str_ptr,
            .str_len = 3,
        },
    };

    expect(list.len == expected_array.len);
    expect(list[0].str_len == 3);

    const list_elem_bytes = list[0].str_bytes;
    const expected_elem_bytes = expected_array[0].str_bytes;

    expect(list_elem_bytes[0] == expected_elem_bytes[0]);
    expect(list_elem_bytes[0] == 'a');
    expect(list_elem_bytes[1] == 'b');
    expect(list_elem_bytes[2] == 'c');
    expect(list_elem_bytes[2] == expected_elem_bytes[2]);

}

test "str_split_in_place_ delimiter on sides" {
    // Str.split "tttghittt" "ttt" == [ "", "ghi", "" ]

    const str_len: usize = 9;
    var str: [str_len]u8 = "tttghittt".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 3;
    var delimiter: [delimiter_len]u8 = "ttt".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const list_len : usize = 3;
    var list: [list_len]RocStr = [_]RocStr{
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        }
    };

    const list_ptr: [*]RocStr = &list;

    str_split_in_place_(
        list_ptr,
        list_len,
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );


    expect(list[0].str_len == 0);
    expect(list[1].str_len == 3);
    expect(list[2].str_len == 0);

    const list_middle_elem_bytes = list[1].str_bytes;

    expect(list_middle_elem_bytes[0] == 'g');
    expect(list_middle_elem_bytes[1] == 'h');
    expect(list_middle_elem_bytes[2] == 'i');

}

test "str_split_in_place_ three pieces" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]

    const str_len: usize = 5;
    var str: [str_len]u8 = "a!b!c".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 1;
    var delimiter: [delimiter_len]u8 = "!".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const list_len : usize = 3;
    var list: [list_len]RocStr = [_]RocStr{
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes = delimiter_ptr,
            .str_len = 1,
        }
    };

    const list_ptr: [*]RocStr = &list;

    str_split_in_place_(
        list_ptr,
        list_len,
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

    var expected_array = [list_len]RocStr{
        RocStr{
            .str_bytes = a_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes = b_ptr,
            .str_len = 1,
        },
        RocStr{
            .str_bytes = c_ptr,
            .str_len = 1,
        }
    };

    expect(expected_array.len == list.len);

    const list_first_elem_bytes = list[0].str_bytes;
    const expected_first_elem_bytes = expected_array[0].str_bytes;

    expect(list_first_elem_bytes[0] == expected_first_elem_bytes[0]);
    expect(list_first_elem_bytes[0] == 'a');
    expect(list[0].str_len == 1);

    const list_second_elem_bytes = list[1].str_bytes;
    const expected_second_elem_bytes = expected_array[1].str_bytes;

    expect(list_second_elem_bytes[0] == expected_second_elem_bytes[0]);
    expect(list_second_elem_bytes[0] == 'b');
    expect(list[1].str_len == 1);

    const list_third_elem_bytes = list[2].str_bytes;
    const expected_third_elem_bytes = expected_array[2].str_bytes;

    expect(list_third_elem_bytes[0] == expected_third_elem_bytes[0]);
    expect(list_third_elem_bytes[0] == 'c');
    expect(list[2].str_len == 1);
}

// This is used for `Str.split : Str, Str -> List Str
// It is used to count how many segments the input `Str`
// needs to be broken into, so that we can allocate a list
// of that size. It always returns at least 1.
export fn count_segments_(
    str_bytes: [*]u8,
    str_len: usize,
    delimiter_bytes: [*]u8,
    delimiter_len: usize
) i64 {
    var count: i64 = 1;

    if (str_len > delimiter_len) {
        var str_index: usize = 0;
        const end_cond: usize = str_len - delimiter_len;

        while (str_index < end_cond) {
            var delimiter_index: usize = 0;

            var matches_delimiter = true;

            while (delimiter_index < delimiter_len) {
                const delimiter_char = delimiter_bytes[delimiter_index];
                const str_char = str_bytes[str_index + delimiter_index];

                if (delimiter_char != str_char) {
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

test "count_segments_ long delimiter" {
    // Str.split "str" "delimiter" == [ "str" ]
    // 1 segment

    const str_len: usize = 3;
    var str: [str_len]u8 = "str".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 9;
    var delimiter: [delimiter_len]u8 = "delimiter".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const segments_count = count_segments_(
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

    expect(segments_count == 1);
}

test "count_segments_ delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments

    const str_len: usize = 11;
    var str: [str_len]u8 = "hello there".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 5;
    var delimiter: [delimiter_len]u8 = "hello".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const segments_count = count_segments_(
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

    expect(segments_count == 2);
}

test "count_segments_ delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments

    const str_len: usize = 5;
    var str: [str_len]u8 = "a!b!c".*;
    const str_ptr: [*]u8 = &str;

    const delimiter_len = 1;
    var delimiter: [delimiter_len]u8 = "!".*;
    const delimiter_ptr: [*]u8 = &delimiter;

    const segments_count = count_segments_(
        str_ptr,
        str_len,
        delimiter_ptr,
        delimiter_len
    );

    expect(segments_count == 3);
}