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
    strBytesPtrs: [*]u8,
    strLen: usize,

    pub fn init(bytes: [*]u8, len: usize) RocStr {
        return RocStr {
            .strBytesPtrs = bytes,
            .strLen = len
        };
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        if (self.strLen != other.strLen) {
            return false;
        }

        var areEq: bool = true;
        var index: usize = 0;
        while (index < self.strLen and areEq) {
            areEq = areEq and self.strBytesPtrs[index] == other.strBytesPtrs[index];
            index = index + 1;
        }

        return areEq;
    }

    test "RocStr.eq: equal" {
        const str1Len = 3;
        var str1: [str1Len]u8 = "abc".*;
        const str1Ptr: [*]u8 = &str1;
        var rocStr1 = RocStr.init(str1Ptr, str1Len);

        const str2Len = 3;
        var str2: [str2Len]u8 = "abc".*;
        const str2Ptr: [*]u8 = &str2;
        var rocStr2 = RocStr.init(str2Ptr, str2Len);

        expect(RocStr.eq(rocStr1, rocStr2));
    }

    test "RocStr.eq: not equal different length" {
        const str1Len = 4;
        var str1: [str1Len]u8 = "abcd".*;
        const str1Ptr: [*]u8 = &str1;
        var rocStr1 = RocStr.init(str1Ptr, str1Len);

        const str2Len = 3;
        var str2: [str2Len]u8 = "abc".*;
        const str2Ptr: [*]u8 = &str2;
        var rocStr2 = RocStr.init(str2Ptr, str2Len);

        expect(!RocStr.eq(rocStr1, rocStr2));
    }

    test "RocStr.eq: not equal same length" {
        const str1Len = 3;
        var str1: [str1Len]u8 = "acb".*;
        const str1Ptr: [*]u8 = &str1;
        var rocStr1 = RocStr.init(str1Ptr, str1Len);

        const str2Len = 3;
        var str2: [str2Len]u8 = "abc".*;
        const str2Ptr: [*]u8 = &str2;
        var rocStr2 = RocStr.init(str2Ptr, str2Len);

        expect(!RocStr.eq(rocStr1, rocStr2));
    }
};

export fn str_split_in_place_(
    array: [*]RocStr,
    arrayLen: usize,
    strBytesPtrs: [*]u8,
    strLen: usize,
    delimiterBytes: [*]u8,
    delimiterLen: usize
) void {
    var retArrayIndex : usize = 0;

    var sliceStartIndex : usize = 0;

    var strIndex : usize = 0;

    if (strLen > delimiterLen) {
        const endIndex : usize = strLen - delimiterLen;
        while (strIndex <= endIndex) {
            var delimiterIndex : usize = 0;
            var matchesDelimiter = true;

            while (delimiterIndex < delimiterLen) {
                var delimiterChar = delimiterBytes[delimiterIndex];
                var strChar = strBytesPtrs[strIndex + delimiterIndex];

                if (delimiterChar != strChar) {
                    matchesDelimiter = false;
                    break;
                }

                delimiterIndex += 1;
            }

            if (matchesDelimiter) {
                array[retArrayIndex] = RocStr.init(strBytesPtrs + sliceStartIndex, strIndex - sliceStartIndex);
                sliceStartIndex = strIndex + delimiterLen;
                retArrayIndex += 1;
                strIndex += delimiterLen;
            } else {
                strIndex += 1;
            }
        }
    }

    array[retArrayIndex] = RocStr.init(strBytesPtrs + sliceStartIndex, strLen - sliceStartIndex);
}

test "str_split_in_place_: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]

    var str: [3]u8 = "abc".*;
    const strPtr: [*]u8 = &str;

    var delimiter: [1]u8 = "!".*;
    const delimiterPtr: [*]u8 = &delimiter;

    var array: [1]RocStr = [_]RocStr{
        undefined,
    };

    const array_ptr: [*]RocStr = &array;

    str_split_in_place_(
        array_ptr,
        1,
        strPtr,
        3,
        delimiterPtr,
        1
    );

    var expected = [1]RocStr{
        RocStr.init(strPtr, 3),
    };

    expect(array.len == expected.len);
    expect(RocStr.eq(array[0], expected[0]));
}

test "str_split_in_place_: delimiter on sides" {
    // Str.split "tttghittt" "ttt" == [ "", "ghi", "" ]

    const strLen: usize = 9;
    var str: [strLen]u8 = "tttghittt".*;
    const strPtr: [*]u8 = &str;

    const delimiterLen = 3;
    var delimiter: [delimiterLen]u8 = "ttt".*;
    const delimiterPtr: [*]u8 = &delimiter;

    const arrayLen : usize = 3;
    var array: [arrayLen]RocStr = [_]RocStr{
        undefined ,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    str_split_in_place_(
        array_ptr,
        arrayLen,
        strPtr,
        strLen,
        delimiterPtr,
        delimiterLen
    );

    const expectedStrLen: usize = 3;
    var expectedStr: [expectedStrLen]u8 = "ghi".*;
    const expectedStrPtr: [*]u8 = &expectedStr;
    var expectedRocStr = RocStr.init(expectedStrPtr, expectedStrLen);

    expect(array.len == 3);
    expect(array[0].strLen == 0);
    expect(RocStr.eq(array[1], expectedRocStr));
    expect(array[2].strLen == 0);
}

test "str_split_in_place_: three pieces" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]

    const strLen: usize = 5;
    var str: [strLen]u8 = "a!b!c".*;
    const strPtr: [*]u8 = &str;

    const delimiterLen = 1;
    var delimiter: [delimiterLen]u8 = "!".*;
    const delimiterPtr: [*]u8 = &delimiter;

    const arrayLen : usize = 3;
    var array: [arrayLen]RocStr = [_]RocStr{
        undefined ,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    str_split_in_place_(
        array_ptr,
        arrayLen,
        strPtr,
        strLen,
        delimiterPtr,
        delimiterLen
    );

    var a: [1]u8 = "a".*;
    const a_ptr: [*]u8 = &a;

    var b: [1]u8 = "b".*;
    const b_ptr: [*]u8 = &b;

    var c: [1]u8 = "c".*;
    const c_ptr: [*]u8 = &c;

    var expectedArray = [arrayLen]RocStr{
        RocStr{
            .strBytesPtrs = a_ptr,
            .strLen = 1,
        },
        RocStr{
            .strBytesPtrs = b_ptr,
            .strLen = 1,
        },
        RocStr{
            .strBytesPtrs = c_ptr,
            .strLen = 1,
        }
    };

    expect(expectedArray.len == array.len);
    expect(RocStr.eq(array[0], expectedArray[0]));
    expect(RocStr.eq(array[1], expectedArray[1]));
    expect(RocStr.eq(array[2], expectedArray[2]));
}

// This is used for `Str.split : Str, Str -> array Str
// It is used to count how many segments the input `Str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
export fn count_segments_(
    strBytesPtrs: [*]u8,
    strLen: usize,
    delimiterBytes: [*]u8,
    delimiterLen: usize
) i64 {
    var count: i64 = 1;

    if (strLen > delimiterLen) {
        var strIndex: usize = 0;
        const endCond: usize = strLen - delimiterLen;

        while (strIndex < endCond) {
            var delimiterIndex: usize = 0;

            var matchesDelimiter = true;

            while (delimiterIndex < delimiterLen) {
                const delimiterChar = delimiterBytes[delimiterIndex];
                const strChar = strBytesPtrs[strIndex + delimiterIndex];

                if (delimiterChar != strChar) {
                    matchesDelimiter = false;
                    break;
                }

                delimiterIndex += 1;
            }

            if (matchesDelimiter) {
                count += 1;
            }

            strIndex += 1;
        }
    }

    return count;
}

test "count_segments_: long delimiter" {
    // Str.split "str" "delimiter" == [ "str" ]
    // 1 segment

    const strLen: usize = 3;
    var str: [strLen]u8 = "str".*;
    const strPtr: [*]u8 = &str;

    const delimiterLen = 9;
    var delimiter: [delimiterLen]u8 = "delimiter".*;
    const delimiterPtr: [*]u8 = &delimiter;

    const segmentsCount = count_segments_(
        strPtr,
        strLen,
        delimiterPtr,
        delimiterLen
    );

    expect(segmentsCount == 1);
}

test "count_segments_: delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments

    const strLen: usize = 11;
    var str: [strLen]u8 = "hello there".*;
    const strPtr: [*]u8 = &str;

    const delimiterLen = 5;
    var delimiter: [delimiterLen]u8 = "hello".*;
    const delimiterPtr: [*]u8 = &delimiter;

    const segmentsCount = count_segments_(
        strPtr,
        strLen,
        delimiterPtr,
        delimiterLen
    );

    expect(segmentsCount == 2);
}

test "count_segments_: delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments

    const strLen: usize = 5;
    var str: [strLen]u8 = "a!b!c".*;
    const strPtr: [*]u8 = &str;

    const delimiterLen = 1;
    var delimiter: [delimiterLen]u8 = "!".*;
    const delimiterPtr: [*]u8 = &delimiter;

    const segmentsCount = count_segments_(
        strPtr,
        strLen,
        delimiterPtr,
        delimiterLen
    );

    expect(segmentsCount == 3);
}
