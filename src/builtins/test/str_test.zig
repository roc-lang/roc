//! TODO

const std = @import("std");
const builtins = @import("builtins");
const expect = std.testing.expect;

const SMALL_STR_MAX_LENGTH = builtins.str.SMALL_STR_MAX_LENGTH;
const Utf8DecodeError = builtins.str.Utf8DecodeError;
const Utf8ByteProblem = builtins.str.Utf8ByteProblem;
const ReverseUtf8View = builtins.str.ReverseUtf8View;
const FromUtf8Result = builtins.str.FromUtf8Result;
const RocListStr = builtins.str.RocListStr;
const TestEnv = builtins.utils.TestEnv;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const numberOfNextCodepointBytes = builtins.str.numberOfNextCodepointBytes;
const toErrUtf8ByteResponse = builtins.str.toErrUtf8ByteResponse;
const validateUtf8BytesX = builtins.str.validateUtf8BytesX;
const validateUtf8Bytes = builtins.str.validateUtf8Bytes;
const fromUtf8Lossy = builtins.str.fromUtf8Lossy;
const isWhitespace = builtins.str.isWhitespace;
const strTrimStart = builtins.str.strTrimStart;
const strJoinWith = builtins.str.strJoinWith;
const strTrimEnd = builtins.str.strTrimEnd;
const startsWith = builtins.str.startsWith;
const strConcat = builtins.str.strConcat;
const sliceHelp = builtins.str.sliceHelp;
const endsWith = builtins.str.endsWith;
const rcNone = builtins.utils.rcNone;
const strTrim = builtins.str.strTrim;
const reserve = builtins.str.reserve;

fn expectOk(result: FromUtf8Result) !void {
    try std.testing.expectEqual(result.is_ok, true);
}

test "isSmallStr: returns true for empty string" {
    const str = RocStr.empty();
    try expect(str.isSmallStr());
}

test "RocStr.eq: small, equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1_len = 3;
    var str1: [str1_len]u8 = "abc".*;
    const str1_ptr: [*]u8 = &str1;
    var roc_str1 = RocStr.init(str1_ptr, str1_len, test_env.getOps());

    const str2_len = 3;
    var str2: [str2_len]u8 = "abc".*;
    const str2_ptr: [*]u8 = &str2;
    var roc_str2 = RocStr.init(str2_ptr, str2_len, test_env.getOps());

    try expect(roc_str1.eq(roc_str2));

    roc_str1.decref(test_env.getOps());
    roc_str2.decref(test_env.getOps());
}

test "RocStr.eq: small, not equal, different length" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1_len = 4;
    var str1: [str1_len]u8 = "abcd".*;
    const str1_ptr: [*]u8 = &str1;
    var roc_str1 = RocStr.init(str1_ptr, str1_len, test_env.getOps());

    const str2_len = 3;
    var str2: [str2_len]u8 = "abc".*;
    const str2_ptr: [*]u8 = &str2;
    var roc_str2 = RocStr.init(str2_ptr, str2_len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try expect(!roc_str1.eq(roc_str2));
}

test "RocStr.eq: small, not equal, same length" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1_len = 3;
    var str1: [str1_len]u8 = "acb".*;
    const str1_ptr: [*]u8 = &str1;
    var roc_str1 = RocStr.init(str1_ptr, str1_len, test_env.getOps());

    const str2_len = 3;
    var str2: [str2_len]u8 = "abc".*;
    const str2_ptr: [*]u8 = &str2;
    var roc_str2 = RocStr.init(str2_ptr, str2_len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try expect(!roc_str1.eq(roc_str2));
}

test "RocStr.eq: large, equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const content = "012345678901234567890123456789";
    const roc_str1 = RocStr.init(content, content.len, test_env.getOps());
    const roc_str2 = RocStr.init(content, content.len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try expect(roc_str1.eq(roc_str2));
}

test "RocStr.eq: large, different lengths, unequal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const content1 = "012345678901234567890123456789";
    const roc_str1 = RocStr.init(content1, content1.len, test_env.getOps());
    const content2 = "012345678901234567890";
    const roc_str2 = RocStr.init(content2, content2.len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try expect(!roc_str1.eq(roc_str2));
}

test "RocStr.eq: large, different content, unequal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const content1 = "012345678901234567890123456789!!";
    const roc_str1 = RocStr.init(content1, content1.len, test_env.getOps());
    const content2 = "012345678901234567890123456789--";
    const roc_str2 = RocStr.init(content2, content2.len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try expect(!roc_str1.eq(roc_str2));
}

test "RocStr.eq: large, garbage after end, equal" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const content = "012345678901234567890123456789";
    const roc_str1 = RocStr.init(content, content.len, test_env.getOps());
    const roc_str2 = RocStr.init(content, content.len, test_env.getOps());
    try expect(roc_str1.bytes != roc_str2.bytes);

    // Insert garbage after the end of each string
    roc_str1.bytes.?[30] = '!';
    roc_str1.bytes.?[31] = '!';
    roc_str2.bytes.?[30] = '-';
    roc_str2.bytes.?[31] = '-';

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
    }

    try expect(roc_str1.eq(roc_str2));
}

test "strSplitHelp: empty delimiter" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "abc" "" == ["abc"]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref(test_env.getOps());
        }

        for (expected) |roc_str| {
            roc_str.decref(test_env.getOps());
        }

        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitHelp: no delimiter" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "abc" "!" == ["abc"]
    const str_arr = "abc";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    var array: [1]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const expected = [1]RocStr{
        str,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref(test_env.getOps());
        }

        for (expected) |roc_str| {
            roc_str.decref(test_env.getOps());
        }

        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
}

test "strSplitHelp: empty start" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str_arr = "/a";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "/";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    const array_len: usize = 2;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const one = RocStr.init("a", 1, test_env.getOps());

    const expected = [2]RocStr{
        RocStr.empty(), one,
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        for (expected) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: empty end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str_arr = "1---- ---- ---- ---- ----2---- ---- ---- ---- ----";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "---- ---- ---- ---- ----";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    const array_len: usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const one = RocStr.init("1", 1, test_env.getOps());
    const two = RocStr.init("2", 1, test_env.getOps());

    const expected = [3]RocStr{
        one, two, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        for (expected) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitHelp: string equals delimiter" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str_delimiter_arr = "/";
    const str_delimiter = RocStr.init(str_delimiter_arr, str_delimiter_arr.len, test_env.getOps());

    const array_len: usize = 2;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str_delimiter, str_delimiter, test_env.getOps());

    const expected = [2]RocStr{ RocStr.empty(), RocStr.empty() };

    defer {
        for (array) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        for (expected) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        str_delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: delimiter on sides" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str_arr = "tttghittt";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "ttt";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    const array_len: usize = 3;
    var array: [array_len]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const array_ptr: [*]RocStr = &array;
    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const ghi_arr = "ghi";
    const ghi = RocStr.init(ghi_arr, ghi_arr.len, test_env.getOps());

    const expected = [3]RocStr{
        RocStr.empty(), ghi, RocStr.empty(),
    };

    defer {
        for (array) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        for (expected) |rocStr| {
            rocStr.decref(test_env.getOps());
        }

        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "strSplitHelp: three pieces" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "a!b!c" "!" == ["a", "b", "c"]
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    const array_len: usize = 3;
    var array: [array_len]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const a = RocStr.init("a", 1, test_env.getOps());
    const b = RocStr.init("b", 1, test_env.getOps());
    const c = RocStr.init("c", 1, test_env.getOps());

    const expected_array = [array_len]RocStr{
        a, b, c,
    };

    defer {
        for (array) |roc_str| {
            roc_str.decref(test_env.getOps());
        }

        for (expected_array) |roc_str| {
            roc_str.decref(test_env.getOps());
        }

        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    try std.testing.expectEqual(expected_array.len, array.len);
    try expect(array[0].eq(expected_array[0]));
    try expect(array[1].eq(expected_array[1]));
    try expect(array[2].eq(expected_array[2]));
}

test "strSplitHelp: overlapping delimiter 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "aaa" "aa" == ["", "a"]
    const str_arr = "aaa";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "aa";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    var array: [2]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const expected = [2]RocStr{
        RocStr.empty(),
        RocStr.init("a", 1, test_env.getOps()),
    };

    // strings are all small so we ignore freeing the memory

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
}

test "strSplitHelp: overlapping delimiter 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "aaa" "aa" == ["", "a"]
    const str_arr = "aaaa";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "aa";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    var array: [3]RocStr = undefined;
    const array_ptr: [*]RocStr = &array;

    builtins.str.strSplitOnHelp(array_ptr, str, delimiter, test_env.getOps());

    const expected = [3]RocStr{
        RocStr.empty(),
        RocStr.empty(),
        RocStr.empty(),
    };

    // strings are all small so we ignore freeing the memory

    try std.testing.expectEqual(array.len, expected.len);
    try expect(array[0].eq(expected[0]));
    try expect(array[1].eq(expected[1]));
    try expect(array[2].eq(expected[2]));
}

test "countSegments: long delimiter" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "str" "delimiter" == ["str"]
    // 1 segment
    const str_arr = "str";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    defer {
        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    const segments_count = builtins.str.countSegments(str, delimiter);
    try std.testing.expectEqual(segments_count, 1);
}

test "countSegments: delimiter at start" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "hello there" "hello" == ["", " there"]
    // 2 segments
    const str_arr = "hello there";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "hello";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    defer {
        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    const segments_count = builtins.str.countSegments(str, delimiter);

    try std.testing.expectEqual(segments_count, 2);
}

test "countSegments: delimiter interspered" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "a!b!c" "!" == ["a", "b", "c"]
    // 3 segments
    const str_arr = "a!b!c";
    const str = RocStr.init(str_arr, str_arr.len, test_env.getOps());

    const delimiter_arr = "!";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len, test_env.getOps());

    defer {
        str.decref(test_env.getOps());
        delimiter.decref(test_env.getOps());
    }

    const segments_count = builtins.str.countSegments(str, delimiter);

    try std.testing.expectEqual(segments_count, 3);
}

test "countSegments: string equals delimiter" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "/" "/" == ["", ""]
    // 2 segments
    const str_delimiter_arr = "/";
    const str_delimiter = RocStr.init(str_delimiter_arr, str_delimiter_arr.len, test_env.getOps());

    defer {
        str_delimiter.decref(test_env.getOps());
    }

    const segments_count = builtins.str.countSegments(str_delimiter, str_delimiter);

    try std.testing.expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 1" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "aaa" "aa" == ["", "a"]
    const segments_count = builtins.str.countSegments(
        RocStr.init("aaa", 3, test_env.getOps()),
        RocStr.init("aa", 2, test_env.getOps()),
    );

    try std.testing.expectEqual(segments_count, 2);
}

test "countSegments: overlapping delimiter 2" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // Str.splitOn "aaa" "aa" == ["", "a"]
    const segments_count = builtins.str.countSegments(
        RocStr.init("aaaa", 4, test_env.getOps()),
        RocStr.init("aa", 2, test_env.getOps()),
    );

    try std.testing.expectEqual(segments_count, 3);
}

test "substringUnsafe: start" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("abcdef", test_env.getOps());
    defer str.decref(test_env.getOps());

    const expected = RocStr.fromSlice("abc", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const actual = builtins.str.substringUnsafe(str, 0, 3, test_env.getOps());

    try expect(RocStr.eq(actual, expected));
}

test "substringUnsafe: middle" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("abcdef", test_env.getOps());
    defer str.decref(test_env.getOps());

    const expected = RocStr.fromSlice("bcd", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const actual = builtins.str.substringUnsafe(str, 1, 3, test_env.getOps());

    try expect(RocStr.eq(actual, expected));
}

test "substringUnsafe: end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("a string so long it is heap-allocated", test_env.getOps());
    defer str.decref(test_env.getOps());

    const expected = RocStr.fromSlice("heap-allocated", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const actual = builtins.str.substringUnsafe(str, 23, 37 - 23, test_env.getOps());

    try expect(RocStr.eq(actual, expected));
}

test "startsWith: food starts with foo" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const food = RocStr.fromSlice("food", test_env.getOps());
    const foo = RocStr.fromSlice("foo", test_env.getOps());
    try expect(startsWith(food, foo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("123456789123456789", test_env.getOps());
    defer str.decref(test_env.getOps());
    try expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.fromSlice("12345678912345678910", test_env.getOps());
    defer str.decref(test_env.getOps());
    const prefix = RocStr.fromSlice("123456789123456789", test_env.getOps());
    defer prefix.decref(test_env.getOps());

    try expect(startsWith(str, prefix));
}

test "endsWith: foo ends with oo" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const foo = RocStr.init("foo", 3, test_env.getOps());
    const oo = RocStr.init("oo", 2, test_env.getOps());
    defer foo.decref(test_env.getOps());
    defer oo.decref(test_env.getOps());

    try expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.init("123456789123456789", 18, test_env.getOps());
    defer str.decref(test_env.getOps());
    try expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.init("12345678912345678910", 20, test_env.getOps());
    const suffix = RocStr.init("345678912345678910", 18, test_env.getOps());
    defer str.decref(test_env.getOps());
    defer suffix.decref(test_env.getOps());

    try expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str = RocStr.init("hello world", 11, test_env.getOps());
    const suffix = RocStr.init("world", 5, test_env.getOps());
    defer str.decref(test_env.getOps());
    defer suffix.decref(test_env.getOps());

    try expect(endsWith(str, suffix));
}

test "RocStr.concat: small concat small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1_len = 3;
    var str1: [str1_len]u8 = "foo".*;
    const str1_ptr: [*]u8 = &str1;
    var roc_str1 = RocStr.init(str1_ptr, str1_len, test_env.getOps());

    const str2_len = 3;
    var str2: [str2_len]u8 = "abc".*;
    const str2_ptr: [*]u8 = &str2;
    var roc_str2 = RocStr.init(str2_ptr, str2_len, test_env.getOps());

    const str3_len = 6;
    var str3: [str3_len]u8 = "fooabc".*;
    const str3_ptr: [*]u8 = &str3;
    var roc_str3 = RocStr.init(str3_ptr, str3_len, test_env.getOps());

    defer {
        roc_str1.decref(test_env.getOps());
        roc_str2.decref(test_env.getOps());
        roc_str3.decref(test_env.getOps());
    }

    const result = strConcat(roc_str1, roc_str2, test_env.getOps());

    defer result.decref(test_env.getOps());

    try expect(roc_str3.eq(result));
}

test "RocStr.joinWith: result is big" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const sep_len = 2;
    var sep: [sep_len]u8 = ", ".*;
    const sep_ptr: [*]u8 = &sep;
    var roc_sep = RocStr.init(sep_ptr, sep_len, test_env.getOps());

    const elem_len = 13;
    var elem: [elem_len]u8 = "foobarbazspam".*;
    const elem_ptr: [*]u8 = &elem;
    var roc_elem = RocStr.init(elem_ptr, elem_len, test_env.getOps());

    const result_len = 43;
    var xresult: [result_len]u8 = "foobarbazspam, foobarbazspam, foobarbazspam".*;
    const result_ptr: [*]u8 = &xresult;
    var roc_result = RocStr.init(result_ptr, result_len, test_env.getOps());

    var elements: [3]RocStr = .{ roc_elem, roc_elem, roc_elem };
    const list = RocListStr{
        .list_length = 3,
        .list_capacity_or_alloc_ptr = 3,
        .list_elements = @as([*]RocStr, @ptrCast(&elements)),
    };

    defer {
        roc_sep.decref(test_env.getOps());
        roc_elem.decref(test_env.getOps());
        roc_result.decref(test_env.getOps());
    }

    const result = strJoinWith(list, roc_sep, test_env.getOps());

    defer result.decref(test_env.getOps());

    try expect(roc_result.eq(result));
}

test "validateUtf8Bytes: ascii" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const raw = "abc";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    const str_result = validateUtf8BytesX(list, test_env.getOps());
    defer str_result.string.decref(test_env.getOps());
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode œ" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const raw = "œ";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    const str_result = validateUtf8BytesX(list, test_env.getOps());
    defer str_result.string.decref(test_env.getOps());
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode ∆" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const raw = "∆";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    const str_result = validateUtf8BytesX(list, test_env.getOps());
    defer str_result.string.decref(test_env.getOps());
    try expectOk(str_result);
}

test "validateUtf8Bytes: emoji" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const raw = "💖";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    const str_result = validateUtf8BytesX(list, test_env.getOps());
    defer str_result.string.decref(test_env.getOps());
    try expectOk(str_result);
}

test "validateUtf8Bytes: unicode ∆ in middle of array" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const raw = "œb∆c¬";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    const str_result = validateUtf8BytesX(list, test_env.getOps());
    defer str_result.string.decref(test_env.getOps());
    try expectOk(str_result);
}

test "fromUtf8Lossy: ascii, emoji" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.fromSlice(u8, "r💖c", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r💖c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try expect(expected.eq(res));
}

fn expectErr(
    list: RocList,
    index: usize,
    err: Utf8DecodeError,
    problem: Utf8ByteProblem,
    test_env: *TestEnv,
) !void {
    const str_ptr = @as([*]u8, @ptrCast(list.bytes));
    const len = list.length;

    try std.testing.expectError(err, numberOfNextCodepointBytes(str_ptr[0..len], index));
    try std.testing.expectEqual(
        toErrUtf8ByteResponse(index, problem),
        validateUtf8Bytes(str_ptr, len, test_env.getOps()),
    );
}

test "validateUtf8Bytes: invalid start byte" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "ab\x80c";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        2,
        error.Utf8InvalidStartByte,
        Utf8ByteProblem.InvalidStartByte,
        &test_env,
    );
}

test "validateUtf8Bytes: unexpected eof for 2 byte sequence" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.UnexpectedEof,
        Utf8ByteProblem.UnexpectedEndOfSequence,
        &test_env,
    );
}

test "validateUtf8Bytes: expected continuation for 2 byte sequence" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.Utf8ExpectedContinuation,
        Utf8ByteProblem.ExpectedContinuation,
        &test_env,
    );
}

test "validateUtf8Bytes: unexpected eof for 3 byte sequence" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.UnexpectedEof,
        Utf8ByteProblem.UnexpectedEndOfSequence,
        &test_env,
    );
}

test "validateUtf8Bytes: expected continuation for 3 byte sequence" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\xa0\xc0";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.Utf8ExpectedContinuation,
        Utf8ByteProblem.ExpectedContinuation,
        &test_env,
    );
}

test "validateUtf8Bytes: unexpected eof for 4 byte sequence" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.UnexpectedEof,
        Utf8ByteProblem.UnexpectedEndOfSequence,
        &test_env,
    );
}

test "validateUtf8Bytes: expected continuation for 4 byte sequence" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x80\x00";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.Utf8ExpectedContinuation,
        Utf8ByteProblem.ExpectedContinuation,
        &test_env,
    );
}

test "validateUtf8Bytes: overlong" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L451
    const raw = "abc\xf0\x80\x80\x80";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.Utf8OverlongEncoding,
        Utf8ByteProblem.OverlongEncoding,
        &test_env,
    );
}

test "validateUtf8Bytes: codepoint out too large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L465
    const raw = "abc\xf4\x90\x80\x80";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.Utf8CodepointTooLarge,
        Utf8ByteProblem.CodepointTooLarge,
        &test_env,
    );
}

test "validateUtf8Bytes: surrogate halves" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L468
    const raw = "abc\xed\xa0\x80";
    const ptr: [*]const u8 = @as([*]const u8, @ptrCast(raw));
    const list = sliceHelp(ptr, raw.len, test_env.getOps());

    try expectErr(
        list,
        3,
        error.Utf8EncodesSurrogateHalf,
        Utf8ByteProblem.EncodesSurrogateHalf,
        &test_env,
    );
}

test "fromUtf8Lossy: invalid start byte" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.fromSlice(u8, "r\x80c", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try expect(expected.eq(res));
}

test "fromUtf8Lossy: overlong encoding" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.fromSlice(u8, "r\xF0\x9F\x92\x96\x80c", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r💖�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try expect(expected.eq(res));
}

test "fromUtf8Lossy: expected continuation" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.fromSlice(u8, "r\xCFc", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try expect(expected.eq(res));
}

test "fromUtf8Lossy: unexpected end" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    var list = RocList.fromSlice(u8, "r\xCF", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try expect(expected.eq(res));
}

test "fromUtf8Lossy: encodes surrogate" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // 0xd83d == 0b1101_1000_0011_1101
    //             wwww xxxx yyyy zzzz
    // becomes 0b1110_1101 0b10_1000_00 0b10_11_1101
    //           1110_wwww   10_xxxx_yy   10_yy_zzzz
    //         0xED        0x90         0xBD
    var list = RocList.fromSlice(u8, "r\xED\xA0\xBDc", false, test_env.getOps());
    defer list.decref(@alignOf(u8), @sizeOf(u8), false, rcNone, test_env.getOps());

    const res = fromUtf8Lossy(list, test_env.getOps());
    defer res.decref(test_env.getOps());
    const expected = RocStr.fromSlice("r�c", test_env.getOps());
    defer expected.decref(test_env.getOps());
    try expect(expected.eq(res));
}

test "isWhitespace" {
    try expect(isWhitespace(' '));
    try expect(isWhitespace('\u{00A0}'));
    try expect(!isWhitespace('x'));
}

test "withAsciiLowercased: small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("cOFFÉ", test_env.getOps());
    try expect(original.isSmallStr());

    const expected = RocStr.fromSlice("coffÉ", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = builtins.str.strWithAsciiLowercased(original, test_env.getOps());
    defer str_result.decref(test_env.getOps());

    try expect(str_result.isSmallStr());
    try expect(str_result.eq(expected));
}

test "withAsciiLowercased: non small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ", test_env.getOps());
    defer original.decref(test_env.getOps());
    try expect(!original.isSmallStr());

    const expected = RocStr.fromSlice("coffÉ coffÉ coffÉ coffÉ coffÉ coffÉ", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = builtins.str.strWithAsciiLowercased(original, test_env.getOps());

    try expect(!str_result.isSmallStr());
    try expect(str_result.eq(expected));
}

test "withAsciiLowercased: seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const l = RocStr.fromSlice("cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ cOFFÉ", test_env.getOps());
    const original = builtins.str.substringUnsafeC(l, 1, l.len() - 1, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(original.isSeamlessSlice());

    const expected = RocStr.fromSlice("offÉ coffÉ coffÉ coffÉ coffÉ coffÉ", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = builtins.str.strWithAsciiLowercased(original, test_env.getOps());

    try expect(!str_result.isSmallStr());
    try expect(str_result.eq(expected));
}

test "withAsciiUppercased: small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("coffé", test_env.getOps());
    try expect(original.isSmallStr());

    const expected = RocStr.fromSlice("COFFé", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = builtins.str.strWithAsciiUppercased(original, test_env.getOps());
    defer str_result.decref(test_env.getOps());

    try expect(str_result.isSmallStr());
    try expect(str_result.eq(expected));
}

test "withAsciiUppercased: non small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    defer original.decref(test_env.getOps());
    try expect(!original.isSmallStr());

    const expected = RocStr.fromSlice("COFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = builtins.str.strWithAsciiUppercased(original, test_env.getOps());

    try expect(!str_result.isSmallStr());
    try expect(str_result.eq(expected));
}

test "withAsciiUppercased: seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const l = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    const original = builtins.str.substringUnsafeC(l, 1, l.len() - 1, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(original.isSeamlessSlice());

    const expected = RocStr.fromSlice("OFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer expected.decref(test_env.getOps());

    const str_result = builtins.str.strWithAsciiUppercased(original, test_env.getOps());

    try expect(!str_result.isSmallStr());
    try expect(str_result.eq(expected));
}

test "caselessAsciiEquals: same str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coFféÉ", test_env.getOps());
    defer str1.decref(test_env.getOps());

    const are_equal = builtins.str.strCaselessAsciiEquals(str1, str1);
    try expect(are_equal);
}

test "caselessAsciiEquals: differently capitalized non-ascii char" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coffé", test_env.getOps());
    defer str1.decref(test_env.getOps());
    try expect(str1.isSmallStr());

    const str2 = RocStr.fromSlice("coffÉ", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = builtins.str.strCaselessAsciiEquals(str1, str2);
    try expect(!are_equal);
}

test "caselessAsciiEquals: small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coffé", test_env.getOps());
    defer str1.decref(test_env.getOps());
    try expect(str1.isSmallStr());

    const str2 = RocStr.fromSlice("COFFé", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = builtins.str.strCaselessAsciiEquals(str1, str2);
    try expect(are_equal);
}

test "caselessAsciiEquals: non small str" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const str1 = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    defer str1.decref(test_env.getOps());
    try expect(!str1.isSmallStr());

    const str2 = RocStr.fromSlice("COFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = builtins.str.strCaselessAsciiEquals(str1, str2);

    try expect(are_equal);
}

test "caselessAsciiEquals: seamless slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const l = RocStr.fromSlice("coffé coffé coffé coffé coffé coffé", test_env.getOps());
    const str1 = builtins.str.substringUnsafeC(l, 1, l.len() - 1, test_env.getOps());
    defer str1.decref(test_env.getOps());

    try expect(str1.isSeamlessSlice());

    const str2 = RocStr.fromSlice("OFFé COFFé COFFé COFFé COFFé COFFé", test_env.getOps());
    defer str2.decref(test_env.getOps());

    const are_equal = builtins.str.strCaselessAsciiEquals(str1, str2);

    try expect(are_equal);
}

test "strTrim: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const trimmedEmpty = strTrim(RocStr.empty(), test_env.getOps());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrim: null byte" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const bytes = [_]u8{0};
    const original = RocStr.init(&bytes, 1, test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), original.len());
    try std.testing.expectEqual(@as(usize, SMALL_STR_MAX_LENGTH), original.getCapacity());

    const original_with_capacity = reserve(original, 40, test_env.getOps());
    defer original_with_capacity.decref(test_env.getOps());

    try std.testing.expectEqual(@as(usize, 1), original_with_capacity.len());
    try std.testing.expectEqual(@as(usize, 41), original_with_capacity.getCapacity());

    const trimmed = strTrim(original.clone(test_env.getOps()), test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try expect(original.eq(trimmed));
}

test "strTrim: blank" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    const trimmed = strTrim(original, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrim: large to large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(!expected.isSmallStr());

    const trimmed = strTrim(original, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try expect(trimmed.eq(expected));
}

test "strTrim: large to small sized slice" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = "             hello         ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try expect(!original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(expected.isSmallStr());

    try expect(original.isUnique());
    const trimmed = strTrim(original, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrim: small to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(original.isSmallStr());

    const expected_bytes = "hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(expected.isSmallStr());

    const trimmed = strTrim(original, test_env.getOps());

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimStart: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const trimmedEmpty = strTrimStart(RocStr.empty(), test_env.getOps());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimStart: blank" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    const trimmed = strTrimStart(original, test_env.getOps());

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimStart: large to large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(!original.isSmallStr());

    const expected_bytes = "hello even more giant world ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(!expected.isSmallStr());

    const trimmed = strTrimStart(original, test_env.getOps());

    try expect(trimmed.eq(expected));
}

test "strTrimStart: large to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = "                    hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try expect(!original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(expected.isSmallStr());

    const trimmed = strTrimStart(original, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrimStart: small to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(original.isSmallStr());

    const expected_bytes = "hello ";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(expected.isSmallStr());

    const trimmed = strTrimStart(original, test_env.getOps());

    try expect(trimmed.eq(expected));
    try expect(trimmed.isSmallStr());
}

test "strTrimEnd: empty" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const trimmedEmpty = strTrimEnd(RocStr.empty(), test_env.getOps());
    try expect(trimmedEmpty.eq(RocStr.empty()));
}

test "strTrimEnd: blank" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const original_bytes = "   ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    const trimmed = strTrimEnd(original, test_env.getOps());

    try expect(trimmed.eq(RocStr.empty()));
}

test "strTrimEnd: large to large" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();
    const original_bytes = " hello even more giant world ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(!original.isSmallStr());

    const expected_bytes = " hello even more giant world";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(!expected.isSmallStr());

    const trimmed = strTrimEnd(original, test_env.getOps());

    try expect(trimmed.eq(expected));
}

test "strTrimEnd: large to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    // `original` will be consumed by the concat; do not free explicitly
    const original_bytes = " hello                    ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());

    try expect(!original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(expected.isSmallStr());

    const trimmed = strTrimEnd(original, test_env.getOps());
    defer trimmed.decref(test_env.getOps());

    try expect(trimmed.eq(expected));
    try expect(!trimmed.isSmallStr());
}

test "strTrimEnd: small to small" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const original_bytes = " hello ";
    const original = RocStr.init(original_bytes, original_bytes.len, test_env.getOps());
    defer original.decref(test_env.getOps());

    try expect(original.isSmallStr());

    const expected_bytes = " hello";
    const expected = RocStr.init(expected_bytes, expected_bytes.len, test_env.getOps());
    defer expected.decref(test_env.getOps());

    try expect(expected.isSmallStr());

    const trimmed = strTrimEnd(original, test_env.getOps());

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
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data_bytes = "foobar";
    var data = RocStr.init(data_bytes, data_bytes.len, test_env.getOps());
    defer data.decref(test_env.getOps());

    try std.testing.expectEqual(data.getCapacity(), SMALL_STR_MAX_LENGTH);
}

test "capacity: big string" {
    var test_env = TestEnv.init(std.testing.allocator);
    defer test_env.deinit();

    const data_bytes = "a string so large that it must be heap-allocated";
    var data = RocStr.init(data_bytes, data_bytes.len, test_env.getOps());
    defer data.decref(test_env.getOps());

    try expect(data.getCapacity() >= data_bytes.len);
}
