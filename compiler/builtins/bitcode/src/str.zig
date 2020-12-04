const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

const RocStr = extern struct {
    bytesPtr: ?[*]u8,
    bytesCount: usize,

    pub inline fn empty() RocStr {
        return RocStr{
            .bytesCount = 0,
            .bytesPtr = null,
        };
    }

    // This clones the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn init(allocator: *Allocator, bytesPtr: [*]const u8, length: usize) RocStr {
        const rocStrSize = @sizeOf(RocStr);

        if (length < rocStrSize) {
            const retSmallStr = RocStr.empty();
            const targetPtr = @ptrToInt(&retSmallStr);
            var index: u8 = 0;

            // TODO isn't there a way to bulk-zero data in Zig?
            // Zero out the data, just to be safe
            while (index < rocStrSize) {
                var offsetPtr = @intToPtr(*u8, targetPtr + index);
                offsetPtr.* = 0;
                index += 1;
            }

            // TODO rewrite this into a for loop
            index = 0;
            while (index < length) {
                var offsetPtr = @intToPtr(*u8, targetPtr + index);
                offsetPtr.* = bytesPtr[index];
                index += 1;
            }

            // set the final byte to be the length
            const finalBytePtr = @intToPtr(*u8, targetPtr + rocStrSize - 1);
            finalBytePtr.* = @truncate(u8, length) ^ 0b10000000;

            return retSmallStr;
        } else {
            var result = allocateStr(allocator, u64, InPlace.Clone, length);

            @memcpy(@ptrCast([*]u8, result.bytesPtr), bytesPtr, length);

            return result;
        }
    }

    // This takes ownership of the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn withCapacity(length: usize) RocStr {
        const rocStrSize = @sizeOf(RocStr);

        if (length < rocStrSize) {
            return RocStr.empty();
        } else {
            var newBytes: []u8 = mem.dupe(allocator, u8, bytes_ptr[0..length]) catch unreachable;

            var newBytesPtr: [*]u8 = @ptrCast([*]u8, &new_bytes);

            return RocStr{
                .bytesPtr = newBytesPtr,
                .bytesCount = length,
            };
        }
    }

    pub fn deinit(self: RocStr, allocator: *Allocator) void {
        if (!self.isSmallStr()) {
            const strBytesPtr: [*]u8 = self.bytesPtr orelse unreachable;

            const strBytes: []u8 = strBytesPtr[0..self.bytesCount];
            allocator.free(strBytes);
        }
    }

    pub fn eq(self: RocStr, other: RocStr) bool {
        const selfBytesPtr: ?[*]const u8 = self.bytesPtr;
        const otherBytesPtr: ?[*]const u8 = other.bytesPtr;

        // If they are byte-for-byte equal, they're definitely equal!
        if (selfBytesPtr == otherBytesPtr and self.bytesCount == other.bytesCount) {
            return true;
        }

        const selfLen = self.len();
        const otherLen = other.len();

        // If their lengths are different, they're definitely unequal.
        if (selfLen != otherLen) {
            return false;
        }

        const selfPtrU8: [*]const u8 = @ptrCast([*]const u8, &self);
        const otherPtrU8: [*]const u8 = @ptrCast([*]const u8, &other);
        const selfBytes: [*]const u8 = if (self.isSmallStr() or self.isEmpty()) selfPtrU8 else selfBytesPtr orelse unreachable;
        const otherBytes: [*]const u8 = if (other.isSmallStr() or other.isEmpty()) otherPtrU8 else otherBytesPtr orelse unreachable;

        var index: usize = 0;
        const length = self.len();

        while (index < length) {
            if (selfBytes[index] != otherBytes[index]) {
                return false;
            }

            index = index + 1;
        }

        return true;
    }

    pub fn isSmallStr(self: RocStr) bool {
        return @bitCast(isize, self.bytesCount) < 0;
    }

    pub fn len(self: RocStr) usize {
        const bytes: [*]const u8 = @ptrCast([*]const u8, &self);
        const lastByte = bytes[@sizeOf(RocStr) - 1];
        const smallLen = @as(usize, lastByte ^ 0b1000_0000);
        const bigLen = self.bytesCount;

        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        return if (self.isSmallStr()) smallLen else bigLen;
    }

    pub fn isEmpty(self: RocStr) bool {
        return self.len() == 0;
    }

    pub fn asU8ptr(self: RocStr) [*]u8 {
        const ifSmall = &@bitCast([16]u8, self);
        const ifBig = @ptrCast([*]u8, self.bytesPtr);
        return if (self.isSmallStr() or self.isEmpty()) ifSmall else ifBig;
    }

    // Given a pointer to some bytes, write the first (len) bytes of this
    // RocStr's contents into it.
    //
    // One use for this function is writing into an `alloca` for a C string that
    // only needs to live long enough to be passed as an argument to
    // a C function - like the file path argument to `fopen`.
    pub fn memcpy(self: RocStr, dest: [*]u8, len: usize) void {
        const smallSrc = @ptrCast(*u8, self);
        const bigSrc = self.bytesPtr;

        // For a small string, copy the bytes directly from `self`.
        // For a large string, copy from the pointed-to bytes.

        // Since this conditional would be prone to branch misprediction,
        // make sure it will compile to a cmov.
        const src: [*]u8 = if (self.isSmallStr()) smallSrc else bigSrc;

        @memcpy(dest, src, len);
    }

    test "RocStr.eq: equal" {
        const str1Len = 3;
        var str1: [str1Len]u8 = "abc".*;
        const str1Ptr: [*]u8 = &str1;
        var rocStr1 = RocStr.init(testing.allocator, str1Ptr, str1Len);

        const str2Len = 3;
        var str2: [str2Len]u8 = "abc".*;
        const str2Ptr: [*]u8 = &str2;
        var rocStr2 = RocStr.init(testing.allocator, str2Ptr, str2Len);

        // TODO: fix those tests
        // expect(rocStr1.eq(rocStr2));

        rocStr1.deinit(testing.allocator);
        rocStr2.deinit(testing.allocator);
    }

    test "RocStr.eq: not equal different length" {
        const str1Len = 4;
        var str1: [str1Len]u8 = "abcd".*;
        const str1Ptr: [*]u8 = &str1;
        var rocStr1 = RocStr.init(testing.allocator, str1Ptr, str1Len);

        const str2Len = 3;
        var str2: [str2Len]u8 = "abc".*;
        const str2Ptr: [*]u8 = &str2;
        var rocStr2 = RocStr.init(testing.allocator, str2Ptr, str2Len);

        expect(!rocStr1.eq(rocStr2));

        rocStr1.deinit(testing.allocator);
        rocStr2.deinit(testing.allocator);
    }

    test "RocStr.eq: not equal same length" {
        const str1Len = 3;
        var str1: [str1Len]u8 = "acb".*;
        const str1Ptr: [*]u8 = &str1;
        var rocStr1 = RocStr.init(testing.allocator, str1Ptr, str1Len);

        const str2Len = 3;
        var str2: [str2Len]u8 = "abc".*;
        const str2Ptr: [*]u8 = &str2;
        var rocStr2 = RocStr.init(testing.allocator, str2Ptr, str2Len);

        // TODO: fix those tests
        // expect(!rocStr1.eq(rocStr2));

        rocStr1.deinit(testing.allocator);
        rocStr2.deinit(testing.allocator);
    }
};

// Str.numberOfBytes

pub fn strNumberOfBytes(string: RocStr) callconv(.C) usize {
    return string.len();
}

// Str.fromInt
pub fn strFromIntC(int: i64) callconv(.C) RocStr {
    return strFromInt(std.heap.c_allocator, int);
}

inline fn strFromInt(allocator: *Allocator, int: i64) RocStr {
    // prepare for having multiple integer types in the future
    return strFromIntHelp(allocator, i64, int);
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

inline fn strSplitInPlace(allocator: *Allocator, array: [*]RocStr, string: RocStr, delimiter: RocStr) void {
    var retArrayIndex: usize = 0;
    var sliceStartIndex: usize = 0;
    var strIndex: usize = 0;

    const bytesPtr = string.asU8ptr();
    const bytesCount = string.len();

    const delimiterBytesPtrs = delimiter.asU8ptr();
    const delimiterLen = delimiter.len();

    if (bytesCount > delimiterLen) {
        const endIndex: usize = bytesCount - delimiterLen + 1;
        while (strIndex <= endIndex) {
            var delimiterIndex: usize = 0;
            var matchesDelimiter = true;

            while (delimiterIndex < delimiterLen) {
                var delimiterChar = delimiterBytesPtrs[delimiterIndex];
                var strChar = bytesPtr[strIndex + delimiterIndex];

                if (delimiterChar != strChar) {
                    matchesDelimiter = false;
                    break;
                }

                delimiterIndex += 1;
            }

            if (matchesDelimiter) {
                const segmentLen: usize = strIndex - sliceStartIndex;

                array[retArrayIndex] = RocStr.init(allocator, bytesPtr + sliceStartIndex, segmentLen);
                sliceStartIndex = strIndex + delimiterLen;
                retArrayIndex += 1;
                strIndex += delimiterLen;
            } else {
                strIndex += 1;
            }
        }
    }

    array[retArrayIndex] = RocStr.init(allocator, bytesPtr + sliceStartIndex, bytesCount - sliceStartIndex);
}

// When we actually use this in Roc, libc will be linked so we have access to std.heap.c_allocator
pub fn strSplitInPlaceC(array: [*]RocStr, string: RocStr, delimiter: RocStr) callconv(.C) void {
    strSplitInPlace(std.heap.c_allocator, array, string, delimiter);
}

test "strSplitInPlace: no delimiter" {
    // Str.split "abc" "!" == [ "abc" ]
    const strArr = "abc";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "!";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    var array: [1]RocStr = undefined;
    const arrayPtr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, arrayPtr, str, delimiter);

    var expected = [1]RocStr{
        str,
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
}

test "strSplitInPlace: empty end" {
    const strArr = "1---- ---- ---- ---- ----2---- ---- ---- ---- ----";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "---- ---- ---- ---- ----";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    var array: [3]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const arrayPtr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, arrayPtr, str, delimiter);

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
    const strArr = "tttghittt";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "ttt";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    var array: [3]RocStr = [_]RocStr{
        undefined,
        undefined,
        undefined,
    };
    const arrayPtr: [*]RocStr = &array;
    strSplitInPlace(testing.allocator, arrayPtr, str, delimiter);

    const ghiArr = "ghi";
    const ghi = RocStr.init(testing.allocator, ghiArr, ghiArr.len);

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
    const strArr = "a!b!c";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "!";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    const arrayLen: usize = 3;
    var array: [arrayLen]RocStr = undefined;
    const arrayPtr: [*]RocStr = &array;

    strSplitInPlace(testing.allocator, arrayPtr, str, delimiter);

    const a = RocStr.init(testing.allocator, "a", 1);
    const b = RocStr.init(testing.allocator, "b", 1);
    const c = RocStr.init(testing.allocator, "c", 1);

    var expectedArray = [arrayLen]RocStr{
        a, b, c,
    };

    defer {
        for (array) |rocStr| {
            rocStr.deinit(testing.allocator);
        }

        for (expectedArray) |rocStr| {
            rocStr.deinit(testing.allocator);
        }

        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    expectEqual(expectedArray.len, array.len);
    expect(array[0].eq(expectedArray[0]));
    expect(array[1].eq(expectedArray[1]));
    expect(array[2].eq(expectedArray[2]));
}

// This is used for `Str.split : Str, Str -> Array Str
// It is used to count how many segments the input `_str`
// needs to be broken into, so that we can allocate a array
// of that size. It always returns at least 1.
pub fn countSegments(string: RocStr, delimiter: RocStr) callconv(.C) usize {
    const bytesPtr = string.asU8ptr();
    const bytesCount = string.len();

    const delimiterBytesPtrs = delimiter.asU8ptr();
    const delimiterLen = delimiter.len();

    var count: usize = 1;

    if (bytesCount > delimiterLen) {
        var strIndex: usize = 0;
        const endCond: usize = bytesCount - delimiterLen + 1;

        while (strIndex < endCond) {
            var delimiterIndex: usize = 0;

            var matchesDelimiter = true;

            while (delimiterIndex < delimiterLen) {
                const delimiterChar = delimiterBytesPtrs[delimiterIndex];
                const strChar = bytesPtr[strIndex + delimiterIndex];

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

test "countSegments: long delimiter" {
    // Str.split "str" "delimiter" == [ "str" ]
    // 1 segment
    const strArr = "str";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "delimiter";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    defer {
        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    const segmentsCount = countSegments(str, delimiter);

    expectEqual(segmentsCount, 1);

    str.deinit(testing.allocator);
    delimiter.deinit(testing.allocator);
}

test "countSegments: delimiter at start" {
    // Str.split "hello there" "hello" == [ "", " there" ]
    // 2 segments
    const strArr = "hello there";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "hello";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    defer {
        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    const segmentsCount = countSegments(str, delimiter);

    expectEqual(segmentsCount, 2);
}

test "countSegments: delimiter interspered" {
    // Str.split "a!b!c" "!" == [ "a", "b", "c" ]
    // 3 segments
    const strArr = "a!b!c";
    const str = RocStr.init(testing.allocator, strArr, strArr.len);

    const delimiterArr = "!";
    const delimiter = RocStr.init(testing.allocator, delimiterArr, delimiterArr.len);

    defer {
        str.deinit(testing.allocator);
        delimiter.deinit(testing.allocator);
    }

    const segmentsCount = countSegments(str, delimiter);

    expectEqual(segmentsCount, 3);
}

// Str.countGraphemeClusters
const grapheme = @import("helpers/grapheme.zig");

pub fn countGraphemeClusters(string: RocStr) callconv(.C) usize {
    if (string.isEmpty()) {
        return 0;
    }

    const bytesLen = string.len();
    const bytesPtr = string.asU8ptr();

    var bytes = bytesPtr[0..bytesLen];
    var iter = (unicode.Utf8View.init(bytes) catch unreachable).iterator();

    var count: usize = 0;
    var graphemeBreakState: ?grapheme.BoundClass = null;
    var graphemeBreakStatePtr = &graphemeBreakState;
    var optLastCodepoint: ?u21 = null;
    while (iter.nextCodepoint()) |curCodepoint| {
        if (optLastCodepoint) |lastCodepoint| {
            var didBreak = grapheme.isGraphemeBreak(lastCodepoint, curCodepoint, graphemeBreakStatePtr);
            if (didBreak) {
                count += 1;
                graphemeBreakState = null;
            }
        }
        optLastCodepoint = curCodepoint;
    }

    // If there are no breaks, but the str is not empty, then there
    // must be a single grapheme
    if (bytesLen != 0) {
        count += 1;
    }

    return count;
}

fn rocStrFromLiteral(bytesArr: *const []u8) RocStr {}

test "countGraphemeClusters: empty string" {
    const count = countGraphemeClusters(RocStr.empty());
    expectEqual(count, 0);
}

test "countGraphemeClusters: ascii characters" {
    const bytesArr = "abcd";
    const bytesLen = bytesArr.len;
    const count = countGraphemeClusters(RocStr.init(testing.allocator, bytesArr, bytesLen));
    expectEqual(count, 4);
}

test "countGraphemeClusters: utf8 characters" {
    const bytesArr = "ãxā";
    const bytesLen = bytesArr.len;
    const count = countGraphemeClusters(RocStr.init(testing.allocator, bytesArr, bytesLen));
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis" {
    const bytesArr = "🤔🤔🤔";
    const bytesLen = bytesArr.len;
    const count = countGraphemeClusters(RocStr.init(testing.allocator, bytesArr, bytesLen));
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis and ut8 characters" {
    const bytesArr = "🤔å🤔¥🤔ç";
    const bytesLen = bytesArr.len;
    const count = countGraphemeClusters(RocStr.init(testing.allocator, bytesArr, bytesLen));
    expectEqual(count, 6);
}

test "countGraphemeClusters: emojis, ut8, and ascii characters" {
    const bytesArr = "6🤔å🤔e¥🤔çpp";
    const bytesLen = bytesArr.len;
    const count = countGraphemeClusters(RocStr.init(testing.allocator, bytesArr, bytesLen));
    expectEqual(count, 10);
}

// Str.startsWith

pub fn startsWith(string: RocStr, prefix: RocStr) callconv(.C) bool {
    const bytesLen = string.len();
    const bytesPtr = string.asU8ptr();

    const prefixLen = prefix.len();
    const prefixPtr = prefix.asU8ptr();

    if (prefixLen > bytesLen) {
        return false;
    }

    // we won't exceed bytesLen due to the previous check
    var i: usize = 0;
    while (i < prefixLen) {
        if (bytesPtr[i] != prefixPtr[i]) {
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
    expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.init(testing.allocator, "12345678912345678910", 20);
    const prefix = RocStr.init(testing.allocator, "123456789123456789", 18);

    expect(startsWith(str, prefix));
}

// Str.endsWith

pub fn endsWith(string: RocStr, suffix: RocStr) callconv(.C) bool {
    const bytesLen = string.len();
    const bytesPtr = string.asU8ptr();

    const suffixLen = suffix.len();
    const suffixPtr = suffix.asU8ptr();

    if (suffixLen > bytesLen) {
        return false;
    }

    const offset: usize = bytesLen - suffixLen;
    var i: usize = 0;
    while (i < suffixLen) {
        if (bytesPtr[i + offset] != suffixPtr[i]) {
            return false;
        }
        i += 1;
    }
    return true;
}

test "endsWith: foo ends with oo" {
    const foo = RocStr.init(testing.allocator, "foo", 3);
    const oo = RocStr.init(testing.allocator, "oo", 2);
    expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init(testing.allocator, "123456789123456789", 18);
    expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init(testing.allocator, "12345678912345678910", 20);
    const suffix = RocStr.init(testing.allocator, "345678912345678910", 18);

    expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init(testing.allocator, "hello world", 11);
    const suffix = RocStr.init(testing.allocator, "world", 5);

    expect(endsWith(str, suffix));
}

// Str.concat

test "RocStr.concat: small concat small" {
    const str1Len = 3;
    var str1: [str1Len]u8 = "foo".*;
    const str1Ptr: [*]u8 = &str1;
    var rocStr1 = RocStr.init(testing.allocator, str1Ptr, str1Len);

    const str2Len = 3;
    var str2: [str2Len]u8 = "abc".*;
    const str2Ptr: [*]u8 = &str2;
    var rocStr2 = RocStr.init(testing.allocator, str2Ptr, str2Len);

    const str3Len = 6;
    var str3: [str3Len]u8 = "fooabc".*;
    const str3Ptr: [*]u8 = &str3;
    var rocStr3 = RocStr.init(testing.allocator, str3Ptr, str3Len);

    const result = strConcat(testing.allocator, 8, InPlace.Clone, rocStr1, rocStr2);

    expect(rocStr3.eq(result));

    rocStr1.deinit(testing.allocator);
    rocStr2.deinit(testing.allocator);
    rocStr3.deinit(testing.allocator);
    result.deinit(testing.allocator);
}

pub fn strConcatC(ptrSize: u32, resultInPlace: InPlace, arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return strConcat(std.heap.c_allocator, ptrSize, resultInPlace, arg1, arg2);
}

inline fn strConcat(allocator: *Allocator, ptrSize: u32, resultInPlace: InPlace, arg1: RocStr, arg2: RocStr) RocStr {
    return switch (ptrSize) {
        4 => strConcatHelp(allocator, i32, resultInPlace, arg1, arg2),
        8 => strConcatHelp(allocator, i64, resultInPlace, arg1, arg2),
        else => unreachable,
    };
}

fn strConcatHelp(allocator: *Allocator, comptime T: type, resultInPlace: InPlace, arg1: RocStr, arg2: RocStr) RocStr {
    if (arg1.isEmpty()) {
        return cloneStr(allocator, T, resultInPlace, arg2);
    } else if (arg2.isEmpty()) {
        return cloneStr(allocator, T, resultInPlace, arg1);
    } else {
        const combinedLen = arg1.len() + arg2.len();

        const smallBytesPtr = 2 * @sizeOf(T);
        const resultIsBig = combinedLen >= smallBytesPtr;

        if (resultIsBig) {
            var result = allocateStr(allocator, T, resultInPlace, combinedLen);

            {
                const oldIfSmall = &@bitCast([16]u8, arg1);
                const oldIfBig = @ptrCast([*]u8, arg1.bytesPtr);
                const oldBytes = if (arg1.isSmallStr()) oldIfSmall else oldIfBig;

                const newBytes: [*]u8 = @ptrCast([*]u8, result.bytesPtr);

                @memcpy(newBytes, oldBytes, arg1.len());
            }

            {
                const oldIfSmall = &@bitCast([16]u8, arg2);
                const oldIfBig = @ptrCast([*]u8, arg2.bytesPtr);
                const oldBytes = if (arg2.isSmallStr()) oldIfSmall else oldIfBig;

                const newBytes = @ptrCast([*]u8, result.bytesPtr) + arg1.len();

                @memcpy(newBytes, oldBytes, arg2.len());
            }

            return result;
        } else {
            var result = [16]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

            // if the result is small, then for sure arg1 and arg2 are also small

            {
                var oldBytes: [*]u8 = @ptrCast([*]u8, &@bitCast([16]u8, arg1));
                var newBytes: [*]u8 = @ptrCast([*]u8, &result);

                @memcpy(newBytes, oldBytes, arg1.len());
            }

            {
                var oldBytes: [*]u8 = @ptrCast([*]u8, &@bitCast([16]u8, arg2));
                var newBytes = @ptrCast([*]u8, &result) + arg1.len();

                @memcpy(newBytes, oldBytes, arg2.len());
            }

            const mask: u8 = 0b1000_0000;
            const finalByte = @truncate(u8, combinedLen) | mask;

            result[smallBytesPtr - 1] = finalByte;

            return @bitCast(RocStr, result);
        }

        return result;
    }
}

const InPlace = packed enum(u8) {
    InPlace,
    Clone,
};

fn cloneStr(allocator: *Allocator, comptime T: type, inPlace: InPlace, str: RocStr) RocStr {
    if (str.isSmallStr() or str.isEmpty()) {
        // just return the bytes
        return str;
    } else {
        var newStr = allocateStr(allocator, T, inPlace, str.bytesCount);

        var oldBytes: [*]u8 = @ptrCast([*]u8, str.bytesPtr);
        var newBytes: [*]u8 = @ptrCast([*]u8, newStr.bytesPtr);

        @memcpy(newBytes, oldBytes, str.bytesCount);

        return newStr;
    }
}

fn allocateStr(allocator: *Allocator, comptime T: type, inPlace: InPlace, numberOfChars: u64) RocStr {
    const length = @sizeOf(T) + numberOfChars;
    // TODO throw an exception if allocation fails
    var newBytes: []T = allocator.alloc(T, length) catch unreachable;

    if (inPlace == InPlace.InPlace) {
        newBytes[0] = @intCast(T, numberOfChars);
    } else {
        newBytes[0] = std.math.minInt(T);
    }

    var firstElement = @ptrCast([*]align(@alignOf(T)) u8, newBytes);
    firstElement += @sizeOf(usize);

    return RocStr{
        .bytesPtr = firstElement,
        .bytesCount = numberOfChars,
    };
}
