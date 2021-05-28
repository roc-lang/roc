const formatfloat = @import("formatfloat.zig");
const utils = @import("utils.zig");
const roc_mem = @import("mem.zig");
const RocList = @import("list.zig").RocList;
const std = @import("std");
const mem = std.mem;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const unicode = std.unicode;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expectError = testing.expectError;
const expect = testing.expect;

const InPlace = packed enum(u8) {
    InPlace,
    Clone,
};

const SMALL_STR_MAX_LENGTH = small_string_size - 1;
const small_string_size = 2 * @sizeOf(usize);
const blank_small_string: [16]u8 = init_blank_small_string(small_string_size);

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
        return RocStr{
            .str_len = 0,
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

    pub fn initBig(in_place: InPlace, number_of_chars: u64) RocStr {
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
        if (!self.isSmallStr() and !self.isEmpty()) {
            utils.decref(self.str_bytes, self.str_len, RocStr.alignment);
        }
    }

    pub fn toSlice(self: RocStr) []u8 {
        const str_bytes_ptr: [*]u8 = self.str_bytes orelse unreachable;
        const str_bytes: []u8 = str_bytes_ptr[0..self.str_len];
        return str_bytes;
    }

    // This takes ownership of the pointed-to bytes if they won't fit in a
    // small string, and returns a (pointer, len) tuple which points to them.
    pub fn withCapacity(length: usize) RocStr {
        const roc_str_size = @sizeOf(RocStr);

        if (length < roc_str_size) {
            return RocStr.empty();
        } else {
            var new_bytes: []T = utils.alloc(length, RocStr.alignment) catch unreachable;

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

    pub fn clone(in_place: InPlace, str: RocStr) RocStr {
        if (str.isSmallStr() or str.isEmpty()) {
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

        if (self.bytes) |source_ptr| {
            if (self.isUnique()) {
                const new_source = utils.unsafeReallocate(source_ptr, RocStr.alignment, self.len(), new_length, element_width);

                return RocStr{ .str_bytes = new_source, .str_len = new_length };
            }
        }

        return self.reallocateFresh(RocStr.alignment, new_length, element_width);
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

    pub fn isSmallStr(self: RocStr) bool {
        // NOTE: returns False for empty string!
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

    pub fn isUnique(self: RocStr) bool {
        // the empty list is unique (in the sense that copying it will not leak memory)
        if (self.isEmpty()) {
            return true;
        }

        // small strings can be copied
        if (self.isSmallStr()) {
            return true;
        }

        // otherwise, check if the refcount is one
        const ptr: [*]usize = @ptrCast([*]usize, @alignCast(8, self.str_bytes));
        return (ptr - 1)[0] == utils.REFCOUNT_ONE;
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
    pub fn memcpy(self: RocStr, dest: [*]u8) void {
        const src = self.asU8ptr();
        @memcpy(dest, src, self.len());
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

        defer {
            roc_str1.deinit();
            roc_str2.deinit();
        }

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

        defer {
            roc_str1.deinit();
            roc_str2.deinit();
        }

        expect(!roc_str1.eq(roc_str2));
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
pub fn strFromIntC(int: i64) callconv(.C) RocStr {
    // prepare for having multiple integer types in the future
    return @call(.{ .modifier = always_inline }, strFromIntHelp, .{ i64, int });
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

// Str.fromFloat
pub fn strFromFloatC(float: f64) callconv(.C) RocStr {
    // NOTE the compiled zig for float formatting seems to use LLVM11-specific features
    // hopefully we can use zig instead of snprintf in the future when we upgrade
    var buf: [100]u8 = undefined;

    const result = formatfloat.formatF64(&buf, 100, "%f", float);

    return RocStr.init(&buf, @intCast(usize, result));
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

    expectEqual(array.len, expected.len);
    expect(array[0].eq(expected[0]));
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
    const str = RocStr.init(str_arr, str_arr.len);

    const delimiter_arr = "delimiter";
    const delimiter = RocStr.init(delimiter_arr, delimiter_arr.len);

    defer {
        str.deinit();
        delimiter.deinit();
    }

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

    defer {
        str.deinit();
        delimiter.deinit();
    }

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

    defer {
        str.deinit();
        delimiter.deinit();
    }

    const segments_count = countSegments(str, delimiter);

    expectEqual(segments_count, 3);
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

fn rocStrFromLiteral(bytes_arr: *const []u8) RocStr {}

test "countGraphemeClusters: empty string" {
    const count = countGraphemeClusters(RocStr.empty());
    expectEqual(count, 0);
}

test "countGraphemeClusters: ascii characters" {
    const bytes_arr = "abcd";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    expectEqual(count, 4);
}

test "countGraphemeClusters: utf8 characters" {
    const bytes_arr = "ãxā";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis" {
    const bytes_arr = "🤔🤔🤔";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    expectEqual(count, 3);
}

test "countGraphemeClusters: emojis and ut8 characters" {
    const bytes_arr = "🤔å🤔¥🤔ç";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    expectEqual(count, 6);
}

test "countGraphemeClusters: emojis, ut8, and ascii characters" {
    const bytes_arr = "6🤔å🤔e¥🤔çpp";
    const bytes_len = bytes_arr.len;
    const str = RocStr.init(bytes_arr, bytes_len);
    defer str.deinit();

    const count = countGraphemeClusters(str);
    expectEqual(count, 10);
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

// Str.startsWithCodePoint
pub fn startsWithCodePoint(string: RocStr, prefix: u32) callconv(.C) bool {
    const bytes_len = string.len();
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

test "startsWithCodePoint: ascii char" {
    const whole = RocStr.init("foobar", 6);
    const prefix = 'f';
    expect(startsWithCodePoint(whole, prefix));
}

test "startsWithCodePoint: emoji" {
    const yes = RocStr.init("💖foobar", 10);
    const no = RocStr.init("foobar", 6);
    const prefix = '💖';
    expect(startsWithCodePoint(yes, prefix));
    expect(!startsWithCodePoint(no, prefix));
}

test "startsWith: foo starts with fo" {
    const foo = RocStr.init("foo", 3);
    const fo = RocStr.init("fo", 2);
    expect(startsWith(foo, fo));
}

test "startsWith: 123456789123456789 starts with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    defer str.deinit();
    expect(startsWith(str, str));
}

test "startsWith: 12345678912345678910 starts with 123456789123456789" {
    const str = RocStr.init("12345678912345678910", 20);
    defer str.deinit();
    const prefix = RocStr.init("123456789123456789", 18);
    defer prefix.deinit();

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
    const foo = RocStr.init("foo", 3);
    const oo = RocStr.init("oo", 2);
    defer foo.deinit();
    defer oo.deinit();

    expect(endsWith(foo, oo));
}

test "endsWith: 123456789123456789 ends with 123456789123456789" {
    const str = RocStr.init("123456789123456789", 18);
    defer str.deinit();
    expect(endsWith(str, str));
}

test "endsWith: 12345678912345678910 ends with 345678912345678910" {
    const str = RocStr.init("12345678912345678910", 20);
    const suffix = RocStr.init("345678912345678910", 18);
    defer str.deinit();
    defer suffix.deinit();

    expect(endsWith(str, suffix));
}

test "endsWith: hello world ends with world" {
    const str = RocStr.init("hello world", 11);
    const suffix = RocStr.init("world", 5);
    defer str.deinit();
    defer suffix.deinit();

    expect(endsWith(str, suffix));
}

// Str.concat
pub fn strConcatC(result_in_place: InPlace, arg1: RocStr, arg2: RocStr) callconv(.C) RocStr {
    return @call(.{ .modifier = always_inline }, strConcat, .{ result_in_place, arg1, arg2 });
}

fn strConcat(result_in_place: InPlace, arg1: RocStr, arg2: RocStr) RocStr {
    if (arg1.isEmpty()) {
        // the second argument is borrowed, so we must increment its refcount before returning
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

    const result = strConcat(InPlace.Clone, roc_str1, roc_str2);

    defer result.deinit();

    expect(roc_str3.eq(result));
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

    expect(roc_result.eq(result));
}

// Str.toBytes
pub fn strToBytesC(arg: RocStr) callconv(.C) RocList {
    return @call(.{ .modifier = always_inline }, strToBytes, .{arg});
}

fn strToBytes(arg: RocStr) RocList {
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

pub fn fromUtf8C(arg: RocList, output: *FromUtf8Result) callconv(.C) void {
    output.* = @call(.{ .modifier = always_inline }, fromUtf8, .{arg});
}

fn fromUtf8(arg: RocList) FromUtf8Result {
    const bytes = @ptrCast([*]const u8, arg.bytes)[0..arg.length];

    if (unicode.utf8ValidateSlice(bytes)) {
        // the output will be correct. Now we need to take ownership of the input
        if (arg.len() <= SMALL_STR_MAX_LENGTH) {
            // turn the bytes into a small string
            const string = RocStr.init(@ptrCast([*]u8, arg.bytes), arg.len());

            // then decrement the input list
            const data_bytes = arg.len();
            utils.decref(arg.bytes, data_bytes, RocStr.alignment);

            return FromUtf8Result{ .is_ok = true, .string = string, .byte_index = 0, .problem_code = Utf8ByteProblem.InvalidStartByte };
        } else {
            const byte_list = arg.makeUnique(RocStr.alignment, @sizeOf(u8));

            const string = RocStr{ .str_bytes = byte_list.bytes, .str_len = byte_list.length };

            return FromUtf8Result{ .is_ok = true, .string = string, .byte_index = 0, .problem_code = Utf8ByteProblem.InvalidStartByte };
        }
    } else {
        const temp = errorToProblem(@ptrCast([*]u8, arg.bytes), arg.length);

        // consume the input list
        const data_bytes = arg.len();
        utils.decref(arg.bytes, data_bytes, RocStr.alignment);

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
pub const Utf8ByteProblem = packed enum(u8) {
    CodepointTooLarge = 0,
    EncodesSurrogateHalf = 1,
    ExpectedContinuation = 2,
    InvalidStartByte = 3,
    OverlongEncoding = 4,
    UnexpectedEndOfSequence = 5,
};

fn validateUtf8Bytes(bytes: [*]u8, length: usize) FromUtf8Result {
    return fromUtf8(RocList{ .bytes = bytes, .length = length });
}

fn validateUtf8BytesX(str: RocList) FromUtf8Result {
    return fromUtf8(str);
}

fn expectOk(result: FromUtf8Result) void {
    expectEqual(result.is_ok, true);
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

    expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: unicode œ" {
    const raw = "œ";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: unicode ∆" {
    const raw = "∆";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: emoji" {
    const raw = "💖";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectOk(validateUtf8BytesX(list));
}

test "validateUtf8Bytes: unicode ∆ in middle of array" {
    const raw = "œb∆c¬";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectOk(validateUtf8BytesX(list));
}

fn expectErr(list: RocList, index: usize, err: Utf8DecodeError, problem: Utf8ByteProblem) void {
    const str_ptr = @ptrCast([*]u8, list.bytes);
    const str_len = list.length;

    expectError(err, numberOfNextCodepointBytes(str_ptr, str_len, index));
    expectEqual(toErrUtf8ByteResponse(index, problem), validateUtf8Bytes(str_ptr, str_len));
}

test "validateUtf8Bytes: invalid start byte" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "ab\x80c";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 2, error.Utf8InvalidStartByte, Utf8ByteProblem.InvalidStartByte);
}

test "validateUtf8Bytes: unexpected eof for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 2 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L426
    const raw = "abc\xc2\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 3 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L430
    const raw = "abc\xe0\xa0\xc0";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: unexpected eof for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.UnexpectedEof, Utf8ByteProblem.UnexpectedEndOfSequence);
}

test "validateUtf8Bytes: expected continuation for 4 byte sequence" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L437
    const raw = "abc\xf0\x90\x80\x00";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.Utf8ExpectedContinuation, Utf8ByteProblem.ExpectedContinuation);
}

test "validateUtf8Bytes: overlong" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L451
    const raw = "abc\xf0\x80\x80\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.Utf8OverlongEncoding, Utf8ByteProblem.OverlongEncoding);
}

test "validateUtf8Bytes: codepoint out too large" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L465
    const raw = "abc\xf4\x90\x80\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.Utf8CodepointTooLarge, Utf8ByteProblem.CodepointTooLarge);
}

test "validateUtf8Bytes: surrogate halves" {
    // https://github.com/ziglang/zig/blob/0.7.x/lib/std/unicode.zig#L468
    const raw = "abc\xed\xa0\x80";
    const ptr: [*]const u8 = @ptrCast([*]const u8, raw);
    const list = sliceHelp(ptr, raw.len);

    expectErr(list, 3, error.Utf8EncodesSurrogateHalf, Utf8ByteProblem.EncodesSurrogateHalf);
}
